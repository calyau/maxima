;;;; lint-expressions.lisp: find risky ways of building and changing
;;;; Maxima expressions in the source
;;;;
;;;; This is the static half of check-expressions.lisp. The runtime
;;;; checks see only the code the test suite reaches. This reads every
;;;; file of src/maxima.system and reports the source patterns behind the
;;;; usual bugs, reached or not. It is a review list, not a verdict: a
;;;; pattern is often fine where it stands, because the arguments happen
;;;; to be simplified or the list is fresh.
;;;;
;;;; Run it in a built Maxima image, so that the reader, the packages and
;;;; the simplifier properties are the ones the code is compiled with:
;;;;
;;;;     ./maxima-local --no-init --batch-string='
;;;;     :lisp (load "lisp-utils/lint-expressions.lisp")
;;;;     :lisp (maxima-exprlint:lint "./" "/tmp/exprlint.txt")
;;;;     '
;;;;
;;;; or lisp-utils/check-expressions.sh --lint.
;;;;
;;;; The rules:
;;;;
;;;;   simp-header      a list consed onto a literal header carrying SIMP,
;;;;                    such as (LIST '(MPLUS SIMP) A B): no simplifier and
;;;;                    no rule ever sees it, so nothing orders, combines or
;;;;                    flattens its arguments
;;;;   simp-header-bag  the same for MLIST and MEQUAL, which are canonical as
;;;;                    long as their elements are simplified
;;;;   computed-rat     a rational built from computed parts, which nobody
;;;;                    reduces or normalizes
;;;;   header-reuse     (CONS (CAR E) (... (CDR E))): new arguments under the
;;;;                    old header, which keeps its SIMP flag
;;;;   simplify-noop    SIMPLIFY or SIMPLIFYA around such a form, or around
;;;;                    CL SUBST: SIMPLIFYA returns a SIMP-flagged form as it
;;;;                    is, so nothing is simplified
;;;;   direct-call      a simplifier (an OPERATORS or SPECSIMP property value,
;;;;                    DEF-SIMPLIFIER ones included) called directly, which
;;;;                    bypasses SIMPLIFYA and every tellsimp rule
;;;;   unsimplified-arg ADD, MUL, TAKE, FTAKE, SIMPLIFYA with T, ... assume
;;;;                    simplified arguments, but get one that visibly is not
;;;;   destructive-arg  a destructive operation on a parameter or on part of
;;;;                    one: the caller's expression changes under it
;;;;   destructive-literal  a destructive operation on a variable that holds
;;;;                    a quoted constant, which changes the constant itself
;;;;   cl-subst         CL SUBST or SUBLIS on an expression keeps the old
;;;;                    SIMP flags, so the result needs RESIMPLIFY
;;;;   eqtest           EQTEST outside a simplifier marks its argument
;;;;                    simplified without simplifying it

(defpackage #:maxima-exprlint
  (:use #:common-lisp)
  (:export #:lint #:lint-files))

(in-package #:maxima-exprlint)

(defmacro m (name) `(quote ,(intern (string name) :maxima)))

;;; ------------------------------------------------------------------
;;; The files, from src/maxima.system as DEFSYSTEM sees it

(defun mkf (name)
  (or (and (find-package "MK") (find-symbol name "MK"))
      (error "lint-expressions: MK::~A not found" name)))

(defun system-files (root)
  (unless (find-package "MK")
    (load (merge-pathnames "lisp-utils/defsystem.lisp" root)))
  (funcall (mkf "ADD-REGISTRY-LOCATION") (merge-pathnames "src/" root))
  (let ((files '()))
    (labels ((walk (c)
               (if (eq (funcall (mkf "COMPONENT-TYPE") c) :file)
                   (let ((p (ignore-errors
                             (funcall (mkf "COMPONENT-FULL-PATHNAME")
                                      c :source))))
                     (when p (push p files)))
                   (mapc #'walk (funcall (mkf "COMPONENT-COMPONENTS") c)))))
      (walk (funcall (mkf "FIND-SYSTEM") (m maxima) :load)))
    (remove-if (lambda (p) (search "/slatec/" (namestring p)))
               (nreverse files))))

;;; ------------------------------------------------------------------
;;; Reading

(defvar *problems* '() "Files that could not be read to the end.")

(defun proper (x)
  "X if it is a proper list, else NIL. Backquoted code read by SBCL has
comma objects where lists are expected."
  (if (sb-int:proper-list-p x) x '()))

(defun file-text (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((text (make-string (file-length s))))
      (subseq text 0 (read-sequence text s)))))

(defun read-toplevel-forms (path text)
  "List of (form start end) for each top-level form of PATH."
  (let ((*package* (find-package :maxima))
        (*read-eval* t)
        (forms '())
        (eof (list nil)))
    (with-input-from-string (s text)
      (loop
        (let* ((start (file-position s))
              (form (handler-case (read s nil eof)
                      (error (c)
                        (push (format nil "~A: read error after char ~D: ~A"
                                      (file-namestring path) start c)
                              *problems*)
                        (return)))))
          (when (eq form eof) (return))
          (push (list form start (file-position s)) forms)
          (when (and (consp form) (eq (car form) 'in-package))
            (setq *package* (find-package (second form)))))))
    (nreverse forms)))

;;; ------------------------------------------------------------------
;;; Findings

(defvar *rules*
  '(("simp-header" "SIMP-flagged expression built by hand")
    ("computed-rat" "rational built from computed parts")
    ("header-reuse" "new arguments under the old, possibly SIMP-flagged header")
    ("simplify-noop" "SIMPLIFY of a form whose header may carry SIMP: a no-op")
    ("direct-call" "simplifier called directly instead of through SIMPLIFYA")
    ("unsimplified-arg"
     "visibly unsimplified argument where a simplified one is assumed")
    ("destructive-literal"
     "destructive operation on a variable holding a quoted constant")
    ("destructive-arg" "destructive operation on a parameter or part of one")
    ("cl-subst" "CL SUBST/SUBLIS keeps stale SIMP flags unless resimplified")
    ("eqtest" "EQTEST outside a simplifier")
    ("simp-header-bag" "SIMP-flagged list or equation built by hand")))

(defstruct hit rule file line name snippet note)

(defvar *hits* '())
(defvar *path* nil)
(defvar *text* "")
(defvar *start* 0)
(defvar *end* 0)
(defvar *name* nil "The definition being walked.")
(defvar *toplevel* nil "The top-level form being walked.")
(defvar *cursors* nil "Hint -> position of its last match in this form.")

(defun whitespace-p (c)
  (member c '(#\Space #\Tab #\Newline)))

(defun match-at (text pos hint)
  "End of HINT matched at POS in TEXT, or NIL. A space in HINT matches any
run of whitespace, and case is ignored."
  (let ((i pos) (n (length text)))
    (loop for c across hint
          do (cond ((char= c #\Space)
                    (unless (and (< i n) (whitespace-p (char text i)))
                      (return-from match-at nil))
                    (loop while (and (< i n) (whitespace-p (char text i)))
                          do (incf i)))
                   ((and (< i n) (char-equal c (char text i))) (incf i))
                   (t (return-from match-at nil))))
    i))

(defvar *form-pos* nil
  "Text position of the form being checked, when it could be found.")

(defun advance (hint)
  "Find HINT after its last match in this top-level form and remember it."
  (let* ((cell (assoc hint *cursors* :test #'string-equal))
         (from (if cell (cdr cell) *start*))
         (pos (loop for i from from below *end*
                    when (match-at *text* i hint) return i)))
    (when pos
      (if cell
          (setf (cdr cell) (1+ pos))
          (push (cons hint (1+ pos)) *cursors*)))
    pos))

(defun hint-position (hint)
  "Where HINT occurs for the form being checked: at or after the form's own
position if known, else after its last match, else the top-level start."
  (let ((near (and *form-pos*
                   (loop for i from *form-pos*
                           below (min *end* (+ *form-pos* 2000))
                         when (match-at *text* i hint) return i))))
    (when near (return-from hint-position near)))
  (let* ((from (or (cdr (assoc hint *cursors* :test #'string-equal)) *start*))
         (pos (loop for i from from below *end*
                    when (match-at *text* i hint) return i)))
    (when (and (null pos) (> from *start*))
      (setq pos (loop for i from *start* below *end*
                      when (match-at *text* i hint) return i)))
    (when pos
      (let ((cell (assoc hint *cursors* :test #'string-equal)))
        (if cell
            (setf (cdr cell) (1+ pos))
            (push (cons hint (1+ pos)) *cursors*))))
    (or pos
        (or (position #\( *text* :start *start* :end *end*) *start*))))

(defun line-of (pos)
  (1+ (count #\Newline *text* :end pos)))

(defun snippet (form)
  (let ((s (handler-case
               (let ((*package* (find-package :maxima)) (*print-case* :downcase)
                     (*print-pretty* nil) (*print-level* 5) (*print-length* 8))
                 (prin1-to-string form))
             (error () "#<unprintable>"))))
    (if (> (length s) 140) (concatenate 'string (subseq s 0 140) "...") s)))

(defun hit (rule form hint &optional note)
  (push (make-hit :rule rule :file (file-namestring *path*)
                  :line (line-of (hint-position (string-downcase hint)))
                  :name (and *name* (snippet *name*)) :snippet (snippet form)
                  :note note)
        *hits*))

;;; ------------------------------------------------------------------
;;; What the image knows

(defvar *simplifiers* (make-hash-table :test 'eq))
(defvar *literal-vars* (make-hash-table :test 'eq)
  "Variables somewhere assigned a quoted cons.")

(defun collect-simplifiers ()
  (clrhash *simplifiers*)
  (do-symbols (s :maxima)
    (dolist (p (list (m operators) (m specsimp)))
      (let ((f (get s p)))
        (when (and f (symbolp f) (fboundp f))
          (setf (gethash f *simplifiers*) t))))))

(defparameter *assume-simplified*
  (mapcar (lambda (s) (intern (string s) :maxima))
          '(#:add #:sub #:mul #:div #:power #:neg #:inv #:ncmul #:ncpower
            #:root #:m+t #:m*t #:m//t #:m-t #:m^t #:take #:ftake #:fapply
            #:add2 #:mul2 #:mul3))
  "Operators and macros whose arguments must already be simplified.")

(defparameter *unsimplified-producers*
  (mapcar (lambda (s) (intern (string s) :maxima))
          '(#:ratdisrep #:cdisrep #:pdisrep #:nformat #:nformat-all #:subst
            #:sublis))
  "Functions whose value is not a simplified expression.")

(defparameter *destructive-first*
  '(rplaca rplacd nreverse sort stable-sort delete-duplicates nreconc nbutlast
    nunion nintersection nset-difference)
  "Destructive on their first argument.")

(defparameter *destructive-second*
  '(delete delete-if delete-if-not nsublis)
  "Destructive on their second argument.")

(defparameter *opaque-ops*
  (mapcar (lambda (s) (intern (string s) :maxima))
          '(#:lambda #:mdefine #:mdefmacro #:mprog #:mprogn #:mdo #:mdoin
            #:mquote #:msetq #:mcond #:mreturn #:mgo #:bigfloat #:mrat
            #:mpois)))

;;; ------------------------------------------------------------------
;;; Recognizing forms

(defun quoted-header (form)
  "If FORM is '(OP . FLAGS) with OP a symbol, return (OP . FLAGS)."
  (and (consp form) (eq (car form) 'quote) (consp (cadr form))
       (symbolp (caadr form)) (caadr form)
       (listp (cdadr form))
       (cadr form)))

(defun constant-form-p (form)
  (or (and (atom form) (or (not (symbolp form)) (keywordp form)
                           (member form '(nil t))))
      (and (consp form) (eq (car form) 'quote))))

(defun car-of (form)
  "X if FORM is (CAR X) or (FIRST X)."
  (and (consp form) (member (car form) '(car first)) (consp (cdr form))
       (null (cddr form)) (cadr form)))

(defun mentions-tail-of-p (x form)
  "Does FORM take a part of X other than its CAR?"
  (cond ((atom form) nil)
        ((and (member (car form)
                      (list* 'cdr 'cddr 'cdddr 'cadr 'caddr 'cadddr 'rest
                             'second 'third 'fourth 'nthcdr 'nth
                             (list (m margs))))
              (member x (cdr form) :test #'equal))
         t)
        (t (some (lambda (f) (mentions-tail-of-p x f))
                 (loop for tail on form
                       while (consp tail)
                       collect (car tail))))))

(defun expression-use-p (x tree)
  "Does TREE look at X as a Maxima expression, through its operator?"
  (let ((ops (list 'caar 'cdar (m mop) (m mplusp) (m mtimesp) (m mexptp)
                   (m mbagp) (m $listp) (m specrepp) (m mnump) (m ratnump)
                   (m mlistp) (m simplifya) (m simplify) (m alike1)))
        (n 0))
    (labels ((walk (f)
               (when (and (consp f) (< (incf n) 20000))
                 (when (and (member (car f) ops) (consp (cdr f))
                            (equal (cadr f) x))
                   (return-from expression-use-p t))
                 (walk (car f))
                 (walk (cdr f)))))
      (walk tree)
      nil)))

(defun header-reuse-p (form &optional (expression-known nil))
  "FORM is (CONS (CAR X) ...), (LIST* (CAR X) ...) or (LIST (CAR X) ...),
the rest built from the arguments of X, and X is an expression."
  (and (consp form) (member (car form) '(cons list list*))
       (consp (cdr form))
       (let ((x (car-of (cadr form))))
         (and x (mentions-tail-of-p x (cddr form))
              (or expression-known (expression-use-p x *toplevel*))))))

(defun expr-unflagged-p (x)
  "Does the literal expression X contain a form without SIMP?"
  (and (consp x) (consp (car x)) (symbolp (caar x))
       (not (member (caar x) *opaque-ops*))
       (or (not (member (m simp) (cdar x)))
           (some #'expr-unflagged-p
                 (loop for tail on (cdr x)
                       while (consp tail)
                       collect (car tail))))))

(defun unsimplified-reason (form)
  "Why the argument FORM is visibly not a simplified expression, or NIL."
  (cond ((atom form) nil)
        ((eq (car form) 'quote)
         (and (expr-unflagged-p (cadr form)) "quoted expression without SIMP"))
        ((eq (car form) 'sb-int:quasiquote)
         (unsimplified-reason (ignore-errors (macroexpand-1 form))))
        ((member (car form) '(list list* cons))
         (let* ((h (cadr form)) (q (quoted-header h)))
           (cond ((and q (not (member (m simp) (cdr q)))
                       (not (member (car q) *opaque-ops*)))
                  (format nil "built on the unflagged header ~(~S~)" q))
                 ((and (consp h) (member (car h) (list 'list (m ncons)))
                       (= (length h) 2)
                       (quoted-header (list 'quote (list (cadr h)))))
                  "built on a fresh header without SIMP"))))
        ((member (car form) *unsimplified-producers*)
         (format nil "value of ~(~A~)" (car form)))))

;;; Destructive targets

(defvar *params* '())

(defun accessor-p (s)
  (and (symbolp s)
       (or (member s (list* 'first 'second 'third 'fourth 'fifth 'rest 'last
                            'nth 'nthcdr (list (m margs))))
           (let ((n (symbol-name s)))
             (and (> (length n) 2) (char= (char n 0) #\C)
                  (char= (char n (1- (length n))) #\R)
                  (every (lambda (c) (member c '(#\A #\D)))
                         (subseq n 1 (1- (length n)))))))))

(defun root-of (form)
  "The variable FORM takes a part of, through accessors, or NIL."
  (cond ((symbolp form) form)
        ((and (consp form) (accessor-p (car form)))
         (root-of (car (last form))))))

(defun param-part-p (form)
  (let ((r (root-of form))) (and r (member r *params*))))

(defun special-p (s)
  (eq (sb-int:info :variable :kind s) :special))

(defun literal-var-part-p (form)
  (let ((r (root-of form)))
    (and r (gethash r *literal-vars*) (special-p r))))

(defun destructive-targets (form)
  "The forms FORM destructively modifies."
  (let ((op (car form)) (args (cdr form)))
    (cond ((member op *destructive-first*) (list (first args)))
          ((member op *destructive-second*) (list (second args)))
          ((eq op 'nconc) (butlast args))
          ((eq op 'nsubst) (list (third args)))
          ((member op '(setf incf decf push pushnew pop remf))
           (let ((places (case op
                           (setf (loop for (place) on args by #'cddr
                                       collect place))
                           ((incf decf pop) (list (first args)))
                           ((push pushnew) (list (second args)))
                           (remf (list (first args))))))
             (loop for place in places
                   when (and (consp place) (accessor-p (car place)))
                     collect (car (last place))
                   when (and (consp place) (eq (car place) 'getf))
                     collect (second place)
                   when (and (eq op 'remf) (symbolp place))
                     collect place))))))

;;; ------------------------------------------------------------------
;;; The walk

(declaim (ftype function walk walk-form))

(defun lambda-vars (lambda-list)
  (loop for x in (proper lambda-list)
        unless (member x lambda-list-keywords)
          collect (if (consp x) (if (consp (car x)) (cadar x) (car x)) x)))

(defun definition-name (form)
  (let ((n (second form)))
    (if (consp n) (car n) n)))

(defparameter *definers*
  (mapcar (lambda (s) (intern (string s) :maxima))
          '(#:defun #:defmfun #:defmacro #:defmspec #:def-simplifier
            #:defun-prop #:defmspec #:define-compiler-macro)))

(defun simplifier-definition-p ()
  (and *name* (symbolp *name*)
       (or (gethash *name* *simplifiers*)
           (let ((n (symbol-name *name*)))
             (and (>= (length n) 4) (string= "SIMP" n :end2 4))))))

(defun check-form (form parent)
  (let ((op (car form)) (args (cdr form)))
    ;; Hand-built headers.
    (when (member op '(list list* cons))
      (let ((q (quoted-header (car args))))
        (when (and q (member (m simp) (cdr q))
                   (notevery #'constant-form-p (cdr args)))
          (cond ((eq (car q) (m rat))
                 (hit "computed-rat" form (format nil "(~(~A~)" (car q))))
                ((member (car q) (list (m mlist) (m mequal)))
                 (hit "simp-header-bag" form
                      (format nil "(~(~A~) simp" (car q))))
                ((member (car q) *opaque-ops*))
                (t (hit "simp-header" form
                        (format nil "(~(~A~) simp" (car q))))))
        (when (and q (eq (car q) (m rat)) (not (member (m simp) (cdr q)))
                   (notevery #'constant-form-p (cdr args)))
          (hit "computed-rat" form "(rat")))
      (when (header-reuse-p form)
        (hit "header-reuse" form (format nil "(~(~A~) (car" op))))
    (when (and (member op (list (m make-rat) (m make-rat-simp) (m rat)))
               (notevery #'constant-form-p args))
      (hit "computed-rat" form (format nil "(~(~A~)" op)))
    ;; SIMPLIFY around a form that keeps its old SIMP flag.
    (when (and (member op (list (m simplify) (m simplifya)))
               (consp (car args))
               (or (header-reuse-p (car args) t)
                   (member (caar args) (list 'subst 'sublis))))
      (hit "simplify-noop" form (format nil "(~(~A~)" op)))
    ;; Direct simplifier calls.
    (when (and (symbolp op) (gethash op *simplifiers*))
      (hit "direct-call" form (format nil "(~(~A~)" op)
           (when (eq op *name*) "recursive")))
    (when (and (eq op 'function) (symbolp (car args))
               (gethash (car args) *simplifiers*))
      (hit "direct-call" form (format nil "~(~A~)" (car args))
           "function object"))
    ;; Unsimplified arguments where simplified ones are assumed.
    (let ((checked
            (cond ((member op *assume-simplified*)
                   (if (member op (list (m take) (m fapply))) (cdr args) args))
                  ((and (member op (list (m addn) (m muln) (m ncmuln)))
                        (eq (second args) t))
                   (let ((l (first args)))
                     (if (and (consp l) (eq (car l) 'list)) (cdr l) (list l))))
                  ((and (eq op (m simplifya)) (eq (second args) t))
                   (let ((x (first args)))
                     (if (and (consp x) (member (car x) '(list list* cons)))
                         (cddr x)
                         '()))))))
      (dolist (a checked)
        (let ((why (unsimplified-reason a)))
          (when why
            (hit "unsimplified-arg" form (format nil "(~(~A~)" op) why)))))
    ;; Destructive operations.
    (when (symbolp op)
      (dolist (target (ignore-errors (destructive-targets form)))
        (cond ((literal-var-part-p target)
               (hit "destructive-literal" form (format nil "(~(~A~)" op)
                    (format nil "~(~A~) holds a quoted constant"
                            (root-of target))))
              ((param-part-p target)
               (hit "destructive-arg" form (format nil "(~(~A~)" op)
                    (format nil "parameter ~(~A~)" (root-of target)))))))
    ;; CL SUBST keeps SIMP flags.
    (when (and (member op '(subst sublis nsubst nsublis))
               (not (and (consp parent)
                         (member (car parent)
                                 (list (m resimplify) (m unsimplify))))))
      (hit "cl-subst" form (format nil "(~(~A~)" op)))
    (when (and (eq op (m eqtest)) (not (simplifier-definition-p)))
      (hit "eqtest" form "(eqtest"))))

(defun walk-body (forms parent)
  (loop for tail on forms while (consp tail) do (walk (car tail) parent)))

(defun walk (form parent)
  (cond ((atom form))
        ((not (sb-int:proper-list-p form))
         (loop for tail on form while (consp tail) do (walk (car tail) parent)))
        (t (handler-case (walk-form form parent)
             (error ()
               (walk-body (cdr form) form))))))

(defun walk-form (form parent)
  (progn
    (let ((op (car form)))
      (cond ((eq op 'quote))
            ((eq op 'declare))
            ((eq op 'sb-int:quasiquote)
             (walk (ignore-errors (macroexpand-1 form)) parent))
            ((eq op 'function)
             (ignore-errors (check-form form parent))
             (let ((f (cadr form)))
               (when (and (consp f) (eq (car f) 'lambda))
                 (let ((*params* (append (lambda-vars (cadr f)) *params*)))
                   (walk-body (cddr f) form)))))
            ((eq op 'lambda)
             (let ((*params* (append (lambda-vars (cadr form)) *params*)))
               (walk-body (cddr form) form)))
            ((and (member op *definers*) (consp (cdr form)))
             (let ((*name* (if (eq op (m def-simplifier))
                               (intern (format nil "SIMP-%~A"
                                               (symbol-name
                                                (definition-name form)))
                                       :maxima)
                               (definition-name form)))
                   (*params* (if (eq op (m def-simplifier))
                                 (cons (intern "FORM" :maxima)
                                       (lambda-vars (third form)))
                                 (lambda-vars (third form)))))
               (walk-body (cdddr form) form)))
            ((member op '(let let*))
             (let ((bindings (cadr form)))
               (dolist (b (proper bindings))
                 (when (consp b) (walk (cadr b) form)))
               (let ((*params* (set-difference
                                *params*
                                (mapcar (lambda (b) (if (consp b) (car b) b))
                                        (proper bindings)))))
                 (walk-body (cddr form) form))))
            ((member op '(dolist dotimes))
             (when (consp (cadr form)) (walk (second (cadr form)) form))
             (walk-body (cddr form) form))
            ((member op '(do do*))
             (dolist (b (proper (cadr form)))
               (when (consp b) (walk-body (cdr b) form)))
             (when (consp (caddr form)) (walk-body (caddr form) form))
             (walk-body (cdddr form) form))
            ((member op '(prog prog*))
             (dolist (b (proper (cadr form)))
               (when (consp b) (walk (cadr b) form)))
             (walk-body (cddr form) form))
            ((member op '(multiple-value-bind destructuring-bind))
             (walk (caddr form) form)
             (let ((*params* (set-difference *params*
                                             (lambda-vars (cadr form)))))
               (walk-body (cdddr form) form)))
            ((member op '(case ecase ccase typecase etypecase ctypecase))
             (walk (cadr form) form)
             (dolist (clause (proper (cddr form)))
               (when (consp clause) (walk-body (cdr clause) form))))
            ((eq op 'cond)
             (dolist (clause (proper (cdr form)))
               (when (consp clause) (walk-body clause form))))
            ((eq op 'handler-case)
             (walk (cadr form) form)
             (dolist (clause (proper (cddr form)))
               (when (consp clause) (walk-body (cddr clause) form))))
            ((member op '(flet labels macrolet))
             (dolist (def (proper (cadr form)))
               (when (consp def)
                 (let ((*params* (append (lambda-vars (cadr def)) *params*)))
                   (walk-body (cddr def) form))))
             (walk-body (cddr form) form))
            ((member op (list 'defvar 'defparameter 'defconstant (m defmvar)
                              (m defprop) (m putprop)))
             (walk-body (cddr form) form))
            (t
             (let ((*form-pos* (or (and (symbolp op)
                                        (advance (format nil "(~(~A~) " op)))
                                   *form-pos*)))
               (ignore-errors (check-form form parent))
               (walk-body (cdr form) form)))))))

(defun collect-literal-vars (form)
  "Record variables FORM assigns a quoted cons, anywhere inside it."
  (when (and (consp form) (not (sb-int:proper-list-p form)))
    (loop for tail on form while (consp tail)
          do (collect-literal-vars (car tail)))
    (return-from collect-literal-vars))
  (when (consp form)
    (let ((op (car form)))
      (cond ((eq op 'quote))
            ((and (member op (list 'defvar 'defparameter (m defmvar)))
                  (symbolp (second form)) (consp (third form))
                  (eq (car (third form)) 'quote) (consp (cadr (third form))))
             (setf (gethash (second form) *literal-vars*) t))
            ((and (member op '(setq setf)))
             (loop for (place value) on (cdr form) by #'cddr
                   when (and (symbolp place) (consp value)
                             (eq (car value) 'quote)
                             (consp (cadr value)))
                     do (setf (gethash place *literal-vars*) t))
             (loop for x in (cdr form) do (collect-literal-vars x)))
            (t (loop for tail on form while (consp tail)
                     do (collect-literal-vars (car tail))))))))

;;; ------------------------------------------------------------------
;;; Entry points

(defun lint-files (paths)
  "Lint PATHS and return the hits, in file and line order."
  (collect-simplifiers)
  (clrhash *literal-vars*)
  (setq *hits* '() *problems* '())
  (let ((files (loop for p in paths
                     for text = (file-text p)
                     collect (list p text (read-toplevel-forms p text)))))
    (loop for (nil nil forms) in files
          do (loop for (form) in forms do (collect-literal-vars form)))
    (loop for (path text forms) in files
          do (let ((*path* path) (*text* text))
               (loop for (form start end) in forms
                     do (let ((*start* start) (*end* end) (*cursors* '())
                              (*name* nil) (*params* '()) (*form-pos* nil)
                              (*toplevel* form))
                          (walk form nil))))))
  (setq *hits* (stable-sort (nreverse *hits*)
                            (lambda (a b)
                              (or (string< (hit-file a) (hit-file b))
                                  (and (string= (hit-file a) (hit-file b))
                                       (< (hit-line a) (hit-line b))))))))

(defun lint (root &optional (output "exprlint.txt"))
  "Lint the files of src/maxima.system under ROOT and write OUTPUT."
  (let* ((root (truename root))
         (hits (lint-files (system-files root))))
    (with-open-file (s output :direction :output :if-exists :supersede)
      (format s "Maxima expression lint: ~D hit~:P~%" (length hits))
      (dolist (p *problems*) (format s "~&  NOTE: ~A~%" p))
      (loop for (rule title) in *rules*
            for these = (remove rule hits :key #'hit-rule :test-not #'string=)
            do (format s "~&  ~5D  ~A~%" (length these) rule))
      (loop for (rule title) in *rules*
            for these = (remove rule hits :key #'hit-rule :test-not #'string=)
            when these
              do (format s "~2%=== ~A: ~A (~D)~%" rule title (length these))
                 (dolist (h these)
                   (format s "~&~A:~D~@[ (~A)~]: ~A~@[  (~A)~]~%"
                           (hit-file h) (hit-line h) (hit-name h)
                           (hit-snippet h) (hit-note h)))))
    (length hits)))
