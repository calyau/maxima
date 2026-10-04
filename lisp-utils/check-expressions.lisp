;;;; check-expressions.lisp: runtime checks of how Maxima builds and
;;;; handles expressions
;;;;
;;;; Many Maxima bugs come from expressions built the wrong way: ADD or
;;;; TAKE given unsimplified arguments, a header with SIMP consed onto
;;;; arguments that were never simplified together, a rational built from
;;;; numbers nobody reduced, a simplifier like SIMPLUS called directly so
;;;; that tellsimp rules never see the expression, or a destructive
;;;; operation on structure the function does not own. Such an
;;;; expression usually looks fine and even displays fine. It only gives
;;;; wrong answers later, somewhere else.
;;;;
;;;; This file attaches checks to an unmodified, built Maxima image with
;;;; SB-INT:ENCAPSULATE, so it is SBCL-only and costs nothing unless it is
;;;; loaded. It never changes a result: a check that would need to ask a
;;;; question, print, or signal an error gives up instead.
;;;;
;;;;     ./maxima-local --no-init --batch-string='
;;;;     :lisp (load "lisp-utils/check-expressions.lisp")
;;;;     :lisp (maxima-exprcheck:install)
;;;;     run_testsuite();
;;;;     :lisp (maxima-exprcheck:write-report "/tmp/exprcheck")
;;;;     '
;;;;
;;;; RUN-TEXI-EXAMPLES runs the examples of a manual file and RUN-FUZZ runs
;;;; random inputs, for code the test suite does not reach.
;;;; lisp-utils/check-expressions.sh drives all three. Every :LISP form
;;;; must sit on a line of its own (see AGENTS.md, sec. 3).
;;;;
;;;; The checks (keywords for INSTALL's :CHECKS argument):
;;;;
;;;;   :CONTRACT      Arguments must be simplified where the callee assumes
;;;;                  they are: ADD, MUL, POWER, NEG, SUB, DIV, ADDN and MULN
;;;;                  with a true flag, ROOT, NCMUL, SIMPLIFYA with a true
;;;;                  second argument (and so TAKE, FTAKE and FAPPLY), and
;;;;                  EQTEST, which marks its argument simplified. The test
;;;;                  is structural: every subexpression carries SIMP,
;;;;                  rationals are canonical, there are no CL ratios,
;;;;                  complexes or single floats, and no operator that the
;;;;                  simplifier always removes (MMINUS, MQUOTIENT, ...).
;;;;   :RESULT        The same test on what a simplifier returns.
;;;;   :HAND-BUILT    An expression that carries SIMP flags but did not come
;;;;                  out of the simplifier must re-simplify to itself. This
;;;;                  catches hand-built non-canonical expressions such as
;;;;                  ((MPLUS SIMP) $X $X) or a stale header reused with new
;;;;                  arguments.
;;;;   :DIRECT-CALL   A simplifier (the value of an OPERATORS or SPECSIMP
;;;;                  property, which includes every DEF-SIMPLIFIER) called
;;;;                  other than through SIMPLIFYA, so rules never see it.
;;;;   :LITERAL       Constants of compiled Maxima code, the shared MSIMPIND
;;;;                  headers and the initial values RESET restores must
;;;;                  never change. Checked after every test problem.
;;;;   :ARG-MUTATION  Maxima functions ($FOO and their FOO-IMPL) and
;;;;                  simplifiers must not change their arguments.
;;;;   :IDEMPOTENCE   Off by default (slow): what a simplifier returns must
;;;;                  re-simplify to itself in the same context.
;;;;
;;;; Each bad object is reported once, where it is first seen, with the
;;;; calling functions, the test problem being run, and the function whose
;;;; literal list it was built on when its header is a compiled constant.

(defpackage #:maxima-exprcheck
  (:use #:common-lisp)
  (:import-from #:maxima #:simp #:ratsimp #:rat #:mrat #:mpois #:bigfloat
                #:mlist #:mequal #:mqapply #:$simp #:dosimp)
  (:export #:install #:uninstall #:reset #:write-report #:merge-reports
           #:*checks* #:*size-limit* #:*frames* #:*opaque-operators*
           #:set-location #:run-texi-examples #:run-fuzz #:fuzz-input
           #:*input-timeout*))

(in-package #:maxima-exprcheck)

;;; ------------------------------------------------------------------
;;; Settings

(defvar *checks*
  '(:contract :result :hand-built :direct-call :literal :arg-mutation)
  "Checks that INSTALL turns on by default.")

(defvar *size-limit* 2000
  "Expressions with more conses than this are neither copied nor
re-simplified, which keeps the cost of a check bounded.")

(defvar *frames* 4
  "How many calling functions a finding records.")

(defvar *opaque-operators*
  '(lambda maxima::mdefine maxima::mdefmacro maxima::mprog maxima::mprogn
    maxima::mdo maxima::mdoin maxima::mquote maxima::msetq maxima::mcond
    maxima::mreturn maxima::mgo)
  "Operators whose arguments legitimately stay unsimplified inside a
simplified expression.")

(defvar *noncanonical-operators*
  '(maxima::mminus maxima::mquotient maxima::%sqrt maxima::%exp)
  "Operators that simplification always rewrites into something else.")

(defparameter *transparent-functions*
  '(maxima::add2 maxima::add2* maxima::addn maxima::mul2 maxima::mul2*
    maxima::mul3 maxima::muln maxima::neg maxima::sub maxima::sub*
    maxima::div maxima::div* maxima::power maxima::power* maxima::root
    maxima::ncmul2 maxima::ncmuln maxima::ncpower maxima::simplifya
    maxima::simplify maxima::eqtest maxima::simpcheck maxima::simpmap)
  "Functions that only pass expressions on. Callers are reported past
them, at the function that used them.")

(defparameter *throw-tags*
  '(maxima::abort-demo maxima::appears maxima::bad-point maxima::break-exit
    maxima::cnt maxima::compatvl maxima::disjoint maxima::divergent
    maxima::done maxima::errorsw maxima::evarrp maxima::float maxima::ggrm
    maxima::isumout maxima::lhospital maxima::limit maxima::limit-found
    maxima::lip? maxima::macsyma-quit maxima::match
    maxima::nfloat-nounform-return maxima::mcatch maxima::mprog
    maxima::newroot maxima::notpoly maxima::pin%ex maxima::ptimes%e
    maxima::rat-err maxima::ratf maxima::relprime maxima::retry maxima::rnk
    maxima::sign-imag-err maxima::srrat maxima::subset maxima::tay-err
    maxima::taylor-catch maxima::unknown)
  "Tags thrown by Maxima code. Re-simplification inside a check catches
them all, so that a throw can never escape a check and change a result.")

;;; ------------------------------------------------------------------
;;; State

(defvar *enabled* '())
(defvar *busy* nil
  "True while a check runs. Every wrapper then just calls through.")
(defvar *resimplifying* nil)
(defvar *wrapped* '())

(defvar *ok* (make-hash-table :test 'eq :weakness :key)
  "Conses that passed the structural test, with everything below them.")
(defvar *reported* (make-hash-table :test 'eq :weakness :key)
  "Bad conses already reported.")
(defvar *simplified* (make-hash-table :test 'eq :weakness :key)
  "Values returned by a real simplification, as opposed to built by hand.")
(defvar *canonical* (make-hash-table :test 'eq :weakness :key)
  "Hand-built expressions found to re-simplify to themselves.")

(defvar *literals* '()
  "Entries (constant copy owner) for every cons constant of Maxima code.")
(defvar *literal-owner* (make-hash-table :test 'eq)
  "Every cons inside a compiled constant -> the functions owning it.")
(defvar *scanned-code* (make-hash-table :test 'eq :weakness :key))

(defvar *simplifier-ops* (make-hash-table :test 'eq)
  "Simplifier function name -> operators whose OPERATORS property it is.")
(defvar *specsimp-functions* (make-hash-table :test 'eq))
(defvar *dispatch* nil
  "The operator SIMPLIFYA is dispatching on, for the simplifier it calls.")
(defvar *current-simplifier* nil)

(defvar *findings* (make-hash-table :test 'equal))

;; Where the run is: set by the test-suite hooks or by SET-LOCATION.
(defvar *file* nil)
(defvar *problem* nil)
(defvar *input* nil)

(defstruct finding
  kind site detail caller callers (count 0) object context extra owner
  location)

;;; ------------------------------------------------------------------
;;; Printing

(defun strip-lineinfo (e depth)
  "A copy of E for printing, without the source positions the parser puts
into headers. Cut off below DEPTH."
  (cond ((or (atom e) (<= depth 0)) e)
        ((and (consp (car e)) (member (caar e) '(rat bigfloat mrat mpois)))
         e)
        (t
         (let ((head (if (and (consp (car e)) (symbolp (caar e)))
                         (cons (caar e) (remove-if #'consp (cdar e)))
                         (strip-lineinfo (car e) (1- depth))))
               (args '())
               (tail (cdr e)))
           (do ((n 0 (1+ n)))
               ((or (atom tail) (>= n 30)))
             (push (strip-lineinfo (car tail) (1- depth)) args)
             (setq tail (cdr tail)))
           (cons head (nreconc args tail))))))

(defun render (e &optional (depth 12))
  (handler-case
      (let ((*package* (find-package :maxima))
            (*print-pretty* nil)
            (*print-circle* t)
            (*print-readably* nil)
            (*print-level* depth)
            (*print-length* 25))
        (let ((s (prin1-to-string (strip-lineinfo e (1+ depth)))))
          (if (> (length s) 700)
              (concatenate 'string (subseq s 0 700) "...")
              s)))
    (error () "#<unprintable>")))

(defun render-maxima (e)
  (handler-case
      (let ((*busy* t)
            (s (coerce (maxima::mstring e) 'string)))
        (if (> (length s) 300) (concatenate 'string (subseq s 0 300) "...") s))
    (error () (render e))))

(defun name-string (name)
  (let ((*package* (find-package :maxima)) (*print-pretty* nil))
    (prin1-to-string name)))

;;; ------------------------------------------------------------------
;;; Who called

(defvar *our-package* (find-package '#:maxima-exprcheck))

(defun our-symbol-p (x)
  (and (symbolp x) (eq (symbol-package x) *our-package*)))

(defun mentions-p (name pred)
  (cond ((consp name)
         (or (mentions-p (car name) pred) (mentions-p (cdr name) pred)))
        (t (funcall pred name))))

(defun skip-frame-p (name)
  (or (mentions-p name #'our-symbol-p)
      (mentions-p name (lambda (x) (eq x 'sb-int:encapsulate)))
      (and (symbolp name)
           (or (member name *transparent-functions*)
               (member (symbol-package name)
                       (load-time-value
                        (mapcar #'find-package
                                '(#:sb-impl #:sb-int #:sb-kernel #:sb-di
                                  #:sb-debug #:sb-c))))))))

(defun simplify-frame-name (name)
  "Unwrap the entry-point wrappers SBCL names frames after."
  (if (and (consp name) (symbolp (car name)) (cdr name)
           (member (symbol-name (car name))
                   '("TL-XEP" "XEP" "&OPTIONAL-PROCESSOR" "&MORE-PROCESSOR"
                     "VARARGS-ENTRY" "HAIRY-ARG-PROCESSOR")
                   :test #'string=))
      (simplify-frame-name (cadr name))
      name))

(defun callers (n)
  "Names of the first N functions on the stack that are neither part of
this checker nor transparent."
  (let ((frame (sb-di:top-frame)) (names '()) (depth 0))
    (loop while (and frame (< (length names) n) (< depth 80))
          do (let ((name (ignore-errors
                          (simplify-frame-name
                           (sb-di:debug-fun-name
                            (sb-di:frame-debug-fun frame))))))
               (unless (or (null name) (skip-frame-p name))
                 (push name names)))
             (setq frame (sb-di:frame-down frame))
             (incf depth))
    (nreverse names)))

;;; ------------------------------------------------------------------
;;; Recording findings

(defun current-location ()
  (when *file*
    (list (file-namestring *file*) *problem* *input*)))

(defun header-owner (e)
  "Functions whose compiled constant is the header of E, or of the first
subexpression of E with such a header."
  (let ((seen 0))
    (labels ((walk (x)
               (when (and (consp x) (< (incf seen) 200))
                 (let ((owner (and (consp (car x))
                                   (gethash (car x) *literal-owner*))))
                   (when owner (return-from header-owner owner)))
                 (when (and (consp (car x)) (listp (cdr x)))
                   (loop for tail on (cdr x)
                         while (consp tail)
                         do (walk (car tail)))))))
      (walk e)
      nil)))

(defun note (kind site &key detail object context extra owner (frames t))
  "Count a finding. The first one of each kind, site and caller keeps an
example and the calling functions."
  (let* ((names (if frames (callers *frames*) '()))
         (caller (if names (name-string (car names)) "-"))
         (key (list kind site detail caller))
         (f (gethash key *findings*)))
    (unless f
      (setq f (make-finding
               :kind kind :site site :detail detail :caller caller
               :callers (mapcar #'name-string names)
               :object (and object (render object))
               :context (and context (render context))
               :extra extra
               :owner (and owner (format nil "~{~A~^, ~}" owner))
               :location (current-location)))
      (setf (gethash key *findings*) f))
    (incf (finding-count f))))

(defmacro checking (&body body)
  "Run BODY as a check: wrappers below it call straight through, and an
error in the checker itself is recorded instead of escaping."
  `(let ((*busy* t))
     (handler-case (progn ,@body)
       (error (c)
         (ignore-errors
          (note :checker-error (princ-to-string (type-of c))
                :detail (princ-to-string c)))))))

(defun enabled-p (check) (member check *enabled*))

;;; ------------------------------------------------------------------
;;; The structural test

(defun rat-defect (e)
  (destructuring-bind (&optional n d &rest more) (cdr e)
    (cond ((or more (not (integerp n)) (not (integerp d))) "non-integer parts")
          ((not (member 'simp (cdar e))) "no SIMP flag")
          ((<= d 0) "denominator not positive")
          ((= d 1) "denominator 1")
          ((zerop n) "numerator 0")
          ((/= (gcd n d) 1) "not reduced"))))

(defun bigfloat-defect (e)
  (let ((prec (third (car e))))
    (destructuring-bind (&optional m x &rest more) (cdr e)
      (cond ((or more (not (integerp m)) (not (integerp x))
                 (not (integerp prec)))
             "malformed")
            ((> (integer-length (abs m)) prec)
             "mantissa longer than precision")))))

(defun node-defect (e)
  "If the single node E cannot be part of a simplified expression, return
(kind . detail).  :OPAQUE means do not look inside."
  (cond ((atom e)
         (typecase e
           (ratio '(:cl-number . "CL ratio"))
           (complex '(:cl-number . "CL complex"))
           (single-float '(:cl-number . "single-float"))))
        ((atom (car e)) '(:malformed . "atomic header"))
        ((not (symbolp (caar e))) '(:malformed . "non-symbol operator"))
        ((not (listp (cdr e))) '(:malformed . "dotted argument list"))
        (t
         (let ((op (caar e)) (flags (cdar e)))
           (cond ((eq op 'rat)
                  (let ((d (rat-defect e))) (if d (cons :bad-rat d) :opaque)))
                 ((eq op 'bigfloat)
                  (let ((d (bigfloat-defect e)))
                    (if d (cons :bad-bigfloat d) :opaque)))
                 ((member op '(mrat mpois)) :opaque)
                 ((not (member 'simp flags))
                  (cond ((member 'ratsimp flags)
                         '(:ratsimp-flag . "RATDISREP output, not simplified"))
                        ((member op '(mlist mequal))
                         '(:unflagged-bag . "list or equation without SIMP"))
                        (t '(:unflagged . "no SIMP flag"))))
                 ((and (member op *noncanonical-operators*)
                       (not (member 'maxima::array flags)))
                  '(:noncanonical-op . "operator the simplifier removes"))
                 ((member op *opaque-operators*) :opaque))))))

(defun bad-part (e)
  "The first part of E that cannot be part of a simplified expression, and
its (kind . detail), or NIL."
  (cond ((atom e)
         (let ((d (node-defect e))) (if d (values e d) nil)))
        ((gethash e *ok*) nil)
        (t
         (let ((d (node-defect e)))
           (cond ((eq d :opaque) (setf (gethash e *ok*) t) nil)
                 (d (values e d))
                 (t
                  (loop for tail on (cdr e)
                        do (multiple-value-bind (bad why) (bad-part (car tail))
                             (when bad (return-from bad-part (values bad why))))
                        while (consp (cdr tail)))
                  (setf (gethash e *ok*) t)
                  nil))))))

(defun conses-up-to (e limit)
  "The number of conses in E, or LIMIT+1 if there are more."
  (let ((n 0))
    (labels ((walk (x)
               (when (consp x)
                 (when (> (incf n) limit) (return-from conses-up-to n))
                 (walk (car x))
                 (walk (cdr x)))))
      (walk e))
    n))

;;; ------------------------------------------------------------------
;;; Re-simplification

(defun call-catching-throws (tags thunk)
  "Call THUNK with a catch for every tag in TAGS. Return :THROWN if one
was thrown to."
  (let ((marker (list nil)))
    (labels ((call (tags)
               (if (null tags)
                   (cons marker (funcall thunk))
                   (catch (car tags) (call (cdr tags))))))
      (let ((r (call tags)))
        (if (and (consp r) (eq (car r) marker)) (cdr r) :thrown)))))

(defun resimplify (e)
  "Simplify a copy of E from scratch. Return :UNKNOWN if that would ask
a question, signal an error, throw, or E is too big."
  (if (> (conses-up-to e *size-limit*) *size-limit*)
      :unknown
      (let ((r (catch 'question
                 (handler-case
                     (let ((*resimplifying* t)
                           (*standard-output* (make-broadcast-stream))
                           (maxima::errcatch t)
                           (maxima::$errormsg nil)
                           (maxima::$error maxima::$error)
                           (maxima::*merror-signals-$error-p* t))
                       (call-catching-throws
                        *throw-tags*
                        (lambda ()
                          (let ((dosimp t))
                            (maxima::simplifya (copy-tree e) nil)))))
                   (error () :unknown)))))
        (if (member r '(:thrown :question)) :unknown r))))

(defun mark-simplified (e)
  "Record E and the forms inside it as simplifier output. Stops at forms
already recorded, so that marking costs only the new conses."
  (let ((n 0))
    (labels ((walk (x)
               (when (and (consp x) (consp (car x))
                          (not (gethash x *simplified*))
                          (< (incf n) *size-limit*))
                 (setf (gethash x *simplified*) t)
                 (unless (or (member (caar x) '(rat bigfloat mrat mpois))
                             (not (listp (cdr x))))
                   (loop for tail on (cdr x)
                         while (consp tail)
                         do (walk (car tail)))))))
      (walk e))))

(defun check-hand-built (site e)
  (unless (or (gethash e *simplified*) (gethash e *canonical*)
              (atom (car e))
              (member (caar e) '(rat bigfloat mrat mpois))
              (member (caar e) *opaque-operators*))
    (let ((r (resimplify e)))
      (cond ((eq r :unknown))
            ((maxima::alike1 r e) (setf (gethash e *canonical*) t))
            (t
             (setf (gethash e *canonical*) t) ; report it once
             (note :not-canonical site
                   :detail "hand-built, re-simplifies differently"
                   :object e :extra (render r) :owner (header-owner e)))))))

(defun check-value (site e &optional context)
  "E must be a simplified expression."
  (multiple-value-bind (bad why) (bad-part e)
    (cond (bad
           (unless (and (consp bad) (gethash bad *reported*))
             (when (consp bad) (setf (gethash bad *reported*) t))
             (note (car why) site :detail (cdr why) :object bad
                   :context (if (eq bad e) context e)
                   :owner (header-owner bad))))
          ((and (consp e) (enabled-p :hand-built))
           (check-hand-built site e)))))

;;; ------------------------------------------------------------------
;;; Wrapping functions

(defun wrap (name fn)
  (when (and (fboundp name) (not (macro-function name))
             (not (special-operator-p name))
             (not (sb-int:encapsulated-p name 'exprcheck)))
    (sb-int:encapsulate name 'exprcheck fn)
    (push name *wrapped*)
    t))

(defun uninstall ()
  "Remove every wrapper INSTALL put in place."
  (dolist (name *wrapped*)
    (ignore-errors (sb-int:unencapsulate name 'exprcheck)))
  (setq *wrapped* '() *enabled* '())
  t)

(defun active-p ()
  (and (not *busy*) $simp))

(defmacro defwrap (name lambda-list &body check)
  "Wrap NAME so that CHECK runs before the original is called."
  (let ((f (gensym "F")))
    `(wrap ',name
           (lambda (,f ,@lambda-list)
             (declare (ignorable ,@lambda-list))
             (when (active-p) (checking ,@check))
             (funcall ,f ,@lambda-list)))))

(defun check-args (site args &optional context)
  (dolist (a args) (check-value site a context)))

(defun install-contract-checks ()
  (defwrap maxima::add2 (x y) (check-args "ADD" (list x y)))
  (defwrap maxima::mul2 (x y) (check-args "MUL" (list x y)))
  (defwrap maxima::mul3 (x y z) (check-args "MUL" (list x y z)))
  (defwrap maxima::power (x y) (check-args "POWER" (list x y)))
  (defwrap maxima::ncmul2 (x y) (check-args "NCMUL" (list x y)))
  (defwrap maxima::ncpower (x y) (check-args "NCPOWER" (list x y)))
  (defwrap maxima::neg (x) (check-args "NEG" (list x)))
  (defwrap maxima::sub (x y) (check-args "SUB" (list x y)))
  (defwrap maxima::div (x y) (check-args "DIV" (list x y)))
  (defwrap maxima::root (x n) (check-args "ROOT" (list x)))
  (defwrap maxima::addn (terms flag)
    (when (and flag (listp terms)) (check-args "ADDN" terms)))
  (defwrap maxima::muln (factors flag)
    (when (and flag (listp factors)) (check-args "MULN" factors)))
  (defwrap maxima::ncmuln (factors flag)
    (when (and flag (listp factors)) (check-args "NCMULN" factors)))
  (defwrap maxima::eqtest (x check)
    ;; X comes back marked simplified, so its arguments must be.
    (when (and (consp x) (consp (car x))
               (not (member 'simp (cdar x)))
               (not (member (caar x) '(rat mrat mpois bigfloat)))
               (not (member (caar x) *opaque-operators*))
               (listp (cdr x)))
      (check-args "EQTEST" (cdr x) x))))

(defun simplifya-wrapper (f x y)
  (if (not (active-p))
      (funcall f x y)
      (let* ((form-p (and (consp x) (consp (car x)) (symbolp (caar x))))
             (op (and form-p (caar x)))
             (flagged (and form-p (member 'simp (cdar x))))
             (real (and form-p (or dosimp (not flagged))
                        (not (member op '(rat mrat mpois bigfloat))))))
        (when form-p
          (checking
            (cond ((and y (not flagged) (enabled-p :contract)
                        (not (member op '(rat mrat mpois bigfloat)))
                        (not (member op *opaque-operators*))
                        (listp (cdr x)))
                   (check-args "SIMPLIFYA t" (cdr x) x))
                  ((and flagged (not dosimp) (enabled-p :hand-built)
                        (not (gethash x *simplified*)))
                   ;; SIMPLIFYA returns X untouched.
                   (check-value "SIMPLIFYA of a SIMP-flagged form" x)))))
        (let ((r (let ((*dispatch* op)) (funcall f x y))))
          (when real
            (checking
              (when (and (consp r) (enabled-p :hand-built)) (mark-simplified r))
              (let ((site (format nil "result of ~A"
                                  (name-string (or (get op 'maxima::operators)
                                                   'maxima::simpargs)))))
                (when (enabled-p :result)
                  (let ((*enabled* (remove :hand-built *enabled*)))
                    (check-value site r x)))
                (when (and (enabled-p :idempotence) (consp r))
                  (let ((again (resimplify r)))
                    (unless (or (eq again :unknown) (maxima::alike1 again r))
                      (note :not-idempotent site
                            :detail "re-simplifies differently"
                            :object r :context x :extra (render again))))))))
          r))))

;;; Arguments that must come back unchanged.

(defun snapshot (args)
  (loop for a in args
        when (and (consp a) (<= (conses-up-to a *size-limit*) *size-limit*))
          collect (cons a (copy-tree a))))

(defun verify-snapshot (site snapshot)
  (loop for (a . copy) in snapshot
        unless (equal a copy)
          do (note :arg-mutated site :detail "argument changed by the call"
                   :object copy :extra (render a))))

(defun autoload-stub-p (f)
  "Is F the function AUTOF installs until the real one is loaded?"
  (let ((name (ignore-errors (sb-kernel:%fun-name f))))
    (and (consp name) (member 'maxima::autof name) t)))

(defun simplifier-wrapper (name)
  (let ((ops (gethash name *simplifier-ops*))
        (specsimp (gethash name *specsimp-functions*)))
    (lambda (f &rest args)
      (if (or (not (active-p)) (null args))
          (apply f args)
          (let ((form (first args))
                (legit (or (and *dispatch*
                                (or (member *dispatch* ops)
                                    (eq (get *dispatch* 'maxima::operators)
                                        name)))
                           (and specsimp
                                (eq *current-simplifier*
                                    'maxima::simpmqapply))))
                (snap nil))
            (checking
              (when (and (not legit) (enabled-p :direct-call))
                (note :direct-call (name-string name)
                      :detail "simplifier called directly, not via SIMPLIFYA"
                      :object form))
              (when (and (enabled-p :arg-mutation)
                         (consp form) (listp (cdr form)))
                (setq snap (snapshot (cdr form)))))
            (multiple-value-prog1
                ;; An autoload stub loads the real simplifier and calls it
                ;; again, which is still the same dispatch.
                (let ((*dispatch* (and (autoload-stub-p f) *dispatch*))
                      (*current-simplifier* name))
                  (apply f args))
              (when snap
                (checking (verify-snapshot (name-string name) snap)))))))))

(defun dollar-wrapper (name)
  (lambda (f &rest args)
    (if (or (not (active-p)) (null args))
        (apply f args)
        (let ((snap (checking (snapshot args))))
          (multiple-value-prog1 (apply f args)
            (when snap
              (checking (verify-snapshot (name-string name) snap))))))))

(defparameter *mutators*
  '(maxima::$setelmx maxima::setelmx-impl)
  "Functions documented to change their arguments.")

(defun collect-simplifiers ()
  (clrhash *simplifier-ops*)
  (clrhash *specsimp-functions*)
  (do-symbols (s :maxima)
    (let ((fn (get s 'maxima::operators)))
      (when (and fn (symbolp fn) (fboundp fn))
        (pushnew s (gethash fn *simplifier-ops*))))
    (let ((fn (get s 'maxima::specsimp)))
      (when (and fn (symbolp fn) (fboundp fn))
        (setf (gethash fn *specsimp-functions*) t)
        (unless (gethash fn *simplifier-ops*)
          (setf (gethash fn *simplifier-ops*) '())))))
  (hash-table-count *simplifier-ops*))

(defun install-simplifier-checks ()
  (collect-simplifiers)
  (loop for name being the hash-keys of *simplifier-ops*
        do (wrap name (simplifier-wrapper name))))

(defun install-dollar-checks ()
  (let ((names '()))
    (do-symbols (s :maxima)
      (when (eq (symbol-package s) (find-package :maxima))
        (let ((impl (get s 'maxima::impl-name)))
          (cond ((and impl (symbolp impl) (fboundp impl)) (push impl names))
                ((and (fboundp s) (not (macro-function s))
                      (char= (char (symbol-name s) 0) #\$)
                      (not (get s 'maxima::mfexpr*)))
                 (push s names))))))
    (dolist (name (remove-duplicates names))
      (unless (or (member name *mutators*) (gethash name *simplifier-ops*))
        (wrap name (dollar-wrapper name))))))

;;; ------------------------------------------------------------------
;;; Compiled constants

(defun code-names (code)
  (ignore-errors
   (loop for i below (sb-kernel:code-n-entries code)
         collect (sb-kernel:%simple-fun-name
                  (sb-kernel:%code-entry-point code i)))))

(defun maxima-code-p (names)
  (and (mentions-p names (lambda (x) (and (symbolp x)
                                          (eq (symbol-package x)
                                              (find-package :maxima)))))
       (not (mentions-p names #'our-symbol-p))))

(defun package-cache-p (c)
  ;; SBCL caches a package lookup in a constant (name . vector).
  (and (stringp (car c)) (simple-vector-p (cdr c))))

(defun register-owner (c owner)
  (let ((n 0))
    (labels ((walk (x)
               (when (and (consp x) (< (incf n) 5000))
                 (pushnew owner (gethash x *literal-owner*) :test #'equal)
                 (walk (car x))
                 (walk (cdr x)))))
      (walk c))))

(defun scan-literals ()
  "Record every cons constant of Maxima code not seen before."
  (let ((new 0))
    (sb-vm::map-allocated-objects
     (lambda (obj type size)
       (declare (ignore size))
       (when (and (= type sb-vm:code-header-widetag)
                  (not (gethash obj *scanned-code*)))
         (setf (gethash obj *scanned-code*) t)
         (let ((names (code-names obj)))
           (when (maxima-code-p names)
             (let ((owner (name-string (car names))))
               (loop for i from sb-vm:code-constants-offset
                       below (sb-kernel:code-header-words obj)
                     for c = (sb-kernel:code-header-ref obj i)
                     when (and (consp c) (not (package-cache-p c)))
                       do (register-owner c owner)
                          (when (< (conses-up-to c 20000) 20000)
                            (push (list c (copy-tree c) owner) *literals*)
                            (incf new))))))))
     :all)
    ;; RESET restores these very objects, so they must not change either.
    (maphash (lambda (var value)
               (when (and (consp value) (not (gethash value *literal-owner*))
                          (< (conses-up-to value 20000) 20000))
                 (let ((owner (format nil "initial value of ~A"
                                      (name-string var))))
                   (push owner (gethash value *literal-owner*))
                   (push (list value (copy-tree value) owner) *literals*)
                   (incf new))))
             maxima::*variable-initial-values*)
    (do-symbols (s :maxima)
      (let ((h (get s 'maxima::msimpind)))
        (when (and (consp h) (not (gethash h *literal-owner*)))
          (let ((owner (format nil "MSIMPIND of ~A" (name-string s))))
            (push owner (gethash h *literal-owner*))
            (push (list h (copy-list h) owner) *literals*))
          (incf new))))
    new))

(defun verify-literals ()
  (dolist (entry *literals*)
    (destructuring-bind (c copy owner) entry
      (unless (equal c copy)
        (let ((*busy* t))
          (note :literal-mutated owner
                :detail "constant changed"
                :object copy :extra (render c) :frames nil))
        (setf (second entry) (copy-tree c))))))

;;; ------------------------------------------------------------------
;;; Where we are: test files, problems, loads

(defun set-location (file problem &optional input)
  "Tell the checker what is running, for workloads other than the test
suite."
  (setq *file* file *problem* problem *input* input))

(defun install-location-hooks ()
  (wrap 'maxima::test-batch
        (lambda (f filename &rest args)
          (let ((*file* filename) (*problem* 1) (*input* nil))
            (apply f filename args))))
  (wrap 'maxima::meval*
        (lambda (f expr)
          (when (and *file* (null *input*) (not *busy*))
            (setq *input* (if (and (consp expr) (consp (car expr))
                                   (eq (caar expr) 'maxima::$errcatch))
                              (cadr expr)
                              expr)))
          (funcall f expr)))
  (wrap 'maxima::batch-equal-check
        (lambda (f expected result)
          (multiple-value-prog1 (funcall f expected result)
            (when (and (enabled-p :literal) (not *busy*))
              (verify-literals))
            (when *problem* (incf *problem*))
            (setq *input* nil))))
  (wrap 'maxima::load-impl
        (lambda (f &rest args)
          (multiple-value-prog1 (apply f args)
            (unless *busy*
              (let ((*busy* t))
                (when (enabled-p :literal) (scan-literals))
                (when (or (enabled-p :direct-call) (enabled-p :arg-mutation))
                  (let ((before (hash-table-count *simplifier-ops*)))
                    (collect-simplifiers)
                    (when (/= before (hash-table-count *simplifier-ops*))
                      (install-simplifier-checks))))
                (when (enabled-p :arg-mutation) (install-dollar-checks)))))))
  (wrap 'maxima::retrieve
        (lambda (f &rest args)
          (if *resimplifying* (throw 'question :question) (apply f args)))))

;;; ------------------------------------------------------------------
;;; Other workloads: the manual's examples and random expressions. Their
;;; inputs run with no terminal: a question gets end of file, and nothing
;;; is printed.

(defvar *input-timeout* 60
  "Seconds an input may run before it is abandoned.")

(defun read-inputs (text)
  "The Maxima statements in TEXT, parsed. Reading stops at a syntax error."
  (let ((forms '()))
    (with-input-from-string (s text)
      (loop
        (let ((r (let ((*standard-output* (make-broadcast-stream)))
                   (catch 'maxima::macsyma-quit
                     (handler-case (maxima::mread s :eof)
                       (error () :eof))))))
          (if (and (consp r) (consp (cdr r)))
              (push (third r) forms)
              (return)))))
    (nreverse forms)))

(defun eval-input (form file n)
  "Evaluate FORM as input N of FILE, the way a test problem runs."
  (set-location file n form)
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream))
        (*standard-input* (make-string-input-stream ""))
        (*query-io* (make-two-way-stream (make-string-input-stream "")
                                         (make-broadcast-stream))))
    (handler-case
        (sb-ext:with-timeout *input-timeout*
          (catch 'maxima::macsyma-quit
            (maxima::meval* `((maxima::$errcatch) ,form))))
      (sb-ext:timeout () :timeout)
      (serious-condition () :error)))
  (when (and (enabled-p :literal) (not *busy*))
    (verify-literals))
  (setq *input* nil))

(defun unsafe-input-p (form)
  "Inputs that would end the process or need the outside world."
  (let ((bad (list 'maxima::$quit 'maxima::$system 'maxima::$run_testsuite
                   'maxima::$demo 'maxima::$read 'maxima::$readonly
                   'maxima::$batch 'maxima::$writefile 'maxima::$closefile
                   'maxima::$appendfile 'maxima::$to_lisp 'maxima::$plot2d
                   'maxima::$plot3d 'maxima::$draw 'maxima::$draw2d
                   'maxima::$draw3d 'maxima::$wxdraw2d 'maxima::$scene)))
    (labels ((walk (x)
               (cond ((member x bad) (return-from unsafe-input-p t))
                     ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk form)
      nil)))

(defun texi-example-blocks (path)
  "The input of each @c ===beg=== ... @c ===end=== block of PATH."
  (let ((blocks '()) (current nil))
    (with-open-file (s path :external-format :utf-8)
      (loop for line = (read-line s nil nil)
            while line
            do (cond ((search "@c ===beg===" line) (setq current '()))
                     ((search "@c ===end===" line)
                      (when current
                        (push (format nil "~{~A~%~}" (reverse current)) blocks))
                      (setq current nil))
                     ((and (listp current) (>= (length line) 2)
                           (string= "@c" line :end2 2))
                      (push (string-left-trim " " (subseq line 2)) current)))))
    (nreverse blocks)))

(defun run-texi-examples (path)
  "Run the examples of the manual source PATH, each block from a clean
state. Return the number of inputs run."
  (let ((file (file-namestring path)) (n 0))
    (dolist (text (texi-example-blocks path))
      (let ((*busy* t))
        (catch 'maxima::macsyma-quit
          (ignore-errors
           (let ((*standard-output* (make-broadcast-stream)))
             (maxima::meval* '((maxima::$kill) maxima::$all))
             (maxima::meval* '((maxima::$reset)))))))
      (dolist (form (read-inputs text))
        (incf n)
        (unless (unsafe-input-p form)
          (eval-input form file n))))
    n))

;;; Random expressions. Each case applies one operation to a random
;;; expression in x, and a finding names the input, so it can be rerun.

(defparameter *fuzz-leaves*
  '("x" "x" "x" "y" "a" "1" "2" "3" "-1" "1/2" "-3/4" "%pi" "%e" "%i" "0"
    "0.5" "sqrt(2)" "2^(1/3)" "%pi/4" "inf"))

(defparameter *fuzz-functions*
  '("sin(~A)" "cos(~A)" "tan(~A)" "exp(~A)" "log(~A)" "sqrt(~A)" "abs(~A)"
    "atan(~A)" "asin(~A)" "acos(~A)" "sinh(~A)" "cosh(~A)" "tanh(~A)"
    "asinh(~A)" "erf(~A)" "gamma(~A)" "signum(~A)" "floor(~A)" "cot(~A)"
    "sec(~A)" "acot(~A)" "atan2(~A,x)" "atan2(x,~A)" "log(1+~A)"
    "realpart(~A)" "imagpart(~A)" "conjugate(~A)" "carg(~A)" "li[2](~A)"
    "bessel_j(0,~A)" "gamma_incomplete(2,~A)" "expintegral_ei(~A)"
    "factorial(~A)" "binomial(~A,2)" "max(~A,x)" "min(1,~A)" "lambert_w(~A)"
    "psi[0](~A)" "elliptic_f(~A,1/2)" "unit_step(~A)" "%e^(%i*~A)"))

(defparameter *fuzz-exponents*
  '("2" "2" "3" "-1" "-2" "1/2" "-1/2" "1/3" "3/2" "x" "n" "%i" "0.5"))

(defparameter *fuzz-operations*
  '("~A" "expand(~A)" "ratsimp(~A)" "fullratsimp(~A)" "factor(~A)"
    "trigsimp(~A)" "radcan(~A)" "logcontract(~A)" "rectform(~A)"
    "polarform(~A)" "float(~A)" "bfloat(~A)" "diff(~A,x)" "diff(~A,x,2)"
    "integrate(~A,x)" "integrate(~A,x,0,1)" "limit(~A,x,0)"
    "limit(~A,x,inf)" "limit(~A,x,0,plus)" "limit(~A,x,1)"
    "taylor(~A,x,0,3)" "taylor(~A,x,inf,2)" "trigreduce(~A)"
    "trigexpand(~A)" "exponentialize(~A)" "demoivre(~A)" "partfrac(~A,x)"
    "subst(x=y^2,~A)" "subst(x=1/y,~A)" "ev(~A,x=1/2)" "sign(~A)"
    "csign(~A)" "is(~A>0)" "abs(~A)" "realpart(~A)" "imagpart(~A)"
    "conjugate(~A)" "solve(~A,x)" "sum(~A,x,1,3)" "ratsubst(z,x^2,~A)"
    "xthru(~A)" "multthru(~A)" "factorsum(~A)" "rootscontract(~A)"
    "radcan(exp(~A))" "logexpand(~A)" "nounify(~A)" "ev(~A,numer)"
    "ev(~A,logexpand=all)" "ev(~A,radexpand=all)" "ev(~A,domain=complex)"
    "ev(~A,exponentialize)" "ev(~A,demoivre)" "scanmap(factor,~A)"
    "horner(~A,x)" "gfactor(~A)" "nterms(~A)" "hipow(~A,x)"
    "coeff(~A,x,1)" "laplace(~A,x,s)" "ilt(~A,x,s)" "powerseries(~A,x,0)"
    "defint(~A,x,0,inf)" "risch(~A,x)" "changevar('integrate(~A,x),x=u^2,u,x)"
    "at(diff(~A,x),x=0)" "ratdisrep(rat(~A))" "totaldisrep(taylor(~A,x,0,2))"
    "trigrat(~A)" "logarc(~A)" "minfactorial(~A)" "makegamma(~A)"
    "makefact(~A)" "rootscontract(sqrt(x)*~A)" "~A+~A" "~A*~A" "~A^2-~A"
    "expand(~A,0,0)" "rat(~A)" "assume(x>0)$ forget(x>0)$ ~A"))

(defvar *fuzz-random* (sb-ext:seed-random-state 0))

(defun fuzz-pick (list) (nth (random (length list) *fuzz-random*) list))

(defun fuzz-expression (depth)
  (if (or (<= depth 0) (< (random 1.0 *fuzz-random*) 0.2))
      (fuzz-pick *fuzz-leaves*)
      (case (random 10 *fuzz-random*)
        ((0 1) (format nil "(~A+~A)" (fuzz-expression (1- depth))
                       (fuzz-expression (1- depth))))
        ((2 3) (format nil "(~A*~A)" (fuzz-expression (1- depth))
                       (fuzz-expression (1- depth))))
        (4 (format nil "(~A-~A)" (fuzz-expression (1- depth))
                   (fuzz-expression (1- depth))))
        (5 (format nil "(~A/~A)" (fuzz-expression (1- depth))
                   (fuzz-expression (1- depth))))
        (6 (format nil "(~A)^~A" (fuzz-expression (1- depth))
                   (fuzz-pick *fuzz-exponents*)))
        (t (format nil (fuzz-pick *fuzz-functions*)
                   (fuzz-expression (1- depth)))))))

(defun fuzz-input (case)
  "The input of fuzz case CASE. The same seed and case give the same input."
  (let ((*fuzz-random* (sb-ext:seed-random-state case)))
    (let* ((op (fuzz-pick *fuzz-operations*))
           (e (fuzz-expression (+ 1 (random 4 *fuzz-random*)))))
      (concatenate 'string
                   (apply #'format nil op
                          (make-list (count #\~ op) :initial-element e))
                   ";"))))

(defun run-fuzz (from below &key (timeout 20))
  "Run fuzz cases FROM below BELOW. Return the number run."
  (let ((*input-timeout* timeout))
    (loop for case from from below below
          do (dolist (form (read-inputs (fuzz-input case)))
               (unless (unsafe-input-p form)
                 (eval-input form "fuzz" case))))
    (- below from)))

;;; ------------------------------------------------------------------
;;; Entry points

(defun reset ()
  "Forget all findings."
  (clrhash *findings*)
  (clrhash *reported*)
  t)

(defun install (&key (checks *checks*))
  "Turn on CHECKS. Return the number of functions wrapped."
  (uninstall)
  (setq *enabled* checks)
  (let ((*busy* t))
    (install-location-hooks)
    (when (enabled-p :literal) (scan-literals))
    (when (or (enabled-p :contract) (enabled-p :hand-built))
      (install-contract-checks))
    (wrap 'maxima::simplifya #'simplifya-wrapper)
    (when (or (enabled-p :direct-call) (enabled-p :arg-mutation))
      (install-simplifier-checks))
    (when (enabled-p :arg-mutation) (install-dollar-checks)))
  (length *wrapped*))

;;; ------------------------------------------------------------------
;;; Reports

(defparameter *kind-order*
  '((:unflagged "Unsimplified subexpression where a simplified one is required")
    (:ratsimp-flag "RATDISREP output (RATSIMP flag) used as if simplified")
    (:bad-rat "Rational that is not a canonical ((RAT SIMP) n d)")
    (:bad-bigfloat "Malformed bigfloat")
    (:cl-number "CL number that is not a Maxima number")
    (:noncanonical-op "Operator that simplification always removes")
    (:malformed "Malformed expression")
    (:unflagged-bag "List or equation without SIMP flag")
    (:not-canonical "Hand-built SIMP-flagged expression that is not canonical")
    (:not-idempotent "Simplifier result that re-simplifies differently")
    (:direct-call "Simplifier called directly instead of through SIMPLIFYA")
    (:literal-mutated
     "Constant mutated: compiled literal, MSIMPIND header or initial value")
    (:arg-mutated "Argument mutated by the called function")
    (:checker-error "Error inside the checker")))

(defun finding-plist (f)
  (list :kind (finding-kind f) :site (finding-site f)
        :detail (finding-detail f) :caller (finding-caller f)
        :callers (finding-callers f) :count (finding-count f)
        :object (finding-object f) :context (finding-context f)
        :extra (finding-extra f) :owner (finding-owner f)
        :location (let ((l (finding-location f)))
                    (when l
                      (list (first l) (second l)
                            (and (third l) (render-maxima (third l))))))))

(defun write-data (path plists)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (dolist (p plists) (prin1 p s) (terpri s))))))

(defun read-data (path)
  (with-open-file (s path)
    (with-standard-io-syntax
      (loop for p = (read s nil :eof) until (eq p :eof) collect p))))

(defun write-text (path plists)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (format s "Maxima expression checks: ~D group~:P, ~D occurrence~:P~%"
            (length plists)
            (reduce #'+ plists :key (lambda (p) (getf p :count))))
    (format s "~&Each group is one kind of finding at one site, from one ~
               calling function.~%")
    (loop for (kind title) in *kind-order*
          for group = (sort (remove kind plists :key (lambda (p) (getf p :kind))
                                                :test-not #'eq)
                            #'> :key (lambda (p) (getf p :count)))
          when group
            do (format s "~2%=== ~A: ~A (~D group~:P, ~D occurrence~:P)~%"
                       kind title (length group)
                       (reduce #'+ group :key (lambda (p) (getf p :count))))
               (dolist (p group)
                 (format s "~%~6D  ~A~@[ [~A]~]~%" (getf p :count)
                         (getf p :site) (getf p :detail))
                 (format s "        from: ~{~A~^ <- ~}~%"
                         (or (getf p :callers) (list (getf p :caller))))
                 (when (getf p :object)
                   (format s "        object: ~A~%" (getf p :object)))
                 (when (getf p :extra)
                   (format s "        now/resimplified: ~A~%" (getf p :extra)))
                 (when (getf p :context)
                   (format s "        in: ~A~%" (getf p :context)))
                 (when (getf p :owner)
                   (format s "        literal header from: ~A~%"
                           (getf p :owner)))
                 (let ((l (getf p :location)))
                   (when l
                     (format s "        first seen: ~A problem ~A~@[: ~A~]~%"
                             (first l) (second l) (third l))))))))

(defun write-report (base)
  "Write BASE.txt (for people) and BASE.sexp (for MERGE-REPORTS)."
  (let* ((*busy* t)
         (plists (loop for f being the hash-values of *findings*
                       collect (finding-plist f))))
    (write-data (format nil "~A.sexp" base) plists)
    (write-text (format nil "~A.txt" base) plists)
    (length plists)))

(defun merge-reports (base paths)
  "Combine the .sexp files PATHS into BASE.txt and BASE.sexp."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (path paths)
      (dolist (p (ignore-errors (read-data path)))
        (let* ((key (list (getf p :kind) (getf p :site) (getf p :detail)
                          (getf p :caller)))
               (old (gethash key table)))
          (if old
              (incf (getf old :count) (getf p :count))
              (setf (gethash key table) (copy-list p))))))
    (let ((plists (loop for p being the hash-values of table collect p)))
      (write-data (format nil "~A.sexp" base) plists)
      (write-text (format nil "~A.txt" base) plists)
      (length plists))))
