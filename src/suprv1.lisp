;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

;;note in converting this file (originally suprv.lisp) to common lisp
;;for the lisp machine, I removed a lot of the old stuff which did not
;;apply, and tried to eliminate any / quoting.  Most of the relevant
;;stuff is in system.lisp for the lispm and nil friends.--wfs

;; It would probably be better to bind *print-base* and *read-base* as
;; needed in various functions instead of doing it this way with
;; *old-ibase* and *old-base*.  There are already some cases where
;; *print-base* is bound in various functions.
(defvar *old-ibase*)
(defvar *old-base*)

(eval-when
    (:compile-toplevel :execute)
  (setq *old-ibase* *read-base* *old-base* *print-base*)
  (setq *read-base* 10. *print-base* 10.))

(declare-top  (special errset
		       *checkfactors* $features featurel
		       dispflag savefile
		       opers *ratweights
		       transp autoload))

(defvar thistime 0)
(defvar mcatch nil)
(defvar brklvl -1)
(defvar allbutl nil)

(defmvar $strdisp t)

(defmvar $grind nil
  "When the variable 'grind' is 'true', the output of 'string' and
  'stringout' has the same format as that of 'grind'; otherwise no
  attempt is made to specially format the output of those functions.")

(defmvar $backtrace '$backtrace)

(defmvar $debugmode nil
  "When 'debugmode' is 'true', Maxima will start the Maxima debugger
  when a Maxima error occurs.

  When 'debugmode' is 'lisp', Maxima will start the Lisp debugger when
  a Maxima error occurs.

  In either case, enabling 'debugmode' will not catch Lisp errors."
 :properties ((assign 'debugmode1)))

(defmvar $poislim 5
  "Determines the domain of the coefficients in the arguments of the
  trig functions.  The initial value of 5 corresponds to the interval
  [-2^(5-1)+1,2^(5-1)], or [-15,16], but it can be set to [-2^(n-1)+1,
  2^(n-1)]."
  :properties ((assign 'poislim1)))

;; This version of meval* makes sure, that the facts from the global variable
;; *local-signs* are cleared with a call to clearsign. The facts are added by
;; asksign and friends. The function meval* is only used for top level
;; evaluations.  For other cases the function meval can be used.

(defmvar $ratvarswitch t) ; If T, start an evaluation with a fresh list VARLIST.

(defun meval* (expr)
  ;; Make sure that clearsign is called after the evaluation.
  (unwind-protect
    (let (*refchkl* *checkfactors*)
      (if $ratvarswitch (setq varlist (cdr $ratvars)))
      (meval expr))
    ;; Clear the facts from asksign and friends.
    (clearsign)))

(defun makelabel10 (x)
  (let (*print-radix*
	(*print-base* 10.))
    ($concat '|| x $linenum)))
(defun makelabel (x)
  (setq *linelabel* (makelabel10 x))
  (unless $nolabels
    (when (or (null (cdr $labels))
	      (when (member *linelabel* (cddr $labels) :test #'equal)
		(setf $labels (delete *linelabel* $labels :count 1 :test #'eq)) t)
	      (not (eq *linelabel* (cadr $labels))))
      (setq $labels (cons (car $labels) (cons *linelabel* (cdr $labels))))))
  *linelabel*)

(defun printlabel ()
  (mtell-open "(~A) " (subseq (print-invert-case *linelabel*) 1)))

(defun mexploden (x)
  (let (*print-radix*
	(*print-base* 10))
    (exploden x)))

(defun addlabel (label)
  (setq $labels (cons (car $labels) (cons label (delete label (cdr $labels) :count 1 :test #'eq)))))

(defun tyi* ()
  (clear-input)
  (do ((n (tyi) (tyi))) (nil)
    (cond ((or (char= n #\newline) (and (> (char-code n) 31) (char/= n #\rubout)))
	   (return n))
	  ((char= n #\page) (format t "~|") (throw 'retry nil)))))

(defun continuep ()
  (loop
   (catch 'retry
     (unwind-protect
	  (progn
	    (fresh-line)
	    (princ (break-prompt))
	    (finish-output)
	    (return (char= (tyi*) #\newline)))
       (clear-input)))))

(defun checklabel (x)	; CHECKLABEL returns T iff label is not in use
  (not (or $nolabels
	   (= $linenum 0)
	   (boundp (makelabel10 x)))))

(defun gctimep (timep tim)
  (cond ((and (eq timep '$all) (not (zerop tim))) (princ (intl:gettext "Total time = ")) t)
	(t (princ (intl:gettext "Time = ")) nil)))

; Following GENERIC-AUTOLOAD is copied from orthopoly/orthopoly-init.lisp.
; Previous version didn't take Clisp, CMUCL, or SBCL into account.

(defvar *autoloaded-files* ())

(defun generic-autoload (file &aux type)
  (unless (member file *autoloaded-files* :test #'equal)
    (push file *autoloaded-files*)
    (setq file (pathname (cdr file)))
    (setq type (pathname-type file))
    (let ((bin-ext #+gcl "o"
		   #+cmu (c::backend-fasl-file-type c::*target-backend*)
		   #+clisp "fas"
		   #+allegro "fasl"
		   #+openmcl (pathname-type ccl::*.fasl-pathname*)
		   #+lispworks (pathname-type (compile-file-pathname "foo.lisp"))
		   #-(or gcl cmu clisp allegro openmcl lispworks) ""))
      (if (member type (list bin-ext "lisp" "lsp")  :test 'equalp)
	  (let ((*read-base* 10.)) #-sbcl (load file) #+sbcl (with-compilation-unit nil (load file)))
	  ($load file)))))

(defvar autoload 'generic-autoload)

(defun load-function (func mexprp)	; The dynamic loader
  (declare (ignore mexprp))
  (let ((file (get func 'autoload)))
    (if file (funcall autoload (cons func file)))))

(defmspec $loadfile (form)
  (loadfile (namestring (maxima-string (meval (cadr form)))) nil
	    (not (member $loadprint '(nil $autoload) :test #'equal))))

(defmfun $setup_autoload (filename &rest functions)
  (let ((file ($file_search filename)))
    (dolist (func functions)
      (nonsymchk func '$setup_autoload)
      (putprop (setq func ($verbify func)) file 'autoload)
      (add2lnc func $props)))
  '$done)

(defun dollarify (l)
  (let ((errset t))
    (cons '(mlist simp)
	  (mapcar #'(lambda (x)
		      (let (y)
			(cond ((numberp x) x)
			      ((numberp (setq y (car (errset (readlist (mexploden x))))))
			       y)
			      (t (makealias x)))))
		  l))))

(defun mfboundp (func)
  (or (mgetl func '(mexpr mmacro))
      (getl func '(translated-mmacro mfexpr* mfexpr*s))))

(defun loadfile (file findp printp)
  (and findp (member $loadprint '(nil $loadfile) :test #'equal) (setq printp nil))
  ;; Should really get the truename of FILE.
  (if printp (format t (intl:gettext "loadfile: loading ~A.~%") file))
  (let* ((path (pathname file))
	 (*package* (find-package :maxima))
	 ($load_pathname path)
	 (*read-base* 10.)
	 (tem (errset #-sbcl (load (pathname file)) #+sbcl (with-compilation-unit nil (load (pathname file))))))
    (or tem (merror (intl:gettext "loadfile: failed to load ~A") (namestring path)))
    (namestring path)))

(defmfun $directory (path)
  (cons '(mlist) (mapcar 'namestring (directory ($filename_merge path)))))

(defmspec $kill (form)
  (clear)	;; get assume db into consistent state
  (mapc #'kill1 (cdr form))
  '$done)

;;; The following *builtin- variables are used to keep/restore builtin
;;; symbols and values during kill operations. Their values are set at
;;; the end of init-cl.lisp, after all symbols have been defined.

(defvar *builtin-symbols* nil)
(defvar *builtin-symbol-props* (make-hash-table))
(defvar *builtin-$props* nil)
(defvar *builtin-$rules* nil)
(defvar *builtin-symbols-with-values* nil)
(defvar *builtin-symbol-values* (make-hash-table))

(defun kill1-atom (x)
  (let ((z (or (and (member x (cdr $aliases) :test #'equal) (get x 'noun)) (get x 'verb))))
    (when (or (null allbutl) (not (member z allbutl :test #'equal)))
      (remvalue x '$kill)
      (mget x 'array)
      (remcompary x)
      (when (member x (cdr $contexts) :test #'equal)
	($killcontext x))
      (when (mget x '$rule)
	(let ((y (ruleof x)))
	  (cond (y ($remrule y x))
		(t (when (not (member x *builtin-$rules* :test #'equal))
		     (fmakunbound x)
		     (setf $rules (delete x $rules :count 1 :test #'eq)))))))
      (when (and (get x 'operators) (rulechk x))
	($remrule x '$all))
      (when (mget x 'trace)
	(macsyma-untrace x))
      (when (get x 'translated)
	(when (not (member x *builtin-symbols* :test #'equal))
			 (remove-transl-fun-props x)
			 (remove-transl-array-fun-props x)))
      (when (not (get x 'sysconst))
	(remprop x 'lineinfo)
	(remprop x 'mprops))
      (dolist (u '(bindtest nonarray evfun evflag opers special mode))
	(remprop x u))
      (dolist (u opers)
	(when (and (remprop x u)
		 (let ((xopval (get x 'operators)))
		   (or (eq xopval 'simpargs1) (eq xopval nil))))
	    (remprop x 'operators)))
      (when (member x (cdr $props) :test #'equal)
	(remprop x 'sp2)
	(killframe x)
	(i-$remove (list x $features)))
      (let ((y (get x 'op)))
        (when (and y 
                   (not (member y *mopl* :test #'equal))
                   (member y (cdr $props) :test #'equal))
	  (kill-operator x)))
      (remalias x nil)
      (setf $arrays (delete x $arrays :count 1 :test #'eq))
      (rempropchk x)
      (setf *autoloaded-files*
	    (delete (assoc x *autoloaded-files* :test #'eq) *autoloaded-files* :count 1 :test #'equal))
      (setf $functions
	    (delete (assoc (ncons x) $functions :test #'equal) $functions :count 1 :test #'equal))
      (setf $macros
	    (delete (assoc (ncons x) $macros :test #'equal) $macros :count 1 :test #'equal))
      (let ((y (assoc (ncons x) $gradefs :test #'equal)))
	(when y
	  (remprop x 'grad)
	  (setf $gradefs (delete y $gradefs :count 1 :test #'equal))))
      (setf $dependencies
	    (delete (assoc (ncons x) $dependencies :test #'equal) $dependencies :count 1 :test #'equal))
      (let ((y (assoc-if #'(lambda (e) (equal x (car e))) (cdr $structures))))
        (when y
          (remprop x 'dimension)
          (remprop x 'defstruct-template)
          (remprop x 'defstruct-default)
          (remprop x 'translate)
          (setf $structures (delete y $structures :count 1 :test #'equal))))
      (when (and (member x *builtin-symbols* :test #'equal)
		 (gethash x *builtin-symbol-props*))
	(setf (symbol-plist x)
	      (copy-tree (gethash x *builtin-symbol-props*))))
      (when (member x *builtin-numeric-constants*)
	(initialize-numeric-constant x))	;; reset db value for $%pi, $%e, etc
      (if z (kill1 z)))))

(defun kill1 (x)
  (if (and (stringp x) (not (getopr0 x))) (return-from kill1 nil))
  (funcall
   #'(lambda (z)
       (cond ((and allbutl (member x allbutl :test #'equal)))
	     ((eq (setq x (getopr x)) '$labels)
	      (dolist (u (cdr $labels))
		(cond ((and allbutl (member u allbutl :test #'equal))
		       (setq z (nconc z (ncons u))))
		      (t (makunbound u) (remprop u 'time)
			 (remprop u 'nodisp))))
	      (setq $labels (cons '(mlist simp) z) $linenum 0))
	     ((member x '($values $arrays $aliases $rules $props
			$let_rule_packages) :test #'equal)
	      (mapc #'kill1 (cdr (symbol-value x))))
	     ((member x '($functions $macros $gradefs $dependencies $structures) :test #'equal)
	      (mapc #'(lambda (y) (kill1 (caar y))) (cdr (symbol-value x))))
	     ((eq x '$myoptions))
	     ((eq x '$tellrats) (setq tellratlist nil))
	     ((eq x '$ratvars) (setq $ratvars '((mlist simp)) varlist nil))
	     ((eq x '$ratweights) (setq *ratweights nil
					$ratweights '((mlist simp))))
	     ((eq x '$features)
	      (cond ((not (equal (cdr $features) featurel))
		     (setq $features (cons '(mlist simp) (copy-list featurel))))))
	     ((or (eq x t) (eq x '$all))
	      (mapc #'kill1 (cdr $infolists))
	      (setq $ratvars '((mlist simp)) varlist nil genvar nil
		    *checkfactors* nil greatorder nil lessorder nil $gensumnum 0
		    *ratweights nil $ratweights
		    '((mlist simp))
		    tellratlist nil $dontfactor '((mlist)) $setcheck nil)
	      (killallcontexts))
	     ((setq z (assoc x '(($inlabels . $inchar) ($outlabels . $outchar) ($linelabels . $linechar)) :test #'eq))
	      (mapc #'(lambda (y) (remvalue y '$kill))
		    (getlabels* (eval (cdr z)) nil)))
	     ((and (fixnump x) (>= x 0)) (remlabels x))
	     ((atom x) (kill1-atom x))
	     ((and (eq (caar x) 'mlist) (fixnump (cadr x))
		   (or (and (null (cddr x))
                         (setq x (append x (ncons (cadr x)))))
                      (and (fixnump (caddr x))
                         (not (> (cadr x) (caddr x))))))
	      (let (($linenum (caddr x))) (remlabels (- (caddr x) (cadr x)))))
	     ((setq z (mgetl (caar x) '(hashar array))) (remarrelem z x))
	     ((and ($subvarp x)
		   (boundp (caar x))
		   (hash-table-p (setq z (symbol-value (caar x)))))
	      ; Evaluate the subscripts (as is done in ARRSTORE)
	      (let ((indices (mevalargs (cdr x))))
		(if (gethash 'dim1 z)
		  (remhash (car indices) z)
		  (remhash indices z))))
         ((eq (caar x) '$@) (mrecord-kill x))
	     ((and (eq (caar x) '$allbut)
		   (not (dolist (u (cdr x))
			  (if (not (symbolp u)) (return t)))))
	      (let ((allbutl (cdr x))) (kill1 t)))
	     (t (improper-arg-err x '$kill))))
   nil))


(defun remlabels (n)
  (prog (l x)
     (setq l (list (exploden $inchar)
		   (exploden $outchar)
		   (exploden $linechar)))
     loop (setq x (mexploden $linenum))
     (do ((l l (cdr l)))
	 ((null l))
       (remvalue (implode (append (car l) x)) '$kill))
     (if (or (minusp (setq n (1- n))) (= $linenum 0)) (return nil))
     (decf $linenum)
     (go loop)))

(defun remvalue (x fn)
  (cond ((not (symbolp x)) (improper-arg-err x fn))
	((boundp x)
	 (let (y)
	   (cond ((or (setq y (member x (cdr $values) :test #'equal))
		      (member x (cdr $labels) :test #'equal))
		  (cond (y (setf $values (delete x $values :count 1 :test #'eq)))
			(t (setf $labels (delete x $labels :count 1 :test #'eq))
			   (remprop x 'time) (remprop x 'nodisp)))
		  (makunbound x)
		  (when (member x *builtin-symbols-with-values* :test #'equal)
		    (setf (symbol-value x)
			  (gethash x *builtin-symbol-values*)))
		  t)
		 ((get x 'special)
		  (makunbound x)
		  (when (member x *builtin-symbols-with-values* :test #'equal)
		    (setf (symbol-value x)
			  (gethash x *builtin-symbol-values*)))
		    t)
		 (transp (setf (symbol-value x) x) t)
		 ((eq x '$default_let_rule_package) t)
		 ;; Next case: X is bound to itself but X is not on values list.
		 ;; Translation code does that; I don't know why.
		 ;; Silently let it stand and hope it doesn't cause trouble.
		 ((eq (symbol-value x) x) t)
		 (t
		  (mtell (intl:gettext "remvalue: ~M doesn't appear to be a known variable; just unbind it anyway.~%") x)
		  (makunbound x)
		  t))))))

(defun ruleof (rule)
  (or (mget rule 'ruleof)
      (let* ((pattern (cadr (mget rule '$rule)))
	     (op (if (atom pattern) nil (caar pattern))) l)
	(and (setq l (get op 'rules))
	     (member rule l :test #'equal) op))))

(defmfun $debugmode (x)
  (debugmode1 nil x)
  (setq $debugmode x))

(defun debugmode1 (assign-var y)
  ;; The user manual says $debugmode has one of three values: false
  ;; (NIL), true (T), or lisp ($lisp).  Enforce that.
  (unless (member y '(nil t $lisp))
    (mseterr assign-var y "Must be one of false, true, or lisp"))
  (setq *mdebug* y))

(defun errlfun1 (mpdls)
  (do ((l bindlist (cdr l))
       (l1))
      ((eq l (car mpdls)) (munbind l1))
    (setq l1 (cons (car l) l1)))
  (do ()
      ((eq loclist (cdr mpdls)))
    (munlocal)))

(defun getalias (x)
  (cond ((get x 'alias))
	((eq x '$false) nil)
	(t x)))

(defun makealias (x)
  (implode (cons #\$ (exploden x))))

;; (DEFMSPEC $F (FORM) (SETQ FORM (FEXPRCHECK FORM)) ...)
;; makes sure that F was called with exactly one argument and
;; returns that argument.

(defun fexprcheck (form)
  (if (or (null (cdr form)) (cddr form))
      (merror (intl:gettext "~:M: expected just one argument; found: ~M") (caar form) form)
      (cadr form)))

(defun nonsymchk (x fn)
  (unless (symbolp x)
    (merror (intl:gettext "~:M: argument must be a symbol; found: ~M") fn x)))

(defmfun $print (&rest args)
  (if (null args)
      '((mlist simp))
      (let ((l args) $stringdisp) ;; Don't print out strings with quotation marks!
	(do ((l l (cddr l)))
	    ((null l))
	  (rplacd l (cons " " (cdr l))))
	(displa (cons '(mtext) l))
	(cadr (reverse l)))))

(defmspec $playback (x)
  (setq x (cdr x))
  (prog (l l1 l2 numbp slowp nostringp inputp timep grindp inchar largp)
     (setq inchar (getlabcharn $inchar)) ; Only the 1st alphabetic char. of $INCHAR is tested
     (setq timep $showtime grindp $grind)
     (do ((x x (cdr x)))( (null x))
       (cond ((fixnump (car x)) (setq numbp (car x)))
             ((eq (car x) '$all))
             ((eq (car x) '$slow) (setq slowp t))
             ((eq (car x) '$nostring) (setq nostringp t))
             ((eq (car x) '$grind) (setq grindp t))
             ((eq (car x) '$input) (setq inputp t))
             ((member (car x) '($showtime $time) :test #'equal) (setq timep (or timep t)))
             ((member (car x) '($gctime $totaltime) :test #'equal) (setq timep '$all))
             ((setq l2 (listargp (car x)))
              (setq l1 (nconc l1 (getlabels (car l2) (cdr l2) nil)) largp t))
             (t (improper-arg-err (car x) '$playback))))
     (cond ((and largp (null numbp)) (go loop))
           ((and (setq l (cdr $labels)) (not $nolabels)) (setq l (cdr l))))
     (when (or (null numbp) (< (length l) numbp))
       (setq l1 (reverse l)) (go loop))
     (do ((i numbp (1- i)) (l2)) ((zerop i) (setq l1 (nconc l1 l2)))
       (setq l2 (cons (car l) l2) l (cdr l)))
     loop (if (null l1) (return '$done))
     (let ((errset t)
           (incharp (char= (getlabcharn (car l1)) inchar)))
       (errset
        (cond ((and (not nostringp) incharp)
               (let ((*linelabel* (car l1))) (mterpri) (printlabel))
               (if grindp
                   (mgrind (meval1 (car l1)) nil)
                   (mapc #'(lambda (x) (write-char x)) (mstring (meval1 (car l1))))) ;gcl doesn't like a
                                      ; simple write-char, therefore wrapped it up in a lambda - are_muc
               (if (get (car l1) 'nodisp) (princ "$") (princ ";"))
               (mterpri))
              ((or incharp
                   (prog2 (when (and timep (setq l (get (car l1) 'time)))
                            (setq x (gctimep timep (cdr l)))
                            (mtell (intl:gettext "~A seconds") (car l))
                            (if x (mtell (intl:gettext "  GC time = ~A seconds") (cdr l)))
                            (mterpri))
                       (not (or inputp (get (car l1) 'nodisp)))))
               (mterpri) (displa (list '(mlabel) (car l1) (meval1 (car l1)))))
              (t (go a)))))
     (when (and slowp (cdr l1) (not (continuep)))
       (return '$terminated))
     a    (setq l1 (cdr l1))
     (go loop)))

(defun listargp (x)
  (let (high)
    (if (and ($listp x) (fixnump (cadr x))
           (or (and (null (cddr x)) (setq high (cadr x)))
              (and (fixnump (setq high (caddr x)))
                 (not (> (cadr x) high)))))
	(cons (cadr x) high))))

(defmspec $alias (form)
  (if (oddp (length (setq form (cdr form))))
      (merror (intl:gettext "alias: expected an even number of arguments.")))
  (do ((l nil (cons (alias (pop form) (pop form))
		    l)))
      ((null form)
       `((mlist simp),@(nreverse l)))))

(defun alias (x y)
  (cond ((nonsymchk x '$alias))
	((nonsymchk y '$alias))
        ((eq x y) y) ; x is already the alias of y
	((get x 'reversealias)
	 (merror (intl:gettext "alias: ~M already has an alias.") x))
	(t (putprop x y'alias)
	   (putprop y x 'reversealias)
	   (add2lnc y $aliases)
	   y)))

(defun remalias (x &optional remp)
  (let ((y (and (or remp (member x (cdr $aliases) :test #'equal)) (get x 'reversealias))))
    (cond ((and y (eq x '%derivative))
	   (remprop x 'reversealias)
	   (setf $aliases (delete x $aliases :count 1 :test #'eq))
	   (remprop '$diff 'alias) '$diff)
	  (y (remprop x 'reversealias)
	     (remprop x 'noun)
	     (setf $aliases (delete x $aliases :count 1 :test #'eq))
	     (remprop (setq x y) 'alias) (remprop x 'verb) x))))

(defun stripdollar (x)
  (cond ((not (atom x))
	 (cond ((and (eq (caar x) 'bigfloat) (not (minusp (cadr x)))) (implode (fpformat x)))
	       (t (merror (intl:gettext "STRIPDOLLAR: argument must be an atom; found: ~M") x))))
	((numberp x) x)
	((null x) 'false)
	((eq x t) 'true)
        ((member (get-first-char x) '(#\$ #\%) :test #'char=)
         (intern (subseq (string x) 1)))
	(t x)))

(defun fullstrip (x)
  (mapcar #'fullstrip1 x))

(defun fullstrip1 (x)
  (or (and (numberp x) x)
      (let ((y (get x 'reversealias))) (if y (stripdollar y)))
      (stripdollar x)))

(defun string* (x)
  (or (and (numberp x) (exploden x))
      (string*1 x)))

(defun string*1 (x)
  (let ($stringdisp $lispdisp)
    (makestring x)))

;;; Note that this function had originally stripped a prefix of '|M|.  This
;;; was intended for operators such as 'MABS, but with the case flipping
;;; performed by explodec this test would always fail.  Dependent code has
;;; been written assuming the '|M| prefix is not stripped so this test has
;;; been disabled for now.
;;;
(defmfun $nounify (x)
  (if (not (or (symbolp x) (stringp x)))
    (merror (intl:gettext "nounify: argument must be a symbol or a string; found: ~M") x))
  (setq x (amperchk x))
  (cond ((get x 'verb))
	((get x 'noun) x)
	(t
	 (let* ((y (explodec x))
		(u #+nil (member (car y) '($ |M| |m|) :test 'eq)
		   (eq (car y) '$)))
	   (cond ((or u (not (eq (car y) '%)))
		  (setq y (implode (cons '% (if u (cdr y) y))))
		  (putprop y x 'noun) (putprop x y 'verb))
		 (t x))))))

(defmfun $verbify (x)
  (if (not (or (symbolp x) (stringp x)))
    (merror (intl:gettext "verbify: argument must be a symbol or a string; found: ~M") x))
  (setq x (amperchk x))
  (cond ((get x 'noun))
        ((eq x '||) x)
	((and (char= (char (symbol-name x) 0) #\%)
	      (prog2
		  ($nounify (implode (cons #\$ (cdr (exploden x)))))
		  (get x 'noun))))
	(t x)))

(defmspec $string (form)
  (let (($lispdisp t))
    (setq form (strmeval (fexprcheck form)))
    (setq form (if $grind (strgrind form) (mstring form)))
    (coerce form 'string)))

(defun makstring (x)
  (setq x (mstring x))
  (do ((l x (cdr l)))
      ((null l))
    (rplaca l (ascii (car l))))
  x)

(defun strmeval (x)
  (cond ((atom x) (meval1 x))
	((member (caar x) '(msetq mdefine mdefmacro) :test #'equal) x)
	(t (meval x))))


(mapc #'(lambda (x) (putprop (car x) (cadr x) 'alias)
		(putprop (cadr x) (car x) 'reversealias))
      '(($block mprog) ($lambda lambda)
	($subst $substitute)
	($go mgo) ($signum %signum)
	($return mreturn) ($factorial mfactorial)
	($ibase *read-base*) ($obase *print-base*)
	($mode_declare $modedeclare)))

;; Validate values assigned to $ibase and $obase, which are aliased to
;; *read-base* and *print-base*, respectively, above.
(putprop '*read-base* #'(lambda (name val)
			  (unless (typep val '(integer 2 36))
			    (mseterr name val "must be an integer between 2 and 36, inclusive")))
	 'assign)

(putprop '*print-base* #'(lambda (name val)
			  (unless (typep val '(integer 2 36))
			    (mseterr name val "must be an integer between 2 and 36, inclusive")))
	 'assign)

(mapc #'(lambda (x) (putprop (car x) (cadr x) 'alias))
      '(($ratcoeff $ratcoef) ($ratnum $ratnumer) ($true t)
        ($derivative $diff) ($prod $product)
	($bothcoeff $bothcoef)))

(defun amperchk (name)
  (cond
    ((symbolp name) name)
    ((stringp name)
     (getalias (or (getopr0 name) (implode (cons #\$ (coerce name 'list))))))
    (t name)))

(defmspec $stringout (x)
  (setq x (cdr x))
  (let*
    ((file (namestring (maxima-string (meval (car x)))))
     (filespec (if (or (eq $file_output_append '$true) (eq $file_output_append t))
	`(savefile ,file :direction :output :if-exists :append :if-does-not-exist :create)
	`(savefile ,file :direction :output :if-exists :supersede :if-does-not-exist :create))))
    (setq x (cdr x))
    (eval
      `(let (maxima-error l1 truename)
	(declare (special $grind $strdisp))
	    (with-open-file ,filespec
	      (cond ((null
		      (errset
		       (do ((l ',x (cdr l)))( (null l))
			 (cond ((member (car l) '($all $input) :test #'equal)
				(setq l (nconc (getlabels* $inchar t) (cdr l))))
			       ((eq (car l) '$values)
				(setq l (nconc (mapcan
						#'(lambda (x)
						    (if (boundp x)
							(ncons (list '(msetq) x (symbol-value x)))))
						(cdr $values))
					       (cdr l))))
			       ((eq (car l) '$functions)
				(setq l (nconc (mapcar
						#'(lambda (x) (consfundef (caar x) nil nil))
						(cdr $functions))
					       (mapcan
						#'(lambda (x)
						    (if (mget x 'aexpr)
							(ncons (consfundef x t nil))))
						(cdr $arrays))
					       (mapcar
						#'(lambda (x) (consfundef (caar x) nil nil))
						(cdr $macros))
					       (cdr l))))
			       ((setq l1 (listargp (car l)))
				(setq l (nconc (getlabels (car l1) (cdr l1) t) (cdr l)))))
			 (if (null l) (return nil))
			 (terpri savefile)
			 (if $grind (mgrind (strmeval (car l)) savefile)
			     (princ (print-invert-case (maknam (mstring (strmeval (car l)))))
					    savefile))
			 (if (or (and (symbolp (car l)) (get (car l) 'nodisp)) (not $strdisp))
			     (write-char #\$ savefile)
			     (write-char #\; savefile)))))
		     (setq maxima-error t)))
	      (setq truename (truename savefile))
	      (terpri savefile))
	    (if maxima-error (merror (intl:gettext "stringout: unspecified error.")))
	    (cl:namestring truename)))))

(defmfun $labels (label-prefix)
  (nonsymchk label-prefix '$labels)
  (cons '(mlist simp) (nreverse (getlabels* label-prefix nil))))

(defmfun $%th (x)
  (prog (l outchar)
     (if (or (not (fixnump x)) (zerop x))
	 (improper-arg-err x '$%th))
     (if (> x 0) (setq x (- x)))
     (if (cdr $labels)
	 (setq l (cddr $labels) outchar (getlabcharn $outchar)))
     loop (if (null l) (merror (intl:gettext "%th: no such previous output: ~M") x))
     (if (and (char= (getlabcharn (car l)) outchar) (= (setq x (1+ x)) 0))
					; Only the 1st alphabetic character of $OUTCHAR is tested.
	 (return (meval (car l))))
     (setq l (cdr l))
     (go loop)))

(defun getlabels (n1 n2 flag)	; FLAG = T for STRINGOUT, = NIL for PLAYBACK and SAVE.
  (do ((i n1 (1+ i)) (l1)
       (l (if flag (list (exploden $inchar))
	      (list (exploden $inchar) (exploden $linechar)
		    (exploden $outchar)))))
      ((> i n2) (nreverse l1))
    (do ((l l (cdr l)) (x (mexploden i)) (z)) ((null l))
      (if (boundp (setq z (implode (append (car l) x))))
	  (setq l1 (cons z l1))))))

(defun getlabels* (label-prefix flag)		; FLAG = T only for STRINGOUT
  (let*
    ((label-prefix-name (symbol-name label-prefix))
     (label-prefix-length (length label-prefix-name)))
    (do ((l (if flag (cddr $labels) (cdr $labels)) (cdr l)) (l1))
        ((null l) l1)
        (let ((label-name-1 (symbol-name (car l))))
          (if
            (and 
              (<= label-prefix-length (length label-name-1))
              (string= label-name-1 label-prefix-name :end1 label-prefix-length))
            (setq l1 (cons (car l) l1)))))))

(defun getlabcharn (label)
  (let ((c (char (symbol-name label) 1)))
    (if (char= c #\%)
	(char (symbol-name label) 2)
	c)))

; Evaluate form while catching throws to some specific tags (called
; "errcatch tags").  If no throw to an errcatch tag is caught, then
; the values from form are returned.  If a throw to an errcatch tag
; is caught, then a Maxima error is signaled.
;
; The errcatch tags are ERRORSW, MACSYMA-QUIT and RAT-ERR.
(defmacro with-errcatch-tag-$errors (form)
  (let ((block-name (gensym)))
    `(block ,block-name
       ; RAT-ERROR-TO-MERROR will catch any throws to RAT-ERR and
       ; call merror with a specific error message.
       (catch 'macsyma-quit
         (catch 'errorsw
           (rat-error-to-merror
             (return-from ,block-name ,form))))
       ; If we're here, then we don't know any information about the
       ; error, so just call MERROR with a vague error message.  This
       ; message will not be printed by MERROR, but it will be stored
       ; in Maxima's error variable.
       (with-$error
         (merror (intl:gettext "An error was caught by errcatch."))))))

; This is similar to the classic errset, but errcatch handles lisp and
; Maxima errors.
(defmacro errcatch (form)
  `(let ((errcatch (cons bindlist loclist))
         (*mdebug* nil))
     (declare (special errcatch))
     (handler-case (list (with-errcatch-tag-$errors ,form))
       (maxima-$error ()
         ; If this was signaled by MERROR, then it has already handled
         ; the setting of the error variable and the printing of any error
         ; messages (as applicable).
         ;
         ; If for some reason this wasn't signaled by MERROR, then it's the
         ; signaler's responsibility to handle error messages.
         ;
         ; Either way, we just need to clean up here.
         (errlfun1 errcatch)
         nil)
       (error (e)
         ; We store the error report message in the error variable and
         ; print the message if errormsg is true.  Then we clean up.
         (setq $error (list '(mlist simp) (princ-to-string e)))
         (when $errormsg
           ($errormsg))
         (errlfun1 errcatch)
         nil))))

(defmspec $errcatch (form)
  (cons '(mlist) (errcatch (mevaln (cdr form)))))

(defmacro mcatch (form)
  `(let ((mcatch (cons bindlist loclist)))
     (unwind-protect
         (catch 'mcatch (rat-error-to-merror ,form))
       (errlfun1 mcatch))))

(defmspec $catch (form)
  (mcatch (mevaln (cdr form))))

(defmfun $throw (exp)
  (if (null mcatch) (merror (intl:gettext "throw: not within 'catch'; expression: ~M") exp))
  (throw 'mcatch exp))

(defmspec $time (l)
  (setq l (cdr l))
  (cons '(mlist simp)
	(mapcar
	 #'(lambda (x)
	     (or (and (symbolp x)
		      (setq x (get x 'time))
		      (if (= (cdr x) 0)
			  (car x)
			  (list '(mlist simp) (car x) (cdr x))))
		 '$unknown))
	 l)))

(defun timeorg (tim)
  (if (> thistime 0)
      (incf thistime (- (get-internal-run-time) tim))))


(defmfun $quit (&optional (exit-code 0))
  "Quit Maxima with an optional exit code for Lisps and systems that
  support exit codes."
  (princ *maxima-epilog*)
  (bye exit-code)
  (mtell (intl:gettext "quit: No known quit function for this Lisp.~%")))

;; File-processing stuff.

(defun mterpri ()
   (terpri)
   (finish-output))

(defmspec $status (form)
  (setq form (cdr form))
  (let* ((keyword (car form))
         (feature (cadr form)))
    (when (not (symbolp keyword))
      (merror (intl:gettext "status: first argument must be a symbol; found: ~M") keyword))
    (when (not (or (stringp feature) (symbolp feature)))
      (merror
        (intl:gettext "status: second argument must be symbol or a string; found: ~M") feature))
    (case keyword
      ($feature (cond ((null feature) (dollarify *features*))
                      ((member (intern (if (stringp feature)
                                           (maybe-invert-string-case feature)
                                           (symbol-name (fullstrip1 feature)))
                                       'keyword)
                               *features* :test #'equal) t)))
      (t (merror (intl:gettext "status: unknown argument: ~M") keyword)))))

(defquote $sstatus (keyword item)
  (cond ((equal keyword '$feature)
         (pushnew ($mkey item) *features*) t)
        ((equal keyword '$nofeature)
         (setq *features* (delete ($mkey item) *features*)) t)
        (t
         (merror (intl:gettext "sstatus: unknown argument: ~M") keyword))))

(dolist (l '($sin $cos $tan $log $plog $sec $csc $cot $sinh $cosh
	     $tanh $sech $csch $coth $asin $acos $atan $acot $acsc $asec $asinh
	     $acosh $atanh $acsch $asech $acoth $binomial $gamma $genfact $del))
  (let ((x ($nounify l)))
    (putprop l x 'alias)
    (putprop x l 'reversealias)))

($nounify '$sum)
($nounify '$lsum)
($nounify '$product)
($nounify '$integrate)
($nounify '$limit)

(defprop $diff %derivative verb)
(defprop %derivative $diff noun)

(eval-when
    (:compile-toplevel :execute)
    (setq *print-base* *old-base* *read-base* *old-ibase*))
