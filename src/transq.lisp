;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Compilation environment for TRANSLATED MACSYMA code.        ;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;;; this are COMPILE-TIME macros for TRANSLATE MACSYMA code.
;;; these guys are either SUBR's LSUBR's or FEXPRS in the interpreter.
;;; (ask me about why I used FEXPRS sometime ok.) -gjc.

(macsyma-module transq macro)
(load-macsyma-macros transm defopt)

;;; Already defined in transl module.
#-cl
(defvar $tr_semicompile nil)	       ; T if expanding for expr code.

;;; function for putting good info in the UNFASL file.


;;#+PDP10
;;(PROGN 'COMPILE

;;(DECLARE (SPECIAL CMSGFILES))

;;(DEFVAR MACRO-FILES NIL)

;;(DEFUN UNFASL-ANNOTATE-VERSIONS ()
;;  (LET ((UNFASL (IF (EQ (CAAR (NAMELIST (CAR CMSGFILES))) 'DSK)
;;		    (CAR CMSGFILES)
;;		    (CADR CMSGFILES))))
;;       (FORMAT UNFASL '|~%;; Compilation by ~A~%|
;;	       (STATUS UNAME))
;;       (FORMAT UNFASL '|;; ~15A~A~%|
;;	       '|Prelude file:|
;;	       (LET ((X (TRUENAME INFILE)))
;;		    (NAMESTRING (CONS (CDAR X) (CDR X)))))
;;       (FORMAT UNFASL '|;; ~15A| '|Macro files:|)
;;       (FORMAT UNFASL '|~{~<~%;; ~15X~:;~A ~A~>~^, ~}~%|
;;	       (DO ((L NIL (CONS (GET (CAR X) 'VERSION) (CONS (CAR X) L)))
;;		    (X MACRO-FILES (CDR X)))
;;		   ((NULL X) (NREVERSE L))))))
;;; END of #+PDP10
;;)

(defmacro def-mtrvar (v a &optional (priority 1))
  priority
  ;; ignored variable around for TRANSLATED files pre 
  ;; 3:03pm  Thursday, 11 March 1982 -gjc
  `(progn #-cl 'compile
    (declare-top (special ,v))

    (if (or (not (boundp ',v))
	    ;; a SYMBOL SET to ITSELF is considered to be
	    ;; UNBOUND for our purposes in Macsyma.
	    (eq ,v ',v))
	(setq ,v ,a))))

;; To do.
;; (DEFOPT TRD-MSYMEVAL (VAR &rest ignore)
;;   VAR)

(defvar *max-expt$-expand* 7)

(defopt expt$ (bas exp)
  (if (not (integerp exp))
      (maxima-error `(|Internal TRANSL error. Call GJC| ,bas ,exp)))
  (let* ((abs-exp (abs exp))
	 (full-exp (cond ((not (> exp *max-expt$-expand*))
			  `(internal-expt$ ,bas ,abs-exp))
			 (t
			  `(^$ ,bas ,abs-exp)))))
    (cond ((minusp exp)
	   `(//$ ,full-exp))
	  (t full-exp))))

(defopt internal-expt$ (exp-base pos-exp)
  (cond ((= pos-exp 0)
	 ;; BROM  wrote X^0 for symmetry in his code, and this
	 ;; macro did some infinite looping! oops.
	 ;; X^0 can only happen in hand-written code, in macros
	 ;; the general-representation simplifier will get rid
	 ;; of it.
	 1.0)
	((= pos-exp 1)
	 exp-base)
	((not (atom exp-base))
	 (let ((sym (gensym)))
	   `(let ((,sym ,exp-base))
	     (declare (flonum ,sym))
	     (internal-expt$ ,sym ,pos-exp))))
	((= pos-exp 2)
	 `(*$ ,exp-base ,exp-base))
	((= pos-exp 3) `(*$ (*$ ,exp-base ,exp-base) ,exp-base))
	((= pos-exp 4)
	 `(internal-expt$ (internal-expt$ ,exp-base 2) 2))
	((= pos-exp 5)
	 `(*$ (internal-expt$ ,exp-base 4) ,exp-base))
	((= pos-exp 6)
	 `(internal-expt$ (internal-expt$ ,exp-base 3) 2))
	((= pos-exp 7)
	 `(*$ ,exp-base (internal-expt$ ,exp-base 6)))
	(t
	 `(*$ ,@(listn exp-base pos-exp)))))

;;; There is a real neat and fancy way to do the above for arbitrary N
;;; repeated squaring in a recrusive fashion.  It is trivial to do
;;; and should be done at some point.

;; (LISTN 'A 3) --> (A A A)

(defun listn (x n)
  (do ((l nil (cons x l)))
      ((minusp (setq n (f1- n))) l)))

#+pdp10
(progn 'compile

       (defvar *known-functions-info-stack* nil
	 "When MDEFUN expands it puts stuff here for MFUNCTION-CALL
	to use.")
       (defvar *unknown-functions-info-stack* nil
	 "When MFUNCTION-CALL expands without info from
	*KNOWN-FUNCTIONS-INFO-STACK* it puts stuff here to be barfed
	at the end of compilation.")

       (defopt mfunction-call (f &rest argl
				 &aux (info (get-info f *known-functions-info-stack*)))
	 (cond ((or (memq info '(lexpr expr))
		    (getl f '(*expr *lexpr)))
		`(,f ,@ (copy-rest-arg argl)))
	       ((get f '*fexpr)
		(format msgfiles
			"~&(Comment *Macsyma* unhandled `fexpr' ~S may barf)~%"
			f)
		`(,f ,@ (copy-rest-arg argl)))
	       ((eq info 'luser)
		(comment ???)
		`(apply ',f ',(copy-rest-arg argl)))
	       (t
		(push-info f argl *unknown-functions-info-stack*)
		`(funcall (progn ',f) ,@  argl))))

;;; A call to this macro is pushed onto the EOF-COMPILE-QUEUE
       (declare (special ttynotes))
       (defmacro unknown-functions-comment ()
	 (let ((unknowns (resolve-known-and-unknown-functions))
	       (m1 "*MACSYMA* ")
	       (m2 "
        -are user functions used but not defined in this file."))
	   (cond (unknowns
		  (setq unknowns
			`(comment ,m1 ,unknowns ,m2))
		  (cond (ttynotes
			 (terpri tyo)
			 (print unknowns tyo)
			 (terpri tyo)))
		  unknowns))))

       (defun resolve-known-and-unknown-functions ()
	 (do ((un))
	     ((null *unknown-functions-info-stack*)
	      un)
	   (let ((ind (top-ind *unknown-functions-info-stack*)))
	     (pop-info ind *unknown-functions-info-stack*)
	     (cond ((pop-info ind *known-functions-info-stack*))
		   (t
		    (push ind un))))))
       ;; END OF #+PDP10
       )

#-(or pdp10 cl)
(defopt mfunction-call (f &rest l)
  (cons f l))


#+cl
(defopt mfunction-call (f &rest l &aux l1)
  #+lispm (setq l1 (copy-rest-arg l))
  #-lispm (setq l1 l)
  (cond((or (fboundp f)
	    (get f 'once-translated)
	    (get f 'translated))
	(cons f l1))
       (t `(lispm-mfunction-call-aux ',f ', l1
	    (list ,@ l1) nil))))


;;; macros for compiled environments.

;;; (FUNGEN&ENV-for-meval <eval vars list> <late eval vars list> .  <EXP>)
;;; will define a function globally with a unique name
;;; (defun <name> <list of variables> <exp>). And return
;;; `((<name>) ,@<eval>> . <late eval>). The resulting expression may
;;; then be passed to a function which will bind variables from
;;; the <late eval vars list> and possibly other variables free in
;;; <exp> and then call MEVAL on the expression.
;;; FUNGEN&ENV-FOR-MEVALSUMARG will also make sure that the <name>
;;; has an mevalsumarg property of T.
;;; the expression was translated using TR-LAMBDA.

(defvar *infile-name-key* '||
  "This is a key gotten from the infile name, in the interpreter
  other completely hackish things with FSUBRS will go on.")
 
#+maclisp
(defun gen-name ( &optional k &aux (n '#,(*array nil 'fixnum 1)))
  (store (arraycall fixnum n 0) (f1+ (arraycall fixnum n 0)))
  (and k (store (arraycall fixnum n 0) k))
  (implode (append (exploden *infile-name-key*)
		   (exploden '|-tr-gen-|)
		   (exploden (arraycall fixnum n 0)))))

#+(or cl nil)
(progn 'compile
       (defvar a-random-counter-for-gen-name 0)
       (defun gen-name (&optional igno) igno
	      (intern (format nil "~A ~A #~D"
			      (status site)
			      (get-universal-time)
			      (setq a-random-counter-for-gen-name
				    (f1+ a-random-counter-for-gen-name)))))
       )

(defun ensure-a-constant-for-meval (exp)
  (cond ((or (numberp exp) (memq exp '(t nil)))
	 exp)
	(t
	 `(let ((val ,exp))
	   (cond ((or (numberp val) (memq val '(t nil)))
		  val)
		 (t (list '(mquote simp) val)))))))

(defmacro proc-ev (x)
  `(mapcar #'ensure-a-constant-for-meval ,x))

(defvar forms-to-compile-queue ())

(defun compile-forms-to-compile-queue-now ()  
  (cond ( forms-to-compile-queue
	 (loop for v in forms-to-compile-queue
		do (eval v) (compile (second v)))))
  (setq forms-to-compile-queue nil))

(defmacro compile-forms-to-compile-queue ()
  (if forms-to-compile-queue
      (nconc (list 'progn ''compile)
	     (prog1 forms-to-compile-queue
	       (setq forms-to-compile-queue nil))
	     (list '(compile-forms-to-compile-queue)))))

(defun emit-defun (exp)
  (if $tr_semicompile (setq exp `(progn ,exp)))
  #-cl
  (setq forms-to-compile-queue (nconc forms-to-compile-queue (list exp)))
  #+cl
  (let #+lispm ((default-cons-area working-storage-area)) #-lispm nil
       (setq forms-to-compile-queue (nconc forms-to-compile-queue (list (copy-tree exp))))))

#+ignore
(defopt fungen&env-for-meval (ev ev-late exp
				 &aux (name (gen-name)))
  (emit-defun `(defun ,name (,@ev ,@ev-late) ,exp))
  `(list* '(,name) ,@(proc-ev ev)
    ',ev-late))
#+ignore
(defopt fungen&env-for-mevalsumarg (ev ev-late tr-exp mac-exp
				       &aux (name (gen-name)))
  (emit-defun
   `(defun ,name (,@ev-late)
     (let ((,ev (get ',name 'sumarg-env)))
       ,tr-exp)))
  
  (emit-defun
   `(defun-prop (,name mevalsumarg-macro) (*ignored*)
     (mbinding (',ev (get ',name 'sumarg-env))
      (mevalatoms ',mac-exp))))
  
  `(progn (putprop ',name (list ,@ev) 'sumarg-env)
    (list '(,name) ',@ev-late)))


(defmacro pop-declare-statement (l)
  `(and (not (atom (car ,l)))
    (eq (caar ,l) 'declare)
    (pop ,l)))


;;; Lambda expressions emitted by the translator.

;; lambda([u,...],...) where any free unquoted variable in the body is
;; either unbound or globally bound or locally bound in some
;; non-enclosing block.  At this point, BODY has already the correct
;; special declarations for elements of ARGL.
(defmacro m-tlambda (argl &body body)
  `(function
    (lambda ,argl
     ,@body)))

;; lambda([u,...,[v]],...) with the same condition as above.
(defmacro m-tlambda& (argl &rest body)
  `(function (lambda (,@(reverse (cdr (reverse argl)))
		      &rest ,@(last argl))
     ,(pop-declare-statement body)
     (setq ,(car (last argl))
	   (cons '(mlist) ,(car (last argl))))
     ,@ body)))

;; lambda([u,...],...) with free unquoted variables in the body which
;; have a local binding in some enclosing block, but no global one,
;; i.e, the complement of the condition for m-tlambda above.
(defmacro m-tlambda&env ((reg-argl env-argl) &body body)
  (declare (ignore env-argl))
  `(function
    (lambda ,reg-argl
     ;;(,@(or (pop-declare-statement body) '(declare)) (special ,@env-argl))
     ,@body)))

;; lambda([u,...,[v]],...) with the same condition as above.
(defmacro m-tlambda&env& ((reg-argl env-argl) &body body)
  (declare (ignore env-argl))
  (let ((last-arg (car (last reg-argl))))
    `(function
      (lambda (,@(butlast reg-argl) &rest ,last-arg)
       ;;(,@(or (pop-declare-statement body) '(declare)) (special ,@env-argl))
       ,(pop-declare-statement body)
       (setq ,last-arg (cons '(mlist) ,last-arg))
       ,@body))))

;; ???
;; (loop for v in '(m-tlambda m-tlambda& m-tlambda&env m-tlambda&env&)
;;        do
;;        (remprop v 'opt)
;;        #+lispm
;;        (remprop v 'compiler:optimizers))

;;#+cl  ;;wrap function around the lambda forms.. 
;;(progn 'compile
;;(defmacro M-TLAMBDA (&REST L )
;;  `(lambda ,@ (copy-rest-arg  l)))
;;(defmacro m-tlambda& (argl &rest body)
;;  `(lambda (,@(REVERSE (CDR (REVERSE ARGL)))
;;	    &REST ,@(LAST ARGL))
;;     ,(pop-declare-statement body)
;;     (SETQ ,(CAR (LAST ARGL))
;;	   (CONS '(MLIST) ,(CAR (LAST ARGL))))
;;     ,@ BODY))

;;(DEFmacro M-TLAMBDA&ENV ( argl &REST BODY
;;		       &AUX (NAME (GEN-NAME))
;;		       (reg-argl (first argl))(env-argl (second argl)))
;;  `(lambda (,@ reg-argl) ,@ (copy-rest-arg body)))
;;(defmacro M-TLAMBDA&ENV&  ( argl &REST BODY &aux (reg-argl (first argl)))
;;  `(lambda ( ,@REG-ARGL) ,@ (copy-rest-arg BODY)))
;;(loop for v in '(m-tlambda m-tlambda& m-tlambda&env m-tlambda&env&)
;;      do
;;      (remprop v 'opt)
;;      (remprop v 'compiler:optimizers))
;;)

;;#+lispm
;;(DEFOPT M-TLAMBDA (&REST L &AUX (NAME (GEN-NAME)))
;;    (EMIT-DEFUN
;;    `(DEFPROP ,NAME APPLICATION-OPERATOR OPERATORS))
;;
;;  `(lambda ,@ (copy-rest-arg  l)))
;;
#-cl
(defopt m-tlambda (&rest l &aux (name (gen-name)))
  (emit-defun `(defun ,name ,@ (copy-rest-arg l)))
  
  ;; just in case this is getting passed in as
  ;; SUBST(LAMBDA([U],...),"FOO",...)
  ;; this little operator property will make sure the right thing
  ;; happens!
  
  (emit-defun
   `(defprop ,name application-operator operators))
  ;; must be 'NAME since #'NAME doesn't point to the operators
  ;; property.
  `',name)

;;(DEFOPT M-TLAMBDA& (ARGL &REST BODY &AUX body1 (NAME (GEN-NAME)))
;;  (setq body1 (copy-rest-arg body))
;;   `(lambda (,@(REVERSE (CDR (REVERSE ARGL)))
;;		  &REST ,@(LAST ARGL))
;;      ,(pop-declare-statement body1)
;;      (SETQ ,(CAR (LAST ARGL))
;;	    (CONS '(MLIST) ,(CAR (LAST ARGL))))
;;      ,@BODY1))


#-cl       
(defopt m-tlambda& (argl &rest body &aux (name (gen-name)))
  (emit-defun
   `(defun ,name (,@(reverse (cdr (reverse argl)))
		  &rest ,@(last argl))
     ,(pop-declare-statement body)
     (setq ,(car (last argl))
      (cons '(mlist) ,(car (last argl))))
     ,@body))

  (emit-defun `(defprop ,name application-operator operators))
  `',name)

(defun for-eval-then-quote (var)
  `(list 'quote ,var))

(defun for-eval-then-quote-argl (argl)
  (mapcar 'for-eval-then-quote argl))

;; Problem: You can pass a lambda expression around in macsyma
;; because macsyma "general-rep" has a CAR which is a list.
;; Solution: Just as well anyway.

;;;eliminated that named function business for lispm

;;(DEFOPT M-TLAMBDA&ENV ( argl &REST BODY
;;		       &AUX  fun
;;		       (reg-argl (first argl))(env-argl (second argl)))
;;  (setq fun `(lambda  (,@ENV-ARGL ,@REG-ARGL)
;;		 ,@ (copy-rest-arg BODY)))
;;  `(MAKE-ALAMBDA ',REG-ARGL
;;	 (LIST* ',fun ,@(FOR-EVAL-THEN-QUOTE-ARGL ENV-ARGL) ',REG-ARGL)))

#+cl ;;the lexical scoping  handles the environment in most cases
;;and it is messy to queue things 

 
#-cl
(defopt m-tlambda&env ( argl &rest body
			     &aux (name (gen-name))
			     (reg-argl (first argl))(env-argl (second argl)))
  (emit-defun `(defun ,name (,@env-argl ,@reg-argl)
		,@ (copy-rest-arg body)))
  `(make-alambda ',reg-argl
    (list* ',name ,@(for-eval-then-quote-argl env-argl) ',reg-argl)))

#-cl
(defopt m-tlambda&env&  ( argl &rest body
			       &aux (name (gen-name))
			       (reg-argl (first argl))(env-argl (second argl)))
  (emit-defun `(defun ,name (,@env-argl ,@reg-argl) ,@ (copy-rest-arg body)))
  `(make-alambda '*n*
    (list* ',name ,@(for-eval-then-quote-argl env-argl)
     ',(do ((n (length reg-argl))
	    (j 1 (f1+ j))
	    (l nil))
	   ((= j n)
	    (push `(cons '(mlist) (listify (f- ,(f1- n) *n*))) l)
	    (nreverse l))
	   (push `(arg ,j) l)))))

;;; this is the important case for numerical hackery.

(defun declare-snarf (body)
  (cond ((and (not (atom (car body)))
	      (eq (caar body) 'declare))
	 (list (car body)))
	(t nil)))


;;; I will use the special variable given by the NAME as a pointer to
;;; an environment.

(defopt m-tlambda-i (mode env argl &rest body
			  &aux (name (gen-name))
			  (declarep (declare-snarf body)))
  (cond ((eq mode '$float)
	 (emit-defun `(declare (flonum (,name ,@(listn nil (length argl))))))
	 (emit-defun `(defprop ,name t flonum-compiled))))
  (emit-defun
   `(defun ,name ,argl
     ,@declarep
     (let ((,env ,name))
       ,@(cond (declarep (cdr (copy-rest-arg body)))
	       (t (copy-rest-arg  body))))))
  (emit-defun #-nil `(setq ,name ',(listn nil (length env)))
	      #+nil `(defparameter ,name (make-list ,(length env)))
	      )
  `(progn (set-vals-into-list ,env ,name)
    (quote ,name)))

;;; This is not optimal code.
;;; I.E. IT SUCKS ROCKS.

(defmacro set-vals-into-list (argl var)
  (do ((j 0 (f1+ j))
       (argl argl (cdr argl))
       (l nil
	  `((setf (nth ,j ,var) ,(car argl)) ,@l)))
      ((null argl) `(progn ,@l))))








