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
(DEFVAR $TR_SEMICOMPILE NIL) ; T if expanding for expr code.

;;; function for putting good info in the UNFASL file.


;#+PDP10
;(PROGN 'COMPILE

;(DECLARE (SPECIAL CMSGFILES))

;(DEFVAR MACRO-FILES NIL)

;(DEFUN UNFASL-ANNOTATE-VERSIONS ()
;  (LET ((UNFASL (IF (EQ (CAAR (NAMELIST (CAR CMSGFILES))) 'DSK)
;		    (CAR CMSGFILES)
;		    (CADR CMSGFILES))))
;       (FORMAT UNFASL '|~%;; Compilation by ~A~%|
;	       (STATUS UNAME))
;       (FORMAT UNFASL '|;; ~15A~A~%|
;	       '|Prelude file:|
;	       (LET ((X (TRUENAME INFILE)))
;		    (NAMESTRING (CONS (CDAR X) (CDR X)))))
;       (FORMAT UNFASL '|;; ~15A| '|Macro files:|)
;       (FORMAT UNFASL '|~{~<~%;; ~15X~:;~A ~A~>~^, ~}~%|
;	       (DO ((L NIL (CONS (GET (CAR X) 'VERSION) (CONS (CAR X) L)))
;		    (X MACRO-FILES (CDR X)))
;		   ((NULL X) (NREVERSE L))))))
;;; END of #+PDP10
;)

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

(DEFVAR *MAX-EXPT$-EXPAND* 7)

(DEFOPT EXPT$ (BAS EXP)
  (if (not (integerp exp))
      (MAXIMA-ERROR `(|Internal TRANSL error. Call GJC| ,BAS ,EXP)))
  (LET* ((ABS-EXP (ABS EXP))
	 (FULL-EXP (COND ((NOT (> EXP *MAX-EXPT$-EXPAND*))
			  `(INTERNAL-EXPT$ ,BAS ,ABS-EXP))
			 (T
			  `(^$ ,BAS ,ABS-EXP)))))
    (COND ((MINUSP EXP)
	   `(//$ ,FULL-EXP))
	  (T FULL-EXP))))

(DEFOPT INTERNAL-EXPT$ (EXP-BASE POS-EXP)
  (COND ((= POS-EXP 0)
	 ;; BROM  wrote X^0 for symmetry in his code, and this
	 ;; macro did some infinite looping! oops.
	 ;; X^0 can only happen in hand-written code, in macros
	 ;; the general-representation simplifier will get rid
	 ;; of it.
	 1.0)
	((= POS-EXP 1)
	 EXP-BASE)
	((NOT (ATOM EXP-BASE))
	 (LET ((SYM (GENSYM)))
	   `(LET ((,SYM ,EXP-BASE))
	      (DECLARE (FLONUM ,SYM))
	      (INTERNAL-EXPT$ ,SYM ,POS-EXP))))
	((= POS-EXP 2)
	 `(*$ ,EXP-BASE ,EXP-BASE))
	((= POS-EXP 3) `(*$ (*$ ,EXP-BASE ,EXP-BASE) ,EXP-BASE))
	((= POS-EXP 4)
	 `(INTERNAL-EXPT$ (INTERNAL-EXPT$ ,EXP-BASE 2) 2))
	((= pos-EXP 5)
	 `(*$ (INTERNAL-EXPT$ ,EXP-BASE 4) ,EXP-BASE))
	((= pos-exp 6)
	 `(internal-expt$ (internal-expt$ ,EXP-BASE 3) 2))
	((= pos-exp 7)
	 `(*$ ,EXP-BASE (internal-expt$ ,EXP-BASE 6)))
	(T
	 `(*$ ,@(LISTN EXP-BASE POS-EXP)))))

;;; There is a real neat and fancy way to do the above for arbitrary N
;;; repeated squaring in a recrusive fashion.  It is trivial to do
;;; and should be done at some point.

;; (LISTN 'A 3) --> (A A A)

(DEFUN LISTN (X N)
  (DO ((L NIL (CONS X L)))
      ((MINUSP (SETQ N (f1- N))) L)))

#+PDP10
(PROGN 'COMPILE

(DEFVAR *KNOWN-FUNCTIONS-INFO-STACK* NIL
	"When MDEFUN expands it puts stuff here for MFUNCTION-CALL
	to use.")
(DEFVAR *UNKNOWN-FUNCTIONS-INFO-STACK* NIL
	"When MFUNCTION-CALL expands without info from
	*KNOWN-FUNCTIONS-INFO-STACK* it puts stuff here to be barfed
	at the end of compilation.")

(DEFOPT MFUNCTION-CALL (F &REST ARGL
			  &AUX (INFO (GET-INFO F *KNOWN-FUNCTIONS-INFO-STACK*)))
  (COND ((OR (MEMQ INFO '(LEXPR EXPR))
	     (GETL F '(*EXPR *LEXPR)))
	 `(,F ,@ (copy-rest-arg ARGL)))
	((GET F '*FEXPR)
	 (FORMAT MSGFILES
		 "~&(COMMENT *MACSYMA* unhandled FEXPR ~S may barf)~%"
		 F)
	 `(,F ,@ (copy-rest-arg ARGL)))
	((EQ INFO 'LUSER)
	 (COMMENT ???)
	 `(APPLY ',F ',(copy-rest-arg ARGL)))
	(T
	 (PUSH-INFO F ARGL *UNKNOWN-FUNCTIONS-INFO-STACK*)
	 `(funcall (progn ',f) ,@  argl))))

;;; A call to this macro is pushed onto the EOF-COMPILE-QUEUE
(DECLARE (SPECIAL TTYNOTES))
(DEFMACRO UNKNOWN-FUNCTIONS-COMMENT ()
  (LET ((UNKNOWNS (RESOLVE-KNOWN-AND-UNKNOWN-FUNCTIONS))
	(M1 "*MACSYMA* ")
	(M2 "
        -are user functions used but not defined in this file."))
    (COND (UNKNOWNS
	   (SETQ UNKNOWNS
		 `(COMMENT ,M1 ,UNKNOWNS ,M2))
	   (COND (TTYNOTES
		  (TERPRI TYO)
		  (PRINT UNKNOWNS TYO)
		  (TERPRI TYO)))
	   UNKNOWNS))))

(DEFUN RESOLVE-KNOWN-AND-UNKNOWN-FUNCTIONS ()
  (DO ((UN))
      ((NULL *UNKNOWN-FUNCTIONS-INFO-STACK*)
       UN)
    (LET ((IND (TOP-IND *UNKNOWN-FUNCTIONS-INFO-STACK*)))
      (POP-INFO IND *UNKNOWN-FUNCTIONS-INFO-STACK*)
      (COND ((POP-INFO IND *KNOWN-FUNCTIONS-INFO-STACK*))
	    (T
	     (PUSH IND UN))))))
;; END OF #+PDP10
)

#-(or PDP10 cl)
(DEFOPT MFUNCTION-CALL (F &REST L)
  (CONS F L))


#+cl
(DEFOPT MFUNCTION-CALL (F &REST L &aux l1)
  #+lispm (setq l1 (copy-rest-arg l))
  #-lispm (setq l1 l)
  (cond((or (fboundp f)
	    (get f 'once-translated)
	    (get f 'translated))
	(CONS F l1))
       (t `(lispm-MFUNCTION-CALL-AUX ',f ', l1
				     (list ,@ l1) NIL))))


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

(DEFVAR *INFILE-NAME-KEY* '||
  "This is a key gotten from the infile name, in the interpreter
  other completely hackish things with FSUBRS will go on.")
 
#+Maclisp
(DEFUN GEN-NAME ( &OPTIONAL K &AUX (N '#,(*ARRAY NIL 'fixnum 1)))
  (STORE (ARRAYCALL FIXNUM N 0) (f1+ (ARRAYCALL FIXNUM N 0)))
  (AND K (STORE (ARRAYCALL FIXNUM N 0) K))
  (IMPLODE (APPEND (EXPLODEN *INFILE-NAME-KEY*)
		   (EXPLODEN '|-tr-gen-|)
		   (EXPLODEN (ARRAYCALL FIXNUM N 0)))))

#+(OR CL NIL)
(PROGN 'COMPILE
(defvar a-random-counter-for-gen-name 0)
(DEFUN GEN-NAME (&OPTIONAL IGNO) igno
  (intern (format nil "~A ~A #~D"
		  (status site)
		  (get-universal-time)
		  (setq a-random-counter-for-gen-name
			(f1+ a-random-counter-for-gen-name)))))
)

(DEFUN ENSURE-A-CONSTANT-FOR-MEVAL (EXP)
  (COND ((OR (NUMBERP EXP) (MEMQ EXP '(T NIL)))
	 EXP)
	(T
	 `(LET ((VAL ,EXP))
	    (COND ((OR (NUMBERP VAL) (MEMQ VAL '(T NIL)))
		   VAL)
		  (T (LIST '(MQUOTE SIMP) VAL)))))))

(DEFMACRO PROC-EV (X)
  `(MAPCAR #'ENSURE-A-CONSTANT-FOR-MEVAL ,X))

(defvar forms-to-compile-queue ())

(defun compile-forms-to-compile-queue-now ()  
  (cond ( FORMS-TO-COMPILE-QUEUE
	 (sloop for v in FORMS-TO-COMPILE-QUEUE
                   do (eval v) (compile (second v)))))
  (SETQ FORMS-TO-COMPILE-QUEUE NIL))

(defmacro compile-forms-to-compile-queue ()
  (IF FORMS-TO-COMPILE-QUEUE
      (NCONC (LIST 'PROGN ''COMPILE)
	     (PROG1 FORMS-TO-COMPILE-QUEUE
		    (SETQ FORMS-TO-COMPILE-QUEUE NIL))
	     (LIST '(COMPILE-FORMS-TO-COMPILE-QUEUE)))))

(DEFUN EMIT-DEFUN (EXP)
  (IF $TR_SEMICOMPILE (SETQ EXP `(PROGN ,EXP)))
  #-CL
  (SETQ FORMS-TO-COMPILE-QUEUE (NCONC FORMS-TO-COMPILE-QUEUE (LIST EXP)))
  #+CL
  (let #+lispm ((default-cons-area working-storage-area)) #-lispm nil
      (SETQ FORMS-TO-COMPILE-QUEUE (NCONC FORMS-TO-COMPILE-QUEUE (LIST (COPY-TREE EXP))))))

#+ignore
(DEFOPT FUNGEN&ENV-FOR-MEVAL (EV EV-LATE EXP
				   &AUX (NAME (GEN-NAME)))
  (EMIT-DEFUN `(DEFUN ,NAME (,@EV ,@EV-LATE) ,EXP))
  `(LIST* '(,NAME) ,@(PROC-EV EV)
	  ',EV-LATE))
#+ignore
(DEFOPT FUNGEN&ENV-FOR-MEVALSUMARG (EV EV-LATE TR-EXP MAC-EXP
					 &AUX (NAME (GEN-NAME)))
  (EMIT-DEFUN
   `(DEFUN ,NAME (,@EV-LATE)
      (LET ((,EV (GET ',NAME 'SUMARG-ENV)))
	,TR-EXP)))
  
  (EMIT-DEFUN
   `(DEFUN-prop (,NAME MEVALSUMARG-MACRO) (*IGNORED*)
      (MBINDING (',EV (GET ',NAME 'SUMARG-ENV))
		(MEVALATOMS ',MAC-EXP))))
  
  `(PROGN (PUTPROP ',NAME (LIST ,@EV) 'SUMARG-ENV)
	  (LIST '(,NAME) ',@EV-LATE)))


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
  `(function (lambda (,@(REVERSE (CDR (REVERSE ARGL)))
		      &REST ,@(LAST ARGL))
     ,(pop-declare-statement body)
     (SETQ ,(CAR (LAST ARGL))
	   (CONS '(MLIST) ,(CAR (LAST ARGL))))
     ,@ BODY)))

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
;; (sloop for v in '(m-tlambda m-tlambda& m-tlambda&env m-tlambda&env&)
;;        do
;;        (remprop v 'opt)
;;        #+lispm
;;        (remprop v 'compiler:optimizers))

;#+cl  ;;wrap function around the lambda forms.. 
;(progn 'compile
;(defmacro M-TLAMBDA (&REST L )
;  `(lambda ,@ (copy-rest-arg  l)))
;(defmacro m-tlambda& (argl &rest body)
;  `(lambda (,@(REVERSE (CDR (REVERSE ARGL)))
;	    &REST ,@(LAST ARGL))
;     ,(pop-declare-statement body)
;     (SETQ ,(CAR (LAST ARGL))
;	   (CONS '(MLIST) ,(CAR (LAST ARGL))))
;     ,@ BODY))

;(DEFmacro M-TLAMBDA&ENV ( argl &REST BODY
;		       &AUX (NAME (GEN-NAME))
;		       (reg-argl (first argl))(env-argl (second argl)))
;  `(lambda (,@ reg-argl) ,@ (copy-rest-arg body)))
;(defmacro M-TLAMBDA&ENV&  ( argl &REST BODY &aux (reg-argl (first argl)))
;  `(lambda ( ,@REG-ARGL) ,@ (copy-rest-arg BODY)))
;(sloop for v in '(m-tlambda m-tlambda& m-tlambda&env m-tlambda&env&)
;      do
;      (remprop v 'opt)
;      (remprop v 'compiler:optimizers))
;)

;#+lispm
;(DEFOPT M-TLAMBDA (&REST L &AUX (NAME (GEN-NAME)))
;    (EMIT-DEFUN
;    `(DEFPROP ,NAME APPLICATION-OPERATOR OPERATORS))
;
;  `(lambda ,@ (copy-rest-arg  l)))
;
#-cl
(DEFOPT M-TLAMBDA (&REST L &AUX (NAME (GEN-NAME)))
  (EMIT-DEFUN `(DEFUN ,NAME ,@ (copy-rest-arg L)))
  
  ;; just in case this is getting passed in as
  ;; SUBST(LAMBDA([U],...),"FOO",...)
  ;; this little operator property will make sure the right thing
  ;; happens!
  
  (EMIT-DEFUN
    `(DEFPROP ,NAME APPLICATION-OPERATOR OPERATORS))
  ;; must be 'NAME since #'NAME doesn't point to the operators
  ;; property.
  `',NAME)

;(DEFOPT M-TLAMBDA& (ARGL &REST BODY &AUX body1 (NAME (GEN-NAME)))
;  (setq body1 (copy-rest-arg body))
;   `(lambda (,@(REVERSE (CDR (REVERSE ARGL)))
;		  &REST ,@(LAST ARGL))
;      ,(pop-declare-statement body1)
;      (SETQ ,(CAR (LAST ARGL))
;	    (CONS '(MLIST) ,(CAR (LAST ARGL))))
;      ,@BODY1))


#-cl       
(DEFOPT M-TLAMBDA& (ARGL &REST BODY &AUX (NAME (GEN-NAME)))
  (EMIT-DEFUN
   `(DEFUN ,NAME (,@(REVERSE (CDR (REVERSE ARGL)))
		  &REST ,@(LAST ARGL))
      ,(pop-declare-statement body)
      (SETQ ,(CAR (LAST ARGL))
	    (CONS '(MLIST) ,(CAR (LAST ARGL))))
      ,@BODY))

  (EMIT-DEFUN `(DEFPROP ,NAME APPLICATION-OPERATOR OPERATORS))
  `',NAME)

(DEFUN FOR-EVAL-THEN-QUOTE (VAR)
  `(list 'QUOTE ,VAR))

(DEFUN FOR-EVAL-THEN-QUOTE-ARGL (ARGL)
  (MAPCAR 'FOR-EVAL-THEN-QUOTE ARGL))

;; Problem: You can pass a lambda expression around in macsyma
;; because macsyma "general-rep" has a CAR which is a list.
;; Solution: Just as well anyway.

;;;eliminated that named function business for lispm

;(DEFOPT M-TLAMBDA&ENV ( argl &REST BODY
;		       &AUX  fun
;		       (reg-argl (first argl))(env-argl (second argl)))
;  (setq fun `(lambda  (,@ENV-ARGL ,@REG-ARGL)
;		 ,@ (copy-rest-arg BODY)))
;  `(MAKE-ALAMBDA ',REG-ARGL
;	 (LIST* ',fun ,@(FOR-EVAL-THEN-QUOTE-ARGL ENV-ARGL) ',REG-ARGL)))

#+cl  ;;the lexical scoping  handles the environment in most cases
      ;;and it is messy to queue things 

 
#-cl
(DEFOPT M-TLAMBDA&ENV ( argl &REST BODY
		       &AUX (NAME (GEN-NAME))
		       (reg-argl (first argl))(env-argl (second argl)))
  (EMIT-DEFUN `(DEFUN ,NAME (,@ENV-ARGL ,@REG-ARGL)
		 ,@ (copy-rest-arg BODY)))
  `(MAKE-ALAMBDA ',REG-ARGL
	 (LIST* ',NAME ,@(FOR-EVAL-THEN-QUOTE-ARGL ENV-ARGL) ',REG-ARGL)))

#-cl
(DEFOPT M-TLAMBDA&ENV&  ( argl &REST BODY
		       &AUX (NAME (GEN-NAME))
		       (reg-argl (first argl))(env-argl (second argl)))
  (EMIT-DEFUN `(DEFUN ,NAME (,@ENV-ARGL ,@REG-ARGL) ,@ (copy-rest-arg BODY)))
  `(MAKE-ALAMBDA '*N*
	 (LIST* ',NAME ,@(FOR-EVAL-THEN-QUOTE-ARGL ENV-ARGL)
		',(DO ((N (LENGTH REG-ARGL))
		       (J 1 (f1+ J))
		       (L NIL))
		      ((= J N)
		       (PUSH `(CONS '(MLIST) (LISTIFY (f- ,(f1- N) *N*))) L)
		       (NREVERSE L))
		    (PUSH `(ARG ,J) L)))))

;;; this is the important case for numerical hackery.

(DEFUN DECLARE-SNARF (BODY)
  (COND ((AND (NOT (ATOM (CAR BODY)))
	      (EQ (CAAR BODY) 'DECLARE))
	 (LIST (CAR BODY)))
	(T NIL)))


;;; I will use the special variable given by the NAME as a pointer to
;;; an environment.

(DEFOPT M-TLAMBDA-I (MODE ENV ARGL &REST BODY
			    &AUX (NAME (GEN-NAME))
			    (DECLAREP (DECLARE-SNARF BODY)))
  (cond ((eq mode '$float)
	 (EMIT-DEFUN `(DECLARE (FLONUM (,NAME ,@(LISTN NIL (LENGTH ARGL))))))
	 (EMIT-DEFUN `(DEFPROP ,NAME T FLONUM-COMPILED))))
  (EMIT-DEFUN
   `(DEFUN ,NAME ,ARGL
      ,@DECLAREP
      (LET ((,ENV ,NAME))
	,@(COND (DECLAREP (CDR (copy-rest-arg BODY)))
		(T (copy-rest-arg  BODY))))))
  (EMIT-DEFUN #-NIL `(SETQ ,NAME ',(LISTN NIL (LENGTH ENV)))
	      #+NIL `(defparameter ,name (make-list ,(length env)))
	      )
  `(PROGN (SET-VALS-INTO-LIST ,ENV ,NAME)
	  (QUOTE ,NAME)))

;;; This is not optimal code.
;;; I.E. IT SUCKS ROCKS.

(DEFMACRO SET-VALS-INTO-LIST (ARGL VAR)
  (DO ((J 0 (f1+ J))
       (ARGL ARGL (CDR ARGL))
       (L NIL
	  `((SETF (NTH ,J ,VAR) ,(CAR ARGL)) ,@L)))
      ((NULL ARGL) `(PROGN ,@L))))








