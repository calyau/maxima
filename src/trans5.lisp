;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module trans5)


(transl-module trans5)

;;; these are TRANSLATE properies for the FSUBRS in JPG;COMM >

;;; LDISPLAY is one of the most beastly of all macsyma idiot
;;; constructs. First of all it makes a variable name and sets it,
;;; but it evaluates its argument such that
;;; x:10, LDISPLAY(F(X)) gives  (E1)   F(10)= ...
;;; LDISPLAY(X) gives X=10 of course. Sometimes it evaluates to get
;;; the left hand side, and sometimes it doesn't. It has its own
;;; private fucking version of the macsyma evaluator.
;;; To see multiple evaluation lossage in the interperter, try
;;; these: LDISPLAY(F(PRINT("FOOBAR")))$

;;;Totally and absolutely FUCKED
;;;(DEFUN $LDISPLAY FEXPR (LL) (DISP1 LL T T))
;;;
;;;(DEFUN $LDISP FEXPR (LL) (DISP1 LL T NIL))
;;;
;;;(DEFUN $DISPLAY FEXPR (LL) (DISP1 LL NIL T))
;;;
;;;(DEFUN $DISP FEXPR (LL) (DISP1 LL NIL NIL))
;;;
;;;(DEFUN DISP1 (LL LABLIST EQNSP)
;;; (COND (LABLIST (SETQ LABLIST (NCONS '(MLIST SIMP)))))
;;; (DO ((LL LL (CDR LL)) (L) (ANS) ($DISPFLAG T) (TIM 0))
;;;     ((NULL LL) (OR LABLIST '$DONE))
;;;     (SETQ L (CAR LL) ANS (MEVAL L))
;;;     (COND ((AND EQNSP (OR (ATOM ANS) (NOT (EQ (CAAR ANS) 'MEQUAL))))
;;;	    (SETQ ANS (LIST '(MEQUAL) (DISP2 L) ANS))))
;;;     (COND (LABLIST (COND ((NOT (CHECKLABEL $LINECHAR)) (SETQ $LINENUM (f1+ $LINENUM))))
;;;		    (MAKELABEL $LINECHAR) (NCONC LABLIST (NCONS LINELABLE))
;;;		    (COND ((NOT $NOLABELS) (SET LINELABLE ANS)))))
;;;     (SETQ TIM (RUNTIME))
;;;     (DISPLA (LIST '(MLABLE) (COND (LABLIST LINELABLE)) ANS))
;;;     (MTERPRI)
;;;     (TIMEORG TIM)))
;;;
;;;(DEFUN DISP2 (X)
;;; (COND ((ATOM X) X)
;;;       ((EQ (CAAR X) 'MQAPPLY)
;;;	(CONS '(MQAPPLY) (CONS (CONS (CAADR X) (MAPCAR 'MEVAL (CDADR X)))
;;;			       (MAPCAR 'MEVAL (CDDR X)))))
;;;       ((EQ (CAAR X) 'MSETQ) (DISP2 (CADR X)))
;;;       ((EQ (CAAR X) 'MSET) (DISP2 (MEVAL (CADR X))))
;;;       ((EQ (CAAR X) 'MLIST) (CONS (CAR X) (MAPCAR 'DISP2 (CDR X))))
;;;       ((GETL (CAAR X) '(FSUBR FEXPR)) X)
;;;       (T (CONS (CAR X) (MAPCAR 'MEVAL (CDR X))))))
;;;


(def%tr $disp (form) 
  `($any . (display-for-tr ,(eq (caar form) '$ldisp)
	    nil				; equationsp
	    ,@(tr-args (cdr form)))))
(def-same%tr $ldisp $disp)

(def%tr $display (form) 
  `($any . (display-for-tr ,(eq (caar form) '$ldisplay)
	    t
	    ,@(mapcar #'tr-exp-to-display (cdr form)))))

(def-same%tr $ldisplay $display)

;;; DISPLAY(F(X,Y,FOO()))
;;; (F X Y (FOO)) => (LET ((&G1 (FOO))) (list '(mequal) (LIST '(F) X Y &G1)
;;;   					   	              (F X Y &G1)))
;;; DISPLAY(X) => (LIST '(MEQUAL) '$X $X)
;;; DISPLAY(Q[I]) => (LIST '(MEQUAL) (LIST '(Q ARRAY) $I) ...)

;;; Ask me why I did this at lisp level, this should be able
;;; to be hacked as a macsyma macro. the brain damage I get
;;; into sometimes...

;;; This walks the translated code attempting to come up
;;; with a reasonable object for display, expressions which
;;; might have to get evaluated twice are pushed on the
;;; VALUE-ALIST (<expression> . <gensym>)
;;; This is incompatible with the interpreter which evaluates
;;; arguments to functions twice. Here I only evaluate non-atomic
;;; things once, and make sure that the order of evaluation is
;;; pretty much correct. I say "pretty much" because MAKE-VALUES
;;; does the optmization of not generating a temporary for a variable.
;;; DISPLAY(FOO(Z,Z:35)) will loose because the second argument will
;;; be evaluated first. I don't seriously expect anyone to find this
;;; bug.

(defvar value-alist nil)
(defun make-values (expr-args)
  (mapcar #'(lambda (arg)
	      (cond ((or (atom arg)
			 (memq (car arg) '(trd-msymeval quote)))
		     arg)
		    (t
		     (let ((sym (gensym)))
		       (push (cons arg sym) value-alist)
		       sym))))
	  expr-args))


(eval-when (compile eval #-pdp10 load)
  #-cl
  (defstruct (disp-hack-ob #+maclisp tree #-maclisp :tree)
    left-ob right-ob)
  #+cl
  (defstruct (disp-hack-ob (:conc-name nil)( :type list )) ;;it wanted tree but that's illegal
    left-ob right-ob)
  )

(defun object-for-display-hack (exp)
  (if (atom exp)
      (make-disp-hack-ob
       #+cl :left-ob #-cl left-ob `',exp
       #+cl :right-ob #-cl right-ob exp)
      (case (car exp)
	(simplify
	 (let ((v (object-for-display-hack (cadr exp))))
	   (make-disp-hack-ob
	    #+cl :left-ob #-cl left-ob (left-ob v)
	    #+cl :right-ob #-cl right-ob `(simplify ,(right-ob v)))))
	(marrayref
	 (let ((vals (make-values (cdr exp))))
	   (make-disp-hack-ob
	    #+cl :left-ob #-cl left-ob `(list (list* ,(car vals) '(array))
					 ,@(cdr vals))
	    #+cl :right-ob #-cl right-ob `(marrayref ,@vals))))
	(mfunction-call
					; assume evaluation of arguments.
	 (let ((vals (make-values (cddr exp))))
	   (make-disp-hack-ob
	    #+cl :left-ob #-cl left-ob `(list '(,(cadr exp)) ,@vals)
	    #+cl :right-ob #-cl right-ob `(mfunction-call ,(cadr exp) ,@vals))))
	(list
	 (let ((obs (mapcar #'object-for-display-hack (cdr exp))))
	   (make-disp-hack-ob
	    #+cl :left-ob #-cl left-ob `(list ,@(mapcar #'(lambda (u) (left-ob u))
							obs))
	    #+cl :right-ob #-cl right-ob `(list ,@(mapcar #'(lambda (u) (right-ob u))
							  obs)))))
	(quote (make-disp-hack-ob
		#+cl :left-ob #-cl left-ob exp
		#+cl :right-ob #-cl right-ob exp))
	(trd-msymeval
	 (make-disp-hack-ob
	  #+cl :left-ob #-cl left-ob `',(cadr exp)
	  #+cl :right-ob #-cl right-ob exp))
	(t
	 (cond ((or (not (atom (car exp)))
		    (getl (car exp) '(fsubr fexpr macro)))
		(make-disp-hack-ob
		 #+cl :left-ob #-cl left-ob `',exp
		 #+cl :right-ob #-cl right-ob exp))
	       (t
		(let ((vals (make-values (cdr exp))))
		  (make-disp-hack-ob
		   #+cl :left-ob #-cl left-ob `(list '(,(untrans-op (car exp)))
						,@vals)
		   #+cl :right-ob #-cl right-ob `(,(car exp) ,@vals)))))))))

(defun tr-exp-to-display (exp)
  (let* ((lisp-exp (dtranslate exp))
	 (value-alist nil)
	 (ob (object-for-display-hack lisp-exp))
	 (disp `(list '(mequal) ,(left-ob ob) ,(right-ob ob))))
    (setq value-alist (nreverse value-alist))
    (if value-alist
	`((lambda ,(mapcar #'cdr value-alist) ,disp)
	  ,@(mapcar #'car value-alist))
	disp)))

(defun untrans-op (op)
  (or (cdr (assq op '((add* . mplus)
		      (sub* . mminus)
		      (mul* . mtimes)
		      (div* . mquotient)
		      (power* . mexpt))))
      op))


;;; From RZ;COMBIN >
;;;
;;;#+MacLisp
;;;(defun $cf fexpr (a)
;;;       (fexprchk a 'cf)
;;;       (let ((divov (status divov))
;;;	     ($listarith nil))
;;;	    (prog2 (sstatus divov t)
;;;		   (cfeval (meval (car a)))
;;;		   (sstatus divov divov))))
;;;
;;;#+Lispm
;;;(defun $cf fexpr (a)
;;;       (fexprchk a 'cf)
;;;       (let (($listarith nil))
;;;	    (cfeval (meval (car a)))))

(def%tr $cf (form)
  (setq form (car (tr-args (cdr form))))
  (push-autoload-def '$cf '(cfeval))
  `($any . ((lambda (divov $listarith)
	      (sstatus divov t)
	      (unwind-protect (cfeval ,form)
		(sstatus divov divov)))
	    (status divov)
	    nil)))

;;; from RZ;TRGRED >
;;;
;;;(DEFUN $TRIGREDUCE FEXPR (L)
;;;    ((LAMBDA (*TRIGRED VAR *NOEXPAND $TRIGEXPAND $VERBOSE $RATPRINT)
;;;	(GCDRED (SP1 (MEVAL (CAR L)))))
;;;     T (COND ((CDR L) (MEVAL (CADR L)))
;;;	     ( '*NOVAR ))
;;;     T NIL NIL NIL))

;; JPG made this an LSUBR now! win win win good old Jeff.
;;(DEF%TR $TRIGREDUCE (FORM)
;;	(LET ((ARG1 (DTRANSLATE (CADR FORM)))
;;	      (ARG2 (COND ((CDDR FORM) (DTRANSLATE (CADDR FORM)))
;;			  (T ''*NOVAR))))
;;	     `($ANY . #%(LET ((*TRIGRED T)
;;			      (VAR ,ARG2)
;;			      (*NOEXPAND T)
;;			      ($TRIGEXPAND NIL)
;;			      ($VERBOSE NIL)
;;			      ($RATPRINT NIL))
;;			     ; gross hack, please fix me quick gjc!!!!
;;			     (OR (SYMBOL-PLIST 'GCDRED) (LOAD (GET '$TRIGREDUCE 'AUTOLOAD)))
;;			     (GCDRED (SP1 ,ARG1))))))

;;; From MATRUN
;;; (DEFUN $APPLY1 FEXPR (L)
;;;       (PROG (*EXPR)
;;;	     (SETQ *EXPR (MEVAL (CAR L)))
;;;	     (MAPC (FUNCTION (LAMBDA (Z)
;;;				     (SETQ *EXPR (APPLY1 *EXPR Z 0))))
;;;		   (CDR L))
;;;	     (RETURN *EXPR)))

(def%tr $apply1 (form &aux
		      (expr (tr-gensym))
		      (rules (tr-gensym)))
  (push-autoload-def '$apply1 '(apply1))
		      
  `($any  . (do ((,expr ,(dtranslate (cadr form))
			(apply1 ,expr (car ,rules) 0))
		 (,rules ',(cddr form) (cdr ,rules)))
		((null ,rules) ,expr))))

;;; This code was written before they had formatting of lisp code invented.
;;;(DEFUN $APPLY2 FEXPR (L)(PROG (*RULELIST)
;;;(SETQ *RULELIST (CDR L))
;;;(RETURN (APPLY2 (MEVAL (CAR L)) 0))))

(def%tr $apply2 (form)
  `($any . ((lambda (*rulelist)
	      (declare (special *rulelist))
	      (apply2 ,(dtranslate (cadr form)) 0))
	    ',(cddr form))))

;;;(DEFUN $APPLYB1 FEXPR (L) 
;;;	 (PROG (*EXPR) 
;;;	       (SETQ *EXPR (MEVAL (CAR L)))
;;;	       (MAPC (FUNCTION (LAMBDA (Z) 
;;;				       (SETQ *EXPR(CAR
;;;					     (APPLY1HACK *EXPR
;;;							 Z)))))
;;;		     (CDR L))
;;;	       (RETURN *EXPR )))

(def%tr $applyb1 (form &aux (expr (tr-gensym)) (rules (tr-gensym)))
  (push-autoload-def '$applyb1 '(apply1hack))
  `($any . (do ((,expr ,(dtranslate (cadr form))
		       (car (apply1hack ,expr (car ,rules))))
		(,rules ',(cddr form) (cdr ,rules)))
	       ((null ,rules) ,expr))))

;;;(DEFUN $APPLYB2 FEXPR (L)(PROG (*RULELIST)
;;;(SETQ *RULELIST (CDR L))
;;;(RETURN(CAR (APPLY2HACK (MEVAL (CAR L)))))))

(def%tr $applyb2 (form)
  (push-autoload-def '$applyb2 '(apply2hack))
  `($any . ((lambda (*rulelist)
	      (declare (special *rulelist))
	      (apply2hack ,(dtranslate (cadr form))))
	    ',(cddr form))))



;;; this nice translation property written by REH.
;;; He is the first macsyma system program to ever
;;; write the translation property for his own special form!


(def%tr $buildq (form)

  (let ((alist				;would be nice to output
	 (mapcar		       ;backquote instead of list/cons
	  #'(lambda (var)	       ;but I'm not sure if things get
	      (cond ((atom var)		;macroexpanded.  -REH
					; Well, any macros are o.k. They
					; get expanded "at the right time". -gjc
		    
		     `(cons ',var ,(dtranslate var)))
		    ((eq (caar var) 'msetq)
		     `(cons ',(cadr var) ,(dtranslate (caddr var))))
		    (t (setq tr-abort t)
		       (tr-tell var
				"Illegal BUILDQ form encountered during translation"))))
					;right thing to do here??
					;how much error checking does transl do now?
					; Yes. Not as much as it should! -GJC
	  
	  (cdr (cadr form)))))
    (cond ((null alist) 
	   `($any quote ,(caddr form)))
	  (t `($any mbuildq-subst (list ,@alist) ',(caddr form))))))


;;; Presently STATUS and SSTATUS are FEXPR which don't evaluate 
;;; their arguments. 

#-cl
(def%tr $sstatus (form)
  `($any . ($sstatus . ,(cdr form))))

#-cl
(def%tr $status (form)
  (setq form (cdr form))
  (cond ((null form)	; %%%PLEASE FIX ME%%% with WNA-CHECKING %%%%%%
	 nil)
	(t
	 (case (car form)
	   ($feature
	    (cond ((null (cdr form))
		   `($any . ($status $feature)))
					; this BOOLEAN check is important, since
					; STATUS(FEATURE,FOO) will always be used in a
					; BOOLEAN context.
		  (t `($boolean . ($status $feature ,(cadr form))))))
	   (t `($any . ($status . ,form)))))))

