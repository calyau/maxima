;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;	** (c) Copyright 1982 Massachusetts Institute of Technology **

(macsyma-module mlisp)

#-(or cl NIL)
(EVAL-WHEN (EVAL COMPILE) (SETQ OLD-IBASE *read-base* *read-base* 10.))
#+cl
(EVAL-WHEN (EVAL COMPILE) (SETQ OLD-read-base *read-BASE* *read-base* 10.))

(declare-top (SPECIAL MSPECLIST MPROPLIST BINDLIST LOCLIST BVARS NOUNSFLAG putl
		  NOITEMS DERIVFLAG DERIVLIST MPROGP MDOP EVP AEXPRP MLOCP $LABELS
		  $VALUES $FUNCTIONS $ARRAYS $RULES $GRADEFS $DEPENDENCIES $ALIASES
		  $MYOPTIONS $PROPS GENVAR $MAXPOSEX $MAXNEGEX $EXPOP $EXPON
		  $FLOAT $NUMER ARYP MSUMP STATE-PDL EVARRP $SETVAL NOUNL
		  $SETCHECKBREAK $REFCHECK DEBUG REFCHKL BAKTRCL MAPLP
		  $NOREPEAT $DETOUT $DOALLMXOPS $DOSCMXOPS OPERS FACTLIST OPEXPRP
		  $TRANSLATE $TRANSRUN $MAPERROR OUTARGS1 OUTARGS2 FMAPLVL MOPL
		  $POWERDISP $SUBSCRMAP $DISPFLAG $OPTIONSET DSKSETP FEXPRERRP
		  $FEATURES ALPHABET $%ENUMER $INFEVAL $SAVEDEF $%% %E-VAL
		  $MAPPRINT FEATUREL OUTFILES FUNDEFSIMP MFEXPRP TRANSP
		  SFINDEX MSPECLIST2 ENVLIST $MACROS LINEL $RATFAC $RATWTLVL
		  $OPERATORS NOEVALARGS $PIECE $PARTSWITCH *GCDL*
		  SCANMAPP))
(declare-top (unspecial args))
#-cl (proclaim ' (GENPREFIX %LS))
#-cl(proclaim '	 (*EXPR RATF $FLOAT))
#-cl(proclaim '	 (*LEXPR MAP1 MMAPCAR FMAPL1 OUTERMAP1 $INPART LINEL $DIFF $INTEGRATE
		 $LDISP $RATVARS $RATWEIGHT))
(declare-top	 (FIXNUM N I J NNEED NGIVEN NCELLS NITEMS LISPSUB INDX FMAPLVL EVFLG 
			 LINEL SFINDEX #-cl (HASHER)))
;  NNEED to be flushed

(SETQ MSPECLIST NIL BINDLIST NIL LOCLIST NIL MPROPLIST NIL $%ENUMER NIL
      $FLOAT NIL NOUNL NIL $REFCHECK NIL SCANMAPP NIL MAPLP NIL
      MPROGP NIL EVP NIL MDOP NIL MLOCP NIL PUTL NIL
      $SUBSCRMAP NIL $TRANSLATE NIL $TRANSRUN T $SAVEDEF T AEXPRP NIL
      $MAPERROR T FMAPLVL 0 $OPTIONSET NIL 
      $SETCHECKBREAK NIL DSKSETP NIL ARYP NIL MSUMP NIL EVARRP NIL
      $INFEVAL NIL FACTLIST NIL $MAPPRINT T FUNDEFSIMP NIL
      MFEXPRP T NOUNSFLAG NIL OPEXPRP NIL ;$OPERATORS NIL
      SFINDEX 1 MSPECLIST2 NIL ENVLIST NIL TRANSP NIL NOEVALARGS NIL
      $PIECE '$PIECE $SETVAL '$SETVAL FEXPRERRP NIL RULEFCNL NIL
      FEATUREL (PURCOPY '($INTEGER $NONINTEGER $EVEN $ODD
			  $RATIONAL $IRRATIONAL $REAL $IMAGINARY
			  $COMPLEX $ANALYTIC $INCREASING $DECREASING
			  $ODDFUN $EVENFUN $POSFUN $COMMUTATIVE $LASSOCIATIVE
			  $RASSOCIATIVE $SYMMETRIC $ANTISYMMETRIC))
      $FEATURES (CONS '(MLIST SIMP) (APPEND FEATUREL NIL)))

;; These three variables are what get stuck in array slots as magic
;; unbound objects.  They are for T, FIXNUM, and FLONUM type arrays
;; respectively.

(DEFVAR MUNBOUND '|#####|)

;; The most negative fixnum.  Sign bit is on and all other bits are zero.
;; Assumes two's complement arithmetic.
(DEFVAR FIXUNBOUND
  #+(or cl NIL) MOST-NEGATIVE-FIXNUM
  #-(or NIL cl) (ROT 1 -1))

;; The PDP10 floating point representation is:
;; 1 bit sign, 8 bit exponent, 27 bit mantissa
;; If positive, exponent is excess 128.  If negative, exponent is one's
;; complement of excess 128.
;; If positive normalized, mantissa is between 2^26 and 2^27-1.  If negative,
;; two's complement.  See RAT;FLOAT for more details.

;; I think this is supposed to be the most negative flonum.  It's close,
;; but not quite.  The smallest is (FSC (ROT 3 -1) 0).

#+PDP10
(DEFVAR FLOUNBOUND (FSC (f- 2 (LSH -1 -1)) 0))

;; H6180 floating point representation is:
;; 8 bit exponent, 1 bit sign, 27 bit mantissa
;; The 8 bit exponent is viewed as two's complement, between 2^7-1 and -2^7.
;; The 28 bit mantissa is viewed as two's complement, between -1 and 1-2^-27.
;; The most negative flonum is given below.  The most positive flonum
;; is its logical complement.

#+H6180
(DEFVAR FLOUNBOUND (FSC (LOGIOR (LSH 1 35.) (LSH 1 27.)) 0))

;; Too bad there's no general way of getting the most negative flonum in
;; a relatively machine-independent manner.

#+LISPM
(DEFVAR FLOUNBOUND '*FLOUNBOUND-DOESNT-MATTER-ANYWAY*)

#+(or cl NIL)
(DEFVAR FLOUNBOUND MOST-NEGATIVE-DOUBLE-FLOAT)

(DEFMVAR MUNBINDP NIL
  "Used for safely MUNBINDing incorrectly-bound variables."
  NO-RESET)
(DEFMVAR $SETCHECK NIL)

(MAPC #'(LAMBDA (X) (SET X (NCONS '(MLIST SIMP))))
      '($VALUES $FUNCTIONS $MACROS $ARRAYS $MYOPTIONS $RULES $PROPS))
 

(DEFMFUN MAPPLY1 (FN ARGS FNNAME form)
  (declare( special aryp) (object fn))
 (COND ;((AND $OPERATORS (MNUMP FN)) (MUL2 FN (CAR ARGS)))
       ((ATOM FN) 
	(cond
	 #-cl				; #+(or cl nil)
	 ((and (symbolp fn) (fboundp fn)
	       (not (consp symbol-function fn)))
	  (apply  fn args))
	 #+(or cl nil)
	 ((ATOM FN) 
	  (cond
	   #+(or cl nil)
	   ((functionp fn)
	    (APPLY FN ARGS))
	  
	   #+cl;;better be a macro or an array.
	   ((fboundp fn)
	    (if (macro-function fn)
	        (progn (merror "~M is a lisp level macro and cannot be applied at maxima level" fn) (eval (cons fn  args)))
	      (mapply1 (symbol-function fn) args fn form)))
	   
	  ((symbol-array fn)
	   (mapply1 (symbol-array fn) args fn form))
	  (t
	   (SETQ FN (GETOPR FN)) (BADFUNCHK FNNAME FN NIL)
	   (LET ((NOEVALARGS T)) (MEVAL (CONS (NCONS FN) ARGS)))))
	 )))
       #+cl
       ((functionp fn)
	(apply fn args))
       #-cl
       ((EQ (CAR FN) 'LAMBDA) (APPLY FN ARGS))
       #+(and Lispm (not cl))
       ((memq (CAR FN)
	      '(NAMED-LAMBDA si:digested-lambda)) (APPLY FN ARGS))
       #-cl
       ((AND (EQ (CAAR FN) 'MFILE)
	     (SETQ FN (EVAL (DSKGET (CADR FN) (CADDR FN) 'VALUE NIL)))
	     NIL))
       ((EQ (CAAR FN) 'LAMBDA) (MLAMBDA FN ARGS FNNAME T form))
       ((EQ (CAAR FN) 'MQUOTE) (CONS (CDR FN) ARGS))
       ((AND ARYP (MEMQ (CAAR FN) '(MLIST $MATRIX)))
	(IF (NOT (OR (= (LENGTH ARGS) 1)
		     (AND (EQ (CAAR FN) '$MATRIX) (= (LENGTH ARGS) 2))))
	    (MERROR "Wrong number of indices:~%~M" (CONS '(MLIST) ARGS)))
	(DO ((ARGS1 ARGS (CDR ARGS1)))
	    ((NULL ARGS1) (LET (($PIECE $PIECE) ($PARTSWITCH 'MAPPLY))
			       (APPLY #'$INPART (CONS FN ARGS))))
	    (UNLESS (fixnump (car args1))
		    (IF EVARRP (THROW 'EVARRP 'NOTEXIST))
		    (MERROR "Subscript must be an integer:~%~M" (CAR ARGS1)))))
       (ARYP (CONS '(MQAPPLY ARRAY) (CONS FN ARGS)))
       ((MEMQ 'array (CDAR FN)) (CONS '(MQAPPLY) (CONS FN ARGS)))
       (T (BADFUNCHK FNNAME FN T))))

#-NIL
;; the last argument to mapply1 for the lineinfo is not correct here..
(DEFMFUN MCALL N (MAPPLY1 (ARG 1) (LISTIFY (f- 1 N)) (ARG 1) nil))

#+NIL
(DEFMFUN MCALL (FN &REST ARGS)
  (MAPPLY1 FN ARGS FN nil))

#-NIL
(declare-top (MAPEX T))  ; To avoid the overuse of pdls in this highly recursive 
		     ; part of the evaluator.

(DEFUN MEVALARGS (ARGS)
 (COND (NOEVALARGS (SETQ NOEVALARGS NIL) ARGS) (T (MAPCAR #'MEVAL ARGS))))

;Function Call stack each element is
; (fname . bindlist) where bindlist was the value at time of entry.
; So you can use this to compute what the bindings were at any
; function call.
(defvar *mlambda-call-stack* (make-array 30 :fill-pointer 0 :adjustable t ))

#-NIL 
(declare-top (MAPEX NIL))

(DEFUN MLAMBDA (FN ARGS FNNAME NOEVAL form)
  (COND ((NOT ($LISTP (CADR FN)))
	 (MERROR "First argument to LAMBDA must be a list:~%~M" (CADR FN))))
  (SETQ NOEVALARGS NIL)
  (let ((PARAMS  (CDADR FN))( MLOCP  T))
    (SETQ LOCLIST (CONS NIL LOCLIST))
    (DO ((A) (P))
	((OR (NULL PARAMS) (AND (NULL ARGS) (NOT (MDEFLISTP PARAMS))))
	 (SETQ ARGS (NRECONC A ARGS) PARAMS (NRECONC P PARAMS)))
      (COND ((MDEFLISTP PARAMS)
	     (SETQ PARAMS (CDAR PARAMS) ARGS (NCONS (CONS '(MLIST) ARGS)))))
      (COND ((AND MFEXPRP (MQUOTEP (CAR PARAMS)))
	     (SETQ A (CONS (CAR ARGS) A) P (CONS (CADAR PARAMS) P)))
	    ((ATOM (CAR PARAMS))
	     (SETQ P (CONS (CAR PARAMS) P)
		   A (CONS (COND (NOEVAL (CAR ARGS))
				 (T (MEVAL (CAR ARGS)))) A)))
	    (T (MERROR "Illegal LAMBDA parameter:~%~M" (CAR PARAMS))))
      (SETQ ARGS (CDR ARGS) PARAMS (CDR PARAMS)))
;    (MBINDING (PARAMS ARGS FNNAME)
;	      (PROG1 (LET ((AEXPRP (AND AEXPRP (NOT (ATOM (CADDR FN)))
;					(EQ (CAAR (CADDR FN)) 'LAMBDA))))
;		       (COND ((NULL (CDDR FN))
;			      (MERROR "No LAMBDA body present"))
;			     ((CDDDR FN) (MEVALN (CDDR FN)))
;			     (T (MEVAL (CADDR FN)))))
;		     ;; the MUNLOCAL should be unwind-protected,  I can't
;		     ;; see how I can work it into the MBINDING macro
;		     ;; at this time. Too bad for the losers who use it.
;		     (MUNLOCAL)))
    ;; we expand the above, and also add stuff for the call stack.
    ;; we also move munlocal into the unwind protect.
    (let (FINISH2033 (FINISH2032 params) (ar *mlambda-call-stack*))
      (declare (type (vector t) ar))
      (UNWIND-PROTECT
       (PROGN
	(or (f> (array-total-size ar) (f+ (fill-pointer ar) 10))
	    (adjust-array ar (f+ (array-total-size ar) 50)
			  :fill-pointer (fill-pointer ar)))
	(vector-push bindlist ar)
	;; rather than pushing all on baktrcl it might be good
	;; to make a *last-form* global that is set in meval1
	;; and is pushed here.  
	;(vector-push baktrcl ar)
	(vector-push form ar)
	(vector-push params ar)
	(vector-push args ar)
	(vector-push fnname ar)
	(MBIND FINISH2032 ARGS FNNAME)
	(SETQ FINISH2033 T)
	(PROG1 (LET ((AEXPRP (AND AEXPRP (NOT (ATOM (CADDR FN)))
				  (EQ (CAAR (CADDR FN)) 'LAMBDA))))
		    (COND
		     ((NULL (CDDR FN)) (MERROR "No LAMBDA body present"))
		     ((CDDDR FN) (MEVALN (CDDR FN)))
		     (T (MEVAL (CADDR FN)))))
	        nil ))
       (IF FINISH2033 (progn (incf (fill-pointer *mlambda-call-stack*) -5)
			     (MUNLOCAL)
			     (MUNBIND FINISH2032)
			     ))))

    ))


(Defmspec MPROGN (FORM) (MEVALN (CDR FORM)))

(DEFMFUN MEVALN (L) ;; called in a few places externally.
 (DO ((BODY L (CDR BODY)) ($%% '$%%)) ((NULL (CDR BODY)) (MEVAL (CAR BODY)))
     (SETQ $%% (MEVAL (CAR BODY)))))

;(DEFMSPEC DOLIST (FORM)  ; temporary
; (SETF (CAR FORM) '(MPROGN)) (MEVAL FORM))

(DEFUN MQAPPLY1 (FORM)
    (declare (special aryp))
 (LET (((FN . ARGL) (CDR FORM)) (AEXPRP))
      (COND ((NOT (MQUOTEP FN)) (SETQ FN (MEVAL FN))))
      (COND ((ATOM FN) (MEVAL (CONS (CONS FN ARYP) ARGL)))
	    ((EQ (CAAR FN) 'LAMBDA)
	     (COND (ARYP (MERROR "Improper array call"))
		   (T (MLAMBDA FN ARGL (CADR FORM) NOEVALARGS form))))
	    (T (MAPPLY1 FN (MEVALARGS ARGL) (CADR FORM) form)))))

(DEFMFUN MEVAL (FORM) (SIMPLIFYA (MEVAL1 FORM) NIL))
;;temporary hack to see what's going on:
(DEFMFUN safe-MGETL (ATOM INDS) (and (symbolp atom)
  (LET ((PROPS (GET ATOM 'MPROPS))) (AND PROPS (GETL PROPS INDS)))))
(DEFMFUN safe-MGET (ATOM INDS) (and (symbolp atom)
  (LET ((PROPS (GET ATOM 'MPROPS))) (AND PROPS (GETf (cdr PROPS) INDS)))))

(defvar *last-meval1-form* nil)

(DEFMFUN MEVAL1 (FORM)
  (declare (special  nounl *break-points* *break-step*))
  (COND ((ATOM FORM)
	 (PROG (VAL)
	   (COND ((NOT (SYMBOLP FORM)) (RETURN FORM))
		 ((AND $NUMER (SETQ VAL (SAFE-MGET FORM '$NUMER))
		       (OR (NOT (EQ FORM '$%E)) $%ENUMER))
		  (RETURN (MEVAL1 VAL)))
		 ((NOT (BOUNDP FORM))
		  (IF (SAFE-GET FORM 'BINDTEST)
		      (MERROR "~:M unbound variable" FORM)
		      (RETURN FORM)))
		 ((MFILEP (SETQ VAL (SYMBOL-VALUE FORM)))
		  (SETQ VAL
			(EVAL (DSKGET (CADR VAL) (CADDR VAL) 'VALUE NIL)))))
	   (WHEN (AND $REFCHECK (MEMQ FORM (CDR $VALUES))
		      (NOT (MEMQ FORM REFCHKL)))
		 (SETQ REFCHKL (CONS FORM REFCHKL))
		 (MTELL "~:M has value.~%" FORM))
	   (RETURN VAL)))
	((OR (AND (ATOM (CAR FORM))
		  (SETQ FORM (CONS (NCONS (CAR FORM)) (CDR FORM))))
	     (ATOM (CAAR FORM)))
	 (LET ((BAKTRCL BAKTRCL) TRANSP) 
	   (PROG (U ARYP)
		 (declare (special aryp))
	     ;;(COND ((EQ DEBUG '$ALL) (SETQ BAKTRCL (CONS FORM BAKTRCL))))
            	 (setq *last-meval1-form* form)
	     (SETQ ARYP (MEMQ 'array (CDAR FORM))) 
	     (COND ((AND (NOT OPEXPRP) (NOT ARYP) 
			 (MEMQ (CAAR FORM) '(MPLUS MTIMES MEXPT MNCTIMES)))
		    (GO C))
		   ;; dont bother pushing mplus and friends on baktrcl
		   ;; should maybe even go below aryp.
		   ((AND debug
			 (PROGN
			  ;(SETQ BAKTRCL (CONS FORM BAKTRCL))
			  ;; if wanting to step, the *break-points*
			  ;; variable will be set to a vector (possibly empty).
			  (when (and *break-points*
				     (or (null  *break-step*)
					 (null (funcall *break-step* form))))
				(let ((ar *break-points*))
				  (declare (type (vector t) ar))
				(sloop for i below (fill-pointer ar)
				       when (eq (car (aref ar i)) form)
				       do (*break-points* form)
				       (loop-finish))))
				NIL)))
		   ((AND $SUBSCRMAP ARYP
			 (DO ((X (MARGS FORM) (CDR X)))
			     ((OR (NULL X) (MXORLISTP (CAR X))) X)))
		    (SETQ NOEVALARGS NIL) (RETURN (SUBGEN FORM)))
		   ((EQ (CAAR FORM) 'MQAPPLY) (RETURN (MQAPPLY1 FORM))))
	     (BADFUNCHK (CAAR FORM) (CAAR FORM) NIL)
	    A    (SETQ U (OR (SAFE-GETL (CAAR FORM) '(NOUN))
			     (AND NOUNSFLAG (EQ (GETCHAR (CAAR FORM) 1) '%)
				  (NOT (OR (GETL-FUN (CAAR FORM)
						     '(SUBR FSUBR LSUBR))
					   (SAFE-GETL (CAAR FORM)
						 '(MFEXPR* MFEXPR*S))))
				  (PROG2 ($VERBIFY (CAAR FORM))
					 (SAFE-GETL (CAAR FORM) '(NOUN))))
			     (AND (NOT ARYP) $TRANSRUN
				  (SETQ TRANSP
					(OR (SAFE-MGETL (CAAR FORM) '(T-MFEXPR))
					    (SAFE-GETL (CAAR FORM)
						  '(TRANSLATED-MMACRO)))))
			     (AND (NOT ARYP)
				  (SETQ U
					(OR (SAFE-MGET (CAAR FORM) 'TRACE)
					    (AND $TRANSRUN
						 (SAFE-GET (CAAR FORM) 'TRANSLATED)
						 (NOT (SAFE-MGET (CAAR FORM)
							    'LOCAL-FUN))
						 (SETQ TRANSP T) (CAAR FORM))))
				  (GETL-FUN U '(EXPR SUBR LSUBR)))
			     (COND (ARYP (SAFE-MGETL (CAAR FORM) '(HASHAR ARRAY)))
				   ((SAFE-MGETL (CAAR FORM) '(MEXPR MMACRO)))
				   ((SAFE-MGETL (CAAR FORM) '(T-MFEXPR)))
				   (T (OR (SAFE-GETL (CAAR FORM)
						'(MFEXPR* MFEXPR*S))
					  (GETL-FUN (CAAR FORM)
						    '(SUBR FSUBR EXPR FEXPR macro
							   LSUBR)))))))
	     (COND ((NULL U) (GO B))
		   ((AND (MEMQ (CAR U) '(MEXPR MMACRO)) (MFILEP (CADR U)))
		    (SETQ U (LIST (CAR U)
				  (DSKGET (CADADR U) (CAR (CDDADR U))
					  (CAR U) NIL))))
		   ((AND (MEMQ (CAR U) '(ARRAY HASHAR)) (MFILEP (CADR U)))
		    (I-$UNSTORE (NCONS (CAAR FORM)))
		    (RETURN (MEVAL1 FORM))))
	     (RETURN 
	      (COND ((EQ (CAR U) 'HASHAR) 
		     (HARRFIND (CONS (CAR FORM) (MEVALARGS (CDR FORM)))))
		    ((MEMQ (CAR U) '(FEXPR FSUBR))
		     (IF FEXPRERRP
			 (MERROR "Attempt to call ~A ~A from MACSYMA level.~
				 ~%Send a bug note."
				 (CAR U) (CAAR FORM)))
		     (SETQ NOEVALARGS NIL) (APPLY (CAAR FORM) (CDR FORM)))
		    ((OR (AND (EQ (CAR U) 'SUBR)
			      (PROG2 (MARGCHK (CAAR FORM) (CDR FORM)) T))
			 (EQ (CAR U) 'LSUBR))
;		       ((MEMQ (CAR U) '(SUBR LSUBR))
;			(MARGCHK (CAAR FORM) (CDR FORM)))
		     (APPLY (CAAR FORM) (MEVALARGS (CDR FORM))))

		    ((EQ (CAR U) 'NOUN)
;			(MARGCHK (CAAR FORM) (CDR FORM))
		     (COND ((OR (MEMQ (CAAR FORM) NOUNL) NOUNSFLAG)
			    (SETQ FORM (CONS (CONS (CADR U) (CDAR FORM))
					     (CDR FORM)))
			    (GO A))
			   (ARYP (GO B))
			   ((MEMQ (CAAR FORM) '(%SUM %PRODUCT))
			    (SETQ U (DO%SUM (CDR FORM) (CAAR FORM))
				  NOEVALARGS NIL)
			    (CONS (NCONS (CAAR FORM)) U))
			   (T (MEVAL2 (MEVALARGS (CDR FORM)) FORM))))
		    ((EQ (CAR U) 'array)
		     (ARRFIND (CONS (CAR FORM) (MEVALARGS (CDR FORM)))))
		    ((EQ (CAR U) 'MEXPR)
		     (MLAMBDA (CADR U) (CDR FORM) (CAAR FORM) NOEVALARGS form))
		    ((MEMQ (CAR U) '(MMACRO TRANSLATED-MMACRO))
		     (SETQ NOEVALARGS NIL)
		     (MEVAL (MMACRO-APPLY (CADR U) FORM)))
		    ((EQ (CAR U) 'MFEXPR*)
		     (SETQ NOEVALARGS NIL)  (APPLY (CADR U) (NCONS FORM)))
		    #+cl
		    ((eq (car u) 'macro)
		     (setq noevalargs nil)
		     (setq form (cons(caar form) (cdr form)))
;		     (setf (car form) (caar form) )
		      (eval form)
		     )
		    #+Maclisp
		    ((EQ (CAR U) 'MFEXPR*S)
		     (SETQ NOEVALARGS NIL)
		     ;; use macsyma Trace if you want to trace this call.
		     (SUBRCALL T (CADR U) FORM))
		    ((EQ (CAR U) 'T-MFEXPR) (APPLY (CADR U) (CDR FORM)))
		    (T (MARGCHK (CAAR FORM) (CDR FORM))
		       (APPLY (CADR U) (MEVALARGS (CDR FORM))))))
	    B   #+(OR PDP10 Multics Franz NIL cl)
	     (IF (AND (NOT ARYP) (LOAD-FUNCTION (CAAR FORM) T)) (GO A))
	     (BADFUNCHK (CAAR FORM) (CAAR FORM) NIL)
	     (IF (SYMBOLP (CAAR FORM))
		 (SETQ U (BOUNDP (CAAR FORM)))
		 (RETURN (MEVAL1-EXTEND FORM)))
	    C   (COND ((OR (NULL U)
			   (AND (SAFE-GET (CAAR FORM) 'OPERATORS) (NOT ARYP))
			   (EQ (CAAR FORM) (SETQ U (SYMBOL-VALUE (CAAR FORM)))))
		       (SETQ FORM (MEVAL2 (MEVALARGS (CDR FORM)) FORM))
		       (RETURN (OR (AND (SAFE-MGET (CAAR FORM) 'ATVALUES)
					(AT1 FORM)) FORM)))
		      ((AND ARYP (SAFE-GET (CAAR FORM) 'NONARRAY))
		       (RETURN (CONS (CONS (CAAR FORM) ARYP)
				     (MEVALARGS (CDR FORM)))))
		      ((ATOM U)
		       (BADFUNCHK (CAAR FORM) U NIL)
		       (SETQ FORM (CONS (CONS (GETOPR U) ARYP) (CDR FORM)))
		       (GO A))
		      ((EQ (CAAR U) 'LAMBDA)
		       (IF ARYP
			   (MERROR "Improper array call")
			   (RETURN (MLAMBDA U (CDR FORM)
					    (CAAR FORM) NOEVALARGS form))))
		      (T (RETURN (MAPPLY1 U (MEVALARGS (CDR FORM))
					 (CAAR FORM) form)))))))
	(T (MAPPLY1 (CAAR FORM) (MEVALARGS (CDR FORM)) (CAAR FORM) form))))

;;old def. had some unsafe plist accesses.
;(DEFMFUN MEVAL1 (FORM)
;  (declare (special  nounl))
;  (COND ((ATOM FORM)
;	 (PROG (VAL)
;	   (COND ((NOT (SYMBOLP FORM)) (RETURN FORM))
;		 ((AND $NUMER (SETQ VAL (MGET FORM '$NUMER))
;		       (OR (NOT (EQ FORM '$%E)) $%ENUMER))
;		  (RETURN (MEVAL1 VAL)))
;		 ((NOT (BOUNDP FORM))
;		  (IF (GET FORM 'BINDTEST)
;		      (MERROR "~:M unbound variable" FORM)
;		      (RETURN FORM)))
;		 ((MFILEP (SETQ VAL (SYMBOL-VALUE FORM)))
;		  (SETQ VAL
;			(EVAL (DSKGET (CADR VAL) (CADDR VAL) 'VALUE NIL)))))
;	   (WHEN (AND $REFCHECK (MEMQ FORM (CDR $VALUES))
;		      (NOT (MEMQ FORM REFCHKL)))
;		 (SETQ REFCHKL (CONS FORM REFCHKL))
;		 (MTELL "~:M has value.~%" FORM))
;	   (RETURN VAL)))
;	((OR (AND (ATOM (CAR FORM))
;		  (SETQ FORM (CONS (NCONS (CAR FORM)) (CDR FORM))))
;	     (ATOM (CAAR FORM)))
;	 (LET ((BAKTRCL BAKTRCL) TRANSP) 
;	   (PROG (U ARYP)
;		 (declare (special aryp))
;	     (COND ((EQ DEBUG '$ALL) (SETQ BAKTRCL (CONS FORM BAKTRCL))))
;	     (SETQ ARYP (MEMQ 'array (CDAR FORM))) 
;	     (COND ((AND (NOT OPEXPRP) (NOT ARYP) 
;			 (MEMQ (CAAR FORM) '(MPLUS MTIMES MEXPT MNCTIMES)))
;		    (GO C))
;		   ((AND $SUBSCRMAP ARYP
;			 (DO ((X (MARGS FORM) (CDR X)))
;			     ((OR (NULL X) (MXORLISTP (CAR X))) X)))
;		    (SETQ NOEVALARGS NIL) (RETURN (SUBGEN FORM)))
;		   ((EQ (CAAR FORM) 'MQAPPLY) (RETURN (MQAPPLY1 FORM))))
;	     (BADFUNCHK (CAAR FORM) (CAAR FORM) NIL)
;	    A    (SETQ U (OR (GETL (CAAR FORM) '(NOUN))
;			     (AND NOUNSFLAG (EQ (GETCHAR (CAAR FORM) 1) '%)
;				  (NOT (OR (GETL-FUN (CAAR FORM)
;						     '(SUBR FSUBR LSUBR))
;					   (GETL (CAAR FORM)
;						 '(MFEXPR* MFEXPR*S))))
;				  (PROG2 ($VERBIFY (CAAR FORM))
;					 (GETL (CAAR FORM) '(NOUN))))
;			     (AND (NOT ARYP) $TRANSRUN
;				  (SETQ TRANSP
;					(OR (MGETL (CAAR FORM) '(T-MFEXPR))
;					    (GETL (CAAR FORM)
;						  '(TRANSLATED-MMACRO)))))
;			     (AND (NOT ARYP)
;				  (SETQ U
;					(OR (MGET (CAAR FORM) 'TRACE)
;					    (AND $TRANSRUN
;						 (GET (CAAR FORM) 'TRANSLATED)
;						 (NOT (MGET (CAAR FORM)
;							    'LOCAL-FUN))
;						 (SETQ TRANSP T) (CAAR FORM))))
;				  (GETL-FUN U '(EXPR SUBR LSUBR)))
;			     (COND (ARYP (MGETL (CAAR FORM) '(HASHAR ARRAY)))
;				   ((MGETL (CAAR FORM) '(MEXPR MMACRO)))
;				   ((MGETL (CAAR FORM) '(T-MFEXPR)))
;				   (T (OR (GETL (CAAR FORM)
;						'(MFEXPR* MFEXPR*S))
;					  (GETL-FUN (CAAR FORM)
;						    '(SUBR FSUBR EXPR FEXPR macro
;							   LSUBR)))))))
;;#+cl     (cond ((eq (car u) 'macro) (show u) (setf (cadr u) (cdadr u))))
;	     (COND ((NULL U) (GO B))
;		   ((AND (MEMQ (CAR U) '(MEXPR MMACRO)) (MFILEP (CADR U)))
;		    (SETQ U (LIST (CAR U)
;				  (DSKGET (CADADR U) (CAR (CDDADR U))
;					  (CAR U) NIL))))
;		   ((AND (MEMQ (CAR U) '(ARRAY HASHAR)) (MFILEP (CADR U)))
;		    (I-$UNSTORE (NCONS (CAAR FORM)))
;		    (RETURN (MEVAL1 FORM))))
;	     (RETURN 
;	      (COND ((EQ (CAR U) 'HASHAR) 
;		     (HARRFIND (CONS (CAR FORM) (MEVALARGS (CDR FORM)))))
;		    ((MEMQ (CAR U) '(FEXPR FSUBR))
;		     (IF FEXPRERRP
;			 (MERROR "Attempt to call ~A ~A from MACSYMA level.~
;				 ~%Send a bug note."
;				 (CAR U) (CAAR FORM)))
;		     (SETQ NOEVALARGS NIL) (APPLY (CAAR FORM) (CDR FORM)))
;		    ((OR (AND (EQ (CAR U) 'SUBR)
;			      (PROG2 (MARGCHK (CAAR FORM) (CDR FORM)) T))
;			 (EQ (CAR U) 'LSUBR))
;;		       ((MEMQ (CAR U) '(SUBR LSUBR))
;;			(MARGCHK (CAAR FORM) (CDR FORM)))
;		     (APPLY (CAAR FORM) (MEVALARGS (CDR FORM))))
;
;		    ((EQ (CAR U) 'NOUN)
;;			(MARGCHK (CAAR FORM) (CDR FORM))
;		     (COND ((OR (MEMQ (CAAR FORM) NOUNL) NOUNSFLAG)
;			    (SETQ FORM (CONS (CONS (CADR U) (CDAR FORM))
;					     (CDR FORM)))
;			    (GO A))
;			   (ARYP (GO B))
;			   ((MEMQ (CAAR FORM) '(%SUM %PRODUCT))
;			    (SETQ U (DO%SUM (CDR FORM) (CAAR FORM))
;				  NOEVALARGS NIL)
;			    (CONS (NCONS (CAAR FORM)) U))
;			   (T (MEVAL2 (MEVALARGS (CDR FORM)) FORM))))
;		    ((EQ (CAR U) 'array)
;		     (ARRFIND (CONS (CAR FORM) (MEVALARGS (CDR FORM)))))
;		    ((EQ (CAR U) 'MEXPR)
;		     (MLAMBDA (CADR U) (CDR FORM) (CAAR FORM) NOEVALARGS form))
;		    ((MEMQ (CAR U) '(MMACRO TRANSLATED-MMACRO))
;		     (SETQ NOEVALARGS NIL)
;		     (MEVAL (MMACRO-APPLY (CADR U) FORM)))
;		    ((EQ (CAR U) 'MFEXPR*)
;		     (SETQ NOEVALARGS NIL)  (APPLY (CADR U) (NCONS FORM)))
;		    #+cl
;		    ((eq (car u) 'macro)
;		     (setq noevalargs nil)
;		     (setq form (cons(caar form) (cdr form)))
;;		     (setf (car form) (caar form) )
;		      (eval form)
;		     )
;		    #+Maclisp
;		    ((EQ (CAR U) 'MFEXPR*S)
;		     (SETQ NOEVALARGS NIL)
;		     ;; use macsyma Trace if you want to trace this call.
;		     (SUBRCALL T (CADR U) FORM))
;		    ((EQ (CAR U) 'T-MFEXPR) (APPLY (CADR U) (CDR FORM)))
;		    (T (MARGCHK (CAAR FORM) (CDR FORM))
;		       (APPLY (CADR U) (MEVALARGS (CDR FORM))))))
;	    B   #+(OR PDP10 Multics Franz NIL cl)
;	     (IF (AND (NOT ARYP) (LOAD-FUNCTION (CAAR FORM) T)) (GO A))
;	     (BADFUNCHK (CAAR FORM) (CAAR FORM) NIL)
;	     (IF (SYMBOLP (CAAR FORM))
;		 (SETQ U (BOUNDP (CAAR FORM)))
;		 (RETURN (MEVAL1-EXTEND FORM)))
;	    C   (COND ((OR (NULL U)
;			   (AND (GET (CAAR FORM) 'OPERATORS) (NOT ARYP))
;			   (EQ (CAAR FORM) (SETQ U (SYMBOL-VALUE (CAAR FORM)))))
;		       (SETQ FORM (MEVAL2 (MEVALARGS (CDR FORM)) FORM))
;		       (RETURN (OR (AND (MGET (CAAR FORM) 'ATVALUES)
;					(AT1 FORM)) FORM)))
;		      ((AND ARYP (GET (CAAR FORM) 'NONARRAY))
;		       (RETURN (CONS (CONS (CAAR FORM) ARYP)
;				     (MEVALARGS (CDR FORM)))))
;		      ((ATOM U)
;		       (BADFUNCHK (CAAR FORM) U NIL)
;		       (SETQ FORM (CONS (CONS (GETOPR U) ARYP) (CDR FORM)))
;		       (GO A))
;		      ((EQ (CAAR U) 'LAMBDA)
;		       (IF ARYP
;			   (MERROR "Improper array call")
;			   (RETURN (MLAMBDA U (CDR FORM)
;					    (CAAR FORM) NOEVALARGS))))
;		      (T (RETURN (MAPPLY1 U (MEVALARGS (CDR FORM))
;					 (CAAR FORM))))))))
;	(T (MAPPLY1 (CAAR FORM) (MEVALARGS (CDR FORM)) (CAAR FORM)))))
;  
;;; This function substitutes for the use of GETL on the
;;; EXPR, FEXPR, MACRO, SUBR, FSUBR, LSUBR, or ARRAY property.
;;; Note: This function used to be incompatible with GETL simply
;;;       to save two conses per function call in MEVAL, but considering
;;;       the amount of consing going on elsewere (e.g. the variable binding!)
;;;       and considering the #+LISPM grossness this introduced, it was
;;;       a bad idea. N.B. If you want efficiency in macsyma evaluation
;;;       use the Macsyma->lisp translator. -gjc
;;; DEFICIENCIES: Functions with some args &QUOTE and some args not
;;; will fail unless MEVAL is changed to call fexprs by (EVAL `(,FOO ,@L))
;;; instead of (APPLY FOO L). However: Officially everything uses 
;;; DEFMSPEC now, there are no fexprs.
;;;; **** This should be rewritten to use the new function FUNCTIONP. ****
;
;;;;from the doe tape:
;;#+LISPM
;;(DEFUN GETL-LM-FCN-PROP (SYM PROPS)
;;  (PROG (FN RPROP ARGS-INFO)
;;    (SETQ RPROP
;;	  (AND (FBOUNDP SYM)
;;	       (SELECT (%DATA-TYPE (SETQ FN (SYMBOL-FUNCTION SYM)))
;;		 (DTP-SYMBOL (RETURN (GETL-LM-FCN-PROP FN PROPS)))
;;		 (DTP-LIST (COND ((MEMQ (CAR FN) '(MACRO SUBST))
;;				  'MACRO FN)
;;				 ((EQ (CAR FN) 'NAMED-LAMBDA)
;;				  (IF (MEMQ '&QUOTE (CADDR FN))
;;				      'FEXPR 'EXPR))
;;				 ((EQ (CAR FN) 'LAMBDA)
;;				  (IF (MEMQ '&QUOTE (CADR FN)) 'FEXPR 'EXPR))
;;				 (T (ERROR () "Unknown definition of ~S -- ~S"
;;					    SYM FN))))
;;		 (DTP-ARRAY-POINTER 'ARRAY)
;;		 ((DTP-FEF-POINTER DTP-U-ENTRY)
;;		  (SETQ ARGS-INFO (%ARGS-INFO FN))
;;		  (COND ((BIT-TEST (f+ %ARG-DESC-QUOTED-REST
;;				      %ARG-DESC-FEF-QUOTE-HAIR)
;;				   ARGS-INFO)
;;			 'FSUBR)
;;			((NOT (= (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
;;				 (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
;;			 'LSUBR)
;;			(T 'SUBR)))
;;		 (T (ERROR () "Unknown object in function cell of ~S -- ~S"
;;			    SYM FN)))))
;;    (RETURN (AND RPROP
;;		 (MEMQ RPROP PROPS)
;;		 (LIST RPROP FN)))))
;;
;
;
;;altered by wfs to fix the translated saved definitions so they work
;#+LISPM 
;(DEFUN GETL-LM-FCN-PROP (SYM PROPS)
;  (PROG (FN RPROP ARGS-INFO)
;	(cond  ((symbolp sym)
;		(cond ((get sym 'translated)
;		       (setq fn (getl sym props)))))
;	       ;;case of compiled function.
;	       ((functionp sym)(setq fn sym))  
;	       (t nil))
;	(SETQ RPROP
;	      (AND (or fn (FBOUNDP SYM))
;		   (let ((funct  (COND ((NULL FN)
;					(SETQ FN (SYMBOL-FUNCTION SYM)))
;				       (T FN))))
;		     (COND ((ml-typep funct 'symbol)
;			    (RETURN (GETL-LM-FCN-PROP FN PROPS)))
;			   ((ml-typep funct 'list)
;			    (COND ((zl-MEMBER (CAR FN) '(MACRO SUBST special)) 'MACRO)
;				  ((EQ (CAR FN) 'NAMED-LAMBDA)
;				   (IF (MEMQ '&QUOTE (CADDR FN)) 'FEXPR 'EXPR))
;				  ((eq (car fn) 'si:digested-lambda)
;				   'subr)
;				  ((AND (MEMQ (CAR FN) PROPS)
;					(zl-MEMBER (CAADR FN) '(NAMED-LAMBDA ))) 
;				   (RETURN FN))
;				  ((EQ (CAR FN) 'LAMBDA)
;				   (IF (MEMQ '&QUOTE (CADR FN)) 'FEXPR 'EXPR))
;				  (T (ERROR  "Unknown definition of ~S -- ~S" SYM FN))))
;			   ((ml-typep funct 'array)
;			    'array)
;			   ((ml-typep funct 'COMPILED-FUNCTION)
;			    (SETQ ARGS-INFO (%ARGS-INFO FN))
;			    #+ti
;			    (COND ((ldb-test %%arg-desc-quoted-rest args-info) 'fsubr)
;				  ((ldb-test %%arg-desc-fef-quote-hair args-info) 'fsubr)
;				  ((NOT (= (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
;					   (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
;				   'LSUBR)
;				  (T 'SUBR))
;			    #-ti
;			    (COND ((BIT-TEST (DPB 1 %%ARG-DESC-QUOTED 0) ARGS-INFO) 'FSUBR)
;				  ((NOT (= (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
;					   (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
;				   'LSUBR)
;				  (T 'SUBR)))
;			   ( t			;(TYPEP funct 'T)
;			    (ERROR  "Unknown object in function cell of ~S -- ~S"
;				    SYM FN))))))
;	(RETURN (AND RPROP
;		     (MEMQ RPROP PROPS)
;		     (LIST RPROP FN)))))
;

;;the following is fine but we don't need it.
;(defun maxima-symbol-function (sym &aux tem fn)
; (check-arg sym symbolp "symbol")
; (cond ((fboundp sym)
;	(setq fn (symbol-function sym))
;	(cond ((functionp fn)(values fn 'subr))
;	      ;;really just suitable for apply I think.
;              ((macro-function sym)(values fn 'macro))
;	      ((arrayp fn)(values fn 'array))
;	      (t (error "unknown fn"))))
;       ((setq tem (symbol-array sym))
;	(values tem 'array))
;       ((setq tem (get sym 'mfexpr*))
;	(values tem 'mfexpr*))
;       (t nil)))

;(DEFUN GETL-LM-FCN-PROP (SYM PROPS)
;  (check-arg sym symbolp "symbol")
;  (and (fboundp sym) (multiple-value-bind (fn typ) (maxima-symbol-function sym)
;		       (cond ((memq typ props)(list typ fn))
;			     ((eq typ 'lambda)(list 'subr fn))
;			     (t nil)))))

;
;(DEFUN GETL-LM-FCN-PROP (SYM PROPS &aux fn typ)
;  (check-arg sym symbolp "symbol")
;  (cond ((fboundp sym)
;	 (setq fn (symbol-function sym))
;	 (cond
;	   #+lucid
;	   ((macro-function sym)
;	    (setq typ 'macro))
;	   ((functionp fn)
;		;;this is what we did but do we want if 'subr not in props??
;		 (return-from GETL-LM-FCN-PROP (list 'subr fn)))
;		((macro-function sym)
;		 (setq typ 'macro))
;		#+lispm ((arrayp fn)(values fn 'array))
;		(t (error "unknown fn"))))
;	((setq fn (symbol-array sym))
;	 (setq typ 'array))
;	((setq fn (get sym 'mfexpr*))
;	 (setq typ 'mfexpr*)))
;  (and typ (member typ props :test 'eq) (list typ fn)))	
;
(DEFUN GETL-LM-FCN-PROP (SYM PROPS &aux fn typ)
  (check-arg sym symbolp "symbol")
  (setq fn sym)
  (cond
    ((functionp fn)
     (setq typ 'subr))
    ((macro-function sym)
     (setq typ 'macro))
    #+lispm ((arrayp fn)(values fn 'array))
    ((setq fn (symbol-array sym))
     (setq typ 'array))
    ((setq fn (get sym 'mfexpr*))
     (setq typ 'mfexpr*)))
  (and typ (member typ props :test 'eq) (list typ fn)))	


;;#+LISPM
;;(DEFUN GETL-LM-FCN-PROP (SYM PROPS)
;;  (PROG (FN RPROP ARGS-INFO)
;;    (SETQ RPROP
;;	  (AND (FBOUNDP SYM)
;;	       (TYPECASE (SETQ FN (SYMBOL-FUNCTION SYM))
;;		 (:SYMBOL (RETURN (GETL-LM-FCN-PROP FN PROPS)))
;;		 (:LIST (COND ((MEMQ (CAR FN) '(MACRO SUBST)) 'MACRO)
;;			      ((EQ (CAR FN) 'NAMED-LAMBDA)
;;			       (IF (MEMQ '&QUOTE (CADDR FN))
;;				   'FEXPR 'EXPR))
;;			      ((EQ (CAR FN) 'LAMBDA)
;;			       (IF (MEMQ '&QUOTE (CADR FN)) 'FEXPR 'EXPR))
;;			      (T (ERROR () "Unknown definition of ~S -- ~S"
;;					 SYM FN))))
;;		 (:ARRAY (kw array))
;;		 (:COMPILED-FUNCTION
;;		  (SETQ ARGS-INFO (%ARGS-INFO FN))
;;		  (COND ((BIT-TEST #-3600 (f+ %ARG-DESC-QUOTED-REST
;;					     %ARG-DESC-FEF-QUOTE-HAIR)
;;				   #+3600 (DPB 1 SI:%%ARG-DESC-QUOTED 0)
;;				   ARGS-INFO)
;;			 'FSUBR)
;;			((NOT (= (LDB %%ARG-DESC-MIN-ARGS ARGS-INFO)
;;				 (LDB %%ARG-DESC-MAX-ARGS ARGS-INFO)))
;;			 'LSUBR)
;;			(T 'SUBR)))
;;		 (T (ERROR () "Unknown object in function cell of ~S -- ~S"
;;			    SYM FN)))))
;;    (RETURN (AND RPROP
;;		 (MEMQ RPROP PROPS)
;;		 (LIST RPROP FN)))))


;#+NIL
;(DEFUN GETL-NIL-FCN-PROP (SYM PROPS)
;  (IF (FBOUNDP SYM)
;      (LET* ((F (SYMBOL-FUNCTION SYM))
;	     (PROP (IF (ATOM F)
;		       (IF (EQ (TYPE-OF F) 'SUBR) 'SUBR 'EXPR)
;		       (CAR F))))
;	(IF (MEMQ PROP PROPS) (LIST PROP F)))))
;    (RETURN (AND RPROP (MEMQ RPROP PROPS) (LIST RPROP FN)))))
 
#+NIL
(defun getl-nil-fcn-prop (sym props)
  (and (fboundp sym)
       (let* ((f (symbol-function sym))
	      (prop (if (atom f)
			(if (ml-typep f (kw COMPILED-FUNCTION)) 'subr 'expr)
			(car f))))
	 (when (memq prop '(defmacro subst)) (setq prop 'macro))
	 (if (memq prop props) (list prop f)))))

(DEFMFUN MEVAL2 (NEWARGS OLD)
  (declare (special aryp))
 (LET ((NEW (CONS (CAR OLD) NEWARGS)) NOSIMP)
      (COND ((NOT (MEMQ 'SIMP (CDAR OLD)))
	     (IF (AND (NOT (EQ (CAAR NEW) 'MLIST)) (EQUAL NEW OLD)) OLD NEW))
	    ((PROG2 (SETQ NOSIMP (NOT (GET (CAAR NEW) 'OPERATORS))) (ALIKE1 NEW OLD))
	     (IF NOSIMP OLD (CONS (DELSIMP (CAR OLD)) (CDR OLD))))
	    (NOSIMP (IF ARYP NEW (CONS (CONS (CAAR NEW) '(SIMP)) NEWARGS)))
	    (T (CONS (CONS (CAAR NEW) ARYP) NEWARGS)))))
 
(DEFUN MPARAMS (VARS)
  (MAPCAR #'(LAMBDA (X) (COND ((ATOM X) X)
			      ((ATOM (CADR X)) (CADR X))
			      (T (CADADR X))))
	  (CDR VARS)))

(DEFMFUN MOP (FORM) (IF (EQ (CAAR FORM) 'MQAPPLY) (CADR FORM) (CAAR FORM)))
	
(DEFMFUN MARGS (FORM) (IF (EQ (CAAR FORM) 'MQAPPLY) (CDDR FORM) (CDR FORM)))

(DEFUN BADFUNCHK (NAME VAL FLAG)
 (IF (OR FLAG (NUMBERP VAL) (MEMQ VAL '(T NIL $%E $%PI $%I)))
;    (OR FLAG (AND (NOT $OPERATORS)
;		   (OR (NUMBERP VAL) (MEMQ VAL '(T NIL $%E $%PI $%I)))))
     (IF (AND (ATOM NAME) (NOT (EQUAL VAL NAME)))
	 (MERROR "~:M evaluates to ~M~
		  ~%Improper name or value in functional position."
		 NAME VAL)
	 (MERROR "Improper name or value in functional position:~%~M"
		 VAL))))

#+MacLisp
(DEFUN MARGCHK (FN ARGS) 
 (LET (EXPR)
      (OR (NOT (OR (SETQ EXPR (GET FN 'EXPR)) (GET FN 'SUBR)))
	  (NOT (ARGS FN))
	  (CAR (ARGS FN))
	  (LET ((NNEED (CDR (ARGS FN))) (NGIVEN (LENGTH ARGS)))
	       (WHEN (NOT (= NNEED NGIVEN))
		     (IF (AND EXPR (NOT (MGET FN 'TRACE))
			      (OR (NULL (CADR EXPR)) (NOT (ATOM (CADR EXPR)))))
			 (SETQ FN (CONS (NCONS FN) (CADR EXPR))))
		     (MERROR "Too ~M arguments supplied to ~M:~%~M"
			     (IF (< NNEED NGIVEN) '|&many| '|&few|)
			     FN
			     (CONS '(MLIST) ARGS)))))))

#+Franz
(defun margchk (fn args)
   (let (expr argdesc)
      (or (not (symbolp fn))
	  (not (getd fn))
	  (null (setq argdesc (car (get fn 'fcn-info))))
	  (let ((minimum (car argdesc))
		(maximum (cdr argdesc))
		(ngiven (length args)))
	     (cond ((or (and maximum (> ngiven maximum))
			(and minimum (< ngiven minimum)))
		    (merror "Too ~M arguments supplied to ~M:~%~M"
			    (cond ((> ngiven maximum) '|&many|)
				  (t '|&few|))
			    fn
			    (cons '(mlist) args))))))))



;;	  (LET ((NNEED (car argdesc)) (NGIVEN (LENGTH ARGS)))
;	     (cond ((NOT (= NNEED NGIVEN))
;		    (MERROR "Too ~M arguments supplied to ~M:~%~M"
;			    (cond ((< NNEED NGIVEN) '|&many|)
;				  (t '|&few|))
;			    FN
;			    (CONS '(MLIST) ARGS))))))))
;#+LISPM
;(DEFUN MARGCHK (FN ARGS &AUX ARG-DESC MIN-NARGS MAX-NARGS ACTUAL-NARGS)
;  (AND (SYMBOLP FN)
;       (FBOUNDP FN)
;       (PROGN (SETQ ARG-DESC (ARGS-INFO FN)
;		    MIN-NARGS (LDB %%ARG-DESC-MIN-ARGS ARG-DESC)
;		    MAX-NARGS (LDB %%ARG-DESC-MAX-ARGS ARG-DESC)
;		    ACTUAL-NARGS (LENGTH ARGS))
;	      (OR (< ACTUAL-NARGS MIN-NARGS)
;		  (AND (ZEROP (LOGAND #-3600 (f+ %ARG-DESC-QUOTED-REST %ARG-DESC-EVALED-REST)
;;;				      #+3600 (DPB 1 SI:%%ARG-DESC-REST-ARG 0)
;				        #+(or lispm 3600)
;					(DPB 1 SI:%%ARG-DESC-REST-ARG 0)
;				      ARG-DESC))     ; has a rest argument means
;		       (> ACTUAL-NARGS MAX-NARGS)))) ; don't check max args.
;       (MERROR "Too ~M arguments supplied to ~M:~%~M"
;	       (IF (< ACTUAL-NARGS MIN-NARGS) '|&few| '|&many|)
;	       `((,FN) ,@(ARGLIST FN))
;	       `((MLIST) ,@ARGS))))





(DEFMFUN MBIND (LAMVARS FNARGS FNNAME)
  (DO ((VARS LAMVARS (CDR VARS)) (ARGS FNARGS (CDR ARGS)))
      ((COND ((AND VARS ARGS) NIL)
	     ((AND (NULL VARS) (NULL ARGS)))
	     (T (MERROR "Too ~M arguments supplied to ~M:~%~M"
		        (IF VARS '|&few| '|&many|)
		        (IF FNNAME (CONS (NCONS FNNAME) LAMVARS)
				   '|&a function|)
		        (CONS '(MLIST) FNARGS)))))
      (LET ((VAR (CAR VARS)))
	(IF (NOT (SYMBOLP VAR))
	    (MERROR "Only symbolic atoms can be bound:~%~M" VAR))
#-Franz (WITHOUT-TTY-INTERRUPTS
	 (LET ((BL (CONS VAR BINDLIST))
	       (ML (CONS (IF (BOUNDP VAR) (SYMBOL-VALUE VAR) MUNBOUND)
			  MSPECLIST)))
	   (SETQ BINDLIST BL MSPECLIST ML)))
#+Franz (SETQ BINDLIST (CONS VAR BINDLIST))
#+Franz (SETQ MSPECLIST (CONS (IF (BOUNDP VAR) (SYMBOL-VALUE VAR) MUNBOUND)
			  MSPECLIST))
	(MSET VAR (CAR ARGS)))))

(DEFMFUN MUNBIND (VARS)
 (DOLIST (VAR (REVERSE VARS))
	 (COND ((EQ (CAR MSPECLIST) MUNBOUND)
		(MAKUNBOUND VAR) (DELQ VAR $VALUES 1))
	       (T (LET ((MUNBINDP T)) (MSET VAR (CAR MSPECLIST)))))
	 (SETQ MSPECLIST (CDR MSPECLIST) BINDLIST (CDR BINDLIST))))

;This takes the place of something like
; (DELETE (ASSOC (NCONS VAR) $DEPENDENCIES) $DEPENDENCIES 1)
;(defun mfunction-delete (var fn-a-list)
;  (sys:delete var fn-a-list :count 1
;	      :test #'(lambda (var elt)
;			(declare (optimize (speed 3) (safety 0)))
;			(and elt (consp (setq elt (car elt)))
;			     (eq (car elt) var) (null (cdr elt))))))

(defun mfunction-delete (var fn-a-list)
  (zl-DELETE (zl-ASSOC (ncons var) fn-a-list) fn-a-list 1))



(DEFMSPEC MLOCAL (L)
 (SETQ LOCLIST (CONS NIL LOCLIST))
 (LET ((MLOCP T)) (MEVAL `(($LOCAL) ,@(CDR L)))))

(DEFMSPEC $LOCAL (L) (SETQ L (CDR L))
 (IF (NOT MLOCP) (MERROR "Improper call to LOCAL"))
 (NOINTERRUPT 'TTY)
 (DOLIST (VAR L)
	 (COND ((NOT (symbolp VAR))
		(NOINTERRUPT NIL) (IMPROPER-ARG-ERR VAR '$LOCAL))
	       ((AND (MGET VAR 'array)
		     #+MacLisp (GET VAR 'array)
		     #+cl (arrayp (symbol-array var))
		     )
		(NOINTERRUPT NIL)
		(MERROR "Attempt to bind a complete array ~M" VAR)))
	 (SETQ MPROPLIST (CONS (GET VAR 'MPROPS) MPROPLIST)
	       FACTLIST (CONS (GET VAR 'DATA) FACTLIST))
	 (DOLIST (FACT (CAR FACTLIST)) (PUTPROP FACT -1 'ULABS))
	  (progn
	 (mfunction-delete var $functions)
	 (mfunction-delete var $macros)
	 (mfunction-delete var $dependencies))
	 (DELQ VAR $ARRAYS 1)
	 (ZL-REMPROP VAR 'MPROPS)
	 (ZL-REMPROP VAR 'DATA))
 (RPLACA LOCLIST (REVERSE L))
 (SETQ MLOCP NIL)
 (NOINTERRUPT NIL)
 '$DONE)


(DEFUN MUNLOCAL NIL
 (NOINTERRUPT 'TTY)
 (DOLIST (VAR (CAR LOCLIST))
   (let ((MPROP  (CAR MPROPLIST))( Y  NIL)( FACT  (CAR FACTLIST)))
	   (REMCOMPARY VAR)
	   (CPUT VAR MPROP 'MPROPS)
	   (COND ((SETQ Y (old-GET MPROP 'MEXPR))
		  (ADD2LNC (CONS (NCONS VAR) (CDADR Y)) $FUNCTIONS))
		 (T (mfunction-delete var $functions)))
	   (COND ((SETQ Y (old-GET MPROP 'MMACRO))
		  (ADD2LNC (CONS (NCONS VAR) (CDADR Y)) $MACROS))
		 (T (mfunction-delete var $macros)))
	   (COND ((OR (old-GET MPROP 'array) (old-GET MPROP 'HASHAR))
		  (ADD2LNC VAR $ARRAYS))
		 (T (DELQ VAR $ARRAYS 1)))
	   (COND ((SETQ Y (OLD-GET MPROP 'DEPENDS))
		  (ADD2LNC (CONS (NCONS VAR) Y) $DEPENDENCIES))
		 (T (mfunction-delete var $dependencies)))
	   (REMPROPCHK VAR)
	   (MAPC #'REMOV (GET VAR 'DATA))
	   (CPUT VAR FACT 'DATA)
	   (DOLIST (U FACT) (ZL-REMPROP U 'ULABS))
	   (SETQ MPROPLIST (CDR MPROPLIST) FACTLIST (CDR FACTLIST))))
 (SETQ LOCLIST (CDR LOCLIST))
 (NOINTERRUPT NIL))

(declare-top (MACROS T))
;;do we really need this??
;;since its incompatible with the special definition


;(defmacro msetq (&rest l) `(mset ',(first l) ,(second l)))

(defmacro msetq (a b) `(mset ',a ,b))

		;; A "run-time macro" needed by MATCOM/MATRUN.
(declare-top (MACROS NIL))
;;works with the defms
(DEFMSPEC MSETQ (L)
  (TWOARGCHECK L)
  (MSET (SIMPLIFYA (CADR L) NIL) (MEVAL (CADDR L))))

(DEFUN MSET (X Y)
   (declare (object y x))
    (PROG NIL
          (COND ((OR (NULL $SETCHECK)
                     (EQ $SETCHECK '$SETCHECK)))
                ((AND (OR (ATOM $SETCHECK)
                          (MEMALIKE X (CDR $SETCHECK))
                          (AND (NOT (ATOM X))
                               (MEMALIKE (CAAR X) (CDR $SETCHECK))))
                      (NOT (EQ X Y)))
                 (DISPLA (LIST '(MTEXT) (DISP2 X) '| set to | Y))
                 (IF $SETCHECKBREAK
                     (LET (($SETVAL Y))
                          (MERRBREAK T)
                          (SETQ Y $SETVAL)))))
          (COND ((ATOM X)
                 (WHEN (OR (NOT (SYMBOLP X))
                           (MEMQ X '(T NIL))
                           (MGET X '$NUMER)
                           (char= (GETCHARN X 1) #\&))
                       (IF MUNBINDP (RETURN NIL))
                       (IF (MGET X '$NUMER)
                           (MERROR "~:M improper value assignment to a numerical quantity" X)
                           (MERROR "~:M improper value assignment" X)))
                 (LET ((F (GET X 'ASSIGN)))
                      (IF (AND F (OR (NOT (EQ X Y))
                                     (MEMQ F '(NEVERSET READ-ONLY-ASSIGN))))
                          (IF (EQ (FUNCALL F X Y) 'MUNBINDP) (RETURN NIL))))
                 (COND ((AND (NOT (BOUNDP X))
                             (NOT DSKSETP))
                        (ADD2LNC X $VALUES))
                       ((AND (NOT (EQ X Y))
                             (OPTIONP X))
                        (IF $OPTIONSET (MTELL "~:M option is being set.~%" X))
                        (IF (NOT (EQ X '$LINENUM)) (ADD2LNC X $MYOPTIONS))))
                 (RETURN (SET X Y)))
                ((MEMQ 'ARRAY (CDAR X))
                 (RETURN (ARRSTORE X Y)))
                ((AND $SUBSCRMAP (MEMQ (CAAR X) '(MLIST $MATRIX)))
                 (RETURN (OUTERMAP1 'MSET X Y)))
                (T (MERROR "Improper value assignment:~%~M" X)))))



(DEFMSPEC $EV (L) (SETQ L (CDR L))
 (LET ((EVP T) (NOUNL NOUNL) ($FLOAT $FLOAT) ($NUMER $NUMER)
       ($EXPOP $EXPOP) ($EXPON $EXPON) ($DOALLMXOPS $DOALLMXOPS)
       ($DOSCMXOPS $DOSCMXOPS) (DERIVFLAG DERIVFLAG) ($DETOUT $DETOUT)
       (NOUNSFLAG NOUNSFLAG) (RULEFCNL RULEFCNL))
   (IF (AND (CDR L) (NULL (CDDR L)) (EQ (CAR L) '$%E) (EQ (CADR L) '$NUMER))
       (SETQ L (APPEND L '($%ENUMER))))
   (DO ((L (CDR L) (CDR L)) (BNDVARS) (BNDVALS) (LOCVARS) (EXP (CAR L))
	(SUBSL) (EVFLG 0) (RATF) (DERIVLIST) (EVFUNL) (FUNCL) (PREDFLG)
	(NOEVAL (MEMQ '$NOEVAL (CDR L))))
       ((NULL L)
	(MBINDING (BNDVARS BNDVARS)
		  (MEVAL `((MLOCAL) ,@LOCVARS))
		  (LET ($TRANSLATE) (MAPC #'MEVAL1 FUNCL))
		  (LET ($NUMER) (SETQ EXP (MEVALATOMS EXP)))
		  (IF ($RATP EXP) (SETQ RATF T EXP ($RATDISREP EXP)))
		  (IF (SPECREPP EXP) (SETQ EXP (SPECDISREP EXP)))
		  (WHEN SUBSL
			(SETQ EXP (SIMPLIFY EXP))
			(DOLIST (ITEM SUBSL)
				(SETQ EXP (MAXIMA-SUBSTITUTE (MEVAL (CAR ITEM))
						      (MEVAL (CDR ITEM))
						      EXP)))))
	(MBINDING (BNDVARS BNDVALS)
		  (IF (AND $NUMER NOEVAL $%ENUMER)
		      (SETQ EXP (MAXIMA-SUBSTITUTE %E-VAL '$%E EXP)))
		  (SETQ EXP (IF NOEVAL
				(RESIMPLIFY EXP)
				(SIMPLIFY (IF PREDFLG (MEVALP EXP) (MEVAL1 EXP)))))
		  (IF (OR (> EVFLG 0) $INFEVAL)
		      (PROG (EXP1)
			    (SETQ EXP (SPECREPCHECK EXP))
		       LOOP (DO ((L EVFUNL (CDR L)) (EXP2 EXP))
				((NULL L) (SETQ EXP1 (MEVAL EXP2)))
				(SETQ EXP2 (LIST (NCONS (CAR L)) EXP2)))
			    (DOLIST (ITEM SUBSL)
				    (SETQ EXP1 (MAXIMA-SUBSTITUTE (MEVAL (CAR ITEM))
							   (MEVAL (CDR ITEM))
							   EXP1)))
			    (COND ((OR (AND (NOT $INFEVAL)
					    (= (SETQ EVFLG (f1- EVFLG)) 0))
				       (PROG2 (SETQ EXP1 (SPECREPCHECK EXP1))
					      (ALIKE1 EXP EXP1)))
				   (SETQ EXP EXP1))
				  (T (SETQ EXP EXP1) (GO LOOP)))))
		  (IF (AND RATF (NOT $NUMER) (NOT $FLOAT))
		      (SETQ EXP (LET ($NOREPEAT) (RATF EXP)))))
	(MUNLOCAL)
	EXP)
       (IF (NOT (OR (ATOM (CAR L))
		    (MEMQ 'array (CDAAR L))
		    (MEMQ (CAAAR L) '(MQUOTE MSETQ MLIST MEQUAL MDEFINE MSET
				      MDEFMACRO $EXPAND $LOCAL $DERIVLIST))))
	   (SETQ L (CONS (MEVAL (CAR L)) (CDR L))))
       (COND ((OR (ATOM (CAR L)) (MEMQ 'array (CDAAR L)) (EQ (CAAAR L) 'MQUOTE))
	      (OR (AND (SYMBOLP (CAR L))
		       (COND ((EQ (CAR L) '$EVAL) (SETQ EVFLG (f1+ EVFLG)))
			     ((MEMQ (CAR L) '($NOEVAL $RESCAN)))
			     ((EQ (CAR L) '$DETOUT)
			      (SETQ $DOALLMXOPS NIL $DOSCMXOPS NIL $DETOUT T))
			     ((EQ (CAR L) '$NUMER) (SETQ $NUMER T $FLOAT T))
			     ((EQ (CAR L) '$NOUNS) (SETQ NOUNSFLAG T))
			     ((EQ (CAR L) '$PRED) (SETQ PREDFLG T))
			     ((EQ (CAR L) '$EXPAND)
			      (SETQ $EXPOP $MAXPOSEX $EXPON $MAXNEGEX))
			     ((EQ (CAR L) '%DERIVATIVE)
			      (SETQ DERIVFLAG T DERIVLIST NIL))
			     ((GET (CAR L) 'EVFLAG)
			      (SETQ BNDVARS (CONS (CAR L) BNDVARS)
				    BNDVALS (CONS (GET (CAR L) 'EVFLAG) BNDVALS)))
			     ((GET (CAR L) 'EVFUN)
			      (SETQ EXP (EVFUNMAKE (CAR L) EXP)
				    EVFUNL (NCONC EVFUNL (NCONS (CAR L)))))))
		  (LET ((FL (MEVAL (CAR L))))
		       (COND ((SYMBOLP FL)
			      (COND ((EQ FL '$DIFF)
				     (SETQ L (LIST* NIL '$DEL (CDR L))))
				    ((EQ FL '$RISCH)
				     (SETQ L (LIST* NIL '$INTEGRATE (CDR L)))))
			      (SETQ NOUNL (CONS ($NOUNIFY FL) NOUNL)))
			     ((NUMBERP FL) (IMPROPER-ARG-ERR (CAR L) '$EV))
			     ((EQ (CAAR FL) 'MLIST)
			      (SETQ L (APPEND FL (CDR L))))
			     ((MEMQ (CAAR FL)
				    '(MSETQ MEQUAL MDEFINE MDEFMACRO MSET))
			      (SETQ L (LIST* NIL FL (CDR L))))
			     (T (IMPROPER-ARG-ERR (CAR L) '$EV))))))
	     ((NOT (MEMQ (CAAAR L) '(MSETQ MLIST MEQUAL MDEFINE MDEFMACRO
				     $EXPAND $LOCAL $DERIVLIST MSET)))
	      (IMPROPER-ARG-ERR (CAR L) '$EV))
	     ((EQ (CAAAR L) '$EXPAND)
	      (COND ((NULL (CDAR L)) (SETQ $EXPOP $MAXPOSEX $EXPON $MAXNEGEX))
		    ((NULL (CDDAR L)) (SETQ $EXPOP (CADAR L) $EXPON $MAXNEGEX))
		    (T (SETQ $EXPOP (CADAR L) $EXPON (CADDAR L)))))
	     ((MEMQ (CAAAR L) '(MDEFINE MDEFMACRO))
	      (LET ((FUN (CADAR L)) $use_fast_arrays)
		(IF (EQ (CAAR FUN) 'MQAPPLY) (SETQ FUN (CADR FUN)))
		(SETQ FUN ($VERBIFY (CAAR FUN)))
		(SETQ FUNCL (NCONC FUNCL (NCONS (CAR L)))
		      LOCVARS (APPEND LOCVARS (NCONS FUN)))
		(IF (RULECHK FUN) (SETQ RULEFCNL (CONS FUN RULEFCNL)))))
	     ((EQ (CAAAR L) '$LOCAL) (SETQ LOCVARS (APPEND LOCVARS (CDAR L))))
	     ((EQ (CAAAR L) '$DERIVLIST) (SETQ DERIVFLAG T DERIVLIST (CDAR L)))
	     ((AND (EQ (CAAAR L) 'MSET)
		   (SETQ L (CONS (LIST '(MSETQ) (MEVAL (CADAR L)) (CADDAR L))
				 (CDR L)))
		   NIL))
	     ((MEMQ (CAAAR L) '(MSETQ MEQUAL))
	      (IF (AND (MSETQP (CAR L)) (MSETQP (CADDAR L)))
		  (SETQ L (NCONC (|:SPREAD| (CAR L)) (CDR L))))
	      (IF (OR NOEVAL (NOT (ATOM (CADAR L))))
		  (SETQ SUBSL (NCONC SUBSL (LIST (CONS (CADDAR L) (CADAR L))))))
	      (IF (ATOM (CADAR L))
		  (SETQ BNDVARS (CONS (CADAR L) BNDVARS)
			BNDVALS (CONS (MEVAL (SPECREPCHECK (CADDAR L))) BNDVALS))))
	     (T (SETQ L (APPEND (CAR L) (CDR L))))))))

(DEFMFUN MEVALATOMS (EXP)
 (COND ((ATOM EXP) (MEVAL1 EXP))
       ((MEMQ 'array (CDAR EXP))
	(LET (EXP1)
	  (LET ((EVARRP T)) (SETQ EXP1 (CATCH 'EVARRP (MEVAL1 EXP))))
	  (IF (EQ EXP1 'NOTEXIST)
	      (CONS (CAR EXP) (MAPCAR #'MEVALATOMS (CDR EXP)))
	      EXP1)))
       ((EQ (CAAR EXP) 'MQUOTE) (CADR EXP))
       ((MEMQ (CAAR EXP) '(MSETQ $DEFINE))
	(LIST (CAR EXP) (CADR EXP) (MEVALATOMS (CADDR EXP))))
       ((OR (AND (EQ (CAAR EXP) '$EV)
		 (CDR EXP)
		 (OR (NULL (CDDR EXP)) (EQUAL (CDDR EXP) '($EVAL))))
	    (EQ (CAAR EXP) 'MPROGN))
	(CONS (CAR EXP) (CONS (MEVALATOMS (CADR EXP)) (CDDR EXP))))
       ((MEMQ (CAAR EXP) '($SUM $PRODUCT %SUM %PRODUCT))
	(IF MSUMP
	   (MEVAL EXP)
	   (LIST (CAR EXP) (CADR EXP) (CADDR EXP)
		 (MEVALATOMS (CADDDR EXP)) (MEVALATOMS (CAR (CDDDDR EXP))))))
       ((AND (EQ (CAAR EXP) '$%TH) (EQ (ml-typep (SIMPLIFY (CADR EXP))) 'fixnum))
	(MEVAL1 EXP))
       ((PROG2 (AUTOLDCHK (CAAR EXP))
	       (AND (OR (GETL-FUN (CAAR EXP) '(FSUBR FEXPR))
			(GETL (CAAR EXP) '(MFEXPR* MFEXPR*S)))
		    (NOT (GET (CAAR EXP) 'EVOK))))
	EXP)
       ((MGETL (CAAR EXP) '(MFEXPRP T-MFEXPR))
	(CONS (CAR EXP)
	      (DO ((A (OR (CDR (MGET (CAAR EXP) 'T-MFEXPR))
			  (CDADR (MGET (CAAR EXP) 'MEXPR)))
		      (CDR A))
		   (B (CDR EXP) (CDR B)) (L))
		  ((NOT (AND A B)) (NREVERSE L))
		  (COND ((MDEFLISTP A)
			 (RETURN (NRECONC L (IF (MQUOTEP (CADAR A))
						B
						(MAPCAR #'MEVALATOMS B)))))
			((MQUOTEP (CAR A)) (SETQ L (CONS (CAR B) L)))
			(T (SETQ L (CONS (MEVALATOMS (CAR B)) L)))))))
       ((OR (EQ (CAAR EXP) 'MMACROEXPANDED)
	    (AND $TRANSRUN (GET (CAAR EXP) 'TRANSLATED-MMACRO))
	    (MGET (CAAR EXP) 'MMACRO))
	(MEVALATOMS (MMACROEXPAND EXP)))
       (T (CONS (CAR EXP) (MAPCAR #'MEVALATOMS (CDR EXP))))))

(PROG1 '(EVOK properties)
       (MAPC #'(LAMBDA (X) (PUTPROP X T 'EVOK))
	     '($MAP $MAPLIST $FULLMAP $MATRIXMAP $FULLMAPL $OUTERMAP $SCANMAP
	       $APPLY)))

(DEFUN EVFUNMAKE (FUN EXP)
 (IF (MSETQP EXP)
     (LIST (CAR EXP) (CADR EXP) (EVFUNMAKE FUN (CADDR EXP)))
     (LIST (NCONS FUN) EXP)))

(DEFUN |:SPREAD| (X)
 (DO ((VAL (DO ((X X (CADDR X))) (NIL)
	       (IF (NOT (MSETQP (CADDR X))) (RETURN (CADDR X)))))
      (X X (CADDR X)) (L))
     ((NOT (MSETQP X)) L)
     (SETQ L (CONS (LIST (CAR X) (CADR X) VAL) L))))

(DEFMFUN MSETQP (X) (AND (NOT (ATOM X)) (EQ (CAAR X) 'MSETQ)))

(DEFMFUN MQUOTEP (X) (AND (NOT (ATOM X)) (EQ (CAAR X) 'MQUOTE)))

(DEFMSPEC MQUOTE (FORM) (CADR FORM))

(DEFMFUN $SUBVARP (X) (AND (NOT (ATOM X)) (MEMQ 'array (CDAR X)) T))

(DEFMFUN MSETERR (X Y)
 (IF MUNBINDP
     'MUNBINDP
     (MERROR "Attempt to set ~:M to ~M~%Improper value assignment" X Y)))

(PROG1 '(ASSIGN properties)
       (MAPC #'(LAMBDA (X) (PUTPROP (CAR X) (CADR X) 'ASSIGN))
	     '(($LINEL MSETCHK) (*read-base* MSETCHK) (*print-base* MSETCHK) (MODULUS MSETCHK)
	       ($INFOLISTS NEVERSET) ($TRACE NEVERSET) ($RATWEIGHTS MSETCHK)
	       ($RATVARS MSETCHK) ($SETCHECK MSETCHK) ($GCD MSETCHK)
	       ($DOTASSOC MSETCHK) ($RATWTLVL MSETCHK) ($RATFAC MSETCHK)
	       ($ALL NEVERSET) ($NUMER NUMERSET) ($FORTINDENT MSETCHK)
	       ($GENSUMNUM MSETCHK) ($GENINDEX MSETCHK) ($FPPRINTPREC MSETCHK)
 	       ($FLOATWIDTH MSETCHK) ($PARSEWINDOW MSETCHK) ($OPTIMPREFIX MSETCHK)
	       ($TTYINTNUM MSETCHK))))

(DEFMFUN MSETCHK (X Y)
 (COND ((MEMQ X '(*read-base* *print-base*))
	(COND #-NIL ((EQ Y 'ROMAN))
	      ((OR (NOT (FIXNUMP Y)) (< Y 2) (> Y 35)) (MSETERR X Y))
	      ((EQ X '*read-base*)
	       #+MacLisp (IF (< Y 11) (SSTATUS + NIL) (SSTATUS + T)))))
       ((MEMQ X '($LINEL $FORTINDENT $GENSUMNUM $FPPRINTPREC $FLOATWIDTH
		  $PARSEWINDOW $TTYINTNUM))
	(IF (NOT (fixnump y)) (MSETERR X Y))
	#+MacLisp
	(WHEN (EQ X '$LINEL)
	  (LINEL T (LINEL NIL Y))
	  (DOLIST (FILE OUTFILES) (LINEL FILE Y))
	  (SETQ LINEL Y))
	#+(or cl Franz) (if (eq x '$linel) (setq linel y))
	(COND ((AND (MEMQ X '($FORTINDENT $GENSUMNUM $FLOATWIDTH $TTYINTNUM)) (< Y 0))
	       (MSETERR X Y))
	      ((AND (EQ X '$PARSEWINDOW) (< Y -1)) (MSETERR X Y))
	      ((AND (EQ X '$FPPRINTPREC) (OR (< Y 0) (= Y 1))) (MSETERR X Y))))
       ((MEMQ X '($GENINDEX $OPTIMPREFIX)) (IF (NOT (SYMBOLP Y)) (MSETERR X Y)))
       ((EQ X '$DOTASSOC) (CPUT 'MNCTIMES Y 'ASSOCIATIVE))
       ((EQ X 'MODULUS)
	(COND ((NULL Y))
	      ((INTEGERP Y)
	       (IF (OR (NOT (PRIMEP Y)) (zl-MEMBER Y '(1 0 -1)))
		   (MTELL "Warning: MODULUS being set to ~:M, a non-prime.~%" Y)))
	      (T (MSETERR X Y))))
       ((EQ X '$SETCHECK)
	(IF (NOT (OR (MEMQ Y '($ALL T NIL)) ($LISTP Y))) (MSETERR X Y)))
       ((EQ X '$GCD) (IF (NOT (OR (NULL Y) (MEMQ Y *GCDL*))) (MSETERR X Y)))
       ((EQ X '$RATVARS)
	(IF ($LISTP Y) (APPLY #'$RATVARS (CDR Y)) (MSETERR X Y)))
       ((EQ X '$RATFAC)
	(IF (AND Y $RATWTLVL)
	    (MERROR "RATFAC and RATWTLVL may not both be used at the same time.")))
       ((EQ X '$RATWEIGHTS)
	(COND ((NOT ($LISTP Y)) (MSETERR X Y))
	      ((NULL (CDR Y)) (KILL1 '$RATWEIGHTS))
	      (T (APPLY #'$RATWEIGHT (CDR Y)))))
       ((EQ X '$RATWTLVL)
	(IF (AND Y (NOT (FIXNUMP Y))) (MSETERR X Y))
	(IF (AND Y $RATFAC)
	    (MERROR "RATFAC and RATWTLVL may not both be used at the same time.")))))

(DEFMFUN NUMERSET (ASSIGN-VAR Y)
 ASSIGN-VAR  ; ignored
 (MSET '$FLOAT Y))

(DEFMFUN NEVERSET (X ASSIGN-VAL)
 ASSIGN-VAL  ; ignored
 (IF MUNBINDP 'MUNBINDP (MERROR "Improper value assignment to ~:M" X)))

(DEFMFUN MMAPEV (L)
 (IF (NULL (CDDR L))
     (MERROR "~:M called with fewer than two arguments." (CAAR L)))
 (LET ((OP (GETOPR (MEVAL (CADR L)))))
      (AUTOLDCHK OP)
      (BADFUNCHK (CADR L) OP NIL)
      (CONS OP (MAPCAR #'MEVAL (CDDR L)))))

(DEFMSPEC $MAP (L) (APPLY #'MAP1 (MMAPEV L)))

(DEFMFUN MAP1 N
 (DO ((I N (f1- I))
      (ARGI (SETARG N (FORMAT1 (ARG N))) (FORMAT1 (ARG (f1- I))))
      (OP (OR (MAPATOM (ARG N)) (MOP (ARG N))))
      (FLAG (MAPATOM (ARG N))
	    (OR FLAG (SETQ FLAG (MAPATOM ARGI))
		(AND (NOT MAPLP) (NOT (ALIKE1 (MOP ARGI) OP)))))
      (ARGL NIL (CONS ARGI ARGL))
      (CDRL NIL (OR FLAG (CONS (MARGS ARGI) CDRL))))
     ((= I 1) (IF FLAG 
		  (COND ((NOT $MAPERROR) 
			 (IF $MAPPRINT (MTELL "MAP is doing an APPLY.~%"))
			 (FUNCER (ARG 1) ARGL))
			((AND (= N 2) (MAPATOM (ARG 2)))
			 (IMPROPER-ARG-ERR (ARG 2) '$MAP))
			(T (MERROR "Arguments to MAPL not uniform - cannot map.")))
		  (MCONS-OP-ARGS
		    OP #+NIL (APPLY #'MMAPCAR (ARG 1) CDRL)
		       #-NIL (APPLY #'MMAPCAR (CONS (ARG 1) CDRL)))))))

(DEFMSPEC $MAPLIST (L)
 (LET ((MAPLP T) RES)
      (SETQ RES (apply #'MAP1 (MMAPEV L)))
      (COND ((ATOM RES) (LIST '(MLIST) RES))
	    ((EQ (CAAR RES) 'MLIST) RES)
	    (T (CONS '(MLIST) (MARGS RES))))))

(DEFMFUN MMAPCAR N 
 (DO ((ANS NIL (CONS (FUNCER (ARG 1) ARGL) ANS))
      (ARGL NIL NIL))
     ((DO ((I N (f1- I))) ((= I 1) NIL)
	  (WHEN (NULL (ARG I))
		(WHEN (OR (< I N) (DO ((J 2 (f1+ J))) ((= J N) NIL)
				      (IF (ARG J) (RETURN T))))
		      (IF $MAPERROR
			  (MERROR "Arguments to MAPL are not of the same length."))
		      (IF $MAPPRINT (MTELL "MAP is truncating.~%")))
		(RETURN T))
	  (SETQ ARGL (CONS (CAR (ARG I)) ARGL))
	  (SETARG I (CDR (ARG I))))
      (NREVERSE ANS))))

(DEFUN MAPATOM (X) (OR (SYMBOLP X) (MNUMP X) ($SUBVARP X)))

(DEFMFUN $MAPATOM (X) (IF (MAPATOM (SPECREPCHECK X)) T))

(DEFMSPEC $FULLMAP (L) (SETQ L (MMAPEV L)) (FMAP1 (CAR L) (CDR L) NIL))

(DEFUN FMAP1 (FN ARGL FMAPCAARL)
 (SETQ ARGL (MAPCAR #'FORMAT1 ARGL))
 (DO ((OP (OR (MAPATOM (CAR ARGL)) (MOP (CAR ARGL))))
      (FMAPLVL (f1- FMAPLVL)) (CDR1 ARGL (CDR CDR1)) (ARGI NIL NIL)
      (CDRL NIL (CONS (MARGS (CAR CDR1)) CDRL)))
     ((NULL CDR1)
      (DO ((ANS NIL (CONS (IF BOTTOM (FUNCER FN CARARGL)
				     (FMAP1 FN CARARGL FMAPCAARL))
			  ANS))
	   (CARARGL NIL NIL) (CDRARGL NIL NIL)
	   (CDRL CDRL CDRARGL) (BOTTOM NIL NIL)
	   (DONE (WHEN (MEMQ NIL CDRL)
		       (WHEN (DOLIST (E CDRL) (IF E (RETURN T)))
			     (IF $MAPERROR
				 (MERROR
				  "FULLMAP found arguments with incompatible structure."))
			     (IF $MAPPRINT (MTELL "FULLMAP is truncating.~%")))
		       T)))
	  (DONE (MCONS-OP-ARGS OP (NREVERSE ANS)))
	  (DO ((OP (OR (SETQ BOTTOM (OR (ZEROP FMAPLVL) (MAPATOM (CAAR CDRL))))
		       (MOP (CAAR CDRL))))
	       (ELEML CDRL (CDR ELEML)) (CAARELEML NIL NIL))
	      ((NULL ELEML)
	       (WHEN (AND DONE (DOLIST (E CDRARGL) (IF E (RETURN T))))
		     (IF $MAPERROR
			 (MERROR "FULLMAP found arguments with incompatible structure."))
		     (IF $MAPPRINT (MTELL "FULLMAP is truncating.~%"))))
	      (SETQ CAARELEML (CAAR ELEML))
	      (OR BOTTOM
		  (SETQ BOTTOM
			(OR (MAPATOM CAARELEML)
			    (NOT (ALIKE1 OP (MOP CAARELEML)))
			    (AND FMAPCAARL (NOT (EQ (CAAR CAARELEML) FMAPCAARL))))))
	      (OR DONE (SETQ DONE (NULL (CDAR ELEML))))
	      (SETQ CARARGL (NCONC (NCONS CAARELEML) CARARGL)
		    CDRARGL (NCONC CDRARGL (NCONS (CDAR ELEML)))))))
     (SETQ ARGI (CAR CDR1))
     (IF (OR (MAPATOM ARGI)
	     (NOT (ALIKE1 OP (MOP ARGI)))
	     (AND FMAPCAARL (NOT (EQ (CAAR ARGI) FMAPCAARL))))
	 (COND ($MAPERROR (MERROR "Incorrect call to FULLMAP."))
	       (T (IF $MAPPRINT (MTELL "FULLMAP is doing an APPLY.~%"))
		  (RETURN (FUNCER FN ARGL)))))))

(DEFMSPEC $MATRIXMAP (L) (LET ((FMAPLVL 2)) (APPLY #'FMAPL1 (MMAPEV L))))

(DEFMSPEC $FULLMAPL (L) (APPLY #'FMAPL1 (MMAPEV L)))

(DEFMFUN FMAPL1 N
 (LET ((HEADER '(MLIST)) ARGL)
      (SETQ ARGL (FMAP1 (ARG 1)
			(MAPCAR
			 #'(LAMBDA (Z)
			    (COND ((NOT (MXORLISTP Z))
				   (MERROR "Argument to FULLMAPL is not a list or matrix."))
				  ((EQ (CAAR Z) '$MATRIX)
				   (SETQ HEADER '($MATRIX))
				   (CONS '(MLIST SIMP) (CDR Z)))
				  (T Z)))
			 (CDR (LISTIFY N)))
			'MLIST))
      (IF (DOLIST (E (CDR ARGL)) (IF (NOT ($LISTP E)) (RETURN T)))
	  ARGL
	  (CONS HEADER (CDR ARGL)))))

(DEFMSPEC $OUTERMAP (L)
 (APPLY (IF (= (LENGTH L) 3) #'FMAPL1 #'OUTERMAP1) (MMAPEV L)))

(DEFMFUN OUTERMAP1 N 
 (LET (OUTARGS1 OUTARGS2)
      (COND ((MXORLISTP (ARG 2))
	     (SETQ OUTARGS1 (NCONS (ARG 1)) OUTARGS2 (LISTIFY (f- 2 N)))
	     (FMAPL1 'OUTERMAP2 (ARG 2)))
	    (T (DO ((I 3 (f1+ I)))
		   ((> I N) (FUNCER (ARG 1) (LISTIFY (f- 1 N))))
		   (WHEN (MXORLISTP (ARG I))
			 (SETQ OUTARGS1 (LISTIFY (f1- I))
			       OUTARGS2 (IF (< I N) (LISTIFY (f- I N))))
			 (RETURN (FMAPL1 'OUTERMAP2 (ARG I)))))))))

(DEFMFUN OUTERMAP2 N
  (IF (NOT (ZEROP N))
      (APPLY #'OUTERMAP1 (APPEND OUTARGS1 (LISTIFY 1) OUTARGS2))))

(DEFMFUN FUNCER (FN ARGS)
  (COND ((AND (NOT OPEXPRP) (MEMQ FN '(MPLUS MTIMES MEXPT MNCTIMES)))
	 (SIMPLIFY (CONS (NCONS FN) ARGS)))
	((OR (MEMQ FN '(OUTERMAP2 CONSTFUN))
	     (AND $TRANSRUN (SYMBOLP FN) (GET FN 'TRANSLATED)
		  (NOT (MGET FN 'LOCAL-FUN)) (FBOUNDP FN)))
	 (APPLY FN (MAPCAR #'SIMPLIFY ARGS)))
	(T (MAPPLY1 FN (MAPCAR #'SIMPLIFY ARGS) FN
		   nil ;; try to get more info to pass
		   ))))

(DEFMSPEC $QPUT (L) (SETQ L (CDR L))
 (IF (NOT (= (LENGTH L) 3)) (WNA-ERR '$QPUT))
 ($PUT (CAR L) (CADR L) (CADDR L)))

(DEFMFUN $GET (ATOM IND) (PROP1 '$GET ATOM NIL IND))

(DEFMFUN $REM (ATOM IND) (PROP1 '$REM ATOM NIL IND))

(DEFMFUN $PUT (ATOM VAL IND)
 (PROG1 (PROP1 '$PUT ATOM VAL IND) (ADD2LNC ATOM $PROPS)))

(DEFUN PROP1 (FUN ATOM VAL IND)
 (NONSYMCHK ATOM FUN) (NONSYMCHK IND FUN)
 (LET ((U (MGET ATOM '$PROPS)))
      (COND ((EQ FUN '$GET) (AND U (old-GET U IND)))
	    ((EQ FUN '$REM) (AND U (ZL-REMPROP U IND) '$DONE))
	    ((NOT U) (MPUTPROP ATOM (LIST NIL IND VAL) '$PROPS) VAL)
	    (T (PUTPROP U VAL IND)))))

(DEFMSPEC $DECLARE (L) (SETQ L (CDR L))
 (IF (ODDP (LENGTH L)) (MERROR "DECLARE takes an even number of arguments."))
 (DO ((L L (CDDR L)) (VARS) (FLAG NIL NIL)) ((NULL L) '$DONE)
     (COND (($LISTP (CADR L))
	    (DO ((L1 (CDADR L) (CDR L1))) ((IF (NULL L1) (SETQ FLAG T)))
		(MEVAL `(($DECLARE) ,(CAR L) ,(CAR L1)))))
	   ((NONSYMCHK (CADR L) '$DECLARE))
	   (T (SETQ VARS (DECLSETUP (CAR L) '$DECLARE))))
     (COND (FLAG)
	   ((MEMQ (CADR L) '($EVFUN $EVFLAG $SPECIAL $NONARRAY $BINDTEST))
	    (DECLARE1 VARS T (STRIPDOLLAR (CADR L)) NIL))
	   ((EQ (CADR L) '$NOUN)
	    (DOLIST (VAR VARS) (ALIAS (GETOPR VAR) ($NOUNIFY VAR))))
	   ((MEMQ (CADR L) '($CONSTANT $NONSCALAR $SCALAR $MAINVAR))
	    (DECLARE1 VARS T (CADR L) T))
	   ((MEMQ (CADR L) OPERS)
	    (IF (MEMQ (CADR L) (CDR $FEATURES)) (DECLARE1 VARS T (CADR L) 'KIND))
	    (DECLARE1 (MAPCAR #'GETOPR VARS) T (CADR L) 'OPERS))
	   ((MEMQ (CADR L) (CDR $FEATURES)) (DECLARE1 VARS T (CADR L) 'KIND))
	   ((EQ (CADR L) '$FEATURE)
	    (DOLIST (VAR VARS) (NONSYMCHK VAR '$DECLARE) (ADD2LNC VAR $FEATURES)))
	   ((EQ (CADR L) '$ALPHABETIC) (DECLARE1 VARS T T '$ALPHABETIC))
	   (T (MERROR "Unknown property to DECLARE: ~:M" (CADR L))))))

(DEFUN DECLARE1 (VARS VAL PROP MPROPP)
 (DOLIST (VAR VARS)
	 (SETQ VAR (GETOPR VAR))
	 (NONSYMCHK VAR '$DECLARE)
	 (COND ((EQ MPROPP 'KIND) (DECLAREKIND VAR PROP))
	       ((EQ MPROPP 'OPERS)
		(PUTPROP (SETQ VAR (LINCHK VAR)) T PROP) (PUTPROP VAR T 'OPERS)
		(IF (NOT (GET VAR 'OPERATORS)) (PUTPROP VAR 'SIMPARGS1 'OPERATORS)))
	       ((EQ MPROPP '$ALPHABETIC)
		(PUTPROP (SETQ VAL (STRIPDOLLAR VAR)) T 'ALPHABET)
		(ADD2LNC (GETCHARN VAL 1) ALPHABET))
	       ((EQ PROP 'SPECIAL)(proclaim (list 'special var))
		(FLUIDIZE VAR))
	       (MPROPP
		(IF (AND (MEMQ PROP '($SCALAR $NONSCALAR))
			 (MGET VAR (IF (EQ PROP '$SCALAR) '$NONSCALAR '$SCALAR)))
		    (MERROR "Inconsistent Declaration: ~:M"
			    `(($DECLARE) ,VAR ,PROP)))
		(MPUTPROP VAR VAL PROP))
	       (T (PUTPROP VAR VAL PROP)))
	 (IF (AND (GET VAR 'OP) (OPERATORP1 VAR)
		  (NOT (MEMQ (SETQ VAR (GET VAR 'OP)) (CDR $PROPS))))
	     (SETQ MOPL (CONS VAR MOPL)))
	 (ADD2LNC (GETOP VAR) $PROPS)))

(DEFUN LINCHK (VAR)
 (IF (MEMQ VAR '($SUM $INTEGRATE $LIMIT $DIFF $TRANSPOSE)) ($NOUNIFY VAR) VAR))

(DEFMSPEC $REMOVE (FORM) (I-$REMOVE (CDR FORM)))

(DEFMFUN I-$REMOVE (L)
 (IF (ODDP (LENGTH L)) (MERROR "REMOVE takes an even number of arguments."))
 (DO ((L L (CDDR L)) (VARS) (FLAG NIL NIL)) ((NULL L) '$DONE)
     (COND (($LISTP (CADR L))
	    (DO ((L1 (CDADR L) (CDR L1))) ((IF (NULL L1) (SETQ FLAG T)))
	 	(I-$REMOVE (LIST (CAR L) (CAR L1)))))
	   ((NONSYMCHK (CADR L) '$REMOVE))
	   (T (SETQ VARS (DECLSETUP (CAR L) '$REMOVE))))
     (COND (FLAG)
	   ((EQ (CADR L) '$VALUE) (I-$REMVALUE VARS))
	   ((EQ (CADR L) '$FUNCTION)
;;*** MERGE LOSSAGE ***
;;*** File R20:AUX:<ATP.SCHELTER.MACSYMA>MLISP.LISP.14 has:
	    (REMOVE1 (MAPCAR #'REM-VERBIFY VARS) 'MEXPR T $FUNCTIONS T))
	   ((EQ (CADR L) '$MACRO)
	    (REMOVE1 (MAPCAR #'REM-VERBIFY VARS) 'MMACRO T $MACROS T))
;;*** File R20:PS:<MACSYM.LSP-TEMP>MLISP.LSP.1 has:
	    ;;(REMOVE1 (MAPCAR #'$VERBIFY VARS) 'MEXPR T $FUNCTIONS T))
	   ;;((EQ (CADR L) '$MACRO)
	    ;;(REMOVE1 (MAPCAR #'$VERBIFY VARS) 'MMACRO T $MACROS T))
	   
	   ((EQ (CADR L) '$ARRAY) (MEVAL `(($REMARRAY) ,@VARS)))
	   ((MEMQ (CADR L) '($ALIAS $NOUN)) (REMALIAS1 VARS (EQ (CADR L) '$ALIAS)))
	   ((EQ (CADR L) '$MATCHDECLARE) (REMOVE1 VARS 'MATCHDECLARE T T NIL))
	   ((EQ (CADR L) '$RULE) (REMRULE VARS))
	   ((MEMQ (CADR L) '($EVFUN $EVFLAG $SPECIAL $NONARRAY $BINDTEST
			     $AUTOLOAD $ASSIGN))
	    (REMOVE1 VARS (STRIPDOLLAR (CADR L)) NIL T NIL))
	   ((MEMQ (CADR L) '($MODE $MODEDECLARE)) (REMOVE1 VARS 'MODE NIL 'FOO NIL))
	   ((EQ (CADR L) '$ATVALUE) (REMOVE1 VARS 'ATVALUES T T NIL))
	   ((MEMQ (CADR L) '($CONSTANT $NONSCALAR $SCALAR $MAINVAR $NUMER $ATOMGRAD))
	    (REMOVE1 VARS (CADR L) T T NIL))
	   ((MEMQ (CADR L) OPERS) (REMOVE1 (MAPCAR #'LINCHK VARS) (CADR L) NIL T NIL))
	   ((MEMQ (CADR L) (CDR $FEATURES)) (REMOVE1 VARS (CADR L) NIL T NIL))
	   ((EQ (CADR L) '$FEATURE) (DOLIST (VAR VARS) (DELQ VAR $FEATURES 1)))
	   ((MEMQ (CADR L) '($ALPHABETIC $TRANSFUN))
	    (REMOVE1 VARS (CADR L) NIL T NIL))
	   ((MEMQ (CADR L) '($GRADEF $GRAD)) (REMOVE1 VARS 'GRAD NIL $GRADEFS T))
	   ((MEMQ (CADR L) '($DEPENDENCY $DEPEND $DEPENDS))
	    (REMOVE1 VARS 'DEPENDS T $DEPENDENCIES T))
	   ((MEMQ (CADR L) '($OP $OPERATOR)) (REMOVE1 VARS '$OP NIL 'FOO NIL))
	   ((MEMQ (CADR L) '($DEFTAYLOR $TAYLORDEF)) (REMOVE1 VARS 'SP2 NIL T NIL))
	   (T (MERROR "Unknown property to REMOVE: ~:M" (CADR L))))))

(DEFUN DECLSETUP (X FN)
 (COND ((ATOM X) (NCONS X))
       ((EQ (CAAR X) '$NOUNIFY) (NCONS (MEVAL X)))
       ((EQ (CAAR X) 'MLIST)
	(MAPCAR #'(LAMBDA (VAR)
		   (COND ((ATOM VAR) VAR)
			 ((EQ (CAAR VAR) '$NOUNIFY) (MEVAL VAR))
			 (T (IMPROPER-ARG-ERR VAR FN))))
		(CDR X)))
       (T (IMPROPER-ARG-ERR X FN))))

(DEFMFUN REMOVE1 (VARS PROP MPROPP INFO FUNP)
 (DO ((VARS VARS (CDR VARS)) (ALLFLG)) ((NULL VARS))
     (NONSYMCHK (CAR VARS) '$REMOVE)
     (COND ((AND (EQ (CAR VARS) '$ALL) (NULL ALLFLG))
	    (SETQ VARS (APPEND VARS (COND ((ATOM INFO) (CDR $PROPS))
					  (FUNP (MAPCAR #'CAAR (CDR INFO)))
					  (T (CDR INFO))))
		  ALLFLG T))
	   (T
	    (let ((VAR  (GETOPR (CAR VARS)))( FLAG  NIL))
	    
	      (COND (MPROPP (MREMPROP VAR PROP)
			      (WHEN (MEMQ PROP '(MEXPR MMACRO))
				    (MREMPROP VAR 'MLEXPRP)
				    (MREMPROP VAR 'MFEXPRP)
				    (IF (NOT (GET VAR 'TRANSLATED))
					(ARGS VAR NIL))
				    (IF (MGET VAR 'TRACE)
					(MACSYMA-UNTRACE VAR))))
		      ((EQ PROP '$OP) (KILL-OPERATOR VAR))
		      ((EQ PROP '$ALPHABETIC)
		       (ZL-REMPROP (SETQ PROP (STRIPDOLLAR VAR)) 'ALPHABET)
		       (zl-DELETE (GETCHARN PROP 1) ALPHABET 1))
		      ((EQ PROP '$TRANSFUN)
		       (REMOVE-TRANSL-FUN-PROPS VAR)
		       (REMOVE-TRANSL-ARRAY-FUN-PROPS VAR))
		      ((OR (SETQ FLAG (MEMQ PROP (CDR $FEATURES))) (MEMQ PROP OPERS))
		       (IF FLAG (UNKIND VAR PROP))
		       (ZL-REMPROP VAR PROP)
		       (IF (NOT (GETL VAR (DELQ PROP (copy-top-level OPERS) 1)))
			   (ZL-REMPROP VAR 'OPERS)))
		      (T (ZL-REMPROP VAR PROP)))
		(COND ((EQ INFO T) (REMPROPCHK VAR))
		      ((EQ INFO 'FOO))
		      (FUNP ;(DELETE (ASSOC (NCONS VAR) INFO) INFO 1)
			    (mfunction-delete var info))
		      (T (DELQ VAR INFO 1))))
	        ))))

(DEFUN REMOVE-TRANSL-FUN-PROPS (FUN)
 (IF (MGET FUN 'TRACE) (MACSYMA-UNTRACE FUN))
 (WHEN (AND (GET FUN 'TRANSLATED) (NOT (EQ $SAVEDEF '$ALL)))
       #+Maclisp
       (DO ((PROPS '(EXPR SUBR LSUBR FEXPR FSUBR) (CDR PROPS)))
	   ((NULL PROPS))
	   (ZL-REMPROP FUN (CAR PROPS)))
       #-Maclisp
       (FMAKUNBOUND FUN)
       (ZL-REMPROP FUN 'TRANSLATED-MMACRO)
       (MREMPROP FUN 'T-MFEXPR)
       (ZL-REMPROP FUN 'FUNCTION-MODE)
       #-(or CL NIL)
       (IF (NOT (MGETL FUN '(MEXPR MMACRO))) (ARGS FUN NIL))
       (IF (NOT (GETL FUN '(A-EXPR A-SUBR))) (ZL-REMPROP FUN 'TRANSLATED))))

(DEFUN REMOVE-TRANSL-ARRAY-FUN-PROPS (FUN)
 (WHEN (AND (GET FUN 'TRANSLATED) (NOT (EQ $SAVEDEF '$ALL)))
       (ZL-REMPROP FUN 'A-EXPR)
       (ZL-REMPROP FUN 'A-SUBR)
       (IF (NOT (FBOUNDP FUN)) (ZL-REMPROP FUN 'TRANSLATED))))

(DEFMFUN REMPROPCHK (VAR)
 (IF (AND (NOT (MGETL VAR '($CONSTANT $NONSCALAR $SCALAR $MAINVAR $NUMER
			   MATCHDECLARE $ATOMGRAD ATVALUES T-MFEXPR)))
	  (NOT (GETL VAR '(EVFUN EVFLAG TRANSLATED NONARRAY BINDTEST
			   OPR SP2 OPERATORS OPERS SPECIAL DATA
			   ALPHABET AUTOLOAD MODE))))
     (DELQ VAR $PROPS 1)))

(DEFUN REM-VERBIFY (FNNAME) (NONSYMCHK FNNAME '$REMOVE) ($VERBIFY FNNAME))



(DEFMSPEC $REMFUNCTION (L) (SETQ L (CDR L))
  (COND ((MEMQ '$ALL L)
	 (SETQ L (NCONC (MAPCAR #'CAAR (CDR $FUNCTIONS))
		        (MAPCAR #'CAAR (CDR $MACROS)))))
	(T (SETQ L (MAPCAR #'REM-VERBIFY L))
	   (DO ((L1 L (CDR L1))) ((NULL L1) T)
	     (IF (NOT (OR (zl-ASSOC (NCONS (CAR L1)) (CDR $FUNCTIONS))
			  (zl-ASSOC (NCONS (CAR L1)) (CDR $MACROS))))
		 (RPLACA L1 NIL)))))
  (REMOVE1 L 'MEXPR T $FUNCTIONS T)
  (REMOVE1 L 'MMACRO T $MACROS T)
  (CONS '(MLIST) L))

;; (SETQ L (MAPCAR #'$VERBIFY L))
; (DO L1 L (CDR L1) (NULL L1)
;     (COND ((EQ (CAR L1) '$ALL)
;	      (LET ((ZZ (DELQ '$ALL L1)))
;		(RPLACA (RPLACD L1 (CDR ZZ)) (CAR ZZ)))
;	      (NCONC L (MAPCAR #'CAAR (CDR $FUNCTIONS))
;		     (MAPCAR #'CAAR (CDR $MACROS))))
;	   ((NOT (OR (ASSOC (NCONS (CAR L1)) (CDR $FUNCTIONS))
;		     (ASSOC (NCONS (CAR L1)) (CDR $MACROS))))
;	    (RPLACA L1 NIL))))
; (REMOVE1 L 'MEXPR T $FUNCTIONS T)
; (REMOVE1 L 'MMACRO T $MACROS T)
; (CONS '(MLIST) L))

(DEFMSPEC $REMARRAY (L) (SETQ L (CDR L))
 (CONS '(MLIST)
       (DO ((L L (CDR L)) (X) (PRED)) ((NULL L) (NREVERSE X))
	   (COND ((EQ (CAR L) '$ALL) (SETQ L (APPEND L (CDR $ARRAYS))))
		 (T (REMCOMPARY (CAR L)) (SETQ PRED (MREMPROP (CAR L) 'array))
		    (SETQ PRED (OR (MREMPROP (CAR L) 'HASHAR) PRED))
		    (SETQ PRED (OR (MREMPROP (CAR L) 'AEXPR) PRED))
		    (SETQ X (CONS (AND PRED (PROG2 (DELQ (CAR L) $ARRAYS 1) (CAR L)))
				  X)))))))

(DEFUN REMCOMPARY (X)
 (COND ((EQ X (MGET X 'array)) (ZL-REMPROP X 'ARRAY-MODE) (ZL-REMPROP X 'array))))

(DEFMSPEC $REMVALUE (FORM) (I-$REMVALUE (CDR FORM)))

(DEFMFUN I-$REMVALUE (L)
 (CONS '(MLIST)
       (DO ((L L (CDR L)) (X) (Y)) ((NULL L) (NREVERSE X))
	   (COND ((EQ (CAR L) '$ALL) (SETQ L (APPEND L (CDR $VALUES))))
		 (T (SETQ X (CONS (COND ((ATOM (CAR L))
					 (IF (REMVALUE (CAR L) '$REMVALUE) (CAR L)))
					((SETQ Y (MGETL (CAAAR L) '(HASHAR ARRAY)))
					 (REMARRELEM Y (CAR L)) (CAR L)))
			    X)))))))

(DEFMFUN REMARRELEM (ARY FORM)
  (IF (MFILEP (CADR ARY)) (I-$UNSTORE (NCONS (CAAR FORM))))
  #-nil
  (LET ((Y (CAR (ARRAYDIMS (CADR ARY)))))
    (ARRSTORE FORM (COND ((EQ Y 'fixnum) 0) ((EQ Y 'flonum) 0.0) (T MUNBOUND))))
  #+nil
 (LET ((Y (ARRAY-TYPE (CADR ARY))))
   (ARRSTORE FORM (OR (CDR (ASSQ Y '((FIXNUM . 0)
				     (FLONUM . 0.0)
				#+NIL(SINGLE-FLOAT . 0.0F0)
				#+NIL(SHORT-FLOAT . 0.0S0)
				#+NIL(DOUBLE-FLOAT . 0.0D0)
				#+NIL(LONG-FLOAT . 0.0L0)
				)))
		      MUNBOUND)))

  )

(DEFMFUN REMRULE (L)
  (DO ((L L (CDR L)) (U)) ((NULL L))
    (COND ((EQ (CAR L) '$ALL) (SETQ L (APPEND L (CDR $RULES))))
	  ((GET (CAR L) 'OPERATORS) ($REMRULE (CAR L) '$ALL))
	  ((SETQ U (RULEOF (CAR L))) ($REMRULE U (CAR L)))
	  ((MGET (CAR L) '$RULE)
	   (ZL-REMPROP (CAR L) 'EXPR) (MREMPROP (CAR L) '$RULE)
	   (DELQ (CAR L) $RULES 1)))))

(DEFMFUN REMALIAS1 (L ALIASP)
  (DO ((L L (CDR L)) (U)) ((NULL L))
    (COND ((EQ (CAR L) '$ALL) (SETQ L (APPEND L (CDR $ALIASES))))
	  ((OR ALIASP (GET (CAR L) 'NOUN)) (REMALIAS (CAR L) T))
	  ((SETQ U (GET (CAR L) 'VERB))
	   (ZL-REMPROP (CAR L) 'VERB) (ZL-REMPROP U 'NOUN)))))

;in maxmac
;(DEFMFUN MGET (ATOM IND)
;  (LET ((PROPS (AND (SYMBOLP ATOM) (GET ATOM 'MPROPS))))
;;    (AND PROPS (GET PROPS IND)))
;    (AND PROPS (GETf (cdr PROPS) IND))))
 


#-cl
(DEFUN MDEFPROP FEXPR (L) (MPUTPROP (CAR L) (CADR L) (CADDR L)) (CAR L))





(DEFMFUN MREMPROP (ATOM IND)
  (LET ((PROPS (GET ATOM 'MPROPS))) (AND PROPS (ZL-REMPROP PROPS IND))))

(DEFMFUN MGETL (ATOM INDS)
  (LET ((PROPS (GET ATOM 'MPROPS))) (AND PROPS (GETL PROPS INDS))))

(DEFMFUN $MATRIX N
  (IF (= N 0)
      (NCONS '($MATRIX))
      (LET ((L (LISTIFY N)))
	(DOLIST (ROW L)
	  (IF (NOT ($LISTP ROW)) (MERROR "Invalid matrix row:~%~M" ROW)))
	(MATCHECK L)
	(CONS '($MATRIX) L))))

(DEFMFUN MATCHECK (L)
  (DO ((L1 (CDR L) (CDR L1)) (N (LENGTH (CAR L)))) ((NULL L1))
    (IF (NOT (= N (LENGTH (CAR L1))))
	(MERROR "All matrix rows are not of the same length."))))

(DEFUN HARRFIND (FORM)
       (PROG (ARY Y LISPSUB ITEML SUB NCELLS NITEMS)
	     (SETQ ARY (symbol-array (MGET (CAAR FORM) 'HASHAR)))
	     (COND ((NOT (= (aref ARY 2) (LENGTH (CDR FORM))))
		    (MERROR "Array ~:M already has dimension ~:M~%~M"
			    (CAAR FORM) (AREF ARY 2) FORM)))
	     (SETQ SUB (CDR FORM))
	     (SETQ ITEML (AREF ARY
				  (SETQ LISPSUB
					(f+ 3 (fixnum-remainder
					       (HASHER SUB) (AREF ARY 0))))))
	A    (COND ((NULL ITEML) (GO B))
		   ((ALIKE (CAAR ITEML) SUB) (RETURN (CDAR ITEML))))
	     (SETQ ITEML (CDR ITEML))
	     (GO A)
	B    (COND (EVARRP (THROW 'EVARRP 'NOTEXIST))
		   ((NULL (SETQ Y (ARRFUNP (CAAR FORM)))) (RETURN (MEVAL2 SUB FORM))))
	     (SETQ Y (ARRFUNCALL Y SUB form))
	     (SETQ ARY (symbol-array (MGET (CAAR FORM) 'HASHAR)))
	     (SETQ ITEML (aref ARY (SETQ LISPSUB (f+ 3 (fixnum-remainder (HASHER SUB) (aref ARY 0))))))
	     (SETQ SUB (NCONS (CONS SUB Y)))
	     (COND (ITEML (NCONC ITEML SUB)) (T (STORE (aref ARY LISPSUB) SUB)))
	     (STORE (AREF ARY 1) (SETQ NITEMS (f1+ (AREF ARY 1))))
	     (COND ((> NITEMS (SETQ NCELLS (AREF ARY 0)))
		    (ARRAYSIZE (CAAR FORM) (f+ NCELLS NCELLS))))
	     (RETURN Y)))

; Types of FIXNUM and FLONUM herein not currently compatible 
; on LISP machine.  Don't worry about it for now.
(DEFUN ARRFIND (FORM)
  (let ((sub (cdr form)) u v type)
    (SETQ V (DIMCHECK (CAAR FORM) SUB NIL))
    (COND (V (SETQ TYPE (CAR (ARRAYDIMS (MGET (CAAR FORM) 'array))))))
    (COND ((AND V (PROG2 #-cl
			 (SETQ U (APPLY (MGET (CAAR FORM) 'array) SUB))
			 #+cl
			 (setq u (apply 'aref (symbol-array
					       (MGET (CAAR FORM) 'array))
					sub))
			 (COND ((EQ TYPE 'flonum) (NOT (= U FLOUNBOUND)))
			       ((EQ TYPE 'fixnum) (NOT (= U FIXUNBOUND)))
			       (T (NOT (EQ U MUNBOUND))))))
	   U)
	  (EVARRP (THROW 'EVARRP 'NOTEXIST))
	  ((OR (NOT V) (NULL (SETQ U (ARRFUNP (CAAR FORM)))))
	   (COND ((EQ TYPE 'flonum) 0.0)
		 ((EQ TYPE 'fixnum) 0)
		 (T (MEVAL2 SUB FORM))))
	  (T (SETQ U (ARRFUNCALL U SUB form))
	     #-cl(STORE (APPLY (MGET (CAAR FORM) 'array) SUB) U)
	     #+cl
	     (setf (apply #'aref (SYMBOL-ARRAY (MGET (CAAR FORM) 'array))
				sub) u)
	     
	     U))))


 
;#+cl
;(defmacro $array (ar typ &rest dims)
;   (setq ar (make-array dims :initial-element init))
;   (cond ((


(DEFMSPEC $ARRAY (X) (SETQ X (CDR X))
 (COND #+cl
       ($use_fast_arrays
	  
	  (mset (car x) (apply '$make_array '$any
			       (mapcar #'1+ (cdr x)))))
       ((SYMBOLP (CAR X))
	(funcall #'(LAMBDA (COMPP)
	  (funcall #'(LAMBDA (FUN DIML FUNP OLD NEW NCELLS)
	    (COND ((MEMQ '$FUNCTION DIML)
		   (SETQ DIML (DELQ '$FUNCTION (copy-top-level DIML) 1) FUNP T)))
	    (SETQ DIML (MAPCAR #'MEVAL DIML))
	    (COND ((NULL DIML) (WNA-ERR '$ARRAY))
		  ((> (LENGTH DIML) 5) (MERROR "ARRAY takes at most 5 indices"))
		  ((MEMQ NIL (MAPCAR #'(LAMBDA (U) (EQ (ml-typep U) 'fixnum)) DIML))
		   (MERROR "Non-integer dimension - ARRAY")))
	    (SETQ DIML (MAPCAR #'1+ DIML))
	    (SETQ NEW (APPLY #'*ARRAY (CONS (IF COMPP FUN (GENSYM))
					    (CONS #-CL (OR COMPP T)
						  #+CL T
						  DIML))))
	    #+cl
	    (COND ((EQ COMPP 'fixnum) (FILLARRAY NEW '(0)))
		  ((EQ COMPP 'flonum) (FILLARRAY NEW '(0.0))))
	    (COND ((NOT (MEMQ COMPP '(FIXNUM FLONUM))) (FILLARRAY NEW (LIST MUNBOUND)))
		  ((OR FUNP (ARRFUNP FUN))
		   (FILLARRAY NEW (LIST (COND ((EQ COMPP 'fixnum) FIXUNBOUND)
					      (T FLOUNBOUND))))))
	    (COND ((NULL (SETQ OLD (MGET FUN 'HASHAR)))
		   (MPUTPROP FUN NEW 'array))
		  (T (COND ((NOT (= (AFUNCALL OLD 2) (LENGTH DIML)))
			    (MERROR "Array ~:M already has ~:M dimension(s)"
				    FUN (AFUNCALL OLD 2))))
		     (SETQ NCELLS (f+ 2 (AFUNCALL OLD 0)))
		     (DO ((N 3 (f1+ N))) ((> N NCELLS))
			 (DO ((ITEMS (AFUNCALL OLD N) (CDR ITEMS))) ((NULL ITEMS))
			     (DO ((X (CAAR ITEMS) (CDR X)) (Y DIML (CDR Y)))
				 ((NULL X)
				  (IF (AND (MEMQ COMPP '(FIXNUM FLONUM))
					   (NOT (EQ (ml-typep (CDAR ITEMS)) COMPP)))
				      (MERROR "Element and array type do not match:~%~M"
					      (CDAR ITEMS)))
				  #-cl(EVAL (LIST 'STORE
						    (CONS NEW (CAAR ITEMS))
						    (LIST 'QUOTE (CDAR ITEMS))))
				  #+cl
				  (setf (APPLY #'Aref
					       (SYMBOL-ARRAY NEW)
					       (CAAR ITEMS))
					(CDAR ITEMS)))
				 (IF (OR (NOT (EQ (ml-typep (CAR X)) 'fixnum))
					 (< (CAR X) 0)
					 (NOT (< (CAR X) (CAR Y))))
				     (MERROR "Improper index for declared array:~%~M"
					     (CAR X))))))
		     (MREMPROP FUN 'HASHAR)
		     (MPUTPROP FUN NEW 'array)))
             (ADD2LNC FUN $ARRAYS)
	     (IF (EQ COMPP 'fixnum) (PUTPROP FUN '$FIXNUM 'ARRAY-MODE))
	     (IF (EQ COMPP 'flonum) (PUTPROP FUN '$FLOAT 'ARRAY-MODE))
	     FUN)
	   ($VERBIFY (CAR X)) (COND (COMPP (SETQ COMPP (CDR COMPP)) (CDDR X)) (T (CDR X)))
	   NIL NIL NIL 0))
	 (ASSQ (CADR X) '(($COMPLETE . T) ($INTEGER . FIXNUM) ($FIXNUM . FIXNUM)
			  ($FLOAT . FLONUM) ($FLONUM . FLONUM)))))
       (($LISTP (CAR X))
	(DO ((U (CDAR X) (CDR U))) ((NULL U)) (MEVAL `(($ARRAY) ,(CAR U) ,@(CDR X))))
	(CAR X))
       (T (MERROR "Improper first argument to ARRAY:~%~M" (CAR X)))))




#+cl
(defmfun $Show_hash_array (x)
  (send x :map-hash
   `(lambda (u v) 
    (format t "~%~A-->~A" u v))))
  
#+cl
;; If this is T then arrays are stored in the value cell,
;; whereas if it is false they are stored in the function cell
(defmvar $use_fast_arrays nil)
#+cl
(DEFMFUN ARRSTORE (L R &aux tem index)
  (cond ($use_fast_arrays
	 (cond ((and (boundp (caar l)) (setq tem (symbol-value (caar l))))
		(setq index (mevalargs (cdr l)))
		(LET ((THE-TYPE (ml-typep TEM)))
 		  (COND ((EQ THE-TYPE 'array)
			 (setf (APPLY #'Aref TEM INDEX)  R))
			((EQ THE-TYPE #+cl 'hash-table
			              #-cl'SI:EQUAL-HASH-TABLE)
			 (cond ((gethash 'dim1 tem)
				(if (cdr index)
				    (error "Array has dimension 1")))
			       (t (or (cdr index)
				      (error "Array has dimension > 1"))))
			 (setf (gethash
				 (if (cdr index) index
				   (car index))
				 tem) r))
			((EQ THE-TYPE  'list)
			 (COND ((EQ (CAAR TEM) 'MLIST)
				(SETQ INDEX (CAR INDEX))
				(SETF (NTH INDEX TEM) R)
				r)
			       ((eq (caar tem) '$matrix)
				(setf (nth (second index) (nth (first index) tem)) r)
				r)
   			       (T
				(ERROR "The value of ~A is not a hash-table ,an ~
                                           array, macsyma list, or a matrix"
					(CAAR L)))))
			(T(cond ((eq tem (caar l))
				 (meval* `((mset) ,(caar l)
					   ,(make-equal-hash-table
					      (cdr (mevalargs (cdr l))))))
				   (arrstore l r))
				(t
				 (error "The value of ~A is not a hash-table , an array,a macsyma list, or a matrix" (caar l))))
			  ))))
	       (t
		(cond ((mget (caar l) 'hashar)
		       (let ($use_fast_arrays)
			 (arrstore l r)))
		      (t
		       (meval* `((mset) ,(caar l)
				 ,(make-equal-hash-table
				    (cdr (mevalargs (cdr l))))))
		       (arrstore l r))))))
	(t
	 (LET ((FUN ($VERBIFY (CAAR L))) ARY SUB (LISPSUB 0) HASHL MQAPPLYP)
	   (COND ((SETQ ARY (MGET FUN 'array))
		  (WHEN (MFILEP ARY)
		    (I-$UNSTORE (NCONS FUN)) (SETQ ARY (MGET FUN 'array)))
		  (DIMCHECK FUN (SETQ SUB (MAPCAR #'MEVAL (CDR L))) T)
		  (IF (AND (MEMQ (SETQ FUN (CAR (ARRAYDIMS ARY))) '(FIXNUM FLONUM))
			   (NOT (EQ (ml-typep R) FUN)))
		      (MERROR "Improper assignment to complete array:~%~M" R))
;		  #-cl(EVAL (LIST 'STORE (CONS ARY SUB) (LIST 'QUOTE R)))
		  #+cl(setf (APPLY #'Aref (SYMBOL-ARRAY ARY) SUB)  R)
		  )
		 ((SETQ ARY (MGET FUN 'HASHAR))
		  (WHEN (MFILEP ARY)
		    (I-$UNSTORE (NCONS FUN)) (SETQ ARY (MGET FUN 'HASHAR)))
		  (IF (NOT (= (AFUNCALL ARY 2) (LENGTH (CDR L))))
		      (MERROR "Array ~:M has dimension ~:M; it was called by ~:M"
			      FUN (AFUNCALL ARY 2) L))
		  (SETQ SUB (MAPCAR #'MEVAL (CDR L)))
		  (SETQ HASHL (AFUNCALL ARY (SETQ LISPSUB
						 (f+ 3 (fixnum-remainder (HASHER SUB) (AFUNCALL ARY 0))))))	  (DO ((HASHL1 HASHL (CDR HASHL1)))
	      ((NULL HASHL1)
	       (COND ((NOT (EQ R MUNBOUND))
		      (SETQ SUB (NCONS (CONS SUB R)))
		      (COND ((NULL HASHL) (STORE (AFUNCALL ARY LISPSUB) SUB))
			    (T (NCONC HASHL SUB)))
		      (STORE (AFUNCALL ARY 1) (f1+ (AFUNCALL ARY 1))))))
	    (COND ((ALIKE (CAAR HASHL1) SUB)
		   (COND ((EQ R MUNBOUND) (STORE (AFUNCALL ARY 1)
						 (f1- (AFUNCALL ARY 1))))
			 (T (NCONC HASHL (NCONS (CONS SUB R)))))
		   (STORE (AFUNCALL ARY LISPSUB) (zl-DELETE (CAR HASHL1) HASHL 1))
		   (RETURN NIL))))
		  (IF (> (AFUNCALL ARY 1) (AFUNCALL ARY 0))
		      (ARRAYSIZE FUN (f* 2 (AFUNCALL ARY 0))))
		  R)
		 ((AND (EQ FUN 'MQAPPLY) (MXORLISTP (SETQ ARY (MEVAL (CADR L))))
		       (PROG2 (SETQ MQAPPLYP T L (CDR L)) NIL)))
		 
		 ((AND (NOT MQAPPLYP)
		       (OR (NOT (BOUNDP FUN)) (NOT (OR (MXORLISTP (SETQ ARY (SYMBOL-VALUE FUN)))
						       (EQ (ml-typep ARY) 'array)))))
		  (IF (MEMQ FUN '(MQAPPLY $%)) (MERROR "Illegal use of :"))
		  (ADD2LNC FUN $ARRAYS)
		  (MPUTPROP FUN (SETQ ARY (GENSYM)) 'HASHAR)
		  (*ARRAY ARY T 7) (STORE (AFUNCALL ARY 0) 4) (STORE (AFUNCALL ARY 1) 0)
		  (STORE (AFUNCALL ARY 2) (LENGTH (CDR L)))
		  (ARRSTORE L R))
		 
		 ((EQ (ml-typep ARY) 'array)
		  (ARRSTORE-EXTEND ARY (MEVALARGS (CDR L)) R))
		 ((OR (EQ (CAAR ARY) 'MLIST) (= (LENGTH L) 2))
		  (COND ((EQ (CAAR ARY) '$MATRIX)
			 (COND ((OR (NOT ($LISTP R)) (NOT (= (LENGTH (CADR ARY)) (LENGTH R))))
				(MERROR "Attempt to assign bad matrix row:~%~M" R))))
			((NOT (= (LENGTH L) 2))
			 (MERROR "Wrong number of indices:~%~M" (CONS '(MLIST) (CDR L)))))
		  (LET ((INDEX (MEVAL (CADR L))))
		    (COND ((NOT (EQ (ml-typep INDEX) 'fixnum))
			   (MERROR "Index not an integer:~%~M" INDEX))
			  ((AND (> INDEX 0) (< INDEX (LENGTH ARY)))
			   (RPLACA (NCDR (CDR ARY) INDEX) R))
			  (T (MERROR "~A - index out of range" INDEX))))
		  R)
		 (T (IF (NOT (= (LENGTH L) 3))
			(MERROR "Wrong number of indices:~%~M" (CONS '(MLIST) (CDR L))))
		    ($SETELMX R (MEVAL (CADR L)) (MEVAL (CADDR L)) ARY)
		    R))))))
		   

#-cl
(DEFMFUN ARRSTORE (L R)
 (LET ((FUN ($VERBIFY (CAAR L))) ARY SUB (LISPSUB 0) HASHL MQAPPLYP)
   (COND ((SETQ ARY (MGET FUN 'array))
	  (WHEN (MFILEP ARY)
		(I-$UNSTORE (NCONS FUN)) (SETQ ARY (MGET FUN 'array)))
	  (DIMCHECK FUN (SETQ SUB (MAPCAR #'MEVAL (CDR L))) T)
	  (IF (AND (MEMQ (SETQ FUN (CAR (ARRAYDIMS ARY))) '(FIXNUM FLONUM))
		   (NOT (EQ (ml-typep R) FUN)))
	      (MERROR "Improper assignment to complete array:~%~M" R))
	  #-lispm(EVAL (LIST 'STORE (CONS ARY SUB) (LIST 'QUOTE R)))
	  )
	 ((SETQ ARY (MGET FUN 'HASHAR))
	  (WHEN (MFILEP ARY)
		(I-$UNSTORE (NCONS FUN)) (SETQ ARY (MGET FUN 'HASHAR)))
	  (IF (NOT (= (AFUNCALL ARY 2) (LENGTH (CDR L))))
	      (MERROR "Array ~:M has dimension ~:M; it was called by ~:M"
		      FUN (AFUNCALL ARY 2) L))
	  (SETQ SUB (MAPCAR #'MEVAL (CDR L)))
	  (SETQ HASHL (AFUNCALL ARY (SETQ LISPSUB (f+ 3 (fixnum-remainder (HASHER SUB) (AFUNCALL ARY 0))))))	  (DO ((HASHL1 HASHL (CDR HASHL1)))
	      ((NULL HASHL1)
	       (COND ((NOT (EQ R MUNBOUND))
		      (SETQ SUB (NCONS (CONS SUB R)))
		      (COND ((NULL HASHL) (STORE (AFUNCALL ARY LISPSUB) SUB))
			    (T (NCONC HASHL SUB)))
		      (STORE (AFUNCALL ARY 1) (f1+ (AFUNCALL ARY 1))))))
	      (COND ((ALIKE (CAAR HASHL1) SUB)
		     (COND ((EQ R MUNBOUND) (STORE (AFUNCALL ARY 1)
						   (f1- (AFUNCALL ARY 1))))
			   (T (NCONC HASHL (NCONS (CONS SUB R)))))
		     (STORE (AFUNCALL ARY LISPSUB) (zl-DELETE (CAR HASHL1) HASHL 1))
		     (RETURN NIL))))
	  (IF (> (AFUNCALL ARY 1) (AFUNCALL ARY 0))
	      (ARRAYSIZE FUN (f* 2 (AFUNCALL ARY 0))))
	  R)
	 ((AND (EQ FUN 'MQAPPLY) (MXORLISTP (SETQ ARY (MEVAL (CADR L))))
	       (PROG2 (SETQ MQAPPLYP T L (CDR L)) NIL)))
	 
	 ((AND (NOT MQAPPLYP)
	       (OR (NOT (BOUNDP FUN)) (NOT (OR (MXORLISTP (SETQ ARY (SYMBOL-VALUE FUN)))
					       (EQ (ml-typep ARY) 'array)))))
	  (IF (MEMQ FUN '(MQAPPLY $%)) (MERROR "Illegal use of :"))
	  (ADD2LNC FUN $ARRAYS)
	  (MPUTPROP FUN (SETQ ARY (GENSYM)) 'HASHAR)
	  (*ARRAY ARY T 7) (STORE (AFUNCALL ARY 0) 4) (STORE (AFUNCALL ARY 1) 0)
	  (STORE (AFUNCALL ARY 2) (LENGTH (CDR L)))
	  (ARRSTORE L R))
	 
	 ((EQ (ml-typep ARY) 'array)
	  (ARRSTORE-EXTEND ARY (MEVALARGS (CDR L)) R))
	 ((OR (EQ (CAAR ARY) 'MLIST) (= (LENGTH L) 2))
	  (COND ((EQ (CAAR ARY) '$MATRIX)
		 (COND ((OR (NOT ($LISTP R)) (NOT (= (LENGTH (CADR ARY)) (LENGTH R))))
			(MERROR "Attempt to assign bad matrix row:~%~M" R))))
		((NOT (= (LENGTH L) 2))
		 (MERROR "Wrong number of indices:~%~M" (CONS '(MLIST) (CDR L)))))
	  (LET ((INDEX (MEVAL (CADR L))))
	    (COND ((NOT (EQ (ml-typep INDEX) 'fixnum))
		   (MERROR "Index not an integer:~%~M" INDEX))
		  ((AND (> INDEX 0) (< INDEX (LENGTH ARY)))
		   (RPLACA (NCDR (CDR ARY) INDEX) R))
		  (T (MERROR "~A - index out of range" INDEX))))
	  R)
	 (T (IF (NOT (= (LENGTH L) 3))
		(MERROR "Wrong number of indices:~%~M" (CONS '(MLIST) (CDR L))))
	    ($SETELMX R (MEVAL (CADR L)) (MEVAL (CADDR L)) ARY)
	    R)))) 

(DEFUN ARRFUNP (X)
 (OR (AND $TRANSRUN (GETL X '(A-EXPR #+Maclisp A-SUBR))) (MGETL X '(AEXPR))))

#-cl
(defmacro system-subrcall* (p argl) p argl
  (cond ((status feature maclisp)
	 `(subrcall* ,p ,argl))
	(t
	 `(MAXIMA-ERROR '|Don't think I can A-SUBR frobulate here!|))))
#+lispm
(defmacro system-subrcall* (p argl) p argl
  (cond
    #-cl
    ((status feature maclisp)
	 `(subrcall* ,p ,argl))
       (t
	 `(error "Don't think I can A-SUBR frobulate here!"))))
#-(or cl NIL)
(defmacro assemble-subrcall* ()
  (cond ((status feature maclisp)
	 (cond ((status feature pdp10)
		'(PROGN 'COMPILE
			(SETPLIST '|the subr| '(SUBR NIL))
			(lap-a-list
			 '((LAP SUBRCALL* SUBR) 
			   (ARGS SUBRCALL* (()  . 2)) 
			   (HRRZ 3 '|the subr|)
			   (HRRZ 4 0 3) 
			   (HRLM 1 0 4) 
			   (MOVEI 1 '|the subr|) 
			   (JCALL 2 '*APPLY) 
			   ()  ))))
	       (t
		;; the above optimizes out the JSP PDLNMK
		;; which is not needed since we know the first argument
		;; is NOT a number. We are more interested in
		;; illustrating the issue than in bumming out
		;; a couple instructions, however there it is.
		'(progn 'compile
			(setplist '|the subr| '(SUBR NIL))
			(defun subrcall* (p argl)
			  (rplaca (cdr (symbol-plist '|the subr|)) p)
			  (apply #'|the subr| argl))))))
	(t nil)))
#-(or cl NIL)
(assemble-subrcall*)
							
(DEFUN ARRFUNCALL (ARRFUN SUBS form)
  (LET ((AEXPRP T))
    (CASE (CAR ARRFUN)
      (AEXPR (MAPPLY1 (CADR ARRFUN) SUBS (CADR ARRFUN) form))
      (A-EXPR (APPLY (CADR ARRFUN) SUBS))
      (A-SUBR 
       (COMMENT "This is what the code used to look like:"
		(EVAL (NCONC (LIST 'SUBRCALL NIL
				   (LIST 'QUOTE (CADR ARRFUN))) SUBS)))
       (SYSTEM-SUBRCALL* (CADR ARRFUN) SUBS)))))

(DEFUN HASHER (L)  ; This is not the best way to write a hasher.  But, 
 (IF (NULL L)	   ; please don't change this code or you're liable to 
     0		   ; break SAVE files.
     (LOGAND #o77777
	     (LET ((X (CAR L)))
		  (COND (($RATP X) (MERROR "Subscripts may not be in CRE form."))
			(#+NIL (ml-typep X '(OR FIXNUM DOUBLE-FLOAT))
			 #-NIL (OR (FIXNUMP X) (FLOATP X))
			 (f+ (IF (FIXNUMP X) X (FIX (+$ X 0.0005)))
			    (f* 7 (HASHER (CDR L)))))
			((ATOM X) (f+ (SXHASH X) (HASHER (CDR L))))
			(T (f+ 1 (SXHASH (CAAR X)) (HASHER (CDR X))
			      (HASHER (CDR L)))))))))

(DEFUN ARRAYSIZE (FUN N)
       (PROG (OLD NEW INDX NCELLS CELL ITEM I Y)
	     (SETQ OLD (symbol-array (MGET FUN 'HASHAR)))
	     (MPUTPROP FUN (SETQ NEW (GENSYM)) 'HASHAR)
	     (*ARRAY NEW T (f+ N 3))
	     (setq new (symbol-array new))
	     (STORE (AREF NEW 0) N)
	     (STORE (AREF NEW 1) (AREF OLD 1))
	     (STORE (AREF NEW 2) (AREF OLD 2))
	     (SETQ INDX 2 NCELLS (f+ 2 (AREF OLD 0)))
	A    (IF (> (SETQ INDX (f1+ INDX)) NCELLS) (RETURN T))
	     (SETQ CELL (AREF OLD INDX))
	B    (IF (NULL CELL) (GO A))
	     (SETQ I (f+ 3 (fixnum-remainder (HASHER (CAR (SETQ ITEM (CAR CELL)))) N)))
	     (IF (SETQ Y (AREF NEW I))
		 (NCONC Y (NCONS ITEM))
		 (STORE (AREF NEW I) (NCONS ITEM)))
	     (SETQ CELL (CDR CELL))
	     (GO B)))

(DEFUN DIMCHECK (ARY SUB FIXPP)
 (DO ((X SUB (CDR X)) (RET T) (Y (CDR (ARRAYDIMS (MGET ARY 'array))) (CDR Y)))
     ((NULL Y)
      (IF X (MERROR "Array ~:M has dimensions ~:M, but was called with ~:M"
		    ARY `((MLIST)
			  ,.(MAPCAR #'1-
				    (CDR (ARRAYDIMS (MGET ARY 'array)))))
			  `((MLIST) ,.SUB))
	    RET))
     (COND ((OR (NULL X) (AND (EQ (ml-typep (CAR X)) 'fixnum)
			      (OR (< (CAR X) 0) (NOT (< (CAR X) (CAR Y))))))
	    (SETQ Y NIL X (CONS NIL T)))
	   ((NOT (fixnump (car x)) )
	    (IF FIXPP (SETQ Y NIL X (CONS NIL T)) (SETQ RET NIL))))))

(DEFUN CONSTLAM (x &aux (lam x))
 (IF AEXPRP
     `(,(CAR LAM) ,(CADR LAM) ,@(MBINDING ((MPARAMS (CADR LAM)))
					  (MAPCAR #'MEVAL (CDDR LAM))))

     LAM))

(DEFMSPEC $DEFINE (L)
  (TWOARGCHECK L)
  (SETQ L (CDR L))
  (MEVAL `((MDEFINE)
	   ,(COND ((MQUOTEP (CAR L)) (CADAR L))
		  ((AND (NOT (ATOM (CAR L)))
			(MEMQ (CAAAR L) '($EV $FUNMAKE $ARRAYMAKE)))
		   (MEVAL (CAR L)))
		  (T (DISP2 (CAR L))))
	   ,(MEVAL (CADR L)))))

(defun set-lineinfo (fnname lineinfo body type)
  (cond ((and (consp lineinfo) (eq 'src (third lineinfo)))
	 (setf (cdddr lineinfo) (list fnname (first lineinfo)))
	 (setf (get fnname 'lineinfo) body))
	(t (remprop fnname 'lineinfo))))

(DEFMSPEC MDEFINE (L )
 (let ($use_fast_arrays) ;;for mdefine's we allow use the oldstyle hasharrays
 (TWOARGCHECK L)
 (SETQ L (CDR L))
 (LET ((FUN (CAR L)) (BODY (CADR L)) ARGS SUBS ARY FNNAME MQDEF REDEF)
   (COND ((OR (ATOM FUN)
	      (AND (SETQ MQDEF (EQ (CAAR FUN) 'MQAPPLY))
		   (MEMQ 'array (CDAR FUN))))
	  (MERROR "Improper function definition:~%~M" FUN))
	 (MQDEF (IF (OR (ATOM (CADR FUN))
			(NOT (SETQ ARY (MEMQ 'array (CDAADR FUN)))))
		    (MERROR "Improper function definition:~%~M" (CADR FUN)))
		(SETQ SUBS (CDADR FUN) ARGS (CDDR FUN) FUN (CADR FUN)
		      FNNAME ($VERBIFY (CAAR FUN)))
		(IF (AND (NOT (MGETL FNNAME '(HASHAR ARRAY)))
			 (GET FNNAME 'SPECSIMP))
		    (MTELL "Warning - you are redefining the MACSYMA ~
			    subscripted function ~:M.~%"
			   FNNAME)))
	 ((PROG2 (SETQ FNNAME ($VERBIFY (CAAR FUN)))
		 (OR (MOPP FNNAME) (MEMQ FNNAME '($ALL $ALLBUT $%))))
	  (MERROR "Improper function name: ~:@M" FNNAME))
	 ((SETQ ARY (MEMQ 'array (CDAR FUN))) (SETQ SUBS (CDR FUN)))
	 (T (SETQ ARGS (CDR FUN) REDEF (MREDEF-CHECK FNNAME))))
   (IF (NOT ARY) (REMOVE1 (NCONS FNNAME) 'MMACRO T $MACROS T))
   (MDEFCHK FNNAME (OR ARGS (AND (NOT MQDEF) SUBS)) ARY MQDEF)
   (IF (NOT (EQ FNNAME (CAAR FUN))) (RPLACA (CAR FUN) FNNAME))
   (COND ((NOT ARY) (IF (AND EVP (MEMQ FNNAME (CAR LOCLIST)))
			(MPUTPROP FNNAME T 'LOCAL-FUN)
			(REMOVE-TRANSL-FUN-PROPS FNNAME))
		    (ADD2LNC (CONS (NCONS FNNAME) ARGS) $FUNCTIONS)
		    (set-lineinfo fnname (cadar fun) body 'mexpr)
		    (MPUTPROP FNNAME (MDEFINE1 ARGS BODY) 'MEXPR)
		    #+MacLisp
		    (IF (NOT REDEF)
			(ARGS FNNAME (IF (NOT (MGET FNNAME 'MLEXPRP))
					 (CONS NIL (LENGTH ARGS)))))
		    (IF $TRANSLATE (TRANSLATE-FUNCTION FNNAME)))
	 ((PROG2 (ADD2LNC FNNAME $ARRAYS)
		 (SETQ ARY (MGETL FNNAME '(HASHAR ARRAY)))
		 (REMOVE-TRANSL-ARRAY-FUN-PROPS FNNAME))
	  (WHEN (MFILEP (CADR ARY))
		(I-$UNSTORE (NCONS FNNAME))
		(SETQ ARY (MGETL FNNAME '(HASHAR ARRAY))))
	  (IF (NOT (= (IF (EQ (CAR ARY) 'HASHAR)
			  (aref (symbol-array (CADR ARY)) 2)
			  #+NIL (ARRAY-/#-DIMS (CADR ARY))
			  #-NIL (LENGTH (CDR (ARRAYDIMS (CADR ARY)))))
		      (LENGTH SUBS)))
	      (MERROR "Array ~:M already defined with different dimensions"
		      FNNAME))
	  (MDEFARRAY FNNAME SUBS ARGS BODY MQDEF))
	 (T (MPUTPROP FNNAME (SETQ ARY (GENSYM)) 'HASHAR)
	    (*ARRAY ARY T 7)
	    (STORE (AFUNCALL ARY 0) 4)
	    (STORE (AFUNCALL ARY 1) 0)
	    (STORE (AFUNCALL ARY 2) (LENGTH SUBS))
	    (MDEFARRAY FNNAME SUBS ARGS BODY MQDEF)))
   (CONS '(MDEFINE SIMP) #-CL L #+CL (COPY-LIST L)))))

; Checks to see if a user is clobbering the name of a system function.  
; Prints a warning and returns T if he is, and NIL if he isn't.
(DEFUN MREDEF-CHECK (FNNAME)
 (COND ((AND (NOT (MGET FNNAME 'MEXPR))
	     (OR (AND #+MacLisp
		      (GETL FNNAME '(SUBR FSUBR MFEXPR*S LSUBR AUTOLOAD))
		      #+Franz (getd fnname)
		      #+NIL
		      (OR (GET FNNAME 'MFEXPR*)
			  (GETL-NIL-FCN-PROP FNNAME '(SUBR)))
		      #+CL
		      (OR (GET FNNAME 'AUTOLOAD)
			  (GETL-LM-FCN-PROP FNNAME '(SUBR FSUBR LSUBR))
			  (GET FNNAME 'MFEXPR*S))
		      (NOT (GET FNNAME 'TRANSLATED)))
		 (MOPP FNNAME)))
	(PRINC "Warning - you are redefining the MACSYMA ")
	(IF (GETL FNNAME '(VERB OPERATORS))
	    (PRINC "command ") (PRINC "function "))
	(PRINC (STRIPDOLLAR FNNAME))
	(TERPRI)
	#+(OR MACLISP FRANZ) (ARGS FNNAME NIL)
	T)))

(DEFUN MDEFARRAY (FUN SUBS ARGS BODY MQDEF)
  (cond ((and  (boundp fun) (hash-table-p fun))
	 (error "~a is already a hash table.  Make it a function first" fun)))
  
  (COND ((AND (NULL ARGS) (NOT MQDEF)) (MPUTPROP FUN (MDEFINE1 SUBS BODY) 'AEXPR))
	((NULL (DOLIST (U SUBS)
		 (IF (NOT (OR ($CONSTANTP U) (char= (GETCHARN U 1) #\&)))
		     (RETURN T))))
 	 (ARRSTORE (CONS (NCONS FUN) SUBS) (MDEFINE1 ARGS BODY)))
	(T (MDEFCHK FUN SUBS T NIL)
	   (MPUTPROP FUN (MDEFINE1 SUBS (MDEFINE1 ARGS BODY)) 'AEXPR))))

(DEFMFUN MSPECFUNP (FUN) (AND (OR (GETL-FUN FUN '(FSUBR FEXPR MACRO))
(GETL FUN '(MFEXPR* MFEXPR*S)) 	 (AND $TRANSRUN (GET FUN
'TRANSLATED-MMACRO)) 	 (MGET FUN 'MMACRO)) (NOT (GET FUN 'EVOK))))

(DEFUN MDEFINE1 (ARGS BODY)
  (IF FUNDEFSIMP
      (LET ((SBODY (SIMPLIFY BODY)))
	(WHEN (AND (NOT (ATOM BODY)) (NOT (ATOM SBODY)))
	      (RPLACA BODY (CAR SBODY)) (RPLACD BODY (CDR SBODY)))))
  (LIST '(LAMBDA) (CONS '(MLIST) ARGS) BODY))

(DEFUN MDEFCHK (FUN ARGS ARY MQDEF)
  (DO ((L ARGS (CDR L)) (MFEX) (MLEX))
      ((NULL L) (AND MFEX (NOT MQDEF) (MPUTPROP FUN MFEX 'MFEXPRP))
		(AND MLEX (NOT MQDEF) (MPUTPROP FUN MLEX 'MLEXPRP)))
    (IF (NOT (OR (MDEFPARAM (CAR L))
		 (AND (OR (NOT ARY) MQDEF)
		      (OR (AND MFEXPRP (MQUOTEP (CAR L))
			       (MDEFPARAM (CADAR L)) (SETQ MFEX T))
			  (AND (MDEFLISTP L)
			       (OR (MDEFPARAM (CADAR L))
				   (AND MFEXPRP (MQUOTEP (CADAR L))
					(MDEFPARAM (CADR (CADAR L)))
					(SETQ MFEX T)))
			       (SETQ MLEX T))))))
	(MERROR "Improper parameter in function definition for ~:M:~%~M"
		FUN (CAR L)))))

(DEFUN MDEFPARAM (X)
  (AND (ATOM X) (NOT (MAXIMA-CONSTANTP X)) (NOT (char= (GETCHARN X 1) #\&))))

(DEFUN MDEFLISTP (L)
  (AND (NULL (CDR L)) ($LISTP (CAR L)) (CDAR L) (NULL (CDDAR L))))

(DEFMFUN MOPP (FUN)
  (AND (NOT (EQ FUN 'MQAPPLY))
       (OR (MOPP1 FUN)
	   (AND (GET FUN 'OPERATORS) (NOT (RULECHK FUN))
		(NOT (MEMQ FUN RULEFCNL)) (NOT (GET FUN 'OPERS))))))

(DEFMFUN MOPP1 (FUN)
  (declare (object fun))
  (AND (SETQ FUN (GET FUN 'OP)) (NOT (MEMQ FUN (CDR $PROPS)))))

;; maybe should have a separate version, or a macro..
(defun mapply (a b c ) (mapply1 a b c nil))

;(DEFMFUN $CALL FEXPR (L)
;  (IF (NULL L) (MERROR "Wrong number of args to CALL"))
;  (MEVAL (CONS (NCONS (CAR L)) (CDR L))))

;(DEFMFUN $ACALL FEXPR (L)
;  (IF (NULL L) (MERROR "Wrong number of args to ACALL"))
;  (MEVAL (CONS (CONS (CAR L) '(ARRAY)) (CDR L))))

(DEFMSPEC $APPLY (L)
  (TWOARGCHECK L)
  (LET ((FUN (MEVAL (CADR L))) (ARG (MEVAL (CADDR L))))
    (IF (NOT ($LISTP ARG))
	(MERROR "Attempt to apply ~:M to ~M~
		 ~%Second argument to APPLY must be a list."
		FUN ARG))
    (AUTOLDCHK (SETQ FUN (GETOPR FUN)))
    (MAPPLY1 FUN (CDR ARG) (CADR L) l)))

(DEFUN AUTOLDCHK (FUN)
  (IF (AND (SYMBOLP FUN)
	   (GET FUN 'AUTOLOAD)
	   (NOT (OR (FBOUNDP FUN) (MFBOUNDP FUN))))
      (LOAD-FUNCTION FUN T)))

(DEFMSPEC $DISPFUN (L) (SETQ L (CDR L))
 (COND ((OR (CDR L) (NOT (EQ (CAR L) '$ALL))) (DISPFUN1 L NIL NIL))
       (T (DISPFUN1 (CDR $FUNCTIONS) T NIL)
	  (DISPFUN1 (MAPCAN #'(LAMBDA (X) (IF (MGET X 'AEXPR) (NCONS X)))
			    (CDR $ARRAYS))
		    NIL T)
	  (DISPFUN1 (CDR $MACROS) T NIL))))

(DEFUN DISPFUN1 (L FLAG MAEXPRP)
 (DOLIST (FUN L) ($LDISP (CONSFUNDEF (IF FLAG (CAAR FUN) FUN) MAEXPRP NIL)))
 '$DONE)

(DEFMSPEC $FUNDEF (X) (CONSFUNDEF (FEXPRCHECK X) NIL NIL))

(DEFUN CONSFUNDEF (X MAEXPRP STRINGP)
 (PROG (ARRYP NAME FUN)
  (SETQ ARRYP (AND (NOT (ATOM X)) (NOT (EQ (CAAR X) 'MQAPPLY)) (MEMQ 'array (CDAR X))))
  (COND ((ATOM X) (SETQ NAME ($VERBIFY X)
			FUN (OR (AND (NOT MAEXPRP) (MGETL NAME '(MEXPR MMACRO)))
				(MGETL NAME '(AEXPR)))))
	(ARRYP (SETQ FUN (MEVAL1 (SETQ NAME (CONS (LIST ($VERBIFY (CAAR X)) 'array) (CDR X)))))
	       (IF (OR (ATOM FUN) (NOT (EQ (CAAR FUN) 'LAMBDA))) (SETQ FUN NIL))))
  (COND ((NOT FUN) (COND (STRINGP (RETURN X)) ((MEMQ 'EDIT STATE-PDL) (TERPRI)))
		   (MERROR "~:M is not the name of a user function." X))
	((AND (NOT ARRYP) (MFILEP (CADR FUN)))
	 (SETQ FUN (LIST (CAR FUN) (DSKGET (CADADR FUN) (CAR (CDDADR FUN)) (CAR FUN) NIL)))))
  (RETURN
   (CONS (IF (EQ (CAR FUN) 'MMACRO) '(MDEFMACRO SIMP) '(MDEFINE SIMP))
	 (COND (ARRYP (CONS (CONS '(MQAPPLY) (CONS NAME (CDADR FUN))) (CDDR FUN)))
	       (T (FUNCALL #'(LAMBDA (BODY)
		    (COND ((AND (EQ (CAR FUN) 'AEXPR) (NOT (ATOM BODY))
				(EQ (CAAR BODY) 'LAMBDA))
			   (LIST (CONS '(MQAPPLY) (CONS (CONS (CONS NAME '(ARRAY))
							      (CDR (CADADR FUN)))
							(CDADR BODY)))
				 (CADDR BODY)))
			  (T (LIST (CONS (CONS NAME (IF (EQ (CAR FUN) 'AEXPR) '(ARRAY)))
					 (CDR (CADADR FUN)))
				   BODY))))
		   (CADDR (CADR FUN)))))))))


(DEFMFUN $FUNMAKE (FUN ARGS)
  (IF (NOT (OR (SYMBOLP FUN) ($SUBVARP FUN)
	       (AND (NOT (ATOM FUN)) (EQ (CAAR FUN) 'LAMBDA))))
      (MERROR "Bad first argument to FUNMAKE: ~M" FUN))
  (IF (NOT ($LISTP ARGS)) (MERROR "Bad second argument to FUNMAKE: ~M" ARGS))
  (MCONS-OP-ARGS (GETOPR FUN) (CDR ARGS)))

(DEFMFUN MCONS-OP-ARGS (OP ARGS)
 (IF (SYMBOLP OP) (CONS (NCONS OP) ARGS) (LIST* '(MQAPPLY) OP ARGS)))

(DEFMFUN OPTIONP (X)
 (AND (BOUNDP X) (NOT (MEMQ X (CDR $VALUES))) (NOT (MEMQ X (CDR $LABELS)))))

(DEFMSPEC MCOND (FORM) (SETQ FORM (CDR FORM))
 (DO ((U FORM (CDDR U)) (V))
     ((NULL U) NIL)
     (COND ((EQ (SETQ V (MEVALP (CAR U))) T) (RETURN (MEVAL (CADR U))))
	   (V (RETURN (LIST* '(MCOND) V (MAPCAR #'MEVAL-ATOMS (CDR U))))))))

(DEFUN MEVAL-ATOMS (FORM) 
 (COND ((ATOM FORM) (MEVAL1 FORM))
       ((EQ (CAAR FORM) 'MQUOTE) (CADR FORM))
       ((AND (OR (GETL-FUN (CAAR FORM) '(FSUBR FEXPR))
		 (GETL (CAAR FORM) '(MFEXPR* MFEXPR*S)))
	     (NOT (MEMQ (CAAR FORM) '(MCOND MAND MOR MNOT MPROGN MDO MDOIN))))
	FORM)
       (T (RECUR-APPLY #'MEVAL-ATOMS FORM))))

(DEFMSPEC MDO (FORM) (SETQ FORM (CDR FORM))
 (FUNCALL #'(LAMBDA (MDOP VAR NEXT TEST DO)
   (SETQ NEXT (OR (CADDDR FORM) (LIST '(MPLUS) (OR (CADDR FORM) 1) VAR))
	 TEST (LIST '(MOR)
		    (COND ((NULL (CAR (CDDDDR FORM))) NIL)
			  (T (LIST (IF (MNEGP ($NUMFACTOR (SIMPLIFY (CADDR FORM))))
				       '(MLESSP)
				       '(MGREATERP))
				   VAR (CAR (CDDDDR FORM)))))
		    (CADR (CDDDDR FORM)))
	 DO (CADDR (CDDDDR FORM)))
   (MBINDING ((NCONS VAR)
	      (NCONS (IF (NULL (CADR FORM)) 1 (MEVAL (CADR FORM)))))
	     (DO ((VAL) (BINDL BINDLIST))
		 ((IS TEST) '$DONE)
	       (COND ((NULL (SETQ VAL (CATCH 'MPROG (PROG2 (MEVAL DO) NIL))))
		      (MSET VAR (MEVAL NEXT)))
		     ((ATOM VAL) (MERROR "GO not in BLOCK:~%~M" VAL))
		     ((NOT (EQ BINDL BINDLIST))
		      (MERROR "Illegal RETURN:~%~M" (CAR VAL)))
		     (T (RETURN (CAR VAL)))))))
  T (OR (CAR FORM) 'MDO) NIL NIL NIL))

(DEFMSPEC MDOIN (FORM) (SETQ FORM (CDR FORM))
 (FUNCALL #'(LAMBDA  (MDOP VAR SET TEST ACTION)
   (SETQ SET (IF (ATOM (SETQ SET (FORMAT1 (MEVAL (CADR FORM)))))
		 (MERROR "Atomic 'IN' argument to DO statement:~%~M" SET)
		 (MARGS SET))
	 TEST (LIST '(MOR)
		    (IF (CAR (CDDDDR FORM))
			(LIST '(MGREATERP) VAR (CAR (CDDDDR FORM))))
		    (CADR (CDDDDR FORM)))
	 ACTION (CADDR (CDDDDR FORM)))
   (COND ((ATOM SET) '$DONE)
	 (T (MBINDING ((NCONS VAR) (NCONS (CAR SET)))
		      (DO ((VAL) (BINDL BINDLIST))
			  ((OR (ATOM SET) (IS TEST))
			   '$DONE)
			(COND ((NULL (SETQ VAL (CATCH 'MPROG (PROG2 (MEVAL ACTION) NIL))))
			       (IF (SETQ SET (CDR SET)) (MSET VAR (CAR SET))))
			      ((ATOM VAL) (MERROR "GO not in BLOCK:~%~M" VAL))
			      ((NOT (EQ BINDL BINDLIST))
			       (MERROR "Illegal RETURN:~%~M" (CAR VAL)))
			      (T (RETURN (CAR VAL)))))))))
  T (OR (CAR FORM) 'MDO) NIL NIL NIL))

(DEFMSPEC MPROG (PROG) (SETQ PROG (CDR PROG))
 (LET (VARS VALS (MLOCP T))
      (IF ($LISTP (CAR PROG)) (SETQ VARS (CDAR PROG) PROG (CDR PROG)))
      (SETQ LOCLIST (CONS NIL LOCLIST))
      (DO ((L VARS (CDR L))) ((NULL L) (SETQ VALS VARS))
	  (IF (NOT (ATOM (CAR L))) (RETURN (SETQ VALS T))))
      (IF (EQ VALS T)
	  (SETQ VALS (MAPCAR #'(LAMBDA (V)
				(COND ((ATOM V) V)
				      ((EQ (CAAR V) 'MSETQ) (MEVAL (CADDR V)))
				      (T (MERROR
					  "Improper form in BLOCK variable list: ~M"
					  V))))
			     VARS)
		VARS (MAPCAR #'(LAMBDA (V) (IF (ATOM V) V (CADR V))) VARS)))
      (MBINDING (VARS VALS)
		(DO ((PROG PROG (CDR PROG)) (MPROGP PROG)
		     (BINDL BINDLIST) (VAL '$DONE) (RETP) (X) ($%% '$%%))
		    ((NULL PROG) (MUNLOCAL) VAL)
		  (COND ((ATOM (CAR PROG))
			 (IF (NULL (CDR PROG))
			     (SETQ RETP T VAL (MEVAL (CAR PROG)))))
			((NULL (SETQ X (CATCH 'MPROG
					 (PROG2 (SETQ VAL (SETQ $%% (MEVAL (CAR PROG))))
						NIL)))))
			((NOT (EQ BINDL BINDLIST))
			 (IF (NOT (ATOM X))
			     (MERROR "Illegal RETURN:~%~M" (CAR X))
			     (MERROR "Illegal GO:~%~M" X)))
			((NOT (ATOM X)) (SETQ RETP T VAL (CAR X)))
			((NOT (SETQ PROG (zl-MEMBER X MPROGP)))
			 (MERROR "No such tag as ~:M" X)))
		  (IF RETP (SETQ PROG '(NIL)))))))

(DEFMFUN MRETURN (X)
 (IF (AND (NOT MPROGP) (NOT MDOP))
     (MERROR "RETURN not in BLOCK:~%~M" X))
 (THROW 'MPROG (NCONS X)))

(DEFMSPEC MGO (TAG)
 (SETQ TAG (FEXPRCHECK TAG))
 (COND ((NOT MPROGP) (MERROR "GO not in BLOCK:~%~M" TAG))
       ((ATOM TAG) (THROW 'MPROG TAG))
       (T (MERROR "Argument to GO not atomic:~%~M" TAG))))

(DEFMSPEC $SUBVAR (L) (SETQ L (CDR L))
 (IF (NULL L) (WNA-ERR '$SUBVAR)) (MEVAL (CONS '(MQAPPLY ARRAY) L)))

(DEFMFUN RAT (X Y) `((RAT SIMP) ,X ,Y))

(DEFMFUN $EXP (X) `((MEXPT) $%E ,X))

(DEFMFUN $SQRT (X) `((%SQRT) ,X))


(DEFMFUN ADD2LNC (ITEM LLIST &aux #+lispm  (default-cons-area working-storage-area))
 (WHEN (NOT (MEMALIKE ITEM (IF ($LISTP LLIST) (CDR LLIST) LLIST)))
       (IF (NOT (ATOM ITEM)) (zl-DELETE (zl-ASSOC (CAR ITEM) LLIST) LLIST 1))
       (NCONC LLIST (NCONS ITEM))))

(DEFMFUN BIGFLOATM* (BF)
 (IF (NOT (MEMQ 'SIMP (CDAR BF)))
     (SETQ BF (CONS (LIST* (CAAR BF) 'SIMP (CDAR BF)) (CDR BF))))
 (IF $FLOAT ($FLOAT BF) BF))

(DEFMFUN $ALLBUT N (CONS '($ALLBUT) (LISTIFY N)))

(DEFMFUN MFILEP (X)
  (AND (NOT (ATOM X)) (NOT (ATOM (CAR X))) (EQ (CAAR X) 'MFILE)))

#-(or NIL cl)
(DEFMFUN DSKSETQ FEXPR (L) (LET ((DSKSETP T)) (MSET (CAR L) (EVAL (CADR L)))))
#+cl
(defquote DSKSETQ (&rest L) (LET ((DSKSETP T)) (MSET (CAR L) (EVAL (CADR L)))))

(DEFMFUN DSKRAT (X)
 (ORDERPOINTER (CADDAR X))
 (MAPC #'(LAMBDA (A B) (DSKRAT-SUBST A B (CDDDDR (CAR X)))  ; for TAYLOR forms
		       (DSKRAT-SUBST A B (CDR X)))
       GENVAR (CADDDR (CAR X)))
 (RPLACA (CDDDAR X) GENVAR)
 #-(OR CL NIL) (GCTWA) 
 (IF (MEMQ 'TRUNC (CAR X)) (SRCONVERT X) X))  ; temporary

(DEFUN DSKRAT-SUBST (X Y Z) 
 (COND ((ATOM Z) Z)
       (T (IF (EQ Y (CAR Z)) (RPLACA Z X) (DSKRAT-SUBST X Y (CAR Z)))
	  (DSKRAT-SUBST X Y (CDR Z))
	  Z)))

(DEFMFUN |''MAKE-FUN| (NOUN-NAME X)
 (LET (($NUMER T) ($FLOAT T))
      (SIMPLIFYA (LIST (NCONS NOUN-NAME) (RESIMPLIFY X)) T)))

(DEFMACRO |''MAKE| (FUN NOUN)
 `(DEFMFUN ,FUN (X) (|''MAKE-FUN| ',NOUN X)))

(|''MAKE| $LOG %LOG)
(|''MAKE| $SIN %SIN) (|''MAKE| $COS %COS) (|''MAKE| $TAN %TAN)
(|''MAKE| $COT %COT) (|''MAKE| $SEC %SEC) (|''MAKE| $CSC %CSC)
(|''MAKE| $SINH %SINH) (|''MAKE| $COSH %COSH) (|''MAKE| $TANH %TANH)
(|''MAKE| $COTH %COTH) (|''MAKE| $SECH %SECH) (|''MAKE| $CSCH %CSCH)
(|''MAKE| $ASIN %ASIN) (|''MAKE| $ACOS %ACOS) (|''MAKE| $ATAN %ATAN)
(|''MAKE| $ACOT %ACOT) (|''MAKE| $ASEC %ASEC) (|''MAKE| $ACSC %ACSC)
(|''MAKE| $ASINH %ASINH) (|''MAKE| $ACOSH %ACOSH) (|''MAKE| $ATANH %ATANH)
(|''MAKE| $ACOTH %ACOTH) (|''MAKE| $ASECH %ASECH) (|''MAKE| $ACSCH %ACSCH)
(|''MAKE| $GAMMA %GAMMA) 

(DEFMFUN $BINOMIAL (X Y)
 (LET (($NUMER T) ($FLOAT T)) (SIMPLIFY (LIST '(%BINOMIAL) X Y))))

(PROG1 '(EVFUN properties)
       (MAPC #'(LAMBDA (X) (PUTPROP X T 'EVFUN))
	     '($RADCAN $FACTOR $RATSIMP $TRIGEXPAND $TRIGREDUCE $LOGCONTRACT
	       $ROOTSCONTRACT $BFLOAT $RATEXPAND $FULLRATSIMP $RECTFORM
	       $POLARFORM)))

(PROG1 '(EVFLAG properties)
       (MAPC #'(LAMBDA (X) (PUTPROP X T 'EVFLAG))
	     '($EXPONENTIALIZE $%EMODE $DEMOIVRE $LOGEXPAND $LOGARC $LOGNUMER
	       $RADEXPAND $KEEPFLOAT $LISTARITH $FLOAT $RATSIMPEXPONS $RATMX
	       $SIMP $SIMPSUM $ALGEBRAIC $RATALGDENOM $FACTORFLAG $RATFAC
	       $INFEVAL $%ENUMER $PROGRAMMODE $LOGNEGINT $LOGABS $LETRAT
	       $HALFANGLES $EXPTISOLATE $ISOLATE_WRT_TIMES $SUMEXPAND
	       $CAUCHYSUM $NUMER_PBRANCH $M1PBRANCH $DOTSCRULES
	       $TRIGEXPAND)))

(MDEFPROP $%E     2.71828182845904523536 $NUMER)  ; (EXP 1) [wrong in ITS-MACLISP]
(MDEFPROP $%PI    3.14159265358979323846 $NUMER)  ; (ATAN 0 -1)
(MDEFPROP $%PHI   1.61803398874989484820 $NUMER)  ; (1+sqrt(5))/2
(MDEFPROP $%GAMMA 0.5772156649015328606  $NUMER)  ; Euler's constant

(MDEFPROP $HERALD_PACKAGE (NIL $TRANSLOAD T) $PROPS)
(MDEFPROP $LOAD_PACKAGE (NIL $TRANSLOAD T) $PROPS)

(DEFPROP BIGFLOAT BIGFLOATM* MFEXPR*)
(DEFPROP LAMBDA CONSTLAM MFEXPR*)
(DEFPROP QUOTE CADR MFEXPR*)  ; Needed by MATCOM/MATRUN.

#-(or cl NIL)
(EVAL-WHEN (EVAL COMPILE) (SETQ *read-base* OLD-IBASE))
#+cl
(EVAL-WHEN (EVAL COMPILE) (SETQ  *read-BASE* OLD-read-base))

; Undeclarations for the file:
(declare-top (NOTYPE N I J NNEED NGIVEN NCELLS NITEMS LISPSUB INDX EVFLG))





