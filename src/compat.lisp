;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;; Maclisp compatibility definitions for the Lisp Machine.  This file
;; is for Lisp differences only.  No knowledge of Macsyma should be
;; contained in this file.

;; Translated code still uses this.
(DEFQUOTE includef (  FILENAME) FILENAME)

;; Couldn't this copy to some static area?
(DEFMACRO PURCOPY (X) X)

#-nocp
(DEFMACRO CHARPOS (IGNOR)ignor  '(CDR (CURSORPOS)))
(DEFMACRO MAXIMA-SLEEP (SECONDS) `(PROCESS-SLEEP (FIX (f* ,SECONDS 60.))))

(DEFUN LINEL (&OPTIONAL STREAM)
       #+lispm 
  (FUNCALL (SI:DECODE-PRINT-ARG STREAM) ':SIZE-IN-CHARACTERS)
  #-lispm stream
  #-lispm 80 
  )

;; Perhaps this should do something.
(DEFMACRO NOINTERRUPT (IGNOR)ignor  NIL)

(DEFMACRO WITHOUT-TTY-INTERRUPTS (&REST FORM)
  `(LET (#+lispm (TV:KBD-INTERCEPTED-CHARACTERS NIL))
     ,@ FORM))

(DEFMACRO FIXNUM-IDENTITY (X) X)
(DEFMACRO FLONUM-IDENTITY (X) X)

(proclaim '(inline *quo *dif))
#+cl
(DEFun *QUO (X Y)
       (cond ((and (integerp x) (integerp y))
	      (truncate x y))
	     (t (/ X Y))))
#+cl
(DEFun *DIF (X Y) (- X Y))


;; Keep the compiler quiet.
;; Use GET-PNAME or FORMAT instead of EXPLODE, EXPLODEN, EXPLODEC.
;; Use AREF instead of GETCHAR and GETCHARN.
;; Use MAKE-SYMBOL instead of MAKNAM.
;; Use INTERN instead of IMPLODE.
;; Use STRING-LESSP instead of ALPHALESSP.
#+lispm 
(progn 
(REMPROP 'EXPLODE       'COMPILER:STYLE-CHECKER)
(REMPROP 'EXPLODEC      'COMPILER:STYLE-CHECKER)
(REMPROP 'EXPLODEN      'COMPILER:STYLE-CHECKER)
(REMPROP 'ALPHALESSP    'COMPILER:STYLE-CHECKER)
(REMPROP 'GETCHARN      'COMPILER:STYLE-CHECKER)
(REMPROP 'GETCHAR       'COMPILER:STYLE-CHECKER)
(REMPROP 'IMPLODE       'COMPILER:STYLE-CHECKER)
(REMPROP 'MAKNAM        'COMPILER:STYLE-CHECKER)
)

(DEFMACRO SFA-CALL (STREAM OPERATION ARG)
  `(FUNCALL ,STREAM (READ-FROM-STRING (STRING-APPEND #\: ,OPERATION)) ,ARG))

;; Things that appear within DECLARE bodies.

;; Why doesn't this work?
;; Because of the brain-damaged way the lispm compiler is written. -gjc
;; (PUTPROP 'DECLARE '(DECLARE-OPTIMIZER) 'COMPILER:OPTIMIZERS)
;;
;; (DEFUN DECLARE-OPTIMIZER (DECLARE-FORM &AUX (RETURN-FORM NIL))
;;   (DO ((L (REVERSE (CDR DECLARE-FORM)) (CDR L)))
;;       ((NULL L))
;;     (IF (NOT (MEMQ (CAAR L)
;; 		   '(FIXNUM FLONUM NOTYPE MACROS ARRAY* GENPREFIX
;; 			    CLOSED MUZZLED MAPEX SPLITFILE)))
;; 	(PUSH (CAR L) RETURN-FORM)))
;;   (IF RETURN-FORM (CONS 'DECLARE RETURN-FORM) NIL))

;; These are in global, so avoid redefinition warning by using FDEFINE
;; rather than DEFun.

;(FDEFINE (kw FLONUM) #'(LAMBDA ( &REST IGNOR)ignor  NIL))
;(FDEFINE (kw FIXNUM) #'(LAMBDA (&QUOTE &REST IGNOR)ignor  NIL))
;(FDEFINE 'ARGS	 #'(LAMBDA (&QUOTE &REST IGNOR)ignor  NIL))

(DEFMACRO ARGS (&REST IGNOR)ignor  NIL)

#-cl
(progn
(DEFMACRO FLONUM (&REST IGNOR)ignor  NIL)
(DEFMACRO FIXNUM (&REST IGNOR)ignor  NIL)
(DEFMACRO MACROS	     ( &REST IGNOR)ignor  NIL)
(DEFMACRO CLOSED	     ( &REST IGNOR)ignor  NIL)
(DEFMACRO NOTYPE	     ( &REST IGNOR)ignor  NIL)
(DEFMACRO ARRAY*	     ( &REST IGNOR)ignor  NIL)
(DEFMACRO GENPREFIX     ( &REST IGNOR)ignor  NIL)
(DEFMACRO MUZZLED	     ( &REST IGNOR)ignor  NIL)
(DEFMACRO MAPEX	     ( &REST IGNOR)ignor  NIL)
(DEFMACRO SPLITFILE     ( &REST IGNOR)ignor  NIL)
(DEFMACRO EXPR-HASH     ( &REST IGNOR)ignor  NIL)
)
;; Run time stuff

(DEFUN SYMBOLCONC (&REST SYMS)
  (INTERN (APPLY #'concatenate 'string
		 (MAPCAR #'(LAMBDA (SYM)
			     (COND ((FLOATP SYM)
				    (FORMAT NIL "~S" SYM))
				   ((INTEGERP SYM)
				    (FORMAT NIL "~D" SYM))
				   ((symbolp sym)
				    (symbol-name sym))
				   (T SYM)))
			 SYMS))))

;(DEFUN QUOTED-ARGS (&QUOTE &REST L)
;  (MAPCAR #'(LAMBDA (X) (PUTPROP X '((1005 (FEF-ARG-OPT FEF-QT-QT))) 'ARGDESC))
;	  L))



(DEFMACRO QUOTE-ARGS ( &REST L)
  `(QUOTED-ARGS-AUX ',L))
(DEFUN QUOTED-ARGS-AUX (L)
  (MAPCAR #'(LAMBDA (X) (PUTPROP X '((1005 (FEF-ARG-OPT FEF-QT-QT))) 'ARGDESC))
	  L))


#+cl
(PROGN 'COMPILE

;;; On the 3600, STORE isn't implemented.  So, implement enough of
;;; it here to satisfy the cases the Macsyma uses.  I have yet to find
;;; it using complicated side effects of the array reference -- it's either
;;; a (STORE (ARRAYCALL ...) ...) or a (STORE (FUNCALL ...) ...) or else
;;; a (STORE (array-called-as-function ...) ...).  So, assume that if the CAR
;;; of the first form isn't ARRAYCALL or FUNCALL, then it's a STORE of the third
;;; form.

(DEFUN STORE-MACRO-HELPER (ARRAY-REF NEW-VALUE)
  ;;this is redundant and should be caught by store but a bug in compiler..
  (cond ((or (eql (car array-ref) 'aref))(equal (car array-ref) '(function aref))
	 `(setf (aref ,@ (cdr array-ref)) ,new-value))
	(t
  (CASE (LENGTH ARRAY-REF)
    (2 `(STORE-INTERNAL-1D ,@ARRAY-REF ,NEW-VALUE))
    (3 `(STORE-INTERNAL-2D ,@ARRAY-REF ,NEW-VALUE))
    (OTHERWISE (ERROR "Cannot expand STORE for array reference ~S" ARRAY-REF))))))



(DEFMACRO STORE (ARRAY-REF NEW-VALUE &aux expand-1 &environment env)
  (cond ((not (memq (car array-ref ) '(aref arraycall)))
	 (setq expand-1 (macroexpand-1 array-ref env))
	 (setq array-ref
	       (cond ((memq (car expand-1 ) '(aref arraycall))
		      expand-1)
		     (t  (macroexpand array-ref env))))))
  
  (CASE (FIRST ARRAY-REF)
    (FUNCALL (STORE-MACRO-HELPER (CDR ARRAY-REF) NEW-VALUE))
;    (ARRAYCALL (STORE-MACRO-HELPER (CDDR ARRAY-REF) NEW-VALUE))
    ;;the arrays ought to all be on in the symbol location by now --wfs
    (ARRAYCALL `(setf ,array-ref ,new-value))
    (aref `(setf ,array-ref ,new-value))
    (OTHERWISE (STORE-MACRO-HELPER `(#',(FIRST ARRAY-REF) . ,(CDR ARRAY-REF)) NEW-VALUE))))


(DEFUN STORE-INTERNAL-1D (ARRAY-SPEC INDEX NEW-VALUE)
  (SLOOP UNTIL (ARRAYP ARRAY-SPEC)
	DO (COND ((SYMBOLP ARRAY-SPEC) (SETQ ARRAY-SPEC (SYMBOL-ARRAY ARRAY-SPEC)))
		 (T (ERROR "STORE failed -- can't find array for ~S" ARRAY-SPEC))))
  (SETF (AREF ARRAY-SPEC INDEX) NEW-VALUE))

(DEFUN STORE-INTERNAL-2D (ARRAY-SPEC I1 I2 NEW-VALUE)
  (SLOOP UNTIL (ARRAYP ARRAY-SPEC)
	DO (COND ((SYMBOLP ARRAY-SPEC) (SETQ ARRAY-SPEC (SYMBOL-ARRAY ARRAY-SPEC)))
		 (T (ERROR "STORE failed -- can't find array for ~S" ARRAY-SPEC))))
  (SETF (AREF ARRAY-SPEC I1 I2) NEW-VALUE))

)  ;End PROGN 'COMPILE




