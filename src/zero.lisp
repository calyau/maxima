;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;  
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module zero)

(declare-top (SPECIAL
	      ;S VAR  V1 V R1 R2 ;; added declares
	      #-cl EXP ;I don't think exp is necessary--wfs
	      $NUMER $LISTCONSTVARS VARLIST GENVAR)
	 (*LEXPR $RAT))

(DEFMFUN $ZEROEQUIV (EXP VAR)
       (declare (special var ))
       (PROG (R S V VARLIST GENVAR)
	     (declare (special S V))
	     (SETQ EXP (SPECREPCHECK EXP))
	     (SETQ R (LET ($LISTCONSTVARS) ($LISTOFVARS EXP)))
	     (IF (AND (CDR R) (OR (CDDR R) (NOT (ALIKE1 (CADR R) VAR))))
		 (RETURN '$DONTKNOW))
	     (SETQ EXP ($EXPONENTIALIZE EXP))
	     (SETQ R (SDIFF EXP VAR))
	     (IF (ISINOP R '%DERIVATIVE) (RETURN '$DONTKNOW))
	     ($RAT R)
	     (SETQ R ($RAT EXP))
	     (SETQ S (CAR R))
	     (SETQ V (RATNUMERATOR (CDR R)))
	     (RETURN (ZEROEQUIV1 V))))

(DEFUN ZEROEQUIV1 (V)
  (declare (special var v s))
       (PROG (V1 V2 COEFF DEG)
	     (declare (special V1 V2))
	     (IF (ATOM V) (RETURN (EQUAL V 0)))
   COEFFLOOP (IF (NULL (CDR V)) (RETURN T))
             (SETQ DEG (CADR V))
	     (IF (EQUAL DEG 0) (RETURN (ZEROEQUIV1 (CADDR V))))
	     (SETQ COEFF (CADDR V))
	     (WHEN (ZEROEQUIV1 COEFF)
		   (SETQ V (CONS (CAR V) (CDDDR V)))
		   (GO COEFFLOOP))
	     (SETQ V1 ($RAT (SDIFF (RATDISREP (CONS S (CONS V (CADDR V))))
				   VAR)))
	     (SETQ V2 (CADR ($RAT (RATDISREP V1))))
	     (IF (EQUAL (PDEGREE V2 (CAR V)) (CADR V))
		 (RETURN (ZEROEQUIV2 V)))
	     (IF (LESSP (PDEGREE V2 (CAR V)) (CADR V))
		 (RETURN (IF (ZEROEQUIV1 V2) (ZEROEQUIV2 V))))
	     (RETURN '$DONTKNOW)))

(DEFUN ZEROEQUIV2 (V)
       (declare (special var v s))
       (PROG (R R1 R2)
	     (declare (special r1 r2))
	     (SETQ R (SIN (TIMES 0.001 (RANDOM 1000.))))
	     (SETQ V (MAXIMA-SUBSTITUTE R VAR (RATDISREP (CONS S (CONS V 1)))))
	     (SETQ V (MEVAL '(($EV) V $NUMER)))
	     (COND ((AND (NUMBERP V) (LESSP (ABS V) (TIMES R 0.01)))
		    (RETURN T))
		   ((NUMBERP V) (RETURN NIL)))
	     (IF (AND (FREE V '$%I) (NOT (ISINOP V '%LOG)))
		 (RETURN '$DONTKNOW))
	     (SETQ R1 ($REALPART V))
	     (SETQ R1 (MEVAL '(($EV) R1 $NUMER)))
	     (IF (NOT (NUMBERP R1)) (RETURN '$DONTKNOW))
	     (SETQ R2 ($IMAGPART V))
	     (SETQ R2 (MEVAL '(($EV) R2 $NUMER)))
	     (IF (NOT (NUMBERP R2)) (RETURN '$DONTKNOW))
	     (COND ((AND (LESSP (ABS R1) (TIMES R 0.01))
			 (LESSP (ABS R2) (TIMES R 0.01)))
		    (RETURN T))
		   (T (RETURN NIL)))))





