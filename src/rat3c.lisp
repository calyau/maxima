;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module rat3c)

;;	THIS IS THE NEW RATIONAL FUNCTION PACKAGE PART 3.
;;	IT INCLUDES THE GCD ROUTINES AND THEIR SUPPORTING FUNCTIONS

(DECLARE-TOP(GENPREFIX A_3))

(LOAD-MACSYMA-MACROS RATMAC)

(declare-top (special $float $keepfloat $algebraic $ratfac *alpha)
	 (special genvar))


;; List of GCD algorithms.  Default one is first.
(DEFMVAR *GCDL* '($SUBRES $EZ $RED $SPMOD $MOD $SPHEN $EEZ $ALGEBRAIC))

(DEFMVAR $GCD (CAR *GCDL*))		     ;Sparse Modular

(DEFUN CGCD (A B) (COND (MODULUS 1)
			((AND $KEEPFLOAT (OR (FLOATP A) (FLOATP B))) 1)
			(T (GCD A B)))) 

(DEFMFUN PQUOTIENTCHK (A B) (IF (EQN B 1) A (PQUOTIENT A B)))

(DEFUN PTIMESCHK (A B) (COND ((EQN A 1) B) ((EQN B 1) A) (T (PTIMES A B)))) 

(DEFUN PFLOATP (X)
       (CATCH 'FLOAT (COND ((PCOEFP X) (FLOATP X)) (T (PFLOATP1 X)))))

(DEFUN PFLOATP1 (X)
  (MAPC #'(LAMBDA (Q) (COND ((PCOEFP Q)
			     (COND ((FLOATP Q) (THROW 'FLOAT T))))
			    ((PFLOATP1 Q))))
	(CDR X))
  NIL)

(DEFMFUN PGCD (X Y)
       (SETQ X (CAR (PGCDA X Y NIL)))
       (COND ((PMINUSP X) (PMINUS X))
	     (MODULUS (MONIZE X))
	     (T X)))

(DEFMFUN PLCM (X Y)
       (SETQ X (PGCDCOFACTS X Y))
       (PTIMES (CAR X) (PTIMES (CADR X) (CADDR X))))

(DEFUN PLCMCOFACTS (X Y)
       (SETQ X (PGCDCOFACTS X Y))
       (LIST (PTIMES (CAR X) (PTIMES (CADR X) (CADDR X)))
	     (CADDR X) (CADR X)))

(DEFUN PGCDCOFACTS (X Y) 
       (LET ((A (PGCDA X Y T)))
	    (COND ((CDR A) A)
		  ((EQUAL (SETQ A (CAR A)) 1) (LIST 1 X Y))
		  ((AND $ALGEBRAIC (NOT (PCOEFP A)))
		   (CONS A (PROG2 (SETQ X (RQUOTIENT X A)
					Y (RQUOTIENT Y A)
					A (PGCDCOFACTS (CDR X) (CDR Y)))
				  (LIST (PTIMES (CAR X) (CADDR A))
					(PTIMES (CAR Y) (CADR A))
					(PTIMES (CADR A) (CDR Y))))))
		  ((EQ A X) (LIST X 1 (PQUOTIENT Y X)))
		  ((EQ A Y) (LIST A (PQUOTIENT X Y) 1))
		  (T (LIST A (PQUOTIENT X A) (PQUOTIENT Y A))))))

(DEFUN PGCDA (X Y COFAC? &AUX A C) 
  (COND ((NOT $GCD) (LIST 1 X Y))
	((AND $KEEPFLOAT (OR (PFLOATP X) (PFLOATP Y)))
	 (COND ((OR (PCOEFP X) (PCOEFP Y)
		    (PCOEFP (SETQ A (CAR (PTERMCONT X))))
		    (PCOEFP (SETQ A (PGCD A (CAR (PTERMCONT Y))))))
		(LIST 1 X Y))
	       (T (LIST A))))
	((PCOEFP X)
	 (COND ((PCOEFP Y)
		(CONS (SETQ A (CGCD X Y))
		      (AND COFAC?
			   (LIST (CQUOTIENT X A) ;(CQUOTIENT 0 0) = 0
				 (CQUOTIENT Y A)))))
	       ((ZEROP X) (LIST Y X 1))
	       (T (LIST (PCONTENT1 (CDR Y) X)))))
	((PCOEFP Y) (COND ((ZEROP Y) (LIST X 1 Y))
			  (T (LIST (PCONTENT1 (CDR X) Y)))))
	((EQUAL X Y) (LIST X 1 1))
	($RATFAC (FPGCDCO X Y))
	((NOT (EQ (P-VAR X) (P-VAR Y)))
	 (LIST (IF (POINTERGP (P-VAR X) (P-VAR Y))
		   (OLDCONTENT1 (P-TERMS X) Y)
		   (OLDCONTENT1 (P-TERMS Y) X))))
	((PROGN (DESETQ (A X) (PTERMCONT X))
		(DESETQ (C Y) (PTERMCONT Y))
		(NOT (AND (EQUAL A 1) (EQUAL C 1))))
	 (MAPCAR #'PTIMES (MONOMGCDCO A C COFAC?) (PGCDA X Y COFAC?)))
	((AND (NOT $ALGEBRAIC) (NOT MODULUS)
	      (DESETQ (A . C) (LIN-VAR-FIND (NREVERSE (PDEGREEVECTOR X))
					    (NREVERSE (PDEGREEVECTOR Y))
					    (REVERSE GENVAR))))
	 (COND ((f= A 1) (LINHACK X Y (CAR C) (CADR C) COFAC?))
	       (T (SETQ A (LINHACK Y X A (CADR C) COFAC?))
		  (IF (CDR A) (RPLACD A (NREVERSE (CDR A))))
		  A)))
	((EQ $GCD '$SPMOD) (LIST (ZGCD X Y)))
	((EQ $GCD '$SUBRES) (LIST (OLDGCD X Y)))
	((EQ $GCD '$ALGEBRAIC)
	 (IF (OR (PALGP X) (PALGP Y))
	     (LET (($GCD '$SUBRES)) (LIST (OLDGCD X Y)))
	     (LET (($GCD '$SPMOD)) (LIST (ZGCD X Y)))))
	((EQ $GCD '$EZ) (EZGCD2 X Y))
	((EQ $GCD '$RED) (LIST (OLDGCD X Y)))
	((EQ $GCD '$MOD) (NEWGCD X Y MODULUS))
	((EQ $GCD '$SPHEN) (SPHGCD X Y))
	((EQ $GCD '$EEZ) (EEZGCD X Y))
	((NOT (MEMQ $GCD *GCDL*))
	 (MERROR "GCD set incorrectly:~%~M" $GCD))
	(T (LIST 1 X Y))))

;; (DEFUN PMINDEG (P) (IF (PCOEFP P) 0 (NXTTOLAST (CDR P))))

;; (DEFUN PDEGRED (P N) (IF (ZEROP N) P (PQUOTIENT P (MAKE-POLY (P-VAR P) N 1))))

(DEFUN MONOMGCDCO (P Q COFAC?)
  (LET ((GCD (MONOMGCD P Q)))
    (CONS GCD (IF COFAC? (LIST (PQUOTIENT P GCD) (PQUOTIENT Q GCD)) ()))))
  
(DEFUN MONOMGCD (P Q)
  (COND ((OR (PCOEFP P) (PCOEFP Q)) 1)
	((EQ (P-VAR P) (P-VAR Q))
	 (MAKE-POLY (P-VAR P) (MIN (P-LE P) (P-LE Q))
		    (MONOMGCD (P-LC P) (P-LC Q))))
	((POINTERGP (CAR P) (CAR Q)) (MONOMGCD (P-LC P) Q))
	(T (MONOMGCD P (P-LC Q)))))
	

(DEFUN LINHACK (POL1 POL2 NONLINDEG VAR COFAC?)
  (PROG (COEFF11 COEFF12 GCDAB RPOL1 RPOL2 GCDCD GCDCOEF)
	(DESETQ (COEFF11 . COEFF12) (BOTHPRODCOEF (MAKE-POLY VAR) POL1))
	(SETQ GCDAB (IF (PZEROP COEFF12) COEFF11
			(PGCD COEFF11 COEFF12)))
	(COND ((EQUAL GCDAB 1)
	       (COND ((SETQ COEFF11 (TESTDIVIDE POL2 POL1))
		      (RETURN (LIST POL1 1 COEFF11)))
		     (T (RETURN (LIST 1 POL1 POL2))))))
	(SETQ RPOL1 (PQUOTIENT POL1 GCDAB))
	(DESETQ (GCDCD RPOL2) (LINHACKCONTENT VAR POL2 NONLINDEG))
	(COND ((EQUAL GCDCD 1)
	       (COND ((SETQ COEFF12 (TESTDIVIDE RPOL2 RPOL1))
		      (RETURN (LIST RPOL1 GCDAB COEFF12)))
		     (T (RETURN (LIST 1 POL1 POL2))))))
	(COND (COFAC? (DESETQ (GCDCOEF COEFF11 COEFF12)
			      (PGCDCOFACTS GCDAB GCDCD))
		      (COND ((SETQ GCDCD (TESTDIVIDE RPOL2 RPOL1))
			     (RETURN (LIST (PTIMES GCDCOEF RPOL1)
					   COEFF11
					   (PTIMES COEFF12 GCDCD))))
			    (T (RETURN (LIST GCDCOEF 
					     (PTIMES COEFF11 RPOL1)
					     (PTIMES COEFF12 RPOL2))))))
	      (T (SETQ GCDCOEF (PGCD GCDCD GCDAB))
		 (COND ((TESTDIVIDE RPOL2 RPOL1)
			(RETURN (LIST (PTIMES GCDCOEF RPOL1))))
		       (T (RETURN (LIST GCDCOEF))))))))

(DEFUN LIN-VAR-FIND (A B C)
  (DO ((VARL C (CDR VARL))
       (DEGL1 A (CDR DEGL1))
       (DEGL2 B (CDR DEGL2)))
      ((OR (NULL DEGL1) (NULL DEGL2)) NIL)
    (IF (EQUAL (MIN (CAR DEGL1) (CAR DEGL2)) 1)
	(RETURN (LIST (CAR DEGL1) (CAR DEGL2) (CAR VARL))))))

(DEFUN LINHACKCONTENT (VAR POL NONLINDEG &AUX (NPOL POL) COEF GCD)
  (DO ((I NONLINDEG (f1- I)))
      ((f= I 0) (LIST (SETQ GCD (PGCD GCD NPOL)) (PQUOTIENT POL GCD)))
    (DESETQ (COEF . NPOL) (BOTHPRODCOEF (MAKE-POLY VAR I 1) NPOL)) 
    (UNLESS (PZEROP COEF)
	    (SETQ GCD (IF (NULL GCD) COEF (PGCD COEF GCD)))
	    (IF (EQUAL GCD 1) (RETURN (LIST 1 POL))))))

;;*** THIS IS THE REDUCED POLYNOMIAL REMAINDER SEQUENCE GCD (COLLINS')

(DEFUN OLDGCD (X Y &AUX U V S EGCD)			;only called from pgcda
  (DESETQ (X  U) (OLDCONTENT X))
  (DESETQ (Y  V) (OLDCONTENT Y))
  (SETQ EGCD (GCD (PGCDEXPON U) (PGCDEXPON V)))
  (IF (> EGCD 1)
      (SETQ U (PEXPON*// U EGCD NIL)
	    V (PEXPON*// V EGCD NIL)))
  (IF (f> (P-LE V) (P-LE U)) (EXCH U V))
  (SETQ S (CASE $GCD
	    ($RED (REDGCD U V))
	    ($SUBRES (SUBRESGCD U V))
	    (T (MERROR "Illegal GCD algorithm"))))
  (UNLESS (EQUAL S 1)
	  (SETQ S (PEXPON*// (PRIMPART
			      (IF $ALGEBRAIC S
				  (PQUOTIENT S (PQUOTIENT (P-LC S)
							  (PGCD (P-LC U)
								(P-LC V))))))
			     EGCD T)))
  (SETQ S (PTIMESCHK S (PGCD X Y)))
  (AND $ALGEBRAIC (NOT (PCOEFP (SETQ U (LEADALGCOEF S))))
       (NOT (EQUAL U S)) (SETQ S (ALGNORMAL S)))
  (COND (MODULUS (MONIZE S))
	((PMINUSP S) (PMINUS S)) 
	(T S)))

(DEFUN PGCDEXPON (P)
  (IF (PCOEFP P) 0
      (DO ((D (CADR P) (GCD D (CAR L)))
	   (L (CDDDR P) (CDDR L)))
	  ((OR (NULL L) (f= D 1)) D))))

(DEFUN PEXPON*// (P N *?)
  (IF (OR (PCOEFP P) (f= N 1)) P
      (DO ((ANS (LIST (CAR P))
		(CONS (CADR L)
		      (CONS (IF *? (f* (CAR L) N)
				(// (CAR L) N))
			    ANS)))
	   (L (CDR P) (CDDR L)))
	  ((NULL L) (NREVERSE ANS)))))

;;polynomial gcd using reduced prs

(defun redgcd (p q &aux (d 0))
  (sloop until (zerop (pdegree q (p-var p)))
	do (psetq p q
		  q (pquotientchk (prem p q) (pexpt (p-lc p) d))
		  d (f+ (p-le p) 1 (f- (p-le q))))
	finally (return (if (pzerop q) p 1))))

;;computes gcd's using subresultant prs TOMS Sept. 1978

(defun subresgcd (p q)					
  (sloop for g = 1 then (p-lc p)
	for h = 1 then (pquotient (pexpt g d) h^1-d)
	for d = (f- (p-le p) (p-le q))
	for h^1-d = (if (equal h 1) 1 (pexpt h (f1- d)))
        do (psetq p q
		  q (pquotientchk (prem p q) (ptimes g (ptimes h h^1-D))))
	if (zerop (pdegree q (p-var p))) return (if (pzerop q) p 1)))

;;*** THIS COMPUTES PSEUDO REMAINDERS

;(DECLARE (SPECIAL K LCU LCV) (FIXNUM K M I))

(DEFUN PSQUOREM1 (U V QUOP)
       (PROG (K (M 0) LCU LCV QUO LC)
	     (declare (special k lcu lcv) (fixnum #-cl k m))
	     (SETQ LCV (PT-LC V))
	     (SETQ K (f- (PT-LE U) (PT-LE V)))
	     (COND ((MINUSP K) (RETURN (LIST 1 '(0 0) U))))
	     (IF QUOP (SETQ LC (PEXPT (PT-LC V) (f1+ K))))
       A     (SETQ LCU (PMINUS (PT-LC U)))
             (IF QUOP (SETQ QUO (CONS (PTIMES (PT-LC U) (PEXPT (PT-LC V) K))
				      (CONS K QUO))))
	     (COND ((NULL (SETQ U (PGCD2 (PT-RED U) (PT-RED V))))
		    (RETURN (LIST LC (NREVERSE QUO) '(0 0))))
		   ((MINUSP (SETQ M (f- (PT-LE U) (PT-LE V))))
		    (SETQ U (COND ((ZEROP K) U)
				  (T (PCTIMES1 (PEXPT LCV K) U))))
		    (RETURN (LIST LC (NREVERSE QUO) U)))
		   ((f> (f1- K) M)
		    (SETQ U (PCTIMES1 (PEXPT LCV (f- (f1- K) M)) U))))
	     (SETQ K M)
	     (GO A)))

(DEFUN PREM (P Q)
  (COND ((PCOEFP P) (IF (PCOEFP Q) (CREMAINDER P Q) P))
	((PCOEFP Q) (PZERO))
	(T (PSIMP (P-VAR P) (PGCD1 (P-TERMS P) (P-TERMS Q))))))

(DEFMFUN PGCD1 (U V) (CADDR (PSQUOREM1 U V NIL)))

(DEFUN PGCD2 (U V &AUX (I 0))
  (declare (special k lcu lcv) (fixnum #-cl k i))
  (COND ((NULL U) (PCETIMES1 V K LCU))
	((NULL V) (PCTIMES1 LCV U))
	((ZEROP (SETQ I (f+ (PT-LE U) (f- K) (f- (CAR V)))))
	 (PCOEFADD (PT-LE U) (PPLUS (PTIMES LCV (PT-LC U))
				    (PTIMES LCU (PT-LC V)))
		   (PGCD2 (PT-RED U) (PT-RED V))))
	((MINUSP I)
	 (LIST* (f+ (PT-LE V) K) (PTIMES LCU (PT-LC V)) (PGCD2 U (PT-RED V))))
	(T (LIST* (PT-LE U) (PTIMES LCV (PT-LC U)) (PGCD2 (PT-RED U) V)))))

;(DECLARE (UNSPECIAL K LCU LCV) (NOTYPE K M I))
       
;;;*** OLDCONTENT REMOVES ALL BUT MAIN VARIABLE AND PUTS THAT IN CONTENT
;;;***  OLDCONTENT OF 3*A*X IS 3*A (WITH MAINVAR=X)

(DEFUN RCONTENT (P)	;RETURNS RAT-FORMS
       (LET ((Q (OLDCONTENTA P)))
	    (LIST (CONS Q 1) (COND ($ALGEBRAIC (RQUOTIENT P Q))
				   (T (CONS (PQUOTIENT P Q) 1))))))

(DEFUN OLDCONTENTA (X)
       (COND ((PCOEFP X) X)
	     (T (SETQ X (CONTSORT (CDR X)))
		(OLDCONTENT2 (CDR X) (CAR X)))))

(DEFMFUN OLDCONTENT (X)
       (COND ((PCOEFP X) (LIST X 1))
	     ((NULL (P-RED X))
	      (LIST (P-LC X) (MAKE-POLY (P-VAR X) (P-LE X) 1)))
	     (T (LET ((U (CONTSORT (CDR X))) V)
                (SETQ U (OLDCONTENT2 (CDR U) (CAR U))
                      V (COND ($ALGEBRAIC (CAR (RQUOTIENT X U)))
			      (T (PCQUOTIENT X U))))
                (COND ((PMINUSP V) (LIST (PMINUS U) (PMINUS V)))
                      (T (LIST U V)))))))

(DEFUN OLDCONTENT1 (X GCD) (COND ((EQUAL GCD 1) 1)
				 ((NULL X) GCD)
				 (T (OLDCONTENT2 (CONTSORT X) GCD))))

(DEFUN OLDCONTENT2 (X GCD)
       (do ((x x (cdr x))
	    (gcd gcd (pgcd (car x) gcd)))
	   ((or (null x) (equal gcd 1)) gcd)))

(DEFUN CONTSORT (X)
       (SETQ X (COEFL X))
       (COND ((zl-MEMBER 1 X)'(1))
	     ((NULL (CDR X))X)
	     (T (SORT X (FUNCTION CONTODR)))))

(DEFUN COEFL (X)
       (do ((x x (cddr x))
	    (ans nil (cons (cadr x) ans)))
	   ((null x) ans)))

(DEFUN CONTODR (A B)
       (COND ((PCOEFP A) T)
	     ((PCOEFP B) NIL)
	     ((EQ (CAR A) (CAR B)) (NOT (f> (CADR A) (CADR B))))
	     (T (POINTERGP (CAR B)(CAR A)))))

;;;*** PCONTENT COMPUTES INTEGER CONTENT
;;;*** PCONTENT OF 3*A*X IS 3 IF MODULUS = NIL  1 OTHERWISE

(DEFUN PCONTENT (X)
       (COND ((PCOEFP X) (LIST X 1))
	     (T (LET ((U (PCONTENTZ X)))
		  (IF (EQN U 1) (LIST 1 X)
		      (LIST U (PCQUOTIENT X U)))))))

(DEFUN PCONTENT1 (X GCD)
       (DO ((X X (CDDR X))
	    (GCD GCD (CGCD GCD (PCONTENTZ (CADR X)))))
	   ((OR (NULL X) (EQUAL GCD 1)) GCD)))

(DEFUN PCONTENTZ (P)
       (COND ((PCOEFP P) P)
	     (T (PCONTENT1 (P-RED P) (PCONTENTZ (P-LC P))))))

(DEFUN UCONTENT (P)					;CONTENT OF UNIV. POLY
  (COND ((PCOEFP P) (ABS P))
	(T (SETQ P (MAPCAR #'ABS (COEFL (CDR P))))
	   (LET ((M (APPLY #'MIN P)))
		(OLDCONTENT2 (zl-DELETE M P) M)))))

;;***	PGCDU CORRESPONDS TO BROWN'S ALGORITHM U

;;;PGCDU IS NOT NOW IN RAT;UFACT >

(DEFMFUN PGCDU (P Q)
  (DO () ((PZEROP Q) (MONIZE P))
    (PSETQ P Q Q (PMODREM P Q))))

;(DECLARE (SPECIAL K Q* QUO) (FIXNUM K))

(DEFUN PMODREM (X Y)
  (COND ((NULL MODULUS)
	 (MERROR "Illegal use of PMODREM"))
	((PACOEFP Y) (IF (PZEROP Y) X 0))
	((PACOEFP X) X)
	((EQ (P-VAR X) (P-VAR Y))
	 (PSIMP (CAR X) (PGCDU1 (P-TERMS X) (P-TERMS Y) NIL)))
	(T (MERROR "Illegal use of PMODREM"))))

(DEFUN PMODQUO (U V &AUX QUO)
  (declare (special quo))
  (COND ((NULL MODULUS)
	 (MERROR "Illegal use of PMODQUO"))
	((PCOEFP V) (CONS (PTIMES (CRECIP V) U) 0))
	((ALG V) (CONS (PTIMES (PAINVMOD V) U) 0))
	((PACOEFP U) (CONS 0 U))
	((NOT (EQ (P-VAR U) (P-VAR V)))
	 (MERROR "Illegal use of PMODQUO"))
	(T (XCONS (PSIMP (CAR U) (PGCDU1 (CDR U) (CDR V) T))
		  (PSIMP (CAR U) QUO)))))

(COMMENT (DEFUN PMODREM (X Y) (COND ((NULL MODULUS)
			      (MERROR "Illegal use of PMODREM"))
			     ((PACOEFP Y) 0)
			     ((PACOEFP X) X)
			     ((EQ (CAR X) (CAR Y))
			      (DPDISREP
			       (DPMODREM (DPREP X) (DPREP Y))))
			     (T (MERROR "Illegal use of PMODREM")))))

(DEFUN PGCDU1 (U V PQUO*)
  (let ((invv (PAINVMOD (PT-LC V))) (k 0) q*)
    (declare (special k quo q*) (fixnum k))
    (SLOOP UNTIL (MINUSP (setq K (f- (PT-LE U) (PT-LE V))))
	  DO (SETQ Q* (PTIMES INVV (PT-LC U)))
	  IF PQUO* DO (SETQ QUO (NCONC QUO (LIST K Q*)))
	  WHEN (PTZEROP (SETQ U (PQUOTIENT2 (PT-RED U) (PT-RED V))))
	   RETURN (PTZERO)
	  FINALLY (RETURN U))))

;(DECLARE (UNSPECIAL K Q* QUO) (NOTYPE K))


(DEFUN NEWPRIME (P)
  (declare (special bigprimes))		;defined later on
  (COND ((NULL P) (CAR BIGPRIMES))
	(T (DO ((PL BIGPRIMES (CDR PL)))
	       ((NULL PL) (SETQ P (FNEWPRIME P))
			  (SETQ BIGPRIMES (NCONC BIGPRIMES (LIST P)))
			  P)
	     (IF (f< (CAR PL) P) (RETURN (CAR PL)))))))

(DEFUN FNEWPRIME (P)	; Finds biggest prime less than fixnum P.
  (DO ((PP (IF (ODDP P) (f- P 2) (f- P 1)) (f- PP 2))) ((f< PP 0))
      (IF (PRIMEP PP) (RETURN PP))))

(DEFUN PRIMEP (P)
  (AND (OR (LESSP P 14.)
	   (LET ((MODULUS P))
		(AND (EQUAL 1 (CEXPT 13. (SUB1 P))) (EQUAL 1 (CEXPT 3 (SUB1 P))))))
       (NULL (CDDR (SETQ P (CFACTORW P))))
       (= 1 (CADR P))))

;; #O <form> reads <form> in octal (base 8)


(DEFVAR BIGPRIMES NIL)
#+CL
(eval-when (load )
 
;; it is convenient to have the bigprimes be actually less than
;; half the size of the most positive fixnum, so that arithmetic is
;; easier
#.
(case most-positive-fixnum
  (2147483647
    '(setq bigprimes
	   '(1073741789 1073741783 1073741741 1073741723 1073741719 1073741717
 1073741689 1073741671 1073741663 1073741651 1073741621 1073741567
 1073741561 1073741527 1073741503 1073741477 1073741467 1073741441
 1073741419 1073741399)
))
  ;; Could always use the following, but it takes several seconds to compute
  ;; so if we want to autoload this file, it is tiresome.
  (t '(DO ((I 0 (f1+ I))				;GENERATES 20 LARGEST
     (P (quotient most-positive-fixnum 2) (NEWPRIME P)))		;PRIMES < WORD
    ((= I 20.)))))

(setq *ALPHA (CAR BIGPRIMES))
)

(DEFMVAR *ALPHA (CAR BIGPRIMES))

#+MacLisp 
(DEFMVAR BIGPRIMES
      '(#O 377777777741 #O 377777777717 #O 377777777703 #O 377777777673
	#O 377777777661 #O 377777777607 #O 377777777563 #O 377777777411
	#O 377777777313 #O 377777777273 #O 377777777233 #O 377777777075
	#O 377777777015 #O 377777776771 #O 377777776755 #O 377777776735
	#O 377777776725 #O 377777776677 #O 377777776661 #O 377777776653))


;;; list of primes less than 2^30 -1  (limit of smallnums in Franz/vax)
#+Franz 
(defmvar bigprimes '(1073741789. 1073741783. 1073741741. 1073741723. 
		  1073741719. 1073741717. 1073741689. 1073741671.
		  1073741663. 1073741651. 1073741621. 1073741567. 
		  1073741561. 1073741527. 1073741503. 1073741477.
		  1073741467. 1073741441. 1073741419. 1073741399.))

#+NIL
(PROGN 'COMPILE
;;; It takes a lot longer to compute the 35 bit primes for the PDP-10,
;;; thats why they are wired-in there.
(DEFMVAR BIGPRIMES
  '(#o3777777775 #o3777777737 #o3777777725 #o3777777701 #o3777777667
    #o3777777665 #o3777777643 #o3777777635 #o3777777607 #o3777777573
    #o3777777557 #o3777777527 #o3777777511 #o3777777503 #o3777777475
    #o3777777455 #o3777777433 #o3777777401 #o3777777361 #o3777777343))

(DEFUN BIGPRIMES ()
  (SETQ BIGPRIMES ())
  (DO ((I 0 (f1+ I)) (P MOST-POSITIVE-FIXNUM (NEWPRIME P)))
      ((= I 20.))))
;; wire it in for now. PRIMEP is losing. (BIGPRIMES)
)

;#+CL
;(DO ((I 0 (f1+ I))				;GENERATES 20 LARGEST
;	     (P (LSH -1 -1) (NEWPRIME P)))		;PRIMES < WORD
;	    ((= I 20.)))



(DEFMFUN $PRIMEP (P)
 (IF (NOT (INTEGERP P)) (MERROR "Argument to PRIMEP must be an integer:~%~M" P))
 (LET ($INTFACLIM) (PRIMEP (ABS P))))



(DEFUN LEADCOEFFICIENT (P) (IF (PCOEFP P) P (LEADCOEFFICIENT (CADDR P))))

(DEFUN MAXCOEFFICIENT (P) (IF (PCOEFP P) (ABS P) (MAXCOEF1 (CDR P))))

(DEFUN MAXCOEF1 (P)
  (IF (NULL P) 0 (MAX (MAXCOEFFICIENT (CADR P)) (MAXCOEF1 (CDDR P)))))

(DEFUN MAXNORM (POLY)
  (IF (NULL POLY) 0 (MAX (NORM (CADR POLY)) (MAXNORM (CDDR POLY)))))

(DEFUN NORM (POLY)
       (COND ((NULL POLY) 0)
	     ((PCOEFP POLY) (ABS POLY))
	     (T (PLUS (NORM (CADDR POLY)) (NORM1 (CDDDR POLY)) )) ))

(DEFUN NORM1 (POLY)
  (IF (NULL POLY) 0 (PLUS (NORM (CADR POLY)) (NORM1 (CDDR POLY)) )) )

(DEFMFUN PDEGREE (P VAR)
  (COND ((PCOEFP P) 0)
	((EQ VAR (P-VAR P)) (P-LE P))
	((POINTERGP VAR (P-VAR P)) 0)
	(T (DO ((L (P-RED P) (PT-RED L))
		(E (PDEGREE (P-LC P) VAR) (MAX E (PDEGREE (PT-LC L) VAR))))
	       ((NULL L) E)))))

(DEFUN POLY-IN-VAR (P V)
  (COND ((OR (PCOEFP P) (POINTERGP V (P-VAR P))) (LIST 0 P))
	((EQ (P-VAR P) V) (P-TERMS P))
	((SLOOP WITH ANS
	       FOR (EXP COEF) ON (P-TERMS P) BY 'PT-RED
	       DO (SETQ ANS (PPLUS1 ANS
				    (EVERYSUBST2 (POLY-IN-VAR COEF V)
						 (LIST (P-VAR P) EXP 1))))
	       FINALLY (RETURN ANS)))))

(DEFUN UNIVAR (X) (OR (NULL X) (AND (PCOEFP (PT-LC X)) (UNIVAR (PT-RED X)))))

;;**THE CHINESE REMAINDER ALGORITHM IS A SPECIAL CASE OF LAGRANGE INTERPOLATION

(DEFUN LAGRANGE3 (U UK P QK)
       (SETQMODULUS P)
       (SETQ UK (PDIFFERENCE UK (PMOD U)))
       (COND ((PZEROP UK) (SETQ MODULUS NIL) U)
	     (T (SETQ UK (PCTIMES (CRECIP (CMOD QK)) UK))
		(SETQ MODULUS NIL)
		(PPLUS U (PCTIMES QK UK)))))


(DEFUN LAGRANGE33 (U UK QK XK)
  (DECLARE (SPECIAL XV))
  (SETQ UK (PDIFFERENCE UK (PCSUBST U XK XV)))
  (COND ((PZEROP UK) U)
	(T (PPLUS U (PTIMES
		     (PCTIMES (CRECIP (PCSUBST QK XK XV)) UK)
		     QK)))))


;;;*************************************************************
 
;;	THIS IS THE END THE NEW RATIONAL FUNCTION PACKAGE PART 3.
;;	IT INCLUDES THE GCD ROUTINES, THEIR SUPPORTING FUNCTIONS
