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
(macsyma-module result)

(DECLARE-TOP(SPECIAL VARLIST GENVAR $RATFAC $KEEPFLOAT MODULUS *ALPHA XV))

(LOAD-MACSYMA-MACROS RATMAC)

(DECLARE-TOP(SPLITFILE MRESUL))

(DEFMFUN $POLY_DISCRIMINANT (POLY VAR)
  (LET* ((VARLIST (LIST VAR))
	 (GENVAR ()) 
	 (RFORM (RFORM POLY))
	 (RVAR (CAR (LAST GENVAR)))
	 (N (PDEGREE (SETQ POLY (CAR RFORM)) RVAR)))
	
    (COND ((= N 1) 1)
	  ((OR (= N 0) (NOT (atom  (CDR RFORM))))
	   (MERROR "ARG. MUST BE A POLYNOMIAL IN VAR"))
	  (T (PDIS (PRESIGN
		    (// (f* N (f1- N)) 2)
		    (PQUOTIENT (RESULTANT POLY (PDERIVATIVE POLY RVAR))
			       (P-LC POLY))))))))

(DEFMFUN $RESULTANT (A B MAINVAR)
       (PROG (VARLIST FORMFLAG $RATFAC RES ANS GENVAR $KEEPFLOAT)
	     (SETQ VARLIST (LIST MAINVAR) $RATFAC T ANS 1)
	     (AND ($RATP A)(setq formflag t)(SETQ a ($ratdisrep a)))
	     (AND ($RATP B)(setq formflag t)(SETQ b ($ratdisrep b)))
	     (NEWVAR A)
	     (NEWVAR B)
	     (SETQ A (LMAKE2 (CADR (RATREP* A)) NIL))
	     (SETQ B (LMAKE2 (CADR (RATREP* B)) NIL))
	     (SETQ MAINVAR (CAADR (RATREP* MAINVAR)))
       (DO ((L1 A (CDR L1))) ((NULL L1))
	   (DO ((L2 B (CDR L2))) ((NULL L2))
	       (SETQ RES (RESULT1 (CAAR L1) (CAAR L2) MAINVAR))
	       (SETQ ANS (PTIMES ANS (PEXPT 
		 (COND ((ZEROP (CADDR RES)) (CAR RES))
		       (T (PTIMESCHK (CAR RES)
				     (PEXPT (MAKPROD (CADR RES) NIL)
					    (CADDR RES)))))
		 (TIMES (CDAR L1) (CDAR L2)))))))
	(RETURN (COND (FORMFLAG (PDIS* ANS)) (T (PDIS ANS))))))

(DEFUN RESULT1 (P1 P2 VAR)
       (COND ((OR (PCOEFP P1) (POINTERGP VAR (CAR P1)))
	      (LIST 1 P1 (pdegree P2 VAR)))
	     ((OR (PCOEFP P2) (POINTERGP VAR (CAR P2)))
	      (LIST 1 P2 (pdegree P1 VAR)))
	     ((NULL (CDDDR P1))
	      (COND ((NULL (CDDDR P2)) (LIST 0 0 1))
		    (T (LIST (PEXPT (CADDR P1) (CADR P2))
			     (PCSUBSTY 0 VAR P2)
			     (CADR P1)))))
	     ((NULL (CDDDR P2))
	      (LIST (PEXPT (CADDR P2) (CADR P1))
		    (PCSUBSTY 0 VAR P1)
		    (CADR P2)))
	     ((> (SETQ VAR (GCD (PGCDEXPON P1) (PGCDEXPON P2))) 1)
	      (LIST 1 (RESULTANT (PEXPON*// P1 VAR NIL)
				 (PEXPON*// P2 VAR NIL)) VAR))
	     (T (LIST 1 (RESULTANT P1 P2) 1))))

(DEFMVAR $RESULTANT '$SUBRES "Designates which resultant algorithm")

(DEFVAR *resultlist '($subres $mod $red))

(DEFMFUN RESULTANT (P1 P2)				;assumes same main var
  (IF (> (P-LE P2) (P-LE P1))
      (PRESIGN (f* (P-LE P1) (P-LE P2)) (RESULTANT P2 P1)) 
      (CASE $RESULTANT
	($SUBRES (SUBRESULT P1 P2))
	#+broken ($MOD (MODRESULT P1 P2))
	($RED (REDRESULT P1 P2))
	(T (MERROR "No such resultant algorithm")))))

(DEFUN PRESIGN (N P)
  (IF (ODDP N) (PMINUS P) P))

(declare-top (SPLITFILE SUBRES))
;computes resultant using subresultant p.r.s. TOMS Sept. 1978

(defun subresult (p q)
  (sloop for g = 1 then (p-lc p)
	for h = 1 then (pquotient (pexpt g d) h^1-d)
	for degq = (pdegree q (p-var p))
	for d = (f- (p-le p) degq)
	for h^1-d = (if (equal h 1) 1 (pexpt h (f1- d)))
	if (zerop degq) return (if (pzerop q) q (pquotient (pexpt q d) h^1-d))
	do (psetq p q
		  q (presign (f1+ d) (pquotient (prem p q)
					       (ptimes g (ptimes h h^1-d)))))))

(DECLARE-TOP (SPLITFILE REDRES))

;	PACKAGE FOR CALCULATING MULTIVARIATE POLYNOMIAL RESULTANTS 
;	USING MODIFIED REDUCED P.R.S.

(DEFUN REDRESULT (U V)
   (PROG (A R SIGMA C)
	 (SETQ A 1)
	 (SETQ SIGMA 0)
	 (SETQ C 1)
    A    (IF (PZEROP (SETQ R (PREM U V))) (RETURN (PZERO)))
	 (SETQ C (PTIMESCHK C (PEXPT (P-LC V)
				     (f* (f- (P-LE U) (P-LE V))
					(f- (P-LE V) (PDEGREE R (P-VAR U))
					   1)))))
	 (SETQ SIGMA (f+ SIGMA (f* (P-LE U) (P-LE V))))
	 (IF (ZEROP (PDEGREE R (P-VAR U)))
	     (RETURN
	      (PRESIGN SIGMA
		       (PQUOTIENT (PEXPT (PQUOTIENTCHK R A) (P-LE V)) C))))
	 (PSETQ U V
		V (PQUOTIENTCHK R A)
		A (PEXPT (P-LC V) (f+ (P-LE U) 1 (f- (P-LE V)))))
	 (GO A)))


(declare-top (SPLITFILE MODRES))

;	PACKAGE FOR CALCULATING MULTIVARIATE POLYNOMIAL RESULTANTS 
;	USING MODULAR AND EVALUATION HOMOMORPHISMS.
; modresultant fails on the following example
;RESULTANT(((-4)*Z)^4+(Y+8*Z)^4+(X-5*Z)^4-1,
;	       ((-4)*Z)^4-(X-5*Z)^3*((-4)*Z)^3+(Y+8*Z)^3*((-4)*Z)^2
;			 +(-2)*(Y+8*Z)^4+((-4)*Z)^4+1,Z)

#+broken
(progn
(DEFUN MODRESULT (A B)
       (MODRESULT1 A B (SORT (UNION* (LISTOVARS A) (LISTOVARS B))
			     (FUNCTION POINTERGP))))

(DEFUN MODRESULT1 (X Y VARL)
       (COND ((NULL MODULUS) (PRES X Y (CAR VARL) (CDR VARL)))
	     (T (CPRES X Y (CAR VARL) (CDR VARL))) ))

(DEFUN PRES (A B XR1 VARL)
       (PROG (M N F A* B* C* P Q C MODULUS HMODULUS)
	(SETQ M (CADR A))
	(SETQ N (CADR B))
	(SETQ F (COEFBOUND M N (MAXNORM (CDR A)) (MAXNORM (CDR B)) ))
	(SETQ Q 1)
	(SETQ C 0)
	(SETQ P *ALPHA)
	(GO STEP3)
STEP2	(SETQ P (NEWPRIME P))
STEP3	(SETQMODULUS P)
	(SETQ A* (PMOD A))
	(SETQ B* (PMOD B))
	(COND ((OR (REJECT A* M XR1) (REJECT B* N XR1)) (GO STEP2)))
	(SETQ C* (CPRES A* B* XR1 VARL))
	(SETQMODULUS NIL)
	(SETQ C (LAGRANGE3 C C* P Q))
	(SETQ Q (TIMES P Q))
	(COND ((GREATERP Q F) (RETURN C))
	      (T (GO STEP2)) ) ))

(DEFUN REJECT (A M XV)
       (NOT (EQN (PDEGREE A XV) M)))

(DEFUN COEFBOUND (M N D E)
       (TIMES 2 (EXPT (f1+ M) (// N 2))
	      (EXPT (f1+ N) (// M 2))
	      (COND ((ODDP N) (f1+ ($ISQRT (f1+ M))))
		    (T 1))
	      (COND ((ODDP M) (f1+ ($ISQRT (f1+ N))))
		    (T 1))
; (FACTORIAL (PLUS M N)) USED TO REPLACE PREV. 4 LINES. KNU II P. 375
	      (EXPT D N)
	      (EXPT E M) ))

(DEFUN MAIN2 (A VAR EXP TOT)
       (COND ((NULL A) (CONS EXP TOT))
	     (T (MAIN2 (CDDR A) VAR
		       (MAX (SETQ VAR (PDEGREE (CADR A) VAR)) EXP)
		       (MAX (f+ (CAR A) VAR) TOT))) ))

(DEFUN CPRES (A B XR1 VARL)				;XR1 IS MAIN VAR WHICH
       (COND ((NULL VARL) (CPRES1 (CDR A) (CDR B)))	;RESULTANT ELIMINATES
	     (T	(PROG (  M2 		  ( M1 (CADR A))
		  ( N1 (CADR B))  N2 (K 0) C D A* B* C* BP XV);XV IS INTERPOLATED VAR
		      (DECLARE (FIXNUM M1 N1 K))

	        STEP2
		  (SETQ XV (CAR VARL))
		  (SETQ VARL (CDR VARL))
		  (SETQ M2 (MAIN2 (CDR A) XV 0 0))	;<XV DEG . TOTAL DEG>
		  (SETQ N2 (MAIN2 (CDR B) XV 0 0))
		  (COND ((ZEROP (f+ (CAR M2) (CAR N2)))
			 (COND ((NULL VARL) (RETURN (CPRES1 (CDR A) (CDR B))))
			       (T (GO STEP2)) ) ))
		  (SETQ K (f1+ (MIN (f+ (f* M1 (CAR N2)) (f* N1 (CAR M2)))
				   (f+ (f* M1 (CDR N2)) (f* N1 (CDR M2))
				      (f- (f* M1 N1))) )))
		  (SETQ C 0)
		  (SETQ D 1)
		  (SETQ M2 (CAR M2) N2 (CAR N2))
		  (SETQ BP (MINUS 1))
	       STEP3
	          (COND ((EQUAL (SETQ BP (ADD1 BP)) MODULUS)
			 (merror "Resultant primes too small."))
			((ZEROP M2) (SETQ A* A))
			(T (SETQ A* (PCSUBST A BP XV))
			   (COND ((REJECT A* M1 XR1)(GO STEP3)) )) )
		  (COND ((ZEROP N2) (SETQ B* B))
			(T (SETQ B* (PCSUBST B BP XV))
			   (COND ((REJECT B* N1 XR1) (GO STEP3))) ))
		  (SETQ C* (CPRES A* B* XR1 VARL))
		  (SETQ C (LAGRANGE33 C C* D BP))
		  (SETQ D (PTIMESCHK D (LIST XV 1 1 0 (CMINUS BP))))
		  (COND ((> (CADR D) K) (RETURN C))
			(T (GO STEP3))) )) ))
)

(declare-top (SPLITFILE BEZOUT))

;; *** NOTE THAT MATRIX PRODUCED IS ALWAYS SYMETRIC
;; *** ABOUT THE MINOR DIAGONAL.

(DEFMFUN $BEZOUT (P Q VAR)
       (LET ((VARLIST (LIST VAR)) GENVAR)
	    (NEWVAR P)
	    (NEWVAR Q)
	    (SETQ P (CADR (RATREP* P))
		  Q (CADR (RATREP* Q)))
	    (SETQ P (COND ((> (CADR Q) (CADR P)) (BEZOUT Q P))
			  (T (BEZOUT P Q))))
	    (CONS '($MATRIX)
		  (MAPCAR #'(LAMBDA (L) (CONS '(MLIST) (MAPCAR 'PDIS L)))
			  P))))

(DEFUN VMAKE (POLY N *L)
       (DO ((I (f1- N) (f1- I))) ((MINUSP I))
	   (COND ((OR (NULL POLY) (< (CAR POLY) I))
		  (SETQ *L (CONS 0 *L)))
		 (T (SETQ *L (CONS (CADR POLY) *L))
		    (SETQ POLY (CDDR POLY)))))
       (NREVERSE *L))

(DEFUN BEZOUT (P Q)
  (LET* ((N (f1+ (P-LE P)))
	 (N2 (f- N (P-LE Q)))
	 (A (VMAKE (P-TERMS P) N NIL))
	 (B (VMAKE (P-TERMS Q) N NIL))
	 (AR (REVERSE (NTHCDR N2 A)))
	 (BR (REVERSE (NTHCDR N2 B)))
	 (L (NZEROS N NIL)))
    (RPLACD (NTHCDR (f1- (P-LE P)) A) NIL)
    (RPLACD (NTHCDR (f1- (P-LE P)) B) NIL)
    (NCONC
     (MAPCAR
      #'(LAMBDA (AR BR)
	  (SETQ L (MAPCAR #'(LAMBDA (A B L)
			      (PPLUSCHK L (PDIFFERENCE
					   (PTIMES BR A) (PTIMES AR B))))
			  A B (CONS 0 L))))
      AR BR)
     (AND (PZEROP (CAR B))
	  (DO ((B (VMAKE (CDR Q) (CADR P) NIL) (ROT* B))
	       (M NIL (CONS B M)))
	      ((NOT (PZEROP (CAR B))) (CONS B M))))) ))

(DEFUN ROT* (B)
  (SETQ B (COPY1 B)) (PROG2 (NCONC B B) (CDR B) (RPLACD B NIL)))


(DEFUN PPLUSCHK (P Q) (COND ((PZEROP P) Q) (T (PPLUS P Q))))
