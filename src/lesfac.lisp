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
(macsyma-module lesfac)

(DECLARE-top (GENPREFIX LSF)
	 (SPECIAL *FNEWVARSW FACLIST RES POLY*))

(LOAD-MACSYMA-MACROS RZMAC RATMAC)

(DEFUN NEWSYM2 (P E &AUX (G (GENSYM)))
  (PUTPROP G E 'DISREP)
  (VALPUT G (SUB1 (VALGET (CAR GENVAR))))
  (SETQ GENVAR (CONS G GENVAR))
  (SETQ VARLIST (CONS E VARLIST))
  (PUTPROP G P 'UNHACKED)
  G) 

(DEFUN GETUNHACK (GEN) (OR (GET GEN 'UNHACKED) (PGET GEN))) 

(DEFMACRO GETDIS (X) `(GET ,X 'DISREP))
(DEFMACRO CONS1 (X) `(CONS ,X 1))

(DEFUN FRPOLY? (R) (EQUAL 1 (CDR R)))

(defmacro setcall (&rest l)
  (setq l (cons 'setcall l))
       (SUBLIS (LIST (CONS 'FNCALL (CDR L))
		     (CONS 'A (CADDR L))
		     (CONS 'B (CADDDR L)))
	       '(PROG1 (CAR (SETQ A FNCALL)) (SETQ B (CADDR A) A (CADR A)))))

(DEFUN PQUOCOF (P Q) 
       ((LAMBDA (QQ) (COND (QQ (LIST Q QQ 1.)) ((LIST 1. P Q))))
	(TESTDIVIDE P Q))) 

(DEFUN POLYST (A) 
       (COND ((PCOEFP A) (LIST A))
	     (T (CONS (CONS (CAR A) (CADR A)) (POLYST (CADDR A)))))) 

(DEFUN CDINF (A B BOTH) 
       (COND ((OR (PCOEFP A) (PCOEFP B)) (LIST 1. A B))
	     (T (SETQ A (NCONS (COPY A))
		      B (NCONS (COND (BOTH (COPY B))(T B))))
		(LIST (CD1 A B BOTH) (CAR A) (CAR B))))) 

(DEFUN CD1 (A B BOTH) 
       (COND ((OR (PCOEFP (CAR A)) (PCOEFP (CAR B))) 1.)
	     ((EQ (CAAR A) (CAAR B))
	      (PTIMES (PEXPT (PGET (CAAR A))		;CHECK FOR ALG. TRUNC.
		    (PROG1 (COND (BOTH (f+ (CADAR A) (CADAR B))) (T (CADAR A)))
			   (RPLACA A (CADDAR A))
			   (COND (BOTH (RPLACA B (CADDAR B)))
				 (T (SETQ B (CDDAR B))))))
		    (CD1 A B BOTH)))
	     ((POINTERGP (CAAR A) (CAAR B)) (CD1 (CDDAR A) B BOTH))
	     (T (CD1 A (CDDAR B) BOTH)))) 

(DEFUN LMAKE (P L) 
       (COND ((PCOEFP P) (CONS P L))
	     ((GET (CAR P) 'UNHACKED)
	      (LMAKE (CADDR P) (CONS (CONS (CAR P) (CADR P)) L)))
	     (T (SETQ L (LMAKE (CADDR P) L))
		(RPLACA L (LIST (CAR P) (CADR P) (CAR L))))))

(DEFUN LMAKE2 (P L)
       (SETQ L (LMAKE P L))
       (MAPC (FUNCTION (LAMBDA (X) (RPLACA X (GETUNHACK (CAR X)))))
	     (CDR L))
       (cond ((equal (car l) 1) (cdr l))
	     (t (RPLACA L (CONS (CAR L) 1)))))
 

(DEFUN PMAKE (L) 
       (COND ((NULL L) 1.)
	     ((= 0. (CDAR L)) (PMAKE (CDR L)))
	     ((NUMBERP (CAAR L))	;CLAUSE SHOULD BE ELIMINATED ASAP
	      (PTIMES (CEXPT (CAAR L) (CDAR L)) (PMAKE (CDR L))))
	     (T (PTIMES (LIST (CAAR L) (CDAR L) 1.) (PMAKE (CDR L)))))) 

(DEFUN FACMGCD (PL)            ;GCD OF POLY LIST FOR EZGCD WITH RATFAC
       (DO ((L (CDR PL) (CDR L))
	    (ANS NIL (CONS (CADDR GCD) ANS))
	    (GCD (CAR PL) (CAR GCD)))
	   ((NULL L) (CONS GCD (NREVERSE ANS)))
	   (SETQ GCD (FPGCDCO GCD (CAR L)))
	   (COND ((EQUAL (CAR GCD) 1) (RETURN (CONS 1 PL)))
		 ((NULL ANS) (SETQ ANS (LIST (CADR GCD))))
		 ((NOT (EQUAL (CADR GCD) 1))
		  (DO ((L2 ANS (CDR L2))) ((NULL L2))
		      (RPLACA L2 (PTIMES (CADR GCD) (CAR L2))))))))
			     

(DEFUN FPGCDCO (P Q) ((LAMBDA ($RATFAC GCDL)    ;FACTORED PGCDCOFACTS
       (COND ((OR (PCOEFP P) (PCOEFP Q)) (PGCDCOFACTS P Q))
	     (T (LIST (PTIMESCHK 
		       (SETCALL PGCDCOFACTS P Q)
		       (CAR (SETQ P (LMAKE P NIL)
				  Q (LMAKE Q NIL)
				  GCDL (MAPCAR 'PMAKE (LGCD1 (CDR P) (CDR Q)) ))))
		      (PTIMESCHK (CAR P) (CADR GCDL))
		      (PTIMESCHK (CAR Q) (CADDR GCDL))))))
		      NIL NIL) ) 

;;	NOTE: ITEMS ON VARLIST ARE POS. NORMAL
;;	INTEGER COEF GCD=1 AND LEADCOEF. IS POS.

(DEFUN LGCD1 (A B) 
       (PROG (PTLIST G BJ C T1 D1 D2) 
	     (SETQ PTLIST (MAPCAR #'(LAMBDA (X)
					   X ;Ignored.
					   B) A))
	     (DO ((A A (CDR A)) (PTLIST PTLIST (CDR PTLIST)))
		 ((NULL A))
		 (DO ((AI (GETUNHACK (CAAR A)))
		      (B (CAR PTLIST) (CDR B)))
		     ((NULL B))
		     (AND (ZEROP (CDAR B)) (GO NEXTB))
		     (SETQ D1 1 D2 1)
		     (SETQ BJ (GETUNHACK (CAAR B)))
		     (SETQ C (COND ((PIRREDP (CAAR A))
				    (COND ((PIRREDP (CAAR B)) 1.)
					  (T (SETCALL PQUOCOF BJ AI))))
				   ((PIRREDP (CAAR B)) (SETCALL PQUOCOF AI BJ))
				   (T (SETCALL PGCDCOFACTS AI BJ))))
		     (COND ((EQUAL C 1.) (GO NEXTB))
			   ((EQUAL AI 1) (GO BLOOP)))
		     ALOOP
		     (COND ((SETQ T1 (TESTDIVIDE AI C))
			    (SETQ AI T1 D1 (f1+ D1))
			    (GO ALOOP)))
		     BLOOP
		     (AND (= D1 1.)
			  (NOT (EQUAL BJ 1))
			  (DO ((T1
			      (TESTDIVIDE BJ C)
			      (TESTDIVIDE BJ C)))
			      ((NULL T1))
			      (SETQ BJ T1 D2 (f1+ D2))))
		     (SETQ G (CONS (CONS (MAKPRODG C T)
					 (MIN (SETQ D1 (f* D1 (CDAR A)))
					      (SETQ D2 (f* D2 (CDAR B)))))
				   G))
		     (COND ((> D1 (CDAR G))
			    (RPLACD (LAST A)
				    (NCONS (CONS (CAAR G) (f- D1 (CDAR G)))))
			    (RPLACD (LAST PTLIST) (NCONS (CDR B)))))
		     (COND ((> D2 (CDAR G))
			    (RPLACD (LAST B)
				    (NCONS (CONS (CAAR G) (f- D2 (CDAR G)))))))
		     (RPLACA (CAR A) (MAKPRODG AI T))
		     (RPLACA (CAR B) (MAKPRODG BJ T))
		     (AND (EQUAL BJ 1) (RPLACD (CAR B) 0))
		     (AND (EQUAL AI 1.) (RPLACD (CAR A) 0) (RETURN NIL))
		     NEXTB))
	     (RETURN (LIST G A B)))) 

(DEFUN MAKPRODG (P SW)
	(COND ((PCOEFP P) P)
		(T (CAR (MAKPROD P SW)))))




(DEFUN DOPGCDCOFACTS (X Y)
       (let (($GCD
	$GCD)( $RATFAC NIL)) (OR (MEMQ $GCD *GCDL*) (SETQ $GCD '$EZ))
		(PGCDCOFACTS X Y)))

(DEFUN FACRPLUS (X Y) 
       ((LAMBDA (A B C D)
		(SETQ X (SETCALL DOPGCDCOFACTS A C) 
		      Y (SETCALL FPGCDCO B D))
		(SETQ A (MAKPROD
			 (PPLUS (PFLATTEN (PTIMESCHK A D))
				(PFLATTEN (PTIMESCHK B C))) NIL))
		(SETQ B (PTIMESCHK B D))
		(COND ($ALGEBRAIC (SETQ Y (PTIMESCHK Y B))
				  (SETCALL FPGCDCO Y A) ;for unexpected gcd
				  (CONS (PTIMES X A) Y))
		      (T (SETQ C (SETCALL CDINF Y B NIL))
			 (SETCALL FPGCDCO Y A)
			 (CONS (PTIMES X A) (PTIMESCHK Y (PTIMESCHK C B))))))
	(CAR X)
	(CDR X)
	(CAR Y)
	(CDR Y)))

(DEFUN MFACPPLUS (L)
       (let (($gcd
	(or $gcd '$ez))( $ratfac nil)( g nil))
		(setq g (OLDCONTENT2 (SORT (COPY1 L) 'CONTODR) 0))
		(COND ((PZEROP G) G)
		      ((DO ((A (PFLATTEN (PQUOTIENT (CAR L) G))
			       (PPLUS A (PFLATTEN (PQUOTIENT (CAR LL) G))))
			    (LL (CDR L) (CDR LL)))
			   ((NULL LL) (PTIMES G (MAKPROD A NIL))))))))

;; no longer called
;;(DEFUN FACPPLUS (P Q)
;;	(PTIMES (SETCALL DOPGCDCOFACTS P Q)
;;		(MAKPROD (PPLUS (PFLATTEN P) (PFLATTEN Q)) NIL)))

(DEFUN  FACRTIMES (X Y GCDSW) 
       (COND ((NOT GCDSW)
	      (CONS (PTIMES (CAR X) (CAR Y)) (PTIMESCHK (CDR X) (CDR Y))))
	     (T (let ((G
		 (CDINF (CAR X) (CAR Y) T))( H
		 (CDINF (CDR X) (CDR Y) T))) (SETQ X (FPGCDCO (CADR G) (CADDR H)))
			       (SETQ Y (FPGCDCO (CADDR G) (CADR H)))
			       (CONS (PTIMES (CAR G) (PTIMES (CADR X) (CADR Y)))
				     (PTIMESCHK (CAR H) (PTIMESCHK (CADDR X) (CADDR Y))))))))

(DEFUN PFACPROD (POLY) 			;FOR RAT3D
       (COND ((PCOEFP POLY) (CFACTOR POLY))
	     (T (NCONC (PFACPROD (CADDR POLY))
		       (LIST (PGET (CAR POLY)) (CADR POLY))))))

(DEFUN FPCONTENT (POLY)
       (let (($ratfac
	nil))				;algebraic uses
       (SETQ POLY (OLDCONTENT POLY))			;rattimes?
       ((lambda (a)					;main var. content
		(cond ((> a 0) (setq a (list (CAADR POLY) A 1))
		               (SETQ POLY
				     (LIST (PTIMES (CAR POLY) A)
					   (PQUOTIENT (CADR POLY) A))))))
	(LOWDEG (CDADR POLY)))
       (COND ((PMINUSP (CADR POLY))
	      (LIST (PMINUS (CAR POLY)) (PMINUS (CADR POLY))))
	     (T POLY))))

;; LOWDEG written to compute the lowest degree of a polynomial. - RZ

(DEFMFUN LOWDEG (P)
  (DO ((L P (CDDR L))) ((NULL (CDDR L)) (CAR L))))

(DEFUN MAKPROD (POLY CONTSWITCH) 
       (COND ((PUREPROD POLY) POLY)
	     ((NULL (CDDDR POLY))
	      (PTIMES (LIST (CAR POLY) (CADR POLY) 1)
		      (MAKPROD (CADDR POLY) CONTSWITCH)))
	     (CONTSWITCH (MAKPROD1 POLY))
	     (T (SETQ POLY (FPCONTENT POLY))
		(PTIMES (MAKPROD (CAR POLY) CONTSWITCH) (MAKPROD1 (CADR POLY)))))) 

(DEFUN MAKPROD1 (POLY*) 
       (DO ((V VARLIST (CDR V))
	    (G GENVAR (CDR G))
	    (P (PDIS POLY*))
	    (RES 1.))
	   ((NULL V) (MAKSYMP POLY*))
	   (AND (ALIKE1 P (CAR V)) (RETURN (PGET (CAR G)))))) 

(DEFUN MAKSYM (P) (NEWSYM2 P (PDIS P))) 

(DEFUN MAKSYMP (P) (COND ((ATOM P) P) (T (PGET (MAKSYM P))))) 

(DEFUN PFLATTEN (H) 
       (PROG (M) 
	     (SETQ M (LISTOVARS H))
	CHECKMORE
	     (COND ((NULL M) (RETURN H))
		   ((NOT ((LAMBDA (P) (OR (NULL P) (EQ (CAR M) (CAR P))))
			  (GETUNHACK (CAR M))))
		    (GO REDO))
		   (T (SETQ M (CDR M)) (GO CHECKMORE)))
	REDO (RETURN (let ($ratfac) (PFLAT1 H))))) 

(DEFUN PFLAT1 (P) 
       (COND ((PCOEFP P) P)
	     ((NULL (CDDDR P))
	      (PTIMES (PEXPT (GETUNHACK (CAR P)) (CADR P)) (PFLAT1 (CADDR P))))
	     (T (DO ((VAL (GETUNHACK (CAR P)))
		     (LD (CADR P) (CAR A))
		     (A (CDDDR P) (CDDR A))
		     (ANS (PFLAT1 (CADDR P))))
		    ((NULL A) (PTIMES ANS (PEXPT VAL LD)))
		    (SETQ ANS
			  (PPLUS (PTIMES ANS
					 (PEXPT VAL (f- LD (CAR A))))
				 (PFLAT1 (CADR A))))))))

(DEFUN PIRREDP (X) 
       (AND (SETQ X (GETDIS X))
	    (OR (ATOM X) (MEMQ 'IRREDUCIBLE (CDAR X)))))

(DEFUN KNOWNFACTORS (D) 
       (PROG (H) 
	     (COND ((PCOEFP D) (RETURN D)))
	     (SETQ H (GETDIS (CAR D)))
	     (RETURN (COND ((OR (ATOM H) (NOT (EQ (CAAR H) 'MTIMES)))
			    (PTIMES (KNOWNFACTORS (CADDR D))
				    (LIST (CAR D) (CADR D) 1.)))
			   (T (SETQ H (GETUNHACK (CAR D)))
			      (PTIMES (KNOWNFACTORS (CADDR D))
				      (PEXPT (KNOWNFACTORS H) (CADR D)))))))) 
