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
(macsyma-module db)

(LOAD-MACSYMA-MACROS MRGMAC)

;; This file uses its own special syntax which is set up here.  The function
;; which does it is defined in LIBMAX;MRGMAC.  It sets up <, >, and : for
;; structure manipulation.  A major bug with this package is that the code is
;; almost completely uncommented.  Someone with nothing better to do should go
;; through it, figure out how it works, and write it down.
;; Note: After recompiling all of macsyma for the Lispm it was found
;;       that some files were compiled with the syntax of ":" set up
;;       incorrectly. The (MODE-SYNTAX-OFF) function, which calls
;;       undocumented system-internal routines evidently did not work anymore.
;;       Therefore I removed the need for MODE-SYNTAX-ON from this file.
;;       7:57pm  Thursday, 25 February 1982 -GJC


;; On systems which cons fixnums, a fixnum is used as a single label cell
;; and a pointer to the fixnum is passed around (i.e. the particular fixnum
;; is passed around.  On systems which have immediate fixnums, a single cons
;; cell is created and the fixnum is stored in the car of the cell.  Fixnums
;; are consed only in PDP-10 MacLisp and Franz Lisp.

#+(OR PDP10 Franz)
(EVAL-WHEN (EVAL COMPILE) (SSTATUS FEATURE FIXCONS))
#+NIL
(EVAL-WHEN (EVAL COMPILE) (SET-NOFEATURE 'FIXCONS))

(DECLARE-TOP(GENPREFIX DB)
	 ;; LAB is not a special.  This declares all occurrences of LAB
	 ;; as a local or a parameter to be a fixnum.  This should really
	 ;; be done using a LOCAL-DECLARE around the entire file so as to
	 ;; make sure any global compiler state gets undone.
	 #+FIXCONS (FIXNUM LAB)
	 (*LEXPR CONTEXT))

;; External specials
;; Please do not use DEFMVAR on these because some of them contain 
;; circular list structure, and we want to be able to load in the 
;; English version of the file at times.  (DEFMVAR tries to print 
;; out their values when the value in core is different from the 
;; value in the file.) - JPG
;; Why don't you set PRINLEVEL and PRINLENGTH in your macsyma? -GJC

(DEFMVAR CONTEXT 'GLOBAL)
(DEFMVAR CONTEXTS NIL)
(DEFMVAR CURRENT 'GLOBAL)
(DEFMVAR +LABS NIL)
(DEFMVAR -LABS NIL)
(DEFMVAR DBTRACE NIL)
(DEFMVAR DBCHECK NIL)
(DEFMVAR DOBJECTS NIL)
(DEFMVAR NOBJECTS NIL)

;; Internal specials

(DEFMVAR MARKS 0) 		
(DECLARE-top (FIXNUM MARKS))
(DEFMVAR +L)
(DECLARE-top (FIXNUM +L))
(DEFMVAR -L)
(DECLARE-TOP (FIXNUM -L))
(DEFMVAR ULABS NIL)

(DEFMVAR CONINDEX 0)
(DECLARE-TOP (FIXNUM CONINDEX))
(DEFMVAR CONNUMBER 50.)
(declare-top (FIXNUM CONNUMBER))

;; The most negative fixnum.  On the PDP-10, this is 1_35.

(DEFMVAR LAB-HIGH-BIT #-cl (ROT 1 -1) #+cl most-negative-fixnum)
;; One less than the number of bits in a fixnum.  On the PDP-10, this is 35.
(DEFMVAR LABNUMBER (f1- (HAULONG LAB-HIGH-BIT)))
;; A cell with the high bit turned on.
(DEFMVAR LAB-HIGH-LAB #+FIXCONS LAB-HIGH-BIT #-FIXCONS (LIST LAB-HIGH-BIT))

(DECLARE-TOP(SPECIAL +S +SM +SL -S -SM -SL LABS LPRS LABINDEX LPRINDEX WORLD *))

;; Macro for indirecting through the contents of a cell.

(DEFMACRO UNLAB (CELL) 
	  #+FIXCONS CELL #-FIXCONS `(CAR ,CELL))

(DEFMACRO SETQ-UNLAB (CELL)
	  #+FIXCONS NIL
	  #-FIXCONS `(SETQ ,CELL (UNLAB ,CELL)))

(DEFMACRO SETQ-COPYN (CELL)
	  #+FIXCONS NIL
	  #-FIXCONS `(SETQ ,CELL (COPYN ,CELL)))

;; Conditionalize primitive functions used in this file.  These are in
;; LAP for Lisp implementations which cons fixnums.  This interface
;; is poorly designed since the meaning of COPYN is varies slightly
;; between systems.  In one case it means "take a cell and produce a
;; new one with the same contents".  In the other, it means "take an
;; immediate fixnum and return a cell containing it."  As a result of
;; this, #+FIXCONS conditionalizations appear in the actual source code.

#-FIXCONS
(PROGN 'COMPILE
  (DEFMACRO COPYN (N) `(LIST ,N))
  (DEFMACRO IORM (CELL N)
	    `(RPLACA ,CELL (LOGIOR (CAR ,CELL) (CAR ,N))))
  (DEFMACRO XORM (CELL N)
	    `(RPLACA ,CELL (LOGXOR (CAR ,CELL) (CAR ,N))))
  )

(defun xxorm (cell n)
  (xorm cell n))
;; The LAP for the PDP-10 version.

#+PDP10 (LAP-A-LIST '(
(LAP COPYN SUBR)
(MOVE TT 0 A)
(JSP T FWCONS)
(POPJ P)
NIL

(LAP IORM SUBR)
(MOVE B 0 B)
(IORM B 0 A)
(POPJ P)
NIL

(LAP XORM SUBR)
(MOVE B 0 B)
(XORM B 0 A)
(POPJ P)
NIL ))

#+Franz
(progn 'compile
       (defmacro copyn (n) `(copyint* ,n))
       (defmacro iorm (cell n) `(replace ,cell (logior ,cell ,n)))
       (defmacro xorm (cell n) `(replace ,cell (logxor ,cell ,n))) )

(DEFPROP GLOBAL 1 CMARK)
;(eval-when ( load )
;(ARRAY CONUNMRK NIL (f1+ CONNUMBER))
;(ARRAY CONMARK T (f1+ CONNUMBER))
;)

(defvar CONUNMRK (*array NIL t (f1+ CONNUMBER)))
(defvar CONMARK (*ARRAY nil  T (f1+ CONNUMBER)))

(DEFMFUN MARK (X) (PUTPROP X T 'MARK))
(DEFMFUN MARKP (X) (AND (SYMBOLP X) (ZL-GET X 'MARK)))

;;
(defun zl-remprop (sym indicator)
  (cond ((symbolp sym) (remprop sym indicator))
	(t (remf (cdr sym) indicator))))

(DEFMFUN UNMRK (X) (zl-REMPROP X 'MARK))

(DEFUN MARKS (X) (COND ((NUMBERP X)) ((ATOM X) (MARK X)) (T (MAPC #'MARKS X))))
(DEFUN UNMRKS (X)
  (COND ((NUMBERP X))
	((OR (ATOM X) (NUMBERP (CAR X))) (UNMRK X))
	(T (MAPC #'UNMRKS X))))

(progn 'compile
(DEFMODE TYPE ()
  (ATOM (SELECTOR +LABS) (SELECTOR -LABS) (SELECTOR DATA))
  SELECTOR)
(DEFMODE INDV ()
  (ATOM (SELECTOR =LABS) (SELECTOR NLABS) (SELECTOR DATA) (SELECTOR IN))
  SELECTOR)
(DEFMODE UNIV ()
  (ATOM (SELECTOR =LABS) (SELECTOR NLABS) (SELECTOR DATA) (SELECTOR UN))
  SELECTOR)
(DEFMODE DATUM ()
  (ATOM (SELECTOR ULABS) (SELECTOR CON) (SELECTOR WN))
  SELECTOR)
(DEFMODE CONTEXT ()
  (ATOM (SELECTOR CMARK FIXNUM 0) (SELECTOR SUBC) (SELECTOR DATA)))
 )




 ;; Is (COPYN 0) really needed in these next four macros instead of simply 0?
;; If the fixnum were to get clobbered, then it would seem that (LIST 0) would
;; be the correct thing to return in the #-FIXCONS case. -cwh

(DEFMACRO +LABZ (X)
  `(COND ((+LABS ,X))
	 (T #+FIXCONS (COPYN 0) #-FIXCONS '(0))))

(DEFMACRO -LABZ (X)
  `(COND ((-LABS ,X))
	 (T #+FIXCONS (COPYN 0) #-FIXCONS '(0))))

(DEFMACRO =LABZ (X)
  `(COND ((=LABS ,X))
	 (T #+FIXCONS (COPYN 0) #-FIXCONS '(0))))

(DEFMACRO NLABZ (X)
  `(COND ((NLABS ,X))
	 (T #+FIXCONS (COPYN 0) #-FIXCONS '(0))))

(DEFMACRO ULABZ (X)
  `(COND ((ULABS ,X))
	 (T #+FIXCONS 0 #-FIXCONS '(0))))

(DEFMACRO SUBP (&rest X)
  #-FIXCONS (SETQ X (MAPCAR #'(LAMBDA (FORM) `(UNLAB ,FORM)) X))
  `(= ,(CAR X) (LOGAND . ,X)))

(DEFUN DBNODE (X) (IF (SYMBOLP X) X (LIST X)))
(DEFUN NODEP (X) (OR (ATOM X) (MNUMP (CAR X))))
(DEFUN DBVARP (X) (GETL X '(UN EX)))

;; Is this supposed to return a fixnum or a cell?

(DEFUN LAB (N) (LSH 1 (f1- N)))

(DEFUN LPR (M N)
  (COND ((DO ((L LPRS (CDR L))) ((NULL L))
	     (IF (AND (LABEQ M (CAAAR L)) (LABEQ N (CDAAR L)))
		 (RETURN (CDAR L)))))
	((= (SETQ LPRINDEX (f1- LPRINDEX)) LABINDEX) (BREAK 'LPR T))
	(T (SETQ LPRS (CONS (CONS (CONS M N) (LSH 1 LPRINDEX)) LPRS))
	   (CDAR LPRS))))

(DEFUN LABEQ (X Y) (EQUAL (LOGIOR X LAB-HIGH-BIT) (LOGIOR Y LAB-HIGH-BIT)))

(DEFUN MARKND (ND)
  (COND ((+LABS ND))
  	((= LPRINDEX (SETQ LABINDEX (f1+ LABINDEX))) (BREAK 'MARKND T))
	(T (SETQ LABS (CONS (CONS ND (LAB LABINDEX)) LABS))
	   (BEG ND (LAB LABINDEX))
	   (CDAR LABS))))

(DEFUN DBV (X R)
  (DECLARE (FIXNUM X R ))
  (DO ((L LPRS (CDR L)) (Y 0)) ((NULL L) Y)
      (declare (fixnum y))
      (IF (AND (NOT (= 0 (LOGAND R (CDAR L)))) (NOT (= 0 (LOGAND X (CAAAR L)))))
	  (SETQ Y (LOGIOR (CDAAR L) Y)))))

(DEFUN DBA (R Y)
  (DECLARE (FIXNUM  R Y ))
  (DO ((L LPRS (CDR L)) (X 0)) ((NULL L) X)
      (DECLARE (FIXNUM X ))
      (IF (AND (NOT (= 0 (LOGAND R (CDAR L)))) (NOT (= 0 (LOGAND (CDAAR L) Y))))
	  (SETQ X (LOGIOR X (CAAAR L))))))
#-cl
(DEFUN PRLAB (X)
  (SETQ-UNLAB X)
  (SETQ X (LET ((*print-base* 2)) (EXPLODEN (BOOLE BOOLE-ANDC1 LAB-HIGH-BIT X))))
  (DO ((I (fixnum-remainder (LENGTH X) 3) 3)) ((NULL X))
      (DO ((J I (f1- J))) ((= 0 J)) (TYO (CAR X)) (SETQ X (CDR X)))
      (TYO #\SPACE)))

#+cl
(DEFUN PRLAB (X)
  (SETQ-UNLAB X)
  (SETQ X (LET ((*print-base* 2)(*read-base* 2))(and x (EXPLODEN (BOOLE BOOLE-ANDC1 LAB-HIGH-BIT X)))))
  (DO ((I (fixnum-remainder (LENGTH X) 3) 3)) ((NULL X))
      (DO ((J I (f1- J))) ((= 0 J)) (TYO (CAR X)) (SETQ X (CDR X)))
      (TYO #\SPACE)))

(DEFUN ONP (CL LAB) (SUBP LAB (+LABZ CL)))
(DEFUN OFFP (CL LAB) (SUBP LAB (-LABZ CL)))
(DEFUN ONPU (LAB FACT) (SUBP LAB (ULABZ FACT)))
(DEFMFUN VISIBLEP (DAT) (AND (NOT (ULABS DAT)) (CNTP DAT)))

(DEFUN CANCEL (LAB DAT)
  (cond
   ((SETQ * (ULABS DAT)) (IORM * LAB))
   (t (SETQ ULABS (CONS DAT ULABS))
      (SETQ-UNLAB LAB)
      (PUTPROP DAT (COPYN LAB) 'ULABS))))

(DEFUN BEG (ND LAB)
  (SETQ-COPYN LAB)
  (IF (QUEUE+P ND LAB) 
      (IF (NULL +S)
	  (SETQ +S (NCONS ND) +SM +S +SL +S)
		    (SETQ +S (CONS ND +S)))))

(DEFUN BEG- (ND LAB)
  (SETQ-COPYN LAB)
  (IF (QUEUE-P ND LAB)
      (IF (NULL -S) (SETQ -S (NCONS ND) -SM -S -SL -S)
		    (SETQ -S (CONS ND -S)))))

(DEFUN MID (ND LAB)
  (IF (QUEUE+P ND LAB)
      (cond
       ((NULL +SM) (SETQ +S (NCONS ND) +SM +S +SL +S))
       (t (RPLACD +SM (CONS ND (CDR +SM)))
	  (IF (EQ +SM +SL) (SETQ +SL (CDR +SL)))
	  (SETQ +SM (CDR +SM))))))

(DEFUN MID- (ND LAB)
  (IF (QUEUE-P ND LAB)
      (cond
       ((NULL -SM) (SETQ -S (NCONS ND) -SM -S -SL -S))
       (t (RPLACD -SM (CONS ND (CDR -SM)))
	  (IF (EQ -SM -SL) (SETQ -SL (CDR -SL)))
	  (SETQ -SM (CDR -SM))))))

(DEFUN END (ND LAB)
  (IF (QUEUE+P ND LAB)
      (cond
       ((NULL +SL) (SETQ +S (NCONS ND) +SM +S +SL +S))
       (t (RPLACD +SL (NCONS ND))
	  (SETQ +SL (CDR +SL))))))

(DEFUN END- (ND LAB)
  (IF (QUEUE-P ND LAB) 
      (cond
       ((NULL -SL) (SETQ -S (NCONS ND) -SM -S -SL -S))
       (t (RPLACD -SL (NCONS ND))
	  (SETQ -SL (CDR -SL))))))

(DEFUN QUEUE+P (ND LAB)
  (COND ((NULL (SETQ * (+LABS ND)))
	 (SETQ +LABS (CONS ND +LABS))
	 (SETQ-UNLAB LAB)
	 (PUT ND (COPYN (LOGIOR LAB-HIGH-BIT LAB)) '+LABS))
	((SUBP LAB *) NIL)
	((SUBP LAB-HIGH-LAB *) (IORM * LAB) NIL)
	(T (IORM * (LOGIOR LAB-HIGH-BIT (UNLAB LAB))))))

(DEFUN QUEUE-P (ND LAB)
  (COND ((NULL (SETQ * (-LABS ND)))
	 (SETQ -LABS (CONS ND -LABS))
	 (SETQ-UNLAB LAB)
	 (PUT ND (COPYN (LOGIOR LAB-HIGH-BIT LAB)) '-LABS))
	((SUBP LAB *) NIL)
	((SUBP LAB-HIGH-LAB *) (IORM * LAB) NIL)
	(T (IORM * (LOGIOR LAB-HIGH-BIT (UNLAB LAB))))))

(DEFUN DQ+ () 
  (IF +S (PROG2 (xXORM (zl-get (car +s) '+labs) ;(+LABS (CAR +S))
		      LAB-HIGH-LAB)
		(CAR +S)
		(COND ((NOT (EQ +S +SM)) (SETQ +S (CDR +S)))
		      ((NOT (EQ +S +SL)) (SETQ +S (CDR +S) +SM +S))
		      (T (SETQ +S NIL +SM NIL +SL NIL))))))

(DEFUN DQ- ()
  (IF -S (PROG2 (XORM (-LABS (CAR -S)) LAB-HIGH-LAB)
		(CAR -S)
		(COND ((NOT (EQ -S -SM)) (SETQ -S (CDR -S)))
		      ((NOT (EQ -S -SL)) (SETQ -S (CDR -S) -SM -S))
		      (T (setq -S NIL -SM NIL -SL NIL))))))

(DEFMFUN CLEAR ()
  (IF DBTRACE (MTELL "~%Clearing ~A" MARKS))
  (MAPC #'(LAMBDA (SYM) (_ (SEL SYM +LABS) NIL)) +LABS)
  (MAPC #'(LAMBDA (SYM) (_ (SEL SYM -LABS) NIL)) -LABS)
  (MAPC #'(LAMBDA (SYM) (ZL-REMPROP SYM 'ULABS)) ULABS)
  (SETQ +S NIL +SM NIL +SL NIL -S NIL -SM NIL -SL NIL 
	LABS NIL LPRS NIL LABINDEX 0 LPRINDEX LABNUMBER  
	MARKS 0 +LABS NIL -LABS NIL ULABS NIL)
  (CONTEXTMARK))

(DEFMFUN TRUEP (PAT)
  (CLEAR)
  (COND ((ATOM PAT) PAT)
	((PROG2 (SETQ PAT (MAPCAR #'SEMANT PAT)) NIL))
	((EQ (CAR PAT) 'KIND) (BEG (CADR PAT) 1) (BEG- (CADDR PAT) 1) (PROPG))
	(T (BEG (CADR PAT) 1) (BEG- (CADDR PAT) 2) (BEG (CAR PAT) (LPR 1 2)) (PROPG))))

(DEFMFUN FALSEP (PAT)
  (CLEAR)
  (COND ((EQ (CAR PAT) 'KIND)
	 (BEG (CADR PAT) 1) (BEG (CADDR PAT) 1) (PROPG))))

(DEFMFUN ISP (PAT) (COND ((TRUEP PAT)) ((FALSEP PAT) NIL) (T 'UNKNOWN)))

(DEFMFUN KINDP (X Y &aux #+lispm (default-cons-area working-storage-area ))
  (IF (NOT (SYMBOLP X)) (MERROR "KINDP called on a non-symbolic atom."))
  (CLEAR)
  (BEG X 1)
  (DO ((P (DQ+) (DQ+))) ((NULL P))
      (IF (EQ Y P) (RETURN T) (MARK+ P (+LABS P)))))

(DEFMFUN TRUE* (PAT)
  (LET ((DUM (SEMANT PAT))) (IF DUM (CNTXT (IND (NCONS DUM)) CONTEXT))))

(DEFMFUN FACT (FUN ARG VAL) (CNTXT (IND (DATUM (LIST FUN ARG VAL))) CONTEXT))

(DEFMFUN KIND (X Y &aux #+kcl (y y))
  (SETQ Y (DATUM (LIST 'KIND X Y))) (CNTXT Y CONTEXT) (ADDF Y X))

(DEFMFUN PAR (S Y)
  (SETQ Y (DATUM (LIST 'PAR S Y))) (CNTXT Y CONTEXT)
  (MAPC #'(LAMBDA (LIS) (ADDF Y LIS)) S))

(DEFMFUN DATUM (PAT) (NCONS PAT))

(DEFUN IND (DAT)
  (MAPC #'(LAMBDA (LIS) (IND1 DAT LIS)) (CDAR DAT))
  (MAPC #'IND2 (CDAR DAT))
  DAT)

(DEFUN IND1 (DAT PAT)
  (COND ((NOT (NODEP PAT)) (MAPC #'(LAMBDA (LIS) (IND1 DAT LIS)) PAT))
	((OR (MARKP PAT) (EQ 'UNKNOWN PAT)))
	(T (ADDF DAT PAT) (MARK PAT))))

(DEFUN IND2 (ND) (IF (NODEP ND) (UNMRK ND) (MAPC #'IND2 ND)))


(DEFMFUN ADDF (DAT ND &aux #+lispm (default-cons-area working-storage-area ))
	 (_ (SEL ND DATA) (CONS DAT (SEL ND DATA))))

(DEFMFUN MAXIMA-REMF (DAT ND) (_ (SEL ND DATA) (FDEL DAT (SEL ND DATA))))

(DEFUN FDEL (FACT DATA)
  (cond
   ((AND (EQ (CAR FACT) (CAAAR DATA))
	 (EQ (CADR FACT) (CADAAR DATA))
	 (EQ (CADDR FACT) (CADDAAR DATA)))
    (CDR DATA))
   (t (DO ((DS DATA (CDR DS)) (D)) ((NULL (CDR DS)))
	  (SETQ D (CAADR DS))
	  (COND ((AND (EQ (CAR FACT) (CAR D))
		      (EQ (CADR FACT) (CADR D))
		      (EQ (CADDR FACT) (CADDR D)))
		 (_ (SEL D CON DATA) (DELQ D (SEL D CON DATA)))
		 (RPLACD DS (CDDR DS)) (RETURN T))))
      DATA)))

(DEFUN SEMANTICS (PAT) (IF (ATOM PAT) PAT (LIST (SEMANT PAT))))

(DEFUN DB-MNUMP (X)
       (OR (NUMBERP X)
	   (AND (NOT (ATOM X))
		(NOT (ATOM (CAR X)))
		(MEMQ (CAAR X) '(RAT BIGFLOAT)))))       

(DEFUN SEMANT (PAT) 
  (COND ((SYMBOLP PAT) (OR (ZL-GET PAT 'VAR) PAT))
	((DB-MNUMP PAT) (DINTNUM PAT))
	(T (MAPCAR #'SEMANT PAT))))

(DEFMFUN DINTERNP (X)
  (COND ((MNUMP X) (DINTNUM X)) 
	((ATOM X) X) 
	((ASSOL X DOBJECTS))))

(DEFMFUN DINTERN (X &aux #+lispm (default-cons-area working-storage-area ))
  (COND ((MNUMP X) (DINTNUM X))
	((ATOM X) X)
	((ASSOL X DOBJECTS))
	(T (SETQ DOBJECTS (CONS (DBNODE X) DOBJECTS))
	   (CAR DOBJECTS))))

(DEFUN DINTNUM (X)
  (COND ((ASSOL X NOBJECTS))
	((PROGN (SETQ X (DBNODE X)) NIL))
	((NULL NOBJECTS) (SETQ NOBJECTS (LIST X)) X)
	((EQ '$POS (RGRP (CAR X) (CAAR NOBJECTS)))
	 (LET ((CONTEXT 'GLOBAL))
	      (FACT 'MGRP X (CAR NOBJECTS)))
	 (SETQ NOBJECTS (CONS X NOBJECTS))  X)
	(T (DO ((LIS NOBJECTS (CDR LIS)) (CONTEXT '$GLOBAL))
	       ((NULL (CDR LIS))
		(LET ((CONTEXT 'GLOBAL))
		     (FACT 'MGRP (CAR LIS) X)) (RPLACD LIS (LIST X)) X)
	       (COND ((EQ '$POS (RGRP (CAR X) (CAADR LIS)))
		      (LET ((CONTEXT 'GLOBAL))
			   (FACT 'MGRP (CAR LIS) X) (FACT 'MGRP X (CADR LIS)))
		      (RPLACD LIS (CONS X (CDR LIS)))
		      (RETURN X)))))))

(DEFMFUN DOUTERN (X) (IF (ATOM X) X (CAR X)))

(DEFMFUN UNTRUE (PAT)
  (KILL (CAR PAT) (SEMANT (CADR PAT)) (SEMANT (CADDR PAT))))

(DEFMFUN KILL (FUN ARG VAL) (KILL2 FUN ARG VAL ARG) (KILL2 FUN ARG VAL VAL))

(DEFUN KILL2 (FUN ARG VAL CL)
  (COND ((NOT (ATOM CL)) (MAPC #'(LAMBDA (LIS) (KILL2 FUN ARG VAL LIS)) CL))
	((NUMBERP CL))
	(T (_ (SEL CL DATA) (KILL3 FUN ARG VAL (SEL CL DATA))))))

(DEFUN KILL3 (FUN ARG VAL DATA)
       (cond
	((AND (EQ FUN (CAAAR DATA))
	      (EQ ARG (CADAAR DATA)) (EQ VAL (CADDAAR DATA)))
	 (CDR DATA))
	(t (DO ((DS DATA (CDR DS)) (D)) ((NULL (CDR DS)))
	       (SETQ D (CAADR DS))
	       (cond
		((NOT (AND (EQ FUN (CAR D))
			   (EQ ARG (CADR D))
			   (EQ VAL (CADDR D))))
		 T)
		(t (_ (SEL D CON DATA) (DELQ D (SEL D CON DATA)))
		   (RPLACD DS (CDDR DS)) (RETURN T))))
	   DATA)))

(DEFMFUN UNKIND (X Y)
       (setq y (car (datum (LIST 'kind x y))))
       (kcntxt y context)
       (MAXIMA-REMF y x))

(defmfun remov (fact)
       (remov4 fact (cadar fact))
       (remov4 fact (caddar fact)))

(defun remov4 (fact cl)	 
  (cond ((or (symbolp cl)			;if CL is a symbol or
	     (and (consp cl)			;an interned number, then we want to REMOV4 FACT
		  (numberp (car cl))))		;from its property list.
	 (_ (sel cl data) (delq fact (sel cl data))))
	((or (atom cl) (atom (car cl))))	;if CL is an atom (not a symbol)
						;or its CAR is an atom then we don't want to do
						;anything to it.
	(t (mapc #'(lambda (lis) (remov4 fact lis))
		 (cond ((atom (caar cl)) (cdr cl)) ;if CL's CAAR is
						;an atom, then CL is an expression, and
						;we want to REMOV4 FACT from the parts
						;of the expression. 
		       ((atom (caaar cl)) (cdar cl)))))))
			      ;if CL's CAAAR is an atom, then CL is a
			      ;fact, and we want to REMOV4 FACT from
			      ;the parts of the fact.

(DEFMFUN KILLFRAME (CL)
       (MAPC #'REMOV (SEL CL DATA))
       (ZL-REMPROP CL '+LABS) (ZL-REMPROP CL '-LABS)
       (ZL-REMPROP CL 'OBJ) (ZL-REMPROP CL 'VAR)
       (ZL-REMPROP CL 'FACT)
       (ZL-REMPROP CL 'WN))

(DEFMFUN ACTIVATE N 
  (DO ((I 1 (f1+ I))) ((> I N))
      (cond
       ((MEMQ (ARG I) CONTEXTS) NIL)
       (t (SETQ CONTEXTS (CONS (ARG I) CONTEXTS))
	  (CMARK (ARG I))))))

(DEFMFUN DEACTIVATE N 
  (DO ((I 1 (f1+ I))) ((> I N))
      (cond
       ((NOT (MEMQ (ARG I) CONTEXTS)) NIL)
       (t (CUNMRK (ARG I))
	  (SETQ CONTEXTS (DELQ (ARG I) CONTEXTS))))))

(DEFMFUN CONTEXT N (NEWCON (LISTIFY N)))

(DEFUN NEWCON (C)
  (IF (> CONINDEX CONNUMBER) (GCCON))
  (SETQ C (IF (NULL C) (LIST '*GC NIL) (LIST '*GC NIL 'SUBC C)))
  (store (AREF CONUNMRK CONINDEX) C)
  (store (AREF CONMARK CONINDEX) (CDR C))
  (SETQ CONINDEX (f1+ CONINDEX))
  C)

;; To be used with the WITH-NEW-CONTEXT macro.
(DEFUN CONTEXT-UNWINDER ()
  (KILLC (AREF CONMARK CONINDEX))
  (SETQ CONINDEX (f1- CONINDEX))
  (SETF (AREF CONUNMRK CONINDEX) ())
  )

(DEFUN GCCON () 
  (GCCON1)
  (WHEN (> CONINDEX CONNUMBER)
	#+GC (GC)
	(GCCON1)
	(WHEN (> CONINDEX CONNUMBER)
	      (MERROR "~%Too many contexts."))))

(DEFUN GCCON1 ()
  (SETQ CONINDEX 0)
  (DO ((I 0 (f1+ I))) ((> I CONNUMBER))
      (cond
       ((NOT (EQ (AREF CONMARK I) (CDR (AREF CONUNMRK I))))
	(KILLC (AREF CONMARK I)))
       (t (STORE (AREF CONUNMRK CONINDEX) (AREF CONUNMRK I))
	  
	  (STORE (AREF CONMARK CONINDEX) (AREF CONMARK I))
	  
	  (SETQ CONINDEX (f1+ CONINDEX))))))

(DEFMFUN CNTXT (DAT CON)
  (IF (NOT (ATOM CON)) (SETQ CON (CDR CON)))
  (PUT CON (CONS DAT (ZL-GET CON 'DATA)) 'DATA)
  (IF (NOT (EQ 'GLOBAL CON)) (PUT DAT CON 'CON))
  DAT)

(defmfun kcntxt (fact con)
       (if (not (atom con)) (setq con (cdr con)))
       (put con (fdel fact (zl-get con 'data)) 'data)
       (if (not (eq 'global con)) (zl-remprop fact 'con))
       fact)

(DEFUN CNTP (F)
  (COND ((NOT (SETQ F (SEL F CON))))
	((SETQ F (ZL-GET F 'CMARK)) (> F 0))))

(DEFMFUN CONTEXTMARK (&aux #+lispm (default-cons-area working-storage-area ))
  (LET ((CON CONTEXT))
       (UNLESS (EQ CURRENT CON)
	       (CUNMRK CURRENT) (SETQ CURRENT CON) (CMARK CON))))

(DEFUN CMARK (CON)
  (IF (NOT (ATOM CON)) (SETQ CON (CDR CON)))
  (LET ((CM (ZL-GET CON 'CMARK)))
    (PUTPROP CON (IF CM (f1+ CM) 1) 'CMARK)
    (MAPC #'CMARK (ZL-GET CON 'SUBC))))

(DEFUN CUNMRK (CON)
  (IF (NOT (ATOM CON)) (SETQ CON (CDR CON)))
  (LET ((CM (ZL-GET CON 'CMARK)))
       (COND (CM (PUTPROP CON (f1- CM) 'CMARK)))
       (MAPC #'CUNMRK (ZL-GET CON 'SUBC))))

(DEFMFUN KILLC (CON)
  (CONTEXTMARK)
  (COND ((NOT (NULL CON))
	 (MAPC #'REMOV (ZL-GET CON 'DATA))
	 (ZL-REMPROP CON 'DATA)
	 (ZL-REMPROP CON 'CMARK)
	 (ZL-REMPROP CON 'SUBC)))
  T)

(DEFUN PROPG ()
  (DO ((X) (LAB)) (NIL)
      (COND ((SETQ X (DQ+))
	     (SETQ LAB (+LABS X))
	     (IF (= 0 (LOGAND (UNLAB LAB) (UNLAB (-LABZ X))))
		 (MARK+ X LAB) (RETURN T)))
	    ((SETQ X (DQ-))
	     (SETQ LAB (-LABS X))
	     (IF (= 0 (LOGAND (UNLAB LAB) (UNLAB (+LABZ X))))
		 (MARK- X LAB) (RETURN T)))
	    (T (RETURN NIL)))))

(DEFUN MARK+ (CL LAB &aux #+lispm (default-cons-area working-storage-area ))
  (COND (DBTRACE (SETQ MARKS (f1+ MARKS))
	 (MTELL "~%Marking ~A +" CL) (PRLAB LAB)))
  (MAPC #'(LAMBDA (LIS) (MARK+0 CL LAB LIS)) (SEL CL DATA)))

(DEFUN MARK+0 (CL LAB FACT)
  (COND (DBCHECK (MTELL "~%Checking ~A from ~A+" (CAR FACT) CL) (PRLAB LAB)))
  (COND ((ONPU LAB FACT))
	((NOT (CNTP FACT)))
	((NULL (SEL FACT WN)) (MARK+1 CL LAB FACT))
	((ONP (SEL FACT WN) WORLD) (MARK+1 CL LAB FACT))
	((OFFP (SEL FACT WN) WORLD) NIL)
	(T (MARK+3 CL LAB FACT))))

(DEFUN MARK+1 (CL LAB DAT)
  (COND ((EQ (CAAR DAT) 'KIND)
	 (IF (EQ (CADAR DAT) CL) (MID (CADDAR DAT) LAB)))  ; E1
	((EQ (CAAR DAT) 'PAR)
	 (IF (NOT (EQ (CADDAR DAT) CL))
	     (PROGN (CANCEL LAB DAT)  ; PR1
		    (MID (CADDAR DAT) LAB)
		    (DO ((LIS (CADAR DAT) (CDR LIS))) ((NULL LIS))
		        (IF (NOT (EQ (CAR LIS) CL)) (MID- (CAR LIS) LAB))))))
	((EQ (CADAR DAT) CL)
	 (IF (+LABS (CAAR DAT))  ; V1
	     (END (CADDAR DAT) (DBV LAB (+LABS (CAAR DAT)))))
	 (IF (-LABS (CADDAR DAT))  ; F4
	     (END- (CAAR DAT) (LPR LAB (-LABS (CADDAR DAT))))))))

(DEFUN MARK+3 (CL LAB DAT) CL LAB ;Ignored
  (IFN (= 0 (LOGAND (UNLAB (+LABZ (CADDAR DAT)))
		    (UNLAB (DBV (+LABZ (CADAR DAT)) (-LABZ (CAAR DAT))))))
       (BEG- (SEL DAT WN) WORLD)))


(DEFUN MARK- (CL LAB &aux #+lispm (default-cons-area working-storage-area ))
  (WHEN DBTRACE
	(SETQ MARKS (f1+ MARKS)) (MTELL "Marking ~A -" CL) (PRLAB LAB))
  (MAPC #'(LAMBDA (LIS) (MARK-0 CL LAB LIS)) (SEL CL DATA)))

(DEFUN MARK-0 (CL LAB FACT)
  (WHEN DBCHECK (MTELL "~%Checking ~A from ~A-" (CAR FACT) CL) (PRLAB LAB))
  (COND ((ONPU LAB FACT))
	((NOT (CNTP FACT)))
	((NULL (SEL FACT WN)) (MARK-1 CL LAB FACT))
	((ONP (SEL FACT WN) WORLD) (MARK-1 CL LAB FACT))
	((OFFP (SEL FACT WN) WORLD) NIL)))

(DEFUN MARK-1 (CL LAB DAT  &aux #+lispm (default-cons-area working-storage-area ))
  (COND ((EQ (CAAR DAT) 'KIND)
	 (IF (NOT (EQ (CADAR DAT) CL)) (MID- (CADAR DAT) LAB)))  ; E4
	((EQ (CAAR DAT) 'PAR)
	 (IF (EQ (CADDAR DAT) CL)
	     (PROG2 (CANCEL LAB DAT)  ; S4
		    (DO ((LIS (CADAR DAT) (CDR LIS))) ((NULL LIS)) (MID- (CAR LIS) LAB)))
	     (PROGN (SETQ-UNLAB LAB)  ; ALL4
		    (DO ((LIS (CADAR DAT) (CDR LIS))) ((NULL LIS))
		        (SETQ LAB (LOGAND (UNLAB (-LABZ (CAR LIS))) LAB)))
		    (SETQ-COPYN LAB)
		    (CANCEL LAB DAT)
		    (MID- (CADDAR DAT) LAB))))
	((EQ (CADDAR DAT) CL)
	 (IF (+LABS (CAAR DAT))  ; A2
	     (END- (CADAR DAT) (DBA (+LABS (CAAR DAT)) LAB)))
	 (IF (+LABS (CADAR DAT))  ; F6
	     (END- (CAAR DAT) (LPR (+LABS (CADAR DAT)) LAB))))))

;	     in out                    in out                  ins  in out
;	-----------		-------------             ----------------
;	E1 |     +		INV1 |     +              AB1 |(+)  +   +
;	E2 |     -		INV2 |     -              AB2 |(+)  -   +
;	E3 | +			INV3 | +                  AB3 |(+)  +   -
;	E4 | -			INV4 | -                  AB4 |(+)  -   -
;                                                         AB5 |(-)  +   +
;            in out                    in out             AB6 |(-)  -   +
;       -----------             -------------             AB7 |(-)  +   -
;       S1 |    (+)             ALL1 |(+)  +              AB8 |(-)  -   -
;       S2 |    (-)             ALL2 |(+)  -
;       S3 |(+)                 ALL3 |(-)  +
;       S4 |(-)                 ALL4 |(-)  -



;	     in rel out	         in rel out	     in rel out
;	---------------	    ---------------	---------------
;	V1 |    (+)  +	    A1 | +  (+)		F1 |     +  (+)
;	V2 |    (+)  -	    A2 | -  (+)		F2 |     +  (-)
;	V3 |    (-)  +	    A3 | +  (-)		F3 |     -  (+)
;	V4 |    (-)  -	    A4 | -  (-)		F4 |     -  (-)
;						F5 |(+)  +
;						F6 |(+)  -
;						F7 |(-)  +
;						F8 |(-)  -


(DEFUN UNI (P1 P2 AL)
  (COND ((DBVARP P1) (DBUNIVAR P1 P2 AL))
	((NODEP P1)
	 (COND ((DBVARP P2) (DBUNIVAR P2 P1 AL))
	       ((NODEP P2) (IF (EQ P1 P2) AL))))
	((DBVARP P2) (DBUNIVAR P2 P1 AL))
	((NODEP P2) NIL)
	((SETQ AL (UNI (CAR P1) (CAR P2) AL)) (UNI (CDR P1) (CDR P2) AL))))

(DEFUN DBUNIVAR (P V AL)
  (LET ((DUM (ASSQ P AL)))
    (COND ((NULL DUM) (CONS (CONS P V) AL))
	  (T (UNI (CDR DUM) V AL)))))

; Undeclarations for the file:

#-NIL
(DECLARE-TOP(NOTYPE LAB))
