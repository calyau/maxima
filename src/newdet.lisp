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
(macsyma-module newdet)

;; THIS IS A VERSION OF THE GENTLEMAN-JOHNSON TREE-MINOR DETERMINANT
;; USING RATIONAL FUNCTIONS.  "A" CAN BE A MATRIX OR AN ARRAY.
;; ANSWER IS IN RATIONAL FORM.
;; RJF  5/2/73

(DECLARE-TOP(SPECIAL VLIST VARLIST GENVAR ARYP)
	 #-cl
	 (FIXNUM RR K J OLD NEW *BINOM* *i* PASCAL N M))
;	 (ARRAY* (NOTYPE *INPUT* 2 *BINOM* 2 *MINOR1* 2 *i* 1)

;;these are general type arrays
(declare-top  (special *INPUT*  *BINOM*  *MINOR1*  *i* ))

(DEFMFUN $NEWDET N
       ((LAMBDA (A)
		(COND ((= N 2)
		       (COND ((NOT (INTEGERP (ARG 2)))
			      (merror "Wrong arg to NEWDET: ~M" (ARG 2))))
		       (SETQ A (ARG 1) N (ARG 2)))
		      ((AND (= N 1) ($MATRIXP (SETQ A (ARG 1))))
		       (SETQ N (LENGTH (CDR (ARG 1)))))
		      (T (merror "Wrong number of args to NEWDET")))
		(NEWDET A N NIL))
	NIL))

(DEFMFUN $PERMANENT N		
       ((LAMBDA (A)
		(COND ((= N 2)
		       (COND ((NOT (INTEGERP (ARG 2)))
			      (merror "Wrong arg to PERM: ~M" (ARG 2))))
		       (SETQ A (ARG 1) N (ARG 2)))
		      ((AND (= N 1) ($MATRIXP (SETQ A (ARG 1))))
		       (SETQ N (LENGTH (CDR (ARG 1)))))
		      (T (merror "Wrong number of args to PERM")))
		(NEWDET A N T))
	NIL))

(DEFUN NEWDET (A N PERM)
  (PROG (RR R K J OLD NEW VLIST M LOC ADDR SIGN) 
	(COND ((> N 50.)
	       (merror "Array too big - NEWDET: ~M" N)))
	(setq  *BINOM* (*ARRAY nil T (ADD1 N) (ADD1 N)))
	(setq  *MINOR1* (*ARRAY nil T 2. (ADD1 (SETQ RR (PASCAL N)))))
	(setq  *i* (*ARRAY nil T (PLUS 2. N)))
	(DO ((K
	    0.
	  (ADD1 K)))
	  ((> K 1.))
	  (DO ((J
	      0.
	    (ADD1 J)))
	    ((> J RR))
	    (STORE (aref *MINOR1* K J) '(0. . 1.))))
	(DO ((K 0. (ADD1 K))) ((> K (ADD1 N))) (STORE (aref *i* K) -1.))
	(setq  *INPUT* (*ARRAY nil T (ADD1 N) (ADD1 N)))
	(DO ((K
	    1.
	  (ADD1 K)))
	  ((> K N))
	  (DO ((J
	      1.
	    (ADD1 J)))
	    ((> J N))
	    (NEWVAR1 (STORE (aref *INPUT* K J)
			    ((LAMBDA (ARYP)
			       #+cl
			       (maref a k j)
			       #-cl
			       (MEVAL (LIST (LIST A 'array) K J))
			       )
			     T)))))
	(NEWVAR (CONS '(MTIMES) VLIST))
	(DO ((K
	    1.
	  (ADD1 K)))
	  ((> K N))
	  (DO ((J
	      1.
	    (ADD1 J)))
	    ((> J N))
	    (STORE (aref *INPUT* K J)
		   (CDR (RATREP* (aref *INPUT* K J))))))
	(SETQ NEW 1.)
	(SETQ OLD 0.)
	(STORE (aref *i* 0.) N)
	(DO ((LOC
	    1.
	  (ADD1 LOC)))
	  ((> LOC N))
	  (STORE (aref *MINOR1* OLD (SUB1 LOC)) (aref *INPUT* 1. LOC)))
	(SETQ M 1.)
     G0193(COND ((> M (SUB1 N)) (GO RET)))
	(SETQ LOC 0.)
	(SETQ J 1.)
     G0189(COND ((> J M) (GO NEXTMINOR)))
	(STORE (aref *i* J) (DIFFERENCE M J))
	(SETQ J (f+ J 1.))
	(GO G0189)
     NEXTMINOR
	(COND ((NOT (EQUAL (aref *MINOR1* OLD LOC) '(0. . 1.)))
	       (SETQ K (SUB1 N))
	       (SETQ J 0.)
	       (SETQ ADDR (PLUS LOC (aref *BINOM* K (ADD1 M))))
	       (SETQ SIGN 1.))
	      (T (GO OVER)))
     NEXTUSE
	(COND
	  ((EQUAL K (aref *i* (ADD1 J)))
	   (SETQ J (ADD1 J))
	   (SETQ SIGN (MINUS SIGN)))
	  (T
	   (STORE
	     (aref *MINOR1* NEW ADDR)
	     (RATPLUS
	       (aref *MINOR1* NEW ADDR)
	       (RATTIMES (aref *MINOR1* OLD LOC)
			 (COND ((OR (EQUAL SIGN 1.) PERM)
				(aref *INPUT* (ADD1 M) (ADD1 K)))
			       (T (RATMINUS (aref *INPUT* (ADD1 M)
						  (ADD1 K)))))
			 T)))))
	(COND ((> K 0.)
	       (SETQ K (SUB1 K))
	       (SETQ ADDR
		     (DIFFERENCE ADDR
				 (aref *BINOM* K (DIFFERENCE M J))))
	       (GO NEXTUSE)))
	(STORE (aref *MINOR1* OLD LOC)  '(0 . 1))
     OVER (SETQ LOC (ADD1 LOC))
	(SETQ J M)
     BACK (COND ((> 1. J) (SETQ M (ADD1 M))(SETQ OLD(DIFFERENCE 1 OLD))(SETQ NEW (DIFFERENCE 1 NEW))(GO G0193)))
	(STORE (aref *i* J) (ADD1 (aref *i* J)))
	(COND ((> (aref *i* (SUB1 J)) (aref *i* J)) (GO NEXTMINOR))
	      (T (STORE (aref *i* J) (DIFFERENCE M J))))
	
	(SETQ J (SUB1 J))
	(GO BACK)
     RET(*REARRAY '*BINOM*)
	(*REARRAY '*INPUT*)
	(SETQ R (CONS (LIST 'MRAT
			    'SIMP
			    VARLIST
			    GENVAR)
		      (aref *MINOR1* OLD 0.)))
	(*REARRAY '*MINOR1*)
	(RETURN R)))

(DEFUN PASCAL (N) 
       (PROG NIL 
	     (STORE (aref *BINOM* 0. 0.) 1.)
	     (DO ((H
		 1.
		 (ADD1 H)))
		 ((> H N))
		 (STORE (aref *BINOM* H 0.) 1.)
		 (STORE (aref *BINOM* (SUB1 H) H) 0.)
		 (DO ((J
		     1.
		     (ADD1 J)))
		     ((> J H))
		     (STORE (aref *BINOM* H J)
			    (PLUS (aref *BINOM* (SUB1 H) (SUB1 J))
				  (aref *BINOM* (SUB1 H) J)))))
	     (RETURN (SUB1 (aref *BINOM* N (LSH N -1.))))))

;;these need to be special in so many places please dont unspecial them..
;;(DECLARE (UNSPECIAL VLIST VARLIST GENVAR ARYP))
