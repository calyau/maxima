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
(macsyma-module mstuff)

(DECLARE-TOP(SPLITFILE MSORT) (FIXNUM N))

(DEFMFUN $SORT N
  (IF (OR (= N 0) (> N 2)) (MERROR "SORT takes 1 or 2 arguments."))
  (LET ((LLIST (ARG 1)) COMPARFUN BFUN)
       (IF (NOT ($LISTP LLIST))
	   (MERROR "The first argument to SORT must be a list:~%~M" LLIST))
       (SETQ LLIST (copy-top-level (CDR LLIST) )
	     COMPARFUN 
	     (MFUNCTION1 (SETQ BFUN (IF (= N 2) (GETOPR (ARG 2)) 'LESSTHAN))))
       (IF (MEMQ BFUN '(LESSTHAN GREAT))
	   (SETQ LLIST (MAPCAR #'RATDISREP LLIST)))
       (CONS '(MLIST SIMP) (SORT LLIST COMPARFUN))))

;; old non closure version
;;(DEFUN MFUNCTION1 (FUN)
;;  `(LAMBDA (X Y) (MEVALP `((,',FUN) ((MQUOTE) ,X) ((MQUOTE) ,Y)))))

#+cl
(DEFUN MFUNCTION1 (FUN)
  (function (LAMBDA (X Y) (MEVALP `((,FUN) ((MQUOTE) ,X) ((MQUOTE) ,Y))))))

(DEFUN LESSTHAN (A B) (IF (GREAT B A) T))

(declare-top (SPLITFILE MAKEL))

(DEFMSPEC $MAKELIST (X) (SETQ X (CDR X))
   (PROG (N FORM ARG A B LV D)
      (SETQ N (LENGTH X))
      (IF (OR (< N 3) (> N 4))
	  (MERROR "MAKELIST takes 3 or 4 arguments."))
      (SETQ FORM (CAR X)
	    ARG (CADR X)
	    A (MEVAL (CADDR X))
	    LV (COND ((= N 3) 
		      (IF ($LISTP A)
			  (MAPCAR #'(LAMBDA (U) (LIST '(MQUOTE) U)) (CDR A))
			  (MERROR "
If 3 arguments are given to MAKELIST,
the 3rd argument should evaluate to a list:~%~M" A)))
		     (T
		      (SETQ B (MEVAL (CADDDR X)))
		      (IF (OR (NOT (FIXNUMP (SETQ D (SUB* B A)))) (< D -1))
			  (MERROR "
If 4 arguments are given to MAKELIST, the difference of the 3rd
and 4th arguments should evaluate to a non-negative integer:~%~M" D)
			  (INTERVAL A B)))))
      (RETURN 
	 (DO ((LV LV (CDR LV)) (ANS))
	     ((NULL LV) (CONS '(MLIST SIMP) (NREVERSE ANS)))
	     (SETQ ANS (CONS (MEVAL `(($EV)
				      ,@(LIST (LIST '(MQUOTE) FORM)
					  (LIST '(MEQUAL SIMP) 
						ARG 
						(CAR LV)))))
			     ANS))))))

(DEFUN INTERVAL (I J)
   (DO ((NN I (ADD2 1 NN)) (M 0 (f1+ M)) (K (SUB* J I)) (ANS))
       ((> M K) (NREVERSE ANS))
       (SETQ ANS (CONS NN ANS))))

(DEFMFUN $SUBLIST (A F)
  (IF ($LISTP A)
      (DO ((A (CDR A) (CDR A)) (X))
	  ((NULL A) (CONS '(MLIST SIMP) (NREVERSE X)))
	  (IF (MEVALP (LIST (NCONS F) (CAR A)))
	      (SETQ X (CONS (CAR A) X))))
      (MERROR "The first argument to SUBLIST must be a list:~%~M" A)))

; Undeclarations for the file:
#-NIL
(DECLARE-TOP(NOTYPE N))


