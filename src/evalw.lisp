;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1981 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module evalw)

;;; Assuming that this will only be a top-level form, it will
;;; only be see by MEVAL when a file is batched.

;;; EVAL_WHEN(TRANSLATE,FOO(ME),BAZ())$
;;; EVAL_WHEN([LOADFILE,BATCH],INITIALIZE())$

(DECLARE-TOP(SPECIAL $VERSION STATE-PDL BATCONL))

;; Gosh. Seems it was really stupid to have EVAL_WHEN for BATCH and DEMO,
;; people use it for the most random things. -gjc

(DEFMSPEC $EVAL_WHEN (ARGL)
  (SETQ ARGL (CDR ARGL))
  (COND ((OR (< (LENGTH ARGL) 2)
	     (NOT (OR (ATOM (CAR ARGL))
		      ($LISTP (CAR ARGL)))))
	 (MERROR "Bad whens form to EVAL_WHEN~%~M" (CAR ARGL))))
  (LET ((DEMOP #-MAXII (IF (AND (EQ (ml-typep $VERSION) 'fixnum)
				(> $VERSION 296.))
			   (CADDR BATCONL)
			   (CADDDR BATCONL))
	       #+Maxii  NIL)
	(WHENS (COND (($LISTP (CAR ARGL)) (CDAR ARGL))
		     (T (LIST (CAR ARGL))))))
    (COND ((COND (#-MAXII (MEMQ 'BATCH STATE-PDL)
		  #+MAXII T ; foo for now!
		  (IF DEMOP (OR (MEMQ '$DEMO WHENS) (MEMQ '$BATCH WHENS))
			    (MEMQ '$BATCH WHENS)))
		 (T
		  ;; this is a form typed in on a c-line by
		  ;; the user. or, perhaps it is inside a
		  ;; program. Which is an error in the translator.
		  ;; What *was* I doing here? -gjc
		  (MEMQ '$TOPLEVEL WHENS)))
	   `(($EVALUATED_WHEN) ,@(MAPCAR 'MEVAL (CDR ARGL))))
	  (T
	   '$NOT_EVALUATED_WHEN))))

