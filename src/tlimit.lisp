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
(macsyma-module tlimit)
(load-macsyma-macros rzmac)

;; TOP LEVEL FUNCTION(S): $TLIMIT $TLDEFINT

(DECLARE-TOP(GENPREFIX TL)
	 (*LEXPR $LIMIT)
	 (SPECIAL $TLIMSWITCH TAYLORED EXP VAR VAL LL UL
		  SILENT-TAYLOR-FLAG)) 

#-NIL
(DEFMFUN $TLIMIT NARGS 
       ((LAMBDA ($TLIMSWITCH) (APPLY '$LIMIT (LISTIFY NARGS))) T)) 
#+NIL
(defmfun $tlimit (&restv argvec)
  (let (($tlimswitch t)) (apply #'$limit argvec)))


(DEFMFUN $TLDEFINT (EXP VAR LL UL) 
       ((LAMBDA ($TLIMSWITCH) ($LDEFINT EXP VAR LL UL)) T))

(DEFUN TLIMP (EXP) ; TO BE EXPANDED TO BE SMARTER (MAYBE)
       T) 

(DEFUN TAYLIM (E *I*) 
  (PROG (EX)
	(SETQ EX (CATCH 'TAYLOR-CATCH
			 (let ((SILENT-TAYLOR-FLAG t))
			   ($TAYLOR E VAR (RIDOFAB VAL) 1.))))
	(OR EX (RETURN (COND ((EQ *I* T) (LIMIT1 E VAR VAL))
			     ((EQ *I* 'THINK) (COND ((MEMQ (CAAR EXP)
							   '(MTIMES MEXPT))
						     (LIMIT1 E VAR VAL))
						    (T (SIMPLIMIT E VAR VAL))))
			     (T (SIMPLIMIT E VAR VAL)))))
	(RETURN
	 (let ((TAYLORED t))
	   (LIMIT
	    (SIMPLIFY
	     ($logcontract ($RATDISREP ex)))
	    ;;(COND ((EQ (CADR EX) 'PS)
	    ;;       (CONS (CAR EX)
	    ;;             (LIST 'PS (THIRD EX) (FOURTH EX)
	    ;;                   (FIFTH EX))))
	    ;;      (t (EX)))
	    VAR
	    VAL
	    'THINK)))))

#-NIL
(DECLARE-TOP(UNSPECIAL TAYLORED EXP VAR VAL LL UL)) 

