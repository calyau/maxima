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

(declare-top(genprefix tl)
	    (*lexpr $limit)
	    (special $tlimswitch taylored exp var val ll ul
		     silent-taylor-flag)) 

#-nil
(defmfun $tlimit nargs 
  ((lambda ($tlimswitch) (apply '$limit (listify nargs))) t)) 
#+nil
(defmfun $tlimit (&restv argvec)
  (let (($tlimswitch t)) (apply #'$limit argvec)))


(defmfun $tldefint (exp var ll ul) 
  ((lambda ($tlimswitch) ($ldefint exp var ll ul)) t))

(defun tlimp (exp)		; TO BE EXPANDED TO BE SMARTER (MAYBE)
  t) 

(defun taylim (e *i*) 
  (prog (ex)
     (setq ex (catch 'taylor-catch
		(let ((silent-taylor-flag t))
		  ($taylor e var (ridofab val) 1.))))
     (or ex (return (cond ((eq *i* t) (limit1 e var val))
			  ((eq *i* 'think) (cond ((memq (caar exp)
							'(mtimes mexpt))
						  (limit1 e var val))
						 (t (simplimit e var val))))
			  (t (simplimit e var val)))))
     (return
       (let ((taylored t))
	 (limit
	  (simplify
	   ($logcontract ($ratdisrep ex)))
	  ;;(COND ((EQ (CADR EX) 'PS)
	  ;;       (CONS (CAR EX)
	  ;;             (LIST 'PS (THIRD EX) (FOURTH EX)
	  ;;                   (FIFTH EX))))
	  ;;      (t (EX)))
	  var
	  val
	  'think)))))

#-nil
(declare-top(unspecial taylored exp var val ll ul)) 

