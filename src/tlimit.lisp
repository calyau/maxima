;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(declare-top (special taylored))

(macsyma-module tlimit)

(load-macsyma-macros rzmac)

;; TOP LEVEL FUNCTION(S): $TLIMIT $TLDEFINT

(defmfun $tlimit (&rest args)
  (let ((limit-using-taylor t))
    (declare (special limit-using-taylor))
    (apply #'$limit args)))

(defmfun $tldefint (exp var ll ul)
  (let ((limit-using-taylor t))
    (declare (special limit-using-taylor))
    ($ldefint exp var ll ul)))

;; Taylor cannot handle conjugate, ceiling, floor, unit_step, or signum 
;; expressions, so let's tell tlimit to *not* try. We also disallow 
;; expressions containing $ind.
(defun tlimp (e x)	
  (or (and ($mapatom e) (not (eq e '$ind)) (not (eq e '$und)))
	  (and (consp e) 
	       (consp (car e)) 
	       (or 
		      (known-ps (caar e)) 
			  (and (eq (caar e) 'mqapply) (known-ps (subfunname e)))
	          (member (caar e) (list 'mplus 'mtimes 'mexpt '%log))
			  (get (caar e) 'grad)
			  ($freeof x e))
		    (every #'(lambda (q) (tlimp q x)) (cdr e)))))

;; Dispatch Taylor, but recurse on the order until either the recursion
;; depth is 15 or the Taylor polynomial is nonzero. When Taylor 
;; fails to find a nonzero Taylor polynomial or the recursion depth is
;; too great, return nil.

;; This recursion on the order attempts to handle limits such as 
;; tlimit(2^n/n^5, n, inf) correctly. 

;; We set up a reasonable environment for calling taylor. When $taylor_logexpand 
;; is true, taylor does some principal branch violating transformations, so we set 
;; it to nil.

;; I know of no compelling reason for defaulting the taylor order to 
;; lhospitallim, but this is documented in the user documentation). 

(defun tlimit-taylor (e x pt n &optional (d 0))
	(let ((ee 0) 
	      (silent-taylor-flag t) 
	      ($taylordepth 8)
		  ($radexpand nil)
		  ($logexpand nil)
		  ($taylor_logexpand nil)
		  ($taylor_simplifier #'(lambda (q) (sratsimp (extra-simp q)))))
		(setq e (partial-logarc e (list '%atan)))
	    (setq ee  (catch 'taylor-catch (let (($logexpand t)) (ratdisrep ($taylor e x pt n)))))
		(cond ((and ee (not (alike1 ee 0))) ee)
			  ;; When taylor returns zero and the depth d is less than 16, 
			  ;; declare a do-over; otherwise return nil.
              ((and ee (< d 16))
			    (tlimit-taylor e x pt (* 4 (max 1 n)) (+ d 1)))
			  (t nil))))

;; Previously when the taylor series failed, there was code for deciding
;; whether to call limit1 or simplimit. The choice depended on the last
;; argument to taylim (previously named *i*) and the main operator of the 
;; expression. This code dispenses with this logic and dispatches limit1
;; when Maxima is unable to find the taylor polynomial. This change orphans 
;; the last argument of taylim.
(defun taylim (e var val flag)
    (declare (ignore flag))
	(let ((et nil))
	  (when (tlimp e var)
		 (setq e (stirling0 e))
	     (setq et (tlimit-taylor e var (ridofab val) $lhospitallim 0)))
	  (if et (let ((taylored t)) (limit et var val 'think)) (limit1 e var val))))
