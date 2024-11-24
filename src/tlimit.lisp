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

;; We have subst([h=0,x=0], taylor(asin(x+h)-asin(x),h,0,1)) = %pi (see
;; bug #4416 limit of Newton quotient involving asin). This bug causes
;; trouble for tlimit((asin(x+h) - asin(x))/h,h,0). Until such bugs are
;; sorted, we will disallow tlimit from attempting limits involving 
;; acos and asin.
(defun tlimp (e x)	
  (or (and ($mapatom e) (not (eq e '$ind)) (not (eq e '$und)))
	  (and (consp e) 
	       (consp (car e)) 
		   (or (not (among x e)) 
		     (not (member (caar e) '($conjugate $ceiling $floor $unit_step %signum %acos %asin) :test #'eq)))
	       (or 
		      (known-ps (caar e)) 
			  (and (eq (caar e) 'mqapply) (known-ps (subfunname e)))
	          (member (caar e) (list 'mplus 'mtimes 'mexpt '%log))
			  (get (caar e) 'grad)
			  ($freeof x e))
		    (every #'(lambda (q) (tlimp q x)) (cdr e)))))

;; Dispatch Taylor, but recurse on the order until either the recursion
;; depth reaches 15 or the Taylor polynomial is nonzero. If Taylor 
;; fails to find a nonzero Taylor polynomial or the recursion depth 
;; exceeds the limit, return NIL.

;; This recursion on the order attempts to handle limits such as 
;; tlimit(2^n / n^5, n, inf) correctly.

;; We set up a reasonable environment for calling Taylor. When $taylor_logexpand 
;; is false, Taylor sometimes generates expressions that vanish but do not readily
;; simplify to zero. For example:
;;
;;   block([taylor_logexpand: false], subst(h=0, taylor(atan(x+h) - atan(x), h, 0, 1)));
;;
;; For the limit code, such expressions can cause errors. To avoid this, we set 
;; taylor_logexpand to true. Previously, this code set the value of $taylor_simplifier
;; to  #'(lambda (q) (sratsimp (extra-simp q)))), but that doesn't seem to be needed,
;; so this version doesn't set a value for this option variable.

;; Also previously, this code applied partial-logarc before calling $taylor. This
;; too doesn't seem to be needed, so it has been removed.	

;; There is no compelling reason to default the Taylor order to 
;; lhospitallim, but this is documented in the user documentation.

(defun tlimit-taylor (e x pt n &optional (d 0))
	(let ((ee) 
	      (silent-taylor-flag t) 
	      ($taylordepth 8)
		  ($radexpand nil)
		  ($taylor_logexpand t)
		  ($logexpand t))
	    (setq ee (catch 'taylor-catch (ratdisrep ($taylor e x pt n))))
		(cond ((and ee (not (alike1 ee 0))) ee)
			  ;; Retry if taylor returns zero and depth is less than 16
              ((and ee (< d 16))
			    (tlimit-taylor e x pt (* 4 (max 1 n)) (1+ d)))
			  (t nil))))

;; Previously, when the Taylor series failed, there was code to decide
;; whether to call limit1 or simplimit. The choice depended on the last
;; argument to taylim (previously named *i*) and the main operator of the 
;; expression. This updated code eliminates that logic and always dispatches
;; limit1 when Maxima is unable to find the Taylor polynomial. As a result, 
;; the last argument of taylim is now unused (orphaned).
(defun taylim (e var val flag)
    (declare (ignore flag))
	(let ((et nil))
	  (when (tlimp e var)
		 (setq e (stirling0 e))
	     (setq et (tlimit-taylor e var (ridofab val) $lhospitallim 0)))
	  (if et (let ((taylored t)) (limit et var val 'think)) (limit1 e var val))))
