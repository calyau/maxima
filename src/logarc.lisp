;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module logarc)

;;;  Logarithmic form of inverse-trig and trig functions.

(defmfun $logarc (exp)
  (cond ((atom exp) exp)
	((arcp (caar exp)) (logarc (caar exp) ($logarc (cadr exp))))
	((eq (caar exp) '%atan2)
	 (logarc '%atan2 (list ($logarc (second exp)) ($logarc (third exp)))))
	(t (recur-apply #'$logarc exp))))

(defun logarc (f x)
  ;; Gives the logarithmic form of arc trig and hyperbolic functions
  (cond ((eq f '%acos)
	 ;; -%i * log(x + %i*sqrt(1-x^2))
	 (mul -1 '$%i (take '(%log) (add x (mul '$%i (root (sub 1 (power x 2)) 2))))))
	((eq f '%asin)
	 ;; -%i * log(sqrt(1-x^2)+%i*x)
	 (mul -1 '$%i (take '(%log) (add (mul '$%i x) (root (sub 1 (power x 2)) 2)))))
	((eq f '%atan)
	 ;; (log(1 + %i*x) - log(1 - %i*x)) /(2 %i)
	 (div (sub (take '(%log) (add 1 (mul '$%i x))) (take '(%log) (sub 1 (mul '$%i x))))
	      (mul 2 '$%i)))
	((eq f '%atan2)
	 ;; atan2(y,x) = -%i*log((x + %i*y)/sqrt(x^2+y^2))
	 (destructuring-bind (y x)
	     x
	   (mul -1 '$%i
	        (take '(%log) (div (add x (mul '$%i y))
	                           (root (add (mul x x) (mul y y)) 2))))))
    	((eq f '%asinh)
	 ;; log(sqrt(x^2+1)+x)
	 (take '(%log) (add x (root (add 1 (power x 2)) 2))))
	((eq f '%acosh)
         ;; log(x+sqrt(x-1)*sqrt(x+1))
         (take '(%log) (add x (mul (root (add x -1) 2) (root (add x 1) 2)))))
    	((eq f '%atanh)
	 ;; (log(x+1)-log(1-x))/2
	 (div (sub (take '(%log) (add 1 x)) (take '(%log) (sub 1 x))) 2))
    	((member f '(%asec %acsc %acot %asech %acsch %acoth) :test #'eq)
	 ;; asec(x) = acos(1/x), and etc.
	 (logarc (zl-get (zl-get (get f '$inverse) 'recip) '$inverse) (inv x)))
	(t (merror "LOGARC: unrecognized argument: ~M" f))))

;; Conditionally apply a logarc transformation to operators that have the 
;; arcp property but are *not* members of the list `l`. Note: %atan2 is not converted.
;; We could blend this functionality into $logarc, but I'm not sure there is much
;; demand for it.
(defun partial-logarc (e l)
  (cond ((atom e) e)
        ((and (arcp (caar e)) (not (member (caar e) l)))
          (logarc (caar e) (partial-logarc (cadr e) l)))
        (t (recur-apply #'(lambda (q) (partial-logarc q l)) e))))
