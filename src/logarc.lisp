;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module logarc)

;;;  Logarc and Halfangles

(defmfun $logarc (exp)
  (cond ((atom exp) exp)
	((arcp (caar exp)) (logarc (caar exp) ($logarc (cadr exp))))
	((eq (caar exp) '$atan2)
	 (logarc '%atan ($logarc (div (cadr exp) (caddr exp)))))
	(t (recur-apply #'$logarc exp))))

(defmfun logarc (f x)
  ;;Gives the logarithmic form of arc trig and hyperbolic functions

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
    	((eq f '%asinh)
	 ;; log(sqrt(x^2+1)+x)
	 (take '(%log) (add x (root (add 1 (power x 2)) 2))))
	((eq f '%acosh)
	 ;; 2 * log(sqrt((x+1)/2)+sqrt((x-1)/2))
	 (mul 2 (take '(%log) (add (root (div (add x 1) 2) 2) (root (div (add x -1) 2) 2)))))
    	((eq f '%atanh)
	 ;;  (log(x+1)-log(1-x))/2
	 (div (sub (take '(%log) (add 1 x)) (take '(%log) (sub 1 x))) 2))
    	((memq f '(%asec %acsc %acot %asech %acsch %acoth))
	 ;; asec(x) = acos(1/x), and etc.
	 (logarc (oldget (oldget (get f '$inverse) 'recip) '$inverse) (inv x)))
	(t (merror "Bad argument to 'logarc'"))))

(defmfun halfangle (f a)
  (and (mtimesp a)
       (ratnump (cadr a))
       (equal (caddr (cadr a)) 2)
       (halfangleaux f (mul 2 a))))

(defun halfangleaux (f a) ;; f=function; a=twice argument
  (let ((sw (memq f '(%cos %cot %coth %cosh))))
    (cond ((memq f '(%sin %cos))
	   (power (div (add 1 (porm sw (take '(%cos) a))) 2) (1//2)))
	  ((memq f '(%tan %cot))
	   (div (add 1 (porm sw (take '(%cos) a))) (take '(%sin) a)))
	  ((memq f '(%sinh %cosh))
	   (power (div (add (take '(%cosh) a) (porm sw 1)) 2) (1//2)))
	  ((memq f '(%tanh %coth))
	   (div (add (take '(%cosh) a) (porm sw 1)) (take '(%sinh) a)))
	  ((memq f '(%sec %csc %sech %csch))
	   (inv (halfangleaux (get f 'recip) a))))))

