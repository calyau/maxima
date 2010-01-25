;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;; Interpolation routine by CFFK.

(macsyma-module intpol)

(load-macsyma-macros transm)

(defmvar $find_root_abs 0.0)
(defmvar $find_root_rel 0.0)
(defmvar $find_root_error t)

(defmspec $interpolate (form)
  (format t "NOTE: The interpolate function has been renamed to find_root.
The variables intpolabs, intpolrel, and intpolerror have been renamed
to find_root_abs, find_root_rel, and find_root_error, respectively.
Perhaps you meant to enter `~a'.~%"
	  (print-invert-case (implode (mstring `(($find_root) ,@(cdr form))))))
  '$done)

(defun find-root-subr (f left right)
  (let (($numer t) ($%enumer t))
    (setq left ($float left)
	  right ($float right)))
  (unless (and (numberp left) (numberp right)) ; the interval boundaries must have
    (return-from find-root-subr (values nil left right))) ;numerical values
  (when (< right left) ;make left the lower and right the upper bound of the interval
    (psetq left right right left))
  (let ((lin 0) (a left) (b right) (fa (funcall f left)) (fb (funcall f right)) c fc)
    (unless (and (numberp fa) (numberp fb))
      (return-from find-root-subr (values nil a b)))
    (when (<= (abs fa) $find_root_abs) ;if a or be is already small enough
      (return-from find-root-subr a))  ;return it as the root
    (when (<= (abs fb) $find_root_abs)
      (return-from find-root-subr b))
    (when (plusp (* fa fb))
      (if (eq $find_root_error t)
	  (merror (intl:gettext "find_root: function has same sign at endpoints: ~M, ~M")
		  `((mequal) ((f) ,a) ,fa)
		  `((mequal) ((f) ,b) ,fb))
	  (return-from find-root-subr '$find_root_error)))
    (when (plusp fa)
      (psetq fa fb
	     fb fa
	     a b
	     b a))
    (loop while (< lin 3) do		; use binary search to close in on the root
	 (setq c (* 0.5 (+ a b))
	       fc (funcall f c))
	 (unless (numberp fc)
	   (return-from find-root-subr (values nil a b)))
	 (when (interpolate-check a c b fc)
	   (return-from find-root-subr c))
	 (if (< (abs (- fc (* 0.5 (+ fa fb)))) (* 0.1 (- fb fa)))
	     (incf lin)
	     (setq lin 0))
	 (if (plusp fc)
	     (setq fb fc b c)
	     (setq fa fc a c)))
    (loop				; now use the regula falsi
       (setq c (if (plusp (+ fb fa))
		   (+ a (* (- b a) (/ fa (- fa fb))))
		   (+ b (* (- a b) (/ fb (- fb fa)))))
	     fc (funcall f c))
       (unless (numberp fc)
	 (return-from find-root-subr (values nil a b)))
       (when (interpolate-check a c b fc)
	 (return-from find-root-subr c))
       (if (plusp fc)
	   (setq fb fc b c)
	   (setq fa fc a c)))))

(defun interpolate-check (a c b fc)
  (not (and (prog1
		(> (abs fc) $find_root_abs)
	      (setq fc (max (abs a) (abs b))))
	    (> (abs (- b c)) (* $find_root_rel fc))
	    (> (abs (- c a)) (* $find_root_rel fc)))))

(defun $find_root (fun-or-expr &rest args)
  (case (length args)
    (2					; function case: f, lo, hi
     (multiple-value-bind (result left right)
	 (find-root-subr (coerce-float-fun fun-or-expr)
			 (first args)
			 (second args))
     (if (numberp result)
       result
       (if (eq result '$find_root_error)
         $find_root_error
         `(($find_root) ,fun-or-expr ,left ,right)))))
    (3					;expr case: expr, var, lo, hi
     (multiple-value-bind (result left right)
	 (find-root-subr (coerce-float-fun (sub ($lhs fun-or-expr) ($rhs fun-or-expr))
					   `((mlist) ,(first args)))
			 (second args)
			 (third args))
     (if (numberp result)
       result
       (if (eq result '$find_root_error)
         $find_root_error
         `(($find_root) ,fun-or-expr ,(first args) ,left ,right)))))
    (t					;wrong number of args
     (wna-err '$find_root))))
