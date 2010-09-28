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
;;; Bigfloat version by rtoy.

(macsyma-module intpol)

(load-macsyma-macros transm)

(defmvar $find_root_abs 0.0
  "Desired absolute error in the root found by find_root")
(defmvar $find_root_rel 0.0
  "Desired relative error in the root found by find_root")
(defmvar $bf_find_root_abs (bcons (intofp 0))
  "Desired absolute error in the root found by bf_find_root")
(defmvar $bf_find_root_rel (bcons (intofp 0))
  "Desired relative error in the root found by bf_find_root")
(defmvar $find_root_error t
  "If true, find_root and bf_find_root prints an error message.
  Otherwise the value of find_root_error is returned.")

(defmspec $interpolate (form)
  (format t "NOTE: The interpolate function has been renamed to find_root.
The variables intpolabs, intpolrel, and intpolerror have been renamed
to find_root_abs, find_root_rel, and find_root_error, respectively.
Perhaps you meant to enter `~a'.~%"
	  (print-invert-case (implode (mstring `(($find_root) ,@(cdr form))))))
  '$done)

(in-package "BIGFLOAT")

;; Define FIND-ROOT-SUBR and INTERPOLATE-CHECK in the BIGFLOAT package
;; so we don't have to write BIGFLOAT::foo for all of the arithmetic
;; operations.

(defun find-root-subr (f left right abserr relerr)
  (flet ((convert (s)
	   ;; Try to convert to a BIGFLOAT type.  If that fails, just
	   ;; return the argument.  Set the flags errcatch and erromsg
	   ;; so we can catch the error, but disable printing of any
	   ;; error messages.
	   (let ((maxima::errcatch t)
		 (maxima::$errormsg nil))
	     (or (car (maxima::errset (to s)))
		 s))))
    (let ((maxima::$numer t)
	  (maxima::$%enumer t))
      (setq left (convert left)
	    right (convert right)))
    (unless (and (numberp left) (numberp right))
      ;; The interval boundaries must have numerical values
      (return-from find-root-subr (values nil left right)))
    (when (< right left)
      ;; Make left the lower and right the upper bound of the interval
      (psetq left right right left))
    (let ((lin 0) (a left) (b right)
	  (fa (convert (funcall f (maxima::to left))))
	  (fb (convert (funcall f (maxima::to right)))) c fc)
      (unless (and (numberp fa) (numberp fb))
	(return-from find-root-subr (values nil a b)))
      (when (<= (abs fa) (to abserr))
	;; If a or b is already small enough, return it as the root
	(return-from find-root-subr a))
      (when (<= (abs fb) (to abserr))
	(return-from find-root-subr b))
      (when (plusp (* fa fb))
	(if (eq maxima::$find_root_error t)
	    (maxima::merror (intl:gettext "find_root: function has same sign at endpoints: ~M, ~M")
			    `((mequal) ((f) ,a) ,fa)
			    `((mequal) ((f) ,b) ,fb))
	    (return-from find-root-subr 'maxima::$find_root_error)))
      (when (plusp fa)
	(psetq fa fb
	       fb fa
	       a b
	       b a))
      ;; Use binary search to close in on the root
      (loop while (< lin 3) do 
	   (setq c (* 0.5 (+ a b))
		 fc (convert (funcall f (maxima::to c))))
	   (unless (numberp fc)
	     (return-from find-root-subr (values nil a b)))
	   (when (interpolate-check a c b fc abserr relerr)
	     (return-from find-root-subr c))
	   (if (< (abs (- fc (* 0.5 (+ fa fb)))) (* 0.1 (- fb fa)))
	       (incf lin)
	       (setq lin 0))
	   (if (plusp fc)
	       (setq fb fc b c)
	       (setq fa fc a c)))
      ;; Now use the regula falsi
      (loop				
	 (setq c (if (plusp (+ fb fa))
		     (+ a (* (- b a) (/ fa (- fa fb))))
		     (+ b (* (- a b) (/ fb (- fb fa)))))
	       fc (convert (funcall f (maxima::to c))))
	 (unless (numberp fc)
	   (return-from find-root-subr (values nil a b)))
	 (when (interpolate-check a c b fc abserr relerr)
	   (return-from find-root-subr c))
	 (if (plusp fc)
	     (setq fb fc b c)
	     (setq fa fc a c))))))

(defun interpolate-check (a c b fc abserr relerr)
  (not (and (prog1
		(> (abs fc) (to abserr))
	      (setq fc (max (abs a) (abs b))))
	    (> (abs (- b c)) (* (to relerr) fc))
	    (> (abs (- c a)) (* (to relerr) fc)))))

(in-package "MAXIMA")
(defun %find-root (name fun-or-expr args)
  (multiple-value-bind (abserr relerr coerce-float fl)
      ;; The name tells us what error values to use, how to coerce the
      ;; function, and what function to use to convert to the desired
      ;; float type.
      (ecase name
	($find_root
	 (values $find_root_abs $find_root_rel 'coerce-float-fun '$float))
	($bf_find_root
	 (values $bf_find_root_abs $bf_find_root_rel 'coerce-bfloat-fun '$bfloat)))
    (case (length args)
      (2
       ;; function case: f, lo, hi
       (multiple-value-bind (result left right)
	   (bigfloat::find-root-subr (funcall coerce-float fun-or-expr)
				     (funcall fl (first args))
				     (funcall fl (second args))
				     abserr
				     relerr)
	 (if (bigfloat:numberp result)
	     (to result)
	     (if (eq result '$find_root_error)
		 $find_root_error
		 `((,name) ,fun-or-expr ,(to left) ,(to right))))))
      (3
       ;; expr case: expr, var, lo, hi
       (multiple-value-bind (result left right)
	   (bigfloat::find-root-subr (funcall coerce-float (sub ($lhs fun-or-expr) ($rhs fun-or-expr))
					      `((mlist) ,(first args)))
				     (funcall fl (second args))
				     (funcall fl (third args))
				     abserr
				     relerr)
	 (if (bigfloat:numberp result)
	     (to result)
	     (if (eq result '$find_root_error)
		 $find_root_error
		 `((,name) ,fun-or-expr ,(first args) ,(to left) ,(to right))))))
      (t
       ;; wrong number of args
       (wna-err name)))))

(defun $find_root (fun-or-expr &rest args)
  (%find-root '$find_root fun-or-expr args))

;; Like find_root but operates on bfloats and returns a bfloat result.
(defun $bf_find_root (fun-or-expr &rest args)
  (%find-root '$bf_find_root fun-or-expr args))
