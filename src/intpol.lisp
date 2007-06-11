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

(load-macsyma-macros transm numerm)

(declare-top (special $find_root_rel $find_root_abs $find_root_error)) 

(or (boundp '$find_root_abs) (setq $find_root_abs 0d0)) 
(or (boundp '$find_root_rel) (setq $find_root_rel 0d0))
(or (boundp '$find_root_error) (setq $find_root_error t))

(defmspec $interpolate (form)
  (format t "NOTE: The interpolate function has been renamed to find_root.
The variables intpolabs, intpolrel, and intpolerror have been renamed
to find_root_abs, find_root_rel, and find_root_error, respectively.
Perhaps you meant to enter `~a'.~%"
	  (print-invert-case (implode (mstring `(($find_root) ,@(cdr form))))))
  '$done)

(defun find-root-subr (f left right)
   (prog (a b c fa fb fc (lin 0))
      (declare (fixnum lin))
      (let (($numer t) ($%enumer t))
        (setq
          ;; Uh-oh. LEFT and RIGHT are evaluated twice !! So burn me at the stake already.
          ;; $FLOAT has a special case for exponents; float(%pi^%e) => 3.14^%e, believe it or not.
          ;; But this MEVAL formulation tries harder, so just do it.
          a (meval `(($float) ,left))
          b (meval `(($float) ,right))))
      (if (not (and (numberp a) (numberp b)))
        (return (values nil a b)))
      (or (> b a) (setq a (prog1 b (setq b a))))
      (setq fa (funcall f a)
	    fb (funcall f b))
      (if (not (and (numberp fa) (numberp fb)))
        (return (values nil a b)))
      (or (> (abs fa) $find_root_abs) (return a))
      (or (> (abs fb) $find_root_abs) (return b))
      (and (plusp (* fa fb))
	   (cond ((eq $find_root_error t)
		  (merror "function has same sign at endpoints~%~M"
			  `((mlist)
			    ((mequal) ((f) ,a) ,fa)
			    ((mequal) ((f) ,b) ,fb))))
		 (t (return $find_root_error))))
      (and (> fa 0d0)
	   (setq fa (prog2 nil fb (setq fb fa)) a (prog2 nil b (setq b a))))
      (setq lin 0)
      binary
      (setq c (* (+ a b) 0.5d0)
	    fc (funcall f c))
      (if (not (numberp fc))
        (return (values nil a b)))
      (and (interpolate-check a c b fc) (return c))
      (cond ((< (abs (- fc (* (+ fa fb) 0.5d0))) (* 1d-1 (- fb fa)))
	     (incf lin))
	    (t (setq lin 0)))
      (cond ((> fc 0d0) (setq fb fc b c)) (t (setq fa fc a c)))
      (or (= lin 3) (go binary))
      falsi
      (setq c (cond ((plusp (+ fb fa))
		     (+ a (* (- b a) (/ fa (- fa fb)))))
		    (t (+ b (* (- a b) (/ fb (- fb fa)))))) 
	    fc (funcall f c))
      (if (not (numberp fc))
        (return (values nil a b)))
      (and (interpolate-check a c b fc) (return c))
      (cond ((plusp fc) (setq fb fc b c)) (t (setq fa fc a c)))
      (go falsi)))

(defun interpolate-check (a c b fc)
  (not (and (prog1 (> (abs fc) $find_root_abs) (setq fc (max (abs a) (abs b))))
	    (> (abs (- b c)) (* $find_root_rel fc))
	    (> (abs (- c a)) (* $find_root_rel fc)))))

(defun $find_root (&rest args)
  (cond
    ((= (length args) 3)
     (multiple-value-bind
       (result left right)
       (find-root-subr
         (coerce-float-fun (first args))
         (second args)
         (third args))
       (if (numberp result)
         result
         `(($find_root) ,(first args) ,left ,right))))
    ((= (length args) 4)
     (multiple-value-bind
       (result left right)
       (find-root-subr
         (coerce-float-fun
           (sub ($lhs (first args)) ($rhs (first args)))
           `((mlist) ,(second args)))
         (third args)
         (fourth args))
       (if (numberp result)
         result
         `(($find_root) ,(first args) ,(second args) ,left ,right))))
    (t
      (wna-err '$find_root))))

