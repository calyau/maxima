;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;; Original code by CFFK.  Modified to interface correctly with TRANSL  ;;;
;;; and the rest of macsyma by GJC                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module rombrg)
(load-macsyma-macros transm numerm)

(declare-top(special user-timesofar))

;;; the following code if for historical frame of reference.
;;;(defun fmeval3 (x1) 
;;;       (cond ((integerp (setq x1 (meval x1))) (float x1))
;;;	     ((floatp x1) x1)
;;;	     (t (displa x1) (error '|not floating point|))))
;;;
;;;(defun qeval3 (y1 x1 z)
;;;       (cond (x1 (fmeval3 (list '($ev) y1 (list '(mequal) x1 z) '$numer)))
;;;	     (t (funcall y1 z))))

(defmvar  $rombergit 11. "the maximum number of iterations" fixnum)
(defmvar  $rombergmin 0. "the minimum number of iterations" fixnum)
(defmvar  $rombergtol 1.0e-4 "the relative tolerance of error" flonum)	
(defmvar  $rombergabs 0.0 "the absolute tolerance of error"  flonum)
(defmvar  $rombergit_used 0 "the number of iterations actually used." fixnum)

(defvar romb-print nil )		; " For ^]"


(defun $romberg_subr (function left right)
  (bind-tramp1$
   f function
   (let ((a (float left))
	 (b (float right))
	 (x 0.0)
	 (tt (make-array  $rombergit :element-type 'double-float))
	 (rr  (make-array  $rombergit :element-type 'double-float))
	 (user-timesofar (cons 'romb-timesofar user-timesofar))
	 (romb-print nil))
     (setq x (-$ b a))
     (setf (aref$ tt 0)
	   (*$ x (+$ (fcall$ f b) (fcall$ f a)) 0.5))
     (setf	(aref$ rr 0.)
		(*$ x (fcall$ f (*$ (+$ b a) 0.5))))
     (do ((l 1. (f1+ l)) (m 4. (f* m 2.)) (y 0.0) (z 0.0) (cerr 0.0))
	 ((= l $rombergit)
	  (merror "`romberg' failed to converge"))
       (declare (flonum y z cerr)
		(fixnum l m))
       (setq y (float m) z (//$ x y))
       (setf (aref$ tt l) (*$ (+$ (aref$ tt (f1- l))
				  (aref$ rr (f1- l))) 0.5))
       (setf (aref$ rr l) 0.0)
       (do ((i 1. (f+ i 2.)))
	   ((> i m))
	 (cond (romb-print
		(setq romb-print nil)	;^] magic.
		(mtell "Romberg: ~A iterations; last error =~A;~
			    calculating F(~A)."
		       i
		       cerr
		       (+$ (*$ z (float i)) a))))
	 (setf (aref$ rr l) (+$ (fcall$ f (+$ (*$ z (float i)) a))
				(aref$ rr l))))
       (setf (aref$ rr l) (*$ z (aref$ rr l) 2.0))
       (setq y 0.0)
       (do ((k l (f1- k))) ((= k 0.))
	 (declare (fixnum k))
	 (setq y (+$ (*$ y 4.0) 3.0))
	 (setf (aref$  tt (f1- k))
	       (+$ (//$ (-$ (aref$ tt k)
			    (aref$ tt (f1- k))) y)
		   (aref$ tt k)))
	 (setf (aref$ rr (f1- k))
	       (+$ (//$ (-$ (aref$ rr k)
			    (aref$ rr (f1- k))) y)
		   (aref$ rr k))))
       (setq y (*$ (+$ (aref$ tt 0.)
		       (aref$ rr 0.)) 0.5))
       ;;; this is the WIN condition test.
       (cond ((and
	       (or (not
		    (< $rombergabs
		       (setq cerr
			     (abs (-$ (aref$ tt 0.)
				      (aref$ rr 0.))))))
		   (not (< $rombergtol
			   ;; cerr = "calculated error"; used for ^]
			   (setq cerr (//$ cerr
					   (cond ((= y 0.0) 1.0)
						 (t (abs y))))))))
	       (> l $rombergmin))
	      (setq $rombergit_used l)
	      #+maclisp
	      (progn (*rearray tt) (*rearray rr))
	      (return y)))))))



(defun romb-timesofar () (setq romb-print t)) ;^] function.
;;; Making the ^] scheme work through this special variable makes
;;; it possible to avoid various timing screws and having to have
;;; special variables for communication between the interrupt and MP
;;; function.  On the other hand, it may make it more difficult to
;;; have multiple reports (double integrals etc.).


;;; TRANSL SUPPORT.

(defprop $romberg_subr $float function-mode)

(defun romberg-macro (form translatep)
  (setq form (cdr form))
  (cond ((= (length form) 3)
	 (cond (translatep
		`(($romberg_subr) ,@form))
	       (t
		`((mprog) ((mlist) ((msetq) $numer t) ((msetq) $%enumer t))
		  (($romberg_subr) ,@form)))))
	((= (length form) 4)
	 (destructuring-let (((exp var . bnds) form))
	   (cond (translatep
		  `(($romberg_subr)
		    ((lambda-i) ((mlist) ,var)
		     (($modedeclare) ,var $float)
		     ,exp)
		    ,@bnds))
		 (t
		  `((mprog) ((mlist) ((msetq) $numer t) ((msetq) $%enumer t))
		    (($romberg_subr)
		     ((lambda) ((mlist) ,var) ,exp)
		     ,@bnds))))))
	(t
	 (wna-err '$romberg))))

(defmspec $romberg (form)
  (meval (romberg-macro form nil)))

(def-translate-property $romberg (form)
  (let (($tr_numer t))
    (translate (romberg-macro form t))))
