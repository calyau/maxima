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

(declare-top (special $find_root_rel $find_root_abs $find_root_error)
	     (flonum $find_root_rel $find_root_abs a b c fa fb fc)
	     (fixnum lin)
	     (notype (interpolate-check flonum flonum flonum flonum))) 

(comment  |  For historical information ONLY.  |
	  (defun fmeval2 (x) 
	    (cond ((integerp (setq x (meval x))) (float x))
		  ((floatp x) x)
		  (t (displa x) (maxima-error '|not floating point|))))
	  (defun qeval (y x z) (cond (x (fmeval2 (list '($ev) y (list '(mequal) x z) '$numer)))
				     (t (funcall y z))))
	  )

(or (boundp '$find_root_abs) (setq $find_root_abs 0.0)) 
(or (boundp '$find_root_rel) (setq $find_root_rel 0.0))
(or (boundp '$find_root_error) (setq $find_root_error t))

(defmspec $interpolate (form)
  (format t
"NOTE: The interpolate function has been renamed to find_root.
The variables intpolabs, intpolrel, and intpolerror have been renamed
to find_root_abs, find_root_rel, and find_root_error, respectively.
Perhaps you meant to enter `~a'.~%" (print-invert-case (implode (mstring `(($find_root) ,@ (cdr form))))))
  '$done)

(defun $find_root_subr (f left right)
  (bind-tramp1$
   f f
   (prog (a b c fa fb fc (lin 0))
      (declare (flonum a b c fa fb fc) (fixnum lin))
      (setq a (float left)
	    b (float right))
      (or (> b a) (setq a (prog2 nil b (setq b a))))
      (setq fa (fcall$ f a)
	    fb (fcall$ f b))
      (or (> (abs fa) $find_root_abs) (return a))
      (or (> (abs fb) $find_root_abs) (return b))
      (and (> (*$ fa fb) 0.0)
	   (cond ((eq $find_root_error t)
		  (merror "function has same sign at endpoints~%~M"
			  `((mlist)
			    ((mequal) ((f) ,a) ,fa)
			    ((mequal) ((f) ,b) ,fb))))
		 (t (return $find_root_error))))
      (and (> fa 0.0)
	   (setq fa (prog2 nil fb (setq fb fa)) a (prog2 nil b (setq b a))))
      (setq lin 0.)
      binary
      (setq c (//$ (+$ a b) 2.0)
	    fc
	    (fcall$ f c))
      (and (interpolate-check a c b fc) (return c))
      (cond ((< (abs (-$ fc (//$ (+$ fa fb) 2.0))) (*$ 0.1 (-$ fb fa)))
	     (setq lin (f1+ lin)))
	    (t (setq lin 0.)))
      (cond ((> fc 0.0) (setq fb fc b c)) (t (setq fa fc a c)))
      (or (= lin 3.) (go binary))
      falsi
      (setq c (cond ((> (+$ fb fa) 0.0)
		     (+$ a (*$ (-$ b a) (//$ fa (-$ fa fb)))))
		    (t (+$ b (*$ (-$ a b) (//$ fb (-$ fb fa)))))) 
	    fc (fcall$ f c))
      (and (interpolate-check a c b fc) (return c))
      (cond ((> fc 0.0) (setq fb fc b c)) (t (setq fa fc a c)))
      (go falsi))))

(defun interpolate-check (a c b fc)
  (not (and (prog2 nil (> (abs fc) $find_root_abs) (setq fc (max (abs a) (abs b))))
	    (> (abs (-$ b c)) (*$ $find_root_rel fc))
	    (> (abs (-$ c a)) (*$ $find_root_rel fc)))))




(defun interpolate-macro (form translp)
  (setq form (cdr form))
  (cond ((= (length form) 3)
	 (cond (translp
		`(($find_root_subr) ,@form))
	       (t
		`((mprog) ((mlist) ((msetq) $numer t))
		  (($find_root_subr)  ,@form)))))
	((= (length form) 4)
	 (destructuring-let (((exp var . bnds) form))
	   (setq exp (sub ($lhs exp) ($rhs exp)))
	   (cond (translp
		  `(($find_root_subr)
		    ((lambda-i) ((mlist) ,var)
		     (($modedeclare) ,var $float)
		     ,exp)
		    ,@bnds))
		 (t
		  `((mprog) ((mlist) ((msetq) $numer t))
		    (($find_root_subr)
		     ((lambda) ((mlist) ,var) ,exp)
		     ,@bnds))))))
	(t (merror "wrong number of args to `interpolate'"))))

(defmspec $find_root (form)
  (meval (interpolate-macro form nil)))

(def-translate-property $find_root (form)
  (let (($tr_numer t))
    (translate (interpolate-macro form t))))


