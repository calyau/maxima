;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;; Maclisp compatibility definitions.
;; This file is for Lisp differences only.  No knowledge of Macsyma should be
;; contained in this file.

(proclaim '(inline *quo))

(defun *quo (x y)
  (cond ((and (integerp x) (integerp y))
	 (truncate x y))
	(t (/ x y))))

;; Run time stuff

(defun symbolconc (&rest syms)
  (intern (apply #'concatenate 'string
		 (mapcar #'(lambda (sym)
			     (cond ((floatp sym)
				    (format nil "~S" sym))
				   ((integerp sym)
				    (format nil "~D" sym))
				   ((symbolp sym)
				    (symbol-name sym))
				   (t sym)))
			 syms))))

;; make a symbol out of the printed represantations of all args
(defun concat (&rest args)
  (intern (format nil "~{~A~^~}" args)))

;;; On the 3600, STORE isn't implemented.  So, implement enough of
;;; it here to satisfy the cases the Macsyma uses.  I have yet to find
;;; it using complicated side effects of the array reference -- it's either
;;; a (STORE (ARRAYCALL ...) ...) or a (STORE (FUNCALL ...) ...) or else
;;; a (STORE (array-called-as-function ...) ...).  So, assume that if the CAR
;;; of the first form isn't ARRAYCALL or FUNCALL, then it's a STORE of the third
;;; form.

(defun store-macro-helper (array-ref new-value)
  ;;this is redundant and should be caught by store but a bug in compiler..
  (cond ((or (eql (car array-ref) 'aref) (equal (car array-ref) '(function aref)))
	 `(setf (aref ,@(cdr array-ref)) ,new-value))
	(t
	 (case (length array-ref)
	   (2 `(store-internal-1d ,@array-ref ,new-value))
	   (3 `(store-internal-2d ,@array-ref ,new-value))
	   (otherwise (error "Cannot expand `store' for array reference ~S" array-ref))))))

(defmacro store (array-ref new-value &aux expand-1 &environment env)
  (cond ((not (member (car array-ref) '(aref arraycall) :test #'eq))
	 (setq expand-1 (macroexpand-1 array-ref env))
	 (setq array-ref
	       (cond ((member (car expand-1) '(aref arraycall) :test #'eq)
		      expand-1)
		     (t  (macroexpand array-ref env))))))
  
  (case (first array-ref)
    (funcall (store-macro-helper (cdr array-ref) new-value))
    ;;the arrays ought to all be on in the symbol location by now --wfs
    (arraycall `(setf ,array-ref ,new-value))
    (aref `(setf ,array-ref ,new-value))
    (otherwise (store-macro-helper `(#',(first array-ref) . ,(cdr array-ref)) new-value))))


(defun store-internal-1d (array-spec index new-value)
  (loop until (arrayp array-spec)
     do (cond ((symbolp array-spec) (setq array-spec (symbol-array array-spec)))
	      (t (error "`store' failed -- can't find array for ~S" array-spec))))
  (setf (aref array-spec index) new-value))

(defun store-internal-2d (array-spec i1 i2 new-value)
  (loop until (arrayp array-spec)
     do (cond ((symbolp array-spec) (setq array-spec (symbol-array array-spec)))
	      (t (error "`store' failed -- can't find array for ~S" array-spec))))
  (setf (aref array-spec i1 i2) new-value))
