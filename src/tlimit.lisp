;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

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

(defun tlimp (expr)		; TO BE EXPANDED TO BE SMARTER (MAYBE)
  (declare (ignore expr))
  t)

;; compute limit of exp by finding its taylor series expansion.
;; asks for $lhospitallim terms of taylor series.
;; this is an arbitrary limit: with default value $lhospitallim = 4,
;;  tlimit(2^n/n^5, n, inf)  =>  0
(defun taylim (exp var val *i*)
  (prog (ex)
     (setq ex (catch 'taylor-catch
		(let ((silent-taylor-flag t))
		  (declare (special silent-taylor-flag))
		  ($taylor exp var (ridofab val) $lhospitallim))))
     (or ex (return (cond ((eq *i* t)
			   (limit1 exp var val))
			  ((eq *i* 'think)
			   (if (member (caar exp) '(mtimes mexpt) :test #'eq)
			       (limit1 exp var val)
			       (simplimit exp var val)))
			  (t
			   (simplimit exp var val)))))
     (return
       (let ((taylored t))
	 (declare (special taylored))
	 (limit (simplify ($ratdisrep ex)) var val 'think)))))
