;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module transf)

(defmvar $tr_float_can_branch_complex t
  "States whether the arc functions might return complex results. The
arc functions are SQRT,LOG,ACOS, etc. e.g. When it is TRUE then
ACOS(X) will be of mode ANY even if X is of mode FLOAT. When FALSE
then ACOS(X) will be of mode FLOAT if and only if X is of mode FLOAT.")


;;; some floating point translations. with tricks.

(defun translate-with-flonum-op (form can-branch-p)
  (let ((arg (translate (cadr form)))
        (lisp-function (gethash (caar form) *flonum-op*)))
    (if (and (eq (car arg) '$float)
             lisp-function)
        (let ((call `(funcall ,lisp-function ,(cdr arg))))
          (if (and can-branch-p
                   $tr_float_can_branch_complex)
              `($any . (complexify ,call))
              `($float . ,call)))
        `($any . (simplify (list '(,(caar form)) ,(cdr arg)))))))

(def%tr %sin (form)
  (translate-with-flonum-op form nil))

(def-same%tr %cos %sin)
(def-same%tr %tan %sin)
(def-same%tr %cot %sin)
(def-same%tr %csc %sin)
(def-same%tr %sec %sin)
(def-same%tr %acot %sin)
(def-same%tr %sinh %sin)
(def-same%tr %cosh %sin)
(def-same%tr %tanh %sin)
(def-same%tr %coth %sin)
(def-same%tr %csch %sin)
(def-same%tr %sech %sin)
(def-same%tr %asinh %sin)
(def-same%tr %acsch %sin)
(def-same%tr %atan %sin)
(def-same%tr %erf %sin)
(def-same%tr %exp %sin)

(def%tr %acos (form)
  (translate-with-flonum-op form t))

(def-same%tr %asin %acos)
(def-same%tr %asec %acos)
(def-same%tr %acsc %acos)
(def-same%tr %acosh %acos)
(def-same%tr %asech %acos)
(def-same%tr %atanh %acos)
(def-same%tr %acoth %acos)
(def-same%tr %log %acos)
(def-same%tr %sqrt %acos)
