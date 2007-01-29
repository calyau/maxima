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

(macsyma-module procs macro)

(defun verify-as-subr-argument-list (property l n)
  (if (or (member '&rest l :test #'eq)
	  (member '&optional l :test #'eq))
      (maxima-error "bad argument list for a ~a property. ~a" property l)
      (let ((length (- (length l) (length (member '&aux l :test #'eq)))))
	(if (eq n '*)
	    (if (< length 6)
		length
		(maxima-error (list "argument list too long for a" property "property.") l))
	    (if (= n length)
		length
		(maxima-error "argument list for a ~a property must be ~d long. ~a"
			      property n l))))))

(defun a-def-property (name argl body property n)
  (verify-as-subr-argument-list property argl n)
  `(defun-prop (,name ,property) ,argl ,@body))

(defmacro def-def-property (name sample-arglist)
  `(defmacro ,(symbolconc 'def- name '-property) (name argl . body)
    (a-def-property name argl body ',name
     ',(verify-as-subr-argument-list 'def-def-property sample-arglist '*))))

(defmacro subr-call (f &rest l)
  `(funcall ,f ,@l))
