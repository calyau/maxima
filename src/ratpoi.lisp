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

(macsyma-module ratpoi)

(declare-top (special $ratvars poiscom1 genvar poisco1))

(setq poisco1 '(1 . 1) poiscom1 '(-1 . 1))

;;; THESE PROGRAMS MAKE POISSON COEFFICIENTS RATIONAL FUNCTIONS (CRE)
;;; POISCDECODE DECODES A COEFFICIENT

(defun poiscdecode (x)
  ($ratdisrep (cons (list 'mrat 'simp (cdr $ratvars) genvar) x)))

;;; INTOPOISCO PUTS AN EXPRESSION INTO POISSON COEFFICIENT FORM

(defun intopoisco (x)
  (if (and (not (atom x)) (numberp (cdr x)))
      x
      (cdr(ratrep x (cdr $ratvars)))))

;;; POISCO+ ADDS 2 COEFFICIENTS
;;; POISCO* MULTIPLIES 2 COEFFICIENTS

(defun poisco* (x y)
  (rattimes x y t))

(defun poisco+ (x y)
  (ratplus x y))

;;; HALVE DIVIDES A COEFFICIENT BY 2

(defun halve (r)
  (rattimes '(1 . 2) r t))

;;; POISSUBSTCO SUBSTITUTES AN EXPRESSION FOR  A VARIABLE IN A COEFFICIENT.

(defun poissubstco (a b x)
  (intopoisco
   (maxima-substitute a b
		      ($ratdisrep (cons (list 'mrat 'simp (cdr $ratvars) genvar) x)))))

(defun poispzero (x)
  (equal 0 (car x)))

;;; TEST FOR ZERO

(defun poiscointeg (h var)
  (intopoisco ($integrate (poiscdecode h) var)))
