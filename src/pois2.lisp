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

(macsyma-module pois2)

(declare-top (special *argc *coef poisvals poisco1 poiscom1 b* a* *a ss
		      cc h* poishift poistsm poissiz poists $wtlvl $poisz $pois1))

(defvar trim nil)

(defmspec mpois (x) x)

(declare-top (special *b *fn))

(defun poislim1 (uu n)
  (declare (ignore uu))
  (unless  (fixnump n)
    (merror "Improper argument to `poislim':~%~M" n))
  (setq poisvals nil)
  (setq poists (ash 1 n))
  (do ((j 0 (1+ j))) ((> j 5))
    (push (expt poists j) poisvals))
  (setq poissiz n
	poistsm (expt 2 (1- n))
	poishift (prog (sum)
		    (setq sum 0)
		    (do ((i 0 (1+ i)))
			((> i 5))
		      (incf sum (* poistsm (expt poists i))))
		    (return sum))
	$poisz '((mpois simp) nil nil)
	$pois1 (list '(mpois simp) nil (list poishift 1)))
  n)


(defun nonperiod (p)
  (and (null (cadr p))
       (equal (caaddr p) poishift)
       (null (cddr (caddr p)))))

(declare-top (special ans trim poiscom1 poishift dc ds *ans *argc *coef))

(poislim1 nil 5)

(setq poisco1 1 poiscom1 -1)
