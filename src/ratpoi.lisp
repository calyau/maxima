;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1980 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module ratpoi)

(DECLARE-top (SPECIAL $RATVARS POISCOM1 POISHIFT $LIM))

(DECLARE-top (SPECIAL $GCDOFF $RATEXPAND GENVAR VARLIST POISCO1 POISCOM1 GEN TR TLIST)) 

(SETQ POISCO1 '(1. . 1.) POISCOM1 '(-1. . 1.)) 

;;; THESE PROGRAMS MAKE POISSON COEFFICIENTS RATIONAL FUNCTIONS (CRE)
;;; POISCDECODE DECODES A COEFFICIENT

(DEFUN POISCDECODE (X) 
       ($RATDISREP (CONS (LIST 'MRAT 'SIMP (CDR $RATVARS) GENVAR) X))) 

;;; INTOPOISCO PUTS AN EXPRESSION INTO POISSON COEFFICIENT FORM

;(defmacro nonperiod (&rest p)
;  (setq p (cons 'nonperiod p))
;  (AND (NULL (CADR P)) (EQUAL (CAADDR P) POISHIFT) (NULL (CDDR (CADDR P)))))

(DEFUN INTOPOISCO (X) 
       (COND ((AND (NOT (ATOM X)) (NUMBERP (CDR X))) X)
             (T (CDR(RATREP X (CDR $RATVARS))))))

;;; POISCO+ ADDS 2 COEFFICIENTS
;;; POISCO* MULTIPLIES 2 COEFFICIENTS

(DEFUN POISCO* (X Y) (RATTIMES X Y T)) 

(DEFUN POISCO+ (X Y) (RATPLUS X Y)) 

;;; HALVE DIVIDES A COEFFICIENT BY 2

(DEFUN HALVE (R) (RATTIMES '(1. . 2.) R T)) 

;;; POISSUBSTCO SUBSTITUTES AN EXPRESSION FOR  A VARIABLE IN A COEFFICIENT.

(DEFUN POISSUBSTCO (A B X) 
       (INTOPOISCO (MAXIMA-SUBSTITUTE A
                               B
                               ($RATDISREP (CONS (LIST 'MRAT
                                                       'SIMP
 (CDR $RATVARS) GENVAR)
                                                 X))))) 

(DEFUN POISPZERO (X) (EQUAL 0. (CAR X))) 


;;; TEST FOR ZERO

(DEFUN POISCOINTEG (H VAR) (INTOPOISCO ($INTEGRATE (POISCDECODE H) VAR))) 

