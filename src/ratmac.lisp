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
(macsyma-module ratmac macro)

;; Macros for manipulating rational functions.

(DEFMACRO PCOEFP (E) `(ATOM ,E))



#-CL
(DEFMACRO PZEROP (X) `(SIGNP E ,X))			;TRUE FOR 0 OR 0.0
;;;(DEFMACRO PZEROP (X) `(LET ((gg1032 ,X)) (AND (NUMBERP gg1032) (ZEROP gg1032))))

(proclaim '(inline pzerop))
#+CL
(defun pzerop (x) (and (numberp x) (zerop x)))


#+CL
(DEFMACRO PZERO () 0)
(DEFMACRO PTZEROP (TERMS) `(NULL ,TERMS))		;for poly terms
(DEFMACRO PTZERO () '())

#-CL
(DEFMACRO CZEROP (C) `(SIGNP E ,C))
#+CL
(defmacro czerop (c) `(pzerop ,c))

(DEFMACRO CMINUS (C) `(MINUS ,C))
(DEFMACRO CMINUSP (C) `(MINUSP ,C))
(DEFMACRO CDERIVATIVE (ign ign1)ign ign1 0)

;; Similar to REMOVE on the Lisp Machine
(DEFMACRO DELET (ITEM LLIST) `(ZL-DELETE ,ITEM (COPY-TOP-LEVEL ,LLIST )))

;; the rational function package uses GENSYM's to represent variables.
;; The PDP-10 implementation used to use the PRINTNAME of the gensym
;; as a place to store a VALUE. Somebody changed this to value-cell instead,
;; even though using the value-cell costs more. Anyway, in NIL I want it
;; to use the property list, as thats a lot cheaper than creating a value
;; cell. Actually, better to use the PACKAGE slot, a kludge is a kludge right?

(DEFMACRO VALGET (ITEM)
  #+NIL `(GET ,ITEM 'GENSYMVAL)
  #-NIL `(SYMBOL-VALUE ,ITEM))

(DEFMACRO VALPUT (ITEM VAL)
  `(SETF (VALGET ,ITEM) ,VAL))

(proclaim '(inline pointergp))
(DEFun POINTERGP (A B) (f> (valget A) (VALGET B)))

;(macro ALGV (L) `(AND $ALGEBRAIC (GET ,(CADR L) 'TELLRAT)))
(defmacro algv (v)
  `(and $algebraic (get ,v 'tellrat)))


(DEFMACRO EQN (&REST L) `(EQUAL . ,L))

(DEFMACRO RZERO () ''(0 . 1))
(DEFMACRO RZEROP (A) `(PZEROP (CAR ,A)))

(defmacro PRIMPART (p) `(cadr (oldcontent ,p)))

;;poly constructor

(defmacro make-poly (var &optional (terms-or-e nil options?) (c nil e-c?)
			 (terms nil terms?))
  (cond ((null options?) `(cons ,var '(1 1)))
	((null e-c?) `(psimp ,var ,terms-or-e))
	((null terms?) `(list ,var ,terms-or-e ,c))
	(t `(psimp ,var (list* ,terms-or-e ,c ,terms)))))

;;Poly selector functions

(defmacro P-VAR (p) `(car ,p))

(defmacro P-TERMS (p) `(cdr ,p))

(defmacro P-LC (p) `(caddr ,p))			;leading coefficient

(defmacro P-LE (p) `(cadr ,p))

(defmacro P-RED (p) `(cdddr ,p))

;;poly terms selectors

(defmacro PT-LC (terms) `(cadr ,terms))

(defmacro PT-LE (terms) `(car ,terms))

(defmacro PT-RED (terms) `(cddr ,terms))

;; Taken from SININT and RISCH.  Somebody document these please.

(DEFMACRO R+ (R . L)
	  (COND ((NULL L) R)
		(T `(RATPL ,R (R+ ,@L)))))

(DEFMACRO R* (R . L)
	  (COND ((NULL L) R)
		(T `(RATTI ,R (R* ,@L) T))))

(DEFMACRO R- (R . L)
	  (COND ((NULL L) `(RATMINUS (RATFIX ,R)))
		(T `(RATDIF (RATFIX ,R) (R+ ,@L)))))


(defvar $ratvarswitch t)

;(defvar *rational-function-files* '(
;ratmac
;rat3a
;rat3b
;rat3c
;rat3e
;nrat4
;ratout
;lesfac
;factor
;algfac
;nalgfa
;newfac
;ufact
;result
;spgcd))
