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
(macsyma-module ratmac macro)

;; Macros for manipulating rational functions.

(defmacro pcoefp (e) `(atom ,e))



#-cl
(defmacro pzerop (x) `(signp e ,x))	;TRUE FOR 0 OR 0.0
;;;(DEFMACRO PZEROP (X) `(LET ((gg1032 ,X)) (AND (NUMBERP gg1032) (ZEROP gg1032))))

(proclaim '(inline pzerop))
#+cl
(defun pzerop (x) (if (fixnump x) (zerop (the fixnum x))
		      (if (consp x) nil
			  (and (floatp x) (zerop x)))))

#+cl
(defmacro pzero () 0)
(defmacro ptzerop (terms) `(null ,terms)) ;for poly terms
(defmacro ptzero () '())

#-cl
(defmacro czerop (c) `(signp e ,c))
#+cl
(defmacro czerop (c) `(pzerop ,c))

(defmacro cminus (c) `(minus ,c))
(defmacro cminusp (c) `(minusp ,c))
(defmacro cderivative (ign ign1)ign ign1 0)

;; Similar to REMOVE on the Lisp Machine
(defmacro delet (item llist) `(zl-delete ,item (copy-top-level ,llist )))

;; the rational function package uses GENSYM's to represent variables.
;; The PDP-10 implementation used to use the PRINTNAME of the gensym
;; as a place to store a VALUE. Somebody changed this to value-cell instead,
;; even though using the value-cell costs more. Anyway, in NIL I want it
;; to use the property list, as thats a lot cheaper than creating a value
;; cell. Actually, better to use the PACKAGE slot, a kludge is a kludge right?

(defmacro valget (item)
  #+nil `(get ,item 'gensymval)
  #-nil `(symbol-value ,item))

(defmacro valput (item val)
  `(setf (valget ,item) ,val))

(proclaim '(inline pointergp))
(defun pointergp (a b) (f> (valget a) (valget b)))

;;(macro ALGV (L) `(AND $ALGEBRAIC (GET ,(CADR L) 'TELLRAT)))
(defmacro algv (v)
  `(and $algebraic (get ,v 'tellrat)))


(defmacro eqn (&rest l) `(equal . ,l))

(defmacro rzero () ''(0 . 1))
(defmacro rzerop (a) `(pzerop (car ,a)))

(defmacro primpart (p) `(cadr (oldcontent ,p)))

;;poly constructor

(defmacro make-poly (var &optional (terms-or-e nil options?) (c nil e-c?)
		     (terms nil terms?))
  (cond ((null options?) `(cons ,var '(1 1)))
	((null e-c?) `(psimp ,var ,terms-or-e))
	((null terms?) `(list ,var ,terms-or-e ,c))
	(t `(psimp ,var (list* ,terms-or-e ,c ,terms)))))

;;Poly selector functions

(defmacro p-var (p) `(car ,p))

(defmacro p-terms (p) `(cdr ,p))

(defmacro p-lc (p) `(caddr ,p))		;leading coefficient

(defmacro p-le (p) `(cadr ,p))

(defmacro p-red (p) `(cdddr ,p))

;;poly terms selectors

(defmacro pt-lc (terms) `(cadr ,terms))

(defmacro pt-le (terms) `(car ,terms))

(defmacro pt-red (terms) `(cddr ,terms))

;; Taken from SININT and RISCH.  Somebody document these please.

(defmacro r+ (r . l)
  (cond ((null l) r)
	(t `(ratpl ,r (r+ ,@l)))))

(defmacro r* (r . l)
  (cond ((null l) r)
	(t `(ratti ,r (r* ,@l) t))))

(defmacro r- (r . l)
  (cond ((null l) `(ratminus (ratfix ,r)))
	(t `(ratdif (ratfix ,r) (r+ ,@l)))))


(defvar $ratvarswitch t)

;;(defvar *rational-function-files* '(
;;ratmac
;;rat3a
;;rat3b
;;rat3c
;;rat3e
;;nrat4
;;ratout
;;lesfac
;;factor
;;algfac
;;nalgfa
;;newfac
;;ufact
;;result
;;spgcd))
