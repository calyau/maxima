;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;; Maclisp compatibility definitions for the Lisp Machine.  This file
;; is for Lisp differences only.  No knowledge of Macsyma should be
;; contained in this file.

;; Translated code still uses this.
(defquote includef (  filename) filename)

;; Couldn't this copy to some static area?
(defmacro purcopy (x) x)

#-nocp
(defmacro charpos (ignor)ignor  '(cdr (cursorpos)))
(defmacro maxima-sleep (seconds) `(process-sleep (fix (f* ,seconds 60.))))

(defun linel (&optional stream)
  #+lispm 
  (funcall (si:decode-print-arg stream) ':size-in-characters)
  #-lispm stream
  #-lispm 80 
  )

;; Perhaps this should do something.
(defmacro nointerrupt (ignor)ignor  nil)

(defmacro without-tty-interrupts (&rest form)
  `(let (#+lispm (tv:kbd-intercepted-characters nil))
    ,@ form))

(defmacro fixnum-identity (x) x)
(defmacro flonum-identity (x) x)

(proclaim '(inline *quo *dif))
#+cl
(defun *quo (x y)
  (cond ((and (integerp x) (integerp y))
	 (truncate x y))
	(t (/ x y))))
#+cl
(defun *dif (x y) (- x y))


;; Keep the compiler quiet.
;; Use GET-PNAME or FORMAT instead of EXPLODE, EXPLODEN, EXPLODEC.
;; Use AREF instead of GETCHAR and GETCHARN.
;; Use MAKE-SYMBOL instead of MAKNAM.
;; Use INTERN instead of IMPLODE.
;; Use STRING-LESSP instead of ALPHALESSP.
#+lispm 
(progn 
  (remprop 'explode       'compiler:style-checker)
  (remprop 'explodec      'compiler:style-checker)
  (remprop 'exploden      'compiler:style-checker)
  (remprop 'alphalessp    'compiler:style-checker)
  (remprop 'getcharn      'compiler:style-checker)
  (remprop 'getchar       'compiler:style-checker)
  (remprop 'implode       'compiler:style-checker)
  (remprop 'maknam        'compiler:style-checker)
  )

(defmacro sfa-call (stream operation arg)
  `(funcall ,stream (read-from-string (string-append #\: ,operation)) ,arg))

;; Things that appear within DECLARE bodies.

;; Why doesn't this work?
;; Because of the brain-damaged way the lispm compiler is written. -gjc
;; (PUTPROP 'DECLARE '(DECLARE-OPTIMIZER) 'COMPILER:OPTIMIZERS)
;;
;; (DEFUN DECLARE-OPTIMIZER (DECLARE-FORM &AUX (RETURN-FORM NIL))
;;   (DO ((L (REVERSE (CDR DECLARE-FORM)) (CDR L)))
;;       ((NULL L))
;;     (IF (NOT (MEMQ (CAAR L)
;; 		   '(FIXNUM FLONUM NOTYPE MACROS ARRAY* GENPREFIX
;; 			    CLOSED MUZZLED MAPEX SPLITFILE)))
;; 	(PUSH (CAR L) RETURN-FORM)))
;;   (IF RETURN-FORM (CONS 'DECLARE RETURN-FORM) NIL))

;; These are in global, so avoid redefinition warning by using FDEFINE
;; rather than DEFun.

;;(FDEFINE (kw FLONUM) #'(LAMBDA ( &REST IGNOR)ignor  NIL))
;;(FDEFINE (kw FIXNUM) #'(LAMBDA (&QUOTE &REST IGNOR)ignor  NIL))
;;(FDEFINE 'ARGS	 #'(LAMBDA (&QUOTE &REST IGNOR)ignor  NIL))

(defmacro args (&rest ignor)ignor  nil)

#-cl
(progn
  (defmacro flonum (&rest ignor)ignor  nil)
  (defmacro fixnum (&rest ignor)ignor  nil)
  (defmacro macros	     ( &rest ignor)ignor  nil)
  (defmacro closed	     ( &rest ignor)ignor  nil)
  (defmacro notype	     ( &rest ignor)ignor  nil)
  (defmacro array*	     ( &rest ignor)ignor  nil)
  (defmacro genprefix     ( &rest ignor)ignor  nil)
  (defmacro muzzled	     ( &rest ignor)ignor  nil)
  (defmacro mapex	     ( &rest ignor)ignor  nil)
  (defmacro splitfile     ( &rest ignor)ignor  nil)
  (defmacro expr-hash     ( &rest ignor)ignor  nil)
  )
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

;;(DEFUN QUOTED-ARGS (&QUOTE &REST L)
;;  (MAPCAR #'(LAMBDA (X) (PUTPROP X '((1005 (FEF-ARG-OPT FEF-QT-QT))) 'ARGDESC))
;;	  L))



(defmacro quote-args ( &rest l)
  `(quoted-args-aux ',l))
(defun quoted-args-aux (l)
  (mapcar #'(lambda (x) (putprop x '((1005 (fef-arg-opt fef-qt-qt))) 'argdesc))
	  l))


#+cl
(progn 'compile

;;; On the 3600, STORE isn't implemented.  So, implement enough of
;;; it here to satisfy the cases the Macsyma uses.  I have yet to find
;;; it using complicated side effects of the array reference -- it's either
;;; a (STORE (ARRAYCALL ...) ...) or a (STORE (FUNCALL ...) ...) or else
;;; a (STORE (array-called-as-function ...) ...).  So, assume that if the CAR
;;; of the first form isn't ARRAYCALL or FUNCALL, then it's a STORE of the third
;;; form.

       (defun store-macro-helper (array-ref new-value)
	 ;;this is redundant and should be caught by store but a bug in compiler..
	 (cond ((or (eql (car array-ref) 'aref))(equal (car array-ref) '(function aref))
		`(setf (aref ,@ (cdr array-ref)) ,new-value))
	       (t
		(case (length array-ref)
		  (2 `(store-internal-1d ,@array-ref ,new-value))
		  (3 `(store-internal-2d ,@array-ref ,new-value))
		  (otherwise (error "Cannot expand STORE for array reference ~S" array-ref))))))



       (defmacro store (array-ref new-value &aux expand-1 &environment env)
	 (cond ((not (memq (car array-ref ) '(aref arraycall)))
		(setq expand-1 (macroexpand-1 array-ref env))
		(setq array-ref
		      (cond ((memq (car expand-1 ) '(aref arraycall))
			     expand-1)
			    (t  (macroexpand array-ref env))))))
  
	 (case (first array-ref)
	   (funcall (store-macro-helper (cdr array-ref) new-value))
	   ;;    (ARRAYCALL (STORE-MACRO-HELPER (CDDR ARRAY-REF) NEW-VALUE))
	   ;;the arrays ought to all be on in the symbol location by now --wfs
	   (arraycall `(setf ,array-ref ,new-value))
	   (aref `(setf ,array-ref ,new-value))
	   (otherwise (store-macro-helper `(#',(first array-ref) . ,(cdr array-ref)) new-value))))


       (defun store-internal-1d (array-spec index new-value)
	 (sloop until (arrayp array-spec)
		do (cond ((symbolp array-spec) (setq array-spec (symbol-array array-spec)))
			 (t (error "STORE failed -- can't find array for ~S" array-spec))))
	 (setf (aref array-spec index) new-value))

       (defun store-internal-2d (array-spec i1 i2 new-value)
	 (sloop until (arrayp array-spec)
		do (cond ((symbolp array-spec) (setq array-spec (symbol-array array-spec)))
			 (t (error "STORE failed -- can't find array for ~S" array-spec))))
	 (setf (aref array-spec i1 i2) new-value))

       )				;End PROGN 'COMPILE




