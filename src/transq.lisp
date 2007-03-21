;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Compilation environment for TRANSLATED MACSYMA code.        ;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;; this are COMPILE-TIME macros for TRANSLATE MACSYMA code.

(macsyma-module transq macro)

(load-macsyma-macros transm defopt)

(defmacro def-mtrvar (v a &optional (priority 1))
  (declare (ignore priority))
  ;; ignored variable around for TRANSLATED files pre
  ;; 3:03pm  Thursday, 11 March 1982 -gjc
  `(progn
    (declare-top (special ,v))

    (if (or (not (boundp ',v))
	    ;; a SYMBOL SET to ITSELF is considered to be
	    ;; UNBOUND for our purposes in Macsyma.
	    (eq ,v ',v))
	(setq ,v ,a))))

(defvar *max-expt$-expand* 7)

(defopt expt$ (bas exp)
  (if (not (integerp exp))
      `(maxima-error "Internal TRANSL error. ~a ~a" ,bas ,exp))
  (let* ((abs-exp (abs exp))
	 (full-exp (cond ((not (> exp *max-expt$-expand*))
			  `(internal-expt$ ,bas ,abs-exp))
			 (t
			  `(expt ,bas ,abs-exp)))))
    (cond ((minusp exp)
	   `(/ ,full-exp))
	  (t full-exp))))

(defopt internal-expt$ (exp-base pos-exp)
  (cond ((zerop pos-exp)
	 ;; BROM  wrote X^0 for symmetry in his code, and this
	 ;; macro did some infinite looping! oops.
	 ;; X^0 can only happen in hand-written code, in macros
	 ;; the general-representation simplifier will get rid
	 ;; of it.
	 1d0)
	((= pos-exp 1)
	 exp-base)
	((not (atom exp-base))
	 (let ((sym (gensym)))
	   `(let ((,sym ,exp-base))
	     (internal-expt$ ,sym ,pos-exp))))
	((= pos-exp 2)
	 `(* ,exp-base ,exp-base))
	((= pos-exp 3) `(* ,exp-base ,exp-base ,exp-base))
	((= pos-exp 4)
	 `(internal-expt$ (internal-expt$ ,exp-base 2) 2))
	((= pos-exp 5)
	 `(* (internal-expt$ ,exp-base 4) ,exp-base))
	((= pos-exp 6)
	 `(internal-expt$ (internal-expt$ ,exp-base 3) 2))
	((= pos-exp 7)
	 `(* ,exp-base (internal-expt$ ,exp-base 6)))
	(t
	 `(* ,@(make-list pos-exp :initial-element exp-base)))))

;;; There is a real neat and fancy way to do the above for arbitrary N
;;; repeated squaring in a recrusive fashion.  It is trivial to do
;;; and should be done at some point.

(defopt mfunction-call (f &rest l &aux l1)
  (setq l1 l)
  (cond ((or (fboundp f)
	     (get f 'once-translated)
	     (get f 'translated))
	 (cons f l1))
	(t `(lispm-mfunction-call-aux ',f ', l1 (list ,@ l1) nil))))


;;; macros for compiled environments.

;;; (FUNGEN&ENV-for-meval <eval vars list> <late eval vars list> .  <EXP>)
;;; will define a function globally with a unique name
;;; (defun <name> <list of variables> <exp>). And return
;;; `((<name>) ,@<eval>> . <late eval>). The resulting expression may
;;; then be passed to a function which will bind variables from
;;; the <late eval vars list> and possibly other variables free in
;;; <exp> and then call MEVAL on the expression.
;;; FUNGEN&ENV-FOR-MEVALSUMARG will also make sure that the <name>
;;; has an mevalsumarg property of T.
;;; the expression was translated using TR-LAMBDA.

(defvar *infile-name-key* '||
  "This is a key gotten from the infile name, in the interpreter
  other completely hackish things with FSUBRS will go on.")

(defvar forms-to-compile-queue ())

(defun compile-forms-to-compile-queue-now ()
  (cond (forms-to-compile-queue
	 (loop for v in forms-to-compile-queue
	       do (eval v) (compile (second v)))))
  (setq forms-to-compile-queue nil))

(defmacro compile-forms-to-compile-queue ()
  (if forms-to-compile-queue
      (nconc (list 'progn ''compile)
	     (prog1
		 forms-to-compile-queue
	       (setq forms-to-compile-queue nil))
	     (list '(compile-forms-to-compile-queue)))))

(defun emit-defun (exp)
  (if $tr_semicompile (setq exp `(progn ,exp)))
  (let nil
    (setq forms-to-compile-queue (nconc forms-to-compile-queue (list (copy-tree exp))))))

(defmacro pop-declare-statement (l)
  `(and (not (atom (car ,l)))
    (eq (caar ,l) 'declare)
    (pop ,l)))


;;; Lambda expressions emitted by the translator.

;; lambda([u,...],...) where any free unquoted variable in the body is
;; either unbound or globally bound or locally bound in some
;; non-enclosing block.  At this point, BODY has already the correct
;; special declarations for elements of ARGL.
(defmacro m-tlambda (argl &body body)
  `(function
    (lambda ,argl
     ,@body)))

;; lambda([u,...,[v]],...) with the same condition as above.
(defmacro m-tlambda& (argl &rest body)
  `(function (lambda (,@(reverse (cdr (reverse argl)))
		      &rest ,@(last argl))
     ,(pop-declare-statement body)
     (setq ,(car (last argl))
	   (cons '(mlist) ,(car (last argl))))
     ,@ body)))

;; lambda([u,...],...) with free unquoted variables in the body which
;; have a local binding in some enclosing block, but no global one,
;; i.e, the complement of the condition for m-tlambda above.
(defmacro m-tlambda&env ((reg-argl env-argl) &body body)
  (declare (ignore env-argl))
  `(function
    (lambda ,reg-argl
     ;;(,@(or (pop-declare-statement body) '(declare)) (special ,@env-argl))
     ,@body)))

;; lambda([u,...,[v]],...) with the same condition as above.
(defmacro m-tlambda&env& ((reg-argl env-argl) &body body)
  (declare (ignore env-argl))
  (let ((last-arg (car (last reg-argl))))
    `(function
      (lambda (,@(butlast reg-argl) &rest ,last-arg)
       ;;(,@(or (pop-declare-statement body) '(declare)) (special ,@env-argl))
       ,(pop-declare-statement body)
       (setq ,last-arg (cons '(mlist) ,last-arg))
       ,@body))))


(defun for-eval-then-quote (var)
  `(list 'quote ,var))

(defun for-eval-then-quote-argl (argl)
  (mapcar 'for-eval-then-quote argl))

;; Problem: You can pass a lambda expression around in macsyma
;; because macsyma "general-rep" has a CAR which is a list.
;; Solution: Just as well anyway.


;;the lexical scoping  handles the environment in most cases
;;and it is messy to queue things

;;; this is the important case for numerical hackery.

(defun declare-snarf (body)
  (cond ((and (not (atom (car body)))
	      (eq (caar body) 'declare))
	 (list (car body)))
	(t nil)))


;;; I will use the special variable given by the NAME as a pointer to
;;; an environment.

(defopt m-tlambda-i (mode env argl &rest body &aux (name (gentemp "maxima"))
			  (declarep (declare-snarf body)))
  (cond ((eq mode '$float)
	 (emit-defun `(declare (flonum (,name ,@(make-list (length argl))))))
	 (emit-defun `(defprop ,name t flonum-compiled))))
  (emit-defun
   `(defun ,name ,argl
     ,@declarep
     (let ((,env ,name))
       ,@(cond (declarep (cdr body))
	       (t body)))))
  (emit-defun `(defparameter ,name (make-list ,(length env))))
  `(progn
    (set-vals-into-list ,env ,name)
    (quote ,name)))

;;; This is not optimal code.
;;; I.E. IT SUCKS ROCKS.

(defmacro set-vals-into-list (argl var)
  (do ((j 0 (1+ j))
       (argl argl (cdr argl))
       (l nil `((setf (nth ,j ,var) ,(car argl)) ,@l)))
      ((null argl) `(progn ,@l))))
