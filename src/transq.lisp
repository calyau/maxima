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

(load-macsyma-macros transm)

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

(define-compiler-macro mfunction-call (f &rest l &aux l1)
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
;;; the expression was translated using TR-LAMBDA.

(defvar *infile-name-key* '||
  "This is a key gotten from the infile name, in the interpreter
  other completely hackish things with FSUBRS will go on.")

(defun skip-declare-exprs (l)
  (do ((l l (cdr l)))
      ((not (and (consp (car l))
                 (eq (caar l) 'declare)))
       l)))

(defun vanilla-lambda (bvl body)
  `(lambda ,bvl
     (declare (special ,@bvl))
     ,@(skip-declare-exprs body)))

(defun rest-arg-lambda (bvl body)
  (let ((req-args (butlast bvl))
        (rest-arg (car (last bvl))))
    `(lambda (,@req-args &rest ,rest-arg)
       (declare (special ,@bvl))
       (push '(mlist) ,rest-arg)
       ,@(skip-declare-exprs body))))

(defun lambda-with-free-vars (bvl fvl lambda-header body)
  (let ((symevals (mapcar (lambda (x) `(maybe-msymeval ',x)) fvl)))
    (funcall lambda-header bvl
      `((let ,(mapcar #'list fvl symevals)
          (declare (special ,@fvl))
          ,@body)))))

(defun make-tlambda (bvl fvl rest-p body)
  (let ((lambda-header (if rest-p #'rest-arg-lambda #'vanilla-lambda)))
    (if (null fvl)
        (funcall lambda-header bvl body)
        (lambda-with-free-vars bvl fvl lambda-header body))))

;;; Lambda expressions emitted by the translator.

;; lambda([u,...],...) where any free unquoted variable in the body is
;; either unbound or globally bound or locally bound in some
;; non-enclosing block.
(defmacro m-tlambda (bvl &rest body)
  (make-tlambda bvl '() nil body))

;; lambda([u,...,[v]],...) with the same condition as above.
(defmacro m-tlambda& (bvl &rest body)
  (make-tlambda bvl '() t body))

;; lambda([u,...],...) with free unquoted variables in the body which
;; have a local binding in some enclosing block, but no global one,
;; i.e, the complement of the condition for m-tlambda above.
(defmacro m-tlambda&env ((bvl fvl) &rest body)
  (make-tlambda bvl fvl nil body))

;; lambda([u,...,[v]],...) with the same condition as above.
(defmacro m-tlambda&env& ((bvl fvl) &rest body)
  (make-tlambda bvl fvl t body))

;; Problem: You can pass a lambda expression around in macsyma
;; because macsyma "general-rep" has a CAR which is a list.
;; Solution: Just as well anyway.


;;the lexical scoping  handles the environment in most cases
;;and it is messy to queue things

;;; this is the important case for numerical hackery.


;;; This is not optimal code.
;;; I.E. IT SUCKS ROCKS.

(defmacro set-vals-into-list (argl var)
  (do ((j 0 (1+ j))
       (argl argl (cdr argl))
       (l nil `((setf (nth ,j ,var) ,(car argl)) ,@l)))
      ((null argl) `(progn ,@l))))
