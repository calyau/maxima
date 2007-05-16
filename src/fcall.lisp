;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module fcall)

(transl-module fcall)

;;; Bug-Fixes:
;;;
;;; 11/15/80	KMP	Remove *TRIED-TO-AUTOLOAD* as a global and replaced
;;;			MFUNCTION-CALL with a trampoline function that calls
;;;			MFUNCTION-CALL-AUX with this info since MFUNCTION-CALL
;;;			was being screwed by the non-local nature of this var
;;;			when calls to itself got nested.
;;;

;;; This file is for macros, fsubrs, and subrs which are run time
;;; support for interpreted translated maxima code.

;;; MFUNCTION-CALL is a macro in LIBMAX;TRANSQ
;;; This is an FSUBR for use in interpreted code.
;;; It should do quit a bit of checking for STATUS PUNT NIL lossage, etc.
;;; The macro will expand into code which will assume normal
;;; functional argument evaluation.

(defmvar $tr_warn_bad_function_calls t
  "Warn when strange kinds of function calls are going on in translated code.")

(defvar *tr-runtime-warned* nil
  "This is an alist of warnings which have been given")

(defmfun $tr_warnings_get ()
  `((mlist) ,@(mapcar #'(lambda (u) `((mlist) ,(car u) ,(cdr u)))  *tr-runtime-warned*)))

(defun mfunction-call-warn (f type)
  (cond ((assoc f *tr-runtime-warned* :test #'eq))
	(t
	 (push (cons f type) *tr-runtime-warned*)
	 (when $tr_warn_bad_function_calls
	   (let ((tabl (cdr (assoc type '((fexpr . (fexpr-warnedp "This may be due to lack of enough translation data *print-base* info."))
					 (macro . (macro-warnedp "Macros should to be loaded when you are translating."))
					 (undefined . (undefined-warnp "The function was totaly undefined. Maybe you want to quote it."))
					 (punt-nil . (punt-nil-warnp "If you want the value of the function name, use `apply'"))
					 (mfexpr . (mfexpr-warnedp "MFEXPRS should be loaded at translating time. Use of them in translated code (nay, any code!), is NOT recommened however.")))
				   :test #'eq))))
	     (cond ((null tabl))
		   ((get f (car tabl)))
		   (t
		    (putprop f t (car tabl))
		    (terpri)
		    (princ "Warning: ")
		    (mgrind f nil)
		    (princ " has a function or macro call which has not been translated properly.")
		    (cond ((cdr tabl)
			   (terpri)
			   (princ (cadr tabl)))))))))))

(defun mapcar-eval (x)
  (mapcar #'eval x))

(defmacro mfunction-call (f &rest argl)
  (if (fboundp f)
      `(,f ,@ argl)
      ;;loses if the argl could not be evaluated but macsyma &quote functions
      ;;but the translator should be fixed so that if (mget f 'mfexprp) is t
      ;;then it doesn't translate as an mfunction-call.
      `(lispm-mfunction-call-aux ', f ',argl (list ,@ argl) nil)))

(defun lispm-mfunction-call-aux (f argl list-argl autoloaded-already? &aux f-prop)
  (cond ((functionp f)
	 (apply f list-argl))
	((macro-function f)
	 (eval (cons f list-argl)))
	((not (symbolp f)) (error "expected symbol or function"))
	((setq f-prop (get f 'mfexpr*))
	 (funcall f-prop (cons nil argl)))
	((setq f-prop (mget f 'mexpr))
	 (cond ((mget f 'mfexprp)
		(mfunction-call-warn f 'mfexpr)
		(meval (cons (list f) argl)))
	       (t
		(mlambda f-prop list-argl f nil nil))))
	((setq f-prop (get f 'autoload))
	 (cond (autoloaded-already?
		(merror "~:@M, Function undefined after loading file:~A " f (namestring (get f 'autoload))))
	       (t
		(funcall autoload (cons f f-prop))
		(lispm-mfunction-call-aux f argl list-argl t))))

	((boundp f)
	 (mfunction-call-warn f 'punt-nil)
	 (mapply (eval f) (mapcar-eval argl) f))
	(t
	 (mfunction-call-warn f 'undefined)
	 `((,f) ,@ list-argl))))

(defquote trd-msymeval (&rest l)
  (let ((a-var? (car l)))
    (if (boundp a-var?)
	(eval a-var?) ;;; ouch!
	(setf (symbol-value a-var?) (if (cdr l) (eval (cadr l))  a-var?))))) ;; double ouch!

(defun expt$ (a b)
  (expt a b))

;;; These are the LAMBDA forms. They have macro properties that set
;;; up very different things in compiled code.

(defvar *fcall-memory* nil
  "This ALIST will never be very long. Considerably less hairy then
	a hashing scheme, perhaps faster in normal use. In either case
	there is the problem of garbage from red-defined functions.")

(defun evalquote (exp)
  (setq exp (eval exp))
  (if (numberp exp)
      exp
      `(quote ,exp)))

;;; (FUNGEN&ENV-for-meval <eval vars list> <late eval vars list>  <EXP>)
;;won't work in cl.  fix later.
(defquote fungen&env-for-meval (&rest args)
  (destructuring-let (((evl levl . body) args))
	    ;;; all we want to do here is make sure that the EVL gets
	    ;;; evaluated now so that we have some kind of compatibility
	    ;;; with compiled code. we could just punt and pass the body.
		     `(($apply) ((mquote) ((lambda) ((mlist) ,@evl) ,@body))
		       ((mquote simp) ((mlist) ,@(mapcar-eval evl))))))

;;; The following code depends on the fact that the argument to an
;;; FEXPR is always EQ, for a given instance of FEXPR call. Lets say
;;; that the efficiency of the code depends on that fact. We cannot use
;;; displacing macros because of the $SAVE problem which I really don't
;;; feel like fooling around with since it is an IN-CORE function, and
;;; totaly cryptic code.

(defquote fungen&env-for-mevalsumarg (&rest args)
  (let ((res (assoc args *fcall-memory* :test #'eq)))
    (cond ((null res)
	   (destructuring-let (((evl levl t-body m-body) args))
			      (setq res (gensym))
			      (putprop res
				       (coerce
					`(lambda (*ignored*)
					   (prog2
					       (mbind ',evl (get ',res 'sumarg-env) nil)
					       (mevalatoms ',m-body)
					     (munbind ',evl)))
					'function)
				       'mevalsumarg-macro)
			      (setf (symbol-function res)
				    (coerce
				     `(lambda ()
					(apply #'(lambda ,evl ,t-body) (get ',res 'sumarg-env)))
				     'function))
			      (setq res `(,args ,res ((,res))))
			      (push res *fcall-memory*))))
    (putprop (cadr res) (mapcar #'eval (car args)) 'sumarg-env)
    (caddr res)))

(defquote m-tlambda-i (&rest args)
  `(lambda ,@(cddr args)))
