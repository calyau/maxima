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

(in-package "MAXIMA")
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
;;; support for interpreted translated macsyma code.

(defun _eval (x)
  ;;  #+NIL (si:internal-eval x)
  (eval x))

;;; #+(OR NIL LMI) This poor man's closures stuff should go, and get replaced
;;; by the real thing!

;;; MFUNCTION-CALL is a macro in LIBMAX;TRANSQ
;;; This is an FSUBR for use in interpreted code.
;;; It should do quit a bit of checking for STATUS PUNT NIL lossage, etc.
;;; The macro will expand into code which will assume normal
;;; functional argument evaluation.

(defmvar $tr_warn_bad_function_calls t
  "Warn when strange kinds of function calls are going on in
	 translated code.")

(defvar *tr-runtime-warned* nil
  "This is an alist of warnings which have been given")

(defmfun $tr_warnings_get ()
  `((mlist) ,@(mapcar #'(lambda (u)
			  `((mlist) ,(car u) ,(cdr u)))
		      *tr-runtime-warned*)))


(defun mfunction-call-warn (f type)
  (cond ((assq f *tr-runtime-warned*))
	(t
	 (push (cons f type) *tr-runtime-warned*)
	 (cond ($tr_warn_bad_function_calls
		(let ((tabl (cdr (assq type '((fexpr . (fexpr-warnedp 
							"This may be due to lack of enough translation data *print-base* info."))
					      (macro . (macro-warnedp 
							"Macros should to be loaded when you are translating."))
					      (undefined . (undefined-warnp
							    "The function was totaly undefined. Maybe you want to quote it."))
					      (punt-nil . (punt-nil-warnp
							   "If you want the value of the function name, use APPLY"))
					      (mfexpr . (mfexpr-warnedp
							 "MFEXPRS should be loaded at translating time. Use of them in
translated code (nay, any code!), is NOT recommened however.")))))))
		  (cond ((null tabl))
			((get f (car tabl)))
			(t
			 (putprop f t (car tabl))
			 (terpri)
			 (princ "Warning: ")
			 (mgrind f nil)
			 (princ
			  " has a function or macro call which has not been
translated properly.")
			 (cond ((cdr tabl)
				(terpri)
				(princ (cadr tabl))))))))))))
	     
(defun mapcar-eval (x) (mapcar #'_eval x))

;;(defmacro max-funcall (f &rest args)
;;  (cond ((fboundp f) `(,f ,@ args))
;;	(t`(max-funcall-aux ',f ',args (list ,@ args)))))
;;
;;(defun max-funcall-aux (fn arglis-syms vals &aux mfun args)
;;  (cond ((functionp fn)(apply fn vals))
;;	((fboundp fn)(fsignal "can't funcall a macro or special form"))
;;	((setq mfun (mget fn 'mexpr))
;;	 (show mfun)
;;	 (progv (setq args(cdr (second mfun))) vals
;;	   (show args vals)
;;	     (show (and  (boundp (car args)) (symbol-value (car args))))
;;	       (mlambda mfun vals fn t)))
;;	(t (fsignal "unknown function call"))))
;;
;;(defun foo (xx)
;;  (mfunction-call $ff xx))

(defmacro mfunction-call  (f &rest argl)
  (cond ((fboundp f)
	 `(,f ,@ argl))
	(t ;;loses if the argl could not be evaluated but macsyma &quote functions
	 ;;but the translator should be fixed so that if (mget f 'mfexprp) is t
	 ;;then it doesn't translate as an mfunction-call.
	 `(lispm-mfunction-call-aux ', f ',argl (list ,@ argl) nil))))
;;#-cl
;;(DEFUN MFUNCTION-CALL FEXPR (F+ARGL)
;;       (MFUNCTION-CALL-AUX (CAR F+ARGL) (CDR F+ARGL) NIL))

(defun lispm-mfunction-call-aux (f argl list-argl autoloaded-already? &aux f-prop)
  (cond
    ((functionp f)
     (apply f list-argl))
    ((macro-function f)			;(SETQ F-PROP (GET F 'MACRO))
     (eval (cons f list-argl)))
    ((not (symbolp f)) (error "expected symbol or function"))
    ((setq f-prop (get f 'mfexpr*))
     ;;save a cons with (locf argl) not(cons nil argl)
     (funcall f-prop (cons nil argl)))
    ((setq f-prop (mget f 'mexpr))
     (cond ((mget f 'mfexprp)
	    (mfunction-call-warn f 'mfexpr)
	    (meval (cons (list f) argl)))
	   (t
	    (mlambda f-prop list-argl f nil nil))))
    ((setq f-prop (get f 'autoload))
     (cond (autoloaded-already?
	    (merror "~:@M, Function undefined after loading file:~A "
		    f
		    (namestring (get f 'autoload))))
	   
	   (t
	    (funcall autoload (cons f f-prop))
	    (lispm-mfunction-call-aux f argl list-argl t))))
    
    ((boundp f)
     (mfunction-call-warn f 'punt-nil)
     (mapply (_eval f) (mapcar-eval argl) f))
    (t
     (mfunction-call-warn f 'undefined)
     `((,f) ,@ list-argl))))

;;#-cl
;;(DEFUN MFUNCTION-CALL-AUX (F ARGL AUTOLOADED-ALREADY?)
;;       (LET ((F-PROP))
;;	    (COND #+NIL
;;		  ((FBOUNDP F)
;;		   (APPLY F (MAPCAR-EVAL ARGL)))
;;		  ((SETQ F-PROP (GETL F '(EXPR LEXPR)))
;;		   (APPLY  (CADR F-PROP) (MAPCAR-EVAL ARGL)))
;;		  ((GETL F '(SUBR LSUBR))
;;		   (APPLY F (MAPCAR-EVAL ARGL)))
;;		  ((GETL F '(FEXPR FSUBR))
;;		   (MFUNCTION-CALL-WARN F 'FEXPR)
;;		   (APPLY  F ARGL))
;;		  ((SETQ F-PROP (GET F 'MACRO))
;;		   (MFUNCTION-CALL-WARN F 'MACRO)
;;		   (_EVAL (FUNCALL F-PROP (CONS F ARGL))))
;;		  ((SETQ F-PROP (MGET F 'MEXPR))
;;		   (COND ((MGET F 'MFEXPRP)
;;			  (MFUNCTION-CALL-WARN F 'MFEXPR)
;;			  (MEVAL (CONS (LIST F) ARGL)))
;;			 (T
;;			  (MAPPLY1 F-PROP (MAPCAR-EVAL ARGL) '|a translated fcall.| nil))))
;;		  ((SETQ F-PROP (GET F 'AUTOLOAD))
;;		   (COND (AUTOLOADED-ALREADY?
;;			  (MERROR "~:@M, Function undefined after loading file:~A "
;;				  F
;;				  (NAMESTRING (GET F 'AUTOLOAD))))
				  
;;			 (T
;;			  (funcall autoload (cons f F-PROP))
;;			  (MFUNCTION-CALL-AUX F ARGL T))))
;;		  ((BOUNDP F)
;;		   (MFUNCTION-CALL-WARN F 'PUNT-NIL)
;;		   (MAPPLY1 (_EVAL F) (MAPCAR-EVAL ARGL) F nil))
;;		  (T
;;		   (MFUNCTION-CALL-WARN F 'UNDEFINED)
;;		   `((,F) ,@(MAPCAR-EVAL ARGL))))))

;;; I think that that just about covers it.

;;; This FEXPR may not work if it is not compiled.
;;#-cl
;;(defun TRD-MSYMEVAL fexpr ( L)
;;       (LET ((A-VAR? (CAR L)))
;;	    (COND ((BOUNDP A-VAR?)
;;		   (_EVAL A-VAR?))   ;;; ouch!
;;		  (t
;;		   ;; double ouch!
;;		   (set A-VAR? (cond ((cdr l) (_EVAL (cadr l)))
;;				     (t a-var?)))))))

(defquote trd-msymeval (&rest l)
  (let ((a-var? (car l)))
    (cond ((boundp a-var?)
	   (_eval a-var?))   ;;; ouch!
	  (t
	   ;; double ouch!
	   (set a-var? (cond ((cdr l) (_eval (cadr l)))
			     (t a-var?)))))))

(defun expt$ (a b)
  (expt a b))

;;; These are the LAMBDA forms. They have macro properties that set
;;; up very different things in compiled code.

;;; (M-TLAMBDA ,@(CDR T-FORM))))
;;; (M-TLAMBDA& ,@(CDR T-FORM))))))


(defun make-m-lambda& (argl body)
  (do ((l nil)
       (largs argl (cdr largs))
       (j 1 (f1+ j)))
      ((null (cdr largs))
       `(lambda *n*
	 ((lambda ,argl ,@body)
	  ,@(nreverse l)
	  (cons '(mlist) (listify (f- ,(f1- j) *n*))))))
    (push `(arg ,j) l)))

;;#-cl 
;;(DEFUN M-TLAMBDA FEXPR (ARGS)
;;       (CONS 'LAMBDA ARGS))

;;#+(and (not cl) lispm )
;;(defquote M-TLAMBDA (&rest ARGS)
;;       (CONS 'LAMBDA ARGS))

(defvar *fcall-memory* nil
  "This ALIST will never be very long. Considerably less hairy then
	a hashing scheme, perhaps faster in normal use. In either case
	there is the problem of garbage from red-defined functions.")
;;#-cl
;;(DEFUN M-TLAMBDA& FEXPR (ARGS)
;;       (LET ((FORM (ASSQ ARGS *FCALL-MEMORY*)))
;;	    (COND (FORM (CDR FORM))
;;		  (T
;;		   (SETQ FORM (MAKE-M-LAMBDA& (CAR ARGS) (CDR ARGS)))
;;		   (PUSH (CONS ARGS FORM) *FCALL-MEMORY*)
;;		   FORM))))

;;#+(and (not cl)lispm)
;;(defquote M-TLAMBDA& (&rest ARGS)
;;       (LET ((FORM (ASSQ ARGS *FCALL-MEMORY*)))
;;	    (COND (FORM (CDR FORM))
;;		  (T
;;		   (SETQ FORM (MAKE-M-LAMBDA& (CAR ARGS) (CDR ARGS)))
;;		   (PUSH (CONS ARGS FORM) *FCALL-MEMORY*)
;;		   FORM))))

(defun evalquote (exp)
  (setq exp (_eval exp))
  (cond ((numberp exp) exp)
	(t `(quote ,exp))))
;;#-cl
;;(DEFUN M-TLAMBDA&ENV FEXPR (ARGS)
;;       (LET (( ((REG-ARGL ENV-ARGL) . BODY)  ARGS))
;;	    `(LAMBDA ,REG-ARGL ((LAMBDA ,ENV-ARGL ,@BODY) ,@(MAPCAR 'EVALQUOTE
;;								    ENV-ARGL)))))

;;#+(and (not cl)lispm)
;;(defquote M-TLAMBDA&ENV (&rest ARGS)
;;       (LET (( ((REG-ARGL ENV-ARGL) . BODY)  ARGS))
;;	    `(LAMBDA ,REG-ARGL ((LAMBDA ,ENV-ARGL ,@BODY) ,@(MAPCAR 'EVALQUOTE
;;								    ENV-ARGL)))))
;;#-cl
;;(DEFUN M-TLAMBDA&ENV& FEXPR (ARGS)
;;       (LET (( ((REG-ARGL ENV-ARGL) . BODY)  ARGS))
;;	    (make-M-LAMBDA& REG-ARGL `(((LAMBDA ,ENV-ARGL ,@BODY)
;;					,@(MAPCAR 'EVALQUOTE ENV-ARGL))))))

;;#+(and (not cl) lispm ) ;;now defined by tranq.lisp  The optimizer method taken out.
;;(defquote M-TLAMBDA&ENV& (&rest ARGS)
;;       (LET (( ((REG-ARGL ENV-ARGL) . BODY)  ARGS))
;;	    (make-M-LAMBDA& REG-ARGL `(((LAMBDA ,ENV-ARGL ,@BODY)
;;					,@(MAPCAR 'EVALQUOTE ENV-ARGL))))))

;;; (FUNGEN&ENV-for-meval <eval vars list> <late eval vars list>  <EXP>)
;;#-cl
;;(DEFUN FUNGEN&ENV-FOR-MEVAL FEXPR (ARGS)
;;       (LET (((EVL LEVL . BODY) ARGS))
;;	    ;;; all we want to do here is make sure that the EVL gets
;;	    ;;; evaluated now so that we have some kind of compatibility
;;	    ;;; with compiled code. we could just punt and pass the body.
;;	    `(($APPLY) ((MQUOTE) ((LAMBDA) ((MLIST) ,@EVL) ,@BODY))
;;		       ((MQUOTE SIMP) ((MLIST) ,@(MAPCAR-EVAL EVL))))))

;;; (FUNGEN&ENV-for-meval <eval vars list> <late eval vars list>  <EXP>)
;;won't work in cl.  fix later.
(defquote fungen&env-for-meval (&rest args)
  (let (((evl levl . body) args))
	    ;;; all we want to do here is make sure that the EVL gets
	    ;;; evaluated now so that we have some kind of compatibility
	    ;;; with compiled code. we could just punt and pass the body.
    `(($apply) ((mquote) ((lambda) ((mlist) ,@evl) ,@body))
      ((mquote simp) ((mlist) ,@(mapcar-eval evl))))))

;;; (FUNGEN&ENV-FOR-MEVALSUMARG EVL VAR <translate-exp> <untranslated-exp>)

;;; The following code depends on the fact that the argument to an
;;; FEXPR is always EQ, for a given instance of FEXPR call. Lets say
;;; that the efficiency of the code depends on that fact. We cannot use
;;; displacing macros because of the $SAVE problem which I really don't
;;; feel like fooling around with since it is an IN-CORE function, and
;;; totaly cryptic code.
;;#-cl
;;(DEFUN FUNGEN&ENV-FOR-MEVALSUMARG FEXPR (ARGS)
;;       (LET ((RES (ASSQ ARGS *FCALL-MEMORY*)))
;;	    (COND ((NULL RES)
;;		   (LET (((EVL LEVL T-BODY M-BODY) ARGS))
;;			(SETQ RES (GENSYM))
;;			(PUTPROP RES
;;				 (coerce
;;				 `(LAMBDA (*IGNORED*)
;;					  (PROG2 (MBIND ',EVL
;;							(GET ',RES 'SUMARG-ENV) NIL)
;;						 (MEVALATOMS ',M-BODY)
;;						 (MUNBIND ',EVL)))
;;				 'function)
;;				 'MEVALSUMARG-MACRO)
;;			;; Obsolete and replaced by the following form --wj
;;;; 			(PUTPROP RES
;;;; 				 `(LAMBDA ()
;;;; 					  (APPLY #'(LAMBDA ,EVL ,T-BODY)
;;;; 						 (GET ',RES 'SUMARG-ENV)))
;;;; 				 'EXPR)
;;			(setf (symbol-function res)
;;			      (coerce
;;			       `(lambda ()
;;				  (apply #'(lambda ,evl ,t-body)
;;					 (get ',res 'sumarg-env)))
;;			       'function))
;;			(SETQ RES `(,ARGS ,RES ((,RES))))
;;			(PUSH RES *FCALL-MEMORY*))))
;;	    (PUTPROP (CADR RES) (MAPCAR #'EVAL (CAR ARGS)) 'SUMARG-ENV)
;;	    (CADDR RES)))

;;; (FUNGEN&ENV-FOR-MEVALSUMARG EVL VAR <translate-exp> <untranslated-exp>)

;;; The following code depends on the fact that the argument to an
;;; FEXPR is always EQ, for a given instance of FEXPR call. Lets say
;;; that the efficiency of the code depends on that fact. We cannot use
;;; displacing macros because of the $SAVE problem which I really don't
;;; feel like fooling around with since it is an IN-CORE function, and
;;; totaly cryptic code.

(defquote fungen&env-for-mevalsumarg (&rest args)
  (let ((res (assq args *fcall-memory*)))
    (cond ((null res)
	   (let (((evl levl t-body m-body) args))
	     (setq res (gensym))
	     (putprop res
		      (coerce
		       `(lambda (*ignored*)
			 (prog2 (mbind ',evl
				       (get ',res 'sumarg-env) nil)
			     (mevalatoms ',m-body)
			   (munbind ',evl)))
		       'function)
		      'mevalsumarg-macro)
	     ;; Obsolete and replaced by the following form --wj
;;; 			(PUTPROP RES
;;; 				 `(LAMBDA ()
;;; 					  (APPLY #'(LAMBDA ,EVL ,T-BODY)
;;; 						 (GET ',RES 'SUMARG-ENV)))
;;; 				 'EXPR)
	     (setf (symbol-function res)
		   (coerce
		    `(lambda ()
		      (apply #'(lambda ,evl ,t-body)
		       (get ',res 'sumarg-env)))
		    'function))
	     (setq res `(,args ,res ((,res))))
	     (push res *fcall-memory*))))
    (putprop (cadr res) (mapcar #'eval (car args)) 'sumarg-env)
    (caddr res)))

;;#-cl
;;(DEFUN M-TLAMBDA-I FEXPR (ARGS)
;;       `(LAMBDA ,@(CDDR ARGS)))

(defquote m-tlambda-i (&rest args)
  `(lambda ,@(cddr args)))

;;#+MACLISP
;;(DEFUN COMPILE-FORMS-TO-COMPILE-QUEUE FEXPR (FORM) FORM)

