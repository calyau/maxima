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

(in-package "MAXIMA")
(macsyma-module mdefun macro)

;;(TRANSL-MODULE MDEFUN) IS CORRECT. But doesn't work in the MPRELU
;; environment.

(load-macsyma-macros transm)

;;; $FIX_NUM_ARGS_FUNCTION $VARIABLE_NUM_ARGS_FUNCTION.

(defvar *known-functions-info-stack* nil
  "When MDEFUN-TR expands it puts stuff here for MFUNCTION-CALL
  to use.")

(defvar *unknown-functions-info-stack* nil
  "When MFUNCTION-CALL expands without info from
  *KNOWN-FUNCTIONS-INFO-STACK* it puts stuff here to be barfed
  at the end of compilation.")


(defmacro mdefun-tr (&rest form)
  (maxima-error "obsolete macro form, please retranslate source code"
		form 'fail-act))

(defmacro mdefun (&rest form)
  (maxima-error "obsolete macro form, please retranslate source code"
		form 'fail-act))

;;; DEFMTRFUN will be the new standard.
;;; It will punt macsyma fexprs since the macro scheme is now
;;; available. I have tried to generalize this enough to do
;;; macsyma macros also.

;;; (DEFMTRFUN-EXTERNAL ($FOO <mode> <property> <&restp>))


#+pdp10
(defun compiler-state () compiler-state)
#+cl
(defun compiler-state () (y-or-n-p "Is `compiler-state' true?"))
#-(or cl pdp10) 
(defun compiler-state () t)
#-cl					;is this used.??
(defmacro defmtrfun-external ((name mode prop &rest restp))
  #+pdp10
  (and (eq prop 'mdefine) (compiler-state)
       (push-info name (cond (restp 'lexpr)
			     (t 'expr))
		  *known-functions-info-stack*))
  #-(or cl nil)
  `(declare (,(cond (restp '*lexpr) (t '*expr))
	     ,name)
    ;; FLONUM declaration is most important
    ;; for numerical work on the pdp-10.
    ,@(if (and (eq prop 'mdefine) (eq mode '$float))
	  `((flonum (,name))))
    ))

#+cl ;;we don't make function type declarations yet.
(defmacro defmtrfun-external (&rest ig) ig nil)

;;; (DEFMTRFUN ($FOO <mode> <property> <&restp>) <ARGL> . BODY)
;;; If the MODE is numeric it should do something about the
;;; numebr declarations for compiling. Also, the information about the
;;; modes of the arguments should not be thrown away.

;;; For the LISPM this sucks, since &REST is built-in.
#+cl
(defmacro defmtrfun  ((name mode prop restp . array-flag) argl . body )
  (let ((	def-header))
    (and array-flag
	 ;; old DEFMTRFUN's might have this extra bit NIL
	 ;; new ones will have (NIL) or (T)
	 (setq array-flag (car array-flag)))
    
    (setq def-header
	  (cond ((eq prop 'mdefine)
		 (cond (array-flag #-cl `(,name a-expr #+maclisp a-subr)
				   #+cl `(:property ,name a-subr))
		       (t name)))
		(t `(,name translated-mmacro))))
    #+pdp10
    (and (eq prop 'mdefine) (compiler-state) (not array-flag)
	 (push-info name (cond (restp 'lexpr)
			       (t 'expr))
		    *known-functions-info-stack*))
    
    `(eval-when (compile eval load)
      ,@(and (not array-flag) `((remprop ',name 'translate)))
      ,@(and mode `((defprop ,name ,mode
		      ,(cond (array-flag 'arrayfun-mode)
			     (t 'function-mode)))))
      ,@(cond (array-flag
	       ;; when loading in hashed array properties
	       ;; most exist or be created. Other
	       ;; array properties must be consistent if
	       ;; they exist.
	       `((insure-array-props ',name ',mode
		  ',(length argl)))))
      ,@(cond ((and (eq prop 'mdefine) (not array-flag))
	       `((cond ((status feature macsyma)
			(mputprop ',name t
				  ,(cond
				    ((not restp)
				     ''$fixed_num_args_function)
				    (t
				     ''$variable_num_args_function)))))
		 ,(cond ((not restp)
			 `(args ',name '(nil . ,(length argl))))))))
      (,(if (consp def-header) 'defun-prop 'defmfun)
       ,def-header ,(cond ((not restp) argl)
			  (t '|mlexpr NARGS|))
       ,@(cond ((not restp)
		body)
	       (t
		(let ((nl (f1- (length argl))))
		  `((cond ((< |mlexpr NARGS| ,nl)
			   ($error
			    'maxima-error ',name
			    '| takes no less than |
			    ,nl
			    ',(cond ((= nl 1)
				     '| argument.|)
				    (t
				     '| arguments.|))))
			  (t
			   ((lambda ,argl
			      ,@body)
			    ;; this conses up the
			    ;; calls to ARGS and LISTIFY.
			    ,@(do ((j 1 (f1+ j))
				   (p-argl nil))
				  ((> j nl)
				   (push
				    `(cons
				      '(mlist)
				      (listify
				       (f- ,nl
					|mlexpr NARGS|)))
				    p-argl)
				   (nreverse p-argl))
				  (push `(arg ,j)
					p-argl)))))))))))))




#-cl
(defun-prop (defmtrfun macro) (form)
  (let (( ((name mode prop restp . array-flag) argl . body) (cdr form))
	(def-header))
    
    (and array-flag
	 ;; old DEFMTRFUN's might have this extra bit NIL
	 ;; new ones will have (NIL) or (T)
	 (setq array-flag (car array-flag)))

    (setq def-header
	  (cond ((eq prop 'mdefine)
		 (cond (array-flag #-cl `(,name a-expr #+maclisp a-subr)
				   #+cl `(:property ,name a-subr))
		       (t name)))
		(t `(,name translated-mmacro))))
    #+pdp10
    (and (eq prop 'mdefine) (compiler-state) (not array-flag)
	 (push-info name (cond (restp 'lexpr)
			       (t 'expr))
		    *known-functions-info-stack*))
    
    `(eval-when (compile eval load)
      ,@(and (not array-flag) `((remprop ',name 'translate)))
      ,@(and mode `((defprop ,name ,mode
		      ,(cond (array-flag 'arrayfun-mode)
			     (t 'function-mode)))))
      ,@(cond (array-flag
	       ;; when loading in hashed array properties
	       ;; most exist or be created. Other
	       ;; array properties must be consistent if
	       ;; they exist.
	       `((insure-array-props ',name ',mode
		  ',(length argl)))))
      ,@(cond ((and (eq prop 'mdefine) (not array-flag))
	       `((cond ((status feature macsyma)
			(mputprop ',name t
				  ,(cond
				    ((not restp)
				     ''$fixed_num_args_function)
				    (t
				     ''$variable_num_args_function)))))
		 ,(cond ((not restp)
			 `(args ',name '(nil . ,(length argl))))))))
      (,(if (consp def-header) 'defun-prop 'defun)
       ,def-header ,(cond ((not restp) argl)
			  (t '|mlexpr NARGS|))
       ,@(cond ((not restp)
		body)
	       (t
		(let ((nl (f1- (length argl))))
		  `((cond ((< |mlexpr NARGS| ,nl)
			   ($error
			    'maxima-error ',name
			    '| takes no less than |
			    ,nl
			    ',(cond ((= nl 1)
				     '| argument.|)
				    (t
				     '| arguments.|))))
			  (t
			   ((lambda ,argl
			      ,@body)
			    ;; this conses up the
			    ;; calls to ARGS and LISTIFY.
			    ,@(do ((j 1 (f1+ j))
				   (p-argl nil))
				  ((> j nl)
				   (push
				    `(cons
				      '(mlist)
				      (listify
				       (f- ,nl
					|mlexpr NARGS|)))
				    p-argl)
				   (nreverse p-argl))
				  (push `(arg ,j)
					p-argl)))))))))))))




