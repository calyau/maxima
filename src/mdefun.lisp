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

;(TRANSL-MODULE MDEFUN) IS CORRECT. But doesn't work in the MPRELU
;; environment.

(load-macsyma-macros transm)

;;; $FIX_NUM_ARGS_FUNCTION $VARIABLE_NUM_ARGS_FUNCTION.

(DEFVAR *KNOWN-FUNCTIONS-INFO-STACK* NIL
  "When MDEFUN-TR expands it puts stuff here for MFUNCTION-CALL
  to use.")

(DEFVAR *UNKNOWN-FUNCTIONS-INFO-STACK* NIL
  "When MFUNCTION-CALL expands without info from
  *KNOWN-FUNCTIONS-INFO-STACK* it puts stuff here to be barfed
  at the end of compilation.")


(DEFmacro MDEFUN-TR (&rest FORM)
  (MAXIMA-ERROR "obsolete macro form, please retranslate source code"
	 form 'fail-act))

(DEFmacro MDEFUN (&rest FORM)
  (MAXIMA-ERROR "obsolete macro form, please retranslate source code"
	 form 'fail-act))

;;; DEFMTRFUN will be the new standard.
;;; It will punt macsyma fexprs since the macro scheme is now
;;; available. I have tried to generalize this enough to do
;;; macsyma macros also.

;;; (DEFMTRFUN-EXTERNAL ($FOO <mode> <property> <&restp>))


#+PDP10
(DEFUN COMPILER-STATE () COMPILER-STATE)
#+CL
(DEFUN COMPILER-STATE () (Y-OR-N-P "Is COMPILER-STATE true?"))
#-(OR CL PDP10) 
(DEFUN COMPILER-STATE () T)
#-cl ;is this used.??
(defmacro defmtrfun-external ((name mode prop &rest restp))
    #+pdp10
    (and (eq prop 'mdefine) (COMPILER-STATE)
	 (PUSH-INFO NAME (COND (RESTP 'LEXPR)
			       (T 'EXPR))
		    *KNOWN-FUNCTIONS-INFO-STACK*))
    #-(or cl NIL)
    `(declare (,(cond (restp '*lexpr) (t '*expr))
	       ,name)
	      ;; FLONUM declaration is most important
	      ;; for numerical work on the pdp-10.
	      ,@(IF (AND (EQ PROP 'MDEFINE) (EQ MODE '$FLOAT))
		    `((FLONUM (,NAME))))
    ))

#+cl  ;;we don't make function type declarations yet.
(defmacro defmtrfun-external (&rest ig) ig nil)

;;; (DEFMTRFUN ($FOO <mode> <property> <&restp>) <ARGL> . BODY)
;;; If the MODE is numeric it should do something about the
;;; numebr declarations for compiling. Also, the information about the
;;; modes of the arguments should not be thrown away.

;;; For the LISPM this sucks, since &REST is built-in.
#+cl
(DEfmacro DEFMTRFUN  ((NAME MODE PROP RESTP . ARRAY-FLAG) ARGL . BODY )
  (let ((	DEF-HEADER))
     (AND ARRAY-FLAG
	 ;; old DEFMTRFUN's might have this extra bit NIL
	 ;; new ones will have (NIL) or (T)
	 (SETQ ARRAY-FLAG (CAR ARRAY-FLAG)))
    
    (SETQ DEF-HEADER
	  (COND ((EQ PROP 'MDEFINE)
		 (COND (ARRAY-FLAG #-CL `(,NAME A-EXPR #+MACLISP A-SUBR)
				   #+CL `(:PROPERTY ,NAME A-SUBR))
		       (T NAME)))
		(T `(,NAME TRANSLATED-MMACRO))))
    #+PDP10
    (AND (EQ PROP 'MDEFINE) (COMPILER-STATE) (NOT ARRAY-FLAG)
	 (PUSH-INFO NAME (COND (RESTP 'LEXPR)
			       (T 'EXPR))
		    *KNOWN-FUNCTIONS-INFO-STACK*))
    
    `(EVAL-WHEN (COMPILE EVAL LOAD)
	    ,@(AND (NOT ARRAY-FLAG) `((REMPROP ',NAME 'TRANSLATE)))
	    ,@(AND MODE `((DEFPROP ,NAME ,MODE
			    ,(COND (ARRAY-FLAG 'ARRAYFUN-MODE)
				   (T 'FUNCTION-MODE)))))
	    ,@(COND (ARRAY-FLAG
		     ;; when loading in hashed array properties
		     ;; most exist or be created. Other
		     ;; array properties must be consistent if
		     ;; they exist.
		     `((INSURE-ARRAY-PROPS ',NAME ',MODE
					   ',(LENGTH ARGL)))))
	    ,@(COND ((AND (EQ PROP 'MDEFINE) (NOT ARRAY-FLAG))
		     `((COND ((STATUS FEATURE MACSYMA)
			      (mputprop ',name t
					,(COND
					   ((NOT RESTP)
					    ''$fixed_num_args_function)
					   (T
					    ''$variable_num_args_function)))))
		       ,(COND ((NOT RESTP)
			       `(ARGS ',NAME '(NIL . ,(LENGTH ARGL))))))))
	    (,(if (consp def-header) 'DEFUN-prop 'defmfun)
	     ,DEF-HEADER ,(COND ((NOT RESTP) ARGL)
				      (T '|mlexpr NARGS|))
	      ,@(COND ((NOT RESTP)
		       BODY)
		      (t
		       (LET ((NL (f1- (LENGTH ARGL))))
			 `((COND ((< |mlexpr NARGS| ,NL)
				  ($ERROR
				    'MAXIMA-ERROR ',NAME
				    '| takes no less than |
				    ,NL
				    ',(COND ((= NL 1)
					     '| argument.|)
					    (T
					     '| arguments.|))))
				 (T
				  ((LAMBDA ,ARGL
				     ,@BODY)
				   ;; this conses up the
				   ;; calls to ARGS and LISTIFY.
				   ,@(DO ((J 1 (f1+ J))
					  (P-ARGL NIL))
					 ((> J NL)
					  (PUSH
					    `(CONS
					       '(MLIST)
					       (LISTIFY
						 (f- ,NL
						    |mlexpr NARGS|)))
					    P-ARGL)
					  (NREVERSE P-ARGL))
				       (PUSH `(ARG ,J)
					     P-ARGL)))))))))))))




#-cl
(DEFUN-prop (DEFMTRFUN MACRO) (FORM)
  (LET (( ((NAME MODE PROP RESTP . ARRAY-FLAG) ARGL . BODY) (CDR FORM))
	(DEF-HEADER))
    
    (AND ARRAY-FLAG
	 ;; old DEFMTRFUN's might have this extra bit NIL
	 ;; new ones will have (NIL) or (T)
	 (SETQ ARRAY-FLAG (CAR ARRAY-FLAG)))

    (SETQ DEF-HEADER
	  (COND ((EQ PROP 'MDEFINE)
		 (COND (ARRAY-FLAG #-CL `(,NAME A-EXPR #+MACLISP A-SUBR)
				   #+CL `(:PROPERTY ,NAME A-SUBR))
		       (T NAME)))
		(T `(,NAME TRANSLATED-MMACRO))))
    #+PDP10
    (AND (EQ PROP 'MDEFINE) (COMPILER-STATE) (NOT ARRAY-FLAG)
	 (PUSH-INFO NAME (COND (RESTP 'LEXPR)
			       (T 'EXPR))
		    *KNOWN-FUNCTIONS-INFO-STACK*))
    
    `(EVAL-WHEN (COMPILE EVAL LOAD)
	    ,@(AND (NOT ARRAY-FLAG) `((REMPROP ',NAME 'TRANSLATE)))
	    ,@(AND MODE `((DEFPROP ,NAME ,MODE
			    ,(COND (ARRAY-FLAG 'ARRAYFUN-MODE)
				   (T 'FUNCTION-MODE)))))
	    ,@(COND (ARRAY-FLAG
		     ;; when loading in hashed array properties
		     ;; most exist or be created. Other
		     ;; array properties must be consistent if
		     ;; they exist.
		     `((INSURE-ARRAY-PROPS ',NAME ',MODE
					   ',(LENGTH ARGL)))))
	    ,@(COND ((AND (EQ PROP 'MDEFINE) (NOT ARRAY-FLAG))
		     `((COND ((STATUS FEATURE MACSYMA)
			      (mputprop ',name t
					,(COND
					  ((NOT RESTP)
					   ''$fixed_num_args_function)
					  (T
					   ''$variable_num_args_function)))))
		       ,(COND ((NOT RESTP)
			       `(ARGS ',NAME '(NIL . ,(LENGTH ARGL))))))))
	    (,(if (consp def-header) 'DEFUN-prop 'defun)
	     ,DEF-HEADER ,(COND ((NOT RESTP) ARGL)
				      (T '|mlexpr NARGS|))
	      ,@(COND ((NOT RESTP)
		       BODY)
		      (t
		       (LET ((NL (f1- (LENGTH ARGL))))
			 `((COND ((< |mlexpr NARGS| ,NL)
				  ($ERROR
				   'MAXIMA-ERROR ',NAME
				   '| takes no less than |
				   ,NL
				   ',(COND ((= NL 1)
					    '| argument.|)
					   (T
					    '| arguments.|))))
				 (T
				  ((LAMBDA ,ARGL
				     ,@BODY)
				   ;; this conses up the
				   ;; calls to ARGS and LISTIFY.
				   ,@(DO ((J 1 (f1+ J))
					  (P-ARGL NIL))
					 ((> J NL)
					  (PUSH
					   `(CONS
					     '(MLIST)
					     (LISTIFY
					      (f- ,NL
						 |mlexpr NARGS|)))
					   P-ARGL)
					  (NREVERSE P-ARGL))
				       (PUSH `(ARG ,J)
					     P-ARGL)))))))))))))




