;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Compilation environment for TRANSLATED MACSYMA code.        ;;;
;;;       (c) Copyright 1980 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module mdefun macro)

(load-macsyma-macros transm)

;;; DEFMTRFUN will be the new standard.
;;; It will punt macsyma fexprs since the macro scheme is now
;;; available. I have tried to generalize this enough to do
;;; macsyma macros also.

;;; (DEFMTRFUN-EXTERNAL ($FOO <mode> <property> <&restp>))

;;we don't make function type declarations yet.
(defmacro defmtrfun-external (&rest ig)
  (declare (ignore ig))
  nil)

;;; (DEFMTRFUN ($FOO <mode> <property> <&restp>) <ARGL> . BODY)
;;; If the MODE is numeric it should do something about the
;;; number declarations for compiling. Also, the information about the
;;; modes of the arguments should not be thrown away.

(defmacro defmtrfun  ((name mode prop restp . array-flag) argl . body )
  (let ((def-header)
        (rest (gensym "TR-REST-ARG")))
    (and array-flag
	 ;; old DEFMTRFUN's might have this extra bit NIL
	 ;; new ones will have (NIL) or (T)
	 (setq array-flag (car array-flag)))
    (setq def-header
	  (cond ((eq prop 'mdefine)
		 (cond (array-flag `(,name a-subr))
		       (t name)))
		(t `(,name translated-mmacro))))

    `(eval-when
      (:compile-toplevel :load-toplevel :execute)

      ,@(and (not array-flag) `((remprop ',name 'translate)))
      ,@(and mode `((defprop ,name ,mode
		      ,(cond (array-flag 'arrayfun-mode)
			     (t 'function-mode)))))
      ,@(cond (array-flag
	       ;; when loading in hashed array properties
	       ;; most exist or be created. Other
	       ;; array properties must be consistent if
	       ;; they exist.
	       `((insure-array-props ',name ',mode ',(length argl)))))
      ,@(cond ((and (eq prop 'mdefine) (not array-flag))
	       `((cond ((status feature macsyma)
			(mputprop ',name t
				  ,(cond ((not restp)
					  ''$fixed_num_args_function)
					 (t
					  ''$variable_num_args_function)))))
		 ,(cond ((not restp) nil)))))
      (,(cond ((consp def-header) 'defun-prop)
              (restp 'defun)
              (t 'defmfun))
       ,def-header ,(cond ((not restp) argl)
                          (t `(&rest ,rest)))
       ,@(if (not restp)
             body
             (let ((required-arg-count (1- (length argl))))
               (if (zerop required-arg-count)
                   `((let ((,(car argl) (cons '(mlist) ,rest)))
                       ,@body))
                   `((when (< (length ,rest) ,required-arg-count)
                       (merror (intl:gettext "~M: expected at least ~M arguments but got ~M: ~M")
                               ',name
                               ,required-arg-count
                               (length ,rest)
                               (cons '(mlist) ,rest)))
                     (apply (lambda ,argl ,@body)
                            (nconc (subseq ,rest 0 ,required-arg-count)
                                   (list (cons '(mlist) (nthcdr ,required-arg-count ,rest)))))))))))))
