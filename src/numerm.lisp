;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (c) Copyright 1976, 1983 Massachusetts Institute of Technology      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module numerm macro)

;;; Macros for interface of lisp numerical routines to maxima
;;; for use with the functions in Maxsrc;Numer.

(defmacro defbindtramp$ (nargs)
  (let ((bind-tramp$ (symbolconc 'bind-tramp nargs '$))
	(tramp$ (symbolconc 'tramp nargs '$)))
    `(progn
       (proclaim (quote (special ,tramp$)))
       (defmacro ,bind-tramp$ (f g &rest body)
	 `(let ((,',tramp$))
	    (let ((,f (make-tramp$ ,g ,',nargs)))
	      ,@body))))))

(defbindtramp$ 1)
(defbindtramp$ 2)
(defbindtramp$ 3)
