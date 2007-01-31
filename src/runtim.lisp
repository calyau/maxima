;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module runtim)

;; This file contains functions which are also defined as macros in the
;; standard Macsyma environment.  They are defined here for the benefit
;; interpreted code in the fix file.  This file is used only in the ITS
;; implementation, as the Macsyma macros are present at runtime in large
;; address space systems.

;; The above comment is idiotic. These functions are open-codeable,
;; and defined as macros only for efficiency. However, the correct
;; way to hack efficiency is through compiler:optimizers, which is
;; what we use now.

;; Subr definitions of ADD* and MUL* needed at runtime for functions generated
;; by TRANSL.  If a function is defined as both a macro and a function, the
;; compiler expands the macro, but still puts the function definitions in the
;; fasl.

;; ADD and MUL to be flushed shortly.  Around for compatibility only.
;; (another CWH comment????) -gjc

;; on the LISPM the &REST list is a stack-allocated cdr-coded list.
;; We have to copy it, so might as well try out some optimizations.

(defun add (&rest v)
  (do ((l nil)
       (r)
       (acc 0))
      ((null v)
       (if (null l)
	   acc
	   (if (zerop acc)
	       (simplifya (cons '(mplus) l) t)
	       (simplifya (list* '(mplus) acc l) t))))
    (setq r (pop v))
    (if (numberp r)
	(incf acc r)
	(push r l))))

(defun mul (&rest v)
  (do ((l nil)
       (r)
       (acc 1))
      ((null v)
       (if (null l)
	   acc
	   (if (equal acc 1)
	       (simplifya (cons '(mtimes) l) t)
	       (simplifya (list* '(mtimes) acc l) t))))
    (setq r (pop v))
    (if (numberp r)
	(setq acc (* r acc))
	(push r l))))

(defun add* (&rest l)
  (simplifya (cons '(mplus) (copy-list l)) nil))

(defun mul* (&rest l)
  (simplifya (cons '(mtimes) (copy-list l)) nil))
