;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module numer)

(load-macsyma-macros numerm)

;;; Interface of lisp numerical routines to macsyma.
;;; 4:34pm  Thursday, 28 May 1981 - George Carrette.

;;; Trampolines for calling with numerical efficiency.

(defvar tramp$-alist ())

(defmacro deftramp$ (nargs)
  (let ((tramp$ (symbolconc 'tramp nargs '$))
	(tramp$-f (symbolconc 'tramp nargs '$-f))
	(tramp$-m (symbolconc 'tramp nargs '$-m))
	(l (make-list nargs)))
    (let ((arg-list (mapcar #'(lambda (ign)ign (gensym)) l)))
      `(progn
	 (push '(,nargs ,tramp$ ,tramp$-f ,tramp$-m) tramp$-alist)
	 (defmvar ,tramp$ "Contains the object to jump to if needed")
	 (defun ,tramp$-f ,arg-list
	   (float (funcall ,tramp$ ,@arg-list)))
	 (defun ,tramp$-m ,arg-list
	   (float (mapply1 ,tramp$ (list ,@arg-list) ',tramp$ nil)))))))

(deftramp$ 1)
(deftramp$ 2)
(deftramp$ 3)

(defmfun make-tramp$ (f n)
  (let ((l (assoc n tramp$-alist :test #'equal)))
    (if (null l)
	(merror "Bug: No trampoline of argument length ~M" n))
    (pop l)
    (let (tramp$ tramp$-m tramp$-f)
      (declare (special tramp$ tramp$-m tramp$-f))
      (setq tramp$ (pop l)
	    tramp$-f (pop l)
	    tramp$-m (pop l))
      (let ((whatnot (funtypep f)))
	(case (car whatnot)
	  ((operators)
	   (setf (symbol-value tramp$) f)
	   (getsubr! tramp$-m))
   	  ((mexpr)
	   (setf (symbol-value tramp$) (cadr whatnot))
	   (getsubr! tramp$-m))
	  ((expr lsubr)
	   (setf (symbol-value tramp$) (cadr whatnot))
	   (getsubr! tramp$-f))
	  (t
	   (merror "Undefined or inscrutable function~%~M" f)))))))


(defun getsubr! (x)
  (or
   (and (symbolp x) (fboundp x) (symbol-function x))
   (maxima-error "No subr property for ~a!" x)))

(defun funtypep (f)
  (cond ((symbolp f)
	 (let ((mprops (mgetl f '(mexpr)))
	       (lprops (and (fboundp f)
			    (list 'expr (symbol-function f)))))
	   (or (if $transrun
		   (or lprops mprops)
		   (or mprops lprops))
	       (getl f '(operators)))))
	((functionp f)
	 (list 'expr f))
	((consp f)
	 (list (if (member (car f) '(function lambda named-lambda) :test #'eq)
		   'expr
		   'mexpr)
	       f))
	(t
	 nil)))
