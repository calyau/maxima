;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;  Maclisp compatibility package

;;  This function should really bash the array or use an invisible pointer
;;  to be compatible with maclisp.  ARRAY-SYMBOL can be either an array object
;;  or a symbol.  This only works for one dimensional arrays right now.
;;  *REARRAY of one arg is supposed to return the array.
;;  Rewrite at some point to use ADJUST-ARRAY-SIZE.

(defun *rearray (array-symbol &optional ign &rest dims)
  (declare (ignore ign))
  (check-arg array-symbol (or (symbolp array-symbol) (arrayp array-symbol))
	     "a symbol or an array")
  ;; All references to *rearray now are to symbols with the
  ;; value cell being used for the array.
  (cond ((null dims))
	((null (cdr dims))
	 (let ((old-array (if (symbolp array-symbol)
			      (symbol-value array-symbol) array-symbol))
	       (new-array (make-array (car dims)))
	       (min-array-length))
	   (setq min-array-length (min (array-dimension old-array 0)
				       (array-dimension new-array 0)))
	   (do ((i 0 (1+ i)))
	       ((= i min-array-length))
	     (setf (aref new-array i) (aref old-array i)))
	   (when (symbolp array-symbol)
	     (setf (symbol-value array-symbol) new-array))
	   new-array))
	(t (maxima-error  "Can't handle *rearray with more than one dimension"))))
