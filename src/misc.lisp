;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;;  Maclisp compatibility package for the Lisp Machine -- run time

;;  This function should really bash the array or use an invisible pointer
;;  to be compatible with maclisp.  ARRAY-SYMBOL can be either an array object
;;  or a symbol.  This only works for one dimensional arrays right now.
;;  IGNORE is normally the type, but Maclisp only has ART-Q arrays.
;;  *REARRAY of one arg is supposed to return the array.
;;  Rewrite at some point to use ADJUST-ARRAY-SIZE.

(defun *rearray (array-symbol &optional ign &rest dims) ign
       (check-arg array-symbol
		  (or (symbolp array-symbol) (arrayp array-symbol))
		  "a symbol or an array")
       ;;All references to *rearray now are to symbols with the
       ;; value cell being used for the array.
       (macrolet ((symbol-array (x) `(symbol-value ,x)))
	 (cond ((null dims))
	       ((null (cdr dims))
		(let ((old-array (if (symbolp array-symbol)
				     (symbol-array array-symbol) array-symbol))
		      (new-array (make-array (car dims)))
		      (min-array-length))
		  (setq min-array-length (min (array-dimension-n 1 old-array)
					      (array-dimension-n 1 new-array)))
		  (do ((i 0 (f1+ i))) ((= i min-array-length))
		    (aset (aref old-array i) new-array i))
		  (if (symbolp array-symbol) (setf (symbol-array  array-symbol)  new-array))
		  new-array))
	       (t (error  "Can't handle *rearray with more than one dimension")))))

(defun runtime nil (#-cl time
			 #+cl get-internal-run-time))


