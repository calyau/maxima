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

(DEFUN *REARRAY (ARRAY-SYMBOL &OPTIONAL IGN &REST DIMS) ign
  (CHECK-ARG ARRAY-SYMBOL
	     (OR (SYMBOLP ARRAY-SYMBOL) (ARRAYP ARRAY-SYMBOL))
	     "a symbol or an array")
  ;;All references to *rearray now are to symbols with the
  ;; value cell being used for the array.
  (macrolet ((symbol-array (x) `(symbol-value ,x)))
  (COND ((NULL DIMS))
	((NULL (CDR DIMS))
	 (LET ((OLD-ARRAY (IF (SYMBOLP ARRAY-SYMBOL)
			      (SYMBOL-ARRAY ARRAY-SYMBOL) ARRAY-SYMBOL))
	       (NEW-ARRAY (make-array (car dims)))
	       (MIN-ARRAY-LENGTH))
	   (SETQ MIN-ARRAY-LENGTH (MIN (ARRAY-DIMENSION-N 1 OLD-ARRAY)
				       (ARRAY-DIMENSION-N 1 NEW-ARRAY)))
	   (DO ((I 0 (f1+ I))) ((= I MIN-ARRAY-LENGTH))
	       (ASET (AREF OLD-ARRAY I) NEW-ARRAY I))
	   (IF (SYMBOLP ARRAY-SYMBOL) (setf (symbol-array  ARRAY-SYMBOL)  NEW-ARRAY))
	   NEW-ARRAY))
	(T (ERROR  "Can't handle *REARRAY with more than one dimension")))))

(DEFUN RUNTIME NIL (#-cl TIME
		    #+cl get-internal-run-time))


