;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auxiliary DISPLA package for doing 1-D display
;;;
;;; (c) 1979 Massachusetts Institute of Technology
;;;
;;; See KMP for details

(in-package :maxima)

(declare-top (special linear-display-break-table fortranp))

;;; (LINEAR-DISPLA <thing-to-display>)
;;;
;;; Display text linearly. This function should be usable in any case
;;;  DISPLA is usable and will attempt to do something reasonable with
;;;  its input.

(defun linear-displa (x)
  (declare (special chrps))
  (fresh-line *standard-output*)
  (cond ((not (atom x))
	 (cond ((eq (caar x) 'mlable)
		(setq chrps 0)
		(cond ((cadr x)
		       (princ "(")
		       (setq chrps
			     (+  3 (length (mgrind (cadr x) nil))))
		       (princ ") ")))
		(mprint (msize (caddr x) nil nil 'mparen 'mparen)
			*standard-output*))
	       ((eq (caar x) 'mtext)
		(do ((x (cdr x) (cdr x))
		     (fortranp))	; Atoms in MTEXT
		    ((null x))		;  should omit ?'s
		  (setq fortranp (atom (car x)))
		  (mgrind (car x) *standard-output*)))
	       (t
		(mgrind x *standard-output*))))
	(t
	 (mgrind x *standard-output*)))
  (terpri))
