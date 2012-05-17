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

;;; (LINEAR-DISPLA <thing-to-display>)
;;;
;;; Display text linearly. This function should be usable in any case
;;;  DISPLA is usable and will attempt to do something reasonable with
;;;  its input.

(defun linear-displa (form)
  (fresh-line *standard-output*)
  (mgrind form *standard-output*)
  (terpri))
