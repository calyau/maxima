;;; -*- mode: lisp; package: cl-maxima; syntax: common-lisp; Base: 10 -*-

(in-package "MAXIMA")

(defmacro :maybe-reset (&rest copy) copy
  nil)

(defmacro with-polynomial-area-new (ign &body body) ign
   `(progn ,@ body))

(defmacro with-polynomial-area (ign &rest body) ign ` (progn ,@ body))

(defmacro new-copy-from-temporary-area (&rest l) (car l))

