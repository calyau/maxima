(defpackage #:odepack
  (:use :cl)
  (:export #:dlsode))

(in-package #:odepack)

(defstruct (dls001
             (:predicate is-dls001-p))
  (part-0 (make-array 218 :element-type 'double-float)
          :type (simple-array double-float (218)))
  (part-1 (make-array 37 :element-type '(f2cl-lib:integer4))
          :type (simple-array (f2cl-lib:integer4) (37))))


(defparameter *dls001-common-block*
  (let* ()
    (declare (ignorable))
    (make-dls001)))

