;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.215 2009/04/07 22:05:21 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.200 2009/01/19 02:38:17 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp 19f (19F)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format double-float))

(in-package :colnew)


(defstruct (colord
             (:predicate is-colord-p))
  (part-0 (make-array 5 :element-type '(integer))
          :type (simple-array (integer) (5)))
  (part-1 (make-array 20 :element-type 'f2cl-lib:integer4)
          :type (simple-array f2cl-lib:integer4 (20))))


(defparameter *colord-common-block*
  (let* ()
    (declare (ignorable))
    (make-colord)))


(defstruct (colbas
             (:predicate is-colbas-p))
  (part-0 (make-array 336 :element-type 'double-float)
          :type (simple-array double-float (336))))


(defparameter *colbas-common-block*
  (let* ()
    (declare (ignorable))
    (make-colbas)))


(defstruct (colest
             (:predicate is-colest-p))
  (part-0 (make-array 200 :element-type 'double-float)
          :type (simple-array double-float (200)))
  (part-1 (make-array 80 :element-type 'f2cl-lib:integer4)
          :type (simple-array f2cl-lib:integer4 (80)))
  (part-2 (make-array 1 :element-type '(integer))
          :type (simple-array (integer) (1))))


(defparameter *colest-common-block*
  (let* ()
    (declare (ignorable))
    (make-colest)))


(defstruct (colout
             (:predicate is-colout-p))
  (part-0 (make-array 1 :element-type 'double-float)
          :type (simple-array double-float (1)))
  (part-1 (make-array 2 :element-type '(integer))
          :type (simple-array (integer) (2))))


(defparameter *colout-common-block*
  (let* ()
    (declare (ignorable))
    (make-colout)))


(defstruct (colloc
             (:predicate is-colloc-p))
  (part-0 (make-array 56 :element-type 'double-float)
          :type (simple-array double-float (56))))


(defparameter *colloc-common-block*
  (let* ()
    (declare (ignorable))
    (make-colloc)))


(defstruct (colapr
             (:predicate is-colapr-p))
  (part-0 (make-array 5 :element-type '(integer))
          :type (simple-array (integer) (5))))


(defparameter *colapr-common-block*
  (let* ()
    (declare (ignorable))
    (make-colapr)))


(defstruct (colmsh
             (:predicate is-colmsh-p))
  (part-0 (make-array 4 :element-type '(integer))
          :type (simple-array (integer) (4))))


(defparameter *colmsh-common-block*
  (let* ()
    (declare (ignorable))
    (make-colmsh)))


(defstruct (colsid
             (:predicate is-colsid-p))
  (part-0 (make-array 42 :element-type 'double-float)
          :type (simple-array double-float (42)))
  (part-1 (make-array 2 :element-type '(integer))
          :type (simple-array (integer) (2))))


(defparameter *colsid-common-block*
  (let* ()
    (declare (ignorable))
    (make-colsid)))


(defstruct (colnln
             (:predicate is-colnln-p))
  (part-0 (make-array 5 :element-type '(integer))
          :type (simple-array (integer) (5))))


(defparameter *colnln-common-block*
  (let* ()
    (declare (ignorable))
    (make-colnln)))


(defun /blockdata-colnew/ ()
  (let ()
    (symbol-macrolet ()
      )))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::/blockdata-colnew/
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types 'nil
                                            :return-values 'nil
                                            :calls 'nil)))

