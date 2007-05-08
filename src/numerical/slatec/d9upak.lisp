;;; Compiled by f2cl version 2.0 beta Date: 2007/05/04 17:29:50 
;;; Using Lisp CMU Common Lisp Snapshot 2007-05 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun d9upak (x y n)
  (declare (type (integer) n) (type (double-float) y x))
  (prog ((absx 0.0))
    (declare (type (double-float) absx))
    (setf absx (f2cl-lib:dabs x))
    (setf n 0)
    (setf y 0.0)
    (if (= x 0.0) (go end_label))
   label10
    (if (>= absx 0.5) (go label20))
    (setf n (f2cl-lib:int-sub n 1))
    (setf absx (* absx 2.0))
    (go label10)
   label20
    (if (< absx 1.0) (go label30))
    (setf n (f2cl-lib:int-add n 1))
    (setf absx (* absx 0.5))
    (go label20)
   label30
    (setf y (coerce (f2cl-lib:dsign absx x) 'double-float))
    (go end_label)
   end_label
    (return (values nil y n))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::d9upak
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (integer))
           :return-values '(nil fortran-to-lisp::y fortran-to-lisp::n)
           :calls 'nil)))

