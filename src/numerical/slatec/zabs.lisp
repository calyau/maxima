;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:18:39 
;;; Using Lisp CMU Common Lisp Snapshot 2006-12 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun zabs (zr zi)
  (declare (type (double-float) zi zr))
  (prog ((u 0.0) (v 0.0) (q 0.0) (s 0.0) (zabs 0.0))
    (declare (type (double-float) zabs s q v u))
    (setf u (abs zr))
    (setf v (abs zi))
    (setf s (+ u v))
    (setf s (* s 1.0))
    (if (= s 0.0) (go label20))
    (if (> u v) (go label10))
    (setf q (/ u v))
    (setf zabs (* v (f2cl-lib:fsqrt (+ 1.0 (* q q)))))
    (go end_label)
   label10
    (setf q (/ v u))
    (setf zabs (* u (f2cl-lib:fsqrt (+ 1.0 (* q q)))))
    (go end_label)
   label20
    (setf zabs 0.0)
    (go end_label)
   end_label
    (return (values zabs nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zabs fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float))
           :return-values '(nil nil)
           :calls 'nil)))

