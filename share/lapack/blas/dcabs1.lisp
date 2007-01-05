;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:42:11 
;;; Using Lisp CMU Common Lisp CVS Head 2006-12-02 00:15:46 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "BLAS")


(defun dcabs1 (z)
  (declare (type (f2cl-lib:complex16) z))
  (prog ((dcabs1 0.0))
    (declare (type (double-float) dcabs1))
    (setf dcabs1
            (+ (f2cl-lib:dabs (f2cl-lib:dble z))
               (f2cl-lib:dabs (f2cl-lib:dimag z))))
    (go end_label)
   end_label
    (return (values dcabs1 nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dcabs1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::complex16))
           :return-values '(nil)
           :calls 'nil)))

