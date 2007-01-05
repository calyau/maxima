;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:42:11 
;;; Using Lisp CMU Common Lisp CVS Head 2006-12-02 00:15:46 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "BLAS")


(defun xerbla (srname info)
  (declare (type (f2cl-lib:integer4) info)
           (type (simple-array character (*)) srname))
  (f2cl-lib:with-multi-array-data
      ((srname character srname-%data% srname-%offset%))
    (prog ()
      (declare)
      (f2cl-lib:fformat t
                        (" ** On entry to " 1 (("~6A")) " parameter number " 1
                         (("~2D")) " had " "an illegal value" "~%")
                        srname
                        info)
     end_label
      (return (values nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xerbla
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-array character (6))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil)
           :calls 'nil)))

