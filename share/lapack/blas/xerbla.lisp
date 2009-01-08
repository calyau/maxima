;;; Compiled by f2cl version:
;;; ("$Id: xerbla.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: xerbla.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: xerbla.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: xerbla.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: xerbla.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: xerbla.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: xerbla.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :blas)


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

