;;; Compiled by f2cl version:
;;; ("$Id: dlassq.lisp,v 1.3 2009-01-08 18:25:24 rtoy Exp $"
;;;  "$Id: dlassq.lisp,v 1.3 2009-01-08 18:25:24 rtoy Exp $"
;;;  "$Id: dlassq.lisp,v 1.3 2009-01-08 18:25:24 rtoy Exp $"
;;;  "$Id: dlassq.lisp,v 1.3 2009-01-08 18:25:24 rtoy Exp $"
;;;  "$Id: dlassq.lisp,v 1.3 2009-01-08 18:25:24 rtoy Exp $"
;;;  "$Id: dlassq.lisp,v 1.3 2009-01-08 18:25:24 rtoy Exp $"
;;;  "$Id: dlassq.lisp,v 1.3 2009-01-08 18:25:24 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :lapack)


(let* ((zero 0.0))
  (declare (type (double-float 0.0 0.0) zero) (ignorable zero))
  (defun dlassq (n x incx scale sumsq)
    (declare (type (double-float) sumsq scale)
             (type (array double-float (*)) x)
             (type (f2cl-lib:integer4) incx n))
    (f2cl-lib:with-multi-array-data
        ((x double-float x-%data% x-%offset%))
      (prog ((absxi 0.0) (ix 0))
        (declare (type (double-float) absxi) (type (f2cl-lib:integer4) ix))
        (cond
          ((> n 0)
           (f2cl-lib:fdo (ix 1 (f2cl-lib:int-add ix incx))
                         ((> ix
                             (f2cl-lib:int-add 1
                                               (f2cl-lib:int-mul
                                                (f2cl-lib:int-add n
                                                                  (f2cl-lib:int-sub
                                                                   1))
                                                incx)))
                          nil)
             (tagbody
               (cond
                 ((/= (f2cl-lib:fref x (ix) ((1 *))) zero)
                  (setf absxi
                          (abs
                           (f2cl-lib:fref x-%data% (ix) ((1 *)) x-%offset%)))
                  (cond
                    ((< scale absxi)
                     (setf sumsq (+ 1 (* sumsq (expt (/ scale absxi) 2))))
                     (setf scale absxi))
                    (t
                     (setf sumsq (+ sumsq (expt (/ absxi scale) 2)))))))
              label10))))
        (go end_label)
       end_label
        (return (values nil nil nil scale sumsq))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlassq
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float))
           :return-values '(nil nil nil fortran-to-lisp::scale
                            fortran-to-lisp::sumsq)
           :calls 'nil)))

