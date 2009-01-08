;;; Compiled by f2cl version:
;;; ("$Id: zdscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zdscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zdscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zdscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zdscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zdscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zdscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :blas)


(defun zdscal (n da zx incx)
  (declare (type (array f2cl-lib:complex16 (*)) zx)
           (type (double-float) da)
           (type (f2cl-lib:integer4) incx n))
  (f2cl-lib:with-multi-array-data
      ((zx f2cl-lib:complex16 zx-%data% zx-%offset%))
    (prog ((i 0) (ix 0))
      (declare (type (f2cl-lib:integer4) ix i))
      (if (or (<= n 0) (<= incx 0)) (go end_label))
      (if (= incx 1) (go label20))
      (setf ix 1)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref zx-%data% (ix) ((1 *)) zx-%offset%)
                  (* (f2cl-lib:dcmplx da 0.0)
                     (f2cl-lib:fref zx-%data% (ix) ((1 *)) zx-%offset%)))
          (setf ix (f2cl-lib:int-add ix incx))
         label10))
      (go end_label)
     label20
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref zx-%data% (i) ((1 *)) zx-%offset%)
                  (* (f2cl-lib:dcmplx da 0.0)
                     (f2cl-lib:fref zx-%data% (i) ((1 *)) zx-%offset%)))
         label30))
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zdscal
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (double-float)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil)
           :calls 'nil)))

