;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:42:11 
;;; Using Lisp CMU Common Lisp CVS Head 2006-12-02 00:15:46 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "BLAS")


(defun zscal (n za zx incx)
  (declare (type (array f2cl-lib:complex16 (*)) zx)
           (type (f2cl-lib:complex16) za)
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
                  (* za (f2cl-lib:fref zx-%data% (ix) ((1 *)) zx-%offset%)))
          (setf ix (f2cl-lib:int-add ix incx))
         label10))
      (go end_label)
     label20
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref zx-%data% (i) ((1 *)) zx-%offset%)
                  (* za (f2cl-lib:fref zx-%data% (i) ((1 *)) zx-%offset%)))
         label30))
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zscal fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (fortran-to-lisp::complex16)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil)
           :calls 'nil)))

