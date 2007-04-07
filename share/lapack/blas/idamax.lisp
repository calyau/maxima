;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:42:11 
;;; Using Lisp CMU Common Lisp CVS Head 2006-12-02 00:15:46 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :blas)


(defun idamax (n dx incx)
  (declare (type (array double-float (*)) dx)
           (type (f2cl-lib:integer4) incx n))
  (f2cl-lib:with-multi-array-data
      ((dx double-float dx-%data% dx-%offset%))
    (prog ((i 0) (ix 0) (dmax 0.0) (idamax 0))
      (declare (type (double-float) dmax)
               (type (f2cl-lib:integer4) idamax ix i))
      (setf idamax 0)
      (if (or (< n 1) (<= incx 0)) (go end_label))
      (setf idamax 1)
      (if (= n 1) (go end_label))
      (if (= incx 1) (go label20))
      (setf ix 1)
      (setf dmax
              (f2cl-lib:dabs
               (f2cl-lib:fref dx-%data% (1) ((1 *)) dx-%offset%)))
      (setf ix (f2cl-lib:int-add ix incx))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (if
           (<=
            (f2cl-lib:dabs (f2cl-lib:fref dx-%data% (ix) ((1 *)) dx-%offset%))
            dmax)
           (go label5))
          (setf idamax i)
          (setf dmax
                  (f2cl-lib:dabs
                   (f2cl-lib:fref dx-%data% (ix) ((1 *)) dx-%offset%)))
         label5
          (setf ix (f2cl-lib:int-add ix incx))
         label10))
      (go end_label)
     label20
      (setf dmax
              (f2cl-lib:dabs
               (f2cl-lib:fref dx-%data% (1) ((1 *)) dx-%offset%)))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (if
           (<=
            (f2cl-lib:dabs (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
            dmax)
           (go label30))
          (setf idamax i)
          (setf dmax
                  (f2cl-lib:dabs
                   (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
         label30))
      (go end_label)
     end_label
      (return (values idamax nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::idamax
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil)
           :calls 'nil)))

