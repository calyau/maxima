;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:42:11 
;;; Using Lisp CMU Common Lisp CVS Head 2006-12-02 00:15:46 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :blas)


(defun dasum (n dx incx)
  (declare (type (array double-float (*)) dx)
           (type (f2cl-lib:integer4) incx n))
  (f2cl-lib:with-multi-array-data
      ((dx double-float dx-%data% dx-%offset%))
    (prog ((i 0) (m 0) (mp1 0) (nincx 0) (dtemp 0.0) (dasum 0.0))
      (declare (type (double-float) dasum dtemp)
               (type (f2cl-lib:integer4) nincx mp1 m i))
      (setf dasum 0.0)
      (setf dtemp 0.0)
      (if (or (<= n 0) (<= incx 0)) (go end_label))
      (if (= incx 1) (go label20))
      (setf nincx (f2cl-lib:int-mul n incx))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                    ((> i nincx) nil)
        (tagbody
          (setf dtemp
                  (+ dtemp
                     (f2cl-lib:dabs
                      (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))))
         label10))
      (setf dasum dtemp)
      (go end_label)
     label20
      (setf m (mod n 6))
      (if (= m 0) (go label40))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf dtemp
                  (+ dtemp
                     (f2cl-lib:dabs
                      (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))))
         label30))
      (if (< n 6) (go label60))
     label40
      (setf mp1 (f2cl-lib:int-add m 1))
      (f2cl-lib:fdo (i mp1 (f2cl-lib:int-add i 6))
                    ((> i n) nil)
        (tagbody
          (setf dtemp
                  (+ dtemp
                     (f2cl-lib:dabs
                      (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
                     (f2cl-lib:dabs
                      (f2cl-lib:fref dx-%data%
                                     ((f2cl-lib:int-add i 1))
                                     ((1 *))
                                     dx-%offset%))
                     (f2cl-lib:dabs
                      (f2cl-lib:fref dx-%data%
                                     ((f2cl-lib:int-add i 2))
                                     ((1 *))
                                     dx-%offset%))
                     (f2cl-lib:dabs
                      (f2cl-lib:fref dx-%data%
                                     ((f2cl-lib:int-add i 3))
                                     ((1 *))
                                     dx-%offset%))
                     (f2cl-lib:dabs
                      (f2cl-lib:fref dx-%data%
                                     ((f2cl-lib:int-add i 4))
                                     ((1 *))
                                     dx-%offset%))
                     (f2cl-lib:dabs
                      (f2cl-lib:fref dx-%data%
                                     ((f2cl-lib:int-add i 5))
                                     ((1 *))
                                     dx-%offset%))))
         label50))
     label60
      (setf dasum dtemp)
      (go end_label)
     end_label
      (return (values dasum nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dasum fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil)
           :calls 'nil)))

