;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2013-11 (20E Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(let ((zero 0.0d0) (one 1.0d0) (cutlo 8.232d-11) (cuthi 1.304d19))
  (declare (type (double-float) zero one cutlo cuthi))
  (defun dnrm2 (n dx incx)
    (declare (type (array double-float (*)) dx)
             (type (f2cl-lib:integer4) incx n))
    (f2cl-lib:with-multi-array-data
        ((dx double-float dx-%data% dx-%offset%))
      (prog ((hitest 0.0d0) (sum 0.0d0) (xmax 0.0d0) (next 0) (dnrm2 0.0d0)
             (j 0) (i 0) (nn 0))
        (declare (type (f2cl-lib:integer4) nn i j next)
                 (type (double-float) dnrm2 xmax sum hitest))
        (if (> n 0) (go label10))
        (setf dnrm2 zero)
        (go label300)
       label10
        (setf next 30)
        (setf sum zero)
        (setf nn (f2cl-lib:int-mul n incx))
        (setf i 1)
       label20
        (f2cl-lib:assigned-goto next (30 50 70 110))
       label30
        (if (> (abs (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)) cutlo)
            (go label85))
        (setf next 50)
        (setf xmax zero)
       label50
        (if (= (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%) zero)
            (go label200))
        (if (> (abs (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)) cutlo)
            (go label85))
        (setf next 70)
        (go label105)
       label100
        (setf i j)
        (setf next 110)
        (setf sum
                (/ (/ sum (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
                   (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
       label105
        (setf xmax (abs (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
        (go label115)
       label70
        (if (> (abs (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)) cutlo)
            (go label75))
       label110
        (if (<= (abs (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)) xmax)
            (go label115))
        (setf sum
                (+ one
                   (* sum
                      (expt
                       (/ xmax
                          (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%))
                       2))))
        (setf xmax (abs (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
        (go label200)
       label115
        (setf sum
                (+ sum
                   (expt
                    (/ (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%) xmax)
                    2)))
        (go label200)
       label75
        (setf sum (* sum xmax xmax))
       label85
        (setf hitest (/ cuthi n))
        (f2cl-lib:fdo (j i (f2cl-lib:int-add j incx))
                      ((> j nn) nil)
          (tagbody
            (if
             (>= (abs (f2cl-lib:fref dx-%data% (j) ((1 *)) dx-%offset%))
                 hitest)
             (go label100))
           label95
            (setf sum
                    (+ sum
                       (expt (f2cl-lib:fref dx-%data% (j) ((1 *)) dx-%offset%)
                             2)))))
        (setf dnrm2 (f2cl-lib:fsqrt sum))
        (go label300)
       label200
        (setf i (f2cl-lib:int-add i incx))
        (if (<= i nn) (go label20))
        (setf dnrm2 (* xmax (f2cl-lib:fsqrt sum)))
       label300
        (go end_label)
       end_label
        (return (values dnrm2 nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dnrm2 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil)
           :calls 'nil)))

