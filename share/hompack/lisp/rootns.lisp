;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2020-04 (21D Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "HOMPACK")


(let* ((limit 20))
  (declare (type (f2cl-lib:integer4 20 20) limit) (ignorable limit))
  (defun rootns
         (n nfe iflag relerr abserr y yp yold ypold a qr lenqr pivot work par
          ipar)
    (declare (type (array f2cl-lib:integer4 (*)) ipar)
             (type (array double-float (*)) par)
             (type (array f2cl-lib:integer4 (*)) pivot)
             (type (array double-float (*)) work qr a ypold yold yp y)
             (type (double-float) abserr relerr)
             (type (f2cl-lib:integer4) lenqr iflag nfe n))
    (f2cl-lib:with-multi-array-data
        ((y double-float y-%data% y-%offset%)
         (yp double-float yp-%data% yp-%offset%)
         (yold double-float yold-%data% yold-%offset%)
         (ypold double-float ypold-%data% ypold-%offset%)
         (a double-float a-%data% a-%offset%)
         (qr double-float qr-%data% qr-%offset%)
         (work double-float work-%data% work-%offset%)
         (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
         (par double-float par-%data% par-%offset%)
         (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
      (labels ((dd01 (f0 f1 dels)
                 (f2cl-lib:f2cl/ (+ f1 (- f0)) dels))
               (dd001 (f0 fp0 f1 dels)
                 (/ (- (dd01 f0 f1 dels) fp0) dels))
               (dd011 (f0 f1 fp1 dels)
                 (/ (- fp1 (dd01 f0 f1 dels)) dels))
               (dd0011 (f0 fp0 f1 fp1 dels)
                 (/ (- (dd011 f0 f1 fp1 dels) (dd001 f0 fp0 f1 dels)) dels))
               (qofs (f0 fp0 f1 fp1 dels s)
                 (+
                  (*
                   (+
                    (*
                     (+ (* (dd0011 f0 fp0 f1 fp1 dels) (- s dels))
                        (dd001 f0 fp0 f1 dels))
                     s)
                    fp0)
                   s)
                  f0)))
        (declare (ftype (function (double-float double-float double-float)
                         (values double-float &rest t))
                        dd01))
        (declare (ftype (function
                         (double-float double-float double-float double-float)
                         (values double-float &rest t))
                        dd001))
        (declare (ftype (function
                         (double-float double-float double-float double-float)
                         (values double-float &rest t))
                        dd011))
        (declare (ftype (function
                         (double-float double-float double-float double-float
                          double-float)
                         (values double-float &rest t))
                        dd0011))
        (declare (ftype (function
                         (double-float double-float double-float double-float
                          double-float double-float)
                         (values double-float &rest t))
                        qofs))
        (prog ((ipp 0) (irho 0) (itangw 0) (itz 0) (iw 0) (iwp 0) (iz0 0)
               (iz1 0) (judy 0) (jw 0) (lcode 0) (np1 0) (aerr 0.0) (dels 0.0)
               (f0 0.0) (f1 0.0) (fp0 0.0) (fp1 0.0) (qsout 0.0) (rerr 0.0)
               (s 0.0) (sa 0.0) (sb 0.0) (sout 0.0) (u 0.0))
          (declare (type (f2cl-lib:integer4) ipp irho itangw itz iw iwp iz0 iz1
                                             judy jw lcode np1)
                   (type (double-float) aerr dels f0 f1 fp0 fp1 qsout rerr s sa
                                        sb sout u))
          (setf u (f2cl-lib:d1mach 4))
          (setf rerr (max relerr u))
          (setf aerr (max abserr 0.0))
          (setf np1 (f2cl-lib:int-add n 1))
          (setf ipp 1)
          (setf irho (f2cl-lib:int-add n 1))
          (setf iw (f2cl-lib:int-add irho n))
          (setf iwp (f2cl-lib:int-add iw np1))
          (setf itz (f2cl-lib:int-add iwp np1))
          (setf iz0 (f2cl-lib:int-add itz np1))
          (setf iz1 (f2cl-lib:int-add iz0 np1))
          (setf itangw (f2cl-lib:int-add iz1 np1))
         label100
          (f2cl-lib:fdo (judy 1 (f2cl-lib:int-add judy 1))
                        ((> judy limit) nil)
            (tagbody
              (f2cl-lib:fdo (jw 1 (f2cl-lib:int-add jw 1))
                            ((> jw np1) nil)
                (tagbody
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-sub
                                         (f2cl-lib:int-add itz jw)
                                         1))
                                       ((1
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-mul 13
                                                            (f2cl-lib:int-add n
                                                                              1))
                                          (f2cl-lib:int-mul 2 n)
                                          lenqr)))
                                       work-%offset%)
                          (-
                           (f2cl-lib:fref y-%data%
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          y-%offset%)
                           (f2cl-lib:fref yold-%data%
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          yold-%offset%)))
                 label110))
              (setf dels
                      (dnrm2 np1
                       (f2cl-lib:array-slice work-%data%
                                             double-float
                                             (itz)
                                             ((1
                                               (f2cl-lib:int-add
                                                (f2cl-lib:int-mul 13
                                                                  (f2cl-lib:int-add
                                                                   n
                                                                   1))
                                                (f2cl-lib:int-mul 2 n)
                                                lenqr)))
                                             work-%offset%)
                       1))
              (setf sa (coerce 0.0f0 'double-float))
              (setf sb dels)
              (setf lcode 1)
             label130
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (root sout qsout sa sb rerr aerr lcode)
                (declare (ignore var-1 var-4 var-5))
                (setf sout var-0)
                (setf sa var-2)
                (setf sb var-3)
                (setf lcode var-6))
              (if (> lcode 0) (go label140))
              (setf qsout
                      (-
                       (qofs
                        (f2cl-lib:fref yold-%data%
                                       (np1)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       yold-%offset%)
                        (f2cl-lib:fref ypold-%data%
                                       (np1)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       ypold-%offset%)
                        (f2cl-lib:fref y-%data%
                                       (np1)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       y-%offset%)
                        (f2cl-lib:fref yp-%data%
                                       (np1)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       yp-%offset%)
                        dels sout)
                       1.0f0))
              (go label130)
             label140
              (cond
                ((> lcode 2)
                 (setf iflag 6)
                 (go end_label)))
              (f2cl-lib:fdo (jw 1 (f2cl-lib:int-add jw 1))
                            ((> jw np1) nil)
                (tagbody
                  (setf (f2cl-lib:fref work-%data%
                                       ((f2cl-lib:int-sub
                                         (f2cl-lib:int-add iw jw)
                                         1))
                                       ((1
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-mul 13
                                                            (f2cl-lib:int-add n
                                                                              1))
                                          (f2cl-lib:int-mul 2 n)
                                          lenqr)))
                                       work-%offset%)
                          (qofs
                           (f2cl-lib:fref yold-%data%
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          yold-%offset%)
                           (f2cl-lib:fref ypold-%data%
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          ypold-%offset%)
                           (f2cl-lib:fref y-%data%
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          y-%offset%)
                           (f2cl-lib:fref yp-%data%
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          yp-%offset%)
                           dels sa))
                 label150))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (tangns sa
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (iw)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (iwp)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (itz)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   ypold a qr lenqr pivot
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (ipp)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (irho)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (itangw)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   nfe n iflag par ipar)
                (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                 var-8 var-9 var-10 var-11 var-13 var-15
                                 var-16))
                (setf sa var-0)
                (setf nfe var-12)
                (setf iflag var-14))
              (if (> iflag 0) (go end_label))
              (daxpy np1 1.0
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (itz)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 13
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        (f2cl-lib:int-mul 2 n)
                                        lenqr)))
                                     work-%offset%)
               1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (iw)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 13
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        (f2cl-lib:int-mul 2 n)
                                        lenqr)))
                                     work-%offset%)
               1)
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16)
                  (tangns sa
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (iw)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (iwp)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (itz)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   ypold a qr lenqr pivot
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (ipp)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (irho)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (itangw)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 13
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            (f2cl-lib:int-mul 2 n)
                                            lenqr)))
                                         work-%offset%)
                   nfe n iflag par ipar)
                (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                 var-8 var-9 var-10 var-11 var-13 var-15
                                 var-16))
                (setf sa var-0)
                (setf nfe var-12)
                (setf iflag var-14))
              (if (> iflag 0) (go end_label))
              (daxpy np1 1.0
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (itz)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 13
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        (f2cl-lib:int-mul 2 n)
                                        lenqr)))
                                     work-%offset%)
               1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (iw)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 13
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        (f2cl-lib:int-mul 2 n)
                                        lenqr)))
                                     work-%offset%)
               1)
              (cond
                ((and
                  (<=
                   (abs
                    (+
                     (f2cl-lib:fref work
                                    ((f2cl-lib:int-add iw n))
                                    ((1
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul 13
                                                         (f2cl-lib:int-add n
                                                                           1))
                                       (f2cl-lib:int-mul 2 n)
                                       lenqr))))
                     (- 1.0f0)))
                   (+ rerr aerr))
                  (<=
                   (dnrm2 n
                    (f2cl-lib:array-slice work
                                          double-float
                                          (itz)
                                          ((1
                                            (f2cl-lib:int-add
                                             (f2cl-lib:int-mul 13
                                                               (f2cl-lib:int-add
                                                                n
                                                                1))
                                             (f2cl-lib:int-mul 2 n)
                                             lenqr))))
                    1)
                   (+
                    (* rerr
                       (dnrm2 n
                        (f2cl-lib:array-slice work
                                              double-float
                                              (iw)
                                              ((1
                                                (f2cl-lib:int-add
                                                 (f2cl-lib:int-mul 13
                                                                   (f2cl-lib:int-add
                                                                    n
                                                                    1))
                                                 (f2cl-lib:int-mul 2 n)
                                                 lenqr))))
                        1))
                    aerr)))
                 (dcopy np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (iw)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 13
                                                             (f2cl-lib:int-add
                                                              n
                                                              1))
                                           (f2cl-lib:int-mul 2 n)
                                           lenqr)))
                                        work-%offset%)
                  1 y 1)
                 (go end_label)))
              (cond
                ((>
                  (*
                   (+ (f2cl-lib:fref yold (np1) ((1 (f2cl-lib:int-add n 1))))
                      (- 1.0f0))
                   (+
                    (f2cl-lib:fref work
                                   ((f2cl-lib:int-add iw n))
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 13
                                                        (f2cl-lib:int-add n 1))
                                      (f2cl-lib:int-mul 2 n)
                                      lenqr))))
                    (- 1.0f0)))
                  0.0f0)
                 (dcopy np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (iw)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 13
                                                             (f2cl-lib:int-add
                                                              n
                                                              1))
                                           (f2cl-lib:int-mul 2 n)
                                           lenqr)))
                                        work-%offset%)
                  1 yold 1)
                 (dcopy np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (iwp)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 13
                                                             (f2cl-lib:int-add
                                                              n
                                                              1))
                                           (f2cl-lib:int-mul 2 n)
                                           lenqr)))
                                        work-%offset%)
                  1 ypold 1))
                (t
                 (dcopy np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (iw)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 13
                                                             (f2cl-lib:int-add
                                                              n
                                                              1))
                                           (f2cl-lib:int-mul 2 n)
                                           lenqr)))
                                        work-%offset%)
                  1 y 1)
                 (dcopy np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (iwp)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 13
                                                             (f2cl-lib:int-add
                                                              n
                                                              1))
                                           (f2cl-lib:int-mul 2 n)
                                           lenqr)))
                                        work-%offset%)
                  1 yp 1)))
             label300))
          (setf iflag 6)
          (go end_label)
         end_label
          (return
           (values nil
                   nfe
                   iflag
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::rootns
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil fortran-to-lisp::nfe fortran-to-lisp::iflag nil
                            nil nil nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dcopy fortran-to-lisp::daxpy
                    fortran-to-lisp::dnrm2 fortran-to-lisp::tangns
                    fortran-to-lisp::root fortran-to-lisp::d1mach))))

