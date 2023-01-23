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


(defun rootqf
       (n nfe iflag relerr abserr y yp yold ypold a qt r dz z w t$ f0 f1 par
        ipar)
  (declare (type (array f2cl-lib:integer4 (*)) ipar)
           (type (array double-float (*)) par)
           (type (array double-float (*)) f1 f0 t$ w z dz r qt a ypold yold yp
                                          y)
           (type (double-float) abserr relerr)
           (type (f2cl-lib:integer4) iflag nfe n))
  (f2cl-lib:with-multi-array-data
      ((y double-float y-%data% y-%offset%)
       (yp double-float yp-%data% yp-%offset%)
       (yold double-float yold-%data% yold-%offset%)
       (ypold double-float ypold-%data% ypold-%offset%)
       (a double-float a-%data% a-%offset%)
       (qt double-float qt-%data% qt-%offset%)
       (r double-float r-%data% r-%offset%)
       (dz double-float dz-%data% dz-%offset%)
       (z double-float z-%data% z-%offset%)
       (w double-float w-%data% w-%offset%)
       (t$ double-float t$-%data% t$-%offset%)
       (f0 double-float f0-%data% f0-%offset%)
       (f1 double-float f1-%data% f1-%offset%)
       (par double-float par-%data% par-%offset%)
       (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
    (labels ((dd01 (p0 p1 dels)
               (f2cl-lib:f2cl/ (+ p1 (- p0)) dels))
             (dd001 (p0 pp0 p1 dels)
               (/ (- (dd01 p0 p1 dels) pp0) dels))
             (dd011 (p0 p1 pp1 dels)
               (/ (- pp1 (dd01 p0 p1 dels)) dels))
             (dd0011 (p0 pp0 p1 pp1 dels)
               (/ (- (dd011 p0 p1 pp1 dels) (dd001 p0 pp0 p1 dels)) dels))
             (qofs (p0 pp0 p1 pp1 dels s)
               (+
                (*
                 (+
                  (*
                   (+ (* (dd0011 p0 pp0 p1 pp1 dels) (- s dels))
                      (dd001 p0 pp0 p1 dels))
                   s)
                  pp0)
                 s)
                p0)))
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
      (prog ((brack nil) (istep 0) (i 0) (lcode 0) (limit 0) (np1 0) (aerr 0.0)
             (dels 0.0) (eta 0.0) (one 0.0) (p0 0.0) (p1 0.0) (pp0 0.0)
             (pp1 0.0) (qsout 0.0) (rerr 0.0) (s 0.0) (sa 0.0) (sb 0.0)
             (sout 0.0) (u 0.0) (zero 0.0))
        (declare (type (double-float) zero u sout sb sa s rerr qsout pp1 pp0 p1
                                      p0 one eta dels aerr)
                 (type (f2cl-lib:integer4) np1 limit lcode i istep)
                 (type f2cl-lib:logical brack))
        (setf one (coerce 1.0f0 'double-float))
        (setf zero (coerce 0.0f0 'double-float))
        (setf u (f2cl-lib:d1mach 4))
        (setf rerr (max relerr u))
        (setf aerr (max abserr zero))
        (setf np1 (f2cl-lib:int-add n 1))
        (setf eta (* 100.0f0 u))
        (setf limit
                (f2cl-lib:int-mul 2
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int
                                    (-
                                     (f2cl-lib:log10
                                      (+ aerr (* rerr (dnrm2 np1 y 1))))))
                                   1)))
        (cond
          ((= iflag (f2cl-lib:int-sub 2))
           (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
               (rho a
                (f2cl-lib:fref y-%data%
                               (1)
                               ((1 (f2cl-lib:int-add n 1)))
                               y-%offset%)
                (f2cl-lib:array-slice y-%data%
                                      double-float
                                      (2)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      y-%offset%)
                f0 par ipar)
             (declare (ignore var-0 var-2 var-3 var-4 var-5))
             (setf (f2cl-lib:fref y-%data%
                                  (1)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  y-%offset%)
                     var-1)))
          ((= iflag (f2cl-lib:int-sub 1))
           (f
            (f2cl-lib:array-slice y-%data%
                                  double-float
                                  (2)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  y-%offset%)
            f0)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (setf (f2cl-lib:fref f0-%data%
                                    (i)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    f0-%offset%)
                       (+
                        (*
                         (f2cl-lib:fref y-%data%
                                        (1)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%)
                         (f2cl-lib:fref f0-%data%
                                        (i)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        f0-%offset%))
                        (*
                         (- 1.0f0
                            (f2cl-lib:fref y-%data%
                                           (1)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           y-%offset%))
                         (-
                          (f2cl-lib:fref y-%data%
                                         ((f2cl-lib:int-add i 1))
                                         ((1 (f2cl-lib:int-add n 1)))
                                         y-%offset%)
                          (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)))))
              label10)))
          (t
           (f
            (f2cl-lib:array-slice y-%data%
                                  double-float
                                  (2)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  y-%offset%)
            f0)
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i n) nil)
             (tagbody
               (setf (f2cl-lib:fref f0-%data%
                                    (i)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    f0-%offset%)
                       (-
                        (+
                         (*
                          (f2cl-lib:fref y-%data%
                                         (1)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         y-%offset%)
                          (- (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)
                             (f2cl-lib:fref f0-%data%
                                            (i)
                                            ((1 (f2cl-lib:int-add n 1)))
                                            f0-%offset%)))
                         (f2cl-lib:fref y-%data%
                                        ((f2cl-lib:int-add i 1))
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%))
                        (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)))
              label20))))
        (setf (f2cl-lib:fref f0-%data%
                             (np1)
                             ((1 (f2cl-lib:int-add n 1)))
                             f0-%offset%)
                (ddot np1 yp 1 y 1))
        (dcopy np1 y 1 dz 1)
        (daxpy np1 (- one) yold 1 dz 1)
        (setf dels (dnrm2 np1 dz 1))
        (setf sa (coerce 0.0f0 'double-float))
        (setf sb dels)
        (setf lcode 1)
       label40
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (root sout qsout sa sb rerr aerr lcode)
          (declare (ignore var-1 var-4 var-5))
          (setf sout var-0)
          (setf sa var-2)
          (setf sb var-3)
          (setf lcode var-6))
        (if (> lcode 0) (go label50))
        (setf qsout
                (-
                 (qofs
                  (f2cl-lib:fref yold-%data%
                                 (1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 yold-%offset%)
                  (f2cl-lib:fref ypold-%data%
                                 (1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 ypold-%offset%)
                  (f2cl-lib:fref y-%data%
                                 (1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 y-%offset%)
                  (f2cl-lib:fref yp-%data%
                                 (1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 yp-%offset%)
                  dels sout)
                 1.0f0))
        (go label40)
       label50
        (cond
          ((> lcode 2)
           (setf iflag 6)
           (go end_label)))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i np1) nil)
          (tagbody
            (setf (f2cl-lib:fref z-%data%
                                 (i)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 z-%offset%)
                    (qofs
                     (f2cl-lib:fref yold-%data%
                                    (i)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    yold-%offset%)
                     (f2cl-lib:fref ypold-%data%
                                    (i)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    ypold-%offset%)
                     (f2cl-lib:fref y-%data%
                                    (i)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    y-%offset%)
                     (f2cl-lib:fref yp-%data%
                                    (i)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    yp-%offset%)
                     dels sa))
           label60))
        (dcopy np1 z 1 dz 1)
        (daxpy np1 (- one) y 1 dz 1)
        (dcopy np1 yold 1 ypold 1)
        (setf brack f2cl-lib:%true%)
        (f2cl-lib:fdo (istep 1 (f2cl-lib:int-add istep 1))
                      ((> istep limit) nil)
          (tagbody
            (cond
              ((= iflag (f2cl-lib:int-sub 2))
               (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                   (rho a
                    (f2cl-lib:fref z-%data%
                                   (1)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   z-%offset%)
                    (f2cl-lib:array-slice z-%data%
                                          double-float
                                          (2)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          z-%offset%)
                    f1 par ipar)
                 (declare (ignore var-0 var-2 var-3 var-4 var-5))
                 (setf (f2cl-lib:fref z-%data%
                                      (1)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      z-%offset%)
                         var-1)))
              ((= iflag (f2cl-lib:int-sub 1))
               (f
                (f2cl-lib:array-slice z-%data%
                                      double-float
                                      (2)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      z-%offset%)
                f1)
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i n) nil)
                 (tagbody
                   (setf (f2cl-lib:fref f1-%data%
                                        (i)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        f1-%offset%)
                           (+
                            (*
                             (f2cl-lib:fref z-%data%
                                            (1)
                                            ((1 (f2cl-lib:int-add n 1)))
                                            z-%offset%)
                             (f2cl-lib:fref f1-%data%
                                            (i)
                                            ((1 (f2cl-lib:int-add n 1)))
                                            f1-%offset%))
                            (*
                             (- 1
                                (f2cl-lib:fref z-%data%
                                               (1)
                                               ((1 (f2cl-lib:int-add n 1)))
                                               z-%offset%))
                             (-
                              (f2cl-lib:fref z-%data%
                                             ((f2cl-lib:int-add i 1))
                                             ((1 (f2cl-lib:int-add n 1)))
                                             z-%offset%)
                              (f2cl-lib:fref a-%data%
                                             (i)
                                             ((1 n))
                                             a-%offset%)))))
                  label80)))
              (t
               (f
                (f2cl-lib:array-slice z-%data%
                                      double-float
                                      (2)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      z-%offset%)
                f1)
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i n) nil)
                 (tagbody
                   (setf (f2cl-lib:fref f1-%data%
                                        (i)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        f1-%offset%)
                           (-
                            (+
                             (*
                              (f2cl-lib:fref z-%data%
                                             (1)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             z-%offset%)
                              (-
                               (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)
                               (f2cl-lib:fref f1-%data%
                                              (i)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              f1-%offset%)))
                             (f2cl-lib:fref z-%data%
                                            ((f2cl-lib:int-add i 1))
                                            ((1 (f2cl-lib:int-add n 1)))
                                            z-%offset%))
                            (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)))
                  label90))))
            (setf (f2cl-lib:fref f1-%data%
                                 (np1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 f1-%offset%)
                    (ddot np1 yp 1 z 1))
            (upqrqf np1 eta dz f0 f1 qt r w t$)
            (dcopy n f1 1 dz 1)
            (dscal n (- one) dz 1)
            (setf (f2cl-lib:fref dz-%data%
                                 (np1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 dz-%offset%)
                    (coerce 0.0f0 'double-float))
            (qrslqf qt r dz w np1)
            (dcopy np1 z 1 w 1)
            (daxpy np1 one dz 1 z 1)
            (cond
              ((and
                (<=
                 (abs
                  (+ (f2cl-lib:fref z (1) ((1 (f2cl-lib:int-add n 1))))
                     (- 1.0f0)))
                 (+ rerr aerr))
                (<= (dnrm2 np1 dz 1)
                    (+
                     (* rerr
                        (dnrm2 n
                         (f2cl-lib:array-slice z
                                               double-float
                                               (2)
                                               ((1 (f2cl-lib:int-add n 1))))
                         1))
                     aerr)))
               (dcopy np1 z 1 y 1)
               (go end_label)))
            (dcopy np1 f1 1 f0 1)
            (cond
              ((<=
                (abs
                 (+ (f2cl-lib:fref z (1) ((1 (f2cl-lib:int-add n 1))))
                    (- 1.0f0)))
                (+ rerr aerr))
               (dcopy np1 z 1 dz 1)
               (daxpy np1 (- one) w 1 dz 1)
               (go label300)))
            (dcopy np1 y 1 yold 1)
            (dcopy np1 z 1 y 1)
            (cond
              ((>
                (*
                 (+ (f2cl-lib:fref y (1) ((1 (f2cl-lib:int-add n 1))))
                    (- 1.0f0))
                 (+ (f2cl-lib:fref yold (1) ((1 (f2cl-lib:int-add n 1))))
                    (- 1.0f0)))
                0)
               (setf brack f2cl-lib:%false%))
              (t
               (setf brack f2cl-lib:%true%)
               (dcopy np1 yold 1 ypold 1)))
            (dcopy np1 y 1 dz 1)
            (daxpy np1 (- one) ypold 1 dz 1)
            (setf dels (dnrm2 np1 dz 1))
            (setf sa
                    (/
                     (- 1.0f0
                        (f2cl-lib:fref y-%data%
                                       (1)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       y-%offset%))
                     (-
                      (f2cl-lib:fref yold-%data%
                                     (1)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     yold-%offset%)
                      (f2cl-lib:fref y-%data%
                                     (1)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     y-%offset%))))
            (dcopy np1 yold 1 dz 1)
            (daxpy np1 (- one) y 1 dz 1)
            (dscal np1 sa dz 1)
            (cond
              ((not brack)
               (cond
                 ((> (dnrm2 np1 dz 1) dels)
                  (setf sa
                          (/
                           (- 1.0f0
                              (f2cl-lib:fref y-%data%
                                             (1)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             y-%offset%))
                           (-
                            (f2cl-lib:fref ypold-%data%
                                           (1)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           ypold-%offset%)
                            (f2cl-lib:fref y-%data%
                                           (1)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           y-%offset%))))
                  (dcopy np1 ypold 1 dz 1)
                  (daxpy np1 (- one) y 1 dz 1)
                  (dscal np1 sa dz 1)))))
            (daxpy np1 one dz 1 z 1)
            (dcopy np1 z 1 dz 1)
            (daxpy np1 (- one) w 1 dz 1)
           label300))
        (setf iflag 6)
        (go end_label)
       end_label
        (return
         (values nil
                 nil
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
                 nil
                 nil
                 nil
                 nil
                 nil))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::rootqf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil fortran-to-lisp::iflag nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::f fortran-to-lisp::dscal
                    fortran-to-lisp::daxpy fortran-to-lisp::dcopy
                    fortran-to-lisp::ddot fortran-to-lisp::dnrm2
                    fortran-to-lisp::qrslqf fortran-to-lisp::upqrqf
                    fortran-to-lisp::root fortran-to-lisp::rho
                    fortran-to-lisp::d1mach))))

