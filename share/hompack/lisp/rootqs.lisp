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


(defun rootqs
       (n nfe iflag lenqr relerr abserr y yp yold ypold a qr pivot pp rhovec z
        dz work par ipar)
  (declare (type (array f2cl-lib:integer4 (*)) ipar)
           (type (array double-float (*)) par)
           (type (array f2cl-lib:integer4 (*)) pivot)
           (type (array double-float (*)) work dz z rhovec pp qr a ypold yold
                                          yp y)
           (type (double-float) abserr relerr)
           (type (f2cl-lib:integer4) lenqr iflag nfe n))
  (f2cl-lib:with-multi-array-data
      ((y double-float y-%data% y-%offset%)
       (yp double-float yp-%data% yp-%offset%)
       (yold double-float yold-%data% yold-%offset%)
       (ypold double-float ypold-%data% ypold-%offset%)
       (a double-float a-%data% a-%offset%)
       (qr double-float qr-%data% qr-%offset%)
       (pp double-float pp-%data% pp-%offset%)
       (rhovec double-float rhovec-%data% rhovec-%offset%)
       (z double-float z-%data% z-%offset%)
       (dz double-float dz-%data% dz-%offset%)
       (work double-float work-%data% work-%offset%)
       (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
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
      (prog ((brack nil) (istep 0) (i 0) (j 0) (lcode 0) (limit 0) (np1 0)
             (zu 0) (aerr 0.0) (dels 0.0) (one 0.0) (p0 0.0) (p1 0.0) (pp0 0.0)
             (pp1 0.0) (qsout 0.0) (rerr 0.0) (s 0.0) (sa 0.0) (sb 0.0)
             (sigma 0.0) (sout 0.0) (u 0.0) (zero 0.0) (lambda$ 0.0))
        (declare (type (double-float) lambda$ zero u sout sigma sb sa s rerr
                                      qsout pp1 pp0 p1 p0 one dels aerr)
                 (type (f2cl-lib:integer4) zu np1 limit lcode j i istep)
                 (type f2cl-lib:logical brack))
        (setf one (coerce 1.0f0 'double-float))
        (setf zero (coerce 0.0f0 'double-float))
        (setf u (f2cl-lib:d1mach 4))
        (setf rerr (max relerr u))
        (setf aerr (max abserr zero))
        (setf np1 (f2cl-lib:int-add n 1))
        (setf limit
                (f2cl-lib:int-mul 2
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int
                                    (-
                                     (f2cl-lib:log10
                                      (+ aerr (* rerr (dnrm2 np1 y 1))))))
                                   1)))
        (setf zu (f2cl-lib:int-add n 2))
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
        (dcopy np1 yold 1 ypold 1)
        (setf brack f2cl-lib:%true%)
        (f2cl-lib:fdo (istep 1 (f2cl-lib:int-add istep 1))
                      ((> istep limit) nil)
          (tagbody
            (f2cl-lib:fdo (j zu (f2cl-lib:int-add j 1))
                          ((> j (f2cl-lib:int-add zu (f2cl-lib:int-mul 2 n) 1))
                           nil)
              (tagbody
                (setf (f2cl-lib:fref work-%data%
                                     (j)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 6
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        lenqr)))
                                     work-%offset%)
                        (coerce 0.0f0 'double-float))
               label70))
            (setf lambda$
                    (f2cl-lib:fref z-%data%
                                   (np1)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   z-%offset%))
            (cond
              ((= iflag (f2cl-lib:int-sub 2))
               (rhojs a lambda$ z qr lenqr pivot pp par ipar)
               (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                   (rho a lambda$ z rhovec par ipar)
                 (declare (ignore var-0 var-2 var-3 var-4 var-5))
                 (setf lambda$ var-1)))
              ((= iflag (f2cl-lib:int-sub 1))
               (fjacs z qr lenqr pivot)
               (dscal lenqr lambda$ qr 1)
               (setf sigma (- 1.0f0 lambda$))
               (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                             ((> j n) nil)
                 (tagbody
                   (setf (f2cl-lib:fref qr-%data%
                                        ((f2cl-lib:fref pivot
                                                        (j)
                                                        ((1
                                                          (f2cl-lib:int-add n
                                                                            2)))))
                                        ((1 lenqr))
                                        qr-%offset%)
                           (+
                            (f2cl-lib:fref qr-%data%
                                           ((f2cl-lib:fref pivot
                                                           (j)
                                                           ((1
                                                             (f2cl-lib:int-add
                                                              n
                                                              2)))))
                                           ((1 lenqr))
                                           qr-%offset%)
                            sigma))
                  label80))
               (dcopy n z 1 rhovec 1)
               (daxpy n (- one) a 1 rhovec 1)
               (f z pp)
               (dscal n (- one) pp 1)
               (daxpy n one rhovec 1 pp 1)
               (daxpy n (- lambda$) pp 1 rhovec 1))
              (t
               (fjacs z qr lenqr pivot)
               (dscal lenqr (- lambda$) qr 1)
               (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                             ((> j n) nil)
                 (tagbody
                   (setf (f2cl-lib:fref qr-%data%
                                        ((f2cl-lib:fref pivot
                                                        (j)
                                                        ((1
                                                          (f2cl-lib:int-add n
                                                                            2)))))
                                        ((1 lenqr))
                                        qr-%offset%)
                           (+
                            (f2cl-lib:fref qr-%data%
                                           ((f2cl-lib:fref pivot
                                                           (j)
                                                           ((1
                                                             (f2cl-lib:int-add
                                                              n
                                                              2)))))
                                           ((1 lenqr))
                                           qr-%offset%)
                            1.0f0))
                  label90))
               (dcopy n z 1 rhovec 1)
               (daxpy n (- one) a 1 rhovec 1)
               (f z pp)
               (daxpy n (- one) a 1 pp 1)
               (daxpy n (- lambda$) pp 1 rhovec 1)))
            (setf (f2cl-lib:fref rhovec-%data%
                                 (np1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 rhovec-%offset%)
                    (coerce 0.0f0 'double-float))
            (setf nfe (f2cl-lib:int-add nfe 1))
            (multiple-value-bind
                  (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
                (pcgqs n qr lenqr pivot pp yp rhovec dz work iflag)
              (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                               var-8))
              (setf iflag var-9))
            (if (> iflag 0) (go end_label))
            (daxpy np1 one dz 1 z 1)
            (cond
              ((and
                (<=
                 (abs
                  (+ (f2cl-lib:fref z (np1) ((1 (f2cl-lib:int-add n 1))))
                     (- 1.0f0)))
                 (+ rerr aerr))
                (<= (dnrm2 np1 dz 1) (+ (* rerr (dnrm2 n z 1)) aerr)))
               (go end_label)))
            (if
             (<
              (abs
               (-
                (f2cl-lib:fref z-%data%
                               (np1)
                               ((1 (f2cl-lib:int-add n 1)))
                               z-%offset%)
                1.0f0))
              (+ rerr aerr))
             (go label300))
            (dcopy np1 y 1 yold 1)
            (dcopy np1 z 1 y 1)
            (cond
              ((>
                (*
                 (+ (f2cl-lib:fref yold (np1) ((1 (f2cl-lib:int-add n 1))))
                    (- 1.0f0))
                 (+ (f2cl-lib:fref y (np1) ((1 (f2cl-lib:int-add n 1))))
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
                                       (np1)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       y-%offset%))
                     (-
                      (f2cl-lib:fref yold-%data%
                                     (np1)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     yold-%offset%)
                      (f2cl-lib:fref y-%data%
                                     (np1)
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
                                             (np1)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             y-%offset%))
                           (-
                            (f2cl-lib:fref ypold-%data%
                                           (np1)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           ypold-%offset%)
                            (f2cl-lib:fref y-%data%
                                           (np1)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           y-%offset%))))
                  (dcopy np1 ypold 1 dz 1)
                  (daxpy np1 (- one) y 1 dz 1)
                  (dscal np1 sa dz 1)))))
            (dcopy np1 y 1 z 1)
            (daxpy np1 one dz 1 z 1)
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
                 nil
                 nil
                 nil
                 nil
                 nil))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::rootqs
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (double-float) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil fortran-to-lisp::nfe fortran-to-lisp::iflag nil
                            nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil)
           :calls '(fortran-to-lisp::f fortran-to-lisp::fjacs
                    fortran-to-lisp::rhojs fortran-to-lisp::dscal
                    fortran-to-lisp::daxpy fortran-to-lisp::dcopy
                    fortran-to-lisp::dnrm2 fortran-to-lisp::pcgqs
                    fortran-to-lisp::rho fortran-to-lisp::root
                    fortran-to-lisp::d1mach))))

