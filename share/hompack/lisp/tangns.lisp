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


(defun tangns
       (rholen y yp tz ypold a qr lenqr pivot pp rhovec work nfe n iflag par
        ipar)
  (declare (type (array f2cl-lib:integer4 (*)) ipar)
           (type (array double-float (*)) par)
           (type (array f2cl-lib:integer4 (*)) pivot)
           (type (f2cl-lib:integer4) iflag n nfe lenqr)
           (type (array double-float (*)) work rhovec pp qr a ypold tz yp y)
           (type (double-float) rholen))
  (f2cl-lib:with-multi-array-data
      ((y double-float y-%data% y-%offset%)
       (yp double-float yp-%data% yp-%offset%)
       (tz double-float tz-%data% tz-%offset%)
       (ypold double-float ypold-%data% ypold-%offset%)
       (a double-float a-%data% a-%offset%)
       (qr double-float qr-%data% qr-%offset%)
       (pp double-float pp-%data% pp-%offset%)
       (rhovec double-float rhovec-%data% rhovec-%offset%)
       (work double-float work-%data% work-%offset%)
       (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
       (par double-float par-%data% par-%offset%)
       (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
    (prog ((j 0) (np1 0) (np2 0) (n2p3 0) (n3p4 0) (n4p5 0) (sigma 0.0)
           (ypnorm 0.0) (lambda$ 0.0))
      (declare (type (double-float) lambda$ ypnorm sigma)
               (type (f2cl-lib:integer4) n4p5 n3p4 n2p3 np2 np1 j))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf np2 (f2cl-lib:int-add n 2))
      (setf n2p3 (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 3))
      (setf n3p4 (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) 4))
      (setf n4p5 (f2cl-lib:int-add (f2cl-lib:int-mul 4 n) 5))
      (setf nfe (f2cl-lib:int-add nfe 1))
      (setf lambda$
              (f2cl-lib:fref y-%data%
                             (np1)
                             ((1 (f2cl-lib:int-add n 1)))
                             y-%offset%))
      (cond
        ((= iflag (f2cl-lib:int-sub 2))
         (rhojs a lambda$ y qr lenqr pivot pp par ipar)
         (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
             (rho a lambda$ y rhovec par ipar)
           (declare (ignore var-0 var-2 var-3 var-4 var-5))
           (setf lambda$ var-1)))
        (t
         (f y pp)
         (dcopy n y 1 rhovec 1)
         (daxpy n -1.0 a 1 rhovec 1)
         (cond
           ((= iflag 0)
            (daxpy n -1.0 a 1 pp 1)
            (fjacs y qr lenqr pivot)
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
                                                          (f2cl-lib:int-add n
                                                                            2)))))
                                        ((1 lenqr))
                                        qr-%offset%)
                         1.0f0))
               label120))
            (daxpy n (- lambda$) pp 1 rhovec 1))
           (t
            (dscal n -1.0 pp 1)
            (daxpy n 1.0 rhovec 1 pp 1)
            (fjacs y qr lenqr pivot)
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
                                                          (f2cl-lib:int-add n
                                                                            2)))))
                                        ((1 lenqr))
                                        qr-%offset%)
                         sigma))
               label170))
            (daxpy n (- lambda$) pp 1 rhovec 1)))))
      (if (< rholen 0.0f0) (setf rholen (dnrm2 n rhovec 1)))
      (dcopy (f2cl-lib:int-mul 2 np1) work 1
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 8 (f2cl-lib:int-add n 1))
                                lenqr)))
                             work-%offset%)
       1)
      (dcopy np1 ypold 1 yp 1)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
          (pcgds n qr lenqr pivot pp yp
           (f2cl-lib:array-slice work-%data%
                                 double-float
                                 (n2p3)
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 8 (f2cl-lib:int-add n 1))
                                    lenqr)))
                                 work-%offset%)
           iflag)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))
        (setf iflag var-7))
      (if (> iflag 0) (go end_label))
      (dcopy (f2cl-lib:int-mul 2 np1)
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 8 (f2cl-lib:int-add n 1))
                                lenqr)))
                             work-%offset%)
       1 work 1)
      (setf ypnorm (dnrm2 np1 yp 1))
      (dscal np1 (/ 1.0f0 ypnorm) yp 1)
      (if (< (ddot np1 yp 1 ypold 1) 0.0f0) (dscal np1 -1.0 yp 1))
      (dscal (f2cl-lib:int-mul 2 np1) 0.0
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 8 (f2cl-lib:int-add n 1))
                                lenqr)))
                             work-%offset%)
       1)
      (dcopy np1 ypold 1 tz 1)
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
          (pcgns n qr lenqr pivot pp rhovec tz
           (f2cl-lib:array-slice work-%data%
                                 double-float
                                 (n2p3)
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 8 (f2cl-lib:int-add n 1))
                                    lenqr)))
                                 work-%offset%)
           iflag)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
        (setf iflag var-8))
      (if (> iflag 0) (go end_label))
      (setf sigma (ddot np1 tz 1 yp 1))
      (daxpy np1 (- sigma) yp 1 tz 1)
      (go end_label)
     end_label
      (return
       (values rholen
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
               nfe
               nil
               iflag
               nil
               nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::tangns
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(fortran-to-lisp::rholen nil nil nil nil nil nil nil
                            nil nil nil nil fortran-to-lisp::nfe nil
                            fortran-to-lisp::iflag nil nil)
           :calls '(fortran-to-lisp::fjacs fortran-to-lisp::f
                    fortran-to-lisp::rhojs fortran-to-lisp::ddot
                    fortran-to-lisp::dnrm2 fortran-to-lisp::dscal
                    fortran-to-lisp::daxpy fortran-to-lisp::dcopy
                    fortran-to-lisp::pcgns fortran-to-lisp::pcgds
                    fortran-to-lisp::rho))))

