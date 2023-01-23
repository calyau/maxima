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


(defun tangqs (y yp ypold a qr pivot pp rhovec work n lenqr iflag nfe par ipar)
  (declare (type (array f2cl-lib:integer4 (*)) ipar)
           (type (array double-float (*)) par)
           (type (f2cl-lib:integer4) nfe iflag lenqr n)
           (type (array f2cl-lib:integer4 (*)) pivot)
           (type (array double-float (*)) work rhovec pp qr a ypold yp y))
  (f2cl-lib:with-multi-array-data
      ((y double-float y-%data% y-%offset%)
       (yp double-float yp-%data% yp-%offset%)
       (ypold double-float ypold-%data% ypold-%offset%)
       (a double-float a-%data% a-%offset%)
       (qr double-float qr-%data% qr-%offset%)
       (pp double-float pp-%data% pp-%offset%)
       (rhovec double-float rhovec-%data% rhovec-%offset%)
       (work double-float work-%data% work-%offset%)
       (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
       (par double-float par-%data% par-%offset%)
       (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
    (prog ((j 0) (np1 0) (pcgwk 0) (zu 0) (one 0.0) (sigma 0.0) (ypnrm 0.0)
           (lambda$ 0.0))
      (declare (type (double-float) lambda$ ypnrm sigma one)
               (type (f2cl-lib:integer4) zu pcgwk np1 j))
      (setf one (coerce 1.0f0 'double-float))
      (setf nfe (f2cl-lib:int-add nfe 1))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf lambda$
              (f2cl-lib:fref y-%data%
                             (np1)
                             ((1 (f2cl-lib:int-add n 1)))
                             y-%offset%))
      (setf pcgwk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 3))
      (setf zu (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) 4))
      (cond
        ((= iflag (f2cl-lib:int-sub 2))
         (rhojs a lambda$ y qr lenqr pivot pp par ipar))
        ((= iflag (f2cl-lib:int-sub 1))
         (f y pp)
         (dscal n (- one) pp 1)
         (daxpy n one y 1 pp 1)
         (daxpy n (- one) a 1 pp 1)
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
                                                    (f2cl-lib:int-add n 2)))))
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
            label10)))
        (t
         (f y pp)
         (daxpy n (- one) a 1 pp 1)
         (fjacs y qr lenqr pivot)
         (dscal lenqr (- lambda$) qr 1)
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (setf (f2cl-lib:fref qr-%data%
                                  ((f2cl-lib:fref pivot
                                                  (j)
                                                  ((1
                                                    (f2cl-lib:int-add n 2)))))
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
            label20))))
      (dcopy (f2cl-lib:int-mul 2 np1) work 1
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (zu)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 8 (f2cl-lib:int-add n 1))
                                lenqr)))
                             work-%offset%)
       1)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf (f2cl-lib:fref rhovec-%data%
                               (j)
                               ((1 (f2cl-lib:int-add n 1)))
                               rhovec-%offset%)
                  (coerce 0.0f0 'double-float))
         label30))
      (setf (f2cl-lib:fref rhovec-%data%
                           (np1)
                           ((1 (f2cl-lib:int-add n 1)))
                           rhovec-%offset%)
              (coerce -1.0f0 'double-float))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
          (pcgqs n qr lenqr pivot pp ypold rhovec yp
           (f2cl-lib:array-slice work-%data%
                                 double-float
                                 (pcgwk)
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 8 (f2cl-lib:int-add n 1))
                                    lenqr)))
                                 work-%offset%)
           iflag)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                         var-8))
        (setf iflag var-9))
      (if (> iflag 0) (go end_label))
      (setf ypnrm (/ 1.0f0 (dnrm2 np1 yp 1)))
      (dscal np1 ypnrm yp 1)
      (dcopy (f2cl-lib:int-mul 2 np1)
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (zu)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 8 (f2cl-lib:int-add n 1))
                                lenqr)))
                             work-%offset%)
       1 work 1)
      (go end_label)
     end_label
      (return
       (values nil
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
               iflag
               nfe
               nil
               nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::tangqs
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::iflag fortran-to-lisp::nfe nil
                            nil)
           :calls '(fortran-to-lisp::fjacs fortran-to-lisp::f
                    fortran-to-lisp::rhojs fortran-to-lisp::dnrm2
                    fortran-to-lisp::dcopy fortran-to-lisp::daxpy
                    fortran-to-lisp::dscal fortran-to-lisp::pcgqs))))

