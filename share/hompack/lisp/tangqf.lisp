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


(defun tangqf (y yp ypold a qt r w s t$ n iflag nfe par ipar)
  (declare (type (array f2cl-lib:integer4 (*)) ipar)
           (type (array double-float (*)) par)
           (type (f2cl-lib:integer4) nfe iflag n)
           (type (array double-float (*)) t$ s w r qt a ypold yp y))
  (f2cl-lib:with-multi-array-data
      ((y double-float y-%data% y-%offset%)
       (yp double-float yp-%data% yp-%offset%)
       (ypold double-float ypold-%data% ypold-%offset%)
       (a double-float a-%data% a-%offset%)
       (qt double-float qt-%data% qt-%offset%)
       (r double-float r-%data% r-%offset%)
       (w double-float w-%data% w-%offset%)
       (s double-float s-%data% s-%offset%)
       (t$ double-float t$-%data% t$-%offset%)
       (par double-float par-%data% par-%offset%)
       (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
    (prog ((i 0) (j 0) (jp1 0) (np1 0) (one 0.0) (ypnrm 0.0) (lambda$ 0.0))
      (declare (type (double-float) lambda$ ypnrm one)
               (type (f2cl-lib:integer4) np1 jp1 j i))
      (setf one (coerce 1.0f0 'double-float))
      (setf nfe (f2cl-lib:int-add nfe 1))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf lambda$
              (f2cl-lib:fref y-%data%
                             (1)
                             ((1 (f2cl-lib:int-add n 1)))
                             y-%offset%))
      (cond
        ((= iflag (f2cl-lib:int-sub 2))
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j np1) nil)
           (tagbody
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                 (rhojac a lambda$
                  (f2cl-lib:array-slice y-%data%
                                        double-float
                                        (2)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%)
                  (f2cl-lib:array-slice qt-%data%
                                        double-float
                                        (1 j)
                                        ((1 (f2cl-lib:int-add n 1))
                                         (1 (f2cl-lib:int-add n 1)))
                                        qt-%offset%)
                  j par ipar)
               (declare (ignore var-0 var-2 var-3 var-4 var-5 var-6))
               (setf lambda$ var-1))
            label10)))
        ((= iflag (f2cl-lib:int-sub 1))
         (f
          (f2cl-lib:array-slice y-%data%
                                double-float
                                (2)
                                ((1 (f2cl-lib:int-add n 1)))
                                y-%offset%)
          (f2cl-lib:array-slice qt-%data%
                                double-float
                                (1 1)
                                ((1 (f2cl-lib:int-add n 1))
                                 (1 (f2cl-lib:int-add n 1)))
                                qt-%offset%))
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i n) nil)
           (tagbody
             (setf (f2cl-lib:fref qt-%data%
                                  (i 1)
                                  ((1 (f2cl-lib:int-add n 1))
                                   (1 (f2cl-lib:int-add n 1)))
                                  qt-%offset%)
                     (+
                      (- (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)
                         (f2cl-lib:fref y-%data%
                                        ((f2cl-lib:int-add i 1))
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%))
                      (f2cl-lib:fref qt-%data%
                                     (i 1)
                                     ((1 (f2cl-lib:int-add n 1))
                                      (1 (f2cl-lib:int-add n 1)))
                                     qt-%offset%)))
            label20))
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (setf jp1 (f2cl-lib:int-add j 1))
             (fjac
              (f2cl-lib:array-slice y-%data%
                                    double-float
                                    (2)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    y-%offset%)
              (f2cl-lib:array-slice qt-%data%
                                    double-float
                                    (1 jp1)
                                    ((1 (f2cl-lib:int-add n 1))
                                     (1 (f2cl-lib:int-add n 1)))
                                    qt-%offset%)
              j)
             (dscal n lambda$
              (f2cl-lib:array-slice qt-%data%
                                    double-float
                                    (1 jp1)
                                    ((1 (f2cl-lib:int-add n 1))
                                     (1 (f2cl-lib:int-add n 1)))
                                    qt-%offset%)
              1)
             (setf (f2cl-lib:fref qt-%data%
                                  (j jp1)
                                  ((1 (f2cl-lib:int-add n 1))
                                   (1 (f2cl-lib:int-add n 1)))
                                  qt-%offset%)
                     (+ (- 1.0f0 lambda$)
                        (f2cl-lib:fref qt-%data%
                                       (j jp1)
                                       ((1 (f2cl-lib:int-add n 1))
                                        (1 (f2cl-lib:int-add n 1)))
                                       qt-%offset%)))
            label30)))
        (t
         (f
          (f2cl-lib:array-slice y-%data%
                                double-float
                                (2)
                                ((1 (f2cl-lib:int-add n 1)))
                                y-%offset%)
          (f2cl-lib:array-slice qt-%data%
                                double-float
                                (1 1)
                                ((1 (f2cl-lib:int-add n 1))
                                 (1 (f2cl-lib:int-add n 1)))
                                qt-%offset%))
         (dscal n (- one)
          (f2cl-lib:array-slice qt-%data%
                                double-float
                                (1 1)
                                ((1 (f2cl-lib:int-add n 1))
                                 (1 (f2cl-lib:int-add n 1)))
                                qt-%offset%)
          1)
         (daxpy n one a 1
          (f2cl-lib:array-slice qt-%data%
                                double-float
                                (1 1)
                                ((1 (f2cl-lib:int-add n 1))
                                 (1 (f2cl-lib:int-add n 1)))
                                qt-%offset%)
          1)
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (setf jp1 (f2cl-lib:int-add j 1))
             (fjac
              (f2cl-lib:array-slice y-%data%
                                    double-float
                                    (2)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    y-%offset%)
              (f2cl-lib:array-slice qt-%data%
                                    double-float
                                    (1 jp1)
                                    ((1 (f2cl-lib:int-add n 1))
                                     (1 (f2cl-lib:int-add n 1)))
                                    qt-%offset%)
              j)
             (dscal n (- lambda$)
              (f2cl-lib:array-slice qt-%data%
                                    double-float
                                    (1 jp1)
                                    ((1 (f2cl-lib:int-add n 1))
                                     (1 (f2cl-lib:int-add n 1)))
                                    qt-%offset%)
              1)
             (setf (f2cl-lib:fref qt-%data%
                                  (j jp1)
                                  ((1 (f2cl-lib:int-add n 1))
                                   (1 (f2cl-lib:int-add n 1)))
                                  qt-%offset%)
                     (+ 1.0f0
                        (f2cl-lib:fref qt-%data%
                                       (j jp1)
                                       ((1 (f2cl-lib:int-add n 1))
                                        (1 (f2cl-lib:int-add n 1)))
                                       qt-%offset%)))
            label50))))
      (dcopy np1 ypold 1
       (f2cl-lib:array-slice qt-%data%
                             double-float
                             (np1 1)
                             ((1 (f2cl-lib:int-add n 1))
                              (1 (f2cl-lib:int-add n 1)))
                             qt-%offset%)
       np1)
      (multiple-value-bind (var-0 var-1 var-2 var-3)
          (qrfaqf qt r np1 iflag)
        (declare (ignore var-0 var-1 var-2))
        (setf iflag var-3))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf (f2cl-lib:fref yp-%data%
                               (j)
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  (coerce 0.0f0 'double-float))
         label70))
      (setf (f2cl-lib:fref yp-%data%
                           (np1)
                           ((1 (f2cl-lib:int-add n 1)))
                           yp-%offset%)
              (coerce 1.0f0 'double-float))
      (qrslqf qt r yp w np1)
      (setf ypnrm (/ 1.0f0 (dnrm2 np1 yp 1)))
      (dscal np1 ypnrm yp 1)
      (dcopy np1 yp 1 s 1)
      (daxpy np1 (- one) ypold 1 s 1)
      (dcopy np1
       (f2cl-lib:array-slice qt-%data%
                             double-float
                             (1 np1)
                             ((1 (f2cl-lib:int-add n 1))
                              (1 (f2cl-lib:int-add n 1)))
                             qt-%offset%)
       1 t$ 1)
      (r1upqf np1 s t$ qt r w)
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil iflag nfe nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::tangqf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::iflag fortran-to-lisp::nfe nil
                            nil)
           :calls '(fortran-to-lisp::fjac fortran-to-lisp::f
                    fortran-to-lisp::dnrm2 fortran-to-lisp::dcopy
                    fortran-to-lisp::daxpy fortran-to-lisp::dscal
                    fortran-to-lisp::r1upqf fortran-to-lisp::qrslqf
                    fortran-to-lisp::qrfaqf fortran-to-lisp::rhojac))))

