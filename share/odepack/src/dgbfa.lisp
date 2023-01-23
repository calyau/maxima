;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2017-01 (21B Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "ODEPACK")


(defun dgbfa (abd lda n ml mu ipvt info)
  (declare (type (array f2cl-lib:integer4 (*)) ipvt)
           (type (f2cl-lib:integer4) info mu ml n lda)
           (type (array double-float (*)) abd))
  (f2cl-lib:with-multi-array-data
      ((abd double-float abd-%data% abd-%offset%)
       (ipvt f2cl-lib:integer4 ipvt-%data% ipvt-%offset%))
    (prog ((i 0) (i0 0) (j 0) (ju 0) (jz 0) (j0 0) (j1 0) (k 0) (kp1 0) (l 0)
           (lm 0) (m 0) (mm 0) (nm1 0) (t$ 0.0))
      (declare (type (double-float) t$)
               (type (f2cl-lib:integer4) nm1 mm m lm l kp1 k j1 j0 jz ju j i0
                                         i))
      (setf m (f2cl-lib:int-add ml mu 1))
      (setf info 0)
      (setf j0 (f2cl-lib:int-add mu 2))
      (setf j1
              (f2cl-lib:int-sub
               (min (the f2cl-lib:integer4 n) (the f2cl-lib:integer4 m))
               1))
      (if (< j1 j0) (go label30))
      (f2cl-lib:fdo (jz j0 (f2cl-lib:int-add jz 1))
                    ((> jz j1) nil)
        (tagbody
          (setf i0 (f2cl-lib:int-sub (f2cl-lib:int-add m 1) jz))
          (f2cl-lib:fdo (i i0 (f2cl-lib:int-add i 1))
                        ((> i ml) nil)
            (tagbody
              (setf (f2cl-lib:fref abd-%data%
                                   (i jz)
                                   ((1 lda) (1 *))
                                   abd-%offset%)
                      0.0)
             label10))
         label20))
     label30
      (setf jz j1)
      (setf ju 0)
      (setf nm1 (f2cl-lib:int-sub n 1))
      (if (< nm1 1) (go label130))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k nm1) nil)
        (tagbody
          (setf kp1 (f2cl-lib:int-add k 1))
          (setf jz (f2cl-lib:int-add jz 1))
          (if (> jz n) (go label50))
          (if (< ml 1) (go label50))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ml) nil)
            (tagbody
              (setf (f2cl-lib:fref abd-%data%
                                   (i jz)
                                   ((1 lda) (1 *))
                                   abd-%offset%)
                      0.0)
             label40))
         label50
          (setf lm
                  (min (the f2cl-lib:integer4 ml)
                       (the f2cl-lib:integer4 (f2cl-lib:int-sub n k))))
          (setf l
                  (f2cl-lib:int-sub
                   (f2cl-lib:int-add
                    (idamax (f2cl-lib:int-add lm 1)
                     (f2cl-lib:array-slice abd-%data%
                                           double-float
                                           (m k)
                                           ((1 lda) (1 *))
                                           abd-%offset%)
                     1)
                    m)
                   1))
          (setf (f2cl-lib:fref ipvt-%data% (k) ((1 *)) ipvt-%offset%)
                  (f2cl-lib:int-sub (f2cl-lib:int-add l k) m))
          (if
           (= (f2cl-lib:fref abd-%data% (l k) ((1 lda) (1 *)) abd-%offset%)
              0.0)
           (go label100))
          (if (= l m) (go label60))
          (setf t$
                  (f2cl-lib:fref abd-%data%
                                 (l k)
                                 ((1 lda) (1 *))
                                 abd-%offset%))
          (setf (f2cl-lib:fref abd-%data% (l k) ((1 lda) (1 *)) abd-%offset%)
                  (f2cl-lib:fref abd-%data%
                                 (m k)
                                 ((1 lda) (1 *))
                                 abd-%offset%))
          (setf (f2cl-lib:fref abd-%data% (m k) ((1 lda) (1 *)) abd-%offset%)
                  t$)
         label60
          (setf t$
                  (/ -1.0
                     (f2cl-lib:fref abd-%data%
                                    (m k)
                                    ((1 lda) (1 *))
                                    abd-%offset%)))
          (dscal lm t$
           (f2cl-lib:array-slice abd-%data%
                                 double-float
                                 ((+ m 1) k)
                                 ((1 lda) (1 *))
                                 abd-%offset%)
           1)
          (setf ju
                  (min
                   (the f2cl-lib:integer4
                        (max (the f2cl-lib:integer4 ju)
                             (the f2cl-lib:integer4
                                  (f2cl-lib:int-add mu
                                                    (f2cl-lib:fref ipvt-%data%
                                                                   (k)
                                                                   ((1 *))
                                                                   ipvt-%offset%)))))
                   (the f2cl-lib:integer4 n)))
          (setf mm m)
          (if (< ju kp1) (go label90))
          (f2cl-lib:fdo (j kp1 (f2cl-lib:int-add j 1))
                        ((> j ju) nil)
            (tagbody
              (setf l (f2cl-lib:int-sub l 1))
              (setf mm (f2cl-lib:int-sub mm 1))
              (setf t$
                      (f2cl-lib:fref abd-%data%
                                     (l j)
                                     ((1 lda) (1 *))
                                     abd-%offset%))
              (if (= l mm) (go label70))
              (setf (f2cl-lib:fref abd-%data%
                                   (l j)
                                   ((1 lda) (1 *))
                                   abd-%offset%)
                      (f2cl-lib:fref abd-%data%
                                     (mm j)
                                     ((1 lda) (1 *))
                                     abd-%offset%))
              (setf (f2cl-lib:fref abd-%data%
                                   (mm j)
                                   ((1 lda) (1 *))
                                   abd-%offset%)
                      t$)
             label70
              (daxpy lm t$
               (f2cl-lib:array-slice abd-%data%
                                     double-float
                                     ((+ m 1) k)
                                     ((1 lda) (1 *))
                                     abd-%offset%)
               1
               (f2cl-lib:array-slice abd-%data%
                                     double-float
                                     ((+ mm 1) j)
                                     ((1 lda) (1 *))
                                     abd-%offset%)
               1)
             label80))
         label90
          (go label110)
         label100
          (setf info k)
         label110
         label120))
     label130
      (setf (f2cl-lib:fref ipvt-%data% (n) ((1 *)) ipvt-%offset%) n)
      (if (= (f2cl-lib:fref abd-%data% (m n) ((1 lda) (1 *)) abd-%offset%) 0.0)
          (setf info n))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil info)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dgbfa fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::daxpy fortran-to-lisp::dscal
                    fortran-to-lisp::idamax))))

