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


(defun fode (s y yp ypold a qr alpha tz pivot nfe n iflag par ipar)
  (declare (type (array f2cl-lib:integer4 (*)) ipar)
           (type (array double-float (*)) par)
           (type (f2cl-lib:integer4) iflag n nfe)
           (type (array f2cl-lib:integer4 (*)) pivot)
           (type (array double-float (*)) tz alpha qr a ypold yp y)
           (type (double-float) s))
  (f2cl-lib:with-multi-array-data
      ((y double-float y-%data% y-%offset%)
       (yp double-float yp-%data% yp-%offset%)
       (ypold double-float ypold-%data% ypold-%offset%)
       (a double-float a-%data% a-%offset%)
       (qr double-float qr-%data% qr-%offset%)
       (alpha double-float alpha-%data% alpha-%offset%)
       (tz double-float tz-%data% tz-%offset%)
       (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
       (par double-float par-%data% par-%offset%)
       (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
    (prog ((i 0) (ierr 0) (ik 0) (j 0) (k 0) (kp1 0) (kpiv 0) (lw 0) (np1 0)
           (sum 0.0) (ypnorm 0.0))
      (declare (type (double-float) ypnorm sum)
               (type (f2cl-lib:integer4) np1 lw kpiv kp1 k j ik ierr i))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf nfe (f2cl-lib:int-add nfe 1))
      (cond
        ((= iflag (f2cl-lib:int-sub 2))
         (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                       ((> k np1) nil)
           (tagbody
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                 (rhojac a
                  (f2cl-lib:fref y-%data%
                                 (1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 y-%offset%)
                  (f2cl-lib:array-slice y-%data%
                                        double-float
                                        (2)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%)
                  (f2cl-lib:array-slice qr-%data%
                                        double-float
                                        (1 k)
                                        ((1 n) (1 (f2cl-lib:int-add n 1)))
                                        qr-%offset%)
                  k par ipar)
               (declare (ignore var-0 var-2 var-3 var-4 var-5 var-6))
               (setf (f2cl-lib:fref y-%data%
                                    (1)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    y-%offset%)
                       var-1))
            label30)))
        (t
         (f
          (f2cl-lib:array-slice y-%data%
                                double-float
                                (2)
                                ((1 (f2cl-lib:int-add n 1)))
                                y-%offset%)
          tz)
         (cond
           ((= iflag 0)
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j n) nil)
              (tagbody
               label100
                (setf (f2cl-lib:fref qr-%data%
                                     (j 1)
                                     ((1 n) (1 (f2cl-lib:int-add n 1)))
                                     qr-%offset%)
                        (- (f2cl-lib:fref a-%data% (j) ((1 n)) a-%offset%)
                           (f2cl-lib:fref tz-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          tz-%offset%)))))
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k n) nil)
              (tagbody
                (fjac
                 (f2cl-lib:array-slice y-%data%
                                       double-float
                                       (2)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       y-%offset%)
                 tz k)
                (setf kp1 (f2cl-lib:int-add k 1))
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j n) nil)
                  (tagbody
                   label110
                    (setf (f2cl-lib:fref qr-%data%
                                         (j kp1)
                                         ((1 n) (1 (f2cl-lib:int-add n 1)))
                                         qr-%offset%)
                            (*
                             (-
                              (f2cl-lib:fref y-%data%
                                             (1)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             y-%offset%))
                             (f2cl-lib:fref tz-%data%
                                            (j)
                                            ((1 (f2cl-lib:int-add n 1)))
                                            tz-%offset%)))))
               label120
                (setf (f2cl-lib:fref qr-%data%
                                     (k kp1)
                                     ((1 n) (1 (f2cl-lib:int-add n 1)))
                                     qr-%offset%)
                        (+ 1.0f0
                           (f2cl-lib:fref qr-%data%
                                          (k kp1)
                                          ((1 n) (1 (f2cl-lib:int-add n 1)))
                                          qr-%offset%))))))
           (t
            (tagbody
             label140
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                 label150
                  (setf (f2cl-lib:fref qr-%data%
                                       (j 1)
                                       ((1 n) (1 (f2cl-lib:int-add n 1)))
                                       qr-%offset%)
                          (+
                           (-
                            (f2cl-lib:fref tz-%data%
                                           (j)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           tz-%offset%)
                            (f2cl-lib:fref y-%data%
                                           ((f2cl-lib:int-add j 1))
                                           ((1 (f2cl-lib:int-add n 1)))
                                           y-%offset%))
                           (f2cl-lib:fref a-%data% (j) ((1 n)) a-%offset%)))))
              (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                            ((> k n) nil)
                (tagbody
                  (fjac
                   (f2cl-lib:array-slice y-%data%
                                         double-float
                                         (2)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         y-%offset%)
                   tz k)
                  (setf kp1 (f2cl-lib:int-add k 1))
                  (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                                ((> j n) nil)
                    (tagbody
                     label160
                      (setf (f2cl-lib:fref qr-%data%
                                           (j kp1)
                                           ((1 n) (1 (f2cl-lib:int-add n 1)))
                                           qr-%offset%)
                              (*
                               (f2cl-lib:fref y-%data%
                                              (1)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              y-%offset%)
                               (f2cl-lib:fref tz-%data%
                                              (j)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              tz-%offset%)))))
                 label170
                  (setf (f2cl-lib:fref qr-%data%
                                       (k kp1)
                                       ((1 n) (1 (f2cl-lib:int-add n 1)))
                                       qr-%offset%)
                          (+
                           (- 1.0f0
                              (f2cl-lib:fref y-%data%
                                             (1)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             y-%offset%))
                           (f2cl-lib:fref qr-%data%
                                          (k kp1)
                                          ((1 n) (1 (f2cl-lib:int-add n 1)))
                                          qr-%offset%))))))))))
     label210
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
          (dcpose n n qr alpha pivot ierr tz yp)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-6 var-7))
        (setf ierr var-5))
      (if (= ierr 0) (go label220))
      (setf iflag 4)
      (go end_label)
     label220
      (setf (f2cl-lib:fref tz-%data%
                           (np1)
                           ((1 (f2cl-lib:int-add n 1)))
                           tz-%offset%)
              (coerce 1.0f0 'double-float))
      (f2cl-lib:fdo (lw 1 (f2cl-lib:int-add lw 1))
                    ((> lw n) nil)
        (tagbody
          (setf i (f2cl-lib:int-sub np1 lw))
          (setf ik (f2cl-lib:int-add i 1))
          (setf sum (coerce 0.0f0 'double-float))
          (f2cl-lib:fdo (j ik (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
             label230
              (setf sum
                      (+ sum
                         (*
                          (f2cl-lib:fref qr-%data%
                                         (i j)
                                         ((1 n) (1 (f2cl-lib:int-add n 1)))
                                         qr-%offset%)
                          (f2cl-lib:fref tz-%data%
                                         (j)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         tz-%offset%))))))
         label240
          (setf (f2cl-lib:fref tz-%data%
                               (i)
                               ((1 (f2cl-lib:int-add n 1)))
                               tz-%offset%)
                  (/ (- sum)
                     (f2cl-lib:fref alpha-%data%
                                    (i)
                                    ((1 n))
                                    alpha-%offset%)))))
      (setf ypnorm (dnrm2 np1 tz 1))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k np1) nil)
        (tagbody
          (setf kpiv
                  (f2cl-lib:fref pivot-%data%
                                 (k)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 pivot-%offset%))
         label260
          (setf (f2cl-lib:fref yp-%data%
                               (kpiv)
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  (/
                   (f2cl-lib:fref tz-%data%
                                  (k)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  tz-%offset%)
                   ypnorm))))
      (if (>= (ddot np1 yp 1 ypold 1) 0.0f0) (go label280))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i np1) nil)
        (tagbody
         label270
          (setf (f2cl-lib:fref yp-%data%
                               (i)
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  (-
                   (f2cl-lib:fref yp-%data%
                                  (i)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  yp-%offset%)))))
     label280
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i np1) nil)
        (tagbody
         label290
          (setf (f2cl-lib:fref ypold-%data%
                               (i)
                               ((1 (f2cl-lib:int-add n 1)))
                               ypold-%offset%)
                  (f2cl-lib:fref yp-%data%
                                 (i)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 yp-%offset%))))
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nfe nil iflag nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::fode fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::nfe nil fortran-to-lisp::iflag nil
                            nil)
           :calls '(fortran-to-lisp::fjac fortran-to-lisp::f
                    fortran-to-lisp::ddot fortran-to-lisp::dnrm2
                    fortran-to-lisp::dcpose fortran-to-lisp::rhojac))))

