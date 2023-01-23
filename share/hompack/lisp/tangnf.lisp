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


(defun tangnf (rholen y yp ypold a qr alpha tz pivot nfe n iflag par ipar)
  (declare (type (array f2cl-lib:integer4 (*)) ipar)
           (type (array double-float (*)) par)
           (type (f2cl-lib:integer4) iflag n nfe)
           (type (array f2cl-lib:integer4 (*)) pivot)
           (type (array double-float (*)) tz alpha qr a ypold yp y)
           (type (double-float) rholen))
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
    (prog ((i 0) (j 0) (jbar 0) (k 0) (kp1 0) (np1 0) (np2 0) (alphak 0.0)
           (beta 0.0) (qrkk 0.0) (sigma 0.0) (sum 0.0) (ypnorm 0.0)
           (lambda$ 0.0))
      (declare (type (double-float) lambda$ ypnorm sum sigma qrkk beta alphak)
               (type (f2cl-lib:integer4) np2 np1 kp1 k jbar j i))
      (setf lambda$
              (f2cl-lib:fref y-%data%
                             (1)
                             ((1 (f2cl-lib:int-add n 1)))
                             y-%offset%))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf np2 (f2cl-lib:int-add n 2))
      (setf nfe (f2cl-lib:int-add nfe 1))
      (cond
        ((= iflag (f2cl-lib:int-sub 2))
         (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                       ((> k np1) nil)
           (tagbody
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                 (rhojac a lambda$
                  (f2cl-lib:array-slice y-%data%
                                        double-float
                                        (2)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%)
                  (f2cl-lib:array-slice qr-%data%
                                        double-float
                                        (1 k)
                                        ((1 n) (1 (f2cl-lib:int-add n 2)))
                                        qr-%offset%)
                  k par ipar)
               (declare (ignore var-0 var-2 var-3 var-4 var-5 var-6))
               (setf lambda$ var-1))
            label30))
         (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
             (rho a lambda$
              (f2cl-lib:array-slice y-%data%
                                    double-float
                                    (2)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    y-%offset%)
              (f2cl-lib:array-slice qr-%data%
                                    double-float
                                    (1 np2)
                                    ((1 n) (1 (f2cl-lib:int-add n 2)))
                                    qr-%offset%)
              par ipar)
           (declare (ignore var-0 var-2 var-3 var-4 var-5))
           (setf lambda$ var-1)))
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
                (setf sigma (f2cl-lib:fref a-%data% (j) ((1 n)) a-%offset%))
                (setf beta
                        (- sigma
                           (f2cl-lib:fref tz-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          tz-%offset%)))
                (setf (f2cl-lib:fref qr-%data%
                                     (j 1)
                                     ((1 n) (1 (f2cl-lib:int-add n 2)))
                                     qr-%offset%)
                        beta)
               label100
                (setf (f2cl-lib:fref qr-%data%
                                     (j np2)
                                     ((1 n) (1 (f2cl-lib:int-add n 2)))
                                     qr-%offset%)
                        (+
                         (-
                          (f2cl-lib:fref y-%data%
                                         ((f2cl-lib:int-add j 1))
                                         ((1 (f2cl-lib:int-add n 1)))
                                         y-%offset%)
                          sigma)
                         (* lambda$ beta)))))
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
                                         ((1 n) (1 (f2cl-lib:int-add n 2)))
                                         qr-%offset%)
                            (* (- lambda$)
                               (f2cl-lib:fref tz-%data%
                                              (j)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              tz-%offset%)))))
               label120
                (setf (f2cl-lib:fref qr-%data%
                                     (k kp1)
                                     ((1 n) (1 (f2cl-lib:int-add n 2)))
                                     qr-%offset%)
                        (+ 1.0f0
                           (f2cl-lib:fref qr-%data%
                                          (k kp1)
                                          ((1 n) (1 (f2cl-lib:int-add n 2)))
                                          qr-%offset%))))))
           (t
            (tagbody
             label140
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (setf sigma
                          (-
                           (f2cl-lib:fref y-%data%
                                          ((f2cl-lib:int-add j 1))
                                          ((1 (f2cl-lib:int-add n 1)))
                                          y-%offset%)
                           (f2cl-lib:fref a-%data% (j) ((1 n)) a-%offset%)))
                  (setf beta
                          (-
                           (f2cl-lib:fref tz-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          tz-%offset%)
                           sigma))
                  (setf (f2cl-lib:fref qr-%data%
                                       (j 1)
                                       ((1 n) (1 (f2cl-lib:int-add n 2)))
                                       qr-%offset%)
                          beta)
                 label150
                  (setf (f2cl-lib:fref qr-%data%
                                       (j np2)
                                       ((1 n) (1 (f2cl-lib:int-add n 2)))
                                       qr-%offset%)
                          (+ sigma (* lambda$ beta)))))
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
                                           ((1 n) (1 (f2cl-lib:int-add n 2)))
                                           qr-%offset%)
                              (* lambda$
                                 (f2cl-lib:fref tz-%data%
                                                (j)
                                                ((1 (f2cl-lib:int-add n 1)))
                                                tz-%offset%)))))
                 label170
                  (setf (f2cl-lib:fref qr-%data%
                                       (k kp1)
                                       ((1 n) (1 (f2cl-lib:int-add n 2)))
                                       qr-%offset%)
                          (+ (- 1.0f0 lambda$)
                             (f2cl-lib:fref qr-%data%
                                            (k kp1)
                                            ((1 n) (1 (f2cl-lib:int-add n 2)))
                                            qr-%offset%))))))))))
      (if (< rholen 0.0f0)
          (setf rholen
                  (dnrm2 n
                   (f2cl-lib:array-slice qr-%data%
                                         double-float
                                         (1 np2)
                                         ((1 n) (1 (f2cl-lib:int-add n 2)))
                                         qr-%offset%)
                   1)))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref yp-%data%
                               (j)
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  (ddot n
                   (f2cl-lib:array-slice qr-%data%
                                         double-float
                                         (1 j)
                                         ((1 n) (1 (f2cl-lib:int-add n 2)))
                                         qr-%offset%)
                   1
                   (f2cl-lib:array-slice qr-%data%
                                         double-float
                                         (1 j)
                                         ((1 n) (1 (f2cl-lib:int-add n 2)))
                                         qr-%offset%)
                   1))
         label220
          (setf (f2cl-lib:fref pivot-%data%
                               (j)
                               ((1 (f2cl-lib:int-add n 1)))
                               pivot-%offset%)
                  j)))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf sigma
                  (f2cl-lib:fref yp-%data%
                                 (k)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 yp-%offset%))
          (setf jbar k)
          (setf kp1 (f2cl-lib:int-add k 1))
          (f2cl-lib:fdo (j kp1 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
              (if
               (>= sigma
                   (f2cl-lib:fref yp-%data%
                                  (j)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  yp-%offset%))
               (go label240))
              (setf sigma
                      (f2cl-lib:fref yp-%data%
                                     (j)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     yp-%offset%))
              (setf jbar j)
             label240))
          (if (= jbar k) (go label260))
          (setf i
                  (f2cl-lib:fref pivot-%data%
                                 (k)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 pivot-%offset%))
          (setf (f2cl-lib:fref pivot-%data%
                               (k)
                               ((1 (f2cl-lib:int-add n 1)))
                               pivot-%offset%)
                  (f2cl-lib:fref pivot-%data%
                                 (jbar)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 pivot-%offset%))
          (setf (f2cl-lib:fref pivot-%data%
                               (jbar)
                               ((1 (f2cl-lib:int-add n 1)))
                               pivot-%offset%)
                  i)
          (setf (f2cl-lib:fref yp-%data%
                               (jbar)
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  (f2cl-lib:fref yp-%data%
                                 (k)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 yp-%offset%))
          (setf (f2cl-lib:fref yp-%data%
                               (k)
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  sigma)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf sigma
                      (f2cl-lib:fref qr-%data%
                                     (i k)
                                     ((1 n) (1 (f2cl-lib:int-add n 2)))
                                     qr-%offset%))
              (setf (f2cl-lib:fref qr-%data%
                                   (i k)
                                   ((1 n) (1 (f2cl-lib:int-add n 2)))
                                   qr-%offset%)
                      (f2cl-lib:fref qr-%data%
                                     (i jbar)
                                     ((1 n) (1 (f2cl-lib:int-add n 2)))
                                     qr-%offset%))
              (setf (f2cl-lib:fref qr-%data%
                                   (i jbar)
                                   ((1 n) (1 (f2cl-lib:int-add n 2)))
                                   qr-%offset%)
                      sigma)
             label250))
         label260
          (setf sigma
                  (ddot (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1)
                   (f2cl-lib:array-slice qr-%data%
                                         double-float
                                         (k k)
                                         ((1 n) (1 (f2cl-lib:int-add n 2)))
                                         qr-%offset%)
                   1
                   (f2cl-lib:array-slice qr-%data%
                                         double-float
                                         (k k)
                                         ((1 n) (1 (f2cl-lib:int-add n 2)))
                                         qr-%offset%)
                   1))
          (cond
            ((= sigma 0.0f0)
             (setf iflag 4)
             (go end_label)))
         label270
          (if (= k n) (go label300))
          (setf qrkk
                  (f2cl-lib:fref qr-%data%
                                 (k k)
                                 ((1 n) (1 (f2cl-lib:int-add n 2)))
                                 qr-%offset%))
          (setf alphak (- (f2cl-lib:fsqrt sigma)))
          (if (< qrkk 0.0f0) (setf alphak (- alphak)))
          (setf (f2cl-lib:fref alpha-%data% (k) ((1 n)) alpha-%offset%) alphak)
          (setf beta (/ 1.0f0 (- sigma (* qrkk alphak))))
          (setf (f2cl-lib:fref qr-%data%
                               (k k)
                               ((1 n) (1 (f2cl-lib:int-add n 2)))
                               qr-%offset%)
                  (- qrkk alphak))
          (f2cl-lib:fdo (j kp1 (f2cl-lib:int-add j 1))
                        ((> j np2) nil)
            (tagbody
              (setf sigma
                      (* beta
                         (ddot (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1)
                          (f2cl-lib:array-slice qr-%data%
                                                double-float
                                                (k k)
                                                ((1 n)
                                                 (1 (f2cl-lib:int-add n 2)))
                                                qr-%offset%)
                          1
                          (f2cl-lib:array-slice qr-%data%
                                                double-float
                                                (k j)
                                                ((1 n)
                                                 (1 (f2cl-lib:int-add n 2)))
                                                qr-%offset%)
                          1)))
              (f2cl-lib:fdo (i k (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                  (setf (f2cl-lib:fref qr-%data%
                                       (i j)
                                       ((1 n) (1 (f2cl-lib:int-add n 2)))
                                       qr-%offset%)
                          (-
                           (f2cl-lib:fref qr-%data%
                                          (i j)
                                          ((1 n) (1 (f2cl-lib:int-add n 2)))
                                          qr-%offset%)
                           (*
                            (f2cl-lib:fref qr-%data%
                                           (i k)
                                           ((1 n) (1 (f2cl-lib:int-add n 2)))
                                           qr-%offset%)
                            sigma)))
                 label280))
              (if (< j np2)
                  (setf (f2cl-lib:fref yp-%data%
                                       (j)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       yp-%offset%)
                          (-
                           (f2cl-lib:fref yp-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          yp-%offset%)
                           (expt
                            (f2cl-lib:fref qr-%data%
                                           (k j)
                                           ((1 n) (1 (f2cl-lib:int-add n 2)))
                                           qr-%offset%)
                            2))))
             label290))
         label300))
      (setf (f2cl-lib:fref alpha-%data% (n) ((1 n)) alpha-%offset%)
              (f2cl-lib:fref qr-%data%
                             (n n)
                             ((1 n) (1 (f2cl-lib:int-add n 2)))
                             qr-%offset%))
      (setf (f2cl-lib:fref tz-%data%
                           (np1)
                           ((1 (f2cl-lib:int-add n 1)))
                           tz-%offset%)
              (coerce 1.0f0 'double-float))
      (f2cl-lib:fdo (i n (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                    ((> i 1) nil)
        (tagbody
          (setf sum (coerce 0.0f0 'double-float))
          (f2cl-lib:fdo (j (f2cl-lib:int-add i 1) (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
             label330
              (setf sum
                      (+ sum
                         (*
                          (f2cl-lib:fref qr-%data%
                                         (i j)
                                         ((1 n) (1 (f2cl-lib:int-add n 2)))
                                         qr-%offset%)
                          (f2cl-lib:fref tz-%data%
                                         (j)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         tz-%offset%))))))
         label340
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
         label360
          (setf (f2cl-lib:fref yp-%data%
                               ((f2cl-lib:fref pivot
                                               (k)
                                               ((1 (f2cl-lib:int-add n 1)))))
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  (/
                   (f2cl-lib:fref tz-%data%
                                  (k)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  tz-%offset%)
                   ypnorm))))
      (if (>= (ddot np1 yp 1 ypold 1) 0.0f0) (go label380))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i np1) nil)
        (tagbody
         label370
          (setf (f2cl-lib:fref yp-%data%
                               (i)
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  (-
                   (f2cl-lib:fref yp-%data%
                                  (i)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  yp-%offset%)))))
     label380
      (f2cl-lib:fdo (i n (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                    ((> i 1) nil)
        (tagbody
          (setf sum
                  (+
                   (f2cl-lib:fref qr-%data%
                                  (i np1)
                                  ((1 n) (1 (f2cl-lib:int-add n 2)))
                                  qr-%offset%)
                   (f2cl-lib:fref qr-%data%
                                  (i np2)
                                  ((1 n) (1 (f2cl-lib:int-add n 2)))
                                  qr-%offset%)))
          (f2cl-lib:fdo (j (f2cl-lib:int-add i 1) (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
             label430
              (setf sum
                      (+ sum
                         (*
                          (f2cl-lib:fref qr-%data%
                                         (i j)
                                         ((1 n) (1 (f2cl-lib:int-add n 2)))
                                         qr-%offset%)
                          (f2cl-lib:fref alpha-%data%
                                         (j)
                                         ((1 n))
                                         alpha-%offset%))))))
         label440
          (setf (f2cl-lib:fref alpha-%data% (i) ((1 n)) alpha-%offset%)
                  (/ (- sum)
                     (f2cl-lib:fref alpha-%data%
                                    (i)
                                    ((1 n))
                                    alpha-%offset%)))))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
         label450
          (setf (f2cl-lib:fref tz-%data%
                               ((f2cl-lib:fref pivot
                                               (k)
                                               ((1 (f2cl-lib:int-add n 1)))))
                               ((1 (f2cl-lib:int-add n 1)))
                               tz-%offset%)
                  (f2cl-lib:fref alpha-%data% (k) ((1 n)) alpha-%offset%))))
      (setf (f2cl-lib:fref tz-%data%
                           ((f2cl-lib:fref pivot
                                           (np1)
                                           ((1 (f2cl-lib:int-add n 1)))))
                           ((1 (f2cl-lib:int-add n 1)))
                           tz-%offset%)
              (coerce 1.0f0 'double-float))
      (setf sigma (ddot np1 tz 1 yp 1))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref tz-%data%
                               (j)
                               ((1 (f2cl-lib:int-add n 1)))
                               tz-%offset%)
                  (-
                   (f2cl-lib:fref tz-%data%
                                  (j)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  tz-%offset%)
                   (* sigma
                      (f2cl-lib:fref yp-%data%
                                     (j)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     yp-%offset%))))
         label470))
      (go end_label)
     end_label
      (return
       (values rholen nil nil nil nil nil nil nil nil nfe nil iflag nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::tangnf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(fortran-to-lisp::rholen nil nil nil nil nil nil nil
                            nil fortran-to-lisp::nfe nil fortran-to-lisp::iflag
                            nil nil)
           :calls '(fortran-to-lisp::fjac fortran-to-lisp::f
                    fortran-to-lisp::ddot fortran-to-lisp::dnrm2
                    fortran-to-lisp::rho fortran-to-lisp::rhojac))))

