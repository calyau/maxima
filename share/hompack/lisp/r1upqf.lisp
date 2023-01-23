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


(defun r1upqf (n s t$ qt r w)
  (declare (type (array double-float (*)) w r qt t$ s)
           (type (f2cl-lib:integer4) n))
  (f2cl-lib:with-multi-array-data
      ((s double-float s-%data% s-%offset%)
       (t$ double-float t$-%data% t$-%offset%)
       (qt double-float qt-%data% qt-%offset%)
       (r double-float r-%data% r-%offset%)
       (w double-float w-%data% w-%offset%))
    (prog ((tt (make-array 2 :element-type 'double-float)) (eta 0.0)
           (skipup nil) (i 0) (indexr 0) (indxr2 0) (j 0) (k 0) (c 0.0)
           (den 0.0) (one 0.0) (ss 0.0) (ww 0.0) (yy 0.0) (temp 0.0)
           (ddot 0.0))
      (declare (type (f2cl-lib:integer4) k j indxr2 indexr i)
               (type f2cl-lib:logical skipup)
               (type (double-float) ddot temp yy ww ss one den c eta)
               (type (array double-float (2)) tt))
      (setf k n)
     label50
      (if
       (or (/= (f2cl-lib:fref t$-%data% (k) ((1 n)) t$-%offset%) 0.0f0)
           (<= k 1))
       (go label60))
      (setf k (f2cl-lib:int-sub k 1))
      (go label50)
     label60
      (setf indexr
              (+
               (the f2cl-lib:integer4
                    (truncate (* (+ (- (+ n n) k) 3) (- k 2)) 2))
               1))
      (f2cl-lib:fdo (i (f2cl-lib:int-add k (f2cl-lib:int-sub 1))
                     (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                    ((> i 1) nil)
        (tagbody
          (cond
            ((= (f2cl-lib:fref t$ (i) ((1 n))) 0.0f0)
             (setf c (coerce 0.0f0 'double-float))
             (setf ss
                     (-
                      (f2cl-lib:sign one
                                     (f2cl-lib:fref t$-%data%
                                                    ((f2cl-lib:int-add i 1))
                                                    ((1 n))
                                                    t$-%offset%)))))
            (t
             (setf den
                     (dnrm2 2
                      (f2cl-lib:array-slice t$-%data%
                                            double-float
                                            (i)
                                            ((1 n))
                                            t$-%offset%)
                      1))
             (setf c (/ (f2cl-lib:fref t$-%data% (i) ((1 n)) t$-%offset%) den))
             (setf ss
                     (/
                      (-
                       (f2cl-lib:fref t$-%data%
                                      ((f2cl-lib:int-add i 1))
                                      ((1 n))
                                      t$-%offset%))
                      den))))
          (setf yy
                  (f2cl-lib:fref r-%data%
                                 (indexr)
                                 ((1
                                   (f2cl-lib:f2cl/
                                    (f2cl-lib:int-mul n (f2cl-lib:int-add n 1))
                                    2)))
                                 r-%offset%))
          (setf ww (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref r-%data%
                               (indexr)
                               ((1
                                 (f2cl-lib:f2cl/
                                  (f2cl-lib:int-mul n (f2cl-lib:int-add n 1))
                                  2)))
                               r-%offset%)
                  (- (* c yy) (* ss ww)))
          (setf (f2cl-lib:fref w-%data%
                               ((f2cl-lib:int-add i 1))
                               ((1 n))
                               w-%offset%)
                  (+ (* ss yy) (* c ww)))
          (setf indexr (f2cl-lib:int-add indexr 1))
          (setf indxr2 (f2cl-lib:int-sub (f2cl-lib:int-add indexr n) i))
          (f2cl-lib:fdo (j (f2cl-lib:int-add i 1) (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf yy
                      (f2cl-lib:fref r-%data%
                                     (indexr)
                                     ((1
                                       (f2cl-lib:f2cl/
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        2)))
                                     r-%offset%))
              (setf ww
                      (f2cl-lib:fref r-%data%
                                     (indxr2)
                                     ((1
                                       (f2cl-lib:f2cl/
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        2)))
                                     r-%offset%))
              (setf (f2cl-lib:fref r-%data%
                                   (indexr)
                                   ((1
                                     (f2cl-lib:f2cl/
                                      (f2cl-lib:int-mul n
                                                        (f2cl-lib:int-add n 1))
                                      2)))
                                   r-%offset%)
                      (- (* c yy) (* ss ww)))
              (setf (f2cl-lib:fref r-%data%
                                   (indxr2)
                                   ((1
                                     (f2cl-lib:f2cl/
                                      (f2cl-lib:int-mul n
                                                        (f2cl-lib:int-add n 1))
                                      2)))
                                   r-%offset%)
                      (+ (* ss yy) (* c ww)))
              (setf indexr (f2cl-lib:int-add indexr 1))
              (setf indxr2 (f2cl-lib:int-add indxr2 1))
             label70))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf yy
                      (f2cl-lib:fref qt-%data%
                                     (i j)
                                     ((1 n) (1 n))
                                     qt-%offset%))
              (setf ww
                      (f2cl-lib:fref qt-%data%
                                     ((f2cl-lib:int-add i 1) j)
                                     ((1 n) (1 n))
                                     qt-%offset%))
              (setf (f2cl-lib:fref qt-%data% (i j) ((1 n) (1 n)) qt-%offset%)
                      (- (* c yy) (* ss ww)))
              (setf (f2cl-lib:fref qt-%data%
                                   ((f2cl-lib:int-add i 1) j)
                                   ((1 n) (1 n))
                                   qt-%offset%)
                      (+ (* ss yy) (* c ww)))
             label80))
          (cond
            ((= (f2cl-lib:fref t$ (i) ((1 n))) 0.0f0)
             (setf (f2cl-lib:fref t$-%data% (i) ((1 n)) t$-%offset%)
                     (abs
                      (f2cl-lib:fref t$-%data%
                                     ((f2cl-lib:int-add i 1))
                                     ((1 n))
                                     t$-%offset%))))
            (t
             (setf (f2cl-lib:fref t$-%data% (i) ((1 n)) t$-%offset%)
                     (dnrm2 2
                      (f2cl-lib:array-slice t$-%data%
                                            double-float
                                            (i)
                                            ((1 n))
                                            t$-%offset%)
                      1))))
          (setf indexr
                  (f2cl-lib:int-sub indexr
                                    (f2cl-lib:int-mul 2 (f2cl-lib:int-sub n i))
                                    3))
         label90))
      (setf temp (f2cl-lib:fref t$-%data% (1) ((1 n)) t$-%offset%))
      (daxpy n temp s 1 r 1)
      (setf indexr 1)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i (f2cl-lib:int-add k (f2cl-lib:int-sub 1))) nil)
        (tagbody
          (cond
            ((=
              (f2cl-lib:fref r
                             (indexr)
                             ((1
                               (f2cl-lib:f2cl/
                                (f2cl-lib:int-mul n (f2cl-lib:int-add n 1))
                                2))))
              0.0f0)
             (setf c (coerce 0.0f0 'double-float))
             (setf ss
                     (-
                      (f2cl-lib:sign one
                                     (f2cl-lib:fref w-%data%
                                                    ((f2cl-lib:int-add i 1))
                                                    ((1 n))
                                                    w-%offset%)))))
            (t
             (setf (f2cl-lib:fref tt (1) ((1 2)))
                     (f2cl-lib:fref r-%data%
                                    (indexr)
                                    ((1
                                      (f2cl-lib:f2cl/
                                       (f2cl-lib:int-mul n
                                                         (f2cl-lib:int-add n
                                                                           1))
                                       2)))
                                    r-%offset%))
             (setf (f2cl-lib:fref tt (2) ((1 2)))
                     (f2cl-lib:fref w-%data%
                                    ((f2cl-lib:int-add i 1))
                                    ((1 n))
                                    w-%offset%))
             (setf den (dnrm2 2 tt 1))
             (setf c
                     (/
                      (f2cl-lib:fref r-%data%
                                     (indexr)
                                     ((1
                                       (f2cl-lib:f2cl/
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        2)))
                                     r-%offset%)
                      den))
             (setf ss
                     (/
                      (-
                       (f2cl-lib:fref w-%data%
                                      ((f2cl-lib:int-add i 1))
                                      ((1 n))
                                      w-%offset%))
                      den))))
          (setf yy
                  (f2cl-lib:fref r-%data%
                                 (indexr)
                                 ((1
                                   (f2cl-lib:f2cl/
                                    (f2cl-lib:int-mul n (f2cl-lib:int-add n 1))
                                    2)))
                                 r-%offset%))
          (setf ww
                  (f2cl-lib:fref w-%data%
                                 ((f2cl-lib:int-add i 1))
                                 ((1 n))
                                 w-%offset%))
          (setf (f2cl-lib:fref r-%data%
                               (indexr)
                               ((1
                                 (f2cl-lib:f2cl/
                                  (f2cl-lib:int-mul n (f2cl-lib:int-add n 1))
                                  2)))
                               r-%offset%)
                  (- (* c yy) (* ss ww)))
          (setf (f2cl-lib:fref w-%data%
                               ((f2cl-lib:int-add i 1))
                               ((1 n))
                               w-%offset%)
                  (coerce 0.0f0 'double-float))
          (setf indexr (f2cl-lib:int-add indexr 1))
          (setf indxr2 (f2cl-lib:int-sub (f2cl-lib:int-add indexr n) i))
          (f2cl-lib:fdo (j (f2cl-lib:int-add i 1) (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf yy
                      (f2cl-lib:fref r-%data%
                                     (indexr)
                                     ((1
                                       (f2cl-lib:f2cl/
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        2)))
                                     r-%offset%))
              (setf ww
                      (f2cl-lib:fref r-%data%
                                     (indxr2)
                                     ((1
                                       (f2cl-lib:f2cl/
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add n
                                                                            1))
                                        2)))
                                     r-%offset%))
              (setf (f2cl-lib:fref r-%data%
                                   (indexr)
                                   ((1
                                     (f2cl-lib:f2cl/
                                      (f2cl-lib:int-mul n
                                                        (f2cl-lib:int-add n 1))
                                      2)))
                                   r-%offset%)
                      (- (* c yy) (* ss ww)))
              (setf (f2cl-lib:fref r-%data%
                                   (indxr2)
                                   ((1
                                     (f2cl-lib:f2cl/
                                      (f2cl-lib:int-mul n
                                                        (f2cl-lib:int-add n 1))
                                      2)))
                                   r-%offset%)
                      (+ (* ss yy) (* c ww)))
              (setf indexr (f2cl-lib:int-add indexr 1))
              (setf indxr2 (f2cl-lib:int-add indxr2 1))
             label100))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf yy
                      (f2cl-lib:fref qt-%data%
                                     (i j)
                                     ((1 n) (1 n))
                                     qt-%offset%))
              (setf ww
                      (f2cl-lib:fref qt-%data%
                                     ((f2cl-lib:int-add i 1) j)
                                     ((1 n) (1 n))
                                     qt-%offset%))
              (setf (f2cl-lib:fref qt-%data% (i j) ((1 n) (1 n)) qt-%offset%)
                      (- (* c yy) (* ss ww)))
              (setf (f2cl-lib:fref qt-%data%
                                   ((f2cl-lib:int-add i 1) j)
                                   ((1 n) (1 n))
                                   qt-%offset%)
                      (+ (* ss yy) (* c ww)))
             label110))
         label120))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::r1upqf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::daxpy fortran-to-lisp::dnrm2))))

