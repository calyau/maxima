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


(defun dcpose (ndim n qr alpha pivot ierr y sum)
  (declare (type (array double-float (*)) sum y)
           (type (array f2cl-lib:integer4 (*)) pivot)
           (type (array double-float (*)) alpha qr)
           (type (f2cl-lib:integer4) ierr n ndim))
  (f2cl-lib:with-multi-array-data
      ((qr double-float qr-%data% qr-%offset%)
       (alpha double-float alpha-%data% alpha-%offset%)
       (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
       (y double-float y-%data% y-%offset%)
       (sum double-float sum-%data% sum-%offset%))
    (prog ((beta 0.0) (sigma 0.0) (alphak 0.0) (qrkk 0.0) (i 0) (j 0) (jbar 0)
           (k 0) (kp1 0) (np1 0))
      (declare (type (f2cl-lib:integer4) np1 kp1 k jbar j i)
               (type (double-float) qrkk alphak sigma beta))
      (setf ierr 0)
      (setf np1 (f2cl-lib:int-add n 1))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j np1) nil)
        (tagbody
          (setf (f2cl-lib:fref sum-%data% (j) ((1 1)) sum-%offset%)
                  (ddot n
                   (f2cl-lib:array-slice qr-%data%
                                         double-float
                                         (1 j)
                                         ((1 ndim) (1 1))
                                         qr-%offset%)
                   1
                   (f2cl-lib:array-slice qr-%data%
                                         double-float
                                         (1 j)
                                         ((1 ndim) (1 1))
                                         qr-%offset%)
                   1))
         label20
          (setf (f2cl-lib:fref pivot-%data% (j) ((1 1)) pivot-%offset%) j)))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf sigma (f2cl-lib:fref sum-%data% (k) ((1 1)) sum-%offset%))
          (setf jbar k)
          (setf kp1 (f2cl-lib:int-add k 1))
          (f2cl-lib:fdo (j kp1 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
              (if
               (>= sigma (f2cl-lib:fref sum-%data% (j) ((1 1)) sum-%offset%))
               (go label40))
              (setf sigma (f2cl-lib:fref sum-%data% (j) ((1 1)) sum-%offset%))
              (setf jbar j)
             label40))
          (if (= jbar k) (go label70))
          (setf i (f2cl-lib:fref pivot-%data% (k) ((1 1)) pivot-%offset%))
          (setf (f2cl-lib:fref pivot-%data% (k) ((1 1)) pivot-%offset%)
                  (f2cl-lib:fref pivot-%data% (jbar) ((1 1)) pivot-%offset%))
          (setf (f2cl-lib:fref pivot-%data% (jbar) ((1 1)) pivot-%offset%) i)
          (setf (f2cl-lib:fref sum-%data% (jbar) ((1 1)) sum-%offset%)
                  (f2cl-lib:fref sum-%data% (k) ((1 1)) sum-%offset%))
          (setf (f2cl-lib:fref sum-%data% (k) ((1 1)) sum-%offset%) sigma)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf sigma
                      (f2cl-lib:fref qr-%data%
                                     (i k)
                                     ((1 ndim) (1 1))
                                     qr-%offset%))
              (setf (f2cl-lib:fref qr-%data%
                                   (i k)
                                   ((1 ndim) (1 1))
                                   qr-%offset%)
                      (f2cl-lib:fref qr-%data%
                                     (i jbar)
                                     ((1 ndim) (1 1))
                                     qr-%offset%))
              (setf (f2cl-lib:fref qr-%data%
                                   (i jbar)
                                   ((1 ndim) (1 1))
                                   qr-%offset%)
                      sigma)
             label50))
         label70
          (setf sigma
                  (ddot (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1)
                   (f2cl-lib:array-slice qr-%data%
                                         double-float
                                         (k k)
                                         ((1 ndim) (1 1))
                                         qr-%offset%)
                   1
                   (f2cl-lib:array-slice qr-%data%
                                         double-float
                                         (k k)
                                         ((1 ndim) (1 1))
                                         qr-%offset%)
                   1))
          (if (/= sigma 0.0f0) (go label60))
          (setf ierr 1)
          (go end_label)
         label60
          (if (= k n) (go label500))
          (setf qrkk
                  (f2cl-lib:fref qr-%data% (k k) ((1 ndim) (1 1)) qr-%offset%))
          (setf alphak (- (f2cl-lib:fsqrt sigma)))
          (if (< qrkk 0.0f0) (setf alphak (- alphak)))
          (setf (f2cl-lib:fref alpha-%data% (k) ((1 n)) alpha-%offset%) alphak)
          (setf beta (/ 1.0f0 (- sigma (* qrkk alphak))))
          (setf (f2cl-lib:fref qr-%data% (k k) ((1 ndim) (1 1)) qr-%offset%)
                  (- qrkk alphak))
          (f2cl-lib:fdo (j kp1 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
             label80
              (setf (f2cl-lib:fref y-%data% (j) ((1 1)) y-%offset%)
                      (* beta
                         (ddot (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1)
                          (f2cl-lib:array-slice qr-%data%
                                                double-float
                                                (k k)
                                                ((1 ndim) (1 1))
                                                qr-%offset%)
                          1
                          (f2cl-lib:array-slice qr-%data%
                                                double-float
                                                (k j)
                                                ((1 ndim) (1 1))
                                                qr-%offset%)
                          1)))))
          (f2cl-lib:fdo (j kp1 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
              (f2cl-lib:fdo (i k (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                  (setf (f2cl-lib:fref qr-%data%
                                       (i j)
                                       ((1 ndim) (1 1))
                                       qr-%offset%)
                          (-
                           (f2cl-lib:fref qr-%data%
                                          (i j)
                                          ((1 ndim) (1 1))
                                          qr-%offset%)
                           (*
                            (f2cl-lib:fref qr-%data%
                                           (i k)
                                           ((1 ndim) (1 1))
                                           qr-%offset%)
                            (f2cl-lib:fref y-%data% (j) ((1 1)) y-%offset%))))
                 label90))
              (setf (f2cl-lib:fref sum-%data% (j) ((1 1)) sum-%offset%)
                      (- (f2cl-lib:fref sum-%data% (j) ((1 1)) sum-%offset%)
                         (expt
                          (f2cl-lib:fref qr-%data%
                                         (k j)
                                         ((1 ndim) (1 1))
                                         qr-%offset%)
                          2)))
             label100))
         label500))
      (setf (f2cl-lib:fref alpha-%data% (n) ((1 n)) alpha-%offset%)
              (f2cl-lib:fref qr-%data% (n n) ((1 ndim) (1 1)) qr-%offset%))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil ierr nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dcpose
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil fortran-to-lisp::ierr nil nil)
           :calls '(fortran-to-lisp::ddot))))

