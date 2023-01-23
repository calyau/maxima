;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2013-11 (20E Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(defun ddecbt (m n a b c ip ier)
  (declare (type (array f2cl-lib:integer4 (*)) ip)
           (type (array double-float (*)) c b a)
           (type (f2cl-lib:integer4) ier n m))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (b double-float b-%data% b-%offset%)
       (c double-float c-%data% c-%offset%)
       (ip f2cl-lib:integer4 ip-%data% ip-%offset%))
    (prog ((dp 0.0d0) (nm1 0) (nm2 0) (km1 0) (i 0) (j 0) (k 0))
      (declare (type (f2cl-lib:integer4) k j i km1 nm2 nm1)
               (type (double-float) dp))
      (if (or (< m 1) (< n 4)) (go label210))
      (setf nm1 (f2cl-lib:int-sub n 1))
      (setf nm2 (f2cl-lib:int-sub n 2))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (dgefa a m m ip ier)
        (declare (ignore var-0 var-1 var-2 var-3))
        (setf ier var-4))
      (setf k 1)
      (if (/= ier 0) (go label200))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j m) nil)
        (tagbody
          (dgesl a m m ip
           (f2cl-lib:array-slice b-%data%
                                 double-float
                                 (1 j 1)
                                 ((1 m) (1 m) (1 n))
                                 b-%offset%)
           0)
          (dgesl a m m ip
           (f2cl-lib:array-slice c-%data%
                                 double-float
                                 (1 j 1)
                                 ((1 m) (1 m) (1 n))
                                 c-%offset%)
           0)
         label10))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j m) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf dp
                      (ddot m
                       (f2cl-lib:array-slice c-%data%
                                             double-float
                                             (i 1 2)
                                             ((1 m) (1 m) (1 n))
                                             c-%offset%)
                       m
                       (f2cl-lib:array-slice c-%data%
                                             double-float
                                             (1 j 1)
                                             ((1 m) (1 m) (1 n))
                                             c-%offset%)
                       1))
              (setf (f2cl-lib:fref b-%data%
                                   (i j 2)
                                   ((1 m) (1 m) (1 n))
                                   b-%offset%)
                      (-
                       (f2cl-lib:fref b-%data%
                                      (i j 2)
                                      ((1 m) (1 m) (1 n))
                                      b-%offset%)
                       dp))
             label30))
         label40))
      (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                    ((> k nm1) nil)
        (tagbody
          (setf km1 (f2cl-lib:int-sub k 1))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j m) nil)
            (tagbody
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i m) nil)
                (tagbody
                  (setf dp
                          (ddot m
                           (f2cl-lib:array-slice c-%data%
                                                 double-float
                                                 (i 1 k)
                                                 ((1 m) (1 m) (1 n))
                                                 c-%offset%)
                           m
                           (f2cl-lib:array-slice b-%data%
                                                 double-float
                                                 (1 j km1)
                                                 ((1 m) (1 m) (1 n))
                                                 b-%offset%)
                           1))
                  (setf (f2cl-lib:fref a-%data%
                                       (i j k)
                                       ((1 m) (1 m) (1 n))
                                       a-%offset%)
                          (-
                           (f2cl-lib:fref a-%data%
                                          (i j k)
                                          ((1 m) (1 m) (1 n))
                                          a-%offset%)
                           dp))
                 label60))
             label70))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dgefa
               (f2cl-lib:array-slice a-%data%
                                     double-float
                                     (1 1 k)
                                     ((1 m) (1 m) (1 n))
                                     a-%offset%)
               m m
               (f2cl-lib:array-slice ip-%data%
                                     f2cl-lib:integer4
                                     (1 k)
                                     ((1 m) (1 n))
                                     ip-%offset%)
               ier)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf ier var-4))
          (if (/= ier 0) (go label200))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j m) nil)
            (tagbody
             label80
              (dgesl
               (f2cl-lib:array-slice a-%data%
                                     double-float
                                     (1 1 k)
                                     ((1 m) (1 m) (1 n))
                                     a-%offset%)
               m m
               (f2cl-lib:array-slice ip-%data%
                                     f2cl-lib:integer4
                                     (1 k)
                                     ((1 m) (1 n))
                                     ip-%offset%)
               (f2cl-lib:array-slice b-%data%
                                     double-float
                                     (1 j k)
                                     ((1 m) (1 m) (1 n))
                                     b-%offset%)
               0)))
         label100))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j m) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf dp
                      (ddot m
                       (f2cl-lib:array-slice b-%data%
                                             double-float
                                             (i 1 n)
                                             ((1 m) (1 m) (1 n))
                                             b-%offset%)
                       m
                       (f2cl-lib:array-slice b-%data%
                                             double-float
                                             (1 j nm2)
                                             ((1 m) (1 m) (1 n))
                                             b-%offset%)
                       1))
              (setf (f2cl-lib:fref c-%data%
                                   (i j n)
                                   ((1 m) (1 m) (1 n))
                                   c-%offset%)
                      (-
                       (f2cl-lib:fref c-%data%
                                      (i j n)
                                      ((1 m) (1 m) (1 n))
                                      c-%offset%)
                       dp))
             label120))
         label130))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j m) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf dp
                      (ddot m
                       (f2cl-lib:array-slice c-%data%
                                             double-float
                                             (i 1 n)
                                             ((1 m) (1 m) (1 n))
                                             c-%offset%)
                       m
                       (f2cl-lib:array-slice b-%data%
                                             double-float
                                             (1 j nm1)
                                             ((1 m) (1 m) (1 n))
                                             b-%offset%)
                       1))
              (setf (f2cl-lib:fref a-%data%
                                   (i j n)
                                   ((1 m) (1 m) (1 n))
                                   a-%offset%)
                      (-
                       (f2cl-lib:fref a-%data%
                                      (i j n)
                                      ((1 m) (1 m) (1 n))
                                      a-%offset%)
                       dp))
             label150))
         label160))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (dgefa
           (f2cl-lib:array-slice a-%data%
                                 double-float
                                 (1 1 n)
                                 ((1 m) (1 m) (1 n))
                                 a-%offset%)
           m m
           (f2cl-lib:array-slice ip-%data%
                                 f2cl-lib:integer4
                                 (1 n)
                                 ((1 m) (1 n))
                                 ip-%offset%)
           ier)
        (declare (ignore var-0 var-1 var-2 var-3))
        (setf ier var-4))
      (setf k n)
      (if (/= ier 0) (go label200))
      (go end_label)
     label200
      (setf ier k)
      (go end_label)
     label210
      (setf ier -1)
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::ddecbt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::ddot fortran-to-lisp::dgesl
                    fortran-to-lisp::dgefa))))

