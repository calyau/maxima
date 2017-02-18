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


(defun dsolbt (m n a b c y ip)
  (declare (type (array f2cl-lib:integer4 (*)) ip)
           (type (array double-float (*)) y c b a)
           (type (f2cl-lib:integer4) n m))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (b double-float b-%data% b-%offset%)
       (c double-float c-%data% c-%offset%)
       (y double-float y-%data% y-%offset%)
       (ip f2cl-lib:integer4 ip-%data% ip-%offset%))
    (prog ((dp 0.0d0) (nm1 0) (nm2 0) (i 0) (k 0) (kb 0) (km1 0) (kp1 0))
      (declare (type (f2cl-lib:integer4) kp1 km1 kb k i nm2 nm1)
               (type (double-float) dp))
      (setf nm1 (f2cl-lib:int-sub n 1))
      (setf nm2 (f2cl-lib:int-sub n 2))
      (dgesl a m m ip y 0)
      (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                    ((> k nm1) nil)
        (tagbody
          (setf km1 (f2cl-lib:int-sub k 1))
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
                       (f2cl-lib:array-slice y-%data%
                                             double-float
                                             (1 km1)
                                             ((1 m) (1 n))
                                             y-%offset%)
                       1))
              (setf (f2cl-lib:fref y-%data% (i k) ((1 m) (1 n)) y-%offset%)
                      (-
                       (f2cl-lib:fref y-%data% (i k) ((1 m) (1 n)) y-%offset%)
                       dp))
             label20))
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
           (f2cl-lib:array-slice y-%data%
                                 double-float
                                 (1 k)
                                 ((1 m) (1 n))
                                 y-%offset%)
           0)
         label30))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf dp
                  (+
                   (ddot m
                    (f2cl-lib:array-slice c-%data%
                                          double-float
                                          (i 1 n)
                                          ((1 m) (1 m) (1 n))
                                          c-%offset%)
                    m
                    (f2cl-lib:array-slice y-%data%
                                          double-float
                                          (1 nm1)
                                          ((1 m) (1 n))
                                          y-%offset%)
                    1)
                   (ddot m
                    (f2cl-lib:array-slice b-%data%
                                          double-float
                                          (i 1 n)
                                          ((1 m) (1 m) (1 n))
                                          b-%offset%)
                    m
                    (f2cl-lib:array-slice y-%data%
                                          double-float
                                          (1 nm2)
                                          ((1 m) (1 n))
                                          y-%offset%)
                    1)))
          (setf (f2cl-lib:fref y-%data% (i n) ((1 m) (1 n)) y-%offset%)
                  (- (f2cl-lib:fref y-%data% (i n) ((1 m) (1 n)) y-%offset%)
                     dp))
         label50))
      (dgesl
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
       (f2cl-lib:array-slice y-%data%
                             double-float
                             (1 n)
                             ((1 m) (1 n))
                             y-%offset%)
       0)
      (f2cl-lib:fdo (kb 1 (f2cl-lib:int-add kb 1))
                    ((> kb nm1) nil)
        (tagbody
          (setf k (f2cl-lib:int-sub n kb))
          (setf kp1 (f2cl-lib:int-add k 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf dp
                      (ddot m
                       (f2cl-lib:array-slice b-%data%
                                             double-float
                                             (i 1 k)
                                             ((1 m) (1 m) (1 n))
                                             b-%offset%)
                       m
                       (f2cl-lib:array-slice y-%data%
                                             double-float
                                             (1 kp1)
                                             ((1 m) (1 n))
                                             y-%offset%)
                       1))
              (setf (f2cl-lib:fref y-%data% (i k) ((1 m) (1 n)) y-%offset%)
                      (-
                       (f2cl-lib:fref y-%data% (i k) ((1 m) (1 n)) y-%offset%)
                       dp))
             label70))
         label80))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf dp
                  (ddot m
                   (f2cl-lib:array-slice c-%data%
                                         double-float
                                         (i 1 1)
                                         ((1 m) (1 m) (1 n))
                                         c-%offset%)
                   m
                   (f2cl-lib:array-slice y-%data%
                                         double-float
                                         (1 3)
                                         ((1 m) (1 n))
                                         y-%offset%)
                   1))
          (setf (f2cl-lib:fref y-%data% (i 1) ((1 m) (1 n)) y-%offset%)
                  (- (f2cl-lib:fref y-%data% (i 1) ((1 m) (1 n)) y-%offset%)
                     dp))
         label100))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dsolbt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::ddot fortran-to-lisp::dgesl))))

