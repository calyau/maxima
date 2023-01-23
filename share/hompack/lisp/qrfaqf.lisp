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


(defun qrfaqf (qt r n iflag)
  (declare (type (f2cl-lib:integer4) iflag n)
           (type (array double-float (*)) r qt))
  (f2cl-lib:with-multi-array-data
      ((qt double-float qt-%data% qt-%offset%)
       (r double-float r-%data% r-%offset%))
    (prog ((i 0) (j 0) (k 0) (indexr 0) (f2cl-lib:isign 0) (one 0.0) (tau 0.0)
           (temp 0.0))
      (declare (type (double-float) temp tau one)
               (type (f2cl-lib:integer4) f2cl-lib:isign indexr k j i))
      (setf one (coerce 1.0f0 'double-float))
      (setf indexr 1)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k (f2cl-lib:int-add n (f2cl-lib:int-sub 1))) nil)
        (tagbody
          (setf temp
                  (dnrm2 (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1)
                   (f2cl-lib:array-slice qt-%data%
                                         double-float
                                         (k k)
                                         ((1 n) (1 n))
                                         qt-%offset%)
                   1))
          (cond
            ((= temp 0.0f0)
             (setf iflag 4)
             (go end_label))
            (t
             (setf f2cl-lib:isign
                     (f2cl-lib:int
                      (f2cl-lib:sign one
                                     (f2cl-lib:fref qt-%data%
                                                    (k k)
                                                    ((1 n) (1 n))
                                                    qt-%offset%))))
             (setf (f2cl-lib:fref r-%data% (indexr) ((1 n)) r-%offset%)
                     (* (f2cl-lib:int-sub f2cl-lib:isign) temp))
             (setf temp (/ f2cl-lib:isign temp))
             (dscal (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1) temp
              (f2cl-lib:array-slice qt-%data%
                                    double-float
                                    (k k)
                                    ((1 n) (1 n))
                                    qt-%offset%)
              1)
             (setf (f2cl-lib:fref qt-%data% (k k) ((1 n) (1 n)) qt-%offset%)
                     (+
                      (f2cl-lib:fref qt-%data% (k k) ((1 n) (1 n)) qt-%offset%)
                      1.0f0))
             (setf indexr (f2cl-lib:int-add indexr 1))
             (f2cl-lib:fdo (j (f2cl-lib:int-add k 1) (f2cl-lib:int-add j 1))
                           ((> j n) nil)
               (tagbody
                 (setf tau
                         (/
                          (ddot (f2cl-lib:int-add (f2cl-lib:int-sub n k) 1)
                           (f2cl-lib:array-slice qt-%data%
                                                 double-float
                                                 (k k)
                                                 ((1 n) (1 n))
                                                 qt-%offset%)
                           1
                           (f2cl-lib:array-slice qt-%data%
                                                 double-float
                                                 (k j)
                                                 ((1 n) (1 n))
                                                 qt-%offset%)
                           1)
                          (f2cl-lib:fref qt-%data%
                                         (k k)
                                         ((1 n) (1 n))
                                         qt-%offset%)))
                 (setf (f2cl-lib:fref r-%data% (indexr) ((1 n)) r-%offset%)
                         (-
                          (f2cl-lib:fref qt-%data%
                                         (k j)
                                         ((1 n) (1 n))
                                         qt-%offset%)
                          (* tau
                             (f2cl-lib:fref qt-%data%
                                            (k k)
                                            ((1 n) (1 n))
                                            qt-%offset%))))
                 (setf indexr (f2cl-lib:int-add indexr 1))
                 (daxpy (f2cl-lib:int-sub n k) (- tau)
                  (f2cl-lib:array-slice qt-%data%
                                        double-float
                                        ((+ k 1) k)
                                        ((1 n) (1 n))
                                        qt-%offset%)
                  1
                  (f2cl-lib:array-slice qt-%data%
                                        double-float
                                        ((+ k 1) j)
                                        ((1 n) (1 n))
                                        qt-%offset%)
                  1)
                label10))))
         label20))
      (cond
        ((= (f2cl-lib:fref qt (n n) ((1 n) (1 n))) 0.0f0)
         (setf iflag 4)
         (go end_label)))
      (setf (f2cl-lib:fref r-%data% (indexr) ((1 n)) r-%offset%)
              (f2cl-lib:fref qt-%data% (n n) ((1 n) (1 n)) qt-%offset%))
      (setf (f2cl-lib:fref qt-%data% (n n) ((1 n) (1 n)) qt-%offset%)
              (coerce 1.0f0 'double-float))
      (f2cl-lib:fdo (k (f2cl-lib:int-add n (f2cl-lib:int-sub 1))
                     (f2cl-lib:int-add k (f2cl-lib:int-sub 1)))
                    ((> k 1) nil)
        (tagbody
          (setf temp (f2cl-lib:fref qt-%data% (k k) ((1 n) (1 n)) qt-%offset%))
          (setf (f2cl-lib:fref qt-%data% (k k) ((1 n) (1 n)) qt-%offset%)
                  (- 1.0f0
                     (f2cl-lib:fref qt-%data%
                                    (k k)
                                    ((1 n) (1 n))
                                    qt-%offset%)))
          (dcopy (f2cl-lib:int-sub n k)
           (f2cl-lib:array-slice qt-%data%
                                 double-float
                                 ((+ k 1) k)
                                 ((1 n) (1 n))
                                 qt-%offset%)
           1
           (f2cl-lib:array-slice qt-%data%
                                 double-float
                                 (k (f2cl-lib:int-add k 1))
                                 ((1 n) (1 n))
                                 qt-%offset%)
           n)
          (dscal (f2cl-lib:int-sub n k) (- one)
           (f2cl-lib:array-slice qt-%data%
                                 double-float
                                 (k (f2cl-lib:int-add k 1))
                                 ((1 n) (1 n))
                                 qt-%offset%)
           n)
          (f2cl-lib:fdo (i n (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                        ((> i (f2cl-lib:int-add k 1)) nil)
            (tagbody
              (setf tau
                      (-
                       (ddot (f2cl-lib:int-sub n k)
                        (f2cl-lib:array-slice qt-%data%
                                              double-float
                                              (i (f2cl-lib:int-add k 1))
                                              ((1 n) (1 n))
                                              qt-%offset%)
                        n
                        (f2cl-lib:array-slice qt-%data%
                                              double-float
                                              (k (f2cl-lib:int-add k 1))
                                              ((1 n) (1 n))
                                              qt-%offset%)
                        n)))
              (setf (f2cl-lib:fref qt-%data% (i k) ((1 n) (1 n)) qt-%offset%)
                      (- tau))
              (setf tau (/ tau temp))
              (daxpy (f2cl-lib:int-sub n k) tau
               (f2cl-lib:array-slice qt-%data%
                                     double-float
                                     (k (f2cl-lib:int-add k 1))
                                     ((1 n) (1 n))
                                     qt-%offset%)
               n
               (f2cl-lib:array-slice qt-%data%
                                     double-float
                                     (i (f2cl-lib:int-add k 1))
                                     ((1 n) (1 n))
                                     qt-%offset%)
               n)
             label30))
         label40))
      (go end_label)
     end_label
      (return (values nil nil nil iflag)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::qrfaqf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::iflag)
           :calls '(fortran-to-lisp::dcopy fortran-to-lisp::daxpy
                    fortran-to-lisp::ddot fortran-to-lisp::dscal
                    fortran-to-lisp::dnrm2))))

