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


(defun pcgds (nn aa lenaa maxa pp start work iflag)
  (declare (type (array f2cl-lib:integer4 (*)) maxa)
           (type (array double-float (*)) work start pp aa)
           (type (f2cl-lib:integer4) iflag lenaa nn))
  (f2cl-lib:with-multi-array-data
      ((aa double-float aa-%data% aa-%offset%)
       (pp double-float pp-%data% pp-%offset%)
       (start double-float start-%data% start-%offset%)
       (work double-float work-%data% work-%offset%)
       (maxa f2cl-lib:integer4 maxa-%data% maxa-%offset%))
    (prog ((stillu nil) (stillb nil) (ab 0.0) (au 0.0) (bb 0.0) (bu 0.0)
           (dznrm 0.0) (pbnprd 0.0) (punprd 0.0) (rbnprd 0.0) (rbtol 0.0)
           (rnprd 0.0) (runprd 0.0) (rutol 0.0) (startk 0.0) (temp 0.0)
           (unrm 0.0) (zlen 0.0) (ztol 0.0) (imax 0) (ind 0) (j 0) (k 0)
           (np1 0) (np2 0) (n2p3 0) (n3p4 0) (n4p5 0) (n5p6 0))
      (declare (type (f2cl-lib:integer4) n5p6 n4p5 n3p4 n2p3 np2 np1 k j ind
                                         imax)
               (type (double-float) ztol zlen unrm temp startk rutol runprd
                                    rnprd rbtol rbnprd punprd pbnprd dznrm bu
                                    bb au ab)
               (type f2cl-lib:logical stillb stillu))
      (setf np1 (f2cl-lib:int-add nn 1))
      (setf np2 (f2cl-lib:int-add nn 2))
      (setf n2p3 (f2cl-lib:int-add (f2cl-lib:int-mul 2 nn) 3))
      (setf n3p4 (f2cl-lib:int-add (f2cl-lib:int-mul 3 nn) 4))
      (setf n4p5 (f2cl-lib:int-add (f2cl-lib:int-mul 4 nn) 5))
      (setf n5p6 (f2cl-lib:int-add (f2cl-lib:int-mul 5 nn) 6))
      (setf k (idamax np1 start 1))
      (setf startk
              (f2cl-lib:fref start-%data%
                             (k)
                             ((1 (f2cl-lib:int-add nn 1)))
                             start-%offset%))
      (dcopy lenaa aa 1
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n5p6)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       1)
      (setf (f2cl-lib:fref maxa-%data%
                           ((f2cl-lib:int-add nn 1))
                           ((1 (f2cl-lib:int-add nn 2)))
                           maxa-%offset%)
              (f2cl-lib:int-add lenaa 1))
      (setf (f2cl-lib:fref maxa-%data%
                           ((f2cl-lib:int-add nn 2))
                           ((1 (f2cl-lib:int-add nn 2)))
                           maxa-%offset%)
              (f2cl-lib:int-sub (f2cl-lib:int-add lenaa nn 3) k))
      (mfacds nn
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n5p6)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       lenaa maxa)
      (dcopy nn pp 1 work 1)
      (if (< k np1)
          (setf (f2cl-lib:fref work-%data%
                               (k)
                               ((1
                                 (f2cl-lib:int-add
                                  (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                  lenaa
                                  nn
                                  1)))
                               work-%offset%)
                  (+
                   (f2cl-lib:fref work-%data%
                                  (k)
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%)
                   1.0)))
      (setf unrm (dnrm2 nn work 1))
      (setf imax (f2cl-lib:int-mul 10 np1))
      (setf stillu f2cl-lib:%true%)
      (setf stillb f2cl-lib:%true%)
      (setf ztol (* 100.0f0 (f2cl-lib:d1mach 4)))
      (setf rbtol (* ztol (abs startk)))
      (setf rutol (* ztol unrm))
      (multds
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       aa
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (np2)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       maxa nn lenaa)
      (setf (f2cl-lib:fref work-%data%
                           ((f2cl-lib:int-add n3p4 nn))
                           ((1
                             (f2cl-lib:int-add
                              (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                              lenaa
                              nn
                              1)))
                           work-%offset%)
              (f2cl-lib:fref work-%data%
                             ((f2cl-lib:int-sub (f2cl-lib:int-add np2 k) 1))
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%))
      (setf ind (f2cl-lib:int-sub (f2cl-lib:int-add n3p4 k) 1))
      (if (< k np1)
          (setf (f2cl-lib:fref work-%data%
                               (ind)
                               ((1
                                 (f2cl-lib:int-add
                                  (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                  lenaa
                                  nn
                                  1)))
                               work-%offset%)
                  (+
                   (f2cl-lib:fref work-%data%
                                  (ind)
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%)
                   (f2cl-lib:fref work-%data%
                                  ((f2cl-lib:int-add np2 nn))
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%))))
      (dscal np1 -1.0
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       1)
      (daxpy nn -1.0 pp 1
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       1)
      (if (< k np1)
          (setf (f2cl-lib:fref work-%data%
                               (ind)
                               ((1
                                 (f2cl-lib:int-add
                                  (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                  lenaa
                                  nn
                                  1)))
                               work-%offset%)
                  (-
                   (f2cl-lib:fref work-%data%
                                  (ind)
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%)
                   1.0)))
      (qimuds
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n5p6)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       maxa nn lenaa)
      (dcopy np1
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       1 work 1)
      (qimuds
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n5p6)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       work maxa nn lenaa)
      (multds
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n4p5)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       aa work maxa nn lenaa)
      (setf (f2cl-lib:fref work-%data%
                           ((f2cl-lib:int-add n4p5 nn))
                           ((1
                             (f2cl-lib:int-add
                              (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                              lenaa
                              nn
                              1)))
                           work-%offset%)
              (f2cl-lib:fref work-%data%
                             (k)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%))
      (if (< k np1)
          (setf (f2cl-lib:fref work-%data%
                               ((f2cl-lib:int-sub (f2cl-lib:int-add n4p5 k) 1))
                               ((1
                                 (f2cl-lib:int-add
                                  (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                  lenaa
                                  nn
                                  1)))
                               work-%offset%)
                  (+
                   (f2cl-lib:fref work-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add n4p5 k)
                                                     1))
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%)
                   (f2cl-lib:fref work-%data%
                                  (np1)
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%))))
      (setf runprd
              (ddot np1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (n3p4)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
               1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (n3p4)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
               1))
      (setf punprd
              (ddot np1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (n4p5)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
               1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (n4p5)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
               1))
      (setf j 1)
     label100
      (if (not (and stillu (<= j imax))) (go label200))
      (cond
        ((> (f2cl-lib:fsqrt runprd) rutol)
         (cond
           ((= punprd 0.0f0)
            (multds
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n3p4)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             aa
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (np2)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             maxa nn lenaa)
            (setf (f2cl-lib:fref work-%data%
                                 ((f2cl-lib:int-add n3p4 nn))
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 5
                                                      (f2cl-lib:int-add nn 1))
                                    lenaa
                                    nn
                                    1)))
                                 work-%offset%)
                    (f2cl-lib:fref work-%data%
                                   ((f2cl-lib:int-sub (f2cl-lib:int-add np2 k)
                                                      1))
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%))
            (setf ind (f2cl-lib:int-sub (f2cl-lib:int-add n3p4 k) 1))
            (if (< k np1)
                (setf (f2cl-lib:fref work-%data%
                                     (ind)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
                        (+
                         (f2cl-lib:fref work-%data%
                                        (ind)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                         (f2cl-lib:fref work-%data%
                                        ((f2cl-lib:int-add np2 nn))
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%))))
            (dscal np1 -1.0
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n3p4)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1)
            (daxpy nn -1.0 pp 1
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n3p4)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1)
            (if (< k np1)
                (setf (f2cl-lib:fref work-%data%
                                     ((f2cl-lib:int-sub
                                       (f2cl-lib:int-add n3p4 k)
                                       1))
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
                        (-
                         (f2cl-lib:fref work-%data%
                                        ((f2cl-lib:int-sub
                                          (f2cl-lib:int-add n3p4 k)
                                          1))
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                         1.0)))
            (qimuds
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n5p6)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n3p4)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             maxa nn lenaa)
            (dcopy np1
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n3p4)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1 work 1)
            (qimuds
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n5p6)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             work maxa nn lenaa)
            (multds
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n4p5)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             aa work maxa nn lenaa)
            (setf (f2cl-lib:fref work-%data%
                                 ((f2cl-lib:int-add n4p5 nn))
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 5
                                                      (f2cl-lib:int-add nn 1))
                                    lenaa
                                    nn
                                    1)))
                                 work-%offset%)
                    (f2cl-lib:fref work-%data%
                                   (k)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%))
            (setf ind (f2cl-lib:int-sub (f2cl-lib:int-add n4p5 k) 1))
            (if (< k np1)
                (setf (f2cl-lib:fref work-%data%
                                     (ind)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
                        (+
                         (f2cl-lib:fref work-%data%
                                        (ind)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                         (f2cl-lib:fref work-%data%
                                        (np1)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%))))
            (setf runprd
                    (ddot np1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (n3p4)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (n3p4)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1))
            (setf punprd
                    (ddot np1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (n4p5)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (n4p5)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1))
            (cond
              ((<= (f2cl-lib:fsqrt runprd) rutol)
               (setf stillu f2cl-lib:%false%)))))
         (cond
           (stillu
            (setf au (/ runprd punprd))
            (dcopy np1
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (np2)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1 work 1)
            (daxpy np1 au
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n4p5)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (np2)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1)
            (daxpy np1 -1.0
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (np2)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1 work 1)
            (setf zlen
                    (dnrm2 np1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (np2)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1))
            (setf dznrm (dnrm2 np1 work 1))
            (if (< (/ dznrm zlen) ztol) (setf stillu f2cl-lib:%false%)))))
        (t
         (setf stillu f2cl-lib:%false%)))
      (cond
        (stillu
         (multds work aa
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n4p5)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          maxa nn lenaa)
         (setf (f2cl-lib:fref work-%data%
                              (np1)
                              ((1
                                (f2cl-lib:int-add
                                 (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                 lenaa
                                 nn
                                 1)))
                              work-%offset%)
                 (f2cl-lib:fref work-%data%
                                ((f2cl-lib:int-sub (f2cl-lib:int-add n4p5 k)
                                                   1))
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%))
         (if (< k np1)
             (setf (f2cl-lib:fref work-%data%
                                  (k)
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%)
                     (+
                      (f2cl-lib:fref work-%data%
                                     (k)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
                      (f2cl-lib:fref work-%data%
                                     ((f2cl-lib:int-add n4p5 nn))
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%))))
         (qimuds
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n5p6)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          work maxa nn lenaa)
         (daxpy np1 (- au) work 1
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n3p4)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          1)
         (setf rnprd
                 (ddot np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (n3p4)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                  1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (n3p4)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                  1))
         (setf bu (/ rnprd runprd))
         (setf runprd rnprd)
         (dcopy np1
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n3p4)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          1 work 1)
         (qimuds
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n5p6)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          work maxa nn lenaa)
         (multds start aa work maxa nn lenaa)
         (setf (f2cl-lib:fref start-%data%
                              (np1)
                              ((1 (f2cl-lib:int-add nn 1)))
                              start-%offset%)
                 (f2cl-lib:fref work-%data%
                                (k)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%))
         (if (< k np1)
             (setf (f2cl-lib:fref start-%data%
                                  (k)
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  start-%offset%)
                     (+
                      (f2cl-lib:fref start-%data%
                                     (k)
                                     ((1 (f2cl-lib:int-add nn 1)))
                                     start-%offset%)
                      (f2cl-lib:fref work-%data%
                                     (np1)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%))))
         (daxpy np1 bu
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n4p5)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          1 start 1)
         (dcopy np1 start 1
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n4p5)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          1)
         (setf punprd
                 (ddot np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (n4p5)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                  1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (n4p5)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                  1))))
      (setf j (f2cl-lib:int-add j 1))
      (go label100)
     label200
      (cond
        ((> j imax)
         (setf iflag 4)
         (go end_label)))
      (multds
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       aa
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n2p3)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       maxa nn lenaa)
      (setf (f2cl-lib:fref work-%data%
                           ((f2cl-lib:int-add n3p4 nn))
                           ((1
                             (f2cl-lib:int-add
                              (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                              lenaa
                              nn
                              1)))
                           work-%offset%)
              (f2cl-lib:fref work-%data%
                             ((f2cl-lib:int-sub (f2cl-lib:int-add n2p3 k) 1))
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%))
      (setf ind (f2cl-lib:int-sub (f2cl-lib:int-add n3p4 k) 1))
      (if (< k np1)
          (setf (f2cl-lib:fref work-%data%
                               (ind)
                               ((1
                                 (f2cl-lib:int-add
                                  (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                  lenaa
                                  nn
                                  1)))
                               work-%offset%)
                  (+
                   (f2cl-lib:fref work-%data%
                                  (ind)
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%)
                   (f2cl-lib:fref work-%data%
                                  ((f2cl-lib:int-add n2p3 nn))
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%))))
      (dscal np1 -1.0
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       1)
      (setf (f2cl-lib:fref work-%data%
                           ((f2cl-lib:int-add n3p4 nn))
                           ((1
                             (f2cl-lib:int-add
                              (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                              lenaa
                              nn
                              1)))
                           work-%offset%)
              (+ startk
                 (f2cl-lib:fref work-%data%
                                ((f2cl-lib:int-add n3p4 nn))
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)))
      (qimuds
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n5p6)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       maxa nn lenaa)
      (dcopy np1
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n3p4)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       1 work 1)
      (qimuds
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n5p6)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       work maxa nn lenaa)
      (multds
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n4p5)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       aa work maxa nn lenaa)
      (setf (f2cl-lib:fref work-%data%
                           ((f2cl-lib:int-add n4p5 nn))
                           ((1
                             (f2cl-lib:int-add
                              (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                              lenaa
                              nn
                              1)))
                           work-%offset%)
              (f2cl-lib:fref work-%data%
                             (k)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%))
      (if (< k np1)
          (setf (f2cl-lib:fref work-%data%
                               ((f2cl-lib:int-sub (f2cl-lib:int-add n4p5 k) 1))
                               ((1
                                 (f2cl-lib:int-add
                                  (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                  lenaa
                                  nn
                                  1)))
                               work-%offset%)
                  (+
                   (f2cl-lib:fref work-%data%
                                  ((f2cl-lib:int-sub (f2cl-lib:int-add n4p5 k)
                                                     1))
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%)
                   (f2cl-lib:fref work-%data%
                                  (np1)
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%))))
      (setf rbnprd
              (ddot np1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (n3p4)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
               1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (n3p4)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
               1))
      (setf pbnprd
              (ddot np1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (n4p5)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
               1
               (f2cl-lib:array-slice work-%data%
                                     double-float
                                     (n4p5)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
               1))
      (setf j 1)
     label300
      (if (not (and stillb (<= j imax))) (go label400))
      (cond
        ((> (f2cl-lib:fsqrt rbnprd) rbtol)
         (cond
           ((= pbnprd 0.0f0)
            (multds
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n3p4)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             aa
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n2p3)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             maxa nn lenaa)
            (setf (f2cl-lib:fref work-%data%
                                 ((f2cl-lib:int-add n3p4 nn))
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 5
                                                      (f2cl-lib:int-add nn 1))
                                    lenaa
                                    nn
                                    1)))
                                 work-%offset%)
                    (f2cl-lib:fref work-%data%
                                   ((f2cl-lib:int-sub (f2cl-lib:int-add n2p3 k)
                                                      1))
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%))
            (setf ind (f2cl-lib:int-sub (f2cl-lib:int-add n3p4 k) 1))
            (if (< k np1)
                (setf (f2cl-lib:fref work-%data%
                                     (ind)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
                        (+
                         (f2cl-lib:fref work-%data%
                                        (ind)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                         (f2cl-lib:fref work-%data%
                                        ((f2cl-lib:int-add n2p3 nn))
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%))))
            (dscal np1 -1.0
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n3p4)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1)
            (setf (f2cl-lib:fref work-%data%
                                 ((f2cl-lib:int-add n3p4 nn))
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 5
                                                      (f2cl-lib:int-add nn 1))
                                    lenaa
                                    nn
                                    1)))
                                 work-%offset%)
                    (+ startk
                       (f2cl-lib:fref work-%data%
                                      ((f2cl-lib:int-add n3p4 nn))
                                      ((1
                                        (f2cl-lib:int-add
                                         (f2cl-lib:int-mul 5
                                                           (f2cl-lib:int-add nn
                                                                             1))
                                         lenaa
                                         nn
                                         1)))
                                      work-%offset%)))
            (qimuds
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n5p6)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n3p4)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             maxa nn lenaa)
            (dcopy np1
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n3p4)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1 work 1)
            (qimuds
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n5p6)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             work maxa nn lenaa)
            (multds
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n4p5)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             aa work maxa nn lenaa)
            (setf (f2cl-lib:fref work-%data%
                                 ((f2cl-lib:int-add n4p5 nn))
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 5
                                                      (f2cl-lib:int-add nn 1))
                                    lenaa
                                    nn
                                    1)))
                                 work-%offset%)
                    (f2cl-lib:fref work-%data%
                                   (k)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%))
            (setf ind (f2cl-lib:int-sub (f2cl-lib:int-add n4p5 k) 1))
            (if (< k np1)
                (setf (f2cl-lib:fref work-%data%
                                     (ind)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
                        (+
                         (f2cl-lib:fref work-%data%
                                        (ind)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                         (f2cl-lib:fref work-%data%
                                        (np1)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%))))
            (setf rbnprd
                    (ddot np1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (n3p4)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (n3p4)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1))
            (setf pbnprd
                    (ddot np1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (n4p5)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (n4p5)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1))
            (cond
              ((<= (f2cl-lib:fsqrt rbnprd) rbtol)
               (setf stillb f2cl-lib:%false%)))))
         (cond
           (stillb
            (setf ab (/ rbnprd pbnprd))
            (dcopy np1
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n2p3)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1 work 1)
            (daxpy np1 ab
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n4p5)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n2p3)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1)
            (daxpy np1 -1.0
             (f2cl-lib:array-slice work-%data%
                                   double-float
                                   (n2p3)
                                   ((1
                                     (f2cl-lib:int-add
                                      (f2cl-lib:int-mul 5
                                                        (f2cl-lib:int-add nn
                                                                          1))
                                      lenaa
                                      nn
                                      1)))
                                   work-%offset%)
             1 work 1)
            (setf zlen
                    (dnrm2 np1
                     (f2cl-lib:array-slice work-%data%
                                           double-float
                                           (n2p3)
                                           ((1
                                             (f2cl-lib:int-add
                                              (f2cl-lib:int-mul 5
                                                                (f2cl-lib:int-add
                                                                 nn
                                                                 1))
                                              lenaa
                                              nn
                                              1)))
                                           work-%offset%)
                     1))
            (setf dznrm (dnrm2 np1 work 1))
            (if (< (/ dznrm zlen) ztol) (setf stillb f2cl-lib:%false%)))))
        (t
         (setf stillb f2cl-lib:%false%)))
      (cond
        (stillb
         (multds work aa
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n4p5)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          maxa nn lenaa)
         (setf (f2cl-lib:fref work-%data%
                              (np1)
                              ((1
                                (f2cl-lib:int-add
                                 (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                 lenaa
                                 nn
                                 1)))
                              work-%offset%)
                 (f2cl-lib:fref work-%data%
                                ((f2cl-lib:int-sub (f2cl-lib:int-add n4p5 k)
                                                   1))
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%))
         (if (< k np1)
             (setf (f2cl-lib:fref work-%data%
                                  (k)
                                  ((1
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int-mul 5
                                                       (f2cl-lib:int-add nn 1))
                                     lenaa
                                     nn
                                     1)))
                                  work-%offset%)
                     (+
                      (f2cl-lib:fref work-%data%
                                     (k)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%)
                      (f2cl-lib:fref work-%data%
                                     ((f2cl-lib:int-add n4p5 nn))
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%))))
         (qimuds
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n5p6)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          work maxa nn lenaa)
         (daxpy np1 (- ab) work 1
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n3p4)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          1)
         (setf rnprd
                 (ddot np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (n3p4)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                  1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (n3p4)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                  1))
         (setf bb (/ rnprd rbnprd))
         (setf rbnprd rnprd)
         (dcopy np1
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n3p4)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          1 work 1)
         (qimuds
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n5p6)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          work maxa nn lenaa)
         (multds start aa work maxa nn lenaa)
         (setf (f2cl-lib:fref start-%data%
                              (np1)
                              ((1 (f2cl-lib:int-add nn 1)))
                              start-%offset%)
                 (f2cl-lib:fref work-%data%
                                (k)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%))
         (if (< k np1)
             (setf (f2cl-lib:fref start-%data%
                                  (k)
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  start-%offset%)
                     (+
                      (f2cl-lib:fref start-%data%
                                     (k)
                                     ((1 (f2cl-lib:int-add nn 1)))
                                     start-%offset%)
                      (f2cl-lib:fref work-%data%
                                     (np1)
                                     ((1
                                       (f2cl-lib:int-add
                                        (f2cl-lib:int-mul 5
                                                          (f2cl-lib:int-add nn
                                                                            1))
                                        lenaa
                                        nn
                                        1)))
                                     work-%offset%))))
         (daxpy np1 bb
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n4p5)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          1 start 1)
         (dcopy np1 start 1
          (f2cl-lib:array-slice work-%data%
                                double-float
                                (n4p5)
                                ((1
                                  (f2cl-lib:int-add
                                   (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                   lenaa
                                   nn
                                   1)))
                                work-%offset%)
          1)
         (setf pbnprd
                 (ddot np1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (n4p5)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                  1
                  (f2cl-lib:array-slice work-%data%
                                        double-float
                                        (n4p5)
                                        ((1
                                          (f2cl-lib:int-add
                                           (f2cl-lib:int-mul 5
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                           lenaa
                                           nn
                                           1)))
                                        work-%offset%)
                  1))))
      (setf j (f2cl-lib:int-add j 1))
      (go label300)
     label400
      (cond
        ((> j imax)
         (setf iflag 4)
         (go end_label)))
      (setf temp
              (/
               (-
                (f2cl-lib:fref work-%data%
                               ((f2cl-lib:int-add n2p3 nn))
                               ((1
                                 (f2cl-lib:int-add
                                  (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                  lenaa
                                  nn
                                  1)))
                               work-%offset%))
               (+ 1.0
                  (f2cl-lib:fref work-%data%
                                 ((f2cl-lib:int-add np2 nn))
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 5
                                                      (f2cl-lib:int-add nn 1))
                                    lenaa
                                    nn
                                    1)))
                                 work-%offset%))))
      (dcopy np1
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (n2p3)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       1 start 1)
      (daxpy np1 temp
       (f2cl-lib:array-slice work-%data%
                             double-float
                             (np2)
                             ((1
                               (f2cl-lib:int-add
                                (f2cl-lib:int-mul 5 (f2cl-lib:int-add nn 1))
                                lenaa
                                nn
                                1)))
                             work-%offset%)
       1 start 1)
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil iflag)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::pcgds fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::iflag)
           :calls '(fortran-to-lisp::ddot fortran-to-lisp::daxpy
                    fortran-to-lisp::dscal fortran-to-lisp::dnrm2
                    fortran-to-lisp::dcopy fortran-to-lisp::idamax
                    fortran-to-lisp::qimuds fortran-to-lisp::multds
                    fortran-to-lisp::d1mach fortran-to-lisp::mfacds))))

