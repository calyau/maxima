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
;;;           (:float-format single-float))

(in-package "FFTPACK5")


(defun mradf3 (m ido l1 cc im1 in1 ch im2 in2 wa1 wa2)
  (declare (type (array double-float (*)) wa2 wa1 ch cc)
           (type (f2cl-lib:integer4) in2 im2 in1 im1 l1 ido m))
  (f2cl-lib:with-multi-array-data
      ((cc double-float cc-%data% cc-%offset%)
       (ch double-float ch-%data% ch-%offset%)
       (wa1 double-float wa1-%data% wa1-%offset%)
       (wa2 double-float wa2-%data% wa2-%offset%))
    (prog ((ic 0) (i 0) (idp2 0) (m1 0) (m2 0) (k 0) (taui 0.0d0) (taur 0.0d0)
           (arg 0.0d0) (m2s 0) (m1d 0))
      (declare (type (double-float) arg taur taui)
               (type (f2cl-lib:integer4) m1d m2s k m2 m1 idp2 i ic))
      (setf m1d
              (f2cl-lib:int-add (f2cl-lib:int-mul (f2cl-lib:int-sub m 1) im1)
                                1))
      (setf m2s (f2cl-lib:int-sub 1 im2))
      (setf arg (/ (* 2.0d0 4.0d0 (atan 1.0d0)) 3.0d0))
      (setf taur (cos arg))
      (setf taui (sin arg))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k l1) nil)
        (tagbody
          (setf m2 m2s)
          (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                        ((> m1 m1d) nil)
            (tagbody
              (setf m2 (f2cl-lib:int-add m2 im2))
              (setf (f2cl-lib:fref ch-%data%
                                   (m2 1 1 k)
                                   ((1 in2) (1 ido) (1 3) (1 l1))
                                   ch-%offset%)
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (m1 1 k 1)
                                      ((1 in1) (1 ido) (1 l1) (1 3))
                                      cc-%offset%)
                       (+
                        (f2cl-lib:fref cc-%data%
                                       (m1 1 k 2)
                                       ((1 in1) (1 ido) (1 l1) (1 3))
                                       cc-%offset%)
                        (f2cl-lib:fref cc-%data%
                                       (m1 1 k 3)
                                       ((1 in1) (1 ido) (1 l1) (1 3))
                                       cc-%offset%))))
              (setf (f2cl-lib:fref ch-%data%
                                   (m2 1 3 k)
                                   ((1 in2) (1 ido) (1 3) (1 l1))
                                   ch-%offset%)
                      (* taui
                         (-
                          (f2cl-lib:fref cc-%data%
                                         (m1 1 k 3)
                                         ((1 in1) (1 ido) (1 l1) (1 3))
                                         cc-%offset%)
                          (f2cl-lib:fref cc-%data%
                                         (m1 1 k 2)
                                         ((1 in1) (1 ido) (1 l1) (1 3))
                                         cc-%offset%))))
              (setf (f2cl-lib:fref ch-%data%
                                   (m2 ido 2 k)
                                   ((1 in2) (1 ido) (1 3) (1 l1))
                                   ch-%offset%)
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (m1 1 k 1)
                                      ((1 in1) (1 ido) (1 l1) (1 3))
                                      cc-%offset%)
                       (* taur
                          (+
                           (f2cl-lib:fref cc-%data%
                                          (m1 1 k 2)
                                          ((1 in1) (1 ido) (1 l1) (1 3))
                                          cc-%offset%)
                           (f2cl-lib:fref cc-%data%
                                          (m1 1 k 3)
                                          ((1 in1) (1 ido) (1 l1) (1 3))
                                          cc-%offset%)))))
             label1001))
         label101))
      (if (= ido 1) (go end_label))
      (setf idp2 (f2cl-lib:int-add ido 2))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k l1) nil)
        (tagbody
          (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                        ((> i ido) nil)
            (tagbody
              (setf ic (f2cl-lib:int-sub idp2 i))
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref ch-%data%
                                       (m2 (f2cl-lib:int-sub i 1) 1 k)
                                       ((1 in2) (1 ido) (1 3) (1 l1))
                                       ch-%offset%)
                          (+
                           (f2cl-lib:fref cc-%data%
                                          (m1 (f2cl-lib:int-sub i 1) k 1)
                                          ((1 in1) (1 ido) (1 l1) (1 3))
                                          cc-%offset%)
                           (+
                            (*
                             (f2cl-lib:fref wa1-%data%
                                            ((f2cl-lib:int-sub i 2))
                                            ((1 ido))
                                            wa1-%offset%)
                             (f2cl-lib:fref cc-%data%
                                            (m1 (f2cl-lib:int-sub i 1) k 2)
                                            ((1 in1) (1 ido) (1 l1) (1 3))
                                            cc-%offset%))
                            (*
                             (f2cl-lib:fref wa1-%data%
                                            ((f2cl-lib:int-sub i 1))
                                            ((1 ido))
                                            wa1-%offset%)
                             (f2cl-lib:fref cc-%data%
                                            (m1 i k 2)
                                            ((1 in1) (1 ido) (1 l1) (1 3))
                                            cc-%offset%))
                            (+
                             (*
                              (f2cl-lib:fref wa2-%data%
                                             ((f2cl-lib:int-sub i 2))
                                             ((1 ido))
                                             wa2-%offset%)
                              (f2cl-lib:fref cc-%data%
                                             (m1 (f2cl-lib:int-sub i 1) k 3)
                                             ((1 in1) (1 ido) (1 l1) (1 3))
                                             cc-%offset%))
                             (*
                              (f2cl-lib:fref wa2-%data%
                                             ((f2cl-lib:int-sub i 1))
                                             ((1 ido))
                                             wa2-%offset%)
                              (f2cl-lib:fref cc-%data%
                                             (m1 i k 3)
                                             ((1 in1) (1 ido) (1 l1) (1 3))
                                             cc-%offset%))))))
                  (setf (f2cl-lib:fref ch-%data%
                                       (m2 i 1 k)
                                       ((1 in2) (1 ido) (1 3) (1 l1))
                                       ch-%offset%)
                          (+
                           (f2cl-lib:fref cc-%data%
                                          (m1 i k 1)
                                          ((1 in1) (1 ido) (1 l1) (1 3))
                                          cc-%offset%)
                           (+
                            (-
                             (*
                              (f2cl-lib:fref wa1-%data%
                                             ((f2cl-lib:int-sub i 2))
                                             ((1 ido))
                                             wa1-%offset%)
                              (f2cl-lib:fref cc-%data%
                                             (m1 i k 2)
                                             ((1 in1) (1 ido) (1 l1) (1 3))
                                             cc-%offset%))
                             (*
                              (f2cl-lib:fref wa1-%data%
                                             ((f2cl-lib:int-sub i 1))
                                             ((1 ido))
                                             wa1-%offset%)
                              (f2cl-lib:fref cc-%data%
                                             (m1 (f2cl-lib:int-sub i 1) k 2)
                                             ((1 in1) (1 ido) (1 l1) (1 3))
                                             cc-%offset%)))
                            (-
                             (*
                              (f2cl-lib:fref wa2-%data%
                                             ((f2cl-lib:int-sub i 2))
                                             ((1 ido))
                                             wa2-%offset%)
                              (f2cl-lib:fref cc-%data%
                                             (m1 i k 3)
                                             ((1 in1) (1 ido) (1 l1) (1 3))
                                             cc-%offset%))
                             (*
                              (f2cl-lib:fref wa2-%data%
                                             ((f2cl-lib:int-sub i 1))
                                             ((1 ido))
                                             wa2-%offset%)
                              (f2cl-lib:fref cc-%data%
                                             (m1 (f2cl-lib:int-sub i 1) k 3)
                                             ((1 in1) (1 ido) (1 l1) (1 3))
                                             cc-%offset%))))))
                  (setf (f2cl-lib:fref ch-%data%
                                       (m2 (f2cl-lib:int-sub i 1) 3 k)
                                       ((1 in2) (1 ido) (1 3) (1 l1))
                                       ch-%offset%)
                          (+
                           (f2cl-lib:fref cc-%data%
                                          (m1 (f2cl-lib:int-sub i 1) k 1)
                                          ((1 in1) (1 ido) (1 l1) (1 3))
                                          cc-%offset%)
                           (* taur
                              (+
                               (*
                                (f2cl-lib:fref wa1-%data%
                                               ((f2cl-lib:int-sub i 2))
                                               ((1 ido))
                                               wa1-%offset%)
                                (f2cl-lib:fref cc-%data%
                                               (m1 (f2cl-lib:int-sub i 1) k 2)
                                               ((1 in1) (1 ido) (1 l1) (1 3))
                                               cc-%offset%))
                               (*
                                (f2cl-lib:fref wa1-%data%
                                               ((f2cl-lib:int-sub i 1))
                                               ((1 ido))
                                               wa1-%offset%)
                                (f2cl-lib:fref cc-%data%
                                               (m1 i k 2)
                                               ((1 in1) (1 ido) (1 l1) (1 3))
                                               cc-%offset%))
                               (+
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%)))))
                           (* taui
                              (-
                               (*
                                (f2cl-lib:fref wa1-%data%
                                               ((f2cl-lib:int-sub i 2))
                                               ((1 ido))
                                               wa1-%offset%)
                                (f2cl-lib:fref cc-%data%
                                               (m1 i k 2)
                                               ((1 in1) (1 ido) (1 l1) (1 3))
                                               cc-%offset%))
                               (*
                                (f2cl-lib:fref wa1-%data%
                                               ((f2cl-lib:int-sub i 1))
                                               ((1 ido))
                                               wa1-%offset%)
                                (f2cl-lib:fref cc-%data%
                                               (m1 (f2cl-lib:int-sub i 1) k 2)
                                               ((1 in1) (1 ido) (1 l1) (1 3))
                                               cc-%offset%))
                               (-
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%)))))))
                  (setf (f2cl-lib:fref ch-%data%
                                       (m2 (f2cl-lib:int-sub ic 1) 2 k)
                                       ((1 in2) (1 ido) (1 3) (1 l1))
                                       ch-%offset%)
                          (-
                           (+
                            (f2cl-lib:fref cc-%data%
                                           (m1 (f2cl-lib:int-sub i 1) k 1)
                                           ((1 in1) (1 ido) (1 l1) (1 3))
                                           cc-%offset%)
                            (* taur
                               (+
                                (*
                                 (f2cl-lib:fref wa1-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa1-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 2)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa1-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa1-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 2)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (+
                                 (*
                                  (f2cl-lib:fref wa2-%data%
                                                 ((f2cl-lib:int-sub i 2))
                                                 ((1 ido))
                                                 wa2-%offset%)
                                  (f2cl-lib:fref cc-%data%
                                                 (m1 (f2cl-lib:int-sub i 1) k
                                                  3)
                                                 ((1 in1) (1 ido) (1 l1) (1 3))
                                                 cc-%offset%))
                                 (*
                                  (f2cl-lib:fref wa2-%data%
                                                 ((f2cl-lib:int-sub i 1))
                                                 ((1 ido))
                                                 wa2-%offset%)
                                  (f2cl-lib:fref cc-%data%
                                                 (m1 i k 3)
                                                 ((1 in1) (1 ido) (1 l1) (1 3))
                                                 cc-%offset%))))))
                           (* taui
                              (-
                               (*
                                (f2cl-lib:fref wa1-%data%
                                               ((f2cl-lib:int-sub i 2))
                                               ((1 ido))
                                               wa1-%offset%)
                                (f2cl-lib:fref cc-%data%
                                               (m1 i k 2)
                                               ((1 in1) (1 ido) (1 l1) (1 3))
                                               cc-%offset%))
                               (*
                                (f2cl-lib:fref wa1-%data%
                                               ((f2cl-lib:int-sub i 1))
                                               ((1 ido))
                                               wa1-%offset%)
                                (f2cl-lib:fref cc-%data%
                                               (m1 (f2cl-lib:int-sub i 1) k 2)
                                               ((1 in1) (1 ido) (1 l1) (1 3))
                                               cc-%offset%))
                               (-
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%)))))))
                  (setf (f2cl-lib:fref ch-%data%
                                       (m2 i 3 k)
                                       ((1 in2) (1 ido) (1 3) (1 l1))
                                       ch-%offset%)
                          (+
                           (f2cl-lib:fref cc-%data%
                                          (m1 i k 1)
                                          ((1 in1) (1 ido) (1 l1) (1 3))
                                          cc-%offset%)
                           (* taur
                              (+
                               (-
                                (*
                                 (f2cl-lib:fref wa1-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa1-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 2)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa1-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa1-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 2)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%)))
                               (-
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%)))))
                           (* taui
                              (-
                               (+
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%)))
                               (+
                                (*
                                 (f2cl-lib:fref wa1-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa1-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 2)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa1-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa1-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 2)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%)))))))
                  (setf (f2cl-lib:fref ch-%data%
                                       (m2 ic 2 k)
                                       ((1 in2) (1 ido) (1 3) (1 l1))
                                       ch-%offset%)
                          (-
                           (* taui
                              (-
                               (+
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa2-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa2-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 3)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%)))
                               (+
                                (*
                                 (f2cl-lib:fref wa1-%data%
                                                ((f2cl-lib:int-sub i 2))
                                                ((1 ido))
                                                wa1-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 (f2cl-lib:int-sub i 1) k 2)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%))
                                (*
                                 (f2cl-lib:fref wa1-%data%
                                                ((f2cl-lib:int-sub i 1))
                                                ((1 ido))
                                                wa1-%offset%)
                                 (f2cl-lib:fref cc-%data%
                                                (m1 i k 2)
                                                ((1 in1) (1 ido) (1 l1) (1 3))
                                                cc-%offset%)))))
                           (+
                            (f2cl-lib:fref cc-%data%
                                           (m1 i k 1)
                                           ((1 in1) (1 ido) (1 l1) (1 3))
                                           cc-%offset%)
                            (* taur
                               (+
                                (-
                                 (*
                                  (f2cl-lib:fref wa1-%data%
                                                 ((f2cl-lib:int-sub i 2))
                                                 ((1 ido))
                                                 wa1-%offset%)
                                  (f2cl-lib:fref cc-%data%
                                                 (m1 i k 2)
                                                 ((1 in1) (1 ido) (1 l1) (1 3))
                                                 cc-%offset%))
                                 (*
                                  (f2cl-lib:fref wa1-%data%
                                                 ((f2cl-lib:int-sub i 1))
                                                 ((1 ido))
                                                 wa1-%offset%)
                                  (f2cl-lib:fref cc-%data%
                                                 (m1 (f2cl-lib:int-sub i 1) k
                                                  2)
                                                 ((1 in1) (1 ido) (1 l1) (1 3))
                                                 cc-%offset%)))
                                (-
                                 (*
                                  (f2cl-lib:fref wa2-%data%
                                                 ((f2cl-lib:int-sub i 2))
                                                 ((1 ido))
                                                 wa2-%offset%)
                                  (f2cl-lib:fref cc-%data%
                                                 (m1 i k 3)
                                                 ((1 in1) (1 ido) (1 l1) (1 3))
                                                 cc-%offset%))
                                 (*
                                  (f2cl-lib:fref wa2-%data%
                                                 ((f2cl-lib:int-sub i 1))
                                                 ((1 ido))
                                                 wa2-%offset%)
                                  (f2cl-lib:fref cc-%data%
                                                 (m1 (f2cl-lib:int-sub i 1) k
                                                  3)
                                                 ((1 in1) (1 ido) (1 l1) (1 3))
                                                 cc-%offset%))))))))
                 label1002))
             label102))
         label103))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mradf3
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil)
           :calls 'nil)))

