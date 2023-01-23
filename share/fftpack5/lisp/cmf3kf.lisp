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


(let ((taur -0.5d0) (taui -0.866025403784439d0))
  (declare (type (double-float) taur taui))
  (defun cmf3kf (lot ido l1 na cc im1 in1 ch im2 in2 wa)
    (declare (type (array double-float (*)) wa ch cc)
             (type (f2cl-lib:integer4) in2 im2 in1 im1 na l1 ido lot))
    (f2cl-lib:with-multi-array-data
        ((cc double-float cc-%data% cc-%offset%)
         (ch double-float ch-%data% ch-%offset%)
         (wa double-float wa-%data% wa-%offset%))
      (prog ((di3 0.0d0) (di2 0.0d0) (dr3 0.0d0) (dr2 0.0d0) (i 0) (m2 0)
             (ci3 0.0d0) (cr3 0.0d0) (ci2 0.0d0) (ti2 0.0d0) (cr2 0.0d0)
             (tr2 0.0d0) (m1 0) (k 0) (sn 0.0d0) (m2s 0) (m1d 0))
        (declare (type (f2cl-lib:integer4) m1d m2s k m1 m2 i)
                 (type (double-float) sn tr2 cr2 ti2 ci2 cr3 ci3 dr2 dr3 di2
                                      di3))
        (setf m1d
                (f2cl-lib:int-add
                 (f2cl-lib:int-mul (f2cl-lib:int-sub lot 1) im1)
                 1))
        (setf m2s (f2cl-lib:int-sub 1 im2))
        (if (> ido 1) (go label102))
        (setf sn (/ 1.0d0 (f2cl-lib:freal (f2cl-lib:int-mul 3 l1))))
        (if (= na 1) (go label106))
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k l1) nil)
          (tagbody
            (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                          ((> m1 m1d) nil)
              (tagbody
                (setf tr2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 2)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 3)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)))
                (setf cr2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 1)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (* taur tr2)))
                (setf (f2cl-lib:fref cc-%data%
                                     (1 m1 k 1 1)
                                     ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                     cc-%offset%)
                        (* sn
                           (+
                            (f2cl-lib:fref cc-%data%
                                           (1 m1 k 1 1)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            tr2)))
                (setf ti2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 2)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 3)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)))
                (setf ci2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 1)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (* taur ti2)))
                (setf (f2cl-lib:fref cc-%data%
                                     (2 m1 k 1 1)
                                     ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                     cc-%offset%)
                        (* sn
                           (+
                            (f2cl-lib:fref cc-%data%
                                           (2 m1 k 1 1)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            ti2)))
                (setf cr3
                        (* taui
                           (-
                            (f2cl-lib:fref cc-%data%
                                           (1 m1 k 1 2)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            (f2cl-lib:fref cc-%data%
                                           (1 m1 k 1 3)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%))))
                (setf ci3
                        (* taui
                           (-
                            (f2cl-lib:fref cc-%data%
                                           (2 m1 k 1 2)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            (f2cl-lib:fref cc-%data%
                                           (2 m1 k 1 3)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%))))
                (setf (f2cl-lib:fref cc-%data%
                                     (1 m1 k 1 2)
                                     ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                     cc-%offset%)
                        (* sn (- cr2 ci3)))
                (setf (f2cl-lib:fref cc-%data%
                                     (1 m1 k 1 3)
                                     ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                     cc-%offset%)
                        (* sn (+ cr2 ci3)))
                (setf (f2cl-lib:fref cc-%data%
                                     (2 m1 k 1 2)
                                     ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                     cc-%offset%)
                        (* sn (+ ci2 cr3)))
                (setf (f2cl-lib:fref cc-%data%
                                     (2 m1 k 1 3)
                                     ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                     cc-%offset%)
                        (* sn (- ci2 cr3)))
               label101))))
       label101
        (go end_label)
       label106
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k l1) nil)
          (tagbody
            (setf m2 m2s)
            (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                          ((> m1 m1d) nil)
              (tagbody
                (setf m2 (f2cl-lib:int-add m2 im2))
                (setf tr2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 2)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 3)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)))
                (setf cr2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 1)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (* taur tr2)))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 m2 k 1 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (* sn
                           (+
                            (f2cl-lib:fref cc-%data%
                                           (1 m1 k 1 1)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            tr2)))
                (setf ti2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 2)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 3)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)))
                (setf ci2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 1)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (* taur ti2)))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 m2 k 1 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (* sn
                           (+
                            (f2cl-lib:fref cc-%data%
                                           (2 m1 k 1 1)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            ti2)))
                (setf cr3
                        (* taui
                           (-
                            (f2cl-lib:fref cc-%data%
                                           (1 m1 k 1 2)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            (f2cl-lib:fref cc-%data%
                                           (1 m1 k 1 3)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%))))
                (setf ci3
                        (* taui
                           (-
                            (f2cl-lib:fref cc-%data%
                                           (2 m1 k 1 2)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            (f2cl-lib:fref cc-%data%
                                           (2 m1 k 1 3)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%))))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 m2 k 2 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (* sn (- cr2 ci3)))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 m2 k 3 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (* sn (+ cr2 ci3)))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 m2 k 2 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (* sn (+ ci2 cr3)))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 m2 k 3 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (* sn (- ci2 cr3)))
               label107))))
       label107
        (go end_label)
       label102
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k l1) nil)
          (tagbody
            (setf m2 m2s)
            (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                          ((> m1 m1d) nil)
              (tagbody
                (setf m2 (f2cl-lib:int-add m2 im2))
                (setf tr2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 2)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 3)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)))
                (setf cr2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 1)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (* taur tr2)))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 m2 k 1 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 m1 k 1 1)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         tr2))
                (setf ti2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 2)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 3)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)))
                (setf ci2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 1)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         (* taur ti2)))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 m2 k 1 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 m1 k 1 1)
                                        ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                        cc-%offset%)
                         ti2))
                (setf cr3
                        (* taui
                           (-
                            (f2cl-lib:fref cc-%data%
                                           (1 m1 k 1 2)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            (f2cl-lib:fref cc-%data%
                                           (1 m1 k 1 3)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%))))
                (setf ci3
                        (* taui
                           (-
                            (f2cl-lib:fref cc-%data%
                                           (2 m1 k 1 2)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%)
                            (f2cl-lib:fref cc-%data%
                                           (2 m1 k 1 3)
                                           ((1 2) (1 in1) (1 l1) (1 ido) (1 3))
                                           cc-%offset%))))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 m2 k 2 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (- cr2 ci3))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 m2 k 3 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (+ cr2 ci3))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 m2 k 2 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (+ ci2 cr3))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 m2 k 3 1)
                                     ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                     ch-%offset%)
                        (- ci2 cr3))
               label103))))
       label103
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i ido) nil)
          (tagbody
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k l1) nil)
              (tagbody
                (setf m2 m2s)
                (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                              ((> m1 m1d) nil)
                  (tagbody
                    (setf m2 (f2cl-lib:int-add m2 im2))
                    (setf tr2
                            (+
                             (f2cl-lib:fref cc-%data%
                                            (1 m1 k i 2)
                                            ((1 2) (1 in1) (1 l1) (1 ido)
                                             (1 3))
                                            cc-%offset%)
                             (f2cl-lib:fref cc-%data%
                                            (1 m1 k i 3)
                                            ((1 2) (1 in1) (1 l1) (1 ido)
                                             (1 3))
                                            cc-%offset%)))
                    (setf cr2
                            (+
                             (f2cl-lib:fref cc-%data%
                                            (1 m1 k i 1)
                                            ((1 2) (1 in1) (1 l1) (1 ido)
                                             (1 3))
                                            cc-%offset%)
                             (* taur tr2)))
                    (setf (f2cl-lib:fref ch-%data%
                                         (1 m2 k 1 i)
                                         ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                         ch-%offset%)
                            (+
                             (f2cl-lib:fref cc-%data%
                                            (1 m1 k i 1)
                                            ((1 2) (1 in1) (1 l1) (1 ido)
                                             (1 3))
                                            cc-%offset%)
                             tr2))
                    (setf ti2
                            (+
                             (f2cl-lib:fref cc-%data%
                                            (2 m1 k i 2)
                                            ((1 2) (1 in1) (1 l1) (1 ido)
                                             (1 3))
                                            cc-%offset%)
                             (f2cl-lib:fref cc-%data%
                                            (2 m1 k i 3)
                                            ((1 2) (1 in1) (1 l1) (1 ido)
                                             (1 3))
                                            cc-%offset%)))
                    (setf ci2
                            (+
                             (f2cl-lib:fref cc-%data%
                                            (2 m1 k i 1)
                                            ((1 2) (1 in1) (1 l1) (1 ido)
                                             (1 3))
                                            cc-%offset%)
                             (* taur ti2)))
                    (setf (f2cl-lib:fref ch-%data%
                                         (2 m2 k 1 i)
                                         ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                         ch-%offset%)
                            (+
                             (f2cl-lib:fref cc-%data%
                                            (2 m1 k i 1)
                                            ((1 2) (1 in1) (1 l1) (1 ido)
                                             (1 3))
                                            cc-%offset%)
                             ti2))
                    (setf cr3
                            (* taui
                               (-
                                (f2cl-lib:fref cc-%data%
                                               (1 m1 k i 2)
                                               ((1 2) (1 in1) (1 l1) (1 ido)
                                                (1 3))
                                               cc-%offset%)
                                (f2cl-lib:fref cc-%data%
                                               (1 m1 k i 3)
                                               ((1 2) (1 in1) (1 l1) (1 ido)
                                                (1 3))
                                               cc-%offset%))))
                    (setf ci3
                            (* taui
                               (-
                                (f2cl-lib:fref cc-%data%
                                               (2 m1 k i 2)
                                               ((1 2) (1 in1) (1 l1) (1 ido)
                                                (1 3))
                                               cc-%offset%)
                                (f2cl-lib:fref cc-%data%
                                               (2 m1 k i 3)
                                               ((1 2) (1 in1) (1 l1) (1 ido)
                                                (1 3))
                                               cc-%offset%))))
                    (setf dr2 (- cr2 ci3))
                    (setf dr3 (+ cr2 ci3))
                    (setf di2 (+ ci2 cr3))
                    (setf di3 (- ci2 cr3))
                    (setf (f2cl-lib:fref ch-%data%
                                         (2 m2 k 2 i)
                                         ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                         ch-%offset%)
                            (-
                             (*
                              (f2cl-lib:fref wa-%data%
                                             (i 1 1)
                                             ((1 ido) (1 2) (1 2))
                                             wa-%offset%)
                              di2)
                             (*
                              (f2cl-lib:fref wa-%data%
                                             (i 1 2)
                                             ((1 ido) (1 2) (1 2))
                                             wa-%offset%)
                              dr2)))
                    (setf (f2cl-lib:fref ch-%data%
                                         (1 m2 k 2 i)
                                         ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                         ch-%offset%)
                            (+
                             (*
                              (f2cl-lib:fref wa-%data%
                                             (i 1 1)
                                             ((1 ido) (1 2) (1 2))
                                             wa-%offset%)
                              dr2)
                             (*
                              (f2cl-lib:fref wa-%data%
                                             (i 1 2)
                                             ((1 ido) (1 2) (1 2))
                                             wa-%offset%)
                              di2)))
                    (setf (f2cl-lib:fref ch-%data%
                                         (2 m2 k 3 i)
                                         ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                         ch-%offset%)
                            (-
                             (*
                              (f2cl-lib:fref wa-%data%
                                             (i 2 1)
                                             ((1 ido) (1 2) (1 2))
                                             wa-%offset%)
                              di3)
                             (*
                              (f2cl-lib:fref wa-%data%
                                             (i 2 2)
                                             ((1 ido) (1 2) (1 2))
                                             wa-%offset%)
                              dr3)))
                    (setf (f2cl-lib:fref ch-%data%
                                         (1 m2 k 3 i)
                                         ((1 2) (1 in2) (1 l1) (1 3) (1 ido))
                                         ch-%offset%)
                            (+
                             (*
                              (f2cl-lib:fref wa-%data%
                                             (i 2 1)
                                             ((1 ido) (1 2) (1 2))
                                             wa-%offset%)
                              dr3)
                             (*
                              (f2cl-lib:fref wa-%data%
                                             (i 2 2)
                                             ((1 ido) (1 2) (1 2))
                                             wa-%offset%)
                              di3)))
                   label104))))
           label104
           label105))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil nil nil))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cmf3kf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil)
           :calls 'nil)))

