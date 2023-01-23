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


(defun cmf4kf (lot ido l1 na cc im1 in1 ch im2 in2 wa)
  (declare (type (array double-float (*)) wa ch cc)
           (type (f2cl-lib:integer4) in2 im2 in1 im1 na l1 ido lot))
  (f2cl-lib:with-multi-array-data
      ((cc double-float cc-%data% cc-%offset%)
       (ch double-float ch-%data% ch-%offset%)
       (wa double-float wa-%data% wa-%offset%))
    (prog ((ci4 0.0d0) (ci2 0.0d0) (cr4 0.0d0) (cr2 0.0d0) (ci3 0.0d0)
           (cr3 0.0d0) (i 0) (m2 0) (tr3 0.0d0) (ti4 0.0d0) (tr2 0.0d0)
           (tr1 0.0d0) (ti3 0.0d0) (tr4 0.0d0) (ti2 0.0d0) (ti1 0.0d0) (m1 0)
           (k 0) (sn 0.0d0) (m2s 0) (m1d 0))
      (declare (type (f2cl-lib:integer4) m1d m2s k m1 m2 i)
               (type (double-float) sn ti1 ti2 tr4 ti3 tr1 tr2 ti4 tr3 cr3 ci3
                                    cr2 cr4 ci2 ci4))
      (setf m1d
              (f2cl-lib:int-add (f2cl-lib:int-mul (f2cl-lib:int-sub lot 1) im1)
                                1))
      (setf m2s (f2cl-lib:int-sub 1 im2))
      (if (> ido 1) (go label102))
      (setf sn (/ 1.0d0 (f2cl-lib:freal (f2cl-lib:int-mul 4 l1))))
      (if (= na 1) (go label106))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k l1) nil)
        (tagbody
          (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                        ((> m1 m1d) nil)
            (tagbody
              (setf ti1
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf ti2
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr4
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf ti3
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr1
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr2
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf ti4
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr3
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf (f2cl-lib:fref cc-%data%
                                   (1 m1 k 1 1)
                                   ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                   cc-%offset%)
                      (* sn (+ tr2 tr3)))
              (setf (f2cl-lib:fref cc-%data%
                                   (1 m1 k 1 3)
                                   ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                   cc-%offset%)
                      (* sn (- tr2 tr3)))
              (setf (f2cl-lib:fref cc-%data%
                                   (2 m1 k 1 1)
                                   ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                   cc-%offset%)
                      (* sn (+ ti2 ti3)))
              (setf (f2cl-lib:fref cc-%data%
                                   (2 m1 k 1 3)
                                   ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                   cc-%offset%)
                      (* sn (- ti2 ti3)))
              (setf (f2cl-lib:fref cc-%data%
                                   (1 m1 k 1 2)
                                   ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                   cc-%offset%)
                      (* sn (+ tr1 tr4)))
              (setf (f2cl-lib:fref cc-%data%
                                   (1 m1 k 1 4)
                                   ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                   cc-%offset%)
                      (* sn (- tr1 tr4)))
              (setf (f2cl-lib:fref cc-%data%
                                   (2 m1 k 1 2)
                                   ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                   cc-%offset%)
                      (* sn (+ ti1 ti4)))
              (setf (f2cl-lib:fref cc-%data%
                                   (2 m1 k 1 4)
                                   ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                   cc-%offset%)
                      (* sn (- ti1 ti4)))
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
              (setf ti1
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf ti2
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr4
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf ti3
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr1
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr2
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf ti4
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr3
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 m2 k 1 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (* sn (+ tr2 tr3)))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 m2 k 3 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (* sn (- tr2 tr3)))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 m2 k 1 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (* sn (+ ti2 ti3)))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 m2 k 3 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (* sn (- ti2 ti3)))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 m2 k 2 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (* sn (+ tr1 tr4)))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 m2 k 4 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (* sn (- tr1 tr4)))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 m2 k 2 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (* sn (+ ti1 ti4)))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 m2 k 4 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (* sn (- ti1 ti4)))
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
              (setf ti1
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf ti2
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr4
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf ti3
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr1
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr2
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 1)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 3)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf ti4
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf tr3
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 2)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 m1 k 1 4)
                                      ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                      cc-%offset%)))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 m2 k 1 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (+ tr2 tr3))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 m2 k 3 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (- tr2 tr3))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 m2 k 1 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (+ ti2 ti3))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 m2 k 3 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (- ti2 ti3))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 m2 k 2 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (+ tr1 tr4))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 m2 k 4 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (- tr1 tr4))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 m2 k 2 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (+ ti1 ti4))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 m2 k 4 1)
                                   ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                   ch-%offset%)
                      (- ti1 ti4))
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
                  (setf ti1
                          (-
                           (f2cl-lib:fref cc-%data%
                                          (2 m1 k i 1)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)
                           (f2cl-lib:fref cc-%data%
                                          (2 m1 k i 3)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)))
                  (setf ti2
                          (+
                           (f2cl-lib:fref cc-%data%
                                          (2 m1 k i 1)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)
                           (f2cl-lib:fref cc-%data%
                                          (2 m1 k i 3)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)))
                  (setf ti3
                          (+
                           (f2cl-lib:fref cc-%data%
                                          (2 m1 k i 2)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)
                           (f2cl-lib:fref cc-%data%
                                          (2 m1 k i 4)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)))
                  (setf tr4
                          (-
                           (f2cl-lib:fref cc-%data%
                                          (2 m1 k i 2)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)
                           (f2cl-lib:fref cc-%data%
                                          (2 m1 k i 4)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)))
                  (setf tr1
                          (-
                           (f2cl-lib:fref cc-%data%
                                          (1 m1 k i 1)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)
                           (f2cl-lib:fref cc-%data%
                                          (1 m1 k i 3)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)))
                  (setf tr2
                          (+
                           (f2cl-lib:fref cc-%data%
                                          (1 m1 k i 1)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)
                           (f2cl-lib:fref cc-%data%
                                          (1 m1 k i 3)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)))
                  (setf ti4
                          (-
                           (f2cl-lib:fref cc-%data%
                                          (1 m1 k i 4)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)
                           (f2cl-lib:fref cc-%data%
                                          (1 m1 k i 2)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)))
                  (setf tr3
                          (+
                           (f2cl-lib:fref cc-%data%
                                          (1 m1 k i 2)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)
                           (f2cl-lib:fref cc-%data%
                                          (1 m1 k i 4)
                                          ((1 2) (1 in1) (1 l1) (1 ido) (1 4))
                                          cc-%offset%)))
                  (setf (f2cl-lib:fref ch-%data%
                                       (1 m2 k 1 i)
                                       ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                       ch-%offset%)
                          (+ tr2 tr3))
                  (setf cr3 (- tr2 tr3))
                  (setf (f2cl-lib:fref ch-%data%
                                       (2 m2 k 1 i)
                                       ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                       ch-%offset%)
                          (+ ti2 ti3))
                  (setf ci3 (- ti2 ti3))
                  (setf cr2 (+ tr1 tr4))
                  (setf cr4 (- tr1 tr4))
                  (setf ci2 (+ ti1 ti4))
                  (setf ci4 (- ti1 ti4))
                  (setf (f2cl-lib:fref ch-%data%
                                       (1 m2 k 2 i)
                                       ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                       ch-%offset%)
                          (+
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 1 1)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            cr2)
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 1 2)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            ci2)))
                  (setf (f2cl-lib:fref ch-%data%
                                       (2 m2 k 2 i)
                                       ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                       ch-%offset%)
                          (-
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 1 1)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            ci2)
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 1 2)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            cr2)))
                  (setf (f2cl-lib:fref ch-%data%
                                       (1 m2 k 3 i)
                                       ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                       ch-%offset%)
                          (+
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 2 1)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            cr3)
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 2 2)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            ci3)))
                  (setf (f2cl-lib:fref ch-%data%
                                       (2 m2 k 3 i)
                                       ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                       ch-%offset%)
                          (-
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 2 1)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            ci3)
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 2 2)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            cr3)))
                  (setf (f2cl-lib:fref ch-%data%
                                       (1 m2 k 4 i)
                                       ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                       ch-%offset%)
                          (+
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 3 1)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            cr4)
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 3 2)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            ci4)))
                  (setf (f2cl-lib:fref ch-%data%
                                       (2 m2 k 4 i)
                                       ((1 2) (1 in2) (1 l1) (1 4) (1 ido))
                                       ch-%offset%)
                          (-
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 3 1)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            ci4)
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (i 3 2)
                                           ((1 ido) (1 3) (1 2))
                                           wa-%offset%)
                            cr4)))
                 label104))))
         label104
         label105))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cmf4kf
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

