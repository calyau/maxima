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


(let ((tr11 0.3090169943749474d0)
      (ti11 0.9510565162951536d0)
      (tr12 -0.8090169943749475d0)
      (ti12 0.5877852522924731d0))
  (declare (type (double-float) tr11 ti11 tr12 ti12))
  (defun c1f5kb (ido l1 na cc in1 ch in2 wa)
    (declare (type (array double-float (*)) wa ch cc)
             (type (f2cl-lib:integer4) in2 in1 na l1 ido))
    (f2cl-lib:with-multi-array-data
        ((cc double-float cc-%data% cc-%offset%)
         (ch double-float ch-%data% ch-%offset%)
         (wa double-float wa-%data% wa-%offset%))
      (prog ((di2 0.0d0) (di5 0.0d0) (dr2 0.0d0) (dr5 0.0d0) (di4 0.0d0)
             (di3 0.0d0) (dr4 0.0d0) (dr3 0.0d0) (i 0) (ci4 0.0d0) (cr4 0.0d0)
             (ci5 0.0d0) (cr5 0.0d0) (ci3 0.0d0) (cr3 0.0d0) (ci2 0.0d0)
             (cr2 0.0d0) (chold2 0.0d0) (chold1 0.0d0) (tr3 0.0d0) (tr4 0.0d0)
             (tr2 0.0d0) (tr5 0.0d0) (ti3 0.0d0) (ti4 0.0d0) (ti2 0.0d0)
             (ti5 0.0d0) (k 0))
        (declare (type (f2cl-lib:integer4) k i)
                 (type (double-float) ti5 ti2 ti4 ti3 tr5 tr2 tr4 tr3 chold1
                                      chold2 cr2 ci2 cr3 ci3 cr5 ci5 cr4 ci4
                                      dr3 dr4 di3 di4 dr5 dr2 di5 di2))
        (if (or (> ido 1) (= na 1)) (go label102))
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k l1) nil)
          (tagbody
            (setf ti5
                    (-
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 2)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 5)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf ti2
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 2)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 5)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf ti4
                    (-
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 3)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 4)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf ti3
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 3)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 4)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf tr5
                    (-
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 2)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 5)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf tr2
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 2)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 5)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf tr4
                    (-
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 3)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 4)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf tr3
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 3)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 4)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf chold1
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     tr2
                     tr3))
            (setf chold2
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     ti2
                     ti3))
            (setf cr2
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (* tr11 tr2)
                     (* tr12 tr3)))
            (setf ci2
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (* tr11 ti2)
                     (* tr12 ti3)))
            (setf cr3
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (* tr12 tr2)
                     (* tr11 tr3)))
            (setf ci3
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (* tr12 ti2)
                     (* tr11 ti3)))
            (setf (f2cl-lib:fref cc-%data%
                                 (1 k 1 1)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    chold1)
            (setf (f2cl-lib:fref cc-%data%
                                 (2 k 1 1)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    chold2)
            (setf cr5 (+ (* ti11 tr5) (* ti12 tr4)))
            (setf ci5 (+ (* ti11 ti5) (* ti12 ti4)))
            (setf cr4 (- (* ti12 tr5) (* ti11 tr4)))
            (setf ci4 (- (* ti12 ti5) (* ti11 ti4)))
            (setf (f2cl-lib:fref cc-%data%
                                 (1 k 1 2)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    (- cr2 ci5))
            (setf (f2cl-lib:fref cc-%data%
                                 (1 k 1 5)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    (+ cr2 ci5))
            (setf (f2cl-lib:fref cc-%data%
                                 (2 k 1 2)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    (+ ci2 cr5))
            (setf (f2cl-lib:fref cc-%data%
                                 (2 k 1 3)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    (+ ci3 cr4))
            (setf (f2cl-lib:fref cc-%data%
                                 (1 k 1 3)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    (- cr3 ci4))
            (setf (f2cl-lib:fref cc-%data%
                                 (1 k 1 4)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    (+ cr3 ci4))
            (setf (f2cl-lib:fref cc-%data%
                                 (2 k 1 4)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    (- ci3 cr4))
            (setf (f2cl-lib:fref cc-%data%
                                 (2 k 1 5)
                                 ((1 in1) (1 l1) (1 ido) (1 5))
                                 cc-%offset%)
                    (- ci2 cr5))
           label101))
        (go end_label)
       label102
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k l1) nil)
          (tagbody
            (setf ti5
                    (-
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 2)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 5)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf ti2
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 2)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 5)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf ti4
                    (-
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 3)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 4)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf ti3
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 3)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 4)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf tr5
                    (-
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 2)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 5)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf tr2
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 2)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 5)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf tr4
                    (-
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 3)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 4)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf tr3
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 3)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 4)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)))
            (setf (f2cl-lib:fref ch-%data%
                                 (1 k 1 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     tr2
                     tr3))
            (setf (f2cl-lib:fref ch-%data%
                                 (2 k 1 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     ti2
                     ti3))
            (setf cr2
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (* tr11 tr2)
                     (* tr12 tr3)))
            (setf ci2
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (* tr11 ti2)
                     (* tr12 ti3)))
            (setf cr3
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (1 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (* tr12 tr2)
                     (* tr11 tr3)))
            (setf ci3
                    (+
                     (f2cl-lib:fref cc-%data%
                                    (2 k 1 1)
                                    ((1 in1) (1 l1) (1 ido) (1 5))
                                    cc-%offset%)
                     (* tr12 ti2)
                     (* tr11 ti3)))
            (setf cr5 (+ (* ti11 tr5) (* ti12 tr4)))
            (setf ci5 (+ (* ti11 ti5) (* ti12 ti4)))
            (setf cr4 (- (* ti12 tr5) (* ti11 tr4)))
            (setf ci4 (- (* ti12 ti5) (* ti11 ti4)))
            (setf (f2cl-lib:fref ch-%data%
                                 (1 k 2 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (- cr2 ci5))
            (setf (f2cl-lib:fref ch-%data%
                                 (1 k 5 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (+ cr2 ci5))
            (setf (f2cl-lib:fref ch-%data%
                                 (2 k 2 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (+ ci2 cr5))
            (setf (f2cl-lib:fref ch-%data%
                                 (2 k 3 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (+ ci3 cr4))
            (setf (f2cl-lib:fref ch-%data%
                                 (1 k 3 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (- cr3 ci4))
            (setf (f2cl-lib:fref ch-%data%
                                 (1 k 4 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (+ cr3 ci4))
            (setf (f2cl-lib:fref ch-%data%
                                 (2 k 4 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (- ci3 cr4))
            (setf (f2cl-lib:fref ch-%data%
                                 (2 k 5 1)
                                 ((1 in2) (1 l1) (1 5) (1 ido))
                                 ch-%offset%)
                    (- ci2 cr5))
           label103))
        (if (= ido 1) (go end_label))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i ido) nil)
          (tagbody
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k l1) nil)
              (tagbody
                (setf ti5
                        (-
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 2)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 5)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)))
                (setf ti2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 2)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 5)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)))
                (setf ti4
                        (-
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 3)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 4)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)))
                (setf ti3
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 3)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 4)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)))
                (setf tr5
                        (-
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 2)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 5)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)))
                (setf tr2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 2)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 5)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)))
                (setf tr4
                        (-
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 3)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 4)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)))
                (setf tr3
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 3)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 4)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 k 1 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 1)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         tr2
                         tr3))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 k 1 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 1)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         ti2
                         ti3))
                (setf cr2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 1)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (* tr11 tr2)
                         (* tr12 tr3)))
                (setf ci2
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 1)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (* tr11 ti2)
                         (* tr12 ti3)))
                (setf cr3
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (1 k i 1)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (* tr12 tr2)
                         (* tr11 tr3)))
                (setf ci3
                        (+
                         (f2cl-lib:fref cc-%data%
                                        (2 k i 1)
                                        ((1 in1) (1 l1) (1 ido) (1 5))
                                        cc-%offset%)
                         (* tr12 ti2)
                         (* tr11 ti3)))
                (setf cr5 (+ (* ti11 tr5) (* ti12 tr4)))
                (setf ci5 (+ (* ti11 ti5) (* ti12 ti4)))
                (setf cr4 (- (* ti12 tr5) (* ti11 tr4)))
                (setf ci4 (- (* ti12 ti5) (* ti11 ti4)))
                (setf dr3 (- cr3 ci4))
                (setf dr4 (+ cr3 ci4))
                (setf di3 (+ ci3 cr4))
                (setf di4 (- ci3 cr4))
                (setf dr5 (+ cr2 ci5))
                (setf dr2 (- cr2 ci5))
                (setf di5 (- ci2 cr5))
                (setf di2 (+ ci2 cr5))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 k 2 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (-
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 1 1)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          dr2)
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 1 2)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          di2)))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 k 2 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (+
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 1 1)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          di2)
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 1 2)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          dr2)))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 k 3 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (-
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 2 1)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          dr3)
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 2 2)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          di3)))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 k 3 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (+
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 2 1)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          di3)
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 2 2)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          dr3)))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 k 4 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (-
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 3 1)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          dr4)
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 3 2)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          di4)))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 k 4 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (+
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 3 1)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          di4)
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 3 2)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          dr4)))
                (setf (f2cl-lib:fref ch-%data%
                                     (1 k 5 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (-
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 4 1)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          dr5)
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 4 2)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          di5)))
                (setf (f2cl-lib:fref ch-%data%
                                     (2 k 5 i)
                                     ((1 in2) (1 l1) (1 5) (1 ido))
                                     ch-%offset%)
                        (+
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 4 1)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          di5)
                         (*
                          (f2cl-lib:fref wa-%data%
                                         (i 4 2)
                                         ((1 ido) (1 4) (1 2))
                                         wa-%offset%)
                          dr5)))
               label104))
           label105))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::c1f5kb
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil)
           :calls 'nil)))

