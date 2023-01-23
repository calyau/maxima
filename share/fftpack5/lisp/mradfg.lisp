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


(defun mradfg (m ido ip l1 idl1 cc c1 c2 im1 in1 ch ch2 im2 in2 wa)
  (declare (type (array double-float (*)) wa ch2 ch c2 c1 cc)
           (type (f2cl-lib:integer4) in2 im2 in1 im1 idl1 l1 ip ido m))
  (f2cl-lib:with-multi-array-data
      ((cc double-float cc-%data% cc-%offset%)
       (c1 double-float c1-%data% c1-%offset%)
       (c2 double-float c2-%data% c2-%offset%)
       (ch double-float ch-%data% ch-%offset%)
       (ch2 double-float ch2-%data% ch2-%offset%)
       (wa double-float wa-%data% wa-%offset%))
    (prog ((ic 0) (j2 0) (ar2h 0.0d0) (ai2 0.0d0) (ar2 0.0d0) (ds2 0.0d0)
           (dc2 0.0d0) (ar1h 0.0d0) (lc 0) (l 0) (ai1 0.0d0) (ar1 0.0d0) (jc 0)
           (i 0) (idij 0) (is 0) (k 0) (j 0) (m1 0) (m2 0) (ik 0) (nbd 0)
           (idp2 0) (ipp2 0) (ipph 0) (dsp 0.0d0) (dcp 0.0d0) (arg 0.0d0)
           (tpi 0.0d0) (m2s 0) (m1d 0))
      (declare (type (double-float) tpi arg dcp dsp ar1 ai1 ar1h dc2 ds2 ar2
                                    ai2 ar2h)
               (type (f2cl-lib:integer4) m1d m2s ipph ipp2 idp2 nbd ik m2 m1 j
                                         k is idij i jc l lc j2 ic))
      (setf m1d
              (f2cl-lib:int-add (f2cl-lib:int-mul (f2cl-lib:int-sub m 1) im1)
                                1))
      (setf m2s (f2cl-lib:int-sub 1 im2))
      (setf tpi (* 2.0d0 4.0d0 (atan 1.0d0)))
      (setf arg (/ tpi (f2cl-lib:ffloat ip)))
      (setf dcp (cos arg))
      (setf dsp (sin arg))
      (setf ipph (the f2cl-lib:integer4 (truncate (+ ip 1) 2)))
      (setf ipp2 (f2cl-lib:int-add ip 2))
      (setf idp2 (f2cl-lib:int-add ido 2))
      (setf nbd (the f2cl-lib:integer4 (truncate (- ido 1) 2)))
      (if (= ido 1) (go label119))
      (f2cl-lib:fdo (ik 1 (f2cl-lib:int-add ik 1))
                    ((> ik idl1) nil)
        (tagbody
          (setf m2 m2s)
          (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                        ((> m1 m1d) nil)
            (tagbody
              (setf m2 (f2cl-lib:int-add m2 im2))
              (setf (f2cl-lib:fref ch2-%data%
                                   (m2 ik 1)
                                   ((1 in2) (1 idl1) (1 ip))
                                   ch2-%offset%)
                      (f2cl-lib:fref c2-%data%
                                     (m1 ik 1)
                                     ((1 in1) (1 idl1) (1 ip))
                                     c2-%offset%))
             label1001))
         label101))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ip) nil)
        (tagbody
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k l1) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref ch-%data%
                                       (m2 1 k j)
                                       ((1 in2) (1 ido) (1 l1) (1 ip))
                                       ch-%offset%)
                          (f2cl-lib:fref c1-%data%
                                         (m1 1 k j)
                                         ((1 in1) (1 ido) (1 l1) (1 ip))
                                         c1-%offset%))
                 label1002))
             label102))
         label103))
      (if (> nbd l1) (go label107))
      (setf is (f2cl-lib:int-sub ido))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ip) nil)
        (tagbody
          (setf is (f2cl-lib:int-add is ido))
          (setf idij is)
          (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                        ((> i ido) nil)
            (tagbody
              (setf idij (f2cl-lib:int-add idij 2))
              (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                            ((> k l1) nil)
                (tagbody
                  (setf m2 m2s)
                  (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                                ((> m1 m1d) nil)
                    (tagbody
                      (setf m2 (f2cl-lib:int-add m2 im2))
                      (setf (f2cl-lib:fref ch-%data%
                                           (m2 (f2cl-lib:int-sub i 1) k j)
                                           ((1 in2) (1 ido) (1 l1) (1 ip))
                                           ch-%offset%)
                              (+
                               (*
                                (f2cl-lib:fref wa-%data%
                                               ((f2cl-lib:int-sub idij 1))
                                               ((1 ido))
                                               wa-%offset%)
                                (f2cl-lib:fref c1-%data%
                                               (m1 (f2cl-lib:int-sub i 1) k j)
                                               ((1 in1) (1 ido) (1 l1) (1 ip))
                                               c1-%offset%))
                               (*
                                (f2cl-lib:fref wa-%data%
                                               (idij)
                                               ((1 ido))
                                               wa-%offset%)
                                (f2cl-lib:fref c1-%data%
                                               (m1 i k j)
                                               ((1 in1) (1 ido) (1 l1) (1 ip))
                                               c1-%offset%))))
                      (setf (f2cl-lib:fref ch-%data%
                                           (m2 i k j)
                                           ((1 in2) (1 ido) (1 l1) (1 ip))
                                           ch-%offset%)
                              (-
                               (*
                                (f2cl-lib:fref wa-%data%
                                               ((f2cl-lib:int-sub idij 1))
                                               ((1 ido))
                                               wa-%offset%)
                                (f2cl-lib:fref c1-%data%
                                               (m1 i k j)
                                               ((1 in1) (1 ido) (1 l1) (1 ip))
                                               c1-%offset%))
                               (*
                                (f2cl-lib:fref wa-%data%
                                               (idij)
                                               ((1 ido))
                                               wa-%offset%)
                                (f2cl-lib:fref c1-%data%
                                               (m1 (f2cl-lib:int-sub i 1) k j)
                                               ((1 in1) (1 ido) (1 l1) (1 ip))
                                               c1-%offset%))))
                     label1004))
                 label104))
             label105))
         label106))
      (go label111)
     label107
      (setf is (f2cl-lib:int-sub ido))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ip) nil)
        (tagbody
          (setf is (f2cl-lib:int-add is ido))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k l1) nil)
            (tagbody
              (setf idij is)
              (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                            ((> i ido) nil)
                (tagbody
                  (setf idij (f2cl-lib:int-add idij 2))
                  (setf m2 m2s)
                  (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                                ((> m1 m1d) nil)
                    (tagbody
                      (setf m2 (f2cl-lib:int-add m2 im2))
                      (setf (f2cl-lib:fref ch-%data%
                                           (m2 (f2cl-lib:int-sub i 1) k j)
                                           ((1 in2) (1 ido) (1 l1) (1 ip))
                                           ch-%offset%)
                              (+
                               (*
                                (f2cl-lib:fref wa-%data%
                                               ((f2cl-lib:int-sub idij 1))
                                               ((1 ido))
                                               wa-%offset%)
                                (f2cl-lib:fref c1-%data%
                                               (m1 (f2cl-lib:int-sub i 1) k j)
                                               ((1 in1) (1 ido) (1 l1) (1 ip))
                                               c1-%offset%))
                               (*
                                (f2cl-lib:fref wa-%data%
                                               (idij)
                                               ((1 ido))
                                               wa-%offset%)
                                (f2cl-lib:fref c1-%data%
                                               (m1 i k j)
                                               ((1 in1) (1 ido) (1 l1) (1 ip))
                                               c1-%offset%))))
                      (setf (f2cl-lib:fref ch-%data%
                                           (m2 i k j)
                                           ((1 in2) (1 ido) (1 l1) (1 ip))
                                           ch-%offset%)
                              (-
                               (*
                                (f2cl-lib:fref wa-%data%
                                               ((f2cl-lib:int-sub idij 1))
                                               ((1 ido))
                                               wa-%offset%)
                                (f2cl-lib:fref c1-%data%
                                               (m1 i k j)
                                               ((1 in1) (1 ido) (1 l1) (1 ip))
                                               c1-%offset%))
                               (*
                                (f2cl-lib:fref wa-%data%
                                               (idij)
                                               ((1 ido))
                                               wa-%offset%)
                                (f2cl-lib:fref c1-%data%
                                               (m1 (f2cl-lib:int-sub i 1) k j)
                                               ((1 in1) (1 ido) (1 l1) (1 ip))
                                               c1-%offset%))))
                     label1008))
                 label108))
             label109))
         label110))
     label111
      (if (< nbd l1) (go label115))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k l1) nil)
            (tagbody
              (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                            ((> i ido) nil)
                (tagbody
                  (setf m2 m2s)
                  (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                                ((> m1 m1d) nil)
                    (tagbody
                      (setf m2 (f2cl-lib:int-add m2 im2))
                      (setf (f2cl-lib:fref c1-%data%
                                           (m1 (f2cl-lib:int-sub i 1) k j)
                                           ((1 in1) (1 ido) (1 l1) (1 ip))
                                           c1-%offset%)
                              (+
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref c1-%data%
                                           (m1 (f2cl-lib:int-sub i 1) k jc)
                                           ((1 in1) (1 ido) (1 l1) (1 ip))
                                           c1-%offset%)
                              (-
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref c1-%data%
                                           (m1 i k j)
                                           ((1 in1) (1 ido) (1 l1) (1 ip))
                                           c1-%offset%)
                              (+
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref c1-%data%
                                           (m1 i k jc)
                                           ((1 in1) (1 ido) (1 l1) (1 ip))
                                           c1-%offset%)
                              (-
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                     label1012))
                 label112))
             label113))
         label114))
      (go label121)
     label115
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
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
                      (setf (f2cl-lib:fref c1-%data%
                                           (m1 (f2cl-lib:int-sub i 1) k j)
                                           ((1 in1) (1 ido) (1 l1) (1 ip))
                                           c1-%offset%)
                              (+
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref c1-%data%
                                           (m1 (f2cl-lib:int-sub i 1) k jc)
                                           ((1 in1) (1 ido) (1 l1) (1 ip))
                                           c1-%offset%)
                              (-
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref c1-%data%
                                           (m1 i k j)
                                           ((1 in1) (1 ido) (1 l1) (1 ip))
                                           c1-%offset%)
                              (+
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref c1-%data%
                                           (m1 i k jc)
                                           ((1 in1) (1 ido) (1 l1) (1 ip))
                                           c1-%offset%)
                              (-
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                     label1016))
                 label116))
             label117))
         label118))
      (go label121)
     label119
      (f2cl-lib:fdo (ik 1 (f2cl-lib:int-add ik 1))
                    ((> ik idl1) nil)
        (tagbody
          (setf m2 m2s)
          (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                        ((> m1 m1d) nil)
            (tagbody
              (setf m2 (f2cl-lib:int-add m2 im2))
              (setf (f2cl-lib:fref c2-%data%
                                   (m1 ik 1)
                                   ((1 in1) (1 idl1) (1 ip))
                                   c2-%offset%)
                      (f2cl-lib:fref ch2-%data%
                                     (m2 ik 1)
                                     ((1 in2) (1 idl1) (1 ip))
                                     ch2-%offset%))
             label1020))
         label120))
     label121
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k l1) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref c1-%data%
                                       (m1 1 k j)
                                       ((1 in1) (1 ido) (1 l1) (1 ip))
                                       c1-%offset%)
                          (+
                           (f2cl-lib:fref ch-%data%
                                          (m2 1 k j)
                                          ((1 in2) (1 ido) (1 l1) (1 ip))
                                          ch-%offset%)
                           (f2cl-lib:fref ch-%data%
                                          (m2 1 k jc)
                                          ((1 in2) (1 ido) (1 l1) (1 ip))
                                          ch-%offset%)))
                  (setf (f2cl-lib:fref c1-%data%
                                       (m1 1 k jc)
                                       ((1 in1) (1 ido) (1 l1) (1 ip))
                                       c1-%offset%)
                          (-
                           (f2cl-lib:fref ch-%data%
                                          (m2 1 k jc)
                                          ((1 in2) (1 ido) (1 l1) (1 ip))
                                          ch-%offset%)
                           (f2cl-lib:fref ch-%data%
                                          (m2 1 k j)
                                          ((1 in2) (1 ido) (1 l1) (1 ip))
                                          ch-%offset%)))
                 label1022))
             label122))
         label123))
      (setf ar1 1.0d0)
      (setf ai1 0.0d0)
      (f2cl-lib:fdo (l 2 (f2cl-lib:int-add l 1))
                    ((> l ipph) nil)
        (tagbody
          (setf lc (f2cl-lib:int-sub ipp2 l))
          (setf ar1h (- (* dcp ar1) (* dsp ai1)))
          (setf ai1 (+ (* dcp ai1) (* dsp ar1)))
          (setf ar1 ar1h)
          (f2cl-lib:fdo (ik 1 (f2cl-lib:int-add ik 1))
                        ((> ik idl1) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref ch2-%data%
                                       (m2 ik l)
                                       ((1 in2) (1 idl1) (1 ip))
                                       ch2-%offset%)
                          (+
                           (f2cl-lib:fref c2-%data%
                                          (m1 ik 1)
                                          ((1 in1) (1 idl1) (1 ip))
                                          c2-%offset%)
                           (* ar1
                              (f2cl-lib:fref c2-%data%
                                             (m1 ik 2)
                                             ((1 in1) (1 idl1) (1 ip))
                                             c2-%offset%))))
                  (setf (f2cl-lib:fref ch2-%data%
                                       (m2 ik lc)
                                       ((1 in2) (1 idl1) (1 ip))
                                       ch2-%offset%)
                          (* ai1
                             (f2cl-lib:fref c2-%data%
                                            (m1 ik ip)
                                            ((1 in1) (1 idl1) (1 ip))
                                            c2-%offset%)))
                 label1024))
             label124))
          (setf dc2 ar1)
          (setf ds2 ai1)
          (setf ar2 ar1)
          (setf ai2 ai1)
          (f2cl-lib:fdo (j 3 (f2cl-lib:int-add j 1))
                        ((> j ipph) nil)
            (tagbody
              (setf jc (f2cl-lib:int-sub ipp2 j))
              (setf ar2h (- (* dc2 ar2) (* ds2 ai2)))
              (setf ai2 (+ (* dc2 ai2) (* ds2 ar2)))
              (setf ar2 ar2h)
              (f2cl-lib:fdo (ik 1 (f2cl-lib:int-add ik 1))
                            ((> ik idl1) nil)
                (tagbody
                  (setf m2 m2s)
                  (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                                ((> m1 m1d) nil)
                    (tagbody
                      (setf m2 (f2cl-lib:int-add m2 im2))
                      (setf (f2cl-lib:fref ch2-%data%
                                           (m2 ik l)
                                           ((1 in2) (1 idl1) (1 ip))
                                           ch2-%offset%)
                              (+
                               (f2cl-lib:fref ch2-%data%
                                              (m2 ik l)
                                              ((1 in2) (1 idl1) (1 ip))
                                              ch2-%offset%)
                               (* ar2
                                  (f2cl-lib:fref c2-%data%
                                                 (m1 ik j)
                                                 ((1 in1) (1 idl1) (1 ip))
                                                 c2-%offset%))))
                      (setf (f2cl-lib:fref ch2-%data%
                                           (m2 ik lc)
                                           ((1 in2) (1 idl1) (1 ip))
                                           ch2-%offset%)
                              (+
                               (f2cl-lib:fref ch2-%data%
                                              (m2 ik lc)
                                              ((1 in2) (1 idl1) (1 ip))
                                              ch2-%offset%)
                               (* ai2
                                  (f2cl-lib:fref c2-%data%
                                                 (m1 ik jc)
                                                 ((1 in1) (1 idl1) (1 ip))
                                                 c2-%offset%))))
                     label1025))
                 label125))
             label126))
         label127))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (f2cl-lib:fdo (ik 1 (f2cl-lib:int-add ik 1))
                        ((> ik idl1) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref ch2-%data%
                                       (m2 ik 1)
                                       ((1 in2) (1 idl1) (1 ip))
                                       ch2-%offset%)
                          (+
                           (f2cl-lib:fref ch2-%data%
                                          (m2 ik 1)
                                          ((1 in2) (1 idl1) (1 ip))
                                          ch2-%offset%)
                           (f2cl-lib:fref c2-%data%
                                          (m1 ik j)
                                          ((1 in1) (1 idl1) (1 ip))
                                          c2-%offset%)))
                 label1028))
             label128))
         label129))
      (if (< ido l1) (go label132))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k l1) nil)
        (tagbody
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i ido) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref cc-%data%
                                       (m1 i 1 k)
                                       ((1 in1) (1 ido) (1 ip) (1 l1))
                                       cc-%offset%)
                          (f2cl-lib:fref ch-%data%
                                         (m2 i k 1)
                                         ((1 in2) (1 ido) (1 l1) (1 ip))
                                         ch-%offset%))
                 label1030))
             label130))
         label131))
      (go label135)
     label132
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
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
                  (setf (f2cl-lib:fref cc-%data%
                                       (m1 i 1 k)
                                       ((1 in1) (1 ido) (1 ip) (1 l1))
                                       cc-%offset%)
                          (f2cl-lib:fref ch-%data%
                                         (m2 i k 1)
                                         ((1 in2) (1 ido) (1 l1) (1 ip))
                                         ch-%offset%))
                 label1033))
             label133))
         label134))
     label135
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (setf j2 (f2cl-lib:int-add j j))
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k l1) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref cc-%data%
                                       (m1 ido (f2cl-lib:int-sub j2 2) k)
                                       ((1 in1) (1 ido) (1 ip) (1 l1))
                                       cc-%offset%)
                          (f2cl-lib:fref ch-%data%
                                         (m2 1 k j)
                                         ((1 in2) (1 ido) (1 l1) (1 ip))
                                         ch-%offset%))
                  (setf (f2cl-lib:fref cc-%data%
                                       (m1 1 (f2cl-lib:int-sub j2 1) k)
                                       ((1 in1) (1 ido) (1 ip) (1 l1))
                                       cc-%offset%)
                          (f2cl-lib:fref ch-%data%
                                         (m2 1 k jc)
                                         ((1 in2) (1 ido) (1 l1) (1 ip))
                                         ch-%offset%))
                 label1036))
             label136))
         label137))
      (if (= ido 1) (go end_label))
      (if (< nbd l1) (go label141))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (setf j2 (f2cl-lib:int-add j j))
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
                      (setf (f2cl-lib:fref cc-%data%
                                           (m1 (f2cl-lib:int-sub i 1)
                                            (f2cl-lib:int-sub j2 1) k)
                                           ((1 in1) (1 ido) (1 ip) (1 l1))
                                           cc-%offset%)
                              (+
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref cc-%data%
                                           (m1 (f2cl-lib:int-sub ic 1)
                                            (f2cl-lib:int-sub j2 2) k)
                                           ((1 in1) (1 ido) (1 ip) (1 l1))
                                           cc-%offset%)
                              (-
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref cc-%data%
                                           (m1 i (f2cl-lib:int-sub j2 1) k)
                                           ((1 in1) (1 ido) (1 ip) (1 l1))
                                           cc-%offset%)
                              (+
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref cc-%data%
                                           (m1 ic (f2cl-lib:int-sub j2 2) k)
                                           ((1 in1) (1 ido) (1 ip) (1 l1))
                                           cc-%offset%)
                              (-
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                     label1038))
                 label138))
             label139))
         label140))
      (go end_label)
     label141
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (setf j2 (f2cl-lib:int-add j j))
          (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                        ((> i ido) nil)
            (tagbody
              (setf ic (f2cl-lib:int-sub idp2 i))
              (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                            ((> k l1) nil)
                (tagbody
                  (setf m2 m2s)
                  (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                                ((> m1 m1d) nil)
                    (tagbody
                      (setf m2 (f2cl-lib:int-add m2 im2))
                      (setf (f2cl-lib:fref cc-%data%
                                           (m1 (f2cl-lib:int-sub i 1)
                                            (f2cl-lib:int-sub j2 1) k)
                                           ((1 in1) (1 ido) (1 ip) (1 l1))
                                           cc-%offset%)
                              (+
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref cc-%data%
                                           (m1 (f2cl-lib:int-sub ic 1)
                                            (f2cl-lib:int-sub j2 2) k)
                                           ((1 in1) (1 ido) (1 ip) (1 l1))
                                           cc-%offset%)
                              (-
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 (f2cl-lib:int-sub i 1) k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref cc-%data%
                                           (m1 i (f2cl-lib:int-sub j2 1) k)
                                           ((1 in1) (1 ido) (1 ip) (1 l1))
                                           cc-%offset%)
                              (+
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                      (setf (f2cl-lib:fref cc-%data%
                                           (m1 ic (f2cl-lib:int-sub j2 2) k)
                                           ((1 in1) (1 ido) (1 ip) (1 l1))
                                           cc-%offset%)
                              (-
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k jc)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)
                               (f2cl-lib:fref ch-%data%
                                              (m2 i k j)
                                              ((1 in2) (1 ido) (1 l1) (1 ip))
                                              ch-%offset%)))
                     label1042))
                 label142))
             label143))
         label144))
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mradfg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil)
           :calls 'nil)))

