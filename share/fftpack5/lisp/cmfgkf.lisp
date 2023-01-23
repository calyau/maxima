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


(defun cmfgkf (lot ido ip l1 lid na cc cc1 im1 in1 ch ch1 im2 in2 wa)
  (declare (type (array double-float (*)) wa ch1 ch cc1 cc)
           (type (f2cl-lib:integer4) in2 im2 in1 im1 na lid l1 ip ido lot))
  (f2cl-lib:with-multi-array-data
      ((cc double-float cc-%data% cc-%offset%)
       (cc1 double-float cc1-%data% cc1-%offset%)
       (ch double-float ch-%data% ch-%offset%)
       (ch1 double-float ch1-%data% ch1-%offset%)
       (wa double-float wa-%data% wa-%offset%))
    (prog ((k 0) (i 0) (chold2 0.0d0) (chold1 0.0d0) (sn 0.0d0) (wai 0.0d0)
           (war 0.0d0) (idlj 0) (lc 0) (l 0) (jc 0) (j 0) (m1 0) (m2 0) (ki 0)
           (ipph 0) (ipp2 0) (m2s 0) (m1d 0))
      (declare (type (double-float) war wai sn chold1 chold2)
               (type (f2cl-lib:integer4) m1d m2s ipp2 ipph ki m2 m1 j jc l lc
                                         idlj i k))
      (setf m1d
              (f2cl-lib:int-add (f2cl-lib:int-mul (f2cl-lib:int-sub lot 1) im1)
                                1))
      (setf m2s (f2cl-lib:int-sub 1 im2))
      (setf ipp2 (f2cl-lib:int-add ip 2))
      (setf ipph (the f2cl-lib:integer4 (truncate (+ ip 1) 2)))
      (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                    ((> ki lid) nil)
        (tagbody
          (setf m2 m2s)
          (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                        ((> m1 m1d) nil)
            (tagbody
              (setf m2 (f2cl-lib:int-add m2 im2))
              (setf (f2cl-lib:fref ch1-%data%
                                   (1 m2 ki 1)
                                   ((1 2) (1 in2) (1 lid) (1 ip))
                                   ch1-%offset%)
                      (f2cl-lib:fref cc1-%data%
                                     (1 m1 ki 1)
                                     ((1 2) (1 in1) (1 lid) (1 ip))
                                     cc1-%offset%))
              (setf (f2cl-lib:fref ch1-%data%
                                   (2 m2 ki 1)
                                   ((1 2) (1 in2) (1 lid) (1 ip))
                                   ch1-%offset%)
                      (f2cl-lib:fref cc1-%data%
                                     (2 m1 ki 1)
                                     ((1 2) (1 in1) (1 lid) (1 ip))
                                     cc1-%offset%))
             label110))))
     label110
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                        ((> ki lid) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (1 m2 ki j)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (+
                           (f2cl-lib:fref cc1-%data%
                                          (1 m1 ki j)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref cc1-%data%
                                          (1 m1 ki jc)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (1 m2 ki jc)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (-
                           (f2cl-lib:fref cc1-%data%
                                          (1 m1 ki j)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref cc1-%data%
                                          (1 m1 ki jc)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (2 m2 ki j)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (+
                           (f2cl-lib:fref cc1-%data%
                                          (2 m1 ki j)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref cc1-%data%
                                          (2 m1 ki jc)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (2 m2 ki jc)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (-
                           (f2cl-lib:fref cc1-%data%
                                          (2 m1 ki j)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref cc1-%data%
                                          (2 m1 ki jc)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)))
                 label112))))
         label112
         label111))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                        ((> ki lid) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref cc1-%data%
                                       (1 m1 ki 1)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          (+
                           (f2cl-lib:fref cc1-%data%
                                          (1 m1 ki 1)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref ch1-%data%
                                          (1 m2 ki j)
                                          ((1 2) (1 in2) (1 lid) (1 ip))
                                          ch1-%offset%)))
                  (setf (f2cl-lib:fref cc1-%data%
                                       (2 m1 ki 1)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          (+
                           (f2cl-lib:fref cc1-%data%
                                          (2 m1 ki 1)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref ch1-%data%
                                          (2 m2 ki j)
                                          ((1 2) (1 in2) (1 lid) (1 ip))
                                          ch1-%offset%)))
                 label117))))
         label117
         label118))
      (f2cl-lib:fdo (l 2 (f2cl-lib:int-add l 1))
                    ((> l ipph) nil)
        (tagbody
          (setf lc (f2cl-lib:int-sub ipp2 l))
          (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                        ((> ki lid) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref cc1-%data%
                                       (1 m1 ki l)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          (+
                           (f2cl-lib:fref ch1-%data%
                                          (1 m2 ki 1)
                                          ((1 2) (1 in2) (1 lid) (1 ip))
                                          ch1-%offset%)
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (1 (f2cl-lib:int-sub l 1) 1)
                                           ((1 ido)
                                            (1
                                             (f2cl-lib:int-add ip
                                                               (f2cl-lib:int-sub
                                                                1)))
                                            (1 2))
                                           wa-%offset%)
                            (f2cl-lib:fref ch1-%data%
                                           (1 m2 ki 2)
                                           ((1 2) (1 in2) (1 lid) (1 ip))
                                           ch1-%offset%))))
                  (setf (f2cl-lib:fref cc1-%data%
                                       (1 m1 ki lc)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          (*
                           (-
                            (f2cl-lib:fref wa-%data%
                                           (1 (f2cl-lib:int-sub l 1) 2)
                                           ((1 ido)
                                            (1
                                             (f2cl-lib:int-add ip
                                                               (f2cl-lib:int-sub
                                                                1)))
                                            (1 2))
                                           wa-%offset%))
                           (f2cl-lib:fref ch1-%data%
                                          (1 m2 ki ip)
                                          ((1 2) (1 in2) (1 lid) (1 ip))
                                          ch1-%offset%)))
                  (setf (f2cl-lib:fref cc1-%data%
                                       (2 m1 ki l)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          (+
                           (f2cl-lib:fref ch1-%data%
                                          (2 m2 ki 1)
                                          ((1 2) (1 in2) (1 lid) (1 ip))
                                          ch1-%offset%)
                           (*
                            (f2cl-lib:fref wa-%data%
                                           (1 (f2cl-lib:int-sub l 1) 1)
                                           ((1 ido)
                                            (1
                                             (f2cl-lib:int-add ip
                                                               (f2cl-lib:int-sub
                                                                1)))
                                            (1 2))
                                           wa-%offset%)
                            (f2cl-lib:fref ch1-%data%
                                           (2 m2 ki 2)
                                           ((1 2) (1 in2) (1 lid) (1 ip))
                                           ch1-%offset%))))
                  (setf (f2cl-lib:fref cc1-%data%
                                       (2 m1 ki lc)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          (*
                           (-
                            (f2cl-lib:fref wa-%data%
                                           (1 (f2cl-lib:int-sub l 1) 2)
                                           ((1 ido)
                                            (1
                                             (f2cl-lib:int-add ip
                                                               (f2cl-lib:int-sub
                                                                1)))
                                            (1 2))
                                           wa-%offset%))
                           (f2cl-lib:fref ch1-%data%
                                          (2 m2 ki ip)
                                          ((1 2) (1 in2) (1 lid) (1 ip))
                                          ch1-%offset%)))
                 label113))))
         label113
          (f2cl-lib:fdo (j 3 (f2cl-lib:int-add j 1))
                        ((> j ipph) nil)
            (tagbody
              (setf jc (f2cl-lib:int-sub ipp2 j))
              (setf idlj
                      (mod
                       (f2cl-lib:int-mul (f2cl-lib:int-sub l 1)
                                         (f2cl-lib:int-sub j 1))
                       ip))
              (setf war
                      (f2cl-lib:fref wa-%data%
                                     (1 idlj 1)
                                     ((1 ido)
                                      (1
                                       (f2cl-lib:int-add ip
                                                         (f2cl-lib:int-sub 1)))
                                      (1 2))
                                     wa-%offset%))
              (setf wai
                      (-
                       (f2cl-lib:fref wa-%data%
                                      (1 idlj 2)
                                      ((1 ido)
                                       (1
                                        (f2cl-lib:int-add ip
                                                          (f2cl-lib:int-sub
                                                           1)))
                                       (1 2))
                                      wa-%offset%)))
              (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                            ((> ki lid) nil)
                (tagbody
                  (setf m2 m2s)
                  (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                                ((> m1 m1d) nil)
                    (tagbody
                      (setf m2 (f2cl-lib:int-add m2 im2))
                      (setf (f2cl-lib:fref cc1-%data%
                                           (1 m1 ki l)
                                           ((1 2) (1 in1) (1 lid) (1 ip))
                                           cc1-%offset%)
                              (+
                               (f2cl-lib:fref cc1-%data%
                                              (1 m1 ki l)
                                              ((1 2) (1 in1) (1 lid) (1 ip))
                                              cc1-%offset%)
                               (* war
                                  (f2cl-lib:fref ch1-%data%
                                                 (1 m2 ki j)
                                                 ((1 2) (1 in2) (1 lid) (1 ip))
                                                 ch1-%offset%))))
                      (setf (f2cl-lib:fref cc1-%data%
                                           (1 m1 ki lc)
                                           ((1 2) (1 in1) (1 lid) (1 ip))
                                           cc1-%offset%)
                              (+
                               (f2cl-lib:fref cc1-%data%
                                              (1 m1 ki lc)
                                              ((1 2) (1 in1) (1 lid) (1 ip))
                                              cc1-%offset%)
                               (* wai
                                  (f2cl-lib:fref ch1-%data%
                                                 (1 m2 ki jc)
                                                 ((1 2) (1 in2) (1 lid) (1 ip))
                                                 ch1-%offset%))))
                      (setf (f2cl-lib:fref cc1-%data%
                                           (2 m1 ki l)
                                           ((1 2) (1 in1) (1 lid) (1 ip))
                                           cc1-%offset%)
                              (+
                               (f2cl-lib:fref cc1-%data%
                                              (2 m1 ki l)
                                              ((1 2) (1 in1) (1 lid) (1 ip))
                                              cc1-%offset%)
                               (* war
                                  (f2cl-lib:fref ch1-%data%
                                                 (2 m2 ki j)
                                                 ((1 2) (1 in2) (1 lid) (1 ip))
                                                 ch1-%offset%))))
                      (setf (f2cl-lib:fref cc1-%data%
                                           (2 m1 ki lc)
                                           ((1 2) (1 in1) (1 lid) (1 ip))
                                           cc1-%offset%)
                              (+
                               (f2cl-lib:fref cc1-%data%
                                              (2 m1 ki lc)
                                              ((1 2) (1 in1) (1 lid) (1 ip))
                                              cc1-%offset%)
                               (* wai
                                  (f2cl-lib:fref ch1-%data%
                                                 (2 m2 ki jc)
                                                 ((1 2) (1 in2) (1 lid) (1 ip))
                                                 ch1-%offset%))))
                     label114))))
             label114
             label115))
         label116))
      (if (> ido 1) (go label136))
      (setf sn (/ 1.0d0 (f2cl-lib:freal (f2cl-lib:int-mul ip l1))))
      (if (= na 1) (go label146))
      (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                    ((> ki lid) nil)
        (tagbody
          (setf m2 m2s)
          (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                        ((> m1 m1d) nil)
            (tagbody
              (setf m2 (f2cl-lib:int-add m2 im2))
              (setf (f2cl-lib:fref cc1-%data%
                                   (1 m1 ki 1)
                                   ((1 2) (1 in1) (1 lid) (1 ip))
                                   cc1-%offset%)
                      (* sn
                         (f2cl-lib:fref cc1-%data%
                                        (1 m1 ki 1)
                                        ((1 2) (1 in1) (1 lid) (1 ip))
                                        cc1-%offset%)))
              (setf (f2cl-lib:fref cc1-%data%
                                   (2 m1 ki 1)
                                   ((1 2) (1 in1) (1 lid) (1 ip))
                                   cc1-%offset%)
                      (* sn
                         (f2cl-lib:fref cc1-%data%
                                        (2 m1 ki 1)
                                        ((1 2) (1 in1) (1 lid) (1 ip))
                                        cc1-%offset%)))
             label149))))
     label149
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                        ((> ki lid) nil)
            (tagbody
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf chold1
                          (* sn
                             (-
                              (f2cl-lib:fref cc1-%data%
                                             (1 m1 ki j)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%)
                              (f2cl-lib:fref cc1-%data%
                                             (2 m1 ki jc)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%))))
                  (setf chold2
                          (* sn
                             (+
                              (f2cl-lib:fref cc1-%data%
                                             (1 m1 ki j)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%)
                              (f2cl-lib:fref cc1-%data%
                                             (2 m1 ki jc)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%))))
                  (setf (f2cl-lib:fref cc1-%data%
                                       (1 m1 ki j)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          chold1)
                  (setf (f2cl-lib:fref cc1-%data%
                                       (2 m1 ki jc)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          (* sn
                             (-
                              (f2cl-lib:fref cc1-%data%
                                             (2 m1 ki j)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%)
                              (f2cl-lib:fref cc1-%data%
                                             (1 m1 ki jc)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%))))
                  (setf (f2cl-lib:fref cc1-%data%
                                       (2 m1 ki j)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          (* sn
                             (+
                              (f2cl-lib:fref cc1-%data%
                                             (2 m1 ki j)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%)
                              (f2cl-lib:fref cc1-%data%
                                             (1 m1 ki jc)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%))))
                  (setf (f2cl-lib:fref cc1-%data%
                                       (1 m1 ki jc)
                                       ((1 2) (1 in1) (1 lid) (1 ip))
                                       cc1-%offset%)
                          chold2)
                 label119))))
         label119
         label120))
      (go end_label)
     label146
      (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                    ((> ki lid) nil)
        (tagbody
          (setf m2 m2s)
          (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                        ((> m1 m1d) nil)
            (tagbody
              (setf m2 (f2cl-lib:int-add m2 im2))
              (setf (f2cl-lib:fref ch1-%data%
                                   (1 m2 ki 1)
                                   ((1 2) (1 in2) (1 lid) (1 ip))
                                   ch1-%offset%)
                      (* sn
                         (f2cl-lib:fref cc1-%data%
                                        (1 m1 ki 1)
                                        ((1 2) (1 in1) (1 lid) (1 ip))
                                        cc1-%offset%)))
              (setf (f2cl-lib:fref ch1-%data%
                                   (2 m2 ki 1)
                                   ((1 2) (1 in2) (1 lid) (1 ip))
                                   ch1-%offset%)
                      (* sn
                         (f2cl-lib:fref cc1-%data%
                                        (2 m1 ki 1)
                                        ((1 2) (1 in1) (1 lid) (1 ip))
                                        cc1-%offset%)))
             label147))))
     label147
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                        ((> ki lid) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (1 m2 ki j)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (* sn
                             (-
                              (f2cl-lib:fref cc1-%data%
                                             (1 m1 ki j)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%)
                              (f2cl-lib:fref cc1-%data%
                                             (2 m1 ki jc)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%))))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (2 m2 ki j)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (* sn
                             (+
                              (f2cl-lib:fref cc1-%data%
                                             (2 m1 ki j)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%)
                              (f2cl-lib:fref cc1-%data%
                                             (1 m1 ki jc)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%))))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (1 m2 ki jc)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (* sn
                             (+
                              (f2cl-lib:fref cc1-%data%
                                             (1 m1 ki j)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%)
                              (f2cl-lib:fref cc1-%data%
                                             (2 m1 ki jc)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%))))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (2 m2 ki jc)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (* sn
                             (-
                              (f2cl-lib:fref cc1-%data%
                                             (2 m1 ki j)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%)
                              (f2cl-lib:fref cc1-%data%
                                             (1 m1 ki jc)
                                             ((1 2) (1 in1) (1 lid) (1 ip))
                                             cc1-%offset%))))
                 label144))))
         label144
         label145))
      (go end_label)
     label136
      (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                    ((> ki lid) nil)
        (tagbody
          (setf m2 m2s)
          (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                        ((> m1 m1d) nil)
            (tagbody
              (setf m2 (f2cl-lib:int-add m2 im2))
              (setf (f2cl-lib:fref ch1-%data%
                                   (1 m2 ki 1)
                                   ((1 2) (1 in2) (1 lid) (1 ip))
                                   ch1-%offset%)
                      (f2cl-lib:fref cc1-%data%
                                     (1 m1 ki 1)
                                     ((1 2) (1 in1) (1 lid) (1 ip))
                                     cc1-%offset%))
              (setf (f2cl-lib:fref ch1-%data%
                                   (2 m2 ki 1)
                                   ((1 2) (1 in2) (1 lid) (1 ip))
                                   ch1-%offset%)
                      (f2cl-lib:fref cc1-%data%
                                     (2 m1 ki 1)
                                     ((1 2) (1 in1) (1 lid) (1 ip))
                                     cc1-%offset%))
             label137))))
     label137
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ipph) nil)
        (tagbody
          (setf jc (f2cl-lib:int-sub ipp2 j))
          (f2cl-lib:fdo (ki 1 (f2cl-lib:int-add ki 1))
                        ((> ki lid) nil)
            (tagbody
              (setf m2 m2s)
              (f2cl-lib:fdo (m1 1 (f2cl-lib:int-add m1 im1))
                            ((> m1 m1d) nil)
                (tagbody
                  (setf m2 (f2cl-lib:int-add m2 im2))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (1 m2 ki j)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (-
                           (f2cl-lib:fref cc1-%data%
                                          (1 m1 ki j)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref cc1-%data%
                                          (2 m1 ki jc)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (2 m2 ki j)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (+
                           (f2cl-lib:fref cc1-%data%
                                          (2 m1 ki j)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref cc1-%data%
                                          (1 m1 ki jc)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (1 m2 ki jc)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (+
                           (f2cl-lib:fref cc1-%data%
                                          (1 m1 ki j)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref cc1-%data%
                                          (2 m1 ki jc)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)))
                  (setf (f2cl-lib:fref ch1-%data%
                                       (2 m2 ki jc)
                                       ((1 2) (1 in2) (1 lid) (1 ip))
                                       ch1-%offset%)
                          (-
                           (f2cl-lib:fref cc1-%data%
                                          (2 m1 ki j)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)
                           (f2cl-lib:fref cc1-%data%
                                          (1 m1 ki jc)
                                          ((1 2) (1 in1) (1 lid) (1 ip))
                                          cc1-%offset%)))
                 label134))))
         label134
         label135))
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
                                       (1 m1 k 1 i)
                                       ((1 2) (1 in1) (1 l1) (1 ip) (1 ido))
                                       cc-%offset%)
                          (f2cl-lib:fref ch-%data%
                                         (1 m2 k i 1)
                                         ((1 2) (1 in2) (1 l1) (1 ido) (1 ip))
                                         ch-%offset%))
                  (setf (f2cl-lib:fref cc-%data%
                                       (2 m1 k 1 i)
                                       ((1 2) (1 in1) (1 l1) (1 ip) (1 ido))
                                       cc-%offset%)
                          (f2cl-lib:fref ch-%data%
                                         (2 m2 k i 1)
                                         ((1 2) (1 in2) (1 l1) (1 ido) (1 ip))
                                         ch-%offset%))
                 label130))))
         label130
         label131))
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
                  (setf (f2cl-lib:fref cc-%data%
                                       (1 m1 k j 1)
                                       ((1 2) (1 in1) (1 l1) (1 ip) (1 ido))
                                       cc-%offset%)
                          (f2cl-lib:fref ch-%data%
                                         (1 m2 k 1 j)
                                         ((1 2) (1 in2) (1 l1) (1 ido) (1 ip))
                                         ch-%offset%))
                  (setf (f2cl-lib:fref cc-%data%
                                       (2 m1 k j 1)
                                       ((1 2) (1 in1) (1 l1) (1 ip) (1 ido))
                                       cc-%offset%)
                          (f2cl-lib:fref ch-%data%
                                         (2 m2 k 1 j)
                                         ((1 2) (1 in2) (1 l1) (1 ido) (1 ip))
                                         ch-%offset%))
                 label122))))
         label122
         label123))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j ip) nil)
        (tagbody
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
                      (setf (f2cl-lib:fref cc-%data%
                                           (1 m1 k j i)
                                           ((1 2) (1 in1) (1 l1) (1 ip)
                                            (1 ido))
                                           cc-%offset%)
                              (+
                               (*
                                (f2cl-lib:fref wa-%data%
                                               (i (f2cl-lib:int-sub j 1) 1)
                                               ((1 ido)
                                                (1
                                                 (f2cl-lib:int-add ip
                                                                   (f2cl-lib:int-sub
                                                                    1)))
                                                (1 2))
                                               wa-%offset%)
                                (f2cl-lib:fref ch-%data%
                                               (1 m2 k i j)
                                               ((1 2) (1 in2) (1 l1) (1 ido)
                                                (1 ip))
                                               ch-%offset%))
                               (*
                                (f2cl-lib:fref wa-%data%
                                               (i (f2cl-lib:int-sub j 1) 2)
                                               ((1 ido)
                                                (1
                                                 (f2cl-lib:int-add ip
                                                                   (f2cl-lib:int-sub
                                                                    1)))
                                                (1 2))
                                               wa-%offset%)
                                (f2cl-lib:fref ch-%data%
                                               (2 m2 k i j)
                                               ((1 2) (1 in2) (1 l1) (1 ido)
                                                (1 ip))
                                               ch-%offset%))))
                      (setf (f2cl-lib:fref cc-%data%
                                           (2 m1 k j i)
                                           ((1 2) (1 in1) (1 l1) (1 ip)
                                            (1 ido))
                                           cc-%offset%)
                              (-
                               (*
                                (f2cl-lib:fref wa-%data%
                                               (i (f2cl-lib:int-sub j 1) 1)
                                               ((1 ido)
                                                (1
                                                 (f2cl-lib:int-add ip
                                                                   (f2cl-lib:int-sub
                                                                    1)))
                                                (1 2))
                                               wa-%offset%)
                                (f2cl-lib:fref ch-%data%
                                               (2 m2 k i j)
                                               ((1 2) (1 in2) (1 l1) (1 ido)
                                                (1 ip))
                                               ch-%offset%))
                               (*
                                (f2cl-lib:fref wa-%data%
                                               (i (f2cl-lib:int-sub j 1) 2)
                                               ((1 ido)
                                                (1
                                                 (f2cl-lib:int-add ip
                                                                   (f2cl-lib:int-sub
                                                                    1)))
                                                (1 2))
                                               wa-%offset%)
                                (f2cl-lib:fref ch-%data%
                                               (1 m2 k i j)
                                               ((1 2) (1 in2) (1 l1) (1 ido)
                                                (1 ip))
                                               ch-%offset%))))
                     label124))))
             label124
             label125))
         label126))
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cmfgkf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil)
           :calls 'nil)))

