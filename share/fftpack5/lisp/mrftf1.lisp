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


(defun mrftf1 (m im n in c ch wa fac)
  (declare (type (array double-float (*)) fac)
           (type (array double-float (*)) wa ch c)
           (type (f2cl-lib:integer4) in n im m))
  (f2cl-lib:with-multi-array-data
      ((c double-float c-%data% c-%offset%)
       (ch double-float ch-%data% ch-%offset%)
       (wa double-float wa-%data% wa-%offset%)
       (fac double-float fac-%data% fac-%offset%))
    (prog ((j 0) (i 0) (m2 0) (nl 0) (modn 0) (tsnm 0.0d0) (tsn 0.0d0)
           (sn 0.0d0) (ix4 0) (ix3 0) (ix2 0) (idl1 0) (ido 0) (l1 0) (ip 0)
           (kh 0) (k1 0) (iw 0) (l2 0) (na 0) (nf 0))
      (declare (type (double-float) sn tsn tsnm)
               (type (f2cl-lib:integer4) nf na l2 iw k1 kh ip l1 ido idl1 ix2
                                         ix3 ix4 modn nl m2 i j))
      (setf nf
              (f2cl-lib:int
               (f2cl-lib:fref fac-%data% (2) ((1 15)) fac-%offset%)))
      (setf na 1)
      (setf l2 n)
      (setf iw n)
      (f2cl-lib:fdo (k1 1 (f2cl-lib:int-add k1 1))
                    ((> k1 nf) nil)
        (tagbody
          (setf kh (f2cl-lib:int-sub nf k1))
          (setf ip
                  (f2cl-lib:int
                   (f2cl-lib:fref fac-%data%
                                  ((f2cl-lib:int-add kh 3))
                                  ((1 15))
                                  fac-%offset%)))
          (setf l1 (the f2cl-lib:integer4 (truncate l2 ip)))
          (setf ido (the f2cl-lib:integer4 (truncate n l2)))
          (setf idl1 (f2cl-lib:int-mul ido l1))
          (setf iw
                  (f2cl-lib:int-sub iw
                                    (f2cl-lib:int-mul (f2cl-lib:int-sub ip 1)
                                                      ido)))
          (setf na (f2cl-lib:int-sub 1 na))
          (if (/= ip 4) (go label102))
          (setf ix2 (f2cl-lib:int-add iw ido))
          (setf ix3 (f2cl-lib:int-add ix2 ido))
          (if (/= na 0) (go label101))
          (mradf4 m ido l1 c im in ch 1 m
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix2)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix3)
                                 ((1 n))
                                 wa-%offset%))
          (go label110)
         label101
          (mradf4 m ido l1 ch 1 m c im in
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix2)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix3)
                                 ((1 n))
                                 wa-%offset%))
          (go label110)
         label102
          (if (/= ip 2) (go label104))
          (if (/= na 0) (go label103))
          (mradf2 m ido l1 c im in ch 1 m
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%))
          (go label110)
         label103
          (mradf2 m ido l1 ch 1 m c im in
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%))
          (go label110)
         label104
          (if (/= ip 3) (go label106))
          (setf ix2 (f2cl-lib:int-add iw ido))
          (if (/= na 0) (go label105))
          (mradf3 m ido l1 c im in ch 1 m
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix2)
                                 ((1 n))
                                 wa-%offset%))
          (go label110)
         label105
          (mradf3 m ido l1 ch 1 m c im in
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix2)
                                 ((1 n))
                                 wa-%offset%))
          (go label110)
         label106
          (if (/= ip 5) (go label108))
          (setf ix2 (f2cl-lib:int-add iw ido))
          (setf ix3 (f2cl-lib:int-add ix2 ido))
          (setf ix4 (f2cl-lib:int-add ix3 ido))
          (if (/= na 0) (go label107))
          (mradf5 m ido l1 c im in ch 1 m
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix2)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix3)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix4)
                                 ((1 n))
                                 wa-%offset%))
          (go label110)
         label107
          (mradf5 m ido l1 ch 1 m c im in
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix2)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix3)
                                 ((1 n))
                                 wa-%offset%)
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (ix4)
                                 ((1 n))
                                 wa-%offset%))
          (go label110)
         label108
          (if (= ido 1) (setf na (f2cl-lib:int-sub 1 na)))
          (if (/= na 0) (go label109))
          (mradfg m ido ip l1 idl1 c c c im in ch ch 1 m
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%))
          (setf na 1)
          (go label110)
         label109
          (mradfg m ido ip l1 idl1 ch ch ch 1 m c c im in
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 n))
                                 wa-%offset%))
          (setf na 0)
         label110
          (setf l2 l1)
         label111))
      (setf sn (/ 1.0d0 n))
      (setf tsn (/ 2.0d0 n))
      (setf tsnm (- tsn))
      (setf modn (mod n 2))
      (setf nl (f2cl-lib:int-sub n 2))
      (if (/= modn 0) (setf nl (f2cl-lib:int-sub n 1)))
      (if (/= na 0) (go label120))
      (setf m2 (f2cl-lib:int-sub 1 im))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf m2 (f2cl-lib:int-add m2 im))
          (setf (f2cl-lib:fref c-%data% (m2 1) ((1 in) (1 *)) c-%offset%)
                  (* sn
                     (f2cl-lib:fref ch-%data%
                                    (i 1)
                                    ((1 m) (1 *))
                                    ch-%offset%)))
         label117))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 2))
                    ((> j nl) nil)
        (tagbody
          (setf m2 (f2cl-lib:int-sub 1 im))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf m2 (f2cl-lib:int-add m2 im))
              (setf (f2cl-lib:fref c-%data% (m2 j) ((1 in) (1 *)) c-%offset%)
                      (* tsn
                         (f2cl-lib:fref ch-%data%
                                        (i j)
                                        ((1 m) (1 *))
                                        ch-%offset%)))
              (setf (f2cl-lib:fref c-%data%
                                   (m2 (f2cl-lib:int-add j 1))
                                   ((1 in) (1 *))
                                   c-%offset%)
                      (* tsnm
                         (f2cl-lib:fref ch-%data%
                                        (i (f2cl-lib:int-add j 1))
                                        ((1 m) (1 *))
                                        ch-%offset%)))
             label118))))
     label118
      (if (/= modn 0) (go end_label))
      (setf m2 (f2cl-lib:int-sub 1 im))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf m2 (f2cl-lib:int-add m2 im))
          (setf (f2cl-lib:fref c-%data% (m2 n) ((1 in) (1 *)) c-%offset%)
                  (* sn
                     (f2cl-lib:fref ch-%data%
                                    (i n)
                                    ((1 m) (1 *))
                                    ch-%offset%)))
         label119))
      (go end_label)
     label120
      (setf m2 (f2cl-lib:int-sub 1 im))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf m2 (f2cl-lib:int-add m2 im))
          (setf (f2cl-lib:fref c-%data% (m2 1) ((1 in) (1 *)) c-%offset%)
                  (* sn
                     (f2cl-lib:fref c-%data%
                                    (m2 1)
                                    ((1 in) (1 *))
                                    c-%offset%)))
         label121))
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 2))
                    ((> j nl) nil)
        (tagbody
          (setf m2 (f2cl-lib:int-sub 1 im))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i m) nil)
            (tagbody
              (setf m2 (f2cl-lib:int-add m2 im))
              (setf (f2cl-lib:fref c-%data% (m2 j) ((1 in) (1 *)) c-%offset%)
                      (* tsn
                         (f2cl-lib:fref c-%data%
                                        (m2 j)
                                        ((1 in) (1 *))
                                        c-%offset%)))
              (setf (f2cl-lib:fref c-%data%
                                   (m2 (f2cl-lib:int-add j 1))
                                   ((1 in) (1 *))
                                   c-%offset%)
                      (* tsnm
                         (f2cl-lib:fref c-%data%
                                        (m2 (f2cl-lib:int-add j 1))
                                        ((1 in) (1 *))
                                        c-%offset%)))
             label122))))
     label122
      (if (/= modn 0) (go end_label))
      (setf m2 (f2cl-lib:int-sub 1 im))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf m2 (f2cl-lib:int-add m2 im))
          (setf (f2cl-lib:fref c-%data% (m2 n) ((1 in) (1 *)) c-%offset%)
                  (* sn
                     (f2cl-lib:fref c-%data%
                                    (m2 n)
                                    ((1 in) (1 *))
                                    c-%offset%)))
         label123))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mrftf1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::mradfg fortran-to-lisp::mradf5
                    fortran-to-lisp::mradf3 fortran-to-lisp::mradf2
                    fortran-to-lisp::mradf4))))

