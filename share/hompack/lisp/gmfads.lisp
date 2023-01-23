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


(defun gmfads (nn a nwk maxa)
  (declare (type (array f2cl-lib:integer4 (*)) maxa)
           (type (array double-float (*)) a)
           (type (f2cl-lib:integer4) nwk nn))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (maxa f2cl-lib:integer4 maxa-%data% maxa-%offset%))
    (prog ((bet 0.0) (del 0.0) (dj 0.0) (g 0.0) (gam 0.0) (gam1 0.0) (phi 0.0)
           (the$ 0.0) (the1 0.0) (xt1 0.0) (xt2 0.0) (zet 0.0) (zet1 0.0) (i 0)
           (i0 0) (i1 0) (i2 0) (i3 0) (i4 0) (j 0) (j1 0) (k 0) (k1 0) (k2 0)
           (kh 0) (kl 0) (kn 0) (ku 0) (kz 0) (l 0) (l1 0) (l2 0) (l3 0) (m 0)
           (m1 0) (n1 0) (nnn 0))
      (declare (type (f2cl-lib:integer4) nnn n1 m1 m l3 l2 l1 l kz ku kn kl kh
                                         k2 k1 k j1 j i4 i3 i2 i1 i0 i)
               (type (double-float) zet1 zet xt2 xt1 the1 the$ phi gam1 gam g
                                    dj del bet))
      (setf g (coerce 0.0f0 'double-float))
      (setf gam (coerce 0.0f0 'double-float))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf k
                  (f2cl-lib:fref maxa-%data%
                                 (i)
                                 ((1 (f2cl-lib:int-add nn 1)))
                                 maxa-%offset%))
          (setf g
                  (+ g
                     (* (f2cl-lib:fref a-%data% (k) ((1 nwk)) a-%offset%)
                        (f2cl-lib:fref a-%data% (k) ((1 nwk)) a-%offset%))))
          (setf gam1 (abs (f2cl-lib:fref a-%data% (k) ((1 nwk)) a-%offset%)))
          (if (> gam1 gam) (setf gam gam1))
         label1))
      (setf zet (coerce 0.0f0 'double-float))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf k
                  (f2cl-lib:fref maxa-%data%
                                 (i)
                                 ((1 (f2cl-lib:int-add nn 1)))
                                 maxa-%offset%))
          (setf k1
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref maxa-%data%
                                  ((f2cl-lib:int-add i 1))
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  maxa-%offset%)
                   1))
          (setf k2 (f2cl-lib:int-sub k1 k))
          (if (= k2 0) (go label3))
          (setf l (f2cl-lib:int-add k 1))
          (f2cl-lib:fdo (j l (f2cl-lib:int-add j 1))
                        ((> j k1) nil)
            (tagbody
              (setf g
                      (+ g
                         (* 2.0f0
                            (f2cl-lib:fref a-%data% (j) ((1 nwk)) a-%offset%)
                            (f2cl-lib:fref a-%data%
                                           (j)
                                           ((1 nwk))
                                           a-%offset%))))
              (setf zet1
                      (abs (f2cl-lib:fref a-%data% (j) ((1 nwk)) a-%offset%)))
              (if (> zet1 zet) (setf zet zet1))
             label2))
         label3))
      (setf zet (/ zet nn))
      (setf del (f2cl-lib:d1mach 4))
      (setf bet del)
      (if (> zet bet) (setf bet zet))
      (if (> gam bet) (setf bet gam))
      (setf g (f2cl-lib:fsqrt g))
      (if (> g 1.0f0) (setf del (* del g)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf n1 (f2cl-lib:int-sub i 1))
          (setf kn
                  (f2cl-lib:fref maxa-%data%
                                 (i)
                                 ((1 (f2cl-lib:int-add nn 1)))
                                 maxa-%offset%))
          (setf kl (f2cl-lib:int-add kn 1))
          (setf ku
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref maxa-%data%
                                  ((f2cl-lib:int-add i 1))
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  maxa-%offset%)
                   1))
          (setf kh (f2cl-lib:int-sub ku kl))
          (setf phi (f2cl-lib:fref a-%data% (kn) ((1 nwk)) a-%offset%))
          (if (< kh 0) (go label10))
          (setf k1 (f2cl-lib:int-add kn 1))
          (setf k2 i)
          (f2cl-lib:fdo (j k1 (f2cl-lib:int-add j 1))
                        ((> j ku) nil)
            (tagbody
              (setf k2 (f2cl-lib:int-sub k2 1))
              (setf kz
                      (f2cl-lib:fref maxa-%data%
                                     (k2)
                                     ((1 (f2cl-lib:int-add nn 1)))
                                     maxa-%offset%))
              (setf phi
                      (+ phi
                         (*
                          (- (f2cl-lib:fref a-%data% (j) ((1 nwk)) a-%offset%))
                          (f2cl-lib:fref a-%data% (j) ((1 nwk)) a-%offset%)
                          (f2cl-lib:fref a-%data% (kz) ((1 nwk)) a-%offset%))))
             label5))
         label10
          (setf phi (abs phi))
          (setf l (f2cl-lib:int-add i 1))
          (setf the$ (coerce 0.0f0 'double-float))
          (setf nnn (f2cl-lib:int-add nn 1))
          (if (= l nnn) (go label11))
          (f2cl-lib:fdo (j l (f2cl-lib:int-add j 1))
                        ((> j nn) nil)
            (tagbody
              (setf l1
                      (f2cl-lib:fref maxa-%data%
                                     (j)
                                     ((1 (f2cl-lib:int-add nn 1)))
                                     maxa-%offset%))
              (setf l2
                      (f2cl-lib:fref maxa-%data%
                                     ((f2cl-lib:int-add j 1))
                                     ((1 (f2cl-lib:int-add nn 1)))
                                     maxa-%offset%))
              (setf l3 (f2cl-lib:int-sub l2 l1 1))
              (setf m (f2cl-lib:int-sub j i))
              (if (< l3 m) (go label6))
              (setf m1 (f2cl-lib:int-add l1 m))
              (if (= n1 0) (go label7))
              (f2cl-lib:fdo (j1 1 (f2cl-lib:int-add j1 1))
                            ((> j1 n1) nil)
                (tagbody
                  (setf i0
                          (f2cl-lib:fref maxa-%data%
                                         (j1)
                                         ((1 (f2cl-lib:int-add nn 1)))
                                         maxa-%offset%))
                  (setf i1
                          (f2cl-lib:fref maxa-%data%
                                         (l)
                                         ((1 (f2cl-lib:int-add nn 1)))
                                         maxa-%offset%))
                  (setf i2 (f2cl-lib:int-sub i j1))
                  (setf i3 (f2cl-lib:int-sub i1 kn 1))
                  (setf i4 (f2cl-lib:int-sub j j1))
                  (if (< i3 i2) (go label8))
                  (if (< l3 i4) (go label8))
                  (setf xt1
                          (f2cl-lib:fref a-%data%
                                         ((f2cl-lib:int-add kn i2))
                                         ((1 nwk))
                                         a-%offset%))
                  (setf xt2
                          (f2cl-lib:fref a-%data%
                                         ((f2cl-lib:int-add l1 i4))
                                         ((1 nwk))
                                         a-%offset%))
                  (setf (f2cl-lib:fref a-%data% (m1) ((1 nwk)) a-%offset%)
                          (+ (f2cl-lib:fref a-%data% (m1) ((1 nwk)) a-%offset%)
                             (* (- xt1)
                                xt2
                                (f2cl-lib:fref a-%data%
                                               (i0)
                                               ((1 nwk))
                                               a-%offset%))))
                 label8))
             label7
              (setf the1
                      (abs (f2cl-lib:fref a-%data% (m1) ((1 nwk)) a-%offset%)))
              (if (< the$ the1) (setf the$ the1))
             label6))
         label11
          (setf the$ (/ (* the$ the$) bet))
          (setf dj del)
          (if (> phi dj) (setf dj phi))
          (if (> the$ dj) (setf dj the$))
          (setf (f2cl-lib:fref a-%data% (kn) ((1 nwk)) a-%offset%) dj)
          (if (= l nnn) (go label4))
          (f2cl-lib:fdo (j l (f2cl-lib:int-add j 1))
                        ((> j nn) nil)
            (tagbody
              (setf l1
                      (f2cl-lib:fref maxa-%data%
                                     (j)
                                     ((1 (f2cl-lib:int-add nn 1)))
                                     maxa-%offset%))
              (setf l2
                      (f2cl-lib:fref maxa-%data%
                                     ((f2cl-lib:int-add j 1))
                                     ((1 (f2cl-lib:int-add nn 1)))
                                     maxa-%offset%))
              (setf l3 (f2cl-lib:int-sub l2 l1 1))
              (setf m (f2cl-lib:int-sub j i))
              (if (< l3 m) (go label9))
              (setf m1 (f2cl-lib:int-add l1 m))
              (setf (f2cl-lib:fref a-%data% (m1) ((1 nwk)) a-%offset%)
                      (/ (f2cl-lib:fref a-%data% (m1) ((1 nwk)) a-%offset%)
                         (f2cl-lib:fref a-%data% (kn) ((1 nwk)) a-%offset%)))
             label9))
         label4))
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::gmfads
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil)
           :calls '(fortran-to-lisp::d1mach))))

