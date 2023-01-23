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


(defun polyp
       (n numt coef kdeg iflg1 iflg2 epsbig epssml numrr nn mmaxt ttotdg
        lambda$ roots arclen nfe totdg pdg qdg r facv cl y yp yold ypold qr
        alpha tz w wp z0 z1 sspar par ideg icount pivot ipar)
  (declare (type (array double-float (*)) sspar)
           (type (double-float) epssml epsbig)
           (type (array double-float (*)) par z1 z0 wp w tz alpha qr ypold yold
                                          yp y cl facv r qdg pdg arclen roots
                                          lambda$ coef)
           (type (array f2cl-lib:integer4 (*)) ipar pivot icount ideg nfe iflg2
                                               kdeg numt)
           (type (f2cl-lib:integer4) totdg ttotdg mmaxt nn numrr iflg1 n))
  (f2cl-lib:with-multi-array-data
      ((numt f2cl-lib:integer4 numt-%data% numt-%offset%)
       (kdeg f2cl-lib:integer4 kdeg-%data% kdeg-%offset%)
       (iflg2 f2cl-lib:integer4 iflg2-%data% iflg2-%offset%)
       (nfe f2cl-lib:integer4 nfe-%data% nfe-%offset%)
       (ideg f2cl-lib:integer4 ideg-%data% ideg-%offset%)
       (icount f2cl-lib:integer4 icount-%data% icount-%offset%)
       (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
       (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%)
       (coef double-float coef-%data% coef-%offset%)
       (lambda$ double-float lambda$-%data% lambda$-%offset%)
       (roots double-float roots-%data% roots-%offset%)
       (arclen double-float arclen-%data% arclen-%offset%)
       (pdg double-float pdg-%data% pdg-%offset%)
       (qdg double-float qdg-%data% qdg-%offset%)
       (r double-float r-%data% r-%offset%)
       (facv double-float facv-%data% facv-%offset%)
       (cl double-float cl-%data% cl-%offset%)
       (y double-float y-%data% y-%offset%)
       (yp double-float yp-%data% yp-%offset%)
       (yold double-float yold-%data% yold-%offset%)
       (ypold double-float ypold-%data% ypold-%offset%)
       (qr double-float qr-%data% qr-%offset%)
       (alpha double-float alpha-%data% alpha-%offset%)
       (tz double-float tz-%data% tz-%offset%)
       (w double-float w-%data% w-%offset%)
       (wp double-float wp-%data% wp-%offset%)
       (z0 double-float z0-%data% z0-%offset%)
       (z1 double-float z1-%data% z1-%offset%)
       (par double-float par-%data% par-%offset%)
       (sspar double-float sspar-%data% sspar-%offset%))
    (prog ((xnp1 (make-array 2 :element-type 'double-float))
           (iproff (make-array 15 :element-type 'f2cl-lib:integer4))
           (lipar (make-array 15 :element-type 'f2cl-lib:integer4))
           (lpar (make-array 25 :element-type 'f2cl-lib:integer4))
           (proff (make-array 25 :element-type 'f2cl-lib:integer4))
           (aarcln 0.0) (ansae 0.0) (ansre 0.0) (arcae 0.0) (arcre 0.0) (i 0)
           (i1 0) (i2 0) (i3 0) (idummy 0) (iflag 0) (ij 0) (ijp1 0)
           (f2cl-lib:index 0) (j 0) (n2 0) (n2p1 0) (nnfe 0) (np1 0) (numpat 0)
           (trace$ 0))
      (declare (type (array f2cl-lib:integer4 (25)) proff lpar)
               (type (array f2cl-lib:integer4 (15)) lipar iproff)
               (type (f2cl-lib:integer4) trace$ numpat np1 nnfe n2p1 n2 j
                                         f2cl-lib:index ijp1 ij iflag idummy i3
                                         i2 i1 i)
               (type (array double-float (2)) xnp1)
               (type (double-float) arcre arcae ansre ansae aarcln))
      (setf n2 (f2cl-lib:int-mul 2 n))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf n2p1 (f2cl-lib:int-add n2 1))
      (if (<= numrr 0) (setf numrr 1))
      (initp iflg1 n numt kdeg coef nn mmaxt par ipar ideg facv cl pdg qdg r)
      (setf (f2cl-lib:fref lipar (1) ((1 15))) 1)
      (setf (f2cl-lib:fref lipar (2) ((1 15))) 1)
      (setf (f2cl-lib:fref lipar (3) ((1 15))) 25)
      (setf (f2cl-lib:fref lipar (4) ((1 15))) 15)
      (setf (f2cl-lib:fref lipar (5) ((1 15))) n)
      (setf (f2cl-lib:fref lipar (6) ((1 15))) n)
      (setf (f2cl-lib:fref lipar (7) ((1 15)))
              (f2cl-lib:int-mul n (f2cl-lib:int-add n 1) mmaxt))
      (setf (f2cl-lib:fref lpar (1) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lpar (2) ((1 25))) (f2cl-lib:int-mul 2 np1))
      (setf (f2cl-lib:fref lpar (3) ((1 25))) (f2cl-lib:int-mul n mmaxt))
      (setf (f2cl-lib:fref lpar (4) ((1 25))) n2)
      (setf (f2cl-lib:fref lpar (5) ((1 25))) (f2cl-lib:int-mul n2 n2))
      (setf (f2cl-lib:fref lpar (6) ((1 25))) n2)
      (setf (f2cl-lib:fref lpar (7) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lpar (8) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lpar (9) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lpar (10) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lpar (11) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lpar (12) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lpar (13) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lpar (14) ((1 25))) (f2cl-lib:int-mul 2 n np1))
      (setf (f2cl-lib:fref lpar (15) ((1 25)))
              (f2cl-lib:int-mul 2 n np1 mmaxt))
      (setf (f2cl-lib:fref lpar (16) ((1 25))) (f2cl-lib:int-mul 2 n mmaxt))
      (setf (f2cl-lib:fref lpar (17) ((1 25)))
              (f2cl-lib:int-mul 2 n np1 mmaxt))
      (setf (f2cl-lib:fref lpar (18) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref lpar (19) ((1 25))) (f2cl-lib:int-mul 2 n))
      (setf (f2cl-lib:fref proff (1) ((1 25))) 1)
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i 19) nil)
        (tagbody
          (setf (f2cl-lib:fref proff (i) ((1 25)))
                  (f2cl-lib:int-add
                   (f2cl-lib:fref proff ((f2cl-lib:int-sub i 1)) ((1 25)))
                   (f2cl-lib:fref lpar ((f2cl-lib:int-sub i 1)) ((1 25)))))
         label10))
      (setf (f2cl-lib:fref iproff (1) ((1 15))) 1)
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i 7) nil)
        (tagbody
          (setf (f2cl-lib:fref iproff (i) ((1 15)))
                  (f2cl-lib:int-add
                   (f2cl-lib:fref iproff ((f2cl-lib:int-sub i 1)) ((1 15)))
                   (f2cl-lib:fref lipar ((f2cl-lib:int-sub i 1)) ((1 15)))))
         label11))
      (setf (f2cl-lib:fref ipar-%data%
                           (1)
                           ((1
                             (f2cl-lib:int-add 42
                                               (f2cl-lib:int-mul 2 n)
                                               (f2cl-lib:int-mul n
                                                                 (f2cl-lib:int-add
                                                                  n
                                                                  1)
                                                                 mmaxt))))
                           ipar-%offset%)
              n)
      (setf (f2cl-lib:fref ipar-%data%
                           (2)
                           ((1
                             (f2cl-lib:int-add 42
                                               (f2cl-lib:int-mul 2 n)
                                               (f2cl-lib:int-mul n
                                                                 (f2cl-lib:int-add
                                                                  n
                                                                  1)
                                                                 mmaxt))))
                           ipar-%offset%)
              mmaxt)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i 19) nil)
        (tagbody
          (setf (f2cl-lib:fref ipar-%data%
                               ((f2cl-lib:int-add
                                 (f2cl-lib:fref iproff (3) ((1 15)))
                                 (f2cl-lib:int-sub i 1)))
                               ((1
                                 (f2cl-lib:int-add 42
                                                   (f2cl-lib:int-mul 2 n)
                                                   (f2cl-lib:int-mul n
                                                                     (f2cl-lib:int-add
                                                                      n
                                                                      1)
                                                                     mmaxt))))
                               ipar-%offset%)
                  (f2cl-lib:fref proff (i) ((1 25))))
         label16))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i 7) nil)
        (tagbody
          (setf (f2cl-lib:fref ipar-%data%
                               ((f2cl-lib:int-add
                                 (f2cl-lib:fref iproff (4) ((1 15)))
                                 (f2cl-lib:int-sub i 1)))
                               ((1
                                 (f2cl-lib:int-add 42
                                                   (f2cl-lib:int-mul 2 n)
                                                   (f2cl-lib:int-mul n
                                                                     (f2cl-lib:int-add
                                                                      n
                                                                      1)
                                                                     mmaxt))))
                               ipar-%offset%)
                  (f2cl-lib:fref iproff (i) ((1 15))))
         label18))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref ipar-%data%
                               ((f2cl-lib:int-add
                                 (f2cl-lib:fref iproff (5) ((1 15)))
                                 (f2cl-lib:int-sub i 1)))
                               ((1
                                 (f2cl-lib:int-add 42
                                                   (f2cl-lib:int-mul 2 n)
                                                   (f2cl-lib:int-mul n
                                                                     (f2cl-lib:int-add
                                                                      n
                                                                      1)
                                                                     mmaxt))))
                               ipar-%offset%)
                  (f2cl-lib:fref ideg-%data% (i) ((1 n)) ideg-%offset%))
          (setf (f2cl-lib:fref ipar-%data%
                               ((f2cl-lib:int-add
                                 (f2cl-lib:fref iproff (6) ((1 15)))
                                 (f2cl-lib:int-sub i 1)))
                               ((1
                                 (f2cl-lib:int-add 42
                                                   (f2cl-lib:int-mul 2 n)
                                                   (f2cl-lib:int-mul n
                                                                     (f2cl-lib:int-add
                                                                      n
                                                                      1)
                                                                     mmaxt))))
                               ipar-%offset%)
                  (f2cl-lib:fref numt-%data% (i) ((1 nn)) numt-%offset%))
         label20))
      (f2cl-lib:fdo (i1 1 (f2cl-lib:int-add i1 1))
                    ((> i1 n) nil)
        (tagbody
          (f2cl-lib:fdo (i2 1 (f2cl-lib:int-add i2 1))
                        ((> i2 np1) nil)
            (tagbody
              (f2cl-lib:fdo (i3 1 (f2cl-lib:int-add i3 1))
                            ((> i3 (f2cl-lib:fref numt (i1) ((1 nn)))) nil)
                (tagbody
                  (setf f2cl-lib:index
                          (f2cl-lib:int-add (f2cl-lib:fref iproff (7) ((1 15)))
                                            (f2cl-lib:int-sub i1 1)
                                            (f2cl-lib:int-mul n
                                                              (f2cl-lib:int-sub
                                                               i2
                                                               1))
                                            (f2cl-lib:int-mul n
                                                              np1
                                                              (f2cl-lib:int-sub
                                                               i3
                                                               1))))
                  (setf (f2cl-lib:fref ipar-%data%
                                       (f2cl-lib:index)
                                       ((1
                                         (f2cl-lib:int-add 42
                                                           (f2cl-lib:int-mul 2
                                                                             n)
                                                           (f2cl-lib:int-mul n
                                                                             (f2cl-lib:int-add
                                                                              n
                                                                              1)
                                                                             mmaxt))))
                                       ipar-%offset%)
                          (f2cl-lib:fref kdeg-%data%
                                         (i1 i2 i3)
                                         ((1 nn) (1 (f2cl-lib:int-add nn 1))
                                          (1 mmaxt))
                                         kdeg-%offset%))
                 label22))))))
     label22
      (f2cl-lib:fdo (i1 1 (f2cl-lib:int-add i1 1))
                    ((> i1 2) nil)
        (tagbody
          (f2cl-lib:fdo (i2 1 (f2cl-lib:int-add i2 1))
                        ((> i2 n) nil)
            (tagbody
              (setf (f2cl-lib:fref par-%data%
                                   ((f2cl-lib:int-add
                                     (f2cl-lib:fref proff (1) ((1 25)))
                                     (f2cl-lib:int-sub i1 1)
                                     (f2cl-lib:int-mul 2
                                                       (f2cl-lib:int-sub i2
                                                                         1))))
                                   ((1
                                     (f2cl-lib:int-add 2
                                                       (f2cl-lib:int-mul 28 n)
                                                       (f2cl-lib:int-mul 6
                                                                         (expt
                                                                          n
                                                                          2))
                                                       (f2cl-lib:int-mul 7
                                                                         n
                                                                         mmaxt)
                                                       (f2cl-lib:int-mul 4
                                                                         (expt
                                                                          n
                                                                          2)
                                                                         mmaxt))))
                                   par-%offset%)
                      (f2cl-lib:fref pdg-%data%
                                     (i1 i2)
                                     ((1 2) (1 n))
                                     pdg-%offset%))
             label36))))
     label36
      (f2cl-lib:fdo (i1 1 (f2cl-lib:int-add i1 1))
                    ((> i1 2) nil)
        (tagbody
          (f2cl-lib:fdo (i2 1 (f2cl-lib:int-add i2 1))
                        ((> i2 np1) nil)
            (tagbody
              (setf (f2cl-lib:fref par-%data%
                                   ((f2cl-lib:int-add
                                     (f2cl-lib:fref proff (2) ((1 25)))
                                     (f2cl-lib:int-sub i1 1)
                                     (f2cl-lib:int-mul 2
                                                       (f2cl-lib:int-sub i2
                                                                         1))))
                                   ((1
                                     (f2cl-lib:int-add 2
                                                       (f2cl-lib:int-mul 28 n)
                                                       (f2cl-lib:int-mul 6
                                                                         (expt
                                                                          n
                                                                          2))
                                                       (f2cl-lib:int-mul 7
                                                                         n
                                                                         mmaxt)
                                                       (f2cl-lib:int-mul 4
                                                                         (expt
                                                                          n
                                                                          2)
                                                                         mmaxt))))
                                   par-%offset%)
                      (f2cl-lib:fref cl-%data%
                                     (i1 i2)
                                     ((1 2) (1 (f2cl-lib:int-add n 1)))
                                     cl-%offset%))
             label37))))
     label37
      (f2cl-lib:fdo (i1 1 (f2cl-lib:int-add i1 1))
                    ((> i1 n) nil)
        (tagbody
          (f2cl-lib:fdo (i2 1 (f2cl-lib:int-add i2 1))
                        ((> i2 (f2cl-lib:fref numt (i1) ((1 nn)))) nil)
            (tagbody
              (setf (f2cl-lib:fref par-%data%
                                   ((f2cl-lib:int-add
                                     (f2cl-lib:fref proff (3) ((1 25)))
                                     (f2cl-lib:int-sub i1 1)
                                     (f2cl-lib:int-mul n
                                                       (f2cl-lib:int-sub i2
                                                                         1))))
                                   ((1
                                     (f2cl-lib:int-add 2
                                                       (f2cl-lib:int-mul 28 n)
                                                       (f2cl-lib:int-mul 6
                                                                         (expt
                                                                          n
                                                                          2))
                                                       (f2cl-lib:int-mul 7
                                                                         n
                                                                         mmaxt)
                                                       (f2cl-lib:int-mul 4
                                                                         (expt
                                                                          n
                                                                          2)
                                                                         mmaxt))))
                                   par-%offset%)
                      (f2cl-lib:fref coef-%data%
                                     (i1 i2)
                                     ((1 nn) (1 mmaxt))
                                     coef-%offset%))
             label38))))
     label38
      (setf (f2cl-lib:fref icount-%data% (1) ((1 n)) icount-%offset%) 0)
      (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf (f2cl-lib:fref icount-%data% (j) ((1 n)) icount-%offset%) 1)
         label50))
      (f2cl-lib:fdo (numpat 1 (f2cl-lib:int-add numpat 1))
                    ((> numpat totdg) nil)
        (tagbody
          (setf (f2cl-lib:fref y-%data%
                               (1)
                               ((1
                                 (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 1)))
                               y-%offset%)
                  (coerce 0.0f0 'double-float))
          (strptp n icount ideg r
           (f2cl-lib:array-slice y-%data%
                                 double-float
                                 (2)
                                 ((1
                                   (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                     1)))
                                 y-%offset%))
          (setf iflag
                  (f2cl-lib:fref iflg2-%data%
                                 (numpat)
                                 ((1 ttotdg))
                                 iflg2-%offset%))
          (if (/= iflag -2) (go label1000))
          (setf arcre epsbig)
          (setf arcae arcre)
          (setf ansre epssml)
          (setf ansae ansre)
          (setf trace$ 0)
          (f2cl-lib:fdo (idummy 1 (f2cl-lib:int-add idummy 1))
                        ((> idummy numrr) nil)
            (tagbody
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16
                     var-17 var-18 var-19 var-20 var-21 var-22 var-23 var-24)
                  (polynf n2 y iflag arcre arcae ansre ansae trace$ qdg nnfe
                   aarcln yp yold ypold qr alpha tz pivot w wp z0 z1 sspar par
                   ipar)
                (declare (ignore var-0 var-1 var-7 var-8 var-11 var-12 var-13
                                 var-14 var-15 var-16 var-17 var-18 var-19
                                 var-20 var-21 var-22 var-23 var-24))
                (setf iflag var-2)
                (setf arcre var-3)
                (setf arcae var-4)
                (setf ansre var-5)
                (setf ansae var-6)
                (setf nnfe var-9)
                (setf aarcln var-10))
              (if (and (/= iflag 2) (/= iflag 3)) (go label66))
             label65))
         label66
          (otputp n numpat cl facv
           (f2cl-lib:array-slice par-%data%
                                 double-float
                                 ((f2cl-lib:fref proff (18) ((1 25))))
                                 ((1
                                   (f2cl-lib:int-add 2
                                                     (f2cl-lib:int-mul 28 n)
                                                     (f2cl-lib:int-mul 6
                                                                       (expt n
                                                                             2))
                                                     (f2cl-lib:int-mul 7
                                                                       n
                                                                       mmaxt)
                                                     (f2cl-lib:int-mul 4
                                                                       (expt n
                                                                             2)
                                                                       mmaxt))))
                                 par-%offset%)
           (f2cl-lib:array-slice y-%data%
                                 double-float
                                 (2)
                                 ((1
                                   (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                     1)))
                                 y-%offset%)
           xnp1)
          (setf (f2cl-lib:fref lambda$-%data%
                               (numpat)
                               ((1 ttotdg))
                               lambda$-%offset%)
                  (f2cl-lib:fref y-%data%
                                 (1)
                                 ((1
                                   (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                                                     1)))
                                 y-%offset%))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i 2) nil)
                (tagbody
                  (setf ij
                          (f2cl-lib:int-sub
                           (f2cl-lib:int-add (f2cl-lib:int-mul 2 j) i)
                           2))
                  (setf ijp1 (f2cl-lib:int-add ij 1))
                  (setf (f2cl-lib:fref roots-%data%
                                       (i j numpat)
                                       ((1 2) (1 (f2cl-lib:int-add nn 1))
                                        (1 ttotdg))
                                       roots-%offset%)
                          (f2cl-lib:fref y-%data%
                                         (ijp1)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 2 n)
                                            1)))
                                         y-%offset%))
                 label70))))
         label70
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i 2) nil)
            (tagbody
              (setf (f2cl-lib:fref roots-%data%
                                   (i np1 numpat)
                                   ((1 2) (1 (f2cl-lib:int-add nn 1))
                                    (1 ttotdg))
                                   roots-%offset%)
                      (f2cl-lib:fref xnp1 (i) ((1 2))))
             label72))
          (setf (f2cl-lib:fref arclen-%data%
                               (numpat)
                               ((1 ttotdg))
                               arclen-%offset%)
                  aarcln)
          (setf (f2cl-lib:fref nfe-%data% (numpat) ((1 ttotdg)) nfe-%offset%)
                  nnfe)
          (setf (f2cl-lib:fref iflg2-%data%
                               (numpat)
                               ((1 ttotdg))
                               iflg2-%offset%)
                  iflag)
         label1000))
      (go end_label)
     end_label
      (return
       (values nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               numrr
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil
               nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::polyp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*)) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil nil nil nil nil
                            fortran-to-lisp::numrr nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::otputp fortran-to-lisp::polynf
                    fortran-to-lisp::strptp fortran-to-lisp::initp))))

