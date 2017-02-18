;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2013-11 (20E Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(defun nsfc
       (n r ic ia ja jlmax il jl ijl jumax iu ju iju q ira jra irac irl jrl iru
        jru flag)
  (declare (type (array f2cl-lib:integer4 (*)) jru iru jrl irl irac jra ira q
                                               iju ju iu ijl jl il ja ia ic r)
           (type (f2cl-lib:integer4) flag jumax jlmax n))
  (f2cl-lib:with-multi-array-data
      ((r f2cl-lib:integer4 r-%data% r-%offset%)
       (ic f2cl-lib:integer4 ic-%data% ic-%offset%)
       (ia f2cl-lib:integer4 ia-%data% ia-%offset%)
       (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
       (il f2cl-lib:integer4 il-%data% il-%offset%)
       (jl f2cl-lib:integer4 jl-%data% jl-%offset%)
       (ijl f2cl-lib:integer4 ijl-%data% ijl-%offset%)
       (iu f2cl-lib:integer4 iu-%data% iu-%offset%)
       (ju f2cl-lib:integer4 ju-%data% ju-%offset%)
       (iju f2cl-lib:integer4 iju-%data% iju-%offset%)
       (q f2cl-lib:integer4 q-%data% q-%offset%)
       (ira f2cl-lib:integer4 ira-%data% ira-%offset%)
       (jra f2cl-lib:integer4 jra-%data% jra-%offset%)
       (irac f2cl-lib:integer4 irac-%data% irac-%offset%)
       (irl f2cl-lib:integer4 irl-%data% irl-%offset%)
       (jrl f2cl-lib:integer4 jrl-%data% jrl-%offset%)
       (iru f2cl-lib:integer4 iru-%data% iru-%offset%)
       (jru f2cl-lib:integer4 jru-%data% jru-%offset%))
    (prog ((cend 0) (qm 0) (rend 0) (rk 0) (vj 0) (jairai 0) (irai 0) (irul 0)
           (i1 0) (irll 0) (j 0) (jtmp 0) (long 0) (jmax 0) (jmin 0) (i 0)
           (lasti 0) (lastid 0) (m 0) (luk 0) (jaiak 0) (iak 0) (k 0) (juptr 0)
           (jumin 0) (jlptr 0) (jlmin 0) (np1 0))
      (declare (type (f2cl-lib:integer4) np1 jlmin jlptr jumin juptr k iak
                                         jaiak luk m lastid lasti i jmin jmax
                                         long jtmp j irll i1 irul irai jairai
                                         vj rk rend qm cend))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf jlmin 1)
      (setf jlptr 0)
      (setf (f2cl-lib:fref il-%data% (1) ((1 *)) il-%offset%) 1)
      (setf jumin 1)
      (setf juptr 0)
      (setf (f2cl-lib:fref iu-%data% (1) ((1 *)) iu-%offset%) 1)
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf (f2cl-lib:fref irac-%data% (k) ((1 *)) irac-%offset%) 0)
          (setf (f2cl-lib:fref jra-%data% (k) ((1 *)) jra-%offset%) 0)
          (setf (f2cl-lib:fref jrl-%data% (k) ((1 *)) jrl-%offset%) 0)
         label1
          (setf (f2cl-lib:fref jru-%data% (k) ((1 *)) jru-%offset%) 0)))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf rk (f2cl-lib:fref r-%data% (k) ((1 *)) r-%offset%))
          (setf iak (f2cl-lib:fref ia-%data% (rk) ((1 *)) ia-%offset%))
          (if
           (>= iak
               (f2cl-lib:fref ia-%data%
                              ((f2cl-lib:int-add rk 1))
                              ((1 *))
                              ia-%offset%))
           (go label101))
          (setf jaiak
                  (f2cl-lib:fref ic-%data%
                                 ((f2cl-lib:fref ja (iak) ((1 *))))
                                 ((1 *))
                                 ic-%offset%))
          (if (> jaiak k) (go label105))
          (setf (f2cl-lib:fref jra-%data% (k) ((1 *)) jra-%offset%)
                  (f2cl-lib:fref irac-%data% (jaiak) ((1 *)) irac-%offset%))
          (setf (f2cl-lib:fref irac-%data% (jaiak) ((1 *)) irac-%offset%) k)
         label2
          (setf (f2cl-lib:fref ira-%data% (k) ((1 *)) ira-%offset%) iak)))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data% (np1) ((1 *)) q-%offset%) np1)
          (setf luk -1)
          (setf vj (f2cl-lib:fref irac-%data% (k) ((1 *)) irac-%offset%))
          (if (= vj 0) (go label5))
         label3
          (setf qm np1)
         label4
          (setf m qm)
          (setf qm (f2cl-lib:fref q-%data% (m) ((1 *)) q-%offset%))
          (if (< qm vj) (go label4))
          (if (= qm vj) (go label102))
          (setf luk (f2cl-lib:int-add luk 1))
          (setf (f2cl-lib:fref q-%data% (m) ((1 *)) q-%offset%) vj)
          (setf (f2cl-lib:fref q-%data% (vj) ((1 *)) q-%offset%) qm)
          (setf vj (f2cl-lib:fref jra-%data% (vj) ((1 *)) jra-%offset%))
          (if (/= vj 0) (go label3))
         label5
          (setf lastid 0)
          (setf lasti 0)
          (setf (f2cl-lib:fref ijl-%data% (k) ((1 *)) ijl-%offset%) jlptr)
          (setf i k)
         label6
          (setf i (f2cl-lib:fref jru-%data% (i) ((1 *)) jru-%offset%))
          (if (= i 0) (go label10))
          (setf qm np1)
          (setf jmin (f2cl-lib:fref irl-%data% (i) ((1 *)) irl-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:int-add
                    (f2cl-lib:fref ijl-%data% (i) ((1 *)) ijl-%offset%)
                    (f2cl-lib:fref il-%data%
                                   ((f2cl-lib:int-add i 1))
                                   ((1 *))
                                   il-%offset%))
                   (f2cl-lib:fref il-%data% (i) ((1 *)) il-%offset%)
                   1))
          (setf long (f2cl-lib:int-sub jmax jmin))
          (if (< long 0) (go label6))
          (setf jtmp (f2cl-lib:fref jl-%data% (jmin) ((1 *)) jl-%offset%))
          (if (/= jtmp k) (setf long (f2cl-lib:int-add long 1)))
          (if (= jtmp k)
              (setf (f2cl-lib:fref r-%data% (i) ((1 *)) r-%offset%)
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref r-%data% (i) ((1 *)) r-%offset%))))
          (if (>= lastid long) (go label7))
          (setf lasti i)
          (setf lastid long)
         label7
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
              (setf vj (f2cl-lib:fref jl-%data% (j) ((1 *)) jl-%offset%))
             label8
              (setf m qm)
              (setf qm (f2cl-lib:fref q-%data% (m) ((1 *)) q-%offset%))
              (if (< qm vj) (go label8))
              (if (= qm vj) (go label9))
              (setf luk (f2cl-lib:int-add luk 1))
              (setf (f2cl-lib:fref q-%data% (m) ((1 *)) q-%offset%) vj)
              (setf (f2cl-lib:fref q-%data% (vj) ((1 *)) q-%offset%) qm)
              (setf qm vj)
             label9))
          (go label6)
         label10
          (setf qm (f2cl-lib:fref q-%data% (np1) ((1 *)) q-%offset%))
          (if (/= qm k) (go label105))
          (if (= luk 0) (go label17))
          (if (/= lastid luk) (go label11))
          (setf irll (f2cl-lib:fref irl-%data% (lasti) ((1 *)) irl-%offset%))
          (setf (f2cl-lib:fref ijl-%data% (k) ((1 *)) ijl-%offset%)
                  (f2cl-lib:int-add irll 1))
          (if (/= (f2cl-lib:fref jl-%data% (irll) ((1 *)) jl-%offset%) k)
              (setf (f2cl-lib:fref ijl-%data% (k) ((1 *)) ijl-%offset%)
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref ijl-%data% (k) ((1 *)) ijl-%offset%)
                       1)))
          (go label17)
         label11
          (if (> jlmin jlptr) (go label15))
          (setf qm (f2cl-lib:fref q-%data% (qm) ((1 *)) q-%offset%))
          (f2cl-lib:fdo (j jlmin (f2cl-lib:int-add j 1))
                        ((> j jlptr) nil)
            (tagbody
              (f2cl-lib:arithmetic-if
               (f2cl-lib:int-sub
                (f2cl-lib:fref jl-%data% (j) ((1 *)) jl-%offset%)
                qm)
               (go label12)
               (go label13)
               (go label15))
             label12))
          (go label15)
         label13
          (setf (f2cl-lib:fref ijl-%data% (k) ((1 *)) ijl-%offset%) j)
          (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                        ((> i jlptr) nil)
            (tagbody
              (if (/= (f2cl-lib:fref jl-%data% (i) ((1 *)) jl-%offset%) qm)
                  (go label15))
              (setf qm (f2cl-lib:fref q-%data% (qm) ((1 *)) q-%offset%))
              (if (> qm n) (go label17))
             label14))
          (setf jlptr (f2cl-lib:int-sub j 1))
         label15
          (setf jlmin (f2cl-lib:int-add jlptr 1))
          (setf (f2cl-lib:fref ijl-%data% (k) ((1 *)) ijl-%offset%) jlmin)
          (if (= luk 0) (go label17))
          (setf jlptr (f2cl-lib:int-add jlptr luk))
          (if (> jlptr jlmax) (go label103))
          (setf qm (f2cl-lib:fref q-%data% (np1) ((1 *)) q-%offset%))
          (f2cl-lib:fdo (j jlmin (f2cl-lib:int-add j 1))
                        ((> j jlptr) nil)
            (tagbody
              (setf qm (f2cl-lib:fref q-%data% (qm) ((1 *)) q-%offset%))
             label16
              (setf (f2cl-lib:fref jl-%data% (j) ((1 *)) jl-%offset%) qm)))
         label17
          (setf (f2cl-lib:fref irl-%data% (k) ((1 *)) irl-%offset%)
                  (f2cl-lib:fref ijl-%data% (k) ((1 *)) ijl-%offset%))
          (setf (f2cl-lib:fref il-%data%
                               ((f2cl-lib:int-add k 1))
                               ((1 *))
                               il-%offset%)
                  (f2cl-lib:int-add
                   (f2cl-lib:fref il-%data% (k) ((1 *)) il-%offset%)
                   luk))
          (setf (f2cl-lib:fref q-%data% (np1) ((1 *)) q-%offset%) np1)
          (setf luk -1)
          (setf rk (f2cl-lib:fref r-%data% (k) ((1 *)) r-%offset%))
          (setf jmin (f2cl-lib:fref ira-%data% (k) ((1 *)) ira-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref ia-%data%
                                  ((f2cl-lib:int-add rk 1))
                                  ((1 *))
                                  ia-%offset%)
                   1))
          (if (> jmin jmax) (go label20))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
              (setf vj
                      (f2cl-lib:fref ic-%data%
                                     ((f2cl-lib:fref ja (j) ((1 *))))
                                     ((1 *))
                                     ic-%offset%))
              (setf qm np1)
             label18
              (setf m qm)
              (setf qm (f2cl-lib:fref q-%data% (m) ((1 *)) q-%offset%))
              (if (< qm vj) (go label18))
              (if (= qm vj) (go label102))
              (setf luk (f2cl-lib:int-add luk 1))
              (setf (f2cl-lib:fref q-%data% (m) ((1 *)) q-%offset%) vj)
              (setf (f2cl-lib:fref q-%data% (vj) ((1 *)) q-%offset%) qm)
             label19))
         label20
          (setf lastid 0)
          (setf lasti 0)
          (setf (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%) juptr)
          (setf i k)
          (setf i1 (f2cl-lib:fref jrl-%data% (k) ((1 *)) jrl-%offset%))
         label21
          (setf i i1)
          (if (= i 0) (go label26))
          (setf i1 (f2cl-lib:fref jrl-%data% (i) ((1 *)) jrl-%offset%))
          (setf qm np1)
          (setf jmin (f2cl-lib:fref iru-%data% (i) ((1 *)) iru-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:int-add
                    (f2cl-lib:fref iju-%data% (i) ((1 *)) iju-%offset%)
                    (f2cl-lib:fref iu-%data%
                                   ((f2cl-lib:int-add i 1))
                                   ((1 *))
                                   iu-%offset%))
                   (f2cl-lib:fref iu-%data% (i) ((1 *)) iu-%offset%)
                   1))
          (setf long (f2cl-lib:int-sub jmax jmin))
          (if (< long 0) (go label21))
          (setf jtmp (f2cl-lib:fref ju-%data% (jmin) ((1 *)) ju-%offset%))
          (if (= jtmp k) (go label22))
          (setf long (f2cl-lib:int-add long 1))
          (setf cend
                  (f2cl-lib:int-sub
                   (f2cl-lib:int-add
                    (f2cl-lib:fref ijl-%data% (i) ((1 *)) ijl-%offset%)
                    (f2cl-lib:fref il-%data%
                                   ((f2cl-lib:int-add i 1))
                                   ((1 *))
                                   il-%offset%))
                   (f2cl-lib:fref il-%data% (i) ((1 *)) il-%offset%)))
          (setf (f2cl-lib:fref irl-%data% (i) ((1 *)) irl-%offset%)
                  (f2cl-lib:int-add
                   (f2cl-lib:fref irl-%data% (i) ((1 *)) irl-%offset%)
                   1))
          (if (>= (f2cl-lib:fref irl-%data% (i) ((1 *)) irl-%offset%) cend)
              (go label22))
          (setf j
                  (f2cl-lib:fref jl-%data%
                                 ((f2cl-lib:fref irl (i) ((1 *))))
                                 ((1 *))
                                 jl-%offset%))
          (setf (f2cl-lib:fref jrl-%data% (i) ((1 *)) jrl-%offset%)
                  (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%))
          (setf (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%) i)
         label22
          (if (>= lastid long) (go label23))
          (setf lasti i)
          (setf lastid long)
         label23
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
              (setf vj (f2cl-lib:fref ju-%data% (j) ((1 *)) ju-%offset%))
             label24
              (setf m qm)
              (setf qm (f2cl-lib:fref q-%data% (m) ((1 *)) q-%offset%))
              (if (< qm vj) (go label24))
              (if (= qm vj) (go label25))
              (setf luk (f2cl-lib:int-add luk 1))
              (setf (f2cl-lib:fref q-%data% (m) ((1 *)) q-%offset%) vj)
              (setf (f2cl-lib:fref q-%data% (vj) ((1 *)) q-%offset%) qm)
              (setf qm vj)
             label25))
          (go label21)
         label26
          (if
           (<=
            (f2cl-lib:fref il-%data%
                           ((f2cl-lib:int-add k 1))
                           ((1 *))
                           il-%offset%)
            (f2cl-lib:fref il-%data% (k) ((1 *)) il-%offset%))
           (go label27))
          (setf j
                  (f2cl-lib:fref jl-%data%
                                 ((f2cl-lib:fref irl (k) ((1 *))))
                                 ((1 *))
                                 jl-%offset%))
          (setf (f2cl-lib:fref jrl-%data% (k) ((1 *)) jrl-%offset%)
                  (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%))
          (setf (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%) k)
         label27
          (setf qm (f2cl-lib:fref q-%data% (np1) ((1 *)) q-%offset%))
          (if (/= qm k) (go label105))
          (if (= luk 0) (go label34))
          (if (/= lastid luk) (go label28))
          (setf irul (f2cl-lib:fref iru-%data% (lasti) ((1 *)) iru-%offset%))
          (setf (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%)
                  (f2cl-lib:int-add irul 1))
          (if (/= (f2cl-lib:fref ju-%data% (irul) ((1 *)) ju-%offset%) k)
              (setf (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%)
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%)
                       1)))
          (go label34)
         label28
          (if (> jumin juptr) (go label32))
          (setf qm (f2cl-lib:fref q-%data% (qm) ((1 *)) q-%offset%))
          (f2cl-lib:fdo (j jumin (f2cl-lib:int-add j 1))
                        ((> j juptr) nil)
            (tagbody
              (f2cl-lib:arithmetic-if
               (f2cl-lib:int-sub
                (f2cl-lib:fref ju-%data% (j) ((1 *)) ju-%offset%)
                qm)
               (go label29)
               (go label30)
               (go label32))
             label29))
          (go label32)
         label30
          (setf (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%) j)
          (f2cl-lib:fdo (i j (f2cl-lib:int-add i 1))
                        ((> i juptr) nil)
            (tagbody
              (if (/= (f2cl-lib:fref ju-%data% (i) ((1 *)) ju-%offset%) qm)
                  (go label32))
              (setf qm (f2cl-lib:fref q-%data% (qm) ((1 *)) q-%offset%))
              (if (> qm n) (go label34))
             label31))
          (setf juptr (f2cl-lib:int-sub j 1))
         label32
          (setf jumin (f2cl-lib:int-add juptr 1))
          (setf (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%) jumin)
          (if (= luk 0) (go label34))
          (setf juptr (f2cl-lib:int-add juptr luk))
          (if (> juptr jumax) (go label106))
          (setf qm (f2cl-lib:fref q-%data% (np1) ((1 *)) q-%offset%))
          (f2cl-lib:fdo (j jumin (f2cl-lib:int-add j 1))
                        ((> j juptr) nil)
            (tagbody
              (setf qm (f2cl-lib:fref q-%data% (qm) ((1 *)) q-%offset%))
             label33
              (setf (f2cl-lib:fref ju-%data% (j) ((1 *)) ju-%offset%) qm)))
         label34
          (setf (f2cl-lib:fref iru-%data% (k) ((1 *)) iru-%offset%)
                  (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%))
          (setf (f2cl-lib:fref iu-%data%
                               ((f2cl-lib:int-add k 1))
                               ((1 *))
                               iu-%offset%)
                  (f2cl-lib:int-add
                   (f2cl-lib:fref iu-%data% (k) ((1 *)) iu-%offset%)
                   luk))
          (setf i k)
         label35
          (setf i1 (f2cl-lib:fref jru-%data% (i) ((1 *)) jru-%offset%))
          (if (< (f2cl-lib:fref r-%data% (i) ((1 *)) r-%offset%) 0)
              (go label36))
          (setf rend
                  (f2cl-lib:int-sub
                   (f2cl-lib:int-add
                    (f2cl-lib:fref iju-%data% (i) ((1 *)) iju-%offset%)
                    (f2cl-lib:fref iu-%data%
                                   ((f2cl-lib:int-add i 1))
                                   ((1 *))
                                   iu-%offset%))
                   (f2cl-lib:fref iu-%data% (i) ((1 *)) iu-%offset%)))
          (if (>= (f2cl-lib:fref iru-%data% (i) ((1 *)) iru-%offset%) rend)
              (go label37))
          (setf j
                  (f2cl-lib:fref ju-%data%
                                 ((f2cl-lib:fref iru (i) ((1 *))))
                                 ((1 *))
                                 ju-%offset%))
          (setf (f2cl-lib:fref jru-%data% (i) ((1 *)) jru-%offset%)
                  (f2cl-lib:fref jru-%data% (j) ((1 *)) jru-%offset%))
          (setf (f2cl-lib:fref jru-%data% (j) ((1 *)) jru-%offset%) i)
          (go label37)
         label36
          (setf (f2cl-lib:fref r-%data% (i) ((1 *)) r-%offset%)
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref r-%data% (i) ((1 *)) r-%offset%)))
         label37
          (setf i i1)
          (if (= i 0) (go label38))
          (setf (f2cl-lib:fref iru-%data% (i) ((1 *)) iru-%offset%)
                  (f2cl-lib:int-add
                   (f2cl-lib:fref iru-%data% (i) ((1 *)) iru-%offset%)
                   1))
          (go label35)
         label38
          (setf i (f2cl-lib:fref irac-%data% (k) ((1 *)) irac-%offset%))
          (if (= i 0) (go label41))
         label39
          (setf i1 (f2cl-lib:fref jra-%data% (i) ((1 *)) jra-%offset%))
          (setf (f2cl-lib:fref ira-%data% (i) ((1 *)) ira-%offset%)
                  (f2cl-lib:int-add
                   (f2cl-lib:fref ira-%data% (i) ((1 *)) ira-%offset%)
                   1))
          (if
           (>= (f2cl-lib:fref ira-%data% (i) ((1 *)) ira-%offset%)
               (f2cl-lib:fref ia-%data%
                              ((f2cl-lib:int-add (f2cl-lib:fref r (i) ((1 *)))
                                                 1))
                              ((1 *))
                              ia-%offset%))
           (go label40))
          (setf irai (f2cl-lib:fref ira-%data% (i) ((1 *)) ira-%offset%))
          (setf jairai
                  (f2cl-lib:fref ic-%data%
                                 ((f2cl-lib:fref ja (irai) ((1 *))))
                                 ((1 *))
                                 ic-%offset%))
          (if (> jairai i) (go label40))
          (setf (f2cl-lib:fref jra-%data% (i) ((1 *)) jra-%offset%)
                  (f2cl-lib:fref irac-%data% (jairai) ((1 *)) irac-%offset%))
          (setf (f2cl-lib:fref irac-%data% (jairai) ((1 *)) irac-%offset%) i)
         label40
          (setf i i1)
          (if (/= i 0) (go label39))
         label41))
      (setf (f2cl-lib:fref ijl-%data% (n) ((1 *)) ijl-%offset%) jlptr)
      (setf (f2cl-lib:fref iju-%data% (n) ((1 *)) iju-%offset%) juptr)
      (setf flag 0)
      (go end_label)
     label101
      (setf flag (f2cl-lib:int-add n rk))
      (go end_label)
     label102
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) rk))
      (go end_label)
     label103
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) k))
      (go end_label)
     label105
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 5 n) k))
      (go end_label)
     label106
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 6 n) k))
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
               flag)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::nsfc fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil
                            fortran-to-lisp::flag)
           :calls 'nil)))

