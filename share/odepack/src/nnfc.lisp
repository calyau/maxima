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


(defun nnfc
       (n r c ic ia ja a z b lmax il jl ijl l d umax iu ju iju u row tmp irl
        jrl flag)
  (declare (type (array double-float (*)) tmp row u d l b z a)
           (type (array f2cl-lib:integer4 (*)) jrl irl iju ju iu ijl jl il ja
                                               ia ic c r)
           (type (f2cl-lib:integer4) flag umax lmax n))
  (f2cl-lib:with-multi-array-data
      ((r f2cl-lib:integer4 r-%data% r-%offset%)
       (c f2cl-lib:integer4 c-%data% c-%offset%)
       (ic f2cl-lib:integer4 ic-%data% ic-%offset%)
       (ia f2cl-lib:integer4 ia-%data% ia-%offset%)
       (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
       (il f2cl-lib:integer4 il-%data% il-%offset%)
       (jl f2cl-lib:integer4 jl-%data% jl-%offset%)
       (ijl f2cl-lib:integer4 ijl-%data% ijl-%offset%)
       (iu f2cl-lib:integer4 iu-%data% iu-%offset%)
       (ju f2cl-lib:integer4 ju-%data% ju-%offset%)
       (iju f2cl-lib:integer4 iju-%data% iju-%offset%)
       (irl f2cl-lib:integer4 irl-%data% irl-%offset%)
       (jrl f2cl-lib:integer4 jrl-%data% jrl-%offset%)
       (a double-float a-%data% a-%offset%)
       (z double-float z-%data% z-%offset%)
       (b double-float b-%data% b-%offset%)
       (l double-float l-%data% l-%offset%)
       (d double-float d-%data% d-%offset%)
       (u double-float u-%data% u-%offset%)
       (row double-float row-%data% row-%offset%)
       (tmp double-float tmp-%data% tmp-%offset%))
    (prog ((lki 0.0d0) (sum 0.0d0) (dk 0.0d0) (rk 0) (ijlb 0) (mu 0) (j 0)
           (jmax 0) (jmin 0) (i2 0) (i 0) (i1 0) (k 0))
      (declare (type (f2cl-lib:integer4) k i1 i i2 jmin jmax j mu ijlb rk)
               (type (double-float) dk sum lki))
      (if
       (>
        (f2cl-lib:int-sub
         (f2cl-lib:fref il-%data% ((f2cl-lib:int-add n 1)) ((1 *)) il-%offset%)
         1)
        lmax)
       (go label104))
      (if
       (>
        (f2cl-lib:int-sub
         (f2cl-lib:fref iu-%data% ((f2cl-lib:int-add n 1)) ((1 *)) iu-%offset%)
         1)
        umax)
       (go label107))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf (f2cl-lib:fref irl-%data% (k) ((1 *)) irl-%offset%)
                  (f2cl-lib:fref il-%data% (k) ((1 *)) il-%offset%))
          (setf (f2cl-lib:fref jrl-%data% (k) ((1 *)) jrl-%offset%) 0)
         label1))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf (f2cl-lib:fref row-%data% (k) ((1 *)) row-%offset%)
                  (coerce (the f2cl-lib:integer4 0) 'double-float))
          (setf i1 0)
          (if (= (f2cl-lib:fref jrl-%data% (k) ((1 *)) jrl-%offset%) 0)
              (go label3))
          (setf i (f2cl-lib:fref jrl-%data% (k) ((1 *)) jrl-%offset%))
         label2
          (setf i2 (f2cl-lib:fref jrl-%data% (i) ((1 *)) jrl-%offset%))
          (setf (f2cl-lib:fref jrl-%data% (i) ((1 *)) jrl-%offset%) i1)
          (setf i1 i)
          (setf (f2cl-lib:fref row-%data% (i) ((1 *)) row-%offset%)
                  (coerce (the f2cl-lib:integer4 0) 'double-float))
          (setf i i2)
          (if (/= i 0) (go label2))
         label3
          (setf jmin (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:int-add jmin
                                     (f2cl-lib:fref iu-%data%
                                                    ((f2cl-lib:int-add k 1))
                                                    ((1 *))
                                                    iu-%offset%))
                   (f2cl-lib:fref iu-%data% (k) ((1 *)) iu-%offset%)
                   1))
          (if (> jmin jmax) (go label5))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
             label4
              (setf (f2cl-lib:fref row-%data%
                                   ((f2cl-lib:fref ju (j) ((1 *))))
                                   ((1 *))
                                   row-%offset%)
                      (coerce (the f2cl-lib:integer4 0) 'double-float))))
         label5
          (setf rk (f2cl-lib:fref r-%data% (k) ((1 *)) r-%offset%))
          (setf jmin (f2cl-lib:fref ia-%data% (rk) ((1 *)) ia-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref ia-%data%
                                  ((f2cl-lib:int-add rk 1))
                                  ((1 *))
                                  ia-%offset%)
                   1))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
              (setf (f2cl-lib:fref row-%data%
                                   ((f2cl-lib:fref ic
                                                   ((f2cl-lib:fref ja
                                                                   (j)
                                                                   ((1 *))))
                                                   ((1 *))))
                                   ((1 *))
                                   row-%offset%)
                      (f2cl-lib:fref a-%data% (j) ((1 *)) a-%offset%))
             label6))
          (setf sum (f2cl-lib:fref b-%data% (rk) ((1 *)) b-%offset%))
          (setf i i1)
          (if (= i 0) (go label10))
         label7
          (setf lki (- (f2cl-lib:fref row-%data% (i) ((1 *)) row-%offset%)))
          (setf (f2cl-lib:fref l-%data%
                               ((f2cl-lib:fref irl (i) ((1 *))))
                               ((1 *))
                               l-%offset%)
                  (- lki))
          (setf sum
                  (+ sum
                     (* lki
                        (f2cl-lib:fref tmp-%data% (i) ((1 *)) tmp-%offset%))))
          (setf jmin (f2cl-lib:fref iu-%data% (i) ((1 *)) iu-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iu-%data%
                                  ((f2cl-lib:int-add i 1))
                                  ((1 *))
                                  iu-%offset%)
                   1))
          (if (> jmin jmax) (go label9))
          (setf mu
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iju-%data% (i) ((1 *)) iju-%offset%)
                   jmin))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
             label8
              (setf (f2cl-lib:fref row-%data%
                                   ((f2cl-lib:fref ju
                                                   ((f2cl-lib:int-add mu j))
                                                   ((1 *))))
                                   ((1 *))
                                   row-%offset%)
                      (+
                       (f2cl-lib:fref row-%data%
                                      ((f2cl-lib:fref ju
                                                      ((f2cl-lib:int-add mu j))
                                                      ((1 *))))
                                      ((1 *))
                                      row-%offset%)
                       (* lki
                          (f2cl-lib:fref u-%data% (j) ((1 *)) u-%offset%))))))
         label9
          (setf i (f2cl-lib:fref jrl-%data% (i) ((1 *)) jrl-%offset%))
          (if (/= i 0) (go label7))
         label10
          (if (= (f2cl-lib:fref row-%data% (k) ((1 *)) row-%offset%) 0.0d0)
              (go label108))
          (setf dk
                  (/ 1.0d0
                     (f2cl-lib:fref row-%data% (k) ((1 *)) row-%offset%)))
          (setf (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%) dk)
          (setf (f2cl-lib:fref tmp-%data% (k) ((1 *)) tmp-%offset%) (* sum dk))
          (if (= k n) (go label19))
          (setf jmin (f2cl-lib:fref iu-%data% (k) ((1 *)) iu-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iu-%data%
                                  ((f2cl-lib:int-add k 1))
                                  ((1 *))
                                  iu-%offset%)
                   1))
          (if (> jmin jmax) (go label12))
          (setf mu
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%)
                   jmin))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
             label11
              (setf (f2cl-lib:fref u-%data% (j) ((1 *)) u-%offset%)
                      (*
                       (f2cl-lib:fref row-%data%
                                      ((f2cl-lib:fref ju
                                                      ((f2cl-lib:int-add mu j))
                                                      ((1 *))))
                                      ((1 *))
                                      row-%offset%)
                       dk))))
         label12
          (setf i i1)
          (if (= i 0) (go label18))
         label14
          (setf (f2cl-lib:fref irl-%data% (i) ((1 *)) irl-%offset%)
                  (f2cl-lib:int-add
                   (f2cl-lib:fref irl-%data% (i) ((1 *)) irl-%offset%)
                   1))
          (setf i1 (f2cl-lib:fref jrl-%data% (i) ((1 *)) jrl-%offset%))
          (if
           (>= (f2cl-lib:fref irl-%data% (i) ((1 *)) irl-%offset%)
               (f2cl-lib:fref il-%data%
                              ((f2cl-lib:int-add i 1))
                              ((1 *))
                              il-%offset%))
           (go label17))
          (setf ijlb
                  (f2cl-lib:int-add
                   (f2cl-lib:int-sub
                    (f2cl-lib:fref irl-%data% (i) ((1 *)) irl-%offset%)
                    (f2cl-lib:fref il-%data% (i) ((1 *)) il-%offset%))
                   (f2cl-lib:fref ijl-%data% (i) ((1 *)) ijl-%offset%)))
          (setf j (f2cl-lib:fref jl-%data% (ijlb) ((1 *)) jl-%offset%))
         label15
          (if (> i (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%))
              (go label16))
          (setf j (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%))
          (go label15)
         label16
          (setf (f2cl-lib:fref jrl-%data% (i) ((1 *)) jrl-%offset%)
                  (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%))
          (setf (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%) i)
         label17
          (setf i i1)
          (if (/= i 0) (go label14))
         label18
          (if
           (>= (f2cl-lib:fref irl-%data% (k) ((1 *)) irl-%offset%)
               (f2cl-lib:fref il-%data%
                              ((f2cl-lib:int-add k 1))
                              ((1 *))
                              il-%offset%))
           (go label19))
          (setf j
                  (f2cl-lib:fref jl-%data%
                                 ((f2cl-lib:fref ijl (k) ((1 *))))
                                 ((1 *))
                                 jl-%offset%))
          (setf (f2cl-lib:fref jrl-%data% (k) ((1 *)) jrl-%offset%)
                  (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%))
          (setf (f2cl-lib:fref jrl-%data% (j) ((1 *)) jrl-%offset%) k)
         label19))
      (setf k n)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf sum (f2cl-lib:fref tmp-%data% (k) ((1 *)) tmp-%offset%))
          (setf jmin (f2cl-lib:fref iu-%data% (k) ((1 *)) iu-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iu-%data%
                                  ((f2cl-lib:int-add k 1))
                                  ((1 *))
                                  iu-%offset%)
                   1))
          (if (> jmin jmax) (go label21))
          (setf mu
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%)
                   jmin))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
             label20
              (setf sum
                      (- sum
                         (* (f2cl-lib:fref u-%data% (j) ((1 *)) u-%offset%)
                            (f2cl-lib:fref tmp-%data%
                                           ((f2cl-lib:fref ju
                                                           ((f2cl-lib:int-add
                                                             mu
                                                             j))
                                                           ((1 *))))
                                           ((1 *))
                                           tmp-%offset%))))))
         label21
          (setf (f2cl-lib:fref tmp-%data% (k) ((1 *)) tmp-%offset%) sum)
          (setf (f2cl-lib:fref z-%data%
                               ((f2cl-lib:fref c (k) ((1 *))))
                               ((1 *))
                               z-%offset%)
                  sum)
         label22
          (setf k (f2cl-lib:int-sub k 1))))
      (setf flag 0)
      (go end_label)
     label104
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 4 n) 1))
      (go end_label)
     label107
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 7 n) 1))
      (go end_label)
     label108
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 8 n) k))
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
               nil
               nil
               nil
               flag)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::nnfc fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::flag)
           :calls 'nil)))

