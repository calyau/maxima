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


(defun nnsc (n r c il jl ijl l d iu ju iju u z b tmp)
  (declare (type (array double-float (*)) tmp b z u d l)
           (type (array f2cl-lib:integer4 (*)) iju ju iu ijl jl il c r)
           (type (f2cl-lib:integer4) n))
  (f2cl-lib:with-multi-array-data
      ((r f2cl-lib:integer4 r-%data% r-%offset%)
       (c f2cl-lib:integer4 c-%data% c-%offset%)
       (il f2cl-lib:integer4 il-%data% il-%offset%)
       (jl f2cl-lib:integer4 jl-%data% jl-%offset%)
       (ijl f2cl-lib:integer4 ijl-%data% ijl-%offset%)
       (iu f2cl-lib:integer4 iu-%data% iu-%offset%)
       (ju f2cl-lib:integer4 ju-%data% ju-%offset%)
       (iju f2cl-lib:integer4 iju-%data% iju-%offset%)
       (l double-float l-%data% l-%offset%)
       (d double-float d-%data% d-%offset%)
       (u double-float u-%data% u-%offset%)
       (z double-float z-%data% z-%offset%)
       (b double-float b-%data% b-%offset%)
       (tmp double-float tmp-%data% tmp-%offset%))
    (prog ((tmpk 0.0d0) (sum 0.0d0) (mu 0) (i 0) (j 0) (ml 0) (jmax 0) (jmin 0)
           (k 0))
      (declare (type (f2cl-lib:integer4) k jmin jmax ml j i mu)
               (type (double-float) sum tmpk))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
         label1
          (setf (f2cl-lib:fref tmp-%data% (k) ((1 *)) tmp-%offset%)
                  (f2cl-lib:fref b-%data%
                                 ((f2cl-lib:fref r (k) ((1 *))))
                                 ((1 *))
                                 b-%offset%))))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf jmin (f2cl-lib:fref il-%data% (k) ((1 *)) il-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref il-%data%
                                  ((f2cl-lib:int-add k 1))
                                  ((1 *))
                                  il-%offset%)
                   1))
          (setf tmpk
                  (* (- (f2cl-lib:fref d-%data% (k) ((1 *)) d-%offset%))
                     (f2cl-lib:fref tmp-%data% (k) ((1 *)) tmp-%offset%)))
          (setf (f2cl-lib:fref tmp-%data% (k) ((1 *)) tmp-%offset%) (- tmpk))
          (if (> jmin jmax) (go label3))
          (setf ml
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref ijl-%data% (k) ((1 *)) ijl-%offset%)
                   jmin))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
             label2
              (setf (f2cl-lib:fref tmp-%data%
                                   ((f2cl-lib:fref jl
                                                   ((f2cl-lib:int-add ml j))
                                                   ((1 *))))
                                   ((1 *))
                                   tmp-%offset%)
                      (+
                       (f2cl-lib:fref tmp-%data%
                                      ((f2cl-lib:fref jl
                                                      ((f2cl-lib:int-add ml j))
                                                      ((1 *))))
                                      ((1 *))
                                      tmp-%offset%)
                       (* tmpk
                          (f2cl-lib:fref l-%data% (j) ((1 *)) l-%offset%))))))
         label3))
      (setf k n)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf sum (- (f2cl-lib:fref tmp-%data% (k) ((1 *)) tmp-%offset%)))
          (setf jmin (f2cl-lib:fref iu-%data% (k) ((1 *)) iu-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iu-%data%
                                  ((f2cl-lib:int-add k 1))
                                  ((1 *))
                                  iu-%offset%)
                   1))
          (if (> jmin jmax) (go label5))
          (setf mu
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iju-%data% (k) ((1 *)) iju-%offset%)
                   jmin))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
             label4
              (setf sum
                      (+ sum
                         (* (f2cl-lib:fref u-%data% (j) ((1 *)) u-%offset%)
                            (f2cl-lib:fref tmp-%data%
                                           ((f2cl-lib:fref ju
                                                           ((f2cl-lib:int-add
                                                             mu
                                                             j))
                                                           ((1 *))))
                                           ((1 *))
                                           tmp-%offset%))))))
         label5
          (setf (f2cl-lib:fref tmp-%data% (k) ((1 *)) tmp-%offset%) (- sum))
          (setf (f2cl-lib:fref z-%data%
                               ((f2cl-lib:fref c (k) ((1 *))))
                               ((1 *))
                               z-%offset%)
                  (- sum))
          (setf k (f2cl-lib:int-sub k 1))
         label6))
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::nnsc fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil)
           :calls 'nil)))

