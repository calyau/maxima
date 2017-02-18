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


(defun mdp (k ek tail v l head last$ next mark)
  (declare (type (array f2cl-lib:integer4 (*)) mark next last$ head l v)
           (type (f2cl-lib:integer4) tail ek k))
  (f2cl-lib:with-multi-array-data
      ((v f2cl-lib:integer4 v-%data% v-%offset%)
       (l f2cl-lib:integer4 l-%data% l-%offset%)
       (head f2cl-lib:integer4 head-%data% head-%offset%)
       (last$ f2cl-lib:integer4 last$-%data% last$-%offset%)
       (next f2cl-lib:integer4 next-%data% next-%offset%)
       (mark f2cl-lib:integer4 mark-%data% mark-%offset%))
    (prog ((tag 0) (free 0) (li 0) (vi 0) (lvi 0) (evi 0) (s 0) (ls 0) (es 0)
           (ilp 0) (ilpmax 0) (i 0))
      (declare (type (f2cl-lib:integer4) i ilpmax ilp es ls s evi lvi vi li
                                         free tag))
      (setf tag (f2cl-lib:fref mark-%data% (ek) ((1 *)) mark-%offset%))
      (setf li ek)
      (setf ilpmax (f2cl-lib:fref last$-%data% (ek) ((1 *)) last$-%offset%))
      (if (<= ilpmax 0) (go label12))
      (f2cl-lib:fdo (ilp 1 (f2cl-lib:int-add ilp 1))
                    ((> ilp ilpmax) nil)
        (tagbody
          (setf i li)
          (setf li (f2cl-lib:fref l-%data% (i) ((1 *)) l-%offset%))
          (setf vi (f2cl-lib:fref v-%data% (li) ((1 *)) v-%offset%))
          (if (= (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%) 0)
              (go label3))
          (if (> (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%) 0)
              (go label1))
          (setf (f2cl-lib:fref head-%data%
                               ((f2cl-lib:int-sub
                                 (f2cl-lib:fref last$ (vi) ((1 *)))))
                               ((1 *))
                               head-%offset%)
                  (f2cl-lib:fref next-%data% (vi) ((1 *)) next-%offset%))
          (go label2)
         label1
          (setf (f2cl-lib:fref next-%data%
                               ((f2cl-lib:fref last$ (vi) ((1 *))))
                               ((1 *))
                               next-%offset%)
                  (f2cl-lib:fref next-%data% (vi) ((1 *)) next-%offset%))
         label2
          (if (> (f2cl-lib:fref next-%data% (vi) ((1 *)) next-%offset%) 0)
              (setf (f2cl-lib:fref last$-%data%
                                   ((f2cl-lib:fref next (vi) ((1 *))))
                                   ((1 *))
                                   last$-%offset%)
                      (f2cl-lib:fref last$-%data%
                                     (vi)
                                     ((1 *))
                                     last$-%offset%)))
         label3
          (setf ls vi)
         label4
          (setf s ls)
          (setf ls (f2cl-lib:fref l-%data% (s) ((1 *)) l-%offset%))
          (if (= ls 0) (go label6))
          (setf es (f2cl-lib:fref v-%data% (ls) ((1 *)) v-%offset%))
          (if (< (f2cl-lib:fref mark-%data% (es) ((1 *)) mark-%offset%) tag)
              (go label5))
          (setf free ls)
          (setf (f2cl-lib:fref l-%data% (s) ((1 *)) l-%offset%)
                  (f2cl-lib:fref l-%data% (ls) ((1 *)) l-%offset%))
          (setf ls s)
         label5
          (go label4)
         label6
          (setf lvi (f2cl-lib:fref l-%data% (vi) ((1 *)) l-%offset%))
          (if (/= lvi 0) (go label7))
          (setf (f2cl-lib:fref l-%data% (i) ((1 *)) l-%offset%)
                  (f2cl-lib:fref l-%data% (li) ((1 *)) l-%offset%))
          (setf li i)
          (setf k (f2cl-lib:int-add k 1))
          (setf (f2cl-lib:fref next-%data% (vi) ((1 *)) next-%offset%)
                  (f2cl-lib:int-sub k))
          (setf (f2cl-lib:fref last$-%data% (ek) ((1 *)) last$-%offset%)
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref last$-%data% (ek) ((1 *)) last$-%offset%)
                   1))
          (go label11)
         label7
          (if (/= (f2cl-lib:fref l-%data% (lvi) ((1 *)) l-%offset%) 0)
              (go label9))
          (setf evi (f2cl-lib:fref v-%data% (lvi) ((1 *)) v-%offset%))
          (if (>= (f2cl-lib:fref next-%data% (evi) ((1 *)) next-%offset%) 0)
              (go label9))
          (if (< (f2cl-lib:fref mark-%data% (evi) ((1 *)) mark-%offset%) 0)
              (go label8))
          (setf (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%) evi)
          (setf (f2cl-lib:fref mark-%data% (evi) ((1 *)) mark-%offset%) -1)
          (setf (f2cl-lib:fref l-%data% (tail) ((1 *)) l-%offset%) li)
          (setf tail li)
          (setf (f2cl-lib:fref l-%data% (i) ((1 *)) l-%offset%)
                  (f2cl-lib:fref l-%data% (li) ((1 *)) l-%offset%))
          (setf li i)
          (go label10)
         label8
          (setf (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%) 0)
          (setf (f2cl-lib:fref mark-%data% (evi) ((1 *)) mark-%offset%)
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref mark-%data% (evi) ((1 *)) mark-%offset%)
                   1))
          (go label10)
         label9
          (setf (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%)
                  (f2cl-lib:int-sub ek))
         label10
          (setf (f2cl-lib:fref v-%data% (free) ((1 *)) v-%offset%) ek)
          (setf (f2cl-lib:fref l-%data% (free) ((1 *)) l-%offset%)
                  (f2cl-lib:fref l-%data% (vi) ((1 *)) l-%offset%))
          (setf (f2cl-lib:fref l-%data% (vi) ((1 *)) l-%offset%) free)
         label11))
     label12
      (setf (f2cl-lib:fref l-%data% (tail) ((1 *)) l-%offset%) 0)
      (go end_label)
     end_label
      (return (values k nil tail nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mdp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(fortran-to-lisp::k nil fortran-to-lisp::tail nil
                            nil nil nil nil nil)
           :calls 'nil)))

