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


(defun mdu (ek dmin v l head last$ next mark)
  (declare (type (array f2cl-lib:integer4 (*)) mark next last$ head l v)
           (type (f2cl-lib:integer4) dmin ek))
  (f2cl-lib:with-multi-array-data
      ((v f2cl-lib:integer4 v-%data% v-%offset%)
       (l f2cl-lib:integer4 l-%data% l-%offset%)
       (head f2cl-lib:integer4 head-%data% head-%offset%)
       (last$ f2cl-lib:integer4 last$-%data% last$-%offset%)
       (next f2cl-lib:integer4 next-%data% next-%offset%)
       (mark f2cl-lib:integer4 mark-%data% mark-%offset%))
    (symbol-macrolet ((vs es))
      (prog ((tag 0) (vi 0) (evi 0) (dvi 0) (s 0) (es 0) (b 0) (vb 0) (ilp 0)
             (ilpmax 0) (blp 0) (blpmax 0) (i 0))
        (declare (type (f2cl-lib:integer4) i blpmax blp ilpmax ilp vb b es vs s
                                           dvi evi vi tag))
        (setf tag
                (f2cl-lib:int-sub
                 (f2cl-lib:fref mark-%data% (ek) ((1 *)) mark-%offset%)
                 (f2cl-lib:fref last$-%data% (ek) ((1 *)) last$-%offset%)))
        (setf i ek)
        (setf ilpmax (f2cl-lib:fref last$-%data% (ek) ((1 *)) last$-%offset%))
        (if (<= ilpmax 0) (go label11))
        (f2cl-lib:fdo (ilp 1 (f2cl-lib:int-add ilp 1))
                      ((> ilp ilpmax) nil)
          (tagbody
            (setf i (f2cl-lib:fref l-%data% (i) ((1 *)) l-%offset%))
            (setf vi (f2cl-lib:fref v-%data% (i) ((1 *)) v-%offset%))
            (f2cl-lib:arithmetic-if
             (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%)
             (go label1)
             (go label10)
             (go label8))
           label1
            (setf tag (f2cl-lib:int-add tag 1))
            (setf dvi (f2cl-lib:fref last$-%data% (ek) ((1 *)) last$-%offset%))
            (setf s (f2cl-lib:fref l-%data% (vi) ((1 *)) l-%offset%))
           label2
            (setf s (f2cl-lib:fref l-%data% (s) ((1 *)) l-%offset%))
            (if (= s 0) (go label9))
            (setf vs (f2cl-lib:fref v-%data% (s) ((1 *)) v-%offset%))
            (if (< (f2cl-lib:fref next-%data% (vs) ((1 *)) next-%offset%) 0)
                (go label3))
            (setf (f2cl-lib:fref mark-%data% (vs) ((1 *)) mark-%offset%) tag)
            (setf dvi (f2cl-lib:int-add dvi 1))
            (go label5)
           label3
            (if (< (f2cl-lib:fref mark-%data% (es) ((1 *)) mark-%offset%) 0)
                (go label6))
            (setf b es)
            (setf blpmax
                    (f2cl-lib:fref last$-%data% (es) ((1 *)) last$-%offset%))
            (f2cl-lib:fdo (blp 1 (f2cl-lib:int-add blp 1))
                          ((> blp blpmax) nil)
              (tagbody
                (setf b (f2cl-lib:fref l-%data% (b) ((1 *)) l-%offset%))
                (setf vb (f2cl-lib:fref v-%data% (b) ((1 *)) v-%offset%))
                (if
                 (>= (f2cl-lib:fref mark-%data% (vb) ((1 *)) mark-%offset%)
                     tag)
                 (go label4))
                (setf (f2cl-lib:fref mark-%data% (vb) ((1 *)) mark-%offset%)
                        tag)
                (setf dvi (f2cl-lib:int-add dvi 1))
               label4))
           label5
            (go label2)
           label6
            (setf (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%) 0)
            (setf (f2cl-lib:fref mark-%data% (es) ((1 *)) mark-%offset%)
                    (f2cl-lib:int-sub
                     (f2cl-lib:fref mark-%data% (es) ((1 *)) mark-%offset%)
                     1))
           label7
            (setf s (f2cl-lib:fref l-%data% (s) ((1 *)) l-%offset%))
            (if (= s 0) (go label10))
            (setf es (f2cl-lib:fref v-%data% (s) ((1 *)) v-%offset%))
            (if (< (f2cl-lib:fref mark-%data% (es) ((1 *)) mark-%offset%) 0)
                (setf (f2cl-lib:fref mark-%data% (es) ((1 *)) mark-%offset%)
                        (f2cl-lib:int-sub
                         (f2cl-lib:fref mark-%data% (es) ((1 *)) mark-%offset%)
                         1)))
            (go label7)
           label8
            (setf evi (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%))
            (setf dvi
                    (f2cl-lib:int-add
                     (f2cl-lib:fref last$-%data% (ek) ((1 *)) last$-%offset%)
                     (f2cl-lib:fref last$-%data% (evi) ((1 *)) last$-%offset%)
                     (f2cl-lib:fref mark-%data% (evi) ((1 *)) mark-%offset%)))
            (setf (f2cl-lib:fref mark-%data% (evi) ((1 *)) mark-%offset%) 0)
           label9
            (setf (f2cl-lib:fref next-%data% (vi) ((1 *)) next-%offset%)
                    (f2cl-lib:fref head-%data% (dvi) ((1 *)) head-%offset%))
            (setf (f2cl-lib:fref head-%data% (dvi) ((1 *)) head-%offset%) vi)
            (setf (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%)
                    (f2cl-lib:int-sub dvi))
            (if (> (f2cl-lib:fref next-%data% (vi) ((1 *)) next-%offset%) 0)
                (setf (f2cl-lib:fref last$-%data%
                                     ((f2cl-lib:fref next (vi) ((1 *))))
                                     ((1 *))
                                     last$-%offset%)
                        vi))
            (if (< dvi dmin) (setf dmin dvi))
           label10))
       label11
        (go end_label)
       end_label
        (return (values nil dmin nil nil nil nil nil nil))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mdu fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil fortran-to-lisp::dmin nil nil nil nil nil nil)
           :calls 'nil)))

