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


(defun mdi (n ia ja max v l head last$ next mark tag flag)
  (declare (type (array f2cl-lib:integer4 (*)) mark next last$ head l v ja ia)
           (type (f2cl-lib:integer4) flag tag max n))
  (f2cl-lib:with-multi-array-data
      ((ia f2cl-lib:integer4 ia-%data% ia-%offset%)
       (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
       (v f2cl-lib:integer4 v-%data% v-%offset%)
       (l f2cl-lib:integer4 l-%data% l-%offset%)
       (head f2cl-lib:integer4 head-%data% head-%offset%)
       (last$ f2cl-lib:integer4 last$-%data% last$-%offset%)
       (next f2cl-lib:integer4 next-%data% next-%offset%)
       (mark f2cl-lib:integer4 mark-%data% mark-%offset%))
    (prog ((sfs 0) (vi 0) (dvi 0) (vj 0) (nextvi 0) (k 0) (kmax 0) (lvk 0)
           (j 0) (jmax 0) (jmin 0))
      (declare (type (f2cl-lib:integer4) jmin jmax j lvk kmax k nextvi vj dvi
                                         vi sfs))
      (f2cl-lib:fdo (vi 1 (f2cl-lib:int-add vi 1))
                    ((> vi n) nil)
        (tagbody
          (setf (f2cl-lib:fref mark-%data% (vi) ((1 *)) mark-%offset%) 1)
          (setf (f2cl-lib:fref l-%data% (vi) ((1 *)) l-%offset%) 0)
         label1
          (setf (f2cl-lib:fref head-%data% (vi) ((1 *)) head-%offset%) 0)))
      (setf sfs (f2cl-lib:int-add n 1))
      (f2cl-lib:fdo (vi 1 (f2cl-lib:int-add vi 1))
                    ((> vi n) nil)
        (tagbody
          (setf jmin (f2cl-lib:fref ia-%data% (vi) ((1 *)) ia-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref ia-%data%
                                  ((f2cl-lib:int-add vi 1))
                                  ((1 *))
                                  ia-%offset%)
                   1))
          (if (> jmin jmax) (go label6))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
              (setf vj (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%))
              (f2cl-lib:arithmetic-if (f2cl-lib:int-sub vj vi)
                                      (go label2)
                                      (go label5)
                                      (go label4))
             label2
              (setf lvk vi)
              (setf kmax
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref mark-%data% (vi) ((1 *)) mark-%offset%)
                       1))
              (if (= kmax 0) (go label4))
              (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                            ((> k kmax) nil)
                (tagbody
                  (setf lvk (f2cl-lib:fref l-%data% (lvk) ((1 *)) l-%offset%))
                  (if (= (f2cl-lib:fref v-%data% (lvk) ((1 *)) v-%offset%) vj)
                      (go label5))
                 label3))
             label4
              (if (>= sfs max) (go label101))
              (setf (f2cl-lib:fref mark-%data% (vi) ((1 *)) mark-%offset%)
                      (f2cl-lib:int-add
                       (f2cl-lib:fref mark-%data% (vi) ((1 *)) mark-%offset%)
                       1))
              (setf (f2cl-lib:fref v-%data% (sfs) ((1 *)) v-%offset%) vj)
              (setf (f2cl-lib:fref l-%data% (sfs) ((1 *)) l-%offset%)
                      (f2cl-lib:fref l-%data% (vi) ((1 *)) l-%offset%))
              (setf (f2cl-lib:fref l-%data% (vi) ((1 *)) l-%offset%) sfs)
              (setf sfs (f2cl-lib:int-add sfs 1))
              (setf (f2cl-lib:fref mark-%data% (vj) ((1 *)) mark-%offset%)
                      (f2cl-lib:int-add
                       (f2cl-lib:fref mark-%data% (vj) ((1 *)) mark-%offset%)
                       1))
              (setf (f2cl-lib:fref v-%data% (sfs) ((1 *)) v-%offset%) vi)
              (setf (f2cl-lib:fref l-%data% (sfs) ((1 *)) l-%offset%)
                      (f2cl-lib:fref l-%data% (vj) ((1 *)) l-%offset%))
              (setf (f2cl-lib:fref l-%data% (vj) ((1 *)) l-%offset%) sfs)
              (setf sfs (f2cl-lib:int-add sfs 1))
             label5))
         label6))
      (f2cl-lib:fdo (vi 1 (f2cl-lib:int-add vi 1))
                    ((> vi n) nil)
        (tagbody
          (setf dvi (f2cl-lib:fref mark-%data% (vi) ((1 *)) mark-%offset%))
          (setf (f2cl-lib:fref next-%data% (vi) ((1 *)) next-%offset%)
                  (f2cl-lib:fref head-%data% (dvi) ((1 *)) head-%offset%))
          (setf (f2cl-lib:fref head-%data% (dvi) ((1 *)) head-%offset%) vi)
          (setf (f2cl-lib:fref last$-%data% (vi) ((1 *)) last$-%offset%)
                  (f2cl-lib:int-sub dvi))
          (setf nextvi (f2cl-lib:fref next-%data% (vi) ((1 *)) next-%offset%))
          (if (> nextvi 0)
              (setf (f2cl-lib:fref last$-%data%
                                   (nextvi)
                                   ((1 *))
                                   last$-%offset%)
                      vi))
         label7
          (setf (f2cl-lib:fref mark-%data% (vi) ((1 *)) mark-%offset%) tag)))
      (go end_label)
     label101
      (setf flag (f2cl-lib:int-add (f2cl-lib:int-mul 9 n) vi))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil nil flag)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mdi fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::flag)
           :calls 'nil)))

