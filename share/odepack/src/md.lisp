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


(defun md (n ia ja max v l head last$ next mark flag)
  (declare (type (array f2cl-lib:integer4 (*)) mark next last$ head l v ja ia)
           (type (f2cl-lib:integer4) flag max n))
  (f2cl-lib:with-multi-array-data
      ((ia f2cl-lib:integer4 ia-%data% ia-%offset%)
       (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
       (v f2cl-lib:integer4 v-%data% v-%offset%)
       (l f2cl-lib:integer4 l-%data% l-%offset%)
       (head f2cl-lib:integer4 head-%data% head-%offset%)
       (last$ f2cl-lib:integer4 last$-%data% last$-%offset%)
       (next f2cl-lib:integer4 next-%data% next-%offset%)
       (mark f2cl-lib:integer4 mark-%data% mark-%offset%))
    (symbol-macrolet ((vk ek))
      (prog ((tag 0) (dmin 0) (ek 0) (tail 0) (k 0))
        (declare (type (f2cl-lib:integer4) k tail ek vk dmin tag))
        (setf tag 0)
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11)
            (mdi n ia ja max v l head last$ next mark tag flag)
          (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                           var-8 var-9 var-10))
          (setf flag var-11))
        (if (/= flag 0) (go end_label))
        (setf k 0)
        (setf dmin 1)
       label1
        (if (>= k n) (go label4))
       label2
        (if (> (f2cl-lib:fref head-%data% (dmin) ((1 *)) head-%offset%) 0)
            (go label3))
        (setf dmin (f2cl-lib:int-add dmin 1))
        (go label2)
       label3
        (setf vk (f2cl-lib:fref head-%data% (dmin) ((1 *)) head-%offset%))
        (setf (f2cl-lib:fref head-%data% (dmin) ((1 *)) head-%offset%)
                (f2cl-lib:fref next-%data% (vk) ((1 *)) next-%offset%))
        (if (> (f2cl-lib:fref head-%data% (dmin) ((1 *)) head-%offset%) 0)
            (setf (f2cl-lib:fref last$-%data%
                                 ((f2cl-lib:fref head (dmin) ((1 *))))
                                 ((1 *))
                                 last$-%offset%)
                    (f2cl-lib:int-sub dmin)))
        (setf k (f2cl-lib:int-add k 1))
        (setf (f2cl-lib:fref next-%data% (vk) ((1 *)) next-%offset%)
                (f2cl-lib:int-sub k))
        (setf (f2cl-lib:fref last$-%data% (ek) ((1 *)) last$-%offset%)
                (f2cl-lib:int-sub dmin 1))
        (setf tag
                (f2cl-lib:int-add tag
                                  (f2cl-lib:fref last$-%data%
                                                 (ek)
                                                 ((1 *))
                                                 last$-%offset%)))
        (setf (f2cl-lib:fref mark-%data% (vk) ((1 *)) mark-%offset%) tag)
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
            (mdm vk tail v l last$ next mark)
          (declare (ignore var-0 var-2 var-3 var-4 var-5 var-6))
          (setf tail var-1))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
            (mdp k ek tail v l head last$ next mark)
          (declare (ignore var-1 var-3 var-4 var-5 var-6 var-7 var-8))
          (setf k var-0)
          (setf tail var-2))
        (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
            (mdu ek dmin v l head last$ next mark)
          (declare (ignore var-0 var-2 var-3 var-4 var-5 var-6 var-7))
          (setf dmin var-1))
        (go label1)
       label4
        (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                      ((> k n) nil)
          (tagbody
            (setf (f2cl-lib:fref next-%data% (k) ((1 *)) next-%offset%)
                    (f2cl-lib:int-sub
                     (f2cl-lib:fref next-%data% (k) ((1 *)) next-%offset%)))
           label5
            (setf (f2cl-lib:fref last$-%data%
                                 ((f2cl-lib:fref next (k) ((1 *))))
                                 ((1 *))
                                 last$-%offset%)
                    k)))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil nil nil nil flag))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::md fortran-to-lisp::*f2cl-function-info*)
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
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::flag)
           :calls '(fortran-to-lisp::mdu fortran-to-lisp::mdp
                    fortran-to-lisp::mdm fortran-to-lisp::mdi))))

