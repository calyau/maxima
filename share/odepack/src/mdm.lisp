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


(defun mdm (vk tail v l last$ next mark)
  (declare (type (array f2cl-lib:integer4 (*)) mark next last$ l v)
           (type (f2cl-lib:integer4) tail vk))
  (f2cl-lib:with-multi-array-data
      ((v f2cl-lib:integer4 v-%data% v-%offset%)
       (l f2cl-lib:integer4 l-%data% l-%offset%)
       (last$ f2cl-lib:integer4 last$-%data% last$-%offset%)
       (next f2cl-lib:integer4 next-%data% next-%offset%)
       (mark f2cl-lib:integer4 mark-%data% mark-%offset%))
    (symbol-macrolet ((vs es))
      (prog ((tag 0) (s 0) (ls 0) (es 0) (b 0) (lb 0) (vb 0) (blp 0)
             (blpmax 0))
        (declare (type (f2cl-lib:integer4) blpmax blp vb lb b es vs ls s tag))
        (setf tag (f2cl-lib:fref mark-%data% (vk) ((1 *)) mark-%offset%))
        (setf tail vk)
        (setf ls (f2cl-lib:fref l-%data% (vk) ((1 *)) l-%offset%))
       label1
        (setf s ls)
        (if (= s 0) (go label5))
        (setf ls (f2cl-lib:fref l-%data% (s) ((1 *)) l-%offset%))
        (setf vs (f2cl-lib:fref v-%data% (s) ((1 *)) v-%offset%))
        (if (< (f2cl-lib:fref next-%data% (vs) ((1 *)) next-%offset%) 0)
            (go label2))
        (setf (f2cl-lib:fref mark-%data% (vs) ((1 *)) mark-%offset%) tag)
        (setf (f2cl-lib:fref l-%data% (tail) ((1 *)) l-%offset%) s)
        (setf tail s)
        (go label4)
       label2
        (setf lb (f2cl-lib:fref l-%data% (es) ((1 *)) l-%offset%))
        (setf blpmax (f2cl-lib:fref last$-%data% (es) ((1 *)) last$-%offset%))
        (f2cl-lib:fdo (blp 1 (f2cl-lib:int-add blp 1))
                      ((> blp blpmax) nil)
          (tagbody
            (setf b lb)
            (setf lb (f2cl-lib:fref l-%data% (b) ((1 *)) l-%offset%))
            (setf vb (f2cl-lib:fref v-%data% (b) ((1 *)) v-%offset%))
            (if (>= (f2cl-lib:fref mark-%data% (vb) ((1 *)) mark-%offset%) tag)
                (go label3))
            (setf (f2cl-lib:fref mark-%data% (vb) ((1 *)) mark-%offset%) tag)
            (setf (f2cl-lib:fref l-%data% (tail) ((1 *)) l-%offset%) b)
            (setf tail b)
           label3))
        (setf (f2cl-lib:fref mark-%data% (es) ((1 *)) mark-%offset%) tag)
       label4
        (go label1)
       label5
        (setf (f2cl-lib:fref l-%data% (tail) ((1 *)) l-%offset%) 0)
        (go end_label)
       end_label
        (return (values nil tail nil nil nil nil nil))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mdm fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil fortran-to-lisp::tail nil nil nil nil nil)
           :calls 'nil)))

