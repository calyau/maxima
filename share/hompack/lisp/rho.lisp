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


(defun rho (a lambda$ x v par ipar)
  (declare (type (array f2cl-lib:integer4 (*)) ipar)
           (type (double-float) lambda$)
           (type (array double-float (*)) par v x a))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (x double-float x-%data% x-%offset%)
       (v double-float v-%data% v-%offset%)
       (par double-float par-%data% par-%offset%)
       (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
    (prog ((j 0) (n 0))
      (declare (type (f2cl-lib:integer4) n j))
      (if (< lambda$ 0.0f0) (setf lambda$ (coerce 0.0f0 'double-float)))
      (hfunp a lambda$ x par ipar)
      (setf n (f2cl-lib:fref ipar-%data% (1) ((1 *)) ipar-%offset%))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j (f2cl-lib:int-mul 2 n)) nil)
        (tagbody
          (setf (f2cl-lib:fref v-%data% (j) ((1 *)) v-%offset%)
                  (f2cl-lib:fref par-%data%
                                 ((f2cl-lib:int-add
                                   (f2cl-lib:fref ipar
                                                  ((f2cl-lib:int-add 3
                                                                     (f2cl-lib:int-sub
                                                                      4
                                                                      1)))
                                                  ((1 *)))
                                   (f2cl-lib:int-sub j 1)))
                                 ((1 *))
                                 par-%offset%))
         label10))
      (go end_label)
     end_label
      (return (values nil lambda$ nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::rho fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil fortran-to-lisp::lambda$ nil nil nil nil)
           :calls '(fortran-to-lisp::hfunp))))

