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


(defun jgroup (n ia ja maxg ngrp igp jgp incl jdone ier)
  (declare (type (array f2cl-lib:integer4 (*)) jdone incl jgp igp ja ia)
           (type (f2cl-lib:integer4) ier ngrp maxg n))
  (f2cl-lib:with-multi-array-data
      ((ia f2cl-lib:integer4 ia-%data% ia-%offset%)
       (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
       (igp f2cl-lib:integer4 igp-%data% igp-%offset%)
       (jgp f2cl-lib:integer4 jgp-%data% jgp-%offset%)
       (incl f2cl-lib:integer4 incl-%data% incl-%offset%)
       (jdone f2cl-lib:integer4 jdone-%data% jdone-%offset%))
    (prog ((i 0) (j 0) (k 0) (kmin 0) (kmax 0) (ncol 0) (ng 0))
      (declare (type (f2cl-lib:integer4) ng ncol kmax kmin k j i))
      (setf ier 0)
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
         label10
          (setf (f2cl-lib:fref jdone-%data% (j) ((1 *)) jdone-%offset%) 0)))
      (setf ncol 1)
      (f2cl-lib:fdo (ng 1 (f2cl-lib:int-add ng 1))
                    ((> ng maxg) nil)
        (tagbody
          (setf (f2cl-lib:fref igp-%data% (ng) ((1 *)) igp-%offset%) ncol)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label20
              (setf (f2cl-lib:fref incl-%data% (i) ((1 *)) incl-%offset%) 0)))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (if (= (f2cl-lib:fref jdone-%data% (j) ((1 *)) jdone-%offset%) 1)
                  (go label50))
              (setf kmin (f2cl-lib:fref ia-%data% (j) ((1 *)) ia-%offset%))
              (setf kmax
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref ia-%data%
                                      ((f2cl-lib:int-add j 1))
                                      ((1 *))
                                      ia-%offset%)
                       1))
              (f2cl-lib:fdo (k kmin (f2cl-lib:int-add k 1))
                            ((> k kmax) nil)
                (tagbody
                  (setf i (f2cl-lib:fref ja-%data% (k) ((1 *)) ja-%offset%))
                  (if
                   (= (f2cl-lib:fref incl-%data% (i) ((1 *)) incl-%offset%) 1)
                   (go label50))
                 label30))
              (setf (f2cl-lib:fref jgp-%data% (ncol) ((1 *)) jgp-%offset%) j)
              (setf ncol (f2cl-lib:int-add ncol 1))
              (setf (f2cl-lib:fref jdone-%data% (j) ((1 *)) jdone-%offset%) 1)
              (f2cl-lib:fdo (k kmin (f2cl-lib:int-add k 1))
                            ((> k kmax) nil)
                (tagbody
                  (setf i (f2cl-lib:fref ja-%data% (k) ((1 *)) ja-%offset%))
                 label40
                  (setf (f2cl-lib:fref incl-%data% (i) ((1 *)) incl-%offset%)
                          1)))
             label50))
          (if (= ncol (f2cl-lib:fref igp-%data% (ng) ((1 *)) igp-%offset%))
              (go label70))
         label60))
      (if (<= ncol n) (go label80))
      (setf ng maxg)
     label70
      (setf ngrp (f2cl-lib:int-sub ng 1))
      (go end_label)
     label80
      (setf ier 1)
      (go end_label)
     end_label
      (return (values nil nil nil nil ngrp nil nil nil nil ier)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::jgroup
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil fortran-to-lisp::ngrp nil nil nil
                            nil fortran-to-lisp::ier)
           :calls 'nil)))

