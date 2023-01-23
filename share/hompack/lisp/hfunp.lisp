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


(defun hfunp (qdg lambda$ x par ipar)
  (declare (type (array f2cl-lib:integer4 (*)) ipar)
           (type (array double-float (*)) par)
           (type (double-float) lambda$)
           (type (array double-float (*)) x qdg))
  (f2cl-lib:with-multi-array-data
      ((qdg double-float qdg-%data% qdg-%offset%)
       (x double-float x-%data% x-%offset%)
       (par double-float par-%data% par-%offset%)
       (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
    (prog ()
      (declare)
      (hfun1p qdg lambda$ x
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 1
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 2
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 3
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 4
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 5
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 6
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 7
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 8
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 9
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 10
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 11
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 12
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 13
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 14
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 15
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 16
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 17
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 18
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:array-slice par-%data%
                             double-float
                             ((f2cl-lib:fref ipar
                                             ((+ 3
                                                 (f2cl-lib:int-add 19
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             par-%offset%)
       (f2cl-lib:fref ipar-%data%
                      ((f2cl-lib:fref ipar
                                      ((f2cl-lib:int-add 28
                                                         (f2cl-lib:int-sub 1
                                                                           1)))
                                      ((1 *))))
                      ((1 *))
                      ipar-%offset%)
       (f2cl-lib:fref ipar-%data%
                      ((f2cl-lib:fref ipar
                                      ((f2cl-lib:int-add 28
                                                         (f2cl-lib:int-sub 2
                                                                           1)))
                                      ((1 *))))
                      ((1 *))
                      ipar-%offset%)
       (f2cl-lib:array-slice ipar-%data%
                             f2cl-lib:integer4
                             ((f2cl-lib:fref ipar
                                             ((+ 28
                                                 (f2cl-lib:int-add 5
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             ipar-%offset%)
       (f2cl-lib:array-slice ipar-%data%
                             f2cl-lib:integer4
                             ((f2cl-lib:fref ipar
                                             ((+ 28
                                                 (f2cl-lib:int-add 6
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             ipar-%offset%)
       (f2cl-lib:array-slice ipar-%data%
                             f2cl-lib:integer4
                             ((f2cl-lib:fref ipar
                                             ((+ 28
                                                 (f2cl-lib:int-add 7
                                                                   (f2cl-lib:int-sub
                                                                    1))))
                                             ((1 *))))
                             ((1 *))
                             ipar-%offset%))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::hfunp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil nil)
           :calls '(fortran-to-lisp::hfun1p))))

