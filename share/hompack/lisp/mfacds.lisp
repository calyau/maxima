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


(defun mfacds (nn q lenaa maxa)
  (declare (type (array f2cl-lib:integer4 (*)) maxa)
           (type (array double-float (*)) q)
           (type (f2cl-lib:integer4) lenaa nn))
  (f2cl-lib:with-multi-array-data
      ((q double-float q-%data% q-%offset%)
       (maxa f2cl-lib:integer4 maxa-%data% maxa-%offset%))
    (prog ((i 0) (imax 0) (lenq 0) (nq 0))
      (declare (type (f2cl-lib:integer4) nq lenq imax i))
      (setf nq (f2cl-lib:int-add nn 1))
      (setf imax
              (f2cl-lib:int-sub
               (f2cl-lib:fref maxa-%data%
                              ((f2cl-lib:int-add nn 2))
                              ((1 (f2cl-lib:int-add nn 2)))
                              maxa-%offset%)
               lenaa
               2))
      (setf lenq
              (f2cl-lib:int-sub
               (f2cl-lib:fref maxa-%data%
                              ((f2cl-lib:int-add nn 2))
                              ((1 (f2cl-lib:int-add nn 2)))
                              maxa-%offset%)
               1))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i imax) nil)
        (tagbody
          (setf (f2cl-lib:fref q-%data%
                               ((f2cl-lib:int-add lenaa i))
                               ((1 (f2cl-lib:int-add lenaa nn 1)))
                               q-%offset%)
                  (coerce 0.0f0 'double-float))
         label100))
      (setf (f2cl-lib:fref q-%data%
                           (lenq)
                           ((1 (f2cl-lib:int-add lenaa nn 1)))
                           q-%offset%)
              1.0)
      (gmfads nq q lenq maxa)
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mfacds
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil nil)
           :calls '(fortran-to-lisp::gmfads))))

