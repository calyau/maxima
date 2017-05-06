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


(let ((lenrls 218) (lenils 37))
  (declare (type (f2cl-lib:integer4) lenrls lenils))
  (defun dsrcom (rsav isav job)
    (declare (type (f2cl-lib:integer4) job)
             (type (array f2cl-lib:integer4 (*)) isav)
             (type (array double-float (*)) rsav))
    (let ((dls001-rls
           (make-array 218
                       :element-type 'double-float
                       :displaced-to (dls001-part-0 *dls001-common-block*)
                       :displaced-index-offset 0))
          (dls001-ils
           (make-array 37
                       :element-type 'f2cl-lib:integer4
                       :displaced-to (dls001-part-1 *dls001-common-block*)
                       :displaced-index-offset 0)))
      (symbol-macrolet ((rls dls001-rls) (ils dls001-ils))
        (f2cl-lib:with-multi-array-data
            ((rsav double-float rsav-%data% rsav-%offset%)
             (isav f2cl-lib:integer4 isav-%data% isav-%offset%))
          (prog ((i 0))
            (declare (type (f2cl-lib:integer4) i))
            (if (= job 2) (go label100))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenrls) nil)
              (tagbody
               label10
                (setf (f2cl-lib:fref rsav-%data% (i) ((1 *)) rsav-%offset%)
                        (f2cl-lib:fref rls (i) ((1 218))))))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenils) nil)
              (tagbody
               label20
                (setf (f2cl-lib:fref isav-%data% (i) ((1 *)) isav-%offset%)
                        (f2cl-lib:fref ils (i) ((1 37))))))
            (go end_label)
           label100
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenrls) nil)
              (tagbody
               label110
                (setf (f2cl-lib:fref rls (i) ((1 218)))
                        (f2cl-lib:fref rsav-%data%
                                       (i)
                                       ((1 *))
                                       rsav-%offset%))))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenils) nil)
              (tagbody
               label120
                (setf (f2cl-lib:fref ils (i) ((1 37)))
                        (f2cl-lib:fref isav-%data%
                                       (i)
                                       ((1 *))
                                       isav-%offset%))))
            (go end_label)
           end_label
            (return (values nil nil nil))))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dsrcom
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil)
           :calls 'nil)))

