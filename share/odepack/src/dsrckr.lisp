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


(let ((lenrls 218) (lenils 37) (lenrlp 4) (lenilp 13) (lenrlr 5) (lenilr 9))
  (declare (type (f2cl-lib:integer4) lenrls lenils lenrlp lenilp lenrlr
                                     lenilr))
  (defun dsrckr (rsav isav job)
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
                       :displaced-index-offset 0))
          (dls002-ils2
           (make-array 4
                       :element-type 'f2cl-lib:integer4
                       :displaced-to (dls002-part-1 *dls002-common-block*)
                       :displaced-index-offset 0))
          (dlsr01-rlsr
           (make-array 5
                       :element-type 'double-float
                       :displaced-to (dlsr01-part-0 *dlsr01-common-block*)
                       :displaced-index-offset 0))
          (dlsr01-ilsr
           (make-array 9
                       :element-type 'f2cl-lib:integer4
                       :displaced-to (dlsr01-part-1 *dlsr01-common-block*)
                       :displaced-index-offset 0))
          (dlpk01-rlsp
           (make-array 4
                       :element-type 'double-float
                       :displaced-to (dlpk01-part-0 *dlpk01-common-block*)
                       :displaced-index-offset 0))
          (dlpk01-ilsp
           (make-array 13
                       :element-type 'f2cl-lib:integer4
                       :displaced-to (dlpk01-part-1 *dlpk01-common-block*)
                       :displaced-index-offset 0)))
      (symbol-macrolet ((rls dls001-rls)
                        (ils dls001-ils)
                        (rls2 (aref (dls002-part-0 *dls002-common-block*) 0))
                        (ils2 dls002-ils2)
                        (rlsr dlsr01-rlsr)
                        (ilsr dlsr01-ilsr)
                        (rlsp dlpk01-rlsp)
                        (ilsp dlpk01-ilsp))
        (f2cl-lib:with-multi-array-data
            ((rsav double-float rsav-%data% rsav-%offset%)
             (isav f2cl-lib:integer4 isav-%data% isav-%offset%))
          (prog ((ioff 0) (i 0))
            (declare (type (f2cl-lib:integer4) i ioff))
            (if (= job 2) (go label100))
            (dcopy lenrls rls 1 rsav 1)
            (setf (f2cl-lib:fref rsav-%data%
                                 ((f2cl-lib:int-add lenrls 1))
                                 ((1 *))
                                 rsav-%offset%)
                    rls2)
            (dcopy lenrlr rlsr 1
             (f2cl-lib:array-slice rsav-%data%
                                   double-float
                                   ((+ lenrls 2))
                                   ((1 *))
                                   rsav-%offset%)
             1)
            (dcopy lenrlp rlsp 1
             (f2cl-lib:array-slice rsav-%data%
                                   double-float
                                   ((+ lenrls lenrlr 2))
                                   ((1 *))
                                   rsav-%offset%)
             1)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenils) nil)
              (tagbody
               label20
                (setf (f2cl-lib:fref isav-%data% (i) ((1 *)) isav-%offset%)
                        (f2cl-lib:fref ils (i) ((1 37))))))
            (setf (f2cl-lib:fref isav-%data%
                                 ((f2cl-lib:int-add lenils 1))
                                 ((1 *))
                                 isav-%offset%)
                    (f2cl-lib:fref ils2 (1) ((1 4))))
            (setf (f2cl-lib:fref isav-%data%
                                 ((f2cl-lib:int-add lenils 2))
                                 ((1 *))
                                 isav-%offset%)
                    (f2cl-lib:fref ils2 (2) ((1 4))))
            (setf (f2cl-lib:fref isav-%data%
                                 ((f2cl-lib:int-add lenils 3))
                                 ((1 *))
                                 isav-%offset%)
                    (f2cl-lib:fref ils2 (3) ((1 4))))
            (setf (f2cl-lib:fref isav-%data%
                                 ((f2cl-lib:int-add lenils 4))
                                 ((1 *))
                                 isav-%offset%)
                    (f2cl-lib:fref ils2 (4) ((1 4))))
            (setf ioff (f2cl-lib:int-add lenils 2))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenilr) nil)
              (tagbody
               label30
                (setf (f2cl-lib:fref isav-%data%
                                     ((f2cl-lib:int-add ioff i))
                                     ((1 *))
                                     isav-%offset%)
                        (f2cl-lib:fref ilsr (i) ((1 9))))))
            (setf ioff (f2cl-lib:int-add ioff lenilr))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenilp) nil)
              (tagbody
               label40
                (setf (f2cl-lib:fref isav-%data%
                                     ((f2cl-lib:int-add ioff i))
                                     ((1 *))
                                     isav-%offset%)
                        (f2cl-lib:fref ilsp (i) ((1 13))))))
            (go end_label)
           label100
            (dcopy lenrls rsav 1 rls 1)
            (setf rls2
                    (f2cl-lib:fref rsav-%data%
                                   ((f2cl-lib:int-add lenrls 1))
                                   ((1 *))
                                   rsav-%offset%))
            (dcopy lenrlr
             (f2cl-lib:array-slice rsav-%data%
                                   double-float
                                   ((+ lenrls 2))
                                   ((1 *))
                                   rsav-%offset%)
             1 rlsr 1)
            (dcopy lenrlp
             (f2cl-lib:array-slice rsav-%data%
                                   double-float
                                   ((+ lenrls lenrlr 2))
                                   ((1 *))
                                   rsav-%offset%)
             1 rlsp 1)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenils) nil)
              (tagbody
               label120
                (setf (f2cl-lib:fref ils (i) ((1 37)))
                        (f2cl-lib:fref isav-%data%
                                       (i)
                                       ((1 *))
                                       isav-%offset%))))
            (setf (f2cl-lib:fref ils2 (1) ((1 4)))
                    (f2cl-lib:fref isav-%data%
                                   ((f2cl-lib:int-add lenils 1))
                                   ((1 *))
                                   isav-%offset%))
            (setf (f2cl-lib:fref ils2 (2) ((1 4)))
                    (f2cl-lib:fref isav-%data%
                                   ((f2cl-lib:int-add lenils 2))
                                   ((1 *))
                                   isav-%offset%))
            (setf (f2cl-lib:fref ils2 (3) ((1 4)))
                    (f2cl-lib:fref isav-%data%
                                   ((f2cl-lib:int-add lenils 3))
                                   ((1 *))
                                   isav-%offset%))
            (setf (f2cl-lib:fref ils2 (4) ((1 4)))
                    (f2cl-lib:fref isav-%data%
                                   ((f2cl-lib:int-add lenils 4))
                                   ((1 *))
                                   isav-%offset%))
            (setf ioff (f2cl-lib:int-add lenils 2))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenilr) nil)
              (tagbody
               label130
                (setf (f2cl-lib:fref ilsr (i) ((1 9)))
                        (f2cl-lib:fref isav-%data%
                                       ((f2cl-lib:int-add ioff i))
                                       ((1 *))
                                       isav-%offset%))))
            (setf ioff (f2cl-lib:int-add ioff lenilr))
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i lenilp) nil)
              (tagbody
               label140
                (setf (f2cl-lib:fref ilsp (i) ((1 13)))
                        (f2cl-lib:fref isav-%data%
                                       ((f2cl-lib:int-add ioff i))
                                       ((1 *))
                                       isav-%offset%))))
            (go end_label)
           end_label
            (return (values nil nil nil))))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dsrckr
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil)
           :calls '(fortran-to-lisp::dcopy))))

