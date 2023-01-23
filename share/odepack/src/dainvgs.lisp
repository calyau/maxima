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


(defun dainvgs (neq t$ y wk iwk tem ydot ier res adda)
  (declare (type (array f2cl-lib:integer4 (*)) iwk)
           (type (array double-float (*)) ydot tem wk y)
           (type (double-float) t$)
           (type (f2cl-lib:integer4) ier neq))
  (let ()
    (symbol-macrolet ((iesp (aref (dlss01-part-1 *dlss01-common-block*) 1))
                      (iys (aref (dlss01-part-1 *dlss01-common-block*) 3))
                      (iba (aref (dlss01-part-1 *dlss01-common-block*) 4))
                      (ibjan (aref (dlss01-part-1 *dlss01-common-block*) 6))
                      (ipian (aref (dlss01-part-1 *dlss01-common-block*) 8))
                      (ipjan (aref (dlss01-part-1 *dlss01-common-block*) 9))
                      (ipr (aref (dlss01-part-1 *dlss01-common-block*) 12))
                      (ipc (aref (dlss01-part-1 *dlss01-common-block*) 13))
                      (ipic (aref (dlss01-part-1 *dlss01-common-block*) 14))
                      (ipisp (aref (dlss01-part-1 *dlss01-common-block*) 15))
                      (iprsp (aref (dlss01-part-1 *dlss01-common-block*) 16))
                      (ipa (aref (dlss01-part-1 *dlss01-common-block*) 17))
                      (nlu (aref (dlss01-part-1 *dlss01-common-block*) 29))
                      (nnz (aref (dlss01-part-1 *dlss01-common-block*) 30))
                      (nsp (aref (dlss01-part-1 *dlss01-common-block*) 31)))
      (f2cl-lib:with-multi-array-data
          ((y double-float y-%data% y-%offset%)
           (wk double-float wk-%data% wk-%offset%)
           (tem double-float tem-%data% tem-%offset%)
           (ydot double-float ydot-%data% ydot-%offset%)
           (iwk f2cl-lib:integer4 iwk-%data% iwk-%offset%))
        (prog ((kmax 0) (kmin 0) (k 0) (j 0) (imul 0) (i 0))
          (declare (type (f2cl-lib:integer4) i imul j k kmin kmax))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nnz) nil)
            (tagbody
             label10
              (setf (f2cl-lib:fref wk-%data%
                                   ((f2cl-lib:int-add iba i))
                                   ((1 *))
                                   wk-%offset%)
                      0.0d0)))
          (setf ier 1)
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
              (funcall res
                       neq
                       t$
                       y
                       (f2cl-lib:array-slice wk-%data%
                                             double-float
                                             (ipa)
                                             ((1 *))
                                             wk-%offset%)
                       ydot
                       ier)
            (declare (ignore var-2 var-3 var-4))
            (when var-0
              (setf neq var-0))
            (when var-1
              (setf t$ var-1))
            (when var-5
              (setf ier var-5)))
          (if (> ier 1) (go end_label))
          (setf kmin (f2cl-lib:fref iwk-%data% (ipian) ((1 *)) iwk-%offset%))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j neq) nil)
            (tagbody
              (setf kmax
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref iwk-%data%
                                      ((f2cl-lib:int-add ipian j))
                                      ((1 *))
                                      iwk-%offset%)
                       1))
              (f2cl-lib:fdo (k kmin (f2cl-lib:int-add k 1))
                            ((> k kmax) nil)
                (tagbody
                  (setf i
                          (f2cl-lib:fref iwk-%data%
                                         ((f2cl-lib:int-add ibjan k))
                                         ((1 *))
                                         iwk-%offset%))
                 label15
                  (setf (f2cl-lib:fref tem-%data% (i) ((1 *)) tem-%offset%)
                          0.0d0)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (funcall adda
                           neq
                           t$
                           y
                           j
                           (f2cl-lib:array-slice iwk-%data%
                                                 f2cl-lib:integer4
                                                 (ipian)
                                                 ((1 *))
                                                 iwk-%offset%)
                           (f2cl-lib:array-slice iwk-%data%
                                                 f2cl-lib:integer4
                                                 (ipjan)
                                                 ((1 *))
                                                 iwk-%offset%)
                           tem)
                (declare (ignore var-2 var-4 var-5 var-6))
                (when var-0
                  (setf neq var-0))
                (when var-1
                  (setf t$ var-1))
                (when var-3
                  (setf j var-3)))
              (f2cl-lib:fdo (k kmin (f2cl-lib:int-add k 1))
                            ((> k kmax) nil)
                (tagbody
                  (setf i
                          (f2cl-lib:fref iwk-%data%
                                         ((f2cl-lib:int-add ibjan k))
                                         ((1 *))
                                         iwk-%offset%))
                 label20
                  (setf (f2cl-lib:fref wk-%data%
                                       ((f2cl-lib:int-add iba k))
                                       ((1 *))
                                       wk-%offset%)
                          (f2cl-lib:fref tem-%data%
                                         (i)
                                         ((1 *))
                                         tem-%offset%))))
              (setf kmin (f2cl-lib:int-add kmax 1))
             label30))
          (setf nlu (f2cl-lib:int-add nlu 1))
          (setf ier 0)
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i neq) nil)
            (tagbody
             label40
              (setf (f2cl-lib:fref tem-%data% (i) ((1 *)) tem-%offset%)
                      0.0d0)))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14)
              (cdrv neq
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipr)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipc)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipic)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipian)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipjan)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (ipa)
                                     ((1 *))
                                     wk-%offset%)
               tem tem nsp
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipisp)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (iprsp)
                                     ((1 *))
                                     wk-%offset%)
               iesp 2 iys)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10 var-11 var-13))
            (setf iesp var-12)
            (setf iys var-14))
          (if (= iys 0) (go label50))
          (setf imul (the f2cl-lib:integer4 (truncate (- iys 1) neq)))
          (setf ier 5)
          (if (= imul 8) (setf ier 1))
          (if (= imul 10) (setf ier 4))
          (go end_label)
         label50
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14)
              (cdrv neq
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipr)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipc)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipic)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipian)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipjan)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (ipa)
                                     ((1 *))
                                     wk-%offset%)
               ydot ydot nsp
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipisp)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (iprsp)
                                     ((1 *))
                                     wk-%offset%)
               iesp 4 iys)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10 var-11 var-13))
            (setf iesp var-12)
            (setf iys var-14))
          (if (/= iys 0) (setf ier 5))
          (go end_label)
         end_label
          (return (values neq t$ nil nil nil nil nil ier nil nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dainvgs
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) t t)
           :return-values '(fortran-to-lisp::neq fortran-to-lisp::t$ nil nil
                            nil nil nil fortran-to-lisp::ier nil nil)
           :calls '(fortran-to-lisp::cdrv))))

