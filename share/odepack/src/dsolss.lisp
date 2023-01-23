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


(defun dsolss (wk iwk x tem)
  (declare (type (array f2cl-lib:integer4 (*)) iwk)
           (type (array double-float (*)) tem x wk))
  (let ()
    (symbol-macrolet ((el0 (aref (dls001-part-0 *dls001-common-block*) 210))
                      (h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (iersl (aref (dls001-part-1 *dls001-common-block*) 14))
                      (miter (aref (dls001-part-1 *dls001-common-block*) 26))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (iesp (aref (dlss01-part-1 *dlss01-common-block*) 1))
                      (ipian (aref (dlss01-part-1 *dlss01-common-block*) 8))
                      (ipjan (aref (dlss01-part-1 *dlss01-common-block*) 9))
                      (ipr (aref (dlss01-part-1 *dlss01-common-block*) 12))
                      (ipc (aref (dlss01-part-1 *dlss01-common-block*) 13))
                      (ipic (aref (dlss01-part-1 *dlss01-common-block*) 14))
                      (ipisp (aref (dlss01-part-1 *dlss01-common-block*) 15))
                      (iprsp (aref (dlss01-part-1 *dlss01-common-block*) 16))
                      (ipa (aref (dlss01-part-1 *dlss01-common-block*) 17))
                      (nsp (aref (dlss01-part-1 *dlss01-common-block*) 31)))
      (f2cl-lib:with-multi-array-data
          ((wk double-float wk-%data% wk-%offset%)
           (x double-float x-%data% x-%offset%)
           (tem double-float tem-%data% tem-%offset%)
           (iwk f2cl-lib:integer4 iwk-%data% iwk-%offset%))
        (prog ((i 0) (r 0.0d0) (phl0 0.0d0) (hl0 0.0d0) (di 0.0d0))
          (declare (type (double-float) di hl0 phl0 r)
                   (type (f2cl-lib:integer4) i))
          (setf iersl 0)
          (f2cl-lib:computed-goto (label100 label100 label300) miter)
         label100
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14)
              (cdrv n
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
               x x nsp
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
               iesp 4 iersl)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10 var-11 var-13))
            (setf iesp var-12)
            (setf iersl var-14))
          (if (/= iersl 0) (setf iersl -1))
          (go end_label)
         label300
          (setf phl0 (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%))
          (setf hl0 (* h el0))
          (setf (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%) hl0)
          (if (= hl0 phl0) (go label330))
          (setf r (/ hl0 phl0))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf di
                      (- 1.0d0
                         (* r
                            (+ 1.0d0
                               (/ -1.0d0
                                  (f2cl-lib:fref wk-%data%
                                                 ((f2cl-lib:int-add i 2))
                                                 ((1 *))
                                                 wk-%offset%))))))
              (if (= (abs di) 0.0d0) (go label390))
             label320
              (setf (f2cl-lib:fref wk-%data%
                                   ((f2cl-lib:int-add i 2))
                                   ((1 *))
                                   wk-%offset%)
                      (/ 1.0d0 di))))
         label330
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label340
              (setf (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)
                      (*
                       (f2cl-lib:fref wk-%data%
                                      ((f2cl-lib:int-add i 2))
                                      ((1 *))
                                      wk-%offset%)
                       (f2cl-lib:fref x-%data% (i) ((1 *)) x-%offset%)))))
          (go end_label)
         label390
          (setf iersl 1)
          (go end_label)
         end_label
          (return (values nil nil nil nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dsolss
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil)
           :calls '(fortran-to-lisp::cdrv))))

