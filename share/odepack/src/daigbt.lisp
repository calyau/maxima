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


(defun daigbt (res adda neq t$ y ydot mb nb pw ipvt ier)
  (declare (type (f2cl-lib:integer4) ier nb mb)
           (type (array double-float (*)) pw ydot y)
           (type (double-float) t$)
           (type (array f2cl-lib:integer4 (*)) ipvt neq))
  (f2cl-lib:with-multi-array-data
      ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
       (ipvt f2cl-lib:integer4 ipvt-%data% ipvt-%offset%)
       (y double-float y-%data% y-%offset%)
       (ydot double-float ydot-%data% ydot-%offset%)
       (pw double-float pw-%data% pw-%offset%))
    (prog ((i 0) (lenpw 0) (lblox 0) (lpb 0) (lpc 0))
      (declare (type (f2cl-lib:integer4) lpc lpb lblox lenpw i))
      (setf lblox (f2cl-lib:int-mul mb mb nb))
      (setf lpb (f2cl-lib:int-add 1 lblox))
      (setf lpc (f2cl-lib:int-add lpb lblox))
      (setf lenpw (f2cl-lib:int-mul 3 lblox))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i lenpw) nil)
        (tagbody
         label10
          (setf (f2cl-lib:fref pw-%data% (i) ((1 *)) pw-%offset%) 0.0d0)))
      (setf ier 1)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
          (funcall res neq t$ y pw ydot ier)
        (declare (ignore var-0 var-2 var-3 var-4))
        (when var-1
          (setf t$ var-1))
        (when var-5
          (setf ier var-5)))
      (if (> ier 1) (go end_label))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7)
          (funcall adda
                   neq
                   t$
                   y
                   mb
                   nb
                   (f2cl-lib:array-slice pw-%data%
                                         double-float
                                         (1)
                                         ((1 *))
                                         pw-%offset%)
                   (f2cl-lib:array-slice pw-%data%
                                         double-float
                                         (lpb)
                                         ((1 *))
                                         pw-%offset%)
                   (f2cl-lib:array-slice pw-%data%
                                         double-float
                                         (lpc)
                                         ((1 *))
                                         pw-%offset%))
        (declare (ignore var-0 var-2 var-5 var-6 var-7))
        (when var-1
          (setf t$ var-1))
        (when var-3
          (setf mb var-3))
        (when var-4
          (setf nb var-4)))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
          (ddecbt mb nb pw
           (f2cl-lib:array-slice pw-%data%
                                 double-float
                                 (lpb)
                                 ((1 *))
                                 pw-%offset%)
           (f2cl-lib:array-slice pw-%data%
                                 double-float
                                 (lpc)
                                 ((1 *))
                                 pw-%offset%)
           ipvt ier)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
        (setf ier var-6))
      (if (= ier 0) (go label20))
      (setf ier (f2cl-lib:int-sub ier))
      (go end_label)
     label20
      (dsolbt mb nb pw
       (f2cl-lib:array-slice pw-%data% double-float (lpb) ((1 *)) pw-%offset%)
       (f2cl-lib:array-slice pw-%data% double-float (lpc) ((1 *)) pw-%offset%)
       ydot ipvt)
      (go end_label)
     end_label
      (return (values nil nil nil t$ nil nil mb nb nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::daigbt
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t t (array fortran-to-lisp::integer4 (*))
                        (double-float) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::t$ nil nil
                            fortran-to-lisp::mb fortran-to-lisp::nb nil nil
                            fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::dsolbt fortran-to-lisp::ddecbt))))

