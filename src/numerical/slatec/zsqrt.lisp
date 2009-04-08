;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.215 2009/04/07 22:05:21 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.200 2009/01/19 02:38:17 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp 19f (19F)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((drt 0.7071067811865476) (dpi 3.141592653589793))
  (declare (type (double-float) drt dpi))
  (defun zsqrt$ (ar ai br bi)
    (declare (type (double-float) bi br ai ar))
    (prog ((zm 0.0) (dtheta 0.0))
      (declare (type (double-float) dtheta zm))
      (setf zm (coerce (realpart (zabs ar ai)) 'double-float))
      (setf zm (f2cl-lib:fsqrt zm))
      (if (= ar 0.0) (go label10))
      (if (= ai 0.0) (go label20))
      (setf dtheta (f2cl-lib:datan (/ ai ar)))
      (if (<= dtheta 0.0) (go label40))
      (if (< ar 0.0) (setf dtheta (- dtheta dpi)))
      (go label50)
     label10
      (if (> ai 0.0) (go label60))
      (if (< ai 0.0) (go label70))
      (setf br 0.0)
      (setf bi 0.0)
      (go end_label)
     label20
      (if (> ar 0.0) (go label30))
      (setf br 0.0)
      (setf bi (f2cl-lib:fsqrt (abs ar)))
      (go end_label)
     label30
      (setf br (f2cl-lib:fsqrt ar))
      (setf bi 0.0)
      (go end_label)
     label40
      (if (< ar 0.0) (setf dtheta (+ dtheta dpi)))
     label50
      (setf dtheta (* dtheta 0.5))
      (setf br (* zm (cos dtheta)))
      (setf bi (* zm (sin dtheta)))
      (go end_label)
     label60
      (setf br (* zm drt))
      (setf bi (* zm drt))
      (go end_label)
     label70
      (setf br (* zm drt))
      (setf bi (* (- zm) drt))
      (go end_label)
     end_label
      (return (values nil nil br bi)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zsqrt$
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float))
           :return-values '(nil nil fortran-to-lisp::br fortran-to-lisp::bi)
           :calls '(fortran-to-lisp::zabs))))

