;;; Compiled by f2cl version 2.0 beta Date: 2005/07/26 12:37:15 
;;; Using Lisp CMU Common Lisp Snapshot 2005-11 (19C)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((drt 0.7071067811865476) (dpi 3.141592653589793))
  (declare (type double-float dpi drt))
  (defun zsqrt (ar ai br bi)
    (declare (type double-float bi br ai ar))
    (prog ((zm 0.0) (dtheta 0.0) (abs$ 0.0f0))
      (declare (type single-float abs$) (type double-float dtheta zm))
      (setf zm (zabs ar ai))
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
      (setf bi (coerce (f2cl-lib:fsqrt (abs ar)) 'double-float))
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

