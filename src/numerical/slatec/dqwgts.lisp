;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun dqwgts (x a b alfa beta integr)
  (declare (type f2cl-lib:integer4 integr) (type double-float beta alfa b a x))
  (prog ((bmx 0.0) (xma 0.0) (dqwgts 0.0))
    (declare (type double-float dqwgts xma bmx))
    (setf xma (- x a))
    (setf bmx (- b x))
    (setf dqwgts (* (expt xma alfa) (expt bmx beta)))
    (f2cl-lib:computed-goto (label40 label10 label20 label30) integr)
   label10
    (setf dqwgts (* dqwgts (f2cl-lib:flog xma)))
    (go label40)
   label20
    (setf dqwgts (* dqwgts (f2cl-lib:flog bmx)))
    (go label40)
   label30
    (setf dqwgts (* dqwgts (f2cl-lib:flog xma) (f2cl-lib:flog bmx)))
   label40
    (go end_label)
   end_label
    (return (values dqwgts nil nil nil nil nil nil))))

