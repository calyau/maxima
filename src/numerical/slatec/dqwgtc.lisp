;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun dqwgtc (x c p2 p3 p4 kp)
  (declare (type f2cl-lib:integer4 kp) (type double-float p4 p3 p2 c x))
  (prog ((dqwgtc 0.0))
    (declare (type double-float dqwgtc))
    (setf dqwgtc (/ 1.0 (- x c)))
    (go end_label)
   end_label
    (return (values dqwgtc nil nil nil nil nil nil))))

