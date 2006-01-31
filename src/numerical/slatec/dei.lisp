;;; Compiled by f2cl version 2.0 beta Date: 2006/01/31 15:11:05 
;;; Using Lisp CMU Common Lisp Snapshot 2006-01 (19C)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun dei (x)
  (declare (type double-float x))
  (prog ((dei 0.0))
    (declare (type double-float dei))
    (setf dei (- (de1 (- x))))
    (go end_label)
   end_label
    (return (values dei nil))))

