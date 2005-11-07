;;; Compiled by f2cl version 2.0 beta Date: 2005/07/26 12:37:15 
;;; Using Lisp CMU Common Lisp Snapshot 2005-11 (19C)
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

