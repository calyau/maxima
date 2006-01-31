;;; Compiled by f2cl version 2.0 beta Date: 2006/01/31 15:11:05 
;;; Using Lisp CMU Common Lisp Snapshot 2006-01 (19C)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun zuchk (yr yi nz ascle tol)
  (declare (type f2cl-lib:integer4 nz) (type double-float tol ascle yi yr))
  (prog ((ss 0.0) (st 0.0) (wr 0.0) (wi 0.0) (abs$ 0.0f0))
    (declare (type single-float abs$) (type double-float wi wr st ss))
    (setf nz 0)
    (setf wr (coerce (abs yr) 'double-float))
    (setf wi (coerce (abs yi) 'double-float))
    (setf st (min wr wi))
    (if (> st ascle) (go end_label))
    (setf ss (max wr wi))
    (setf st (/ st tol))
    (if (< ss st) (setf nz 1))
    (go end_label)
   end_label
    (return (values nil nil nz nil nil))))

