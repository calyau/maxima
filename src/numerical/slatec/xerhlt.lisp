;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:19:07
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun xerhlt (messg)
  (declare (type (simple-array base-char (*)) messg))
  (f2cl-lib:with-array-data (messg-%data% messg-%offset% messg)
    (declare (type f2cl-lib:integer4 messg-%offset%)
             (type (simple-array base-char (*)) messg-%data%)
             (ignorable messg-%offset% messg-%data%))
    (prog () (declare) end_label (return (values nil)))))

