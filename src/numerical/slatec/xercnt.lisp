;;; Compiled by f2cl version 2.0 beta Date: 2005/05/19 15:09:32 
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun xercnt (librar subrou messg nerr level kontrl)
  (declare (type f2cl-lib:integer4 kontrl level nerr)
           (type (simple-array character (*)) messg subrou librar))
  (prog ()
    (declare)
    (go end_label)
   end_label
    (return (values nil nil nil nil nil nil))))

