;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:19:07
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun xercnt (librar subrou messg nerr level kontrl)
  (declare (type f2cl-lib:integer4 kontrl level nerr)
           (type (simple-array base-char (*)) messg subrou librar))
  (f2cl-lib:with-array-data (librar-%data% librar-%offset% librar)
    (declare (type f2cl-lib:integer4 librar-%offset%)
             (type (simple-array base-char (*)) librar-%data%)
             (ignorable librar-%offset% librar-%data%))
    (f2cl-lib:with-array-data (subrou-%data% subrou-%offset% subrou)
      (declare (type f2cl-lib:integer4 subrou-%offset%)
               (type (simple-array base-char (*)) subrou-%data%)
               (ignorable subrou-%offset% subrou-%data%))
      (f2cl-lib:with-array-data (messg-%data% messg-%offset% messg)
        (declare (type f2cl-lib:integer4 messg-%offset%)
                 (type (simple-array base-char (*)) messg-%data%)
                 (ignorable messg-%offset% messg-%data%))
        (prog ()
          (declare)
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil)))))))

