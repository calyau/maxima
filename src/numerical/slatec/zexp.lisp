;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:19:23
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun zexp (ar ai br bi)
  (declare (type double-float bi br ai ar))
  (prog ((zm 0.0) (ca 0.0) (cb 0.0))
    (declare (type double-float cb ca zm))
    (setf zm (exp ar))
    (setf ca (* zm (cos ai)))
    (setf cb (* zm (sin ai)))
    (setf br ca)
    (setf bi cb)
    (go end_label)
   end_label
    (return (values nil nil br bi))))

