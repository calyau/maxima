;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun zdiv (ar ai br bi cr ci)
  (declare (type double-float ar ai br bi cr ci))
  (prog ((bm 0.0) (ca 0.0) (cb 0.0) (cc 0.0) (cd 0.0))
    (declare (type double-float cd cc cb ca bm))
    (setf bm (/ 1.0 (zabs br bi)))
    (setf cc (* br bm))
    (setf cd (* bi bm))
    (setf ca (* (+ (* ar cc) (* ai cd)) bm))
    (setf cb (* (- (* ai cc) (* ar cd)) bm))
    (setf cr ca)
    (setf ci cb)
    (go end_label)
   end_label
    (return (values nil nil nil nil cr ci))))

