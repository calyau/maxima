;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:19:23
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
    (setf bm
            (/ 1.0
               (multiple-value-bind
                   (ret-val var-0 var-1)
                   (zabs br bi)
                 (declare (ignore))
                 (when var-0 (setf br var-0))
                 (when var-1 (setf bi var-1))
                 ret-val)))
    (setf cc (* br bm))
    (setf cd (* bi bm))
    (setf ca (* (+ (* ar cc) (* ai cd)) bm))
    (setf cb (* (- (* ai cc) (* ar cd)) bm))
    (setf cr ca)
    (setf ci cb)
    (go end_label)
   end_label
    (return (values nil nil br bi cr ci))))

