;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:19:07
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(defun initds (os nos eta)
  (declare (type single-float eta)
           (type f2cl-lib:integer4 nos)
           (type (simple-array double-float (*)) os))
  (f2cl-lib:with-array-data (os-%data% os-%offset% os)
    (declare (type f2cl-lib:integer4 os-%offset%)
             (type (simple-array double-float (*)) os-%data%)
             (ignorable os-%offset% os-%data%))
    (prog ((initds 0) (t_ 0.0f0) (i 0) (ii 0) (err 0.0f0))
      (declare (type single-float err t_) (type f2cl-lib:integer4 ii i initds))
      (if (< nos 1)
          (xermsg "SLATEC" "INITDS" "Number of coefficients is less than 1" 2
           1))
      (setf err 0.0f0)
      (f2cl-lib:fdo (ii 1 (f2cl-lib:int-add ii 1))
                    ((> ii nos) nil)
        (tagbody
          (setf i (f2cl-lib:int-sub (f2cl-lib:int-add nos 1) ii))
          (setf err
                  (+ err
                     (abs
                      (f2cl-lib:freal
                       (f2cl-lib:fref os-%data% (i) ((1 t)) os-%offset%)))))
          (if (> err eta) (go label20))
         label10))
     label20
      (if (= i nos)
          (xermsg "SLATEC" "INITDS"
           "Chebyshev series too short for specified accuracy" 1 1))
      (setf initds i)
      (go end_label)
     end_label
      (return (values initds nil nil nil)))))

