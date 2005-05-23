;;; Compiled by f2cl version 2.0 beta Date: 2005/05/19 15:09:32 
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((iparam (make-array 9 :element-type 'f2cl-lib:integer4)))
  (declare (type (simple-array f2cl-lib:integer4 (9)) iparam))
  (f2cl-lib:fset (f2cl-lib:fref iparam (1) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (2) ((1 9))) 2)
  (f2cl-lib:fset (f2cl-lib:fref iparam (3) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (4) ((1 9))) 10)
  (f2cl-lib:fset (f2cl-lib:fref iparam (5) ((1 9))) 1)
  (f2cl-lib:fset (f2cl-lib:fref iparam (6) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (7) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (8) ((1 9))) 0)
  (f2cl-lib:fset (f2cl-lib:fref iparam (9) ((1 9))) 0)
  (defun j4save (iwhich ivalue iset)
    (declare (type f2cl-lib:logical iset)
             (type f2cl-lib:integer4 ivalue iwhich))
    (prog ((j4save 0))
      (declare (type f2cl-lib:integer4 j4save))
      (setf j4save (f2cl-lib:fref iparam (iwhich) ((1 9))))
      (if iset (f2cl-lib:fset (f2cl-lib:fref iparam (iwhich) ((1 9))) ivalue))
      (go end_label)
     end_label
      (return (values j4save nil nil nil)))))

