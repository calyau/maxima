;;; Compiled by f2cl version 2.0 beta Date: 2006/01/31 15:11:05 
;;; Using Lisp CMU Common Lisp Snapshot 2006-01 (19C)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((iparam
       (make-array 9
                   :element-type 'f2cl-lib:integer4
                   :initial-contents '(0 2 0 10 1 0 0 0 0))))
  (declare (type (simple-array f2cl-lib:integer4 (9)) iparam))
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

