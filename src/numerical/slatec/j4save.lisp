;;; Compiled by f2cl version 2.0 beta Date: 2006/11/28 21:41:12 
;;; Using Lisp CMU Common Lisp Snapshot 2006-12 (19D)
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
    (declare (type f2cl-lib:logical iset) (type (integer) ivalue iwhich))
    (prog ((j4save 0))
      (declare (type (integer) j4save))
      (setf j4save (f2cl-lib:fref iparam (iwhich) ((1 9))))
      (if iset (setf (f2cl-lib:fref iparam (iwhich) ((1 9))) ivalue))
      (go end_label)
     end_label
      (return (values j4save nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(:and) '(:or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::j4save
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((integer) (integer) fortran-to-lisp::logical)
           :return-values '(nil nil nil)
           :calls 'nil)))

