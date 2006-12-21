;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:18:39 
;;; Using Lisp CMU Common Lisp Snapshot 2006-12 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun zmlt (ar ai br bi cr ci)
  (declare (type (double-float) ci cr bi br ai ar))
  (prog ((ca 0.0) (cb 0.0))
    (declare (type (double-float) cb ca))
    (setf ca (- (* ar br) (* ai bi)))
    (setf cb (+ (* ar bi) (* ai br)))
    (setf cr ca)
    (setf ci cb)
    (go end_label)
   end_label
    (return (values nil nil nil nil cr ci))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zmlt fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil nil fortran-to-lisp::cr
                            fortran-to-lisp::ci)
           :calls 'nil)))

