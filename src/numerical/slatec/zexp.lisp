;;; Compiled by f2cl version 2.0 beta Date: 2007/05/04 17:29:50 
;;; Using Lisp CMU Common Lisp Snapshot 2007-05 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun zexp (ar ai br bi)
  (declare (type (double-float) bi br ai ar))
  (prog ((zm 0.0) (ca 0.0) (cb 0.0))
    (declare (type (double-float) cb ca zm))
    (setf zm (exp ar))
    (setf ca (* zm (cos ai)))
    (setf cb (* zm (sin ai)))
    (setf br ca)
    (setf bi cb)
    (go end_label)
   end_label
    (return (values nil nil br bi))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zexp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float))
           :return-values '(nil nil fortran-to-lisp::br fortran-to-lisp::bi)
           :calls 'nil)))

