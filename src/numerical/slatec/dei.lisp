;;; Compiled by f2cl version:
;;; ("$Id: dei.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: dei.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: dei.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: dei.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: dei.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: dei.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: dei.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun dei (x)
  (declare (type (double-float) x))
  (prog ((dei 0.0))
    (declare (type (double-float) dei))
    (setf dei (- (de1 (- x))))
    (go end_label)
   end_label
    (return (values dei nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dei fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types '((double-float))
                                            :return-values '(nil)
                                            :calls '(fortran-to-lisp::de1))))

