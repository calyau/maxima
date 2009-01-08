;;; Compiled by f2cl version:
;;; ("$Id: fdump.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: fdump.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: fdump.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: fdump.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: fdump.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: fdump.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: fdump.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun fdump () (prog () (declare) (go end_label) end_label (return (values))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::fdump fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo :arg-types 'nil
                                            :return-values 'nil
                                            :calls 'nil)))

