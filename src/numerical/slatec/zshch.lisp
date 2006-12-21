;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:18:39 
;;; Using Lisp CMU Common Lisp Snapshot 2006-12 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun zshch (zr zi cshr cshi cchr cchi)
  (declare (type (double-float) cchi cchr cshi cshr zi zr))
  (prog ((ch 0.0) (cn 0.0) (sh 0.0) (sn 0.0))
    (declare (type (double-float) sn sh cn ch))
    (setf sh (sinh zr))
    (setf ch (cosh zr))
    (setf sn (sin zi))
    (setf cn (cos zi))
    (setf cshr (* sh cn))
    (setf cshi (* ch sn))
    (setf cchr (* ch cn))
    (setf cchi (* sh sn))
    (go end_label)
   end_label
    (return (values nil nil cshr cshi cchr cchi))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zshch fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil fortran-to-lisp::cshr fortran-to-lisp::cshi
                            fortran-to-lisp::cchr fortran-to-lisp::cchi)
           :calls 'nil)))

