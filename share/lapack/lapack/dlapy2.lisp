;;; Compiled by f2cl version:
;;; ("$Id: dlapy2.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dlapy2.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dlapy2.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dlapy2.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dlapy2.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dlapy2.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dlapy2.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :lapack)


(let* ((zero 0.0) (one 1.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one)
           (ignorable zero one))
  (defun dlapy2 (x y)
    (declare (type (double-float) y x))
    (prog ((w 0.0) (xabs 0.0) (yabs 0.0) (z 0.0) (dlapy2 0.0))
      (declare (type (double-float) w xabs yabs z dlapy2))
      (setf xabs (abs x))
      (setf yabs (abs y))
      (setf w (max xabs yabs))
      (setf z (min xabs yabs))
      (cond
        ((= z zero)
         (setf dlapy2 w))
        (t
         (setf dlapy2 (* w (f2cl-lib:fsqrt (+ one (expt (/ z w) 2)))))))
      (go end_label)
     end_label
      (return (values dlapy2 nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlapy2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float))
           :return-values '(nil nil)
           :calls 'nil)))

