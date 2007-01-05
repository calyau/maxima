;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:42:11 
;;; Using Lisp CMU Common Lisp CVS Head 2006-12-02 00:15:46 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "LAPACK")


(let* ((zero 0.0) (one 1.0))
  (declare (type (double-float 0.0 0.0) zero)
           (type (double-float 1.0 1.0) one))
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

