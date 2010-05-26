;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.221 2010/05/26 19:25:52 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.204 2010/02/23 05:21:30 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.114 2010/05/17 01:42:14 rtoy Exp $")

;;; Using Lisp CMU Common Lisp CVS Head 2010-05-25 18:21:07 (20A Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :colnew)


(defun solutn (x z dmval)
  (declare (type (array double-float (*)) dmval)
           (type (array double-float (*)) z)
           (type double-float x))
  (let ()
    (symbol-macrolet ((xt (%blank%-xt *%blank%-common-block*))
                      (gamma (%blank%-gamma *%blank%-common-block*)))
      (f2cl-lib:with-multi-array-data
          ((z double-float z-%data% z-%offset%)
           (dmval double-float dmval-%data% dmval-%offset%))
        (prog ((d2cons 0.0) (dcons 0.0) (cons$ 0.0))
          (declare (type double-float cons$ dcons d2cons))
          (setf cons$ (* gamma x (+ 1.0f0 (* -0.5f0 x x))))
          (setf dcons (* gamma (+ 1.0f0 (* -1.5f0 x x))))
          (setf d2cons (* -3.0f0 gamma x))
          (if (> x xt) (go label10))
          (setf (f2cl-lib:fref z-%data% (1) ((1 4)) z-%offset%) (* 2.0f0 x))
          (setf (f2cl-lib:fref z-%data% (2) ((1 4)) z-%offset%)
                  (coerce 2.0f0 'double-float))
          (setf (f2cl-lib:fref z-%data% (3) ((1 4)) z-%offset%)
                  (+ (* -2.0f0 x) cons$))
          (setf (f2cl-lib:fref z-%data% (4) ((1 4)) z-%offset%)
                  (- dcons 2.0f0))
          (setf (f2cl-lib:fref dmval-%data% (2) ((1 2)) dmval-%offset%) d2cons)
          (go label20)
         label10
          (setf (f2cl-lib:fref z-%data% (1) ((1 4)) z-%offset%)
                  (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref z-%data% (2) ((1 4)) z-%offset%)
                  (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref z-%data% (3) ((1 4)) z-%offset%) (- cons$))
          (setf (f2cl-lib:fref z-%data% (4) ((1 4)) z-%offset%) (- dcons))
          (setf (f2cl-lib:fref dmval-%data% (2) ((1 2)) dmval-%offset%)
                  (- d2cons))
         label20
          (setf (f2cl-lib:fref dmval-%data% (1) ((1 2)) dmval-%offset%)
                  (coerce 0.0f0 'double-float))
          (go end_label)
         end_label
          (return (values nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::solutn
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(double-float (array double-float (4))
                        (array double-float (2)))
           :return-values '(nil nil nil)
           :calls 'nil)))

