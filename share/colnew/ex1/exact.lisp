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


(defun exact (x u)
  (declare (type (array double-float (*)) u) (type (double-float) x))
  (f2cl-lib:with-multi-array-data
      ((u double-float u-%data% u-%offset%))
    (prog ()
      (declare)
      (setf (f2cl-lib:fref u-%data% (1) ((1 4)) u-%offset%)
              (+
               (* 0.25f0
                  (- (* 10.0f0 (f2cl-lib:flog 2.0f0)) 3.0f0)
                  (- 1.0f0 x))
               (* 0.5f0
                  (- (+ (/ 1.0f0 x) (* (+ 3.0f0 x) (f2cl-lib:flog x))) x))))
      (setf (f2cl-lib:fref u-%data% (2) ((1 4)) u-%offset%)
              (+ (* -0.25f0 (- (* 10.0f0 (f2cl-lib:flog 2.0f0)) 3.0f0))
                 (* 0.5f0
                    (-
                     (+ (/ (/ -1.0f0 x) x) (f2cl-lib:flog x) (/ (+ 3.0f0 x) x))
                     1.0f0))))
      (setf (f2cl-lib:fref u-%data% (3) ((1 4)) u-%offset%)
              (* 0.5f0
                 (+ (/ 2.0f0 (expt x 3)) (/ 1.0f0 x) (/ (/ -3.0f0 x) x))))
      (setf (f2cl-lib:fref u-%data% (4) ((1 4)) u-%offset%)
              (* 0.5f0
                 (+ (/ -6.0f0 (expt x 4))
                    (/ (/ -1.0f0 x) x)
                    (/ 6.0f0 (expt x 3)))))
      (go end_label)
     end_label
      (return (values nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::exact fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (array double-float (4)))
           :return-values '(nil nil)
           :calls 'nil)))

