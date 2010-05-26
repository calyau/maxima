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
;;;           (:array-slicing t) (:declare-common t)
;;;           (:float-format double-float))

(in-package :colnew)


(defstruct (%blank%
             (:predicate is-%blank%-p))
  (en 0.0 :type double-float)
  (s 0.0 :type double-float)
  (el 0.0 :type double-float)
  (cons$ 0.0 :type double-float))


(defparameter *%blank%-common-block*
  (let* ()
    (declare (ignorable))
    (make-%blank%)))


(defun solutn (x z dmval)
  (declare (type (array double-float (*)) dmval)
           (type (array double-float (*)) z)
           (type double-float x))
  (let ()
    (symbol-macrolet ((el (%blank%-el *%blank%-common-block*)))
      (f2cl-lib:with-multi-array-data
          ((z double-float z-%data% z-%offset%)
           (dmval double-float dmval-%data% dmval-%offset%))
        (prog ((ex 0.0))
          (declare (type double-float ex))
          (setf ex (exp (* (- el) x)))
          (setf (f2cl-lib:fref z-%data% (1) ((1 5)) z-%offset%) (- 1.0f0 ex))
          (setf (f2cl-lib:fref z-%data% (2) ((1 5)) z-%offset%) (* el ex))
          (setf (f2cl-lib:fref z-%data% (3) ((1 5)) z-%offset%)
                  (* (- (expt el 2)) (expt x 2) ex))
          (setf (f2cl-lib:fref z-%data% (4) ((1 5)) z-%offset%)
                  (* (+ (* (expt el 3) (expt x 2)) (* -2.0f0 (expt el 2) x))
                     ex))
          (setf (f2cl-lib:fref z-%data% (5) ((1 5)) z-%offset%)
                  (*
                   (-
                    (+ (* (- (expt el 4)) (expt x 2)) (* 4.0f0 (expt el 3) x))
                    (* 2.0f0 (expt el 2)))
                   ex))
          (setf (f2cl-lib:fref dmval-%data% (1) ((1 2)) dmval-%offset%)
                  (* (- el) (f2cl-lib:fref z-%data% (2) ((1 5)) z-%offset%)))
          (setf (f2cl-lib:fref dmval-%data% (2) ((1 2)) dmval-%offset%)
                  (*
                   (+ (* (expt el 5) x x)
                      (* -6.0f0 (expt el 4) x)
                      (* 6.0f0 (expt el 3)))
                   ex))
          (go end_label)
         end_label
          (return (values nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::solutn
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(double-float (array double-float (5))
                        (array double-float (2)))
           :return-values '(nil nil nil)
           :calls 'nil)))

