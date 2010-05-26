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


(defun dfsub (x z df)
  (declare (type (array double-float (*)) df)
           (type (array double-float (*)) z)
           (type double-float x))
  (let ()
    (symbol-macrolet ((eps4mu (%blank%-eps4mu *%blank%-common-block*))
                      (dmu (%blank%-dmu *%blank%-common-block*)))
      (f2cl-lib:with-multi-array-data
          ((z double-float z-%data% z-%offset%)
           (df double-float df-%data% df-%offset%))
        (prog ()
          (declare)
          (setf (f2cl-lib:fref df-%data% (1 1) ((1 2) (1 4)) df-%offset%)
                  (+ (/ (/ 1.0f0 x) x)
                     (/
                      (+ 1.0f0
                         (/ (f2cl-lib:fref z-%data% (3) ((1 4)) z-%offset%) x))
                      eps4mu)))
          (setf (f2cl-lib:fref df-%data% (1 2) ((1 2) (1 4)) df-%offset%)
                  (/ -1.0f0 x))
          (setf (f2cl-lib:fref df-%data% (1 3) ((1 2) (1 4)) df-%offset%)
                  (/
                   (-
                    (+ 1.0f0
                       (/
                        (* -1 (f2cl-lib:fref z-%data% (1) ((1 4)) z-%offset%))
                        x)))
                   eps4mu))
          (setf (f2cl-lib:fref df-%data% (1 4) ((1 2) (1 4)) df-%offset%)
                  (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref df-%data% (2 1) ((1 2) (1 4)) df-%offset%)
                  (/
                   (+ 1.0f0
                      (/ (- (f2cl-lib:fref z-%data% (1) ((1 4)) z-%offset%))
                         x))
                   dmu))
          (setf (f2cl-lib:fref df-%data% (2 2) ((1 2) (1 4)) df-%offset%)
                  (coerce 0.0f0 'double-float))
          (setf (f2cl-lib:fref df-%data% (2 3) ((1 2) (1 4)) df-%offset%)
                  (/ (/ 1.0f0 x) x))
          (setf (f2cl-lib:fref df-%data% (2 4) ((1 2) (1 4)) df-%offset%)
                  (/ -1.0f0 x))
          (go end_label)
         end_label
          (return (values nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dfsub fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(double-float (array double-float (4))
                        (array double-float (8)))
           :return-values '(nil nil nil)
           :calls 'nil)))

