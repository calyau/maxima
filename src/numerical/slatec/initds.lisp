;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 1.215 2009/04/07 22:05:21 rtoy Exp $"
;;;  "f2cl2.l,v 1.37 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl3.l,v 1.6 2008/02/22 22:19:33 rtoy Exp $"
;;;  "f2cl4.l,v 1.7 2008/02/22 22:19:34 rtoy Exp $"
;;;  "f2cl5.l,v 1.200 2009/01/19 02:38:17 rtoy Exp $"
;;;  "f2cl6.l,v 1.48 2008/08/24 00:56:27 rtoy Exp $"
;;;  "macros.l,v 1.112 2009/01/08 12:57:19 rtoy Exp $")

;;; Using Lisp CMU Common Lisp 19f (19F)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun initds (os nos eta)
  (declare (type (single-float) eta)
           (type (integer) nos)
           (type (simple-array double-float (*)) os))
  (prog ((initds 0) (i 0) (ii 0) (err 0.0f0))
    (declare (type (single-float) err) (type (integer) ii i initds))
    (if (< nos 1)
        (xermsg "SLATEC" "INITDS" "Number of coefficients is less than 1" 2 1))
    (setf err 0.0f0)
    (f2cl-lib:fdo (ii 1 (f2cl-lib:int-add ii 1))
                  ((> ii nos) nil)
      (tagbody
        (setf i (f2cl-lib:int-sub (f2cl-lib:int-add nos 1) ii))
        (setf err
                (+ err (abs (f2cl-lib:freal (f2cl-lib:fref os (i) ((1 *)))))))
        (if (> err eta) (go label20))
       label10))
   label20
    (if (= i nos)
        (xermsg "SLATEC" "INITDS"
         "Chebyshev series too short for specified accuracy" 1 1))
    (setf initds i)
    (go end_label)
   end_label
    (return (values initds nil nil nil))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::initds
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-array double-float (*)) (integer)
                        (single-float))
           :return-values '(nil nil nil)
           :calls '(fortran-to-lisp::xermsg))))

