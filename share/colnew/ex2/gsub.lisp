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


(defun gsub (i z g)
  (declare (type double-float g)
           (type (array double-float (*)) z)
           (type (f2cl-lib:integer4) i))
  (f2cl-lib:with-multi-array-data
      ((z double-float z-%data% z-%offset%))
    (prog ()
      (declare)
      (f2cl-lib:computed-goto (label10 label20 label10 label30) i)
     label10
      (setf g (f2cl-lib:fref z-%data% (1) ((1 4)) z-%offset%))
      (go end_label)
     label20
      (setf g (f2cl-lib:fref z-%data% (3) ((1 4)) z-%offset%))
      (go end_label)
     label30
      (setf g
              (+
               (- (f2cl-lib:fref z-%data% (4) ((1 4)) z-%offset%)
                  (* 0.3f0 (f2cl-lib:fref z-%data% (3) ((1 4)) z-%offset%)))
               0.7f0))
      (go end_label)
     end_label
      (return (values nil nil g)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::gsub fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (4))
                        double-float)
           :return-values '(nil nil fortran-to-lisp::g)
           :calls 'nil)))

