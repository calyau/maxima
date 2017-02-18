;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2013-11 (20E Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(defun dmnorm (n v w)
  (declare (type (array double-float (*)) w v) (type (f2cl-lib:integer4) n))
  (f2cl-lib:with-multi-array-data
      ((v double-float v-%data% v-%offset%)
       (w double-float w-%data% w-%offset%))
    (prog ((vm 0.0d0) (i 0) (dmnorm 0.0d0))
      (declare (type (f2cl-lib:integer4) i) (type (double-float) dmnorm vm))
      (setf vm 0.0d0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
         label10
          (setf vm
                  (max vm
                       (* (abs (f2cl-lib:fref v-%data% (i) ((1 n)) v-%offset%))
                          (f2cl-lib:fref w-%data% (i) ((1 n)) w-%offset%))))))
      (setf dmnorm vm)
      (go end_label)
     end_label
      (return (values dmnorm nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dmnorm
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil)
           :calls 'nil)))

