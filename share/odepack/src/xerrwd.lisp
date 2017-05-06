;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2017-01 (21B Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "ODEPACK")


(defun xerrwd (msg nmes nerr level ni i1 i2 nr r1 r2)
  (declare (type (double-float) r2 r1)
           (type (f2cl-lib:integer4) nr i2 i1 ni level nerr nmes)
           (type (string *) msg))
  (f2cl-lib:with-multi-array-data
      ((msg character msg-%data% msg-%offset%))
    (prog ((lunit 0) (mesflg 0))
      (declare (type (f2cl-lib:integer4) mesflg lunit))
      (setf lunit (ixsav 1 0 f2cl-lib:%false%))
      (setf mesflg (ixsav 2 0 f2cl-lib:%false%))
      (if (= mesflg 0) (go label100))
      (f2cl-lib:fformat lunit ("~1@T" ("~A") "~%") msg)
      (if (= ni 1)
          (f2cl-lib:fformat lunit
                            ("~6@T" "In above message,  I1 =" 1 (("~10D"))
                             "~%")
                            i1))
      (if (= ni 2)
          (f2cl-lib:fformat lunit
                            ("~6@T" "In above message,  I1 =" 1 (("~10D"))
                             "~3@T" "I2 =" 1 (("~10D")) "~%")
                            i1
                            i2))
      (if (= nr 1)
          (f2cl-lib:fformat lunit
                            ("~6@T" "In above message,  R1 =" 1
                             (("~21,13,2,0,'*,,'DE")) "~%")
                            r1))
      (if (= nr 2)
          (f2cl-lib:fformat lunit
                            ("~6@T" "In above,  R1 =" 1
                             (("~21,13,2,0,'*,,'DE")) "~3@T" "R2 =" 1
                             (("~21,13,2,0,'*,,'DE")) "~%")
                            r1
                            r2))
     label100
      (if (/= level 2) (go end_label))
      (f2cl-lib::stop)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::xerrwd
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((string) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (double-float))
           :return-values '(nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::ixsav))))

