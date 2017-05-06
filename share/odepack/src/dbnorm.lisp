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


(defun dbnorm (n a nra ml mu w)
  (declare (type (array double-float (*)) w a)
           (type (f2cl-lib:integer4) mu ml nra n))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (w double-float w-%data% w-%offset%))
    (prog ((an 0.0d0) (sum 0.0d0) (i 0) (i1 0) (jlo 0) (jhi 0) (j 0)
           (dbnorm 0.0d0))
      (declare (type (f2cl-lib:integer4) j jhi jlo i1 i)
               (type (double-float) dbnorm sum an))
      (setf an 0.0d0)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf sum 0.0d0)
          (setf i1 (f2cl-lib:int-add i mu 1))
          (setf jlo
                  (max (the f2cl-lib:integer4 (f2cl-lib:int-sub i ml))
                       (the f2cl-lib:integer4 1)))
          (setf jhi
                  (min (the f2cl-lib:integer4 (f2cl-lib:int-add i mu))
                       (the f2cl-lib:integer4 n)))
          (f2cl-lib:fdo (j jlo (f2cl-lib:int-add j 1))
                        ((> j jhi) nil)
            (tagbody
             label10
              (setf sum
                      (+ sum
                         (/
                          (abs
                           (f2cl-lib:fref a-%data%
                                          ((f2cl-lib:int-sub i1 j) j)
                                          ((1 nra) (1 n))
                                          a-%offset%))
                          (f2cl-lib:fref w-%data% (j) ((1 n)) w-%offset%))))))
          (setf an
                  (max an
                       (* sum
                          (f2cl-lib:fref w-%data% (i) ((1 n)) w-%offset%))))
         label20))
      (setf dbnorm an)
      (go end_label)
     end_label
      (return (values dbnorm nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dbnorm
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil)
           :calls 'nil)))

