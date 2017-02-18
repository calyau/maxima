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


(defun nroc (n ic ia ja a jar ar p flag)
  (declare (type (array double-float (*)) ar a)
           (type (array f2cl-lib:integer4 (*)) p jar ja ia ic)
           (type (f2cl-lib:integer4) flag n))
  (f2cl-lib:with-multi-array-data
      ((ic f2cl-lib:integer4 ic-%data% ic-%offset%)
       (ia f2cl-lib:integer4 ia-%data% ia-%offset%)
       (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
       (jar f2cl-lib:integer4 jar-%data% jar-%offset%)
       (p f2cl-lib:integer4 p-%data% p-%offset%)
       (a double-float a-%data% a-%offset%)
       (ar double-float ar-%data% ar-%offset%))
    (prog ((i 0) (newj 0) (j 0) (jmax 0) (jmin 0) (k 0))
      (declare (type (f2cl-lib:integer4) k jmin jmax j newj i))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k n) nil)
        (tagbody
          (setf jmin (f2cl-lib:fref ia-%data% (k) ((1 *)) ia-%offset%))
          (setf jmax
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref ia-%data%
                                  ((f2cl-lib:int-add k 1))
                                  ((1 *))
                                  ia-%offset%)
                   1))
          (if (> jmin jmax) (go label5))
          (setf (f2cl-lib:fref p-%data%
                               ((f2cl-lib:int-add n 1))
                               ((1 *))
                               p-%offset%)
                  (f2cl-lib:int-add n 1))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
              (setf newj
                      (f2cl-lib:fref ic-%data%
                                     ((f2cl-lib:fref ja (j) ((1 *))))
                                     ((1 *))
                                     ic-%offset%))
              (setf i (f2cl-lib:int-add n 1))
             label1
              (if (>= (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%) newj)
                  (go label2))
              (setf i (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%))
              (go label1)
             label2
              (if (= (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%) newj)
                  (go label102))
              (setf (f2cl-lib:fref p-%data% (newj) ((1 *)) p-%offset%)
                      (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%))
              (setf (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%) newj)
              (setf (f2cl-lib:fref jar-%data% (newj) ((1 *)) jar-%offset%)
                      (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%))
              (setf (f2cl-lib:fref ar-%data% (newj) ((1 *)) ar-%offset%)
                      (f2cl-lib:fref a-%data% (j) ((1 *)) a-%offset%))
             label3))
          (setf i (f2cl-lib:int-add n 1))
          (f2cl-lib:fdo (j jmin (f2cl-lib:int-add j 1))
                        ((> j jmax) nil)
            (tagbody
              (setf i (f2cl-lib:fref p-%data% (i) ((1 *)) p-%offset%))
              (setf (f2cl-lib:fref ja-%data% (j) ((1 *)) ja-%offset%)
                      (f2cl-lib:fref jar-%data% (i) ((1 *)) jar-%offset%))
             label4
              (setf (f2cl-lib:fref a-%data% (j) ((1 *)) a-%offset%)
                      (f2cl-lib:fref ar-%data% (i) ((1 *)) ar-%offset%))))
         label5))
      (setf flag 0)
      (go end_label)
     label102
      (setf flag (f2cl-lib:int-add n k))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil flag)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::nroc fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil
                            fortran-to-lisp::flag)
           :calls 'nil)))

