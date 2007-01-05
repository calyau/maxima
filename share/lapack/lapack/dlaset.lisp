;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:42:11 
;;; Using Lisp CMU Common Lisp CVS Head 2006-12-02 00:15:46 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "LAPACK")


(defun dlaset (uplo m n alpha beta a lda)
  (declare (type (array double-float (*)) a)
           (type (double-float) beta alpha)
           (type (f2cl-lib:integer4) lda n m)
           (type (simple-array character (*)) uplo))
  (f2cl-lib:with-multi-array-data
      ((uplo character uplo-%data% uplo-%offset%)
       (a double-float a-%data% a-%offset%))
    (prog ((i 0) (j 0))
      (declare (type (f2cl-lib:integer4) j i))
      (cond
        ((lsame uplo "U")
         (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i
                               (min
                                (the f2cl-lib:integer4
                                     (f2cl-lib:int-add j (f2cl-lib:int-sub 1)))
                                (the f2cl-lib:integer4 m)))
                            nil)
               (tagbody
                 (setf (f2cl-lib:fref a-%data%
                                      (i j)
                                      ((1 lda) (1 *))
                                      a-%offset%)
                         alpha)
                label10))
            label20)))
        ((lsame uplo "L")
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j
                           (min (the f2cl-lib:integer4 m)
                                (the f2cl-lib:integer4 n)))
                        nil)
           (tagbody
             (f2cl-lib:fdo (i (f2cl-lib:int-add j 1) (f2cl-lib:int-add i 1))
                           ((> i m) nil)
               (tagbody
                 (setf (f2cl-lib:fref a-%data%
                                      (i j)
                                      ((1 lda) (1 *))
                                      a-%offset%)
                         alpha)
                label30))
            label40)))
        (t
         (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                       ((> j n) nil)
           (tagbody
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i m) nil)
               (tagbody
                 (setf (f2cl-lib:fref a-%data%
                                      (i j)
                                      ((1 lda) (1 *))
                                      a-%offset%)
                         alpha)
                label50))
            label60))))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i
                        (min (the f2cl-lib:integer4 m)
                             (the f2cl-lib:integer4 n)))
                     nil)
        (tagbody
          (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%) beta)
         label70))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dlaset
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((simple-array character (1))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (double-float) (array double-float (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::lsame))))

