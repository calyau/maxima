;;; Compiled by f2cl version 2.0 beta Date: 2006/12/21 03:42:11 
;;; Using Lisp CMU Common Lisp CVS Head 2006-12-02 00:15:46 (19D)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "LAPACK")


(let* ((one 1.0) (zero 0.0))
  (declare (type (double-float 1.0 1.0) one)
           (type (double-float 0.0 0.0) zero))
  (defun dorgl2 (m n k a lda tau work info)
    (declare (type (array double-float (*)) work tau a)
             (type (f2cl-lib:integer4) info lda k n m))
    (f2cl-lib:with-multi-array-data
        ((a double-float a-%data% a-%offset%)
         (tau double-float tau-%data% tau-%offset%)
         (work double-float work-%data% work-%offset%))
      (prog ((i 0) (j 0) (l 0))
        (declare (type (f2cl-lib:integer4) i j l))
        (setf info 0)
        (cond
          ((< m 0)
           (setf info -1))
          ((< n m)
           (setf info -2))
          ((or (< k 0) (> k m))
           (setf info -3))
          ((< lda (max (the f2cl-lib:integer4 1) (the f2cl-lib:integer4 m)))
           (setf info -5)))
        (cond
          ((/= info 0)
           (xerbla "DORGL2" (f2cl-lib:int-sub info))
           (go end_label)))
        (if (<= m 0) (go end_label))
        (cond
          ((< k m)
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j n) nil)
             (tagbody
               (f2cl-lib:fdo (l (f2cl-lib:int-add k 1) (f2cl-lib:int-add l 1))
                             ((> l m) nil)
                 (tagbody
                   (setf (f2cl-lib:fref a-%data%
                                        (l j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           zero)
                  label10))
               (if (and (> j k) (<= j m))
                   (setf (f2cl-lib:fref a-%data%
                                        (j j)
                                        ((1 lda) (1 *))
                                        a-%offset%)
                           one))
              label20))))
        (f2cl-lib:fdo (i k (f2cl-lib:int-add i (f2cl-lib:int-sub 1)))
                      ((> i 1) nil)
          (tagbody
            (cond
              ((< i n)
               (cond
                 ((< i m)
                  (setf (f2cl-lib:fref a-%data%
                                       (i i)
                                       ((1 lda) (1 *))
                                       a-%offset%)
                          one)
                  (dlarf "Right" (f2cl-lib:int-sub m i)
                   (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
                   (f2cl-lib:array-slice a double-float (i i) ((1 lda) (1 *)))
                   lda (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%)
                   (f2cl-lib:array-slice a
                                         double-float
                                         ((+ i 1) i)
                                         ((1 lda) (1 *)))
                   lda work)))
               (dscal (f2cl-lib:int-sub n i)
                (- (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%))
                (f2cl-lib:array-slice a
                                      double-float
                                      (i (f2cl-lib:int-add i 1))
                                      ((1 lda) (1 *)))
                lda)))
            (setf (f2cl-lib:fref a-%data% (i i) ((1 lda) (1 *)) a-%offset%)
                    (- one
                       (f2cl-lib:fref tau-%data% (i) ((1 *)) tau-%offset%)))
            (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                          ((> l (f2cl-lib:int-add i (f2cl-lib:int-sub 1))) nil)
              (tagbody
                (setf (f2cl-lib:fref a-%data% (i l) ((1 lda) (1 *)) a-%offset%)
                        zero)
               label30))
           label40))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil info))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dorgl2
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil fortran-to-lisp::info)
           :calls '(fortran-to-lisp::dscal fortran-to-lisp::dlarf
                    fortran-to-lisp::xerbla))))

