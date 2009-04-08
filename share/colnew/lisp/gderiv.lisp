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
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :colnew)


(defun gderiv (gi nrow irow zval dgz mode dgsub)
  (declare (type (array double-float (*)) dgz zval)
           (type (integer) mode irow nrow)
           (type (array double-float (*)) gi))
  (let ()
    (symbol-macrolet ((mstar (aref (colord-part-0 *colord-common-block*) 2))
                      (izeta (aref (colsid-part-1 *colsid-common-block*) 0))
                      (nonlin (aref (colnln-part-0 *colnln-common-block*) 0))
                      (iter (aref (colnln-part-0 *colnln-common-block*) 1)))
      (f2cl-lib:with-multi-array-data
          ((gi double-float gi-%data% gi-%offset%)
           (zval double-float zval-%data% zval-%offset%)
           (dgz double-float dgz-%data% dgz-%offset%))
        (prog ((dot 0.0) (j 0)
               (dg (make-array 40 :element-type 'double-float)))
          (declare (type (array double-float (40)) dg)
                   (type (integer) j)
                   (type double-float dot))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody label10 (setf (f2cl-lib:fref dg (j) ((1 40))) 0.0)))
          (multiple-value-bind (var-0 var-1 var-2)
              (funcall dgsub izeta zval dg)
            (declare (ignore var-1 var-2))
            (when var-0
              (setf izeta var-0)))
          (if (or (= nonlin 0) (> iter 0)) (go label30))
          (setf dot 0.0)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
             label20
              (setf dot
                      (+ dot
                         (* (f2cl-lib:fref dg (j) ((1 40)))
                            (f2cl-lib:fref zval-%data%
                                           (j)
                                           ((1 1))
                                           zval-%offset%))))))
          (setf (f2cl-lib:fref dgz-%data% (izeta) ((1 1)) dgz-%offset%) dot)
         label30
          (if (= mode 2) (go label50))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
              (setf (f2cl-lib:fref gi-%data%
                                   (irow j)
                                   ((1 nrow) (1 1))
                                   gi-%offset%)
                      (f2cl-lib:fref dg (j) ((1 40))))
             label40
              (setf (f2cl-lib:fref gi-%data%
                                   (irow (f2cl-lib:int-add mstar j))
                                   ((1 nrow) (1 1))
                                   gi-%offset%)
                      0.0)))
          (go end_label)
         label50
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
              (setf (f2cl-lib:fref gi-%data%
                                   (irow j)
                                   ((1 nrow) (1 1))
                                   gi-%offset%)
                      0.0)
             label60
              (setf (f2cl-lib:fref gi-%data%
                                   (irow (f2cl-lib:int-add mstar j))
                                   ((1 nrow) (1 1))
                                   gi-%offset%)
                      (f2cl-lib:fref dg (j) ((1 40))))))
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::gderiv
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (integer) (integer)
                        (array double-float (1)) (array double-float (1))
                        (integer) t)
           :return-values '(nil nil nil nil nil nil nil)
           :calls 'nil)))

