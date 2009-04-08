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


(defun dmzsol (kd mstar n v z dmz)
  (declare (type (array double-float (*)) z)
           (type (array double-float (*)) dmz v)
           (type (integer) n mstar kd))
  (f2cl-lib:with-multi-array-data
      ((v double-float v-%data% v-%offset%)
       (dmz double-float dmz-%data% dmz-%offset%)
       (z double-float z-%data% z-%offset%))
    (prog ((l 0) (fact 0.0) (j 0) (i 0) (jz 0))
      (declare (type double-float fact) (type (integer) jz i j l))
      (setf jz 1)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mstar) nil)
            (tagbody
              (setf fact (f2cl-lib:fref z-%data% (jz) ((1 1)) z-%offset%))
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l kd) nil)
                (tagbody
                  (setf (f2cl-lib:fref dmz-%data%
                                       (l i)
                                       ((1 kd) (1 1))
                                       dmz-%offset%)
                          (+
                           (f2cl-lib:fref dmz-%data%
                                          (l i)
                                          ((1 kd) (1 1))
                                          dmz-%offset%)
                           (* fact
                              (f2cl-lib:fref v-%data%
                                             (l jz)
                                             ((1 kd) (1 1))
                                             v-%offset%))))
                 label10))
              (setf jz (f2cl-lib:int-add jz 1))
             label20))
         label30))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dmzsol
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((integer) (integer) (integer) (array double-float (*))
                        (array double-float (1)) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil)
           :calls 'nil)))

