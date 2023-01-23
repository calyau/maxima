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


(defun dorthog (vnew v hes n ll ldhes kmp snormw)
  (declare (type (double-float) snormw)
           (type (f2cl-lib:integer4) kmp ldhes ll n)
           (type (array double-float (*)) hes v vnew))
  (f2cl-lib:with-multi-array-data
      ((vnew double-float vnew-%data% vnew-%offset%)
       (v double-float v-%data% v-%offset%)
       (hes double-float hes-%data% hes-%offset%))
    (prog ((arg 0.0d0) (sumdsq 0.0d0) (tem 0.0d0) (vnrm 0.0d0) (i 0) (i0 0))
      (declare (type (f2cl-lib:integer4) i0 i)
               (type (double-float) vnrm tem sumdsq arg))
      (setf vnrm (dnrm2 n vnew 1))
      (setf i0
              (max (the f2cl-lib:integer4 1)
                   (the f2cl-lib:integer4
                        (f2cl-lib:int-add (f2cl-lib:int-sub ll kmp) 1))))
      (f2cl-lib:fdo (i i0 (f2cl-lib:int-add i 1))
                    ((> i ll) nil)
        (tagbody
          (setf (f2cl-lib:fref hes-%data%
                               (i ll)
                               ((1 ldhes) (1 *))
                               hes-%offset%)
                  (ddot n
                   (f2cl-lib:array-slice v-%data%
                                         double-float
                                         (1 i)
                                         ((1 n) (1 *))
                                         v-%offset%)
                   1 vnew 1))
          (setf tem
                  (-
                   (f2cl-lib:fref hes-%data%
                                  (i ll)
                                  ((1 ldhes) (1 *))
                                  hes-%offset%)))
          (daxpy n tem
           (f2cl-lib:array-slice v-%data%
                                 double-float
                                 (1 i)
                                 ((1 n) (1 *))
                                 v-%offset%)
           1 vnew 1)
         label10))
      (setf snormw (dnrm2 n vnew 1))
      (if (/= (+ vnrm (* 0.001d0 snormw)) vnrm) (go end_label))
      (setf sumdsq 0.0d0)
      (f2cl-lib:fdo (i i0 (f2cl-lib:int-add i 1))
                    ((> i ll) nil)
        (tagbody
          (setf tem
                  (-
                   (ddot n
                    (f2cl-lib:array-slice v-%data%
                                          double-float
                                          (1 i)
                                          ((1 n) (1 *))
                                          v-%offset%)
                    1 vnew 1)))
          (if
           (=
            (+ (f2cl-lib:fref hes-%data% (i ll) ((1 ldhes) (1 *)) hes-%offset%)
               (* 0.001d0 tem))
            (f2cl-lib:fref hes-%data% (i ll) ((1 ldhes) (1 *)) hes-%offset%))
           (go label30))
          (setf (f2cl-lib:fref hes-%data%
                               (i ll)
                               ((1 ldhes) (1 *))
                               hes-%offset%)
                  (-
                   (f2cl-lib:fref hes-%data%
                                  (i ll)
                                  ((1 ldhes) (1 *))
                                  hes-%offset%)
                   tem))
          (daxpy n tem
           (f2cl-lib:array-slice v-%data%
                                 double-float
                                 (1 i)
                                 ((1 n) (1 *))
                                 v-%offset%)
           1 vnew 1)
          (setf sumdsq (+ sumdsq (expt tem 2)))
         label30))
      (if (= sumdsq 0.0d0) (go end_label))
      (setf arg (max 0.0d0 (- (expt snormw 2) sumdsq)))
      (setf snormw (f2cl-lib:fsqrt arg))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil snormw)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dorthog
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float))
           :return-values '(nil nil nil nil nil nil nil
                            fortran-to-lisp::snormw)
           :calls '(fortran-to-lisp::daxpy fortran-to-lisp::ddot
                    fortran-to-lisp::dnrm2))))

