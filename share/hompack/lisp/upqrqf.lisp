;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2020-04 (21D Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "HOMPACK")


(defun upqrqf (n eta s f0 f1 qt r w t$)
  (declare (type (array double-float (*)) t$ w r qt f1 f0 s)
           (type (double-float) eta)
           (type (f2cl-lib:integer4) n))
  (f2cl-lib:with-multi-array-data
      ((s double-float s-%data% s-%offset%)
       (f0 double-float f0-%data% f0-%offset%)
       (f1 double-float f1-%data% f1-%offset%)
       (qt double-float qt-%data% qt-%offset%)
       (r double-float r-%data% r-%offset%)
       (w double-float w-%data% w-%offset%)
       (t$ double-float t$-%data% t$-%offset%))
    (prog ((tt (make-array 2 :element-type 'double-float)) (skipup nil) (i 0)
           (indexr 0) (indxr2 0) (j 0) (k 0) (c 0.0) (den 0.0) (one 0.0)
           (ss 0.0) (ww 0.0) (yy 0.0) (dnrm2 0.0))
      (declare (type (double-float) dnrm2 yy ww ss one den c)
               (type (f2cl-lib:integer4) k j indxr2 indexr i)
               (type f2cl-lib:logical skipup)
               (type (array double-float (2)) tt))
      (setf one (coerce 1.0f0 'double-float))
      (setf skipup f2cl-lib:%true%)
      (setf indexr 1)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref t$-%data% (i) ((1 n)) t$-%offset%)
                  (ddot (f2cl-lib:int-add (f2cl-lib:int-sub n i) 1)
                   (f2cl-lib:array-slice r-%data%
                                         double-float
                                         (indexr)
                                         ((1
                                           (f2cl-lib:f2cl/
                                            (f2cl-lib:int-mul n
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            2)))
                                         r-%offset%)
                   1
                   (f2cl-lib:array-slice s-%data%
                                         double-float
                                         (i)
                                         ((1 n))
                                         s-%offset%)
                   1))
          (setf indexr
                  (f2cl-lib:int-add
                   (f2cl-lib:int-sub (f2cl-lib:int-add indexr n) i)
                   1))
         label10))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref w-%data% (i) ((1 n)) w-%offset%)
                  (- (f2cl-lib:fref f1-%data% (i) ((1 n)) f1-%offset%)
                     (f2cl-lib:fref f0-%data% (i) ((1 n)) f0-%offset%)
                     (ddot n
                      (f2cl-lib:array-slice qt-%data%
                                            double-float
                                            (1 i)
                                            ((1 n) (1 n))
                                            qt-%offset%)
                      1 t$ 1)))
          (cond
            ((> (abs (f2cl-lib:fref w (i) ((1 n))))
                (* eta
                   (+ (abs (f2cl-lib:fref f1 (i) ((1 n))))
                      (abs (f2cl-lib:fref f0 (i) ((1 n)))))))
             (setf skipup f2cl-lib:%false%))
            (t
             (setf (f2cl-lib:fref w-%data% (i) ((1 n)) w-%offset%)
                     (coerce 0.0f0 'double-float))))
         label20))
      (if skipup (go end_label))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref t$-%data% (i) ((1 n)) t$-%offset%)
                  (ddot n
                   (f2cl-lib:array-slice qt-%data%
                                         double-float
                                         (i 1)
                                         ((1 n) (1 n))
                                         qt-%offset%)
                   n w 1))
         label30))
      (setf den (/ 1.0f0 (ddot n s 1 s 1)))
      (dscal n den s 1)
      (r1upqf n s t$ qt r w)
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::upqrqf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dscal fortran-to-lisp::ddot
                    fortran-to-lisp::r1upqf))))

