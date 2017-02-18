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


(defun dainvg (res adda neq t$ y ydot miter ml mu pw ipvt ier)
  (declare (type (array f2cl-lib:integer4 (*)) ipvt)
           (type (f2cl-lib:integer4) ier mu ml miter)
           (type (array double-float (*)) pw ydot y)
           (type (double-float) t$)
           (type (array f2cl-lib:integer4 (*)) neq))
  (f2cl-lib:with-multi-array-data
      ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
       (y double-float y-%data% y-%offset%)
       (ydot double-float ydot-%data% ydot-%offset%)
       (pw double-float pw-%data% pw-%offset%)
       (ipvt f2cl-lib:integer4 ipvt-%data% ipvt-%offset%))
    (prog ((i 0) (lenpw 0) (mlp1 0) (nrowpw 0) (itemp 0))
      (declare (type (f2cl-lib:integer4) itemp nrowpw mlp1 lenpw i))
      (if (>= miter 4) (go label100))
      (setf lenpw
              (f2cl-lib:int-mul
               (f2cl-lib:fref neq-%data% (1) ((1 1)) neq-%offset%)
               (f2cl-lib:fref neq-%data% (1) ((1 1)) neq-%offset%)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i lenpw) nil)
        (tagbody
         label10
          (setf (f2cl-lib:fref pw-%data% (i) ((1 *)) pw-%offset%) 0.0d0)))
      (setf ier 1)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
          (funcall res neq t$ y pw ydot ier)
        (declare (ignore var-0 var-2 var-3 var-4))
        (when var-1
          (setf t$ var-1))
        (when var-5
          (setf ier var-5)))
      (if (> ier 1) (go end_label))
      (setf itemp (f2cl-lib:fref neq-%data% (1) ((1 1)) neq-%offset%))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
          (funcall adda neq t$ y 0 0 pw itemp)
        (declare (ignore var-0 var-2 var-3 var-4 var-5))
        (when var-1
          (setf t$ var-1))
        (when var-6
          (setf itemp var-6)))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
          (dgefa pw itemp itemp ipvt ier)
        (declare (ignore var-0 var-1 var-2 var-3))
        (setf ier var-4))
      (if (= ier 0) (go label20))
      (setf ier (f2cl-lib:int-sub ier))
      (go end_label)
     label20
      (dgesl pw itemp itemp ipvt ydot 0)
      (go end_label)
     label100
      (setf nrowpw (f2cl-lib:int-add (f2cl-lib:int-mul 2 ml) mu 1))
      (setf lenpw
              (f2cl-lib:int-mul
               (f2cl-lib:fref neq-%data% (1) ((1 1)) neq-%offset%)
               nrowpw))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i lenpw) nil)
        (tagbody
         label110
          (setf (f2cl-lib:fref pw-%data% (i) ((1 *)) pw-%offset%) 0.0d0)))
      (setf ier 1)
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
          (funcall res neq t$ y pw ydot ier)
        (declare (ignore var-0 var-2 var-3 var-4))
        (when var-1
          (setf t$ var-1))
        (when var-5
          (setf ier var-5)))
      (if (> ier 1) (go end_label))
      (setf mlp1 (f2cl-lib:int-add ml 1))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
          (funcall adda
                   neq
                   t$
                   y
                   ml
                   mu
                   (f2cl-lib:array-slice pw-%data%
                                         double-float
                                         (mlp1)
                                         ((1 *))
                                         pw-%offset%)
                   nrowpw)
        (declare (ignore var-0 var-2 var-5))
        (when var-1
          (setf t$ var-1))
        (when var-3
          (setf ml var-3))
        (when var-4
          (setf mu var-4))
        (when var-6
          (setf nrowpw var-6)))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
          (dgbfa pw nrowpw (aref neq 0) ml mu ipvt ier)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
        (setf ier var-6))
      (if (= ier 0) (go label120))
      (setf ier (f2cl-lib:int-sub ier))
      (go end_label)
     label120
      (dgbsl pw nrowpw (aref neq 0) ml mu ipvt ydot 0)
      (go end_label)
     end_label
      (return (values nil nil nil t$ nil nil nil ml mu nil nil ier)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dainvg
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t t (array fortran-to-lisp::integer4 (*))
                        (double-float) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::t$ nil nil nil
                            fortran-to-lisp::ml fortran-to-lisp::mu nil nil
                            fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::dgbsl fortran-to-lisp::dgbfa
                    fortran-to-lisp::dgesl fortran-to-lisp::dgefa))))

