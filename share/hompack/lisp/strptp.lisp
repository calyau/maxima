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


(defun strptp (n icount ideg r x)
  (declare (type (array double-float (*)) x r)
           (type (array f2cl-lib:integer4 (*)) ideg icount)
           (type (f2cl-lib:integer4) n))
  (f2cl-lib:with-multi-array-data
      ((icount f2cl-lib:integer4 icount-%data% icount-%offset%)
       (ideg f2cl-lib:integer4 ideg-%data% ideg-%offset%)
       (r double-float r-%data% r-%offset%)
       (x double-float x-%data% x-%offset%))
    (prog ((xxxx (make-array 2 :element-type 'double-float)) (twopi 0.0)
           (angle 0.0) (j 0))
      (declare (type (f2cl-lib:integer4) j)
               (type (array double-float (2)) xxxx)
               (type (double-float) angle twopi))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (cond
            ((>= (f2cl-lib:fref icount (j) ((1 n)))
                 (f2cl-lib:fref ideg (j) ((1 n))))
             (setf (f2cl-lib:fref icount-%data% (j) ((1 n)) icount-%offset%) 1))
            (t
             (setf (f2cl-lib:fref icount-%data% (j) ((1 n)) icount-%offset%)
                     (f2cl-lib:int-add
                      (f2cl-lib:fref icount-%data% (j) ((1 n)) icount-%offset%)
                      1))
             (go label20)))
         label10))
     label20
      (setf twopi (coerce (* 8.0f0 (atan 1.0f0)) 'double-float))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf angle
                  (*
                   (/ twopi
                      (f2cl-lib:fref ideg-%data% (j) ((1 n)) ideg-%offset%))
                   (f2cl-lib:fref icount-%data% (j) ((1 n)) icount-%offset%)))
          (setf (f2cl-lib:fref xxxx (1) ((1 2))) (cos angle))
          (setf (f2cl-lib:fref xxxx (2) ((1 2))) (sin angle))
          (mulp xxxx
           (f2cl-lib:array-slice r-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 r-%offset%)
           (f2cl-lib:array-slice x-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 x-%offset%))
         label30))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::strptp
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*)))
           :return-values '(nil nil nil nil nil)
           :calls '(fortran-to-lisp::mulp))))

