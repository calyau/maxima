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
;;;           (:float-format single-float))

(in-package "FFTPACK5")


(defun c1f2kb (ido l1 na cc in1 ch in2 wa)
  (declare (type (array double-float (*)) wa ch cc)
           (type (f2cl-lib:integer4) in2 in1 na l1 ido))
  (f2cl-lib:with-multi-array-data
      ((cc double-float cc-%data% cc-%offset%)
       (ch double-float ch-%data% ch-%offset%)
       (wa double-float wa-%data% wa-%offset%))
    (prog ((ti2 0.0d0) (tr2 0.0d0) (i 0) (chold2 0.0d0) (chold1 0.0d0) (k 0))
      (declare (type (f2cl-lib:integer4) k i)
               (type (double-float) chold1 chold2 tr2 ti2))
      (if (or (> ido 1) (= na 1)) (go label102))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k l1) nil)
        (tagbody
          (setf chold1
                  (+
                   (f2cl-lib:fref cc-%data%
                                  (1 k 1 1)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (1 k 1 2)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref cc-%data%
                               (1 k 1 2)
                               ((1 in1) (1 l1) (1 ido) (1 2))
                               cc-%offset%)
                  (-
                   (f2cl-lib:fref cc-%data%
                                  (1 k 1 1)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (1 k 1 2)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref cc-%data%
                               (1 k 1 1)
                               ((1 in1) (1 l1) (1 ido) (1 2))
                               cc-%offset%)
                  chold1)
          (setf chold2
                  (+
                   (f2cl-lib:fref cc-%data%
                                  (2 k 1 1)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (2 k 1 2)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref cc-%data%
                               (2 k 1 2)
                               ((1 in1) (1 l1) (1 ido) (1 2))
                               cc-%offset%)
                  (-
                   (f2cl-lib:fref cc-%data%
                                  (2 k 1 1)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (2 k 1 2)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref cc-%data%
                               (2 k 1 1)
                               ((1 in1) (1 l1) (1 ido) (1 2))
                               cc-%offset%)
                  chold2)
         label101))
      (go end_label)
     label102
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k l1) nil)
        (tagbody
          (setf (f2cl-lib:fref ch-%data%
                               (1 k 1 1)
                               ((1 in2) (1 l1) (1 2) (1 ido))
                               ch-%offset%)
                  (+
                   (f2cl-lib:fref cc-%data%
                                  (1 k 1 1)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (1 k 1 2)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref ch-%data%
                               (1 k 2 1)
                               ((1 in2) (1 l1) (1 2) (1 ido))
                               ch-%offset%)
                  (-
                   (f2cl-lib:fref cc-%data%
                                  (1 k 1 1)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (1 k 1 2)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref ch-%data%
                               (2 k 1 1)
                               ((1 in2) (1 l1) (1 2) (1 ido))
                               ch-%offset%)
                  (+
                   (f2cl-lib:fref cc-%data%
                                  (2 k 1 1)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (2 k 1 2)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)))
          (setf (f2cl-lib:fref ch-%data%
                               (2 k 2 1)
                               ((1 in2) (1 l1) (1 2) (1 ido))
                               ch-%offset%)
                  (-
                   (f2cl-lib:fref cc-%data%
                                  (2 k 1 1)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)
                   (f2cl-lib:fref cc-%data%
                                  (2 k 1 2)
                                  ((1 in1) (1 l1) (1 ido) (1 2))
                                  cc-%offset%)))
         label103))
      (if (= ido 1) (go end_label))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i ido) nil)
        (tagbody
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k l1) nil)
            (tagbody
              (setf (f2cl-lib:fref ch-%data%
                                   (1 k 1 i)
                                   ((1 in2) (1 l1) (1 2) (1 ido))
                                   ch-%offset%)
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (1 k i 1)
                                      ((1 in1) (1 l1) (1 ido) (1 2))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 k i 2)
                                      ((1 in1) (1 l1) (1 ido) (1 2))
                                      cc-%offset%)))
              (setf tr2
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (1 k i 1)
                                      ((1 in1) (1 l1) (1 ido) (1 2))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (1 k i 2)
                                      ((1 in1) (1 l1) (1 ido) (1 2))
                                      cc-%offset%)))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 k 1 i)
                                   ((1 in2) (1 l1) (1 2) (1 ido))
                                   ch-%offset%)
                      (+
                       (f2cl-lib:fref cc-%data%
                                      (2 k i 1)
                                      ((1 in1) (1 l1) (1 ido) (1 2))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 k i 2)
                                      ((1 in1) (1 l1) (1 ido) (1 2))
                                      cc-%offset%)))
              (setf ti2
                      (-
                       (f2cl-lib:fref cc-%data%
                                      (2 k i 1)
                                      ((1 in1) (1 l1) (1 ido) (1 2))
                                      cc-%offset%)
                       (f2cl-lib:fref cc-%data%
                                      (2 k i 2)
                                      ((1 in1) (1 l1) (1 ido) (1 2))
                                      cc-%offset%)))
              (setf (f2cl-lib:fref ch-%data%
                                   (2 k 2 i)
                                   ((1 in2) (1 l1) (1 2) (1 ido))
                                   ch-%offset%)
                      (+
                       (*
                        (f2cl-lib:fref wa-%data%
                                       (i 1 1)
                                       ((1 ido) (1 1) (1 2))
                                       wa-%offset%)
                        ti2)
                       (*
                        (f2cl-lib:fref wa-%data%
                                       (i 1 2)
                                       ((1 ido) (1 1) (1 2))
                                       wa-%offset%)
                        tr2)))
              (setf (f2cl-lib:fref ch-%data%
                                   (1 k 2 i)
                                   ((1 in2) (1 l1) (1 2) (1 ido))
                                   ch-%offset%)
                      (-
                       (*
                        (f2cl-lib:fref wa-%data%
                                       (i 1 1)
                                       ((1 ido) (1 1) (1 2))
                                       wa-%offset%)
                        tr2)
                       (*
                        (f2cl-lib:fref wa-%data%
                                       (i 1 2)
                                       ((1 ido) (1 1) (1 2))
                                       wa-%offset%)
                        ti2)))
             label104))
         label105))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::c1f2kb
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil)
           :calls 'nil)))

