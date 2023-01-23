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


(defun multds (y aa x maxa nn lenaa)
  (declare (type (f2cl-lib:integer4) lenaa nn)
           (type (array f2cl-lib:integer4 (*)) maxa)
           (type (array double-float (*)) x aa y))
  (f2cl-lib:with-multi-array-data
      ((y double-float y-%data% y-%offset%)
       (aa double-float aa-%data% aa-%offset%)
       (x double-float x-%data% x-%offset%)
       (maxa f2cl-lib:integer4 maxa-%data% maxa-%offset%))
    (prog ((b 0.0) (cc 0.0) (i 0) (ii 0) (kk 0) (kl 0) (ku 0))
      (declare (type (f2cl-lib:integer4) ku kl kk ii i)
               (type (double-float) cc b))
      (if (> lenaa nn) (go label20))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
         label10
          (setf (f2cl-lib:fref y-%data% (i) ((1 nn)) y-%offset%)
                  (* (f2cl-lib:fref aa-%data% (i) ((1 lenaa)) aa-%offset%)
                     (f2cl-lib:fref x-%data% (i) ((1 nn)) x-%offset%)))))
      (go end_label)
     label20
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
         label40
          (setf (f2cl-lib:fref y-%data% (i) ((1 nn)) y-%offset%)
                  (coerce 0.0f0 'double-float))))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf kl
                  (f2cl-lib:fref maxa-%data%
                                 (i)
                                 ((1 (f2cl-lib:int-add nn 1)))
                                 maxa-%offset%))
          (setf ku
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref maxa-%data%
                                  ((f2cl-lib:int-add i 1))
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  maxa-%offset%)
                   1))
          (setf ii (f2cl-lib:int-add i 1))
          (setf cc (f2cl-lib:fref x-%data% (i) ((1 nn)) x-%offset%))
          (f2cl-lib:fdo (kk kl (f2cl-lib:int-add kk 1))
                        ((> kk ku) nil)
            (tagbody
              (setf ii (f2cl-lib:int-sub ii 1))
              (setf (f2cl-lib:fref y-%data% (ii) ((1 nn)) y-%offset%)
                      (+ (f2cl-lib:fref y-%data% (ii) ((1 nn)) y-%offset%)
                         (*
                          (f2cl-lib:fref aa-%data%
                                         (kk)
                                         ((1 lenaa))
                                         aa-%offset%)
                          cc)))))))
     label100
      (if (= nn 1) (go end_label))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i nn) nil)
        (tagbody
          (setf kl
                  (f2cl-lib:int-add
                   (f2cl-lib:fref maxa-%data%
                                  (i)
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  maxa-%offset%)
                   1))
          (setf ku
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref maxa-%data%
                                  ((f2cl-lib:int-add i 1))
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  maxa-%offset%)
                   1))
          (f2cl-lib:arithmetic-if (f2cl-lib:int-sub ku kl)
                                  (go label200)
                                  (go label210)
                                  (go label210))
         label210
          (setf ii i)
          (setf b (coerce 0.0f0 'double-float))
          (f2cl-lib:fdo (kk kl (f2cl-lib:int-add kk 1))
                        ((> kk ku) nil)
            (tagbody
              (setf ii (f2cl-lib:int-sub ii 1))
             label220
              (setf b
                      (+ b
                         (*
                          (f2cl-lib:fref aa-%data%
                                         (kk)
                                         ((1 lenaa))
                                         aa-%offset%)
                          (f2cl-lib:fref x-%data%
                                         (ii)
                                         ((1 nn))
                                         x-%offset%))))))
          (setf (f2cl-lib:fref y-%data% (i) ((1 nn)) y-%offset%)
                  (+ (f2cl-lib:fref y-%data% (i) ((1 nn)) y-%offset%) b))
         label200))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::multds
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil)
           :calls 'nil)))

