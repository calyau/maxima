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


(defun solvds (nn a nwk maxa v)
  (declare (type (array f2cl-lib:integer4 (*)) maxa)
           (type (array double-float (*)) v a)
           (type (f2cl-lib:integer4) nwk nn))
  (f2cl-lib:with-multi-array-data
      ((a double-float a-%data% a-%offset%)
       (v double-float v-%data% v-%offset%)
       (maxa f2cl-lib:integer4 maxa-%data% maxa-%offset%))
    (prog ((c 0.0) (k 0) (kk 0) (kl 0) (ku 0) (l 0) (n 0))
      (declare (type (f2cl-lib:integer4) n l ku kl kk k)
               (type (double-float) c))
      (f2cl-lib:fdo (n 1 (f2cl-lib:int-add n 1))
                    ((> n nn) nil)
        (tagbody
          (setf kl
                  (f2cl-lib:int-add
                   (f2cl-lib:fref maxa-%data%
                                  (n)
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  maxa-%offset%)
                   1))
          (setf ku
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref maxa-%data%
                                  ((f2cl-lib:int-add n 1))
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  maxa-%offset%)
                   1))
          (f2cl-lib:arithmetic-if (f2cl-lib:int-sub ku kl)
                                  (go label180)
                                  (go label160)
                                  (go label160))
         label160
          (setf k n)
          (setf c (coerce 0.0f0 'double-float))
          (f2cl-lib:fdo (kk kl (f2cl-lib:int-add kk 1))
                        ((> kk ku) nil)
            (tagbody
              (setf k (f2cl-lib:int-sub k 1))
              (setf c
                      (+ c
                         (* (f2cl-lib:fref a-%data% (kk) ((1 nwk)) a-%offset%)
                            (f2cl-lib:fref v-%data% (k) ((1 nn)) v-%offset%))))
             label170))
          (setf (f2cl-lib:fref v-%data% (n) ((1 nn)) v-%offset%)
                  (- (f2cl-lib:fref v-%data% (n) ((1 nn)) v-%offset%) c))
         label180))
     label800
      (f2cl-lib:fdo (n 1 (f2cl-lib:int-add n 1))
                    ((> n nn) nil)
        (tagbody
          (setf k
                  (f2cl-lib:fref maxa-%data%
                                 (n)
                                 ((1 (f2cl-lib:int-add nn 1)))
                                 maxa-%offset%))
          (setf (f2cl-lib:fref v-%data% (n) ((1 nn)) v-%offset%)
                  (/ (f2cl-lib:fref v-%data% (n) ((1 nn)) v-%offset%)
                     (f2cl-lib:fref a-%data% (k) ((1 nwk)) a-%offset%)))
         label480))
      (if (= nn 1) (go end_label))
      (setf n nn)
      (f2cl-lib:fdo (l 2 (f2cl-lib:int-add l 1))
                    ((> l nn) nil)
        (tagbody
          (setf kl
                  (f2cl-lib:int-add
                   (f2cl-lib:fref maxa-%data%
                                  (n)
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  maxa-%offset%)
                   1))
          (setf ku
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref maxa-%data%
                                  ((f2cl-lib:int-add n 1))
                                  ((1 (f2cl-lib:int-add nn 1)))
                                  maxa-%offset%)
                   1))
          (f2cl-lib:arithmetic-if (f2cl-lib:int-sub ku kl)
                                  (go label530)
                                  (go label510)
                                  (go label510))
         label510
          (setf k n)
          (f2cl-lib:fdo (kk kl (f2cl-lib:int-add kk 1))
                        ((> kk ku) nil)
            (tagbody
              (setf k (f2cl-lib:int-sub k 1))
              (setf (f2cl-lib:fref v-%data% (k) ((1 nn)) v-%offset%)
                      (- (f2cl-lib:fref v-%data% (k) ((1 nn)) v-%offset%)
                         (* (f2cl-lib:fref a-%data% (kk) ((1 nwk)) a-%offset%)
                            (f2cl-lib:fref v-%data% (n) ((1 nn)) v-%offset%))))
             label520))
         label530
          (setf n (f2cl-lib:int-sub n 1))
         label500))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::solvds
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil)
           :calls 'nil)))

