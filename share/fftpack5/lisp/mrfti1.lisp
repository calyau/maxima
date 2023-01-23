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


(let ((ntryh
       (make-array 4
                   :element-type 'f2cl-lib:integer4
                   :initial-contents '(4 2 3 5))))
  (declare (type (array f2cl-lib:integer4 (4)) ntryh))
  (defun mrfti1 (n wa fac)
    (declare (type (array double-float (*)) fac)
             (type (array double-float (*)) wa)
             (type (f2cl-lib:integer4) n))
    (f2cl-lib:with-multi-array-data
        ((wa double-float wa-%data% wa-%offset%)
         (fac double-float fac-%data% fac-%offset%))
      (prog ((tpi 0.0d0) (argh 0.0d0) (argld 0.0d0) (arg 0.0d0) (ii 0)
             (fi 0.0d0) (ipm 0) (ido 0) (l2 0) (ld 0) (ip 0) (k1 0) (l1 0)
             (nfm1 0) (is 0) (ib 0) (i 0) (nr 0) (nq 0) (ntry 0) (j 0) (nf 0)
             (nl 0))
        (declare (type (f2cl-lib:integer4) nl nf j ntry nq nr i ib is nfm1 l1
                                           k1 ip ld l2 ido ipm ii)
                 (type (double-float) fi arg argld argh tpi))
        (setf nl n)
        (setf nf 0)
        (setf j 0)
       label101
        (setf j (f2cl-lib:int-add j 1))
        (f2cl-lib:arithmetic-if (f2cl-lib:int-sub j 4)
                                (go label102)
                                (go label102)
                                (go label103))
       label102
        (setf ntry (f2cl-lib:fref ntryh (j) ((1 4))))
        (go label104)
       label103
        (setf ntry (f2cl-lib:int-add ntry 2))
       label104
        (setf nq (the f2cl-lib:integer4 (truncate nl ntry)))
        (setf nr (f2cl-lib:int-sub nl (f2cl-lib:int-mul ntry nq)))
        (f2cl-lib:arithmetic-if nr (go label101) (go label105) (go label101))
       label105
        (setf nf (f2cl-lib:int-add nf 1))
        (setf (f2cl-lib:fref fac-%data%
                             ((f2cl-lib:int-add nf 2))
                             ((1 15))
                             fac-%offset%)
                (coerce (the f2cl-lib:integer4 ntry) 'double-float))
        (setf nl nq)
        (if (/= ntry 2) (go label107))
        (if (= nf 1) (go label107))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i nf) nil)
          (tagbody
            (setf ib (f2cl-lib:int-add (f2cl-lib:int-sub nf i) 2))
            (setf (f2cl-lib:fref fac-%data%
                                 ((f2cl-lib:int-add ib 2))
                                 ((1 15))
                                 fac-%offset%)
                    (f2cl-lib:fref fac-%data%
                                   ((f2cl-lib:int-add ib 1))
                                   ((1 15))
                                   fac-%offset%))
           label106))
        (setf (f2cl-lib:fref fac-%data% (3) ((1 15)) fac-%offset%)
                (coerce (the f2cl-lib:integer4 2) 'double-float))
       label107
        (if (/= nl 1) (go label104))
        (setf (f2cl-lib:fref fac-%data% (1) ((1 15)) fac-%offset%)
                (coerce (the f2cl-lib:integer4 n) 'double-float))
        (setf (f2cl-lib:fref fac-%data% (2) ((1 15)) fac-%offset%)
                (coerce (the f2cl-lib:integer4 nf) 'double-float))
        (setf tpi (* 8.0d0 (f2cl-lib:datan 1.0d0)))
        (setf argh (/ tpi (f2cl-lib:ffloat n)))
        (setf is 0)
        (setf nfm1 (f2cl-lib:int-sub nf 1))
        (setf l1 1)
        (if (= nfm1 0) (go end_label))
        (f2cl-lib:fdo (k1 1 (f2cl-lib:int-add k1 1))
                      ((> k1 nfm1) nil)
          (tagbody
            (setf ip
                    (f2cl-lib:int
                     (f2cl-lib:fref fac-%data%
                                    ((f2cl-lib:int-add k1 2))
                                    ((1 15))
                                    fac-%offset%)))
            (setf ld 0)
            (setf l2 (f2cl-lib:int-mul l1 ip))
            (setf ido (the f2cl-lib:integer4 (truncate n l2)))
            (setf ipm (f2cl-lib:int-sub ip 1))
            (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                          ((> j ipm) nil)
              (tagbody
                (setf ld (f2cl-lib:int-add ld l1))
                (setf i is)
                (setf argld (* (f2cl-lib:ffloat ld) argh))
                (setf fi 0.0d0)
                (f2cl-lib:fdo (ii 3 (f2cl-lib:int-add ii 2))
                              ((> ii ido) nil)
                  (tagbody
                    (setf i (f2cl-lib:int-add i 2))
                    (setf fi (+ fi 1.0d0))
                    (setf arg (* fi argld))
                    (setf (f2cl-lib:fref wa-%data%
                                         ((f2cl-lib:int-sub i 1))
                                         ((1 n))
                                         wa-%offset%)
                            (f2cl-lib:dcos arg))
                    (setf (f2cl-lib:fref wa-%data% (i) ((1 n)) wa-%offset%)
                            (f2cl-lib:dsin arg))
                   label108))
                (setf is (f2cl-lib:int-add is ido))
               label109))
            (setf l1 l2)
           label110))
        (go end_label)
       end_label
        (return (values nil nil nil))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mrfti1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil)
           :calls 'nil)))

