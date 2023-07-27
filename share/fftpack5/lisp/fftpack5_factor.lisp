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
  (defun factor (n nf fac)
    (declare (type (array double-float (*)) fac)
             (type (f2cl-lib:integer4) nf n))
    (f2cl-lib:with-multi-array-data
        ((fac double-float fac-%data% fac-%offset%))
      (prog ((nr 0) (nq 0) (ntry 0) (j 0) (nl 0))
        (declare (type (f2cl-lib:integer4) nl j ntry nq nr))
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
        (setf (f2cl-lib:fref fac-%data% (nf) ((1 *)) fac-%offset%)
                (coerce (the f2cl-lib:integer4 ntry) 'double-float))
        (setf nl nq)
        (if (/= nl 1) (go label104))
        (go end_label)
       end_label
        (return (values nil nf nil))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::factor
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)))
           :return-values '(nil fortran-to-lisp::nf nil)
           :calls 'nil)))

