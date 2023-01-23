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


(defun mcfti1 (n wa fnf fac)
  (declare (type (double-float) fnf)
           (type (array double-float (*)) fac wa)
           (type (f2cl-lib:integer4) n))
  (f2cl-lib:with-multi-array-data
      ((wa double-float wa-%data% wa-%offset%)
       (fac double-float fac-%data% fac-%offset%))
    (prog ((ido 0) (l2 0) (ip 0) (k1 0) (l1 0) (iw 0) (nf 0))
      (declare (type (f2cl-lib:integer4) nf iw l1 k1 ip l2 ido))
      (multiple-value-bind (var-0 var-1 var-2)
          (factor n nf fac)
        (declare (ignore var-0 var-2))
        (setf nf var-1))
      (setf fnf (coerce (the f2cl-lib:integer4 nf) 'double-float))
      (setf iw 1)
      (setf l1 1)
      (f2cl-lib:fdo (k1 1 (f2cl-lib:int-add k1 1))
                    ((> k1 nf) nil)
        (tagbody
          (setf ip
                  (f2cl-lib:int
                   (f2cl-lib:fref fac-%data% (k1) ((1 *)) fac-%offset%)))
          (setf l2 (f2cl-lib:int-mul l1 ip))
          (setf ido (the f2cl-lib:integer4 (truncate n l2)))
          (tables ido ip
           (f2cl-lib:array-slice wa-%data%
                                 double-float
                                 (iw)
                                 ((1 *))
                                 wa-%offset%))
          (setf iw
                  (f2cl-lib:int-add iw
                                    (f2cl-lib:int-mul (f2cl-lib:int-sub ip 1)
                                                      (f2cl-lib:int-add ido
                                                                        ido))))
          (setf l1 l2)
         label110))
      (go end_label)
     end_label
      (return (values nil nil fnf nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::mcfti1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (double-float) (array double-float (*)))
           :return-values '(nil nil fortran-to-lisp::fnf nil)
           :calls '(fortran-to-lisp::tables fortran-to-lisp::factor))))

