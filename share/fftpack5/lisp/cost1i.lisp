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


(defun cost1i (n wsave lensav ier)
  (declare (type (array double-float (*)) wsave)
           (type (f2cl-lib:integer4) ier lensav n))
  (f2cl-lib:with-multi-array-data
      ((wsave double-float wsave-%data% wsave-%offset%))
    (prog ((ier1 0) (lnsv 0) (kc 0) (k 0) (fk 0.0d0) (dt 0.0d0) (pi$ 0.0d0)
           (ns2 0) (np1 0) (nm1 0))
      (declare (type (double-float) pi$ dt fk)
               (type (f2cl-lib:integer4) nm1 np1 ns2 k kc lnsv ier1))
      (setf ier 0)
      (cond
        ((< lensav
            (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                              (f2cl-lib:int
                               (f2cl-lib:f2cl/
                                (f2cl-lib:flog (f2cl-lib:freal n))
                                (f2cl-lib:flog 2.0d0)))
                              4))
         (setf ier 2)
         (xerfft "COST1I" 3)
         (go label300)))
      (if (<= n 3) (go end_label))
      (setf nm1 (f2cl-lib:int-sub n 1))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf ns2 (the f2cl-lib:integer4 (truncate n 2)))
      (setf pi$ (* 4.0d0 (atan 1.0d0)))
      (setf dt (/ pi$ (f2cl-lib:ffloat nm1)))
      (setf fk 0.0d0)
      (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                    ((> k ns2) nil)
        (tagbody
          (setf kc (f2cl-lib:int-sub np1 k))
          (setf fk (+ fk 1.0d0))
          (setf (f2cl-lib:fref wsave-%data% (k) ((1 lensav)) wsave-%offset%)
                  (* 2.0d0 (sin (* fk dt))))
          (setf (f2cl-lib:fref wsave-%data% (kc) ((1 lensav)) wsave-%offset%)
                  (* 2.0d0 (cos (* fk dt))))
         label101))
      (setf lnsv
              (f2cl-lib:int-add nm1
                                (f2cl-lib:int
                                 (/ (f2cl-lib:flog (f2cl-lib:freal nm1))
                                    (f2cl-lib:flog 2.0d0)))
                                4))
      (multiple-value-bind (var-0 var-1 var-2 var-3)
          (rfft1i nm1
           (f2cl-lib:array-slice wsave-%data%
                                 double-float
                                 ((+ n 1))
                                 ((1 lensav))
                                 wsave-%offset%)
           lnsv ier1)
        (declare (ignore var-0 var-1 var-2))
        (setf ier1 var-3))
      (cond
        ((/= ier1 0)
         (setf ier 20)
         (xerfft "COST1I" -5)))
     label300
      (go end_label)
     end_label
      (return (values nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cost1i
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::rfft1i fortran-to-lisp::xerfft))))

