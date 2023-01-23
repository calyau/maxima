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


(defun sinqmf (lot jump n inc x lenx wsave lensav work lenwrk ier)
  (declare (type (array double-float (*)) work wsave x)
           (type (f2cl-lib:integer4) ier lenwrk lensav lenx inc n jump lot))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (wsave double-float wsave-%data% wsave-%offset%)
       (work double-float work-%data% work-%offset%))
    (prog ((ier1 0) (xhold 0.0d0) (m 0) (kc 0) (k 0) (lj 0) (ns2 0))
      (declare (type (double-float) xhold)
               (type (f2cl-lib:integer4) ns2 lj k kc m ier1))
      (setf ier 0)
      (cond
        ((< lenx
            (f2cl-lib:int-add
             (f2cl-lib:int-mul (f2cl-lib:int-add lot (f2cl-lib:int-sub 1))
                               jump)
             (f2cl-lib:int-mul inc (f2cl-lib:int-add n (f2cl-lib:int-sub 1)))
             1))
         (setf ier 1)
         (xerfft "SINQMF" 6)
         (go label300))
        ((< lensav
            (f2cl-lib:int-add (f2cl-lib:int-mul 2 n)
                              (f2cl-lib:int
                               (f2cl-lib:f2cl/
                                (f2cl-lib:flog (f2cl-lib:freal n))
                                (f2cl-lib:flog 2.0d0)))
                              4))
         (setf ier 2)
         (xerfft "SINQMF" 8)
         (go label300))
        ((< lenwrk (f2cl-lib:int-mul lot n))
         (setf ier 3)
         (xerfft "SINQMF" 10)
         (go label300))
        ((not (xercon inc jump n lot))
         (setf ier 4)
         (xerfft "SINQMF" -1)
         (go label300)))
      (if (= n 1) (go end_label))
      (setf ns2 (the f2cl-lib:integer4 (truncate n 2)))
      (setf lj
              (f2cl-lib:int-add
               (f2cl-lib:int-mul (f2cl-lib:int-sub lot 1) jump)
               1))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k ns2) nil)
        (tagbody
          (setf kc (f2cl-lib:int-sub n k))
          (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m jump))
                        ((> m lj) nil)
            (tagbody
              (setf xhold
                      (f2cl-lib:fref x-%data%
                                     (m k)
                                     ((1 inc) (1 *))
                                     x-%offset%))
              (setf (f2cl-lib:fref x-%data% (m k) ((1 inc) (1 *)) x-%offset%)
                      (f2cl-lib:fref x-%data%
                                     (m (f2cl-lib:int-add kc 1))
                                     ((1 inc) (1 *))
                                     x-%offset%))
              (setf (f2cl-lib:fref x-%data%
                                   (m (f2cl-lib:int-add kc 1))
                                   ((1 inc) (1 *))
                                   x-%offset%)
                      xhold)
             label201))
         label101))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (cosqmf lot jump n inc x lenx wsave lensav work lenwrk ier1)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9))
        (setf ier1 var-10))
      (cond
        ((/= ier1 0)
         (setf ier 20)
         (xerfft "SINQMF" -5)
         (go label300)))
      (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 2))
                    ((> k n) nil)
        (tagbody
          (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m jump))
                        ((> m lj) nil)
            (tagbody
              (setf (f2cl-lib:fref x-%data% (m k) ((1 inc) (1 *)) x-%offset%)
                      (-
                       (f2cl-lib:fref x-%data%
                                      (m k)
                                      ((1 inc) (1 *))
                                      x-%offset%)))
             label202))
         label102))
     label300
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::sinqmf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::cosqmf fortran-to-lisp::xercon
                    fortran-to-lisp::xerfft))))

