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


(defun costf1 (n inc x wsave work ier)
  (declare (type (double-float) work)
           (type (array double-float (*)) wsave x)
           (type (f2cl-lib:integer4) ier inc n))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (wsave double-float wsave-%data% wsave-%offset%))
    (prog ((dsum 0.0d0) (xi 0.0d0) (i 0) (snm1 0.0d0) (ier1 0) (lnwk 0)
           (lnsv 0) (lenx 0) (modn 0) (t2 0.0d0) (t1 0.0d0) (kc 0) (k 0)
           (tx2 0.0d0) (x1p3 0.0d0) (x1h 0.0d0) (ns2 0) (np1 0) (nm1 0))
      (declare (type (f2cl-lib:integer4) nm1 np1 ns2 k kc modn lenx lnsv lnwk
                                         ier1 i)
               (type (double-float) x1h x1p3 tx2 t1 t2 snm1 xi dsum))
      (setf ier 0)
      (setf nm1 (f2cl-lib:int-sub n 1))
      (setf np1 (f2cl-lib:int-add n 1))
      (setf ns2 (the f2cl-lib:integer4 (truncate n 2)))
      (f2cl-lib:arithmetic-if (f2cl-lib:int-sub n 2)
                              (go label200)
                              (go label101)
                              (go label102))
     label101
      (setf x1h
              (+ (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
                 (f2cl-lib:fref x-%data% (1 2) ((1 inc) (1 *)) x-%offset%)))
      (setf (f2cl-lib:fref x-%data% (1 2) ((1 inc) (1 *)) x-%offset%)
              (* 0.5d0
                 (- (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
                    (f2cl-lib:fref x-%data%
                                   (1 2)
                                   ((1 inc) (1 *))
                                   x-%offset%))))
      (setf (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
              (* 0.5d0 x1h))
      (go label200)
     label102
      (if (> n 3) (go label103))
      (setf x1p3
              (+ (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
                 (f2cl-lib:fref x-%data% (1 3) ((1 inc) (1 *)) x-%offset%)))
      (setf tx2
              (+ (f2cl-lib:fref x-%data% (1 2) ((1 inc) (1 *)) x-%offset%)
                 (f2cl-lib:fref x-%data% (1 2) ((1 inc) (1 *)) x-%offset%)))
      (setf (f2cl-lib:fref x-%data% (1 2) ((1 inc) (1 *)) x-%offset%)
              (* 0.5d0
                 (- (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
                    (f2cl-lib:fref x-%data%
                                   (1 3)
                                   ((1 inc) (1 *))
                                   x-%offset%))))
      (setf (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
              (* 0.25d0 (+ x1p3 tx2)))
      (setf (f2cl-lib:fref x-%data% (1 3) ((1 inc) (1 *)) x-%offset%)
              (* 0.25d0 (- x1p3 tx2)))
      (go label200)
     label103
      (setf dsum
              (- (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
                 (f2cl-lib:fref x-%data% (1 n) ((1 inc) (1 *)) x-%offset%)))
      (setf (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
              (+ (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
                 (f2cl-lib:fref x-%data% (1 n) ((1 inc) (1 *)) x-%offset%)))
      (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                    ((> k ns2) nil)
        (tagbody
          (setf kc (f2cl-lib:int-sub np1 k))
          (setf t1
                  (+ (f2cl-lib:fref x-%data% (1 k) ((1 inc) (1 *)) x-%offset%)
                     (f2cl-lib:fref x-%data%
                                    (1 kc)
                                    ((1 inc) (1 *))
                                    x-%offset%)))
          (setf t2
                  (- (f2cl-lib:fref x-%data% (1 k) ((1 inc) (1 *)) x-%offset%)
                     (f2cl-lib:fref x-%data%
                                    (1 kc)
                                    ((1 inc) (1 *))
                                    x-%offset%)))
          (setf dsum
                  (+ dsum
                     (*
                      (f2cl-lib:fref wsave-%data% (kc) ((1 *)) wsave-%offset%)
                      t2)))
          (setf t2
                  (* (f2cl-lib:fref wsave-%data% (k) ((1 *)) wsave-%offset%)
                     t2))
          (setf (f2cl-lib:fref x-%data% (1 k) ((1 inc) (1 *)) x-%offset%)
                  (- t1 t2))
          (setf (f2cl-lib:fref x-%data% (1 kc) ((1 inc) (1 *)) x-%offset%)
                  (+ t1 t2))
         label104))
      (setf modn (mod n 2))
      (if (= modn 0) (go label124))
      (setf (f2cl-lib:fref x-%data%
                           (1 (f2cl-lib:int-add ns2 1))
                           ((1 inc) (1 *))
                           x-%offset%)
              (+
               (f2cl-lib:fref x-%data%
                              (1 (f2cl-lib:int-add ns2 1))
                              ((1 inc) (1 *))
                              x-%offset%)
               (f2cl-lib:fref x-%data%
                              (1 (f2cl-lib:int-add ns2 1))
                              ((1 inc) (1 *))
                              x-%offset%)))
     label124
      (setf lenx
              (f2cl-lib:int-add (f2cl-lib:int-mul inc (f2cl-lib:int-sub nm1 1))
                                1))
      (setf lnsv
              (f2cl-lib:int-add nm1
                                (f2cl-lib:int
                                 (/ (f2cl-lib:flog (f2cl-lib:freal nm1))
                                    (f2cl-lib:flog 2.0d0)))
                                4))
      (setf lnwk nm1)
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
          (rfft1f nm1 inc x lenx
           (f2cl-lib:array-slice wsave-%data%
                                 double-float
                                 ((+ n 1))
                                 ((1 *))
                                 wsave-%offset%)
           lnsv
           (make-array 1 :element-type (type-of work) :initial-element work)
           lnwk ier1)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
        (setf ier1 var-8))
      (cond
        ((/= ier1 0)
         (setf ier 20)
         (xerfft "COSTF1" -5)
         (go label200)))
      (setf snm1 (/ 1.0d0 (f2cl-lib:ffloat nm1)))
      (setf dsum (* snm1 dsum))
      (if (/= (mod nm1 2) 0) (go label30))
      (setf (f2cl-lib:fref x-%data% (1 nm1) ((1 inc) (1 *)) x-%offset%)
              (+ (f2cl-lib:fref x-%data% (1 nm1) ((1 inc) (1 *)) x-%offset%)
                 (f2cl-lib:fref x-%data% (1 nm1) ((1 inc) (1 *)) x-%offset%)))
     label30
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                    ((> i n) nil)
        (tagbody
          (setf xi
                  (* 0.5d0
                     (f2cl-lib:fref x-%data%
                                    (1 i)
                                    ((1 inc) (1 *))
                                    x-%offset%)))
          (setf (f2cl-lib:fref x-%data% (1 i) ((1 inc) (1 *)) x-%offset%)
                  (* 0.5d0
                     (f2cl-lib:fref x-%data%
                                    (1 (f2cl-lib:int-sub i 1))
                                    ((1 inc) (1 *))
                                    x-%offset%)))
          (setf (f2cl-lib:fref x-%data%
                               (1 (f2cl-lib:int-sub i 1))
                               ((1 inc) (1 *))
                               x-%offset%)
                  dsum)
          (setf dsum (+ dsum xi))
         label105))
      (if (/= modn 0) (go label117))
      (setf (f2cl-lib:fref x-%data% (1 n) ((1 inc) (1 *)) x-%offset%) dsum)
     label117
      (setf (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
              (* 0.5d0
                 (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)))
      (setf (f2cl-lib:fref x-%data% (1 n) ((1 inc) (1 *)) x-%offset%)
              (* 0.5d0
                 (f2cl-lib:fref x-%data% (1 n) ((1 inc) (1 *)) x-%offset%)))
     label200
      (go end_label)
     end_label
      (return (values nil nil nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::costf1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (double-float) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::xerfft fortran-to-lisp::rfft1f))))

