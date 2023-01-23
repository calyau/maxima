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


(defun sintb1 (n inc x wsave xh work ier)
  (declare (type (double-float) work)
           (type (array double-float (*)) xh wsave x)
           (type (f2cl-lib:integer4) ier inc n))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (wsave double-float wsave-%data% wsave-%offset%)
       (xh double-float xh-%data% xh-%offset%))
    (prog ((dsum 0.0d0) (i 0) (fnp1s4 0.0d0) (ier1 0) (lnwk 0) (lnsv 0)
           (lnxh 0) (modn 0) (t2 0.0d0) (t1 0.0d0) (kc 0) (k 0) (ns2 0) (np1 0)
           (xhold 0.0d0) (srt3s2 0.0d0))
      (declare (type (f2cl-lib:integer4) np1 ns2 k kc modn lnxh lnsv lnwk ier1
                                         i)
               (type (double-float) srt3s2 xhold t1 t2 fnp1s4 dsum))
      (setf ier 0)
      (f2cl-lib:arithmetic-if (f2cl-lib:int-sub n 2)
                              (go label200)
                              (go label102)
                              (go label103))
     label102
      (setf srt3s2 (/ (f2cl-lib:fsqrt 3.0d0) 2.0d0))
      (setf xhold
              (* srt3s2
                 (+ (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
                    (f2cl-lib:fref x-%data%
                                   (1 2)
                                   ((1 inc) (1 *))
                                   x-%offset%))))
      (setf (f2cl-lib:fref x-%data% (1 2) ((1 inc) (1 *)) x-%offset%)
              (* srt3s2
                 (- (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
                    (f2cl-lib:fref x-%data%
                                   (1 2)
                                   ((1 inc) (1 *))
                                   x-%offset%))))
      (setf (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%) xhold)
      (go label200)
     label103
      (setf np1 (f2cl-lib:int-add n 1))
      (setf ns2 (the f2cl-lib:integer4 (truncate n 2)))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k ns2) nil)
        (tagbody
          (setf kc (f2cl-lib:int-sub np1 k))
          (setf t1
                  (- (f2cl-lib:fref x-%data% (1 k) ((1 inc) (1 *)) x-%offset%)
                     (f2cl-lib:fref x-%data%
                                    (1 kc)
                                    ((1 inc) (1 *))
                                    x-%offset%)))
          (setf t2
                  (* (f2cl-lib:fref wsave-%data% (k) ((1 *)) wsave-%offset%)
                     (+
                      (f2cl-lib:fref x-%data% (1 k) ((1 inc) (1 *)) x-%offset%)
                      (f2cl-lib:fref x-%data%
                                     (1 kc)
                                     ((1 inc) (1 *))
                                     x-%offset%))))
          (setf (f2cl-lib:fref xh-%data%
                               ((f2cl-lib:int-add k 1))
                               ((1 *))
                               xh-%offset%)
                  (+ t1 t2))
          (setf (f2cl-lib:fref xh-%data%
                               ((f2cl-lib:int-add kc 1))
                               ((1 *))
                               xh-%offset%)
                  (- t2 t1))
         label104))
      (setf modn (mod n 2))
      (if (= modn 0) (go label124))
      (setf (f2cl-lib:fref xh-%data%
                           ((f2cl-lib:int-add ns2 2))
                           ((1 *))
                           xh-%offset%)
              (* 4.0d0
                 (f2cl-lib:fref x-%data%
                                (1 (f2cl-lib:int-add ns2 1))
                                ((1 inc) (1 *))
                                x-%offset%)))
     label124
      (setf (f2cl-lib:fref xh-%data% (1) ((1 *)) xh-%offset%) 0.0d0)
      (setf lnxh np1)
      (setf lnsv
              (f2cl-lib:int-add np1
                                (f2cl-lib:int
                                 (/ (f2cl-lib:flog (f2cl-lib:freal np1))
                                    (f2cl-lib:flog 2.0d0)))
                                4))
      (setf lnwk np1)
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
          (rfft1f np1 1 xh lnxh
           (f2cl-lib:array-slice wsave-%data%
                                 double-float
                                 ((+ ns2 1))
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
         (xerfft "SINTB1" -5)
         (go label200)))
      (if (/= (mod np1 2) 0) (go label30))
      (setf (f2cl-lib:fref xh-%data% (np1) ((1 *)) xh-%offset%)
              (+ (f2cl-lib:fref xh-%data% (np1) ((1 *)) xh-%offset%)
                 (f2cl-lib:fref xh-%data% (np1) ((1 *)) xh-%offset%)))
     label30
      (setf fnp1s4 (/ (f2cl-lib:ffloat np1) 4.0d0))
      (setf (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%)
              (* fnp1s4 (f2cl-lib:fref xh-%data% (1) ((1 *)) xh-%offset%)))
      (setf dsum (f2cl-lib:fref x-%data% (1 1) ((1 inc) (1 *)) x-%offset%))
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref x-%data%
                               (1 (f2cl-lib:int-sub i 1))
                               ((1 inc) (1 *))
                               x-%offset%)
                  (* fnp1s4 (f2cl-lib:fref xh-%data% (i) ((1 *)) xh-%offset%)))
          (setf dsum
                  (+ dsum
                     (* fnp1s4
                        (f2cl-lib:fref xh-%data%
                                       ((f2cl-lib:int-sub i 1))
                                       ((1 *))
                                       xh-%offset%))))
          (setf (f2cl-lib:fref x-%data% (1 i) ((1 inc) (1 *)) x-%offset%) dsum)
         label105))
      (if (/= modn 0) (go label200))
      (setf (f2cl-lib:fref x-%data% (1 n) ((1 inc) (1 *)) x-%offset%)
              (* fnp1s4
                 (f2cl-lib:fref xh-%data%
                                ((f2cl-lib:int-add n 1))
                                ((1 *))
                                xh-%offset%)))
     label200
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::sintb1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (double-float)
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::xerfft fortran-to-lisp::rfft1f))))

