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


(defun msntf1 (lot jump n inc x wsave dsum xh work ier)
  (declare (type (double-float) work)
           (type (array double-float (*)) xh dsum wsave x)
           (type (f2cl-lib:integer4) ier inc n jump lot))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (wsave double-float wsave-%data% wsave-%offset%)
       (dsum double-float dsum-%data% dsum-%offset%)
       (xh double-float xh-%data% xh-%offset%))
    (prog ((i 0) (sfnp1 0.0d0) (ier1 0) (lnwk 0) (lnsv 0) (lnxh 0) (modn 0)
           (t2 0.0d0) (t1 0.0d0) (m1 0) (kc 0) (k 0) (ns2 0) (np1 0)
           (xhold 0.0d0) (m 0) (ssqrt3 0.0d0) (lj 0))
      (declare (type (double-float) ssqrt3 xhold t1 t2 sfnp1)
               (type (f2cl-lib:integer4) lj m np1 ns2 k kc m1 modn lnxh lnsv
                                         lnwk ier1 i))
      (setf ier 0)
      (setf lj
              (f2cl-lib:int-add
               (f2cl-lib:int-mul (f2cl-lib:int-sub lot 1) jump)
               1))
      (f2cl-lib:arithmetic-if (f2cl-lib:int-sub n 2)
                              (go label101)
                              (go label102)
                              (go label103))
     label102
      (setf ssqrt3 (/ 1.0d0 (f2cl-lib:fsqrt 3.0d0)))
      (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m jump))
                    ((> m lj) nil)
        (tagbody
          (setf xhold
                  (* ssqrt3
                     (+
                      (f2cl-lib:fref x-%data% (m 1) ((1 inc) (1 *)) x-%offset%)
                      (f2cl-lib:fref x-%data%
                                     (m 2)
                                     ((1 inc) (1 *))
                                     x-%offset%))))
          (setf (f2cl-lib:fref x-%data% (m 2) ((1 inc) (1 *)) x-%offset%)
                  (* ssqrt3
                     (-
                      (f2cl-lib:fref x-%data% (m 1) ((1 inc) (1 *)) x-%offset%)
                      (f2cl-lib:fref x-%data%
                                     (m 2)
                                     ((1 inc) (1 *))
                                     x-%offset%))))
          (setf (f2cl-lib:fref x-%data% (m 1) ((1 inc) (1 *)) x-%offset%)
                  xhold)
         label112))
     label101
      (go label200)
     label103
      (setf np1 (f2cl-lib:int-add n 1))
      (setf ns2 (the f2cl-lib:integer4 (truncate n 2)))
      (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                    ((> k ns2) nil)
        (tagbody
          (setf kc (f2cl-lib:int-sub np1 k))
          (setf m1 0)
          (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m jump))
                        ((> m lj) nil)
            (tagbody
              (setf m1 (f2cl-lib:int-add m1 1))
              (setf t1
                      (-
                       (f2cl-lib:fref x-%data%
                                      (m k)
                                      ((1 inc) (1 *))
                                      x-%offset%)
                       (f2cl-lib:fref x-%data%
                                      (m kc)
                                      ((1 inc) (1 *))
                                      x-%offset%)))
              (setf t2
                      (*
                       (f2cl-lib:fref wsave-%data% (k) ((1 *)) wsave-%offset%)
                       (+
                        (f2cl-lib:fref x-%data%
                                       (m k)
                                       ((1 inc) (1 *))
                                       x-%offset%)
                        (f2cl-lib:fref x-%data%
                                       (m kc)
                                       ((1 inc) (1 *))
                                       x-%offset%))))
              (setf (f2cl-lib:fref xh-%data%
                                   (m1 (f2cl-lib:int-add k 1))
                                   ((1 lot) (1 *))
                                   xh-%offset%)
                      (+ t1 t2))
              (setf (f2cl-lib:fref xh-%data%
                                   (m1 (f2cl-lib:int-add kc 1))
                                   ((1 lot) (1 *))
                                   xh-%offset%)
                      (- t2 t1))
             label114))
         label104))
      (setf modn (mod n 2))
      (if (= modn 0) (go label124))
      (setf m1 0)
      (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m jump))
                    ((> m lj) nil)
        (tagbody
          (setf m1 (f2cl-lib:int-add m1 1))
          (setf (f2cl-lib:fref xh-%data%
                               (m1 (f2cl-lib:int-add ns2 2))
                               ((1 lot) (1 *))
                               xh-%offset%)
                  (* 4.0d0
                     (f2cl-lib:fref x-%data%
                                    (m (f2cl-lib:int-add ns2 1))
                                    ((1 inc) (1 *))
                                    x-%offset%)))
         label123))
     label124
      (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m 1))
                    ((> m lot) nil)
        (tagbody
          (setf (f2cl-lib:fref xh-%data% (m 1) ((1 lot) (1 *)) xh-%offset%)
                  0.0d0)
         label127))
      (setf lnxh
              (f2cl-lib:int-add (f2cl-lib:int-sub lot 1)
                                (f2cl-lib:int-mul lot (f2cl-lib:int-sub np1 1))
                                1))
      (setf lnsv
              (f2cl-lib:int-add np1
                                (f2cl-lib:int
                                 (/ (f2cl-lib:flog (f2cl-lib:freal np1))
                                    (f2cl-lib:flog 2.0d0)))
                                4))
      (setf lnwk (f2cl-lib:int-mul lot np1))
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
             var-10)
          (rfftmf lot 1 np1 lot xh lnxh
           (f2cl-lib:array-slice wsave-%data%
                                 double-float
                                 ((+ ns2 1))
                                 ((1 *))
                                 wsave-%offset%)
           lnsv
           (make-array 1 :element-type (type-of work) :initial-element work)
           lnwk ier1)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                         var-9))
        (setf ier1 var-10))
      (cond
        ((/= ier1 0)
         (setf ier 20)
         (xerfft "MSNTF1" -5)
         (go label200)))
      (if (/= (mod np1 2) 0) (go label30))
      (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m 1))
                    ((> m lot) nil)
        (tagbody
          (setf (f2cl-lib:fref xh-%data% (m np1) ((1 lot) (1 *)) xh-%offset%)
                  (+
                   (f2cl-lib:fref xh-%data%
                                  (m np1)
                                  ((1 lot) (1 *))
                                  xh-%offset%)
                   (f2cl-lib:fref xh-%data%
                                  (m np1)
                                  ((1 lot) (1 *))
                                  xh-%offset%)))
         label20))
     label30
      (setf sfnp1 (/ 1.0d0 (f2cl-lib:ffloat np1)))
      (setf m1 0)
      (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m jump))
                    ((> m lj) nil)
        (tagbody
          (setf m1 (f2cl-lib:int-add m1 1))
          (setf (f2cl-lib:fref x-%data% (m 1) ((1 inc) (1 *)) x-%offset%)
                  (* 0.5d0
                     (f2cl-lib:fref xh-%data%
                                    (m1 1)
                                    ((1 lot) (1 *))
                                    xh-%offset%)))
          (setf (f2cl-lib:fref dsum-%data% (m1) ((1 *)) dsum-%offset%)
                  (f2cl-lib:fref x-%data% (m 1) ((1 inc) (1 *)) x-%offset%))
         label125))
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                    ((> i n) nil)
        (tagbody
          (setf m1 0)
          (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m jump))
                        ((> m lj) nil)
            (tagbody
              (setf m1 (f2cl-lib:int-add m1 1))
              (setf (f2cl-lib:fref x-%data%
                                   (m (f2cl-lib:int-sub i 1))
                                   ((1 inc) (1 *))
                                   x-%offset%)
                      (* 0.5d0
                         (f2cl-lib:fref xh-%data%
                                        (m1 i)
                                        ((1 lot) (1 *))
                                        xh-%offset%)))
              (setf (f2cl-lib:fref dsum-%data% (m1) ((1 *)) dsum-%offset%)
                      (+ (f2cl-lib:fref dsum-%data% (m1) ((1 *)) dsum-%offset%)
                         (* 0.5d0
                            (f2cl-lib:fref xh-%data%
                                           (m1 (f2cl-lib:int-sub i 1))
                                           ((1 lot) (1 *))
                                           xh-%offset%))))
              (setf (f2cl-lib:fref x-%data% (m i) ((1 inc) (1 *)) x-%offset%)
                      (f2cl-lib:fref dsum-%data% (m1) ((1 *)) dsum-%offset%))
             label115))
         label105))
      (if (/= modn 0) (go label200))
      (setf m1 0)
      (f2cl-lib:fdo (m 1 (f2cl-lib:int-add m jump))
                    ((> m lj) nil)
        (tagbody
          (setf m1 (f2cl-lib:int-add m1 1))
          (setf (f2cl-lib:fref x-%data% (m n) ((1 inc) (1 *)) x-%offset%)
                  (* 0.5d0
                     (f2cl-lib:fref xh-%data%
                                    (m1 (f2cl-lib:int-add n 1))
                                    ((1 lot) (1 *))
                                    xh-%offset%)))
         label116))
     label200
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::msntf1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (double-float) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::xerfft fortran-to-lisp::rfftmf))))

