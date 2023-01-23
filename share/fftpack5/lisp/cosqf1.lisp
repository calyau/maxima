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


(defun cosqf1 (n inc x wsave work ier)
  (declare (type (array double-float (*)) work wsave x)
           (type (f2cl-lib:integer4) ier inc n))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (wsave double-float wsave-%data% wsave-%offset%)
       (work double-float work-%data% work-%offset%))
    (prog ((xim1 0.0d0) (i 0) (ier1 0) (lnwk 0) (lnsv 0) (lenx 0) (modn 0)
           (kc 0) (k 0) (np2 0) (ns2 0))
      (declare (type (f2cl-lib:integer4) ns2 np2 k kc modn lenx lnsv lnwk ier1
                                         i)
               (type (double-float) xim1))
      (setf ier 0)
      (setf ns2 (the f2cl-lib:integer4 (truncate (+ n 1) 2)))
      (setf np2 (f2cl-lib:int-add n 2))
      (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                    ((> k ns2) nil)
        (tagbody
          (setf kc (f2cl-lib:int-sub np2 k))
          (setf (f2cl-lib:fref work-%data% (k) ((1 *)) work-%offset%)
                  (+ (f2cl-lib:fref x-%data% (1 k) ((1 inc) (1 *)) x-%offset%)
                     (f2cl-lib:fref x-%data%
                                    (1 kc)
                                    ((1 inc) (1 *))
                                    x-%offset%)))
          (setf (f2cl-lib:fref work-%data% (kc) ((1 *)) work-%offset%)
                  (- (f2cl-lib:fref x-%data% (1 k) ((1 inc) (1 *)) x-%offset%)
                     (f2cl-lib:fref x-%data%
                                    (1 kc)
                                    ((1 inc) (1 *))
                                    x-%offset%)))
         label101))
      (setf modn (mod n 2))
      (if (/= modn 0) (go label301))
      (setf (f2cl-lib:fref work-%data%
                           ((f2cl-lib:int-add ns2 1))
                           ((1 *))
                           work-%offset%)
              (+
               (f2cl-lib:fref x-%data%
                              (1 (f2cl-lib:int-add ns2 1))
                              ((1 inc) (1 *))
                              x-%offset%)
               (f2cl-lib:fref x-%data%
                              (1 (f2cl-lib:int-add ns2 1))
                              ((1 inc) (1 *))
                              x-%offset%)))
     label301
      (f2cl-lib:fdo (k 2 (f2cl-lib:int-add k 1))
                    ((> k ns2) nil)
        (tagbody
          (setf kc (f2cl-lib:int-sub np2 k))
          (setf (f2cl-lib:fref x-%data% (1 k) ((1 inc) (1 *)) x-%offset%)
                  (+
                   (*
                    (f2cl-lib:fref wsave-%data%
                                   ((f2cl-lib:int-sub k 1))
                                   ((1 *))
                                   wsave-%offset%)
                    (f2cl-lib:fref work-%data% (kc) ((1 *)) work-%offset%))
                   (*
                    (f2cl-lib:fref wsave-%data%
                                   ((f2cl-lib:int-sub kc 1))
                                   ((1 *))
                                   wsave-%offset%)
                    (f2cl-lib:fref work-%data% (k) ((1 *)) work-%offset%))))
          (setf (f2cl-lib:fref x-%data% (1 kc) ((1 inc) (1 *)) x-%offset%)
                  (-
                   (*
                    (f2cl-lib:fref wsave-%data%
                                   ((f2cl-lib:int-sub k 1))
                                   ((1 *))
                                   wsave-%offset%)
                    (f2cl-lib:fref work-%data% (k) ((1 *)) work-%offset%))
                   (*
                    (f2cl-lib:fref wsave-%data%
                                   ((f2cl-lib:int-sub kc 1))
                                   ((1 *))
                                   wsave-%offset%)
                    (f2cl-lib:fref work-%data% (kc) ((1 *)) work-%offset%))))
         label102))
      (if (/= modn 0) (go label303))
      (setf (f2cl-lib:fref x-%data%
                           (1 (f2cl-lib:int-add ns2 1))
                           ((1 inc) (1 *))
                           x-%offset%)
              (* (f2cl-lib:fref wsave-%data% (ns2) ((1 *)) wsave-%offset%)
                 (f2cl-lib:fref work-%data%
                                ((f2cl-lib:int-add ns2 1))
                                ((1 *))
                                work-%offset%)))
     label303
      (setf lenx
              (f2cl-lib:int-add (f2cl-lib:int-mul inc (f2cl-lib:int-sub n 1))
                                1))
      (setf lnsv
              (f2cl-lib:int-add n
                                (f2cl-lib:int
                                 (/ (f2cl-lib:flog (f2cl-lib:freal n))
                                    (f2cl-lib:flog 2.0d0)))
                                4))
      (setf lnwk n)
      (multiple-value-bind
            (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8)
          (rfft1f n inc x lenx
           (f2cl-lib:array-slice wsave-%data%
                                 double-float
                                 ((+ n 1))
                                 ((1 *))
                                 wsave-%offset%)
           lnsv work lnwk ier1)
        (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7))
        (setf ier1 var-8))
      (cond
        ((/= ier1 0)
         (setf ier 20)
         (xerfft "COSQF1" -5)
         (go label400)))
      (f2cl-lib:fdo (i 3 (f2cl-lib:int-add i 2))
                    ((> i n) nil)
        (tagbody
          (setf xim1
                  (* 0.5d0
                     (+
                      (f2cl-lib:fref x-%data%
                                     (1 (f2cl-lib:int-sub i 1))
                                     ((1 inc) (1 *))
                                     x-%offset%)
                      (f2cl-lib:fref x-%data%
                                     (1 i)
                                     ((1 inc) (1 *))
                                     x-%offset%))))
          (setf (f2cl-lib:fref x-%data% (1 i) ((1 inc) (1 *)) x-%offset%)
                  (* 0.5d0
                     (-
                      (f2cl-lib:fref x-%data%
                                     (1 (f2cl-lib:int-sub i 1))
                                     ((1 inc) (1 *))
                                     x-%offset%)
                      (f2cl-lib:fref x-%data%
                                     (1 i)
                                     ((1 inc) (1 *))
                                     x-%offset%))))
          (setf (f2cl-lib:fref x-%data%
                               (1 (f2cl-lib:int-sub i 1))
                               ((1 inc) (1 *))
                               x-%offset%)
                  xim1)
         label103))
     label400
      (go end_label)
     end_label
      (return (values nil nil nil nil nil ier)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::cosqf1
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil fortran-to-lisp::ier)
           :calls '(fortran-to-lisp::xerfft fortran-to-lisp::rfft1f))))

