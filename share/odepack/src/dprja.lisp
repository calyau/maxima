;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2013-11 (20E Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(defun dprja (neq y yh nyh ewt ftem savf wm iwm f jac)
  (declare (type (f2cl-lib:integer4) nyh)
           (type (array double-float (*)) wm savf ftem ewt yh y)
           (type (array f2cl-lib:integer4 (*)) iwm neq))
  (let ()
    (symbol-macrolet ((el0 (aref (dls001-part-0 *dls001-common-block*) 210))
                      (h (aref (dls001-part-0 *dls001-common-block*) 211))
                      (tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (uround (aref (dls001-part-0 *dls001-common-block*) 217))
                      (ierpj (aref (dls001-part-1 *dls001-common-block*) 13))
                      (jcur (aref (dls001-part-1 *dls001-common-block*) 15))
                      (miter (aref (dls001-part-1 *dls001-common-block*) 26))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (nfe (aref (dls001-part-1 *dls001-common-block*) 34))
                      (nje (aref (dls001-part-1 *dls001-common-block*) 35))
                      (pdnorm (aref (dlsa01-part-0 *dlsa01-common-block*) 21)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (iwm f2cl-lib:integer4 iwm-%data% iwm-%offset%)
           (y double-float y-%data% y-%offset%)
           (yh double-float yh-%data% yh-%offset%)
           (ewt double-float ewt-%data% ewt-%offset%)
           (ftem double-float ftem-%data% ftem-%offset%)
           (savf double-float savf-%data% savf-%offset%)
           (wm double-float wm-%data% wm-%offset%))
        (prog ((np1 0) (mu 0) (ml3 0) (ml 0) (meband 0) (meb1 0) (mband 0)
               (mba 0) (lenp 0) (jj 0) (j1 0) (j 0) (ii 0) (ier 0) (i2 0)
               (i1 0) (i 0) (yjj 0.0d0) (yj 0.0d0) (yi 0.0d0) (srur 0.0d0)
               (r0 0.0d0) (r 0.0d0) (hl0 0.0d0) (fac 0.0d0) (con 0.0d0))
          (declare (type (double-float) con fac hl0 r r0 srur yi yj yjj)
                   (type (f2cl-lib:integer4) i i1 i2 ier ii j j1 jj lenp mba
                                             mband meb1 meband ml ml3 mu np1))
          (setf nje (f2cl-lib:int-add nje 1))
          (setf ierpj 0)
          (setf jcur 1)
          (setf hl0 (* h el0))
          (f2cl-lib:computed-goto
           (label100 label200 label300 label400 label500)
           miter)
         label100
          (setf lenp (f2cl-lib:int-mul n n))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i lenp) nil)
            (tagbody
             label110
              (setf (f2cl-lib:fref wm-%data%
                                   ((f2cl-lib:int-add i 2))
                                   ((1 *))
                                   wm-%offset%)
                      0.0d0)))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (funcall jac
                       neq
                       tn
                       y
                       0
                       0
                       (f2cl-lib:array-slice wm-%data%
                                             double-float
                                             (3)
                                             ((1 *))
                                             wm-%offset%)
                       n)
            (declare (ignore var-0 var-2 var-3 var-4 var-5))
            (when var-1
              (setf tn var-1))
            (when var-6
              (setf n var-6)))
          (setf con (- hl0))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i lenp) nil)
            (tagbody
             label120
              (setf (f2cl-lib:fref wm-%data%
                                   ((f2cl-lib:int-add i 2))
                                   ((1 *))
                                   wm-%offset%)
                      (*
                       (f2cl-lib:fref wm-%data%
                                      ((f2cl-lib:int-add i 2))
                                      ((1 *))
                                      wm-%offset%)
                       con))))
          (go label240)
         label200
          (setf fac (dmnorm n savf ewt))
          (setf r0 (* 1000.0d0 (abs h) uround n fac))
          (if (= r0 0.0d0) (setf r0 1.0d0))
          (setf srur (f2cl-lib:fref wm-%data% (1) ((1 *)) wm-%offset%))
          (setf j1 2)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf yj (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%))
              (setf r
                      (max (* srur (abs yj))
                           (/ r0
                              (f2cl-lib:fref ewt-%data%
                                             (j)
                                             ((1 *))
                                             ewt-%offset%))))
              (setf (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%)
                      (+ (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%) r))
              (setf fac (/ (- hl0) r))
              (multiple-value-bind (var-0 var-1 var-2 var-3)
                  (funcall f neq tn y ftem)
                (declare (ignore var-0 var-2 var-3))
                (when var-1
                  (setf tn var-1)))
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                 label220
                  (setf (f2cl-lib:fref wm-%data%
                                       ((f2cl-lib:int-add i j1))
                                       ((1 *))
                                       wm-%offset%)
                          (*
                           (-
                            (f2cl-lib:fref ftem-%data%
                                           (i)
                                           ((1 *))
                                           ftem-%offset%)
                            (f2cl-lib:fref savf-%data%
                                           (i)
                                           ((1 *))
                                           savf-%offset%))
                           fac))))
              (setf (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%) yj)
              (setf j1 (f2cl-lib:int-add j1 n))
             label230))
          (setf nfe (f2cl-lib:int-add nfe n))
         label240
          (setf pdnorm
                  (/
                   (dfnorm n
                    (f2cl-lib:array-slice wm-%data%
                                          double-float
                                          (3)
                                          ((1 *))
                                          wm-%offset%)
                    ewt)
                   (abs hl0)))
          (setf j 3)
          (setf np1 (f2cl-lib:int-add n 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref wm-%data% (j) ((1 *)) wm-%offset%)
                      (+ (f2cl-lib:fref wm-%data% (j) ((1 *)) wm-%offset%)
                         1.0d0))
             label250
              (setf j (f2cl-lib:int-add j np1))))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
              (dgefa
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (3)
                                     ((1 *))
                                     wm-%offset%)
               n n
               (f2cl-lib:array-slice iwm-%data%
                                     f2cl-lib:integer4
                                     (21)
                                     ((1 *))
                                     iwm-%offset%)
               ier)
            (declare (ignore var-0 var-1 var-2 var-3))
            (setf ier var-4))
          (if (/= ier 0) (setf ierpj 1))
          (go end_label)
         label300
          (go end_label)
         label400
          (setf ml (f2cl-lib:fref iwm-%data% (1) ((1 *)) iwm-%offset%))
          (setf mu (f2cl-lib:fref iwm-%data% (2) ((1 *)) iwm-%offset%))
          (setf ml3 (f2cl-lib:int-add ml 3))
          (setf mband (f2cl-lib:int-add ml mu 1))
          (setf meband (f2cl-lib:int-add mband ml))
          (setf lenp (f2cl-lib:int-mul meband n))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i lenp) nil)
            (tagbody
             label410
              (setf (f2cl-lib:fref wm-%data%
                                   ((f2cl-lib:int-add i 2))
                                   ((1 *))
                                   wm-%offset%)
                      0.0d0)))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (funcall jac
                       neq
                       tn
                       y
                       ml
                       mu
                       (f2cl-lib:array-slice wm-%data%
                                             double-float
                                             (ml3)
                                             ((1 *))
                                             wm-%offset%)
                       meband)
            (declare (ignore var-0 var-2 var-5))
            (when var-1
              (setf tn var-1))
            (when var-3
              (setf ml var-3))
            (when var-4
              (setf mu var-4))
            (when var-6
              (setf meband var-6)))
          (setf con (- hl0))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i lenp) nil)
            (tagbody
             label420
              (setf (f2cl-lib:fref wm-%data%
                                   ((f2cl-lib:int-add i 2))
                                   ((1 *))
                                   wm-%offset%)
                      (*
                       (f2cl-lib:fref wm-%data%
                                      ((f2cl-lib:int-add i 2))
                                      ((1 *))
                                      wm-%offset%)
                       con))))
          (go label570)
         label500
          (setf ml (f2cl-lib:fref iwm-%data% (1) ((1 *)) iwm-%offset%))
          (setf mu (f2cl-lib:fref iwm-%data% (2) ((1 *)) iwm-%offset%))
          (setf mband (f2cl-lib:int-add ml mu 1))
          (setf mba
                  (min (the f2cl-lib:integer4 mband)
                       (the f2cl-lib:integer4 n)))
          (setf meband (f2cl-lib:int-add mband ml))
          (setf meb1 (f2cl-lib:int-sub meband 1))
          (setf srur (f2cl-lib:fref wm-%data% (1) ((1 *)) wm-%offset%))
          (setf fac (dmnorm n savf ewt))
          (setf r0 (* 1000.0d0 (abs h) uround n fac))
          (if (= r0 0.0d0) (setf r0 1.0d0))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j mba) nil)
            (tagbody
              (f2cl-lib:fdo (i j (f2cl-lib:int-add i mband))
                            ((> i n) nil)
                (tagbody
                  (setf yi (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%))
                  (setf r
                          (max (* srur (abs yi))
                               (/ r0
                                  (f2cl-lib:fref ewt-%data%
                                                 (i)
                                                 ((1 *))
                                                 ewt-%offset%))))
                 label530
                  (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                          (+ (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                             r))))
              (multiple-value-bind (var-0 var-1 var-2 var-3)
                  (funcall f neq tn y ftem)
                (declare (ignore var-0 var-2 var-3))
                (when var-1
                  (setf tn var-1)))
              (f2cl-lib:fdo (jj j (f2cl-lib:int-add jj mband))
                            ((> jj n) nil)
                (tagbody
                  (setf (f2cl-lib:fref y-%data% (jj) ((1 *)) y-%offset%)
                          (f2cl-lib:fref yh-%data%
                                         (jj 1)
                                         ((1 nyh) (1 *))
                                         yh-%offset%))
                  (setf yjj (f2cl-lib:fref y-%data% (jj) ((1 *)) y-%offset%))
                  (setf r
                          (max (* srur (abs yjj))
                               (/ r0
                                  (f2cl-lib:fref ewt-%data%
                                                 (jj)
                                                 ((1 *))
                                                 ewt-%offset%))))
                  (setf fac (/ (- hl0) r))
                  (setf i1
                          (max (the f2cl-lib:integer4 (f2cl-lib:int-sub jj mu))
                               (the f2cl-lib:integer4 1)))
                  (setf i2
                          (min (the f2cl-lib:integer4 (f2cl-lib:int-add jj ml))
                               (the f2cl-lib:integer4 n)))
                  (setf ii
                          (f2cl-lib:int-add
                           (f2cl-lib:int-sub (f2cl-lib:int-mul jj meb1) ml)
                           2))
                  (f2cl-lib:fdo (i i1 (f2cl-lib:int-add i 1))
                                ((> i i2) nil)
                    (tagbody
                     label540
                      (setf (f2cl-lib:fref wm-%data%
                                           ((f2cl-lib:int-add ii i))
                                           ((1 *))
                                           wm-%offset%)
                              (*
                               (-
                                (f2cl-lib:fref ftem-%data%
                                               (i)
                                               ((1 *))
                                               ftem-%offset%)
                                (f2cl-lib:fref savf-%data%
                                               (i)
                                               ((1 *))
                                               savf-%offset%))
                               fac))))
                 label550))
             label560))
          (setf nfe (f2cl-lib:int-add nfe mba))
         label570
          (setf pdnorm
                  (/
                   (dbnorm n
                    (f2cl-lib:array-slice wm-%data%
                                          double-float
                                          ((+ ml 3))
                                          ((1 *))
                                          wm-%offset%)
                    meband ml mu ewt)
                   (abs hl0)))
          (setf ii (f2cl-lib:int-add mband 2))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf (f2cl-lib:fref wm-%data% (ii) ((1 *)) wm-%offset%)
                      (+ (f2cl-lib:fref wm-%data% (ii) ((1 *)) wm-%offset%)
                         1.0d0))
             label580
              (setf ii (f2cl-lib:int-add ii meband))))
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (dgbfa
               (f2cl-lib:array-slice wm-%data%
                                     double-float
                                     (3)
                                     ((1 *))
                                     wm-%offset%)
               meband n ml mu
               (f2cl-lib:array-slice iwm-%data%
                                     f2cl-lib:integer4
                                     (21)
                                     ((1 *))
                                     iwm-%offset%)
               ier)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))
            (setf ier var-6))
          (if (/= ier 0) (setf ierpj 1))
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil nil nil nil nil nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dprja fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)) t t)
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dgbfa fortran-to-lisp::dbnorm
                    fortran-to-lisp::dgefa fortran-to-lisp::dfnorm
                    fortran-to-lisp::dmnorm))))

