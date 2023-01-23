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
;;;           (:float-format double-float))

(in-package "HOMPACK")


(let* ((litfh 4))
  (declare (type (f2cl-lib:integer4 4 4) litfh) (ignorable litfh))
  (defun stepnf
         (n nfe iflag start crash hold h relerr abserr s y yp yold ypold a qr
          alpha tz pivot w wp z0 z1 sspar par ipar)
    (declare (type (array f2cl-lib:integer4 (*)) ipar)
             (type (array double-float (*)) par)
             (type (array double-float (*)) sspar)
             (type (array f2cl-lib:integer4 (*)) pivot)
             (type (array double-float (*)) z1 z0 wp w tz alpha qr a ypold yold
                                            yp y)
             (type (double-float) s abserr relerr h hold)
             (type f2cl-lib:logical crash start)
             (type (f2cl-lib:integer4) iflag nfe n))
    (f2cl-lib:with-multi-array-data
        ((y double-float y-%data% y-%offset%)
         (yp double-float yp-%data% yp-%offset%)
         (yold double-float yold-%data% yold-%offset%)
         (ypold double-float ypold-%data% ypold-%offset%)
         (a double-float a-%data% a-%offset%)
         (qr double-float qr-%data% qr-%offset%)
         (alpha double-float alpha-%data% alpha-%offset%)
         (tz double-float tz-%data% tz-%offset%)
         (w double-float w-%data% w-%offset%)
         (wp double-float wp-%data% wp-%offset%)
         (z0 double-float z0-%data% z0-%offset%)
         (z1 double-float z1-%data% z1-%offset%)
         (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
         (sspar double-float sspar-%data% sspar-%offset%)
         (par double-float par-%data% par-%offset%)
         (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
      (labels ((dd01 (f0 f1 dels)
                 (f2cl-lib:f2cl/ (+ f1 (- f0)) dels))
               (dd001 (f0 fp0 f1 dels)
                 (/ (- (dd01 f0 f1 dels) fp0) dels))
               (dd011 (f0 f1 fp1 dels)
                 (/ (- fp1 (dd01 f0 f1 dels)) dels))
               (dd0011 (f0 fp0 f1 fp1 dels)
                 (/ (- (dd011 f0 f1 fp1 dels) (dd001 f0 fp0 f1 dels)) dels))
               (qofs (f0 fp0 f1 fp1 dels s)
                 (+
                  (*
                   (+
                    (*
                     (+ (* (dd0011 f0 fp0 f1 fp1 dels) (- s dels))
                        (dd001 f0 fp0 f1 dels))
                     s)
                    fp0)
                   s)
                  f0)))
        (declare (ftype (function (double-float double-float double-float)
                         (values double-float &rest t))
                        dd01))
        (declare (ftype (function
                         (double-float double-float double-float double-float)
                         (values double-float &rest t))
                        dd001))
        (declare (ftype (function
                         (double-float double-float double-float double-float)
                         (values double-float &rest t))
                        dd011))
        (declare (ftype (function
                         (double-float double-float double-float double-float
                          double-float)
                         (values double-float &rest t))
                        dd0011))
        (declare (ftype (function
                         (double-float double-float double-float double-float
                          double-float double-float)
                         (values double-float &rest t))
                        qofs))
        (prog ((fail nil) (itnum 0) (j 0) (judy 0) (np1 0) (dcalc 0.0)
               (dels 0.0) (f0 0.0) (f1 0.0) (fouru 0.0) (fp0 0.0) (fp1 0.0)
               (hfail 0.0) (ht 0.0) (lcalc 0.0) (rcalc 0.0) (rholen 0.0)
               (temp 0.0) (twou 0.0))
          (declare (type f2cl-lib:logical fail)
                   (type (f2cl-lib:integer4) itnum j judy np1)
                   (type (double-float) dcalc dels f0 f1 fouru fp0 fp1 hfail ht
                                        lcalc rcalc rholen temp twou))
          (setf twou (* 2.0f0 (f2cl-lib:d1mach 4)))
          (setf fouru (+ twou twou))
          (setf np1 (f2cl-lib:int-add n 1))
          (setf crash f2cl-lib:%true%)
          (if (< s 0.0f0) (go end_label))
          (cond
            ((< h (* fouru (+ 1.0f0 s)))
             (setf h (* fouru (+ 1.0f0 s)))
             (go end_label)))
          (setf temp (dnrm2 np1 y 1))
          (if (>= (* 0.5f0 (+ (* relerr temp) abserr)) (* twou temp))
              (go label40))
          (cond
            ((/= relerr 0.0f0)
             (setf relerr (* fouru (+ 1.0f0 fouru)))
             (setf abserr (max abserr 0.0)))
            (t
             (setf abserr (* fouru temp))))
          (go end_label)
         label40
          (setf crash f2cl-lib:%false%)
          (if (not start) (go label300))
          (setf fail f2cl-lib:%false%)
          (setf start f2cl-lib:%false%)
          (setf h
                  (min h
                       0.1
                       (f2cl-lib:fsqrt
                        (f2cl-lib:fsqrt (+ (* relerr temp) abserr)))))
          (setf (f2cl-lib:fref ypold-%data%
                               (1)
                               ((1 (f2cl-lib:int-add n 1)))
                               ypold-%offset%)
                  (coerce 1.0f0 'double-float))
          (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
              (setf (f2cl-lib:fref ypold-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   ypold-%offset%)
                      (coerce 0.0f0 'double-float))
             label50))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13)
              (tangnf s y yp ypold a qr alpha tz pivot nfe n iflag par ipar)
            (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                             var-10 var-12 var-13))
            (setf s var-0)
            (setf nfe var-9)
            (setf iflag var-11))
          (if (> iflag 0) (go end_label))
         label70
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
              (setf temp
                      (+
                       (f2cl-lib:fref y-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      y-%offset%)
                       (* h
                          (f2cl-lib:fref yp-%data%
                                         (j)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         yp-%offset%))))
              (setf (f2cl-lib:fref w-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   w-%offset%)
                      temp)
              (setf (f2cl-lib:fref z0-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   z0-%offset%)
                      temp)
             label80))
          (f2cl-lib:fdo (judy 1 (f2cl-lib:int-add judy 1))
                        ((> judy litfh) nil)
            (tagbody
              (setf rholen (coerce -1.0f0 'double-float))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13)
                  (tangnf rholen w wp ypold a qr alpha tz pivot nfe n iflag par
                   ipar)
                (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                 var-8 var-10 var-12 var-13))
                (setf rholen var-0)
                (setf nfe var-9)
                (setf iflag var-11))
              (if (> iflag 0) (go end_label))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j np1) nil)
                (tagbody
                  (setf (f2cl-lib:fref w-%data%
                                       (j)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       w-%offset%)
                          (+
                           (f2cl-lib:fref w-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          w-%offset%)
                           (f2cl-lib:fref tz-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          tz-%offset%)))
                 label90))
              (setf itnum judy)
              (cond
                ((= judy 1)
                 (setf lcalc (dnrm2 np1 tz 1))
                 (setf rcalc rholen)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j np1) nil)
                   (tagbody
                     (setf (f2cl-lib:fref z1-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          z1-%offset%)
                             (f2cl-lib:fref w-%data%
                                            (j)
                                            ((1 (f2cl-lib:int-add n 1)))
                                            w-%offset%))
                    label110)))
                ((= judy 2)
                 (setf lcalc (/ (dnrm2 np1 tz 1) lcalc))
                 (setf rcalc (/ rholen rcalc))))
              (if (<= (dnrm2 np1 tz 1) (+ (* relerr (dnrm2 np1 w 1)) abserr))
                  (go label600))
             label200))
          (cond
            ((<= h (* fouru (+ 1.0f0 s)))
             (setf iflag 6)
             (go end_label)))
          (setf h (* 0.5f0 h))
          (go label70)
         label300
          (setf fail f2cl-lib:%false%)
         label320
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
              (setf temp
                      (qofs
                       (f2cl-lib:fref yold-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      yold-%offset%)
                       (f2cl-lib:fref ypold-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      ypold-%offset%)
                       (f2cl-lib:fref y-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      y-%offset%)
                       (f2cl-lib:fref yp-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      yp-%offset%)
                       hold (+ hold h)))
              (setf (f2cl-lib:fref w-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   w-%offset%)
                      temp)
              (setf (f2cl-lib:fref z0-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   z0-%offset%)
                      temp)
             label330))
          (f2cl-lib:fdo (judy 1 (f2cl-lib:int-add judy 1))
                        ((> judy litfh) nil)
            (tagbody
              (setf rholen (coerce -1.0f0 'double-float))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13)
                  (tangnf rholen w wp yp a qr alpha tz pivot nfe n iflag par
                   ipar)
                (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                 var-8 var-10 var-12 var-13))
                (setf rholen var-0)
                (setf nfe var-9)
                (setf iflag var-11))
              (if (> iflag 0) (go end_label))
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j np1) nil)
                (tagbody
                  (setf (f2cl-lib:fref w-%data%
                                       (j)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       w-%offset%)
                          (+
                           (f2cl-lib:fref w-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          w-%offset%)
                           (f2cl-lib:fref tz-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          tz-%offset%)))
                 label420))
              (setf itnum judy)
              (cond
                ((= judy 1)
                 (setf lcalc (dnrm2 np1 tz 1))
                 (setf rcalc rholen)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j np1) nil)
                   (tagbody
                     (setf (f2cl-lib:fref z1-%data%
                                          (j)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          z1-%offset%)
                             (f2cl-lib:fref w-%data%
                                            (j)
                                            ((1 (f2cl-lib:int-add n 1)))
                                            w-%offset%))
                    label440)))
                ((= judy 2)
                 (setf lcalc (/ (dnrm2 np1 tz 1) lcalc))
                 (setf rcalc (/ rholen rcalc))))
              (if (<= (dnrm2 np1 tz 1) (+ (* relerr (dnrm2 np1 w 1)) abserr))
                  (go label600))
             label500))
          (setf fail f2cl-lib:%true%)
          (setf hfail h)
          (cond
            ((<= h (* fouru (+ 1.0f0 s)))
             (setf iflag 6)
             (go end_label)))
          (setf h (* 0.5f0 h))
          (go label320)
         label600
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
              (setf (f2cl-lib:fref yold-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   yold-%offset%)
                      (f2cl-lib:fref y-%data%
                                     (j)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     y-%offset%))
              (setf (f2cl-lib:fref ypold-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   ypold-%offset%)
                      (f2cl-lib:fref yp-%data%
                                     (j)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     yp-%offset%))
              (setf (f2cl-lib:fref y-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   y-%offset%)
                      (f2cl-lib:fref w-%data%
                                     (j)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     w-%offset%))
              (setf (f2cl-lib:fref yp-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   yp-%offset%)
                      (f2cl-lib:fref wp-%data%
                                     (j)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     wp-%offset%))
              (setf (f2cl-lib:fref w-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   w-%offset%)
                      (-
                       (f2cl-lib:fref y-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      y-%offset%)
                       (f2cl-lib:fref yold-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      yold-%offset%)))
             label620))
          (setf hold (dnrm2 np1 w 1))
          (setf s (+ s hold))
         label700
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
              (setf (f2cl-lib:fref tz-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   tz-%offset%)
                      (-
                       (f2cl-lib:fref z0-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      z0-%offset%)
                       (f2cl-lib:fref y-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      y-%offset%)))
              (setf (f2cl-lib:fref w-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   w-%offset%)
                      (-
                       (f2cl-lib:fref z1-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      z1-%offset%)
                       (f2cl-lib:fref y-%data%
                                      (j)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      y-%offset%)))
             label710))
          (setf dcalc (dnrm2 np1 tz 1))
          (if (/= dcalc 0.0f0) (setf dcalc (/ (dnrm2 np1 w 1) dcalc)))
          (if (= itnum 1) (setf lcalc (coerce 0.0f0 'double-float)))
          (cond
            ((= (+ lcalc rcalc dcalc) 0.0f0)
             (setf ht
                     (* (f2cl-lib:fref sspar-%data% (7) ((1 8)) sspar-%offset%)
                        hold)))
            (t
             (setf ht
                     (*
                      (expt
                       (/ 1.0f0
                          (max
                           (/ lcalc
                              (f2cl-lib:fref sspar-%data%
                                             (1)
                                             ((1 8))
                                             sspar-%offset%))
                           (/ rcalc
                              (f2cl-lib:fref sspar-%data%
                                             (2)
                                             ((1 8))
                                             sspar-%offset%))
                           (/ dcalc
                              (f2cl-lib:fref sspar-%data%
                                             (3)
                                             ((1 8))
                                             sspar-%offset%))))
                       (/ 1.0f0
                          (f2cl-lib:fref sspar-%data%
                                         (8)
                                         ((1 8))
                                         sspar-%offset%)))
                      hold))))
          (setf h
                  (min
                   (max ht
                        (*
                         (f2cl-lib:fref sspar-%data%
                                        (6)
                                        ((1 8))
                                        sspar-%offset%)
                         hold)
                        (f2cl-lib:fref sspar-%data%
                                       (4)
                                       ((1 8))
                                       sspar-%offset%))
                   (* (f2cl-lib:fref sspar-%data% (7) ((1 8)) sspar-%offset%)
                      hold)
                   (f2cl-lib:fref sspar-%data% (5) ((1 8)) sspar-%offset%)))
          (cond
            ((= itnum 1)
             (setf h (max h hold)))
            ((= itnum litfh)
             (setf h (min h hold))))
          (if fail (setf h (min h hfail)))
          (go end_label)
         end_label
          (return
           (values nil
                   nfe
                   iflag
                   start
                   crash
                   hold
                   h
                   relerr
                   abserr
                   s
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil
                   nil)))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::stepnf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) fortran-to-lisp::logical
                        fortran-to-lisp::logical (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil fortran-to-lisp::nfe fortran-to-lisp::iflag
                            fortran-to-lisp::start fortran-to-lisp::crash
                            fortran-to-lisp::hold fortran-to-lisp::h
                            fortran-to-lisp::relerr fortran-to-lisp::abserr
                            fortran-to-lisp::s nil nil nil nil nil nil nil nil
                            nil nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::dnrm2 fortran-to-lisp::tangnf
                    fortran-to-lisp::d1mach))))

