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


(let ((failed nil)
      (i 0)
      (itcnt 0)
      (litfh 0)
      (j 0)
      (jp1 0)
      (np1 0)
      (alpha 0.0)
      (dels 0.0)
      (eta 0.0)
      (fouru 0.0)
      (gamma 0.0)
      (hfail 0.0)
      (htemp 0.0)
      (idlerr 0.0)
      (one 0.0)
      (p0 0.0)
      (p1 0.0)
      (pp0 0.0)
      (pp1 0.0)
      (temp 0.0)
      (twou 0.0)
      (wkold 0.0))
  (declare (type f2cl-lib:logical failed)
           (type (f2cl-lib:integer4) i itcnt litfh j jp1 np1)
           (type (double-float) alpha dels eta fouru gamma hfail htemp idlerr
                                one p0 p1 pp0 pp1 temp twou wkold))
  (defun stepqf
         (n nfe iflag start crash hold h wk relerr abserr s y yp yold ypold a
          qt r f0 f1 z0 dz w t$ sspar par ipar)
    (declare (type (array f2cl-lib:integer4 (*)) ipar)
             (type (array double-float (*)) par)
             (type (array double-float (*)) sspar)
             (type (array double-float (*)) t$ w dz z0 f1 f0 r qt a ypold yold
                                            yp y)
             (type (double-float) s abserr relerr wk h hold)
             (type f2cl-lib:logical crash start)
             (type (f2cl-lib:integer4) iflag nfe n))
    (f2cl-lib:with-multi-array-data
        ((y double-float y-%data% y-%offset%)
         (yp double-float yp-%data% yp-%offset%)
         (yold double-float yold-%data% yold-%offset%)
         (ypold double-float ypold-%data% ypold-%offset%)
         (a double-float a-%data% a-%offset%)
         (qt double-float qt-%data% qt-%offset%)
         (r double-float r-%data% r-%offset%)
         (f0 double-float f0-%data% f0-%offset%)
         (f1 double-float f1-%data% f1-%offset%)
         (z0 double-float z0-%data% z0-%offset%)
         (dz double-float dz-%data% dz-%offset%)
         (w double-float w-%data% w-%offset%)
         (t$ double-float t$-%data% t$-%offset%)
         (sspar double-float sspar-%data% sspar-%offset%)
         (par double-float par-%data% par-%offset%)
         (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
      (labels ((dd01 (p0 p1 dels)
                 (f2cl-lib:f2cl/ (+ p1 (- p0)) dels))
               (dd001 (p0 pp0 p1 dels)
                 (/ (- (dd01 p0 p1 dels) pp0) dels))
               (dd011 (p0 p1 pp1 dels)
                 (/ (- pp1 (dd01 p0 p1 dels)) dels))
               (dd0011 (p0 pp0 p1 pp1 dels)
                 (/ (- (dd011 p0 p1 pp1 dels) (dd001 p0 pp0 p1 dels)) dels))
               (qofs (p0 pp0 p1 pp1 dels s)
                 (+
                  (*
                   (+
                    (*
                     (+ (* (dd0011 p0 pp0 p1 pp1 dels) (- s dels))
                        (dd001 p0 pp0 p1 dels))
                     s)
                    pp0)
                   s)
                  p0)))
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
        (prog ()
          (declare)
          (setf one (coerce 1.0f0 'double-float))
          (setf twou (* 2.0f0 (f2cl-lib:d1mach 4)))
          (setf fouru (+ twou twou))
          (setf np1 (f2cl-lib:int-add n 1))
          (setf failed f2cl-lib:%false%)
          (setf crash f2cl-lib:%true%)
          (setf eta (* 50.0f0 twou))
          (setf litfh
                  (f2cl-lib:int-mul 2
                                    (f2cl-lib:int-add
                                     (f2cl-lib:int
                                      (-
                                       (f2cl-lib:log10
                                        (+ abserr
                                           (* relerr (dnrm2 np1 y 1))))))
                                     1)))
          (if (< s 0.0f0) (go end_label))
          (cond
            ((< h (* fouru (+ 1.0f0 s)))
             (setf h (* fouru (+ 1.0f0 s)))
             (go end_label)))
          (setf temp (+ (dnrm2 np1 y 1) 1.0f0))
          (cond
            ((< (* 0.5f0 (+ (* relerr temp) abserr)) (* twou temp))
             (cond
               ((/= relerr 0.0f0)
                (setf relerr (* fouru (+ 1.0f0 fouru)))
                (setf temp (coerce 0.0f0 'double-float))
                (setf abserr (max abserr temp)))
               (t
                (setf abserr (* fouru temp))))
             (go end_label)))
          (setf crash f2cl-lib:%false%)
          (cond
            (start
             (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                    var-10 var-11 var-12 var-13)
                 (tangqf y yp ypold a qt r w dz t$ n iflag nfe par ipar)
               (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-12 var-13))
               (setf iflag var-10)
               (setf nfe var-11))
             (if (> iflag 0) (go end_label))))
          (cond
            ((= iflag (f2cl-lib:int-sub 2))
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                 (rho a
                  (f2cl-lib:fref y-%data%
                                 (1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 y-%offset%)
                  (f2cl-lib:array-slice y-%data%
                                        double-float
                                        (2)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%)
                  f0 par ipar)
               (declare (ignore var-0 var-2 var-3 var-4 var-5))
               (setf (f2cl-lib:fref y-%data%
                                    (1)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    y-%offset%)
                       var-1)))
            ((= iflag (f2cl-lib:int-sub 1))
             (f
              (f2cl-lib:array-slice y-%data%
                                    double-float
                                    (2)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    y-%offset%)
              f0)
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                 (setf (f2cl-lib:fref f0-%data%
                                      (i)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      f0-%offset%)
                         (+
                          (*
                           (f2cl-lib:fref y-%data%
                                          (1)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          y-%offset%)
                           (f2cl-lib:fref f0-%data%
                                          (i)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          f0-%offset%))
                          (*
                           (- 1.0f0
                              (f2cl-lib:fref y-%data%
                                             (1)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             y-%offset%))
                           (-
                            (f2cl-lib:fref y-%data%
                                           ((f2cl-lib:int-add i 1))
                                           ((1 (f2cl-lib:int-add n 1)))
                                           y-%offset%)
                            (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)))))
                label5)))
            (t
             (f
              (f2cl-lib:array-slice y-%data%
                                    double-float
                                    (2)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    y-%offset%)
              f0)
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                 (setf (f2cl-lib:fref f0-%data%
                                      (i)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      f0-%offset%)
                         (-
                          (+
                           (*
                            (f2cl-lib:fref y-%data%
                                           (1)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           y-%offset%)
                            (- (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)
                               (f2cl-lib:fref f0-%data%
                                              (i)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              f0-%offset%)))
                           (f2cl-lib:fref y-%data%
                                          ((f2cl-lib:int-add i 1))
                                          ((1 (f2cl-lib:int-add n 1)))
                                          y-%offset%))
                          (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)))
                label10))))
          (setf (f2cl-lib:fref f0-%data%
                               (np1)
                               ((1 (f2cl-lib:int-add n 1)))
                               f0-%offset%)
                  (ddot np1 yp 1 y 1))
         label20
          (cond
            (start
             (dcopy np1 y 1 z0 1)
             (daxpy np1 h yp 1 z0 1))
            (t
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i np1) nil)
               (tagbody
                 (setf (f2cl-lib:fref z0-%data%
                                      (i)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      z0-%offset%)
                         (qofs
                          (f2cl-lib:fref yold-%data%
                                         (i)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         yold-%offset%)
                          (f2cl-lib:fref ypold-%data%
                                         (i)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         ypold-%offset%)
                          (f2cl-lib:fref y-%data%
                                         (i)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         y-%offset%)
                          (f2cl-lib:fref yp-%data%
                                         (i)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         yp-%offset%)
                          hold (+ hold h)))
                label30))))
          (cond
            ((= iflag (f2cl-lib:int-sub 2))
             (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                 (rho a
                  (f2cl-lib:fref z0-%data%
                                 (1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 z0-%offset%)
                  (f2cl-lib:array-slice z0-%data%
                                        double-float
                                        (2)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        z0-%offset%)
                  f1 par ipar)
               (declare (ignore var-0 var-2 var-3 var-4 var-5))
               (setf (f2cl-lib:fref z0-%data%
                                    (1)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    z0-%offset%)
                       var-1)))
            ((= iflag (f2cl-lib:int-sub 1))
             (f
              (f2cl-lib:array-slice z0-%data%
                                    double-float
                                    (2)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    z0-%offset%)
              f1)
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                 (setf (f2cl-lib:fref f1-%data%
                                      (i)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      f1-%offset%)
                         (+
                          (*
                           (f2cl-lib:fref z0-%data%
                                          (1)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          z0-%offset%)
                           (f2cl-lib:fref f1-%data%
                                          (i)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          f1-%offset%))
                          (*
                           (- 1.0f0
                              (f2cl-lib:fref z0-%data%
                                             (1)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             z0-%offset%))
                           (-
                            (f2cl-lib:fref z0-%data%
                                           ((f2cl-lib:int-add i 1))
                                           ((1 (f2cl-lib:int-add n 1)))
                                           z0-%offset%)
                            (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)))))
                label40)))
            (t
             (f
              (f2cl-lib:array-slice z0-%data%
                                    double-float
                                    (2)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    z0-%offset%)
              f1)
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i n) nil)
               (tagbody
                 (setf (f2cl-lib:fref f1-%data%
                                      (i)
                                      ((1 (f2cl-lib:int-add n 1)))
                                      f1-%offset%)
                         (-
                          (+
                           (*
                            (f2cl-lib:fref z0-%data%
                                           (1)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           z0-%offset%)
                            (- (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)
                               (f2cl-lib:fref f1-%data%
                                              (i)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              f1-%offset%)))
                           (f2cl-lib:fref z0-%data%
                                          ((f2cl-lib:int-add i 1))
                                          ((1 (f2cl-lib:int-add n 1)))
                                          z0-%offset%))
                          (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)))
                label50))))
          (setf (f2cl-lib:fref f1-%data%
                               (np1)
                               ((1 (f2cl-lib:int-add n 1)))
                               f1-%offset%)
                  (ddot np1 yp 1 z0 1))
          (cond
            (failed
             (cond
               ((= iflag (f2cl-lib:int-sub 2))
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j np1) nil)
                  (tagbody
                    (multiple-value-bind
                          (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                        (rhojac a
                         (f2cl-lib:fref z0-%data%
                                        (1)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        z0-%offset%)
                         (f2cl-lib:array-slice z0-%data%
                                               double-float
                                               (2)
                                               ((1 (f2cl-lib:int-add n 1)))
                                               z0-%offset%)
                         (f2cl-lib:array-slice qt-%data%
                                               double-float
                                               (1 j)
                                               ((1 (f2cl-lib:int-add n 1))
                                                (1 (f2cl-lib:int-add n 1)))
                                               qt-%offset%)
                         j par ipar)
                      (declare (ignore var-0 var-2 var-3 var-4 var-5 var-6))
                      (setf (f2cl-lib:fref z0-%data%
                                           (1)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           z0-%offset%)
                              var-1))
                   label60)))
               ((= iflag (f2cl-lib:int-sub 1))
                (f
                 (f2cl-lib:array-slice z0-%data%
                                       double-float
                                       (2)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       z0-%offset%)
                 (f2cl-lib:array-slice qt-%data%
                                       double-float
                                       (1 1)
                                       ((1 (f2cl-lib:int-add n 1))
                                        (1 (f2cl-lib:int-add n 1)))
                                       qt-%offset%))
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i n) nil)
                  (tagbody
                    (setf (f2cl-lib:fref qt-%data%
                                         (i 1)
                                         ((1 (f2cl-lib:int-add n 1))
                                          (1 (f2cl-lib:int-add n 1)))
                                         qt-%offset%)
                            (+
                             (- (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)
                                (f2cl-lib:fref z0-%data%
                                               ((f2cl-lib:int-add i 1))
                                               ((1 (f2cl-lib:int-add n 1)))
                                               z0-%offset%))
                             (f2cl-lib:fref qt-%data%
                                            (i 1)
                                            ((1 (f2cl-lib:int-add n 1))
                                             (1 (f2cl-lib:int-add n 1)))
                                            qt-%offset%)))
                   label70))
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j n) nil)
                  (tagbody
                    (setf jp1 (f2cl-lib:int-add j 1))
                    (fjac
                     (f2cl-lib:array-slice z0-%data%
                                           double-float
                                           (2)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           z0-%offset%)
                     (f2cl-lib:array-slice qt-%data%
                                           double-float
                                           (1 jp1)
                                           ((1 (f2cl-lib:int-add n 1))
                                            (1 (f2cl-lib:int-add n 1)))
                                           qt-%offset%)
                     j)
                    (dscal n
                     (f2cl-lib:fref z0-%data%
                                    (1)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    z0-%offset%)
                     (f2cl-lib:array-slice qt-%data%
                                           double-float
                                           (1 jp1)
                                           ((1 (f2cl-lib:int-add n 1))
                                            (1 (f2cl-lib:int-add n 1)))
                                           qt-%offset%)
                     1)
                    (setf (f2cl-lib:fref qt-%data%
                                         (j jp1)
                                         ((1 (f2cl-lib:int-add n 1))
                                          (1 (f2cl-lib:int-add n 1)))
                                         qt-%offset%)
                            (+
                             (- 1.0f0
                                (f2cl-lib:fref z0-%data%
                                               (1)
                                               ((1 (f2cl-lib:int-add n 1)))
                                               z0-%offset%))
                             (f2cl-lib:fref qt-%data%
                                            (j jp1)
                                            ((1 (f2cl-lib:int-add n 1))
                                             (1 (f2cl-lib:int-add n 1)))
                                            qt-%offset%)))
                   label80)))
               (t
                (f
                 (f2cl-lib:array-slice z0-%data%
                                       double-float
                                       (2)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       z0-%offset%)
                 (f2cl-lib:array-slice qt-%data%
                                       double-float
                                       (1 1)
                                       ((1 (f2cl-lib:int-add n 1))
                                        (1 (f2cl-lib:int-add n 1)))
                                       qt-%offset%))
                (dscal n (- one)
                 (f2cl-lib:array-slice qt-%data%
                                       double-float
                                       (1 1)
                                       ((1 (f2cl-lib:int-add n 1))
                                        (1 (f2cl-lib:int-add n 1)))
                                       qt-%offset%)
                 1)
                (daxpy n one a 1
                 (f2cl-lib:array-slice qt-%data%
                                       double-float
                                       (1 1)
                                       ((1 (f2cl-lib:int-add n 1))
                                        (1 (f2cl-lib:int-add n 1)))
                                       qt-%offset%)
                 1)
                (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                              ((> j n) nil)
                  (tagbody
                    (setf jp1 (f2cl-lib:int-add j 1))
                    (fjac
                     (f2cl-lib:array-slice z0-%data%
                                           double-float
                                           (2)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           z0-%offset%)
                     (f2cl-lib:array-slice qt-%data%
                                           double-float
                                           (1 jp1)
                                           ((1 (f2cl-lib:int-add n 1))
                                            (1 (f2cl-lib:int-add n 1)))
                                           qt-%offset%)
                     j)
                    (dscal n
                     (-
                      (f2cl-lib:fref z0-%data%
                                     (1)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     z0-%offset%))
                     (f2cl-lib:array-slice qt-%data%
                                           double-float
                                           (1 jp1)
                                           ((1 (f2cl-lib:int-add n 1))
                                            (1 (f2cl-lib:int-add n 1)))
                                           qt-%offset%)
                     1)
                    (setf (f2cl-lib:fref qt-%data%
                                         (j jp1)
                                         ((1 (f2cl-lib:int-add n 1))
                                          (1 (f2cl-lib:int-add n 1)))
                                         qt-%offset%)
                            (+ 1.0f0
                               (f2cl-lib:fref qt-%data%
                                              (j jp1)
                                              ((1 (f2cl-lib:int-add n 1))
                                               (1 (f2cl-lib:int-add n 1)))
                                              qt-%offset%)))
                   label90))))
             (dcopy np1 yp 1
              (f2cl-lib:array-slice qt-%data%
                                    double-float
                                    (np1 1)
                                    ((1 (f2cl-lib:int-add n 1))
                                     (1 (f2cl-lib:int-add n 1)))
                                    qt-%offset%)
              np1)
             (setf nfe (f2cl-lib:int-add nfe 1))
             (multiple-value-bind (var-0 var-1 var-2 var-3)
                 (qrfaqf qt r np1 iflag)
               (declare (ignore var-0 var-1 var-2))
               (setf iflag var-3))
             (if (> iflag 0) (go end_label))
             (dcopy n f1 1 dz 1)
             (dscal n (- one) dz 1)
             (setf (f2cl-lib:fref dz-%data%
                                  (np1)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  dz-%offset%)
                     (coerce 0.0f0 'double-float))
             (qrslqf qt r dz w np1)
             (daxpy np1 one dz 1 z0 1)
             (dcopy np1 f1 1 f0 1)
             (cond
               ((= iflag (f2cl-lib:int-sub 2))
                (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                    (rho a
                     (f2cl-lib:fref z0-%data%
                                    (1)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    z0-%offset%)
                     (f2cl-lib:array-slice z0-%data%
                                           double-float
                                           (2)
                                           ((1 (f2cl-lib:int-add n 1)))
                                           z0-%offset%)
                     f1 par ipar)
                  (declare (ignore var-0 var-2 var-3 var-4 var-5))
                  (setf (f2cl-lib:fref z0-%data%
                                       (1)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       z0-%offset%)
                          var-1)))
               ((= iflag (f2cl-lib:int-sub 1))
                (f
                 (f2cl-lib:array-slice z0-%data%
                                       double-float
                                       (2)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       z0-%offset%)
                 f1)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i n) nil)
                  (tagbody
                    (setf (f2cl-lib:fref f1-%data%
                                         (i)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         f1-%offset%)
                            (+
                             (*
                              (f2cl-lib:fref z0-%data%
                                             (1)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             z0-%offset%)
                              (f2cl-lib:fref f1-%data%
                                             (i)
                                             ((1 (f2cl-lib:int-add n 1)))
                                             f1-%offset%))
                             (*
                              (- 1.0f0
                                 (f2cl-lib:fref z0-%data%
                                                (1)
                                                ((1 (f2cl-lib:int-add n 1)))
                                                z0-%offset%))
                              (-
                               (f2cl-lib:fref z0-%data%
                                              ((f2cl-lib:int-add i 1))
                                              ((1 (f2cl-lib:int-add n 1)))
                                              z0-%offset%)
                               (f2cl-lib:fref a-%data%
                                              (i)
                                              ((1 n))
                                              a-%offset%)))))
                   label100)))
               (t
                (f
                 (f2cl-lib:array-slice z0-%data%
                                       double-float
                                       (2)
                                       ((1 (f2cl-lib:int-add n 1)))
                                       z0-%offset%)
                 f1)
                (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                              ((> i n) nil)
                  (tagbody
                    (setf (f2cl-lib:fref f1-%data%
                                         (i)
                                         ((1 (f2cl-lib:int-add n 1)))
                                         f1-%offset%)
                            (-
                             (+
                              (*
                               (f2cl-lib:fref z0-%data%
                                              (1)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              z0-%offset%)
                               (-
                                (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)
                                (f2cl-lib:fref f1-%data%
                                               (i)
                                               ((1 (f2cl-lib:int-add n 1)))
                                               f1-%offset%)))
                              (f2cl-lib:fref z0-%data%
                                             ((f2cl-lib:int-add i 1))
                                             ((1 (f2cl-lib:int-add n 1)))
                                             z0-%offset%))
                             (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)))
                   label110))))
             (setf (f2cl-lib:fref f1-%data%
                                  (np1)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  f1-%offset%)
                     (ddot np1 yp 1 z0 1)))
            (t
             (dcopy np1 z0 1 dz 1)
             (daxpy np1 (- one) y 1 dz 1)))
          (f2cl-lib:fdo (itcnt 1 (f2cl-lib:int-add itcnt 1))
                        ((> itcnt litfh) nil)
            (tagbody
              (upqrqf np1 eta dz f0 f1 qt r w t$)
              (dcopy n f1 1 dz 1)
              (dscal n (- one) dz 1)
              (setf (f2cl-lib:fref dz-%data%
                                   (np1)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   dz-%offset%)
                      (coerce 0.0f0 'double-float))
              (qrslqf qt r dz w np1)
              (daxpy np1 one dz 1 z0 1)
              (cond
                ((<= (dnrm2 np1 dz 1) (+ (* relerr (dnrm2 np1 z0 1)) abserr))
                 (go label160)))
              (dcopy np1 f1 1 f0 1)
              (cond
                ((= iflag (f2cl-lib:int-sub 2))
                 (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                     (rho a
                      (f2cl-lib:fref z0-%data%
                                     (1)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     z0-%offset%)
                      (f2cl-lib:array-slice z0-%data%
                                            double-float
                                            (2)
                                            ((1 (f2cl-lib:int-add n 1)))
                                            z0-%offset%)
                      f1 par ipar)
                   (declare (ignore var-0 var-2 var-3 var-4 var-5))
                   (setf (f2cl-lib:fref z0-%data%
                                        (1)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        z0-%offset%)
                           var-1)))
                ((= iflag (f2cl-lib:int-sub 1))
                 (f
                  (f2cl-lib:array-slice z0-%data%
                                        double-float
                                        (2)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        z0-%offset%)
                  f1)
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref f1-%data%
                                          (i)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          f1-%offset%)
                             (+
                              (*
                               (f2cl-lib:fref z0-%data%
                                              (1)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              z0-%offset%)
                               (f2cl-lib:fref f1-%data%
                                              (i)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              f1-%offset%))
                              (*
                               (- 1.0f0
                                  (f2cl-lib:fref z0-%data%
                                                 (1)
                                                 ((1 (f2cl-lib:int-add n 1)))
                                                 z0-%offset%))
                               (-
                                (f2cl-lib:fref z0-%data%
                                               ((f2cl-lib:int-add i 1))
                                               ((1 (f2cl-lib:int-add n 1)))
                                               z0-%offset%)
                                (f2cl-lib:fref a-%data%
                                               (i)
                                               ((1 n))
                                               a-%offset%)))))
                    label120)))
                (t
                 (f
                  (f2cl-lib:array-slice z0-%data%
                                        double-float
                                        (2)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        z0-%offset%)
                  f1)
                 (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                               ((> i n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref f1-%data%
                                          (i)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          f1-%offset%)
                             (-
                              (+
                               (*
                                (f2cl-lib:fref z0-%data%
                                               (1)
                                               ((1 (f2cl-lib:int-add n 1)))
                                               z0-%offset%)
                                (-
                                 (f2cl-lib:fref a-%data%
                                                (i)
                                                ((1 n))
                                                a-%offset%)
                                 (f2cl-lib:fref f1-%data%
                                                (i)
                                                ((1 (f2cl-lib:int-add n 1)))
                                                f1-%offset%)))
                               (f2cl-lib:fref z0-%data%
                                              ((f2cl-lib:int-add i 1))
                                              ((1 (f2cl-lib:int-add n 1)))
                                              z0-%offset%))
                              (f2cl-lib:fref a-%data% (i) ((1 n)) a-%offset%)))
                    label130))))
              (setf (f2cl-lib:fref f1-%data%
                                   (np1)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   f1-%offset%)
                      (ddot np1 yp 1 z0 1))
             label140))
         label150
          (setf failed f2cl-lib:%true%)
          (setf hfail h)
          (cond
            ((<= h (* fouru (+ 1.0f0 s)))
             (setf iflag 6)
             (go end_label))
            (t
             (setf h (* 0.5f0 h))))
          (go label20)
         label160
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13)
              (tangqf z0 t$ yp a qt r w dz f1 n iflag nfe par ipar)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-12 var-13))
            (setf iflag var-10)
            (setf nfe var-11))
          (if (> iflag 0) (go end_label))
          (setf alpha (ddot np1 t$ 1 yp 1))
          (if (< alpha 0.5f0) (go label150))
          (setf alpha (acos alpha))
          (dcopy np1 y 1 yold 1)
          (dcopy np1 z0 1 y 1)
          (dcopy np1 yp 1 ypold 1)
          (dcopy np1 t$ 1 yp 1)
          (setf htemp hold)
          (daxpy np1 (- one) yold 1 z0 1)
          (setf hold (dnrm2 np1 z0 1))
          (setf s (+ s hold))
          (setf wkold wk)
          (setf idlerr
                  (f2cl-lib:fsqrt
                   (f2cl-lib:fsqrt (+ abserr (* relerr (dnrm2 np1 y 1))))))
          (setf idlerr (min (* 0.5f0 hold) idlerr))
          (setf wk (/ (* 2.0f0 (abs (sin (* 0.5f0 alpha)))) hold))
          (cond
            (start
             (setf gamma wk))
            (t
             (setf gamma (+ wk (* (/ hold (+ hold htemp)) (- wk wkold))))))
          (setf gamma (max gamma (* 0.01f0 one)))
          (setf h (f2cl-lib:fsqrt (/ (* 2.0f0 idlerr) gamma)))
          (setf h
                  (min
                   (max (f2cl-lib:fref sspar-%data% (1) ((1 4)) sspar-%offset%)
                        (*
                         (f2cl-lib:fref sspar-%data%
                                        (3)
                                        ((1 4))
                                        sspar-%offset%)
                         hold)
                        h)
                   (* (f2cl-lib:fref sspar-%data% (4) ((1 4)) sspar-%offset%)
                      hold)
                   (f2cl-lib:fref sspar-%data% (2) ((1 4)) sspar-%offset%)))
          (if failed (setf h (min hfail h)))
          (setf start f2cl-lib:%false%)
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
                   wk
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
  (setf (gethash 'fortran-to-lisp::stepqf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) fortran-to-lisp::logical
                        fortran-to-lisp::logical (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (double-float) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil fortran-to-lisp::nfe fortran-to-lisp::iflag
                            fortran-to-lisp::start fortran-to-lisp::crash
                            fortran-to-lisp::hold fortran-to-lisp::h
                            fortran-to-lisp::wk fortran-to-lisp::relerr
                            fortran-to-lisp::abserr fortran-to-lisp::s nil nil
                            nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::fjac fortran-to-lisp::f
                    fortran-to-lisp::dscal fortran-to-lisp::daxpy
                    fortran-to-lisp::dcopy fortran-to-lisp::ddot
                    fortran-to-lisp::dnrm2 fortran-to-lisp::upqrqf
                    fortran-to-lisp::qrslqf fortran-to-lisp::qrfaqf
                    fortran-to-lisp::rhojac fortran-to-lisp::rho
                    fortran-to-lisp::tangqf fortran-to-lisp::d1mach))))

