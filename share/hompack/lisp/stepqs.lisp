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


(let ((wrge
       (make-array 8
                   :element-type 'single-float
                   :initial-contents '(0.8735115f0 0.1531947f0 0.03191815f0
                                       3.339946f-11 0.4677788f0 6.970123f-4
                                       1.980863f-6 1.122789f-9)))
      (acof
       (make-array 12
                   :element-type 'single-float
                   :initial-contents '(0.9043128f0 -0.7075675f0 -4.667383f0
                                       -3.677482f0 0.8516099f0 -0.1953119f0
                                       -4.830636f0 -0.9770528f0 1.040061f0
                                       0.03793395f0 1.042177f0 0.04450706f0)))
      (failed nil)
      (i 0)
      (itcnt 0)
      (litfh 0)
      (j 0)
      (lk 0)
      (lst 0)
      (np1 0)
      (pcgwk 0)
      (zu 0)
      (alpha 0.0)
      (cordis 0.0)
      (dels 0.0)
      (fouru 0.0)
      (gamma 0.0)
      (hfail 0.0)
      (htemp 0.0)
      (idlerr 0.0)
      (omega 0.0)
      (one 0.0)
      (p0 0.0)
      (p1 0.0)
      (pp0 0.0)
      (pp1 0.0)
      (sigma 0.0)
      (temp 0.0)
      (theta 0.0)
      (twou 0.0)
      (wkold 0.0)
      (xstep 0.0)
      (lambda$ 0.0))
  (declare (type (array single-float (8)) wrge)
           (type (array single-float (12)) acof)
           (type f2cl-lib:logical failed)
           (type (f2cl-lib:integer4) i itcnt litfh j lk lst np1 pcgwk zu)
           (type (double-float) alpha cordis dels fouru gamma hfail htemp
                                idlerr omega one p0 p1 pp0 pp1 sigma temp theta
                                twou wkold xstep lambda$))
  (defun stepqs
         (n nfe iflag lenqr start crash hold h wk relerr abserr s y yp yold
          ypold a qr pivot pp rhovec z0 dz t$ work sspar par ipar)
    (declare (type (array f2cl-lib:integer4 (*)) ipar)
             (type (array double-float (*)) par)
             (type (array double-float (*)) sspar)
             (type (array f2cl-lib:integer4 (*)) pivot)
             (type (array double-float (*)) work t$ dz z0 rhovec pp qr a ypold
                                            yold yp y)
             (type (double-float) s abserr relerr wk h hold)
             (type f2cl-lib:logical crash start)
             (type (f2cl-lib:integer4) lenqr iflag nfe n))
    (f2cl-lib:with-multi-array-data
        ((y double-float y-%data% y-%offset%)
         (yp double-float yp-%data% yp-%offset%)
         (yold double-float yold-%data% yold-%offset%)
         (ypold double-float ypold-%data% ypold-%offset%)
         (a double-float a-%data% a-%offset%)
         (qr double-float qr-%data% qr-%offset%)
         (pp double-float pp-%data% pp-%offset%)
         (rhovec double-float rhovec-%data% rhovec-%offset%)
         (z0 double-float z0-%data% z0-%offset%)
         (dz double-float dz-%data% dz-%offset%)
         (t$ double-float t$-%data% t$-%offset%)
         (work double-float work-%data% work-%offset%)
         (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
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
          (setf litfh 10)
          (setf pcgwk (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 3))
          (setf zu (f2cl-lib:int-add (f2cl-lib:int-mul 3 n) 4))
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
             (setf idlerr (f2cl-lib:fsqrt (f2cl-lib:fsqrt abserr)))
             (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                           ((> j (f2cl-lib:int-add (f2cl-lib:int-mul 2 n) 2))
                            nil)
               (tagbody
                 (setf (f2cl-lib:fref work-%data%
                                      (j)
                                      ((1
                                        (f2cl-lib:int-add
                                         (f2cl-lib:int-mul 8
                                                           (f2cl-lib:int-add n
                                                                             1))
                                         lenqr)))
                                      work-%offset%)
                         (coerce 0.0f0 'double-float))
                label10))
             (multiple-value-bind
                   (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                    var-10 var-11 var-12 var-13 var-14)
                 (tangqs y yp ypold a qr pivot pp rhovec work n lenqr iflag nfe
                  par ipar)
               (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                                var-8 var-9 var-10 var-13 var-14))
               (setf iflag var-11)
               (setf nfe var-12))
             (if (> iflag 0) (go end_label))))
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
          (f2cl-lib:fdo (itcnt 1 (f2cl-lib:int-add itcnt 1))
                        ((> itcnt litfh) nil)
            (tagbody
              (f2cl-lib:fdo (j zu (f2cl-lib:int-add j 1))
                            ((> j
                                (f2cl-lib:int-add zu (f2cl-lib:int-mul 2 n) 1))
                             nil)
                (tagbody
                  (setf (f2cl-lib:fref work-%data%
                                       (j)
                                       ((1
                                         (f2cl-lib:int-add
                                          (f2cl-lib:int-mul 8
                                                            (f2cl-lib:int-add n
                                                                              1))
                                          lenqr)))
                                       work-%offset%)
                          (coerce 0.0f0 'double-float))
                 label40))
              (setf lambda$
                      (f2cl-lib:fref z0-%data%
                                     (np1)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     z0-%offset%))
              (cond
                ((= iflag (f2cl-lib:int-sub 2))
                 (rhojs a lambda$ z0 qr lenqr pivot pp par ipar)
                 (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
                     (rho a lambda$ z0 rhovec par ipar)
                   (declare (ignore var-0 var-2 var-3 var-4 var-5))
                   (setf lambda$ var-1)))
                ((= iflag (f2cl-lib:int-sub 1))
                 (fjacs z0 qr lenqr pivot)
                 (dscal lenqr lambda$ qr 1)
                 (setf sigma (- 1.0f0 lambda$))
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref qr-%data%
                                          ((f2cl-lib:fref pivot
                                                          (j)
                                                          ((1
                                                            (f2cl-lib:int-add n
                                                                              2)))))
                                          ((1 lenqr))
                                          qr-%offset%)
                             (+
                              (f2cl-lib:fref qr-%data%
                                             ((f2cl-lib:fref pivot
                                                             (j)
                                                             ((1
                                                               (f2cl-lib:int-add
                                                                n
                                                                2)))))
                                             ((1 lenqr))
                                             qr-%offset%)
                              sigma))
                    label50))
                 (dcopy n z0 1 rhovec 1)
                 (daxpy n (- one) a 1 rhovec 1)
                 (f z0 pp)
                 (dscal n (- one) pp 1)
                 (daxpy n one rhovec 1 pp 1)
                 (daxpy n (- lambda$) pp 1 rhovec 1))
                (t
                 (fjacs z0 qr lenqr pivot)
                 (dscal lenqr (- lambda$) qr 1)
                 (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                               ((> j n) nil)
                   (tagbody
                     (setf (f2cl-lib:fref qr-%data%
                                          ((f2cl-lib:fref pivot
                                                          (j)
                                                          ((1
                                                            (f2cl-lib:int-add n
                                                                              2)))))
                                          ((1 lenqr))
                                          qr-%offset%)
                             (+
                              (f2cl-lib:fref qr-%data%
                                             ((f2cl-lib:fref pivot
                                                             (j)
                                                             ((1
                                                               (f2cl-lib:int-add
                                                                n
                                                                2)))))
                                             ((1 lenqr))
                                             qr-%offset%)
                              1.0f0))
                    label60))
                 (dcopy n z0 1 rhovec 1)
                 (daxpy n (- one) a 1 rhovec 1)
                 (f z0 pp)
                 (daxpy n (- one) a 1 pp 1)
                 (daxpy n (- lambda$) pp 1 rhovec 1)))
              (setf (f2cl-lib:fref rhovec-%data%
                                   (np1)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   rhovec-%offset%)
                      (coerce 0.0f0 'double-float))
              (setf nfe (f2cl-lib:int-add nfe 1))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9)
                  (pcgqs n qr lenqr pivot pp yp rhovec dz
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (pcgwk)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 8
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            lenqr)))
                                         work-%offset%)
                   iflag)
                (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6
                                 var-7 var-8))
                (setf iflag var-9))
              (if (> iflag 0) (go end_label))
              (daxpy np1 one dz 1 z0 1)
              (setf xstep (dnrm2 np1 dz 1))
              (cond
                ((<= xstep (+ (* relerr (dnrm2 np1 z0 1)) abserr))
                 (go label160)))
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
                 var-10 var-11 var-12 var-13 var-14)
              (tangqs z0 t$ yp a qr pivot pp rhovec work n lenqr iflag nfe par
               ipar)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10 var-13 var-14))
            (setf iflag var-11)
            (setf nfe var-12))
          (if (> iflag 0) (go end_label))
          (setf alpha (ddot np1 t$ 1 yp 1))
          (if (< alpha 0.5f0) (go label150))
          (setf alpha (acos alpha))
          (cond
            (start
             (dcopy np1 y 1
              (f2cl-lib:array-slice work-%data%
                                    double-float
                                    (pcgwk)
                                    ((1
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul 8
                                                         (f2cl-lib:int-add n
                                                                           1))
                                       lenqr)))
                                    work-%offset%)
              1)
             (daxpy np1 h yp 1
              (f2cl-lib:array-slice work-%data%
                                    double-float
                                    (pcgwk)
                                    ((1
                                      (f2cl-lib:int-add
                                       (f2cl-lib:int-mul 8
                                                         (f2cl-lib:int-add n
                                                                           1))
                                       lenqr)))
                                    work-%offset%)
              1))
            (t
             (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                           ((> i np1) nil)
               (tagbody
                 (setf (f2cl-lib:fref work-%data%
                                      ((f2cl-lib:int-sub
                                        (f2cl-lib:int-add pcgwk i)
                                        1))
                                      ((1
                                        (f2cl-lib:int-add
                                         (f2cl-lib:int-mul 8
                                                           (f2cl-lib:int-add n
                                                                             1))
                                         lenqr)))
                                      work-%offset%)
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
                label170))))
          (daxpy np1 (- one) z0 1
           (f2cl-lib:array-slice work-%data%
                                 double-float
                                 (pcgwk)
                                 ((1
                                   (f2cl-lib:int-add
                                    (f2cl-lib:int-mul 8 (f2cl-lib:int-add n 1))
                                    lenqr)))
                                 work-%offset%)
           1)
          (setf cordis
                  (dnrm2 np1
                   (f2cl-lib:array-slice work-%data%
                                         double-float
                                         (pcgwk)
                                         ((1
                                           (f2cl-lib:int-add
                                            (f2cl-lib:int-mul 8
                                                              (f2cl-lib:int-add
                                                               n
                                                               1))
                                            lenqr)))
                                         work-%offset%)
                   1))
          (dcopy np1 y 1 yold 1)
          (dcopy np1 z0 1 y 1)
          (dcopy np1 yp 1 ypold 1)
          (dcopy np1 t$ 1 yp 1)
          (setf htemp hold)
          (daxpy np1 (- one) yold 1 z0 1)
          (setf hold (dnrm2 np1 z0 1))
          (setf s (+ s hold))
          (cond
            ((<= itcnt 1)
             (setf theta (coerce 8.0f0 'double-float)))
            ((= itcnt 4)
             (setf theta (coerce 1.0f0 'double-float)))
            (t
             (setf omega (/ xstep cordis))
             (cond
               ((< itcnt 4)
                (setf lk (f2cl-lib:int-sub (f2cl-lib:int-mul 4 itcnt) 7))
                (cond
                  ((>= omega (f2cl-lib:fref wrge (lk) ((1 8))))
                   (setf theta (coerce 1.0f0 'double-float)))
                  ((>= omega
                       (f2cl-lib:fref wrge ((f2cl-lib:int-add lk 1)) ((1 8))))
                   (setf theta
                           (+ (f2cl-lib:fref acof (lk) ((1 12)))
                              (*
                               (f2cl-lib:fref acof
                                              ((f2cl-lib:int-add lk 1))
                                              ((1 12)))
                               (f2cl-lib:flog omega)))))
                  ((>= omega
                       (f2cl-lib:fref wrge ((f2cl-lib:int-add lk 2)) ((1 8))))
                   (setf theta
                           (+
                            (f2cl-lib:fref acof
                                           ((f2cl-lib:int-add lk 2))
                                           ((1 12)))
                            (*
                             (f2cl-lib:fref acof
                                            ((f2cl-lib:int-add lk 3))
                                            ((1 12)))
                             (f2cl-lib:flog omega)))))
                  (t
                   (setf theta (coerce 8.0f0 'double-float)))))
               ((>= itcnt 7)
                (setf theta (coerce 0.125f0 'double-float)))
               (t
                (setf lk (f2cl-lib:int-sub (f2cl-lib:int-mul 4 itcnt) 16))
                (cond
                  ((> omega (f2cl-lib:fref wrge (lk) ((1 8))))
                   (setf lst (f2cl-lib:int-sub (f2cl-lib:int-mul 2 itcnt) 1))
                   (setf theta
                           (+ (f2cl-lib:fref acof (lst) ((1 12)))
                              (*
                               (f2cl-lib:fref acof
                                              ((f2cl-lib:int-add lst 1))
                                              ((1 12)))
                               (f2cl-lib:flog omega)))))
                  (t
                   (setf theta (coerce 0.125f0 'double-float))))))))
          (setf idlerr (* theta idlerr))
          (setf idlerr (min (* 0.5f0 hold) idlerr))
          (setf wkold wk)
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
                   nil
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
  (setf (gethash 'fortran-to-lisp::stepqs
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        fortran-to-lisp::logical fortran-to-lisp::logical
                        (double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil fortran-to-lisp::nfe fortran-to-lisp::iflag nil
                            fortran-to-lisp::start fortran-to-lisp::crash
                            fortran-to-lisp::hold fortran-to-lisp::h
                            fortran-to-lisp::wk fortran-to-lisp::relerr
                            fortran-to-lisp::abserr fortran-to-lisp::s nil nil
                            nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil)
           :calls '(fortran-to-lisp::f fortran-to-lisp::fjacs
                    fortran-to-lisp::rhojs fortran-to-lisp::ddot
                    fortran-to-lisp::dscal fortran-to-lisp::daxpy
                    fortran-to-lisp::dcopy fortran-to-lisp::dnrm2
                    fortran-to-lisp::pcgqs fortran-to-lisp::rho
                    fortran-to-lisp::tangqs fortran-to-lisp::d1mach))))

