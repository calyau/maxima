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


(let* ((limitd 1000))
  (declare (type (f2cl-lib:integer4 1000 1000) limitd) (ignorable limitd))
  (let ((y1sout 0.0)
        (xold 0.0)
        (sqnp1 0.0)
        (sout 0.0)
        (sb 0.0)
        (sa 0.0)
        (s99 0.0)
        (s 0.0)
        (hold 0.0)
        (h 0.0)
        (epst 0.0)
        (epsstp 0.0)
        (curtol 0.0)
        (cursw 0.0)
        (aold 0.0)
        (np1 0)
        (nfec 0)
        (limit 0)
        (lcode 0)
        (ksteps 0)
        (kold 0)
        (kgi 0)
        (k 0)
        (jw 0)
        (judy 0)
        (j 0)
        (ivc 0)
        (iter 0)
        (iflagc 0)
        (st99 nil)
        (crash nil)
        (start nil)
        (gi (make-array 11 :element-type 'double-float))
        (g (make-array 13 :element-type 'double-float))
        (w (make-array 12 :element-type 'double-float))
        (alphas (make-array 12 :element-type 'double-float))
        (iv (make-array 10 :element-type 'f2cl-lib:integer4)))
    (declare (type (double-float) y1sout xold sqnp1 sout sb sa s99 s hold h
                                  epst epsstp curtol cursw aold)
             (type (f2cl-lib:integer4) np1 nfec limit lcode ksteps kold kgi k
                                       jw judy j ivc iter iflagc)
             (type f2cl-lib:logical st99 crash start)
             (type (array double-float (11)) gi)
             (type (array double-float (13)) g)
             (type (array double-float (12)) w alphas)
             (type (array f2cl-lib:integer4 (10)) iv))
    (defun fixpdf
           (n y iflag arctol eps trace$ a ndima nfe arclen yp ypold qr alpha tz
            pivot wt phi p par ipar)
      (declare (type (array f2cl-lib:integer4 (*)) ipar)
               (type (array double-float (*)) par)
               (type (array f2cl-lib:integer4 (*)) pivot)
               (type (double-float) arclen eps arctol)
               (type (array double-float (*)) p phi wt tz alpha qr ypold yp a
                                              y)
               (type (f2cl-lib:integer4) nfe ndima trace$ iflag n))
      (f2cl-lib:with-multi-array-data
          ((y double-float y-%data% y-%offset%)
           (a double-float a-%data% a-%offset%)
           (yp double-float yp-%data% yp-%offset%)
           (ypold double-float ypold-%data% ypold-%offset%)
           (qr double-float qr-%data% qr-%offset%)
           (alpha double-float alpha-%data% alpha-%offset%)
           (tz double-float tz-%data% tz-%offset%)
           (wt double-float wt-%data% wt-%offset%)
           (phi double-float phi-%data% phi-%offset%)
           (p double-float p-%data% p-%offset%)
           (pivot f2cl-lib:integer4 pivot-%data% pivot-%offset%)
           (par double-float par-%data% par-%offset%)
           (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
        (prog ()
          (declare)
          (if (or (<= n 0) (<= eps 0.0f0)) (setf iflag 7))
          (if (and (>= iflag -2) (<= iflag 0)) (go label10))
          (if (= iflag 2) (go label35))
          (if (= iflag 3) (go label30))
          (setf iflag 7)
          (go end_label)
         label10
          (setf arclen (coerce 0.0f0 'double-float))
          (setf s (coerce 0.0f0 'double-float))
          (if (<= arctol 0.0f0) (setf arctol (* 0.5f0 (f2cl-lib:fsqrt eps))))
          (setf nfec 0)
          (setf iflagc iflag)
          (setf np1 (f2cl-lib:int-add n 1))
          (setf sqnp1 (f2cl-lib:fsqrt (f2cl-lib:dble np1)))
          (setf cursw (coerce 10.0f0 'double-float))
          (setf st99 f2cl-lib:%false%)
          (setf start f2cl-lib:%true%)
          (setf crash f2cl-lib:%false%)
          (setf hold (coerce 1.0f0 'double-float))
          (setf h (coerce 0.1f0 'double-float))
          (setf epsstp arctol)
          (setf ksteps 0)
          (setf (f2cl-lib:fref ypold-%data%
                               (1)
                               ((1 (f2cl-lib:int-add n 1)))
                               ypold-%offset%)
                  (coerce 1.0f0 'double-float))
          (setf (f2cl-lib:fref yp-%data%
                               (1)
                               ((1 (f2cl-lib:int-add n 1)))
                               yp-%offset%)
                  (coerce 1.0f0 'double-float))
          (setf (f2cl-lib:fref y-%data%
                               (1)
                               ((1 (f2cl-lib:int-add n 1)))
                               y-%offset%)
                  (coerce 0.0f0 'double-float))
          (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
              (setf (f2cl-lib:fref ypold-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   ypold-%offset%)
                      (coerce 0.0f0 'double-float))
              (setf (f2cl-lib:fref yp-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   yp-%offset%)
                      (coerce 0.0f0 'double-float))
             label20))
          (cond
            ((>= iflagc (f2cl-lib:int-sub 1))
             (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                           ((> j np1) nil)
               (tagbody
                 (setf (f2cl-lib:fref a-%data%
                                      ((f2cl-lib:int-sub j 1))
                                      ((1 n))
                                      a-%offset%)
                         (f2cl-lib:fref y-%data%
                                        (j)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%))
                label23))))
         label30
          (setf limit limitd)
         label35
          (f2cl-lib:fdo (iter 1 (f2cl-lib:int-add iter 1))
                        ((> iter limit) nil)
            (tagbody
              (cond
                ((< (f2cl-lib:fref y (1) ((1 (f2cl-lib:int-add n 1)))) 0.0f0)
                 (tagbody
                  label40
                   (setf arclen (+ arclen s))
                   (setf iflag 5)
                   (go end_label))))
             label50
              (if (<= s (* 7.0f0 sqnp1)) (go label80))
              (setf arclen (+ arclen s))
              (setf s (coerce 0.0f0 'double-float))
             label60
              (setf start f2cl-lib:%true%)
              (setf crash f2cl-lib:%false%)
              (cond
                ((= iflagc (f2cl-lib:int-sub 2))
                 (f2cl-lib:fdo (jw 1 (f2cl-lib:int-add jw 1))
                               ((> jw ndima) nil)
                   (tagbody
                     (setf (f2cl-lib:fref qr-%data%
                                          (jw 1)
                                          ((1 n) (1 (f2cl-lib:int-add n 1)))
                                          qr-%offset%)
                             (f2cl-lib:fref a-%data% (jw) ((1 n)) a-%offset%))
                    label63))
                 (rhoa a
                  (f2cl-lib:fref y-%data%
                                 (1)
                                 ((1 (f2cl-lib:int-add n 1)))
                                 y-%offset%)
                  (f2cl-lib:array-slice y-%data%
                                        double-float
                                        (2)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%)
                  par ipar)
                 (f2cl-lib:fdo (jw 1 (f2cl-lib:int-add jw 1))
                               ((> jw ndima) nil)
                   (tagbody
                     (setf aold
                             (f2cl-lib:fref qr-%data%
                                            (jw 1)
                                            ((1 n) (1 (f2cl-lib:int-add n 1)))
                                            qr-%offset%))
                     (cond
                       ((> (abs (+ (f2cl-lib:fref a (jw) ((1 n))) (- aold)))
                           (+ 1.0f0 (abs aold)))
                        (setf arclen (+ arclen s))
                        (setf iflag 5)
                        (go end_label)))
                    label65)))
                (t
                 (f
                  (f2cl-lib:array-slice y-%data%
                                        double-float
                                        (2)
                                        ((1 (f2cl-lib:int-add n 1)))
                                        y-%offset%)
                  yp)
                 (f2cl-lib:fdo (jw 1 (f2cl-lib:int-add jw 1))
                               ((> jw n) nil)
                   (tagbody
                     (setf aold
                             (f2cl-lib:fref a-%data% (jw) ((1 n)) a-%offset%))
                     (cond
                       ((= iflagc (f2cl-lib:int-sub 1))
                        (setf (f2cl-lib:fref a-%data% (jw) ((1 n)) a-%offset%)
                                (+
                                 (/
                                  (*
                                   (f2cl-lib:fref y-%data%
                                                  (1)
                                                  ((1 (f2cl-lib:int-add n 1)))
                                                  y-%offset%)
                                   (f2cl-lib:fref yp-%data%
                                                  (jw)
                                                  ((1 (f2cl-lib:int-add n 1)))
                                                  yp-%offset%))
                                  (- 1.0f0
                                     (f2cl-lib:fref y-%data%
                                                    (1)
                                                    ((1
                                                      (f2cl-lib:int-add n 1)))
                                                    y-%offset%)))
                                 (f2cl-lib:fref y-%data%
                                                ((f2cl-lib:int-add jw 1))
                                                ((1 (f2cl-lib:int-add n 1)))
                                                y-%offset%))))
                       (t
                        (setf (f2cl-lib:fref a-%data% (jw) ((1 n)) a-%offset%)
                                (/
                                 (-
                                  (f2cl-lib:fref y-%data%
                                                 ((f2cl-lib:int-add jw 1))
                                                 ((1 (f2cl-lib:int-add n 1)))
                                                 y-%offset%)
                                  (*
                                   (f2cl-lib:fref y-%data%
                                                  (1)
                                                  ((1 (f2cl-lib:int-add n 1)))
                                                  y-%offset%)
                                   (f2cl-lib:fref yp-%data%
                                                  (jw)
                                                  ((1 (f2cl-lib:int-add n 1)))
                                                  yp-%offset%)))
                                 (- 1.0f0
                                    (f2cl-lib:fref y-%data%
                                                   (1)
                                                   ((1 (f2cl-lib:int-add n 1)))
                                                   y-%offset%))))))
                     (cond
                       ((> (abs (+ (f2cl-lib:fref a (jw) ((1 n))) (- aold)))
                           (+ 1.0f0 (abs aold)))
                        (setf arclen (+ arclen s))
                        (setf iflag 5)
                        (go end_label)))
                    label70))))
              (go label100)
             label80
              (if
               (or
                (<=
                 (f2cl-lib:fref y-%data%
                                (1)
                                ((1 (f2cl-lib:int-add n 1)))
                                y-%offset%)
                 0.99f0)
                st99)
               (go label100))
             label90
              (setf st99 f2cl-lib:%true%)
              (setf epsstp eps)
              (setf arctol eps)
              (go label60)
             label100
              (setf curtol (* cursw hold))
              (setf epst (/ eps epsstp))
              (f2cl-lib:fdo (jw 1 (f2cl-lib:int-add jw 1))
                            ((> jw np1) nil)
                (tagbody
                  (cond
                    ((<=
                      (abs
                       (+ (f2cl-lib:fref yp (jw) ((1 (f2cl-lib:int-add n 1))))
                          (-
                           (f2cl-lib:fref ypold
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))))))
                      curtol)
                     (setf (f2cl-lib:fref wt-%data%
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          wt-%offset%)
                             (+
                              (abs
                               (f2cl-lib:fref y-%data%
                                              (jw)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              y-%offset%))
                              1.0f0)))
                    (t
                     (setf (f2cl-lib:fref wt-%data%
                                          (jw)
                                          ((1 (f2cl-lib:int-add n 1)))
                                          wt-%offset%)
                             (*
                              (+
                               (abs
                                (f2cl-lib:fref y-%data%
                                               (jw)
                                               ((1 (f2cl-lib:int-add n 1)))
                                               y-%offset%))
                               1.0f0)
                              epst))))
                 label110))
              (multiple-value-bind
                    (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                     var-9 var-10 var-11 var-12 var-13 var-14 var-15 var-16
                     var-17 var-18 var-19 var-20 var-21 var-22 var-23 var-24
                     var-25 var-26 var-27 var-28 var-29 var-30 var-31 var-32
                     var-33)
                  (steps #'fode np1 y s h epsstp wt start hold k kold crash phi
                   p yp alphas w g ksteps xold ivc iv kgi gi ypold a qr alpha
                   tz pivot nfec iflagc par ipar)
                (declare (ignore var-0 var-1 var-2 var-6 var-12 var-13 var-14
                                 var-15 var-16 var-17 var-21 var-23 var-24
                                 var-25 var-26 var-27 var-28 var-29 var-32
                                 var-33))
                (setf s var-3)
                (setf h var-4)
                (setf epsstp var-5)
                (setf start var-7)
                (setf hold var-8)
                (setf k var-9)
                (setf kold var-10)
                (setf crash var-11)
                (setf ksteps var-18)
                (setf xold var-19)
                (setf ivc var-20)
                (setf kgi var-22)
                (setf nfec var-30)
                (setf iflagc var-31))
              (cond
                ((> trace$ 0)
                 (f2cl-lib:fformat trace$
                                   ("~%" " STEP" 1 (("~5D")) "~3@T" "NFE =" 1
                                    (("~5D")) "~3@T" "ARC LENGTH =" 1
                                    (("~9,4,0,'*,F")) "~3@T" "LAMBDA =" 1
                                    (("~7,4,0,'*,F")) "~5@T" "X vector:" "~%" t
                                    ("~1@T" 6 (("~12,4,2,0,'*,,'EE"))) "~%")
                                   iter
                                   nfec
                                   s
                                   (f2cl-lib:fref y-%data%
                                                  (1)
                                                  ((1 (f2cl-lib:int-add n 1)))
                                                  y-%offset%)
                                   (do ((jw 2 (f2cl-lib:int-add jw 1))
                                        (%ret nil))
                                       ((> jw np1) (nreverse %ret))
                                     (declare (type f2cl-lib:integer4 jw))
                                     (push
                                      (f2cl-lib:fref y-%data%
                                                     (jw)
                                                     ((1
                                                       (f2cl-lib:int-add n 1)))
                                                     y-%offset%)
                                      %ret)))))
              (setf nfe nfec)
              (cond
                ((= iflagc 4)
                 (setf arclen (+ arclen s))
                 (setf iflag 4)
                 (go end_label)))
             label120
              (cond
                (crash
                 (setf iflag 2)
                 (setf eps epsstp)
                 (if (< arctol eps) (setf arctol eps))
                 (setf limit (f2cl-lib:int-sub limit iter))
                 (go end_label)))
             label130
              (cond
                ((>= (f2cl-lib:fref y (1) ((1 (f2cl-lib:int-add n 1)))) 1.0f0)
                 (tagbody
                   (if st99 (go label160))
                   (setf s99 (- s (* 0.5f0 hold)))
                  label135
                   (sintrp s y s99 wt yp np1 kold phi ivc iv kgi gi alphas g w
                    xold p)
                   (if
                    (<
                     (f2cl-lib:fref wt-%data%
                                    (1)
                                    ((1 (f2cl-lib:int-add n 1)))
                                    wt-%offset%)
                     1.0f0)
                    (go label140))
                   (setf s99 (* 0.5f0 (+ (- s hold) s99)))
                   (go label135)
                  label140
                   (f2cl-lib:fdo (judy 1 (f2cl-lib:int-add judy 1))
                                 ((> judy np1) nil)
                     (tagbody
                       (setf (f2cl-lib:fref y-%data%
                                            (judy)
                                            ((1 (f2cl-lib:int-add n 1)))
                                            y-%offset%)
                               (f2cl-lib:fref wt-%data%
                                              (judy)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              wt-%offset%))
                       (setf (f2cl-lib:fref ypold-%data%
                                            (judy)
                                            ((1 (f2cl-lib:int-add n 1)))
                                            ypold-%offset%)
                               (f2cl-lib:fref yp-%data%
                                              (judy)
                                              ((1 (f2cl-lib:int-add n 1)))
                                              yp-%offset%))
                      label144))
                   (setf s s99)
                   (go label90))))
             label150))
          (setf iflag 3)
          (go end_label)
         label160
          (setf sa (- s hold))
          (setf sb s)
          (setf lcode 1)
         label170
          (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
              (root sout y1sout sa sb eps eps lcode)
            (declare (ignore var-1 var-4 var-5))
            (setf sout var-0)
            (setf sa var-2)
            (setf sb var-3)
            (setf lcode var-6))
          (if (> lcode 0) (go label190))
          (sintrp s y sout wt yp np1 kold phi ivc iv kgi gi alphas g w xold p)
          (setf y1sout
                  (-
                   (f2cl-lib:fref wt-%data%
                                  (1)
                                  ((1 (f2cl-lib:int-add n 1)))
                                  wt-%offset%)
                   1.0f0))
          (go label170)
         label190
          (setf iflag 1)
          (if (> lcode 2) (setf iflag 6))
          (setf arclen (+ arclen sa))
          (sintrp s y sa wt yp np1 kold phi ivc iv kgi gi alphas g w xold p)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j np1) nil)
            (tagbody
             label210
              (setf (f2cl-lib:fref y-%data%
                                   (j)
                                   ((1 (f2cl-lib:int-add n 1)))
                                   y-%offset%)
                      (f2cl-lib:fref wt-%data%
                                     (j)
                                     ((1 (f2cl-lib:int-add n 1)))
                                     wt-%offset%))))
          (go end_label)
         end_label
          (return
           (values nil
                   nil
                   iflag
                   arctol
                   eps
                   nil
                   nil
                   nil
                   nfe
                   arclen
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
  (setf (gethash 'fortran-to-lisp::fixpdf
                 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (double-float) (fortran-to-lisp::integer4)
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (double-float)
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil fortran-to-lisp::iflag
                            fortran-to-lisp::arctol fortran-to-lisp::eps nil
                            nil nil fortran-to-lisp::nfe
                            fortran-to-lisp::arclen nil nil nil nil nil nil nil
                            nil nil nil nil)
           :calls '(fortran-to-lisp::f fortran-to-lisp::rhoa
                    fortran-to-lisp::root fortran-to-lisp::sintrp
                    fortran-to-lisp::steps))))

