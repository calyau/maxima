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


(let ((two
       (make-array 13
                   :element-type 'double-float
                   :initial-contents '(2.0 4.0 8.0 16.0 32.0 64.0 128.0 256.0
                                       512.0 1024.0 2048.0 4096.0 8192.0)))
      (gstr
       (make-array 13
                   :element-type 'double-float
                   :initial-contents '(0.5 0.0833 0.0417 0.0264 0.0188 0.0143
                                       0.0114 0.00936 0.00789 0.00679 0.00592
                                       0.00524 0.00468)))
      (psi (make-array 12 :element-type 'double-float))
      (beta (make-array 12 :element-type 'double-float))
      (sig (make-array 13 :element-type 'double-float))
      (v (make-array 12 :element-type 'double-float))
      (phase1 nil)
      (nornd nil)
      (i 0)
      (ifail 0)
      (im1 0)
      (ip1 0)
      (iq 0)
      (j 0)
      (jv 0)
      (km1 0)
      (km2 0)
      (knew 0)
      (kp1 0)
      (kp2 0)
      (kprev 0)
      (l 0)
      (limit1 0)
      (limit2 0)
      (ns 0)
      (nsm2 0)
      (nsp1 0)
      (nsp2 0)
      (absh 0.0)
      (erk 0.0)
      (erkm1 0.0)
      (erkm2 0.0)
      (erkp1 0.0)
      (err 0.0)
      (fouru 0.0)
      (hnew 0.0)
      (p5eps 0.0)
      (r 0.0)
      (reali 0.0)
      (realns 0.0)
      (rho 0.0)
      (round$ 0.0)
      (sum 0.0)
      (tau 0.0)
      (temp1 0.0)
      (temp2 0.0)
      (temp3 0.0)
      (temp4 0.0)
      (temp5 0.0)
      (temp6 0.0)
      (twou 0.0))
  (declare (type f2cl-lib:logical phase1 nornd)
           (type (f2cl-lib:integer4) i ifail im1 ip1 iq j jv km1 km2 knew kp1
                                     kp2 kprev l limit1 limit2 ns nsm2 nsp1
                                     nsp2)
           (type (double-float) absh erk erkm1 erkm2 erkp1 err fouru hnew p5eps
                                r reali realns rho round$ sum tau temp1 temp2
                                temp3 temp4 temp5 temp6 twou)
           (type (array double-float (12)) psi beta v)
           (type (array double-float (13)) two gstr sig))
  (defun steps
         (f neqn y x h eps wt start hold k kold crash phi p yp alpha w g ksteps
          xold ivc iv kgi gi fpwa1 fpwa2 fpwa3 fpwa4 fpwa5 ifpwa1 ifpc1 ifpc2
          par ipar)
    (declare (type (array f2cl-lib:integer4 (*)) ipar)
             (type (array double-float (*)) par)
             (type (array f2cl-lib:integer4 (*)) ifpwa1)
             (type (array double-float (*)) gi)
             (type (array f2cl-lib:integer4 (*)) iv)
             (type (array double-float (*)) g)
             (type (array double-float (*)) w alpha)
             (type f2cl-lib:logical crash start)
             (type (double-float) xold hold eps h x)
             (type (array double-float (*)) fpwa5 fpwa4 fpwa3 fpwa2 fpwa1 yp p
                                            phi wt y)
             (type (f2cl-lib:integer4) ifpc2 ifpc1 kgi ivc ksteps kold k neqn))
    (f2cl-lib:with-multi-array-data
        ((y double-float y-%data% y-%offset%)
         (wt double-float wt-%data% wt-%offset%)
         (phi double-float phi-%data% phi-%offset%)
         (p double-float p-%data% p-%offset%)
         (yp double-float yp-%data% yp-%offset%)
         (fpwa1 double-float fpwa1-%data% fpwa1-%offset%)
         (fpwa2 double-float fpwa2-%data% fpwa2-%offset%)
         (fpwa3 double-float fpwa3-%data% fpwa3-%offset%)
         (fpwa4 double-float fpwa4-%data% fpwa4-%offset%)
         (fpwa5 double-float fpwa5-%data% fpwa5-%offset%)
         (alpha double-float alpha-%data% alpha-%offset%)
         (w double-float w-%data% w-%offset%)
         (g double-float g-%data% g-%offset%)
         (iv f2cl-lib:integer4 iv-%data% iv-%offset%)
         (gi double-float gi-%data% gi-%offset%)
         (ifpwa1 f2cl-lib:integer4 ifpwa1-%data% ifpwa1-%offset%)
         (par double-float par-%data% par-%offset%)
         (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%))
      (prog ()
        (declare)
        (setf twou (* 2.0f0 (f2cl-lib:d1mach 4)))
        (setf fouru (+ twou twou))
        (setf crash f2cl-lib:%true%)
        (if (>= (abs h) (* fouru (abs x))) (go label5))
        (setf h (f2cl-lib:sign (* fouru (abs x)) h))
        (go end_label)
       label5
        (setf p5eps (* 0.5f0 eps))
        (setf round$ (coerce 0.0f0 'double-float))
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
           label10
            (setf round$
                    (+ round$
                       (expt
                        (/ (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%)
                           (f2cl-lib:fref wt-%data%
                                          (l)
                                          ((1 neqn))
                                          wt-%offset%))
                        2)))))
        (setf round$ (* twou (f2cl-lib:fsqrt round$)))
        (if (>= p5eps round$) (go label15))
        (setf eps (* 2.0f0 round$ (+ 1.0f0 fouru)))
        (go end_label)
       label15
        (setf crash f2cl-lib:%false%)
        (setf (f2cl-lib:fref g-%data% (1) ((1 13)) g-%offset%)
                (coerce 1.0f0 'double-float))
        (setf (f2cl-lib:fref g-%data% (2) ((1 13)) g-%offset%)
                (coerce 0.5f0 'double-float))
        (setf (f2cl-lib:fref sig (1) ((1 13))) (coerce 1.0f0 'double-float))
        (if (not start) (go label99))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13)
            (funcall f
                     x
                     y
                     yp
                     fpwa1
                     fpwa2
                     fpwa3
                     fpwa4
                     fpwa5
                     ifpwa1
                     ifpc1
                     (f2cl-lib:int-sub neqn 1)
                     ifpc2
                     par
                     ipar)
          (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                           var-10 var-12 var-13))
          (when var-0
            (setf x var-0))
          (when var-9
            (setf ifpc1 var-9))
          (when var-11
            (setf ifpc2 var-11)))
        (if (> ifpc2 0) (go end_label))
        (setf sum (coerce 0.0f0 'double-float))
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
            (setf (f2cl-lib:fref phi-%data%
                                 (l 1)
                                 ((1 neqn) (1 16))
                                 phi-%offset%)
                    (f2cl-lib:fref yp-%data% (l) ((1 neqn)) yp-%offset%))
            (setf (f2cl-lib:fref phi-%data%
                                 (l 2)
                                 ((1 neqn) (1 16))
                                 phi-%offset%)
                    (coerce 0.0f0 'double-float))
           label20
            (setf sum
                    (+ sum
                       (expt
                        (/ (f2cl-lib:fref yp-%data% (l) ((1 neqn)) yp-%offset%)
                           (f2cl-lib:fref wt-%data%
                                          (l)
                                          ((1 neqn))
                                          wt-%offset%))
                        2)))))
        (setf sum (f2cl-lib:fsqrt sum))
        (setf absh (abs h))
        (if (< eps (* 16.0f0 sum h h))
            (setf absh (* 0.25f0 (f2cl-lib:fsqrt (/ eps sum)))))
        (setf h (f2cl-lib:sign (max absh (* fouru (abs x))) h))
        (setf hold (coerce 0.0f0 'double-float))
        (setf k 1)
        (setf kold 0)
        (setf kprev 0)
        (setf start f2cl-lib:%false%)
        (setf phase1 f2cl-lib:%true%)
        (setf nornd f2cl-lib:%true%)
        (if (> p5eps (* 100.0f0 round$)) (go label99))
        (setf nornd f2cl-lib:%false%)
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
           label25
            (setf (f2cl-lib:fref phi-%data%
                                 (l 15)
                                 ((1 neqn) (1 16))
                                 phi-%offset%)
                    (coerce 0.0f0 'double-float))))
       label99
        (setf ifail 0)
       label100
        (setf kp1 (f2cl-lib:int-add k 1))
        (setf kp2 (f2cl-lib:int-add k 2))
        (setf km1 (f2cl-lib:int-sub k 1))
        (setf km2 (f2cl-lib:int-sub k 2))
        (if (/= h hold) (setf ns 0))
        (if (<= ns kold) (setf ns (f2cl-lib:int-add ns 1)))
        (setf nsp1 (f2cl-lib:int-add ns 1))
        (if (< k ns) (go label199))
        (setf (f2cl-lib:fref beta (ns) ((1 12))) (coerce 1.0f0 'double-float))
        (setf realns (coerce (the f2cl-lib:integer4 ns) 'double-float))
        (setf (f2cl-lib:fref alpha-%data% (ns) ((1 12)) alpha-%offset%)
                (/ 1.0f0 realns))
        (setf temp1 (* h realns))
        (setf (f2cl-lib:fref sig (nsp1) ((1 13))) (coerce 1.0f0 'double-float))
        (if (< k nsp1) (go label110))
        (f2cl-lib:fdo (i nsp1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf im1 (f2cl-lib:int-sub i 1))
            (setf temp2 (f2cl-lib:fref psi (im1) ((1 12))))
            (setf (f2cl-lib:fref psi (im1) ((1 12))) temp1)
            (setf (f2cl-lib:fref beta (i) ((1 12)))
                    (/
                     (* (f2cl-lib:fref beta (im1) ((1 12)))
                        (f2cl-lib:fref psi (im1) ((1 12))))
                     temp2))
            (setf temp1 (+ temp2 h))
            (setf (f2cl-lib:fref alpha-%data% (i) ((1 12)) alpha-%offset%)
                    (/ h temp1))
            (setf reali (coerce (the f2cl-lib:integer4 i) 'double-float))
           label105
            (setf (f2cl-lib:fref sig ((f2cl-lib:int-add i 1)) ((1 13)))
                    (* reali
                       (f2cl-lib:fref alpha-%data% (i) ((1 12)) alpha-%offset%)
                       (f2cl-lib:fref sig (i) ((1 13)))))))
       label110
        (setf (f2cl-lib:fref psi (k) ((1 12))) temp1)
        (if (> ns 1) (go label120))
        (f2cl-lib:fdo (iq 1 (f2cl-lib:int-add iq 1))
                      ((> iq k) nil)
          (tagbody
            (setf temp3
                    (coerce
                     (the f2cl-lib:integer4
                          (f2cl-lib:int-mul iq (f2cl-lib:int-add iq 1)))
                     'double-float))
            (setf (f2cl-lib:fref v (iq) ((1 12))) (/ 1.0f0 temp3))
           label115
            (setf (f2cl-lib:fref w-%data% (iq) ((1 12)) w-%offset%)
                    (f2cl-lib:fref v (iq) ((1 12))))))
        (setf ivc 0)
        (setf kgi 0)
        (if (= k 1) (go label140))
        (setf kgi 1)
        (setf (f2cl-lib:fref gi-%data% (1) ((1 11)) gi-%offset%)
                (f2cl-lib:fref w-%data% (2) ((1 12)) w-%offset%))
        (go label140)
       label120
        (if (<= k kprev) (go label130))
        (if (= ivc 0) (go label122))
        (setf jv
                (f2cl-lib:int-sub kp1
                                  (f2cl-lib:fref iv-%data%
                                                 (ivc)
                                                 ((1 10))
                                                 iv-%offset%)))
        (setf ivc (f2cl-lib:int-sub ivc 1))
        (go label123)
       label122
        (setf jv 1)
        (setf temp4
                (coerce (the f2cl-lib:integer4 (f2cl-lib:int-mul k kp1))
                        'double-float))
        (setf (f2cl-lib:fref v (k) ((1 12))) (/ 1.0f0 temp4))
        (setf (f2cl-lib:fref w-%data% (k) ((1 12)) w-%offset%)
                (f2cl-lib:fref v (k) ((1 12))))
        (if (/= k 2) (go label123))
        (setf kgi 1)
        (setf (f2cl-lib:fref gi-%data% (1) ((1 11)) gi-%offset%)
                (f2cl-lib:fref w-%data% (2) ((1 12)) w-%offset%))
       label123
        (setf nsm2 (f2cl-lib:int-sub ns 2))
        (if (< nsm2 jv) (go label130))
        (f2cl-lib:fdo (j jv (f2cl-lib:int-add j 1))
                      ((> j nsm2) nil)
          (tagbody
            (setf i (f2cl-lib:int-sub k j))
            (setf (f2cl-lib:fref v (i) ((1 12)))
                    (- (f2cl-lib:fref v (i) ((1 12)))
                       (*
                        (f2cl-lib:fref alpha-%data%
                                       ((f2cl-lib:int-add j 1))
                                       ((1 12))
                                       alpha-%offset%)
                        (f2cl-lib:fref v ((f2cl-lib:int-add i 1)) ((1 12))))))
           label125
            (setf (f2cl-lib:fref w-%data% (i) ((1 12)) w-%offset%)
                    (f2cl-lib:fref v (i) ((1 12))))))
        (if (/= i 2) (go label130))
        (setf kgi (f2cl-lib:int-sub ns 1))
        (setf (f2cl-lib:fref gi-%data% (kgi) ((1 11)) gi-%offset%)
                (f2cl-lib:fref w-%data% (2) ((1 12)) w-%offset%))
       label130
        (setf limit1 (f2cl-lib:int-sub kp1 ns))
        (setf temp5 (f2cl-lib:fref alpha-%data% (ns) ((1 12)) alpha-%offset%))
        (f2cl-lib:fdo (iq 1 (f2cl-lib:int-add iq 1))
                      ((> iq limit1) nil)
          (tagbody
            (setf (f2cl-lib:fref v (iq) ((1 12)))
                    (- (f2cl-lib:fref v (iq) ((1 12)))
                       (* temp5
                          (f2cl-lib:fref v
                                         ((f2cl-lib:int-add iq 1))
                                         ((1 12))))))
           label135
            (setf (f2cl-lib:fref w-%data% (iq) ((1 12)) w-%offset%)
                    (f2cl-lib:fref v (iq) ((1 12))))))
        (setf (f2cl-lib:fref g-%data% (nsp1) ((1 13)) g-%offset%)
                (f2cl-lib:fref w-%data% (1) ((1 12)) w-%offset%))
        (if (= limit1 1) (go label137))
        (setf kgi ns)
        (setf (f2cl-lib:fref gi-%data% (kgi) ((1 11)) gi-%offset%)
                (f2cl-lib:fref w-%data% (2) ((1 12)) w-%offset%))
       label137
        (setf (f2cl-lib:fref w-%data%
                             ((f2cl-lib:int-add limit1 1))
                             ((1 12))
                             w-%offset%)
                (f2cl-lib:fref v ((f2cl-lib:int-add limit1 1)) ((1 12))))
        (if (>= k kold) (go label140))
        (setf ivc (f2cl-lib:int-add ivc 1))
        (setf (f2cl-lib:fref iv-%data% (ivc) ((1 10)) iv-%offset%)
                (f2cl-lib:int-add limit1 2))
       label140
        (setf nsp2 (f2cl-lib:int-add ns 2))
        (setf kprev k)
        (if (< kp1 nsp2) (go label199))
        (f2cl-lib:fdo (i nsp2 (f2cl-lib:int-add i 1))
                      ((> i kp1) nil)
          (tagbody
            (setf limit2 (f2cl-lib:int-sub kp2 i))
            (setf temp6
                    (f2cl-lib:fref alpha-%data%
                                   ((f2cl-lib:int-sub i 1))
                                   ((1 12))
                                   alpha-%offset%))
            (f2cl-lib:fdo (iq 1 (f2cl-lib:int-add iq 1))
                          ((> iq limit2) nil)
              (tagbody
               label145
                (setf (f2cl-lib:fref w-%data% (iq) ((1 12)) w-%offset%)
                        (- (f2cl-lib:fref w-%data% (iq) ((1 12)) w-%offset%)
                           (* temp6
                              (f2cl-lib:fref w-%data%
                                             ((f2cl-lib:int-add iq 1))
                                             ((1 12))
                                             w-%offset%))))))
           label150
            (setf (f2cl-lib:fref g-%data% (i) ((1 13)) g-%offset%)
                    (f2cl-lib:fref w-%data% (1) ((1 12)) w-%offset%))))
       label199
        (setf ksteps (f2cl-lib:int-add ksteps 1))
        (if (< k nsp1) (go label215))
        (f2cl-lib:fdo (i nsp1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf temp1 (f2cl-lib:fref beta (i) ((1 12))))
            (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                          ((> l neqn) nil)
              (tagbody
               label205
                (setf (f2cl-lib:fref phi-%data%
                                     (l i)
                                     ((1 neqn) (1 16))
                                     phi-%offset%)
                        (* temp1
                           (f2cl-lib:fref phi-%data%
                                          (l i)
                                          ((1 neqn) (1 16))
                                          phi-%offset%)))))
           label210))
       label215
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
            (setf (f2cl-lib:fref phi-%data%
                                 (l kp2)
                                 ((1 neqn) (1 16))
                                 phi-%offset%)
                    (f2cl-lib:fref phi-%data%
                                   (l kp1)
                                   ((1 neqn) (1 16))
                                   phi-%offset%))
            (setf (f2cl-lib:fref phi-%data%
                                 (l kp1)
                                 ((1 neqn) (1 16))
                                 phi-%offset%)
                    (coerce 0.0f0 'double-float))
           label220
            (setf (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%)
                    (coerce 0.0f0 'double-float))))
        (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                      ((> j k) nil)
          (tagbody
            (setf i (f2cl-lib:int-sub kp1 j))
            (setf ip1 (f2cl-lib:int-add i 1))
            (setf temp2 (f2cl-lib:fref g-%data% (i) ((1 13)) g-%offset%))
            (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                          ((> l neqn) nil)
              (tagbody
                (setf (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%)
                        (+ (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%)
                           (* temp2
                              (f2cl-lib:fref phi-%data%
                                             (l i)
                                             ((1 neqn) (1 16))
                                             phi-%offset%))))
               label225
                (setf (f2cl-lib:fref phi-%data%
                                     (l i)
                                     ((1 neqn) (1 16))
                                     phi-%offset%)
                        (+
                         (f2cl-lib:fref phi-%data%
                                        (l i)
                                        ((1 neqn) (1 16))
                                        phi-%offset%)
                         (f2cl-lib:fref phi-%data%
                                        (l ip1)
                                        ((1 neqn) (1 16))
                                        phi-%offset%)))))
           label230))
        (if nornd (go label240))
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
            (setf tau
                    (- (* h (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%))
                       (f2cl-lib:fref phi-%data%
                                      (l 15)
                                      ((1 neqn) (1 16))
                                      phi-%offset%)))
            (setf (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%)
                    (+ (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%) tau))
           label235
            (setf (f2cl-lib:fref phi-%data%
                                 (l 16)
                                 ((1 neqn) (1 16))
                                 phi-%offset%)
                    (- (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%)
                       (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%)
                       tau))))
        (go label250)
       label240
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
           label245
            (setf (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%)
                    (+ (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%)
                       (* h
                          (f2cl-lib:fref p-%data%
                                         (l)
                                         ((1 neqn))
                                         p-%offset%))))))
       label250
        (setf xold x)
        (setf x (+ x h))
        (setf absh (abs h))
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13)
            (funcall f
                     x
                     p
                     yp
                     fpwa1
                     fpwa2
                     fpwa3
                     fpwa4
                     fpwa5
                     ifpwa1
                     ifpc1
                     (f2cl-lib:int-sub neqn 1)
                     ifpc2
                     par
                     ipar)
          (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                           var-10 var-12 var-13))
          (when var-0
            (setf x var-0))
          (when var-9
            (setf ifpc1 var-9))
          (when var-11
            (setf ifpc2 var-11)))
        (if (> ifpc2 0) (go end_label))
        (setf erkm2 (coerce 0.0f0 'double-float))
        (setf erkm1 (coerce 0.0f0 'double-float))
        (setf erk (coerce 0.0f0 'double-float))
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
            (setf temp3
                    (/ 1.0f0
                       (f2cl-lib:fref wt-%data% (l) ((1 neqn)) wt-%offset%)))
            (setf temp4
                    (- (f2cl-lib:fref yp-%data% (l) ((1 neqn)) yp-%offset%)
                       (f2cl-lib:fref phi-%data%
                                      (l 1)
                                      ((1 neqn) (1 16))
                                      phi-%offset%)))
            (f2cl-lib:arithmetic-if km2
                                    (go label265)
                                    (go label260)
                                    (go label255))
           label255
            (setf erkm2
                    (+ erkm2
                       (expt
                        (*
                         (+
                          (f2cl-lib:fref phi-%data%
                                         (l km1)
                                         ((1 neqn) (1 16))
                                         phi-%offset%)
                          temp4)
                         temp3)
                        2)))
           label260
            (setf erkm1
                    (+ erkm1
                       (expt
                        (*
                         (+
                          (f2cl-lib:fref phi-%data%
                                         (l k)
                                         ((1 neqn) (1 16))
                                         phi-%offset%)
                          temp4)
                         temp3)
                        2)))
           label265
            (setf erk (+ erk (expt (* temp4 temp3) 2)))))
        (f2cl-lib:arithmetic-if km2 (go label280) (go label275) (go label270))
       label270
        (setf erkm2
                (* absh
                   (f2cl-lib:fref sig (km1) ((1 13)))
                   (f2cl-lib:fref gstr (km2) ((1 13)))
                   (f2cl-lib:fsqrt erkm2)))
       label275
        (setf erkm1
                (* absh
                   (f2cl-lib:fref sig (k) ((1 13)))
                   (f2cl-lib:fref gstr (km1) ((1 13)))
                   (f2cl-lib:fsqrt erkm1)))
       label280
        (setf temp5 (* absh (f2cl-lib:fsqrt erk)))
        (setf err
                (* temp5
                   (- (f2cl-lib:fref g-%data% (k) ((1 13)) g-%offset%)
                      (f2cl-lib:fref g-%data% (kp1) ((1 13)) g-%offset%))))
        (setf erk
                (* temp5
                   (f2cl-lib:fref sig (kp1) ((1 13)))
                   (f2cl-lib:fref gstr (k) ((1 13)))))
        (setf knew k)
        (f2cl-lib:arithmetic-if km2 (go label299) (go label290) (go label285))
       label285
        (if (<= (max erkm1 erkm2) erk) (setf knew km1))
        (go label299)
       label290
        (if (<= erkm1 (* 0.5f0 erk)) (setf knew km1))
       label299
        (if (<= err eps) (go label400))
        (setf phase1 f2cl-lib:%false%)
        (setf x xold)
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (setf temp1 (/ 1.0f0 (f2cl-lib:fref beta (i) ((1 12)))))
            (setf ip1 (f2cl-lib:int-add i 1))
            (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                          ((> l neqn) nil)
              (tagbody
               label305
                (setf (f2cl-lib:fref phi-%data%
                                     (l i)
                                     ((1 neqn) (1 16))
                                     phi-%offset%)
                        (* temp1
                           (-
                            (f2cl-lib:fref phi-%data%
                                           (l i)
                                           ((1 neqn) (1 16))
                                           phi-%offset%)
                            (f2cl-lib:fref phi-%data%
                                           (l ip1)
                                           ((1 neqn) (1 16))
                                           phi-%offset%))))))
           label310))
        (if (< k 2) (go label320))
        (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
           label315
            (setf (f2cl-lib:fref psi ((f2cl-lib:int-sub i 1)) ((1 12)))
                    (- (f2cl-lib:fref psi (i) ((1 12))) h))))
       label320
        (setf ifail (f2cl-lib:int-add ifail 1))
        (setf temp2 (coerce 0.5f0 'double-float))
        (f2cl-lib:arithmetic-if (f2cl-lib:int-sub ifail 3)
                                (go label335)
                                (go label330)
                                (go label325))
       label325
        (if (< p5eps (* 0.25f0 erk))
            (setf temp2 (f2cl-lib:fsqrt (/ p5eps erk))))
       label330
        (setf knew 1)
       label335
        (setf h (* temp2 h))
        (setf k knew)
        (setf ns 0)
        (if (>= (abs h) (* fouru (abs x))) (go label340))
        (setf crash f2cl-lib:%true%)
        (setf h (f2cl-lib:sign (* fouru (abs x)) h))
        (setf eps (+ eps eps))
        (go end_label)
       label340
        (go label100)
       label400
        (setf kold k)
        (setf hold h)
        (setf temp1 (* h (f2cl-lib:fref g-%data% (kp1) ((1 13)) g-%offset%)))
        (if nornd (go label410))
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
            (setf temp3 (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%))
            (setf rho
                    (-
                     (* temp1
                        (- (f2cl-lib:fref yp-%data% (l) ((1 neqn)) yp-%offset%)
                           (f2cl-lib:fref phi-%data%
                                          (l 1)
                                          ((1 neqn) (1 16))
                                          phi-%offset%)))
                     (f2cl-lib:fref phi-%data%
                                    (l 16)
                                    ((1 neqn) (1 16))
                                    phi-%offset%)))
            (setf (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%)
                    (+ (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%) rho))
            (setf (f2cl-lib:fref phi-%data%
                                 (l 15)
                                 ((1 neqn) (1 16))
                                 phi-%offset%)
                    (- (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%)
                       (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%)
                       rho))
           label405
            (setf (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%) temp3)))
        (go label420)
       label410
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
            (setf temp3 (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%))
            (setf (f2cl-lib:fref y-%data% (l) ((1 neqn)) y-%offset%)
                    (+ (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%)
                       (* temp1
                          (-
                           (f2cl-lib:fref yp-%data% (l) ((1 neqn)) yp-%offset%)
                           (f2cl-lib:fref phi-%data%
                                          (l 1)
                                          ((1 neqn) (1 16))
                                          phi-%offset%)))))
           label415
            (setf (f2cl-lib:fref p-%data% (l) ((1 neqn)) p-%offset%) temp3)))
       label420
        (multiple-value-bind
              (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
               var-10 var-11 var-12 var-13)
            (funcall f
                     x
                     y
                     yp
                     fpwa1
                     fpwa2
                     fpwa3
                     fpwa4
                     fpwa5
                     ifpwa1
                     ifpc1
                     (f2cl-lib:int-sub neqn 1)
                     ifpc2
                     par
                     ipar)
          (declare (ignore var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8
                           var-10 var-12 var-13))
          (when var-0
            (setf x var-0))
          (when var-9
            (setf ifpc1 var-9))
          (when var-11
            (setf ifpc2 var-11)))
        (if (> ifpc2 0) (go end_label))
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
            (setf (f2cl-lib:fref phi-%data%
                                 (l kp1)
                                 ((1 neqn) (1 16))
                                 phi-%offset%)
                    (- (f2cl-lib:fref yp-%data% (l) ((1 neqn)) yp-%offset%)
                       (f2cl-lib:fref phi-%data%
                                      (l 1)
                                      ((1 neqn) (1 16))
                                      phi-%offset%)))
           label425
            (setf (f2cl-lib:fref phi-%data%
                                 (l kp2)
                                 ((1 neqn) (1 16))
                                 phi-%offset%)
                    (-
                     (f2cl-lib:fref phi-%data%
                                    (l kp1)
                                    ((1 neqn) (1 16))
                                    phi-%offset%)
                     (f2cl-lib:fref phi-%data%
                                    (l kp2)
                                    ((1 neqn) (1 16))
                                    phi-%offset%)))))
        (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                      ((> i k) nil)
          (tagbody
            (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                          ((> l neqn) nil)
              (tagbody
               label430
                (setf (f2cl-lib:fref phi-%data%
                                     (l i)
                                     ((1 neqn) (1 16))
                                     phi-%offset%)
                        (+
                         (f2cl-lib:fref phi-%data%
                                        (l i)
                                        ((1 neqn) (1 16))
                                        phi-%offset%)
                         (f2cl-lib:fref phi-%data%
                                        (l kp1)
                                        ((1 neqn) (1 16))
                                        phi-%offset%)))))
           label435))
        (setf erkp1 (coerce 0.0f0 'double-float))
        (if (or (= knew km1) (= k 12)) (setf phase1 f2cl-lib:%false%))
        (if phase1 (go label450))
        (if (= knew km1) (go label455))
        (if (> kp1 ns) (go label460))
        (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                      ((> l neqn) nil)
          (tagbody
           label440
            (setf erkp1
                    (+ erkp1
                       (expt
                        (/
                         (f2cl-lib:fref phi-%data%
                                        (l kp2)
                                        ((1 neqn) (1 16))
                                        phi-%offset%)
                         (f2cl-lib:fref wt-%data% (l) ((1 neqn)) wt-%offset%))
                        2)))))
        (setf erkp1
                (* absh
                   (f2cl-lib:fref gstr (kp1) ((1 13)))
                   (f2cl-lib:fsqrt erkp1)))
        (if (> k 1) (go label445))
        (if (>= erkp1 (* 0.5f0 erk)) (go label460))
        (go label450)
       label445
        (if (<= erkm1 (min erk erkp1)) (go label455))
        (if (or (>= erkp1 erk) (= k 12)) (go label460))
       label450
        (setf k kp1)
        (setf erk erkp1)
        (go label460)
       label455
        (setf k km1)
        (setf erk erkm1)
       label460
        (setf hnew (+ h h))
        (if phase1 (go label465))
        (if
         (>= p5eps
             (* erk (f2cl-lib:fref two ((f2cl-lib:int-add k 1)) ((1 13)))))
         (go label465))
        (setf hnew h)
        (if (>= p5eps erk) (go label465))
        (setf temp2
                (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add k 1))
                        'double-float))
        (setf r (expt (/ p5eps erk) (/ 1.0f0 temp2)))
        (setf hnew (* absh (max 0.5 (min 0.9 r))))
        (setf hnew (f2cl-lib:sign (max hnew (* fouru (abs x))) h))
       label465
        (setf h hnew)
        (go end_label)
       end_label
        (return
         (values nil
                 nil
                 nil
                 x
                 h
                 eps
                 nil
                 start
                 hold
                 k
                 kold
                 crash
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 ksteps
                 xold
                 ivc
                 nil
                 kgi
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 nil
                 ifpc1
                 ifpc2
                 nil
                 nil))))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::steps fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '(t (fortran-to-lisp::integer4) (array double-float (*))
                        (double-float) (double-float) (double-float)
                        (array double-float (*)) fortran-to-lisp::logical
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) fortran-to-lisp::logical
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (fortran-to-lisp::integer4) (double-float)
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*)))
           :return-values '(nil nil nil fortran-to-lisp::x fortran-to-lisp::h
                            fortran-to-lisp::eps nil fortran-to-lisp::start
                            fortran-to-lisp::hold fortran-to-lisp::k
                            fortran-to-lisp::kold fortran-to-lisp::crash nil
                            nil nil nil nil nil fortran-to-lisp::ksteps
                            fortran-to-lisp::xold fortran-to-lisp::ivc nil
                            fortran-to-lisp::kgi nil nil nil nil nil nil nil
                            fortran-to-lisp::ifpc1 fortran-to-lisp::ifpc2 nil
                            nil)
           :calls '(fortran-to-lisp::d1mach))))

