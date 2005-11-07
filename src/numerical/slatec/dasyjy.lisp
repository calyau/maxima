;;; Compiled by f2cl version 2.0 beta Date: 2005/07/26 12:37:15 
;;; Using Lisp CMU Common Lisp Snapshot 2005-11 (19C)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((tols -6.90775527898214)
      (con1 0.666666666666667)
      (con2 0.333333333333333)
      (con548 0.104166666666667)
      (ar
       (make-array 8
                   :element-type 'double-float
                   :initial-contents '(0.0835503472222222 0.128226574556327
                                       0.29184902646414 0.881627267443758
                                       3.32140828186277 14.9957629868626
                                       78.9230130115865 474.451538868264)))
      (br
       (make-array 10
                   :element-type 'double-float
                   :initial-contents '(-0.145833333333333 -0.0987413194444444
                                       -0.143312053915895 -0.317227202678414
                                       -0.94242914795712 -3.51120304082635
                                       -15.727263620368 -82.2814390971859
                                       -492.355370523671 -3316.21856854797)))
      (c
       (make-array 65
                   :element-type 'double-float
                   :initial-contents '(-0.208333333333333 0.125
                                       0.334201388888889 -0.401041666666667
                                       0.0703125 -1.02581259645062
                                       1.84646267361111 -0.8912109375
                                       0.0732421875 4.66958442342625
                                       -11.207002616223 8.78912353515625
                                       -2.3640869140625 0.112152099609375
                                       -28.2120725582002 84.6362176746007
                                       -91.81824154324 42.5349987453885
                                       -7.36879435947963 0.227108001708984
                                       212.570130039217 -765.252468141182
                                       1059.990452528 -699.579627376133
                                       218.190511744212 -26.4914304869516
                                       0.572501420974731 -1919.45766231841
                                       8061.72218173731 -13586.5500064341
                                       11655.3933368645 -5305.6469786134
                                       1200.90291321635 -108.090919788395
                                       1.72772750258446 20204.2913309661
                                       -96980.5983886375 192547.001232532
                                       -203400.177280416 122200.464983017
                                       -41192.6549688976 7109.51430248936
                                       -493.915304773088 6.07404200127348
                                       -242919.187900551 1311763.61466298
                                       -2998015.91853811 3763271.2976564
                                       -2813563.22658653 1268365.27332162
                                       -331645.172484564 45218.7689813627
                                       -2499.83048181121 24.3805296995561
                                       3284469.85307204 -1.97068191184322e7
                                       5.09526024926646e7 -7.41051482115327e7
                                       6.6344512274729e7 -3.75671766607634e7
                                       1.32887671664218e7 -2785618.12808645
                                       308186.404612662 -13886.089753717
                                       110.017140269247)))
      (alfa (make-array 104 :element-type 'double-float))
      (beta (make-array 130 :element-type 'double-float))
      (gama
       (make-array 26
                   :element-type 'double-float
                   :initial-contents '(0.629960524947437 0.251984209978975
                                       0.154790300415656 0.110713062416159
                                       0.0857309395527395 0.0697161316958684
                                       0.0586085671893714 0.0504698873536311
                                       0.0442600580689155 0.039372066154351
                                       0.0354283195924455 0.0321818857502098
                                       0.0294646240791158 0.0271581677112934
                                       0.0251768272973862 0.0234570755306079
                                       0.0219508390134907 0.0206210828235646
                                       0.0194388240897881 0.0183810633800683
                                       0.0174293213231963 0.0165685837786612
                                       0.0157865285987918 0.0150729501494096
                                       0.0144193250839955 0.0138184805735342))))
  (declare (type (array double-float (26)) gama)
           (type (array double-float (130)) beta)
           (type (array double-float (104)) alfa)
           (type (array double-float (65)) c)
           (type (array double-float (10)) br)
           (type (array double-float (8)) ar)
           (type double-float con548 con2 con1 tols))
  (defun dasyjy (funjy x fnu flgjy in y wk iflw)
    (declare (type (array double-float (*)) wk y)
             (type f2cl-lib:integer4 iflw in)
             (type double-float flgjy fnu x))
    (f2cl-lib:with-multi-array-data
        ((y double-float y-%data% y-%offset%)
         (wk double-float wk-%data% wk-%offset%))
      (prog ((cr (make-array 10 :element-type 'double-float))
             (dr (make-array 10 :element-type 'double-float))
             (kmax (make-array 5 :element-type 'f2cl-lib:integer4))
             (upol (make-array 10 :element-type 'double-float)) (abw2 0.0)
             (akm 0.0) (ap 0.0) (asum 0.0) (az 0.0) (bsum 0.0) (crz32 0.0)
             (dfi 0.0) (elim 0.0) (fi 0.0) (fn 0.0) (fn2 0.0) (phi 0.0)
             (rcz 0.0) (rden 0.0) (relb 0.0) (rfn2 0.0) (rtz 0.0) (rzden 0.0)
             (sa 0.0) (sb 0.0) (suma 0.0) (sumb 0.0) (s1 0.0) (ta 0.0)
             (tau 0.0) (tb 0.0) (tfn 0.0) (tol 0.0) (t2 0.0) (xx 0.0) (z 0.0)
             (z32 0.0) (i 0) (j 0) (jn 0) (jr 0) (ju 0) (k 0) (kb 0) (klast 0)
             (kp1 0) (ks 0) (ksp1 0) (kstemp 0) (l 0) (lr 0) (lrp1 0) (iseta 0)
             (isetb 0) (abs$ 0.0f0))
        (declare (type single-float abs$)
                 (type (array f2cl-lib:integer4 (5)) kmax)
                 (type f2cl-lib:integer4 isetb iseta lrp1 lr l kstemp ksp1 ks
                                         kp1 klast kb k ju jr jn j i)
                 (type (array double-float (10)) upol dr cr)
                 (type double-float z32 z xx t2 tol tfn tb tau ta s1 sumb suma
                                    sb sa rzden rtz rfn2 relb rden rcz phi fn2
                                    fn fi elim dfi crz32 bsum az asum ap akm
                                    abw2))
        (setf ta (f2cl-lib:d1mach 3))
        (setf tol (max ta 1.e-15))
        (setf tb (f2cl-lib:d1mach 5))
        (setf ju (f2cl-lib:i1mach 15))
        (if (= flgjy 1.0) (go label6))
        (setf jr (f2cl-lib:i1mach 14))
        (setf elim (* -2.303 tb (f2cl-lib:int-add ju jr)))
        (go label7)
       label6
        (setf elim (* -2.303 (+ (* tb ju) 3.0)))
       label7
        (setf fn fnu)
        (setf iflw 0)
        (f2cl-lib:fdo (jn 1 (f2cl-lib:int-add jn 1))
                      ((> jn in) nil)
          (tagbody
            (setf xx (/ x fn))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%)
                           (- 1.0 (* xx xx)))
            (setf abw2
                    (coerce
                     (abs (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%))
                     'double-float))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%)
                           (f2cl-lib:fsqrt abw2))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%)
                           (expt fn con2))
            (if (> abw2 0.2775) (go label80))
            (setf sa 0.0)
            (if (= abw2 0.0) (go label10))
            (setf sa (/ tols (f2cl-lib:flog abw2)))
           label10
            (setf sb sa)
            (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                          ((> i 5) nil)
              (tagbody
                (setf akm (max sa 2.0))
                (f2cl-lib:fset (f2cl-lib:fref kmax (i) ((1 5)))
                               (f2cl-lib:int akm))
                (setf sa (+ sa sb))
               label20))
            (setf kb (f2cl-lib:fref kmax (5) ((1 5))))
            (setf klast (f2cl-lib:int-sub kb 1))
            (setf sa (f2cl-lib:fref gama (kb) ((1 26))))
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k klast) nil)
              (tagbody
                (setf kb (f2cl-lib:int-sub kb 1))
                (setf sa
                        (+
                         (* sa
                            (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%))
                         (f2cl-lib:fref gama (kb) ((1 26)))))
               label30))
            (setf z (* (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%) sa))
            (setf az (coerce (abs z) 'double-float))
            (setf rtz (f2cl-lib:fsqrt az))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)
                           (* con1 az rtz))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                           (* (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)
                              fn))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                           (* rtz
                              (f2cl-lib:fref wk-%data%
                                             (7)
                                             ((1 *))
                                             wk-%offset%)))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                           (*
                            (-
                             (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%))
                            (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)))
            (if (<= z 0.0) (go label35))
            (if (> (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%) elim)
                (go label75))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                           (-
                            (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)))
           label35
            (setf phi (f2cl-lib:fsqrt (f2cl-lib:fsqrt (+ sa sa sa sa))))
            (setf kb (f2cl-lib:fref kmax (5) ((1 5))))
            (setf klast (f2cl-lib:int-sub kb 1))
            (setf sb (f2cl-lib:fref beta (kb 1) ((1 26) (1 5))))
            (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                          ((> k klast) nil)
              (tagbody
                (setf kb (f2cl-lib:int-sub kb 1))
                (setf sb
                        (+
                         (* sb
                            (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%))
                         (f2cl-lib:fref beta (kb 1) ((1 26) (1 5)))))
               label40))
            (setf ksp1 1)
            (setf fn2 (* fn fn))
            (setf rfn2 (/ 1.0 fn2))
            (setf rden 1.0)
            (setf asum 1.0)
            (setf relb (* tol (abs sb)))
            (setf bsum sb)
            (f2cl-lib:fdo (ks 1 (f2cl-lib:int-add ks 1))
                          ((> ks 4) nil)
              (tagbody
                (setf ksp1 (f2cl-lib:int-add ksp1 1))
                (setf rden (* rden rfn2))
                (setf kstemp (f2cl-lib:int-sub 5 ks))
                (setf kb (f2cl-lib:fref kmax (kstemp) ((1 5))))
                (setf klast (f2cl-lib:int-sub kb 1))
                (setf sa (f2cl-lib:fref alfa (kb ks) ((1 26) (1 4))))
                (setf sb (f2cl-lib:fref beta (kb ksp1) ((1 26) (1 5))))
                (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                              ((> k klast) nil)
                  (tagbody
                    (setf kb (f2cl-lib:int-sub kb 1))
                    (setf sa
                            (+
                             (* sa
                                (f2cl-lib:fref wk-%data%
                                               (1)
                                               ((1 *))
                                               wk-%offset%))
                             (f2cl-lib:fref alfa (kb ks) ((1 26) (1 4)))))
                    (setf sb
                            (+
                             (* sb
                                (f2cl-lib:fref wk-%data%
                                               (1)
                                               ((1 *))
                                               wk-%offset%))
                             (f2cl-lib:fref beta (kb ksp1) ((1 26) (1 5)))))
                   label50))
                (setf ta (* sa rden))
                (setf tb (* sb rden))
                (setf asum (+ asum ta))
                (setf bsum (+ bsum tb))
                (if (and (<= (abs ta) tol) (<= (abs tb) relb)) (go label70))
               label60))
           label70
            (setf bsum
                    (/ bsum
                       (* fn
                          (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%))))
            (go label160)
           label75
            (setf iflw 1)
            (go end_label)
           label80
            (f2cl-lib:fset (f2cl-lib:fref upol (1) ((1 10))) 1.0)
            (setf tau
                    (/ 1.0 (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%)))
            (setf t2 (/ 1.0 (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%)))
            (if (>= (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%) 0.0)
                (go label90))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)
                           (coerce
                            (abs
                             (-
                              (f2cl-lib:fref wk-%data% (2) ((1 *)) wk-%offset%)
                              (atan
                               (f2cl-lib:fref wk-%data%
                                              (2)
                                              ((1 *))
                                              wk-%offset%))))
                            'double-float))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                           (* (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)
                              fn))
            (setf rcz
                    (/ (- con1)
                       (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)))
            (setf z32
                    (* 1.5 (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)))
            (setf rtz (expt z32 con2))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                           (* rtz
                              (f2cl-lib:fref wk-%data%
                                             (7)
                                             ((1 *))
                                             wk-%offset%)))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                           (*
                            (-
                             (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%))
                            (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)))
            (go label100)
           label90
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)
                           (coerce
                            (abs
                             (-
                              (f2cl-lib:flog
                               (/
                                (+ 1.0
                                   (f2cl-lib:fref wk-%data%
                                                  (2)
                                                  ((1 *))
                                                  wk-%offset%))
                                xx))
                              (f2cl-lib:fref wk-%data%
                                             (2)
                                             ((1 *))
                                             wk-%offset%)))
                            'double-float))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                           (* (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)
                              fn))
            (setf rcz
                    (/ con1 (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)))
            (if (> (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%) elim)
                (go label75))
            (setf z32
                    (* 1.5 (f2cl-lib:fref wk-%data% (3) ((1 *)) wk-%offset%)))
            (setf rtz (expt z32 con2))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (7) ((1 *)) wk-%offset%)
                           (expt fn con2))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                           (* rtz
                              (f2cl-lib:fref wk-%data%
                                             (7)
                                             ((1 *))
                                             wk-%offset%)))
            (f2cl-lib:fset (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                           (* (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                              (f2cl-lib:fref wk-%data%
                                             (5)
                                             ((1 *))
                                             wk-%offset%)))
           label100
            (setf phi (f2cl-lib:fsqrt (* (+ rtz rtz) tau)))
            (setf tb 1.0)
            (setf asum 1.0)
            (setf tfn (/ tau fn))
            (setf rden (/ 1.0 fn))
            (setf rfn2 (* rden rden))
            (setf rden 1.0)
            (f2cl-lib:fset (f2cl-lib:fref upol (2) ((1 10)))
                           (*
                            (+ (* (f2cl-lib:fref c (1) ((1 65))) t2)
                               (f2cl-lib:fref c (2) ((1 65))))
                            tfn))
            (setf crz32 (* con548 rcz))
            (setf bsum (+ (f2cl-lib:fref upol (2) ((1 10))) crz32))
            (setf relb (* tol (abs bsum)))
            (setf ap tfn)
            (setf ks 0)
            (setf kp1 2)
            (setf rzden rcz)
            (setf l 2)
            (setf iseta 0)
            (setf isetb 0)
            (f2cl-lib:fdo (lr 2 (f2cl-lib:int-add lr 2))
                          ((> lr 8) nil)
              (tagbody
                (setf lrp1 (f2cl-lib:int-add lr 1))
                (f2cl-lib:fdo (k lr (f2cl-lib:int-add k 1))
                              ((> k lrp1) nil)
                  (tagbody
                    (setf ks (f2cl-lib:int-add ks 1))
                    (setf kp1 (f2cl-lib:int-add kp1 1))
                    (setf l (f2cl-lib:int-add l 1))
                    (setf s1 (f2cl-lib:fref c (l) ((1 65))))
                    (f2cl-lib:fdo (j 2 (f2cl-lib:int-add j 1))
                                  ((> j kp1) nil)
                      (tagbody
                        (setf l (f2cl-lib:int-add l 1))
                        (setf s1 (+ (* s1 t2) (f2cl-lib:fref c (l) ((1 65)))))
                       label110))
                    (setf ap (* ap tfn))
                    (f2cl-lib:fset (f2cl-lib:fref upol (kp1) ((1 10)))
                                   (* ap s1))
                    (f2cl-lib:fset (f2cl-lib:fref cr (ks) ((1 10)))
                                   (* (f2cl-lib:fref br (ks) ((1 10))) rzden))
                    (setf rzden (* rzden rcz))
                    (f2cl-lib:fset (f2cl-lib:fref dr (ks) ((1 10)))
                                   (* (f2cl-lib:fref ar (ks) ((1 8))) rzden))
                   label120))
                (setf suma (f2cl-lib:fref upol (lrp1) ((1 10))))
                (setf sumb
                        (+
                         (f2cl-lib:fref upol
                                        ((f2cl-lib:int-add lr 2))
                                        ((1 10)))
                         (* (f2cl-lib:fref upol (lrp1) ((1 10))) crz32)))
                (setf ju lrp1)
                (f2cl-lib:fdo (jr 1 (f2cl-lib:int-add jr 1))
                              ((> jr lr) nil)
                  (tagbody
                    (setf ju (f2cl-lib:int-sub ju 1))
                    (setf suma
                            (+ suma
                               (* (f2cl-lib:fref cr (jr) ((1 10)))
                                  (f2cl-lib:fref upol (ju) ((1 10))))))
                    (setf sumb
                            (+ sumb
                               (* (f2cl-lib:fref dr (jr) ((1 10)))
                                  (f2cl-lib:fref upol (ju) ((1 10))))))
                   label130))
                (setf rden (* rden rfn2))
                (setf tb (- tb))
                (if (> (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%) 0.0)
                    (setf tb (coerce (abs tb) 'double-float)))
                (if (< rden tol) (go label131))
                (setf asum (+ asum (* suma tb)))
                (setf bsum (+ bsum (* sumb tb)))
                (go label140)
               label131
                (if (= iseta 1) (go label132))
                (if (< (abs suma) tol) (setf iseta 1))
                (setf asum (+ asum (* suma tb)))
               label132
                (if (= isetb 1) (go label133))
                (if (< (abs sumb) relb) (setf isetb 1))
                (setf bsum (+ bsum (* sumb tb)))
               label133
                (if (and (= iseta 1) (= isetb 1)) (go label150))
               label140))
           label150
            (setf tb (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%))
            (if (> (f2cl-lib:fref wk-%data% (1) ((1 *)) wk-%offset%) 0.0)
                (setf tb (- tb)))
            (setf bsum (/ bsum tb))
           label160
            (multiple-value-bind (var-0 var-1 var-2 var-3 var-4)
                (funcall funjy
                         (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                         (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                         (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                         fi
                         dfi)
              (declare (ignore))
              (when var-0
                (f2cl-lib:fset
                 (f2cl-lib:fref wk-%data% (6) ((1 *)) wk-%offset%)
                 var-0))
              (when var-1
                (f2cl-lib:fset
                 (f2cl-lib:fref wk-%data% (5) ((1 *)) wk-%offset%)
                 var-1))
              (when var-2
                (f2cl-lib:fset
                 (f2cl-lib:fref wk-%data% (4) ((1 *)) wk-%offset%)
                 var-2))
              (when var-3
                (setf fi var-3))
              (when var-4
                (setf dfi var-4)))
            (setf ta (/ 1.0 tol))
            (setf tb (* (f2cl-lib:d1mach 1) ta 1000.0))
            (if (> (abs fi) tb) (go label165))
            (setf fi (* fi ta))
            (setf dfi (* dfi ta))
            (setf phi (* phi tol))
           label165
            (f2cl-lib:fset (f2cl-lib:fref y-%data% (jn) ((1 *)) y-%offset%)
                           (/ (* flgjy phi (+ (* fi asum) (* dfi bsum)))
                              (f2cl-lib:fref wk-%data%
                                             (7)
                                             ((1 *))
                                             wk-%offset%)))
            (setf fn (- fn flgjy))
           label170))
        (go end_label)
       end_label
        (return (values nil nil nil nil nil nil nil iflw))))))

