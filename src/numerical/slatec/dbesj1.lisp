;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((ntj1 0)
      (xsml 0.0)
      (xmin 0.0)
      (bj1cs (make-array 19 :element-type 'double-float))
      (first nil))
  (declare (type f2cl-lib:logical first)
           (type (simple-array double-float (19)) bj1cs)
           (type double-float xmin xsml)
           (type f2cl-lib:integer4 ntj1))
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (1) ((1 19))) -0.11726141513332787)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (2) ((1 19))) -0.2536152183079064)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (3) ((1 19))) 0.050127080984469566)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (4) ((1 19))) -0.004631514809625082)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (5) ((1 19))) 2.4799622941591407e-4)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (6) ((1 19))) -8.678948686278827e-6)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (7) ((1 19))) 2.142939171437937e-7)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (8) ((1 19))) -3.93609307918318e-9)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (9) ((1 19))) 5.591182317946881e-11)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (10) ((1 19))) -6.327616404661393e-13)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (11) ((1 19))) 5.840991610857247e-15)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (12) ((1 19))) -4.4825338187012576e-17)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (13) ((1 19))) 2.905384492625025e-19)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (14) ((1 19))) -1.6117321978414414e-21)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (15) ((1 19))) 7.739478819392746e-24)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (16) ((1 19))) -3.248693782111998e-26)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (17) ((1 19))) 1.2022376772274104e-28)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (18) ((1 19))) -3.95201221265135e-31)
  (f2cl-lib:fset (f2cl-lib:fref bj1cs (19) ((1 19))) 1.1616780822664534e-33)
  (setq first f2cl-lib:%true%)
  (defun dbesj1 (x)
    (declare (type double-float x))
    (prog ((ampl 0.0) (theta 0.0) (y 0.0) (dbesj1 0.0))
      (declare (type double-float dbesj1 y theta ampl))
      (cond
       (first
        (setf ntj1
                (initds bj1cs 19
                 (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
        (setf xsml (f2cl-lib:fsqrt (* 8.0 (f2cl-lib:d1mach 3))))
        (setf xmin (* 2.0 (f2cl-lib:d1mach 1)))))
      (setf first f2cl-lib:%false%)
      (setf y (coerce (abs x) 'double-float))
      (if (> y 4.0) (go label20))
      (setf dbesj1 0.0)
      (if (= y 0.0) (go end_label))
      (if (<= y xmin)
          (xermsg "SLATEC" "DBESJ1" "ABS(X) SO SMALL J1 UNDERFLOWS" 1 1))
      (if (> y xmin) (setf dbesj1 (* 0.5 x)))
      (if (> y xsml)
          (setf dbesj1
                  (* x (+ 0.25 (dcsevl (- (* 0.125 y y) 1.0) bj1cs ntj1)))))
      (go end_label)
     label20
      (multiple-value-bind
          (var-0 var-1 var-2)
          (d9b1mp y ampl theta)
        (declare (ignore var-0))
        (setf ampl var-1)
        (setf theta var-2))
      (setf dbesj1 (* (f2cl-lib:sign ampl x) (cos theta)))
      (go end_label)
     end_label
      (return (values dbesj1 nil)))))

