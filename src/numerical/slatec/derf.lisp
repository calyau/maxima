;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((nterf 0)
      (xbig 0.0)
      (sqeps 0.0)
      (erfcs (make-array 21 :element-type 'double-float))
      (sqrtpi 1.772453850905516)
      (first nil))
  (declare (type f2cl-lib:logical first)
           (type (simple-array double-float (21)) erfcs)
           (type double-float sqrtpi sqeps xbig)
           (type f2cl-lib:integer4 nterf))
  (f2cl-lib:fset (f2cl-lib:fref erfcs (1) ((1 21))) -0.04904612123469181)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (2) ((1 21))) -0.14226120510371365)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (3) ((1 21))) 0.010035582187599796)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (4) ((1 21))) -5.768764699767486e-4)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (5) ((1 21))) 2.7419931252196067e-5)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (6) ((1 21))) -1.1043175507344509e-6)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (7) ((1 21))) 3.8488755420345033e-8)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (8) ((1 21))) -1.1808582533875464e-9)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (9) ((1 21))) 3.233421582605091e-11)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (10) ((1 21))) -7.991015947004547e-13)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (11) ((1 21))) 1.7990725113961456e-14)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (12) ((1 21))) -3.7186354878186934e-16)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (13) ((1 21))) 7.103599003714253e-18)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (14) ((1 21))) -1.2612455119155225e-19)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (15) ((1 21))) 2.0916406941769294e-21)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (16) ((1 21))) -3.2539731029314073e-23)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (17) ((1 21))) 4.7668672097976744e-25)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (18) ((1 21))) -6.598012078285136e-27)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (19) ((1 21))) 8.655011469963763e-29)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (20) ((1 21))) -1.0788925177498063e-30)
  (f2cl-lib:fset (f2cl-lib:fref erfcs (21) ((1 21))) 1.2811883993017004e-32)
  (setq first f2cl-lib:%true%)
  (defun derf (x)
    (declare (type double-float x))
    (prog ((y 0.0) (derf 0.0))
      (declare (type double-float derf y))
      (cond
       (first
        (setf nterf
                (initds erfcs 21
                 (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
        (setf xbig
                (f2cl-lib:fsqrt
                 (- (f2cl-lib:flog (* sqrtpi (f2cl-lib:d1mach 3))))))
        (setf sqeps (f2cl-lib:fsqrt (* 2.0 (f2cl-lib:d1mach 3))))))
      (setf first f2cl-lib:%false%)
      (setf y (coerce (abs x) 'double-float))
      (if (> y 1.0) (go label20))
      (if (<= y sqeps) (setf derf (/ (* 2.0 x x) sqrtpi)))
      (if (> y sqeps)
          (setf derf (* x (+ 1.0 (dcsevl (- (* 2.0 x x) 1.0) erfcs nterf)))))
      (go end_label)
     label20
      (if (<= y xbig) (setf derf (f2cl-lib:sign (- 1.0 (derfc y)) x)))
      (if (> y xbig) (setf derf (f2cl-lib:sign 1.0 x)))
      (go end_label)
     end_label
      (return (values derf nil)))))

