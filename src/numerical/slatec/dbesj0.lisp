;;; Compiled by f2cl version 2.0 beta 2002-05-06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((ntj0 0)
      (xsml 0.0)
      (bj0cs (make-array 19 :element-type 'double-float))
      (first nil))
  (declare (type f2cl-lib:logical first)
           (type (simple-array double-float (19)) bj0cs)
           (type double-float xsml)
           (type f2cl-lib:integer4 ntj0))
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (1) ((1 19))) 0.10025416196893913)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (2) ((1 19))) -0.6652230077644051)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (3) ((1 19))) 0.2489837034982813)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (4) ((1 19))) -0.03325272317003577)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (5) ((1 19))) 0.0023114179304694017)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (6) ((1 19))) -9.911277419950809e-5)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (7) ((1 19))) 2.8916708643998806e-6)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (8) ((1 19))) -6.121085866303262e-8)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (9) ((1 19))) 9.838650793856785e-10)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (10) ((1 19))) -1.2423551597301767e-11)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (11) ((1 19))) 1.2654336302559047e-13)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (12) ((1 19))) -1.0619456495287243e-15)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (13) ((1 19))) 7.470621075802458e-18)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (14) ((1 19))) -4.4697032274412785e-20)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (15) ((1 19))) 2.3024281584337433e-22)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (16) ((1 19))) -1.0319144794166697e-24)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (17) ((1 19))) 4.0608178274873336e-27)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (18) ((1 19))) -1.4143836005240915e-29)
  (f2cl-lib:fset (f2cl-lib:fref bj0cs (19) ((1 19))) 4.391090549669889e-32)
  (setq first f2cl-lib:%true%)
  (defun dbesj0 (x)
    (declare (type double-float x))
    (prog ((ampl 0.0) (theta 0.0) (y 0.0) (dbesj0 0.0))
      (declare (type double-float dbesj0 y theta ampl))
      (cond
       (first
        (setf ntj0
                (initds bj0cs 19
                 (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
        (setf xsml (f2cl-lib:fsqrt (* 8.0 (f2cl-lib:d1mach 3))))))
      (setf first f2cl-lib:%false%)
      (setf y (coerce (abs x) 'double-float))
      (if (> y 4.0) (go label20))
      (setf dbesj0 1.0)
      (if (> y xsml) (setf dbesj0 (dcsevl (- (* 0.125 y y) 1.0) bj0cs ntj0)))
      (go end_label)
     label20
      (multiple-value-bind
          (var-0 var-1 var-2)
          (d9b0mp y ampl theta)
        (declare (ignore var-0))
        (setf ampl var-1)
        (setf theta var-2))
      (setf dbesj0 (* ampl (cos theta)))
      (go end_label)
     end_label
      (return (values dbesj0 nil)))))

