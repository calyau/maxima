;;; Compiled by f2cl version 2.0 beta Date: 2005/05/19 15:09:32 
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((ntk1 0)
      (xmin 0.0)
      (xsml 0.0)
      (xmax 0.0)
      (bk1cs (make-array 16 :element-type 'double-float))
      (first$ nil))
  (declare (type f2cl-lib:logical first$)
           (type (simple-array double-float (16)) bk1cs)
           (type double-float xmax xsml xmin)
           (type f2cl-lib:integer4 ntk1))
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (1) ((1 16))) 0.02530022733894777)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (2) ((1 16))) -0.3531559607765449)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (3) ((1 16))) -0.12261118082265715)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (4) ((1 16))) -0.006975723859639864)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (5) ((1 16))) -1.730288957513052e-4)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (6) ((1 16))) -2.4334061415659684e-6)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (7) ((1 16))) -2.213387630734726e-8)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (8) ((1 16))) -1.4114883926335278e-10)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (9) ((1 16))) -6.666901694199329e-13)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (10) ((1 16))) -2.427449850519366e-15)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (11) ((1 16))) -7.023863479386288e-18)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (12) ((1 16))) -1.6543275155100994e-20)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (13) ((1 16))) -3.233834745994449e-23)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (14) ((1 16))) -5.331275052926527e-26)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (15) ((1 16))) -7.513040716215723e-29)
  (f2cl-lib:fset (f2cl-lib:fref bk1cs (16) ((1 16))) -9.155085717654187e-32)
  (setq first$ f2cl-lib:%true%)
  (defun dbesk1 (x)
    (declare (type double-float x))
    (prog ((xmaxt 0.0) (y 0.0) (dbesk1 0.0))
      (declare (type double-float dbesk1 y xmaxt))
      (cond
        (first$
         (setf ntk1
                 (initds bk1cs 16
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xmin
                 (exp
                  (+
                   (max (f2cl-lib:flog (f2cl-lib:d1mach 1))
                        (- (f2cl-lib:flog (f2cl-lib:d1mach 2))))
                   0.01)))
         (setf xsml (f2cl-lib:fsqrt (* 4.0 (f2cl-lib:d1mach 3))))
         (setf xmaxt (- (f2cl-lib:flog (f2cl-lib:d1mach 1))))
         (setf xmax
                 (+ xmaxt
                    (/ (* -0.5 xmaxt (f2cl-lib:flog xmaxt)) (+ xmaxt 0.5))))))
      (setf first$ f2cl-lib:%false%)
      (if (<= x 0.0) (xermsg "SLATEC" "DBESK1" "X IS ZERO OR NEGATIVE" 2 2))
      (if (> x 2.0) (go label20))
      (if (< x xmin) (xermsg "SLATEC" "DBESK1" "X SO SMALL K1 OVERFLOWS" 3 2))
      (setf y 0.0)
      (if (> x xsml) (setf y (* x x)))
      (setf dbesk1
              (+ (* (f2cl-lib:flog (* 0.5 x)) (dbesi1 x))
                 (/ (+ 0.75 (dcsevl (- (* 0.5 y) 1.0) bk1cs ntk1)) x)))
      (go end_label)
     label20
      (setf dbesk1 0.0)
      (if (> x xmax) (xermsg "SLATEC" "DBESK1" "X SO BIG K1 UNDERFLOWS" 1 1))
      (if (> x xmax) (go end_label))
      (setf dbesk1 (* (exp (- x)) (dbsk1e x)))
      (go end_label)
     end_label
      (return (values dbesk1 nil)))))

