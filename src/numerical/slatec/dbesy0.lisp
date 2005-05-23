;;; Compiled by f2cl version 2.0 beta Date: 2005/05/19 15:09:32 
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((nty0 0)
      (xsml 0.0)
      (by0cs (make-array 19 :element-type 'double-float))
      (twodpi 0.6366197723675814)
      (first$ nil))
  (declare (type f2cl-lib:logical first$)
           (type (simple-array double-float (19)) by0cs)
           (type double-float twodpi xsml)
           (type f2cl-lib:integer4 nty0))
  (f2cl-lib:fset (f2cl-lib:fref by0cs (1) ((1 19))) -0.011277839392865573)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (2) ((1 19))) -0.12834523756042035)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (3) ((1 19))) -0.10437884799794249)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (4) ((1 19))) 0.023662749183969694)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (5) ((1 19))) -0.002090391647700486)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (6) ((1 19))) 1.0397545393905725e-4)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (7) ((1 19))) -3.369747162423972e-6)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (8) ((1 19))) 7.729384267670667e-8)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (9) ((1 19))) -1.3249767726642596e-9)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (10) ((1 19))) 1.764823261540453e-11)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (11) ((1 19))) -1.8810550715801962e-13)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (12) ((1 19))) 1.6418654853661494e-15)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (13) ((1 19))) -1.1956594386046061e-17)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (14) ((1 19))) 7.377296297440186e-20)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (15) ((1 19))) -3.9068434767104375e-22)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (16) ((1 19))) 1.795503664436158e-24)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (17) ((1 19))) -7.22962712544801e-27)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (18) ((1 19))) 2.5717279316351685e-29)
  (f2cl-lib:fset (f2cl-lib:fref by0cs (19) ((1 19))) -8.141268814163695e-32)
  (setq first$ f2cl-lib:%true%)
  (defun dbesy0 (x)
    (declare (type double-float x))
    (prog ((ampl 0.0) (theta 0.0) (y 0.0) (dbesy0 0.0))
      (declare (type double-float dbesy0 y theta ampl))
      (cond
        (first$
         (setf nty0
                 (initds by0cs 19
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xsml (f2cl-lib:fsqrt (* 4.0 (f2cl-lib:d1mach 3))))))
      (setf first$ f2cl-lib:%false%)
      (if (<= x 0.0) (xermsg "SLATEC" "DBESY0" "X IS ZERO OR NEGATIVE" 1 2))
      (if (> x 4.0) (go label20))
      (setf y 0.0)
      (if (> x xsml) (setf y (* x x)))
      (setf dbesy0
              (+ (* twodpi (f2cl-lib:flog (* 0.5 x)) (dbesj0 x))
                 0.375
                 (dcsevl (- (* 0.125 y) 1.0) by0cs nty0)))
      (go end_label)
     label20
      (multiple-value-bind (var-0 var-1 var-2)
          (d9b0mp x ampl theta)
        (declare (ignore var-0))
        (setf ampl var-1)
        (setf theta var-2))
      (setf dbesy0 (* ampl (sin theta)))
      (go end_label)
     end_label
      (return (values dbesy0 nil)))))

