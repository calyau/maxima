;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:18:50
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((nty1 0)
      (xmin 0.0)
      (xsml 0.0)
      (by1cs (make-array 20 :element-type 'double-float))
      (twodpi 0.6366197723675814)
      (first nil))
  (declare (type f2cl-lib:logical first)
           (type (simple-array double-float (20)) by1cs)
           (type double-float twodpi xsml xmin)
           (type f2cl-lib:integer4 nty1))
  (f2cl-lib:fset (f2cl-lib:fref by1cs (1) ((1 20))) 0.03208047100611909)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (2) ((1 20))) 1.2627078974335004)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (3) ((1 20))) 0.0064999618999231745)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (4) ((1 20))) -0.08936164528860505)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (5) ((1 20))) 0.013250881221757098)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (6) ((1 20))) -8.979059119648353e-4)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (7) ((1 20))) 3.647361487958307e-5)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (8) ((1 20))) -1.0013743816660006e-6)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (9) ((1 20))) 1.9945396573901739e-8)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (10) ((1 20))) -3.023065601803382e-10)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (11) ((1 20))) 3.6098781569478117e-12)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (12) ((1 20))) -3.4874882972875826e-14)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (13) ((1 20))) 2.7838789715591766e-16)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (14) ((1 20))) -1.867870968619488e-18)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (15) ((1 20))) 1.0685315339116826e-20)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (16) ((1 20))) -5.274721956684483e-23)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (17) ((1 20))) 2.2701994031556638e-25)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (18) ((1 20))) -8.595390353945233e-28)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (19) ((1 20))) 2.8854043798337947e-30)
  (f2cl-lib:fset (f2cl-lib:fref by1cs (20) ((1 20))) -8.647541138937175e-33)
  (setq first f2cl-lib:%true%)
  (defun dbesy1 (x)
    (declare (type double-float x))
    (prog ((ampl 0.0) (theta 0.0) (y 0.0) (dbesy1 0.0))
      (declare (type double-float dbesy1 y theta ampl))
      (cond
       (first
        (setf nty1
                (initds by1cs 20
                 (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
        (setf xmin
                (* 1.571
                   (exp
                    (+
                     (max (f2cl-lib:flog (f2cl-lib:d1mach 1))
                          (- (f2cl-lib:flog (f2cl-lib:d1mach 2))))
                     0.01))))
        (setf xsml (f2cl-lib:fsqrt (* 4.0 (f2cl-lib:d1mach 3))))))
      (setf first f2cl-lib:%false%)
      (if (<= x 0.0) (xermsg "SLATEC" "DBESY1" "X IS ZERO OR NEGATIVE" 1 2))
      (if (> x 4.0) (go label20))
      (if (< x xmin) (xermsg "SLATEC" "DBESY1" "X SO SMALL Y1 OVERFLOWS" 3 2))
      (setf y 0.0)
      (if (> x xsml) (setf y (* x x)))
      (setf dbesy1
              (+
               (* twodpi
                  (f2cl-lib:flog (* 0.5 x))
                  (multiple-value-bind
                      (ret-val var-0)
                      (dbesj1 x)
                    (declare (ignore))
                    (when var-0 (setf x var-0))
                    ret-val))
               (/
                (+ 0.5
                   (multiple-value-bind
                       (ret-val var-0 var-1 var-2)
                       (dcsevl (- (* 0.125 y) 1.0) by1cs nty1)
                     (declare (ignore var-0 var-1))
                     (when var-2 (setf nty1 var-2))
                     ret-val))
                x)))
      (go end_label)
     label20
      (multiple-value-bind
          (var-0 var-1 var-2)
          (d9b1mp x ampl theta)
        (declare (ignore))
        (when var-0 (setf x var-0))
        (when var-1 (setf ampl var-1))
        (when var-2 (setf theta var-2)))
      (setf dbesy1 (* ampl (sin theta)))
      (go end_label)
     end_label
      (return (values dbesy1 x)))))

