;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:18:33
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((naif 0)
      (naig 0)
      (x3sml 0.0)
      (xmax 0.0)
      (aifcs (make-array 13 :element-type 'double-float))
      (aigcs (make-array 13 :element-type 'double-float))
      (first nil))
  (declare (type f2cl-lib:logical first)
           (type (simple-array double-float (13)) aigcs aifcs)
           (type double-float xmax x3sml)
           (type f2cl-lib:integer4 naig naif))
  (f2cl-lib:fset (f2cl-lib:fref aifcs (1) ((1 13))) -0.03797135849667)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (2) ((1 13))) 0.05919188853726364)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (3) ((1 13))) 9.862928057727998e-4)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (4) ((1 13))) 6.848843819076567e-6)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (5) ((1 13))) 2.5942025962194715e-8)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (6) ((1 13))) 6.176612774081375e-11)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (7) ((1 13))) 1.0092454172466117e-13)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (8) ((1 13))) 1.2014792511179939e-16)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (9) ((1 13))) 1.0882945588716993e-19)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (10) ((1 13))) 7.751377219668488e-23)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (11) ((1 13))) 4.454811203717564e-26)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (12) ((1 13))) 2.1092845231692342e-29)
  (f2cl-lib:fset (f2cl-lib:fref aifcs (13) ((1 13))) 8.370173591074134e-33)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (1) ((1 13))) 0.018152365581161272)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (2) ((1 13))) 0.021572563166010757)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (3) ((1 13))) 2.567835698748325e-4)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (4) ((1 13))) 1.4265214119792408e-6)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (5) ((1 13))) 4.572114920018043e-9)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (6) ((1 13))) 9.525170843564709e-12)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (7) ((1 13))) 1.39256346057714e-14)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (8) ((1 13))) 1.5070999142762379e-17)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (9) ((1 13))) 1.2559148312567775e-20)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (10) ((1 13))) 8.306307377082133e-24)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (11) ((1 13))) 4.4657538493718574e-27)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (12) ((1 13))) 1.9900855034518866e-30)
  (f2cl-lib:fset (f2cl-lib:fref aigcs (13) ((1 13))) 7.470288525653334e-34)
  (setq first f2cl-lib:%true%)
  (defun dai (x)
    (declare (type double-float x))
    (prog ((theta 0.0) (xm 0.0) (z 0.0) (xmaxt 0.0) (dai 0.0))
      (declare (type double-float dai xmaxt z xm theta))
      (cond
       (first
        (setf naif
                (initds aifcs 13
                 (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
        (setf naig
                (initds aigcs 13
                 (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
        (setf x3sml (expt (f2cl-lib:d1mach 3) 0.3334))
        (setf xmaxt (expt (* -1.5 (f2cl-lib:flog (f2cl-lib:d1mach 1))) 0.6667))
        (setf xmax
                (-
                 (+ xmaxt
                    (/ (* (- xmaxt) (f2cl-lib:flog xmaxt))
                       (+ (* 4.0 (f2cl-lib:fsqrt xmaxt)) 1.0)))
                 0.01))))
      (setf first f2cl-lib:%false%)
      (if (>= x -1.0) (go label20))
      (multiple-value-bind
          (var-0 var-1 var-2)
          (d9aimp x xm theta)
        (declare (ignore))
        (when var-0 (setf x var-0))
        (when var-1 (setf xm var-1))
        (when var-2 (setf theta var-2)))
      (setf dai (* xm (cos theta)))
      (go end_label)
     label20
      (if (> x 1.0) (go label30))
      (setf z 0.0)
      (if (> (abs x) x3sml) (setf z (expt x 3)))
      (setf dai
              (+ 0.375
                 (-
                  (multiple-value-bind
                      (ret-val var-0 var-1 var-2)
                      (dcsevl z aifcs naif)
                    (declare (ignore var-1))
                    (when var-0 (setf z var-0))
                    (when var-2 (setf naif var-2))
                    ret-val)
                  (* x
                     (+ 0.25
                        (multiple-value-bind
                            (ret-val var-0 var-1 var-2)
                            (dcsevl z aigcs naig)
                          (declare (ignore var-1))
                          (when var-0 (setf z var-0))
                          (when var-2 (setf naig var-2))
                          ret-val))))))
      (go end_label)
     label30
      (if (> x xmax) (go label40))
      (setf dai
              (*
               (multiple-value-bind
                   (ret-val var-0)
                   (daie x)
                 (declare (ignore))
                 (when var-0 (setf x var-0))
                 ret-val)
               (exp (/ (* -2.0 x (f2cl-lib:fsqrt x)) 3.0))))
      (go end_label)
     label40
      (setf dai 0.0)
      (xermsg "SLATEC" "DAI" "X SO BIG AI UNDERFLOWS" 1 1)
      (go end_label)
     end_label
      (return (values dai x)))))

