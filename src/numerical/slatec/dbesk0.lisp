;;; Compiled by f2cl version 2.0 beta Date: 2005/05/19 15:09:32 
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((ntk0 0)
      (xsml 0.0)
      (xmax 0.0)
      (bk0cs (make-array 16 :element-type 'double-float))
      (first$ nil))
  (declare (type f2cl-lib:logical first$)
           (type (simple-array double-float (16)) bk0cs)
           (type double-float xmax xsml)
           (type f2cl-lib:integer4 ntk0))
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (1) ((1 16))) -0.03532739323390277)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (2) ((1 16))) 0.3442898999246285)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (3) ((1 16))) 0.0359799365153615)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (4) ((1 16))) 0.001264615411446926)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (5) ((1 16))) 2.286212103119452e-5)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (6) ((1 16))) 2.5347910790261494e-7)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (7) ((1 16))) 1.904516377220209e-9)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (8) ((1 16))) 1.0349695257633625e-11)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (9) ((1 16))) 4.2598161427910826e-14)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (10) ((1 16))) 1.3744654358807508e-16)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (11) ((1 16))) 3.5708965285083736e-19)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (12) ((1 16))) 7.631643660116437e-22)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (13) ((1 16))) 1.365424988440782e-24)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (14) ((1 16))) 2.075275266906668e-27)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (15) ((1 16))) 2.7128142180729857e-30)
  (f2cl-lib:fset (f2cl-lib:fref bk0cs (16) ((1 16))) 3.0825938879146666e-33)
  (setq first$ f2cl-lib:%true%)
  (defun dbesk0 (x)
    (declare (type double-float x))
    (prog ((xmaxt 0.0) (y 0.0) (dbesk0 0.0))
      (declare (type double-float dbesk0 y xmaxt))
      (cond
        (first$
         (setf ntk0
                 (initds bk0cs 16
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xsml (f2cl-lib:fsqrt (* 4.0 (f2cl-lib:d1mach 3))))
         (setf xmaxt (- (f2cl-lib:flog (f2cl-lib:d1mach 1))))
         (setf xmax
                 (+ xmaxt
                    (/ (* -0.5 xmaxt (f2cl-lib:flog xmaxt)) (+ xmaxt 0.5))))))
      (setf first$ f2cl-lib:%false%)
      (if (<= x 0.0) (xermsg "SLATEC" "DBESK0" "X IS ZERO OR NEGATIVE" 2 2))
      (if (> x 2.0) (go label20))
      (setf y 0.0)
      (if (> x xsml) (setf y (* x x)))
      (setf dbesk0
              (+ (- (* (- (f2cl-lib:flog (* 0.5 x))) (dbesi0 x)) 0.25)
                 (dcsevl (- (* 0.5 y) 1.0) bk0cs ntk0)))
      (go end_label)
     label20
      (setf dbesk0 0.0)
      (if (> x xmax) (xermsg "SLATEC" "DBESK0" "X SO BIG K0 UNDERFLOWS" 1 1))
      (if (> x xmax) (go end_label))
      (setf dbesk0 (* (exp (- x)) (dbsk0e x)))
      (go end_label)
     end_label
      (return (values dbesk0 nil)))))

