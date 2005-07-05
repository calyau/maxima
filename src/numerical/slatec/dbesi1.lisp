;;; Compiled by f2cl version 2.0 beta Date: 2005/06/20 01:53:39 
;;; Using Lisp CMU Common Lisp Snapshot 2005-06 (19B)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((nti1 0)
      (xmin 0.0)
      (xsml 0.0)
      (xmax 0.0)
      (bi1cs
       (make-array 17
                   :element-type 'double-float
                   :initial-contents '(-0.0019717132610998596
                                       0.4073488766754648 0.03483899429995946
                                       0.0015453945563001237
                                       4.188852109837778e-5
                                       7.649026764836211e-7
                                       1.0042493924741179e-8
                                       9.93220779192381e-11
                                       7.663801791844764e-13
                                       4.741418923816739e-15
                                       2.404114404074518e-17
                                       1.0171505007093713e-19
                                       3.6450935657866947e-22
                                       1.1205749502562039e-24
                                       2.987544193446809e-27
                                       6.973231093919471e-30
                                       1.43679482206208e-32)))
      (first$ nil))
  (declare (type f2cl-lib:logical first$)
           (type (simple-array double-float (17)) bi1cs)
           (type double-float xmax xsml xmin)
           (type f2cl-lib:integer4 nti1))
  (setq first$ f2cl-lib:%true%)
  (defun dbesi1 (x)
    (declare (type double-float x))
    (prog ((y 0.0) (dbesi1 0.0) (abs$ 0.0f0))
      (declare (type single-float abs$) (type double-float dbesi1 y))
      (cond
        (first$
         (setf nti1
                 (initds bi1cs 17
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xmin (* 2.0 (f2cl-lib:d1mach 1)))
         (setf xsml (f2cl-lib:fsqrt (* 4.5 (f2cl-lib:d1mach 3))))
         (setf xmax (f2cl-lib:flog (f2cl-lib:d1mach 2)))))
      (setf first$ f2cl-lib:%false%)
      (setf y (coerce (abs x) 'double-float))
      (if (> y 3.0) (go label20))
      (setf dbesi1 0.0)
      (if (= y 0.0) (go end_label))
      (if (<= y xmin)
          (xermsg "SLATEC" "DBESI1" "ABS(X) SO SMALL I1 UNDERFLOWS" 1 1))
      (if (> y xmin) (setf dbesi1 (* 0.5 x)))
      (if (> y xsml)
          (setf dbesi1
                  (* x (+ 0.875 (dcsevl (- (/ (* y y) 4.5) 1.0) bi1cs nti1)))))
      (go end_label)
     label20
      (if (> y xmax)
          (xermsg "SLATEC" "DBESI1" "ABS(X) SO BIG I1 OVERFLOWS" 2 2))
      (setf dbesi1 (* (exp y) (dbsi1e x)))
      (go end_label)
     end_label
      (return (values dbesi1 nil)))))

