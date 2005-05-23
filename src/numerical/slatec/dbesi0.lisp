;;; Compiled by f2cl version 2.0 beta Date: 2005/05/19 15:09:32 
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((nti0 0)
      (xsml 0.0)
      (xmax 0.0)
      (bi0cs (make-array 18 :element-type 'double-float))
      (first$ nil))
  (declare (type f2cl-lib:logical first$)
           (type (simple-array double-float (18)) bi0cs)
           (type double-float xmax xsml)
           (type f2cl-lib:integer4 nti0))
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (1) ((1 18))) -0.07660547252839145)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (2) ((1 18))) 1.9273379539938083)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (3) ((1 18))) 0.22826445869203013)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (4) ((1 18))) 0.013048914667072904)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (5) ((1 18))) 4.3442709008164877e-4)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (6) ((1 18))) 9.422657686001934e-6)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (7) ((1 18))) 1.434006289510691e-7)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (8) ((1 18))) 1.613849069661749e-9)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (9) ((1 18))) 1.3966500445356697e-11)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (10) ((1 18))) 9.579451725505446e-14)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (11) ((1 18))) 5.333981859862503e-16)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (12) ((1 18))) 2.4587160884374706e-18)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (13) ((1 18))) 9.53568089024877e-21)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (14) ((1 18))) 3.154382039721427e-23)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (15) ((1 18))) 9.004564101094637e-26)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (16) ((1 18))) 2.24064736912367e-28)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (17) ((1 18))) 4.9030346032428375e-31)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (18) ((1 18))) 9.508172606122666e-34)
  (setq first$ f2cl-lib:%true%)
  (defun dbesi0 (x)
    (declare (type double-float x))
    (prog ((y 0.0) (dbesi0 0.0) (abs$ 0.0f0))
      (declare (type single-float abs$) (type double-float dbesi0 y))
      (cond
        (first$
         (setf nti0
                 (initds bi0cs 18
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xsml (f2cl-lib:fsqrt (* 4.5 (f2cl-lib:d1mach 3))))
         (setf xmax (f2cl-lib:flog (f2cl-lib:d1mach 2)))))
      (setf first$ f2cl-lib:%false%)
      (setf y (coerce (abs x) 'double-float))
      (if (> y 3.0) (go label20))
      (setf dbesi0 1.0)
      (if (> y xsml)
          (setf dbesi0 (+ 2.75 (dcsevl (- (/ (* y y) 4.5) 1.0) bi0cs nti0))))
      (go end_label)
     label20
      (if (> y xmax)
          (xermsg "SLATEC" "DBESI0" "ABS(X) SO BIG I0 OVERFLOWS" 2 2))
      (setf dbesi0 (* (exp y) (dbsi0e x)))
      (go end_label)
     end_label
      (return (values dbesi0 nil)))))

