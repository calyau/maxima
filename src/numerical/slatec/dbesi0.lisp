;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:18:44
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
      (first nil))
  (declare (type f2cl-lib:logical first)
           (type (simple-array double-float (18)) bi0cs)
           (type double-float xmax xsml)
           (type f2cl-lib:integer4 nti0))
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (1) ((1 18))) -0.07660547252839145)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (2) ((1 18))) 1.9273379539938083)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (3) ((1 18))) 0.22826445869203013)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (4) ((1 18))) 0.013048914667072906)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (5) ((1 18))) 4.344270900816488e-4)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (6) ((1 18))) 9.422657686001936e-6)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (7) ((1 18))) 1.4340062895106911e-7)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (8) ((1 18))) 1.613849069661749e-9)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (9) ((1 18))) 1.39665004453567e-11)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (10) ((1 18))) 9.579451725505446e-14)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (11) ((1 18))) 5.333981859862503e-16)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (12) ((1 18))) 2.458716088437471e-18)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (13) ((1 18))) 9.53568089024877e-21)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (14) ((1 18))) 3.1543820397214273e-23)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (15) ((1 18))) 9.004564101094639e-26)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (16) ((1 18))) 2.2406473691236703e-28)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (17) ((1 18))) 4.903034603242837e-31)
  (f2cl-lib:fset (f2cl-lib:fref bi0cs (18) ((1 18))) 9.508172606122669e-34)
  (setq first f2cl-lib:%true%)
  (defun dbesi0 (x)
    (declare (type double-float x))
    (prog ((y 0.0) (dbesi0 0.0))
      (declare (type double-float dbesi0 y))
      (cond
       (first
        (setf nti0
                (initds bi0cs 18
                 (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
        (setf xsml (f2cl-lib:fsqrt (* 4.5 (f2cl-lib:d1mach 3))))
        (setf xmax (f2cl-lib:flog (f2cl-lib:d1mach 2)))))
      (setf first f2cl-lib:%false%)
      (setf y (coerce (abs x) 'double-float))
      (if (> y 3.0) (go label20))
      (setf dbesi0 1.0)
      (if (> y xsml)
          (setf dbesi0
                  (+ 2.75
                     (multiple-value-bind
                         (ret-val var-0 var-1 var-2)
                         (dcsevl (- (/ (* y y) 4.5) 1.0) bi0cs nti0)
                       (declare (ignore var-0 var-1))
                       (when var-2 (setf nti0 var-2))
                       ret-val))))
      (go end_label)
     label20
      (if (> y xmax)
          (xermsg "SLATEC" "DBESI0" "ABS(X) SO BIG I0 OVERFLOWS" 2 2))
      (setf dbesi0
              (* (exp y)
                 (multiple-value-bind
                     (ret-val var-0)
                     (dbsi0e x)
                   (declare (ignore))
                   (when var-0 (setf x var-0))
                   ret-val)))
      (go end_label)
     end_label
      (return (values dbesi0 x)))))

