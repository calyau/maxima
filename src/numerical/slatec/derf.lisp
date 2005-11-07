;;; Compiled by f2cl version 2.0 beta Date: 2005/07/26 12:37:15 
;;; Using Lisp CMU Common Lisp Snapshot 2005-11 (19C)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((nterf 0)
      (xbig 0.0)
      (sqeps 0.0)
      (erfcs
       (make-array 21
                   :element-type 'double-float
                   :initial-contents '(-0.049046121234691806
                                       -0.14226120510371365
                                       0.010035582187599796
                                       -5.768764699767485e-4
                                       2.741993125219606e-5
                                       -1.1043175507344507e-6
                                       3.8488755420345036e-8
                                       -1.1808582533875466e-9
                                       3.2334215826050907e-11
                                       -7.991015947004549e-13
                                       1.7990725113961456e-14
                                       -3.718635487818693e-16
                                       7.103599003714253e-18
                                       -1.2612455119155226e-19
                                       2.0916406941769294e-21
                                       -3.2539731029314073e-23
                                       4.766867209797675e-25
                                       -6.598012078285134e-27
                                       8.655011469963763e-29
                                       -1.0788925177498064e-30
                                       1.2811883993017003e-32)))
      (sqrtpi 1.772453850905516)
      (first$ nil))
  (declare (type f2cl-lib:logical first$)
           (type (simple-array double-float (21)) erfcs)
           (type double-float sqrtpi sqeps xbig)
           (type f2cl-lib:integer4 nterf))
  (setq first$ f2cl-lib:%true%)
  (defun derf (x)
    (declare (type double-float x))
    (prog ((y 0.0) (derf 0.0) (abs$ 0.0f0))
      (declare (type single-float abs$) (type double-float derf y))
      (cond
        (first$
         (setf nterf
                 (initds erfcs 21
                  (* 0.1f0 (f2cl-lib:freal (f2cl-lib:d1mach 3)))))
         (setf xbig
                 (f2cl-lib:fsqrt
                  (- (f2cl-lib:flog (* sqrtpi (f2cl-lib:d1mach 3))))))
         (setf sqeps (f2cl-lib:fsqrt (* 2.0 (f2cl-lib:d1mach 3))))))
      (setf first$ f2cl-lib:%false%)
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

