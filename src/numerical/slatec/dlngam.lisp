;;; Compiled by f2cl version 2.0 beta on 2002/04/25 at 13:19:06
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "SLATEC")


(let ((xmax 0.0)
      (dxrel 0.0)
      (sq2pil 0.9189385332046728)
      (sqpi2l 0.22579135264472744)
      (pi_ 3.141592653589793)
      (first nil))
  (declare (type f2cl-lib:logical first)
           (type double-float pi_ sqpi2l sq2pil dxrel xmax))
  (setq first f2cl-lib:%true%)
  (defun dlngam (x)
    (declare (type double-float x))
    (prog ((sinpiy 0.0) (y 0.0) (temp 0.0) (dlngam 0.0))
      (declare (type double-float dlngam temp y sinpiy))
      (cond
       (first (setf temp (/ 1.0 (f2cl-lib:flog (f2cl-lib:d1mach 2))))
              (setf xmax (* temp (f2cl-lib:d1mach 2)))
              (setf dxrel (f2cl-lib:fsqrt (f2cl-lib:d1mach 4)))))
      (setf first f2cl-lib:%false%)
      (setf y (coerce (abs x) 'double-float))
      (if (> y 10.0) (go label20))
      (setf dlngam
              (coerce
               (f2cl-lib:flog
                (abs
                 (multiple-value-bind
                     (ret-val var-0)
                     (dgamma x)
                   (declare (ignore))
                   (when var-0 (setf x var-0))
                   ret-val)))
               'double-float))
      (go end_label)
     label20
      (if (> y xmax)
          (xermsg "SLATEC" "DLNGAM" "ABS(X) SO BIG DLNGAM OVERFLOWS" 2 2))
      (if (> x 0.0)
          (setf dlngam
                  (+ (- (+ sq2pil (* (- x 0.5) (f2cl-lib:flog x))) x)
                     (multiple-value-bind
                         (ret-val var-0)
                         (d9lgmc y)
                       (declare (ignore))
                       (when var-0 (setf y var-0))
                       ret-val))))
      (if (> x 0.0) (go end_label))
      (setf sinpiy (coerce (abs (sin (* pi_ y))) 'double-float))
      (if (= sinpiy 0.0)
          (xermsg "SLATEC" "DLNGAM" "X IS A NEGATIVE INTEGER" 3 2))
      (if (< (abs (/ (- x (f2cl-lib:aint (- x 0.5))) x)) dxrel)
          (xermsg "SLATEC" "DLNGAM"
           "ANSWER LT HALF PRECISION BECAUSE X TOO NEAR NEGATIVE INTEGER" 1 1))
      (setf dlngam
              (- (+ sqpi2l (* (- x 0.5) (f2cl-lib:flog y)))
                 x
                 (f2cl-lib:flog sinpiy)
                 (multiple-value-bind
                     (ret-val var-0)
                     (d9lgmc y)
                   (declare (ignore))
                   (when var-0 (setf y var-0))
                   ret-val)))
      (go end_label)
     end_label
      (return (values dlngam x)))))

