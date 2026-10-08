;;; Checks which square roots are correctly rounded, old FPROOT versus
;;; the ISQRT version. Load prototypes/fpsqrt.lisp first.
(in-package :maxima)
(defun cr-ok (m e p res)
  ;; Is RES = (M2 E2) within half an ulp of sqrt(m*2^(e-p))?
  (destructuring-bind (mm ee) res
    (let* ((l (- (* 2 (- ee p)) 2 (- e p)))
           (lo (* (expt (- (* 2 mm) 1) 2)))
           (hi (* (expt (+ (* 2 mm) 1) 2))))
      (if (>= l 0)
          (<= (ash lo l) m (ash hi l))
          (<= lo (ash m (- l)) hi)))))
(defun check-cr (prec trials)
  (let ((fpprec prec) (old-bad 0) (new-bad 0) (state (sb-ext:seed-random-state 7)))
    (dotimes (i trials)
      (let* ((m (+ (ash 1 (1- prec)) (random (ash 1 (1- prec)) state)))
             (e (- (random 400 state) 200))
             (a (list (list 'bigfloat 'simp prec) m e)))
        (unless (cr-ok m e prec (funcall *orig-fproot* a 2)) (incf old-bad))
        (unless (cr-ok m e prec (fpsqrt-isqrt a)) (incf new-bad))))
    (format t "~&prec ~4D: not correctly rounded: old ~D new ~D of ~D~%" prec old-bad new-bad trials)))
(dolist (p '(56 64 100 200 333 1000)) (check-cr p 20000))
