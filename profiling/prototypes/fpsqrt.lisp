(in-package :maxima)
;; Bigfloat square root via ISQRT, correctly rounded (half to even).
(defvar *orig-fproot* (fdefinition 'fproot))
(defun fpsqrt-isqrt (a)
  ;; A is a positive bigfloat ((BIGFLOAT ...) M E). Returns (M E).
  (destructuring-bind (m e) (cdr (bigfloatp a))
    (let* ((p fpprec)
           (k (+ p 4))
           (k (if (oddp (- e p k)) (1+ k) k))
           (n (ash m k))
           (q (isqrt n))
           ;; Append a sticky bit so that FPROUND never sees a false tie.
           (q2 (+ (ash q 1) (if (= (* q q) n) 0 1)))
           (mant (fpround q2)))
      (list mant (+ *m (/ (- e p k) 2) -1 p)))))
(defun fproot (a n)
  (if (and (eql n 2) (null *decfp) (not (eql (cadr a) 0)))
      (fpsqrt-isqrt a)
      (funcall *orig-fproot* a n)))
