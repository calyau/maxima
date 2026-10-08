;;; Compares the ISQRT square root of prototypes/fpsqrt.lisp with
;;; FPROOT on random arguments and times both. Load fpsqrt.lisp first.
(in-package :maxima)
(defun check-sqrt (prec trials)
  (let ((fpprec prec) (diff 0) (maxulp 0) (state (sb-ext:seed-random-state 42)))
    (dotimes (i trials)
      (let* ((m (+ (ash 1 (1- prec)) (random (ash 1 (1- prec)) state)))
             (e (- (random 400 state) 200))
             (a (list (list 'bigfloat 'simp prec) m e))
             (old (funcall *orig-fproot* a 2))
             (new (fpsqrt-isqrt a)))
        (unless (equal old new)
          (incf diff)
          (let ((ulp (if (= (second old) (second new)) (abs (- (first old) (first new))) :exp)))
            (when (and (numberp ulp) (> ulp maxulp)) (setq maxulp ulp))
            (when (< diff 4) (format t "~&prec ~D m ~D e ~D old ~S new ~S~%" prec m e old new))))))
    (format t "~&prec ~4D: ~D/~D differ, max ulp ~D~%" prec diff trials maxulp)))
(dolist (p '(56 64 100 200 333 1000)) (check-sqrt p 20000))
;; exactness: perfect squares
(let ((fpprec 56)) (print (list (fpsqrt-isqrt (bcons (intofp 4))) (funcall *orig-fproot* (bcons (intofp 4)) 2)
                                (fpsqrt-isqrt (bcons (intofp 9))) (funcall *orig-fproot* (bcons (intofp 9)) 2))))
;; timing
(defun time-sqrt (prec n fn)
  (let* ((fpprec prec) (a (bcons (fpquotient (intofp 2) (intofp 3)))) (t0 (get-internal-run-time)))
    (dotimes (i n) (funcall fn a 2))
    (/ (- (get-internal-run-time) t0) (float internal-time-units-per-second) n 1e-6)))
(dolist (p '(56 100 333 1000 3333))
  (format t "~&prec ~5D: old ~8,2F us  new ~8,2F us~%" p
          (time-sqrt p 2000 *orig-fproot*) (time-sqrt p 2000 (lambda (a n) (declare (ignore n)) (fpsqrt-isqrt a)))))
