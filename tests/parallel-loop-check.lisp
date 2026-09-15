;;; Numerical loop oracles for rtest_parallel_loop.mac.
(in-package :maxima)

(defun parallel-loop-test-number (value)
  (if (ratnump value) (/ (second value) (third value)) value))

(defun parallel-loop-test-maxima-number (value)
  (if (and (rationalp value) (not (integerp value)))
      (list '(rat simp) (numerator value) (denominator value))
      value))

(defun parallel-loop-test-values (from step limit parallel-p)
  ;; The body has no influence on the loop controls. A lock protects the
  ;; test's collector; sort afterwards because worker order is unspecified.
  (let* ((variable (gensym "$LOOP-CHECK"))
         (observed nil)
         (lock (%make-lock "loop regression"))
         (body (list
                (list (lambda (value)
                        (%with-lock (lock) (push value observed))
                        nil))
                variable))
         (form (list (list (if parallel-p 'mdo-parallel 'mdo))
                     variable from step nil limit nil body)))
    (declare (ignorable lock))
    (progv (list variable) (list variable)
      (meval form))
    (sort observed #'< :key #'parallel-loop-test-number)))

(defun $parallel_loop_values (from step limit mode)
  (cons '(mlist simp)
        (parallel-loop-test-values from step limit (eq mode '$parallel))))

(defun $parallel_loop_random_check (mode)
  ;; All expected values are derived in exact Common Lisp arithmetic.
  ;; Limits sit just before or after a step, including beyond double
  ;; precision. Lists stay short even when the numbers are enormous.
  (loop with state = 9137
        repeat 192
        always
        (progn
          (setq state (mod (+ (* state 1664525) 1013904223) (expt 2 32)))
          (let* ((magnitude (expt 2 (mod state 160)))
                 (step (* (if (logbitp 8 state) 1 -1) magnitude))
                 (from (* (- (mod state 19) 9) magnitude))
                 (count (mod (ash state -9) 8))
                 (fraction (/ (1+ (mod state 7)) (expt 2 200)))
                 (limit (+ from (* step (- count fraction))))
                 (expected (sort (loop for k below count
                                       collect (+ from (* k step))) #'<))
                 (actual (parallel-loop-test-values
                          from step (parallel-loop-test-maxima-number limit)
                          (eq mode '$parallel))))
            (equal expected actual)))))
