;;;; Native ECL semaphore adapter checks; separate from portable bindings.
(in-package :maxima)

#+(and ecl threads)
(defun check-ecl-threadcheck-regressions (&optional (stream *debug-io*))
  (let ((checks 0) (seed 91427))
    (labels ((wait (gate timeout)
               (threadcheck-wait-for-token
                (lambda () (mp:try-get-semaphore gate)) timeout))
             (verify (value)
               (incf checks)
               (unless value (error "ECL threadcheck regression ~D failed" checks))))
      ;; Empty zero-timeout waits return immediately. Posted tokens survive
      ;; timeouts and are consumed exactly once, including multiple tokens.
      (dotimes (trial 128)
        (setq seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
        (let* ((count (mod seed 16))
               (gate (mp:make-semaphore :count 0)))
          (verify (null (wait gate 0)))
          (when (plusp count) (mp:signal-semaphore gate count))
          (dotimes (index count)
            (verify (eq t (wait gate 0)))
            (verify (= (mp:semaphore-count gate) (- count index 1))))
          (verify (null (wait gate 0)))
          (verify (zerop (mp:semaphore-count gate)))))
      ;; A later producer proves that this is a bounded wait rather than
      ;; just a single nonblocking attempt. Always join the probe worker.
      (let* ((gate (mp:make-semaphore :count 0))
             (worker (mp:process-run-function
                      "threadcheck delayed signal"
                      (lambda () (sleep 0.02) (mp:signal-semaphore gate)))))
        (unwind-protect (verify (eq t (wait gate 2)))
          (mp:process-join worker))
        (verify (null (wait gate 0))))
      (format stream "~&threadcheck: ~D ECL regression assertions passed~%" checks)
      t)))
