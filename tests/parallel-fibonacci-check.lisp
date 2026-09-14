;;; Independent Fibonacci references and concurrent-root regression checks.
(in-package :maxima)

(defun fibonacci-test-reference (n)
  ;; Extend the defining recurrence backwards for negative indices, using
  ;; only exact addition/subtraction rather than FFIB's doubling formulas.
  (loop with a = 0 with b = 1 repeat (abs n)
        do (if (minusp n) (psetq a (- b a) b a) (psetq a b b (+ a b)))
        finally (return a)))

(defun fibonacci-test-range (start count)
  (loop with a = (fibonacci-test-reference start)
        with b = (fibonacci-test-reference (+ start 1))
        repeat count collect a do (psetq a b b (+ a b))))

(defun $fibonacci_serial_check ()
  (and
   (loop for n from -256 to 256 always
         (let ((reference (fibonacci-test-reference n)))
           (and (equal (multiple-value-list (ffib n)) (list reference))
                (= ($fib n) reference))))
   ;; The public entry point leaves non-fixnum arguments unevaluated.
   (every (lambda (n) (equal ($fib n) (list '($fib) n)))
          (list '$x 3/2 2.0d0 (+ most-positive-fixnum 1)))))

(defun $fibonacci_random_check ()
  (loop with seed = 24109 repeat 128 always
        (progn
          (setq seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
          (let* ((n (- (mod seed 4001) 2000))
                 (value ($fib n))
                 (previous ($fib (- n 1)))
                 (next ($fib (+ n 1))))
            (and (= value (fibonacci-test-reference n))
                 (= (- (* previous next) (* value value))
                    (if (oddp n) -1 1)))))))

(defun $fibonacci_parallel_check (fallback-p)
  (let* ((start 20000) (count 48)
         (expected (fibonacci-test-range start count))
         ($parallel_threads (if fallback-p 1 4)))
    (loop repeat 6 always
          (equal expected
                 (call-in-parallel
                  (loop for n from start below (+ start count)
                        collect (let ((index n)) (lambda () ($fib index)))))))))

(defun $fibonacci_native_check ()
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) t
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (let* ((start 20000) (count 32)
         (expected (fibonacci-test-range start (* 2 count)))
         (gate (%make-lock "Fibonacci test gate"))
         (ready 0) (threads nil)
         (deadline (+ (get-internal-real-time) (* 10 internal-time-units-per-second)))
         (job
          (make-parallel-job
           :thunks
           (coerce
            (loop for half below 2 collect
                  (let ((offset (* half count)))
                    (lambda ()
                      ;; Each of two workers must claim a distinct item.
                      ;; Neither can finish it before both are inside.
                      (%with-lock (gate) (incf ready))
                      (loop until (%with-lock (gate) (= ready 2))
                            do (when (>= (get-internal-real-time) deadline)
                                 (error "Fibonacci test rendezvous timed out"))
                               (sleep 0.001))
                      (loop repeat 6 always
                            (equal (subseq expected offset (+ offset count))
                                   (loop for n from (+ start offset)
                                         below (+ start offset count)
                                         collect ($fib n)))))))
            'vector)
           :results (vector nil nil) :errors (vector nil nil) :count 2
           :lock (%make-lock "Fibonacci test job")
           :captured (capture-bindings (specials-to-bind nil)))))
    (unwind-protect
         (progn
           (dotimes (index 2)
             (push (%spawn (run-worker job) "Fibonacci test worker") threads))
           (dolist (thread threads) (%join thread))
           (setq threads nil)
           (and (= ready 2) (every #'null (job-errors job))
                (every (lambda (value) (eq value t)) (job-results job))))
      ;; A partial spawn failure leaves the first worker with a bounded wait.
      ;; Attempt every join even if an earlier cleanup join reports an error.
      (dolist (thread threads) (ignore-errors (%join thread))))))
