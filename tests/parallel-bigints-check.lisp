;;; Exact integer arithmetic checks independent of serial multiplication.
(in-package :maxima)

(defstruct bigint-test-case operands answers snapshot)

(defun bigint-test-snapshot (operands)
  (let ((*print-base* 10) (*print-radix* nil) (*print-readably* t))
    (mapcar #'write-to-string operands)))

(defun bigint-test-signed (value negative-p)
  (if negative-p (- value) value))

(defun bigint-test-case (bits mask)
  (let* ((pivot (ash 1 bits))
         (left-negative (logbitp 0 mask))
         (right-negative (logbitp 1 mask))
         (dividend-negative (logbitp 2 mask))
         (operand-left (bigint-test-signed (1- pivot) left-negative))
         (operand-right (bigint-test-signed (1+ pivot) right-negative))
         (divisor (1+ pivot))
         (half-bits (max 1 (floor bits 2)))
         (quotient (+ (ash 1 half-bits) 17))
         (remainder (1- (ash 1 (mod (ash mask -3) bits))))
         ;; Expand products by powers of two; do not use the multiplication
         ;; that the check is meant to validate to create its reference.
         (dividend (bigint-test-signed
                    (+ (ash quotient bits) quotient remainder)
                    dividend-negative))
         (scaled-left (bigint-test-signed
                       (- (ash quotient bits) quotient) left-negative))
         (scaled-right (bigint-test-signed
                        (+ (ash quotient bits) quotient) right-negative))
         (square (+ (ash 1 (* 2 half-bits)) (ash 34 half-bits) 289))
         (operands (list operand-left operand-right dividend divisor scaled-left scaled-right
                         square quotient))
         (sum (if (eq left-negative right-negative)
                  (bigint-test-signed (ash 1 (1+ bits)) left-negative)
                  (if left-negative 2 -2)))
         (difference (if (eq left-negative right-negative)
                         (if left-negative 2 -2)
                         (bigint-test-signed (ash 1 (1+ bits)) left-negative))))
    (make-bigint-test-case
     :operands operands
     :answers
     (list sum difference
           (bigint-test-signed (1- (ash 1 (* 2 bits)))
                              (not (eq left-negative right-negative)))
           1 quotient
           (if dividend-negative
               (- (+ quotient (if (zerop remainder) 0 1))) quotient)
           (if (and dividend-negative (plusp remainder))
               (- divisor remainder) remainder)
           quotient (1- quotient) quotient 0 operand-left 0)
     :snapshot (bigint-test-snapshot operands))))

(defun bigint-test-cases ()
  (append
   (loop for bits in '(1 2 7 15 16 30 31 32 60 61 62 63 64 65 127 128 129
                      255 256 257 511 512 1023 1024 2048 4096 8192)
         for mask from 0 collect (bigint-test-case bits mask))
   (loop with generator = 7927 repeat 37
         do (setq generator
                  (mod (+ (* generator 1664525) 1013904223) (expt 2 32)))
         collect (bigint-test-case (1+ (mod (ash generator -8) 4096)) generator))))

(defun bigint-test-results (test-case maxima-p)
  (destructuring-bind
      (operand-left operand-right dividend divisor scaled-left scaled-right square root)
      (bigint-test-case-operands test-case)
    (declare (ignore root))
    (if maxima-p
        (list (add operand-left operand-right) (sub operand-left operand-right) (mul operand-left operand-right)
              ($gcd operand-left operand-right) ($gcd scaled-left scaled-right)
              (ftake '$floor (div dividend divisor))
              (ftake '$mod dividend divisor)
              ($isqrt square) ($isqrt (1- square)) ($isqrt (1+ square))
              (add operand-left (neg operand-left)) (mul operand-left 1) (mul operand-left 0))
        (list (+ operand-left operand-right) (- operand-left operand-right) (* operand-left operand-right)
              (gcd operand-left operand-right) (gcd scaled-left scaled-right)
              (floor dividend divisor) (mod dividend divisor)
              (isqrt square) (isqrt (1- square)) (isqrt (1+ square))
              (+ operand-left (- operand-left)) (* operand-left 1) (* operand-left 0)))))

(defun bigint-test-valid-p (test-case result)
  (and (equal result (bigint-test-case-answers test-case))
       (every #'integerp result)
       (equal (bigint-test-snapshot (bigint-test-case-operands test-case))
              (bigint-test-case-snapshot test-case))))

(defun $parallel_bigints_site_check (mode maxima-p)
  (let* ((cases (bigint-test-cases))
         (thunk (lambda ()
                  (every (lambda (test-case)
                           (bigint-test-valid-p
                            test-case (bigint-test-results test-case maxima-p)))
                         cases)))
         (result (parallel-input-run
                  thunk (if (and (eq mode '$worker) (not (parallel-threads-p)))
                            '$fallback mode))))
    (and (= (length cases) 64)
         (if (eq mode '$public) (every #'identity result) result)
         (every (lambda (test-case)
                  (equal (bigint-test-snapshot (bigint-test-case-operands test-case))
                         (bigint-test-case-snapshot test-case))) cases))))

(defun $parallel_bigints_nested_check (outer-mode inner-mode maxima-p)
  (parallel-input-run
   (lambda () ($parallel_bigints_site_check inner-mode maxima-p))
   (if (and (eq outer-mode '$worker) (not (parallel-threads-p)))
       '$fallback outer-mode)))

(defun $parallel_bigints_native_check (maxima-p)
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (return-from $parallel_bigints_native_check
    ($parallel_bigints_site_check '$fallback maxima-p))
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (when (parallel-threads-p)
   (let* ((cases (bigint-test-cases))
         (lock (%make-lock "bignum arithmetic regression"))
         (entered (vector nil nil)) (finished (vector nil nil))
         (cancelled nil) (distinct-threads nil) (jobs nil) (threads nil))
    (labels
        ((await-state (predicate)
           (let ((deadline (+ (get-internal-real-time)
                              (* 15 internal-time-units-per-second))))
             (loop
               (when (%with-lock (lock) (funcall predicate)) (return t))
               (when (or (%with-lock (lock) cancelled)
                         (> (get-internal-real-time) deadline))
                 (return nil))
               (sleep 0.001))))
         (make-job (role)
           (make-parallel-job
            :thunks
            (vector
             (lambda ()
               (%with-lock (lock) (setf (aref entered role) t))
               (unless (await-state (lambda () (every #'identity entered)))
                 (error "Bignum test workers did not overlap."))
               (let ((valid
                       (loop repeat 4 always
                             (every (lambda (test-case)
                                      (bigint-test-valid-p
                                       test-case
                                       (bigint-test-results test-case maxima-p)))
                                    cases))))
                 (%with-lock (lock) (setf (aref finished role) t))
                 (and (await-state (lambda () (every #'identity finished))) valid))))
            :results (vector nil) :errors (vector nil) :count 1
            :lock (%make-lock "bignum arithmetic job")
            :captured (capture-bindings (specials-to-bind nil)))))
      (unwind-protect
           (progn
             (dotimes (role 2)
               (let ((job (make-job role)))
                 (push job jobs)
                 (push (%spawn (run-worker job) "bignum arithmetic worker") threads)))
             (setq distinct-threads (not (eq (first threads) (second threads))))
             (dolist (thread threads) (%join thread))
             (setq threads nil)
             (and (= (length cases) 64) distinct-threads (every #'identity entered)
                  (every #'identity finished)
                  (every (lambda (job)
                           (and (equalp (job-results job) #(t))
                                (not (aref (job-errors job) 0)))) jobs)
                  (every (lambda (test-case)
                           (equal (bigint-test-snapshot (bigint-test-case-operands test-case))
                                  (bigint-test-case-snapshot test-case))) cases)))
        (%with-lock (lock) (setq cancelled t))
        (dolist (thread threads) (%join thread)))))))
