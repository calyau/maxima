;;; Invocation-local processed-factorial checks.
(in-package :maxima)

(defun factorial-test-expression (offset width exponent)
  (let* ((argument (add '$x offset))
         (factors (loop for i from 1 to width collect (add argument i))))
    (power (apply #'mul (ftake 'mfactorial argument) factors) exponent)))

(defun factorial-test-expected (offset width exponent)
  (power (ftake 'mfactorial (add '$x offset width)) exponent))

(defun $factorial_reentry_check (mode)
  (let* ((varlist varlist) (genvar genvar)
         (expression (factorial-test-expression 0 1 1))
         (expected (factorial-test-expected 0 1 1))
         (original (symbol-function 'factexpand))
         (seen nil) (calls 0) (inner-ok nil) actual)
    (unwind-protect
         (progn
           (setf (symbol-function 'factexpand)
                 (lambda (e)
                   (incf calls)
                   (when (> calls 200)
                     (error "Factorial re-entry did not make progress"))
                   (unless seen
                     (setq seen t)
                     (setq inner-ok
                           (parallel-input-run
                            (lambda ()
                              (let ((varlist varlist) (genvar genvar))
                                (prog1
                                    (alike1 (factpluscomb
                                             (ftake 'mfactorial '$x))
                                            (ftake 'mfactorial '$x))
                                  (when (eq mode '$throw)
                                    (throw 'factorial-test-exit :inner-exit)))))
                            (if (eq mode '$worker)
                                #+(or sb-thread (and ccl openmcl-native-threads)
                                      (and ecl threads)) '$worker
                                #-(or sb-thread (and ccl openmcl-native-threads)
                                      (and ecl threads)) '$fallback
                                '$serial))))
                   (funcall original e)))
           ;; The throwing case catches inside the hook so the outer root
           ;; continues with its own processed-factorial list.
           (when (eq mode '$throw)
             (let ((hook (symbol-function 'factexpand)))
               (setf (symbol-function 'factexpand)
                     (lambda (e)
                       (let ((result (catch 'factorial-test-exit (funcall hook e))))
                         (if (eq result :inner-exit)
                             (progn (setq inner-ok t) (funcall original e))
                             result))))))
           (setq actual (factpluscomb (copy-tree expression)))
           (and seen inner-ok (alike1 actual expected)))
      (setf (symbol-function 'factexpand) original))))

(defun factorial-test-integer (n)
  (loop for k from 2 to n for product = k then (* product k)
        finally (return (or product 1))))

(defun $factorial_exact_check ()
  ;; Independent positive integer products after symbolic simplification.
  (let ((seed 81937))
    (loop repeat 96 always
          (progn
            (setq seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
            (let* ((offset (- (mod seed 9) 4))
                   (width (mod (ash seed -8) 6))
                   (exponent (1+ (mod (ash seed -16) 3)))
                   (at (+ 8 (mod (ash seed -20) 12)))
                   (expression (factorial-test-expression offset width exponent))
                   (saved (copy-tree expression))
                   (actual ($factcomb expression)))
              (and (alike1 expression saved)
                   (alike1 actual (factorial-test-expected offset width exponent))
                   (eql (meval (subst at '$x actual))
                        (expt (factorial-test-integer (+ at offset width)) exponent))))))))

(defun $factorial_parallel_check (fallback-p)
  (let* (($parallel_threads (if fallback-p 1 4))
         (jobs (loop for i below 24 collect
                     (list (- (mod i 9) 4) (mod i 5) (1+ (mod i 3)))))
         (expected (mapcar (lambda (job) (apply #'factorial-test-expected job)) jobs)))
    (loop repeat 8 always
          (every #'alike1
                 (call-in-parallel
                  (mapcar (lambda (job)
                            (let ((expression (apply #'factorial-test-expression job)))
                              (lambda () ($factcomb (copy-tree expression))))) jobs))
                 expected))))

(defun $factorial_rational_state_check ()
  (let* ((a (gensym "FACT-OUTER-A")) (b (gensym "FACT-OUTER-B"))
         (genvar (list a b)) (saved genvar))
    (setf (symbol-value a) 101 (symbol-value b) 202)
    (and (alike1 ($factcomb (factorial-test-expression 0 2 1))
                 (factorial-test-expected 0 2 1))
         (eq genvar saved)
         (= (symbol-value a) 101) (= (symbol-value b) 202))))
