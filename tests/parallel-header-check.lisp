;;; Precision cache and rounding scratch must follow the caller's state.
(in-package :maxima)

(defun parallel-header-state ()
  (mapcar (lambda (symbol)
            (list (boundp symbol)
                  (when (boundp symbol) (symbol-value symbol))))
          '(*bfloat-header* *bfloat-header-prec* *m)))

(defun parallel-header-mode (mode)
  (if (and (eq mode '$worker) (not (parallel-threads-p))) '$fallback mode))

(defun $parallel_header_scope (mode exit-kind unbound-p)
  (let* ((*standard-output* (make-broadcast-stream))
         (symbols '(*bfloat-header* *bfloat-header-prec* *m))
         (initial (list (list 'bigfloat 'simp 101) 101 29))
         (changed (list (list 'bigfloat 'simp 203) 203 -31)))
    (progv symbols initial
      (when unbound-p (maxima-makunbound '*m))
      (let* ((before (parallel-header-state))
             (seen t)
             (observed 0)
             (lock (%make-lock "header scope observations"))
             (result
               (catch 'parallel-header-exit
                 (handler-case
                     (parallel-input-run
                      (lambda ()
                        (let ((inherited (equal before (parallel-header-state))))
                          (%with-lock (lock)
                            (incf observed)
                            (setq seen (and seen inherited)))
                          (unwind-protect
                               (progn
                                 (loop for symbol in symbols for value in changed
                                       do (set symbol value))
                                 (case exit-kind
                                   ($error (error "Expected header scope error."))
                                   ($throw (throw 'parallel-header-exit inherited))
                                   (otherwise inherited)))
                            ;; Public scheduling can reuse a runner for several
                            ;; items. Restore this item's scratch so the next
                            ;; item can check the runner's inherited state.
                            (when (eq mode '$public)
                              (loop for symbol in symbols for state in before
                                    do (if (first state)
                                           (set symbol (second state))
                                           (maxima-makunbound symbol)))))))
                      (parallel-header-mode mode))
                   (error () :error)))))
        (declare (ignorable lock))
        (and seen (plusp observed)
             (case exit-kind
               ($error (eq result :error))
               (otherwise (if (eq mode '$public) (every #'identity result) result)))
             (equal before (parallel-header-state)))))))

(defun $parallel_header_nested (outer-mode inner-mode)
  (parallel-input-run
   (lambda () ($parallel_header_scope inner-mode '$normal nil))
   (parallel-header-mode outer-mode)))

(defun $parallel_header_numeric ()
  ;; The existing oracle decodes the binary value as an exact rational.
  ;; Compare independently to rational input, not to a serial BFLOAT call.
  (loop with generator = 51887 repeat 64 always
        (progn
          (setq generator (mod (+ (* generator 1664525) 1013904223)
                               (expt 2 32)))
          (let* ((digits (+ 12 (mod generator 189)))
                 (exact (/ (- (mod generator 20001) 10000)
                           (+ 3 (mod (ash generator -8) 997)))))
            (with-thread-local-environment
              (mset '$fpprec digits)
              (let* ((reference (parallel-precision-test-state))
                     (sample (parallel-input-run
                              (lambda () (parallel-precision-test-sample (list exact)))
                              (parallel-header-mode '$worker))))
                (parallel-precision-test-accurate-p
                 sample reference (list exact) digits)))))))

(defun $parallel_header_native ()
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (return-from $parallel_header_native ($parallel_header_numeric))
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  ;; Keep the raw native state probe active with serial scheduling.
  (progn
    (let* ((lock (%make-lock "parallel header regression"))
           (entered (vector nil nil)) (finished (vector nil nil))
           (cancelled nil) (threads nil) (jobs nil))
      (labels ((await-state (flags)
                 (let ((deadline (+ (get-internal-real-time)
                                    (* 15 internal-time-units-per-second))))
                   (loop
                     (when (%with-lock (lock) (every #'identity flags)) (return t))
                     (when (or (%with-lock (lock) cancelled)
                               (> (get-internal-real-time) deadline))
                       (return nil))
                     (sleep 0.001))))
               (make-job (role)
                 (make-parallel-job
                  :thunks
                  (vector
                   (lambda ()
                     (mset '$fpprec (if (zerop role) 23 97))
                     (let ((expected (list (list t (list 'bigfloat 'simp fpprec))
                                           (list t fpprec) (list t (+ 41 role)))))
                       (setq *bfloat-header* (list 'bigfloat 'simp fpprec)
                             *bfloat-header-prec* fpprec
                             *m (+ 41 role))
                       (%with-lock (lock) (setf (aref entered role) t))
                       (unless (await-state entered)
                         (error "Header test workers did not overlap."))
                       (let ((valid (equal expected (parallel-header-state))))
                         (%with-lock (lock) (setf (aref finished role) t))
                         (and (await-state finished) valid)))))
                  :results (vector nil) :errors (vector nil) :count 1
                  :lock (%make-lock "parallel header job")
                  :captured (capture-bindings (specials-to-bind nil)))))
        (unwind-protect
             (progn
               (dotimes (role 2)
                 (let ((job (make-job role)))
                   (push job jobs)
                   (push (%spawn (run-worker job) "parallel header worker") threads)))
               (let ((distinct (not (eq (first threads) (second threads)))))
                 (dolist (thread threads) (%join thread))
                 (setq threads nil)
                 (and distinct (every #'identity entered) (every #'identity finished)
                      (every (lambda (job)
                               (and (equalp (job-results job) #(t))
                                    (not (aref (job-errors job) 0)))) jobs))))
          (%with-lock (lock) (setq cancelled t))
          (dolist (thread threads) (%join thread)))))))
