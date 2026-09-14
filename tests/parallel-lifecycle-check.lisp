;;; Controlled worker failures. Run in an otherwise idle Maxima process.
(in-package :maxima)

(defun $parallel_lifecycle_check (lifecycle-mode &optional (real-workers-p t)
                                                        (item-count 3)
                                                        (worker-budget 3))
  (when (and real-workers-p (not (parallel-threads-p)))
    (return-from $parallel_lifecycle_check '$skipped))
  (let* ((saved-spawn (and (fboundp '%spawn) (symbol-function '%spawn)))
         (saved-join (and (fboundp '%join) (symbol-function '%join)))
         (saved-runner (symbol-function 'call-as-runner))
         (saved-counters (list *live-workers* *peak-workers* *items-run-by-workers*))
         ($parallel_threads
          (if real-workers-p (+ worker-budget *live-workers*) 1))
         (granted (if real-workers-p
                      (max 0 (min (1- item-count) (1- worker-budget))) 0))
         (record-lock (%make-lock "worker lifecycle regression"))
         ;; Each record holds THREAD, RELEASED-P, FINISHED-P and JOINED-P.
         (records nil)
         (spawn-calls 0)
         (join-calls 0)
         (budget-held-p t)
         (correct nil)
         (cleanup-error nil))
    (declare (ignorable record-lock))
    (labels
        ((release-record (record)
           (%with-lock (record-lock) (setf (aref record 1) t)))
         (test-spawn (worker-function worker-name)
           (incf spawn-calls)
           (when (or (eq lifecycle-mode '$spawn_first)
                     (and (eq lifecycle-mode '$spawn_second) (= spawn-calls 2)))
             (error "lifecycle-injected-start"))
           (let* ((record (vector nil nil nil nil))
                  (worker
                   (funcall
                    saved-spawn
                    (lambda ()
                      ;; Only the join wrapper releases this worker. A
                      ;; deadline also lets a broken test worker terminate.
                      (let ((deadline (+ (get-internal-real-time)
                                         (* 10 internal-time-units-per-second))))
                        (loop until (%with-lock (record-lock) (aref record 1))
                              while (< (get-internal-real-time) deadline)
                              do (sleep 0.001))
                        (if (%with-lock (record-lock) (aref record 1))
                            (progn
                              (funcall worker-function)
                              (%with-lock (record-lock)
                                (setf (aref record 2) t)))
                            (%with-lock (record-lock)
                              (setf (aref record 2) :timed-out)))))
                    worker-name)))
             (setf (aref record 0) worker)
             (push record records)
             worker))
         (test-join (worker)
           (incf join-calls)
           (%with-lock (*budget-lock*)
             (unless (= *live-workers* (+ granted (first saved-counters)))
               (setq budget-held-p nil)))
           (let ((record (find worker records :key (lambda (r) (aref r 0)))))
             (unless record (error "Unknown lifecycle test worker"))
             (release-record record)
             (multiple-value-prog1 (funcall saved-join worker)
               (setf (aref record 3) t)
               (when (or (eq lifecycle-mode '$join_errors)
                         (and (= join-calls 1)
                              (member lifecycle-mode
                                      '($join_error $both_errors $throw_join_error))))
                 (error "lifecycle-injected-join-~D" join-calls)))))
         (test-runner (job worker-p)
           (unless worker-p
             (case lifecycle-mode
               (($caller_error $both_errors) (error "lifecycle-injected-caller"))
               (($caller_throw $throw_join_error)
                (throw 'lifecycle-escape :escaped))))
           (funcall saved-runner job worker-p))
         (error-matches-p (outcome message)
           (and (consp outcome) (eq (first outcome) :error)
                (search message (second outcome)))))
      (unwind-protect
           (progn
             (when saved-spawn (setf (symbol-function '%spawn) #'test-spawn))
             (when saved-join (setf (symbol-function '%join) #'test-join))
             (setf (symbol-function 'call-as-runner) #'test-runner)
             (let* ((outcome
                     (catch 'lifecycle-escape
                       (handler-case
                           (list :result
                                 (call-in-parallel
                                  (loop for value from 1 to item-count collect
                                        (let ((saved-value value))
                                          (lambda () saved-value)))))
                         (error (condition)
                           (list :error (princ-to-string condition))))))
                    (expected-workers
                     (cond ((not real-workers-p) 0)
                           ((eq lifecycle-mode '$spawn_first) 0)
                           ((eq lifecycle-mode '$spawn_second) 1)
                           (t granted))))
               ;; Snapshot before the test's own emergency cleanup. That
               ;; cleanup must not hide production's failure to join.
               (setq correct
                     (and (= (length records) expected-workers)
                          (= join-calls expected-workers)
                          budget-held-p
                          (%with-lock (record-lock)
                            (every (lambda (r) (eq (aref r 2) t)) records))
                          (every (lambda (r) (aref r 3)) records)
                          (= *live-workers* (first saved-counters))
                          (case lifecycle-mode
                            (($spawn_first $spawn_second)
                             (error-matches-p outcome "lifecycle-injected-start"))
                            (($caller_error $both_errors)
                             (error-matches-p outcome "lifecycle-injected-caller"))
                            (($caller_throw $throw_join_error) (eq outcome :escaped))
                            (($join_error $join_errors)
                             (error-matches-p outcome "lifecycle-injected-join-1"))
                            ($success
                             (equal outcome
                                    (list :result
                                          (loop for value from 1 to item-count
                                                collect value)))))
                          t))))
        (when saved-spawn (setf (symbol-function '%spawn) saved-spawn))
        (when saved-join (setf (symbol-function '%join) saved-join))
        (setf (symbol-function 'call-as-runner) saved-runner)
        (dolist (record records)
          (unless (aref record 3)
            (release-record record)
            (handler-case (funcall saved-join (aref record 0))
              (error () (setq cleanup-error t)))))
        (%with-lock (*budget-lock*)
          (setf *live-workers* (first saved-counters)
                *peak-workers* (second saved-counters)
                *items-run-by-workers* (third saved-counters))))
      (and correct (not cleanup-error)))))

(defun $parallel_lifecycle_random ()
  ;; The property is independent of how many workers were granted: all
  ;; started workers finish before return, and the original budget returns.
  (loop with state = 91871
        with modes = (if (parallel-threads-p)
                         '($success $spawn_first $spawn_second $caller_error
                           $caller_throw $join_error $join_errors $both_errors $throw_join_error)
                         '($success $caller_error $caller_throw))
        repeat 64
        always
        (progn
          (setq state (mod (+ (* state 1664525) 1013904223) (expt 2 32)))
          ($parallel_lifecycle_check
           (nth (mod state (length modes)) modes) (parallel-threads-p)
           (+ 3 (mod (ash state -8) 7))
           (+ 3 (mod (ash state -16) 3))))))
