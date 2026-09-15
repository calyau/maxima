;;; Exact ownership transitions, with real asynchronous runtime callbacks.
(in-package :maxima)

(define-condition startup-test-interrupt (error) ())
(defun startup-test-thread ()
  #+sb-thread sb-thread:*current-thread*
  #+(and ccl openmcl-native-threads (not sb-thread)) ccl:*current-process*
  #+(and ecl threads (not sb-thread) (not ccl)) mp:*current-process*
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) nil)
(defun startup-test-deferred-p ()
  #+sb-thread (not sb-sys:*interrupts-enabled*)
  #+(and ccl openmcl-native-threads (not sb-thread)) (minusp ccl::*interrupt-level*)
  #+(and ecl threads (not sb-thread) (not ccl)) (not si:*interrupts-enabled*)
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) nil)
(defun startup-test-send (thread function)
  #+sb-thread (sb-thread:interrupt-thread thread function)
  #+(and ccl openmcl-native-threads (not sb-thread)) (ccl:process-interrupt thread function)
  #+(and ecl threads (not sb-thread) (not ccl)) (mp:interrupt-process thread function)
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (declare (ignore thread function)))

(defun $parallel_startup_interrupt_check (phase signal-kind &optional (item-count 4) (slots 3))
  (unless (parallel-threads-p) (return-from $parallel_startup_interrupt_check '$skipped))
  (let* ((main-thread (startup-test-thread))
         (record-lock (%make-lock "startup interrupt regression"))
         (old-claim (symbol-function 'claim-workers))
         (old-spawn (symbol-function '%spawn))
         (old-join (symbol-function '%join))
         (saved-counters (list *live-workers* *peak-workers* *items-run-by-workers*))
         ($parallel_threads (+ slots *live-workers*))
         (records nil) (spawn-count 0) (controller nil) (controller-joined nil)
         (armed nil) (sent nil) (delivered nil) (timed-out nil) (outcome nil)
         (budget-held t) (correct nil))
    (declare (ignorable record-lock))
    (labels ((wait-for (predicate)
               (loop with deadline = (+ (get-internal-real-time) (* 10 internal-time-units-per-second))
                     until (%with-lock (record-lock) (funcall predicate))
                     do (when (> (get-internal-real-time) deadline)
                          (%with-lock (record-lock) (setq timed-out t))
                          (return-from wait-for nil))
                        (sleep 0.001)
                     finally (return t)))
             (pause-for-signal ()
               (%with-lock (record-lock) (setq armed t))
               ;; A masked region must finish registering ownership before
               ;; callback delivery. Otherwise wait here for actual delivery.
               (if (startup-test-deferred-p)
                   (unless (wait-for (lambda () sent)) (error "No startup interrupt queued"))
                   (progn (wait-for (lambda () nil)) (error "Startup interrupt not delivered"))))
             (claim (wanted)
               (let ((number (funcall old-claim wanted)))
                 (when (eq phase '$claim) (pause-for-signal))
                 number))
             (spawn (function name)
               ;; Record THREAD, RELEASED, FINISHED and JOINED independently
               ;; of CALL-IN-PARALLEL's private list of registered workers.
               (let* ((record (vector nil nil nil nil))
                      (thread (funcall old-spawn
                                       (lambda ()
                                         (unwind-protect
                                              (when (wait-for (lambda () (aref record 1)))
                                                (funcall function))
                                           (%with-lock (record-lock) (setf (aref record 2) t))))
                                       name)))
                 (setf (aref record 0) thread)
                 (push record records)
                 (incf spawn-count)
                 (when (or (and (eq phase '$spawn_first) (= spawn-count 1))
                           (and (eq phase '$spawn_second) (= spawn-count 2)))
                   (pause-for-signal))
                 thread))
             (join (thread)
               (let ((record (find thread records :key (lambda (record) (aref record 0)))))
                 (assert record)
                 (%with-lock (*budget-lock*)
                   (unless (= *live-workers*
                              (+ (first saved-counters) (min (1- slots) (1- item-count))))
                     (setq budget-held nil)))
                 (%with-lock (record-lock) (setf (aref record 1) t))
                 (prog1 (funcall old-join thread) (setf (aref record 3) t)))))
      (unwind-protect
           (progn
             (setq controller
                   (funcall old-spawn
                            (lambda ()
                              (when (wait-for (lambda () armed))
                                (startup-test-send
                                 main-thread
                                 (lambda ()
                                   (%with-lock (record-lock) (setq delivered t))
                                   (if (eq signal-kind '$throw)
                                       (throw 'startup-test-escape :interrupted)
                                       (error 'startup-test-interrupt))))
                                (%with-lock (record-lock) (setq sent t))))
                            "startup interrupt controller"))
             (setf (symbol-function 'claim-workers) #'claim
                   (symbol-function '%spawn) #'spawn
                   (symbol-function '%join) #'join)
             (setq outcome
                   (catch 'startup-test-escape
                     (handler-case
                         (list :result (call-in-parallel
                                        (loop for n from 1 to item-count collect
                                              (let ((value n)) (lambda () (* value value))))))
                       (startup-test-interrupt () :interrupted))))
             (funcall old-join controller)
             (setq controller-joined t)
             ;; Capture failure before emergency cleanup can repair it.
             (setq correct
                   (and (eq outcome :interrupted) delivered sent (not timed-out)
                        (= spawn-count (ecase phase ($claim 0) ($spawn_first 1) ($spawn_second 2)))
                        budget-held
                        (= *live-workers* (first saved-counters))
                        (%with-lock (record-lock)
                          (every (lambda (record) (and (aref record 2) (aref record 3))) records)))))
        (setf (symbol-function 'claim-workers) old-claim
              (symbol-function '%spawn) old-spawn
              (symbol-function '%join) old-join)
        (%with-lock (record-lock)
          (setq armed t)
          (dolist (record records) (setf (aref record 1) t)))
        (dolist (record records)
          ;; CCL JOIN-PROCESS consumes its completion notification. Never
          ;; join again when production has already joined this worker.
          (unless (aref record 3) (funcall old-join (aref record 0))))
        (when (and controller (not controller-joined)) (funcall old-join controller))
        (%with-lock (*budget-lock*)
          (setf *live-workers* (first saved-counters)
                *peak-workers* (second saved-counters)
                *items-run-by-workers* (third saved-counters)))))
    correct))
(values)

(defun $parallel_startup_delivery_check ()
  (unless (parallel-threads-p) (return-from $parallel_startup_delivery_check '$skipped))
  (let* ((origin (startup-test-thread)) ($parallel_threads 2) ($errormsg nil)
         (record-lock (%make-lock "startup body interrupt delivery"))
         (records nil) (controller nil) (controller-joined nil) (timed-out nil)
         (saved-counters (list *live-workers* *peak-workers* *items-run-by-workers*))
         (result nil) (correct nil))
    (declare (ignorable record-lock))
    (labels ((wait-for (predicate)
               (loop with deadline = (+ (get-internal-real-time) (* 10 internal-time-units-per-second))
                     until (%with-lock (record-lock) (funcall predicate))
                     do (when (> (get-internal-real-time) deadline)
                          (%with-lock (record-lock) (setq timed-out t))
                          (return-from wait-for nil))
                        (sleep 0.001)
                     finally (return t)))
             (item (value)
               (let ((record (vector (startup-test-thread) nil (startup-test-deferred-p))))
                 (%with-lock (record-lock) (push record records))
                 (unless (wait-for (lambda () (aref record 1)))
                   (error "Startup body callback was not delivered"))
                 (* value value))))
      (unwind-protect
           (progn
             (setq controller
                   (%spawn (lambda ()
                             (when (wait-for (lambda () (= (length records) 2)))
                               (dolist (record (%with-lock (record-lock) (copy-list records)))
                                 (let ((target-record record))
                                   (startup-test-send
                                    (aref target-record 0)
                                    (lambda ()
                                      (%with-lock (record-lock)
                                        (setf (aref target-record 1) t))))))))
                           "startup body callback controller"))
             (setq result
                   (handler-case (call-in-parallel (list (lambda () (item -129))
                                                         (lambda () (item 1000000000001))))
                     (error () :failed)))
             (%join controller)
             (setq controller-joined t)
             (setq correct
                   (and (equal result '(16641 1000000000002000000000001))
                        (not timed-out) (= *live-workers* (first saved-counters))
                        (= (count origin records :key (lambda (record) (aref record 0))) 1)
                        (every (lambda (record) (and (aref record 1) (not (aref record 2)))) records))))
        (when (and controller (not controller-joined)) (%join controller))
        (%with-lock (*budget-lock*)
          (setf *live-workers* (first saved-counters)
                *peak-workers* (second saved-counters)
                *items-run-by-workers* (third saved-counters)))))
    correct))

(defmacro startup-test-without-interrupts (&body body)
  #+sb-thread `(sb-sys:without-interrupts ,@body)
  #+(and ccl openmcl-native-threads (not sb-thread)) `(ccl:without-interrupts ,@body)
  #+(and ecl threads (not sb-thread) (not ccl)) `(mp:without-interrupts ,@body)
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) `(progn ,@body))

(defun $parallel_startup_policy_check (mode)
  (let (($parallel_threads 1) (saved-live *live-workers*) (body-count 0)
        (initial-policy (startup-test-deferred-p))
        (disabled-policy
          #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) t
          #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) nil))
    (labels ((body ()
               (incf body-count)
               (list (* -129 -129) (startup-test-deferred-p)))
             (run (expected)
               (equal (call-in-parallel (list #'body)) (list (list 16641 expected))))
             (nested (expected)
               (equal (call-in-parallel
                       (list (lambda () (call-in-parallel (list #'body)))))
                      (list (list (list 16641 expected))))))
      (and (ecase mode
             ($normal (run initial-policy))
             ($disabled (startup-test-without-interrupts (run disabled-policy)))
             ($nested (nested initial-policy))
             ($disabled_nested
              (startup-test-without-interrupts (nested disabled-policy)))
             ($empty (null (call-in-parallel nil))))
           (= body-count (if (eq mode '$empty) 0 1))
           (eq initial-policy (startup-test-deferred-p))
           (= saved-live *live-workers*)))))

(defun $parallel_startup_random_check ()
  (unless (parallel-threads-p) (return-from $parallel_startup_random_check '$skipped))
  (loop with state = 530035 repeat 64 always
    (progn
      (setq state (mod (+ (* state 1664525) 1013904223) (expt 2 32)))
      (let ((saved-live *live-workers*))
        (unwind-protect
             (progn
               ;; Stand in for unrelated reservations in this idle fixture.
               ;; Cleanup must preserve that count rather than reset to zero.
               (setq *live-workers* (+ saved-live (mod (ash state -9) 4)))
               ($parallel_startup_interrupt_check
                (nth (mod state 3) '($claim $spawn_first $spawn_second))
                (if (logbitp 5 state) '$error '$throw)
                (+ 4 (mod (ash state -12) 17)) (+ 3 (mod (ash state -20) 4))))
          (setq *live-workers* saved-live))))))

(defun $parallel_startup_worker_check ()
  (unless (parallel-threads-p) (return-from $parallel_startup_worker_check '$skipped))
  (let ((origin (startup-test-thread))
        (spawn (symbol-function '%spawn)) (join (symbol-function '%join))
        (result nil))
    ;; Capture the primitives before the child replaces their global cells.
    ;; Its outer join must never enter the child's startup observation hook.
    (funcall join
             (funcall spawn
                      (lambda ()
                        (with-thread-local-environment
                          (setq result
                                (and (not (eq origin (startup-test-thread)))
                                     ($parallel_startup_interrupt_check '$spawn_second '$throw)))))
                      "startup regression from worker"))
    result))

(defun startup-test-collect ()
  #+sb-thread
  (let ((before sb-kernel::*gc-epoch*))
    (sb-ext:gc :full t)
    (not (eq before sb-kernel::*gc-epoch*)))
  #+(and ccl openmcl-native-threads (not sb-thread))
  (let ((before (ccl::gccounts)))
    (ccl:gc)
    (> (ccl::gccounts) before))
  #+(and ecl threads (not sb-thread) (not ccl))
  (multiple-value-bind (bytes before old-status) (si:gc-stats t)
    (declare (ignore bytes))
    (unwind-protect (progn (ext:gc t) (> (nth-value 1 (si:gc-stats t)) before))
      (si:gc-stats old-status)))
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) nil)

(defun $parallel_startup_mask_check (mode)
  (unless (parallel-threads-p) (return-from $parallel_startup_mask_check '$skipped))
  (loop repeat (ecase mode ($policy 64) ($gc 10)) always
    (let ((record (vector nil nil nil)) (record-lock (%make-lock "startup mask regression"))
          (origin (startup-test-thread)) (controller nil) (saved-live *live-workers*))
      (declare (ignorable record-lock))
      (labels ((wait-for (index)
                 (loop with deadline = (+ (get-internal-real-time) (* 10 internal-time-units-per-second))
                       until (%with-lock (record-lock) (aref record index))
                       do (when (> (get-internal-real-time) deadline)
                            (return-from wait-for nil))
                          (sleep 0.001)
                       finally (return t)))
               (scope ()
                 (%without-interrupts
                   (%with-lock (record-lock) (setf (aref record 0) t))
                   (and (wait-for 1)
                        (%with-lock (record-lock)
                          (eq (aref record 2) (eq mode '$gc)))))))
        (unwind-protect
             (progn
               (setq controller
                     (%spawn
                      (lambda ()
                        (when (wait-for 0)
                          (ecase mode
                            ($policy
                             (startup-test-send
                              origin (lambda ()
                                       (%with-lock (record-lock)
                                         (setf (aref record 2) t)))))
                            ($gc
                             (let ((collected (startup-test-collect)))
                               (%with-lock (record-lock)
                                 (setf (aref record 2) collected)))))
                        (%with-lock (record-lock) (setf (aref record 1) t))))
                      "startup mask controller"))
               (and (ecase mode
                      ($policy
                       (startup-test-without-interrupts
                         (and (scope)
                              (%with-lock (record-lock) (not (aref record 2))))))
                      ($gc (scope)))
                    (wait-for 2) (= saved-live *live-workers*)))
          ;; Only this cleanup joins the controller, including failure paths.
          (%with-lock (record-lock) (setf (aref record 0) t))
          (when controller (%join controller)))))))

(defun $parallel_startup_fallback_check ()
  (let (($parallel_threads
          #+(and ecl threads) (if (string= (lisp-implementation-version) "21.2.1") 8 1)
          #-(and ecl threads) 1)
        (spawn (and (fboundp '%spawn) (symbol-function '%spawn)))
        (attempts 0) (bodies 0) (saved-live *live-workers*))
    (unwind-protect
         (progn
           (when spawn
             (setf (symbol-function '%spawn)
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (incf attempts)
                     (error "Serial fallback tried to create a worker"))))
           (and (equal (handler-case
                           (call-in-parallel
                            (mapcar (lambda (value)
                                      (lambda () (incf bodies) (* value value)))
                                    '(-129 0 1000000000001)))
                         (error () :failed))
                       '(16641 0 1000000000002000000000001))
                (= bodies 3) (zerop attempts) (= saved-live *live-workers*)))
      (when spawn (setf (symbol-function '%spawn) spawn)))))
