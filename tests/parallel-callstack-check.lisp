;;; Call-stack storage checks for the parallel scheduler.
(in-package :maxima)

(defun callstack-test-mode (mode)
  (if (and (eq mode '$worker) (not (parallel-threads-p))) '$fallback mode))

(defun callstack-test-seed (frames spare)
  (let* ((active (* 5 frames))
         (stack (make-array (+ active spare) :fill-pointer active
                            :adjustable t :initial-element nil)))
    (dotimes (frame frames)
      (let ((offset (* frame 5)))
        (setf (aref stack (+ offset 1)) '(($callstack_test_ancestor) 13)
              (aref stack (+ offset 2)) '($callstack_test_argument)
              (aref stack (+ offset 3)) '(13)
              (aref stack (+ offset 4)) '$callstack_test_ancestor)))
    stack))

(defun callstack-test-copy-p (original prefix)
  (and (not (eq *mlambda-call-stack* original))
       (array-has-fill-pointer-p *mlambda-call-stack*)
       (adjustable-array-p *mlambda-call-stack*)
       (equal (coerce *mlambda-call-stack* 'list) prefix)))

(defun $parallel_callstack_site_check (mode frames spare)
  (let* ((ambient *mlambda-call-stack*)
         (*mlambda-call-stack* (callstack-test-seed frames spare))
         (original *mlambda-call-stack*)
         (capacity (array-total-size original))
         (prefix (coerce original 'list))
         (result
           (parallel-input-run
            (lambda ()
              (and
               (not (eq *mlambda-call-stack* ambient))
               (callstack-test-copy-p original prefix)
               (let ((owned *mlambda-call-stack*))
                 (unwind-protect
                      (progn
                        (when (plusp (length owned))
                          (setf (aref owned 0) :private-frame))
                        (dotimes (entry (+ capacity 7))
                          (vector-push-extend entry owned))
                        (and (= (length owned)
                                (+ (length prefix) capacity 7))
                             (equal (coerce original 'list) prefix)))
                   ;; A runner can execute several items. Balance this item's
                   ;; scratch mutations just as MLAMBDA balances its frames.
                   (replace owned prefix)
                   (setf (fill-pointer owned) (length prefix))))))
            (callstack-test-mode mode))))
    (and (if (eq mode '$public) (every #'identity result) result)
         (eq *mlambda-call-stack* original)
         (= (array-total-size original) capacity)
         (equal (coerce original 'list) prefix))))

(defun $parallel_callstack_nested_check (outer-mode inner-mode)
  (let* ((*mlambda-call-stack* (callstack-test-seed 2 0))
         (original *mlambda-call-stack*)
         (prefix (coerce original 'list)))
    (and
     (parallel-input-run
      (lambda ()
        (and
         (callstack-test-copy-p original prefix)
         (let ((outer *mlambda-call-stack*))
           (dotimes (field 5)
             (vector-push-extend (list :outer field) outer))
           (let ((outer-prefix (coerce outer 'list)))
             (and
              (parallel-input-run
               (lambda ()
                 (and (callstack-test-copy-p outer outer-prefix)
                      (progn
                        (setf (aref *mlambda-call-stack* 0) :inner)
                        (vector-push-extend :inner *mlambda-call-stack*)
                        t)))
               (callstack-test-mode inner-mode))
              (eq *mlambda-call-stack* outer)
              (equal (coerce outer 'list) outer-prefix))))))
      (callstack-test-mode outer-mode))
     (eq *mlambda-call-stack* original)
     (equal (coerce original 'list) prefix))))

(defun $parallel_callstack_generated_check ()
  (let ((seed 61873))
    (loop repeat 64 always
          (progn
            (setq seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
            ($parallel_callstack_site_check
             (nth (mod seed 4) '($caller $worker $fallback $public))
             (mod (ash seed -8) 8) (mod (ash seed -16) 31))))))

(defun $parallel_callstack_native_check (frames)
  (unless (parallel-threads-p)
    (return-from $parallel_callstack_native_check
      (and ($parallel_callstack_site_check '$fallback frames 0)
           ($parallel_callstack_nested_check '$fallback '$fallback))))
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (let* ((ambient *mlambda-call-stack*)
         (ambient-prefix (coerce ambient 'list))
         (*mlambda-call-stack* (callstack-test-seed frames 0))
         (original *mlambda-call-stack*)
         (prefix (coerce original 'list))
         (names (vector (make-symbol "CALLSTACK-A")
                        (make-symbol "CALLSTACK-B")))
         (lock (%make-lock "call stack regression"))
         (entered (vector nil nil)) (checked (vector nil nil))
         (valid (vector nil nil))
         (finished-b nil) (cancelled nil) (threads nil)
         (saved-hook (and (fboundp 'callstack-native-point)
                          (symbol-function 'callstack-native-point))))
    (labels
        ((await-state (predicate)
           (let ((deadline (+ (get-internal-real-time)
                              (* 10 internal-time-units-per-second))))
             (loop
               (when (%with-lock (lock) (funcall predicate)) (return t))
               (when (or (%with-lock (lock) cancelled)
                         (> (get-internal-real-time) deadline))
                 (return nil))
               (sleep 0.001))))
         (point (role)
           (%with-lock (lock) (setf (aref entered role) t))
           (when (await-state (lambda () (and (aref entered 0)
                                              (aref entered 1))))
             (let ((ok (and (not (eq *mlambda-call-stack* original))
                            (not (eq *mlambda-call-stack* ambient))
                            (= (length *mlambda-call-stack*)
                               (+ 5 (length prefix)))
                            (equal (subseq (coerce *mlambda-call-stack* 'list)
                                           0 (length prefix)) prefix)
                            (eq (aref *mlambda-call-stack*
                                      (1- (length *mlambda-call-stack*)))
                                (aref names role)))))
               (%with-lock (lock)
                 (setf (aref valid role) ok (aref checked role) t)))
             (await-state (lambda () (and (aref checked 0) (aref checked 1)))))
           ;; B unwinds its complete MLAMBDA frame before A returns. This
           ;; keeps the unguarded negative control from corrupting the image.
           (when (zerop role) (await-state (lambda () finished-b)))
           17)
         (make-job (role)
           (make-parallel-job
            :thunks
            (vector (lambda ()
                      (mlambda
                       (list '(lambda) '((mlist))
                             (list '(mplus)
                                   (list '(callstack-native-point) role)
                                   (+ 10 role)))
                       nil (aref names role) t (list (list (aref names role))))))
            :results (vector nil) :errors (vector nil) :count 1
            :lock (%make-lock "call stack job")
            :captured (capture-bindings (specials-to-bind nil)))))
      (unwind-protect
           (let ((job-a (make-job 0)) (job-b (make-job 1)))
             (setf (symbol-function 'callstack-native-point) #'point)
             (push (%spawn (run-worker job-a) "call stack A") threads)
             (when (await-state (lambda () (aref entered 0)))
               (push (%spawn
                      (lambda ()
                        (unwind-protect (funcall (run-worker job-b))
                          (%with-lock (lock) (setq finished-b t))))
                      "call stack B") threads))
             (dolist (thread threads) (%join thread))
             (setq threads nil)
             (and (every #'identity valid)
                  (equalp (job-results job-a) #(27))
                  (equalp (job-results job-b) #(28))
                  (not (aref (job-errors job-a) 0))
                  (not (aref (job-errors job-b) 0))
                  (equal (coerce original 'list) prefix)
                  (equal (coerce ambient 'list) ambient-prefix)))
        (%with-lock (lock) (setq cancelled t finished-b t))
        (dolist (thread threads) (%join thread))
        (if saved-hook
            (setf (symbol-function 'callstack-native-point) saved-hook)
            (fmakunbound 'callstack-native-point))))))

(defun $parallel_callstack_unwind_check (mode throw-p)
  (let* ((*mlambda-call-stack* (callstack-test-seed 2 0))
         (original *mlambda-call-stack*)
         (prefix (coerce original 'list))
         (capacity (array-total-size original))
         (seen nil)
         (saved-hook (and (fboundp 'callstack-unwind-point)
                          (symbol-function 'callstack-unwind-point))))
    (unwind-protect
         (progn
           (setf (symbol-function 'callstack-unwind-point)
                 (lambda ()
                   (setq seen
                         (and (not (eq *mlambda-call-stack* original))
                              (= (length *mlambda-call-stack*)
                                 (+ (length prefix) 5))
                              (equal (subseq (coerce *mlambda-call-stack* 'list)
                                             0 (length prefix)) prefix)))
                   (if throw-p (throw 'callstack-test-exit :escaped)
                       (merror "callstack-test: controlled failure"))))
           (let ((outcome
                   (catch 'callstack-test-exit
                     (parallel-input-observe
                      (lambda ()
                        (parallel-input-run
                         (lambda ()
                           (mlambda '((lambda) ((mlist))
                                      ((callstack-unwind-point)))
                                    nil '$callstack_test_failure t
                                    '(($callstack_test_failure))))
                         (callstack-test-mode mode)))
                      "" ""))))
             (and seen
                  (if throw-p (eq outcome :escaped)
                      (and (null (first outcome))
                           (equal (second outcome)
                                  '((mlist simp)
                                    "callstack-test: controlled failure"))))
                  (eq *mlambda-call-stack* original)
                  (= (array-total-size original) capacity)
                  (equal (coerce original 'list) prefix)
                  ;; The caller can still enter and leave another real frame.
                  (= (mlambda '((lambda) ((mlist)) ((mplus) 20 22))
                              nil '$callstack_test_after t
                              '(($callstack_test_after))) 42)
                  (equal (coerce original 'list) prefix))))
      (if saved-hook
          (setf (symbol-function 'callstack-unwind-point) saved-hook)
          (fmakunbound 'callstack-unwind-point)))))
