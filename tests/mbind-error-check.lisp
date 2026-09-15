;;; Saved ERRORMSG state during nested and concurrent variable binding.
(in-package :maxima)

(defun mbind-error-test-frame (initial nested-levels exit-mode)
  (let* (($errormsg initial)
         ($values (copy-list $values)) ($myoptions (copy-list $myoptions))
         ($error nil) (bindlist nil) (mspeclist nil) (loclist nil)
         (*standard-output* (make-broadcast-stream))
         (entry-scratch *$errormsg-value*)
         (test-symbol (make-symbol "$MBIND-ERROR-LOCAL"))
         (visited 0) (body-ok nil) (nested-ok t))
    (setf (get test-symbol 'assign)
          (lambda (assigned-symbol assigned-value)
            (declare (ignore assigned-symbol assigned-value))
            (when (and mbindp (not munbindp))
              (incf visited)
              (when (plusp nested-levels)
                (unless (mbind-error-test-frame
                         (not initial) (1- nested-levels) '$normal)
                  (setq nested-ok nil))))))
    (progv (list test-symbol) '(37)
      (let ((outcome
              (catch 'mbind-error-test-exit
                (errcatch
                 (mbinding ((list test-symbol '$errormsg) (list 11 (not initial)))
                   (setq body-ok
                         (and (eq $errormsg (not initial))
                              (eq (car mspeclist) initial)
                              (= (symbol-value test-symbol) 11)))
                   (case exit-mode
                     ($error (merror "mbind-error-test: controlled failure"))
                     ($throw (throw 'mbind-error-test-exit :escaped))
                     (otherwise 42)))))))
        (and body-ok nested-ok (= visited 1)
             (case exit-mode
               ($error
                (and (null outcome)
                     (equal $error '((mlist simp)
                                    "mbind-error-test: controlled failure"))))
               ($throw (eq outcome :escaped))
               (otherwise (equal outcome '(42))))
             (eq $errormsg initial)
             (eq *$errormsg-value* entry-scratch)
             (= (symbol-value test-symbol) 37)
             (null bindlist) (null mspeclist) (null loclist))))))

(defun $mbind_error_site_check (mode initial nested-levels exit-mode)
  (let ((result
          (parallel-input-run
           (lambda ()
             (let ((*$errormsg-value* :outside-mbind))
               (mbind-error-test-frame initial nested-levels exit-mode)))
           (if (and (eq mode '$worker) (not (parallel-threads-p)))
               '$fallback mode))))
    (if (eq mode '$public) (every #'identity result) result)))

(defun $mbind_error_unbound_check ()
  (let* (($values (copy-list $values)) ($myoptions (copy-list $myoptions))
         (bindlist nil) (mspeclist nil)
         (*$errormsg-value* :outside-mbind)
         (probe-symbol (make-symbol "MBIND-UNBOUND-REFERENCE"))
         ;; ECL 21.2.1 leaves a PROGV binding bound after MAKUNBOUND, even
         ;; without Maxima. Characterize that underlying Lisp behavior while
         ;; requiring the saved MUNBOUND marker and scratch restoration.
         (bound-after-makunbound
           (progv (list probe-symbol) nil
             (setf (symbol-value probe-symbol) t)
             (makunbound probe-symbol)
             (boundp probe-symbol))))
    (setf (get probe-symbol 'assign)
          (lambda (assigned-symbol assigned-value)
            (declare (ignore assigned-symbol assigned-value))
            ;; Make the option bound after MBIND has captured its unbound
            ;; state, so SYMBOL-VALUES-IN must use the captured marker.
            (when (and mbindp (not munbindp)) (setq $errormsg t))))
    (progv (list '$errormsg probe-symbol) nil
      (and (not (boundp '$errormsg))
           (mbinding ((list probe-symbol '$errormsg) '(17 t))
             (and $errormsg (eq (car mspeclist) munbound)))
           (eq (boundp '$errormsg) bound-after-makunbound)
           (eq *$errormsg-value* :outside-mbind)
           (null bindlist) (null mspeclist)))))

(defun $mbind_error_empty_check (initial)
  (let (($errormsg initial) (bindlist nil) (mspeclist nil)
        (*$errormsg-value* :outside-mbind))
    (and (equal (multiple-value-list
                 (mbinding (nil nil) (values 17 29))) '(17 29))
         (eq $errormsg initial)
         (eq *$errormsg-value* :outside-mbind)
         (null bindlist) (null mspeclist))))

(defun $mbind_error_failure_check (failure-mode)
  (let* (($errormsg t) ($error nil)
         ($values (copy-list $values)) ($myoptions (copy-list $myoptions))
         (bindlist nil) (mspeclist nil) (loclist nil)
         (*standard-output* (make-broadcast-stream))
         (*$errormsg-value* :outside-mbind)
         (*mlambda-call-stack* (copy-parallel-call-stack *mlambda-call-stack*))
         (stack-prefix (coerce *mlambda-call-stack* 'list))
         (test-symbol (make-symbol "$MBIND-ERROR-FIRST"))
         (last-symbol (make-symbol "$MBIND-ERROR-LAST"))
         (parameters (list test-symbol '$errormsg last-symbol))
         (test-arguments (if (eq failure-mode '$too_few) '(11 nil) '(11 nil 23)))
         (expected-error nil))
    (when (eq failure-mode '$too_many)
      (setq test-arguments '(11 nil 23 47)))
    (setf (get test-symbol 'assign)
          (lambda (assigned-symbol assigned-value)
            (declare (ignore assigned-symbol assigned-value))
            (when (and mbindp (not munbindp))
              (let (($errormsg nil)) (mbinding (nil nil) 0)))))
    (when (member failure-mode '($reject $reject_first $plain_reject))
      (setf (get (if (eq failure-mode '$reject_first) test-symbol last-symbol) 'assign)
            (lambda (assigned-symbol assigned-value)
              (declare (ignore assigned-symbol assigned-value))
              (when (and mbindp (not munbindp)) (merror "mbind-error-test: rejected binding")))))
    (setq expected-error
          (if (member failure-mode '($reject $reject_first $plain_reject))
              '((mlist simp) "mbind-error-test: rejected binding")
              (list '(mlist simp)
                    (intl:gettext "~A arguments supplied to ~M; found: ~M")
                    (if (eq failure-mode '$too_few)
                        (intl:gettext "Too few") (intl:gettext "Too many"))
                    (cons '($mbind_error_function) parameters)
                    (cons '(mlist) test-arguments))))
    (progv (list test-symbol last-symbol) '(37 41)
      (and
       ;; Catch the condition directly: ERRCATCH would perform its own
       ;; binding cleanup and could hide a failure in MBIND's cleanup.
       (eq (handler-case
               (with-$error
                 (if (eq failure-mode '$plain_reject)
                     (mbinding (parameters test-arguments) 999)
                     (mlambda (list '(lambda) (cons '(mlist) parameters) 999)
                              test-arguments '$mbind_error_function t
                              '(($mbind_error_function))))
                 :unexpected-return)
             (maxima-$error () :caught))
           :caught)
       (equal $error expected-error)
       $errormsg
       (eq *$errormsg-value* :outside-mbind)
       (= (symbol-value test-symbol) 37)
       (= (symbol-value last-symbol) 41)
       (null bindlist) (null mspeclist) (null loclist)
       (equal (coerce *mlambda-call-stack* 'list) stack-prefix)))))

(defun $mbind_error_generated_check ()
  (let ((generator 90551))
    (loop repeat 64 always
          (progn
            (setq generator
                  (mod (+ (* generator 1664525) 1013904223) (expt 2 32)))
            ($mbind_error_site_check
             (nth (mod generator 5) '($serial $caller $worker $fallback $public))
             (oddp (ash generator -8)) (mod (ash generator -16) 7)
             (nth (mod (ash generator -24) 3) '($normal $error $throw)))))))

(defun $mbind_error_native_check (initial &optional hold-second)
  (unless (parallel-threads-p)
    (return-from $mbind_error_native_check
      ($mbind_error_site_check '$fallback initial (if hold-second 4 3) '$normal)))
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (let* ((lock (%make-lock "MBIND error-state regression"))
         (paused nil) (finished nil) (first-finished nil)
         (second-valid t) (cancelled nil) (threads nil)
         (saved-scratch *$errormsg-value*)
         (observed nil) (restored nil) (job-a nil) (job-b nil))
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
         (make-job (thunk)
           (make-parallel-job
            :thunks (vector thunk) :results (vector nil) :errors (vector nil)
            :count 1 :lock (%make-lock "MBIND error-state job")
            :captured (capture-bindings (specials-to-bind nil)))))
      (unwind-protect
           (progn
             (setq job-a
                   (make-job
                    (lambda ()
                      (let* (($errormsg initial)
                             ($values (copy-list $values))
                             ($myoptions (copy-list $myoptions))
                             (test-symbol (make-symbol "$MBIND-ERROR-WORKER")))
                        (setf (get test-symbol 'assign)
                              (lambda (assigned-symbol assigned-value)
                                (declare (ignore assigned-symbol assigned-value))
                                (when (and mbindp (not munbindp))
                                  (%with-lock (lock) (setq paused t))
                                  (unless (await-state (lambda () finished))
                                    (error "MBIND test synchronization timed out.")))))
                        (progv (list test-symbol) '(37)
                          (mbinding ((list test-symbol '$errormsg) (list 11 initial))
                            (setq observed (eq (car mspeclist) initial)))
                          (setq restored (eq $errormsg initial))
                          (and observed restored))))))
             (setq job-b
                   (make-job
                    (lambda ()
                      (let* (($errormsg (not initial))
                             ($values (copy-list $values))
                             ($myoptions (copy-list $myoptions))
                             (test-symbol (make-symbol "$MBIND-ERROR-SECOND")))
                        (if hold-second
                            (progv (list test-symbol) '(41)
                              (setf (get test-symbol 'assign)
                                    (lambda (assigned-symbol assigned-value)
                                      (declare (ignore assigned-symbol assigned-value))
                                      (when (and mbindp (not munbindp))
                                        (%with-lock (lock) (setq finished t))
                                        (unless (await-state (lambda () first-finished))
                                          (error "MBIND test synchronization timed out.")))))
                              (mbinding ((list test-symbol '$errormsg)
                                         (list 23 (not initial)))
                                (setq second-valid
                                      (eq (car mspeclist) (not initial)))
                                19))
                            (mbinding (nil nil) 19))))))
             (push (%spawn
                    (lambda ()
                      (unwind-protect (funcall (run-worker job-a))
                        (%with-lock (lock) (setq first-finished t))))
                    "MBIND state A") threads)
             (when (await-state (lambda () paused))
               (push (%spawn
                      (lambda ()
                        (unwind-protect (funcall (run-worker job-b))
                          (%with-lock (lock) (setq finished t))))
                      "MBIND state B") threads))
             (dolist (thread threads) (%join thread))
             (setq threads nil)
             (and observed restored second-valid
                  (eq *$errormsg-value* saved-scratch)
                  (equalp (job-results job-a) #(t))
                  (equalp (job-results job-b) #(19))
                  (not (aref (job-errors job-a) 0))
                  (not (aref (job-errors job-b) 0))))
        (%with-lock (lock) (setq cancelled t finished t first-finished t))
        (dolist (thread threads) (%join thread))
        ;; Restore the real shared cell after the deliberately faulty parent.
        (setq *$errormsg-value* saved-scratch)))))
