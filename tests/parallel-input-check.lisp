;;; Regression helpers for rtest_parallel_input.mac.
(in-package :maxima)

(defun parallel-input-observe (thunk standard-text query-text)
  ;; Use separate streams: READONLY uses STANDARD-INPUT on SBCL/CMUCL
  ;; and QUERY-IO elsewhere. Neither channel may be consumed by a runner.
  (with-input-from-string (standard standard-text)
    (with-input-from-string (query query-text)
      (let* ((output (make-string-output-stream))
             (*standard-input* standard)
             (*query-io* (make-two-way-stream query output))
             (*standard-output* (make-broadcast-stream))
             ($errormsg nil)
             ($error nil)
             (result (errcatch (funcall thunk))))
        (list result $error (file-position standard) (file-position query)
              (get-output-stream-string output))))))

(defun parallel-input-run (thunk mode)
  (case mode
    ($serial (funcall thunk))
    ($fallback
     (let (($parallel_threads 1))
       (first (call-in-parallel (list thunk)))))
    ($public
     (let (($parallel_threads 4))
       (call-in-parallel (make-list 8 :initial-element thunk))))
    (($caller $worker)
     ;; Force the execution site, without relying on a sleep or on which
     ;; runner happens to claim an item first in CALL-IN-PARALLEL.
     (let ((job (make-parallel-job
                 :thunks (vector thunk) :results (vector nil)
                 :errors (vector nil) :count 1
                 :lock (%make-lock "parallel input test")
                 :captured (capture-bindings (specials-to-bind nil)))))
       (if (eq mode '$worker)
           #+(or sb-thread (and ccl openmcl-native-threads)
                 (and ecl threads))
           (%join (%spawn (run-worker job) "parallel input test"))
           #-(or sb-thread (and ccl openmcl-native-threads)
                 (and ecl threads))
           (error "The worker test requires native threads.")
           (call-as-runner job nil))
       (when (aref (job-errors job) 0)
         (setq $error (aref (job-errors job) 0))
         (error 'maxima-$error))
       (aref (job-results job) 0)))
    (otherwise (error "Unknown parallel input test mode: ~S" mode))))

(defun parallel-input-refused-p (observation)
  (destructuring-bind (result error standard-position query-position output)
      observation
    (and (null result)
         (equal error
                (list '(mlist simp)
                      (intl:gettext
                       "parallel: interactive input is not allowed in a parallel computation.")))
         (zerop standard-position) (zerop query-position)
         (string= output ""))))

(defun $parallel_input_check (mode)
  ;; Empty channels reproduce READONLY's old silent NIL result. Valid
  ;; answers prove that rejecting input does not depend on EOF either.
  (every
   (lambda (text)
     (every
      (lambda (thunk)
        (parallel-input-refused-p
         (parallel-input-observe
          (lambda () (parallel-input-run thunk mode)) text text)))
      (list (lambda () ($readonly "Do not print this prompt"))
            (lambda () ($read "Do not evaluate the supplied input"))
            (lambda () (retrieve "Do not ask this question" nil))
            (lambda () (retrieve nil 'noprint)))))
   '("" "12345;" "error(\"Input must not be evaluated\");")))

(defun $parallel_input_serial_check ()
  ;; Characterize the ordinary input behavior, including the two Lisp
  ;; families' different stream selection and READ versus READONLY.
  (let ((read-only (parallel-input-observe #'$readonly "11+2;" "19+4;"))
        (read-value (parallel-input-observe #'$read "11+2;" "19+4;"))
        (question (parallel-input-observe
                   (lambda () (retrieve "Question" nil)) "99;" "pos;"))
        (empty (parallel-input-observe #'$readonly "" "")))
    (and (alike1 (first (first read-only))
                 #+(or sbcl cmu) '((mplus) 11 2)
                 #-(or sbcl cmu) '((mplus) 19 4))
         (equal (first read-value) #+(or sbcl cmu) '(13)
                                  #-(or sbcl cmu) '(23))
         (equal (first question) '($pos))
         (plusp (fourth question))
         (search "Question" (fifth question))
         (equal (first empty) '(nil))
         t)))

(defun $parallel_input_random_check ()
  ;; A reproducible LCG, independent of Maxima's shared random state.
  ;; Check exact large integers and input preservation over 128 trials.
  (loop with state = 87013
        repeat 128
        always
        (progn
          (setq state (mod (+ (* state 1664525) 1013904223) (expt 2 32)))
          (let* ((n (* (- state (expt 2 31)) (expt 10 (mod state 25))))
                 (text (format nil "~D;" n))
                 (serial (parallel-input-observe #'$read text text))
                 (parallel (parallel-input-observe
                            (lambda ()
                              (parallel-input-run #'$read '$fallback))
                            text text)))
            (and (equal (first serial) (list n))
                 (parallel-input-refused-p parallel))))))
