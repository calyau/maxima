;;;; Regression checks for the thread-environment checker itself.
;;;; Loaded by threadcheck.lisp after the portable isolation checks.

(in-package :maxima)

(defun check-threadcheck-regressions (&optional (stream *debug-io*))
  (let ((checks 0)
        (seed 20260914))
    (labels ((verify (value)
               (incf checks)
               (unless value
                 (error "threadcheck regression ~D failed" checks)))
             (sample ()
               ;; A fixed sequence makes failed trials reproducible on all
               ;; Lisps, whose RANDOM implementations need not agree.
               (setq seed (mod (+ (* seed 1664525) 1013904223)
                               (expt 2 32)))))
      ;; Deliberately demand two bindings the macro does not provide.
      ;; Both must be reported, and even a failed check must restore the
      ;; original values, including an originally unbound symbol.
      (let* ((a (gensym "THREADCHECK-BOUND-"))
             (b (gensym "THREADCHECK-UNBOUND-"))
             (original (list :original))
             (*thread-environment-variables* (list a b)))
        (set a original)
        (unwind-protect
             (let ((leaked (check-bindings (make-broadcast-stream))))
               (verify (equal leaked (list a b)))
               (verify (eq (symbol-value a) original))
               (verify (not (boundp b))))
          (makunbound a)
          (makunbound b)))
      ;; Check the aggregate result without starting threads in this
      ;; portable test.  A failed buffer check or race must fail the
      ;; aggregate; an unsupported race must preserve its boolean API.
      (let ((race (symbol-function 'check-race))
            (output (make-broadcast-stream)))
        (unwind-protect
             (progn
               (setf (symbol-function 'check-race)
                     (lambda (&optional stream) (declare (ignore stream)) t))
               (let ((linearray (make-array 3)))
                 (verify (null (check-thread-environment output))))
               (setf (symbol-function 'check-race)
                     (lambda (&optional stream) (declare (ignore stream)) nil))
               (verify (null (check-thread-environment output)))
               (setf (symbol-function 'check-race)
                     #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
                     (lambda (&optional stream)
                       (declare (ignore stream)) t)
                     #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) race)
               (verify (eq t (check-thread-environment output))))
          (setf (symbol-function 'check-race) race)))
      ;; Properties across varied initial values and nonlocal exits:
      ;; nested environments restore the enclosing computation's state,
      ;; and every display buffer belongs to just one environment.
      (dotimes (trial 128)
        (let* ((symbols '(sign minus odds evens))
               (values (loop repeat 4 collect (list (sample))))
               (outer linearray))
          (progv symbols values
            (with-thread-local-environment
              (verify (and (not (eq linearray outer))
                           (every #'null linearray)))
              (let ((inner linearray)
                    (markers (loop repeat 4 collect (list (sample)))))
                (loop for symbol in symbols for value in markers
                      do (set symbol value))
                (catch 'threadcheck-exit
                  (with-thread-local-environment
                    (verify (not (eq linearray inner)))
                    (loop for symbol in symbols
                          do (set symbol (list (sample))))
                    (throw 'threadcheck-exit nil)))
                (verify (eq linearray inner))
                (verify (every #'eq markers
                               (mapcar #'symbol-value symbols)))))
            (verify (eq linearray outer))
            (verify (every #'eq values (mapcar #'symbol-value symbols))))))
      ;; Older ECL needs a bounded poll around its nonblocking semaphore
      ;; operation. Exercise the wait algorithm on every Lisp as well.
      (let ((attempts 0))
        (verify (eq t (threadcheck-wait-for-token
                       (lambda () (incf attempts) (>= attempts 3)) 1)))
        (verify (= attempts 3)))
      (let ((attempts 0))
        (verify (null (threadcheck-wait-for-token
                       (lambda () (incf attempts) nil) 0)))
        (verify (= attempts 1)))
      (verify (eq t (threadcheck-wait-for-token (lambda () :available) 0)))
      (let ((start (get-internal-real-time)))
        (verify (null (threadcheck-wait-for-token (lambda () nil) 1/50)))
        (verify (>= (- (get-internal-real-time) start)
                    (* 1/50 internal-time-units-per-second))))
      (format stream "~&threadcheck: ~D regression assertions passed~%" checks)
      t)))
