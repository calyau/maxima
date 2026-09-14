;;; Database query and context concurrency regressions, in an idle process.
(in-package :maxima)

(defun parallel-database-job (thunk)
  (make-parallel-job :thunks (vector thunk) :results (vector nil)
                     :errors (vector nil) :count 1
                     :lock (%make-lock "database test job")
                     :captured (capture-bindings (specials-to-bind nil))))

(defun parallel-database-job-value (job)
  (when (aref (job-errors job) 0)
    (setq $error (aref (job-errors job) 0))
    (error 'maxima-$error))
  (aref (job-results job) 0))

(defun parallel-database-join-workers (threads)
  (declare (ignorable threads))
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (let ((first-error nil))
    (dolist (thread threads)
      (handler-case (%join thread)
        (error (condition)
          (unless first-error (setq first-error condition)))))
    (when first-error (error first-error))))

(defun parallel-database-pair (left right native-p)
  ;; Separate one-item jobs guarantee both bodies run in actual workers.
  (let ((jobs (list (parallel-database-job left)
                    (parallel-database-job right)))
        (threads nil))
    (declare (ignorable threads))
    (unwind-protect
         (if (and native-p (parallel-threads-p))
             #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
             (dolist (job jobs)
               (push (%spawn (run-worker job) "database query test") threads))
             #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
             (error "Unavailable native workers")
             (dolist (job jobs) (call-as-runner job nil)))
      (parallel-database-join-workers threads))
    (mapcar #'parallel-database-job-value jobs)))

(defun $parallel_database_queries (native-p)
  (let ((probe (gensym "$DATABASE_QUERY")))
    (with-new-context ()
      (mkind probe '$real)
      (mdata t 'mgrp probe 0)
      (let ((checks
             (list (lambda () (kindp probe '$real))
                   (lambda () (eq (kind-any-of probe '($real $imaginary)) '$real))
                   (lambda () (kind-all-of-p probe '($real)))
                   (lambda () (null (decl-complex-kind probe)))
                   (lambda () (truep (list 'kind probe '$real)))
                   (lambda () (null (falsep (list 'kind probe '$real))))
                   (lambda () (eq (isp (list 'kind probe '$real)) t))
                   (lambda () (eq (dcomp probe 0) '$pos)))))
        ;; Fixed inputs with independent expected relations, varied order.
        (labels ((check-sequence (seed)
                   (loop with state = seed repeat 256 always
                         (progn
                           (setq state (mod (+ (* state 1664525) 1013904223)
                                            (expt 2 32)))
                           (funcall (nth (mod (ash state -16) 8) checks))))))
          (every #'identity
                 (parallel-database-pair
                  (lambda () (check-sequence 17011))
                  (lambda () (check-sequence 81727)) native-p)))))))

(defun $parallel_database_contexts (native-p)
  (let ((probe (gensym "$DATABASE_LOCAL"))
        (inherited (gensym "$DATABASE_INHERITED")))
    (with-new-context ()
      (mdata t 'mgrp inherited 0)
      (labels ((query-in-context (relation expected)
                 (with-new-context ()
                   (mdata t relation probe 0)
                   (loop repeat 128 always
                         (and (eq (dcomp probe 0) expected)
                              (eq (dcomp inherited 0) '$pos))))))
        (and (every #'identity
                    (parallel-database-pair
                     (lambda () (query-in-context 'mgrp '$pos))
                     (lambda () (query-in-context 'meqp '$zero)) native-p))
             ;; The temporary contexts leave neither facts nor broken marks.
             (eq (dcomp probe 0) '$pnz)
             (eq (dcomp inherited 0) '$pos))))))

(defvar *parallel-database-test-role* nil)

(defun $parallel_database_overlap (&optional (query-kind :kindp))
  (unless (parallel-threads-p)
    (return-from $parallel_database_overlap '$skipped))
  (let* ((probe (gensym "$DATABASE_OVERLAP"))
         (gate (%make-lock "database overlap gate"))
         (first-paused nil) (release-first nil) (second-ready nil)
         (overlapped nil)
         (old-beg (symbol-function 'beg))
         (old-clear (symbol-function 'clear))
         (old-dmark (symbol-function 'dmark))
         (threads nil)
         (jobs nil))
    (declare (ignorable threads gate second-ready))
    (labels ((wait-for (predicate)
               (let ((deadline (+ (get-internal-real-time)
                                  (* 10 internal-time-units-per-second))))
                 (loop until (funcall predicate) do
                       (when (> (get-internal-real-time) deadline)
                         (error "Database regression gate timed out"))
                       (sleep 0.001)))))
      (with-new-context ()
        (mkind probe '$real)
        (mdata t 'mgrp probe 0)
        (unwind-protect
             (progn
               (flet ((pause-query (node)
                        (when (and (eq *parallel-database-test-role* :first)
                                   (eq node probe))
                          (%with-lock (gate) (setq first-paused t))
                          (wait-for (lambda () (%with-lock (gate) release-first)))
                          (%with-lock (gate) (setq first-paused nil)))))
                 (setf (symbol-function 'beg)
                       (lambda (node label)
                         (prog1 (funcall old-beg node label)
                           (unless (eq query-kind :dcomp) (pause-query node)))))
                 (setf (symbol-function 'dmark)
                       (lambda (node label)
                         (prog1 (funcall old-dmark node label)
                           (when (eq query-kind :dcomp) (pause-query node))))))
               (setf (symbol-function 'clear)
                     (lambda ()
                       (when (eq *parallel-database-test-role* :second)
                         (%with-lock (gate)
                           (when first-paused (setq overlapped t))))
                       (funcall old-clear)))
               (let ((first-job
                       (parallel-database-job
                        (lambda ()
                          (let ((*parallel-database-test-role* :first))
                            (ecase query-kind
                              (:kindp (kindp probe '$real))
                              (:kind-any (eq (kind-any-of probe '($real)) '$real))
                              (:kind-all (kind-all-of-p probe '($real)))
                              (:complex-kind (null (decl-complex-kind probe)))
                              (:truep (truep (list 'kind probe '$real)))
                              (:falsep (null (falsep (list 'kind probe '$real))))
                              (:isp (eq (isp (list 'kind probe '$real)) t))
                              (:dcomp (eq (dcomp probe 0) '$pos)))))))
                     (second-job
                       (parallel-database-job
                        (lambda ()
                          (let ((*parallel-database-test-role* :second))
                            (%with-lock (gate) (setq second-ready t))
                            (if (eq query-kind :dcomp)
                                (kindp probe '$real)
                                (eq (dcomp probe 0) '$pos)))))))
                 (setq jobs (list first-job second-job))
                 #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
                 (progn
                   (push (%spawn (run-worker first-job) "database paused query") threads)
                   (wait-for (lambda () (%with-lock (gate) first-paused)))
                   (push (%spawn (run-worker second-job) "database competing query") threads)
                   (wait-for (lambda () (%with-lock (gate) second-ready)))
                   ;; Give the competitor a chance to reach CLEAR while the
                   ;; first query is paused. Timing can miss the old race,
                   ;; but cannot make a correctly serialized query overlap.
                   (sleep 0.05))))
          (unwind-protect
               (progn
                 (%with-lock (gate) (setq release-first t))
                 (parallel-database-join-workers threads))
            (setf (symbol-function 'beg) old-beg
                  (symbol-function 'clear) old-clear
                  (symbol-function 'dmark) old-dmark)))
        (and (not overlapped)
             (every #'parallel-database-job-value jobs))))))

(defun $parallel_database_overlap_all ()
  (every #'$parallel_database_overlap
         '(:kindp :kind-any :kind-all :complex-kind :truep :falsep :isp :dcomp)))

(defun parallel-database-rational (value)
  (if (integerp value) value
      (list '(rat simp) (numerator value) (denominator value))))

(defun $parallel_database_numbers (native-p)
  ;; Each context inserts a rational node and is then collected. The other
  ;; worker may still be querying the same number through its own live facts.
  (labels ((check-numbers (seed)
             (let ((probe (gensym "$DATABASE_NUMBER")))
               (loop with state = seed repeat 96 always
                     (progn
                       (setq state (mod (+ (* state 1664525) 1013904223)
                                        (expt 2 32)))
                       (let* ((value (/ (* (- state (expt 2 31))
                                          (expt 10 (mod (ash state -8) 30)))
                                       (1+ (mod state 17))))
                              (query (+ value (- (mod (ash state -16) 3) 1)))
                              (expected (cond ((> value query) '$pos)
                                              ((< value query) '$neg)
                                              (t '$zero))))
                         (with-new-context ()
                           (mdata t 'meqp probe (parallel-database-rational value))
                           (eq (dcomp probe (parallel-database-rational query))
                               expected))))))))
    (every #'identity
           (parallel-database-pair (lambda () (check-numbers 1759))
                                   (lambda () (check-numbers 1759)) native-p))))

(defun $parallel_database_callback (native-p)
  (let ((operator (gensym "$DATABASE_LEARN_CALLBACK"))
        (probe (gensym "$DATABASE_CALLBACK_VALUE")))
    (with-new-context ()
      (mdata t 'mgrp probe 0)
      (unwind-protect
           (progn
             (setf (get operator 'learn)
                   (lambda (pattern)
                     (declare (ignore pattern))
                     (parallel-database-pair
                      (lambda () (eq (dcomp probe 0) '$pos))
                      (lambda () (eq (dcomp probe 0) '$pos)) native-p)))
             ;; A transaction around LEARN itself would wait for workers
             ;; which cannot enter their own database transactions.
             (equal (learn (list (list operator) probe) t) '(t t)))
        (remprop operator 'learn)))))

(defun $parallel_database_unwind (native-p)
  (let ((probe (gensym "$DATABASE_UNWIND"))
        (old-beg (symbol-function 'beg)))
    (with-new-context ()
      (mkind probe '$real)
      (unwind-protect
           (progn
             (setf (symbol-function 'beg)
                   (lambda (node label)
                     (prog1 (funcall old-beg node label)
                       (when (eq node probe)
                         (case *parallel-database-test-role*
                           (:error (error "database-injected-query-error"))
                           (:throw (throw 'database-query-escape :escaped)))))))
             (every #'identity
                    (parallel-database-pair
                     (lambda ()
                       (let ((caught
                               (handler-case
                                   (let ((*parallel-database-test-role* :error))
                                     (kindp probe '$real))
                                 (error (condition)
                                   (and (search "database-injected-query-error"
                                                (princ-to-string condition)) :caught)))))
                         (and (eq caught :caught) (kindp probe '$real))))
                     (lambda ()
                       (and (eq (catch 'database-query-escape
                                  (let ((*parallel-database-test-role* :throw))
                                    (kindp probe '$real)))
                                :escaped)
                            (kindp probe '$real))) native-p)))
        (setf (symbol-function 'beg) old-beg)))))

(defun $parallel_database_edges ()
  ;; Characterize old database entry points and verify that failed context
  ;; operations leave subsequent queries usable. All symbols are private.
  (let ((probe (gensym "$DATABASE_EDGE"))
        (other (gensym "$DATABASE_EDGE_OTHER"))
        (kind-name (gensym "$DATABASE_EDGE_KIND"))
        (missing (gensym "$DATABASE_MISSING"))
        ($error nil) ($errormsg nil))
    (with-new-context ()
      (let ((here $context))
        (and (eq (truep t) t)
             (eq (isp (list 'kind missing kind-name)) 'unknown)
             (progn (true* (list 'kind probe kind-name))
                    (kindp probe kind-name))
             (progn (true* (list 'mgrp other 0))
                    (eq (dcomp other 0) '$pos))
             (progn (par (list probe other) missing)
                    (find 'par (get probe 'data) :key #'caar))
             ;; Debug output must not interfere with query results.
             (let ((dbtrace t) (*trace-output* (make-broadcast-stream)))
               (kindp probe kind-name))
             (null (errcatch ($newcontext probe other)))
             (null (errcatch ($newcontext 3)))
             (null ($newcontext here))
             (null (errcatch ($supcontext probe other missing)))
             (null (errcatch ($supcontext 3)))
             (null (errcatch ($supcontext here)))
             (null (errcatch ($supcontext missing other)))
             ;; The low-level API also accepts a context handle cons.
             (let* ((node (gensym "$DATABASE_HANDLE"))
                    (datum (datum (list 'kind node kind-name))))
               (cntxt datum (cons nil here))
               (assert (eq (getf (cdr datum) 'con) here))
               (kcntxt (car datum) (cons nil here))
               (not (member datum (get here 'data))))
             ;; A number's equal value may have a different representation.
             (progn (mdata t 'mgrp probe 3)
                    (mdata t 'mgrp probe 1)
                    (mdata t 'meqp other '((rat simp) 5 4))
                    (dintnum 1.25)
                    (eq (dcomp other 1.25) '$zero))
             (kindp probe kind-name)
             t)))))

(defun $parallel_database_gc_edges ()
  ;; Legacy context GC compacts an array of live handles. Give it private
  ;; arrays and fresh context symbols so the session's arrays stay intact.
  (let ((live (gensym "$DATABASE_GC_LIVE"))
        (dead (gensym "$DATABASE_GC_DEAD"))
        (probe (gensym "$DATABASE_GC_VALUE"))
        (kind-name (gensym "$DATABASE_GC_KIND"))
        ($error nil) ($errormsg nil))
    (with-new-context ()
      (unwind-protect
           (progn
             (let ((context dead)) (mkind probe kind-name))
             (let ((*connumber* 2) (*conindex* 0)
                   (conmark (vector live dead nil))
                   (conunmrk (vector (cons nil live) nil nil)))
               (gccon)
               (assert (= *conindex* 2))
               (assert (eq (aref conmark 0) live))
               (assert (null (aref conmark 1)))
               (assert (null (get probe 'data)))
               (assert (null (get dead 'data))))
             ;; With no spare slot and all handles live, GC must report
             ;; exhaustion; the temporary counter bindings still unwind.
             (and (let ((*connumber* 0) (*conindex* 0)
                        (conmark (vector live))
                        (conunmrk (vector (cons nil live))))
                    (null (errcatch (gccon))))
                  (progn (killc nil) t)
                  (not (kindp probe kind-name))))
        (killc dead)
        (killc live)))))
