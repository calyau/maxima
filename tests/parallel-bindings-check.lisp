(in-package :maxima)

#+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
(defun bindings-native-probe (kind &optional (root-bound t))
  (let* ((symbol (make-symbol "$SHARED-FORMAL"))
         (root 97) (lock (%make-lock "binding probe"))
         (entered (vector nil nil)) (checked (vector nil nil))
         (observed (vector nil nil)) (finished-b nil) (cancelled nil)
         (threads nil) (jobs nil)
         (hook (make-symbol "BINDINGS-NATIVE-POINT")))
    (when root-bound (setf (symbol-value symbol) root))
    (labels ((await-state (predicate)
               (let ((deadline (+ (get-internal-real-time)
                                  (* 10 internal-time-units-per-second))))
                 (loop
                   (when (%with-lock (lock) (funcall predicate)) (return t))
                   (when (or (%with-lock (lock) cancelled)
                             (> (get-internal-real-time) deadline))
                     (error "Binding probe handshake timed out."))
                   (sleep 0.001))))
             (point (role)
               (%with-lock (lock) (setf (aref entered role) t))
               (await-state (lambda () (every #'identity entered)))
               (%with-lock (lock)
                 (setf (aref observed role) (symbol-value symbol)
                       (aref checked role) t))
               (await-state (lambda () (every #'identity checked)))
               (when (zerop role) (await-state (lambda () finished-b)))
               (aref observed role))
             (job (role)
               (make-parallel-job
                :thunks
                (vector
                 (lambda ()
                   ;; Supply a private frame array even on pre-#34 builds.
                   ;; This probe isolates value-cell storage from frame races.
                   (let ((*mlambda-call-stack*
                           (make-array 30 :fill-pointer 0 :adjustable t)))
                     (cond ((eq kind '$list)
                            (mbinding ((list (list '(mlist) symbol))
                                       (list (list '(mlist) (+ 11 (* 12 role)))))
                              (point role)))
                           ((eq kind '$block)
                         (meval (list '(mprog)
                                      (list '(mlist) (list '(msetq) symbol (+ 11 (* 12 role))))
                                      (list (list hook) role))))
                           (t (mlambda
                          (list '(lambda) (list '(mlist) symbol)
                                (list (list hook) role))
                          (list (+ 11 (* 12 role)))
                          (make-symbol "$BINDING-PROBE") t nil))))))
                :results (vector nil) :errors (vector nil) :count 1
                :lock (%make-lock "binding probe job")
                :captured (capture-bindings (specials-to-bind nil)))))
      (unwind-protect
           (progn
             (setf (symbol-function hook) #'point)
             (let ((first-job (job 0)) (second-job (job 1)))
               (setq jobs (list first-job second-job))
               (push (%spawn (run-worker first-job) "binding A") threads)
               (await-state (lambda () (aref entered 0)))
               (push (%spawn
                      (lambda ()
                        (unwind-protect (funcall (run-worker second-job))
                          (%with-lock (lock) (setq finished-b t)))) "binding B") threads)
               (dolist (thread threads) (%join thread))
               (setq threads nil)
               (let ((valid (and (equalp observed #(11 23))
                                 (every (lambda (job) (not (aref (job-errors job) 0))) jobs)
                                 (eq (boundp symbol) root-bound)
                                 (or (not root-bound) (= (symbol-value symbol) root)))))
                 (format t "~&BINDING-NATIVE ~S OBSERVED=~S ROOT=~S VALID=~S~%"
                         kind observed
                         (and (boundp symbol) (symbol-value symbol)) valid)
                 valid)))
        (%with-lock (lock) (setq cancelled t finished-b t))
        (dolist (thread threads) (%join thread))
        (fmakunbound hook)))))


(defun bindings-test-run (thunk mode)
  (let ((actual (if (and (eq mode '$worker) (not (parallel-threads-p)))
                    '$fallback mode)))
    (parallel-input-run thunk actual)))

(defun $bindings_scope_check (mode kind root-bound)
  (let* ((symbol (make-symbol "$BINDINGS-SCOPE"))
         (hook (make-symbol "BINDINGS-SCOPE-HOOK"))
         ($values (copy-list $values)) ($myoptions (copy-list $myoptions))
         (before-values (copy-list $values))
         (seen nil) (result nil))
    (when root-bound (setf (symbol-value symbol) 97))
    (unwind-protect
         (progn
           (setf (symbol-function hook)
                 (lambda ()
                   (setq seen (and (= (symbol-value symbol) 31)
                                   (or root-bound (member symbol (cdr $values)))))
                   (symbol-value symbol)))
           (setq result
                 (bindings-test-run
                  (lambda ()
                    (case kind
                      ($list
                       (mbinding ((list (list '(mlist) symbol))
                                  (list (list '(mlist) 31)))
                         (funcall (symbol-function hook))))
                      ($block
                       (meval (list '(mprog)
                                    (list '(mlist) (list '(msetq) symbol 31))
                                    (list (list hook)))))
                      (otherwise
                       (mlambda (list '(lambda) (list '(mlist) symbol)
                                      (list (list hook)))
                                '(31) hook t nil)))) mode))
           (and seen (= result 31) (eq (boundp symbol) root-bound)
                (or (not root-bound) (= (symbol-value symbol) 97))
                (equal before-values $values)))
      (fmakunbound hook))))

(defun $bindings_native_check (kind root-bound)
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (and (parallel-threads-p) (bindings-native-probe kind root-bound))
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  ($bindings_scope_check '$fallback kind root-bound))

(defun $bindings_registry_check (mode action)
  (let* ((local (make-symbol "$REGISTRY-LOCAL"))
         (first (make-symbol "$REGISTRY-FIRST"))
         (second (make-symbol "$REGISTRY-SECOND"))
         (new (make-symbol "$REGISTRY-NEW"))
         (nested (make-symbol "$REGISTRY-NESTED"))
         (hook (make-symbol "REGISTRY-HOOK"))
         ($values (list '(mlist simp) first second))
         ($myoptions (list '(mlist simp)))
         (option (make-symbol "$REGISTRY-OPTION"))
         (observed nil))
    (setf (symbol-value first) 11 (symbol-value second) 13
          (symbol-value option) 17 (get option 'option) t)
    (unwind-protect
         (progn
           (setf (symbol-function hook)
                 (lambda ()
                   (setq observed (and (= (symbol-value local) 23)
                                       (member local (cdr $values))))
                   (mset new 29)
                   (case action
                     ($reorder (remvalue first '$remvalue) (mset first 37))
                     ($remove (remvalue second '$remvalue))
                     ($options (mset option 41))
                     ($nested (bindings-test-run (lambda () (mset nested 43)) '$worker)))
                   (when (eq action '$error) (error "Controlled registry exit"))
                   23))
           (handler-case
               (bindings-test-run
                (lambda ()
                  (mlambda (list '(lambda) (list '(mlist) local)
                                 (list (list hook))) '(23) hook t nil)) mode)
             (error () (unless (eq action '$error) (error "Unexpected registry error"))))
           (and observed (not (boundp local)) (= (symbol-value new) 29)
                (equal (cdr $values)
                       (case action ($reorder (list second new first))
                                    ($remove (list first new))
                                    ($nested (list first second new nested))
                                    (otherwise (list first second new))))
                (if (eq action '$options)
                    (and (= (symbol-value option) 41) (member option (cdr $myoptions)))
                    (null (cdr $myoptions)))
                t))
      (fmakunbound hook))))

(defun $bindings_nested_check (mode inner-mode kind)
  (let* ((local (make-symbol "$INHERITED-PARAMETER"))
         (hook (make-symbol "INHERITED-HOOK"))
         ($values (copy-list $values)) ($myoptions (copy-list $myoptions))
         (result nil))
    (setf (symbol-value local) 97)
    (unwind-protect
         (progn
           (setf (symbol-function hook)
                 (lambda ()
                   (bindings-test-run (lambda () (symbol-value local)) inner-mode)))
           (setf result
                 (bindings-test-run
                  (lambda ()
                    (if (eq kind '$block)
                        (meval (list '(mprog)
                                     (list '(mlist) (list '(msetq) local 47))
                                     (list (list hook))))
                        (mlambda (list '(lambda) (list '(mlist) local)
                                       (list (list hook))) '(47) hook t nil))) mode))
           (and (= result 47) (= (symbol-value local) 97)))
      (fmakunbound hook))))

(defun $bindings_character_check (mode scenario)
  (let* ((first (make-symbol "$CHARACTER-FIRST"))
         (second (make-symbol "$CHARACTER-SECOND"))
         (hook (make-symbol "BINDINGS-CHARACTER-HOOK"))
         ($values (copy-list $values)) ($myoptions (copy-list $myoptions))
         (events nil) (result nil))
    (setf (symbol-value first) 97 (symbol-value second) 89)
    (unwind-protect
         (progn
           (setf result
                 (bindings-test-run
                  (lambda ()
                    (let ((frames (fill-pointer *mlambda-call-stack*))
                          (saved-bindlist bindlist) (saved-mspeclist mspeclist))
                      (flet ((invoke (parameters arguments bodies &optional (noeval t))
                               (mlambda (list* '(lambda) (cons '(mlist) parameters) bodies)
                                        arguments hook noeval nil)))
                        (let ((value
                                (case scenario
                                  ($arguments
                                   (invoke (list first second)
                                           (list (list '(msetq) first (list '(mplus) first 1))
                                                 (list '(msetq) second (list '(mplus) second 1)))
                                           (list (list '(mlist) first second)) nil))
                                  ($duplicates (invoke (list first first) '(11 23) (list first)))
                                  ($rest (invoke (list (list '(mlist) first)) '(11 13 17) (list first)))
                                  ($quoted
                                   (let ((mfexprp t))
                                     (invoke (list (list '(mquote) first))
                                             (list (list '(mplus) 11 13)) (list first) nil)))
                                  ($multi (invoke (list first) '(31)
                                                  (list (list '(msetq) first 43) first)))
                                  (($assign $setter)
                                   (setf (get first (if (eq scenario '$assign) 'assign 'setter-method))
                                         (lambda (variable value)
                                           (push value events)
                                           (when (eq scenario '$setter)
                                             (setf (symbol-value variable) value))))
                                   (invoke (list first) '(31)
                                           (list (list '(msetq) first 43) first)))
                                  (($few $many $nobody $failure)
                                   (when (eq scenario '$failure)
                                     (setf (get second 'assign)
                                           (lambda (variable value)
                                             (declare (ignore variable value))
                                             (merror "Controlled second-binding error"))))
                                   (handler-case
                                       (with-$error
                                         (case scenario
                                           ($few (invoke (list first second) '(31) (list first)))
                                           ($many (invoke (list first) '(31 43) (list first)))
                                           ($nobody (invoke (list first) '(31) nil))
                                           ($failure (invoke (list first second) '(31 43) (list first))))
                                         :not-caught)
                                     (maxima-$error () :caught)))
                                  (($throw $body_error)
                                   (setf (symbol-function hook)
                                         (lambda ()
                                           (if (eq scenario '$throw)
                                               (throw 'bindings-character-exit :caught)
                                               (error "Controlled body error"))))
                                   (catch 'bindings-character-exit
                                     (handler-case
                                         (invoke (list first) '(31) (list (list (list hook))))
                                       (error () :caught))))
                                  ($nonlocal_return
                                   (block bindings-character-exit
                                     (mbinding ((list first) '(31))
                                       (return-from bindings-character-exit
                                         (symbol-value first)))))
                                  ($nonlocal_go
                                   (let ((observed nil))
                                     (tagbody
                                       (mbinding ((list first) '(31))
                                         (setq observed (symbol-value first))
                                         (go finished))
                                       (setq observed :wrong)
                                      finished)
                                     observed))
                                  ($multiple
                                   (multiple-value-list
                                    (mbinding ((list first) '(31))
                                      (values (symbol-value first) 43 47))))
                                  (otherwise (error "Unknown binding characterization")))))
                          (and (eql frames (fill-pointer *mlambda-call-stack*))
                               (eq saved-bindlist bindlist) (eq saved-mspeclist mspeclist)
                               (funcall (if (eq scenario '$multiple) #'equal #'alike1) value
                                       (case scenario
                                         ($arguments '((mlist) 98 90))
                                         ($duplicates 23)
                                         ($rest '((mlist) 11 13 17))
                                         ($quoted 24)
                                         (($multi $assign $setter) 43)
                                         (($few $many $nobody $failure $throw $body_error) :caught)
                                         (($nonlocal_return $nonlocal_go) 31)
                                         ($multiple '(31 43 47))))))))) mode))
           (and result
                (= (symbol-value first) (if (eq scenario '$arguments) 98 97))
                (= (symbol-value second) (if (eq scenario '$arguments) 90 89))
                (or (not (member scenario '($assign $setter)))
                    (equal events '(97 43 31)))))
      (fmakunbound hook))))

(defun $bindings_symbol_macro_check (mode)
  (let* ((symbol (make-symbol "BINDINGS-SYMBOL-MACRO"))
         (calls 0) (result nil))
    (eval `(define-symbol-macro ,symbol 42))
    (setf (symbol-value symbol) 97)
    (setf result
          (bindings-test-run
           (lambda ()
             (let ((*macroexpand-hook*
                     (lambda (expander form environment)
                       (incf calls) (funcall expander form environment))))
               (mlambda (list '(lambda) (list '(mlist) symbol) symbol)
                        '(31) (make-symbol "$SYMBOL-MACRO-CHECK") t nil))) mode))
    (and (= result 31) (= (symbol-value symbol) 97) (zerop calls))))

(defun $bindings_registry_generated_check (mode)
  ;; Independent append/delete model over 128 deterministic operation streams.
  ;; The model never consults the runner or registry reconciliation helpers.
  (loop with state = 75023 for trial below 128 always
        (let* ((symbols (loop repeat 8 collect (make-symbol "$GENERATED-VALUE")))
               ($values (cons '(mlist simp) (subseq symbols 0 4)))
               ($myoptions (list '(mlist simp)))
               (expected (copy-list (cdr $values))) (operations nil))
          (loop for item in (cdr $values) do (setf (symbol-value item) 11))
          (dotimes (step 20)
            (setf state (mod (+ (* state 1664525) 1013904223) (ash 1 32)))
            (let* ((item (nth (mod (ash state -8) 8) symbols))
                   (assign (logbitp 18 state)))
              (push (list item assign) operations)
              (if assign
                  (unless (member item expected) (setf expected (append expected (list item))))
                  (setf expected (remove item expected)))))
          (bindings-test-run
           (lambda ()
             (dolist (operation (reverse operations))
               (if (second operation)
                   (mset (first operation) 23)
                   (when (boundp (first operation))
                     (remvalue (first operation) '$remvalue))))) mode)
          (and (equal (cdr $values) expected)
               (every (lambda (item) (eq (boundp item) (not (null (member item expected))))) symbols)))))

#+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
(defun bindings-native-registry-probe ()
      (let* ((test-left-symbol (make-symbol "$NATIVE-LEFT")) (test-right-symbol (make-symbol "$NATIVE-RIGHT"))
             (added (vector (make-symbol "$NATIVE-ADDED-A") (make-symbol "$NATIVE-ADDED-B")))
             ($values (list '(mlist simp) test-left-symbol test-right-symbol))
             ($myoptions (list '(mlist simp)))
             (lock (%make-lock "parallel registry test"))
             (entered (vector nil nil)) (changed (vector nil nil))
             (observed (vector nil nil)) (cancelled nil) (threads nil) (jobs nil))
        (setf (symbol-value test-left-symbol) 11 (symbol-value test-right-symbol) 13)
        (labels ((await-state (bits)
                   (let ((deadline (+ (get-internal-real-time)
                                      (* 10 internal-time-units-per-second))))
                     (loop
                       (when (%with-lock (lock) (every #'identity bits)) (return t))
                       (when (or (%with-lock (lock) cancelled)
                                 (> (get-internal-real-time) deadline))
                         (error "Registry barrier timed out"))
                       (sleep 0.001))))
                 (make-job (role)
                   (make-parallel-job
                    :thunks (vector (lambda ()
                                      (%with-lock (lock) (setf (aref entered role) t))
                                      (await-state entered)
                                      (mset (aref added role) (+ 23 role))
                                      (remvalue (if (zerop role) test-left-symbol test-right-symbol) '$remvalue)
                                      (%with-lock (lock) (setf (aref changed role) t))
                                      (await-state changed)
                                      (setf (aref observed role)
                                            (equal (cdr $values)
                                                   (list (if (zerop role) test-right-symbol test-left-symbol)
                                                         (aref added role))))))
                    :results (vector nil) :errors (vector nil) :count 1
                    :lock (%make-lock "registry test job")
                    :captured (capture-bindings (specials-to-bind nil)))))
          (unwind-protect
               (progn
                 (dotimes (role 2)
                   (let ((job (make-job role)))
                     (push job jobs)
                     (push (%spawn (run-worker job) "registry test worker") threads)))
                 (dolist (thread threads) (%join thread))
                 (setq threads nil)
                 (and (every #'identity observed)
                      (every (lambda (job) (not (aref (job-errors job) 0))) jobs)
                      (not (boundp test-left-symbol)) (not (boundp test-right-symbol))
                      (= (symbol-value (aref added 0)) 23)
                      (= (symbol-value (aref added 1)) 24)
                      (= (length (cdr $values)) 2)
                      (every (lambda (item) (member item (cdr $values))) added)
                      t))
            (%with-lock (lock) (setq cancelled t))
            (dolist (thread threads) (%join thread))))))

(defun $bindings_registry_native_check ()
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (and (parallel-threads-p) (bindings-native-registry-probe))
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  ($bindings_registry_generated_check '$fallback))

(defun $bindings_target_check (mode custom-list-p)
  (let* ((symbol (make-symbol "$CUSTOM-LOCAL"))
         (operator (if custom-list-p 'mlist (make-symbol "$CUSTOM-TARGET")))
         (saved (get operator 'mset_extension_operator))
         (setter-calls 0) (result nil)
         (target (list (list operator) symbol)))
    (setf (symbol-value symbol) 97)
    (unwind-protect
         (progn
           (setf (get operator 'mset_extension_operator)
                 (lambda (place value)
                   (declare (ignore place))
                   (incf setter-calls)
                   (setf (symbol-value symbol) (if (consp value) (second value) value))))
           (setf result
                 (bindings-test-run
                  (lambda ()
                    (handler-case
                        (with-$error
                          (mbinding ((list target) '(31)) (symbol-value symbol)))
                      (maxima-$error () :refused))) mode))
           (and (= (symbol-value symbol) 97)
                (if (eq mode '$serial)
                    (and (eql result 31) (= setter-calls 2))
                    (and (eq result :refused) (zerop setter-calls)))))
      (if saved (setf (get operator 'mset_extension_operator) saved)
          (remprop operator 'mset_extension_operator)))))

(defun $bindings_list_check (mode)
  (let ((first (make-symbol "$LIST-LOCAL-A"))
        (second (make-symbol "$LIST-LOCAL-B"))
        ($values (copy-list $values)) ($myoptions (copy-list $myoptions)))
    (setf (symbol-value first) 97 (symbol-value second) 89)
    (and
     (bindings-test-run
      (lambda ()
        (let* ((target (list '(mlist) first (list '(mlist) second first)))
               (snapshot (copy-tree target)))
          (mbinding ((list target) (list '((mlist) 11 ((mlist) 13 17))))
            (and (= (symbol-value first) 17) (= (symbol-value second) 13)
                 (equal target snapshot))))) mode)
     (= (symbol-value first) 97) (= (symbol-value second) 89))))

(defun $bindings_captured_nested_check (outer-mode inner-mode)
  (let* ((symbol (make-symbol "$CAPTURED-OUTER-INDEX"))
         ($values (copy-list $values)) ($myoptions (copy-list $myoptions)))
    (setf (symbol-value symbol) 97)
    (let ((job (make-parallel-job
                :thunks (vector (lambda ()
                                  (mset symbol 47)
                                  (bindings-test-run (lambda () (symbol-value symbol)) inner-mode)))
                :results (vector nil) :errors (vector nil) :count 1
                :lock (%make-lock "captured index test")
                :captured (capture-bindings (specials-to-bind (list symbol))))))
      (if (and (eq outer-mode '$worker) (parallel-threads-p))
          (%join (%spawn (run-worker job) "captured index test worker"))
          (call-as-runner job nil))
      (and (not (aref (job-errors job) 0))
           (eql (aref (job-results job) 0) 47) (= (symbol-value symbol) 97)))))

(defun $bindings_invalid_check (mode)
  (let ((constant (make-symbol "BINDINGS-CONSTANT")))
    (eval `(defconstant ,constant 97))
    (every
     (lambda (target)
       (flet ((probe ()
                (let ((saved-bindlist bindlist) (saved-mspeclist mspeclist))
                  (let ((condition
                          (handler-case
                              (with-$error (mbinding ((list target) '(31)) :accepted))
                            (error (condition) (type-of condition)))))
                    (and (eq saved-bindlist bindlist) (eq saved-mspeclist mspeclist)
                         (not (eq condition :accepted)) condition)))))
         (let ((serial (probe)))
           (and serial (eq serial (bindings-test-run #'probe mode))))))
     (list nil t :bindings-constant 17 "invalid local" constant))))

(defun $bindings_debugger_check (mode root-bound)
  (let* ((symbol (make-symbol "$DEBUGGER-LOCAL"))
         (hook (make-symbol "DEBUGGER-LOCAL-HOOK"))
         ($values (copy-list $values)) ($myoptions (copy-list $myoptions)))
    (when root-bound (setf (symbol-value symbol) 97))
    (unwind-protect
         (bindings-test-run
          (lambda ()
            (let ((initial-bindlist bindlist)
                  (*diff-bindlist* nil) (*diff-mspeclist* nil))
              (setf (symbol-function hook)
                    (lambda ()
                      (remove-bindings initial-bindlist)
                      (let ((outer-correct (and (eq (boundp symbol) root-bound)
                                                (or (not root-bound) (= (symbol-value symbol) 97)))))
                        (restore-bindings)
                        (and outer-correct (= (symbol-value symbol) 31)))))
              (and (mlambda (list '(lambda) (list '(mlist) symbol)
                                   (list (list hook))) '(31) hook t nil)
                   (eq (boundp symbol) root-bound)
                   (or (not root-bound) (= (symbol-value symbol) 97))))) mode)
      (fmakunbound hook))))

(defun $bindings_captured_registry_check (mode remove-p)
  (let* ((symbol (make-symbol "$CAPTURED-REGISTRY-ENTRY"))
         ($values (if remove-p (list '(mlist simp) symbol) (list '(mlist simp))))
         ($myoptions (list '(mlist simp)))
         (original (copy-list $values)))
    (when remove-p (setf (symbol-value symbol) 97))
    (let ((job (make-parallel-job
                :thunks (vector (lambda ()
                                  (mset symbol 31)
                                  (if remove-p
                                      (progn (remvalue symbol '$remvalue) (not (boundp symbol)))
                                      (and (= (symbol-value symbol) 31) (member symbol (cdr $values))))))
                :results (vector nil) :errors (vector nil) :count 1
                :lock (%make-lock "captured registry test")
                :captured (capture-bindings (specials-to-bind (list symbol))))))
      (if (and (eq mode '$worker) (parallel-threads-p))
          (%join (%spawn (run-worker job) "captured registry test worker"))
          (call-as-runner job nil))
      (and (not (aref (job-errors job) 0)) (aref (job-results job) 0)
           (eq (boundp symbol) remove-p)
           (or (not remove-p) (= (symbol-value symbol) 97))
           (equal original $values)))))

(defun $bindings_registry_unwind_check (mode)
  (let* ((removed (make-symbol "$UNWIND-REMOVED"))
         (added (make-symbol "$UNWIND-ADDED"))
         (option (make-symbol "$UNWIND-OPTION"))
         ($values (list '(mlist simp) removed))
         ($myoptions (list '(mlist simp)))
         (observed nil))
    (setf (symbol-value removed) 11 (symbol-value option) 17
          (get option 'option) t)
    (let* ((thunk (lambda ()
                    (mset added 29)
                    (remvalue removed '$remvalue)
                    (mset option 41)
                    ;; Leave RUN-ONE-ITEM without its error handler converting
                    ;; the exit into a normally returning runner.
                    (throw 'bindings-registry-exit :escaped)))
           (job (make-parallel-job
                 :thunks (vector thunk) :results (vector nil)
                 :errors (vector nil) :count 1
                 :lock (%make-lock "registry unwind test")
                 :captured (capture-bindings (specials-to-bind nil)))))
      (labels ((invoke ()
                 (catch 'bindings-registry-exit
                   (case mode
                     ($serial (call-with-private-parallel-registries nil thunk))
                     ($fallback
                      (let (($parallel_threads 1)) (call-in-parallel (list thunk))))
                     ($worker (funcall (run-worker job)))
                     (otherwise (call-as-runner job nil))))))
        (if (eq mode '$worker)
            #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
            (%join (%spawn (lambda () (setf observed (invoke))) "registry unwind test"))
            #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
            (setf observed (invoke))
            (setf observed (invoke)))))
    (and (eq observed :escaped) (not (boundp removed))
         (= (symbol-value added) 29) (= (symbol-value option) 41)
         (equal (cdr $values) (list added))
         (equal (cdr $myoptions) (list option)))))
