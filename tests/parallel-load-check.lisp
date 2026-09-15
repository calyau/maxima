;;; File initialization observations in an otherwise idle test process.
(in-package :maxima)

(defstruct loading-test-state
  lock (hits 0) (body-count 0) (sites-ok t) symbol kind positions missing-p)
(defvar *loading-test-state* nil)

(defun $parallel_load_test_initialize ()
  (let* ((state *loading-test-state*) (symbol (loading-test-state-symbol state)))
    (%with-lock ((loading-test-state-lock state))
      (incf (loading-test-state-hits state)))
    (case (loading-test-state-kind state)
      ($autom
       (setf (macro-function symbol)
             (lambda (form environment)
               (declare (ignore environment))
               (list '* (second form) (second form)))))
      ($auto_mspec
       (setf (get symbol 'mfexpr*) (lambda (form) (mul (second form) (second form)))))
      (($auto_mexpr $auto_mexpr_mac)
       (mputprop symbol '((lambda) ((mlist) $x) ((mtimes) $x $x)) 'mexpr))
      (otherwise (setf (symbol-function symbol) (lambda (x) (mul x x)))))
    '$done))

(defun loading-test-file (extension)
  (combine-path
   *maxima-testsdir*
   (concatenate 'string
                (if (loading-test-state-missing-p *loading-test-state*)
                    (concatenate 'string "missing-"
                                 (symbol-name (loading-test-state-symbol *loading-test-state*)))
                    "parallel-load-fixture")
                "." extension)))

(defun loading-test-stream (function)
  (with-input-from-string (input "parallel_load_test_initialize()$")
    (unwind-protect (funcall function input)
      (%with-lock ((loading-test-state-lock *loading-test-state*))
        (push (file-position input) (loading-test-state-positions *loading-test-state*))))))

(defun loading-test-prepare (kind symbol)
  (case kind
    (($autoload $translated $custom $translated_custom)
     (setf (get symbol 'autoload) (loading-test-file "lisp")))
    ($autof (autof symbol (loading-test-file "lisp")))
    ($autom (autom symbol (loading-test-file "lisp")))
    ($auto_mspec (auto-mspec symbol (loading-test-file "lisp")))
    ($auto_mexpr (auto-mexpr symbol (loading-test-file "lisp")))
    ($auto_mexpr_mac ($auto_mexpr symbol (loading-test-file "mac")))))

(defun loading-test-call (kind symbol)
  (case kind
    ($load_lisp ($load (loading-test-file "lisp")))
    ($load_maxima ($load (loading-test-file "mac")))
    ($loadfile (meval (list '($loadfile) (loading-test-file "lisp"))))
    ($batchload ($batchload (loading-test-file "mac")))
    ($batchload_stream (loading-test-stream #'$batchload))
    ($batchload_internal (loading-test-stream #'batchload-stream))
    ($batch (let ((*maxima-run-string* nil)) ($batch (loading-test-file "mac"))))
    ($batch_stream (let ((*maxima-run-string* nil)) (loading-test-stream #'$batch)))
    ($demo (let (($batch_answers_from_file nil) (*maxima-run-string* nil)
                 (*terminal-io* *query-io*))
             ($demo (loading-test-file "mac"))))
    ($aload (aload (loading-test-file "lisp")))
    ($aload_mac ($aload_mac (loading-test-file "mac")))
    ($generic (generic-autoload (cons symbol (loading-test-file "lisp"))))
    ($translated (mfunction-call-aux symbol '(5) nil))
    (($custom $translated_custom)
     (let ((autoload (lambda (entry) (declare (ignore entry)) ($parallel_load_test_initialize))))
       (if (eq kind '$custom)
           (load-function symbol t)
           (mfunction-call-aux symbol '(5) nil))))
    ($autom (macroexpand (list symbol 5)))
    (otherwise (meval (list (list symbol) 5)))))

(defun loading-test-operation (kind)
  (case kind
    ($loadfile '$loadfile)
    (($batchload $batchload_stream $batchload_internal $aload_mac $auto_mexpr_mac) '$batchload)
    (($batch $batch_stream $demo) '$batch)
    (otherwise '$load)))

(defun loading-test-thread ()
  #+sb-thread sb-thread:*current-thread*
  #+(and ccl openmcl-native-threads (not sb-thread)) ccl:*current-process*
  #+(and ecl threads (not sb-thread) (not ccl)) mp:*current-process*
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) nil)

(defun loading-test-site-p (mode parent-thread)
  (case mode
    ($public t)
    ($worker
     (if (parallel-threads-p)
         (not (eq parent-thread (loading-test-thread)))
         (eq parent-thread (loading-test-thread))))
    (otherwise (eq parent-thread (loading-test-thread)))))

(defun loading-test-results-p (result mode)
  (if (eq mode '$public)
      (and (= (length result) 8) (every #'identity result))
      result))

(defun loading-test-mode (mode)
  (if (and (eq mode '$worker) (not (parallel-threads-p))) '$fallback mode))

(defun loading-test-value (kind symbol value)
  (case kind
    ($autom (eval (macroexpand (list symbol value))))
    (($translated $translated_custom) (mfunction-call-aux symbol (list value) nil))
    (otherwise (meval (list (list symbol) value)))))

(defun loading-test-observe (kind symbol)
  ;; DEMO reads STANDARD-INPUT on SBCL and TERMINAL-IO elsewhere. Its
  ;; call binds TERMINAL-IO to the observer's private QUERY-IO stream.
  (parallel-input-observe (lambda () (loading-test-call kind symbol))
                          (format nil ";~%") (format nil ";~%")))

(defun loading-test-refused-p (observation kind)
  (and (null (first observation))
       (zerop (third observation)) (zerop (fourth observation))
       (equal (second observation)
              (list '(mlist simp)
                    (intl:gettext
                     "~M: this function cannot run in a parallel computation.")
                    (loading-test-operation kind)))))

(defun $parallel_loading_check (kind mode &optional initialized-p outer-mode missing-p)
  (let* ((old-state *loading-test-state*) (old-files *autoloaded-files*)
         (symbol (gensym "$LOAD-TEST-"))
         (origin-thread (loading-test-thread))
         (serial-p (and (eq mode '$serial)
                        (or (null outer-mode) (eq outer-mode '$serial))))
         (state (make-loading-test-state :symbol symbol :kind kind :missing-p missing-p
                                        :lock (%make-lock "file initialization"))))
    (unwind-protect
         (progn
           (setq *loading-test-state* state)
           (when missing-p
             (assert (not (probe-file (loading-test-file "lisp"))))
             (assert (not (probe-file (loading-test-file "mac")))))
           (loading-test-prepare kind symbol)
           (when initialized-p
             (unless (and (first (loading-test-observe kind symbol))
                          (= (loading-test-state-hits state) 1))
               (return-from $parallel_loading_check nil))
             (setf (loading-test-state-positions state) nil))
           (labels ((body (parent-thread)
                      (%with-lock ((loading-test-state-lock state))
                        (incf (loading-test-state-body-count state))
                        (unless (loading-test-site-p mode parent-thread)
                          (setf (loading-test-state-sites-ok state) nil)))
                      (if initialized-p
                          (loop for value in '(-5 0 7 129) always
                                (= (loading-test-value kind symbol value)
                                   (* value value)))
                          (let ((observation (loading-test-observe kind symbol)))
                            (if serial-p (not (null (first observation)))
                                (loading-test-refused-p observation kind)))))
                    (run-inner ()
                      (when (loading-test-site-p (or outer-mode '$serial) origin-thread)
                        (let ((parent-thread (loading-test-thread)))
                          (parallel-input-run (lambda () (body parent-thread))
                                              (loading-test-mode mode))))))
             (let* ((files-before *autoloaded-files*)
                    (result (parallel-input-run
                             #'run-inner
                             (if outer-mode (loading-test-mode outer-mode) '$serial))))
               (and (loading-test-results-p result mode)
                    (= (loading-test-state-body-count state) (if (eq mode '$public) 8 1))
                    (loading-test-state-sites-ok state)
                    (if (or initialized-p serial-p)
                        (and (= (loading-test-state-hits state) 1)
                             (= (loading-test-value kind symbol 5) 25)
                             (or (not initialized-p)
                                 (equal *autoloaded-files* files-before)))
                        (and (zerop (loading-test-state-hits state))
                             (every #'zerop (loading-test-state-positions state))
                             (equal *autoloaded-files* old-files)
                             ;; A rejected first use must still be loadable
                             ;; serially after all runner scopes have unwound.
                             (or missing-p
                                 (and (first (loading-test-observe kind symbol))
                                      (= (loading-test-state-hits state) 1)
                                      (= (loading-test-value kind symbol 5) 25)))))))))
      (setq *loading-test-state* old-state *autoloaded-files* old-files)
      (when (fboundp symbol) (fmakunbound symbol))
      (setf (symbol-plist symbol) nil))))

(defun $parallel_loading_symbolic_check (mode)
  (let ((symbol (gensym "$UNDEFINED-LOAD-"))
        (parent-thread (loading-test-thread))
        (body-count 0) (sites-ok t) (site-lock (%make-lock "symbolic loading probe")))
    (declare (ignorable site-lock))
    (let ((result
            (parallel-input-run
             (lambda ()
               (%with-lock (site-lock)
                 (incf body-count)
                 (unless (loading-test-site-p mode parent-thread) (setq sites-ok nil)))
               (alike1 (meval (list (list symbol) 5)) (list (list symbol) 5)))
             (loading-test-mode mode))))
      (and (loading-test-results-p result mode)
           (= body-count (if (eq mode '$public) 8 1)) sites-ok))))

(defun $parallel_loading_missing_check (kind mode)
  ($parallel_loading_check kind mode nil nil t))
