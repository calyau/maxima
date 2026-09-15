;;; Dynamic execution-context checks for the parallel scheduler.
(in-package :maxima)

(defun context-test-active-p ()
  (and (boundp '*parallel-evaluation-p*)
       (symbol-value '*parallel-evaluation-p*)))

(defun context-test-mode (mode)
  (if (and (eq mode '$worker) (not (parallel-threads-p))) '$fallback mode))

(defun context-test-guard ()
  ;; The parent has no guard yet; characterization must reach the body there.
  (when (fboundp 'ensure-serial-execution)
    (funcall (symbol-function 'ensure-serial-execution) '$context_test_operation)))

(defun $parallel_context_site_check (mode outer)
  (progv '(*parallel-evaluation-p*) (list outer)
    (let* ((active (not (eq mode '$serial)))
           (actual (parallel-input-run #'context-test-active-p
                                       (context-test-mode mode))))
      (and (if (eq mode '$public)
               (equal actual (make-list 8 :initial-element t))
               (eq actual (if active t outer)))
           (eq (context-test-active-p) outer)))))

(defun $parallel_context_guard_check (mode clear-input-p)
  (progv '(*parallel-evaluation-p*) '(nil)
    (let* ((entered nil)
           (observation
             (parallel-input-observe
              (lambda ()
                (parallel-input-run
                 (lambda ()
                   (flet ((guarded-body ()
                            (context-test-guard)
                            (setq entered t)
                            17))
                     (if clear-input-p
                         (let ((*parallel-input-forbidden* nil)) (guarded-body))
                         (guarded-body))))
                 (context-test-mode mode)))
              "" "")))
      (and (not (context-test-active-p))
           (if (eq mode '$serial)
               (and entered (equal (first observation) '(17))
                    (null (second observation)))
               (and (not entered) (null (first observation))
                    (equal (second observation)
                           (list '(mlist simp)
                                 (intl:gettext
                                  "~M: this function cannot run in a parallel computation.")
                                 '$context_test_operation))))))))

(defun $parallel_context_capture_check (fallback-p)
  (progv '(*parallel-evaluation-p*) '(nil)
    (let (($parallel_threads (if fallback-p 1 4)))
      (and
       (equal
        (call-in-parallel (make-list 16 :initial-element #'context-test-active-p)
                          '(*parallel-evaluation-p*))
        (make-list 16 :initial-element t))
       (not (context-test-active-p))))))

(defun context-test-tree (remaining-depth square-input branch-count)
  (and (context-test-active-p)
       (if (zerop remaining-depth)
           (* square-input square-input)
           (let* ((inputs (loop for i below branch-count collect (+ square-input i)))
                  (results
                    (call-in-parallel
                     (mapcar (lambda (v)
                               (lambda ()
                                 (context-test-tree (1- remaining-depth)
                                                    v branch-count))) inputs))))
             (and (context-test-active-p)
                  (if (= remaining-depth 1)
                      (equal results (mapcar (lambda (v) (* v v)) inputs))
                      (every #'identity results)))))))

(defun $parallel_context_generated_check ()
  (let ((seed 39187))
    (loop repeat 32 always
          (progn
            (setq seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
            (progv '(*parallel-evaluation-p*) '(nil)
              (let (($parallel_threads (1+ (mod seed 4)))
                    (remaining-depth (mod (ash seed -8) 4))
                    (branch-count (1+ (mod (ash seed -12) 3)))
                    (square-input (- (mod (ash seed -16) 1000) 500)))
                (and (first (call-in-parallel
                             (list (lambda ()
                                     (context-test-tree remaining-depth
                                                        square-input branch-count)))))
                     (not (context-test-active-p)))))))))

(defun $parallel_context_unwind_check (throw-p outer)
  (progv '(*parallel-evaluation-p*) (list outer)
    (let ((original (symbol-function 'run-items)) (seen nil) result)
      (unwind-protect
           (progn
             (setf (symbol-function 'run-items)
                   (lambda (job &optional worker-p)
                     (declare (ignore job worker-p))
                     (setq seen (context-test-active-p))
                     (if throw-p (throw 'context-test-exit :escaped)
                         (error "Controlled runner failure"))))
             (setq result
                   (catch 'context-test-exit
                     (handler-case
                         (parallel-input-run (lambda () t) '$caller)
                       (error () :failed))))
             (and seen (eq result (if throw-p :escaped :failed))
                  (eq (context-test-active-p) outer)))
        (setf (symbol-function 'run-items) original)))))

(defun $parallel_context_empty_check ()
  (progv '(*parallel-evaluation-p*) '(nil)
    (and (null (call-in-parallel nil)) (not (context-test-active-p))
         (null (context-test-guard)))))

(defun $parallel_context_default_check ()
  (and (boundp '*parallel-evaluation-p*) (not (context-test-active-p))
       (null (context-test-guard))))
