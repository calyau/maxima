;;;; Batch entry point for threadcheck.sh; also works in a VPATH build.

(in-package :maxima)

(defun run-threadcheck-and-exit (directory mode)
  "Run MODE's checks from the source tests DIRECTORY and exit with its status."
  (let ((status 2))
    ;; BYE in the cleanup also contains Maxima throws.  Ordinary Lisp load
    ;; errors must not fall back to Maxima's successful auto-continue path.
    (unwind-protect
         (handler-case
             (flet ((run (name)
                      ;; The checker is loaded below, after this form is
                      ;; compiled.  Resolve its functions only at run time.
                      (funcall (symbol-function name) *standard-output*)))
               (load (merge-pathnames
                      "../lisp-utils/thread-environment-check.lisp" directory))
               (cond
                 ((equal mode "bindings")
                  (let* ((leaked (run 'check-bindings))
                         (fresh (run 'check-fresh-linearray)))
                    (setf status (if (and (null leaked) fresh) 0 1)))
                  (when (zerop status)
                    (load (merge-pathnames "threadcheck-regression.lisp"
                                           directory))
                    (unless (run 'check-threadcheck-regressions)
                      (setf status 1))))
                 ((equal mode "race")
                  (setf status (case (run 'check-race)
                                 (:skipped 77)
                                 ((nil) 1)
                                 (otherwise 0))))
                 (t (error "Unknown threadcheck mode: ~S" mode)))
               (format t "~&threadcheck: ~A~%"
                       (case status (0 "PASS") (77 "SKIP") (otherwise "FAIL"))))
           (error (e)
             (setf status 2)
             (format *error-output* "~&threadcheck: ERROR: ~A~%" e)))
      (bye status))))

(run-threadcheck-and-exit
 (make-pathname :name nil :type nil :defaults *load-truename*)
 (maxima-getenv "MAXIMA_THREADCHECK_MODE"))
