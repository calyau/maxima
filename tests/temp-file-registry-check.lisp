;;; Temporary-file registry and cleanup regressions.
(in-package :maxima)

(defun registry-test-isolated (thunk)
  ;; Replace the actual table, rather than a binding visible only to the
  ;; caller. CALL-IN-PARALLEL joins workers before this scope is restored.
  (let ((saved *temp-files-list*))
    (unwind-protect
         (progn
           (setq *temp-files-list* (make-hash-table :test 'equal :size 1))
           (funcall thunk))
      (setq *temp-files-list* saved))))

(defun registry-test-snapshot ()
  ;; The fallback characterizes the parent before the helper is introduced.
  (if (fboundp 'registered-temp-files)
      (funcall (symbol-function 'registered-temp-files))
      (loop for key being the hash-keys of *temp-files-list* collect key)))

(defun registry-test-lock-held-p ()
  (when (boundp '*temp-files-lock*)
    (let ((lock (symbol-value '*temp-files-lock*)))
      (declare (ignorable lock))
      #+sb-thread (sb-thread:holding-mutex-p lock)
      #+(and ccl openmcl-native-threads)
      (eq (ccl::%%lock-owner lock) ccl:*current-process*)
      #+(and ecl threads) (mp:holding-lock-p lock)
      #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
      nil)))

(defun $temp_registry_paths_check ()
  (registry-test-isolated
   (lambda ()
     (every
      (lambda (directory)
        (let ((*maxima-tempdir* directory))
          (every
           (lambda (file)
             (every
              (lambda (preserve)
                (clrhash *temp-files-list*)
                (let* ((key (if directory (format nil "~a/~a" directory file)
                                file))
                       (expected (format nil "~a" key))
                       (actual (plot-temp-file0 file preserve)))
                  (and (string= actual expected)
                       (= (hash-table-count *temp-files-list*)
                          (if preserve 0 1))
                       (or preserve (gethash key *temp-files-list*)))))
              '(nil t :keep)))
           (list "plain.gnuplot" "" "a b-ä.gnuplot" "a/b" nil 17
                 (pathname "path.gnuplot")))))
      '(nil "registry-test" "registry-test/")))))

(defun $temp_registry_duplicate_check ()
  (registry-test-isolated
   (lambda ()
     (let ((*maxima-tempdir* nil))
       (plot-temp-file0 (copy-seq "same"))
       (plot-temp-file0 (copy-seq "same"))
       (plot-temp-file0 "same" t)
       (let ((snapshot (registry-test-snapshot)))
         (when snapshot (setf (car snapshot) "changed-list"))
         (and (= (hash-table-count *temp-files-list*) 1)
              (gethash "same" *temp-files-list*)
              (equal (registry-test-snapshot) '("same"))))))))

(defun registry-test-prefix-p (names)
  (let ((seen (make-array '(32 128) :element-type 'bit :initial-element 0)))
    (and
     (loop for name in names always
           (and (stringp name) (eql (search "reg-" name) 0)
                (let* ((split (position #\- name :start 4))
                       (job (and split (parse-integer name :start 4 :end split)))
                       (i (and split (parse-integer name :start (1+ split)))))
                  (and job i (<= 0 job 31) (<= 0 i 127)
                       (zerop (aref seen job i))
                       (eql (setf (aref seen job i) 1) 1)))))
     (loop for job below 32 always
           (loop for i from 1 below 128 always
                 (or (zerop (aref seen job i))
                     (= (aref seen job (1- i)) 1)))))))

(defun $temp_registry_parallel_check (fallback-p)
  (registry-test-isolated
   (lambda ()
     (let (($parallel_threads (if fallback-p 1 4)))
       (and
        (every
         #'identity
         (call-in-parallel
          (append
           (loop for job below 32 collect
                 (let ((index job))
                   (lambda ()
                     (let ((*maxima-tempdir* nil))
                       (dotimes (i 128)
                         (plot-temp-file0 (format nil "reg-~D-~D" index i))))
                     t)))
           (loop repeat 4 collect
                 (lambda ()
                   (loop repeat 16 always
                         (registry-test-prefix-p (registry-test-snapshot))))))))
        (= (hash-table-count *temp-files-list*) 4096)
        (let ((names (registry-test-snapshot)))
          (and (= (length names) 4096) (registry-test-prefix-p names))))))))

(defun $temp_registry_cleanup_check (worker-p)
  (registry-test-isolated
   (lambda ()
     (let* ((directory (or *maxima-tempdir* (namestring (truename "."))))
            (prefix (format nil "~a/maxima-registry-~a-" directory
                            (random-name 24)))
            (first (concatenate 'string prefix "first"))
            (late (concatenate 'string prefix "late"))
            (keep (concatenate 'string prefix "keep"))
            (missing (concatenate 'string prefix "missing"))
            (original (symbol-function 'apparently-a-directory-p))
            (created nil) (seen nil) (outside-lock t)
            (*maxima-tempdir* nil))
       (unwind-protect
            (progn
              (dolist (name (list first late keep))
                (with-open-file (out name :direction :output
                                     :if-exists :error)
                  (push name created)
                  (write-line "owned registry test file" out)))
              (plot-temp-file0 first)
              (plot-temp-file0 keep t)
              (plot-temp-file0 missing)
              ;; PROBE-FILE rejects this key; cleanup retains its historical
              ;; IGNORE-ERRORS treatment of malformed/missing registry keys.
              (setf (gethash '(invalid pathname) *temp-files-list*) t)
              (setf (symbol-function 'apparently-a-directory-p)
                    (lambda (file)
                      (unless seen
                        (setq seen t)
                        (when (registry-test-lock-held-p)
                          (setq outside-lock nil)
                          (error "Cleanup must release the registry lock"))
                        (parallel-input-run
                         (lambda ()
                           (let ((*maxima-tempdir* nil))
                             (plot-temp-file0 late)))
                         (if (and worker-p (parallel-threads-p))
                             '$worker '$serial)))
                      (funcall original file)))
              (let ((result (delete-temp-files)))
                (setf (symbol-function 'apparently-a-directory-p) original)
                (and seen outside-lock (null result)
                     (not (probe-file first)) (probe-file late) (probe-file keep)
                     (= (hash-table-count *temp-files-list*) 4)
                     (null (delete-temp-files))
                     (not (probe-file late)) (probe-file keep) t)))
         (setf (symbol-function 'apparently-a-directory-p) original)
         (dolist (file created)
           (when (probe-file file) (delete-file file))))))))

(defun $temp_registry_empty_check ()
  (registry-test-isolated
   (lambda ()
     (and (null (registry-test-snapshot)) (null (delete-temp-files))
          (zerop (hash-table-count *temp-files-list*))))))

(defstruct (registry-test-name (:print-function registry-test-print)) thunk)

(defun registry-test-print (name stream print-depth)
  (declare (ignore print-depth))
  (funcall (registry-test-name-thunk name))
  (write-string "registry-object" stream))

(defun $temp_registry_print_check (worker-p error-p)
  (registry-test-isolated
   (lambda ()
     (let* ((*maxima-tempdir* nil)
            (seen nil) (failed nil) result
            (name
              (make-registry-test-name
               :thunk
               (lambda ()
                 (assert (not (registry-test-lock-held-p)))
                 (parallel-input-run
                  (lambda ()
                    (let ((*maxima-tempdir* nil)) (plot-temp-file0 "inner")))
                  (if (and worker-p (parallel-threads-p)) '$worker '$serial))
                 (setq seen t)
                 (when error-p (error "Controlled pathname printing error"))))))
       (handler-case (setq result (plot-temp-file0 name))
         (error () (setq failed t)))
       (and seen (eq failed error-p)
            (or failed (equal result "registry-object"))
            (gethash name *temp-files-list*) (gethash "inner" *temp-files-list*)
            (= (hash-table-count *temp-files-list*) 2)
            (not (registry-test-lock-held-p)))))))
