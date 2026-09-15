;;; Current-binding removal, including ECL's separate global value cell.
(in-package :maxima)

(defun unbinding-test-remove (symbol path)
  (ecase path
    (:primitive (maxima-makunbound symbol))
    (:mbind
     (let ((bindlist nil) (mspeclist nil)
           ($values (copy-list $values)) ($myoptions (copy-list $myoptions)))
       (mbinding ((list symbol) '(31))
         (assert (= (symbol-value symbol) 31)))
       symbol))
    (:debugger
     (let ((bindlist nil) (mspeclist nil)
           (*diff-bindlist* nil) (*diff-mspeclist* nil)
           ($values (copy-list $values)) ($myoptions (copy-list $myoptions)))
       (mbind (list symbol) '(31) nil)
       (remove-bindings nil)
       (assert (not (boundp symbol)))
       (restore-bindings)
       (assert (= (symbol-value symbol) 31))
       (munbind (list symbol))
       (assert (null bindlist)) (assert (null mspeclist))
       symbol))
    ((:known :reset :unknown :labels)
     (let (($values (list '(mlist simp))) ($labels (list '(mlist simp)))
           ($linenum 97) (*standard-output* (make-broadcast-stream)))
       (setf (symbol-value symbol) 31)
       (case path
         (:known (push symbol (cdr $values)))
         (:reset (setf (get symbol 'reset-on-kill) t))
         (:labels (push symbol (cdr $labels))))
       (if (eq path :labels) (kill1 '$labels) (remvalue symbol '$remvalue))
       (assert (not (boundp symbol)))
       symbol))
    (:captured
     (assert (call-with-captured-bindings
              (list (list symbol nil nil))
              (lambda () (not (boundp symbol)))))
     symbol)))

(defun unbinding-test-frame (path outer-bound inner-bound exit-mode)
  (let* ((symbol (make-symbol "$UNBINDING-CHECK"))
         (root (list :global-value))
         (outer (list :outer-value))
         (observed nil))
    (setf (symbol-value symbol) root)
    (and
     (progv (list symbol) (if outer-bound (list outer) nil)
       (let ((result
               (catch 'unbinding-test-exit
                 (handler-case
                     (progv (list symbol) (if inner-bound (list 73) nil)
                       (setq observed
                             (and (eq (unbinding-test-remove symbol path) symbol)
                                  (if (member path '(:mbind :captured :debugger))
                                      (and (eq (boundp symbol) inner-bound)
                                           (or (not inner-bound) (= (symbol-value symbol) 73)))
                                      (not (boundp symbol)))))
                       (ecase exit-mode
                         (:normal (values 17 29))
                         (:throw (throw 'unbinding-test-exit :escaped))
                         (:error (error "unbinding-test: controlled error"))))
                   (error () :caught)))))
         (and observed
              (eql result (ecase exit-mode (:normal 17) (:throw :escaped) (:error :caught)))
              (eq (boundp symbol) outer-bound)
              (or (not outer-bound) (eq (symbol-value symbol) outer)))))
     (and (boundp symbol) (eq (symbol-value symbol) root)))))

(defun $unbinding_scope_check (mode path)
  (let* ((path (ecase path ($primitive :primitive) ($mbind :mbind) ($captured :captured) ($debugger :debugger)
                            ($known :known) ($reset :reset) ($unknown :unknown) ($labels :labels)))
         (thunk
           (lambda ()
             (loop for outer-bound in '(nil t) always
                   (loop for inner-bound in (if (eq path :debugger) '(nil) '(nil t))
                         always
                         (loop for exit-mode in '(:normal :throw :error) always
                               (unbinding-test-frame path outer-bound inner-bound exit-mode))))))
         (result
           (parallel-input-run
            thunk (if (and (eq mode '$worker) (not (parallel-threads-p)))
                      '$fallback mode))))
    (if (eq mode '$public) (every #'identity result) result)))

(defun $unbinding_invalid_check ()
  (every
   (lambda (value)
     (flet ((condition-type (function)
              (handler-case (progn (funcall function value) :accepted)
                (error (condition) (type-of condition)))))
       (let ((expected (condition-type #'makunbound)))
         (and (not (eq expected :accepted))
              (eq (condition-type #'maxima-makunbound) expected)))))
   (list nil t :unbinding-constant pi 17 "x" #(1) '(x))))

(defun $unbinding_global_check ()
  (loop for index below 64 always
        (let ((symbol (make-symbol "$UNBINDING-GLOBAL")))
          (when (oddp index) (setf (symbol-value symbol) (ash 1 index)))
          (and (eq (maxima-makunbound symbol) symbol)
               (not (boundp symbol))
               (eq (maxima-makunbound symbol) symbol)
               (not (boundp symbol))))))

(defun $unbinding_native_check ()
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (return-from $unbinding_native_check ($unbinding_scope_check '$fallback '$primitive))
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (when (parallel-threads-p)
    (let* ((symbol (make-symbol "$UNBINDING-SHARED"))
           (root (list :global))
           (lock (%make-lock "unbinding regression"))
           (entered (vector nil nil)) (removed (vector nil nil))
           (valid (vector nil nil)) (cancelled nil) (threads nil))
      (setf (symbol-value symbol) root)
      (labels ((await-state (predicate)
                 (let ((deadline (+ (get-internal-real-time)
                                    (* 10 internal-time-units-per-second))))
                   (loop
                     (when (%with-lock (lock) (funcall predicate)) (return t))
                     (when (or (%with-lock (lock) cancelled)
                               (> (get-internal-real-time) deadline))
                       (return nil))
                     (sleep 0.001))))
               (worker (role)
                 (lambda ()
                   (progv (list symbol) (list (+ 100 role))
                     (%with-lock (lock) (setf (aref entered role) t))
                     (when (and (await-state (lambda () (every #'identity entered)))
                                (or (zerop role)
                                    (await-state (lambda () (aref removed 0)))))
                       (let ((own (= (symbol-value symbol) (+ 100 role))))
                         (maxima-makunbound symbol)
                         (%with-lock (lock) (setf (aref removed role) t))
                         (when (await-state (lambda () (every #'identity removed)))
                           (%with-lock (lock)
                             (setf (aref valid role) (and own (not (boundp symbol))))))))))))
        (unwind-protect
             (progn
               (dotimes (role 2)
                 (push (%spawn (worker role) "unbinding test worker") threads))
               (dolist (thread threads) (%join thread))
               (setq threads nil)
               (and (every #'identity entered) (every #'identity removed)
                    (every #'identity valid) (and (boundp symbol) (eq (symbol-value symbol) root))))
          (%with-lock (lock) (setq cancelled t))
          (dolist (thread threads) (%join thread)))))))

(defun $unbinding_multiple_values_check ()
  (let ((left (make-symbol "$UNBINDING-LEFT"))
        (right (make-symbol "$UNBINDING-RIGHT")))
    (setf (symbol-value left) 27 (symbol-value right) 29)
    (and (equal (multiple-value-list
                 (call-with-captured-bindings
                  (list (list left t 11) (list right nil nil))
                  (lambda () (values (symbol-value left) (boundp right) 17))))
                '(11 nil 17))
         (equal (multiple-value-list
                 (call-with-captured-bindings nil (lambda () (values 19 23))))
                '(19 23))
         (boundp left) (= (symbol-value left) 27)
         (boundp right) (= (symbol-value right) 29))))

(defun $unbinding_symbol_macro_check (mode)
  (parallel-input-run
   (lambda ()
     (let ((symbol (make-symbol "$UNBINDING-SYMBOL-MACRO"))
           (expansions 0)
           (old-hook *macroexpand-hook*))
       (eval `(define-symbol-macro ,symbol 42))
       (setf (symbol-value symbol) :global)
       (and
        (progv (list symbol) '(:local)
          (let ((*macroexpand-hook*
                  (lambda (expander form environment)
                    (incf expansions)
                    (funcall old-hook expander form environment))))
            (and (eq (maxima-makunbound symbol) symbol)
                 (not (boundp symbol)) (zerop expansions))))
        (boundp symbol) (eq (symbol-value symbol) :global))))
   (if (and (eq mode '$worker) (not (parallel-threads-p))) '$fallback mode)))
