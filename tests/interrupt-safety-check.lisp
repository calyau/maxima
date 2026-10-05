;;; A user interrupt must not leave Maxima's own bookkeeping half done.
;;; The tests that use these checks are in rtest_mset.mac.
(in-package :maxima)

(defun interrupt-safety-current-thread ()
  #+sb-thread sb-thread:*current-thread*
  #+(and ccl openmcl-native-threads (not sb-thread)) ccl:*current-process*
  #+(and ecl threads (not sb-thread) (not ccl)) mp:*current-process*
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) nil)

(defun interrupt-safety-send (thread function)
  #+sb-thread (sb-thread:interrupt-thread thread function)
  #+(and ccl openmcl-native-threads (not sb-thread))
  (ccl:process-interrupt thread function)
  #+(and ecl threads (not sb-thread) (not ccl))
  (mp:interrupt-process thread function)
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (declare (ignore thread function)))

(defun interrupt-safety-threads-p ()
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) t
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) nil)

(defun interrupt-safety-spawn (function name)
  #+sb-thread (sb-thread:make-thread function :name name)
  #+(and ccl openmcl-native-threads (not sb-thread))
  (ccl:process-run-function name function)
  #+(and ecl threads (not sb-thread) (not ccl))
  (mp:process-run-function name function)
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (declare (ignore function name)))

(defun interrupt-safety-join (thread)
  #+sb-thread (sb-thread:join-thread thread :default nil)
  #+(and ccl openmcl-native-threads (not sb-thread)) (ccl:join-process thread)
  #+(and ecl threads (not sb-thread) (not ccl)) (mp:process-join thread)
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
  (declare (ignore thread)))

(defun interrupt-safety-copy-call-stack (stack)
  (let ((copy (make-array (array-total-size stack)
                          :fill-pointer (fill-pointer stack) :adjustable t)))
    (replace copy stack)
    copy))

;;; ------------------------------------------------------------------
;;; WITH-INTERRUPTS-DEFERRED holds an interrupt back until its body is
;;; done, and then delivers it.

(defun $interrupt_deferral_check ()
  (unless (interrupt-safety-threads-p)
    (return-from $interrupt_deferral_check '$skipped))
  (let ((events nil) (self (interrupt-safety-current-thread)) (sender nil))
    (with-interrupts-deferred
      (setq sender (interrupt-safety-spawn
                    (lambda ()
                      (interrupt-safety-send
                       self (lambda () (push :interrupt events))))
                    "interrupt deferral check"))
      (interrupt-safety-join sender)
      ;; The interrupt has been sent by now, but must not have run.
      (sleep 0.05)
      (push :body-done events))
    ;; ... and is delivered once the body is done.
    (loop repeat 1000 until (member :interrupt events) do (sleep 0.001))
    (equal (reverse events) '(:body-done :interrupt))))

;;; ------------------------------------------------------------------
;;; An interrupt that abandons a cleanup half way.  The ASSIGN property
;;; throws, the way an interrupt would, the first time the variable is
;;; restored, so MUNBIND (or MUNBIND-TO) stops between two of its
;;; restorations.  Nothing further in is left to finish the job; the
;;; repair the top level runs before reading the next input must.

(defun $interrupted_cleanup_check (kind)
  (let* (($values (copy-list $values)) ($myoptions (copy-list $myoptions))
         (*mlambda-call-stack* (interrupt-safety-copy-call-stack *mlambda-call-stack*))
         (bindlist bindlist) (mspeclist mspeclist) (loclist loclist)
         (bind-mark bindlist) (loc-mark loclist)
         (stack-mark (fill-pointer *mlambda-call-stack*))
         (first (make-symbol "$INTERRUPT-FIRST"))
         (second (make-symbol "$INTERRUPT-SECOND"))
         (thrown nil))
    (setf (get second 'assign)
          (lambda (var value)
            (declare (ignore var value))
            (when (and munbindp (not thrown))
              (setq thrown t)
              (throw 'interrupted-cleanup :interrupted))))
    (progv (list first second) '(1 2)
      (catch 'interrupted-cleanup
        (ecase kind
          ($function_call
           (mlambda `((lambda) ((mlist) ,first ,second) 0) '(10 20)
                    '$interrupt_test t nil))
          ($block_variables
           (mbinding ((list first second) (list 10 20)) 0))
          ($block_local
           ;; A LOCAL frame inside the block, so that the repair has to
           ;; take that down as well.
           (meval `((mprog) ((mlist) ((msetq) ,first 10) ((msetq) ,second 20))
                    (($local) $interrupt_test_local)
                    0)))))
      (let ((leaked (list (symbol-value first) (symbol-value second)
                          (length bindlist) (length loclist))))
        ;; What CONTINUE does before it reads the next input.
        (unwind-maxima-frames (cons bind-mark loc-mark) stack-mark)
        (list '(mlist) thrown
              ;; It was left inconsistent ...
              (not (equal leaked (list 1 2 (length bind-mark)
                                       (length loc-mark))))
              ;; ... and the repair made it whole again.
              (eql (symbol-value first) 1) (eql (symbol-value second) 2)
              (eq bindlist bind-mark) (eq loclist loc-mark)
              (eql (length mspeclist) (length bind-mark))
              (eql (fill-pointer *mlambda-call-stack*) stack-mark))))))

;;; ------------------------------------------------------------------
;;; Real interrupts at random moments.  Each round evaluates EXPR,
;;; interrupts it after a random fraction of the time it takes, goes
;;; through Maxima's own debugger hook back to a top level the way Ctrl+C
;;; does, runs the repair CONTINUE runs, and then checks that nothing
;;; inconsistent is left.  Returns true, or the problems found and how
;;; often.

(defvar *interrupt-safety-armed* nil)

(defun interrupt-safety-problems (bind-mark loc-mark stack-mark)
  (let ((problems nil))
    (unless (eq bindlist bind-mark) (push '$bindlist problems))
    (unless (eql (length mspeclist) (length bind-mark))
      (push '$mspeclist problems))
    (unless (eq loclist loc-mark) (push '$loclist problems))
    (unless (eql (fill-pointer *mlambda-call-stack*) stack-mark)
      (push '$call_stack problems))
    (unless (and (boundp '$interrupt_test_a) (eql $interrupt_test_a 100))
      (push '$leaked_value problems))
    (unless (eq $simp t) (push '$leaked_simp problems))
    (unless (eql fpprec (+ 2 (integer-length (expt 10 $fpprec))))
      (push '$fpprec_mismatch problems))
    (unless (and (eq context '$initial) (eq $context '$initial))
      (push '$context problems))
    (dolist (f (cdr $functions))
      (unless (mget (caar f) 'mexpr) (push '$function_without_definition problems)))
    (let ((listed (member '((mgreaterp) $interrupt_test_x 0)
                          (cdr ($facts '$initial)) :test #'alike1))
          (known (eq (meval '(($is) ((mgreaterp) $interrupt_test_x 0))) t)))
      (unless (eq (not listed) (not known)) (push '$facts_mismatch problems)))
    problems))

(defun interrupt-safety-reset ()
  ;; What an interrupted round may legitimately leave behind: it stopped
  ;; between two statements of EXPR.
  (let (($errormsg nil) (*standard-output* (make-broadcast-stream)))
    (meval '(($forget) ((mgreaterp) $interrupt_test_x 0)))
    (meval '((msetq) $fpprec 16))
    (meval '(($kill) $interrupt_test_k))))

(defun $interrupt_stress_check (expr rounds)
  (unless (interrupt-safety-threads-p)
    (return-from $interrupt_stress_check '$skipped))
  (let* ((self (interrupt-safety-current-thread))
         (bind-mark bindlist) (loc-mark loclist)
         (stack-mark (fill-pointer *mlambda-call-stack*))
         (*standard-output* (make-broadcast-stream))
         ($interrupt_test_a 100) ($fpprec $fpprec)
         ;; The fastest of a few runs: the first one may pay for warming
         ;; up, and too long a time would let most rounds finish before
         ;; their interrupt arrives.
         (duration (loop repeat 3
                         minimize (let ((start (get-internal-real-time)))
                                    (meval expr)
                                    (/ (float (max 1 (- (get-internal-real-time) start)))
                                       internal-time-units-per-second))))
         (counts nil) (interrupted 0))
    (declare (special $interrupt_test_a))
    (interrupt-safety-reset)
    (dotimes (round rounds)
      (let ((delay (random duration)) (sender nil))
        (catch 'return-from-debugger
          (let ((*interrupt-safety-armed* t))
            ;; Registered before it can possibly interrupt us.
            (with-interrupts-deferred
              (setq sender
                    (interrupt-safety-spawn
                     (lambda ()
                       (sleep delay)
                       (interrupt-safety-send
                        self
                        (lambda ()
                          (when *interrupt-safety-armed*
                            (let ((*debugger-hook* #'maxima-lisp-debugger))
                              (invoke-debugger
                               (make-condition 'simple-error
                                               :format-control "Test interrupt")))))))
                     "interrupt stress check")))
            (meval expr)
            (setq *interrupt-safety-armed* nil)
            (decf interrupted)))
        (incf interrupted)
        (interrupt-safety-join sender)
        (unwind-maxima-frames (cons bind-mark loc-mark) stack-mark)
        (dolist (problem (interrupt-safety-problems bind-mark loc-mark stack-mark))
          (let ((entry (assoc problem counts)))
            (if entry (incf (cdr entry)) (push (cons problem 1) counts))))
        ;; Start the next round from a clean state even if this one was not.
        (unless (eq bindlist bind-mark) (setq bindlist bind-mark))
        (setq mspeclist (last mspeclist (length bind-mark)) loclist loc-mark)
        (setf (fill-pointer *mlambda-call-stack*) stack-mark)
        (setq $interrupt_test_a 100 context '$initial $context '$initial)
        (meval '((msetq) $simp t))
        (interrupt-safety-reset)))
    ;; A test in which hardly any round was actually interrupted would
    ;; prove nothing.
    (when (< (* 4 interrupted) rounds)
      (push (cons '$too_few_interrupts interrupted) counts))
    (or (null counts)
        (cons '(mlist)
              (mapcar (lambda (entry) (list '(mequal) (car entry) (cdr entry)))
                      counts)))))
