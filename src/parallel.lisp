;;;; parallel.lisp -- run independent computations on several cores
;;;;
;;;; The primitive under parallel MAKELIST and parallel FOR.  Three
;;;; properties shape everything here:
;;;;
;;;; ONE BUDGET FOR THE WHOLE SESSION.  The limit is on the total number
;;;; of worker threads alive at once, not on the number a single
;;;; parallel region may start.  A parallel region nested inside another
;;;; therefore draws from the same pool as its parent, and finds it
;;;; empty when the parent has spent it.
;;;;
;;;; A REGION THAT GETS NO WORKERS RUNS SERIALLY AND NEVER WAITS.  This
;;;; is what makes nesting safe: an inner region that blocked until a
;;;; slot freed would be waiting on threads that are themselves waiting
;;;; for it to finish.  Since it never waits, there is no cycle to
;;;; deadlock on.
;;;;
;;;; THE SERIAL PATH IS NOT A DEGRADED MODE.  GCL has no threads and
;;;; CLISP none usable, and both are supported lisps -- but the same
;;;; path also runs whenever the budget is spent, which on a busy
;;;; nested computation is most of the time.  It has to produce
;;;; identical answers, and it is exercised constantly rather than
;;;; rarely.

(in-package :maxima)

;;; ------------------------------------------------------------------
;;; What this lisp can do.
;;;
;;; Read-time conditionals rather than run-time tests: a lisp without
;;; threads must not even compile a reference to a threading symbol it
;;; does not have.

(defun parallel-threads-p ()
  "True when this lisp can run Maxima code on more than one thread."
  #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) t
  #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) nil)

#+sb-thread
(progn
  (defun %spawn (function name) (sb-thread:make-thread function :name name))
  (defun %join (thread) (sb-thread:join-thread thread))
  (defun %make-lock (name) (sb-thread:make-mutex :name name))
  (defmacro %with-lock ((lock) &body body)
    `(sb-thread:with-recursive-lock (,lock) ,@body)))

#+(and ccl openmcl-native-threads (not sb-thread))
(progn
  (defun %spawn (function name) (ccl:process-run-function name function))
  (defun %join (thread) (ccl:join-process thread))
  (defun %make-lock (name) (ccl:make-lock name))
  (defmacro %with-lock ((lock) &body body)
    `(ccl:with-lock-grabbed (,lock) ,@body)))

#+(and ecl threads (not sb-thread) (not ccl))
(progn
  (defun %spawn (function name) (mp:process-run-function name function))
  (defun %join (thread) (mp:process-join thread))
  (defun %make-lock (name) (mp:make-lock :name name :recursive t))
  (defmacro %with-lock ((lock) &body body)
    `(mp:with-lock (,lock) ,@body)))

#-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads))
(progn
  ;; No threads: the lock is a placeholder so the accounting code below
  ;; reads the same on every lisp.  Nothing ever contends for it.
  (defun %make-lock (name) (declare (ignore name)) nil)
  (defmacro %with-lock ((lock) &body body)
    (declare (ignore lock))
    `(progn ,@body)))

;;; ------------------------------------------------------------------
;;; How many cores are there?
;;;
;;; MAXIMA_NUM_CORES comes first because the launch scripts are in a
;;; better position to answer than lisp is.  src/maxima.in asks nproc,
;;; which respects CPU affinity -- pinned to two cores it says 2 where
;;; getconf and /proc/cpuinfo both say 4 -- and src/maxima.bat.in reads
;;; the NUMBER_OF_PROCESSORS that Windows sets for every process.
;;;
;;; The rest is for images that were not started through those scripts.
;;; Measured on SBCL 2.2.9: neither SB-IMPL::*N-CPUS* nor
;;; SB-SYS:MACHINE-CORES exists, so there is nothing to ask it and the
;;; count has to come from the operating system.  /proc is Linux-only,
;;; and counts cores this process may not be allowed to use; elsewhere
;;; this ends at 1 and parallel evaluation stays off until the user sets
;;; $PARALLEL_THREADS, which is the safe direction to be wrong in.

(defun cpu-count-from-environment ()
  (let ((setting (maxima-getenv "MAXIMA_NUM_CORES")))
    (when (and setting (plusp (length setting)))
      (let ((n (ignore-errors (parse-integer setting :junk-allowed nil))))
        (when (and (integerp n) (plusp n)) n)))))

(defun cpu-count-from-proc ()
  (ignore-errors
    (with-open-file (in "/proc/cpuinfo" :if-does-not-exist nil)
      (when in
        (let ((n (loop for line = (read-line in nil nil)
                       while line
                       count (and (>= (length line) 9)
                                  (string= "processor" line :end2 9)))))
          (when (plusp n) n))))))

(defun parallel-cpu-count ()
  "Number of cores available to this process, or 1 if it cannot be found."
  (or (cpu-count-from-environment)
      #+(and ccl openmcl-native-threads) (ignore-errors (ccl:cpu-count))
      #+(and ecl threads) (ignore-errors (si:get-number-of-processors))
      (cpu-count-from-proc)
      1))

(defmvar $parallel_threads 0
  "Greatest number of threads Maxima may use at once, counting the one
it is already running on.  0 means ask the operating system.  1 turns
parallel evaluation off without changing any other behaviour.")

(defun parallel-thread-limit ()
  (let ((setting $parallel_threads))
    (if (and (integerp setting) (plusp setting))
        setting
        (parallel-cpu-count))))

;;; ------------------------------------------------------------------
;;; The shared budget.
;;;
;;; *LIVE-WORKERS* counts worker threads currently running, session
;;; wide, and what is left to hand out is worked out from it on each
;;; request rather than kept in a counter of its own.  Keeping a
;;; remaining-budget variable instead would freeze the limit at whatever
;;; it was the first time anything ran in parallel, and a later
;;; parallel_threads:2 would be accepted and ignored.
;;;
;;; This is deliberately NOT per-thread state.  Binding it in
;;; WITH-THREAD-LOCAL-ENVIRONMENT would give every worker a fresh full
;;; budget and the cap would fail silently under exactly the nesting it
;;; exists to bound.  It belongs with the shared session state, never
;;; with the per-computation state.

(defvar *budget-lock* (%make-lock "maxima worker budget"))

(defvar *live-workers* 0
  "Worker threads running right now, across the whole session.")

;;; The peak is here for the tests: a parallel computation that quietly
;;; ran on the serial path returns exactly the right answers, so a test
;;; comparing answers alone cannot tell the two apart and passes
;;; whatever happened.  Reading the peak is what lets such a test fail
;;; when it ought to.

(defvar *peak-workers* 0)

;;; Peak worker count alone is not enough to prove work happened on
;;; another thread: the budget is claimed before any body is evaluated,
;;; so a run whose caller finished every item before a worker started
;;; would still show a peak.  This counts items a worker really
;;; evaluated, which the serial path cannot produce at all.

(defvar *items-run-by-workers* 0)

(defun claim-workers (wanted)
  "Take up to WANTED workers from the shared budget; return how many.
Never waits: a caller that gets fewer than it asked for, or none, runs
the rest of its work itself."
  (if (or (not (parallel-threads-p)) (not (plusp wanted)))
      0
      (%with-lock (*budget-lock*)
        ;; One slot belongs to the thread doing the asking, which is why
        ;; the limit is reduced by one before anything is handed out.
        (let* ((free (max 0 (- (parallel-thread-limit) 1 *live-workers*)))
               (granted (min wanted free)))
          (incf *live-workers* granted)
          (when (> *live-workers* *peak-workers*)
            (setq *peak-workers* *live-workers*))
          granted))))

(defun release-workers (count)
  (when (plusp count)
    (%with-lock (*budget-lock*)
      (decf *live-workers* count))))

;;; ------------------------------------------------------------------
;;; What a test needs to see.

(defmfun $parallel_cores ()
  "Greatest number of threads Maxima will use at once, this one included."
  (parallel-thread-limit))

(defmfun $parallel_threads_available ()
  "FALSE on a lisp where parallel evaluation falls back to running
serially -- GCL has no threads, and CLISP none that are usable."
  (parallel-threads-p))

(defmfun $parallel_peak_workers ()
  "Most worker threads alive at once since the last reset, not counting
the thread that started them."
  *peak-workers*)

(defmfun $parallel_worker_items ()
  "How many elements worker threads have evaluated since the last reset.
Zero whenever everything ran on the calling thread, so a test asserting
this is positive is one the serial path cannot pass by accident."
  *items-run-by-workers*)

(defmfun $parallel_reset_counters ()
  (%with-lock (*budget-lock*)
    (setq *peak-workers* *live-workers*)
    (setq *items-run-by-workers* 0)))

;;; ------------------------------------------------------------------
;;; Running the work.
;;;
;;; Workers and the calling thread all take items from one shared index,
;;; so an uneven body -- integrate(x^i) gets harder with i -- spreads
;;; itself rather than leaving one runner with the expensive half.
;;; Results are stored by index, so the answer does not depend on which
;;; runner finished first.

(defstruct (parallel-job (:conc-name job-))
  thunks results errors (next 0) lock count specials)

;;; Maxima binds a user variable by saving the symbol's value, MSETting
;;; it, and putting the old value back when the binding ends
;;; (MBIND-DOIT and MUNBIND, src/mlisp.lisp).  That writes the one
;;; global value cell every runner shares, so without a binding of its
;;; own each runner's loop variable overwrites the others' -- measured
;;; at 2 wrong results in 200 on a body as plain as i^2, which is the
;;; worst rate to have: common enough to happen, rare enough to ship.
;;;
;;; The fix is the same one the groundwork used for SIGN: a binding up
;;; the stack catches every assignment below it, so nothing in MBIND,
;;; MEVAL or the body has to change.  BINDLIST and MSPECLIST are bound
;;; for the same reason -- they are the stacks MBIND pushes the saved
;;; values onto, and two runners pushing and popping one stack corrupt
;;; each other's unwinding.

(defun call-with-private-bindings (symbols thunk)
  "Run THUNK with a binding of each of SYMBOLS private to this thread,
starting from the value it has now, or unbound if it has none."
  (let ((bound (mapcar #'boundp symbols)))
    (progv symbols
        (mapcar (lambda (symbol) (and (boundp symbol) (symbol-value symbol)))
                symbols)
      ;; PROGV cannot make a single binding unbound, so the ones that
      ;; had no value are emptied again here.  MAKUNBOUND on a variable
      ;; bound by PROGV empties that binding, not the global one.
      (loop for symbol in symbols
            for was-bound in bound
            unless was-bound do (makunbound symbol))
      (funcall thunk))))

(defun job-take (job)
  "Index of the next unclaimed item, or NIL when they are all taken."
  (%with-lock ((job-lock job))
    (let ((index (job-next job)))
      (when (< index (job-count job))
        (setf (job-next job) (1+ index))
        index))))

(defun run-one-item (job index)
  "Evaluate item INDEX, recording either its value or its error.
$ERROR is per-thread state, so a worker's error message has to be
copied out of the worker's own binding before the parent can show it."
  (let ((outcome (errcatch (funcall (aref (job-thunks job) index)))))
    (if outcome
        (setf (aref (job-results job) index) (first outcome))
        (setf (aref (job-errors job) index)
              (or $error '((mlist simp) "parallel: a worker failed"))))))

(defun run-items (job &optional worker-p)
  "Take items until there are none left.  A worker reports how many it
got, once, at the end: counting per item would put every runner through
the same lock on every element, and the count exists for tests rather
than for the computation."
  (let ((mine 0))
    (loop for index = (job-take job)
          while index
          do (run-one-item job index)
             (incf mine))
    (when (and worker-p (plusp mine))
      (%with-lock (*budget-lock*)
        (incf *items-run-by-workers* mine)))
    mine))

(defun job-specials-to-bind (job)
  (list* 'bindlist 'mspeclist (job-specials job)))

(defun run-worker (job)
  "A worker's whole life.  WITH-THREAD-LOCAL-ENVIRONMENT must be entered
here, inside the thread: a new thread inherits no dynamic bindings, so
wrapping the spawn in a LET binds nothing the worker will ever see."
  (lambda ()
    (with-thread-local-environment
      (call-with-private-bindings
       (job-specials-to-bind job)
       (lambda () (run-items job t))))))

(defun call-in-parallel (thunks &optional specials)
  "Call each thunk and return their values as a list, in order.

SPECIALS names variables each runner must have to itself -- for a
MAKELIST, the loop variable, whose value cell every runner would
otherwise share.

Runs the thunks on as many cores as the shared budget allows, and
serially when it allows none -- the answers are the same either way."
  (let* ((thunks (coerce thunks 'vector))
         (count (length thunks)))
    (if (zerop count)
        '()
        (let* ((job (make-parallel-job
                     :thunks thunks
                     :results (make-array count :initial-element nil)
                     :errors (make-array count :initial-element nil)
                     :lock (%make-lock "maxima parallel job")
                     :count count
                     :specials (remove-if-not #'symbolp specials)))
               ;; One item stays with the calling thread, so asking for
               ;; COUNT-1 workers is asking for a runner per item.
               (granted (claim-workers (1- count)))
               (threads '()))
          (unwind-protect
               (progn
                 #+(or sb-thread (and ccl openmcl-native-threads)
                       (and ecl threads))
                 (dotimes (i granted)
                   (push (%spawn (run-worker job)
                                 (format nil "maxima worker ~D" i))
                         threads))
                 ;; The calling thread is a runner too, which is what
                 ;; makes a region with no workers simply serial.  It
                 ;; takes the same private bindings as a worker, so
                 ;; every runner is isolated the same way and the two
                 ;; paths cannot differ in what they leave behind.
                 (call-with-private-bindings
                  (job-specials-to-bind job)
                  (lambda () (run-items job)))
                 (mapc #'%join threads))
            (release-workers granted))
          ;; Report the first failure by index, so the same input always
          ;; reports the same error however the work was distributed.
          (let ((failed (position-if-not #'null (job-errors job))))
            (when failed
              (setq $error (aref (job-errors job) failed))
              (error 'maxima-$error)))
          (coerce (job-results job) 'list)))))
