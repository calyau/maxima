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
  (defun %make-lock (lock-name) (declare (ignore lock-name)) nil)
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

;;; ECL has no branch here on purpose.  SI:GET-NUMBER-OF-PROCESSORS does
;;; not exist in ECL 21.2.1, and naming a symbol that a package does not
;;; export is a READER error -- it happens before any of this code runs,
;;; so IGNORE-ERRORS cannot catch it and the file simply fails to
;;; compile.  It took Maxima's whole ECL build down with it:
;;; "Cannot find the external symbol GET-NUMBER-OF-PROCESSORS in SI".
;;; ECL therefore takes its count from MAXIMA_NUM_CORES, which is what
;;; src/maxima.in is for, and from /proc after that.
(defun parallel-cpu-count ()
  "Number of cores available to this process, or 1 if it cannot be found."
  (or (cpu-count-from-environment)
      #+(and ccl openmcl-native-threads) (ignore-errors (ccl:cpu-count))
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
;;; parallel(...) -- a do loop across cores.
;;;
;;; "for" is reader syntax, not a function: it parses to an MDO form, so
;;; there is no way to spell parallel_for(i, 1, ...) that anyone would
;;; want to type.  A wrapper taking the loop unevaluated keeps the
;;; loop's own syntax, and reads much the way OpenMP's pragma sits above
;;; the loop it applies to:
;;;
;;;     parallel(for i: 1 thru 20 do heavy(i))
;;;
;;; Only a counted loop can be spread: the iteration values have to be
;;; known before any of them runs.  A "next" clause computes each value
;;; from the one before, and "while" and "unless" decide after each body
;;; whether there is another -- both are sequential by construction.
;;; Rather than refuse those, this evaluates them as the ordinary loop
;;; they are, so wrapping anything in parallel() is always safe and at
;;; worst does nothing.
;;;
;;; A do loop is run for its effects, and whether those are independent
;;; is the caller's claim to make -- the same bargain OpenMP strikes.
;;; What it cannot allow is "return", which would name one iteration's
;;; exit out of many happening at once, so that is an error rather than
;;; a race.

(defun parallel-mdo-values (parts)
  "Collect the iteration values with MDO's own stepping and bound checks."
  (let ((collecting-parts (copy-list parts))
        (iteration-values nil))
    ;; A private Lisp closure avoids a user variable or a shared collector.
    ;; Only the body is replaced; MDO still checks the bound at each step.
    ;; In particular, neither exact bounds nor repeated floating addition
    ;; can be replaced by FLOOR of a floating-point quotient.
    (setf (seventh collecting-parts)
          (list (list (lambda (iteration-value)
                        (when (and iteration-values
                                   (alike1 iteration-value
                                           (car iteration-values)))
                          (merror (intl:gettext
                                   "parallel: the loop step does not advance the iteration variable.")))
                        (push iteration-value iteration-values)
                        nil))
                (first parts)))
    ;; MDO uses MSET to bind its variable. A nested collector must not
    ;; overwrite the value being used by another runner's loop.
    (call-with-captured-bindings
     (capture-bindings (list (first parts)))
     (lambda () (meval (cons '(mdo) collecting-parts))))
    (nreverse iteration-values)))

(defun parallel-loop (variable values body)
  "Evaluate BODY once per element of VALUES, with VARIABLE set to it.
Every runner has its own binding of VARIABLE, so the assignment below
stays inside the runner that made it."
  (call-in-parallel
   (mapcar (lambda (value)
             (lambda ()
               (mset variable value)
               (let ((mdop t))
                 (when (catch 'mprog (prog2 (meval body) nil))
                   (merror (intl:gettext "parallel: 'return' cannot leave a ~
                                          parallel do loop"))))
               nil))
           values)
   (list variable))
  '$done)

(defun parallel-mdo (form)
  "A counted do loop, spread over the cores when its values can be known
before any body runs, and evaluated as the ordinary loop it is when they
cannot."
  (let* ((parts (copy-list (cdr form)))
         (variable (first parts)))
    ;; Delegate unsupported shapes before evaluating any controls. MDO
    ;; must see their side effects exactly once, in its ordinary order.
    (if (or (null variable) (not (symbolp variable))
            (null (fifth parts)) (fourth parts) (sixth parts))
        (meval (cons '(mdo) parts))
        ;; MDO evaluates STEP before FROM. Quote their evaluated values
        ;; when handing them back to MDO, including on the fallback path.
        (let* ((step (if (third parts) (meval (third parts)) 1))
               (from (if (second parts) (meval (second parts)) 1)))
          (setf (second parts) (list '(mquote) from)
                (third parts) (list '(mquote) step))
          (if (and (numberp from) (numberp step) (not (zerop step)))
              (parallel-loop variable (parallel-mdo-values parts)
                             (seventh parts))
              (meval (cons '(mdo) parts)))))))

(defun parallel-mdoin (form)
  "A do loop over the members of a list.  The members are known before
any body runs, so this needs no counting -- but an ATOM here is one of
MDOIN's hashed-array shapes, which is left to MDOIN itself."
  (let* ((parts (cdr form))
         (variable (car parts))
         (set (format1 (meval (cadr parts))))
         (limit (car (cddddr parts)))
         (until (cadr (cddddr parts)))
         (body (caddr (cddddr parts))))
    (if (or (null variable) limit until ($atom set))
        (meval (cons '(mdoin) parts))
        (parallel-loop variable (margs set) body))))

;;; The two operators the reader makes out of THRU_PARALLEL and
;;; IN_PARALLEL.  They carry exactly MDO's and MDOIN's own argument
;;; shapes, so falling back means handing the same parts to the ordinary
;;; operator.

(defmspec mdo-parallel (form)
  (parallel-mdo form))

(defmspec mdoin-parallel (form)
  (parallel-mdoin form))

(defmspec $parallel (form)
  (let ((argument (cadr form)))
    (if (and (consp argument) (consp (car argument)))
        (case (caar argument)
          ((mdo mdo-parallel) (parallel-mdo argument))
          ((mdoin mdoin-parallel) (parallel-mdoin argument))
          (t (meval argument)))
        (meval argument))))

;;; do_parallel is the other half of the picture: PARALLEL_MAKELIST and
;;; a parallel do loop run one expression over many values, while this
;;; runs many different expressions at once.
;;;
;;; Its arguments must not be evaluated on the way in -- that is the
;;; whole point.  Evaluating them to pass them along would compute every
;;; one of them, sequentially, before the first thread ever started.

(defmspec $do_parallel (form)
  (let* ((arguments (cdr form))
         ;; do_parallel([a, b, c]) and do_parallel(a, b, c) both read as
         ;; "run these three", so a lone list argument is its elements.
         (expressions (if (and (null (cdr arguments))
                               (consp (car arguments))
                               (consp (caar arguments))
                               (eq (caaar arguments) 'mlist))
                          (cdar arguments)
                          arguments)))
    (call-in-parallel
     (mapcar (lambda (expression) (lambda () (meval expression))) expressions))
    '$done))

;;; ------------------------------------------------------------------
;;; Running the work.
;;;
;;; Workers and the calling thread all take items from one shared index,
;;; so an uneven body -- integrate(x^i) gets harder with i -- spreads
;;; itself rather than leaving one runner with the expensive half.
;;; Results are stored by index, so the answer does not depend on which
;;; runner finished first.

(defstruct (parallel-job (:conc-name job-))
  thunks results errors (next 0) lock count captured)

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

;;; The values have to be read in the thread that starts the workers,
;;; not in the workers.  A new thread inherits no dynamic bindings, only
;;; global values, so a worker reading a variable for itself sees the
;;; session-wide value and not what its caller had -- and for the
;;; context variables that is wrong rather than merely different: a
;;; parallel region nested inside a parallel body would hang its
;;; contexts off the global one and lose every assumption the outer body
;;; had made.  Measured before this was captured: the inner bodies of a
;;; nested region answered pnz for a fact the outer body had just
;;; asserted.

(defun capture-bindings (symbols)
  "What each of SYMBOLS is worth here and now, for a runner to start from."
  (mapcar (lambda (symbol)
            (list symbol (boundp symbol) (and (boundp symbol)
                                              (symbol-value symbol))))
          symbols))

(defun call-with-captured-bindings (captured thunk)
  "Run THUNK with each captured variable bound privately to this thread,
starting from the value CAPTURE-BINDINGS recorded."
  (progv (mapcar #'first captured) (mapcar #'third captured)
    ;; PROGV cannot make a single binding unbound, so the ones that had
    ;; no value are emptied again here.  MAKUNBOUND on a variable bound
    ;; by PROGV empties that binding, not the global one.
    (loop for (symbol bound-p nil) in captured
          unless bound-p do (makunbound symbol))
    (funcall thunk)))

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

;;; LOCLIST belongs here because ERRCATCH saves (cons bindlist loclist)
;;; and ERRLFUN1 (src/suprv1.lisp) unwinds by calling MUNLOCAL until
;;; LOCLIST is EQ to the cons it saved.  Shared between runners, that
;;; cons is no longer anywhere in this thread's chain, so the loop pops
;;; an already-empty LOCLIST for ever: an error in one element left the
;;; whole run spinning at full CPU, on about one run in five.
;;;
;;; MUNLOCAL also pops MPROPLIST and FACTLIST, which MLOCAL pushes
;;; alongside LOCLIST.  They are not bound here because nothing yet
;;; measures what a local() inside a parallel body does to them; a body
;;; using local() is still to be checked rather than assumed safe.
;;;
;;; The context variables are here because WITH-NEW-CONTEXT
;;; (src/maxmac.lisp) makes a scratch context named by a gensym, works
;;; inside it and kills it again, and INTEGRATE and the limit code use
;;; it constantly.  $SUPCONTEXT registers the new name with
;;; (setq $contexts (mcons name $contexts)) -- a read, a cons and a
;;; write of one shared list -- so two runners doing it at once lose one
;;; of the two names, and the runner whose name went missing then fails
;;; with "supcontext: no such context ctxt<n>".  Measured before this
;;; binding: 30 of 30 parallel integrate() runs failed, and a scratch
;;; context was left behind in the user-visible contexts list.
;;;
;;; This does NOT give a thread its own assumptions, which would be a
;;; change to what Maxima means rather than a bug fix (issue #3).  The
;;; fact database lives on symbol plists keyed by the context symbol,
;;; not in these variables, so a body that calls assume() still writes
;;; the same context's plist and every thread still sees it.  What is
;;; per-thread here is only which context a thread is currently in and
;;; its own list of context names.
;;; CURRENT (src/db.lisp) is deliberately NOT in this list, though it
;;; looks like it belongs: it records which context the fact database
;;; has marked, and CONTEXTMARK does nothing when it already equals
;;; CONTEXT, so sharing it is why a nested parallel region cannot see
;;; the facts of the body that started it.
;;;
;;; Binding it makes things worse, not better, and the reason is the one
;;; the bigfloat six taught: CURRENT is half of a pair.  The other half
;;; is the CMARK counters on the context symbols' plists, which no
;;; binding can make private.  A runner with its own CURRENT unmarks its
;;; caller's chain, marks its own, and then loses the binding on the way
;;; out while the counters it changed stay changed -- so the caller's
;;; contexts are left unmarked and its facts invisible.  Measured:
;;; binding it took a plain inherited assumption from 0 wrong in 200 to
;;; 200 wrong in 200.  The pair has to move together or not at all,
;;; which means the counters need to stop being global first (issue #3).
;;; The streams are captured for the same reason as everything else
;;; here, and it shows up in the test suite.  RUN_TESTSUITE rebinds
;;; *STANDARD-OUTPUT* around each problem to catch whatever it prints
;;; (TEST-BATCH, src/mload.lisp), and WITH-THREAD-LOCAL-ENVIRONMENT
;;; rebinds the streams too -- but inside the worker, where it can only
;;; see the session-wide values.  So a message printed by a worker went
;;; past the harness and into the log, while the same message printed by
;;; the calling thread was caught: the suite's output changed from run to
;;; run depending on which runner happened to take the failing element.
;;; Binding them from what the caller had puts every runner's output
;;; where the caller's would have gone.
(defun specials-to-bind (specials)
  (list* 'bindlist 'mspeclist 'loclist
         '$context 'context '$contexts '$activecontexts
         '*standard-output* '*error-output* '*trace-output*
         '*query-io* '*standard-input*
         specials))

;;; Giving each runner a context of its own -- so that facts a body
;;; asserts belong to that body and go away with it -- is written and
;;; measured, and is NOT enabled, because it corrupts the database it
;;; was meant to tidy.
;;;
;;; The scoping itself is right: a context sees its parent's facts
;;; through the SUBC chain and killing it takes its own with it, and on
;;; the serial path 200 regions in a row left every inherited assumption
;;; intact.  What it cannot survive is concurrency.  Deciding which
;;; facts are visible goes through CONTEXTMARK (src/db.lisp), which
;;; keeps a count on each context symbol's plist and walks the chain
;;; incrementing and decrementing it.  Those counts are global and the
;;; updates are a read, an add and a write, so runners marking and
;;; unmarking at the same time lose each other's updates and the count
;;; on a context holding real assumptions drifts to zero.  Measured with
;;; this enabled: a plain inherited assume() went from right every time
;;; to wrong 157 times in 200, getting worse the more regions had run,
;;; against 0 in 200 on the serial path.
;;;
;;; A lock around the counter walk would stop the updates being lost and
;;; still not be correct: the count is a count, so two runners' chains
;;; are marked at once and each can see the other's facts -- the exact
;;; leak the scoping exists to prevent.  Marking has to become per
;;; thread before this can be turned on, which means the counts have to
;;; stop living on shared plists.  That is issue #3.
;;;
;;; Until then a body's facts stay where they always went, and the
;;; documented rule stands on its own: iterations must not depend on
;;; each other, assumptions included.

(defun call-as-runner (job worker-p)
  (call-with-captured-bindings
   (job-captured job)
   (lambda ()
     ;; The caller and the serial fallback obey the same rule as workers:
     ;; whether an item may ask must not depend on who happened to take it.
     ;; Bind inside the runner; new threads do not inherit LET bindings.
     (let ((*parallel-input-forbidden* t))
       (run-items job worker-p)))))

(defun run-worker (job)
  "A worker's whole life.  WITH-THREAD-LOCAL-ENVIRONMENT must be entered
here, inside the thread: a new thread inherits no dynamic bindings, so
wrapping the spawn in a LET binds nothing the worker will ever see."
  (lambda ()
    (with-thread-local-environment
      (call-as-runner job t))))

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
                     ;; Read here, in the thread that is about to start
                     ;; the workers: they cannot read it for themselves.
                     :captured (capture-bindings
                                (specials-to-bind
                                 (remove-if-not #'symbolp specials)))))
               ;; One item stays with the calling thread, so asking for
               ;; COUNT-1 workers is asking for a runner per item.
               (granted (claim-workers (1- count)))
               #+(or sb-thread (and ccl openmcl-native-threads)
                     (and ecl threads))
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
                 (call-as-runner job nil)
                 #+(or sb-thread (and ccl openmcl-native-threads)
                       (and ecl threads))
                 (mapc #'%join threads))
            (release-workers granted))
          ;; Report the first failure by index, so the same input always
          ;; reports the same error however the work was distributed.
          (let ((failed (position-if-not #'null (job-errors job))))
            (when failed
              (setq $error (aref (job-errors job) failed))
              (error 'maxima-$error)))
          (coerce (job-results job) 'list)))))
