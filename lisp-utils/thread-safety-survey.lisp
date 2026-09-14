;;;; thread-safety-survey.lisp -- global state that would leak between threads
;;;;
;;;; Maxima runs in one thread today.  Before any part of it can run in
;;;; more than one, the state a computation reaches through a special
;;;; variable has to be sorted into what may be shared and what may not.
;;;;
;;;; The distinction is not "is it global" but "is it bound".  In SBCL,
;;;; CCL, ECL and ABCL a LET on a special binds per thread: each thread
;;;; gets its own binding stack and only the global value is shared.  So a
;;;; variable its callers always bind is already thread-safe however
;;;; global it looks, and a variable SETQ'd with no binding in scope is
;;;; not, however innocent it looks.  This asks the compiler which is
;;;; which, instead of reading 1000 variables by hand.
;;;;
;;;; Run it in a built Maxima image.  It needs SBCL's xref data, so it is
;;;; SBCL-only, like check-dependencies.lisp beside it.
;;;;
;;;;     ./maxima-local --lisp=sbcl --no-init
;;;;     :lisp (load "lisp-utils/thread-safety-survey.lisp")
;;;;     :lisp (maxima-thread-survey:survey)
;;;;
;;;; The counts are facts.  The CATEGORY column is hand-reviewed and
;;;; deliberately incomplete: a variable nobody has looked at yet is
;;;; reported as untriaged rather than guessed at, so the list stays
;;;; honest as the code moves.  A variable that grows its first SETQ site
;;;; appears here by itself.
;;;;
;;;; Reading the output: a high SETS count with a low BINDS count is not
;;;; automatically a bug, and the categories want different fixes rather
;;;; than more of the same one.
;;;;
;;;; :OUT-PARAMETER and :PER-COMPUTATION both get the same cheap fix, and
;;;; it is not rewriting the assignments.  A binding anywhere up-stack
;;;; captures every SETQ below it, so binding these once, in whatever
;;;; wrapper starts an evaluation on a thread, makes the existing
;;;; protocol thread-safe with callers and callees untouched.  That
;;;; matters for compatibility: SIGN is how COMPAR hands an answer back,
;;;; some 50-odd share files reach into that machinery, and none of them
;;;; has to change.  Measured on CCL with a two-thread race over a
;;;; SIGN-shaped protocol: 1797 wrong answers in 4000 unbound, 0 bound.
;;;;
;;;; :SHARED-ENVIRONMENT must NOT be bound, which is why it is a separate
;;;; category: two threads are supposed to see one fact database and one
;;;; set of user function definitions, so that state wants a lock or an
;;;; explicit per-thread environment instead.
;;;;
;;;; So the useful product of this survey is the BIND-LIST below -- the
;;;; variables a thread entry point has to bind -- not the raw counts.
;;;;
;;;; WHAT THIS CANNOT SEE.  WHO-SETS reports assignments to a variable.
;;;; It does not report:
;;;;
;;;;   - assignments in a top-level form.  The ten (setq $props ...) forms
;;;;     in conjugate.lisp, gamma.lisp, nset.lisp and friends do not
;;;;     appear here.  For threading that is usually harmless -- they run
;;;;     once at load time, before any thread exists -- but do not read a
;;;;     zero as "nothing writes this".
;;;;
;;;;   - mutation of the object a variable holds.  ADD2LNC extends $PROPS
;;;;     with (nconc llist (ncons item)) and never assigns $PROPS, so it
;;;;     is invisible.  So is the fact database, which lives on symbol
;;;;     plists via PUTPROP in db.lisp, not in a variable at all.
;;;;
;;;; Both blind spots fall on :SHARED-ENVIRONMENT, the category that is
;;;; hardest to make thread-safe.  The counts below are therefore a floor
;;;; on the work, and the state most likely to corrupt silently is the
;;;; state this tool is worst at finding.  Locking that is a separate job
;;;; from the binding this survey plans.

;;; Wrapped in EVAL-WHEN so that the SB-INTROSPECT: symbols further down
;;; can be READ when this file is compiled, not only when it is loaded.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

(defpackage #:maxima-thread-survey
  (:use #:common-lisp)
  (:export #:survey #:variables #:bind-list))

(in-package #:maxima-thread-survey)

;;; ------------------------------------------------------------------
;;; Hand-reviewed classification.
;;;
;;; Symbols are named by string and looked up at run time: this file is
;;; read before anyone knows whether package MAXIMA exists.

(defparameter *classification*
  '((:out-parameter
     "callee hands its answer back through this; bind it at the thread
      entry point and every assignment below lands in that binding"
     "SIGN" "MINUS" "ODDS" "EVENS"
     "WIDTH" "HEIGHT" "DEPTH")
    (:per-computation
     "belongs to one evaluation; bind it at the thread entry point and it
      becomes per-thread for free"
     "$ERROR" "$ERROR_SYMS" "*LOCAL-SIGNS*" "$MULTIPLICITIES"
     "$%RNUM_LIST" "$LINENUM" "TSTACK" "$GENSUMNUM"
     "$INTEGRATION_CONSTANT_COUNTER")
    (:shared-environment
     "user-visible session state; sharing it is correct, so this wants a
      lock or an explicit per-thread environment, never a binding"
     "$VALUES" "$FUNCTIONS" "$RULES" "$ARRAYS" "$PROPS" "$CONTEXT"
     "$CONTEXTS" "$LABELS" "$STRUCTURES" "$%" "$RATVARS" "$RATWEIGHTS"
     "*RATWEIGHTS" "$FEATURES")
    (:startup
     "set while starting up and read thereafter; nothing to do"
     "$MAXIMA_OBJDIR" "$MAXIMA_TEMPDIR" "$MAXIMA_USERDIR" "$BROWSER"
     "$URL_BASE" "*MAXIMA-LANG-SUBDIR*"))
  "Each entry is (CATEGORY RATIONALE . VARIABLE-NAMES).  Everything else
that is assigned anywhere is reported as :UNTRIAGED.")

(defun category-of (symbol)
  (let ((name (symbol-name symbol)))
    (or (loop for (category nil . names) in *classification*
              when (member name names :test #'string=)
                return category)
        :untriaged)))

(defun rationale-of (category)
  (second (assoc category *classification*)))

;;; ------------------------------------------------------------------
;;; Collecting the facts

(defun maxima-package ()
  (or (find-package "MAXIMA")
      (error "thread-safety-survey: package MAXIMA not found.  Run this ~
              in a built Maxima image, not a bare lisp.")))

(defun specials ()
  "Every special variable whose home package is MAXIMA."
  (let ((package (maxima-package))
        (out '()))
    (do-symbols (symbol package)
      (when (and (eq (symbol-package symbol) package)
                 (eq (sb-int:info :variable :kind symbol) :special))
        (pushnew symbol out)))
    out))

(defun xref (function symbol)
  "Number of sites FUNCTION reports for SYMBOL, and the files they are in."
  (let ((raw (or (ignore-errors (funcall function symbol)) '())))
    (values (length raw)
            (sort (remove-duplicates
                   (loop for entry in raw
                         for file = (ignore-errors
                                      (sb-introspect:definition-source-pathname
                                       (cdr entry)))
                         when file collect (file-namestring file))
                   :test #'string=)
                  #'string<))))

(defstruct entry
  ;; No (:CONC-NAME NIL): the bare slot names would define accessors on
  ;; CL:VARIABLE, which is package-locked.
  variable category sets binds files)

(defun variables ()
  "One ENTRY per MAXIMA special that is assigned somewhere, worst first."
  (let ((out '()))
    (dolist (symbol (specials))
      (multiple-value-bind (sets files) (xref #'sb-introspect:who-sets symbol)
        (when (plusp sets)
          (push (make-entry :variable symbol
                            :category (category-of symbol)
                            :sets sets
                            :binds (xref #'sb-introspect:who-binds symbol)
                            :files files)
                out))))
    (sort out #'> :key #'entry-sets)))

;;; A survey that examined nothing looks exactly like a survey that found
;;; nothing.  An image built without xref data, or one where the sources
;;; have moved, reports every variable as never assigned.

(defun sanity-check (entries total)
  (when (zerop total)
    (error "thread-safety-survey: no special variables found in MAXIMA."))
  (when (null entries)
    (error "thread-safety-survey: ~D special variables, not one of them ~
            assigned anywhere.  This image has no xref data, so the ~
            survey would come out empty whatever the code says."
           total))
  (when (notany #'entry-files entries)
    (error "thread-safety-survey: found assignments but could not resolve ~
            a single source file for them.  The recorded pathnames and ~
            this tree do not agree; the file column would be blank ~
            throughout.")))

;;; ------------------------------------------------------------------
;;; Reporting

(defun report-group (entries limit stream)
  (if (null entries)
      (format stream "  none~%")
      (progn
        (format stream "  ~5@A ~5@A  ~28A ~A~%" "SETS" "BINDS" "VARIABLE" "WHERE")
        (dolist (e (subseq entries 0 (min limit (length entries))))
          (format stream "  ~5D ~5D  ~28A ~{~A~^ ~}~:[~; ...~]~%"
                  (entry-sets e) (entry-binds e) (entry-variable e)
                  (subseq (entry-files e) 0 (min 3 (length (entry-files e))))
                  (> (length (entry-files e)) 3)))
        (when (> (length entries) limit)
          (format stream "  ... and ~D more~%" (- (length entries) limit))))))

(defun split-lines (string)
  (loop with start = 0
        for position = (position #\Newline string :start start)
        collect (subseq string start position)
        while position
        do (setf start (1+ position))))

(defun classified-but-unseen (entries)
  "Classified names that did not turn up in the survey, split by why.
A name that is not a special variable at all has moved or was mistyped,
and the classification has rotted.  A name that IS one but shows no
assignment is the expected case for state written only by a top-level
form or mutated in place -- see WHAT THIS CANNOT SEE above."
  (let ((seen (mapcar (lambda (e) (symbol-name (entry-variable e))) entries))
        (gone '()) (invisible '()))
    (loop for (category nil . names) in *classification*
          do (loop for name in names
                   unless (member name seen :test #'string=)
                     do (let ((symbol (find-symbol name (maxima-package))))
                          (if (and symbol
                                   (eq (sb-int:info :variable :kind symbol)
                                       :special))
                              (push (cons category name) invisible)
                              (push (cons category name) gone)))))
    (values (nreverse gone) (nreverse invisible))))

(defun report-stale-classification (entries stream)
  (multiple-value-bind (gone invisible) (classified-but-unseen entries)
    (when gone
      (format stream "~&~%~D classified name~:P is not a special variable ~
                      in MAXIMA any more -- renamed, removed or mistyped, ~
                      and silently degrading to untriaged:~%" (length gone))
      (loop for (category . name) in gone
            do (format stream "  ~(~A~) ~A~%" category name)))
    (when invisible
      (format stream "~&~%~D classified variable~:P exists but shows no ~
                      assignment; written by a top-level form or mutated ~
                      in place, so xref cannot see it:~%" (length invisible))
      (loop for (category . name) in invisible
            do (format stream "  ~(~A~) ~A~%" category name)))))

(defun report-categories (entries stream)
  (format stream "~&~%By category:~%")
  (dolist (category (append (mapcar #'first *classification*) '(:untriaged)))
    (let ((in-category (remove-if-not (lambda (e) (eq (entry-category e) category))
                                      entries)))
      (when in-category
        (format stream "~&  ~(~20A~) ~3D variable~:P, ~D assignment~:P~%"
                category (length in-category)
                (reduce #'+ in-category :key #'entry-sets))
        (let ((why (rationale-of category)))
          (when why
            (format stream "~{      ~A~%~}"
                    (remove "" (mapcar (lambda (line) (string-trim " " line))
                                       (split-lines why))
                            :test #'string=))))))))

(defun survey (&key (stream *standard-output*) (limit 25))
  "Print the survey.  Returns the ENTRY list so it can be post-processed."
  (let* ((all (specials))
         (entries (variables)))
    (sanity-check entries (length all))
    (let ((unbound (remove-if-not (lambda (e) (zerop (entry-binds e))) entries))
          (both (remove-if (lambda (e) (zerop (entry-binds e))) entries)))
      (format stream "~&thread-safety-survey: ~D special variable~:P in ~
                      package MAXIMA~%" (length all))
      (format stream "~&  ~4D set, never bound   -- leak between threads ~
                      as written~%" (length unbound))
      (format stream "~&  ~4D set and bound      -- review each assignment~%"
              (length both))
      (format stream "~&  ~4D never assigned     -- already per-thread ~
                      wherever bound~2%" (- (length all) (length entries)))
      (format stream "~&--- set, never bound ---~%")
      (report-group unbound limit stream)
      (format stream "~&~%--- set and bound ---~%")
      (report-group both limit stream)
      (report-categories entries stream)
      (report-stale-classification entries stream)
      (values))))

;;; ------------------------------------------------------------------
;;; The actionable product: what a thread entry point has to bind.
;;;
;;; :SHARED-ENVIRONMENT is deliberately absent -- binding those would give
;;; each thread its own fact database and its own user functions, which is
;;; not multi-threading but several unrelated Maximas.  :STARTUP is absent
;;; because nothing writes it after startup.  :UNTRIAGED is included, and
;;; called out, because an unreviewed variable that is assigned somewhere
;;; is likelier to want binding than not -- but each one still has to be
;;; looked at before anybody relies on this list.

(defun bind-list (&key (stream *standard-output*) (include-untriaged t))
  "Print a LET list of the variables a per-thread evaluation must bind."
  (let* ((wanted (if include-untriaged
                     '(:out-parameter :per-computation :untriaged)
                     '(:out-parameter :per-computation)))
         (entries (remove-if-not (lambda (e) (member (entry-category e) wanted))
                                 (variables))))
    (format stream "~&;; ~D variable~:P to bind per evaluation thread~%"
            (length entries))
    (format stream "(let (")
    (loop for e in (sort entries #'string< :key (lambda (e) (symbol-name (entry-variable e))))
          for first = t then nil
          do (format stream "~:[~%      ~;~]~A~@[  ; ~(~A~)~]"
                     first (entry-variable e)
                     (when (eq (entry-category e) :untriaged) "untriaged")))
    (format stream ")~%  ...)~%")
    (values)))
