;;;; thread-environment-check.lisp -- does WITH-THREAD-LOCAL-ENVIRONMENT
;;;; actually isolate what it claims to?
;;;;
;;;; The macro (src/suprv1.lisp) is a LET over some thirty specials.  Two
;;;; things can make such a LET quietly do nothing, and neither shows up
;;;; as an error:
;;;;
;;;;   A variable the compiler has not been told is special is bound
;;;;   LEXICALLY.  The macro then isolates nothing, and every SETQ
;;;;   underneath still hits the global.  This is not hypothetical: VLIST
;;;;   was in exactly that state, because its only declaration is a
;;;;   DECLARE-TOP in rat3e.lisp and DECLARE-TOP proclaims at load time
;;;;   only inside a macro module, which rat3e is not.
;;;;
;;;;   A variable can drop off the list as the macro is edited.
;;;;
;;;; The obvious check -- "is the global unchanged afterwards?" -- catches
;;;; neither, because a lexical binding leaves the global alone too.  What
;;;; discriminates is SET, which always writes the symbol's value cell.
;;;; Under a dynamic binding SET writes that binding and the value is gone
;;;; when it unwinds; under a lexical one it writes the global and the
;;;; value is still sitting there afterwards.  So the check below assigns
;;;; a marker inside the environment and then looks for survivors.
;;;;
;;;; Run it in a built Maxima image.  The binding checks work on any
;;;; lisp; the race needs threads, so it is skipped where there are none.
;;;;
;;;;     ./maxima-local --no-init
;;;;     :lisp (load "lisp-utils/thread-environment-check.lisp")
;;;;     :lisp (maxima::check-thread-environment)

(in-package :maxima)

(defparameter *thread-environment-variables*
  '(;; COMPAR's answer, in four parts
    sign minus odds evens
    ;; DISPLA's box dimensions and its layout scratch
    width height depth linearray
    ;; CRE's variables and their ordering
    varlist genvar vlist
    ;; bigfloat precision: six variables holding one piece of state
    $fpprec fpprec *bigfloatone* *bigfloatzero* *bfhalf* *bfmhalf*
    ;; state belonging to one line of computation
    tstack *local-signs* $multiplicities $%rnum_list $error $error_syms
    $linenum $gensumnum $integration_constant_counter
    ;; output attribution and the question channel
    *standard-output* *error-output* *trace-output*
    *query-io* *standard-input*)
  "What WITH-THREAD-LOCAL-ENVIRONMENT is expected to bind.  Kept here
rather than derived from the macro on purpose: a check that reads its
expectations out of the thing it is checking cannot fail.")

(defun check-bindings (&optional (stream *debug-io*))
  "Returns the variables whose value survived the binding -- which is
what a lexical binding, or an omission from the macro, looks like."
  ;; The marker has to be a value the variable is allowed to hold: SBCL
  ;; declares the type of *STANDARD-OUTPUT* and friends and refuses a
  ;; keyword, where CCL accepts anything.  A freshly made object of a
  ;; fitting kind is both acceptable and EQ-distinguishable.
  (let ((leaked '())
        (markers (mapcar (lambda (symbol)
                           (cons symbol
                                 (if (and (boundp symbol)
                                          (streamp (symbol-value symbol)))
                                     (make-broadcast-stream)
                                     (list :thread-environment-marker))))
                         *thread-environment-variables*)))
    (with-thread-local-environment
      (loop for (symbol . marker) in markers do (set symbol marker)))
    (loop for (symbol . marker) in markers
          when (and (boundp symbol) (eq (symbol-value symbol) marker))
            do (push symbol leaked))
    (format stream "~&thread-environment-check: ~D variable~:P~%"
            (length *thread-environment-variables*))
    (format stream "~&  leaked out of the binding: ~:[none~;~:*~a~]~%"
            (nreverse leaked))
    leaked))

(defun check-fresh-linearray (&optional (stream *debug-io*))
  "LINEARRAY must be a fresh array: sharing DISPLA's scratch is the
display-corruption problem itself, so binding it to the same array would
look right and fix nothing."
  (let ((outer linearray) inner)
    (with-thread-local-environment (setq inner linearray))
    (let ((ok (and (not (eq outer inner)) (= (length inner) (length outer)))))
      (format stream "~&  linearray is a distinct array of the same size: ~a~%" ok)
      ok)))

;;; The race.  SIGN is the one worth racing: it is how COMPAR returns an
;;; answer, so if binding fixes SIGN it fixes the pattern generally.

#+(or ccl sbcl)
(defun check-race (&optional (stream *debug-io*))
  (flet ((race (wrap)
           (let ((bad 0) (threads '()))
             (declare (ignorable threads))
             (flet ((worker (tag)
                      (lambda ()
                        (funcall wrap
                                 (lambda ()
                                   (dotimes (i 2000)
                                     (setq sign tag)
                                     #+ccl (ccl:process-allow-schedule)
                                     #+sbcl (sb-thread:thread-yield)
                                     (unless (eq sign tag) (incf bad))))))))
               #+ccl
               (let ((done (ccl:make-semaphore)))
                 (dolist (tag '($pos $neg))
                   (let ((f (worker tag)))
                     (ccl:process-run-function
                      "w" (lambda () (funcall f) (ccl:signal-semaphore done)))))
                 (dotimes (i 2) (ccl:wait-on-semaphore done)))
               #+sbcl
               (progn
                 (dolist (tag '($pos $neg))
                   (push (sb-thread:make-thread (worker tag)) threads))
                 (mapc #'sb-thread:join-thread threads)))
             bad)))
    (let ((unbound (race (lambda (f) (funcall f))))
          (bound   (race (lambda (f) (with-thread-local-environment (funcall f))))))
      (format stream "~&  race on SIGN: ~D wrong of 4000 unbound, ~D bound~%"
              unbound bound)
      (when (zerop unbound)
        (format stream "~&  NOTE: the unbound race found nothing, so this run ~
                        proves little;~%        the threads probably did not ~
                        interleave.~%"))
      (zerop bound))))

#-(or ccl sbcl)
(defun check-race (&optional (stream *debug-io*))
  (format stream "~&  race skipped: no thread support known for this lisp~%")
  t)

(defun check-thread-environment (&optional (stream *debug-io*))
  "Returns T if the environment isolates everything it claims to."
  (let* ((leaked (check-bindings stream))
         (fresh (check-fresh-linearray stream))
         (raced (check-race stream))
         (ok (and (null leaked) fresh raced)))
    (format stream "~&thread-environment-check: ~:[FAILED~;ok~]~%" ok)
    ok))
