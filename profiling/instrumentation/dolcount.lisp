;;; Counts DOLLARIFY calls (report with *DC-CALLS*) and times one call of
;;; the original against a memoized lookup with (DC-BENCH).
(in-package :maxima)
(defvar *dc-orig* (fdefinition 'dollarify))
(defvar *dc-calls* 0)
(defvar *dc-time* 0)
(setf (fdefinition 'dollarify)
      (lambda (l) (incf *dc-calls*) (funcall *dc-orig* l)))
(defun dc-bench ()
  (let ((l '($x $y)) (n 200000))
    (flet ((tm (f) (let ((t0 (get-internal-run-time)))
                     (dotimes (i n) (funcall f l))
                     (/ (- (get-internal-run-time) t0) (float internal-time-units-per-second) n 1e-6))))
      (let ((memo (make-hash-table :test 'eq)))
        (format *debug-io* "~&DC per call: original ~,2F us, memo lookup ~,3F us~%"
                (tm *dc-orig*)
                (tm (lambda (x) (or (gethash x memo) (setf (gethash x memo) (funcall *dc-orig* x))))))))))
