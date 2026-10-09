;;; Round 2. Counts, per sign of the argument, how often the MNQP call in
;;; SIGN-MABS proves the argument nonzero, and the time it takes. Report with
;;; mabsreport().
(in-package :maxima)
(defvar *mabs* (make-hash-table :test 'equal))
(defun sign-mabs (x)
  (let ((*complexsign* t))
    (sign (cadr x))
    (cond ((member sign '($pos $zero) :test #'eq))
	  ((member sign '($neg $pn) :test #'eq) (setq sign '$pos))
	  ((let* ((s0 sign) (t0 (get-internal-run-time))
                  (r (eq t (mnqp 0 (cadr x))))
                  (e (gethash (list s0 r) *mabs*)))
             (unless e (setq e (setf (gethash (list s0 r) *mabs*) (list 0 0))))
             (incf (first e)) (incf (second e) (- (get-internal-run-time) t0))
             r)
           (setq sign '$pos)) ; abs(nonzero) > 0
	  (t (setq sign '$pz minus nil evens (nconc odds evens) odds nil)))))
(defun $mabsreport ()
  (maphash (lambda (k v) (format *debug-io* "~&~30a ~10d ~8,2f s~%" k (first v) (/ (second v) internal-time-units-per-second))) *mabs*))
