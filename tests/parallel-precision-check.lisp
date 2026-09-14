;;; Precision inheritance and independent rational error bounds.
(in-package :maxima)

(defun parallel-precision-test-state ()
  (copy-tree (list $fpprec fpprec *bigfloatone* *bigfloatzero*
                   *bfhalf* *bfmhalf*)))

(defun parallel-precision-test-rational (value)
  ;; Decode the stored binary value exactly; do not round it again at the
  ;; requested precision or use Maxima's approximate RAT conversion.
  (* (second value)
     (expt 2 (- (third value) (bigfloat-prec value)))))

(defun parallel-precision-test-sample (rationals)
  (list (parallel-precision-test-state)
        (mapcar (lambda (value)
                  ($bfloat (cl-rat-to-maxima value)))
                rationals)))

(defun parallel-precision-test-accurate-p (sample state rationals digits)
  (and (equal (first sample) state)
       (= (length (second sample)) (length rationals))
       (every (lambda (value exact)
                (and ($bfloatp value)
                     (< (/ (abs (- (parallel-precision-test-rational value)
                                   exact))
                           (max 1 (abs exact)))
                        (expt 10 (- 3 digits)))))
              (second sample) rationals)))

(defun $parallel_precision_inherited (mode digits)
  (with-thread-local-environment
    (mset '$fpprec digits)
    (let* ((before (parallel-precision-test-state))
           (rationals '(1/3 -2/7 123456789/999999991))
           (result (parallel-input-run
                    (lambda () (parallel-precision-test-sample rationals))
                    mode))
           (samples (if (eq mode '$public) result (list result))))
      (and (equal before (parallel-precision-test-state))
           (every (lambda (sample)
                    (parallel-precision-test-accurate-p
                     sample before rationals digits))
                  samples)))))

(defun $parallel_precision_restored (mode fail-p)
  (with-thread-local-environment
    (mset '$fpprec 37)
    (let* ((before (parallel-precision-test-state))
           ($errormsg nil)
           (result (errcatch
                     (parallel-input-run
                      (lambda ()
                        (mset '$fpprec 83)
                        (when fail-p (merror "Expected precision test error"))
                        7)
                      mode))))
      (and (if fail-p (null result) (not (null result)))
           (equal before (parallel-precision-test-state))))))

(defun $parallel_precision_internal (mode)
  ;; Algorithms sometimes bind the working bit precision temporarily.
  ;; Copy that state as it is; rebuilding it from $FPPREC loses it.
  (with-thread-local-environment
    (mset '$fpprec 47)
    (let* ((fpprec (+ fpprec 9))
           (before (parallel-precision-test-state))
           (result (parallel-input-run #'parallel-precision-test-state mode)))
      (and (equal before (parallel-precision-test-state))
           (if (eq mode '$public)
               (every (lambda (state) (equal state before)) result)
               (equal result before))))))

(defun $parallel_precision_random ()
  ;; The actual execution site is forced, so the test does not pass merely
  ;; because the caller consumed every item before workers started.
  (loop with state = 78173
        repeat 64
        always
        (progn
          (setq state (mod (+ (* state 1664525) 1013904223) (expt 2 32)))
          (with-thread-local-environment
            (let* ((digits (+ 12 (mod state 189)))
                   (exact (/ (- (mod state 20001) 10000)
                             (+ 3 (mod (ash state -8) 997)))))
              (mset '$fpprec digits)
              (let* ((before (parallel-precision-test-state))
                     (result (parallel-input-run
                              (lambda ()
                                (parallel-precision-test-sample (list exact)))
                              (if (parallel-threads-p) '$worker '$fallback))))
                (and (equal before (parallel-precision-test-state))
                     (parallel-precision-test-accurate-p
                      result before (list exact) digits))))))))
