;;; Exact references and deterministic re-entry checks for SUMSUM.
(in-package :maxima)

(defun $summation_serial_check ()
  (and (equal (multiple-value-list (sumsum 11 '$k 1 1 nil 1)) '(nil (11)))
       (equal (multiple-value-list (sumsum 0 '$k 1 3 nil 1)) '(nil (0)))
       ;; An entirely unevaluated sum returns one value, a partial sum two.
       (equal (multiple-value-list (sumsum '(($f) $k) '$k 1 3 nil 1))
              '(((%sum) (($f) $k) $k 1 3)))
       (equal (multiple-value-list (sumsum '((mplus) 11 (($f) $k)) '$k 1 3 nil 1))
              '(((%sum) (($f simp) $k) $k 1 3) (33)))))

(defun $summation_reentry_check (mode)
  ;; Intercept the shared simplifier at a numeric argument unique to the
  ;; outer sum. The nested call completes before the outer one resumes.
  ;; This forces the critical ordering without relying on scheduler timing.
  (let ((original (symbol-function 'simplifya)) (entered nil) (inner-ok nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'simplifya)
                 (lambda (expression simplified-p)
                   (when (and (eq mode '$throw) entered (eql expression 22))
                     (throw 'summation-test-exit t))
                   (when (and (eql expression 11) (not entered))
                     (setq entered t)
                     (flet ((inner ()
                              ;; Both result accumulators must be isolated.
                              (let ((values (multiple-value-list
                                             (sumsum '((mplus) 22 (($f) $k))
                                                     '$k 1 1 nil 1))))
                                (and (equal values
                                            '(((%sum) (($f simp) $k) $k 1 1) (22)))
                                     t))))
                       (setq inner-ok
                             (case mode
                               ($worker
                                (parallel-input-run #'inner
                                 #+(or sb-thread (and ccl openmcl-native-threads)
                                       (and ecl threads)) '$worker
                                 #-(or sb-thread (and ccl openmcl-native-threads)
                                       (and ecl threads)) '$fallback))
                               ($throw (catch 'summation-test-exit (inner) nil))
                               (otherwise (inner))))))
                   (funcall original expression simplified-p)))
           (let* ((throw-p (eq mode '$throw))
                  (expression (if throw-p '((mplus) 7 11 (($g) $k)) 11))
                  (values (multiple-value-list (sumsum expression '$k 1 1 nil 1))))
             (and entered inner-ok
                  (equal values (if throw-p '(((%sum) (($g simp) $k) $k 1 1) (11 7)) '(nil (11)))))))
      (setf (symbol-function 'simplifya) original))))

(defun summation-test-case (power coefficient constant lo hi)
  (let* ((expression (list '(mplus)
                           (if (zerop power) 1 (list '(mexpt) '$k power))
                           (list '(mtimes) (div (numerator coefficient)
                                               (denominator coefficient)) '$k)
                           constant))
         (before (copy-tree expression))
         (values (multiple-value-list (sumsum expression '$k lo hi nil 1)))
         ;; Exact finite addition is independent of the Bernoulli formulas
         ;; used by the polynomial summation implementation.
         (expected (loop for k from lo to hi
                         sum (+ (expt k power) (* coefficient k) constant))))
    (and (equal expression before)
         (= (length values) 2) (null (first values))
         (alike1 ($ratsimp (addn (second values) nil))
                 (div (numerator expected) (denominator expected))))))

(defun $summation_random_check ()
  (loop with seed = 27931 repeat 128 always
        (progn
          (setq seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
          (let ((lo (- (mod seed 7) 3)))
            (summation-test-case (mod seed 6)
                                 (/ (- (mod seed 17) 8) (1+ (mod seed 5)))
                                 (- (mod seed 11) 5) lo (+ lo (mod seed 13)))))))

(defun $summation_parallel_check (fallback-p)
  (let (($parallel_threads (if fallback-p 1 4)))
    (every (lambda (value) (eq value t))
           (call-in-parallel
            (loop for index below 48 collect
                  (let ((i index))
                    (lambda ()
                      (summation-test-case (mod i 6) (/ (- i 24) 3)
                                           (- i 20) 1 (+ 2 (mod i 12))))))))))

(defun $summation_match_state_check ()
  (let ((*a 'summation-test-coefficient) (*n 'summation-test-power))
    (declare (special *a *n))
    (and (equal (multiple-value-list (sumsum '((mexpt) $k 2) '$k 1 3 nil 1))
                '(nil (14)))
         (eq *a 'summation-test-coefficient)
         (eq *n 'summation-test-power))))
