;;; Compiled zeta argument classification and independent numeric references.
(in-package :maxima)

(defun zeta-test-rational (value)
  ;; Decode Maxima's stored binary bigfloat value without rounding it again.
  (* (second value) (expt 2 (- (third value) (bigfloat-prec value)))))

(defun zeta-test-apery-bounds (digits)
  ;; DLMF 25.6.9: zeta(3) = 5/2 sum((-1)^(k-1)/(k^3 binomial(2k,k))).
  ;; Successive partial sums bracket the limit of this alternating series.
  ;; All operations here are exact CL integer/rational arithmetic.
  (loop with sum = 0 with binomial = 2
        with threshold = (expt 10 (- (+ digits 8)))
        for k from 1
        for term = (/ (if (oddp k) 1 -1) (* k k k binomial))
        for next = (+ sum term)
        when (< (abs term) threshold)
          return (values (* 5/2 (min sum next)) (* 5/2 (max sum next)))
        do (setq sum next
                 binomial (/ (* binomial (+ (* 2 k) 2) (+ (* 2 k) 1))
                             (* (+ k 1) (+ k 1))))))

(defun $zeta_bigfloat_check (digits mode)
  (with-thread-local-environment
    (mset '$fpprec digits)
    (multiple-value-bind (lower upper) (zeta-test-apery-bounds digits)
      (let* ((argument ($bfloat 3))
             (before (copy-tree argument))
             (result (parallel-input-run
                      (lambda () (to (float-zeta argument))) mode))
             (tolerance (expt 10 (- 2 digits))))
        (and ($bfloatp result) (equal before argument)
             (<= (- lower tolerance) (zeta-test-rational result)
                 (+ upper tolerance)))))))

(defun $zeta_dispatch_check ()
  ;; Capture the normalized argument at the first numerical operation.
  ;; This characterizes conversion, including CLISP's mixed rational/float
  ;; complex extension, without using the zeta algorithm as its own oracle.
  (let ((original (symbol-function 'bigfloat:realpart)))
    (unwind-protect
         (progn
           (setf (symbol-function 'bigfloat:realpart)
                 (lambda (value) (throw 'zeta-normalized value)))
           (every
            (lambda (value)
              (let* ((input (bigfloat:to value))
                     (wanted
                      (cond ((rationalp input) (float input))
                            ((and (complexp input)
                                  (rationalp (realpart input))
                                  (rationalp (imagpart input)))
                             (coerce input '(complex flonum)))
                            (t input)))
                     (observed (catch 'zeta-normalized (float-zeta value))))
                (if (typep wanted 'bigfloat::numeric)
                    (eq wanted observed)
                    (and (equal (type-of wanted) (type-of observed))
                         (eql wanted observed)))))
            (list 0 1 -1 2/3 (expt 10 100)
                  (complex 2/3 4/5) (complex -5/7 11/3)
                  1.0f0 1.0d0 1.0l0
                  (complex 2.0d0 3.0d0) (complex 2/3 3.0d0)
                  (bigfloat:bigfloat 3) (bigfloat:bigfloat 2 1))))
      (setf (symbol-function 'bigfloat:realpart) original))))

(defun $zeta_random_check ()
  (loop with state = 62419 repeat 32 always
        (progn
          (setq state (mod (+ (* state 1664525) 1013904223) (expt 2 32)))
          ($zeta_bigfloat_check (+ 12 (mod state 70)) '$serial))))
