;;; Exact generating-function references and shared-cache regression checks.
(in-package :maxima)

(defun combin-cache-test-reference (kind count)
  ;; DLMF24.2.E1 and24.2.E6: reciprocal of (exp(t)-1)/t or cosh(t).
  ;; Work only with exact CL rationals, then convert coefficients to n! form.
  (let ((factorials (make-array (+ count 2) :initial-element 1))
        (denominator (make-array (1+ count) :initial-element 0))
        (inverse (make-array (1+ count) :initial-element 0)))
    (loop for n from 1 to (1+ count) do
          (setf (aref factorials n) (* n (aref factorials (1- n)))))
    (dotimes (n (1+ count))
      (setf (aref denominator n)
            (if (eq kind 'bern) (/ 1 (aref factorials (1+ n)))
                (if (evenp n) (/ 1 (aref factorials n)) 0))))
    (setf (aref inverse 0) 1)
    (loop for n from 1 to count do
          (setf (aref inverse n)
                (- (loop for k from 1 to n
                         sum (* (aref denominator k) (aref inverse (- n k)))))))
    (map 'vector #'* inverse (subseq factorials 0 (1+ count)))))

(defparameter *combin-cache-test-bern* (combin-cache-test-reference 'bern 128))
(defparameter *combin-cache-test-euler* (combin-cache-test-reference 'euler 128))

(defun combin-cache-test-number (value)
  (cond ((integerp value) value)
        ((and (consp value) (consp (car value)) (eq (caar value) 'rat))
         (/ (second value) (third value)))
        (t nil)))

(defun combin-cache-test-cold (thunk)
  ;; Reset the actual globals in this otherwise idle test process. Workers
  ;; must see the same arrays. Restore after THUNK has joined its workers.
  (let ((old-bn *bn*) (old-bd *bd*) (old-eu *eu*)
        (old-bern-limit (get 'bern 'lim)) (old-euler-limit (get '*eu* 'lim)))
    (unwind-protect
         (progn
           (setq *bn* (make-array 17 :adjustable t :element-type 'integer :initial-element 0)
                 *bd* (make-array 17 :adjustable t :element-type 'integer :initial-element 0)
                 *eu* (make-array 11 :adjustable t :element-type 'integer :initial-element 0))
           (loop for i from 1 below 17
                 for value = (aref *combin-cache-test-bern* (* 2 (1+ i))) do
                 (setf (aref *bn* i) (numerator value) (aref *bd* i) (denominator value)))
           (dotimes (i 11)
             (setf (aref *eu* i) (aref *combin-cache-test-euler* (* 2 (1+ i)))))
           (putprop 'bern 16 'lim) (putprop '*eu* 11 'lim)
           (funcall thunk))
      (setq *bn* old-bn *bd* old-bd *eu* old-eu)
      (putprop 'bern old-bern-limit 'lim) (putprop '*eu* old-euler-limit 'lim))))

(defun $combin_cache_reference_check ()
  (let (($zerobern t))
    (loop for n from 0 to 96 always
          (and (eql (combin-cache-test-number (ftake '%bern n))
                    (aref *combin-cache-test-bern* n))
               (eql (combin-cache-test-number (ftake '%euler n))
                    (aref *combin-cache-test-euler* n))))))

(defun $combin_cache_indexing_check ()
  (combin-cache-test-cold
   (lambda ()
     (let (($zerobern nil))
       (loop for n from 0 to 48 always
             (and (eql (combin-cache-test-number (ftake '%bern n))
                       (aref *combin-cache-test-bern* (if (< n 3) n (* 2 (1- n)))))
                  (eql (combin-cache-test-number (ftake '%euler n))
                       (aref *combin-cache-test-euler* (* 2 n)))))))))

(defun $combin_cache_publication_check ()
  (combin-cache-test-cold
   (lambda ()
     (let ((original (symbol-function 'putprop)) (seen nil) (correct nil))
       (unwind-protect
            (progn
              (setf (symbol-function 'putprop)
                    (lambda (symbol value indicator)
                      (prog1 (funcall original symbol value indicator)
                        (when (and (eq symbol 'bern) (eq indicator 'lim)
                                   (= value 17) (not seen))
                          (setq seen t)
                          (let (($zerobern t) ($errormsg nil))
                            (setq correct
                                  (handler-case
                                      (eql (combin-cache-test-number (ftake '%bern 36))
                                           (aref *combin-cache-test-bern* 36))
                                    (error () nil))))))))
              (let (($zerobern t))
                (and (eql (combin-cache-test-number (ftake '%bern 36))
                          (aref *combin-cache-test-bern* 36)) seen correct)))
         (setf (symbol-function 'putprop) original))))))

(defun $combin_cache_progress_check ()
  (combin-cache-test-cold
   (lambda ()
     (let (($zerobern t))
       (ftake '%bern 60) (ftake '%euler 40)
       (let ((bern-limit (get 'bern 'lim)) (euler-limit (get '*eu* 'lim)))
         ;; A smaller in-flight producer can finish after the larger one.
         ;; Completed cache prefixes must remain available in that ordering.
         (and (eql (combin-cache-test-number (bern 36)) (aref *combin-cache-test-bern* 36))
              (eql (euler 24) (aref *combin-cache-test-euler* 24))
              (>= (get 'bern 'lim) bern-limit)
              (>= (get '*eu* 'lim) euler-limit)))))))

(defun $combin_cache_capacity_check ()
  (combin-cache-test-cold
   (lambda ()
     ;; Capacity may exceed the completed prefix after a prior interrupted
     ;; calculation. A smaller cache miss must not shrink that reservation.
     (setq *bn* (adjust-array *bn* 80) *bd* (adjust-array *bd* 80)
           *eu* (adjust-array *eu* 80))
     (let (($zerobern t))
       (and (eql (combin-cache-test-number (ftake '%bern 36)) (aref *combin-cache-test-bern* 36))
            (eql (ftake '%euler 24) (aref *combin-cache-test-euler* 24))
            (>= (length *bn*) 80) (>= (length *bd*) 80) (>= (length *eu*) 80))))))

(defun $combin_cache_parallel_check (fallback-p)
  (loop for kind in '(bern euler) always
        (loop repeat 12 always
              (combin-cache-test-cold
               (lambda ()
                 (let* (($parallel_threads (if fallback-p 1 4))
                        (operator (if (eq kind 'bern) '%bern '%euler))
                        (reference (if (eq kind 'bern) *combin-cache-test-bern* *combin-cache-test-euler*))
                        (indices (loop for i below 32 collect (+ 40 (* 2 (mod (* i 13) 32)))))
                        (actual (call-in-parallel
                                 (loop for n in indices collect
                                       (let ((index n))
                                         (lambda ()
                                           (let (($zerobern t))
                                             (combin-cache-test-number (ftake operator index)))))))))
                   (every #'eql actual (mapcar (lambda (n) (aref reference n)) indices))))))))

(defun $combin_cache_callback_check (mode)
  (combin-cache-test-cold
   (lambda ()
     (let ((original (symbol-function 'nxtbincoef)) (seen nil) (inner-ok nil))
       (unwind-protect
            (progn
              (setf (symbol-function 'nxtbincoef)
                    (lambda (m nom n)
                      (unless seen
                        (setq seen t)
                        (setq inner-ok
                              (parallel-input-run
                               (lambda ()
                                 (let (($zerobern t))
                                   (eql (combin-cache-test-number (ftake '%bern 40))
                                        (aref *combin-cache-test-bern* 40))))
                               (if (eq mode '$worker)
                                   #+(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) '$worker
                                   #-(or sb-thread (and ccl openmcl-native-threads) (and ecl threads)) '$fallback
                                   '$serial))))
                      (funcall original m nom n)))
              (let (($zerobern t))
                (and (eql (combin-cache-test-number (ftake '%bern 36))
                          (aref *combin-cache-test-bern* 36)) seen inner-ok)))
         (setf (symbol-function 'nxtbincoef) original))))))

(defun $combin_cache_polynomial_check ()
  ;; Exact finite differences and sums use ordinary integer/rational arithmetic.
  (let (($zerobern t) (seed 70139))
    (loop repeat 64 always
          (progn
            (setq seed (mod (+ (* seed 1103515245) 12345) 2147483648))
            (let* ((n (1+ (mod seed 16))) (upper (1+ (mod (ash seed -8) 12)))
                   (expected (loop for k below upper sum (expt k n)))
                   (actual (/ (- (combin-cache-test-number ($bernpoly upper (1+ n)))
                                 (combin-cache-test-number (ftake '%bern (1+ n))))
                              (1+ n))))
              (= actual expected))))))

(defun $combin_cache_error_check ()
  (combin-cache-test-cold
   (lambda ()
     (let ((original (symbol-function 'nxtbincoef)) (seen nil) (failed nil))
       (setq failed
        (unwind-protect
            (progn
              (setf (symbol-function 'nxtbincoef)
                    (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (setq seen t)
                      (error "Intentional cache recurrence failure")))
              (let (($zerobern t))
                (and (handler-case (progn (ftake '%bern 36) nil) (error () t))
                     (handler-case (progn (ftake '%euler 24) nil) (error () t))
                     seen)))
         (setf (symbol-function 'nxtbincoef) original)))
       ;; A failed producer must leave its reserved slots unavailable; the
       ;; next request must compute them and retain exact public values.
       (and failed (= (get 'bern 'lim) 16) (= (get '*eu* 'lim) 11)
            ($combin_cache_reference_check))))))
