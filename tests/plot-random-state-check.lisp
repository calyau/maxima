;;; Plot filename generation must not consume or replace the caller's RNG.
(in-package :maxima)

(defun plot-rng-test-word-p (word count)
  (and (stringp word) (= (length word) (max count 0))
       (every (lambda (c) (find c "0123456789abcdefghijklmnopqrstuvwxyz")) word)))

(defun plot-rng-test-call (count &optional error-p)
  (let* ((*random-state* (make-random-state nil))
         (saved *random-state*) (reference (make-random-state saved))
         (failed nil) word)
    (handler-case (setq word (random-name count))
      (error () (setq failed t)))
    (and (eq failed error-p)
         (or failed (plot-rng-test-word-p word count))
         (eq *random-state* saved)
         (loop repeat 32 always (= (random 1000000) (random 1000000 reference))))))

(defun $plot_rng_preservation_check ()
  (every #'plot-rng-test-call '(0 -1 -7 1 16 64 129)))

(defun $plot_rng_error_check ()
  (every (lambda (count) (plot-rng-test-call count t)) '(nil t "16" $x)))

(defun $plot_rng_generated_check ()
  (let ((seed 32851))
    (loop repeat 128 always
          (progn
            (setq seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
            (plot-rng-test-call (- (mod seed 72) 7))))))

(defun $plot_rng_parallel_check (fallback-p)
  (let (($parallel_threads (if fallback-p 1 4)))
    (every #'identity
           (call-in-parallel
            (loop for i below 64 collect
                  (let ((count (mod (* i 13) 65)))
                    (lambda () (plot-rng-test-call count))))))))

(defun $plot_rng_path_check ()
  ;; The registration table belongs to this otherwise idle test process.
  ;; No file is written by these naming helpers.
  (let* ((*random-state* (make-random-state nil))
         (saved *random-state*) (reference (make-random-state saved))
         (*temp-files-list* (make-hash-table :test 'equal))
         (options (list '$plot_format '$gnuplot '$gnuplot_term '$default))
         (path (plot-set-gnuplot-script-file-name options)))
    (and (plot-rng-test-word-p (pathname-name path) 16)
         (string= (pathname-type path) "gnuplot")
         (gethash path *temp-files-list*)
         (eq *random-state* saved)
         (loop repeat 32 always (= (random 1000000) (random 1000000 reference))))))

(defun $plot_rng_maxima_check ()
  ;; Maxima's user-visible RNG is separate from the Common Lisp RNG.
  (let* ((mt19937::*random-state* ($make_random_state 17391))
         (saved mt19937::*random-state*)
         (reference (mt19937::make-random-state saved)))
    (random-name 16)
    (and (eq mt19937::*random-state* saved)
         (loop repeat 32 always (= ($random 1000000)
                                    (mt19937::random 1000000 reference))))))

(defun $plot_rng_fresh_check ()
  ;; Repeated names must not become identical merely because the caller's
  ;; state stays unchanged. This is a freshness smoke check, not a guarantee
  ;; that random names can never collide.
  (let* ((*random-state* (make-random-state nil))
         (names (loop repeat 32 collect (random-name 16))))
    (not (every (lambda (name) (string= name (first names))) (rest names)))))
