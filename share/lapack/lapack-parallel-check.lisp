;;; Explicit LAPACK guards and independent small-matrix references.
(in-package :maxima)

(defun lapack-test-mode (mode)
  (if (and (eq mode '$worker) (not (parallel-threads-p))) '$fallback mode))

(defun lapack-test-error-p (observation operation)
  (and (null (first observation))
       (equal (second observation)
              (list '(mlist simp)
                    (intl:gettext
                     "~M: this function cannot run in a parallel computation.")
                    operation))))

(defun $lapack_parallel_guard_check (mode clear-input-p)
  (let ((original (symbol-function 'lapack-lispify-matrix)))
    (unwind-protect
         (every
          (lambda (operation)
            (let ((entered nil))
              ;; Stop the unguarded parent before translated numeric code.
              ;; A wrong guard cannot turn this negative control into a race.
              (setf (symbol-function 'lapack-lispify-matrix)
                    (lambda (&rest args)
                      (declare (ignore args))
                      (setq entered t)
                      (merror "lapack-test: numeric converter entered")))
              (let ((observation
                      (parallel-input-observe
                       (lambda ()
                         (parallel-input-run
                          (lambda ()
                            (flet ((call-adapter ()
                                     (mfuncall operation
                                               #$matrix([2,1],[1,2])$)))
                              (if clear-input-p
                                  (let ((*parallel-input-forbidden* nil))
                                    (call-adapter))
                                  (call-adapter))))
                          (lapack-test-mode mode)))
                       "" "")))
                (and (not entered) (lapack-test-error-p observation operation)))))
          '($dgeev $dgesvd $zgeev $zheev $dgeqrf))
      (setf (symbol-function 'lapack-lispify-matrix) original))))

(defun $lapack_parallel_load_check (mode &optional direct-p load-path)
  (let ((original (symbol-function 'maxima-load-pathname-directory))
        (entered nil))
    (unwind-protect
         (progn
           ;; This helper is called by the package loader before loading its
           ;; build description. No compiler or translated code is reached.
           (setf (symbol-function 'maxima-load-pathname-directory)
                 (lambda ()
                   (setq entered t)
                   (merror "lapack-test: package initialization entered")))
           (let ((observation
                   (parallel-input-observe
                    (lambda ()
                      (parallel-input-run
                       (lambda ()
                         (if direct-p
                             (load (or load-path
                                       ($file_search "load-lapack.lisp"
                                                     $file_search_lisp)))
                             ($load (or load-path "lapack"))))
                       (lapack-test-mode mode)))
                    "" "")))
             (and (not entered) (lapack-test-error-p observation '$lapack))))
      (setf (symbol-function 'maxima-load-pathname-directory) original))))

(defun lapack-test-maxima-matrix (matrix-rows)
  (apply #'mfuncall '$matrix
         (mapcar (lambda (row-values)
                   (cons '(mlist)
                         (mapcar (lambda (number)
                                   (add (realpart number)
                                        (mul '$%i (imagpart number))))
                                 row-values)))
                 matrix-rows)))

(defun lapack-test-number (expression)
  (complex ($float ($realpart expression)) ($float ($imagpart expression))))

(defun lapack-test-rows (matrix-value)
  (assert ($matrixp matrix-value))
  (mapcar (lambda (row-values) (mapcar #'lapack-test-number (cdr row-values)))
          (cdr matrix-value)))

(defun lapack-test-transpose (matrix-rows)
  (apply #'mapcar #'list matrix-rows))

(defun lapack-test-adjoint (matrix-rows)
  (mapcar (lambda (row-values) (mapcar #'conjugate row-values))
          (lapack-test-transpose matrix-rows)))

(defun lapack-test-product (left-rows right-rows)
  (let ((columns (lapack-test-transpose right-rows)))
    (mapcar (lambda (row-values)
              (mapcar (lambda (column-values)
                        (assert (= (length row-values) (length column-values)))
                        (loop for x in row-values for y in column-values
                              sum (* x y))) columns)) left-rows)))

(defun lapack-test-identity (size)
  (loop for row-index below size collect
        (loop for column-index below size collect
              (if (= row-index column-index) 1 0))))

(defun lapack-test-close (actual expected)
  (<= (abs (- actual expected)) (* 5d-12 (max 1 (abs expected)))))

(defun lapack-test-close-rows (actual expected)
  (and (= (length actual) (length expected))
       (every (lambda (entry-a entry-b)
                (and (= (length entry-a) (length entry-b))
                     (every #'lapack-test-close entry-a entry-b)))
              actual expected)))

(defun lapack-test-two-values-p (actual expected)
  (and (= (length actual) 2)
       (or (every #'lapack-test-close actual expected)
           (every #'lapack-test-close actual (reverse expected)))))

(defun lapack-test-diagonal (diagonal nrows ncols)
  (loop for row-index below nrows collect
        (loop for column-index below ncols collect
              (if (and (= row-index column-index)
                       (< row-index (length diagonal)))
                  (nth row-index diagonal) 0))))

(defun lapack-test-unit-columns-p (matrix-rows)
  (every (lambda (column-values)
           (lapack-test-close
            (loop for value in column-values sum (expt (abs value) 2)) 1))
         (lapack-test-transpose matrix-rows)))

(defun lapack-test-eigen-case (operation matrix-rows expected right-p left-p)
  (let* ((input (lapack-test-maxima-matrix matrix-rows))
         (saved (copy-tree input))
         (result (if (eq operation '$zheev)
                     (mfuncall operation input right-p)
                     (mfuncall operation input right-p left-p)))
         (eigenvalues (mapcar #'lapack-test-number (cdr (second result))))
         (diagonal (lapack-test-diagonal eigenvalues 2 2))
         (right (third result)) (left (fourth result)))
    (and (alike1 input saved)
         (lapack-test-two-values-p eigenvalues expected)
         (if right-p
             (let ((vectors (lapack-test-rows right)))
               (and (lapack-test-unit-columns-p vectors)
                    (lapack-test-close-rows
                     (lapack-test-product matrix-rows vectors)
                     (lapack-test-product vectors diagonal))))
             (null right))
         (if left-p
             (let* ((vectors (lapack-test-rows left))
                    (adjoint (lapack-test-adjoint vectors)))
               (and (lapack-test-unit-columns-p vectors)
                    (lapack-test-close-rows
                     (lapack-test-product adjoint matrix-rows)
                     (lapack-test-product diagonal adjoint))))
             (null left)))))

(defun $lapack_serial_eigen_check ()
  (let ((seed 73193))
    (loop repeat 24 always
          (progn
            (setq seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
            (let ((entry-a (- (mod seed 31) 15))
                  (entry-b (1+ (mod (ash seed -8) 7)))
                  (entry-c (1+ (mod (ash seed -16) 5))))
              (and
               (every
                (lambda (flags)
                  (and
                   (lapack-test-eigen-case '$dgeev
                    (list (list entry-a entry-b) (list entry-b entry-a))
                    (list (+ entry-a entry-b) (- entry-a entry-b))
                    (first flags) (second flags))
                   (lapack-test-eigen-case '$dgeev
                    (list (list entry-a (- entry-b)) (list entry-b entry-a))
                    (list (complex entry-a entry-b)
                          (complex entry-a (- entry-b)))
                    (first flags) (second flags))
                   (lapack-test-eigen-case '$zgeev
                    (list (list (complex entry-a entry-c) entry-b)
                          (list 0 (complex entry-b (- entry-c))))
                    (list (complex entry-a entry-c)
                          (complex entry-b (- entry-c)))
                    (first flags) (second flags))))
                '((nil nil) (t nil) (nil t) (t t)))
               (every (lambda (vectors-p)
                        (lapack-test-eigen-case '$zheev
                         (list (list entry-a (complex 0 entry-b))
                               (list (complex 0 (- entry-b)) entry-a))
                         (list (+ entry-a entry-b) (- entry-a entry-b))
                         vectors-p nil))
                      '(nil t))))))))

(defun lapack-test-svd-case (matrix-rows left-p right-p)
  (let* ((input (lapack-test-maxima-matrix matrix-rows))
         (saved (copy-tree input))
         (nrows (length matrix-rows)) (ncols (length (first matrix-rows)))
         (result ($dgesvd input left-p right-p))
         (singular (cdr (second result)))
         (u (third result)) (vt (fourth result)))
    (and (alike1 input saved) (= (length singular) (min nrows ncols))
         (every (lambda (s) (and (realp s) (>= s 0))) singular)
         (loop for tail on singular while (cdr tail)
               always (>= (car tail) (cadr tail)))
         ;; Independent Frobenius norm identity, including omitted vectors.
         (lapack-test-close
          (loop for s in singular sum (* s s))
          (loop for row-values in matrix-rows sum
                (loop for value in row-values sum (* value value))))
         (if left-p
             (let ((vectors (lapack-test-rows u)))
               (lapack-test-close-rows
                (lapack-test-product (lapack-test-adjoint vectors) vectors)
                (lapack-test-identity nrows)))
             (null u))
         (if right-p
             (let ((vectors (lapack-test-rows vt)))
               (lapack-test-close-rows
                (lapack-test-product vectors (lapack-test-adjoint vectors))
                (lapack-test-identity ncols)))
             (null vt))
         (or (not (and left-p right-p))
             (lapack-test-close-rows
              (lapack-test-product
               (lapack-test-product (lapack-test-rows u)
                                    (lapack-test-diagonal singular nrows ncols))
               (lapack-test-rows vt)) matrix-rows)))))

(defun lapack-test-factorization-case (matrix-rows)
  (let* ((input (lapack-test-maxima-matrix matrix-rows))
         (saved (copy-tree input))
         (result ($dgeqrf input))
         (q-rows (lapack-test-rows (second result)))
         (r-rows (lapack-test-rows (third result))))
    (and
     (alike1 input saved)
     (lapack-test-close-rows
      (lapack-test-product q-rows r-rows) matrix-rows)
     (lapack-test-close-rows
      (lapack-test-product (lapack-test-adjoint q-rows) q-rows)
      (lapack-test-identity (length matrix-rows)))
     (every (lambda (flags)
              (lapack-test-svd-case matrix-rows (first flags) (second flags)))
            '((nil nil) (t nil) (nil t) (t t))))))

(defun $lapack_serial_factorization_check ()
  (let ((seed 92813))
    (loop repeat 24 always
          (progn
            (setq seed (mod (+ (* seed 1664525) 1013904223) (expt 2 32)))
            (let* ((entry-a (- (mod seed 17) 8))
                   (entry-b (- (mod (ash seed -8) 13) 6))
                   (entry-c (- (mod (ash seed -16) 19) 9))
                   (wide (list (list entry-a entry-b 0)
                               (list 0 entry-c entry-a))))
              (every #'lapack-test-factorization-case
                     (list wide (lapack-test-transpose wide)
                           (list (list entry-a 0) (list 0 entry-c)))))))))

(defun $lapack_serial_boundary_check ()
  (and
   (every #'lapack-test-factorization-case
          '(((0)) ((-7)) ((0 0 0)) ((0) (0) (0))
            ((-3 0 4)) ((-3) (0) (4)) ((0 0) (0 0))))
   ;; Zero matrices exercise repeated eigenvalues without prescribing a basis.
   (every (lambda (operation)
            (every (lambda (flags)
                     (lapack-test-eigen-case operation '((0 0) (0 0)) '(0 0)
                                             (first flags) (second flags)))
                   '((nil nil) (t nil) (nil t) (t t))))
          '($dgeev $zgeev))
   (every (lambda (vectors-p)
            (lapack-test-eigen-case '$zheev '((0 0) (0 0)) '(0 0)
                                    vectors-p nil)) '(nil t))))

(defun $lapack_parallel_supported_check (fallback-p)
  (let (($parallel_threads (if fallback-p 1 4)))
    (every
     #'identity
     (call-in-parallel
      (loop for sample-index below 32 collect
            (let* ((entry-a (+ 3 sample-index)) (entry-b (1+ (mod sample-index 5)))
                   (matrix-rows (list (list entry-a entry-b)
                                      (list entry-b (+ entry-a 2))))
                   (answer (list (list (- sample-index 8)) (list (+ sample-index 1))))
                   (rhs (lapack-test-product matrix-rows answer))
                   (input (lapack-test-maxima-matrix matrix-rows))
                   (rhs-input (lapack-test-maxima-matrix rhs))
                   (saved (copy-tree input)) (rhs-saved (copy-tree rhs-input))
                   (complex-input
                     (lapack-test-maxima-matrix
                      (list (list (complex 0 entry-a) entry-b)
                            (list (- entry-b) (complex 0 (+ entry-a 2))))))
                   (complex-saved (copy-tree complex-input)))
              (lambda ()
                (and
                 (lapack-test-close-rows
                  (lapack-test-rows ($dgesv input rhs-input)) answer)
                 (lapack-test-close-rows
                  (lapack-test-rows ($dgemm input input))
                  (lapack-test-product matrix-rows matrix-rows))
                 (lapack-test-close ($dlange '$one_norm input)
                                    (+ entry-a 2 entry-b))
                 (lapack-test-close ($zlange '$one_norm complex-input)
                                    (+ entry-a 2 entry-b))
                 (alike1 input saved) (alike1 rhs-input rhs-saved)
                 (alike1 complex-input complex-saved)))))))))
