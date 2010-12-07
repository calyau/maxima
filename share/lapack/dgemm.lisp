;;; Simple raw interface to LAPACK dgemm, general real matrix
;;; multiplication.
(in-package :maxima)

(defun %%dgemm (a b &key c transpose_a transpose_b (alpha 1d0) (beta 0d0))
  (multiple-value-bind (a-nrows a-ncols)
      (maxima-matrix-dims a)
    (multiple-value-bind (b-nrows b-ncols)
	(maxima-matrix-dims b)
      (let ((alpha ($float alpha))
	    (beta ($float beta))
	    (matrix-a (lapack-lispify-matrix a a-nrows a-ncols))
	    (matrix-b (lapack-lispify-matrix b a-nrows a-ncols))
	    (matrix-c (cond ((and c (not (zerop beta)))
			     (lapack-lispify-matrix c a-nrows b-ncols))
			    (t
			     ;; No C matrix given, or beta is zero.
			     ;; Force beta to be zero to tell LAPACK
			     ;; not to add C.  But we still need to
			     ;; create a matrix.
			     (setf beta 0d0)
			     (make-array (* a-nrows b-ncols) :element-type 'double-float))))
	    (trans-a (if transpose_a "t" "n"))
	    (trans-b (if transpose_b "t" "n")))
	(blas::dgemm trans-a trans-b
		     a-nrows b-ncols a-ncols
		     alpha
		     matrix-a a-nrows
		     matrix-b b-nrows
		     beta
		     matrix-c a-nrows)
	;; matrix-c contains the desired result.
	(lapack-maxify-matrix a-nrows b-ncols matrix-c)))))

;; Main interface from maxima to Lapack routine dgemm.  Just parses
;; args and calls %%dgemm to do the dirty work.
(defun $%dgemm (a b options)
  (let* ((args (lispify-maxima-keyword-options
		(cdr options)
		'($c $transpose_a $transpose_b $alpha $beta))))
    (apply #'%%dgemm a b args)))
    