(in-package "MAXIMA")

(defun maxima-matrix-dims (a)
  (let ((row (second a)))
    ;; Should we check that all rows have the same length?
    (values (length (cdr a)) (length (cdr row)))))

(defun lapack-lispify-matrix (a nrow ncol)
  "Convert a Maxima matrix A of dimension NROW and NCOL to Lisp matrix
  suitable for use with LAPACK"
  (let* ((mat (make-array (* nrow ncol)
			  :element-type 'double-float))
	 (mat-2d (make-array (list ncol nrow)
			     :element-type 'double-float
			     :displaced-to mat))
	 (r 0))
    (dolist (row (cdr a))
      (let ((c 0))
	(dolist (col (cdr row))
	  ;; Fortran matrices are in column-major order!
	  (setf (aref mat-2d c r) (coerce col 'double-float))
	  (incf c)))
      (incf r))
    mat))

(defun lapack-maxify-matrix (nrow ncol a)
  "Convert an LAPACK matrix of dimensions NROW and NCOL into a Maxima
matrix (list of lists)"
  (let ((2d (make-array (list ncol nrow) :element-type 'double-float
			:displaced-to a)))
    (let (res)
      (dotimes (r nrow)
	(let (row)
	  (dotimes (c ncol)
	    ;; Fortran arrays are column-major order!
	    (push (aref 2d c r) row))
	  (push `((mlist) ,@(nreverse row)) res)))
      `(($matrix) ,@(nreverse res)))))

(defun $dgeev (a &optional right-vec-p left-vec-p)
  "
DGEEV computes for an N-by-N real nonsymmetric matrix A, the
eigenvalues and, optionally, the left and/or right eigenvectors.

The right eigenvector v(j) of A satisfies
                 A * v(j) = lambda(j) * v(j)
where lambda(j) is its eigenvalue.
The left eigenvector u(j) of A satisfies
              u(j)**H * A = lambda(j) * u(j)**H
where u(j)**H denotes the conjugate transpose of u(j).

The computed eigenvectors are normalized to have Euclidean norm
equal to 1 and largest component real.

A list of three items is returned.  The first item is a list of the
eigenvectors.  The second item is false or the matrix of right
eigenvectors.  The last itme is false or the matrix of left
eigenvectors."
  (flet ((make-eigval (wr wi)
	   `((mlist) ,@(map 'list #'(lambda (r i)
				      (add r (mul '$%i i)))
			    wr wi)))
	 (make-eigvec (n vr wi)
	   ;; dgeev stores the eigen vectors in a special way.  Undo
	   ;; that.  For simplicity, we create a 2-D matrix and store
	   ;; the eigenvectors there.  Then we convert that matrix
	   ;; into a form that maxima wants.  Somewhat inefficient.
	   (let ((evec (make-array (list n n))))
	     (do ((col 0 (incf col))
		  (posn 0))
		 ((>= col n))
	       (cond ((zerop (aref wi col))
		      (dotimes (row n)
			(setf (aref evec row col) (aref vr posn))
			(incf posn)))
		     (t
		      (dotimes (row n)
			(let* ((next-posn (+ posn n))
			       (val+ (add (aref vr posn)
					  (mul '$%i (aref vr next-posn))))
			       (val- (sub (aref vr posn)
					  (mul '$%i (aref vr next-posn)))))
			  (setf (aref evec row col) val+)
			  (setf (aref evec row (1+ col)) val-)
			  (incf posn)))
		      ;; Skip over the next column, which we've already used
		      (incf col)
		      (incf posn n))))
	     ;; Now convert this 2-D Lisp matrix into a maxima matrix
	     (let (res)
	       (dotimes (r n)
		 (let (row)
		   (dotimes (c n)
		     (push (aref evec r c) row))
		   (push `((mlist) ,@(nreverse row)) res)))
	       `(($matrix) ,@(nreverse res)))
	     )))
    
    (let* ((n (maxima-matrix-dims a))
	   (a-mat (lapack-lispify-matrix a n n))
	   (wr (make-array n :element-type 'double-float))
	   (wi (make-array n :element-type 'double-float))
	   (vl (make-array (if left-vec-p (* n n) 0)
			   :element-type 'double-float))
	   (vr (make-array (if right-vec-p (* n n) 0)
			   :element-type 'double-float)))
      ;; XXX: FIXME: We need to do more error checking in the calls to
      ;; dgeev!
      (multiple-value-bind (z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
				    z-ldvl z-vr z-ldvr z-work z-lwork info)
	  ;; Figure out how much space we need in the work array.
	  (lapack:dgeev (if left-vec-p "V" "N")
			(if right-vec-p "V" "N")
			n a-mat n wr wi vl n vr n wr -1 0)
	(declare (ignore z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
			 z-ldvl z-vr z-ldvr z-work z-lwork info))
	(let* ((opt-lwork (truncate (aref wr 0)))
	       (work (make-array opt-lwork :element-type 'double-float)))
	  ;; Now do the work with the optimum size of the work space.
	  (multiple-value-bind (z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
					z-ldvl z-vr z-ldvr z-work z-lwork info)
	      (lapack:dgeev (if left-vec-p "V" "N")
			    (if right-vec-p "V" "N")
			    n a-mat n wr wi vl n vr n work opt-lwork 0)
	    (declare (ignore z-jobvl z-jobvr z-n z-a z-lda z-wr z-wi z-vl
			     z-ldvl z-vr z-ldvr z-work z-lwork))
	    ;; Convert wr+%i*wi to maxima form
	    #+nil
	    (progn
	      (format t "info = ~A~%" info)
	      (format t "lwork = ~A~%" (aref work 0))
	      (format t "vr = ~A~%" vr))
	    (let ((e-val (make-eigval wr wi))
		  (e-vec-right (if right-vec-p
					 (list (make-eigvec n vr wi))
					 nil))
		  (e-vec-left (if left-vec-p
					(list (make-eigvec n vl wi))
					nil)))
	      `((mlist) ,e-val ,@e-vec-right ,@e-vec-left))))))))

(defun $dgesvd (a jobu jobvt)
  "
DGESVD computes the singular value decomposition (SVD) of a real
M-by-N matrix A, optionally computing the left and/or right singular
vectors. The SVD is written

     A = U * SIGMA * transpose(V)

where SIGMA is an M-by-N matrix which is zero except for its
min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
are the singular values of A; they are real and non-negative, and
are returned in descending order.  The first min(m,n) columns of
U and V are the left and right singular vectors of A.

Note that the routine returns V**T, not V.

A list of three items is returned.  The first is a list containing the
non-zero elements of SIGMA.  The second is the matrix U.  The third is
V**T."
  
  (flet ((maxify-vector (v)
	   `((mlist) ,@(coerce v 'list)))
	 (fixup-jobu (arg)
	   (if arg "All columns of U" "No columns of U"))
	 (fixup-jobvt (arg)
	   (if arg "All columns of V^T" "No columns of V^T")))
    (multiple-value-bind (nrow ncol)
	(maxima-matrix-dims a)  
      (let* ((a-mat (lapack-lispify-matrix a nrow ncol))
	     (s (make-array (min nrow ncol) :element-type 'double-float))
	     (u (make-array (* nrow nrow) :element-type 'double-float))
	     (u1 (make-array (list nrow nrow) :element-type 'double-float
			     :displaced-to u))
	     (vt (make-array (* ncol ncol)
			     :element-type 'double-float))
	     (vt1 (make-array (list ncol ncol) :element-type 'double-float
			      :displaced-to vt))
	     (wr (make-array 1 :element-type 'double-float)))
	;; XXX: FIXME: We need to do more error checking in the calls to
	;; dgesvd!
	(multiple-value-bind (z-jobu z-jobvt z-m z-n z-a z-lda z-s z-u z-ldu
				     z-vt z-ldvt z-work z-lwork info)
	    ;; Figure out the optimum size for the work array
	    (lapack::dgesvd (fixup-jobu jobu)
			    (fixup-jobvt jobvt)
			    nrow ncol a-mat nrow
			    s u nrow
			    vt ncol
			    wr -1
			    0)
	  (declare (ignore z-jobu z-jobvt z-m z-n z-a z-lda z-s z-u z-ldu
			   z-vt z-ldvt z-work z-lwork info))
	  (let* ((opt-lwork (truncate (aref wr 0)))
		 (work (make-array opt-lwork :element-type 'double-float)))
	    ;; Allocate the optimum work array and do the requested
	    ;; computation.
	    (multiple-value-bind (z-jobu z-jobvt z-m z-n z-a z-lda z-s z-u
					 z-ldu z-vt z-ldvt z-work z-lwork info)
		(lapack::dgesvd (fixup-jobu jobu)
				(fixup-jobvt jobvt)
				nrow ncol a-mat nrow
				s u nrow
				vt ncol
				work opt-lwork
				0)
	      (declare (ignore z-jobu z-jobvt z-m z-n z-a z-lda z-s z-u z-ldu
			       z-vt z-ldvt z-work z-lwork info))
	      (let ((u-max (lapack-maxify-matrix nrow nrow u1))
		    (vt-max (lapack-maxify-matrix ncol ncol vt1))
		    (s-max (maxify-vector s)))
		`((mlist) ,s-max ,u-max ,vt-max)))))))))
