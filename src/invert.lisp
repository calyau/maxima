;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)

(defmfun $adjoint (mat)
  (let* ((n ($length mat))
	 (adj (simplify ($ident n))))
    (unless (like n 1)
      (do ((i 1 (1+ i)))
	  ((> i n))
	(do ((j 1 (1+ j)))
	    ((> j n))
	  (maset (mul* (power -1 (+ i j))
		       (simplify ($determinant (simplify ($minor mat j i)))))
		 adj i j))))
    adj))

(defmfun $invert_by_adjoint (mat)
  (let*
    ((adj (simplify ($adjoint mat)))
     (det (let (($scalarmatrixp t))
               (ncmul2 (simplify ($row mat 1))
                       (simplify ($col adj 1)))))
     (mat1 (if (and $scalarmatrixp (= ($length mat) 1)) (maref adj 1 1) adj)))
    (if $detout
      `((mtimes) ((mexpt) ,det -1) ,mat1)
      (div mat1 det))))
  
(defmvar $invert_method nil)
(defmvar $invert_by_adjoint_size_limit 8)

(defmfun $invert (&rest args)
  (case $invert_method
    (($lu) (apply #'invert-via-$invert_by_lu args))
    (($gausselim) (apply #'$invert_by_gausselim args))
    (($adjoint) (apply #'$invert_by_adjoint args))
    ((nil)
      ;; Select a method appropriate for the matrix.
      ;; This could be more sophisticated.
      (let*
        ((my-matrix (first args))
         (size (length (rest my-matrix))))
        (if (<= size $invert_by_adjoint_size_limit)
          (apply #'$invert_by_adjoint args)
          (apply #'$invert_by_gausselim args))))
    (t
      (mtell "invert: unrecognized invert_method=~M; assume default.~%" $invert_method)
      (let (($invert_method nil))
        (apply #'$invert args)))))

(defun invert-via-$invert_by_lu (m &optional (field-name (if $ratmx '$crering '$generalring)))
  ;; Call functions from package linearalgebra via MFUNCALL to autoload them if necessary.
  (if $detout
    (let*
      ((field (mfuncall '$require_ring field-name "$second" "$invert"))
       (d-i (funcall 'invert-by-lu-with-determinant m field-name))
       (d (first d-i))
       (i (second d-i))
       (d-times-i (multiply-matrix-elements d (funcall 'mring-mult field) i))
       (d^-1 (funcall (funcall 'mring-reciprocal field) d)))
      (list '(mtimes) d^-1 d-times-i))
    (mfuncall '$invert_by_lu m field-name)))

;; I wonder if this function already exists somewhere. Oh well.
(defun multiply-matrix-elements (a multiply m)
  (cons (car m) (mapcar #'(lambda (row) (cons (car row) (mapcar #'(lambda (x) (funcall multiply a x)) (cdr row)))) (cdr m))))
