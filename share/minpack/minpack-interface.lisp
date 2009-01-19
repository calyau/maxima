(in-package #-gcl #:maxima #+gcl "MAXIMA")

;; Simple Maxima interface to minpack routines

;; m equations in n unknowns.  n <= m
(defun $minpack_lmder1 (m init-x fcn jacobian tol)
  (let* ((n (length (cdr init-x)))
	 (x (make-array n :element-type 'double-float
			:initial-contents (mapcar #'(lambda (z)
						      (cl:float z 1d0))
						  (cdr init-x))))
	 (fvec (make-array m :element-type 'double-float))
	 (fjac (make-array (* m n) :element-type 'double-float))
	 (ldfjac m)
	 (info 0)
	 (lwa (+ m (* 5 n)))
	 (wa (make-array lwa :element-type 'double-float))
	 (ipvt (make-array n :element-type 'f2cl-lib:integer4)))
    (labels ((fcn-and-jacobian (m n x fvec fjac ldfjac iflag)
	       (declare (type f2cl-lib:integer4 m n ldfjac iflag)
			(type (cl:array double-float (*)) x fvec fjac)
			(ignore m n))
	       (ecase iflag
		 (1
		  ;; Compute function at point x, placing result in fvec
		  (let ((val (mfuncall fcn
				       (cons '(mlist)
					     (coerce x 'list)))))
		    (replace fvec (mapcar #'(lambda (z)
					      (cl:float z 1d0))
					  (cdr val)))))
		 (2
		  ;; Compute Jacobian at point x, placing result in fjac
		  (let ((j (mfuncall jacobian
				     (cons '(mlist)
					   (coerce x 'list)))))
		    ;; Extract out elements of Jacobian and place into
		    ;; fjac, in column-major order.
		    (let ((row-index 0))
		      (dolist (row (cdr j))
			(let ((col-index 0))
			  (dolist (col (cdr row))
			    (setf (aref fjac (+ row-index (* ldfjac col-index)))
				  (cl:float col 1d0))
			    (incf col-index)))
			(incf row-index))))))
	       (values m n nil nil nil ldfjac nil)))
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 ldfjac var-6 info)
	  (minpack:lmder1 #'fcn-and-jacobian
			  m
			  n
			  x
			  fvec
			  fjac
			  ldfjac
			  tol
			  info
			  ipvt
			  wa
			  lwa)
	(declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6))

	;; Return a list of the solution and the info flag
	(list '(mlist)
	      (list* '(mlist) (coerce x 'list))
	      (minpack:enorm m fvec)
	      info)))))
