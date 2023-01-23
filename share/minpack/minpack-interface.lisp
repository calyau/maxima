;;; -*- Mode: lisp -*-

;;; Simple Maxima interface to minpack routines

(in-package #:maxima)

(defmvar $debug_minpack nil
  "Set to true to enable debugging messages from minpack routines")

(defmfun $minpack_lsquares (fcns vars init-x
				 &key
				 (jacobian t)
				 (tolerance #.(sqrt double-float-epsilon)))
  "Minimize the sum of the squares of m functions in n unknowns (n <= m)

   VARS    list of the variables
   INIT-X  initial guess
   FCNS    list of the m functions

   Optional keyword args (key = val)
   TOLERANCE  tolerance in solution 
   JACOBIAN   If true, maxima computes the Jacobian directly from FCNS.
              If false, the Jacobian is internally computed using a
              forward-difference approximation.
              Otherwise, it is a function returning the Jacobian"
  (unless (and (listp fcns) (eq (caar fcns) 'mlist))
    (merror "~M: ~M is not a list of functions" %%pretty-fname fcns))
  (unless (and (listp vars) (eq (caar vars) 'mlist))
    (merror "~M: ~M is not a list of variables" %%pretty-fname vars))
  (unless (and (listp init-x) (eq (caar init-x) 'mlist))
    (merror "~M: ~M is not a list of initial values" %%pretty-fname init-x))
  (setf tolerance ($float tolerance))
  (unless (and (realp tolerance) (plusp tolerance))
    (merror "~M: tolerance must be a non-negative real number, not: ~M"
	    %%pretty-fname tolerance))
  
  (let* ((n (length (cdr vars)))
	 (m (length (cdr fcns)))
	 (x (make-array n :element-type 'double-float
			:initial-contents (mapcar #'(lambda (z)
						      ($float z))
						  (cdr init-x))))
	 (fvec (make-array m :element-type 'double-float))
	 (fjac (make-array (* m n) :element-type 'double-float))
	 (ldfjac m)
	 (info 0)
	 (ipvt (make-array n :element-type 'f2cl-lib:integer4))
	 (fv (coerce-float-fun fcns vars))
	 (fj (cond ((eq jacobian t)
		    ;; T means compute it ourselves
		    (meval `(($jacobian) ,fcns ,vars)))
		   (jacobian
		    ;; Use the specified Jacobian
		    )
		   (t
		    ;; No jacobian at all
		    nil))))
    ;; Massage the Jacobian into a function
    (when jacobian
      (setf fj (coerce-float-fun fj vars)))
    (cond
      (jacobian
       ;; Jacobian given (or computed by maxima), so use lmder1
       (let* ((lwa (+ m (* 5 n)))
	      (wa (make-array lwa :element-type 'double-float)))
	 (labels ((fcn-and-jacobian (m n x fvec fjac ldfjac iflag)
		    (declare (type f2cl-lib:integer4 m n ldfjac iflag)
			     (type (cl:array double-float (*)) x fvec fjac))
		    (ecase iflag
		      (1
		       ;; Compute function at point x, placing result
		       ;; in fvec (subseq needed because sometimes
		       ;; we're called with vector that is longer than
		       ;; we want.  Perfectly valid Fortran, though.)
		       (let ((val (apply 'funcall fv (subseq (coerce x 'list) 0 n))))
			 (unless (consp val)
			   (merror "Unable to evaluate function at the point ~M"
				   (list* '(mlist) (subseq (coerce x 'list) 0 n))))
			 (when $debug_minpack
			   (format t "f(~{~A~^, ~}) =~%[~@<~{~A~^, ~:_~}~:>]~%"
				   (coerce x 'list)
				   (cdr val)))
			 (replace fvec (mapcar #'(lambda (z)
						   (cl:float z 1d0))
					       (cdr val)))))
		      (2
		       ;; Compute Jacobian at point x, placing result in fjac
		       (let ((j (apply 'funcall fj (subseq (coerce x 'list) 0 n))))
			 (unless (consp j)
			   (merror "Unable to evaluate Jacobian at the point ~M"
				   (list* '(mlist) (subseq (coerce x 'list) 0 n))))
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
		    (values m n nil nil nil ldfjac iflag)))
	   (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 ldfjac var-6 info)
	       (minpack:lmder1 #'fcn-and-jacobian
			       m
			       n
			       x
			       fvec
			       fjac
			       ldfjac
			       tolerance
			       info
			       ipvt
			       wa
			       lwa)
	     (declare (ignore ldfjac var-0 var-1 var-2 var-3 var-4 var-5 var-6))

	     ;; Return a list of the solution and the info flag
	     (list '(mlist)
		   (list* '(mlist) (coerce x 'list))
		   (minpack:enorm m fvec)
		   info)))))
      (t
       ;; No Jacobian given so we need to use differences to compute
       ;; a numerical Jacobian.  Use lmdif1.
       (let* ((lwa (+ m (* 5 n) (* m n)))
	      (wa (make-array lwa :element-type 'double-float)))
	 (labels ((fval (m n x fvec iflag)
		    (declare (type f2cl-lib:integer4 m n ldfjac iflag)
			     (type (cl:array double-float (*)) x fvec fjac))
		    ;; Compute function at point x, placing result in fvec
		    (let ((val (apply 'funcall fv (subseq (coerce x 'list) 0 n))))
		      (unless (consp val)
			(merror "Unable to evaluate function at the point ~M"
				(list* '(mlist) (subseq (coerce x 'list) 0 n))))
		      (when $debug_minpack
			(format t "f(~{~A~^, ~}) =~%[~@<~{~A~^, ~:_~}~:>]~%"
				(coerce x 'list)
				(cdr val)))
		      (replace fvec (mapcar #'(lambda (z)
						(cl:float z 1d0))
					    (cdr val))))
		    (values m n nil nil iflag)))
	   (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 info)
	       (minpack:lmdif1 #'fval
			       m
			       n
			       x
			       fvec
			       tolerance
			       info
			       ipvt
			       wa
			       lwa)
	     (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5))

	     ;; Return a list of the solution and the info flag
	     (list '(mlist)
		   (list* '(mlist) (coerce x 'list))
		   (minpack:enorm m fvec)
		   info))))))))

(defmfun $minpack_solve (fcns vars init-x
			      &key
			      (jacobian t)
			      (tolerance #.(sqrt double-float-epsilon)))
  "Solve the system of n equations in n unknowns

   VARS    list of the n variables
   INIT-X  initial guess
   FCNS    list of the n functions

   Optional keyword args (key = val)
   TOLERANCE  tolerance in solution 
   JACOBIAN   If true, maxima computes the Jacobian directly from FCNS.
              If false, the Jacobian is internally computed using a
              forward-difference approximation.
              Otherwise, it is a function returning the Jacobian"
  (unless (and (listp fcns) (eq (caar fcns) 'mlist))
    (merror "~M: ~M is not a list of functions"
	    %%pretty-fname fcns))
  (unless (and (listp vars) (eq (caar vars) 'mlist))
    (merror "~M:  ~M is not a list of variables"
	    %%pretty-fname vars))
  (unless (and (listp init-x) (eq (caar init-x) 'mlist))
    (merror "~M: ~M is not a list of initial values"
	    %%pretty-fname init-x))
  (unless (and (realp tolerance) (plusp tolerance))
    (merror "~M: tolerance must be a non-negative real number, not: ~M"
	    %%pretty-fname tolerance))

  (let* ((n (length (cdr vars)))
	 (x (make-array n :element-type 'double-float
			:initial-contents (mapcar #'(lambda (z)
						      ($float z))
						  (cdr init-x))))
	 (fvec (make-array n :element-type 'double-float))
	 (fjac (make-array (* n n) :element-type 'double-float))
	 (ldfjac n)
	 (info 0)
	 (fv (coerce-float-fun fcns vars))
	 (fj (cond ((eq jacobian t)
		    ;; T means compute it ourselves
		    (mfuncall '$jacobian fcns vars))
		   (jacobian
		    ;; Use the specified Jacobian
		    )
		   (t
		    ;; No jacobian at all
		    nil))))
    ;; Massage the Jacobian into a function
    (when jacobian
      (setf fj (coerce-float-fun fj vars)))
    (cond
      (jacobian
       ;; Jacobian given (or computed by maxima), so use lmder1
       (let* ((lwa (/ (* n (+ n 13)) 2))
	      (wa (make-array lwa :element-type 'double-float)))
	 (labels ((fcn-and-jacobian (n x fvec fjac ldfjac iflag)
		    (declare (type f2cl-lib:integer4 n ldfjac iflag)
			     (type (cl:array double-float (*)) x fvec fjac))
		    (ecase iflag
		      (1
		       ;; Compute function at point x, placing result
		       ;; in fvec (subseq needed because sometimes
		       ;; we're called with vector that is longer than
		       ;; we want.  Perfectly valid Fortran, though.)
		       (let ((val (apply 'funcall fv (subseq (coerce x 'list) 0 n))))
			 (replace fvec (mapcar #'(lambda (z)
						   (cl:float z 1d0))
					       (cdr val)))))
		      (2
		       ;; Compute Jacobian at point x, placing result in fjac
		       (let ((j (apply 'funcall fj (subseq (coerce x 'list) 0 n))))
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
		    (values n nil nil nil ldfjac iflag)))
	   (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 ldfjac var-6 info)
	       (minpack:hybrj1 #'fcn-and-jacobian
			       n
			       x
			       fvec
			       fjac
			       ldfjac
			       tolerance
			       info
			       wa
			       lwa)
	     (declare (ignore ldfjac var-0 var-1 var-2 var-3 var-4 var-6))
	     ;; Return a list of the solution and the info flag
	     (list '(mlist)
		   (list* '(mlist) (coerce x 'list))
		   (minpack:enorm n fvec)
		   info)))))
      (t
       ;; No Jacobian given so we need to use differences to compute
       ;; a numerical Jacobian.  Use lmdif1.
       (let* ((lwa (/ (* n (+ (* 3 n) 13)) 2))
	      (wa (make-array lwa :element-type 'double-float)))
	 (labels ((fval (n x fvec iflag)
		    (declare (type f2cl-lib:integer4 n ldfjac iflag)
			     (type (cl:array double-float (*)) x fvec fjac))
		    ;; Compute function at point x, placing result in fvec
		    (let ((val (apply 'funcall fv (subseq (coerce x 'list) 0 n))))
		      (replace fvec (mapcar #'(lambda (z)
						(cl:float z 1d0))
					    (cdr val))))
		    (values n nil nil iflag)))
	   (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 info)
	       (minpack:hybrd1 #'fval
			       n
			       x
			       fvec
			       tolerance
			       info
			       wa
			       lwa)
	     (declare (ignore var-0 var-1 var-2 var-3 var-4))

	     ;; Return a list of the solution and the info flag
	     (list '(mlist)
		   (list* '(mlist) (coerce x 'list))
		   (minpack:enorm n fvec)
		   info))))))))
