;;; -*- Mode: lisp -*-

;;; Simple Maxima interface to COBYLA, Constrained Optimization BY
;;; Linear Approximations.

(in-package #:maxima)

;; Variable that will hold the function that calculates the function
;; value and the constraints.
(defvar *calcfc*)

(in-package #:bigfloat)

;; COBYLA always calls CALCFC to compute the function value and the
;; constraint equations.  But we want to be able to specify different
;; versions.  So, COBYLA calls CALCFC, which then calls *CALCFC* to
;; do the real computation.
(cl:defun calcfc (n m x f con)
  (cl:declare (cl:ignore f))
  (cl:funcall maxima::*calcfc* n m x con))

(cl:in-package #:maxima)

;; The actual interface to COBYLA.  Take the inputs from maxima,
;; massage them into a suitable Lisp form, and call COBYLA to find the
;; answer.
(defmfun $bf_fmin_cobyla
    (f vars init-x
       &key constraints
       (rhobeg 1d0)
       (rhoend nil rhoendp)
       (iprint 0)
       (maxfun 1000))
  (unless (listp vars)
    (merror "~M: vars must be a list of variables. Got: ~M"
	    %%pretty-fname vars))
  (unless (listp init-x)
    (merror "~M: Initial values must be a list of values. Got: ~M"
	    %%pretty-fname init-x))

  (unless (= (length (cdr vars))
	     (length (cdr init-x)))
    (merror
     "~M: Number of initial values (~M) does not match the number of variables ~M~%"
     %%pretty-fname
     (length (cdr init-x))
     (length (cdr vars))))

  (unless (and (integerp iprint)
	       (<= 0 iprint 3))
    (merror
     "~M: iprint must be an integer between 0 and 3, inclusive, not: ~M~%"
     %%pretty-fname iprint))

  (unless (and (integerp maxfun) (plusp maxfun))
    (merror
     "~M: maxfun must be a positive integer, not: ~M~%"
     %%pretty-fname maxfun))

  (unless rhoendp
    (setf rhoend (bigfloat:sqrt (bigfloat:expt (bigfloat:bigfloat 10) (- $fpprec)))))
  
  ;; Go through constraints and convert f >= g to f - g, f <= g to g -
  ;; f, and f = g to f - g and g - f.  This is because cobyla expects
  ;; all constraints to of the form h>=0.
  (let (normalized-constraints)
    (dolist (c (cdr constraints))
      (let ((op ($op c)))
	(cond ((string-equal op ">=")
	       (push (sub ($lhs c) ($rhs c)) normalized-constraints))
	      ((string-equal op "<=")
	       (push (sub ($rhs c) ($lhs c)) normalized-constraints))
	      ((string-equal op "=")
	       (push (sub ($lhs c) ($rhs c)) normalized-constraints)
	       (push (sub ($rhs c) ($lhs c)) normalized-constraints))
	      (t
	       (merror "~M: Constraint equation must be =, <= or >=: got ~M"
		       %%pretty-fname op)))))

    (setf normalized-constraints
	  (list* '(mlist)
		 (nreverse normalized-constraints)))
    #+nil
    (mformat t "cons = ~M~%" normalized-constraints)

    (let* ((n (length (cdr vars)))
	   (m (length (cdr normalized-constraints)))
	   (x (make-array n
			  :initial-contents (mapcar #'(lambda (z)
							(let ((r (bigfloat:to ($bfloat z))))
							  r))
						    (cdr init-x))))
	   ;; Real work array for cobyla.
	   (w (make-array (+ (* n (+ (* 3 n)
				     (* 2 m)
				     11))
			     (+ (* 4 m) 6)
			     6)))
	   ;; Integer work array for cobyla.
	   (iact (make-array (+ m 1) :element-type 'f2cl-lib::integer4))
	   (fv (coerce-bfloat-fun f vars))
	   (cv (coerce-bfloat-fun normalized-constraints vars))
	   (*calcfc* #'(lambda (nn mm xval cval)
			 ;; Compute the function and the constraints at
			 ;; the given xval.  The function value is
			 ;; returned is returned, and the constraint
			 ;; values are stored in cval.
			 (declare (fixnum nn mm)
				  (type (cl:array t (*)) xval cval))
			 (let* ((x-list (map 'list #'to xval))
				(f (bigfloat:maybe-to (apply fv x-list)))
				(c (apply cv x-list)))
			   ;; Do we really need these checks?
			   #+nil
			   (progn
			     (format t "xval = ~S~%" xval)
			     (format t "f = ~A~%" f)
			     (format t "c = ~A~%" c))
			   (setf c (mapcar #'bigfloat:maybe-to (cdr c)))
			   ;;(format t "c = ~A~%" c)
			   (unless (or (floatp f) ($bfloatp f) (typep f 'bigfloat:bigfloat))
			     (merror "At the point ~M:~%The objective function did not evaluate to a number: ~M"
				     (list* '(mlist) x-list)
				     f))
			   (let ((bad-cons (loop for cval in c
						 for k from 1
						 unless (or (floatp cval)
							    ($bfloatp cval)
							    (typep cval 'bigfloat:bigfloat))
						   collect k)))
			     (when bad-cons
			       ;; List the constraints that did not
			       ;; evaluate to a number to make it easier
			       ;; for the user to figure out which
			       ;; constraints were bad.
			       (mformat t "At the point ~M:~%" (list* '(mlist) x-list))
			       (merror
				(with-output-to-string (msg)
				  (loop for index in bad-cons
					do
					   (mformat msg "Constraint ~M did not evaluate to a real: ~M~%"
						    index
						    (elt normalized-constraints index)))))))
			   (replace cval c)
			   ;; This is the f2cl calling convention for
			   ;; CALCFC.  For some reason, f2cl thinks n
			   ;; and m are modified, so they must be
			   ;; returned.
			   (values nn mm nil
				   f nil)))))
      (multiple-value-bind (null-0 null-1 null-2 null-3 null-4 null-5 neval null-6 null-7 ierr)
	  (bigfloat::cobyla n m x (bigfloat:to rhobeg) (bigfloat:to rhoend) iprint maxfun w iact 0)
	(declare (ignore null-0 null-1 null-2 null-3 null-4 null-5 null-6 null-7))
	;; Should we put a check here if the number of function
	;; evaluations equals maxfun?  When iprint is not 0, the output
	;; from COBYLA makes it clear that something bad happened.
	(let ((x-list (map 'list #'to x)))
	  ;; Return the optimum function value, the point that gives the
	  ;; optimum, the value of the constraints, and the number of
	  ;; function evaluations.  For convenience.  Only the point and
	  ;; the number of evaluations is really needed.
	  (make-mlist
	   (list* '(mlist)
		  (mapcar #'(lambda (var val)
			      `((mequal) ,var ,val))
			  (cdr vars)
			  (coerce x 'list)))
	   (apply fv x-list)
	   neval
	   ierr))))))
