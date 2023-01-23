;;; -*- Mode: lisp -*-

;;; Simple Maxima interface to COBYLA, Constrained Optimization BY
;;; Linear Approximations.

(in-package #:maxima)

(defvar *debug-cobyla* nil
  "Non-Nil to enable some debugging prints in the coblya interface")

;; Variable that will hold the function that calculates the function
;; value and the constraints.
(defvar *calcfc*)

;; COBYLA always calls CALCFC to compute the function value and the
;; constraint equations.  But we want to be able to specify different
;; versions.  So, COBYLA calls CALCFC, which then calls *CALCFC* to
;; do the real computation.
(defun cobyla::calcfc (n m x f con)
  (declare (ignore f))
  (funcall maxima::*calcfc* n m x con))

(defmfun $fmin_cobyla
    (f vars init-x
       &key constraints (rhobeg 1d0) (rhoend 1d-6) ( iprint 0) (maxfun 1000))
"COBYLA (Constrained Optimization BY Linear Approximation).

 This is a maxima interface to the routine COBYLA.  Interface
 Copyright Raymond Toy 2010 Interface released under the terms of the
 GNU General Public License.

 The COBYLA Fortran routines are included with permission from the
 author Michael J. D. Powell, 2010/10/08.  The code was obtained from
 http://plato.asu.edu/sub/nlores.html#general.

 fmin_cobyla(f, vars, init_x, [args])

   f       Real function to be minimized
   vars    List of variables over which to minimize
   init_x  Initial guess.  Do NOT set to all zeroes.

 The optional arguments are all keyword arguments of the form key =
 value:

   constraints  List of expressions for the constraints on the variables.
                Each expression is assumed to be greater than or equal
                to zero.  Constraints must be of the form f >= g, f <=
                g, or f = g.
   rhobeg       Initial value of the internal RHO variable which controls 
                the size of simplex.  (Defaults to 1.)
   rhoend       The desired final value rho parameter.  It is approximately
                the accuracy in the variables. (Defaults to 1d-6.)
   iprint       Verbose output level.  (Defaults to 0.)
                0 - No output
                1 - Summary at the end of the calculation
		2 - Each new value of RHO and SIGMA is printed, including 
		    the vector of variables, some function information when 
		    RHO is reduced.
		3 - Like 2, but information is printed when F(X) is computed.
   maxfun       The maximum number of function evaluations.  (Defaults to
                1000).

 The parameter SIGMA is an internal penalty parameter, it being
 assumed that a change to X is an improvement if it reduces the merit
 function

            F(X)+SIGMA*MAX(0.0,-C1(X),-C2(X),...,-CM(X))

 where C1, C2 are the constraint functions.  When printed the
 parameter MAXCV is the MAX term above, which stands for MAXimum
 Constraint Violation.

 On return, a vector of four elements is given:

  1:  The value of the variables giving the minimum
  2:  The minimized function value
  3:  The number of function evaluations.
  4:  Success code:
        0 - success
        1 - maxfun limit reached
        2 - rounding issues
       -1 - maxcv exceeds rhoend probably indicating constraints not
            satisfied.

 You can find some examples in share/cobyla/ex.

 An example of minimizing x1*x2 with 1-x1^2-x2^2 >= 0:

   load(fmin_cobyla);
   fmin_cobyla(x1*x2, [x1, x2], [1,1], constraints = [1-x1^2-x2^2 >= 0], iprint=1);

 => [[x1 = 0.707107555323284, x2 = - 0.7071060070503778], 
          - 0.4999999999998015, 64, 0]

 The theoretical solution is x1 = 1/sqrt(2), x2 = -1/sqrt(2).
"
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
    (when *debug-cobyla*
      (mformat t "cons = ~M~%" normalized-constraints))

    (let* ((n (length (cdr vars)))
	   (m (length (cdr normalized-constraints)))
	   (x (make-array n
			  :element-type 'double-float
			  :initial-contents
			  (mapcar #'(lambda (z)
				      (let ((r ($float z)))
					(if (floatp r)
					    r
					    (merror "~M: Does not evaluate to a float:  ~M"
						    %%pretty-fname z))))
				  (cdr init-x))))
	   ;; Real work array for cobyla.
	   (w (make-array (+ (* n (+ (* 3 n)
				     (* 2 m)
				     11))
			     (+ (* 4 m) 6)
			     6)
			  :element-type 'double-float))
	   ;; Integer work array for cobyla.
	   (iact (make-array (+ m 1) :element-type 'f2cl-lib::integer4))
	   (fv (coerce-float-fun f vars))
	   (cv (coerce-float-fun normalized-constraints vars))
	   (*calcfc*
	    #'(lambda (nn mm xval cval)
		;; Compute the function and the constraints at
		;; the given xval.  The function value is
		;; returned is returned, and the constraint
		;; values are stored in cval.
		(declare (fixnum nn mm)
			 (type (cl:array cl:double-float (*)) xval cval))
		(let* ((x-list (coerce xval 'list))
		       (f (apply fv x-list))
		       (c (apply cv x-list)))
		  ;; Do we really need these checks?
		  (unless (floatp f)
		    (merror "~M: The objective function did not evaluate to a number at ~M"
			    %%pretty-fname (list* '(mlist) x-list)))
		  (unless (every #'floatp (cdr c))
		    (let ((bad-cons
			   (loop for cval in (cdr c)
				 for k from 1
				 unless (floatp cval)
				   collect k)))
		      ;; List the constraints that did not
		      ;; evaluate to a number to make it easier
		      ;; for the user to figure out which
		      ;; constraints were bad.
		      (mformat t "~M: At the point ~M:~%"
			       %%pretty-fname (list* '(mlist) x-list))
		      (merror
		       (with-output-to-string (msg)
			 (loop for index in bad-cons
			       do
				  (mformat msg
					   "Constraint ~M: ~M did not evaluate to a number.~%"
					   index
					   (elt normalized-constraints index)))))))
		  (replace cval c :start2 1)
		  ;; This is the f2cl calling convention for
		  ;; CALCFC.  For some reason, f2cl thinks n
		  ;; and m are modified, so they must be
		  ;; returned.
		  (values nn mm nil f nil)))))
      (multiple-value-bind (null-0 null-1 null-2 null-3 null-4 null-5
			    neval null-6 null-7 ierr)
	  (cobyla:cobyla n m x rhobeg rhoend iprint maxfun w iact 0)
	(declare (ignore null-0 null-1 null-2 null-3 null-4 null-5 null-6 null-7))
	;; Should we put a check here if the number of function
	;; evaluations equals maxfun?  When iprint is not 0, the output
	;; from COBYLA makes it clear that something bad happened.
	(let* ((x-list (coerce x 'list))
	       (soln (list* '(mlist)
			    (mapcar #'(lambda (var val)
					`((mequal) ,var ,val))
				    (cdr vars)
				    (coerce x 'list))))
	       ;; The maxcv value.  See cobyla.f MPP value. MPP = M +
	       ;; 2, adjusted for 0-indexing lisp arrays.
	       (max-cv (aref w (+ m 1))))
	  ;; Return a list off the following items:
	  ;;  1:  The point that gives the optimum in the form var=val
	  ;;  2:  The corresponding value of the function
	  ;;  3:  Number of function evaluations
	  ;;  4:  Return code
	  ;;        0  - no errors
	  ;;        1  - maxfun limit reached
	  ;;        2  - rounding issues
	  ;;        -1 - maxcv exceeds rhoend probably indicating
	  ;;             constraints not satisfied.
	  (make-mlist
	   soln
	   (apply fv x-list)
	   neval
	   (if (> max-cv rhoend) -1 ierr)))))))
