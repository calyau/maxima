;;; -*- Mode: lisp -*-

;; Simple Maxima inteface to hompack routines

(in-package #-gcl #:maxima #+gcl "MAXIMA")

(defmvar $debug_hompack nil
  "Set to non-NIL to enable some debugging prints")

(defun parse-equations (eqnlist varlist fname)
  (let ((eqns (cdr eqnlist))
	(vars (cdr varlist))
	(total-deg 1)
	numt
	final-kdeg
	final-coef)
    ;; TODO: Check that varlist is a list of symbols.
    
    ;; Process each equation
    (dolist (eqn eqns)
      (when $debug_hompack
	(displa eqn))

      (unless ($polynomialp eqn varlist)
	(merror "~M: Is not a polynomial in ~M: ~M"
		fname varlist eqn))

      (let ((args (if (string-equal ($op eqn) "+")
		      (cdr ($args eqn))
		      (list eqn)))
	    eqn-deg kdeg coef)

	(push (length args) numt)
	;; TODO: Verify that eqn is a sum of terms.

	;; Process each term of the equation
	(dolist (term args)
	  (when $debug_hompack
	    (displa term))
	  (let* ((prod 1)
		 (term-deg 0)
		 (deg (mapcar #'(lambda (v)
				  ;; TODO: verify that V is a product
				  
				  ;; For the term, determine the power
				  ;; of each variable and also create
				  ;; a product of the variable raised
				  ;; to the corresponding power.
				  (let ((p ($hipow term v)))
				    (incf term-deg p)
				    ;; TODO: Check that p is non-negative integer
				    (setf prod (mul prod (pow v p)))
				    p))
			      vars))
		 (c ($float ($expand (div term prod)))))
	    ;; Check that c is a real number since it's supposed
	    ;; to be the numerical coefficient of the polynomial term.
	    (unless (floatp c)
	      (merror "~M: coefficient of ~M is not a number: ~M"
		      fname term c))
	    (when $debug_hompack
	      (format t "deg ~A~%" deg)
	      (format t "c ~A~%" c)
	      (format t "term deg ~A~%" term-deg))
	    (push term-deg eqn-deg)
	    ;; Accumulate these results for each term on a list.
	    (push c coef)
	    (push (list* '(mlist) deg) kdeg)))
	(setf total-deg (* total-deg (reduce #'max eqn-deg)))
	;; Now accumlate the results from each equation on to the final list
	(push (list* '(mlist) (nreverse kdeg)) final-kdeg)
	(push (list* '(mlist) (nreverse coef)) final-coef)))
    ;; Finally, return the full list of coefficients and degress for
    ;; each term of each equation.
    (setf final-kdeg (nreverse final-kdeg))
    (setf final-coef (nreverse final-coef))
    (setf numt (nreverse numt))
    (values total-deg
	    final-kdeg
	    final-coef
	    numt)))

(defun convert-coef (coef)
  (let* ((dim-j (length coef))
	 ;;(dim-k (reduce #'max (mapcar #'$length coef)))
	 (dim-k (reduce #'max coef :key #'$length))
	 (array (make-array (list dim-j dim-k) :element-type 'double-float))
	 (f2cl-array (make-array (* dim-j dim-k) :element-type 'double-float)))
    (when $debug_hompack
      (format t "dim ~A ~A~%" dim-j dim-k)
      (format t "array ~A~%" array))
    ;; F2CL maps multi-dimensional arrays into one-dimensional arrays
    ;; in column-major order.  That is, the order of indices is
    ;; reversed.
    (loop for eqn in coef for j from 1 do
      (loop for term in (cdr eqn) for k from 1 do
	(when $debug_hompack
	  (format t "eqn ~A coef ~A = ~A~%" j k term))
	(setf (aref array (1- j) (1- k)) ($float term))
	(setf (f2cl-lib::fref f2cl-array (j k) ((1 dim-j) (1 dim-k)))
	      ($float term))))
    (when $debug_hompack
      (format t "coef array ~A~%" array))
    f2cl-array))

(defun convert-kdeg (kdeg numt)
  (let* ((dim-j (length kdeg))
	 (dim-l (1+ dim-j))
	 (dim-k (reduce #'max numt))
	 (array (make-array (list dim-j dim-l dim-k) :element-type 'f2cl-lib:integer4))
	 (f2cl-array (make-array (* dim-j dim-l dim-k) :element-type 'f2cl-lib:integer4)))
    (loop for eqn in kdeg for j from 1 do
      (loop for term in (cdr eqn) for l from 1 do
	(loop for p in (cdr term) for k from 1 do
	  (when $debug_hompack
	    (format t "set eqn ~A var ~A term ~A = ~A~%"
		    j k l p))
	  (setf (aref array (1- j) (1- k) (1- l)) p)
	  (setf (f2cl-lib::fref f2cl-array (j k l) ((1 dim-j) (1 dim-l) (1 dim-k))) p))))
    (when $debug_hompack
      (format t "kdeg array ~A~%" array))
    f2cl-array))

(defmfun $hompack_polsys (eqnlist varlist &key (iflg1 0) (epsbig 1d-4) (epssml 1d-14) (numrr 10) iflg2 sspar)
  "Solve the system of n polynomial equations in n unknowns.

  EQNLIST   list of polynomial equations.  Must be in the form sum(c[k]*x1^p1*x2^p2*...*xn^pn)
  VARLIST   list of the n variables.

  Optional keywords args (key = val)
  IFLG1     One of 0, 1, 10, 11.  See docs for more info.
  EPSBIG    Local error tolerance allowed the path tracker
  EPSSML    Accuracy desired for final solution.
  NUMRR     Number of multiples of 1000 steps before abandoning path.

  IFLG2     List indicating which paths to search. -2 means track this
            path.  Length must be the total degree of the equations.
  SSPAR     List of parameters used for optimal step size estimation.  If
            SSPAR[j] < 0, use default value.  See Fortran polynf and
            stepnf for more info."

  (unless (= ($length eqnlist) ($length varlist))
    (merror "~M: Number of equations (~M) is not the number of variables (~M)"
	    %%pretty-fname (length eqnlist) (length varlist)))
  (unless (and (listp varlist) (eq (caar varlist) 'mlist))
    (merror "~M:  ~M is not a list of variables"
	    %%pretty-fname varlist))
  (unless (member iflg1 '(0 1 10 11))
    (merror "~M: iflg1 must be 0, 1, 10, or 11.  Got ~M"
	    %%pretty-fname iflg1))

  (when sspar
    (unless (and sspar (listp sspar) (eq (caar sspar) 'mlist))
      (merror "~M: sspar must be a list not ~M"
	      %%pretty-fname sspar))
    (unless (= ($length sspar) 8)
      (merror "~M: sspar must have length 8 not ~M"
	      %%pretty-fname ($length sspar)))
    (unless (every #'(lambda (x)
		       (typep ($float x) 'double-float))
		   (cdr sspar))))

  (multiple-value-bind (total-deg kdeg coef numt)
      (parse-equations eqnlist varlist %%pretty-fname)
    (when iflg2
      (unless (= (length iflg2) total-deg)
	(merror "~M: Length of iflg2 must be ~M not ~M"
		%%pretty-fname total-deg (length iflg2)))
      (unless (every #'integerp (cdr iflg2))
	(merror "~M: iflg2 must be integers: ~M"
		%%pretty-fname iflg2)))
    (let* ((n ($length eqnlist))
	   (mmaxt (reduce #'max numt))
	   (lenwk (+ 21 (* 61 n) (* 10 n n) (* 7 n mmaxt) (* 4 n n mmaxt)))
	   (wk (make-array lenwk :element-type 'double-float))
	   (leniwk (+ 43 (* 7 n) (* n (1+ n) mmaxt)))
	   (iwk (make-array lenwk :element-type 'f2cl-lib:integer4))
	   (lamda (make-array total-deg :element-type 'double-float))
	   (arclen (make-array total-deg :element-type 'double-float))
	   (nfe (make-array total-deg :element-type 'f2cl-lib:integer4))
	   (roots (make-array (* 2 (1+ n) total-deg) :element-type 'double-float))
	   (iflg2 (if iflg2
		      (make-array total-deg :element-type 'f2cl-lib:integer4
					    :initial-contents (cdr iflg2))
		      (make-array total-deg :element-type 'f2cl-lib:integer4
			      :initial-element -2)))
	   (sspar (if sspar
		      (make-array 8 :element-type 'double-float
				    :initial-contents (cdr sspar))
		      (make-array 8 :element-type 'double-float
				:initial-element -1d0)))
	   (coef-array (convert-coef coef))
	   (kdeg-array (convert-kdeg kdeg numt))
	   (numt (make-array (length numt) :element-type 'f2cl-lib:integer4
					   :initial-contents numt)))
      (when $debug_hompack 
	(format t "coef-array ~A~%" coef-array)
	(format t "kdeg-array ~A~%" kdeg-array))

      (multiple-value-bind (ignore-n ignore-numt ignore-coef-array ignore-kdeg-array
			    ret-iflg1)
	  (hompack::polsys n numt coef-array kdeg-array iflg1 iflg2 epsbig epssml
			   sspar numrr n mmaxt total-deg lenwk leniwk
			   lamda roots arclen nfe wk iwk)
	(declare (ignore ignore-n ignore-numt ignore-coef-array ignore-kdeg-array))

	;; Convert the roots into a list where each element of the
	;; list is a list of the form [x1=r1, x2=r2, ..., xn=rn].
	(let
	    ((ret-code
	       ;; Figure out if anything bad happened.  If iflg1 is
	       ;; negative, just use it.  Otherwise, check that iflg2
	       ;; has a normal return for all values.  If so, return
	       ;; 0.  Otherwise return 1 to indicate that.
	       (cond ((minusp ret-iflg1)
		      ret-iflg1)
		     ((every #'(lambda (x) (= x 1)) iflg2)
		      0)
		     (t
		      1)))
	     (r (list* '(mlist)
		       (loop for m from 1 to total-deg
			     collect
			     (list* '(mlist)
				    (loop for j from 1 to n for var in (cdr varlist)
					  collect
					  (list
					   '(mequal)
					   var      
					   (add (f2cl-lib::fref roots
								(1 j m)
								((1 2) (1 (1+ n)) (1 total-deg)))
						(mul '$%i
						     (f2cl-lib::fref roots
								     (2 j m)
								     ((1 2) (1 (1+ n)) (1 total-deg))))))))))))

					       
	  (list '(mlist)
		ret-code
		r
		(list* '(mlist) (coerce iflg2 'list))
		(list* '(mlist) (coerce lamda 'list))
		(list* '(mlist) (coerce arclen 'list))
		(list* '(mlist) (coerce nfe 'list))))))))