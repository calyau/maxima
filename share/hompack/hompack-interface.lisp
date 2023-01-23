;;; -*- Mode: lisp -*-

;; Simple Maxima interface to hompack routines

(in-package #:maxima)

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
				    (setf prod (mul prod (power v p)))
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
	;; Now accumulate the results from each equation on to the final list
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

(defmfun $hompack_polsys (eqnlist varlist
				  &key
				  (iflg1 0) (epsbig 1d-4) (epssml 1d-14) (numrr 10)
				  iflg2 sspar)
  "Solve the system of n polynomial equations in n unknowns.

  EQNLIST   list of polynomial equations.  Must be in the form sum(c[k]*x1^p1*x2^p2*...*xn^pn)
  VARLIST   list of the n variables.

  Returns a list with the following components
   ifail    Indicates results of the algorithm:
              -1 -
              -2 -
              -3 - computed total deg is too small (shouldn't happen)
              -4 - length of real work area is too small (shouldn't happen)
              -5 - length of integer work area is too small (shouldn't happen)
              -6 - if ifgl1 is not 00 or 01 or 10 or 11 (shouldn't happen)
               0 - normal return (iflg2 list only contains 1)
               1 - error (iflg2 list has values other than 1)
   roots    The roots of the system of equations
   iflg2    How the path terminated
              1 - normal return
              2 - specified error tolerance cannot be met.  Increase epsbig
                  and epssml
              3 - maximum number of steps exceeded.  Increase numrr.
                  However, the path may be diverging if the lambda value is
                  near 1 and the root values are large
              4 - Jacobian matrix does not have full rank
              5 - Tracking algorithm has lost the zero curve and is not making
                  progress.  The error tolerances were too lenient.
              6 - Normal flow Newton iteration failed to converge.  The error
                  tolerance epsbig may be too stringent
              7 - Illegal input parameters
   lambda   List of the final lambda (continuation) parameter
   arclen   List of the arc length of the patch for each root
   nfe      List of the number of Jacobian matrix evaluations required
            to track each path

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
				    :initial-contents (cdr ($float sspar)))
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

	(flet
	    ((maxify-roots (roots)
	       (list* '
		(mlist)
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
	  (let
	      ((ret-code
		 ;; Figure out if anything bad happened.  If iflg1 is
		 ;; negative, just use it.  Otherwise, check that iflg2
		 ;; Has a normal return for all values.  If so, return
		 ;; 0.  Otherwise return 1 to indicate that.
		 (cond ((minusp ret-iflg1)
			ret-iflg1)
		       ((every #'(lambda (x) (= x 1)) iflg2)
			0)
		       (t
			1)))
	       (r (maxify-roots roots)))
					       
	    (list '(mlist)
		  ret-code
		  r
		  (list* '(mlist) (coerce iflg2 'list))
		  (list* '(mlist) (coerce lamda 'list))
		  (list* '(mlist) (coerce arclen 'list))
		  (list* '(mlist) (coerce nfe 'list)))))))))


(defvar *f-fun* nil)
(defvar *fjac-fun* nil)

(in-package "HOMPACK")

;; For HOMPACK_FIXPDF to evaluate the given function at the given
;; point X, returning the values in V.  The function is in *F-FUN*,
;; computed by Maxima.
(defun f (x v)
  (declare (type (array double-float (*)) v x))
  (f2cl-lib:with-multi-array-data
      ((x cl:double-float x-%data% x-%offset%)
       (v cl:double-float v-%data% v-%offset%))
    (funcall maxima::*f-fun* x v)
    (values nil nil)))


;; For HOMPACK_FIXPDF to evaluate the k-th column of the Jacobian at
;; the given point X, returning the values in V.  The Jacobian is in
;; *FJAC-FUN*, computed by Maxima.
(defun fjac (x v k)
  (declare (type (f2cl-lib:integer4) k) (type (cl:array cl:double-float (*)) v x))
  (f2cl-lib:with-multi-array-data
      ((x double-float x-%data% x-%offset%)
       (v double-float v-%data% v-%offset%))
    (funcall maxima::*fjac-fun* x v k)
    (values nil nil nil)))

(in-package "MAXIMA")

(defmfun $hompack_fixpdf (fcns varlist
			       &key
			       (iflag -1) (arctol -1d0) (eps 1d-5) (trace 0) inita)
  "hompack_fixpdf"

  (let* ((n (length (cdr varlist)))
	 (y (make-array (1+ n) :element-type 'double-float))
	 (ndima n)
	 (a (make-array ndima :element-type 'double-float))
	 (yp (make-array (1+ n) :element-type 'double-float))
	 (ypold (make-array (1+ n) :element-type 'double-float))
	 (qr (make-array (* n (1+ n)) :element-type 'double-float))
	 (alpha (make-array n :element-type 'double-float))
	 (tz (make-array (1+ n) :element-type 'double-float))
	 (pivot (make-array (1+ n) :element-type 'f2cl-lib:integer4))
	 (wt (make-array (1+ n) :element-type 'double-float))
	 (phi (make-array (* 16 (1+ n)) :element-type 'double-float))
	 (p (make-array (1+ n) :element-type 'double-float))
	 (par (make-array 1 :element-type 'double-float))
	 (ipar (make-array 1 :element-type 'f2cl-lib:integer4))
	 (arclen 0d0)
	 (nfe 0)
	 (fvs (coerce-float-fun fcns varlist))
	 (fj (let ((fj (meval `(($jacobian) ,fcns ,varlist))))
	       (mapcar #'(lambda (fjf)
			   (coerce-float-fun fjf varlist))
		       ;; The Fortran code wants the k-th column, so
		       ;; transpose the Jacobian here to make the
		       ;; interface between Maxima and Fortran a
		       ;; little simpler.  Then we can just extract
		       ;; the k-th row of the transpose to get what's
		       ;; needed.
		       (cdr ($transpose fj)))))
	 (*f-fun*
	   #'(lambda (x v)
	       ;; Compute the functions at the point X and return the
	       ;; value in V.
	       (declare (type (cl:array cl:double-float (*)) x v))
	       (let* ((x-list (coerce x 'list))
		      (v-list (apply fvs x-list)))
		 (map-into v #'identity (cdr v-list)))))
	 (*fjac-fun*
	   #'(lambda (x v k)
	       (declare (type (f2cl-lib:integer4) k) (type (cl:array cl:double-float (*)) v x))
	       ;; Compute the k'th column of the Jacobian matrix of
	       ;; F(x) evaluated at X and store the result in V.
	       (let* ((x-list (coerce x 'list))
		      (v-list (apply (elt fj (1- k)) x-list)))
		 (map-into v #'identity (cdr v-list))))))

    (when inita
      (loop for ia in (cdr inita)
	    for k from 0 below (length a)
	    do
	       (let ((val ($float ia)))
		 (setf (aref a k) val)
		 (setf (aref y (1+ k)) val))))
    (multiple-value-bind (ignore-n ignore-y
			  iflag arctol eps
			  ignore-trace ignore-a ignore-ndima
			  nfe
			  arclen ignore-yp ignore-ypold ignore-qr ignore-alpha
			  ignore-tz ignore-pivot ignore-wt ignore-phi ignore-p
			  ignore-par ignore-ipar)
	(hompack::fixpdf n y iflag arctol eps trace a ndima nfe arclen yp ypold
			 qr alpha tz pivot wt phi p par ipar)
      (declare (ignore ignore-n ignore-y ignore-trace ignore-a ignore-ndima
		       ignore-yp ignore-ypold ignore-qr ignore-alpha
		       ignore-tz ignore-pivot ignore-wt ignore-phi ignore-p ignore-par
		       ignore-ipar))
      (let ((ans-y (list* '(mlist) (rest (coerce y 'list)))))
	(list '(mlist)
	       iflag
	       ans-y
	       arctol
	       eps
	       nfe
	       arclen)))))
