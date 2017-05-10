(in-package #-gcl #:maxima #+gcl "MAXIMA")

;; Initialize for DLSODE.  This must be called before running DLSODE_STEP.
;;
;; Parameters:
;;   f    - list of differential equations to be solved
;;   vars - list of the independent variable and the dependent
;;          variables.  The order of the dependent variables must be
;;          in the same order as the equations in f.

;;   mf   - method to be used.  It should be one of the following
;;          values:

;;            10  Nonstiff (Adams) method, no Jacobian used.
;;            21  Stiff (BDF) method, user-supplied full Jacobian.
;;            22  Stiff method, internally generated full
;;                Jacobian.
;;           The Fortran version of DLSODE supports additional
;;           methods, but these are not supported here.
;; Output:
;;   A state object is created that should be used as the state
;;   parameter in DLSODE_STEP.  The user must not modify these. The
;;   output includes the equations, the set of variables, the mf
;;   parameter, and various work arrays that must be modified between
;;   calls to dlsode_step.
(defun-checked $dlsode_init ((f vars mf))
  ;; Verify values of mf.
  (unless (member mf '(10 21 22) :test #'eql)
    (merror "MF must be 10, 21, or 22"))
  (let* ((neq (length (cdr f)))
	 (lrw (ecase mf
		(10
		 (+ 20 (* 16 neq)))
		((21 22)
		 (+ 22 (* 9 neq) (* neq neq)))))
	 (rwork (make-array lrw :element-type 'flonum))
	 (liw (ecase mf
		(10
		 20)
		((21 22)
		 (+ 20 neq))))
	 (iwork (make-array liw :element-type 'f2cl-lib:integer4))
	 (fjac (when (= mf 21)
		 ;; Jacobian is needed only for mf = 21, so compute only then.
		 (compile nil
			  (coerce-float-fun
			   (meval `(($jacobian) ,f ,(list* '(mlist) (cddr vars))))
			   vars)))))
    ;; Make sure neq is consistent with the number of elements in f
    ;; and vars
    (unless (= (1+ neq) ($length vars))
      (merror "Expected ~M variables but got ~M: ~M"
	      (1+ neq) ($length vars) vars))
    (make-mlist
     (make-mlist '$f (compile nil (coerce-float-fun f vars)))
     (make-mlist '$vars vars)
     (make-mlist '$mf mf)
     (make-mlist '$neq neq)
     (make-mlist '$lrw lrw)
     (make-mlist '$liw liw)
     (make-mlist '$rwork rwork)
     (make-mlist '$iwork iwork)
     (make-mlist '$fjac fjac))))

;; Main DLSODE routine to compute each output point.  Must be called
;; after DLSODE_INIT.
;;
;; For full details see comments in fortran/dlsode.f
;;
;; Parameters:
;;
;;   init-y - For the first call (when istate = 1), the initial values
;;   tt     - Value of the independent value
;;   tout   - Next point where output is desired (/= tt)
;;   rtol   - relative tolerance parameter

;;   atol   - Absolute tolerance parameter, scalar of vector.  If
;;            scalar, it applies to all dependent variables.
;;            Otherwise it must be the tolerance for each dependent
;;            variable.
;;
;;            Use rtol = 0 for pure absolute error and use atol = 0
;;            for pure relative error.
;;            
;;   istate - 1 for the first call to dlsode, 2 for subsequent calls.
;;   state  - state returned by dlsode-init.
;;
;; Output:
;;   A list consisting of the following items:
;;     t      - independent variable value
;;     y      - list of values of the dependent variables at time t.
;;     istate - Integration status:
;;                 1 - no work because tout = tt
;;                 2 - successful result
;;                -1 - Excess work done on this call
;;                -2 - Excess accuracy requested
;;                -3 - Illegal input detected
;;                -4 - Repeated error test failures
;;                -5 - Repeated convergence failures (perhaps bad
;;                     Jacobian or wrong choice of mf or tolerances)
;;                -6 - Error weight becase zero during problem
;;                     (solution component i vanishded and atol(i) = 0.
;;     info   - association list of various bits of information:
;;                n_steps      - total steps taken thus far
;;                n_f_eval     - total number of function evals
;;                n_j_eval     - total number of Jacobian evals
;;                method_order - method order
;;                len_rwork    - Actual length used for real work array
;;                len_iwork    - Actual length used for integer work array
;;
(defun-checked $dlsode_step ((init-y tt tout rtol atol istate state))
  (let ((f ($assoc '$f state))
	(vars ($assoc '$vars state))
	(mf ($assoc '$mf state))
	(neq ($assoc '$neq state))
	(lrw ($assoc '$lrw state))
	(liw ($assoc '$liw state))
	(rwork ($assoc '$rwork state))
	(iwork ($assoc '$iwork state))
	(fjac ($assoc '$fjac state)))
    ;; Verify that we got something from state.  (Do we need more validation?)
    (unless (and f vars mf neq lrw liw rwork iwork)
      (merror "State appears to be invalid"))
    (let* ((ff f)
	   (itol (if (listp atol)
		     2
		     1))
	   (y-array (make-array (1- (length init-y))
				:element-type 'double-float
				:initial-contents (cdr ($float init-y)))))
      #+nil
      (progn
	(format t "vars = ~S~%" vars)
	(format t "ff = ~S~%" ff)
	(format t "fjac = ~S~%" fjac))
      (flet ((fex (neq tt y ydot)
	       (declare (type double-float tt)
			(type (cl:array double-float (*)) y ydot)
			(ignore neq))
	       (let* ((y-list (coerce y 'list))
		      (yval (cl:apply ff tt y-list)))
		 #+nil
		 (progn
		   (format t "fex: t = ~S; y = ~S~%" tt y)
		   (format t "     yval = ~S~%" yval)
		   (format t "     ydot = ~S~%" ydot))
		 (replace ydot (cdr yval))))
	     (jex (neq tt y ml mu pd nrpd)
	       (declare (type f2cl-lib:integer4 ml mu nrpd)
			(type double-float tt)
			(type (cl:array double-float (*)) y)
			(type (cl:array double-float *) pd)
			(ignore neq ml mu))
	       #+nil
	       (progn
		 (format t "jex: tt = ~S; y = ~S~%" tt y)
		 (format t "     ml, mu = ~S ~S~%" ml mu)
		 (format t "     pd = ~S~%" pd)
		 (format t "     nrpd = ~S~%" nrpd)
		 (format t "     fjac = ~S~%" fjac))
	       (let* ((y-list (coerce y 'list))
		      (j (cl:apply fjac tt y-list))
		      (row 1))
		 #+nil
		 (progn
		   (format t "    y-list = ~S~%" y-list)
		   (format t "    j = ~S~%" j))
		 (dolist (r (cdr j))
		   (let ((col 1))
		     (dolist (c (cdr r))
		       (setf (f2cl-lib:fref pd (row col) ((1 nrpd) (1)))
			     c)
		       (incf col)))
		   (incf row))
		 #+nil
		 (format t "pd = ~S~%" pd))))

	(multiple-value-bind (ign-0 ign-1 ign-2
			      ret-tout
			      ign-5 ign-6 ign-7 ign-8 ign-9
			      ret-istate)
	    (odepack:dlsode #'fex
			    (make-array 1 :element-type 'f2cl-lib:integer4
					  :initial-element neq)
			    y-array
			    tt
			    tout
			    itol
			    (make-array 1 :element-type 'double-float
					  :initial-element ($float rtol))
			    (if (listp atol)
				(make-array (1- (length atol))
					    :element-type 'double-float
					    :initial-contents (rest ($float atol)))
				(make-array 1 :element-type 'double-float
					    :initial-element ($float atol)))
			    1 ;;  itask
			    istate
			    0 ;; iopt
			    rwork
			    lrw
			    iwork
			    liw
			    #'jex
			    mf)
	  (declare (ignore ign-0 ign-1 ign-2 ign-5 ign-6 ign-7 ign-8 ign-9))

	  (list '(mlist)
		ret-tout
		(list* '(mlist)
		       (coerce y-array 'list))
		ret-istate
		(make-mlist
		 (make-mlist '$n_steps (aref iwork 10))
		 (make-mlist '$n_f_eval (aref iwork 11))
		 (make-mlist '$n_j_eval (aref iwork 12))
		 (make-mlist '$method_order (aref iwork 13))
		 (make-mlist '$len_rwork (aref iwork 16))
		 (make-mlist '$len_iwork (aref iwork 17)))))))))

(defun-checked $dlsode ((f yvars inity trange rtol atol mf))
  (let* ((tvar (elt trange 1))
	 (tstart ($float (elt trange 2)))
	 (tend ($float (elt trange 3)))
	 (tstep ($float (elt trange 4)))
	 (vars ($cons tvar yvars))
	 (state ($dlsode_init f vars mf))
	 result)
    (loop for tout from tstart upto tend by tstep
	  do
	     (let ((r ($dlsode_step inity tstart tout rtol atol 1 state)))
	       (when (minusp (elt r 3))
		 ;; See dlsode.f for the definitions for these values.
		 (merror "Error ~M: ~M"
			 (elt r 3)
			 (ecase (elt r 3)
			   (-1
			    "Excess work done on this call (perhaps wrong MF")
			   (-2
			    "Excess accuracy requested (tolerances too small)")
			   (-3
			    "Illegal input detected (see printed message)")
			   (-4
			    "Repeated error test failures (check all inputs)")
			   (-5
			    "Repeated convergence failures (perhaps bad Jacobian supplied or wrong choice of MF or tolerances)")
			   (-6
			    "Error weight became zero during problem (solution component i vanished, and ATOL or ATOL(i) = 0.)"))))
	       (push ($cons (elt r 1) (elt r 2))
		     result)))
    (list* '(mlist)
	   (nreverse result))))
