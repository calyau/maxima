(in-package #:maxima)

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
    (make-mlist
     (make-mlist '$mf mf)
     (make-mlist '$neq neq)
     (make-mlist '$lrw lrw)
     (make-mlist '$liw liw)
     (make-mlist '$rwork rwork)
     (make-mlist '$iwork iwork)
     (make-mlist '$fjac fjac))))

(defun-checked $dlsode_step ((f vars init-y tt tout rtol atol istate state))
  (let ((mf ($assoc '$mf state))
	(neq ($assoc '$neq state))
	(lrw ($assoc '$lrw state))
	(liw ($assoc '$liw state))
	(rwork ($assoc '$rwork state))
	(iwork ($assoc '$iwork state))
	(fjac ($assoc '$fjac state)))
    ;; Verify that we got something from state.  (Do we need more validation?)
    (unless (and mf neq lrw liw rwork iwork)
      (merror "State appears to be invalid"))
    ;; Make sure neq is consistent with the number of elements in f
    ;; and vars
    (unless (= neq ($length f))
      (merror "Expected ~M equations but got ~M: ~M"
	      neq ($length f) f))
    (unless (= (1+ neq) ($length vars))
      (merror "Expected ~M variables but go ~M: ~M"
	      (1+ neq) ($length vars) vars))
    (let* ((ff (compile nil (coerce-float-fun f vars)))
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
			(ignore neq))
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
				($float atol))
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
