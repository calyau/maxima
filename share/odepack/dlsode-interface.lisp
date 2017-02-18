(in-package #:maxima)

(defun $dlsode_init (f mf)
  (let* ((neq (length (cdr f)))
	 (lrw (ecase mf
		(10
		 (+ 20 (* 16 neq)))
		((21 22)
		 (+ 22 (* 9 neq) (* neq neq)))))
	 (rwork (make-array lrw :element-type 'flonum))
	 (liw (ecase mf
		(10
		 10)
		((21 22)
		 (+ 20 neq))))
	 (iwork (make-array liw :element-type 'f2cl-lib:integer4)))
    (make-mlist
	  (make-mlist '$mf mf)
	  (make-mlist '$neq neq)
	  (make-mlist '$lrw lrw)
	  (make-mlist '$liw liw)
	  (make-mlist '$rwork rwork)
	  (make-mlist '$iwork iwork))))

(defun $dlsode (vars f init-y tt tout rtol atol istate state)
  (let ((mf ($assoc '$mf state))
	(neq ($assoc '$neq state))
	(lrw ($assoc '$lrw state))
	(liw ($assoc '$liw state))
	(rwork ($assoc '$rwork state))
	(iwork ($assoc '$iwork state)))
    (let* ((ff (compile nil (coerce-float-fun f vars)))
	   (fjac (compile nil
			  (coerce-float-fun
			   (meval `(($jacobian) ,f ,(list* '(mlist) (cddr vars))))
			   vars)))
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
	       (declare (type f2cl-lib:integer4 neq)
			(type double-float tt)
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
	       (declare (type f2cl-lib:integer4 neq ml mu nrpd)
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
					  :initial-element rtol)
			    (if (listp atol)
				(make-array (1- (length atol))
					    :element-type 'double-float
					    :initial-contents (rest atol))
				atol)
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
		(list '(mlist)
		      (list '(mequal) '$n_steps (aref iwork 10))
		      (list '(mequal) '$n_f_eval (aref iwork 11))
		      (list '(mequal) '$n_j_eval (aref iwork 12))
		      (list '(mequal) '$method_order (aref iwork 13))
		      (list '(mequal) '$len_rwork (aref iwork 16))
		      (list '(mequal) '$len_iwork (aref iwork 17)))))))))
  
(defun $dlsode_aux (vars f init-y tt tout rtol atol istate mf)
  (let* ((neq (length (cdr f)))
	 (lrw (ecase mf
		(10
		 (+ 20 (* 16 neq)))
		((21 22)
		 (+ 22 (* 9 neq) (* neq neq)))))
	 (rwork (make-array lrw :element-type 'flonum))
	 (liw (ecase mf
		(10
		 10)
		((21 22)
		 (+ 20 neq))))
	 (iwork (make-array liw :element-type 'f2cl-lib:integer4))
	 (ff (compile nil (coerce-float-fun f vars)))
	 (fjac (compile nil
			(coerce-float-fun
			 (meval `(($jacobian) ,f ,(list* '(mlist) (cddr vars))))
			 vars)))
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
	     (declare (type f2cl-lib:integer4 neq)
		      (type double-float tt)
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
	     (declare (type f2cl-lib:integer4 neq ml mu nrpd)
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
				      :initial-element rtol)
			  (if (listp atol)
			      (make-array (1- (length atol))
					  :element-type 'double-float
					  :initial-contents (rest atol))
			      atol)
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
	       (list '(mlist)
		     (list '(mequal) '$n_steps (aref iwork 10))
		     (list '(mequal) '$n_f_eval (aref iwork 11))
		     (list '(mequal) '$n_j_eval (aref iwork 12))
		     (list '(mequal) '$method_order (aref iwork 13))
		     (list '(mequal) '$len_rwork (aref iwork 16))
		     (list '(mequal) '$len_iwork (aref iwork 17))))))))
		
	
