;;; Maxima interface to quadpack integration

(in-package :maxima)

(defvar *debug-quadpack*
  nil
  "Set to non-NIL to enable printing of the error object when the
  Slatec routines throw an error.")

#-(or gcl ecl)
(defmacro get-integrand (fun var)
  `(compile nil (coerce-float-fun ,fun `((mlist) ,,var))))

#+(or gcl ecl)
(defmacro get-integrand (fun var)
  `(coerce-float-fun ,fun `((mlist) ,,var)))

(defun float-or-lose (val)
  (let ((v ($float val)))
    (if (numberp v)
	v
	(error (intl:gettext "~A is not a real floating-point number") val))))

;; Convert the maxima function FUN of the one variable VAR to a
;; compiled lisp function returning a float that is suitable for use
;; as the integrand for the quadpack routines.  If the integrand
;; cannot be evaluated at the specified point, signal an error.
(defmacro float-integrand-or-lose (name fun var)
  (let ((f (gensym "COMPILED-FUN-"))
	(x (gensym "VAR-"))
	(result (gensym "RESULT-")))
    `(let ((,f (get-integrand ,fun ,var)))
       (lambda (,x)
	 (let ((,result (funcall ,f ,x)))
	   (unless (cl:numberp ,result)
	     (merror (intl:gettext "~M: Cannot numerically evaluate ~M at ~M") ,name ,fun ,x))
	   (float ,result))))))
     
(defmfun $quad_qag (fun var a b key &key
			(epsrel 1e-8)
			(limit 200)
			(epsabs 0.0))
  (quad_argument_check %%pretty-fname fun var a b)
  (unless (and (integerp key) (>= key 1) (<= key 6))
    (merror (intl:gettext "~M: Key must be 1, 2, 3, 4, 5, or 6; found: ~M")
	    %%pretty-fname key))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-epsabs z-epsrel z-key result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqag (float-integrand-or-lose '$quad_qag fun var)
			 (float-or-lose a)
			 (float-or-lose b)
			 (float-or-lose epsabs)
			 (float-or-lose epsrel)
			 key
			 0.0 0.0 0 0
			 limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-epsabs z-epsrel z-key z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error (e)
	(when *debug-quadpack*
	  (format t "~S" e))
	`(($quad_qag) ,fun ,var ,a ,b ,key
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))

(defmfun $quad_qags (fun var a b &key
		  (epsrel 1e-8)
		  (limit 200)
		  (epsabs 0.0))
  (quad_argument_check %%pretty-fname fun var a b) 
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-epsabs z-epsrel result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqags (float-integrand-or-lose '$quad_qags fun var)
			  (float-or-lose a)
			  (float-or-lose b)
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-epsabs z-epsrel z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error (e)
	(when *debug-quadpack*
	  (format t "~S" e))
	`(($quad_qags) ,fun ,var ,a ,b
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))

(defmfun $quad_qagi (fun var a b &key
		      (epsrel 1e-8)
		      (limit 200)
		      (epsabs 0.0))
  (quad_argument_check %%pretty-fname fun var a b)
  ;; Massage the limits a and b into what Quadpack QAGI wants.
  (flet ((fixup (low high)
	   (let (bnd inf)
	     ;; Cases to handle: (minf, x), (x, inf), (minf, inf).
	     ;; Everything else is an error.
	     (cond ((eq low '$minf)
		    (cond ((eq high '$inf)
			   (setf bnd 0)
			   (setf inf 2))
			  (t
			   (setq bnd ($float high))
			   (setq inf low))))
		   ((eq high '$inf)
		    (setq bnd ($float low))
		    (setq inf high))
		   (t
		    (merror "~M: Unexpected limits of integration: ~M, ~M~%"
			    %%pretty-fname low high)))
	     (values bnd inf))))

    (multiple-value-bind (bound inf-type)
	(fixup a b)
      (let* ((lenw (* 4 limit))
	     (work (make-array lenw :element-type 'flonum))
	     (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	     (infinity (ecase inf-type
			 ((1 $inf)
			  ;; Interval is [bound, infinity]
			  1)
			 ((-1 $minf)
			  ;; Interval is [-infinity, bound]
			  -1)
			 ((2 $both)
			  ;; Interval is [-infinity, infinity]
			  2))))
	(handler-case
	    (multiple-value-bind (junk z-bound z-inf z-epsabs z-epsrel
				       result abserr neval ier
				       z-limit z-lenw last)
		(slatec:dqagi (float-integrand-or-lose '$quad_qagi fun var)
			      (float-or-lose bound)
			      infinity
			      (float-or-lose epsabs)
			      (float-or-lose epsrel)
			      0.0 0.0 0 0
			      limit lenw 0 iwork work)
	      (declare (ignore junk z-bound z-inf z-epsabs z-epsrel
			       z-limit z-lenw last))
	      (list '(mlist) result abserr neval ier))
	  (error (e)
	    (when *debug-quadpack*
	      (format t "~S" e))
	    `(($quad_qagi) ,fun ,var ,a ,b
	      ((mequal) $epsrel ,epsrel)
	      ((mequal) $epsabs ,epsabs)
	      ((mequal) $limit ,limit))))))))

(defmfun $quad_qawc (fun var c a b &key
		  (epsrel 1e-8)
		  (limit 200)
		  (epsabs 0.0))
  (quad_argument_check %%pretty-fname fun var a b) 
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-c z-epsabs z-epsrel result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqawc (float-integrand-or-lose '$quad_qawc fun var)
			  (float-or-lose a)
			  (float-or-lose b)
			  (float-or-lose c)
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-c z-epsabs z-epsrel z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error (e)
	(when *debug-quadpack*
	  (format t "~S" e))
	`(($quad_qawc) ,fun ,var ,c ,a ,b
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))

(defmfun $quad_qawf (fun var a omega trig &key
		  (epsabs 1e-10)
		  (limit 200)
		  (maxp1 100)
		  (limlst 10))
  (let* ((leniw limit)
	 (lenw (+ (* 2 leniw) (* 25 maxp1)))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array leniw :element-type 'f2cl-lib:integer4))
	 (integr (ecase trig
		   ((1 %cos $cos) 1)
		   ((2 %sin $sin) 2))))
    (handler-case
	(multiple-value-bind (junk z-a z-omega z-integr
				   epsabs result abserr neval ier
				   z-limlst z-lst
				   z-leniw z-maxp1 z-lenw)
	    (slatec:dqawf (float-integrand-or-lose '$quad_qawf fun var)
			  (float-or-lose a)
			  (float-or-lose omega)
			  integr
			  (float-or-lose epsabs)
			  0.0 0.0 0 0
			  limlst 0 leniw maxp1 lenw iwork work)
	  (declare (ignore junk z-a z-omega z-integr epsabs z-limlst z-lst
			   z-leniw z-maxp1 z-lenw))
	  (list '(mlist) result abserr neval ier))
      (error (e)
	(when *debug-quadpack*
	  (format t "~S" e))
	`(($quad_qawf) ,fun ,var ,a ,omega ,trig
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit)
	  ((mequal) $maxp1 ,maxp1)
	  ((mequal) $limlst ,limlst))))))

(defmfun $quad_qawo (fun var a b omega trig &key
		  (epsrel 1e-8)
		  (limit 200)
		  (maxp1 100)
		  (epsabs 0.0))
  (quad_argument_check %%pretty-fname fun var a b) 
  (let* ((leniw limit)
	 (lenw (+ (* 2 leniw) (* 25 maxp1)))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array leniw :element-type 'f2cl-lib:integer4))
	 (integr (ecase trig
		   ((1 %cos $cos) 1)
		   ((2 %sin $sin) 2))))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-omega z-integr z-epsabs z-epsrel
				   result abserr neval ier
				   z-leniw z-maxp1 z-lenw z-lst)
	    (slatec:dqawo (float-integrand-or-lose '$quad_qawo fun var)
			  (float-or-lose a)
			  (float-or-lose b)
			  (float-or-lose omega)
			  integr
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  leniw maxp1 lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-omega z-integr z-epsabs z-epsrel
			   z-lst z-leniw z-maxp1 z-lenw))
	  (list '(mlist) result abserr neval ier))
      (error (e)
	(when *debug-quadpack*
	  (format t "~S" e))
	`(($quad_qawo) ,fun ,var ,a ,b ,omega ,trig
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit)
	  ((mequal) $maxp1 ,maxp1))))))

(defmfun $quad_qaws (fun var a b alfa beta wfun &key
		  (epsrel 1e-8)
		  (limit 200)
		  (epsabs 0.0))
  (quad_argument_check %%pretty-fname fun var a b) 
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-alfa z-beta z-int z-epsabs z-epsrel
				   result abserr neval ier
				   z-limit z-lenw last)
	    (slatec:dqaws (float-integrand-or-lose '$quad_qaws fun var)
			  (float-or-lose a)
			  (float-or-lose b)
			  (float-or-lose alfa)
			  (float-or-lose beta)
			  wfun
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  limit lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-alfa z-beta z-int z-epsabs z-epsrel
			   z-limit z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error (e)
	(when *debug-quadpack*
	  (format t "~S" e))
	`(($quad_qaws) ,fun ,var ,a ,b ,alfa ,beta ,wfun
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))

(defmfun $quad_qagp (fun var a b points
		  &key (epsrel 1e-8) (epsabs 0.0) (limit 200))
  (quad_argument_check %%pretty-fname fun var a b)
  (let* ((npts2 (+ 2 (length (cdr points))))
	 (p (make-array npts2 :element-type 'flonum))
	 (leniw (max limit (- (* 3 npts2) 2)))
	 (lenw (- (* 2 leniw) npts2))
	 (work (make-array lenw :element-type 'flonum))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4)))
    (map-into p #'float-or-lose (cdr points))
    (handler-case
	(multiple-value-bind (junk z-a z-b z-npts z-points z-epsabs z-epsrel
				   result abserr neval ier
				   z-leniw z-lenw last)
	    (slatec:dqagp (float-integrand-or-lose '$quad_qagp fun var)
			  (float-or-lose a)
			  (float-or-lose b)
			  npts2
			  p
			  (float-or-lose epsabs)
			  (float-or-lose epsrel)
			  0.0 0.0 0 0
			  leniw lenw 0 iwork work)
	  (declare (ignore junk z-a z-b z-npts z-points z-epsabs z-epsrel
			   z-leniw z-lenw last))
	  (list '(mlist) result abserr neval ier))
      (error (e)
	(when *debug-quadpack*
	  (format t "~S" e))
	`(($quad_qagp) ,fun ,var ,a ,b ,points
	  ((mequal) $epsrel ,epsrel)
	  ((mequal) $epsabs ,epsabs)
	  ((mequal) $limit ,limit))))))
					
;; error checking similar to that done by $defint
(defun quad_argument_check (name exp var ll ul) 
  (setq exp (ratdisrep exp))
  (setq var (ratdisrep var))
  (setq ll (ratdisrep ll))
  (setq ul (ratdisrep ul))
  (cond (($constantp var)
	 (merror "~M: Variable of integration not a variable: ~M" name var)))
  (cond ((not (or ($subvarp var) (atom var)))
	 (merror "~M: Improper variable of integration: ~M" name var))
	((or (among var ul)
	     (among var ll))
	 (merror "~M: Limit contains variable of integration: ~M" name var))))

(defun quad-control (parameter &optional new-value)
  (values
   (slatec:j4save (case parameter
		    ($current_error 1)
		    ($control 2)
		    ($max_message 4)
		    (otherwise
		     (merror "Parameter should be current_error, control, or max_message")))
		  (or new-value 0)
		  (if new-value t nil))))

(defmfun $quad_control (parameter &rest new-value)
  (quad-control parameter (if new-value (car new-value))))

;; Tests
;;
;; These tests were taken from the QUADPACK book.
;;

;; Test 1
;; integrate(x^alpha*log(1/x),x,0,1)
;; => (1+alpha)^(-2)
;;
;; alpha = 0.9(0.1)0(0.2)2.6
;;
;; QAG with key 1, 3, 6
;;
;; For key = 1, 3, 6: fails for alpha = -0.9 (ier = 3)
;;
;; quad_qag(x^2*log(1/x),x,0,1,3)
;;
;;  for alpha : -0.9 thru 0 step 0.1 do print(alpha, float((1+alpha)^(-2)), quad_qag(x^alpha*log(1/x),x,0,1,3));
;;  for alpha : 0.0 thru 2.6 step 0.2 do print(alpha, float((1+alpha)^(-2)), quad_qag(x^alpha*log(1/x),x,0,1,3));

;; Test 2
;; integrate(4^(-alpha)/((x - %pi/4)^2 + 16^(-alpha)), x, 0, 1)
;; => atan((4-%pi)*4^(alpha-1)) + atan(%pi*4^(alpha-1))
;;
;; alpha = 0(1)20
;; QAG with key = 1, 3, 6
;;
;; Fails for key = 1: alpha >= 18 (ier = 2)
;; Fails for key = 3, 6: alpha >= 19 (ier = 2)
;;
;; for alpha : 0.0 thru 20 step 1 do print(alpha, float(atan((4-%pi)*4^(alpha-1)) + atan(%pi*4^(alpha-1))), quad_qag(4^(-alpha)/((x - %pi/4)^2 + 16^(-alpha)),x,0,1,3));

;; Test 3
;; integrate(cos(2^alpha*sin(x)), x, 0, %pi)
;; => %pi * J0(2^alpha)
;;
;; alpha = 0(1)10
;;
;; QAG with Key 1, 3, 6
;;
;; for alpha : 0.0 thru 10 step 1 do print(alpha, float(%pi * bessel_j(0,2^alpha)), quad_qag(cos(2^alpha*sin(x)),x,0,float(%pi),3));

;; Test 4 (same integral as 1)
;; integrate(x^alpha*log(1/x),x,0,1)
;; => (1+alpha)^(-2)
;;
;; DQNG, DQAGS, DQAG (key = 1)
;;
;; Failures:
;; DQNG: alpha <= 1.0 (ier = 1)
;; DQAG: alpha = -0.9 (ier = 3)
;;
;;  for alpha : -0.9 thru 0 step 0.1 do print(alpha, float((1+alpha)^(-2)), quad_qags(x^alpha*log(1/x),x,0,1));
;;  for alpha : 0.0 thru 2.6 step 0.2 do print(alpha, float((1+alpha)^(-2)), quad_qags(x^alpha*log(1/x),x,0,1));
;;
;;  for alpha : -0.9 thru 0 step 0.1 do print(alpha, float((1+alpha)^(-2)), quad_qag(x^alpha*log(1/x),x,0,1, 1));
;;  for alpha : 0.0 thru 2.6 step 0.2 do print(alpha, float((1+alpha)^(-2)), quad_qag(x^alpha*log(1/x),x,0,1, 1));


;; Test 5
;; Same integral as 2
;;
;; DQNG, DQAGS, DQAG (key = 1)
;;
;; Failures:
;; DQNG:  alpha >= 2 (ier = 1)
;; DQAGS: alpha >= 10 (ier = 5)
;; DQAG:  alpha >= 18 (ier = 2)
;;
;; for alpha : 0.0 thru 20 step 1 do print(alpha, float(atan((4-%pi)*4^(alpha-1)) + atan(%pi*4^(alpha-1))), quad_qag(4^(-alpha)/((x - %pi/4)^2 + 16^(-alpha)),x,0,1,1));
;; for alpha : 0.0 thru 20 step 1 do print(alpha, float(atan((4-%pi)*4^(alpha-1)) + atan(%pi*4^(alpha-1))), quad_qags(4^(-alpha)/((x - %pi/4)^2 + 16^(-alpha)),x,0,1));


;; Test 6
;; Same integral as test 3
;;
;; DQNG, DQAGS, DQAG (key = 6)
;;
;; for alpha : 0.0 thru 10 step 1 do print(alpha, float(%pi * bessel_j(0,2^alpha)), quad_qags(cos(2^alpha*sin(x)),x,0,float(%pi),3));
;; Failures:
;; DQNG: alpha >= 7 (ier = 1)

;; Test 7
;; integrate(|x - 1/3|^alpha, x, 0, 1)
;; => ((2/3)^(alpha + 1) + (1/3)^(alpha + 1))/(alpha + 1)
;;
;; alpha = -0.8(0.1)2.1
;; DQAGS, DQAGP (point of singularity supplied)
;;
;; 
;; for alpha : -8 thru 21 do print(float(alpha/10), float(((2/3)^(alpha/10 + 1) + (1/3)^(alpha/10 + 1))/(alpha/10 + 1)), quad_qags(abs(x-1/3)^(alpha/10), x, 0, 1));
;; No failures.

;; Test 8
;; integrate(abs(x - pi/4)^alpha, x, 0, 1)
;; => ((1-pi/4)^(alpha+1) + (pi/4)^(alpha + 1))/(alpha + 1)
;;
;; alpha = -0.8(0.1)2.1
;;
;; DQAGS, DQAGP
;;
;; for alpha : -8 thru 21 do print(float(alpha/10), float(((1-%pi/4)^(alpha/10+1) + (%pi/4)^(alpha/10 + 1))/(alpha/10 + 1)), quad_qags(abs(x-%pi/4)^(alpha/10), x, 0, 1));
;; Failures:
;; DQAGS: alpha <= -0.5 (ier = 3)

;; Test 9
;;
;; integrate((1-x*x)^(-1/2)/(x+1+2^(-alpha)),x, -1, 1)
;; => %pi*((1+2^(-alpha))^2-1)^(-1/2)
;;
;; alpha = 1(1)20
;;
;; for alpha : 1 thru 20 do print(alpha, float(%pi*((1+2^(-alpha))^2-1)^(-1/2)),quad_qaws(1/(x+1+2^(-alpha)), x, -1, 1, -0.5, -0.5, 1));

;; Test 10
;; integrate((sin(x))^(alpha - 1), x, 0, %pi/2) =
;; integrate(x^(alpha - 1)*(sin(x)/x)^(alpha-1), x, 0, %pi/2)
;; => 2^(alpha - 2)*(Gamma(alpha/2))^2/Gamma(alpha)
;;
;; alpha = 0.1(0.1)2
;;
;; DQAGS, DQAWS
;; Failures:
;; None.
;; for alpha : 1 thru 20 do print(alpha/10.0, float(2^(alpha/10 - 2)*(gamma(alpha/20))^2/gamma(alpha/10)),quad_qags((sin(x))^(alpha/10 - 1), x, 0, %pi/2));
;;
;; for alpha : 1 thru 20 do print(alpha/10.0, float(2^(alpha/10 - 2)*(gamma(alpha/20))^2/gamma(alpha/10)),quad_qaws(if equal(x,0.0) then 1 else (sin(x)/x)^(alpha/10 - 1), x, 0, %pi/2, alpha/10-1, 0, 1));

;; Test 11
;; integrate((log(1/x))^(alpha-1), x, 0, 1) =
;; integrate((1-x)^(alpha - 1)*(log(1/x)/(1-x))^(alpha-1), x, 0, 1)
;; => Gamma(alpha)
;;
;; alpha = 0.1(0.1)2
;;
;; DQAGS, DQAWS
;; for alpha : 1 thru 20 do print(alpha/10.0, float(gamma(alpha/10)),quad_qags(log(1/x)^(alpha/10-1),x,0,1));
;;
;; for alpha : 1 thru 20 do print(alpha/10.0, float(gamma(alpha/10)),quad_qaws(if equal(x,1) then 1 else (log(1/x)/(1-x))^(alpha/10-1),x,0,1,0,alpha/10-1,1));


;; Test 12
;; integrate(exp(20*(x-1))*sin(2^alpha*x), x, 0, 1) 
;; => (20*sin(2^alpha) - 2^alpha*cos(2^alpha) + 2^alpha*exp(-20))/(400 + 4^alpha)
;;
;; alpha = 0(1)9
;;
;; DQAG (key = 6), DQAWO
;; Failures:
;; None
;;
;; for alpha : 0 thru 9 do print(alpha, float((20*sin(2^alpha) - 2^alpha*cos(2^alpha) + 2^alpha*exp(-20))/(400 + 4^alpha)), quad_qag(exp(20*(x-1))*sin(2^alpha*x), x, 0, 1, 6));
;;
;; for alpha : 0 thru 9 do print(alpha, float((20*sin(2^alpha) - 2^alpha*cos(2^alpha) + 2^alpha*exp(-20))/(400 + 4^alpha)), quad_qawo(exp(20*(x-1)), x, 0, 1, 2^alpha, 'sin));
;;

;; Test 13
;; integrate((x*(1-x))^(-1/2)*cos(2^alpha*x), x, 0, 1)
;; => cos(2^(alpha-1))*J0(2^(alpha - 1))
;;
;; alpha = 0(1)8
;;
;; DQAGS, DQAWO, DQAWS
;;
;; Failures:
;; DQAGS: alpha = 4 (ier = 5)
;;
;; for alpha : 0 thru 8 do print(alpha, float(cos(2^(alpha-1))*bessel_j(0,2^(alpha - 1))), quad_qags((x*(1-x))^(-1/2)*cos(2^alpha*x), x, 0, 1));
;;
;; This doesn't work:
;; for alpha : 0 thru 8 do print(alpha, float(cos(2^(alpha-1))*bessel_j(0,2^(alpha - 1))), quad_qawo(if equal(x,0) or equal(x,1) then 0 else (x*(1-x))^(-1/2), x, 0, 1, 2^alpha, 'cos));
;;
;; for alpha : 0 thru 8 do print(alpha, float(cos(2^(alpha-1))*bessel_j(0,2^(alpha - 1))), quad_qaws(cos(2^alpha*x), x, 0, 1, -1/2, -1/2, 1));

;; Test 14
;;
;; integrate(x^(-1/2)*exp(-2^(-alpha)*x) * cos(x), x, 0, inf);
;; => sqrt(%pi)*(1-4^(-alpha))^(-1/4)*cos(atan(2^alpha)/2)
;;
;; quad_qawf(x^(-1/2)*exp(-2^(-2)*x), x, 0, 1, cos)
;; quad_qawf(x^(-1/2)*exp(-2^(-2)*x), x, 1e-8, 1, cos)
;; quad_qawo(x^(-1/2)*exp(-2^(-2)*x), x, 1e-8, 20*2^2, 1, cos)
;;

;; Test 15
;;
;; integrate(x^2*exp(-2^(-alpha)*x), x, 0, inf)
;; => 2^(3*alpha + 1)
;;
;; alpha = 0(1)5
;;
;; quad_qagi(x^2*exp(-2^(-alpha)*x), x, 0, inf)
;;
;; for alpha : 0 thru 5 do print(alpha, float(2^(3*alpha+1)), quad_qagi(x^2*exp(-2^(-alpha)*x), x, 0, inf));
;;

;; Test 16
;; integrate(x^(alpha - 1)/(1+10*x)^2, 0, inf)
;; => 10^(-alpha)*(1-alpha)*pi/sin(pi*alpha)
;; if alpha /= 1.  Otherwise result = 1/10 when alpha = 1.
;;
;; alpha = 0.1(0.1)1.9
;;
;; DQAGI
;;
;; Failures: None
;;
;; for alpha : 1 thru 19 do print(alpha/10.0, float(10^(-alpha/10)*(1-alpha/10)*%pi/sin(%pi*alpha/10)), quad_qagi(x^(alpha/10 - 1)/(1+10*x)^2, x, 0, inf));

;; Test 17
;;
;; Cauchy principal value of
;;
;; integrate(2^(-alpha)*(((x-1)^2 + 4^(-alpha))*(x-2))^(-1), x, 0, 5);
;; => (2^(-alpha)*ln(3/2) - 2^(-alpha-1)*ln((16 + 4^(-alpha))/(1+4^(-alpha))) - atan(2^(alpha +  2)) - atan(2^alpha))/(1 + 4^(-alpha))
;;
;; alpha = 0(1)10
;;
;; quad_qawc(2^(-alpha)*((x-1)^2 + 4^(-alpha))^(-1), 2, 0, 5)
;;
;; for alpha : 0 thru 10 do print(alpha, float((2^(-alpha)*log(3/2) - 2^(-alpha-1)*log((16 + 4^(-alpha))/(1+4^(-alpha))) - atan(2^(alpha +  2)) - atan(2^alpha))/(1 + 4^(-alpha))), quad_qawc(2^(-alpha)*((x-1)^2 + 4^(-alpha))^(-1), x, 2, 0, 5));
