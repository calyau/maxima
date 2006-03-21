;;; Maxima interface to quadpack integration

(in-package :maxima)

(defun $quad_qag (fun var a b key &optional (epsrel 1d-8) (limit 200))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun fun `((mlist) ,var)))))
    (multiple-value-bind (junk z-a z-b z-epsabs z-epsrel z-key result abserr neval ier
			       z-limit z-lenw last)
	(slatec:dqag #'(lambda (x)
			  (float (funcall f x) 1d0))
		      ($float a)
		      ($float b)
		      0d0
		      ($float epsrel) key
		      0d0 0d0 0 0
		      limit lenw 0 iwork work)
      (declare (ignore junk z-a z-b z-epsabs z-epsrel z-key z-limit z-lenw last))
      (list '(mlist) result abserr neval ier))))
  
(defun $quad_qags (fun var a b &optional (epsrel 1d-8) (limit 200))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun fun `((mlist) ,var)))))
    (multiple-value-bind (junk z-a z-b z-epsabs z-epsrel result abserr neval ier
			       z-limit z-lenw last)
	(slatec:dqags #'(lambda (x)
			   (float (funcall f x) 1d0))
		       ($float a)
		       ($float b)
		       0d0
		       ($float epsrel)
		       0d0 0d0 0 0
		       limit lenw 0 iwork work)
      (declare (ignore junk z-a z-b z-epsabs z-epsrel z-limit z-lenw last))
      (list '(mlist) result abserr neval ier))))

(defun $quad_qagi (fun var bound inf-type  &optional (epsrel 1d-8) (limit 200))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun fun `((mlist) ,var))))
	 (infinity (case inf-type
		     ((1 $inf)
		      ;; Interval is [bound, infinity]
		      1)
		     ((-1 $minf)
		      ;; Interval is [-infinity, bound]
		      -1)
		     ((2 $both)
		      ;; Interval is [-infinity, infinity]
		      2))))
    (multiple-value-bind (junk z-bound z-inf z-epsabs z-epsrel result abserr neval ier
			       z-limit z-lenw last)
	(slatec:dqagi #'(lambda (x)
			   (float (funcall f x) 1d0))
		       ($float bound)
		       infinity
		       0d0
		       ($float epsrel)
		       0d0 0d0 0 0
		       limit lenw 0 iwork work)
      (declare (ignore junk z-bound z-inf z-epsabs z-epsrel z-limit z-lenw last))
      (list '(mlist) result abserr neval ier))))

(defun $quad_qawc (fun var c a b &optional (epsrel 1d-8) (limit 200))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun fun `((mlist) ,var)))))
    (multiple-value-bind (junk z-a z-b z-c z-epsabs z-epsrel result abserr neval ier
			       z-limit z-lenw last)
	(slatec:dqawc #'(lambda (x)
			   (float (funcall f x) 1d0))
		       ($float a)
		       ($float b)
		       ($float c)
		       0d0
		       ($float epsrel)
		       0d0 0d0 0 0
		       limit lenw 0 iwork work)
      (declare (ignore junk z-a z-b z-c z-epsabs z-epsrel z-limit z-lenw last))
      (list '(mlist) result abserr neval ier))))

(defun $quad_qawf (fun var a omega trig &optional (epsabs 1d-10) (limit 200)
		       (maxp1 100) (limlst 10))
  (let* ((leniw limit)
	 (lenw (+ (* 2 leniw) (* 25 maxp1)))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array leniw :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun fun `((mlist) ,var))))
	 (integr (ecase trig
		   ((1 %cos $cos) 1)
		   ((2 %sin $sin) 2))))
    (multiple-value-bind (junk z-a z-omega z-integr epsabs result abserr neval ier
			       z-limlst z-lst
			       z-leniw z-maxp1 z-lenw)
	(slatec:dqawf #'(lambda (x)
			   (float (funcall f x) 1d0))
		       ($float a)
		       ($float omega)
		       integr
		       ($float epsabs)
		       0d0 0d0 0 0
		       limlst 0 leniw maxp1 lenw iwork work)
      (declare (ignore junk z-a z-omega z-integr epsabs z-limlst z-lst
		       z-leniw z-maxp1 z-lenw))
      (list '(mlist) result abserr neval ier))))

(defun $quad_qawo (fun var a b omega trig &optional (epsrel 1d-10) (limit 200)
		       (maxp1 100))
  (let* ((leniw limit)
	 (lenw (+ (* 2 leniw) (* 25 maxp1)))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array leniw :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun fun `((mlist) ,var))))
	 (integr (ecase trig
		   ((1 %cos $cos) 1)
		   ((2 %sin $sin) 2))))
    (multiple-value-bind (junk z-a z-b z-omega z-integr z-epsabs z-epsrel
			       result abserr neval ier
			       z-leniw z-maxp1 z-lenw z-lst)
	(slatec:dqawo #'(lambda (x)
			   (float (funcall f x) 1d0))
		       ($float a)
		       ($float b)
		       ($float omega)
		       integr
		       0d0
		       ($float epsrel)
		       0d0 0d0 0 0
		       leniw maxp1 lenw 0 iwork work)
      (declare (ignore junk z-a z-b z-omega z-integr z-epsabs z-epsrel
		       z-lst z-leniw z-maxp1 z-lenw))
      (list '(mlist) result abserr neval ier))))

(defun $quad_qaws (fun var a b alfa beta wfun &optional (epsrel 1d-10) (limit 200))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun fun `((mlist) ,var)))))
    (multiple-value-bind (junk z-a z-b z-alfa z-beta z-int z-epsabs z-epsrel
			       result abserr neval ier
			       z-limit z-lenw last)
	(slatec:dqaws #'(lambda (x)
			   (float (funcall f x) 1d0))
		       ($float a)
		       ($float b)
		       ($float alfa)
		       ($float beta)
		       wfun
		       0d0
		       ($float epsrel)
		       0d0 0d0 0 0
		       limit lenw 0 iwork work)
      (declare (ignore junk z-a z-b z-alfa z-beta z-int z-epsabs z-epsrel
		       z-limit z-lenw last))
      (list '(mlist) result abserr neval ier))))


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
;; Failures:
;; DQNG: alpha >= 7 (ier = 1)

;; Test 7
;; integrate(|x - 1/3|^alpha, x, 0, 1)
;; => ((2/3)^(alpha + 1) + (1/3)^(alpha + 1))/(alpha + 1)
;;
;; alpha = -0.8(0.1)2.1
;; DQAGS, DQAGP (point of singularity supplied)
;;
;; No failures.

;; Test 8
;; integrate(abs(x - pi/4)^alpha, x, 0, 1)
;; => ((1-pi/4)^(alpha+1) + (pi/4)^(alpha + 1))/(alpha + 1)
;;
;; alpha = -0.8(0.1)2.1
;;
;; DQAGS, DQAGP
;;
;; Failures:
;; DQAGS: alpha <= -0.5 (ier = 3)

;; Test 9
;;
;; integrate((1-x*x)^(-1/2)/(x+1+2^(-alpha)),x, -1, 1)
;; => %pi*((1+2^(-alpha))^2-1)^(-1/2)
;;
;; alpha = 1(1)20
;;
;; quad_qaws(1/(x+1+2^(-4)), x, -1, 1, -0.5, -0.5, 1)

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

;; Test 11
;; integrate((log(1/x))^(alpha-1), x, 0, 1) =
;; integrate((1-x)^(alpha - 1)*(log(1/x)/(1-x))^(alpha-1), x, 0, 1)
;; => Gamma(alpha)
;;
;; alpha = 0.1(0.1)2
;;
;; DQAGS, DQAWS

;; Test 12
;; integrate(exp(20*(x-1))*sin(2^alpha*x), x, 0, 1) 
;; => (20*sin(2^alpha) - 2^alpha*cos(2^alpha) + 2^alpha*exp(-20))/(400 + 4^alpha)
;;
;; alpha = 0(1)9
;;
;; DQAG (key = 6), DQAWO
;;
;; Failures:
;; None

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


;; Test 14
;;
;; integrate(x^(-1/2)*exp(-2^(-alpha)*x) * cos(x), x, 0, inf);
;; => sqrt(%pi)*(1-4^(-alpha))^(-1/4)*cos(atan(2^alpha)/2)
;;
;; quad_qawf(x^(-1/2)*exp(-2^(-2)*x), x, 0, 1, cos)
;; quad_qawf(x^(-1/2)*exp(-2^(-2)*x), x, 1d-8, 1, cos)
;; quad_qawo(x^(-1/2)*exp(-2^(-2)*x), x, 1d-8, 20*2^2, 1, cos)
;;

;; Test 15
;;
;; integrate(x^2*exp(-2^(-alpha)*x), x, 0, inf)
;; => 2^(3*alpha + 1)
;;
;; alpha = 0(1)5
;;
;; quad_qagi(x^2*exp(-2^(-alpha)*x), x, 0, inf)
;; Test 16
;; integral 0 to infinity x^(alpha - 1)/(1+10*x)^2 =
;; 10^(-alpha)*(1-alpha)*pi/sin(pi*alpha)
;; if alpha /= 1.  Otherwise result = 1/10 when alpha = 1.
;;
;; alpha = 0.1(0.1)1.9
;;
;; DQAGI
;;
;; Failures: None

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
