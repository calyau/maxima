;;; Maxima interface to quadpack integration

(in-package :maxima)

(defun $quad_qag (fun var a b key &optional (epsrel 1d-8) (limit 200))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun `((mlist) ,fun)
			      `((mlist) ,var)))))
    (multiple-value-bind (junk z-a z-b z-epsabs z-epsrel z-key result abserr neval ier z-limit z-lenw last)
	(slatec::dqag #'(lambda (x)
			    (float (second (funcall f x)) 1d0))
			(float a) (float b) 0d0 (float epsrel) key
			0d0 0d0 0 0
			limit lenw 0 iwork work)
      (declare (ignore junk z-a z-b z-epsabs z-epsrel z-key z-limit z-lenw))
      (list '(mlist) result abserr neval ier))))
  
(defun $quad_qags (fun var a b &optional (epsrel 1d-8) (limit 200))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun `((mlist) ,fun)
			      `((mlist) ,var)))))
    (multiple-value-bind (junk z-a z-b z-epsabs z-epsrel result abserr neval ier z-limit z-lenw last)
	(slatec::dqags #'(lambda (x)
			    (float (second (funcall f x)) 1d0))
			(float a) (float b) 0d0 (float epsrel)
			0d0 0d0 0 0
			limit lenw 0 iwork work)
      (declare (ignore junk z-a z-b z-epsabs z-epsrel z-limit z-lenw))
      (list '(mlist) result abserr neval ier))))

(defun $quad_qagi (fun var bound inf-type  &optional (epsrel 1d-8) (limit 200))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun `((mlist) ,fun)
					   `((mlist) ,var))))
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
    (multiple-value-bind (junk z-bound z-inf z-epsabs z-epsrel result abserr neval ier z-limit z-lenw last)
	(slatec::dqagi #'(lambda (x)
			   (float (second (funcall f x)) 1d0))
			(float bound) infinity 0d0 (float epsrel)
			0d0 0d0 0 0
			limit lenw 0 iwork work)
      (declare (ignore junk z-a z-b z-epsabs z-epsrel z-limit z-lenw))
      (list '(mlist) result abserr neval ier))))

(defun $quad_qawc (fun var c a b &optional (epsrel 1d-8) (limit 200))
  (let* ((lenw (* 4 limit))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array limit :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun `((mlist) ,fun)
					   `((mlist) ,var)))))
    (multiple-value-bind (junk z-a z-b z-c z-epsabs z-epsrel result abserr neval ier z-limit z-lenw last)
	(slatec::dqawc #'(lambda (x)
			   (float (second (funcall f x)) 1d0))
		       (float a) (float b) (float c)
		       0d0 (float epsrel)
		       0d0 0d0 0 0
		       limit lenw 0 iwork work)
      (declare (ignore junk z-a z-b z-epsabs z-epsrel z-limit z-lenw))
      (list '(mlist) result abserr neval ier))))

(defun $quad_qawf (fun var a omega trig &optional (epsabs 1d-10) (limit 200) (maxp1 100) (limlst 10))
  (let* ((leniw limit)
	 (lenw (+ (* 2 leniw) (* 25 maxp1)))
	 (work (make-array lenw :element-type 'double-float))
	 (iwork (make-array leniw :element-type 'f2cl-lib:integer4))
	 (f (compile nil (coerce-float-fun `((mlist) ,fun)
					   `((mlist) ,var))))
	 (integr (ecase trig
		   ((1 %cos $cos) 1)
		   ((2 %sin $sin) 2))))
    (multiple-value-bind (junk z-a z-omega z-integr epsabs result abserr neval ier z-limlst z-lst
			       z-leniw z-maxp1 z-lenw)
	(slatec::dqawf #'(lambda (x)
			   (float (second (funcall f x)) 1d0))
		       (float a) (float omega) integr
		       (float epsabs)
		       0d0 0d0 0 0
		       limlst 0 leniw maxp1 lenw iwork work)
      (declare (ignore junk z-a z-omega z-integr z-limlst z-lst z-leniw z-maxp1 z-lenw))
      (list '(mlist) result abserr neval ier))))

