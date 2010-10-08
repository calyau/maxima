;;; -*- Mode: lisp -*-

;;; Simple Maxima interface to minpack routines

(in-package #-gcl #:maxima #+gcl "MAXIMA")

;; Variable that will hold the function that calculates the function
;; value and the constraints.
(defvar *calcfc*)

(in-package #-gcl #:cobyla #+gcl "COBYLA")

;; COBYLA always calls CALCFC to compute the function value and the
;; constraint equations.  But we want to be able to specify different
;; versions.  So, COBYLA calls CALCFC, which then calls *CALCFC* to
;; do the real compuation.
(defun calcfc (n m x f con)
  (declare (ignore f))
  (funcall maxima::*calcfc* n m x con))

(in-package #-gcl #:maxima #+gcl "MAXIMA")

(defun %cobyla (vars init-x f conlist
		 &key (rhobeg .5d0) (rhoend 1d-6) (iprint 0) (maxfun 1000))
  (let* ((n (length (cdr vars)))
	 (m (length (cdr conlist)))
	 (x (make-array n :element-type 'double-float
			:initial-contents (mapcar #'(lambda (z)
						      (let ((r ($float z)))
							(if (floatp r)
							    r
							    (merror "Does not evaluate to a float:  ~M"
								    z))))
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
	 (cv (coerce-float-fun conlist vars))
	 (*calcfc* #'(lambda (nn mm xval cval)
		       (declare (fixnum nn mm)
				(type (cl:array double-float (*)) xval cval))
		       (let* ((x-list (coerce xval 'list))
			      (f (apply fv x-list))
			      (c (apply cv x-list)))
			 ;; Do we really need these checks?
			 (unless (floatp f)
			   (merror "The objective function did not evaluate to a number at ~M"
				   (list '(mlist) x-list)))
			 (unless (every #'floatp (cdr c))
			   (merror "The constraints did not evaluate to a number at ~M"
				   (list '(mlist) x-list)))
			 (replace cval c :start2 1)
			 (values nn mm nil
				 f nil)))))
    (multiple-value-bind (null-0 null-1 null-2 null-3 null-4 null-5 neval null-6 null-7)
	(cobyla:cobyla n m x rhobeg rhoend iprint maxfun w iact)
      (declare (ignore null-0 null-1 null-2 null-3 null-4 null-5 null-6 null-7))
      (let ((x-list (coerce x 'list)))
	(values (apply fv x-list)
		x
		(apply cv x-list)
		neval)))))

;; Interface.  See fmin_cobyla.mac for documentation.
(defun $fmin_cobyla (f vars conlist init-x &rest options)
  (let* ((args (lispify-maxima-keyword-options options '($rhobeg $rhoend $iprint $maxfun))))
    (multiple-value-bind (fmin xopt copt neval)
	(apply #'%cobyla vars init-x f conlist args)
      (list '(mlist)
	    (list* '(mlist) (mapcar #'(lambda (var val)
					`((mequal) ,var ,val))
				    (cdr vars)
				    (coerce xopt 'list)))
	    fmin
	    copt
	    neval))))
