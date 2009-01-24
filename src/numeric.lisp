;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10-*- ;;;;

;;; This package contains a numeric class for use with Maxima.  The
;;; purpose is to allow users to write numerical algorithms that
;;; support double-float, (complex double-float) and Maxima bfloat and
;;; complex bfloat arithmetic, without having to write separate
;;; versions for each.  Of course, specially written versions will be
;;; faster, but this allows users to write bigfloat routines in a more
;;; "natural" Lisp style.

(in-package #-gcl #:bigfloat #+gcl "BIGFLOAT")

(defun intofp (re)
  ;; Kind of like Maxima's INTOFP, but we only handle numeric types.
  ;; We should return a list of mantissa and exponent.
  (cond ((floatp re)
	 (maxima::floattofp re))
	((eql re 0)
	 '(0 0))
	((integerp re)
	 (list (maxima::fpround re) (cl:+ maxima::*m maxima::fpprec)))
	((typep re 'ratio)
	 ;; Should we do something better than converting the
	 ;; numerator and denominator to floats and dividing?
	 (maxima::fpquotient (intofp (numerator re))
			     (intofp (denominator re))))
	((maxima::$bfloatp re)
	 (cdr re))
	(t
	 (error "Don't know how to convert ~S to a BIGFLOAT" re))))

(defclass numeric ()
  ()
  (:documentation "Basic class, like CL's NUMBER type"))

(defclass bigfloat (numeric)
  ((real :initform (intofp 0)
	 :reader real-value
	 :initarg :real
	 :documentation "A Maxima bigfloat.  This contains just the mantissa and exponent of
                         a bigfloat.  The bigfloat marker is not stored." ))
  (:documentation "Big float, roughly equivalent to a Maxima bfloat object"))

(defclass complex-bigfloat (numeric)
  ;; Currently, the real and imaginary parts contain a list of the
  ;; mantissa and exponent.  Should they be bigfloat objects instead?
  ((real :initform (intofp 0)
	 :reader real-value
	 :initarg :real
	 :documentation "Real part of a complex bigfloat")
   (imag :initform (intofp 0)
	 :reader imag-value
	 :initarg :imag
	 :documentation "Imaginary part of a complex bigfloat"))
  (:documentation "Complex bigfloat"))

(defmethod make-load-form ((x bigfloat) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-of x)
		  :real ',(real-value x)))
;;; BIGFLOAT - External
;;;
;;;    BIGFLOAT converts a number to a BIGFLOAT or COMPLEX-BIGFLOAT.
;;; This is intended to convert CL numbers or Maxima (internal)
;;; numbers to an bigfloat object.
(defun bigfloat (re &optional im)
  "Convert RE to a BIGFLOAT.  If IM is given, return a COMPLEX-BIGFLOAT"
  (cond (im
	 (make-instance 'complex-bigfloat
			:real (intofp re)
			:imag (intofp im)))
	((cl:realp re)
	 (make-instance 'bigfloat :real (intofp re)))
	((maxima::$bfloatp re)
	 (make-instance 'bigfloat :real (intofp re)))
	((maxima::complex-number-p re 'maxima::bigfloat-or-number-p)
	 (make-instance 'complex-bigfloat
			:real (intofp (maxima::$realpart re))
			:imag (intofp (maxima::$imagpart re))))
	(t
	 (make-instance 'bigfloat :real (intofp re)))))


;;; MAXIMA::TO - External
;;;
;;;    Convert a CL number, a BIGFLOAT, or a COMPLEX-BIGFLOAT to
;;; Maxima's internal representation of the number.
(defmethod maxima::to ((z float))
  z)

(defmethod maxima::to ((z rational))
  (if (typep z 'ratio)
      (maxima::div (numerator z) (denominator z))
      z))

(defmethod maxima::to ((z cl:complex))
  (maxima::add (cl:realpart z)
	       (maxima::mul 'maxima::$%i
			    (cl:imagpart z))))

(defmethod maxima::to ((z bigfloat))
  "Convert BIGFLOAT  object to a maxima number"
  (maxima::bcons (real-value z)))

(defmethod maxima::to ((z complex-bigfloat))
  "Convert COMPLEX-BIGFLOAT  object to a maxima number"
  (maxima::add (maxima::bcons (real-value z))
	       (maxima::mul 'maxima::$%i
			    (maxima::bcons (imag-value z)))))
  

(defmethod make-load-form ((x complex-bigfloat) &optional environment)
  (declare (ignore environment))
  `(make-instance ',(class-of x)
		  :real ',(real-value x)
		  :imag ',(imag-value x)))

;; The print-object and describe-object methods are mostly for
;; debugging purposes.  Maxima itself shouldn't ever see such objects.
(defmethod print-object ((x bigfloat) stream)
  (let ((r (real-value x)))
    (multiple-value-bind (sign output-list)
	(if (cl:minusp (first r))
	    (values "-" (maxima::fpformat (maxima::bcons (list (cl:- (first r)) (second r)))))
	    (values "+" (maxima::fpformat (maxima::bcons r))))
      (format stream "~A~{~D~}" sign output-list))))

(defmethod print-object ((x complex-bigfloat) stream)
  (format stream "~A~A*%i" (realpart x) (imagpart x)))

(defmethod describe-object ((x bigfloat) stream)
  (let ((r (real-value x)))
    (format stream "~&~S is a BIGFLOAT with mantissa ~D and exponent ~D~%"
	    x (first r) (second r))))

(defmethod describe-object ((x complex-bigfloat) stream)
  (format stream "~S is a COMPLEX-BIGFLOAT~%" x)
  (describe-object (realpart x) stream)
  (describe-object (imagpart x) stream))


(defgeneric add1 (a)
  (:documentation "Add 1"))

(defgeneric sub1 (a)
  (:documentation "Subtract 1"))


(defgeneric two-arg-+ (a b)
  (:documentation "A + B"))

(defgeneric two-arg-- (a b)
  (:documentation "A - B"))

(defgeneric two-arg-* (a b)
  (:documentation "A * B"))

(defgeneric two-arg-/ (a b)
  (:documentation "A / B"))

(defgeneric two-arg-< (a b)
  (:documentation "A < B"))

(defgeneric two-arg-> (a b)
  (:documentation "A > B"))

(defgeneric two-arg-<= (a b)
  (:documentation "A <= B"))

(defgeneric two-arg->= (a b)
  (:documentation "A >= B"))

(defgeneric two-arg-= (a b)
  (:documentation "A = B?"))


(defgeneric unary-minus (a)
  (:documentation "-A"))

(defgeneric unary-divide (a)
  (:documentation "1 / A"))


;;; Basic arithmetic operations

;;; 1+ and 1-

(defmethod add1 ((a number))
  (cl::1+ a))

(defmethod add1 ((a bigfloat))
  (make-instance 'bigfloat :real (maxima::fpplus (real-value a) (maxima::fpone))))

(defmethod add1 ((a complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpplus (real-value a) (maxima::fpone))
		 :imag (imag-value a)))

(defmethod sub1 ((a number))
  (cl::1- a))

(defmethod sub1 ((a bigfloat))
  (make-instance 'bigfloat :real (maxima::fpdifference (real-value a) (maxima::fpone))))

(defmethod sub1 ((a complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpdifference (real-value a) (maxima::fpone))
		 :imag (imag-value a)))

(declaim (inline 1+ 1-))

(defun 1+ (x)
  (add1 x))

(defun 1- (x)
  (sub1 x))

;; Add two numbers
(defmethod two-arg-+ ((a number) (b number))
  (cl:+ a b))

(defmethod two-arg-+ ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpplus (real-value a) (real-value b))))

(defmethod two-arg-+ ((a complex-bigfloat) (b complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpplus (real-value a) (real-value b))
		 :imag (maxima::fpplus (imag-value a) (imag-value b))))

;; Handle contagion for two-arg-+
(defmethod two-arg-+ ((a bigfloat) (b float))
  (make-instance 'bigfloat :real (maxima::fpplus (real-value a) (intofp b))))

(defmethod two-arg-+ ((a bigfloat) (b rational))
  (make-instance 'bigfloat :real (maxima::fpplus (real-value a) (intofp b))))

(defmethod two-arg-+ ((a float) (b bigfloat))
  (two-arg-+ b a))

(defmethod two-arg-+ ((a rational) (b bigfloat))
  (two-arg-+ b a))

(defmethod two-arg-+ ((a complex-bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpplus (real-value a) (real-value b))
		 :imag (imag-value a)))

(defmethod two-arg-+ ((a complex-bigfloat) (b number))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpplus (real-value a) (intofp (cl:realpart b)))
		 :imag (maxima::fpplus (imag-value a) (intofp (cl:imagpart b)))))

(defmethod two-arg-+ ((a bigfloat) (b complex-bigfloat))
  (two-arg-+ b a))

(defmethod two-arg-+ ((a number) (b complex-bigfloat))
  (two-arg-+ b a))

(defun + (&rest args)
  (if (null args)
      0
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		(two-arg-+ res (car args))))
	  ((null args) res))))

;; Negate a number
(defmethod unary-minus ((a number))
  (cl:- a))

(defmethod unary-minus ((a bigfloat))
  (make-instance 'bigfloat :real (maxima::fpminus (real-value a))))

(defmethod unary-minus ((a complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpminus (real-value a))
		 :imag (maxima::fpminus (imag-value a))))

;;; Subtract two numbers
(defmethod two-arg-- ((a number) (b number))
  (cl:- a b))

(defmethod two-arg-- ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpdifference (real-value a) (real-value b))))

(defmethod two-arg-- ((a complex-bigfloat) (b complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpdifference (real-value a) (real-value b))
		 :imag (maxima::fpdifference (imag-value a) (imag-value b))))

;; Handle contagion for two-arg--
(defmethod two-arg-- ((a bigfloat) (b float))
  (make-instance 'bigfloat :real (maxima::fpdifference (real-value a) (intofp b))))

(defmethod two-arg-- ((a bigfloat) (b rational))
  (make-instance 'bigfloat :real (maxima::fpdifference (real-value a) (intofp b))))

(defmethod two-arg-- ((a float) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpdifference (intofp a) (real-value b))))

(defmethod two-arg-- ((a rational) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpdifference (intofp a) (real-value b))))

(defmethod two-arg-- ((a complex-bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpdifference (real-value a) (real-value b))
		 :imag (imag-value a)))

(defmethod two-arg-- ((a complex-bigfloat) (b number))
  (if (cl:complexp b)
      (two-arg-- a (bigfloat (cl:realpart b) (cl:imagpart b)))
      (two-arg-- a (bigfloat b))))

(defmethod two-arg-- ((a bigfloat) (b complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpdifference (real-value a) (real-value b))
		 :imag (maxima::fpminus (imag-value b))))

(defmethod two-arg-- ((a number) (b complex-bigfloat))
  (if (cl:complexp a)
      (two-arg-- (make-instance 'complex-bigfloat :real (cl:realpart a) :imag (cl:imagpart a))
		 b)
      (two-arg-- (bigfloat a) b)))

(defun - (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (two-arg-- result (car nlist))))
      (unary-minus number)))

;;; Multiply two numbers
(defmethod two-arg-* ((a number) (b number))
  (cl:* a b))

(defmethod two-arg-* ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fptimes* (real-value a) (real-value b))))

(defmethod two-arg-* ((a complex-bigfloat) (b complex-bigfloat))
  (let ((a-re (real-value a))
	(a-im (imag-value a))
	(b-re (real-value b))
	(b-im (imag-value b)))
    (make-instance 'complex-bigfloat
		   :real (maxima::fpdifference (maxima::fptimes* a-re b-re)
					       (maxima::fptimes* a-im b-im))
		   :imag (maxima::fpplus (maxima::fptimes* a-re b-im)
					 (maxima::fptimes* a-im b-re)))))

;; Handle contagion for two-arg-*
(defmethod two-arg-* ((a bigfloat) (b cl:float))
  (make-instance 'bigfloat :real (maxima::fptimes* (real-value a) (intofp b))))

(defmethod two-arg-* ((a bigfloat) (b cl:rational))
  (make-instance 'bigfloat :real (maxima::fptimes* (real-value a) (intofp b))))

(defmethod two-arg-* ((a float) (b bigfloat))
  (two-arg-* b a))

(defmethod two-arg-* ((a rational) (b bigfloat))
  (two-arg-* b a))

(defmethod two-arg-* ((a complex-bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fptimes* (real-value a) (real-value b))
		 :imag (maxima::fptimes* (imag-value a) (real-value b))))

(defmethod two-arg-* ((a complex-bigfloat) (b number))
  (if (cl:complexp b)
      (two-arg-* a (bigfloat (cl:realpart b) (cl:imagpart b)))
      (two-arg-* a (bigfloat b))))

(defmethod two-arg-* ((a bigfloat) (b complex-bigfloat))
  (two-arg-* b a))

(defmethod two-arg-* ((a number) (b complex-bigfloat))
  (two-arg-* b a))

(defun * (&rest args)
  (if (null args)
      1
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		(two-arg-* res (car args))))
	  ((null args) res))))

;;; Reciprocal of a number
(defmethod unary-divide ((a number))
  (cl:/ a))

(defmethod unary-divide ((a bigfloat))
  (make-instance 'bigfloat :real (maxima::fpquotient (maxima::fpone) (real-value a))))

(defmethod unary-divide ((b complex-bigfloat))
  ;; Could just call two-arg-/, but let's optimize this a little
  (let ((a-re (maxima::fpone))
	(b-re (real-value b))
	(b-im (imag-value b)))
    (if (maxima::fpgreaterp (maxima::fpabs b-re) (maxima::fpabs b-im))
	(let* ((r (maxima::fpquotient b-im b-re))
	       (dn (maxima::fpplus b-re (maxima::fptimes* r b-im))))
	  (make-instance 'complex-bigfloat
			 :real (maxima::fpquotient a-re dn)
			 :imag (maxima::fpquotient (maxima::fpminus r)
						   dn)))
	(let* ((r (maxima::fpquotient b-re b-im))
	       (dn (maxima::fpplus b-im (maxima::fptimes* r b-re))))
	  (make-instance 'complex-bigfloat
			 :real (maxima::fpquotient r dn)
			 :imag (maxima::fpquotient (maxima::fpminus a-re)
						   dn))))))
;;; Divide two numbers
(defmethod two-arg-/ ((a number) (b number))
  (cl:/ a b))

(defmethod two-arg-/ ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpquotient (real-value a) (real-value b))))

(defmethod two-arg-/ ((a complex-bigfloat) (b complex-bigfloat))
  (let ((a-re (real-value a))
	(a-im (imag-value a))
	(b-re (real-value b))
	(b-im (imag-value b)))
    (if (maxima::fpgreaterp (maxima::fpabs b-re) (maxima::fpabs b-im))
	(let* ((r (maxima::fpquotient b-im b-re))
	       (dn (maxima::fpplus b-re (maxima::fptimes* r b-im))))
	  (make-instance 'complex-bigfloat
			 :real (maxima::fpquotient (maxima::fpplus a-re
								   (maxima::fptimes* a-im r))
						   dn)
			 :imag (maxima::fpquotient (maxima::fpdifference a-im
									 (maxima::fptimes* a-re r))
						   dn)))
	(let* ((r (maxima::fpquotient b-re b-im))
	       (dn (maxima::fpplus b-im (maxima::fptimes* r b-re))))
	  (make-instance 'complex-bigfloat
			 :real (maxima::fpquotient (maxima::fpplus (maxima::fptimes* a-re r)
								   a-im)
						   dn)
			 :imag (maxima::fpquotient (maxima::fpdifference
						    (maxima::fptimes* a-im r)
						    a-re)
						   dn))))))
;; Handle contagion for two-arg-/
(defmethod two-arg-/ ((a bigfloat) (b cl:float))
  (make-instance 'bigfloat :real (maxima::fpquotient (real-value a) (intofp b))))

(defmethod two-arg-/ ((a bigfloat) (b cl:rational))
  (make-instance 'bigfloat :real (maxima::fpquotient (real-value a) (intofp b))))

(defmethod two-arg-/ ((a cl:float) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpquotient (intofp a)
						     (real-value b))))

(defmethod two-arg-/ ((a cl:rational) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpquotient (intofp a)
						     (real-value b))))


(defmethod two-arg-/ ((a complex-bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpquotient (real-value a) (real-value b))
		 :imag (maxima::fpquotient (imag-value a) (real-value b))))

(defmethod two-arg-/ ((a complex-bigfloat) (b number))
  (if (cl:complexp b)
      (two-arg-/ a (bigfloat (cl:realpart b) (cl:imagpart b)))
      (two-arg-/ a (bigfloat b))))

(defmethod two-arg-/ ((a bigfloat) (b complex-bigfloat))
  (two-arg-/ (make-instance 'complex-bigfloat :real (real-value a))
	     b))

(defmethod two-arg-/ ((a number) (b complex-bigfloat))
  (if (cl:complexp a)
      (two-arg-/ (make-instance 'complex-bigfloat :real (cl:realpart a) :imag (cl:imagpart a))
		 b)
      (two-arg-/ (bigfloat a) b)))


(defun / (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result (two-arg-/ result (car nlist))))
      (unary-divide number)))

;;; Compare against zero (zerop, minusp, plusp)
(macrolet
    ((frob (name)
       (let ((cl-name (intern (symbol-name name) :cl)))
	 `(progn
	    (defmethod ,name ((x float))
	      (,cl-name x))
	    (defmethod ,name ((x rational))
	      (,cl-name x))))))
  (frob plusp)
  (frob minusp))

(defmethod zerop ((x number))
  (cl:zerop x))

(defmethod zerop ((x bigfloat))
  (let ((r (real-value x)))
    (and (zerop (first r))
	 (zerop (second r)))))

(defmethod zerop ((a complex-bigfloat))
  (and (equal (real-value a) '(0 0))
       (equal (imag-value a) '(0 0))))

(defmethod plusp ((x bigfloat))
  (cl:plusp (first (real-value x))))

(defmethod minusp ((x bigfloat))
  (cl:minusp (first (real-value x))))



;;; Equality 
(defmethod two-arg-= ((a number) (b number))
  (cl:= a b))

(defmethod two-arg-= ((a bigfloat) (b bigfloat))
  (equal (real-value a) (real-value b)))

(defmethod two-arg-= ((a complex-bigfloat) (b complex-bigfloat))
  (and (equal (real-value a) (real-value b))
       (equal (imag-value a) (imag-value b))))

;; Handle contagion for two-arg-=.  This needs some work.  CL says
;; floats and rationals are compared by converting the float to a
;; rational before converting.
(defmethod two-arg-= ((a bigfloat) (b number))
  (if (cl:realp b)
      (equal (real-value a) (intofp b))
      nil))

(defmethod two-arg-= ((a number) (b bigfloat))
  (if (cl:realp a)
      (equal (intofp a) (real-value b))
      (and (equal (intofp a) (real-value b))
	   (equal (real-value b) '(0 0)))))

(defmethod two-arg-= ((a complex-bigfloat) (b number))
  (and (two-arg-= (make-instance 'bigfloat :real (real-value a)) (cl:realpart b))
       (two-arg-= (make-instance 'bigfloat :real (imag-value a)) (cl:imagpart b))))

(defmethod two-arg-= ((a number) (b complex-bigfloat))
  (two-arg-= b a))

(defun = (number &rest more-numbers)
  "Returns T if all of its arguments are numerically equal, NIL otherwise."
  (declare (optimize (safety 2))
	   #-gcl (dynamic-extent more-numbers))
  (do ((nlist more-numbers (cdr nlist)))
      ((atom nlist) t)
    (declare (list nlist))
    (if (not (two-arg-= (car nlist) number))
	(return nil))))

(defun /= (number &rest more-numbers)
  "Returns T if no two of its arguments are numerically equal, NIL otherwise."
  (declare (optimize (safety 2))
	   #-gcl (dynamic-extent more-numbers))
  (do* ((head number (car nlist))
	(nlist more-numbers (cdr nlist)))
       ((atom nlist) t)
    (declare (list nlist))
    (unless (do* ((nl nlist (cdr nl)))
		 ((atom nl) t)
	      (declare (list nl))
	      (if (two-arg-= head (car nl))
		  (return nil)))
      (return nil))))

;;; Comparison operations
(macrolet
    ((frob (op)
       (let ((method (intern (concatenate 'string
					  (string '#:two-arg-)
					  (symbol-name op))))
	     (cl-fun (find-symbol (symbol-name op) :cl)))
	 `(progn
	    (defmethod ,method ((a float) (b float))
	      (,cl-fun a b))
	    (defmethod ,method ((a float) (b rational))
	      (,cl-fun a b))
	    (defmethod ,method ((a rational) (b float))
	      (,cl-fun a b))
	    (defmethod ,method ((a rational) (b rational))
	      (,cl-fun a b))
	    (defun ,op (number &rest more-numbers)
	      "Returns T if its arguments are in strictly increasing order, NIL otherwise."
	      (declare (optimize (safety 2))
		       #-gcl (dynamic-extent more-numbers))
	      (do* ((n number (car nlist))
		    (nlist more-numbers (cdr nlist)))
		   ((atom nlist) t)
		(declare (list nlist))
		(if (not (,method n (car nlist))) (return nil))))))))
  (frob <)
  (frob >)
  (frob <=)
  (frob >=))

(defmethod two-arg-< ((x bigfloat) (y bigfloat))
  (maxima::fplessp (real-value x) (real-value y)))

(defmethod two-arg-< ((x bigfloat) (y float))
  (maxima::fplessp (real-value x) (intofp y)))

(defmethod two-arg-< ((x bigfloat) (y rational))
  (maxima::fplessp (real-value x) (intofp y)))

(defmethod two-arg-> ((x bigfloat) (y bigfloat))
  (maxima::fpgreaterp (real-value x) (real-value y)))

(defmethod two-arg-> ((x bigfloat) (y float))
  (maxima::fpgreaterp (real-value x) (intofp y)))

(defmethod two-arg-> ((x bigfloat) (y rational))
  (maxima::fpgreaterp (real-value x) (intofp y)))

(defmethod two-arg-<= ((x bigfloat) (y bigfloat))
  (or (equal (real-value x) (real-value y))
      (maxima::fplessp (real-value x) (real-value y))))

(defmethod two-arg-<= ((x bigfloat) (y float))
  (or (equal (real-value x) (intofp y))
      (maxima::fplessp (real-value x) (intofp y))))

(defmethod two-arg-<= ((x bigfloat) (y rational))
  (or (equal (real-value x) (intofp y))
      (maxima::fplessp (real-value x) (intofp y))))

(defmethod two-arg->= ((x bigfloat) (y bigfloat))
  (or (equal (real-value x) (real-value y))
      (maxima::fpgreaterp (real-value x) (real-value y))))

(defmethod two-arg->= ((x bigfloat) (y float))
  (or (equal (real-value x) (intofp y))
      (maxima::fpgreaterp (real-value x) (intofp y))))

(defmethod two-arg->= ((x bigfloat) (y rational))
  (or (equal (real-value x) (intofp y))
      (maxima::fpgreaterp (real-value x) (intofp y))))

;; Need to define incf and decf to call our generic +/- methods.
(defmacro incf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number. This number is
  incremented by the second argument, DELTA, which defaults to 1."
  (multiple-value-bind (dummies vals newval setter getter)
      (#-gcl get-setf-expansion #+gcl get-setf-method
	     place env)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
              (,(car newval) (+ ,getter ,d)))
         ,setter))))

(defmacro decf (place &optional (delta 1) &environment env)
  "The first argument is some location holding a number. This number is
  decremented by the second argument, DELTA, which defaults to 1."
  (multiple-value-bind (dummies vals newval setter getter)
      (#-gcl get-setf-expansion #+gcl get-setf-method
	     place env)
    (let ((d (gensym)))
      `(let* (,@(mapcar #'list dummies vals)
              (,d ,delta)
              (,(car newval) (- ,getter ,d)))
         ,setter))))

    

;;; Special functions for real-valued arguments
(macrolet
    ((frob (name)
       (let ((cl-name (intern (symbol-name name) :cl)))
	 `(progn
	    (defmethod ,name ((x number))
	      (,cl-name x))))))
  (frob sqrt)
  (frob abs)
  (frob exp)
  (frob sin)
  (frob cos)
  (frob tan)
  (frob asin)
  (frob acos)
  (frob sinh)
  (frob cosh)
  (frob tanh)
  (frob asinh)
  (frob acosh)
  (frob atanh)
  )

(defmethod abs ((x bigfloat))
  (make-instance 'bigfloat :real (maxima::fpabs (real-value x))))

(defmethod abs ((z complex-bigfloat))
  (let ((x (make-instance 'bigfloat :real (real-value z)))
	(y (make-instance 'bigfloat :real (imag-value z))))
    ;; Bigfloats don't overflow, so we don't need anything special.
    (sqrt (+ (* x x) (* y y)))))

(defmethod exp ((x bigfloat))
  (make-instance 'bigfloat :real (maxima::fpexp (real-value x))))

(defmethod sin ((x bigfloat))
  (make-instance 'bigfloat :real (maxima::fpsin (real-value x) t)))

(defmethod cos ((x bigfloat))
  (make-instance 'bigfloat :real (maxima::fpsin (real-value x) nil)))

(defmethod tan ((x bigfloat))
  (let ((r (real-value x)))
    (make-instance 'bigfloat
		   :real (maxima::fpquotient (maxima::fpsin r t)
				     (maxima::fpsin r nil)))))

(defmethod asin ((x bigfloat))
  (make-instance 'bigfloat :real (cdr (maxima::fpasin (maxima::bcons (real-value x))))))

(defmethod acos ((x bigfloat))
  (make-instance 'bigfloat :real (cdr (maxima::fpacos (maxima::bcons (real-value x))))))


(defmethod sqrt ((x bigfloat))
  (if (minusp x)
      (make-instance 'complex-bigfloat
		     :real (intofp 0)
		     :imag (maxima::fproot (maxima::bcons (maxima::fpabs (real-value x))) 2))
      (make-instance 'bigfloat :real (maxima::fproot (maxima::bcons (real-value x)) 2))))

(defmethod sqrt ((z complex-bigfloat))
  (multiple-value-bind (rx ry)
      (maxima::complex-sqrt (maxima::bcons (real-value z))
			    (maxima::bcons (imag-value z)))
    (make-instance 'complex-bigfloat :real rx :imag ry)))

(defmethod one-arg-log ((a number))
  (cl:log a))

(defmethod one-arg-log ((a bigfloat))
  (if (minusp a)
      (make-instance 'complex-bigfloat
		     :real (maxima::fplog (maxima::fpabs (real-value a)))
		     :imag (maxima::fppi))
      (make-instance 'bigfloat :real (maxima::fplog (real-value a)))))

(defmethod one-arg-log ((a complex-bigfloat))
  (let ((res (maxima::big-float-log (maxima::bcons (real-value a))
				    (maxima::bcons (imag-value a)))))
    (make-instance 'complex-bigfloat
		   :real (cdr (maxima::$realpart res))
		   :imag (cdr (maxima::$imagpart res)))))

(defmethod two-arg-log ((a number) (b number))
  (cl:log a b))

(defmethod two-arg-log ((a numeric) (b numeric))
  (two-arg-/ (one-arg-log a) (one-arg-log b)))

(defmethod two-arg-log ((a numeric) (b float))
  (two-arg-/ (one-arg-log a) (one-arg-log (intofp b))))

(defmethod two-arg-log ((a numeric) (b rational))
  (two-arg-/ (one-arg-log a) (one-arg-log (intofp b))))

(defmethod two-arg-log ((a float) (b numeric))
  (two-arg-/ (one-arg-log (intofp a)) (one-arg-log b)))

(defmethod two-arg-log ((a rational) (b numeric))
  (two-arg-/ (one-arg-log (intofp a)) (one-arg-log b)))

(defun log (a &optional b)
  (if b
      (two-arg-log a b)
      (one-arg-log a)))

(defmethod sinh ((x bigfloat))
  (let ((r (maxima::bcons (real-value x))))
    (make-instance 'bigfloat :real (cdr (maxima::fpsinh r)))))

(defmethod cosh ((x bigfloat))
  (let ((r (maxima::bcons (real-value x))))
    (make-instance 'bigfloat :real (cdr (maxima::$bfloat `((maxima::%cosh) ,r))))))

(defmethod tanh ((x bigfloat))
  (let ((r (maxima::bcons (real-value x))))
    (make-instance 'bigfloat :real (cdr (maxima::fptanh r)))))

(defmethod asinh ((x bigfloat))
  (let ((r (real-value x)))
    (make-instance 'bigfloat :real (cdr (maxima::fpasinh (maxima::bcons r))))))

(defmethod atanh ((x bigfloat))
  (let ((r (maxima::big-float-atanh (maxima::bcons (real-value x)))))
    (if (maxima::bigfloatp r)
	(make-instance 'bigfloat :real (cdr r))
	(make-instance 'complex-bigfloat
		       :real (cdr (maxima::$realpart r))
		       :imag (cdr (maxima::$imagpart r))))))

(defmethod acosh ((x bigfloat))
  (let* ((r (real-value x))
	 (value (maxima::mevalp `((maxima::%acosh maxima::simp)
				  ,(maxima::bcons r)))))
    (if (maxima::bigfloatp value)
	(make-instance 'bigfloat :real (cdr value))
	(make-instance 'complex-bigfloat
		       :real (cdr (maxima::$realpart value))
		       :imag (cdr (maxima::$imagpart value))))))

;;; Complex arguments

;;; Special functions for complex args
(macrolet
    ((frob (name &optional big-float-op-p)
       (if big-float-op-p
	   (let ((big-op (intern (concatenate 'string
					      (string '#:big-float-)
					      (string name))
				 '#:maxima)))
	     `(defmethod ,name ((a complex-bigfloat))
		(let ((res (,big-op (maxima::bcons (real-value a))
				    (maxima::bcons (imag-value a)))))
		  (make-instance 'complex-bigfloat
				 :real (cdr (maxima::$realpart res))
				 :imag (cdr (maxima::$imagpart res))))))
	   (let ((max-op (intern (concatenate 'string "$" (string name)) '#:maxima)))
	     `(defmethod ,name ((a complex-bigfloat))
		;; We should do something better than calling mevalp
		(let* ((arg (maxima::add (maxima::bcons (real-value a))
					 (maxima::mul 'maxima::$%i (maxima::bcons (imag-value a)))))
		       (result (maxima::mevalp `((,',max-op maxima::simp) ,arg))))
		  (make-instance 'complex-bigfloat
				 :real (cdr (maxima::$realpart result))
				 :imag (cdr (maxima::$imagpart result)))))))))
  (frob exp)
  (frob sin)
  (frob cos)
  (frob tan)
  (frob asin t)
  (frob acos t)
  (frob sinh)
  (frob cosh)
  (frob tanh t)
  (frob asinh t)
  (frob acosh)
  (frob atanh t))

(defmethod one-arg-atan ((a number))
  (cl:atan a))

(defmethod one-arg-atan ((a bigfloat))
  (make-instance 'bigfloat :real (maxima::fpatan (real-value a))))

(defmethod one-arg-atan ((a complex-bigfloat))
  ;; We should do something better than calling mevalp
  (let* ((arg (maxima::add (maxima::bcons (real-value a))
			   (maxima::mul 'maxima::$%i (maxima::bcons (imag-value a)))))
	 (result (maxima::mevalp `((maxima::%atan maxima::simp) ,arg))))
    (make-instance 'complex-bigfloat
		   :real (cdr (maxima::$realpart result))
		   :imag (cdr (maxima::$imagpart result)))))

;; Really want type real, but gcl doesn't like that.
(defmethod two-arg-atan ((a #-gcl real #+gcl number) (b #-gcl real #+gcl number))
  (cl:atan a b))

(defmethod two-arg-atan ((a bigfloat) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpatan2 (real-value a) (real-value b))))

(defmethod two-arg-atan ((a bigfloat) (b float))
  (make-instance 'bigfloat :real (maxima::fpatan2 (real-value a) (intofp b))))

(defmethod two-arg-atan ((a bigfloat) (b rational))
  (make-instance 'bigfloat :real (maxima::fpatan2 (real-value a) (intofp b))))

(defmethod two-arg-atan ((a float) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpatan2 (intofp a) (real-value b))))

(defmethod two-arg-atan ((a rational) (b bigfloat))
  (make-instance 'bigfloat :real (maxima::fpatan2 (intofp a) (real-value b))))

(defun atan (a &optional b)
  (if b
      (two-arg-atan a b)
      (one-arg-atan a)))
      
(defmethod scale-float ((a float) (n integer))
  (cl:scale-float a n))

(defmethod scale-float ((a bigfloat) (n integer))
  (if (cl:zerop (car (real-value a)))
      (make-instance 'bigfloat :real (list 0 0))
      (destructuring-bind (mantissa exp)
	  (real-value a)
	(make-instance 'bigfloat :real (list mantissa (+ exp n))))))

(macrolet
    ((frob (name)
       (let ((cl-name (intern (string name) '#:cl)))
	 `(defmethod ,name ((a number))
	    (,cl-name a)))))
  (frob realpart)
  (frob imagpart)
  (frob conjugate)
  (frob phase)
  (frob floor))
  
  
(defmethod realpart ((a bigfloat))
  (make-instance 'bigfloat :real (real-value a)))

(defmethod realpart ((a complex-bigfloat))
  (make-instance 'bigfloat :real (real-value a)))

(defmethod imagpart ((a bigfloat))
  (make-instance 'bigfloat :real (intofp 0)))

(defmethod imagpart ((a complex-bigfloat))
  (make-instance 'bigfloat :real (imag-value a)))

(defmethod conjugate ((a bigfloat))
  (make-instance 'bigfloat :real (real-value a)))

(defmethod conjugate ((a complex-bigfloat))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (maxima::fpminus (imag-value a))))

(defmethod cis ((a float))
  (cl:cis a))

(defmethod cis ((a rational))
  (cl:cis a))

(defmethod cis ((a bigfloat))
  (make-instance 'complex-bigfloat
		 :real (maxima::fpsin (real-value a) t)
		 :imag (maxima::fpsin (real-value a) nil)))

(defmethod phase ((a bigfloat))
  (let ((r (real-value a)))
    (if (cl:>= (car r) 0)
	(make-instance 'bigfloat :real (list 0 0))
	(make-instance 'bigfloat :real (maxima::fpquotient (maxima::fppi) (intofp 2))))))

(defmethod phase ((a complex-bigfloat))
  (make-instance 'bigfloat :real (maxima::fpatan2 (imag-value a) (real-value a))))

(defun max (number &rest more-numbers)
  "Returns the greatest of its arguments."
  (declare (optimize (safety 2)) (type (or real bigfloat) number)
	   #-gcl (dynamic-extent more-numbers))
  (dolist (real more-numbers)
    (when (> real number)
      (setq number real)))
  number)

(defun min (number &rest more-numbers)
  "Returns the least of its arguments."
  (declare (optimize (safety 2)) (type (or real bigfloat) number)
	   #-gcl (dynamic-extent more-numbers))
  (do ((nlist more-numbers (cdr nlist))
       (result (the (or real bigfloat) number)))
      ((null nlist) (return result))
    (declare (list nlist))
    (if (< (car nlist) result)
	(setq result (car nlist)))))

;; We really want a real type, but gcl doesn't like it, so use number
;; instead.
(defmethod one-arg-complex ((a #-gcl real #+gcl number))
  (cl:complex a))

(defmethod one-arg-complex ((a bigfloat))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (intofp 0)))

(defmethod two-arg-complex ((a #-gcl real #+gcl number) (b #-gcl real #+gcl number))
  (cl:complex a b))

(defmethod two-arg-complex ((a bigfloat) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (real-value b)))

(defmethod two-arg-complex ((a float) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (intofp a)
		 :imag (real-value b)))

(defmethod two-arg-complex ((a rational) (b bigfloat))
  (make-instance 'complex-bigfloat
		 :real (intofp a)
		 :imag (real-value b)))

(defmethod two-arg-complex ((a bigfloat) (b float))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (intofp b)))

(defmethod two-arg-complex ((a bigfloat) (b rational))
  (make-instance 'complex-bigfloat
		 :real (real-value a)
		 :imag (intofp b)))

(defun complex (a &optional b)
  (if b
      (two-arg-complex a b)
      (one-arg-complex a)))

(defmethod floor ((a bigfloat))
  (maxima::fpentier (maxima::bcons (real-value a))))

(defmethod ffloor ((a bigfloat))
  (make-instance 'bigfloat :real (intofp (floor a))))

(defmethod expt ((a number) (b number))
  (cl:expt a b))

;; This needs more work
(defmethod expt ((a numeric) b)
  (exp (* b (log a))))

(defmethod expt (a (b numeric))
  (exp (* b (log a))))

;;; TO - External
;;;
;;;    TO takes a maxima number and converts it.  Floats remain
;;; floats, maxima rationals are converted to CL rationals.  Maxima
;;; bigfloats are convert to BIGFLOATS.  Maxima complex numbers are
;;; converted to CL complex numbers or to COMPLEX-BIGFLOAT's.
(defun to (maxima-num &optional imag)
  (cond (imag
	 (complex (to maxima-num) (to imag)))
	(t
	 (cond ((cl:realp maxima-num)
		maxima-num)
	       ((consp maxima-num)
		;; Some kind of maxima number
		(cond ((maxima::ratnump maxima-num)
		       ;; Maxima rational ((mrat ...) num den)
		       (/ (second maxima-num) (third maxima-num)))
		      ((maxima::$bfloatp maxima-num)
		       (bigfloat maxima-num))
		      ((maxima::complex-number-p maxima-num #'(lambda (x)
								(or (cl:realp x)
								    (maxima::$bfloatp x))))
		       ;; We have some kind of complex number
		       (let ((re (maxima::$realpart maxima-num))
			     (im (maxima::$imagpart maxima-num)))
			 (to re im)))))
	       (t
		(maxima::merror "BIGFLOAT:  Unable to convert ~M to a CL or BIGFLOAT number" maxima-num))))))

;;; EPSILON - External
;;;
;;;   Return the float epsilon value for the given float type.
(defmethod epsilon ((x cl:float))
  (etypecase x
    (short-float short-float-epsilon)
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)
    (long-float long-float-epsilon)))

(defmethod epsilon ((x bigfloat))
  ;; epsilon is just above 2^(-fpprec).
  (make-instance 'bigfloat
		 :real (list (1+ (ash 1 (1- maxima::fpprec)))
			     (- (1- maxima::fpprec)))))

      

;; Compiler macros to convert + to multiple calls to two-arg-+.  Same
;; for -, *, and /.
#-gcl
(define-compiler-macro + (&whole form &rest args)
  (declare (ignore form))
  (if (null args)
      0
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		`(two-arg-+ ,res ,(car args))))
	  ((null args) res))))

#-gcl
(define-compiler-macro - (&whole form number &rest more-numbers)
  (declare (ignore form))
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result `(two-arg-- ,result ,(car nlist))))
      `(unary-minus ,number)))

#-gcl
(define-compiler-macro * (&whole form &rest args)
  (declare (ignore form))
  (if (null args)
      1
      (do ((args (cdr args) (cdr args))
	   (res (car args)
		`(two-arg-* ,res ,(car args))))
	  ((null args) res))))

#-gcl
(define-compiler-macro / (number &rest more-numbers)
  (if more-numbers
      (do ((nlist more-numbers (cdr nlist))
	   (result number))
	  ((atom nlist) result)
         (declare (list nlist))
	 (setq result `(two-arg-/ ,result ,(car nlist))))
      `(unary-divide ,number)))

#-gcl
(define-compiler-macro /= (&whole form number &rest more-numbers)
  ;; Convert (/= x y) to (not (two-arg-= x y)).  Should we try to
  ;; handle a few more cases?
  (if (cdr more-numbers)
      form
      `(not (two-arg-= ,number ,(car more-numbers)))))

;; Compiler macros to convert <, >, <=, and >= into multiple calls of
;; the corresponding two-arg-<foo> function.
#-gcl
(macrolet
    ((frob (op)
       (let ((method (intern (concatenate 'string
					  (string '#:two-arg-)
					  (symbol-name op)))))
	 `(define-compiler-macro ,op (number &rest more-numbers)
	    (do* ((n number (car nlist))
		  (nlist more-numbers (cdr nlist))
		  (res nil))
		 ((atom nlist) 
		  `(and ,@(nreverse res)))
	      (push `(,',method ,n ,(car nlist)) res))))))
  (frob <)
  (frob >)
  (frob <=)
  (frob >=))

