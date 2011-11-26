;; macros.l - all the basic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;Hamilton, New Zeland 1992-95 - all rights reserved;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; NOTE: If you change this file, please file a ticket to the f2cl
;;; project (trac.common-lisp.net/f2cl/wiki) so that the changes can
;;; be incorporated into the official version.
(in-package :f2cl-lib)

(defparameter *f2cl-macros-version*
  "$Id: macros.l,v fceac530ef0c 2011/11/26 04:02:26 toy $")

(eval-when
    #+gcl (compile load eval)
    #-gcl (:compile-toplevel :load-toplevel :execute)
    (proclaim '(special *verbose*)))
;;----------------------------------------------------------------------------
(defvar *check-array-bounds* nil
  "If non-NIL, generated code checks for array bounds.  If NIL, checking
is not included")

;;------------------------------------------------------------------------------
;;
;; Define the equivalent types between Fortran and Lisp.  This MUST
;; match the types given in f2cl1.l so keep it in sync!
(deftype logical ()
  `(member t nil))

;; Decide what you want integer*4 to be.  Good choices are fixnum or
;; (signed-byte 32).  The latter is good only if your compiler does a
;; good job with this type.  If you aren't sure, use fixnum.  CMUCL
;; does a good job with (signed-byte 32).
;;
;; If you change this, you may need to change some of the macros
;; below, such as INT and AINT!

#+(or cmu scl sbcl)
(deftype integer4 (&optional (low #x-80000000) (high #x7fffffff))
  `(integer ,low ,high))
#-(or cmu scl sbcl)
(deftype integer4 (&optional low high)
  (declare (ignore low high))
  'fixnum)

(deftype integer2 ()
  `(signed-byte 16))
(deftype integer1 ()
  `(signed-byte 8))
(deftype real8 ()
  'double-float)
(deftype real4 ()
  'single-float)
(deftype complex8 ()
  `(complex single-float))
(deftype complex16 ()
  `(complex double-float))

(deftype array-double-float ()
  `(array double-float (*)))
(deftype array-integer4 ()
  `(array integer4 (*)))
(deftype array-single-float ()
  `(array single-float (*)))
(deftype array-strings ()
  `(array string (*)))

(defconstant %false% nil)
(defconstant %true% t)

;;------------------------------------------------------------------------------

;;-----------------------------------------------------------------------------

;; Array dimensions are (d1, d2, d3, ...)
;;
;; Then x(n1, n2, n3, ...) means index is
;;
;; n1 + d1*(n2 + d2*(n3 + d3*(n4 + d4*(n5))))

;; Return an expression that computes the column major index given the
;; indices and the bounds on each dimension.  The bounds are a list of
;; the upper and lower bounds for each dimension.
(defun col-major-index (indices dims)
  (flet ((get-offset (n bound)
	   (let ((lo (first bound)))
	     (if (and (numberp lo) (zerop lo))
		 n
		 `(the fixnum (- (the fixnum ,n) (the fixnum ,lo))))))
	 (get-size (bound)
	   (destructuring-bind (lo hi)
	       bound
	     (cond ((numberp lo)
		    (cond ((numberp hi)
			   (1+ (- hi lo)))
			  ((= lo 1)
			   hi)
			  (t
			   `(- ,hi ,(- lo 1)))))
		   (t
		    `(the fixnum (- ,hi (the fixnum (- (the fixnum ,lo) 1)))))))))
    (let* ((rev-idx (reverse indices))
	   (rev-dim (reverse dims))
	   (idx (get-offset (first rev-idx) (first rev-dim))))
      (do ((d (rest rev-dim) (rest d))
	   (n (rest rev-idx) (rest n)))
	  ((endp d)
	   idx)
	(setf idx `(the fixnum (+ ,(get-offset (first n) (first d))
				  (the fixnum (* ,(get-size (first d)) ,idx)))))))))

(defun check-array-bounds (indices bounds)
  `(and ,@(mapcar #'(lambda (idx dim)
		     `(<= ,(first dim) ,idx ,(second dim)))
		 indices bounds)))

(defmacro fref (arr indices bounds &optional offset)
  (if *check-array-bounds*
      `(aref ,arr (if ,(check-array-bounds indices bounds)
		      (the fixnum (+ (the fixnum ,(or offset 0)) ,(col-major-index indices bounds)))
		      (error "Out of bounds index for array ~S"
			     ',arr)))
      `(aref ,arr (the fixnum (+ (the fixnum ,(or offset 0)) ,(col-major-index indices bounds))))))

(defmacro fset (a b) 
  `(setf (fref ,(second a) ,@(cddr a)) ,b))

(defmacro fref-string (s range)
  `(subseq ,s (1- ,(first range)) ,(second range)))

(defmacro fset-string (a b)
  `(setf (fref-string ,(second a) ,(third a)) (string ,b)))

(defmacro f2cl-// (a b)
  `(concatenate 'string ,a ,b))

;; (with-array-data ((data-var offset-var array))
;;   ...
;; )

(defun find-array-data (array)
  (declare (type (array * (*)) array))
  (let ((offset 0))
    (declare (type fixnum offset)
	     (optimize (speed 3) (safety 0)))
    (loop
     (multiple-value-bind (displaced-to index-offset)
	 (array-displacement array)
       (when (null displaced-to)
	 (return-from find-array-data (values array offset)))
       (incf offset index-offset)
       (setf array displaced-to)))))

(defmacro with-array-data ((data-var offset-var array) &rest body)
  `(multiple-value-bind (,data-var ,offset-var)
    (find-array-data ,array)
     ,@body))

(defun multi-array-data-aux (array-info body)
  (let ((results body))
    (dolist (a (reverse array-info))
      (destructuring-bind (array a-type var-name offset-var)
	  a
	(setf results
	      `((multiple-value-bind (,var-name ,offset-var)
		    (find-array-data ,array)
		  (declare (ignorable ,offset-var ,var-name)
			   (type f2cl-lib:integer4 ,offset-var)
			   (type (simple-array ,a-type (*)) ,var-name))
		  ,@results)))))
    (first results)))

(defmacro with-multi-array-data (array-info &rest body)
  (multi-array-data-aux array-info body))

;; Create an array slice for the array named VNAME whose elements are
;; of type TYPE.  The slice starts at the indices INDICES and the
;; original array has dimensions given by BOUND.
;;
;; This is done by making a displaced array to VNAME with the
;; appropriate offset.
(defmacro array-slice (vname type indices bounds &optional offset)
  ;; To figure the size of the sliced array, use ARRAY-TOTAL-SIZE
  ;; instead of the f2cl derived/declared BOUNDS, just in case we
  ;; screwed up or in case we changed the size of the array in some
  ;; other way.  This isn't possible in a function, but the array
  ;; might be in a common block and we could change the dimensions of
  ;; the common block at runtime.  (Some Fortran code like mpfun does
  ;; this, although it's actually illegal.  Neat hack to "dynamically"
  ;; change the dimensions.  Of course, for this to work in Fortran,
  ;; the common block has to contain exactly that one array, or the
  ;; array must be the last element of the common block.)
  ;;
  ;; Note: In some places in LAPACK, an array slice is taken where the
  ;; slice exceeds the bounds of the array.  However, the array is
  ;; never accessed.  What are we to do?  We could modify the LAPACK
  ;; routines (everywhere!) to check for this, or we can silently make
  ;; array-slice make a 0-element array.  If the array is then
  ;; accessed, we should get an error at the point of access, not the
  ;; point of creation.
  ;;
  ;; This seems somewhat reasonable, so let's do that for array
  ;; slices.
  `(make-array (max 0 (- (array-total-size ,vname)
			 (the fixnum
			   (+ ,(col-major-index indices bounds)
			      (or ,offset 0)))))
    :element-type ',type
    :displaced-to ,vname
    :displaced-index-offset (min (array-total-size ,vname)
				 (the fixnum
				   (+ ,(col-major-index indices bounds)
				      (or ,offset 0))))))

;; Compute an initializer for make-array given the data in the list
;; DATA.  The array has en element type of TYPE and has dimensions of
;; DIMS.
(defmacro array-initialize (type dims data)
  (let ((data-list (gensym))
	(data-len (length data))
	(total-length (gensym)))
    `(let* ((,data-list (list ,@data))
	    (,total-length (reduce #'* (list ,@dims))))
       (cond ((< ,data-len ,total-length)
	      ;; Need to append some data.
	      (append ,data-list (make-list (- ,total-length ,data-len)
					    :initial-element (coerce 0 ',type))))
	     ((> ,data-len ,total-length)
	      ;; Need to truncate some data
	      (subseq ,data-list 0 ,total-length))
	     (t
	      ,data-list)))))  

;;----------------------------------------------------------------------------

#-aclpc (defmacro while (con &rest body)
	  `(loop (if (not ,con) (return t)) ,@body))
;;------------------------------------------------------------------

(defmacro fortran_comment (&rest args)
  (declare (ignore args)))

;;----------------------------------------------------------------------------
;; fdo has similar syntax as do except there will only be one do_vble

(defmacro fdo (do_vble_clause predicate_clause &rest body)
  (let ((step (gensym (symbol-name '#:step-)))
	(iteration_count (gensym (symbol-name '#:cnt-)))
	(loop-var (first do_vble_clause)))
    `(prog* ((,step ,(third (third do_vble_clause)))
	     (,iteration_count 
	      (max 0 (the integer4
		       (truncate (the integer4
				   (+ (the integer4 (- ,(third (first predicate_clause))
						       ,(second do_vble_clause)))
				      ,step))
				 ,step))
		   )))
      (declare (type integer4 ,step ,iteration_count))
      ;; initialise loop variable
      (setq ,loop-var ,(second do_vble_clause))
      loop
      (return
	(cond				; all iterations done
	  ((zerop ,iteration_count) nil)
	  ;; execute loop, in/de-crement loop vble and decrement cntr
	  ,(list 't
		  (append '(tagbody)
			  (append 
			   (append body
				   `(continue
				     (setq ,loop-var (the integer4 ,(third do_vble_clause))
					   ,iteration_count (the integer4 (1- ,iteration_count)))))
			   '((go loop)))))))
      exit)))

;;----------------------------------------------------------------------------
;; macro for division 

(defmacro f2cl/ (x y)
  (let ((top (gensym))
	(bot (gensym)))
    `(let ((,top ,x)
	   (,bot ,y))
      (if (and (typep ,top 'integer)
	       (typep ,bot 'integer))
	  (values (the integer4 (truncate ,top ,bot)))
	  (/ ,top ,bot)))))

(defmacro int-add (arg &rest more-args)
  (if (null more-args)
      arg
      (if (> (length more-args) 1)
	  `(the integer4 (+ ,arg (int-add ,@more-args)))
	  `(the integer4 (+ ,arg ,@more-args)))))

(defun convert-int-sub (args)
  (let ((nargs (length args)))
    (case nargs
      (1
       `(the integer4 (- ,(first args))))
      (2
       `(the integer4 (- ,(first args) ,(second args))))
      (t
       (let ((result `(the integer4 (- ,(first args) ,(second args)))))
	 (dolist (arg (rest (rest args)))
	   (setf result `(the integer4 (- ,result ,arg))))
	 result)))))

(defmacro int-sub (&rest args)
  (convert-int-sub args))
  
(defmacro int-mul (arg &rest more-args)
  (if (null more-args)
      arg
      (if (> (length more-args) 1)
	  `(the integer4 (* ,arg (int-mul ,@more-args)))
	  `(the integer4 (* ,arg ,@more-args)))))


;; macro for a lisp equivalent of Fortran arithmetic IFs
(defmacro arithmetic-if (pred s1 s2 s3)
  (let ((tst (gensym)))
    `(let ((,tst ,pred))
      (cond ((< ,tst 0) ,s1)
	    ((= ,tst 0) ,s2)
	    (t ,s3)))))

;; macro for a lisp equivalent of Fortran computed GOTOs
(defun computed-goto-aux (tags)
  (let ((idx 0)
	(result '()))
    (dolist (tag tags (nreverse result))
      (incf idx)
      (push `(,idx (go ,tag)) result))))

(defmacro computed-goto (tag-lst i)
  `(case ,i
    ,@(computed-goto-aux tag-lst)))

;; macro for a lisp equivalent of Fortran assigned GOTOs
(eval-when
    #+gcl (compile load eval)
    #-gcl (:load-toplevel :compile-toplevel :execute)
(defun make-label (n) 
  (read-from-string (concatenate 'string (symbol-name :label) (princ-to-string n))))


(defun assigned-goto-aux (tag-list)
  (let ((cases nil))
    (dolist (tag tag-list)
      (push `(,tag (go ,(f2cl-lib::make-label tag)))
	    cases))
    (push `(t (error "Unknown label for assigned goto")) cases)
    (nreverse cases)))
)


;; macro for a lisp equivalent of Fortran assigned GOTOs
(defmacro assigned-goto (var tag-list)
  `(case ,var
     ,@(assigned-goto-aux tag-list)))

;;-----------------------------------------------------------------------------
;;
;; Define the intrinsic functions
;;
;; Reference:  The Fortran 77 standard found at www.fortran.com.  Section 15.10

;; INT is the generic name as well as the integer version.  IFIX is
;; the same.  IDINT is the double version.

(declaim (inline int ifix idfix))

#-(or cmu scl)
(defun int (x)
  ;; We use fixnum here because f2cl thinks Fortran integers are
  ;; fixnums.  If this should change, we need to change the ranges
  ;; here as well.
  (etypecase x
    (integer
     (the integer4 x))
    (single-float
     (truncate (the (single-float #.(float most-negative-fixnum)
				  #.(float most-positive-fixnum))
		 x)))
    (double-float
     (truncate (the (double-float #.(float most-negative-fixnum 1d0)
				  #.(float most-positive-fixnum 1d0))
		    x)))
    #+clisp
    (long-float
     (truncate (the (long-float #.(float most-negative-fixnum 1l0)
				#.(float most-positive-fixnum 1l0))
		 x)))
    ((complex single-float)
     (the integer4
       (truncate (the (single-float #.(float (- (ash 1 31)))
				    #.(float (1- (ash 1 31))))
		      (realpart x)))))
    ((complex double-float)
     (the integer4
       (truncate (the (double-float #.(float (- (ash 1 31)) 1d0)
				    #.(float (1- (ash 1 31)) 1d0))
		      (realpart x)))))))

#+(or cmu scl)
(defun int (x)
  ;; For CMUCL, we support the full 32-bit integer range, so INT can
  ;; return a full 32-bit integer.  Tell CMUCL that this is true so we
  ;; generate fast code.  If this is not true, the original Fortran
  ;; code was wrong.
  (etypecase x
    (integer
     (the integer4 x))
    (single-float
     (the integer4
       (truncate (the (single-float #.(float (- (ash 1 31)))
				    #.(float (1- (ash 1 31))))
		   x))))
    (double-float
     (the integer4
       (truncate (the (double-float #.(float (- (ash 1 31)) 1d0)
				    #.(float (1- (ash 1 31)) 1d0))
		   x))))
    #+scl
    (long-float
     (the integer4
       (truncate (the (long-float #.(float (- (ash 1 31)) 1l0)
				  #.(float (1- (ash 1 31)) 1l0))
		   x))))
    #+(and cmu double-double)
    (kernel:double-double-float
     (the integer4
       (truncate (the (kernel:double-double-float #.(float (- (ash 1 31)) 1w0)
						  #.(float (1- (ash 1 31)) 1w0))
		   x))))
    ((complex single-float)
     (the integer4
	  (truncate (the (single-float #.(float (- (ash 1 31)))
				       #.(float (1- (ash 1 31))))
			 (realpart x)))))
    ((complex double-float)
     (the integer4
	  (truncate (the (double-float #.(float (- (ash 1 31)) 1d0)
				       #.(float (1- (ash 1 31)) 1d0))
			 (realpart x)))))))


(defun ifix (x)
  (int x))
(defun idfix (x)
  (int x))

;; AINT is the generic and specific function for real; DINT, for
;; double.  It truncates its arg towards zero and returns the integer
;; as a floating-point number of the same type as its arg.
;;
;; ANINT is the generic and specific function for real; DNINT, for
;; double. It rounds to the nearest integer and returns the result as
;; a float of the same type.
;;
;; NINT is the generic and specific function for real; IDNINT, for
;; double.  Does the same as ANINT, but the result is an integer.

(declaim (inline aint dint anint dnint nint idnint))

;; This is based on the algorithm given by Anton Ertl in
;; comp.arch.arithmetic on Oct. 26, 2002:
;;
;; #define X 9007199254740992. /* 2^53 */
;; double rint(double r)
;; {
;;   if (r<0.0)
;;     return (r+X)-X;
;;   else
;;     return (r-X)+X;
;; }
;;
;; This assumes that we in round-to-nearest mode (the default).
;;
;; These only work if you have IEEE FP arithmetic.  There are 2
;; versions given below.  One is for non-x87, which assumes that
;; single and double FP numbers are properly rounded after each
;; operation.  The version for x87 stores away a value to make sure
;; the rounding happens correctly.
;;
;; Finally, the last version if for platforms where none of this
;; holds and we call ftruncate.
;;
;; With CMUCL pre-18e on sparc, this definition of aint reduces the
;; cost of MPNORM (from MPFUN) from 48.89 sec to 24.88 sec (a factor
;; of 2!) when computing pi to 29593 digits or os.

(declaim (inline rint-s rint-d))
#+(and cmu (or :sse2 (not x86)))
(progn
(defun rint-s (x)
  (declare (single-float x))
  (let ((const (scale-float 1f0 24)))
    (if (>= x 0)
	(+ (- x const) const)
	(- (+ x const) const))))

(defun rint-d (x)
  (declare (double-float x))
  (let ((const (scale-float 1d0 53)))
    (if (>= x 0)
	(+ (- x const) const)
	(- (+ x const) const))))
)

#+(and cmu (and x86 x87))
(progn
(defun rint-s (x)
  (declare (single-float x))
  (let ((junks (make-array 1 :element-type 'single-float))
	(const (scale-float 1f0 24)))
    (if (>= x 0)
	(progn
	  (setf (aref junks 0) (- x const))
	  (+ (aref junks 0) const))
	(progn
	  (setf (aref junks 0) (+ x const))
	  (- (aref junks 0) const)))))

(defun rint-d (x)
  (declare (double-float x))
  (let ((junkd (make-array 1 :element-type 'double-float))
	(const (scale-float 1d0 53)))
    (if (>= x 0)
	(progn
	  (setf (aref junkd 0) (- x const))
	  (+ (aref junkd 0) const))
	(progn
	  (setf (aref junkd 0) (+ x const))
	  (- (aref junkd 0) const)))))
)

;; Truncate x to an integer.
#+cmu
(defun aint (x)
  ;; rint above is fast.  We use it to round the number, and then
  ;; adjust the result to truncate.
  (etypecase x
    (single-float
     (let ((r (rint-s x)))
       (if (> (abs r) (abs x))
	   (if (> r 0)
	       (- r 1)
	       (+ r 1))
	   r)))
    (double-float
     (let ((r (rint-d x)))
       (if (> (abs r) (abs x))
	   (if (> r 0)
	       (- r 1)
	       (+ r 1))
	   r)))
    #+double-double
    (kernel:double-double-float
     (locally 
       (declare (optimize (space 0) (speed 3)))
       (values (ftruncate (the kernel:double-double-float x)))))
    ))
    

#-cmu
(defun aint (x)
  ;; ftruncate is exactly what we want.
  (etypecase x
    (single-float
     (locally 
       (declare (optimize (space 0) (speed 3)))
       (values (ftruncate (the single-float x)))))
    (double-float
     (locally 
       (declare (optimize (space 0) (speed 3)))
       (values (ftruncate (the double-float x)))))
    (long-float
     (locally 
       (declare (optimize (space 0) (speed 3)))
       (values (ftruncate (the long-float x)))))))

(defun dint (x)
  (aint x))

(defun anint (x)
  (values (fround x)))
(defun dnint (x)
  (values (fround x)))
(defun nint (x)
  (values (round x)))
(defun idnint (x)
  (values (round x)))

;; Type conversion
;;
;; FREAL is F2CL's version of the Fortran REAL which takes converts
;; its arg to a real.  SNGL is the same.  DBLE returns a double.  They
;; also return the real part of a complex number.  CMPLX takes one or
;; two args and creates a complex number.

(declaim (inline freal sngl dble dfloat cmplx))
(defun freal (x)
  (coerce (realpart x) 'single-float))

(defun sngl (x)
  (coerce (realpart x) 'single-float))

(defun dble (x)
  (coerce (realpart x) 'double-float))

(defun dfloat (x)
  (coerce (realpart x) 'double-float))

(defun cmplx (x &optional y)
  (complex x (if y y 0)))

(defun dcmplx (x &optional y)
  (coerce (complex x (if y y 0)) '(complex double-float)))

(defun ichar (c)
  (if (stringp c)
      (char-int (aref c 0))
      (char-int c)))
(defun fchar (i)			;intrinsic function char
  (code-char i))

(declaim (inline iabs dabs cabs cdabs amod dmod))
#-aclpc
(defun iabs (x)
  (declare (type integer4 x))
  (abs x))
(defun dabs (x)
  (declare (type double-float x))
  (abs x))
(defun cabs (x)
  (declare (type complex x))
  (abs x))
(defun cdabs (x)
  (declare (type (complex double-float) x))
  (abs x))

(defun amod (x y)
  (declare (type single-float x y))
  (mod x y))
(defun dmod (x y)
  (declare (type double-float x y))
  (mod x y))


;; Transfer of sign.  SIGN is the generic and specific function for
;; real.  ISIGN is for integers; DSIGN for doubles.  Basically
;; computes sign(a2)*|a1|.

(declaim (inline isign sign dsign))

(defun isign (x y)
  (declare (type integer4 x y))
  (if (>= y 0)
      (the integer4 (abs x))
      (the integer4 (- (the integer4 (abs x))))))

;; Fortran 77 says SIGN is a generic!
(defun sign (x y)
  (declare (type (or integer4 single-float double-float) x y))
  (etypecase x
    (integer4
     (isign x y))
    (single-float
     (float-sign y x))
    (double-float
     (float-sign y x))))

(defun dsign (x y)
  (declare (type double-float x y))
  (float-sign y x))

;; Positive difference.  DIM is the generic and specific function for
;; real.  IDIM is for integers; DDIM, doubles.
;;
;; If a1 > a2, returns a1-a2, otherwise 0.  Basically the same as
;; max(0, a1-a2).
(declaim (inline idim dim ddim))
(defun idim (x y)
  (declare (type integer4 x y))
  (max 0 (- x y)))

(defun dim (x y)
  (declare (type (or integer4 single-float double-float) x y))
  (etypecase x
    (integer4
     (max 0 (- x y)))
    (single-float
     (max 0f0 (- x y)))
    (double-float
     (max 0d0 (- x y)))))

(defun ddim (x y)
  (declare (type double-float x y))
  (max 0d0 (- x y)))

;; Double-precision product.  How this is done isn't specified, but I
;; suspect the real args are converted to doubles and then the product
;; is computed.
(defun dprod (x y)
  (declare (single-float x y))
  (* (float x 1d0) (float y 1d0)))

;; The max and min functions.
;;
;; MAX is the generic. MAX0, AMAX1, and DMAX1 returns the max of the
;; args with the same type as the args.
;;
;; AMAX0 takes integer args and returns the max as a real. MAX1 takes
;; real args and returns the max as a integer.  (How the conversion is
;; done isn't specified.)
;;
;; Should we make these macros that expand directly to the appropriate
;; max?
(defun max0 (x y &rest z)
  #-gcl(declare (integer x y))
  (apply #'max x y z))
(defun amax1 (x y &rest z)
  #-gcl(declare (single-float x y))
  (apply #'max x y z))
(defun dmax1 (x y &rest z)
  #-gcl(declare (double-float x y))
  (apply #'max x y z))
(defun max1 (x y &rest z)
  #-gcl(declare (single-float x y))
  (int (apply #'max x y z)))
(defun amax0 (x y &rest z)
  #-gcl(declare (type integer4 x y))
  (float (apply #'max x y z) 1f0))

(defun min0 (x y &rest z)
  (apply #'min x y z))
(defun amin1 (x y &rest z)
  (apply #'min x y z))
(defun dmin1 (x y &rest z)
  (apply #'min x y z))

(defun amin0 (x y &rest z)
  (float (apply #'min x y z)))
(defun min1 (x y &rest z)
  (nint (apply #'min x y z)))

;; Define some compile macros for these max/min functions.
#+(or cmu scl)
(progn
(define-compiler-macro max0 (&rest args)
  `(max ,@args))
(define-compiler-macro amax1 (&rest args)
  `(max ,@args))
(define-compiler-macro dmax1 (&rest args)
  `(max ,@args))
(define-compiler-macro min0 (&rest args)
  `(min ,@args))
(define-compiler-macro amin1 (&rest args)
  `(min ,@args))
(define-compiler-macro dmin1 (&rest args)
  `(min ,@args))
(define-compiler-macro min1 (&rest args)
  `(nint (min ,@args)))

(define-compiler-macro amax0 (&rest args)
  `(float (max ,@args)))
(define-compiler-macro max1 (&rest args)
  `(nint (max ,@args)))

(define-compiler-macro amin0 (&rest args)
  `(float (min ,@args)))
(define-compiler-macro min1 (&rest args)
  `(nint (min ,@args)))
) ; end progn

(defun len (s)
  (length s))

;; From http://www.fortran.com/fortran/F77_std/rjcnf0001-sh-15.html#sh-15.10:
;;
;; INDEX(a1 ,a2) returns an integer value indicating the starting
;; position within the character string a1 of a substring identical
;; to string a2 . If a2 occurs more than once in a1 , the starting
;; position of the first occurrence is returned.
;;
;; If a2 does not occur in a1 , the value zero is returned. Note
;; that zero is returned if LEN(a1) < LEN(a2).
;;
;; Thus the arguments are in the opposite order for CL's SEARCH function.
(defun index (s1 s2)
  (or (search s2 s1) 0))

;; These string operations need some work!
(defun lge (s1 s2)
  (string>= s1 s2))
(defun lgt (s1 s2)
  (string> s1 s2))
(defun lle (s1 s2)
  (string<= s1 s2))
(defun llt (s1 s2)
  (string< s1 s2))

(defun fstring-/= (s1 s2)
  (not (string= s1 s2)))
(defun fstring-= (s1 s2)
  (string= s1 s2))
(defun fstring-> (s1 s2)
  (string> s1 s2))
(defun fstring->= (s1 s2)
  (string>= s1 s2))
(defun fstring-< (s1 s2)
  (string< s1 s2))
(defun fstring-<= (s1 s2)
  (string<= s1 s2))


;; AIMAG: imaginary part of a complex number
;; CONJG: conjugate of a complex number
(declaim (inline aimag conjg dconjg dimag))
(defun aimag (c)
  (imagpart c))
(defun dimag (c)
  (declare (type (complex double-float) c))
  (imagpart c))
(defun conjg (c)
  (conjugate c))
(defun dconjg (c)
  (declare (type (complex double-float) c))
  (conjugate c))

(declaim (inline fsqrt flog))
(defun fsqrt (x)
  (typecase x
    (single-float
     (sqrt (the (or (single-float (0f0)) (member 0f0)) x)))
    (double-float
     (sqrt (the (or (double-float (0d0)) (member 0d0)) x)))
    #+(or scl clisp)
    (long-float
     (sqrt (the (or (long-float (0l0)) (member 0l0)) x)))
    #+(and cmu double-double)
    (kernel:double-double-float
     (sqrt (the (or (kernel:double-double-float (0w0)) (member 0w0)) x)))
    (t
     (sqrt x))))

(defun flog (x)
  (typecase x
    (single-float
     (log (the (or (single-float (0f0)) (member 0f0)) x)))
    (double-float
     (log (the (or (double-float (0d0)) (member 0d0)) x)))
    #+(or scl clisp)
    (long-float
     (log (the (or (long-float (0l0)) (member 0l0)) x)))
    #+(and cmu double-double)
    (kernel:double-double-float
     (log (the (or (kernel:double-double-float (0w0)) (member 0w0)) x)))
    (t
     (log x))))
  
;; Tell Lisp that the arguments always have the correct range.  If
;; this is not true, the original Fortran code was broken anyway, so
;; GIGO (garbage in, garbage out).

(declaim (inline dsqrt csqrt zsqrt alog dlog clog alog10 dlog10))
(defun dsqrt (x)
  (declare (type (double-float 0d0) x))
  (sqrt  x))
(defun csqrt (x)
  (sqrt x))
(defun zsqrt (x)
  (sqrt x))
(defun alog (x)
  (declare (type (or (single-float (0f0)) (member 0f0)) x))
  (log x))
(defun dlog (x)
  (declare (type (or (double-float (0d0)) (member 0d0)) x))
  (log x))
(defun clog (x)
  (log x))
(defun alog10 (x)
  (declare (type (or (single-float (0f0)) (member 0f0)) x))
  (log x 10f0))
(defun dlog10 (x)
  (declare (type (or (double-float (0d0)) (member 0d0)) x))
  (log x 10.0d0))

(declaim (inline log10))
(defun log10 (x)
  (typecase x
    (single-float
     (log (the (or (single-float (0.0f0)) (member 0f0)) x) 10f0))
    (double-float
     (log (the (or (double-float (0.0d0)) (member 0d0)) x) 10d0))
    #+(or scl clisp)
    (long-float
     (log (the (or (long-float (0.0l0)) (member 0l0)) x) 10l0))
    #+(and cmu double-double)
    (kernel:double-double-float
     (log (the (or (kernel:double-double-float (0.0w0)) (member 0w0)) x) 10w0))
    (t
     (/ (log x)
	(typecase x
	  #+(and cmu double-double)
	  ((complex kernel:double-double-float)
	   10w0)
	  #+(or scl clisp)
	  ((complex long-float)
	   10l0)
	  ((complex double-float)
	   10d0)
	  ((complex single-float)
	   10f0)
	  (t
	   (coerce 10 (type-of (realpart x)))))))))

(declaim (inline dexp cexp))
(defun dexp (x)
  (declare (type double-float x))
  (exp x))
(defun cexp (x)
  (declare (type complex x))
  (exp x))

(declaim (inline dsin csin dcos ccos dtan ctan dasin dacos datan atan2 datan2 dsinh dcosh dtanh))
(defun dsin (x)
  (declare (type double-float x))
  (sin x))
(defun csin (x)
  (declare (type complex x))
  (sin x))

(defun dcos (x)
  (declare (type double-float x))
  (cos x))
(defun ccos (x)
  (declare (type complex x))
  (cos x))

(defun dtan (x)
  (declare (type double-float x))
  (tan x))
(defun ctan (x)
  (declare (type complex x))
  (tan x))

(defun dasin (x)
  (declare (type double-float x))
  (asin x))
(defun dacos (x)
  (declare (type double-float x))
  (acos x))
(defun datan (x)
  (declare (type double-float x))
  (atan x))
(defun atan2 (x y)
  (declare (type single-float x))
  (atan x y))
(defun datan2 (x y)
  (declare (type double-float x y))
  (atan x y))

(defun dsinh (x)
  (declare (type double-float x))
  (sinh x))
(defun dcosh (x)
  (declare (type double-float x))
  (cosh x))
(defun dtanh (x)
  (declare (type double-float x))
  (tanh x))

(declaim (inline ffloat))
(defun ffloat (x)
  (coerce x 'single-float))

(defun process-implied-do (ido array-bnds var-types init)
  (destructuring-bind (data-vars &rest looping)
      ido
    (labels
	((convert-type (type)
	   (if (eq type 'integer4)
	       `(truncate (pop ,init))
	       `(coerce (pop ,init) ',type)))
	 (map-vars (v)
	   (mapcar #'(lambda (x b vt)
		       `(fset (fref ,(first x) ,(second x) ,b)
			      ,(convert-type vt)))
		   v array-bnds var-types)))
      (let ((body (map-vars data-vars)))
	(dolist (loopvar looping)
	  (destructuring-bind (index-var start end &optional step)
	      loopvar
	    (setf body `((do ((,index-var ,start (+ ,index-var ,(or step 1))))
			    ((> ,index-var ,end))
			  ,@body)))))
	(car body)))))


;; Process implied do loops for data statements
(defmacro data-implied-do (implied-do array-bnds var-types vals)
  (let ((v (gensym)))
    `(let ((,v (list ,@vals)))
      ,(process-implied-do implied-do array-bnds var-types v))))

;;-----------------------------------------------------------------------------
   
;; Map Fortran logical unit numbers to Lisp streams

#-gcl
(defparameter *lun-hash*
  (make-hash-table))

#+gcl
(defvar *lun-hash*
  (make-hash-table))

(defun lun->stream (lun &optional readp)
  (let ((stream (gethash lun *lun-hash*)))
    (if stream
	stream
	(cond ((eql lun 5)
	       ;; Always standard input
	       (setf (gethash lun *lun-hash*) *standard-input*))
	      ((or (eql lun 6)
		   (eql lun t))
	       ;; Always standard output
	       (setf (gethash lun *lun-hash*) *standard-output*))
	      ((integerp lun)
	       ;; All other cases open a file fort<n>.dat
	       (setf (gethash lun *lun-hash*)
		     (open (format nil "fort~d.dat" lun)
			   :direction :io
			   :if-exists :rename)))
	      ((stringp lun)
	       (setf (gethash lun *lun-hash*)
		     (if readp
			 (make-string-input-stream lun)
			 (make-string-output-stream))))))))

(defun init-fortran-io ()
  "Initialize the F2CL Fortran I/O subsystem to sensible defaults"
  (clrhash *lun-hash*)
  (setf (gethash 6 *lun-hash*) *standard-output*)
  (setf (gethash 5 *lun-hash*) *standard-input*)
  (setf (gethash t *lun-hash*) *standard-output*))

(defun close-fortran-io ()
  "Close all F2CL Fortran units (except for standard output and input)
causing all pending operations to be flushed"
  (maphash #'(lambda (key val)
	       (when (and (streamp val) (not (member key '(5 6 t))))
		 (format t "Closing unit ~A: ~A~%" key val)
		 (close val)))
	       *lun-hash*))

(defun %open-file (&key unit file status access form recl blank)
  (declare (ignore unit))
  ;; We should also check for values of form that we don't support.
  (when recl
    (error "F2CL-LIB does not support record lengths"))
  (when blank
    (error "F2CL-LIB does not support any BLANK mode for files"))
  (when (and access (not (string-equal "sequential"
				       (string-right-trim " " access))))
    (error "F2CL-LIB does not support ACCESS mode ~S" access))
  (when (and form (not (string-equal "unformatted"
				     (string-right-trim " " form))))
    (error "F2CL-LIB does not support FORM ~S" form))
  (let ((s (and status (string-right-trim " " status))))
    (finish-output)
    (cond ((or (null s) (string-equal s "unknown"))
	   (open file :direction :io :if-exists :supersede
		 :if-does-not-exist :create))
	  ((string-equal s "old")
	   (open file :direction :io :if-does-not-exist nil :if-exists :overwrite))
	  ((string-equal s "new")
	   (open file :direction :io :if-exists nil))
	  (t
	   (error "F2CL-LIB does not support this mode for OPEN: ~S~%"
		  s)))))

(defmacro open-file (&key unit iostat err file status access form recl blank)
  (let ((result (gensym)))
    `(prog ((,result (%open-file :unit ,unit :file ,file :status ,status
				 :access ,access :form ,form :recl ,recl :blank ,blank)))
	(when ,result
	  (setf (gethash ,unit *lun-hash*) ,result))
	,(if err `(unless ,result (go ,(f2cl-lib::make-label err))))
	,(if iostat `(setf ,iostat (if ,result 0 1))))))

(defun %rewind (unit)
  (file-position (lun->stream unit) :start))

(defmacro rewind (&key unit iostat err)
  (let ((result (gensym)))
    `(prog ((,result (%rewind ,unit)))
	(declare (ignorable ,result))
	,(if err `(unless ,result (go ,(f2cl-lib::make-label err))))
	,(if iostat `(setf ,iostat (if ,result 0 1))))))
	  

(defun %close (&key unit status)
  (when status
    (error "F2CL-LIB does not support STATUS"))
  (cl:close (lun->stream unit)))

(defmacro close$ (&key unit iostat err status)
  (let ((result (gensym)))
    `(prog ((,result (%close :unit ,unit  :status ,status)))
	(declare (ignorable ,result))
	,(if err `(unless ,result (go ,(f2cl-lib::make-label err))))
	,(if iostat `(setf ,iostat (if ,result 0 1))))))

#-gcl
(declaim (ftype (function (t) stream) lun->stream))

(defmacro fformat (dest-lun format-cilist &rest args)
  (let ((stream (gensym)))
    `(let ((,stream (lun->stream ,dest-lun)))
       (execute-format-main ,stream ',format-cilist ,@args)
       ,@(unless (or (eq t dest-lun) (numberp dest-lun))
	  `((when (stringp ,dest-lun)
	     (replace ,dest-lun (get-output-stream-string ,stream))))))))

(defun execute-format (top stream format arg-list)
  (do ((formats format (if (and top (null formats))
			   format
			   (rest formats))))
      ((or (null arg-list)
	   (and (not top)
		(null formats)))
       #+nil
       (progn
	 (format t "~&end formats = ~S~%" formats)
	 (format t "~&end arg-list = ~S~%" arg-list))
       (do ((more formats (rest more)))
	   ((not (stringp (first more))))
	 (format stream (first more)))
       arg-list)

    (when (null formats)
      ;; We're out of formats but not arguments.  I think Fortran says
      ;; we should start over at the last repeat spec.  So we look
      ;; over all the formats until we find the first number.  That
      ;; means it's a repeat spec.
      ;;
      ;; This is probably wrong for complicated format statements.
      (do ((f format (cdr f))
	   (last-rep nil))
	  ((null f)
	   (setf formats last-rep))
	(when (or (eq (car f) t)
		  (numberp (car f)))
	  (setf last-rep f)))

      (when (null formats)
	;; Now what?  We couldn't find a repeat spec, so should we
	;; just start over?
	(setf formats format)))
    #+nil
    (let ((*print-circle* t))
      (format t "~&arg-list = ~S~%" arg-list)
      (format t "~&formats = ~S~%" formats))
    (cond ((listp (first formats))
	   (format stream (caar formats) (pop arg-list)))
	  ((eq (first formats) #\:)
	   ;; Terminate control if there are no more items
	   (when (null arg-list)
	     (return-from execute-format)))
	  ((numberp (first formats))
	   ;; Repeat a group some fixed number of times
	   (dotimes (k (first formats))
	     ;;(format t "k = ~A, format = ~S~%" k (second formats))
	     (setf arg-list
		   (execute-format nil stream (second formats) arg-list))
	     ;; Gotta exit if we're out of arguments to print!
	     (unless arg-list
	       (return)))
	   (setf formats (rest formats))
	   ;; Output a newline after the repeat (I think Fortran says this)
	   ;;(format stream "~&")
	   ;;(format t "  cont with format = ~S~%" formats)
	   )
	  ((eq (first formats) t)
	   ;; Repeat "forever" (until we run out of data)
	   (loop while arg-list do
		(setf arg-list
		      (execute-format nil stream (second formats) arg-list))
	      ;; Output a newline after the repeat (I think Fortran says this)
		(format stream "~%")))
	  (t
	   (format stream (car formats))))))
	   
(defun flatten-list (x)
  (labels ((flatten-helper (x r);; 'r' is the stuff to the 'right'.
	     (cond ((null x) r)
		   ((atom x)
		    (cons x r))
		   (t (flatten-helper (car x)
				      (flatten-helper (cdr x) r))))))
    (flatten-helper x nil)))

;; Fortran G format, roughly.  We use ~F for numbers "near" 1, and use
;; ~E otherwise.
;;
;; Note that g77 seems to use an exponent marker of E for single and
;; double floats, but Sun Fortran uses E and D.  I think I like E and
;; D to distinguish between them.  Also note that g77 uses just enough
;; digits for the numbers, but Sun Fortran seems to specify the number
;; of printed digits to be 16 or so.  Thus 1.0 is "1.0" with g77, but
;; "1.0000000000000" with Sun Fortran.  I like g77's style better.
(defun fortran-format-g (stream arg colon-p at-p &rest args)
  (declare (ignore colon-p at-p args))
  (let* ((marker (typecase arg
		   (single-float "E")
		   (double-float "D")))
	 (a (abs arg))
	 ;; g77 uses limits 1d-4 and 1d9.  Sun Fortran uses 1 and
	 ;; 1d15.
	 (format-string (if (or (zerop a)
				(and (>= a 1d-4)
				     (< a 1d9)))
			    "~F"
			    (concatenate 'string "~,,2,,,,'"
					 marker
					 "E"))))
    (format stream format-string arg)))

;; Output objects in Fortran style, roughly.  This basically means
;; complex numbers are printed as "(<re>, <im>)", floats use
;; FORTRAN-FORMAT-G, integers use ~D, strings are printed as is, and
;; T/NIL become "T" or "F".
(defun fortran-format (stream arg colon-p at-p &rest args)
  (declare (ignore colon-p at-p args))
  (etypecase arg
    (complex
     #-gcl
     (format stream "(~/f2cl-lib::fortran-format-g/, ~/f2cl-lib::fortran-format-g/)"
	     (realpart arg) (imagpart arg))
     #+gcl
     (progn
       (fortran-format-g stream (realpart arg) nil nil)
       (fortran-format-g stream (imagpart arg) nil nil)))
    (float
     #-gcl
     (format stream "  ~/f2cl-lib::fortran-format-g/" arg)
     #+gcl
     (fortran-format-g stream arg nil nil))
    (integer
     (format stream "  ~D" arg))
    (string
     (format stream "~A" arg))
    ((member t nil)
     (format stream (if arg "T " "F ")))))
     
(defun execute-format-main (stream format &rest args)
  (cond
    ((eq format :list-directed)
     ;; This prints out the args separated by spaces and puts a line
     ;; break after about 80 columns.
     (format stream "~& ~{~<~%~1,81:;~?~>~^~}~%"
	     (let (pars)
	       (dolist (v args)
		 ;; Some special cases.  Let FORTRAN-FORMAT handle
		 ;; most cases, except strings, which we just print
		 ;; out ourselves.  Lists (from implied-do loops) and
		 ;; arrays use FORTRAN-FORMAT for each element.
		 (typecase v
		   (string
		    (push "~A"  pars)
		    (push (list v) pars))
		   (cons
		    (dolist (item v)
		      #-gcl
		      (progn
			(push "~/f2cl-lib::fortran-format/" pars)
			(push (list item) pars))
		      (progn
			(push "~A" pars)
			(push (fortran-format nil item nil nil) pars))))
		   (array
		    (dotimes (k (length v))
		      #-gcl
		      (progn
			(push "~/f2cl-lib::fortran-format/" pars)
			(push (list (aref v k)) pars))
		      #+gcl
		      (progn
			(push "~A" pars)
			(push (fortran-format nil (list (aref v k)) nil nil)
			      pars))))
		   (t
		    #-gcl
		    (progn
		      (push "~/f2cl-lib::fortran-format/" pars)
		      (push (list v) pars))
		    #+gcl
		    (progn
		      (push "~A" pars)
		      (push (fortran-format nil (list v) nil nil) pars)))))
	       ;;(format t "~S~%" (reverse pars))
	       (nreverse pars))))
    (t
     (let ((format-list (copy-tree format))
	   (arg-list
	    (apply #'append
		   (map 'list #'(lambda (x)
				  (cond ((or (numberp x)
					     (typep x 'bigfloat:bigfloat)
					     (typep x 'bigfloat:complex-bigfloat))
					 (list x))
					((stringp x)
					 (list x))
					((member x '(t nil))
					 ;; Convert T and NIL to :T
					 ;; and :F so we print out T
					 ;; and F, respectively.
					 (case x
					   ((t)
					    (list :t))
					   ((nil)
					    (list :f))
					   (t
					    (list x))))
					(t
					 (coerce x 'list))))
			args))))
       (execute-format t stream format-list arg-list)))))


;; Initialize a multi-dimensional array of character strings. I think
;; we need to do it this way to appease some picky compilers (like
;; CMUCL).  The initial-element is needed to get rid of a warning
;; about the default initial element not being a simple
;; string. However, this initializes all elements of the array to
;; exactly the same string, so we loop over the entire array contents
;; and initialize each element with a string of the appropriate
;; length.  The string is initialized with #\Space because it seems
;; that's what Fortran initializes it to.
(defmacro f2cl-init-string (dims len &optional inits)
  (let ((init (gensym (symbol-name '#:array-)))
	(k (gensym (symbol-name '#:idx-))))
    `(let ((,init (make-array (* ,@dims)
			      :element-type `(simple-array character (,',@len))
			      :initial-element (make-string ,@len))))
       (dotimes (,k (array-total-size ,init))
	 (setf (aref ,init ,k)
	       (make-string ,@len :initial-element #\Space)))
       ,@(when inits
	   (let ((k 0)
		 (forms nil))
	     (dolist (val inits)
	       (push `(replace (aref ,init ,k) ,val) forms)
	       (incf k))
	     (nreverse forms)))
		
       ,init)))

;; This macro is supposed to set LHS to the RHS assuming that the LHS
;; is a Fortran CHARACTER type of length LEN.
;;
;; Currently, converts the RHS to the appropriate length string and
;; assigns it to the LHS.  However, this can generate quite a bit of
;; garbage.  We might want to be a bit smarter and use loops to
;; replace the individual characters of the LHS with the appropriate
;; characters from the RHS.
(defmacro f2cl-set-string (lhs rhs (string len))
  (declare (ignore string))
  (etypecase lhs
    (symbol
     ;; Assignment to a simple string.
     `(setf ,lhs (f2cl-string ,rhs ,len)))
    (list
     ;; Assignment to an array
     `(fset ,lhs (f2cl-string ,rhs ,len)))))

(defun f2cl-string (string len)
  ;; Create a string of the desired length by either appending spaces
  ;; or truncating the string.
  (let ((slen (length string)))
    (cond ((= slen len)
	   ;; Need to make a copy of the string, so we don't have
	   ;; duplicated structure.
	   (copy-seq string))
	  ((> slen len)
	   ;; Truncate the string
	   (subseq string 0 len))
	  (t
	   ;; String is too short, so append some spaces
	   (concatenate 'string string (make-string (- len slen) :initial-element #\Space))))))


;;; Strictly speaking, this is not part of Fortran, but so many
;;; Fortran packages use these routines, we're going to add them here.
;;; They're much easier to implement in Lisp than in Fortran!

;;
;;  DOUBLE-PRECISION MACHINE CONSTANTS
;;  D1MACH( 1) = B**(EMIN-1), THE SMALLEST POSITIVE MAGNITUDE.
;;  D1MACH( 2) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.
;;  D1MACH( 3) = B**(-T), THE SMALLEST RELATIVE SPACING.
;;  D1MACH( 4) = B**(1-T), THE LARGEST RELATIVE SPACING.
;;  D1MACH( 5) = LOG10(B)
;;

(defun d1mach (i)
  (ecase i
    (1 least-positive-normalized-double-float)
    (2 most-positive-double-float)
    (3 #-(or gcl ecl) double-float-epsilon
       #+(or gcl ecl) (scale-float (float #X10000000000001 1d0) -105))
    (4 (scale-float #-(or gcl ecl) double-float-epsilon
		    #+(or gcl ecl) (scale-float (float #X10000000000001 1d0) -105)
		    1))
    (5 (log (float (float-radix 1d0) 1d0) 10d0))))

(defun r1mach (i)
  (ecase i
    (1 least-positive-normalized-single-float)
    (2 most-positive-single-float)
    (3 single-float-epsilon)
    (4 (scale-float single-float-epsilon 1))
    (5 (log (float (float-radix 1f0)) 10f0))))

;;
;;     This is the CMLIB version of I1MACH, the integer machine
;;     constants subroutine originally developed for the PORT library.
;;
;;     I1MACH can be used to obtain machine-dependent parameters
;;     for the local machine environment.  It is a function
;;     subroutine with one (input) argument, and can be called
;;     as follows, for example
;;
;;          K = I1MACH(I)
;;
;;     where I=1,...,16.  The (output) value of K above is
;;     determined by the (input) value of I.  The results for
;;     various values of I are discussed below.
;;
;;  I/O unit numbers.
;;    I1MACH( 1) = the standard input unit.
;;    I1MACH( 2) = the standard output unit.
;;    I1MACH( 3) = the standard punch unit.
;;    I1MACH( 4) = the standard error message unit.
;;
;;  Words.
;;    I1MACH( 5) = the number of bits per integer storage unit.
;;    I1MACH( 6) = the number of characters per integer storage unit.
;;
;;  Integers.
;;    assume integers are represented in the S-digit, base-A form
;;
;;               sign ( X(S-1)*A**(S-1) + ... + X(1)*A + X(0) )
;;
;;               where 0 .LE. X(I) .LT. A for I=0,...,S-1.
;;    I1MACH( 7) = A, the base.
;;    I1MACH( 8) = S, the number of base-A digits.
;;    I1MACH( 9) = A**S - 1, the largest magnitude.
;;
;;  Floating-Point Numbers.
;;    Assume floating-point numbers are represented in the T-digit,
;;    base-B form
;;               sign (B**E)*( (X(1)/B) + ... + (X(T)/B**T) )
;;
;;               where 0 .LE. X(I) .LT. B for I=1,...,T,
;;               0 .LT. X(1), and EMIN .LE. E .LE. EMAX.
;;    I1MACH(10) = B, the base.
;;
;;  Single-Precision
;;    I1MACH(11) = T, the number of base-B digits.
;;    I1MACH(12) = EMIN, the smallest exponent E.
;;    I1MACH(13) = EMAX, the largest exponent E.
;;
;;  Double-Precision
;;    I1MACH(14) = T, the number of base-B digits.
;;    I1MACH(15) = EMIN, the smallest exponent E.
;;    I1MACH(16) = EMAX, the largest exponent E.
(defun i1mach (i)
  (ecase i
    ;; What does the unit numbers really mean in Lisp?  What do we
    ;; really want?
    
    ;; The standard input unit
    (1 5)
    ;; The standard output unit
    (2 6)
    ;; The standard punch unit
    (3 6)
    ;; The standard error message unit
    (4 6)

    ;; The number of bits per integer storage unit.  What does this
    ;; mean in Lisp?
    (5
     (integer-length most-positive-fixnum))
    ;; The number of characters per integer storage unit.  What does
    ;; this mean in Lisp?
    (6 4)

    ;; The base of integers.  Assume 2's complement
    (7 2)
    ;; The number of base-2 digits.  Assume fixnum size?
    (8 (integer-length most-positive-fixnum))
    ;; The largest magnitude
    (9 most-positive-fixnum)

    ;; Base of floating-poing representation
    (10 (float-radix 1f0))
    ;; Number of digits in representation
    (11 (float-digits 1f0))
    ;; Smallest exponent
    (12 (multiple-value-bind (frac exp sign)
	    (decode-float least-positive-normalized-single-float)
	  (declare (ignore frac sign))
	  (+ exp 1)))
    ;; Largest exponent
    (13 (multiple-value-bind (frac exp sign)
	    (decode-float most-positive-single-float)
	  (declare (ignore frac sign))
	  (- exp 1)))
    ;; Same for double-precision
    (14 (float-digits 1d0))
    (15 (multiple-value-bind (frac exp sign)
	    (decode-float least-positive-normalized-double-float)
	  (declare (ignore frac sign))
	  (+ exp 1)))
    (16 (multiple-value-bind (frac exp sign)
	    (decode-float most-positive-double-float)
	  (declare (ignore frac sign))
	  (- exp 1)))
    ))

;; F2cl cannot tell if a STOP statement is an error condition or just
;; the end of the program.  So, by default, we signal a continuable
;; error.  However, we give the user the option of silently returning
;; or not.
(defvar *stop-signals-error-p* nil
  "When non-NIL, STOP will signal an continuable error.  Otherwise, STOP just returns")

(defun stop (&optional arg)
  (when arg
    (format cl::*error-output* "~A~%" arg))
  (when *stop-signals-error-p*
    (cerror "Continue anyway" "STOP reached")))

(defmacro f2cl-copy-seq (dst src dst-type src-type)
  (flet ((copy-error ()
	   (error "F2CL cannot copy arrays of element type ~A to ~A~%"
		  src-type dst-type)))
    (cond ((subtypep dst-type 'float)
	   ;; Copy to float array
	   (cond ((subtypep src-type 'float)
		  `(replace ,dst ,src))
		 ((subtypep src-type 'complex)
		  ;; Copy complex to float by putting each real and
		  ;; imaginary part into the float array, in order.
		  (let ((idx (gensym "IDX-"))
			(el (gensym "EL-")))
		    `(loop for ,idx of-type fixnum from 0 by 2 below (length ,dst)
			for ,el of-type ,src-type across ,src
			do
			(progn
			  (setf (aref ,dst ,idx) (realpart ,el))
			  (setf (aref ,dst (1+ ,idx)) (imagpart ,el))))))
		 (t
		  (copy-error))))
	  ((subtypep dst-type 'complex)
	   ;; Copy to complex array
	   (cond ((subtypep src-type 'float)
		  (let ((idx (gensym "IDX-"))
			(dst-idx (gensym "DST-IDX-")))
		    `(loop for ,idx of-type fixnum from 0 by 2 below (length ,src)
			for ,dst-idx of-type fixnum from 0 below (length ,dst)
			do
			(setf (aref ,dst ,dst-idx) (complex (aref ,src ,idx)
							    (aref ,src (1+ ,idx)))))))
		 ((subtypep src-type 'complex)
		  `(replace ,dst ,src))
		 (t
		  (copy-error))))
	  (t
	   (copy-error)))))

(defmacro make-compatible-seq (type array array-type)
  (let ((element-type (second type))
	(array-type (second array-type)))
    (cond ((subtypep element-type 'float)
	   (cond ((subtypep array-type 'complex)
		  `(make-array (* 2 (length ,array)) :element-type ',element-type))
		 (t
		  `(make-array (length ,array) :element-type ',element-type))))
	  ((subtypep element-type 'complex)
	   (cond ((subtypep array-type 'complex)
		  `(make-array (length ,array) :element-type ',element-type))
		 (t
		  `(make-array (ceiling (length ,array) 2) :element-type ',element-type))))
	  (t
	   (error "Don't know how to make an array with element-type ~A~%" element-type)))))


;;;-------------------------------------------------------------------------
;;; end of macros.l
;;;
;;; $Id: macros.l,v fceac530ef0c 2011/11/26 04:02:26 toy $
;;; $Log$
;;; Revision 1.117  2011/02/28 22:21:07  rtoy
;;; When opening an old file, we should set :if-exists to :overwrite to
;;; overwrite the file if written too.
;;;
;;; Revision 1.116  2011/02/20 20:51:04  rtoy
;;; Oops.  STOP should signal an error if *STOP-SIGNALS-ERROR-P* is
;;; non-NIL.
;;;
;;; Revision 1.115  2010/12/28 00:06:52  rtoy
;;; Assert the type of the arg to fsqrt to be non-negative, excluding
;;; negative zero.
;;;
;;; Revision 1.21  2010/10/16 15:24:09  rtoy
;;; Allow f2cl-lib to recognize BIGFLOAT and COMPLEX-BIGFLOAT numbers so
;;; that they can at least be printed, if not quite in the format
;;; specified by the format string.
;;;
;;; Revision 1.20  2009/01/08 18:25:34  rtoy
;;; Update f2cl to latest version of f2cl, and regenerate all of the lisp
;;; code.
;;;
;;; The testsuite works fine, so I assume the quadpack and other slatec
;;; routines are ok.
;;;
;;; The lsquares tests runs fine, so the lbfgs routines appear to be ok.
;;;
;;; src/numerical/f2cl-lib.lisp:
;;; o Update from f2cl macros.l, 2009/01/08
;;;
;;; src/numerical/f2cl-package.lisp:
;;; o Update from f2cl f2cl0.l, 2009/01/08
;;;
;;; src/numerical/slatec:
;;; o Regenerate lisp files
;;;
;;; share/lbfgs:
;;; o Split lbfgs.f into one function per file and add these new files.
;;; o Generate new lisp files
;;; o Update lbfgs.mac to load the new list files.
;;; o Added lbfgs-lisp.system so we know how to regenerate the lisp files
;;;   in the future.
;;;
;;; share/lapack:
;;; o Add lapack-lisp.system so we know how to regenerate the lisp files
;;;   in the future.
;;; o Regenerate all of the lisp files.
;;;
;;; Revision 1.19  2008/07/27 07:04:15  robert_dodier
;;; Merge patches-for-ecl-branch into main trunk.
;;; With these changes, Maxima builds without errors with Clisp, CMUCL, SBCL, and GCL,
;;; and run_testsuite does not appear to report any errors that are not reported
;;; otherwise. Not yet tested with ECL.
;;;
;;; Revision 1.18  2008/03/26 14:48:26  robert_dodier
;;; For (D1MACH 3) and (D1MACH 4), make the values for GCL match the
;;; values as shown in d1mach.f.
;;;
;;; Revision 1.17  2008/03/26 13:17:55  rtoy
;;; Oops.  Fix bug in previous commit where the test for lun = t was
;;; inside the test for integerp lun.  Just use separate tests for units
;;; 5, 6, and t.
;;;
;;; Revision 1.16  2008/03/26 12:54:54  rtoy
;;; Change how *lun-hash* is handled, based on Robert's changes.  Rather
;;; than initializing *lun-hash* to NIL, we continue to initialize it to
;;; a(n) (empty) hash-table.  Now we don't have to check *lun-hash* for
;;; NIL everywhere.  Then we change lun->stream to initialize the table
;;; for units 5, 6, and t to the correct streams.  The effect should be
;;; the same and nicely localized to the one function.
;;;
;;; This change (or a very similar one) will go into f2cl.
;;;
;;; The following test works with both clisp, gcl, and cmucl:
;;;
;;; (%i1) :lisp (slatec::xerprn "FOOBAR" -1 "BAZQUUX" 72)
;;;
;;; Revision 1.15  2008/03/15 20:00:48  robert_dodier
;;; Lazy initialization of *LUN-HASH*. Putting the initialization into
;;; DEFPARAMETER caused some Clisp installations to barf up an error message
;;; (something about attempted write on a closed stream) when SLATEC::XERPRN was called.
;;;
;;; Revision 1.14  2007/04/28 15:17:27  are_muc
;;; reverted back the changes to %close and %open-file, as rtay whishes to
;;; keep f2cl versions idetical.
;;;
;;; Revision 1.13  2007/04/27 17:44:45  are_muc
;;; changed key argument lists to %open-file and %close by removing unused
;;; ones and allowing them nevertheless to remove compiler warnings.
;;;
;;; Revision 1.12  2007/01/11 17:08:41  rtoy
;;; GCL apparently supports
;;; least-positive-normalized-{double,single}-float now.
;;;
;;; Revision 1.11  2006/12/26 14:53:47  rtoy
;;; SBCL can handle integer4 types well, too.
;;;
;;; Revision 1.10  2006/12/20 18:13:05  rtoy
;;; Update to latest f2cl versions.
;;;
;;; Revision 1.9  2006/07/27 05:37:46  robert_dodier
;;; Commit patches submitted by Douglas Crosher to support Scieneer and Allegro.
;;; With these changes, make and run_testsuite succeed for SBCL, Clisp, and GCL on Linux.
;;;
;;; Revision 1.8  2006/01/31 15:22:26  rtoy
;;; o Regenerate all f2cl'ed code because dasyjy.f was not getting the
;;;   alfa and beta arrays intialized from the data statements.  (This was
;;;   caused by a bug in f2cl.)
;;; o Update f2cl-lib.lisp to match the version of f2cl used to regenerate
;;;   these files.
;;;
;;; Revision 1.7  2005/05/19 12:40:27  rtoy
;;; Update and merge to current version of macros.l from the f2cl
;;; distribution.  Most of the changes done for maxima have been merged to
;;; the f2cl version, and we're just picking up the changes in the f2cl
;;; version.
;;;
;;;-----------------------------------------------------------------------------
