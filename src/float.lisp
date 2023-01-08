;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module float)

;; EXPERIMENTAL BIGFLOAT PACKAGE VERSION 2- USING BINARY MANTISSA
;; AND POWER-OF-2 EXPONENT.
;; EXPONENTS MAY BE BIG NUMBERS NOW (AUG. 1975 --RJF)
;; Modified:	July 1979 by CWH to run on the Lisp Machine and to comment
;;              the code.
;;		August 1980 by CWH to run on Multics and to install
;;		new FIXFLOAT.
;;		December 1980 by JIM to fix BIGLSH not to pass LSH a second
;;		argument with magnitude greater than MACHINE-FIXNUM-PRECISION.

;; Number of bits of precision in a fixnum and in the fields of a flonum for
;; a particular machine.  These variables should only be around at eval
;; and compile time.  These variables should probably be set up in a prelude
;; file so they can be accessible to all Macsyma files.

(eval-when
    (:compile-toplevel :load-toplevel :execute)
    (defconstant +machine-fixnum-precision+ (integer-length most-positive-fixnum)))

;; Internal specials

;; FPROUND uses this to return a second value, i.e. it sets it before
;; returning.  This number represents the number of binary digits its input
;; bignum had to be shifted right to be aligned into the mantissa.  For
;; example, aligning 1 would mean shifting it FPPREC-1 places left, and
;; aligning 7 would mean shifting FPPREC-3 places left.

(defvar *m)

;; *DECFP = T if the computation is being done in decimal radix.  NIL implies
;; base 2.  Decimal radix is used only during output.

(defvar *decfp nil)

;; FIXME:  These don't appear to be used anywhere.  Remove these.
(defvar max-bfloat-%pi bigfloat%pi)
(defvar max-bfloat-%e  bigfloat%e)
(defvar max-bfloat-%gamma bigfloat%gamma)
(defvar max-bfloat-log2 bigfloat_log2)


(declare-top (special *cancelled $bfloat))

;; Representation of a Bigfloat:  ((BIGFLOAT SIMP precision) mantissa exponent)
;; precision -- number of bits of precision in the mantissa.
;;		precision = (integer-length mantissa)
;; mantissa -- a signed integer representing a fractional portion computed by
;;	       fraction = (// mantissa (^ 2 precision)).
;; exponent -- a signed integer representing the scale of the number.
;;	       The actual number represented is (* fraction (^ 2 exponent)).

(defun hipart (x nn)
  (if (bignump nn)
      (abs x)
      (haipart x nn)))

(defun fpprec1 (assign-var q)
  (declare (ignore assign-var))
  (if (or (not (fixnump q)) (< q 1))
      (merror (intl:gettext "fpprec: value must be a positive integer; found: ~M") q))
  (setq fpprec (+ 2 (integer-length (expt 10. q)))
	bigfloatone ($bfloat 1)
	bigfloatzero ($bfloat 0)
	bfhalf (list (car bigfloatone) (cadr bigfloatone) 0)
	bfmhalf (list (car bigfloatone) (- (cadr bigfloatone)) 0))
  q)

;; FPSCAN is called by lexical scan when a
;; bigfloat is encountered.  For example, 12.01B-3
;; would be the result of (FPSCAN '(/1 /2) '(/0 /1) '(/- /3))
;; Arguments to FPSCAN are a list of characters to the left of the
;; decimal point, to the right of the decimal point, and in the exponent.

(defun fpscan (lft rt exp &aux (*read-base* 10.) (*m 1) (*cancelled 0))
  (setq exp (readlist exp))
  (bigfloatp
   (let ((fpprec (+ 4 fpprec (integer-length exp)
		    (floor (1+ (* #.(/ (log 10.0) (log 2.0)) (length lft))))))
	 $float temp)
     (setq temp (add (readlist lft)
		     (div (readlist rt) (expt 10. (length rt)))))
     ($bfloat (cond ((> (abs exp) 1000.)
		     (cons '(mtimes) (list temp (list '(mexpt) 10. exp))))
		    (t (mul2 temp (power 10. exp))))))))

(defun dim-bigfloat (form result)
  (let (($lispdisp nil))
    (dimension-atom (maknam (fpformat form)) result)))

;; Assume that X has the form ((BIGFLOAT ... <prec>) ...).
;; Return <prec>.
(defun bigfloat-prec (x)
  (car (last (car x))))

;; Converts the bigfloat L to list of digits including |.| and the
;; exponent marker |b|. The number of significant digits is controlled
;; by $fpprintprec.
(defun fpformat (l)
  (if (not (member 'simp (cdar l) :test #'eq))
      (setq l (cons (cons (caar l) (cons 'simp (cdar l))) (cdr l))))
  (cond ((equal (cadr l) 0)
	 (if (not (equal (caddr l) 0))
	     (mtell "FPFORMAT: warning: detected an incorrect form of 0.0b0: ~M, ~M~%"
		    (cadr l) (caddr l)))
	 (list '|0| '|.| '|0| '|b| '|0|))
	(t ;; L IS ALWAYS POSITIVE FP NUMBER
	 (let* ((extradigs (floor (1+ (quotient (integer-length (caddr l)) #.(/ (log 10.0) (log 2.0))))))
		(fpprec (+ extradigs (decimalsin (- (bigfloat-prec l) 2))))
	        (*m 1)
	        (*cancelled 0))
	   (setq l
		 (let ((*decfp t)
		       (of (bigfloat-prec l))
		       (l (cdr l))
		       (expon nil))
		   (setq expon (- (cadr l) of))
		   (setq l (if (minusp expon)
			       (fpquotient (intofp (car l)) (fpintexpt 2 (- expon) of))
			       (fptimes* (intofp (car l)) (fpintexpt 2 expon of))))
		   (incf fpprec (- extradigs))
		   (list (fpround (car l)) (+ (- extradigs) *m (cadr l)))))
	   (let ((*print-base* 10.)
		 *print-radix*
		 (expo-adjust 0)
		 (l1 nil))
             (setq l1 (let*
			  ((effective-printprec (if (or (= $fpprintprec 0) (> $fpprintprec fpprec)) fpprec $fpprintprec))
			   (integer-to-explode (round (car l) (expt 10 (- fpprec effective-printprec))))
			   (exploded-integer (explodec integer-to-explode)))
			;; If the rounded integer has more digits than
			;; expected, we need to adjust the exponent by
			;; this amount.  This also means we need to remove
			;; these extra digits so that the result has the
			;; desired number of digits.
			(setf expo-adjust (- (length exploded-integer) effective-printprec))
			(when (plusp expo-adjust)
			  (setf exploded-integer (butlast exploded-integer expo-adjust)))
			(if $bftrunc
			    (do ((l (nreverse exploded-integer) (cdr l)))
				((not (eq '|0| (car l))) (nreverse l)))
			    exploded-integer)))
             (nconc (ncons (car l1)) (ncons '|.|)
                    (or (cdr l1) (ncons '|0|))
                    (ncons '|b|)
                    (explodec (+ (1- (cadr l)) expo-adjust))))))))

;; NOTE: This is a modified version of FORMAT-EXP-AUX from CMUCL to
;; support printing of bfloats.
(defun bfloat-format-e (stream arg colonp atp
			&optional w d e (k 1)
			  overflowchar (padchar #\space) exponentchar)
  (declare (ignore colonp))
  (flet ((exponent-value (x)
	   ;; Compute the (decimal exponent) of the bfloat number X.
	   (let* (($fpprintprec 1)
		  (f (fpformat x))
		  (marker (position '|b| f)))
	     ;; FIXME: do something better than printing and reading
	     ;; the result.
	     (read-from-string
	      (format nil "~{~A~}" (nthcdr (1+ marker) f)))))
	 (bfloat-to-string (x fdigits scale)
	   ;; Print the bfloat X with FDIGITS after the decimal
	   ;; point. This means, roughtly, FDIGITS+1 significant
	   ;; digits.
	   (let* (($fpprintprec (if fdigits
				    (if (zerop fdigits)
					1
					(+ fdigits scale))
				    0))
		  (f (fpformat (bcons (fpabs (cdr x)))))
		  (marker (position '|b| f))
		  (digits (remove '|.| (subseq f 0 marker))))
	     ;; Depending on the value of k, move the decimal
	     ;; point. DIGITS was printed assuming the decimal point
	     ;; is after the first digit. But if fdigits = 0, fpformat
	     ;; actually printed out one too many digits, so we need
	     ;; to remove that.
	     (when (and fdigits (zerop fdigits))
	       (setf digits (butlast digits)))
	     (cond ((zerop k)
		    (push '|.| digits))
		   ((minusp k)
		    ;; Put the leading decimal and then some zeroes
		    (dotimes (i (abs k))
		      (push #\0 digits))
		    (push '|.| digits))
		   (t
		    ;; The number is scaled by 10^k. Do this by
		    ;; putting the decimal point in the right place,
		    ;; appending zeroes if needed.
		    (setf digits
			  (cond ((> k (length digits))
				 (concatenate 'list
					      digits
					      (make-list (- k (length digits))
							 :initial-element #\0)
					      (list '|.|)))
				(t
				 (concatenate 'list
					      (subseq digits 0 k)
					      (list '|.|)
					      (subseq digits k)))))))
	     (let* ((str (format nil "~{~A~}" digits))
		    (len (length str)))
	       (when (and fdigits (>= fdigits len))
		 ;; Append some zeroes to get the desired number of digits
		 (setf str (concatenate 'string str
					(make-string (+ 1 k (- fdigits len))
						     :initial-element #\0)))
		 (setf len (length str)))
	       (values str
		       len
		       (char= (aref str 0) #\.)
		       (char= (aref str (1- (length str))) #\.)
		       1
		       0)))))
    (let* ((num-expt (exponent-value arg))
	   (expt (if (zerop (second arg))
		     0
		     (1+ (- num-expt k))))
	   (estr (format nil "~D" (abs expt)))
	   (elen (if e (max (length estr) e) (length estr)))
	   (add-zero-p nil))
      (cond ((and w overflowchar e (> elen e))
	     ;; Exponent overflow
	     (dotimes (i w)
	       (write-char overflowchar stream)))
	    (t
	     ;; The hairy case
	     (let* ((fdig (if d
			      (if (plusp k)
				  (1+ (- d k))
				  d)
			      nil))
		    (spaceleft (if w
				   (- w 2 elen
				      (if (or atp (minusp (second arg)))
					  1 0))
				   nil)))
	       #+(or)
	       (progn
		 (format t "d, k = ~D ~D~%" d k)
		 (format t "fdig = ~D, spaceleft = ~D~%" fdig spaceleft))
	       
	       (multiple-value-bind (fstr flen lpoint tpoint)
		   (bfloat-to-string arg fdig (or k 1))
		 #+(or)
		 (format t "fstr flen lpoint tpoint = ~S ~S ~S ~S~%"
			 fstr flen lpoint tpoint)
		 (when (and d (zerop d)) (setq tpoint nil))
		 (when w
		   (decf spaceleft flen)
		   ;; See CLHS 22.3.3.2.  "If the parameter d is
		   ;; omitted, ... [and] if the fraction to be
		   ;; printed is zero then a single zero digit should
		   ;; appear after the decimal point."  So we need to
		   ;; subtract one from here because we're going to
		   ;; add an extra 0 digit later.
		   (when (and (null d) (char= (aref fstr (1- flen)) #\.))
		     (setf add-zero-p t)
		     (decf spaceleft))
		   (when lpoint
		     (if (or (> spaceleft 0) tpoint)
			 (decf spaceleft)
			 (setq lpoint nil)))
		   (when (and tpoint (<= spaceleft 0))
		     (setq tpoint nil)))
		 #+(or)
		 (format t "w, spaceleft overflowchar = ~S ~S ~S~%"
			 w spaceleft overflowchar)
		 (cond ((and w (< spaceleft 0) overflowchar)
			;; Significand overflow; output the overflow char
			(dotimes (i w)
			  (write-char overflowchar stream)))
		       (t
			(when w
			  (dotimes (i spaceleft)
			    (write-char padchar stream)))
			(if (minusp (second arg))
			    (write-char #\- stream)
			    (when atp (write-char #\+ stream)))
			(when lpoint
			  (write-char #\0 stream))

			(write-string fstr stream)
			;; Add a zero if we need it.  Which means
			;; we figured out we need one above, or
			;; another condition.  Basically, append a
			;; zero if there are no width constraints
			;; and if the last char to print was a
			;; decimal (so the trailing fraction is
			;; zero.)
			(when (or add-zero-p
				  (and (null w)
				       (char= (aref fstr (1- flen)) #\.)))
			  (write-char #\0 stream))
			(write-char (if exponentchar
					exponentchar
					#\b)
				    stream)
			(write-char (if (minusp expt) #\- #\+) stream)
			(when e
			  (dotimes (i (- e (length estr)))
			    (write-char #\0 stream)))
			(write-string estr stream)))))))))
  (values))

;; NOTE: This is a modified version of FORMAT-FIXED-AUX from CMUCL to
;; support printing of bfloats.
(defun bfloat-format-f (stream number colonp atsign &optional w d (k 0) ovf (pad #\space))
  (declare (ignore colonp))
  (labels
      ((exponent-value (x)
	 ;; Compute the (decimal exponent) of the bfloat number X.
	 (let* (($fpprintprec 1)
		(f (fpformat x))
		(marker (position '|b| f)))
	   ;; FIXME: do something better than printing and reading
	   ;; the result.
	   (read-from-string
	    (format nil "~{~A~}" (nthcdr (1+ marker) f)))))
       (bfloat-to-string (x fdigits scale spaceleft)
	 ;; Print the bfloat X with FDIGITS after the decimal
	 ;; point. To do this we need to know the exponent because
	 ;; fpformat always produces exponential output. If the
	 ;; exponent is E, and we want FDIGITS after the decimal
	 ;; point, we need FDIGITS + E digits printed.
	 (flet ((compute-prec (exp spaceleft)
		  #+nil
		  (format t "compute-prec ~D ~D~%" exp spaceleft)
		  (cond (fdigits
			 (+ fdigits exp 1))
			(spaceleft
			 (max (1- spaceleft) (1+ exp)))
			(t
			 (max (1+ exp) 0)))))
	   (let* ((exp (+ k (exponent-value x)))
		  ($fpprintprec (compute-prec exp spaceleft))
		  (f (let ((maxima::$bftrunc nil))
		       #+nil
		       (format t "printprec = ~D~%" $fpprintprec)
		       (fpformat (bcons (fpabs (cdr x))))))
		  (marker (position '|b| f))
		  (digits (remove '|.| (subseq f 0 marker))))
	     ;; Depending on the value of scale, move the decimal
	     ;; point. DIGITS was printed assuming the decimal point
	     ;; is after the first digit. But if fdigits = 0, fpformat
	     ;; actually printed out one too many digits, so we need
	     ;; to remove that.
	     #+nil
	     (format t "exp, fdigits = ~D ~D, digits = ~S~%" exp fdigits digits)
	     #+nil
	     (when (and fdigits (zerop fdigits))
	       (setf digits (butlast digits)))
	     ;; Figure out where the decimal point should go.  An
	     ;; exponent of 0 means the decimal is after the first
	     ;; digit.
	     (cond ((minusp exp)
		    (dotimes (k (1- (abs exp)))
		      (push '|0| digits))
		    (push '|.| digits))
		   ((< exp (length digits))
		    #+nil
		    (format t "exp, len = ~D ~D~%" exp (length digits))
		    (setf digits (concatenate 'list
					      (subseq digits 0 (1+ exp))
					      (list '|.|)
					      (subseq digits (1+ exp)))))
		   (t
		    (setf digits (append digits (list '|.|)))))
	     (let* ((str (format nil "~{~A~}" digits))
		    (len (length str)))
	       #+nil
	       (format t "str = ~S~%" str)
	       (when (and fdigits (>= fdigits len))
		 ;; Append some zeroes to get the desired number of digits
		 (setf str (concatenate 'string str
					(make-string (+ 1 scale (- fdigits len))
						     :initial-element #\0)))
		 (setf len (length str)))
	       (values str
		       len
		       (char= (aref str 0) #\.)
		       (char= (aref str (1- (length str))) #\.)
		       1
		       0))))))
    (let ((spaceleft w))
      (when (and w (or atsign (minusp (second number))))
	(decf spaceleft))
      (multiple-value-bind (str len lpoint tpoint)
	  (bfloat-to-string number d k spaceleft)
	;;if caller specifically requested no fraction digits, suppress the
	;;optional trailing zero
	(when (and d (zerop d)) (setq tpoint nil))
	(when w 
	  (decf spaceleft len)
	  ;;optional leading zero
	  (when lpoint
	    (if (or (> spaceleft 0) tpoint) ;force at least one digit
		(decf spaceleft)
		(setq lpoint nil)))
	  ;;optional trailing zero
	  (when tpoint
	    (if (> spaceleft 0)
		(decf spaceleft)
		(setq tpoint nil))))
	(cond ((and w (< spaceleft 0) ovf)
	       ;;field width overflow
	       (dotimes (i w) (write-char ovf stream))
	       t)
	      (t
	       (when w (dotimes (i spaceleft) (write-char pad stream)))
	       (if (minusp (second number))
		   (write-char #\- stream)
		   (if atsign (write-char #\+ stream)))
	       (when lpoint (write-char #\0 stream))
	       (write-string str stream)
	       (when tpoint (write-char #\0 stream))
	       nil))))))

;; NOTE: This is a modified version of FORMAT-EXP-AUX from CMUCL to
;; support printing of bfloats.
(defun bfloat-format-g (stream arg colonp atsign
			&optional w d e (k 1)
			  ovf (pad #\space) exponentchar)
  (declare (ignore colonp))
  (flet ((exponent-value (x)
	   ;; Compute the (decimal exponent) of the bfloat number X.
	   (let* (($fpprintprec 1)
		  (f (fpformat x))
		  (marker (position '|b| f)))
	     ;; FIXME: do something better than printing and reading
	     ;; the result.
	     (read-from-string
	      (format nil "~{~A~}" (nthcdr (1+ marker) f)))))
	 (bfloat-to-string (x fdigits)
	   ;; Print the bfloat X with FDIGITS after the decimal
	   ;; point. This means, roughtly, FDIGITS+1 significant
	   ;; digits.
	   (let* (($fpprintprec (if fdigits
				    (if (zerop fdigits)
					1
					(1+ fdigits))
				    0))
		  (f (fpformat (bcons (fpabs (cdr x)))))
		  (marker (position '|b| f))
		  (digits (remove '|.| (subseq f 0 marker))))
	     ;; Depending on the value of k, move the decimal
	     ;; point. DIGITS was printed assuming the decimal point
	     ;; is after the first digit. But if fdigits = 0, fpformat
	     ;; actually printed out one too many digits, so we need
	     ;; to remove that.
	     (when (and fdigits (zerop fdigits))
	       (setf digits (butlast digits)))
	     (cond ((zerop k)
		    (push '|.| digits))
		   ((minusp k)
		    ;; Put the leading decimal and then some zeroes
		    (dotimes (i (abs k))
		      (push #\0 digits))
		    (push '|.| digits))
		   (t
		    ;; The number is scaled by 10^k. Do this by
		    ;; putting the decimal point in the right place,
		    ;; appending zeroes if needed.
		    (setf digits
			  (cond ((> k (length digits))
				 (concatenate 'list
					      digits
					      (make-list (- k (length digits))
							 :initial-element #\0)
					      (list '|.|)))
				(t
				 (concatenate 'list
					      (subseq digits 0 k)
					      (list '|.|)
					      (subseq digits k)))))))
	     (let* ((str (format nil "~{~A~}" digits))
		    (len (length str)))
	       (when (and fdigits (>= fdigits len))
		 ;; Append some zeroes to get the desired number of digits
		 (setf str (concatenate 'string str
					(make-string (+ 1 k (- fdigits len))
						     :initial-element #\0)))
		 (setf len (length str)))
	       (values str
		       len
		       (char= (aref str 0) #\.)
		       (char= (aref str (1- (length str))) #\.)
		       1
		       0)))))
    (let* ((n (1+ (exponent-value arg)))
	   (orig-d d))
      	;; Default d if omitted.  The procedure is taken directly from
	;; the definition given in the manual (CLHS 22.3.3.3), and is
	;; not very efficient, since we generate the digits twice.
	;; Future maintainers are encouraged to improve on this.
	;;
	;; It's also not very clear whether q in the spec is the
	;; number of significant digits or not.  I (rtoy) think it
	;; makes more sense if q is the number of significant digits.
	;; That way 1d300 isn't printed as 1 followed by 300 zeroes.
	;; Exponential notation would be used instead.
	(unless d
	  (let* ((q (1- (nth-value 1 (bfloat-to-string arg nil)))))
	    (setq d (max q (min n 7)))))
	(let* ((ee (if e (+ e 2) 4))
	       (ww (if w (- w ee) nil))
	       (dd (- d n)))
	  #+(or)
	  (progn
	    (format t "d  = ~A~%" d)
	    (format t "ee = ~A~%" ee)
	    (format t "ww = ~A~%" ww)
	    (format t "dd = ~A~%" dd)
	    (format t "n  = ~A~%" n))
	  (cond ((<= 0 dd d)
		 ;; Use dd fraction digits, even if that would cause
		 ;; the width to be exceeded.  We choose accuracy over
		 ;; width in this case.
		 (let* ((fill-char (if (bfloat-format-f stream arg nil atsign
							ww
							dd
							0
							ovf pad)
				       ovf
				       #\space)))
		   (dotimes (i ee) (write-char fill-char stream))))
		(t
		 (bfloat-format-e stream arg nil atsign
				  w
				  orig-d
				  e (or k 1)
				  ovf pad exponentchar)))))))

;; Tells you if you have a bigfloat object.  BUT, if it is a bigfloat,
;; it will normalize it by making the precision of the bigfloat match
;; the current precision setting in fpprec.  And it will also convert
;; bogus zeroes (mantissa is zero, but exponent is not) to a true
;; zero.
(defun bigfloatp (x)
  ;; A bigfloat object looks like '((bigfloat simp <prec>) <mantissa> <exp>)
  ;; Note bene that the simp flag is optional -- don't count on its presence.
  (prog (x-prec)
     (cond ((not ($bfloatp x)) (return nil))
	   ((= fpprec (setq x-prec (bigfloat-prec x)))
	    ;; Precision matches.  (Should we fix up bogus bigfloat
	    ;; zeros?)
	    (return x))
	   ((> fpprec x-prec)
	    ;; Current precision is higher than bigfloat precision.
	    ;; Scale up mantissa and adjust exponent to get the
	    ;; correct precision.
	    (setq x (bcons (list (fpshift (cadr x) (- fpprec x-prec))
				 (caddr x)))))
	   (t
	    ;; Current precision is LOWER than bigfloat precision.
	    ;; Round the number to the desired precision.
	    (setq x (bcons (list (fpround (cadr x))
				 (+ (caddr x) *m fpprec (- x-prec)))))))
     ;; Fix up any bogus zeros that we might have created.
     (return (if (equal (cadr x) 0) (bcons (list 0 0)) x))))

(defun bigfloat2rat (x)
  (setq x (bigfloatp x))
  (let (($float2bf t)
	(exp nil)
	(y nil)
	(sign nil))
    (setq exp (cond ((minusp (cadr x))
		     (setq sign t
			   y (fpration1 (cons (car x) (fpabs (cdr x)))))
		     (rplaca y (* -1 (car y))))
		    (t (fpration1 x))))
    (when $ratprint
      (princ "`rat' replaced ")
      (when sign (princ "-"))
      (princ (maknam (fpformat (cons (car x) (fpabs (cdr x))))))
      (princ " by ")
      (princ (car exp))
      (write-char #\/)
      (princ (cdr exp))
      (princ " = ")
      (setq x ($bfloat (list '(rat simp) (car exp) (cdr exp))))
      (when sign (princ "-"))
      (princ (maknam (fpformat (cons (car x) (fpabs (cdr x))))))
      (terpri)
      (finish-output))
    exp))

(defun fpration1 (x)
  (let ((fprateps (cdr ($bfloat (if $bftorat
				    (list '(rat simp) 1 (exptrl 2 (1- fpprec)))
				    $ratepsilon)))))
    (or (and (equal x bigfloatzero) (cons 0 1))
	(prog (y a)
	   (return (do ((xx x (setq y (invertbigfloat
				       (bcons (fpdifference (cdr xx) (cdr ($bfloat a)))))))
			(num (setq a (fpentier x))
			     (+ (* (setq a (fpentier y)) num) onum))
			(den 1 (+ (* a den) oden))
			(onum 1 num)
			(oden 0 den))
		       ((and (not (zerop den))
			     (not (fpgreaterp
				   (fpabs (fpquotient
					   (fpdifference (cdr x)
							 (fpquotient (cdr ($bfloat num))
								     (cdr ($bfloat den))))
					   (cdr x)))
				   fprateps)))
			(cons num den))))))))

(defun float-nan-p (x)
  (and (floatp x) (not (= x x))))

(defun float-inf-p (x)
  (and (floatp x) (not (float-nan-p x)) (beyond-extreme-values x)))

(defun beyond-extreme-values (x)
  (multiple-value-bind (most-negative most-positive) (extreme-float-values x)
    (cond
      ((< x 0) (< x most-negative))
      ((> x 0) (> x most-positive))
      (t nil))))

(defun extreme-float-values (x)
  ;; BLECHH, I HATE ENUMERATING CASES. IS THERE A BETTER WAY ??
  (typecase x ;gcl returns an atomic list type with type-of
    (short-float (values most-negative-short-float most-positive-short-float))
    (single-float (values most-negative-single-float most-positive-single-float))
    (double-float (values most-negative-double-float most-positive-double-float))
    (long-float (values most-negative-long-float most-positive-long-float))
    ;; NOT SURE THE FOLLOWING REALLY WORKS
    ;; #+(and cmu double-double)
    ;; (kernel:double-double-float
    ;;   (values most-negative-double-double-float most-positive-double-double-float))
    ))

;; Convert a floating point number into a bigfloat.
(defun floattofp (x)
  (if (float-nan-p x)
    (merror (intl:gettext "bfloat: attempted conversion of floating point NaN (not-a-number).~%")))
  (if (float-inf-p x)
    (merror (intl:gettext "bfloat: attempted conversion of floating-point infinity.~%")))
  (unless $float2bf
    (let ((p (float-precision x)))
      (if (< fpprec p)
        (mtell (intl:gettext "bfloat: converting float ~S to bigfloat.~%") x))))

  ;; Need to check for zero because different lisps return different
  ;; values for integer-decode-float of a 0.  In particular CMUCL
  ;; returns 0, -1075.  A bigfloat zero needs to have an exponent and
  ;; mantissa of zero.
  (if (zerop x)
      (list 0 0)
      (multiple-value-bind (frac exp sign)
	  (integer-decode-float x)
	;; Scale frac to the desired number of bits, and adjust the
	;; exponent accordingly.
	(let ((scale (- fpprec (integer-length frac))))
	  (list (ash (* sign frac) scale)
		(+ fpprec (- exp scale)))))))

;; Convert a bigfloat into a floating point number.
(defun fp2flo (l)
  (let ((precision (bigfloat-prec l))
	(mantissa (cadr l))
	(exponent (caddr l))
	(fpprec machine-mantissa-precision)
	(*m 0))
    ;; Round the mantissa to the number of bits of precision of the
    ;; machine, and then convert it to a floating point fraction.  We
    ;; have 0.5 <= mantissa < 1
    (setq mantissa (quotient (fpround mantissa) (expt 2.0 machine-mantissa-precision)))
    ;; Multiply the mantissa by the exponent portion.  I'm not sure
    ;; why the exponent computation is so complicated.
    ;;
    ;; GCL doesn't signal overflow from scale-float if the number
    ;; would overflow.  We have to do it this way.  0.5 <= mantissa <
    ;; 1.  The largest double-float is .999999 * 2^1024.  So if the
    ;; exponent is 1025 or higher, we have an overflow.
    (let ((e (+ exponent (- precision) *m machine-mantissa-precision)))
      (if (>= e 1025)
	  (merror (intl:gettext "float: floating point overflow converting ~:M") l)
	  (scale-float mantissa e)))))

;; New machine-independent version of FIXFLOAT.  This may be buggy. - CWH
;; It is buggy!  On the PDP10 it dies on (RATIONALIZE -1.16066076E-7)
;; which calls FLOAT on some rather big numbers.  ($RATEPSILON is approx.
;; 7.45E-9) - JPG

(defun fixfloat (x)
  (let (($ratepsilon (expt 2.0 (- machine-mantissa-precision))))
    (maxima-rationalize x)))

;; Takes a flonum arg and returns a rational number corresponding to the flonum
;; in the form of a dotted pair of two integers.  Since the denominator will
;; always be a positive power of 2, this number will not always be in lowest
;; terms.

(defun bcons (s)
  `((bigfloat simp ,fpprec) . ,s))

(defmfun ($bfloat :properties ((evfun t))) (x)
  (let (y)
    (cond ((bigfloatp x))
	  ((or (numberp x)
	       (member x '($%e $%pi $%gamma) :test #'eq))
	   (bcons (intofp x)))
	  ((or (atom x) (member 'array (cdar x) :test #'eq))
	   (if (eq x '$%phi)
	       ($bfloat '((mtimes simp)
			  ((rat simp) 1 2)
			  ((mplus simp) 1 ((mexpt simp) 5 ((rat simp) 1 2)))))
	       x))
	  ((eq (caar x) 'mexpt)
	   (if (equal (cadr x) '$%e)
	       (*fpexp ($bfloat (caddr x)))
	       (exptbigfloat ($bfloat (cadr x)) (caddr x))))
	  ((eq (caar x) 'mncexpt)
	   (list '(mncexpt) ($bfloat (cadr x)) (caddr x)))
	  ((eq (caar x) 'rat)
	   (ratbigfloat (cdr x)))
	  ((setq y (safe-get (caar x) 'floatprog))
	   (funcall y (mapcar #'$bfloat (cdr x))))
	  ((or (trigp (caar x)) (arcp (caar x)) (eq (caar x) '$entier))
	   (setq y ($bfloat (cadr x)))
	   (if ($bfloatp y)
	       (cond ((eq (caar x) '$entier) ($entier y))
		     ((arcp (caar x))
		      (setq y ($bfloat (logarc (caar x) y)))
		      (if (free y '$%i)
			  y (let ($ratprint) (fparcsimp ($rectform y)))))
		     ((member (caar x) '(%cot %sec %csc) :test #'eq)
		      (invertbigfloat
		       ($bfloat (list (ncons (safe-get (caar x) 'recip)) y))))
		     (t ($bfloat (exponentialize (caar x) y))))
	       (subst0 (list (ncons (caar x)) y) x)))
	  (t (recur-apply #'$bfloat x)))))

(defprop mplus addbigfloat floatprog)
(defprop mtimes timesbigfloat floatprog)
(defprop %sin sinbigfloat floatprog)
(defprop %cos cosbigfloat floatprog)
(defprop rat ratbigfloat floatprog)
(defprop %atan atanbigfloat floatprog)
(defprop %tan tanbigfloat floatprog)
(defprop %log logbigfloat floatprog)
(defprop mabs mabsbigfloat floatprog)

(defun addbigfloat (h)
  (prog (fans tst r nfans)
     (setq fans (setq tst bigfloatzero) nfans 0)
     (do ((l h (cdr l)))
	 ((null l))
       (cond ((setq r (bigfloatp (car l)))
	      (setq fans (bcons (fpplus (cdr r) (cdr fans)))))
	     (t (setq nfans (list '(mplus) (car l) nfans)))))
     (return (cond ((equal nfans 0) fans)
		   ((equal fans tst) nfans)
		   (t (simplify (list '(mplus) fans nfans)))))))

(defun ratbigfloat (r)
  ;; R is a Maxima ratio, represented as a list of the numerator and
  ;; denominator.  FLOAT-RATIO doesn't like it if the numerator is 0,
  ;; so handle that here.
  (if (zerop (car r))
      (bcons (list 0 0))
      (bcons (float-ratio r))))

;; This is borrowed from CMUCL (float-ratio-float), and modified for
;; converting ratios to Maxima's bfloat numbers.
(defun float-ratio (x)
  (let* ((signed-num (first x))
	 (plusp (plusp signed-num))
	 (num (if plusp signed-num (- signed-num)))
	 (den (second x))
	 (digits fpprec)
	 (scale 0))
    (declare (fixnum digits scale))
    ;;
    ;; Strip any trailing zeros from the denominator and move it into the scale
    ;; factor (to minimize the size of the operands.)
    (let ((den-twos (1- (integer-length (logxor den (1- den))))))
      (declare (fixnum den-twos))
      (decf scale den-twos)
      (setq den (ash den (- den-twos))))
    ;;
    ;; Guess how much we need to scale by from the magnitudes of the numerator
    ;; and denominator.  We want one extra bit for a guard bit.
    (let* ((num-len (integer-length num))
	   (den-len (integer-length den))
	   (delta (- den-len num-len))
	   (shift (1+ (the fixnum (+ delta digits))))
	   (shifted-num (ash num shift)))
      (declare (fixnum delta shift))
      (decf scale delta)
      (labels ((float-and-scale (bits)
		 (let* ((bits (ash bits -1))
			(len (integer-length bits)))
		   (cond ((> len digits)
			  (assert (= len (the fixnum (1+ digits))))
			  (multiple-value-bind (f0)
			      (floatit (ash bits -1))
			    (list (first f0) (+ (second f0)
						(1+ scale)))))
			 (t
			  (multiple-value-bind (f0)
			      (floatit bits)
			    (list (first f0) (+ (second f0) scale)))))))
	       (floatit (bits)
		 (let ((sign (if plusp 1 -1)))
		   (list (* sign bits) 0))))
	(loop
	  (multiple-value-bind (fraction-and-guard rem)
	      (truncate shifted-num den)
	    (let ((extra (- (integer-length fraction-and-guard) digits)))
	      (declare (fixnum extra))
	      (cond ((/= extra 1)
		     (assert (> extra 1)))
		    ((oddp fraction-and-guard)
		     (return
		       (if (zerop rem)
			   (float-and-scale
			    (if (zerop (logand fraction-and-guard 2))
				fraction-and-guard
				(1+ fraction-and-guard)))
			   (float-and-scale (1+ fraction-and-guard)))))
		    (t
		     (return (float-and-scale fraction-and-guard)))))
	    (setq shifted-num (ash shifted-num -1))
	    (incf scale)))))))

(defun decimalsin (x)
  (do ((i (quotient (* 59. x) 196.) (1+ i))) ;log[10](2)=.301029
      (nil)
    (when (> (integer-length (expt 10. i)) x)
      (return (1- i)))))

(defun atanbigfloat (x)
  (*fpatan (car x) (cdr x)))

(defun *fpatan (a y)
  (fpend (let ((fpprec (+ 8. fpprec)))
	   (if (null y)
	       (if ($bfloatp a) (fpatan (cdr ($bfloat a)))
		   (list '(%atan) a))
	       (fpatan2 (cdr ($bfloat a)) (cdr ($bfloat (car y))))))))

;; Bigfloat atan
(defun fpatan (x)
  (prog (term x2 ans oans one two tmp)
     (setq one (intofp 1) two (intofp 2))
     (cond ((fpgreaterp (fpabs x) one)
	    ;; |x| > 1.
	    ;;
	    ;; Use A&S 4.4.5:
	    ;;    atan(x) + acot(x) = +/- pi/2 (+ for x >= 0, - for x < 0)
	    ;;
	    ;; and A&S 4.4.8
	    ;;    acot(z) = atan(1/z)
	    (setq tmp (fpquotient (fppi) two))
	    (setq ans (fpdifference tmp (fpatan (fpquotient one x))))
	    (return (cond ((fplessp x (intofp 0))
			   (fpdifference ans (fppi)))
			  (t ans))))
	   ((fpgreaterp (fpabs x) (fpquotient one two))
	    ;; |x| > 1/2
	    ;;
	    ;; Use A&S 4.4.42, third formula:
	    ;;
	    ;; atan(z) = z/(1+z^2)*[1 + 2/3*r + (2*4)/(3*5)*r^2 + ...]
	    ;;
	    ;; r = z^2/(1+z^2)
	    (setq tmp (fpquotient x (fpplus (fptimes* x x) one)))
	    (setq x2 (fptimes* x tmp) term (setq ans one))
	    (do ((n 0 (1+ n)))
		((equal ans oans))
	      (setq term
		    (fptimes* term (fptimes* x2 (fpquotient
						 (intofp (+ 2 (* 2 n)))
						 (intofp (+ (* 2 n) 3))))))
	      (setq oans ans ans (fpplus term ans)))
	    (setq ans (fptimes* tmp ans)))
	   (t
	    ;; |x| <= 1/2.  Use Taylor series (A&S 4.4.42, first
	    ;; formula).
	    (setq ans x x2 (fpminus (fptimes* x x)) term x)
	    (do ((n 3 (+ n 2)))
		((equal ans oans))
	      (setq term (fptimes* term x2))
	      (setq oans ans
		    ans (fpplus ans (fpquotient term (intofp n)))))))
     (return ans)))

;; atan(y/x) taking into account the quadrant.  (Also equal to
;; arg(x+%i*y).)
(defun fpatan2 (y x)
  (cond ((equal (car x) 0)
	 ;; atan(y/0) = atan(inf), but what sign?
	 (cond ((equal (car y) 0)
		(merror (intl:gettext "atan2: atan2(0, 0) is undefined.")))
	       ((minusp (car y))
		;; We're on the negative imaginary axis, so -pi/2.
		(fpquotient (fppi) (intofp -2)))
	       (t
		;; The positive imaginary axis, so +pi/2
		(fpquotient (fppi) (intofp 2)))))
	((signp g (car x))
	 ;; x > 0.  atan(y/x) is the correct value.
	 (fpatan (fpquotient y x)))
	((signp g (car y))
	 ;; x < 0, and y > 0.  We're in quadrant II, so the angle we
	 ;; want is pi+atan(y/x).
	 (fpplus (fppi) (fpatan (fpquotient y  x))))
	(t
	 ;; x <= 0 and y <= 0.  We're in quadrant III, so the angle we
	 ;; want is atan(y/x)-pi.
	 (fpdifference (fpatan (fpquotient y x)) (fppi)))))

(defun tanbigfloat (a)
  (setq a (car a))
  (fpend (let ((fpprec (+ 8. fpprec)))
	   (cond (($bfloatp a)
		  (setq a (cdr ($bfloat a)))
		  (fpquotient (fpsin a t) (fpsin a nil)))
		 (t (list '(%tan) a))))))

;; Returns a list of a mantissa and an exponent.
(defun intofp (l)
  (cond ((not (atom l)) ($bfloat l))
	((floatp l) (floattofp l))
	((equal 0 l) '(0 0))
	((eq l '$%pi) (fppi))
	((eq l '$%e) (fpe))
	((eq l '$%gamma) (fpgamma))
	(t (list (fpround l) (+ *m fpprec)))))

;; It seems to me that this function gets called on an integer
;; and returns the mantissa portion of the mantissa/exponent pair.

;; "STICKY BIT" CALCULATION FIXED 10/14/75 --RJF
;; BASE must not get temporarily bound to NIL by being placed
;; in a PROG list as this will confuse stepping programs.

(defun fpround (l &aux (*print-base* 10.) *print-radix*)
  (prog (adjust)
     (cond
       ((null *decfp)
	;;*M will be positive if the precision of the argument is greater than
	;;the current precision being used.
	(setq *m (- (integer-length l) fpprec))
	(when (= *m 0)
	  (setq *cancelled 0)
	  (return l))
	;;FPSHIFT is essentially LSH.
	(setq adjust (fpshift 1 (1- *m)))
	(when (minusp l) (setq adjust (- adjust)))
	(incf l adjust)
	(setq *m (- (integer-length l) fpprec))
	(setq *cancelled (abs *m))
	(cond ((zerop (hipart l (- *m)))
					;ONLY ZEROES SHIFTED OFF
	       (return (fpshift (fpshift l (- -1 *m))
				1)))	; ROUND TO MAKE EVEN
	      (t (return (fpshift l (- *m))))))
       (t
	(setq *m (- (flatsize (abs l)) fpprec))
	(setq adjust (fpshift 1 (1- *m)))
	(when (minusp l) (setq adjust (- adjust)))
	(setq adjust (* 5 adjust))
	(setq *m (- (flatsize (abs (setq l (+ l adjust)))) fpprec))
	(return (fpshift l (- *m)))))))

;; Compute (* L (expt d n)) where D is 2 or 10 depending on
;; *decfp. Throw away an fractional part by truncating to zero.
(defun fpshift (l n)
  (cond ((null *decfp)
	 (cond ((and (minusp n) (minusp l))
		;; Left shift of negative number requires some
		;; care. (That is, (truncate l (expt 2 n)), but use
		;; shifts instead.)
		(- (ash (- l) n)))
	       (t
		(ash l n))))
	((> n 0)
	 (* l (expt 10. n)))
	((< n 0.)
	 (quotient l (expt 10. (- n))))
	(t l)))

;; Bignum LSH -- N is assumed (and declared above) to be a fixnum.
;; This isn't really LSH, since the sign bit isn't propagated when
;; shifting to the right, i.e. (BIGLSH -100 -3) = -40, whereas
;; (LSH -100 -3) = 777777777770 (on a 36 bit machine).
;; This actually computes (* X (EXPT 2 N)).  As of 12/21/80, this function
;; was only called by FPSHIFT.  I would like to hear an argument as why this
;; is more efficient than simply writing (* X (EXPT 2 N)).  Is the
;; intermediate result created by (EXPT 2 N) the problem?  I assume that
;; EXPT tries to LSH when possible.

(defun biglsh (x n)
  (cond ((and (not (bignump x))
	      (< n #.(- +machine-fixnum-precision+)))
	 0)
	;; Either we are shifting a fixnum to the right, or shifting
	;; a fixnum to the left, but not far enough left for it to become
	;; a bignum.
	((and (not (bignump x))
	      (or (<= n 0)
		  (< (+ (integer-length x) n) #.+machine-fixnum-precision+)))
	 ;; The form which follows is nearly identical to (ASH X N), however
	 ;; (ASH -100 -20) = -1, whereas (BIGLSH -100 -20) = 0.
	 (if (>= x 0)
	     (ash x n)
	     (- (biglsh (- x) n)))) ;(- x) may be a bignum even is x is a fixnum.
	;; If we get here, then either X is a bignum or our answer is
	;; going to be a bignum.
	((< n 0)
	 (cond ((> (abs n) (integer-length x)) 0)
	       ((> x 0)
		(hipart x (+ (integer-length x) n)))
	       (t (- (hipart x (+ (integer-length x) n))))))
	((= n 0) x)
	;; Isn't this the kind of optimization that compilers are
	;; supposed to make?
	((< n #.(1- +machine-fixnum-precision+)) (* x (ash 1 n)))
	(t (* x (expt 2 n)))))


;; exp(x)
;;
;; For negative x, use exp(-x) = 1/exp(x)
;;
;; For x > 0, exp(x) = exp(r+y) = exp(r) * exp(y), where x = r + y and
;; r = floor(x).
(defun fpexp (x)
  (prog (r s)
     (unless (signp ge (car x))
       (return (fpquotient (fpone) (fpexp (fpabs x)))))
     (setq r (fpintpart x :skip-exponent-check-p t))
     (return (cond ((< r 2)
		    (fpexp1 x))
		   (t
		    (setq s (fpexp1 (fpdifference x (intofp r))))
		    (fptimes* s
			      (cdr (bigfloatp
				    (let ((fpprec (+ fpprec (integer-length r) -1))
					  (r r))
				      (bcons (fpexpt (fpe) r))))))))))) ; patch for full precision %E

;; exp(x) for small x, using Taylor series.
(defun fpexp1 (x)
  (prog (term ans oans)
     (setq ans (setq term (fpone)))
     (do ((n 1 (1+ n)))
	 ((equal ans oans))
       (setq term (fpquotient (fptimes* x term) (intofp n)))
       (setq oans ans)
       (setq ans (fpplus ans term)))
     (return ans)))

;; Does one higher precision to round correctly.
;; A and B are each a list of a mantissa and an exponent.
(defun fpquotient (a b)
  (cond ((equal (car b) 0)
	 (merror (intl:gettext "pquotient: attempted quotient by zero.")))
	((equal (car a) 0) '(0 0))
	(t (list (fpround (quotient (fpshift (car a) (+ 3 fpprec)) (car b)))
		 (+ -3 (- (cadr a) (cadr b)) *m)))))

(defun fpgreaterp (a b)
  (fpposp (fpdifference a b)))

(defun fplessp (a b)
  (fpposp (fpdifference b a)))

(defun fpposp (x)
  (> (car x) 0))

(defun fpmin (arg1 &rest args)
  (let ((min arg1))
    (mapc #'(lambda (u) (if (fplessp u min) (setq min u))) args)
    min))

(defun fpmax (arg1 &rest args)
  (let ((max arg1))
    (mapc #'(lambda (u) (if (fpgreaterp u max) (setq max u))) args)
    max))

;; The following functions compute bigfloat values for %e, %pi,
;; %gamma, and log(2).  For each precision, the computed value is
;; cached in a hash table so it doesn't need to be computed again.
;; There are functions to return the hash table or clear the hash
;; table, for debugging.
;;
;; Note that each of these return a bigfloat number, but without the
;; bigfloat tag.
;;
;; See
;; https://sourceforge.net/p/maxima/bugs/1842/
;; for an explanation.
(let ((table (make-hash-table)))
  (defun fpe ()
    (let ((value (gethash fpprec table)))
      (if value
	  value
	  (setf (gethash fpprec table) (cdr (fpe1))))))
  (defun fpe-table ()
    table)
  (defun clear_fpe_table ()
    (clrhash table)))

(let ((table (make-hash-table)))
  (defun fppi ()
    (let ((value (gethash fpprec table)))
      (if value
	  value
	  (setf (gethash fpprec table) (cdr (fppi1))))))
  (defun fppi-table ()
    table)
  (defun clear_fppi_table ()
    (clrhash table)))

(let ((table (make-hash-table)))
  (defun fpgamma ()
    (let ((value (gethash fpprec table)))
      (if value
	  value
	  (setf (gethash fpprec table) (cdr (fpgamma1))))))
  (defun fpgamma-table ()
    table)
  (defun clear_fpgamma_table ()
    (clrhash table)))

(let ((table (make-hash-table)))
  (defun fplog2 ()
    (let ((value (gethash fpprec table)))
      (if value
	  value
	  (setf (gethash fpprec table) (comp-log2)))))
  (defun fplog2-table ()
    table)
  (defun clear_fplog2_table ()
    (clrhash table)))

;; This doesn't need a hash table because there's never a problem with
;; using a high precision value and rounding to a lower precision
;; value because 1 is always an exact bfloat.
(defun fpone ()
  (cond (*decfp (intofp 1))
	((= fpprec (bigfloat-prec bigfloatone)) (cdr bigfloatone))
	(t (intofp 1))))

;;----------------------------------------------------------------------------;;
;;
;; The values of %e, %pi, %gamma and log(2) are computed by the technique of 
;; binary splitting. See http://www.ginac.de/CLN/binsplit.pdf for details.
;;
;; Volker van Nek, Sept. 2014

;;
;; Euler's number E
;;
(defun fpe1 ()
  (let ((e (compe (+ fpprec 12))))               ;; compute additional bits 
    (bcons (list (fpround (car e)) (cadr e))) )) ;; round to fpprec
;; 
;; Taylor: %e = sum(s[i] ,i,0,inf) where s[i] = 1/i!
;;
(defun compe (prec) 
  (let ((fpprec prec))
    (multiple-value-bind (tt qq) (split-taylor-e 0 (taylor-e-size prec))
      (fpquotient (intofp tt) (intofp qq)) )))
;;
;; binary splitting:
;;
;;                  1
;; s[i] = ----------------------
;;        q[0]*q[1]*q[2]*..*q[i]
;; 
;;  where q[0] = 1 
;;        q[i] = i 
;; 
(defun split-taylor-e (i j)
  (let (qq tt)
    (if (= (- j i) 1) 
      (setq qq (if (= i 0) 1 i) 
            tt 1 )
      (let ((m (ash (+ i j) -1)))
        (multiple-value-bind (tl ql) (split-taylor-e i m)
          (multiple-value-bind (tr qr) (split-taylor-e m j)
            (setq qq (* ql qr)
                  tt (+ (* qr tl) tr) )))))
    (values tt qq) ))
;;
;;   stop when i! > 2^fpprec
;;
;;   log(i!) = sum(log(k), k,1,i) > fpprec * log(2)
;;
(defun taylor-e-size (prec)
  (let ((acc 0)
        (lim (* prec (log 2))) )
    (do ((i 1 (1+ i))) 
        ((> acc lim) i)
      (incf acc (log i)) )))
;;
;;----------------------------------------------------------------------------;;
;;
;; PI
;;
(defun fppi1 ()
  (let ((pi1 (comppi (+ fpprec 10))))
    (bcons (list (fpround (car pi1)) (cadr pi1))) ))
;;
;; Chudnovsky & Chudnovsky:
;;
;; C^(3/2)/(12*%pi) = sum(s[i], i,0,inf), 
;;
;;    where s[i] = (-1)^i*(6*i)!*(A*i+B) / (i!^3*(3*i)!*C^(3*i))
;;
;;       and A = 545140134, B = 13591409, C = 640320
;;
(defun comppi (prec) 
  (let ((fpprec prec)
        nr n d oldn tt qq n*qq )
    ;; STEP 1:
    ;; compute n/d = sqrt(10005) :
    ;;
    ;;                         n[0]   n[i+1] = n[i]^2+a*d[i]^2            n[inf]
    ;; quadratic Heron: x[0] = ----,                          , sqrt(a) = ------
    ;;                         d[0]   d[i+1] = 2*n[i]*d[i]                d[inf]
    ;;
    (multiple-value-setq (nr n d) (sqrt-10005-constants fpprec))
    (dotimes (i nr)
      (setq oldn n
            n (+ (* n n) (* 10005 d d))
            d (* 2 oldn d) ))
    ;; STEP 2:
    ;; divide C^(3/2)/12 = 3335*2^7*sqrt(10005) 
    ;;   by Chudnovsky-sum = tt/qq :
    ;;
    (setq nr (ceiling (* fpprec 0.021226729578153))) ;; nr of summands
                      ;; fpprec*log(2)/log(C^3/(24*6*2*6))
    (multiple-value-setq (tt qq) (split-chudnovsky 0 (1+ nr)))
    (setq n (* 3335 n)
          n*qq (intofp (* n qq)) )
    (fpquotient (list (car n*qq) (+ (cadr n*qq) 7))
                (intofp (* d tt)) )))
;;
;; The returned n and d serve as start values for the iteration. 
;; n/d = sqrt(10005) with a precision of p = ceiling(prec/2^nr) bits 
;;     where nr is the number of needed iterations.
;;
(defun sqrt-10005-constants (prec)
  (let (ilen p nr n d)
    (if (< prec 128)
      (setq nr 0 p prec)
      (setq ilen (integer-length prec)
            nr (- ilen 7)
            p (ceiling (* prec (expt 2.0 (- nr)))) ))
    (cond 
      ((<= p  76) (setq n         256192036001 d         2561280120))
      ((<= p  89) (setq n       51244811200700 d       512320048001))
      ((<= p 102) (setq n     2050048640064001 d     20495363200160))
      ((<= p 115) (setq n   410060972824000900 d   4099584960080001))
      (t          (setq n 16404488961600100001 d 164003893766400200)) )
    (values nr n d) ))
;;
;; binary splitting:
;; 
;;        a[i] * p[0]*p[1]*p[2]*..*p[i]
;; s[i] = -----------------------------
;;               q[0]*q[1]*q[2]*..*q[i]
;; 
;;  where a[0] = B 
;;        p[0] = q[0] = 1 
;;        a[i] = A*i+B
;;        p[i] = - (6*i-5)*(2*i-1)*(6*i-1)
;;        q[i] = C^3/24*i^3
;;
(defun split-chudnovsky (i j)
  (let (aa pp/qq pp qq tt)
    (if (= (- j i) 1) 
      (if (= i 0) 
        (setq aa 13591409 pp 1 qq 1 tt aa)
        (setq aa (+ (* i 545140134) 13591409)
              pp/qq (/ (* (- 5 (* 6 i)) (- (* 2 i) 1) (- (* 6 i) 1)) 
                       10939058860032000 ) ; C^3/24
              pp (numerator pp/qq)
              qq (* (denominator pp/qq) (expt i 3))
              tt (* aa pp) ))
      (let ((m (ash (+ i j) -1)))
        (multiple-value-bind (tl ql pl) (split-chudnovsky i m)
          (multiple-value-bind (tr qr pr) (split-chudnovsky m j)
            (setq pp (* pl pr)
                  qq (* ql qr)
                  tt (+ (* qr tl) (* pl tr)) )))))
    (values tt qq pp) ))
;;
;;----------------------------------------------------------------------------;;
;;
;; Euler-Mascheroni constant GAMMA
;;
(defun fpgamma1 ()
  (let ((res (comp-bf%gamma (+ fpprec 14))))
    (bcons (list (fpround (car res)) (cadr res))) ))
;;
;; Brent-McMillan algorithm
;;
;; Let
;;       alpha = 4.970625759544 
;;
;;       n > 0 and N-1 >= alpha*n
;;
;;       H(k) = sum(1/i, i,1,k)
;;
;;       S = sum(H(k)*(n^k/k!)^2, k,0,N-1)
;; 
;;       I = sum((n^k/k!)^2, k,0,N-1)
;;
;;       T = 1/(4*n)*sum((2*k)!^3/(k!^4*(16*n)^(2*k)), k,0,2*n-1)
;;
;; and
;;       %gamma = S/I - T/I^2 - log(n)
;;
;; Then 
;;       |%gamma - gamma| < 24 * e^(-8*n)
;;
;; (Corollary 2, Remark 2, Brent/Johansson http://arxiv.org/pdf/1312.0039v1.pdf)
;;
(defun comp-bf%gamma (prec)
  (let* ((fpprec prec)
         (n (ceiling (* 1/8 (+ (* prec (log 2.0)) (log 24.0)))))
         (n2 (* n n))
         (alpha 4.970625759544)
         (lim (ceiling (* alpha n))) 
          sums/sumi    ;; S/I
          sumi sumi2   ;; I and I^2
          sumt/sumi2 ) ;; T/I^2
    (multiple-value-bind (vv tt qq dd) (split-gamma-1 1 (1+ lim) n2) 
      ;;
      ;; sums      = vv/(qq*dd)
      ;; sumi      = tt/qq
      ;; sums/sumi = vv/(qq*dd)*qq/tt = vv/(dd*tt)
      ;;
      (setq sums/sumi (fpquotient (intofp vv) (intofp (* dd tt))) 
            sumi      (fpquotient (intofp tt) (intofp qq))
            sumi2     (fptimes* sumi sumi) )
      ;;
      (multiple-value-bind (ttt qqq) (split-gamma-2 0 (* 2 n) (* 32 n2))
        ;;
        ;; sumt       = 1/(4*n)*ttt/qqq 
        ;; sumt/sumi2 = ttt/(4*n*qqq*sumi2)
        ;;
        (setq sumt/sumi2 (fpquotient (intofp ttt) 
                                     (fptimes* (intofp (* 4 n qqq)) sumi2) ))
        ;; %gamma :
        (fpdifference sums/sumi (fpplus sumt/sumi2 (log-n n)) )))))
;;
;; split S and I simultaneously:
;;
;; summands I[0] = 1, I[i]/I[i-1] = n^2/i^2
;;
;;          S[0] = 0, S[i]/S[i-1] = n^2/i^2*H(i)/H(i-1)
;;
;;        p[0]*p[1]*p[2]*..*p[i]
;; I[i] = ----------------------
;;        q[0]*q[1]*q[2]*..*q[i]
;; 
;;  where p[0] = n^2 
;;        q[0] = 1 
;;        p[i] = n^2
;;        q[i] = i^2
;;                                   c[0]   c[1]   c[2]        c[i]
;; S[i] = H[i] * I[i],  where H[i] = ---- + ---- + ---- + .. + ----
;;                                   d[0]   d[1]   d[2]        d[i]
;;    and c[0] = 0 
;;        d[0] = 1 
;;        c[i] = 1
;;        d[i] = i
;;
(defun split-gamma-1 (i j n2) 
  (let (pp cc dd qq tt vv)
    (cond 
      ((= (- j i) 1) 
        (if (= i 1) ;; S[0] is 0 -> start with i=1 and add I[0]=1 to tt :
          (setq  pp n2  cc 1  dd 1  qq 1        tt (1+ n2)  vv n2)  
          (setq  pp n2  cc 1  dd i  qq (* i i)  tt pp       vv tt) ))
      (t 
        (let* ((m (ash (+ i j) -1)) tmp) 
          (multiple-value-bind (vl tl ql dl cl pl) (split-gamma-1 i m n2)
            (multiple-value-bind (vr tr qr dr cr pr) (split-gamma-1 m j n2)
              (setq pp (* pl pr)
                    cc (+ (* cl dr) (* dl cr))
                    dd (* dl dr)
                    qq (* ql qr)
                    tmp (* pl tr)
                    tt (+ (* tl qr) tmp)
                    vv (+ (* dr (+ (* vl qr) (* cl tmp))) (* dl pl vr)) ))))))
    (values vv tt qq dd cc pp) ))
;;
;; split 4*n*T:
;;
;; summands T[0] = 1, T[i]/T[i-1] = (2*i-1)^3/(32*i*n^2)
;; 
;;        p[0]*p[1]*p[2]*..*p[i]
;; T[i] = ----------------------
;;        q[0]*q[1]*q[2]*..*q[i]
;; 
;;  where p[0] = q[0] = 1
;;        p[i] = (2*i-1)^3
;;        q[i] = 32*i*n^2
;;
(defun split-gamma-2 (i j n2*32) 
  (let (pp qq tt)
    (cond 
      ((= (- j i) 1) 
        (if (= i 0) 
          (setq  pp 1                      qq 1            tt 1)
          (setq  pp (expt (1- (* 2 i)) 3)  qq (* i n2*32)  tt pp) ))
      (t 
        (let* ((m (ash (+ i j) -1))) 
          (multiple-value-bind (tl ql pl) (split-gamma-2 i m n2*32)
            (multiple-value-bind (tr qr pr) (split-gamma-2 m j n2*32)
              (setq pp (* pl pr)
                    qq (* ql qr)
                    tt (+ (* tl qr) (* pl tr)) ))))))
    (values tt qq pp) ))
;;
;;----------------------------------------------------------------------------;;
;;
;; log(2) = 18*L(26) - 2*L(4801) + 8*L(8749)
;;
;;   where L(k) = atanh(1/k)
;;
;;   see http://numbers.computation.free.fr/Constants/constants.html
;;
;;;(defun $log2 () (bcons (comp-log2))) ;; checked against reference table
;;
(defun comp-log2 ()
  (let ((res
         (let ((fpprec (+ fpprec 12)))
           (fpplus 
             (fpdifference (n*atanh-1/k 18 26) (n*atanh-1/k 2 4801))
             (n*atanh-1/k 8 8749) ))))
    (list (fpround (car res)) (cadr res)) ))
;;
;; Taylor: atanh(1/k) = sum(s[i], i,0,inf)
;;
;;    where s[i] = 1/((2*i+1)*k^(2*i+1))
;;
(defun n*atanh-1/k (n k) ;; integer n,k
  (let* ((k2 (* k k))
         (nr (ceiling (* fpprec (/ (log 2) (log k2))))) )
      (multiple-value-bind (tt qq bb) (split-atanh-1/k 0 (1+ nr) k k2)
        (fpquotient (intofp (* n tt)) (intofp (* bb qq))) )))
;;
;; binary splitting:
;;                      1
;; s[i] = -----------------------------
;;        b[i] * q[0]*q[1]*q[2]*..*q[i]
;; 
;;  where b[0] = 1 
;;        q[0] = k
;;        b[i] = 2*i+1
;;        q[i] = k^2
;;
(defun split-atanh-1/k (i j k k2)
  (let (bb qq tt)
    (if (= (- j i) 1) 
      (if (= i 0) 
        (setq  bb 1             qq k   tt 1)
        (setq  bb (1+ (* 2 i))  qq k2  tt 1) )
      (let ((m (ash (+ i j) -1)))
        (multiple-value-bind (tl ql bl) (split-atanh-1/k i m k k2)
          (multiple-value-bind (tr qr br) (split-atanh-1/k m j k k2)
            (setq bb (* bl br)
                  qq (* ql qr)
                  tt (+ (* br qr tl) (* bl tr)) )))))
    (values tt qq bb) ))
;;
;;----------------------------------------------------------------------------;;
;;
;; log(n) = log(n/2^k) + k*log(2)
;;
;;;(defun $log10 () (bcons (log-n 10))) ;; checked against reference table
;;
(defun log-n (n) ;; integer n > 0
  (cond 
    ((= 1 n) (list 0 0))
    ((= 2 n) (comp-log2))
    (t
      (let ((res 
             (let ((fpprec (+ fpprec 10))
                   (k (integer-length n)) )
               ;; choose k so that |n/2^k - 1| is as small as possible:
               (when (< n (* (coerce 2/3 'flonum) (ash 1 k))) (decf k))
               ;; now |n/2^k - 1| <= 1/3
               (fpplus (log-u/2^k n k fpprec) 
                       (fptimes* (intofp k) (comp-log2)) ))))
        (list (fpround (car res)) (cadr res)) ))))
;;
;; log(1+u/v)  = 2 * sum(s[i], i,0,inf)
;;
;;   where s[i] = (u/(2*v+u))^(2*i+1)/(2*i+1)
;;
(defun log-u/2^k (u k prec) ;; integer u k; x = u/2^k; |x - 1| < 1
  (setq u (- u (ash 1 k)))  ;; x <-- x - 1
  (cond 
    ((= 0 u) (list 0 0))
    (t
      (while (evenp u) (setq u (ash u -1)) (decf k))
      (let* ((u2 (* u u))
             (w (+ u (ash 2 k)))
             (w2 (* w w))
             (nr (ceiling (* prec (/ (log 2) 2 (log (abs (/ w u)))))))
              lg/2 )
        (multiple-value-bind (tt qq bb) (split-log-1+u/v 0 (1+ nr) u u2 w w2)
          (setq lg/2 (fpquotient (intofp tt) (intofp (* bb qq)))) ;; sum
          (list (car lg/2) (1+ (cadr lg/2))) )))))                ;; 2*sum
;;
;; binary splitting:
;;
;;               p[0]*p[1]*p[2]*..*p[i]
;; s[i] = -----------------------------
;;        b[i] * q[0]*q[1]*q[2]*..*q[i]
;; 
;;  where b[0] = 1 
;;        p[0] = u
;;        q[0] = w = 2*v+u
;;        b[i] = 2*i+1
;;        p[i] = u^2 
;;        q[i] = w^2
;;
(defun split-log-1+u/v (i j u u2 w w2)
  (let (pp bb qq tt)
    (if (= (- j i) 1) 
      (if (= i 0) 
        (setq  pp u   bb 1             qq w   tt u)
        (setq  pp u2  bb (1+ (* 2 i))  qq w2  tt pp) )
      (let ((m (ash (+ i j) -1))) 
        (multiple-value-bind (tl ql bl pl) (split-log-1+u/v i m u u2 w w2)
          (multiple-value-bind (tr qr br pr) (split-log-1+u/v m j u u2 w w2)
            (setq bb (* bl br)
                  pp (* pl pr)
                  qq (* ql qr)
                  tt (+ (* br qr tl) (* bl pl tr)) )))))
    (values tt qq bb pp) ))
;;
;;----------------------------------------------------------------------------;;


(defun fpdifference (a b)
  (fpplus a (fpminus b)))

(defun fpminus (x)
  (if (equal (car x) 0)
      x
      (list (- (car x)) (cadr x))))

(defun fpplus (a b)
  (prog (*m exp man sticky)
     (setq *cancelled 0)
     (cond ((equal (car a) 0) (return b))
	   ((equal (car b) 0) (return a)))
     (setq exp (- (cadr a) (cadr b)))
     (setq man (cond ((equal exp 0)
		      (setq sticky 0)
		      (fpshift (+ (car a) (car b)) 2))
		     ((> exp 0)
		      (setq sticky (hipart (car b) (- 1 exp)))
		      (setq sticky (cond ((signp e sticky) 0)
					 ((signp l (car b)) -1)
					 (t 1)))
					; COMPUTE STICKY BIT
		      (+ (fpshift (car a) 2)
					; MAKE ROOM FOR GUARD DIGIT & STICKY BIT
			    (fpshift (car b) (- 2 exp))))
		     (t (setq sticky (hipart (car a) (1+ exp)))
			(setq sticky (cond ((signp e sticky) 0)
					   ((signp l (car a)) -1)
					   (t 1)))
			(+ (fpshift (car b) 2)
			      (fpshift (car a) (+ 2 exp))))))
     (setq man (+ man sticky))
     (return (cond ((equal man 0) '(0 0))
		   (t (setq man (fpround man))
		      (setq exp (+ -2 *m (max (cadr a) (cadr b))))
		      (list man exp))))))

(defun fptimes* (a b)
  (if (or (zerop (car a)) (zerop (car b)))
      '(0 0)
      (list (fpround (* (car a) (car b)))
	    (+ *m (cadr a) (cadr b) (- fpprec)))))

;; Don't use the symbol BASE since it is SPECIAL.

(defun fpintexpt (int nn fixprec)	;INT is integer
  (setq fixprec (truncate fixprec (1- (integer-length int)))) ;NN is pos
  (let ((bas (intofp (expt int (min nn fixprec)))))
    (if (> nn fixprec)
	(fptimes* (intofp (expt int (rem nn fixprec)))
		  (fpexpt bas (quotient nn fixprec)))
	bas)))

;; NN is positive or negative integer

(defun fpexpt (p nn)
  (cond ((zerop nn) (fpone))
	((eql nn 1) p)
	((< nn 0) (fpquotient (fpone) (fpexpt p (- nn))))
	(t (prog (u)
	      (if (oddp nn)
		  (setq u p)
		  (setq u (fpone)))
	      (do ((ii (quotient nn 2) (quotient ii 2)))
		  ((zerop ii))
		(setq p (fptimes* p p))
		(when (oddp ii)
		  (setq u (fptimes* u p))))
	      (return u)))))

(defun exptbigfloat (p n)
  (cond ((equal n 1) p)
	((equal n 0) ($bfloat 1))
	((not ($bfloatp p)) (list '(mexpt) p n))
	((equal (cadr p) 0) ($bfloat 0))
	((and (< (cadr p) 0) (ratnump n))
	 (mul2 (let ($numer $float $keepfloat $ratprint)
		 (power -1 n))
	       (exptbigfloat (bcons (fpminus (cdr p))) n)))
	((and (< (cadr p) 0) (not (integerp n)))
	 (cond ((or (equal n 0.5) (equal n bfhalf))
		(exptbigfloat p '((rat simp) 1 2)))
	       ((or (equal n -0.5) (equal n bfmhalf))
		(exptbigfloat p '((rat simp) -1 2)))
	       (($bfloatp (setq n ($bfloat n)))
		(cond ((equal n ($bfloat (fpentier n)))
		       (exptbigfloat p (fpentier n)))
		      (t ;; for P<0: P^N = (-P)^N*cos(pi*N) + i*(-P)^N*sin(pi*N)
		       (setq p (exptbigfloat (bcons (fpminus (cdr p))) n)
			     n ($bfloat `((mtimes) $%pi ,n)))
		       (add2 ($bfloat `((mtimes) ,p ,(*fpsin n nil)))
			     `((mtimes simp) ,($bfloat `((mtimes) ,p ,(*fpsin n t)))
			       $%i)))))
	       (t (list '(mexpt) p n))))
	((and (ratnump n) (< (caddr n) 10.))
	 (bcons (fpexpt (fproot p (caddr n)) (cadr n))))
	((not (integerp n))
	 (setq n ($bfloat n))
	 (cond
	   ((not ($bfloatp n)) (list '(mexpt) p n))
	   (t
	    (let ((extrabits (max 1 (+ (caddr n) (integer-length (caddr p))))))
	      (setq p
		    (let ((fpprec (+ extrabits fpprec)))
		      (fpexp (fptimes* (cdr (bigfloatp n)) (fplog (cdr (bigfloatp p)))))))
	      (setq p (list (fpround (car p)) (+ (- extrabits) *m (cadr p))))
	      (bcons p)))))
	;; The number of extra bits required
	((< n 0) (invertbigfloat (exptbigfloat p (- n))))
	(t (bcons (fpexpt (cdr p) n)))))

(defun fproot (a n)  ; computes a^(1/n)  see Fitch, SIGSAM Bull Nov 74

  ;; Special case for a = 0b0. General algorithm loops endlessly in that case.

  ;; Unlike many or maybe all of the other functions named FP-something,
  ;; FPROOT assumes it is called with an argument like
  ;; '((BIGFLOAT ...) FOO BAR) instead of '(FOO BAR).
  ;; However FPROOT does return something like '(FOO BAR).

  (if (eql (cadr a) 0)
      '(0 0)
      (progn
	(let* ((ofprec fpprec)
	       (fpprec (+ fpprec 2))	;assumes a>0 n>=2
	       (bk (fpexpt (intofp 2) (1+ (quotient (cadr (setq a (cdr (bigfloatp a)))) n)))))
	  (do ((x bk (fpdifference x
				   (setq bk (fpquotient (fpdifference
							 x (fpquotient a (fpexpt x n1))) n))))
	       (n1 (1- n))
	       (n (intofp n)))
	      ((or (equal bk '(0 0))
		   (> (- (cadr x) (cadr bk)) ofprec))
	       (setq a x))))
	(list (fpround (car a)) (+ -2 *m (cadr a))))))

(defun timesbigfloat (h)
  (prog (fans r nfans)
     (setq fans (bcons (fpone)) nfans 1)
     (do ((l h (cdr l)))
	 ((null l))
       (if (setq r (bigfloatp (car l)))
	   (setq fans (bcons (fptimes* (cdr r) (cdr fans))))
	   (setq nfans (list '(mtimes) (car l) nfans))))
     (return (if (equal nfans 1)
		 fans
		 (simplify (list '(mtimes) fans nfans))))))

(defun invertbigfloat (a)
  ;; If A is a bigfloat, be sure to round it to the current precision.
  ;; (See Bug 2543079 for one of the symptoms.)
  (let ((b (bigfloatp a)))
    (if b
	(bcons (fpquotient (fpone) (cdr b)))
	(simplify (list '(mexpt) a -1)))))

(defun *fpexp (a)
  (fpend (let ((fpprec (+ 8. fpprec)))
           (if ($bfloatp a)
               (fpexp (cdr (bigfloatp a)))
	       (list '(mexpt) '$%e a)))))

(defun *fpsin (a fl)
  (fpend (let ((fpprec (+ 8. fpprec)))
	   (cond (($bfloatp a) (fpsin (cdr ($bfloat a)) fl))
		 (fl (list '(%sin) a))
		 (t (list '(%cos) a))))))

(defun fpend (a)
  (cond ((equal (car a) 0) (bcons a))
	((numberp (car a))
	 (setq a (list (fpround (car a)) (+ -8. *m (cadr a))))
	 (bcons a))
	(t a)))

(defun fparcsimp (e)   ; needed for e.g. ASIN(.123567812345678B0) with
  ;; FPPREC 16, to get rid of the miniscule imaginary
  ;; part of the a+bi answer.
  (if (and (mplusp e) (null (cdddr e))
	   (mtimesp (caddr e)) (null (cdddr (caddr e)))
	   ($bfloatp (cadr (caddr e)))
	   (eq (caddr (caddr e)) '$%i)
	   (< (caddr (cadr (caddr e))) (+ (- fpprec) 2)))
      (cadr e)
      e))

(defun sinbigfloat (x)
  (*fpsin (car x) t))

(defun cosbigfloat (x)
  (*fpsin (car x) nil))

;; THIS VERSION OF FPSIN COMPUTES SIN OR COS TO PRECISION FPPREC,
;; BUT CHECKS FOR THE POSSIBILITY OF CATASTROPHIC CANCELLATION DURING
;; ARGUMENT REDUCTION (E.G. SIN(N*%PI+EPSILON))
;; *FPSINCHECK* WILL CAUSE PRINTOUT OF ADDITIONAL INFO WHEN
;; EXTRA PRECISION IS NEEDED FOR SIN/COS CALCULATION.  KNOWN
;; BAD FEATURES:  IT IS NOT NECESSARY TO USE EXTRA PRECISION FOR, E.G.
;; SIN(PI/2), WHICH IS NOT NEAR ZERO, BUT  EXTRA
;; PRECISION IS USED SINCE IT IS NEEDED FOR COS(PI/2).
;; PRECISION SEEMS TO BE 100% SATSIFACTORY FOR LARGE ARGUMENTS, E.G.
;; SIN(31415926.0B0), BUT LESS SO FOR SIN(3.1415926B0).  EXPLANATION
;; NOT KNOWN.  (9/12/75  RJF)

(defvar  *fpsincheck* nil)

;; FL is a T for sin and NIL for cos.
(defun fpsin (x fl)
  (prog (piby2 r sign res k *cancelled)
     (setq sign (cond (fl (signp g (car x)))
		      (t))
	   x (fpabs x))
     (when (equal (car x) 0)
       (return (if fl (intofp 0) (intofp 1))))
     (return
       (cdr
	(bigfloatp
	 (let ((fpprec (max fpprec (+ fpprec (cadr x))))
	       (xt (bcons x))
	       (*cancelled 0)
	       (oldprec fpprec))
	   (prog (x)
	    loop (setq x (cdr (bigfloatp xt)))
	    (setq piby2 (fpquotient (fppi) (intofp 2)))
	    (setq r (fpintpart (fpquotient x piby2) :skip-exponent-check-p t))
	    (setq x (fpplus x (fptimes* (intofp (- r)) piby2)))
	    (setq k *cancelled)
	    (fpplus x (fpminus piby2))
	    (setq *cancelled (max k *cancelled))
	    (when *fpsincheck*
	      (print `(*canc= ,*cancelled fpprec= ,fpprec oldprec= ,oldprec)))
	    (cond ((not (> oldprec (- fpprec *cancelled)))
		   (setq r (rem r 4))
		   (setq res
			 (cond (fl (cond ((= r 0) (fpsin1 x))
					 ((= r 1) (fpcos1 x))
					 ((= r 2) (fpminus (fpsin1 x)))
					 ((= r 3) (fpminus (fpcos1 x)))))
			       (t (cond ((= r 0) (fpcos1 x))
					((= r 1) (fpminus (fpsin1 x)))
					((= r 2) (fpminus (fpcos1 x)))
					((= r 3) (fpsin1 x))))))
		   (return (bcons (if sign res (fpminus res)))))
		  (t
		   (incf fpprec *cancelled)
		     (go loop))))))))))

(defun fpcos1 (x)
  (fpsincos1 x nil))

;; Compute SIN or COS in (0,PI/2).  FL is T for SIN, NIL for COS.
;;
;; Use Taylor series
(defun fpsincos1 (x fl)
  (prog (ans term oans x2)
     (setq ans (if fl x (intofp 1))
	   x2 (fpminus(fptimes* x x)))
     (setq term ans)
     (do ((n (if fl 3 2) (+ n 2)))
	 ((equal ans oans))
       (setq term (fptimes* term (fpquotient x2 (intofp (* n (1- n))))))
       (setq oans ans
	     ans (fpplus ans term)))
     (return ans)))

(defun fpsin1(x)
  (fpsincos1 x t))

(defun fpabs (x)
  (if (signp ge (car x))
      x
      (cons (- (car x)) (cdr x))))

(defun fpentier (f)
  (let ((fpprec (bigfloat-prec f)))
    (fpintpart (cdr f))))

;; Calculate the integer part of a floating point number that is represented as
;; a list
;;
;;    (MANTISSA EXPONENT)
;;
;; The special variable fpprec should be bound to the precision (in bits) of the
;; number. This encodes how many bits are known of the result and also a right
;; shift. The pair denotes the number MANTISSA * 2^(EXPONENT - FPPREC), of which
;; FPPREC bits are known.
;;
;; If EXPONENT is large and positive then we might not have enough
;; information to calculate the integer part. Specifically, we only
;; have enough information if EXPONENT < FPPREC. If that isn't the
;; case, we signal a Maxima error.  However, if SKIP-EXPONENT-CHECK-P
;; is non-NIL, this check is skipped, and we compute the integer part
;; as requested.
;;
;; For the bigfloat code here, skip-exponent-check-p should be true.
;; For other uses (see commit 576c7508 and bug #2784), this should be
;; nil, which is the default.
(defun fpintpart (f &key skip-exponent-check-p)
  (destructuring-bind (mantissa exponent)
      f
    (let ((m (- fpprec exponent)))
	(if (plusp m)
	    (quotient mantissa (expt 2 (- fpprec exponent)))
	    (if (and (not skip-exponent-check-p) (< exponent fpprec))
		(merror "~M doesn't have enough precision to compute its integer part"
                `((bigfloat ,fpprec) ,mantissa ,exponent))
		(* mantissa (expt 2 (- m))))))))

(defun logbigfloat (a)
  (cond (($bfloatp (car a))
	 (big-float-log ($bfloat (car a))))
	(t
	 (list '(%log) (car a)))))


;;; Computes the log of a bigfloat number.
;;;
;;; Uses the series
;;;
;;; log(1+x) = sum((x/(x+2))^(2*n+1)/(2*n+1),n,0,inf);
;;;
;;;
;;;                  INF      x   2 n + 1
;;;                  ====  (-----)
;;;                  \      x + 2
;;;          =  2     >    --------------
;;;                  /        2 n + 1
;;;                  ====
;;;                  n = 0
;;;
;;;
;;; which converges for x > 0.
;;;
;;; Note that FPLOG is given 1+X, not X.
;;;
;;; However, to aid convergence of the series, we scale 1+x until 1/e
;;; < 1+x <= e.
;;;
(defun fplog (x)
  (prog (over two ans oldans term e sum)
     (unless (> (car x) 0)
       (merror (intl:gettext "fplog: argument must be positive; found: ~M") (car x)))
     (setq e (fpe)
	   over (fpquotient (fpone) e)
	   ans 0)
     ;; Scale X until 1/e < X <= E.  ANS keeps track of how
     ;; many factors of E were used.  Set X to NIL if X is E.
     (do ()
	 (nil)
       (cond ((equal x e) (setq x nil) (return nil))
	     ((and (fplessp x e) (fplessp over x))
	      (return nil))
	     ((fplessp x over)
	      (setq x (fptimes* x e))
	      (decf ans))
	     (t
	      (incf ans)
	      (setq x (fpquotient x e)))))
     (when (null x) (return (intofp (1+ ans))))
     ;; Prepare X for the series.  The series is for 1 + x, so
     ;; get x from our X.  TERM is (x/(x+2)).  X becomes
     ;; (x/(x+2))^2.
     (setq x (fpdifference  x (fpone))
	   ans (intofp ans))
     (setq x (fpexpt (setq term (fpquotient x (fpplus x (setq two (intofp 2))))) 2))
     ;; Sum the series until the sum (in ANS) doesn't change
     ;; anymore.
     (setq sum (intofp 0))
     (do ((n 1 (+ n 2)))
	 ((equal sum oldans))
       (setq oldans sum)
       (setq sum (fpplus sum (fpquotient term (intofp n))))
       (setq term (fptimes* term x)))
     (return (fpplus ans (fptimes* two sum)))))

(defun mabsbigfloat (l)
  (prog (r)
     (setq r (bigfloatp (car l)))
     (return (if (null r)
		 (list '(mabs) (car l))
		 (bcons (fpabs (cdr r)))))))


;;;; Bigfloat implementations of special functions.
;;;;

;;; This is still a bit messy.  Some functions here take bigfloat
;;; numbers, represented by ((bigfloat) <mant> <exp>), but others want
;;; just the FP number, represented by (<mant> <exp>).  Likewise, some
;;; return a bigfloat, some return just the FP.
;;;
;;; This needs to be systemized somehow.  It isn't helped by the fact
;;; that some of the routines above also do the samething.
;;;
;;; The implementation for the special functions for a complex
;;; argument are mostly taken from W. Kahan, "Branch Cuts for Complex
;;; Elementary Functions or Much Ado About Nothing's Sign Bit", in
;;; Iserles and Powell (eds.) "The State of the Art in Numerical
;;; Analysis", pp 165-211, Clarendon Press, 1987

;; Compute exp(x) - 1, but do it carefully to preserve precision when
;; |x| is small.  X is a FP number, and a FP number is returned.  That
;; is, no bigfloat stuff.
(defun fpexpm1 (x)
  ;; What is the right breakpoint here?  Is 1 ok?  Perhaps 1/e is better?
  (cond ((fpgreaterp (fpabs x) (fpone))
	 ;; exp(x) - 1
	 (fpdifference (fpexp x) (fpone)))
	(t
	 ;; Use Taylor series for exp(x) - 1
	 (let ((ans x)
	       (oans nil)
	       (term x))
	   (do ((n 2 (1+ n)))
	       ((equal ans oans))
	     (setf term (fpquotient (fptimes* x term) (intofp n)))
	     (setf oans ans)
	     (setf ans (fpplus ans term)))
	   ans))))

;; log(1+x) for small x.  X is FP number, and a FP number is returned.
(defun fplog1p (x)
  ;; Use the same series as given above for fplog.  For small x we use
  ;; the series, otherwise fplog is accurate enough.
  (cond ((fpgreaterp (fpabs x) (fpone))
	 (fplog (fpplus x (fpone))))
	(t
	 (let* ((sum (intofp 0))
		(term (fpquotient x (fpplus x (intofp 2))))
		(f (fptimes* term term))
		(oldans nil))
	   (do ((n 1 (+ n 2)))
	       ((equal sum oldans))
	     (setq oldans sum)
	     (setq sum (fpplus sum (fpquotient term (intofp n))))
	     (setq term (fptimes* term f)))
	   (fptimes* sum (intofp 2))))))

;; sinh(x) for real x.  X is a bigfloat, and a bigfloat is returned.
(defun fpsinh (x)
  ;; X must be a maxima bigfloat

  ;; See, for example, Hart et al., Computer Approximations, 6.2.27:
  ;;
  ;; sinh(x) = 1/2*(D(x) + D(x)/(1+D(x)))
  ;;
  ;; where D(x) = exp(x) - 1.
  ;;
  ;; But for negative x, use sinh(x) = -sinh(-x) because D(x)
  ;; approaches -1 for large negative x.
  (cond ((equal 0 (cadr x))
         ;; Special case: x=0. Return immediately.
         (bigfloatp x))
        ((fpposp (cdr x))
         ;; x is positive.
         (let ((d (fpexpm1 (cdr (bigfloatp x)))))
           (bcons (fpquotient (fpplus d (fpquotient d (fpplus d (fpone))))
                              (intofp 2)))))
        (t
         ;; x is negative.
         (bcons 
           (fpminus (cdr (fpsinh (bcons (fpminus (cdr (bigfloatp x)))))))))))

(defun big-float-sinh (x &optional y)
  ;; The rectform for sinh for complex args should be numerically
  ;; accurate, so return nil in that case.
  (unless y
    (fpsinh x)))

;; asinh(x) for real x.  X is a bigfloat, and a bigfloat is returned.
(defun fpasinh (x)
  ;; asinh(x) = sign(x) * log(|x| + sqrt(1+x*x))
  ;;
  ;; And
  ;;
  ;; asinh(x) = x, if 1+x*x = 1
  ;;          = sign(x) * (log(2) + log(x)), large |x|
  ;;          = sign(x) * log(2*|x| + 1/(|x|+sqrt(1+x*x))), if |x| > 2
  ;;          = sign(x) * log1p(|x|+x^2/(1+sqrt(1+x*x))), otherwise.
  ;;
  ;; But I'm lazy right now and we only implement the last 2 cases.
  ;; We should implement all cases.
  (let* ((fp-x (cdr (bigfloatp x)))
	 (absx (fpabs fp-x))
	 (one (fpone))
	 (two (intofp 2))
	 (minus (minusp (car fp-x)))
	 result)
    ;; We only use two formulas here.  |x| <= 2 and |x| > 2.  Should
    ;; we add one for very big x and one for very small x, as given above.
    (cond ((fpgreaterp absx two)
	   ;; |x| > 2
	   ;;
	   ;; log(2*|x| + 1/(|x|+sqrt(1+x^2)))
	   (setf result (fplog (fpplus (fptimes* absx two)
				       (fpquotient one
						   (fpplus absx
							   (fproot (bcons (fpplus one
										  (fptimes* absx absx)))
							    2)))))))
	  (t
	   ;; |x| <= 2
	   ;;
	   ;; log1p(|x|+x^2/(1+sqrt(1+x^2)))
	   (let ((x*x (fptimes* absx absx)))
	     (setq result (fplog1p (fpplus absx
					   (fpquotient x*x
						       (fpplus one
							       (fproot (bcons (fpplus one x*x))
								       2)))))))))
    (if minus
	(bcons (fpminus result))
	(bcons result))))

(defun complex-asinh (x y)
  ;; asinh(z) = -%i * asin(%i*z)
  (multiple-value-bind (u v)
      (complex-asin (mul -1 y) x)
    (values v (bcons (fpminus (cdr u))))))

(defun big-float-asinh (x &optional y)
  (if y
      (multiple-value-bind (u v)
	  (complex-asinh x y)
	(add u (mul '$%i v)))
      (fpasinh x)))

(defun fpasin-core (x)
  ;; asin(x) = atan(x/(sqrt(1-x^2))
  ;;         = sgn(x)*[%pi/2 - atan(sqrt(1-x^2)/abs(x))]
  ;;
  ;; Use the first for  0 <= x < 1/2 and the latter for 1/2 < x <= 1.
  ;;
  ;; If |x| > 1, we need to do something else.
  ;;
  ;; asin(x) = -%i*log(sqrt(1-x^2)+%i*x)
  ;;         = -%i*log(%i*x + %i*sqrt(x^2-1))
  ;;         = -%i*[log(|x + sqrt(x^2-1)|) + %i*%pi/2]
  ;;         = %pi/2 - %i*log(|x+sqrt(x^2-1)|)

  (let ((fp-x (cdr (bigfloatp x))))
    (cond ((minusp (car fp-x))
	   ;; asin(-x) = -asin(x);
	   (mul -1 (fpasin (bcons (fpminus fp-x)))))
	  ((fplessp fp-x (cdr bfhalf))
	   ;; 0 <= x < 1/2
	   ;; asin(x) = atan(x/sqrt(1-x^2))
	   (bcons
	    (fpatan (fpquotient fp-x
				(fproot (bcons
					 (fptimes* (fpdifference (fpone) fp-x)
						   (fpplus (fpone) fp-x)))
					2)))))
	  ((fpgreaterp fp-x (fpone))
	   ;; x > 1
	   ;; asin(x) = %pi/2 - %i*log(|x+sqrt(x^2-1)|)
	   ;;
	   ;; Should we try to do something a little fancier with the
	   ;; argument to log and use log1p for better accuracy?
	   (let ((arg (fpplus fp-x
			      (fproot (bcons (fptimes* (fpdifference fp-x (fpone))
						       (fpplus fp-x (fpone))))
				      2))))
	     (add (div '$%pi 2)
		  (mul -1 '$%i (bcons (fplog arg))))))

	  (t
	   ;; 1/2 <= x <= 1
	   ;; asin(x) = %pi/2 - atan(sqrt(1-x^2)/x)
	   (add (div '$%pi 2)
		(mul -1
		     (bcons
		      (fpatan
		       (fpquotient (fproot (bcons (fptimes* (fpdifference (fpone) fp-x)
							    (fpplus (fpone) fp-x)))
					   2)
				   fp-x)))))))))

;; asin(x) for real x.  X is a bigfloat, and a maxima number (real or
;; complex) is returned.
(defun fpasin (x)
  ;; asin(x) = atan(x/(sqrt(1-x^2))
  ;;         = sgn(x)*[%pi/2 - atan(sqrt(1-x^2)/abs(x))]
  ;;
  ;; Use the first for  0 <= x < 1/2 and the latter for 1/2 < x <= 1.
  ;;
  ;; If |x| > 1, we need to do something else.
  ;;
  ;; asin(x) = -%i*log(sqrt(1-x^2)+%i*x)
  ;;         = -%i*log(%i*x + %i*sqrt(x^2-1))
  ;;         = -%i*[log(|x + sqrt(x^2-1)|) + %i*%pi/2]
  ;;         = %pi/2 - %i*log(|x+sqrt(x^2-1)|)

  ($bfloat (fpasin-core x)))

;; Square root of a complex number (xx, yy).  Both are bigfloats.  FP
;; (non-bigfloat) numbers are returned.
(defun complex-sqrt (xx yy)
  (let* ((x (cdr (bigfloatp xx)))
	 (y (cdr (bigfloatp yy)))
	 (rho (fpplus (fptimes* x x)
		      (fptimes* y y))))
    (setf rho (fpplus (fpabs x) (fproot (bcons rho) 2)))
    (setf rho (fpplus rho rho))
    (setf rho (fpquotient (fproot (bcons rho) 2) (intofp 2)))

    (let ((eta rho)
	  (nu y))
      (when (fpgreaterp rho (intofp 0))
	(setf nu (fpquotient (fpquotient nu rho) (intofp 2)))
	(when (fplessp x (intofp 0))
	  (setf eta (fpabs nu))
	  (setf nu (if (minusp (car y))
		       (fpminus rho)
		       rho))))
      (values eta nu))))

;; asin(z) for complex z = x + %i*y.  X and Y are bigfloats.  The real
;; and imaginary parts are returned as bigfloat numbers.
(defun complex-asin (x y)
  (let ((x (cdr (bigfloatp x)))
	(y (cdr (bigfloatp y))))
    (multiple-value-bind (re-sqrt-1-z im-sqrt-1-z)
	(complex-sqrt (bcons (fpdifference (intofp 1) x))
		      (bcons (fpminus y)))
      (multiple-value-bind (re-sqrt-1+z im-sqrt-1+z)
	  (complex-sqrt (bcons (fpplus (intofp 1) x))
			(bcons y))
	;; Realpart is atan(x/Re(sqrt(1-z)*sqrt(1+z)))
	;; Imagpart is asinh(Im(conj(sqrt(1-z))*sqrt(1+z)))
	(values (bcons
		 (let ((d (fpdifference (fptimes* re-sqrt-1-z
						  re-sqrt-1+z)
					(fptimes* im-sqrt-1-z
						  im-sqrt-1+z))))
		   ;; Check for division by zero.  If we would divide
		   ;; by zero, return pi/2 or -pi/2 according to the
		   ;; sign of X.
		   (cond ((equal d '(0 0))
			  (if (fplessp x '(0 0))
			      (fpminus (fpquotient (fppi) (intofp 2)))
			      (fpquotient (fppi) (intofp 2))))
			 (t
			  (fpatan (fpquotient x d))))))
		(fpasinh (bcons
			  (fpdifference (fptimes* re-sqrt-1-z
						  im-sqrt-1+z)
					(fptimes* im-sqrt-1-z
						  re-sqrt-1+z)))))))))

(defun big-float-asin (x &optional y)
  (if y
      (multiple-value-bind (u v) (complex-asin x y)
	(add u (mul '$%i v)))
      (fpasin x)))


;; tanh(x) for real x.  X is a bigfloat, and a bigfloat is returned.
(defun fptanh (x)
  ;; X is Maxima bigfloat
  ;; tanh(x) = D(2*x)/(2+D(2*x))
  (let* ((two (intofp 2))
	 (fp (cdr (bigfloatp x)))
	 (d (fpexpm1 (fptimes* fp two))))
    (bcons (fpquotient d (fpplus d two)))))

;; tanh(z), z = x + %i*y.  X, Y are bigfloats, and a maxima number is
;; returned.
(defun complex-tanh (x y)
  (let* ((tv (cdr (tanbigfloat (list y))))
	 (beta (fpplus (fpone) (fptimes* tv tv)))
	 (s (cdr (fpsinh x)))
	 (s^2 (fptimes* s s))
	 (rho (fproot (bcons (fpplus (fpone) s^2)) 2))
	 (den (fpplus (fpone) (fptimes* beta s^2))))
    (values (bcons (fpquotient (fptimes* beta (fptimes* rho s)) den))
	    (bcons (fpquotient tv den)))))

(defun big-float-tanh (x &optional y)
  (if y
      (multiple-value-bind (u v) (complex-tanh x y)
	(add u (mul '$%i v)))
      (fptanh x)))

;; atanh(x) for real x, |x| <= 1.  X is a bigfloat, and a bigfloat is
;; returned.
(defun fpatanh (x)
  ;; atanh(x) = -atanh(-x)
  ;;          = 1/2*log1p(2*x/(1-x)), x >= 0.5
  ;;          = 1/2*log1p(2*x+2*x*x/(1-x)), x <= 0.5

  (let* ((fp-x (cdr (bigfloatp x))))
    (cond ((fplessp fp-x (intofp 0))
	   ;; atanh(x) = -atanh(-x)
	   (mul -1 (fpatanh (bcons (fpminus fp-x)))))
	  ((fpgreaterp fp-x (fpone))
	   ;; x > 1, so use complex version.
	   (multiple-value-bind (u v)
	       (complex-atanh x (bcons (intofp 0)))
	     (add u (mul '$%i v))))
	  ((fpgreaterp fp-x (cdr bfhalf))
	   ;; atanh(x) = 1/2*log1p(2*x/(1-x))
	   (bcons
	    (fptimes* (cdr bfhalf)
		      (fplog1p (fpquotient (fptimes* (intofp 2) fp-x)
					   (fpdifference (fpone) fp-x))))))
	  (t
	   ;; atanh(x) = 1/2*log1p(2*x + 2*x*x/(1-x))
	   (let ((2x (fptimes* (intofp 2) fp-x)))
	     (bcons
	      (fptimes* (cdr bfhalf)
			(fplog1p (fpplus 2x
					 (fpquotient (fptimes* 2x fp-x)
						     (fpdifference (fpone) fp-x)))))))))))

;; Stuff which follows is derived from atanh z = (log(1 + z) - log(1 - z))/2
;; which apparently originates with Kahan's "Much ado" paper.

;; The formulas for eta and nu below can be easily derived from
;; rectform(atanh(x+%i*y)) =
;;
;; 1/4*log(((1+x)^2+y^2)/((1-x)^2+y^2)) + %i/2*(arg(1+x+%i*y)-arg(1-x+%i*(-y)))
;;
;; Expand the argument of log out and divide it out and we get
;;
;; log(((1+x)^2+y^2)/((1-x)^2+y^2)) = log(1+4*x/((1-x)^2+y^2))
;;
;; When y = 0, Im atanh z = 1/2 (arg(1 + x) - arg(1 - x))
;;                        = if x < -1 then %pi/2 else if x > 1 then -%pi/2 else <whatever>
;;
;; Otherwise, arg(1 - x + %i*(-y)) = - arg(1 - x + %i*y),
;; and Im atanh z = 1/2 (arg(1 + x + %i*y) + arg(1 - x + %i*y)).
;; Since arg(x)+arg(y) = arg(x*y) (almost), we can simplify the
;; imaginary part to
;;
;; arg((1+x+%i*y)*(1-x+%i*y)) = arg((1-x)*(1+x)-y^2+2*y*%i)
;; = atan2(2*y,((1-x)*(1+x)-y^2))
;;
;; These are the eta and nu forms below.
(defun complex-atanh (x y)
  (let* ((fpx (cdr (bigfloatp x)))
	 (fpy (cdr (bigfloatp y)))
	 (beta (if (minusp (car fpx))
		   (fpminus (fpone))
		   (fpone)))
     (x-lt-minus-1 (mevalp `((mlessp) ,x -1)))
     (x-gt-plus-1 (mevalp `((mgreaterp) ,x 1)))
     (y-equals-0 (like y '((bigfloat) 0 0)))
	 (x (fptimes* beta fpx))
	 (y (fptimes* beta (fpminus fpy)))
	 ;; Kahan has rho = 4/most-positive-float.  What should we do
	 ;; here about that?  Our big floats don't really have a
	 ;; most-positive float value.
	 (rho (intofp 0))
	 (t1 (fpplus (fpabs y) rho))
	 (t1^2 (fptimes* t1 t1))
	 (1-x (fpdifference (fpone) x))
	 ;; eta = log(1+4*x/((1-x)^2+y^2))/4
	 (eta (fpquotient
	       (fplog1p (fpquotient (fptimes* (intofp 4) x)
				    (fpplus (fptimes* 1-x 1-x)
					    t1^2)))
	       (intofp 4)))
     ;; If y = 0, then Im atanh z = %pi/2 or -%pi/2.
	 ;; Otherwise nu = 1/2*atan2(2*y,(1-x)*(1+x)-y^2)
	 (nu (if y-equals-0
	   ;; EXTRA FPMINUS HERE TO COUNTERACT FPMINUS IN RETURN VALUE
	   (fpminus (if x-lt-minus-1
			(cdr ($bfloat '((mquotient) $%pi 2)))
			(if x-gt-plus-1
			    (cdr ($bfloat '((mminus) ((mquotient) $%pi 2))))
			    (merror "COMPLEX-ATANH: HOW DID I GET HERE?"))))
	   (fptimes* (cdr bfhalf)
		       (fpatan2 (fptimes* (intofp 2) y)
				(fpdifference (fptimes* 1-x (fpplus (fpone) x))
					      t1^2))))))
    (values (bcons (fptimes* beta eta))
	;; WTF IS FPMINUS DOING HERE ??
	    (bcons (fpminus (fptimes* beta nu))))))

(defun big-float-atanh (x &optional y)
  (if y
      (multiple-value-bind (u v) (complex-atanh x y)
	(add u (mul '$%i v)))
      (fpatanh x)))

;; acos(x) for real x.  X is a bigfloat, and a maxima number is returned.
(defun fpacos (x)
  ;; acos(x) = %pi/2 - asin(x)
  ($bfloat (add (div '$%pi 2) (mul -1 (fpasin-core x)))))

(defun complex-acos (x y)
  (let ((x (cdr (bigfloatp x)))
	(y (cdr (bigfloatp y))))
    (multiple-value-bind (re-sqrt-1-z im-sqrt-1-z)
	(complex-sqrt (bcons (fpdifference (intofp 1) x))
		      (bcons (fpminus y)))
      (multiple-value-bind (re-sqrt-1+z im-sqrt-1+z)
	  (complex-sqrt (bcons (fpplus (intofp 1) x))
			(bcons y))
	(values (bcons
		 (fptimes* (intofp 2)
			   (fpatan (fpquotient re-sqrt-1-z re-sqrt-1+z))))
		(fpasinh (bcons
			  (fpdifference
			   (fptimes* re-sqrt-1+z im-sqrt-1-z)
			   (fptimes* im-sqrt-1+z re-sqrt-1-z)))))))))


(defun big-float-acos (x &optional y)
  (if y
      (multiple-value-bind (u v) (complex-acos x y)
	(add u (mul '$%i v)))
      (fpacos x)))

(defun complex-log (x y)
  (let* ((x (cdr (bigfloatp x)))
	 (y (cdr (bigfloatp y)))
	 (t1 (let (($float2bf t))
	       ;; No warning message, please.
	       (floattofp 1.2)))
	 (t2 (intofp 3))
	 (rho (fpplus (fptimes* x x)
		      (fptimes* y y)))
	 (abs-x (fpabs x))
	 (abs-y (fpabs y))
	 (beta (fpmax abs-x abs-y))
	 (theta (fpmin abs-x abs-y)))
    (values (if (or (fpgreaterp t1 beta)
		    (fplessp rho t2))
		(fpquotient (fplog1p (fpplus (fptimes* (fpdifference beta (fpone))
						       (fpplus beta (fpone)))
					     (fptimes* theta theta)))
			    (intofp 2))
		(fpquotient (fplog rho) (intofp 2)))
	    (fpatan2 y x))))

(defun big-float-log (x &optional y)
  (if y
      (multiple-value-bind (u v) (complex-log x y)
	(add (bcons u) (mul '$%i (bcons v))))
      (flet ((%log (x)
	       ;; x is (mantissa exp), where mantissa = frac*2^fpprec,
	       ;; with 1/2 < frac <= 1 and x is frac*2^exp.  To
	       ;; compute log(x), use log(x) = log(frac)+ exp*log(2).
	       (cdr
		(let* ((extra 8)
		       (fpprec (+ fpprec extra))
		       (log-frac
			(fplog #+nil
			       (cdr ($bfloat
				     (cl-rat-to-maxima (/ (car x)
							  (ash 1 (- fpprec 8))))))
			       (list (ash (car x) extra) 0)))
		       (log-exp (fptimes* (intofp (second x)) (fplog2)))
		       (result (bcons (fpplus log-frac log-exp))))
		  (let ((fpprec (- fpprec extra)))
		    (bigfloatp result))))))
	(let ((fp-x (cdr (bigfloatp x))))
	  (cond ((onep1 x)
		 ;; Special case for log(1).  See:
		 ;; https://sourceforge.net/p/maxima/bugs/2240/
		 (bcons (intofp 0)))
		((fplessp fp-x (intofp 0))
		 ;; ??? Do we want to return an exact %i*%pi or a float
		 ;; approximation?
		 (add (big-float-log (bcons (fpminus fp-x)))
		      (mul '$%i (bcons (fppi)))))
		(t
		 (bcons (%log fp-x))))))))

(defun big-float-sqrt (x &optional y)
  (if y
      (multiple-value-bind (u v) (complex-sqrt x y)
	(add (bcons u) (mul '$%i (bcons v))))
      (let ((fp-x (cdr (bigfloatp x))))
	(if (fplessp fp-x (intofp 0))
	    (mul '$%i (bcons (fproot (bcons (fpminus fp-x)) 2)))
	    (bcons (fproot x 2))))))

(eval-when
    (:load-toplevel :execute)
    (fpprec1 nil $fpprec))		; Set up user's precision
