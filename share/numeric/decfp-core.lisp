;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;

(in-package :maxima)
(declare-top (special *m  fpprec $fpprec $float $bfloat $ratprint $ratepsilon $domain $m1pbranch
		      $rounddecimalfloats machine-mantiss-precision))

(defvar $rounddecimalfloats nil)

;;; test case
#| 
sum:0$
for i: 1 thru 50 do sum:sum+decbfloat(9/10^i) ;
is (sum<1) ;  should be true
is (sum+1/10^50=1.0L0) ;  should be true

|#
;;; REVISED 1/2016 to use decimal fpprec, input, output  and inside.
;;;;Revised 2/2016 to allow BOTH decimal and binary
;;; RJF.
;;; Revised 3/19/2016 to allow printing of decimal bigfloats with wxmaxima
;;; Revised 3/19/2016 to include $rationalize.
;; rl():=load("c:/lisp/decfp.lisp")
;;;;  problems to be expected:  

;;; 1. There is a possibility that there are programs
;;;   that expect binary radix.

;;; 2. I started to write the output code so that the decimal point is on
;;; the right of the mantissa. That is  1/2  is  5 X 10^(-1).
;;; I suppose I could do this, for decimal precision 4, do 5000 x 10^-(-4).

;;; This latter version has the property that precision can be
;;; deduced from the length of the fraction part, except for 0x10^0.
;;; The previous binary version put the binary point on the left.
;;;   so in binary precision 10,  the code would be 1024 x 2^(-11)
;;;  we store the precision in the header, nevertheless, for speed.
;;;;;;;;;;;;;;


;; some decimal functions

;; Reduce the number of decimal digits in a decimal bigfloat B
;; to the number specified

(defun $decimalfptrim(B)		; B is a bigfloat
  (if (decimalfpp B)
      (cons (car B)(decimalfptrim (cdr B) t))  ;;trim excess digits
    ($bfloat B))) ;; if its binary, $bfloat should trim it

;;  decimalfptrim will carefully round

(defun decimalfptrim(f &optional (flag $rounddecimalfloats))  ;;  f is (significand exponent)
  (let ((s (car f))
	(e (cadr f)))
    (cond((eql s 0)(list 0 0))
	 (t
	  ;; first remove trailing zeros
	  (while (= (mod s 10) 0)(setf s (truncate s 10))(incf e))
	  ;; there may still be more digits than allowed by $fpprec
	  (if flag
	  (let ((adjust (- (flatsize10 s) $fpprec)))
;;;	    (format t"~%in decimalfptrim, s=~s, adjust=~s~%" s adjust)
	    (if (<= adjust 0) (list s e)
	      (decimalround s e adjust)	    ))
	  (list s e) ;; don't round!
	  
	  )))))

(defun decimalround (s e ad) ; we want to remove ad digits from the right
  (let*((guard 0)	     ; this is the guard digit   
	(sign (signum s)))
					;significand with one extra (guard) digit
					;sticky has the rest
    (multiple-value-bind 
	(swithguard sticky) 
	(truncate s (expt 10 (1- ad)))
      ;; now have last digit of s, the one to drop off,  at left end of swithguard
      (setf guard (mod swithguard 10))
;;;      (format t "~% in decimalround swg=~s guard=~s sign=~s~%" swithguard guard sign) 
      ;; because (mod 13 10) is 3, (mod -13 10) is 7...
      ;;  round 13b0 to 1b0,  round -13 to -1b0  in 1 digit..
    
      ;; because (mod 18 10) is 8, (mod -18 10) is 2
      ;;  round 18 to 2  round -18 to -2
    
      ;; now consider round-to-even
      ;; because (mod 15 10) is 5   and 1 is odd, round to 2.
      ;; because (mod -15 10) is 5  and 1 is odd, round to -2.

      (decimalfptrim
       (list (cond ((= sign 1) 
		    (cond ((< guard 5) (truncate swithguard 10)) ; no sweat
			  ;; in case we have 96 b0 we round up and get 10b0. too many digits
			  ;; so in the case of xxxx99X, or worse 9999.999X we need to re-trim.
			  ((or (> guard 5)
			       (and (= guard 5)(> sticky 0)))
			   (1+ (truncate swithguard 10))) ;no sweat again.
			  (t
			   ;; guard digit is 5 and sticky is 0. (cannot be negative if sign is 1)
			   ;; so find the even digit at or above this number
			   (let* ((snoguard(truncate swithguard 10))
				  (checkthis (mod snoguard 10)))
			     (if (evenp checkthis)(list snoguard (+  e ad)) ; this is already even
			       (1+ snoguard)))))) 
		   (t		
		  ;;; i.e. sign=-1, because sign=0 can't happen
		    (cond ((> guard 5) (truncate swithguard 10))
			  ((or (< guard 5)
			       (and (= guard 5)(< sticky 0)))
			   (1- (truncate swithguard 10)))
			  ;; guard digit is 5 and sticky is 0 (cannot be pos if sign is -1)
			  (t    (let* ((snoguard(truncate swithguard 10))
				       (checkthis (mod snoguard 10)))
				  (if (evenp checkthis)snoguard ; this is already even
				    (1- snoguard)))))))
	     (+ e ad))))))
	  
(defun flatsize10(r) (if (= r 0) 1 
		       (ceiling (* #.(log 2)(integer-length (abs r))) #.(log 10 ))))

(defvar *m)

;; *DECFP = T if the computation is being done in decimal radix.  NIL implies
;; base 2.  
;;;originally, Decimal radix was used only during output.
;;; now -- in this file it is used all the time...  1/2016/RJf

;; Representation of a decimal Bigfloat:  ((BIGFLOAT SIMP precision DECIMAL) significand exponent)
;;  significand and exponent and precision are all integers. 
;;  NOW -- number of decimal digits 1/2016
;;		precision >= (integer-length (abs significand)))  =( flatsize10 significand)
;;  NOW        The actual number represented is (* significand (^ 10 exponent)).

;; for decimals, we use $fpprec which is the decimal digit count.
;; the derived number fpprec  (no $) is the approximate number
;; of binary bits, computed by fpprec1 when $fpprec is set, used by the binary arithmetic.


(defun dim-bigfloat (form result)
  (let (($lispdisp nil))
    (dimension-atom (maknam (decfpformat form)) result)))

;; needed for wxmaxima front end.
(defun wxxml-bigfloat (x l r)  (append l '("<n>") (decfpformat x) '("</n>") r))


(defun decimalfpp(x)(and (consp x)
			 (eq (caar x) 'bigfloat)
			 (member 'decimal (cddar x)))) ; decimal bigfloat predicate
(defun binaryfpp(x)(and (consp x)
			(eq (caar x) 'bigfloat)
			(not(member 'decimal (cddar x))))) ; binary bigfloat predicate


;; Converts the bigfloat L to list of digits including |.| and the
;; exponent marker |b|. The number of significant digits is controlled
;; by $fpprintprec.

;; uses L marker instead of b.   123b0 prints 1.23L2

(defun decfpformat (l) 
  (if (decimalfpp l)
  (cond ((= (cadr l) 0) '(|0| |.| |L| |0|))
	((> (cadr l) 0)
	 (let ((M(explodec (format nil "~s"(cadr l)))))
	   (append (cons (car M)(cons '|.| (cdr M))) '(|L|)(explodec(+ (caddr l)(length M) -1)))))

	(t  (let ((M(explodec (format nil "~s"(cadr l)))))
	      (append (cons '|-|(cons (car M)(cons '|.| (cdr M))))'(|L|)(explodec(+ (caddr l)(length M) -1))))))
  ;; oh, it's a binary bigfloat
  (fpformat l)))


;; Tells you if you have a bigfloat object.  BUT, if it is a binary bigfloat,
;; it will normalize it by making the precision of the bigfloat match
;; the current precision setting in $fpprec.  And it will also convert
;; bogus zeroes (mantissa is zero, but exponent is not) to a true
;; zero.
;; overwrites the standard bigfloatp

(defun bigfloatp (x) 
  (cond ((decimalfpp x) x)
  ;; A bigfloat object looks like '((bigfloat simp <prec>) <mantissa> <exp>)
	;; just copy out the same stuff from float.lisp
	(t
-	 (prog nil
     (cond ((not ($bfloatp x)) (return nil))
	   ((= fpprec (caddar x))
	    ;; Precision matches.  (Should we fix up bogus bigfloat
	    ;; zeros?)
	    (return x))
	   ((> fpprec (caddar x))
	    ;; Current precision is higher than bigfloat precision.
	    ;; Scale up mantissa and adjust exponent to get the
	    ;; correct precision.
	    (setq x (bcons (list (fpshift (cadr x) (- fpprec (caddar x)))
				 (caddr x)))))
	   (t
	    ;; Current precision is LOWER than bigfloat precision.
	    ;; Round the number to the desired precision.
	    (setq x (bcons (list (fpround (cadr x))
				 (+ (caddr x) *m fpprec (- (caddar x))))))))
     ;; Fix up any bogus zeros that we might have created.
     (return (if (equal (cadr x) 0) (bcons (list 0 0)) x)))
  )))

;; overwrite this one, too
(defun bigfloat2rat (x)
  (if (decimalfpp x) (let ((k (* (cadr x)(expt 10 (caddr x)))))
		       (list '(rat) (numerator k)(denominator k)))
    (let ()
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
      (terpri))
    exp))))

;; Convert a machine floating point number into a bigfloat.
;; Since it is a binary float, no point in making it decimal..
;; Probably we don't do this a lot, so we just do it
;; with a short program.
(defun decfloat-from-fp (x)  ($binarybfloat ($rationalize x)))

;; Convert a bigfloat into a floating point number.
(defun fp2flo(l)(decfp2flo l)) ; redefine usual
(defun decfp2flo (l)
  (if (decimalfpp l) 
      (coerce  (* (cadr l)(expt 10 (caddr l))) 'double-float)
     (let ((precision (caddar l))
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
	  (scale-float mantissa e))))
    ))

(defun decbcons (s)
  `((bigfloat simp ,$fpprec decimal) . ,(decimalfptrim s)))

;; probably not useful.  We only allow new decimal bigfloats
;; if they are typed in as 1.2L0 etc  Or maybe if they are integers??


(defun $bfloat (x) ;; used by too many other programs. need to replace it here
  (cond 
   ((bigfloatp x))	   ; return x, possibly changed precision
   ((numberp x)		   ;; favors decimal conversion for CL numbers
    (decintofp x))
   #+ignore
   ((numberp x)	;; favors binary conversion for CL numbers
    (bcons (intofp x)))
   ((atom x)($binarybfloat x))		; %pi, %phi
   ((eq (caar x) 'rat)
    (decrat2fp x))
   ((and(eq (caar x) 'mexpt)
	(decimalfpp (cadr x))
	(integerp (caddr x))
	(>= (caddr x) 0))
    (decfppower (cadr x)(caddr x)))	; float^integer
   
      ((eq (caar x) 'mtimes)
       (dectimesbigfloat (mapcar #'$decbfloat(cdr x))))  ;; not enough.?
      
      ((eq (caar x) 'mplus)
       (decaddbigfloat (mapcar #'$decbfloat(cdr x))))

   ;; here we return to the previous program
   (t  ($binarybfloat x))))

(defun $decbfloat (x)
  (cond 
   ((decimalfpp x) 
    (if (null $rounddecimalfloats) x	; just return it
      (decbcons (decimalfptrim (cdr x))))); possibly chop/round it
   
   
   ((numberp x)  ;; favors decimal conversion for CL numbers
	    (decrat2fp x))
   ((atom x)($binarybfloat x))		; %pi, %phi
   ((eq (caar x) 'rat)
    (decrat2fp x))
   ((and(eq (caar x) 'mexpt)
	(decimalfpp (cadr x))
	(integerp (caddr x))
	(>= (caddr x) 0))
    (decfppower (cadr x)(caddr x)))	; float^integer
   ;; here we return to the previous program
   
   (t  ($binarybfloat x))))

(defun decfppower (ba e)  ;ba^e,  e is nonneg int
  (let ((ans '(1 0))
	(b (cdr ba)))
    (dotimes (i e (decbcons (decimalfptrim ans))) (setf ans (decfptimes ans b)))))

(defun $binarybfloat (x &aux (y nil))
    (cond ((setf y (bigfloatp x)) y )
	  ((or (numberp x)
	       (member x '($%e $%pi $%gamma) :test #'eq))
	   (bcons (intofp x)))
	  ((or (atom x) (member 'array (cdar x) :test #'eq))
	   (if (eq x '$%phi)
	       ($binarybfloat '((mtimes simp)
			  ((rat simp) 1 2)
			  ((mplus simp) 1 ((mexpt simp) 5 ((rat simp) 1 2)))))
	       x))
	  ((eq (caar x) 'mexpt)
	   (if (equal (cadr x) '$%e)
	       (*fpexp (decfp2binfp (caddr x))) ;; exp(x)
	       (exptbigfloat (decfp2binfp (cadr x)) (decfp2binfp(caddr x)))))
	  ((eq (caar x) 'mncexpt)
	   (list '(mncexpt) ($binarybfloat (cadr x)) (caddr x)))
	  ((eq (caar x) 'rat)
	   (ratbigfloat (cdr x)))
	  ((setq y (safe-get (caar x) 'floatprog))
	   (funcall y (mapcar #'$binarybfloat (cdr x))) 
	   )
	  ;; removed stuff here
	  #+ignore
	  ((or (trigp (caar x)) (arcp (caar x)) (eq (caar x) '$entier))
	   (setq y ($binarybfloat (cadr x)))
	   (if ($bfloatp y)
	       (cond ((eq (caar x) '$entier) ($entier y))
		     ((arcp (caar x))
		      (setq y ($binarybfloat (logarc (caar x) y)))
		      (if (free y '$%i)
			  y (let ($ratprint) (fparcsimp ($rectform y)))))
		     ((member (caar x) '(%cot %sec %csc) :test #'eq)
		      (invertbigfloat
		       ($binarybfloat (list (ncons (safe-get (caar x) 'recip)) y))))
		     (t ($binarybfloat (exponentialize (caar x) y))))
	       (subst0 (list (ncons (caar x)) y) x)))
	  (t (recur-apply #'$binarybfloat x)))) ;;

;; works for sin cos atan tan log ... and everything else not explicitly redefined


#| (defun decsinbigfloat(f)(let((a(car f)))
			  (sinbigfloat  (if (decimalfpp a) (list(decfp2binfp a))f ) )))
(defun deccosbigfloat(f)(let((a(car f)))
			  (cosbigfloat  (if (decimalfpp a) (list(decfp2binfp a))f ))))
(defun decatanbigfloat(f)(let((a(car f)))
			   (atanbigfloat  (if (decimalfpp a) (list(decfp2binfp a))f ))))
(defun dectanbigfloat(f)(let((a(car f)))
			  (tanbigfloat  (if (decimalfpp a) (list(decfp2binfp a))f ))))
(defun declogbigfloat(f)(let((a(car f)))
			  (logbigfloat  (if (decimalfpp a) (list(decfp2binfp a))f )))) 
;(defprop %sin decsinbigfloat floatprog)
;(defprop %cos deccosbigfloat floatprog)
;(defprop %atan decatanbigfloat floatprog)
;(defprop %tan dectanbigfloat floatprog)
;(defprop %log declogbigfloat floatprog)
|#



(defun make-decimalfp-prog(j)
  (let ((k (get j 'floatprog)))
   ; (format t "~% name ~s has floatprog ~s" j k)
    (if (and k (symbolp k)) ; nil is a symbol too...
	(let ((program 
	       `(lambda(f)(let ((a (car f)))
			    (,k (if (decimalfpp a) (list(decfp2binfp a))
				  f ))))))
	  (format t "~%redefining bigfloat ~s to also work for decimal bigfloat" j)
	  (setf (get j 'floatprog) (compile nil program))))))

(eval-when
    #+gcl (load eval)
    #-gcl (:load-toplevel :execute)
     (do-symbols (j :maxima 'decimal-floats-installed) 
       (unless (member j '(mabs mplus mtimes rat) :test 'eq)
		       (make-decimalfp-prog j))))

;; exceptions to the uniform treatment above...

(defun decmabsbigfloat (l)
  (let* ((a (car l))
	 (r (bigfloatp a)))
;;    (format t "~%decmabsbigfloat a=~s r=~s" a r)
    (cond ((null r) (list '(mabs) a))
	  ((decimalfpp a)
	   (decbcons (list (abs (cadr a))(caddr a))))
	  (t (bcons(fpabs (cdr r)))))))   ;; probably never used

(defprop mabs decmabsbigfloat floatprog)

;;(defun decrat2fp(f)(bigfloat2rat (car f)))

;;etc

(defprop mplus decaddbigfloat floatprog)
(defprop mtimes dectimesbigfloat floatprog)
(defprop rat decrat2fp floatprog) ; this appears to not be used..


(defun decfp2binfp(f)			;convert integer or rational or decimal fp to binary fp
  (cond((numberp f)($binarybfloat f))
       ((decimalfpp f)			;check if decimal
	($binarybfloat (let((r (* (cadr f)(expt 10 (caddr f)))))
			 (list '(rat) (numerator r)(denominator r)))))
       (t ($binarybfloat f))))
		    
(defun decaddbigfloat (h)
  ;; h is a list of bigfloats. it will
  ;; return one bigfloat, maybe decimal
  ;; if there is at least one binary, convert all to binary.
  (let ((anybin (some #'binaryfpp h))
	(r nil)
	(fans nil))
    (if (and anybin (some #'decimalfpp h)) 
	;; then there are some of each
	(setf h (map 'list #'decfp2binfp h))	; convert everything to binary
      ;;otherwise everything is decimal or maybe integer??
      )
;;    (format t "~% decAddBigFloat h=~s" h)
    (if anybin ; all are in binary
	(let ()
	  (setf fans (bcons '(0 0)))
	  (map nil #'(lambda(z)
			   (setf r (bigfloatp z))
			   (setq fans (bcons (fpplus (cdr r)(cdr fans)))))
		   h))
	     ;; all are decimal
      (let()
	(setf fans (decbcons '(0 0)))
	(map nil #'(lambda(z)
			   (setf r ($bfloat z))
			   (setq fans (decbcons (decfpplus (cdr r)(cdr fans)))))
	     h)))
      fans))


;; convert a maxima rat to a decimal float. 1/2016

;;  for n/d rational number   divide(n*10^$fpprec,d) to get [s,r] such that  n/d= s/10^fprec+r.
;;  e.g.  321/4 .. $fpprec=10, we get                         [802500000000, 0]
;; so 321/4  is  802500000000 x 10^-10 or 80.25.
;; better would be 8025 x10^-2 .

(defun decrat2fp (r)		; r is ((rat) n d)
  (cond((decimalfpp r) r)
       ((and (consp r)(eq (caar r) 'rat))
	(let* ((n (cadr r))
	       (d (caddr r))
	       (expon (+ (flatsize10 d)$fpprec))
	       (shifter (expt 10 expon )))
	  ;; add expon zeros to the numerator, divide by d (actually, truncate) then mult  by 10^ expon
	  (decbcons (list (truncate (* n shifter) d)	  (- expon)))))
       ((integerp r)
	(decbcons (list r 0)))
       (t(decrat2fp ($rationalize r))))) ; other numbers

      
;; intofp takes any expression and return a bigfloat,
;; decintofp modifies that so that the answer is
;; decimal if possible.  %pi is not returned as decimal.
;; Returns a bigfloat with header
(defun decintofp (l)
  (cond ((not (atom l)) ($bfloat l))
	((floatp l) (decfloat-from-fp l))
	((equal 0 l)(decbcons  '(0 0)))
	((eq l '$%pi)(bcons (fppi)))
	((eq l '$%e) (bcons(fpe)))
	((eq l '$%gamma) (bcons(fpgamma)))
	(t (decbcons (list l 0)))
	))

(defun decfpquotient (a b) ; decimal version just a hack
  (cond ((equal (car b) 0)
	 (merror (intl:gettext "pquotient: attempted quotient by zero.")))
	((equal (car a) 0) '(0 0))
	(t (decimalfptrim (list (truncate (/ (* (expt 10 $fpprec)(car a))
					  (car b)))
					   (- (cadr a)(cadr b) $fpprec ))))))

(defun decfpdifference (a b)
  (fpplus a (fpminus b)))

(defun decfpminus (x)
  (if (equal (car x) 0)
      x
      (list (- (car x)) (cadr x))))

(defun decfpplus(a b)
  ;; totally without rounding
  (let ((exp(- (cadr a) (cadr b))))

    (cond ((= exp 0)(list (+ (car a)(car b)) (cadr a)))
	  ((> exp 0)	
	   (list (+ (* (expt 10 exp) (car a))
		    (car b))
			   (cadr b)))
	  
	  (t  (list (+ (* (car b)(expt 10 (- exp))) 
		       (car a))
		    (cadr a))))))

(defun dectimesbigfloat (h)
  ;; h is a list of bigfloats. it will
  ;; return one bigfloat, maybe decimal
  ;; if there is at least one binary, convert all to binary.
  (let ((anybin (some #'binaryfpp h))  ;anybin is T if there is a binary bigfloat
	(r nil)
	(fans nil))
    (if (and anybin (some #'decimalfpp h)) 
	;; then there are some of each
	(setf h (map 'list #'decfp2binfp h))	; some binary and some dec:convert everything to binary
      ;;otherwise everything is decimal or maybe integer??
      )
   ;; (format t "~% decTimesBigFloat h=~s,~% anybin=~s" h anybin)
    (if anybin ; all are in binary
	(let ()
	  (setf fans (bcons(fpone)))
	  (map nil #'(lambda(z)
			   (setf r (bigfloatp z))
			   (setq fans (bcons (fptimes* (cdr r)(cdr fans)))))
		   h))
	     ;; all are decimal
      (let()

	(setf fans (decbcons '(1 0)))
	(map nil #'(lambda(z)
			   (setf r ($bfloat z))
			   (setq fans (decbcons (decfptimes (cdr r)(cdr fans)))))
	     h)))
      fans))

(defun decfptimes (a b) (decimalfptrim (decfptimes* a b)))

(defun decfptimes* (a b)
  ;; totally without rounding
  (list (* (car a)(car b))
	(+ (cadr a)(cadr b))))

;; Don't use the symbol BASE since it is SPECIAL.

(defun decfpintexpt (int nn fixprec)	;INT is integer
  (setq fixprec (truncate fixprec (1- (integer-length int)))) ;NN is pos
  (let ((bas (decintofp (expt int (min nn fixprec)))))
    (if (> nn fixprec)
	(fptimes* (decintofp (expt int (rem nn fixprec)))
		  (fpexpt bas (quotient nn fixprec)))
	bas)))

;; NN is positive or negative integer
(defun decfpone()'(1 0))

;; this is  not yet elaborated on.
;; rewrite modeled on addbigfloat
(defun timesbigfloat (h)
  (prog (fans r nfans)
     (setq fans (decbcons (fpone)) nfans 1)
     (do ((l h (cdr l)))
	 ((null l))
       (if (setq r (bigfloatp (car l)))
	   (setq fans (decbcons (fptimes* (cdr r) (cdr fans))))
	   (setq nfans (list '(mtimes) (car l) nfans))))
     (return (if (equal nfans 1)
		 fans
		 (simplifya (list '(mtimes) fans nfans) nil)))))


(defun invertbigfloat (a) ;works and this has 1/3.0L0 in binary
    (bcons (fpquotient (fpone)   (if (decimalfpp a)
				     (cdr(decfp2binfp a))
				   (cdr a)))))

;;; taken out of nparse.lisp.
(defun make-number (data)
  (setq data (nreverse data))
  ;; Maxima really wants to read in any number as a flonum
  ;; (except when we have a bigfloat, of course!).  So convert exponent
  ;; markers to the flonum-exponent-marker.
  (let ((marker (car (nth 3 data))))
    (unless (eql marker flonum-exponent-marker)
      (when (member marker '(#\E #\F #\S #\D  #+cmu #\W))
        (setf (nth 3 data) (list flonum-exponent-marker)))))
  (cond  ((equal (nth 3 data) '(#\B))
	 ;; (format t "~% exponent B data=~s~%" data)
	  (let*
	      ((*read-base* 10.)
	       (int-part (readlist (or (first data) '(#\0))))
	       (frac-part (readlist (or (third data) '(#\0))))
	       (frac-len (length (third data)))
	       (exp-sign (first (fifth data)))
	       (exp (readlist (sixth data))))
	    (if (and $fast_bfloat_conversion
		     (> (abs exp) $fast_bfloat_threshold))
		;; Exponent is large enough that we don't want to do exact
		;; rational arithmetic.  Instead we do bfloat arithmetic.
		;; For example, 1.234b1000 is converted by computing
		;; bfloat(1234)*10b0^(1000-3).  Extra precision is used
		;; during the bfloat computations.
		(let* ((extra-prec (+ *fast-bfloat-extra-bits* (ceiling (log exp 2e0))))
		       (fpprec (+ fpprec extra-prec))
		       (mant (+ (* int-part (expt 10 frac-len)) frac-part))
		       (bf-mant (bcons (intofp mant)))
		       (p (power (bcons (intofp 10))
				 (- (if (char= exp-sign #\-)
					(- exp)
				      exp)
				    frac-len)))
		       ;; Compute the product using extra precision.  This
		       ;; helps to get the last bit correct (but not
		       ;; always).  If we didn't do this, then bf-mant and
		       ;; p would be rounded to the target precision and
		       ;; then the product is rounded again.  Doing it
		       ;; this way, we still have 3 roundings, but bf-mant
		       ;; and p aren't rounded too soon.
		       (result (mul bf-mant p)))
		  (let ((fpprec (- fpprec extra-prec)))
		    ;; Now round the product back to the desired precision.
		    (bigfloatp result)))
	      ;; For bigfloats, turn them into rational numbers then
	      ;; convert to bigfloat.  Fix for the 0.25b0 # 2.5b-1 bug.
	      ;; Richard J. Fateman posted this fix to the Maxima list
	      ;; on 10 October 2005.  Without this fix, some tests in
	      ;; rtestrationalize will fail.  Used with permission.
	      (let ((ratio (* (+ int-part (* frac-part (expt 10 (- frac-len))))
			      (expt 10 (if (char= exp-sign #\-)
					   (- exp)
					 exp)))))
		($binarybfloat (cl-rat-to-maxima ratio))))))
	 ;; decimal input
	 ((equal (nth 3 data) '(#\L))
	  ;;	  (format t "~% exponent L data=~s~%" data)
	 (let*
	     ((*read-base* 10.)
	      (int-part (readlist (or (first data) '(#\0))))
	      (frac-part (readlist (or (third data) '(#\0))))
	      (frac-len (length (third data)))
	      (exp-sign (first (fifth data)))
	      (exp (readlist (sixth data))))
      	
	   (let* ((bigmant
		   (+ frac-part (* int-part (expt 10 frac-len))))
		  (bigexp (- (if (char= exp-sign #\-)
				 (- exp)
			       exp) frac-len)) )

	     (decbcons (list bigmant bigexp)))))
	 (t  (readlist (apply #'append data)))))


;; from maxmin.lisp

(defun $rationalize (e)

  (setq e (ratdisrep e))
  (cond ((floatp e) 
	 (let ((significand) (expon) (sign))
	   (multiple-value-setq (significand expon sign) (integer-decode-float e))
	   (cl-rat-to-maxima (* sign significand (expt 2 expon)))))
	
	#+ignore
	 (($bfloatp e) (cl-rat-to-maxima (* (cadr e)(expt 2 (- (caddr e) (third (car e)))))))
	 ((binaryfpp e) (cl-rat-to-maxima (* (cadr e)(expt 2 (- (caddr e) (third (car e)))))))
	 ((decimalfpp e) (cl-rat-to-maxima (* (cadr e)(expt 10 (caddr e)))))
	
	(($mapatom e) e)
	(t (simplify (cons (list (mop e)) (mapcar #'$rationalize (margs e)))))))

;; from trigi

(defun big-float-eval (op z)
  (if (decimalfpp z)(setf z (decfp2binfp z)))
  (when (complex-number-p z 'bigfloat-or-number-p)
    (let ((x ($realpart z))
	  (y ($imagpart z))
	  (bop (gethash op *big-float-op*)))
      (macrolet ((afloatp (u)
                   `(or (floatp ,u) ($bfloatp ,u))))
        ;; If bop is non-NIL, we want to try that first.  If bop
        ;; declines (by returning NIL), we silently give up and use the
        ;; rectform version.
        (cond ((and (afloatp x) (like 0 y))
               (or (and bop (funcall bop ($bfloat x)))
                   ($bfloat `((,op simp) ,x))))
              ((or (afloatp x) (afloatp y))
               (or (and bop (funcall bop ($bfloat x) ($bfloat y)))
                   (let ((z (add ($bfloat x) (mul '$%i ($bfloat y)))))
                     (setq z ($rectform `((,op simp) ,z)))
                     ($bfloat z)))))))))

#|
rationalize(1.0b-1)-1/10			;   should be non-zero
rationalize(1.0L-1)-1/10		; ; should be zero
|#

(eval-when
    #+gcl (load eval)
    #-gcl (:load-toplevel :execute)
     (fpprec1 nil $fpprec)		; Set up user's precision
     )

;; these 3 functions below are not needed .. see advise-fun-simp
;; for the workaround.
#+ignore
(defun $decfloor(f)  
  (if (decimalfpp f) (floor (* (cadr f)(expt 10 (caddr f))))
    (mfuncall '$floor f)))

#+ignore
(defun $decceiling(f)
  (if (decimalfpp f)(ceiling (* (cadr f)(expt 10 (caddr f))))
    (mfuncall '$ceiling f)))

#+ignore
(defun $dectruncate(f)
  (if (decimalfpp f)(truncate (* (cadr f)(expt 10 (caddr f))))
    (mfuncall '$truncate f)))

(defun advise-fun-simp (r test var val)  ;; hack the simplification
    (unless (get r 'orig)
      (setf (get r 'orig) (get r 'operators))) ; get the simplification program
    (setf (get r 'operators)
      (compile nil ` (lambda(,var %x %y)(declare(ignore %x %y))
			    (if ,test ,val (,(get r 'orig) ,var 1 t))))))
;; sort of like tellsimp
(advise-fun-simp '%truncate '(decimalfpp (cadr f)) 'f '(truncate (* (cadadr f)(expt 10 (caddr(cadr f))))))
(advise-fun-simp '$floor '(decimalfpp (cadr f)) 'f    '(floor (* (cadadr f)      (expt 10 (caddr(cadr f))))))
(advise-fun-simp '$ceiling '(decimalfpp (cadr f)) 'f  '(ceiling (* (cadadr f)    (expt 10 (caddr(cadr f))))))
;; why $floor but %truncate? historical accident..

;; $remainder is a function...  another inconsistency..

(defvar *oldremainder (symbol-function '$remainder))

(defun $remainder (x y)  ;; convert to rational?  remainder is always 0 in Rational Field
  (if (decimalfpp x)(setf x (bigfloat2rat x)))
  (if (decimalfpp y)(setf y (bigfloat2rat y)))
;  (format t "~% x=~s y=~s" x y) test
  (funcall *oldremainder x y))
      
(defun $decimalfpp(x)(if (decimalfpp x) t nil))      
      



(in-package :bigfloat)
;; from numeric.lisp
;; which uses structures for bigfloats, including complex
;; stuff that did not exist when the float.lisp stuff was written c. 1974

(defun bigfloat (re &optional im) 
  "Convert RE to a BIGFLOAT.  If IM is given, return a COMPLEX-BIGFLOAT"
  (if (maxima::decimalfpp re)(setf re (maxima::decfp2binfp re))) ;added rjf
  (if (maxima::decimalfpp im)(setf im (maxima::decfp2binfp im))) ;added rjf
  (cond (im
	 (make-instance 'complex-bigfloat
			:real (intofp re)

			:imag (intofp im)))
	((cl:realp re)
	 (make-instance 'bigfloat :real (intofp re)))
	((cl:complexp re)
	 (make-instance 'complex-bigfloat
			:real (intofp (cl:realpart re))
			:imag (intofp (cl:imagpart re))))
	((maxima::$bfloatp re)
	 
	 (make-instance 'bigfloat :real re )) ;new
		
	((maxima::complex-number-p re 'maxima::bigfloat-or-number-p)
	 (make-instance 'complex-bigfloat
			:real (maxima::decfp2binfp (maxima::$realpart re))
			:imag (maxima::decfp2binfp (maxima::$imagpart re))
			))
	 ((typep re 'bigfloat)
	  ;; Done this way so the new bigfloat updates the precision of
	  ;; the given bigfloat, if necessary.
	  (make-instance 'bigfloat :real (real-value re)))
	 ((typep re 'complex-bigfloat)
	  ;; Done this way so the new bigfloat updates the precision of
	  ;; the given bigfloat, if necessary.
	  (make-instance 'complex-bigfloat
			 :real (real-value re)
			 :imag (imag-value re)))
	 (t
	  (make-instance 'bigfloat :real (maxima::decimalfpp re)))))



#| BUGS 3/19/2016   abs(2.0l0) comes out as binary. eh. hard to fix because
bigfloat-impl::bigfloat  loses the radix.
atan2(3,4.0l0) wrong.  where to patch??
remainder (6.1L0,6)

atan2(3.0l0,4) works

|#


