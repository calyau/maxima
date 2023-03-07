;;  Copyright 2005, 2006, 2020, 2021 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)

(macsyma-module conjugate)

($put '$conjugate 1 '$version)
;; Let's remove built-in symbols from list for user-defined properties.
(setq $props (remove '$conjugate $props))

(setf (get 'conjugate-superscript-star 'tex-lbp) (tex-lbp 'mexpt))
(setf (get 'conjugate-superscript-star 'tex-rbp) (tex-rbp 'mexpt))

(defun tex-conjugate (x l r)
  ;; Punt to TeX output for MEXPT, but defeat the special case for
  ;; powers of trig functions, e.g. sin(x)^2 which is set as sin^2 x,
  ;; by supplying an expression which has a different operator
  ;; (namely CONJUGATE-SUPERSCRIPT-STAR) instead of MEXPT.
  (tex-mexpt `((conjugate-superscript-star) ,(second x) "\\ast") l r))

(defprop $conjugate tex-conjugate tex)
(defprop $conjugate 121. tex-lbp)
(defprop $conjugate 120. tex-rbp)

(defprop $conjugate simp-conjugate operators)

;; Maybe $conjugate should have a msimpind property. But with some Maxima versions,
;; kill(conjugate) eliminates the msimpind property; after that, conjugate gives rubbish.
;; Until this is resolved, $conjugate doesn't have a msimpind property.

(eval-when
    (:load-toplevel :execute)
    (let (($context '$global) (context '$global))
      (meval '(($declare) $conjugate $complex))
      ;; Let's remove built-in symbols from list for user-defined properties.
      (setq $props (remove '$conjugate $props))))

;; When a function commutes with the conjugate, give the function the
;; commutes-with-conjugate property. The log function commutes with
;; the conjugate on all of C except on the negative real axis. Thus
;; log does not get the commutes-with-conjugate property.  Instead,
;; log gets the conjugate-function property.

;; What important functions have I missed?

;; (1) Arithmetic operators

(setf (get 'mplus 'commutes-with-conjugate) t)
(setf (get 'mtimes 'commutes-with-conjugate) t)
;(setf (get 'mnctimes 'commutes-with-conjugate) t) ;; generally I think users will want this
(setf (get '%signum 'commutes-with-conjugate) t) ;; x=/=0, conjugate(signum(x)) = conjugate(x/abs(x)) = signum(conjugate(x))
;; Trig-like functions and other such functions

(setf (get '%cosh 'commutes-with-conjugate) t)
(setf (get '%sinh 'commutes-with-conjugate) t)
(setf (get '%tanh 'commutes-with-conjugate) t)
(setf (get '%sech 'commutes-with-conjugate) t)
(setf (get '%csch 'commutes-with-conjugate) t)
(setf (get '%coth 'commutes-with-conjugate) t)
(setf (get '%cos 'commutes-with-conjugate) t)
(setf (get '%sin 'commutes-with-conjugate) t)
(setf (get '%tan 'commutes-with-conjugate) t)
(setf (get '%sec 'commutes-with-conjugate) t)
(setf (get '%csc 'commutes-with-conjugate) t)
(setf (get '%cot 'commutes-with-conjugate) t)
(setf (get '$atan2 'commutes-with-conjugate) t)

(setf (get '%jacobi_cn 'commutes-with-conjugate) t)
(setf (get '%jacobi_sn 'commutes-with-conjugate) t)
(setf (get '%jacobi_dn 'commutes-with-conjugate) t)

(setf (get '%gamma 'commutes-with-conjugate) t)
(setf (get '$pochhammer 'commutes-with-conjugate) t)

;; Collections

(setf (get '$matrix 'commutes-with-conjugate) t)
(setf (get 'mlist 'commutes-with-conjugate) t)
(setf (get '$set 'commutes-with-conjugate) t)

;; Relations

(setf (get 'mequal 'commutes-with-conjugate) t)
(setf (get 'mnotequal 'commutes-with-conjugate) t)
(setf (get '%transpose 'commutes-with-conjugate) t)

;; Oddball functions

(setf (get '$max 'commutes-with-conjugate) t)
(setf (get '$min 'commutes-with-conjugate) t)

;; When a function has the conjugate-function property, use a non-generic function to conjugate it. 
;; The argument to a conjugate function for an operator op is the CL list of arguments to op. For
;; example, the conjugate function for log gets the argument for log, not the expression log(x). 
;; It would be a bit more efficient if a conjugate function received the full expression--that
;; way for a pure nounform return (for example, return conjugate(log(x))), a conjugate function
;; would not not need to apply the operator to the argument to the conjugate function, instead it 
;; could simply paste ($conjugate simp) onto the expression.

;; Not done: conjugate-functions for all the inverse trigonometric functions.

;; Trig like and hypergeometric like functions

(setf (get '%log 'conjugate-function) 'conjugate-log)
(setf (get '%plog 'conjugate-function) 'conjugate-plog)
(setf (get 'mexpt 'conjugate-function) 'conjugate-mexpt)
(setf (get '%asin 'conjugate-function) 'conjugate-asin)
(setf (get '%acos 'conjugate-function) 'conjugate-acos)
(setf (get '%atan 'conjugate-function) 'conjugate-atan)
(setf (get '%atanh 'conjugate-function) 'conjugate-atanh)
(setf (get '%asec 'conjugate-function) 'conjugate-asec)
(setf (get '%acsc 'conjugate-function) 'conjugate-acsc)

(setf (get '%bessel_j 'conjugate-function) 'conjugate-bessel-j)
(setf (get '%bessel_y 'conjugate-function) 'conjugate-bessel-y)
(setf (get '%bessel_i 'conjugate-function) 'conjugate-bessel-i)
(setf (get '%bessel_k 'conjugate-function) 'conjugate-bessel-k)

(setf (get '%hankel_1 'conjugate-function) 'conjugate-hankel-1)
(setf (get '%hankel_2 'conjugate-function) 'conjugate-hankel-2)
(setf (get '%log_gamma 'conjugate-function) 'conjugate-log-gamma)

;; conjugate of polylogarithm li & psi
(setf (get '$li 'conjugate-function) 'conjugate-li)
(setf (get '$psi 'conjugate-function) 'conjugate-psi)
;; Other things:

(setf (get '%sum 'conjugate-function) 'conjugate-sum)
(setf (get '%product 'conjugate-function) 'conjugate-product)

;; Return true iff Maxima can prove that z is not on the
;; negative real axis.

(defun off-negative-real-axisp (z)
  (setq z (trisplit z))	          ; split into real and imaginary
  (or (eql t (mnqp (cdr z) 0))     ; y #  0
      (eql t (mgqp (car z) 0))))   ; x >= 0

(defun on-negative-real-axisp (z)
  (setq z (trisplit z))
  (and (eql t (meqp (cdr z) 0))
       (eql t (mgrp 0 (car z)))))

(defun off-negative-one-to-onep (z)
  (setq z (trisplit z)) ; split z into real and imaginary parts
  (or
    (eq t (mnqp (cdr z) 0))    ; y # 0
    (eq t (mgrp (car z) 1))    ; x > 1
    (eq t (mgrp -1 (car z))))) ; -1 > x

(defun in-domain-of-asin (z)
  (setq z (trisplit z)) ; split z into real and imaginary parts
  (let ((x (car z)) (y (cdr z))) ;z = x+%i*y
    (or
      (eq t (mnqp y 0)) ; y # 0
      (and
	     (eq t (mgrp x -1))     ; x > -1
	     (eq t (mgrp 1 x)))))) ; x < 1

;; Return conjugate(log(x)). Actually, x is a lisp list (x).

(defun conjugate-log (x)
  (setq x (car x))
  (cond ((off-negative-real-axisp x)
	 (take '(%log) (take '($conjugate) x)))
	((on-negative-real-axisp x)
	 (add (take '(%log) (neg x)) (mul -1 '$%i '$%pi)))
	(t (list '($conjugate simp)  (take '(%log) x)))))


;; Return conjugate(plog(x)); again, x is the CL list (x).
(defun conjugate-plog (x)
  (setq x (car x))
  (cond ((off-negative-real-axisp x)
	 (take '(%plog) (take '($conjugate) x)))
	((on-negative-real-axisp x)
	 (add (take '(%plog) (neg x)) (mul -1 '$%i '$%pi)))
	(t (list '($conjugate simp)  (take '(%plog) x)))))

;; Return conjugate(x^p), where e = (x, p). Suppose x isn't on the negative real axis.
;; Then conjugate(x^p) == conjugate(exp(p * log(x))) == exp(conjugate(p) * conjugate(log(x)))
;; == exp(conjugate(p) * log(conjugate(x)) = conjugate(x)^conjugate(p). Thus, when
;; x is off the negative real axis, commute the conjugate with ^. Also if p is an integer
;; ^ commutes with the conjugate.

;; We don't need to call $ratdisrep before checking if p is a declared integer--the
;; simpcheck at the top level of simp-conjugate does that for us. So we can call
;; maxima-integerp on p instead of using $featurep.

;; The rule that is commented out is, I think, correct, but I'm not sure how useful it is and
;; the testsuite plus the share testsuite never use this rule. For now, let's keep
;; it commented out.

;; Running the testsuite plus the share testsuite calls conjugate-mexpt 63,441
;; times. This is far more times than all the other conjugate functions. Of these
;; calls, the exponent is an integer 63,374 times. So for efficiency, we check 
;; (maxima-integerp p) first. 

;; The case of a nounform return only happens 9 times. For the nounform return, the power has 
;; been simplified at the higher level. So at least for running the testsuite, we shouldn't 
;; worry all that much about re-simplifying the power for the nounform return.

(defun conjugate-mexpt (e)
  (let ((x (first e)) (p (second e)))
    (cond ((or (maxima-integerp p) (off-negative-real-axisp x))
	             (power (take '($conjugate) x) (take '($conjugate) p)))
          ;((on-negative-real-axisp x) ;conjugate(x^p) = exp(-%i %pi conjugate(p)) (-x)^p 
          ;    (setq p (take '($conjugate) p)) 
          ;    (mul (power '$%e (mul -1 '$%i '$%pi p)) (power (mul -1 x) p)))
          (t
           (list '($conjugate simp) (power x p))))))

(defun conjugate-sum (e)
  (if (and ($featurep (third e) '$real) ($featurep (fourth e) '$real)) 
    (take '(%sum) (take '($conjugate) (first e)) (second e) (third e) (fourth e))
    (list '($conjugate simp) (simplifya (cons '(%sum) e) t))))

(defun conjugate-product (e)
  (if (and ($featurep (third e) '$real) ($featurep (fourth e) '$real)) 
    (take '(%product) (take '($conjugate) (first e)) (second e) (third e) (fourth e))
    (list '($conjugate simp) (simplifya (cons '(%product) e) t))))

(defun conjugate-asin (x)
  (setq x (car x))
  (if (in-domain-of-asin x) (take '(%asin) (take '($conjugate) x))
    (list '($conjugate simp) (take '(%asin) x))))
  
(defun conjugate-acos (x)
  (setq x (car x))
  (if (in-domain-of-asin x) (take '(%acos) (take '($conjugate) x))
    (list '($conjugate simp) (take '(%acos) x))))

(defun conjugate-acsc (x)
  (setq x (car x))
  (if (off-negative-one-to-onep x) (take '(%acsc) (take '($conjugate) x))
      (list '($conjugate simp) (take '(%acsc) x))))

(defun conjugate-asec (x)
  (setq x (car x))
  (if (off-negative-one-to-onep x) (take '(%asec) (take '($conjugate) x))
      (list '($conjugate simp) (take '(%asec) x))))

(defun conjugate-atan (x)
  (let ((xx))
    (setq x (car x))
    (setq xx (mul '$%i x))
    (if (in-domain-of-asin xx)
        (take '(%atan) (take '($conjugate) x))
         (list '($conjugate simp) (take '(%atan) x)))))

;; atanh and asin are entire on the same set; DLMF http://dlmf.nist.gov/4.37.F1 and
;; http://dlmf.nist.gov/4.23.F1

(defun conjugate-atanh (x)
  (setq x (car x))
  (if (in-domain-of-asin x) (take '(%atanh) (take '($conjugate) x))
    (list '($conjugate simp) (take '(%atanh) x))))

;; Integer order Bessel functions are entire; thus they commute with the
;; conjugate (Schwartz refection principle). But non-integer order Bessel
;; functions are not analytic along the negative real axis. Notice that DLMF
;; http://dlmf.nist.gov/10.11.E9  isn't correct; we have, for example
;; conjugate(bessel_j(1/2,-1)) =/= bessel_j(1/2,conjugate(-1))

(defun conjugate-bessel-j (z)
  (let ((n (first z)) (x (second z)))
    (if (or ($featurep n '$integer) (off-negative-real-axisp x))
        (take '(%bessel_j) (take '($conjugate) n) (take '($conjugate) x))
        (list '($conjugate simp) (simplifya (cons '(%bessel_j)  z) t)))))

(defun conjugate-bessel-y (z)
  (let ((n (first z)) (x (second z)))
    (if (off-negative-real-axisp x)
        (take '(%bessel_y) (take '($conjugate) n) (take '($conjugate) x))
        (list '($conjugate simp) (simplifya (cons '(%bessel_y) z) t)))))

(defun conjugate-bessel-i (z)
  (let ((n (first z)) (x (second z)))
    (if (or ($featurep n '$integer) (off-negative-real-axisp x))
        (take '(%bessel_i) (take '($conjugate) n) (take '($conjugate) x))
        (list '($conjugate simp) (simplifya (cons '(%bessel_i) z) t)))))

(defun conjugate-bessel-k (z)
  (let ((n (first z)) (x (second z)))
    (if (off-negative-real-axisp x)
        (take '(%bessel_k) (take '($conjugate) n) (take '($conjugate) x))
       (list '($conjugate simp) (simplifya (cons '(%bessel_k) z) t)))))

(defun conjugate-hankel-1 (z)
  (let ((n (first z)) (x (second z)))
    (if (off-negative-real-axisp x)
        (take '(%hankel_2) (take '($conjugate) n) (take '($conjugate) x))
        (list '($conjugate simp) (simplifya (cons '(%hankel_1) z) t)))))

(defun conjugate-hankel-2 (z)
  (let ((n (first z)) (x (second z)))
    (if (off-negative-real-axisp x)
        (take '(%hankel_1) (take '($conjugate) n) (take '($conjugate) x))
        (list '($conjugate simp) (simplifya (cons '(%hankel_2) z) t)))))

(defun conjugate-log-gamma (z)
	(setq z (first z))
	(if (off-negative-real-axisp z)
		   (take '(%log_gamma) (take '($conjugate) z)) 
		(list '($conjugate simp) (take '(%log_gamma) z))))

;; conjugate of polylogarithm li[s](x), where z = (s,x). We have li[s](x) = x+x^2/2^s+x^3/3^s+...
;; Since for all integers k, we have conjugate(x^k/k^s) = conjugate(x)^k/k^conjugate(s), we 
;; commute conjugate with li.
(defun conjugate-li (z)
	(let ((s (take '($conjugate) (first z))) (x (take '($conjugate) (second z))))
	   (take '(mqapply) `(($li array) ,s) x)))

(defun conjugate-psi (z)
	(let ((s (take '($conjugate) (first z))) (x (take '($conjugate) (second z))))
	   (take '(mqapply) `(($psi array) ,s) x)))

;; When all derivative variables & orders are real, commute the derivative with
;; the conjugate.
(defun conjugate-derivative (z)
   (cond ((every #'manifestly-real-p (cdr z))
           (setq z (cons (take '($conjugate) (car z)) (cdr z)))
           (simplifya (cons (list '%derivative) z) t))
         (t
           (list '($conjugate simp) (simplifya (cons (list '%derivative) z) t)))))
          
(setf (get '%derivative 'conjugate-function) 'conjugate-derivative)     
  
;; When a function maps "everything" into the reals, put real-valued on the
;; property list of the function name. This duplicates some knowledge that
;; $rectform has. So it goes. 

(setf (get '%imagpart 'real-valued) t)
(setf (get 'mabs 'real-valued) t)
(setf (get '%realpart 'real-valued) t)
(setf (get '%carg 'real-valued) t)
(setf (get '$ceiling 'real-valued) t)
(setf (get '$floor 'real-valued) t)
(setf (get '$mod 'real-valued) t)
(setf (get '$unit_step 'real-valued) t)
(setf (get '$charfun 'real-valued) t)


;; The function manifestly-real-p makes some effort to determine if its input is 
;; real valued.  

;; manifestly-real-p isn't a great name, but it's OK. Since (manifestly-real-p '$inf) --> true
;; it might be called manifestly-extended-real-p. A nonscalar isn't real.

;; There might be some advantage to requiring that the subscripts to a $subvarp
;; all be real.  Why? Well li[n] maps reals to reals when n is real, but li[n] does
;; not map the reals to reals when n is nonreal.

(defun manifestly-real-p (e)
  (let (($inflag t))
   (or
	  ($numberp e)
	  (and ($mapatom e)
		     (not (manifestly-pure-imaginary-p e))
	       (not (manifestly-complex-p e))
	       (not (manifestly-nonreal-p e)))
	  (and (consp e) (consp (car e)) (get (caar e) 'real-valued)) ;F(xxx), where F is declared real-valued
	  (and ($subvarp e) (manifestly-real-p ($op e)))))) ;F[n],  where F is declared real-valued

;; The function  manifestly-pure-imaginary-p makes some effort to determine if its input is 
;; a multiple of %i.

(defun manifestly-pure-imaginary-p (e)
  (let (($inflag t))
    (or 
     (and ($mapatom e)
	     (or
	      (eq e '$%i)
	      (and (symbolp e) (kindp e '$imaginary) (not ($nonscalarp e)))
	      (and ($subvarp e) (manifestly-pure-imaginary-p ($op e)))))
        ;; For now, let's use $csign on constant expressions only; once $csign improves,
        ;; the ban on nonconstant expressions can be removed.
        (and ($constantp e) (not (eq '$und e)) (not (eq '$ind e)) (eq '$imaginary ($csign e))))))

;; Don't use (kindp e '$complex)!

(defun manifestly-complex-p (e)
  (let (($inflag t))
    (or (and (symbolp e) (decl-complexp e) (not ($nonscalarp e)))
	      (eq e '$infinity)
	      (and ($subvarp e) (manifestly-complex-p ($op e)) (not ($nonscalarp e))))))

(defun manifestly-nonreal-p (e)
  (and (symbolp e) (or (member e `($und $ind t nil)) ($nonscalarp e))))

;; We could make commutes_with_conjugate and maps_to_reals features. But I
;; doubt it would get much use.

(defun simp-conjugate (e f z)
  (oneargcheck e)
  (setq e (simpcheck (cadr e) z))	; simp and disrep if necessary

  (cond ((complexp e) (conjugate e))    ; never happens, but might someday.
	((manifestly-real-p e) e)
	((manifestly-pure-imaginary-p e) (mul -1 e))
	((or (manifestly-nonreal-p e) ($mapatom e))
      (list '($conjugate simp) e))

	((op-equalp e '$conjugate) (car (margs e)))

	((and (symbolp (mop e)) (get (mop e) 'real-valued)) e)

	((and (symbolp (mop e)) (get (mop e) 'commutes-with-conjugate))
	 (simplify (cons (list (mop e)) (mapcar #'(lambda (s) (take '($conjugate) s)) (margs e)))))

	((setq f (and (symbolp (mop e)) (get (mop e) 'conjugate-function)))
      (funcall f (margs e)))
	  
  ;;subscripted functions	  
	((setq f (and ($subvarp (mop e)) (get (caar (mop e)) 'conjugate-function)))
	 	 (funcall f (append (margs (mop e)) (margs e))))

	(t 
    (list '($conjugate simp) e))))
