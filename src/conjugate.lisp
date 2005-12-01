;;  Copyright 2005 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)
(macsyma-module conjugate)

($put '$conjugate 1 '$version)

(defprop $conjugate tex-postfix tex)
(defprop $conjugate ("^\\star") texsym)
(defprop $conjugate 160. tex-lbp)

;; A fix feature(x,complex) --> true for all symbols bug.

;; I'M NOT CONVINCED THIS NEEDS FIXING, SINCE X IN R => X IN C
;; WE WANT EVENP(X) => INTEGERP(X) DON'T WE ??

;; (defmfun $featurep (e ind)
;;   (cond ((not (symbolp ind))
;; 	 (merror "~M is not a symbolic atom - `featurep'." ind))
;; 	((eq ind '$integer) (maxima-integerp e))
;; 	((eq ind '$noninteger) (nonintegerp e))
;; 	((eq ind '$even) (mevenp e))
;; 	((eq ind '$odd) (moddp e))
;; 	((eq ind '$real)
;; 	 (if (atom e)
;; 	     (or (numberp e) (kindp e '$real) (numberp (numer e)))
;; 	   (free ($rectform e) '$%i)))
;; 	
;; 	((symbolp e) (kindp e ind))))

;; This is defined in compar.lisp, in exactly the same way so kill this one.
#+nil
(defun op-equalp (e &rest op)
  (and (consp e) (consp (car e)) (some #'(lambda (s) (equal (caar e) s)) op)))

(defprop $conjugate simp-conjugate operators)

(setf (get 'mabs 'realvaluedp) t)
(setf (get '%realpart 'realvaluedp) t)
(setf (get '%imagpart 'realvaluedp) t)

;; When a function commutes with the conjugate, give the function the
;; commutes-with-conjugate property. The log function commutes with
;; the conjugate on all of C except on the negative real axis. Thus
;; log does not get the commutes-with-conjugate property.  Instead,
;; log gets the conjugate-function property. 

(setf (get 'mplus 'commutes-with-conjugate) t)
(setf (get 'mtimes 'commutes-with-conjugate) t)
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
(setf (get '$matrix 'commutes-with-conjugate) t)
(setf (get 'mlist 'commutes-with-conjugate) t)
(setf (get 'mequal 'commutes-with-conjugate) t)

;; When a function has the conjugate-function property,
;; use a non-generic function to conjugate it.

(setf (get '%log 'conjugate-function) 'conjugate-log)
(setf (get 'mexpt 'conjugate-function) 'conjugate-mexpt)

;; Return true iff Maxima can prove that z is not on the 
;; negative real axis.

(defun off-negative-real-axisp (z)
  (setq z ($rectform z))
  (let ((x ($realpart z)) (y ($imagpart z)) ($prederror nil))
    (or (eq t (mevalp `((mgreaterp) ,y 0))) 
	(eq t (mevalp `((mgreaterp) 0 ,y)))
	(eq t (mevalp `((mgreaterp) ,x 0))))))

;; Return conjugate(log(x)). Actually, x is a lisp list (x).

(defun conjugate-log (x)
  (setq x (car x))
  (if (off-negative-real-axisp x) (simplifya `((%log) ,(mfuncall '$conjugate x)) nil)
    `(($conjugate simp) ((%log) ,x))))

;; Return conjugate(x^p), where e = (x, p)

(defun conjugate-mexpt (e)
  (let (($prederror nil) (x (first e)) (p (second e)))
    (cond ((or (integerp p) (and ($ratnump p) (off-negative-real-axisp x)))
	   (power (mfuncall '$conjugate x) p))
	  ((eq t (mevalp `((mgreaterp) ,x 0)))
	   (power x (mfuncall '$conjugate p)))
	  (t `(($conjugate simp) ,(power x p))))))

(defun simp-conjugate (e y z)
  (declare (ignore y))
  (oneargcheck e)
  (let ((f))
    (cond ((not (memq 'simp (car e)))
	   (setq e (simplifya (nth 1 e) z))
	   (cond ((complexp e) (conjugate e)) ;; never happens, but might someday.
		 (($mapatom e) 
		  (cond
            ((like e '$%i)                          ;; $FEATUREP doesn't know %i is imaginary ... oh well.
             (mult -1 '$%i))
            ((or ($atom e) (eq (caar e) 'rat))      ;; $ATOM catches bigfloats but not rats ... oh well.
             (if ($featurep e '$real) e
               (if ($featurep e '$imaginary) (mult -1 e)
                 `(($conjugate simp) ,e))))
            ;; Otherwise, E is something like (($X ARRAY) 123).
            ;; Call it real if (CAAR E) ($X in the example) is known to be real.
            (t
              (if ($featurep (caar e) '$real) e
                (if ($featurep (caar e) '$imaginary) (mult -1 e)
                  `(($conjugate simp) ,e))))))

		 ((get (mop e) 'realvaluedp) e)

		 ((op-equalp e '$conjugate) (car (margs e)))
		 
		 ((get (mop e) 'commutes-with-conjugate) (mfuncall '$map '$conjugate e))
		 
		 ((setq f (get (mop e) 'conjugate-function)) (funcall f (margs e)))
		 
		 (t `(($conjugate simp) ,e))))
	  (t e))))
