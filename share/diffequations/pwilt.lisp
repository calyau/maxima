;;
;; Copyright (C) 2010, 2011 Mark H. Weaver <mhw@netris.org>
;;
;; pwilt: inverse laplace transforms for piecewise functions
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;

(in-package :maxima)

($put '$pwilt 1 '$version)

(if (not ($get '$hstep '$version)) ($load '$hstep))

(defmvar $pwilt_hstep_all nil
  "If true, pwilt includes hstep in all terms of the result.")

(defun pwilt-add-parts (a b)
  (flet ((merge (ap bp) (cons (car ap) (add (cdr ap) (cdr bp)))))
    (let ((r '()))
      (loop (cond ((endp a) (return (nreconc r b)))
		  ((endp b) (return (nreconc r a)))
		  ((alike1 (caar a) (caar b))
		   (push (merge (pop a) (pop b)) r))
		  ((great (caar a) (caar b))
		   (push (pop a) r))
		  (t
		   (push (pop b) r)))))))

(defun pwilt-mult-parts (a b)
  (reduce #'pwilt-add-parts
	  (mapcar #'(lambda (ap)
		      (mapcar #'(lambda (bp)
				  (cons (add (car ap) (car bp))
					(mul (cdr ap) (cdr bp))))
			      b))
		  a)))

(defun pwilt-parts (e s)
  (setq e (specrepcheck e))
  (cond ((freeof s e) (list (cons 0 e)))
	((mplusp e) (reduce #'pwilt-add-parts
			    (mapcar #'(lambda (e) (pwilt-parts e s))
				    (margs e))))
	((mtimesp e) (reduce #'pwilt-mult-parts
			     (mapcar #'(lambda (e) (pwilt-parts e s))
				     (margs e))))
	((and (mexptp e) (eq (mexpt-base e) '$%e))
	 (let ((l (islinear (mexpt-expt e) s)))
	   (if l (list (cons (car l) (power '$%e (cdr l))))
	     (list (cons 0 e)))))
	((and (mexptp e) (eql (mexpt-expt e) -1))
	 (let ((dp (pwilt-parts (mexpt-base e) s)))
	   (cond ((= 1 (length dp))
		  (list (cons (neg (caar dp))
			      (inv (cdar dp)))))
		 ((and (= 2 (length dp))
		       (freeof '%pwilt_periodic (car (first dp)))
		       (freeof '%pwilt_periodic (car (second dp)))
		       (zerop1 (add (cdr (first dp))
				    (cdr (second dp)))))
		  (let* ((offset (car (first dp)))
			 (period (sub offset (car (second dp))))
			 (scale (cdr (first dp))))
		    (case (asksign period)
		      ($positive)
		      ($negative (setq offset (sub offset period)
				       period (neg period)
				       scale (neg scale)))
		      (t (return-from pwilt-parts (list (cons 0 e)))))
		    (list (cons (add offset (take '(%pwilt_periodic) period))
				scale))))
		 (t (list (cons 0 e))))))
	(t (list (cons 0 e)))))

(defun pwilt-one-part (p s x xtemp)
  (let ((xnew (add x (car p)))
	(lexp (cdr p)))
    (cond ((freeof s lexp) (mul lexp (take '($delta) xnew)))
	  ((or $pwilt_hstep_all (not (zerop1 (car p))))
	   (mul ($hstep xnew)
		(maxima-substitute xnew xtemp
				   (pwilt1 lexp s xtemp))))
	  (t (pwilt1 lexp s x)))))

(defun pwilt-subst-periodic (e)
  (let ((counter 0))
    (labels
	((recurse (e) (cond ((and (not (atom e))
				  (eq (caar e) '%pwilt_periodic))
			     (incf counter)
			     (mul -1 '$%k (cadr e)))
			    ((mplusp e)
			     (addn (mapcar #'recurse (cdr e)) t))
			    (t e))))
      (let ((result (recurse e)))
	(and (< counter 2)
	     (freeof '%pwilt_periodic result)
	     result)))))

;;
;; Returns the degree of E in S, or NIL if E is not a polynomial in S
;;
;; XXX Note that this does not expand or simplify, so it may
;;     overestimate the degree if the highest-order term gets
;;     cancelled out!
;;
(defun pwilt-polynomial-degree (e s)
  (labels
      ((recurse (e)
	 (setq e (specrepcheck e))
	 (cond ((alike1 s e) 1)
	       ((freeof s e) 0)
	       ((mplusp e) (reduce #'max (mapcar #'recurse (cdr e))))
	       ((mtimesp e) (reduce #'+ (mapcar #'recurse (cdr e))))
	       ((and (mexptp e)
		     (integerp (mexpt-expt e))
		     (>= (mexpt-expt e) 0))
		(* (mexpt-expt e) (recurse (mexpt-base e))))
	       (t (return-from pwilt-polynomial-degree nil)))))
    (recurse e)))

(defun pwilt1 (e s x)
  (setq e (specrepcheck e))
  (cond ((mbagp e) (cons (car e)
			 (mapcar #'(lambda (ee) (pwilt1 ee s x))
				 (cdr e))))
	((freeof s e) (mul e (take '($delta) x)))
	((mplusp e) (addn (mapcar #'(lambda (ee) (pwilt1 ee s x))
				  (cdr e))
			  t))
	((and (null (atom e))
	      (eq (caar e) '%laplace)
	      (eq (cadddr e) s))
	 (if (eq x (caddr e)) (cadr e)
	   (subst x (caddr e) (cadr e))))
	(t (pwilt2 e s x))))

;;
;; Calculate the inverse laplace transform by the residue method.
;;
;; This works only when e is of the form P(s)/Q(s), where P(s) and
;; Q(s) are polynomials in s, and where the degree of P(s) is less
;; than the degree of Q(s).
;;
(defun pwilt2 (e s x)
  (flet ((fail () (return-from pwilt2 (list '(%ilt simp) e s x))))
    (let* ((n ($num e))
	   (d ($denom e))
	   (n-deg (pwilt-polynomial-degree n s))
	   (d-deg (pwilt-polynomial-degree d s)))
      (unless (and n-deg d-deg (< n-deg d-deg))
	(let ((e2 ($partfrac e s)))
	  (if (alike1 e e2)
	      (fail)
	    (return-from pwilt2 (pwilt1 e2 s x)))))
      (let* (($multiplicities nil)
	     (solns (let (($breakup nil) ($programmode t)
			  ($globalsolve nil) ($solvedecomposes t)
			  ($solveexplicit t) ($solvefactors t))
		      ($solve d s)))
	     ($demoivre t))
	(unless (and ($listp solns)
		     ($listp $multiplicities)
		     (= (length solns)
			(length $multiplicities))
		     (= d-deg (reduce #'+ (cdr $multiplicities)
				      :initial-value 0)))
	  (fail))
	(addn (mapcar #'(lambda (soln m)
			  (unless (and (mequalp soln)
				       (eq s (mequal-lhs soln)))
			    (fail))
			  (maxima-substitute
			   (mequal-rhs soln) s
			   ($diff (mul (power '$%e (mul s x))
				       (div n ($diff d s m))
				       m)
				  s (- m 1))))
		      (cdr solns) (cdr $multiplicities))
	      t)))))

(defun $pwilt (e s x)
  (setq e (specrepcheck e))
  (if (mbagp e)
      (cons (delsimp (car e))
	    (mapcar #'(lambda (ee) ($pwilt ee s x))
		    (cdr e)))
    (let* ((xtemp (gensym))
	   (periodic nil)
	   (result
	    (addn (mapcar #'(lambda (p)
			      (if (freeof '%pwilt_periodic (car p))
				  (pwilt-one-part p s x xtemp)
				(let* ((offset
					(or (pwilt-subst-periodic (car p))
					    (return-from $pwilt
					      (list '(%pwilt) e s x))))
				       (p (cons offset (cdr p))))
				  (setq periodic t)
				  (pwilt-one-part p s x xtemp))))
			  (pwilt-parts e s))
		  t)))
      (if periodic
	  (list '(%sum simp) result '$%k 0 '$inf)
	result))))
