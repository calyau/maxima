;; Maxima functions for finding the maximum or minimum
;; Copyright (C) 2005, Barton Willis

;; Barton Willis
;; Department of Mathematics, 
;; University of Nebraska at Kearney
;; Kearney NE 68847
;; willisb@unk.edu

;; This source code is licensed under the terms of the Lisp Lesser 
;; GNU Public License (LLGPL). The LLGPL consists of a preamble, published
;; by Franz Inc. (http://opensource.franz.com/preamble.html), and the GNU 
;; Library General Public License (LGPL), version 2, or (at your option)
;; any later version.  When the preamble conflicts with the LGPL, 
;; the preamble takes precedence. 

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Library General Public License for details.

;; You should have received a copy of the GNU Library General Public
;; License along with this library; if not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301, USA.

(in-package :maxima)
(macsyma-module maxmin)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ($put '$trylevel 1 '$maxmin)  ;; Default: only use basic simplification rules
  ($put '$maxmin 1 '$version))  ;; Let's have version numbers 1,2,3,...

;; Return true if there is pi in the CL list p and qi in the CL lisp q such that
;; x is between pi and qi.  This means that either pi <= x <= qi or
;; qi <= x <= pi. For example, 2x is between x and 3x.

;; Strangely, sign((a-b)*(b-a)) --> pnz but sign(expand((a-b)*(b-a))) --> nz.
;; This is the reason for the $expand.

;; The betweenp simplification is done last; this has some interesting effects:
;; max(x^2,x^4,x^6,x^2+1) (standard simplification) --> max(x^4,x^6,x^2+1) 
;; (betweenp) --> max(x^4,x^6,x^2+1).  If the betweenp simplification were done 
;; first, we'd have max(x^2,x^4,x^6,x^2+1) --> max(x^2,x^6,x^2+1) --> max(x^6,x^2+1).
	  
(defun betweenp (x p q)
  (let ((sgn) ($prederror nil))
    (cond ((or (null p) (null q)) nil)
	  (t
	   (setq sgn (csign ($expand (mul ($limit (sub x (car p))) ($limit (sub (car q) x))))))
	   (or 
	    (eq sgn '$pos) (eq sgn '$pz)
	    (betweenp x (list (first p)) (cdr q))
	    (betweenp x (cdr p) (list (first q))))))))

;; Return true iff expand(x + y) evaluates to zero. 

(defun add-inversep (x y)
  (like 0 ($expand (add x y))))
	    
;; When get(trylevel,maxmin) is two or greater, max and min try additional 
;; O(n^2) and O(n^3) methods.
 
;; Undone:  max(1-x,1+x) - max(x,-x) --> 1.

(defprop $max simp-max operators)

(defun simp-max (l tmp z)
  (cond ((member 'simp (car l)) l)
	(t
	 (let ((limitp t) (acc) (sgn) (num-max '$minf) (issue-warning))
	   (let (($simp nil)) (setq l ($flatten l)))
	   (setq l (mapcar #'(lambda (x) (simplifya x z)) (cdr l)))
	   (setq l (mapcar #'specrepcheck l))
	   
	   ;; It's reasonable to map $limit onto l. But $limit makes a mess for
	   ;; some (admittedly oddball) cases: try limit(true), limit(false), and
	   ;; limit(?foo).

	   ;;(setq l (mapcar #'$limit l))
	   		 
	   ;; We begin by finding the largest number in l. Alternatively,
	   ;; we could find the largest constant expression, but we'd need
	   ;; to be careful to expunge complex valued constant expressions.
	   ;; Notice that constantp(%i) --> true.  At least for now, (mnump '$%i) 
	   ;; is false. If this changes, it will break this code.
	  	    
	   (dolist (li l)
	     (if (mnump li) (setq num-max (if (mgrp li num-max) li num-max)) (push li acc)))
	   (setq l acc)
	   (setq acc (list num-max))

	   ;; When e and -e are members of l, replace e by |e|. Do this only when
	   ;; trylevel is 2 or higher.  
	   
	   (cond ((eq t (mgrp ($get '$trylevel '$maxmin) 1))
		  (setq sgn nil)
		  (dolist (li l)
		    (setq tmp (if (lenient-realp li) (member-if #'(lambda (s) (add-inversep li s)) sgn) nil))
		    (if tmp (setf (car tmp) (simplify `((mabs) ,li))) (push li sgn)))
		  (setq l sgn)))
		    
	   (dolist (x l)
	     (setq sgn (mapcar #'(lambda (s) (list ($compare s x) s)) acc))
	     (setq sgn (delete-if #'(lambda (s) (member (car s) `(&< &= &<=))) sgn))
	     (if (or (null sgn) 
		     (and (not (member '&> (mapcar #'car sgn)))
			  (not (member '&>= (mapcar #'car sgn)))))
		 (push `('&= ,x) sgn))
	     (setq acc (mapcar #'second sgn)))
	   
	   (setq issue-warning (member '$notcomparable (mapcar #'car sgn)))

	   ;; Skip the betweenp simplification when issue-warning is true.  
	   ;; Try the betweenp simplification when trylevel is 3 or higher.

	   (cond ((and (not issue-warning) (eq t (mgrp ($get '$trylevel '$maxmin) 2)))
		  (setq l nil)
		  (setq sgn (cdr acc))
		  (dolist (ai acc)
		    (if (not (betweenp ai sgn sgn)) (push ai l))
		    (setq sgn `(,@(cdr sgn) ,ai)))
		  (setq acc l)))
	   
	   (cond ((null acc) '$minf)
		 ((null (cdr acc)) (car acc))
		 (t 
		  (setq acc (delete '$minf acc))
		  (if issue-warning 
		      (mtell "Nonorderable argument(s) in 'max' or 'min.' Returning a noun form.~%"))
		  `(($max simp) ,@(sort acc 'great))))))))

(defprop $min simp-min operators)

(defun limitneg (x)
  (if ($freeof '$minf '$inf x) (neg x) ($limit (neg x))))

(defun simp-min (l tmp z)
  (cond ((member 'simp (car l)) l)
	(t
	 (let (($simp nil)) (setq l ($flatten l)))
	 (setq l (mapcar #'limitneg (margs l)))
	 (setq l (simp-max `(($max) ,@l) tmp z))
	 (if (op-equalp l '$max)
	     `(($min simp) ,@(mapcar #'limitneg (margs l))) (limitneg l)))))

;; Several functions (derivdegree for example) use the maximin function.  Here is 
;; a replacement that uses simp-min or simp-max.

(defun maximin (l op) (simplify `((,op) ,@l)))
 
(defun $lmax (e)
  (simp-max `(($max) ,@(require-list-or-set e "$lmax")) nil nil))

(defun $lmin (e)
  (simp-min `(($min) ,@(require-list-or-set e "$lmin")) nil nil))

;; Return the narrowest comparison operator op (<, <=, =, >, >=) such that
;; a op b evaluates to true. Return 'unknown' when either there is no such 
;; operator or a Maxima's sign function isn't powerful enough to determine
;; such an operator; when Maxima is able to show that either argument is not 
;; real valued, return 'notcomparable.'

;; The subtraction can be a problem--for example, compare(0.1, 1/10)
;; evaluates to "=". But for double floats, I believe 0.1d0 > 1/10. 
;; If you want to convert double and big floats to exact rational
;; numbers, use $rationalize.

;; I think compare(asin(x), asin(x) +1) should evaluate to < without
;; being quizzed about the sign of x.  Thus the call to lenient-extended-realp.

(defun $compare (a b)
  (let ((sgn) ($prederror nil))
    (cond  
     ((like ($limit a) ($limit b)) '&=)
     ((or (not (lenient-extended-realp a)) (not (lenient-extended-realp b))) '$notcomparable)
     ((eq (setq sgn (csign ($limit ($ratsimp (sub a b))))) '$neg) '&<)
     ((eq sgn '$nz) '&<=)
     ((eq sgn '$zero) '&=)
     ((eq sgn '$pz) '&>=)
     ((eq sgn '$pos) '&>)
     ((eq sgn '$pn) '&#)
     ((eq sgn '$pnz) '$unknown)
     (t '$unknown))))
  
;; When it's fairly likely that the real domain of e is nonempty, return true; 
;; otherwise, return false. Even if z has been declared complex, the real domain
;; of z is nonempty; thus (lenient-extended-realp z) --> true.  When does this
;; function lie?  One example is acos(abs(x) + 2). The real domain of this 
;; expression is empty, yet lenient-extended-realp returns true for this input.

(defun lenient-extended-realp (e)
  (and ($freeof '$infinity '$%i '$und '$ind '$false '$true t nil e) ;; what else?
       (not (mbagp e))
       (not ($featurep e '$nonscalarp))
       (not (mrelationp e))
       (not ($member e $arrays))))

(defun lenient-realp (e)
  (and ($freeof '$inf '$minf e) (lenient-extended-realp e)))

;; Convert all floats and big floats in e to an exact rational representation. 

(defun $rationalize (e)
  (cond ((floatp e) (cl-rat-to-maxima (rationalize e)))
	(($bfloatp e) (cl-rat-to-maxima (* (cadr e)(expt 2 (- (caddr e) (third (car e)))))))
	(($mapatom e) e)
	(t (mfuncall '$fullmap '$rationalize e))))

