;;  Author Barton Willis
;;  University of Nebraska at Kearney
;;  Copyright (C) 2006, 2007 Barton Willis

;;  This program is free software; you can redistribute it and/or modify 
;;  it under the terms of the GNU General Public License as published by	 
;;  the Free Software Foundation; either version 2 of the License, or		 
;;  (at your option) any later version.					 
 		       								 
;;  This program is distributed in the hope that it will be useful,		 
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of		 
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the		 
;;  GNU General Public License for more details.				 
 		       								 
;;  You should have received a copy of the GNU General Public License	
;;  along with this program; if not, write to the Free Software 		 
;;  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; The next three functions convert max and min to abs functions.

(defun max-to-abs (e)
  (reduce #'(lambda (a b) (div (add (add a b) (take '(mabs) (sub a b))) 2)) e))

(defun min-to-abs (e)
  (reduce #'(lambda (a b) (div (sub (add a b) (take '(mabs) (sub a b))) 2)) e))

(defun convert-from-max-min-to-abs (e)
  (cond (($mapatom e) e)
	((op-equalp e '$max) (max-to-abs (mapcar 'convert-from-max-min-to-abs (margs e))))
	((op-equalp e '$min) (min-to-abs (mapcar 'convert-from-max-min-to-abs (margs e))))
	(t (simplifya `((,(mop e)) ,@(mapcar 'convert-from-max-min-to-abs (margs e))) nil))))

(defun maxima-variable-p (e)
  (or (symbolp e) ($subvarp e)))

;; Orphaned, but might still be useful.

(defun suppress-multiple-zeros (q)
  (let ((acc 1) ($factorflag nil))
    (setq q ($sqfr q))
    (setq q (if (mtimesp q) (margs q) (list q)))
    (dolist (qi q acc)
      (setq acc (mul acc (cond ((mnump qi) (if (eq t (meqp qi 0)) 0 1))
			       ((mexptp qi) (nth 1 qi))
			       (t qi)))))))
  	   
;; to_poly(p,vars) returns a polynomial in the variables 'vars' that has a zero whenever
;; p has a zero. When 1 is a member of vars, constant terms, such as sqrt(5) also get
;; converted to polynomial form. The value of vars defaults to all variables including 
;; constants.

(defun $to_poly (p &optional (vars 'convert-all-vars))
  (let (($listconstvars t) (subs) (q) (convert-cnst nil) (nv `(($set)))) ;; new variables

    (if (eq vars 'convert-all-vars) (setq vars ($cons 1 ($listofvars p))))
    
    (if (not ($listp vars))
	(merror "The second argument to 'topoly' must be a list"))
    
    (cond (($member 1 vars) 
	   (setq convert-cnst t)
	   (setq vars ($delete 1 vars))))

    (setq p (meqhk p))
    (setq q ($ratdenom p))
    (if (not ($constantp q)) (mtell "Assuming that ~:M " `((mnotequal) ,q 0)))
    (setq p ($ratdisrep ($ratnumer p))) ;;($radcan p))))

    ;; It's OK to send every expression through convert-from-max-min-to-abs.
    ;; I put in the conditional to skip the ratsimp for expressions that don't
    ;; involve max or min.

    (setq p (if ($freeof '$max '$min p) p ($ratsimp (convert-from-max-min-to-abs p))))
    
    (setq p (to-polynomial p nil vars convert-cnst nil))
    (setq q (third p))
    `((mlist) ((mlist) ,(first p) ,@(second p)) ((mlist) ,@(third p)))))))
   
(defun to-polynomial (p acc vars convert-cnst inequal)
  (cond ((or (maxima-variable-p p)
	     (mnump p)
	     (and ($emptyp vars) (not convert-cnst))
	     (and (not ($constantp p)) ($lfreeof vars p))
	     (and ($constantp p) (not convert-cnst)))
	 (list p acc inequal))
	     
	((mexptp p)
	 (let ((n (nth 2 p)) (b (nth 1 p)) (nv))
	   (cond ((integerp n) 
		  (setq b (to-polynomial b nil vars convert-cnst nil))
		  (setq acc (append (second b) acc))
		  (setq inequal (append (third b) inequal))
		  (setq b (first b))
		  (if (> n 0) (list (power b n) acc inequal) 
		    (merror "Unable to convert to polynomial form")))
		 
		 (($ratnump n)
		  (setq b (to-polynomial b nil vars convert-cnst nil))
		  (setq acc (append (second b) acc))
		  (setq inequal (append (third b) inequal))
		  (setq b (first b))
		  (setq nv (gentemp "$%G"))
		  (setq inequal (cons `((mleqp) (($carg) ,nv) ((mtimes) ,n $%pi)) inequal))
		  (setq inequal (cons `((mlessp) ((mtimes) -1 $%pi ,n) (($carg) ,nv)) inequal)) 
		  (setq acc (cons (sub (power nv ($denom n)) (power b ($num n))) acc))
		  (list nv acc inequal))
		 (t (merror "Nonalgebraic argument given to 'topoly'")))))

	((op-equalp p 'mabs)
	 (let ((b) (nv))
	   (setq b (to-polynomial (first (margs p)) nil vars convert-cnst nil))
	   (setq acc (append (second b) acc))
	   (setq inequal (append (third b) inequal))
	   (setq b (first b))
	   (setq nv (gentemp "$%G"))
	   (list nv (cons (sub (power nv 2) (power b 2)) acc) (cons `((mequal) (($carg) ,nv) 0) inequal))))
	 
	((mtimesp p)
	 (let ((z 1) (acc nil))
	   (setq p (mapcar #'(lambda (s) (to-polynomial s nil vars convert-cnst nil)) (margs p)))
	   (dolist (pk p)
	     (setq z (mul z (first pk)))
	     (setq acc (append acc (second pk)))
	     (setq inequal (append inequal (third pk))))
	   (list z acc inequal)))

	((mplusp p)
	  (let ((z 0) (acc nil))
	    (setq p (mapcar #'(lambda (s) (to-polynomial s nil vars convert-cnst nil)) (margs p)))
	    (dolist (pk p)
	      (setq z (add z (first pk)))
	      (setq acc (append acc (second pk)))
	      (setq inequal (append inequal (third pk))))
	    (list z acc inequal)))

	(t (merror "Nonalgebraic argument given to 'topoly'"))))


