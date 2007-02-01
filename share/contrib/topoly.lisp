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

(defun max-to-abs (e)
  (reduce #'(lambda (a b) (div (add (add a b) (take '(mabs) (sub a b))) 2)) e))

(defun min-to-abs (e)
  (reduce #'(lambda (a b) (div (sub (add a b) (take '(mabs) (sub a b))) 2)) e))

(defun convert-from-max-min-to-abs (e)
  (cond (($mapatom e) e)
	((op-equalp e '$max) (max-to-abs (margs e)))
	((op-equalp e '$min) (min-to-abs (margs e)))
	(t (simplifya `((,(mop e)) ,@(mapcar 'convert-from-max-min-to-abs (margs e))) nil))))

(defun maxima-variable-p (e)
  (or (symbolp e) ($subvarp e)))

(defun suppress-multiple-zeros (q)
  (let ((acc 1) ($factorflag nil))
    (setq q ($factor q))
    (setq q (if (mtimesp q) (margs q) (list q)))
    (dolist (qi q acc)
      (setq acc (mul acc (cond ((mnump qi) (if (eq t (meqp qi 0)) 0 1))
			       ((mexptp qi) (nth 1 qi))
			       (t qi)))))))
  	   
;; topoly(p,vars) returns a polynomial in the variables 'vars' that has a zero whenever
;; p has a zero. When 1 is a member of vars, constant terms, such as sqrt(5) also get
;; converted to polynomial form. The value of vars defaults to all variables including 
;; constants.

(defun $topoly (p &optional (vars 'convert-all-vars))
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
    (setq p ($ratdisrep ($ratnumer ($radcan p))))
    (setq p (convert-from-max-min-to-abs p))
    (setq p ($ratsimp p)) ;; --what's the story here? 
    (setq p (to-polynomial p nil vars convert-cnst))
    (setq subs (second p))
    (setq p (first p))
    (dolist (sk subs)
      (setq nv ($union nv ($setify ($listofvars ($lhs sk))))))
    
    (setq nv ($setdifference nv ($setify vars)))
    (setq nv ($listify nv))
    (setq p (if (null subs) p ($first (mfuncall '$eliminate `((mlist) ,p ,@subs) nv))))
    `((mequal) ,(suppress-multiple-zeros p) 0)))

(defun to-polynomial (p subs vars convert-cnst)
  ;;(print `(p = ,p))
  (cond ((or (maxima-variable-p p)
	     (mnump p)
	     ($emptyp vars)
	     (and (not ($constantp p)) ($lfreeof vars p))
	     (and ($constantp p) (not convert-cnst)))
	 (list p subs))
	     
	((mexptp p)
	 (let ((n (nth 2 p)) (b (nth 1 p)) (nv))
	   (cond ((integerp n) 
		  (setq b (to-polynomial b nil vars convert-cnst))
		  (setq subs (append (second b) subs))
		  (setq b (first b))
		  (if (> n 0) (list (power b n) subs) (merror "Unable to convert to a polynomial equation")))
		 
		 (($ratnump n)
		  (setq b (to-polynomial b nil vars convert-cnst))
		  (setq subs (append (second b) subs))
		  (setq b (first b))
		  (setq nv (gensym))
		  (setq subs (cons `((mequal) ,(power nv ($denom n)) ,(power b ($num n))) subs))
		  (list nv subs))
		 (t (merror "Nonalgebraic argument given to 'topoly'")))))

	((op-equalp p 'mabs)
	 (let ((b) (nv))
	   (setq b (to-polynomial (first (margs p)) nil vars convert-cnst))
	   (setq subs (append (second b) subs))
	   (setq b (first b))
	   (setq nv (gensym))
	   (list nv (cons `((mequal) ,(power nv 2) ,(power b 2)) subs))))
	 
	((mtimesp p)
	 (let ((z 1) (acc nil))
	   (setq p (mapcar #'(lambda (s) (to-polynomial s nil vars convert-cnst)) (margs p)))
	   (dolist (pk p)
	     (setq z (mul z (first pk)))
	     (setq acc (append acc (second pk))))
	   (list z acc)))

	((mplusp p)
	  (let ((z 0) (acc nil))
	    (setq p (mapcar #'(lambda (s) (to-polynomial s nil vars convert-cnst)) (margs p)))
	    (dolist (pk p)
	      (setq z (add z (first pk)))
	      (setq acc (append acc (second pk))))
	    (list z acc)))

	 (t (merror "Nonalgebraic argument given to 'topoly'"))))
