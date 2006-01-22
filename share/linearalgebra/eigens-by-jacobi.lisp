;;  Copyright 2006 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ($put '$infolevel '$debug '|$linalg|)       
  ($put '$eigensbyjacobi 1 '$version))    ;; Let's have version numbers 1,2,3,...

(defun inform (level pck msg &rest arg)
  (if (member level (member ($get '$infolevel pck) `($debug $obnoxious $chatty $silent)))
      (apply 'mtell `(,msg ,@arg))))
		 
;; One sweep zeros each member of the matrix; for a n x n matrix, this requires n(n-1)/2
;; Jacobi rotations. 

;; For double floats, eps is the machine epsilon; for bigfloats, it is 1/2^fpprec.

;; The variable 'change' tracks the greatest percent change in a diagonal entry in
;; a sweep.  When the diagonal entry is less than eps, the percent change set to zero.
;; The iteration stops when 'change' is less than eps (numerical convergence).

;; The matrix entries are computed with numerically friendly formulae--they
;; have the form new value <-- old value + correction.  In general, the 
;; correction is 'small.' These formulae are well-known; I used the reference
;; "Numerical Recipes in Fortran," by Press et.al.

(defun $eigens_by_jacobi (mm &optional (fld-name '$floatfield))
  (if (not (member fld-name `($floatfield $bigfloatfield)))
      (merror "The field must either be 'floatfield' or 'bigfloatfield'"))
  
  (setq mm (mfuncall '$mat_fullunblocker mm))
  ($require_symmetric_matrix mm "$first" "$eigens_by_jacobi")
     
  (let* ((mat (copy-tree mm)) (g) (h) (sweeps 0) (rotations 0) (eps) (change)
	 (theta) (mpq) (c) (s)  (tee) (tau) (d) (v ($identfor mat)) (x)
	 (n ($first ($matrix_size mat))) (continue (> n 1))
	 (fld ($require_ring fld-name "$second" "$eigens_by_jacobi"))
	 (one (funcall (mring-mult-id fld)))
	 (zero (funcall (mring-add-id fld))))
	
    (flet
	((fzerop (a) (funcall (mring-fzerop fld) a))
	 (fabs (a) (funcall (mring-abs fld) a))
	 (fnegate (a) (funcall (mring-negate fld) a))
	 (fpsqrt (a) (funcall (mring-psqrt fld) a))
	 (fadd (a b) (funcall (mring-add fld) a b))
	 (fsub (a b) (funcall (mring-sub fld) a b))
	 (fmult (a b) (funcall (mring-mult fld) a b))
	 (fdiv (a b) (funcall (mring-div fld) a b))
	 (fgreat (a b) (funcall (mring-great fld) a b))
	 (fmax (a b) (if (funcall (mring-great fld) a b) a b))
	 (fconvert (a) (funcall (mring-maxima-to-mring fld) a)))
	 
      (matrix-map mat n n #'fconvert)
      (setq eps (if (eq fld-name '$floatfield) double-float-epsilon ($bfloat (div 1 (power 2 fpprec)))))
             
      (loop for p from 1 to n do (push (array-elem mat p p) d))
      (setq d (reverse d))
      (push '(mlist) d)
      
      (while continue
	(if (> sweeps 50) (merror "Exceeded maximum allowable number of Jacobi sweeps"))
	(incf sweeps)

	(setq change zero)
	(loop for p from 1 to n do 
	  (loop for q from (+ p 1) to n do
	    (setq mpq (array-elem mat p q))
	    (cond ((not (fzerop mpq))
		   (incf rotations)
		   (setq theta (fdiv (fsub (array-elem mat q q) (array-elem mat p p))(fmult 2 mpq)))
		   (setq tee (fdiv one (fadd (fabs theta) (fpsqrt (fadd one (fmult theta theta))))))
		   (if (fgreat 0 theta) (setq tee (fnegate tee)))
		   (setq c (fdiv one (fpsqrt (fadd one (fmult tee tee)))))
		   (setq s (fmult tee c))
		   (setq tau (fdiv s (fadd one c)))
		   (setmatelem mat zero p q)
		   
		   (loop for k from 1 to (- p 1) do
		     (setq g (array-elem mat k p))
		     (setq h (array-elem mat k q))
		     (setmatelem mat (fsub g (fmult s (fadd h (fmult g tau)))) k p)
		     (setmatelem mat (fadd h (fmult s (fsub g (fmult h tau)))) k q))
		   
		   (loop for k from (+ p 1) to (- q 1) do
		     (setq g (array-elem mat p k))
		     (setq h (array-elem mat k q))
		     (setmatelem mat (fsub g (fmult s (fadd h (fmult g tau)))) p k)
		     (setmatelem mat (fadd h (fmult s (fsub g (fmult h tau)))) k q))
		   
		   (loop for k from (+ q 1) to n do
		     (setq g (array-elem mat p k))
		     (setq h (array-elem mat q k))
		     (setmatelem mat (fsub g (fmult s (fadd h (fmult g tau)))) p k)
		     (setmatelem mat (fadd h (fmult s (fsub g (fmult h tau)))) q k))
		   
		   (setmatelem mat (fsub (array-elem mat p p) (fmult tee mpq)) p p)
		   (setmatelem mat (fadd (array-elem mat q q) (fmult tee mpq)) q q)

		   (loop for k from 1 to n do
		     (setq g (array-elem v k p))
		     (setq h (array-elem v k q))
		     (setmatelem v (fsub g (fmult s (fadd h (fmult g tau)))) k p)
		     (setmatelem v (fadd h (fmult s (fsub g (fmult h tau)))) k q))))))
		
	(setq change zero)
	(loop for i from 1 to n do
	  (setq x (array-elem mat i i))
	  (setq change (fmax change (if (fgreat x eps) (fabs (fdiv (fsub (nth i d) x) x)) zero)))
	  (setf (nth i d) x))
	
	(inform '$debug '|$linalg| "The largest percent change was ~:M~%" change)
	(setq continue (fgreat change eps)))
	
      (inform '$chatty '|$linalg| "number of sweeps: ~:M~%" sweeps)
      (inform '$chatty '|$linalg| "number of rotations: ~:M~%" rotations)
      `((mlist) ,d ,v))))

      
				 
		     
		   
		   
		     
		     
		     
		     
		     
		   
		   
		 
  
		