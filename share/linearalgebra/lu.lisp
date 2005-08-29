;;  Copyright 2005 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$lu 1 '$version)

(defun $mytest (fn)
  (let ((*collect-errors* nil))
    (setq fn ($file_search1 fn '((mlist) $file_search_maxima)))
    (test-batch fn nil :show-all t :show-expected t)))

(defstruct mfield
  coerce-to-lisp-float
  abs
  great
  add
  div
  mult
  sub
  add-id
  mult-id
  max
  fzerop
  maxima-to-mfield
  mfield-to-maxima)
			
(defun $require_field (f pos fun)
  (if (not (mfield-p f))
      (merror "The ~:M argument of the function ~:M must be an ordered field" pos fun)))

(defun list-swap (p i j)
  (let ((x (nth i p)))
    (setf (nth i p) (nth j p))
    (setf (nth j p) x)))
	
(defparameter $floatfield (make-mfield
			   :coerce-to-lisp-float #'(lambda (s) s)
			   :abs #'abs
			   :great #'>
			   :add #'+
			   :div #'/
			   :mult #'*
			   :sub #'-
			   :add-id 0.0
			   :mult-id 1.0
			   :max #'max
			   :fzerop #'(lambda (s) (= s 0.0))
			   :mfield-to-maxima #'(lambda (s) s)
			   :maxima-to-mfield #'(lambda (s) 
						 (setq s ($float s))
						 (if (floatp s) s 
						   (merror "Unable to convert matrix entry to a float")))))

(defparameter $complexfloatfield (make-mfield
				  :coerce-to-lisp-float #'(lambda (s) s)
				  :abs #'abs
				  :great #'>
				  :add #'+
				  :div #'/
				  :mult #'*
				  :sub #'-
				  :add-id 0.0
				  :mult-id 1.0
				  :max #'max
				  :fzerop #'(lambda (s) (= s 0.0))
				  :mfield-to-maxima #'(lambda (s) (add (realpart s) (mul '$%i (imagpart s))))
				  :maxima-to-mfield 
				  #'(lambda (s) 
				      (setq s ($rectform s))
				      (if (and (floatp ($float ($realpart s))) (floatp ($float ($imagpart s))))
					  (complex ($float ($realpart s)) ($float ($imagpart s)))
					(merror "Unable to convert matrix entry to a complex float")))))


(defparameter $rationalfield (make-mfield
			      :coerce-to-lisp-float #'(lambda (s) ($float s))
			      :abs #'abs
			      :great #'>
			      :add #'+
			      :div #'/
			      :mult #'*
			      :sub #'-
			      :add-id 0
			      :mult-id 1
			      :max #'max
			      :fzerop #'(lambda (s) (= s 0))
			      :mfield-to-maxima #'(lambda (s) 
						    (simplify `((rat) ,(numerator s) ,(denominator s))))
			      :maxima-to-mfield 
			      #'(lambda (s) 
				  (if ($ratnump s) (if (integerp s) s (/ ($num s) ($denom s)))
				    (merror "Unable to convert matrix entry to a rational number")))))

(defparameter $crefield (make-mfield
			 :coerce-to-lisp-float nil
			 :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
			 :great #'(lambda (a b) (like b 0))
			 :add #'add
			 :div #'div
			 :mult #'mult
			 :sub #'sub
			 :add-id 0
			 :mult-id 1
			 :max  #'(lambda (a b)  (maximum (list a b)))
			 :fzerop #'(lambda (s) (like s 0))
			 :mfield-to-maxima #'(lambda (s) s)
			 :maxima-to-mfield #'(lambda (s) ($rat s))))
		
(defparameter $generalfield (make-mfield
			     :coerce-to-lisp-float nil
			     :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
			     :great #'(lambda (a b) (like b 0))
			     :add #'(lambda (a b) ($rectform (add a b)))
			     :div #'(lambda (a b) ($rectform (div a b)))
			     :mult #'(lambda (a b) ($rectform (mult a b)))
			     :sub #'(lambda (a b) ($rectform (sub a b)))
			     :add-id 0
			     :mult-id 1
			     :max #'(lambda (a b)  (maximum (list a b)))
			     :fzerop #'(lambda (s) (like s 0))
			     :mfield-to-maxima #'(lambda (s) s)
			     :maxima-to-mfield #'(lambda (s) s)))

(defparameter $bigfloatfield (make-mfield
			      :coerce-to-lisp-float #'(lambda (s) 
							(setq s ($rectform ($float s)))
							(complex ($realpart s) ($imagpart s)))
			      
			      :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
			      :great #'mgrp
			      :add #'(lambda (a b) ($rectform (add a b)))
			      :div #'(lambda (a b) ($rectform (div a b)))
			      :mult #'(lambda (a b) ($rectform (mult a b)))
			      :sub #'(lambda (a b) ($rectform (sub a b)))
			      :add-id 0
			      :mult-id 1
			      :max #'(lambda (a b)  (maximum (list a b)))
			      :fzerop #'(lambda (s) (like s bigfloatzero))
			      :mfield-to-maxima #'(lambda (s) s)
			      :maxima-to-mfield #'(lambda (s) 
						    (setq s ($bfloat s))
						    (if ($bfloatp s) s
						      (merror "Unable to convert matrix entry to a big float")))))

;; Map the lisp function fn over the r by c Maxima matrix m.

(defun matrix-map (m r c fn)
  (loop for i from 1 to r do
    (loop for j from 1 to c do
      (setmatelem m (funcall fn (nth j (nth i m))) i j))))

;; Return the i,j entry of the Maxima matrix m. The rows of m have been permuted according
;; to the Maxima list p.

(defun m-elem (m p i j)
  (nth j (nth (nth i p) m)))

;; Set the i j entry of the Maxima matrix m to x.

(defun setmatelem (m x i j) 
  (setf (nth j (nth i m)) x))

;; Return m[perm[i], k] - sum(m[perm[i],s] * m[perm[s],k],s,1,n)

(defun partial-matrix-prod (m p i k n fadd fsub fmult add-id)
  (loop for s from 1 to n do
    (setq add-id (funcall fadd add-id (funcall fmult (m-elem m p i s) (m-elem m p s k)))))
  (setmatelem m (funcall fsub (m-elem m p i k) add-id) (nth i p) k))

(defun $lu_factor (mm &optional (fld $generalfield))
  ($require_square_matrix mm "$first" "$lu_factor")
  ($require_nonempty_matrix mm "$first" "$lu_factor")
  ($require_field fld "$second" "$lu_factor")

  (let ((m (copy-tree mm)) (c ($length mm)) (perm))
    (matrix-map m c c (mfield-maxima-to-mfield fld))
    (loop for i from 1 to c do (push i perm))
    (setq perm (reverse perm))
    (push '(mlist) perm)
    (lu-factor m perm c fld)))

(defun $get_lu_factors (x)
  (let ((mat ($first x)) (mp) (p ($second x)) (perm) (r) (c) (id) (lower) (upper))

    (setq r ($matrix_size mat))
    (setq c ($second r))
    (setq r ($first r))
    (setq id ($args ($ident c)))
    (loop for i from 1 to c do
	 (push (nth (nth i p) id) perm)
	 (push (nth (nth i p) mat) mp))
    (setq perm (reverse perm))
    (setq mp (reverse mp))
    (push '($matrix) perm)
    (push '($matrix) mp)

    (setq lower (copy-tree mp))
    (setq upper (copy-tree mp))
    
    (loop for i from 1 to r do
      (loop for j from 1 to c do
	(if (= i j) (setmatelem lower 1 i j))
	(if (> i j) (setmatelem upper 0 i j))
	(if (< i j) (setmatelem lower 0 i j))))

    `((mlist) ,($transpose perm) ,lower ,upper)))
        

(defun lu-factor (m perm c fld)
  (let ((pos) (kp1) (mx) (cnd)
	(fadd (mfield-add fld))
	(fsub (mfield-sub fld))
	(fmult (mfield-mult fld))
	(fdiv (mfield-div fld))
	(fabs (mfield-abs fld))
	(fgreat (mfield-great fld))
	(fzerop (mfield-fzerop fld))
	(add-id (mfield-add-id fld)))
    
    (loop for k from 1 to c do 
      (loop for i from k to c  do (partial-matrix-prod m perm i k (- k 1) fadd fsub fmult add-id))
      (setq mx (funcall fabs (m-elem m perm k k)))
      (setq pos k)
      (loop for s from k to c do
	(if (funcall fgreat 
		     (funcall fabs (m-elem m perm s k)) mx) (setq pos s mx 
								  (funcall fabs (m-elem m perm s k)))))
      (list-swap perm k pos)

      (setq kp1 (+ 1 k))
      (loop for i from kp1 to c do
	(if (funcall fzerop (m-elem m perm k k)) (merror "Unable to compute the LU factorization"))
	(setmatelem m (funcall fdiv (m-elem m perm i k) (m-elem m perm k k)) (nth i perm) k)
	(partial-matrix-prod m perm k i (- k 1) fadd fsub fmult add-id)))

    (cond ((not (eq nil (mfield-coerce-to-lisp-float fld)))
	   (setq cnd (mat-cond-by-lu m perm c (mfield-coerce-to-lisp-float fld)))
	   (matrix-map m c c (mfield-mfield-to-maxima fld))
	   
	   `((mlist) ,m ,perm ,cnd))
	  (t `((mlist) ,m ,perm)))))
    

(defun row-sum (mat i start stop fn)
  (let ((acc 0.0))
    (setq mat (nth i mat))
    (loop for i from start to stop do
      (setq acc (+ acc (abs (funcall fn (nth i mat))))))
    acc))
    
(defun mat-cond-by-lu (mat perm n fn)
  (let ((l-norm) (l-cond) (u-norm) (uu-norm) (u-cond) (d))
    (setq l-norm 0.0)
    (loop for i from 2 to n do (setq l-norm (max l-norm (row-sum mat (nth i perm) 1 (- i 1) fn))))
    (setq l-cond (* (+ 1 l-norm) (/ (- 1 (expt l-norm n)) (- 1 l-norm))))

    (setq u-norm 0.0)
    (setq uu-norm 0.0)
   
    (loop for i from 1 to n do
      (setq u-norm (max u-norm (row-sum mat (nth i perm) i n fn)))
      (setq d (abs (funcall fn (m-elem mat perm i i))))
      (cond ((= d 0.0)
	     (setq uu-norm '$inf)
	     (return nil)))
      (setq uu-norm (max uu-norm (/ (row-sum mat (nth i perm) (+ i 1) n fn) d))))
      
    (cond ((eq uu-norm '$inf) '$inf)
	  (t
	   (setq u-cond (* uu-norm (/ (- 1 (expt uu-norm n)) (- 1 uu-norm))))
	   (* l-cond u-cond)))))

(defun $lu_backsub(m b1 &optional (fld $generalfield))
   
  ($require_list m "$first" "$lu_backsub")
  ($require_field fld "$second" "$lu_backsub")
  (let ((mat) (n) (r) (c) (bb) (acc) (perm) (id-perm) (b)
	(fadd (mfield-add fld))
	(fsub (mfield-sub fld))
	(fmult (mfield-mult fld))
	(fdiv (mfield-div fld))
	(add-id (mfield-add-id fld)))
    
    (setq mat (copy-tree ($first m)))
    (setq perm ($second m))
    (setq n ($matrix_size mat))
    (setq r ($first n))
    (setq c ($second n))
    (matrix-map mat r c (mfield-maxima-to-mfield fld))

    (setq b (copy-tree b1))
    (matrix-map b r 1 (mfield-maxima-to-mfield fld))

    (setq bb (copy-tree b))
    (loop for i from 1 to r do
      (setmatelem bb (m-elem b perm i 1) i 1))

    (setq id-perm (mfuncall '$makelist 'i 'i 1 r))

    (loop for i from 2 to c do
      (setq acc add-id)
      (loop for j from 1 to (- i 1) do
	(setq acc (funcall fadd acc (funcall fmult (m-elem mat perm i j) (m-elem bb id-perm j 1)))))
      (setmatelem bb (funcall fsub (m-elem bb id-perm i 1) acc) i 1))
    
    (loop for i from r downto 1 do
      (setq acc (m-elem bb id-perm i 1))
      (loop for j from (+ 1 i) to c do
	(setq acc (funcall fsub acc (funcall fmult (m-elem mat perm i j) (m-elem bb id-perm j 1)))))
      (setmatelem bb (funcall fdiv acc (m-elem mat perm i i)) i 1))
        
    (matrix-map bb r 1 (mfield-mfield-to-maxima fld))
    bb))



    
	      
