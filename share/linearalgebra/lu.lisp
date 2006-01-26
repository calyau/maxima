;;  Copyright 2005 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$lu 1 '$version)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ($load "mring"))
;;  (if (not (functionp '$rationalize)) ($load "nummod")))

(defun $mytest (fn)
  (let ((*collect-errors* nil))
    (setq fn ($file_search1 fn '((mlist) $file_search_maxima)))
    (test-batch fn nil :show-all t :show-expected t)))

(defun list-swap (p i j)
  (let ((x (nth i p)))
    (setf (nth i p) (nth j p))
    (setf (nth j p) x)))
	
;; Map the lisp function fn over the r by c Maxima matrix m.  This function isn't
;; block matrix friendly.

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
	 
;; Return the infinity norm (the largest row sum) of the r by c matrix mat. The function
;; fn coerces matrix elements into double floats. The argument 'mat' is a Maxima
;; style matrix; thus mat = (($matrix) ((mlist) a b c) etc).

(defun mat-infinity-norm (mat fn)
  (reduce 'max (mapcar #'(lambda (s) (reduce #'+ (mapcar #'(lambda (p) (abs (funcall fn p))) s))) 
		      (mapcar #'cdr (cdr mat)))))

(defun $lu_factor (mm &optional (fld '$generalring))
  ($require_square_matrix mm "$first" "$lu_factor")
  ($require_nonempty_matrix mm "$first" "$lu_factor")
  (setq fld ($require_ring fld "$second" "$lu_factor"))

  (let ((m (copy-tree mm)) (c ($length mm)) (perm) (cnd) (fn))
    (matrix-map m c c (mring-maxima-to-mring fld))
    (loop for i from 1 to c do (push i perm))
    (setq perm (reverse perm))
    
    ;; When the matrix elements can be converted to CL floats, find
    ;; the infinity norm of m. The norm is used to bound the condition
    ;; number.

    (cond ((not (eq nil (mring-coerce-to-lisp-float fld)))
	   (setq fn (mring-coerce-to-lisp-float fld))
	   (setq cnd (mat-infinity-norm m fn)))
	  (t (setq cnd 0.0)))
    (push '(mlist) perm)
    (lu-factor m perm c fld cnd)))

(defun $get_lu_factors (x)
  (let ((mat ($first x)) (mp) (p ($second x)) (perm) (r) (c) (id) (lower) (upper) (zero))

    (setq r ($matrix_size mat))
    (setq c ($second r))
    (setq r ($first r))
    (setq id ($args ($identfor mat)))
    (loop for i from 1 to c do
	 (push (nth (nth i p) id) perm)
	 (push (nth (nth i p) mat) mp))
    (setq perm (reverse perm))
    (setq mp (reverse mp))
    (push '($matrix) perm)
    (push '($matrix) mp)

    (setq lower (copy-tree mp))
    (setq upper (copy-tree mp))
    
    (setq id ($identfor ($first ($first mat))))
    (setq zero ($zerofor ($first ($first mat))))
    (loop for i from 1 to r do
      (loop for j from 1 to c do
	(if (= i j) (setmatelem lower id i j))
	(if (> i j) (setmatelem upper zero i j))
	(if (< i j) (setmatelem lower zero i j))))	
    `((mlist) ,($transpose perm) ,lower ,upper)))
        
(defun lu-factor (m perm c fld &optional (cnd 1.0))
  (let ((pos) (kp1) (mx) (lb) (ub)
	(fadd (mring-add fld))
	(fsub (mring-sub fld))
	(fmult (mring-mult fld))
	(fdiv (mring-div fld))
	(fabs (mring-abs fld))
	(fgreat (mring-great fld))
	(fzerop (mring-fzerop fld))
	(add-id (funcall (mring-add-id fld))))
    
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
    
    (cond ((not (eq nil (mring-coerce-to-lisp-float fld)))
	   (multiple-value-setq (lb ub) (mat-cond-by-lu m perm c (mring-coerce-to-lisp-float fld)))
	   (setq lb ($limit (mul lb cnd)))
	   (setq ub ($limit (mul ub cnd)))
	   (matrix-map m c c (mring-mring-to-maxima fld))
	   `((mlist) ,m ,perm ,(mring-name fld) ,lb ,ub))
	  (t 
	   (matrix-map m c c (mring-mring-to-maxima fld))
	   `((mlist) ,m ,perm ,(mring-name fld))))))
        
;; The first argument should be a matrix in packed LU form. The Maxima list perm
;; specifies the true row ordering. When r is false, reflect the matrix horizontally
;; and vertically.

(defun m-elem-reflect (mat perm n r i j)
  (cond ((and r (= i j)) 1)
	(r (m-elem mat perm (+ n (- i) 1) (+ n (- j) 1)))
	(t (m-elem mat perm i j))))

;; The first argument mat should be a matrix in the packed LU format. 
;; When l-or-u is lower, return the i,j element of the lower triangular
;; factor; when l-or-u is upper, return the j, i element of upper triangular
;; factor. The first argument mat should be a matrix in the packed LU format. 

(defun mat-cond-by-lu (mat perm n fn)
  (let ((lb0) (ub0) (lb1) (ub1))
    (multiple-value-setq (lb0 ub0) (triangular-mat-cond mat perm n fn t))
    (multiple-value-setq (lb1 ub1) (triangular-mat-cond mat perm n fn nil))
    (values ($limit (mul lb0 lb1)) ($limit (mul ub0 ub1)))))

;; Return lower and upper bounds for the infinity norm condition number of the lower or
;; upper triangular part of the matrix mat. The function fn coerces the matrix
;; elements to double floats.  When the matrix is singular, return infinity.
;; This code is based on pseudo-code (algorithm 2.1) in ``Survey of condition 
;; number estimation,'' by Nicholas J. Higham, SIAM Review, Vol. 29, No. 4, December,
;; 1987.  The lower and upper bounds can differ from the true value by arbitrarily 
;; large factors. 

(defun triangular-mat-cond (mat perm n fn r)
  (let ((z) (d-max 0.0) (z-max 0.0) (s) (d))
    (setq z (make-array (+ 1 n)))
    (catch 'singular
      (loop for i from n downto 1 do
	(setq d (abs (funcall fn (m-elem-reflect mat perm n r i i))))
	(if (= 0.0 d) (throw 'singular (values '$inf '$inf)) (setq d (/ 1 d)))
	(setq d-max (max d-max d))
	(setq s 1.0)
	(loop for j from (+ 1 i) to n do
	  (incf s (* (abs (funcall fn (m-elem-reflect mat perm n r i j))) (aref z j))))
	(setf (aref z i) (* d s))
	(setq z-max (max z-max (aref z i))))
      (values d-max z-max))))
      

(defun $lu_backsub(m b1)
   
  ($require_list m "$first" "$lu_backsub")
  (if (< ($length m) 3) (merror "The first argument to 'lu_backsub' must be a list with at least 3 members"))
  
  (let* ((mat) (n) (r) (c) (bb) (acc) (perm) (id-perm) (b) 
	 (fld (get (mfuncall '$ev (fourth m)) 'ring)) (cc) 
	 (fadd (mring-add fld))
	 (fsub (mring-sub fld))
	 (fmult (mring-mult fld))
	 (fdiv (mring-rdiv fld))
	 (add-id (funcall (mring-add-id fld))))
    
    (setq mat (copy-tree ($first m)))
    (setq perm ($second m))
    (setq n ($matrix_size mat))
    (setq r ($first n))
    (setq c ($second n))
    
    (matrix-map mat r c (mring-maxima-to-mring fld))
    ;(displa `((mequal) mat ,mat))
    (setq b (copy-tree b1))
    (setq c ($second ($matrix_size mat)))
    
    (setq cc ($second ($matrix_size b)))
    (matrix-map b r cc (mring-maxima-to-mring fld))

    (setq bb (copy-tree b))
    (loop for i from 1 to r do
      (loop for j from 1 to cc do
	(setmatelem bb (m-elem b perm i j) i j)))
    
    (setq id-perm (mfuncall '$makelist 'i 'i 1 r))

    (loop for q from 1 to cc do 
      (loop for i from 2 to c do
	(setq acc add-id)
	(loop for j from 1 to (- i 1) do
	  (setq acc (funcall fadd acc (funcall fmult (m-elem mat perm i j) (m-elem bb id-perm j q)))))
	(setmatelem bb (funcall fsub (m-elem bb id-perm i q) acc) i q)))
    
    (loop for q from 1 to cc do
      (loop for i from r downto 1 do
	(setq acc (m-elem bb id-perm i q))
	(loop for j from (+ 1 i) to c do
	  (setq acc (funcall fsub acc (funcall fmult (m-elem mat perm i j) (m-elem bb id-perm j q)))))
	(setmatelem bb (funcall fdiv acc (m-elem mat perm i i)) i q)))
    
    (matrix-map bb r cc (mring-mring-to-maxima fld))
    bb))

(defun $invert_by_lu (m  &optional (fld '$generalring))
  ($require_square_matrix m "$first" "$invert_by_lu")
  ;($lu_backsub ($lu_factor m fld) ($dotidentmatrix ($first ($matrix_size m)))))
  ($lu_backsub ($lu_factor m fld) ($identfor m)))
  
#|
(defun $determinant_by_lu (m &optional (fld $generalring))
  ($require_square_matrix m "$first" "$determinant_by_lu")
 
  (let ((acc (funcall (mring-mult-id fld) nil))
	(fmult (mring-mult fld))
	(n ($first ($matrix_size m)))
	(perm))

    (setq m ($lu_factor m fld))
    (setq perm ($second m))
    (setq m ($first m))
    (loop for i from 1 to n do
      (setq d (m-elem m perm i i))
      (displa `((mequal) d ,d))
      (if ($matrixp d) (setq d ($determinant_by_lu d fld)))
      (setq acc (funcall fmult acc d)))
    acc))
|#

(defun $mat_cond (m p)
  ($require_square_matrix m "$first" "$mat_cond")
  (mul (simplify (mfunction-call |$mat_norm| m p))
       (simplify (mfunction-call |$mat_norm| ($invert_by_lu m) p))))

;; Return an identity matrix that has the same shape as the matrix
;; mat. The first argument 'mat' should be a square Maxima matrix or a 
;; non-matrix. When 'mat' is a matrix, each entry of 'mat' can be a
;; square matrix -- thus 'mat' can be a blocked Maxima matrix. The
;; matrix can be blocked to any (finite) depth.

(defun $identfor (mat &optional (fld-name '$generalring) (p 1) (q 1))
  ($require_ring fld-name "$second" "$identfor")
  (let* ((fld (get fld-name 'ring))
	 (add-id (funcall (mring-mring-to-maxima fld) (funcall (mring-add-id fld))))
	 (mul-id (funcall (mring-mring-to-maxima fld) (funcall (mring-mult-id fld)))))
	
    (cond (($matrixp mat)
	   ($require_square_matrix mat "$first" "$identfor")
	   (let ((n ($first ($matrix_size mat))) (mc))
	     (setq mc (copy-tree mat))
	     (loop for i from 1 to n do 
	       (loop for j from 1 to n do 
		 (setf (nth j (nth i mc)) (if (= p q) ($identfor (nth j (nth i mat)) fld-name j i)
					    ($identfor (nth j (nth i mat)) fld-name p q)))))
	     mc))
	  (t (if (= p q) mul-id add-id)))))



    