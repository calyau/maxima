;; Copyright 2006 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$linalgextra 1 '$version)

(defun $circulant (lst)
  ($require_list lst "$first" "$circulant")
  (let ((q) (n ($length lst)))
    (setq lst (rest lst))
    (setq q (list lst))
    (decf n)
    (dotimes (i n)
      (setq lst `(,@(rest lst) ,(car lst)))
      (push lst q))
    (setq q (mapcar #'(lambda(s) (cons '(mlist) s)) q))
    (push '($matrix) q)))

(defun $cauchy_matrix (p &optional q)
  ($require_list p "$first" "$cauchy_matrix")
  (if q ($require_list q "$second" "$cauchy_matrix") (setq q p))
  (let ((row) (mat))
    (setq p (margs p))
    (setq q (margs q))
    (dolist (pj p)
      (setq row nil)
      (dolist (qj q)
	(push (div 1 (add pj qj)) row))
      (setq row (nreverse row))
      (push '(mlist) row)
      (push row mat))
    (setq mat (nreverse mat))
    (push '($matrix) mat)))

(defun $hessian (e vars)
  (if ($listp vars)
      (let ((n ($length vars)))
	($genmatrix `((lambda) ((mlist) i j) ($diff ,e (nth i ,vars) 1 (nth j ,vars) 1)) n n))
    `(($hessian) ,e ,vars)))

(defun $jacobian (e vars)
  (if (and ($listp vars) ($listp e))
      (let ((m ($length e)) (n ($length vars)))
	($genmatrix `((lambda) ((mlist) i j) ($diff (nth i ,e) (nth j ,vars))) m n))
    `(($jacobian) ,e ,vars)))

;; Use Sylvester's criterion to decide if the self-adjoint part of a matrix is 
;; negative definite (neg), negative semidefinite (nz), positive semidefinite (pz), 
;; or positive definite (pos). By the self-adjoint part of a matrix M, I mean
;; (M + M^*) / 2, where ^* is the conjugate transpose. 

;; For purely numerical matrices, there more efficient algorithms; for symbolic
;; matrices, things like matrix([a,b],[b,a]), assume(a > 0, a^2 > b^2), I think
;; this is the best we can do.

;; (1) The divide by 2 isn't needed, but try assume(a > 0, a^2 > b^2),
;; matrix_sign(matrix([a,b],[b,a])) without the divide by 2.

(defun $matrix_sign (m)
  (let ((i 1) (sgn) (n) (matrix-sign))
    ($require_square_matrix m '$first '$matrix_sign)
    (setq m (div (add m ($ctranspose m)) 2)) ;; see (1)
    (setq n ($first ($matrix_size m)))
    (setq matrix-sign (csign (ratdisrep (newdet m i nil))))
    
    (while (and (memq matrix-sign '($neg $nz $pz $pos)) (< i n))
      (incf i)
      (setq sgn (csign (ratdisrep (newdet m i nil))))      
      (cond
       ((and (eq matrix-sign '$neg) (memq sgn '($nz $neg)))
	(setq matrix-sign sgn))
       
       ((and (eq matrix-sign '$neg) (eq sgn '$zero))
	(setq matrix-sign '$nz))
       
       ((eq matrix-sign '$neg)
	(setq matrix-sign '$pnz))
       
       ((and (eq matrix-sign '$nz) (memq sgn '($neg $nz $zero)))
	(setq matrix-sign '$nz))
	
       ((eq matrix-sign '$nz) 
	(setq matrix-sign '$pnz))

       ((and (eq matrix-sign '$pz) (memq sgn '($pz $pos $zero)))
	(setq matrix-sign '$pz))

       ((eq matrix-sign '$pz) 
	(setq matrix-sign '$pnz))

       ((and (eq matrix-sign '$pos) (memq sgn '($pz $pos)))
	(setq matrix-sign sgn))

       ((and (eq matrix-sign '$pos) (eq sgn '$zero))
	(setq matrix-sign '$pz))
       
       ((eq matrix-sign '$pos)
	(setq matrix-sign '$pnz))

       (t (setq matrix-sign '$pnz))))
    
    (if (eq matrix-sign '$zero) '$pnz matrix-sign)))

  
