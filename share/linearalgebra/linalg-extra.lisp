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
  (cond (($listp vars)
	 (let ((z) (mat nil))
	   (setq vars (margs vars))
	   (dolist (vi vars)
	     (setq z ($diff e vi))
	     (push (cons '(mlist) (mapcar #'(lambda (s) ($diff z s)) vars)) mat))
	   (cons '($matrix) (reverse mat))))
	(t  `(($hessian) ,e ,vars))))
	    
(defun $jacobian (e vars)
  (cond ((and ($listp vars) ($listp e))
	 (setq e (margs e))
	 (setq vars (margs vars))
	 (let ((mat nil))
	   (dolist (ei e)
	     (push (cons '(mlist) (mapcar #'(lambda (s) ($diff ei s)) vars)) mat))
	   (cons '($matrix) (reverse mat))))
	(t `(($jacobian) ,e ,vars))))

(defun $vandermonde_matrix (l)
  (let ((x) (row) (acc))
    (setq l (require-list l "$vandermonde_matrix"))
    (dolist (li l)
      (setq x 1 row nil)
      (dolist (lk l)
        (declare (ignore lk))
	(push x row)
	(setq x (mul x li)))
      (setq row (nreverse row))
      (push '(mlist) row)
      (push row acc))
    (setq acc (nreverse acc))
    (push '($matrix) acc)
    acc))

;; Use Sylvester's criterion to decide if the self-adjoint part of a matrix is 
;; negative definite (neg) or positive definite (pos). By the self-adjoint part 
;; of a matrix M, I mean (M + M^*) / 2, where ^* is the conjugate transpose. For
;; all other cases, return pnz. This algorithm is unable to determine if a matrix
;; is negative semidefinite or positive semidefinite.

;; For purely numerical matrices, there more efficient algorithms; for symbolic
;; matrices, things like matrix([a,b],[b,a]), assume(a > 0, a^2 > b^2), I think
;; this is the best we can do.

;; (1) The divide by 2 isn't needed, but try assume(a > 0, a^2 > b^2),
;; matrix_sign(matrix([a,b],[b,a])) without the divide by 2.

(defun $matrix_sign (m)
  (let ((n) (det) (p-sgn nil) (n-sgn nil))
    ($require_square_matrix m '$first '$matrix_sign)
    (setq m (div (add m ($ctranspose m)) 2)) ;; see (1)
    (setq n ($first ($matrix_size m)))
    
    (loop for i from 1 to n do
      (setq det (ratdisrep (newdet m i nil)))
      (push (csign det) p-sgn)
      (push (csign (mul (power -1 i) det)) n-sgn))
    
    (cond ((every #'(lambda (s) (eq s '$pos)) p-sgn) '$pos)
	  ((every #'(lambda (s) (eq s '$pos)) n-sgn) '$neg)
	  (t '$pnz))))



 
