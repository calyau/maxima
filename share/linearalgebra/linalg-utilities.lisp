;;  Copyright 2005 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$linalgutilities 2 '$version)

(defun $listp (e &optional (f nil))
  (and (op-equalp e 'mlist) (or (eq f nil) (every #'(lambda (s) (eq t (mfuncall f s))) (margs e)))))

(defun $matrixp (e  &optional (f nil))
  (and (op-equalp e '$matrix) (every #'(lambda (s) ($listp s f)) (margs e))))

(defun $require_nonempty_matrix (m pos fun)
  (if (not (and ($matrixp m) (> ($length m) 0) (> ($length ($first m)) 0)))
      (merror "The ~:M argument of the function ~:M must be a nonempty matrix" pos fun)))

(defun $blockmatrixp (m) ($matrixp m '$matrixp))

(defun $require_matrix (m pos fun)
  (if (not ($matrixp m))
      (merror "The ~:M argument of the function ~:M must be a matrix" pos fun)))

(defun $require_unblockedmatrix (m pos fun)
  (if (or (not ($matrixp m)) ($blockmatrixp m))
      (merror "The ~:M argument of the function ~:M must be an unblocked matrix" pos fun)))

(defun $require_square_matrix (m pos fun)
  (if (not (and ($matrixp m) (= ($length m) ($length ($first m)))))
      (merror "The ~:M argument of the function ~:M must be a square matrix" pos fun)))

(defun array-elem (m i j)
  (nth j (nth i m)))

(defun $require_symmetric_matrix (m fun pos)
  (if (not ($matrixp m))
      (merror "The ~:M argument to ~:M must be a matrix" pos fun))

  (let ((n ($matrix_size m)))
    (if (not (= ($first n) ($second n)))
	(merror "The ~:M argument to ~:M must be a square matrix" pos fun))
    (setq n ($first n))
    (loop for i from 1 to n do
      (loop for j from (+ i 1) to n do
	(if (not (like (array-elem m i j) (array-elem m j i)))
	    (merror "The ~:M argument to ~:M must be a symmetric matrix" pos fun)))))
  '$done)

(defun $matrix_size(m)
  ($require_matrix m "$first" "$matrix_size")
  `((mlist) ,($length m) ,($length ($first m))))
  
(defun $require_list (lst pos fun)
  (if (not ($listp lst))
      (merror "The ~:M argument of the function ~:M must be a list" pos fun)))

(defun $require_posinteger(i pos fun)
  (if (not (and (integerp i) (> i 0)))
      (merror "The ~:M argument of the function ~:M must be a positive integer" pos fun)))

;; Map the lisp function fn over the matrix m. This function is block matrix friendly.

(defun full-matrix-map (m fn)
  (if (or ($listp m) ($matrixp m))
      (cons (car m) (mapcar #'(lambda (s) (full-matrix-map s fn)) (cdr m)))
    (setf m (funcall fn m))))

(defun $ctranspose (m)
  (mfuncall '$transpose (full-matrix-map m #'(lambda (s) (simplifya `(($conjugate) ,s) nil)))))
 
(defun $zeromatrixp (m)
  (if (or ($matrixp m) ($listp m)) (every '$zeromatrixp (cdr m))
    (eq t (meqp 0 ($rectform m)))))
	
(eval-when (eval compile load)
  (mfuncall '$alias '$copylist '$copy '$copymatrix '$copy))

(defun $copy (e) (copy-tree e))

