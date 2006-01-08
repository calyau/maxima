;;  Copyright 2005 by Barton Willis

;;  This is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;; This software has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

($put '$cholesky 1 '$version)

(defun fn-melem (mat perm i j fn)
  (funcall fn (m-elem mat perm i j)))

(defun $require_selfadjoint_matrix (m fun pos)
  (if (or (not ($matrixp m)) 
	  (not (= ($first ($matrix_size m)) ($second ($matrix_size m))))
	  (not (mfuncall '$zeromatrixp (sub m ($ctranspose m)))))
      (merror "The ~:M argument to the function ~:M must be a selfadjoint matrix" pos fun)))

(defun $cholesky (m &optional (fld-name '$generalring))
  ($require_selfadjoint_matrix m "$first" "$cholesky")
  ($require_nonempty_matrix m "$first" "$cholesky")
 
  (let* ((n ($first ($matrix_size m))) (perm) (lii) (lii-inv) (l) (acc) (x)
	 (fld ($require_ring fld-name "$second" "$cholesky"))
	 (fsub (mring-sub fld))
	 (fzerop (mring-fzerop fld))
	 (fmult (mring-mult fld))
	 (fadjoint (mring-adjoint fld))
	 (fpsqrt (mring-psqrt fld))
	 (fgreat (mring-great fld))
	 (fconvert (mring-maxima-to-mring fld))
	 (freciprocal (mring-reciprocal fld)))

    (setq l ($zerofor m))
    (loop for k from 1 to n do 
      (push k perm))

    (setq perm (reverse perm))
    (push '(mlist) perm)
    (loop for i from 1 to n do 
      (setq acc (fn-melem m perm i i fconvert))
      (loop for k from 1 to (- i 1) do 
	(setq acc (funcall fsub acc (funcall fmult 
					     (m-elem l perm i k)
					     (funcall fadjoint (m-elem l perm i k))))))
      
      (setq lii (if ($matrixp acc) ($cholesky acc fld-name) (funcall fpsqrt acc)))
      (if (null lii) (merror "Unable to find the Cholesky factorization"))
      (setmatelem l lii i i)
      (if (funcall fzerop lii) (merror "Unable to find the Cholesky factorization"))
      (setq lii-inv (funcall fadjoint (funcall freciprocal lii)))
      
      (loop for j from (+ i 1) to n do
	(setq acc (fn-melem m perm j i fconvert))
	(loop for k from 1 to (- i 1) do
	  (setq acc (funcall fsub acc (funcall fmult 
					       (m-elem l perm j k) 
					       (funcall fadjoint (m-elem l perm i k))))))
	(setmatelem l (funcall fmult acc lii-inv) j i)))
    (matrix-map l n n (mring-mring-to-maxima  fld))
    l))
	
	
