;; These binding powers make a .. b + 1 == a .. (b+1).

($infix ".." 80 80)

(setf (get '$.. 'operators) 'simp-integer-sequence)

#| 

For integers a,b, and n define a .. b = [a,a+1, ..., a+n]. 

a .. b expands to a list of integers when either (1) ceiling(a) and
floor(b) are integers or (2) both a and b are declared integers and 
b - a is an integer. Also a .. b expands to the empty list when csign
determines that a > b and that a and b are real-valued. Thus a + 1
.. a --> [], but %i + 1 .. %i --> noun form.
|#

(defun simp-integer-sequence (e yy z)
  (declare (ignore yy))
  (twoargcheck e)
  (let ((i) (ii) (j) (jj) (acc nil))
    (setq i (simplifya (second e) z))
    (setq j (simplifya (third e) z))
    (cond ((and (integerp i) (integerp j))
	   (while (>= j i)
	     (push j acc)
	     (decf j))
	   (simplify (cons '(mlist) acc)))
	  
	  ((and ($featurep i '$integer) ($featurep j '$integer) (integerp (sub j i)))
	   (simplify (cons '(mlist) (mapcar #'(lambda (s) (add i s)) (margs (take '($..) 0 (sub j i)))))))

	  ((and (eq '$neg (csign (sub j i))) (not (eq t (csign i))) (not (eq t (csign j))))
	   (simplify `((mlist))))

	  (t
	   (setq ii (take '($ceiling) i))
	   (setq jj (take '($floor) j))
	   (if (and (integerp ii) (integerp jj)) (take '($..) ii jj) `(($.. simp) ,i ,j))))))
	  
	  
    
    