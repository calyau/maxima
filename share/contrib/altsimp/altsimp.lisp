;; Authors: Barton Willis with help from Richard Fateman

;; To simplify a sum with n terms, the standard simplus function calls great O(n^2) times. By 
;; using sorting more effectively, this code reduced the calls to great to O(n log_2(n)). Also, 
;; this code tries to be infinity correct: thus inf + inf --> inf,inf + 1 --> inf, and inf + minf --> und. 
;; Since -1 * inf doesn't simplify to minf, this code doesn't simplify inf - inf to und. Maybe the 
;; special case dispatch part of this code makes the task of extending simplus (to intervals, for example)
;; easier. The basic design of this code is due to Barton Willis.


#| Fixed bugs:

(1) ceiling(asin(-107) -42) <--- bug! Gets stuck. I think -1.0 * (complex) should 
    expand, but it doesn't. I fixed this by changing the condition for a "do-over" from 
   (and (or (equalp cf 1) (equalp cf -1)) (mplusp x) ...)  to (and (or (eq cf 1) (eq cf -1)) (mplusp x) ...) 

(2) rat(x) + taylor(x^42,x,0,1) --> error. Fixed by adding taylor terms separately from mrat terms.

Unfixed bugs:

(1) rtest15, #222 sqrt(2)/6-(2*sqrt(2))/6 --> simplify correctly.

(2) compare('inf,'inf) --> "<" (related to rtest_allnummod #133 bug).

(3) 0 + 0.0 --> 0; this may be the cause of bug in rtest_trig, #29. Fixing this is
    easy: change (if (not (mzerop num-sum)) (push num-sum acc)) to
    (if (not (eq 0 num-sum)) (push num-sum acc)). But this fix causes problems with
    the testsuite (I think it does an asksign and it's either very slow or gets
    stuck.) Compare tlimit((x*atan(x))/(1+x),x,inf) with the standard simplus and
    with a simplus that does 0.0 + x --> 0.0 + x.

|#

(in-package :maxima)
(declaim (optimize (speed 3)(safety 0)))

(define-modify-macro mincf (&optional (i 1)) addk)

(defmacro opcons (op &rest args)
  `(simplify (list (list ,op) ,@args)))

(defmacro opapply (op args)
  `(simplify (cons (list ,op) ,args)))

(defun mzerop (z)
  (and (mnump z)
       (or (and (numberp z)(= z 0))
	   (and (bigfloatp z)(= (cadr z) 0))))) ;bigfloat zeros may be diff precisions

(defun convert-to-coeff-form (x)  
  (let ((c))
    (cond ((mnump x) (cons 1 x))
	  ((mtimesp x) 
	   (pop x)  ;remove (car x) which is (mtimes ..)
	   (cond ((mnump (setf c (car x))) ;set c to numeric coeff.
		  (pop x) ; remove numeric coeff.
		  (if (null (cdr x));; if only one more item, that's it.
		      (cons  (car x) c)
		    (cons  `((mtimes simp) ,@x) c)))
		 (t (cons  `((mtimes simp) ,@x) 1))))
	  (t (cons x 1)))))

;; The expression e must be simplified (ok)

(defun number-times-expr (cf e)
  (cond ((eq cf 1) e)
	((mzerop cf) 0) ;; wrong for 0.0 * e and 0.0b0 * e.
	((mtimesp e) 
	 (if (mnump (cadr e)) (mult cf e) `((mtimes simp) ,cf ,@(cdr e))))
	(t (mul cf e))))

;; Add an expression x to a list of equalities l.

(defun add-expr-mequal (x l)
  (setq l (mapcar 'cdr l))
  (push (list x x) l)
  (setq l (list (reduce 'add (mapcar 'first l)) (reduce 'add (mapcar 'second l))))
  (simplifya (cons '(mequal) l) t))
  
(defun add-expr-mrat (x l)
  (ratf (cons '(mplus) (cons (ratf x) l))))

(defun add-expr-taylor (x l)
  (taylor1 (cons '(mplus) (cons x l)) nil))

(defun add-expr-mlist (x l)
  (setq l (if (cdr l) (reduce 'addmx l) (car l)))
  (opapply 'mlist (mapcar #'(lambda (s) (add x s)) (cdr l))))

;; Add an expression x to a list of matrices l.

(defun add-expr-matrix (x l)
  (mxplusc x (if (cdr l) (reduce 'addmx l) (car l))))

;; Return a + b, where a, b in {minf, inf, ind, und, infinity}. I should
;; extend this to allow zeroa and zerob (but I'm not sure zeroa and zerob
;; are supposed to be allowed outside the limit code).

(defun add-extended-real (a b)
  (cond ((eq a '$minf) 
	 (cond ((memq b '($minf $ind)) '$minf)
	       ((memq b '($und $inf)) '$und)
	       ((eq b '$infinity) '$infinity)))
	((eq a '$ind)
	 (cond ((eq b '$minf) '$minf)
	       ((eq b '$ind) '$ind)
	       ((eq b '$und) '$und)
	       ((eq b '$inf) '$inf)
	       ((eq b '$infinity) '$infinity)))
	((eq a '$und) '$und)
	((eq a '$inf)
	 (cond ((memq b '($minf $und)) '$und)
	       ((memq b '($inf $ind)) '$inf)
	       ((eq b '$infinity) '$infinity)))
	((eq a '$infinity) (if (eq b '$und) '$und '$infinity))))

;; Add an expression x to a list of infinities.

(defun add-expr-infinities (x l) 
  (setq l (if l (reduce 'add-extended-real l) (car l)))
  (if (mnump x) l `((mplus simp) ,x ,l)))
	
;; I assumed that if a list of distinct members is sorted using great,
;; then it's still sorted after multiplying each list member by a nonzero
;; maxima number. I'm not sure this is true.

;; If l has n summands, simplus calls great O(n log_2(n)) times. All
;; other spendy functions are called O(n) times. The standard simplus
;; function calls great O(n^2) times, I think.

;(defvar *calls-to-simplus* 0)
;(defvar *simplus-length* 0)
;(defvar *its-an-atom* 0)
;(defvar *not-an-atom* 0)


(defun simplus (l w z)
  (declare (ignore w))
  ;;(incf *calls-to-simplus*)
  ;;(if (> 8 (length l)) (incf *simplus-length*))
  (let ((acc nil) (cf) (x) (num-sum 0) (do-over nil) (mequal-terms nil) (mrat-terms nil) 
	(inf-terms nil) (matrix-terms nil) (mlist-terms nil) (taylor-terms nil) (op)
	(atom-hash (make-hash-table :test #'eq :size 8)))

    (setq l (margs l))

    ;; simplfy and flatten
    (dolist (li l)
      (setq li (simplifya li z))
      (if (mplusp li) (setq acc (append acc (cdr li))) (push li acc)))
    (setq l acc)
    (setq acc nil)
    (dolist (li l)
      ;;(if (atom li) (incf *its-an-atom*) (incf *not-an-atom*))
      (cond ((mnump li) (mincf num-sum li))
	    
	    ;; factor out infrequent cases.
	    ((and (consp li) (consp (car li)) (memq (caar li) '(mequal mrat $matrix mlist)))
	     (setq op (caar li))
	     (cond ((eq op 'mequal)
		    (push li mequal-terms))
		   (($taylorp li)
		    (push li taylor-terms))
		   ((eq op 'mrat)
		    (push li mrat-terms))
		   ((eq op '$matrix)
		    (push li matrix-terms))
		   ((eq op 'mlist)
		    (if $listarith (push li mlist-terms) (push (convert-to-coeff-form li) acc)))))

	    ;; Put non-infinite atoms into a hashtable; push infinite atoms into inf-terms.
	    ((atom li)
	     (if (memq li '($minf $inf $infinity $und $ind))
		 (push li inf-terms)
	       (progn
		 (setq cf (gethash li atom-hash))
		 (setf (gethash li atom-hash) (if cf (1+ cf) 1)))))

	    (t (push (convert-to-coeff-form li) acc))))

     ;; push atoms in the hashtable into the accumulator acc; sort acc.
    (maphash #'(lambda (cf a) (push (cons cf a) acc)) atom-hash)
    (setq l (sort acc 'great :key 'car))
 
    ;; common term crunch: when the new coefficient is -1 or 1 (for example, 5*a - 4*a),
    ;; set the "do-over" flag to true. In this case, the sum needs to be re-simplified.
    ;; Without the do over flag, a + 5*a - 4*a --> a + a. Last I checked, the testsuite
    ;; does not test the do-over scheme.

    (setq acc nil)
    (while l
      (setq x (pop l))
      (setq cf (cdr x))
      (setq x (car x))
      (while (and l (like x (caar l)))
	(mincf cf (cdr (pop l))))
      (if (and (or (eq cf 1) (eq cf -1)) (mplusp x)) (setq do-over t))
      (setq x (number-times-expr cf x))
      (cond ((mnump x) (mincf num-sum x))
	    ((not (mzerop x)) (push x acc))))

    ;; (setq acc (sort acc '$orderlessp))   ;;<-- not sure this is needed.

    ;; I think we want x + 0.0 --> x + 0.0, not x + 0.0 --> x.
    ;; If float and bfloat were simplifying functions we could do 
    ;; x + 0.0 --> float(x) and 0.0b0 + x --> bfloat(x). Changing this
    ;; test from mzerop to (eq 0 num-sum) causes problems with the test suite.
    ;; For example, if x + 0.0 --> x + 0.0, we get an asksign for 
    ;; tlimit((x*atan(x))/(1+x),x,inf). That's due to the (bogus) floating point
    ;; calculations done by the limit code.
  
    ;;(if (not (eq 0 num-sum)) (push num-sum acc))
    (if (not (mzerop num-sum)) (push num-sum acc))
   
    ;;(if do-over (incf *do-over*)) ;; never happens for testsuite!
    (setq acc
	  (cond (do-over (simplifya `((mplus) ,@acc) nil))
		((null acc) 0)
		((null (cdr acc)) (car acc))
		(t (cons '(mplus simp) acc))))
    
    ;; special case dispatch
    (if mequal-terms
	(setq acc (add-expr-mequal acc mequal-terms)))
    (if taylor-terms
	(setq acc (add-expr-taylor acc taylor-terms)))
    (if mrat-terms
	(setq acc (add-expr-mrat acc mrat-terms)))
    (if mlist-terms
	(setq acc (add-expr-mlist acc mlist-terms)))
    (if matrix-terms
	(setq acc (add-expr-matrix acc matrix-terms)))
    (if inf-terms
	(setq acc (add-expr-infinities acc inf-terms)))   
 
    acc))

