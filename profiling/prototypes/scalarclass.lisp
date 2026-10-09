;; Round 2. SCALARCLASS takes an optional CONSTANT, true when ($CONSTANTP EXP)
;; is already known. Then its arguments are known constant too, and
;; CONSTTERMP need not walk them with $CONSTANTP again at every level.
(in-package :maxima)

(defun scalarclass (exp &optional constant) ;  Returns $SCALAR, $NONSCALAR, or NIL (unknown).
  (cond ((mnump exp)
         ;; Maxima numbers are scalar.
         '$scalar)
        ((atom exp)
	 (cond ((or (mget exp '$nonscalar)
	            (and (not (mget exp '$scalar))
	                 ;; Arrays are nonscalar, but not if declared scalar.
	                 (or (arrayp exp)
	                     ($member exp $arrays))))
	        '$nonscalar)
	       ((or (mget exp '$scalar)
	            ;; Include constant atoms which are not declared nonscalar.
	            constant
	            ($constantp exp))
	        '$scalar)))
        ((member 'array (car exp))
         (cond ((mget (caar exp) '$scalar) '$scalar)
               ((mget (caar exp) '$nonscalar) '$nonscalar)
               (t nil)))
	((specrepp exp) (scalarclass (specdisrep exp) constant))
	((scalarclass (caar exp)))
	((member (caar exp) '(mplus mtimes))
	 (do ((l (cdr exp) (cdr l))) ((null l) '$scalar)
	   (if (not (consttermp (car l) constant))
	       (return (scalarclass-list l constant)))))
	((and (eq (caar exp) 'mqapply) (scalarclass (cadr exp))))
	((mxorlistp exp) '$nonscalar)
	(t
	 (do ((exp (cdr exp) (cdr exp)) (l '(1)))
	      ((null exp) (scalarclass-list l constant))
	    (if (not (consttermp (car exp) constant))
	        (setq l (cons (car exp) l)))))))

(defun scalarclass-list (llist &optional constant)
  (cond ((null llist) nil)
	((null (cdr llist)) (scalarclass (car llist) constant))
	(t (let ((sc-car (scalarclass (car llist) constant))
		 (sc-cdr (scalarclass-list (cdr llist) constant)))
	     (cond ((or (eq sc-car '$nonscalar)
			(eq sc-cdr '$nonscalar))
		    '$nonscalar)
		   ((and (eq sc-car '$scalar) (eq sc-cdr '$scalar))
		    '$scalar))))))

(defun consttermp (x &optional constant)
  (and (or constant ($constantp x))
       (not (eq (scalarclass x t) '$nonscalar))))

(defun simpnct-constantp (term)
  (and $dotconstrules
       (or (mnump term)
	   (consttermp term))))
