;; Definitions to avoid undefined variable warnings
(defvar allbutl nil)
(defvar dcount 0)
(defvar greatorder nil)
(defvar lessorder nil)

;; Redefining toplevel-macsyma-eval for post_eval_functions 
;; Define the finaleval list
(defmvar $pre_eval_functions `((mlist))
  nil
  :setting-predicate #'(lambda (val)
			 (and ($listp val)
			      (every 'symbolp (margs val)))))

(defmvar $post_eval_functions `((mlist))
  nil
  :setting-predicate #'(lambda (val)
			 (and ($listp val)
			      (every 'symbolp (margs val)))))

(defun toplevel-macsyma-eval (x)
;; Functional definition of toplevel-macsyma-eval
  (dolist (fi (margs $pre_eval_functions) x)
    (setq x (mfuncall fi x)))
  (setq x (meval* x))
  (dolist (fi (margs $post_eval_functions) x)
    (setq x (mfuncall fi x))))


(defmfun kill1 (x)
;; Redefine kill1 in order to be able to properly reset post_eval_functions
;; with kill(all) and kill(post_eval_functions)
  (funcall 
   #'(lambda (z)
       (cond ((and allbutl (member x allbutl :test #'eq)))
	     ((eq (setq x (getopr x)) '$labels)
	      (dolist (u (cdr $labels))
		(cond ((and allbutl (member u allbutl :test #'eq))
		       (setq z (nconc z (ncons u))))
		      (t (makunbound u) (remprop u 'time)
			 (remprop u 'nodisp))))
	      (setq $labels (cons '(mlist simp) z) $linenum 0 dcount 0))
	     ((member x '($values $arrays $aliases $rules $props
			$let_rule_packages) :test #'eq)
	      (mapc #'kill1 (cdr (symbol-value x))))
	     ((member x '($functions $macros $gradefs $dependencies $structures) :test #'eq)
	      (mapc #'(lambda (y) (kill1 (caar y))) (cdr (symbol-value x))))
	     ((eq x '$myoptions))
	     ((eq x '$tellrats) (setq tellratlist nil))
	     ((eq x '$ratweights) (setq *ratweights nil
					$ratweights '((mlist simp))))
	     ((eq x '$features)
	      (cond ((not (equal (cdr $features) featurel))
		     (setq $features (cons '(mlist simp) 
					   (copy-list featurel ))))))
	     ((eq x '$pre_eval_functions) (setq $pre_eval_functions '((mlist)) ))
	     ((eq x '$post_eval_functions) (setq $post_eval_functions '((mlist)) ))
	     ((or (eq x t) (eq x '$all))
	      (setq $pre_eval_functions '((mlist)))
	      (setq $post_eval_functions '((mlist)))
	      (mapc #'kill1 (cdr $infolists))
	      (setq $ratvars '((mlist simp)) varlist nil genvar nil
		    checkfactors nil greatorder nil lessorder nil $gensumnum 0
		    *ratweights nil $ratweights 
		    '((mlist simp))
		    tellratlist nil $dontfactor '((mlist)) $setcheck nil)
	      (killallcontexts))
	     ((setq z (assoc x '(($inlabels . $inchar) ($outlabels . $outchar)
				($linelabels . $linechar)) :test #'eq))
	      (mapc #'(lambda (y) (remvalue y '$kill))
		    (getlabels* (eval (cdr z)) nil)))
	     ((and (fixnump x) (not (< x 0))) (remlabels x))
	     ((atom x) (kill1-atom x))
	     ((and (eq (caar x) 'mlist) (fixnump (cadr x))
		   (or (and (null (cddr x)) 
			    (setq x (append x (ncons (cadr x)))))
		       (and (fixnump (caddr x)) (not (> (cadr x) (caddr x))))))
	      (let (($linenum (caddr x))) (remlabels (f- (caddr x) (cadr x)))))
	     ((setq z (mgetl (caar x) '(hashar array))) (remarrelem z x))
	     ((and ($subvarp x)
		   (boundp (caar x))
		   (hash-table-p (setq z (symbol-value (caar x)))))
	      ; Evaluate the subscripts (as is done in ARRSTORE)
	      (let ((indices (mevalargs (cdr x))))
		(if (gethash 'dim1 z)
		  (remhash (car indices) z)
		  (remhash indices z))))
	     ((eq (caar x) '$@) (mrecord-kill x))
	     ((and (eq (caar x) '$allbut)
		   (not (dolist (u (cdr x)) 
			  (if (not (symbolp u)) (return t)))))
	      (let ((allbutl (cdr x))) (kill1 t)))
	     (t (improper-arg-err x '$kill))))
   nil))

;; Define a couple variables specific to this code
(defmvar $allunitslist `((mlist)))
(defmvar $unitformatresults t)

;; Ensure that units formatting isn't invoked by letsimp.
;; Unless unitformatresults is bound to NIL, letsimp runs into
;; an infinite recursion via NFORMAT.

(let ((builtin-letsimp (get '$letsimp 'mfexpr*)))
  (defmspec $letsimp (e)
    (let (($unitformatresults nil))
      (funcall builtin-letsimp e))))

;; Code to enable correct display of multiplication by units via nformat
(defun notunitfree (form)
  ;;returns t if expression contains units, nil otherwise
  (cond ((atom form) ($member form $allunitslist))
        ((null (car form)) nil)
  	((atom (car form))(or ($member (car form) $allunitslist)
			      (notunitfree (cdr form))))
	(t (or (notunitfree (cdr (car form)))
	       (notunitfree (cdr form))))))


;; Code to optionally group units by common unit

(defun unitmember (form list1)
   (cond ((equal (car list1) nil) nil)
         ((equal form (car list1)) t) 
	 (t (unitmember form (cdr list1)))))
   	 
(defun groupbyaddlisp (form)
    (cond ((or (not(notunitfree (car form))) (not(notunitfree (cadr form))))
            form)
    	  ((and (null (cddr form))(equal (meval (list '(mplus simp) (caddr (car form)) (list '(mtimes simp) -1 (caddr (cadr form))))) 0))
              (list (list '(mtimes) (meval (list '(mplus simp) (cadr (car form)) (cadr (cadr form)))) (caddr (car form)))))
	  ((null (cddr form)) form)
          ((equal (meval (list '(mplus simp) (caddr (car form)) (list '(mtimes simp) -1 (caddr (cadr form))))) 0)
	       (groupbyaddlisp (cons (list '(mtimes) (meval (list '(mplus simp) (cadr (car form)) (cadr (cadr form)))) (caddr (car form)))
	              (cddr form))))
	  (t (cons (list '(mtimes) (cadr (car form)) (caddr (car form))) (groupbyaddlisp (cdr form))))))	     
	   
(defun groupadd (form) 
   (cond ((and (not (atom form)) (notunitfree form) (equal (caar form) 'mplus))
           (let ((temp1 (groupbyaddlisp (cdr (nformat form)))))
           (cond ((or (not (equal (cdr temp1) nil)) (and (atom (cadr temp1)) (not (equal (cadr temp1) nil))))
                  (cons '(mplus) temp1))
		 ((equal (cdr temp1) nil) (car temp1))))) 
         (t form)))
    
(defun onlyunits (form)
  ;;returns t if expression contains only units, nil otherwise
  (cond ((null (car form)) t)
        ((atom (car form))(and ($member (car form) $allunitslist)
			      (onlyunits (cdr form))))
	(t (and (onlyunits (list (car (cdr (car form)))))
	       (onlyunits (cdr form))))))

(defun getunits (form)
  ;;returns a list containing all unit terms
  (cond ((null (cadr form)) '())
	((atom (cadr form)) 
	 (if ($member (cadr form) $allunitslist) 
	     (cons (cadr form) 
		   (getunits (cons (car form) (cdr (cdr form)))))
	     (getunits (cons (car form) (cdr (cdr form))))))
	(t
	 (if ($member (cadr (cadr form)) $allunitslist) 
	     (cons (cadr form) 
		   (getunits (cons (car form) (cdr (cdr form)))))
	     (getunits (cons (car form) (cdr (cdr form))))))))

(defun nonunits (form)
  ;;returns a list containing all non-unit terms
  (cond ((null (cadr form)) '())
	((atom (cadr form)) 
	 (if (not($member (cadr form) $allunitslist)) 
	     (cons (cadr form) 
		   (nonunits (cons (car form) (cdr (cdr form)))))
	     (nonunits (cons (car form) (cdr (cdr form))))))
	(t
	 (if (not($member (cadr (cadr form)) $allunitslist)) 
	     (cons (cadr form) 
		   (nonunits (cons (car form) (cdr (cdr form)))))
	     (nonunits (cons (car form) (cdr (cdr form))))))))

(defun unitmtimeswrapper (form)
   (cond ((and (notunitfree form) (not(onlyunits (cdr form))) 
               (not (equal '(-1) (nonunits form))) (equal $unitformatresults t))
	 (list '(mtimes) 
	       (cons '(mtimes simp) (nonunits form))
	       (cons '(mtimes simp) (getunits form))))
         ((onlyunits (cdr form)) (form-mtimes form))
         (t (form-mtimes form))))


(defmfun nformat (form)
  (cond ((atom form)
	 (cond ((and (numberp form) (minusp form)) (list '(mminus) (- form)))
	       ((eq t form) (if in-p t '$true))
	       ((eq nil form) (if in-p nil '$false))
	       (t form)))
	((atom (car form))
	 form)
	((eq 'rat (caar form))
	 (cond ((minusp (cadr form))
		(list '(mminus) (list '(rat) (- (cadr form)) (caddr form))))
	       (t (cons '(rat) (cdr form)))))
	((eq 'mmacroexpanded (caar form)) (nformat (caddr form)))
	((null (cdar form)) form)
	((eq 'mplus (caar form)) (form-mplus form))
	((eq 'mtimes (caar form)) (unitmtimeswrapper form))
	((eq 'mexpt (caar form)) (form-mexpt form))
	((eq 'mrat (caar form)) (form-mrat form))
	((eq 'mpois (caar form)) (nformat ($outofpois form)))
	((eq 'bigfloat (caar form))
	 (if (minusp (cadr form))
	     (list '(mminus) (list (car form) (- (cadr form)) (caddr form)))
	     (cons (car form) (cdr form))))
	(t form)))
