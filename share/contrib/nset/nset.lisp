;; A new Maxima set package

;; Authors: Barton Willis, University of Nebraska at Kearney (aka UNK), and
;;          Stavros Macrakis

;; November 2002 

;; License: GPL
;; The user of this code assumes all risk for its use. It has no warranty.
;; If you don't know the meaning of "no warranty," don't use this code. :)

(in-package "MAXIMA")
($put '$nset 1 '$version)

(defmacro while (cond &rest body)
  `(do ()
       ((not ,cond))
     ,@body))

;; Display sets as { .. }. The display code only effects the way sets 
;; are displayed; a user cannot define a set by 
;;  (c1) {a,b}   <== bogus.
;; The display code doesn't work in commercial Macsyma.

(put '$set 'msize-matchfix 'grind)
(put '$set '((#\{ ) #\} ) 'dissym)
(put '$set 'dimension-match 'dimension)

;; Support for TeXing sets. If your mactex doesn't TeX the empty set
;; correctly, get the latest mactex.lisp.

(defprop $set tex-matchfix tex)
(defprop $set (("\\left \\{" ) " \\right \\}") texsym)

(defun require-set (x context-string)
  (cond (($setp x) 
	 (cdr (simp-set x 1 nil)))
	(($listp x) 
	 (cdr (simp-set `(($set) ,@(cdr x)) 1 nil)))
	(t 
	 (merror "Function ~:M expects a list set or a set, instead found ~:M" context-string x))))

;; When a is a list, setify(a) is equivalent to apply(set, a).
;; When a isn't a list, signal an error. 

(defun $setify (a)
  (cond (($listp a)
	 `(($set) ,@(cdr a)))
	(t
	 (merror "Function setify requires a list, instead found ~:M" a))))

;; When a is a list, convert a and all of its elements that are lists
;; into sets.  When a isn't a list, return a.

(defun $fullsetify (a)
  (cond (($listp a) 
	 `(($set) ,@(mapcar '$fullsetify (cdr a))))
	(t a)))

;; If a is a set, convert the top-level set to a list; when a isn't a
;; list, return a.

(defun $listify (a)
  (cond (($setp a) `((mlist simp) ,@(cdr a)))
	(t a)))

;; full_listify(a) converts all sets in a into lists.

(defun $full_listify (a)
  (setq a (subst '(mlist simp) '($set simp) a :test 'equal))
  (subst '(mlist simp) '($set simp) a :test 'equal))

(defprop $set simp-set operators)

;; Simplify a set. 

(defun simp-set (a y z)
  (declare (ignore y))
  (cond ((not (memq 'simp (car a)))
	 (setq a (mapcar #'(lambda (x) (simplifya x z)) (cdr a)))
	 (setq a (sorted-remove-duplicates (sort a '$orderlessp)))
	 `(($set simp) ,@a))
	(t a)))

;; Return true iff the operator of a is set.

(defun $setp (a)
  (and (consp a) (not (null (memq '$set (car a))))))

;; Return the cardinality of a set; if the argument is a list, convert it to a
;; set. Works even when simp : false.  For example,

;; (C1) cardinality(set(a,a,a)), simp : false;
;; (D1) 				   1
 
(defun $cardinality (a)
  (length (require-set a "$cardinality")))

;; Return true iff a is a subset of b. If either argument is a list, first 
;; convert it to a set. Signal an error if a or b aren't lists or sets.

(defun $subsetp (a b)
  (setq a (require-set a "$subsetp"))
  (setq b (require-set b "$subsetp"))
  (and (<= (length a) (length b)) (set-subsetp a b)))

;; Return true iff sets a and b are equal;  If either argument is a list, first
;; convert convert it to a set. Signal an error if either a or b aren't lists
;; or sets.

(defun $setequality (a b)
  (setq a (require-set a "$setequality"))
  (setq b (require-set b "$setequality"))
  (and (= (length a) (length b)) (every #'like a b)))
	
;;  Adjoin x to the list or set a and return a set.

(defun $adjoin (x a)
  (setq a (require-set a "$adjoin"))
  (multiple-value-bind (f i) (b-search-expr x a 0 (length a))
    (cond ((not f)
	   (setq a (nconc (subseq a 0 i) (cons x  (nthcdr i a))))
	   `(($set simp) ,@a))
	  (t `(($set simp) ,@a)))))
		
;; union(a1,a2,...an) returns the union of the sets a1,a2,...,an. 
;; If any argument is a list, convert it to a set. Signal an error 
;; if one of the arguments isn't a list or a set. When union receives 
;; no arguments, it returns the empty set.

(defun $union (&rest a)
  (let ((acc nil))
    (dolist (ai a `(($set simp) ,@acc))
      (setq acc (set-union acc (require-set ai "$union"))))))

;; Remove elements of b from a. Works on lists or sets.

(defun $setdifference (a b)
  `(($set simp) ,@(sset-difference (require-set a "$setdifference")
				   (require-set b "$setdifference"))))

;; intersection(a1,a2,...an) returns the intersection of the sets 
;; a1,a2,...,an. Signal an error if one of the arguments isn't a 
;; list or a set. intersection must receive at least one argument.

(defun $intersection (a &rest b)
  (let ((acc (require-set a "$intersection")))
    (cond ((consp b)
	   (dolist (bi b)
	     (setq acc (set-intersect acc (require-set bi "$intersection"))))))
    `(($set simp) ,@acc)))
    
;; intersect is an alias for intersection.

(defun $intersect (a &rest b)
  (apply '$intersection (cons a b)))

;; Return true iff x as an element of the set or list a.  Use like 
;; to test for equality. Signal an error if a isn't a set or list.

(defun $elementp (x a)
  (setq a (require-set a "$elementp"))
  (b-search-expr x a 0 (length a)))
 
;; dupe(e,n) returns an n element Maxima list [e,e,e,...e]. When n < 0 or
;; n isn't an integer, signal an error. 

(defun $dupe (e n)
  (cond ((and (integerp n) (> n -1))
	 (cons '(mlist simp) (make-list n :initial-element e)))
	(t 
	 (merror "Second argument to dupe must be a nonnegative integer"))))

;; Return true if and only if the lists or sets a and b are disjoint;
;; signal an error if a or b aren't lists or sets.

#|
(defun $disjointp-binary-search-version (a b)
  (setq a (require-set a "$disjointp"))
  (setq b (require-set b "$disjointp"))
  (if (> (length a) (length b)) (rotatef a b))
  (let ((n (length b)))
    (catch 'disjoint 
      (dolist (ai a)
	(if (b-search-expr ai b 0 n) (throw 'disjoint nil)))
      t)))
|#

(defun $disjointp (a b)
  (setq a (require-set a "$disjointp"))
  (setq b (require-set b "$disjointp"))
  (set-disjointp a b))

;; Return the set of elements of the list or set a for which the predicate 
;; f evaluates to true; signal an error if a isn't a list or a set.

(defun $subset (a f)
  (setq a (require-set a "$subset"))
  (let ((acc))
    (dolist (x a `(($set simp) ,@(nreverse acc)))
      (if (mfuncall f x) (setq acc (cons x acc))))))

;; Return a list of two sets. The first set is the subset of a for which
;; the predicate f evaluates to true and the second is the subset of a
;; for which f evaluates to false.

(defun $partition_set (a f)
  (setq a (require-set a "$subset"))
  (let ((t-acc) (f-acc))
    (dolist (x a `((mlist simp) (($set simp) ,@(nreverse f-acc)) 
		   (($set simp) ,@(nreverse t-acc))))
      (if (mfuncall f x) (setq t-acc (cons x t-acc)) (setq f-acc (cons x f-acc))))))

;; Return the union of a - b and b - a; signal an error if a or b
;; aren't lists or sets.

(defun $symmdifference (a b)
  `(($set simp) ,@(set-symmetric-difference 
		   (require-set a "$symmdifference")
		   (require-set b "$symmdifference"))))

;; Return a list of the elements in b that are not in a.

(defun $complement (a b)
  `(($set simp) ,@(sset-difference (require-set b "$complement")
				   (require-set a "$complement"))))

;; Return the set of all subsets of a.  If a has n elements, powerset(a) has
;; 2^n elements.  Signal an error if the argument isn't a list or set.

(defun $powerset (a)
  (setq a (mapcar #'(lambda (x) (cons '($set simp) (sort x '$orderlessp)))
		  (xpowerset (require-set a "$powerset"))))
  `(($set simp) ,@(sort a '$orderlessp)))

;; powerset is defined in ratout.lisp. We'll name this function xpowerset. 

(defun xpowerset (a)
  (cond ((null a) (list nil))
	(t
	 (let ((x (car a))
	       (b (xpowerset (cdr a))))
	   (append b (mapcar #'(lambda (u) (cons x u)) b))))))

;; Return the set of all subsets of a that have exactly n elements.
;; Signal an error if the first argument isn't a list or a set or if
;; the second argument isn't a nonnegative integer.

(defun $subpowerset (a n)
  (setq a (require-set a "$subpowerset"))
  (cond ((and (integerp n) (> n -1))
	 (cond ((> n 0)
		(setq a (mapcar #'(lambda (x) (simp-set `(($set) ,@x) 1 nil))
				(subpowerset a n))))
	       ((= n 0)
		(setq a '((($set))))))
	 (simp-set `(($set) ,@a) 1 nil))
	(t
	 (merror "Second argument to subpowerset must be a nonnegative integer, instead found ~:M" n))))
  
(defun subpowerset (a n)
  (cond ((or (< n 1) (null a))
	 nil)
	((= n 1) (mapcar #'list a))
	(t (let ((x (car a))
		 (b (subpowerset (cdr a) (- n 1))))
	     (append (subpowerset (cdr a) n)
		     (mapcar #'(lambda (u) (cons x u)) b))))))

(defun $permutations (a)
  (if ($listp a) (setq a (cdr a))
    (setq a (require-set a "$permutations")))
  (cons '($set)(mapcar #'(lambda (x) (cons '(mlist simp) x))(permutation a))))

(defun permutation (a)
  (cond ((< (length a) 2)
	 (list a))
	(t
	 (let ((p) (ai) (n (length a)))
	   (dotimes (i n p)
	     (setq ai (car a))
	     (setq p (append p (mapcar #'(lambda (x) 
					   (cons ai x)) (permutation (cdr a)))))
	     (setq a (list-rotate a)))))))

(defun list-rotate (a)
  `(,@(rest a) ,(car a)))

;;; Returns 3 values
;;; FOUND -- is X in L
;;; POSITION -- where is X in L; if not in L, position it is before
;;; REST -- everything after X in L

(defun b-search-expr (x l lo len)
  (declare (fixnum lo len))
  (if (= len 0) (values nil lo l)
    (let ((mid) (midl))
      (while (> len 1)
        (if ($orderlessp x (car (setq midl (nthcdr (setq mid (floor len 2)) l))))
            (setq len mid)
          (setq l midl
		lo (+ lo mid)
                len (- len mid))))
      (cond (($orderlessp x (nth 0 l)) (values nil lo l))
            ((like x (nth 0 l)) (values t lo (cdr l)))
            (t (values nil (1+ lo) (cdr l)))))))

;; Flatten is somewhat difficult to define -- essentially it evaluates an 
;; expression as if its main operator had been declared nary; however, there 
;; is a difference.  We have

;; (C1) load("flatten.lisp");
;; (D1)         flatten.lisp
;; (C2) flatten(f(g(f(f(x)))));
;; (D2)         f(g(f(f(x))))
;; (C3) declare(f,nary);
;; (D3)         DONE
;; (C4) ev(d2);
;; (D4)         f(g(f(x)))

;; Unlike declaring the main operator of an expression to be nary, flatten 
;; doesn't recurse into other function arguments.  

;; This is supposed to be a clone of Macsyma's flatten function.  
;; Unlike the Macsyma version, this version
;;    (a) handles CRE expressions,
;;    (b) doesn't try to flatten expressions of the form a^(b^c) -- Macsyma's
;;        flatten gives an error about a "wrong number of arguments to "^"."

;; There are other functions other than ^ that we shouldn't try
;; to flatten -- Bessel functions, etc.  

;; Return the operator and argument of the expression e.  Remove simp
;; from the op.

(defun get-op-and-arg (e)
  (let ((op) (arg))
    (cond ((or ($atom e) ($subvarp e))
	   (setq op nil)
	   (setq arg nil))
	  ((and (consp (nth 0 e)) ($subvarp (nth 1 e)))
	   (setq op `(,(nth 0 e) ,(nth 1 e)))
	   (setq arg (cddr e)))
	  (t
	   (setq op (nth 0 e))
	   (setq arg (cdr e))))
    (values (remove 'simp op) arg)))

(defun $flatten (e)
  (setq e (ratdisrep e))
  (cond ((or ($atom e) ($subvarp e) (eq ($inpart e 0) '&^)) e)
	(t
	 (multiple-value-bind (op e) (get-op-and-arg e)
	   (setq e (mapcan #'(lambda (x) (flatten-op x op)) e))
	   (cond ((and (consp (car op)) (eq (caar op) 'mqapply))
		  (append op e))
		 (t
		  `(,op ,@e)))))))

(defun flatten-op (e op)
  (multiple-value-bind (e-op e-arg) (get-op-and-arg e)
    (cond ((equal e-op op)
	   (mapcan #'(lambda (x) (flatten-op x op)) e-arg))
	  (t
	   (list e)))))

(defun sorted-remove-duplicates (l)
  (do ((l l (cdr l))
       (acc))
      ((null l) (nreverse acc))
    (if (not (and (cdr l) (like (car l) (cadr l))))
	(setq acc (cons (car l) acc)))))

;; Code from Stavros Macrakis

(defmacro do-merge-symm (list1 list2 eqfun lessfun bothfun onefun)
  ;; Like do-merge-asym, but calls onefun if an element appears in one but
  ;; not the other list, regardless of which list it appears in.
  `(do-merge-asym ,list1 ,list2 ,eqfun ,lessfun ,bothfun ,onefun ,onefun))

(defmacro do-merge-asym
  (list1 list2 eqfun lessfun bothfun only1fun only2fun)
  ;; Takes two lists.
  ;; The element equality function is eqfun, and they must be sorted by lessfun.
  ;; Calls bothfun on each element that is shared by the two lists;
  ;; calls only1fun on each element that appears only in the first list;
  ;; calls only2fun on each element that appears only in the second list.
  ;; If both/only1/only2 fun are nil, treat as no-op.
  ;; Initializes the variable "res" to nil; returns its value as the result.
  (let ((l1var (gensym))
	(l2var (gensym)))
    `(do ((,l1var ,list1)
	  (,l2var ,list2)
	  res)
	 ;; The variable RES is for the use of both/only1/only2-fun
	 ;; do-merge-asym returns (nreverse res)
	 ((cond ((null ,l1var)
		 (if ,only2fun
		     (while ,l2var
		       (funcall ,only2fun (car ,l2var))
		       (setq ,l2var (cdr ,l2var))))
		 t)
		((null ,l2var)
		 (if ,only1fun
		     (while ,l1var
		       (funcall ,only1fun (car ,l1var))
		       (setq ,l1var (cdr ,l1var))))
		 t)
		((funcall ,eqfun (car ,l1var) (car ,l2var))
		 (if ,bothfun (funcall ,bothfun (car ,l1var)))
		 (setq ,l1var (cdr ,l1var) ,l2var (cdr ,l2var))
		 nil)
		((funcall ,lessfun (car ,l1var) (car ,l2var))
		 (if ,only1fun (funcall ,only1fun (car ,l1var)))
		 (setq ,l1var (cdr ,l1var))
		 nil)
		(t
		 (if ,only2fun (funcall ,only2fun (car ,l2var)))
		 (setq ,l2var (cdr ,l2var))
		 nil))
	  (nreverse res)))))

;;; Test
; (do-merge-asym '(a a a b c g h k l)
; 	       '(a b b c c h i j k k)
; 	       'eq
; 	       'string<
; 	       '(lambda (x) (prin0 'both x))
; 	       '(lambda (x) (prin0 'one1 x))
; 	       '(lambda (x) (prin0 'one2 x)))
; both a
; one1 a
; one1 a
; both b
; one2 b
; both c
; one2 c
; one1 g
; both h
; one2 i
; one2 j
; both k
; one2 k
; nil

;;; Examples of use

(defun set-intersect (l1 l2)
  ;;  Only works for lists of sorted by $orderlessp.
  (do-merge-symm
   l1 l2
   #'like
   #'$orderlessp
   #'(lambda (x) (push x res))
   nil))

(defun set-union (l1 l2)
  ;; Only works for lists of sorted by $orderlessp.
  (do-merge-symm
   l1 l2
   #'like
   #'$orderlessp
   #'(lambda (x) (push x res))
   #'(lambda (x) (push x res))))

(defun sset-difference (l1 l2)
  ;; Only works for lists of sorted by $orderlessp.
  (do-merge-asym
   l1 l2
   #'like
   #'$orderlessp
   nil
   #'(lambda (x) (push x res))
   nil))

(defun set-subsetp (l1 l2)
  ;; Is l1 a subset of l2
  (catch 'subset
    (do-merge-asym
     l1 l2
     #'like
     #'$orderlessp
     nil
     #'(lambda (x) (declare (ignore x)) (throw 'subset nil))
     nil)
    t))

(defun set-symmetric-difference (l1 l2)	; i.e. xor
  (do-merge-symm
   l1 l2
   #'like
   #'$orderlessp
   nil
   #'(lambda (x) (push x res))))
	
(defun set-disjointp (l1 l2)
  (catch 'disjoint
    (do-merge-symm
     l1 l2
     #'like
     #'$orderlessp
     #'(lambda (x) (declare (ignore x)) (throw 'disjoint nil))
     nil)
    t))
   
;; Return { x in a | f(x) = maximum of f on a}. When the optional
;; third argument has a value, minimize instead of maximize.

(defun $extremal_subset (a f &optional s)
  (setq a (require-set a "$extremal_subset"))
  (cond ((null a) 
	 `(($set simp)))
	(t
	 (if s (setq s -1) (setq s 1))
	 (let* ((max-subset (nth 0 a))
		(mx (mul s (mfuncall f max-subset)))
		(x))
	   (setq max-subset `(,max-subset))
	   (setq a (cdr a))
	   (dolist (ai a)
	     (setq x (mul s (mfuncall f ai)))
	     (cond ((is-boole-check (mgrp x mx))
		    (setq mx x
			  max-subset `(,ai)))
		   ((like x mx)
		    (setq max-subset (cons ai max-subset)))))
	   `(($set simp) ,@(nreverse max-subset))))))
   
(defun bool-checked-mfuncall (f x y)
  (let ((bool (is-boole-check (mfuncall f x y))))
    (if (not (or (eq bool 't) (eq bool nil))) 
	(merror "~:M(~:M,~:M) doesn't evaluate to a boolean" f x y)
      bool)))
    
;; Return the set of equivalence classes of f on the set l.  The
;; function f must be an boolean-valued function defined on the
;; cartesian product of l with l; additionally, f should be an
;; equivalence relation.

;; The lists acc and tail share structure.
           
(defun $equiv_classes (l f)
  (setq l (require-set l "$eclass"))
  (do ((l l (cdr l))
       (acc)
       (tail)
       (x))
      ((null l) (cons '($set) (mapcar #'(lambda (x) (cons '($set) x)) acc)))
    (setq x (car l))
    (setq tail (member-if #'(lambda (z) (bool-checked-mfuncall f x (car z))) acc))
    (cond ((null tail)
	   (setq acc (cons `(,x) acc)))
	  (t
	   (setf (car tail) (cons x (car tail)))))))

;; cartesian_product(a,b1,b2,...,bn) returns the set with members
;; of the form [x0,x1, ..., xn], where x0 in a,  x1 in b1, ... , and 
;; xn in bn. With just one argument cartesian_product(a) returns the 
;; set with members [a1],[a2], ... [an], where a1, ..., an are the members of a.

;; Signal an error when a or any b isn't a list or a set.

;; After completing the dolist (bi b), the list a doesn't have duplicate 
;; members -- thus we can get by with  only sorting a.

(defun $cartesian_product (a &rest b)
  (setq a (mapcar #'list (require-set a "$cartesian_product")))
  (let ((acc))
    (dolist (bi b)
      (setq acc nil
	    bi (require-set bi "$cartesian_product"))
      (dolist (bij bi (setq a acc))
	(setq acc (append acc (mapcar #'(lambda (x) (cons bij x)) a))))))
  (cons '($set simp) 
	(sort (mapcar #'(lambda (x) (cons '(mlist simp) (reverse x))) a) 
	      '$orderlessp))) 
      
  

