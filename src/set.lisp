;; Support for Maxima sets.
;; Author: Barton Willis
;; Send bug reports to willisb@unk.edu

;; This code is in the public domain.  It has no warranty. Use this
;; code at your own risk. 

(in-package "MAXIMA")

;; Use the predicate canonlt to order the elements of a set.  The
;; default is $unorderedp.  The predicate $unorderedp always 
;; returns true; when canonlt is its default value, sets are 
;; never sorted. Other choices for $canonlt include $ordergreatp 
;; and $orderlessp.

(defun $unorderedp (a b) t)
(defmvar $canonlt '$unorderedp)

;; The set package doesn't distinguish between sets and lists.  We're
;; in trouble if we need to work simultaneously with a set of 
;; lists and a set of sets.  The commerical Macsyma seems to treat
;; all set elements as lists; thus setify([[1,2],[2,1]) returns 
;; [[1,2],[2,1]] because [1,2] and [2,1] are treated as lists 
;; (and consequently they are not equal).  In this package, the 
;; user may decide if set elements that are lists are treated as 
;; lists or as sets.  When $set_elements_can_be_sets is true 
;; (the default),  set elements that are lists are treated 
;; as sets; otherwise,  when  $set_elements_can_be_sets is 
;; false, set elements that are lists are treated as lists.

(defmvar $set_elements_can_be_sets t)

;; For non-lists x and y, equalp(x,y) returns is(ratsimp(x-y)=0).
;; Signal an error if either x or y is a list. Since equalp uses 
;; ratsimp, equalp(x/x,1) is true and equalp(x^(a*b),(x^a)^b)
;; is false. 

(defun $equalp (x y)
  (cond ((or ($listp x) ($listp y))
	 (merror "Both arguments to EQUALP must be non-lists."))	
	(t ($xequalp x y))))

;; If you are certain that x and y are not lists, you might call
;; (at Maxima level) ?xequalp instead of equalp.

(defun $xequalp (x y)
  (like 0 ($ratsimp (add* x (*mminus y)))))

;; If x and y are not lists, $elem_equalp(x,y) returns 
;; equalp(x,y).  If x and y are both lists, return 
;; setequality(x,y) if set_elements_can_be_sets; otherwise 
;; return equalp(x[1],y[1]) and equalp(x[2],y[2]) and ....
;; Finally, if exactly one of x or y is a list, return false. 

(defun $elem_equalp (x  y)
  (cond ((and ($listp x) ($listp y)) 
	 (cond ($set_elements_can_be_sets
		($setequality x y))
	       ((and ($emptyp x) ($emptyp y)) t)
	       (t
		(and 
		 (= ($length x) ($length y))
		 ($elem_equalp ($first x) ($first y))
		 ($elem_equalp ($rest x) ($rest y))))))
	((or ($listp x) ($listp y)) nil)
	(t ($xequalp x y))))

;;  Adjoin x to the Maxima list a; use equalp for the equality test.
;;  When a isn't a list, signal an error.

(defun $adjoin (x a)
  (cond (($listp a)
	 (cons '(mlist) (adjoin x (margs a) :test #'$elem_equalp)))
	(t (merror "The second argument to ADJOIN must be a list"))))

;; Setify removes duplicates from a Maxima list and sorts the
;; list using the partial ordering function canonlt. To remove the
;; duplicates from the list, we use element_equalp to test for equality.
;; When the argument isn't a list, signal an error.

(defun $setify (a)
  (cond (($listp a)
	 (mysort (cons '(mlist) (remove-duplicates (margs a) :test #'$elem_equalp))))
	(t (merror "The argument to SETIFY must be a list."))))

;; When $canonlt is $unorderedp, don't sort; when $canonlt isn't
;; $unorderedp, sort the list using the predicate $canonlt.

(defun mysort (a)
  (cond ((eq $canonlt '$unorderedp) a)
	(t ($sort a $canonlt))))

;; The maxima function call union(a1,a2,...an) forms the union of the
;; sets a1,a2,...an.

(defmfun $union ( &rest a)
  (setq a (margs a))
  (cond ((member nil (mapcar #'$listp a))
	 (merror "Each argument to UNION must be a list."))
	(t
	 (cons '(mlist) (remove-duplicates (apply 'append  (map 'list 'rest a)) :test #'$elem_equalp)))))

;; Remove elements of b from a.  Signal an error if a or b aren't lists.
;; Use element_equalp for the equality test.

(defun $setdifference (a b)
  (cond ((and ($listp a) ($listp b))
	 (cons '(mlist) (set-difference (margs a) (margs b) :test #'$elem_equalp)))
	(t (merror "Both arguments to SETDIFFERENCE must be lists."))))

;; Return the intersection of lists a and b.  Use element_equalp for the
;; equality test. Signal an error if a or b aren't lists.

(defmfun $intersection ( &rest a)
  (setq a (margs a))
  (cond ((member nil (mapcar #'$listp a))
	 (merror "Each argument to INTERSECTION must be a list."))
	(t
	 (setq a (mapcar #'margs a))
	 (cons '(mlist)
	       (reduce #'(lambda (x y)
			   (intersection x y :test #'$elem_equalp))
		       a :from-end nil)))))

;; Return true iff a is a subset of b.  Signal an error if
;; a or b aren't Maxima lists.

(defun $subsetp (a b)
  (cond ((and ($listp a) ($listp b))
	 (xsubsetp (margs a) b))
	(t (merror "Both arguments to SUBSETP must be lists."))))

;; xsubsetp returns true if and only if each element of the Lisp
;; list a is a member of the Maxima list b.  This function isn't 
;; inteneded to be a user function; it doesn't check whether b is a 
;; Maxima list. Notice that the empty set is a subset of every 
;; set.

(defun xsubsetp (a b)
  (cond ((null a) t)
	(t
	 (and ($elementp (car a) b) (xsubsetp (cdr a) b)))))

;; Return true iff a is a subset of b and b is a subset of a; return
;; false if a or b are not lists.

(defun $setequality (a b)
  (cond ((and ($listp a) ($listp b))
	 (if (and ($subsetp a b) ($subsetp b a)) t nil))
	(t nil)))


;; Return true iff x as an element of the list a; use $elem_equalp 
;; to test for equality if x isn't a list and use $setequality to 
;; test for equality if x is a list.  Return false if a isn't a list.

(defun $elementp (x a)
  (cond (($listp a)
	 (cond (($listp x)
		(cond ($set_elements_can_be_sets
		       (if (member x (margs a) :test #'$setequality) t nil))
		      (t
		       (if (member x (margs a) :test #'$elem_equalp) t nil))))
	       (t
		(if (member x (margs a) :test #'$elem_equalp) t nil))))
	(t nil)))

;; Return true if e is an empty Maxima list; otherwise, signal an
;; error.

(defun $emptyp(e)
  (cond (($listp e)
	 (like e '((mlist))))
	(t (merror "Argument to EMPTYP must be a list."))))

;; Return an n element Maxima list [e,e,e,...e]. When n < 0 or
;; n isn't an integer, signal an error.

(defun $dupe (e n)
  (cond ((and (integerp n) (> n -1))
	 (cons '(mlist) (make-list n :initial-element e)))
	(t (merror "Second argument to DUPE must be a nonnegative integer."))))

;; Return true if and only if the lists a and b are disjoint;
;; signal an error if a or b aren't lists.

(defun $disjointp (a b)
  (cond ((and ($listp a) ($listp b))
	 (not (intersection (margs a) (margs b) :test #'$elem_equalp)))
	(t (merror "Both arguments to DISJOINTP must be lists."))))

;; Return those elements of a for which the predicate f evaluates
;; to true; signal an error if a isn't a list.

(defun $subset (a f)
  (cond (($listp a)
	 (setq a (margs a))
	 (let ((acc nil))
	   (dolist (x a (cons '(mlist) acc))
	     (if (mfuncall f x) (setq acc (cons x acc))))))
	(t (merror "First argument to SUBSET must be a list."))))

;; Return the union of a - b and b - a; signal an error if a or b
;; aren't lists.

(defun $symmdifference (a b)
  (cond ((and ($listp a) ($listp b))
	 (mfuncall '$union ($setdifference a b) ($setdifference b a)))
	(t (merror "Both arguments to SYMMDIFFERENCE must be lists."))))

;; Return a list of the elements in b that are not in a.

(defun $complement (a b)
  (cond ((and ($listp a) ($listp b))
	 ($setdifference b a))
	(t (merror "Both arguments to COMPLEMENT must be lists."))))

;; Return true if and only if the argument is a Maxima list and the
;; list does not have duplicate elements.  setp doesn't check that
;; the list is ordered according to canonlt.

(defun $setp (a)
  (and ($listp a) (setp (margs a))))

(defun setp (a)
  (cond ((null a) t)
	(t (and (setp (cdr a)) (not (member (car a) (cdr a) :test #'$elem_equalp))))))

;; Return the set of all subsets of a.  If a has n elements, powerset(a) has
;; 2^n elements.  Signal an error if the argument isn't a Maxima list.

(defun $powerset (a)
  (cond (($listp a)
	 (setq a ($setify a))
	 (cons '(mlist) (mapcar #'(lambda (x) (cons '(mlist) x))
				(powerset (margs a)))))
	(t (merror "Argument to POWERSET must be a list."))))

(defun powerset (a)
  (cond ((null a) (list nil))
	(t
	 (let ((x (car a))
	       (b (powerset (cdr a))))
	   (append b (mapcar #'(lambda (u) (cons x u)) b))))))

;; Return the set of all subsets of a that have exactly n elements.
;; Signal an error if the first argument isn't a Maxima list or if
;; the second argument isn't a nonnegative integer.

(defun $subpowerset (a n)
  (cond (($listp a)
	 (setq a ($setify a))
	 (cond ((and (integerp n) (> n -1))
		(cons '(mlist) (mapcar #'(lambda (x) (cons '(mlist) x))
				       (subpowerset (margs a) n))))
	       (t
		(merror "Second argument to SUBPOWERSET must
be a nonnegative integer."))))
	(t (merror "First argument to SUBPOWERSET must be a list."))))

(defun subpowerset (a n)
  (cond ((or (< n 1) (null a))
	 nil)
	((= n 1) (mapcar #'list a))
	(t (let ((x (car a))
		 (b (subpowerset (cdr a) (- n 1))))
	     (append (subpowerset (cdr a) n)
		     (mapcar #'(lambda (u) (cons x u)) b))))))

