(in-package "MAXIMA")

(defvar $todd_coxeter_state nil)
(proclaim '(type (vector t)  $todd_coxeter_state))
;;  To turn on debug printing set to T
(defvar *debug* nil)
;;  When *debug* is not nil, this holds the multiplications for
;; the current row.
(defvar *this-row* nil)

(eval-when (compile eval)
  (defmacro nvars () '(aref $todd_coxeter_state 0))
  (defmacro ncosets() '(aref $todd_coxeter_state 1))
  (defmacro multiply-table () '(aref $todd_coxeter_state 2))
  (defmacro relations () '(aref $todd_coxeter_state 3))
  (defmacro subgroup-generators () '(aref $todd_coxeter_state 4))
  (defmacro row1-relations () '(aref $todd_coxeter_state 5))
  (defmacro with-multiply-table (&body body)
    `(let ((nvars (nvars))(multiply-table (multiply-table)))
      (declare (fixnum nvars) (type (vector t) multiply-table))
      ,@ body)) 

  (defmacro  undef (s) `(eql 0 ,s))

  ;; Multiply coset K times variable R
  ;; jfa: original macro mult renamed to tc-mult to avoid clash with
  ;; maxima function mult
  (defmacro tc-mult (k r) `(the coset (aref (table ,r) ,k)))

  ;; Force  k . r = s and  k = s . r^-1
  (defmacro define-tc-mult (k r s)
    `(progn (setf (tc-mult ,k ,r) ,s)
      (setf (tc-mult ,s (- ,r)) ,k)))

  ;; cosets M < N are to be made equal
  (defmacro push-todo (m n)
    `(progn (vector-push-extend ,m *todo*)
      (vector-push-extend ,n *todo*)))

  #-(and :ansi-cl (not gcl))
  (defmacro f+ (a b) `(the fixnum (+ (the fixnum ,a) (the fixnum ,b))))
  #+(and :ansi-cl (not gcl))
  (define-compiler-macro f+ (a b) `(the fixnum (+ (the fixnum ,a) (the fixnum ,b))))
  #-(and :ansi-cl (not gcl))
  (defmacro f- (a &optional b) `(the fixnum (- (the fixnum ,a)
					     ,@ (if b `((the fixnum ,b))))))
  #+(and :ansi-cl (not gcl))
  (define-compiler-macro f- (a &optional b) `(the fixnum (- (the fixnum ,a)
							  ,@ (if b `((the fixnum ,b))))))
  
  ;; The multiplication table for variable I 
  (defmacro table (i)
    `(the (vector (coset)) (aref  multiply-table (f+ ,i nvars))))

  ;; Some optional declarations of functions.
  (proclaim '(ftype (function (fixnum) t) doing-row)) 
  (proclaim '(ftype (function (t t t) t) set-up todd-coxeter )) 
  (proclaim
   '(ftype (function nil t) fill-in-inverses dprint-state next-coset
     dcheck-tables replace-coset-in-multiply-table)) 
  (proclaim
   '(ftype (function (t) t) $todd_coxeter coerce-rel has-repeat)) 
  (proclaim '(ftype (function (t t) t) my-print))
  (proclaim '(ftype (function (t *) t) $todd_coxeter))


  ) ;; end of the macros and proclamations.

(eval-when (compile eval load)
  (deftype coset nil 'fixnum)
  (proclaim '(type (vector (coset)) *todo*))
  )

;; The data type we use to enumerate cosets.


(defvar *todo* (make-array 10 :element-type 'coset :fill-pointer 0 :adjustable t))

;; NVARS is the number of of variables.   It should be the maximum
;; of the absolute values of the entries in the relations RELS.
;; The format of the relations is variables X,Y,.. correspond to
;; numbers 1,2,.. and X^^-1, Y^^-1 .. are -1,-2,...   RELS is
;; a list of lists in these variables.
;; Thus rels = '((1 -2 -1) (2 2 3) ..)  (ie [x1.x2^^-1 . x1^^-1, x2.x2.x3,.. ))
;; SUBGP is also a list of lists.
;; Returns order of G/H, where G is Free/(rels), and H is
;; This is the main entry point at lisp level.
;; Example: (TODD-COXETER 2 '((1 1) (1 2 1 2 1 2) (2 2)))
;; returns 6.   In (multiply-table) we find the current
;; state of the action of the variables on the cosets G/H.
;; For computing the symmetric group using the relations
;; p(i,j) :=concat(x,i).concat(x,j);
;; symet(n):=create_list(if (j - i) = 1 then (p(i,j))^^3 else
;;     if (not i = j) then (p(i,j))^^2 else p(i,i) , j,1,n-1,i,1,j);
;; todd_coxeter(symet(n)) == n!
;; the running time of the first version of this code is observed to be quadratic
;; in the number of cosets.  On a rios it is approx 5*10^-5 * (ncosets)^2.

(defun todd-coxeter (nvars rels subgp &aux (i 1) (c 0 ))
  (declare (fixnum i c))
  (set-up nvars rels subgp)
  (sloop while (>= (ncosets) i)
	 do  (incf c) ;; count how many row tries.. 
	 (cond
	   ;; row still being done
	   ((doing-row i) (replace-coset-in-multiply-table))
	   ;; row finished but there is work to do
	   ((> (the fixnum (fill-pointer *todo*)) 0)
	    (incf i) (replace-coset-in-multiply-table))
	   ;; row finished -- no work
	   (t (incf i))))
  (format t "~%Rows tried ~a" c) 
  (ncosets))

;; Store the data in $todd_coxeter_state, and build multiply-table.
(defun set-up (nvars rels subgp)
  (setf $todd_coxeter_state  (make-array 6))
  (setf (nvars) nvars)
  (setf (ncosets) 1)
  (setf (relations) rels)
  (setf (subgroup-generators) subgp)
  (setf (row1-relations) (append (subgroup-generators)  (relations)))
  (setf (fill-pointer *todo*) 0)
  (setf (multiply-table) (make-array (* 2 (1+ (nvars)))))
  (with-multiply-table
      (sloop for rel in (row1-relations) do
	     (sloop for v in rel
		    do (or (<= 1 (abs v)  nvars)
			   (error
			    "Vars must be integers with absolute value between 1 and ~a"
			    nvars))))
    (sloop for i from (f- nvars)  to nvars
	   when (not (zerop i))
	   do (setf (table i) (make-array 10 :adjustable t :element-type 'coset)))
    ))

;; Starts multiplying coset i times the relations.  Basic fact is i . rel = i.
;; This gives a condition on the multiplication table.  Once we have made it all
;; the way through the relations for a given coset i, and NOT had any
;; incosistency in our current multiplication table, then we go on the the next
;; coset.  The coset 1 denotes H.  so for generators h of H we we have 1 . h = 1.
;; So when we do row 1, we add to the relations the generators of H.

;; When we do find an inconsistency eg: 7 . y = 1 and 4 . y = 1 or 7 = 1 . y^^-1
;; and 4 . y = 1, then we would know that 4 and 7 represent the same coset, and
;; so we put 4 and 7 in the *todo* vector and return t so that
;; replace-coset-in-multiply-table will identify them.  While we are running
;; inside doing-row, the multiply-table is accurate, up to our current state of
;; knowledge.  Note that once we find such a nonpermutation action of y, we could
;; not maintain the consistency of (table i) and (table -i).  We exit doing-row
;; with value t, to indicate replacements should be done, and that we must
;; return to complete row i.  (Actually we return t even in the case we were
;; finished the row and found the duplicate in the last step).

(defun doing-row ( i &aux (j 0) (k 0) (r 0)(s 0) *this-row* relations)
  (declare (fixnum j  k i r s))
  (setf relations (if (eql i 1) (row1-relations) (relations)))
  (with-multiply-table
      (sloop for rel in relations
	     for v on relations
	     do
	     (setq k i)
	     (sloop 
	      do
	      (setq r (car rel))
	      (setq s (tc-mult  k r))
	      (cond
		((undef s)
		 (cond ((cdr rel)
			(setq s  (next-coset))
			(define-tc-mult k r s))
		       (t (setq s (tc-mult i  (- r)))
			  (cond ((undef s) (define-tc-mult k r i))
				((< k s) (push-todo k s)(return-from doing-row (cdr v)))
				((> k s) (push-todo s k)(return-from doing-row (cdr v))))
			  (loop-finish)))))
	      (cond ((setq rel (cdr rel))
		     (when *debug* 
		       (push s *this-row*)
		       (my-print (reverse *this-row*) i))
		     (setq k s)
		     (incf j))
		    ((< i s) (push-todo i s) (return-from doing-row (cdr v)))
		    ((> i s) (push-todo s i) (return-from doing-row (cdr v)))
		    (t		     ;rel is exhausted and it matched 
		     (loop-finish))))))
  (when *debug*  (dcheck-tables) 
	(my-print (reverse *this-row*) i))
  nil)

;; FILL-IN-INVERSES not only completes the (table i) for i < 0
;; but at the same time checks that (table i) for i > 0
;; does not have repeats.   eg if  5 . y = 3 and 7 . y = 3,
;; then this would show up when we go to build the inverse.
;; if it does we add 5 and 7 to the *todo* vector.

(defun fill-in-inverses (&aux (s 0) (sp 0))
  (declare (fixnum s sp))
					;(format t "~%In fillThere are now ~a cosets" (ncosets))
  (with-multiply-table
      (sloop for i from 1 to nvars
	     do (let ((ta1 (table i))
		      (ta2 (table (- i))))
		  (declare (type (vector (coset)) ta1 ta2))
		  (sloop for j from 1 to (ncosets) do (setf (aref ta2 j) 0))
		  (sloop for j from 1 to (ncosets)  do
			 (setf s (aref ta1 j))
			 when (not (eql 0 s))
			 do
			 (setf sp (aref ta2 s))
			 (cond ((eql 0 sp) (setf (aref ta2 s) j))
			       (t ;; there's a duplicate!
				(push-todo sp j)
				(return-from fill-in-inverses t))))))))
		

;; set n (vector-pop *todo*) , m (vector-pop *todo*)
;; and replace n by m in multiply-table and in *todo*.
;; The replacement is done carefully so as not to lose ANY
;; information from multiply-table, without recording it in
;; *todo*.   It finishes by doing FILL-IN-INVERSES which may
;; in turn cause entries to be added to *todo*.

(defun replace-coset-in-multiply-table (&aux (m 0) (n 0) (s 0) (s2 0) )
  (declare (fixnum m n s s2 ))
  (with-multiply-table	   
      (tagbody
       again
	 (setf n (vector-pop *todo*))
	 (setf m (vector-pop *todo*))
	 (unless (eql m n)
	   (dprint-state)
	   (if *debug* (format t "     ~a --> ~a " n m))
	   
	   (sloop for i from 1 to nvars
		  do
		  (let ((ta (table i)))
		    (declare (type  (vector (coset)) ta))
		    (setq s2 (tc-mult n i))
		    (unless (undef s2)
		      (setq s (tc-mult m i))
		      (cond
			((undef s) (setf (tc-mult m i) s2))
			((< s s2) (push-todo s s2))
			((> s s2)(push-todo s2 s))))
		    (sloop for  j downfrom (f- n 1) to 1
			   do (setq s (aref ta j))
			   (cond ((>  s n) (setf (aref ta j) (f- s 1)))
				 ((eql s n) (setf (aref ta j) m) )))
		    (sloop for  j from n below (ncosets)
			   do (setq s (aref ta (f+ j 1)))
			   (cond ((>  s n) (setf (aref ta j) (f- s 1)))
				 ((eql s n) (setf (aref ta j) m) )
				 (t  (setf (aref ta j) s))))))
		
	   (sloop for i downfrom (f- (fill-pointer *todo*) 1) to 0
		  do (setf s (aref *todo* i))
		  (cond ((> s n) (setf (aref *todo* i) (f- s 1)))
			((eql s n)(setf (aref *todo* i)  m))))
	   (decf (ncosets))
	   (dprint-state)
	   (progn nil)
	   )

	 (if (>  (the fixnum (fill-pointer *todo*)) 0) (go again))
	 ;;(format t "~%There are now ~a cosets" (ncosets))
	 ;; check for new duplicates introduced!!
	 (if (fill-in-inverses) (go again))
	 )))

;; Get the next coset number, making sure the multiply-table will
;; have room for it, and is appropriately cleaned up.
(defun next-coset ()
  (let* ((n (f+ 1 (ncosets)))
	 (m 0))
    (declare (fixnum n))
    (with-multiply-table    
	(let ((ta (table 1)))
	  (unless (> (the fixnum (array-total-size ta)) (f+ n 1))
	    (setf m (f+ n  (ash n -1)))
	    (sloop for i from (f- 0 nvars) to nvars 
		   when (not (eql i 0))
		   do (setf ta (table i))
		   (setf (table i) (adjust-array   ta m ))))
	  (sloop for i from 1  to nvars
		 do (setf (aref (table i) n) 0)
		 (setf (aref (table (f- i)) n) 0))))
    (setf (ncosets) n)))



;;  $todd_coxeter parses maxima args
;; todd_coxeter(rels, subgrp) computes the
;; order of G/H where G = Free(listofvars(rels))/subgp_generated(rels));
;; and H is generated by subgp.   Subgp defaults to [].
;; todd_coxeter([x^^3,y.x.y^^-1 . x^^-1],[])  gives 6  the order of the symmetric group
;; on 3 elements.

(defun $todd_coxeter (rels &optional (subgp '((mlist))))
  (let ((vars ($sort ($listofvars rels)))
	(neg 1))
    (declare (special neg vars))
    (todd-coxeter ($length vars) (mapcar 'coerce-rel (cdr rels))
		  (mapcar 'coerce-rel (cdr subgp))
		  )))

(defun coerce-rel (rel )
  (declare (special vars neg))
  (cond ((atom rel)(list (* neg (position rel vars))))
	(t (case (caar rel)
	     (mnctimes (apply 'append (mapcar 'coerce-rel (cdr rel))))
	     (mncexpt
	      (let* ((n  (meval* (third rel)))
		     (neg (signum n))
		     (v (coerce-rel (second rel))))
		(declare (special neg))
		(sloop for i below (abs (third rel))
		       append v)))
	     (otherwise (error "bad rel"))))))

;; The following functions are for debugging purposes, and
;; for displaying the rows as they are computed.   They are
;; not required for correct running.
;;#+debug
(progn
  (defvar *names* '(nil x y z))
  (defun my-print (ro i  &aux relations)
    (when *debug*
      (fresh-line)
      ;;  (print ro)
      (format t "Row ~a " i)
      (setq relations (if (eql i 1) (row1-relations) (relations)))
      (sloop for rel in relations
	     do 
	     (sloop for v on rel
		    do (format t
			       (if (> (car v) 0) "~a" "~(~a~)")
			       (nth (abs (car v)) *names*))
		    (cond ((null ro) (return-from my-print)))
		    (if (cdr v) (princ (pop ro))
			(format t "~a | ~a" i i))))))
     
  (defun has-repeat (ar &aux (j (+ 1 (ncosets)))  ans tem)
    (sloop for k from 1 to (ncosets)
	   do (setq tem (aref ar k))
	   (cond ((and  (not (eql tem 0))
			(find tem ar :start (+ k 1) :end j))
		  (pushnew tem ans))))
    ans)

  (defun dcheck-tables ( &aux tem )
    (when *debug*
      (with-multiply-table
	  (sloop for i from 1 to (nvars)
		 do (if (setq tem (has-repeat (table i )) )
			(format t "~%Table ~a has repeat ~a " i tem))))))
     
  (defun dprint-state ()
    (when *debug*
      (with-multiply-table
	  (format t "~%Ncosets = ~a, *todo*=" (ncosets) *todo*)
	(sloop for i from 1 to (nvars) do
	       (format t "~%~a:~a" (nth i *names*)
		       (subseq (table i ) 1  (+ 1 (ncosets)))))
	(my-print (reverse *this-row*) 0)
	))
    ))
