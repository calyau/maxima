;; Maxima code for extracting all powers
;; Author: Barton Willis
;; Send bug reports to willisb@unk.edu

;; This code is in the public domain.  It has no warranty. Use this
;; code at your own risk. 

(in-package "MAXIMA")

;; Return true iff the main operator of e is '+'.

(defun addp (e)
  (declare (inline addp))
  (like ($inpart e 0) '&+))

;; Return true iff the main operator of e is '*'.

(defun multp (e)
  (declare (inline multp))
  (like ($inpart e 0) '&*))

;; Return true iff the main operator of e is '^'.

(defun expp (e)
  (declare (inline expp))
  (like ($inpart e 0) '&^))

(defun powers (e x)
  (cond 
   (($atom e)
    (if (like e x) (list 1) (list 0)))
   (($freeof x e) (list 0))
   ((and (expp e) (like (cadr e) x)) (list (caddr e)))
   ((multp e) (powers-mul (margs e) x))	  
   ((addp e)
    (let ((acc nil))
      (powers-add (margs e) x acc)))
   (t nil)))

;; If e is a list, map $powers over the list.  If e is a sum of powers of
;; powers of x, return a list of the exponents; otherwise, return false.
;; $powers works on the internal form of e;  call it a bug or a feature, power 
;; doesn't expand e. 

(defun $powers (e x)
  (cond (($symbolp x)
	 (cond (($listp e)
		(cons '(mlist) (mapcar #'(lambda (s) ($powers s x)) (margs e))))
	       (t
		(let ((p (powers e x)))
		  (cond
		   ((like p nil) nil)
		   (t (cons '(mlist) p)))))))
	(t (merror "Second argument to POWERS must be a symbol"))))

;;  Insert x into a sorted list a. For many expressions, this function
;; isn't as inefficient as it looks; the Maxima simpilifer often sorts
;; the powers from highest to lowest.  When this is the case, 
;; insert-power only has to make one comparision.  This function is
;; very much faster than using merge.

(defun insert-power (x a)
  (cond ((null a) x)
	((eql (car x) (car a)) a)
	(($ordergreatp (car x) (car a)) (cons (car x) a))
	(t 
	 (cons (car a) (insert-power x (cdr a))))))

;; Map powers over a Lisp list x using acc as an accumulator.

(defun powers-add (e x acc)
  (cond  
   ((null e) acc)
   (t
    (let (p)
      (setq p (powers (car e) x))
      (cond (p
	     (setf acc (insert-power p acc))
	     (powers-add (cdr e) x acc))
	    (t nil))))))

;;  If only one element of the list e is a power of x, return the power
;;  otherwise, return nil.

(defun powers-mul (e x)
  (cond
   ((null e) nil)
   (($freeof x (car e)) (powers-mul (cdr e) x))
   ((freeofl x (cdr e)) (powers (car e) x))
   (t nil)))


