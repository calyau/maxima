;;; -*- Mode: Lisp -*-
;;;
;;; PERMUT: A Package for generating permutations

#-NIL(includef (cond ((status feature ITS)     '|libmax;prelud >|)
		((status feature Multics) '|prelude|)
		((status feature Unix)    '|libmax//prelud.l|)
		(t (error '|Unknown system -- see MC:LIBMAX;INCLUD >|))))

#-NIL(declare (mapex T))

(defun all_permutations (l)
   (if (null l) '(())
       (do ((elt (car l))
	    (permute (all_permutations (cdr l)) (cdr permute))
	    (ans))
	   ((null permute) ans)
	  (setq ans
		(append (permute_an_elt_through elt (car permute)) ans)))))

(defun permute_an_elt_through (elt l)
   (if (null l) (ncons (ncons elt))
       (cons (cons elt l)
	     (mapcar #'(lambda (x) (cons (car l) x))
		     (permute_an_elt_through elt (cdr l))))))

(defun $permutations (l)
   (if ($listp l)
       (cons '(MLIST) (mapcar #'(lambda (x) (cons '(MLIST) x))
			      (all_permutations (cdr l))))
       (displa l)
       (merror "~&Error, argument not a list---PERMUTATIONS~%")))
