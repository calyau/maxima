;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macsyma-module ComGen)

   ;*********************************************************************
   ;****************                                   ******************
   ;**************** Combinatorial Generating Routines ******************
   ;****************                                   ******************
   ;*********************************************************************

;;; Currently this includes a routine $LIST_PERMS for generating a list
;;; of all permutations of the elements of a given list. This should be
;;; generalized to a function $MAP_PERMS which maps a given function over
;;; all permutations without necessarily constructing the list of all
;;; permutations. Similar routines should be provided for generating
;;; ordered and unordered combinations (I already have a lisp version
;;; of these in ALGBRA;TOPOL >. ---Bil


#+Maclisp
(declare (mapex T))

(defun all_permutations (elts)
   (if (null elts) (ncons () )
      (mapcan #'(lambda (sub_perm)
		   (permute_an_elt_through (car elts) sub_perm))
	      (all_permutations (cdr elts)))))

(defun permute_an_elt_through (elt elts)
   (if (null elts) (ncons (ncons elt))
      (cons (cons elt elts)
	    (mapcar #'(lambda (sub_perm_thru) (cons (car elts) sub_perm_thru))
		    (permute_an_elt_through elt (cdr elts))))))

(defun $list_perms (mlist)
   (if ($listp mlist)
       (cons '(MLIST) (mapcar #'(lambda (x) (cons '(MLIST) x))
			      (all_permutations (cdr mlist))))
      (merror "The argument to LIST_PERMS was not a list:~%~M" mlist)))

;;; This is for a temporary backwards compatible definition for ASB.

(defun $permutations (mlist) ($list_perms mlist))
