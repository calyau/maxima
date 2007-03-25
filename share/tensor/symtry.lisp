;;; -*- Mode:LISP; Package:MACSYMA -*-
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; Comments: Symmetrization module for itensor.lisp
;;
;;; symtry 100 Feb 12, 1982

(in-package :maxima)

;	** (c) Copyright 1979 Massachusetts Institute of Technology **

(declare-top (special symtypes $symmetries $allsym csign smlist $idummyx))

(setq symtypes '($sym $anti $cyc) $symmetries '((mlist simp)))

;$SYMMETRIES is a list of indexed objects with declared symmetries
;$ALLSYM if non-nil means that all indexed objects are assumed symmetric

(defun $decsym (name ncov ncontr covl contrl)               ;DEClare SYMmetries
       (prog (tensor)
	     (cond ((not (eq (ml-typep name) 'symbol))
		    (merror "First argument must be a possible tensor name"))
		   ((not (and (eq (ml-typep ncov) 'fixnum)
			      (eq (ml-typep ncontr) 'fixnum)
			      (signp ge ncov)
			      (signp ge ncontr)))
	            (merror
		     "2nd and 3rd arguments must be non-negative integers"))
		   ((not (and (eq (caar covl) 'mlist)
			      (eq (caar contrl) 'mlist)))
		    (merror "4th and 5th arguments must be lists"))
		   ((and (< ncov 2) (< ncontr 2))
		    (merror "This object can have no symmetry properties"))
		   ((or (and (< ncov 2) (not (null (cdr covl))))
			(and (< ncontr 2) (not (null (cdr contrl)))))
		    (merror
   "Non-null list associated with zero or single index specification")))
             (setq tensor (implode (nconc (exploden name) (ncons 45)
				          (exploden ncov) (ncons 45)
					  (exploden ncontr))))
	     (do ((covl (cdr covl) (cdr covl)) (carl) (arglist) (prop))
		 ((null covl))
		 (cond ((not (member (setq prop (caar (setq carl (car covl))))
				     symtypes :test #'equal))
			(merror "Invalid symmetry operator: ~M" carl))
		       ((and (null (cddr carl)) (eq (cadr carl) '$all))
			(setq arglist (interval 1 ncov)))
		       (t (setq arglist (check-symargs (cdr carl) (1+ ncov)))))
		 (setq carl (zl-get tensor prop))
		 (putprop tensor (cons (cons arglist (car carl)) (cdr carl))
			  prop))
	     (do ((contl (cdr contrl) (cdr contl)) (carl) (arglist) (prop))
		 ((null contl))
		 (cond ((not (member (setq prop (caar (setq carl (car contl))))
				     symtypes :test #'equal))
			(merror "Invalid symmetry operator: ~M" carl))
		       ((and (null (cddr carl)) (eq (cadr carl) '$all))
			(setq arglist (interval 1 ncontr)))
		       ((setq arglist (check-symargs (cdr carl) (1+ ncontr)))))
		 (setq carl (zl-get tensor prop))
		 (putprop tensor (cons (car carl) (cons arglist (cdr carl)))
			  prop))
	     (add2lnc tensor $symmetries)
	     (return '$done)))

;(defun interval (i j)     ;INTERVAL returns the list of integers from I thru J.
;       (do ((n i (1+ n)) (ans))             ;Thus (INTERVAL 3 5) yields (3 4 5)
;           ((> n j) (nreverse ans))
;           (setq ans (cons n ans))))

(defun check-symargs (ll n)            ;Returns an ascending list of the unique
				       ;elements of LL and checks that they are
       (do ((l ll (cdr l)) (c) (ans))  ;integers between 0 and N
	   ((null l) (cond ((null (cdr ans))
			    (merror "Only one distinct index in symmetry property declaration"))
			   (t (sort ans '<))))
	   (setq c (car l))
	   (cond ((not (and (eq (ml-typep c) 'fixnum) (> c 0) (< c n)))
		  (merror "Bad argument encountered for symmetry operator"))
		 ((not (member c ans :test #'equal)) (setq ans (cons c ans))))))

(defun $dispsym (name ncov ncontr)                          ;DISPlay SYMmetries
       (prog (tensor)
             (setq tensor (implode (nconc (exploden name) (ncons 45)
				          (exploden ncov) (ncons 45)
					  (exploden ncontr))))
	     (cond ((not (member tensor (cdr $symmetries) :test #'equal))
		    (return (ncons smlist))))
	     (return
	      (do ((q symtypes (cdr q)) (l) (prop))
		  ((null q) (consmlist l))
		  (cond ((not (null (setq prop (zl-get tensor (car q)))))
			 (setq l
			  (append l
				 (ncons
			          (consmlist
			           (list
				    (car q)
				    (consmlist (mapcar 'consmlist (car prop)))
				    (consmlist (mapcar 'consmlist (cdr prop))))
))))))))))

(defun $remsym (name ncov ncontr)
  (prog (tensor)
    (setq tensor (implode (nconc (exploden name) (ncons 45)
				 (exploden ncov) (ncons 45)
				 (exploden ncontr))))
    (cond ((not (member tensor (cdr $symmetries) :test #'equal))
	   (mtell "~&No symmetries have been declared for this tensor.~%"))
	  (t (setq $symmetries (delete tensor $symmetries :test #'equal))
	   (zl-remprop tensor '$sym)
	   (zl-remprop tensor '$anti)
	   (zl-remprop tensor '$cyc)))
    (return '$done)))

(defun $canform (e)                              ;Convert E into CANonical FORM
       (cond ((atom e) e)
	     ((eq (caar e) 'mequal)
	      (mysubst0 (list (car e) ($canform (cadr e)) ($canform (caddr e)))
			e))
	     ((eq (caar e) 'mplus)
	      (mysubst0 (simplus (cons '(mplus) (mapcar '$canform (cdr e)))
				 1 nil) e))
	     ((eq (caar e) 'mtimes) (mysubst0 (simplifya (canprod e) nil) e))
	     ((rpobj e) (canten e t))
	     (t (mysubst0 (simplifya (cons (ncons (caar e))
					   (mapcar '$canform (cdr e))) t) e))))

(defun canten (e nfprpobjs)		;CANonical TENsor
  (prog (cov contr deriv tensor)
     ((lambda (dummy) (and nfprpobjs dummy (setq e (rename1 e dummy))))
      (nonumber (cdaddr ($indices e)))) ;NFPRPOBJS is Not From Product
     (setq cov (copy-tree (cdadr e))	;of RP (indexed) OBJects
	   contr (copy-tree (cdaddr e))
	   deriv (copy-tree (cdddr e))
	   tensor (implode (nconc (exploden (caar e)) (ncons 45)
				  (exploden (length cov)) (ncons 45)
				  (exploden (length contr))))
	   csign nil)	   ;Set when reordering antisymmetric indices.
					;Indicates whether overall sign of
					;expression needs changing.
     (cond ((or (or (eq (caar e) '$levi_civita) (eq (caar e) '%levi_civita))
		(or (eq (caar e) '$kdelta) (eq (caar e) '%kdelta)))
	    (setq cov (antisort cov) contr (antisort contr)))
	   ((or $allsym (eq (caar e) '$kdels) (eq (caar e) '%kdels))
	    (setq cov (itensor-sort cov) contr (itensor-sort contr)))
	   ((member ($verbify tensor) (cdr $symmetries) :test #'equal)
	    (do ((q symtypes (cdr q)) (type))
		((null q))
	      (setq type (car q))
	      (do ((props (car (zl-get ($verbify tensor) type)) (cdr props)) (p))
		  ((null props))
		(setq p (car props)
		      cov (inserts (symsort (extract-elements p cov) type)
				   cov p)))
	      (do ((props (cdr (zl-get ($verbify tensor) type)) (cdr props)) (p))
		  ((null props))
		(setq p (car props)
		      contr (inserts (symsort (extract-elements p contr)
					      type)
				     contr p))))))
     (setq tensor (mysubst0 (append (list (car e)
					  (consmlist cov)
					  (consmlist contr))
				    (cond ($iframe_flag deriv)
					  (t (itensor-sort deriv) ))
				    ) e))
     (cond (csign (setq tensor (neg tensor))))
     (return tensor)))

(defun rename1 (e dummy)          ;Renames dummy indices in a consistent manner
       (sublis (cleanup0 dummy) e))

(defun cleanup0 (a)
       (do ((b a (cdr b)) (n 1 (1+ n)) (l) (dumx))
	   ((null b) l)
	   (setq dumx (concat $idummyx n))
	   (cond ((not (eq dumx (car b)))
		  (setq l (cons (cons (car b) dumx) l))))))

(defun extract-elements (a b)  ;Extracts the elements from B specified by the indices in
                      ;i.e. (EXTRACT-elements '(2 5) '(A B C D E F)) yields (B E)
       (do ((a a) (b b (cdr b)) (n 1 (1+ n)) (l))
	   ((null a) (nreverse l))
	   (cond ((equal (car a) n)
		  (setq l (cons (car b) l) a (cdr a))))))

(defun inserts (a b c)          ;Substitutes A into B with respect to the index
       (do ((a a) (b b (cdr b)) (c c) (n 1 (1+ n)) (l))        ;specification C
	   ((null a) (nreconc l b))
	   (cond ((equal (car c) n)
		  (setq l (cons (car a) l) a (cdr a) c (cdr c)))
		 (t (setq l (cons (car b) l))))))

(defun symsort (l type)
       (cond ((eq type '$sym) (sort l #'less))           ;SORT SYMmetric indices
	     ((eq type '$anti) (antisort l))
	     (t (cycsort l))))

(defun antisort (l)         ;SORT ANTIsymmetric indices and set CSIGN as needed
       ((lambda (q) (cond ((equal ($levi_civita (consmlist (mapcar 'cdr q))) -1)
		           (setq csign (not csign))))
		    (mapcar 'car q))
	(sort (index l) #'less :key #'car)))

(defun index (l)             ;(INDEX '(A B C)) yields ((A . 1) (B . 2) (C . 3))
       (do ((l l (cdr l)) (n 1 (1+ n)) (q))
	   ((null l) (nreverse q))
	   (setq q (cons (cons (car l) n) q))))

(defun cycsort (l)                                         ;SORT CYClic indices
       ((lambda (n) (cond ((equal n 0) l)
			  (t (append (nthcdr n l)
				     (reverse (nthcdr (f- (length l) n)
						      (reverse l)))))))
	(1- (cdr (least l)))))

(defun least (l)   ;Returns a dotted pair containing the alphanumerically least
                   ;element in L in the car and its index in L in the cdr
       (do ((l (cdr l) (cdr l)) (a (cons (car l) 1)) (n 2 (1+ n)))
	   ((null l) a)
	   (cond ((less (car l) (car a)) (setq a (cons (car l) n))))))

(declare-top (special free-indices))

(defun canprod (e)
  (prog (scalars indexed)
    (cond
      (
        (catch 'foo
          (do
            ((f (cdr e) (cdr f)) (obj))
            (
              (null f)
              (setq scalars (nreverse scalars)
                    indexed (nreverse indexed)
              )
              nil
            )
            (setq obj (car f))
            (cond
              ((atom obj) (setq scalars (cons obj scalars)))
              ((rpobj obj) (setq indexed (cons obj indexed)))
              ((eq (caar obj) 'mplus) (throw 'foo t))
              (t (setq scalars (cons obj scalars)))
            )
          )
        )
        (return ($canform ($expand e)))
      )
      ((null indexed) (return e))
      (
        (null (cdr indexed))
        (return
          (nconc (ncons '(mtimes)) scalars (ncons (canten (car indexed) t)))
        )
      )
      (t
        (return
          (nconc
            (ncons '(mtimes))
            scalars
            (mapcar
              (function (lambda (z) (canten z nil)))
              (
                (lambda (q)
                  (rename1 q
                    (nonumber (cdaddr ($indices2 (cons '(mtimes) (reverse q)))))
                  )
                )
                (mapcar
                  #'cdr
                  (sortcar
                    (progn
                      (setq free-indices (nonumber (cdadr ($indices e))))
                      (mapcar 'describe-tensor indexed)
                    )
                    #'tensorpred
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

(defun tensorpred (x y)
       (do ((x x (cdr x)) (y y (cdr y)) (a) (b))
	   ((null x))
	   (setq a (car x) b (car y))
	   (and (not (equal a b)) (return
				   (cond ((eq (ml-typep a) 'fixnum) (< a b))
					 ((and (listp a) (listp b)) (tensorpred a b))
					 ((null a) t)
					 ((null b) nil)
					 (t (alphalessp a b)))))))

(defun describe-tensor (f)
       (cons (tdescript f) f))

(defun tdescript (f)
       (prog (name indices lcov lcontr lderiv)
	     (setq name (caar f)
		   indices (append (cdadr f) (cdaddr f) (cdddr f))
		   lcov (length (cdadr f))
		   lcontr (length (cdaddr f))
		   lderiv (length (cdddr f)))
	     (return (list (car (least (intersect indices free-indices)))
		           (f+ lcov (f+ lcontr lderiv) )
			   lderiv lcov name))))

(declare-top (unspecial free-indices))


