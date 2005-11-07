;;; -*-  mode: lisp; package: cl-maxima; syntax: common-lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(declare-top (unspecial p y)) 

;;   These functions can be used to keep an alphabetical masterlist in
;;*genvar* and *varlist* and use them.  I think *genpairs* is now
;;redundant second genpairs is much smaller than *genpairs* would be and
;;just keeps the pairs needed for the current form.  *varlist* and
;;*genvar* are still the global ones.


;;(ratsetup varlist genvar) does ratsetup1 and ratsetup2.  Which map the
;;above over varlist but also do things all the way down the list.
;;could do (ratsetup *varlist* *genvar*) if you want to fix them up.  to
;;get latest tellrat info and ratweight level info etc.

;;if new-newvar has been called on x and varlist is *varlist* then
;;new-prep1 should have all the variables it wants in genpairs and so we
;;could use the old prep1.  In fact new-newvar must be called first
;;because the newvarmexpt function which handles exponentiation does not
;;have a new- analogue and so will call (newsym) not (add-newvar)

;;    IDEAS NOT YET IMPLEMENTED:      Change the gensym so that instead
;;of allocating a symbol one uses a number (between 1 and 2^16 say).
;;Instead of using the value cell to record the ordering, this is done
;;in an array : so the function for POINTERGP would look like (> (aref
;;genvar x) (aref genvar y)) the functions VALGET and VALPUT would just
;;need changing to (aref genvar x) etc.

;;   Another idea would be to change PTIMES and PPLUS etc. so that their
;;internal calls to themselves would involve another function say
;;NPTIMES which would take as its arguments and values a reusable type
;;of polynomial like a an array etc.  Then one would only need the
;;functions to change would be the functions which change the
;;NPOLYNOMIALS back to the polynomials and vice versa.

;;    We have implemented a temporary storage area
;;*TEMPORARY-POLYNOMIAL-AREA* which will be the default cons area inside
;;various functions and their recursive subcalls.  We then copy to
;;WORKING-STORAGE-AREA the result of the function.  Meanwhile the
;;process POLYNOMIAL-AREA reclaims the temporary area when *SAFE* and it
;;contains too much.  When *CHANGE-DEFAULT-CONS*  we use the temporary
;;area in functions inside the body of (WITH-POLYNOMIAL-AREA ( form1
;;form2 ..) body) The body is put in a PROGN and its final result as
;;well as FORM1 FORM2..  copied into WORKING-STORAGE-AREA.  Then *safe*
;;is reset to true.  We have also arranged so the debugger rebinds
;;default-cons-area so that we won't create things in the wrong area
;;while inside it.

;;the following are faster than the previous ones in the ratmac

;(defvar *safe* nil)


(defun safe-putprop ( sym value indicator &aux #+lispm(working-storage-area default-cons-area))
  (putprop sym value indicator))
;;(defun POINTERGP (A B) (> (VALGET A) (VALGET B)))
;;as a subst it is faster any problems 'wfs
;#+lispm
;(defsubst POINTERGP (A B) (> (VALGET A) (VALGET B)))

(DEFUN NEW-PREP1 (X &AUX TEMP) 
       (COND ((FLOATP X)
	      (COND ($KEEPFLOAT (CONS X 1.0)) ((PREPFLOAT X))))
	     ((integerp x) (cons (cmod x) 1))
     	     ((typep x 'rational)
	      (cond ((null modulus)(cons
				    (numerator x) (denominator x)))
		    (t (cquotient (numerator x) (denominator x)))))

	     ((ATOM X)(COND ((ASSOLIKE X GENPAIRS))
			    (T(FORMAT T "***IN NEW-PREP1**")
					      (ADD-NEWVAR-TO-GENPAIRS X ))))
	     ((AND $RATFAC (ASSOLIKE X GENPAIRS)))
	     ((EQ (CAAR X) 'MPLUS)
	      (COND ($RATFAC
		     (SETQ X (MAPCAR 'NEW-PREP1 (CDR X)))
		     (COND ((ANDMAPC 'FRPOLY? X)
			    (CONS (MFACPPLUS (MAPL #'(lambda (X)
						      (RPLACA X (CAAR X)))
						  X)) 
				  1))
			   (T (DO ((A (CAR X) (FACRPLUS A (CAR L)))
				   (L (CDR X) (CDR L)))
				  ((NULL L) A)))))
		    (T (DO ((A (NEW-PREP1 (CADR X)) (RATPLUS A (NEW-PREP1 (CAR L))))
			    (L (CDDR X) (CDR L)))
			   ((NULL L) A)))))
	     ((EQ (CAAR X) 'MTIMES)
	      (DO ((A (SAVEFACTORS (NEW-PREP1 (CADR X)))
		      (RATTIMES A (SAVEFACTORS (NEW-PREP1 (CAR L))) SW))
		   (L (CDDR X) (CDR L))
		   (SW (NOT (AND $NOREPEAT (MEMQ 'RATSIMP (CDAR X))))))
		  ((NULL L) A)))
	     ((EQ (CAAR X) 'MEXPT)
	      (NEWVARMEXPT X (CADDR X) T))
	     ((EQ (CAAR X) 'MQUOTIENT)
	      (RATQUOTIENT (SAVEFACTORS (NEW-PREP1 (CADR X)))
			   (SAVEFACTORS (NEW-PREP1 (CADDR X)))))
	     ((EQ (CAAR X) 'MMINUS)
	      (RATMINUS (NEW-PREP1 (CADR X))))
	     ((EQ (CAAR X) 'RAT)
	      (COND (MODULUS (CONS (CQUOTIENT (CMOD (CADR X)) (CMOD (CADDR X))) 1))
		    (T (CONS (CADR X) (CADDR X)))))
	     ((EQ (CAAR X) 'BIGFLOAT)(BIGFLOAT2RAT X))
	     ((EQ (CAAR X) 'MRAT)
	      (COND ((AND *WITHINRATF* (MEMQ 'TRUNC (CAR X)))
		     (THROW 'RATF NIL))
		    ((CATCH 'COMPATVL
		       (PROGN (SETQ TEMP (COMPATVARL (CADDAR X)
						     VARLIST
						     (CADDDR (CAR X))
						     GENVAR))
			      T))
		     (COND ((MEMQ 'TRUNC (CAR X))
			    (CDR ($TAYTORAT X)))
			   ((AND (NOT $KEEPFLOAT)
				 (OR (PFLOATP (CADR X)) (PFLOATP (CDDR X))))
			    (CDR (RATREP* ($RATDISREP X))))
			   ((SUBLIS TEMP (CDR X)))))
		    (T (CDR (RATREP* ($RATDISREP X))))))
	     ((ASSOLIKE X GENPAIRS))
	     (T (SETQ X (LITTLEFR1 X))
		(COND ((ASSOLIKE X GENPAIRS))
		      (T (format t "%%in new-prep1")
			 (add-newvar-to-genpairs  X))))))

;;because symbolics will assign a common lisp print name only when the symbol is referred to
(defun safe-string (symb)
  (let (#+lispm (default-cons-area working-storage-area))
    (string symb)))
#+symbolics
(defun check-pnames ( &aux *exceptions*)
  (declare (special *exceptions*))
  (mapatoms #'(lambda (at &aux tem name)
		(declare (special *exceptions*))
		(cond (
		       (setq tem (get at 'scl::print-name))
		       (cond ((or (not (stringp tem))
				  (eql (%area-number tem) *temporary-polynomial-area*)
				  (not (eql (string-length tem)
					    (array-total-size
					     (setq name
						   (symbol-name at)))))
				  (sloop for i from 0 below (string-length tem)
					when (not (eql (aref tem i)
						       (int-char (aref name i))))
					do (return t)))
			      (push (list at tem) *exceptions*))))))
	    'maxima nil)
  *exceptions*)
#+symbolics
(defun fix-pnames (lis)
  (sloop for v in lis do (setf (get (car v) 'scl::print-name) (format nil "~A" (car v)))))



(Defun New-RATF (L &aux  genpairs)
    (PROG (U *WITHINRATF*)
	  (SETQ *WITHINRATF* T)
	  (WHEN (EQ '%% (CATCH 'RATF (new-NEWVAR L))) ;;get the new variables onto *varlist*
	    (SETQ *WITHINRATF* NIL) (RETURN (SRF L)))	;new-prep1 should not have to add any.
  (let ((varlist *varlist*)(genvar *genvar*))

	  (SETQ U (CATCH 'RATF (new-RATREP* L)))	; for truncation routines
	  (RETURN (OR U (PROG2 (SETQ *WITHINRATF* NIL) (SRF L)))))))



(DEFUN new-NEWVAR (L  )
;  (let (( vlist varlist))
  (my-NEWVAR1 L))
;  (setq varlist (sortgreat vlist))
 ; vlist))
 ; (SETQ VARLIST (NCONC (SORTGREAT VLIST) VARLIST)))


(defun NEW-RATREP* (x)
  ;;the ratsetup is done in my-newvar1
    (xcons (new-prep1 x)
	   (list* 'mrat 'simp *varlist* *genvar*
		  		  (IF (AND (NOT (ATOM X)) (MEMQ 'IRREDUCIBLE (CDAR X)))
		      '(IRREDUCIBLE)))))
	  
(defun new-rat (x &aux genpairs)
  (cond
    ((polynomialp x) (cons x 1))
    ((rational-functionp x) x)
    ((and (listp x) (eq (caar x) 'mrat))
	 (cond ((memq (car (num (cdr x))) *genvar*)
		(cdr x))
	       (t (format t "~%disrepping")(new-rat  ($totaldisrep x)))))
	(t 
	     
  (prog (u *withinratf*)
	(setq *withinratf* t)
	(cond ((mbagp x)(return (cons (car x) (mapcar 'new-rat (cdr x)))))
	      (t 
	(when (eq '%% (catch 'ratf (new-newvar x)))
	  (setq *withinratf* nil)(return (srf x)))
	(let ((varlist *varlist*)(genvar *genvar*))
	  (setq u (catch 'ratf (new-prep1 x)))  ;;truncations
	  (return (or u (prog2 (setq *withinratf* nil) (srf x)))))))))))

;(DEFUN my-NEWVAR1 (X &aux caarx)
;       (COND ((NUMBERP X) NIL)
;	     ((assolike x genpairs) nil)
;	     ((ATOM X) (add-newvar-to-genpairs X )nil)
;	     ((MEMQ (setq caarx (CAAR X))
;		    '(MPLUS MTIMES RAT MDIFFERENCE
;			    MQUOTIENT MMINUS BIGFLOAT mlist))
;	      (MAPC (FUNCTION my-NEWVAR1) (CDR X)))
;	     (t (case caarx
;		  (mexpt (my-newvar1 (second x)))
;		  (mnctimes (add-newvar-to-genpairs x))
;		  (mrat (ferror "how did you get here"))
;		  (otherwise (ferror "What is x like"))))))

(DEFUN my-NEWVAR1 (X)
       (COND ((NUMBERP X) NIL)
	     ((assolike x genpairs) nil)
	    ;;; ((MEMALIKE X VARLIST))we 're using *varlist*
;	;     ((MEMALIKE X VLIST) NIL)
	     ((ATOM X) (add-newvar-to-genpairs X )nil)
	     ((MEMQ (CAAR X)
		    '(MPLUS MTIMES RAT MDIFFERENCE
			    MQUOTIENT MMINUS BIGFLOAT))
	      (MAPC (FUNCTION my-NEWVAR1) (CDR X)))
	     
	     ((EQ (CAAR X) 'MEXPT)
	       (my-newvar1 (second  x) ))
	     ;; ;(NEWVARMEXPT X (CADDR X) NIL))
	     ((EQ (CAAR X) 'MRAT)(ferror " how did you get here Bill?")
	      (AND *WITHINRATF* (MEMQ 'TRUNC (CDDDAR X)) (THROW 'RATF '%%))
	      (COND ($RATFAC (MAPC 'NEWVAR3 (CADDAR X)))
		    (T (MAPC (FUNCTION my-NEWVAR1) (REVERSE (CADDAR X))))))
	     ((eq (caar x) 'mnctimes)(add-newvar-to-genpairs x ))
	     (T (ferror "What is x like ? ~A" x))))

;;need this?
;	      (COND (*FNEWVARSW (SETQ X (LITTLEFR1 X))
;				  (MAPC (FUNCTION NEWVAR1)
;					(CDR X))
;				  (OR (MEMALIKE X VLIST)
;				      (MEMALIKE X VARLIST)
;;				      (PUTONVLIST X)))
;;		      (T (PUTONVLIST X))))))

(defun add-newvar-to-genpairs (va &aux the-gensym)
  (cond ((assolike va nil) genpairs)
	(t (setq the-gensym (add-newvar va))
	   (push (cons va (rget the-gensym)) genpairs)
	   (rat-setup1 va the-gensym)(rat-setup2 va the-gensym)))
  nil)


;;might be worthwhile to keep a resource or list of gensyms so that when
;;you reset-vgp then you don't just discard them you reuse them via the gensym call

(defmacro with-working-storage-area(&body body)
  `(let  (#+lispm(default-cons-area working-storage-area))
    ,@ body))
 
(defvar *genvar-resemble* t)


(defun add-newvar ( va &optional (use-*genpairs* t)&aux  the-gensym)
  "If va is not in varlist ADD-NEWVAR splices va into the varlist and a new gensym
into genvar ordering and adds to genpairs"
 (declare (special $order_function))
   use-*genpairs*  ;;don't use it
  (cond ((and (symbolp va) (not (eql (aref  (safe-string va) 0) #\$))) (ferror "doesn't begin with $")))
  (let (#+lispm(default-cons-area working-storage-area))
   (multiple-value-bind (after there)
       (find-in-ordered-list va *varlist* $order_function)
     (cond ((not there)
	    (setq the-gensym (gensym-readable va))
;	    (cond ((and (symbolp va) *genvar-resemble*)
;                   (setq the-gensym (make-symbol (string-trim "$" (safe-string va)))))
;		  (t
;		   (setq the-gensym (gensym))))
	    #+lispm
	    (setq va (new-copy-from-temporary-area va))
	    (safe-putprop the-gensym va 'disrep)
;	    (cond (use-*genpairs* (push (cons va (rget the-gensym)) *genpairs*)))
;	    (rat-setup1 va the-gensym)(rat-setup2 va the-gensym)
	    (with-working-storage-area
	      (setq *genvar* (nsplice-in after the-gensym *genvar*))
	      (setq *varlist* (nsplice-in after va  *varlist*)))
    	    (cond ( *check-order*
;		   (check-repeats *varlist*)
		   (check-order *varlist*)))
	    (sloop for v in (nthcdr  (max 0 after) *genvar*)
		  for i from  (f1+  after)
		  do (set v i)))
	   (there (setq the-gensym (nth after *genvar*))
		  (cond ((not (nc-equal (get the-gensym 'disrep) va))
			 (fsignal "bad-correspondence" )))))
  (values the-gensym (not there)))))




(DEFUN RAT-SETUP1 (V G)
  (AND $RATWTLVL
	        (SETQ V (ASSOLIKE V *RATWEIGHTS))
	        (IF V (SAFE-PUTPROP G V '$RATWEIGHT) (REMPROP G '$RATWEIGHT))))



(DEFUN RAT-SETUP2 (V G)
  (WHEN $ALGEBRAIC
		(COND ((SETQ V (ALGPGET  V))
		       (let (#+lispm(default-cons-area working-storage-area))
		       (SAFE-PUTPROP  G  V 'TELLRAT)))
		      (T (REMPROP  G 'TELLRAT)))))



(defun te (f g)
    (let* ((genvar   (nreverse (sort (union1 (listovars f) (listovars g))
		       #'pointergp)))
	   (varlist (sloop for v in genvar collecting (get v 'disrep) )))
      (break t)
     (ratreduce  f g)))

;;

(defun new-pfactor (poly)
  "returns an alternating list: factor1 expt1 factor2 expt2 ..."
  (let ((genvar (nreverse (sort (listovars poly) #'pointergp))))
    (pfactor poly)))

(defun multiply-factors-with-multiplicity (a-list &aux ( answer 1))
  (sloop for v in a-list by 'cddr
	for w in (cdr a-list) by 'cddr
	do (sloop while (> w 0)
		 do (setq answer (n* answer v))
		 (setq w (f1- w))))
  answer)

(defun copy-vgp ()
  (setq *varlist* (copy-list *varlist*))
  (setq *genvar* (copy-list *genvar*)) nil)


(defun q-var (f)(cond ((atom f) nil)
		      (t (aref f 0))))

(defun ar-last (aray)
  (aref aray (f1- (length (the cl:array aray)))))
(defun ar-second-last (aray)
  (aref aray (f- (length (the cl:array aray)) 2)))

(defun set-fill-pointer (aray n)(setf (fill-pointer aray ) n) aray)
(defun constant-term-in-main-variable (f)
     (cond ((czerop (ar-second-last f))
	    (ar-last f))
	   (t 0)))

#+debug
(progn
(DEFMFUN PPLUS (X Y)
  (COND ((PCOEFP X) (PCPLUS X Y))
	((PCOEFP Y) (PCPLUS Y X))
	((EQ (P-VAR X) (P-VAR Y))
	 (PSIMP (P-VAR X) (PPLUS1 (P-TERMS Y) (P-TERMS X))))
	((POINTERGP (P-VAR X) (P-VAR Y))
	 (PSIMP (P-VAR X) (PCPLUS1 Y (P-TERMS X))))
	(T (PSIMP (P-VAR Y) (PCPLUS1 X (P-TERMS Y))))))

(DEFMFUN PTIMES (X Y)
  (COND ((PCOEFP X) (IF (PZEROP X) 0 (PCTIMES X Y)))
	((PCOEFP Y) (IF (PZEROP Y) 0 (PCTIMES Y X)))
	((EQ (P-VAR X) (P-VAR Y))
	 (PALGSIMP (P-VAR X) (PTIMES1 (P-TERMS X) (P-TERMS Y)) (ALG X)))
	((POINTERGP (P-VAR X) (P-VAR Y))
	 (PSIMP (P-VAR X) (PCTIMES1 Y (P-TERMS X))))
	(T (PSIMP (P-VAR Y) (PCTIMES1 X (P-TERMS Y))))))
(DEFUN PTIMES (X Y)
    (COND ((ATOM X)
           (COND ((AND (NUMBERP X)
                       (ZEROP X))
                  0)
                 (T (PCTIMES X Y))))
          ((ATOM Y)
           (COND ((AND (NUMBERP Y)
                       (ZEROP Y))
                  0)
                 (T (PCTIMES Y X))))
          ((EQ (CAR X) (CAR Y))
           (PALGSIMP (CAR X) (PTIMES1 (CDR X) (CDR Y)) (ALG X)))
          ((> (SYMBOL-VALUE (CAR X)) (SYMBOL-VALUE (CAR Y)))
           (PSIMP (CAR X) (PCTIMES1 Y (CDR X))))
          (T (PSIMP (CAR Y) (PCTIMES1 X (CDR Y))))))

(DEFMFUN PDIFFERENCE (X Y)
  (COND ((PCOEFP X) (PCDIFFER X Y))
	((PCOEFP Y) (PCPLUS (CMINUS Y) X))
	((EQ (P-VAR X) (P-VAR Y))
	 (PSIMP (P-VAR X) (PDIFFER1 (P-TERMS X) (P-TERMS Y))))
	((POINTERGP (P-VAR X) (P-VAR Y))
	 (PSIMP (P-VAR X) (PCDIFFER2 (P-TERMS X) Y)))
	(T (PSIMP (P-VAR Y) (PCDIFFER1 X (P-TERMS Y))))))


(DEFUN PFACTOR (P &aux ($ALGEBRAIC ALGFAC*))
       (COND ((PCOEFP P) (CFACTOR P))
	     ($RATFAC (PFACPROD P))
	     (T (SETQ P (FACTOROUT P))
		(COND ((EQUAL (CADR P) 1) (CAR P))
		      ((NUMBERP (CADR P)) (APPEND (CFACTOR (CADR P)) (CAR P)))
		      (T ((LAMBDA (CONT)
		            (NCONC
			     (COND ((EQUAL (CAR CONT) 1) NIL)
				   (ALGFAC*
				    (COND (MODULUS (LIST (CAR CONT) 1))
					  ((EQUAL (CAR CONT) '(1 . 1)) NIL)
					  ((EQUAL (CDAR CONT) 1)
					   (LIST (CAAR CONT) 1))
					  (T (LIST (CAAR CONT) 1 (CDAR CONT) -1))))
				   (T (CFACTOR (CAR CONT))))
			     (PFACTOR11 (PSQFR (CADR CONT)))
			     (CAR P)))
			  (COND (MODULUS (LIST (LEADALGCOEF (CADR P))
					       (MONIZE (CADR P))))
				(ALGFAC* (ALGCONTENT (CADR P)))

				(T (PCONTENT (CADR P))))))))))


(DEFUN FULLRATSIMP (L)
 (LET (($EXPOP 0) ($EXPON 0) (INRATSIMP T) $RATSIMPEXPONS)
      (SETQ L ($TOTALDISREP L)) (FR1 L VARLIST)))    
)

#+lispm
(progn
(defun pplus (x y)
  (with-polynomial-area ()
      (COND ((PCOEFP X) (PCPLUS X Y))
	((PCOEFP Y) (PCPLUS Y X))
	((EQ (P-VAR X) (P-VAR Y))
	 (PSIMP (P-VAR X) (PPLUS1 (P-TERMS Y) (P-TERMS X))))
	((POINTERGP (P-VAR X) (P-VAR Y))
	 (PSIMP (P-VAR X) (PCPLUS1 Y (P-TERMS X))))
	(T (PSIMP (P-VAR Y) (PCPLUS1 X (P-TERMS Y)))))))

(defun ptimes (x y)
  (with-polynomial-area ()
    (COND ((PCOEFP X) (IF (PZEROP X) (PZERO) (PCTIMES X Y)))
	       ((PCOEFP Y) (IF (PZEROP Y) (PZERO) (PCTIMES Y X)))
	       ((EQ (P-VAR X) (P-VAR Y))
		(PALGSIMP (P-VAR X) (PTIMES1 (P-TERMS X) (P-TERMS Y)) (ALG X)))
	       ((POINTERGP (P-VAR X) (P-VAR Y))
		(PSIMP (P-VAR X) (PCTIMES1 Y (P-TERMS X))))
	       (T (PSIMP (P-VAR Y) (PCTIMES1 X (P-TERMS Y)))))))


(DEFMFUN PDIFFERENCE (X Y)
  (with-polynomial-area ()
  (COND ((PCOEFP X) (PCDIFFER X Y))
	((PCOEFP Y) (PCPLUS (CMINUS Y) X))
	((EQ (P-VAR X) (P-VAR Y))
	 (PSIMP (P-VAR X) (PDIFFER1 (P-TERMS X) (P-TERMS Y))))
	((POINTERGP (P-VAR X) (P-VAR Y))
	 (PSIMP (P-VAR X) (PCDIFFER2 (P-TERMS X) Y)))
	(T (PSIMP (P-VAR Y) (PCDIFFER1 X (P-TERMS Y)))))))


(DEFUN PFACTOR (P &aux ($ALGEBRAIC ALGFAC*))
      (with-polynomial-area () (COND ((PCOEFP P) (CFACTOR P))
	     ($RATFAC (PFACPROD P))
	     (T (SETQ P (FACTOROUT P))
		(COND ((EQUAL (CADR P) 1) (CAR P))
		      ((NUMBERP (CADR P)) (APPEND (CFACTOR (CADR P)) (CAR P)))
		      (T ((LAMBDA (CONT)
		            (NCONC
			     (COND ((EQUAL (CAR CONT) 1) NIL)
				   (ALGFAC*
				    (COND (MODULUS (LIST (CAR CONT) 1))
					  ((EQUAL (CAR CONT) '(1 . 1)) NIL)
					  ((EQUAL (CDAR CONT) 1)
					   (LIST (CAAR CONT) 1))
					  (T (LIST (CAAR CONT) 1 (CDAR CONT) -1))))
				   (T (CFACTOR (CAR CONT))))
			     (PFACTOR11 (PSQFR (CADR CONT)))
			     (CAR P)))
			  (COND (MODULUS (LIST (LEADALGCOEF (CADR P))
					       (MONIZE (CADR P))))
				(ALGFAC* (ALGCONTENT (CADR P)))
				(T (PCONTENT (CADR P)))))))))))

;;timings of factoring (x+y+z)^10
;;on explorer feb 2 microcode ?? 6.6sec
;;on 3600     feb 10 rel60d microcode 313 5.1 sec maxima 2.6
    ;;;(tried loading all the new compiled files) and it was 4.6sec
;;on 3600     feb 10 microcode 315 4.45 seconds 

;;timings of factoring (x+y+z)^20
;;3600 rel6 betaII mic.315 20.7 seconds


(DEFMFUN $FACTOR (&rest args)
  (check-arg args (and (listp args)(memq (length args) '( 1 2))) "one or two args")
  (with-polynomial-area-new ()
    :reset
  (LET ($INTFACLIM (VARLIST (CDR $RATVARS)) GENVAR ANS)
    (SETQ ANS (APPLY #'FACTOR args))
    (IF (AND FACTORRESIMP $NEGDISTRIB
	     (MTIMESP ANS) (NULL (CDDDR ANS))
	     (EQUAL (CADR ANS) -1) (MPLUSP (CADDR ANS)))
	(LET (($EXPOP 0) ($EXPON 0)) ($MULTTHRU ANS))
	ANS))) )


(DEFMFUN RATREDUCE (X Y &AUX B)
  (with-polynomial-area ()
  (COND ((PZEROP Y) (ERRRJF "QUOTIENT by ZERO"))
	((PZEROP X) (RZERO))
	((EQN Y 1) (CONS X 1))
	((AND $KEEPFLOAT (PCOEFP Y) (OR $FLOAT (FLOATP Y) (PFLOATP X)))
	 (CONS (PCTIMES (QUOTIENT 1.0 Y) X) 1))
	(T (SETQ B (PGCDCOFACTS X Y))
	   (SETQ B (RATALGDENOM (RPLACD (CDR B) (CADDR B))))
	   (COND ((AND MODULUS (PCOEFP (CDR B)))
		  (CONS (PCTIMES (CRECIP (CDR B)) (CAR B)) 1))
		 ((PMINUSP (CDR B))
		  (CONS (PMINUS (CAR B)) (PMINUS (CDR B))))
		 (T B))))))


(DEFMFUN RATQUOTIENT (X Y)
  (with-polynomial-area ()
    (RATTIMES X (RATINVERT Y) T)) )



(DEFUN FULLRATSIMP (L)
  (with-polynomial-area ()
    (LET (($EXPOP 0) ($EXPON 0) (INRATSIMP T) $RATSIMPEXPONS)
	     (SETQ L ($TOTALDISREP L)) (FR1 L VARLIST))))

(DEFUN FULLRATSIMP (L)
  (with-polynomial-area ()
    (LET (($EXPOP 0) ($EXPON 0) (INRATSIMP T) $RATSIMPEXPONS)
      (cond ((mbagp l)(cons (car l) (mapcar  #'fullratsimp (cdr l))))
	    (t 
	     (SETQ L ($TOTALDISREP L)) (FR1 L VARLIST))) )))


(DEFMFUN $TRIGREDUCE N
 (LET ((*TRIGRED T) (*NOEXPAND T) VAR $TRIGEXPAND $VERBOSE $RATPRINT)
 (declare (special *trigred *noexpand $trigexpand $verbose $ratprint ans var $verbose))   
      (COND ((= N 2) (SETQ VAR (ARG 2)))
	    ((= N 1) (SETQ VAR '*NOVAR))
	    (T (merror "Wrong number of args to TRIGREDUCE")))
     (with-polynomial-area ()
      (GCDRED (SP1 (ARG 1))))))

)


;;the following works but is slow see projective
(DEFMFUN $GCDlist (&rest fns)
  (cond ((and (eq (length fns) 1)
	      ($listp (car fns))(setq fns (cdr (car fns))))))
  (let  ( VARLIST  gcd-denom gcd-num rat-fns )
	(cond ((eq (length fns) 1) (car fns))
	      (t
	       (sloop for v in fns
		     do (newvar v))
	  (with-polynomial-area ()
	      (setq rat-fns (sloop for v in fns
		     collecting (cdr (ratrep* v))))
	      (setq gcd-num (num (car rat-fns)))
	      (sloop for w in (cdr rat-fns)
		    do
		    (setq gcd-num (pgcd gcd-num (num  w))))
	      (setq gcd-denom (denom (car rat-fns)))
	      (sloop for w in (cdr rat-fns)
		    do (setq gcd-denom (pgcd gcd-denom (denom w))))
	      (ratdisrep (cons (list 'mrat 'simp varlist genvar)
			       (cons gcd-num gcd-denom))))))))


;;;;the following works but seems slower than factoring
;(defun $projective ( vector)
;  (check-arg vector '$listp nil)
;  (let  ( VARLIST  (fns (cdr vector))
;			answer gcd-num factor lcm-denom  rat-fns )
;	       (sloop for v in fns
;		     do (newvar v))
;	  (with-polynomial-area ()
;	      (setq rat-fns (sloop for v in fns
;		     collecting (cdr (ratrep* v))))
;	      (setq gcd-num (num (car rat-fns)))
;	      (sloop for w in (cdr rat-fns)
;		    do
;		    (setq gcd-num (pgcd gcd-num (num  w))))
;	      (setq lcm-denom (denom (car rat-fns)))
;	      (sloop for w in (cdr rat-fns)
;		    do (setq lcm-denom (plcm lcm-denom (denom w))))
;	      (setq factor (cons lcm-denom gcd-num))
;	      (setq answer (sloop for v in rat-fns
;		    collecting (rattimes v factor t)))
;	      (setq header (list 'mrat 'simp varlist genvar))
;	      (sloop for v in answer
;		    collecting (ratdisrep (cons header v)) into tem
;		    finally (return (cons '(mlist) tem))))))


(defun factoredp (poly)
  (cond ((atom poly) t)
	(t (memq  'factored (car poly)))))
(defun exponent (expr prod)
  (cond ((atom prod) 0)
	((eq (caar prod) 'mexpt)(cond ((eq (second prod) expr)(third prod))
				      (t 0)))
	(t(check-arg prod '$productp nil)
	 (sloop for v in (cdr prod)
                 do

		 (cond 
		       ((equal expr v) (return 1))
		       ((numberp v))
		       ((atom v))
		       ((and (equal (caar v) 'mexpt)
			     (equal (second v) expr))
			     (return (third v))))
		 finally (return 0)))))



 
		 
(defun $projective (vector &aux factors first-one
		    factored-vector expon lcm-denom tem fac where proj)
 (with-polynomial-area ()  
  (setq factored-vector (sloop for v in (cdr vector)
			      when (factoredp v) collecting v
			      else collecting ($factor v)))
  (sloop for v in factored-vector
	for i from 0
	when (not ($zerop v))
	do (setq first-one v)(setq where i) (return 'done))
  (cond ((null where) 'Image_not_in_Projective_space)
	(t
	 (setq factored-vector (zl-DELETE first-one factored-vector 1))
	 (setq proj (sloop for w in  factored-vector collecting (div* w first-one)))
	 (sloop for term in proj
	       when (not (numberp term) )
	       do
	       (cond ((atom term)(setq fac term))
		     (t
		      (sloop for v in (cdr term) do
			    (cond ((atom v)(setq fac v))
				  ((eq (caar v) 'mexpt)(setq fac (second v)))
				  ((eq (caar v) 'mplus )(setq fac v)))
			    (cond ((not (zl-MEMBER fac factors))(push fac factors)))))))
	 (sloop for w in factors
	       do (setq expon 0)
	       (setq expon (sloop for v in proj
				 
				 when (< (setq tem (exponent w v)) 0)
				 
				 minimize tem))
	       (cond ((not (eql expon 0))
		      (push  `((mexpt simp) ,w ,expon) lcm-denom))))
	 (cond (lcm-denom (push '(mtimes simp) lcm-denom))
	       (t (setq lcm-denom 1)))
	 
	 
	 (sloop for v in proj
	       collecting (div* v lcm-denom) into tem
	       finally (return
			 
			 (cons '(mlist)  (nsplice-in (f1- where)
						     (div* 1 lcm-denom) tem))))))))
(defun $zeta3_ratsimp (expr &aux answer)
  (setq answer (new-rat expr))
  (setq answer (rationalize-denom-zeta3 answer))
  (new-disrep answer))

(defun rationalize-denom-zeta3 (expr &aux the-denom the-num the-gen)
  (setq the-gen (add-newvar '$%zeta3))
  (cond ((polynomialp expr) expr)
	((variable-in-polyp (denom expr) the-gen)
	 (setq the-denom  (denom expr))
	 (setq the-num (num expr))
	 (setq the-denom (conj-zeta3 the-denom the-gen))
	 (ratreduce  (ptimes the-num the-denom) (ptimes the-denom (denom expr))))
	(t expr)))

(defun conj-zeta3 (expr the-gen &aux answer)
  (cond ((atom expr) expr)
	((eq (car expr) the-gen)
	 (setq expr (copy-list expr))
	 (setf (second expr) 2)
	 (palgsimp the-gen  (cdr expr) (alg expr)))
	(t (setq answer (copy-list expr))
	   (do ((r (cddr answer)  (cddr r)))
	       ((not (consp r)) answer)
	     (rplaca r (conj-zeta3 (car r) the-gen))))))

(defun variable-in-polyp (poly gen)
  (catch 'its-in
    (variable-in-polyp1 poly gen)))
(defun variable-in-polyp1 (poly gen)
  (cond ((atom poly) nil)
	((eq (car poly) gen) (throw 'its-in t))
	(t
	 (do ((r (cddr poly) (cddr r)))
	     ((not (consp  r)) nil)
	   (variable-in-polyp1 (car r) gen)))))

(defun $zeta3_factor (poly)
  (with-polynomial-area (*genvar*)
    ($factor poly `((MPLUS) ((MEXPT) $%ZETA3 2) $%ZETA3 1)))) ; %zeta3^2+%zeta3+1

(DEFUN new-NEWVARMEXPT (X E FLAG) 
  (declare (special RADLIST EXPSUMSPLIT VLIST))
       ;; WHEN FLAG IS T, CALL RETURNS RATFORM
       (PROG (TOPEXP) 
	     (COND ((AND (FIXP E) (NOT FLAG))
		    (RETURN (NEWVAR1 (CADR X))))

		   ;; THIS MAKES PROBLEMS FOR RISCH ((AND(NOT(FIXP
		   ;;E))(MEMQ 'RATSIMP (CDAR X))) (RETURN(SETQ VLIST
		   ;;(CONS X VLIST))))
		   )
	     (SETQ TOPEXP 1)
	TOP  (COND

	      ;; X=B^N FOR N A NUMBER
	      ((FIXP E)
	       (SETQ TOPEXP (TIMES TOPEXP E))
	       (SETQ X (CADR X)))
	      ((ATOM E) NIL)

	      ;; X=B^(P/Q) FOR P AND Q INTEGERS
	      ((EQ (CAAR E) 'RAT)
	       (COND ((OR (MINUSP (CADR E)) (GREATERP (CADR E) 1))
		      (SETQ TOPEXP (TIMES TOPEXP (CADR E)))
		      (SETQ X (LIST '(MEXPT)
				    (CADR X)
				    (LIST '(RAT) 1 (CADDR E))))))
	       (COND ((OR FLAG (NUMBERP (CADR X)) ))
		     (*RATSIMP*
		      (COND ((MEMALIKE X RADLIST) (RETURN NIL))
			    (T (SETQ RADLIST (CONS X RADLIST))
			       (RETURN (NEWVAR1 (CADR X))))) )
		     ($ALGEBRAIC (NEWVAR1 (CADR X)))))
	      ;; X=B^(A*C)
	      ((EQ (CAAR E) 'MTIMES)
	       (COND
		((OR 

		     ;; X=B^(N *C)
		     (AND (ATOM (CADR E))
			  (FIXP (CADR E))
			  (SETQ TOPEXP (TIMES TOPEXP (CADR E)))
			  (SETQ E (CDDR E)))

		     ;; X=B^(P/Q *C)
		     (AND (NOT (ATOM (CADR E)))
			  (EQ (CAAADR E) 'RAT)
			  (NOT (EQUAL 1 (CADADR E)))
			  (SETQ TOPEXP (TIMES TOPEXP (CADADR E)))
			  (SETQ E (CONS (LIST '(RAT)
					      1
					      (CADDR (CADR E)))
					(CDDR E)))))
		 (SETQ X
		       (LIST '(MEXPT)
			     (CADR X)
			     (SETQ E (SIMPLIFY (CONS '(MTIMES)
						      E)))))
		 (GO TOP))))

	      ;; X=B^(A+C)
	      ((AND (EQ (CAAR E) 'MPLUS) EXPSUMSPLIT)	;SWITCH CONTROLS
	       (SETQ					;SPLITTING EXPONENT
		X					;SUMS
		(CONS
		 '(MTIMES)
		 (MAPCAR 
		  (FUNCTION (LAMBDA (LL) 
				    (LIST '(MEXPT)
					  (CADR X)
					  (SIMPLIFY (LIST '(MTIMES)
							   TOPEXP
							   LL)))))
		  (CDR E))))
	       (COND (FLAG (RETURN (NEW-PREP1 X)))
		     (T (RETURN (NEWVAR1 X))))))
	     (COND (FLAG NIL)
		   ((EQUAL 1 TOPEXP)
		    (COND ((OR (ATOM X)
			       (NOT (EQ (CAAR X) 'MEXPT)))
			   (NEWVAR1 X))
			  ((OR (MEMALIKE X VARLIST) (MEMALIKE X VLIST))
			   NIL)
			  (T (COND ((OR (ATOM X) (NULL *FNEWVARSW))
				    (PUTONVLIST X))
				   (T (SETQ X (LITTLEFR1 X))
				      (MAPC (FUNCTION NEWVAR1)
					    (CDR X))
				     (OR (MEMALIKE X VLIST)
					 (MEMALIKE X VARLIST)
					 (PUTONVLIST X)))))))
		   (T (NEWVAR1 X)))
	     (RETURN
	      (COND
	       ((NULL FLAG) NIL)
	       ((EQUAL 1 TOPEXP)
		(COND
		 ((AND (NOT (ATOM X)) (EQ (CAAR X) 'MEXPT))
		  (COND ((ASSOLIKE X GENPAIRS))
; *** SHOULD ONLY GET HERE IF CALLED FROM FR1. *FNEWVARSW=NIL
			(T (SETQ X (LITTLEFR1 X))
			 (COND ((ASSOLIKE X GENPAIRS))
			       (T (new-NEWSYM X))))))
		 (T (NEW-PREP1 X))))
	       (T (RATEXPT (NEW-PREP1 X) TOPEXP))))))


(DEFUN new-NEWSYM (E &aux #+Lispm(working-storage-area default-cons-area))
  (PROG (G P)
	(COND ((SETQ G (ASSOLIKE E GENPAIRS))
	       (RETURN G)))
	#-lispm
	(SETQ G (GENSYM))
	#+lispm
	(SETQ G (gensym-readable e))
	(PUTPROP G E 'DISREP)
	(add-newvar e)
;	(PUSH E VARLIST)
;	(PUSH (CONS E (RGET G)) GENPAIRS)
;	(VALPUT G (IF GENVAR (F1- (VALGET (CAR GENVAR))) 1))
;	(PUSH G GENVAR)
	(COND ((SETQ P (AND $ALGEBRAIC (ALGPGET E)))
;	       (ALGORDSET P GENVAR)
	       (PUTPROP G P 'TELLRAT)))
	(RETURN (RGET G))))



;; the tellrat must be compatible with *genvar*

(DEFUN TELLRAT1 (X &AUX VARLIST GENVAR $ALGEBRAIC $RATFAC ALGVAR)
  (SETQ X ($TOTALDISREP X))
  (AND (NOT (ATOM X)) (EQ (CAAR X) 'MEQUAL)
       (NEWVAR (CADR X)))
  (NEWVAR (SETQ X (MEQHK X)))
  (OR VARLIST (MERROR "Improper polynomial"))
  (setq x (PRIMPART (CADR ($new_rat X))))
  (SETQ ALGVAR (if (symbolp (car x)) (get (car x) 'disrep)))
  (setq x (p-terms x))
  (IF (NOT (EQUAL (PT-LC X) 1)) (MERROR "Minimal polynomial must be monic"))
  (DO ((P (PT-RED X) (PT-RED P))) ((PTZEROP P)) (SETF (PT-LC P) (PDIS (PT-LC P))))
  (SETQ ALGVAR (CONS ALGVAR X))
  (IF (SETQ X (ASSOL (CAR ALGVAR) TELLRATLIST))
      (SETQ TELLRATLIST (zl-REMOVE X TELLRATLIST)))
  (PUSH ALGVAR TELLRATLIST))


