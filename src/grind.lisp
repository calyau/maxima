;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module grind)

(declare-top (GENPREFIX GRI)
	 (SPECIAL LOP ROP *GRIND-CHARLIST* CHRPS $ALIASES ALIASLIST LINEL)
	 (FIXNUM (CHRCT))
	 (*EXPR LBP RBP))

(DEFUN CHRCT () (f- LINEL CHRPS))

(DEFUN CHRCT* () (f- LINEL CHRPS))

#-MAXII
(DEFVAR ALPHABET '(#\% #\_))
(DEFVAR FORTRANP NIL)

;(DEFMSPEC $GRIND (X) (SETQ X (CDR X))
;  (LET (Y)
;    (IF (NOT (ZEROP (CHARPOS T))) (MTERPRI))
;    (COND ((OR (NULL X) (CDR X)) (WNA-ERR '$GRIND))
;	  ((ATOM (SETQ X (STRMEVAL (CAR X))))
;	   (SETQ X ($VERBIFY X))
;	   (COND ((SETQ Y (MGET X 'MEXPR))
;		  (MGRIND (LIST '(MDEFINE) (CONS (LIST X) (CDADR Y)) (CADDR Y)) NIL))
;		 ((SETQ Y (MGET X 'MMACRO))
;		  (MGRIND (LIST '(MDEFMACRO) (CONS (LIST X) (CDADR Y)) (CADDR Y)) NIL))
;		 ((SETQ Y (MGET X 'AEXPR))
;		  (MGRIND (LIST '(MDEFINE) (CONS (LIST X 'ARRAY) (CDADR Y)) (CADDR Y)) NIL))
;		 (T (MGRIND X NIL)))
;	   (TYO #/$ NIL))
;	  (T (MGRIND X NIL) (TYO #/$ NIL)))
;    '$DONE))
;Update from F302 --gsb
(DEFMSPEC $GRIND (X) (SETQ X (CDR X))
  (LET (Y)
    #+nocp(fresh-line)
    #-nocp(IF (NOT (ZEROP (CHARPOS T))) (MTERPRI))
    (COND ((OR (NULL X) (CDR X)) (WNA-ERR '$GRIND))
	  ((SYMBOLP (SETQ X (STRMEVAL (CAR X))))
	   (SETQ X ($VERBIFY X))
	   (COND ((SETQ Y (MGET X 'MEXPR))
		  (MGRIND (LIST '(MDEFINE) (CONS (LIST X) (CDADR Y)) (CADDR Y)) NIL))
		 ((SETQ Y (MGET X 'MMACRO))
		  (MGRIND (LIST '(MDEFMACRO) (CONS (LIST X) (CDADR Y)) (CADDR Y)) NIL))
		 ((SETQ Y (MGET X 'AEXPR))
		  (MGRIND (LIST '(MDEFINE) (CONS (LIST X 'array) (CDADR Y)) (CADDR Y)) NIL))
		 (T (MGRIND X NIL)))
	   (TYO #\$ NIL))
	  (T (MGRIND X NIL) (TYO #\$ NIL)))
    '$DONE))

(defun show-msize (lis)
  (format t "~%Length is ~A" (car lis))
  (sloop for v in (cdr lis)
	when (numberp v) do (princ (ascii v))
			    	else when (consp v)
	    do   (show-msize v)))
;;Msize returns a list whose first member is the number of characters
;;in the printed representation of the rest of the list.
;;thus to print something given it's msize you could
;;use msize-print if you did not care about line breaks etc.
;;If you care about them then you should send a newline
;;if the current distance to the margin is bigger than the first element of lis

(defun msize-print (lis)
  (sloop for v in (cdr lis)
	when (numberp v)
	  do (princ (ascii v))
	else do (msize-print v)))

(defun i-$grind (x)
  (LET (Y)
    #+nocp(fresh-line)
    #-nocp(IF (NOT (ZEROP (CHARPOS T))) (MTERPRI))
    (COND  ((SYMBOLP (SETQ X (STRMEVAL  X)))
	    (SETQ X ($VERBIFY X))
	    (COND ((SETQ Y (MGET X 'MEXPR))
		   (MGRIND (LIST '(MDEFINE) (CONS (LIST X) (CDADR Y)) (CADDR Y)) NIL))
		  ((SETQ Y (MGET X 'MMACRO))
		   (MGRIND (LIST '(MDEFMACRO) (CONS (LIST X) (CDADR Y)) (CADDR Y)) NIL))
		  ((SETQ Y (MGET X 'AEXPR))
		   (MGRIND (LIST '(MDEFINE) (CONS (LIST X 'array) (CDADR Y)) (CADDR Y)) NIL))
		  (T (MGRIND X NIL)))
	    (TYO #\$ NIL))
	  (T (MGRIND X NIL) (TYO #\$ NIL)))
    '$DONE))
  

(DEFUN MGRIND (X OUT)
  (SETQ CHRPS 0)
  (MPRINT (MSIZE X NIL NIL 'MPAREN 'MPAREN) OUT))

(DEFUN MPRINT (X OUT)
  (COND (#-cl (INTEGERP X)
	 #+cl (characterp x)
	(SETQ CHRPS (f1+ CHRPS)) (TYO X OUT))
	((< (CAR X) (CHRCT*)) (MAPC #'(LAMBDA (L) (MPRINT L OUT)) (CDR X)))
	(T (PROG (I) (SETQ I CHRPS)
	     (MPRINT (CADR X) OUT)
	     (COND ((NULL (CDDR X)) (RETURN NIL))
		   ((AND (OR (ATOM (CADR X)) (< (CAADR X) (CHRCT*)))
			 (OR (> (CHRCT*) (// LINEL 2))
			     (ATOM (CADDR X)) (< (CAADDR X) (CHRCT*))))
		    (SETQ I CHRPS)
		    (MPRINT (CADDR X) OUT))
		   (T (SETQ I (f1+ I)) (SETQ CHRPS 0) (TERPRI OUT)
		      (MTYOTBSP I OUT) (MPRINT (CADDR X) OUT)))
	     (DO ((L (CDDDR X) (CDR L))) ((NULL L))
		 (cond
		  ((OR (ATOM (CAR L)) (< (CAAR L) (CHRCT*))) NIL)
		  (t (SETQ CHRPS 0) (TERPRI OUT) (MTYOTBSP I OUT)))
		 (MPRINT (CAR L) OUT))))))

(DEFUN MTYOTBSP (N OUT) (DECLARE (FIXNUM N))
  (SETQ CHRPS (f+ N CHRPS))
  (DO () ((< N 8)) (TYO #\TAB OUT) (SETQ N (f- N 8)))
  (DO () ((< N 1)) (TYO #\SPACE OUT) (SETQ N (f1- N))))

(DEFUN STRGRIND (X)
  (LET (*GRIND-CHARLIST* (CHRPS 0))
    (STRPRINT (MSIZE X NIL NIL 'MPAREN 'MPAREN))
    (NREVERSE *GRIND-CHARLIST*)))

(DEFUN STRPRINT (X)
  (COND ((ATOM X) (STYO X))
	((< (CAR X) (CHRCT*)) (MAPC #'STRPRINT (CDR X)))
	(T (PROG (I)
	     (SETQ I CHRPS)
	     (STRPRINT (CADR X))
	     (COND ((NULL (CDDR X)) (RETURN NIL))
		   ((AND (OR (ATOM (CADR X)) (< (CAADR X) (CHRCT*)))
			 (OR (> (CHRCT*) (// LINEL 2))
			     (ATOM (CADDR X)) (< (CAADDR X) (CHRCT*))))
		    (SETQ I CHRPS)
		    (STRPRINT (CADDR X)))
		   (T (SETQ I (f1+ I)) (SETQ CHRPS 0) (STERPRI)
		      (STYOTBSP I) (STRPRINT (CADDR X))))
	     (DO ((L (CDDDR X) (CDR L))) ((NULL L))
		 (cond
		  ((OR (ATOM (CAR L)) (< (CAAR L) (CHRCT*))) NIL)
		  (t (SETQ CHRPS 0) (STERPRI) (STYOTBSP I)))
		 (STRPRINT (CAR L)))))))

(DEFUN STYO (X) (SETQ *GRIND-CHARLIST* (CONS X *GRIND-CHARLIST*) CHRPS (f1+ CHRPS)))

(DEFUN STERPRI () (SETQ *GRIND-CHARLIST* (CONS #\NEWLINE *GRIND-CHARLIST*) CHRPS 0))

(DEFUN STYOTBSP (N) (DECLARE (FIXNUM N)) (SETQ CHRPS N)
  (DO () ((< N 8)) (SETQ *GRIND-CHARLIST* (CONS #\TAB *GRIND-CHARLIST*) N (f- N 8)))
  (DO () ((< N 1)) (SETQ *GRIND-CHARLIST* (CONS #\SPACE *GRIND-CHARLIST*) N (f1- N))))

(DEFMFUN MSTRING (X)
  (NREVERSE (STRING1 (MSIZE X NIL NIL 'MPAREN 'MPAREN) NIL)))

(DEFUN STRING1 (X L)
  (cond
   ((ATOM X) (CONS X L))
   (t (SETQ X  (CDR X))
      (DO () ((NULL X) L) (SETQ L (STRING1 (CAR X) L) X (CDR X))))))



(DEFUN MSIZE (X L R LOP ROP)
  (SETQ X (NFORMAT X))
  (COND ((ATOM X) (IF FORTRANP (MSZ (MAKESTRING X) L R) (MSIZE-ATOM X L R)))
	((OR (<= (LBP (CAAR X)) (RBP LOP)) (> (LBP ROP) (RBP (CAAR X))))
	 (MSIZE-PAREN X L R))
	((MEMQ 'array (CDAR X)) (MSIZE-ARRAY X L R))
	((safe-GET (CAAR X) 'GRIND)
	 (the (values t) (FUNCALL (GET (CAAR X) 'GRIND) X L R)))
	(T (MSIZE-FUNCTION X L R NIL))))

(DEFUN MSIZE-ATOM (X L R)
  (PROG (Y)
    (COND ((NUMBERP X) (SETQ Y (EXPLODEN X)))
	  ((AND (SETQ Y (safe-GET X 'REVERSEALIAS))
		(NOT (AND (MEMQ X $ALIASES) (GET X 'NOUN))))
	   (SETQ Y (EXPLODEN Y)))
	  ((SETQ Y (ASSQR X ALIASLIST)) (RETURN (MSIZE (CAR Y) L R LOP ROP)))
	  ((NULL (SETQ Y (IF (EQ '%DERIVATIVE X)
			     (COPY-TOP-LEVEL '(#\% #\D #\I #\F #\F))
			     (EXPLODEN X)))))
	  ((char= #\$ (CAR Y)) (SETQ Y (SLASH (CDR Y))))
	  ((char= #\% (CAR Y)) (SETQ Y (SLASH (CDR Y))))
	  ((char= #\& (CAR Y))
	   (DO ((L (CDR Y) (CDR L))) ((NULL L))
	       (COND ((OR (zl-MEMBER (CAR L)
				     '(#. double-quote-char
					  #. back-slash-char
					     #. semi-colon-char #\$))
			  (AND (char< (CAR L) #\space)
			       (NOT (char= (CAR L) #\return ;13
					   ))))
		      (RPLACD L (CONS (CAR L) (CDR L)))
		      (RPLACA L #. back-slash-char) (SETQ L (CDR L)))))
	   (SETQ Y (CONS #. double-quote-char (NCONC (CDR Y) (LIST #. double-quote-char)))))
	  (T (SETQ Y (CONS #\? (SLASH Y)))))
  (RETURN (MSZ Y L R))))

(DEFUN MSZ (X L R) (SETQ X (NRECONC L (NCONC X R))) (CONS (LENGTH X) X))

(DEFUN SLASH (X)
  (DO ((L (CDR X) (CDR L))) ((NULL L))
      (IF (or (#+cl ALPHANUMERICP #-cl ALPHANUMP (CAR L))
	      (eql (car l) #\_))
	      NIL
	 (progn (RPLACD L (CONS (CAR L) (CDR L)))
		(RPLACA L #. back-slash-char) (SETQ L (CDR L)))))
  (IF (ALPHABETP (CAR X)) X (CONS #. back-slash-char X)))

#-cl
(DEFUN ALPHANUMP (N) (DECLARE (FIXNUM N))
  (OR (ASCII-NUMBERP N) (ALPHABETP N)))

(DEFUN MSIZE-PAREN (X L R) (MSIZE X (CONS #. left-parentheses-char L) (CONS #. right-parentheses-char R) 'MPAREN 'MPAREN))

;; The variables LB and RB are not uses here syntactically, but for
;; communication.  The FORTRAN program rebinds them to #/( and #/) since
;; Fortran array references are printed with parens instead of brackets.

(DEFVAR LB #\[)
(DEFVAR RB #\])

(DEFUN MSIZE-ARRAY (X L R &AUX F)
  (IF (EQ (CAAR X) 'MQAPPLY) (SETQ F (CADR X) X (CDR X)) (SETQ F (CAAR X)))
  (COND ((AND (symbolp (CAAR X)) (GET (CAAR X) 'VERB) (GET (CAAR X) 'ALIAS))
	 (SETQ L (RECONC '(#\' #\') L)))
	((AND (symbolp (CAAR X))
	      (GET (CAAR X) 'NOUN)
	      (NOT (MEMQ (CAAR X) (CDR $ALIASES)))
	      (NOT (GET (CAAR X) 'REVERSEALIAS)))
	 (SETQ L (CONS #\' L))))
  (SETQ L (MSIZE F L (LIST LB) LOP 'MFUNCTION)
	R (MSIZE-LIST (CDR X) NIL (CONS RB R)))
  (CONS (f+ (CAR L) (CAR R)) (CONS L (CDR R))))

(DEFUN MSIZE-FUNCTION (X L R OP)
  (COND ((not (symbolp (caar x))))
	((AND (GET (CAAR X) 'VERB) (GET (CAAR X) 'ALIAS))
	 (SETQ L (RECONC '(#\' #\') L)))
	((AND (GET (CAAR X) 'NOUN) (NOT (MEMQ (CAAR X) (CDR $ALIASES)))
	      (NOT (GET (CAAR X) 'REVERSEALIAS)))
	 (SETQ L (CONS #\' L))))
  (SETQ L (MSIZE (IF OP (GETOP (CAAR X)) (CAAR X)) L (NCONS #. left-parentheses-char ) 'MPAREN 'MPAREN)
	R (MSIZE-LIST (CDR X) NIL (CONS #. right-parentheses-char R)))
  (CONS (f+ (CAR L) (CAR R)) (CONS L (CDR R))))

(DEFUN MSIZE-LIST (X L R)
  (IF (NULL X) (MSZ NIL L R)
      (DO ((NL) (W 0))
	  ((NULL (CDR X))
	   (SETQ NL (CONS (MSIZE (CAR X) L R 'MPAREN 'MPAREN) NL))
	   (CONS (f+ W (CAAR NL)) (NREVERSE NL)))
	  (DECLARE (FIXNUM W))
	  (SETQ NL (CONS (MSIZE (CAR X) L (LIST #\,) 'MPAREN 'MPAREN) NL)
		W (f+ W (CAAR NL)) X (CDR X) L NIL))))

(DEFUN MSIZE-PREFIX (X L R)
  (MSIZE (CADR X) (RECONC (STRSYM (CAAR X)) L) R (CAAR X) ROP))

(DEFUN MSIZE-INFIX (X L R)
  (IF (OR (NULL (CDDR X)) (CDDDR X)) (WNA-ERR (CAAR X)))
  (SETQ L (MSIZE (CADR X) L NIL LOP (CAAR X))
	R (MSIZE (CADDR X) (REVERSE (STRSYM (CAAR X))) R (CAAR X) ROP))
  (LIST (f+ (CAR L) (CAR R)) L R))

(DEFUN MSIZE-POSTFIX (X L R)
  (MSIZE (CADR X) L (APPEND (STRSYM (CAAR X)) R) LOP (CAAR X)))

(DEFUN MSIZE-NARY (X L R) (MSZNARY X L R (STRSYM (CAAR X))))

(DEFUN MSIZE-NOFIX (X L R) (MSIZE (CAAR X) L R (CAAR X) ROP))

(DEFUN MSIZE-MATCHFIX (X L R)
  (SETQ L (NRECONC L (CAR (STRSYM (CAAR X))))
	L (CONS (LENGTH L) L)
	R (APPEND (CDR (STRSYM (CAAR X))) R)
	X (MSIZE-LIST (CDR X) NIL R))
  (CONS (f+ (CAR L) (CAR X)) (CONS L (CDR X))))

(DEFUN MSZNARY (X L R DISSYM)
 (COND ((NULL (CDDR X)) (MSIZE-FUNCTION X L R T))
       (T (SETQ L (MSIZE (CADR X) L NIL LOP (CAAR X)))
	  (DO ((OL (CDDR X) (CDR OL)) (NL (LIST L)) (W (CAR L)))
	      ((NULL (CDR OL))
	       (SETQ R (MSIZE (CAR OL) (REVERSE DISSYM) R (CAAR X) ROP))
	       (CONS (f+ (CAR R) W) (NREVERSE (CONS R NL))))
	     (DECLARE (FIXNUM W))
	      (SETQ NL (CONS (MSIZE (CAR OL) (REVERSE DISSYM) NIL (CAAR X) (CAAR X))
			     NL)
		    W (f+ (CAAR NL) W))))))

(DEFUN STRSYM (X) (OR (GET X 'STRSYM) (GET X 'DISSYM)))

(DEFPROP BIGFLOAT MSZ-BIGFLOAT GRIND)

(DEFUN MSZ-BIGFLOAT (X L R)
  (MSZ (MAPCAR #'(LAMBDA (L) (GETCHARN L 1)) (FPFORMAT X)) L R))

(DEFPROP MPROGN MSIZE-MATCHFIX GRIND)
(DEFPROP MLIST MSIZE-MATCHFIX GRIND)

(DEFPROP MQAPPLY MSZ-MQAPPLY GRIND)

(DEFUN MSZ-MQAPPLY (X L R)
  (SETQ L (MSIZE (CADR X) L (LIST #. left-parentheses-char ) LOP 'MFUNCTION)
	R (MSIZE-LIST (CDDR X) NIL (CONS #. right-parentheses-char R)))
  (CONS (f+ (CAR L) (CAR R)) (CONS L (CDR R))))


(DEFPROP MQUOTE MSIZE-PREFIX GRIND)
(DEFPROP MQUOTE 201. RBP)
(DEFPROP MSETQ MSIZE-INFIX GRIND)
(DEFPROP MSETQ MSIZE-INFIX GRIND)

(DEFPROP MSETQ (#\:) STRSYM)
(DEFPROP MSETQ 180. RBP)
(DEFPROP MSETQ 20. RBP)

(DEFPROP MSET MSIZE-INFIX GRIND)
(DEFPROP MSET (#\: #\:) STRSYM)
(DEFPROP MSET 180. LBP)
(DEFPROP MSET 20. RBP)

(DEFPROP MDEFINE MSZ-MDEF GRIND)
(DEFPROP MDEFINE (#\: #\=) STRSYM)
(DEFPROP MDEFINE 180. LBP)
(DEFPROP MDEFINE 20. RBP)

(DEFPROP MDEFMACRO MSZ-MDEF GRIND)
(DEFPROP MDEFMACRO (#\: #\: #\=) STRSYM)
(DEFPROP MDEFMACRO 180. LBP)
(DEFPROP MDEFMACRO 20. RBP)

(DEFUN MSZ-MDEF (X L R)
  (SETQ L (MSIZE (CADR X) L (COPY-TOP-LEVEL (STRSYM (CAAR X))) LOP (CAAR X))
	R (MSIZE (CADDR X) NIL R (CAAR X) ROP))
  (SETQ X (CONS (f- (CAR L) (CAADR L)) (CDDR L)))
  (IF (AND (NOT (ATOM (CADR R))) (NOT (ATOM (CADDR R)))
	   (< (f+ (CAR L) (CAADR R) (CAADDR R)) LINEL))
      (SETQ X (NCONC X (LIST (CADR R) (CADDR R)))
	    R (CONS (CAR R) (CDDDR R))))
  (CONS (f+ (CAR L) (CAR R)) (CONS (CADR L) (CONS X (CDR R)))))


(DEFPROP MFACTORIAL MSIZE-POSTFIX GRIND)
(DEFPROP MFACTORIAL 160. LBP)

(DEFPROP MEXPT MSZ-MEXPT GRIND)
(DEFPROP MEXPT 140. LBP)
(DEFPROP MEXPT 139. RBP)

(DEFUN MSZ-MEXPT (X L R)
  (SETQ L (MSIZE (CADR X) L NIL LOP 'MEXPT)
	R (IF (MMMINUSP (SETQ X (NFORMAT (CADDR X))))
	      (MSIZE (CADR X) (REVERSE '(#\^ #\-)) R 'MEXPT ROP)
	      (MSIZE X (LIST #\^) R 'MEXPT ROP)))
  (LIST (f+ (CAR L) (CAR R)) L R))


(DEFPROP MNCEXPT MSIZE-INFIX GRIND)
(DEFPROP MNCEXPT 135. LBP)
(DEFPROP MNCEXPT 134. RBP)

(DEFPROP MNCTIMES MSIZE-NARY GRIND)
(DEFPROP MNCTIMES 110. LBP)
(DEFPROP MNCTIMES 109. RBP)

(DEFPROP MTIMES MSZ-MTIMES GRIND)
(DEFPROP MTIMES 120. LBP)
(DEFPROP MTIMES 120. RBP)

(DEFUN MSZ-MTIMES (X L R) (MSZNARY X L R '(#\*)))


(DEFPROP MQUOTIENT MSIZE-INFIX GRIND)
(DEFPROP MQUOTIENT 120. LBP)
(DEFPROP MQUOTIENT 121. RBP) 
(DEFPROP RAT MSIZE-INFIX GRIND)
(DEFPROP RAT 120. LBP)
(DEFPROP RAT 121. RBP)

(DEFPROP MPLUS MSZ-MPLUS GRIND)
(DEFPROP MPLUS 100. LBP)
(DEFPROP MPLUS 100. RBP)

(DEFUN MSZ-MPLUS (X L R)
 (COND ((NULL (CDDR X))
	(IF (NULL (CDR X))
	    (MSIZE-FUNCTION X L R T)
	    (MSIZE (CADR X) (APPEND (NCONS #\+) L) R 'MPLUS ROP)))
       (T (SETQ L (MSIZE (CADR X) L NIL LOP 'MPLUS) X (CDDR X))
	  (DO ((NL (LIST L)) (W (CAR L)) (DISSYM))
	      ((NULL (CDR X))
	       (IF (MMMINUSP (CAR X)) (SETQ L (CADAR X) DISSYM (LIST #\-))
		   (SETQ L (CAR X) DISSYM (LIST #\+)))
	       (SETQ R (MSIZE L DISSYM R 'MPLUS ROP))
	       (CONS (f+ (CAR R) W) (NREVERSE (CONS R NL))))
	     (DECLARE (FIXNUM W))
	      (IF (MMMINUSP (CAR X)) (SETQ L (CADAR X) DISSYM (LIST #\-))
		  (SETQ L (CAR X) DISSYM (LIST #\+)))
	      (SETQ NL (CONS (MSIZE L DISSYM NIL 'MPLUS 'MPLUS) NL)
		    W (f+ (CAAR NL) W)
		    X (CDR X))))))

(DEFPROP MMINUS MSIZE-PREFIX GRIND)
(DEFPROP MMINUS (#\-) STRSYM)
(DEFPROP MMINUS 100. RBP)
(DEFPROP MMINUS 100. LBP)

(DEFPROP MEQUAL MSIZE-INFIX GRIND)
(DEFPROP MEQUAL 80. LBP)
(DEFPROP MEQUAL 80. RBP)

(DEFPROP MNOTEQUAL MSIZE-INFIX GRIND)
(DEFPROP MNOTEQUAL 80. LBP)
(DEFPROP MNOTEQUAL 80. RBP)

(DEFPROP MGREATERP MSIZE-INFIX GRIND)
(DEFPROP MGREATERP 80. LBP)
(DEFPROP MGREATERP 80. RBP)

(DEFPROP MGEQP MSIZE-INFIX GRIND)
(DEFPROP MGEQP 80. LBP)
(DEFPROP MGEQP 80. RBP)

(DEFPROP MLESSP MSIZE-INFIX GRIND)
(DEFPROP MLESSP 80. LBP)
(DEFPROP MLESSP 80. RBP)

(DEFPROP MLEQP MSIZE-INFIX GRIND)
(DEFPROP MLEQP 80. LBP)
(DEFPROP MLEQP 80. RBP)

(DEFPROP MNOT MSIZE-PREFIX GRIND)
(DEFPROP MNOT 70. RBP)

(DEFPROP MAND MSIZE-NARY GRIND)
(DEFPROP MAND 60. LBP)
(DEFPROP MAND 60. RBP)

(DEFPROP MOR MSIZE-NARY GRIND)
(DEFPROP MOR 50. LBP)
(DEFPROP MOR 50. RBP)

(DEFPROP MCOND MSZ-MCOND GRIND)
(DEFPROP MCOND 25. LBP)
(DEFPROP MCOND 25. RBP)

(DEFUN MSZ-MCOND (X L R &AUX IF)
    (SETQ IF (NRECONC L '(#\I #\F #\SPACE)) IF (CONS (LENGTH IF) IF)
	  L (MSIZE (CADR X) NIL NIL 'MCOND 'MPAREN))
    (COND ((EQ '$FALSE (FIFTH X))
	   (SETQ X (MSIZE (CADDR X)
			  (REVERSE '(#\SPACE #\T #\H #\E #\N #\SPACE))
			  R 'MCOND ROP))
	   (LIST (f+ (CAR IF) (CAR L) (CAR X)) IF L X))
	  (T (SETQ R (MSIZE (FIFTH X)
			    (REVERSE '(#\SPACE #\E #\L #\S #\E #\SPACE))
			    R 'MCOND ROP)
		   X (MSIZE (CADDR X)
			    (REVERSE '(#\SPACE #\T #\H #\E #\N #\SPACE))
			    NIL 'MCOND 'MPAREN))
	     (LIST (f+ (CAR IF) (CAR L) (CAR X) (CAR R)) IF L X R))))

(defprop text-string msize-text-string grind)
(defun msize-text-string (x l r)
  (cons (length (cdr x)) (cdr x))
  )

(DEFPROP MDO MSZ-MDO GRIND)
(DEFPROP MDO 30. LBP)
(DEFPROP MDO 30. RBP)
(DEFPROP MDOIN MSZ-MDOIN GRIND)
(DEFPROP MDOIN 30. RBP)

(DEFUN MSZ-MDO (X L R)
  (MSZNARY (CONS '(MDO) (STRMDO X)) L R '(#\SPACE)))

(DEFUN MSZ-MDOIN (X L R)
  (MSZNARY (CONS '(MDO) (STRMDOIN X)) L R '(#\SPACE)))

(DEFUN STRMDO (X)
  (NCONC (COND ((SECOND X) `($FOR ,(SECOND X))))
	 (COND ((EQUAL 1 (THIRD X)) NIL)
	       ((THIRD X)  `($FROM ,(THIRD X))))
	 (COND ((EQUAL 1 (FOURTH X)) NIL)
	       ((FOURTH X) `($STEP ,(FOURTH X)))
	       ((FIFTH X)  `($NEXT ,(FIFTH X))))
	 (COND ((SIXTH X)  `($THRU ,(SIXTH X))))
	 (COND ((NULL (SEVENTH X)) NIL)
	       ((EQ 'MNOT (CAAR (SEVENTH X)))
		`($WHILE ,(CADR (SEVENTH X))))
	       (T `($UNLESS ,(SEVENTH X))))
	 `($DO ,(EIGHTH X))))

(DEFUN STRMDOIN (X)
  (NCONC `($FOR ,(SECOND X) $IN ,(THIRD X))
	 (COND ((SIXTH X) `($THRU ,(SIXTH X))))
	 (COND ((NULL (SEVENTH X)) NIL)
	       ((EQ 'MNOT (CAAR (SEVENTH X)))
		`($WHILE ,(CADR (SEVENTH X))))
	       (T `($UNLESS ,(SEVENTH X))))
	 `($DO ,(EIGHTH X))))



