;;;  -*- LISP -*-
;;;	** (c) Copyright 1979 Massachusetts Institute of Technology **

(in-package "MAXIMA")

(macsyma-module hyp)

(eval-when (compile eval)
(declare-top (special fun w b l alglist $true $false n  c l1 l2))
)
(DECLARE-TOP (SPECIAL VAR PAR ZEROSIGNTEST PRODUCTCASE 
		  FLDEG FLGKUM CHECKCOEFSIGNLIST SERIESLIST
		  $EXPONENTIALIZE $BESTRIGLIM $RADEXPAND FAIL-SYM)
	     )


;; (eval-when (compile eval) (load '((dsk ell) macros >)) )

(declare-top (special fldeg flgkum listcmdiff checkcoefsignlist serieslist
		      fl1f1))
(SETQ FLGKUM T FLDEG T FL1F1 T CHECKCOEFSIGNLIST NIL)

(declare-top (special $exponentialize $bestriglim $radexpand))

(setq fail-sym (gensym))
(defvar 3//2 '((rat simp) 3 2))
(defvar 1//2 '((rat simp) 1 2))
(defvar -1//2 '((rat simp) -1 2))

(eval-when (eval compile)
(defmacro fixp (x) `(typep ,x 'fixnum))

(setq FLGKUM T FLDEG T FL1F1 T CHECKCOEFSIGNLIST ()
;;      $BESTRIGLIM 3. $RADEXPAND '$ALL
      FAIL-SYM (GENSYM))

(DEFMACRO SIMP (X) `(SIMPLIFYA ,X ()))

(DEFMACRO SIMP-LIST (L) `(MAPCAR #'(LAMBDA (X) (SIMP X)) ,L))

;; jfa removed to avoid conflict with main definition of MABS.
;; (DEFMACRO MABS (X) `(SIMP `((MABS) ,,X)))

(DEFMACRO MSQRT (X) `(M^T ,X 1//2))

(DEFMACRO MEXPT (X) `(M^T '$%E ,X))

(DEFMACRO MLOG (X) `(SIMP `((%LOG) ,,X)))

(DEFMACRO MSIN (X) `(SIMP `((%SIN) ,,X)))

(DEFMACRO MCOS (X) `(SIMP `((%COS) ,,X)))

(DEFMACRO MASIN (X) `(SIMP `((%ASIN) ,,X)))

(DEFMACRO MATAN (X) `(SIMP `((%ATAN) ,,X)))

(DEFMACRO MGAMMA (X) `(SIMP `((%GAMMA) ,,X)))

(DEFMACRO MBINOM (X Y) `(SIMP `((%BINOMIAL) ,,X ,,Y)))

(DEFMACRO MERF (X) `(SIMP `((%ERF) ,,X)))

(DEFMACRO =1//2 (X) `(ALIKE1 ,X 1//2))

(DEFMACRO =3//2 (X) `(ALIKE1 ,X 3//2))

(DEFMACRO =-1//2 (X) `(ALIKE1 ,X -1//2))
)


(DEFUN $HGFRED
       (L1 L2 ARG &aux ($bestriglim 3) ($radexpand '$all))
       (prog()
	    (setq var arg par arg)
	    (return (HGFSIMP-EXEC (CDR L1)(CDR L2) ARG))))


(DEFUN HGFSIMP-EXEC
       (L1 L2 ARG)
       (setq l1 (copy-tree l1) l2 (copy-tree l2))
       (PROG (RES $exponentialize)
	     (SETQ  RES
		   (HGFSIMP L1 L2 ARG))
	     (COND ((OR (NUMBERP RES)(NOT (ATOM RES)))
		    (RETURN RES)))
	     (RETURN (FPQFORM L1 L2 ARG))))


(DEFUN HGFSIMP
       (L1 L2 VAR)
       (PROG (RESIMP )
	    (SETQ L1
		  (MACSIMP L1)
		  L2
		  (MACSIMP L2)
		  RESIMP
		  (SIMPG L1 L2))
	    
	    (COND ((NOT (EQ (CAR RESIMP) 'FAIL))(RETURN RESIMP)))
	    (COND ((SETQ LISTCMDIFF
			 (INTDIFFL1L2 (CADR RESIMP)
				      (CADDR RESIMP)))
		   (return (splitpfq listcmdiff
				     (cadr resimp)
				     (caddr resimp)))))
	    (RETURN (DISPATCH-SPEC-SIMP (CADR RESIMP)
					(CADDR RESIMP)))))



(DEFUN MACSIMP
       (L)

       (COND ((NULL L) NIL)
	     (T (APPEND (LIST (SIMPLIFYA (CAR L) NIL)) (CDR L)))))


(DEFUN SIMPG
       (L1 L2)
       (PROG(IL)
	    (COND ((NULL (SETQ IL (INTERSECTION L1 L2)))
		   (RETURN (SIMPG-EXEC L1 L2))))
	    (RETURN (SIMPG-EXEC (DEL IL L1)(DEL IL L2)))))   


(DEFUN DEL
       (A B)
       (COND ((NULL A) B)(T (DEL (CDR A) (ZL-DELETE (CAR A) B 1)))))


(DEFUN SIMPG-EXEC
       (L1 L2)
       (PROG(N)
	    (COND ((ZEROP-IN-L L1)(RETURN 1)))
	    (COND ((SETQ N (hyp-NEGP-IN-L L1))
		   (RETURN (CREATE-POLY L1 L2 N))))
	    (COND ((OR (ZEROP-IN-L L2)(hyp-NEGP-IN-L L2))
		   (RETURN 'UNDEF)))
	    (RETURN (APPEND (LIST 'FAIL)(LIST L1)(LIST L2)))))
			

(DEFUN INTDIFFL1L2
       (L1 L2)
       (COND ((NULL L1)  NIL)(T (INTDIFF L1 L2))))

(DEFUN INTDIFF
       (L1 L2)
       (PROG(L A DIF)
	    (SETQ L L2 A (CAR L1))
	    JUMP
	    (COND ((NULL L)(RETURN (INTDIFFL1L2 (CDR L1) L2))))
	    (COND ((NNI (SETQ DIF (SUB A (CAR L))))
		   (RETURN (LIST A DIF))))
	    (SETQ L (CDR L))
	    (GO JUMP)))		     


(DEFUN CREATE-POLY
       (L1 L2 N)
       ((LAMBDA(LEN1 LEN2)
	       (COND ((AND (EQual LEN1 2)(EQual LEN2 1))
		      (2F1POLYS L1 L2 N))
		     ((AND (EQual LEN1 1)(EQual LEN2 1))
		      (1F1POLYS L2 N))
		     ((AND (EQual LEN1 2)(ZEROP LEN2))
		      (2F0POLYS L1 N))
		     (T (CREATE-ANY-POLY L1 L2 (mul -1 N)))))
	(LENGTH L1)
	(LENGTH L2)))


(DEFUN 1F1POLYS
       (L2 N)
       (PROG(C FACT1 FACT2)
	    (SETQ C
		  (CAR L2)
		  N
		  (MUL -1 N)
		  FACT1
		  (MUL (POWER 2 N)
		       (FACTORIAL N)
		       (INV (POWER -1 N)))
		  FACT2
		  (MUL (POWER 2 (INV 2))(POWER VAR (INV 2))))
	    (COND ((EQUAL C (DIV 1 2))
		   (RETURN (MUL FACT1
				(INV (FACTORIAL (ADD N N)))
				(HERMPOL (ADD N N) FACT2)))))
	    (COND ((EQUAL C (DIV 3 2))
		   (RETURN (MUL FACT1
				(INV (FACTORIAL (ADD N N 1)))
				(HERMPOL (ADD N N 1) FACT2)))))
	    (RETURN (MUL (FACTORIAL N)
			 (GM C)
			 (GM (ADD C N))
			 (LAGPOL N (SUB C 1) VAR)))))


(DEFUN HERMPOL(N ARG)(LIST '(MQAPPLY)(LIST '($%HE ARRAY) N) ARG))
(DEFUN LAGPOL(N A ARG)(LIST '(MQAPPLY)(LIST '($%L ARRAY) N A) ARG))


(DEFUN 2F0POLYS
       (L1 N)
       (PROG(A B TEMP X)
	    (SETQ A (CAR L1) B (CADR L1))
	    (COND ((EQUAL (SUB B A)(DIV -1 2))
		   (SETQ TEMP A A B B TEMP)))
	    (COND ((EQUAL (SUB B A)(DIV 1 2))
		   (SETQ X (POWER (DIV 2 (MUL -1 VAR))(INV 2)))
		   (RETURN (INTERHERMPOL N A B X))))
	    (SETQ X (MUL -1 (INV VAR)) N (MUL -1 N))
	    (RETURN (MUL (FACTORIAL N)
			 (INV (POWER X N))
			 (INV (POWER -1 N))
			 (LAGPOL N (ADD B N) X)))))

(DEFUN INTERHERMPOL
       (N A B X)
       (PROG(FACT)
	    (SETQ FACT (POWER X (MUL -1 N)))
	    (COND ((EQUAL A N)
		   (SETQ N (MUL -2 N))
		   (RETURN (MUL FACT (HERMPOL N X)))))
	    (COND ((EQUAL B N)
		   (SETQ N (SUB 1 (ADD N N)))
		   (RETURN (MUL FACT (HERMPOL N X)))))))


(DEFUN 2F1POLYS
       (L1 L2 N)
       (PROG(L V LGF)
	    (COND ((NOT (EQ (CAR L1) N))(setq l1 (REVERSE L1))))
	    (SETQ L (VFVP (DIV (ADD (CADR L1) N) 2)))
	    (SETQ V (CDR (ZL-assoc 'V L)))
	    
	    (cond ((setq lgf (legpol (car l1)(cadr l1)(car l2)))
		   (return lgf)))
	    (COND ((EQUAL (SUB (CAR L2) V) '((RAT SIMP) 1 2))
		   (RETURN (mul 
				(cond ((zerp v) 1)
				      (t (mul (factorial (* -1 n))
					      (inv (factf (mul 2 v)(* -1 n))))))
				(GEGENPOL (mul -1 N)
					  V
					  (SUB 1 (MUL 2 PAR)))))))
	    (RETURN (mul (factorial (* -1 n))
			 (inv (factf (add 1 v) (* -1 n)))
			 (JACOBPOL (mul -1 N)
				   (ADD (CAR L2) -1)
				   (SUB (MUL 2 V)(CAR L2))
				   (SUB 1 (MUL 2 PAR)))))))


(DEFUN JACOBPOL
       (N A B X)
       (LIST '(MQAPPLY)(LIST '($%P ARRAY) N A B) X))


(DEFUN GEGENPOL(N V X)
       (cond ((equal v 0) (tchebypol n x))
	     (t (LIST '(MQAPPLY)(LIST '($%C ARRAY) N V) X)))) 
(defun legenpol(n x)(list '(mqapply)(list '($%P array) n) x))
(defun tchebypol (n x)(list '(mqapply)(list '($%T array) n) x))
(DEFUN CREATE-ANY-POLY
       (L1 L2 N)
       (PROG(RESULT EXP PRODNUM PRODEN)
	    (SETQ RESULT 1 PRODNUM 1 PRODEN 1 EXP 1)
	    LOOP
	    (COND ((ZEROP N) (RETURN RESULT)))
	    (SETQ PRODNUM
		  (MUL PRODNUM (MULL L1))
		  PRODEN
		  (MUL PRODEN (MULL L2)))
	    (SETQ RESULT
		  (ADD RESULT
		       (MUL PRODNUM
			    (POWER VAR EXP)
			    (INV PRODEN)
			    (INV (FACTORIAL EXP)))))
	    (SETQ N
		  (sub N 1)
		  EXP
		  (add EXP 1)
		  L1
		  (INCR1 L1)
		  L2
		  (INCR1 L2))
	    (GO LOOP)))


(DEFUN MULL(L)(COND ((NULL L) 1)(T (MUL (CAR L)(MULL (CDR L))))))


(DEFUN INCR1
       (L)
       (COND ((NULL L) NIL)
	     (T (APPEND (LIST (ADD (CAR L) 1))(INCR1 (CDR L))))))


(DEFUN DISPATCH-SPEC-SIMP
       (L1 L2)
       (PROG(LEN1 LEN2)
	    (SETQ LEN1 (LENGTH L1) LEN2 (LENGTH L2))
	    (COND ((AND (LESSP LEN1 2)(LESSP LEN2 2))
		   (RETURN (SIMP2>F<2 L1 L2 LEN1 LEN2))))
	    (COND ((AND (EQUAL LEN1 2)(EQUAL LEN2 1))
		   (RETURN (SIMP2F1 L1 L2))))
	    (RETURN (FPQFORM L1 L2 VAR))))


(DEFUN SIMP2>F<2
       (L1 L2 LEN1 LEN2)
       (PROG()
	    (COND ((AND (ZEROP LEN1)(ZEROP LEN2))
		   (RETURN (POWER '$%E VAR))))
	    (COND ((AND (ZEROP LEN1)(EQUAL LEN2 1))
		   (RETURN (BEStrig (CAR L2) VAR))))
	    (COND ((ZEROP LEN2)(RETURN (BINOM (CAR L1)))))
	    (RETURN (CONFL L1 L2 var))))


	    

(DEFUN BEStrig
       (A X)
       (prog (n res)
	     (setq res (mul (gm a) (power x (div (sub 1 a) 2))))
	     (COND ((AND (MAXIMA-INTEGERP (ADD A A))
			 (NUMBERP (SETQ N (SUB A (INV 2))))
			 (LESSP N $bestriglim))
		    (return (mul res
				 (MEVAL (BESREDTRIG (- N 1)
						    (mul 2
							 '$%I
							 (power
							  x
							  (inv
							   2)))))))))
	     (cond ((equal (checksigntm x) '$negative)
		    (return (mul res
			 (BES (SUB A 1) (setq X (mul -1 x)) 'J)))))
	     (return (mul res (BES (SUB A 1) X 'I)))))
	    
	    

(DEFUN BES
       (A X FLG)
       (LIST '(MQAPPLY)
	     (LIST (COND ((EQ FLG 'J) '($%J ARRAY))
			 (T '($%IBES ARRAY)))
		   A)
	     (MUL 2 (POWER X (INV 2)))))




(DEFUN BESREDTRIG
       (N Z)
       (COND ((MINUSP N)(TRIGREDMINUS (MUL -1 (ADD1 N)) Z))
	     (T (TRIGREDPLUS N Z))))
(DEFUN TRIGREDPLUS
       (N Z)
       ((LAMBDA(NPINV2)
	       (MUL (CTR Z)
		    (ADD (MUL (SIN% (SUB Z NPINV2))
			      (FIRSTSUM N Z))
			 (MUL (COS% (SUB Z NPINV2))
			      (SECONDSUM N Z)))))
	(MUL N '$%PI (INV 2))))


(DEFUN TRIGREDMINUS
       (N Z)
       ((LAMBDA(NPINV2)
	       (MUL (CTR Z)
		    (SUB (MUL (COS% (ADD Z NPINV2))
			      (FIRSTSUM N Z))
			 (MUL (SIN% (ADD Z NPINV2))
			      (SECONDSUM N Z)))))
	(MUL N '$%PI (INV 2))))

(DEFUN FIRSTSUM
       (N Z)
       (PROG(COUNT RESULT 2R N1)
	    (SETQ N1 ($ENTIER (DIV N 2)) COUNT 0 RESULT 1)
	    LOOP
	    (COND ((EQ COUNT N1)(RETURN RESULT)))
	    (SETQ COUNT
		  (ADD1 COUNT)
		  2R
		  (ADD COUNT COUNT)
		  RESULT
		  (ADD RESULT
		       (DIV (MUL (POWER -1 COUNT)
				 (FACTORIAL (ADD N 2R)))
			    (MUL (FACTORIAL 2R)
				 (FACTORIAL (SUB N 2R))
				 (POWER (ADD Z Z) 2R)))))
	    (GO LOOP)))

(DEFUN SECONDSUM
       (N Z)
       (PROG(COUNT RESULT 2R+1 N1)
	    (SETQ N1
		  ($ENTIER (DIV (SUB1 N) 2))
		  COUNT
		  0
		  RESULT
		  (INV Z))
	    (COND ((EQual N1 -1)(RETURN 0)))
	    LOOP
	    (COND ((EQ COUNT N1)(RETURN RESULT)))
	    (SETQ COUNT
		  (ADD1 COUNT)
		  2R+1
		  (ADD COUNT COUNT 1)
		  RESULT
		  (ADD RESULT
		       (DIV (MUL (POWER -1 COUNT)
				 (FACTORIAL (ADD N 2R+1)))
			    (MUL (FACTORIAL 2R+1)
				 (FACTORIAL (SUB N 2R+1))
				 (POWER (ADD Z Z) 2R+1)))))
	    (GO LOOP)))

(DEFUN CTR(Z)(POWER (DIV 2 (MUL '$%PI Z))(INV 2)))

(DEFUN NEGCOEF
       (X)
       (PROG(D)
	    (COND ((NULL (SETQ D (CDR (ZL-REMPROP 'D (D*U X)))))
		   (RETURN T)))
	    (COND ((EQ (ASKSIGN (INV D)) '$POSITIVE)
		   (RETURN NIL)))
	    (RETURN T)))


(DEFUN BINOM(A)(POWER (SUB 1 VAR) (MUL -1 A)))



(DEFUN KUMMER
       (L1 L2)
       (MUL (LIST '(MEXPT) '$%E VAR)
	    (confl (LIST (SUB (CAR L2)(CAR L1))) L2 (MUL -1 VAR))))


(DEFUN ZEROP-IN-L
       (L)
       (COND ((NULL L) NIL)
	     ((NUMBERP (CAR L))
	      (COND ((ZEROP (CAR L)) T)(T (ZEROP-IN-L (CDR L)))))
	     (T (ZEROP-IN-L (CDR L)))))


(DEFUN hyp-NEGP-IN-L
       (L)
       (COND ((NULL L) NIL)
	     ((MAXIMA-INTEGERP (CAR L))
	      (COND ((MINUSP (CAR L)) (CAR L))
		    (T (hyp-NEGP-IN-L (CDR L)))))
	     (T (hyp-NEGP-IN-L (CDR L)))))


(DEFUN INTERSECTION
       (L1 L2)
       (cond ((null l1) nil)
	     ((zl-member (car l1) l2)
	      (cons (car l1)
		    (intersection (cdr l1)
				  (zl-delete (car l1) l2 1))))
	     (t (intersection (cdr l1) l2))))

(DEFUN 2INP
       (L)
       (PROG(COUNT)
	    (SETQ COUNT 0)
	    LOOP
	    (COND ((AND (NULL L)(GREATERP COUNT 1))(RETURN T)))
	    (COND ((NULL L)(RETURN NIL)))
	    (COND ((MAXIMA-INTEGERP (CAR L))(SETQ COUNT (ADD1 COUNT))))
	    (SETQ L (CDR L))
	    (GO LOOP)))


(DEFUN 2RATP
       (L)
       (PROG(COUNT)
	    (SETQ COUNT 0)
	    LOOP
	    (COND ((AND (NULL L)(GREATERP COUNT 1))(RETURN T)))
	    (COND ((NULL L)(RETURN NIL)))
	    (COND ((EQ (CAAAR L) 'RAT)(SETQ COUNT (ADD1 COUNT))))
	    (SETQ L (CDR L))
	    (GO LOOP)))
;2NUMP SHOULD BE ELIMINATED. IT IS NOT EFFICIENT TO USE ANYTHING ELSE BUT JUST CONVERTING TO RAT REPRESENTATION ALL 0.X ,X IN N. ESPECIALLY LATER WHEN WE CONVERT TO OMONIMA FOR TESTING TO FIND THE RIGHT FORMULA


(DEFUN 2NUMP
       (L)
       (PROG(COUNT)
	    (SETQ COUNT 0)
	    LOOP
	    (COND ((AND (NULL L)(GREATERP COUNT 1))(RETURN T)))
	    (COND ((NULL L)(RETURN NIL)))
	    (COND ((NUMBERP (CAR L))(SETQ COUNT (ADD1 COUNT))))
	    (SETQ L (CDR L))
	    (GO LOOP)))


(DEFUN WHITFUN(K M VAR)(LIST '(MQAPPLY)(LIST '($%M ARRAY) K M) VAR))

(DEFUN SIMP2F1
       (L1 L2)
       (PROG(A B C LGF)
	    (SETQ A (CAR L1) B (CADR L1) C (CAR L2))
	    (cond ((and (equal a 1)
			(equal b 1)
			(equal c 2))
		   (return (mul (inv (mul -1 var))
				($log (add 1 (mul -1 var)))))))
	    (cond ((or (equal c  (div 3 2))
		       (equal c  (div 1 2)))
		   (cond ((setq lgf (trig-log (list a b) (list c)))
			  (return lgf)))))
	    
	    (cond ((or
		    (equal (sub a b) (div 1 2))
		    (equal (sub b a) (div 1 2)))
		   (cond ((setq lgf (hyp-cos a b c))(return lgf)))))
	    (cond ((and (maxima-integerp a)
			(maxima-integerp b) (maxima-integerp c))
		   (return (simpr2f1 (list a b) (list c)))))
	    (cond ((and (maxima-integerp (add c (inv 2)))
			(maxima-integerp (add a b)))
		   (return (step4 a b c))))
	    (cond ((maxima-integerp (add (sub a b) (inv 2)))
		   (cond ((setq lgf (step7 a b c))
			  (return lgf)))))
	    (COND ((SETQ LGF (LEGFUN A B C))(RETURN LGF)))
	    (PRINT 'SIMP2F1-WILL-CONTINUE-IN)
	    (RETURN  (FPQFORM L1 L2 VAR))))

(defun step7 (a b c)
       (prog (l m n k mn kl sym sym1 r)
	     (setq l (s+c a)
		   sym (cdras 'f l)
		   mn  (cdras 'c l)
		   l (s+c c)
		   sym1 (cdras 'f l))
	     (cond ((not (equal (mul sym 2) sym1))(return nil)))
	     (setq kl (cdras 'c l)
		   l  (s+c b)
		   r (sub (add (inv 2) (cdras 'c l)) mn)
		   m ($num mn)
		   n ($denom mn)
		   k ($num kl)
		   l ($denom kl))
	     (cond ((equal (* 2 l) n)
		    (cond ((maxima-integerp (// (- k m) n))
			   (return (hyp-algv k l m n a b))))))
	     (cond ((maxima-integerp (// k (* 2 l)))
		    (cond ((maxima-integerp (// m n))
			   (return (hyp-algv k l m n a b)))
			  (t (return nil))))
		   ((maxima-integerp (// m n))
			   (return nil))
		   ((maxima-integerp (/ (- (* k n) (* 2 l m)) (* 2 l n)))
		    (return (hyp-algv k l m n a b))))
	     (return nil)))

(defun getxy
       (k l m n)
       (prog (x y)
	     (setq y 0)
	     loop
	     (cond ((maxima-integerp (setq x
				    (// (+ y
					   (// k l)
					   (* -2 (// m n)))
					2)))
		    (return (list x y))))
	     (setq y (+ 2 y))
	     (go loop)))

(defun hyp-algv  (k l m n a b c)
       (prog (x y xy a-b)
	     (setq a-b (- a b))
	     (setq xy (getxy k l m n)
		   x (car xy)
		   y (cdr xy))
	     (cond ((< x 0)(go out)))
	     (cond ((< x y)(cond ((< (+ a-b x (inv 2)) 0)
				  (return (f88 x y a c fun)))
				 (t (return (f87 x y a c fun)))))
		   (t (cond ((< (+ a-b x (inv 2)) 0)
			     (return (f90 x y a c fun)))
			    (t (return (f89 x y a c fun))))))
	     out
	     (setq w (* x -1))
	     (cond ((< (- (+ a-b (inv 2)) w) 0)
		    (return (f92 x y a c fun)))
		   (t (return (f91 x y a c fun))))))

(defun f87 (x y a c fun )
       (mul
	(inv (mul (factf c y)
		  (factf (sub (add c y) (add a x)) (- x y))
		  (factf (sub (add c y) (add a x (inv 2)))
			 (sub (add a x (inv 2)) (add a (inv 2))))))
	(power 'ell (sub 1 c))
	(power (sub 1 'ell)(sub (add y c) (add a (inv 2))))
	($diff (mul (power 'ell (add a x))
		    (power (sub 1 'ell)(mul -1 a))
		    ($diff (mul (power 'ell (sub (add (inv 2) x) y))
				($diff (mul (power 'ell (sub (add c y) 1))
					    (power (sub 1 'ell)
						   (sub (add (inv 2)
							     (mul 2 a)
							     (* 2 x))
							(add c y)))
					    fun)
				       'ell x))
				'ell (- x y)))
	       'ell y)))

(defun f88 (x y a c fun )
       (mul
	(inv (mul (factf c y)
		  (factf (sub (add c y) (add a x)) (- x y))
		  (factf (add a (inv 2) x)
			 (sub b (sub x (sub a (inv 2)))))))
	(power 'ell (sub 1 c))
	(power (sub 1 'ell)(sub (add y c) (add a (inv 2))))
	($diff (mul (power 'ell (add a x))
		    (power (sub 1 'ell)(mul -1 a))
		    ($diff (mul (power 'ell (sub c (sub x (sub (inv 2) (mul a 2))))))
			   (power (sub 1 'ell) (sub (add a x b)(sub c y)))
				($diff (mul (power 'ell (sub b  1 ))
					    
					    fun)
				       'ell (sub b (sub a (sub (x (inv 2))))))
				'ell (- x y)))
	       'ell y)))



(DEFUN SIMPR2F1
       (L1 L2)
       ((LAMBDA (INL1P INL1BP INL2P)
		(COND (INL2P (COND ((AND INL1P INL1BP)
				    (derivint (- (car l1) 1)
					      (- (cadr l1)
						 (car l1))
					      (- (- (car l2)
						    (cadr l1))
						 1)))
				   (INL1P (GEREDno2 (CADR L1)
						    (CAR L1)
						    (CAR L2)))
				   (INL1BP (GEREDno2 (CAR L1)
						     (CADR L1)
						     (CAR L2)))
				   (T 'FAIL1)))
		      (INL1P (COND (INL1BP 'D) (T 'C)))
		      ((EQ (CAAAR L1) 'RAT)
		       (COND (INL1BP 'C) (T 'D)))
		      (T 'FAILG)))
	(MAXIMA-INTEGERP (CAR L1))
	(MAXIMA-INTEGERP (CADR L1))
	(MAXIMA-INTEGERP (CAR L2))))
(DEFUN GEREDno1
       (L1 L2)
       (COND ((AND (GREATERP (CAR L2)(CAR L1))
		   (GREATERP (CAR L2)(CADR L1)))
	      (GEREDF (CAR L1)(CADR L1)(CAR L2)))
	     (T (GERED1 L1 L2 'HGFSIMP))))
(DEFUN GEREDno2
       (A B C)
       (COND ((GREATERP C B)(GEREDF B A C))(T (GERED2 A B C))))
(defun derivint
       (n m l)(subst var 'psey
       (mul (power -1 m)
	    (factorial (+ n m l 1))
	    (inv (factorial n))
	    (inv (factorial l))
	    (inv (factorial (+ n m)))
	    (inv (factorial (+ m l)))
	    ($diff  (mul (power (sub 1 'psey) (+ m l))
			 ($diff (mul (power  'psey  -1)
				     -1
				     ($log (sub 1 'psey)))
				'psey
				l))
		    'psey
		    (+ n m)))))



(defun hyp-cos
       (a b c)
       (prog (a2 a1 z1)
	     (setq a1 (div (sub (add a b) (div 1 2)) 2))
	     (setq z1 (sub 1 var))
	     (setq a2 (mul c (inv 2)))
	     (cond ((equal (sub (add a b) (div 1 2)) c)
		    (return (mul (power 2 (sub (mul a1 2) 1))
				 (inv (power  z1 (div 1 2)))
				 (power (add 1
					     (power z1
						    (div 1
							 2)))
					(sub 1 (mul 2 a1)))))))
	     (cond ((equal (add 1 (mul 2 a1)) c)
		    (return (mul (power 2 (sub c 1))
				 (power (add 1
					     (power z1
						    (div 1
							 2)))
					(mul -1 (sub c 1)))))))
	     ))

(DEFUN DEGEN2F1
       (A B C)
       (COND ((EQ (QUEST (SUB C B)) '$NEGATIVE)
	      (COND ((EQ (QUEST (SUB C A)) '$NEGATIVE)
		     (GERED1 (LIST A B)(LIST C) 'HGFSIMP))
		    (T (GERED2 A B C))))
	     ((EQ (QUEST (SUB C A)) '$NEGATIVE)(GERED2 B A C))
	     (T (REST-DEGEN A B C))))


(DEFUN REST-DEGEN
       (A B C)
       (PROG(M N L)
	    (COND ((NNI (SETQ M (SUB A 1)))
		   (RETURN (REST-DEGEN-1 A B C M))))
	    (COND ((NI B)(RETURN (REST-DEGEN-2 A B C))))
	    (COND ((AND (NNI (SETQ N (SUB C 1)))
			(NNI (SETQ M (SUB (SUB A N) 1)))
			(NNI (SETQ L (SUB B A)))
			(EQ (SUB (SUB C A) B)
			    (MUL -1 (ADD M M N L 1))))
		   (RETURN (GERED1 (LIST A B)
				   (LIST C)
				   'HGFSIMP))))
	    (RETURN (hyp-DEG B A C))))


(DEFUN REST-DEGEN-1
       (A B C M)
       (PROG(N L)
	    (COND ((AND (NI B)
			(NI (SUB (SUB C A) B))
			(NNI (SUB (SUB C A) 1)))
		   (RETURN (DEG299 A B C))))
	    (COND ((AND (NNI (SETQ N (SUB (SUB C M) 2)))
			(NNI (SETQ L (SUB B C)))
			(EQUAL (SUB (SUB C A) B)
			       (MUL -1 (ADD L M 1))))
		   (RETURN (GERED1 (LIST A B)
				   (LIST C)
				   'HGFSIMP))))
	    (COND ((NNI (SETQ L (SUB (SUB B M) 1)))
		   (RETURN (REST-DEGEN-1A A B C M L))))
	    (RETURN (hyp-DEG B A C))))


(DEFUN REST-DEGEN-1A
       (A B C M L)
       (PROG(N)
	    (COND ((AND (NNI (SETQ N
				   (SUB (SUB (SUB C M) L) 2)))
			(EQUAL (SUB N M)(SUB (SUB C A) B)))
		   (RETURN (DEG2913 A B C))))
	    (COND ((AND (EQUAL C (MUL -1 N))
			(EQUAL (SUB (SUB C A) B)
			       (MUL -1 (ADD M M L N 2))))
		   (RETURN (DEG2918 A B C))))
	    (RETURN (hyp-DEG B A C))))


(DEFUN REST-DEGEN-2
       (A B C)
       (PROG(M L)
	    (COND ((AND (NI C)(NI (SUB (SUB C A) B)))
		   (RETURN (REST-DEGEN-2A A B C))))
	    (COND ((AND (NNI (SETQ M (SUB C 1)))
			(NNI (SETQ L (SUB A C)))
			(NI (SUB (SUB C A) B)))
		   (RETURN (DEG292 A B C))))
	    (RETURN (hyp-DEG B A C))))


(DEFUN REST-DEGEN-2A
       (A B C)
       (PROG()
	    (COND ((NNI (SUB A C))
		   (RETURN (GERED1 (LIST A B)
				   (LIST C)
				   'HGFSIMP))))
	    (COND ((NNI (SUB (SUB C A) 1))
		   (RETURN (DEG2917 A B C))))
	    (RETURN (hyp-DEG B A C))))

(DEFUN QUEST
       (A)
       (COND ((NUMBERP A)(CHECKSIGNTM A))
	     ((EQUAL (CAaR A) 'RAT)(CHECKSIGNTM A))
	     (T NIL)))



(DEFUN NNI(A)(COND ((MAXIMA-INTEGERP A)(NOT (MINUSP A)))))


(DEFUN NI(A)(NOT (MAXIMA-INTEGERP A)))


(DEFUN hyp-DEG
       (A B C)
       (PROG()
	    (COND (FLDEG (SETQ FLDEG NIL)
			 (RETURN (HGFSIMP (LIST A B)
					  (LIST C)
					  VAR))))
	    (SETQ FLDEG T)
	    (RETURN (FPQFORM (LIST A B)(LIST C) VAR))))


(DEFUN DEG2913
       (A B C)
       (MUL (POWER (MUL -1 VAR)(MUL -1 B))
	    (HGFSIMP (LIST (ADD B 1 (MUL -1 C)) B)
		     (LIST (ADD B 1 (MUL -1 A)))
		     (INV VAR))))


(DEFUN DEG2918
       (A B C)
       (MUL (POWER VAR (SUB 1 C))
	    (POWER (SUB 1 VAR)(ADD C (MUL -1 A)(MUL -1 B)))
	    (HGFSIMP (LIST (SUB 1 A)(SUB 1 B))
		     (LIST (SUB 2 C))
		     VAR)))


(DEFUN DEG2917
       (A B C)
       (MUL (POWER VAR (SUB 1 C))
	    (HGFSIMP (LIST (ADD A 1 (MUL -1 C))
			   (ADD B 1 (MUL -1 C)))
		     (LIST (SUB 2 C))
		     VAR)))


(DEFUN DEG299
       (A B C)
       (MUL (POWER (MUL -1 VAR)(MUL -1 A))
	    (HGFSIMP (LIST A (ADD A 1 (MUL -1 C)))
		     (LIST (ADD A 1 (MUL -1 B)))
		     (INV VAR))))


(DEFUN LEGFUN                      
       (A B C)			   
       (PROG(1-C A-B C-A-B INV2)
	    (SETQ 1-C
		  (SUB 1 C)
		  A-B
		  (SUB A B)
		  C-A-B
		  (SUB (SUB C A) B)
		  INV2
		  (INV 2))
	    (COND ((EQUAL A-B INV2)   
		   (RETURN (GERED1 (LIST A B)(LIST C) 'LEGF24))))
	    (COND ((EQUAL A-B (MUL -1 INV2))
		   (RETURN (LEGF24 (LIST A B)(LIST C) VAR))))
	    (COND ((EQUAL C-A-B INV2)
		   (RETURN (LEGF20 (LIST A B)(LIST C) VAR))))
	    (COND ((EQUAL C-A-B (MUL -1 INV2))
		   (RETURN (GERED1 (LIST A B)(LIST C) 'LEGF20))))
	    (COND ((EQUAL 1-C A-B)
		   (RETURN (LEGF16 (LIST A B)(LIST C) VAR))))
	    (COND ((EQUAL 1-C (MUL -1 A-B))
		   (RETURN (GERED1 (LIST A B)(LIST C) 'LEGF16))))
	    (COND ((EQUAL 1-C C-A-B)
		   (RETURN (GERED1 (LIST A B)(LIST C) 'LEGF14))))
	    (COND ((EQUAL 1-C (MUL -1 C-A-B))
		   (RETURN (LEGF14 (LIST A B)(LIST C) VAR))))
	    (COND ((EQUAL A-B (MUL -1 C-A-B))
		   (RETURN (LEGF36 (LIST A B)(LIST C) VAR))))
	    (COND ((OR (EQUAL 1-C INV2)
		       (EQUAL 1-C (MUL -1 INV2)))
		   (RETURN (LEGPOL A B C))))
	    (COND ((EQUAL A-B C-A-B)
		   (RETURN 'LEGENDRE-FUNCT-TO-BE-DISCOVERED)))
	    (RETURN NIL)))



(DEFUN LEGF20
       (L1 L2 VAR)
       (PROG(M N B C)
	    (SETQ B (CADR L1) C (CAR L2))
	    (SETQ M (SUB 1 C) N (MUL -1 (ADD B B M)))
	    (RETURN (MUL (LF N M)
			 (LEGEN N
				M
				(POWER (SUB 1 VAR) (INV 2))
				'$P)))))


(DEFUN LEGF24
       (L1 L2 VAR)
       (PROG(M N A C)
	    (SETQ A
		  (CAR L1)
		  C
		  (CAR L2)
		  M
		  (SUB 1 C)
		  N
		  (MUL -1 (ADD A A M)))
	    (RETURN (MUL (LF N M)
			 (POWER VAR (ADD N M))
			 (LEGEN N
				M
				(INV (POWER (SUB 1 VAR)
					    (INV 2)))
				'$P)))))


(DEFUN LEGF16
       (L1 L2 VAR)
       (PROG(M N A C)
	    (SETQ A (CAR L1) C (CAR L2) M (SUB 1 C) N (MUL -1 A))
	    (RETURN (MUL (POWER 2 (MUL -1 N))
			 (POWER (SUB VAR 1)(DIV M -2))
			 (INV (GM (SUB 1 M)))
			 (POWER (ADD VAR 1)(ADD (DIV M 2) N))
			 (LEGEN N
				M
				(DIV (ADD 1 VAR)(SUB 1 VAR))
				'$P)))))


(DEFUN LF
       (N M)
       (MUL (POWER 2 M)
	    (INV (POWER (SUB (POWER VAR 2) 1)(DIV M 2)))
	    (INV (GM (SUB 1 M)))))


(DEFUN LEGF14
       (L1 L2 VAR)
       (PROG(M N A C b)
	    (SETQ l (s+c (car l1))
		  a (cond ((eq (cdras 'c l) 0) (cdras 'f l))
			  (t (mul -1 (cdras 'f l))))
		  C (CAR L2) M (SUB 1 C)
		  N (mul -1 a))
	    (RETURN (MUL (POWER  (ADD VAR 1)(DIV M 2))
			 (POWER (SUB VAR 1)(DIV M -2))
			 (INV (GM (SUB 1 M)))
			 (LEGEN N M (SUB 1 (MUL 2 VAR)) '$P)))))


(DEFUN LEGF36
       (L1 L2 VAR)
       (PROG(N M A B)
	    (SETQ A (CAR L1) B (CADR L1) N (SUB B 1) M (SUB B A))
	    (RETURN (MUL (POWER 2 N)
			 (GM (ADD 1 N))
			 (GM (ADD 1 N M))
			 (POWER (ADD VAR 1)
				(ADD (DIV M 2)(MUL -1 N) -1))
			 (POWER (SUB VAR 1)(DIV M -2))
			 (INV (GM (ADD 2 N N)))
			 (POWER '$%E (MUL -1 '$%I M '$%PI))
			 (LEGEN N M (DIV (SUB 2 VAR) VAR) '$Q)))))


(DEFUN LEGEN
       (N M X PQ)
       (LIST '(MQAPPLY)
	     (LIST (COND ((EQ PQ '$Q) '($%Q ARRAY))
			 (T '($%P ARRAY)))
		   N
		   M)
	     X))


(DEFUN LEGPOL
       (A B C)
       (PROG(L V)
	    (COND ((NOT (hyp-NEGP-IN-L (LIST A)))
		   (RETURN 'FAIL-1-IN-C-1-CASE)))
	    (SETQ L (VFVP (DIV (ADD B A) 2)))
	    (SETQ V (CDR (ZL-ASSOC 'V L)))
	    (COND ((AND (EQUAL V '((RAT SIMP) 1 2))(EQUAL C 1))
		   (RETURN (LEGENPOL (MUL -1 A)
				     (SUB 1 (MUL 2 VAR))))))
	    (COND ((AND (EQUAL C '((RAT SIMP) 1 2))
			(EQUAL (SUB B A) '((RAT SIMP) 1 2)))
		   (RETURN (MUL (FACTORIAL (MUL -1 A))
				(POWER 2 A)
				(MULTAUG (INV 2) (MUL -1 A))
				(LEGENPOL (MUL -1 A)
					  (POWER
					   VAR
					   (DIV -1 2)))))))
	    (return nil)))


       
(DEFUN MULTAUG
       (A N)
       (COND ((ZEROP N) 1)(T (MUL A (MULTAUG (ADD A 1)(SUB1 N))))))


(DEFUN GERED1
       (L1 L2 SIMPFLG)
       (MUL (POWER (SUB 1 VAR)
		   (ADD (CAR L2)
			(MUL -1 (CAR L1))
			(MUL -1 (CADR L1))))
	    (funcall SIMPFLG
		     (LIST (SUB (CAR L2) (CAR L1))
			   (SUB (CAR L2) (CADR L1)))
		     L2
		     VAR)))





(DEFUN GERED2
       (A B C)
       (MUL (POWER (SUB 1 VAR)(MUL -1 A))
	    (HGFSIMP (LIST A (SUB C B))
		     (LIST C)
		     (DIV VAR (SUB VAR 1)))))


(DEFUN GEREDF
       (A B C)
       (ADD (DIV (MUL (GM C)
		      (GM (ADD C (MUL -1 A)(MUL -1 B)))
		      (POWER VAR (MUL -1 A))
		      (HGFSIMP (LIST A (ADD A 1 (MUL -1 C)))
			       (LIST (ADD A B (MUL -1 C) 1))
			       (SUB 1 (DIV 1 VAR))))
		 (MUL (GM (SUB C A))(GM (SUB C B))))
	    (DIV (MUL (GM C)
		      (GM (ADD A B (MUL -1 C)))
		      (POWER (SUB 1 VAR)
			     (ADD C (MUL -1 A)(MUL -1 B)))
		      (POWER VAR (SUB A C))
		      (HGFSIMP (LIST (SUB C A)(SUB 1 A))
			       (LIST (ADD C
					  (MUL -1 A)
					  (MUL -1 B)
					  1))
			       (SUB 1 (DIV 1 VAR))))
		 (MUL (GM A)(GM B)))))



(DEFUN TRIG-LOG
       (L1 L2)
       (COND ((EQUAL (SIMPLIFYA (CAR L2) NIL) '((RAT SIMP) 3 2))
	      (TRIG-LOG-3 L1 L2))
	     ((EQUAL (SIMPLIFYA (CAR L2) NIL) '((RAT SIMP) 1 2))
	      (TRIG-LOG-1 L1 L2))
	     (T nil)))


(DEFUN TRIG-LOG-3
       (L1 L2)
       (COND ((AND (OR (equal (car l1) 1) (equal (cadr l1) 1))
		   (OR (equal (car l1) (div 1 2))
		       (equal (cadr l1) (div 1 2))))
	      (TRIG-LOG-3-EXEC L1 L2))
	     ((and (equal (car l1) (cadr l1))
		   (or (equal 1 (car l1))
		       (equal (div 1 2) (car l1))))
	      (trig-log-3a-exec l1 l2))
	     ((or(equal (add (car l1) (cadr l1)) 1)
		 (equal (add (car l1) (cadr l1)) 2))
	      (trig-sin l1 l2))
	     ((or (equal (sub (car l1) (cadr l1)) (div 1 2))
		  (equal (sub (cadr l1) (car l1)) (div 1 2)))
	      (trig-3 l1 l2))
	     (T nil)))

(defun trig-3
       (l1 l2)
       (prog (a z)
	     (return (mul (inv (setq z (power var (div 1 2))))
			  (inv 2)
			  (inv (setq a
				     (sub 1
					  (sub (add (car l1)
						    (cadr l1))
					       (div 1 2)))))
			  (sub (power (add 1 z) a)
			       (power (sub 1 z) a))))))
(defun trig-sin
       (l1 l2)
       (prog (a1 z1 a b c)
	     (setq a (car l1) b (cadr l1) c (car l2))
	     (cond ((equal (add a b) 1)
		    (return (mul (inv (mul (mul -1 (sub a b))
					   ($sin ($asin ($sqrt var)))))
				 ($sin (mul (mul -1
						 (sub a b))
					    ($asin ($sqrt var)))))))
		   ((eq (add a b) 2)
		    (return (mul ($sin (mul (setq z1
						  ($asin ($sqrt
							  var)))
					    (setq a1
						  (mul -1
						       (sub a
							    b)))))
				 (inv (mul a1
					   ($sin z1)
					   ($cos z1)))))))
	     (return nil)))

;Generates atan if arg positive else log
(defun trig-log-3-exec
       (l1 l2)
       (prog (z)
	     (cond ((equal (checksigntm var) '$positive)
		    (return (mul (power (setq z
					      (power var
						     (div 1
							  2)))
					-1)
				 (inv 2)
				 ($log (div (add 1 z)
					    (sub 1 z))))))
		   ((equal (checksigntm var) '$negative)
		    (return (mul (power (setq z
					      (power (mul -1
							  var)
						     (div 1
							  2)))
					-1)
				 ($atan z)))))))

(defun trig-log-1
       (l1 l2)
       (prog (a b c z1 $exponentialize)
	     
	     (setq a (car l1) b (cadr l1) c (car l2))
	     (cond ((equal (add a b) 0)
		    (cond ((equal (checksigntm var) '$positive)
			   (return ($cos (mul (mul 2 a)
					      ($asin (power var
							    (inv 2)))))))
			  (t (return (div (add (power (add (setq
							    z1
							    (power
							     (add
							      (mul
							       var
							       -1)
							      1)
							     (inv 2)))
							   var)
						      (mul 2 a))
					       (power (sub z1 var)
						      (mul 2 a)))
					  2)))
			  ((equal (add a b) 1)
			   (return (mul (inv ($cos (setq z1
							 ($asin
							  ($sqrt
							   var)))))
					($cos (mul z1 (sub a b))))))
			  ((or (equal (sub a b) (inv 2))
			       (equal (sub a b) (inv -2)))
			   (return (add (div (power (add 1
							 (setq
							  z1
							  (power
							   var
							   (inv
							    2))))
						    (mul -2 a))
					     2)
					(div (power (sub 1 z1)
						    (mul -2 a))
					     2)))))))
	     
	     (return nil)))



(DEFUN TRIG-LOG-1 (A B)			;; 2F1's with C = 1/2
  (LET (X Z $EXPONENTIALIZE)		;; 15.1.17, 11, 18, 12, 9, and 19
       (setq a (car l1) b (cadr l1))
       (COND ((=0 (M+T A B))
	      (COND ((EQUAL (CHECKSIGNTM VAR) '$POSITIVE)
		     (MCOS (M*T 2. A (MASIN (MSQRT VAR)))))
		    ((EQUAL (CHECKSIGNTM VAR) '$NEGATIVE)
		     (M*T 1//2
			  (M+T (M^T (M+T (SETQ X (MSQRT (M-T 1. VAR)))
					 (SETQ Z (MSQRT (M-T VAR))))
				    (SETQ B (M*T 2. B)))
			       (M^T (M-T X Z) B))))
		    (T ())))
	     ((EQUAL (M+T A B) 1.)
	      (COND ((EQUAL (CHECKSIGNTM VAR) '$POSITIVE)
		     (M//T (MCOS (M*T (M-T A B) (SETQ Z (MASIN (MSQRT VAR)))))
			   (MCOS Z)))
		    ((EQUAL (CHECKSIGNTM VAR) '$NEGATIVE)
		     (M*T 1//2 (M//T (SETQ X (MSQRT (M-T 1. VAR))))
			  (M+T (M^T (M+T X (SETQ Z (MSQRT (M-T VAR))))
				    (SETQ B (M-T A B)))
			       (M^T (M-T X Z) B))))
		    (T ())))
	     ((=1//2 (MABS (M-T B A)))
	      (COND ((EQUAL (CHECKSIGNTM VAR) '$POSITIVE)
		     (M*T 1//2
			  (M+T (M^T (M1+T (SETQ Z (MSQRT VAR)))
				    (SETQ B (M-T 1//2 (M+T A B))))
			       (M^T (M-T 1. Z) B))))
		    ((EQUAL (CHECKSIGNTM VAR) '$NEGATIVE)
		     (M*T (M^T (MCOS (SETQ Z (MATAN (MSQRT (M-T VAR)))))
			       (SETQ B (M+T A B -1//2)))
			  (MCOS (M*T B Z))))
		    (T ())))
	     (T ()))))


; List L contains two elements first the numerator parameter that
;exceeds the denumerator one and is called "C", second
;the difference of the two parameters which is called "M". 

(DEFUN DIFFINTPROP-GEN-EXEC (L L1 L2) 
       (PROG (C M POLY CONSTFACT ) 
	     (SETQ C (CAR L) 
		   M (CADR L) 
		   L1 (ZL-DELETE C L1 1.) 
		   C (SUB C M)
		   L2 (ZL-DELETE C L2 1.) 
		   POLY ($EXPAND (CONSTRPOLY C M 'AVGOUSTIS)) 
		   CONSTFACT (CREATECONSTFACT C M))
	     (RETURN (YANMULT CONSTFACT
			      (DIFFINTPROP-EXEC POLY L1 L2))))) 

(DEFUN CONSTRPOLY (C M K) 
       (COND ((ZEROP M) 1.)
	     (T (MUL (ADD C K (SUB1 M)) (CONSTRPOLY C (SUB1 M) K))))) 

(DEFUN CREATECONSTFACT (C M) 
       (COND ((ZEROP M) 1.)
	     (T (MUL (INV (ADD C (SUB1 M)))
		     (CREATECONSTFACT C (SUB1 M)))))) 

(DEFUN DIFFINTPROP-EXEC (POLY L1 L2) 
       (DISTRDIFFINTPROP (CREATECOEFPOWLIST-EXEC POLY) L1 L2)) 

(DEFUN DISTRDIFFINTPROP (L L1 L2) 
       (COND ((NULL L) 0.)
	     (T (ADD (YANMULT ($FACTOR (CADAR L))
			      (DIFFINTPROP (CAAR L) L1 L2))
		     (DISTRDIFFINTPROP (CDR L) L1 L2))))) 

(DEFUN DIFFINTPROP (POW L1 L2) 
       (COND ((ZEROP POW) (HGFSIMP L1 L2 VAR))
	     ((EQUAL POW 1.)
	      (YANMULT (MUL (DIV (MULTPL L1) (MULTPL L2)) VAR)
		       (HGFSIMP (INCR1 L1) (INCR1 L2) VAR)))
	     (T (SEARCHADDSERIESLIST POW L1 L2)))) 

(DEFUN SEARCHADDSERIESLIST (POW L1 L2) 
       (PROG (SERIES RES) 
	     (COND ((SETQ SERIES (SEARCHSERIESLISTP SERIESLIST POW))
		    (RETURN (EVAL SERIES))))
	     (SETQ 
	      SERIESLIST
	      (APPEND
	       SERIESLIST
	       (LIST
		(LIST
		 POW
		 (SETQ RES
		       '(YANMULT (MUL (DIV (MULTPL L1) (MULTPL L2))
				      VAR)
				 (DIFFINTPROPRECURSE (SUB1 POW)
						     (INCR1 L1)
						     (INCR1 L2))))))))
	     (RETURN (EVAL RES)))) 

(DEFUN DIFFINTPROPRECURSE (POW L1 L2) 
       (PROG (POLY) 
	     (SETQ POLY
		   ($EXPAND (POWER (ADD 'AVGOUSTIS 1.) POW)))
	     (RETURN (DIFFINTPROP-EXEC POLY L1 L2)))) 

(DEFUN MULTPL (L) 
       (COND ((NULL L) 1.) (T (MUL (CAR L) (MULTPL (CDR L)))))) 

(DEFUN CREATECOEFPOWLIST-EXEC (POLY) 
       (PROG (HP CONSTER) 
	     (SETQ CONSTER (CONSTERMINIT POLY 'AVGOUSTIS) 
		   HP ($HIPOW POLY 'AVGOUSTIS))
	     (RETURN (APPEND (LIST (LIST 0. CONSTER))
			     (CREATECOEFPOWLIST POLY HP))))) 

(DEFUN CREATECOEFPOWLIST (POLY HP) 
       (COND ((EQUAL HP 1.)
	      (LIST (LIST 1. ($COEFF POLY 'AVGOUSTIS))))
	     (T (APPEND (CREATECOEFPOWLIST POLY (SUB1 HP))
			(LIST (LIST HP
				    ($COEFF POLY
					    (POWER 'AVGOUSTIS
						   HP)))))))) 

(DEFUN CONSTERMINIT (FUN VAR) 
       (COND ((EQ (CAAR FUN) 'MPLUS) (CONSTERM (CDR FUN) VAR))
	     (T (COND ((FREEVAR FUN) FUN) (T 0.))))) 

(DEFUN SEARCHSERIESLISTP (SERIESLIST POW) 
       (COND ((NULL SERIESLIST) NIL)
	     ((EQUAL (CAAR SERIESLIST) POW) (CADAR SERIESLIST))
	     (T (SEARCHSERIESLISTP (CDR SERIESLIST) POW)))) 

(DEFUN YANMULT (A B) 
       (COND ((EQ (CAAR B) 'MPLUS) (YANMUL A (CDR B)))
	     (T (MUL A B)))) 

(DEFUN YANMUL (A B) 
       (COND ((NULL B) 0.)
	     (T (ADD (MUL A (CAR B)) (YANMUL A (CDR B)))))) 


(DEFUN FREEVARPAR(EXP)(COND ((FREEVAR EXP)(FREEPAR EXP))(T NIL)))

(DECLARE-top (SPECIAL serieslist VAR PAR ZEROSIGNTEST PRODUCTCASE))
(setq par '$P)
(DEFUN FREEVAR (A) 
       (COND ((ATOM A) (NOT (EQ A VAR)))
	     ((ALIKE1 A VAR)NIL)
	     ((AND (NOT (ATOM (CAR A)))
		   (MEMQ 'ARRAY (CDAR A)))
	      (COND ((FREEVAR (CDR A)) T)
		    (T (PRINC 'VARIABLE-OF-INTEGRATION-APPEARED-IN-SUBSCRIPT)
		       (ERR))))
	     (T (AND (FREEVAR (CAR A)) (FREEVAR (CDR A))))))

(DEFUN FREEPAR
       (EXP)
       (COND ((ATOM EXP)(NOT (EQ EXP PAR)))
	     (T (AND (FREEPAR (CAR EXP))(FREEPAR (CDR EXP))))))

(DEFUN HASPAR(EXP)(COND ((FREEPAR EXP) NIL)(T T)))

(DEFUN CONFL
       (L1 L2 VAR)
       (PROG(A C A-C K M z)
	    (SETQ A (CAR L1) C (CAR L2))
	    (COND ((EQUAL C (ADD A A))

		   (RETURN (MUL (POWER '$%E (setq z (DIV VAR 2)))
				(bestrig (add a (inv 2))
					 (div (mul z z) 4))))))
					 
		
	    (COND ((NOT (MAXIMA-INTEGERP (SETQ A-C (SUB A C))))
		   (GO KUMCHECK)))
	    (COND ((MINUSP A-C)(RETURN (ERFGAMMARED A C VAR))))
	    (RETURN (KUMMER L1 L2))
	    KUMCHECK
	    (COND ((MAXIMA-INTEGERP A)(RETURN (KUMMER L1 L2))))
	    (SETQ M
		  (DIV (SUB C 1) 2)
		  K
		  (ADD (INV 2) M (MUL -1 A)))
	    (RETURN (MUL (POWER VAR (MUL -1 (ADD (INV 2) M)))
			 (POWER '$%E (DIV VAR 2))
			 (WHITFUN K M VAR)))))
(DEFUN HYPREDERF
       (X)
       (PROG()
	    (SETQ X (MUL '$%I (POWER X (INV 2))))
	    (RETURN (MUL (POWER '$%PI (INV 2))
			 (INV 2)
			 (INV X)
			 (LIST '(%ERF) X)))))
(DEFUN ERFGAMMARED
       (A C Z)
       (COND ((AND (NUMP A)(NUMP C))(ERFGAMNUMRED A C Z))
	     (T (GAMMAREDS A C Z))))
(DEFUN GAMMAREDS
       (A C Z)
       (PROG(M NUMPROD RESULT COUNT ATEMP)
	    (SETQ M (SUB C A))
	    (COND ((EQ M 1)(RETURN (HYPREDINCGM A Z))))
	    (SETQ NUMPROD
		  (PROD A M)
		  COUNT
		  2
		  ATEMP
		  A
		  RESULT
		  (SUB (MUL 2
			    NUMPROD
			    (INV ATEMP)
			    (HYPREDINCGM ATEMP Z))
		       (MUL 2
			    NUMPROD
			    (INV (SETQ ATEMP (ADD ATEMP 1)))
			    (HYPREDINCGM ATEMP Z))))
	    LOOP
	    (COND ((EQ COUNT M)(RETURN RESULT)))
	    (SETQ COUNT
		  (ADD1 COUNT)
		  ATEMP
		  (ADD ATEMP 1)
		  RESULT
		  (ADD RESULT
		       (MUL (POWER -1 COUNT)
			    (INV (FACTORIAL (SUB M
						 (SUB1 COUNT))))
			    NUMPROD
			    (INV ATEMP)
			    (HYPREDINCGM ATEMP Z))))
	    (GO LOOP)))
(DEFUN HYPREDINCGM
       (A Z)
       (PROG()
	    (SETQ Z (MUL -1 Z))
	    (RETURN (MUL A
			 (POWER Z (MUL -1 A))
			 (LIST '($%GAMMAGREEK) A Z)))))
(DEFUN PROD
       (A M)
       (COND ((EQ M 2) (MUL A (ADD A 1)))
	     (T (MUL (ADD A (SUB1 M))(PROD A (SUB1 M))))))
(DEFUN ERFGAMNUMRED
       (A C Z)
       (COND ((MAXIMA-INTEGERP (SUB C (INV 2)))(ERFRED A C Z))
	     (T (GAMMAREDS A C Z))))
(DEFUN ERFRED
       (A C Z)
       (PROG(N M)
	    (SETQ N (SUB A (INV 2)) M (SUB C (DIV 3 2)))
	    (COND ((NOT (OR (GREATERP N M)(MINUSP N)))
		   (RETURN (THno33 N M Z))))
	    (COND ((AND (MINUSP N)(MINUSP M))
		   (RETURN (THno35 (MUL -1 N)(MUL -1 M) Z))))
	    (COND ((AND (MINUSP N)(PLUSP M))
		   (RETURN (THno34 (MUL -1 N) M Z))))
	    (RETURN (GAMMAREDS (ADD N (INV 2))
			       (ADD M (DIV 3 2))
			       Z))))
(DEFUN THno33
       (N M X)
       ((LAMBDA(M-N)
	       (SUBST X
		      'YANNIS
		      (MUL (DIV (MUL (POWER -1 M-N)
				     (FCTRL (DIV 3 2) M-N)
				     (FCTRL (ADD M-N
						 (DIV 3 2))
					    N))
				(MUL (FCTRL 1 M-N)
				     (FCTRL (inv 2) N)))
			   (MEVAL (LIST '($DIFF)
					(MUL (POWER '$%E
						    'YANNIS)
					     (MEVAL (LIST '($DIFF)
							  (MUL
							   (POWER
							    '$%E
							    (MUL
							     -1
							     'YANNIS))
							   (HYPREDERF
							    'YANNIS))
							  'YANNIS
							  M-N)))
					'YANNIS
					N)))))
	(SUB M N)))
(DEFUN THno34
       (N M X)
       (SUBST X
	      'YANNIS
	      (MUL (POWER -1 M)
		   (DIV (MUL (FCTRL (DIV 3 2) M)
			     (POWER '$%E 'YANNIS))
			(MUL (FCTRL 1 M)
			     (FCTRL (ADD1 M) N)
			     (POWER 'YANNIS M)))
		   (MEVAL (LIST '($DIFF)
				(MUL (POWER 'YANNIS
					    (PLUS M N))
				     (MEVAL (LIST '($DIFF)
						  (MUL (POWER '$%E
							      (MUL
							       -1
							       'YANNIS))
						       (HYPREDERF 'YANNIS))
						  'YANNIS
						  M)))
				'YANNIS
				N)))))
(DEFUN THno35
       (N M X)
       (SUBST X
	      'YANNIS
	      (MUL (DIV (POWER 'YANNIS (SUB M (inv 2)))
			(MUL (POWER -1 (TIMES -1 M))
			     (FCTRL 1 N)
			     (FCTRL (INV -2) M)))
		   (MEVAL (LIST '($DIFF)
				(MUL (POWER 'YANNIS (inv 2))
				     (POWER '$%E 'YANNIS)
				     (MEVAL (LIST '($DIFF)
						  (MUL (POWER '$%E
							      (MUL
							       -1
							       'YANNIS))
						       (POWER 'YANNIS
							      N)
						       (HYPREDERF 'YANNIS))
						  'YANNIS
						  N)))
				'YANNIS
				M)))))
(DEFUN FCTRL
       (A N)
       (COND ((ZEROP N) 1)
	     ((one n) a)
	     (T (MUL (ADD A (SUB1 N))(FCTRL A (SUB1 N))))))

(defun one (x)(equal x 1))



(DEFUN CHECKSIGNTM			
       (EXPR)				
       (PROG (ASLIST QUEST ZEROSIGNTEST PRODUCTCASE)	
	     (SETQ ASLIST CHECKCOEFSIGNLIST)
	     (COND ((ATOM EXPR) (GO LOOP)))
	     (COND ((EQ (CAAR EXPR) 'MTIMES)
		    (SETQ PRODUCTCASE T)))
	     LOOP
	     (COND ((NULL ASLIST)
		    (SETQ CHECKCOEFSIGNLIST
			  (APPEND CHECKCOEFSIGNLIST
				  (LIST (CONS
					 EXPR
					 (LIST
					  (SETQ
					   QUEST
					   (CHECKFLAGANDACT
					    EXPR)))))))
		    (RETURN QUEST)))
	     (COND ((EQUAL (CAAR ASLIST) EXPR)
		    (RETURN (CADAR ASLIST))))
	     (SETQ ASLIST (CDR ASLIST))
	     (GO LOOP))) 

(DEFUN CHECKFLAGANDACT
       (EXPR)
       (COND (PRODUCTCASE (SETQ PRODUCTCASE NIL)
			  (FINDSIGNOFTHEIRPRODUCT (FINDSIGNOFACTORS
						   (CDR EXPR))))
	     (T (ASKSIGN ($REALPART EXPR))))) 

(DEFUN FINDSIGNOFACTORS
       (LISTOFACTORS)
       (COND ((NULL LISTOFACTORS) NIL)
	     ((EQ ZEROSIGNTEST '$ZERO) '$ZERO)
	     (T (APPEND (LIST (SETQ ZEROSIGNTEST
				    (CHECKSIGNTM (CAR
						  LISTOFACTORS))))
			(FINDSIGNOFACTORS (CDR LISTOFACTORS)))))) 

(DEFUN FINDSIGNOFTHEIRPRODUCT
       (LIST)
       (PROG (SIGN)
	     (COND ((EQ LIST '$ZERO) (RETURN '$ZERO)))
	     (SETQ SIGN '$POSITIVE)
	     LOOP
	     (COND ((NULL LIST) (RETURN SIGN)))
	     (COND ((EQ (CAR LIST) '$POSITIVE)
		    (SETQ LIST (CDR LIST))
		    (GO LOOP)))
	     (COND ((EQ (CAR LIST) '$NEGATIVE)
		    (SETQ SIGN
			  (CHANGESIGN SIGN)
			  LIST
			  (CDR LIST))
		    (GO LOOP)))
	     (RETURN '$ZERO))) 

(DEFUN CHANGESIGN
       (SIGN)
       (COND ((EQ SIGN '$POSITIVE) '$NEGATIVE) (T '$POSITIVE))) 


(SETQ PAR '$P)                           

(DEFUN VFVP(EXP)(M2 EXP '(V FREEVARPAR) NIL))


(DEFUN D*U
       (EXP)
       (M2 EXP
	   '((MTIMES)((COEFFTT)(D FREEPAR))((COEFFTT)(U HASPAR)))
	   NIL))

(DEFUN FPQFORM
       (L1 L2 ARG)
       (LIST '(MQAPPLY)
	     (LIST '($%F ARRAY)(LENGTH L1)(LENGTH L2))
	     (APPEND (LIST '(MLIST)) L1)
	     (APPEND (LIST '(MLIST)) L2)
	     ARG))



(defun splitpfq
       (l l1 l2)
       (prog(result prodnum proden count k a1 b1)
	    (setq result
		  0
		  prodnum
		  1
		  proden
		  1
		  count
		  0
		  k
		  (cadr l)
		  a1
		  (car l)
		  b1
		  (sub a1 k))
	    (setq l1
		  (zl-delete a1 l1 1)
		  l2
		  (zl-delete b1 l2 1)
		  result
		  (hgfsimp l1 l2 var))
	    loop
	    (cond ((eq count k) (return result)))
	    (setq count
		  (add1 count)
		  prodnum
		  (mul prodnum (mull l1))
		  proden
		  (mul proden (mull l2))
		  result
		  (add result
		       (mul (combin k count)
			    (div prodnum proden)
			    (power var count)
			    (hgfsimp (setq l1 (incr1 l1))
				     (setq l2 (incr1 l2))
				     var))))
	    (go loop)))

(defun combin
       (k count)
       (div (factorial k)
	    (mul (factorial count)(factorial (sub k count)))))


;Algor. II from thesis:minimizes differentiations
(defun ALGII(a b c)
       (prog (m n ap con sym m+n)
	     (cond ((not (setq sym (cdras 'f (s+c a))))
		    (setq sym 0)))
	     (setq  con (sub a sym))
	     (setq ap sym)
	     (setq m+n (add a b))
	     (setq m ($entier con))
	     (cond ((minusp m)(add1 m)))
	     (setq ap (add (sub con m) ap))
	     (setq n (add b ap))
	     (cond ((and (minusp (mul n m))(greaterp (abs m) (abs n)))
		    (return (list ap (sub ap n) m+n))))
	     (return  (list ap (add ap m) m+n))))
			    
			   



;Algor. 2F1-RL from thesis:step 4:dispatch on a+m,-a+n,1/2+l cases
(defun step4
       (a b c)
       (prog (aprime m n $ratsimpexponens $ratprint newf)
	     (setq alglist
		   (algii a b c)
		   aprime
		   (cadr alglist)
		   m
		   (caddr alglist)
		   n
		   (sub c (inv 2)))
	     (setq $ratsimpexponens $true $ratprint $false)
	     (setq newf
		   ($ratsimp (subst aprime
				    'psa
				    (power (add (inv 2)
						(mul (power (sub
							     1
							     var)
							    (inv
							     2))
						     (inv 2)))
					   (sub 1
						(mul 2 'psa))))))
	     (return (subst var 'ell
			    (algiii (subst 'ell var newf)
				    m n aprime)))))

;Pattern match for s(ymbolic) + c(onstant) in parameter
(DEFUN s+C
       (EXP)
       (M2 EXP
	   '((MPLUS)((COEFFPT)(F nonnump))((COEFFPP)(C $numberp)))
	   NIL))

(defun nonnump (z)
       (cond ((not ($numberp z)) t)
	     (t nil)))

;Algor. III from thesis:determines which Differ. Formula to use
(defun algiii (fun m n aprime)
       (prog (mm nn)
	     (setq mm (abs m) nn (abs n))
	     (cond ((and (nni m) (nni n))
		    (cond ((lessp m n) (return (f81 fun m n aprime)))
			  (t (return (f85 fun mm nn aprime)))))
		   ((and (hyp-negp n) (hyp-negp m))
		    (cond ((greaterp (abs n) (abs m))
			   (return (f86 fun mm nn aprime)))
			  (t (return (f82 fun mm nn aprime)))))
		   ((and (hyp-negp m) (nni n))(return (f83 fun mm nn aprime)))
		   (t (return (f84 fun mm nn aprime))))))

;Factorial function:x*(x+1)*(x+2)...(x+n-1)
(defun factf (x n)
       (cond ((zerop n) 1)
	     (t (mul x (factf (add x 1) (sub n 1))))))

;Formula  #85 from Yannis thesis:finds by differentiating F[2,1](a,b,c,z)
; given F[2,1](a+m,b,c+n,z) where b=-a and c=1/2, n,m integers
(defun f85 (fun m n a)
       (mul (factf (inv 2) n)
	    (inv (power -1 n))
	    (inv (factf (sub (add a m) n) n))
	    (inv (factf (sub (inv 2) (mul a -1)) n))
	    (inv (factf a (- m n)))
	    (power (sub 1 'ell) (sub (sub (add 1 n) m) a))
	    ($diff (mul
		    (power (sub 1 'ell) (sub (add a m) 1))
		    (power 'ell (sub 1 a))
		    ($diff (mul
			    (power 'ell (sub (add a m -1) n))
			    fun) 'ell (- m n))) 'ell n)))

;Used to find negative things that are not integers,eg RAT's	
(defun hyp-negp(x) (cond ((equal (asksign x) '$negative) t)(t nil)))

(defun f81 (fun m n a)
       (mul (factf (add (inv 2) (- n m)) m)
	    (factf (inv 2) (- n m))
	    (inv (power -1 m))
	    (inv (factf a m))
	    (inv (factf (add (inv 2) n (sub a m)) m))
	    (inv (factf (sub (inv 2) a) (- n m)))
	    (inv (factf (add (inv 2) a) (- n m)))
	    (power (sub 1 'ell) (sub 1 a))
	    ($diff (mul 
		    (power (sub 1 'ell) (add a n (inv -2)))
		    ($diff (mul
			    (power (sub 1 'ell) (inv -2))
			    fun) 'ell (- n m))) 'ell m)))

(defun f82
       (fun m n a)
       (mul (inv (factf (sub (inv 2) n) m))
	    ;; Was this both inverse?
	    (inv (factf (sub (add (inv 2) m) n) (- n m)))
	    (power 'ell (add n (inv 2)))
	    (power (sub 1 'ell) (sub (add m (inv 2) a) n))
	    ($diff (mul (power (sub 1 'ell)
			       (sub (sub n a) (inv 2)))
			($diff (mul  (power 'ell (inv -2)) fun)
			       'ell
			       (- n m)))
		   'ell
		   m)))

(defun f83
       (fun m n a)
       (mul (factf (inv 2) n)
	    (inv (factf (sub (inv 2) a) n))
	    (inv (factf (add (sub (inv 2) a) n) m))
	    (inv (factf (add (inv 2) a) n))
	    (power (sub 1 'ell) (add m n (inv 2)))
	    (power 'ell (sub (add (inv 2) a) n))
	    ($diff (mul (power 'ell (sub (sub (+ m n)  a)(inv 2)))
			($diff (mul (power (sub 1 'ell)
					   (inv -2))
				    fun)
			       'ell
			       n))
		   'ell
		   m)))

(defun f84
       (fun m n a)
       (mul (inv (mul (factf a m) (factf (sub (inv 2) n) n)))
	    (power 'ell (sub 1 a))
	    ($diff (mul (power 'ell (sub (add a m n) (inv 2)))
			($diff (mul (power 'ell (inv -2)) fun)
			       'ell
			       n))
		   'ell
		   m)))

(defun f86
       (fun m n a)
       (mul (inv (mul (factf (sub (inv 2) n) n)
		      (factf (sub (inv 2) a) (- m n))))
	    (power 'ell (add n (inv 2)))
	    (power (sub 1 'ell)(add (inv 2) a))
	    ($diff (mul (power 'ell a)
			(power (sub 1 'ell)(sub m a))
			($diff (mul (power 'ell
					   (sub (sub (sub m n) (inv 2)) a))
				    (power (sub 1 'ell)
					   (inv -2))
				    fun) 'ell (- m n)))
			'ell n)))


(eval-when (compile)
(DECLARE-top (unSPECIAL serieslist VAR PAR ZEROSIGNTEST PRODUCTCASE
			fldeg flgkum listcmdiff checkcoefsignlist ))

(declare-top (unspecial fun w b l alglist n  c ))
)