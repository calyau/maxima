;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE2]LINDE1.MC;4
;;; Written on 9/15/1984 22:03:39, from MACSYMA 302
;;; Translated for LPH

;;; TRANSL-AUTOLOAD version NIL
;;; TRANSS version 87 TRANSL version 1157 TRUTIL version 27
;;; TRANS1 version 108 TRANS2 version 39 TRANS3 version 50
;;; TRANS4 version 29 TRANS5 version 26 TRANSF version NIL
;;; TROPER version 15 TRPRED version 6 MTAGS version NIL
;;; MDEFUN version 58 TRANSQ version 88 FCALL version 40
;;; ACALL version 70 TRDATA version 68 MCOMPI version 146
;;; TRMODE version 73 TRHOOK version NIL
(eval-when (compile eval)
      (setq *infile-name-key*
	          (namestring (truename '#.standard-input))))

(eval-when (compile)
   (setq $tr_semicompile 'NIL)
   (setq forms-to-compile-queue ()))

(comment "MAX$DISK:[SHARE2]LINDE1.MC;4")

;;; General declarations required for translated MACSYMA code.

(DECLARE (SPECIAL $GLOBALSOLVE))

(DEFMTRFUN-EXTERNAL ($SOLDIFF $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($IVPSOL $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($EVEC $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($CONSMAT3 $ANY MDEFINE NIL NIL))


(DEFPROP $SOLDIFF T TRANSLATED)

(ADD2LNC '$SOLDIFF $PROPS)

(DEFMTRFUN
  ($SOLDIFF $ANY MDEFINE NIL NIL) ($EQ1 $Y $X $A) NIL
  ((LAMBDA ($EQ2 $A1 $A2 $A3 $A4 $A5)
     NIL
     (SETQ $EQ2 (ADD* (SIMPLIFY (MFUNCTION-CALL $LHS $EQ1))
		      (*MMINUS (SIMPLIFY (MFUNCTION-CALL $RHS $EQ1)))))
     (SETQ
       $A1
       (SIMPLIFY (MFUNCTION-CALL
		   $RATCOEF $EQ2 (SIMPLIFY (LIST '(%DERIVATIVE) $Y $X)))))
     (SETQ $A2 (SIMPLIFY (MFUNCTION-CALL $RATCOEF $EQ2 $Y)))
     (SETQ
       $A3
       (ADD*
	 $EQ2 (*MMINUS (MUL* $A1 (SIMPLIFY (LIST '(%DERIVATIVE) $Y $X))))
	 (MUL* (*MMINUS $A2) $Y)))
     (SETQ $A4 (SIMPLIFY (MFUNCTION-CALL $INTEGRATE (DIV $A2 $A1) $X)))
     (SETQ
       $A5 (SIMPLIFY
	     (MFUNCTION-CALL
	       $INTEGRATE (DIV (MUL* (SIMPLIFY ($EXP $A4)) $A3) $A1) $X)))
     (ADD* (DIV '$CONST1 (SIMPLIFY ($EXP $A4)))
	   (*MMINUS (DIV $A5 (SIMPLIFY ($EXP $A4))))))
   '$EQ2 '$A1 '$A2 '$A3 '$A4 '$A5))

(DEFPROP $IVPSOL T TRANSLATED)

(ADD2LNC '$IVPSOL $PROPS)

(DEFMTRFUN
 ($IVPSOL $ANY MDEFINE NIL NIL) ($DIFFEQ $Y $X $A $BCEQ) NIL
 ((LAMBDA ($Z $DERY $DYA $YA $CO1)
   NIL
   (PROG ()
    (SETQ $Z (SIMPLIFY ($SOLDIFF $DIFFEQ $Y $X $A)))
    (SETQ $DERY (SIMPLIFY (MFUNCTION-CALL $DIFF $Z $X 1)))
    (SETQ $DYA (SIMPLIFY (MFUNCTION-CALL $SUBSTITUTE $A $X $DERY)))
    (SETQ $YA (SIMPLIFY (MFUNCTION-CALL $SUBSTITUTE $A $X $Z)))
    (SETQ $CO1
	  (SIMPLIFY
	    (MFUNCTION-CALL
	      $SOLVE
	      (SIMPLIFY
		(MFUNCTION-CALL
		  $SUBSTITUTE $YA $Y
		  (SIMPLIFY
		    (MFUNCTION-CALL
		      $SUBSTITUTE $DYA
		      (SIMPLIFY (LIST '(%DERIVATIVE) $Y $X)) $BCEQ))))
	      '$CONST1)))
    (RETURN
     (COND
      ((> (MFUNCTION-CALL $LENGTH $CO1) 1)
       (LIST
	'(MLIST)
	(SIMPLIFY
	 (MFUNCTION-CALL
	  $PRINT
	  '|&SOLUTIONS ARE NOT UNIQUE, POSSIBLE SOLUTIONS ARE:
             |))
	(SIMPLIFY
	  (MAP1 (GETOPR
		  (M-TLAMBDA
		    ($L1) NIL (SIMPLIFY (MFUNCALL '$DISPLAY $L1))))
		$CO1))
	(RETURN $Z)))
      ((LIKE $CO1 '((MLIST)))
	 (RETURN
	   (SIMPLIFY (MFUNCTION-CALL
		       $PRINT
		       '|&No solution for given initial
condition|))))
      (T (SIMPLIFY
	   (MFUNCTION-CALL
	     $SUBSTITUTE
	     (SIMPLIFY (MFUNCTION-CALL $RHS (MARRAYREF $CO1 1)))
	     '$CONST1 $Z)))))))
  '$Z '$DERY '$DYA '$YA '$CO1))

(DEFPROP $EVEC T TRANSLATED)

(ADD2LNC '$EVEC $PROPS)

(DEFMTRFUN
 ($EVEC $ANY MDEFINE NIL NIL) ($M $MU $MODES) NIL
 ((LAMBDA ($EQUATIONS $UNKNOWNS $X)
   NIL
   (DO (($L 1 (+ 1 $L))) ((IS-BOOLE-CHECK (MGRP $L $MODES)) '$DONE)
     (LIST
       '(MLIST) (SETQ $EQUATIONS '((MLIST)))
       (SETQ $UNKNOWNS '((MLIST))) (MARRAYSET 1 $X $L)
       (DO (($I 1 (+ 1 $I))) ((IS-BOOLE-CHECK (MGRP $I $MODES)) '$DONE)
	 (COND
	   ((NOT (= $I $L))
	      (LIST
		'(MLIST)
		(SETQ
		  $UNKNOWNS
		  (SIMPLIFY
		    (MFUNCTION-CALL $CONS (MARRAYREF $X $I) $UNKNOWNS)))
		(SETQ
		  $EQUATIONS
		  (SIMPLIFY
		    (MFUNCTION-CALL
		      $CONS
		      (SIMPLIFY
			(LIST
			  '(MEQUAL)
			  (DOSUM
			    (FUNGEN&ENV-FOR-MEVALSUMARG
			      ($I $M $X) ($J)
			      (MUL* (MARRAYREF $M $I $J) (MARRAYREF $X $J))
			      ((MTIMES) (($M ARRAY) $I $J)
			       (($X ARRAY) $J)))
			    '$J 1 $MODES T)
			  (MUL* (MARRAYREF $MU $L) (MARRAYREF $X $I))))
		      $EQUATIONS)))))))
       (SIMPLIFY
	 (MFUNCTION-CALL
	   $PRINT
	   (SIMPLIFY (MFUNCTION-CALL $SOLVE $EQUATIONS $UNKNOWNS)))))))
  '$EQUATIONS '$UNKNOWNS '$X))

(DEFPROP $CONSMAT3 T TRANSLATED)

(ADD2LNC '$CONSMAT3 $PROPS)

(DEFMTRFUN
 ($CONSMAT3 $ANY MDEFINE NIL NIL) ($L1 $L2 $L3) NIL
 ((LAMBDA ($A $B $MAT3 $CP $CPM $SOL1 $L)
    NIL
    (PROG ()
	 (SETQ
	   $MAT3
	   (SIMPLIFY
	     (LIST '($MATRIX) (LIST '(MLIST) 1 2 3) (LIST '(MLIST) $B 3 $A)
		   (LIST '(MLIST) 1 1 (ADD* $L1 $L2 $L3 -4)))))
	 (SETQ
	   $CP
	   (SIMPLIFY
	     (MFUNCTION-CALL
	       $EXPAND
	       (MUL* (*MMINUS (ADD* $L (*MMINUS $L1)))
		     (ADD* $L (*MMINUS $L2)) (ADD* $L (*MMINUS $L3))))))
	 (SETQ $CPM (SIMPLIFY (MFUNCTION-CALL $CHARPOLY $MAT3 $L)))
	 (SETQ $GLOBALSOLVE T)
	 (SETQ
	   $SOL1
	   (SIMPLIFY
	     (MFUNCTION-CALL
	       $SOLVE
	       (LIST
		 '(MLIST)
		 (SIMPLIFY
		   (LIST
		     '(MEQUAL) (SIMPLIFY (MFUNCTION-CALL $RATCOEF $CP $L))
		     (SIMPLIFY (MFUNCTION-CALL $RATCOEF $CPM $L))))
		 (SIMPLIFY
		   (LIST '(MEQUAL)
			 (SIMPLIFY (MFUNCTION-CALL $RATCOEF $CP $L 0))
			 (SIMPLIFY (MFUNCTION-CALL $RATCOEF $CPM $L 0)))))
	       (LIST '(MLIST) $A $B))))
	 (RETURN (SIMPLIFY (MFUNCALL '$EV $MAT3)))))
  '$A '$B '$MAT3 '$CP '$CPM '$SOL1 '$L))

(compile-forms-to-compile-queue)

