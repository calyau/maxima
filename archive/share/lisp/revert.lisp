;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE2]REVERT.MC;5
;;; Written on 9/20/1984 06:28:12, from MACSYMA 302
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

(comment "MAX$DISK:[SHARE2]REVERT.MC;5")

;;; General declarations required for translated MACSYMA code.

(DECLARE (SPECIAL $W $V))

(DEF-MTRVAR $V '$V 1)

(DEF-MTRVAR $W '$W 1)

(DEFMTRFUN-EXTERNAL ($REVERT $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($REVERT2 $ANY MDEFINE NIL NIL))


(MEVAL* '(($DECLARE) ((MLIST) $V $W) $SPECIAL))

(DEFPROP $REVERT T TRANSLATED)

(ADD2LNC '$REVERT $PROPS)

(DEFMTRFUN
 ($REVERT $ANY MDEFINE NIL NIL) ($EXP $T) NIL
 ((LAMBDA ($F $N $V $W $R1)
    NIL
    (SETQ $N (SIMPLIFY (MFUNCTION-CALL $HIPOW $EXP $T)))
    (APPLY '$ARRAY (LIST '((MLIST) $V $W) $N))
    (SETQ $R1 (DIV 1 (SIMPLIFY (MFUNCTION-CALL $RATCOEF $EXP $T 1))))
    (MARRAYSET 1 (TRD-MSYMEVAL $V '$V) 1)
    (DO (($K 2 (+ 1 $K))) ((IS-BOOLE-CHECK (MGRP $K $N)) '$DONE)
      (MARRAYSET (MUL* (SIMPLIFY (MFUNCTION-CALL $RATCOEF $EXP $T $K)) $R1)
		 (TRD-MSYMEVAL $V '$V) $K)
      (MARRAYSET (*MMINUS (MARRAYREF (TRD-MSYMEVAL $V '$V) $K))
		 (TRD-MSYMEVAL $W '$W) $K))
    (MARRAYSET 1 (TRD-MSYMEVAL $W '$W) 1)
    (SETQ
      $F
      (M-TLAMBDA&ENV
	(($J) ($N)) NIL
	(DO (($I (ADD* $J 1) (ADD* 1 $I)))
	    ((IS-BOOLE-CHECK (MGRP $I $N)) '$DONE)
	  (MARRAYSET
	    (ADD*
	      (MARRAYREF (TRD-MSYMEVAL $W '$W) $I)
	      (*MMINUS
		(MUL* (MARRAYREF (TRD-MSYMEVAL $W '$W) $J)
		      (MARRAYREF
			(TRD-MSYMEVAL $V '$V) (ADD* $I (*MMINUS $J) 1)))))
	    (TRD-MSYMEVAL $W '$W) $I))))
    (DO (($J 2 (+ 1 $J))) ((IS-BOOLE-CHECK (MGRP $J (ADD* $N -1))) '$DONE)
      (SIMPLIFY (MFUNCALL $F $J)))
    (DO (($K 2 (+ 1 $K))) ((IS-BOOLE-CHECK (MGRP $K (ADD* $N -1))) '$DONE)
      (DO (($J $K (+ 1 $J)))
	  ((IS-BOOLE-CHECK (MGRP $J (ADD* $N -1))) '$DONE)
	(SIMPLIFY (MFUNCALL $F $J))))
    (SETQ $F 0)
    (DO (($K 1 (+ 1 $K))) ((IS-BOOLE-CHECK (MGRP $K $N)) '$DONE)
      (SETQ $F (ADD* $F (MUL* (MARRAYREF (TRD-MSYMEVAL $W '$W) $K)
			      (POWER (MUL* $T $R1) $K)))))
    $F)
  '$F '$N '$V '$W '$R1))

(DEFPROP $REVERT2 T TRANSLATED)

(ADD2LNC '$REVERT2 $PROPS)

(DEFMTRFUN
 ($REVERT2 $ANY MDEFINE NIL NIL) ($EXP $VAR $N) NIL
 ((LAMBDA ($F $V $W $R1 $N)
    NIL
    (PROG ()
	 (SETQ $N (SIMPLIFY (MFUNCTION-CALL $HIPOW $EXP $VAR)))
	 (COND ((LIKE $N 0) (RETURN $EXP)))
	 (APPLY '$ARRAY (LIST '((MLIST) $V $W) $N))
	 (MARRAYSET (SIMPLIFY (MFUNCTION-CALL $RATCOEF $EXP $VAR 0))
		    (TRD-MSYMEVAL $V '$V) 0)
	 (SETQ
	   $R1 (DIV 1 (SIMPLIFY (MFUNCTION-CALL $RATCOEF $EXP $VAR 1))))
	 (MARRAYSET 1 (TRD-MSYMEVAL $V '$V) 1)
	 (DO (($K 2 (+ 1 $K))) ((IS-BOOLE-CHECK (MGRP $K $N)) '$DONE)
	   (MARRAYSET
	     (MUL* (SIMPLIFY (MFUNCTION-CALL $RATCOEF $EXP $VAR $K)) $R1)
	     (TRD-MSYMEVAL $V '$V) $K)
	   (MARRAYSET (*MMINUS (MARRAYREF (TRD-MSYMEVAL $V '$V) $K))
		      (TRD-MSYMEVAL $W '$W) $K))
	 (MARRAYSET 1 (TRD-MSYMEVAL $W '$W) 1)
	 (SETQ
	   $F
	   (M-TLAMBDA&ENV
	     (($J) ($N)) NIL
	     (DO (($I (ADD* $J 1) (ADD* 1 $I)))
		 ((IS-BOOLE-CHECK (MGRP $I $N)) '$DONE)
	       (MARRAYSET
		 (ADD*
		   (MARRAYREF (TRD-MSYMEVAL $W '$W) $I)
		   (*MMINUS (MUL* (MARRAYREF (TRD-MSYMEVAL $W '$W) $J)
				  (MARRAYREF (TRD-MSYMEVAL $V '$V)
					     (ADD* $I (*MMINUS $J) 1)))))
		 (TRD-MSYMEVAL $W '$W) $I))))
	 (DO (($J 2 (+ 1 $J)))
	     ((IS-BOOLE-CHECK (MGRP $J (ADD* $N -1))) '$DONE)
	   (SIMPLIFY (MFUNCALL $F $J)))
	 (DO (($K 2 (+ 1 $K)))
	     ((IS-BOOLE-CHECK (MGRP $K (ADD* $N -1))) '$DONE)
	   (DO (($J $K (+ 1 $J)))
	       ((IS-BOOLE-CHECK (MGRP $J (ADD* $N -1))) '$DONE)
	     (SIMPLIFY (MFUNCALL $F $J))))
	 (SETQ $F 0)
	 (SETQ $R1 (SIMPLIFY (MFUNCTION-CALL $RATDISREP $R1)))
	 (DO (($K 1 (+ 1 $K))) ((IS-BOOLE-CHECK (MGRP $K $N)) '$DONE)
	   (SETQ
	     $F
	     (ADD*
	       (MUL*
		 (SIMPLIFY
		   (MFUNCTION-CALL
		     $RATDISREP (MARRAYREF (TRD-MSYMEVAL $W '$W) $K)))
		 (POWER
		   (MUL*
		     (SIMPLIFY
		       (MFUNCTION-CALL
			 $RATDISREP
			 (ADD*
			   $VAR
			   (*MMINUS (MARRAYREF (TRD-MSYMEVAL $V '$V) 0)))))
		     $R1)
		   $K))
	       $F)))
	 (RETURN $F)))
  '$F '$V '$W '$R1 '$N))

(compile-forms-to-compile-queue)

