;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE1]ELIM.MC;4
;;; Written on 9/10/1984 00:48:31, from MACSYMA 302
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

(comment "MAX$DISK:[SHARE1]ELIM.MC;4")

;;; General declarations required for translated MACSYMA code.

(DECLARE (SPECIAL $DISPFLAG))

(DEFMTRFUN-EXTERNAL ($ELIMINATE $ANY MDEFINE NIL NIL))


(DEFPROP $ELIMINATE T TRANSLATED)

(ADD2LNC '$ELIMINATE $PROPS)

(DEFMTRFUN
 ($ELIMINATE $ANY MDEFINE NIL NIL) ($EQNS $VARS) NIL
 ((LAMBDA ($TEQNS $SV $SE $L $FLAG $DISPFLAG)
    (DECLARE (FIXNUM $L))
    NIL
    (SETQ $FLAG (SETQ $DISPFLAG NIL))
    (COND
      ((NOT
	 (AND (MFUNCTION-CALL $LISTP $EQNS) (MFUNCTION-CALL $LISTP $VARS)))
	 (SIMPLIFY
	   (MFUNCTION-CALL $ERROR '|&THE ARGUMENTS MUST BOTH BE LISTS|))))
    (COND
      ((> (MFUNCTION-CALL $LENGTH $VARS)
	  (SETQ $L (MFUNCTION-CALL $LENGTH $EQNS)))
	 (SIMPLIFY
	   (MFUNCTION-CALL $ERROR '|&MORE VARIABLES THEN EQUATIONS|))))
    (COND
      ((= $L 1)
	 (SIMPLIFY (MFUNCTION-CALL
		     $ERROR '|&CAN'T ELIMINATE FROM ONLY ONE EQUATION|))))
    (COND
      ((= (MFUNCTION-CALL $LENGTH $VARS) $L)
	 (SETQ $VARS (SIMPLIFY (MFUNCTION-CALL $REVERSE $VARS)))
	 (SETQ $SV (MARRAYREF $VARS 1))
	 (SETQ
	   $VARS
	   (SIMPLIFY (MFUNCTION-CALL
		       $REVERSE (SIMPLIFY (MFUNCTION-CALL $REST $VARS)))))
	 (SETQ $FLAG T)))
    (SETQ $EQNS (SIMPLIFY (MAP1 (GETOPR 'MEQHK) $EQNS)))
    (DO (($V) (MDO (CDR $VARS) (CDR MDO))) ((NULL MDO) '$DONE)
      (SETQ $V (CAR MDO))
      (SETQ $TEQNS '((MLIST)))
      (DO (($J 1 (+ 1 $J)))
	  ((OR (> $J $L)
	       (NOT (MFUNCTION-CALL $FREEOF $V (SIMPLIFY ($FIRST $EQNS)))))
	     '$DONE)
	(SETQ $TEQNS
	      (SIMPLIFY
		(MFUNCTION-CALL $CONS (SIMPLIFY ($FIRST $EQNS)) $TEQNS)))
	(SETQ $EQNS (SIMPLIFY (MFUNCTION-CALL $REST $EQNS))))
      (COND
	((LIKE $EQNS '((MLIST))) (SETQ $EQNS $TEQNS))
	(T (SETQ
	     $TEQNS
	     (SIMPLIFY
	       (MFUNCTION-CALL
		 $APPEND $TEQNS (SIMPLIFY (MFUNCTION-CALL $REST $EQNS)))))
	   (SETQ $EQNS (SIMPLIFY ($FIRST $EQNS)))
	   (SETQ $L (+ $L -1)) (SETQ $SE '((MLIST)))
	   (DO (($J 1 (+ 1 $J))) ((> $J $L) '$DONE)
	     (SETQ
	       $SE
	       (SIMPLIFY
		 (MFUNCTION-CALL
		   $CONS
		   (SIMPLIFY (MFUNCTION-CALL
			       $RESULTANT $EQNS (MARRAYREF $TEQNS $J) $V))
		   $SE))))
	   (SETQ $EQNS $SE))))
    (COND
      ($FLAG
	(LIST
	  '(MLIST)
	  (SIMPLIFY
	    (MFUNCTION-CALL
	      $RHS
	      (SIMPLIFY
		(MFUNCALL
		  '$EV
		  (SIMPLIFY
		    (MFUNCTION-CALL
		      $LAST
		      (SIMPLIFY
			(MFUNCTION-CALL $SOLVE (MARRAYREF $EQNS 1) $SV))))
		  '$EVAL))))))
      (T $EQNS)))
  '$TEQNS '$SV '$SE 0 '$FLAG '$DISPFLAG))

(compile-forms-to-compile-queue)

