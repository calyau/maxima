;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE1]DETERM.MC;10
;;; Written on 9/24/1984 02:26:22, from MACSYMA 302
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

(comment "MAX$DISK:[SHARE1]DETERM.MC;10")

;;; General declarations required for translated MACSYMA code.

(DECLARE (SPECIAL $TAKEGCD))

(DEFMTRFUN-EXTERNAL ($DET $ANY MDEFINE NIL NIL))


(SIMPLIFY (MFUNCTION-CALL $LOAD '|functs|))

(DEFPROP $DET T TRANSLATED)

(ADD2LNC '$DET $PROPS)

(DEFMTRFUN
 ($DET $ANY MDEFINE NIL NIL) ($M) NIL
 ((LAMBDA ($N $A)
   NIL
   (SETQ $N (MFUNCTION-CALL $LENGTH $M))
   (COND
     ((IS-BOOLE-CHECK (MLSP $N 2))
	(SIMPLIFY (MFUNCTION-CALL $ERROR '|&Improper argument:| $M))))
   (SETQ $A (SIMPLIFY (MFUNCTION-CALL GENSYM)))
   (SIMPLIFY (MFUNCALL '$ARRAY $A $N $N))
   (DO (($I 1 (+ 1 $I))) ((IS-BOOLE-CHECK (MGRP $I $N)) '$DONE)
     (DO (($J 1 (+ 1 $J))) ((IS-BOOLE-CHECK (MGRP $J $N)) '$DONE)
       (SIMPLIFY
	 (MFUNCTION-CALL
	   MSET
	   (SIMPLIFY
	     (MFUNCTION-CALL $ARRAYAPPLY $A (LIST '(MLIST) $I $J)))
	   (MARRAYREF $M $I $J)))))
   (SIMPLIFY
     (MFUNCTION-CALL
       MSET
       (SIMPLIFY (MFUNCTION-CALL $ARRAYAPPLY $A (LIST '(MLIST) 0 0))) 1))
   ((LAMBDA ()
     ((LAMBDA (MCATCH)
       (PROG2
	NIL
	(*CATCH
	  'MCATCH
	  (PROGN
	    (DO (($K 2 (+ 2 $K)))
		((IS-BOOLE-CHECK (MGRP $K $N)) '$DONE)
	      (COND
		((NOT (LIKE $K $N))
		  ((LAMBDA ($C0 $L1 $L2 $U)
		     NIL
		     (PROG ()
			(SETQ
			  $L1
			  (COND
			    ((NOT
			       (LIKE (MARRAYREF $A (+ $K -1) (+ $K -1))
				     0))
			       NIL)
			    (T
			      (DO (($S $K (+ 1 $S)))
				  ((IS-BOOLE-CHECK (MGRP $S $N)) '$DONE)
				(COND
				  ((NOT
				     (LIKE (MARRAYREF $A $S (+ $K -1))
					   0))
				     (DO (($J 1 (+ 1 $J)))
					 ((IS-BOOLE-CHECK (MGRP $J $N))
					    '$DONE)
				       ((LAMBDA ($T)
					  NIL
					  (SETQ
					    $T
					    (MARRAYREF $A (+ $K -1) $J))
					  (MARRAYSET (MARRAYREF $A $S $J)
						     $A (+ $K -1) $J)
					  (MARRAYSET $T $A $S $J))
					'$T))
				     (RETURN NIL))
				  ((LIKE $S $N)
				     ((LAMBDA (X)
					(COND
					  ((NULL MCATCH)
					     (DISPLA X)
					     (*MERROR
					       '|THROW not within CATCH|)))
					(*THROW 'MCATCH X))
				      0)))))))
			(SETQ
			  $L2
			  (COND
			    ((IS-BOOLE-CHECK $L1) T)
			    (T
			      (DO (($T $K (+ 1 $T)))
				  ((IS-BOOLE-CHECK (MGRP $T $N)) '$DONE)
				(SETQ
				  $C0
				  (SIMPLIFY
				    (MFUNCTION-CALL
				      $DETERMINANT
				      (SIMPLIFY
					(LIST
					  '($MATRIX)
					  (LIST
					    '(MLIST)
					    (MARRAYREF $A (+ $K -1)
						       (+ $K -1))
					    (MARRAYREF $A (+ $K -1) $K))
					  (LIST
					    '(MLIST)
					    (MARRAYREF $A $T (+ $K -1))
					    (MARRAYREF $A $T $K)))))))
				(COND
				  ((NOT (LIKE $C0 0))
				     (SETQ $U $T) (RETURN NIL))
				  ((LIKE $T $N)
				     ((LAMBDA (X)
					(COND
					  ((NULL MCATCH)
					     (DISPLA X)
					     (*MERROR
					       '|THROW not within CATCH|)))
					(*THROW 'MCATCH X))
				      0)))))))
			(RETURN
			  (COND
			    ((IS-BOOLE-CHECK $L2) (RETURN T))
			    (T
			      (SETQ
				$C0
				(SIMPLIFY ($GCDIVIDE
					    $C0
					    (MARRAYREF
					      $A (+ $K -2) (+ $K -2)))))
			      (COND
				((NOT (LIKE $U $K))
				   (DO (($J 1 (+ 1 $J)))
				       ((IS-BOOLE-CHECK (MGRP $J $N))
					  '$DONE)
				     ((LAMBDA ($T)
					NIL
					(SETQ $T (MARRAYREF $A $K $J))
					(MARRAYSET (MARRAYREF $A $T $J)
						   $A $K $J)
					(MARRAYSET $T $A $T $J))
				      '$T))))
			      (DO (($I (+ $K 1) (+ 1 $I)))
				  ((IS-BOOLE-CHECK (MGRP $I $N)) '$DONE)
				((LAMBDA ($C1 $C2)
				   NIL
				   (SETQ
				     $C1
				     (SIMPLIFY
				       ($GCDIVIDE
					 (*MMINUS
					   (SIMPLIFY
					     (MFUNCTION-CALL
					       $DETERMINANT
					       (SIMPLIFY
						 (LIST
						   '($MATRIX)
						   (LIST '(MLIST)
							 (MARRAYREF
							   $A (+ $K -1)
							   (+ $K -1))
							 (MARRAYREF
							   $A (+ $K -1)
							   $K))
						   (LIST
						     '(MLIST)
						     (MARRAYREF $A $I
								(+ $K -1))
						     (MARRAYREF $A $I
								$K)))))))
					 (MARRAYREF $A (+ $K -2)
						    (+ $K -2)))))
				   (SETQ
				     $C2
				     (SIMPLIFY
				       ($GCDIVIDE
					 (SIMPLIFY
					   (MFUNCTION-CALL
					     $DETERMINANT
					     (SIMPLIFY
					       (LIST
						 '($MATRIX)
						 (LIST
						   '(MLIST)
						   (MARRAYREF $A $K
							      (+ $K -1))
						   (MARRAYREF $A $K $K))
						 (LIST
						   '(MLIST)
						   (MARRAYREF $A $I
							      (+ $K -1))
						   (MARRAYREF
						     $A $I $K))))))
					 (MARRAYREF $A (+ $K -2)
						    (+ $K -2)))))
				   (MARRAYSET 0 $A $I (+ $K -1))
				   (MARRAYSET 0 $A $I $K)
				   (DO (($J (+ $K 1) (+ 1 $J)))
				       ((IS-BOOLE-CHECK (MGRP $J $N))
					  '$DONE)
				     (MARRAYSET
				       (SIMPLIFY
					 ($GCDIVIDE
					   (ADD*
					     (MUL* $C0
						   (MARRAYREF $A $I $J))
					     (MUL* $C1
						   (MARRAYREF $A $K $J))
					     (MUL* $C2
						   (MARRAYREF
						     $A (+ $K -1) $J)))
					   (MARRAYREF $A (+ $K -2)
						      (+ $K -2))))
				       $A $I $J)))
				 '$C1 '$C2))
			      (SETQ $C0 0)
			      (DO (($J (+ $K 1) (+ 1 $J)))
				  ((IS-BOOLE-CHECK (MGRP $J $N)) '$DONE)
				(MARRAYSET 0 $A $K $J)
				(MARRAYSET 0 $A (+ $K -1) $J)))))))
		   '$C0 '$L1 '$L2 '$U)))
	      (MARRAYSET
		(COND
		  ((LIKE $K (ADD* $N -1)) 0)
		  (T
		    (SIMPLIFY
		      ($GCDIVIDE
			(SIMPLIFY
			  (MFUNCTION-CALL
			    $DETERMINANT
			    (SIMPLIFY (MFUNCTION-CALL
					$GENMATRIX $A $K $K (+ $K -1)))))
			(MARRAYREF $A (+ $K -2) (+ $K -2))))))
		$A $K $K)
	      (MARRAYSET 0 $A (+ $K -2) (+ $K -2))
	      (MARRAYSET 0 $A (+ $K -1) (+ $K -1))
	      (MARRAYSET 0 $A $K (+ $K -1))
	      (MARRAYSET 0 $A (+ $K -1) $K)
	      (COND ((OR (LIKE $K $N) (LIKE $K (ADD* $N -1)))
		       (RETURN NIL))))
	    (MARRAYREF $A $N $N)))
	(ERRLFUN1 MCATCH)))
      (CONS BINDLIST LOCLIST)))))
  '$N '$A))

(compile-forms-to-compile-queue)

