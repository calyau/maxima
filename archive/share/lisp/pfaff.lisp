;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE2]PFAFF.MC;2
;;; Written on 9/20/1984 05:27:53, from MACSYMA 302
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

(comment "PFAFF.MC")

;;; General declarations required for translated MACSYMA code.

(DECLARE (SPECIAL $ERREXP $PFAFFM ^W))

(DEF-MTRVAR $PFAFFM '$PFAFFM 1)

(DEFMTRFUN-EXTERNAL ($PFAFFIAN $ANY MDEFINE NIL NIL))


(SETQ ^W T)

(MEVAL* '(($DECLARE) $PFAFFM $SPECIAL))

(DEFPROP $PFAFFIAN T TRANSLATED)

(ADD2LNC '$PFAFFIAN $PROPS)

(DEFMTRFUN
 ($PFAFFIAN $ANY MDEFINE NIL NIL) ($INTEG $LIST) NIL
 ((LAMBDA ($SGN $ANS $PDUM $LDUM)
   NIL
   (PROG ()
    (COND
     ((NOT (AND (MFUNCTION-CALL $LISTP $LIST)
		(MFUNCTION-CALL $INTEGERP $INTEG)
		(IS-BOOLE-CHECK (MGRP $INTEG 0))
		(LIKE (MFUNCTION-CALL $LENGTH $LIST)
		      (DIV (MUL* $INTEG (ADD* $INTEG 1)) 2))))
      (SIMPLIFY
       (MFUNCTION-CALL
	$ERROR
	(PROGN
	 (SETQ $ERREXP (LIST '(MLIST) $INTEG $LIST))
	 '|&Invalid arg to PFAFFIAN.MERREXP holds the offending expression.|)))))
    (COND ((LIKE $INTEG 1) (RETURN (SIMPLIFY ($FIRST $LIST)))))
    (COND ((MFUNCTION-CALL $EVENP $INTEG) (RETURN 0)))
    (COND
     ((LIKE $INTEG 3)
	(RETURN
	  (ADD*
	    (MUL* (MARRAYREF $LIST 1) (MARRAYREF $LIST 6))
	    (*MMINUS (MUL* (MARRAYREF $LIST 2) (MARRAYREF $LIST 5)))
	    (MUL* (MARRAYREF $LIST 3) (MARRAYREF $LIST 4)))))
     (T
      (DO (($KZERO 1 (+ 1 $KZERO)))
	  ((IS-BOOLE-CHECK (MGRP $KZERO $INTEG)) '$DONE)
	(SETQ
	  $ANS
	  (ADD*
	    $ANS
	    (MUL*
	      (SIMPLIFY (MFUNCTION-CALL $INPART $LIST $KZERO))
	      (SETQ $SGN (*MMINUS $SGN))
	      (SIMPLIFY
		(MFUNCTION-CALL
		  $PFAFFIAN (ADD* $INTEG -2)
		  (SIMPLIFY
		    (MFUNCTION-CALL
		      $REST
		      (SIMPLIFY
			(MFUNCTION-CALL
			  $INPART $LIST
			  (SIMPLIFY
			    (MAPPLY-TR
			      '$ALLBUT
			      (COND
				((MFUNCTION-CALL
				   $LISTP
				   (SETQ
				     $PDUM
				     (MARRAYREF
				       (TRD-MSYMEVAL $PFAFFM '$PFAFFM)
				       $INTEG $KZERO)))
				   $PDUM)
				(T
				  (MARRAYSET
				    (PROGN
				      (SETQ
					$LDUM
					(DIV (MUL* (+ $KZERO 1)
						   (ADD* (MUL* 2 $INTEG)
							 (- $KZERO)))
					     2))
				      (SETQ $PDUM '((MLIST)))
				      (COND
					((NOT (= $KZERO 1))
					   (DO (($MDUM 2 (+ 1 $MDUM)))
					       ((> $MDUM $KZERO) '$DONE)
					     (SETQ
					       $PDUM
					       (SIMPLIFY
						 (MFUNCTION-CALL
						   $ENDCONS
						   (ADD*
						     $KZERO
						     (DIV (MUL*
							    (+ $MDUM
							       -1)
							    (ADD*
							      (MUL* 2
								    $INTEG)
							      (- $MDUM)))
							  2))
						   $PDUM))))))
				      (DO
					(($MDUM
					   (ADD*
					     (DIV (MUL*
						    $KZERO
						    (ADD* (MUL* 2 $INTEG)
							  (- $KZERO) 1))
						  2)
					     1)
					   (ADD* 1 $MDUM)))
					((IS-BOOLE-CHECK
					   (MGRP $MDUM $LDUM))
					   '$DONE)
					(SETQ
					  $PDUM
					  (SIMPLIFY
					    (MFUNCTION-CALL $ENDCONS $MDUM
							    $PDUM))))
				      $PDUM)
				    (TRD-MSYMEVAL $PFAFFM '$PFAFFM)
				    $INTEG $KZERO)))))))
		      $INTEG))))))))))
    (RETURN $ANS)))
  -1 0 '$PDUM '$LDUM))

(SETQ ^W NIL)

(compile-forms-to-compile-queue)

