;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE2]SQDNST.MC;2
;;; Written on 9/20/1984 20:14:19, from MACSYMA 302
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

(comment "MAX$DISK:[SHARE2]SQDNST.MC;2")

;;; General declarations required for translated MACSYMA code.

(DECLARE)

(DEFMTRFUN-EXTERNAL ($SQRTDENEST $ANY MDEFINE NIL NIL))


(DEFPROP $SQRTDENEST T TRANSLATED)

(ADD2LNC '$SQRTDENEST $PROPS)

(DEFMTRFUN
 ($SQRTDENEST $ANY MDEFINE NIL NIL) ($A) NIL
 (SIMPLIFY
  (MFUNCTION-CALL
   $SUBSTITUTE
   (SIMPLIFY
     (LIST
       '(MEQUAL) '&^
       (M-TLAMBDA
	 ($A $B) NIL
	 ((LAMBDA ($DISCR $MAX $MIN)
	    NIL
	    (COND
	      ((AND
		 (MFUNCTION-CALL
		   $EVENP (SIMPLIFY (MFUNCTION-CALL $DENOM $B)))
		 (NOT (MFUNCTION-CALL $ATOM $A))
		 (LIKE (SIMPLIFY (MFUNCTION-CALL $INPART $A 0)) '&+)
		 (PROGN
		   (SETQ
		     $MAX
		     (MAXIMUM (LIST (SIMPLIFY ($FIRST $A))
				    (SIMPLIFY (MFUNCTION-CALL $REST $A)))))
		   (SETQ $MIN (ADD* $A (*MMINUS $MAX)))
		   (MFUNCTION-CALL
		     $NUMBERP
		     (SETQ
		       $DISCR
		       (SIMPLIFY
			 (LIST
			   '(%SQRT)
			   (ADD* 1
				 (*MMINUS (POWER (DIV $MIN $MAX) 2)))))))))
		 (POWER
		   (ADD*
		     (SIMPLIFY
		       (LIST '(%SQRT) (DIV (MUL* $MAX (ADD* 1 $DISCR)) 2)))
		     (MUL*
		       (SIMPLIFY (LIST '(%SIGNUM) $MIN))
		       (SIMPLIFY
			 (LIST '(%SQRT)
			       (DIV (MUL* $MAX (ADD* 1 (*MMINUS $DISCR)))
				    2)))))
		   (MUL* 2 $B)))
	      (T (POWER $A $B))))
	  '$DISCR '$MAX '$MIN))))
   $A)))

(compile-forms-to-compile-queue)

