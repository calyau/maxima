;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE1]NCHRPL.MC;1
;;; Written on 9/12/1984 04:58:26, from MACSYMA 302
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

(comment "MAX$DISK:[SHARE1]NCHRPL.MC;1")

;;; General declarations required for translated MACSYMA code.

(DECLARE (SPECIAL $MAPPRINT $MAPERROR))

(DEFMTRFUN-EXTERNAL ($MATTRACE $ANY MDEFINE NIL NIL))

(PUTPROP 'MAPLIST_TR (OR (GET 'MARRAYREF 'AUTOLOAD) T) 'AUTOLOAD)

(DEFMTRFUN-EXTERNAL ($NCHARPOLY $ANY MDEFINE NIL NIL))


(DEFPROP $MATTRACE T TRANSLATED)

(ADD2LNC '$MATTRACE $PROPS)

(DEFMTRFUN
  ($MATTRACE $ANY MDEFINE NIL NIL) ($A) NIL
  ((LAMBDA ($ANS)
     NIL
     (DO (($I 1 (+ 1 $I))) ((> $I (MFUNCTION-CALL $LENGTH $A)) '$DONE)
       (SETQ $ANS (ADD* $ANS (MARRAYREF $A $I $I))))
     $ANS)
   0))

(DEFPROP $NCHARPOLY T TRANSLATED)

(ADD2LNC '$NCHARPOLY $PROPS)

(DEFMTRFUN
  ($NCHARPOLY $ANY MDEFINE NIL NIL) ($A $VAR) NIL
  ((LAMBDA ($AK $TRLIST $SYMLIST $K $P $MAPERROR $MAPPRINT)
     NIL
     (DO ((MDO 1 (+ 1 MDO)))
	 ((> MDO (+ (MFUNCTION-CALL $LENGTH $A) -1)) '$DONE)
       ((LAMBDA ()
	  NIL
	  (SETQ $AK (NCMUL2 $A $AK))
	  (SETQ $TRLIST
		(SIMPLIFY (MFUNCTION-CALL
			    $CONS (SIMPLIFY (MFUNCTION-CALL $MATTRACE $AK))
			    $TRLIST))))))
     (SETQ $TRLIST (SIMPLIFY (MFUNCTION-CALL $REVERSE $TRLIST)))
     (DO (($I) (MDO (CDR $TRLIST) (CDR MDO))) ((NULL MDO) '$DONE)
       (SETQ $I (CAR MDO))
       (SETQ $K (ADD* $K 1))
       (SETQ
	 $SYMLIST
	 (SIMPLIFY
	   (MFUNCTION-CALL
	     $CONS
	     (DIV
	       (SIMPLIFY (MAPPLY-TR '&+ (MAPLIST_TR '&* $SYMLIST $TRLIST)))
	       (*MMINUS $K))
	     $SYMLIST))))
     (DO (($I 0 (+ 1 $I))) ((LIKE $SYMLIST '((MLIST))) '$DONE)
       ((LAMBDA ()
	  NIL
	  (SETQ
	    $P
	    (ADD* $P (MUL* (SIMPLIFY ($FIRST $SYMLIST)) (POWER $VAR $I))))
	  (SETQ $SYMLIST (SIMPLIFY (MFUNCTION-CALL $REST $SYMLIST))))))
     (SIMPLIFY (MFUNCTION-CALL $RATSIMP $P $VAR)))
   $A (LIST '(MLIST) (SIMPLIFY (MFUNCTION-CALL $MATTRACE $A)))
   (LIST '(MLIST) 1) 0 0 NIL NIL))

(compile-forms-to-compile-queue)

