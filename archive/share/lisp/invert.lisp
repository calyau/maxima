;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE1]INVERT.MC;2
;;; Written on 9/12/1984 02:52:37, from MACSYMA 302
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

(comment "MAX$DISK:[SHARE1]INVERT.MC;2")

;;; General declarations required for translated MACSYMA code.

(DECLARE (SPECIAL $SCALARMATRIXP))

(DEFMTRFUN-EXTERNAL ($ADJOINT $ANY MDEFINE NIL NIL))

(DEF-MTRVAR $SCALARMATRIXP '$SCALARMATRIXP 1)

(DEFMTRFUN-EXTERNAL ($INVERT $ANY MDEFINE NIL NIL))


(DEFPROP $ADJOINT T TRANSLATED)

(ADD2LNC '$ADJOINT $PROPS)

(DEFMTRFUN
  ($ADJOINT $ANY MDEFINE NIL NIL) ($MAT) NIL
  ((LAMBDA ($ADJ $N)
     NIL
     (SETQ $N (MFUNCTION-CALL $LENGTH $MAT))
     (SETQ $ADJ (SIMPLIFY (MFUNCTION-CALL $IDENT $N)))
     (COND
       ((NOT (LIKE $N 1))
	  (DO (($I 1 (+ 1 $I))) ((IS-BOOLE-CHECK (MGRP $I $N)) '$DONE)
	    (DO (($J 1 (+ 1 $J))) ((IS-BOOLE-CHECK (MGRP $J $N)) '$DONE)
	      (MARRAYSET
		(MUL* (POWER -1 (+ $I $J))
		      (SIMPLIFY
			(MFUNCTION-CALL
			  $DETERMINANT
			  (SIMPLIFY (MFUNCTION-CALL $MINOR $MAT $J $I)))))
		$ADJ $I $J)))))
     $ADJ)
   '$ADJ '$N))

(DEFPROP $INVERT T TRANSLATED)

(ADD2LNC '$INVERT $PROPS)

(DEFMTRFUN
  ($INVERT $ANY MDEFINE NIL NIL) ($MAT) NIL
  ((LAMBDA ($ADJ $ANS)
     NIL
     (SETQ $ADJ (SIMPLIFY (MFUNCTION-CALL $ADJOINT $MAT)))
     (SETQ
       $ANS
       ((LAMBDA ($SCALARMATRIXP)
	  NIL (DIV $ADJ (NCMUL2 (SIMPLIFY (MFUNCTION-CALL $ROW $MAT 1))
				(SIMPLIFY (MFUNCTION-CALL $COL $ADJ 1)))))
	T))
     (COND ((AND (LIKE (TRD-MSYMEVAL $SCALARMATRIXP '$SCALARMATRIXP) T)
		 (= (MFUNCTION-CALL $LENGTH $MAT) 1))
	      (MARRAYREF $ANS 1 1))
	   (T $ANS)))
   '$ADJ '$ANS))

(compile-forms-to-compile-queue)

