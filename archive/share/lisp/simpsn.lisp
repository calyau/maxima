;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE2]SIMPSN.MC;1
;;; Written on 9/20/1984 19:48:18, from MACSYMA 302
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

(comment "MAX$DISK:[SHARE2]SIMPSN.MC;1")

;;; General declarations required for translated MACSYMA code.

(DECLARE)

(DEFMTRFUN-EXTERNAL ($TRAPRULE $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($SIMPSON $ANY MDEFINE NIL NIL))


(DEFPROP $TRAPRULE T TRANSLATED)

(ADD2LNC '$TRAPRULE $PROPS)

(DEFMTRFUN
 ($TRAPRULE $ANY MDEFINE NIL NIL) ($F $A $B $N) NIL
 (MUL*
   (DIV (ADD* $B (*MMINUS $A)) $N)
   (ADD*
     (DIV (ADD* (SIMPLIFY (MFUNCALL $F $B)) (SIMPLIFY (MFUNCALL $F $A))) 2)
     (DOSUM
       (FUNGEN&ENV-FOR-MEVALSUMARG
	 ($A $B $N $F) ($I)
	 (SIMPLIFY
	   (MFUNCALL
	     $F (ADD* $A (MUL* (DIV (ADD* $B (*MMINUS $A)) $N) $I))))
	 (($APPLY) $F
	  ((MLIST)
	   ((MPLUS) $A
	    ((MTIMES) ((MQUOTIENT) ((MPLUS) $B ((MMINUS) $A)) $N) $I)))))
       '$I 1 (ADD* $N -1) T))))

(DEFPROP $SIMPSON T TRANSLATED)

(ADD2LNC '$SIMPSON $PROPS)

(DEFMTRFUN
  ($SIMPSON $ANY MDEFINE NIL NIL) ($F $A $B $N) NIL
  (MUL*
    (DIV (DIV (ADD* $B (*MMINUS $A)) $N) 3)
    (ADD*
      (SIMPLIFY (MFUNCALL $F $A)) (SIMPLIFY (MFUNCALL $F $B))
      (MUL*
	4 (DOSUM
	    (FUNGEN&ENV-FOR-MEVALSUMARG
	      ($A $B $N $F) ($I)
	      (SIMPLIFY
		(MFUNCALL $F (ADD* $A (MUL* (DIV (ADD* $B (*MMINUS $A)) $N)
					    (ADD* (MUL* 2 $I) -1)))))
	      (($APPLY) $F
	       ((MLIST)
		((MPLUS) $A
		 ((MTIMES) ((MQUOTIENT) ((MPLUS) $B ((MMINUS) $A)) $N)
		  ((MPLUS) ((MTIMES) 2 $I) ((MMINUS) 1)))))))
	    '$I 1 (DIV $N 2) T))
      (MUL*
	2
	(DOSUM
	  (FUNGEN&ENV-FOR-MEVALSUMARG
	    ($A $B $N $F) ($I)
	    (SIMPLIFY
	      (MFUNCALL
		$F
		(ADD*
		  $A (MUL* (DIV (ADD* $B (*MMINUS $A)) $N) (MUL* 2 $I)))))
	    (($APPLY) $F
	     ((MLIST)
	      ((MPLUS) $A
	       ((MTIMES) ((MQUOTIENT) ((MPLUS) $B ((MMINUS) $A)) $N)
		((MTIMES) 2 $I))))))
	  '$I 1 (DIV (ADD* $N -2) 2) T)))))

(compile-forms-to-compile-queue)

