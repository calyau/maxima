;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE1]NTRIG.MC;1
;;; Written on 9/12/1984 05:31:40, from MACSYMA 302
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

(comment "MAX$DISK:[SHARE1]NTRIG.MC;1")

;;; General declarations required for translated MACSYMA code.

(DECLARE (SPECIAL $N))

(DECLARE (SPECIAL $N))

(DEFMTRFUN-EXTERNAL ($USIN $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($UCOS $ANY MDEFINE NIL NIL))


((PROGN 'COMPILE
	(PROGN 'COMPILE (MEVAL* '(($MODEDECLARE) $N $ANY))
	       (MEVAL* '(($DECLARE) $N $SPECIAL)) NIL (DEF-MTRVAR $N '$N))
	(PROGN 'COMPILE)))

(DEFPROP $USIN T TRANSLATED)

(ADD2LNC '$USIN $PROPS)

(DEFMTRFUN
 ($USIN $ANY MDEFINE NIL NIL) ($N) NIL
 ((LAMBDA ($YUK)
    NIL
    (COND
      ((OR (LIKE $YUK 1) (LIKE $YUK 9))
	 (DIV (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1) 4))
      ((OR (LIKE $YUK 2) (LIKE $YUK 8))
	 (DIV
	   (MUL* (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1)
		 (SIMPLIFY
		   (LIST '(%SQRT) (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5))))
	   (MUL* 4 (SIMPLIFY (LIST '(%SQRT) 2)))))
      ((OR (LIKE $YUK 3) (LIKE $YUK 7))
	 (DIV (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 1) 4))
      ((OR (LIKE $YUK 4) (LIKE $YUK 6))
	 (DIV
	   (SIMPLIFY (LIST '(%SQRT) (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5)))
	   (MUL* 2 (SIMPLIFY (LIST '(%SQRT) 2)))))))
  (SIMPLIFY
    (LIST '(MABS) (SIMPLIFY ($REMAINDER (TRD-MSYMEVAL $N '$N) 10))))))

(DEFPROP $UCOS T TRANSLATED)

(ADD2LNC '$UCOS $PROPS)

(DEFMTRFUN
 ($UCOS $ANY MDEFINE NIL NIL) ($N) NIL
 ((LAMBDA ($YUK)
    NIL
    (COND
      ((LIKE $YUK 1)
	 (DIV
	   (SIMPLIFY (LIST '(%SQRT) (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5)))
	   (MUL* 2 (SIMPLIFY (LIST '(%SQRT) 2)))))
      ((LIKE $YUK 2) (DIV (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 1) 4))
      ((LIKE $YUK 3)
	 (DIV
	   (MUL* (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1)
		 (SIMPLIFY
		   (LIST '(%SQRT) (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5))))
	   (MUL* 4 (SIMPLIFY (LIST '(%SQRT) 2)))))
      ((LIKE $YUK 4) (DIV (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1) 4))
      ((LIKE $YUK 6)
	 (DIV (*MMINUS (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1)) 4))
      ((LIKE $YUK 7)
	 (DIV
	   (MUL* (*MMINUS (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1))
		 (SIMPLIFY
		   (LIST '(%SQRT) (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5))))
	   (MUL* 4 (SIMPLIFY (LIST '(%SQRT) 2)))))
      ((LIKE $YUK 8)
	 (DIV (*MMINUS (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 1)) 4))
      ((LIKE $YUK 9)
	 (DIV (*MMINUS
		(SIMPLIFY
		  (LIST '(%SQRT) (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5))))
	      (MUL* 2 (SIMPLIFY (LIST '(%SQRT) 2)))))))
  (SIMPLIFY
    (LIST '(MABS) (SIMPLIFY ($REMAINDER (TRD-MSYMEVAL $N '$N) 10))))))

(compile-forms-to-compile-queue)

