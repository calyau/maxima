;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LMIVAX::MAX$DISK:[SHARE1]DBLINT.MC;2
;;; Written on 9/09/1984 16:32:33, from MACSYMA 302
;;; Translated for LPH

;;; TRANSL-AUTOLOAD version NIL
;;; DCL version NIL TRANSS version 87 TRANSL version 1157
;;; TRUTIL version 27 TRANS1 version 108 TRANS2 version 39
;;; TRANS3 version 50 TRANS4 version 29 TRANS5 version 26
;;; TRANSF version NIL TROPER version 15 TRPRED version 6
;;; MTAGS version NIL MDEFUN version 58 TRANSQ version 88
;;; FCALL version 40 ACALL version 70 TRDATA version 68
;;; MCOMPI version 146 TRMODE version 73 TRHOOK version NIL
(eval-when (compile eval)
      (setq *infile-name-key*
	          (namestring (truename '#.standard-input))))

(eval-when (compile)
   (setq $tr_semicompile 'NIL)
   (setq forms-to-compile-queue ()))

(comment "MAX$DISK:[SHARE1]DBLINT.MC;2")

;;; General declarations required for translated MACSYMA code.

(DECLARE (FIXNUM $DBLINT_Y $DBLINT_X) (SPECIAL $DBLINT_X $DBLINT_Y))

(DECLARE (SPECIAL $DBLINT_Y))

(DECLARE (SPECIAL $DBLINT_X))

(DEFMTRFUN-EXTERNAL ($DBLINT $ANY MDEFINE NIL NIL))


(MEVAL* '(($MODEDECLARE) $DBLINT_Y $FIXNUM))

(MEVAL* '(($DECLARE) $DBLINT_Y $SPECIAL))

(DEFPROP $DBLINT_Y ASSIGN-MODE-CHECK ASSIGN)

(DEF-MTRVAR $DBLINT_Y 10)

(MEVAL* '(($MODEDECLARE) $DBLINT_X $FIXNUM))

(MEVAL* '(($DECLARE) $DBLINT_X $SPECIAL))

(DEFPROP $DBLINT_X ASSIGN-MODE-CHECK ASSIGN)

(DEF-MTRVAR $DBLINT_X 10)

(DEFPROP $DBLINT T TRANSLATED)

(ADD2LNC '$DBLINT $PROPS)

(DEFMTRFUN
  ($DBLINT $ANY MDEFINE NIL NIL) ($F $C $D $A $B) (DECLARE (FLONUM $B $A))
  (PROGN
    NIL
    ((LAMBDA ($M2 $N2 $H $J1 $J2 $J3 $X $DOX $COX $HX $K1 $K2 $K3 $Y $Z $L)
       (DECLARE
	 (FLONUM
	   $L $Z $Y $K3 $K2 $K1 $HX $COX $DOX $X $J3 $J2 $J1 $H $N2 $M2))
       (PROG ()
	    (SETQ $N2 (|//$| 0.5d+0 (FLOAT (TRD-MSYMEVAL $DBLINT_X 0))))
	    (SETQ $M2 (|//$| 0.5d+0 (FLOAT (TRD-MSYMEVAL $DBLINT_Y 0))))
	    (SETQ $H (*$ (+$ $B (-$ $A)) $N2))
	    (SETQ $J1 0.0D+0)
	    (SETQ $J2 0.0D+0)
	    (SETQ $J3 0.0D+0)
	    (DO (($I 0 (+ 1 $I)))
		((> $I (* 2 (TRD-MSYMEVAL $DBLINT_X 0))) '$DONE)
	      NIL
	      (SETQ $X (+$ $A (*$ (FLOAT $I) $H)))
	      (SETQ $DOX (MFUNCALL $D $X))
	      (SETQ $COX (MFUNCALL $C $X))
	      (SETQ $HX (*$ (+$ $DOX (-$ $COX)) $M2))
	      (SETQ $K1 (+$ (MFUNCALL $F $X $COX) (MFUNCALL $F $X $DOX)))
	      (SETQ $K2 0.0D+0)
	      (SETQ $K3 0.0D+0)
	      (DO (($J 1 (+ 1 $J)))
		  ((> $J (+ (* 2 (TRD-MSYMEVAL $DBLINT_Y 0)) -1)) '$DONE)
		NIL
		(SETQ $Y (+$ $COX (*$ (FLOAT $J) $HX)))
		(SETQ $Z (MFUNCALL $F $X $Y))
		(COND ((MFUNCTION-CALL $EVENP $J) (SETQ $K2 (+$ $K2 $Z)))
		      (T (SETQ $K3 (+$ $K3 $Z)))))
	      (SETQ
		$L (|//$| (*$ (+$ $K1 (*$ 2.0d+0 $K2) (*$ 4.0d+0 $K3)) $HX)
			  3.0d+0))
	      (COND ((OR (= $I 0) (= $I (* 2 (TRD-MSYMEVAL $DBLINT_X 0))))
		       (SETQ $J1 (+$ $J1 $L)))
		    ((MFUNCTION-CALL $EVENP $I) (SETQ $J2 (+$ $J2 $L)))
		    (T (SETQ $J3 (+$ $J3 $L)))))
	    (RETURN
	      (|//$|
		(*$ (+$ $J1 (*$ 2.0d+0 $J2) (*$ 4.0d+0 $J3)) $H) 3.0d+0))))
     0.0D+0 0.0D+0 0.0D+0 0.0D+0 0.0D+0 0.0D+0 0.0D+0 0.0D+0
     0.0D+0 0.0D+0 0.0D+0 0.0D+0 0.0D+0 0.0D+0 0.0D+0 0.0D+0)))

(compile-forms-to-compile-queue)

