;;; -*- Mode: Lisp; package:maxima; syntax:common-lisp ;Base: 10 -*- ;;;

(in-package "MAXIMA")
;;** Variable settings were **

;;TRANSCOMPILE:FALSE;
;;TR\_SEMICOMPILE:FALSE;
;;TRANSLATE\_FAST\_ARRAYS:TRUE;
;;TR\_WARN\_UNDECLARED:COMPILE;
;;TR\_WARN\_MEVAL:COMPFILE;
;;TR\_WARN\_FEXPR:COMPFILE;
;;TR\_WARN\_MODE:ALL;
;;TR\_WARN\_UNDEFINED\_VARIABLE:ALL;
;;TR\_FUNCTION\_CALL\_DEFAULT:GENERAL;
;;TR\_ARRAY\_AS\_REF:TRUE;
;;TR\_NUMER:FALSE;
;;DEFINE\_VARIABLE:FALSE;

(PROGN
  (DEFPROP $ELIMINATE T TRANSLATED)
  (ADD2LNC '$ELIMINATE $PROPS)
  (DEFMTRFUN ($ELIMINATE $ANY MDEFINE NIL NIL) ($EQNS $VARS) NIL
      ((LAMBDA ($TEQNS $SV $SE $L $FLAG $DISPFLAG)
         NIL
         NIL
         (SETQ $FLAG (SETQ $DISPFLAG NIL))
         (COND
           ((NOT (AND (MFUNCTION-CALL $LISTP $EQNS)
                      (MFUNCTION-CALL $LISTP $VARS)))
            (SIMPLIFY
                (MFUNCTION-CALL $ERROR
                    '|&THE ARGUMENTS MUST BOTH BE LISTS|))))
         (COND
           ((> (MFUNCTION-CALL $LENGTH $VARS)
               (SETQ $L (MFUNCTION-CALL $LENGTH $EQNS)))
            (SIMPLIFY
                (MFUNCTION-CALL $ERROR
                    '|&MORE VARIABLES THEN EQUATIONS|))))
         (COND
           ((EQL $L 1)
            (SIMPLIFY
                (MFUNCTION-CALL $ERROR
                    '|&CAN'T ELIMINATE FROM ONLY ONE EQUATION|))))
         (COND
           ((EQL (MFUNCTION-CALL $LENGTH $VARS) $L)
            (SETQ $VARS (SIMPLIFY (MFUNCTION-CALL $REVERSE $VARS)))
            (SETQ $SV (MAREF $VARS 1))
            (SETQ $VARS
                  (SIMPLIFY
                      (MFUNCTION-CALL $REVERSE
                          (SIMPLIFY (MFUNCTION-CALL $REST $VARS)))))
            (SETQ $FLAG T)))
         (SETQ $EQNS (SIMPLIFY (MAP1 (GETOPR 'MEQHK) $EQNS)))
         (DO (($V) (MDO (CDR $VARS) (CDR MDO))) ((NULL MDO) '$DONE)
           (SETQ $V (CAR MDO))
           (SETQ $TEQNS '((MLIST)))
           (DO (($J 1 (F+ 1 $J)))
               ((OR (> $J $L)
                    (NOT (MFUNCTION-CALL $FREEOF $V
                             (SIMPLIFY ($FIRST $EQNS)))))
                '$DONE)
             (SETQ $TEQNS
                   (SIMPLIFY
                       (MFUNCTION-CALL $CONS (SIMPLIFY ($FIRST $EQNS))
                           $TEQNS)))
             (SETQ $EQNS (SIMPLIFY (MFUNCTION-CALL $REST $EQNS))))
           (COND
             ((LIKE $EQNS '((MLIST))) (SETQ $EQNS $TEQNS))
             (T (SETQ $TEQNS
                      (SIMPLIFY
                          (MFUNCTION-CALL $APPEND $TEQNS
                              (SIMPLIFY (MFUNCTION-CALL $REST $EQNS)))))
                (SETQ $EQNS (SIMPLIFY ($FIRST $EQNS)))
                (SETQ $L (F+ $L -1)) (SETQ $SE '((MLIST)))
                (DO (($J 1 (F+ 1 $J))) ((> $J $L) '$DONE)
                  (SETQ $SE
                        (SIMPLIFY
                            (MFUNCTION-CALL $CONS
                                (SIMPLIFY
                                    (MFUNCTION-CALL $RESULTANT $EQNS
                                     (MAREF $TEQNS $J) $V))
                                $SE))))
                (SETQ $EQNS $SE))))
         (COND
           ($FLAG (LIST '(MLIST)
                        (SIMPLIFY
                            (MFUNCTION-CALL $RHS
                                (SIMPLIFY
                                    (MFUNCALL '$EV
                                     (SIMPLIFY
                                      (MFUNCTION-CALL $LAST
                                       (SIMPLIFY
                                        (MFUNCTION-CALL $SOLVE
                                         (MAREF $EQNS 1) $SV))))
                                     '$EVAL))))))
           (T $EQNS)))
       '$TEQNS '$SV '$SE 0 '$FLAG '$DISPFLAG)))