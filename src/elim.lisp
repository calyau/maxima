;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 5/12/85 13:46:23;;Maxima System version 8
;;** Variable settings were **

(in-package "MAXIMA")

;;TRANSCOMPILE:FALSE;
;;TR_SEMICOMPILE:FALSE;
;;TRANSLATE_FAST_ARRAYS:TRUE;
;;TR_WARN_UNDECLARED:COMPILE;
;;TR_WARN_MEVAL:COMPFILE;
;;TR_WARN_FEXPR:COMPFILE;
;;TR_WARN_MODE:ALL;
;;TR_WARN_UNDEFINED_VARIABLE:ALL;
;;TR_FUNCTION_CALL_DEFAULT:GENERAL;
;;TR_ARRAY_AS_REF:TRUE;
;;TR_NUMER:FALSE;
;;DEFINE_VARIABLE:FALSE;
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $ELIMINATE T TRANSLATED)
 (ADD2LNC '$ELIMINATE $PROPS)
 (DEFMTRFUN
  ($ELIMINATE $ANY MDEFINE NIL NIL)
  ($EQNS $VARS)
  NIL
  ((LAMBDA
    ($TEQNS $SV $SE $L $FLAG $DISPFLAG)
    NIL
    NIL
    (SETQ $FLAG (SETQ $DISPFLAG NIL))
    (COND ((NOT (AND ($LISTP $EQNS)
                     ($LISTP $VARS)))
           (SIMPLIFY ($ERROR '|&THE ARGUMENTS MUST BOTH BE LISTS|))))
    (COND ((> ($LENGTH $VARS)
              (SETQ $L ($LENGTH $EQNS)))
           (SIMPLIFY ($ERROR '|&MORE VARIABLES THEN EQUATIONS|))))
    (COND ((EQL $L 1)
           (SIMPLIFY ($ERROR '|&CAN'T ELIMINATE FROM ONLY ONE EQUATION|))))
    (COND ((EQL ($LENGTH $VARS) $L)
           (SETQ $VARS ($REVERSE $VARS))
           (SETQ $SV (MAREF $VARS 1))
           (SETQ $VARS ($REVERSE (SIMPLIFY ($REST $VARS))))
           (SETQ $FLAG T)))
    (SETQ $EQNS (SIMPLIFY (MAP1 (GETOPR 'MEQHK) $EQNS)))
    (DO (($V)
         (MDO (CDR $VARS) (CDR MDO)))
        ((NULL MDO) '$DONE)
      (SETQ $V (CAR MDO))
      (SETQ $TEQNS '((MLIST)))
      (DO (($J 1 (f+ 1 $J)))
          ((OR (> $J $L)
               (NOT ($FREEOF $V (SIMPLIFY ($FIRST $EQNS)))))
           '$DONE)
       (SETQ $TEQNS ($CONS (SIMPLIFY ($FIRST $EQNS)) $TEQNS))
       (SETQ $EQNS (SIMPLIFY ($REST $EQNS))))
      (COND ((LIKE $EQNS '((MLIST)))
             (SETQ $EQNS $TEQNS))
            (T
             (SETQ $TEQNS ($APPEND $TEQNS (SIMPLIFY ($REST $EQNS))))
             (SETQ $EQNS (SIMPLIFY ($FIRST $EQNS)))
             (SETQ $L (ADD* $L -1))
             (SETQ $SE '((MLIST)))
             (DO (($J 1 (f+ 1 $J)))
                 ((> $J $L) '$DONE)
               (SETQ $SE ($CONS (SIMPLIFY ($RESULTANT $EQNS
                                                      (MAREF $TEQNS $J)
                                                      $V))
                                $SE)))
             (SETQ $EQNS $SE))))
    (COND
     ($FLAG
      (LIST
       '(MLIST)
       ($RHS
        (SIMPLIFY
         (MFUNCALL '$EV
                   (SIMPLIFY ($LAST (SIMPLIFY ($SOLVE (MAREF $EQNS 1)
                                                      $SV))))
                   '$EVAL)))))
     (T $EQNS)))
   '$TEQNS
   '$SV
   '$SE
   '$L
   '$FLAG
   '$DISPFLAG)))