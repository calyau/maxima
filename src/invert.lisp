;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 5/12/85 13:57:48;;Maxima System version 8
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
 (DEFPROP $ADJOINT T TRANSLATED)
 (ADD2LNC '$ADJOINT $PROPS)
 (DEFMTRFUN
  ($ADJOINT $ANY MDEFINE NIL NIL)
  ($MAT)
  NIL
  ((LAMBDA
    ($ADJ $N)
    NIL
    (SETQ $N ($LENGTH $MAT))
    (SETQ $ADJ (SIMPLIFY ($IDENT $N)))
    (COND
     ((NOT (LIKE $N 1))
      (DO (($I 1 (f+ 1 $I)))
          ((> $I $N) '$DONE)
        (DO (($J 1 (f+ 1 $J)))
            ((> $J $N) '$DONE)
         (MASET (MUL* (POWER -1 (f+ $I $J))
                      (SIMPLIFY ($DETERMINANT (SIMPLIFY ($MINOR $MAT
                                                                $J
                                                                $I)))))
                $ADJ
                $I
                $J)))))
    $ADJ)
   '$ADJ
   '$N)))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (DEFPROP $INVERT T TRANSLATED)
       (ADD2LNC '$INVERT $PROPS)
       (DEFMTRFUN ($INVERT $ANY MDEFINE NIL NIL)
                  ($MAT)
                  NIL
                  ((LAMBDA ($ADJ $ANS)
                       NIL
                       (SETQ $ADJ (SIMPLIFY ($ADJOINT $MAT)))
                       (SETQ $ANS ((LAMBDA ($SCALARMATRIXP)
                                       NIL
                                       (DIV $ADJ
                                            (NCMUL2 (SIMPLIFY ($ROW $MAT 1))
                                                    (SIMPLIFY ($COL $ADJ
                                                                    1)))))
                                   T))
                       (COND ((AND (LIKE (TRD-MSYMEVAL $SCALARMATRIXP
                                                       '$SCALARMATRIXP)
                                         T)
                                   (EQL ($LENGTH $MAT) 1))
                              (MAREF $ANS 1 1))
                             (T $ANS)))
                   '$ADJ
                   '$ANS)))