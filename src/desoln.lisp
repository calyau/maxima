;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 5/12/85 13:15:46;;Maxima System version 8
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
 (DEFPROP $DESOLVE T TRANSLATED)
 (ADD2LNC '$DESOLVE $PROPS)
 (DEFMTRFUN
  ($DESOLVE $ANY MDEFINE NIL NIL)
  ($EQNS $VARS)
  NIL
  ((LAMBDA
    ($TEQNS $TVARS $OVAR $LVAR $FLAG $DISPFLAG)
    NIL
    NIL
    (SETQ $FLAG NIL)
    (COND ((NOT ($LISTP $VARS))
           (SETQ $EQNS (LIST '(MLIST) $EQNS))
           (SETQ $VARS (LIST '(MLIST) $VARS))
           (SETQ $FLAG T)))
    (COND
     ((NOT (EQL ($LENGTH (SETQ $OVAR (MAREF $VARS 1)))
                1))
      (SIMPLIFY ($ERROR $OVAR
                        '|&contains more than one independent variable.|))))
    (SETQ $OVAR (SIMPLIFY ($INPART $OVAR 1)))
    (SETQ $DISPFLAG NIL)
    (SETQ
     $TEQNS
     (SIMPLIFY (MAP1 (GETOPR (M-TLAMBDA&ENV (($Z) ($OVAR $LVAR))
                                            NIL
                                            (SIMPLIFY ($LAPLACE $Z
                                                                $OVAR
                                                                $LVAR))))
                     $EQNS)))
    (SETQ
     $TVARS
     (SIMPLIFY (MAP1 (GETOPR (M-TLAMBDA&ENV (($Z) ($OVAR $LVAR))
                                            NIL
                                            (SIMPLIFY `((%LAPLACE) ,$Z ,
                                                        $OVAR ,$LVAR))))
                     $VARS)))
    (SETQ
     $TEQNS
     ((LAMBDA (ERRCATCH RET)
          (COND ((NULL (SETQ RET (ERRSET (PROGN (SIMPLIFY ($SOLVE $TEQNS
                                                                  $TVARS)))
                                         LISPERRPRINT)))
                 (ERRLFUN1 ERRCATCH)))
          (CONS '(MLIST) RET))
      (CONS BINDLIST LOCLIST)
      NIL))
    (COND ((OR (LIKE $TEQNS '((MLIST)))
               (LIKE $TEQNS (LIST '(MLIST) '((MLIST)))))
           (SIMPLIFY ($ERROR '|&DESOLVE can't handle this case.|)))
          (T (SETQ $TEQNS (SIMPLIFY ($FIRST $TEQNS)))))
    (COND ((NOT (LIKE $FLAG T))
           (SETQ $TEQNS (SIMPLIFY ($FIRST $TEQNS)))))
    (SETQ
     $TEQNS
     (SIMPLIFY (MAP1 (GETOPR (M-TLAMBDA&ENV (($Z) ($LVAR $OVAR))
                                            NIL
                                            (SIMPLIFY ($ILT $Z
                                                            $LVAR
                                                            $OVAR))))
                     $TEQNS)))
    (COND ((AND $FLAG (EQL ($LENGTH $TVARS) 1))
           (MAREF $TEQNS 1))
          (T $TEQNS)))
   '$TEQNS
   '$TVARS
   '$OVAR
   '$LVAR
   '$FLAG
   '$DISPFLAG)))