;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;; 
;;;Translated on: 7/18/85 13:12:45;;Maxima System version 2
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





NIL
(EVAL-WHEN (COMPILE LOAD EVAL) (MEVAL* '(($DECLARE) $PT $SPECIAL $YP 
                                             $SPECIAL $YOLD $SPECIAL $%Q%
					     $SPECIAL $YNEW
                                             $SPECIAL $X $SPECIAL $Y $SPECIAL 
                                             $METHOD $SPECIAL $%F% $SPECIAL $%G%
                                             $SPECIAL $MSG1 $SPECIAL $MSG2 
                                             $SPECIAL $INTFACTOR $SPECIAL 
                                             $ODEINDEX $SPECIAL $SINGSOLVE 
                                             $SPECIAL)))
#+symbolics  ;;above doesn't affect compiler for some reasons
(declare (special $PT  $YP                                          $YOLD  $%Q% 
                                              $X  $Y  
                                             $METHOD  $%F%  $%G%
                                              $MSG1  $MSG2 
                                              $INTFACTOR  
                                             $ODEINDEX  $SINGSOLVE ))


(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $ODE2 T TRANSLATED)
 (ADD2LNC '$ODE2 $PROPS)
 (DEFMTRFUN
  ($ODE2 $ANY MDEFINE NIL NIL)
  ($EQ $YOLD $X)
  NIL
  ((LAMBDA
    ($DERIVSUBST $YNEW)
    NIL
    (SIMPLIFY
     ($SUBSTITUTE
      (TRD-MSYMEVAL $YOLD '$YOLD)
      $YNEW
      (SIMPLIFY ($ODE2A (SIMPLIFY ($SUBSTITUTE $YNEW
                                               (TRD-MSYMEVAL $YOLD
                                                             '$YOLD)
                                               $EQ))
                        $YNEW
                        (TRD-MSYMEVAL $X '$X))))))
   NIL
   '$YNEW)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $ODE2A T TRANSLATED)
 (ADD2LNC '$ODE2A $PROPS)
 (DEFMTRFUN
  ($ODE2A $ANY MDEFINE NIL NIL)
  ($EQ $Y $X)
  NIL
  ((LAMBDA
    ($DE $A1 $A2 $A3 $A4 $%Q% $MSG1)
    NIL
    (PROG
     NIL
     (SETQ $INTFACTOR NIL)
     (SETQ $METHOD '$NONE)
     (COND (($FREEOF (SIMPLIFY `((%DERIVATIVE) ,(TRD-MSYMEVAL $Y '$Y) ,
                                 (TRD-MSYMEVAL $X '$X) 2))
                     $EQ)
            (COND (($FTEST (SIMPLIFY ($ODE1A $EQ
                                             (TRD-MSYMEVAL $Y '$Y)
                                             (TRD-MSYMEVAL $X '$X))))
                   (RETURN (TRD-MSYMEVAL $%Q% '$%Q%)))
                  (T (RETURN NIL)))))
     (COND
      ((NOT
        (LIKE
         (SIMPLIFY
          ($DERIVDEGREE
           (SETQ $DE (SIMPLIFY ($DESIMP (ADD* ($LHS $EQ)
                                              (*MMINUS ($RHS $EQ))))))
           (TRD-MSYMEVAL $Y '$Y)
           (TRD-MSYMEVAL $X '$X)))
         2))
       (RETURN ($FAILURE (TRD-MSYMEVAL $MSG1 '$MSG1) $EQ))))
     (SETQ $A1 (SIMPLIFY ($COEFF $DE
                                 (SIMPLIFY `((%DERIVATIVE) ,
                                             (TRD-MSYMEVAL $Y '$Y) ,
                                             (TRD-MSYMEVAL $X '$X) 2)))))
     (SETQ $A2 (SIMPLIFY ($COEFF $DE
                                 (SIMPLIFY `((%DERIVATIVE) ,
                                             (TRD-MSYMEVAL $Y '$Y) ,
                                             (TRD-MSYMEVAL $X '$X))))))
     (SETQ $A3 (SIMPLIFY ($COEFF $DE (TRD-MSYMEVAL $Y '$Y))))
     (SETQ
      $A4
      (SIMPLIFY
       ($EXPAND
        (ADD* $DE
              (*MMINUS (MUL* $A1
                             (SIMPLIFY `((%DERIVATIVE) ,
                                         (TRD-MSYMEVAL $Y '$Y) ,
                                         (TRD-MSYMEVAL $X '$X) 2))))
              (MUL* (*MMINUS $A2)
                    (SIMPLIFY `((%DERIVATIVE) ,(TRD-MSYMEVAL $Y '$Y) ,
                                (TRD-MSYMEVAL $X '$X))))
              (MUL* (*MMINUS $A3) (TRD-MSYMEVAL $Y '$Y))))))
     (COND ((AND ($PR2 $A1)
                 ($PR2 $A2)
                 ($PR2 $A3)
                 ($PR2 $A4)
                 ($FTEST (SIMPLIFY ($HOM2 $A1 $A2 $A3))))
            (COND ((LIKE $A4 0)
                   (RETURN (TRD-MSYMEVAL $%Q% '$%Q%)))
                  (T (RETURN (SIMPLIFY ($VARP (TRD-MSYMEVAL $%Q% '$%Q%)
                                              (DIV (*MMINUS $A4) $A1))))))))
     (RETURN (COND (($FTEST (SIMPLIFY ($REDUCE $DE)))
                    (RETURN (TRD-MSYMEVAL $%Q% '$%Q%)))
                   (T (RETURN NIL))))))
   '$DE
   '$A1
   '$A2
   '$A3
   '$A4
   '$%Q%
   '$MSG1)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $ODE1A T TRANSLATED)
 (ADD2LNC '$ODE1A $PROPS)
 (DEFMTRFUN
  ($ODE1A $ANY MDEFINE NIL NIL)
  ($EQ $Y $X)
  NIL
  ((LAMBDA
    ($DE $DES)
    NIL
    (PROG
     NIL
     (COND
      ((NOT
        (LIKE
         (SIMPLIFY
          ($DERIVDEGREE
           (SETQ $DE (SIMPLIFY ($EXPAND (ADD* ($LHS $EQ)
                                              (*MMINUS ($RHS $EQ))))))
           (TRD-MSYMEVAL $Y '$Y)
           (TRD-MSYMEVAL $X '$X)))
         1))
       (RETURN ($FAILURE (TRD-MSYMEVAL $MSG1 '$MSG1) $EQ))))
     (COND ((LIKE (SIMPLIFY ($LINEAR2 $DE
                                      (SIMPLIFY `((%DERIVATIVE) ,
                                                  (TRD-MSYMEVAL $Y '$Y) ,
                                                  (TRD-MSYMEVAL $X '$X)))))
                  NIL)
            (RETURN ($FAILURE (TRD-MSYMEVAL $MSG2 '$MSG2) $EQ))))
     (SETQ $DES (SIMPLIFY ($DESIMP $DE)))
     (SETQ $DE (SIMPLIFY ($SOLVE1 $DES
                                  (SIMPLIFY `((%DERIVATIVE) ,
                                              (TRD-MSYMEVAL $Y '$Y) ,
                                              (TRD-MSYMEVAL $X '$X))))))
     (COND (($FTEST (SIMPLIFY ($SOLVELNR $DE)))
            (RETURN (TRD-MSYMEVAL $%Q% '$%Q%))))
     (COND (($FTEST (SIMPLIFY ($SEPARABLE $DE)))
            (RETURN (TRD-MSYMEVAL $%Q% '$%Q%))))
     (COND (($FTEST (SIMPLIFY ($INTEGFACTOR (TRD-MSYMEVAL $%G% '$%G%)
                                            (TRD-MSYMEVAL $%F% '$%F%))))
            (RETURN (SIMPLIFY ($EXACT (MUL* (TRD-MSYMEVAL $%Q% '$%Q%)
                                            (TRD-MSYMEVAL $%G% '$%G%))
                                      (MUL* (TRD-MSYMEVAL $%Q% '$%Q%)
                                            (TRD-MSYMEVAL $%F% '$%F%)))))))
     (COND ((LIKE (SIMPLIFY ($LINEAR2 $DES
                                      (SIMPLIFY `((%DERIVATIVE) ,
                                                  (TRD-MSYMEVAL $Y '$Y) ,
                                                  (TRD-MSYMEVAL $X '$X)))))
                  NIL)
            (RETURN ($FAILURE (TRD-MSYMEVAL $MSG2 '$MSG2) $EQ))))
     (COND (($FTEST (SIMPLIFY ($INTEGFACTOR (TRD-MSYMEVAL $%G% '$%G%)
                                            (TRD-MSYMEVAL $%F% '$%F%))))
            (RETURN (SIMPLIFY ($EXACT (MUL* (TRD-MSYMEVAL $%Q% '$%Q%)
                                            (TRD-MSYMEVAL $%G% '$%G%))
                                      (MUL* (TRD-MSYMEVAL $%Q% '$%Q%)
                                            (TRD-MSYMEVAL $%F% '$%F%)))))))
     (COND (($FTEST (SIMPLIFY ($SOLVEHOM $DE)))
            (RETURN (TRD-MSYMEVAL $%Q% '$%Q%))))
     (COND (($FTEST (SIMPLIFY ($SOLVEBERNOULLI $DE)))
            (RETURN (TRD-MSYMEVAL $%Q% '$%Q%))))
     (RETURN (COND (($FTEST (SIMPLIFY ($GENHOM $DE)))
                    (RETURN (TRD-MSYMEVAL $%Q% '$%Q%)))
                   (T (RETURN NIL))))))
   '$DE
   '$DES)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $DESIMP T TRANSLATED)
 (ADD2LNC '$DESIMP $PROPS)
 (DEFMTRFUN
  ($DESIMP $ANY MDEFINE NIL NIL)
  ($EQ)
  NIL
  ((LAMBDA
    ($INFLAG)
    NIL
    (PROG
     NIL
     (SETQ $EQ (SIMPLIFY ($FACTOR $EQ)))
     (COND ((OR ($ATOM $EQ)
                (NOT (LIKE (SIMPLIFY ($INPART $EQ 0)) '&*)))
            (RETURN (SIMPLIFY ($EXPAND $EQ)))))
     (SETQ
      $EQ
      (SIMPLIFY
       (MAP1
        (GETOPR (M-TLAMBDA ($U)
                           NIL
                           (COND (($FREEOF (SIMPLIFY ($NOUNIFY '$DIFF))
                                           $U)
                                  1)
                                 (T $U))))
        $EQ)))
     (RETURN (SIMPLIFY ($EXPAND $EQ)))))
   T)))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (DEFPROP $PR2 T TRANSLATED)
       (ADD2LNC '$PR2 $PROPS)
       (DEFMTRFUN ($PR2 $BOOLEAN MDEFINE NIL NIL)
                  ($%F%)
                  NIL
                  ($FREEOF (TRD-MSYMEVAL $Y '$Y)
                           (SIMPLIFY `((%DERIVATIVE) ,(TRD-MSYMEVAL $Y '$Y) ,
                                       (TRD-MSYMEVAL $X '$X)))
                           (SIMPLIFY `((%DERIVATIVE) ,(TRD-MSYMEVAL $Y '$Y) ,
                                       (TRD-MSYMEVAL $X '$X) 2))
                           (TRD-MSYMEVAL $%F% '$%F%))))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (DEFPROP $FTEST T TRANSLATED)
       (ADD2LNC '$FTEST $PROPS)
       (DEFMTRFUN ($FTEST $BOOLEAN MDEFINE NIL NIL)
                  ($CALL)
                  NIL
                  (NOT (LIKE (SETQ $%Q% $CALL)
                             NIL))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $SOLVE1 T TRANSLATED)
 (ADD2LNC '$SOLVE1 $PROPS)
 (DEFMTRFUN ($SOLVE1 $ANY MDEFINE NIL NIL)
            ($EQ $Y)
            NIL
            ((LAMBDA ($PROGRAMMODE)
               NIL
               (SIMPLIFY ($FIRST (SIMPLIFY ($SOLVE $EQ
                                                   (TRD-MSYMEVAL $Y '$Y))))))
             T)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $LINEAR2 T TRANSLATED)
 (ADD2LNC '$LINEAR2 $PROPS)
 (DEFMTRFUN
  ($LINEAR2 $ANY MDEFINE NIL NIL)
  ($EXPR $X)
  NIL
  ((LAMBDA
    NIL
    NIL
    (PROG
     NIL
     (SETQ $%F% (SIMPLIFY ($RATCOEF $EXPR (TRD-MSYMEVAL $X '$X))))
     (COND ((NOT ($FREEOF (TRD-MSYMEVAL $X '$X) (TRD-MSYMEVAL $%F% '$%F%)))
            (RETURN NIL)))
     (SETQ
      $%G%
      (SIMPLIFY ($RATSIMP (ADD* $EXPR
                                (*MMINUS (MUL* (TRD-MSYMEVAL $%F%
                                                             '$%F%)
                                               (TRD-MSYMEVAL $X '$X)))))))
     (RETURN ($FREEOF (TRD-MSYMEVAL $X '$X) (TRD-MSYMEVAL $%G% '$%G%))))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $SOLVELNR T TRANSLATED)
 (ADD2LNC '$SOLVELNR $PROPS)
 (DEFMTRFUN
  ($SOLVELNR $ANY MDEFINE NIL NIL)
  ($EQ)
  NIL
  ((LAMBDA
    ($%F% $%G% $W $%C)
    NIL
    (PROG
     NIL
     (COND ((LIKE (SIMPLIFY ($LINEAR2 ($RHS $EQ) (TRD-MSYMEVAL $Y '$Y)))
                  NIL)
            (RETURN NIL)))
     (SETQ
      $W
      (SIMPLIFY ($EXP (SIMPLIFY ($INTEGRATE (TRD-MSYMEVAL $%F% '$%F%)
                                            (TRD-MSYMEVAL $X '$X))))))
     (SETQ $METHOD '$LINEAR)
     (RETURN
      (SIMPLIFY
       (LIST
        '(MEQUAL)
        (TRD-MSYMEVAL $Y '$Y)
        (MUL* $W
              (ADD* (SIMPLIFY ($INTEGRATE (DIV (TRD-MSYMEVAL $%G%
                                                             '$%G%)
                                               $W)
                                          (TRD-MSYMEVAL $X '$X)))
                    $%C)))))))
   '$%F%
   '$%G%
   '$W
   '$%C)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $SEPARABLE T TRANSLATED)
 (ADD2LNC '$SEPARABLE $PROPS)
 (DEFMTRFUN
  ($SEPARABLE $ANY MDEFINE NIL NIL)
  ($EQ)
  NIL
  ((LAMBDA
    ($XPART $YPART $FLAG $INFLAG $%C)
    NIL
    (PROG
     NIL
     (SETQ $EQ (SIMPLIFY ($FACTOR ($RHS $EQ))))
     (COND ((OR ($ATOM $EQ)
                (NOT (LIKE (SIMPLIFY ($INPART $EQ 0)) '&*)))
            (SETQ $EQ (LIST '(MLIST) $EQ))))
     (DO (($U)
          (MDO (CDR $EQ) (CDR MDO)))
         ((NULL MDO) '$DONE)
       (SETQ $U (CAR MDO))
       (COND (($FREEOF (TRD-MSYMEVAL $X '$X) $U)
              (SETQ $YPART ($CONS $U $YPART)))
             (($FREEOF (TRD-MSYMEVAL $Y '$Y) $U)
              (SETQ $XPART ($CONS $U $XPART)))
             (T (RETURN (SETQ $FLAG T)))))
     (COND ((LIKE $FLAG T)
            (RETURN NIL)))
     (COND ((LIKE $XPART '((MLIST)))
            (SETQ $XPART 1))
           (T (SETQ $XPART (SIMPLIFY (MAPPLY-TR '&* $XPART)))))
     (COND ((LIKE $YPART '((MLIST)))
            (SETQ $YPART 1))
           (T (SETQ $YPART (SIMPLIFY (MAPPLY-TR '&* $YPART)))))
     (SETQ $METHOD '$SEPARABLE)
     (RETURN
      (SIMPLIFY
       (LIST
        '(MEQUAL)
        (SIMPLIFY ($RATSIMP (SIMPLIFY ($INTEGRATE (DIV 1 $YPART)
                                                  (TRD-MSYMEVAL $Y
                                                                '$Y)))))
        (ADD*
         (SIMPLIFY ($RATSIMP (SIMPLIFY ($INTEGRATE $XPART
                                                   (TRD-MSYMEVAL $X
                                                                 '$X)))))
         $%C))))))
   '((MLIST))
   '((MLIST))
   NIL
   T
   '$%C)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $INTEGFACTOR T TRANSLATED)
 (ADD2LNC '$INTEGFACTOR $PROPS)
 (DEFMTRFUN
  ($INTEGFACTOR $ANY MDEFINE NIL NIL)
  ($M $N)
  NIL
  ((LAMBDA
    ($B1 $B2 $DMDX $DMDY $DNDX $DNDY $DD $%E_TO_NUMLOG)
    NIL
    (PROG
     NIL
     (SETQ $DMDY (SIMPLIFY ($RATSIMP (SIMPLIFY ($DIFF $M
                                                      (TRD-MSYMEVAL $Y
                                                                    '$Y))))))
     (SETQ $DNDX (SIMPLIFY ($RATSIMP (SIMPLIFY ($DIFF $N
                                                      (TRD-MSYMEVAL $X
                                                                    '$X))))))
     (COND ((LIKE (SETQ $DD (ADD* $DMDY (*MMINUS $DNDX)))
                  0)
            (RETURN 1)))
     (SETQ $DMDX (SIMPLIFY ($RATSIMP (SIMPLIFY ($DIFF $M
                                                      (TRD-MSYMEVAL $X
                                                                    '$X))))))
     (SETQ $DNDY (SIMPLIFY ($RATSIMP (SIMPLIFY ($DIFF $N
                                                      (TRD-MSYMEVAL $Y
                                                                    '$Y))))))
     (COND ((AND (LIKE (ADD* $DMDX (*MMINUS $DNDY)) 0)
                 (LIKE (ADD* $DMDY $DNDX) 0))
            (RETURN (DIV 1 (ADD* (POWER $M 2) (POWER $N 2))))))
     (COND
      (($FREEOF (TRD-MSYMEVAL $Y '$Y)
                (SETQ $B1 (SIMPLIFY ($RATSIMP (DIV $DD $N)))))
       (RETURN
        (SIMPLIFY ($EXP (SIMPLIFY ($INTEGRATE $B1
                                              (TRD-MSYMEVAL $X '$X))))))))
     (RETURN
      (COND
       (($FREEOF (TRD-MSYMEVAL $X '$X)
                 (SETQ $B2 (SIMPLIFY ($RATSIMP (DIV $DD $M)))))
        (RETURN
         (SIMPLIFY ($EXP (SIMPLIFY ($INTEGRATE (*MMINUS $B2)
                                               (TRD-MSYMEVAL $Y '$Y)))))))
       (T (RETURN NIL))))))
   '$B1
   '$B2
   '$DMDX
   '$DMDY
   '$DNDX
   '$DNDY
   '$DD
   T)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $EXACT T TRANSLATED)
 (ADD2LNC '$EXACT $PROPS)
 (DEFMTRFUN
  ($EXACT $ANY MDEFINE NIL NIL)
  ($M $N)
  NIL
  ((LAMBDA
    ($A $YNEW $%C)
    NIL
    (PROG
     NIL
     (SETQ $INTFACTOR (SIMPLIFY ($SUBSTITUTE (TRD-MSYMEVAL $YOLD '$YOLD)
                                             $YNEW
                                             (TRD-MSYMEVAL $%Q% '$%Q%))))
     (SETQ $A (SIMPLIFY ($INTEGRATE (SIMPLIFY ($RATSIMP $M))
                                    (TRD-MSYMEVAL $X '$X))))
     (SETQ $METHOD '$EXACT)
     (RETURN
      (SIMPLIFY
       (LIST
        '(MEQUAL)
        (SIMPLIFY
         ($RATSIMP
          (ADD*
           $A
           (SIMPLIFY
            ($INTEGRATE
             (SIMPLIFY
              ($RATSIMP
               (ADD*
                $N
                (*MMINUS (SIMPLIFY ($DIFF $A
                                          (TRD-MSYMEVAL $Y '$Y)))))))
             (TRD-MSYMEVAL $Y '$Y))))))
        $%C)))))
   '$A
   '$YNEW
   '$%C)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $SOLVEHOM T TRANSLATED)
 (ADD2LNC '$SOLVEHOM $PROPS)
 (DEFMTRFUN
  ($SOLVEHOM $ANY MDEFINE NIL NIL)
  ($EQ)
  NIL
  ((LAMBDA
    ($QQ $A1 $A2 $%C)
    NIL
    (PROG
     NIL
     (SETQ
      $A1
      (SIMPLIFY ($RATSIMP (SIMPLIFY ($SUBSTITUTE (MUL* (TRD-MSYMEVAL $X
                                                                     '$X)
                                                       $QQ)
                                                 (TRD-MSYMEVAL $Y '$Y)
                                                 ($RHS $EQ))))))
     (COND ((NOT ($FREEOF (TRD-MSYMEVAL $X '$X) $A1))
            (RETURN NIL)))
     (SETQ
      $A2
      (SIMPLIFY
       ($RATSIMP
        (SIMPLIFY
         ($SUBSTITUTE (DIV (TRD-MSYMEVAL $Y '$Y) (TRD-MSYMEVAL $X '$X))
                      $QQ
                      (SIMPLIFY ($INTEGRATE (DIV 1
                                                 (ADD* $A1
                                                       (*MMINUS $QQ)))
                                            $QQ)))))))
     (SETQ $METHOD '$HOMOGENEOUS)
     (RETURN (SIMPLIFY (LIST '(MEQUAL)
                             (MUL* $%C (TRD-MSYMEVAL $X '$X))
                             (SIMPLIFY ($EXP $A2)))))))
   '$QQ
   '$A1
   '$A2
   '$%C)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $SOLVEBERNOULLI T TRANSLATED)
 (ADD2LNC '$SOLVEBERNOULLI $PROPS)
 (DEFMTRFUN
  ($SOLVEBERNOULLI $ANY MDEFINE NIL NIL)
  ($EQ)
  NIL
  ((LAMBDA
    ($A1 $A2 $N $%C)
    NIL
    (PROG
     NIL
     (SETQ $A1 (SIMPLIFY ($COEFF (SETQ $EQ (SIMPLIFY ($EXPAND ($RHS $EQ))))
                                 (TRD-MSYMEVAL $Y '$Y)
                                 1)))
     (COND ((NOT ($FREEOF (TRD-MSYMEVAL $Y '$Y) $A1))
            (RETURN NIL)))
     (SETQ
      $N
      (SIMPLIFY
       ($HIPOW
        (SIMPLIFY ($RATSIMP (ADD* $EQ
                                  (*MMINUS (MUL* $A1
                                                 (TRD-MSYMEVAL $Y '$Y))))))
        (TRD-MSYMEVAL $Y '$Y))))
     (SETQ $A2 (SIMPLIFY ($COEFF $EQ (TRD-MSYMEVAL $Y '$Y) $N)))
     (COND
      ((OR
        (NOT ($FREEOF (TRD-MSYMEVAL $Y '$Y) $A2))
        (NOT ($FREEOF (TRD-MSYMEVAL $X '$X) (TRD-MSYMEVAL $Y '$Y) $N))
        (LIKE $N 0)
        (NOT
         (LIKE
          $EQ
          (SIMPLIFY ($EXPAND (ADD* (MUL* $A1 (TRD-MSYMEVAL $Y '$Y))
                                   (MUL* $A2
                                         (POWER (TRD-MSYMEVAL $Y '$Y)
                                                $N))))))))
       (RETURN NIL)))
     (SETQ $A1 (SIMPLIFY ($INTEGRATE $A1 (TRD-MSYMEVAL $X '$X))))
     (SETQ $METHOD '$BERNOULLI)
     (SETQ $ODEINDEX $N)
     (RETURN
      (SIMPLIFY
       (LIST
        '(MEQUAL)
        (TRD-MSYMEVAL $Y '$Y)
        (MUL*
         (SIMPLIFY ($EXP $A1))
         (POWER
          (ADD*
           (MUL*
            (ADD* 1 (*MMINUS $N))
            (SIMPLIFY
             ($INTEGRATE (MUL* $A2
                               (SIMPLIFY ($EXP (MUL* (ADD* $N -1)
                                                     $A1))))
                         (TRD-MSYMEVAL $X '$X))))
           $%C)
          (DIV 1 (ADD* 1 (*MMINUS $N))))))))))
   '$A1
   '$A2
   '$N
   '$%C)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $GENHOM T TRANSLATED)
 (ADD2LNC '$GENHOM $PROPS)
 (DEFMTRFUN
  ($GENHOM $ANY MDEFINE NIL NIL)
  ($EQ)
  NIL
  ((LAMBDA
    ($%G% $U $N $A1 $A2 $A3 $%C)
    NIL
    (PROG
     NIL
     (SETQ $%G% (DIV (MUL* ($RHS $EQ) (TRD-MSYMEVAL $X '$X))
                     (TRD-MSYMEVAL $Y '$Y)))
     (SETQ
      $N
      (SIMPLIFY
       ($RATSIMP (DIV (MUL* (TRD-MSYMEVAL $X '$X)
                            (SIMPLIFY ($DIFF (TRD-MSYMEVAL $%G% '$%G%)
                                             (TRD-MSYMEVAL $X '$X))))
                      (MUL* (TRD-MSYMEVAL $Y '$Y)
                            (SIMPLIFY ($DIFF (TRD-MSYMEVAL $%G% '$%G%)
                                             (TRD-MSYMEVAL $Y '$Y))))))))
     (COND ((NOT ($FREEOF (TRD-MSYMEVAL $X '$X) (TRD-MSYMEVAL $Y '$Y) $N))
            (RETURN NIL)))
     (SETQ
      $A1
      (SIMPLIFY
       ($RATSIMP (SIMPLIFY ($SUBSTITUTE (DIV $U
                                             (POWER (TRD-MSYMEVAL $X
                                                                  '$X)
                                                    $N))
                                        (TRD-MSYMEVAL $Y '$Y)
                                        (TRD-MSYMEVAL $%G% '$%G%))))))
     (SETQ $A2 (SIMPLIFY ($INTEGRATE (DIV 1 (MUL* $U (ADD* $N $A1))) $U)))
     (COND ((NOT ($FREEOF (SIMPLIFY ($NOUNIFY '$INTEGRATE)) $A2))
            (RETURN NIL)))
     (SETQ
      $A3
      (SIMPLIFY
       ($RATSIMP (SIMPLIFY ($SUBSTITUTE (MUL* (TRD-MSYMEVAL $Y '$Y)
                                              (POWER (TRD-MSYMEVAL $X
                                                                   '$X)
                                                     $N))
                                        $U
                                        $A2)))))
     (SETQ $METHOD '$GENHOM)
     (SETQ $ODEINDEX $N)
     (RETURN (SIMPLIFY (LIST '(MEQUAL)
                             (TRD-MSYMEVAL $X '$X)
                             (MUL* $%C (SIMPLIFY ($EXP $A3))))))))
   '$%G%
   '$U
   '$N
   '$A1
   '$A2
   '$A3
   '$%C)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $HOM2 T TRANSLATED)
 (ADD2LNC '$HOM2 $PROPS)
 (DEFMTRFUN ($HOM2 $ANY MDEFINE NIL NIL)
            ($A1 $A2 $A3)
            NIL
            ((LAMBDA ($AP $AQ $PT)
               NIL
               (PROG NIL
                     (SETQ $AP (DIV $A2 $A1))
                     (SETQ $AQ (DIV $A3 $A1))
                     (COND (($FTEST (SIMPLIFY ($CC2 $AP
                                                    $AQ
                                                    (TRD-MSYMEVAL $Y '$Y)
                                                    (TRD-MSYMEVAL $X '$X))))
                            (RETURN (TRD-MSYMEVAL $%Q% '$%Q%))))
                     (COND (($FTEST (SIMPLIFY ($EXACT2 $A1 $A2 $A3)))
                            (RETURN (TRD-MSYMEVAL $%Q% '$%Q%))))
                     (COND ((LIKE (SETQ $PT (SIMPLIFY ($PTTEST $AP)))
                                  NIL)
                            (GO $END)))
                     (COND (($FTEST (SIMPLIFY ($EULER2 $AP $AQ)))
                            (RETURN (TRD-MSYMEVAL $%Q% '$%Q%))))
                     (COND (($FTEST (SIMPLIFY ($BESSEL2 $AP $AQ)))
                            (RETURN (TRD-MSYMEVAL $%Q% '$%Q%))))
                $END (RETURN (COND (($FTEST (SIMPLIFY ($XCC2 $AP $AQ)))
                                    (RETURN (TRD-MSYMEVAL $%Q% '$%Q%)))
                                   (T (RETURN NIL))))))
             '$AP
             '$AQ
             '$PT)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $CC2 T TRANSLATED)
 (ADD2LNC '$CC2 $PROPS)
 (DEFMTRFUN
  ($CC2 $ANY MDEFINE NIL NIL)
  ($%F% $%G% $Y $X)
  NIL
  ((LAMBDA
    ($A $SIGN $RADEXPAND $ALPHA $ZERO $POS $YNEW $%K1 $%K2)
    NIL
    (PROG
     NIL
     (COND ((NOT (AND ($FREEOF (TRD-MSYMEVAL $X '$X)
                               (TRD-MSYMEVAL $Y '$Y)
                               (TRD-MSYMEVAL $%F% '$%F%))
                      ($FREEOF (TRD-MSYMEVAL $X '$X)
                               (TRD-MSYMEVAL $Y '$Y)
                               (TRD-MSYMEVAL $%G% '$%G%))))
            (RETURN NIL)))
     (SETQ $METHOD '$CONSTCOEFF)
     (SETQ $A (ADD* (POWER (TRD-MSYMEVAL $%F% '$%F%) 2)
                    (*MMINUS (MUL* 4 (TRD-MSYMEVAL $%G% '$%G%)))))
     (COND (($FREEOF '$%I $A)
            (SETQ $SIGN (SIMPLIFY ($ASKSIGN $A))))
           (T
            (SETQ $RADEXPAND T)
            (SETQ $SIGN '$PNZ)))
     (COND
      ((LIKE $SIGN $ZERO)
       (RETURN
        (SIMPLIFY
         (LIST
          '(MEQUAL)
          (TRD-MSYMEVAL $Y '$Y)
          (MUL*
           (SIMPLIFY ($EXP (DIV (MUL* (*MMINUS (TRD-MSYMEVAL $%F%
                                                             '$%F%))
                                      (TRD-MSYMEVAL $X '$X))
                                2)))
           (ADD* $%K1 (MUL* $%K2 (TRD-MSYMEVAL $X '$X)))))))))
     (COND
      ((LIKE $SIGN $POS)
       (RETURN
        (SIMPLIFY
         (LIST
          '(MEQUAL)
          (TRD-MSYMEVAL $Y '$Y)
          (ADD*
           (MUL*
            $%K1
            (SIMPLIFY
             ($EXP (DIV (MUL* (ADD* (*MMINUS (TRD-MSYMEVAL $%F%
                                                           '$%F%))
                                    (SIMPLIFY (LIST '(%SQRT) $A)))
                              (TRD-MSYMEVAL $X '$X))
                        2))))
           (MUL*
            $%K2
            (SIMPLIFY
             ($EXP
              (DIV (MUL* (ADD* (*MMINUS (TRD-MSYMEVAL $%F% '$%F%))
                               (*MMINUS (SIMPLIFY (LIST '(%SQRT)
                                                        $A))))
                         (TRD-MSYMEVAL $X '$X))
                   2))))))))))
     (SETQ $A (*MMINUS $A))
     (SETQ $ALPHA (DIV (MUL* (TRD-MSYMEVAL $X '$X)
                             (SIMPLIFY (LIST '(%SQRT) $A)))
                       2))
     (COND
      ((LIKE (TRD-MSYMEVAL $EXPONENTIALIZE NIL) NIL)
       (RETURN
        (SIMPLIFY
         (LIST
          '(MEQUAL)
          (TRD-MSYMEVAL $Y '$Y)
          (MUL*
           (SIMPLIFY ($EXP (DIV (MUL* (*MMINUS (TRD-MSYMEVAL $%F%
                                                             '$%F%))
                                      (TRD-MSYMEVAL $X '$X))
                                2)))
           (ADD* (MUL* $%K1 (SIMPLIFY (LIST '(%SIN) $ALPHA)))
                 (MUL* $%K2 (SIMPLIFY (LIST '(%COS) $ALPHA))))))))))
     (RETURN
      (SIMPLIFY
       (LIST
        '(MEQUAL)
        (TRD-MSYMEVAL $Y '$Y)
        (MUL* (SIMPLIFY ($EXP (DIV (MUL* (*MMINUS (TRD-MSYMEVAL $%F%
                                                                '$%F%))
                                         (TRD-MSYMEVAL $X '$X))
                                   2)))
              (ADD* (MUL* $%K1 (SIMPLIFY ($EXP (MUL* '$%I $ALPHA))))
                    (MUL* $%K2
                          (SIMPLIFY ($EXP (MUL* (*MMINUS '$%I)
                                                $ALPHA)))))))))))
   '$A
   '$SIGN
   '$ALL
   '$ALPHA
   '$ZERO
   '$POS
   '$YNEW
   '$%K1
   '$%K2)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $EXACT2 T TRANSLATED)
 (ADD2LNC '$EXACT2 $PROPS)
 (DEFMTRFUN
  ($EXACT2 $ANY MDEFINE NIL NIL)
  ($A1 $A2 $A3)
  NIL
  ((LAMBDA
    ($B1 $%K1 $%K2)
    NIL
    (PROG
     NIL
     (COND
      ((LIKE
        (SIMPLIFY
         ($RATSIMP (ADD* (SIMPLIFY ($DIFF $A1 (TRD-MSYMEVAL $X '$X) 2))
                         (*MMINUS (SIMPLIFY ($DIFF $A2
                                                   (TRD-MSYMEVAL $X '$X))))
                         $A3)))
        0)
       (SETQ
        $B1
        (SIMPLIFY
         ($EXP
          (*MMINUS
           (SIMPLIFY
            ($INTEGRATE
             (SIMPLIFY
              ($RATSIMP
               (DIV
                (ADD*
                 $A2
                 (*MMINUS (SIMPLIFY ($DIFF $A1
                                           (TRD-MSYMEVAL $X '$X)))))
                $A1)))
             (TRD-MSYMEVAL $X '$X))))))))
      (T (RETURN NIL)))
     (SETQ $METHOD '$EXACT)
     (RETURN
      (SIMPLIFY
       (LIST '(MEQUAL)
             (TRD-MSYMEVAL $Y '$Y)
             (ADD* (MUL* $%K1
                         $B1
                         (SIMPLIFY ($INTEGRATE (DIV 1 (MUL* $A1 $B1))
                                               (TRD-MSYMEVAL $X '$X))))
                   (MUL* $%K2 $B1)))))))
   '$B1
   '$%K1
   '$%K2)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $XCC2 T TRANSLATED)
 (ADD2LNC '$XCC2 $PROPS)
 (DEFMTRFUN
  ($XCC2 $ANY MDEFINE NIL NIL)
  ($AP $AQ)
  NIL
  ((LAMBDA
    ($D $B1 $Z $RADEXPAND)
    NIL
    (PROG
     NIL
     (COND ((LIKE $AQ 0)
            (RETURN NIL)))
     (SETQ
      $D
      (SIMPLIFY ($RATSIMP (DIV (ADD* (SIMPLIFY ($DIFF $AQ
                                                      (TRD-MSYMEVAL $X
                                                                    '$X)))
                                     (MUL* 2 $AP $AQ))
                               (MUL* 2 (POWER $AQ (RREMAINDER 3 2)))))))
     (COND (($FREEOF (TRD-MSYMEVAL $X '$X) (TRD-MSYMEVAL $Y '$Y) $D)
            (SETQ $B1 (SIMPLIFY ($CC2 $D 1 (TRD-MSYMEVAL $Y '$Y) $Z))))
           (T (RETURN NIL)))
     (SETQ $METHOD '$XFORMTOCONSTCOEFF)
     (RETURN
      (SIMPLIFY
       ($SUBSTITUTE (SIMPLIFY ($INTEGRATE (SIMPLIFY (LIST '(%SQRT) $AQ))
                                          (TRD-MSYMEVAL $X '$X)))
                    $Z
                    $B1)))))
   '$D
   '$B1
   '$Z
   '$ALL)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $VARP T TRANSLATED)
 (ADD2LNC '$VARP $PROPS)
 (DEFMTRFUN
  ($VARP $ANY MDEFINE NIL NIL)
  ($SOLN $%G%)
  NIL
  ((LAMBDA
    ($Y1 $Y2 $Y3 $Y4 $WR $HEURISTIC $%K1 $%K2)
    NIL
    (PROG
     NIL
     (SETQ
      $Y1
      (SIMPLIFY
       ($RATSIMP (SIMPLIFY ($SUBSTITUTE (LIST '(MLIST)
                                              (SIMPLIFY (LIST '(MEQUAL)
                                                              $%K1
                                                              1))
                                              (SIMPLIFY (LIST '(MEQUAL)
                                                              $%K2
                                                              0)))
                                        ($RHS $SOLN))))))
     (SETQ
      $Y2
      (SIMPLIFY
       ($RATSIMP (SIMPLIFY ($SUBSTITUTE (LIST '(MLIST)
                                              (SIMPLIFY (LIST '(MEQUAL)
                                                              $%K1
                                                              0))
                                              (SIMPLIFY (LIST '(MEQUAL)
                                                              $%K2
                                                              1)))
                                        ($RHS $SOLN))))))
     (SETQ $WR (ADD* (MUL* $Y1 (SIMPLIFY ($DIFF $Y2 (TRD-MSYMEVAL $X '$X))))
                     (*MMINUS (MUL* $Y2
                                    (SIMPLIFY ($DIFF $Y1
                                                     (TRD-MSYMEVAL $X
                                                                   '$X)))))))
     (COND ((LIKE $WR 0)
            (RETURN NIL)))
     (COND ((AND (LIKE (TRD-MSYMEVAL $METHOD '$METHOD) '$CONSTCOEFF)
                 (NOT ($FREEOF '%SIN $WR))
                 (NOT ($FREEOF '%COS $WR)))
            (SETQ $HEURISTIC T)
            (SETQ $WR (SIMPLIFY ($RATSIMP (SIMPLIFY ($TRIGREDUCE $WR)))))))
     (SETQ $Y3 (SIMPLIFY ($RATSIMP (DIV (MUL* $Y1
                                              (TRD-MSYMEVAL $%G% '$%G%))
                                        $WR))))
     (SETQ $Y4 (SIMPLIFY ($RATSIMP (DIV (MUL* $Y2
                                              (TRD-MSYMEVAL $%G% '$%G%))
                                        $WR))))
     (SETQ
      $YP
      (SIMPLIFY
       ($RATSIMP
        (ADD* (MUL* $Y2 (SIMPLIFY ($INTEGRATE $Y3 (TRD-MSYMEVAL $X '$X))))
              (*MMINUS (MUL* $Y1
                             (SIMPLIFY ($INTEGRATE $Y4
                                                   (TRD-MSYMEVAL $X
                                                                 '$X)))))))))
     (COND
      ((LIKE $HEURISTIC T)
       (SETQ
        $YP
        (SIMPLIFY ($RATSIMP (SIMPLIFY ($TRIGREDUCE (TRD-MSYMEVAL $YP
                                                                 '$YP))))))))
     (SETQ $METHOD '$VARIATIONOFPARAMETERS)
     (RETURN (SIMPLIFY (LIST '(MEQUAL)
                             (TRD-MSYMEVAL $Y '$Y)
                             (ADD* ($RHS $SOLN) (TRD-MSYMEVAL $YP '$YP)))))))
   '$Y1
   '$Y2
   '$Y3
   '$Y4
   '$WR
   NIL
   '$%K1
   '$%K2)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $REDUCE T TRANSLATED)
 (ADD2LNC '$REDUCE $PROPS)
 (DEFMTRFUN
  ($REDUCE $ANY MDEFINE NIL NIL)
  ($EQ)
  NIL
  ((LAMBDA
    ($B1 $QQ)
    NIL
    (PROG
     NIL
     (SETQ
      $B1
      (SIMPLIFY
       ($SUBSTITUTE
        (LIST '(MLIST)
              (SIMPLIFY (LIST '(MEQUAL)
                              (SIMPLIFY `((%DERIVATIVE) ,
                                          (TRD-MSYMEVAL $Y '$Y) ,
                                          (TRD-MSYMEVAL $X '$X) 2))
                              $QQ))
              (SIMPLIFY (LIST '(MEQUAL)
                              (SIMPLIFY `((%DERIVATIVE) ,
                                          (TRD-MSYMEVAL $Y '$Y) ,
                                          (TRD-MSYMEVAL $X '$X)))
                              $QQ)))
        $EQ)))
     (COND (($FREEOF (TRD-MSYMEVAL $Y '$Y) $B1)
            (RETURN (SIMPLIFY ($NLX $EQ)))))
     (RETURN (COND (($FREEOF (TRD-MSYMEVAL $X '$X) $B1)
                    (RETURN (SIMPLIFY ($NLY $EQ))))
                   (T (RETURN NIL))))))
   '$B1
   '$QQ)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $NLX T TRANSLATED)
 (ADD2LNC '$NLX $PROPS)
 (DEFMTRFUN
  ($NLX $ANY MDEFINE NIL NIL)
  ($EQ)
  NIL
  ((LAMBDA
    ($DE $B $A1 $V $%K1 $%C)
    NIL
    (PROG
     NIL
     (SETQ
      $DE
      (SIMPLIFY
       ($SUBSTITUTE
        (LIST '(MLIST)
              (SIMPLIFY (LIST '(MEQUAL)
                              (SIMPLIFY `((%DERIVATIVE) ,
                                          (TRD-MSYMEVAL $Y '$Y) ,
                                          (TRD-MSYMEVAL $X '$X) 2))
                              (SIMPLIFY `((%DERIVATIVE) ,$V ,
                                          (TRD-MSYMEVAL $X '$X)))))
              (SIMPLIFY (LIST '(MEQUAL)
                              (SIMPLIFY `((%DERIVATIVE) ,
                                          (TRD-MSYMEVAL $Y '$Y) ,
                                          (TRD-MSYMEVAL $X '$X)))
                              $V)))
        $EQ)))
     (COND ((LIKE (SETQ $B (SIMPLIFY ($ODE1A $DE $V (TRD-MSYMEVAL $X '$X))))
                  NIL)
            (RETURN NIL)))
     (SETQ
      $A1
      (SIMPLIFY
       ($SUBSTITUTE
        (LIST '(MLIST)
              (SIMPLIFY (LIST '(MEQUAL)
                              $V
                              (SIMPLIFY `((%DERIVATIVE) ,
                                          (TRD-MSYMEVAL $Y '$Y) ,
                                          (TRD-MSYMEVAL $X '$X)))))
              (SIMPLIFY (LIST '(MEQUAL) $%C $%K1)))
        $B)))
     (RETURN
      (COND (($FTEST (SIMPLIFY ($NLXY $A1
                                      (SIMPLIFY `((%DERIVATIVE) ,
                                                  (TRD-MSYMEVAL $Y '$Y)
                                                  ,(TRD-MSYMEVAL $X
                                                                 '$X))))))
             (SETQ $METHOD '$FREEOFY)
             (RETURN (TRD-MSYMEVAL $%Q% '$%Q%)))
            (T (RETURN NIL))))))
   '$DE
   '$B
   '$A1
   '$V
   '$%K1
   '$%C)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $NLY T TRANSLATED)
 (ADD2LNC '$NLY $PROPS)
 (DEFMTRFUN
  ($NLY $ANY MDEFINE NIL NIL)
  ($EQ)
  NIL
  ((LAMBDA
    ($DE $B $A1 $YZ $V $%C $%K1)
    NIL
    (PROG
     NIL
     (SETQ
      $DE
      (SIMPLIFY
       ($SUBSTITUTE
        (LIST '(MLIST)
              (SIMPLIFY (LIST '(MEQUAL)
                              (SIMPLIFY `((%DERIVATIVE) ,
                                          (TRD-MSYMEVAL $Y '$Y) ,
                                          (TRD-MSYMEVAL $X '$X) 2))
                              (MUL* $V
                                    (SIMPLIFY `((%DERIVATIVE) ,$V ,$YZ)))))
              (SIMPLIFY (LIST '(MEQUAL)
                              (SIMPLIFY `((%DERIVATIVE) ,
                                          (TRD-MSYMEVAL $Y '$Y) ,
                                          (TRD-MSYMEVAL $X '$X)))
                              $V))
              (SIMPLIFY (LIST '(MEQUAL) (TRD-MSYMEVAL $Y '$Y) $YZ)))
        $EQ)))
     (COND ((LIKE (SETQ $B (SIMPLIFY ($ODE1A $DE $V $YZ)))
                  NIL)
            (RETURN NIL)))
     (SETQ
      $A1
      (SIMPLIFY
       ($SUBSTITUTE
        (LIST '(MLIST)
              (SIMPLIFY (LIST '(MEQUAL)
                              $V
                              (SIMPLIFY `((%DERIVATIVE) ,
                                          (TRD-MSYMEVAL $Y '$Y) ,
                                          (TRD-MSYMEVAL $X '$X)))))
              (SIMPLIFY (LIST '(MEQUAL) $YZ (TRD-MSYMEVAL $Y '$Y)))
              (SIMPLIFY (LIST '(MEQUAL) $%C $%K1)))
        $B)))
     (RETURN
      (COND (($FTEST (SIMPLIFY ($NLXY $A1
                                      (SIMPLIFY `((%DERIVATIVE) ,
                                                  (TRD-MSYMEVAL $Y '$Y)
                                                  ,(TRD-MSYMEVAL $X
                                                                 '$X))))))
             (SETQ $METHOD '$FREEOFX)
             (RETURN (TRD-MSYMEVAL $%Q% '$%Q%)))
            (T (RETURN NIL))))))
   '$DE
   '$B
   '$A1
   '$YZ
   '$V
   '$%C
   '$%K1)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $NLXY T TRANSLATED)
 (ADD2LNC '$NLXY $PROPS)
 (DEFMTRFUN
  ($NLXY $ANY MDEFINE NIL NIL)
  ($EQ $DE)
  NIL
  ((LAMBDA
    ($PROGRAMMODE $EQ1 $%K2 $%C)
    NIL
    (PROG
     NIL
     (SETQ $EQ1 (SIMPLIFY ($SOLVE $EQ $DE)))
     (SETQ
      $EQ1
      (MAPLIST_TR
       (M-TLAMBDA&ENV
        (($ZZ) ($%K2 $%C))
        NIL
        (COND (($FTEST (SIMPLIFY ($ODE1A $ZZ
                                         (TRD-MSYMEVAL $Y '$Y)
                                         (TRD-MSYMEVAL $X '$X))))
               (SIMPLIFY ($SUBSTITUTE $%K2
                                      $%C
                                      (TRD-MSYMEVAL $%Q% '$%Q%))))))
       $EQ1))
     (RETURN (COND ((EQL ($LENGTH $EQ1) 1)
                    (RETURN (SIMPLIFY ($FIRST $EQ1))))
                   (T (RETURN $EQ1))))))
   T
   '$EQ1
   '$%K2
   '$%C)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $PTTEST T TRANSLATED)
 (ADD2LNC '$PTTEST $PROPS)
 (DEFMTRFUN
  ($PTTEST $ANY MDEFINE NIL NIL)
  ($A)
  NIL
  ((LAMBDA ($A1 $A2 $A3)
     NIL
     (PROG NIL
           (COND ((LIKE (SETQ $A1 (SIMPLIFY ($RATSIMP $A)))
                        0)
                  (RETURN NIL)))
           (SETQ $A1 (SIMPLIFY ($EXPAND (DIV 1 $A1))))
           (COND ((LIKE (SETQ $A2 (SIMPLIFY ($COEFF $A1
                                                    (TRD-MSYMEVAL $X '$X)
                                                    1)))
                        0)
                  (RETURN NIL)))
           (COND ((NOT ($FREEOF (TRD-MSYMEVAL $X '$X) $A2))
                  (RETURN NIL)))
           (SETQ $A3 (SIMPLIFY ($COEFF $A1 (TRD-MSYMEVAL $X '$X) 0)))
           (RETURN (COND ((NOT (LIKE $A1
                                     (ADD* (MUL* $A2
                                                 (TRD-MSYMEVAL $X '$X))
                                           $A3)))
                          (RETURN NIL))
                         (T (RETURN (DIV (*MMINUS $A3) $A2)))))))
   '$A1
   '$A2
   '$A3)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $EULER2 T TRANSLATED)
 (ADD2LNC '$EULER2 $PROPS)
 (DEFMTRFUN
  ($EULER2 $ANY MDEFINE NIL NIL)
  ($A $B)
  NIL
  ((LAMBDA
    ($DC $RP $IP $ALPHA $BETA $SIGN $RADEXPAND $%K1 $%K2 $POS $ZERO)
    NIL
    (PROG
     NIL
     (COND
      ((NOT
        ($FREEOF
         (TRD-MSYMEVAL $X '$X)
         (TRD-MSYMEVAL $Y '$Y)
         (SETQ
          $BETA
          (SIMPLIFY
           ($RATSIMP (MUL* $B
                           (POWER (ADD* (TRD-MSYMEVAL $X '$X)
                                        (*MMINUS (TRD-MSYMEVAL $PT
                                                               '$PT)))
                                  2)))))))
       (RETURN NIL)))
     (SETQ $METHOD '$EULER)
     (SETQ $ALPHA (MUL* $A
                        (ADD* (TRD-MSYMEVAL $X '$X)
                              (*MMINUS (TRD-MSYMEVAL $PT '$PT)))))
     (SETQ $DC (SIMPLIFY ($RATSIMP (ADD* (POWER (ADD* $ALPHA -1) 2)
                                         (*MMINUS (MUL* 4 $BETA))))))
     (SETQ $RP (SIMPLIFY ($RATSIMP (DIV (*MMINUS (ADD* $ALPHA -1)) 2))))
     (SETQ $SIGN (SIMPLIFY ($ASKSIGN $DC)))
     (COND
      ((LIKE $SIGN $ZERO)
       (RETURN
        (SIMPLIFY
         (LIST
          '(MEQUAL)
          (TRD-MSYMEVAL $Y '$Y)
          (MUL*
           (POWER (ADD* (TRD-MSYMEVAL $X '$X)
                        (*MMINUS (TRD-MSYMEVAL $PT '$PT)))
                  $RP)
           (ADD*
            $%K1
            (MUL*
             $%K2
             (SIMPLIFY (LIST '(%LOG)
                             (ADD* (TRD-MSYMEVAL $X '$X)
                                   (*MMINUS (TRD-MSYMEVAL $PT
                                                          '$PT)))))))))))))
     (COND
      ((LIKE $SIGN $POS)
       (SETQ $IP (DIV (SIMPLIFY (LIST '(%SQRT) $DC)) 2))
       (RETURN
        (SIMPLIFY
         (LIST '(MEQUAL)
               (TRD-MSYMEVAL $Y '$Y)
               (ADD* (MUL* $%K1
                           (POWER (ADD* (TRD-MSYMEVAL $X '$X)
                                        (*MMINUS (TRD-MSYMEVAL $PT
                                                               '$PT)))
                                  (ADD* $RP $IP)))
                     (MUL* $%K2
                           (POWER (ADD* (TRD-MSYMEVAL $X '$X)
                                        (*MMINUS (TRD-MSYMEVAL $PT
                                                               '$PT)))
                                  (ADD* $RP (*MMINUS $IP))))))))))
     (SETQ $DC (*MMINUS $DC))
     (SETQ $IP (DIV (SIMPLIFY (LIST '(%SQRT) $DC)) 2))
     (RETURN
      (SIMPLIFY
       (LIST
        '(MEQUAL)
        (TRD-MSYMEVAL $Y '$Y)
        (MUL*
         (POWER (ADD* (TRD-MSYMEVAL $X '$X)
                      (*MMINUS (TRD-MSYMEVAL $PT '$PT)))
                $RP)
         (ADD*
          (MUL*
           $%K1
           (SIMPLIFY
            (LIST
             '(%SIN)
             (MUL*
              $IP
              (SIMPLIFY (LIST '(%LOG)
                              (ADD* (TRD-MSYMEVAL $X '$X)
                                    (*MMINUS (TRD-MSYMEVAL $PT
                                                           '$PT)))))))))
          (MUL*
           $%K2
           (SIMPLIFY
            (LIST
             '(%COS)
             (MUL*
              $IP
              (SIMPLIFY
               (LIST '(%LOG)
                     (ADD* (TRD-MSYMEVAL $X '$X)
                           (*MMINUS (TRD-MSYMEVAL $PT '$PT))))))))))))))))
   '$DC
   '$RP
   '$IP
   '$ALPHA
   '$BETA
   '$SIGN
   NIL
   '$%K1
   '$%K2
   '$POS
   '$ZERO)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $BESSEL2 T TRANSLATED)
 (ADD2LNC '$BESSEL2 $PROPS)
 (DEFMTRFUN
  ($BESSEL2 $ANY MDEFINE NIL NIL)
  ($A $B)
  NIL
  ((LAMBDA
    ($NU $B1 $INTP $RADEXPAND $%K1 $%Y $%K2 $%J)
    NIL
    (PROG
     NIL
     (COND
      ((NOT
        ($FREEOF
         (TRD-MSYMEVAL $X '$X)
         (TRD-MSYMEVAL $Y '$Y)
         (SETQ
          $B1
          (SIMPLIFY
           ($RATSIMP (MUL* (ADD* 1 (*MMINUS $B))
                           (POWER (ADD* (TRD-MSYMEVAL $X '$X)
                                        (*MMINUS (TRD-MSYMEVAL $PT
                                                               '$PT)))
                                  2)))))))
       (RETURN NIL)))
     (COND
      ((NOT
        (LIKE
         (SIMPLIFY ($RATSIMP (MUL* $A
                                   (ADD* (TRD-MSYMEVAL $X '$X)
                                         (*MMINUS (TRD-MSYMEVAL $PT
                                                                '$PT))))))
         1))
       (RETURN NIL)))
     (SETQ $NU (SIMPLIFY (LIST '(%SQRT) $B1)))
     (SETQ $METHOD '$BESSEL)
     (COND
      ((LIKE $NU (RREMAINDER 1 2))
       (RETURN
        (SIMPLIFY
         (LIST
          '(MEQUAL)
          (TRD-MSYMEVAL $Y '$Y)
          (DIV
           (ADD*
            (MUL*
             $%K1
             (SIMPLIFY (LIST '(%SIN)
                             (ADD* (TRD-MSYMEVAL $X '$X)
                                   (*MMINUS (TRD-MSYMEVAL $PT '$PT))))))
            (MUL*
             $%K2
             (SIMPLIFY (LIST '(%COS)
                             (ADD* (TRD-MSYMEVAL $X '$X)
                                   (*MMINUS (TRD-MSYMEVAL $PT
                                                          '$PT)))))))
           (SIMPLIFY (LIST '(%SQRT)
                           (ADD* (TRD-MSYMEVAL $X '$X)
                                 (*MMINUS (TRD-MSYMEVAL $PT '$PT)))))))))))
     (COND ((IS-BOOLE-CHECK (SIMPLIFY ($FEATUREP $NU '$INTEGER)))
            (SETQ $INTP '$Y))
           (($NUMBERP $NU)
            (SETQ $INTP '$N)))
     $LOOP
     (COND
      ((NOT (OR (LIKE $INTP '$Y)
                (LIKE $INTP '$N)))
       (SETQ $INTP (SIMPLIFY ($READONLY '|&Is|
                                        $NU
                                        '|&an integer?  Type Y or N.|)))
       (GO $LOOP)))
     (COND
      ((LIKE $INTP '$Y)
       (RETURN
        (SIMPLIFY
         (LIST
          '(MEQUAL)
          (TRD-MSYMEVAL $Y '$Y)
          (ADD*
           (MUL*
            $%K1
            (SIMPLIFY
             (MAPPLY (MAREF $%J $NU)
                     (LIST (ADD* (TRD-MSYMEVAL $X '$X)
                                 (*MMINUS (TRD-MSYMEVAL $PT '$PT))))
                     '(($%J ARRAY) $NU))))
           (MUL*
            $%K2
            (SIMPLIFY
             (MAPPLY (MAREF $%Y $NU)
                     (LIST (ADD* (TRD-MSYMEVAL $X '$X)
                                 (*MMINUS (TRD-MSYMEVAL $PT '$PT))))
                     '(($%Y ARRAY) $NU))))))))))
     (RETURN
      (SIMPLIFY
       (LIST
        '(MEQUAL)
        (TRD-MSYMEVAL $Y '$Y)
        (ADD*
         (MUL*
          $%K1
          (SIMPLIFY (MAPPLY (MAREF $%J $NU)
                            (LIST (ADD* (TRD-MSYMEVAL $X '$X)
                                        (*MMINUS (TRD-MSYMEVAL $PT
                                                               '$PT))))
                            '(($%J ARRAY) $NU))))
         (MUL*
          $%K2
          (SIMPLIFY
           (MAPPLY (MAREF $%J (*MMINUS $NU))
                   (LIST (ADD* (TRD-MSYMEVAL $X '$X)
                               (*MMINUS (TRD-MSYMEVAL $PT '$PT))))
                   '(($%J ARRAY) ((MMINUS) $NU)))))))))))
   '$NU
   '$B1
   '$INTP
   '$ALL
   '$%K1
   '$%Y
   '$%K2
   '$%J)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $IC1 T TRANSLATED)
 (ADD2LNC '$IC1 $PROPS)
 (DEFMTRFUN
  ($IC1 $ANY MDEFINE NIL NIL)
  ($SOLN $XC $YC)
  NIL
  ((LAMBDA
    ($%C)
    NIL
    (PROGN
     (SIMPLIFY ($NOTEQN $XC))
     (SIMPLIFY ($NOTEQN $YC))
     (SIMPLIFY ($BOUNDTEST '$%C $%C))
     (SIMPLIFY
      ($RATSIMP
       (SIMPLIFY
        ($SUBSTITUTE
         (LIST
          '(MLIST)
          (SIMPLIFY
           (LIST
            '(MEQUAL)
            '$%C
            ($RHS
             (SIMPLIFY
              ($SOLVE1 (SIMPLIFY ($SUBSTITUTE (LIST '(MLIST)
                                                    $XC
                                                    $YC)
                                              $SOLN))
                       $%C))))))
         $SOLN))))))
   '$%C)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $BC2 T TRANSLATED)
 (ADD2LNC '$BC2 $PROPS)
 (DEFMTRFUN
  ($BC2 $ANY MDEFINE NIL NIL)
  ($SOLN $XA $YA $XB $YB)
  NIL
  ((LAMBDA
    ($PROGRAMMODE $BACKSUBST $SINGSOLVE $TEMP $%K1 $%K2)
    NIL
    (PROG
     NIL
     (SIMPLIFY ($NOTEQN $XA))
     (SIMPLIFY ($NOTEQN $YA))
     (SIMPLIFY ($NOTEQN $XB))
     (SIMPLIFY ($NOTEQN $YB))
     (SIMPLIFY ($BOUNDTEST '$%K1 $%K1))
     (SIMPLIFY ($BOUNDTEST '$%K2 $%K2))
     (SETQ
      $TEMP
      (MAPLIST_TR
       (M-TLAMBDA&ENV (($ZZ) ($SOLN))
                      NIL
                      (SIMPLIFY ($SUBSTITUTE $ZZ $SOLN)))
       (SIMPLIFY ($SOLVE (LIST '(MLIST)
                               (SIMPLIFY ($SUBSTITUTE (LIST '(MLIST)
                                                            $XA
                                                            $YA)
                                                      $SOLN))
                               (SIMPLIFY ($SUBSTITUTE (LIST '(MLIST)
                                                            $XB
                                                            $YB)
                                                      $SOLN)))
                         (LIST '(MLIST) $%K1 $%K2)))))
     (RETURN (COND ((EQL ($LENGTH $TEMP) 1)
                    (RETURN (SIMPLIFY ($FIRST $TEMP))))
                   (T (RETURN $TEMP))))))
   T
   T
   T
   '$TEMP
   '$%K1
   '$%K2)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $IC2 T TRANSLATED)
 (ADD2LNC '$IC2 $PROPS)
 (DEFMTRFUN
  ($IC2 $ANY MDEFINE NIL NIL)
  ($SOLN $XA $YA $DYA)
  NIL
  ((LAMBDA
    ($PROGRAMMODE $BACKSUBST $SINGSOLVE $TEMP $%K2 $%K1)
    NIL
    (PROG
     NIL
     (SIMPLIFY ($NOTEQN $XA))
     (SIMPLIFY ($NOTEQN $YA))
     (SIMPLIFY ($NOTEQN $DYA))
     (SIMPLIFY ($BOUNDTEST '$%K1 $%K1))
     (SIMPLIFY ($BOUNDTEST '$%K2 $%K2))
     (SETQ $TEMP (ADD* ($LHS $SOLN) (*MMINUS ($RHS $SOLN))))
     (SETQ
      $TEMP
      (MAPLIST_TR
       (M-TLAMBDA&ENV (($ZZ) ($SOLN))
                      NIL
                      (SIMPLIFY ($SUBSTITUTE $ZZ $SOLN)))
       (SIMPLIFY
        ($SOLVE
         (LIST
          '(MLIST)
          (SIMPLIFY ($SUBSTITUTE (LIST '(MLIST) $XA $YA) $SOLN))
          (SIMPLIFY
           ($SUBSTITUTE
            (LIST '(MLIST) $DYA $XA)
            (SIMPLIFY
             (LIST
              '(MEQUAL)
              ($LHS $DYA)
              (DIV
               (*MMINUS
                (SIMPLIFY
                 ($SUBSTITUTE 0
                              ($LHS $DYA)
                              (SIMPLIFY ($DIFF $TEMP ($LHS $XA))))))
               (SIMPLIFY ($DIFF $TEMP ($LHS $YA)))))))))
         (LIST '(MLIST) $%K1 $%K2)))))
     (RETURN (COND ((EQL ($LENGTH $TEMP) 1)
                    (RETURN (SIMPLIFY ($FIRST $TEMP))))
                   (T (RETURN $TEMP))))))
   T
   T
   T
   '$TEMP
   '$%K2
   '$%K1)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $NOTEQN T TRANSLATED)
 (ADD2LNC '$NOTEQN $PROPS)
 (DEFMTRFUN ($NOTEQN $ANY MDEFINE NIL NIL)
            ($X)
            NIL
            (COND ((OR ($ATOM (TRD-MSYMEVAL $X '$X))
                       (NOT (LIKE (SIMPLIFY ($INPART (TRD-MSYMEVAL $X '$X)
                                                     0))
                                  '&=)))
                   (DISPLAY-FOR-TR NIL NIL (TRD-MSYMEVAL $X '$X))
                   (DISPLAY-FOR-TR NIL NIL '|&Not an equation|)
                   (SIMPLIFY ($ERROR))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (DEFPROP $BOUNDTEST T TRANSLATED)
       (ADD2LNC '$BOUNDTEST $PROPS)
       (DEFMTRFUN ($BOUNDTEST $ANY MDEFINE NIL NIL)
                  ($X $Y)
                  NIL
                  (COND ((NOT (LIKE (TRD-MSYMEVAL $X '$X)
                                    (TRD-MSYMEVAL $Y '$Y)))
                         (DISPLAY-FOR-TR NIL NIL (TRD-MSYMEVAL $X '$X))
                         (DISPLAY-FOR-TR NIL NIL '|&Must not be bound|)
                         (SIMPLIFY ($ERROR))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $FAILURE T TRANSLATED)
 (ADD2LNC '$FAILURE $PROPS)
 (DEFMTRFUN
  ($FAILURE $BOOLEAN MDEFINE NIL NIL)
  ($MSG $EQ)
  NIL
  ((LAMBDA
    ($YNEW)
    NIL
    (PROGN
     (COND ((NOT (IS-BOOLE-CHECK ($STATUS $FEATURE &ODE)))
            (DISPLAY-FOR-TR T
                            NIL
                            (SIMPLIFY ($SUBSTITUTE (TRD-MSYMEVAL $YOLD
                                                                 '$YOLD)
                                                   $YNEW
                                                   $EQ)))
            (DISPLAY-FOR-TR NIL NIL $MSG)))
     NIL))
   '$YNEW)))
(EVAL-WHEN (load COMPILE) (MEVAL '(($REMOVE) $X $SPECIAL $Y $SPECIAL)))
(SETQ $MSG1 '|&Not a proper differential equation|)
(SETQ $MSG2 '|&First order equation not linear in y'|)