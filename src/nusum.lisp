;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 4/21/85 11:00:16
(in-package "MAXIMA")

(EVAL-WHEN (COMPILE EVAL LOAD)
       (EVAL-WHEN (COMPILE EVAL LOAD)
              (DEFPROP $DVA T TRANSLATED)
              (ADD2LNC '$DVA $PROPS)
              (DEFMTRFUN ($DVA $ANY MDEFMACRO NIL NIL)
                         ($VAR)
                         NIL
                         (MBUILDQ-SUBST (LIST (CONS '$VAR $VAR))
                                        '(($DEFINE_VARIABLE) $VAR 
                                          ((MQUOTE) $VAR) $ANY)))))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (MEVAL* '(($MODEDECLARE) $%N $ANY))
       (MEVAL* '(($DECLARE) $%N $SPECIAL))
       NIL
       (DEF-MTRVAR $%N '$%N))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (MEVAL* '(($MODEDECLARE) $%PW $ANY))
       (MEVAL* '(($DECLARE) $%PW $SPECIAL))
       NIL
       (DEF-MTRVAR $%PW '$%PW))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (MEVAL* '(($MODEDECLARE) $%F $ANY))
       (MEVAL* '(($DECLARE) $%F $SPECIAL))
       NIL
       (DEF-MTRVAR $%F '$%F))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (MEVAL* '(($MODEDECLARE) $%F1 $ANY))
       (MEVAL* '(($DECLARE) $%F1 $SPECIAL))
       NIL
       (DEF-MTRVAR $%F1 '$%F1))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (MEVAL* '(($MODEDECLARE) $L% $ANY))
       (MEVAL* '(($DECLARE) $L% $SPECIAL))
       NIL
       (DEF-MTRVAR $L% '$L%))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (MEVAL* '(($MODEDECLARE) $SOLVEP $ANY))
       (MEVAL* '(($DECLARE) $SOLVEP $SPECIAL))
       NIL
       (DEF-MTRVAR $SOLVEP '$SOLVEP))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (MEVAL* '(($MODEDECLARE) $%R $ANY))
       (MEVAL* '(($DECLARE) $%R $SPECIAL))
       NIL
       (DEF-MTRVAR $%R '$%R))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (MEVAL* '(($MODEDECLARE) $P $ANY))
       (MEVAL* '(($DECLARE) $P $SPECIAL))
       NIL
       (DEF-MTRVAR $P '$P))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (MEVAL* '(($MODEDECLARE) $%CF $ANY))
       (MEVAL* '(($DECLARE) $%CF $SPECIAL))
       NIL
       (DEF-MTRVAR $%CF '$%CF)
(proclaim '(special $%0 $%1 $%% $y $maperror $mapprint
		    ;$%2 $n $%n $%pw $p $%g ;thing the problem was errset.
		    ))
)
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $ALGEBRAICP T TRANSLATED)
 (ADD2LNC '$ALGEBRAICP $PROPS)
 (DEFMTRFUN
  ($ALGEBRAICP $BOOLEAN MDEFINE NIL NIL)
  ($%1)
  NIL
  ((LAMBDA
    NIL
    ((LAMBDA
      (MCATCH)
      (PROG2
       NIL
       (CATCH
        'MCATCH
        (PROGN
         (SIMPLIFY
          ($SUBSTITUTE
           (SIMPLIFY
            (LIST
             '(MEQUAL)
             '&^
             (M-TLAMBDA
              ($%1 $%2)
              NIL
              (COND
               ((NOT ($INTEGERP $%2))
                ((LAMBDA (X)
                     (COND ((NULL MCATCH)
                            (DISPLA X)
                            (MERROR "THROW not within CATCH")))
                     (THROW 'MCATCH X))
                 T))))))
           $%1))
         NIL))
       (ERRLFUN1 MCATCH)))
     (CONS BINDLIST LOCLIST))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (DEFPROP $HICOEF T TRANSLATED)
       (ADD2LNC '$HICOEF $PROPS)
       (DEFMTRFUN ($HICOEF $ANY MDEFINE NIL NIL)
                  ($X $N)
                  NIL
                  (PROGN (SETQ $X (SIMPLIFY ($RATSIMP $X $N)))
                         (SIMPLIFY ($COEFF $X $N (SIMPLIFY ($HIPOW $X $N)))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (DEFPROP $GENPOL T TRANSLATED)
       (ADD2LNC '$GENPOL $PROPS)
       (DEFMTRFUN ($GENPOL $ANY MDEFINE NIL NIL)
                  ($N)
                  NIL
                  (COND ((IS-BOOLE-CHECK (MLSP $N 0)) 0)
                        (T (ADD* (SIMPLIFY ($CONCAT '$% $N))
                                 (MUL* (TRD-MSYMEVAL $%N '$%N)
                                       (SIMPLIFY ($GENPOL (ADD* $N -1)))))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $CLIST T TRANSLATED)
 (ADD2LNC '$CLIST $PROPS)
 (DEFMTRFUN
  ($CLIST $ANY MDEFINE NIL NIL)
  ($P)
  NIL
  (COND
   ((LIKE 0 (TRD-MSYMEVAL $P '$P)) '((MLIST)))
   (T
    ($CONS
     (SIMPLIFY
      ($RATDISREP (SETQ $%PW (SIMPLIFY ($RATCOEF (TRD-MSYMEVAL $P '$P)
                                                 (TRD-MSYMEVAL $%N '$%N)
                                                 0)))))
     (SIMPLIFY ($CLIST (RATF (DIV (ADD* (TRD-MSYMEVAL $P '$P)
                                        (*MMINUS (TRD-MSYMEVAL $%PW
                                                               '$%PW)))
                                  (TRD-MSYMEVAL $%N '$%N))))))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $UNSUM T TRANSLATED)
 (ADD2LNC '$UNSUM $PROPS)
 (DEFMTRFUN
  ($UNSUM $ANY MDEFINE NIL NIL)
  ($%G $%N)
  NIL
  (COND
   ((OR ($ATOM $%G)
        (NOT (LIKE ($PART $%G 0) '&+)))
    (SIMPLIFY
     ($FACTOR
      (MUL*
       (ADD*
        (DIV
         ($NUM $%G)
         (SIMPLIFY
          ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N) -1)
                       (TRD-MSYMEVAL $%N '$%N)
                       (SIMPLIFY ($PRODGUNCH ($NUM $%G)
                                             (TRD-MSYMEVAL $%N '$%N)
                                             1)))))
        (*MMINUS
         (DIV
          ($DENOM $%G)
          (SIMPLIFY
           ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N) -1)
                        (TRD-MSYMEVAL $%N '$%N)
                        (SIMPLIFY ($PRODGUNCH ($DENOM $%G)
                                              (TRD-MSYMEVAL $%N '$%N)
                                              1)))))))
       (DIV (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N) -1)
                                   (TRD-MSYMEVAL $%N '$%N)
                                   ($NUM $%G)))
            ($DENOM $%G))))))
   (T
    (SIMPLIFY
     (MAP1 (GETOPR (M-TLAMBDA ($X)
                              NIL
                              (SIMPLIFY ($UNSUM $X
                                                (TRD-MSYMEVAL $%N '$%N)))))
           $%G))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $PRODFLIP T TRANSLATED)
 (ADD2LNC '$PRODFLIP $PROPS)
 (DEFMTRFUN
  ($PRODFLIP $ANY MDEFINE NIL NIL)
  ($%0)
  NIL
  (SIMPLIFY
   ($SUBSTITUTE
    (LIST
     '(MLIST)
     (SIMPLIFY (LIST '(MEQUAL) (SIMPLIFY ($NOUNIFY '$PRODUCT)) '$PRODUCT))
     (SIMPLIFY
      (LIST '(MEQUAL)
            '$PRODUCT
            (M-TLAMBDA ($%0 $%1 $% $%%)
                       NIL
                       (DIV 1
                            (SIMPLIFY ($PRODU (DIV 1 $%0)
                                              $%1
                                              (TRD-MSYMEVAL $% '$%)
                                              (TRD-MSYMEVAL $%% '$%%))))))))
    $%0))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $PRODGUNCH T TRANSLATED)
 (ADD2LNC '$PRODGUNCH $PROPS)
 (DEFMTRFUN
  ($PRODGUNCH $ANY MDEFINE NIL NIL)
  ($%0 $%N $%2)
  NIL
  (SIMPLIFY
   ($SUBSTITUTE
    (LIST
     '(MLIST)
     (SIMPLIFY
      (LIST
       '(MEQUAL)
       (SIMPLIFY ($NOUNIFY '%SIN))
       (M-TLAMBDA&ENV
        (($%0) ($%2))
        NIL
        (MUL*
         (SIMPLIFY
          (LIST '(%SIN)
                (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                             $%2)
                                       (TRD-MSYMEVAL $%N '$%N)
                                       $%0))))
         ((LAMBDA
           ($TRIGEXPAND)
           NIL
           (SIMPLIFY
            ($EXPAND
             (DIV
              (SIMPLIFY (LIST '(%SIN) $%0))
              (SIMPLIFY
               (LIST
                '(%SIN)
                (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N
                                                           '$%N)
                                             $%2)
                                       (TRD-MSYMEVAL $%N '$%N)
                                       $%0))))))))
          T)))))
     (SIMPLIFY
      (LIST
       '(MEQUAL)
       (SIMPLIFY ($NOUNIFY '$PRODUCT))
       (M-TLAMBDA&ENV
        (($%0 $%1 $% $%3) ($%2))
        NIL
        (DIV
         (MUL*
          (SIMPLIFY
           ($FUNMAKE
            (SIMPLIFY ($NOUNIFY '$PRODUCT))
            (LIST '(MLIST)
                  $%0
                  $%1
                  (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                               $%2)
                                         (TRD-MSYMEVAL $%N '$%N)
                                         (TRD-MSYMEVAL $% '$%)))
                  (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                               $%2)
                                         (TRD-MSYMEVAL $%N '$%N)
                                         $%3)))))
          (SIMPLIFY
           ($PRODU
            $%0
            $%1
            (TRD-MSYMEVAL $% '$%)
            (ADD* (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                               $%2)
                                         (TRD-MSYMEVAL $%N '$%N)
                                         (TRD-MSYMEVAL $% '$%)))
                  -1))))
         (SIMPLIFY
          ($PRODU $%0
                  $%1
                  (ADD* $%3 1)
                  (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                               $%2)
                                         (TRD-MSYMEVAL $%N '$%N)
                                         $%3))))))))
     (SIMPLIFY
      (LIST
       '(MEQUAL)
       '%BINOMIAL
       (M-TLAMBDA&ENV
        (($%0 $%1) ($%2))
        NIL
        (MUL*
         (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N) $%2)
                                (TRD-MSYMEVAL $%N '$%N)
                                (SIMPLIFY `((%BINOMIAL) ,$%0 ,$%1))))
         (SIMPLIFY
          ($PRODU
           (DIV (ADD* $%1 '$%) (ADD* $%0 '$%))
           '$%
           1
           (ADD* (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                              $%2)
                                        (TRD-MSYMEVAL $%N '$%N)
                                        $%1))
                 (*MMINUS $%1))))
         (SIMPLIFY
          ($PRODU
           (DIV (ADD* (*MMINUS $%1) $%0 '$%)
                (ADD* (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N
                                                                 '$%N)
                                                   $%2)
                                             (TRD-MSYMEVAL $%N '$%N)
                                             $%1))
                      (*MMINUS $%1)
                      $%0
                      '$%))
           '$%
           1
           (ADD* (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                              $%2)
                                        (TRD-MSYMEVAL $%N '$%N)
                                        (ADD* $%0 (*MMINUS $%1))))
                 $%1
                 (*MMINUS $%0))))))))
     (SIMPLIFY
      (LIST
       '(MEQUAL)
       '$BETA
       (M-TLAMBDA&ENV
        (($%0 $%1) ($%2))
        NIL
        (MUL*
         (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N) $%2)
                                (TRD-MSYMEVAL $%N '$%N)
                                (SIMPLIFY (LIST '($BETA) $%0 $%1))))
         (SIMPLIFY
          ($PRODU (DIV (ADD* $%0 $%1 '$%) (ADD* $%0 '$%))
                  '$%
                  0
                  (ADD* (MUL* (SIMPLIFY ($RATCOEF $%0
                                                  (TRD-MSYMEVAL $%N
                                                                '$%N)))
                              $%2)
                        -1)))
         (SIMPLIFY
          ($PRODU
           (DIV (ADD* $%0
                      $%1
                      (MUL* $%2
                            (SIMPLIFY ($RATCOEF $%0
                                                (TRD-MSYMEVAL $%N
                                                              '$%N))))
                      '$%)
                (ADD* $%1 '$%))
           '$%
           0
           (ADD* (MUL* (SIMPLIFY ($RATCOEF $%1
                                           (TRD-MSYMEVAL $%N '$%N)))
                       $%2)
                 -1)))))))
     (SIMPLIFY
      (LIST
       '(MEQUAL)
       '&!
       (M-TLAMBDA&ENV
        (($%0) ($%2))
        NIL
        (DIV
         (SIMPLIFY `((MFACTORIAL) ,
                     (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N
                                                                '$%N)
                                                  $%2)
                                            (TRD-MSYMEVAL $%N '$%N)
                                            $%0))))
         (SIMPLIFY
          ($PRODU
           (ADD* $%0 '$%)
           '$%
           1
           (ADD* (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                              $%2)
                                        (TRD-MSYMEVAL $%N '$%N)
                                        $%0))
                 (*MMINUS $%0))))))))
     (SIMPLIFY
      (LIST
       '(MEQUAL)
       '%GAMMA
       (M-TLAMBDA&ENV
        (($%0) ($%2))
        NIL
        (DIV
         (SIMPLIFY `((%GAMMA) ,
                     (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N
                                                                '$%N)
                                                  $%2)
                                            (TRD-MSYMEVAL $%N '$%N)
                                            $%0))))
         (SIMPLIFY
          ($PRODU
           (ADD* $%0 '$% -1)
           '$%
           1
           (ADD* (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                              $%2)
                                        (TRD-MSYMEVAL $%N '$%N)
                                        $%0))
                 (*MMINUS $%0)))))))))
    $%0))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $PRODU T TRANSLATED)
 (ADD2LNC '$PRODU $PROPS)
 (DEFMTRFUN
  ($PRODU $ANY MDEFINE NIL NIL)
  ($%0 $%1 $% $%3)
  NIL
  ((LAMBDA
    ($X $Y)
    NIL
    (COND
     ((NOT ($INTEGERP $Y))
      (SIMPLIFY ($FUNMAKE (SIMPLIFY ($NOUNIFY '$PRODUCT))
                          (LIST '(MLIST)
                                $%0
                                $%1
                                (TRD-MSYMEVAL $% '$%)
                                $%3))))
     ((IS-BOOLE-CHECK (MLSP $Y -1))
      (DIV 1
           (SIMPLIFY ($PRODU $%0
                             $%1
                             (ADD* $%3 1)
                             (ADD* (TRD-MSYMEVAL $% '$%) -1)))))
     (T (DO (($I 0 (f+ 1 $I)))
            ((> $I $Y) '$DONE)
          (SETQ $X (MUL* $X
                         (SIMPLIFY ($SUBSTITUTE (ADD* $I
                                                      (TRD-MSYMEVAL $%
                                                                    '$%))
                                                $%1
                                                $%0)))))
        $X)))
   1
   (SIMPLIFY ($RATSIMP (ADD* $%3 (*MMINUS (TRD-MSYMEVAL $% '$%))))))))
#+nil
(EVAL-WHEN (COMPILE EVAL LOAD)
       (DEFPROP $NUSUM T TRANSLATED)
       (ADD2LNC '$NUSUM $PROPS)
       (DEFMTRFUN ($NUSUM NIL MDEFINE NIL NIL)
                  ($%A $%N $%L $%H)
                  NIL
                  ((LAMBDA ($MAPPRINT $PROGRAMMODE $SOLVENULLWARN)
                       NIL
                       (MAREF 'MQAPPLY
                              (SIMPLIFY ($NUSUML $%A
                                                 (TRD-MSYMEVAL $%N '$%N)
                                                 $%L
                                                 $%H
                                                 '((MLIST))))
                              1))
                   NIL
                   T
                   NIL)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $NUSUM T TRANSLATED)
 (ADD2LNC '$NUSUM $PROPS)
 (DEFMTRFUN ($NUSUM $ANY MDEFINE NIL NIL)
            ($%A $%N $%L $%H)
            NIL
            ((LAMBDA ($MAPPRINT $PROGRAMMODE $SOLVENULLWARN)
                 NIL
                 (SIMPLIFY ($FIRST (SIMPLIFY ($NUSUML $%A
                                                      (TRD-MSYMEVAL $%N
                                                                    '$%N)
                                                      $%L
                                                      $%H
                                                      '((MLIST)))))))
             NIL
             T
             NIL)))
(EVAL-WHEN (COMPILE EVAL LOAD)
       (DEFPROP $FUNCSOLVE T TRANSLATED)
       (ADD2LNC '$FUNCSOLVE $PROPS)
       (DEFMTRFUN ($FUNCSOLVE NIL MDEFINE NIL NIL)
                  ($%A $%F)
                  NIL
                  ((LAMBDA ($MAPPRINT $PROGRAMMODE $SOLVENULLWARN)
                       NIL
                       (MAREF 'MQAPPLY
                              (SIMPLIFY ($FUNCSOL $%A
                                                  (TRD-MSYMEVAL $%F '$%F)
                                                  '((MLIST))))
                              1))
                   NIL
                   T
                   NIL)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $DIMSUM T TRANSLATED)
 (ADD2LNC '$DIMSUM $PROPS)
 (DEFMTRFUN
  ($DIMSUM $ANY MDEFINE NIL NIL)
  ($%CL)
  NIL
  ((LAMBDA
    (|tr-gensym~128| |tr-gensym~129| |tr-gensym~130| |tr-gensym~131|)
    (UNWIND-PROTECT
     (PROGN
      (MSETCHK '$RATFAC |tr-gensym~128|)
      ((LAMBDA
        ($RATFAC $%CD $%PT $%PW)
        NIL
        (SETQ
         $%CD
         (SIMPLIFY
          (MAP1
           (GETOPR
            (M-TLAMBDA
             ($X)
             NIL
             (SIMPLIFY ($HIPOW (ADD* (SIMPLIFY ($RATSIMP $X))
                                     (DIV 1 (TRD-MSYMEVAL $%N '$%N)))
                               (TRD-MSYMEVAL $%N '$%N)))))
           (LIST '(MLIST)
                 (ADD* (MAREF $%CL 1) (MAREF $%CL 2))
                 (ADD* (MAREF $%CL 1) (*MMINUS (MAREF $%CL 2)))
                 (MAREF $%CL 3)))))
        (MASET (MAX (MAREF $%CD 1) (f+ (MAREF $%CD 2) -1)) $%CD 1)
        (SIMPLIFY
         ($INPART
          (SIMPLIFY
           ($SUBSTITUTE
            (SETQ
             $SOLVEP
             (SIMPLIFY
              ($SOLVE
               (SIMPLIFY
                ($CLIST
                 (SIMPLIFY
                  ($SUBSTITUTE
                   (SETQ
                    $%PT
                    (SIMPLIFY
                     ($FUNMAKE
                      'LAMBDA
                      (LIST
                       '(MLIST)
                       (LIST '(MLIST) (TRD-MSYMEVAL $%N '$%N))
                       (SIMPLIFY
                        ($GENPOL
                         (COND
                          ((AND
                            (IS-BOOLE-CHECK
                             (MLSP (MAREF $%CD 1)
                                   (MAREF $%CD 2)))
                            ($INTEGERP
                             (SETQ
                              $%PW
                              (SIMPLIFY
                               ($SUBSTITUTE
                                (SIMPLIFY
                                 ($SOLVE
                                  (SIMPLIFY
                                   ($RATCOEF
                                    (ADD*
                                     (MUL*
                                      (MAREF $%CL 1)
                                      (ADD*
                                       (TRD-MSYMEVAL
                                        $%N
                                        '$%N)
                                       '$%))
                                     (MUL*
                                      (MAREF $%CL 2)
                                      (TRD-MSYMEVAL
                                       $%N
                                       '$%N)))
                                    (TRD-MSYMEVAL $%N
                                                  '$%N)
                                    (MAREF $%CD 2)))
                                  '$%))
                                '$%)))))
                           (MAXIMUM
                            (LIST
                             (TRD-MSYMEVAL $%PW '$%PW)
                             (ADD* (MAREF $%CD 3)
                                   (*MMINUS (MAREF $%CD
                                                   1))))))
                          (T (ADD* (MAREF $%CD 3)
                                   (*MMINUS (MAREF $%CD 1)))))))))))
                   (SIMPLIFY ($INPART (TRD-MSYMEVAL $%F '$%F) 0))
                   (NCMUL2 $%CL
                           (LIST '(MLIST)
                                 (TRD-MSYMEVAL $%F1 '$%F1)
                                 (TRD-MSYMEVAL $%F '$%F)
                                 1))))))
               ($APPEND (COND (($LISTP (TRD-MSYMEVAL $L% '$L%))
                               (TRD-MSYMEVAL $L% '$L%))
                              (T
                               (SETQ $L% (LIST '(MLIST)
                                               (TRD-MSYMEVAL $L%
                                                             '$L%)))))
                        (SIMPLIFY ($CLIST (SIMPLIFY ($INPART $%PT
                                                             2))))))))
            (PROGN
             (SETQ
              $L%
              (SIMPLIFY
               (MAP1 (GETOPR '&=)
                     (TRD-MSYMEVAL $L% '$L%)
                     (SIMPLIFY ($SUBSTITUTE (TRD-MSYMEVAL $SOLVEP
                                                          '$SOLVEP)
                                            (TRD-MSYMEVAL $L% '$L%))))))
             $%PT)))
          2)))
       |tr-gensym~128|
       |tr-gensym~129|
       |tr-gensym~130|
       |tr-gensym~131|))
     (MSETCHK '$RATFAC (TRD-MSYMEVAL $RATFAC NIL))))
   NIL
   '$%CD
   '$%PT
   '$%PW)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $RATSOLVE T TRANSLATED)
 (ADD2LNC '$RATSOLVE $PROPS)
 (DEFMTRFUN
  ($RATSOLVE $ANY MDEFINE NIL NIL)
  ($P $X)
  NIL
  (SIMPLIFY
   (MAPPLY-TR
    '$APPEND
    (MAPLIST_TR
     (M-TLAMBDA&ENV
      (($P) ($X))
      NIL
      (COND
       ((OR (LIKE (SIMPLIFY ($HIPOW (TRD-MSYMEVAL $P '$P) $X)) 1)
            (LIKE (SIMPLIFY ($SUBSTITUTE 0 $X (TRD-MSYMEVAL $P '$P))) 0))
        (SIMPLIFY ($SOLVE (SIMPLIFY ($SUBSTITUTE (M-TLAMBDA ($X $Y)
                                                            NIL
                                                            $X)
                                                 '&^
                                                 (TRD-MSYMEVAL $P '$P)))
                          $X)))
       (T '((MLIST)))))
     (MUL* 2 (POWER (SIMPLIFY ($FACTOR (TRD-MSYMEVAL $P '$P))) 2)))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $PRODSHIFT T TRANSLATED)
 (ADD2LNC '$PRODSHIFT $PROPS)
 (DEFMTRFUN
  ($PRODSHIFT $ANY MDEFINE NIL NIL)
  ($%0 $%2)
  NIL
  (SIMPLIFY
   ($SUBSTITUTE
    (LIST
     '(MLIST)
     (SIMPLIFY (LIST '(MEQUAL) (SIMPLIFY ($NOUNIFY '$PRODUCT)) '$PRODUCT))
     (SIMPLIFY
      (LIST
       '(MEQUAL)
       '$PRODUCT
       (M-TLAMBDA&ENV
        (($%0 $%1 $% $%3) ($%2))
        NIL
        (SIMPLIFY ($PRODU (SIMPLIFY ($SUBSTITUTE (ADD* $%1
                                                       (*MMINUS $%2))
                                                 $%1
                                                 $%0))
                          $%1
                          (ADD* (TRD-MSYMEVAL $% '$%) $%2)
                          (ADD* $%3 $%2)))))))
    $%0))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $RFORN T TRANSLATED)
 (ADD2LNC '$RFORN $PROPS)
 (DEFMTRFUN
  ($RFORN $ANY MDEFINE NIL NIL)
  ($%3)
  NIL
  ((LAMBDA
    (|tr-gensym~132| |tr-gensym~133|)
    (UNWIND-PROTECT
     (PROGN
      (MSETCHK '$RATFAC |tr-gensym~133|)
      ((LAMBDA
        ($Y $RATFAC)
        NIL
        (SETQ $P (MUL* (TRD-MSYMEVAL $P '$P)
                       (SIMPLIFY ($PRODU $Y
                                         (TRD-MSYMEVAL $%N '$%N)
                                         (TRD-MSYMEVAL $%N '$%N)
                                         (ADD* (TRD-MSYMEVAL $%N '$%N)
                                               $%3
                                               -1)))))
        (SETQ
         $%R
         (SIMPLIFY
          ($RATSIMP
           (DIV (TRD-MSYMEVAL $%R '$%R)
                (LIST '(MLIST)
                      (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N
                                                                 '$%N)
                                                   $%3)
                                             (TRD-MSYMEVAL $%N '$%N)
                                             $Y))
                      $Y))))))
       |tr-gensym~132|
       |tr-gensym~133|))
     (MSETCHK '$RATFAC (TRD-MSYMEVAL $RATFAC NIL))))
   (SIMPLIFY ($GCD (MAREF (TRD-MSYMEVAL $%R '$%R) 2)
                   (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N)
                                                (*MMINUS $%3))
                                          (TRD-MSYMEVAL $%N '$%N)
                                          (MAREF (TRD-MSYMEVAL $%R '$%R)
                                                 1)))))
   T)))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $RFORM T TRANSLATED)
 (ADD2LNC '$RFORM $PROPS)
 (DEFMTRFUN
  ($RFORM $ANY MDEFINE NIL NIL)
  ($%R)
  NIL
  (COND
   ((IS-BOOLE-CHECK (SIMPLIFY (RATP (DIV (MAREF (TRD-MSYMEVAL $%R '$%R) 1)
                                         (MAREF (TRD-MSYMEVAL $%R '$%R) 2))
                                    (TRD-MSYMEVAL $%N '$%N))))
    (COND (($ALGEBRAICP (TRD-MSYMEVAL $%R '$%R))
           (PROGN (MSETCHK '$GCD '$RED)
                  (SETQ $GCD '$RED))
           (SETQ $ALGEBRAIC T)))
    ((LAMBDA
      ($P)
      NIL
      (SIMPLIFY ($RFORN 1))
      (DO
       (($%3)
        (MDO
         (CDR
          (SIMPLIFY
           ($RATSOLVE
            (SIMPLIFY
             ($RESULTANT
              (MAREF (TRD-MSYMEVAL $%R '$%R) 1)
              (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N '$%N) '$%)
                                     (TRD-MSYMEVAL $%N '$%N)
                                     (MAREF (TRD-MSYMEVAL $%R '$%R) 2)))
              (TRD-MSYMEVAL $%N '$%N)))
            '$%)))
         (CDR MDO)))
       ((NULL MDO) '$DONE)
       (SETQ $%3 (CAR MDO))
       (COND
        ((AND ($INTEGERP (SETQ $%3 (SIMPLIFY ($SUBSTITUTE (LIST '(MLIST)
                                                                $%3)
                                                          '$%))))
              (IS-BOOLE-CHECK (MGRP $%3 0)))
         (SIMPLIFY ($RFORN $%3)))))
      (LIST '(MLIST)
            (TRD-MSYMEVAL $P '$P)
            (DIV (MAREF (TRD-MSYMEVAL $%R '$%R) 1)
                 (MAREF (TRD-MSYMEVAL $%R '$%R) 2))))
     1))
   (T (SIMPLIFY ($ERROR (DIV (MAREF (TRD-MSYMEVAL $%R '$%R) 1)
                             (MAREF (TRD-MSYMEVAL $%R '$%R) 2))
                        '|&NON-RATIONAL TERM RATIO TO NUSUM|))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $NUSUML T TRANSLATED)
 (ADD2LNC '$NUSUML $PROPS)
 (DEFMTRFUN
  ($NUSUML $ANY MDEFINE NIL NIL)
  ($%A $%N $%L $%H $L%)
  NIL
  (COND
   ((LIKE $%A 0) (LIST '(MLIST) 0))
   (T
    ((LAMBDA
      (|tr-gensym~135| |tr-gensym~136|
                       |tr-gensym~137|
                       |tr-gensym~138|
                       |tr-gensym~139|
                       |tr-gensym~140|
                       |tr-gensym~141|
                       |tr-gensym~142|
                       |tr-gensym~143|
                       |tr-gensym~144|
                       |tr-gensym~145|
                       |tr-gensym~146|
                       |tr-gensym~147|)
      (UNWIND-PROTECT
       (PROGN
        (MSETCHK 'MODULUS |tr-gensym~136|)
        (MSETCHK '$RATFAC |tr-gensym~139|)
        (MSETCHK '$GCD |tr-gensym~140|)
        ((LAMBDA
          ($SOLVEP MODULUS
                   $RV
                   $PRODHACK
                   $RATFAC
                   $GCD
                   $ALGEBRAIC
                   $RATALGDENOM
                   $MATRIX_ELEMENT_MULT
                   $DISPFLAG
                   $MAPERROR
                   $%F
                   $%F1)
          NIL
          (SIMPLIFY ($RATVARS (TRD-MSYMEVAL $%N '$%N)))
          (COND
           ((AND
             (NOT
              (LIKE
               '((MLIST))
               ((LAMBDA
                 (ERRCATCH RET)
                 (COND
                  ((NULL
                    (SETQ
                     RET
                     (ERRSET
                      (PROGN
                       (SETQ
                        $%CF
                        (SIMPLIFY
                         ($DIMSUM
                          (LIST
                           '(MLIST)
                           (*MMINUS
                            ($NUM
                             (MAREF
                              'MQAPPLY
                              (SETQ
                               $%R
                               (SIMPLIFY
                                ($RFORM
                                 ((LAMBDA ($%A)
                                      NIL
                                      (LIST '(MLIST)
                                            ($NUM $%A)
                                            ($DENOM $%A)))
                                  (SIMPLIFY
                                   ($FACTOR
                                    (DIV
                                     (SIMPLIFY
                                      ($SUBSTITUTE
                                       (ADD*
                                        (TRD-MSYMEVAL
                                         $%N
                                         '$%N)
                                        1)
                                       (TRD-MSYMEVAL
                                        $%N
                                        '$%N)
                                       $%A))
                                     (SIMPLIFY
                                      ($PRODGUNCH
                                       $%A
                                       (TRD-MSYMEVAL
                                        $%N
                                        '$%N)
                                       1)))))))))
                              2)))
                           (SIMPLIFY
                            ($SUBSTITUTE
                             (ADD* (TRD-MSYMEVAL $%N '$%N) -1)
                             (TRD-MSYMEVAL $%N '$%N)
                             ($DENOM
                              (MAREF (TRD-MSYMEVAL $%R '$%R)
                                     2))))
                           (MAREF (TRD-MSYMEVAL $%R '$%R) 1))))))
                      LISPERRPRINT)))
                   (ERRLFUN1 ERRCATCH)))
                 (CONS '(MLIST) RET))
                (CONS BINDLIST LOCLIST)
                NIL)))
             (NOT (LIKE '((MLIST)) (TRD-MSYMEVAL $SOLVEP '$SOLVEP))))
            ($CONS
             (PROGN
              (SETQ $%F (DIV (SIMPLIFY ($PRODGUNCH ($NUM $%A)
                                                   (TRD-MSYMEVAL $%N
                                                                 '$%N)
                                                   1))
                             ($DENOM $%A)))
              (SETQ
               $%F1
               (SIMPLIFY
                ($RATSIMP (SIMPLIFY ($RADCAN (TRD-MSYMEVAL $%CF
                                                           '$%CF))))))
              (SIMPLIFY (MAPPLY-TR '$RATVARS $RV))
              (SETQ
               $%F1
               (SIMPLIFY
                ($SUBSTITUTE
                 (M-TLAMBDA ($%0 $%1 $% $%3)
                            NIL
                            (SIMPLIFY ($PRODU $%0
                                              $%1
                                              (TRD-MSYMEVAL $% '$%)
                                              $%3)))
                 (SIMPLIFY ($NOUNIFY '$PRODUCT))
                 (ADD*
                  (SIMPLIFY
                   ($FACTOR
                    (SIMPLIFY
                     ($SUBSTITUTE
                      $%H
                      (TRD-MSYMEVAL $%N '$%N)
                      (SIMPLIFY
                       ($FACTOR
                        (DIV
                         (MUL*
                          ($NUM (MAREF (TRD-MSYMEVAL $%R '$%R)
                                       2))
                          (TRD-MSYMEVAL $%F '$%F)
                          (SIMPLIFY
                           ($SUBSTITUTE
                            (ADD* (TRD-MSYMEVAL $%N '$%N) 1)
                            (TRD-MSYMEVAL $%N '$%N)
                            (TRD-MSYMEVAL $%F1 '$%F1))))
                         (MAREF (TRD-MSYMEVAL $%R '$%R) 1))))))))
                  (*MMINUS
                   (SIMPLIFY
                    ($FACTOR
                     (SIMPLIFY
                      ($SUBSTITUTE
                       $%L
                       (TRD-MSYMEVAL $%N '$%N)
                       (SIMPLIFY
                        ($FACTOR
                         (DIV
                          (MUL*
                           $%A
                           (TRD-MSYMEVAL $%F1 '$%F1)
                           (SIMPLIFY
                            ($SUBSTITUTE
                             (ADD* (TRD-MSYMEVAL $%N '$%N)
                                   -1)
                             (TRD-MSYMEVAL $%N '$%N)
                             ($DENOM
                              (MAREF (TRD-MSYMEVAL $%R
                                                   '$%R)
                                     2)))))
                          (MAREF (TRD-MSYMEVAL $%R '$%R) 1)))))))))))))
              (COND
               ((IS-BOOLE-CHECK (SIMPLIFY (RATP $%A
                                                (TRD-MSYMEVAL $%N
                                                              '$%N))))
                (SIMPLIFY ($FACTOR (TRD-MSYMEVAL $%F1 '$%F1))))
               (T (TRD-MSYMEVAL $%F1 '$%F1))))
             (TRD-MSYMEVAL $L% '$L%)))
           (T (SIMPLIFY (MAPPLY-TR '$RATVARS $RV))
              (LIST '(MLIST)
                    (SIMPLIFY (MFUNCALL '$SUM
                                        $%A
                                        (TRD-MSYMEVAL $%N '$%N)
                                        $%L
                                        $%H))))))
         |tr-gensym~135|
         |tr-gensym~136|
         |tr-gensym~137|
         |tr-gensym~138|
         |tr-gensym~139|
         |tr-gensym~140|
         |tr-gensym~141|
         |tr-gensym~142|
         |tr-gensym~143|
         |tr-gensym~144|
         |tr-gensym~145|
         |tr-gensym~146|
         |tr-gensym~147|))
       (MSETCHK 'MODULUS (TRD-MSYMEVAL MODULUS 'MODULUS))
       (MSETCHK '$RATFAC (TRD-MSYMEVAL $RATFAC NIL))
       (MSETCHK '$GCD (TRD-MSYMEVAL $GCD '$GCD))))
     NIL
     NIL
     (TRD-MSYMEVAL $RATVARS '$RATVARS)
     T
     T
     '$SPMOD
     NIL
     T
     '&*
     NIL
     NIL
     (SIMPLIFY ($FUNMAKE '$%F (LIST '(MLIST) (TRD-MSYMEVAL $%N '$%N))))
     (SIMPLIFY ($FUNMAKE '$%F
                         (LIST '(MLIST) (ADD* (TRD-MSYMEVAL $%N '$%N) 1)))))))))
(EVAL-WHEN (COMPILE EVAL LOAD)
 (DEFPROP $FUNCSOL T TRANSLATED)
 (ADD2LNC '$FUNCSOL $PROPS)
 (DEFMTRFUN
  ($FUNCSOL $ANY MDEFINE NIL NIL)
  ($%A $%F $L%)
  NIL
  ((LAMBDA
    (|tr-gensym~148| |tr-gensym~149|
                     |tr-gensym~150|
                     |tr-gensym~151|
                     |tr-gensym~152|
                     |tr-gensym~153|
                     |tr-gensym~154|
                     |tr-gensym~155|)
    (UNWIND-PROTECT
     (PROGN
      (MSETCHK '$RATFAC |tr-gensym~148|)
      ((LAMBDA
        ($RATFAC $MAPERROR $LINENUM $DISPFLAG $%F1 $%CL $%CM $%N)
        NIL
        (SETQ
         $%F1
         (SIMPLIFY
          ($SUBSTITUTE (SIMPLIFY (LIST '(MEQUAL)
                                       (TRD-MSYMEVAL $%N '$%N)
                                       (ADD* (TRD-MSYMEVAL $%N '$%N) 1)))
                       (TRD-MSYMEVAL $%F '$%F))))
        (SETQ
         $%CL
         (SIMPLIFY
          ($FACTOR
           (MAREF
            'MQAPPLY
            (SIMPLIFY
             ($AUGCOEFMATRIX
              (LIST
               '(MLIST)
               (SETQ
                $%A
                ($NUM (SIMPLIFY ($XTHRU (ADD* ($LHS $%A)
                                              (*MMINUS ($RHS $%A))))))))
              (LIST '(MLIST)
                    (TRD-MSYMEVAL $%F1 '$%F1)
                    (TRD-MSYMEVAL $%F '$%F))))
            1))))
        (SETQ $%CM (SIMPLIFY ($RFORM (SIMPLIFY ($REST $%CL -1)))))
        (MASET
         (SIMPLIFY
          ($RATSIMP (DIV (SIMPLIFY ($SUBSTITUTE (ADD* (TRD-MSYMEVAL $%N
                                                                    '$%N)
                                                      1)
                                                (TRD-MSYMEVAL $%N '$%N)
                                                (MAREF $%CM 1)))
                         (MAREF $%CM 1))))
         $%CM
         2)
        ($APPEND
         ((LAMBDA
           (ERRCATCH RET)
           (COND
            ((NULL
              (SETQ
               RET
               (ERRSET
                (PROGN
                 (SIMPLIFY
                  (LIST
                   '(MEQUAL)
                   (TRD-MSYMEVAL $%F '$%F)
                   (SIMPLIFY
                    ($FACTOR
                     (DIV
                      (SIMPLIFY
                       ($DIMSUM
                        (SIMPLIFY
                         ($RATSIMP
                          (LIST '(MLIST)
                                (DIV (MAREF $%CL 1)
                                     ($NUM (MAREF $%CM 2)))
                                (DIV (MAREF $%CL 2)
                                     ($DENOM (MAREF $%CM 2)))
                                (DIV (MUL* (MAREF $%CM 1)
                                           (MAREF $%CL 3))
                                     ($DENOM (MAREF $%CM 2))))))))
                      (MAREF $%CM 1)))))))
                LISPERRPRINT)))
             (ERRLFUN1 ERRCATCH)))
           (CONS '(MLIST) RET))
          (CONS BINDLIST LOCLIST)
          NIL)
         (TRD-MSYMEVAL $L% '$L%)))
       |tr-gensym~148|
       |tr-gensym~149|
       |tr-gensym~150|
       |tr-gensym~151|
       |tr-gensym~152|
       |tr-gensym~153|
       |tr-gensym~154|
       |tr-gensym~155|))
     (MSETCHK '$RATFAC (TRD-MSYMEVAL $RATFAC NIL))))
   T
   NIL
   (TRD-MSYMEVAL $LINENUM '$LINENUM)
   NIL
   '$%F1
   '$%CL
   '$%CM
   (SIMPLIFY ($INPART (TRD-MSYMEVAL $%F '$%F) 1)))))
