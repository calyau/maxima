;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 6/08/85 17:56:35;;Maxima System version 16
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
(eval-when (compile eval load)
       (MEVAL* '(($MODEDECLARE) $BESTLENGTH $FIXNUM))
       (MEVAL* '(($DECLARE) $BESTLENGTH $SPECIAL))
       (DEFPROP $BESTLENGTH ASSIGN-MODE-CHECK ASSIGN)
       (DEF-MTRVAR $BESTLENGTH 0))
(eval-when (compile eval load)
       (MEVAL* '(($MODEDECLARE) $TRYLENGTH $FIXNUM))
       (MEVAL* '(($DECLARE) $TRYLENGTH $SPECIAL))
       (DEFPROP $TRYLENGTH ASSIGN-MODE-CHECK ASSIGN)
       (DEF-MTRVAR $TRYLENGTH 0))
(eval-when (compile eval load)
(proclaim '(special $ans ))

(SIMPLIFY ($PUT '%SIN '%COS '$COMPLEMENT_FUNCTION))
(SIMPLIFY ($PUT '%COS '%SIN '$COMPLEMENT_FUNCTION))
(SIMPLIFY ($PUT '%SINH '%COSH '$COMPLEMENT_FUNCTION))
(SIMPLIFY ($PUT '%COSH '%SINH '$COMPLEMENT_FUNCTION))
(SIMPLIFY ($PUT '%COS 1 '$UNITCOF))
(SIMPLIFY ($PUT '%SIN 1 '$UNITCOF))
(SIMPLIFY ($PUT '%COSH 1 '$UNITCOF))
(SIMPLIFY ($PUT '%SINH -1 '$UNITCOF))
(SIMPLIFY ($PUT '%COS -1 '$COMPLEMENT_COF))
(SIMPLIFY ($PUT '%SIN -1 '$COMPLEMENT_COF))
(SIMPLIFY ($PUT '%COSH 1 '$COMPLEMENT_COF))
(SIMPLIFY ($PUT '%SINH 1 '$COMPLEMENT_COF))
(SIMPLIFY ($PUT '%SIN '$TRIGONOMETRIC '$TYPE))
(SIMPLIFY ($PUT '%COS '$TRIGONOMETRIC '$TYPE))
(SIMPLIFY ($PUT '%SINH '$HYPER_TRIGONOMETRIC '$TYPE))
(SIMPLIFY ($PUT '%COSH '$HYPER_TRIGONOMETRIC '$TYPE))
)
NIL
(EVAL-WHEN (COMPILE LOAD EVAL) (MEVAL* '(($DECLARE) $LIST2 
                                                              $SPECIAL)))
(eval-when (compile eval load)
       (DEFPROP $TRIGONOMETRICP T TRANSLATED)
       (ADD2LNC '$TRIGONOMETRICP $PROPS)
       (DEFMTRFUN ($TRIGONOMETRICP $BOOLEAN MDEFINE NIL NIL)
                  ($EXP)
                  NIL
                  (OR (LIKE (SIMPLIFY ($GET (SIMPLIFY ($INPART $EXP 0))
                                            '$TYPE))
                            '$TRIGONOMETRIC)
                      (LIKE (SIMPLIFY ($GET (TRD-MSYMEVAL $PIECE '$PIECE)
                                            '$TYPE))
                            '$HYPER_TRIGONOMETRIC))))
(eval-when (compile eval load)
       (DEFUN $TRIGRULE0
              (|tr-gensym~0|)
              (CATCH 'MATCH
                      (PROG ($A |tr-gensym~1| |tr-gensym~2|)
                            (DECLARE (SPECIAL $A |tr-gensym~1| |tr-gensym~2|))
                            (COND ((NOT (EQUAL (KAR (KAR |tr-gensym~0|))
                                               '%TAN))
                                   (MATCHERR)))
                            (SETQ |tr-gensym~1| (KDR |tr-gensym~0|))
                            (SETQ |tr-gensym~2| (KAR |tr-gensym~1|))
                            (SETQ $A |tr-gensym~2|)
                            (COND ((NTHKDR |tr-gensym~1| 1)
                                   (MATCHERR)))
                            (RETURN (MUL* (POWER (SIMPLIFY (LIST '(%COS) $A))
                                                 -1)
                                          (SIMPLIFY (LIST '(%SIN) $A)))))))
       (ADD2LNC '$TRIGRULE0 $RULES)
       (MDEFPROP $TRIGRULE0
                 ((MEQUAL) ((%TAN SIMP) $A)
                           ((MTIMES SIMP) ((MEXPT SIMP) ((%COS SIMP) $A) -1)
                                          ((%SIN SIMP) $A)))
                 $RULE)
       (MDEFPROP $TRIGRULE0 $DEFRULE $RULETYPE))
(eval-when (compile eval load)
       (DEFUN $TRIGRULE1
              (|tr-gensym~3|)
              (CATCH 'MATCH
                      (PROG ($A |tr-gensym~4| |tr-gensym~5|)
                            (DECLARE (SPECIAL $A |tr-gensym~4| |tr-gensym~5|))
                            (COND ((NOT (EQUAL (KAR (KAR |tr-gensym~3|))
                                               '%TAN))
                                   (MATCHERR)))
                            (SETQ |tr-gensym~4| (KDR |tr-gensym~3|))
                            (SETQ |tr-gensym~5| (KAR |tr-gensym~4|))
                            (SETQ $A |tr-gensym~5|)
                            (COND ((NTHKDR |tr-gensym~4| 1)
                                   (MATCHERR)))
                            (RETURN (MUL* (POWER (SIMPLIFY (LIST '(%COS) $A))
                                                 -1)
                                          (SIMPLIFY (LIST '(%SIN) $A)))))))
       (ADD2LNC '$TRIGRULE1 $RULES)
       (MDEFPROP $TRIGRULE1
                 ((MEQUAL) ((%TAN SIMP) $A)
                           ((MTIMES SIMP) ((MEXPT SIMP) ((%COS SIMP) $A) -1)
                                          ((%SIN SIMP) $A)))
                 $RULE)
       (MDEFPROP $TRIGRULE1 $DEFRULE $RULETYPE))
(eval-when (compile eval load)
       (DEFUN $TRIGRULE2
              (|tr-gensym~6|)
              (CATCH 'MATCH
                      (PROG ($A |tr-gensym~7| |tr-gensym~8|)
                            (DECLARE (SPECIAL $A |tr-gensym~7| |tr-gensym~8|))
                            (COND ((NOT (EQUAL (KAR (KAR |tr-gensym~6|))
                                               '%SEC))
                                   (MATCHERR)))
                            (SETQ |tr-gensym~7| (KDR |tr-gensym~6|))
                            (SETQ |tr-gensym~8| (KAR |tr-gensym~7|))
                            (SETQ $A |tr-gensym~8|)
                            (COND ((NTHKDR |tr-gensym~7| 1)
                                   (MATCHERR)))
                            (RETURN (POWER (SIMPLIFY (LIST '(%COS) $A)) -1)))))
       (ADD2LNC '$TRIGRULE2 $RULES)
       (MDEFPROP $TRIGRULE2
                 ((MEQUAL) ((%SEC SIMP) $A) ((MEXPT SIMP) ((%COS SIMP) $A) -1))
                 $RULE)
       (MDEFPROP $TRIGRULE2 $DEFRULE $RULETYPE))
(eval-when (compile eval load)
       (DEFUN $TRIGRULE3
              (|tr-gensym~9|)
              (CATCH 'MATCH
                      (PROG ($A |tr-gensym~10| |tr-gensym~11|)
                            (DECLARE (SPECIAL $A
                                              |tr-gensym~10|
                                              |tr-gensym~11|))
                            (COND ((NOT (EQUAL (KAR (KAR |tr-gensym~9|))
                                               '%CSC))
                                   (MATCHERR)))
                            (SETQ |tr-gensym~10| (KDR |tr-gensym~9|))
                            (SETQ |tr-gensym~11| (KAR |tr-gensym~10|))
                            (SETQ $A |tr-gensym~11|)
                            (COND ((NTHKDR |tr-gensym~10| 1)
                                   (MATCHERR)))
                            (RETURN (POWER (SIMPLIFY (LIST '(%SIN) $A)) -1)))))
       (ADD2LNC '$TRIGRULE3 $RULES)
       (MDEFPROP $TRIGRULE3
                 ((MEQUAL) ((%CSC SIMP) $A) ((MEXPT SIMP) ((%SIN SIMP) $A) -1))
                 $RULE)
       (MDEFPROP $TRIGRULE3 $DEFRULE $RULETYPE))
(eval-when (compile eval load)
       (DEFUN $TRIGRULE4
              (|tr-gensym~12|)
              (CATCH 'MATCH
                      (PROG ($A |tr-gensym~13| |tr-gensym~14|)
                            (DECLARE (SPECIAL $A
                                              |tr-gensym~13|
                                              |tr-gensym~14|))
                            (COND ((NOT (EQUAL (KAR (KAR |tr-gensym~12|))
                                               '%COT))
                                   (MATCHERR)))
                            (SETQ |tr-gensym~13| (KDR |tr-gensym~12|))
                            (SETQ |tr-gensym~14| (KAR |tr-gensym~13|))
                            (SETQ $A |tr-gensym~14|)
                            (COND ((NTHKDR |tr-gensym~13| 1)
                                   (MATCHERR)))
                            (RETURN (MUL* (SIMPLIFY (LIST '(%COS) $A))
                                          (POWER (SIMPLIFY (LIST '(%SIN)
                                                                 $A))
                                                 -1))))))
       (ADD2LNC '$TRIGRULE4 $RULES)
       (MDEFPROP $TRIGRULE4
                 ((MEQUAL) ((%COT SIMP) $A)
                           ((MTIMES SIMP) ((%COS SIMP) $A)
                                          ((MEXPT SIMP) ((%SIN SIMP) $A) -1)))
                 $RULE)
       (MDEFPROP $TRIGRULE4 $DEFRULE $RULETYPE))
(eval-when (compile eval load)
       (DEFUN $HTRIGRULE1
              (|tr-gensym~15|)
              (CATCH 'MATCH
                      (PROG ($A |tr-gensym~16| |tr-gensym~17|)
                            (DECLARE (SPECIAL $A
                                              |tr-gensym~16|
                                              |tr-gensym~17|))
                            (COND ((NOT (EQUAL (KAR (KAR |tr-gensym~15|))
                                               '%TANH))
                                   (MATCHERR)))
                            (SETQ |tr-gensym~16| (KDR |tr-gensym~15|))
                            (SETQ |tr-gensym~17| (KAR |tr-gensym~16|))
                            (SETQ $A |tr-gensym~17|)
                            (COND ((NTHKDR |tr-gensym~16| 1)
                                   (MATCHERR)))
                            (RETURN (MUL* (POWER (SIMPLIFY (LIST '(%COSH)
                                                                 $A))
                                                 -1)
                                          (SIMPLIFY (LIST '(%SINH) $A)))))))
       (ADD2LNC '$HTRIGRULE1 $RULES)
       (MDEFPROP $HTRIGRULE1
                 ((MEQUAL) ((%TANH SIMP) $A)
                           ((MTIMES SIMP) ((MEXPT SIMP) ((%COSH SIMP) $A) -1)
                                          ((%SINH SIMP) $A)))
                 $RULE)
       (MDEFPROP $HTRIGRULE1 $DEFRULE $RULETYPE))
(eval-when (compile eval load)
       (DEFUN $HTRIGRULE2
              (|tr-gensym~18|)
              (CATCH 'MATCH
                      (PROG ($A |tr-gensym~19| |tr-gensym~20|)
                            (DECLARE (SPECIAL $A
                                              |tr-gensym~19|
                                              |tr-gensym~20|))
                            (COND ((NOT (EQUAL (KAR (KAR |tr-gensym~18|))
                                               '%SECH))
                                   (MATCHERR)))
                            (SETQ |tr-gensym~19| (KDR |tr-gensym~18|))
                            (SETQ |tr-gensym~20| (KAR |tr-gensym~19|))
                            (SETQ $A |tr-gensym~20|)
                            (COND ((NTHKDR |tr-gensym~19| 1)
                                   (MATCHERR)))
                            (RETURN (POWER (SIMPLIFY (LIST '(%COSH) $A)) -1)))))
       (ADD2LNC '$HTRIGRULE2 $RULES)
       (MDEFPROP $HTRIGRULE2
                 ((MEQUAL) ((%SECH SIMP) $A)
                           ((MEXPT SIMP) ((%COSH SIMP) $A) -1))
                 $RULE)
       (MDEFPROP $HTRIGRULE2 $DEFRULE $RULETYPE))
(eval-when (compile eval load)
       (DEFUN $HTRIGRULE3
              (|tr-gensym~21|)
              (CATCH 'MATCH
                      (PROG ($A |tr-gensym~22| |tr-gensym~23|)
                            (DECLARE (SPECIAL $A
                                              |tr-gensym~22|
                                              |tr-gensym~23|))
                            (COND ((NOT (EQUAL (KAR (KAR |tr-gensym~21|))
                                               '%CSCH))
                                   (MATCHERR)))
                            (SETQ |tr-gensym~22| (KDR |tr-gensym~21|))
                            (SETQ |tr-gensym~23| (KAR |tr-gensym~22|))
                            (SETQ $A |tr-gensym~23|)
                            (COND ((NTHKDR |tr-gensym~22| 1)
                                   (MATCHERR)))
                            (RETURN (POWER (SIMPLIFY (LIST '(%SINH) $A)) -1)))))
       (ADD2LNC '$HTRIGRULE3 $RULES)
       (MDEFPROP $HTRIGRULE3
                 ((MEQUAL) ((%CSCH SIMP) $A)
                           ((MEXPT SIMP) ((%SINH SIMP) $A) -1))
                 $RULE)
       (MDEFPROP $HTRIGRULE3 $DEFRULE $RULETYPE))
(eval-when (compile eval load)
       (DEFUN $HTRIGRULE4
              (|tr-gensym~24|)
              (CATCH 'MATCH
                      (PROG ($A |tr-gensym~25| |tr-gensym~26|)
                            (DECLARE (SPECIAL $A
                                              |tr-gensym~25|
                                              |tr-gensym~26|))
                            (COND ((NOT (EQUAL (KAR (KAR |tr-gensym~24|))
                                               '%COTH))
                                   (MATCHERR)))
                            (SETQ |tr-gensym~25| (KDR |tr-gensym~24|))
                            (SETQ |tr-gensym~26| (KAR |tr-gensym~25|))
                            (SETQ $A |tr-gensym~26|)
                            (COND ((NTHKDR |tr-gensym~25| 1)
                                   (MATCHERR)))
                            (RETURN (MUL* (SIMPLIFY (LIST '(%COSH) $A))
                                          (POWER (SIMPLIFY (LIST '(%SINH)
                                                                 $A))
                                                 -1))))))
       (ADD2LNC '$HTRIGRULE4 $RULES)
       (MDEFPROP $HTRIGRULE4
                 ((MEQUAL) ((%COTH SIMP) $A)
                           ((MTIMES SIMP) ((%COSH SIMP) $A)
                                          ((MEXPT SIMP) ((%SINH SIMP) $A) -1)))
                 $RULE)
       (MDEFPROP $HTRIGRULE4 $DEFRULE $RULETYPE))
(eval-when (compile eval load)
 (DEFPROP $TRIGSIMP T TRANSLATED)
 (ADD2LNC '$TRIGSIMP $PROPS)
 (DEFMTRFUN
  ($TRIGSIMP $ANY MDEFINE NIL NIL)
  ($X)
  NIL
  (SIMPLIFY
   ($TRIGSIMP3
    (SIMPLIFY ($RADCAN (DO ((|tr-gensym~27| $X
                                            (APPLY1 |tr-gensym~27|
                                                    (CAR |tr-gensym~28|)
                                                    0))
                            (|tr-gensym~28| '($TRIGRULE1 $TRIGRULE2 
                                              $TRIGRULE3 $TRIGRULE4 
                                              $HTRIGRULE1 $HTRIGRULE2 
                                              $HTRIGRULE3 $HTRIGRULE4)
                                            (CDR |tr-gensym~28|)))
                           ((NULL |tr-gensym~28|) |tr-gensym~27|)
                        )))))))
(eval-when (compile eval load)
 (DEFPROP $TRIGSIMP3 T TRANSLATED)
 (ADD2LNC '$TRIGSIMP3 $PROPS)
 (DEFMTRFUN
  ($TRIGSIMP3 $ANY MDEFINE NIL NIL)
  ($EXPN)
  NIL
  (PROGN (SETQ $EXPN (SIMPLIFY ($TOTALDISREP $EXPN)))
         (SIMPLIFY ($RATSIMP (DIV (SIMPLIFY ($TRIGSIMP1 ($NUM $EXPN)))
                                  (SIMPLIFY ($TRIGSIMP1 ($DENOM $EXPN)))))))))
(eval-when (compile eval load)
       (DEFPROP $TRIGSIMP1 T TRANSLATED)
       (ADD2LNC '$TRIGSIMP1 $PROPS)
       (DEFMTRFUN ($TRIGSIMP1 $ANY MDEFINE NIL NIL)
                  ($EXPN)
                  NIL
                  ((LAMBDA ($LISTOFTRIGSQ $BESTLENGTH $TRYLENGTH)
                       NIL
                       (ASSIGN-MODE-CHECK '$TRYLENGTH $TRYLENGTH)
                       (ASSIGN-MODE-CHECK '$BESTLENGTH $BESTLENGTH)
                       (SETQ $LISTOFTRIGSQ (SIMPLIFY ($LISTOFTRIGSQ $EXPN)))
                       (PROGN (ASSIGN-MODE-CHECK '$BESTLENGTH 999999)
                              (SETQ $BESTLENGTH 999999))
                       (COND ((NOT (LIKE $LISTOFTRIGSQ '((MLIST))))
                              (SIMPLIFY ($IMPROVE $EXPN
                                                  $EXPN
                                                  $LISTOFTRIGSQ)))
                             (T $EXPN)))
                   '$LISTOFTRIGSQ
                   0
                   0)))
(eval-when (compile eval load)
 (DEFPROP $IMPROVE T TRANSLATED)
 (ADD2LNC '$IMPROVE $PROPS)
 (DEFMTRFUN
  ($IMPROVE $ANY MDEFINE NIL NIL)
  ($EXPN $SUBSOFAR $LISTOFTRIGSQ)
  NIL
  (COND
   ((LIKE $LISTOFTRIGSQ '((MLIST)))
    (COND ((< ((LAMBDA (|tr-gensym~31|)
                   (PROGN (ASSIGN-MODE-CHECK '$TRYLENGTH |tr-gensym~31|)
                          (SETQ $TRYLENGTH |tr-gensym~31|)))
               ($EXPNLENGTH $SUBSOFAR))
              (TRD-MSYMEVAL $BESTLENGTH 0))
           ((LAMBDA (|tr-gensym~30|)
                (PROGN (ASSIGN-MODE-CHECK '$BESTLENGTH |tr-gensym~30|)
                       (SETQ $BESTLENGTH |tr-gensym~30|)))
            (TRD-MSYMEVAL $TRYLENGTH 0))
           $SUBSOFAR)
          (T $EXPN)))
   (T
    (SETQ $SUBSOFAR (SIMPLIFY ($IMPROVE $EXPN
                                        $SUBSOFAR
                                        (SIMPLIFY ($REST $LISTOFTRIGSQ)))))
    (DO
     (($ALT) (MDO (CDR (SIMPLIFY ($FIRST $LISTOFTRIGSQ))) (CDR MDO)))
     ((NULL MDO) '$DONE)
     (SETQ $ALT (CAR MDO))
     (SETQ
      $SUBSOFAR
      (SIMPLIFY
       ($IMPROVE
        $SUBSOFAR
        (SIMPLIFY
         ($RATSUBST
          (ADD*
           (SIMPLIFY ($GET (SIMPLIFY ($INPART $ALT 0)) '$UNITCOF))
           (MUL*
            (SIMPLIFY ($GET (TRD-MSYMEVAL $PIECE '$PIECE)
                            '$COMPLEMENT_COF))
            (POWER
             (SIMPLIFY (MAPPLY (SIMPLIFY ($GET (TRD-MSYMEVAL $PIECE
                                                             '$PIECE)
                                               '$COMPLEMENT_FUNCTION))
                               (LIST (SIMPLIFY ($FIRST $ALT)))
                               '(($GET) $PIECE 
                                 ((MQUOTE) $COMPLEMENT_FUNCTION))))
             2)))
          (POWER $ALT 2)
          $SUBSOFAR))
        (SIMPLIFY ($REST $LISTOFTRIGSQ))))))
    $SUBSOFAR))))
(eval-when (compile eval load)
 (DEFPROP $LISTOFTRIGSQ T TRANSLATED)
 (ADD2LNC '$LISTOFTRIGSQ $PROPS)
 (DEFMTRFUN
  ($LISTOFTRIGSQ $ANY MDEFINE NIL NIL)
  ($EXPN)
  NIL
  (COND
   (($ATOM $EXPN) '((MLIST)))
   (T
    ((LAMBDA
      ($INFLAG $ANS) 
      NIL
      (PROG
       NIL
       (COND ((AND (LIKE (SIMPLIFY ($INPART $EXPN 0)) '&^)
                   ($INTEGERP (SIMPLIFY ($INPART $EXPN 2)))
                   (NOT (IS-BOOLE-CHECK (MLSP (TRD-MSYMEVAL $PIECE
                                                            '$PIECE)
                                              2))))
              (COND (($ATOM (SETQ $EXPN (SIMPLIFY ($INPART $EXPN 1))))
                     (RETURN '((MLIST))))
                    (($TRIGONOMETRICP $EXPN)
                     (RETURN (LIST '(MLIST) (LIST '(MLIST) $EXPN)))))))
       (SETQ $INFLAG T)
       (DO
        (($ARG) (MDO (CDR $EXPN) (CDR MDO)))
        ((NULL MDO) '$DONE)
        (SETQ $ARG (CAR MDO))
        (SETQ
         $ANS
         (SIMPLIFY ($SPECIALUNION (SIMPLIFY ($LISTOFTRIGSQ $ARG))
                                  (TRD-MSYMEVAL $ANS '$ANS)))))
       (RETURN (TRD-MSYMEVAL $ANS '$ANS))))
     '$INFLAG
     '((MLIST)))))))
(eval-when (compile eval load)
 (DEFPROP $SPECIALUNION T TRANSLATED)
 (ADD2LNC '$SPECIALUNION $PROPS)
 (DEFMTRFUN
  ($SPECIALUNION $ANY MDEFINE NIL NIL)
  ($LIST1 $LIST2)
  NIL
  (COND
   ((LIKE $LIST1 '((MLIST))) (TRD-MSYMEVAL $LIST2 '$LIST2))
   ((LIKE (TRD-MSYMEVAL $LIST2 '$LIST2) '((MLIST))) $LIST1)
   (T
    ((LAMBDA
      ($ALTERNATES)
      NIL
      (DO
       (($ALT) (MDO (CDR $ALTERNATES) (CDR MDO)))
       ((NULL MDO) '$DONE)
       (SETQ $ALT (CAR MDO))
       (SETQ
        $LIST2
        (SIMPLIFY ($UPDATE $ALT
                           (SIMPLIFY ($GET (SIMPLIFY ($INPART $ALT 0))
                                           '$COMPLEMENT_FUNCTION))))))
      (SIMPLIFY ($SPECIALUNION (SIMPLIFY ($REST $LIST1))
                               (TRD-MSYMEVAL $LIST2 '$LIST2))))
     (SIMPLIFY ($FIRST $LIST1)))))))
(eval-when (compile eval load) 
 (DEFPROP $UPDATE T TRANSLATED)
 (ADD2LNC '$UPDATE $PROPS)
 (DEFMTRFUN
  ($UPDATE $ANY MDEFINE NIL NIL)
  ($FORM $COMPLEMENT)
  NIL
  ((LAMBDA
    ($ANS)
     (declare (special $ans))
    NIL
    NIL
    (SETQ $COMPLEMENT (SIMPLIFY (MFUNCALL $COMPLEMENT
                                          (SIMPLIFY ($INPART $FORM 1)))))
    (SETQ
     $ANS
     (DO (($ELEMENT)
          (MDO (CDR (TRD-MSYMEVAL $LIST2 '$LIST2)) (CDR MDO)))
         ((NULL MDO) '$DONE)
       (SETQ $ELEMENT (CAR MDO))
       (COND (($MEMBER $FORM $ELEMENT)
              (RETURN '$FOUND))
             (($MEMBER $COMPLEMENT $ELEMENT)
              (RETURN ($CONS (LIST '(MLIST) $FORM $COMPLEMENT)
                             (SIMPLIFY ($DELETE $ELEMENT
                                                (TRD-MSYMEVAL $LIST2
                                                              '$LIST2)))))))))
    (COND ((LIKE (TRD-MSYMEVAL $ANS '$ANS) '$FOUND)
           (TRD-MSYMEVAL $LIST2 '$LIST2))
          ((LIKE (TRD-MSYMEVAL $ANS '$ANS) '$DONE)
           ($CONS (LIST '(MLIST) $FORM) (TRD-MSYMEVAL $LIST2 '$LIST2)))
          (T (TRD-MSYMEVAL $ANS '$ANS))))
   '$ANS)))
(eval-when (compile eval load) 
       (DEFPROP $EXPNLENGTH T TRANSLATED)
       (ADD2LNC '$EXPNLENGTH $PROPS)
       (DEFMTRFUN ($EXPNLENGTH $FIXNUM MDEFINE NIL NIL)
                  ($EXPR)
                  NIL
                  ((LAMBDA ($INFLAG)
                       NIL
                       (COND (($ATOM $EXPR) 1)
                             (T (f+ 1
                                   ($ARGSLENGTH (SIMPLIFY ($ARGS $EXPR)))))))
                   T)))
(eval-when (compile eval load) 
 (DEFPROP $ARGSLENGTH T TRANSLATED)
 (ADD2LNC '$ARGSLENGTH $PROPS)
 (DEFMTRFUN ($ARGSLENGTH $ANY MDEFINE NIL NIL)
            ($ARGS)
            NIL
            (SIMPLIFY (MAPPLY-TR '&+
                                 (SIMPLIFY (MAP1 (GETOPR '$EXPNLENGTH)
                                                 $ARGS))))))