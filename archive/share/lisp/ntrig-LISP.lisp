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
  'COMPILE
  ((PROGN
     'COMPILE
     (PROGN
       'COMPILE
       (MEVAL* '(($MODEDECLARE) $N $ANY))
       (MEVAL* '(($DECLARE) $N $SPECIAL))
       NIL
       (DEF-MTRVAR $N '$N))
     (PROGN
       'COMPILE
       (ADD2LNC '$N $PROPS)
       (MDEFPROP $N ($INTEGERP) MATCHDECLARE)))))
(PROGN
  'COMPILE
  (MDEFPROP $SINRULE1 %SIN RULEOF)
  (ADD2LNC '$SINRULE1 $RULES)
  (MDEFPROP %SIN 2 RULENUM)
  (DEFUN $SINRULE1 (X ANS A3)
    (SETQ X (SIMP-%SIN X ANS A3))
    (COND
      ((TRD-MSYMEVAL *AFTERFLAG '*AFTERFLAG) X)
      (T (PROG (|tr-gensym~0| *AFTERFLAG)
           DECLARE
           (SPECIAL |tr-gensym~0| *AFTERFLAG)
           (SETQ *AFTERFLAG T)
           (COND ((OR (ATOM X) (NOT (EQ (CAAR X) '%SIN))) (RETURN X)))
           (SETQ |tr-gensym~0| (CDR X))
           (SETQ ANS
                 (CATCH 'MATCH
                   (PROG ($N |tr-gensym~1|)
                     (DECLARE (SPECIAL $N |tr-gensym~1|))
                     (SETQ |tr-gensym~1| (KAR |tr-gensym~0|))
                     (SETQ |tr-gensym~1|
                           (DIV |tr-gensym~1| '((RAT SIMP) 1 10)))
                     (SETQ |tr-gensym~1| (DIV |tr-gensym~1| '$%PI))
                     (COND
                       (($INTEGERP |tr-gensym~1|)
                        (SETQ $N |tr-gensym~1|))
                       ((MATCHERR)))
                     (COND ((NTHKDR |tr-gensym~0| 1) (MATCHERR)))
                     (RETURN
                       (SIMPLIFY
                           (MFUNCTION-CALL $USIN (TRD-MSYMEVAL $N '$N)))))))
           (RETURN (OR ANS (EQTEST X X)))))))
  (MDEFPROP $SINRULE1
      ((MEQUAL) ((%SIN SIMP) ((MTIMES SIMP) ((RAT SIMP) 1 10) $%PI $N))
       (($USIN SIMP) $N))
      $RULE)
  (MDEFPROP %SIN (SIMP-%SIN) OLDRULES)
  (DEFPROP %SIN $SINRULE1 OPERATORS)
  (MDEFPROP %SIN ($SINRULE1 SIMP-%SIN) OLDRULES))
(PROGN
  'COMPILE
  (MDEFPROP $COSRULE1 %COS RULEOF)
  (ADD2LNC '$COSRULE1 $RULES)
  (MDEFPROP %COS 2 RULENUM)
  (DEFUN $COSRULE1 (X ANS A3)
    (SETQ X (SIMP-%COS X ANS A3))
    (COND
      ((TRD-MSYMEVAL *AFTERFLAG '*AFTERFLAG) X)
      (T (PROG (|tr-gensym~2| *AFTERFLAG)
           DECLARE
           (SPECIAL |tr-gensym~2| *AFTERFLAG)
           (SETQ *AFTERFLAG T)
           (COND ((OR (ATOM X) (NOT (EQ (CAAR X) '%COS))) (RETURN X)))
           (SETQ |tr-gensym~2| (CDR X))
           (SETQ ANS
                 (CATCH 'MATCH
                   (PROG ($N |tr-gensym~3|)
                     (DECLARE (SPECIAL $N |tr-gensym~3|))
                     (SETQ |tr-gensym~3| (KAR |tr-gensym~2|))
                     (SETQ |tr-gensym~3|
                           (DIV |tr-gensym~3| '((RAT SIMP) 1 10)))
                     (SETQ |tr-gensym~3| (DIV |tr-gensym~3| '$%PI))
                     (COND
                       (($INTEGERP |tr-gensym~3|)
                        (SETQ $N |tr-gensym~3|))
                       ((MATCHERR)))
                     (COND ((NTHKDR |tr-gensym~2| 1) (MATCHERR)))
                     (RETURN
                       (SIMPLIFY
                           (MFUNCTION-CALL $UCOS (TRD-MSYMEVAL $N '$N)))))))
           (RETURN (OR ANS (EQTEST X X)))))))
  (MDEFPROP $COSRULE1
      ((MEQUAL) ((%COS SIMP) ((MTIMES SIMP) ((RAT SIMP) 1 10) $%PI $N))
       (($UCOS SIMP) $N))
      $RULE)
  (MDEFPROP %COS (SIMP-%COS) OLDRULES)
  (DEFPROP %COS $COSRULE1 OPERATORS)
  (MDEFPROP %COS ($COSRULE1 SIMP-%COS) OLDRULES))
(PROGN
  'COMPILE
  (MDEFPROP $TANRULE1 %TAN RULEOF)
  (ADD2LNC '$TANRULE1 $RULES)
  (MDEFPROP %TAN 2 RULENUM)
  (DEFUN $TANRULE1 (X ANS A3)
    (SETQ X (SIMP-%TAN X ANS A3))
    (COND
      ((TRD-MSYMEVAL *AFTERFLAG '*AFTERFLAG) X)
      (T (PROG (|tr-gensym~4| *AFTERFLAG)
           DECLARE
           (SPECIAL |tr-gensym~4| *AFTERFLAG)
           (SETQ *AFTERFLAG T)
           (COND ((OR (ATOM X) (NOT (EQ (CAAR X) '%TAN))) (RETURN X)))
           (SETQ |tr-gensym~4| (CDR X))
           (SETQ ANS
                 (CATCH 'MATCH
                   (PROG ($N |tr-gensym~5|)
                     (DECLARE (SPECIAL $N |tr-gensym~5|))
                     (SETQ |tr-gensym~5| (KAR |tr-gensym~4|))
                     (SETQ |tr-gensym~5|
                           (DIV |tr-gensym~5| '((RAT SIMP) 1 10)))
                     (SETQ |tr-gensym~5| (DIV |tr-gensym~5| '$%PI))
                     (COND
                       (($INTEGERP |tr-gensym~5|)
                        (SETQ $N |tr-gensym~5|))
                       ((MATCHERR)))
                     (COND ((NTHKDR |tr-gensym~4| 1) (MATCHERR)))
                     (RETURN
                       (MUL* (POWER (SIMPLIFY
                                     (MFUNCTION-CALL $UCOS
                                      (TRD-MSYMEVAL $N '$N)))
                                    -1)
                             (SIMPLIFY
                                 (MFUNCTION-CALL $USIN
                                     (TRD-MSYMEVAL $N '$N))))))))
           (RETURN (OR ANS (EQTEST X X)))))))
  (MDEFPROP $TANRULE1
      ((MEQUAL) ((%TAN SIMP) ((MTIMES SIMP) ((RAT SIMP) 1 10) $%PI $N))
       ((MTIMES SIMP) ((MEXPT SIMP) (($UCOS SIMP) $N) -1)
        (($USIN SIMP) $N)))
      $RULE)
  (MDEFPROP %TAN (SIMP-%TAN) OLDRULES)
  (DEFPROP %TAN $TANRULE1 OPERATORS)
  (MDEFPROP %TAN ($TANRULE1 SIMP-%TAN) OLDRULES))
(PROGN
  'COMPILE
  (MDEFPROP $COTRULE1 %COT RULEOF)
  (ADD2LNC '$COTRULE1 $RULES)
  (MDEFPROP %COT 2 RULENUM)
  (DEFUN $COTRULE1 (X ANS A3)
    (SETQ X (SIMP-%COT X ANS A3))
    (COND
      ((TRD-MSYMEVAL *AFTERFLAG '*AFTERFLAG) X)
      (T (PROG (|tr-gensym~6| *AFTERFLAG)
           DECLARE
           (SPECIAL |tr-gensym~6| *AFTERFLAG)
           (SETQ *AFTERFLAG T)
           (COND ((OR (ATOM X) (NOT (EQ (CAAR X) '%COT))) (RETURN X)))
           (SETQ |tr-gensym~6| (CDR X))
           (SETQ ANS
                 (CATCH 'MATCH
                   (PROG ($N |tr-gensym~7|)
                     (DECLARE (SPECIAL $N |tr-gensym~7|))
                     (SETQ |tr-gensym~7| (KAR |tr-gensym~6|))
                     (SETQ |tr-gensym~7|
                           (DIV |tr-gensym~7| '((RAT SIMP) 1 10)))
                     (SETQ |tr-gensym~7| (DIV |tr-gensym~7| '$%PI))
                     (COND
                       (($INTEGERP |tr-gensym~7|)
                        (SETQ $N |tr-gensym~7|))
                       ((MATCHERR)))
                     (COND ((NTHKDR |tr-gensym~6| 1) (MATCHERR)))
                     (RETURN
                       (MUL* (SIMPLIFY
                                 (MFUNCTION-CALL $UCOS
                                     (TRD-MSYMEVAL $N '$N)))
                             (POWER (SIMPLIFY
                                     (MFUNCTION-CALL $USIN
                                      (TRD-MSYMEVAL $N '$N)))
                                    -1))))))
           (RETURN (OR ANS (EQTEST X X)))))))
  (MDEFPROP $COTRULE1
      ((MEQUAL) ((%COT SIMP) ((MTIMES SIMP) ((RAT SIMP) 1 10) $%PI $N))
       ((MTIMES SIMP) (($UCOS SIMP) $N)
        ((MEXPT SIMP) (($USIN SIMP) $N) -1)))
      $RULE)
  (MDEFPROP %COT (SIMP-%COT) OLDRULES)
  (DEFPROP %COT $COTRULE1 OPERATORS)
  (MDEFPROP %COT ($COTRULE1 SIMP-%COT) OLDRULES))
(PROGN
  'COMPILE
  (MDEFPROP $SECRULE1 %SEC RULEOF)
  (ADD2LNC '$SECRULE1 $RULES)
  (MDEFPROP %SEC 2 RULENUM)
  (DEFUN $SECRULE1 (X ANS A3)
    (SETQ X (SIMP-%SEC X ANS A3))
    (COND
      ((TRD-MSYMEVAL *AFTERFLAG '*AFTERFLAG) X)
      (T (PROG (|tr-gensym~8| *AFTERFLAG)
           DECLARE
           (SPECIAL |tr-gensym~8| *AFTERFLAG)
           (SETQ *AFTERFLAG T)
           (COND ((OR (ATOM X) (NOT (EQ (CAAR X) '%SEC))) (RETURN X)))
           (SETQ |tr-gensym~8| (CDR X))
           (SETQ ANS
                 (CATCH 'MATCH
                   (PROG ($N |tr-gensym~9|)
                     (DECLARE (SPECIAL $N |tr-gensym~9|))
                     (SETQ |tr-gensym~9| (KAR |tr-gensym~8|))
                     (SETQ |tr-gensym~9|
                           (DIV |tr-gensym~9| '((RAT SIMP) 1 10)))
                     (SETQ |tr-gensym~9| (DIV |tr-gensym~9| '$%PI))
                     (COND
                       (($INTEGERP |tr-gensym~9|)
                        (SETQ $N |tr-gensym~9|))
                       ((MATCHERR)))
                     (COND ((NTHKDR |tr-gensym~8| 1) (MATCHERR)))
                     (RETURN
                       (POWER (SIMPLIFY
                                  (MFUNCTION-CALL $UCOS
                                      (TRD-MSYMEVAL $N '$N)))
                              -1)))))
           (RETURN (OR ANS (EQTEST X X)))))))
  (MDEFPROP $SECRULE1
      ((MEQUAL) ((%SEC SIMP) ((MTIMES SIMP) ((RAT SIMP) 1 10) $%PI $N))
       ((MEXPT SIMP) (($UCOS SIMP) $N) -1))
      $RULE)
  (MDEFPROP %SEC (SIMP-%SEC) OLDRULES)
  (DEFPROP %SEC $SECRULE1 OPERATORS)
  (MDEFPROP %SEC ($SECRULE1 SIMP-%SEC) OLDRULES))
(PROGN
  'COMPILE
  (MDEFPROP $CSCRULE1 %CSC RULEOF)
  (ADD2LNC '$CSCRULE1 $RULES)
  (MDEFPROP %CSC 2 RULENUM)
  (DEFUN $CSCRULE1 (X ANS A3)
    (SETQ X (SIMP-%CSC X ANS A3))
    (COND
      ((TRD-MSYMEVAL *AFTERFLAG '*AFTERFLAG) X)
      (T (PROG (|tr-gensym~10| *AFTERFLAG)
           DECLARE
           (SPECIAL |tr-gensym~10| *AFTERFLAG)
           (SETQ *AFTERFLAG T)
           (COND ((OR (ATOM X) (NOT (EQ (CAAR X) '%CSC))) (RETURN X)))
           (SETQ |tr-gensym~10| (CDR X))
           (SETQ ANS
                 (CATCH 'MATCH
                   (PROG ($N |tr-gensym~11|)
                     (DECLARE (SPECIAL $N |tr-gensym~11|))
                     (SETQ |tr-gensym~11| (KAR |tr-gensym~10|))
                     (SETQ |tr-gensym~11|
                           (DIV |tr-gensym~11| '((RAT SIMP) 1 10)))
                     (SETQ |tr-gensym~11| (DIV |tr-gensym~11| '$%PI))
                     (COND
                       (($INTEGERP |tr-gensym~11|)
                        (SETQ $N |tr-gensym~11|))
                       ((MATCHERR)))
                     (COND ((NTHKDR |tr-gensym~10| 1) (MATCHERR)))
                     (RETURN
                       (POWER (SIMPLIFY
                                  (MFUNCTION-CALL $USIN
                                      (TRD-MSYMEVAL $N '$N)))
                              -1)))))
           (RETURN (OR ANS (EQTEST X X)))))))
  (MDEFPROP $CSCRULE1
      ((MEQUAL) ((%CSC SIMP) ((MTIMES SIMP) ((RAT SIMP) 1 10) $%PI $N))
       ((MEXPT SIMP) (($USIN SIMP) $N) -1))
      $RULE)
  (MDEFPROP %CSC (SIMP-%CSC) OLDRULES)
  (DEFPROP %CSC $CSCRULE1 OPERATORS)
  (MDEFPROP %CSC ($CSCRULE1 SIMP-%CSC) OLDRULES))
(PROGN
  'COMPILE
  (DEFPROP $USIN T TRANSLATED)
  (ADD2LNC '$USIN $PROPS)
  (DEFMTRFUN ($USIN $ANY MDEFINE NIL NIL) ($N) NIL
      ((LAMBDA ($YUK)
         NIL
         (COND
           ((OR (LIKE $YUK 1) (LIKE $YUK 9))
            (DIV (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1) 4))
           ((OR (LIKE $YUK 2) (LIKE $YUK 8))
            (DIV (MUL* (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1)
                       (SIMPLIFY
                           (LIST '(%SQRT)
                                 (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5))))
                 (MUL* 4 (SIMPLIFY (LIST '(%SQRT) 2)))))
           ((OR (LIKE $YUK 3) (LIKE $YUK 7))
            (DIV (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 1) 4))
           ((OR (LIKE $YUK 4) (LIKE $YUK 6))
            (DIV (SIMPLIFY
                     (LIST '(%SQRT)
                           (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5)))
                 (MUL* 2 (SIMPLIFY (LIST '(%SQRT) 2)))))))
       (SIMPLIFY
           (LIST '(MABS)
                 (SIMPLIFY ($REMAINDER (TRD-MSYMEVAL $N '$N) 10)))))))
(PROGN
  'COMPILE
  (DEFPROP $UCOS T TRANSLATED)
  (ADD2LNC '$UCOS $PROPS)
  (DEFMTRFUN ($UCOS $ANY MDEFINE NIL NIL) ($N) NIL
      ((LAMBDA ($YUK)
         NIL
         (COND
           ((LIKE $YUK 1)
            (DIV (SIMPLIFY
                     (LIST '(%SQRT)
                           (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5)))
                 (MUL* 2 (SIMPLIFY (LIST '(%SQRT) 2)))))
           ((LIKE $YUK 2)
            (DIV (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 1) 4))
           ((LIKE $YUK 3)
            (DIV (MUL* (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1)
                       (SIMPLIFY
                           (LIST '(%SQRT)
                                 (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5))))
                 (MUL* 4 (SIMPLIFY (LIST '(%SQRT) 2)))))
           ((LIKE $YUK 4)
            (DIV (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1) 4))
           ((LIKE $YUK 6)
            (DIV (*MMINUS (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1)) 4))
           ((LIKE $YUK 7)
            (DIV (MUL* (*MMINUS (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) -1))
                       (SIMPLIFY
                           (LIST '(%SQRT)
                                 (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 5))))
                 (MUL* 4 (SIMPLIFY (LIST '(%SQRT) 2)))))
           ((LIKE $YUK 8)
            (DIV (*MMINUS (ADD* (SIMPLIFY (LIST '(%SQRT) 5)) 1)) 4))
           ((LIKE $YUK 9)
            (DIV (*MMINUS (SIMPLIFY
                              (LIST '(%SQRT)
                                    (ADD* (SIMPLIFY (LIST '(%SQRT) 5))
                                     5))))
                 (MUL* 2 (SIMPLIFY (LIST '(%SQRT) 2)))))))
       (SIMPLIFY
           (LIST '(MABS)
                 (SIMPLIFY ($REMAINDER (TRD-MSYMEVAL $N '$N) 10)))))))