;FILENAME: SETM >

;This file contains 2 packages for Set Theory -
;the first is for LISP and the second is for MACSYMA.


;The LISP Set Package:

;contents of the LISP Set Package:
;INTERSECT+ INTERSECT2
;UNION+ UNION2
;COMPLEMENT 
;SETDIFFERENCE
;SYMMDIFFERENCE
;POWERSET
;SETIFY 
;SUBSET SUBLIST 
;SETP 
;SETEQUALP 
;SUBSETP 
;DISJOINTP


;auxiliary functions: XEROX


(DECLARE #-NIL(MAPEX T)
	 (SPECIAL CANONLT $CANONLT SUPERCANONLT)
         #-NIL(*FEXPR ERRM1 ERRM2)
	 )


(DEFmacro XEROX (X)
   (LIST 'APPEND (CADR X) NIL))

;XEROX COPIES A LIST.


(DEFUN INTERSECT+ N
   (DO ((X (LISTIFY N) (DO ((Y X (CDDR Y))
                  (A NIL (CONS (INTERSECT2 (CAR Y) (CADR Y)) A)))
                 ((NULL Y) A))))
       ((NULL (CDR X)) (CAR X))))


(DEFUN INTERSECT2 (A B)
   (SETQ A (SETIFY A) B (SETIFY B))
   (PROG (U)
   1 (COND ((OR (NULL A) (NULL B))
            (RETURN (NREVERSE U)))
           ((FUNCALL CANONLT (CAR A) (CAR B))
            (SETQ A (CDR A)))
           ((FUNCALL CANONLT (CAR B) (CAR A))
            (SETQ B (CDR B)))
           (T (SETQ U (CONS (CAR A) U)
                    A (CDR A)
                    B (CDR B))))
     (GO 1)))



(DEFUN UNION+ N
   (DO ((X (LISTIFY N) (DO ((Y X (CDDR Y))
                  (A NIL (CONS (UNION2 (CAR Y) (CADR Y)) A)))
                 ((NULL Y) A))))
       ((NULL (CDR X)) (CAR X))))


(DEFUN UNION2 (A B)
   (SETQ A (SETIFY A) B (SETIFY B))
   (PROG (U)
   1  (COND ((NULL A) (RETURN (APPEND (NREVERSE U) B)))
            ((NULL B) (RETURN (APPEND (NREVERSE U) A)))
            ((FUNCALL CANONLT (CAR A) (CAR B))
             (SETQ U (CONS (CAR A) U)
                   A (CDR A)))
            ((FUNCALL CANONLT (CAR B) (CAR A))
             (SETQ U (CONS (CAR B) U)
                   B (CDR B)))
            (T (SETQ U (CONS (CAR A) U)
                     A (CDR A)
                     B (CDR B))))
      (GO 1)))


(DEFUN COMPLEMENT (A B)
   (SETQ A (SETIFY A) B (SETIFY B))
   (PROG (U)
   1  (COND ((NULL A) (RETURN (APPEND (NREVERSE U) B)))
            ((NULL B) (RETURN (NREVERSE U)))
            ((FUNCALL CANONLT (CAR A) (CAR B))
             (SETQ A (CDR A)))
            ((FUNCALL CANONLT (CAR B) (CAR A))
             (SETQ U (CONS (CAR B) U)
                   B (CDR B)))
            (T (SETQ A (CDR A) B (CDR B))))
      (GO 1)))


(DEFUN SETDIFFERENCE (A B)  (COMPLEMENT B A))


(DEFUN SYMMDIFFERENCE (A B)
   (UNION2 (COMPLEMENT A B) (COMPLEMENT B A)))



(DEFUN POWERSET (X)
   (SETQ X (SETIFY X))
   (DO ((X X (CDR X))
        (A (LIST NIL)))
       ((NULL X) (MAPCAR (FUNCTION REVERSE) (NREVERSE A)))
       (SETQ A 
          (APPEND 
             (DO ((A A (CDR A))
                  (Y (CAR X))
                  (B))
                 ((NULL A) (NREVERSE B))
                 (SETQ B (CONS (CONS Y (CAR A)) B)))
             A))))




(DEFUN SETIFY (X)
   (COND ((SETP X) X)
         (T (SETQ X (SORT (XEROX X) SUPERCANONLT))
            (DO ((X (CDR X) (CDR X))
                 (Y (NCONS (CAR X)))
                 (A (CAR X)))
                ((NULL X) (NREVERSE Y))
                (COND ((FUNCALL CANONLT A (CAR X))
                       (SETQ A (CAR X) Y (CONS A Y))))))))



(DEFUN SUBSET (A F) (SUBLIST (SETIFY A) F))

(DEFUN SUBLIST (A F)
   (DO ((A A (CDR A))
        (X))
       ((NULL A) (NREVERSE X))
       (COND ((FUNCALL F (CAR A))
              (SETQ X (CONS (CAR A) X))))))



(DEFUN SETP (L)
   (DO ((L (CDR L) (CDR L))
        (A (CAR L) (CAR L)))
       ((NULL L) T)
       (OR (FUNCALL CANONLT A (CAR L))
           (RETURN NIL))))



(DEFUN SETEQUALP (A B)
   (AND (= (LENGTH A) (LENGTH B))
        (DO ((A A (CDR A))
             (B B (CDR B)))
            ((NULL A) T)
            (COND ((OR (FUNCALL CANONLT (CAR A) (CAR B))
                       (FUNCALL CANONLT (CAR B) (CAR A)))
                   (RETURN NIL))))))


;SETEQUALP ASSUMES A AND B ARE SETS

(DEFUN SUBSETP (A B)
   (SETQ A (SETIFY A) B (SETIFY B))
   (SETEQUALP A (INTERSECT2 A B)))


(DEFUN DISJOINTP (A B)
   (NULL (INTERSECT2 A B)))


;The MACSYMA Set Package:

;contents of MACSYMA Set Package:
;$INTERSECT $UNION 
;$COMPLEMENT $SETDIFFERENCE $SYMMDIFFERENCE 
;$POWERSET
;$SETIFY $SUBSET 
;$SETP $SUBSETP $DISJOINTP 


;auxiliary functions:
;INLIST OUTLIST SUBLISTM CANONLT-SET




(DEFMACRO INLIST (X)
   (CONS 'CDR (CDR X)))


(DEFMACRO OUTLIST (X)
   (LIST 'CONS
         (LIST 'QUOTE
               (LIST 'MLIST 'SIMP))
         (CADR X)))



(DEFUN $INTERSECT N
   ((LAMBDA (X CANONLT SUPERCANONLT)
      (COND ((LLISTP X)
             (CANONLT-SET)
             (OUTLIST
                 (APPLY (FUNCTION INTERSECT+)
                        (MAPCAR (FUNCTION INLIST) X))))
            (T (ERRM2  NIL '|INTERSECT| '|lists|))))
    (LISTIFY N)
    $CANONLT
    $CANONLT))


(DEFUN $UNION N
   ((LAMBDA (X CANONLT SUPERCANONLT)
       (COND ((LLISTP X)
              (CANONLT-SET)
              (OUTLIST
                 (APPLY (FUNCTION UNION+)
                        (MAPCAR (FUNCTION INLIST) X))))
             (T (ERRM2 NIL '|UNION| '|lists|))))
    (LISTIFY N)
    $CANONLT
    $CANONLT))


(DEFUN $COMPLEMENT (A B)
   (COND ((AND ($LISTP A) ($LISTP B))
          (OUTLIST
             ((LAMBDA (CANONLT SUPERCANONLT)
                 (CANONLT-SET)
                 (COMPLEMENT (INLIST A) (INLIST B)))
              $CANONLT
              $CANONLT)))
         (T (ERRM2 NIL '|COMPLEMENT| '|lists|))))



(DEFUN $SETDIFFERENCE (A B)
   (COND ((AND ($LISTP A) ($LISTP B))
          (OUTLIST
             ((LAMBDA (CANONLT SUPERCANONLT)
                 (CANONLT-SET)
                 (COMPLEMENT (INLIST B) (INLIST A)))
              $CANONLT
              $CANONLT)))
         (T (ERRM2 NIL '|SETDIFFERENCE| '|lists|))))



(DEFUN $SYMMDIFFERENCE (A B)
   (COND ((AND ($LISTP A) ($LISTP B))
          (OUTLIST
             ((LAMBDA (CANONLT SUPERCANONLT)
                 (CANONLT-SET)
                 (SYMMDIFFERENCE (INLIST A) (INLIST B)))
              $CANONLT
              $CANONLT)))
         (T (ERRM2 NIL '|SYMMDIFFERENCE| '|lists|))))




(DEFUN $POWERSET (X)
   (COND (($LISTP X)
          ((LAMBDA (CANONLT SUPERCANONLT)
              (CANONLT-SET)
              (OUTLIST 
                 (MAPCAR (FUNCTION OUTLIST)
                         (POWERSET (INLIST X)))))
           $CANONLT
           $CANONLT))
         (T (ERRM1 NIL '|POWERSET| '|a list|))))
           



(DEFUN $SETIFY (X)
   (COND (($LISTP X)
          ((LAMBDA (CANONLT SUPERCANONLT)
              (CANONLT-SET)
              (OUTLIST (SETIFY (INLIST X))))
           $CANONLT
           $CANONLT))
         (T (ERRM1 NIL '|SETIFY| '|a list|))))



(DEFUN $SUBSET (A F)
   (COND (($LISTP A)
          (OUTLIST 
             ((LAMBDA (CANONLT SUPERCANONLT)
                 (CANONLT-SET)
                 (SUBLISTM (INLIST A) F))
              $CANONLT
              $CANONLT)))
         (T (ERRM1 '|first| '|SUBSET| '|a list|))))





(DEFUN $SETP (X)
   (COND (($LISTP X)
          ((LAMBDA (CANONLT SUPERCANONLT)
              (CANONLT-SET)
              (SETP (INLIST X)))
           $CANONLT
           $CANONLT))
         (T (ERRM1 NIL '|SETP| '|a list|))))



(DEFUN $SUBSETP (A B)
   (COND ((AND ($LISTP A) ($LISTP B))
          ((LAMBDA (CANONLT SUPERCANONLT)
              (CANONLT-SET)
              (SUBSETP (INLIST A) (INLIST B)))
           $CANONLT
           $CANONLT))
         (T (ERRM2 NIL '|SUBSETP| '|lists|))))



(DEFUN $DISJOINTP (A B)
   (COND ((AND ($LISTP A) ($LISTP B))
          ((LAMBDA (CANONLT SUPERCANONLT)
              (CANONLT-SET)
              (DISJOINTP (INLIST A) (INLIST B)))
           $CANONLT
           $CANONLT))
         (T (ERRM2 NIL '|DISJOINTP| '|lists|))))


(DEFUN LLISTP (L)
   (DO ((L L (CDR L)))
       ((NULL L) T)
       (OR ($LISTP (CAR L)) (RETURN NIL))))




(DEFUN SUBLISTM (A F)
   (DO ((A A (CDR A))
        (X))
       ((NULL A) (NREVERSE X))
       (COND ((MEVALP (LIST (NCONS F) (CAR A)))
              (SETQ X (CONS (CAR A) X))))))




(DEFUN CANONLT-SET NIL
   (OR (EQ $CANONLT '$ORDERLESSP)
       (SETQ CANONLT '(LAMBDA (X Y) (MCALL $CANONLT X Y))
             SUPERCANONLT '(LAMBDA (X Y)
                              (COND ((MCALL $CANONLT X Y) T)
                                    ((MCALL $CANONLT Y X) NIL)
                                    (T ($ORDERLESSP X Y)))))))



#-NIL(DECLARE (READ) (READ))

(DEFUN INLIST (X) (CDR X))

(DEFUN OUTLIST (X) (CONS '(MLIST SIMP) X))


(SETQ $CANONLT '$ORDERLESSP 
      CANONLT '$ORDERLESSP
      SUPERCANONLT '$ORDERLESSP)

(DEFUN ERRM0 (&rest X)
   (PRINC '|Incorrect argument syntax to function |)
   (PRINC (CAR X))
   (PRINC '|.|)
   (ERR))


(DEFUN ERRM1 (&rest X)
   (PRINC '|The |)
   (COND ((CAR X) 
          (PRINC (CAR X)) 
          (PRINC '| |)))
   (PRINC '|argument to |)
   (PRINC (CADR X))
   (PRINC '| must be |)
   (PRINC (CADDR X))
   (PRINC '|.|)
   (ERR))

(DEFUN ERRM2 (&rest X)
   (PRINC '|The |)
   (COND  ((CAR X) 
           (PRINC (CAR X)) 
           (PRINC '| |)))
   (PRINC '|arguments to |)
   (PRINC (CADR X))
   (PRINC '| must be |)
   (PRINC (CADDR X))
   (PRINC '|.|)
   (ERR))
