;;; -*- Mode: Lisp; Package: Macsyma -*-
;;; Translated code for LOCAL::MAX$DISK:[SHAREM]DEFSTM.MC;49
;;; Written on 9/12/1984 12:15:45, from MACSYMA 302
;;; Translated for 176228

;;; TRANSL-AUTOLOAD version NIL
;;; TRANSS version 87 TRANSL version 1157 TRUTIL version 27
;;; TRANS1 version 108 TRANS2 version 39 TRANS3 version 50
;;; TRANS4 version 29 TRANS5 version 26 TRANSF version NIL
;;; TROPER version 15 TRPRED version 6 MTAGS version NIL
;;; MDEFUN version 58 TRANSQ version 88 FCALL version 40
;;; ACALL version 70 TRDATA version 68 MCOMPI version 146
;;; TRMODE version 75 TRHOOK version NIL
(eval-when (compile eval)
      (setq *infile-name-key*
	          (namestring (truename '#.standard-input))))

(eval-when (compile)
   (setq $tr_semicompile 'NIL)
   (setq forms-to-compile-queue ()))

(comment "MAX$DISK:[SHAREM]DEFSTM.MC;49")

;;; General declarations required for translated MACSYMA code.

(DECLARE (SPECIAL $%%EXISTING_STRUCTURES%% $LOADPRINT))

(DEF-MTRVAR $LOADPRINT '$LOADPRINT 1)

(DEFMTRFUN-EXTERNAL ($ASSESS_MODE $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($NAME_OF_SLOT_ID $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($EQUAL_OP $BOOLEAN MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($SLOT_TYPE $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($OBTAIN_DEFAULT_VALUE_FOR_MODE $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($%AUX_ALTERANT% $ANY MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($%AUX_CONSTRUCTOR% $ANY MDEFINE NIL NIL))

(DECLARE (SPECIAL $%%EXISTING_STRUCTURES%%))

(DEFMTRFUN-EXTERNAL ($INITIALIZE_STRUCTURE_LIST $LIST MDEFINE NIL NIL))

(DEFMTRFUN-EXTERNAL ($SAVE_RUNTIME_STRUCTURE_INFO $ANY MDEFINE NIL NIL))


(COND ((IS-BOOLE-CHECK (TRD-MSYMEVAL $LOADPRINT '$LOADPRINT))
	 (SIMPLIFY (MFUNCTION-CALL $PRINT '$DEFSTM '|&source| '49))))

(SIMPLIFY (MFUNCTION-CALL $PUT '$DEFSTM '49 '$VERSION))

(MEVAL* '(($MATCHFIX) &{ &}))

(DEFPROP $ASSESS_MODE T TRANSLATED)

(ADD2LNC '$ASSESS_MODE $PROPS)

(DEFMTRFUN
 ($ASSESS_MODE $ANY MDEFINE NIL NIL) ($X) NIL
 (COND
  ((NOT (MFUNCTION-CALL $SYMBOLP $X))
   (COND
    ((AND
       (LIKE (SIMPLIFY (MFUNCTION-CALL $PART $X 0)) '$MODE)
       (= (MFUNCTION-CALL $LENGTH $X) 2)
       (MFUNCTION-CALL $SYMBOLP (SIMPLIFY (MFUNCTION-CALL $PART $X 1)))
       (MFUNCTION-CALL
	 $SYMBOLP (SIMPLIFY (MFUNCTION-CALL $PART $X 2))))
       (SIMPLIFY (MFUNCTION-CALL $PART $X 2)))
    (T
     (SIMPLIFY
      (MFUNCTION-CALL
       $ERROR
       '|&If slot name identifier is not a symbol, it must be of the following form:
MODE(TYPE,SLOT_NAME)[= DEFAULT VALUE] , not|
       $X)))))))

(DEFPROP $NAME_OF_SLOT_ID T TRANSLATED)

(ADD2LNC '$NAME_OF_SLOT_ID $PROPS)

(DEFMTRFUN ($NAME_OF_SLOT_ID $ANY MDEFINE NIL NIL) ($X) NIL
	   (COND ((MFUNCTION-CALL $SYMBOLP $X) $X)
		 (T (SIMPLIFY (MFUNCTION-CALL $PART $X 1)))))

(MEVAL* '(($MODEDECLARE) (($FUNCTION) $EQUAL_OP) $BOOLEAN))

(DEFPROP $EQUAL_OP T TRANSLATED)

(ADD2LNC '$EQUAL_OP $PROPS)

(DEFMTRFUN ($EQUAL_OP $BOOLEAN MDEFINE NIL NIL) ($X) NIL
	   (COND ((AND (NOT (MFUNCTION-CALL $ATOM $X))
		       (LIKE (SIMPLIFY (MFUNCTION-CALL $PART $X 0)) '&=))
		    T)))

(DEFPROP $SLOT_TYPE T TRANSLATED)

(ADD2LNC '$SLOT_TYPE $PROPS)

(DEFMTRFUN
  ($SLOT_TYPE $ANY MDEFINE NIL NIL) ($TYPES $INDEX) NIL
  (COND
    ((MFUNCTION-CALL $LISTP $TYPES) (MARRAYREF $TYPES $INDEX)) (T $TYPES)))

(DEFPROP $OBTAIN_DEFAULT_VALUE_FOR_MODE T TRANSLATED)

(ADD2LNC '$OBTAIN_DEFAULT_VALUE_FOR_MODE $PROPS)

(DEFMTRFUN
  ($OBTAIN_DEFAULT_VALUE_FOR_MODE $ANY MDEFINE NIL NIL) ($MODE) NIL
  (COND
    ((MFUNCTION-CALL $MEMBER $MODE '((MLIST SIMP) $FIXNUM $RATIONAL)) 0)
    (T
      (COND
	((LIKE $MODE '$BOOLEAN) NIL)
	(T (COND
	     ((MFUNCTION-CALL $MEMBER $MODE '((MLIST SIMP) $FLOAT $NUMBER))
		0.0D+0)
	     (T (COND ((LIKE $MODE '$LIST) '((MLIST)))
		      (T '((MQUOTE) $%UNDEFINED%))))))))))

(DEFPROP $%AUX_ALTERANT% T TRANSLATED)

(ADD2LNC '$%AUX_ALTERANT% $PROPS)

(DEFMTRFUN
 ($%AUX_ALTERANT% $ANY MDEFINE NIL NIL)
 ($ALT $EXTEND_NAME $SLOT_NAMES $QUAN $MODE_TYPE $OBJ $ARGS)
 (DECLARE (FIXNUM $QUAN))
 (PROGN
  NIL
  ((LAMBDA ($RESULT)
    NIL
    NIL
    (DO (($ELE) (MDO (CDR $ARGS) (CDR MDO))) ((NULL MDO) '$DONE)
     (SETQ $ELE (CAR MDO))
     ((LAMBDA ($NOM)
	NIL
	(COND
	  ((NOT (MFUNCTION-CALL $EQUAL_OP $ELE))
	     (SIMPLIFY
	       (MFUNCTION-CALL
		 $ERROR '|&Alterant argument must specify a value:|
		 $ELE)))
	  (T
	    (COND
	      ((NOT (MFUNCTION-CALL
		      $MEMBER
		      (SETQ $NOM (SIMPLIFY (MFUNCTION-CALL $LHS $ELE)))
		      $SLOT_NAMES))
		 (SIMPLIFY
		   (MFUNCTION-CALL $ERROR '|&Incorrect slot specifier to|
				   $ALT '|&:| $NOM)))
	      (T
		(DO (($I 1 (+ 1 $I))) ((> $I $QUAN) '$DONE)
		  (COND
		    ((LIKE (MARRAYREF $SLOT_NAMES $I) $NOM)
		       (RETURN
			 (SETQ
			   $RESULT
			   (SIMPLIFY
			     (MFUNCTION-CALL
			       $ENDCONS
			       (MBUILDQ-SUBST
				 (LIST
				   (CONS '$I $I)
				   (CONS
				     '$VAL
				     (SIMPLIFY (MFUNCTION-CALL $RHS $ELE)))
				   (CONS '$TYPE
					 (SIMPLIFY
					   (MFUNCTION-CALL
					     $SLOT_TYPE $MODE_TYPE $I))))
				 '(($EXTEND_SET) $TEMP $I
				   (($MODE_IDENTITY) $TYPE $VAL)))
			       $RESULT))))))))))))
      '$NOM))
    (COND ((= (MFUNCTION-CALL $LENGTH $RESULT) 0) NIL)
	  (T (MBUILDQ-SUBST
	       (LIST (CONS '$RESULT $RESULT) (CONS '$OBJ $OBJ)
		     (CONS '$ALT $ALT) (CONS '$EXTEND_NAME $EXTEND_NAME))
	       '((MPROG) ((MLIST) ((MSETQ) $TEMP $OBJ))
		 (($ALTER_EXTEND_CHECK) $TEMP
		  ((MQUOTE) $ALT) ((MQUOTE) $EXTEND_NAME))
		 (($SPLICE) $RESULT)
		 (($MODE_IDENTITY) $EXTEND_NAME $TEMP))))))
   '((MLIST)))))

(DEFPROP $%AUX_CONSTRUCTOR% T TRANSLATED)

(ADD2LNC '$%AUX_CONSTRUCTOR% $PROPS)

(DEFMTRFUN
 ($%AUX_CONSTRUCTOR% $ANY MDEFINE NIL NIL)
 ($CONSTRUCT $SLOT_NAMES $DEFAULTS $QUAN $MODE_TYPE $NAME $ARGS)
 (DECLARE (FIXNUM $QUAN))
 (PROGN
  NIL
  ((LAMBDA ($INITS)
    NIL
    NIL
    (DO (($ELE) (MDO (CDR $ARGS) (CDR MDO))) ((NULL MDO) '$DONE)
     (SETQ $ELE (CAR MDO))
     ((LAMBDA ($NOM)
	NIL
	(COND
	  ((NOT (MFUNCTION-CALL $EQUAL_OP $ELE))
	     (SIMPLIFY
	       (MFUNCTION-CALL
		 $ERROR '|&Constructor argument must specify a value:|
		 $ELE)))
	  (T
	    (COND
	      ((NOT (MFUNCTION-CALL
		      $MEMBER
		      (SETQ $NOM (SIMPLIFY (MFUNCTION-CALL $LHS $ELE)))
		      $SLOT_NAMES))
		 (SIMPLIFY
		   (MFUNCTION-CALL $ERROR '|&Incorrect slot specifier to|
				   $CONSTRUCT '|&:| $NOM)))
	      (T
		(DO (($I 1 (+ 1 $I))) ((> $I $QUAN) '$DONE)
		  (COND
		    ((LIKE (MARRAYREF $SLOT_NAMES $I) $NOM)
		       (RETURN
			 (MARRAYSET
			   (MBUILDQ-SUBST
			     (LIST
			       (CONS '$VAL
				     (SIMPLIFY (MFUNCTION-CALL $RHS $ELE)))
			       (CONS
				 '$TYPE
				 (SIMPLIFY (MFUNCTION-CALL
					     $SLOT_TYPE $MODE_TYPE $I))))
			     '(($MODE_IDENTITY) $TYPE $VAL))
			   $INITS $I))))))))))
      '$NOM))
    (MBUILDQ-SUBST
      (LIST (CONS '$INITS $INITS) (CONS '$NAME $NAME))
      '(($MODE_IDENTITY) $NAME
	(($MAKE_EXTEND) ((MQUOTE) $NAME) (($SPLICE) $INITS)))))
   (SIMPLIFY (MFUNCTION-CALL COPY-TREE $DEFAULTS)))))

(MEVAL* '(($MODEDECLARE) $%%EXISTING_STRUCTURES%% $LIST))

(MEVAL* '(($DECLARE) $%%EXISTING_STRUCTURES%% $SPECIAL))

(DEFPROP $%%EXISTING_STRUCTURES%% ASSIGN-MODE-CHECK ASSIGN)

(DEF-MTRVAR $%%EXISTING_STRUCTURES%% '((MLIST)))

(DEFPROP $DEF_STRUCTURE T TRANSLATED)

(ADD2LNC '$DEF_STRUCTURE $PROPS)

(DEFMTRFUN
 ($DEF_STRUCTURE $ANY MDEFMACRO T NIL) ($NAME $OPTIONS $SLOTS) NIL
 (PROGN
  NIL
  ((LAMBDA ($CONSTRUCT $ALT $MODE_TYPE $CONC $INCLUDE $INCLUDED_VALUES
		       $FIRST $QUAN $SLOT_NUM $DEFAULT_VALUE $INC_MODES)
    (DECLARE (FIXNUM $SLOT_NUM $QUAN))
    NIL
    (COND
      ((NOT (MFUNCTION-CALL $SYMBOLP $NAME))
	 (SIMPLIFY
	   (MFUNCTION-CALL
	     $ERROR '|&First argument to DEF_STRUCTURE must be a name|
	     $NAME))))
    (SETQ $QUAN (MFUNCTION-CALL $LENGTH $SLOTS))
    (DO (($OPTION) (MDO (CDR $OPTIONS) (CDR MDO))) ((NULL MDO) '$DONE)
      (SETQ $OPTION (CAR MDO))
      (COND
	((AND (NOT (MFUNCTION-CALL $ATOM $OPTION))
	      (LIKE (SIMPLIFY (MFUNCTION-CALL $LHS $OPTION)) '$MODE))
	   ((LAMBDA ($VALUE)
	      NIL
	      (SETQ $MODE_TYPE $VALUE)
	      (SETQ
		$DEFAULT_VALUE
		(SIMPLIFY (MFUNCTION-CALL
			    $OBTAIN_DEFAULT_VALUE_FOR_MODE $MODE_TYPE))))
	    (SIMPLIFY (MFUNCTION-CALL $RHS $OPTION))))))
    (DO (($OPTION) (MDO (CDR $OPTIONS) (CDR MDO))) ((NULL MDO) '$DONE)
     (SETQ $OPTION (CAR MDO))
     (COND
      ((MFUNCTION-CALL $ATOM $OPTION)
	 (COND
	   ((MFUNCTION-CALL $MEMBER $OPTION
			    '((MLIST SIMP) $BUT_FIRST $INCLUDE $MODE))
	      (SIMPLIFY
		(MFUNCTION-CALL
		  $ERROR '|&The| $OPTION
		  '|&option to DEF_STRUCTURE must have a value|)))
	   (T
	     (COND
	       ((LIKE $OPTION '$CONC_NAME)
		  (SETQ $CONC
			(SIMPLIFY (MFUNCTION-CALL $CONCAT $NAME '&_))))
	       (T
		 (COND ((MFUNCTION-CALL
			  $MEMBER $OPTION
			  '((MLIST SIMP) $CONSTRUCTOR $ALTERANT))
			  '$DONE)
		       (T (SIMPLIFY
			    (MFUNCTION-CALL
			      $ERROR '|&Unknown option to DEF_STRUCTURE|
			      $OPTION)))))))))
      (T
       ((LAMBDA ($VALUE)
	 NIL
	 ((LAMBDA ($MULTIPLE_VALUESP $SELECTOR)
	   NIL
	   NIL
	   (COND
	    ((MFUNCTION-CALL
	       $MEMBER $SELECTOR '((MLIST) $CONSTRUCTOR $ALTERANT
				   $CONC_NAME $BUT_FIRST $MODE))
	     (COND
	      ($MULTIPLE_VALUESP
	       (SIMPLIFY
		(MFUNCTION-CALL
		 $ERROR
		 '|&Only the INCLUDE option to DEF_STRUCTURE can have a list as its RHS:|
		 $OPTION)))
	      ((NOT (MFUNCTION-CALL $SYMBOLP $VALUE))
		 (SIMPLIFY (MFUNCTION-CALL
			     $ERROR '|&RHS of option| $SELECTOR
			     '|&must be a name|))))))
	   (COND
	    ((LIKE $SELECTOR '$CONSTRUCTOR)
	       (SETQ $CONSTRUCT $VALUE))
	    (T
	     (COND
	      ((LIKE $SELECTOR '$ALTERANT) (SETQ $ALT $VALUE))
	      (T
	       (COND
		((LIKE $SELECTOR '$CONC_NAME)
		   (SETQ $CONC $VALUE))
		(T
		 (COND
		  ((LIKE $SELECTOR '$BUT_FIRST)
		     (SETQ $FIRST $VALUE))
		  (T
		   (COND
		    ((LIKE $SELECTOR '$INCLUDE)
		     ((LAMBDA ()
		       NIL
		       (SETQ
			 $INCLUDE
			 (COND ($MULTIPLE_VALUESP
				 (SIMPLIFY ($FIRST $VALUE)))
			       (T $VALUE)))
		       (COND
			((NOT (MFUNCTION-CALL
				$SYMBOLP $INCLUDE))
			 (SIMPLIFY
			  (MFUNCTION-CALL
			   $ERROR
			   '|&First element of RHS list for INCLUDE option to DEF_STRUCTURE must be a name|
			   $INCLUDE))))
		       (SETQ
			 $INC_MODES
			 (SIMPLIFY
			   (MFUNCTION-CALL $GET $INCLUDE
					   '$MODE_TYPES)))
		       (COND
			($MULTIPLE_VALUESP
			 ((LAMBDA ($N_SLOTS)
			   (DECLARE (FIXNUM $N_SLOTS))
			   NIL
			   (COND
			    ((NOT (LIKE (SIMPLIFY
					  (MFUNCTION-CALL
					    $GET $INCLUDE
					    '$N_ARGS))
					$N_SLOTS))
			     (SIMPLIFY
			      (MFUNCTION-CALL
			       $ERROR
			       '|&Incorrect number of slot initializations given to INCLUDE option of DEF_STRUCTURE|))))
			   (SETQ
			    $INCLUDED_VALUES
			    ((LAMBDA
				($DEFAULTS $SPECS $NAMES)
			      NIL
			      (SETQ $SLOT_NUM
				    (+ $N_SLOTS
				       $SLOT_NUM))
			      (DO
				(($I 1 (+ 1 $I)))
				((> $I $N_SLOTS) '$DONE)
			       ((LAMBDA ($ARG)
				 NIL
				 ((LAMBDA ($EQP)
				   NIL
				   NIL
				   ((LAMBDA
				       ($NAME_SPEC)
				     NIL
				     ((LAMBDA
					 ($TYP $T)
				       NIL
				       (COND
					((NOT
					   (MFUNCTION-CALL
					     $MEMBER
					     $T
					     $NAMES))
					  (SIMPLIFY
					    (MFUNCTION-CALL
					      $ERROR
					      $ARG
					      '|&is a bad slot name for|
					      $INCLUDE)))
					(T
					 ((LAMBDA
					     ()
					   ((LAMBDA
					       (MCATCH)
					     (PROG2
					      NIL
					      (*CATCH
					       'MCATCH
					       (PROGN
						(DO
						  (($M
						     1
						     (+
						       1
						       $M)))
						  ((>
						     $M
						     $N_SLOTS)
						    '$DONE)
						 (COND
						  ((LIKE
						     $T
						     (MARRAYREF
						       $NAMES
						       $M))
						   ((LAMBDA
						      ($DM)
						     NIL
						     (COND
						      ((AND
							 (IS-BOOLE-CHECK
							   $TYP)
							 (NOT
							   (LIKE
							     $TYP
							     $DM)))
						       (SIMPLIFY
							(MFUNCTION-CALL
							 $ERROR
							 '|&Mode spec for included slot disagrees with slot from original structure|))))
						     ((LAMBDA
							(X)
						       (COND
							((NULL
							   MCATCH)
							 (DISPLA
							   X)
							 (*MERROR
							  '|THROW not within CATCH|)))
						       (*THROW
							 'MCATCH
							 X))
						      (COND
						       ($EQP
							(MARRAYSET
							 (MBUILDQ-SUBST
							  (LIST
							   (CONS
							    '$VAL
							    (SIMPLIFY
							     (MFUNCTION-CALL
							       $RHS
							       $ARG))))
							  '$VAL)
							 $DEFAULTS
							 $M)
							(SIMPLIFY
							 (MFUNCALL
							  '$MODE_IDENTITY
							  $DM
							  (SIMPLIFY
							    (MFUNCTION-CALL
							      $RHS
							      $ARG))))))))
						    (SIMPLIFY
						      (MFUNCTION-CALL
							$SLOT_TYPE
							$INC_MODES
							$M))))))))
					      (ERRLFUN1
						MCATCH)))
					    (CONS
					      BINDLIST
					      LOCLIST)))))))
				      (SIMPLIFY
					(MFUNCTION-CALL
					  $ASSESS_MODE
					  $NAME_SPEC))
				      (SIMPLIFY
					(MFUNCTION-CALL
					  $NAME_OF_SLOT_ID
					  $NAME_SPEC))))
				    (COND
				      ($EQP
					(SIMPLIFY
					  (MFUNCTION-CALL
					    $LHS
					    $ARG)))
				      (T $ARG))))
				  (MFUNCTION-CALL
				    $EQUAL_OP
				    $ARG)))
				(MARRAYREF $SPECS $I)))
			      $DEFAULTS)
			     ((LAMBDA (G0017 G0018)
				(COND
				  ((NOT (< G0018
					   G0017))
				     (DO
				       (($M G0017
					    (1+ $M))
					(G0019
					  NIL
					  (CONS
					    $DEFAULT_VALUE
					    G0019)))
				       ((> $M
					   G0018)
					  (CONS
					    '(MLIST)
					    (NREVERSE
					      G0019)))
				       (DECLARE
					 (FIXNUM $M))))
				  (T (INTERVAL-ERROR
				       '$MAKELIST
				       G0017
				       G0018))))
			      1 $N_SLOTS)
			     (SIMPLIFY (MFUNCTION-CALL
					 $REST $VALUE))
			     (SIMPLIFY
			       (MFUNCTION-CALL
				 $GET $INCLUDE
				 '$SLOT_NAMES)))))
			  (+ -1
			     (MFUNCTION-CALL
			       $LENGTH $VALUE))))))))
		    (T
		      (COND
			((LIKE $SELECTOR '$MODE) '$DONE)
			(T
			  (SIMPLIFY
			    (MFUNCTION-CALL
			      $ERROR
			      '|&Unknown option to DEF_STRUCTURE|
			      $SELECTOR)))))))))))))))
	  (MFUNCTION-CALL $LISTP $VALUE)
	  (SIMPLIFY (MFUNCTION-CALL $LHS $OPTION))))
	(SIMPLIFY (MFUNCTION-CALL $RHS $OPTION))))))
    ((LAMBDA ($DEFAULTS $SLOT_NAMES $RET_MACROS $ACCESSORS)
      NIL
      (DO (($I 1 (+ 1 $I))) ((> $I $QUAN) '$DONE)
	((LAMBDA ($SLOT)
	   NIL
	   ((LAMBDA ($EQP)
	      NIL
	      NIL
	      ((LAMBDA ($NOM_SPEC)
		 NIL
		 ((LAMBDA ($TYPE $TYPED)
		    NIL
		    NIL
		    (COND
		      ((SETQ $TYPED (NOT (LIKE $TYPE NIL)))
			 (COND
			   ((AND (MFUNCTION-CALL $ATOM $MODE_TYPE)
				 (NOT (LIKE $TYPE $MODE_TYPE)))
			      (SETQ
				$MODE_TYPE
				((LAMBDA (G0023 G0024)
				   (COND
				     ((NOT (< G0024 G0023))
					(DO (($M G0023 (1+ $M))
					     (G0025
					       NIL
					       (CONS $MODE_TYPE G0025)))
					    ((> $M G0024)
					       (CONS '(MLIST)
						     (NREVERSE G0025)))
					  (DECLARE (FIXNUM $M))))
				     (T (INTERVAL-ERROR
					  '$MAKELIST G0023 G0024))))
				 1 $QUAN))))
			 (COND
			   ((MFUNCTION-CALL $LISTP $MODE_TYPE)
			      (MARRAYSET $TYPE $MODE_TYPE $I)
			      (MARRAYSET
				(SIMPLIFY (MFUNCTION-CALL
					    $OBTAIN_DEFAULT_VALUE_FOR_MODE
					    $TYPE))
				$DEFAULTS $I)))))
		    (SETQ
		      $SLOT_NAMES
		      (SIMPLIFY
			(MFUNCTION-CALL
			  $ENDCONS
			  (SIMPLIFY
			    (MFUNCTION-CALL $NAME_OF_SLOT_ID $NOM_SPEC))
			  $SLOT_NAMES)))
		    (COND
		      ($EQP
			(MARRAYSET
			  (MBUILDQ-SUBST
			    (LIST
			      (CONS
				'$VAL
				(SIMPLIFY (MFUNCTION-CALL $RHS $SLOT))))
			    '$VAL)
			  $DEFAULTS $I)
			(SIMPLIFY
			  (MFUNCALL
			    '$MODE_IDENTITY
			    (COND
			      ($TYPED $TYPE)
			      (T (SIMPLIFY (MFUNCTION-CALL
					     $SLOT_TYPE $MODE_TYPE $I))))
			    (MARRAYREF $DEFAULTS $I))))))
		  (SIMPLIFY (MFUNCTION-CALL $ASSESS_MODE $NOM_SPEC))
		  '$TYPED))
	       (COND ($EQP (SIMPLIFY (MFUNCTION-CALL $LHS $SLOT)))
		     (T $SLOT))))
	    (MFUNCTION-CALL $EQUAL_OP $SLOT)))
	 (MARRAYREF $SLOTS $I)))
      (SETQ
	$ACCESSORS
	(COND
	  ((LIKE $CONC NIL) $SLOT_NAMES)
	  (T
	    ((LAMBDA (G0026 G0027)
	       (COND
		 ((NOT (< G0027 G0026))
		    (DO
		      (($K G0026 (1+ $K))
		       (G0028
			 NIL
			 (CONS
			   (SIMPLIFY
			     (MFUNCTION-CALL $CONCAT $CONC
					     (MARRAYREF $SLOT_NAMES $K)))
			   G0028)))
		      ((> $K G0027) (CONS '(MLIST) (NREVERSE G0028)))
		      (DECLARE (FIXNUM $K))))
		 (T (INTERVAL-ERROR '$MAKELIST G0026 G0027))))
	     1 $QUAN))))
      ((LAMBDA ($ARG)
	 NIL
	 (DO (($J 1 (+ 1 $J))) ((> $J $QUAN) '$DONE)
	   (SETQ
	     $RET_MACROS
	     (SIMPLIFY
	       (MFUNCTION-CALL
		 $ENDCONS
		 (MBUILDQ-SUBST
		   (LIST
		     (CONS '$SLOT_NUM $SLOT_NUM)
		     (CONS '$ELEMENT (MARRAYREF $ACCESSORS $J))
		     (CONS '$ARG $ARG) (CONS '$NAME $NAME)
		     (CONS '$LAMODE
			   (SIMPLIFY
			     (MFUNCTION-CALL $SLOT_TYPE $MODE_TYPE $J))))
		   '((MDEFMACRO) (($ELEMENT) $%X_%)
		     (($BUILDQ) ((MLIST) $%X_%)
		      (($MODE_IDENTITY) $LAMODE
		       (($REFERENCE_AN_EXTEND) $ARG ((MQUOTE) $ELEMENT)
			((MQUOTE) $NAME) $SLOT_NUM)))))
		 $RET_MACROS)))
	   (SETQ $SLOT_NUM (+ $SLOT_NUM 1))))
       (COND ((LIKE $FIRST NIL) '$%X_%)
	     (T (MBUILDQ-SUBST
		  (LIST (CONS '$FUNCT $FIRST)) '(($FUNCT) $%X_%)))))
      (COND
	((NOT (LIKE $INCLUDE NIL))
	  (SETQ
	    $ACCESSORS
	    (SIMPLIFY
	      (MFUNCTION-CALL
		$APPEND
		(SIMPLIFY (MFUNCTION-CALL $GET $INCLUDE '$ACCESSORS))
		$ACCESSORS)))
	  (SETQ
	    $SLOT_NAMES
	    (SIMPLIFY
	      (MFUNCTION-CALL
		$APPEND
		(SIMPLIFY (MFUNCTION-CALL $GET $INCLUDE '$SLOT_NAMES))
		$SLOT_NAMES)))
	  (SETQ
	    $MODE_TYPE
	    (COND
	      ((MFUNCTION-CALL $LISTP $MODE_TYPE)
		 (SIMPLIFY
		   (MFUNCTION-CALL
		     $APPEND
		     (COND
		       ((MFUNCTION-CALL $LISTP $INC_MODES) $INC_MODES)
		       (T
			 ((LAMBDA (G0029 G0030)
			    (COND
			      ((NOT (< G0030 G0029))
				 (DO (($M G0029 (1+ $M))
				      (G0031
					NIL (CONS $INC_MODES G0031)))
				     ((> $M G0030)
					(CONS '(MLIST) (NREVERSE G0031)))
				   (DECLARE (FIXNUM $M))))
			      (T (INTERVAL-ERROR
				   '$MAKELIST G0029 G0030))))
			  1
			  (SIMPLIFY
			    (MFUNCTION-CALL $GET $INCLUDE '$N_ARGS)))))
		     $MODE_TYPE)))
	      (T
		(COND
		  ((MFUNCTION-CALL $LISTP $INC_MODES)
		     (SIMPLIFY
		       (MFUNCTION-CALL
			 $APPEND $INC_MODES
			 ((LAMBDA (G0032 G0033)
			    (COND
			      ((NOT (< G0033 G0032))
				 (DO (($M G0032 (1+ $M))
				      (G0034
					NIL (CONS $MODE_TYPE G0034)))
				     ((> $M G0033)
					(CONS '(MLIST) (NREVERSE G0034)))
				   (DECLARE (FIXNUM $M))))
			      (T (INTERVAL-ERROR
				   '$MAKELIST G0032 G0033))))
			  1 $QUAN))))
		  (T
		    (COND
		      ((NOT (LIKE $MODE_TYPE $INC_MODES))
			 (SIMPLIFY
			   (MFUNCTION-CALL
			     $APPEND
			     ((LAMBDA (G0035 G0036)
				(COND
				  ((NOT (< G0036 G0035))
				     (DO (($M G0035 (1+ $M))
					  (G0037
					    NIL
					    (CONS $INC_MODES G0037)))
					 ((> $M G0036)
					    (CONS '(MLIST)
						  (NREVERSE G0037)))
				       (DECLARE (FIXNUM $M))))
				  (T (INTERVAL-ERROR
				       '$MAKELIST G0035 G0036))))
			      1
			      (SIMPLIFY
				(MFUNCTION-CALL $GET $INCLUDE '$N_ARGS)))
			     ((LAMBDA (G0038 G0039)
				(COND
				  ((NOT (< G0039 G0038))
				     (DO (($M G0038 (1+ $M))
					  (G0040
					    NIL
					    (CONS $MODE_TYPE G0040)))
					 ((> $M G0039)
					    (CONS '(MLIST)
						  (NREVERSE G0040)))
				       (DECLARE (FIXNUM $M))))
				  (T (INTERVAL-ERROR
				       '$MAKELIST G0038 G0039))))
			      1 $QUAN))))))))))
	  (SETQ
	    $DEFAULTS
	    (SIMPLIFY
	      (MFUNCTION-CALL
		$APPEND
		(COND ((LIKE $INCLUDED_VALUES NIL)
			 (SIMPLIFY
			   (MFUNCTION-CALL $GET $INCLUDE '$DEFAULTS)))
		      (T $INCLUDED_VALUES))
		$DEFAULTS)))
	  (SETQ $QUAN (MFUNCTION-CALL $LENGTH $DEFAULTS))))
      ((LAMBDA (|tr-gensym~0|)
	 (PROGN
	   (ASSIGN-MODE-CHECK '$%%EXISTING_STRUCTURES%% |tr-gensym~0|)
	   (SETQ $%%EXISTING_STRUCTURES%% |tr-gensym~0|)))
       (SIMPLIFY (MFUNCTION-CALL
		   $CONS $NAME
		   (TRD-MSYMEVAL $%%EXISTING_STRUCTURES%% '((MLIST))))))
      (SIMPLIFY (MFUNCTION-CALL $PUT $NAME $QUAN '$N_ARGS))
      (SIMPLIFY (MFUNCTION-CALL $PUT $NAME $DEFAULTS '$DEFAULTS))
      (SIMPLIFY (MFUNCTION-CALL $PUT $NAME $ACCESSORS '$ACCESSORS))
      (SIMPLIFY (MFUNCTION-CALL $PUT $NAME $SLOT_NAMES '$SLOT_NAMES))
      (SIMPLIFY (MFUNCTION-CALL $PUT $NAME $MODE_TYPE '$MODE_TYPES))
      (COND
	((NOT (LIKE $ALT NIL))
	   (SETQ
	     $RET_MACROS
	     (SIMPLIFY
	       (MFUNCTION-CALL
		 $CONS
		 (MBUILDQ-SUBST
		   (LIST (CONS '$ALT $ALT) (CONS '$SLOT_NAMES $SLOT_NAMES)
			 (CONS '$QUAN $QUAN) (CONS '$MODE_TYPE $MODE_TYPE)
			 (CONS '$NAME $NAME))
		   '((MDEFMACRO) (($ALT) $%OBJ_% ((MLIST) $%ARGS_%))
		     (($%AUX_ALTERANT%) ((MQUOTE) $ALT)
		      ((MQUOTE) $NAME) ((MQUOTE) $SLOT_NAMES) $QUAN
		      ((MQUOTE) $MODE_TYPE) $%OBJ_% $%ARGS_%)))
		 $RET_MACROS)))))
      (COND
	((NOT (LIKE $CONSTRUCT NIL))
	   (SETQ
	     $RET_MACROS
	     (SIMPLIFY
	       (MFUNCTION-CALL
		 $CONS
		 (MBUILDQ-SUBST
		   (LIST
		     (CONS '$CONSTRUCT $CONSTRUCT)
		     (CONS '$SLOT_NAMES $SLOT_NAMES)
		     (CONS '$DEFAULTS $DEFAULTS) (CONS '$QUAN $QUAN)
		     (CONS '$MODE_TYPE $MODE_TYPE) (CONS '$NAME $NAME))
		   '((MDEFMACRO) (($CONSTRUCT) ((MLIST) $%ARGS_%))
		     (($%AUX_CONSTRUCTOR%) ((MQUOTE) $CONSTRUCT)
		      ((MQUOTE) $SLOT_NAMES) ((MQUOTE) $DEFAULTS)
		      $QUAN ((MQUOTE) $MODE_TYPE)
		      ((MQUOTE) $NAME) $%ARGS_%)))
		 $RET_MACROS)))))
      (MBUILDQ-SUBST
	(LIST (CONS '$NAME $NAME) (CONS '$RET_MACROS $RET_MACROS))
	'((MPROGN) (($SPLICE) $RET_MACROS) ((MQUOTE) $NAME))))
     ((LAMBDA (G0020 G0021)
	(COND ((NOT (< G0021 G0020))
		 (DO (($M G0020 (1+ $M))
		      (G0022 NIL (CONS $DEFAULT_VALUE G0022)))
		     ((> $M G0021) (CONS '(MLIST) (NREVERSE G0022)))
		   (DECLARE (FIXNUM $M))))
	      (T (INTERVAL-ERROR '$MAKELIST G0020 G0021))))
      1 $QUAN)
     '((MLIST)) '((MLIST)) '((MLIST))))
   (SIMPLIFY (MFUNCTION-CALL $CONCAT '$MAKE_ $NAME))
   (SIMPLIFY (MFUNCTION-CALL $CONCAT '$ALTER_ $NAME)) '$ANY
   NIL NIL NIL NIL 0 1 '((MQUOTE) $%UNDEFINED%) '$INC_MODES)))

(DEFPROP $INITIALIZE_STRUCTURE_LIST T TRANSLATED)

(ADD2LNC '$INITIALIZE_STRUCTURE_LIST $PROPS)

(DEFMTRFUN ($INITIALIZE_STRUCTURE_LIST $LIST MDEFINE NIL NIL) NIL NIL
	   (PROGN (ASSIGN-MODE-CHECK '$%%EXISTING_STRUCTURES%% '((MLIST)))
		  (SETQ $%%EXISTING_STRUCTURES%% '((MLIST)))))

(DEFPROP $SAVE_RUNTIME_STRUCTURE_INFO T TRANSLATED)

(ADD2LNC '$SAVE_RUNTIME_STRUCTURE_INFO $PROPS)

(DEFMTRFUN
  ($SAVE_RUNTIME_STRUCTURE_INFO $ANY MDEFINE NIL NIL) ($FILE) NIL
  ((LAMBDA ($PATH)
     NIL
     ((LAMBDA ($NFILE $NAME $VERSION_NO)
	NIL
	(SIMPLIFY
	  (MFUNCTION-CALL
	    $PUT $NAME
	    (COND ((LIKE $VERSION_NO NIL) '$%UNKNOWN%) (T $VERSION_NO))
	    '$VERSION))
	(SIMPLIFY
	  (MAPPLY-TR
	    '$SAVE
	    (SIMPLIFY
	      (MFUNCTION-CALL
		$APPEND (LIST '(MLIST) $NFILE $NAME '&{)
		(TRD-MSYMEVAL $%%EXISTING_STRUCTURES%% '((MLIST)))))))
	(PROGN (ASSIGN-MODE-CHECK '$%%EXISTING_STRUCTURES%% '((MLIST)))
	       (SETQ $%%EXISTING_STRUCTURES%% '((MLIST))))
	(SIMPLIFY (MFUNCTION-CALL $COMPILE_LISP_FILE $NFILE)))
      (SIMPLIFY (MFUNCTION-CALL
		  INTERN (SIMPLIFY (MFUNCTION-CALL NAMESTRING $PATH))))
      (SIMPLIFY
	(MFUNCTION-CALL
	  $CONCAT
	  (SIMPLIFY
	    (MFUNCTION-CALL
	      INTERN (SIMPLIFY (MFUNCTION-CALL PATHNAME-NAME $PATH))))))
      (SIMPLIFY (MFUNCTION-CALL PATHNAME-VERSION $PATH))))
   (SIMPLIFY
     (MFUNCTION-CALL MERGE-PATHNAME-DEFAULTS '=.LSP
		     (SIMPLIFY (MFUNCTION-CALL STRIPDOLLAR $FILE))))))

(compile-forms-to-compile-queue)

