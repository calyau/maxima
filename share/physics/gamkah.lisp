
	(SETQ IBASE 8.)

 	(SETQ SAVENO 5372)

 	(DEFPROP $K1
		 (LAMBDA ($LIST $PARIT) (SIMPLIFY ($K0 $LIST 0 1 $PARIT)))
		 EXPR)

 	(ARGS '$K1 '(NIL . 2))

 	(DEFPROP $K1 T TRANSLATED)

 	(ADD2LNC '$K1 $PROPS)

 	(MDEFPROP $K1
		  ((LAMBDA) ((MLIST) $LIST $PARIT) (($K0) $LIST 0 1 $PARIT))
		  MEXPR)

 	(ARGS '$K1 '(NIL . 2))

 	(ADD2LNC '(($K1) $LIST $PARIT) $FUNCTIONS)

 	(DEFPROP
	 $JMP
	 (LAMBDA ($LIST $ELEM $OLDI $PARIT) 
	   ((LAMBDA ($I) 
	      (PROG NIL 
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((AND (LIKE ($PART $LIST $I) $ELEM)
				(NOT (LIKE $I $OLDI)))
			   (GO $ON))
			  (T (GO $LOOP)))
	       $ON  (RETURN (LIST '(MLIST)
				  (MUL (MEVAL1 '(($PARIT ARRAY) $I))
				       (MEVAL1 '(($PARIT ARRAY) $OLDI)))
				  $I))))
	    '$I))
	 EXPR)

 	(ARGS '$JMP '(NIL . 4))

 	(DEFPROP $JMP T TRANSLATED)

 	(ADD2LNC '$JMP $PROPS)

 	(MDEFPROP $JMP
		  ((LAMBDA)
		   ((MLIST) $LIST $ELEM $OLDI $PARIT)
		   ((MPROG)
		    ((MLIST) $I)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MAND)
		      ((MEQUAL) (($PART) $LIST $I) $ELEM)
		      ((MNOTEQUAL) $I $OLDI))
		     ((MGO) $ON)
		     T
		     ((MGO) $LOOP))
		    $ON
		    ((MRETURN)
		     ((MLIST)
		      ((MTIMES) (($PARIT ARRAY) $I) (($PARIT ARRAY) $OLDI))
		      $I))))
		  MEXPR)

 	(ARGS '$JMP '(NIL . 4))

 	(ADD2LNC '(($JMP) $LIST $ELEM $OLDI $PARIT) $FUNCTIONS)

 	(DEFPROP
	 $K0
	 (LAMBDA ($LIST $I0 $INC $PARIT) 
	   ((LAMBDA ($I $PA $C $UNT $USED) 
	      (PROG NIL 
		    (SETQ $I $I0)
		    (SETQ $UNT (LIST '(MLIST)))
		    (SETQ $USED (LIST '(MLIST)))
	       $LOOP(SETQ $I (ADD $I $INC))
		    (COND ((OR (IS (MGRP $I ($LENGTH $LIST)))
			       ($MEMBER $I $USED))
			   (RETURN (LIST '(MLIST) $USED $UNT))))
		    (COND ((NOT (LIKE (MEVAL1 '(($PARIT ARRAY) $I)) 0))
			   (GO $CTD)))
		    (SETQ $UNT (SIMPLIFY ($APPEND $UNT
						  (LIST '(MLIST)
							($PART $LIST $I)))))
		    (SETQ $USED
			  (SIMPLIFY ($APPEND $USED (LIST '(MLIST) $I))))
		    (GO $LOOP)
	       $CTD (SETQ $PA ($PART $LIST $I))
		    (SETQ $C (SIMPLIFY ($JMP $LIST $PA $I $PARIT)))
		    (SETQ $INC (SIMPLIFY (LIST '(MMINUS)
					       (MUL $INC ($FIRST $C)))))
		    (SETQ $I ($LAST $C))
		    (GO $LOOP)))
	    '$I
	    '$PA
	    '$C
	    '$UNT
	    '$USED))
	 EXPR)

 	(ARGS '$K0 '(NIL . 4))

 	(DEFPROP $K0 T TRANSLATED)

 	(ADD2LNC '$K0 $PROPS)

 	(MDEFPROP $K0
		  ((LAMBDA)
		   ((MLIST) $LIST $I0 $INC $PARIT)
		   ((MPROG)
		    ((MLIST) $I $PA $C $UNT $USED)
		    ((MSETQ) $I $I0)
		    ((MSETQ) $UNT ((MLIST)))
		    ((MSETQ) $USED ((MLIST)))
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I $INC))
		    ((MCOND)
		     ((MOR)
		      ((MGREATERP) $I (($LENGTH) $LIST))
		      (($MEMBER) $I $USED))
		     ((MRETURN) ((MLIST) $USED $UNT))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOTEQUAL) (($PARIT ARRAY) $I) 0)
		     ((MGO) $CTD)
		     T
		     $FALSE)
		    ((MSETQ)
		     $UNT
		     (($APPEND) $UNT ((MLIST) (($PART) $LIST $I))))
		    ((MSETQ) $USED (($APPEND) $USED ((MLIST) $I)))
		    ((MGO) $LOOP)
		    $CTD
		    ((MSETQ) $PA (($PART) $LIST $I))
		    ((MSETQ) $C (($JMP) $LIST $PA $I $PARIT))
		    ((MSETQ) $INC ((MMINUS) ((MTIMES) $INC (($FIRST) $C))))
		    ((MSETQ) $I (($LAST) $C))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$K0 '(NIL . 4))

 	(ADD2LNC '(($K0) $LIST $I0 $INC $PARIT) $FUNCTIONS)

 	(DEFPROP
	 $PAR
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($I $IOLD $NZP $CTIND $LEN $PA $PARIT) 
	      (PROG NIL 
		    (SETQ $NZP 1)
		    (SETQ $CTIND 0)
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $I 0)
		    (SETQ $IOLD 1)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((IS (MGRP $I $LEN)) (RETURN $PARIT)))
		    (SETQ $PA ($PART $LIST $I))
		    (COND
		     ((AND ($MEMBER $PA (MEVAL1 '$IND))
			   (LIKE (ADD $LEN
				      (- ($LENGTH (SIMPLIFY ($DELETE $PA
								     $LIST)))))
				 2))
		      (GO $CTI)))
		    (MSET '(($PARIT ARRAY) $I) 0)
		    (GO $LOOP)
	       $CTI (SETQ $NZP
			  (MUL (POWER -1
				      (ADD $I
					   (SIMPLIFY (LIST '(MMINUS)
							   $IOLD))))
			       $NZP))
		    (MSET '(($PARIT ARRAY) $I) $NZP)
		    (SETQ $IOLD $I)
		    (GO $LOOP)))
	    '$I
	    '$IOLD
	    '$NZP
	    '$CTIND
	    '$LEN
	    '$PA
	    '$PARIT))
	 EXPR)

 	(ARGS '$PAR '(NIL . 1))

 	(DEFPROP $PAR T TRANSLATED)

 	(ADD2LNC '$PAR $PROPS)

 	(MDEFPROP $PAR
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $I $IOLD $NZP $CTIND $LEN $PA $PARIT)
		    ((MSETQ) $NZP 1)
		    ((MSETQ) $CTIND 0)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $I 0)
		    ((MSETQ) $IOLD 1)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND) ((MGREATERP) $I $LEN) ((MRETURN) $PARIT) T $FALSE)
		    ((MSETQ) $PA (($PART) $LIST $I))
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) $PA $IND)
		      ((MEQUAL)
		       ((MPLUS)
			$LEN
			((MMINUS) (($LENGTH) (($DELETE) $PA $LIST))))
		       2))
		     ((MGO) $CTI)
		     T
		     $FALSE)
		    ((MSETQ) (($PARIT ARRAY) $I) 0)
		    ((MGO) $LOOP)
		    $CTI
		    ((MSETQ)
		     $NZP
		     ((MTIMES)
		      ((MEXPT) ((MMINUS) 1) ((MPLUS) $I ((MMINUS) $IOLD)))
		      $NZP))
		    ((MSETQ) (($PARIT ARRAY) $I) $NZP)
		    ((MSETQ) $IOLD $I)
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$PAR '(NIL . 1))

 	(ADD2LNC '(($PAR) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $K2
	 (LAMBDA ($LIST $START $PARIT) 
	   ((LAMBDA ($INC $K0R) 
	      (SETQ $INC (SIMPLIFY (LIST '(MMINUS)
					 (MEVAL1 '(($PARIT ARRAY)
						   (ADD $START -1))))))
	      (SETQ $K0R
		    (SIMPLIFY ($K0 $LIST
				   (ADD $START
					(SIMPLIFY (LIST '(MMINUS)
							$INC)))
				   $INC
				   $PARIT)))
	      $K0R)
	    '$INC
	    '$K0R))
	 EXPR)

 	(ARGS '$K2 '(NIL . 3))

 	(DEFPROP $K2 T TRANSLATED)

 	(ADD2LNC '$K2 $PROPS)

 	(MDEFPROP $K2
		  ((LAMBDA)
		   ((MLIST) $LIST $START $PARIT)
		   ((MPROG)
		    ((MLIST) $INC $K0R)
		    ((MSETQ)
		     $INC
		     ((MMINUS) (($PARIT ARRAY) ((MPLUS) $START ((MMINUS) 1)))))
		    ((MSETQ)
		     $K0R
		     (($K0)
		      $LIST
		      ((MPLUS) $START ((MMINUS) $INC))
		      $INC
		      $PARIT))
		    ((MRETURN) $K0R)))
		  MEXPR)

 	(ARGS '$K2 '(NIL . 3))

 	(ADD2LNC '(($K2) $LIST $START $PARIT) $FUNCTIONS)

 	(DEFPROP
	 $KAH0
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($LEN $ANS $PARIT $USED $I $K1R $K2R $CT) 
	      (PROG NIL 
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $ANS (LIST '(MLIST)))
		    (SETQ $CT 0)
		    (SETQ $PARIT (SIMPLIFY ($PAR $LIST)))
		    (SETQ $K1R (SIMPLIFY ($K1 $LIST $PARIT)))
		    (SETQ $ANS (SIMPLIFY ($APPEND ($LAST $K1R) $ANS)))
		    (SETQ $USED ($FIRST $K1R))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((IS (MGRP $I $LEN))
			   (RETURN (MUL (POWER -2
					       (DIV (ADD $LEN
							 (- ($LENGTH $USED)))
						    2))
					(SIMPLIFY ($CAT $ANS))
					(POWER -1 $CT)))))
		    (COND ((OR ($MEMBER $I $USED)
			       (NOT (LIKE (MEVAL1 '(($PARIT ARRAY) $I))
					  0)))
			   (GO $LOOP)))
		    (SETQ $CT (ADD $CT 1))
		    (SETQ $K2R (SIMPLIFY ($K2 $LIST $I $PARIT)))
		    (SETQ $ANS (SIMPLIFY ($ADDON $ANS ($LAST $K2R))))
		    (SETQ $USED (SIMPLIFY ($APPEND $USED ($FIRST $K2R))))
		    (GO $LOOP)))
	    '$LEN
	    '$ANS
	    '$PARIT
	    '$USED
	    '$I
	    '$K1R
	    '$K2R
	    '$CT))
	 EXPR)

 	(ARGS '$KAH0 '(NIL . 1))

 	(DEFPROP $KAH0 T TRANSLATED)

 	(ADD2LNC '$KAH0 $PROPS)

 	(MDEFPROP $KAH0
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $LEN $ANS $PARIT $USED $I $K1R $K2R $CT)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $ANS ((MLIST)))
		    ((MSETQ) $CT 0)
		    ((MSETQ) $PARIT (($PAR) $LIST))
		    ((MSETQ) $K1R (($K1) $LIST $PARIT))
		    ((MSETQ) $ANS (($APPEND) (($LAST) $K1R) $ANS))
		    ((MSETQ) $USED (($FIRST) $K1R))
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I $LEN)
		     ((MRETURN)
		      ((MTIMES)
		       ((MEXPT)
			((MMINUS) 2)
			((MQUOTIENT)
			 ((MPLUS) $LEN ((MMINUS) (($LENGTH) $USED)))
			 2))
		       (($CAT) $ANS)
		       ((MEXPT) ((MMINUS) 1) $CT)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MOR)
		      (($MEMBER) $I $USED)
		      ((MNOTEQUAL) (($PARIT ARRAY) $I) 0))
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MSETQ) $CT ((MPLUS) $CT 1))
		    ((MSETQ) $K2R (($K2) $LIST $I $PARIT))
		    ((MSETQ) $ANS (($ADDON) $ANS (($LAST) $K2R)))
		    ((MSETQ) $USED (($APPEND) $USED (($FIRST) $K2R)))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$KAH0 '(NIL . 1))

 	(ADD2LNC '(($KAH0) $LIST) $FUNCTIONS)

 	(DEFPROP $CAT
		 (LAMBDA ($ANS) 
		   (SETQ $ANS (SIMPLIFY ($EXPAND $ANS)))
		   (COND ((NOT (LIKE ($PART $ANS 0) (MEVAL1 '&+)))
			  (COND ((LIKE ($PART $ANS 0) (MEVAL1 '&*))
				 (MUL ($PART $ANS 1)
				      (MEVAL '(($TR) (($PART) $ANS 2)))))
				(T (MEVAL '(($TR) $ANS)))))
			 (T (SIMPLIFY ($MAP $CAT $ANS)))))
		 EXPR)

 	(ARGS '$CAT '(NIL . 1))

 	(DEFPROP $CAT T TRANSLATED)

 	(ADD2LNC '$CAT $PROPS)

 	(MDEFPROP $CAT
		  ((LAMBDA)
		   ((MLIST) $ANS)
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ) $ANS (($EXPAND) $ANS))
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $ANS 0) &+)
		     ((MCOND)
		      ((MEQUAL) (($PART) $ANS 0) &*)
		      ((MTIMES) (($PART) $ANS 1) (($TR) (($PART) $ANS 2)))
		      T
		      (($TR) $ANS))
		     T
		     (($MAP) $CAT $ANS))))
		  MEXPR)

 	(ARGS '$CAT '(NIL . 1))

 	(ADD2LNC '(($CAT) $ANS) $FUNCTIONS)

 	(DEFPROP $ADDON
		 (LAMBDA ($EXP $NEXP) 
		   (SETQ $EXP (SIMPLIFY ($EXPAND $EXP)))
		   (COND ((NOT (LIKE ($PART $EXP 0) (MEVAL1 '&+)))
			  (SIMPLIFY ($ADO1 $EXP)))
			 (T (SIMPLIFY ($MAP $ADO1 $EXP)))))
		 EXPR)

 	(ARGS '$ADDON '(NIL . 2))

 	(DEFPROP $ADDON T TRANSLATED)

 	(ADD2LNC '$ADDON $PROPS)

 	(MDEFPROP $ADDON
		  ((LAMBDA)
		   ((MLIST) $EXP $NEXP)
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ) $EXP (($EXPAND) $EXP))
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $EXP 0) &+)
		     (($ADO1) $EXP)
		     T
		     (($MAP) $ADO1 $EXP))))
		  MEXPR)

 	(ARGS '$ADDON '(NIL . 2))

 	(ADD2LNC '(($ADDON) $EXP $NEXP) $FUNCTIONS)

 	(DEFPROP
	 $ADO
	 (LAMBDA ($EXP) 
	   (ADD (SIMPLIFY ($APPEND (MEVAL1 '$NEXP) $EXP))
		(SIMPLIFY ($APPEND (SIMPLIFY ($REVERSE (MEVAL1 '$NEXP)))
				   $EXP))))
	 EXPR)

 	(ARGS '$ADO '(NIL . 1))

 	(DEFPROP $ADO T TRANSLATED)

 	(ADD2LNC '$ADO $PROPS)

 	(MDEFPROP $ADO
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPLUS)
		    (($APPEND) $NEXP $EXP)
		    (($APPEND) (($REVERSE) $NEXP) $EXP)))
		  MEXPR)

 	(ARGS '$ADO '(NIL . 1))

 	(ADD2LNC '(($ADO) $EXP) $FUNCTIONS)

 	(DEFPROP $ADO1
		 (LAMBDA ($EXP) 
		   (COND ((LIKE ($PART $EXP 0) (MEVAL1 '&*))
			  (MUL ($PART $EXP 1)
			       (SIMPLIFY ($ADO ($PART $EXP 2)))))
			 (T (SIMPLIFY ($ADO $EXP)))))
		 EXPR)

 	(ARGS '$ADO1 '(NIL . 1))

 	(DEFPROP $ADO1 T TRANSLATED)

 	(ADD2LNC '$ADO1 $PROPS)

 	(MDEFPROP $ADO1
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MCOND)
		    ((MEQUAL) (($PART) $EXP 0) &*)
		    ((MTIMES) (($PART) $EXP 1) (($ADO) (($PART) $EXP 2)))
		    T
		    (($ADO) $EXP)))
		  MEXPR)

 	(ARGS '$ADO1 '(NIL . 1))

 	(ADD2LNC '(($ADO1) $EXP) $FUNCTIONS)

 	(DEFPROP
	 $G0C
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($FAC $I) 
	      (PROG NIL 
		    (SETQ $FAC 1)
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((IS (MGRP $I ($LENGTH $LIST)))
			   (RETURN (MUL $FAC (MEVAL '(($G0) $LIST))))))
		    (COND
		     ((NOT (LIKE ($PART $LIST $I)
				 ($PART $LIST
					(COND ((NOT (LIKE $I ($LENGTH $LIST)))
					       (ADD $I 1))
					      (T 1)))))
		      (GO $LOOP))
		     (T (SETQ $FAC
			      (MUL $FAC
				   (MEVAL '(($D0)
					    (($PART) $LIST $I)
					    (($PART) $LIST $I)))))))
		    (SETQ $LIST (SIMPLIFY ($OUTEL $LIST $I)))
		    (GO $LOOP)))
	    '$FAC
	    '$I))
	 EXPR)

 	(ARGS '$G0C '(NIL . 1))

 	(DEFPROP $G0C T TRANSLATED)

 	(ADD2LNC '$G0C $PROPS)

 	(MDEFPROP $G0C
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $FAC $I)
		    ((MSETQ) $FAC 1)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I (($LENGTH) $LIST))
		     ((MRETURN) ((MTIMES) $FAC (($G0) $LIST)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOTEQUAL)
		      (($PART) $LIST $I)
		      (($PART)
		       $LIST
		       ((MCOND)
			((MNOTEQUAL) $I (($LENGTH) $LIST))
			((MPLUS) $I 1)
			T
			1)))
		     ((MGO) $LOOP)
		     T
		     ((MSETQ)
		      $FAC
		      ((MTIMES)
		       $FAC
		       (($D0) (($PART) $LIST $I) (($PART) $LIST $I)))))
		    ((MSETQ) $LIST (($OUTEL) $LIST $I))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$G0C '(NIL . 1))

 	(ADD2LNC '(($G0C) $LIST) $FUNCTIONS)

 	(DEFPROP $OUTEL
		 (LAMBDA ($LIST $I) 
		   (SIMPLIFY ($APPEND ($REST $LIST
					     (ADD $I -1 (- ($LENGTH $LIST))))
				      ($REST $LIST (ADD $I 1)))))
		 EXPR)

 	(ARGS '$OUTEL '(NIL . 2))

 	(DEFPROP $OUTEL T TRANSLATED)

 	(ADD2LNC '$OUTEL $PROPS)

 	(MDEFPROP $OUTEL
		  ((LAMBDA)
		   ((MLIST) $LIST $I)
		   (($APPEND)
		    (($REST)
		     $LIST
		     ((MPLUS) $I ((MMINUS) 1) ((MMINUS) (($LENGTH) $LIST))))
		    (($REST) $LIST ((MPLUS) $I 1))))
		  MEXPR)

 	(ARGS '$OUTEL '(NIL . 2))

 	(ADD2LNC '(($OUTEL) $LIST $I) $FUNCTIONS)

 	(DEFPROP
	 $KAHG0C
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($LEN $ANS $PARIT $USED $I $K1R $K2R $CT) 
	      (PROG NIL 
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $ANS (LIST '(MLIST)))
		    (SETQ $CT 0)
		    (SETQ $PARIT (SIMPLIFY ($PAR $LIST)))
		    (SETQ $K1R (SIMPLIFY ($K1 $LIST $PARIT)))
		    (SETQ $ANS (SIMPLIFY ($APPEND ($LAST $K1R) $ANS)))
		    (SETQ $USED ($FIRST $K1R))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((IS (MGRP $I $LEN))
			   (RETURN (MUL (POWER -2
					       (DIV (ADD $LEN
							 (- ($LENGTH $USED)))
						    2))
					(COND ((NOT (LIKE ($PART $ANS 0)
							  (MEVAL1 '&+)))
					       (SIMPLIFY ($G0C $ANS)))
					      (T (SIMPLIFY ($MAP $G0 $ANS))))
					(POWER -1 $CT)))))
		    (COND ((OR ($MEMBER $I $USED)
			       (NOT (LIKE (MEVAL1 '(($PARIT ARRAY) $I))
					  0)))
			   (GO $LOOP)))
		    (SETQ $CT (ADD $CT 1))
		    (SETQ $K2R (SIMPLIFY ($K2 $LIST $I $PARIT)))
		    (SETQ $ANS (SIMPLIFY ($ADDON $ANS ($LAST $K2R))))
		    (SETQ $USED (SIMPLIFY ($APPEND $USED ($FIRST $K2R))))
		    (GO $LOOP)))
	    '$LEN
	    '$ANS
	    '$PARIT
	    '$USED
	    '$I
	    '$K1R
	    '$K2R
	    '$CT))
	 EXPR)

 	(ARGS '$KAHG0C '(NIL . 1))

 	(DEFPROP $KAHG0C T TRANSLATED)

 	(ADD2LNC '$KAHG0C $PROPS)

 	(MDEFPROP $KAHG0C
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $LEN $ANS $PARIT $USED $I $K1R $K2R $CT)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $ANS ((MLIST)))
		    ((MSETQ) $CT 0)
		    ((MSETQ) $PARIT (($PAR) $LIST))
		    ((MSETQ) $K1R (($K1) $LIST $PARIT))
		    ((MSETQ) $ANS (($APPEND) (($LAST) $K1R) $ANS))
		    ((MSETQ) $USED (($FIRST) $K1R))
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I $LEN)
		     ((MRETURN)
		      ((MTIMES)
		       ((MEXPT)
			((MMINUS) 2)
			((MQUOTIENT)
			 ((MPLUS) $LEN ((MMINUS) (($LENGTH) $USED)))
			 2))
		       ((MCOND)
			((MNOTEQUAL) (($PART) $ANS 0) &+)
			(($G0C) $ANS)
			T
			(($MAP) $G0 $ANS))
		       ((MEXPT) ((MMINUS) 1) $CT)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MOR)
		      (($MEMBER) $I $USED)
		      ((MNOTEQUAL) (($PARIT ARRAY) $I) 0))
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MSETQ) $CT ((MPLUS) $CT 1))
		    ((MSETQ) $K2R (($K2) $LIST $I $PARIT))
		    ((MSETQ) $ANS (($ADDON) $ANS (($LAST) $K2R)))
		    ((MSETQ) $USED (($APPEND) $USED (($FIRST) $K2R)))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$KAHG0C '(NIL . 1))

 	(ADD2LNC '(($KAHG0C) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $KAHG
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($LEN $ANS $PARIT $USED $I $K1R $K2R $CT) 
	      (PROG NIL 
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $ANS (LIST '(MLIST)))
		    (SETQ $CT 0)
		    (SETQ $PARIT (SIMPLIFY ($PAR $LIST)))
		    (SETQ $K1R (SIMPLIFY ($K1 $LIST $PARIT)))
		    (SETQ $ANS (SIMPLIFY ($APPEND ($LAST $K1R) $ANS)))
		    (SETQ $USED ($FIRST $K1R))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((IS (MGRP $I $LEN))
			   (RETURN (MUL (POWER -2
					       (DIV (ADD $LEN
							 (- ($LENGTH $USED)))
						    2))
					(COND ((NOT (LIKE ($PART $ANS 0)
							  (MEVAL1 '&+)))
					       (MEVAL '(($G0) $ANS)))
					      (T (SIMPLIFY ($MAP $G0 $ANS))))
					(POWER -1 $CT)))))
		    (COND ((OR ($MEMBER $I $USED)
			       (NOT (LIKE (MEVAL1 '(($PARIT ARRAY) $I))
					  0)))
			   (GO $LOOP)))
		    (SETQ $CT (ADD $CT 1))
		    (SETQ $K2R (SIMPLIFY ($K2 $LIST $I $PARIT)))
		    (SETQ $ANS (SIMPLIFY ($ADDON $ANS ($LAST $K2R))))
		    (SETQ $USED (SIMPLIFY ($APPEND $USED ($FIRST $K2R))))
		    (GO $LOOP)))
	    '$LEN
	    '$ANS
	    '$PARIT
	    '$USED
	    '$I
	    '$K1R
	    '$K2R
	    '$CT))
	 EXPR)

 	(ARGS '$KAHG '(NIL . 1))

 	(DEFPROP $KAHG T TRANSLATED)

 	(ADD2LNC '$KAHG $PROPS)

 	(MDEFPROP $KAHG
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $LEN $ANS $PARIT $USED $I $K1R $K2R $CT)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $ANS ((MLIST)))
		    ((MSETQ) $CT 0)
		    ((MSETQ) $PARIT (($PAR) $LIST))
		    ((MSETQ) $K1R (($K1) $LIST $PARIT))
		    ((MSETQ) $ANS (($APPEND) (($LAST) $K1R) $ANS))
		    ((MSETQ) $USED (($FIRST) $K1R))
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I $LEN)
		     ((MRETURN)
		      ((MTIMES)
		       ((MEXPT)
			((MMINUS) 2)
			((MQUOTIENT)
			 ((MPLUS) $LEN ((MMINUS) (($LENGTH) $USED)))
			 2))
		       ((MCOND)
			((MNOTEQUAL) (($PART) $ANS 0) &+)
			(($G0) $ANS)
			T
			(($MAP) $G0 $ANS))
		       ((MEXPT) ((MMINUS) 1) $CT)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MOR)
		      (($MEMBER) $I $USED)
		      ((MNOTEQUAL) (($PARIT ARRAY) $I) 0))
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MSETQ) $CT ((MPLUS) $CT 1))
		    ((MSETQ) $K2R (($K2) $LIST $I $PARIT))
		    ((MSETQ) $ANS (($ADDON) $ANS (($LAST) $K2R)))
		    ((MSETQ) $USED (($APPEND) $USED (($FIRST) $K2R)))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$KAHG '(NIL . 1))

 	(ADD2LNC '(($KAHG) $LIST) $FUNCTIONS)

	(SETQ IBASE 10.)


