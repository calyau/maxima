
	(SETQ IBASE 8.)

 	(SETQ SAVENO 5372)

 	(DEFPROP
	 $CONJ
	 (LAMBDA ($AMP) 
	   ((LAMBDA ($LHP0 $RHP0) 
	      (PROG NIL 
		    (COND ((AND (IS ($FREEOF (MEVAL1 '$LHP) $AMP))
				(IS ($FREEOF (MEVAL1 '$RHP) $AMP)))
			   (RETURN (SIMPLIFY ($REVERSE $AMP)))))
		    (SETQ $AMP (SIMPLIFY ($EV $AMP
					      ((MEQUAL) $LHP $LHP0)
					      ((MEQUAL) $RHP $RHP0))))
		    (RETURN (SIMPLIFY ($REVERSE (SIMPLIFY ($EV $AMP
							       ((MEQUAL)
								$LHP0
								$RHP)
							       ((MEQUAL)
								$RHP0
								$LHP))))))))
	    '$LHP0
	    '$RHP0))
	 EXPR)

 	(ARGS '$CONJ '(NIL . 1))

 	(DEFPROP $CONJ T TRANSLATED)

 	(ADD2LNC '$CONJ $PROPS)

 	(MDEFPROP $CONJ
		  ((LAMBDA)
		   ((MLIST) $AMP)
		   ((MPROG)
		    ((MLIST) $LHP0 $RHP0)
		    ((MCOND)
		     ((MAND) (($FREEOF) $LHP $AMP) (($FREEOF) $RHP $AMP))
		     ((MRETURN) (($REVERSE) $AMP))
		     T
		     $FALSE)
		    ((MSETQ)
		     $AMP
		     (($EV) $AMP ((MEQUAL) $LHP $LHP0) ((MEQUAL) $RHP $RHP0)))
		    ((MRETURN)
		     (($REVERSE)
		      (($EV)
		       $AMP
		       ((MEQUAL) $LHP0 $RHP)
		       ((MEQUAL) $RHP0 $LHP))))))
		  MEXPR)

 	(ARGS '$CONJ '(NIL . 1))

 	(ADD2LNC '(($CONJ) $AMP) $FUNCTIONS)

 	(DEFPROP
	 $SPISUM
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($OLIND $DEX $I $IPP $LIND) 
	      (PROG NIL 
		    (SETQ $LIND ($LENGTH (MEVAL1 '$IND)))
		    (SETQ $OLIND (MEVAL1 '$IND))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((IS (MGRP $I $LIND)) (RETURN $LIST)))
		    (SETQ $DEX ($PART $OLIND $I))
		    (COND ((AND (IS ($FREEOF $DEX (MEVAL1 '$NPIND)))
				($MEMBER $DEX $LIST))
			   (GO $DOIT))
			  (T (GO $LOOP)))
	       $DOIT(SETQ $IPP (SIMPLIFY ($CONCAT $DEX
						  (MEVAL1 '$PRIME))))
		    (SETQ $LIST
			  (SIMPLIFY ($APPLY $EV
					    ((MLIST)
					     $LIST
					     ((MEQUAL) $DEX $IPP)))))
		    (MEVAL '(($CIND) ((MLIST) $IPP)))
		    (GO $LOOP)))
	    '$OLIND
	    '$DEX
	    '$I
	    '$IPP
	    '$LIND))
	 EXPR)

 	(ARGS '$SPISUM '(NIL . 1))

 	(DEFPROP $SPISUM T TRANSLATED)

 	(ADD2LNC '$SPISUM $PROPS)

 	(MDEFPROP $SPISUM
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $OLIND $DEX $I $IPP $LIND)
		    ((MSETQ) $LIND (($LENGTH) $IND))
		    ((MSETQ) $OLIND $IND)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND) ((MGREATERP) $I $LIND) ((MRETURN) $LIST) T $FALSE)
		    ((MSETQ) $DEX (($PART) $OLIND $I))
		    ((MCOND)
		     ((MAND) (($FREEOF) $DEX $NPIND) (($MEMBER) $DEX $LIST))
		     ((MGO) $DOIT)
		     T
		     ((MGO) $LOOP))
		    $DOIT
		    ((MSETQ) $IPP (($CONCAT) $DEX $PRIME))
		    ((MSETQ)
		     $LIST
		     (($APPLY) $EV ((MLIST) $LIST ((MEQUAL) $DEX $IPP))))
		    (($CIND) ((MLIST) $IPP))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$SPISUM '(NIL . 1))

 	(ADD2LNC '(($SPISUM) $LIST) $FUNCTIONS)

 	(DEFPROP $SQ2 %SQ2 VERB)

 	(MDEFPROP $SQ2 T MLEXPRP)

 	(MDEFPROP $SQ2
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ) $LIST (($GETRED) $LIST))
		    ((MSETQ) $LIS (($APPEND) $LIS ((MLIST) $LIST)))
		    ((MRETURN) 1)))
		  MEXPR)

 	(ADD2LNC '(($SQ2) ((MLIST) $LIST)) $FUNCTIONS)

 	(DEFPROP %SQ2 $SQ2 NOUN)

 	(DEFPROP $ZMAT
		 (LAMBDA ($EXP) 
		   (COND ((= ($LENGTH $EXP) 2)
			  (MEVAL '(($ZN) (($FIRST) $EXP) (($LAST) $EXP))))
			 (T ($FIRST $EXP))))
		 EXPR)

 	(ARGS '$ZMAT '(NIL . 1))

 	(DEFPROP $ZMAT T TRANSLATED)

 	(ADD2LNC '$ZMAT $PROPS)

 	(MDEFPROP $ZMAT
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MCOND)
		    ((MEQUAL) (($LENGTH) $EXP) 2)
		    (($ZN) (($FIRST) $EXP) (($LAST) $EXP))
		    T
		    (($FIRST) $EXP)))
		  MEXPR)

 	(ARGS '$ZMAT '(NIL . 1))

 	(ADD2LNC '(($ZMAT) $EXP) $FUNCTIONS)

 	(DEFPROP $SREMUV
		 (LAMBDA ($EXP) 
		   (COND ((= ($LENGTH $EXP) 3)
			  (MEVAL '(($UV) (($PART) $EXP 1) (($PART) $EXP 2))))
			 (T (MEVAL '(($UV) (($PART) $EXP 1))))))
		 EXPR)

 	(ARGS '$SREMUV '(NIL . 1))

 	(DEFPROP $SREMUV T TRANSLATED)

 	(ADD2LNC '$SREMUV $PROPS)

 	(MDEFPROP $SREMUV
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MCOND)
		    ((MEQUAL) (($LENGTH) $EXP) 3)
		    (($UV) (($PART) $EXP 1) (($PART) $EXP 2))
		    T
		    (($UV) (($PART) $EXP 1))))
		  MEXPR)

 	(ARGS '$SREMUV '(NIL . 1))

 	(ADD2LNC '(($SREMUV) $EXP) $FUNCTIONS)

 	(DEFPROP $SQ1
		 (LAMBDA ($BIT) 
		   ((LAMBDA ($LIS $BITP) 
		      (SETQ $LIS (LIST '(MLIST)))
		      (SETQ $BITP (SIMPLIFY ($EV $BIT ((MEQUAL) $G $SQ2))))
		      (COND ((NOT (IS ($FREEOF (MEVAL1 '&^)
					       (DIV $BIT $BITP))))
			     (SIMPLIFY ($EV $BIT ((MEQUAL) $G $SQ2)))))
		      (MUL $BITP
			   (SIMPLIFY ($SQI (MEVAL1 '$SPNS) $LIS))))
		    '$LIS
		    '$BITP))
		 EXPR)

 	(ARGS '$SQ1 '(NIL . 1))

 	(DEFPROP $SQ1 T TRANSLATED)

 	(ADD2LNC '$SQ1 $PROPS)

 	(MDEFPROP $SQ1
		  ((LAMBDA)
		   ((MLIST) $BIT)
		   ((MPROG)
		    ((MLIST) $LIS $BITP)
		    ((MSETQ) $LIS ((MLIST)))
		    ((MSETQ) $BITP (($EV) $BIT ((MEQUAL) $G $SQ2)))
		    ((MCOND)
		     ((MNOT) (($FREEOF) &^ ((MQUOTIENT) $BIT $BITP)))
		     (($EV) $BIT ((MEQUAL) $G $SQ2))
		     T
		     $FALSE)
		    ((MRETURN) ((MTIMES) $BITP (($SQI) $SPNS $LIS)))))
		  MEXPR)

 	(ARGS '$SQ1 '(NIL . 1))

 	(ADD2LNC '(($SQ1) $BIT) $FUNCTIONS)

 	(DEFPROP
	 $POLS
	 (LAMBDA ($SPNS $LIS) 
	   (COND ((LIKE ($PART $SPNS 1 0) (MEVAL1 '$UVS))
		  ((LAMBDA ($SPNS1 $LIS1) 
		     (SETQ $SPNS1
			   (SIMPLIFY ($SUBSTPART (($SREMUV) (($FIRST) $SPNS))
						 $SPNS
						 1)))
		     (SETQ $LIS1
			   (SIMPLIFY ($SUBSTPART (($APPEND)
						  ((MLIST)
						   $G5
						   (($LAST) (($PART) $SPNS 1)))
						  (($LAST) $LIS))
						 $LIS
						 2)))
		     (DIV (ADD (SIMPLIFY ($SQI $SPNS1 $LIS))
			       (SIMPLIFY ($SQI $SPNS1 $LIS1)))
			  2))
		   '$SPNS1
		   '$LIS1))
		 (T ((LAMBDA ($SPNS2 $LIS2) 
		       (SETQ $SPNS2
			     (SIMPLIFY ($SUBSTPART (($SREMUV) (($LAST) $SPNS))
						   $SPNS
						   2)))
		       (SETQ $LIS2
			     (SIMPLIFY ($SUBSTPART (($APPEND)
						    ((MLIST)
						     $G5
						     (($LAST)
						      (($PART) $SPNS 2)))
						    (($FIRST) $LIS))
						   $LIS
						   1)))
		       (DIV (ADD (SIMPLIFY ($SQI $SPNS2 $LIS))
				 (SIMPLIFY ($SQI $SPNS2 $LIS2)))
			    2))
		     '$SPNS2
		     '$LIS2))))
	 EXPR)

 	(ARGS '$POLS '(NIL . 2))

 	(DEFPROP $POLS T TRANSLATED)

 	(ADD2LNC '$POLS $PROPS)

 	(MDEFPROP $POLS
		  ((LAMBDA)
		   ((MLIST) $SPNS $LIS)
		   ((MCOND)
		    ((MEQUAL) (($PART) $SPNS 1 0) $UVS)
		    ((MPROG)
		     ((MLIST) $SPNS1 $LIS1)
		     ((MSETQ)
		      $SPNS1
		      (($SUBSTPART) (($SREMUV) (($FIRST) $SPNS)) $SPNS 1))
		     ((MSETQ)
		      $LIS1
		      (($SUBSTPART)
		       (($APPEND)
			((MLIST) $G5 (($LAST) (($PART) $SPNS 1)))
			(($LAST) $LIS))
		       $LIS
		       2))
		     ((MRETURN)
		      ((MQUOTIENT)
		       ((MPLUS) (($SQI) $SPNS1 $LIS) (($SQI) $SPNS1 $LIS1))
		       2)))
		    T
		    ((MPROG)
		     ((MLIST) $SPNS2 $LIS2)
		     ((MSETQ)
		      $SPNS2
		      (($SUBSTPART) (($SREMUV) (($LAST) $SPNS)) $SPNS 2))
		     ((MSETQ)
		      $LIS2
		      (($SUBSTPART)
		       (($APPEND)
			((MLIST) $G5 (($LAST) (($PART) $SPNS 2)))
			(($FIRST) $LIS))
		       $LIS
		       1))
		     ((MRETURN)
		      ((MQUOTIENT)
		       ((MPLUS) (($SQI) $SPNS2 $LIS) (($SQI) $SPNS2 $LIS2))
		       2)))))
		  MEXPR)

 	(ARGS '$POLS '(NIL . 2))

 	(ADD2LNC '(($POLS) $SPNS $LIS) $FUNCTIONS)

 	(DEFPROP
	 $SQ
	 (LAMBDA ($SPN1 $AMP $SPN2) 
	   ((LAMBDA ($SPNS) 
	      (PROG NIL 
		    (SETQ $SPNS (LIST '(MLIST) $SPN1 $SPN2))
		    (SETQ $AMP (SIMPLIFY ($EXPAND (MUL $AMP $AMP))))
		    (RETURN (COND ((LIKE ($PART $AMP 0) (MEVAL1 '&+))
				   (RETURN (SIMPLIFY ($MAP $SQ1 $AMP))))
				  (T (RETURN (SIMPLIFY ($SQ1 $AMP))))))))
	    '$SPNS))
	 EXPR)

 	(ARGS '$SQ '(NIL . 3))

 	(DEFPROP $SQ T TRANSLATED)

 	(ADD2LNC '$SQ $PROPS)

 	(MDEFPROP $SQ
		  ((LAMBDA)
		   ((MLIST) $SPN1 $AMP $SPN2)
		   ((MPROG)
		    ((MLIST) $SPNS)
		    ((MSETQ) $SPNS ((MLIST) $SPN1 $SPN2))
		    ((MSETQ) $AMP (($EXPAND) ((MTIMES) $AMP $AMP)))
		    ((MCOND)
		     ((MEQUAL) (($PART) $AMP 0) &+)
		     ((MRETURN) (($MAP) $SQ1 $AMP))
		     T
		     ((MRETURN) (($SQ1) $AMP)))))
		  MEXPR)

 	(ARGS '$SQ '(NIL . 3))

 	(ADD2LNC '(($SQ) $SPN1 $AMP $SPN2) $FUNCTIONS)

 	(DEFPROP
	 $SQAM
	 (LAMBDA ($SPN1 $A1 $SPN2 $A2) 
	   ((LAMBDA ($SPNS) 
	      (PROG NIL 
		    (SETQ $SPNS (LIST '(MLIST) $SPN1 $SPN2))
		    (SETQ $AMP (SIMPLIFY ($EXPAND (MUL $A1 $A2))))
		    (RETURN
		     (COND
		      ((LIKE ($PART (MEVAL1 '$AMP) 0)
			     (MEVAL1 '&+))
		       (RETURN (SIMPLIFY ($MAP $SQ1 $AMP))))
		      (T (RETURN (SIMPLIFY ($SQ1 (MEVAL1 '$AMP)))))))))
	    '$SPNS))
	 EXPR)

 	(ARGS '$SQAM '(NIL . 4))

 	(DEFPROP $SQAM T TRANSLATED)

 	(ADD2LNC '$SQAM $PROPS)

 	(MDEFPROP $SQAM
		  ((LAMBDA)
		   ((MLIST) $SPN1 $A1 $SPN2 $A2)
		   ((MPROG)
		    ((MLIST) $SPNS)
		    ((MSETQ) $SPNS ((MLIST) $SPN1 $SPN2))
		    ((MSETQ) $AMP (($EXPAND) ((MTIMES) $A1 $A2)))
		    ((MCOND)
		     ((MEQUAL) (($PART) $AMP 0) &+)
		     ((MRETURN) (($MAP) $SQ1 $AMP))
		     T
		     ((MRETURN) (($SQ1) $AMP)))))
		  MEXPR)

 	(ARGS '$SQAM '(NIL . 4))

 	(ADD2LNC '(($SQAM) $SPN1 $A1 $SPN2 $A2) $FUNCTIONS)

 	(DEFPROP
	 $SQI
	 (LAMBDA ($SPNS $LIS) 
	   ((LAMBDA ($BIGL) 
	      (PROG NIL 
		    (COND ((NOT (IS ($FREEOF (MEVAL1 '$UVS) $SPNS)))
			   (RETURN (SIMPLIFY ($POLS $SPNS $LIS)))))
		    (SETQ 
		     $BIGL
		     (LIST
		      '(MLIST)
		      (SIMPLIFY ($ZMAT ($PART $SPNS 1)))
		      ($FIRST $LIS)
		      (SIMPLIFY ($ZMAT ($PART $SPNS 2)))
		      (COND
		       ((IS (MEVAL1 '$NOP))
			(SIMPLIFY ($CONJ ($LAST $LIS))))
		       (T
			(SIMPLIFY
			 ($CONJ (SIMPLIFY ($SPISUM ($LAST $LIS)))))))))
		    (RETURN (COND ((IS (MEVAL1 '$NTR))
				   (RETURN (SIMPLIFY ($APPLY $GT $BIGL))))
				  (T (RETURN (SIMPLIFY ($APPLY $TR
							       $BIGL))))))))
	    '$BIGL))
	 EXPR)

 	(ARGS '$SQI '(NIL . 2))

 	(DEFPROP $SQI T TRANSLATED)

 	(ADD2LNC '$SQI $PROPS)

 	(MDEFPROP $SQI
		  ((LAMBDA)
		   ((MLIST) $SPNS $LIS)
		   ((MPROG)
		    ((MLIST) $BIGL)
		    ((MCOND)
		     ((MNOT) (($FREEOF) $UVS $SPNS))
		     ((MRETURN) (($POLS) $SPNS $LIS))
		     T
		     $FALSE)
		    ((MSETQ)
		     $BIGL
		     ((MLIST)
		      (($ZMAT) (($PART) $SPNS 1))
		      (($FIRST) $LIS)
		      (($ZMAT) (($PART) $SPNS 2))
		      ((MCOND)
		       $NOP
		       (($CONJ) (($LAST) $LIS))
		       T
		       (($CONJ) (($SPISUM) (($LAST) $LIS))))))
		    ((MCOND)
		     $NTR
		     ((MRETURN) (($APPLY) $GT $BIGL))
		     T
		     ((MRETURN) (($APPLY) $TR $BIGL)))))
		  MEXPR)

 	(ARGS '$SQI '(NIL . 2))

 	(ADD2LNC '(($SQI) $SPNS $LIS) $FUNCTIONS)

	(SETQ IBASE 10.)


