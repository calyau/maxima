
	(SETQ IBASE 8.)

 	(SETQ SAVENO 5454)

 	(DEFPROP $ROR (LAMBDA ($LIST) (SIMPLIFY ($REOR $LIST 1))) EXPR)

 	(ARGS '$ROR '(NIL . 1))

 	(DEFPROP $ROR T TRANSLATED)

 	(ADD2LNC '$ROR $PROPS)

 	(MDEFPROP $ROR ((LAMBDA) ((MLIST) $LIST) (($REOR) $LIST 1)) MEXPR)

 	(ARGS '$ROR '(NIL . 1))

 	(ADD2LNC '(($ROR) $LIST) $FUNCTIONS)

 	(DEFPROP $DCON
		 (LAMBDA ($EXP) 
		   (COND ((AND (LIKE ($PART $EXP 1) ($PART $EXP 2))
			       ($MEMBER ($PART $EXP 1) (TRD-MSYMEVAL $IND)))
			  (TRD-MSYMEVAL $N))
			 (T $EXP)))
		 EXPR)

 	(ARGS '$DCON '(NIL . 1))

 	(DEFPROP $DCON T TRANSLATED)

 	(ADD2LNC '$DCON $PROPS)

 	(MDEFPROP $DCON
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MCOND)
		    ((MAND)
		     ((MEQUAL) (($PART) $EXP 1) (($PART) $EXP 2))
		     (($MEMBER) (($PART) $EXP 1) $IND))
		    $N
		    T
		    $EXP))
		  MEXPR)

 	(ARGS '$DCON '(NIL . 1))

 	(ADD2LNC '(($DCON) $EXP) $FUNCTIONS)

 	(DEFPROP $SER
		 (LAMBDA ($LIST) 
		   ((LAMBDA ($SOBS $COBS) 
		      (PROG NIL 
			    (SETQ $COBS 1)
			    (COND ((LIKE $LIST
					 (LIST '(MLIST)
					       '((MLIST))))
				   (RETURN 1)))
		       $BEG (SETQ $SOBS (SIMPLIFY ($SER1 $LIST)))
			    (COND ((LIKE ($PART $SOBS 2)
					 (LIST '(MLIST)
					       '((MLIST))))
				   (RETURN (MUL* $COBS ($PART $SOBS 1)))))
			    (SETQ $COBS (MUL* $COBS ($PART $SOBS 1)))
			    (SETQ $LIST ($PART $SOBS 2))
			    (GO $BEG)))
		    '$SOBS
		    '$COBS))
		 EXPR)

 	(ARGS '$SER '(NIL . 1))

 	(DEFPROP $SER T TRANSLATED)

 	(ADD2LNC '$SER $PROPS)

 	(MDEFPROP $SER
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $SOBS $COBS)
		    ((MSETQ) $COBS 1)
		    ((MCOND)
		     ((MEQUAL) $LIST ((MLIST) ((MLIST))))
		     ((MRETURN) 1)
		     T
		     $FALSE)
		    $BEG
		    ((MSETQ) $SOBS (($SER1) $LIST))
		    ((MCOND)
		     ((MEQUAL) (($PART) $SOBS 2) ((MLIST) ((MLIST))))
		     ((MRETURN) ((MTIMES) $COBS (($PART) $SOBS 1)))
		     T
		     $FALSE)
		    ((MSETQ) $COBS ((MTIMES) $COBS (($PART) $SOBS 1)))
		    ((MSETQ) $LIST (($PART) $SOBS 2))
		    ((MGO) $BEG)))
		  MEXPR)

 	(ARGS '$SER '(NIL . 1))

 	(ADD2LNC '(($SER) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $SWAP
	 (LAMBDA ($LIST $I) 
	   ((LAMBDA ($L $LI $LIP $A1 $A2) 
	      (PROG NIL 
		    (SETQ $L (MEVAL '(($LISBRE) $LIST $I)))
		    (SETQ $LI ($PART $LIST $I))
		    (SETQ $LIP ($PART $LIST (ADD* $I 1)))
		    (SETQ $A1 ($APPEND ($PART $L 1) ($REST ($PART $L 2) 1)))
		    (SETQ $A2 ($APPEND ($PART $L 1)
				       (LIST '(MLIST) $LIP $LI)
				       ($REST ($PART $L 2) 1)))
		    (COND ((OR ($MEMBER $LI (TRD-MSYMEVAL $IND))
			       ($MEMBER $LIP (TRD-MSYMEVAL $IND)))
			   (GO $CON))
			  (T (GO $ON)))
	       $ON  (RETURN (ADD* (MUL* 2
					(MEVAL '(($SOR) $LI $LIP))
					(SIMPLIFY ($REOR $A1 1)))
				  (SIMPLIFY (LIST '(MMINUS)
						  (SIMPLIFY ($REOR $A2 1))))))
	       $CON (COND
		     ((LIKE $LI $LIP)
		      (RETURN
		       (ADD* (MUL* 2
				   (TRD-MSYMEVAL $N)
				   (SIMPLIFY ($CRUNCH $A1)))
			     (SIMPLIFY (LIST '(MMINUS)
					     (SIMPLIFY ($CRUNCH $A2))))))))
		    (COND ((AND (IS ($FREEOF ($PART $LIST $I) $A1))
				(IS ($FREEOF ($PART $LIST (ADD* $I 1)) $A1)))
			   (GO $ON)))
		    (COND
		     (($MEMBER $LI (TRD-MSYMEVAL $IND))
		      (RETURN
		       (ADD*
			(MUL*
			 2
			 (SIMPLIFY
			  ($CRUNCH
			   (SIMPLIFY (MAPPLY (TRD-MSYMEVAL $EV)
					     (LIST $A1
						   (MEVAL '((MEQUAL)
							    $LI
							    $LIP)))
					     '$EV)))))
			(SIMPLIFY (LIST '(MMINUS)
					(SIMPLIFY ($CRUNCH $A2))))))))
		    (RETURN
		     (ADD*
		      (MUL*
		       2
		       (SIMPLIFY
			($CRUNCH (SIMPLIFY (MAPPLY (TRD-MSYMEVAL $EV)
						   (LIST $A1
							 (MEVAL '((MEQUAL)
								  $LIP
								  $LI)))
						   '$EV)))))
		      (SIMPLIFY (LIST '(MMINUS)
				      (SIMPLIFY ($CRUNCH $A2))))))))
	    '$L
	    '$LI
	    '$LIP
	    '$A1
	    '$A2))
	 EXPR)

 	(ARGS '$SWAP '(NIL . 2))

 	(DEFPROP $SWAP T TRANSLATED)

 	(ADD2LNC '$SWAP $PROPS)

 	(MDEFPROP $SWAP
		  ((LAMBDA)
		   ((MLIST) $LIST $I)
		   ((MPROG)
		    ((MLIST) $L $LI $LIP $A1 $A2)
		    ((MSETQ) $L (($LISBRE) $LIST $I))
		    ((MSETQ) $LI (($PART) $LIST $I))
		    ((MSETQ) $LIP (($PART) $LIST ((MPLUS) $I 1)))
		    ((MSETQ)
		     $A1
		     (($APPEND) (($PART) $L 1) (($REST) (($PART) $L 2) 1)))
		    ((MSETQ)
		     $A2
		     (($APPEND)
		      (($PART) $L 1)
		      ((MLIST) $LIP $LI)
		      (($REST) (($PART) $L 2) 1)))
		    ((MCOND)
		     ((MOR) (($MEMBER) $LI $IND) (($MEMBER) $LIP $IND))
		     ((MGO) $CON)
		     T
		     ((MGO) $ON))
		    $ON
		    ((MRETURN)
		     ((MPLUS)
		      ((MTIMES) 2 (($SOR) $LI $LIP) (($REOR) $A1 1))
		      ((MMINUS) (($REOR) $A2 1))))
		    $CON
		    ((MCOND)
		     ((MEQUAL) $LI $LIP)
		     ((MRETURN)
		      ((MPLUS)
		       ((MTIMES) 2 $N (($CRUNCH) $A1))
		       ((MMINUS) (($CRUNCH) $A2))))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND)
		      (($FREEOF) (($PART) $LIST $I) $A1)
		      (($FREEOF) (($PART) $LIST ((MPLUS) $I 1)) $A1))
		     ((MGO) $ON)
		     T
		     $FALSE)
		    ((MCOND)
		     (($MEMBER) $LI $IND)
		     ((MRETURN)
		      ((MPLUS)
		       ((MTIMES)
			2
			(($CRUNCH)
			 (($APPLY) $EV ((MLIST) $A1 ((MEQUAL) $LI $LIP)))))
		       ((MMINUS) (($CRUNCH) $A2))))
		     T
		     $FALSE)
		    ((MRETURN)
		     ((MPLUS)
		      ((MTIMES)
		       2
		       (($CRUNCH)
			(($APPLY) $EV ((MLIST) $A1 ((MEQUAL) $LIP $LI)))))
		      ((MMINUS) (($CRUNCH) $A2))))))
		  MEXPR)

 	(ARGS '$SWAP '(NIL . 2))

 	(ADD2LNC '(($SWAP) $LIST $I) $FUNCTIONS)

 	(DEFPROP
	 $CRUNCH1
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($QW) 
	      (SETQ $QW (SIMPLIFY ($SUBSTITUTE (TRD-MSYMEVAL $CRUNCH0)
					       (TRD-MSYMEVAL $G)
					       $EXP)))
	      (SIMPLIFY ($EV $QW)))
	    '$QW))
	 EXPR)

 	(ARGS '$CRUNCH1 '(NIL . 1))

 	(DEFPROP $CRUNCH1 T TRANSLATED)

 	(ADD2LNC '$CRUNCH1 $PROPS)

 	(MDEFPROP $CRUNCH1
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $QW)
		    ((MSETQ) $QW (($SUBSTITUTE) $CRUNCH0 $G $EXP))
		    ((MRETURN) (($EV) $QW))))
		  MEXPR)

 	(ARGS '$CRUNCH1 '(NIL . 1))

 	(ADD2LNC '(($CRUNCH1) $EXP) $FUNCTIONS)

 	(DEFPROP
	 $REOR
	 (LAMBDA ($LIST $I) 
	   ((LAMBDA ($LI2 $PI $LI) 
	      (PROG NIL 
		    (COND ((NOT (IS (MLSP $I ($LENGTH $LIST))))
			   (RETURN (SIMPLIFY ($G00 $LIST)))))
		    (COND ((LIKE ($PART $LIST $I) ($PART $LIST (ADD* $I 1)))
			   (GO $FIN)))
		    (COND ((IS ($ORDERLESSP ($PART $LIST $I)
					    ($PART $LIST (ADD* $I 1))))
			   (RETURN (SIMPLIFY ($REOR $LIST (ADD* $I 1)))))
			  (T (RETURN (SIMPLIFY ($SWAP $LIST $I)))))
	       $FIN (SETQ $PI ($PART $LIST $I))
		    (SETQ $LI (MEVAL '(($LISBRE) $LIST $I)))
		    (COND ((LIKE (ADD* $I 1) ($LENGTH $LIST))
			   (SETQ $LI2 '((MLIST))))
			  (T (SETQ $LI2 ($REST ($PART $LI 2) 1))))
		    (RETURN
		     (MUL* (MEVAL '(($D) $PI $PI))
			   (SIMPLIFY ($REOR ($APPEND ($PART $LI 1) $LI2)
					    (MAXIMUM (LIST 1
							   (ADD* $I -1)))))))))
	    '$LI2
	    '$PI
	    '$LI))
	 EXPR)

 	(ARGS '$REOR '(NIL . 2))

 	(DEFPROP $REOR T TRANSLATED)

 	(ADD2LNC '$REOR $PROPS)

 	(MDEFPROP $REOR
		  ((LAMBDA)
		   ((MLIST) $LIST $I)
		   ((MPROG)
		    ((MLIST) $LI2 $PI $LI)
		    ((MCOND)
		     ((MGEQP) $I (($LENGTH) $LIST))
		     ((MRETURN) (($G00) $LIST))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL)
		      (($PART) $LIST $I)
		      (($PART) $LIST ((MPLUS) $I 1)))
		     ((MGO) $FIN)
		     T
		     $FALSE)
		    ((MCOND)
		     (($ORDERLESSP)
		      (($PART) $LIST $I)
		      (($PART) $LIST ((MPLUS) $I 1)))
		     ((MRETURN) (($REOR) $LIST ((MPLUS) $I 1)))
		     T
		     ((MRETURN) (($SWAP) $LIST $I)))
		    $FIN
		    ((MSETQ) $PI (($PART) $LIST $I))
		    ((MSETQ) $LI (($LISBRE) $LIST $I))
		    ((MCOND)
		     ((MEQUAL) ((MPLUS) $I 1) (($LENGTH) $LIST))
		     ((MSETQ) $LI2 ((MLIST)))
		     T
		     ((MSETQ) $LI2 (($REST) (($PART) $LI 2) 1)))
		    ((MRETURN)
		     ((MTIMES)
		      (($D) $PI $PI)
		      (($REOR)
		       (($APPEND) (($PART) $LI 1) $LI2)
		       (($MAX) 1 ((MPLUS) $I ((MMINUS) 1))))))))
		  MEXPR)

 	(ARGS '$REOR '(NIL . 2))

 	(ADD2LNC '(($REOR) $LIST $I) $FUNCTIONS)

 	(DEFPROP
	 $SER1
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($H1 $H2 $LIS $I $LEN $DEX $VEC1 $VEC2) 
	      (PROG NIL 
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $VEC1 ($PART $LIST 2 2))
		    (SETQ $DEX ($PART $LIST 2 1))
		    (COND ((LIKE $DEX $VEC1)
			   (RETURN (LIST '(MLIST)
					 (TRD-MSYMEVAL $N)
					 ($APPEND (LIST '(MLIST)
							'((MLIST)))
						  ($REST $LIST 2))))))
		    (SETQ $I 2)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((NOT (IS (MGRP $I $LEN))) (GO $ONIT)))
		    (COND ((AND ($MEMBER $VEC1 (TRD-MSYMEVAL $IND))
				(NOT (IS ($FREEOF $VEC1 ($REST $LIST 2)))))
			   (RETURN (LIST '(MLIST)
					 1
					 ($APPEND (LIST '(MLIST)
							'((MLIST))
							(LIST '(MLIST)
							      $VEC1
							      $DEX))
						  ($REST $LIST 2)))))
			  (T (RETURN (LIST '(MLIST)
					   (MEVAL '(($DP) $VEC1 $DEX))
					   ($APPEND (LIST '(MLIST)
							  '((MLIST)))
						    ($REST $LIST 2))))))
	       $ONIT(COND ((LIKE ($PART $LIST $I 1) $DEX) (GO $S1)))
		    (COND ((LIKE ($PART $LIST $I 2) $DEX) (GO $S2)))
		    (GO $LOOP)
	       $S1  (SETQ $VEC2 ($PART $LIST $I 2))
		    (GO $S3)
	       $S2  (SETQ $VEC2 ($PART $LIST $I 1))
	       $S3  (SETQ $LIS (MEVAL '(($LISBRE) $LIST $I)))
		    (SETQ $LIS
			  ($APPEND (LIST '(MLIST) '((MLIST)))
				   ($REST ($APPEND ($FIRST $LIS) ($LAST $LIS))
					  2)))
		    (SETQ $H1 ($MEMBER $VEC1 (TRD-MSYMEVAL $IND)))
		    (SETQ $H2 ($MEMBER $VEC2 (TRD-MSYMEVAL $IND)))
		    (COND ((AND (NOT (IS $H1)) (NOT (IS $H2)))
			   (RETURN (LIST '(MLIST)
					 (MEVAL '(($DP) $VEC1 $VEC2))
					 $LIS))))
		    (COND ((IS $H1)
			   (RETURN (LIST '(MLIST)
					 1
					 ($APPEND $LIS
						  (LIST '(MLIST)
							(LIST '(MLIST)
							      $VEC1
							      $VEC2)))))))
		    (RETURN (LIST '(MLIST)
				  1
				  ($APPEND $LIS
					   (LIST '(MLIST)
						 (LIST '(MLIST)
						       $VEC2
						       $VEC1)))))))
	    '$H1
	    '$H2
	    '$LIS
	    '$I
	    '$LEN
	    '$DEX
	    '$VEC1
	    '$VEC2))
	 EXPR)

 	(ARGS '$SER1 '(NIL . 1))

 	(DEFPROP $SER1 T TRANSLATED)

 	(ADD2LNC '$SER1 $PROPS)

 	(MDEFPROP $SER1
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $H1 $H2 $LIS $I $LEN $DEX $VEC1 $VEC2)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $VEC1 (($PART) $LIST 2 2))
		    ((MSETQ) $DEX (($PART) $LIST 2 1))
		    ((MCOND)
		     ((MEQUAL) $DEX $VEC1)
		     ((MRETURN)
		      ((MLIST)
		       $N
		       (($APPEND) ((MLIST) ((MLIST))) (($REST) $LIST 2))))
		     T
		     $FALSE)
		    ((MSETQ) $I 2)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND) ((MLEQP) $I $LEN) ((MGO) $ONIT) T $FALSE)
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) $VEC1 $IND)
		      ((MNOT) (($FREEOF) $VEC1 (($REST) $LIST 2))))
		     ((MRETURN)
		      ((MLIST)
		       1
		       (($APPEND)
			((MLIST) ((MLIST)) ((MLIST) $VEC1 $DEX))
			(($REST) $LIST 2))))
		     T
		     ((MRETURN)
		      ((MLIST)
		       (($DP) $VEC1 $DEX)
		       (($APPEND) ((MLIST) ((MLIST))) (($REST) $LIST 2)))))
		    $ONIT
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIST $I 1) $DEX)
		     ((MGO) $S1)
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIST $I 2) $DEX)
		     ((MGO) $S2)
		     T
		     $FALSE)
		    ((MGO) $LOOP)
		    $S1
		    ((MSETQ) $VEC2 (($PART) $LIST $I 2))
		    ((MGO) $S3)
		    $S2
		    ((MSETQ) $VEC2 (($PART) $LIST $I 1))
		    $S3
		    ((MSETQ) $LIS (($LISBRE) $LIST $I))
		    ((MSETQ)
		     $LIS
		     (($APPEND)
		      ((MLIST) ((MLIST)))
		      (($REST) (($APPEND) (($FIRST) $LIS) (($LAST) $LIS)) 2)))
		    ((MSETQ) $H1 (($MEMBER) $VEC1 $IND))
		    ((MSETQ) $H2 (($MEMBER) $VEC2 $IND))
		    ((MCOND)
		     ((MAND) ((MNOT) $H1) ((MNOT) $H2))
		     ((MRETURN) ((MLIST) (($DP) $VEC1 $VEC2) $LIS))
		     T
		     $FALSE)
		    ((MCOND)
		     $H1
		     ((MRETURN)
		      ((MLIST)
		       1
		       (($APPEND) $LIS ((MLIST) ((MLIST) $VEC1 $VEC2)))))
		     T
		     $FALSE)
		    ((MRETURN)
		     ((MLIST)
		      1
		      (($APPEND) $LIS ((MLIST) ((MLIST) $VEC2 $VEC1)))))))
		  MEXPR)

 	(ARGS '$SER1 '(NIL . 1))

 	(ADD2LNC '(($SER1) $LIST) $FUNCTIONS)

 	(DEFPROP $SRES
		 (LAMBDA ($LIST $I) 
		   ((LAMBDA ($QE) 
		      (SETQ $QE (MEVAL '(($LISBRE) $LIST $I)))
		      ($APPEND (SIMPLIFY (MARRAYREF $QE 1))
			       ($REST (SIMPLIFY (MARRAYREF $QE 2)))))
		    '$QE))
		 EXPR)

 	(ARGS '$SRES '(NIL . 2))

 	(DEFPROP $SRES T TRANSLATED)

 	(ADD2LNC '$SRES $PROPS)

 	(MDEFPROP $SRES
		  ((LAMBDA)
		   ((MLIST) $LIST $I)
		   ((MPROG)
		    ((MLIST) $QE)
		    ((MSETQ) $QE (($LISBRE) $LIST $I))
		    ((MRETURN)
		     (($APPEND) (($QE ARRAY) 1) (($REST) (($QE ARRAY) 2))))))
		  MEXPR)

 	(ARGS '$SRES '(NIL . 2))

 	(ADD2LNC '(($SRES) $LIST $I) $FUNCTIONS)

 	(DEFPROP $CRUNCH00
		 (LAMBDA ($LIST) 
		   (SIMPLIFY ($CRUNCH0 (MEVAL '(($GETRED) $LIST)))))
		 EXPR)

 	(ARGS '$CRUNCH00 '(NIL . 1))

 	(DEFPROP $CRUNCH00 T TRANSLATED)

 	(ADD2LNC '$CRUNCH00 $PROPS)

 	(MDEFPROP
	 $CRUNCH00
	 (LAMBDA N 
	   (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
			     ($CRUNCH00 (CONS '(MLIST) (LISTIFY N))))
		 (T (SETQ NOEVALARGS NIL)
		    ($CRUNCH00 (CONS '(MLIST)
				     (MEVALARGS (LISTIFY N)))))))
	 T-MFEXPR)

 	(MDEFPROP $CRUNCH00
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   (($CRUNCH0) (($GETRED) $LIST)))
		  MEXPR)

 	(ARGS '$CRUNCH00 '(NIL . 1))

 	(ADD2LNC '(($CRUNCH00) ((MLIST) $LIST)) $FUNCTIONS)

 	(MDEFPROP $CRUNCH00 T MLEXPRP)

 	(DEFPROP $EPSUB
		 (LAMBDA ($LIST) 
		   (DIV (MEVAL '(($G) $G5 $LIST)) (MUL* 4 '$%I)))
		 EXPR)

 	(ARGS '$EPSUB '(NIL . 1))

 	(DEFPROP $EPSUB T TRANSLATED)

 	(ADD2LNC '$EPSUB $PROPS)

 	(MDEFPROP $EPSUB
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($EPSUB (CONS '(MLIST)
						    (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($EPSUB (CONS '(MLIST)
					   (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)

 	(MDEFPROP $EPSUB
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MQUOTIENT) (($G) $G5 $LIST) ((MTIMES) 4 $%I)))
		  MEXPR)

 	(ARGS '$EPSUB '(NIL . 1))

 	(ADD2LNC '(($EPSUB) ((MLIST) $LIST)) $FUNCTIONS)

 	(MDEFPROP $EPSUB T MLEXPRP)

 	(DEFPROP
	 $TCON
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($ANS $PO $I $CONS $INE $XP1 $XP2 $H1 $H2) 
	      (PROG NIL 
		    (COND ((IS ($FREEOF (TRD-MSYMEVAL $D) $EXP))
			   (RETURN (COND ((IS ($FREEOF (TRD-MSYMEVAL $EPS)
						       $EXP))
					  $EXP)
					 (T (SIMPLIFY ($EPSCON $EXP)))))))
		    (SETQ $INE (LIST '(MLIST) '((MLIST))))
		    (SETQ $CONS 1)
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((LIKE (SIMPLIFY ($INPART $EXP $I))
				 (TRD-MSYMEVAL $END))
			   (GO $ON)))
		    (COND ((NOT (LIKE (SIMPLIFY ($INPART $EXP $I 0))
				      (TRD-MSYMEVAL $D)))
			   (GO $NONE)))
		    (SETQ $PO (SIMPLIFY ($INPART $EXP $I)))
		    (SETQ $XP1 (SIMPLIFY ($INPART $PO 1)))
		    (SETQ $XP2 (SIMPLIFY ($INPART $PO 2)))
		    (SETQ $H1 ($MEMBER $XP1 (TRD-MSYMEVAL $IND)))
		    (SETQ $H2 ($MEMBER $XP2 (TRD-MSYMEVAL $IND)))
		    (COND ((AND (NOT (IS $H1)) (NOT (IS $H2))) (GO $IN0)))
		    (COND ((LIKE $XP1 $XP2) (GO $EQIND)))
		    (COND ((IS $H1)
			   (SETQ $INE ($APPEND $INE
					       (LIST '(MLIST)
						     (LIST '(MLIST)
							   $XP1
							   $XP2)))))
			  (T (SETQ $INE ($APPEND $INE
						 (LIST '(MLIST)
						       (LIST '(MLIST)
							     $XP2
							     $XP1))))))
		    (GO $LOOP)
	       $ON  (RETURN (MEVAL '(($GCONP) ((MTIMES) $CONS (($SER) $INE)))))
	       $IN0 (SETQ $CONS (MUL* $CONS (MEVAL '(($DP) $XP1 $XP2))))
		    (GO $LOOP)
	       $NONE(SETQ $ANS (SIMPLIFY ($INPART $EXP $I)))
		    (COND ((LIKE ($PART $ANS 0) '&^)
			   (SETQ $ANS (SIMPLIFY ($PCON $ANS))))
			  ((LIKE ($PART $ANS 0) (TRD-MSYMEVAL $D))
			   (SETQ $ANS (SIMPLIFY ($DCON $ANS)))))
		    (SETQ $CONS (MUL* $CONS $ANS))
		    (GO $LOOP)
	       $EQIND
		    (SETQ $CONS (MUL* $CONS (TRD-MSYMEVAL $N)))
		    (GO $LOOP)))
	    '$ANS
	    '$PO
	    '$I
	    '$CONS
	    '$INE
	    '$XP1
	    '$XP2
	    '$H1
	    '$H2))
	 EXPR)

 	(ARGS '$TCON '(NIL . 1))

 	(DEFPROP $TCON T TRANSLATED)

 	(ADD2LNC '$TCON $PROPS)

 	(MDEFPROP $TCON
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $ANS $PO $I $CONS $INE $XP1 $XP2 $H1 $H2)
		    ((MCOND)
		     (($FREEOF) $D $EXP)
		     ((MRETURN)
		      ((MCOND) (($FREEOF) $EPS $EXP) $EXP T (($EPSCON) $EXP)))
		     T
		     $FALSE)
		    ((MSETQ) $INE ((MLIST) ((MLIST))))
		    ((MSETQ) $CONS 1)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MEQUAL) (($INPART) $EXP $I) $END)
		     ((MGO) $ON)
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOTEQUAL) (($INPART) $EXP $I 0) $D)
		     ((MGO) $NONE)
		     T
		     $FALSE)
		    ((MSETQ) $PO (($INPART) $EXP $I))
		    ((MSETQ) $XP1 (($INPART) $PO 1))
		    ((MSETQ) $XP2 (($INPART) $PO 2))
		    ((MSETQ) $H1 (($MEMBER) $XP1 $IND))
		    ((MSETQ) $H2 (($MEMBER) $XP2 $IND))
		    ((MCOND)
		     ((MAND) ((MNOT) $H1) ((MNOT) $H2))
		     ((MGO) $IN0)
		     T
		     $FALSE)
		    ((MCOND) ((MEQUAL) $XP1 $XP2) ((MGO) $EQIND) T $FALSE)
		    ((MCOND)
		     $H1
		     ((MSETQ)
		      $INE
		      (($APPEND) $INE ((MLIST) ((MLIST) $XP1 $XP2))))
		     T
		     ((MSETQ)
		      $INE
		      (($APPEND) $INE ((MLIST) ((MLIST) $XP2 $XP1)))))
		    ((MGO) $LOOP)
		    $ON
		    ((MRETURN) (($GCONP) ((MTIMES) $CONS (($SER) $INE))))
		    $IN0
		    ((MSETQ) $CONS ((MTIMES) $CONS (($DP) $XP1 $XP2)))
		    ((MGO) $LOOP)
		    $NONE
		    ((MSETQ) $ANS (($INPART) $EXP $I))
		    ((MCOND)
		     ((MEQUAL) (($PART) $ANS 0) &^)
		     ((MSETQ) $ANS (($PCON) $ANS))
		     T
		     ((MCOND)
		      ((MEQUAL) (($PART) $ANS 0) $D)
		      ((MSETQ) $ANS (($DCON) $ANS))
		      T
		      $FALSE))
		    ((MSETQ) $CONS ((MTIMES) $CONS $ANS))
		    ((MGO) $LOOP)
		    $EQIND
		    ((MSETQ) $CONS ((MTIMES) $CONS $N))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$TCON '(NIL . 1))

 	(ADD2LNC '(($TCON) $EXP) $FUNCTIONS)

 	(DEFPROP
	 $PCON
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($B1 $B2 $H1 $H2) 
	      (PROG NIL 
		    (COND ((NOT (LIKE ($PART $EXP 1 0) (TRD-MSYMEVAL $D)))
			   (RETURN (COND ((IS ($FREEOF (TRD-MSYMEVAL $EPS)
						       $EXP))
					  $EXP)
					 (T (SIMPLIFY ($EPSCON $EXP)))))))
		    (SETQ $B1 ($PART $EXP 1 1))
		    (SETQ $B2 ($PART $EXP 1 2))
		    (SETQ $H1 ($MEMBER $B1 (TRD-MSYMEVAL $IND)))
		    (SETQ $H2 ($MEMBER $B2 (TRD-MSYMEVAL $IND)))
		    (COND ((AND (NOT (IS $H1)) (NOT (IS $H2))) (RETURN $EXP)))
		    (COND ((LIKE $B1 $B2) (RETURN (TRD-MSYMEVAL $N))))
		    (COND ((AND (IS $H1) (IS $H2)) (RETURN (TRD-MSYMEVAL $N))))
		    (COND ((IS $H1) (RETURN (MEVAL '(($D) $B2 $B2)))))
		    (RETURN (COND ((IS $H2)
				   (RETURN (MEVAL '(($D) $B1 $B1))))))))
	    '$B1
	    '$B2
	    '$H1
	    '$H2))
	 EXPR)

 	(ARGS '$PCON '(NIL . 1))

 	(DEFPROP $PCON T TRANSLATED)

 	(ADD2LNC '$PCON $PROPS)

 	(MDEFPROP $PCON
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $B1 $B2 $H1 $H2)
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $EXP 1 0) $D)
		     ((MRETURN)
		      ((MCOND) (($FREEOF) $EPS $EXP) $EXP T (($EPSCON) $EXP)))
		     T
		     $FALSE)
		    ((MSETQ) $B1 (($PART) $EXP 1 1))
		    ((MSETQ) $B2 (($PART) $EXP 1 2))
		    ((MSETQ) $H1 (($MEMBER) $B1 $IND))
		    ((MSETQ) $H2 (($MEMBER) $B2 $IND))
		    ((MCOND)
		     ((MAND) ((MNOT) $H1) ((MNOT) $H2))
		     ((MRETURN) $EXP)
		     T
		     $FALSE)
		    ((MCOND) ((MEQUAL) $B1 $B2) ((MRETURN) $N) T $FALSE)
		    ((MCOND) ((MAND) $H1 $H2) ((MRETURN) $N) T $FALSE)
		    ((MCOND) $H1 ((MRETURN) (($D) $B2 $B2)) T $FALSE)
		    ((MCOND) $H2 ((MRETURN) (($D) $B1 $B1)) T $FALSE)))
		  MEXPR)

 	(ARGS '$PCON '(NIL . 1))

 	(ADD2LNC '(($PCON) $EXP) $FUNCTIONS)

 	(DEFPROP
	 $CRUNCH0
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($JQ $LEN $BET $CONBIT $LENCON $IN $OUT $BEG $I $J $DEX
		     $NOTIN $RET) 
	      (PROG NIL 
		    (COND ((< ($LENGTH $LIST) 2)
			   (RETURN (SIMPLIFY ($G0 $LIST)))))
		    (SETQ $LEN ($LENGTH $LIST))
		    (COND ((AND (LIKE (TRD-MSYMEVAL $N) 4)
				(IS (TRD-MSYMEVAL $KAHAF))
				(IS (MGRP $LEN 3)))
			   (RETURN (MEVAL '(($KAHG) $LIST)))))
		    (SETQ $NOTIN '((MLIST)))
	       $STAR(SETQ $I 0)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((IS (MGRP $I $LEN)) (GO $RETF)))
		    (SETQ $JQ ($PART $LIST $I))
		    (SETQ $ICH (ADD* $I 1))
		    (COND
		     ((LIKE $JQ ($PART $LIST (TRD-MSYMEVAL $ICH)))
		      (RETURN
		       ((LAMBDA ($DPI) 
			  (PROG NIL 
				(SETQ $DPI (SIMPLIFY ($DI0 $JQ $JQ)))
				(RETURN
				 (COND
				  ((LIKE $DPI 0) (RETURN 0))
				  (T
				   (RETURN
				    (COND
				     ((NOT (LIKE (TRD-MSYMEVAL $ICH) 1))
				      (MUL*
				       $DPI
				       (SIMPLIFY
					($CRUNCH0 (SIMPLIFY ($SRES $LIST
								   $I))))))
				     (T
				      (MUL*
				       $DPI
				       (SIMPLIFY
					($G0
					 ($REST
					  ($APPEND
					   ($PART (MEVAL '(($LISBRE) $LIST $I))
						  1)
					   ($PART (MEVAL '(($LISBRE) $LIST $I))
						  2))))))))))))))
			'$DPI))))
		    (COND ((AND ($MEMBER $JQ (TRD-MSYMEVAL $IND))
				(NOT ($MEMBER $JQ $NOTIN)))
			   (GO $FND)))
		    (GO $LOOP)
	       $FND (SETQ $DEX $JQ)
		    (SETQ $J $I)
	       $T2  (SETQ $J (ADD* $J 1))
		    (COND ((IS (MGRP $J $LEN)) (GO $POUT)))
		    (COND ((LIKE ($PART $LIST $J) $DEX) (GO $DOIT))
			  (T (GO $T2)))
	       $DOIT(SETQ $BET (ADD* $J (SIMPLIFY (LIST '(MMINUS) $I))))
		    (COND ((IS (MGRP $BET 6)) (GO $BORE)))
		    (SETQ $CONBIT ($PART (TRD-MSYMEVAL $CONTAB) $BET))
		    (SETQ $LENCON ($LENGTH $CONBIT))
		    (SETQ $OUT ($REST $LIST $J))
		    (SETQ $BEG ($REST $LIST
				      (ADD* $I
					    -1
					    (SIMPLIFY (LIST '(MMINUS)
							    $LEN)))))
		    (SETQ $IN ($REST ($REST $LIST $I)
				     (ADD* $J
					   (SIMPLIFY (LIST '(MMINUS)
							   $LEN))
					   -1)))
		    (RETURN (DOSUM '((MTIMES)
				     (($FIRST)
				      (($PART)
				       $CONBIT
				       ((MPLUS) ((MTIMES) 2 $I) ((MMINUS) 1))))
				     ((MPROG)
				      ((MLIST) $NB)
				      ((MSETQ)
				       $NB
				       (($NEWL)
					$IN
					(($PART) $CONBIT ((MTIMES) 2 $I))
					$OUT))
				      ((MCOND)
				       ((MEQUAL) (($PART) $NB 1) 0)
				       ((MRETURN) 0)
				       T
				       ((MRETURN)
					((MTIMES)
					 (($PART) $NB 1)
					 (($CRUNCH0)
					  (($APPEND)
					   $BEG
					   (($PART) $NB 2))))))))
				   '$I
				   1
				   (DIV $LENCON 2)
				   T))
	       $POUT(SETQ $NOTIN ($APPEND $NOTIN
					  (LIST '(MLIST)
						($PART $LIST $I))))
		    (GO $STAR)
	       $BORE(SETQ $NOTIN ($APPEND $NOTIN (LIST '(MLIST) $DEX)))
		    (GO $STAR)
	       $RETF(RETURN (SIMPLIFY ($G0 $LIST)))))
	    '$JQ
	    '$LEN
	    '$BET
	    '$CONBIT
	    '$LENCON
	    '$IN
	    '$OUT
	    '$BEG
	    '$I
	    '$J
	    '$DEX
	    '$NOTIN
	    '$RET))
	 EXPR)

 	(ARGS '$CRUNCH0 '(NIL . 1))

 	(DEFPROP $CRUNCH0 T TRANSLATED)

 	(ADD2LNC '$CRUNCH0 $PROPS)

 	(MDEFPROP $CRUNCH0
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST)
		     $JQ
		     $LEN
		     $BET
		     $CONBIT
		     $LENCON
		     $IN
		     $OUT
		     $BEG
		     $I
		     $J
		     $DEX
		     $NOTIN
		     $RET)
		    ((MCOND)
		     ((MLESSP) (($LENGTH) $LIST) 2)
		     ((MRETURN) (($G0) $LIST))
		     T
		     $FALSE)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MCOND)
		     ((MAND) ((MEQUAL) $N 4) $KAHAF ((MGREATERP) $LEN 3))
		     ((MRETURN) (($KAHG) $LIST))
		     T
		     $FALSE)
		    ((MSETQ) $NOTIN ((MLIST)))
		    $STAR
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND) ((MGREATERP) $I $LEN) ((MGO) $RETF) T $FALSE)
		    ((MSETQ) $JQ (($PART) $LIST $I))
		    ((MSETQ) $ICH ((MPLUS) $I 1))
		    ((MCOND)
		     ((MEQUAL) $JQ (($PART) $LIST $ICH))
		     ((MRETURN)
		      ((MPROG)
		       ((MLIST) $DPI)
		       ((MSETQ) $DPI (($DI0) $JQ $JQ))
		       ((MCOND)
			((MEQUAL) $DPI 0)
			((MRETURN) 0)
			T
			((MRETURN)
			 ((MCOND)
			  ((MNOTEQUAL) $ICH 1)
			  ((MTIMES) $DPI (($CRUNCH0) (($SRES) $LIST $I)))
			  T
			  ((MTIMES)
			   $DPI
			   (($G0)
			    (($REST)
			     (($APPEND)
			      (($PART) (($LISBRE) $LIST $I) 1)
			      (($PART) (($LISBRE) $LIST $I) 2))))))))))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) $JQ $IND)
		      ((MNOT) (($MEMBER) $JQ $NOTIN)))
		     ((MGO) $FND)
		     T
		     $FALSE)
		    ((MGO) $LOOP)
		    $FND
		    ((MSETQ) $DEX $JQ)
		    ((MSETQ) $J $I)
		    $T2
		    ((MSETQ) $J ((MPLUS) $J 1))
		    ((MCOND) ((MGREATERP) $J $LEN) ((MGO) $POUT) T $FALSE)
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIST $J) $DEX)
		     ((MGO) $DOIT)
		     T
		     ((MGO) $T2))
		    $DOIT
		    ((MSETQ) $BET ((MPLUS) $J ((MMINUS) $I)))
		    ((MCOND) ((MGREATERP) $BET 6) ((MGO) $BORE) T $FALSE)
		    ((MSETQ) $CONBIT (($PART) $CONTAB $BET))
		    ((MSETQ) $LENCON (($LENGTH) $CONBIT))
		    ((MSETQ) $OUT (($REST) $LIST $J))
		    ((MSETQ)
		     $BEG
		     (($REST) $LIST ((MPLUS) $I ((MMINUS) 1) ((MMINUS) $LEN))))
		    ((MSETQ)
		     $IN
		     (($REST)
		      (($REST) $LIST $I)
		      ((MPLUS) $J ((MMINUS) $LEN) ((MMINUS) 1))))
		    ((MRETURN)
		     (($SUM)
		      ((MTIMES)
		       (($FIRST)
			(($PART)
			 $CONBIT
			 ((MPLUS) ((MTIMES) 2 $I) ((MMINUS) 1))))
		       ((MPROG)
			((MLIST) $NB)
			((MSETQ)
			 $NB
			 (($NEWL) $IN (($PART) $CONBIT ((MTIMES) 2 $I)) $OUT))
			((MCOND)
			 ((MEQUAL) (($PART) $NB 1) 0)
			 ((MRETURN) 0)
			 T
			 ((MRETURN)
			  ((MTIMES)
			   (($PART) $NB 1)
			   (($CRUNCH0) (($APPEND) $BEG (($PART) $NB 2))))))))
		      $I
		      1
		      ((MQUOTIENT) $LENCON 2)))
		    $POUT
		    ((MSETQ)
		     $NOTIN
		     (($APPEND) $NOTIN ((MLIST) (($PART) $LIST $I))))
		    ((MGO) $STAR)
		    $BORE
		    ((MSETQ) $NOTIN (($APPEND) $NOTIN ((MLIST) $DEX)))
		    ((MGO) $STAR)
		    $RETF
		    ((MRETURN) (($G0) $LIST))))
		  MEXPR)

 	(ARGS '$CRUNCH0 '(NIL . 1))

 	(ADD2LNC '(($CRUNCH0) $LIST) $FUNCTIONS)

 	(DEFPROP $DI0
		 (LAMBDA ($AOB $BOB) 
		   (COND ((NOT (IS ($FREEOF $AOB (TRD-MSYMEVAL $IND))))
			  (TRD-MSYMEVAL $N))
			 (T (MEVAL '(($SOR) $AOB $BOB)))))
		 EXPR)

 	(ARGS '$DI0 '(NIL . 2))

 	(DEFPROP $DI0 T TRANSLATED)

 	(ADD2LNC '$DI0 $PROPS)

 	(MDEFPROP $DI0
		  ((LAMBDA)
		   ((MLIST) $AOB $BOB)
		   ((MCOND)
		    ((MNOT) (($FREEOF) $AOB $IND))
		    $N
		    T
		    (($SOR) $AOB $BOB)))
		  MEXPR)

 	(ARGS '$DI0 '(NIL . 2))

 	(ADD2LNC '(($DI0) $AOB $BOB) $FUNCTIONS)

 	(DEFPROP
	 $CONP
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($INP) 
	      (PROG NIL 
		    (COND ((IS ($FREEOF (TRD-MSYMEVAL $D) $EXP))
			   (RETURN (COND ((IS ($FREEOF (TRD-MSYMEVAL $EPS)
						       $EXP))
					  $EXP)
					 (T (SIMPLIFY ($EPSCON $EXP)))))))
		    (SETQ $EXP (SIMPLIFY ($EXPAND $EXP)))
		    (SETQ $INP (SIMPLIFY ($INPART $EXP 0)))
		    (COND ((LIKE $INP '&*)
			   (RETURN (SIMPLIFY ($TCON $EXP)))))
		    (COND ((LIKE $INP (TRD-MSYMEVAL $D))
			   (RETURN (SIMPLIFY ($DCON $EXP)))))
		    (COND ((LIKE $INP '&^)
			   (RETURN (SIMPLIFY ($PCON $EXP)))))
		    (RETURN (MAP1 (GETOPR (TRD-MSYMEVAL $CON)) $EXP))))
	    '$INP))
	 EXPR)

 	(ARGS '$CONP '(NIL . 1))

 	(DEFPROP $CONP T TRANSLATED)

 	(ADD2LNC '$CONP $PROPS)

 	(MDEFPROP $CONP
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $INP)
		    ((MCOND)
		     (($FREEOF) $D $EXP)
		     ((MRETURN)
		      ((MCOND) (($FREEOF) $EPS $EXP) $EXP T (($EPSCON) $EXP)))
		     T
		     $FALSE)
		    ((MSETQ) $EXP (($EXPAND) $EXP))
		    ((MSETQ) $INP (($INPART) $EXP 0))
		    ((MCOND)
		     ((MEQUAL) $INP &*)
		     ((MRETURN) (($TCON) $EXP))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) $INP $D)
		     ((MRETURN) (($DCON) $EXP))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) $INP &^)
		     ((MRETURN) (($PCON) $EXP))
		     T
		     $FALSE)
		    ((MRETURN) (($MAP) $CON $EXP))))
		  MEXPR)

 	(ARGS '$CONP '(NIL . 1))

 	(ADD2LNC '(($CONP) $EXP) $FUNCTIONS)

 	(DEFPROP $CON
		 (LAMBDA ($EXP) (SIMPLIFY ($RATSIMP (SIMPLIFY ($CONP $EXP)))))
		 EXPR)

 	(ARGS '$CON '(NIL . 1))

 	(DEFPROP $CON T TRANSLATED)

 	(ADD2LNC '$CON $PROPS)

 	(MDEFPROP $CON
		  ((LAMBDA) ((MLIST) $EXP) (($RATSIMP) (($CONP) $EXP)))
		  MEXPR)

 	(ARGS '$CON '(NIL . 1))

 	(ADD2LNC '(($CON) $EXP) $FUNCTIONS)

 	(DEFPROP $EPSCON
		 (LAMBDA ($EXP) 
		   (COND ((AND (IS (TRD-MSYMEVAL $NTR))
			       (NOT (IS ($FREEOF (TRD-MSYMEVAL $G) $EXP))))
			  $EXP)
			 ((LIKE ($PART $EXP 0) (TRD-MSYMEVAL $EPS))
			  (MEVAL '(($EPSFIX) $EXP)))
			 (T (MEVAL '(($CGT)
				     (($COTR)
				      (($EV) $EXP ((MEQUAL) $EPS $EPSUB))))))))
		 EXPR)

 	(ARGS '$EPSCON '(NIL . 1))

 	(DEFPROP $EPSCON T TRANSLATED)

 	(ADD2LNC '$EPSCON $PROPS)

 	(MDEFPROP $EPSCON
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MCOND)
		    ((MAND) $NTR ((MNOT) (($FREEOF) $G $EXP)))
		    $EXP
		    T
		    ((MCOND)
		     ((MEQUAL) (($PART) $EXP 0) $EPS)
		     (($EPSFIX) $EXP)
		     T
		     (($CGT) (($COTR) (($EV) $EXP ((MEQUAL) $EPS $EPSUB)))))))
		  MEXPR)

 	(ARGS '$EPSCON '(NIL . 1))

 	(ADD2LNC '(($EPSCON) $EXP) $FUNCTIONS)

 	(DEFPROP
	 $CRUNCH
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($QW) 
	      (SETQ $QW (SIMPLIFY ($SUBSTITUTE (TRD-MSYMEVAL $CRUNCH00)
					       (TRD-MSYMEVAL $G)
					       $EXP)))
	      (SIMPLIFY ($RATSIMP (SIMPLIFY ($EV $QW)))))
	    '$QW))
	 EXPR)

 	(ARGS '$CRUNCH '(NIL . 1))

 	(DEFPROP $CRUNCH T TRANSLATED)

 	(ADD2LNC '$CRUNCH $PROPS)

 	(MDEFPROP $CRUNCH
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $QW)
		    ((MSETQ) $QW (($SUBSTITUTE) $CRUNCH00 $G $EXP))
		    ((MRETURN) (($RATSIMP) (($EV) $QW)))))
		  MEXPR)

 	(ARGS '$CRUNCH '(NIL . 1))

 	(ADD2LNC '(($CRUNCH) $EXP) $FUNCTIONS)

 	(DEFPROP $G00
		 (LAMBDA ($LIST) 
		   (COND ((LIKE $LIST '((MLIST))) 1)
			 (T (SIMPLIFY (MAPPLY (TRD-MSYMEVAL $G)
					      (CDR $LIST)
					      '$G)))))
		 EXPR)

 	(ARGS '$G00 '(NIL . 1))

 	(DEFPROP $G00 T TRANSLATED)

 	(ADD2LNC '$G00 $PROPS)

 	(MDEFPROP $G00
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MCOND)
		    ((MEQUAL) $LIST ((MLIST)))
		    1
		    T
		    (($APPLY) $G $LIST)))
		  MEXPR)

 	(ARGS '$G00 '(NIL . 1))

 	(ADD2LNC '(($G00) $LIST) $FUNCTIONS)

 	(DEFPROP $G0
		 (LAMBDA ($LIST) 
		   (COND ((LIKE $LIST '((MLIST))) 1)
			 ((IS (TRD-MSYMEVAL $COF)) (SIMPLIFY ($ROR $LIST)))
			 (T (SIMPLIFY (MAPPLY (TRD-MSYMEVAL $G)
					      (CDR $LIST)
					      '$G)))))
		 EXPR)

 	(ARGS '$G0 '(NIL . 1))

 	(DEFPROP $G0 T TRANSLATED)

 	(ADD2LNC '$G0 $PROPS)

 	(MDEFPROP $G0
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MCOND)
		    ((MEQUAL) $LIST ((MLIST)))
		    1
		    T
		    ((MCOND) $COF (($ROR) $LIST) T (($APPLY) $G $LIST))))
		  MEXPR)

 	(ARGS '$G0 '(NIL . 1))

 	(ADD2LNC '(($G0) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $GCONP
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($CONS $I $GBITS $SUBLIS $PI $H1 $H2) 
	      (PROG NIL 
		    (COND ((IS ($FREEOF (TRD-MSYMEVAL $G)
					(TRD-MSYMEVAL $EPS)
					$EXP))
			   (RETURN $EXP)))
		    (SETQ $EXP (SIMPLIFY ($EXPAND $EXP)))
		    (COND (($MEMBER ($PART $EXP 0)
				    (LIST '(MLIST)
					  '&+
					  '&//
					  '&-))
			   (RETURN (MAP1 (GETOPR (TRD-MSYMEVAL $GCONP))
					 $EXP))))
		    (COND ((NOT (LIKE ($PART $EXP 0) '&*))
			   (RETURN (COND ((IS ($FREEOF (TRD-MSYMEVAL $EPS)
						       $EXP))
					  $EXP)
					 (T (SIMPLIFY ($EPSCON $EXP)))))))
		    (SETQ $GBITS 1)
		    (SETQ $SUBLIS '((MLIST)))
		    (SETQ $CONS 1)
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((IS (MGRP $I ($LENGTH $EXP))) (GO $ON)))
		    (SETQ $PI ($PART $EXP $I))
		    (COND (($MEMBER ($PART $PI 0)
				    (LIST '(MLIST)
					  (TRD-MSYMEVAL $G)
					  (TRD-MSYMEVAL $EPS)))
			   (GO $GBIT)))
		    (COND ((NOT (LIKE ($PART $PI 0) (TRD-MSYMEVAL $D)))
			   (GO $NOTD)))
		    (SETQ $H1 (SIMPLIFY ($FREEOF ($PART $PI 1)
						 (TRD-MSYMEVAL $IND))))
		    (SETQ $H2 (SIMPLIFY ($FREEOF ($PART $PI 2)
						 (TRD-MSYMEVAL $IND))))
		    (COND ((AND (IS $H1) (IS $H2)) (GO $NOTD)))
		    (COND
		     ((AND (IS $H1) (NOT (IS $H2)))
		      (SETQ $SUBLIS
			    ($APPEND $SUBLIS
				     (LIST '(MLIST)
					   (MEVAL '((MEQUAL)
						    (($PART) $PI 2)
						    (($PART) $PI 1)))))))
		     (T (SETQ $SUBLIS
			      ($APPEND $SUBLIS
				       (LIST '(MLIST)
					     (MEVAL '((MEQUAL)
						      (($PART) $PI 1)
						      (($PART) $PI 2))))))))
		    (GO $LOOP)
	       $NOTD(SETQ $CONS (MUL* $CONS $PI))
		    (GO $LOOP)
	       $GBIT(SETQ $GBITS (MUL* $GBITS $PI))
		    (GO $LOOP)
	       $ON  (DO (($I 1 (+ 1 $I))) 
			((> $I ($LENGTH $SUBLIS)) '$DONE) 
		     (COND
		      ((IS ($FREEOF ($LHS (SIMPLIFY (MARRAYREF $SUBLIS $I)))
				    $GBITS))
		       (COND
			((OR (IS ($FREEOF ($RHS (SIMPLIFY (MARRAYREF $SUBLIS
								     $I)))
					  (TRD-MSYMEVAL $IND)))
			     (IS ($FREEOF ($RHS (SIMPLIFY (MARRAYREF $SUBLIS
								     $I)))
					  $GBITS)))
			 (SETQ $CONS
			       (MUL* (MEVAL '(($D)
					      (($LHS) (($SUBLIS ARRAY) $I))
					      (($RHS) (($SUBLIS ARRAY) $I))))
				     $CONS))
			 (SETQ $SUBLIS
			       (SIMPLIFY (PART1 (LIST (LIST '(MQUOTE SIMP)
							    '((MLIST)))
						      (LIST '(MQUOTE SIMP)
							    $SUBLIS)
						      (LIST '(MQUOTE SIMP)
							    $I))
						T
						NIL
						$INFLAG))))
			(T (SETQ $SUBLIS
				 (SIMPLIFY (PART1 '(((MEQUAL)
						     (($RHS)
						      (($SUBLIS ARRAY) $I))
						     (($LHS)
						      (($SUBLIS ARRAY) $I)))
						    $SUBLIS
						    $I)
						  T
						  NIL
						  $INFLAG))))))))
		    (SETQ $GBITS (SIMPLIFY (MAPPLY (TRD-MSYMEVAL $EV)
						   (LIST $GBITS $SUBLIS)
						   '$EV)))
		    (COND ((NOT (IS ($FREEOF (TRD-MSYMEVAL $EPS) $GBITS)))
			   (SETQ $GBITS (SIMPLIFY ($EPSCON $GBITS)))))
		    (RETURN (MUL* $GBITS $CONS))))
	    '$CONS
	    '$I
	    '$GBITS
	    '$SUBLIS
	    '$PI
	    '$H1
	    '$H2))
	 EXPR)

 	(ARGS '$GCONP '(NIL . 1))

 	(DEFPROP $GCONP T TRANSLATED)

 	(ADD2LNC '$GCONP $PROPS)

 	(MDEFPROP $GCONP
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $CONS $I $GBITS $SUBLIS $PI $H1 $H2)
		    ((MCOND)
		     (($FREEOF) $G $EPS $EXP)
		     ((MRETURN) $EXP)
		     T
		     $FALSE)
		    ((MSETQ) $EXP (($EXPAND) $EXP))
		    ((MCOND)
		     (($MEMBER) (($PART) $EXP 0) ((MLIST) &+ &// &-))
		     ((MRETURN) (($MAP) $GCONP $EXP))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $EXP 0) &*)
		     ((MRETURN)
		      ((MCOND) (($FREEOF) $EPS $EXP) $EXP T (($EPSCON) $EXP)))
		     T
		     $FALSE)
		    ((MSETQ) $GBITS 1)
		    ((MSETQ) $SUBLIS ((MLIST)))
		    ((MSETQ) $CONS 1)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I (($LENGTH) $EXP))
		     ((MGO) $ON)
		     T
		     $FALSE)
		    ((MSETQ) $PI (($PART) $EXP $I))
		    ((MCOND)
		     (($MEMBER) (($PART) $PI 0) ((MLIST) $G $EPS))
		     ((MGO) $GBIT)
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $PI 0) $D)
		     ((MGO) $NOTD)
		     T
		     $FALSE)
		    ((MSETQ) $H1 (($FREEOF) (($PART) $PI 1) $IND))
		    ((MSETQ) $H2 (($FREEOF) (($PART) $PI 2) $IND))
		    ((MCOND) ((MAND) $H1 $H2) ((MGO) $NOTD) T $FALSE)
		    ((MCOND)
		     ((MAND) $H1 ((MNOT) $H2))
		     ((MSETQ)
		      $SUBLIS
		      (($APPEND)
		       $SUBLIS
		       ((MLIST) ((MEQUAL) (($PART) $PI 2) (($PART) $PI 1)))))
		     T
		     ((MSETQ)
		      $SUBLIS
		      (($APPEND)
		       $SUBLIS
		       ((MLIST) ((MEQUAL) (($PART) $PI 1) (($PART) $PI 2))))))
		    ((MGO) $LOOP)
		    $NOTD
		    ((MSETQ) $CONS ((MTIMES) $CONS $PI))
		    ((MGO) $LOOP)
		    $GBIT
		    ((MSETQ) $GBITS ((MTIMES) $GBITS $PI))
		    ((MGO) $LOOP)
		    $ON
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH) $SUBLIS)
		     NIL
		     ((MCOND)
		      (($FREEOF) (($LHS) (($SUBLIS ARRAY) $I)) $GBITS)
		      ((MCOND)
		       ((MOR)
			(($FREEOF) (($RHS) (($SUBLIS ARRAY) $I)) $IND)
			(($FREEOF) (($RHS) (($SUBLIS ARRAY) $I)) $GBITS))
		       ((MPROG)
			((MLIST))
			((MSETQ)
			 $CONS
			 ((MTIMES)
			  (($D)
			   (($LHS) (($SUBLIS ARRAY) $I))
			   (($RHS) (($SUBLIS ARRAY) $I)))
			  $CONS))
			((MSETQ) $SUBLIS (($SUBSTPART) ((MLIST)) $SUBLIS $I)))
		       T
		       ((MSETQ)
			$SUBLIS
			(($SUBSTPART)
			 ((MEQUAL)
			  (($RHS) (($SUBLIS ARRAY) $I))
			  (($LHS) (($SUBLIS ARRAY) $I)))
			 $SUBLIS
			 $I)))
		      T
		      $FALSE))
		    ((MSETQ) $GBITS (($APPLY) $EV ((MLIST) $GBITS $SUBLIS)))
		    ((MCOND)
		     ((MNOT) (($FREEOF) $EPS $GBITS))
		     ((MSETQ) $GBITS (($EPSCON) $GBITS))
		     T
		     $FALSE)
		    ((MRETURN) ((MTIMES) $GBITS $CONS))))
		  MEXPR)

 	(ARGS '$GCONP '(NIL . 1))

 	(ADD2LNC '(($GCONP) $EXP) $FUNCTIONS)

 	(DEFPROP $KAHG (GAMKAH > DSK SHARE2) AUTOLOAD)

 	(ADD2LNC '$KAHG $PROPS)

	(SETQ IBASE 10.)


