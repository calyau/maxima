
	(SETQ IBASE 8.)

 	(SETQ SAVENO 5372)

 	(DEFPROP
	 $CHIS
	 (LAMBDA ($EXP) 
	   (PROG NIL 
		 (SETQ $EXP (SIMPLIFY ($EV $EXP
					   ((MEQUAL) $G $GR)
					   ((MEQUAL) $GT $GR))))
		 (SETQ $EXP (SIMPLIFY ($EXPAND $EXP)))
		 (RETURN (COND ((= ($NTERMS $EXP) 1)
				(RETURN (SIMPLIFY ($CHIS1 $EXP))))
			       (T (RETURN (SIMPLIFY ($MAP $CHIS1 $EXP))))))))
	 EXPR)

 	(ARGS '$CHIS '(NIL . 1))

 	(DEFPROP $CHIS T TRANSLATED)

 	(ADD2LNC '$CHIS $PROPS)

 	(MDEFPROP $CHIS
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ)
		     $EXP
		     (($EV) $EXP ((MEQUAL) $G $GR) ((MEQUAL) $GT $GR)))
		    ((MSETQ) $EXP (($EXPAND) $EXP))
		    ((MCOND)
		     ((MEQUAL) (($NTERMS) $EXP) 1)
		     ((MRETURN) (($CHIS1) $EXP))
		     T
		     ((MRETURN) (($MAP) $CHIS1 $EXP)))))
		  MEXPR)

 	(ARGS '$CHIS '(NIL . 1))

 	(ADD2LNC '(($CHIS) $EXP) $FUNCTIONS)

 	(MDEFPROP $GR T MLEXPRP)

 	(MDEFPROP $GR
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   (($GS00) (($GETRED) $LIST)))
		  MEXPR)

 	(ADD2LNC '(($GR) ((MLIST) $LIST)) $FUNCTIONS)

 	(DEFPROP
	 $CHIS1
	 (LAMBDA ($EXP) 
	   (COND
	    ((LIKE ($PART $EXP 0) (MEVAL1 '&-))
	     (SIMPLIFY (LIST '(MMINUS)
			     (SIMPLIFY ($CHIS1 (SIMPLIFY (LIST '(MMINUS)
							       $EXP)))))))
	    ((AND (LIKE ($PART $EXP 0) (MEVAL1 '&^))
		  (LIKE ($PART $EXP 2) 2))
	     (SIMPLIFY ($CHIS2 (MUL ($PART $EXP 1)
				    (SIMPLIFY ($APPLY (($PART) $EXP 1 0)
						      (($REVERSE)
						       ((MLIST)
							(($PART)
							 $EXP
							 1
							 1)))))))))
	    (T (DIV (SIMPLIFY ($CHIS2 ($NUM $EXP))) ($DENOM $EXP)))))
	 EXPR)

 	(ARGS '$CHIS1 '(NIL . 1))

 	(DEFPROP $CHIS1 T TRANSLATED)

 	(ADD2LNC '$CHIS1 $PROPS)

 	(MDEFPROP $CHIS1
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MCOND)
		    ((MEQUAL) (($PART) $EXP 0) &-)
		    ((MMINUS) (($CHIS1) ((MMINUS) $EXP)))
		    T
		    ((MCOND)
		     ((MAND)
		      ((MEQUAL) (($PART) $EXP 0) &^)
		      ((MEQUAL) (($PART) $EXP 2) 2))
		     (($CHIS2)
		      ((MTIMES)
		       (($PART) $EXP 1)
		       (($APPLY)
			(($PART) $EXP 1 0)
			(($REVERSE) ((MLIST) (($PART) $EXP 1 1))))))
		     T
		     ((MQUOTIENT) (($CHIS2) (($NUM) $EXP)) (($DENOM) $EXP)))))
		  MEXPR)

 	(ARGS '$CHIS1 '(NIL . 1))

 	(ADD2LNC '(($CHIS1) $EXP) $FUNCTIONS)

 	(DEFPROP $CHEG
		 (LAMBDA ($EXP) 
		   (COND ((NOT (IS ($FREEOF (MEVAL1 '$G) $EXP)))
			  (SIMPLIFY ($COTR $EXP)))
			 (T $EXP)))
		 EXPR)

 	(ARGS '$CHEG '(NIL . 1))

 	(DEFPROP $CHEG T TRANSLATED)

 	(ADD2LNC '$CHEG $PROPS)

 	(MDEFPROP $CHEG
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MCOND)
		    ((MNOT) (($FREEOF) $G $EXP))
		    (($COTR) $EXP)
		    T
		    $EXP))
		  MEXPR)

 	(ARGS '$CHEG '(NIL . 1))

 	(ADD2LNC '(($CHEG) $EXP) $FUNCTIONS)

 	(DEFPROP
	 $CHIS2
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($I $GLIS $CON) 
	      (PROG NIL 
		    (SETQ $CON 1)
		    (SETQ $GLIS (LIST '(MLIST)))
		    (DO (($I 1 (+ 1 $I))) 
			((> $I ($LENGTH $EXP)) '$DONE) 
		     (COND
		      ((NOT (LIKE ($PART $EXP $I 0) (MEVAL1 '$GS00)))
		       (SETQ $CON (MUL $CON ($PART $EXP $I))))
		      (T (SETQ $GLIS (SIMPLIFY ($APPEND $GLIS (LIST '(MLIST) ($PART $EXP $I 1))))))))
		    (RETURN
		     (COND
		      ((LIKE $GLIS (LIST '(MLIST))) (RETURN $CON))
		      (T
		       (RETURN
			(MUL
			 $CON
			 (SIMPLIFY ($CHEG (SIMPLIFY ($GFIXIT $GLIS)))))))))))
	    0
	    '$GLIS
	    '$CON))
	 EXPR)

 	(ARGS '$CHIS2 '(NIL . 1))

 	(DEFPROP $CHIS2 T TRANSLATED)

 	(ADD2LNC '$CHIS2 $PROPS)

 	(MDEFPROP $CHIS2
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $I $GLIS $CON)
		    ((MSETQ) $CON 1)
		    ((MSETQ) $GLIS ((MLIST)))
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH SIMP) $EXP)
		     NIL
		     ((MPROG)
		      ((MLIST))
		      ((MCOND)
		       ((MNOTEQUAL) (($PART) $EXP $I 0) $GS00)
		       ((MSETQ) $CON ((MTIMES) $CON (($PART) $EXP $I)))
		       T
		       ((MSETQ)
			$GLIS
			(($APPEND) $GLIS ((MLIST) (($PART) $EXP $I 1)))))))
		    ((MCOND)
		     ((MEQUAL) $GLIS ((MLIST)))
		     ((MRETURN) $CON)
		     T
		     ((MRETURN) ((MTIMES) $CON (($CHEG) (($GFIXIT) $GLIS)))))))
		  MEXPR)

 	(ARGS '$CHIS2 '(NIL . 1))

 	(ADD2LNC '(($CHIS2) $EXP) $FUNCTIONS)

 	(DEFPROP
	 $GFIXIT
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($CON $B1 $B2 $I $J $BIT) 
	      (PROG NIL 
		    (SETQ $B1 ($PART $LIST 1))
		    (SETQ $B2 ($PART $LIST 2))
		    (COND ((NOT (IS ($FREEOF (MEVAL1 '$G5)
					     (MEVAL1 '$LHP)
					     (MEVAL1 '$RHP)
					     (LIST '(MLIST) $B1 $B2))))
			   (GO $NOZ)))
		    (COND ((OR (IS ($ODDP ($LENGTH $B1)))
			       (IS ($ODDP ($LENGTH $B2))))
			   (RETURN 0)))
	       $NOZ (COND ((= ($LENGTH $LIST) 2) (SETQ $CON 1))
			  (T (SETQ $CON (MEVAL '(($PRODUCT)
						 (($G) (($PART) $LIST $I))
						 $I
						 3
						 (($LENGTH) $LIST))))))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (+ $I 1))
		    (COND ((> $I ($LENGTH $B1))
			   (RETURN (MUL $CON
					(SIMPLIFY ($APPLY $GT $B1))
					(SIMPLIFY ($APPLY $GT $B2))))))
		    (SETQ $BIT ($PART $B1 $I))
		    (COND ((AND ($MEMBER $BIT (MEVAL1 '$IND))
				($MEMBER $BIT $B2)
				(= ($LENGTH (SIMPLIFY ($DELETE $BIT $B1)))
				   (+ ($LENGTH $B1) -1)))
			   (GO $ON))
			  (T (GO $LOOP)))
	       $ON  (SETQ $B1
			  ($REST (MEVAL '(($CYC)
					  $B1
					  ((MPLUS) $I ((MMINUS) 1))))))
		    (SETQ $J 0)
	       $LOP2(SETQ $J (ADD $J 1))
		    (COND ((NOT (LIKE ($PART $B2 $J) $BIT)) (GO $LOP2)))
		    (SETQ $B2
			  ($REST (MEVAL '(($CYC)
					  $B2
					  ((MPLUS) $J ((MMINUS) 1))))))
		    (RETURN (MUL 2
				 $CON
				 (ADD (SIMPLIFY ($APPLY $GT
							(($APPEND) $B1 $B2)))
				      (SIMPLIFY ($APPLY $GT
							(($APPEND)
							 (($REVERSE) $B1)
							 $B2))))))))
	    '$CON
	    '$B1
	    '$B2
	    0
	    '$J
	    '$BIT))
	 EXPR)

 	(ARGS '$GFIXIT '(NIL . 1))

 	(DEFPROP $GFIXIT T TRANSLATED)

 	(ADD2LNC '$GFIXIT $PROPS)

 	(MDEFPROP $GFIXIT
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $CON $B1 $B2 $I $J $BIT)
		    ((MSETQ) $B1 (($PART) $LIST 1))
		    ((MSETQ) $B2 (($PART) $LIST 2))
		    ((MCOND)
		     ((MNOT) (($FREEOF) $G5 $LHP $RHP ((MLIST) $B1 $B2)))
		     ((MGO) $NOZ)
		     T
		     $FALSE)
		    ((MCOND)
		     ((MOR)
		      (($ODDP) (($LENGTH) $B1))
		      (($ODDP) (($LENGTH) $B2)))
		     ((MRETURN) 0)
		     T
		     $FALSE)
		    $NOZ
		    ((MCOND)
		     ((MEQUAL) (($LENGTH) $LIST) 2)
		     ((MSETQ) $CON 1)
		     T
		     ((MSETQ)
		      $CON
		      (($PRODUCT)
		       (($G) (($PART) $LIST $I))
		       $I
		       3
		       (($LENGTH) $LIST))))
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I (($LENGTH) $B1))
		     ((MRETURN)
		      ((MTIMES) $CON (($APPLY) $GT $B1) (($APPLY) $GT $B2)))
		     T
		     $FALSE)
		    ((MSETQ) $BIT (($PART) $B1 $I))
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) $BIT $IND)
		      (($MEMBER) $BIT $B2)
		      ((MEQUAL)
		       (($LENGTH) (($DELETE) $BIT $B1))
		       ((MPLUS) (($LENGTH) $B1) ((MMINUS) 1))))
		     ((MGO) $ON)
		     T
		     ((MGO) $LOOP))
		    $ON
		    ((MSETQ)
		     $B1
		     (($REST) (($CYC) $B1 ((MPLUS) $I ((MMINUS) 1)))))
		    ((MSETQ) $J 0)
		    $LOP2
		    ((MSETQ) $J ((MPLUS) $J 1))
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $B2 $J) $BIT)
		     ((MGO) $LOP2)
		     T
		     $FALSE)
		    ((MSETQ)
		     $B2
		     (($REST) (($CYC) $B2 ((MPLUS) $J ((MMINUS) 1)))))
		    ((MRETURN)
		     ((MTIMES)
		      2
		      $CON
		      ((MPLUS)
		       (($APPLY) $GT (($APPEND) $B1 $B2))
		       (($APPLY) $GT (($APPEND) (($REVERSE) $B1) $B2)))))))
		  MEXPR)

 	(ARGS '$GFIXIT '(NIL . 1))

 	(ADD2LNC '(($GFIXIT) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $COTR
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($ODDKILL) 
	      (SETQ $ODDKILL T)
	      (COND ((NOT (LIKE (MEVAL1 '$N) 4)) $EXP)
		    (T (SIMPLIFY ($CHIS (MEVAL '(($ZFIX)
						 (($EV)
						  $EXP
						  ((MEQUAL) $GT $G)))))))))
	    '$ODDKILL))
	 EXPR)

 	(ARGS '$COTR '(NIL . 1))

 	(DEFPROP $COTR T TRANSLATED)

 	(ADD2LNC '$COTR $PROPS)

 	(MDEFPROP $COTR
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $ODDKILL)
		    ((MSETQ) $ODDKILL $TRUE)
		    ((MCOND)
		     ((MNOTEQUAL) $N 4)
		     $EXP
		     T
		     (($CHIS) (($ZFIX) (($EV) $EXP ((MEQUAL) $GT $G)))))))
		  MEXPR)

 	(ARGS '$COTR '(NIL . 1))

 	(ADD2LNC '(($COTR) $EXP) $FUNCTIONS)

	(SETQ IBASE 10.)


