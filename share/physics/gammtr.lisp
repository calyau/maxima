
	(SETQ IBASE 8.)

 	(SETQ SAVENO 5372)

 	(DSKSETQ $MTRFLAG NIL)

 	(DEFPROP $SOR
		 (LAMBDA ($AOBS $BOBS) 
		   (COND ((IS (MEVAL1 '$MTRFLAG))
			  (SIMPLIFY ($SORD $AOBS $BOBS)))
			 (T (SIMPLIFY ($SORO $AOBS $BOBS)))))
		 EXPR)

 	(ARGS '$SOR '(NIL . 2))

 	(DEFPROP $SOR T TRANSLATED)

 	(ADD2LNC '$SOR $PROPS)

 	(MDEFPROP $SOR
		  ((LAMBDA)
		   ((MLIST) $AOBS $BOBS)
		   ((MCOND)
		    $MTRFLAG
		    (($SORD) $AOBS $BOBS)
		    T
		    (($SORO) $AOBS $BOBS)))
		  MEXPR)

 	(ARGS '$SOR '(NIL . 2))

 	(ADD2LNC '(($SOR) $AOBS $BOBS) $FUNCTIONS)

 	(DEFPROP
	 $SORD
	 (LAMBDA ($AOBS $BOBS) 
	   ((LAMBDA ($AY $BY $H2 $H1) 
	      (PROG NIL 
		    (SETQ $H1 (COND ((LIKE ($PART $AOBS 0)
					   (MEVAL1 '$MTER))
				     T)))
		    (SETQ $H2 (COND ((LIKE ($PART $BOBS 0)
					   (MEVAL1 '$MTER))
				     T)))
		    (COND ((AND (IS $H1) (NOT (IS $H2)))
			   (RETURN (SIMPLIFY ($SORO ($PART (MEVAL1 '$SUBS)
							   ($PART $AOBS 1)
							   1)
						    $BOBS)))))
		    (COND ((AND (IS $H2) (NOT (IS $H1)))
			   (RETURN (SIMPLIFY ($SORO ($PART (MEVAL1 '$SUBS)
							   ($PART $BOBS 1)
							   1)
						    $AOBS)))))
		    (COND ((AND (NOT (IS $H2)) (NOT (IS $H1)))
			   (RETURN (SIMPLIFY ($SORO $AOBS $BOBS)))))
		    (SETQ $AY ($PART $AOBS 1))
		    (SETQ $BY ($PART $BOBS 1))
		    (RETURN
		     (ADD
		      (SIMPLIFY ($SORO ($PART (MEVAL1 '$SUBS) $AY 1)
				       ($PART (MEVAL1 '$SUBS) $BY 1)))
		      (MUL
		       (POWER -1
			      (ADD (MEVAL1 '(($VARS ARRAY) $AY))
				   (SIMPLIFY (LIST '(MMINUS)
						   (MEVAL1 '(($VARS ARRAY)
							     $BY))))
				   -1))
		       ($PART (MEVAL1 '$SUBS) $AY 2)
		       ($PART (MEVAL1 '$SUBS) $BY 2))))))
	    '$AY
	    '$BY
	    '$H2
	    '$H1))
	 EXPR)

 	(ARGS '$SORD '(NIL . 2))

 	(DEFPROP $SORD T TRANSLATED)

 	(ADD2LNC '$SORD $PROPS)

 	(MDEFPROP $SORD
		  ((LAMBDA)
		   ((MLIST) $AOBS $BOBS)
		   ((MPROG)
		    ((MLIST) $AY $BY $H2 $H1)
		    ((MSETQ)
		     $H1
		     ((MCOND)
		      ((MEQUAL) (($PART) $AOBS 0) $MTER)
		      $TRUE
		      T
		      $FALSE))
		    ((MSETQ)
		     $H2
		     ((MCOND)
		      ((MEQUAL) (($PART) $BOBS 0) $MTER)
		      $TRUE
		      T
		      $FALSE))
		    ((MCOND)
		     ((MAND) $H1 ((MNOT) $H2))
		     ((MRETURN)
		      (($SORO) (($PART) $SUBS (($PART) $AOBS 1) 1) $BOBS))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND) $H2 ((MNOT) $H1))
		     ((MRETURN)
		      (($SORO) (($PART) $SUBS (($PART) $BOBS 1) 1) $AOBS))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND) ((MNOT) $H2) ((MNOT) $H1))
		     ((MRETURN) (($SORO) $AOBS $BOBS))
		     T
		     $FALSE)
		    ((MSETQ) $AY (($PART) $AOBS 1))
		    ((MSETQ) $BY (($PART) $BOBS 1))
		    ((MRETURN)
		     ((MPLUS)
		      (($SORO) (($PART) $SUBS $AY 1) (($PART) $SUBS $BY 1))
		      ((MTIMES)
		       ((MEXPT)
			((MMINUS) 1)
			((MPLUS)
			 (($VARS ARRAY) $AY)
			 ((MMINUS) (($VARS ARRAY) $BY))
			 ((MMINUS) 1)))
		       (($PART) $SUBS $AY 2)
		       (($PART) $SUBS $BY 2))))))
		  MEXPR)

 	(ARGS '$SORD '(NIL . 2))

 	(ADD2LNC '(($SORD) $AOBS $BOBS) $FUNCTIONS)

 	(DEFPROP $ZMAK4
		 (LAMBDA ($LIST) 
		   ((LAMBDA ($MTRFLAG $VARS $SUBS) 
		      (PROG NIL 
			    (SETQ $MTRFLAG NIL)
			    (COND ((NOT (IS ($OKTRICK $LIST)))
				   (RETURN (MEVAL '(($ZMAK) $LIST)))))
			    (SETQ $MTRFLAG T)
			    (SETQ $LIST (SIMPLIFY ($ZDREM $LIST)))
			    (RETURN (MUL ($PART $LIST 2)
					 (MEVAL '(($TR0)
						  (($MTERC)
						   (($PART) $LIST 1))))))))
		    '$MTRFLAG
		    '$VARS
		    '$SUBS))
		 EXPR)

 	(ARGS '$ZMAK4 '(NIL . 1))

 	(DEFPROP $ZMAK4 T TRANSLATED)

 	(ADD2LNC '$ZMAK4 $PROPS)

 	(MDEFPROP $ZMAK4
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $MTRFLAG $VARS $SUBS)
		    ((MSETQ) $MTRFLAG $FALSE)
		    ((MCOND)
		     ((MNOT) (($OKTRICK) $LIST))
		     ((MRETURN) (($ZMAK) $LIST))
		     T
		     $FALSE)
		    ((MSETQ) $MTRFLAG $TRUE)
		    ((MSETQ) $LIST (($ZDREM) $LIST))
		    ((MRETURN)
		     ((MTIMES)
		      (($PART) $LIST 2)
		      (($TR0) (($MTERC) (($PART) $LIST 1)))))))
		  MEXPR)

 	(ARGS '$ZMAK4 '(NIL . 1))

 	(ADD2LNC '(($ZMAK4) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $ZDREM
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($CON $PIT1 $PIT2) 
	      (PROG NIL 
		    (COND ((IS ($FREEOF (MEVAL1 '$ZD) $LIST))
			   (RETURN (LIST '(MLIST) $LIST 1))))
		    (SETQ $CON 1)
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD (MEVAL1 '$I) 1))
		    (COND ((IS (MGRP (MEVAL1 '$I) ($LENGTH $LIST)))
			   (RETURN (LIST '(MLIST) $LIST $CON))))
		    (COND ((NOT (LIKE ($PART $LIST (MEVAL1 '$I) 0)
				      (MEVAL1 '$ZD)))
			   (GO $LOOP)))
		    (SETQ $PIT1 ($PART $LIST (MEVAL1 '$I) 1))
		    (SETQ $PIT2 ($PART $LIST (MEVAL1 '$I) 2))
		    (SETQ $CON (DIV $CON
				    (ADD (MEVAL '(($D) $PIT1 $PIT1))
					 (SIMPLIFY (LIST '(MMINUS)
							 (MUL $PIT2 $PIT2))))))
		    (SETQ $LIST (SIMPLIFY ($SUBSTPART $ZN $LIST $I 0)))
		    (GO $LOOP)))
	    '$CON
	    '$PIT1
	    '$PIT2))
	 EXPR)

 	(ARGS '$ZDREM '(NIL . 1))

 	(DEFPROP $ZDREM T TRANSLATED)

 	(ADD2LNC '$ZDREM $PROPS)

 	(MDEFPROP $ZDREM
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $CON $PIT1 $PIT2)
		    ((MCOND)
		     (($FREEOF) $ZD $LIST)
		     ((MRETURN) ((MLIST) $LIST 1))
		     T
		     $FALSE)
		    ((MSETQ) $CON 1)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I (($LENGTH) $LIST))
		     ((MRETURN) ((MLIST) $LIST $CON))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $LIST $I 0) $ZD)
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MSETQ) $PIT1 (($PART) $LIST $I 1))
		    ((MSETQ) $PIT2 (($PART) $LIST $I 2))
		    ((MSETQ)
		     $CON
		     ((MQUOTIENT)
		      $CON
		      ((MPLUS)
		       (($D) $PIT1 $PIT1)
		       ((MMINUS) ((MTIMES) $PIT2 $PIT2)))))
		    ((MSETQ) $LIST (($SUBSTPART) $ZN $LIST $I 0))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$ZDREM '(NIL . 1))

 	(ADD2LNC '(($ZDREM) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $MTERC
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($JCT $I $NEW $PIT) 
	      (PROG NIL 
		    (SETQ $JCT 0)
		    (SETQ $VARS (LIST '(MLIST)))
		    (SETQ $SUBS (LIST '(MLIST)))
		    (SETQ $NEW (LIST '(MLIST)))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((IS (MGRP $I ($LENGTH $LIST))) (RETURN $NEW)))
		    (SETQ $PIT ($PART $LIST $I))
		    (COND ((NOT (LIKE ($PART $PIT 0) (MEVAL1 '$ZN)))
			   (GO $NOTZ)))
		    (SETQ $JCT (ADD $JCT 1))
		    (SETQ $SUBS
			  (SIMPLIFY ($APPEND (MEVAL1 '$SUBS)
					     (LIST '(MLIST)
						   (LIST '(MLIST)
							 ($PART $PIT 1)
							 ($PART $PIT 2))))))
		    (SETQ $VARS (SIMPLIFY ($APPEND (MEVAL1 '$VARS)
						   (LIST '(MLIST) $I))))
		    (SETQ $NEW
			  (SIMPLIFY ($APPEND $NEW
					     (LIST '(MLIST)
						   (MEVAL1 '(($MTER ARRAY)
							     $JCT))))))
		    (GO $LOOP)
	       $NOTZ(SETQ $NEW (SIMPLIFY ($APPEND $NEW
						  (LIST '(MLIST) $PIT))))
		    (GO $LOOP)))
	    '$JCT
	    '$I
	    '$NEW
	    '$PIT))
	 EXPR)

 	(ARGS '$MTERC '(NIL . 1))

 	(DEFPROP $MTERC T TRANSLATED)

 	(ADD2LNC '$MTERC $PROPS)

 	(MDEFPROP $MTERC
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $JCT $I $NEW $PIT)
		    ((MSETQ) $JCT 0)
		    ((MSETQ) $VARS ((MLIST)))
		    ((MSETQ) $SUBS ((MLIST)))
		    ((MSETQ) $NEW ((MLIST)))
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I (($LENGTH) $LIST))
		     ((MRETURN) $NEW)
		     T
		     $FALSE)
		    ((MSETQ) $PIT (($PART) $LIST $I))
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $PIT 0) $ZN)
		     ((MGO) $NOTZ)
		     T
		     $FALSE)
		    ((MSETQ) $JCT ((MPLUS) $JCT 1))
		    ((MSETQ)
		     $SUBS
		     (($APPEND)
		      $SUBS
		      ((MLIST) ((MLIST) (($PART) $PIT 1) (($PART) $PIT 2)))))
		    ((MSETQ) $VARS (($APPEND) $VARS ((MLIST) $I)))
		    ((MSETQ)
		     $NEW
		     (($APPEND) $NEW ((MLIST) (($MTER ARRAY) $JCT))))
		    ((MGO) $LOOP)
		    $NOTZ
		    ((MSETQ) $NEW (($APPEND) $NEW ((MLIST) $PIT)))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$MTERC '(NIL . 1))

 	(ADD2LNC '(($MTERC) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $OKTRICK
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($LEN $I $ZLAST $ZFIRST $FLZ) 
	      (PROG NIL 
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $FLZ 0)
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND
		     ((IS (MGRP $I ($LENGTH $LIST)))
		      (RETURN
		       (COND ((IS ($EVENP (ADD $LEN
					       $ZLAST
					       (SIMPLIFY (LIST '(MMINUS)
							       $ZFIRST)))))
			      NIL)
			     (T)))))
		    (COND ((NOT ($MEMBER ($PART $LIST $I 0)
					 (LIST '(MLIST)
					       (MEVAL1 '$ZN)
					       (MEVAL1 '$ZA)
					       (MEVAL1 '$ZD))))
			   (GO $LOOP)))
		    (COND ((LIKE $FLZ 0) (SETQ $ZFIRST $I) (SETQ $FLZ 1)))
		    (COND ((IS ($EVENP (ADD $LEN
					    $I
					    (SIMPLIFY (LIST '(MMINUS)
							    $ZLAST)))))
			   (RETURN NIL)))
		    (SETQ $ZLAST $I)
		    (GO $LOOP)))
	    '$LEN
	    '$I
	    '$ZLAST
	    '$ZFIRST
	    '$FLZ))
	 EXPR)

 	(ARGS '$OKTRICK '(NIL . 1))

 	(DEFPROP $OKTRICK T TRANSLATED)

 	(ADD2LNC '$OKTRICK $PROPS)

 	(MDEFPROP $OKTRICK
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $LEN $I $ZLAST $ZFIRST $FLZ)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $FLZ 0)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I (($LENGTH) $LIST))
		     ((MRETURN)
		      ((MCOND)
		       (($EVENP) ((MPLUS) $LEN $ZLAST ((MMINUS) $ZFIRST)))
		       $FALSE
		       T
		       $TRUE))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOT)
		      (($MEMBER) (($PART) $LIST $I 0) ((MLIST) $ZN $ZA $ZD)))
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) $FLZ 0)
		     ((MPROG) ((MLIST)) ((MSETQ) $ZFIRST $I) ((MSETQ) $FLZ 1))
		     T
		     $FALSE)
		    ((MCOND)
		     (($EVENP) ((MPLUS) $LEN $I ((MMINUS) $ZLAST)))
		     ((MRETURN) $FALSE)
		     T
		     $FALSE)
		    ((MSETQ) $ZLAST $I)
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$OKTRICK '(NIL . 1))

 	(ADD2LNC '(($OKTRICK) $LIST) $FUNCTIONS)

 	(DEFPROP $SORO
		 (LAMBDA ($AYQ $BYQ) 
		   (COND ((AND (IS (MEVAL1 '$DEF))
			       (NOT (IS ($FREEOF (MEVAL1 '&+)
						 (MEVAL1 '&*)
						 (MEVAL1 '&//)
						 (MEVAL1 '&+)
						 (LIST '(MLIST)
						       $AYQ
						       $BYQ)))))
			  (MEVAL '(($D0) $AYQ $BYQ)))
			 ((IS (MEVAL1 '$DOF))
			  (MEVAL '(($DP) $AYQ $BYQ)))
			 (T (MEVAL '(($D) $A $B)))))
		 EXPR)

 	(ARGS '$SORO '(NIL . 2))

 	(DEFPROP $SORO T TRANSLATED)

 	(ADD2LNC '$SORO $PROPS)

 	(MDEFPROP $SORO
		  ((LAMBDA)
		   ((MLIST) $AYQ $BYQ)
		   ((MCOND)
		    ((MAND)
		     $DEF
		     ((MNOT) (($FREEOF) &+ &* &// &+ ((MLIST) $AYQ $BYQ))))
		    (($D0) $AYQ $BYQ)
		    T
		    ((MCOND) $DOF (($DP) $AYQ $BYQ) T (($D) $A $B))))
		  MEXPR)

 	(ARGS '$SORO '(NIL . 2))

 	(ADD2LNC '(($SORO) $AYQ $BYQ) $FUNCTIONS)

 	(ADD2LNC '$MTRFLAG $VALUES)

	(SETQ IBASE 10.)


