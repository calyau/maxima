
	(SETQ IBASE 8.)
 	(SETQ SAVENO 5372)
 	
(DEFPROP
	 $REM5
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($I $J $P1 $P2 $P3 $FAC) 
	      (PROG NIL 
		    (SETQ $FAC 1)
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((IS (MGRP $I (MEVAL1 '$LEN)))
			   (RETURN (LIST '(MLIST) $FAC $LIST))))
		    (COND ((LIKE ($PART $LIST $I) (MEVAL1 '$G5))
			   (GO $FUR)))
		    (GO $LOOP)
	       $FUR (SETQ $J $I)
	       $L2  (SETQ $J (ADD $J 1))
		    (COND ((IS (MGRP $J (MEVAL1 '$LEN)))
			   (RETURN (LIST '(MLIST)
					 $FAC
					 (MEVAL '(($CYC) $LIST $I))))))
		    (COND ((LIKE ($PART $LIST $J) (MEVAL1 '$G5))
			   (GO $DEAL)))
		    (GO $L2)
	       $DEAL(SETQ $FAC
			  (MUL (POWER -1
				      (ADD $J
					   (SIMPLIFY (LIST '(MMINUS)
							   $I))
					   1))
			       $FAC))
		    (SETQ $P1
			  ($REST $LIST
				 (ADD $I
				      -1
				      (SIMPLIFY (LIST '(MMINUS)
						      (MEVAL1 '$LEN))))))
		    (SETQ $P3 ($REST $LIST $J))
		    (SETQ $P2
			  ($REST ($REST $LIST $I)
				 (ADD $J
				      -1
				      (SIMPLIFY (LIST '(MMINUS)
						      (MEVAL1 '$LEN))))))
		    (SETQ $LIST (SIMPLIFY ($APPEND $P1 $P2 $P3)))
		    (GO $LOOP)))
	    '$I
	    '$J
	    '$P1
	    '$P2
	    '$P3
	    '$FAC))
	 EXPR)

 	(ARGS '$REM5 '(NIL . 1))

 	(DEFPROP $REM5 T TRANSLATED)

 	(ADD2LNC '$REM5 $PROPS)

 	(MDEFPROP $REM5
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $I $J $P1 $P2 $P3 $FAC)
		    ((MSETQ) $FAC 1)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I $LEN)
		     ((MRETURN) ((MLIST) $FAC $LIST))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIST $I) $G5)
		     ((MGO) $FUR)
		     T
		     $FALSE)
		    ((MGO) $LOOP)
		    $FUR
		    ((MSETQ) $J $I)
		    $L2
		    ((MSETQ) $J ((MPLUS) $J 1))
		    ((MCOND)
		     ((MGREATERP) $J $LEN)
		     ((MRETURN) ((MLIST) $FAC (($CYC) $LIST $I)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIST $J) $G5)
		     ((MGO) $DEAL)
		     T
		     $FALSE)
		    ((MGO) $L2)
		    $DEAL
		    ((MSETQ)
		     $FAC
		     ((MTIMES)
		      ((MEXPT) ((MMINUS) 1) ((MPLUS) $J ((MMINUS) $I) 1))
		      $FAC))
		    ((MSETQ)
		     $P1
		     (($REST) $LIST ((MPLUS) $I ((MMINUS) 1) ((MMINUS) $LEN))))
		    ((MSETQ) $P3 (($REST) $LIST $J))
		    ((MSETQ)
		     $P2
		     (($REST)
		      (($REST) $LIST $I)
		      ((MPLUS) $J ((MMINUS) 1) ((MMINUS) $LEN))))
		    ((MSETQ) $LIST (($APPEND) $P1 $P2 $P3))
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ARGS '$REM5 '(NIL . 1))

 	(ADD2LNC '(($REM5) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $REAR5
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($I $LEN $NOTIN $P) 
	      (PROG NIL 
		    (SETQ $NOTIN (LIST '(MLIST) ($FIRST $LIST)))
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $I 1)
	       $LOOP(SETQ $I (ADD $I 1))
		    (COND ((IS (MGRP $I $LEN)) (GO $LO1)))
		    (SETQ $P ($PART $LIST $I))
		    (COND ((AND ($MEMBER $P (MEVAL1 '$IND))
				(IS ($FREEOF $P $NOTIN)))
			   (SETQ $LIST
				 (MEVAL '(($CYC)
					  $LIST
					  ((MPLUS) $I ((MMINUS) 1)))))
			   (SIMPLIFY ($APPEND $NOTIN
					      (LIST '(MLIST) $P)))))
		    (COND ((NOT (IS (MGRP (MEVAL '(($CONUM) $LIST $P))
					  6)))
			   (RETURN (MEVAL '(($LISCUT) $LIST)))))
		    (GO $LOOP)
	       $LO1 (RETURN
		     (SIMPLIFY ($ERROR (MEVAL1 '|&REAR5 DIED IN PAIN|)
				       $LIST)))))
	    '$I
	    '$LEN
	    '$NOTIN
	    '$P))
	 EXPR)

 	(ARGS '$REAR5 '(NIL . 1))

 	(DEFPROP $REAR5 T TRANSLATED)

 	(ADD2LNC '$REAR5 $PROPS)

 	(MDEFPROP $REAR5
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $I $LEN $NOTIN $P)
		    ((MSETQ) $NOTIN ((MLIST) (($FIRST) $LIST)))
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $I 1)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND) ((MGREATERP) $I $LEN) ((MGO) $LO1) T $FALSE)
		    ((MSETQ) $P (($PART) $LIST $I))
		    ((MCOND)
		     ((MAND) (($MEMBER) $P $IND) (($FREEOF) $P $NOTIN))
		     ((DOLIST)
		      ((MSETQ) $LIST (($CYC) $LIST ((MPLUS) $I ((MMINUS) 1))))
		      (($APPEND) $NOTIN ((MLIST) $P)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MLEQP) (($CONUM) $LIST $P) 6)
		     ((MRETURN) (($LISCUT) $LIST))
		     T
		     $FALSE)
		    ((MGO) $LOOP)
		    $LO1
		    (($ERROR) |&REAR5 DIED IN PAIN| $LIST)))
		  MEXPR)

 	(ARGS '$REAR5 '(NIL . 1))

 	(ADD2LNC '(($REAR5) $LIST) $FUNCTIONS)

 	(DEFPROP $REMUN
		 (LAMBDA ($LIST $REMS) 
		   (DO (($I 1 (+ 1 $I))) 
		       ((> $I ($LENGTH $REMS)) '$DONE) 
		    (SETQ $LIST
		     (SIMPLIFY ($APPLY $APPEND
				       (($LISBRE) $LIST (($PART) $REMS $I))))))
		   $LIST)
		 EXPR)

 	(ARGS '$REMUN '(NIL . 2))

 	(DEFPROP $REMUN T TRANSLATED)

 	(ADD2LNC '$REMUN $PROPS)

 	(MDEFPROP $REMUN
		  ((LAMBDA)
		   ((MLIST) $LIST $REMS)
		   ((DOLIST)
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH SIMP) $REMS)
		     NIL
		     ((MSETQ)
		      $LIST
		      (($APPLY) $APPEND (($LISBRE) $LIST (($PART) $REMS $I)))))
		    $LIST))
		  MEXPR)

 	(ARGS '$REMUN '(NIL . 2))

 	(ADD2LNC '(($REMUN) $LIST $REMS) $FUNCTIONS)

 	(DEFPROP $MAP1
		 (LAMBDA ($F1 $A1 $A2 $A3 $A4) 
		   (MEVAL '(($NEWMAP)
			    ((LAMBDA)
			     ((MLIST) $X1)
			     ((MCOND)
			      (($CONSTANTP) $X1)
			      $X1
			      T
			      (($F1) $X1 $A2 $A3 $A4)))
			    $A1)))
		 EXPR)

 	(ARGS '$MAP1 '(NIL . 5))

 	(DEFPROP $MAP1 T TRANSLATED)

 	(ADD2LNC '$MAP1 $PROPS)

 	(MDEFPROP $MAP1
		  ((LAMBDA)
		   ((MLIST) $F1 $A1 $A2 $A3 $A4)
		   (($NEWMAP)
		    ((LAMBDA)
		     ((MLIST) $X1)
		     ((MCOND)
		      (($CONSTANTP) $X1)
		      $X1
		      T
		      (($F1) $X1 $A2 $A3 $A4)))
		    $A1))
		  MEXPR)

 	(ARGS '$MAP1 '(NIL . 5))

 	(ADD2LNC '(($MAP1) $F1 $A1 $A2 $A3 $A4) $FUNCTIONS)

 	(DEFPROP $MAP2
		 (LAMBDA ($F2 $A1 $A2 $A3 $A4) 
		   (MEVAL '(($NEWMAP)
			    ((LAMBDA)
			     ((MLIST) $X2)
			     ((MCOND)
			      (($CONSTANTP) $X2)
			      $X2
			      T
			      (($F2) $A1 $X2 $A3 $A4)))
			    $A2)))
		 EXPR)

 	(ARGS '$MAP2 '(NIL . 5))

 	(DEFPROP $MAP2 T TRANSLATED)

 	(ADD2LNC '$MAP2 $PROPS)

 	(MDEFPROP $MAP2
		  ((LAMBDA)
		   ((MLIST) $F2 $A1 $A2 $A3 $A4)
		   (($NEWMAP)
		    ((LAMBDA)
		     ((MLIST) $X2)
		     ((MCOND)
		      (($CONSTANTP) $X2)
		      $X2
		      T
		      (($F2) $A1 $X2 $A3 $A4)))
		    $A2))
		  MEXPR)

 	(ARGS '$MAP2 '(NIL . 5))

 	(ADD2LNC '(($MAP2) $F2 $A1 $A2 $A3 $A4) $FUNCTIONS)

 	(DEFPROP $MAP3
		 (LAMBDA ($F3 $A1 $A2 $A3 $A4) 
		   (MEVAL '(($NEWMAP)
			    ((LAMBDA)
			     ((MLIST) $X3)
			     ((MCOND)
			      (($CONSTANTP) $X3)
			      $X3
			      T
			      (($F3) $A1 $A2 $X3 $A4)))
			    $A3)))
		 EXPR)

 	(ARGS '$MAP3 '(NIL . 5))

 	(DEFPROP $MAP3 T TRANSLATED)

 	(ADD2LNC '$MAP3 $PROPS)

 	(MDEFPROP $MAP3
		  ((LAMBDA)
		   ((MLIST) $F3 $A1 $A2 $A3 $A4)
		   (($NEWMAP)
		    ((LAMBDA)
		     ((MLIST) $X3)
		     ((MCOND)
		      (($CONSTANTP) $X3)
		      $X3
		      T
		      (($F3) $A1 $A2 $X3 $A4)))
		    $A3))
		  MEXPR)

 	(ARGS '$MAP3 '(NIL . 5))

 	(ADD2LNC '(($MAP3) $F3 $A1 $A2 $A3 $A4) $FUNCTIONS)

 	(DEFPROP $MAP4
		 (LAMBDA ($A1 $A2 $A3 $A4) 
		   (MEVAL '(($NEWMAP)
			    ((LAMBDA)
			     ((MLIST) $X4)
			     ((MCOND)
			      (($CONSTANTP) $X4)
			      $X4
			      T
			      (($EPSP) $A1 $A2 $A3 $X4)))
			    $A4)))
		 EXPR)

 	(ARGS '$MAP4 '(NIL . 4))

 	(DEFPROP $MAP4 T TRANSLATED)

 	(ADD2LNC '$MAP4 $PROPS)

 	(MDEFPROP $MAP4
		  ((LAMBDA)
		   ((MLIST) $A1 $A2 $A3 $A4)
		   (($NEWMAP)
		    ((LAMBDA)
		     ((MLIST) $X4)
		     ((MCOND)
		      (($CONSTANTP) $X4)
		      $X4
		      T
		      (($EPSP) $A1 $A2 $A3 $X4)))
		    $A4))
		  MEXPR)

 	(ARGS '$MAP4 '(NIL . 4))

 	(ADD2LNC '(($MAP4) $A1 $A2 $A3 $A4) $FUNCTIONS)

 	(DEFPROP $EPS1
		 (LAMBDA ($X01 $X02 $X03 $X04) 
		   (SIMPLIFY ($MAP1 (MEVAL '((LAMBDA)
					     ((MLIST) $X11 $X12 $X13 $X14)
					     (($MAP2)
					      ((LAMBDA)
					       ((MLIST) $X21 $X22 $X23 $X24)
					       (($MAP3)
						((LAMBDA)
						 ((MLIST) $X31 $X32 $X33 $X34)
						 (($MAP4) $X31 $X32 $X33 $X34))
						$X21
						$X22
						$X23
						$X24))
					      $X11
					      $X12
					      $X13
					      $X14)))
				    $X01
				    $X02
				    $X03
				    $X04)))
		 EXPR)

 	(ARGS '$EPS1 '(NIL . 4))

 	(DEFPROP $EPS1 T TRANSLATED)

 	(ADD2LNC '$EPS1 $PROPS)

 	(MDEFPROP $EPS1
		  ((LAMBDA)
		   ((MLIST) $X01 $X02 $X03 $X04)
		   (($MAP1)
		    ((LAMBDA)
		     ((MLIST) $X11 $X12 $X13 $X14)
		     (($MAP2)
		      ((LAMBDA)
		       ((MLIST) $X21 $X22 $X23 $X24)
		       (($MAP3)
			((LAMBDA)
			 ((MLIST) $X31 $X32 $X33 $X34)
			 (($MAP4) $X31 $X32 $X33 $X34))
			$X21
			$X22
			$X23
			$X24))
		      $X11
		      $X12
		      $X13
		      $X14))
		    $X01
		    $X02
		    $X03
		    $X04))
		  MEXPR)

 	(ARGS '$EPS1 '(NIL . 4))

 	(ADD2LNC '(($EPS1) $X01 $X02 $X03 $X04) $FUNCTIONS)

 	(MDEFPROP $EPSP T MLEXPRP)

 	(MDEFPROP $EPSP
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST) $LISTP $I)
		    ((MCOND)
		     ((MNOT) $EPSOF)
		     ((MRETURN) (($APPLY) $EPS $LIST))
		     T
		     $FALSE)
		    ((MSETQ) $LISTP (($SORT) $LIST))
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MEQUAL) $I 4)
		     ((MRETURN)
		      ((MTIMES)
		       (($SIGNAT) $LIST $LISTP)
		       (($APPLY) $EPS $LISTP)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL)
		      (($LISTP ARRAY) $I)
		      (($LISTP ARRAY) ((MPLUS) $I 1)))
		     ((MRETURN) 0)
		     T
		     $FALSE)
		    ((MGO) $LOOP)))
		  MEXPR)

 	(ADD2LNC '(($EPSP) ((MLIST) $LIST)) $FUNCTIONS)

 	(DEFPROP
	 $TRIV5
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($I $SU $I1 $I2 $I3 $I4) 
	      (PROG NIL 
		    (SETQ $SU 0)
		    (SETQ $LEN (+ ($LENGTH $LIST) -1))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (+ $I 1))
		    (COND ((NOT (LIKE ($PART $LIST $I) (MEVAL1 '$G5)))
			   (GO $LOOP)))
		    (SETQ $LIST (MEVAL '(($CYC)
					 (($DELETE) $G5 $LIST)
					 ((MPLUS) $I ((MMINUS) 1)))))
		    (COND ((LIKE (MEVAL1 '$LEN) 4)
			   (RETURN (MUL 4
					'$%I
					(SIMPLIFY ($EPS0 $LIST))))))
		    (DO (($I1 1 (+ 1 $I1))) 
			((IS (MGRP $I1 (MEVAL1 '$LEN))) '$DONE) 
		     (DO (($I2 (+ $I1 1) (+ 1 $I2)))
		      ((IS (MGRP $I2 (MEVAL1 '$LEN))) '$DONE)
		      (DO (($I3 (+ $I2 1) (+ 1 $I3))) 
			  ((IS (MGRP $I3 (MEVAL1 '$LEN))) '$DONE) 
		       (DO (($I4 (+ $I3 1) (+ 1 $I4)))
			((IS (MGRP $I4 (MEVAL1 '$LEN))) '$DONE)
			(SETQ 
			 $SU
			 (ADD
			  $SU
			  (MUL
			   '$%I
			   (EXPT -1 (+ $I1 $I2 $I3 $I4))
			   (SIMPLIFY ($EPS0 (LIST '(MLIST)
						  (MEVAL1 '(($LIST ARRAY) $I1))
						  (MEVAL1 '(($LIST ARRAY) $I2))
						  (MEVAL1 '(($LIST ARRAY) $I3))
						  (MEVAL1 '(($LIST ARRAY)
							    $I4)))))
			   (MEVAL '(($TR0)
				    (($REMUN)
				     $LIST
				     ((MLIST) $I4 $I3 $I2 $I1)))))))))))
		    (RETURN $SU)))
	    0
	    '$SU
	    0
	    0
	    0
	    0))
	 EXPR)

 	(ARGS '$TRIV5 '(NIL . 1))

 	(DEFPROP $TRIV5 T TRANSLATED)

 	(ADD2LNC '$TRIV5 $PROPS)

 	(MDEFPROP $TRIV5
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $I $SU $I1 $I2 $I3 $I4)
		    ((MSETQ) $SU 0)
		    ((MSETQ) $LEN ((MPLUS) (($LENGTH) $LIST) ((MMINUS) 1)))
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $LIST $I) $G5)
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MSETQ)
		     $LIST
		     (($CYC) (($DELETE) $G5 $LIST) ((MPLUS) $I ((MMINUS) 1))))
		    ((MCOND)
		     ((MEQUAL) $LEN 4)
		     ((MRETURN) ((MTIMES) 4 $%I (($EPS0) $LIST)))
		     T
		     $FALSE)
		    ((MDO)
		     $I1
		     NIL
		     NIL
		     NIL
		     $LEN
		     NIL
		     ((MDO)
		      $I2
		      ((MPLUS) $I1 1)
		      NIL
		      NIL
		      $LEN
		      NIL
		      ((MDO)
		       $I3
		       ((MPLUS) $I2 1)
		       NIL
		       NIL
		       $LEN
		       NIL
		       ((MDO)
			$I4
			((MPLUS) $I3 1)
			NIL
			NIL
			$LEN
			NIL
			((MSETQ)
			 $SU
			 ((MPLUS)
			  $SU
			  ((MTIMES)
			   $%I
			   ((MEXPT) ((MMINUS) 1) ((MPLUS) $I1 $I2 $I3 $I4))
			   (($EPS0)
			    ((MLIST)
			     (($LIST ARRAY) $I1)
			     (($LIST ARRAY) $I2)
			     (($LIST ARRAY) $I3)
			     (($LIST ARRAY) $I4)))
			   (($TR0)
			    (($REMUN) $LIST ((MLIST) $I4 $I3 $I2 $I1))))))))))
		    ((MRETURN) $SU)))
		  MEXPR)

 	(ARGS '$TRIV5 '(NIL . 1))

 	(ADD2LNC '(($TRIV5) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $EPSFIX
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($QW) 
	      (SETQ $QW (SIMPLIFY ($SUBSTITUTE (MEVAL1 '$EPS00)
					       (MEVAL1 '$EPS)
					       $EXP)))
	      (SIMPLIFY ($EV $QW)))
	    '$QW))
	 EXPR)

 	(ARGS '$EPSFIX '(NIL . 1))

 	(DEFPROP $EPSFIX T TRANSLATED)

 	(ADD2LNC '$EPSFIX $PROPS)

 	(MDEFPROP $EPSFIX
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $QW)
		    ((MSETQ) $QW (($SUBSTITUTE) $EPS00 $EPS $EXP))
		    ((MRETURN) (($EV) $QW))))
		  MEXPR)

 	(ARGS '$EPSFIX '(NIL . 1))

 	(ADD2LNC '(($EPSFIX) $EXP) $FUNCTIONS)

 	(MDEFPROP $EPS00 T MLEXPRP)

 	(MDEFPROP $EPS00
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   (($EPS0) (($GETRED) $LIST)))
		  MEXPR)

 	(ADD2LNC '(($EPS00) ((MLIST) $LIST)) $FUNCTIONS)

 	(DEFPROP
	 $TR5
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($RR $I $LEN) 
	      (PROG NIL 
		    (COND ((NOT (LIKE (MEVAL1 '$N) 4))
			   (SIMPLIFY ($PRINT (MEVAL1 '|&USING G5 IN|)
					     (MEVAL1 '$N)
					     (MEVAL1 '&DIMENSIONS)))))
		    (COND ((NOT (IS ($FREEOF (MEVAL1 '$LHP)
					     (MEVAL1 '$RHP)
					     $LIST)))
			   (RETURN (SIMPLIFY ($PROJR $LIST)))))
		    (SETQ $LEN ($LENGTH $LIST))
		    (COND ((LIKE ($LENGTH (SIMPLIFY ($DELETE (MEVAL1 '$G5)
							     $LIST)))
				 (ADD $LEN -1))
			   (GO $LENON)))
		    (SETQ $RR (SIMPLIFY ($REM5 $LIST)))
		    (SETQ $LIST ($PART $RR 2))
		    (SETQ $FAC ($PART $RR 1))
		    (SETQ $LEN ($LENGTH $LIST))
		    (COND ((IS ($FREEOF (MEVAL1 '$G5) $LIST))
			   (RETURN (MUL (MEVAL1 '$FAC)
					(MEVAL '(($TR0) $LIST))))))
	       $LENON
		    (COND ((OR (IS ($EVENP $LEN))
			       (< ($LENGTH (SIMPLIFY ($LISTOFVARS $LIST))) 4))
			   (RETURN 0)))
		    (COND ((IS (MLSP $LEN 5)) (RETURN 0)))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (+ $I 1))
		    (COND ((IS (MGRP $I $LEN)) (GO $LO1)))
		    (COND ((AND ($MEMBER ($PART $LIST $I)
					 (MEVAL1 '$IND))
				(LIKE ($LENGTH (SIMPLIFY ($DELETE ($PART $LIST
									 $I)
								  $LIST)))
				      (ADD $LEN -2)))
			   (RETURN (SIMPLIFY ($CONT5 (MEVAL '(($CYC)
							      $LIST
							      ((MPLUS)
							       $I
							       ((MMINUS)
								1)))))))))
		    (COND ((AND (LIKE ($PART $LIST $I) ($PART $LIST (+ $I 1)))
				(IS ($FREEOF ($PART $LIST $I)
					     (MEVAL1 '$IND))))
			   (RETURN (MUL (MEVAL '(($D)
						 (($PART) $LIST $I)
						 (($PART) $LIST $I)))
					(MEVAL '(($TR)
						 (($APPEND)
						  (($REST)
						   $LIST
						   ((MPLUS)
						    ((MMINUS) 1)
						    $I
						    ((MMINUS) $LEN)))
						  (($REST)
						   $LIST
						   ((MPLUS) $I 1)))))))))
		    (GO $LOOP)
	       $LO1 (RETURN (SIMPLIFY ($TRIV5 $LIST)))))
	    '$RR
	    0
	    '$LEN))
	 EXPR)

 	(ARGS '$TR5 '(NIL . 1))

 	(DEFPROP $TR5 T TRANSLATED)

 	(ADD2LNC '$TR5 $PROPS)

 	(MDEFPROP $TR5
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $RR $I $LEN)
		    ((MCOND)
		     ((MNOTEQUAL) $N 4)
		     (($PRINT) |&USING G5 IN| $N &DIMENSIONS)
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOT) (($FREEOF) $LHP $RHP $LIST))
		     ((MRETURN) (($PROJR) $LIST))
		     T
		     $FALSE)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MCOND)
		     ((MEQUAL)
		      (($LENGTH) (($DELETE) $G5 $LIST))
		      ((MPLUS) $LEN ((MMINUS) 1)))
		     ((MGO) $LENON)
		     T
		     $FALSE)
		    ((MSETQ) $RR (($REM5) $LIST))
		    ((MSETQ) $LIST (($PART) $RR 2))
		    ((MSETQ) $FAC (($PART) $RR 1))
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MCOND)
		     (($FREEOF) $G5 $LIST)
		     ((MRETURN) ((MTIMES) $FAC (($TR0) $LIST)))
		     T
		     $FALSE)
		    $LENON
		    ((MCOND)
		     ((MOR)
		      (($EVENP) $LEN)
		      ((MLESSP) (($LENGTH) (($LISTOFVARS) $LIST)) 4))
		     ((MRETURN) 0)
		     T
		     $FALSE)
		    ((MCOND) ((MLESSP) $LEN 5) ((MRETURN) 0) T $FALSE)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND) ((MGREATERP) $I $LEN) ((MGO) $LO1) T $FALSE)
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) (($PART) $LIST $I) $IND)
		      ((MEQUAL)
		       (($LENGTH) (($DELETE) (($PART) $LIST $I) $LIST))
		       ((MPLUS) $LEN ((MMINUS) 2))))
		     ((MRETURN)
		      (($CONT5) (($CYC) $LIST ((MPLUS) $I ((MMINUS) 1)))))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND)
		      ((MEQUAL)
		       (($PART) $LIST $I)
		       (($PART) $LIST ((MPLUS) $I 1)))
		      (($FREEOF) (($PART) $LIST $I) $IND))
		     ((MRETURN)
		      ((MTIMES)
		       (($D) (($PART) $LIST $I) (($PART) $LIST $I))
		       (($TR)
			(($APPEND)
			 (($REST)
			  $LIST
			  ((MPLUS) ((MMINUS) 1) $I ((MMINUS) $LEN)))
			 (($REST) $LIST ((MPLUS) $I 1))))))
		     T
		     $FALSE)
		    ((MGO) $LOOP)
		    $LO1
		    ((MRETURN) (($TRIV5) $LIST))))
		  MEXPR)

 	(ARGS '$TR5 '(NIL . 1))

 	(ADD2LNC '(($TR5) $LIST) $FUNCTIONS)

 	(DEFPROP
	 $CONT5
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($LIS $IN $OUT $LEN) 
	      (PROG NIL 
		    (SETQ $LIS (MEVAL '(($LISCUT) $LIST)))
		    (SETQ $IN ($FIRST $LIS))
		    (SETQ $OUT ($LAST $LIS))
		    (SETQ $LEN
			  ($LENGTH (SIMPLIFY ($DELETE (MEVAL1 '$G5)
						      $IN))))
		    (RETURN
		     (COND
		      ((IS ($ODDP $LEN))
		       (RETURN
			(SIMPLIFY
			 (LIST
			  '(MMINUS)
			  (MUL
			   2
			   (SIMPLIFY
			    ($TR5 (SIMPLIFY ($APPEND (SIMPLIFY ($REVERSE $IN))
						     $OUT)))))))))
		      (T
		       (RETURN
			(MUL
			 2
			 (ADD
			  (SIMPLIFY ($TR5 (SIMPLIFY ($APPEND $IN $OUT))))
			  (SIMPLIFY
			   ($TR5 (SIMPLIFY ($APPEND (SIMPLIFY ($REVERSE $IN))
						    $OUT))))))))))))
	    '$LIS
	    '$IN
	    '$OUT
	    '$LEN))
	 EXPR)

 	(ARGS '$CONT5 '(NIL . 1))

 	(DEFPROP $CONT5 T TRANSLATED)

 	(ADD2LNC '$CONT5 $PROPS)

 	(MDEFPROP $CONT5
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $LIS $IN $OUT $LEN)
		    ((MSETQ) $LIS (($LISCUT) $LIST))
		    ((MSETQ) $IN (($FIRST) $LIS))
		    ((MSETQ) $OUT (($LAST) $LIS))
		    ((MSETQ) $LEN (($LENGTH) (($DELETE) $G5 $IN)))
		    ((MCOND)
		     (($ODDP) $LEN)
		     ((MRETURN)
		      ((MMINUS)
		       ((MTIMES)
			2
			(($TR5) (($APPEND) (($REVERSE) $IN) $OUT)))))
		     T
		     ((MRETURN)
		      ((MTIMES)
		       2
		       ((MPLUS)
			(($TR5) (($APPEND) $IN $OUT))
			(($TR5) (($APPEND) (($REVERSE) $IN) $OUT))))))))
		  MEXPR)

 	(ARGS '$CONT5 '(NIL . 1))

 	(ADD2LNC '(($CONT5) $LIST) $FUNCTIONS)

 	(DEFPROP $EPS0
		 (LAMBDA ($LIST) 
		   (COND ((AND (IS (MEVAL1 '$EPSEF))
			       (NOT (IS ($FREEOF (MEVAL1 '&+)
						 (MEVAL1 '&*)
						 (MEVAL1 '&-)
						 (MEVAL1 '&//)
						 $LIST))))
			  (SIMPLIFY ($APPLY $EPS1 $LIST)))
			 (T (SIMPLIFY ($APPLY $EPSP $LIST)))))
		 EXPR)

 	(ARGS '$EPS0 '(NIL . 1))

 	(DEFPROP $EPS0 T TRANSLATED)

 	(ADD2LNC '$EPS0 $PROPS)

 	(MDEFPROP $EPS0
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MCOND)
		    ((MAND) $EPSEF ((MNOT) (($FREEOF) &+ &* &- &// $LIST)))
		    (($APPLY) $EPS1 $LIST)
		    T
		    (($APPLY) $EPSP $LIST)))
		  MEXPR)

 	(ARGS '$EPS0 '(NIL . 1))

 	(ADD2LNC '(($EPS0) $LIST) $FUNCTIONS)

 	(DEFPROP $SIGNAT
		 (LAMBDA ($LIST1 $LIST2) 
		   ((LAMBDA ($I $S $LIS1 $LIS2) 
		      (PROG NIL 
			    (COND ((LIKE $LIST1 $LIST2) (RETURN 1)))
			    (SETQ $S 0)
			    (SETQ $B1 ($FIRST $LIST1))
			    (SETQ $I 0)
		       $LO1 (SETQ $I (+ $I 1))
			    (COND ((LIKE ($PART $LIST2 $I)
					 (MEVAL1 '$B1))
				   (GO $ON))
				  (T (GO $LO1)))
		       $ON  (SETQ $S (ADD $S $I -1))
			    (SETQ $LIST2
				  (MEVAL '(($CYC)
					   $LIST2
					   ((MPLUS) $I ((MMINUS) 1)))))
			    (SETQ $LIS1 ($REST $LIST1 1))
			    (SETQ $LIS2 ($REST $LIST2 1))
			    (SETQ $I 0)
			    (SETQ $B1 ($PART $LIS1 1))
		       $T1  (SETQ $I (+ $I 1))
			    (SETQ $LIS2 (MEVAL '(($CYC) $LIS2 1)))
			    (COND ((LIKE ($PART $LIS2 1) (MEVAL1 '$B1))
				   (GO $ON1))
				  (T (GO $T1)))
		       $ON1 (SETQ $LIS1 ($REST $LIS1 1))
			    (SETQ $LIS2 ($REST $LIS2 1))
			    (SETQ $I 0)
			    (SETQ $B1 ($PART $LIS1 1))
		       $T2  (SETQ $I (+ $I 1))
			    (SETQ $LIS2 (MEVAL '(($CYC) $LIS2 1)))
			    (COND ((LIKE ($PART $LIS2 1) (MEVAL1 '$B1))
				   (GO $ON2))
				  (T (GO $T2)))
		       $ON2 (SETQ $S (ADD $S $I))
			    (RETURN (POWER -1 $S))))
		    0
		    '$S
		    '$LIS1
		    '$LIS2))
		 EXPR)

 	(ARGS '$SIGNAT '(NIL . 2))

 	(DEFPROP $SIGNAT T TRANSLATED)

 	(ADD2LNC '$SIGNAT $PROPS)

 	(MDEFPROP $SIGNAT
		  ((LAMBDA)
		   ((MLIST) $LIST1 $LIST2)
		   ((MPROG)
		    ((MLIST) $I $S $LIS1 $LIS2)
		    ((MCOND) ((MEQUAL) $LIST1 $LIST2) ((MRETURN) 1) T $FALSE)
		    ((MSETQ) $S 0)
		    ((MSETQ) $B1 (($FIRST) $LIST1))
		    ((MSETQ) $I 0)
		    $LO1
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIST2 $I) $B1)
		     ((MGO) $ON)
		     T
		     ((MGO) $LO1))
		    $ON
		    ((MSETQ) $S ((MPLUS) $S $I ((MMINUS) 1)))
		    ((MSETQ) $LIST2 (($CYC) $LIST2 ((MPLUS) $I ((MMINUS) 1))))
		    ((MSETQ) $LIS1 (($REST) $LIST1 1))
		    ((MSETQ) $LIS2 (($REST) $LIST2 1))
		    ((MSETQ) $I 0)
		    ((MSETQ) $B1 (($PART) $LIS1 1))
		    $T1
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MSETQ) $LIS2 (($CYC) $LIS2 1))
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIS2 1) $B1)
		     ((MGO) $ON1)
		     T
		     ((MGO) $T1))
		    $ON1
		    ((MSETQ) $LIS1 (($REST) $LIS1 1))
		    ((MSETQ) $LIS2 (($REST) $LIS2 1))
		    ((MSETQ) $I 0)
		    ((MSETQ) $B1 (($PART) $LIS1 1))
		    $T2
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MSETQ) $LIS2 (($CYC) $LIS2 1))
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIS2 1) $B1)
		     ((MGO) $ON2)
		     T
		     ((MGO) $T2))
		    $ON2
		    ((MSETQ) $S ((MPLUS) $S $I))
		    ((MRETURN) ((MEXPT) ((MMINUS) 1) $S))))
		  MEXPR)

 	(ARGS '$SIGNAT '(NIL . 2))

 	(ADD2LNC '(($SIGNAT) $LIST1 $LIST2) $FUNCTIONS)

 	(DEFPROP
	 $PROJR
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($LEN $J $JP $LBI $LBJ $A $B $P1 $P2 $F $LB) 
	      (PROG NIL 
		    (SETQ $I 0)
		    (SETQ $LEN ($LENGTH $LIST))
	       $LOOP(SETQ $I (+ $I 1))
		    (COND ((IS (MGRP $I $LEN))
			   (RETURN (MEVAL '(($TR) $LIST)))))
		    (COND ((LIKE ($PART $LIST $I) (MEVAL1 '$LHP))
			   (GO $T1)))
		    (COND ((LIKE ($PART $LIST $I) (MEVAL1 '$RHP))
			   (GO $T2)))
		    (GO $LOOP)
	       $T1  (SETQ $F -1)
		    (SETQ $A (MEVAL1 '$LHP))
		    (SETQ $B (MEVAL1 '$RHP))
		    (GO $T3)
	       $T2  (SETQ $F 1)
		    (SETQ $A (MEVAL1 '$RHP))
		    (SETQ $B (MEVAL1 '$LHP))
	       $T3  (COND ((OR (LIKE ($PART $LIST (+ $I 1))
				     (MEVAL1 '$LHP))
			       (LIKE ($PART $LIST (+ $I 1))
				     (MEVAL1 '$RHP)))
			   (GO $SPEC)))
		    (SETQ $J $I)
	       $TG1 (SETQ $J (ADD $J 1))
		    (COND ((IS (MGRP $J ($LENGTH $LIST))) (GO $BORE)))
		    (SETQ $JP ($PART $LIST $J))
		    (COND ((AND (NOT (LIKE $JP (MEVAL1 '$LHP)))
				(NOT (LIKE $JP (MEVAL1 '$RHP))))
			   (GO $TG1)))
		    (SETQ $BET (ADD $J (- $I) -1))
		    (COND ((AND (LIKE $JP $B)
				(IS ($EVENP (MEVAL1 '$BET))))
			   (RETURN 0)))
		    (COND ((AND (LIKE $JP $A)
				(IS ($ODDP (MEVAL1 '$BET))))
			   (RETURN 0)))
		    (SETQ $LBI (MEVAL '(($LISBRE) $LIST $I)))
		    (RETURN (SIMPLIFY ($PROJR (SIMPLIFY ($APPLY $APPEND
								$LBI)))))
	       $BORE(SETQ $LB (MEVAL '(($LISBRE) $LIST $I)))
		    (RETURN
		     (ADD
		      (DIV (SIMPLIFY ($TR5 (SIMPLIFY ($APPEND ($PART $LB 1)
							      ($PART $LB 2)))))
			   2)
		      (MUL
		       (DIV $F 2)
		       (SIMPLIFY
			($TR5 (SIMPLIFY ($APPEND ($PART $LB 1)
						 (LIST '(MLIST)
						       (MEVAL1 '$G5))
						 ($PART $LB 2))))))))
	       $SPEC(SETQ $P1 ($PART $LIST $I))
		    (SETQ $P2 ($PART $LIST (+ $I 1)))
		    (COND ((OR (AND (LIKE $P1 (MEVAL1 '$LHP))
				    (LIKE $P2 (MEVAL1 '$RHP)))
			       (AND (LIKE $P1 (MEVAL1 '$RHP))
				    (LIKE $P2 (MEVAL1 '$LHP))))
			   (RETURN 0)))
		    (SETQ $P1 (MEVAL '(($LISBRE) $LIST $I)))
		    (RETURN (SIMPLIFY ($TR5 (SIMPLIFY ($APPEND ($PART $P1 1)
							       ($PART $P1
								      2))))))))
	    '$LEN
	    '$J
	    '$JP
	    '$LBI
	    '$LBJ
	    '$A
	    '$B
	    '$P1
	    '$P2
	    '$F
	    '$LB))
	 EXPR)

 	(ARGS '$PROJR '(NIL . 1))

 	(DEFPROP $PROJR T TRANSLATED)

 	(ADD2LNC '$PROJR $PROPS)

 	(MDEFPROP $PROJR
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $LEN $J $JP $LBI $LBJ $A $B $P1 $P2 $F $LB)
		    ((MSETQ) $I 0)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I $LEN)
		     ((MRETURN) (($TR) $LIST))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIST $I) $LHP)
		     ((MGO) $T1)
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIST $I) $RHP)
		     ((MGO) $T2)
		     T
		     $FALSE)
		    ((MGO) $LOOP)
		    $T1
		    ((MSETQ) $F ((MMINUS) 1))
		    ((MSETQ) $A $LHP)
		    ((MSETQ) $B $RHP)
		    ((MGO) $T3)
		    $T2
		    ((MSETQ) $F 1)
		    ((MSETQ) $A $RHP)
		    ((MSETQ) $B $LHP)
		    $T3
		    ((MCOND)
		     ((MOR)
		      ((MEQUAL) (($PART) $LIST ((MPLUS) $I 1)) $LHP)
		      ((MEQUAL) (($PART) $LIST ((MPLUS) $I 1)) $RHP))
		     ((MGO) $SPEC)
		     T
		     $FALSE)
		    ((MSETQ) $J $I)
		    $TG1
		    ((MSETQ) $J ((MPLUS) $J 1))
		    ((MCOND)
		     ((MGREATERP) $J (($LENGTH) $LIST))
		     ((MGO) $BORE)
		     T
		     $FALSE)
		    ((MSETQ) $JP (($PART) $LIST $J))
		    ((MCOND)
		     ((MAND) ((MNOTEQUAL) $JP $LHP) ((MNOTEQUAL) $JP $RHP))
		     ((MGO) $TG1)
		     T
		     $FALSE)
		    ((MSETQ) $BET ((MPLUS) $J ((MMINUS) $I) ((MMINUS) 1)))
		    ((MCOND)
		     ((MAND) ((MEQUAL) $JP $B) (($EVENP) $BET))
		     ((MRETURN) 0)
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND) ((MEQUAL) $JP $A) (($ODDP) $BET))
		     ((MRETURN) 0)
		     T
		     $FALSE)
		    ((MSETQ) $LBI (($LISBRE) $LIST $I))
		    ((MRETURN) (($PROJR) (($APPLY) $APPEND $LBI)))
		    $BORE
		    ((MSETQ) $LB (($LISBRE) $LIST $I))
		    ((MRETURN)
		     ((MPLUS)
		      ((MQUOTIENT)
		       (($TR5) (($APPEND) (($PART) $LB 1) (($PART) $LB 2)))
		       2)
		      ((MTIMES)
		       ((MQUOTIENT) $F 2)
		       (($TR5)
			(($APPEND)
			 (($PART) $LB 1)
			 ((MLIST) $G5)
			 (($PART) $LB 2))))))
		    $SPEC
		    ((MSETQ) $P1 (($PART) $LIST $I))
		    ((MSETQ) $P2 (($PART) $LIST ((MPLUS) $I 1)))
		    ((MCOND)
		     ((MOR)
		      ((MAND) ((MEQUAL) $P1 $LHP) ((MEQUAL) $P2 $RHP))
		      ((MAND) ((MEQUAL) $P1 $RHP) ((MEQUAL) $P2 $LHP)))
		     ((MRETURN) 0)
		     T
		     $FALSE)
		    ((MSETQ) $P1 (($LISBRE) $LIST $I))
		    ((MRETURN)
		     (($TR5) (($APPEND) (($PART) $P1 1) (($PART) $P1 2))))))
		  MEXPR)

 	(ARGS '$PROJR '(NIL . 1))

 	(ADD2LNC '(($PROJR) $LIST) $FUNCTIONS)

	(SETQ IBASE 10.)


