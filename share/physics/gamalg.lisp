
(SETQ IBASE 8.)
 	(SETQ SAVENO 5466)
 	(DSKSETQ $PARTSWITCH T)
 	(DSKSETQ $LISTARITH NIL)
 	(DSKSETQ $CONTAB
		 '((MLIST SIMP)
		   ((MLIST SIMP) ((MLIST SIMP) $N) ((MLIST SIMP)))
		   ((MLIST SIMP)
		    ((MLIST SIMP) ((MPLUS SIMP) 2 ((MTIMES SIMP) -1 $N)))
		    ((MLIST SIMP) 1))
		   ((MLIST SIMP)
		    ((MLIST SIMP) 4)
		    ((MLIST SIMP))
		    ((MLIST SIMP) ((MPLUS SIMP) -4 $N))
		    ((MLIST SIMP) 1 2))
		   ((MLIST SIMP)
		    ((MLIST SIMP) -2)
		    ((MLIST SIMP) 3 2 1)
		    ((MLIST SIMP) ((MPLUS SIMP) 4 ((MTIMES SIMP) -1 $N)))
		    ((MLIST SIMP) 1 2 3))
		   ((MLIST SIMP)
		    ((MLIST SIMP) 2)
		    ((MLIST SIMP) 3 2 1 4)
		    ((MLIST SIMP) 2)
		    ((MLIST SIMP) 4 1 2 3)
		    ((MLIST SIMP) ((MPLUS SIMP) -4 $N))
		    ((MLIST SIMP) 1 2 3 4))
		   ((MLIST SIMP)
		    ((MLIST SIMP) 2)
		    ((MLIST SIMP) 2 3 4 5 1)
		    ((MLIST SIMP) -2)
		    ((MLIST SIMP) 1 4 3 2 5)
		    ((MLIST SIMP) -2)
		    ((MLIST SIMP) 1 5 2 3 4)
		    ((MLIST SIMP) ((MPLUS SIMP) 4 ((MTIMES SIMP) -1 $N)))
		    ((MLIST SIMP) 1 2 3 4 5))))
 	(DSKSETQ $CONTAB4
		 '((MLIST SIMP)
		   ((MLIST SIMP) ((MLIST SIMP) 4) ((MLIST SIMP)))
		   ((MLIST SIMP) ((MLIST SIMP) -2) ((MLIST SIMP) 1))
		   ((MLIST SIMP) ((MLIST SIMP) 4) ((MLIST SIMP)))
		   ((MLIST SIMP) ((MLIST SIMP) -2) ((MLIST SIMP) 3 2 1))
		   ((MLIST SIMP)
		    ((MLIST SIMP) 2)
		    ((MLIST SIMP) 3 2 1 4)
		    ((MLIST SIMP) 2)
		    ((MLIST SIMP) 4 1 2 3))
		   ((MLIST SIMP) ((MLIST SIMP) -2) ((MLIST SIMP) 5 4 3 2 1))))
 	(DSKSETQ $IND '((MLIST SIMP)))
 	(DSKSETQ $COF NIL)
 	(DSKSETQ $NOP NIL)
 	(DSKSETQ $NPIND '((MLIST SIMP)))
 	(DSKSETQ $NTR NIL)
 	(DSKSETQ $KINS '((MLIST SIMP)))
 	(DSKSETQ $DEF T)
 	(DSKSETQ $BORED NIL)
 	(DSKSETQ $BORELEN 4)
 	(DSKSETQ $ZERM NIL)
 	(DSKSETQ $FLAGPARADE
		 '((MLIST SIMP)
		   $BORED
		   $COF
		   $DEF
		   $DOF
		   $DSIMP
		   $EPSEF
		   $EPSOF
		   $KAHAF
		   $MTRICK
		   $NOP
		   $NTR
		   $PLATU
		   $VIRED
		   $ZERM
		   $BORELEN
		   $COMPS
		   $IND
		   $NPIND
		   $KINS
		   $SCALARS))
 	(DSKSETQ $MTRICK NIL)
 	(DSKSETQ $KAHAF NIL)
 	(DSKSETQ $CONTLIM 4)
 	(DSKSETQ $PLAT '((MLIST SIMP) NIL NIL))
 	(DSKSETQ $DOF T)
 	(DSKSETQ $PLATU NIL)
 	(DSKSETQ $SCALARS '((MLIST SIMP)))
 	(DSKSETQ $DSIMP T)
 	(DSKSETQ $KINSARG '((MLIST SIMP)))
 	(DSKSETQ $VIRED NIL)
 	(DSKSETQ $DANGER
		 '((MLIST SIMP) $UV $UVS $ZA $ZDEN $ZD $ZN $G5 $LHP $RHP))
 	(DSKSETQ $EPSEF T)
 	(DSKSETQ $EPSOF T)
 	(DSKSETQ $COMPS '((MLIST SIMP)))
 	(DSKSETQ $ODDKILL NIL)
 	(DEFPROP $NEWMAP
		 (LAMBDA ($FN $AN) 
		   (COND ((IS ($ATOM $AN)) (MEVAL '(($FN) $AN)))
			 (T (FMAP1 (GETOPR $FN) (LIST $AN) NIL))))
		 EXPR)
 	(ARGS '$NEWMAP '(NIL . 2))
 	(DEFPROP $NEWMAP T TRANSLATED)
 	(ADD2LNC '$NEWMAP $PROPS)
 	(MDEFPROP $NEWMAP
		  ((LAMBDA)
		   ((MLIST) $FN $AN)
		   ((MCOND) (($ATOM) $AN) (($FN) $AN) T (($FULLMAP) $FN $AN)))
		  MEXPR)
 	(ARGS '$NEWMAP '(NIL . 2))
 	(ADD2LNC '(($NEWMAP) $FN $AN) $FUNCTIONS)
 	(DEFPROP $NLIS
		 (LAMBDA ($IN $REPL) 
		   ((LAMBDA ($I $INK) 
		      (SETQ $INK $IN)
		      (DO (($I 1 (+ 1 $I))) 
			  ((> $I ($LENGTH $IN)) '$DONE) 
		       (SETQ $IN
			(SIMPLIFY ($SUBSTPART (($PART) $INK (($PART) $REPL $I))
					      $IN
					      $I))))
		      $IN)
		    '$I
		    '$INK))
		 EXPR)
 	(ARGS '$NLIS '(NIL . 2))
 	(DEFPROP $NLIS T TRANSLATED)
 	(ADD2LNC '$NLIS $PROPS)
 	(MDEFPROP $NLIS
		  ((LAMBDA)
		   ((MLIST) $IN $REPL)
		   ((MPROG)
		    ((MLIST) $I $INK)
		    ((MSETQ) $INK $IN)
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH SIMP) $IN)
		     NIL
		     ((MSETQ)
		      $IN
		      (($SUBSTPART) (($PART) $INK (($PART) $REPL $I)) $IN $I)))
		    $IN))
		  MEXPR)
 	(ARGS '$NLIS '(NIL . 2))
 	(ADD2LNC '(($NLIS) $IN $REPL) $FUNCTIONS)
 	(DEFPROP $NEWC
		 (LAMBDA ($DEX $VEC $OUT) 
		   ((LAMBDA ($I) 
		      (PROG NIL 
			    (SETQ $I 0)
		       $LOOP(SETQ $I (ADD* $I 1))
			    (COND ((IS (MGRP $I ($LENGTH $OUT))) (GO $FUR)))
			    (COND ((LIKE ($PART $OUT $I) $DEX) (GO $FUR)))
			    (GO $LOOP)
		       $FUR (RETURN (SIMPLIFY ($SUBSTPART $VEC $OUT $I)))))
		    '$I))
		 EXPR)
 	(ARGS '$NEWC '(NIL . 3))
 	(DEFPROP $NEWC T TRANSLATED)
 	(ADD2LNC '$NEWC $PROPS)
 	(MDEFPROP $NEWC
		  ((LAMBDA)
		   ((MLIST) $DEX $VEC $OUT)
		   ((MPROG)
		    ((MLIST) $I)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I (($LENGTH) $OUT))
		     ((MGO) $FUR)
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) (($PART) $OUT $I) $DEX)
		     ((MGO) $FUR)
		     T
		     $FALSE)
		    ((MGO) $LOOP)
		    $FUR
		    (($SUBSTPART) $VEC $OUT $I)))
		  MEXPR)
 	(ARGS '$NEWC '(NIL . 3))
 	(ADD2LNC '(($NEWC) $DEX $VEC $OUT) $FUNCTIONS)
 	(DEFPROP $IC
		 (LAMBDA ($A) (COND (($MEMBER $A (MEVAL1 '$IND)) T)))
		 EXPR)
 	(ARGS '$IC '(NIL . 1))
 	(DEFPROP $IC T TRANSLATED)
 	(ADD2LNC '$IC $PROPS)
 	(MDEFPROP $IC
		  ((LAMBDA)
		   ((MLIST) $A)
		   ((MCOND) (($MEMBER) $A $IND) $TRUE T $FALSE))
		  MEXPR)
 	(ARGS '$IC '(NIL . 1))
 	(ADD2LNC '(($IC) $A) $FUNCTIONS)
 	(DEFPROP $LISBRE
		 (LAMBDA ($LIST $I) 
		   (LIST '(MLIST)
			 ($REST $LIST (ADD* -1 (- ($LENGTH $LIST)) $I))
			 ($REST $LIST $I)))
		 EXPR)
 	(ARGS '$LISBRE '(NIL . 2))
 	(DEFPROP $LISBRE T TRANSLATED)
 	(ADD2LNC '$LISBRE $PROPS)
 	(MDEFPROP $LISBRE
		  ((LAMBDA)
		   ((MLIST) $LIST $I)
		   ((MLIST)
		    (($REST)
		     $LIST
		     ((MPLUS) ((MMINUS) 1) ((MMINUS) (($LENGTH) $LIST)) $I))
		    (($REST) $LIST $I)))
		  MEXPR)
 	(ARGS '$LISBRE '(NIL . 2))
 	(ADD2LNC '(($LISBRE) $LIST $I) $FUNCTIONS)
 	(DEFPROP
	 $GLUE3
	 (LAMBDA ($LIS1 $LIS2 $LIS3) 
	   ((LAMBDA ($P1 $P2 $P3 $I1 $I2 $I3) 
	      (SETQ $P1 ($FIRST $LIS1))
	      (SETQ $P2 ($FIRST $LIS2))
	      (SETQ $P3 ($FIRST $LIS3))
	      (COND
	       ((NOT (LIKE (ADD* $P1 $P2 $P3) 0))
		(SIMPLIFY
		 ($ERROR (MEVAL1 '|&INCONSISTENT MOMENTUM ASSIGNMENTS|)
			 $LIS1
			 $LIS2
			 $LIS3))))
	      (SETQ $I1 ($LAST $LIS1))
	      (SETQ $I2 ($LAST $LIS2))
	      (SETQ $I3 ($LAST $LIS3))
	      (ADD*
	       (MUL* (SIMPLIFY ($SOR $I1 $I2))
		     (ADD* (SIMPLIFY ($SOR $P1 $I3))
			   (SIMPLIFY (LIST '(MMINUS)
					   (SIMPLIFY ($SOR $P2 $I3))))))
	       (MUL* (SIMPLIFY ($SOR $I2 $I3))
		     (ADD* (SIMPLIFY ($SOR $P2 $I1))
			   (SIMPLIFY (LIST '(MMINUS)
					   (SIMPLIFY ($SOR $P3 $I1))))))
	       (SIMPLIFY
		(LIST '(MMINUS)
		      (MUL* (SIMPLIFY ($SOR $I3 $I1))
			    (ADD* (SIMPLIFY ($SOR $P1 $I2))
				  (SIMPLIFY (LIST '(MMINUS)
						  (SIMPLIFY ($SOR $P3
								  $I2))))))))))
	    '$P1
	    '$P2
	    '$P3
	    '$I1
	    '$I2
	    '$I3))
	 EXPR)
 	(ARGS '$GLUE3 '(NIL . 3))
 	(DEFPROP $GLUE3 T TRANSLATED)
 	(ADD2LNC '$GLUE3 $PROPS)
 	(MDEFPROP $GLUE3
		  ((LAMBDA)
		   ((MLIST) $LIS1 $LIS2 $LIS3)
		   ((MPROG)
		    ((MLIST) $P1 $P2 $P3 $I1 $I2 $I3)
		    ((MSETQ) $P1 (($FIRST) $LIS1))
		    ((MSETQ) $P2 (($FIRST) $LIS2))
		    ((MSETQ) $P3 (($FIRST) $LIS3))
		    ((MCOND)
		     ((MNOTEQUAL) ((MPLUS) $P1 $P2 $P3) 0)
		     (($ERROR)
		      |&INCONSISTENT MOMENTUM ASSIGNMENTS|
		      $LIS1
		      $LIS2
		      $LIS3)
		     T
		     $FALSE)
		    ((MSETQ) $I1 (($LAST) $LIS1))
		    ((MSETQ) $I2 (($LAST) $LIS2))
		    ((MSETQ) $I3 (($LAST) $LIS3))
		    ((MRETURN)
		     ((MPLUS)
		      ((MTIMES)
		       (($SOR) $I1 $I2)
		       ((MPLUS) (($SOR) $P1 $I3) ((MMINUS) (($SOR) $P2 $I3))))
		      ((MTIMES)
		       (($SOR) $I2 $I3)
		       ((MPLUS) (($SOR) $P2 $I1) ((MMINUS) (($SOR) $P3 $I1))))
		      ((MMINUS)
		       ((MTIMES)
			(($SOR) $I3 $I1)
			((MPLUS)
			 (($SOR) $P1 $I2)
			 ((MMINUS) (($SOR) $P3 $I2)))))))))
		  MEXPR)
 	(ARGS '$GLUE3 '(NIL . 3))
 	(ADD2LNC '(($GLUE3) $LIS1 $LIS2 $LIS3) $FUNCTIONS)
 	(DEFPROP
	 $LISCUT
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($LEN $NUM $LISMID $LISEND) 
	      (PROG NIL 
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $NUM (SIMPLIFY ($CONUM $LIST ($FIRST $LIST))))
		    (SETQ $LISEND ($REST $LIST (ADD* $NUM 2)))
		    (SETQ 
		     $LISMID
		     (SIMPLIFY
		      ($DELETE ($FIRST $LIST)
			       ($REST $LIST
				      (ADD* (SIMPLIFY (LIST '(MMINUS)
							    $LEN))
					    $NUM
					    1)))))
		    (RETURN (COND ((> ($LENGTH $LISMID) ($LENGTH $LISEND))
				   (RETURN (LIST '(MLIST)
						 $LISEND
						 $LISMID)))
				  (T (RETURN (LIST '(MLIST)
						   $LISMID
						   $LISEND)))))))
	    '$LEN
	    '$NUM
	    '$LISMID
	    '$LISEND))
	 EXPR)
 	(ARGS '$LISCUT '(NIL . 1))
 	(DEFPROP $LISCUT T TRANSLATED)
 	(ADD2LNC '$LISCUT $PROPS)
 	(MDEFPROP $LISCUT
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $LEN $NUM $LISMID $LISEND)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $NUM (($CONUM) $LIST (($FIRST) $LIST)))
		    ((MSETQ) $LISEND (($REST) $LIST ((MPLUS) $NUM 2)))
		    ((MSETQ)
		     $LISMID
		     (($DELETE)
		      (($FIRST) $LIST)
		      (($REST) $LIST ((MPLUS) ((MMINUS) $LEN) $NUM 1))))
		    ((MCOND)
		     ((MGREATERP) (($LENGTH) $LISMID) (($LENGTH) $LISEND))
		     ((MRETURN) ((MLIST) $LISEND $LISMID))
		     T
		     ((MRETURN) ((MLIST) $LISMID $LISEND)))))
		  MEXPR)
 	(ARGS '$LISCUT '(NIL . 1))
 	(ADD2LNC '(($LISCUT) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $FLAGS
	 (LAMBDA NIL 
	   ((LAMBDA ($FL) 
	      (SIMPLIFY ($PRINT (MEVAL1 '|&DIMENSION =|)
				(MEVAL1 '$N)))
	      (DO (($I 1 (+ 1 $I))) 
		  ((> $I ($LENGTH (MEVAL1 '$FLAGPARADE)))
		   '$DONE) 
	       (SETQ $FL ($PART (MEVAL1 '$FLAGPARADE) $I)) 
	       (SIMPLIFY ($PRINT $FL
				 (MEVAL1 '&=)
				 (SIMPLIFY ($EV $FL))))))
	    '$FL))
	 EXPR)
 	(ARGS '$FLAGS '(NIL . 0))
 	(DEFPROP $FLAGS T TRANSLATED)
 	(ADD2LNC '$FLAGS $PROPS)
 	(MDEFPROP $FLAGS
		  ((LAMBDA)
		   ((MLIST))
		   ((MPROG)
		    ((MLIST) $FL)
		    (($PRINT) |&DIMENSION =| $N)
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH SIMP) $FLAGPARADE)
		     NIL
		     ((DOLIST)
		      ((MSETQ) $FL (($PART) $FLAGPARADE $I))
		      (($PRINT) $FL &= (($EV) $FL))))))
		  MEXPR)
 	(ARGS '$FLAGS '(NIL . 0))
 	(ADD2LNC '(($FLAGS)) $FUNCTIONS)
 	(DEFPROP
	 $CYC
	 (LAMBDA ($LIST $NUM) 
	   ((LAMBDA ($LIS2 $LEN) 
	      (SETQ $LEN ($LENGTH $LIST))
	      (COND ((IS (MLSP $NUM 0)) (SETQ $NUM (ADD* $LEN $NUM))))
	      (SETQ $LIS2 $LIST)
	      (DO (($I 1 (+ 1 $I))) 
		  ((IS (MGRP $I $LEN)) '$DONE) 
	       (SETQ $LIST
		(SIMPLIFY ($SUBSTPART (($LIS2 ARRAY)
				       ((MCOND)
					((MLEQP) ((MPLUS) $I $NUM) $LEN)
					((MPLUS) $I $NUM)
					T
					((MPLUS) $I $NUM ((MMINUS) $LEN))))
				      $LIST
				      $I))))
	      $LIST)
	    '$LIS2
	    '$LEN))
	 EXPR)
 	(ARGS '$CYC '(NIL . 2))
 	(DEFPROP $CYC T TRANSLATED)
 	(ADD2LNC '$CYC $PROPS)
 	(MDEFPROP $CYC
		  ((LAMBDA)
		   ((MLIST) $LIST $NUM)
		   ((MPROG)
		    ((MLIST) $LIS2 $LEN)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MCOND)
		     ((MLESSP) $NUM 0)
		     ((MSETQ) $NUM ((MPLUS) $LEN $NUM))
		     T
		     $FALSE)
		    ((MSETQ) $LIS2 $LIST)
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     $LEN
		     NIL
		     ((MSETQ)
		      $LIST
		      (($SUBSTPART)
		       (($LIS2 ARRAY)
			((MCOND)
			 ((MLEQP) ((MPLUS) $I $NUM) $LEN)
			 ((MPLUS) $I $NUM)
			 T
			 ((MPLUS) $I $NUM ((MMINUS) $LEN))))
		       $LIST
		       $I)))
		    ((MRETURN) $LIST)))
		  MEXPR)
 	(ARGS '$CYC '(NIL . 2))
 	(ADD2LNC '(($CYC) $LIST $NUM) $FUNCTIONS)
 	(DEFPROP
	 $TOOBIG
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($ABL $LIS3 $LIS $B1 $B2 $I $BIN $LIS2) 
	      (PROG NIL 
		    (SETQ $LIS ($REST $LIST 2))
		    (SETQ $B1 ($FIRST $LIST))
		    (SETQ $B2 ($PART $LIST 2))
		    (COND ((AND (LIKE $B1 ($FIRST $LIS))
				(LIKE $B2 ($LAST $LIS)))
			   (SETQ $ABL (MUL* (SIMPLIFY ($SOR $B1 $B1))
					    (SIMPLIFY ($SOR $B2 $B2))))
			   (SETQ $LIS3 ($REST ($REST $LIS -1) 1))
			   (GO $LEV)))
		    (COND ((LIKE $B1 ($FIRST $LIS))
			   (SETQ $ABL (SIMPLIFY ($SOR $B1 $B1)))
			   (SETQ $LIS3 ($APPEND (LIST '(MLIST) $B2)
						($REST $LIS 1)))
			   (GO $LEV)))
		    (COND ((LIKE $B2 ($LAST $LIS))
			   (SETQ $ABL (SIMPLIFY ($SOR $B2 $B2)))
			   (SETQ $LIS3 ($APPEND (LIST '(MLIST) $B1)
						($REST $LIS -1)))
			   (GO $LEV)))
		    (SETQ $ABL 1)
		    (SETQ $LIS3 ($APPEND (LIST '(MLIST) $B2 $B1) $LIS))
	       $LEV (SETQ $BIN (COND (($MEMBER $B1 (MEVAL1 '$IND)) $B1)
				     (T $B2)))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((NOT (LIKE ($PART $LIS $I) $BIN)) (GO $LOOP)))
		    (SETQ $LIS2 (SIMPLIFY ($LISBRE $LIS $I)))
		    (RETURN
		     (ADD*
		      (MUL* 2
			    (SIMPLIFY ($TR0 ($APPEND ($PART $LIS2 1)
						     (LIST '(MLIST)
							   (COND ((LIKE $B1
									$BIN)
								  $B2)
								 (T $B1)))
						     ($PART $LIS2 2)))))
		      (SIMPLIFY (LIST '(MMINUS)
				      (MUL* $ABL (SIMPLIFY ($TR0 $LIS3)))))))))
	    '$ABL
	    '$LIS3
	    '$LIS
	    '$B1
	    '$B2
	    '$I
	    '$BIN
	    '$LIS2))
	 EXPR)
 	(ARGS '$TOOBIG '(NIL . 1))
 	(DEFPROP $TOOBIG T TRANSLATED)
 	(ADD2LNC '$TOOBIG $PROPS)
 	(MDEFPROP $TOOBIG
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $ABL $LIS3 $LIS $B1 $B2 $I $BIN $LIS2)
		    ((MSETQ) $LIS (($REST) $LIST 2))
		    ((MSETQ) $B1 (($FIRST) $LIST))
		    ((MSETQ) $B2 (($PART) $LIST 2))
		    ((MCOND)
		     ((MAND)
		      ((MEQUAL) $B1 (($FIRST) $LIS))
		      ((MEQUAL) $B2 (($LAST) $LIS)))
		     ((DOLIST)
		      ((MSETQ)
		       $ABL
		       ((MTIMES) (($SOR) $B1 $B1) (($SOR) $B2 $B2)))
		      ((MSETQ) $LIS3 (($REST) (($REST) $LIS ((MMINUS) 1)) 1))
		      ((MGO) $LEV))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) $B1 (($FIRST) $LIS))
		     ((DOLIST)
		      ((MSETQ) $ABL (($SOR) $B1 $B1))
		      ((MSETQ)
		       $LIS3
		       (($APPEND) ((MLIST) $B2) (($REST) $LIS 1)))
		      ((MGO) $LEV))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) $B2 (($LAST) $LIS))
		     ((DOLIST)
		      ((MSETQ) $ABL (($SOR) $B2 $B2))
		      ((MSETQ)
		       $LIS3
		       (($APPEND) ((MLIST) $B1) (($REST) $LIS ((MMINUS) 1))))
		      ((MGO) $LEV))
		     T
		     $FALSE)
		    ((MSETQ) $ABL 1)
		    ((MSETQ) $LIS3 (($APPEND) ((MLIST) $B2 $B1) $LIS))
		    $LEV
		    ((MSETQ) $BIN ((MCOND) (($MEMBER) $B1 $IND) $B1 T $B2))
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MNOTEQUAL) (($PART) $LIS $I) $BIN)
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MSETQ) $LIS2 (($LISBRE) $LIS $I))
		    ((MRETURN)
		     ((MPLUS)
		      ((MTIMES)
		       2
		       (($TR0)
			(($APPEND)
			 (($PART) $LIS2 1)
			 ((MLIST) ((MCOND) ((MEQUAL) $B1 $BIN) $B2 T $B1))
			 (($PART) $LIS2 2))))
		      ((MMINUS) ((MTIMES) $ABL (($TR0) $LIS3)))))))
		  MEXPR)
 	(ARGS '$TOOBIG '(NIL . 1))
 	(ADD2LNC '(($TOOBIG) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $REAR
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($ALB $IMIN $LIST1 $CON $MINLEN $FORB $I $LEN $NOTIN $P) 
	      (PROG NIL 
		    (SETQ $NOTIN '((MLIST)))
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $ALB (SIMPLIFY ($CONUM $LIST ($FIRST $LIST))))
		    (SETQ $IMIN 1)
		    (SETQ $MINLEN
			  (MINIMUM (LIST $ALB
					 (ADD* $LEN
					       (SIMPLIFY (LIST '(MMINUS)
							       $ALB))
					       -2))))
		    (SETQ $FORB
			  (COND ((IS (MLSP $ALB
					   (ADD* $LEN
						 (SIMPLIFY (LIST '(MMINUS)
								 $ALB))
						 -2)))
				 0)
				(T -1)))
		    (SETQ $I 1)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((IS (MGRP $I $LEN)) (GO $LO1)))
		    (SETQ $P ($PART $LIST $I))
		    (COND ((AND ($MEMBER $P (MEVAL1 '$IND))
				(NOT ($MEMBER $P $NOTIN))
				(LIKE ($LENGTH (SIMPLIFY ($DELETE $P $LIST)))
				      (ADD* $LEN -2)))
			   (SETQ $LIST1 (SIMPLIFY ($CYC $LIST (ADD* $I -1)))))
			  (T (GO $LOOP)))
		    (SETQ $CON (SIMPLIFY ($CONUM $LIST1 $P)))
		    (COND ((OR (IS (MLSP $CON 3))
			       (IS (MLSP (ADD* $LEN
					       (SIMPLIFY (LIST '(MMINUS)
							       $CON))
					       -2)
					 3)))
			   (RETURN (SIMPLIFY ($LISCUT $LIST1)))))
		    (SETQ $NOTIN ($APPEND (LIST '(MLIST) $P) $NOTIN))
		    (COND
		     ((IS
		       (MLSP (MINIMUM (LIST $CON
					    (ADD* $LEN
						  (SIMPLIFY (LIST '(MMINUS)
								  $CON))
						  -2)))
			     $MINLEN))
		      (SETQ $MINLEN
			    (MINIMUM (LIST $CON
					   (ADD* $LEN
						 (SIMPLIFY (LIST '(MMINUS)
								 $CON))
						 -2))))
		      (SETQ $IMIN $I)
		      (SETQ 
		       $FORB
		       (COND ((IS (MLSP $CON
					(ADD* $LEN
					      (SIMPLIFY (LIST '(MMINUS)
							      $CON))
					      -2)))
			      0)
			     (T -1)))))
		    (GO $LOOP)
	       $LO1 (RETURN
		     (COND
		      ((IS (MLSP $MINLEN 6))
		       (RETURN
			(SIMPLIFY ($LISCUT (SIMPLIFY ($CYC $LIST
							   (ADD* $IMIN
								 -1)))))))
		      (T
		       (SIMPLIFY
			($THROW
			 (SIMPLIFY
			  ($TOOBIG (SIMPLIFY ($CYC $LIST
						   (ADD* $IMIN
							 -1
							 $FORB))))))))))))
	    '$ALB
	    '$IMIN
	    '$LIST1
	    '$CON
	    '$MINLEN
	    '$FORB
	    '$I
	    '$LEN
	    '$NOTIN
	    '$P))
	 EXPR)
 	(ARGS '$REAR '(NIL . 1))
 	(DEFPROP $REAR T TRANSLATED)
 	(ADD2LNC '$REAR $PROPS)
 	(MDEFPROP $REAR
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST)
		     $ALB
		     $IMIN
		     $LIST1
		     $CON
		     $MINLEN
		     $FORB
		     $I
		     $LEN
		     $NOTIN
		     $P)
		    ((MSETQ) $NOTIN ((MLIST)))
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $ALB (($CONUM) $LIST (($FIRST) $LIST)))
		    ((MSETQ) $IMIN 1)
		    ((MSETQ)
		     $MINLEN
		     (($MIN) $ALB ((MPLUS) $LEN ((MMINUS) $ALB) ((MMINUS) 2))))
		    ((MSETQ)
		     $FORB
		     ((MCOND)
		      ((MLESSP)
		       $ALB
		       ((MPLUS) $LEN ((MMINUS) $ALB) ((MMINUS) 2)))
		      0
		      T
		      ((MMINUS) 1)))
		    ((MSETQ) $I 1)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND) ((MGREATERP) $I $LEN) ((MGO) $LO1) T $FALSE)
		    ((MSETQ) $P (($PART) $LIST $I))
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) $P $IND)
		      ((MNOT) (($MEMBER) $P $NOTIN))
		      ((MEQUAL)
		       (($LENGTH) (($DELETE) $P $LIST))
		       ((MPLUS) $LEN ((MMINUS) 2))))
		     ((MSETQ) $LIST1 (($CYC) $LIST ((MPLUS) $I ((MMINUS) 1))))
		     T
		     ((MGO) $LOOP))
		    ((MSETQ) $CON (($CONUM) $LIST1 $P))
		    ((MCOND)
		     ((MOR)
		      ((MLESSP) $CON 3)
		      ((MLESSP) ((MPLUS) $LEN ((MMINUS) $CON) ((MMINUS) 2)) 3))
		     ((MRETURN) (($LISCUT) $LIST1))
		     T
		     $FALSE)
		    ((MSETQ) $NOTIN (($APPEND) ((MLIST) $P) $NOTIN))
		    ((MCOND)
		     ((MLESSP)
		      (($MIN) $CON ((MPLUS) $LEN ((MMINUS) $CON) ((MMINUS) 2)))
		      $MINLEN)
		     ((DOLIST)
		      ((MSETQ)
		       $MINLEN
		       (($MIN)
			$CON
			((MPLUS) $LEN ((MMINUS) $CON) ((MMINUS) 2))))
		      ((MSETQ) $IMIN $I)
		      ((MSETQ)
		       $FORB
		       ((MCOND)
			((MLESSP)
			 $CON
			 ((MPLUS) $LEN ((MMINUS) $CON) ((MMINUS) 2)))
			0
			T
			((MMINUS) 1))))
		     T
		     $FALSE)
		    ((MGO) $LOOP)
		    $LO1
		    ((MCOND)
		     ((MLESSP) $MINLEN 6)
		     ((MRETURN)
		      (($LISCUT) (($CYC) $LIST ((MPLUS) $IMIN ((MMINUS) 1)))))
		     T
		     (($THROW)
		      (($TOOBIG)
		       (($CYC) $LIST ((MPLUS) $IMIN ((MMINUS) 1) $FORB)))))))
		  MEXPR)
 	(ARGS '$REAR '(NIL . 1))
 	(ADD2LNC '(($REAR) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $PLATES
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($YESTES $LEN $I $PL $EVLIS) 
	      (PROG NIL 
		    (SETQ $LEN ($LENGTH $LIST))
		    (SETQ $YESTES ($PART (MEVAL1 '$PLAT)
					 (ADD* (DIV $LEN 2) -2)))
		    (COND ((LIKE $YESTES T) (GO $ON))
			  ((LIKE $YESTES (MEVAL1 '$END)) (GO $SUIC)))
		    (SIMPLIFY (MAPPLY (MEVAL1 '$LOADFILE)
				      (CDR (LIST '(MLIST)
						 (MEVAL1 '$GAM)
						 (MEVAL1 '$PLV)
						 (MEVAL1 '$SWOLF)))
				      '$LOADFILE))
		    (GO $ON)
	       $SUIC(SIMPLIFY ($PRINT (MEVAL1 '|&ENTERING ANTIC0 WITH |)
				      $LEN
				      (MEVAL1 '|& UNCONTRACTED GAMMAS:/
|)				      $LIST
				      (MEVAL1 '|&/
MAY BE SUICIDAL.|)))(RETURN (SIMPLIFY ($ANTIC $LIST)))
	       $ON  (SETQ $EVLIS '((MLIST)))
		    (DO (($I 1 (+ 1 $I))) 
			((IS (MGRP $I $LEN)) '$DONE) 
		     (SETQ $EVLIS
		      ($APPEND $EVLIS
			       (LIST '(MLIST)
				     (MEVAL '((MEQUAL)
					      (($CONCAT) $VQ $I)
					      (($PART) $LIST $I)))))))
		    (RETURN
		     (SIMPLIFY
		      (MAPPLY (MEVAL1 '$EV)
			      (CDR (LIST '(MLIST)
					 (SIMPLIFY (MARRAYREF '$PLAN
							      (ADD* (DIV $LEN
									 2)
								    -2)))
					 $EVLIS))
			      '$EV)))))
	    '$YESTES
	    '$LEN
	    '$I
	    '$PL
	    '$EVLIS))
	 EXPR)
 	(ARGS '$PLATES '(NIL . 1))
 	(DEFPROP $PLATES T TRANSLATED)
 	(ADD2LNC '$PLATES $PROPS)
 	(MDEFPROP $PLATES
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $YESTES $LEN $I $PL $EVLIS)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ)
		     $YESTES
		     (($PART)
		      $PLAT
		      ((MPLUS) ((MQUOTIENT) $LEN 2) ((MMINUS) 2))))
		    ((MCOND)
		     ((MEQUAL) $YESTES $TRUE)
		     ((MGO) $ON)
		     T
		     ((MCOND) ((MEQUAL) $YESTES $END) ((MGO) $SUIC) T $FALSE))
		    (($APPLY) $LOADFILE ((MLIST) $GAM $PLV $SWOLF))
		    ((MGO) $ON)
		    $SUIC
		    (($PRINT)
		     |&ENTERING ANTIC0 WITH |
		     $LEN
		     |& UNCONTRACTED GAMMAS:/
|		     $LIST
		     |&/
MAY BE SUICIDAL.|)  ((MRETURN) (($ANTIC) $LIST))
		    $ON
		    ((MSETQ) $EVLIS ((MLIST)))
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     $LEN
		     NIL
		     ((MSETQ)
		      $EVLIS
		      (($APPEND)
		       $EVLIS
		       ((MLIST)
			((MEQUAL) (($CONCAT) $VQ $I) (($PART) $LIST $I))))))
		    ((MRETURN)
		     (($APPLY)
		      $EV
		      ((MLIST)
		       (($PLAN ARRAY)
			((MPLUS) ((MQUOTIENT) $LEN 2) ((MMINUS) 2)))
		       $EVLIS)))))
		  MEXPR)
 	(ARGS '$PLATES '(NIL . 1))
 	(ADD2LNC '(($PLATES) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $PACHE
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($I $PI $LEN) 
	      (PROG NIL 
		    (SETQ $LEN ($LENGTH $LIST))
		    (COND
		     ((LIKE ($FIRST $LIST) ($LAST $LIST))
		      (RETURN
		       (SIMPLIFY
			($TELL (MUL* (SIMPLIFY ($SOR ($FIRST $LIST)
						     ($FIRST $LIST)))
				     (SIMPLIFY ($ANTIC ($REST ($REST $LIST 1)
							      -1))))
			       $LEN)))))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((IS (MGRP $I $LEN))
			   (RETURN (SIMPLIFY ($TELL (SIMPLIFY ($ANTIC $LIST))
						    $LEN)))))
		    (COND ((NOT (LIKE ($PART $LIST $I)
				      ($PART $LIST (ADD* $I 1))))
			   (GO $LOOP)))
		    (SETQ $PI ($PART $LIST $I))
		    (RETURN
		     (SIMPLIFY
		      ($TELL
		       (MUL*
			(SIMPLIFY ($SOR $PI $PI))
			(SIMPLIFY
			 ($ANTIC
			  ($APPEND ($REST $LIST
					  (ADD* (SIMPLIFY (LIST '(MMINUS)
								$LEN))
						$I
						-1))
				   ($REST $LIST (ADD* $I 1))))))
		       $LEN)))))
	    '$I
	    '$PI
	    '$LEN))
	 EXPR)
 	(ARGS '$PACHE '(NIL . 1))
 	(DEFPROP $PACHE T TRANSLATED)
 	(ADD2LNC '$PACHE $PROPS)
 	(MDEFPROP $PACHE
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $I $PI $LEN)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MCOND)
		     ((MEQUAL) (($FIRST) $LIST) (($LAST) $LIST))
		     ((MRETURN)
		      (($TELL)
		       ((MTIMES)
			(($SOR) (($FIRST) $LIST) (($FIRST) $LIST))
			(($ANTIC) (($REST) (($REST) $LIST 1) ((MMINUS) 1))))
		       $LEN))
		     T
		     $FALSE)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MGREATERP) $I $LEN)
		     ((MRETURN) (($TELL) (($ANTIC) $LIST) $LEN))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOTEQUAL)
		      (($PART) $LIST $I)
		      (($PART) $LIST ((MPLUS) $I 1)))
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MSETQ) $PI (($PART) $LIST $I))
		    ((MRETURN)
		     (($TELL)
		      ((MTIMES)
		       (($SOR) $PI $PI)
		       (($ANTIC)
			(($APPEND)
			 (($REST)
			  $LIST
			  ((MPLUS) ((MMINUS) $LEN) $I ((MMINUS) 1)))
			 (($REST) $LIST ((MPLUS) $I 1)))))
		      $LEN))))
		  MEXPR)
 	(ARGS '$PACHE '(NIL . 1))
 	(ADD2LNC '(($PACHE) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $ANTIC
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($LEN $I $L1 $BIT1 $SYQ) 
	      (PROG NIL 
		    (SETQ $LEN ($LENGTH $LIST))
		    (COND ((IS (MLSP $LEN 5))
			   (RETURN (SIMPLIFY ($TRIV $LIST)))))
		    (SETQ $BIT1 ($PART $LIST 1))
		    (SETQ $SYQ 0)
		    (SETQ $I 1)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((IS (MGRP $I $LEN)) (RETURN $SYQ)))
		    (SETQ $L1 (SIMPLIFY ($LISBRE ($REST $LIST 1) (ADD* $I -1))))
		    (SETQ 
		     $SYQ
		     (ADD* $SYQ
			   (MUL* (SIMPLIFY ($SOR $BIT1 ($PART $LIST $I)))
				 (POWER -1 $I)
				 (SIMPLIFY ($PACHE ($APPEND ($PART $L1 1)
							    ($PART $L1 2)))))))
		    (GO $LOOP)))
	    '$LEN
	    '$I
	    '$L1
	    '$BIT1
	    '$SYQ))
	 EXPR)
 	(ARGS '$ANTIC '(NIL . 1))
 	(DEFPROP $ANTIC T TRANSLATED)
 	(ADD2LNC '$ANTIC $PROPS)
 	(MDEFPROP $ANTIC
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $LEN $I $L1 $BIT1 $SYQ)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MCOND)
		     ((MLESSP) $LEN 5)
		     ((MRETURN) (($TRIV) $LIST))
		     T
		     $FALSE)
		    ((MSETQ) $BIT1 (($PART) $LIST 1))
		    ((MSETQ) $SYQ 0)
		    ((MSETQ) $I 1)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND) ((MGREATERP) $I $LEN) ((MRETURN) $SYQ) T $FALSE)
		    ((MSETQ)
		     $L1
		     (($LISBRE) (($REST) $LIST 1) ((MPLUS) $I ((MMINUS) 1))))
		    ((MSETQ)
		     $SYQ
		     ((MPLUS)
		      $SYQ
		      ((MTIMES)
		       (($SOR) $BIT1 (($PART) $LIST $I))
		       ((MEXPT) ((MMINUS) 1) $I)
		       (($PACHE)
			(($APPEND) (($PART) $L1 1) (($PART) $L1 2))))))
		    ((MGO) $LOOP)))
		  MEXPR)
 	(ARGS '$ANTIC '(NIL . 1))
 	(ADD2LNC '(($ANTIC) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $TELL
	 (LAMBDA ($EXP $LEN) 
	   (PROG NIL 
		 (COND ((OR (NOT (IS (MEVAL1 '$BORED)))
			    (IS (MGRP (MEVAL1 '$BORELEN) $LEN)))
			(RETURN $EXP)))
		 (SIMPLIFY ($PRINT (MEVAL1 '|&EXITING TR0 WITH|)
				   ($NTERMS $EXP)
				   (MEVAL1 '|&TERMS.|)
				   (SIMPLIFY ($STATUS $FREECORE))
				   (MEVAL1 '|&CORE FREE. TIME =|)
				   (SIMPLIFY ($STATUS $TIME))))
		 (RETURN $EXP)))
	 EXPR)
 	(ARGS '$TELL '(NIL . 2))
 	(DEFPROP $TELL T TRANSLATED)
 	(ADD2LNC '$TELL $PROPS)
 	(MDEFPROP $TELL
		  ((LAMBDA)
		   ((MLIST) $EXP $LEN)
		   ((MPROG)
		    ((MLIST))
		    ((MCOND)
		     ((MOR) ((MNOT) $BORED) ((MGREATERP) $BORELEN $LEN))
		     ((MRETURN) $EXP)
		     T
		     $FALSE)
		    (($PRINT)
		     |&EXITING TR0 WITH|
		     (($NTERMS) $EXP)
		     |&TERMS.|
		     (($STATUS) $FREECORE)
		     |&CORE FREE. TIME =|
		     (($STATUS) $TIME))
		    ((MRETURN) $EXP)))
		  MEXPR)
 	(ARGS '$TELL '(NIL . 2))
 	(ADD2LNC '(($TELL) $EXP $LEN) $FUNCTIONS)
 	(DEFPROP $CGT
		 (LAMBDA ($EXP) 
		   (SIMPLIFY ($EV $EXP ((MEQUAL) $G $TR) ((MEQUAL) $GT $TR))))
		 EXPR)
 	(ARGS '$CGT '(NIL . 1))
 	(DEFPROP $CGT T TRANSLATED)
 	(ADD2LNC '$CGT $PROPS)
 	(MDEFPROP $CGT
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   (($EV) $EXP ((MEQUAL) $G $TR) ((MEQUAL) $GT $TR)))
		  MEXPR)
 	(ARGS '$CGT '(NIL . 1))
 	(ADD2LNC '(($CGT) $EXP) $FUNCTIONS)
 	(DEFPROP
	 $DFIX
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($QW) 
	      (SETQ $QW (SIMPLIFY ($SUBSTITUTE (MEVAL1 '$D0)
					       (MEVAL1 '$D)
					       $EXP)))
	      (SIMPLIFY ($EV $QW)))
	    '$QW))
	 EXPR)
 	(ARGS '$DFIX '(NIL . 1))
 	(DEFPROP $DFIX T TRANSLATED)
 	(ADD2LNC '$DFIX $PROPS)
 	(MDEFPROP $DFIX
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $QW)
		    ((MSETQ) $QW (($SUBSTITUTE) $D0 $D $EXP))
		    ((MRETURN) (($EV) $QW))))
		  MEXPR)
 	(ARGS '$DFIX '(NIL . 1))
 	(ADD2LNC '(($DFIX) $EXP) $FUNCTIONS)
 	(DEFPROP
	 $NSET
	 (LAMBDA ($DIM) 
	   ((LAMBDA ($LVN) 
	      (SETQ $N $DIM)
	      (COND ((LIKE $DIM 4)
		     (SETQ $CONTAB (MEVAL1 '$CONTAB4))
		     (SETQ $CONTLIM 10534))
		    (T (SETQ $CONTAB (SIMPLIFY ($EV $CONTAB
						    ((MEQUAL) $N $DIM)
						    $INFEVAL)))))
	      (SIMPLIFY ($PRINT (MEVAL1 '|&DIMENSIONS =|)
				(MEVAL1 '$N)))
	      (SETQ $LVN (SIMPLIFY ($LISTOFVARS (MEVAL1 '$N))))
	      (DO (($I 1 (+ 1 $I))) 
		  ((> $I ($LENGTH $LVN)) '$DONE) 
	       (SIMPLIFY
		(MAPPLY (MEVAL1 '$DECLARE)
			(CDR (LIST '(MLIST)
				   (SIMPLIFY (MARRAYREF $LVN $I))
				   (MEVAL1 '$CONSTANT)))
			'$DECLARE)))
	      (MEVAL1 '$N))
	    '$LVN))
	 EXPR)
 	(ARGS '$NSET '(NIL . 1))
 	(DEFPROP $NSET T TRANSLATED)
 	(ADD2LNC '$NSET $PROPS)
 	(MDEFPROP $NSET
		  ((LAMBDA)
		   ((MLIST) $DIM)
		   ((MPROG)
		    ((MLIST) $LVN)
		    ((MSETQ) $N $DIM)
		    ((MCOND)
		     ((MEQUAL) $DIM 4)
		     ((DOLIST)
		      ((MSETQ) $CONTAB $CONTAB4)
		      ((MSETQ) $CONTLIM 10534))
		     T
		     ((MSETQ)
		      $CONTAB
		      (($EV) $CONTAB ((MEQUAL) $N $DIM) $INFEVAL)))
		    (($PRINT) |&DIMENSIONS =| $N)
		    ((MSETQ) $LVN (($LISTOFVARS) $N))
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH SIMP) $LVN)
		     NIL
		     (($APPLY) $DECLARE ((MLIST) (($LVN ARRAY) $I) $CONSTANT)))
		    $N))
		  MEXPR)
 	(ARGS '$NSET '(NIL . 1))
 	(ADD2LNC '(($NSET) $DIM) $FUNCTIONS)
 	(DEFPROP
	 $NEWL
	 (LAMBDA ($IN $CON $OUT) 
	   ((LAMBDA ($ONE $TWO) 
	      (PROG NIL 
		    (COND ((NOT (LIKE $CON '((MLIST))))
			   (RETURN (LIST '(MLIST)
					 1
					 ($APPEND (SIMPLIFY ($NLIS $IN $CON))
						  $OUT)))))
		    (COND ((LIKE $IN '((MLIST)))
			   (RETURN (LIST '(MLIST) 1 $OUT))))
		    (COND
		     ((NOT (= ($LENGTH $IN) 2))
		      (SIMPLIFY ($ERROR (MEVAL1 '|&SCREW /
UP IN NEWL|)				$IN
					$OUT))))
		    (SETQ $ONE ($PART $IN 1))
		    (SETQ $TWO ($PART $IN 2))
		    (COND ((OR ($MEMBER $ONE (MEVAL1 '$IND))
			       ($MEMBER $TWO (MEVAL1 '$IND)))
			   (GO $FUR)))
		    (RETURN (LIST '(MLIST)
				  (SIMPLIFY ($SOR $ONE $TWO))
				  $OUT))
	       $FUR (COND ((AND ($MEMBER $ONE (MEVAL1 '$IND))
				($MEMBER $TWO (MEVAL1 '$IND))
				(LIKE $ONE $TWO))
			   (RETURN (LIST '(MLIST)
					 (MEVAL1 '$N)
					 $OUT))))
		    (COND ((AND ($MEMBER $ONE (MEVAL1 '$IND))
				(NOT (IS ($FREEOF $ONE $OUT))))
			   (RETURN (LIST '(MLIST)
					 1
					 (SIMPLIFY ($NEWC $ONE $TWO $OUT))))))
		    (COND ((AND ($MEMBER $TWO (MEVAL1 '$IND))
				(NOT (IS ($FREEOF $TWO $OUT))))
			   (RETURN (LIST '(MLIST)
					 1
					 (SIMPLIFY ($NEWC $TWO $ONE $OUT))))))
		    (RETURN (LIST '(MLIST)
				  (SIMPLIFY ($SOR $ONE $TWO))
				  $OUT))))
	    '$ONE
	    '$TWO))
	 EXPR)
 	(ARGS '$NEWL '(NIL . 3))
 	(DEFPROP $NEWL T TRANSLATED)
 	(ADD2LNC '$NEWL $PROPS)
 	(MDEFPROP $NEWL
		  ((LAMBDA)
		   ((MLIST) $IN $CON $OUT)
		   ((MPROG)
		    ((MLIST) $ONE $TWO)
		    ((MCOND)
		     ((MNOTEQUAL) $CON ((MLIST)))
		     ((MRETURN)
		      ((MLIST) 1 (($APPEND) (($NLIS) $IN $CON) $OUT)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MEQUAL) $IN ((MLIST)))
		     ((MRETURN) ((MLIST) 1 $OUT))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOTEQUAL) (($LENGTH) $IN) 2)
		     (($ERROR) |&SCREW /
UP IN NEWL| $IN $OUT)T
		     $FALSE)
		    ((MSETQ) $ONE (($PART) $IN 1))
		    ((MSETQ) $TWO (($PART) $IN 2))
		    ((MCOND)
		     ((MOR) (($MEMBER) $ONE $IND) (($MEMBER) $TWO $IND))
		     ((MGO) $FUR)
		     T
		     $FALSE)
		    ((MRETURN) ((MLIST) (($SOR) $ONE $TWO) $OUT))
		    $FUR
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) $ONE $IND)
		      (($MEMBER) $TWO $IND)
		      ((MEQUAL) $ONE $TWO))
		     ((MRETURN) ((MLIST) $N $OUT))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) $ONE $IND)
		      ((MNOT) (($FREEOF) $ONE $OUT)))
		     ((MRETURN) ((MLIST) 1 (($NEWC) $ONE $TWO $OUT)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) $TWO $IND)
		      ((MNOT) (($FREEOF) $TWO $OUT)))
		     ((MRETURN) ((MLIST) 1 (($NEWC) $TWO $ONE $OUT)))
		     T
		     $FALSE)
		    ((MRETURN) ((MLIST) (($SOR) $ONE $TWO) $OUT))))
		  MEXPR)
 	(ARGS '$NEWL '(NIL . 3))
 	(ADD2LNC '(($NEWL) $IN $CON $OUT) $FUNCTIONS)
 	(DEFPROP $DP
		 (LAMBDA ($OBSA $OBSB) 
		   (COND ((IS ($ORDERGREATP $OBSA $OBSB))
			  (SIMPLIFY ($DSIM $OBSB $OBSA)))
			 (T (SIMPLIFY ($DSIM $OBSA $OBSB)))))
		 EXPR)
 	(ARGS '$DP '(NIL . 2))
 	(DEFPROP $DP T TRANSLATED)
 	(ADD2LNC '$DP $PROPS)
 	(MDEFPROP $DP
		  ((LAMBDA)
		   ((MLIST) $OBSA $OBSB)
		   ((MCOND)
		    (($ORDERGREATP) $OBSA $OBSB)
		    (($DSIM) $OBSB $OBSA)
		    T
		    (($DSIM) $OBSA $OBSB)))
		  MEXPR)
 	(ARGS '$DP '(NIL . 2))
 	(ADD2LNC '(($DP) $OBSA $OBSB) $FUNCTIONS)
 	(DEFPROP $SOR0
		 (LAMBDA ($OBSA $OBSB) 
		   (COND ((IS ($ORDERGREATP $OBSA $OBSB))
			  (SIMPLIFY ($DSIM $OBSB $OBSA)))
			 (T (SIMPLIFY ($DSIM $OBSA $OBSB)))))
		 EXPR)
 	(ARGS '$SOR0 '(NIL . 2))
 	(DEFPROP $SOR0 T TRANSLATED)
 	(ADD2LNC '$SOR0 $PROPS)
 	(MDEFPROP $SOR0
		  ((LAMBDA)
		   ((MLIST) $OBSA $OBSB)
		   ((MCOND)
		    (($ORDERGREATP) $OBSA $OBSB)
		    (($DSIM) $OBSB $OBSA)
		    T
		    (($DSIM) $OBSA $OBSB)))
		  MEXPR)
 	(ARGS '$SOR0 '(NIL . 2))
 	(ADD2LNC '(($SOR0) $OBSA $OBSB) $FUNCTIONS)
 	(DEFPROP $DSIM
		 (LAMBDA ($A $B) 
		   (COND ((OR (NOT (IS (MEVAL1 '$DSIMP)))
			      (NOT ($MEMBER (LIST '(MLIST) $A $B)
					    (MEVAL1 '$KINSARG))))
			  (MEVAL '(($D) $A $B)))
			 (T (SIMPLIFY (MARRAYREF '$DHASH $A $B)))))
		 EXPR)
 	(ARGS '$DSIM '(NIL . 2))
 	(DEFPROP $DSIM T TRANSLATED)
 	(ADD2LNC '$DSIM $PROPS)
 	(MDEFPROP $DSIM
		  ((LAMBDA)
		   ((MLIST) $A $B)
		   ((MCOND)
		    ((MOR)
		     ((MNOT) $DSIMP)
		     ((MNOT) (($MEMBER) ((MLIST) $A $B) $KINSARG)))
		    (($D) $A $B)
		    T
		    (($DHASH ARRAY) $A $B)))
		  MEXPR)
 	(ARGS '$DSIM '(NIL . 2))
 	(ADD2LNC '(($DSIM) $A $B) $FUNCTIONS)
 	(DEFPROP $NAP
		 (LAMBDA ($LIST $BIT) 
		   (COND (($LISTP $BIT) ($APPEND $LIST $BIT))
			 (T ($APPEND $LIST (LIST '(MLIST) $BIT)))))
		 EXPR)
 	(ARGS '$NAP '(NIL . 2))
 	(DEFPROP $NAP T TRANSLATED)
 	(ADD2LNC '$NAP $PROPS)
 	(MDEFPROP $NAP
		  ((LAMBDA)
		   ((MLIST) $LIST $BIT)
		   ((MCOND)
		    (($LISTP) $BIT)
		    (($APPEND) $LIST $BIT)
		    T
		    (($APPEND) $LIST ((MLIST) $BIT))))
		  MEXPR)
 	(ARGS '$NAP '(NIL . 2))
 	(ADD2LNC '(($NAP) $LIST $BIT) $FUNCTIONS)
 	(DEFPROP $KAHANE
		 (LAMBDA ($LIST) 
		   (SIMPLIFY ($EV (($KAH0) $LIST) ((MEQUAL) $G $TR))))
		 EXPR)
 	(ARGS '$KAHANE '(NIL . 1))
 	(DEFPROP $KAHANE T TRANSLATED)
 	(ADD2LNC '$KAHANE $PROPS)
 	(MDEFPROP $KAHANE
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   (($EV) (($KAH0) $LIST) ((MEQUAL) $G $TR)))
		  MEXPR)
 	(ARGS '$KAHANE '(NIL . 1))
 	(ADD2LNC '(($KAHANE) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $UNCIND
	 (LAMBDA ($OUTLIS) 
	   (SETQ $OUTLIS (SIMPLIFY ($GETRED $OUTLIS)))
	   (DO (($I 1 (+ 1 $I))) 
	       ((> $I ($LENGTH $OUTLIS)) '$DONE) 
	    (SETQ $IND
	     (SIMPLIFY ($DELETE ($PART $OUTLIS $I) (MEVAL1 '$IND)))))
	   (MEVAL1 '$IND))
	 EXPR)
 	(ARGS '$UNCIND '(NIL . 1))
 	(DEFPROP $UNCIND T TRANSLATED)
 	(ADD2LNC '$UNCIND $PROPS)
 	(MDEFPROP $UNCIND T MLEXPRP)
 	(MDEFPROP $UNCIND
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $OUTLIS))
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ) $OUTLIS (($GETRED) $OUTLIS))
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH) $OUTLIS)
		     NIL
		     ((MSETQ) $IND (($DELETE) (($PART) $OUTLIS $I) $IND)))
		    $IND))
		  MEXPR)
 	(ARGS '$UNCIND '(NIL . 1))
 	(ADD2LNC '(($UNCIND) ((MLIST) $OUTLIS)) $FUNCTIONS)
 	(MDEFPROP $UNCIND
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($UNCIND (CONS '(MLIST)
						     (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($UNCIND (CONS '(MLIST)
					    (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)
 	(DEFPROP
	 $CIND
	 (LAMBDA ($NEWIND) 
	   (PROG NIL 
		 (SETQ $NEWIND (SIMPLIFY ($GETRED $NEWIND)))
		 (RETURN
		  (PROGN (DO (($I 1 (+ 1 $I))) 
			     ((> $I ($LENGTH $NEWIND)) '$DONE) 
			  (COND
			   ((IS ($FREEOF ($PART $NEWIND $I)
					 (MEVAL1 '$IND)))
			    (SETQ $IND ($APPEND (LIST '(MLIST)
						      ($PART $NEWIND $I))
						(MEVAL1 '$IND))))))
			 (RETURN (MEVAL1 '$IND))))))
	 EXPR)
 	(ARGS '$CIND '(NIL . 1))
 	(DEFPROP $CIND T TRANSLATED)
 	(ADD2LNC '$CIND $PROPS)
 	(MDEFPROP $CIND
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($CIND (CONS '(MLIST)
						   (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($CIND (CONS '(MLIST)
					  (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)
 	(MDEFPROP $CIND T MLEXPRP)
 	(MDEFPROP $CIND
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $NEWIND))
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ) $NEWIND (($GETRED) $NEWIND))
		    ((DOLIST)
		     ((MDO)
		      $I
		      NIL
		      NIL
		      NIL
		      (($LENGTH) $NEWIND)
		      NIL
		      ((MCOND)
		       (($FREEOF) (($PART) $NEWIND $I) $IND)
		       ((MSETQ)
			$IND
			(($APPEND) ((MLIST) (($PART) $NEWIND $I)) $IND))
		       T
		       $FALSE))
		     ((MRETURN) $IND))))
		  MEXPR)
 	(ARGS '$CIND '(NIL . 1))
 	(ADD2LNC '(($CIND) ((MLIST) $NEWIND)) $FUNCTIONS)
 	(DEFPROP
	 $GETRED
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($LISTARITH $NEW $I) 
	      (SETQ $LISTARITH T)
	      (SETQ $LIST (SIMPLIFY ($EV $LIST)))
	      (SETQ $NEW '((MLIST)))
	      (DO (($I 1 (+ 1 $I))) 
		  ((> $I ($LENGTH $LIST)) '$DONE) 
	       (SETQ $NEW
		(SIMPLIFY ($NAP $NEW (SIMPLIFY (MARRAYREF $LIST $I))))))
	      $NEW)
	    '$LISTARITH
	    '$NEW
	    '$I))
	 EXPR)
 	(ARGS '$GETRED '(NIL . 1))
 	(DEFPROP $GETRED T TRANSLATED)
 	(ADD2LNC '$GETRED $PROPS)
 	(MDEFPROP $GETRED
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $LISTARITH $NEW $I)
		    ((MSETQ) $LISTARITH $TRUE)
		    ((MSETQ) $LIST (($EV) $LIST))
		    ((MSETQ) $NEW ((MLIST)))
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH) $LIST)
		     NIL
		     ((MSETQ) $NEW (($NAP) $NEW (($LIST ARRAY) $I))))
		    ((MRETURN) $NEW)))
		  MEXPR)
 	(ARGS '$GETRED '(NIL . 1))
 	(ADD2LNC '(($GETRED) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $SCALS
	 (LAMBDA ($LIST) 
	   (SETQ $LIST (SIMPLIFY ($GETRED $LIST)))
	   (DO (($I 1 (+ 1 $I))) 
	       ((> $I ($LENGTH $LIST)) '$DONE) 
	    (COND
	     ((IS ($FREEOF ($PART $LIST $I) (MEVAL1 '$SCALARS)))
	      ((LAMBDA ($PIT) 
		 (SETQ $PIT ($PART $LIST $I))
		 (COND
		  (($MEMBER $PIT (MEVAL1 '$DANGER))
		   (SIMPLIFY
		    ($PRINT
		     $PIT
		     (MEVAL1
		      '|&HAS A MEANING IN GAMALG - YOU MAY CAUSE CONFUSION|)))))
		 (SETQ $SCALARS ($APPEND (MEVAL1 '$SCALARS)
					 (LIST '(MLIST) $PIT)))
		 (SIMPLIFY (MAPPLY (MEVAL1 '$DECLARE)
				   (CDR (LIST '(MLIST)
					      $PIT
					      (MEVAL1 '$CONSTANT)))
				   '$DECLARE)))
	       '$PIT))))
	   (MEVAL1 '$SCALARS))
	 EXPR)
 	(ARGS '$SCALS '(NIL . 1))
 	(DEFPROP $SCALS T TRANSLATED)
 	(ADD2LNC '$SCALS $PROPS)
 	(MDEFPROP $SCALS
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ) $LIST (($GETRED) $LIST))
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH) $LIST)
		     NIL
		     ((MCOND)
		      (($FREEOF) (($PART) $LIST $I) $SCALARS)
		      ((MPROG)
		       ((MLIST) $PIT)
		       ((MSETQ) $PIT (($PART) $LIST $I))
		       ((MCOND)
			(($MEMBER) $PIT $DANGER)
			(($PRINT)
			 $PIT
			 |&HAS A MEANING IN GAMALG - YOU MAY CAUSE CONFUSION|)
			T
			$FALSE)
		       ((MSETQ) $SCALARS (($APPEND) $SCALARS ((MLIST) $PIT)))
		       (($APPLY) $DECLARE ((MLIST) $PIT $CONSTANT)))
		      T
		      $FALSE))
		    ((MRETURN) $SCALARS)))
		  MEXPR)
 	(ARGS '$SCALS '(NIL . 1))
 	(ADD2LNC '(($SCALS) ((MLIST) $LIST)) $FUNCTIONS)
 	(MDEFPROP $SCALS T MLEXPRP)
 	(MDEFPROP $SCALS
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($SCALS (CONS '(MLIST)
						    (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($SCALS (CONS '(MLIST)
					   (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)
 	(DEFPROP $ZMAK0
		 (LAMBDA ($LIST) 
		   ((LAMBDA ($CST) 
		      (SETQ $CST 1)
		      (SETQ $LIST
			    (SIMPLIFY ($EV $LIST
					   ((MDEFINE) (($ZN) $X) $X)
					   ((MEQUAL) $ZD $ZD0))))
		      (MUL* $CST (SIMPLIFY ($TR0 $LIST))))
		    '$CST))
		 EXPR)
 	(ARGS '$ZMAK0 '(NIL . 1))
 	(DEFPROP $ZMAK0 T TRANSLATED)
 	(ADD2LNC '$ZMAK0 $PROPS)
 	(MDEFPROP $ZMAK0
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $CST)
		    ((MSETQ) $CST 1)
		    ((MSETQ)
		     $LIST
		     (($EV)
		      $LIST
		      ((MDEFINE) (($ZN) $X) $X)
		      ((MEQUAL) $ZD $ZD0)))
		    ((MRETURN) ((MTIMES) $CST (($TR0) $LIST)))))
		  MEXPR)
 	(ARGS '$ZMAK0 '(NIL . 1))
 	(ADD2LNC '(($ZMAK0) $LIST) $FUNCTIONS)
 	(DEFPROP $TRM
		 (LAMBDA ($LIST) 
		   (COND ((IS ($FREEOF (TRD-MSYMEVAL $ZD)
				       (TRD-MSYMEVAL $ZN)
				       (TRD-MSYMEVAL $ZA)
				       $LIST))
			  (SIMPLIFY ($TR0 $LIST)))
			 ((IS (TRD-MSYMEVAL $ZERM)) (SIMPLIFY ($ZMAK0 $LIST)))
			 ((AND (IS (TRD-MSYMEVAL $MTRICK))
			       (LIKE (TRD-MSYMEVAL $N) 4)
			       (IS ($EVENP ($LENGTH $LIST))))
			  (MEVAL '(($ZMAK4) $LIST)))
			 (T (SIMPLIFY ($ZMAK $LIST)))))
		 EXPR)
 	(ARGS '$TRM '(NIL . 1))
 	(DEFPROP $TRM T TRANSLATED)
 	(ADD2LNC '$TRM $PROPS)
 	(MDEFPROP $TRM
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MCOND)
		    (($FREEOF) $ZD $ZN $ZA $LIST)
		    (($TR0) $LIST)
		    T
		    ((MCOND)
		     $ZERM
		     (($ZMAK0) $LIST)
		     T
		     ((MCOND)
		      ((MAND)
		       $MTRICK
		       ((MEQUAL) $N 4)
		       (($EVENP) (($LENGTH) $LIST)))
		      (($ZMAK4) $LIST)
		      T
		      (($ZMAK) $LIST)))))
		  MEXPR)
 	(ARGS '$TRM '(NIL . 1))
 	(ADD2LNC '(($TRM) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $UNSCALS
	 (LAMBDA ($LIST) 
	   (SETQ $LIST (SIMPLIFY ($GETRED $LIST)))
	   (SIMPLIFY (MAPPLY (MEVAL1 '$KILL) (CDR $LIST) '$KILL))
	   (DO (($I 1 (+ 1 $I))) 
	       ((> $I ($LENGTH $LIST)) '$DONE) 
	    (SETQ $SCALARS
	     (SIMPLIFY ($DELETE (SIMPLIFY (MARRAYREF $LIST $I))
				(MEVAL1 '$SCALARS)))))
	   (MEVAL1 '$SCALARS))
	 EXPR)
 	(ARGS '$UNSCALS '(NIL . 1))
 	(DEFPROP $UNSCALS T TRANSLATED)
 	(ADD2LNC '$UNSCALS $PROPS)
 	(MDEFPROP $UNSCALS T MLEXPRP)
 	(MDEFPROP $UNSCALS
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ) $LIST (($GETRED) $LIST))
		    (($APPLY) $KILL $LIST)
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH) $LIST)
		     NIL
		     ((MSETQ)
		      $SCALARS
		      (($DELETE) (($LIST ARRAY) $I) $SCALARS)))
		    ((MRETURN) $SCALARS)))
		  MEXPR)
 	(ARGS '$UNSCALS '(NIL . 1))
 	(ADD2LNC '(($UNSCALS) ((MLIST) $LIST)) $FUNCTIONS)
 	(MDEFPROP $UNSCALS
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($UNSCALS (CONS '(MLIST)
						      (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($UNSCALS (CONS '(MLIST)
					     (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)
 	(DEFPROP
	 $UNKINDEF
	 (LAMBDA ($LIST) 
	   (SETQ $LIST (SIMPLIFY ($GETRED $LIST)))
	   (DO (($I 1 (+ 1 $I))) 
	       ((> $I ($LENGTH $LIST)) '$DONE) 
	    ((LAMBDA ($LI $L1 $L2 $DELIS) (PROG NIL (SETQ $LI ($PART $LIST $I)) (COND ((NOT (LIKE ($PART $LI 0) (MEVAL1 '
															 $D))) (RETURN (MEVAL1 '
																		$NOTD)))) (SETQ $L1 ($PART $LI 1)) (SETQ $L2 ($PART $LI 2)) (SETQ $DELIS (COND ((IS ($ORDERGREATP $L1 $L2)) (LIST '
																																   (
																																    MLIST) $L2 $L1)) (T (LIST '
																																			       (
																																				MLIST) $L1 $L2)))) (SETQ $KINSARG (SIMPLIFY ($DELETE $DELIS (MEVAL1 '
																																												     $KINSARG)))) (RETURN (SETQ $KINS (SIMPLIFY (MAPPLY (MEVAL1 '
																																																				 $DELETE) (CDR (LIST '
																																																						      (
																																																						       MLIST) (MEVAL '
																																																								      (
																																																								       (
																																																									MEQUAL)
																																																								       $LI
																																																								       (
																																																									(
																																																									 $DHASH
																																																									 ARRAY)
																																																									(
																																																									 (
																																																									  $DELIS
																																																									  ARRAY)
																																																									 1)
																																																									(
																																																									 (
																																																									  $DELIS
																																																									  ARRAY)
																																																									 2)))) (MEVAL1 '
																																																											$KINS))) '
																																																												  $DELETE))))))
	     '$LI '$L1 '$L2 '$DELIS))
	   (MEVAL1 '$KINS))
	 EXPR)
 	(ARGS '$UNKINDEF '(NIL . 1))
 	(DEFPROP $UNKINDEF T TRANSLATED)
 	(ADD2LNC '$UNKINDEF $PROPS)
 	(MDEFPROP $UNKINDEF T MLEXPRP)
 	(MDEFPROP $UNKINDEF
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ) $LIST (($GETRED) $LIST))
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH) $LIST)
		     NIL
		     ((MPROG)
		      ((MLIST) $LI $L1 $L2 $DELIS)
		      ((MSETQ) $LI (($PART) $LIST $I))
		      ((MCOND)
		       ((MNOTEQUAL) (($PART) $LI 0) $D)
		       ((MRETURN) $NOTD)
		       T
		       $FALSE)
		      ((MSETQ) $L1 (($PART) $LI 1))
		      ((MSETQ) $L2 (($PART) $LI 2))
		      ((MSETQ)
		       $DELIS
		       ((MCOND)
			(($ORDERGREATP) $L1 $L2)
			((MLIST) $L2 $L1)
			T
			((MLIST) $L1 $L2)))
		      ((MSETQ) $KINSARG (($DELETE) $DELIS $KINSARG))
		      ((MSETQ)
		       $KINS
		       (($APPLY)
			$DELETE
			((MLIST)
			 ((MEQUAL)
			  $LI
			  (($DHASH ARRAY)
			   (($DELIS ARRAY) 1)
			   (($DELIS ARRAY) 2)))
			 $KINS)))))
		    ((MRETURN) $KINS)))
		  MEXPR)
 	(ARGS '$UNKINDEF '(NIL . 1))
 	(ADD2LNC '(($UNKINDEF) ((MLIST) $LIST)) $FUNCTIONS)
 	(MDEFPROP
	 $UNKINDEF
	 (LAMBDA N 
	   (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
			     ($UNKINDEF (CONS '(MLIST) (LISTIFY N))))
		 (T (SETQ NOEVALARGS NIL)
		    ($UNKINDEF (CONS '(MLIST)
				     (MEVALARGS (LISTIFY N)))))))
	 T-MFEXPR)
 	(DEFPROP $ZNK
		 (LAMBDA ($X) 
		   (COND ((NOT (= ($LENGTH $X) 2))
			  (SETQ $C 1)
			  (SETQ $MASP 0)
			  ($FIRST $X))
			 (T (SETQ $C 1) (SETQ $MASP ($LAST $X)) ($FIRST $X))))
		 EXPR)
 	(ARGS '$ZNK '(NIL . 1))
 	(DEFPROP $ZNK T TRANSLATED)
 	(ADD2LNC '$ZNK $PROPS)
 	(MDEFPROP $ZNK T MLEXPRP)
 	(MDEFPROP $ZNK
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $X))
		   ((MCOND)
		    ((MNOTEQUAL) (($LENGTH) $X) 2)
		    ((DOLIST) ((MSETQ) $C 1) ((MSETQ) $MASP 0) (($FIRST) $X))
		    T
		    ((DOLIST)
		     ((MSETQ) $C 1)
		     ((MSETQ) $MASP (($LAST) $X))
		     (($FIRST) $X))))
		  MEXPR)
 	(ARGS '$ZNK '(NIL . 1))
 	(ADD2LNC '(($ZNK) ((MLIST) $X)) $FUNCTIONS)
 	(MDEFPROP $ZNK
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($ZNK (CONS '(MLIST)
						  (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($ZNK (CONS '(MLIST)
					 (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)
 	(DEFPROP
	 $ZMAK
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($C $MASP $PI $I) 
	      (PROG NIL 
		    (SETQ $C 1)
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (SETQ $PI ($PART $LIST $I))
		    (COND ((NOT ($MEMBER ($PART $PI 0)
					 (LIST '(MLIST)
					       (MEVAL1 '$ZN)
					       (MEVAL1 '$ZD))))
			   (GO $LOOP)))
		    (SETQ $LIST
			  (SIMPLIFY ($SUBSTPART (($EV)
						 (($LIST ARRAY) $I)
						 ((MEQUAL) $ZN $ZNK)
						 ((MEQUAL) $ZD $ZDK))
						$LIST
						$I)))
		    (RETURN
		     (COND
		      ((NOT (LIKE $MASP 0))
		       (RETURN
			(MUL*
			 $C
			 (ADD*
			  (MUL*
			   (SIMPLIFY
			    ($TRM
			     (SIMPLIFY (MAPPLY (MEVAL1 '$APPEND)
					       (CDR (SIMPLIFY ($LISBRE $LIST
								       $I)))
					       '$APPEND))))
			   $MASP)
			  (SIMPLIFY ($TRM $LIST))))))
		      (T (RETURN (MUL* $C (SIMPLIFY ($TRM $LIST)))))))))
	    '$C
	    '$MASP
	    '$PI
	    '$I))
	 EXPR)
 	(ARGS '$ZMAK '(NIL . 1))
 	(DEFPROP $ZMAK T TRANSLATED)
 	(ADD2LNC '$ZMAK $PROPS)
 	(MDEFPROP $ZMAK
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $C $MASP $PI $I)
		    ((MSETQ) $C 1)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MSETQ) $PI (($PART) $LIST $I))
		    ((MCOND)
		     ((MNOT) (($MEMBER) (($PART) $PI 0) ((MLIST) $ZN $ZD)))
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MSETQ)
		     $LIST
		     (($SUBSTPART)
		      (($EV)
		       (($LIST ARRAY) $I)
		       ((MEQUAL) $ZN $ZNK)
		       ((MEQUAL) $ZD $ZDK))
		      $LIST
		      $I))
		    ((MCOND)
		     ((MNOTEQUAL) $MASP 0)
		     ((MRETURN)
		      ((MTIMES)
		       $C
		       ((MPLUS)
			((MTIMES)
			 (($TRM) (($APPLY) $APPEND (($LISBRE) $LIST $I)))
			 $MASP)
			(($TRM) $LIST))))
		     T
		     ((MRETURN) ((MTIMES) $C (($TRM) $LIST))))))
		  MEXPR)
 	(ARGS '$ZMAK '(NIL . 1))
 	(ADD2LNC '(($ZMAK) $LIST) $FUNCTIONS)
 	(MDEFPROP $TR
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   (($RATSIMP) (($TRM1) (($GETRED) $LIST))))
		  MEXPR)
 	(ADD2LNC '(($TR) ((MLIST) $LIST)) $FUNCTIONS)
 	(MDEFPROP $TR T MLEXPRP)
 	(DEFPROP $MAPR
		 (LAMBDA ($AR $BR) 
		   (SIMPLIFY ($NEWMAP (MEVAL '((LAMBDA)
					       ((MLIST) $XR)
					       ((MCOND)
						(($CONSTANTP) $XR)
						$XR
						T
						(($DP) $AR $XR))))
				      $BR)))
		 EXPR)
 	(ARGS '$MAPR '(NIL . 2))
 	(DEFPROP $MAPR T TRANSLATED)
 	(ADD2LNC '$MAPR $PROPS)
 	(MDEFPROP $MAPR
		  ((LAMBDA)
		   ((MLIST) $AR $BR)
		   (($NEWMAP)
		    ((LAMBDA)
		     ((MLIST) $XR)
		     ((MCOND) (($CONSTANTP) $XR) $XR T (($DP) $AR $XR)))
		    $BR))
		  MEXPR)
 	(ARGS '$MAPR '(NIL . 2))
 	(ADD2LNC '(($MAPR) $AR $BR) $FUNCTIONS)
 	(DEFPROP $D0
		 (LAMBDA ($AYW $BYW) 
		   (SIMPLIFY ($MAPL (MEVAL '((LAMBDA)
					     ((MLIST) $XAW $YAW)
					     (($MAPR) $XAW $YAW)))
				    $AYW
				    $BYW)))
		 EXPR)
 	(ARGS '$D0 '(NIL . 2))
 	(DEFPROP $D0 T TRANSLATED)
 	(ADD2LNC '$D0 $PROPS)
 	(MDEFPROP $D0
		  ((LAMBDA)
		   ((MLIST) $AYW $BYW)
		   (($MAPL)
		    ((LAMBDA) ((MLIST) $XAW $YAW) (($MAPR) $XAW $YAW))
		    $AYW
		    $BYW))
		  MEXPR)
 	(ARGS '$D0 '(NIL . 2))
 	(ADD2LNC '(($D0) $AYW $BYW) $FUNCTIONS)
 	(DEFPROP $MAPL
		 (LAMBDA ($FL $AL $BL) 
		   (SIMPLIFY ($NEWMAP (MEVAL '((LAMBDA)
					       ((MLIST) $XL)
					       ((MCOND)
						(($CONSTANTP) $XL)
						$XL
						T
						(($FL) $XL $BL))))
				      $AL)))
		 EXPR)
 	(ARGS '$MAPL '(NIL . 3))
 	(DEFPROP $MAPL T TRANSLATED)
	(ADD2LNC '$MAPL $PROPS)
 	(MDEFPROP $MAPL
		  ((LAMBDA)
		   ((MLIST) $FL $AL $BL)
		   (($NEWMAP)
		    ((LAMBDA)
		     ((MLIST) $XL)
		     ((MCOND) (($CONSTANTP) $XL) $XL T (($FL) $XL $BL)))
		    $AL))
		  MEXPR)
 	(ARGS '$MAPL '(NIL . 3))
 	(ADD2LNC '(($MAPL) $FL $AL $BL) $FUNCTIONS)
 	(DEFPROP $DS (LAMBDA ($PMOM) (MEVAL '(($D) $PMOM $PMOM))) EXPR)
 	(ARGS '$DS '(NIL . 1))
 	(DEFPROP $DS T TRANSLATED)
 	(ADD2LNC '$DS $PROPS)
 	(MDEFPROP $DS ((LAMBDA) ((MLIST) $PMOM) (($D) $PMOM $PMOM)) MEXPR)
 	(ARGS '$DS '(NIL . 1))
 	(ADD2LNC '(($DS) $PMOM) $FUNCTIONS)
 	(DEFPROP
	 $ZDK
	 (LAMBDA ($X) 
	   (COND ((NOT (= ($LENGTH $X) 2))
		  (SETQ $C (DIV 1
				(COND ((IS (MEVAL1 '$VIRED))
				       (SIMPLIFY ($DS ($FIRST $X))))
				      (T (SIMPLIFY ($SOR ($FIRST $X)
							 ($FIRST $X)))))))
		  (SETQ $MASP 0)
		  ($FIRST $X))
		 (T
		  (SETQ $C
			(DIV 1
			     (COND ((IS (MEVAL1 '$VIRED))
				    (MEVAL '(($ZDEN)
					     (($FIRST) $X)
					     (($LAST) $X))))
				   (T (ADD* (SIMPLIFY ($SOR ($FIRST $X)
							    ($FIRST $X)))
					    (SIMPLIFY (LIST '(MMINUS)
							    (POWER ($LAST $X)
								   2))))))))
		  (SETQ $MASP ($LAST $X))
		  ($FIRST $X))))
	 EXPR)
 	(ARGS '$ZDK '(NIL . 1))
 	(DEFPROP $ZDK T TRANSLATED)
 	(ADD2LNC '$ZDK $PROPS)
 	(MDEFPROP $ZDK
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $X))
		   ((MCOND)
		    ((MNOTEQUAL) (($LENGTH) $X) 2)
		    ((DOLIST)
		     ((MSETQ)
		      $C
		      ((MQUOTIENT)
		       1
		       ((MCOND)
			$VIRED
			(($DS) (($FIRST) $X))
			T
			(($SOR) (($FIRST) $X) (($FIRST) $X)))))
		     ((MSETQ) $MASP 0)
		     (($FIRST) $X))
		    T
		    ((DOLIST)
		     ((MSETQ)
		      $C
		      ((MQUOTIENT)
		       1
		       ((MCOND)
			$VIRED
			(($ZDEN) (($FIRST) $X) (($LAST) $X))
			T
			((MPLUS)
			 (($SOR) (($FIRST) $X) (($FIRST) $X))
			 ((MMINUS) ((MEXPT) (($LAST) $X) 2))))))
		     ((MSETQ) $MASP (($LAST) $X))
		     (($FIRST) $X))))
		  MEXPR)
 	(ARGS '$ZDK '(NIL . 1))
 	(ADD2LNC '(($ZDK) ((MLIST) $X)) $FUNCTIONS)
 	(MDEFPROP $ZDK T MLEXPRP)
 	(MDEFPROP $ZDK
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($ZDK (CONS '(MLIST)
						  (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($ZDK (CONS '(MLIST)
					 (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)
 	(DEFPROP $ZD0
		 (LAMBDA ($EXP) 
		   (SETQ $CST (DIV (MEVAL1 '$CST)
				   (COND ((IS (MEVAL1 '$VIRED))
					  (SIMPLIFY ($DS $EXP)))
					 (T (SIMPLIFY ($SOR $EXP $EXP))))))
		   $EXP)
		 EXPR)
 	(ARGS '$ZD0 '(NIL . 1))
 	(DEFPROP $ZD0 T TRANSLATED)
 	(ADD2LNC '$ZD0 $PROPS)
 	(MDEFPROP $ZD0
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ)
		     $CST
		     ((MQUOTIENT)
		      $CST
		      ((MCOND) $VIRED (($DS) $EXP) T (($SOR) $EXP $EXP))))
		    ((MRETURN) $EXP)))
		  MEXPR)
 	(ARGS '$ZD0 '(NIL . 1))
 	(ADD2LNC '(($ZD0) $EXP) $FUNCTIONS)
 	(DEFPROP
	 $KINDEF
	 (LAMBDA ($LIST) 
	   (SETQ $LIST (SIMPLIFY ($GETRED $LIST)))
	   (DO (($I 1 (+ 1 $I))) 
	       ((> $I ($LENGTH $LIST)) '$DONE) 
	    ((LAMBDA ($LB $RB $L1 $L2 $LP1 $LP2) (PROG NIL (SETQ $LB ($LHS (SIMPLIFY (MARRAYREF $LIST $I)))) (SETQ $RB ($RHS (SIMPLIFY (MARRAYREF $LIST $I)))) (COND ((LIKE ($PART $LB 0) (MEVAL1 '
																								   $D)) (GO $ON))) (RETURN (MEVAL1 '
																												    $BOUND)) $ON (SETQ $L1 ($PART $LB 1)) (SETQ $L2 ($PART $LB 2)) (COND ((IS ($ORDERGREATP $L1 $L2)) (SETQ $LP1 $L2) (SETQ $LP2 $L1)) (T (SETQ $LP2 $L2) (SETQ $LP1 $L1))) (COND ((NOT (IS ($FREEOF $LB (MEVAL1 '
																																																						  $KINS)))) (SIMPLIFY (MAPPLY (MEVAL1 '
																																																										       $UNKINDEF) (CDR (LIST '
																																																													      (
																																																													       MLIST) $LB)) '
																																																															     $UNKINDEF)))) (SETQ $KINSARG ($APPEND (MEVAL1 '
																																																																					    $KINSARG) (LIST '
																																																																							     (
																																																																							      MLIST) (LIST '
																																																																									    (
																																																																									     MLIST) $LP1 $LP2)))) (SETQ $KINS ($APPEND (MEVAL1 '
																																																																																$KINS) (LIST '
																																																																																	      (
																																																																																	       MLIST) (MEVAL '
																																																																																			      (
																																																																																			       (
																																																																																				MEQUAL)
																																																																																			       $LB
																																																																																			       $RB))))) (RETURN (MARRAYSET $RB '
																																																																																								$DHASH $LP1 $LP2))))
	     '$LB '$RB '$L1 '$L2 '$LP1 '$LP2))
	   (MEVAL1 '$KINS))
	 EXPR)
 	(ARGS '$KINDEF '(NIL . 1))
 	(DEFPROP $KINDEF T TRANSLATED)
 	(ADD2LNC '$KINDEF $PROPS)
 	(MDEFPROP $KINDEF
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST))
		    ((MSETQ) $LIST (($GETRED) $LIST))
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH) $LIST)
		     NIL
		     ((MPROG)
		      ((MLIST) $LB $RB $L1 $L2 $LP1 $LP2)
		      ((MSETQ) $LB (($LHS) (($LIST ARRAY) $I)))
		      ((MSETQ) $RB (($RHS) (($LIST ARRAY) $I)))
		      ((MCOND)
		       ((MEQUAL) (($PART) $LB 0) $D)
		       ((MGO) $ON)
		       T
		       $FALSE)
		      ((MRETURN) $BOUND)
		      $ON
		      ((MSETQ) $L1 (($PART) $LB 1))
		      ((MSETQ) $L2 (($PART) $LB 2))
		      ((MCOND)
		       (($ORDERGREATP) $L1 $L2)
		       ((DOLIST) ((MSETQ) $LP1 $L2) ((MSETQ) $LP2 $L1))
		       T
		       ((DOLIST) ((MSETQ) $LP2 $L2) ((MSETQ) $LP1 $L1)))
		      ((MCOND)
		       ((MNOT) (($FREEOF) $LB $KINS))
		       (($APPLY) $UNKINDEF ((MLIST) $LB))
		       T
		       $FALSE)
		      ((MSETQ)
		       $KINSARG
		       (($APPEND) $KINSARG ((MLIST) ((MLIST) $LP1 $LP2))))
		      ((MSETQ)
		       $KINS
		       (($APPEND) $KINS ((MLIST) ((MEQUAL) $LB $RB))))
		      ((MSETQ) (($DHASH ARRAY) $LP1 $LP2) $RB)))
		    ((MRETURN) $KINS)))
		  MEXPR)
 	(ARGS '$KINDEF '(NIL . 1))
 	(ADD2LNC '(($KINDEF) ((MLIST) $LIST)) $FUNCTIONS)
 	(MDEFPROP $KINDEF T MLEXPRP)
 	(MDEFPROP $KINDEF
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($KINDEF (CONS '(MLIST)
						     (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($KINDEF (CONS '(MLIST)
					    (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)
 	(DEFPROP $DCH
		 (LAMBDA ($LIST) 
		   (COND ((AND (LIKE ($FIRST $LIST) ($LAST $LIST))
			       ($MEMBER ($FIRST $LIST) (MEVAL1 '$IND)))
			  (MEVAL1 '$N))
			 (T (SIMPLIFY (MAPPLY (MEVAL1 '$SOR)
					      (CDR $LIST)
					      '$SOR)))))
		 EXPR)
 	(ARGS '$DCH '(NIL . 1))
 	(DEFPROP $DCH T TRANSLATED)
 	(ADD2LNC '$DCH $PROPS)
 	(MDEFPROP $DCH
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MCOND)
		    ((MAND)
		     ((MEQUAL) (($FIRST) $LIST) (($LAST) $LIST))
		     (($MEMBER) (($FIRST) $LIST) $IND))
		    $N
		    T
		    (($APPLY) $SOR $LIST)))
		  MEXPR)
 	(ARGS '$DCH '(NIL . 1))
 	(ADD2LNC '(($DCH) $LIST) $FUNCTIONS)
 	(DEFPROP $SOR
		 (LAMBDA ($AYQ $BYQ) 
		   (COND ((AND (IS (MEVAL1 '$DEF))
			       (NOT (IS ($FREEOF (MEVAL1 '&+)
						 (MEVAL1 '&*)
						 (MEVAL1 '&//)
						 (MEVAL1 '&-)
						 (LIST '(MLIST)
						       $AYQ
						       $BYQ)))))
			  (SIMPLIFY ($D0 $AYQ $BYQ)))
			 ((IS (MEVAL1 '$DOF))
			  (SIMPLIFY ($DP $AYQ $BYQ)))
			 (T (MEVAL '(($D) $A $B)))))
		 EXPR)
 	(ARGS '$SOR '(NIL . 2))
 	(DEFPROP $SOR T TRANSLATED)
 	(ADD2LNC '$SOR $PROPS)
 	(MDEFPROP $SOR
		  ((LAMBDA)
		   ((MLIST) $AYQ $BYQ)
		   ((MCOND)
		    ((MAND)
		     $DEF
		     ((MNOT) (($FREEOF) &+ &* &// &- ((MLIST) $AYQ $BYQ))))
		    (($D0) $AYQ $BYQ)
		    T
		    ((MCOND) $DOF (($DP) $AYQ $BYQ) T (($D) $A $B))))
		  MEXPR)
 	(ARGS '$SOR '(NIL . 2))
 	(ADD2LNC '(($SOR) $AYQ $BYQ) $FUNCTIONS)
 	(DEFPROP
	 $CONT
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($NUM $I $LENCON $BITS $IN $OUT $CONBIT) 
	      (PROG NIL 
		    (COND ((LIKE (MEVAL1 '$N) 4)
			   (COND ((IS (MEVAL1 '$KAHAF))
				  (RETURN (SIMPLIFY ($KAHANE $LIST))))
				 (T (RETURN (SIMPLIFY ($CONT4 $LIST)))))))
		    (SETQ $BITS (SIMPLIFY ($LISCUT $LIST)))
		    (SETQ $IN ($FIRST $BITS))
		    (SETQ $OUT ($LAST $BITS))
		    (SETQ $NUM (+ ($LENGTH $IN) 1))
		    (COND
		     ((IS (MGRP $NUM (MEVAL1 '$CONTLIM)))
		      ((LAMBDA ($OUTB) 
			 (SETQ $OUTB $OUT)
			 (SETQ $OUT $IN)
			 (SETQ $IN $OUTB)
			 (SETQ $NUM (+ ($LENGTH $IN) 1))
			 (COND ((IS (MGRP $NUM (MEVAL1 '$CONTLIM)))
				((LAMBDA ($RE) 
				   (SETQ $RE (SIMPLIFY ($REAR $LIST)))
				   (SETQ $IN ($PART $RE 1))
				   (SETQ $OUT ($PART $RE 2))
				   (SETQ $NUM (+ ($LENGTH $IN) 1)))
				 '$RE))))
		       '$OUTB)))
		    (SETQ $CONBIT ($PART (MEVAL1 '$CONTAB) $NUM))
		    (SETQ $LENCON ($LENGTH $CONBIT))
		    (RETURN (SIMPLIFY ($SUM ((MTIMES)
					     (($FIRST)
					      (($CONBIT ARRAY)
					       ((MPLUS)
						((MTIMES) 2 $I)
						((MMINUS) 1))))
					     ((MPROG)
					      ((MLIST) $NB)
					      ((MSETQ)
					       $NB
					       (($NEWL)
						$IN
						(($CONBIT ARRAY)
						 ((MTIMES) 2 $I))
						$OUT))
					      ((MCOND)
					       ((MEQUAL) (($PART) $NB 1) 0)
					       ((MRETURN) 0)
					       T
					       ((MRETURN)
						((MTIMES)
						 (($PART) $NB 1)
						 (($TR0) (($PART) $NB 2)))))))
					    $I
					    1
					    ((MQUOTIENT) $LENCON 2))))))
	    '$NUM
	    '$I
	    '$LENCON
	    '$BITS
	    '$IN
	    '$OUT
	    '$CONBIT))
	 EXPR)
 	(ARGS '$CONT '(NIL . 1))
 	(DEFPROP $CONT T TRANSLATED)
 	(ADD2LNC '$CONT $PROPS)
 	(MDEFPROP $CONT
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $NUM $I $LENCON $BITS $IN $OUT $CONBIT)
		    ((MCOND)
		     ((MEQUAL) $N 4)
		     ((MCOND)
		      $KAHAF
		      ((MRETURN) (($KAHANE) $LIST))
		      T
		      ((MRETURN) (($CONT4) $LIST)))
		     T
		     $FALSE)
		    ((MSETQ) $BITS (($LISCUT) $LIST))
		    ((MSETQ) $IN (($FIRST) $BITS))
		    ((MSETQ) $OUT (($LAST) $BITS))
		    ((MSETQ) $NUM ((MPLUS) (($LENGTH) $IN) 1))
		    ((MCOND)
		     ((MGREATERP) $NUM $CONTLIM)
		     ((MPROG)
		      ((MLIST) $OUTB)
		      ((MSETQ) $OUTB $OUT)
		      ((MSETQ) $OUT $IN)
		      ((MSETQ) $IN $OUTB)
		      ((MSETQ) $NUM ((MPLUS) (($LENGTH) $IN) 1))
		      ((MCOND)
		       ((MGREATERP) $NUM $CONTLIM)
		       ((MPROG)
			((MLIST) $RE)
			((MSETQ) $RE (($REAR) $LIST))
			((MSETQ) $IN (($PART) $RE 1))
			((MSETQ) $OUT (($PART) $RE 2))
			((MSETQ) $NUM ((MPLUS) (($LENGTH) $IN) 1)))
		       T
		       $FALSE))
		     T
		     $FALSE)
		    ((MSETQ) $CONBIT (($PART) $CONTAB $NUM))
		    ((MSETQ) $LENCON (($LENGTH) $CONBIT))
		    (($SUM)
		     ((MTIMES)
		      (($FIRST)
		       (($CONBIT ARRAY)
			((MPLUS) ((MTIMES) 2 $I) ((MMINUS) 1))))
		      ((MPROG)
		       ((MLIST) $NB)
		       ((MSETQ)
			$NB
			(($NEWL) $IN (($CONBIT ARRAY) ((MTIMES) 2 $I)) $OUT))
		       ((MCOND)
			((MEQUAL) (($PART) $NB 1) 0)
			((MRETURN) 0)
			T
			((MRETURN)
			 ((MTIMES)
			  (($PART) $NB 1)
			  (($TR0) (($PART) $NB 2)))))))
		     $I
		     1
		     ((MQUOTIENT) $LENCON 2))))
		  MEXPR)
 	(ARGS '$CONT '(NIL . 1))
 	(ADD2LNC '(($CONT) $LIST) $FUNCTIONS)
 	(DEFPROP $CONUM
		 (LAMBDA ($LIST $DEX) 
		   ((LAMBDA ($LEN $I) 
		      (PROG NIL 
			    (SETQ $LEN ($LENGTH $LIST))
			    (SETQ $I 1)
		       $LOOP(SETQ $I (ADD* $I 1))
			    (RETURN (COND ((LIKE ($PART $LIST $I) $DEX)
					   (RETURN (ADD* $I -2)))
					  (T (GO $LOOP))))))
		    '$LEN
		    '$I))
		 EXPR)
 	(ARGS '$CONUM '(NIL . 2))
 	(DEFPROP $CONUM T TRANSLATED)
 	(ADD2LNC '$CONUM $PROPS)
 	(MDEFPROP $CONUM
		  ((LAMBDA)
		   ((MLIST) $LIST $DEX)
		   ((MPROG)
		    ((MLIST) $LEN $I)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MSETQ) $I 1)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND)
		     ((MEQUAL) (($PART) $LIST $I) $DEX)
		     ((MRETURN) ((MPLUS) $I ((MMINUS) 2)))
		     T
		     ((MGO) $LOOP))))
		  MEXPR)
 	(ARGS '$CONUM '(NIL . 2))
 	(ADD2LNC '(($CONUM) $LIST $DEX) $FUNCTIONS)
 	(DEFPROP $G0
		 (LAMBDA ($LIST) 
		   (COND ((LIKE $LIST '((MLIST))) 1)
			 (T (SIMPLIFY (MAPPLY (MEVAL1 '$G)
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
		    (($APPLY) $G $LIST)))
		  MEXPR)
 	(ARGS '$G0 '(NIL . 1))
 	(ADD2LNC '(($G0) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $TR0
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($JQ $I $LEN) 
	      (PROG NIL 
		    (COND ((NOT (IS ($FREEOF (MEVAL1 '$G5)
					     (MEVAL1 '$LHP)
					     (MEVAL1 '$RHP)
					     $LIST)))
			   (RETURN (MEVAL '(($TR5) $LIST)))))
		    (SETQ $LEN ($LENGTH $LIST))
		    (COND
		     ((AND (IS (MEVAL1 '$BORED))
			   (IS (MGRP $LEN (MEVAL1 '$BORELEN))))
		      (SIMPLIFY ($PRINT (MEVAL1 '|&ENTERING TR0 WITH |)
					$LIST))))
		    (COND ((IS ($ODDP $LEN)) (RETURN 0)))
		    (COND ((LIKE $LEN 2)
			   (RETURN (MUL* 4
					 (SIMPLIFY (MAPPLY (MEVAL1 '$DCH00)
							   (CDR $LIST)
							   '$DCH00))))))
		    (COND ((LIKE $LEN 0) (RETURN 4)))
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((IS (MGRP $I $LEN)) (GO $LO1)))
		    (SETQ $JQ ($PART $LIST $I))
		    (COND
		     ((AND
		       ($MEMBER $JQ (MEVAL1 '$IND))
		       (IS
			'((MPROG)
			  ((MLIST) $TRICH)
			  ((MSETQ) $TRICH (($LENGTH) (($DELETE) $JQ $LIST)))
			  ((MCOND)
			   ((MEQUAL) $TRICH ((MPLUS) $LEN ((MMINUS) 2)))
			   ((MRETURN) $TRUE)
			   T
			   $FALSE)
			  ((MCOND)
			   ((MLESSP) $TRICH ((MPLUS) $LEN ((MMINUS) 2)))
			   (($ERROR)
			    $JQ
			    |&APPEARS MORE THAN TWICE AS A CONTRACTED INDEX IN/
|			    $LIST)
			   T
			   ((MRETURN) $FALSE)))))
		      (RETURN (SIMPLIFY ($CATCH (($CONT)
						 (($CYC)
						  $LIST
						  ((MPLUS)
						   $I
						   ((MMINUS) 1)))))))))
		    (COND
		     ((AND (LIKE $JQ ($PART $LIST (ADD* $I 1)))
			   (IS ($FREEOF $JQ (MEVAL1 '$IND))))
		      (RETURN
		       (SIMPLIFY
			($TELL
			 ((LAMBDA ($SOD) 
			    (PROG NIL 
				  (SETQ $SOD (SIMPLIFY ($SOR $JQ $JQ)))
				  (RETURN
				   (COND
				    ((LIKE $SOD 0) (RETURN 0))
				    (T
				     (RETURN
				      (MUL*
				       $SOD
				       (SIMPLIFY
					($TR0
					 ($APPEND
					  ($REST
					   $LIST
					   (ADD* -1
						 $I
						 (SIMPLIFY (LIST '(MMINUS)
								 $LEN))))
					  ($REST $LIST (ADD* $I 1))))))))))))
			  '$SOD)
			 $LEN)))))
		    (GO $LOOP)
	       $LO1 (RETURN
		     (COND
		      ((IS (MLSP $LEN 5)) (RETURN (SIMPLIFY ($TRIV $LIST))))
		      (T
		       (RETURN
			(SIMPLIFY ($TELL (COND ((IS (MEVAL1 '$PLATU))
						(SIMPLIFY ($PLATES $LIST)))
					       (T (SIMPLIFY ($ANTIC $LIST))))
					 $LEN))))))))
	    '$JQ
	    '$I
	    '$LEN))
	 EXPR)
 	(ARGS '$TR0 '(NIL . 1))
 	(DEFPROP $TR0 T TRANSLATED)
 	(ADD2LNC '$TR0 $PROPS)
 	(MDEFPROP $TR0
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $JQ $I $LEN)
		    ((MCOND)
		     ((MNOT) (($FREEOF) $G5 $LHP $RHP $LIST))
		     ((MRETURN) (($TR5) $LIST))
		     T
		     $FALSE)
		    ((MSETQ) $LEN (($LENGTH) $LIST))
		    ((MCOND)
		     ((MAND) $BORED ((MGREATERP) $LEN $BORELEN))
		     (($PRINT) |&ENTERING TR0 WITH | $LIST)
		     T
		     $FALSE)
		    ((MCOND) (($ODDP) $LEN) ((MRETURN) 0) T $FALSE)
		    ((MCOND)
		     ((MEQUAL) $LEN 2)
		     ((MRETURN) ((MTIMES) 4 (($APPLY) $DCH00 $LIST)))
		     T
		     $FALSE)
		    ((MCOND) ((MEQUAL) $LEN 0) ((MRETURN) 4) T $FALSE)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MCOND) ((MGREATERP) $I $LEN) ((MGO) $LO1) T $FALSE)
		    ((MSETQ) $JQ (($PART) $LIST $I))
		    ((MCOND)
		     ((MAND)
		      (($MEMBER) $JQ $IND)
		      ((MPROG)
		       ((MLIST) $TRICH)
		       ((MSETQ) $TRICH (($LENGTH) (($DELETE) $JQ $LIST)))
		       ((MCOND)
			((MEQUAL) $TRICH ((MPLUS) $LEN ((MMINUS) 2)))
			((MRETURN) $TRUE)
			T
			$FALSE)
		       ((MCOND)
			((MLESSP) $TRICH ((MPLUS) $LEN ((MMINUS) 2)))
			(($ERROR)
			 $JQ
			 |&APPEARS MORE THAN TWICE AS A CONTRACTED INDEX IN/
|			 $LIST)
			T
			((MRETURN) $FALSE))))
		     ((MRETURN)
		      (($CATCH)
		       (($CONT) (($CYC) $LIST ((MPLUS) $I ((MMINUS) 1))))))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND)
		      ((MEQUAL) $JQ (($PART) $LIST ((MPLUS) $I 1)))
		      (($FREEOF) $JQ $IND))
		     ((MRETURN)
		      (($TELL)
		       ((MPROG)
			((MLIST) $SOD)
			((MSETQ) $SOD (($SOR) $JQ $JQ))
			((MCOND)
			 ((MEQUAL) $SOD 0)
			 ((MRETURN) 0)
			 T
			 ((MRETURN)
			  ((MTIMES)
			   $SOD
			   (($TR0)
			    (($APPEND)
			     (($REST)
			      $LIST
			      ((MPLUS) ((MMINUS) 1) $I ((MMINUS) $LEN)))
			     (($REST) $LIST ((MPLUS) $I 1))))))))
		       $LEN))
		     T
		     $FALSE)
		    ((MGO) $LOOP)
		    $LO1
		    ((MCOND)
		     ((MLESSP) $LEN 5)
		     ((MRETURN) (($TRIV) $LIST))
		     T
		     ((MRETURN)
		      (($TELL)
		       ((MCOND) $PLATU (($PLATES) $LIST) T (($ANTIC) $LIST))
		       $LEN)))))
		  MEXPR)
 	(ARGS '$TR0 '(NIL . 1))
 	(ADD2LNC '(($TR0) $LIST) $FUNCTIONS)
 	(DEFPROP $DCH00
		 (LAMBDA ($FI $LI) 
		   (COND ((AND (LIKE $FI $LI)
			       ($MEMBER $FI (MEVAL1 '$IND)))
			  (MEVAL1 '$N))
			 (T (SIMPLIFY ($SOR $FI $LI)))))
		 EXPR)
 	(ARGS '$DCH00 '(NIL . 2))
 	(DEFPROP $DCH00 T TRANSLATED)
 	(ADD2LNC '$DCH00 $PROPS)
 	(MDEFPROP $DCH00
		  ((LAMBDA)
		   ((MLIST) $FI $LI)
		   ((MCOND)
		    ((MAND) ((MEQUAL) $FI $LI) (($MEMBER) $FI $IND))
		    $N
		    T
		    (($SOR) $FI $LI)))
		  MEXPR)
 	(ARGS '$DCH00 '(NIL . 2))
 	(ADD2LNC '(($DCH00) $FI $LI) $FUNCTIONS)
 	(DEFPROP
	 $TRIV
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($OA $OB $OC $OD) 
	      (PROG NIL 
		    (COND ((NOT (= ($LENGTH $LIST) 4)) (GO $DIFI)))
		    (SETQ $OA ($PART $LIST 1))
		    (SETQ $OB ($PART $LIST 2))
		    (SETQ $OC ($PART $LIST 3))
		    (SETQ $OD ($PART $LIST 4))
		    (RETURN
		     (MUL*
		      4
		      (ADD* (MUL* (SIMPLIFY ($SOR $OA $OB))
				  (SIMPLIFY ($SOR $OC $OD)))
			    (MUL* (SIMPLIFY ($SOR $OA $OD))
				  (SIMPLIFY ($SOR $OB $OC)))
			    (SIMPLIFY (LIST '(MMINUS)
					    (MUL* (SIMPLIFY ($SOR $OA $OC))
						  (SIMPLIFY ($SOR $OB
								  $OD))))))))
	       $DIFI(RETURN
		     (COND
		      ((> ($LENGTH $LIST) 2)
		       (SIMPLIFY ($ERROR (MEVAL1 '|&TRIV ERROR|)
					 $LIST)))
		      ((= ($LENGTH $LIST) 2)
		       (RETURN (MUL* 4
				     (SIMPLIFY (MAPPLY (MEVAL1 '$DCH00)
						       (CDR $LIST)
						       '$DCH00)))))
		      (T (RETURN 4))))))
	    '$OA
	    '$OB
	    '$OC
	    '$OD))
	 EXPR)
 	(ARGS '$TRIV '(NIL . 1))
 	(DEFPROP $TRIV T TRANSLATED)
 	(ADD2LNC '$TRIV $PROPS)
 	(MDEFPROP $TRIV
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $OA $OB $OC $OD)
		    ((MCOND)
		     ((MNOTEQUAL) (($LENGTH) $LIST) 4)
		     ((MGO) $DIFI)
		     T
		     $FALSE)
		    ((MSETQ) $OA (($PART) $LIST 1))
		    ((MSETQ) $OB (($PART) $LIST 2))
		    ((MSETQ) $OC (($PART) $LIST 3))
		    ((MSETQ) $OD (($PART) $LIST 4))
		    ((MRETURN)
		     ((MTIMES)
		      4
		      ((MPLUS)
		       ((MTIMES) (($SOR) $OA $OB) (($SOR) $OC $OD))
		       ((MTIMES) (($SOR) $OA $OD) (($SOR) $OB $OC))
		       ((MMINUS)
			((MTIMES) (($SOR) $OA $OC) (($SOR) $OB $OD))))))
		    $DIFI
		    ((MCOND)
		     ((MGREATERP) (($LENGTH) $LIST) 2)
		     (($ERROR) |&TRIV ERROR| $LIST)
		     T
		     ((MCOND)
		      ((MEQUAL) (($LENGTH) $LIST) 2)
		      ((MRETURN) ((MTIMES) 4 (($APPLY) $DCH00 $LIST)))
		      T
		      ((MRETURN) 4)))))
		  MEXPR)
 	(ARGS '$TRIV '(NIL . 1))
 	(ADD2LNC '(($TRIV) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $CONT4
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($INH $H1 $H2 $LELM $BITS $IN $OUT $LENI $LENO) 
	      (PROG NIL 
		    (SETQ $BITS (SIMPLIFY ($LISCUT $LIST)))
		    (SETQ $IN ($FIRST $BITS))
		    (COND ((LIKE $IN '((MLIST)))
			   (RETURN (MUL* 4 (SIMPLIFY ($TR0 ($LAST $BITS)))))))
		    (SETQ $OUT ($LAST $BITS))
		    (SETQ $LENI ($LENGTH $IN))
		    (SETQ $LENO ($LENGTH $OUT))
		    (COND ((LIKE $LENI 2) (GO $TWOBIT)))
		    (COND ((NOT (LIKE $LENO 2)) (GO $ON)))
		    (SETQ $INH $OUT)
		    (SETQ $OUT $IN)
		    (SETQ $IN $INH)
	       $TWOBIT
		    (SETQ $H1 (SIMPLIFY ($FREEOF (SIMPLIFY (MARRAYREF $IN 1))
						 (MEVAL1 '$IND))))
		    (SETQ $H2 (SIMPLIFY ($FREEOF (SIMPLIFY (MARRAYREF $IN 2))
						 (MEVAL1 '$IND))))
		    (COND ((AND (IS $H1) (IS $H2))
			   (RETURN (MUL* 4
					 (SIMPLIFY (MAPPLY (MEVAL1 '$SOR)
							   (CDR $IN)
							   '$SOR))
					 (SIMPLIFY ($TR0 $OUT)))))
			  ((AND (NOT (IS $H1))
				(LIKE (SIMPLIFY (MARRAYREF $IN 2))
				      (SIMPLIFY (MARRAYREF $IN 1))))
			   (RETURN (MUL* 20 (SIMPLIFY ($TR0 $OUT))))))
		    (COND
		     ((AND (NOT (IS $H1))
			   (NOT (IS ($FREEOF (SIMPLIFY (MARRAYREF $IN 1))
					     $OUT))))
		      (RETURN
		       (MUL*
			4
			(SIMPLIFY
			 ($TR0
			  (SIMPLIFY
			   (MAPPLY (MEVAL1 '$EV)
				   (CDR (LIST '(MLIST)
					      $OUT
					      (MEVAL '((MEQUAL)
						       (($IN ARRAY) 1)
						       (($IN ARRAY) 2)))))
				   '$EV)))))))
		     ((AND (NOT (IS $H2))
			   (NOT (IS ($FREEOF (SIMPLIFY (MARRAYREF $IN 2))
					     $OUT))))
		      (RETURN
		       (MUL*
			4
			(SIMPLIFY
			 ($TR0
			  (SIMPLIFY
			   (MAPPLY (MEVAL1 '$EV)
				   (CDR (LIST '(MLIST)
					      $OUT
					      (MEVAL '((MEQUAL)
						       (($IN ARRAY) 2)
						       (($IN ARRAY) 1)))))
				   '$EV)))))))
		     (T (RETURN (MUL* 4
				      (SIMPLIFY (MAPPLY (MEVAL1 '$SOR)
							(CDR $IN)
							'$SOR))
				      (SIMPLIFY ($TR0 $OUT))))))
	       $ON  (RETURN
		     (COND
		      ((IS ($ODDP $LENI))
		       (RETURN
			(SIMPLIFY
			 (LIST '(MMINUS)
			       (MUL* 2
				     (SIMPLIFY ($TR0 ($APPEND ($REVERSE $IN)
							      $OUT))))))))
		      (T
		       (RETURN
			(PROGN
			 (SETQ $LELM ($LAST $IN))
			 (SETQ $IN ($REST $IN -1))
			 (MUL*
			  2
			  (ADD* (SIMPLIFY ($TR0 ($APPEND (LIST '(MLIST)
							       $LELM)
							 $IN
							 $OUT)))
				(SIMPLIFY ($TR0 ($APPEND ($REVERSE $IN)
							 (LIST '(MLIST)
							       $LELM)
							 $OUT))))))))))))
	    '$INH
	    '$H1
	    '$H2
	    '$LELM
	    '$BITS
	    '$IN
	    '$OUT
	    '$LENI
	    '$LENO))
	 EXPR)
 	(ARGS '$CONT4 '(NIL . 1))
 	(DEFPROP $CONT4 T TRANSLATED)
 	(ADD2LNC '$CONT4 $PROPS)
 	(MDEFPROP $CONT4
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MPROG)
		    ((MLIST) $INH $H1 $H2 $LELM $BITS $IN $OUT $LENI $LENO)
		    ((MSETQ) $BITS (($LISCUT) $LIST))
		    ((MSETQ) $IN (($FIRST) $BITS))
		    ((MCOND)
		     ((MEQUAL) $IN ((MLIST)))
		     ((MRETURN) ((MTIMES) 4 (($TR0) (($LAST) $BITS))))
		     T
		     $FALSE)
		    ((MSETQ) $OUT (($LAST) $BITS))
		    ((MSETQ) $LENI (($LENGTH) $IN))
		    ((MSETQ) $LENO (($LENGTH) $OUT))
		    ((MCOND) ((MEQUAL) $LENI 2) ((MGO) $TWOBIT) T $FALSE)
		    ((MCOND) ((MNOTEQUAL) $LENO 2) ((MGO) $ON) T $FALSE)
		    ((MSETQ) $INH $OUT)
		    ((MSETQ) $OUT $IN)
		    ((MSETQ) $IN $INH)
		    $TWOBIT
		    ((MSETQ) $H1 (($FREEOF) (($IN ARRAY) 1) $IND))
		    ((MSETQ) $H2 (($FREEOF) (($IN ARRAY) 2) $IND))
		    ((MCOND)
		     ((MAND) $H1 $H2)
		     ((MRETURN) ((MTIMES) 4 (($APPLY) $SOR $IN) (($TR0) $OUT)))
		     T
		     ((MCOND)
		      ((MAND)
		       ((MNOT) $H1)
		       ((MEQUAL) (($IN ARRAY) 2) (($IN ARRAY) 1)))
		      ((MRETURN) ((MTIMES) 20 (($TR0) $OUT)))
		      T
		      $FALSE))
		    ((MCOND)
		     ((MAND)
		      ((MNOT) $H1)
		      ((MNOT) (($FREEOF) (($IN ARRAY) 1) $OUT)))
		     ((MRETURN)
		      ((MTIMES)
		       4
		       (($TR0)
			(($APPLY)
			 $EV
			 ((MLIST)
			  $OUT
			  ((MEQUAL) (($IN ARRAY) 1) (($IN ARRAY) 2)))))))
		     T
		     ((MCOND)
		      ((MAND)
		       ((MNOT) $H2)
		       ((MNOT) (($FREEOF) (($IN ARRAY) 2) $OUT)))
		      ((MRETURN)
		       ((MTIMES)
			4
			(($TR0)
			 (($APPLY)
			  $EV
			  ((MLIST)
			   $OUT
			   ((MEQUAL) (($IN ARRAY) 2) (($IN ARRAY) 1)))))))
		      T
		      ((MRETURN)
		       ((MTIMES) 4 (($APPLY) $SOR $IN) (($TR0) $OUT)))))
		    $ON
		    ((MCOND)
		     (($ODDP) $LENI)
		     ((MRETURN)
		      ((MMINUS)
		       ((MTIMES)
			2
			(($TR0) (($APPEND) (($REVERSE) $IN) $OUT)))))
		     T
		     ((MRETURN)
		      ((DOLIST)
		       ((MSETQ) $LELM (($LAST) $IN))
		       ((MSETQ) $IN (($REST) $IN ((MMINUS) 1)))
		       ((MTIMES)
			2
			((MPLUS)
			 (($TR0) (($APPEND) ((MLIST) $LELM) $IN $OUT))
			 (($TR0)
			  (($APPEND)
			   (($REVERSE) $IN)
			   ((MLIST) $LELM)
			   $OUT)))))))))
		  MEXPR)
 	(ARGS '$CONT4 '(NIL . 1))
 	(ADD2LNC '(($CONT4) $LIST) $FUNCTIONS)
 	(DEFPROP $GCH
		 (LAMBDA ($LIST) 
		   (COND ((NOT (IS ($FREEOF (MEVAL1 '$ZN)
					    (MEVAL1 '$ZD)
					    $LIST)))
			  (SIMPLIFY (MAPPLY (MEVAL1 '$ZFIX1)
					    (CDR $LIST)
					    '$ZFIX1)))
			 ((AND (IS ($FREEOF (MEVAL1 '$G5) $LIST))
			       (OR (NOT (LIKE (MEVAL1 '$NAMETAG)
					      (MEVAL1 '$G)))
				   (IS (MEVAL1 '$ODDKILL)))
			       (IS ($ODDP ($LENGTH $LIST))))
			  0)
			 (T (SIMPLIFY (MAPPLY (MEVAL1 '$NAMETAG)
					      (CDR $LIST)
					      '$NAMETAG)))))
		 EXPR)
 	(ARGS '$GCH '(NIL . 1))
 	(DEFPROP $GCH T TRANSLATED)
 	(ADD2LNC '$GCH $PROPS)
 	(MDEFPROP $GCH
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MCOND)
		    ((MNOT) (($FREEOF) $ZN $ZD $LIST))
		    (($APPLY) $ZFIX1 $LIST)
		    T
		    ((MCOND)
		     ((MAND)
		      (($FREEOF) $G5 $LIST)
		      ((MOR) ((MNOTEQUAL) $NAMETAG $G) $ODDKILL)
		      (($ODDP) (($LENGTH) $LIST)))
		     0
		     T
		     (($APPLY) $NAMETAG $LIST))))
		  MEXPR)
 	(ARGS '$GCH '(NIL . 1))
 	(ADD2LNC '(($GCH) $LIST) $FUNCTIONS)
 	(DEFPROP
	 $ZFIX1
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($C $MASP $PI $I) 
	      (PROG NIL 
		    (SETQ $LIST (SIMPLIFY ($GETRED $LIST)))
		    (SETQ $C 1)
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (SETQ $PI ($PART $LIST $I))
		    (COND ((NOT ($MEMBER ($PART $PI 0)
					 (LIST '(MLIST)
					       (MEVAL1 '$ZN)
					       (MEVAL1 '$ZD))))
			   (GO $LOOP)))
		    (SETQ $LIST
			  (SIMPLIFY ($SUBSTPART (($EV)
						 (($LIST ARRAY) $I)
						 ((MEQUAL) $ZN $ZNK)
						 ((MEQUAL) $ZD $ZDK))
						$LIST
						$I)))
		    (RETURN
		     (COND
		      ((NOT (LIKE $MASP 0))
		       (RETURN
			(MUL*
			 $C
			 (ADD*
			  (MUL*
			   (SIMPLIFY
			    ($GCH
			     (SIMPLIFY (MAPPLY (MEVAL1 '$APPEND)
					       (CDR (SIMPLIFY ($LISBRE $LIST
								       $I)))
					       '$APPEND))))
			   $MASP)
			  (SIMPLIFY ($GCH $LIST))))))
		      (T (RETURN (MUL* $C (SIMPLIFY ($GCH $LIST)))))))))
	    '$C
	    '$MASP
	    '$PI
	    '$I))
	 EXPR)
 	(ARGS '$ZFIX1 '(NIL . 1))
 	(DEFPROP $ZFIX1 T TRANSLATED)
 	(ADD2LNC '$ZFIX1 $PROPS)
 	(MDEFPROP $ZFIX1 T MLEXPRP)
 	(MDEFPROP $ZFIX1
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST) $C $MASP $PI $I)
		    ((MSETQ) $LIST (($GETRED) $LIST))
		    ((MSETQ) $C 1)
		    ((MSETQ) $I 0)
		    $LOOP
		    ((MSETQ) $I ((MPLUS) $I 1))
		    ((MSETQ) $PI (($PART) $LIST $I))
		    ((MCOND)
		     ((MNOT) (($MEMBER) (($PART) $PI 0) ((MLIST) $ZN $ZD)))
		     ((MGO) $LOOP)
		     T
		     $FALSE)
		    ((MSETQ)
		     $LIST
		     (($SUBSTPART)
		      (($EV)
		       (($LIST ARRAY) $I)
		       ((MEQUAL) $ZN $ZNK)
		       ((MEQUAL) $ZD $ZDK))
		      $LIST
		      $I))
		    ((MCOND)
		     ((MNOTEQUAL) $MASP 0)
		     ((MRETURN)
		      ((MTIMES)
		       $C
		       ((MPLUS)
			((MTIMES)
			 (($GCH) (($APPLY) $APPEND (($LISBRE) $LIST $I)))
			 $MASP)
			(($GCH) $LIST))))
		     T
		     ((MRETURN) ((MTIMES) $C (($GCH) $LIST))))))
		  MEXPR)
 	(ARGS '$ZFIX1 '(NIL . 1))
 	(ADD2LNC '(($ZFIX1) ((MLIST) $LIST)) $FUNCTIONS)
 	(MDEFPROP $ZFIX1
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($ZFIX1 (CONS '(MLIST)
						    (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($ZFIX1 (CONS '(MLIST)
					   (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)
 	(DEFPROP
	 $ZFIXR
	 (LAMBDA ($EXP $NAMETAG) 
	   ((LAMBDA ($QW) 
	      (SETQ $QW
		    (SIMPLIFY ($SUBSTITUTE (LIST '(MLIST)
						 (MEVAL '((MEQUAL) $G $ZFIX1))
						 (MEVAL '((MEQUAL)
							  $GT
							  $ZFIX1)))
					   $EXP)))
	      (SIMPLIFY ($EV $QW)))
	    '$QW))
	 EXPR)
 	(ARGS '$ZFIXR '(NIL . 2))
 	(DEFPROP $ZFIXR T TRANSLATED)
 	(ADD2LNC '$ZFIXR $PROPS)
 	(MDEFPROP $ZFIXR
		  ((LAMBDA)
		   ((MLIST) $EXP $NAMETAG)
		   ((MPROG)
		    ((MLIST) $QW)
		    ((MSETQ)
		     $QW
		     (($SUBSTITUTE)
		      ((MLIST) ((MEQUAL) $G $ZFIX1) ((MEQUAL) $GT $ZFIX1))
		      $EXP))
		    ((MRETURN) (($EV) $QW))))
		  MEXPR)
 	(ARGS '$ZFIXR '(NIL . 2))
 	(ADD2LNC '(($ZFIXR) $EXP $NAMETAG) $FUNCTIONS)
 	(DEFPROP $ZFIX
		 (LAMBDA ($EXP) 
		   (COND ((IS ($FREEOF (MEVAL1 '$ZN)
				       (MEVAL1 '$ZD)
				       $EXP))
			  $EXP)
			 (($MEMBER ($PART $EXP 0)
				   (LIST '(MLIST)
					 (MEVAL1 '$G)
					 (MEVAL1 '$GT)))
			  (SIMPLIFY ($ZFIXR $EXP ($PART $EXP 0))))
			 ((OR ($CONSTANTP $EXP) (IS ($MAPATOM $EXP))) $EXP)
			 (T (MAP1 (GETOPR (MEVAL1 '$ZFIX)) $EXP))))
		 EXPR)
 	(ARGS '$ZFIX '(NIL . 1))
 	(DEFPROP $ZFIX T TRANSLATED)
 	(ADD2LNC '$ZFIX $PROPS)
 	(MDEFPROP $ZFIX
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MCOND)
		    (($FREEOF) $ZN $ZD $EXP)
		    $EXP
		    T
		    ((MCOND)
		     (($MEMBER) (($PART) $EXP 0) ((MLIST) $G $GT))
		     (($ZFIXR) $EXP (($PART) $EXP 0))
		     T
		     ((MCOND)
		      ((MOR) (($CONSTANTP) $EXP) (($MAPATOM) $EXP))
		      $EXP
		      T
		      (($MAP) $ZFIX $EXP)))))
		  MEXPR)
 	(ARGS '$ZFIX '(NIL . 1))
 	(ADD2LNC '(($ZFIX) $EXP) $FUNCTIONS)
 	(DEFPROP
	 $TCON
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($ANS $PO $I $CONS $INE $XP1 $XP2 $H1 $H2) 
	      (PROG NIL 
		    (COND ((IS ($FREEOF (MEVAL1 '$D) $EXP))
			   (RETURN (COND ((IS ($FREEOF (MEVAL1 '$EPS)
						       $EXP))
					  $EXP)
					 (T (MEVAL '(($EPSCON) $EXP)))))))
		    (SETQ $INE (LIST '(MLIST) '((MLIST))))
		    (SETQ $CONS 1)
		    (SETQ $I 0)
	       $LOOP(SETQ $I (ADD* $I 1))
		    (COND ((LIKE (SIMPLIFY ($INPART $EXP $I))
				 (MEVAL1 '$END))
			   (GO $ON)))
		    (COND ((NOT (LIKE (SIMPLIFY ($INPART $EXP $I 0))
				      (MEVAL1 '$D)))
			   (GO $NONE)))
		    (SETQ $PO (SIMPLIFY ($INPART $EXP $I)))
		    (SETQ $XP1 (SIMPLIFY ($INPART $PO 1)))
		    (SETQ $XP2 (SIMPLIFY ($INPART $PO 2)))
		    (SETQ $H1 ($MEMBER $XP1 (MEVAL1 '$IND)))
		    (SETQ $H2 ($MEMBER $XP2 (MEVAL1 '$IND)))
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
	       $IN0 (SETQ $CONS (MUL* $CONS (SIMPLIFY ($DP $XP1 $XP2))))
		    (GO $LOOP)
	       $NONE(SETQ $ANS (SIMPLIFY ($INPART $EXP $I)))
		    (COND ((LIKE ($PART $ANS 0) (MEVAL1 '&^))
			   (SETQ $ANS (MEVAL '(($PCON) $ANS))))
			  ((LIKE ($PART $ANS 0) (MEVAL1 '$D))
			   (SETQ $ANS (MEVAL '(($DCON) $ANS)))))
		    (SETQ $CONS (MUL* $CONS $ANS))
		    (GO $LOOP)
	       $EQIND
		    (SETQ $CONS (MUL* $CONS (MEVAL1 '$N)))
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
	 $AXPROP
	 (LAMBDA ($LIST) 
	   ((LAMBDA ($KMOM $IN1 $IN2 $NVEC) 
	      (COND ((< ($LENGTH $LIST) 3)
		     (SIMPLIFY ($ERROR (MEVAL1 '|&AXPROP ARGS WRONG|)
				       $LIST))))
	      (SETQ $KMOM ($FIRST $LIST))
	      (SETQ $IN1 (SIMPLIFY (MARRAYREF $LIST 2)))
	      (SETQ $IN2 (SIMPLIFY (MARRAYREF $LIST 3)))
	      (SETQ $NVEC (COND ((= ($LENGTH $LIST) 4) ($LAST $LIST))
				(T (MEVAL1 '$ETA))))
	      (ADD*
	       (SIMPLIFY (LIST '(MMINUS)
			       (MEVAL '(($D) $IN1 $IN2))))
	       (DIV (ADD* (MUL* (MEVAL '(($D) $NVEC $IN1))
				(MEVAL '(($D) $KMOM $IN2)))
			  (MUL* (MEVAL '(($D) $NVEC $IN2))
				(MEVAL '(($D) $KMOM $IN1))))
		    (MEVAL '(($D) $KMOM $NVEC)))
	       (COND ((= ($LENGTH $LIST) 3) 0)
		     (T (SIMPLIFY (LIST '(MMINUS)
					(DIV (MUL* (SIMPLIFY ($DSIM $NVEC
								    $NVEC))
						   (MEVAL '(($D) $KMOM $IN1))
						   (MEVAL '(($D) $KMOM $IN2)))
					     (POWER (MEVAL '(($D) $KMOM $NVEC))
						    2))))))))
	    '$KMOM
	    '$IN1
	    '$IN2
	    '$NVEC))
	 EXPR)
 	(ARGS '$AXPROP '(NIL . 1))
 	(DEFPROP $AXPROP T TRANSLATED)
 	(ADD2LNC '$AXPROP $PROPS)
 	(MDEFPROP $AXPROP
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST) $KMOM $IN1 $IN2 $NVEC)
		    ((MCOND)
		     ((MLESSP) (($LENGTH) $LIST) 3)
		     (($ERROR) |&AXPROP ARGS WRONG| $LIST)
		     T
		     $FALSE)
		    ((MSETQ) $KMOM (($FIRST) $LIST))
		    ((MSETQ) $IN1 (($LIST ARRAY) 2))
		    ((MSETQ) $IN2 (($LIST ARRAY) 3))
		    ((MSETQ)
		     $NVEC
		     ((MCOND)
		      ((MEQUAL) (($LENGTH) $LIST) 4)
		      (($LAST) $LIST)
		      T
		      $ETA))
		    ((MRETURN)
		     ((MPLUS)
		      ((MMINUS) (($D) $IN1 $IN2))
		      ((MQUOTIENT)
		       ((MPLUS)
			((MTIMES) (($D) $NVEC $IN1) (($D) $KMOM $IN2))
			((MTIMES) (($D) $NVEC $IN2) (($D) $KMOM $IN1)))
		       (($D) $KMOM $NVEC))
		      ((MCOND)
		       ((MEQUAL) (($LENGTH) $LIST) 3)
		       0
		       T
		       ((MMINUS)
			((MQUOTIENT)
			 ((MTIMES)
			  (($DSIM) $NVEC $NVEC)
			  (($D) $KMOM $IN1)
			  (($D) $KMOM $IN2))
			 ((MEXPT) (($D) $KMOM $NVEC) 2))))))))
		  MEXPR)
 	(ARGS '$AXPROP '(NIL . 1))
 	(ADD2LNC '(($AXPROP) ((MLIST) $LIST)) $FUNCTIONS)
 	(MDEFPROP $AXPROP T MLEXPRP)
 	(MDEFPROP $AXPROP
		  (LAMBDA N 
		    (COND (NOEVALARGS (SETQ NOEVALARGS NIL)
				      ($AXPROP (CONS '(MLIST)
						     (LISTIFY N))))
			  (T (SETQ NOEVALARGS NIL)
			     ($AXPROP (CONS '(MLIST)
					    (MEVALARGS (LISTIFY N)))))))
		  T-MFEXPR)
 	(DEFPROP
	 $TRM1
	 (LAMBDA ($LIST) 
	   (COND
	    ((IS ($FREEOF (TRD-MSYMEVAL $ID) $LIST)) (SIMPLIFY ($TRM $LIST)))
	    (T
	     (SIMPLIFY
	      ($TRM ((LAMBDA (MAPLP RES) 
		       (DECLARE (SPECIAL MAPLP))
		       (SETQ RES (MAP1 (GETOPR (TRD-MSYMEVAL $IDFIX)) $LIST))
		       (COND ((ATOM RES) (LIST '(MLIST) RES))
			     ((EQ (CAAR RES) 'MLIST) RES)
			     (T (CONS '(MLIST) (CDR RES)))))
		     T
		     NIL))))))
	 EXPR)
 	(ARGS '$TRM1 '(NIL . 1))
 	(DEFPROP $TRM1 T TRANSLATED)
 	(ADD2LNC '$TRM1 $PROPS)
 	(MDEFPROP $TRM1
		  ((LAMBDA)
		   ((MLIST) $LIST)
		   ((MCOND)
		    (($FREEOF) $ID $LIST)
		    (($TRM) $LIST)
		    T
		    (($TRM) (($MAPLIST) $IDFIX $LIST))))
		  MEXPR)
 	(ARGS '$TRM1 '(NIL . 1))
 	(ADD2LNC '(($TRM1) $LIST) $FUNCTIONS)
 	(DEFPROP $IDFIX
		 (LAMBDA ($ELM) 
		   (COND ((IS ($FREEOF (TRD-MSYMEVAL $ID) $ELM)) $ELM)
			 (T (MEVAL '(($ZN)
				     (($EV) $ELM ((MEQUAL) $ID 0))
				     (($COEFF) $ELM $ID))))))
		 EXPR)
 	(ARGS '$IDFIX '(NIL . 1))
 	(DEFPROP $IDFIX T TRANSLATED)
 	(ADD2LNC '$IDFIX $PROPS)
 	(MDEFPROP $IDFIX
		  ((LAMBDA)
		   ((MLIST) $ELM)
		   ((MCOND)
		    (($FREEOF) $ID $ELM)
		    $ELM
		    T
		    (($ZN) (($EV) $ELM ((MEQUAL) $ID 0)) (($COEFF) $ELM $ID))))
		  MEXPR)
 	(ARGS '$IDFIX '(NIL . 1))
 	(ADD2LNC '(($IDFIX) $ELM) $FUNCTIONS)
 	(MDEFPROP $BTR
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST) $ANS)
		    ((MSETQ) $DEF $FALSE)
		    ((MSETQ) $ANS (($DFIX) (($TR) $LIST)))
		    ((MSETQ) $DEF $TRUE)
		    ((MRETURN) $ANS)))
		  MEXPR)
 	(ADD2LNC '(($BTR) ((MLIST) $LIST)) $FUNCTIONS)
 	(MDEFPROP $BTR T MLEXPRP)
 	(ADD2LNC '$CONTAB $VALUES)
 	(ADD2LNC '$CONTAB4 $VALUES)
 	(ADD2LNC '$IND $VALUES)
 	(ADD2LNC '$COF $VALUES)
 	(ADD2LNC '$NOP $VALUES)
 	(ADD2LNC '$NPIND $VALUES)
 	(ADD2LNC '$NTR $VALUES)
 	(ADD2LNC '$KINS $VALUES)
 	(ADD2LNC '$DEF $VALUES)
 	(ADD2LNC '$BORED $VALUES)
 	(ADD2LNC '$BORELEN $VALUES)
 	(ADD2LNC '$ZERM $VALUES)
 	(ADD2LNC '$FLAGPARADE $VALUES)
 	(ADD2LNC '$MTRICK $VALUES)
 	(ADD2LNC '$KAHAF $VALUES)
 	(ADD2LNC '$CONTLIM $VALUES)
 	(ADD2LNC '$PLAT $VALUES)
 	(ADD2LNC '$DOF $VALUES)
 	(ADD2LNC '$PLATU $VALUES)
 	(ADD2LNC '$SCALARS $VALUES)
 	(ADD2LNC '$DSIMP $VALUES)
 	(ADD2LNC '$KINSARG $VALUES)
 	(ADD2LNC '$VIRED $VALUES)
 	(ADD2LNC '$DANGER $VALUES)
 	(ADD2LNC '$EPSEF $VALUES)
 	(ADD2LNC '$EPSOF $VALUES)
 	(ADD2LNC '$COMPS $VALUES)
 	(ADD2LNC '$ODDKILL $VALUES)
 	(DEFPROP $CON (GAMCON > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$CON $PROPS)
 	(DEFPROP $CRUNCH (GAMCON > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$CRUNCH $PROPS)
 	(DEFPROP $KAH0 (GAMKAH > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$KAH0 $PROPS)
 	(DEFPROP $CONJ (GAMSQ > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$CONJ $PROPS)
 	(DEFPROP $GFIX (GAMGFI > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$GFIX $PROPS)
 	(DEFPROP $ZMAK4 (GAMMTR > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$ZMAK4 $PROPS)
 	(DEFPROP $ZMAK5 (GAM5 > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$ZMAK5 $PROPS)
 	(MDEFPROP $UV T $CONSTANT)
 	(ADD2LNC '$UV $PROPS)
 	(DEFPROP $SQ (GAMSQ > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$SQ $PROPS)
 	(DEFPROP $SQAM (GAMSQ > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$SQAM $PROPS)
 	(DEFPROP $COTR (GAMCHI > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$COTR $PROPS)
 	(DEFPROP $TR5 (GAM5 > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$TR5 $PROPS)
 	(DEFPROP $EPSFIX (GAM5 > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$EPSFIX $PROPS)
 	(DEFPROP $COMPDEF (GAMNOC > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$COMPDEF $PROPS)
 	(DEFPROP $NONCOV (GAMNOC > DSK SHARE2) AUTOLOAD)
 	(ADD2LNC '$NONCOV $PROPS)
(SETQ IBASE 10.)
