
	(SETQ IBASE 8.)

 	(SETQ SAVENO 5372)

 	(DEFPROP $NEWMAP
		 (LAMBDA ($FN $AN) 
		   (COND ((IS ($ATOM $AN)) (MEVAL '(($FN) $AN)))
			 (T (SIMPLIFY ($FULLMAP $FN $AN)))))
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

 	(DEFPROP $MULTIMAP
		 (LAMBDA ($LIST $N) 
		   (SIMPLIFY ($MAPN (SIMPLIFY ($GEN $N)) $LIST $N)))
		 EXPR)

 	(ARGS '$MULTIMAP '(NIL . 2))

 	(DEFPROP $MULTIMAP T TRANSLATED)

 	(ADD2LNC '$MULTIMAP $PROPS)

 	(MDEFPROP $MULTIMAP
		  ((LAMBDA) ((MLIST) $LIST $N) (($MAPN) (($GEN) $N) $LIST $N))
		  MEXPR)

 	(ARGS '$MULTIMAP '(NIL . 2))

 	(ADD2LNC '(($MULTIMAP) $LIST $N) $FUNCTIONS)

 	(DEFPROP $MULTILIN
		 (LAMBDA ($LIST $FPRIM) 
		   (SIMPLIFY ($MULTIMAP $LIST ($LENGTH $LIST))))
		 EXPR)

 	(ARGS '$MULTILIN '(NIL . 2))

 	(DEFPROP $MULTILIN T TRANSLATED)

 	(ADD2LNC '$MULTILIN $PROPS)

 	(MDEFPROP $MULTILIN
		  ((LAMBDA)
		   ((MLIST) $LIST $FPRIM)
		   (($MULTIMAP) $LIST (($LENGTH) $LIST)))
		  MEXPR)

 	(ARGS '$MULTILIN '(NIL . 2))

 	(ADD2LNC '(($MULTILIN) $LIST $FPRIM) $FUNCTIONS)

 	(DEFPROP
	 $MAPN
	 (LAMBDA ($FN0 $LIST $N) 
	   (SIMPLIFY
	    ($NEWMAP (SIMPLIFY ($SUBSTITUTE (SIMPLIFY (GENSYM))
					    (MEVAL1 '$%QWX)
					    (MEVAL '((LAMBDA)
						     ((MLIST) $%QWX)
						     ((MCOND)
						      (($CONSTANTP) $%QWX)
						      $%QWX
						      T
						      (($FN0)
						       (($SUBSTPART)
							$%QWX
							$LIST
							$N)))))))
		     (MEVAL1 '(($LIST ARRAY) $N)))))
	 EXPR)

 	(ARGS '$MAPN '(NIL . 3))

 	(DEFPROP $MAPN T TRANSLATED)

 	(ADD2LNC '$MAPN $PROPS)

 	(MDEFPROP $MAPN
		  ((LAMBDA)
		   ((MLIST) $FN0 $LIST $N)
		   (($NEWMAP)
		    (($SUBSTITUTE)
		     ((GENSYM))
		     $%QWX
		     ((LAMBDA)
		      ((MLIST) $%QWX)
		      ((MCOND)
		       (($CONSTANTP) $%QWX)
		       $%QWX
		       T
		       (($FN0) (($SUBSTPART) $%QWX $LIST $N)))))
		    (($LIST ARRAY) $N)))
		  MEXPR)

 	(ARGS '$MAPN '(NIL . 3))

 	(ADD2LNC '(($MAPN) $FN0 $LIST $N) $FUNCTIONS)

 	(DEFPROP
	 $GEN
	 (LAMBDA ($I) 
	   (COND
	    ((LIKE $I 1) (MEVAL1 '$FPRIM))
	    (T
	     (SIMPLIFY
	      ($SUBSTITUTE
	       (ADD $I -1)
	       (MEVAL1 '$%QWIM)
	       (SIMPLIFY ($SUBSTITUTE (SIMPLIFY (GENSYM))
				      (MEVAL1 '$%QWNOM)
				      (MEVAL '((LAMBDA)
					       ((MLIST) $%QWNOM)
					       (($MAPN)
						(($GEN) $%QWIM)
						$%QWNOM
						$%QWIM))))))))))
	 EXPR)

 	(ARGS '$GEN '(NIL . 1))

 	(DEFPROP $GEN T TRANSLATED)

 	(ADD2LNC '$GEN $PROPS)

 	(MDEFPROP $GEN
		  ((LAMBDA)
		   ((MLIST) $I)
		   ((MCOND)
		    ((MEQUAL) $I 1)
		    $FPRIM
		    T
		    (($SUBSTITUTE)
		     ((MPLUS) $I ((MMINUS) 1))
		     $%QWIM
		     (($SUBSTITUTE)
		      ((GENSYM))
		      $%QWNOM
		      ((LAMBDA)
		       ((MLIST) $%QWNOM)
		       (($MAPN) (($GEN) $%QWIM) $%QWNOM $%QWIM))))))
		  MEXPR)

 	(ARGS '$GEN '(NIL . 1))

 	(ADD2LNC '(($GEN) $I) $FUNCTIONS)

 	(DEFPROP
	 $GFIX
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($QW) 
	      (SETQ $QW
		    (SIMPLIFY ($SUBSTITUTE (LIST '(MLIST)
						 (MEVAL '((MEQUAL) $GT $GDT))
						 (MEVAL '((MEQUAL) $G $GD)))
					   $EXP)))
	      (SIMPLIFY ($EV $QW)))
	    '$QW))
	 EXPR)

 	(ARGS '$GFIX '(NIL . 1))

 	(DEFPROP $GFIX T TRANSLATED)

 	(ADD2LNC '$GFIX $PROPS)

 	(MDEFPROP $GFIX
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $QW)
		    ((MSETQ)
		     $QW
		     (($SUBSTITUTE)
		      ((MLIST) ((MEQUAL) $GT $GDT) ((MEQUAL) $G $GD))
		      $EXP))
		    ((MRETURN) (($EV) $QW))))
		  MEXPR)

 	(ARGS '$GFIX '(NIL . 1))

 	(ADD2LNC '(($GFIX) $EXP) $FUNCTIONS)

 	(DEFPROP $GTA (LAMBDA ($LIST) (SIMPLIFY ($APPLY $GT $LIST))) EXPR)

 	(ARGS '$GTA '(NIL . 1))

 	(DEFPROP $GTA T TRANSLATED)

 	(ADD2LNC '$GTA $PROPS)

 	(MDEFPROP $GTA ((LAMBDA) ((MLIST) $LIST) (($APPLY) $GT $LIST)) MEXPR)

 	(ARGS '$GTA '(NIL . 1))

 	(ADD2LNC '(($GTA) $LIST) $FUNCTIONS)

 	(DEFPROP $GA (LAMBDA ($LIST) (SIMPLIFY ($APPLY $G $LIST))) EXPR)

 	(ARGS '$GA '(NIL . 1))

 	(DEFPROP $GA T TRANSLATED)

 	(ADD2LNC '$GA $PROPS)

 	(MDEFPROP $GA ((LAMBDA) ((MLIST) $LIST) (($APPLY) $G $LIST)) MEXPR)

 	(ARGS '$GA '(NIL . 1))

 	(ADD2LNC '(($GA) $LIST) $FUNCTIONS)

 	(MDEFPROP $GD T MLEXPRP)

 	(MDEFPROP $GD
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   (($MULTILIN) (($GETRED) $LIST) $GA))
		  MEXPR)

 	(ADD2LNC '(($GD) ((MLIST) $LIST)) $FUNCTIONS)

 	(MDEFPROP $GDT T MLEXPRP)

 	(MDEFPROP $GDT
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   (($MULTILIN) (($GETRED) $LIST) $GTA))
		  MEXPR)

 	(ADD2LNC '(($GDT) ((MLIST) $LIST)) $FUNCTIONS)

	(SETQ IBASE 10.)


