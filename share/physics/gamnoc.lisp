
	(SETQ IBASE 8.)

 	(SETQ SAVENO 5372)

 	(DSKSETQ $WICOM '((MLIST SIMP)))

 	(DSKSETQ $METSIG '((MLIST SIMP) 1 -1 -1 -1))

 	(MDEFPROP $COMPDEF
		  ((LAMBDA)
		   ((MLIST) ((MLIST) $LIST))
		   ((MPROG)
		    ((MLIST) $I)
		    ((MSETQ) $LIST (($GETRED) $LIST))
		    ((MDO)
		     $I
		     NIL
		     NIL
		     NIL
		     (($LENGTH) $LIST)
		     NIL
		     ((MPROG)
		      ((MLIST) $PI $LP)
		      ((MSETQ) $PI (($PART) $LIST $I))
		      ((MSETQ) $LP (($LHS) $PI))
		      ((MCOND)
		       ((MNOT) (($FREEOF) $LP $WICOM))
		       (($UNCOMPDEF) $LP)
		       T
		       $FALSE)
		      ((MSETQ) $WICOM (($APPEND) $WICOM ((MLIST) $LP)))
		      ((MSETQ) $COMPS (($APPEND) $COMPS ((MLIST) $PI)))
		      ((MSETQ) (($COMPHASH ARRAY) $LP) (($RHS) $PI))))
		    ((MRETURN) $COMPS)))
		  MEXPR)

 	(ADD2LNC '(($COMPDEF) ((MLIST) $LIST)) $FUNCTIONS)

 	(MDEFPROP $COMPDEF T MLEXPRP)

 	(DEFPROP
	 $NONCOV
	 (LAMBDA ($EXP) 
	   ((LAMBDA ($QW) 
	      (SETQ $QW (SIMPLIFY ($SUBSTITUTE (MEVAL1 '$DNC)
					       (MEVAL1 '$D)
					       (MEVAL '(($DFIX) $EXP)))))
	      (SIMPLIFY ($EV $QW)))
	    '$QW))
	 EXPR)

 	(ARGS '$NONCOV '(NIL . 1))

 	(DEFPROP $NONCOV T TRANSLATED)

 	(ADD2LNC '$NONCOV $PROPS)

 	(MDEFPROP $NONCOV
		  ((LAMBDA)
		   ((MLIST) $EXP)
		   ((MPROG)
		    ((MLIST) $QW)
		    ((MSETQ) $QW (($SUBSTITUTE) $DNC $D (($DFIX) $EXP)))
		    ((MRETURN) (($EV) $QW))))
		  MEXPR)

 	(ARGS '$NONCOV '(NIL . 1))

 	(ADD2LNC '(($NONCOV) $EXP) $FUNCTIONS)

 	(DEFPROP
	 $DNC
	 (LAMBDA ($A $B) 
	   ((LAMBDA ($AV $BV) 
	      (PROG NIL 
		    (COND ((IS ($FREEOF $A $B (MEVAL1 '$WICOM)))
			   (RETURN (MEVAL '(($D) $A $B)))))
		    (SETQ $AV (MEVAL1 '(($COMPHASH ARRAY) $A)))
		    (SETQ $BV (MEVAL1 '(($COMPHASH ARRAY) $B)))
		    (COND ((AND ($INTEGERP $AV) ($INTEGERP $BV))
			   (RETURN (COND ((NOT (LIKE $AV $BV)) 0)
					 (T (MEVAL1 '(($METSIG ARRAY)
						      (ADD 1 $AV))))))))
		    (COND ((AND ($LISTP $AV) ($LISTP $BV))
			   (RETURN (SIMPLIFY ($SUM ((MTIMES)
						    (($METSIG ARRAY) $I)
						    (($AV ARRAY) $I)
						    (($BV ARRAY) $I))
						   $I
						   1
						   4)))))
		    (COND ((NOT (IS ($FREEOF (MEVAL1 '$COMPHASH) $BV)))
			   (RETURN (COND (($LISTP $AV)
					  (MEVAL '(($D) $A $B)))
					 (T (MEVAL '(($D) $B $AV)))))))
		    (COND ((NOT (IS ($FREEOF (MEVAL1 '$COMPHASH) $AV)))
			   (RETURN (COND (($LISTP $BV)
					  (MEVAL '(($D) $B $A)))
					 (T (MEVAL '(($D) $A $BV)))))))
		    (RETURN (COND (($LISTP $AV)
				   (RETURN ($PART $AV (ADD 1 $BV))))
				  (T (RETURN ($PART $BV (ADD 1 $AV))))))))
	    '$AV
	    '$BV))
	 EXPR)

 	(ARGS '$DNC '(NIL . 2))

 	(DEFPROP $DNC T TRANSLATED)

 	(ADD2LNC '$DNC $PROPS)

 	(MDEFPROP $DNC
		  ((LAMBDA)
		   ((MLIST) $A $B)
		   ((MPROG)
		    ((MLIST) $AV $BV)
		    ((MCOND)
		     (($FREEOF) $A $B $WICOM)
		     ((MRETURN) (($D) $A $B))
		     T
		     $FALSE)
		    ((MSETQ) $AV (($COMPHASH ARRAY) $A))
		    ((MSETQ) $BV (($COMPHASH ARRAY) $B))
		    ((MCOND)
		     ((MAND) (($INTEGERP) $AV) (($INTEGERP) $BV))
		     ((MRETURN)
		      ((MCOND)
		       ((MNOTEQUAL) $AV $BV)
		       0
		       T
		       (($METSIG ARRAY) ((MPLUS) 1 $AV))))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MAND) (($LISTP) $AV) (($LISTP) $BV))
		     ((MRETURN)
		      (($SUM)
		       ((MTIMES)
			(($METSIG ARRAY) $I)
			(($AV ARRAY) $I)
			(($BV ARRAY) $I))
		       $I
		       1
		       4))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOT) (($FREEOF) $COMPHASH $BV))
		     ((MRETURN)
		      ((MCOND) (($LISTP) $AV) (($D) $A $B) T (($D) $B $AV)))
		     T
		     $FALSE)
		    ((MCOND)
		     ((MNOT) (($FREEOF) $COMPHASH $AV))
		     ((MRETURN)
		      ((MCOND) (($LISTP) $BV) (($D) $B $A) T (($D) $A $BV)))
		     T
		     $FALSE)
		    ((MCOND)
		     (($LISTP) $AV)
		     ((MRETURN) (($PART) $AV ((MPLUS) 1 $BV)))
		     T
		     ((MRETURN) (($PART) $BV ((MPLUS) 1 $AV))))))
		  MEXPR)

 	(ARGS '$DNC '(NIL . 2))

 	(ADD2LNC '(($DNC) $A $B) $FUNCTIONS)

 	(MDEFPROP $UNCOMPDEF T MLEXPRP)

 	(MDEFPROP $UNCOMPDEF
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
		      ((MLIST) $PI)
		      ((MSETQ) $PI (($PART) $LIST $I))
		      ((MSETQ) $WICOM (($DELETE) $PI $WICOM))
		      ((MSETQ)
		       $COMPS
		       (($APPLY)
			$DELETE
			((MLIST)
			 ((MEQUAL) $PI (($COMPHASH ARRAY) $PI))
			 $COMPS)))))
		    ((MRETURN) $COMPS)))
		  MEXPR)

 	(ADD2LNC '(($UNCOMPDEF) ((MLIST) $LIST)) $FUNCTIONS)

 	(ADD2LNC '$WICOM $VALUES)

 	(ADD2LNC '$METSIG $VALUES)

	(SETQ IBASE 10.)


