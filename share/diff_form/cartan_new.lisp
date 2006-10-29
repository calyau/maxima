;;this program is derived from maxima/share/calculus/cartan.lisp
;;I add clifford operator &,&2
;;I add clifford differential operator d_c
  

(DEFPROP $\| %\| VERB) 
(DEFPROP $\| &\| OP) 
(DEFPROP &\| $\| OPR) 
(ADD2LNC (QUOTE &\|) $PROPS) 
(DEFPROP $\| DIMENSION-infix DIMENSION) 
(DEFPROP $\| (32 124 32) DISSYM) 
(DEFPROP $\| 120 LBP) 
(DEFPROP $\| 180 RBP) 
(DEFPROP $\| PARSE-INFIX LED) 
(DEFPROP $\| MSIZE-INFIX GRIND) 
(DEFPROP %\| DIMENSION-infix DIMENSION) 
(DEFPROP %\|  (32 124 32) DISSYM) 
(MDEFPROP $\| ((LAMBDA) ((MLIST) $V $F) ((MPROG SIMP) ((MLIST SIMP)
 $I $J $EXT101 $EXT102 $EXT103 $EXT104) ((MSETQ SIMP) $EXT103 
(($EXPAND SIMP) $F)) ((MSETQ SIMP) $EXT102 ((MTIMES SIMP) 
(($V SIMP ARRAY) 1) (($COEFF SIMP) $EXT103 (($BASIS SIMP ARRAY) 1)))) 
((MDO SIMP) $I 2 NIL NIL $DIM NIL ((MPROGN SIMP) ((MSETQ SIMP) 
$EXT101 (($COEFF SIMP) $EXT103 (($BASIS SIMP ARRAY) $I))) 
((MCOND SIMP) ((MNOTEQUAL SIMP) $EXT101 0) ((MSETQ SIMP) $EXT101
 (($SUBSTITUTE SIMP) (($EXTSUB SIMP ARRAY) $I) $EXT101)) T $FALSE)
 ((MSETQ SIMP) $EXT102 ((MPLUS SIMP) $EXT102 ((MTIMES SIMP) $EXT101
 (($V SIMP ARRAY) $I)))))) ((MRETURN SIMP) (($EXPAND SIMP) $EXT102))))
 MEXPR) 
(ADD2LNC (QUOTE (($\|) $V $F)) $FUNCTIONS) 
(DEFPROP %\| $\| NOUN) 

(DEFPROP $@ %@ VERB) 
(DEFPROP $@ &@ OP) 
(DEFPROP &@ $@ OPR) 
(ADD2LNC (QUOTE &@) $PROPS) 
(DEFPROP $@ DIMENSION-infix DIMENSION) 
(DEFPROP $@ (32 38 32) DISSYM) 
;;(DEFPROP $@ 140 LBP) 
;;(DEFPROP $@ 180 RBP)
(DEFPROP $@ 80 LBP)
(DEFPROP $@ 100 RBP) 
(DEFPROP $@ PARSE-INFIX LED) 
(DEFPROP $@ MSIZE-INFIX GRIND) 
(DEFPROP %@ DIMENSION-infix DIMENSION) 
(DEFPROP %@ (32 38 32) DISSYM)
;;exterior operator @ 
(MDEFPROP $@ 
	  ((LAMBDA) 
	   ((MLIST) $F $G) 
	   ((MPROG SIMP) 
	    ((MLIST SIMP) $I $J $EXT101 $EXT102 $EXT103 $EXT104 $EXT105) 
	    ((MSETQ SIMP) $EXT101 0) 
	    ((MSETQ SIMP) $EXT102 $TRUE) 
	    ((MSETQ SIMP) $EXT103 
	     (($EXPAND SIMP) $F)) 
	    ((MDO SIMP) $I $DIM -1 NIL 0 NIL 
	     ((MPROGN SIMP) 
	      ((MSETQ SIMP) $EXT104 
	       (($EXPAND SIMP) 
		(($BOTHCOEF SIMP) $EXT103 
		 (($BASIS SIMP ARRAY) $I)))) 
	      ((MSETQ SIMP) $EXT105 
	       (($FIRST SIMP) $EXT104)) 
	      ((MCOND SIMP) 
	       ((MNOTEQUAL SIMP) $EXT105 0) 
	       ((MPROGN SIMP) 
		((MSETQ SIMP) $EXT103 (($LAST SIMP) $EXT104))  
		((MSETQ SIMP) $EXT101 
		 ((MPLUS SIMP) $EXT101 
;;(($@ SIMP) $EXT105
;;move $EXT105 later 
		  (($@ SIMP) $EXT105
		   ((MTIMES SIMP)
		    (($BASIS SIMP ARRAY) $I) 
		    (($SUBSTITUTE SIMP) 
		     (($EXTSUBB SIMP ARRAY) $I) $G)) )))  
		((MSETQ SIMP) $EXT102 $FALSE)) T $FALSE))) 
	    ((MCOND SIMP) $EXT102 
	     ((MRETURN SIMP) 
	      (($EXPAND SIMP) ((MTIMES SIMP) $F $G))) 
	     T ((MRETURN SIMP) (($EXPAND SIMP) $EXT101))))) 
	  MEXPR)
 
(ADD2LNC (QUOTE (($@) $F $G)) $FUNCTIONS) 
(DEFPROP %@ $@ NOUN) 
;;start definition with clifford operator
(DEFPROP $& %& VERB) 
(DEFPROP $& &\& OP) 
(DEFPROP &\& $\& OPR) 
(ADD2LNC (QUOTE &\&) $PROPS) 
(DEFPROP $& DIMENSION-infix DIMENSION) 
(DEFPROP $& (32 38 32) DISSYM) 
(DEFPROP $& 140 LBP) 
(DEFPROP $& 180 RBP) 
(DEFPROP $& PARSE-INFIX LED) 
(DEFPROP $& MSIZE-INFIX GRIND) 
(DEFPROP %& DIMENSION-infix DIMENSION) 
(DEFPROP %& (32 38 32) DISSYM) 
;;clifford operator &
(MDEFPROP $& ((LAMBDA) 
	      ((MLIST) $F $G) 
	      ((MPROG SIMP)
	       ((MLIST SIMP) $I $J $EXT101 $EXT102 $EXT103 $EXT104 $EXT105)
	       ((MSETQ SIMP) $EXT101 0) 
	       ((MSETQ SIMP) $EXT102 $TRUE) 
	       ((MSETQ SIMP) $EXT103 
		(($EXPAND SIMP) $F)) 
	       ((MDO SIMP) $I $DIM -1 NIL 1 NIL 
		((MPROGN SIMP) ((MSETQ SIMP) $EXT104 
				(($EXPAND SIMP) 
				 (($BOTHCOEF SIMP) $EXT103 
				  (($BASIS SIMP ARRAY) $I))))
		 ((MSETQ SIMP) $EXT105 (($FIRST SIMP) $EXT104)) 
		 ((MCOND SIMP) ((MNOTEQUAL SIMP) $EXT105 0) 
		  ((MPROGN SIMP) ((MSETQ SIMP) $EXT103 (($LAST SIMP) $EXT104))
		   ((MSETQ SIMP) $EXT101 ((MPLUS SIMP) $EXT101 
;;(($& SIMP) $EXT105				  
					  (($& SIMP) $EXT105
					   ((MTIMES SIMP)  
					    (($BASIS SIMP ARRAY) $I) 
					    (($SUBSTITUTE SIMP) 
					     (($EXTSUBB2 SIMP ARRAY) $I) $G))
					   )))
		   ((MSETQ SIMP) $EXT102 $FALSE)) T $FALSE))) 
	       ((MCOND SIMP) $EXT102 
		((MRETURN SIMP) (($EXPAND SIMP) 
				 ((MTIMES SIMP) $F $G))) T 
				 ((MRETURN SIMP) (($EXPAND SIMP) $EXT101))))) MEXPR) 

(ADD2LNC (QUOTE (($&) $F $G)) $FUNCTIONS) 
(DEFPROP %& $& NOUN)

;;exterior differntial operator
(MDEFPROP $D ((LAMBDA) 
	      ((MLIST) $F) 
	      (($SUM SIMP) 
	       (($@ SIMP) 
		(($BASIS SIMP ARRAY) $I) 
		(($DIFF SIMP) $F 
		 (($COORDS SIMP ARRAY) $I)))
	       $I 1 $DIM)) MEXPR) 

(ADD2LNC (QUOTE (($D2) $F)) $FUNCTIONS)

;;clifford differntial operator
(MDEFPROP $D_C ((LAMBDA) 
	      ((MLIST) $F) 
	      (($SUM SIMP) 
	       (($& SIMP) 
		(($BASIS SIMP ARRAY) $I) 
		(($DIFF SIMP) $F 
		 (($COORDS SIMP ARRAY) $I)))
	       $I 1 $DIM)) MEXPR)

(ADD2LNC (QUOTE (($D_C) $F)) $FUNCTIONS)

;;another clifford operator with different basis.
;;clifford operator & and exterior operator go with.
;;but we ofen need to calc clifford algebra another basis at the same time.
;;before using &2 operator you must "infix("&2")$ "
(DEFPROP $&2 %&2 VERB) 
(DEFPROP $&2 &\&2 OP) 
(DEFPROP &\&2 $\&2 OPR) 
(ADD2LNC (QUOTE &\&2) $PROPS) 
(DEFPROP $&2 DIMENSION-infix DIMENSION) 
(DEFPROP $&2 (32 38 32) DISSYM) 
(DEFPROP $&2 140 LBP) 
(DEFPROP $&2 180 RBP) 
(DEFPROP $&2 PARSE-INFIX LED) 
(DEFPROP $&2 MSIZE-INFIX GRIND) 
(DEFPROP %&2 DIMENSION-infix DIMENSION) 
(DEFPROP %&2 (32 38 32) DISSYM) 
;;clifford operator &
(MDEFPROP $&2 ((LAMBDA) 
	      ((MLIST) $F $G) 
	      ((MPROG SIMP)
	       ((MLIST SIMP) $I $J $EXT101 $EXT102 $EXT103 $EXT104 $EXT105)
	       ((MSETQ SIMP) $EXT101 0) 
	       ((MSETQ SIMP) $EXT102 $TRUE) 
	       ((MSETQ SIMP) $EXT103 
		(($EXPAND SIMP) $F)) 
	       ((MDO SIMP) $I $N_DIM -1 NIL 1 NIL 
		((MPROGN SIMP) ((MSETQ SIMP) $EXT104 
				(($EXPAND SIMP) 
				 (($BOTHCOEF SIMP) $EXT103 
				  (($BASIS2 SIMP ARRAY) $I))))
		 ((MSETQ SIMP) $EXT105 (($FIRST SIMP) $EXT104)) 
		 ((MCOND SIMP) ((MNOTEQUAL SIMP) $EXT105 0) 
		  ((MPROGN SIMP) ((MSETQ SIMP) $EXT103 (($LAST SIMP) $EXT104))
		   ((MSETQ SIMP) $EXT101 ((MPLUS SIMP) $EXT101 
					  (($&2 SIMP) $EXT105 ((MTIMES SIMP) 
							      (($BASIS2 SIMP ARRAY) $I) 
							      (($SUBSTITUTE SIMP) 
							       (($EXTSUBB4 SIMP ARRAY) $I) $G)))))
		   ((MSETQ SIMP) $EXT102 $FALSE)) T $FALSE))) 
	       ((MCOND SIMP) $EXT102 
		((MRETURN SIMP) (($EXPAND SIMP) 
				 ((MTIMES SIMP) $F $G))) T 
				 ((MRETURN SIMP) (($EXPAND SIMP) $EXT101))))) MEXPR) 

(ADD2LNC (QUOTE (($&2) $F $G)) $FUNCTIONS) 
(DEFPROP %&2 $&2 NOUN)
 

 











