;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 5/12/85 13:46:23;;Maxima System version 8
;;** Variable settings were **

(in-package "MAXIMA")

;;TRANSCOMPILE:FALSE;
;;TR_SEMICOMPILE:FALSE;
;;TRANSLATE_FAST_ARRAYS:TRUE;
;;TR_WARN_UNDECLARED:COMPILE;
;;TR_WARN_MEVAL:COMPFILE;
;;TR_WARN_FEXPR:COMPFILE;
;;TR_WARN_MODE:ALL;
;;TR_WARN_UNDEFINED_VARIABLE:ALL;
;;TR_FUNCTION_CALL_DEFAULT:GENERAL;
;;TR_ARRAY_AS_REF:TRUE;
;;TR_NUMER:FALSE;
;;DEFINE_VARIABLE:FALSE;
(eval-when (compile eval load)
  (defprop $eliminate t translated)
  (add2lnc '$eliminate $props)
  (defmtrfun
      ($eliminate $any mdefine nil nil)
      ($eqns $vars)
    nil
    ((lambda
	 ($teqns $sv $se $l $flag $dispflag)
       nil
       nil
       (setq $flag (setq $dispflag nil))
       (cond ((not (and ($listp $eqns)
			($listp $vars)))
	      (simplify ($error '|&The arguments must both be lists|))))
       (cond ((> ($length $vars)
		 (setq $l ($length $eqns)))
	      (simplify ($error '|&More variables then equations|))))
       (cond ((eql $l 1)
	      (simplify ($error '|&Can't eliminate from only one equation|))))
       (cond ((eql ($length $vars) $l)
	      (setq $vars ($reverse $vars))
	      (setq $sv (maref $vars 1))
	      (setq $vars ($reverse (simplify ($rest $vars))))
	      (setq $flag t)))
       (setq $eqns (simplify (map1 (getopr 'meqhk) $eqns)))
       (do (($v)
	    (mdo (cdr $vars) (cdr mdo)))
	   ((null mdo) '$done)
	 (setq $v (car mdo))
	 (setq $teqns '((mlist)))
	 (do (($j 1 (f+ 1 $j)))
	     ((or (> $j $l)
		  (not ($freeof $v (simplify ($first $eqns)))))
	      '$done)
	   (setq $teqns ($cons (simplify ($first $eqns)) $teqns))
	   (setq $eqns (simplify ($rest $eqns))))
	 (cond ((like $eqns '((mlist)))
		(setq $eqns $teqns))
	       (t
		(setq $teqns ($append $teqns (simplify ($rest $eqns))))
		(setq $eqns (simplify ($first $eqns)))
		(setq $l (add* $l -1))
		(setq $se '((mlist)))
		(do (($j 1 (f+ 1 $j)))
		    ((> $j $l) '$done)
		  (setq $se ($cons (simplify ($resultant $eqns
							 (maref $teqns $j)
							 $v))
				   $se)))
		(setq $eqns $se))))
       (cond
	 ($flag
	  (list
	   '(mlist)
	   ($rhs
	    (simplify
	     (mfuncall '$ev
		       (simplify ($last (simplify ($solve (maref $eqns 1)
							  $sv))))
		       '$eval)))))
	 (t $eqns)))
     '$teqns
     '$sv
     '$se
     '$l
     '$flag
     '$dispflag)))