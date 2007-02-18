;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 5/12/85 13:15:46;;Maxima System version 8
;;** Variable settings were **

(in-package :maxima)

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
  (defprop $desolve t translated)
  (add2lnc '$desolve $props)
  (defmtrfun
      ($desolve $any mdefine nil nil)
      ($eqns $vars)
    nil
    ((lambda
	 ($teqns $tvars $ovar $lvar $flag $dispflag)
       nil
       nil
       (setq $flag nil)
       (cond ((not ($listp $vars))
	      (setq $eqns (list '(mlist) $eqns))
	      (setq $vars (list '(mlist) $vars))
	      (setq $flag t)))
       (cond
	 ((not (eql ($length (setq $ovar (maref $vars 1)))
		    1))
	  (simplify ($error $ovar
			    (make-mstring "contains more than one independent variable.")))))
       (setq $ovar (simplify ($inpart $ovar 1)))
       (setq $dispflag nil)
       (setq
	$teqns
	(simplify (map1 (getopr (m-tlambda&env (($z) ($ovar $lvar))
					       nil
					       (simplify ($laplace $z
								   $ovar
								   $lvar))))
			$eqns)))
       (setq
	$tvars
	(simplify (map1 (getopr (m-tlambda&env (($z) ($ovar $lvar))
					       nil
					       (simplify `((%laplace) ,$z ,
							   $ovar ,$lvar))))
			$vars)))
       (setq
	$teqns
	((lambda (errcatch ret)
	   (cond ((null (setq ret (errset (progn (simplify ($solve $teqns
								   $tvars)))
					  lisperrprint)))
		  (errlfun1 errcatch)))
	   (cons '(mlist) ret))
	 (cons bindlist loclist)
	 nil))
       (cond ((or (like $teqns '((mlist)))
		  (like $teqns (list '(mlist) '((mlist)))))
	      (simplify ($error (make-mstring "`desolve' can't handle this case."))))
	     (t (setq $teqns (simplify ($first $teqns)))))
       (cond ((not (like $flag t))
	      (setq $teqns (simplify ($first $teqns)))))
       (setq
	$teqns
	(simplify (map1 (getopr (m-tlambda&env (($z) ($lvar $ovar))
					       nil
					       (simplify ($ilt $z
							       $lvar
							       $ovar))))
			$teqns)))
       (cond ((and $flag (eql ($length $tvars) 1))
	      (maref $teqns 1))
	     (t $teqns)))
     '$teqns
     '$tvars
     '$ovar
     '$lvar
     '$flag
     '$dispflag)))
