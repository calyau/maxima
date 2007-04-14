;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 5/12/85 13:57:48;;Maxima System version 8
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
(eval-when
    #+gcl (compile eval load)
    #-gcl (:compile-toplevel :execute :load-toplevel)
  (defprop $adjoint t translated)
  (add2lnc '$adjoint $props)
  (defmtrfun
      ($adjoint $any mdefine nil nil)
      ($mat)
    nil
    ((lambda
	 ($adj $n)
       nil
       (setq $n ($length $mat))
       (setq $adj (simplify ($ident $n)))
       (cond
	 ((not (like $n 1))
	  (do (($i 1 (f+ 1 $i)))
	      ((> $i $n) '$done)
	    (do (($j 1 (f+ 1 $j)))
		((> $j $n) '$done)
	      (maset (mul* (power -1 (f+ $i $j))
			   (simplify ($determinant (simplify ($minor $mat
								     $j
								     $i)))))
		     $adj
		     $i
		     $j)))))
       $adj)
     '$adj
     '$n)))
(eval-when
    #+gcl (compile eval load)
    #-gcl (:compile-toplevel :execute :load-toplevel)
  (defprop $invert t translated)
  (add2lnc '$invert $props)
  (defmtrfun ($invert $any mdefine nil nil)
      ($mat)
    nil
    ((lambda ($adj $ans)
       nil
       (setq $adj (simplify ($adjoint $mat)))
       (setq $ans ((lambda ($scalarmatrixp)
		     nil
		     (div $adj
			  (ncmul2 (simplify ($row $mat 1))
				  (simplify ($col $adj
						  1)))))
		   t))
       (cond ((and (like (trd-msymeval $scalarmatrixp
				       '$scalarmatrixp)
			 t)
		   (eql ($length $mat) 1))
	      (maref $ans 1 1))
	     (t $ans)))
     '$adj
     '$ans)))
