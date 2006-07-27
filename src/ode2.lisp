;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;; 
;;;Translated on: 7/18/85 13:12:45;;Maxima System version 2
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





nil
(eval-when (compile load eval) (meval* '(($declare) $pt $special $yp 
					 $special $yold $special $%q%
					 $special $ynew
					 $special $x $special $y $special 
					 $method $special $%f% $special $%g%
					 $special $msg1 $special $msg2 
					 $special $intfactor $special 
					 $odeindex $special $singsolve 
					 $special)))
#+symbolics ;;above doesn't affect compiler for some reasons
(declare (special $pt  $yp                                          $yold  $%q% 
		  $x  $y  
		  $method  $%f%  $%g%
		  $msg1  $msg2 
		  $intfactor  
		  $odeindex  $singsolve ))


(eval-when (compile eval load)
  (defprop $ode2 t translated)
  (add2lnc '$ode2 $props)
  (defmtrfun
      ($ode2 $any mdefine nil nil)
      ($eq $yold $x)
    nil
    ((lambda
	 ($derivsubst $ynew)
       nil
       (simplify
	($substitute
	 (trd-msymeval $yold '$yold)
	 $ynew
	 (simplify ($ode2a (simplify ($substitute $ynew
						  (trd-msymeval $yold
								'$yold)
						  $eq))
			   $ynew
			   (trd-msymeval $x '$x))))))
     nil
     '$ynew)))
(eval-when (compile eval load)
  (defprop $ode2a t translated)
  (add2lnc '$ode2a $props)
  (defmtrfun
      ($ode2a $any mdefine nil nil)
      ($eq $y $x)
    nil
    ((lambda
	 ($de $a1 $a2 $a3 $a4 $%q% $msg)
       nil
       (prog
	   nil
	  (setq $intfactor nil)
	  (setq $method '$none)
	  (cond (($freeof (simplify `((%derivative) ,(trd-msymeval $y '$y) ,
				      (trd-msymeval $x '$x) 2))
			  $eq)
		 (cond (($ftest (simplify ($ode1a $eq
						  (trd-msymeval $y '$y)
						  (trd-msymeval $x '$x))))
			(return (trd-msymeval $%q% '$%q%)))
		       (t (return nil)))))
	  (cond
	    ((not
	      (like
	       (simplify
		($derivdegree
		 (setq $de (simplify ($desimp (add* ($lhs $eq)
						    (*mminus ($rhs $eq))))))
		 (trd-msymeval $y '$y)
		 (trd-msymeval $x '$x)))
	       2))
	     (return ($failure (trd-msymeval $msg '$msg) $eq))))
	  (setq $a1 (simplify ($coeff $de
				      (simplify `((%derivative) ,
						  (trd-msymeval $y '$y) ,
						  (trd-msymeval $x '$x) 2)))))
	  (setq $a2 (simplify ($coeff $de
				      (simplify `((%derivative) ,
						  (trd-msymeval $y '$y) ,
						  (trd-msymeval $x '$x))))))
	  (setq $a3 (simplify ($coeff $de (trd-msymeval $y '$y))))
	  (setq
	   $a4
	   (simplify
	    ($expand
	     (add* $de
		   (*mminus (mul* $a1
				  (simplify `((%derivative) ,
					      (trd-msymeval $y '$y) ,
					      (trd-msymeval $x '$x) 2))))
		   (mul* (*mminus $a2)
			 (simplify `((%derivative) ,(trd-msymeval $y '$y) ,
				     (trd-msymeval $x '$x))))
		   (mul* (*mminus $a3) (trd-msymeval $y '$y))))))
	  (cond ((and ($pr2 $a1)
		      ($pr2 $a2)
		      ($pr2 $a3)
		      ($pr2 $a4)
		      ($ftest (simplify ($hom2 $a1 $a2 $a3))))
		 (cond ((like $a4 0)
			(return (trd-msymeval $%q% '$%q%)))
		       (t (return (simplify ($varp (trd-msymeval $%q% '$%q%)
						   (div (*mminus $a4) $a1))))))))
	  (return (cond (($ftest (simplify ($reduce $de)))
			 (return (trd-msymeval $%q% '$%q%)))
			(t (return nil))))))
     '$de
     '$a1
     '$a2
     '$a3
     '$a4
     '$%q%
     '$msg1)))
(eval-when (compile eval load)
  (defprop $ode1a t translated)
  (add2lnc '$ode1a $props)
  (defmtrfun
      ($ode1a $any mdefine nil nil)
      ($eq $y $x)
    nil
    ((lambda
	 ($de $des)
       nil
       (prog
	   nil
	  (cond
	    ((not
	      (like
	       (simplify
		($derivdegree
		 (setq $de (simplify ($expand (add* ($lhs $eq)
						    (*mminus ($rhs $eq))))))
		 (trd-msymeval $y '$y)
		 (trd-msymeval $x '$x)))
	       1))
	     (return ($failure (trd-msymeval $msg1 '$msg1) $eq))))
	  (cond ((like (simplify ($linear2 $de
					   (simplify `((%derivative) ,
						       (trd-msymeval $y '$y) ,
						       (trd-msymeval $x '$x)))))
		       nil)
		 (return ($failure (trd-msymeval $msg2 '$msg2) $eq))))
	  (setq $des (simplify ($desimp $de)))
	  (setq $de (simplify ($solve1 $des
				       (simplify `((%derivative) ,
						   (trd-msymeval $y '$y) ,
						   (trd-msymeval $x '$x))))))
	  (cond (($ftest (simplify ($solvelnr $de)))
		 (return (trd-msymeval $%q% '$%q%))))
	  (cond (($ftest (simplify ($separable $de)))
		 (return (trd-msymeval $%q% '$%q%))))
	  (cond (($ftest (simplify ($integfactor (trd-msymeval $%g% '$%g%)
						 (trd-msymeval $%f% '$%f%))))
		 (return (simplify ($exact (mul* (trd-msymeval $%q% '$%q%)
						 (trd-msymeval $%g% '$%g%))
					   (mul* (trd-msymeval $%q% '$%q%)
						 (trd-msymeval $%f% '$%f%)))))))
	  (cond ((like (simplify ($linear2 $des
					   (simplify `((%derivative) ,
						       (trd-msymeval $y '$y) ,
						       (trd-msymeval $x '$x)))))
		       nil)
		 (return ($failure (trd-msymeval $msg2 '$msg2) $eq))))
	  (cond (($ftest (simplify ($integfactor (trd-msymeval $%g% '$%g%)
						 (trd-msymeval $%f% '$%f%))))
		 (return (simplify ($exact (mul* (trd-msymeval $%q% '$%q%)
						 (trd-msymeval $%g% '$%g%))
					   (mul* (trd-msymeval $%q% '$%q%)
						 (trd-msymeval $%f% '$%f%)))))))
	  (cond (($ftest (simplify ($solvehom $de)))
		 (return (trd-msymeval $%q% '$%q%))))
	  (cond (($ftest (simplify ($solvebernoulli $de)))
		 (return (trd-msymeval $%q% '$%q%))))
	  (return (cond (($ftest (simplify ($genhom $de)))
			 (return (trd-msymeval $%q% '$%q%)))
			(t (return nil))))))
     '$de
     '$des)))
(eval-when (compile eval load)
  (defprop $desimp t translated)
  (add2lnc '$desimp $props)
  (defmtrfun
      ($desimp $any mdefine nil nil)
      ($eq)
    nil
    ((lambda
	 ($inflag)
       nil
       (prog
	   nil
	  (setq $eq (simplify ($factor $eq)))
	  (cond ((or ($atom $eq)
		     (not (like (simplify ($inpart $eq 0)) '&*)))
		 (return (simplify ($expand $eq)))))
	  (setq
	   $eq
	   (simplify
	    (map1
	     (getopr (m-tlambda ($u)
				nil
				(cond (($freeof (simplify ($nounify '$diff))
						$u)
				       1)
				      (t $u))))
	     $eq)))
	  (return (simplify ($expand $eq)))))
     t)))
(eval-when (compile eval load)
  (defprop $pr2 t translated)
  (add2lnc '$pr2 $props)
  (defmtrfun ($pr2 $boolean mdefine nil nil)
      ($%f%)
    nil
    ($freeof (trd-msymeval $y '$y)
	     (simplify `((%derivative) ,(trd-msymeval $y '$y) ,
			 (trd-msymeval $x '$x)))
	     (simplify `((%derivative) ,(trd-msymeval $y '$y) ,
			 (trd-msymeval $x '$x) 2))
	     (trd-msymeval $%f% '$%f%))))
(eval-when (compile eval load)
  (defprop $ftest t translated)
  (add2lnc '$ftest $props)
  (defmtrfun ($ftest $boolean mdefine nil nil)
      ($call)
    nil
    (not (like (setq $%q% $call)
	       nil))))
(eval-when (compile eval load)
  (defprop $solve1 t translated)
  (add2lnc '$solve1 $props)
  (defmtrfun ($solve1 $any mdefine nil nil)
      ($eq $y)
    nil
    ((lambda ($programmode)
       nil
       (simplify ($first (simplify ($solve $eq
					   (trd-msymeval $y '$y))))))
     t)))
(eval-when (compile eval load)
  (defprop $linear2 t translated)
  (add2lnc '$linear2 $props)
  (defmtrfun
      ($linear2 $any mdefine nil nil)
      ($expr $x)
    nil
    ((lambda
	 nil
       nil
       (prog
	   nil
	  (setq $%f% (simplify ($ratcoef $expr (trd-msymeval $x '$x))))
	  (cond ((not ($freeof (trd-msymeval $x '$x) (trd-msymeval $%f% '$%f%)))
		 (return nil)))
	  (setq
	   $%g%
	   (simplify ($ratsimp (add* $expr
				     (*mminus (mul* (trd-msymeval $%f%
								  '$%f%)
						    (trd-msymeval $x '$x)))))))
	  (return ($freeof (trd-msymeval $x '$x) (trd-msymeval $%g% '$%g%))))))))
(eval-when (compile eval load)
  (defprop $solvelnr t translated)
  (add2lnc '$solvelnr $props)
  (defmtrfun
      ($solvelnr $any mdefine nil nil)
      ($eq)
    nil
    ((lambda
	 ($%f% $%g% $w $%c)
       nil
       (prog
	   nil
	  (cond ((like (simplify ($linear2 ($rhs $eq) (trd-msymeval $y '$y)))
		       nil)
		 (return nil)))
	  (setq
	   $w
	   (simplify ($exp (simplify ($integrate (trd-msymeval $%f% '$%f%)
						 (trd-msymeval $x '$x))))))
	  (setq $method '$linear)
	  (return
	    (simplify
	     (list
	      '(mequal)
	      (trd-msymeval $y '$y)
	      (mul* $w
		    (add* (simplify ($integrate (div (trd-msymeval $%g%
								   '$%g%)
						     $w)
						(trd-msymeval $x '$x)))
			  $%c)))))))
     '$%f%
     '$%g%
     '$w
     '$%c)))
(eval-when (compile eval load)
  (defprop $separable t translated)
  (add2lnc '$separable $props)
  (defmtrfun
      ($separable $any mdefine nil nil)
      ($eq)
    nil
    ((lambda
	 ($xpart $ypart $flag $inflag $%c)
       nil
       (prog
	   nil
	  (setq $eq (simplify ($factor ($rhs $eq))))
	  (cond ((or ($atom $eq)
		     (not (like (simplify ($inpart $eq 0)) '&*)))
		 (setq $eq (list '(mlist) $eq))))
	  (do (($u)
	       (mdo (cdr $eq) (cdr mdo)))
	      ((null mdo) '$done)
	    (setq $u (car mdo))
	    (cond (($freeof (trd-msymeval $x '$x) $u)
		   (setq $ypart ($cons $u $ypart)))
		  (($freeof (trd-msymeval $y '$y) $u)
		   (setq $xpart ($cons $u $xpart)))
		  (t (return (setq $flag t)))))
	  (cond ((like $flag t)
		 (return nil)))
	  (cond ((like $xpart '((mlist)))
		 (setq $xpart 1))
		(t (setq $xpart (simplify (mapply-tr '&* $xpart)))))
	  (cond ((like $ypart '((mlist)))
		 (setq $ypart 1))
		(t (setq $ypart (simplify (mapply-tr '&* $ypart)))))
	  (setq $method '$separable)
	  (return
	    (simplify
	     (list
	      '(mequal)
	      (simplify ($ratsimp (simplify ($integrate (div 1 $ypart)
							(trd-msymeval $y
								      '$y)))))
	      (add*
	       (simplify ($ratsimp (simplify ($integrate $xpart
							 (trd-msymeval $x
								       '$x)))))
	       $%c))))))
     '((mlist))
     '((mlist))
     nil
     t
     '$%c)))
(eval-when (compile eval load)
  (defprop $integfactor t translated)
  (add2lnc '$integfactor $props)
  (defmtrfun
      ($integfactor $any mdefine nil nil)
      ($m $n)
    nil
    ((lambda
	 ($b1 $b2 $dmdx $dmdy $dndx $dndy $dd $%e_to_numlog)
       nil
       (prog
	   nil
	  (setq $dmdy (simplify ($ratsimp (simplify ($diff $m
							   (trd-msymeval $y
									 '$y))))))
	  (setq $dndx (simplify ($ratsimp (simplify ($diff $n
							   (trd-msymeval $x
									 '$x))))))
	  (cond ((like (setq $dd (add* $dmdy (*mminus $dndx)))
		       0)
		 (return 1)))
	  (setq $dmdx (simplify ($ratsimp (simplify ($diff $m
							   (trd-msymeval $x
									 '$x))))))
	  (setq $dndy (simplify ($ratsimp (simplify ($diff $n
							   (trd-msymeval $y
									 '$y))))))
	  (cond ((and (like (add* $dmdx (*mminus $dndy)) 0)
		      (like (add* $dmdy $dndx) 0))
		 (return (div 1 (add* (power $m 2) (power $n 2))))))
	  (cond
	    (($freeof (trd-msymeval $y '$y)
		      (setq $b1 (simplify ($ratsimp (div $dd $n)))))
	     (return
	       (simplify ($exp (simplify ($integrate $b1
						     (trd-msymeval $x '$x))))))))
	  (return
	    (cond
	      (($freeof (trd-msymeval $x '$x)
			(setq $b2 (simplify ($ratsimp (div $dd $m)))))
	       (return
		 (simplify ($exp (simplify ($integrate (*mminus $b2)
						       (trd-msymeval $y '$y)))))))
	      (t (return nil))))))
     '$b1
     '$b2
     '$dmdx
     '$dmdy
     '$dndx
     '$dndy
     '$dd
     t)))
(eval-when (compile eval load)
  (defprop $exact t translated)
  (add2lnc '$exact $props)
  (defmtrfun
      ($exact $any mdefine nil nil)
      ($m $n)
    nil
    ((lambda
	 ($a $ynew $%c)
       nil
       (prog
	   nil
	  (setq $intfactor (simplify ($substitute (trd-msymeval $yold '$yold)
						  $ynew
						  (trd-msymeval $%q% '$%q%))))
	  (setq $a (simplify ($integrate (simplify ($ratsimp $m))
					 (trd-msymeval $x '$x))))
	  (setq $method '$exact)
	  (return
	    (simplify
	     (list
	      '(mequal)
	      (simplify
	       ($ratsimp
		(add*
		 $a
		 (simplify
		  ($integrate
		   (simplify
		    ($ratsimp
		     (add*
		      $n
		      (*mminus (simplify ($diff $a
						(trd-msymeval $y '$y)))))))
		   (trd-msymeval $y '$y))))))
	      $%c)))))
     '$a
     '$ynew
     '$%c)))
(eval-when (compile eval load)
  (defprop $solvehom t translated)
  (add2lnc '$solvehom $props)
  (defmtrfun
      ($solvehom $any mdefine nil nil)
      ($eq)
    nil
    ((lambda
	 ($qq $a1 $a2 $%c)
       nil
       (prog
	   nil
	  (setq
	   $a1
	   (simplify ($ratsimp (simplify ($substitute (mul* (trd-msymeval $x
									  '$x)
							    $qq)
						      (trd-msymeval $y '$y)
						      ($rhs $eq))))))
	  (cond ((not ($freeof (trd-msymeval $x '$x) $a1))
		 (return nil)))
	  (setq
	   $a2
	   (simplify
	    ($ratsimp
	     (simplify
	      ($substitute (div (trd-msymeval $y '$y) (trd-msymeval $x '$x))
			   $qq
			   (simplify ($integrate (div 1
						      (add* $a1
							    (*mminus $qq)))
						 $qq)))))))
	  (setq $method '$homogeneous)
	  (return (simplify (list '(mequal)
				  (mul* $%c (trd-msymeval $x '$x))
				  (simplify ($exp $a2)))))))
     '$qq
     '$a1
     '$a2
     '$%c)))
(eval-when (compile eval load)
  (defprop $solvebernoulli t translated)
  (add2lnc '$solvebernoulli $props)
  (defmtrfun
      ($solvebernoulli $any mdefine nil nil)
      ($eq)
    nil
    ((lambda
	 ($a1 $a2 $n $%c)
       nil
       (prog
	   nil
	  (setq $a1 (simplify ($coeff (setq $eq (simplify ($expand ($rhs $eq))))
				      (trd-msymeval $y '$y)
				      1)))
	  (cond ((not ($freeof (trd-msymeval $y '$y) $a1))
		 (return nil)))
	  (setq
	   $n
	   (simplify
	    ($hipow
	     (simplify ($ratsimp (add* $eq
				       (*mminus (mul* $a1
						      (trd-msymeval $y '$y))))))
	     (trd-msymeval $y '$y))))
	  (setq $a2 (simplify ($coeff $eq (trd-msymeval $y '$y) $n)))
	  (cond
	    ((or
	      (not ($freeof (trd-msymeval $y '$y) $a2))
	      (not ($freeof (trd-msymeval $x '$x) (trd-msymeval $y '$y) $n))
	      (like $n 0)
	      (not
	       (like
		$eq
		(simplify ($expand (add* (mul* $a1 (trd-msymeval $y '$y))
					 (mul* $a2
					       (power (trd-msymeval $y '$y)
						      $n))))))))
	     (return nil)))
	  (setq $a1 (simplify ($integrate $a1 (trd-msymeval $x '$x))))
	  (setq $method '$bernoulli)
	  (setq $odeindex $n)
	  (return
	    (simplify
	     (list
	      '(mequal)
	      (trd-msymeval $y '$y)
	      (mul*
	       (simplify ($exp $a1))
	       (power
		(add*
		 (mul*
		  (add* 1 (*mminus $n))
		  (simplify
		   ($integrate (mul* $a2
				     (simplify ($exp (mul* (add* $n -1)
							   $a1))))
			       (trd-msymeval $x '$x))))
		 $%c)
		(div 1 (add* 1 (*mminus $n))))))))))
     '$a1
     '$a2
     '$n
     '$%c)))
(eval-when (compile eval load)
  (defprop $genhom t translated)
  (add2lnc '$genhom $props)
  (defmtrfun
      ($genhom $any mdefine nil nil)
      ($eq)
    nil
    ((lambda
	 ($%g% $u $n $a1 $a2 $a3 $%c)
       nil
       (prog
	   nil
	  (setq $%g% (div (mul* ($rhs $eq) (trd-msymeval $x '$x))
			  (trd-msymeval $y '$y)))
	  (setq
	   $n
	   (simplify
	    ($ratsimp (div (mul* (trd-msymeval $x '$x)
				 (simplify ($diff (trd-msymeval $%g% '$%g%)
						  (trd-msymeval $x '$x))))
			   (mul* (trd-msymeval $y '$y)
				 (simplify ($diff (trd-msymeval $%g% '$%g%)
						  (trd-msymeval $y '$y))))))))
	  (cond ((not ($freeof (trd-msymeval $x '$x) (trd-msymeval $y '$y) $n))
		 (return nil)))
	  (setq
	   $a1
	   (simplify
	    ($ratsimp (simplify ($substitute (div $u
						  (power (trd-msymeval $x
								       '$x)
							 $n))
					     (trd-msymeval $y '$y)
					     (trd-msymeval $%g% '$%g%))))))
	  (setq $a2 (simplify ($integrate (div 1 (mul* $u (add* $n $a1))) $u)))
	  (cond ((not ($freeof (simplify ($nounify '$integrate)) $a2))
		 (return nil)))
	  (setq
	   $a3
	   (simplify
	    ($ratsimp (simplify ($substitute (mul* (trd-msymeval $y '$y)
						   (power (trd-msymeval $x
									'$x)
							  $n))
					     $u
					     $a2)))))
	  (setq $method '$genhom)
	  (setq $odeindex $n)
	  (return (simplify (list '(mequal)
				  (trd-msymeval $x '$x)
				  (mul* $%c (simplify ($exp $a3))))))))
     '$%g%
     '$u
     '$n
     '$a1
     '$a2
     '$a3
     '$%c)))
(eval-when (compile eval load)
  (defprop $hom2 t translated)
  (add2lnc '$hom2 $props)
  (defmtrfun ($hom2 $any mdefine nil nil)
      ($a1 $a2 $a3)
    nil
    ((lambda ($ap $aq $pt)
       nil
       (prog nil
	  (setq $ap (div $a2 $a1))
	  (setq $aq (div $a3 $a1))
	  (cond (($ftest (simplify ($cc2 $ap
					 $aq
					 (trd-msymeval $y '$y)
					 (trd-msymeval $x '$x))))
		 (return (trd-msymeval $%q% '$%q%))))
	  (cond (($ftest (simplify ($exact2 $a1 $a2 $a3)))
		 (return (trd-msymeval $%q% '$%q%))))
	  (cond ((like (setq $pt (simplify ($pttest $ap)))
		       nil)
		 (go $end)))
	  (cond (($ftest (simplify ($euler2 $ap $aq)))
		 (return (trd-msymeval $%q% '$%q%))))
	  (cond (($ftest (simplify ($bessel2 $ap $aq)))
		 (return (trd-msymeval $%q% '$%q%))))
	  $end (return (cond (($ftest (simplify ($xcc2 $ap $aq)))
			      (return (trd-msymeval $%q% '$%q%)))
			     (t (return nil))))))
     '$ap
     '$aq
     '$pt)))
(eval-when (compile eval load)
  (defprop $cc2 t translated)
  (add2lnc '$cc2 $props)
  (defmtrfun
      ($cc2 $any mdefine nil nil)
      ($%f% $%g% $y $x)
    nil
    ((lambda
	 ($a $sign $radexpand $alpha $zero $pos $ynew $%k1 $%k2)
       nil
       (prog
	   nil
	  (cond ((not (and ($freeof (trd-msymeval $x '$x)
				    (trd-msymeval $y '$y)
				    (trd-msymeval $%f% '$%f%))
			   ($freeof (trd-msymeval $x '$x)
				    (trd-msymeval $y '$y)
				    (trd-msymeval $%g% '$%g%))))
		 (return nil)))
	  (setq $method '$constcoeff)
	  (setq $a (add* (power (trd-msymeval $%f% '$%f%) 2)
			 (*mminus (mul* 4 (trd-msymeval $%g% '$%g%)))))
	  (cond (($freeof '$%i $a)
		 (setq $sign (simplify ($asksign $a))))
		(t
		 (setq $radexpand t)
		 (setq $sign '$pnz)))
	  (cond
	    ((like $sign $zero)
	     (return
	       (simplify
		(list
		 '(mequal)
		 (trd-msymeval $y '$y)
		 (mul*
		  (simplify ($exp (div (mul* (*mminus (trd-msymeval $%f%
								    '$%f%))
					     (trd-msymeval $x '$x))
				       2)))
		  (add* $%k1 (mul* $%k2 (trd-msymeval $x '$x)))))))))
	  (cond
	    ((like $sign $pos)
	     (return
	       (simplify
		(list
		 '(mequal)
		 (trd-msymeval $y '$y)
		 (add*
		  (mul*
		   $%k1
		   (simplify
		    ($exp (div (mul* (add* (*mminus (trd-msymeval $%f%
								  '$%f%))
					   (simplify (list '(%sqrt) $a)))
				     (trd-msymeval $x '$x))
			       2))))
		  (mul*
		   $%k2
		   (simplify
		    ($exp
		     (div (mul* (add* (*mminus (trd-msymeval $%f% '$%f%))
				      (*mminus (simplify (list '(%sqrt)
							       $a))))
				(trd-msymeval $x '$x))
			  2))))))))))
	  (setq $a (*mminus $a))
	  (setq $alpha (div (mul* (trd-msymeval $x '$x)
				  (simplify (list '(%sqrt) $a)))
			    2))
	  (cond
	    ((like (trd-msymeval $exponentialize nil) nil)
	     (return
	       (simplify
		(list
		 '(mequal)
		 (trd-msymeval $y '$y)
		 (mul*
		  (simplify ($exp (div (mul* (*mminus (trd-msymeval $%f%
								    '$%f%))
					     (trd-msymeval $x '$x))
				       2)))
		  (add* (mul* $%k1 (simplify (list '(%sin) $alpha)))
			(mul* $%k2 (simplify (list '(%cos) $alpha))))))))))
	  (return
	    (simplify
	     (list
	      '(mequal)
	      (trd-msymeval $y '$y)
	      (mul* (simplify ($exp (div (mul* (*mminus (trd-msymeval $%f%
								      '$%f%))
					       (trd-msymeval $x '$x))
					 2)))
		    (add* (mul* $%k1 (simplify ($exp (mul* '$%i $alpha))))
			  (mul* $%k2
				(simplify ($exp (mul* (*mminus '$%i)
						      $alpha)))))))))))
     '$a
     '$sign
     '$all
     '$alpha
     '$zero
     '$pos
     '$ynew
     '$%k1
     '$%k2)))
(eval-when (compile eval load)
  (defprop $exact2 t translated)
  (add2lnc '$exact2 $props)
  (defmtrfun
      ($exact2 $any mdefine nil nil)
      ($a1 $a2 $a3)
    nil
    ((lambda
	 ($b1 $%k1 $%k2)
       nil
       (prog
	   nil
	  (cond
	    ((like
	      (simplify
	       ($ratsimp (add* (simplify ($diff $a1 (trd-msymeval $x '$x) 2))
			       (*mminus (simplify ($diff $a2
							 (trd-msymeval $x '$x))))
			       $a3)))
	      0)
	     (setq
	      $b1
	      (simplify
	       ($exp
		(*mminus
		 (simplify
		  ($integrate
		   (simplify
		    ($ratsimp
		     (div
		      (add*
		       $a2
		       (*mminus (simplify ($diff $a1
						 (trd-msymeval $x '$x)))))
		      $a1)))
		   (trd-msymeval $x '$x))))))))
	    (t (return nil)))
	  (setq $method '$exact)
	  (return
	    (simplify
	     (list '(mequal)
		   (trd-msymeval $y '$y)
		   (add* (mul* $%k1
			       $b1
			       (simplify ($integrate (div 1 (mul* $a1 $b1))
						     (trd-msymeval $x '$x))))
			 (mul* $%k2 $b1)))))))
     '$b1
     '$%k1
     '$%k2)))
(eval-when (compile eval load)
  (defprop $xcc2 t translated)
  (add2lnc '$xcc2 $props)
  (defmtrfun
      ($xcc2 $any mdefine nil nil)
      ($ap $aq)
    nil
    ((lambda
	 ($d $b1 $z $radexpand)
       nil
       (prog
	   nil
	  (cond ((like $aq 0)
		 (return nil)))
	  (setq
	   $d
	   (simplify ($ratsimp (div (add* (simplify ($diff $aq
							   (trd-msymeval $x
									 '$x)))
					  (mul* 2 $ap $aq))
				    (mul* 2 (power $aq (rremainder 3 2)))))))
	  (cond (($freeof (trd-msymeval $x '$x) (trd-msymeval $y '$y) $d)
		 (setq $b1 (simplify ($cc2 $d 1 (trd-msymeval $y '$y) $z))))
		(t (return nil)))
	  (setq $method '$xformtoconstcoeff)
	  (return
	    (simplify
	     ($substitute (simplify ($integrate (simplify (list '(%sqrt) $aq))
						(trd-msymeval $x '$x)))
			  $z
			  $b1)))))
     '$d
     '$b1
     '$z
     '$all)))
(eval-when (compile eval load)
  (defprop $varp t translated)
  (add2lnc '$varp $props)
  (defmtrfun
      ($varp $any mdefine nil nil)
      ($soln $%g%)
    nil
    ((lambda
	 ($y1 $y2 $y3 $y4 $wr $heuristic $%k1 $%k2)
       nil
       (prog
	   nil
	  (setq
	   $y1
	   (simplify
	    ($ratsimp (simplify ($substitute (list '(mlist)
						   (simplify (list '(mequal)
								   $%k1
								   1))
						   (simplify (list '(mequal)
								   $%k2
								   0)))
					     ($rhs $soln))))))
	  (setq
	   $y2
	   (simplify
	    ($ratsimp (simplify ($substitute (list '(mlist)
						   (simplify (list '(mequal)
								   $%k1
								   0))
						   (simplify (list '(mequal)
								   $%k2
								   1)))
					     ($rhs $soln))))))
	  (setq $wr (add* (mul* $y1 (simplify ($diff $y2 (trd-msymeval $x '$x))))
			  (*mminus (mul* $y2
					 (simplify ($diff $y1
							  (trd-msymeval $x
									'$x)))))))
	  (cond ((like $wr 0)
		 (return nil)))
	  (cond ((and (like (trd-msymeval $method '$method) '$constcoeff)
		      (not ($freeof '%sin $wr))
		      (not ($freeof '%cos $wr)))
		 (setq $heuristic t)
		 (setq $wr (simplify ($ratsimp (simplify ($trigreduce $wr)))))))
	  (setq $y3 (simplify ($ratsimp (div (mul* $y1
						   (trd-msymeval $%g% '$%g%))
					     $wr))))
	  (setq $y4 (simplify ($ratsimp (div (mul* $y2
						   (trd-msymeval $%g% '$%g%))
					     $wr))))
	  (setq
	   $yp
	   (simplify
	    ($ratsimp
	     (add* (mul* $y2 (simplify ($integrate $y3 (trd-msymeval $x '$x))))
		   (*mminus (mul* $y1
				  (simplify ($integrate $y4
							(trd-msymeval $x
								      '$x)))))))))
	  (cond
	    ((like $heuristic t)
	     (setq
	      $yp
	      (simplify ($ratsimp (simplify ($trigreduce (trd-msymeval $yp
								       '$yp))))))))
	  (setq $method '$variationofparameters)
	  (return (simplify (list '(mequal)
				  (trd-msymeval $y '$y)
				  (add* ($rhs $soln) (trd-msymeval $yp '$yp)))))))
     '$y1
     '$y2
     '$y3
     '$y4
     '$wr
     nil
     '$%k1
     '$%k2)))
(eval-when (compile eval load)
  (defprop $reduce t translated)
  (add2lnc '$reduce $props)
  (defmtrfun
      ($reduce $any mdefine nil nil)
      ($eq)
    nil
    ((lambda
	 ($b1 $qq)
       nil
       (prog
	   nil
	  (setq
	   $b1
	   (simplify
	    ($substitute
	     (list '(mlist)
		   (simplify (list '(mequal)
				   (simplify `((%derivative) ,
					       (trd-msymeval $y '$y) ,
					       (trd-msymeval $x '$x) 2))
				   $qq))
		   (simplify (list '(mequal)
				   (simplify `((%derivative) ,
					       (trd-msymeval $y '$y) ,
					       (trd-msymeval $x '$x)))
				   $qq)))
	     $eq)))
	  (cond (($freeof (trd-msymeval $y '$y) $b1)
		 (return (simplify ($nlx $eq)))))
	  (return (cond (($freeof (trd-msymeval $x '$x) $b1)
			 (return (simplify ($nly $eq))))
			(t (return nil))))))
     '$b1
     '$qq)))
(eval-when (compile eval load)
  (defprop $nlx t translated)
  (add2lnc '$nlx $props)
  (defmtrfun
      ($nlx $any mdefine nil nil)
      ($eq)
    nil
    ((lambda
	 ($de $b $a1 $v $%k1 $%c)
       nil
       (prog
	   nil
	  (setq
	   $de
	   (simplify
	    ($substitute
	     (list '(mlist)
		   (simplify (list '(mequal)
				   (simplify `((%derivative) ,
					       (trd-msymeval $y '$y) ,
					       (trd-msymeval $x '$x) 2))
				   (simplify `((%derivative) ,$v ,
					       (trd-msymeval $x '$x)))))
		   (simplify (list '(mequal)
				   (simplify `((%derivative) ,
					       (trd-msymeval $y '$y) ,
					       (trd-msymeval $x '$x)))
				   $v)))
	     $eq)))
	  (cond ((like (setq $b (simplify ($ode1a $de $v (trd-msymeval $x '$x))))
		       nil)
		 (return nil)))
	  (setq
	   $a1
	   (simplify
	    ($substitute
	     (list '(mlist)
		   (simplify (list '(mequal)
				   $v
				   (simplify `((%derivative) ,
					       (trd-msymeval $y '$y) ,
					       (trd-msymeval $x '$x)))))
		   (simplify (list '(mequal) $%c $%k1)))
	     $b)))
	  (return
	    (cond (($ftest (simplify ($nlxy $a1
					    (simplify `((%derivative) ,
							(trd-msymeval $y '$y)
							,(trd-msymeval $x
								       '$x))))))
		   (setq $method '$freeofy)
		   (return (trd-msymeval $%q% '$%q%)))
		  (t (return nil))))))
     '$de
     '$b
     '$a1
     '$v
     '$%k1
     '$%c)))
(eval-when (compile eval load)
  (defprop $nly t translated)
  (add2lnc '$nly $props)
  (defmtrfun
      ($nly $any mdefine nil nil)
      ($eq)
    nil
    ((lambda
	 ($de $b $a1 $yz $v $%c $%k1)
       nil
       (prog
	   nil
	  (setq
	   $de
	   (simplify
	    ($substitute
	     (list '(mlist)
		   (simplify (list '(mequal)
				   (simplify `((%derivative) ,
					       (trd-msymeval $y '$y) ,
					       (trd-msymeval $x '$x) 2))
				   (mul* $v
					 (simplify `((%derivative) ,$v ,$yz)))))
		   (simplify (list '(mequal)
				   (simplify `((%derivative) ,
					       (trd-msymeval $y '$y) ,
					       (trd-msymeval $x '$x)))
				   $v))
		   (simplify (list '(mequal) (trd-msymeval $y '$y) $yz)))
	     $eq)))
	  (cond ((like (setq $b (simplify ($ode1a $de $v $yz)))
		       nil)
		 (return nil)))
	  (setq
	   $a1
	   (simplify
	    ($substitute
	     (list '(mlist)
		   (simplify (list '(mequal)
				   $v
				   (simplify `((%derivative) ,
					       (trd-msymeval $y '$y) ,
					       (trd-msymeval $x '$x)))))
		   (simplify (list '(mequal) $yz (trd-msymeval $y '$y)))
		   (simplify (list '(mequal) $%c $%k1)))
	     $b)))
	  (return
	    (cond (($ftest (simplify ($nlxy $a1
					    (simplify `((%derivative) ,
							(trd-msymeval $y '$y)
							,(trd-msymeval $x
								       '$x))))))
		   (setq $method '$freeofx)
		   (return (trd-msymeval $%q% '$%q%)))
		  (t (return nil))))))
     '$de
     '$b
     '$a1
     '$yz
     '$v
     '$%c
     '$%k1)))
(eval-when (compile eval load)
  (defprop $nlxy t translated)
  (add2lnc '$nlxy $props)
  (defmtrfun
      ($nlxy $any mdefine nil nil)
      ($eq $de)
    nil
    ((lambda
	 ($programmode $eq1 $%k2 $%c)
       nil
       (prog
	   nil
	  (setq $eq1 (simplify ($solve $eq $de)))
	  (setq
	   $eq1
	   (maplist_tr
	    (m-tlambda&env
	     (($zz) ($%k2 $%c))
	     nil
	     (cond (($ftest (simplify ($ode1a $zz
					      (trd-msymeval $y '$y)
					      (trd-msymeval $x '$x))))
		    (simplify ($substitute $%k2
					   $%c
					   (trd-msymeval $%q% '$%q%))))))
	    $eq1))
	  (return (cond ((eql ($length $eq1) 1)
			 (return (simplify ($first $eq1))))
			(t (return $eq1))))))
     t
     '$eq1
     '$%k2
     '$%c)))
(eval-when (compile eval load)
  (defprop $pttest t translated)
  (add2lnc '$pttest $props)
  (defmtrfun
      ($pttest $any mdefine nil nil)
      ($a)
    nil
    ((lambda ($a1 $a2 $a3)
       nil
       (prog nil
	  (cond ((like (setq $a1 (simplify ($ratsimp $a)))
		       0)
		 (return nil)))
	  (setq $a1 (simplify ($expand (div 1 $a1))))
	  (cond ((like (setq $a2 (simplify ($coeff $a1
						   (trd-msymeval $x '$x)
						   1)))
		       0)
		 (return nil)))
	  (cond ((not ($freeof (trd-msymeval $x '$x) $a2))
		 (return nil)))
	  (setq $a3 (simplify ($coeff $a1 (trd-msymeval $x '$x) 0)))
	  (return (cond ((not (like $a1
				    (add* (mul* $a2
						(trd-msymeval $x '$x))
					  $a3)))
			 (return nil))
			(t (return (div (*mminus $a3) $a2)))))))
     '$a1
     '$a2
     '$a3)))
(eval-when (compile eval load)
  (defprop $euler2 t translated)
  (add2lnc '$euler2 $props)
  (defmtrfun
      ($euler2 $any mdefine nil nil)
      ($a $b)
    nil
    ((lambda
	 ($dc $rp $ip $alpha $beta $sign $radexpand $%k1 $%k2 $pos $zero)
       nil
       (prog
	   nil
	  (cond
	    ((not
	      ($freeof
	       (trd-msymeval $x '$x)
	       (trd-msymeval $y '$y)
	       (setq
		$beta
		(simplify
		 ($ratsimp (mul* $b
				 (power (add* (trd-msymeval $x '$x)
					      (*mminus (trd-msymeval $pt
								     '$pt)))
					2)))))))
	     (return nil)))
	  (setq $method '$euler)
	  (setq $alpha (mul* $a
			     (add* (trd-msymeval $x '$x)
				   (*mminus (trd-msymeval $pt '$pt)))))
	  (setq $dc (simplify ($ratsimp (add* (power (add* $alpha -1) 2)
					      (*mminus (mul* 4 $beta))))))
	  (setq $rp (simplify ($ratsimp (div (*mminus (add* $alpha -1)) 2))))
	  (setq $sign (simplify ($asksign $dc)))
	  (cond
	    ((like $sign $zero)
	     (return
	       (simplify
		(list
		 '(mequal)
		 (trd-msymeval $y '$y)
		 (mul*
		  (power (add* (trd-msymeval $x '$x)
			       (*mminus (trd-msymeval $pt '$pt)))
			 $rp)
		  (add*
		   $%k1
		   (mul*
		    $%k2
		    (simplify (list '(%log)
				    (add* (trd-msymeval $x '$x)
					  (*mminus (trd-msymeval $pt
								 '$pt)))))))))))))
	  (cond
	    ((like $sign $pos)
	     (setq $ip (div (simplify (list '(%sqrt) $dc)) 2))
	     (return
	       (simplify
		(list '(mequal)
		      (trd-msymeval $y '$y)
		      (add* (mul* $%k1
				  (power (add* (trd-msymeval $x '$x)
					       (*mminus (trd-msymeval $pt
								      '$pt)))
					 (add* $rp $ip)))
			    (mul* $%k2
				  (power (add* (trd-msymeval $x '$x)
					       (*mminus (trd-msymeval $pt
								      '$pt)))
					 (add* $rp (*mminus $ip))))))))))
	  (setq $dc (*mminus $dc))
	  (setq $ip (div (simplify (list '(%sqrt) $dc)) 2))
	  (return
	    (simplify
	     (list
	      '(mequal)
	      (trd-msymeval $y '$y)
	      (mul*
	       (power (add* (trd-msymeval $x '$x)
			    (*mminus (trd-msymeval $pt '$pt)))
		      $rp)
	       (add*
		(mul*
		 $%k1
		 (simplify
		  (list
		   '(%sin)
		   (mul*
		    $ip
		    (simplify (list '(%log)
				    (add* (trd-msymeval $x '$x)
					  (*mminus (trd-msymeval $pt
								 '$pt)))))))))
		(mul*
		 $%k2
		 (simplify
		  (list
		   '(%cos)
		   (mul*
		    $ip
		    (simplify
		     (list '(%log)
			   (add* (trd-msymeval $x '$x)
				 (*mminus (trd-msymeval $pt '$pt))))))))))))))))
     '$dc
     '$rp
     '$ip
     '$alpha
     '$beta
     '$sign
     nil
     '$%k1
     '$%k2
     '$pos
     '$zero)))
(eval-when (compile eval load)
  (defprop $bessel2 t translated)
  (add2lnc '$bessel2 $props)
  (defmtrfun
      ($bessel2 $any mdefine nil nil)
      ($a $b)
    nil
    ((lambda
	 ($nu $b1 $intp $radexpand $%k1 $%y $%k2 $%j)
       nil
       (prog
	   nil
	  (cond
	    ((not
	      ($freeof
	       (trd-msymeval $x '$x)
	       (trd-msymeval $y '$y)
	       (setq
		$b1
		(simplify
		 ($ratsimp (mul* (add* 1 (*mminus $b))
				 (power (add* (trd-msymeval $x '$x)
					      (*mminus (trd-msymeval $pt
								     '$pt)))
					2)))))))
	     (return nil)))
	  (cond
	    ((not
	      (like
	       (simplify ($ratsimp (mul* $a
					 (add* (trd-msymeval $x '$x)
					       (*mminus (trd-msymeval $pt
								      '$pt))))))
	       1))
	     (return nil)))
	  (setq $nu (simplify (list '(%sqrt) $b1)))
	  (setq $method '$bessel)
	  (cond
	    ((like $nu (rremainder 1 2))
	     (return
	       (simplify
		(list
		 '(mequal)
		 (trd-msymeval $y '$y)
		 (div
		  (add*
		   (mul*
		    $%k1
		    (simplify (list '(%sin)
				    (add* (trd-msymeval $x '$x)
					  (*mminus (trd-msymeval $pt '$pt))))))
		   (mul*
		    $%k2
		    (simplify (list '(%cos)
				    (add* (trd-msymeval $x '$x)
					  (*mminus (trd-msymeval $pt
								 '$pt)))))))
		  (simplify (list '(%sqrt)
				  (add* (trd-msymeval $x '$x)
					(*mminus (trd-msymeval $pt '$pt)))))))))))
	  (cond ((is-boole-check (simplify ($featurep $nu '$integer)))
		 (setq $intp '$y))
		(($numberp $nu)
		 (setq $intp '$n)))
	  $loop
	  (cond
	    ((not (or (like $intp '$y)
		      (like $intp '$n)))
	     (setq $intp (simplify ($readonly (make-mstring "Is")
					      $nu
					      (make-mstring "an integer?  Type Y or N."))))
	     (go $loop)))
	  (cond
	    ((like $intp '$y)
	     (return
	       (simplify
		(list
		 '(mequal)
		 (trd-msymeval $y '$y)
		 (add*
		  (mul*
		   $%k1
		   (simplify
		    (mapply (maref $%j $nu)
			    (list (add* (trd-msymeval $x '$x)
					(*mminus (trd-msymeval $pt '$pt))))
			    '(($%j array) $nu))))
		  (mul*
		   $%k2
		   (simplify
		    (mapply (maref $%y $nu)
			    (list (add* (trd-msymeval $x '$x)
					(*mminus (trd-msymeval $pt '$pt))))
			    '(($%y array) $nu))))))))))
	  (return
	    (simplify
	     (list
	      '(mequal)
	      (trd-msymeval $y '$y)
	      (add*
	       (mul*
		$%k1
		(simplify (mapply (maref $%j $nu)
				  (list (add* (trd-msymeval $x '$x)
					      (*mminus (trd-msymeval $pt
								     '$pt))))
				  '(($%j array) $nu))))
	       (mul*
		$%k2
		(simplify
		 (mapply (maref $%j (*mminus $nu))
			 (list (add* (trd-msymeval $x '$x)
				     (*mminus (trd-msymeval $pt '$pt))))
			 '(($%j array) ((mminus) $nu)))))))))))
     '$nu
     '$b1
     '$intp
     '$all
     '$%k1
     '$%y
     '$%k2
     '$%j)))
(eval-when (compile eval load)
  (defprop $ic1 t translated)
  (add2lnc '$ic1 $props)
  (defmtrfun
      ($ic1 $any mdefine nil nil)
      ($soln $xc $yc)
    nil
    ((lambda
	 ($%c)
       nil
       (progn
	 (simplify ($noteqn $xc))
	 (simplify ($noteqn $yc))
	 (simplify ($boundtest '$%c $%c))
	 (simplify
	  ($ratsimp
	   (simplify
	    ($substitute
	     (list
	      '(mlist)
	      (simplify
	       (list
		'(mequal)
		'$%c
		($rhs
		 (simplify
		  ($solve1 (simplify ($substitute (list '(mlist)
							$xc
							$yc)
						  $soln))
			   $%c))))))
	     $soln))))))
     '$%c)))
(eval-when (compile eval load)
  (defprop $bc2 t translated)
  (add2lnc '$bc2 $props)
  (defmtrfun
      ($bc2 $any mdefine nil nil)
      ($soln $xa $ya $xb $yb)
    nil
    ((lambda
	 ($programmode $backsubst $singsolve $temp $%k1 $%k2)
       nil
       (prog
	   nil
	  (simplify ($noteqn $xa))
	  (simplify ($noteqn $ya))
	  (simplify ($noteqn $xb))
	  (simplify ($noteqn $yb))
	  (simplify ($boundtest '$%k1 $%k1))
	  (simplify ($boundtest '$%k2 $%k2))
	  (setq
	   $temp
	   (maplist_tr
	    (m-tlambda&env (($zz) ($soln))
			   nil
			   (simplify ($substitute $zz $soln)))
	    (simplify ($solve (list '(mlist)
				    (simplify ($substitute (list '(mlist)
								 $xa
								 $ya)
							   $soln))
				    (simplify ($substitute (list '(mlist)
								 $xb
								 $yb)
							   $soln)))
			      (list '(mlist) $%k1 $%k2)))))
	  (return (cond ((eql ($length $temp) 1)
			 (return (simplify ($first $temp))))
			(t (return $temp))))))
     t
     t
     t
     '$temp
     '$%k1
     '$%k2)))
(eval-when (compile eval load)
  (defprop $ic2 t translated)
  (add2lnc '$ic2 $props)
  (defmtrfun
      ($ic2 $any mdefine nil nil)
      ($soln $xa $ya $dya)
    nil
    ((lambda
	 ($programmode $backsubst $singsolve $temp $%k2 $%k1)
       nil
       (prog
	   nil
	  (simplify ($noteqn $xa))
	  (simplify ($noteqn $ya))
	  (simplify ($noteqn $dya))
	  (simplify ($boundtest '$%k1 $%k1))
	  (simplify ($boundtest '$%k2 $%k2))
	  (setq $temp (add* ($lhs $soln) (*mminus ($rhs $soln))))
	  (setq
	   $temp
	   (maplist_tr
	    (m-tlambda&env (($zz) ($soln))
			   nil
			   (simplify ($substitute $zz $soln)))
	    (simplify
	     ($solve
	      (list
	       '(mlist)
	       (simplify ($substitute (list '(mlist) $xa $ya) $soln))
	       (simplify
		($substitute
		 (list '(mlist) $dya $xa)
		 (simplify
		  (list
		   '(mequal)
		   ($lhs $dya)
		   (div
		    (*mminus
		     (simplify
		      ($substitute 0
				   ($lhs $dya)
				   (simplify ($diff $temp ($lhs $xa))))))
		    (simplify ($diff $temp ($lhs $ya)))))))))
	      (list '(mlist) $%k1 $%k2)))))
	  (return (cond ((eql ($length $temp) 1)
			 (return (simplify ($first $temp))))
			(t (return $temp))))))
     t
     t
     t
     '$temp
     '$%k2
     '$%k1)))
(eval-when (compile eval load)
  (defprop $noteqn t translated)
  (add2lnc '$noteqn $props)
  (defmtrfun ($noteqn $any mdefine nil nil)
      ($x)
    nil
    (cond ((or ($atom (trd-msymeval $x '$x))
	       (not (like (simplify ($inpart (trd-msymeval $x '$x)
					     0))
			  '&=)))
	   (display-for-tr nil nil (trd-msymeval $x '$x))
	   (display-for-tr nil nil (make-mstring "Not an equation"))
	   (simplify ($error))))))
(eval-when (compile eval load)
  (defprop $boundtest t translated)
  (add2lnc '$boundtest $props)
  (defmtrfun ($boundtest $any mdefine nil nil)
      ($x $y)
    nil
    (cond ((not (like (trd-msymeval $x '$x)
		      (trd-msymeval $y '$y)))
	   (display-for-tr nil nil (trd-msymeval $x '$x))
	   (display-for-tr nil nil (make-mstring "Must not be bound"))
	   (simplify ($error))))))
(eval-when (compile eval load)
  (defprop $failure t translated)
  (add2lnc '$failure $props)
  (defmtrfun
      ($failure $boolean mdefine nil nil)
      ($msg $eq)
    nil
    ((lambda
	 ($ynew)
       nil
       (progn
	 (cond ((not (is-boole-check (status $feature &ode)))
		(display-for-tr t
				nil
				(simplify ($substitute (trd-msymeval $yold
								     '$yold)
						       $ynew
						       $eq)))
		(display-for-tr nil nil $msg)))
	 nil))
     '$ynew)))
(eval-when (load compile) (meval '(($remove) $x $special $y $special)))
(setq $msg1 (make-mstring "Not a proper differential equation"))
(setq $msg2 (make-mstring "First order equation not linear in y'"))
