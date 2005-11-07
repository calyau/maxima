;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 4/21/85 11:00:16
(in-package :maxima)

(eval-when (compile eval load)
  (eval-when (compile eval load)
    (defprop $dva t translated)
    (add2lnc '$dva $props)
    (defmtrfun ($dva $any mdefmacro nil nil)
	($var)
      nil
      (mbuildq-subst (list (cons '$var $var))
		     '(($define_variable) $var 
		       ((mquote) $var) $any)))))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $%n $any))
  (meval* '(($declare) $%n $special))
  nil
  (def-mtrvar $%n '$%n))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $%pw $any))
  (meval* '(($declare) $%pw $special))
  nil
  (def-mtrvar $%pw '$%pw))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $%f $any))
  (meval* '(($declare) $%f $special))
  nil
  (def-mtrvar $%f '$%f))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $%f1 $any))
  (meval* '(($declare) $%f1 $special))
  nil
  (def-mtrvar $%f1 '$%f1))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $l% $any))
  (meval* '(($declare) $l% $special))
  nil
  (def-mtrvar $l% '$l%))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $solvep $any))
  (meval* '(($declare) $solvep $special))
  nil
  (def-mtrvar $solvep '$solvep))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $%r $any))
  (meval* '(($declare) $%r $special))
  nil
  (def-mtrvar $%r '$%r))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $p $any))
  (meval* '(($declare) $p $special))
  nil
  (def-mtrvar $p '$p))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $%cf $any))
  (meval* '(($declare) $%cf $special))
  nil
  (def-mtrvar $%cf '$%cf)
  (proclaim '(special $%0 $%1 $%% $y $maperror $mapprint
					;$%2 $n $%n $%pw $p $%g ;thing the problem was errset.
	      ))
  )
(eval-when (compile eval load)
  (defprop $algebraicp t translated)
  (add2lnc '$algebraicp $props)
  (defmtrfun
      ($algebraicp $boolean mdefine nil nil)
      ($%1)
    nil
    ((lambda
	 nil
       ((lambda
	    (mcatch)
	  (prog2
	      nil
	      (catch
		  'mcatch
		(progn
		  (simplify
		   ($substitute
		    (simplify
		     (list
		      '(mequal)
		      '&^
		      (m-tlambda
		       ($%1 $%2)
		       nil
		       (cond
			 ((not ($integerp $%2))
			  ((lambda (x)
			     (cond ((null mcatch)
				    (displa x)
				    (merror "`throw' not within `catch'")))
			     (throw 'mcatch x))
			   t))))))
		    $%1))
		  nil))
	    (errlfun1 mcatch)))
	(cons bindlist loclist))))))
(eval-when (compile eval load)
  (defprop $hicoef t translated)
  (add2lnc '$hicoef $props)
  (defmtrfun ($hicoef $any mdefine nil nil)
      ($x $n)
    nil
    (progn (setq $x (simplify ($ratsimp $x $n)))
	   (simplify ($coeff $x $n (simplify ($hipow $x $n)))))))
(eval-when (compile eval load)
  (defprop $genpol t translated)
  (add2lnc '$genpol $props)
  (defmtrfun ($genpol $any mdefine nil nil)
      ($n)
    nil
    (cond ((is-boole-check (mlsp $n 0)) 0)
	  (t (add* (simplify ($concat '$% $n))
		   (mul* (trd-msymeval $%n '$%n)
			 (simplify ($genpol (add* $n -1)))))))))
(eval-when (compile eval load)
  (defprop $clist t translated)
  (add2lnc '$clist $props)
  (defmtrfun
      ($clist $any mdefine nil nil)
      ($p)
    nil
    (cond
      ((like 0 (trd-msymeval $p '$p)) '((mlist)))
      (t
       ($cons
	(simplify
	 ($ratdisrep (setq $%pw (simplify ($ratcoef (trd-msymeval $p '$p)
						    (trd-msymeval $%n '$%n)
						    0)))))
	(simplify ($clist (ratf (div (add* (trd-msymeval $p '$p)
					   (*mminus (trd-msymeval $%pw
								  '$%pw)))
				     (trd-msymeval $%n '$%n))))))))))
(eval-when (compile eval load)
  (defprop $unsum t translated)
  (add2lnc '$unsum $props)
  (defmtrfun
      ($unsum $any mdefine nil nil)
      ($%g $%n)
    nil
    (cond
      ((or ($atom $%g)
	   (not (like ($part $%g 0) '&+)))
       (simplify
	($factor
	 (mul*
	  (add*
	   (div
	    ($num $%g)
	    (simplify
	     ($substitute (add* (trd-msymeval $%n '$%n) -1)
			  (trd-msymeval $%n '$%n)
			  (simplify ($prodgunch ($num $%g)
						(trd-msymeval $%n '$%n)
						1)))))
	   (*mminus
	    (div
	     ($denom $%g)
	     (simplify
	      ($substitute (add* (trd-msymeval $%n '$%n) -1)
			   (trd-msymeval $%n '$%n)
			   (simplify ($prodgunch ($denom $%g)
						 (trd-msymeval $%n '$%n)
						 1)))))))
	  (div (simplify ($substitute (add* (trd-msymeval $%n '$%n) -1)
				      (trd-msymeval $%n '$%n)
				      ($num $%g)))
	       ($denom $%g))))))
      (t
       (simplify
	(map1 (getopr (m-tlambda ($x)
				 nil
				 (simplify ($unsum $x
						   (trd-msymeval $%n '$%n)))))
	      $%g))))))
(eval-when (compile eval load)
  (defprop $prodflip t translated)
  (add2lnc '$prodflip $props)
  (defmtrfun
      ($prodflip $any mdefine nil nil)
      ($%0)
    nil
    (simplify
     ($substitute
      (list
       '(mlist)
       (simplify (list '(mequal) (simplify ($nounify '$product)) '$product))
       (simplify
	(list '(mequal)
	      '$product
	      (m-tlambda ($%0 $%1 $% $%%)
			 nil
			 (div 1
			      (simplify ($produ (div 1 $%0)
						$%1
						(trd-msymeval $% '$%)
						(trd-msymeval $%% '$%%))))))))
      $%0))))
(eval-when (compile eval load)
  (defprop $prodgunch t translated)
  (add2lnc '$prodgunch $props)
  (defmtrfun
      ($prodgunch $any mdefine nil nil)
      ($%0 $%n $%2)
    nil
    (simplify
     ($substitute
      (list
       '(mlist)
       (simplify
	(list
	 '(mequal)
	 (simplify ($nounify '%sin))
	 (m-tlambda&env
	  (($%0) ($%2))
	  nil
	  (mul*
	   (simplify
	    (list '(%sin)
		  (simplify ($substitute (add* (trd-msymeval $%n '$%n)
					       $%2)
					 (trd-msymeval $%n '$%n)
					 $%0))))
	   ((lambda
		($trigexpand)
	      nil
	      (simplify
	       ($expand
		(div
		 (simplify (list '(%sin) $%0))
		 (simplify
		  (list
		   '(%sin)
		   (simplify ($substitute (add* (trd-msymeval $%n
							      '$%n)
						$%2)
					  (trd-msymeval $%n '$%n)
					  $%0))))))))
	    t)))))
       (simplify
	(list
	 '(mequal)
	 (simplify ($nounify '$product))
	 (m-tlambda&env
	  (($%0 $%1 $% $%3) ($%2))
	  nil
	  (div
	   (mul*
	    (simplify
	     ($funmake
	      (simplify ($nounify '$product))
	      (list '(mlist)
		    $%0
		    $%1
		    (simplify ($substitute (add* (trd-msymeval $%n '$%n)
						 $%2)
					   (trd-msymeval $%n '$%n)
					   (trd-msymeval $% '$%)))
		    (simplify ($substitute (add* (trd-msymeval $%n '$%n)
						 $%2)
					   (trd-msymeval $%n '$%n)
					   $%3)))))
	    (simplify
	     ($produ
	      $%0
	      $%1
	      (trd-msymeval $% '$%)
	      (add* (simplify ($substitute (add* (trd-msymeval $%n '$%n)
						 $%2)
					   (trd-msymeval $%n '$%n)
					   (trd-msymeval $% '$%)))
		    -1))))
	   (simplify
	    ($produ $%0
		    $%1
		    (add* $%3 1)
		    (simplify ($substitute (add* (trd-msymeval $%n '$%n)
						 $%2)
					   (trd-msymeval $%n '$%n)
					   $%3))))))))
       (simplify
	(list
	 '(mequal)
	 '%binomial
	 (m-tlambda&env
	  (($%0 $%1) ($%2))
	  nil
	  (mul*
	   (simplify ($substitute (add* (trd-msymeval $%n '$%n) $%2)
				  (trd-msymeval $%n '$%n)
				  (simplify `((%binomial) ,$%0 ,$%1))))
	   (simplify
	    ($produ
	     (div (add* $%1 '$%) (add* $%0 '$%))
	     '$%
	     1
	     (add* (simplify ($substitute (add* (trd-msymeval $%n '$%n)
						$%2)
					  (trd-msymeval $%n '$%n)
					  $%1))
		   (*mminus $%1))))
	   (simplify
	    ($produ
	     (div (add* (*mminus $%1) $%0 '$%)
		  (add* (simplify ($substitute (add* (trd-msymeval $%n
								   '$%n)
						     $%2)
					       (trd-msymeval $%n '$%n)
					       $%1))
			(*mminus $%1)
			$%0
			'$%))
	     '$%
	     1
	     (add* (simplify ($substitute (add* (trd-msymeval $%n '$%n)
						$%2)
					  (trd-msymeval $%n '$%n)
					  (add* $%0 (*mminus $%1))))
		   $%1
		   (*mminus $%0))))))))
       (simplify
	(list
	 '(mequal)
	 '$beta
	 (m-tlambda&env
	  (($%0 $%1) ($%2))
	  nil
	  (mul*
	   (simplify ($substitute (add* (trd-msymeval $%n '$%n) $%2)
				  (trd-msymeval $%n '$%n)
				  (simplify (list '($beta) $%0 $%1))))
	   (simplify
	    ($produ (div (add* $%0 $%1 '$%) (add* $%0 '$%))
		    '$%
		    0
		    (add* (mul* (simplify ($ratcoef $%0
						    (trd-msymeval $%n
								  '$%n)))
				$%2)
			  -1)))
	   (simplify
	    ($produ
	     (div (add* $%0
			$%1
			(mul* $%2
			      (simplify ($ratcoef $%0
						  (trd-msymeval $%n
								'$%n))))
			'$%)
		  (add* $%1 '$%))
	     '$%
	     0
	     (add* (mul* (simplify ($ratcoef $%1
					     (trd-msymeval $%n '$%n)))
			 $%2)
		   -1)))))))
       (simplify
	(list
	 '(mequal)
	 '&!
	 (m-tlambda&env
	  (($%0) ($%2))
	  nil
	  (div
	   (simplify `((mfactorial) ,
		       (simplify ($substitute (add* (trd-msymeval $%n
								  '$%n)
						    $%2)
				  (trd-msymeval $%n '$%n)
				  $%0))))
	   (simplify
	    ($produ
	     (add* $%0 '$%)
	     '$%
	     1
	     (add* (simplify ($substitute (add* (trd-msymeval $%n '$%n)
						$%2)
					  (trd-msymeval $%n '$%n)
					  $%0))
		   (*mminus $%0))))))))
       (simplify
	(list
	 '(mequal)
	 '%gamma
	 (m-tlambda&env
	  (($%0) ($%2))
	  nil
	  (div
	   (simplify `((%gamma) ,
		       (simplify ($substitute (add* (trd-msymeval $%n
								  '$%n)
						    $%2)
				  (trd-msymeval $%n '$%n)
				  $%0))))
	   (simplify
	    ($produ
	     (add* $%0 '$% -1)
	     '$%
	     1
	     (add* (simplify ($substitute (add* (trd-msymeval $%n '$%n)
						$%2)
					  (trd-msymeval $%n '$%n)
					  $%0))
		   (*mminus $%0)))))))))
      $%0))))
(eval-when (compile eval load)
  (defprop $produ t translated)
  (add2lnc '$produ $props)
  (defmtrfun
      ($produ $any mdefine nil nil)
      ($%0 $%1 $% $%3)
    nil
    ((lambda
	 ($x $y)
       nil
       (cond
	 ((not ($integerp $y))
	  (simplify ($funmake (simplify ($nounify '$product))
			      (list '(mlist)
				    $%0
				    $%1
				    (trd-msymeval $% '$%)
				    $%3))))
	 ((is-boole-check (mlsp $y -1))
	  (div 1
	       (simplify ($produ $%0
				 $%1
				 (add* $%3 1)
				 (add* (trd-msymeval $% '$%) -1)))))
	 (t (do (($i 0 (f+ 1 $i)))
		((> $i $y) '$done)
	      (setq $x (mul* $x
			     (simplify ($substitute (add* $i
							  (trd-msymeval $%
									'$%))
						    $%1
						    $%0)))))
	    $x)))
     1
     (simplify ($ratsimp (add* $%3 (*mminus (trd-msymeval $% '$%))))))))
#+nil
(eval-when (compile eval load)
  (defprop $nusum t translated)
  (add2lnc '$nusum $props)
  (defmtrfun ($nusum nil mdefine nil nil)
      ($%a $%n $%l $%h)
    nil
    ((lambda ($mapprint $programmode $solvenullwarn)
       nil
       (maref 'mqapply
	      (simplify ($nusuml $%a
				 (trd-msymeval $%n '$%n)
				 $%l
				 $%h
				 '((mlist))))
	      1))
     nil
     t
     nil)))
(eval-when (compile eval load)
  (defprop $nusum t translated)
  (add2lnc '$nusum $props)
  (defmtrfun ($nusum $any mdefine nil nil)
      ($%a $%n $%l $%h)
    nil
    ((lambda ($mapprint $programmode $solvenullwarn)
       nil
       (simplify ($first (simplify ($nusuml $%a
					    (trd-msymeval $%n
							  '$%n)
					    $%l
					    $%h
					    '((mlist)))))))
     nil
     t
     nil)))
(eval-when (compile eval load)
  (defprop $funcsolve t translated)
  (add2lnc '$funcsolve $props)
  (defmtrfun ($funcsolve nil mdefine nil nil)
      ($%a $%f)
    nil
    ((lambda ($mapprint $programmode $solvenullwarn)
       nil
       (maref 'mqapply
	      (simplify ($funcsol $%a
				  (trd-msymeval $%f '$%f)
				  '((mlist))))
	      1))
     nil
     t
     nil)))
(eval-when (compile eval load)
  (defprop $dimsum t translated)
  (add2lnc '$dimsum $props)
  (defmtrfun
      ($dimsum $any mdefine nil nil)
      ($%cl)
    nil
    ((lambda
	 (|tr-gensym~128| |tr-gensym~129| |tr-gensym~130| |tr-gensym~131|)
       (unwind-protect
	    (progn
	      (msetchk '$ratfac |tr-gensym~128|)
	      ((lambda
		   ($ratfac $%cd $%pt $%pw)
		 nil
		 (setq
		  $%cd
		  (simplify
		   (map1
		    (getopr
		     (m-tlambda
		      ($x)
		      nil
		      (simplify ($hipow (add* (simplify ($ratsimp $x))
					      (div 1 (trd-msymeval $%n '$%n)))
					(trd-msymeval $%n '$%n)))))
		    (list '(mlist)
			  (add* (maref $%cl 1) (maref $%cl 2))
			  (add* (maref $%cl 1) (*mminus (maref $%cl 2)))
			  (maref $%cl 3)))))
		 (maset (max (maref $%cd 1) (f+ (maref $%cd 2) -1)) $%cd 1)
		 (simplify
		  ($inpart
		   (simplify
		    ($substitute
		     (setq
		      $solvep
		      (simplify
		       ($solve
			(simplify
			 ($clist
			  (simplify
			   ($substitute
			    (setq
			     $%pt
			     (simplify
			      ($funmake
			       'lambda
			       (list
				'(mlist)
				(list '(mlist) (trd-msymeval $%n '$%n))
				(simplify
				 ($genpol
				  (cond
				    ((and
				      (is-boole-check
				       (mlsp (maref $%cd 1)
					     (maref $%cd 2)))
				      ($integerp
				       (setq
					$%pw
					(simplify
					 ($substitute
					  (simplify
					   ($solve
					    (simplify
					     ($ratcoef
					      (add*
					       (mul*
						(maref $%cl 1)
						(add*
						 (trd-msymeval
						  $%n
						  '$%n)
						 '$%))
					       (mul*
						(maref $%cl 2)
						(trd-msymeval
						 $%n
						 '$%n)))
					      (trd-msymeval $%n
							    '$%n)
					      (maref $%cd 2)))
					    '$%))
					  '$%)))))
				     (maximum
				      (list
				       (trd-msymeval $%pw '$%pw)
				       (add* (maref $%cd 3)
					     (*mminus (maref $%cd
							     1))))))
				    (t (add* (maref $%cd 3)
					     (*mminus (maref $%cd 1)))))))))))
			    (simplify ($inpart (trd-msymeval $%f '$%f) 0))
			    (ncmul2 $%cl
				    (list '(mlist)
					  (trd-msymeval $%f1 '$%f1)
					  (trd-msymeval $%f '$%f)
					  1))))))
			($append (cond (($listp (trd-msymeval $l% '$l%))
					(trd-msymeval $l% '$l%))
				       (t
					(setq $l% (list '(mlist)
							(trd-msymeval $l%
								      '$l%)))))
				 (simplify ($clist (simplify ($inpart $%pt
								      2))))))))
		     (progn
		       (setq
			$l%
			(simplify
			 (map1 (getopr '&=)
			       (trd-msymeval $l% '$l%)
			       (simplify ($substitute (trd-msymeval $solvep
								    '$solvep)
						      (trd-msymeval $l% '$l%))))))
		       $%pt)))
		   2)))
	       |tr-gensym~128|
	       |tr-gensym~129|
	       |tr-gensym~130|
	       |tr-gensym~131|))
	 (msetchk '$ratfac (trd-msymeval $ratfac nil))))
     nil
     '$%cd
     '$%pt
     '$%pw)))
(eval-when (compile eval load)
  (defprop $ratsolve t translated)
  (add2lnc '$ratsolve $props)
  (defmtrfun
      ($ratsolve $any mdefine nil nil)
      ($p $x)
    nil
    (simplify
     (mapply-tr
      '$append
      (maplist_tr
       (m-tlambda&env
	(($p) ($x))
	nil
	(cond
	  ((or (like (simplify ($hipow (trd-msymeval $p '$p) $x)) 1)
	       (like (simplify ($substitute 0 $x (trd-msymeval $p '$p))) 0))
	   (simplify ($solve (simplify ($substitute (m-tlambda ($x $y)
							       nil
							       $x)
						    '&^
						    (trd-msymeval $p '$p)))
			     $x)))
	  (t '((mlist)))))
       (mul* 2 (power (simplify ($factor (trd-msymeval $p '$p))) 2)))))))
(eval-when (compile eval load)
  (defprop $prodshift t translated)
  (add2lnc '$prodshift $props)
  (defmtrfun
      ($prodshift $any mdefine nil nil)
      ($%0 $%2)
    nil
    (simplify
     ($substitute
      (list
       '(mlist)
       (simplify (list '(mequal) (simplify ($nounify '$product)) '$product))
       (simplify
	(list
	 '(mequal)
	 '$product
	 (m-tlambda&env
	  (($%0 $%1 $% $%3) ($%2))
	  nil
	  (simplify ($produ (simplify ($substitute (add* $%1
							 (*mminus $%2))
						   $%1
						   $%0))
			    $%1
			    (add* (trd-msymeval $% '$%) $%2)
			    (add* $%3 $%2)))))))
      $%0))))
(eval-when (compile eval load)
  (defprop $rforn t translated)
  (add2lnc '$rforn $props)
  (defmtrfun
      ($rforn $any mdefine nil nil)
      ($%3)
    nil
    ((lambda
	 (|tr-gensym~132| |tr-gensym~133|)
       (unwind-protect
	    (progn
	      (msetchk '$ratfac |tr-gensym~133|)
	      ((lambda
		   ($y $ratfac)
		 nil
		 (setq $p (mul* (trd-msymeval $p '$p)
				(simplify ($produ $y
						  (trd-msymeval $%n '$%n)
						  (trd-msymeval $%n '$%n)
						  (add* (trd-msymeval $%n '$%n)
							$%3
							-1)))))
		 (setq
		  $%r
		  (simplify
		   ($ratsimp
		    (div (trd-msymeval $%r '$%r)
			 (list '(mlist)
			       (simplify ($substitute (add* (trd-msymeval $%n
									  '$%n)
							    $%3)
						      (trd-msymeval $%n '$%n)
						      $y))
			       $y))))))
	       |tr-gensym~132|
	       |tr-gensym~133|))
	 (msetchk '$ratfac (trd-msymeval $ratfac nil))))
     (simplify ($gcd (maref (trd-msymeval $%r '$%r) 2)
		     (simplify ($substitute (add* (trd-msymeval $%n '$%n)
						  (*mminus $%3))
					    (trd-msymeval $%n '$%n)
					    (maref (trd-msymeval $%r '$%r)
						   1)))))
     t)))
(eval-when (compile eval load)
  (defprop $rform t translated)
  (add2lnc '$rform $props)
  (defmtrfun
      ($rform $any mdefine nil nil)
      ($%r)
    nil
    (cond
      ((is-boole-check (simplify (ratp (div (maref (trd-msymeval $%r '$%r) 1)
					    (maref (trd-msymeval $%r '$%r) 2))
				       (trd-msymeval $%n '$%n))))
       (cond (($algebraicp (trd-msymeval $%r '$%r))
	      (progn (msetchk '$gcd '$red)
		     (setq $gcd '$red))
	      (setq $algebraic t)))
       ((lambda
	    ($p)
	  nil
	  (simplify ($rforn 1))
	  (do
	   (($%3)
	    (mdo
	     (cdr
	      (simplify
	       ($ratsolve
		(simplify
		 ($resultant
		  (maref (trd-msymeval $%r '$%r) 1)
		  (simplify ($substitute (add* (trd-msymeval $%n '$%n) '$%)
					 (trd-msymeval $%n '$%n)
					 (maref (trd-msymeval $%r '$%r) 2)))
		  (trd-msymeval $%n '$%n)))
		'$%)))
	     (cdr mdo)))
	   ((null mdo) '$done)
	    (setq $%3 (car mdo))
	    (cond
	      ((and ($integerp (setq $%3 (simplify ($substitute (list '(mlist)
								      $%3)
								'$%))))
		    (is-boole-check (mgrp $%3 0)))
	       (simplify ($rforn $%3)))))
	  (list '(mlist)
		(trd-msymeval $p '$p)
		(div (maref (trd-msymeval $%r '$%r) 1)
		     (maref (trd-msymeval $%r '$%r) 2))))
	1))
      (t (simplify ($error (div (maref (trd-msymeval $%r '$%r) 1)
				(maref (trd-msymeval $%r '$%r) 2))
			   '|&NON-RATIONAL TERM RATIO TO NUSUM|))))))
(eval-when (compile eval load)
  (defprop $nusuml t translated)
  (add2lnc '$nusuml $props)
  (defmtrfun
      ($nusuml $any mdefine nil nil)
      ($%a $%n $%l $%h $l%)
    nil
    (cond
      ((like $%a 0) (list '(mlist) 0))
      (t
       ((lambda
	    (|tr-gensym~135| |tr-gensym~136|
	     |tr-gensym~137|
	     |tr-gensym~138|
	     |tr-gensym~139|
	     |tr-gensym~140|
	     |tr-gensym~141|
	     |tr-gensym~142|
	     |tr-gensym~143|
	     |tr-gensym~144|
	     |tr-gensym~145|
	     |tr-gensym~146|
	     |tr-gensym~147|)
	  (unwind-protect
	       (progn
		 (msetchk 'modulus |tr-gensym~136|)
		 (msetchk '$ratfac |tr-gensym~139|)
		 (msetchk '$gcd |tr-gensym~140|)
		 ((lambda
		      ($solvep modulus
		       $rv
		       $prodhack
		       $ratfac
		       $gcd
		       $algebraic
		       $ratalgdenom
		       $matrix_element_mult
		       $dispflag
		       $maperror
		       $%f
		       $%f1)
		    nil
		    (simplify ($ratvars (trd-msymeval $%n '$%n)))
		    (cond
		      ((and
			(not
			 (like
			  '((mlist))
			  ((lambda
			       (errcatch ret)
			     (cond
			       ((null
				 (setq
				  ret
				  (errset
				   (progn
				     (setq
				      $%cf
				      (simplify
				       ($dimsum
					(list
					 '(mlist)
					 (*mminus
					  ($num
					   (maref
					    'mqapply
					    (setq
					     $%r
					     (simplify
					      ($rform
					       ((lambda ($%a)
						  nil
						  (list '(mlist)
							($num $%a)
							($denom $%a)))
						(simplify
						 ($factor
						  (div
						   (simplify
						    ($substitute
						     (add*
						      (trd-msymeval
						       $%n
						       '$%n)
						      1)
						     (trd-msymeval
						      $%n
						      '$%n)
						     $%a))
						   (simplify
						    ($prodgunch
						     $%a
						     (trd-msymeval
						      $%n
						      '$%n)
						     1)))))))))
					    2)))
					 (simplify
					  ($substitute
					   (add* (trd-msymeval $%n '$%n) -1)
					   (trd-msymeval $%n '$%n)
					   ($denom
					    (maref (trd-msymeval $%r '$%r)
						   2))))
					 (maref (trd-msymeval $%r '$%r) 1))))))
				   lisperrprint)))
				(errlfun1 errcatch)))
			     (cons '(mlist) ret))
			   (cons bindlist loclist)
			   nil)))
			(not (like '((mlist)) (trd-msymeval $solvep '$solvep))))
		       ($cons
			(progn
			  (setq $%f (div (simplify ($prodgunch ($num $%a)
							       (trd-msymeval $%n
									     '$%n)
							       1))
					 ($denom $%a)))
			  (setq
			   $%f1
			   (simplify
			    ($ratsimp (simplify ($radcan (trd-msymeval $%cf
								       '$%cf))))))
			  (simplify (mapply-tr '$ratvars $rv))
			  (setq
			   $%f1
			   (simplify
			    ($substitute
			     (m-tlambda ($%0 $%1 $% $%3)
					nil
					(simplify ($produ $%0
							  $%1
							  (trd-msymeval $% '$%)
							  $%3)))
			     (simplify ($nounify '$product))
			     (add*
			      (simplify
			       ($factor
				(simplify
				 ($substitute
				  $%h
				  (trd-msymeval $%n '$%n)
				  (simplify
				   ($factor
				    (div
				     (mul*
				      ($num (maref (trd-msymeval $%r '$%r)
						   2))
				      (trd-msymeval $%f '$%f)
				      (simplify
				       ($substitute
					(add* (trd-msymeval $%n '$%n) 1)
					(trd-msymeval $%n '$%n)
					(trd-msymeval $%f1 '$%f1))))
				     (maref (trd-msymeval $%r '$%r) 1))))))))
			      (*mminus
			       (simplify
				($factor
				 (simplify
				  ($substitute
				   $%l
				   (trd-msymeval $%n '$%n)
				   (simplify
				    ($factor
				     (div
				      (mul*
				       $%a
				       (trd-msymeval $%f1 '$%f1)
				       (simplify
					($substitute
					 (add* (trd-msymeval $%n '$%n)
					       -1)
					 (trd-msymeval $%n '$%n)
					 ($denom
					  (maref (trd-msymeval $%r
							       '$%r)
						 2)))))
				      (maref (trd-msymeval $%r '$%r) 1)))))))))))))
			  (cond
			    ((is-boole-check (simplify (ratp $%a
							     (trd-msymeval $%n
									   '$%n))))
			     (simplify ($factor (trd-msymeval $%f1 '$%f1))))
			    (t (trd-msymeval $%f1 '$%f1))))
			(trd-msymeval $l% '$l%)))
		      (t (simplify (mapply-tr '$ratvars $rv))
			 (list '(mlist)
			       (simplify (mfuncall '$sum
						   $%a
						   (trd-msymeval $%n '$%n)
						   $%l
						   $%h))))))
		  |tr-gensym~135|
		  |tr-gensym~136|
		  |tr-gensym~137|
		  |tr-gensym~138|
		  |tr-gensym~139|
		  |tr-gensym~140|
		  |tr-gensym~141|
		  |tr-gensym~142|
		  |tr-gensym~143|
		  |tr-gensym~144|
		  |tr-gensym~145|
		  |tr-gensym~146|
		  |tr-gensym~147|))
	    (msetchk 'modulus (trd-msymeval modulus 'modulus))
	    (msetchk '$ratfac (trd-msymeval $ratfac nil))
	    (msetchk '$gcd (trd-msymeval $gcd '$gcd))))
	nil
	nil
	(trd-msymeval $ratvars '$ratvars)
	t
	t
	'$spmod
	nil
	t
	'&*
	nil
	nil
	(simplify ($funmake '$%f (list '(mlist) (trd-msymeval $%n '$%n))))
	(simplify ($funmake '$%f
			    (list '(mlist) (add* (trd-msymeval $%n '$%n) 1)))))))))
(eval-when (compile eval load)
  (defprop $funcsol t translated)
  (add2lnc '$funcsol $props)
  (defmtrfun
      ($funcsol $any mdefine nil nil)
      ($%a $%f $l%)
    nil
    ((lambda
	 (|tr-gensym~148| |tr-gensym~149|
	  |tr-gensym~150|
	  |tr-gensym~151|
	  |tr-gensym~152|
	  |tr-gensym~153|
	  |tr-gensym~154|
	  |tr-gensym~155|)
       (unwind-protect
	    (progn
	      (msetchk '$ratfac |tr-gensym~148|)
	      ((lambda
		   ($ratfac $maperror $linenum $dispflag $%f1 $%cl $%cm $%n)
		 nil
		 (setq
		  $%f1
		  (simplify
		   ($substitute (simplify (list '(mequal)
						(trd-msymeval $%n '$%n)
						(add* (trd-msymeval $%n '$%n) 1)))
				(trd-msymeval $%f '$%f))))
		 (setq
		  $%cl
		  (simplify
		   ($factor
		    (maref
		     'mqapply
		     (simplify
		      ($augcoefmatrix
		       (list
			'(mlist)
			(setq
			 $%a
			 ($num (simplify ($xthru (add* ($lhs $%a)
						       (*mminus ($rhs $%a))))))))
		       (list '(mlist)
			     (trd-msymeval $%f1 '$%f1)
			     (trd-msymeval $%f '$%f))))
		     1))))
		 (setq $%cm (simplify ($rform (simplify ($rest $%cl -1)))))
		 (maset
		  (simplify
		   ($ratsimp (div (simplify ($substitute (add* (trd-msymeval $%n
									     '$%n)
							       1)
							 (trd-msymeval $%n '$%n)
							 (maref $%cm 1)))
				  (maref $%cm 1))))
		  $%cm
		  2)
		 ($append
		  ((lambda
		       (errcatch ret)
		     (cond
		       ((null
			 (setq
			  ret
			  (errset
			   (progn
			     (simplify
			      (list
			       '(mequal)
			       (trd-msymeval $%f '$%f)
			       (simplify
				($factor
				 (div
				  (simplify
				   ($dimsum
				    (simplify
				     ($ratsimp
				      (list '(mlist)
					    (div (maref $%cl 1)
						 ($num (maref $%cm 2)))
					    (div (maref $%cl 2)
						 ($denom (maref $%cm 2)))
					    (div (mul* (maref $%cm 1)
						       (maref $%cl 3))
						 ($denom (maref $%cm 2))))))))
				  (maref $%cm 1)))))))
			   lisperrprint)))
			(errlfun1 errcatch)))
		     (cons '(mlist) ret))
		   (cons bindlist loclist)
		   nil)
		  (trd-msymeval $l% '$l%)))
	       |tr-gensym~148|
	       |tr-gensym~149|
	       |tr-gensym~150|
	       |tr-gensym~151|
	       |tr-gensym~152|
	       |tr-gensym~153|
	       |tr-gensym~154|
	       |tr-gensym~155|))
	 (msetchk '$ratfac (trd-msymeval $ratfac nil))))
     t
     nil
     (trd-msymeval $linenum '$linenum)
     nil
     '$%f1
     '$%cl
     '$%cm
     (simplify ($inpart (trd-msymeval $%f '$%f) 1)))))
