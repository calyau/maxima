;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;Translated on: 6/08/85 17:56:35;;Maxima System version 16
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
(eval-when (compile eval load)
  (meval* '(($modedeclare) $bestlength $fixnum))
  (meval* '(($declare) $bestlength $special))
  (defprop $bestlength assign-mode-check assign)
  (def-mtrvar $bestlength 0))
(eval-when (compile eval load)
  (meval* '(($modedeclare) $trylength $fixnum))
  (meval* '(($declare) $trylength $special))
  (defprop $trylength assign-mode-check assign)
  (def-mtrvar $trylength 0))
(eval-when (compile eval load)
  (proclaim '(special $ans ))

  (simplify ($put '%sin '%cos '$complement_function))
  (simplify ($put '%cos '%sin '$complement_function))
  (simplify ($put '%sinh '%cosh '$complement_function))
  (simplify ($put '%cosh '%sinh '$complement_function))
  (simplify ($put '%cos 1 '$unitcof))
  (simplify ($put '%sin 1 '$unitcof))
  (simplify ($put '%cosh 1 '$unitcof))
  (simplify ($put '%sinh -1 '$unitcof))
  (simplify ($put '%cos -1 '$complement_cof))
  (simplify ($put '%sin -1 '$complement_cof))
  (simplify ($put '%cosh 1 '$complement_cof))
  (simplify ($put '%sinh 1 '$complement_cof))
  (simplify ($put '%sin '$trigonometric '$type))
  (simplify ($put '%cos '$trigonometric '$type))
  (simplify ($put '%sinh '$hyper_trigonometric '$type))
  (simplify ($put '%cosh '$hyper_trigonometric '$type))
  )
nil
(eval-when (compile load eval) (meval* '(($declare) $list2 
					 $special)))
(eval-when (compile eval load)
  (defprop $trigonometricp t translated)
  (add2lnc '$trigonometricp $props)
  (defmtrfun ($trigonometricp $boolean mdefine nil nil)
      ($exp)
    nil
    (or (like (simplify ($get (simplify ($inpart $exp 0))
			      '$type))
	      '$trigonometric)
	(like (simplify ($get (trd-msymeval $piece '$piece)
			      '$type))
	      '$hyper_trigonometric))))
(eval-when (compile eval load)
  (defun $trigrule0
      (|tr-gensym~0|)
    (catch 'match
      (prog ($a |tr-gensym~1| |tr-gensym~2|)
	 (declare (special $a |tr-gensym~1| |tr-gensym~2|))
	 (cond ((not (equal (kar (kar |tr-gensym~0|))
			    '%tan))
		(matcherr)))
	 (setq |tr-gensym~1| (kdr |tr-gensym~0|))
	 (setq |tr-gensym~2| (kar |tr-gensym~1|))
	 (setq $a |tr-gensym~2|)
	 (cond ((nthkdr |tr-gensym~1| 1)
		(matcherr)))
	 (return (mul* (power (simplify (list '(%cos) $a))
			      -1)
		       (simplify (list '(%sin) $a)))))))
  (add2lnc '$trigrule0 $rules)
  (mdefprop $trigrule0
	    ((mequal) ((%tan simp) $a)
	     ((mtimes simp) ((mexpt simp) ((%cos simp) $a) -1)
	      ((%sin simp) $a)))
	    $rule)
  (mdefprop $trigrule0 $defrule $ruletype))
(eval-when (compile eval load)
  (defun $trigrule1
      (|tr-gensym~3|)
    (catch 'match
      (prog ($a |tr-gensym~4| |tr-gensym~5|)
	 (declare (special $a |tr-gensym~4| |tr-gensym~5|))
	 (cond ((not (equal (kar (kar |tr-gensym~3|))
			    '%tan))
		(matcherr)))
	 (setq |tr-gensym~4| (kdr |tr-gensym~3|))
	 (setq |tr-gensym~5| (kar |tr-gensym~4|))
	 (setq $a |tr-gensym~5|)
	 (cond ((nthkdr |tr-gensym~4| 1)
		(matcherr)))
	 (return (mul* (power (simplify (list '(%cos) $a))
			      -1)
		       (simplify (list '(%sin) $a)))))))
  (add2lnc '$trigrule1 $rules)
  (mdefprop $trigrule1
	    ((mequal) ((%tan simp) $a)
	     ((mtimes simp) ((mexpt simp) ((%cos simp) $a) -1)
	      ((%sin simp) $a)))
	    $rule)
  (mdefprop $trigrule1 $defrule $ruletype))
(eval-when (compile eval load)
  (defun $trigrule2
      (|tr-gensym~6|)
    (catch 'match
      (prog ($a |tr-gensym~7| |tr-gensym~8|)
	 (declare (special $a |tr-gensym~7| |tr-gensym~8|))
	 (cond ((not (equal (kar (kar |tr-gensym~6|))
			    '%sec))
		(matcherr)))
	 (setq |tr-gensym~7| (kdr |tr-gensym~6|))
	 (setq |tr-gensym~8| (kar |tr-gensym~7|))
	 (setq $a |tr-gensym~8|)
	 (cond ((nthkdr |tr-gensym~7| 1)
		(matcherr)))
	 (return (power (simplify (list '(%cos) $a)) -1)))))
  (add2lnc '$trigrule2 $rules)
  (mdefprop $trigrule2
	    ((mequal) ((%sec simp) $a) ((mexpt simp) ((%cos simp) $a) -1))
	    $rule)
  (mdefprop $trigrule2 $defrule $ruletype))
(eval-when (compile eval load)
  (defun $trigrule3
      (|tr-gensym~9|)
    (catch 'match
      (prog ($a |tr-gensym~10| |tr-gensym~11|)
	 (declare (special $a
			   |tr-gensym~10|
			   |tr-gensym~11|))
	 (cond ((not (equal (kar (kar |tr-gensym~9|))
			    '%csc))
		(matcherr)))
	 (setq |tr-gensym~10| (kdr |tr-gensym~9|))
	 (setq |tr-gensym~11| (kar |tr-gensym~10|))
	 (setq $a |tr-gensym~11|)
	 (cond ((nthkdr |tr-gensym~10| 1)
		(matcherr)))
	 (return (power (simplify (list '(%sin) $a)) -1)))))
  (add2lnc '$trigrule3 $rules)
  (mdefprop $trigrule3
	    ((mequal) ((%csc simp) $a) ((mexpt simp) ((%sin simp) $a) -1))
	    $rule)
  (mdefprop $trigrule3 $defrule $ruletype))
(eval-when (compile eval load)
  (defun $trigrule4
      (|tr-gensym~12|)
    (catch 'match
      (prog ($a |tr-gensym~13| |tr-gensym~14|)
	 (declare (special $a
			   |tr-gensym~13|
			   |tr-gensym~14|))
	 (cond ((not (equal (kar (kar |tr-gensym~12|))
			    '%cot))
		(matcherr)))
	 (setq |tr-gensym~13| (kdr |tr-gensym~12|))
	 (setq |tr-gensym~14| (kar |tr-gensym~13|))
	 (setq $a |tr-gensym~14|)
	 (cond ((nthkdr |tr-gensym~13| 1)
		(matcherr)))
	 (return (mul* (simplify (list '(%cos) $a))
		       (power (simplify (list '(%sin)
					      $a))
			      -1))))))
  (add2lnc '$trigrule4 $rules)
  (mdefprop $trigrule4
	    ((mequal) ((%cot simp) $a)
	     ((mtimes simp) ((%cos simp) $a)
	      ((mexpt simp) ((%sin simp) $a) -1)))
	    $rule)
  (mdefprop $trigrule4 $defrule $ruletype))
(eval-when (compile eval load)
  (defun $htrigrule1
      (|tr-gensym~15|)
    (catch 'match
      (prog ($a |tr-gensym~16| |tr-gensym~17|)
	 (declare (special $a
			   |tr-gensym~16|
			   |tr-gensym~17|))
	 (cond ((not (equal (kar (kar |tr-gensym~15|))
			    '%tanh))
		(matcherr)))
	 (setq |tr-gensym~16| (kdr |tr-gensym~15|))
	 (setq |tr-gensym~17| (kar |tr-gensym~16|))
	 (setq $a |tr-gensym~17|)
	 (cond ((nthkdr |tr-gensym~16| 1)
		(matcherr)))
	 (return (mul* (power (simplify (list '(%cosh)
					      $a))
			      -1)
		       (simplify (list '(%sinh) $a)))))))
  (add2lnc '$htrigrule1 $rules)
  (mdefprop $htrigrule1
	    ((mequal) ((%tanh simp) $a)
	     ((mtimes simp) ((mexpt simp) ((%cosh simp) $a) -1)
	      ((%sinh simp) $a)))
	    $rule)
  (mdefprop $htrigrule1 $defrule $ruletype))
(eval-when (compile eval load)
  (defun $htrigrule2
      (|tr-gensym~18|)
    (catch 'match
      (prog ($a |tr-gensym~19| |tr-gensym~20|)
	 (declare (special $a
			   |tr-gensym~19|
			   |tr-gensym~20|))
	 (cond ((not (equal (kar (kar |tr-gensym~18|))
			    '%sech))
		(matcherr)))
	 (setq |tr-gensym~19| (kdr |tr-gensym~18|))
	 (setq |tr-gensym~20| (kar |tr-gensym~19|))
	 (setq $a |tr-gensym~20|)
	 (cond ((nthkdr |tr-gensym~19| 1)
		(matcherr)))
	 (return (power (simplify (list '(%cosh) $a)) -1)))))
  (add2lnc '$htrigrule2 $rules)
  (mdefprop $htrigrule2
	    ((mequal) ((%sech simp) $a)
	     ((mexpt simp) ((%cosh simp) $a) -1))
	    $rule)
  (mdefprop $htrigrule2 $defrule $ruletype))
(eval-when (compile eval load)
  (defun $htrigrule3
      (|tr-gensym~21|)
    (catch 'match
      (prog ($a |tr-gensym~22| |tr-gensym~23|)
	 (declare (special $a
			   |tr-gensym~22|
			   |tr-gensym~23|))
	 (cond ((not (equal (kar (kar |tr-gensym~21|))
			    '%csch))
		(matcherr)))
	 (setq |tr-gensym~22| (kdr |tr-gensym~21|))
	 (setq |tr-gensym~23| (kar |tr-gensym~22|))
	 (setq $a |tr-gensym~23|)
	 (cond ((nthkdr |tr-gensym~22| 1)
		(matcherr)))
	 (return (power (simplify (list '(%sinh) $a)) -1)))))
  (add2lnc '$htrigrule3 $rules)
  (mdefprop $htrigrule3
	    ((mequal) ((%csch simp) $a)
	     ((mexpt simp) ((%sinh simp) $a) -1))
	    $rule)
  (mdefprop $htrigrule3 $defrule $ruletype))
(eval-when (compile eval load)
  (defun $htrigrule4
      (|tr-gensym~24|)
    (catch 'match
      (prog ($a |tr-gensym~25| |tr-gensym~26|)
	 (declare (special $a
			   |tr-gensym~25|
			   |tr-gensym~26|))
	 (cond ((not (equal (kar (kar |tr-gensym~24|))
			    '%coth))
		(matcherr)))
	 (setq |tr-gensym~25| (kdr |tr-gensym~24|))
	 (setq |tr-gensym~26| (kar |tr-gensym~25|))
	 (setq $a |tr-gensym~26|)
	 (cond ((nthkdr |tr-gensym~25| 1)
		(matcherr)))
	 (return (mul* (simplify (list '(%cosh) $a))
		       (power (simplify (list '(%sinh)
					      $a))
			      -1))))))
  (add2lnc '$htrigrule4 $rules)
  (mdefprop $htrigrule4
	    ((mequal) ((%coth simp) $a)
	     ((mtimes simp) ((%cosh simp) $a)
	      ((mexpt simp) ((%sinh simp) $a) -1)))
	    $rule)
  (mdefprop $htrigrule4 $defrule $ruletype))
(eval-when (compile eval load)
  (defprop $trigsimp t translated)
  (add2lnc '$trigsimp $props)
  (defmtrfun
      ($trigsimp $any mdefine nil nil)
      ($x)
    nil
    (simplify
     ($trigsimp3
      (simplify ($radcan (do ((|tr-gensym~27| $x
					      (apply1 |tr-gensym~27|
						      (car |tr-gensym~28|)
						      0))
			      (|tr-gensym~28| '($trigrule1 $trigrule2 
						$trigrule3 $trigrule4 
						$htrigrule1 $htrigrule2 
						$htrigrule3 $htrigrule4)
					      (cdr |tr-gensym~28|)))
			     ((null |tr-gensym~28|) |tr-gensym~27|)
			   )))))))
(eval-when (compile eval load)
  (defprop $trigsimp3 t translated)
  (add2lnc '$trigsimp3 $props)
  (defmtrfun
      ($trigsimp3 $any mdefine nil nil)
      ($expn)
    nil
    (progn (setq $expn (simplify ($totaldisrep $expn)))
	   (simplify ($ratsimp (div (simplify ($trigsimp1 ($num $expn)))
				    (simplify ($trigsimp1 ($denom $expn)))))))))
(eval-when (compile eval load)
  (defprop $trigsimp1 t translated)
  (add2lnc '$trigsimp1 $props)
  (defmtrfun ($trigsimp1 $any mdefine nil nil)
      ($expn)
    nil
    ((lambda ($listoftrigsq $bestlength $trylength)
       nil
       (assign-mode-check '$trylength $trylength)
       (assign-mode-check '$bestlength $bestlength)
       (setq $listoftrigsq (simplify ($listoftrigsq $expn)))
       (progn (assign-mode-check '$bestlength 999999)
	      (setq $bestlength 999999))
       (cond ((not (like $listoftrigsq '((mlist))))
	      (simplify ($improve $expn
				  $expn
				  $listoftrigsq)))
	     (t $expn)))
     '$listoftrigsq
     0
     0)))
(eval-when (compile eval load)
  (defprop $improve t translated)
  (add2lnc '$improve $props)
  (defmtrfun
      ($improve $any mdefine nil nil)
      ($expn $subsofar $listoftrigsq)
    nil
    (cond
      ((like $listoftrigsq '((mlist)))
       (cond ((< ((lambda (|tr-gensym~31|)
		    (progn (assign-mode-check '$trylength |tr-gensym~31|)
			   (setq $trylength |tr-gensym~31|)))
		  ($expnlength $subsofar))
		 (trd-msymeval $bestlength 0))
	      ((lambda (|tr-gensym~30|)
		 (progn (assign-mode-check '$bestlength |tr-gensym~30|)
			(setq $bestlength |tr-gensym~30|)))
	       (trd-msymeval $trylength 0))
	      $subsofar)
	     (t $expn)))
      (t
       (setq $subsofar (simplify ($improve $expn
					   $subsofar
					   (simplify ($rest $listoftrigsq)))))
       (do
	(($alt) (mdo (cdr (simplify ($first $listoftrigsq))) (cdr mdo)))
	((null mdo) '$done)
	 (setq $alt (car mdo))
	 (setq
	  $subsofar
	  (simplify
	   ($improve
	    $subsofar
	    (simplify
	     ($ratsubst
	      (add*
	       (simplify ($get (simplify ($inpart $alt 0)) '$unitcof))
	       (mul*
		(simplify ($get (trd-msymeval $piece '$piece)
				'$complement_cof))
		(power
		 (simplify (mapply (simplify ($get (trd-msymeval $piece
								 '$piece)
						   '$complement_function))
				   (list (simplify ($first $alt)))
				   '(($get) $piece 
				     ((mquote) $complement_function))))
		 2)))
	      (power $alt 2)
	      $subsofar))
	    (simplify ($rest $listoftrigsq))))))
       $subsofar))))
(eval-when (compile eval load)
  (defprop $listoftrigsq t translated)
  (add2lnc '$listoftrigsq $props)
  (defmtrfun
      ($listoftrigsq $any mdefine nil nil)
      ($expn)
    nil
    (cond
      (($atom $expn) '((mlist)))
      (t
       ((lambda
	    ($inflag $ans) 
	  nil
	  (prog
	      nil
	     (cond ((and (like (simplify ($inpart $expn 0)) '&^)
			 ($integerp (simplify ($inpart $expn 2)))
			 (not (is-boole-check (mlsp (trd-msymeval $piece
								  '$piece)
						    2))))
		    (cond (($atom (setq $expn (simplify ($inpart $expn 1))))
			   (return '((mlist))))
			  (($trigonometricp $expn)
			   (return (list '(mlist) (list '(mlist) $expn)))))))
	     (setq $inflag t)
	     (do
	      (($arg) (mdo (cdr $expn) (cdr mdo)))
	      ((null mdo) '$done)
	       (setq $arg (car mdo))
	       (setq
		$ans
		(simplify ($specialunion (simplify ($listoftrigsq $arg))
					 (trd-msymeval $ans '$ans)))))
	     (return (trd-msymeval $ans '$ans))))
	'$inflag
	'((mlist)))))))
(eval-when (compile eval load)
  (defprop $specialunion t translated)
  (add2lnc '$specialunion $props)
  (defmtrfun
      ($specialunion $any mdefine nil nil)
      ($list1 $list2)
    nil
    (cond
      ((like $list1 '((mlist))) (trd-msymeval $list2 '$list2))
      ((like (trd-msymeval $list2 '$list2) '((mlist))) $list1)
      (t
       ((lambda
	    ($alternates)
	  nil
	  (do
	   (($alt) (mdo (cdr $alternates) (cdr mdo)))
	   ((null mdo) '$done)
	    (setq $alt (car mdo))
	    (setq
	     $list2
	     (simplify ($update $alt
				(simplify ($get (simplify ($inpart $alt 0))
						'$complement_function))))))
	  (simplify ($specialunion (simplify ($rest $list1))
				   (trd-msymeval $list2 '$list2))))
	(simplify ($first $list1)))))))
(eval-when (compile eval load) 
  (defprop $update t translated)
  (add2lnc '$update $props)
  (defmtrfun
      ($update $any mdefine nil nil)
      ($form $complement)
    nil
    ((lambda
	 ($ans)
       (declare (special $ans))
       nil
       nil
       (setq $complement (simplify (mfuncall $complement
					     (simplify ($inpart $form 1)))))
       (setq
	$ans
	(do (($element)
	     (mdo (cdr (trd-msymeval $list2 '$list2)) (cdr mdo)))
	    ((null mdo) '$done)
	  (setq $element (car mdo))
	  (cond (($member $form $element)
		 (return '$found))
		(($member $complement $element)
		 (return ($cons (list '(mlist) $form $complement)
				(simplify ($delete $element
						   (trd-msymeval $list2
								 '$list2)))))))))
       (cond ((like (trd-msymeval $ans '$ans) '$found)
	      (trd-msymeval $list2 '$list2))
	     ((like (trd-msymeval $ans '$ans) '$done)
	      ($cons (list '(mlist) $form) (trd-msymeval $list2 '$list2)))
	     (t (trd-msymeval $ans '$ans))))
     '$ans)))
(eval-when (compile eval load) 
  (defprop $expnlength t translated)
  (add2lnc '$expnlength $props)
  (defmtrfun ($expnlength $fixnum mdefine nil nil)
      ($expr)
    nil
    ((lambda ($inflag)
       nil
       (cond (($atom $expr) 1)
	     (t (f+ 1
		    ($argslength (simplify ($args $expr)))))))
     t)))
(eval-when (compile eval load) 
  (defprop $argslength t translated)
  (add2lnc '$argslength $props)
  (defmtrfun ($argslength $any mdefine nil nil)
      ($args)
    nil
    (simplify (mapply-tr '&+
			 (simplify (map1 (getopr '$expnlength)
					 $args))))))