;=============================================================================
;    (c) copyright 1988	 Kent State University  kent, ohio 44242 
;		all rights reserved.
;
; Authors:  Paul S. Wang, Barbara Gates
; Permission to use this work for any purpose is granted provided that
; the copyright notice, author and support credits above are retained.
;=============================================================================

(include-if (null (getd 'wrs)) convmac.l)

(declare (special *gentran-dir tempvartype* tempvarname* tempvarnum* genstmtno*
	genstmtincr* *symboltable* *instk* *stdin* *currin* *outstk*
	*stdout* *currout* *outchanl* *lispdefops* *lisparithexpops*
	*lisplogexpops* *lispstmtops* *lispstmtgpops*))
;;  -----------  ;;
;;  lsprat.l     ;;    lisp-to-ratfor translation module
;;  -----------  ;;

(put nil '*ratforname* ".false.")
(put 'or       '*ratforprecedence* 1)
(put 'and      '*ratforprecedence* 2)
(put 'not      '*ratforprecedence* 3)
(put 'equal    '*ratforprecedence* 4)
(put 'notequal '*ratforprecedence* 4)
(put 'greaterp '*ratforprecedence* 4)
(put 'geqp     '*ratforprecedence* 4)
(put 'lessp    '*ratforprecedence* 4)
(put 'leqp     '*ratforprecedence* 4)
(put 'plus     '*ratforprecedence* 5)
(put 'times    '*ratforprecedence* 6)
(put 'quotient '*ratforprecedence* 6)
(put 'minus    '*ratforprecedence* 7)
(put 'expt     '*ratforprecedence* 8)
(print "bar") (terpri)
(put 'or       '*ratforop* "||")
(put 'and      '*ratforop* '|&|)
(put 'not      '*ratforop* '|!|)
(put 'equal    '*ratforop* '|==|)
(put 'notequal '*ratforop* '|!=|)
(put 'greaterp '*ratforop* '|>|)
(put 'geqp     '*ratforop* '|>=|)
(print "foo")
(put 'lessp    '*ratforop* '|<|)
(put 'leqp     '*ratforop* '|<=|)
(put 'plus     '*ratforop* '|+|)
(put 'times    '*ratforop* '|*|)
(put 'quotient '*ratforop* '|//|)
(put 'expt     '*ratforop* '|**|)
(put 'minus    '*ratforop* '|-|)

;;                                        ;;
;;  lisp-to-ratfor translation functions  ;;
;;                                        ;;


;;  control function  ;;

(de ratcode (forms)
  (foreach f in forms conc
	   (cond ((atom f)
		  (cond ((equal f '$begin_group)
			 (mkfratbegingp))
                        ((equal f '$end_group)
			 (mkfratendgp))
                        (t
			 (ratexp f))))
		 ((or (lispstmtp f) (lispstmtgpp f))
		  (cond (*gendecs
			 (prog (r)
			       (setq r (append (ratdecs (symtabget '*main*
								   '*decs*))
					       (ratstmt f)))
			       (symtabrem '*main* '*decs*)
			       (return r)))
			(t
			 (ratstmt f))))
		 ((lispdefp f)
		  (ratsubprog f))
		 (t
		  (ratexp f)))))

;;  subprogram translation  ;;

(de ratsubprog (def)
  (prog (type stype name params body lastst r)
	(setq name (cadr def))
	(setq body (cdddr def))
	(cond ((and body (equal body '(nil))) (setq body ())))
	(cond ((and (onep (length body))
		    (lispstmtgpp (car body)))
	       (progn
		(setq body (cdar body))
		(cond ((null (car body))
		       (setq body (cdr body)))))))
	(cond (body
	       (cond ((lispreturnp (setq lastst (car (reverse body))))
		      (setq body (aconc body '(end))))
		     ((not (lispendp lastst))
		      (setq body (append body (list '(return) '(end))))))))
	(cond ((setq type (symtabget name name))
	       (progn
		(setq type (cadr type))
		(symtabrem name name))))
	(setq stype (or (symtabget name '*type*)
			(cond ((or type (functionp body name))
			       'function)
			      (t
			       'subroutine))))
	(symtabrem name '*type*)
	(setq params (or (symtabget name '*params*) (caddr def)))
	(symtabrem name '*params*)
	(setq r (mkfratsubprogdec type stype name params))
	(cond (*gendecs
	       (setq r (append r (ratdecs (symtabget name '*decs*))))))
	(setq r (append r (foreach s in body conc (ratstmt s))))
	(cond (*gendecs
	       (progn
		(symtabrem name nil)
		(symtabrem name '*decs*))))
	(return r)))

;;  generation of declarations  ;;

(de ratdecs (decs)
  (foreach tl in (formtypelists decs) conc (mkfratdec (car tl) (cdr tl))))

;;  expression translation  ;;

(de ratexp (exp)
  (ratexp1 exp 0))

(de ratexp1 (exp wtin)
  (cond ((atom exp) (list (ratforname exp)))
	((eq (car exp) 'literal) (ratliteral exp))
	((onep (length exp)) exp)
	((memq (car exp) '(minus not))
	 (let* ((wt (ratforprecedence (car exp)))
		(res (cons (ratforop (car exp)) (ratexp1 (cadr exp) wt))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	((or (memq (car exp) *lisparithexpops*)
	     (memq (car exp) *lisplogexpops*))
	 (let* ((wt (ratforprecedence (car exp)))
		(op (ratforop (car exp)))
		(res (ratexp1 (cadr exp) wt))
		(res1))
	       (setq exp (cdr exp))
	       (cond ((eq op '+)
		      (while (setq exp (cdr exp))
                         (progn
			  (setq res1 (ratexp1 (car exp) wt))
			  (cond ((or (eq (car res1) '-)
				     (and (numberp (car res1))
					  (minusp (car res1))))
				 (setq res (append res res1)))
				(t
				 (setq res (append res (cons op res1))))))))
		     (t
		      (while (setq exp (cdr exp))
                         (setq res (append res
					   (cons op
						 (ratexp1 (car exp) wt)))))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	(t
	 (let ((res (cons (car exp) (cons '|(| (ratexp1 (cadr exp) 0)))))
              (setq exp (cdr exp))
	      (while (setq exp (cdr exp))
                 (setq res (append res (cons '|,| (ratexp1 (car exp) 0)))))
              (aconc res '|)| )))))

(de ratforname (name)
  (or (get name '*ratforname*) name))

(de ratforop (op)
  (or (get op '*ratforop*) op))

(de ratforprecedence (op)
  (or (get op '*ratforprecedence*) 9))

;;  statement translation  ;;

(de ratstmt (stmt)
  (cond ((null stmt) nil)
	((equal stmt '$begin_group) (mkfratbegingp))
	((equal stmt '$end_group) (mkfratendgp))
	((lisplabelp stmt) (ratstmtno stmt))
	((equal (car stmt) 'literal) (ratliteral stmt))
	((lispreadp stmt) (ratread stmt))
	((lispassignp stmt) (ratassign stmt))
	((lispprintp stmt) (ratwrite stmt))
	((lispcondp stmt) (ratif stmt))
	((lispbreakp stmt) (ratbreak stmt))
	((lispgop stmt) (ratgoto stmt))
	((lispreturnp stmt) (ratreturn stmt))
	((lispstopp stmt) (ratstop stmt))
	((lispendp stmt) (ratend stmt))
	((lispdop stmt) (ratloop stmt))
	((lispstmtgpp stmt) (ratstmtgp stmt))
	((lispdefp stmt) (ratsubprog stmt))
	((lispcallp stmt) (ratcall stmt))))

(de ratassign (stmt)
  (mkfratassign (cadr stmt) (caddr stmt)))

(de ratbreak (stmt)
  (mkfratbreak))

(de ratcall (stmt)
  (mkfratcall (car stmt) (cdr stmt)))

(de ratdo (var lo nextexp exitcond body)
  (prog (r hi incr)
	(setq hi
	      (car (delete1 'greaterp (delete1 'lessp (delete1 var exitcond)))))
	(setq incr (car (delete1 'plus (delete1 var nextexp))))
	(setq r (mkfratdo var lo hi incr))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (minus 1))
	(return r)))

(de ratend (stmt)
  (mkfratend))

(de ratforfor (var lo nextexp cond body)
  (prog (r)
	(cond (cond
	       (setq cond (list 'not cond))))
	(cond ((equal nextexp '(nil))
	       (setq r (mkfratfor var lo cond var nil)))
	      (nextexp
	       (setq r (mkfratfor var lo cond var nextexp)))
	      (t
	       (setq r (mkfratfor var lo cond nil nil))))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (minus 1))
	(return r)))

(de ratgoto (stmt)
  (prog (stmtno)
	(setq stmtno (or (get (cadr stmt) '*stmtno*)
			 (put (cadr stmt) '*stmtno* (genstmtno))))
	(return (mkfratgo stmtno))))

(de ratif (stmt)
  (prog (r st)
	(setq r (mkfratif (caadr stmt)))
	(indentratlevel (+ 1))
	(setq st (seqtogp (cdadr stmt)))
	(cond ((and (listp st)
		    (equal (car st) 'cond)
		    (equal (length st) 2))
	       (setq st (mkstmtgp 0 (list st)))))
	(setq r (append r (ratstmt st)))
	(indentratlevel (minus 1))
	(setq stmt (cdr stmt))
	(while (and (setq stmt (cdr stmt))
		    (neq (caar stmt) t))
	       (progn
		(setq r (append r (mkfratelseif (caar stmt))))
		(indentratlevel (+ 1))
		(setq st (seqtogp (cdar stmt)))
		(cond ((and (listp st)
			    (equal (car st) 'cond)
			    (equal (length st) 2))
		       (setq st (mkstmtgp 0 (list st)))))
		(setq r (append r (ratstmt st)))
		(indentratlevel (minus 1))))
	(cond (stmt
	       (progn
		(setq r (append r (mkfratelse)))
		(indentratlevel (+ 1))
		(setq st (seqtogp (cdar stmt)))
		(cond ((and (listp st)
			    (equal (car st) 'cond)
			    (equal (length st) 2))
		       (setq st (mkstmtgp 0 (list st)))))
		(setq r (append r (ratstmt st)))
		(indentratlevel (minus 1)))))
	(return r)))

(de ratliteral (stmt)
  (mkfratliteral (cdr stmt)))

(de ratloop (stmt)
  (prog (var lo nextexp exitcond body r)
	(cond ((complexdop stmt)
	       (return (ratstmt (seqtogp (simplifydo stmt))))))
	(cond ((setq var (cadr stmt))
	       (progn
		(setq lo (cadar var))
		(cond ((equal (length (car var)) 3)
		       (setq nextexp (or (caddar var) (list 'nil)))))
		(setq var (caar var)))))
	(cond ((setq exitcond (caddr stmt))
	       (setq exitcond (car exitcond))))
	(setq body (seqtogp (cdddr stmt)))
	(cond ((and var
		    lo
		    (equal (car nextexp) 'plus)
		    (member var nextexp)
		    (member (car exitcond) '(greaterp lessp))
		    (member var exitcond))
	       (return (ratdo var lo nextexp exitcond body)))
	      ((and exitcond
		    (not var))
	       (return (ratwhile exitcond body)))
	      ((and var
		    (not lo)
		    (lisplogexpp nextexp)
		    (equal exitcond var))
	       (return (ratrepeat body nextexp)))
	      (t
	       (return (ratforfor var lo nextexp exitcond body))))))

(de ratread (stmt)
  (mkfratread (cadr stmt)))

(de ratrepeat (body exitcond)
  (prog (r)
	(setq r (mkfratrepeat))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (minus 1))
	(return (append r (mkfratuntil exitcond)))))

(de ratreturn (stmt)
  (mkfratreturn (cadr stmt)))

(de ratstmtgp (stmtgp)
  (prog (r)
	(cond ((equal (car stmtgp) 'progn)
	       (setq stmtgp (cdr stmtgp)))
	      (t
	       (setq stmtgp (cddr stmtgp))))
	(setq r (mkfratbegingp))
	(indentratlevel (+ 1))
	(setq r (append r (foreach stmt in stmtgp conc (ratstmt stmt))))
	(indentratlevel (minus 1))
	(return (append r (mkfratendgp)))))

(de ratstmtno (label)
  (prog (stmtno)
	(setq stmtno (or (get label '*stmtno*)
			 (put label '*stmtno* (genstmtno))))
	(return (mkfratcontinue stmtno))))

(de ratstop (stmt)
  (mkfratstop))

(de ratwhile (cond body)
  (prog (r)
	(cond (cond
	       (setq cond (list 'not cond))))
	(setq r (mkfratwhile cond))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (minus 1))
	(return r)))

(de ratwrite (stmt)
  (mkfratwrite (cdr stmt)))


;;                                    ;;
;;  ratfor code formatting functions  ;;
;;                                    ;;


;;  statement formatting  ;;

(de mkfratassign (lhs rhs)
  (append (append (cons (mkrattab) (ratexp lhs))
		  (cons '= (ratexp rhs)))
	  (list (mkterpri))))

(de mkfratbegingp ()
  (list (mkrattab) '{ (mkterpri)))

(de mkfratbreak ()
  (list (mkrattab) 'break (mkterpri)))

(de mkfratcall (fname params)
  (progn
   (cond (params
	  (setq params (append (append (list '|(|)
				       (foreach p in (insertcommas params)
						conc (ratexp p)))
			       (list '|)|)))))
   (append (append (list (mkrattab) 'call '| |)
		   (ratexp fname))
	   (append params
		   (list (mkterpri))))))

(de mkfratcontinue (stmtno)
  (list stmtno '| | (mkrattab) 'continue (mkterpri)))

(de mkfratdec (type varlist)
  (progn
   (setq type (or type 'dimension))
   (setq varlist (foreach v in (insertcommas varlist) conc (ratexp v)))
   (cond ((implicitp type)
	  (append (list (mkrattab) type '| |  '|(|)
                  (append varlist (list '|)| (mkterpri)))))
	 (t
	  (append (list (mkrattab) type '| | )
		  (aconc varlist (mkterpri)))))))

(de mkfratdo (var lo hi incr)
  (progn
   (cond ((onep incr)
	  (setq incr nil))
	 (incr
	  (setq incr (cons '|,| (ratexp incr)))))
   (append (append (append (list (mkrattab) 'do '| |)
			   (ratexp var))
		   (append (cons '|=| (ratexp lo))
			   (cons '|,| (ratexp hi))))
	   (append incr
		   (list (mkterpri))))))

(de mkfratelse ()
  (list (mkrattab) 'else (mkterpri)))

(de mkfratelseif (exp)
  (append (append (list (mkrattab) 'else '| | 'if '| | '|(|)
		  (ratexp exp))
	  (list '|)| (mkterpri))))

(de mkfratend ()
  (list (mkrattab) 'end (mkterpri)))

(de mkfratendgp ()
  (list (mkrattab) '} (mkterpri)))

(de mkfratfor (var1 lo cond var2 nextexp)
  (progn
   (cond (var1
	  (setq var1 (append (ratexp var1) (cons '= (ratexp lo))))))
   (cond (cond
	  (setq cond (ratexp cond))))
   (cond (var2
	  (setq var2 (append (ratexp var2) (cons '= (ratexp nextexp))))))
   (append (append (append (list (mkrattab) 'for '| |  '|(|)
			   var1)
		   (cons '|;| cond))
	   (append (cons '|;| var2)
		   (list '|)| (mkterpri))))))

(de mkfratgo (stmtno)
  (list (mkrattab) 'goto '| | stmtno (mkterpri)))

(de mkfratif (exp)
  (append (append (list (mkrattab) 'if '| |  '|(|)
		  (ratexp exp))
	  (list '|)| (mkterpri))))

(de mkfratliteral (args)
  (foreach a in args conc
	   (cond ((equal a '$tab) (list (mkrattab)))
		 ((equal a '$cr) (list (mkterpri)))
		 ((listp a) (ratexp a))
		 (t (list a)))))

(de mkfratread (var)
  (append (list (mkrattab) 'read '|(*,*)| '| | )
	  (append (ratexp var) (list (mkterpri)))))

(de mkfratrepeat ()
  (list (mkrattab) 'repeat (mkterpri)))

(de mkfratreturn (exp)
  (cond (exp
	 (append (append (list (mkrattab) 'return '|(|) (ratexp exp))
		 (list '|)| (mkterpri))))
	(t
	 (list (mkrattab) 'return (mkterpri)))))

(de mkfratstop ()
  (list (mkrattab) 'stop (mkterpri)))

(de mkfratsubprogdec (type stype name params)
  (progn
   (cond (params
	  (setq params (aconc (cons '|(|
				    (foreach p in (insertcommas params)
					     conc (ratexp p)))
			      '|)|))))
   (cond (type
	  (setq type (list (mkrattab) type '| |  stype '| | )))
	 (t
	  (setq type (list (mkrattab) stype '| | ))))
   (append (append type (ratexp name))
	   (aconc params (mkterpri)))))

(de mkfratuntil (logexp)
  (append (list (mkrattab) 'until '| |  '|(|)
	  (append (ratexp logexp) (list '|)| (mkterpri)))))

(de mkfratwhile (exp)
  (append (append (list (mkrattab) 'while '| |  '|(|)
		  (ratexp exp))
	  (list '|)| (mkterpri))))

(de mkfratwrite (arglist)
  (append (append (list (mkrattab) 'write '|(*,*)| '| | )
		  (foreach arg in (insertcommas arglist) conc (ratexp arg)))
	  (list (mkterpri))))

;;  indentation control  ;;

(de mkrattab ()
  (list 'rattab ratcurrind*))

(de indentratlevel (n)
  (setq ratcurrind* (+ ratcurrind* (* n tablen*))))
