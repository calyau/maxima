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
;;  segmnt.l     ;;    segmentation module
;;  -----------  ;;

(declare (special *gentranopt *gentranlang maxexpprintlen*))

;;                           ;;
;; 1. segmentation routines  ;;
;;                           ;;


(de seg (forms)
  ; exp  --+-->  exp                                          ;
  ;        +-->  (assign    assign    ... assign      exp   ) ;
  ;                     (1)       (2)           (n-1)    (n)  ;
  ; stmt  --+-->  stmt                                        ;
  ;         +-->  stmtgp                                      ;
  ; stmtgp  ----->  stmtgp                                    ;
  ; def  ----->  def                                          ;
  (foreach f in forms collect
	   (cond ((lispexpp f)
		  (cond ((toolongexpp f)
			 (segexp f 'unknown))
			(t
			 f)))
		 ((lispstmtp f)
		  (segstmt f))
		 ((lispstmtgpp f)
		  (cond ((toolongstmtgpp f)
			 (seggroup f))
			(t
			 f)))
		 ((lispdefp f)
		  (cond ((toolongdefp f)
			 (segdef f))
			(t
			 f)))
		 (t
		  f))))

(de segexp (exp type)
  ; exp  -->  (assign    assign    ... assign      exp   ) ;
  ;                  (1)       (2)           (n-1)    (n)  ;
  (reverse (segexp1 exp type)))

(de segexp1 (exp type)
  ; exp  -->  (exp    assign      assign      ... assign   ) ;
  ;               (n)       (n-1)       (n-2)           (1)  ;
  (prog (res tempvarname)
	(setq tempvarname tempvarname*)
	(cond (*gentranopt
	       (setq tempvarname (stripdollar1 tempvarname))
	       (setq tempvarname* (explode2 tempvarname))
	       (setq tempvarname* (compress (cons (car tempvarname*)
						  tempvarname*)))))
	(setq res (segexp2 exp type))
	(recurunmark res)
	(setq tempvarname* tempvarname)
	(cond ((equal (car res) (cadadr res))
	       (progn
		(setq res (cdr res))
		(rplaca res (caddar res)))))
	(return res)))

(de segexp2 (exp type)
  ; exp  -->  (exp    assign      assign      ... assign   ) ;
  ;               (n)       (n-1)       (n-2)           (1)  ;
  (prog (expn assigns newassigns unops op termlist var tmp)
	(setq expn exp)
	(while (equal (length expn) 2)
	       (progn
		(setq unops (cons (car expn) unops))
		(setq expn (cadr expn))))
	(setq op (car expn))
	(foreach term in (cdr expn) do
		 (progn
		  (cond ((toolongexpp term)
			 (progn
			  (setq tmp (segexp2 term type))
			  (setq term (car tmp))
			  (setq newassigns (cdr tmp))))
			(t
			 (setq newassigns 'nil)))
		  (cond ((and (toolongexpp (cons op (cons term termlist)))
			      termlist
			      (or (> (length termlist) 1)
				  (listp (car termlist))))
			 (progn
			  (recurunmark termlist)
			  (setq var (or var (tempvar type)))
			  (markvar var)
			  (setq assigns
				(cons (mkassign var
						(cond ((onep (length termlist))
						       (car termlist))
						      (t
						       (cons op termlist))))
				      assigns))
			  (setq termlist (list var term))))
			(t
			 (setq termlist (aconc termlist term))))
		  (setq assigns (append newassigns assigns))))
	(setq expn (cond ((onep (length termlist))
			  (car termlist))
			 (t
			  (cons op termlist))))
	(while unops
	       (progn
		(setq expn (list (car unops) expn))
		(setq unops (cdr unops))))
	(cond ((equal expn exp)
	       (progn
		(recurunmark expn)
		(setq var (or var (tempvar type)))
		(markvar var)
		(setq assigns (list (mkassign var expn)))
		(setq expn var))))
	(return (cons expn assigns))))

(de segstmt (stmt)
  ; assign  --+-->  assign ;
  ;           +-->  stmtgp ;
  ; cond  --+-->  cond     ;
  ;         +-->  stmtgp   ;
  ; do --+-->  do          ;
  ;      +-->  stmtgp      ;
  ; return  --+-->  return ;
  ;           +-->  stmtgp ;
  (cond ((lispassignp stmt)
	 (cond ((toolongassignp stmt)
		(segassign stmt))
	       (t
		stmt)))
	((lispcondp stmt)
	 (cond ((toolongcondp stmt)
		(segcond stmt))
	       (t
		stmt)))
	((lispdop stmt)
	 (cond ((toolongdop stmt)
		(segdo stmt))
	       (t
		stmt)))
	((lispreturnp stmt)
	 (cond ((toolongreturnp stmt)
		(segreturn stmt))
	       (t
		stmt)))
	(t
	 stmt)))

(de segassign (stmt)
  ; assign  -->  stmtgp ;
  (prog (var exp type)
	(setq var (cadr stmt))
	(setq type (getvartype var))
	(setq exp (caddr stmt))
	(setq stmt (segexp1 exp type))
	(rplaca stmt (mkassign var (car stmt)))
	(return (mkstmtgp 0 (reverse stmt)))))

(de segcond (cond)
  ; cond  --+-->  cond   ;
  ;         +-->  stmtgp ;
  (prog (tassigns res markedvars type)
	(cond ((eq *gentranlang 'c)
	       (setq type 'int))
	      (t
	       (setq type 'logical)))
	(while (setq cond (cdr cond))
	       (prog (exp stmt)
		     (cond ((toolongexpp (setq exp (caar cond)))
			    (progn
			     (setq exp (segexp1 exp type))
			     (setq tassigns (append (cdr exp) tassigns))
			     (setq exp (car exp))
			     (markvar exp)
			     (setq markedvars (cons exp markedvars)))))
		     (setq stmt (foreach st in (cdar cond) collect
					 (segstmt st)))
		     (setq res (cons (cons exp stmt) res))))
	(recurunmark markedvars)
	(return (cond (tassigns
		       (mkstmtgp 0
				 (reverse (cons (mkcond (reverse res))
						tassigns))))
		      (t
		       (mkcond (reverse res)))))))

(de segdo (stmt)
  ; do  --+-->  do     ;
  ;       +-->  stmtgp ;
  (prog (tassigns var initexp nextexp exitcond body markedvars type)
	(setq body (cdddr stmt))
	(cond ((setq var (cadr stmt))
	       (progn
		(cond ((toolongexpp (setq initexp (cadar var)))
		       (progn
			(setq type (getvartype (caar var)))
			(setq initexp (segexp1 initexp type))
			(setq tassigns (cdr initexp))
			(setq initexp (car initexp))
			(markvar initexp)
			(setq markedvars (cons initexp markedvars)))))
		(cond ((toolongexpp (setq nextexp (caddar var)))
		       (progn
			(setq type (getvartype (caar var)))
			(setq nextexp (segexp1 nextexp type))
			(setq body (append body (reverse (cdr nextexp))))
			(setq nextexp (car nextexp))
			(markvar nextexp)
			(setq markedvars (cons nextexp markedvars)))))
		(setq var (list (list (caar var) initexp nextexp))))))
	(cond ((toolongexpp (car (setq exitcond (caddr stmt))))
	       (prog (texps ltype)
		     (cond ((eq *gentranlang 'c)
			    (setq ltype 'int))
			   (t
			    (setq ltype 'logical)))
		     (setq texps (segexp1 (car exitcond) ltype))
		     (markvar (car texps))
		     (setq markedvars (cons (car texps) markedvars))
		     (rplaca exitcond (car texps))
		     (foreach texp in (reverse (cdr texps)) do
			      (progn
			       (setq texp (reverse texp))
			       (setq var
				     (cons (cdr (reverse (cons (car texp)
							       texp)))
					   var))))
		     (setq var (reverse var)))))
	(setq body (foreach st in body collect (segstmt st)))
	(recurunmark markedvars)
	(return (cond (tassigns
		       (mkstmtgp 0 (reverse (cons (mkdo var exitcond body)
						  tassigns))))
		      (t
		       (mkdo var exitcond body))))))

(de segreturn (ret)
  ; return  -->  stmtgp ;
  (progn
   (setq ret (segexp1 (cadr ret) 'unknown))
   (rplaca ret (mkreturn (car ret)))
   (mkstmtgp 0 (reverse ret))))

(de seggroup (stmtgp)
  ; stmtgp  -->  stmtgp ;
  (prog (locvars res)
	(cond ((equal (car stmtgp) 'prog)
	       (progn
		(setq locvars (cadr stmtgp))
		(setq stmtgp (cdr stmtgp))))
	      (t
	       (setq locvars 0)))
	(while (setq stmtgp (cdr stmtgp))
	       (setq res (cons (segstmt (car stmtgp)) res)))
	(return (mkstmtgp locvars (reverse res)))))

(de segdef (def)
  ; def  -->  def ;
  (mkdef (cadr def)
	 (caddr def)
	 (foreach stmt in (cdddr def) collect (segstmt stmt))))


;;                                             ;;
;;  2. long statement & expression predicates  ;;
;;                                             ;;


(de toolongexpp (exp)
  (greaterp (numprintlen exp) maxexpprintlen*))

(de toolongstmtp (stmt)
  (cond ((atom stmt) nil)  ;; pwang 11/11/86
	((lispstmtp stmt)
	 (cond ((lispcondp stmt)
		(toolongcondp stmt))
	       ((lispassignp stmt)
		(toolongassignp stmt))
	       ((lispreturnp stmt)
		(toolongreturnp stmt))
	       ((lispdop stmt)
		(toolongdop stmt))
	       (t
		(eval (cons 'or
			    (foreach exp in stmt collect (toolongexpp exp)))))))
	(t
	 (toolongstmtgpp stmt))))

(de toolongassignp (assign)
  (toolongexpp (caddr assign)))

(de toolongcondp (cond)
  (prog (toolong)
	(while (setq cond (cdr cond))
	       (cond ((or (toolongexpp (caar cond))
			  (toolongstmtp (cadar cond)))
		      (setq toolong t))))
	(return toolong)))

(de toolongdop (dostmt)
  (cond ((greaterp (eval (cons 'plus (foreach exp in (caadr dostmt) collect
					      (numprintlen exp))))
		   maxexpprintlen*) t)
	((toolongexpp (caaddr dostmt)) t)
	((lispstmtgpp (cadddr dostmt)) (toolongstmtgpp (cadddr dostmt)))
	(t (eval (cons 'or (foreach stmt in (cdddr dostmt) collect
				    (toolongstmtp stmt)))))))

(de toolongreturnp (ret)
  (toolongexpp (cadr ret)))

(de toolongstmtgpp (stmtgp)
  (eval (cons 'or
	      (foreach stmt in (cdr stmtgp) collect (toolongstmtp stmt)))))

(de toolongdefp (def)
  (cond ((lispstmtgpp (cadddr def))
	 (toolongstmtgpp (cadddr def)))
	(t
	 (eval (cons 'or
		     (foreach stmt in (cdddr def) collect
			      (toolongstmtp stmt)))))))


;;                            ;;
;;  3. print length function  ;;
;;                            ;;


(de numprintlen (exp)
  (cond ((atom exp)
	 (length (explode exp)))
	((onep (length exp))
	 (numprintlen (car exp)))
	(t
	 (plus (length exp)
	       (eval (cons 'plus
			   (foreach elt in (cdr exp) collect
				    (numprintlen elt))))))))
