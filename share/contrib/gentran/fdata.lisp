(defun fortdata (stmt)
  (append (list (mkforttab) "data " (cadr stmt) "//")
	  (addcom (cddr stmt))
	  (list "//"))
)

(setq COMMA* ",")

(defun addcom(nl)
(cond ((null nl) nil)
      ((null (cdr nl)) nl)
      (t (cons (car nl) (cons COMMA* (addcom (cdr nl)))))
      )
)



(defun fortstmt (stmt)
  (cond ((null stmt) nil)
	((member stmt '($begin_group $end_group)) nil)
	((lisplabelp stmt) (fortstmtno stmt))
	((eq (car stmt) 'data) (fortdata stmt))
	((eq (car stmt) 'literal) (fortliteral stmt))
	((lispreadp stmt) (fortread stmt))
	((lispassignp stmt) (fortassign stmt))
	((lispprintp stmt) (fortwrite stmt))
	((lispcondp stmt) (fortif stmt))
	((lispbreakp stmt) (fortbreak stmt))
	((lispgop stmt) (fortgoto stmt))
	((lispreturnp stmt) (fortreturn stmt))
	((lispstopp stmt) (fortstop stmt))
	((lispendp stmt) (fortend stmt))
	((lispdop stmt) (fortloop stmt))
	((lispstmtgpp stmt) (fortstmtgp stmt))
	((lispdefp stmt) (fortsubprog stmt))
	((lispcallp stmt) (fortcall stmt))))




(defun fortexp1 (exp wtin)
  (cond ((atom exp) (list (fortranname exp)))
	((eq (car exp) 'data) (fortdata exp))
	((eq (car exp) 'literal) (fortliteral exp))
	((null (cdr exp)) exp)
	((memq (car exp) '(minus not))
	 (let* ((wt (fortranprecedence (car exp)))
		(res (cons (fortranop (car exp)) (fortexp1 (cadr exp) wt))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	((or (memq (car exp) *lisparithexpops*)
	     (memq (car exp) *lisplogexpops*))
	 (let* ((wt (fortranprecedence (car exp)))
		(op (fortranop (car exp)))
		(res (fortexp1 (cadr exp) wt))
		(res1))
	       (setq exp (cdr exp))
	       (cond ((eq op '+)
		      (while (setq exp (cdr exp))
                         (progn
			  (setq res1 (fortexp1 (car exp) wt))
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
						 (fortexp1 (car exp) wt)))))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	(t
	 (let ((res (cons (car exp) (cons '|(| (fortexp1 (cadr exp) 0)))))
              (setq exp (cdr exp))
	      (while (setq exp (cdr exp))
                 (setq res (append res (cons '|,| (fortexp1 (car exp) 0)))))
              (aconc res '|)|)))))



(defun franzstmt (stmt)
  ; return the franz lisp equivalent statement ;
  (cond ((member (caar stmt) '( msetq mdo )) 
	 (setq lefttype (exptype (cadr stmt))) ))
		;;added by Trevor 12/28/86

  (cond ((null stmt) nil)
	((maclabelp stmt) (franzlabel stmt))
	((macstmtgpp stmt) (franzstmtgp stmt))
	((macdefp stmt) (franzdef stmt))
	((macreadp stmt) (franzread stmt))
	((macmatassignp stmt) (franzmatassign stmt))
	((macnestassignp stmt) (franznestassign stmt))
	((macassignp stmt) (franzassign stmt))
	((macifp stmt) (franzif stmt))
	((macforp stmt) (franzfor stmt))
	((macforinp stmt) (franzforin stmt))
	((macgop stmt) (franzgo stmt))
	((macretp stmt) (franzret stmt))
	((macprintp stmt) (franzprint stmt))
	((macstopp stmt) (franzstop stmt))
	((macendp stmt) (franzend stmt))
	((mac$literalp stmt) (franzliteral (stripdollar1 (caar stmt)) stmt))
	((maccallp stmt) (franzcall stmt))))


(defun mac$literalp (stmt)
  ; is stmt a $literal function? ;
  (memq (caar stmt) '($literal literal $data data)))

(defun franzliteral (fn stmt)
  (cons fn
	(foreach exp in (cdr stmt) collect
		 (cond ((memq exp '($tab $cr)) exp)
		       ((listp exp) (franzexp exp 0 stmt))
		       (t (stripdollar1 exp))))))
	
(defun macexpp (exp)
  ; is exp an arithmetic or logical macsyma expression? ;
  (cond ((null exp) nil)
	((atom exp))
	((atom (car exp)) nil)
	((not (memq (caar exp) '(mcond mdefine mdo mdoin mgo mprog mprogn
			         mreturn msetq $end $ev $literal $print
				 $readonly $stop $data))))))
