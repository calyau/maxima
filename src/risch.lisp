;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module risch)

(load-macsyma-macros rzmac ratmac)

(declare-top (special *mosesflag
                      context *in-risch-p*))

(defmvar $erfflag t "Controls whether `risch' generates `erfs'")

(defvar *changevp* t
  "When nil prevents changevar hack")

(defmacro pair (al bl) `(mapcar #'cons ,al ,bl))

;; internal representation of risch expressions: list with canonical rational
;; expression (CRE) as first element, standard maxima expressions as remaining
;; elements.  risch expression is sum of CRE and remaining elements.
(defmacro rischzero () ''((0 . 1) 0))

(defun rischnoun (exp1 risch-ratform risch-intvar &optional (exp2 exp1 exp2p))
  (unless exp2p (setq exp1 (rzero)))
  `(,exp1 ((%integrate) ,(disrep exp2 risch-ratform) ,risch-intvar)))

(defun getrischvar ()
  (do ((vl varlist (cdr vl))
       (gl genvar (cdr gl)))
      ((null (cdr vl)) (car gl))))

;; test whether CRE p is constant with respect to variable of integration.
;; requires variables in varlist and genvar
;; to be ordered as by intsetup, with var of integration ordered before
;; any other expressions that contain it.
(defun risch-pconstp (p risch-mainvar)
  (or (pcoefp p) (pointergp risch-mainvar (car p))))

(defun risch-constp (r risch-mainvar)
  (setq r (ratfix r))
  (and (risch-pconstp (car r) risch-mainvar)
       (risch-pconstp (cdr r) risch-mainvar)))

;; adds two risch expressions (defined above).
(defun rischadd (x y)
  (destructuring-let (((a . b) x) ((c . d) y))
    (cons (r+ a c) (append b d))))

(defmfun $risch (exp risch-var)
  (let ((*integrator-level* 0))
    (declare (special *integrator-level*))
    (with-new-context (context)
      (rischint exp risch-var))))

(defun spderivative (p risch-var)
  (cond ((pcoefp p) '(0 . 1))
	((null (cdr p)) '(0 . 1))
	((or (not (atom (car p))) (numberp (car p))) ;P IS A RATFORM
	 (let ((denprime (spderivative (cdr p) risch-var)))
	   (cond ((rzerop denprime)
		  (ratqu (spderivative (car p) risch-var) (cdr p)))
		 (t (ratqu (r- (r* (spderivative (car p) risch-var)
				   (cdr p))
			       (r* (car p) denprime))
			   (r* (cdr p) (cdr p)))))))
	(t (r+ (spderivative1 (car p)
			      (cadr p)
			      (caddr p)
			      risch-var)
	       (spderivative (cons (car p) (cdddr p))
			     risch-var)))))

(defun spderivative1 (var1 deg coeff risch-var)
  (cond ((eq var1 risch-var)
	 (r* (ratexpt (cons (list risch-var 1 1) 1) (1- deg))
	     (pctimes deg coeff)))
	((pointergp risch-var var1) '(0 . 1))
	((equal deg 0) (spderivative coeff risch-var))
	(t (r+ (r* (ratexpt (cons (list var1 1 1) 1) deg)
		   (spderivative coeff risch-var))
	       (r* (cond ((equal deg 1) coeff)
			 (t (r* deg
				coeff
				(ratexpt (cons (list var1 1 1) 1)
					 (1- deg)))))
		   (get var1 'rischdiff) )))))

(defun polylogp (exp &optional sub)
  (and (mqapplyp exp) (eq (subfunname exp) '$li)
       (or (null sub) (equal sub (car (subfunsubs exp))))))

(defun rischint (exp risch-intvar &aux ($logarc nil) ($exponentialize nil)
                     ($gcd '$algebraic) ($algebraic t) (implicit-real t)
                     ($float nil) ($numer nil)
                     ;; The risch integrator expects $logexpand T. Otherwise,
                     ;; the integrator hangs for special types of integrals
                     ;; (See bug report ID:3039452)
                     ($logexpand t))
  (prog ($%e_to_numlog $logsimp risch-y z risch-var risch-ratform risch-liflag
	 risch-mainvar varlist genvar $ratfac $ratalgdenom risch-degree
	 rischform-value risch-trigint risch-hypertrigint risch-operator)
     (if (specrepp exp)
	 (setq exp (specdisrep exp)))
     (if (specrepp risch-intvar)
	 (setq risch-intvar (specdisrep risch-intvar)))
     (if (mnump risch-intvar)
	 (merror (intl:gettext "risch: attempt to integrate wrt a number: ~:M") risch-intvar))
     (if (and (atom risch-intvar)
	      (isinop exp risch-intvar))
	 (go noun))
     (multiple-value-setq (rischform-value risch-trigint risch-hypertrigint risch-operator)
       (rischform exp risch-intvar))
     (cond (risch-trigint
	    (return (trigin1 exp risch-intvar)))
	   (risch-hypertrigint
	    (return (hypertrigint1 exp risch-intvar t)))
	   (risch-operator
	    (go noun)))
     (multiple-value-setq (risch-y risch-operator)
       (intsetup exp risch-intvar))
     (if risch-operator
	 (go noun))
     (setq risch-ratform (car risch-y))
     (setq varlist (caddr risch-ratform))
     (setq risch-mainvar (caadr (ratf risch-intvar)))
     (setq genvar (cadddr risch-ratform))
     (unless (some #'algpget varlist)
       (setq $algebraic nil)
       (setq $gcd (car *gcdl*)))
     (setq risch-var (getrischvar))
     (setq z (tryrisch (cdr risch-y) risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree risch-var))
     (setf (caddr risch-ratform) varlist)
     (setf (cadddr risch-ratform) genvar)
     (return (cond ((atom (cdr z))
		    (disrep (car z) risch-ratform))
		   (t
		    (let (($logsimp t)
			  ($%e_to_numlog t))
		      (simplify (list* '(mplus)
				       (disrep (car z) risch-ratform)
				       (cdr z)))))))
   noun
     (return (list '(%integrate) exp risch-intvar))))

(defun rischform (l risch-intvar)
  (let (risch-trigint risch-hypertrigint risch-operator)
    (labels
	((rischform-impl (l risch-intvar)
	   (cond ((or (atom l)
		      (alike1 risch-intvar l)
		      (freeof risch-intvar l))
		  nil)
		 ((polylogp l)
		  (if (and (integerp (car (subfunsubs l)))
			   (signp g (car (subfunsubs l))))
		      (rischform-impl (car (subfunargs l)) risch-intvar)
		      (setq risch-operator t)))
		 ((atom (caar l))
		  (case (caar l)
		    ((%sin %cos %tan %cot %sec %csc)
		     (setq risch-trigint t $exponentialize t)
		     (rischform-impl (cadr l) risch-intvar))
		    ((%asin %acos %atan %acot %asec %acsc)
		     (setq risch-trigint t $logarc t)
		     (rischform-impl (cadr l) risch-intvar))
		    ((%sinh %cosh %tanh %coth %sech %csch)
		     (setq risch-hypertrigint t $exponentialize t)
		     (rischform-impl (cadr l) risch-intvar))
		    ((%asinh %acosh %atanh %acoth %asech %acsch)
		     (setq risch-hypertrigint t $logarc t)
		     (rischform-impl (cadr l) risch-intvar))
		    ((mtimes mplus mexpt rat %erf %log)
		     (mapc #'(lambda (e)
			       (rischform-impl e risch-intvar))
			   (cdr l)))
		    (t
		     (setq risch-operator (caar l)))))
		 (t
		  (setq risch-operator (caar l))))))
      (values (rischform-impl l risch-intvar)
	      risch-trigint
	      risch-hypertrigint
	      risch-operator))))

(defun hypertrigint1 (exp risch-var hyperfunc)
  (let ((result (if hyperfunc
                    (sinint (resimplify exp) risch-var)
                    (rischint (resimplify exp) risch-var))))
    ;; The result can contain solveable integrals. Look for this case.
    (if (isinop result '%integrate)
        ;; Found an integral. Evaluate the result again.
        ;; Set the flag *in-risch-p* to make sure that we do not call
        ;; rischint again from the integrator. This avoids endless loops.
        (let ((*in-risch-p* t)) 
          (meval (list '($ev) result '$nouns)))
        result)))

(defun trigin1 (risch-*exp risch-var)
  (let ((yyy (hypertrigint1 risch-*exp risch-var nil)))
    (setq yyy (div ($expand ($num yyy))
		   ($expand ($denom yyy))))
    (let ((rischp risch-var)
	  (rp-polylogp t)
	  $logarc $exponentialize result)
      (setq result (sratsimp (if (and (freeof '$%i risch-*exp) (freeof '$li yyy))
                                 ($realpart yyy)
                                 ($rectform yyy))))
      ;; The result can contain solveable integrals. Look for this case.
      (if (isinop result '%integrate)
          ;; Found an integral. Evaluate the result again.
          ;; Set the flag *in-risch-p* to make sure that we do not call
          ;; rischint again from the integrator. This avoids endless loops.
          (let ((*in-risch-p* t)) 
            (meval (list '($ev) result '$nouns)))
          result))))

(defun tryrisch (exp risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree risch-var)
  (prog (risch-logptdx risch-expflag risch-expstuff risch-expint risch-y)
     (setq risch-expstuff '(0 . 1))
     (cond ((eq risch-mainvar risch-var)
	    (return (rischfprog exp risch-ratform risch-var)))
	   ((eq (get risch-var 'leadop)
		'mexpt)
	    (setq risch-expflag t)))
     (multiple-value-setq (risch-y risch-logptdx risch-expint)
       (rischlogdprog exp risch-ratform risch-intvar risch-liflag risch-var
		      risch-expflag risch-mainvar risch-expint risch-degree))
     (dolist (rat risch-logptdx)
       (let (rischlogeprog-value)
	 (setq risch-y
	       (rischadd (multiple-value-setq (rischlogeprog-value risch-expint)
			   (rischlogeprog rat risch-ratform nil risch-intvar risch-expstuff
					  risch-var risch-expflag risch-mainvar risch-expint))
			 risch-y))))
     (if varlist
	 (setq risch-y (rischadd (tryrisch1 risch-expstuff risch-mainvar
					    risch-ratform risch-intvar risch-liflag
					    risch-degree)
			   risch-y)))
     (return (if risch-expint
		 (rischadd (rischexppoly risch-expint risch-var
					 risch-ratform risch-intvar risch-liflag
					 risch-degree risch-mainvar)
			   risch-y)
		 risch-y))))

(defun tryrisch1 (exp risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree)
  (let* ((varlist (reverse (cdr (reverse varlist))))
	 (risch-var (getrischvar)))
    (tryrisch exp risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree risch-var)))

(defun rischfprog (rat risch-ratform risch-var)
  (multiple-value-bind (dprog-ret risch-logptdx)
      (dprog rat risch-ratform risch-var)
    (cons (cdr (ratrep* dprog-ret))
	  (let ((varlist varlist)
		(genvar (subseq genvar 0 (length varlist))))
	    (mapcar #'(lambda (p)
			(eprog p risch-ratform risch-var nil))
		    risch-logptdx)))))

(defun rischlogdprog (ratarg risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree)
  (prog (arootf deriv thebpg thetop thebot prod1 prod2 ans
	 risch-wholepart risch-logptdx risch-parnumer risch-pardenom
	 risch-rootfactor rischlogpoly-value)
     (setq ans '(0 . 1))
     (cond ((or (pcoefp (cdr ratarg))
		(pointergp risch-var (cadr ratarg)))
	    (multiple-value-setq (rischlogpoly-value risch-expint)
	      (rischlogpoly ratarg risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree))
	    (return (values rischlogpoly-value
			    risch-logptdx
			    risch-expint))))

     (multiple-value-setq (risch-rootfactor risch-pardenom)
       (aprog (ratdenominator ratarg) risch-var))
     (multiple-value-setq (risch-parnumer risch-wholepart)
       (cprog (ratnumerator ratarg)
	      (ratdenominator ratarg)
	      risch-var
	      risch-pardenom))
     (do ((risch-rootfactor (reverse risch-rootfactor) (cdr risch-rootfactor))
	  (risch-parnumer (reverse risch-parnumer) (cdr risch-parnumer))
	  (risch-klth (length risch-rootfactor) (1- risch-klth)))
	 ((= risch-klth 1))
       (setq arootf (car risch-rootfactor))
       (cond
	 ((pcoefp arootf))
	 ((and (eq (get (car arootf) 'leadop) 'mexpt)
	       (null (cdddr arootf)))
	  (setq
	   risch-expint
	   (append
	    (cond ((and (not (atom (car risch-parnumer)))
			(not (atom (caar risch-parnumer)))
			(eq (caaar risch-parnumer) (car arootf)))
		   (gennegs arootf (cdaar risch-parnumer) (cdar risch-parnumer) risch-klth))
		  (t (list
		      (list 'neg (car risch-parnumer)
			    (car arootf) risch-klth (cadr arootf)))))
	    risch-expint)))
	 ((not (zerop (pdegree arootf risch-var)))
	  (setq deriv (spderivative arootf risch-mainvar))
	  (setq thebpg (bprog arootf (ratnumerator deriv) risch-var))
	  (setq thetop (car risch-parnumer))
	  (do ((kx (1- risch-klth) (1- kx))) ((= kx 0))
	    (setq prod1 (r* thetop (car thebpg)))
	    (setq prod2 (r* thetop (cdr thebpg) (ratdenominator deriv)))
	    (setq thebot (pexpt arootf kx))
	    (setq ans (r+ ans (ratqu (r- prod2) (r* kx thebot))))
	    (setq thetop
		  (r+ prod1 (ratqu (spderivative prod2 risch-mainvar) kx)))
	    (setq thetop (cdr (ratdivide thetop thebot))))
	  (push (ratqu thetop arootf) risch-logptdx))))
     (push (ratqu (car risch-parnumer) (car risch-rootfactor)) risch-logptdx)
     (cond ((or (pzerop ans) (pzerop (car ans)))
	    (multiple-value-setq (rischlogpoly-value risch-expint)
	      (rischlogpoly risch-wholepart risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree))
	    (return (values rischlogpoly-value
			    risch-logptdx
			    risch-expint))))
     (setq thetop (cadr (pdivide (ratnumerator ans)
				 (ratdenominator ans))))
     (multiple-value-setq (rischlogpoly-value risch-expint)
       (rischlogpoly risch-wholepart risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree))
     (return (values (rischadd (ncons (ratqu thetop (ratdenominator ans)))
			       rischlogpoly-value)
		     risch-logptdx
		     risch-expint))))

(defun gennegs (denom num numdenom risch-klth)
  (cond ((null num) nil)
	(t (cons (list 'neg (cadr num)
		       (car denom)
		       (- risch-klth (car num))
		       (r* numdenom (caddr denom) ))
		 (gennegs denom (cddr num) numdenom risch-klth)))))

(defun rischlogeprog (p risch-ratform risch-switch1 risch-intvar risch-expstuff
		      risch-var risch-expflag risch-mainvar risch-expint)
  (labels
      ((impl (p risch-switch1)
	 (prog (p1e p2e p2deriv logcoef ncc dcc allcc expcoef my-divisor
		risch-parnumer risch-pardenom)
	    (if (or (pzerop p) (pzerop (car p)))
		(return (rischzero)))
	    (setq p1e (ratnumerator p))
	    (desetq (dcc p2e) (oldcontent (ratdenominator p)))
	    (cond ((and (not risch-switch1)
			(cdr (setq risch-pardenom (intfactor p2e))))
		   (setq risch-parnumer nil)
		   (setq risch-switch1 t)
		   (desetq (ncc p1e) (oldcontent p1e))
		   (multiple-value-setq (risch-parnumer)
		     (cprog p1e p2e risch-var risch-pardenom))
		   (setq allcc (ratqu ncc dcc))
		   (return (do ((pnum risch-parnumer (cdr pnum))
				(pden risch-pardenom (cdr pden))
				(ans (rischzero)))
			       ((or (null pnum) (null pden))
				(setq risch-switch1 nil) ans)
			     (setq ans (rischadd
					(impl
					 (r* allcc (ratqu (car pnum) (car pden)))
					 risch-switch1)
					ans))))))
	    (when (and risch-expflag (null (p-red p2e)))
	      (push (cons 'neg p) risch-expint)
	      (return (rischzero)))
	    (if risch-expflag
		(setq expcoef (r* (p-le p2e) (ratqu (get risch-var 'rischdiff)
						    (make-poly risch-var)))))
	    (setq p1e (ratqu p1e (ptimes dcc (p-lc p2e)))
		  p2e (ratqu p2e (p-lc p2e))) ;MAKE DENOM MONIC
	    (setq p2deriv (spderivative p2e risch-mainvar))
	    (setq my-divisor (if risch-expflag
				 (r- p2deriv (r* p2e expcoef))
				 p2deriv))
	    (when (equal my-divisor '(0 . 1))
	      ;; (format t "HEY RISCHLOGEPROG, FOUND ZERO DIVISOR; GIVE UP.~%")
	      (return (rischnoun p risch-ratform risch-intvar)))
	    (setq logcoef (ratqu p1e my-divisor))
	    (when (risch-constp logcoef risch-mainvar)
	      (if risch-expflag
		  (setq risch-expstuff (r- risch-expstuff (r* expcoef logcoef))))
	      (return
		(list
		 '(0 . 1)
		 (list '(mtimes)
		       (disrep logcoef risch-ratform)
		       (logmabs (disrep p2e risch-ratform))))))
	    (if (and risch-expflag
		     $liflag
		     *changevp*)
		(let* ((newvar (gensym))
		       (new-int ($changevar
				 `((%integrate) ,(simplify (disrep p risch-ratform)) ,risch-intvar)
				 (sub newvar (get risch-var 'rischexpr))
				 newvar risch-intvar))
		       (*changevp* nil)) ;prevents recursive changevar
		  (if (and (freeof risch-intvar new-int)
			   (freeof '%integrate
				   (setq new-int (rischint (sdiff new-int newvar)
							   newvar))))
		      (return
			(list (rzero)
			      (maxima-substitute (get risch-var 'rischexpr) newvar new-int))))))
	    (return (rischnoun p risch-ratform risch-intvar)))))
    (values (impl p risch-switch1)
	    risch-expint)))


(defun findint (exp)
  (cond ((atom exp) nil)
	((atom (car exp)) (findint (cdr exp)))
	((eq (caaar exp) '%integrate) t)
	(t (findint (cdr exp)))))

(defun logequiv (fn1 fn2 risch-intvar)
  (freeof risch-intvar ($ratsimp (div* (remabs (leadarg fn1))
				 (remabs (leadarg fn2))))))

(defun remabs (exp)
  (cond ((atom exp) exp)
	((eq (caar exp) 'mabs) (cadr exp))
	(t exp)))

(defun getfnsplit (l risch-intvar)
  (let (coef fn)
    (dolist (x l (values (muln coef nil) (muln fn nil)))
      (if (free x risch-intvar)
          (push x coef)
          (push x fn)))))

(defun getfncoeff (a form risch-intvar risch-liflag risch-degree risch-cary risch-nogood risch-lians)
  (labels
      ((getfncoeff-impl (a)
	 (cond ((null a) 0)
	       ((equal (car a) 0)
		(getfncoeff-impl (cdr a)))
	       ((and (listp (car a))
		     (eq (caaar a) 'mplus)
		     (ratpl (getfncoeff-impl (cdar a))
			    (getfncoeff-impl (cdr a)))))
	       ((and (listp (car a))
		     (eq (caaar a) 'mtimes))
		(multiple-value-bind (coef newfn)
		    (getfnsplit (cdar a) risch-intvar)
		  ;; (car a) is a mtimes expression. We insert coef and newfn as the
		  ;; new arguments to the mtimes expression. This causes problems if
		  ;;   (1) coef is a mtimes expression too and
		  ;;   (2) (car a) has already a simp flag
		  ;; We get a nested mtimes expression, which does not sgetfncoeff-implify.
		  ;; We comment out the following code (DK 09/2009):
		  ;; (setf (cdar a) (list coef newfn))
	   
		  ;; Insert a complete mtimes expression without simpflag.
		  ;; Nested mtimes expressions sgetfncoeff-implify further.
		  (setf (car a) (list '(mtimes) coef newfn))
	   
		  (setf (cdar a) (list coef newfn))
		  (cond ((zerop1 coef)
			 (getfncoeff-impl (cdr a)))
			((and (matanp newfn)
			      (member '$%i varlist :test #'eq))
			 (let (($logarc t) ($logexpand '$all))
			   (rplaca a ($expand (resimplify (car a)))))
			 (getfncoeff-impl a))
			((and (alike1 (leadop newfn) (leadop form))
			      (or (alike1 (leadarg newfn) (leadarg form))
				  (and (mlogp newfn)
				       (logequiv form newfn risch-intvar))))
			 (ratpl (rform coef)
				(prog2 (rplaca a 0)
				    (getfncoeff-impl (cdr a)))))
			((do ((vl varlist (cdr vl)))
			     ((null vl))
			   (and (not (atom (car vl)))
				(alike1 (leadop (car vl)) (leadop newfn))
				(if (mlogp newfn)
				    (logequiv (car vl) newfn risch-intvar)
				    (alike1 (car vl) newfn))
				(rplaca (cddar a) (car vl))
				(return nil))))
			((let (vlist)
			   (declare (special vlist))
			   ;; newvar1 accesses the special var vlist.
			   (newvar1 (car a))
			   (null vlist))
			 (setq risch-cary
			       (ratpl (cdr (ratrep* (car a)))
				      risch-cary))
			 (rplaca a 0)
			 (getfncoeff-impl (cdr a)))
			((and risch-liflag
			      (mlogp form)
			      (mlogp newfn))
			 (let (res)
			   (multiple-value-setq (res risch-nogood)
			     (dilog (cons (car a) form) risch-intvar risch-degree risch-nogood))
			   (push res risch-lians))
			 (rplaca a 0)
			 (getfncoeff-impl (cdr a)))
			((and risch-liflag
			      (polylogp form)
			      (mlogp newfn)
			      (logequiv form newfn risch-intvar))
			 (push (mul* (cadar a) (make-li (1+ (car (subfunsubs form)))
							(leadarg form)))
			       risch-lians)
			 (rplaca a 0)
			 (getfncoeff-impl (cdr a)))
			(t (setq risch-nogood t) 0))))
	       (t
		(rplaca a (list '(mtimes) 1 (car a)))
		(getfncoeff-impl a)))))
    (values (getfncoeff-impl a) risch-cary risch-nogood
	    risch-lians)))


(defun rischlogpoly (exp risch-ratform risch-intvar risch-liflag risch-var risch-expflag risch-mainvar risch-expint risch-degree)
  (let
      ((result
	 (cond ((equal exp '(0 . 1))
		(rischzero))
	       (risch-expflag
		(push (cons 'poly exp) risch-expint)
		(rischzero))
	       ((not (among risch-var exp))
		(tryrisch1 exp risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree))
	       (t
		(do ((risch-degree (pdegree (car exp) risch-var) (1- risch-degree))
		     (p (car exp))
		     (den (cdr exp))
		     (risch-lians ())
		     (sum (rzero))
		     (risch-cary (rzero))
		     (risch-y)
		     (z)
		     (ak)
		     (risch-nogood)
		     (lbkpl1))
		    ((minusp risch-degree)
		     (cons sum (append risch-lians (cdr risch-y))))
		  (setq ak (r- (ratqu (polcoef p risch-degree risch-var) den)
			       (r* (cons (1+ risch-degree) 1)
				   risch-cary
				   (get risch-var 'rischdiff))))
		  (if (not (pzerop (polcoef p risch-degree risch-var)))
		      (setq p (if (pcoefp p)
				  (pzero)
				  (psimp risch-var (p-red p)))))
		  (setq risch-y (tryrisch1 ak risch-mainvar
					   risch-ratform risch-intvar risch-liflag risch-degree))
		  (setq risch-cary (car risch-y))
		  (and (> risch-degree 0)
		       (setq risch-liflag $liflag))
	   
		  (multiple-value-setq (z risch-cary risch-nogood risch-lians)
		    (getfncoeff (cdr risch-y)
				(get risch-var 'rischexpr)
				risch-intvar risch-liflag risch-degree risch-cary risch-nogood
				risch-lians))
		  (setq risch-liflag nil)
		  (cond ((and (> risch-degree 0)
			      (or risch-nogood
				  (findint (cdr risch-y))))
			 (return (rischnoun sum risch-ratform 
					    risch-intvar
					    (r+ (r* ak
						    (make-poly risch-var risch-degree 1))
						(ratqu p den))))))
		  (setq lbkpl1 (ratqu z (cons (1+ risch-degree) 1)))
		  (setq sum (r+ (r* lbkpl1 (make-poly risch-var (1+ risch-degree) 1))
				(r* risch-cary (if (zerop risch-degree) 1
						   (make-poly risch-var risch-degree 1)))
				sum)))))))
    (values result risch-expint)))

(defun make-li (sub arg)
  (subfunmake '$li (ncons sub) (ncons arg)))

;;integrates log(ro)^risch-degree*log(rn)' in terms of polylogs
;;finds constants c,d and integers j,k such that
;;c*ro^j+d=rn^k  If ro and rn are poly's then can assume either j=1 or k=1
(defun dilog (l risch-intvar risch-degree risch-nogood)
  (destructuring-let* ((((nil coef nlog) . olog) l)
		       (narg (remabs (cadr nlog)))
		       (varlist varlist)
		       (genvar genvar)
		       (rn (rform narg))	;; can add new vars to varlist
		       (ro (rform (cadr olog)))
		       (risch-var (caar ro))
		       ((j . k) (ratreduce (pdegree (car rn) risch-var) (pdegree (car ro) risch-var)))
		       (idx (gensym))
		       (rc) (rd))
    (cond ((and (= j 1) (> k 1))
	   (setq rn (ratexpt rn k)
		 coef (div coef k)
		 narg (rdis rn)))
	  ((and (= k 1) (> j 1))
	   (setq ro (ratexpt ro j)
		 coef (div coef (f* j risch-degree))
		 olog (mul j olog))))
    (desetq (rc . rd) (ratdivide rn ro))
    (let
	((result
	  (cond ((and (freeof risch-intvar (rdis rc)) ;; can't use risch-constp because varlist
		      (freeof risch-intvar (rdis rd))) ;; is not set up with vars in correct order.
		 (setq narg ($ratsimp (sub 1 (div narg (rdis rd)))))
		 (mul* coef (power -1 (1+ risch-degree))
		       `((mfactorial) ,risch-degree)
		       (dosum (mul* (power -1 idx)
				    (div* (power olog idx)
					  `((mfactorial) ,idx))
				    (make-li (add risch-degree (neg idx) 1) narg))
			      idx 0 risch-degree t)))
		(t
		 (setq risch-nogood t)
		 0))))
      (values result risch-nogood))))

(defun exppolycontrol (flag f a expg n risch-ratform risch-intvar risch-liflag risch-degree risch-mainvar)
  (let (risch-y l risch-var (varlist varlist) (genvar genvar))
    (setq varlist (reverse (cdr (reverse varlist))))
    (setq risch-var (getrischvar))
    (setq risch-y (get risch-var 'leadop))
    (cond ((and (not (pzerop (ratnumerator f)))
		(risch-constp (setq l (ratqu a f)) risch-mainvar))
	   (cond (flag		;; multiply in expg^n - n may be negative
		  (list (r* l (ratexpt (cons (list expg 1 1) 1) n))
			0))
		 (t l)))
	  ((eq risch-y risch-intvar)
	   (rischexpvar flag (list f a expg n) risch-ratform risch-intvar risch-y risch-var risch-mainvar))
	  (t
	   (rischexplog (eq risch-y 'mexpt) flag f a
			(list expg n (get risch-var 'rischarg)
			      risch-var (get risch-var 'rischdiff))
			risch-ratform risch-intvar
			risch-liflag
			risch-degree
			risch-y
			risch-var
			risch-mainvar)))))

(defun rischexppoly (risch-expint risch-var risch-ratform risch-intvar risch-liflag risch-degree risch-mainvar)
  (let (risch-y
	w
	num
	denom type
	(ans (rischzero))
	(expdiff (ratqu (get risch-var 'rischdiff) (list risch-var 1 1))))
    (do ((risch-expint risch-expint (cdr risch-expint)))
	((null risch-expint)
	 ans)
      (desetq (type . risch-y) (car risch-expint))
      (desetq (num . denom) (ratfix risch-y))
      (cond ((eq type 'neg)
	     (setq w (exppolycontrol t
				     (r* (- (cadr denom))
					 expdiff)
				     (ratqu num (caddr denom))
				     risch-var
				     (- (cadr denom))
				     risch-ratform
				     risch-intvar
				     risch-liflag
				     risch-degree
				     risch-mainvar)))
	    ((or (numberp num)
		 (not (eq (car num) risch-var)))
	     (setq w (tryrisch1 risch-y risch-mainvar
				risch-ratform risch-intvar risch-liflag risch-degree)))
	    (t
	     (setq w (rischzero))
	     (do ((num (cdr num) (cddr num)))
		 ((null num))
	       (cond ((equal (car num) 0)
		      (setq w (rischadd
			       (tryrisch1 (ratqu (cadr num) denom)
					  risch-mainvar
					  risch-ratform risch-intvar risch-liflag risch-degree)
			       w)))
		     (t
		      (setq w (rischadd (exppolycontrol
					 t
					 (r* (car num) expdiff)
					 (ratqu (cadr num) denom)
					 risch-var
					 (car num)
					 risch-ratform
					 risch-intvar
					 risch-liflag
					 risch-degree
					 risch-mainvar)
					w)))))))
      (setq ans (rischadd w ans)))))

(defun rischexpvar (flag l risch-ratform risch-intvar risch-y risch-var risch-mainvar)
  (prog (lcm risch-m p risch-alphar risch-gamma delta r s
	 tt denom k wl wv i ytemp ttemp yalpha f a expg n yn yd
	 risch-beta)
     (desetq (f a expg n) l)
     (cond ((or (pzerop a) (pzerop (car a)))
	    (return (cond ((null flag) (rzero))
			  (t (rischzero))))))
     (setq denom (ratdenominator f))
     (multiple-value-setq (p risch-alphar)
       (findpr (cdr (partfrac a risch-mainvar))
	       (cdr (partfrac f risch-mainvar))
	       risch-y
	       risch-mainvar))
     (setq lcm (plcm (ratdenominator a) p))
     (setq risch-y (ratpl (spderivative (cons 1 p) risch-mainvar)
			  (ratqu f p)))
     (setq lcm (plcm lcm (ratdenominator risch-y)))
     (setq r (car (ratqu lcm p)))
     (setq s (car (r* lcm risch-y)))
     (setq tt (car (r* a lcm)))
     (setq risch-beta (pdegree r risch-mainvar))
     (setq risch-gamma (pdegree s risch-mainvar))
     (setq delta (pdegree tt risch-mainvar))
     (setq risch-alphar (max (- (1+ delta) risch-beta)
			     (- delta risch-gamma)))
     (setq risch-m 0)
     (cond ((equal (1- risch-beta) risch-gamma)
	    (setq risch-y (r* -1
			      (ratqu (polcoef s risch-gamma risch-var)
				     (polcoef r risch-beta risch-var))))
	    (and (equal (cdr risch-y) 1)
		 (numberp (car risch-y))
		 (setq risch-m (car risch-y)))))
     (setq risch-alphar (max risch-alphar risch-m))
     (if (minusp risch-alphar)
	 (return (if flag
		     (cxerfarg (rzero) expg n a risch-ratform risch-intvar risch-mainvar)
		     nil)))
     (cond ((not (and (equal risch-alphar risch-m)
		      (not (zerop risch-m))))
	    (go down2)))
     (setq k (+ risch-alphar risch-beta -2))
     (setq wl nil)
   l2
     (setq wv (list (cons (polcoef tt k risch-var) 1)))
     (setq i risch-alphar)
   l1
     (setq wv
	   (cons (r+ (r* (cons i 1)
			 (polcoef r (+ k 1 (- i)) risch-var))
		     (cons (polcoef s (+ k (- i)) risch-var) 1))
		 wv))
     (decf i)
     (cond ((> i -1)
	    (go l1)))
     (setq wl (cons wv wl))
     (decf k)
     (cond ((> k -1)
	    (go l2)))
     (multiple-value-setq (risch-y risch-m)
       (lsa wl))
     (if (or (eq risch-y 'singular)
	     (eq risch-y 'inconsistent))
	 (cond ((null flag)
		(return nil))
	       (t
		(return (cxerfarg (rzero) expg n a risch-ratform risch-intvar risch-mainvar)))))
     (setq k 0)
     (setq lcm 0)
     (setq risch-y (cdr risch-y))
   l3
     (setq lcm
	   (r+ (r* (car risch-y) (pexpt (list risch-mainvar 1 1) k))
	       lcm))
     (incf k)
     (setq risch-y (cdr risch-y))
     (cond ((null risch-y)
	    (return (cond ((null flag)
			   (ratqu lcm p))
			  (t
			   (list (r* (ratqu lcm p)
				     (cons (list expg n 1) 1))
				 0))))))
     (go l3)
   down2
     (cond ((> (1- risch-beta) risch-gamma)
	    (setq k (+ risch-alphar (1- risch-beta)))
	    (setq denom #'(lambda ()
			    (ratti risch-alphar (polcoef r risch-beta risch-var) t))))
	   ((< (1- risch-beta) risch-gamma)
	    (setq k (+ risch-alphar risch-gamma))
	    (setq denom #'(lambda ()
			    (polcoef s risch-gamma risch-var))))
	   (t
	    (setq k (+ risch-alphar risch-gamma))
	    (setq denom
		  #'(lambda ()
		      (ratpl (ratti risch-alphar (polcoef r risch-beta risch-var) t)
			     (polcoef s risch-gamma risch-var))))))
     (setq risch-y 0)
   loop
     (setq yn (polcoef (ratnumerator tt) k risch-var)
	   yd (r* (ratdenominator tt)	;DENOM MAY BE 0
		  (cond ((zerop risch-alphar)
			 (polcoef s risch-gamma risch-var))
			(t
			 (funcall denom)))))
     (cond ((rzerop yd)
	    (cond ((pzerop yn)
		   (setq k (1- k) risch-alphar (1- risch-alphar))
		   (go loop))		;need more constraints?
		  (t
		   (cond
		     ((null flag)
		      (return nil))
		     (t
		      (return (cxerfarg (rzero) expg n a risch-ratform risch-intvar risch-mainvar)))))))
	   (t
	    (setq yalpha (ratqu yn yd))))
     (setq ytemp (r+ risch-y (r* yalpha
				 (cons (list risch-mainvar risch-alphar 1) 1) )))
     (setq ttemp (r- tt (r* yalpha
			    (r+ (r* s (cons (list risch-mainvar risch-alphar 1) 1))
				(r* r risch-alphar
				    (list risch-mainvar (1- risch-alphar) 1))))))
     (decf k)
     (decf risch-alphar)
     (cond ((< risch-alphar 0)
	    (cond
	      ((rzerop ttemp)
	       (cond
		 ((null flag)
		  (return (ratqu ytemp p)))
		 (t
		  (return (list (ratqu (r* ytemp (cons (list expg n 1) 1))
				       p)
				0)))))
	      ((null flag)
	       (return nil))
	      ((and (risch-constp (setq ttemp (ratqu ttemp lcm)) risch-mainvar)
		    $erfflag
		    (equal (pdegree (car (get expg 'rischarg)) risch-mainvar) 2)
		    (equal (pdegree (cdr (get expg 'rischarg)) risch-mainvar) 0))
	       (return (list (ratqu (r* ytemp (cons (list expg n 1) 1)) p)
			     (erfarg2 (r* n (get expg 'rischarg))
				      ttemp risch-ratform risch-intvar risch-mainvar))))
	      (t
	       (return
		 (cxerfarg
		  (ratqu (r* risch-y (cons (list expg n 1) 1)) p)
		  expg
		  n
		  (ratqu tt lcm)
		  risch-ratform
		  risch-intvar
		  risch-mainvar))))))
     (setq risch-y ytemp)
     (setq tt ttemp)
     (go loop)))


;; *JM should be declared as an array, although it is not created
;; by this file. -- cwh

(defun lsa (mm)
  (prog (d *mosesflag m2 risch-m)
     (setq d (length (car mm)))
     ;; MTOA stands for MATRIX-TO-ARRAY.  An array is created and
     ;; associated functionally with the symbol *JM.  The elements
     ;; of the array are initialized from the matrix MM.
     (mtoa '*jm* (length mm) d mm)
     (setq risch-m (tfgeli '*jm*  (length mm) d))
     (cond ((or (and (null (car risch-m)) (null (cadr risch-m)))
		(and (car risch-m)
		     (> (length (car risch-m)) (- (length mm) (1- d)))))
	    (return (values 'singular risch-m)))
	   ((cadr risch-m) (return (values 'inconsistent risch-m))))
     (setq *mosesflag t)
     (ptorat '*jm* (1- d) d)
     (setq m2 (xrutout '*jm* (1- d) d nil nil))
     (setq m2 (lsafix (cdr m2) (caddr risch-m)))
     (return (values m2 risch-m))))

(defun lsafix (l n)
  (declare (special *jm*))
  (do ((n n (cdr n))
       (l l (cdr l)))
      ((null l))
    (setf (aref *jm* 1 (car n)) (car l)))
  (do ((s (length l) (1- s))
       (ans))
      ((= s 0) (cons '(list) ans))
    (setq ans (cons (aref *jm* 1 s) ans))))


(defun findpr (alist flist risch-y risch-mainvar &aux (p 1) fterm)
  (let (risch-alphar)
    (do ((alist alist (cdr alist))) ((null alist))
      (setq fterm (findflist (cadar alist) flist))
      (if fterm (setq flist (remove risch-y flist :count 1 :test #'eq)))
      (setq risch-alphar
	    (cond ((null fterm) (caddar alist))
		  ((equal (caddr fterm) 1)
		   (fpr-dif (car flist) (caddar alist) risch-mainvar))
		  (t (max (- (caddar alist) (caddr fterm)) 0))))
      (if (not (zerop risch-alphar))
	  (setq p (ptimes p (pexpt (cadar alist) risch-alphar)))))
    (do ((flist flist (cdr flist)))
	((null flist))
      (when (equal (caddar flist) 1)
	(setq risch-alphar (fpr-dif (car flist) 0 risch-mainvar))
	(setq p (ptimes p (pexpt (cadar flist) risch-alphar)))))
    (values p risch-alphar)))

(defun fpr-dif (fterm alpha risch-mainvar)
  (destructuring-let* (((num den mult) fterm)
		       (risch-m (spderivative den risch-mainvar))
		       (n))
    (cond ((rzerop risch-m) alpha)
	  (t (setq n (ratqu (cdr (ratdivide num den))
			    risch-m))
	     (if (and (equal (cdr n) 1) (numberp (car n)))
		 (max (car n) alpha)
		 alpha)))))

(defun findflist (a llist)
  (cond ((null llist) nil)
	((equal (cadar llist) a) (car llist))
	(t (findflist a (cdr llist)))))


(defun rischexplog (expexpflag flag f a l
		    risch-ratform risch-intvar risch-liflag risch-degree risch-y risch-var risch-mainvar)
  (prog (lcm yy risch-m p risch-alphar risch-gamma delta
	 mu r s tt denom ymu rbeta expg n eta logeta logdiff
	 temp risch-cary risch-nogood vector aarray rmu rrmu rarray
	 risch-beta
	 risch-lians)
     (desetq (expg n eta logeta logdiff) l)
     (cond ((or (pzerop a)
		(pzerop (car a)))
	    (return (cond ((null flag)
			   (rzero))
			  (t
			   (rischzero))))))
     (multiple-value-setq (p risch-alphar)
       (findpr (cdr (partfrac a risch-var))
	       (cdr (partfrac f risch-var))
	       risch-y
	       risch-mainvar))
     (setq lcm (plcm (ratdenominator a) p))
     (setq risch-y (ratpl (spderivative (cons 1 p) risch-mainvar)
			  (ratqu f p)))
     (setq lcm (plcm lcm (ratdenominator risch-y)))
     (setq r (car (ratqu lcm p)))
     (setq s (car (r* lcm risch-y)))
     (setq tt (car (r* a lcm)))
     (setq risch-beta (pdegree r risch-var))
     (setq risch-gamma (pdegree s risch-var))
     (setq delta (pdegree tt risch-var))
     (cond (expexpflag
	    (setq mu (max (- delta risch-beta)
			  (- delta risch-gamma)))
	    (go expcase)))
     (setq mu (max (- (1+ delta) risch-beta)
		   (- (1+ delta) risch-gamma)))
     (cond ((< risch-beta risch-gamma)
	    (go back))
	   ((= (1- risch-beta) risch-gamma)
	    (go down1)))
     (setq risch-y (tryrisch1 (ratqu (r- (r* (polcoef r (1- risch-beta) risch-var)
					     (polcoef s risch-gamma risch-var))
					 (r* (polcoef r risch-beta risch-var)
					     (polcoef s (1- risch-gamma) risch-var)))
				     (r* (polcoef r risch-beta risch-var)
					 (polcoef r risch-beta risch-var) ))
			      risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree))
     (setq risch-cary (car risch-y))
     (multiple-value-setq (yy risch-cary risch-nogood risch-lians)
       (getfncoeff (cdr risch-y)
		   (get risch-var 'rischexpr)
		   risch-intvar risch-liflag risch-degree risch-cary risch-nogood
		   risch-lians))
     (cond ((and (not (findint (cdr risch-y)))
		 (not risch-nogood)
		 (not (atom yy))
		 (equal (cdr yy) 1)
		 (numberp (car yy))
		 (> (car yy) mu))
	    (setq mu (car yy))))
     (go back)
   expcase
     (cond ((not (equal risch-beta risch-gamma))
	    (go back)))
     (setq risch-y (tryrisch1 (ratqu (polcoef s risch-gamma risch-var) (polcoef r risch-beta risch-var))
			      risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree))
     (cond ((findint (cdr risch-y))
	    (go back)))
     (setq yy (ratqu (r* -1 (car risch-y)) eta))
     (cond ((and (equal (cdr yy) 1)
		 (numberp (car yy))
		 (> (car yy) mu))
	    (setq mu (car yy))))
     (go back)
   down1
     (setq risch-y (tryrisch1 (ratqu (polcoef s risch-gamma risch-var) (polcoef r risch-beta risch-var))
			      risch-mainvar risch-ratform risch-intvar risch-liflag risch-degree))
     (setq risch-cary (car risch-y))
     (multiple-value-setq (yy risch-cary risch-nogood risch-lians)
       (getfncoeff (cdr risch-y)
		   (get risch-var 'rischexpr)
		   risch-intvar risch-liflag risch-degree risch-cary risch-nogood
		   risch-lians))
     (cond ((and (not (findint (cdr risch-y)))
		 (not risch-nogood)
		 (equal (cdr yy) 1)
		 (numberp (car yy))
		 (> (- (car yy)) mu))
	    (setq mu (- (car yy)))))
   back
     (if (minusp mu)
	 (return (if flag
		     (cxerfarg (rzero) expg n a risch-ratform risch-intvar risch-mainvar)
		     nil)))
     (cond ((> risch-beta risch-gamma)
	    (go lsacall))
	   ((= risch-beta risch-gamma)
	    (go recurse)))
     (setq denom (polcoef s risch-gamma risch-var))
     (setq risch-y '(0 . 1))
   linearloop
     (setq ymu (ratqu (polcoef (ratnumerator tt) (+ mu risch-gamma) risch-var)
		      (r* (ratdenominator tt) denom)))
     (setq risch-y (r+ risch-y (setq ymu (r* ymu (pexpt (list logeta 1 1) mu) ))))
     (setq tt (r- tt
		  (r* s ymu)
		  (r* r (spderivative ymu risch-mainvar))))
     (decf mu)
     (cond  ((not (< mu 0))
	     (go linearloop))
	    ((not flag)
	     (return (if (rzerop tt)
			 (ratqu risch-y p)
			 nil)))
	    ((rzerop tt)
	     (return (cons (ratqu (r* risch-y (cons (list expg n 1) 1)) p) '(0))))
	    (t
	     (return (cxerfarg (ratqu (r* risch-y (cons (list expg n 1) 1)) p)
			       expg
			       n
			       (ratqu tt lcm)
			       risch-ratform
			       risch-intvar
			       risch-mainvar))))
   recurse
     (setq rbeta (polcoef r risch-beta risch-var))
     (setq risch-y '(0 . 1))
   recurseloop
     (setq f (r+ (ratqu (polcoef s risch-gamma risch-var) rbeta)
		 (if expexpflag
		     (r* mu (spderivative eta risch-mainvar))
		     0)))
     (setq ymu (exppolycontrol nil
			       f
			       (ratqu (polcoef (ratnumerator tt)
					       (+ risch-beta mu)
					       risch-var)
				      (r* (ratdenominator tt) rbeta))
			       expg
			       n
			       risch-ratform
			       risch-intvar
			       risch-liflag
			       risch-degree
			       risch-mainvar))
     (when (null ymu)
       (return (cond ((null flag)
		      nil)
		     (t
		      (return (cxerfarg (ratqu (r* risch-y (cons (list expg n 1) 1)) p)
					expg n (ratqu tt lcm)
					risch-ratform risch-intvar risch-mainvar))))))
     (setq risch-y (r+ risch-y (setq ymu (r* ymu (pexpt (list logeta 1 1) mu)))))
     (setq tt (r- tt
		  (r* s ymu)
		  (r* r (spderivative ymu risch-mainvar))))
     (decf mu)
     (cond
       ((not (< mu 0))
	(go recurseloop))
       ((not flag)
	(return (cond ((rzerop tt)
		       (ratqu risch-y p))
		      (t nil))))
       ((rzerop tt)
	(return (cons (ratqu (r* risch-y (cons (list expg n 1) 1)) p)
		      '(0))))
       (t
	(return (cxerfarg (ratqu (r* risch-y (cons (list expg n 1) 1)) p)
			  expg
			  n
			  (ratqu tt lcm)
			  risch-ratform
			  risch-intvar
			  risch-mainvar))))
   lsacall
     (setq rrmu mu)
   muloop
     (setq temp (r* (ratexpt (cons (list logeta 1 1) 1) (1- mu))
		    (r+ (r* s (cons (list logeta 1 1) 1))
			(r* mu r logdiff ))))
   mu1
     (setq vector nil)
     (setq rmu (+ rrmu risch-beta))
   rmuloop
     (setq vector (cons (ratqu (polcoef (ratnumerator temp) rmu risch-var)
			       (ratdenominator temp)) vector))
     (decf rmu)
     (unless (< rmu 0)
       (go rmuloop))
     (decf mu)
     (setq aarray (append aarray (list (reverse vector))))
     (cond ((not (< mu 0))
	    (go muloop))
	   ((equal mu -2)
	    (go skipmu)))
     (setq temp tt)
     (go mu1)
   skipmu
     (setq rarray nil)
   arrayloop
     (setq vector nil)
     (setq vector (mapcar 'car aarray))
     (setq aarray (mapcar 'cdr aarray))
     (setq rarray (append rarray (list vector)))
     (unless (null (car aarray)) (go arrayloop))
     (setq rmu (1+ rrmu))
     (setq vector nil)
   array1loop
     (setq vector (cons '(0 . 1) vector))
     (decf rmu)
     (unless (< rmu 0) (go array1loop))
     (setq aarray nil)
   array2loop
     (cond ((equal (car rarray) vector)
	    nil)
	   (t
	    (setq aarray (cons (car rarray) aarray))))
     (setq rarray (cdr rarray))
     (when rarray (go array2loop))
     (setq rarray (reverse aarray))
     (multiple-value-setq (temp risch-m)
       (lsa rarray))
     (when (or (eq temp 'singular)
	       (eq temp 'inconsistent))
       (return (if (null flag)
		   nil
		   (cxerfarg (rzero) expg n a risch-ratform risch-intvar risch-mainvar))))
     (setq temp (reverse  (cdr temp)))
     (setq rmu 0)
     (setq risch-y 0)
   l3
     (setq risch-y (r+ risch-y (r* (car temp) (pexpt (list logeta 1 1) rmu))))
     (setq temp (cdr temp))
     (incf rmu)
     (unless (> rmu rrmu)
       (go l3))
     (return (if (null flag)
		 (ratqu risch-y p)
		 (cons (r* (list expg n 1) (ratqu risch-y p)) '(0))))))


(defun erfarg (exparg coef risch-ratform risch-mainvar)
  (prog (num denom erfarg)
     (setq exparg (r- exparg))
     (unless (and (setq num (pnthrootp (ratnumerator exparg) 2))
		  (setq denom (pnthrootp (ratdenominator exparg) 2)))
       (return nil))
     (setq erfarg (cons num denom))
     (if (risch-constp
	  (setq coef (ratqu coef (spderivative erfarg risch-mainvar)))
	  risch-mainvar)
	 (return (simplify `((mtimes) ((rat) 1 2)
			     ((mexpt) $%pi ((rat) 1 2))
			     ,(disrep coef risch-ratform)
			     ((%erf) ,(disrep erfarg risch-ratform))))))))

(defun erfarg2 (exparg coeff risch-ratform risch-intvar risch-mainvar)
  (let ((risch-var risch-mainvar)
	a b c d)
    (when (and (= (pdegree (car exparg) risch-var) 2)
	       (eq (caar exparg) risch-var)
	       (risch-pconstp (cdr exparg) risch-mainvar)
	       (risch-constp coeff risch-mainvar))
      (setq a (ratqu (r* -1 (caddar exparg))
		     (cdr exparg)))
      (setq b (disrep (ratqu (r* -1 (polcoef (car exparg) 1 risch-var))
			     (cdr exparg))
		      risch-ratform))
      (setq c (disrep (ratqu (r* (polcoef (car exparg) 0 risch-var))
			     (cdr exparg))
		      risch-ratform))
      (setq d (ratsqrt a risch-ratform))
      (setq a (disrep a risch-ratform))
      (simplify `((mtimes)
		  ((mtimes)
		   ((mexpt) $%e ((mplus) ,c
				 ((mquotient) ((mexpt) ,b 2)
				  ((mtimes) 4 ,a))))
		   ((rat) 1 2)
		   ,(disrep coeff risch-ratform)
		   ((mexpt) ,d -1)
		   ((mexpt) $%pi ((rat) 1 2)))
		  ((%erf) ((mplus)
			   ((mtimes) ,d ,risch-intvar)
			   ((mtimes) ,b ((rat) 1 2) ((mexpt) ,d -1)))))))))


(defun cxerfarg (ans expg n numdenom risch-ratform risch-intvar risch-mainvar
		 &aux (arg (r* n (get expg 'rischarg)))
		   (fails 0))
  (prog (denom erfans num nerf)
     (desetq (num . denom) numdenom)
     (unless $erfflag
       (setq fails num)
       (go lose))
     (if (setq erfans (erfarg arg numdenom risch-ratform risch-mainvar))
	 (return (list ans erfans)))
   again
     (when (and (not (pcoefp denom))
		(null (p-red denom))
		(eq (get (car denom) 'leadop) 'mexpt))
       (setq arg (r+ arg (r* (- (p-le denom))
			     (get (p-var denom) 'rischarg)))
	     denom (p-lc denom))
       (go again))
     (loop for (coef exparg exppoly) in (explist num arg 1)
	   do (setq coef (ratqu coef denom)
		    nerf (or (erfarg2 exparg coef risch-ratform risch-intvar risch-mainvar)
			     (erfarg exparg coef risch-ratform risch-mainvar)))
	      (if nerf
		  (push nerf erfans)
		  (setq fails (pplus fails exppoly))))
   lose
     (return
       (if (pzerop fails)
	   (cons ans erfans)
	   (rischadd (cons ans erfans)
		     (rischnoun (r* (ratexpt (cons (make-poly expg) 1) n)
				    (ratqu fails (cdr numdenom)))
				risch-ratform
				risch-intvar))))))

(defun explist (p oarg exps)
  (cond ((or (pcoefp p)
	     (not (eq 'mexpt
		      (get (p-var p) 'leadop))))
	 (list (list p oarg (ptimes p exps))))
	(t
	 (loop with narg = (get (p-var p) 'rischarg)
	       for (exp coef) on (p-terms p) by #'cddr
	       nconc (explist coef
			      (r+ oarg (r* exp narg))
			      (ptimes exps
				      (make-poly (p-var p) exp 1)))))))


(defun intsetup (exp risch-*var)
  (prog (varlist clist $factorflag dlist genpairs old risch-y z $ratfac $keepfloat
	 *fnewvarsw)
   y
     (setq exp (radcan1 exp risch-*var))
     (fnewvar exp)
     (setq *fnewvarsw t)
   a
     (setq clist nil)
     (setq dlist nil)
     (setq z varlist)
   up
     (setq risch-y (pop z))
     (cond ((freeof risch-*var risch-y)
	    (push risch-y clist))
	   ((eq risch-y risch-*var)
	    nil)
	   ((and (mexptp risch-y)
		 (not (eq (cadr risch-y) '$%e)))
	    (cond ((not (freeof risch-*var (caddr risch-y)))
		   (setq dlist `((mexpt simp)
				 $%e
				 ,(mul2* (caddr risch-y)
					 `((%log) ,(cadr risch-y)))))
		   (setq exp (maxima-substitute dlist risch-y exp))
		   (setq varlist nil)
		   (go y))
		  ((atom (caddr risch-y))
		   (cond ((numberp (caddr risch-y))
			  (push risch-y dlist))
			 (t
			  ;;(setq operator t)
			  (return (values nil t)))))
		  (t
		   (push risch-y dlist))))
	   (t
	    (push risch-y dlist)))
     (if z
	 (go up))
     (if (member '$%i clist :test #'eq)
	 (setq clist (cons '$%i (delete '$%i clist :test #'equal))))
     (setq varlist (append clist
			   (cons risch-*var
				 (nreverse (sort (append dlist nil)
						 #'(lambda (a b)
						     (intgreat a b risch-*var)))))))
     (orderpointer varlist)
     (setq old varlist)
     (mapc #'(lambda (b)
	       (intset1 b risch-*var))
	   (cons risch-*var dlist))
     (cond ((alike old varlist)
	    (return (values (ratrep* exp)
			    nil)))
	   (t (go a)))))

(defun leadop (exp)
  (cond ((atom exp)
	 exp)
	((mqapplyp exp)
	 (cadr exp))
	(t
	 (caar exp))))

(defun leadarg (exp)
  (cond ((atom exp)
	 0)
	((and (mexptp exp) (eq (cadr exp) '$%e))
	 (caddr exp))
	((mqapplyp exp)
	 (car (subfunargs exp)))
	(t
	 (cadr exp))))

(defun intset1 (b risch-*var)
  (let (e c d)
    (fnewvar
     (setq d (if (mexptp b)		;needed for radicals
		 `((mtimes simp)
		   ,b
		   ,(radcan1 (sdiff (simplify (caddr b)) risch-*var)
			     risch-*var))
		 (radcan1 (sdiff (simplify b) risch-*var)
			  risch-*var))))
    (setq d (ratrep* d))
    (setq c (ratrep* (leadarg b)))
    (setq e (cdr (assoc b (pair varlist genvar) :test #'equal)))
    (putprop e (leadop b) 'leadop)
    (putprop e b 'rischexpr)
    (putprop e (cdr d) 'rischdiff)
    (putprop e (cdr c) 'rischarg)))

;; order of expressions for risch.
;; expressions containing erf and li last.
;; then order by size of expression to guarantee that
;; any subexpressions are considered smaller.
;; this relation should be transitive, since it is called by sort. 
(defun intgreat (a b risch-*var)
  (cond ((and (not (atom a))
	      (not (atom b)))
	 (cond ((and (not (freeof '%erf a))
		     (freeof '%erf b))
		t)
	       ((and (not (freeof '$li a))
		     (freeof '$li b))
		t)
	       ((and (freeof '$li a)
		     (not (freeof '$li b)))
		nil)
	       ((and (freeof '%erf a)
		     (not (freeof '%erf b)))
		nil)
	       ((> (conssize a) (conssize b))
		t)
	       ((< (conssize a) (conssize b))
		nil)
	       (t
		(great (resimplify (fixintgreat a risch-*var))
		       (resimplify (fixintgreat b risch-*var))))))
	(t
	 (great (resimplify (fixintgreat a risch-*var))
		(resimplify (fixintgreat b risch-*var))))))

(defun fixintgreat (a risch-*var)
  (subst '/_101x risch-*var a))
