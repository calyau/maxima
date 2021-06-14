;;; ALL GENTRAN FILES IN ONE


;*******************************************************************************
;*                                                                             *
;*  copyright (c) 1988 kent state univ.  kent, ohio 44242
;* modified for Maxima 01/15/2019 Michael D. Stern, Richard J. Fateman
;* and other unknown soldiers.                      *
;*                                                                             *
;*******************************************************************************


;;  ---------  ;;                 load
;;  gtload.l   ;;    gentran code generation package
;;  ---------  ;;              for vaxima


(in-package :maxima)

(declare-top (special *c *cr* *currin* *currout* *endofloopstack* *eof*
        *fortran *gendecs *instk* *lisparithexpops* *lispdefops* *lisplogexpops*
        *lispstmtgpops* *lispstmtops* *outchanl* *outstk* *ratfor *reswds*
        *slash* *stdin* *stdout* *symboltable* *vexptrm allnum comma* expty
        oincr onextexp tvname $ccurrind $clinelen $dblfloat $fortcurrind
        $fortlinelen $genfloat $genstmtincr $genstmtno $gentranlang $gentranopt
        $gentranparser $gentranseg $implicit $maxexpprintlen $optimvarname
        $ratcurrind $ratlinelen $tablen $tempvarname $tempvarnum $tempvartype
        $usefortcomplex $geninpath $genoutpath fnotn))


;; convmac text follows. garbage redefinition of stuff in lisp.
(defvar semicolon '|\;|)
(defvar sharpsign '|\#|)

    
(defmacro foreach (elt kw1 lst kw2 stmt)
;                                                                              ;
; (foreach elt   -->  (progn (mapc    (function (lambda (elt) stmt)) lst) nil) ;
;       in lst                                                                 ;
;       do stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->  (progn (map     (function (lambda (elt) stmt)) lst) nil) ;
;       on lst                                                                 ;
;       do stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->         (mapcar  (function (lambda (elt) stmt)) lst)      ;
;       in lst                                                                 ;
;  collect stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->         (maplist (function (lambda (elt) stmt)) lst)      ;
;       on lst                                                                 ;
;  collect stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->         (mapcan  (function (lambda (elt) stmt)) lst)      ;
;       in lst                                                                 ;
;     conc stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->         (mapcon  (function (lambda (elt) stmt)) lst)      ;
;       on lst                                                                 ;
;     conc stmt)                                                               ;
;                                                                              ;
  (let ((fcn (cdr (assoc kw2 (cdr (assoc kw1 '((in (do . mapc)
						       (collect . mapcar)
						       (conc . mapcan))
						   (on (do . map)
						       (collect . maplist)
						       (conc . mapcon)))))))))
       (cond ((member fcn '(mapc map))
	      `(progn (,fcn (function (lambda (,elt) ,stmt))
			    ,lst)
		      nil))
	     (t
	      `(,fcn (function (lambda (,elt) ,stmt))
		     ,lst)))))


(defmacro aconc (m1 m2)
;                                              ;
; (aconc lst elt)  -->  (nconc lst (list elt)) ;
;                                              ;
  `(nconc ,m1 (list ,m2)))

;; really naive stuff below. rjf's opinion..
(defun caaaddr (p) (caaar (cddr p)))

(defun CADADDR (x) (car(cdr(car(cdr(cdr x))))))

(defun CADDADDR (x) (car(cdr(cdr(car(cdr(cdr x)))))))

(defun CADDDDDDDR (x) (car(cdr(cdr(cdr(cdr(cdr(cdr(cdr x)))))))))
					;;8th item. initial one is (nth 0) so this, above, is (nth 7 x) 

(defun CADDDDDDR (x) (car(cdr(cdr(cdr(cdr(cdr(cdr x))))))))

(defun CADDDDDR (x) (car(cdr(cdr(cdr(cdr(cdr x)))))))

(defun CDADADDR (x) (cdr(car(cdr(car(cdr(cdr x)))))))

(defun CADDDDR (x) (car(cdr(cdr(cdr(cdr x))))))

(defun compress (m)
;                                    ;
; (compress lst)  -->  (implode lst) ;
;                                    ;
  (coerce m 'string))

(defun append1 (x y)
   (append x (cons y ())))

(defmacro delete1 (e lst)
;                                            ;
; (delete1 elt lst)  -->  (delete elt lst 1) ;
;                                            ;
  `(delete ,e ,lst :count 1))

(defun explode2 (m)
;                                     ;
; (explode2 arg)  -->  (explodec arg) ;
;                                     ;
  (coerce (princ-to-string m) 'list))



(defmacro flag (&rest m)
;                                                   ;
; (flag varlst fname)  -->  (foreach v in varlst do ;
;                             (putprop v t fname))  ;
;                                                   ;
  `(loop for v in ,(cadr m) do ;; rjf fix?
     (putprop v t ,(caddr m))))


(defun flagp (var fname)
;                                         ;
; (flagp var fname)  -->  (get var fname) ;
;                                         ;
  (get var fname))


(defun geq (n1 n2)
;                              ;
; (geq n1 n2)  -->  (>= n1 n2) ;
;                              ;
  (>= n1 n2))


(defun idp (m)
;                               ;
; (idp exp)  -->  (symbolp exp) ;
;                               ;
  (symbolp m))


(defun mkfil (m)
;                                     ;
; (mkfil arg)  -->  (stripdollar arg) ;
;                                     ;
;;;  (cons 'stripdollar m)) --mds
        (stripdollar m))

(defun posn ()
;                       ;
; (posn)  -->  (nwritn) ;
;                       ;
  #+clisp (SYS::LINE-POSITION)
  #+gcl (si::file-column *standard-output*)
  #+cmu (lisp::charpos *standard-output*)
  #+sbcl (sb-impl::charpos))



(defmacro prettyprint (m)
;                                                      ;
; (prettyprint exp)  -->  (prog1 ($prpr exp) (terpri)) ;
;                                                      ;
  `(prog1 (linear-displa ,m) (terpri)))


(defun put (id proptype propval)
;                                                               ;
					; (put id proptype propval)  -->  (putprop id propval proptype) ;
  
  (setf (get id proptype) propval) ;; rjf
;                                                               ;
  #+ignore (putprop id propval proptype)
  )



(defun rederr (m)
;                                ;
; (rederr msg)  -->  (error msg) ;
;                                ;
  (cons 'error m))


(defmacro remflag (varlst fname)
;                                                      ;
; (remflag varlst fname)  -->  (foreach v in varlst do ;
;                                (remprop v fname))    ;
;                                                      ;
  `(foreach v in ,varlst do
     (remprop v ,fname)))



(defmacro repeat (stmt exp)
;                                                             ;
; (repeat stmt exp)  -->  (prog ()                            ;
;                               loop                          ;
;                               stmt                          ;
;                               (cond ((not exp) (go loop)))) ;
;                                                             ;
  `(prog ()
	 loop
	 ,stmt
	 (cond ((not ,exp) (go loop))))) ;; keep old definition due to evaluation order side effects -- mds

(defmacro spaces (m)
;                                       ;
; (spaces n)  -->  (do ((i n (sub1 i))) ;
;		       ((< i 1))    ;
;		       (princ " "))     ;
;                                       ;
  `(dotimes (i ,m) (princ " ")))




;;  -----------  ;;
;;  templt.l     ;;    template processing routines
;;  -----------  ;;



;;                               ;;
;;  1. text processing routines  ;;
;;                               ;;


;;  fortran  ;;


(defun procforttem ()
  (prog (c)
	(setq c (procfortcomm))
	(loop while (not (eq c '$eof$)) do  ;;changed, rjf
	       (cond ((eql c *cr*)
		      (progn (pprin2 *cr*)
			     (setq c (procfortcomm))))
                     ((eql c #\<)
		      (setq c (read-char (cdr *currin*) nil '$eof$))
		      (cond ((eql c #\<)
			     (setq c (procactive)))
			    (t
			     (pprin2 #\<)
			     (pprin2 c)
			     (setq c (read-char (cdr *currin*) nil '$eof$)))))

                      ((eq c #\>)
		      (setq c (read-char (cdr *currin*) nil '$eof$))
		      (cond ((eql c #\>)
			     (setq c '$eof$)) ;;-mds terminate file processing if >> found in "passive" section (consistent Macsyma 2.4)
			    (t
			     (pprin2 #\>)
			     (pprin2 c)
			     (setq c (read-char (cdr *currin*) nil '$eof$)))))

		     (t
		      (progn (pprin2 c)
			     (setq c (read-char (cdr *currin*) nil '$eof$))))))))



;;; The following now checks for EOF seen in a comment.  --mds courtesy of Macsyma
(defun procfortcomm ()
  ; <col 1>c ... <cr> ;
  ; <col 1>C ... <cr> ;
  ; <col 1>* ... <cr> ;
  (do ((c))
      ((not (member (setq c (read-char (cdr *currin*) nil '$eof$))
		    '(#\c #\C #\*)))
       c)
      (pprin2 c)
      (loop (setq c (read-char (cdr *currin*) nil '$eof$))
	    (cond ((eql c #\Newline) (pprin2 c) (return nil))
		  ((eq c '$eof$) (pprin2 #\Newline) (return c))
		  (t (pprin2 c))))))


;;  ratfor  ;;


(defun procrattem () ;;; use character objects --mds. gentranlang:ratfor has not been extensively tested
  (prog (c)
	(setq c (read-char (cdr *currin*) nil '$eof$))
	(loop while (not (eq c '$eof$)) do
	       (cond ((eq c sharpsign)
		      (setq c (procratcomm)))
                     ((eql c #\<)
		      (setq c (read-char (cdr *currin*) nil '$eof$))
		      (cond ((eql c #\<)
			     (setq c (procactive)))
			    (t
			     (pprin2 '<)
			     (pprin2 c)
			     (setq c (read-char (cdr *currin*) nil '$eof$)))))
                      ((eq c #\>)
		      (setq c (read-char (cdr *currin*) nil '$eof$))
		      (cond ((eq c #\>)
			     (setq c '$eof$))
			    (t
			     (pprin2 #\>)
			     (pprin2 c)
			     (setq c (read-char (cdr *currin*) nil '$eof$)))))
		     (t
		      (progn (pprin2 c)
			     (setq c (read-char (cdr *currin*) nil '$eof$))))))))

(defun procratcomm ()
  ; # ... <cr> ;
  (prog (c)
	(pprin2 sharpsign)
	(loop while (not (eql (setq c (read-char (cdr *currin*) nil '$eof$)) *cr*)) do
	       (pprin2 c))
	(pprin2 *cr*)
	(return (read-char (cdr *currin*) nil '$eof$))))


;;  c  ;;


(defun procctem ()  ;;; use character objects --mds
  (prog (c)
	(setq c (read-char (cdr *currin*) nil '$eof$))
	(loop while (not (eq c '$eof$)) do
	       (cond ((eql c *slash*)
		      (setq c (procccomm)))
                     ((eql c #\<)
		      (setq c (read-char (cdr *currin*) nil '$eof$))
		      (cond ((eql c #\<)
			     (setq c (procactive)))
			    (t
			     (pprin2 #\<)
			     (pprin2 c)
			     (setq c (read-char (cdr *currin*) nil '$eof$)))))

                     ((eql c #\>)
		      (setq c (read-char (cdr *currin*) nil '$eof$))
		      (cond ((eql c #\>)
			     (setq c '$eof$))
			    (t
			     (pprin2 #\>)
			     (pprin2 c)
			     (setq c (read-char (cdr *currin*) nil '$eof$)))))
		     (t
		      (progn (pprin2 c)
			     (setq c (read-char (cdr *currin*) nil '$eof$))))))))

(defun procccomm ()  ;;; use character objects --mds
  ; /* ... */ ;
  (prog (c)
	(pprin2 *slash*)
	(setq c (read-char (cdr *currin*) nil '$eof$))
	(cond ((eql c #\*)
	       (progn (pprin2 c)
		      (setq c (read-char (cdr *currin*) nil '$eof$))
		      (repeat (progn (loop while (not (eql c #\*)) do
					    (progn (pprin2 c)
						   (setq c (read-char (cdr *currin*) nil '$eof$))))
				     (pprin2 c)
				     (setq c (read-char (cdr *currin*) nil '$eof$)))
			      (eql c *slash*))
		      (pprin2 c)
		      (setq c (read-char (cdr *currin*) nil '$eof$)))))
	(return c)))


;;                                        ;;
;;  2. template file active part handler  ;;
;;                                        ;;


(defun procactive ()
  ;  procactive reads vaxima expressions and statements inside "<<" and ">>"  ;
  ;  and mevals each.                                                         ;
  (prog (vexp vexptrm c)
   loop (setq vexp ($readvexp *currin*))
	(setq vexptrm *vexptrm)
	(meval vexp)
	(cond ((member vexptrm '(#\NULL #\>))
	       (return (cond ((equal (setq c (read-char (cdr *currin*) nil '$eof$)) *cr*)
			      (read-char (cdr *currin*) nil '$eof$))
			     (c)))))
	(go loop)))

(defun $readvexp (in)   ;; parsing now done by mread --mds
  (prog (test iport)
      (setq iport (cdr in))
      (setq test (peek-char t iport))
      (cond
	      ((equal test #\>)
                (cond
                        ((and (equal (tyi iport) #\>) (equal (tyi iport) #\>))
	                (setq *vexptrm test)
                        (return nil))
                        (t (gentranerr 'e nil "single > after active statement" nil))))

	      ((member test '(#\; #\$ #\NULL))
	       (setq *vexptrm test)))
               (setq *vexptrm #\$)
  	      (setq test (let (*prompt-on-read-hang*) (mread iport nil)))
              (return (third test))))




;;  ---------  ;;
;;  init.l     ;;    declarations & initializations
;;  ---------  ;;


;;                                                                           ;;
;;  1. user-accessible commands, functions, operators, switches & variables  ;;
;;                                                                           ;;


;;  gentran commands  ;;

;; The following will be declared with defmfun instead.
;(declare (nlambda $gentran $gentranin $gentranout $gentranshut
;			  $gentranpush $gentranpop $gentran_on $gentran_off)) $on, $off renamed consistent with Macsyma --mds

;;  gentran functions  ;;

;;  gentran operators  ;;

;;  user-accessible primitive functions  ;;


;;  mode switches  ;;



(setq *fortran nil)
(setq *ratfor  nil)
(setq *c       nil)
(setq *gendecs t)

(put 'fortran 'simpfg '((nil) (t (gentranswitch 'fortran))))
(put 'ratfor  'simpfg '((nil) (t (gentranswitch 'ratfor))))
(put 'c       'simpfg '((nil) (t (gentranswitch 'c))))
(put 'gendecs 'simpfg '((nil) (t (gendecs nil))))
;;  flags  ;;





;;  user-accessible global variables  ;;
(setq $genfloat      nil)
(setq $dblfloat      nil)
(setq $usefortcomplex nil)
(setq $gentranopt    nil)
(setq $gentranseg    t)
(setq $gentranparser nil)
(setq $gentranlang    'fortran)
(setq $maxexpprintlen 800)
(setq $tempvarname    '$t)
(setq $optimvarname     '$u)
(setq $tempvarnum     0)
(setq $tempvartype    nil)
(setq $implicit    nil)
(setq $genstmtno      25000)
(setq $genstmtincr    1)
(setq $fortcurrind    6)
(setq $ratcurrind     0)
(setq $ccurrind       0)
(setq $tablen         4)
(setq $fortlinelen    72)
(setq $ratlinelen     80)
(setq $clinelen       80)
(setq $geninpath nil)
(setq $genoutpath nil)



;; these functions enable input/output filepath selection --mds
(defun concat2 (str1 str2)
        (compress (append (exploden str1) (exploden str2))))

(defun outpath (fname)
        (if (and $genoutpath (not (member fname '($all t nil))))
                (concat2 $genoutpath fname)
                fname))

(defun fsearch (inf)
(if $geninpath
        ($file_search inf $geninpath)
        ($file_search inf)))


;;                                                   ;;
;;  2. system variables, operators & property lists  ;;
;;                                                   ;;


;;  global variables  ;;



(setq *stdin*   (cons t *standard-input*))
(setq *instk*           (list *stdin*))
(setq *currin*          (car *instk*))
(setq *stdout*  (cons t *standard-output*))
(setq *outstk*          (list *stdout*))
(setq *currout*         (car *outstk*))
(setq *outchanl*        (list (cdr *currout*)))
(setq *symboltable*     (list '*main*))
(setq *endofloopstack*  ())
(setq *lisparithexpops* (list 'expt 'minus 'plus 'quotient 'times))
(setq *lisplogexpops*   (list 'and 'equal 'geqp 'greaterp 'leqp 'lessp 'not
			      'notequal 'or))
(setq *lispstmtops*     (list 'break 'cond 'do 'end 'go 'princ 'return 'setq
			      'stop))
(setq *lispstmtgpops*   (list 'prog 'progn))
(setq *lispdefops*      (list 'defun))
(setq *slash* (code-char 47))
(setq *cr*    (code-char 10))
(setq *eof*    (code-char 0))
(setq *reswds* '(lambda mand mcond mdefine mdo mdoin mequal mexpt mgeqp
			 mgo mgreaterp mleqp mlessp mlist mminus mnot mnotequal
			 mor mplus mprog mprogn mquotient mreturn msetq mtimes
			 rat $end $ev $false $matrix $print $readonly $stop))

;;  dummy operator  ;;


;;  property list values  ;;
(progn (defprop mor       or       franznotn)
	       (defprop mand      and      franznotn)
	       (defprop mnot      not      franznotn)
	       (defprop mgreaterp greaterp franznotn)
	       (defprop mequal    equal    franznotn)
	       (defprop mnotequal notequal franznotn)
	       (defprop mlessp    lessp    franznotn)
	       (defprop mgeqp     geqp     franznotn)
	       (defprop mleqp     leqp     franznotn)
	       (defprop mplus     plus     franznotn)
	       (defprop mtimes    times    franznotn)
	       (defprop mquotient quotient franznotn)
	       (defprop rat       quotient franznotn)
	       (defprop mexpt     expt     franznotn)
	       (defprop mminus    minus    franznotn)
	       (defprop mabs      abs      franznotn))




;;  -----------  ;;
;;  global.l     ;;    general functions
;;  -----------  ;;


;;                                                                   ;;
;;  1. temporary variable generation, marking & unmarking functions  ;;
;;                                                                   ;;


(defun tempvar (type)
  ;                                                           ;
  ; if type member '(nil 0) then type <- $tempvartype         ;
  ;                                                           ;
  ; if type neq 'nil and type neq 'unknown then               ;
  ;    var <- 1st unmarked tvar of vtype type or of vtype nil ;
  ;           which isn't in the symbol table                 ;
  ;    put type on var's vtype property list                  ;
  ;    put declaration in symbol table                        ;
  ; else if type = nil then                                   ;
  ;    var <- 1st unmarked tvar of type nil                   ;
  ;           which isn't in the symbol table                 ;
  ; else type = 'unknown                                      ;
  ;    var <- 1st unmarked tvar of type nil                   ;
  ;           which isn't in the symbol table                 ;
  ;    put 'unknown on var's vtype property list              ;
  ;    print warning - "undeclared"                           ;
  ;                                                           ;
  ; return var                                                ;
  ;                                                           ;
  (prog (tvar num)
(cond ($tempvartype (setq $tempvartype (stripdollar1 $tempvartype))))
	(cond ((member type '(nil 0)) (setq type $tempvartype)))
(cond (type (setq type (stripdollar1 type))))
(setq $tempvarname (stripdollar1 $tempvarname))
	(setq num $tempvarnum)
        (prog ()
        loop
        (setq tvar (implode (append (exploden $tempvarname)(explode num)))) 
        (cond ((or (markedvarp tvar) (not(member (getvartype tvar) (list type nil)))) (setq num (+ 1 num))(go loop))))
	(put tvar '*vtype* type)
	(cond ((equal type 'unknown)
	       (gentranerr 'w tvar "undeclared variable" nil))
	      (type
	       (symtabput nil tvar (list type))))
	(return tvar)))

(defun markvar (var)
  (cond ((numberp var) var)
	((atom var) 
	 (putprop var t '*marked*)
	 var) ;;was  (flag (list var) '*marked*) var)
	(t (loop for v in var do (markvar v)) var)))

(defun markedvarp (var)
  (flagp var '*marked*))

(defun unmarkvar (var)
  (cond ((numberp var) var)
	(t (remflag (list var) '*marked*))))

(defun recurunmark (exp)
  (cond ((atom exp) (unmarkvar exp))
	(t (foreach elt in exp do (recurunmark elt)))))


;;                                           ;;
;;  2. statement number generation function  ;;
;;                                           ;;

(defun genstmtno ()
  (incf $genstmtno $genstmtincr))

;;                                                             ;;
;;  3. symbol table insertion, retrieval & deletion functions  ;;
;;                                                             ;;


(defun symtabput (name type value)
  ;                                                                       ;
  ; call                                                inserts           ;
  ; (symtabput subprogname nil       nil              ) subprog name      ;
  ; (symtabput subprogname '*type*   subprogtype      ) subprogram type   ;
  ; (symtabput subprogname '*params* paramlist        ) parameter list    ;
  ; (symtabput subprogname vname     '(type d1 d2 ...)) type & dimensions ;
  ;                                                     for variable,     ;
  ;                                                     variable range,   ;
  ;     if subprogname=nil                              parameter, or     ;
  ;        then subprogname <- car symboltable          function name     ;
  ;                                                                       ;
  (progn
   (setq name (or name (car *symboltable*)))
   (setq *symboltable* (cons name (delete1 name *symboltable*)))
   (cond ((member type '(*type* *params*))
	  (put name type value))
	 (type
	  (prog (v vtype vdims dec decs)
		(setq v type)
		(setq vtype (car value))
		(setq vdims (cdr value))
		(setq decs (get name '*decs*))
		(setq dec (assoc v decs))
		(setq decs (delete1 dec decs))
		(setq vtype (or vtype (cond ((> (length dec) 1)
					     (cadr dec)))))
		(setq vdims (or vdims (cond ((> (length dec) 2)
					     (cddr dec)))))
		(setq dec (cons v (cons vtype vdims)))
		(put name '*decs* (aconc decs dec)))))))

(defun symtabget (name type)
  ;                                                                   ;
  ; call                              retrieves                       ;
  ; (symtabget nil         nil      ) all subprogram names            ;
  ; (symtabget subprogname '*type*  ) subprogram type                 ;
  ; (symtabget subprogname '*params*) parameter list                  ;
  ; (symtabget subprogname vname    ) type & dimensions for variable, ;
  ;                                   variable range, parameter, or   ;
  ;                                   function name                   ;
  ; (symtabget subprogname '*decs*  ) all types & dimensions          ;
  ;                                                                   ;
  ;     if subprogname=nil & 2nd arg is non-nil                       ;
  ;        then subprogname <- car symboltable                        ;
  ;                                                                   ;
  (progn
   (cond (type (setq name (or name (car *symboltable*)))))
   (cond ((null name) *symboltable*)
	 ((member type '(*type* *params* *decs*)) (get name type))
	 ((assoc type (get name '*decs*))))))

(defun symtabrem (name type)
  ;                                                                   ;
  ; call                              deletes                         ;
  ; (symtabrem subprogname nil      ) subprogram name                 ;
  ; (symtabrem subprogname '*type*  ) subprogram type                 ;
  ; (symtabrem subprogname '*params*) parameter list                  ;
  ; (symtabrem subprogname vname    ) type & dimensions for variable, ;
  ;                                   variable range, parameter, or   ;
  ;                                   function name                   ;
  ; (symtabrem subprogname '*decs*  ) all types & dimensions          ;
  ;                                                                   ;
  ;     if subprogname=nil                                            ;
  ;        then subprogname <- car symboltable                        ;
  ;                                                                   ;
  (progn
   (setq name (or name (car *symboltable*)))
   (cond ((null type)
	  (setq *symboltable* (or (delete1 name *symboltable*) '(*main*))))
	 ((member type '(*type* *params* *decs*))
	  (remprop name type))
	 (t (prog (v dec decs)
		  (setq v type)
		  (setq decs (get name '*decs*))
		  (setq dec (assoc v decs))
		  (setq decs (delete1 dec decs))
		  (put name '*decs* decs))))))

(defun getvartype (var)
  (prog (type)
	(cond ((listp var) (setq var (car var))))
	(setq type (symtabget nil var))
	(cond ((and type (> (length type) 1))
	       (setq type (cadr type)))
	      ((setq type nil)))

     ;; suppress implicit typing of tvars --mds
	(cond ((and $implicit (not (eq (stripdollar1 $gentranlang) 'c)) (atom var) (null type))
	       (setq type (imptype var))))
	(return type)))


;; A function to set gentranlang with checking. Can also set it directly from Maxima

(defun $setgentranlang(a)
	(setq a (stripdollar1 a))
	(cond ((not (member a '(fortran ratfor c) :test #'eq))
		(merror "arg must be one of fortran c or ratfor")))
	(setq $gentranlang a))

(defun imptype(var)
   (cond ((member (car (exploden var)) '(#\i #\j #\k #\l #\m #\n #\I #\J #\K #\L #\M #\N)) 'integer) ;; fixed old char's for implicit --mds
	 (t 'real)))

(defun arrayeltp (exp)
   (or (get (car exp) 'array) (> (length (symtabget nil (car exp))) 2))) ;;--mds display undeclared array elements with [...] in c.


;;                                                       ;;
;;  4. input & output file stack manipulation functions  ;;
;;                                                       ;;


(defun delinstk (pr)
  (progn
   (setq *instk* (or (delete1 pr *instk*) (list *stdin*)))
   (setq *currin* (car *instk*))))

(defun delstk (pr)
  ; remove all occurrences of filepair from output file stack ;
  (loop while (member pr (cdr (reverse *outstk*)))
	 do (popstk pr)))

(defun flisteqp (flist1 flist2)
  (progn
   (setq flist1 (foreach f in flist1 collect (mkfil f)))
   (foreach f in flist2 do (setq flist1 (delete1 (mkfil f) flist1)))
   (null flist1)))

(defun filpr (fname stk)
  ; retrieve fname's filepair from stack stk ;
  (cond ((null stk) nil)
	((and (caar stk) (equal (mkfil fname) (mkfil (caar stk))))
	 (car stk))
	((filpr fname (cdr stk)))))

(defun mkfilpr (fname)
  ; open output channel & return filepair (fname . chan#) ;
  (cons fname (if (streamp fname)
		  fname
		  (open fname :direction :output :if-exists :append :if-does-not-exist :create))))

(defun pfilpr (flist stk)
  ; retrieve flist's "parallel" filepair from stack stk ;
  (cond ((null stk) nil)
	((and (null (caar stk)) (flisteqp flist (cdar stk)))
	 (car stk))
	((pfilpr flist (cdr stk)))))

(defun popinstk ()
  (delinstk *currin*))

(defun popstk (pr)
  ; remove top-most occurrence of filepair from output file stack ;
  (cond ((car pr)
	 (resetstk (delete1 pr *outstk*)))
	(t
	 (prog (stk1 stk2)
	       (setq stk1 *outstk*)
	       (loop while (not (equal (car stk1) pr))
		      do (progn (setq stk2 (aconc stk2 (car stk1)))
				(setq stk1 (cdr stk1))))
	       (loop while (not (equal (car stk1) '(nil)))
		      do (setq stk1 (cdr stk1)))
	       (resetstk (append stk2 (cdr stk1)))))))

(defun pushinstk (pr)
  (progn (setq *instk* (cons pr *instk*))
	 (setq *currin* (car *instk*))))

(defun pushstk (pr)
  ; push filepair onto output file stack ;
  (progn (setq *outstk* (cons pr *outstk*))
	 (resetstkvars)))

(defun resetstk (stk)
  (prog (s)
	(cond (stk
	       (repeat (cond ((or (caar stk) (equal (car stk) '(nil)))
			      (setq s (aconc s (car stk))))
			     (t (progn
				 (foreach f in (cdar stk) do
					  (cond ((not (filpr f *outstk*))
						 (setq stk
						       (cons
							(delete1 f (car stk))
							(cdr stk))))))
				 (cond ((equal (car stk) '(nil))
					(setq stk (cdr stk)))
				       (t
					(setq s (aconc s (car stk))))))))
		       (null (setq stk (cdr stk))))))
	(setq *outstk* (or s (list *stdout*)))
	(resetstkvars)))

(defun resetstkvars ()
  ; reset current-output to filepair on top of output file stack, ;
  ; reset output channel list to channel #'s corresponding to     ;
  ;  name(s) in current-output                                    ;
  (progn
   (setq *currout* (car *outstk*))
   (setq *outchanl* (cond ((car *currout*) (list (cdr *currout*)))
			  (t (foreach f in (cdr *currout*) collect
				      (cdr (filpr f *outstk*))))))))


;;                                      ;;
;;  5. functions for making lisp forms  ;;
;;                                      ;;


(defun mkassign (var exp)
  (list 'setq var exp))

(defun mkcond (pairs)
  (cons 'cond pairs))

(defun mkdef (name params body)
  (append (list 'defun name params) body))

(defun mkdo (var exitcond body)
  (append (list 'do var exitcond) body))

(defun mkreturn (exp)
  (list 'return exp))

(defun mkstmtgp (vars stmts)
   (cond ((numberp vars) (cons 'progn stmts))
	 ((cons 'prog (cons vars stmts)))))

(defun mkterpri ()
  '(terpri))


;;                           ;;
;;  6. lisp form predicates  ;;
;;                           ;;


(defun lispassignp (stmt)
  (and (listp stmt)
       (equal (car stmt) 'setq)))

(defun lispbreakp (form)
  (equal (car form) 'break))

(defun lispcallp (form)
  (listp form))

(defun lispcondp (stmt)
  (and (listp stmt)
       (equal (car stmt) 'cond)))

(defun lispdefp (form)
  (and (listp form)
       (member (car form) *lispdefops*)))

(defun lispdop (stmt)
  (and (listp stmt)
       (equal (car stmt) 'do)))

(defun lispexpp (form)
  (or (atom form)
      (member (car form) (append *lisparithexpops* *lisplogexpops*))
      (not (member (car form) (append (append *lispstmtops* *lispstmtgpops*)
				      *lispdefops*)))))

(defun lispendp (form)
  (and (listp form)
       (equal (car form) 'end)))

(defun lispgop (form)
  (equal (car form) 'go))

(defun lisplabelp (form)
  (atom form))

(defun lisplogexpp (form)
  (or (atom form)
      (member (car form) *lisplogexpops*)
      (not (member (car form) (append (append *lisparithexpops* *lispstmtops*)
				      (append *lispstmtgpops* *lispdefops*))))))

(defun lispprintp (form)
  (equal (car form) 'princ))

(defun lispreadp (form)
  (and (equal (car form) 'setq)
       (listp (caddr form))
       (equal (caaddr form) 'read)))

(defun lispreturnp (stmt)
  (and (listp stmt)
       (equal (car stmt) 'return)))

(defun lispstmtp (form)
  (or (atom form)
      (member (car form) *lispstmtops*)
      (and (atom (car form))
	   (not (member (car form) (append
				    (append *lisparithexpops* *lisplogexpops*)
				    (append *lispstmtgpops* *lispdefops*)))))))

(defun lispstmtgpp (form)
  (and (listp form)
       (member (car form) *lispstmtgpops*)))

(defun lispstopp (form)
  (equal (car form) 'stop))


;;                      ;;
;;  7. type predicates  ;;
;;                      ;;


(defun gfunctionp (stmt name)
  ; does stmt contain an assignment which assigns a value to name? ;
  ; does it contain a (return exp) stmt?                           ;
  ;  i.e., (setq name exp) -or- (return exp)                       ;
  (cond ((or (null stmt) (atom stmt)) nil)
	((and (equal (car stmt) 'setq) (equal (cadr stmt) name)) t)
	((and (equal (car stmt) 'return) (cdr stmt)) t)
	((eval (cons 'or
		     (foreach st in stmt collect (gfunctionp st name)))))))

(defun implicitp (type)
  (prog (xtype ximp r)
  (cond ((stringp type)
	(setq xtype (exploden type))
	(setq ximp (exploden 'implicit)))
        (t
        (setq xtype (explode2 type))
	(setq ximp (explode2 'implicit))))
	(setq r t)
	(repeat (setq r (and r (equal (car xtype) (car ximp))))
		(or (null (setq xtype (cdr xtype)))
		    (null (setq ximp (cdr ximp)))))
	(return r)))
       

(defun inttypep (type)
  (cond ((member type '(integer int long short)))
	((prog (xtype xint r)
	       (setq xtype (exploden type))
	       (setq xint (exploden 'integer))
	       (setq r t)
	       (repeat (setq r (and r (equal (car xtype) (car xint))))
		       (or (null (setq xtype (cdr xtype)))
			   (null (setq xint (cdr xint)))))
	       (return r)))))


;;                      ;;
;;  8. misc. functions  ;;
;;                      ;;


(defun complexdop (dostmt)
  (and (lispdop dostmt)
       (or (> (length (cadr dostmt)) 1)
	   (> (length (caddr dostmt)) 1))))

(defun formtypelists (varlists)
  ; ( (var type d1 d2 ..)         ( (type (var d1 d2 ..) ..)   ;
  ;    .                     -->     .                         ;
  ;    .                             .                         ;
  ;   (var type d1 d2 ..) )         (type (var d1 d2 ..) ..) ) ;
  (prog (type typelists tl)
	(foreach vl in varlists do
		 (progn
		  (setq type (cadr vl))
		  (cond ((onep (length (setq vl (delete1 type vl))))
			 (setq vl (car vl))))
		  (cond ((setq tl (assoc type typelists))
			 (setq typelists (delete1 tl typelists)))
			(t
			 (setq tl (list type))))
		  (setq typelists (aconc typelists (aconc tl vl)))))
	(return typelists)))

(defun insertcommas (lst)
  (prog (result)
	(cond ((null lst) (return nil)))
	(setq result (list (car lst)))
	(loop while (setq lst (cdr lst))
	       do (setq result (cons (car lst)
				     (cons '|,| result))))
	(return (reverse result))))


(defun noerrmevalp (pred)
  ;mevalp without call to merror
  (let ((ans (mevalp1 pred)))
       (cond ((member ans '(t nil)) ans)
	     (t '$unknown))))

(defun simplifydo (dostmt)
  (prog (varlst exitlst stmtlst result tmp1 tmp2)
	(cond ((not (lispdop dostmt)) (return dostmt)))
	(setq varlst (reverse (cadr dostmt)))
	(setq exitlst (caddr dostmt))
	(setq stmtlst
	      (foreach st in (cdddr dostmt) collect (simplifydo st)))
	(setq result
	      (foreach st in (cdr exitlst) collect (simplifydo st)))
	(setq exitlst (list (car exitlst)))
	(foreach var in (cdr varlst) do
		 (progn
		  (setq tmp1 (cons (mkassign (car var) (cadr var)) tmp1))
		  (cond ((cddr var)
			 (setq tmp2
			       (cons (mkassign (car var) (caddr var)) tmp2))))))
	(setq varlst (list (car varlst)))
	(setq result (cons (mkdo varlst exitlst (append stmtlst tmp2)) result))
	(setq result (append tmp1 result))
	(return result)))

(defun seqtogp (lst)
  (cond ((or (null lst) (atom lst) (lispstmtp lst) (lispstmtgpp lst))
	 lst)
	((and (onep (length lst)) (listp (car lst)))
	 (seqtogp (car lst)))
	((mkstmtgp 0 (foreach st in lst collect (seqtogp st))))))


(defun stripdollar1 (x)
  (cond ((not (atom x))
	 (cond ((and (eq (caar x) 'bigfloat)
		     (not (minusp (cadr x))))
		(implode (fpformat x)))
	       (t
		(merror "atomic arg required" x))))
	((numberp x)
	 x)
	((member (char (string x) 0) '(#\$ #\% #\&))
	 (intern (subseq (string x) 1)))
	(t
	 x)))




;;  -----------  ;;
;;  output.l     ;;    code formatting & printing
;;  -----------  ;;        and error handler



;;                                        ;;
;;  code formatting & printing functions  ;;
;;                                        ;;

;;  princ with case inverted
(defun princ-invert-case (sym)
  (princ (print-invert-case sym)))

;;  fortran code formatting & printing functions  ;;

(defun formatfort (lst)
  (foreach c in *outchanl* do
     (let ((*standard-output* c))
       (formatfort1 lst))))

(defun formatfort1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt))) $fortlinelen)
			  (fortcontline)))
		   (princ-invert-case elt))))))

(defun fortcontline ()
  (progn
   (terpri)
   (princ "     .")
   (forttab (- $fortcurrind 6))
   (spaces 1)))

(defun forttab (n)
  (progn
   (setq $fortcurrind (min (+ n 6) (- $fortlinelen 40)))
   (spaces (- $fortcurrind (posn)))))

;;  ratfor code formatting & printing functions  ;;

(defun formatrat (lst)
  (foreach c in *outchanl* do
     (let ((*standard-output* c))
       (formatrat1 lst))))

(defun formatrat1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt)))
				    $ratlinelen)
			  (ratcontline)))
		   (princ-invert-case elt))))))

(defun ratcontline ()
  (progn
   (terpri)
   (rattab $ratcurrind)
   (spaces 1)))

(defun rattab (n)
  (progn
   (setq $ratcurrind (min n (- $ratlinelen 40)))
   (spaces (- $ratcurrind (posn)))))

;;  c code formatting & printing functions  ;;

(defun formatc (lst)
  (foreach c in *outchanl* do
     (let ((*standard-output* c))
       (formatc1 lst))))

(defun formatc1 (lst)
  (foreach elt in lst do
	   (cond ((listp elt)
		  (eval elt))
		 (t
		  (progn
		   (cond ((> (+ (posn) (length (explode2 elt)))
				    $clinelen)
			  (ccontline)))
		   (princ-invert-case elt))))))

(defun ccontline ()
  (progn
   (terpri)
   (ctab $ccurrind)
   (spaces 1)))

(defun ctab (n)
  (progn
   (setq $ccurrind (min n (- $clinelen 40)))
   (spaces (- $ccurrind (posn)))))


;;                             ;;
;;  general printing function  ;;
;;                             ;;


(defun pprin2 (arg)
  (if (eql arg *cr*)
      (foreach c in *outchanl* do (terpri c))
      (foreach c in *outchanl* do (princ arg c))))


;;                 ;;
;;  error handler  ;;
;;                 ;;


;;  error & warning message printing routine  ;;
(defun gentranerr( msgtype exp msg1 msg2)
  (if (eq msgtype 'e) ($error exp msg1 msg2) (mtell exp msg1 msg2)))





;;  -----------  ;;
;;  vaxlsp.l     ;;    lisp code generation module
;;  -----------  ;;

(defvar lefttype 'real)


;; genfloat is an option variable, if set to t to cause all constants to be floated
;; dblfloat is an option variable if set to t to cause all floats to convert or display as doubles --mds

;;                               ;;
;; 2. vaxima -> lisp translation ;;
;;                               ;;

(defun safe-car (x) (if (listp x) (car x) nil))
(defun safe-caar (x) (if (listp x) (car (safe-car x)) nil))

(defun franz (form)
  ; translate form from macsyma internal representation into franz lisp ;
  (foreach f in form collect
	   (cond ((member f '($begin_group $end_group))
		  f)
		 ((macexpp f)
		  (franzexp f 0 f))
		 (t
		  (franzstmt f)) )))


(defun franzexp (exp ind context)

  (setq allnum t ) ;;set flag to check if all numbers in an expression
  (cond ((atom exp)

	 (cond
	   ((and (numberp exp) (not $genfloat)(not(equal ind 3))(not (and (listp context) (listp (car context)) (equal (caar context) 'rat)))
		 exp)) ;; floats integers in denominator --mds
	   ((numberp exp)
		(cond ((equal ind 0)

		       (setq expty (exptype  context ))
		       (cond(allnum (setq expty lefttype)))
				;;solve all numbers in an expression

		       (cond ((eq expty 'integer)
			       exp)
			     ((eq expty 'real)
			      (float exp))
			     ((eq expty 'double)       ;"double" & "complex"
			      (coerce exp 'double-float))	       ;are for the time being
			     ((eq expty 'complex)
			      (gcomplex exp))
			     (t (float exp)) ))

		     ((equal ind 1)
                     (if (and (listp context) (listp (car context)) (equal (caar context) 'rat)) (float exp)
		       exp)) ;;;; floats integers in rational power --mds

		     ((equal ind 2)
		      (float exp))

		     ((equal ind 3)
		      (coerce exp 'double-float))

		     ((equal ind 4)
		      (gcomplex exp))))

	       ((char= (char (string exp) 0) #\&)
		(format nil "\"~A\"" (stripdollar1 exp)))
	       ((eq exp t) (cond ((eq (stripdollar1 $gentranlang) 'c) 1)
				 (t '| .true. |)))
	       (t
		(stripdollar1 exp))))

	((eq (caar exp) '$gquote) (cadr exp)) ;; gquote added by pwang 11/10/86
	((eq (caar exp) 'mtimes)
	 (simptimes1 (foreach term in (cdr exp) collect
			      (franzexp term ind exp))
		      ))

	((eq (caar exp) 'mexpt)
	 ;; ((mexpt) x -1) --> (quotient 1.0 x)                    ;
	 ;; ((mexpt) x ((mminus) i)) --> (quotient 1.0 (expt x i)) ;
	 ;; ((mexpt) $%e x) --> (exp x) ;rjf 10/14/2018
	 
	 (let ((var (cadr exp)) (pow (caddr exp)))
	   (cond ((eq var '$%e) (list 'exp (franzexp pow 3 context))) ;;rjf --mds double-float numbers in exponentials
	    
	    ((or (eql pow -1)
			 (and (listp pow)
			      (eq (caar pow) 'mminus)
			      (onep (cadr pow))))
		     (list 'quotient (franzexp 1 ind exp) (franzexp var ind exp)))
		    ((and (numberp pow) (minusp pow))
		     (list 'quotient
			   (franzexp 1 ind exp)
			   (list 'expt (franzexp var ind exp)
				 (franzexp (abs pow) (if (equal (stripdollar1 $gentranlang) 'c) 3 1) nil))))
		    ((and (listp pow) (eq (caar pow) 'mminus))
		     (list 'quotient
			   (franzexp 1 ind exp)
			   (list 'expt (franzexp var ind exp)
				       (franzexp (cadr pow) (if (equal (stripdollar1 $gentranlang) 'c) 3 1) nil))))
		    (t
		     (list 'expt (franzexp (cadr exp) ind exp)
				 (franzexp (caddr exp) (if (equal (stripdollar1 $gentranlang) 'c) 3 1) nil)))))) ;;dfloat powers in c --mds
((and (and (eq (caar exp) 'mminus) (numberp (cadr exp)))
		    (and (= 1 (length (car exp)))
			 (and (numberp (cadr exp)) (numberp (caddr exp))) ))

	 (cond ((get (caar exp) 'franznotn)
		(cons (get (caar exp) 'franznotn)
		(mapcar (function
			(lambda (elt) (franzexp elt ind context)))
		       (cdr exp))))
		(t
		 (cons (franzexp (caar exp) 1 nil)
		       (mapcar (function
				(lambda (elt) (franzexp elt 1 nil)))
			       (cdr exp)))) )  )
	;; added by Trevor 12/28/86

((setq fnotn (get (caar exp) 'franznotn))
      (if (and (equal fnotn 'plus) (equal (car exp) '(mplus simp)))
                 (cons fnotn
	       (mapcar (function
			(lambda (elt) (franzexp elt ind exp)))
		       (reverse (cdr exp)))) ;; --mds reverse terms in simp sum consistent with Macsyma 2.4
         (cons fnotn
	       (mapcar (function
			(lambda (elt) (franzexp elt ind exp)))
		       (cdr exp)))))
	(t
          (if (member 'array (car exp)) (put (stripdollar1 (caar exp)) 'array t)) ;;--mds mark undeclared array indices for [...] in c.
	 (cons (franzexp (caar exp) 1 nil)
	       (mapcar (function
			(lambda (elt) (franzexp elt 1 nil)))
		       (cdr exp))))))
;;	1 is always the right selection?????

;;	Following several functions were added by Trevor 12/86

( defun exptype ( exp )
    ( prog(ty1 ty2)

	( cond ( ( null exp ) ( return 'integer ) ) )
	( cond ( ( atom exp ) ( return ( itemtype exp ) ) ) )

	(cond ((and (listp (car exp)) (eq 'array (cadar exp)))
	       (return (exptype (caar exp))) ))

	(cond ((member (car exp)
	       '((mplus) (mminus) (mtimes) (mquotient) (mexpt)) )
	       (setq ty1 'integer))
               
	      (t (setq ty1 (exptype (car exp)))))

	(setq ty2 (exptype (cdr exp)))

	(cond((or (eq ty1 'complex) (eq ty2 'complex))
	      (return 'complex)))

	(cond((or (eq ty1 'double) (eq ty2 'double))
	      (return 'double)))

	(cond((or (eq ty1 'real) (eq ty2 'real))
	      (return 'real)))

	(cond((and (eq ty1 'integer) (eq ty2 'integer))
	      (return 'integer))
	     (t (return 'nil)))  ))


( defun itemtype ( item )
    ( prog()

	( cond ( ( numberp item )
		 ( cond ( ( floatp item ) ( return 'real ) )
			( t ( return 'integer ) )  ))
	       ( t
		 (setq allnum nil)
			;; set flag to to nil to show
			;; not all numbers in an expression
		 ( return ( getvartype (stripdollar1 item)) ) )  )))


;; --mds A purely syntactic mechanism to display floats as doubles
(defun dfix (c)
        (if (member c '(#\e #\d #\E #\D))
                #\d c))

(defun double (num)
    (prog (dnum)
	(cond ((floatp num)
                        (setq dnum (exploden num))
                        (setq dnum (map 'list 'dfix dnum))
                        (if (member #\d dnum) dnum
                        (setq dnum (append dnum '(#\d #\0))))
	                (return (implode dnum)))
	      (t  num))))

;; this prints real numbers in Fortran complex format by a purely synntactic mechanism --mds
(defun gcomplex (num)
   (if (and $usefortcomplex (not (equal (stripdollar1 $gentranlang) 'c)))
    (prog (cnum)
	(cond ((floatp num)
	       (setq cnum (append (exploden num) '( #\, #\0 #\. #\0 #\))))
	       (setq cnum (cons '|(| cnum ))
	       (return (implode cnum)))
	      (t (return (intern (format nil "(~a.0,0.0)" num))))))
              (float num)))


(defun simptimes1 (terms)
  (let ((neg) (denoms))
       (setq terms
	     (foreach trm in (simptimes2 terms) conc
		      (cond ((atom trm)
			     (cond ((member trm '(1 1.0)) ())
				   ((member trm '(-1 -1.0)) (setq neg (not neg))
							    ())
				   (t (list trm))))
			    ((and (eq (car trm) 'minus)
				  (member (cadr trm) '(1 1.0)))
			     (setq neg (not neg)) ())
			    ((and (eq (car trm) 'quotient)
				  (member (cadr trm) '(1 1.0)))
			     (setq denoms (aconc denoms (caddr trm))) ())
			    (t (list trm)))))
       (setq terms (or terms (list (franzexp 1 0 terms))))

       (cond (neg (setq terms (cons (list 'minus (car terms))
				    (cdr terms)))))
       (setq terms (cond ((onep (length terms)) (car terms))
			 (t (cons 'times terms))))
       (foreach d in denoms do
		(setq terms (list 'quotient terms d)))
       terms))

(defun simptimes2 (terms)
  (foreach trm in terms conc
	   (cond ((atom trm) (list trm))
		 ((eq (car trm) 'times) (simptimes2 (cdr trm)))
		 (t (list trm)))))

(defun franzstmt (stmt)
  ; return the franz lisp equivalent statement ;
  (cond ((member (safe-caar stmt) '( msetq mdo ))
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
	((macstopp stmt) (franzstop))
	((macendp stmt) (franzend))
	((mac$literalp stmt) (franzliteral (stripdollar1 (caar stmt)) stmt))
;;;	((maccallp stmt) (franzcall stmt)))) must be a mac call if it's nothing else.
        (t (franzcall stmt))))

(defun mac$literalp (stmt)
  ; is stmt a $literal function? ;
  (member (caar stmt) '($literal literal $data data) :test #'eq))

(defun franzliteral (fn stmt)
  (cons fn
	(foreach exp in (cdr stmt) collect
		 (cond ((member exp '($tab $cr) :test #'eq) exp)
		       ((listp exp) (franzexp exp 0 stmt))
		       (t (stripdollar1 exp))))))

(defun franzlabel (label)
  ; return the franz lisp representation for a label ;
  (stripdollar1 label))

(defun franzstmtgp (stmtgp)
  ; return the franz lisp representation for a statement group ;
  (append '(prog ()) (mapcar 'franzstmt (cdr stmtgp))))

(defun franzdef (def)
  ; return the franz lisp representation for a function definition ;
  ; case 1: ((msetq) id ((lambda) ((mlist) id ... id) exp))        ;
  ;           -->  (defun id (id ... id) (prog () (return exp)))   ;
  ; case 2: ((mdefine) ((id) id ... id) exp)                       ;
  ;           -->  (defun id (id ... id) (prog () (return exp)))   ;
  ; case 3: ((mdefine) ((id) id ... id) stmtgp)                    ;
  ;           -->  (defun id  (id ... id) (prog () stmt ... stmt)) ;
  (cond ((equal (caar def) 'msetq)
	 `(defun ,(franzexp (cadr def) 0 ( cadr def ) )

;; not sure how to change here and below a lot. mainly the ind;;


		 ,(mapcar (function (lambda (elt)
					    (franzexp elt 0 elt)))
			  (cdadaddr def))
		 (prog () (return ,(franzexp (caddaddr def) 0 ( caddaddr def ))))))
	((macexpp (caddr def))
	 `(defun ,(franzexp (caaadr def) 0 ( caaadr def ))
		 ,(mapcar (function (lambda (elt)
					    (franzexp elt 0 elt )))
			  (cdadr def))
		 (prog () (return ,(franzexp (caddr def) 0 ( caddr def ) )))))
	(t
	 `(defun ,(franzexp (caaadr def) 0 ( caaadr def ) )
		 ,(mapcar (function (lambda (elt)
					    (franzexp elt 0 elt )))
			  (cdadr def))
		 ,(franzstmt (caddr def))))))

(defun franzread (stmt)
  ; return the franz lisp representation for a read statement ;
  (let (varlist outlist fr)
       (setq varlist nil)
       (do ((s stmt (caddr s)))
	   ((or (null s) (atom s) (not (macstmtp s))))
	   (cond ((equal (caar s) 'msetq)
		  (setq varlist (cons (franzexp (cadr s) 0 ( cadr s ) )
				      varlist)))
		 (t
		  (setq outlist
			(mapcar (function (lambda (elt)
						  (franzexp elt 0 elt )))
				(cdr s))))))
       (setq fr nil)
       (cond (outlist (setq fr (append1 fr (cons 'princ outlist)))))
       (cond (varlist (setq fr (append1 fr `(setq ,(car varlist) (read))))))
       (do ((v varlist (cdr v)))
	   ((null (cdr v)))
	   (setq fr (append1 fr `(setq ,(cadr v) ,(car v)))))
       (cond ((> (length fr) 1) (cons 'progn fr))
	     (t (car fr)))))

(defun franzmatassign (stmt)
  ; return the franz lisp representation for a matrix assignment statement ;
(put (stripdollar1 (cadr stmt)) 'array t) ;; print matrix elements with [...] in c
  (do ((rows (cdaddr stmt) (cdr rows)) (r 1 (1+ r)) (fr (list 'progn)))
      ((null rows) fr)
      (do ((cols (cdar rows) (cdr cols)) (c 1 (1+ c)))
	  ((null cols))
	  (setq fr (append1 fr (list 'setq
				     (franzexp (list (list (cadr stmt)) r c)
					     0  (list (list (cadr stmt)) r c))
				     (franzexp (car cols)
					     0  (car cols) )))))))

(defun franznestassign (stmt)
  ; return the franz lisp representation for a nested assignment statement ;
  (let (varlist exp fr)
       (do ((s stmt (caddr s)))
	   ((or (atom s) (not (macstmtp s)))
	    (setq exp (franzexp s 0 s )))
	   (setq varlist (cons (franzexp (cadr s) 0 ( cadr s )) varlist)))
       (setq fr `(progn (setq ,(car varlist) ,exp)))
       (do ((v varlist (cdr v)))
	   ((null (cdr v)))
	   (setq fr (append1 fr `(setq ,(cadr v) ,(car v)))))
       fr))

(defun franzassign (stmt)
  ; return the franz lisp representation for an assignment statement ;
  `(setq ,(franzexp (cadr stmt) 0 ( cadr stmt ))
	 ,(franzexp (caddr stmt) 0 ( caddr stmt ) )))

(defun franzif (stmt)
  ; return the franz lisp representation for an if statement ;
  (destructuring-bind (x exp stmt1 y stmt2) stmt
   (declare (ignore x y))
   (let ((fr '(cond)))
        (setq fr (append1 fr (list (franzexp exp 0 exp)
				   (franzstmt stmt1))))
        (cond ((not (equal stmt2 '$false))
	       (append1 fr (list 't (franzstmt stmt2))))
	      (t
	       fr)))))

(defun franzfor (stmt)
  ; return the franz lisp representation for a for statement      ;
  ; ((mdo) var lo incr nextexp hi exitcond dobody)                ;
  ;   -->  (do ((var lo (+ var incr))  =or=  (var lo nextexp)) ;
  ;            ((or (> var hi) exitcond))                  ;
  ;            dobody)                                            ;
  (destructuring-bind (var lo incr nextexp hi exitcond dobody) (cdr stmt)
   (let (dovars doexit posincr)
       (setq oincr    incr
	     onextexp nextexp)
       (setq var      (franzexp var 0 var )
	     lo       (franzexp lo 0 lo )
	     incr     (franzexp incr 0 incr )
	     nextexp  (franzexp nextexp 0 nextexp )
	     hi       (franzexp hi 0 hi )
	     exitcond (franzexp exitcond 0 exitcond )
	     dobody   (franzstmt dobody))
       (cond ((and (not var) (or lo incr nextexp hi))
	      (setq tvname $tempvarname)
	      (setq $tempvarname 'i)
	      (setq var ($tempvar nil))
	      (setq $tempvarname tvname)))
       (cond ((and (not lo) (or var incr hi))
	      (setq lo 1)))
       (cond ((and (not incr) (not nextexp) (or var lo hi))
	      (setq incr 1)))
       (cond (incr
	      (cond ((or (null (getvartype var))
			 (inttypep (getvartype var)))
		     (cond ((numberp lo) (setq lo (floor lo))))
		     (cond ((numberp hi) (setq hi (floor hi))))
		     (cond ((numberp incr) (setq incr (floor incr))))))
	      (setq dovars `((,var ,lo (plus ,var ,incr))))))
       (cond (nextexp
	      (setq dovars `((,var ,lo ,nextexp)))))
       (cond (hi
	      (cond (nextexp
		     (setq posincr (noerrmevalp '((mgeqp) onextexp 0))))
		    (t
		     (setq posincr (noerrmevalp '((mgeqp) oincr 0)))))
	      (cond (posincr
		     (setq doexit `((greaterp ,var ,hi))))
		    (t
		     (setq doexit `((lessp ,var ,hi)))))))
       (cond (exitcond (setq doexit (append1 doexit exitcond))))
       (cond ((> (length doexit) 1)
	      (setq doexit (list (cons 'or doexit)))))
       `(do ,dovars ,doexit ,dobody))))

(defun franzforin (stmt)
  ; return the franz lisp representation for a for-in statement             ;
  ; ((mdoin) dovar dolist nil nil nil doexitcond dobody)                    ;
  ;   -->  (do ((genvar 1 (+ genvar 1)))                                 ;
  ;            ((> genvar listlength))                               ;
  ;            (cond ((equal genvar 1) (setq dovar list(1)))                ;
  ;                  ((equal genvar 2) (setq dovar list(2)))                ;
  ;                    .                                                    ;
  ;                    .                                                    ;
  ;                  ((equal genvar listlength) (setq dovar list(length)))) ;
  ;            (cond ((doexitcond) (break)))                                ;
  ;            dobody)                                                      ;
  (let ((gvar) condbody)
    (destructuring-bind (dovar (_x1 . dolist) _x2 _x3 _x4 doexitcond dobody) (cdr stmt)
       (declare (ignore _x1 _x2 _x3 _x4))
       (setq tvname $tempvarname)
       (setq $tempvarname 'i)
       (setq gvar ($tempvar nil))
       (setq $tempvarname tvname)
       (setq dovar (franzexp dovar 0 dovar ))
       (do ((i 1 (1+ i)))
	   ((> i (length dolist)))
	   (setq condbody
		 (append condbody
			 `(((equal ,gvar ,i)
			    (setq ,dovar ,(franzexp (nthelem i dolist)
						  0  (nthelem i dolist))))))))
       (cond (doexitcond
	      `(do ((,gvar 1 (+ ,gvar 1)))
		   ((> ,gvar ,(length dolist)))
		   (progn
		    ,(cons 'cond condbody)
		    (cond (,(franzexp doexitcond 0 doexitcond ) (break)))
		    ,(franz dobody))))
	     (t
	      `(do ((,gvar 1 (+ ,gvar 1)))
		   ((> ,gvar ,(length dolist)))
		   (progn
		    ,(cons 'cond condbody)
		    ,(franz dobody))))))))

(defun franzgo (stmt)
  ; return the franz lisp representation for a go statement ;
  `(go ,(franzlabel (cadr stmt))))

(defun franzret (stmt)
  ; return the franz lisp representation for a return statement ;
  (cond ((cdr stmt) `(return ,(franzexp (cadr stmt) 0 ( cadr stmt ) )))
	(t '(return))))

(defun franzprint (stmt)
  ; return the franz lisp representation for a print statement ;
  (cons 'princ
	(mapcar (function (lambda (elt)
				  (franzexp elt 0 elt )))
		(cdr stmt))))

(defun franzstop ()
  ; return the franz lisp representation for a stop statement ;
  '(stop))

(defun franzend ()
  ; return the franz lisp representation for an end statement ;
  '(end))

(defun franzcall (exp)
  ; return the franz lisp representation for a call statement ;
  (cond ((cdr exp) (cons (franzexp (caar exp) 0 ( caar exp ))
			 (mapcar (function (lambda (elt)
						   (franzexp elt
							    0 elt )))
				 (cdr exp))))
	(t (list (franzexp (caar exp) 0 ( caar exp ) )))))




(defun macexpp (exp)
  ; is exp an arithmetic or logical macsyma expression? ;
  (cond ((null exp) nil)
	((atom exp))
	((atom (car exp)) nil)
	((not (member (caar exp) '(mcond mdefine mdo mdoin mgo mprog mprogn
				 mreturn msetq $end $ev $literal $print
				 $readonly $stop $data) :test #'eq)))))

(defun maclogexpp (exp)
  ; is exp a macsyma logical expression? ;
  (cond ((atom exp)
	 (not (numberp exp)))
	((listp (car exp))
	 (not (member (caar exp)
		    '(mcond mdefine mdo mdoin mgo mexpt mminus mplus mprog
		      mprogn mquotient mreturn msetq mtimes rat $end $ev
		      $print $readonly $stop) :test #'eq)))))

(defun macstmtp (stmt)
  ; is stmt a macsyma statement? ;
  (cond ((null stmt) nil)
	((atom stmt))
	((atom (car stmt)) nil)
	((member (caar stmt) '(mcond mdo mdoin mgo mreturn msetq $end $print
			       $readonly $stop))
	 t)))

(defun macstmtgpp (stmt)
  ; is stmt a macsyma statement group? ;
  (cond ((or (null stmt) (atom stmt) (atom (car stmt))) nil)
	((member (caar stmt) '(mprog mprogn $ev)) t)))

(defun macdefp (stmt)
  ; is stmt a macsyma function or procedure definition? ;
  (cond ((or (null stmt) (atom stmt) (atom (car stmt))) nil)
	((or (equal (caar stmt) 'mdefine)
	     (and (equal (caar stmt) 'msetq)
		  (listp (caddr stmt))
		  (listp (caaddr stmt))
		  (equal (caaaddr stmt) 'lambda))))))


(defun macassignp (stmt)
  ; is stmt a macsyma assignment statement? ;
  (equal (safe-caar stmt) 'msetq))

(defun macnestassignp (stmt)
  ; is stmt a macsyma nested assignment statement? ;
  (and (macassignp stmt)
       (listp (caddr stmt))
       (listp (caaddr stmt))
       (macassignp (caddr stmt))))

(defun macmatassignp (stmt)
  ; is stmt a macsyma matrix assignment statement? ;
  (cond ((or (null stmt) (atom stmt) (atom (car stmt))) nil)
	((equal (caar stmt) '$matrix))
	((equal (caar stmt) 'msetq)
	 (macmatassignp (caddr stmt)))))

(defun macifp (stmt)
  ; is stmt a macsyma if-then or if-then-else statement? ;
  (equal (safe-caar stmt) 'mcond))

(defun macforp (stmt)
  ; is stmt a macsyma for-loop? ;
  (equal (safe-caar stmt) 'mdo))

(defun macforinp (stmt)
  ; is stmt a macsyma for-in-loop? ;
  (equal (safe-caar stmt) 'mdoin))

(defun macgop (stmt)
  ; is stmt a macsyma go statement? ;
  (equal (safe-caar stmt) 'mgo))

(defun maclabelp (stmt)
  ; is stmt a macsyma statement label? ;
  (atom stmt))


;;;(defun maccallp (stmt)
;;;  ; is stmt a macsyma call statement? ;
;;;  t)

(defun macretp (stmt)
  ; is stmt a macsyma return statement? ;
  (equal (safe-caar stmt) 'mreturn))

(defun macreadp (stmt)
  ; is stmt a macsyma read statement? ;
  (cond ((or (null stmt) (atom stmt) (atom (car stmt))) nil)
	((equal (safe-caar stmt) '$readonly))
	((equal (safe-caar stmt) 'msetq)
	 (macreadp (caddr stmt)))))

(defun macprintp (stmt)
  ; is stmt a macsyma print statement? ;
  (equal (safe-caar stmt) '$print))

(defun macstopp (stmt)
  ; is stmt a macsyma stop statement? ;
  (equal (safe-caar stmt) '$stop))

(defun macendp (stmt)
  ; is stmt a macsyma end statement? ;
  (equal (safe-caar stmt) '$end))



;;  -----------  ;;
;;  parser.l     ;;    gentran parser module
;;  -----------  ;;


;;                                            ;;
;;  2. vaxima internal representation parser  ;;
;;                                            ;;


(defun gentranparse (forms)
  (foreach f in forms do
	   (cond ((not (or (pmstmt f)
			   (pmexp f)
			   (pmlogexp f)))
		  (gentranerr 'e f "cannot be translated" nil)))))

(defun pmexp (s)
  ; exp  ::=  const | var | funcall | ((mminus ~) exp) |       ;
  ;           ((mquotient ~) exp exp) | ((rat ~) exp exp) |    ;
  ;           ((mexpt ~) exp exp) | ((mplus ~) exp exp exp') | ;
  ;           ((mtimes ~) exp exp exp')                        ;
  ; funcall  ::=  ((id ~) exp')                                ;
  (cond ((atom s)
	 (or (pmconst s)
	     (pmid s)))
	((and (listp s)
	      (listp (car s)))
	 (cond ((pmidop (car s))
		(pmexp1 (cdr s)))
	       ((pmmminusop (car s))
		(and (equal (length s) 2)
		     (pmexp (cadr s))))
	       ((or (pmmquotientop (car s))
		    (pmratop (car s))
		    (pmmexptop (car s)))
		(and (equal (length s) 3)
		     (pmexp (cadr s))
		     (pmexp (caddr s))))
	       ((or (pmmplusop (car s))
		    (pmmtimesop (car s)))
		(and (> (length s) 2)
		     (pmexp (cadr s))
		     (pmexp (caddr s))
		     (pmexp1 (cdddr s))))))))

(defun pmexp1 (s)
  ; exp'  ::=  exp exp' | epsilon ;
  (or (null s)
      (and (pmexp (car s))
	   (pmexp1 (cdr s)))))

(defun pmlogexp (s)
  ; logexp  ::=  t | nil | var | funcall | relexp | ((mnot ~) logexp) | ;
  ;              ((mand ~) logexp logexp logexp') |                     ;
  ;              ((mor ~) logexp logexp logexp')                        ;
  ; relexp  ::=  ((mgreaterp ~) exp exp) | ((mequal ~) exp exp) |       ;
  ;              ((mnotequal ~) exp exp) | ((mlessp ~) exp exp) |       ;
  ;              ((mgeqp ~) exp exp) | ((mleqp ~) exp exp)              ;
  ; funcall  ::=  (id exp')                                             ;
  (cond ((atom s)
	 (or (pmid s)
	     (null s)
	     (equal s t)))
	((and (listp s)
	      (listp (car s)))
	 (cond ((pmidop (car s))
		(pmexp1 (cdr s)))
	       ((or (pmmgreaterpop (car s))
		    (pmmequalop (car s))
		    (pmmnotequalop (car s))
		    (pmmlesspop (car s))
		    (pmmgeqpop (car s))
		    (pmmleqpop (car s)))
		(and (equal (length s) 3)
		     (pmexp (cadr s))
		     (pmexp (caddr s))))
	       ((pmmnotop (car s))
		(and (equal (length s) 2)
		     (pmlogexp (cadr s))))
	       ((or (pmmandop (car s))
		    (pmmorop (car s)))
		(and (> (length s) 2)
		     (pmlogexp (cadr s))
		     (pmlogexp (caddr s))
		     (pmlogexp1 (cdddr s))))))))

(defun pmlogexp1 (s)
  ; logexp'  ::=  logexp logexp' | epsilon ;
  (or (null s)
      (and (pmlogexp (car s))
	   (pmlogexp1 (cdr s)))))

(defun pmstmt (s)
  ; stmt  ::=  assign | nestassign | matassign | cond | for | forin | go |  ;
  ;            label | call | return | stop | end | read | print | stmtgp | ;
  ;            defn                                                         ;
  ; assign  ::=  ((msetq ~) var exp) | ((msetq ~) var logexp)               ;
  ; nestassign  ::=  ((msetq ~) var ((msetq ~) var nestassign'))            ;
  ; nestassign'  ::=  ((msetq ~) var nestassign') | exp | logexp            ;
  ; matassign  ::=  ((msetq ~) var (($matrix ~) list list'))                ;
  ; cond  ::=  ((mcond ~) logexp stmt t $false) |                           ;
  ;            ((mcond ~) logexp stmt t stmt)                               ;
  ; for  ::=  ((mdo ~) varnil exp exp exp exp logexp stmt)                  ;
  ; forin  ::=  ((mdoin ~) var list nil nil nil logexp stmt)                ;
  ; go  ::=  ((mgo ~) label)                                                ;
  ; label  ::=  id                                                          ;
  ; call  ::=  ((id ~) params)                                              ;
  ; return  ::=  ((mreturn ~) retexp)                                       ;
  ; stop  ::=  (($stop ~))                                                  ;
  ; end  ::=  (($end ~))                                                    ;
  ; read  ::=  ((msetq ~) var read')                                        ;
  ; read'  ::=  ((msetq ~) var read') | (($readonly ~) var)                 ;
  ; print  ::=  (($print ~) params)                                         ;
  ; stmtgp  ::=  ((mprog ~) stmt stmt') | ((mprogn ~) stmt stmt') |         ;
  ;              (($ev ~) stmt stmt')                                       ;
  ; defn  ::=  ((msetq ~) id ((lambda ~) list retexp)) |                    ;
  ;            ((mdefine ~) ((id ~) id') retexp) |                          ;
  ;            ((mdefine ~) ((id ~) id') stmt) |                            ;
  ;            ((mdefine ~) ((id ~) id'))                                   ;
  (cond ((atom s)
	 (pmid s))
	((and (listp s)
	      (listp (car s)))
	 (cond ((pmmsetqop (car s))
		(pmmsetq1 (cdr s)))
	       ((pmmcondop (car s))
		(and (> (length s) 4)
		     (pmlogexp (cadr s))
		     (pmstmt (caddr s))
		     (equal (cadddr s) 't)
		     (pmmcond1 (cddddr s))))
	       ((pmmdoop (car s))
		(and (equal (length s) 8)
		     (pmvarnil (cadr s))
		     (pmexp (caddr s))
		     (pmexp (cadddr s))
		     (pmexp (caddddr s))
		     (pmexp (cadddddr s))
		     (pmlogexp (caddddddr s))
		     (pmstmt (cadddddddr s))))
	       ((pmmdoinop (car s))
		(and (equal (length s) 8)
		     (pmvar (cadr s))
		     (pmlist (caddr s))
		     (null (cadddr s))
		     (null (caddddr s))
		     (null (cadddddr s))
		     (pmlogexp (caddddddr s))
		     (pmstmt (cadddddddr s))))
	       ((pmmgoop (car s))
		(and (equal (length s) 2)
		     (pmid (cadr s))))
	       ((pmmreturnop (car s))
		(or (equal (length s) 1)
		    (and (equal (length s) 2)
			 (pmretexp (cadr s)))))
	       ((pm$stopop (car s))
		(equal (length s) 1))
	       ((pm$endop (car s))
		(equal (length s) 1))
	       ((pm$printop (car s))
		(pmparams1 (cdr s)))
	       ((pm$declare_typeop (car s)))
	       ((or (pmmprogop (car s))
		    (pmmprognop (car s))
		    (pm$evop (car s)))
		(and (> (length s) 1)
		     (pmstmt (cadr s))
		     (pmstmt1 (cddr s))))
	       ((pmmdefineop (car s))
		(and (> (length s) 1)
		     (pmidparamop (cadr s))
		     (or (null (cddr s))
			 (pmmdefine1 (cddr s)))))
	       ((pmidop (car s))
		(pmparams1 (cdr s)))))))

(defun pmstmt1 (s)
  ; stmt'  ::=  stmt stmt' | epsilon  ;
  (or (null s)
      (and (pmstmt (car s))
	   (pmstmt1 (cdr s)))))

(defun pmmsetq1 (s)
  (cond ((and (listp s)
	      (atom (car s)))
	 (and (pmid (car s))
	      (pmmsetq2 (cdr s))))
	((and (listp s)
	      (listp (car s)))
	 (and (> (length (car s)) 1)
	      (pmidop (caar s))
	      (pmexp (cadar s))
	      (pmexp1 (cddar s))
	      (pmmsetq3 (cdr s))))))

(defun pmmsetq2 (s)
  (cond ((and (listp s)
	      (listp (car s))
	      (equal (length s) 1)
	      (equal (length (car s)) 3)
	      (pmlambdaop (caar s)))
	 (and (pmlist (cadar s))
	      (pmretexp (caddar s))))
	((pmmsetq3 s))))

(defun pmmsetq3 (s)
  (cond ((and (listp s)
	      (listp (car s))
	      (equal (length s) 1)
	      (> (length (car s)) 1)
	      (pm$matrixop (caar s)))
	 (and (pmlist (cadar s))
	      (pmlist1 (cddar s))))
	((pmmsetq4 s))))

(defun pmmsetq4 (s)
  (cond ((listp s)
	 (cond ((pmexp (car s))
		(null (cdr s)))
	       ((pmlogexp (car s))
		(null (cdr s)))
	       ((and (listp (car s))
		     (pmmsetqop (caar s)))
		(and (equal (length s) 1)
		     (> (length (car s)) 1)
		     (pmvar (cadar s))
		     (pmmsetq4 (cddar s))))
	       ((and (listp (car s))
		     (pm$readonlyop (caar s)))
		(and (equal (length s) 1)
		     (or (equal (length (car s)) 1)
			 (and (equal (length (car s)) 2)
			      (pmvar (cadar s))))))))))

(defun pmmcond1 (s)
  (cond ((equal s '($false)))
	((pmstmt (car s))
	 (null (cdr s)))))

(defun pmidop (s)
  (and (listp s)
       (not (member (car s) *reswds*))))

(defun pmmminusop (s)
  (and (listp s)
       (equal (car s) 'mminus)))

(defun pmmquotientop (s)
  (and (listp s)
       (equal (car s) 'mquotient)))

(defun pmratop (s)
  (and (listp s)
       (equal (car s) 'rat)))

(defun pmmexptop (s)
  (and (listp s)
       (equal (car s) 'mexpt)))

(defun pmmplusop (s)
  (and (listp s)
       (equal (car s) 'mplus)))

(defun pmmtimesop (s)
  (and (listp s)
       (equal (car s) 'mtimes)))

(defun pmmgreaterpop (s)
  (and (listp s)
       (equal (car s) 'mgreaterp)))

(defun pmmequalop (s)
  (and (listp s)
       (equal (car s) 'mequal)))

(defun pmmnotequalop (s)
  (and (listp s)
       (equal (car s) 'mnotequal)))

(defun pmmlesspop (s)
  (and (listp s)
       (equal (car s) 'mlessp)))

(defun pmmgeqpop (s)
  (and (listp s)
       (equal (car s) 'mgeqp)))

(defun pmmleqpop (s)
  (and (listp s)
       (equal (car s) 'mleqp)))

(defun pmmnotop (s)
  (and (listp s)
       (equal (car s) 'mnot)))

(defun pmmandop (s)
  (and (listp s)
       (equal (car s) 'mand)))

(defun pmmorop (s)
  (and (listp s)
       (equal (car s) 'mor)))

(defun pmmsetqop (s)
  (and (listp s)
       (equal (car s) 'msetq)))

(defun pmmcondop (s)
  (and (listp s)
       (equal (car s) 'mcond)))

(defun pmmdoop (s)
  (and (listp s)
       (equal (car s) 'mdo)))

(defun pmmdoinop (s)
  (and (listp s)
       (equal (car s) 'mdoin)))

(defun pmmgoop (s)
  (and (listp s)
       (equal (car s) 'mgo)))

(defun pmmreturnop (s)
  (and (listp s)
       (equal (car s) 'mreturn)))

(defun pm$stopop (s)
  (and (listp s)
       (equal (car s) '$stop)))

(defun pm$endop (s)
  (and (listp s)
       (equal (car s) '$end)))

(defun pm$printop (s)
  (and (listp s)
       (equal (car s) '$print)))

(defun pm$declare_typeop (s)
  (and (listp s)
       (equal (car s) '$declare_type)))

(defun pmmprogop (s)
  (and (listp s)
       (equal (car s) 'mprog)))

(defun pmmprognop (s)
  (and (listp s)
       (equal (car s) 'mprogn)))

(defun pm$evop (s)
  (and (listp s)
       (equal (car s) '$ev)))

(defun pmmdefineop (s)
  (and (listp s)
       (equal (car s) 'mdefine)))

(defun pm$readonlyop (s)
  (and (listp s)
       (equal (car s) '$readonly)))

(defun pmlambdaop (s)
  (and (listp s)
       (equal (car s) 'lambda)))

(defun pm$matrixop (s)
  (and (listp s)
       (equal (car s) '$matrix)))

(defun pmmlistop (s)
  (and (listp s)
       (equal (car s) 'mlist)))

(defun pmidparamop (s)
  (and (listp s)
       (pmidop (car s))
       (pmid1 (cdr s))))

(defun pmmdefine1 (s)
  (and (listp s)
       (equal (length s) 1)
       (or (pmretexp (car s))
	   (pmstmt (car s)))))

(defun pmid1 (s)
  ; id'  ::=  id id' | epsilon ;
  (or (null s)
      (and (pmid (car s))
	   (pmid1 (cdr s)))))

(defun pmvar (s)
  ; var  ::=  id | arrelt          ;
  ; arrelt  ::=  ((id ~) exp exp') ;
  (cond ((atom s)
	 (pmid s))
	((listp s)
	 (and (> (length s) 1)
	      (pmidop (car s))
	      (pmexp (cadar s))
	      (pmexp1 (cddar s))))))

(defun pmvarnil (s)
  ; varnil  ::=  var | nil ;
  (or (null s)
      (pmvar s)))

(defun pmretexp (s)
  ; retexp  ::=  exp | logexp | string | epsilon ;
  (or (null s)
      (pmexp s)
      (pmlogexp s)
      (pmstring s)))

(defun pmparams1 (s)
  ; params  ::=  exp params | logexp params | string params | epsilon ;
  (or (null s)
      (and (pmexp (car s))
	   (pmparams1 (cdr s)))
      (and (pmlogexp (car s))
	   (pmparams1 (cdr s)))
      (and (pmstring (car s))
	   (pmparams1 (cdr s)))))

(defun pmlist (s)
  ; list  ::=  ((mlist ~) exp exp') | ((mlist ~) logexp logexp') ;
  (and (listp s)
       (pmmlistop (car s))
       (pmlist2 (cdr s))))

(defun pmlist2 (s)
  (or (and (pmexp (car s))
	   (pmexp1 (cdr s)))
      (and (pmlogexp (car s))
	   (pmlogexp1 (cdr s)))))

(defun pmlist1 (s)
  ; list'  ::=  list list' | epsilon ;
  (or (null s)
      (and (pmlist (car s))
	   (pmlist1 (cdr s)))))

(defun pmconst (s)
  (or (numberp s)
      (null s)
      (equal s t)))

(defun pmstring (s)
  (and (atom s)
       (equal (car (explodec s)) '&)))

(defun pmid (s)
  (and (atom s)
       (not (member s '(t nil)))))





;;  -----------  ;;
;;  segmnt.l     ;;    segmentation module
;;  -----------  ;;



;;                           ;;
;; 1. segmentation routines  ;;
;;                           ;;


(defun seg (forms)
  ; exp  --+-->  exp                                          ;
  ;        +-->  (assign    assign    ... assign      exp   ) ;
  ;                     (1)       (2)           (n-1)    (n)  ;
  ; stmt  --+-->  stmt                                        ;
  ;         +-->  stmtgp                                      ;
  ; stmtgp  ----->  stmtgp                                    ;
  ; def  ----->  def                                          ;
  (foreach f in forms collect
	   (cond ((and (not (atom f)) (not (equal (car f) 'literal)) (lispexpp f))
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

(defun segexp (exp type)
  ; exp  -->  (assign    assign    ... assign      exp   ) ;
  ;                  (1)       (2)           (n-1)    (n)  ;
  (reverse (segexp1 exp type)))

(defun segexp1 (exp type)
  ; exp  -->  (exp    assign      assign      ... assign   ) ;
  ;               (n)       (n-1)       (n-2)           (1)  ;
  (prog (res tempvarname)
	(setq tempvarname $tempvarname)
	(setq res (segexp2 exp type))
	(recurunmark res)
	(setq $tempvarname tempvarname)
	(cond ((equal (car res) (cadadr res))
	       (progn
		(setq res (cdr res))
		(rplaca res (caddar res)))))
	(return res)))

(defun segexp2 (exp type)
  ; exp  -->  (exp    assign      assign      ... assign   ) ;
  ;               (n)       (n-1)       (n-2)           (1)  ;
  (prog (expn assigns newassigns unops op termlist var tmp)
	(setq expn exp)
	(loop while (equal (length expn) 2) do
		(setq unops (cons (car expn) unops))
		(setq expn (cadr expn)))
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
	(loop while unops do
		(setq expn (list (car unops) expn))
		(setq unops (cdr unops)))
	(cond ((equal expn exp)
	       (progn
		(recurunmark expn)
		(setq var (or var (tempvar type)))
		(markvar var)
		(setq assigns (list (mkassign var expn)))
		(setq expn var))))
	(return (cons expn assigns))))

(defun segstmt (stmt)
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

(defun segassign (stmt)
  ; assign  -->  stmtgp ;
  (prog (var exp type)
	(setq var (cadr stmt))
	(setq type (getvartype var))
	(setq exp (caddr stmt))
	(setq stmt (segexp1 exp type))
	(rplaca stmt (mkassign var (car stmt)))
	(return (mkstmtgp 0 (reverse stmt)))))

(defun segcond (cond)
  ; cond  --+-->  cond   ;
  ;         +-->  stmtgp ;
  (prog (tassigns res markedvars type)
	(cond ((eq (stripdollar1 $gentranlang) 'c)
	       (setq type 'int))
	      (t
	       (setq type 'logical)))
	(loop while (setq cond (cdr cond)) do
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

(defun segdo (stmt)
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
		     (cond ((eq (stripdollar1 $gentranlang) 'c)
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

(defun segreturn (ret)
  ; return  -->  stmtgp ;
  (progn
   (setq ret (segexp1 (cadr ret) 'unknown))
   (rplaca ret (mkreturn (car ret)))
   (mkstmtgp 0 (reverse ret))))

(defun seggroup (stmtgp)
  ; stmtgp  -->  stmtgp ;
  (prog (locvars res)
	(cond ((equal (car stmtgp) 'prog)
	       (progn
		(setq locvars (cadr stmtgp))
		(setq stmtgp (cdr stmtgp))))
	      (t
	       (setq locvars 0)))
	(loop while (setq stmtgp (cdr stmtgp)) do
	       (setq res (cons (segstmt (car stmtgp)) res)))
	(return (mkstmtgp locvars (reverse res)))))

(defun segdef (def)
  ; def  -->  def ;
  (mkdef (cadr def)
	 (caddr def)
	 (foreach stmt in (cdddr def) collect (segstmt stmt))))


;;                                             ;;
;;  2. long statement & expression predicates  ;;
;;                                             ;;


(defun toolongexpp (exp)
  (> (numprintlen exp) $maxexpprintlen))

(defun toolongstmtp (stmt)
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

(defun toolongassignp (assign)
  (toolongexpp (caddr assign)))

(defun toolongcondp (cond)
  (prog (toolong)
	(loop while (setq cond (cdr cond)) do
	       (cond ((or (toolongexpp (caar cond))
			  (toolongstmtp (cadar cond)))
		      (setq toolong t))))
	(return toolong)))

(defun toolongdop (dostmt)
  (cond ((> (eval (cons '+ (foreach exp in (caadr dostmt) collect
					      (numprintlen exp))))
		   $maxexpprintlen) t)
	((toolongexpp (caaddr dostmt)) t)
	((lispstmtgpp (cadddr dostmt)) (toolongstmtgpp (cadddr dostmt)))
	(t (eval (cons 'or (foreach stmt in (cdddr dostmt) collect
				    (toolongstmtp stmt)))))))

(defun toolongreturnp (ret)
  (toolongexpp (cadr ret)))

(defun toolongstmtgpp (stmtgp)
  (eval (cons 'or
	      (foreach stmt in (cdr stmtgp) collect (toolongstmtp stmt)))))

(defun toolongdefp (def)
  (cond ((lispstmtgpp (cadddr def))
	 (toolongstmtgpp (cadddr def)))
	(t
	 (eval (cons 'or
		     (foreach stmt in (cdddr def) collect
			      (toolongstmtp stmt)))))))


;;                            ;;
;;  3. print length function  ;;
;;                            ;;


(defun numprintlen (exp)
  (cond ((atom exp)
	 (length (explode exp)))
	((onep (length exp))
	 (numprintlen (car exp)))
	(t
	 (+ (length exp)
	       (eval (cons '+
			   (foreach elt in (cdr exp) collect
				    (numprintlen elt))))))))






;;  -----------  ;;
;;  lspfor.l     ;;    lisp-to-fortran translation module
;;  -----------  ;;

(put 'or       '*fortranprecedence* 1)
(put 'and      '*fortranprecedence* 2)
(put 'not      '*fortranprecedence* 3)
(put 'equal    '*fortranprecedence* 4)
(put 'notequal '*fortranprecedence* 4)
(put '>        '*fortranprecedence* 4)
(put 'greaterp '*fortranprecedence* 4)
(put 'geqp     '*fortranprecedence* 4)
(put '<        '*fortranprecedence* 4)
(put 'lessp    '*fortranprecedence* 4)
(put 'leqp     '*fortranprecedence* 4)
(put '+        '*fortranprecedence* 5)
(put 'plus     '*fortranprecedence* 5)
(put '*        '*fortranprecedence* 6)
(put 'times    '*fortranprecedence* 6)
(put 'quotient '*fortranprecedence* 6)
(put '-        '*fortranprecedence* 7)
(put 'minus    '*fortranprecedence* 7)
(put 'expt     '*fortranprecedence* 8)
(put 'or       '*fortranop* '| .or. |)
(put 'and      '*fortranop* '| .and. |)
(put 'not      '*fortranop* '| .not. |)
(put 'equal    '*fortranop* '| .eq. |)
(put 'notequal '*fortranop* '| .ne. |)
(put '>        '*fortranop* '| .gt. |)
(put 'greaterp '*fortranop* '| .gt. |)
(put 'geqp     '*fortranop* '| .ge. |)
(put '<        '*fortranop* '| .lt. |)
(put 'lessp    '*fortranop* '| .lt. |)
(put 'leqp     '*fortranop* '| .le. |)
(put '+        '*fortranop* '|+|)
(put 'plus     '*fortranop* '|+|)
(put '*        '*fortranop* '|*|)
(put 'times    '*fortranop* '|*|)
(put 'quotient '*fortranop* '|/|)
(put 'expt     '*fortranop* '|**|)
(put '-        '*fortranop* '|-|)
(put 'minus    '*fortranop* '|-|)
(put nil '*fortranname* ".false.")

;;                                         ;;
;;  lisp-to-fortran translation functions  ;;
;;                                         ;;


;;  control function  ;;


(defun fortcode (forms)
  (foreach f in forms conc
	   (cond ((atom f)
		  (cond ((member f '($begin_group $end_group)) ())
		        (t (fortexp f))))
		 ((or (lispstmtp f)
		      (lispstmtgpp f))
		  (cond (*gendecs
			 (prog (r)
			       (setq r
				     (append (fortdecs (symtabget '*main*
								  '*decs*))
					     (fortstmt f)))
			       (symtabrem '*main* '*decs*)
			       (return r)))
			(t
			 (fortstmt f))))
		 ((lispdefp f)
		  (fortsubprog f))
		 (t
		  (fortexp f)))))


;;  subprogram translation  ;;


(defun fortsubprog (def)
  (prog (type stype name params body lastst r)
	(setq name (cadr def))
	(setq body (cdddr def))
	(cond ((and body (equal body '(nil))) (setq body ())))
	(cond ((and (onep (length body))
		    (lispstmtgpp (car body)))
	       (progn (setq body (cdar body))
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
			(cond ((or type
				   (gfunctionp body name))
			       'function)
			      (t
			       'subroutine))))
	(symtabrem name '*type*)
	(setq params (or (symtabget name '*params*) (caddr def)))
	(symtabrem name '*params*)
	(setq r (mkffortsubprogdec type stype name params))
	(cond (*gendecs
	       (setq r (append r (fortdecs (symtabget name '*decs*))))))
	(setq r (append r (foreach s in body conc (fortstmt s))))
	(cond (*gendecs
	       (progn
		(symtabrem name nil)
		(symtabrem name '*decs*))))
	(return r)))

;;  generation of declarations  ;;

(defun fortdecs (decs)
  (foreach tl in (formtypelists decs) conc
	   (mkffortdec (car tl) (cdr tl))))

;;  expression translation  ;;

;; --mds print floats as "double" n.0d0
(defun dbl (n)
    (cond ((= (float 1) 1.0d0)
             (if (floatp n) (double n) n))
          (t  (if (floatp n) (coerce n 'double-float) n))))

(defun fortexp (exp)
        (if $dblfloat (map 'list 'dbl (fortexp1 exp 0))
        (fortexp1 exp 0)))

(defun fortexp1 (exp wtin)
  (cond ((atom exp) (list (fortranname exp)))
	((eq (car exp) 'data) (fortdata exp))
	((eq (car exp) 'literal) (fortliteral exp))
	((null (cdr exp)) exp)
	((member (car exp) '(minus not) :test #'eq)
	 (let* ((wt (fortranprecedence (car exp)))
		(res (cons (fortranop (car exp)) (fortexp1 (cadr exp) wt))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	((or (member (car exp) *lisparithexpops* :test #'eq)
	     (member (car exp) *lisplogexpops* :test #'eq))
	 (let* ((wt (fortranprecedence (car exp)))
		(op (fortranop (car exp)))
		(res (fortexp1 (cadr exp) wt))
		(res1))
	       (setq exp (cdr exp))
	       (cond ((eq op '+)
		      (loop while (setq exp (cdr exp)) do
                         (progn
			  (setq res1 (fortexp1 (car exp) wt))
			  (cond ((or (eq (car res1) '-)
				     (and (numberp (car res1))
					  (minusp (car res1))))
				 (setq res (append res res1)))
				(t
				 (setq res (append res (cons op res1))))))))
		     (t
		      (loop while (setq exp (cdr exp)) do
                         (setq res (append res
					   (cons op
						 (fortexp1 (car exp) wt)))))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	(t
	 (let ((res (cons (car exp) (cons '|(| (fortexp1 (cadr exp) 0)))))
              (setq exp (cdr exp))
	      (loop while (setq exp (cdr exp)) do
                 (setq res (append res (cons '|,| (fortexp1 (car exp) 0)))))
              (aconc res '|)|)))))

(defun fortranname (name)   
  (if (symbolp name) (or (get name '*fortranname*) name)
  name))

(defun fortranop (op)
  (or (get op '*fortranop*) op))

(defun fortranprecedence (op)
  (or (get op '*fortranprecedence*) 9))

;;  statement translation  ;;

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

(defun fortassign (stmt)
  (mkffortassign (cadr stmt) (caddr stmt)))

(defun fortbreak (stmt)
  (declare (ignore stmt))
  (cond ((null *endofloopstack*)
	 (gentranerr 'e nil "break not inside loop - cannot be translated" nil))
	((atom (car *endofloopstack*))
	 (prog (n1)
	       (setq n1 (genstmtno))
	       (rplaca *endofloopstack* (list (car *endofloopstack*) n1))
	       (return (mkffortgo n1))))
	(t
	 (mkffortgo (cadar *endofloopstack*)))))

(defun fortcall (stmt)
  (mkffortcall (car stmt) (cdr stmt)))

(defun fortdo (var lo nextexp exitcond body)
  (prog (n1 hi incr result)
	(setq n1 (genstmtno))
	(setq *endofloopstack* (cons n1 *endofloopstack*))
	(setq hi (car (delete1 'greaterp (delete1 'lessp (delete1 var exitcond)))))
	(setq incr (car (delete1 'plus (delete1 var nextexp))))
	(setq result (mkffortdo n1 var lo hi incr))
	(indentfortlevel (+ 1))
	(setq result (append result (foreach st in body conc (fortstmt st))))
	(indentfortlevel (- 1))
	(setq result (append result (mkffortcontinue n1)))
	(cond ((listp (car *endofloopstack*))
	       (setq result
		     (append result
			     (mkffortcontinue (cadar *endofloopstack*))))))
	(setq *endofloopstack* (cdr *endofloopstack*))
	(return result)))

(defun fortend (stmt)
  (declare (ignore stmt))
  (mkffortend))

(defun fortfor (var lo nextexp exitcond body)
  (prog (n1 n2 result)
	(setq n1 (genstmtno))
	(setq n2 (genstmtno))
	(setq *endofloopstack* (cons n2 *endofloopstack*))
	(cond (var (setq result (mkffortassign var lo))))
	(cond (exitcond
	       (setq result (append result
				    (append (list n1 '| | )
					    (mkffortifgo exitcond n2)))))
	      (t
	       (setq result (append result (mkffortcontinue n1)))))
	(indentfortlevel (+ 1))
	(setq result (append result (foreach st in body conc (fortstmt st))))
	(cond (nextexp
	       (progn
		(cond ((equal nextexp '(nil)) (setq nextexp nil)))
		(setq result (append result (mkffortassign var nextexp))))))
	(setq result (append result (mkffortgo n1)))
	(indentfortlevel (- 1))
	(setq result (append result (mkffortcontinue n2)))
	(cond ((listp (car *endofloopstack*))
	       (setq result
		     (append result
			     (mkffortcontinue (cadar *endofloopstack*))))))
	(setq *endofloopstack* (cdr *endofloopstack*))
	(return result)))

(defun fortgoto (stmt)
  (prog (stmtno)
	(cond ((not (setq stmtno (get (cadr stmt) '*stmtno*)))
	       (setq stmtno (put (cadr stmt) '*stmtno* (genstmtno)))))
	(return (mkffortgo stmtno))))

(defun fortif (stmt)
  (prog (n1 n2 res)
	(setq stmt (cdr stmt))
	(cond ((onep (length stmt))
	       (cond ((equal (caar stmt) 't)
		      (return (foreach st in (cdar stmt) conc
				       (fortstmt st))))
		     (t
		      (return
		       (progn
			(setq n1 (genstmtno))
			(setq res (mkffortifgo (list 'not (caar stmt)) n1))
			(indentfortlevel (+ 1))
			(setq res (append res (foreach st in (cdar stmt) conc
						       (fortstmt st))))
			(indentfortlevel (- 1))
			(append res (mkffortcontinue n1)))))))
	      (t
	       (return
		(progn
		 (setq n1 (genstmtno))
		 (setq n2 (genstmtno))
		 (setq res (mkffortifgo (list 'not (caar stmt)) n1))
		 (indentfortlevel (+ 1))
		 (setq res (append res (foreach st in (cdar stmt) conc
						(fortstmt st))))
		 (setq res (append res (mkffortgo n2)))
		 (indentfortlevel (- 1))
		 (setq res (append res (mkffortcontinue n1)))
		 (indentfortlevel (+ 1))
		 (setq res (append res (fortif (cons 'cond (cdr stmt)))))
		 (indentfortlevel (- 1))
		 (append res (mkffortcontinue n2))))))))

(defun fortliteral (stmt)
  (foreach a in (cdr stmt) conc
	   (cond ((equal a '$tab) (list (mkforttab)))
		 ((equal a '$cr) (list (mkterpri)))
		 ((listp a) (fortexp a))
		 (t (list a)))))

;; fortdata added by pwang 12/12/88
(defun fortdata (stmt)
  (append (list (mkforttab) "data " (cadr stmt) '|/|)
	  (addcom (cddr stmt))
	  (list '|/|))
)

(setq COMMA* ",")

(defun addcom(nl)
(cond ((null nl) nil)
      ((null (cdr nl)) nl)
      (t (cons (car nl) (cons COMMA* (addcom (cdr nl)))))
      )
)

(defun fortloop (stmt)
  (prog (var lo nextexp exitcond body)
	(cond ((complexdop stmt)
	       (return (fortstmt (seqtogp (simplifydo stmt))))))
	(cond ((setq var (cadr stmt))
	       (progn
		(setq lo (cadar var))
		(cond ((equal (length (car var)) 3)
		       (setq nextexp (or (caddar var) (list 'nil)))))
		(setq var (caar var)))))
	(cond ((setq exitcond (caddr stmt))
	       (setq exitcond (car exitcond))))
	(setq body (cdddr stmt))
	(cond ((and var
		    lo
		    (equal (car nextexp) 'plus)
		    (member var nextexp)
		    (member (car exitcond) (list 'greaterp 'lessp))
		    (member var exitcond))
	       (return (fortdo var lo nextexp exitcond body)))
	      ((and exitcond
		    (not var))
	       (return (fortwhile exitcond body)))
	      ((and var
		    (not lo)
		    (lisplogexpp nextexp)
		    (equal exitcond var))
	       (return (fortrepeat body nextexp)))
	      (t
	       (return (fortfor var lo nextexp exitcond body))))))

(defun fortread (stmt)
  (mkffortread (cadr stmt)))

(defun fortrepeat (body exitcond)
  (prog (n result)
	(setq n (genstmtno))
	(setq *endofloopstack* (cons 'dummy *endofloopstack*))
	(setq result (mkffortcontinue n))
	(indentfortlevel (+ 1))
	(setq result (append result (foreach st in body conc (fortstmt st))))
	(indentfortlevel (- 1))
	(setq result (append result (mkffortifgo (list 'not exitcond) n)))
	(cond ((listp (car *endofloopstack*))
	       (setq result
		     (append result
			     (mkffortcontinue (cadar *endofloopstack*))))))
	(setq *endofloopstack* (cdr *endofloopstack*))
	(return result)))

(defun fortreturn (stmt)
  (cond ((onep (length stmt))
	 (mkffortreturn))
	((not (eq (car *symboltable*) '*main*))
	 (append (mkffortassign (car *symboltable*) (cadr stmt))
		 (mkffortreturn)))
	(t
	 (gentranerr 'e
		     nil
		     "return not inside function - cannot be translated"
		     nil))))

(defun fortstmtgp (stmtgp)
  (progn
   (cond ((equal (car stmtgp) 'progn)
	  (setq stmtgp (cdr stmtgp)))
	 (t
	  (setq stmtgp (cddr stmtgp))))
   (foreach stmt in stmtgp conc (fortstmt stmt))))

(defun fortstmtno (label)
  (prog (stmtno)
	(cond ((not (setq stmtno (get label '*stmtno*)))
	       (setq stmtno (put label '*stmtno* (genstmtno)))))
	(return (mkffortcontinue stmtno))))

(defun fortstop (stmt)
  (declare (ignore stmt))
  (mkffortstop))

(defun fortwhile (exitcond body)
  (prog (n1 n2 result)
	(setq n1 (genstmtno))
	(setq n2 (genstmtno))
	(setq *endofloopstack* (cons n2 *endofloopstack*))
	(setq result (append (list n1 '| |) (mkffortifgo exitcond n2)))
	(indentfortlevel (+ 1))
	(setq result (append result (foreach st in body conc (fortstmt st))))
	(setq result (append result (mkffortgo n1)))
	(indentfortlevel (- 1))
	(setq result (append result (mkffortcontinue n2)))
	(cond ((listp (car *endofloopstack*))
	       (setq result
		     (append result
			     (mkffortcontinue (cadar *endofloopstack*))))))
	(setq *endofloopstack* (cdr *endofloopstack*))
	(return result)))

(defun fortwrite (stmt)
  (mkffortwrite (cdr stmt)))


;;                                     ;;
;;  fortran code formatting functions  ;;
;;                                     ;;


;;  statement formatting  ;;

(defun mkffortassign (lhs rhs)
  (append (append (cons (mkforttab) (fortexp lhs))
		  (cons '= (fortexp rhs)))
	  (list (mkterpri))))

(defun mkffortcall (fname params)
  (progn
   (cond (params
	  (setq params (append (append (list '|(|)
				        (foreach p in (insertcommas params)
						 conc (fortexp p)))
				(list '|)|)))))
   (append (append (list (mkforttab) 'call '| |)
		   (fortexp fname))
	   (append params (list (mkterpri))))))

(defun mkffortcontinue (stmtno)
  (list stmtno '| | (mkforttab) 'continue (mkterpri)))

(defun mkffortdec (type varlist)
  (progn
   (setq type (or type 'dimension))
   (setq varlist (foreach v in (insertcommas varlist)
		          conc (fortexp v)))
   (cond ((implicitp type)
          (append (list (mkforttab) type '| | '|(|)
		  (append varlist
		          (list '|)| (mkterpri)))))
	 (t
	  (append (list (mkforttab) type '| |)
	          (aconc varlist (mkterpri)))))))

(defun mkffortdo (stmtno var lo hi incr)
  (progn
   (cond ((onep incr)
          (setq incr nil))
	 (incr
	  (setq incr (cons '|,| (fortexp incr)))))
   (append (append (append (list (mkforttab) 'do '| | stmtno '| | )
		           (fortexp var))
		   (append (cons '= (fortexp lo))
		           (cons '|,| (fortexp hi))))
	   (append incr
	           (list (mkterpri))))))

(defun mkffortend ()
  (list (mkforttab) 'end (mkterpri)))

(defun mkffortgo (stmtno)
  (list (mkforttab) 'goto '| | stmtno (mkterpri)))

(defun mkffortifgo (exp stmtno)
  (append (append (list (mkforttab) 'if '| | '|(|)
		  (fortexp exp))
	  (list '|)| '| |  'goto '| |  stmtno (mkterpri))))


(defun mkffortread (var)
  (append (list (mkforttab) 'read '|(*,*)| '| | )
	  (append (fortexp var)
		  (list (mkterpri)))))

(defun mkffortreturn ()
  (list (mkforttab) 'return (mkterpri)))

(defun mkffortstop ()
  (list (mkforttab) 'stop (mkterpri)))

(defun mkffortsubprogdec (type stype name params)
  (progn
   (cond (params
	  (setq params
		(aconc (cons '|(|
			     (foreach p in (insertcommas params) conc
				      (fortexp p)))
		       '|)|))))
   (cond (type
	  (setq type (list (mkforttab) type '| |  stype '| | )))
	 (t
	  (setq type (list (mkforttab) stype '| | ))))
   (append (append type (fortexp name))
	   (aconc params (mkterpri)))))

(defun quotstring (arg)
(if (stringp arg) (compress (cons #\" (append (exploden arg) '(#\"))))  ;; -mds fix absent "..." in fortwrite of strings
        arg))

(defun mkffortwrite (arglist)
  (append (append (list (mkforttab) 'write '|(*,*)| '| | )
		  (foreach arg in (insertcommas (map 'list 'quotstring arglist)) conc (fortexp arg))) ;; -mds
	  (list (mkterpri))))

;;  indentation control  ;;

(defun mkforttab ()
  (list 'forttab (- $fortcurrind 6)))

(defun indentfortlevel (n)
  (setq $fortcurrind (+ $fortcurrind (* n $tablen))))




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
(put 'or       '*ratforop* "||")
(put 'and      '*ratforop* '|&|)
(put 'not      '*ratforop* '|!|)
(put 'equal    '*ratforop* '|==|)
(put 'notequal '*ratforop* '|!=|)
(put 'greaterp '*ratforop* '|>|)
(put 'geqp     '*ratforop* '|>=|)
(put 'lessp    '*ratforop* '|<|)
(put 'leqp     '*ratforop* '|<=|)
(put 'plus     '*ratforop* '|+|)
(put 'times    '*ratforop* '|*|)
(put 'quotient '*ratforop* '|/|)
(put 'expt     '*ratforop* '|**|)
(put 'minus    '*ratforop* '|-|)

;;                                        ;;
;;  lisp-to-ratfor translation functions  ;;
;;                                        ;;


;;  control function  ;;

(defun ratcode (forms)
  (foreach f in forms conc
	   (cond ((atom f)
		  (cond ((equal f '$begin_group)
			 (mkfratbegingp))
                        ((equal f '$end_group)
			 (mkfratendgp))
                        (t
			 (ratexpgen f))))
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
		  (ratexpgen f)))))

;;  subprogram translation  ;;

(defun ratsubprog (def)
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
			(cond ((or type (gfunctionp body name))
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

(defun ratdecs (decs)
  (foreach tl in (formtypelists decs) conc (mkfratdec (car tl) (cdr tl))))

;;  expression translation  ;;

(defun ratexpgen (exp)
   (if $dblfloat (map 'list 'dbl (ratexpgen1 exp 0))
  (ratexpgen1 exp 0)))

(defun ratexpgen1 (exp wtin)
  (cond ((atom exp) (list (ratforname exp)))
	((eq (car exp) 'literal) (ratliteral exp))
	((onep (length exp)) exp)
	((member (car exp) '(minus not) :test #'eq)
	 (let* ((wt (ratforprecedence (car exp)))
		(res (cons (ratforop (car exp)) (ratexpgen1 (cadr exp) wt))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	((or (member (car exp) *lisparithexpops* :test #'eq)
	     (member (car exp) *lisplogexpops* :test #'eq))
	 (let* ((wt (ratforprecedence (car exp)))
		(op (ratforop (car exp)))
		(res (ratexpgen1 (cadr exp) wt))
		(res1))
	       (setq exp (cdr exp))
	       (cond ((eq op '+)
		      (loop while (setq exp (cdr exp)) do
                         (progn
			  (setq res1 (ratexpgen1 (car exp) wt))
			  (cond ((or (eq (car res1) '-)
				     (and (numberp (car res1))
					  (minusp (car res1))))
				 (setq res (append res res1)))
				(t
				 (setq res (append res (cons op res1))))))))
		     (t
		      (loop while (setq exp (cdr exp)) do
                         (setq res (append res
					   (cons op
						 (ratexpgen1 (car exp) wt)))))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)|))
		     (t res))))
	(t
	 (let ((res (cons (car exp) (cons '|(| (ratexpgen1 (cadr exp) 0)))))
              (setq exp (cdr exp))
	      (loop while (setq exp (cdr exp)) do
                 (setq res (append res (cons '|,| (ratexpgen1 (car exp) 0)))))
              (aconc res '|)| )))))

(defun ratforname (name)
  (if (symbolp name) (or (get name '*ratforname*) name) name))

(defun ratforop (op)
  (or (get op '*ratforop*) op))

(defun ratforprecedence (op)
  (or (get op '*ratforprecedence*) 9))

;;  statement translation  ;;

(defun ratstmt (stmt)
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

(defun ratassign (stmt)
  (mkfratassign (cadr stmt) (caddr stmt)))

(defun ratbreak (stmt)
  (declare (ignore stmt))
  (mkfratbreak))

(defun ratcall (stmt)
  (mkfratcall (car stmt) (cdr stmt)))

(defun ratdo (var lo nextexp exitcond body)
  (prog (r hi incr)
	(setq hi
	      (car (delete1 'greaterp (delete1 'lessp (delete1 var exitcond)))))
	(setq incr (car (delete1 'plus (delete1 var nextexp))))
	(setq r (mkfratdo var lo hi incr))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (- 1))
	(return r)))

(defun ratend (stmt)
  (declare (ignore stmt))
  (mkfratend))

(defun ratforfor (var lo nextexp cond body)
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
	(indentratlevel (- 1))
	(return r)))

(defun ratgoto (stmt)
  (prog (stmtno)
	(setq stmtno (or (get (cadr stmt) '*stmtno*)
			 (put (cadr stmt) '*stmtno* (genstmtno))))
	(return (mkfratgo stmtno))))

(defun ratif (stmt)
  (prog (r st)
	(setq r (mkfratif (caadr stmt)))
	(indentratlevel (+ 1))
	(setq st (seqtogp (cdadr stmt)))
	(cond ((and (listp st)
		    (equal (car st) 'cond)
		    (equal (length st) 2))
	       (setq st (mkstmtgp 0 (list st)))))
	(setq r (append r (ratstmt st)))
	(indentratlevel (- 1))
	(setq stmt (cdr stmt))
	(loop while (and (setq stmt (cdr stmt))
			 (not (eq (caar stmt) t))) do
	      (setq r (append r (mkfratelseif (caar stmt))))
	      (indentratlevel (+ 1))
	      (setq st (seqtogp (cdar stmt)))
	      (cond ((and (listp st)
			  (equal (car st) 'cond)
			  (equal (length st) 2))
		     (setq st (mkstmtgp 0 (list st)))))
	      (setq r (append r (ratstmt st)))
	      (indentratlevel (- 1)))
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
		(indentratlevel (- 1)))))
	(return r)))

(defun ratliteral (stmt)
  (mkfratliteral (cdr stmt)))

(defun ratloop (stmt)
  (prog (var lo nextexp exitcond body)
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

(defun ratread (stmt)
  (mkfratread (cadr stmt)))

(defun ratrepeat (body exitcond)
  (prog (r)
	(setq r (mkfratrepeat))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (- 1))
	(return (append r (mkfratuntil exitcond)))))

(defun ratreturn (stmt)
  (mkfratreturn (cadr stmt)))

(defun ratstmtgp (stmtgp)
  (prog (r)
	(cond ((equal (car stmtgp) 'progn)
	       (setq stmtgp (cdr stmtgp)))
	      (t
	       (setq stmtgp (cddr stmtgp))))
	(setq r (mkfratbegingp))
	(indentratlevel (+ 1))
	(setq r (append r (foreach stmt in stmtgp conc (ratstmt stmt))))
	(indentratlevel (- 1))
	(return (append r (mkfratendgp)))))

(defun ratstmtno (label)
  (prog (stmtno)
	(setq stmtno (or (get label '*stmtno*)
			 (put label '*stmtno* (genstmtno))))
	(return (mkfratcontinue stmtno))))

(defun ratstop (stmt)
  (declare (ignore stmt))
  (mkfratstop))

(defun ratwhile (cond body)
  (prog (r)
	(cond (cond
	       (setq cond (list 'not cond))))
	(setq r (mkfratwhile cond))
	(indentratlevel (+ 1))
	(setq r (append r (ratstmt body)))
	(indentratlevel (- 1))
	(return r)))

(defun ratwrite (stmt)
  (mkfratwrite (cdr stmt)))


;;                                    ;;
;;  ratfor code formatting functions  ;;
;;                                    ;;


;;  statement formatting  ;;

(defun mkfratassign (lhs rhs)
  (append (append (cons (mkrattab) (ratexpgen lhs))
		  (cons '= (ratexpgen rhs)))
	  (list (mkterpri))))

(defun mkfratbegingp ()
  (list (mkrattab) '{ (mkterpri)))

(defun mkfratbreak ()
  (list (mkrattab) 'break (mkterpri)))

(defun mkfratcall (fname params)
  (progn
   (cond (params
	  (setq params (append (append (list '|(|)
				       (foreach p in (insertcommas params)
						conc (ratexpgen p)))
			       (list '|)|)))))
   (append (append (list (mkrattab) 'call '| |)
		   (ratexpgen fname))
	   (append params
		   (list (mkterpri))))))

(defun mkfratcontinue (stmtno)
  (list stmtno '| | (mkrattab) 'continue (mkterpri)))

(defun mkfratdec (type varlist)
  (progn
   (setq type (or type 'dimension))
   (setq varlist (foreach v in (insertcommas varlist) conc (ratexpgen v)))
   (cond ((implicitp type)
	  (append (list (mkrattab) type '| |  '|(|)
                  (append varlist (list '|)| (mkterpri)))))
	 (t
	  (append (list (mkrattab) type '| | )
		  (aconc varlist (mkterpri)))))))

(defun mkfratdo (var lo hi incr)
  (progn
   (cond ((onep incr)
	  (setq incr nil))
	 (incr
	  (setq incr (cons '|,| (ratexpgen incr)))))
   (append (append (append (list (mkrattab) 'do '| |)
			   (ratexpgen var))
		   (append (cons '|=| (ratexpgen lo))
			   (cons '|,| (ratexpgen hi))))
	   (append incr
		   (list (mkterpri))))))

(defun mkfratelse ()
  (list (mkrattab) 'else (mkterpri)))

(defun mkfratelseif (exp)
  (append (append (list (mkrattab) 'else '| | 'if '| | '|(|)
		  (ratexpgen exp))
	  (list '|)| (mkterpri))))

(defun mkfratend ()
  (list (mkrattab) 'end (mkterpri)))

(defun mkfratendgp ()
  (list (mkrattab) '} (mkterpri)))

(defun mkfratfor (var1 lo cond var2 nextexp)
  (progn
   (cond (var1
	  (setq var1 (append (ratexpgen var1) (cons '= (ratexpgen lo))))))
   (cond (cond
	  (setq cond (ratexpgen cond))))
   (cond (var2
	  (setq var2 (append (ratexpgen var2) (cons '= (ratexpgen nextexp))))))
   (append (append (append (list (mkrattab) 'for '| |  '|(|)
			   var1)
		   (cons semicolon cond))
	   (append (cons semicolon var2)
		   (list '|)| (mkterpri))))))

(defun mkfratgo (stmtno)
  (list (mkrattab) 'goto '| | stmtno (mkterpri)))

(defun mkfratif (exp)
  (append (append (list (mkrattab) 'if '| |  '|(|)
		  (ratexpgen exp))
	  (list '|)| (mkterpri))))

(defun mkfratliteral (args)
  (foreach a in args conc
	   (cond ((equal a '$tab) (list (mkrattab)))
		 ((equal a '$cr) (list (mkterpri)))
		 ((listp a) (ratexpgen a))
		 (t (list a)))))

(defun mkfratread (var)
  (append (list (mkrattab) 'read '|(*,*)| '| | )
	  (append (ratexpgen var) (list (mkterpri)))))

(defun mkfratrepeat ()
  (list (mkrattab) 'repeat (mkterpri)))

(defun mkfratreturn (exp)
  (cond (exp
	 (append (append (list (mkrattab) 'return '|(|) (ratexpgen exp))
		 (list '|)| (mkterpri))))
	(t
	 (list (mkrattab) 'return (mkterpri)))))

(defun mkfratstop ()
  (list (mkrattab) 'stop (mkterpri)))

(defun mkfratsubprogdec (type stype name params)
  (progn
   (cond (params
	  (setq params (aconc (cons '|(|
				    (foreach p in (insertcommas params)
					     conc (ratexpgen p)))
			      '|)|))))
   (cond (type
	  (setq type (list (mkrattab) type '| |  stype '| | )))
	 (t
	  (setq type (list (mkrattab) stype '| | ))))
   (append (append type (ratexpgen name))
	   (aconc params (mkterpri)))))

(defun mkfratuntil (logexp)
  (append (list (mkrattab) 'until '| |  '|(|)
	  (append (ratexpgen logexp) (list '|)| (mkterpri)))))

(defun mkfratwhile (exp)
  (append (append (list (mkrattab) 'while '| |  '|(|)
		  (ratexpgen exp))
	  (list '|)| (mkterpri))))

(defun mkfratwrite (arglist)
  (append (append (list (mkrattab) 'write '|(*,*)| '| | )
		  (foreach arg in (insertcommas (map 'list 'quotstring arglist)) conc (ratexpgen arg)))
	  (list (mkterpri))))

;;  indentation control  ;;

(defun mkrattab ()
  (list 'rattab $ratcurrind))

(defun indentratlevel (n)
  (setq $ratcurrind (+ $ratcurrind (* n $tablen))))



;;  ---------  ;;
;;  lspc.l     ;;    lisp-to-c translation module
;;  ---------  ;;

(put nil '*cname* 0)

(put 'or       '*cprecedence* 1)
(put 'and      '*cprecedence* 2)
(put 'equal    '*cprecedence* 3)
(put 'notequal '*cprecedence* 3)
(put 'greaterp '*cprecedence* 4)
(put 'geqp     '*cprecedence* 4)
(put 'lessp    '*cprecedence* 4)
(put 'leqp     '*cprecedence* 4)
(put 'plus     '*cprecedence* 5)
(put 'times    '*cprecedence* 6)
(put 'quotient '*cprecedence* 6)
(put 'not      '*cprecedence* 7)
(put 'minus    '*cprecedence* 7)
(put 'and      '*cop* '|&&|)
(put 'not      '*cop* '|!|)
(put 'equal    '*cop* '|==|)
(put 'notequal '*cop* '|!=|)
(put 'greaterp '*cop* '|>|)
(put 'geqp     '*cop* '|>=|)
(put 'lessp    '*cop* '|<|)
(put 'leqp     '*cop* '|<=|)
(put 'plus     '*cop* '|+|)
(put 'times    '*cop* '|*|)
(put 'quotient '*cop* '|/|)
(put 'minus    '*cop* '|-|)
(put 'or       '*cop* "||")
;;                                  ;;
;;  lisp-to-c transltion functions  ;;
;;                                  ;;


;;  control function  ;;

(defun ccode (forms)
  (foreach f in forms conc
	   (cond ((atom f)
		  (cond ((equal f '$begin_group) (mkfcbegingp))
			((equal f '$end_group) (mkfcendgp))
			(t (cexp f))))
		 ((or (lispstmtp f) (lispstmtgpp f))
		  (cond (*gendecs (prog (r)
					(setq r
					      (append
					       (cdecs (symtabget '*main*
								 '*decs*))
					       (cstmt f)))
					(symtabrem '*main* '*decs*)
					(return r)))
			(t (cstmt f))))
		 ((lispdefp f) (cproc f))
		 (t (cexp f)))))

;;  procedure translation  ;;

(defun cproc (def)
  (prog (type name params paramtypes vartypes body r)
	(setq name (cadr def))
	(setq body (cdddr def))
	(cond ((and body (equal body '(nil))) (setq body ())))
	(cond ((and (onep (length body))
		    (lispstmtgpp (car body)))
	       (progn
		(setq body (cdar body))
		(cond ((null (car body))
		       (setq body (cdr body)))))))
	(cond ((setq type (symtabget name name))
	       (progn
		(setq type (cadr type))
		(symtabrem name name))))
	(setq params (or (symtabget name '*params*) (caddr def)))
	(symtabrem name '*params*)
	(foreach dec in (symtabget name '*decs*) do
		 (cond ((member (car dec) params)
			(setq paramtypes (aconc paramtypes dec)))
		       (t
			(setq vartypes (aconc vartypes dec)))))
	(setq r (append (mkfcprocdec type name params)
			(cdecs paramtypes)))
	(cond (body
	       (setq r (append r (mkfcbegingp)))
	       (indentclevel (+ 1))
	       (cond (*gendecs (setq r (append r (cdecs vartypes)))))
	       (setq r (append r (foreach s in body conc (cstmt s))))
	       (indentclevel (- 1))
	       (setq r (append r (mkfcendgp)))))
	(cond (*gendecs
	       (progn
		(symtabrem name nil)
		(symtabrem name '*decs*))))
	(return r)))

;;  generation of declarations  ;;

(defun cdecs (decs)
  (foreach tl in (formtypelists decs) conc (mkfcdec (car tl) (cdr tl))))

;;  expression translation  ;;

(defun cexp (exp)
(if $dblfloat (map 'list 'dbl (cexp1 exp 0))
  (cexp1 exp 0)))

(defun cexp1 (exp wtin)
  (cond ((atom exp) (list (cname exp)))
	((eq (car exp) 'literal) (cliteral exp))
	((member (car exp) '(minus not) :test #'eq)
	 (let* ((wt (cprecedence (car exp)))
		(res (cons (cop (car exp)) (cexp1 (cadr exp) wt))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)| ))
		     (t res))))
	((eq (car exp) 'expt)
	 (append (cons 'pow (cons '|(| (cexp1 (cadr exp) 0)))
                 (aconc (cons '|,| (cexp1 (caddr exp) 0)) '|)| )))
	((or (member (car exp) *lisparithexpops* :test #'eq)
	     (member (car exp) *lisplogexpops* :test #'eq))
	 (let* ((wt (cprecedence (car exp)))
		(op (cop (car exp)))
		(res (cexp1 (cadr exp) wt))
		(res1))
	       (setq exp (cdr exp))
	       (cond ((eq op '+)
		      (loop while (setq exp (cdr exp)) do
			  (setq res1 (cexp1 (car exp) wt))
			  (cond ((or (eq (car res1) '-)
				     (and (numberp (car res1))
					  (minusp (car res1))))
				 (setq res (append res res1)))
				(t
				 (setq res (append res (cons op res1)))))))
		     (t
		      (loop while (setq exp (cdr exp)) do
                         (setq res (append res
					   (cons op
						 (cexp1 (car exp) wt)))))))
	       (cond ((< wt wtin) (aconc (cons '|(| res) '|)| ))
		     (t res))))
	((arrayeltp exp)
	 (let ((res (list (car exp))))
	      (loop while (setq exp (cdr exp)) do
                 (setq res (append res
				   (aconc (cons '|[| (cexp1 (car exp) 0)) '|]|))))
	      res))
	((onep (length exp)) (aconc (aconc exp '|(|) '|)| ))
	(t
	 (let ((res (cons (car exp) (cons '|(| (cexp1 (cadr exp) 0)))))
              (setq exp (cdr exp))
	      (loop while (setq exp (cdr exp)) do
                 (setq res (append res (cons '|,| (cexp1 (car exp) 0)))))
              (aconc res '|)|)))))

(defun cname (name)
  (if (symbolp name) (or (get name '*cname*) name) name))

(defun cop (op)
  (or (get op '*cop*) op))

(defun cprecedence (op)
  (or (get op '*cprecedence*) 8))

;;  statement translation  ;;

(defun cstmt (stmt)
  (cond ((null stmt) nil)
	((equal stmt '$begin_group) (mkfcbegingp))
	((equal stmt '$end_group) (mkfcendgp))
	((lisplabelp stmt) (clabel stmt))
	((equal (car stmt) 'literal) (cliteral stmt))
	((lispassignp stmt) (cassign stmt))
	((lispcondp stmt) (cif stmt))
	((lispbreakp stmt) (cbreak stmt))
	((lispgop stmt) (cgoto stmt))
	((lispreturnp stmt) (creturn stmt))
	((lispstopp stmt) (cexit stmt))
	((lispdop stmt) (cloop stmt))
	((lispstmtgpp stmt) (cstmtgp stmt))
	((lispdefp stmt) (cproc stmt))
	(t (cexpstmt stmt))))

(defun cassign (stmt)
  (mkfcassign (cadr stmt) (caddr stmt)))

(defun cbreak (stmt)
  (declare (ignore stmt))
  (mkfcbreak))

(defun cexit (stmt)
  (declare (ignore stmt))
  (mkfcexit))

(defun cexpstmt (exp)
  (append (cons (mkctab) (cexp exp))
	  (list semicolon (mkterpri))))

(defun cfor (var lo nextexp cond body)
  (prog (r)
	(cond (cond (setq cond (list 'not cond))))
	(cond ((equal nextexp '(nil))
	       (setq r (mkfcfor var lo cond var nil)))
	      (nextexp
	       (setq r (mkfcfor var lo cond var nextexp)))
	      (t
	       (setq r (mkfcfor var lo cond nil nil))))
	(indentclevel (+ 1))
	(setq r (append r (cstmt body)))
	(indentclevel (- 1))
	(return r)))

(defun cgoto (stmt)
  (mkfcgo (cadr stmt)))

(defun cif (stmt)
  (prog (r st)
	(setq r (mkfcif (caadr stmt)))
	(indentclevel (+ 1))
	(setq st (seqtogp (cdadr stmt)))
	(cond ((and (listp st)
		    (equal (car st) 'cond)
		    (equal (length st) 2))
	       (setq st (mkstmtgp 0 (list st)))))
	(setq r (append r (cstmt st)))
	(indentclevel (- 1))
	(setq stmt (cdr stmt))
	(loop while (and (setq stmt (cdr stmt)) 	       (progn
		    (not (eq (caar stmt) t)))) do
	       (progn
		(setq r (append r (mkfcelseif (caar stmt))))
		(indentclevel (+ 1))
		(setq st (seqtogp (cdar stmt)))
		(cond ((and (listp st)
			    (equal (car st) 'cond)
			    (equal (length st) 2))
		       (setq st (mkstmtgp 0 (list st)))))
		(setq r (append r (cstmt st)))
		(indentclevel (- 1))))
	(cond (stmt (progn
		     (setq r (append r (mkfcelse)))
		     (indentclevel (+ 1))
		     (setq st (seqtogp (cdar stmt)))
		     (cond ((and (listp st)
				 (equal (car st) 'cond)
				 (equal (length st) 2))
			    (setq st (mkstmtgp 0 (list st)))))
		     (setq r (append r (cstmt st)))
		     (indentclevel (- 1)))))
	(return r)))

(defun clabel (label)
  (mkfclabel label))

(defun cliteral (stmt)
  (mkfcliteral (cdr stmt)))

(defun cloop (stmt)
  (prog (var lo nextexp exitcond body)
	(cond ((complexdop stmt)
	       (return (cstmt (seqtogp (simplifydo stmt))))))
	(cond ((setq var (cadr stmt))
	       (progn
		(setq lo (cadar var))
		(cond ((equal (length (car var)) 3)
		       (setq nextexp (or (caddar var) (list 'nil)))))
		(setq var (caar var)))))
	(cond ((setq exitcond (caddr stmt))
	       (setq exitcond (car exitcond))))
	(setq body (seqtogp (cdddr stmt)))
	(cond ((and exitcond (not var))
	       (return (cwhile exitcond body)))
	      ((and var
		    (not lo)
		    (lisplogexpp nextexp)
		    (equal exitcond var))
	       (return (crepeat body nextexp)))
	      (t
	       (return (cfor var lo nextexp exitcond body))))))

(defun crepeat (body logexp)
  (prog (r)
	(setq r (mkfcdo))
	(indentclevel (+ 1))
	(setq r (append r (cstmt body)))
	(indentclevel (- 1))
	(return (append r (mkfcdowhile (list 'not logexp))))))

(defun creturn (stmt)
  (mkfcreturn (cadr stmt)))

(defun cstmtgp (stmtgp)
  (prog (r)
	(cond ((equal (car stmtgp) 'progn)
	       (setq stmtgp (cdr stmtgp)))
	      (t
	       (setq stmtgp (cddr stmtgp))))
	(setq r (mkfcbegingp))
	(indentclevel (+ 1))
	(setq r (append r (foreach stmt in stmtgp conc (cstmt stmt))))
	(indentclevel (- 1))
	(return (append r (mkfcendgp)))))

(defun cwhile (cond body)
  (prog (r)
	(cond (cond (setq cond (list 'not cond))))
	(setq r (mkfcwhile cond))
	(indentclevel (+ 1))
	(setq r (append r (cstmt body)))
	(indentclevel (- 1))
	(return r)))


;;                               ;;
;;  c code formatting functions  ;;
;;                               ;;


;;  statement formatting  ;;

(defun mkfcassign (lhs rhs)
  (append (append (cons (mkctab) (cexp lhs))
		  (cons '= (cexp rhs)))
	  (list semicolon (mkterpri))))

(defun mkfcbegingp ()
  (list (mkctab) '{ (mkterpri)))

(defun mkfcbreak ()
  (list (mkctab) 'break semicolon (mkterpri)))

(defun mkfcdec (type varlist)
  (progn
   (setq varlist
	 (foreach v in varlist collect
		  (cond ((atom v) v)
			(t (cons (car v)
				 (foreach dim in (cdr v) collect
					  (1+ dim)))))))
   (append (cons (mkctab)
		 (cons type
		       (cons '| |  (foreach v in (insertcommas varlist) conc
					  (cexp v)))))
	   (list semicolon (mkterpri)))))

(defun mkfcdo ()
  (list (mkctab) 'do (mkterpri)))

(defun mkfcdowhile (exp)
  (append (append (list (mkctab) 'while '| | '|(|)
		  (cexp exp))
	  (list '|)| semicolon (mkterpri))))

(defun mkfcelse ()
  (list (mkctab) 'else (mkterpri)))

(defun mkfcelseif (exp)
  (append (append (list (mkctab) 'else '| |  'if '| |  '|(|) (cexp exp))
		  (list '|)| (mkterpri))))

(defun mkfcendgp ()
  (list (mkctab) '} (mkterpri)))

(defun mkfcexit ()
  (list (mkctab) 'exit '|(| 0 '|)| semicolon (mkterpri)))

(defun mkfcfor (var1 lo cond var2 nextexp)
  (progn
   (cond (var1 (setq var1 (append (cexp var1) (cons '= (cexp lo))))))
   (cond (cond (setq cond (cexp cond))))
   (cond (var2 (setq var2 (append (cexp var2) (cons '= (cexp nextexp))))))
   (append (append (append (list (mkctab) 'for '| |  '|(|) var1)
		   (cons semicolon cond))
	   (append (cons semicolon var2)
		   (list '|)| (mkterpri))))))

(defun mkfcgo (label)
  (list (mkctab) 'goto '| |  label semicolon (mkterpri)))

(defun mkfcif (exp)
  (append (append (list (mkctab) 'if '| |  '|(|)
		  (cexp exp))
	  (list '|)| (mkterpri))))

(defun mkfclabel (label)
  (list label '|:| (mkterpri)))

(defun mkfcliteral (args)
  (foreach a in args conc
	   (cond ((equal a '$tab) (list (mkctab)))
		 ((equal a '$cr) (list (mkterpri)))
		 ((listp a) (cexp a))
		 (t (list a)))))

(defun mkfcprocdec (type name params)
  (progn
   (setq params
	 (aconc (cons '|(| (foreach p in (insertcommas params) conc
				   (cexp p)))
		'|)|))
   (cond (type (append (cons (mkctab) (cons type (cons '| |  (cexp name))))
		       (aconc params (mkterpri))))
	 (t (append (cons (mkctab) (cexp name))
		    (aconc params (mkterpri)))))))

(defun mkfcreturn (exp)
  (cond (exp
	 (append (append (list (mkctab) 'return '|(|) (cexp exp))
		 (list '|)| semicolon (mkterpri))))
	(t
	 (list (mkctab) 'return semicolon (mkterpri)))))

(defun mkfcwhile (exp)
  (append (append (list (mkctab) 'while '| |  '|(|)
		  (cexp exp))
	  (list '|)| (mkterpri))))

;;  indentation control  ;;

(defun mkctab ()
  (list 'ctab $ccurrind))

(defun indentclevel (n)
  (setq $ccurrind (+ $ccurrind (* n $tablen))))



;;  -----------  ;;
;;  intrfc.l     ;;    command parsing routines & control functions
;;  -----------  ;;



;;                               ;;
;;  1. command parsing routines  ;;
;;                               ;;


;;  command parsers  ;;

;; functions callable from Maxima append genoutpath to output filespecs -- mds

(defmspec $gentran (forms)
  ;                                                     ;
  ;  gentran(stmt1,stmt2,...,stmtn {,[f1,f2,...,fm]});  ;
  ;      -->                                            ;
  ;  (gentran (stmt1 stmt2 ... stmtn)                   ;
  ;           (f1 f2 ... fm))                           ;
  ;                                                     ;
  (prog (flist)
	(setq forms (reverse forms))
	(cond ((and (listp (car forms))
		    (listp (caar forms))
		    (equal (caaar forms) 'mlist))
	       (setq flist (cdar forms))
	       (setq forms (cdr forms))))
	(setq forms (cdr (reverse forms)))
	(return (gentran forms (setq flist (map 'list 'outpath  flist))))))


(defmfun $gentranout (&rest flist)
  ;                                                                     ;
  ;  gentranout(f1,f2,...,fn);  -->  (gentranoutpush (f1 f2 ... fn) t)  ;
  ;                                                                     ;
  (gentranoutpush (setq flist (map 'list 'outpath  flist)) t))

(defun gentranout (flist)
  ;                                                                       ;
  ;  (gentranout (f1 f2 ... fn))  -->  (gentranoutpush (f1 f2 ... fn) t)  ;
  ;                                                                       ;
  (gentranoutpush flist t))


(defmfun $gentranshut (&rest flist)
  ;                                                                 ;
  ;  gentranshut(f1,f2,...,fn);  -->  (gentranshut (f1 f2 ... fn))  ;
  ;                                                                 ;
  (gentranshut (setq flist (map 'list 'outpath  flist))))


(defmfun $gentranpush (&rest flist)
  ;                                                                        ;
  ;  gentranpush(f1,f2,...,fn);  -->  (gentranoutpush (f1 f2 ... fn) nil)  ;
  ;                                                                        ;
  (gentranoutpush (setq flist (map 'list 'outpath  flist)) nil))

(defun gentranpush (flist)
  ;                                                                          ;
  ;  (gentranpush (f1 f2 ... fn))  -->  (gentranoutpush (f1 f2 ... fn) nil)  ;
  ;                                                                          ;
  (gentranoutpush flist nil))


(defmfun $gentranpop (&rest flist)
  ;                                                               ;
  ;  gentranpop(f1,f2,...,fn);  -->  (gentranpop (f1 f2 ... fn))  ;
  ;                                                               ;
  (gentranpop (setq flist (map 'list 'outpath  flist))))


(defmfun $gentranin (&rest forms)
  ;                                              ;
  ;  gentranin(f1,f2,...,fn {,[f1,f2,...,fm]});  ;
  ;      -->                                     ;
  ;  (gentranin (f1 f2 ... fn) (f1 f2 ... fm))   ;
  ;                                              ;
  (prog (outflist)
	(setq forms (reverse forms))
	(cond ((and (listp (car forms))
		    (listp (caar forms))
		    (equal (caaar forms) 'mlist))
	       (setq outflist (cdar forms))
	       (setq forms (cdr forms))))
	(setq forms (reverse forms))
    (return (gentranin forms (setq outflist (map 'list 'outpath outflist))))))

;; cleanup function for when gentranin hangs ==mds courtesy of Macsyma inc.
(defun $gentraninshut () (popinstk) '$done)


(defmfun $gentran_on (&rest flaglist) ;; renamed consistent with Macsyma --mds
  ;                              ;
  ;  on(flag1,flag2,...,flagn);  ;
  ;    -->                       ;
  ;  (onoff flaglist t)          ;
  ;                              ;
  (onoff flaglist t))

(defun on (flaglist)
  ;                                          ;
  ;  (on flaglist)  -->  (onoff flaglist t)  ;
  ;                                          ;
  (onoff flaglist t))


(defmfun $gentran_off (&rest flaglist) ;; renamed consistent with Macsyma --mds
  ;                               ;
  ;  off(flag1,flag2,...,flagn);  ;
  ;    -->                        ;
  ;  (onoff flaglist nil)         ;
  ;                               ;
  (onoff flaglist nil))

(defun gentran_off (flaglist) ;; renamed consistent with Macsyma --mds
  ;                                             ;
  ;  (off flaglist)  -->  (onoff flaglist nil)  ;
  ;                                             ;
  (onoff flaglist nil))



;;                        ;;
;;  2. control functions  ;;
;;                        ;;


;;  command control functions  ;;


(defun gentran (forms flist)
  (prog ()
	(cond ((setq flist (preproc flist))
	       (eval (list 'gentranoutpush (list 'quote flist) nil))))
	(setq forms (preproc forms))
	(cond ($gentranparser (gentranparse forms)))
	(setq forms (franz forms))
	(cond ($gentranseg (setq forms (seg forms))))
	(cond ((eq (stripdollar1 $gentranlang) 'ratfor)
	       (formatrat (ratcode forms)))
	      ((eq (stripdollar1 $gentranlang) 'c)
	       (formatc (ccode forms)))
	      ((formatfort (fortcode forms))))
	(return (cond (flist
		       (progn
			(setq flist (or (car *currout*)
					(cons '(mlist) (cdr *currout*))))
			(eval '(gentranpop '(nil)))
			flist))
		      (t
		       (or (car *currout*)
			   (cons '(mlist)
				 (cdr *currout*))))))))


(defun gentranoutpush (flist outp)
  ;  open, [delete,] push  ;
  (prog (fp)
	(setq flist (fargstonames (preproc flist) t))
	(cond ((onep (length flist))
	       (progn
		(setq fp (or (filpr (car flist) *outstk*)
			     (mkfilpr (car flist))))
		(cond (outp (delstk fp)))
		(pushstk fp)))
	      (t
	       (progn
		(setq fp (foreach f in flist collect
				  (or (filpr f *outstk*) (mkfilpr f))))
		(cond (outp
		       (progn
			(foreach p in fp do (delstk p))
			(delstk (pfilpr flist *outstk*)))))
		(pushstk '(nil))
		(cond (outp
		       (foreach p in fp do (pushstk p)))
		      ((foreach p in fp do
				(cond ((not (member p *outstk*))
				       (pushstk p))))))
		(pushstk (cons nil flist)))))
	(resetstk *outstk*)
	(return (or (car *currout*)
		    (cons '(mlist) (cdr *currout*))))))


(defun gentranshut (flist)
  ;  close, delete, [output to t]  ;
  (prog (trm fp)
	(setq flist (fargstonames (preproc flist) nil))
	(cond ((onep (length flist))
	       (progn
		(setq trm (equal (car *currout*) (car flist)))
		(setq fp (filpr (car flist) *outstk*))
		(close (cdr fp))
		(delstk fp)
		(cond (trm (pushstk *stdout*)))))
	      (t
	       (progn
		(cond ((car *currout*)
		       (setq trm (member (car *currout*) flist)))
		      (t
		       (setq trm
			     (eval (cons 'and
					 (foreach f in (cdr *currout*) collect
						  (cond ((member f flist)
							 t))))))))
		(setq fp (foreach f in flist collect (filpr f *outstk*)))
		(foreach p in fp do (close (cdr p)))
		(foreach p in fp do (delstk p))
		(delstk (pfilpr flist *outstk*))
		(cond (trm (pushstk *stdout*))))))
	(resetstk *outstk*)
	(return (or (car *currout*)
		    (cons '(mlist) (cdr *currout*))))))


(defun gentranpop (flist)
  ;  [close,] delete  ;
  (prog (fp)
	(setq flist (preproc flist))
	(cond ((member '$all flist)
	       (loop while (> (length *outstk*) 1) do
		      (gentranpop '(nil)))
	       (return (car *currout*))))
	(setq flist (fargstonames flist nil))
	(cond ((onep (length flist))
	       (progn
		(setq fp (filpr (car flist) *outstk*))
		(popstk fp)
		(cond ((not (member fp *outstk*)) (close (cdr fp))))))
	      (t
	       (progn
		(setq fp (foreach f in flist collect (filpr f *outstk*)))
		(popstk (pfilpr flist *outstk*))
		(foreach p in fp do
			 (cond ((not (member p *outstk*))
				(close (cdr p))))))))
	(return (or (car *currout*)
		    (cons '(mlist)
			  (cdr *currout*))))))


(defun gentranin (inlist outlist)
  (prog (ogendecs) ;; disable declarations of tempvars in template --mds
        (setq ogendecs *gendecs)
        (setq *gendecs nil)
        (setq inlist (map 'list 'fsearch inlist)) ;; use filesearch to find input files --mds
	(foreach inf in (setq inlist (preproc inlist)) do
		 (cond ((listp inf)
                        (if (not inf)(gentranerr 'e inf "file not found in searchpath" nil)
			(gentranerr 'e inf "wrong type of arg" nil)))
		       
		       ((not(open (stripdollar inf) :direction :probe)) ;; rjf 11/1/2018
			(gentranerr 'e inf "nonexistent input file" nil))
		       
		       ))
	(cond (outlist
	       (eval (list 'gentranoutpush (list 'quote outlist) nil))))
	(foreach inf in inlist do
		 (progn
		  (cond ((equal inf (car *stdin*))
			 (pushinstk *stdin*))
			((filpr inf *instk*)
			 (gentranerr 'e
				     inf
				     "template file already open for input"
				     nil))
			(t
		        (pushinstk (cons inf (open inf
                                                  :direction :input)))))



		  (cond ((eq (stripdollar1 $gentranlang) 'ratfor) (procrattem))
			((eq (stripdollar1 $gentranlang) 'c) (procctem))
			(t (procforttem)))
		  (cond ((cdr *currin*) (close (cdr *currin*))))
		  (popinstk)))
        (setq *gendecs ogendecs) ;;re-enable gendecs --mds
	(return (cond (outlist
		       (progn
			(setq outlist (or (car *currout*)
					  (cons '(mlist) (cdr *currout*))))
			(eval '(gentranpop '(nil)))
			outlist))
		      (t
		       (or (car *currout*)
			   (cons '(mlist) (cdr *currout*))))))
                           ))



;;  misc. control functions  ;;



(defun onoff (flags onp)
  (foreach f in flags do
	   (prog (flag funlist)
		 (setq flag (setq f (stripdollar1 f)))
		 (setq f (implode (cons #\* (exploden f)))) ;;--mds
		 (set f onp)
		 (cond ((setq funlist (assoc onp (get flag 'simpfg)))
			(foreach form in (cdr funlist) do (eval form))))))
  '$done)



(defun $tempvar (type)
  (tempvar (stripdollar1 type)))


(defun $markvar (var)
  (markvar (stripdollar1 var))
  var)


(defun $markedvarp (var)
  (markedvarp (stripdollar1 var)))


(defun $unmarkvar (var)
  (unmarkvar (stripdollar1 var))
  '$done)


(defun $recurunmark (exp)
  (cond ((atom exp) (unmarkvar (stripdollar1 exp)))
	(t (foreach elt in exp do ($recurunmark elt))))
  '$done)


(defun $gendecs (name)
  (gendecs name))


;;  file arg conversion function  ;;


(defun fargstonames (args openp)
  (prog (names)
	(setq args
	      (foreach a in (if (listp args) args (list args)) conc
		       (cond ((member a '(nil 0))
			      (cond ((car *currout*)
				     (list (car *currout*)))
				    (t
				     (cdr *currout*))))
			     ((eq a 't)
			      (list (car *stdout*)))
			     ((eq a '$all)
			      (foreach fp in *outstk* conc
				       (cond ((and (car fp)
						   (not (equal fp *stdout*)))
					      (list (car fp))))))
			     ((atom a)
			      (cond (openp
				     (progn (list a)))
				    ((filpr a *outstk*)
				     (list a))
				    (t
				     (gentranerr 'w
						 a
						 "file not open for output"
						 nil))))
			     (t
			      (gentranerr 'e a "wrong type of arg" nil)))))


 (loop for z in args do  (if (not (member z names))(push z names)))
 (return (nreverse names))))


 



;;  mode switch control functions  ;;


(defun gentranswitch (lang)
  ;                   ;
  ;  on/off fortran;  ;
  ;  on/off ratfor;   ;
  ;  on/off c;        ;
  ;                   ;
  (prog (hlang flag exp)
	(setq hlang $gentranlang)
	(setq $gentranlang lang)
        (setq flag (implode (cons #\* (exploden lang)))) ;;--mds
	(loop while (eval flag) do
		(setq exp (gentranswitch1 (list (third (mread (cdr *currin*) nil))
						                   )))
		(eval (list 'gentran (list 'quote exp) 'nil)))
	(setq $gentranlang hlang)))

(defun gentranswitch1 (exp)
  (prog (r)
	(setq r (gentranswitch2 exp))
	(cond (r (return (car r)))
	      (t (return r)))))

(defun gentranswitch2 (exp)
  (cond ((atom exp)
	 (list exp))
((and (listp (car exp))
	      (member (caar exp) '(gentran_off $gentran_off))) ;;renamed --mds
	 (foreach f in (cdr exp) do
		  (onoff (list f) nil)))
	(t
	 (list (foreach e in exp conc (gentranswitch2 e))))))


(defun gendecs (name)
  ;                        ;
  ;  on/off gendecs;       ;
  ;                        ;
  ;  gendecs subprogname;  ;
  ;                        ;
  (progn
   (cond ((equal name 0)
	  (setq name nil)))
   (cond ((eq (stripdollar1 $gentranlang) 'ratfor)
	  (formatrat (ratdecs (symtabget name '*decs*))))
	 ((eq (stripdollar1 $gentranlang) 'c)
	  (formatc (cdecs (symtabget name '*decs*))))
	 ((formatfort (fortdecs (symtabget name '*decs*)))))
   (symtabrem name nil)
   (symtabrem name '*decs*)
   '$done))


;;  misc. control functions  ;;


(defun gentranpairs (prs)
  ;                                ;
  ;  gentranpairs dottedpairlist;  ;
  ;                                ;
  (progn
   (cond ((eq (stripdollar1 $gentranlang) 'ratfor)
	  (foreach pr in prs do
		   (formatrat (mkfratassign (car pr) (cdr pr)))))
	 ((eq (stripdollar1 $gentranlang) 'c)
	  (foreach pr in prs do
		   (formatc (mkfcassign (car pr) (cdr pr)))))
	 ((foreach pr in prs do
		   (formatfort (mkffortassign (car pr) (cdr pr))))))))



;;  --------  ;;
;;  pre.l     ;;    preprocessing module
;;  --------  ;;



(defun preproc (exp)
  (prog (r)
	(setq r (preproc1 exp))
	(cond (r (return (car r)))
	      (t (return r)))))


(defun preproc1 (exp)
  (cond ((atom exp)
	 (list exp))
	((or (atom (car exp))
	     (listp (caar exp)))
	 (list (foreach e in exp conc (preproc1 e))))
	((and $gentranopt
	   ;;   (member gentranopt* '(vaxima macsyma $vaxima $macsyma))
              (macassignp exp)
	      (not (macdefp exp))
	      (not (macnestassignp exp))
	      (not (macmatassignp exp)))
	 (setq exp (foreach e in exp conc (preproc1 e)))
	 (prog (lhs rhs tvarlist tvartype tassigns tvarname)
	       (setq lhs (cadr exp))
	       (setq rhs ($optimize (caddr exp)))
	       (cond ((macexpp rhs)
		      (return (list (list '(msetq) lhs rhs)))))
	       (setq rhs (cdr rhs))
	       (cond ((and (listp (car rhs))
			   (listp (caar rhs))
			   (equal (caaar rhs) 'mlist))
                           (setq tvarname $tempvarname)
                           (setq $tempvarname $optimvarname) ;; use optimvarname for tempvars generated by optimize --mds
		      (setq tvarlist (cdar rhs))
		      (setq rhs (cdr rhs))
		      (setq tvartype (getvartype (cond ((atom lhs) lhs)
						       (t (car lhs)))))
		      (foreach tv in tvarlist do
			       (prog (v)
				     (setq v (tempvar tvartype))
				     (markvar v)
				     (putprop tv v '*varname*)
				     (setq rhs (subst v tv rhs))))
		      (foreach tv in tvarlist do
			       (progn
				(unmarkvar (get tv '*varname*))
				(putprop tv nil '*varname*)))
		      (setq rhs (reverse rhs))
		      (setq tassigns (reverse (cdr rhs)))
		      (setq rhs (car rhs))
                      (setq $tempvarname tvarname)))
	       (cond (tassigns
		      (return (list (append1 (cons '(mprogn) tassigns)
					     (list '(msetq) lhs rhs)))))
		     (t
		      (return (list (list '(msetq) lhs rhs)))))))
	((member (stripdollar1 (caar exp)) '(lsetq rsetq lrsetq))
	 ; (($lsetq  ~) (name d1 d2 ... dn) exp)                               ;
	 ;   -->  ((msetq) ((name) (($eval) d1) (($eval) d2) ... (($eval) dn)) ;
         ;	           exp)                                                ;
	 ; (($rsetq  ~) var exp)                                               ;
	 ;   -->  ((msetq) var (($eval) exp))                                  ;
	 ; (($lrsetq ~) ((name) d1 d2 ... dn) exp)                             ;
	 ;   -->  ((msetq) (name (($eval) d1) (($eval) d2) ... (($eval) dn))   ;
         ;                 (($eval) exp))                                      ;
	 (prog (op lhs rhs)
	       (setq op (stripdollar1 (caar exp)))
	       (setq lhs (cadr exp))
	       (setq rhs (caddr exp))
	       (cond ((and (member op '(lsetq lrsetq))
			   (listp lhs))
		      (setq lhs
			    (cons (list (caar lhs))
				  (foreach s in (cdr lhs) collect
					   (list '($eval) s))))))
	       (cond ((member op '(rsetq lrsetq))
		      (setq rhs (list '($eval) rhs))))
	       (return (preproc1 (list '(msetq) lhs rhs)))))
	((equal (stripdollar1 (caar exp)) 'eval)
	 (preproc1 (meval (cadr exp))))
	((and (equal (caar exp) 'msetq)
	      (listp (caddr exp))
	      (listp (caaddr exp))
	      (equal (caaaddr exp) 'lambda))
	 ; store subprogram name & parameters in symbol table ;
	 (symtabput (stripdollar1 (cadr exp))
		    '*params*
		    (foreach p in (cdadaddr exp) collect (stripdollar1 p)))
	 (list (foreach e in exp conc (preproc1 e))))
	((equal (caar exp) 'mdefine)
	 ; store subprogram name & parameters in symbol table ;
	 (symtabput (stripdollar1 (caaadr exp))
		    '*params*
		    (foreach p in (cdadr exp) collect (stripdollar1 p)))
	 (list (foreach e in exp conc (preproc1 e))))
	((equal (stripdollar1 (caar exp)) 'type)
	  ; store type declarations in symbol table ;
	  (setq exp (car (preproc1 (cdr exp))))
	  (setq exp (preprocdec exp))
	  (foreach var in (cdr exp) do
		   (cond ((member (car exp) '(subroutine function))
			  (symtabput var '*type* (car exp)))
			 (t
			  (symtabput nil
				     (cond ((atom var) var)
					   (t (caar var)))
				     (cond ((atom var) (list (car exp)))
					   (t (cons (car exp) (cdr var))))))))
	 nil)
	((equal (stripdollar1 (caar exp)) 'body)
	 ; (($body) stmt1 stmt2 ... stmtn)                                  ;
	 ; -->                                                              ;
	 ; if a main (fortran or ratfor) program is being generated then    ;
	 ;    ((mprogn) stmt1 stmt2 ... stmtn (($stop)) (($end)))           ;
	 ; else if fortran or ratfor then                                   ;
	 ;    ((mprogn) stmt1 stmt2 ... stmtn ((mreturn)) (($end)))         ;
	 ; else c                                                           ;
	 ;    ((mprog) stmt1 stmt2 ... stmtn)                               ;
	 (cond ((eq (stripdollar1 $gentranlang) 'c)
		(preproc1 (cons '(mprog) (cdr exp))))
	       (t
		(progn
		 (setq exp (reverse (cons '(mprogn) (cdr exp))))
		 (cond ((equal (car *symboltable*) '*main*)
			(setq exp (cons '(($end)) (cons '(($stop)) exp))))
		       (t
			(setq exp (cons '(($end)) (cons '((mreturn)) exp)))))
		 (preproc1 (reverse exp))))))
	((member (stripdollar1 (caar exp)) '(subroutine function cprocedure))
	 ; store subprogram name, (subprogram type), (return value type),   ;
	 ; parameter list in symbol table                                   ;
	 ; (($subroutine/$function/$cprocedure) {&type} (($name) $p1..$pn)) ;
	 ;  --> ((mdefine) (($name) $p1..$pn))                              ;
	 (prog (decs)
	       (cond ((member (stripdollar1 $gentranlang) '(fortran ratfor) :test #'eq)
		      (setq decs (list `(($type) ,(caar exp)
						 ,(caaar (last exp)))))))
	       (cond ((equal (length exp) 3)
		      (setq decs
			    (aconc decs
				   `(($type) ,(cadr exp)
					     ,(caaar (last exp)))))))
	       (setq decs
		     (cons '(mdefine)
			   (cons (car (last exp))
				 decs)))
	       (return (preproc1 decs))))
	(t
	 (list (foreach e in exp conc (preproc1 e))))))

(defun preprocdec (arg) ;; tries to parse type declarations that look like expressions.  Better to use strings for types.
  ; $var  -->  var                           ;
  ; |$implicit type|  -->  implicit\ type    ;
  ; ((mtimes) $type int)  -->  type*int      ;
  ; ((mplus) $v1 ((mminus) $v2))  -->  v1-v2 ;
  (cond ((atom arg)
	 (stripdollar1 arg))
	((atom (car arg))
	 (foreach a in arg collect (preprocdec a)))
	((equal (caar arg) 'mtimes)
	 (intern (compress (append (append (exploden (stripdollar1 (cadr arg)))
					   (exploden #\*))
				   (exploden (caddr arg)))))) ;;--mds works but reverses case of declarations
	((equal (caar arg) 'mplus)
	 (intern (compress (append (append (exploden (stripdollar1 (cadr arg)))
					   (exploden #\-))
				   (exploden (stripdollar1 (cadaddr arg)))))))
	(t
	 (foreach a in arg collect (preprocdec a)))))


