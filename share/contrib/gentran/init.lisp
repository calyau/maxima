;=============================================================================
;    (c) copyright 1988	 Kent State University  kent, ohio 44242 
;		all rights reserved.
;
; Authors:  Paul S. Wang, Barbara Gates
; Permission to use this work for any purpose is granted provided that
; the copyright notice, author and support credits above are retained.
;=============================================================================

(cond ((null (getd 'wrs)) (include "convmac.l")))

(declare (special *gentran-dir tempvartype* tempvarname* tempvarnum* genstmtno*
	genstmtincr* *symboltable* *instk* *stdin* *currin* *outstk*
	*stdout* *currout* *outchanl* *lispdefops* *lisparithexpops*
	*lisplogexpops* *lispstmtops* *lispstmtgpops*))
(declare (special ccurrind* clinelen* fortcurrind* fortlinelen* genstmtincr*
	 genstmtno* *gentranlang gentranopt*
	  maxexpprintlen* ratcurrind* ratlinelen* tablen*
	  tempvarname* tempvarnum* tempvartype*))

;;  ---------  ;;
;;  init.l     ;;    declarations & initializations
;;  ---------  ;;


;;                                                                           ;;
;;  1. user-accessible commands, functions, operators, switches & variables  ;;
;;                                                                           ;;


;;  gentran commands  ;;

(declare (nlambda $gentran $gentranin $gentranout $gentranshut
			  $gentranpush $gentranpop $on $off))

;;  gentran functions  ;;

;;  gentran operators  ;;

;;  user-accessible primitive functions  ;;

;;  mode switches  ;;

(declare (special *c *fortran *gendecs *ratfor))

(setq *fortran nil)
(setq *ratfor  nil)
(setq *if* nil)
(setq *c       nil)
(setq *gendecs t)

(put 'fortran 'simpfg '((nil) (t (gentranswitch 'fortran))))
(put 'ratfor  'simpfg '((nil) (t (gentranswitch 'ratfor))))
(put 'c       'simpfg '((nil) (t (gentranswitch 'c))))
(put 'gendecs 'simpfg '((nil) (t (gendecs nil))))
;;  flags  ;;

(declare (special *float *gentranopt *gentranparser *gentranseg))

(setq *float         nil)
(setq *gentranopt    nil)
(setq *gentranparser nil)
(setq *gentranseg    t)

;;  user-accessible global variables  ;;

(setq *gentranlang    'fortran)
(setq gentranopt* 'vaxima)
(setq maxexpprintlen* 800)
(setq tempvarname*    't)
(setq tempvarnum*     0)
(setq tempvartype*    nil)
(setq genstmtno*      25000)
(setq genstmtincr*    1)
(setq fortcurrind*    6)
(setq ratcurrind*     0)
(setq ccurrind*       0)
(setq tablen*         4)
(setq fortlinelen*    72)
(setq ratlinelen*     80)
(setq clinelen*       80)

;;                                                   ;;
;;  2. system variables, operators & property lists  ;;
;;                                                   ;;


;;  global variables  ;;

(declare (special *cr* *currin* *currout* *endofloopstack* *errin* *errout*
	  *instk* *lisparithexpops* *lispdefops* *lisplogexpops*
	  *lispstmtgpops* *lispstmtops* *outchanl* *outstk* *reswds* *slash*
	  *stdin* *stdout* *symboltable*))

(setq *stdin*   (cons t piport))
(setq *instk*           (list *stdin*))
(setq *currin*          (car *instk*))
(setq *stdout*  (cons t poport))
(setq *outstk*          (list *stdout*))
(setq *currout*         (car *outstk*))
(setq *outchanl*        (list (cdr *currout*)))
(setq *errin*   piport)
(setq *errout*  errport)
(setq *symboltable*     (list '*main*))
(setq *endofloopstack*  ())
(setq *lisparithexpops* (list 'expt 'minus 'plus 'quotient 'times))
(setq *lisplogexpops*   (list 'and 'equal 'geqp 'greaterp 'leqp 'lessp 'not
			      'notequal 'or))
(setq *lispstmtops*     (list 'break 'cond 'do 'end 'go 'princ 'return 'setq
			      'stop))
(setq *lispstmtgpops*   (list 'prog 'progn))
(setq *lispdefops*      (list 'defun))
(setq *slash* (ascii 47))
(setq *cr*    (ascii 10))
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

