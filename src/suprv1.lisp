;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

;;(macsyma-module suprv)

;;note in converting this file (originally suprv.lisp) to common lisp
;;for the lisp machine, I removed a lot of the old stuff which did not
;;apply, and tried to eliminate any / quoting.  Most of the relevant
;;stuff is in system.lisp for the lispm and nil friends.--wfs

;; #+MACLISP is ITS, Twenex, or Multics MacLisp.
;; #+PDP10 is ITS or Twenex MacLisp.
;; #+LISPM is the Lisp Machine or the Lisp Machine compiler running on ITS.
;; #+MACLISP and #+LISPM indicate which system a piece of code is intended
;; "for", not which system the code is being compiled "in".
;; #+GC means include gctime messages, and ok to call GCTWA here and there.
;; #-MAXII means not needed in new macsyma I/O and system organization.

;; Setting BASE to 10 at compile time needed for LAP to work.


#-(or cl nil)
(eval-when (eval compile)
  (setq old-ibase *read-base* old-base *print-base*) 
  (setq *read-base* 10. *print-base* 10.))
#+cl
(eval-when (eval compile)
  (setq old-ibase *read-base* old-base *print-base*) 
  (setq *read-base* 10. *print-base* 10.))

(defmvar mopl nil)

(declare-top 
 (special m$+ gcflag gct $lasttime $parsetime $disptime
	  bindlist loclist errset $labels linelable $batcount $filesize
	  st rephrase $dispflag refchkl baktrcl rubout ttyheight
	  cntly newline dskfnp dsksavep *rset cntl@
	  ^w ^r ^q ^d lf tab ff cntlc alt batconl cr vt ^h ^s bsp
	  $values $functions $arrays $aliases $gradefs $dependencies
	  $rules $props $ratvars $ratvarswitch *mdebug* errbrksw errcatch
	  varlist genvar $device $filename $filenum lbp rbp
	  $gensumnum checkfactors $features featurel $backtrace
	  $weightlevels tellratlist $dontfactor $infolists loadfiles
	  $dskall errlist allbutl lisperrprint backrub
	  gc-daemon gc-overflow demonl $dynamalloc alloclevel infile
	  alarmclock $c18maxtime $fileid dcount gclinenum thistime
	  $nolabels $batchkill dispflag saveno mcatch brklvl savefile
	  st1 stime0 $%% $error
	  *in-$batchload* *in-translate-file*
	  lessorder greatorder $errorfun mbreak reprint pos $strdisp
	  $dskuse smart-tty rubout-tty more-^w oldst alphabet
	  $loadprint ttyints opers
	  *ratweights $ratweights quitmsg mquitmsg contmsg
	  loadf display-file $grind scrollp $cursordisp
	  stringdisp $lispdisp mexprp defaultf reading
	  bporg gcsyml ^amsg ^bmsg ^hmsg
	  state-pdl promptmsg gcprompt command printmsg mrg-punt
	  new-c-line-hook transp $contexts $setcheck $macros
	  undf-fnctn autoload)
 #+cl  (special error-call)
 ;;#+Franz  (special ptport display-to-disk)
 ;;	 (*EXPR REPRINT)
 ;;	 (*LEXPR CONCAT $FILEDEFAULTS $PRINT)
 ;;	 (FIXNUM $FILESIZE DCOUNT $BATCOUNT I N N1 N2 TTYHEIGHT
 ;;		 $FILENUM THISTIME GCT TIM GCLINENUM ALLOCLEVEL
 ;;		 BRKLVL CMTCNT BPORG BPORG0 #-cl (COMPUTIME FIXNUM FIXNUM)
 ;;		 #-cl (CASIFY FIXNUM) #-cl (GETLABCHARN))
 ;;	 (FLONUM U1 STIME0)
 ;;	 (NOTYPE (ASCII-NUMBERP FIXNUM))
 ;;	 (ARRAY* (FIXNUM DISPLAY-FILE 1))
 )


;; This affects the runtime environment.  ALJABR;LOADER also does this, but
;; leave it here for other systems.  On the Lisp Machine, this is bound
;; per stack group.

(defmvar $prompt
    '_
  nil
  no-reset)

;;(eval-when (compile eval load)
;;  (defun control-char (ch)
;;    (code-char (+ (char-code #\) (- (char-code ch) (char-code #\A))))))

(progn (mapc #'(lambda (x) (putprop (car x) (cadr x) 'opalias))
	     '((+ $+) (- $-) (* $*) (// $//) (^ $^) (|.| |$.|) (< $<) (= $=)
	       (> $>) (|(| |$(|) (|)| |$)|) (|[| |$[|) (|]| |$]|) (|,| |$,|) (|:| |$:|)
	       (|!| |$!|) (|#| |$#|) (|'| |$'|) (|;| |$;|)))
       (mapc #'(lambda (x) (set (car x)
				(cond ((char< (cadr x) #.(code-char 160.))
				       (ascii (cadr x)))
				      (t (cadr x)))))
	     '((cntl@ #.(code-char 0))
	       (cntlc #.(code-char 3))
	       (bsp #\backspace) (tab #\tab) (lf #\linefeed)
	       (vt #.(code-char 11))
	       (ff #\page) (cr #\return)
	       (cntly #.(code-char 25))
	       (sp #\space)
	       (newline #\newline) (rubout #\rubout)))
       (setq gcsyml nil)
       (dotimes (i 14.) (push (gensym) gcsyml))
       #-cl  (setq alt #-multics (intern (string #\escape)) #+multics '&)
       #-cl (setq $plotundefined (*$ 2.0 -8.5070591e+37))
       (setq $lasttime '((mlist) 0 0) thistime 0 gct 0 gcflag nil
	     $parsetime nil $disptime nil mexprp nil)
       (setq batconl nil $batcount 0 $batchkill nil $strdisp t $grind nil)
       (setq refchkl nil *mdebug* nil baktrcl nil errbrksw nil mbreak nil $errorfun nil
	     errcatch nil demonl (list nil) mcatch nil brklvl -1
	     allbutl nil loadf nil $backtrace '$backtrace)
       (setq *in-$batchload* nil *in-translate-file* nil)
       (setq backrub nil)
       (setq $debugmode nil $bothcases t
	     $pagepause nil $dskgc nil $poislim 5)
       (setq $loadprint nil ^s nil loadfiles nil)
       ;;      (SETQ $FILEID NIL $C18MAXTIME 150.0E6)
       (setq $nolabels nil $aliases '((mlist simp)) lessorder nil greatorder nil)
       (setq $infolists
	     (purcopy '((mlist simp) $labels $values $functions $macros $arrays
			$myoptions $props $aliases $rules $gradefs
			$dependencies $let_rule_packages)))
       (setq $labels (list '(mlist simp)))
       (setq $dskuse nil $device '$dsk $dispflag t linelable nil)
       (setq rephrase nil st nil oldst nil reprint nil pos nil)
       (setq dcount 0 $filenum 0 $storenum 1000. $filesize 16. $dskall t
	     new-c-line-hook nil dskfnp nil ttyints t
	     gclinenum 0 dsksavep nil saveno 0 $dynamalloc nil alloclevel 0)
       (setq quitmsg  " "
	     mquitmsg " (Into LISP.  Type control-G to get to MACSYMA.)" 
	     contmsg  "(Type <space> to continue, <return> to terminate.)"
	     ^amsg    "  (Type EXIT; to exit.)"
	     ^bmsg   #-multics "LISP  (Type <Alt>P<Space> to continue.)" 
	     #+multics "LISP  (Type <Dollarsign>P<Carriage Return> to continue)"
	     ^hmsg "
 (Use the RUBOUT or DEL(ETE) key to erase a character.)" ^dmsg-on "
 (Printout of GC statistics turned on.  Type control-D again to turn them off.)
"	     ^dmsg-off "
 (Printout of GC statistics turned off.)
"	     gcprompt "Type ALL; NONE; a level-no. or the name of the space.
"	     more-^w nil 
	     lisperrprint t printmsg nil promptmsg nil mrg-punt nil reading nil)
       ;;      (SETQ $CALCOMPNUM 100.)
       (setq state-pdl (purcopy (ncons 'lisp-toplevel)))
       ;;       #+MULTICS (SETQ $PLOT3DSIZE 20 $MULTGRAPH T)
					; Slashify ':' on printout on other systems for the benefit of Lispm.
;;; Figure out how to do the above for Franz.
       '(random properties))

(defmvar $% '$% "The last out-line computed, corresponds to lisp *"
	 no-reset)

(defmvar $inchar '$%i
  "The alphabetic prefix of the names of expressions typed by the user.")

(defmvar $outchar '$%o
  "The alphabetic prefix of the names of expressions returned by the system.")

(defmvar $linechar '$%t
  "The alphabetic prefix of the names of intermediate displayed expressions.")

(defmvar $linenum 1 "the line number of the last expression."
	 fixnum no-reset)

(defmvar $direc 'jrmu
  "The default file directory for SAVE, STORE, FASSAVE, and STRINGOUT."
  no-reset)

(defmvar casep t
  "Causes translation of characters from lower to upper case on ITS, 
   and from upper to lower case on Multics and Franz.")
;;(DEFMVAR $ERREXP '$ERREXP)

(defmvar user-timesofar nil)



(defvar moremsg "--Pause--")
(defvar morecontinue "--Continued--")
(defvar moreflush nil)
(defmvar $morewait nil "needs to be documented" no-reset)

(defmvar $showtime nil)

(defmvar aliaslist nil
  "is used by the MAKEATOMIC scheme which has never been completed"
  no-reset)

;;(declare-top (SETQ *print-base* 8))


(defun sys-gctime ()
  (status gctime))


;;#.(SETQ NALT #-MULTICS #\ALT #+MULTICS #\&)

(defmvar $file_string_print t
  "If TRUE, filenames are output as strings; if FALSE, as lists.")

(defmvar $showtime nil)

(defmfun meval* (test)
  (let (refchkl baktrcl checkfactors)
    (prog2 (if $ratvarswitch (setq varlist (cdr $ratvars)))
	(meval test)
      (clearsign))))

(defmfun makelabel (x)
  (when (and $dskuse (not $nolabels) (> (setq dcount (f1+ dcount)) $filesize))
    (setq dcount 0) (dsksave))
  (setq linelable (concat x $linenum))
  (if (not $nolabels)
      (if (or (null (cdr $labels))
	      (when (memq linelable (cddr $labels))
		(delq linelable $labels 1) t)
	      (not (eq linelable (cadr $labels))))
	  (setq $labels (cons (car $labels) (cons linelable (cdr $labels))))))
  linelable)

(defmfun printlabel nil
  (mtell-open "(~A) " (maknam (cdr (exploden linelable)))))

(defmfun mexploden (x)
  (let (*print-radix*
	(*print-base* 10))
    (exploden x)))

(defmfun addlabel (label)
  (setq $labels (cons (car $labels) (cons label (delq label (cdr $labels) 1)))))

(defmfun tyi* nil
  (clear-input)
  (do ((n (tyi) (tyi))) (nil)
    (cond ((or (char= n #\newline) (and (> (char-code n) 31) (not (char= n #\rubout))))
	   (return n))
	  ((char= n #\page) (format t "~|") (throw 'retry nil)))))

(defun continuep nil
  (loop
   (catch 'retry
     (unwind-protect
	  (progn
	    (fresh-line)
	    (princ (stripdollar $prompt))
	    (finish-output)
	    (return (char= (tyi*) #\newline)))
       (clear-input)))))

(defun checklabel (x)	; CHECKLABEL returns T iff label is not in use
  (not (or $nolabels (= $linenum 0) (boundp (concat x $linenum)))))

(defun gctimep (timep tim)
  (cond ((and (eq timep '$all) (not (zerop tim))) (princ "Totaltime= ") t)
	(t (princ "Time= ") nil)))

;; If $BOTHCASES is T, lower case letters will not be converted to upper case.

(defmfun $bothcases (x) (bothcases1 nil x))

(defun bothcases1 (symbol value)
  symbol			;Always bound to $BOTHCASES.  Ignored.
  ;; This won't work with the Lisp Machine reader.
  ;; #+MacLisp (DO ((I 97. (f1+ I))) ((> I 122.))
  ;;	       (SETSYNTAX I (IF VALUE 1 321.) (IF VALUE I (f- I 32.))))
  (setq casep (not value)) value)

;;(DEFUN BACKSPACE1 (NIL X)
;; (COND (X (ADD2LNC 8 ALPHABET)
;;	  (SETSYNTAX 8 322. NIL))
;;       (T (DELETE 8 ALPHABET 1)
;;	  (SETSYNTAX 8 131392. NIL)))
;; (SETQ BSPP X))

#+cl
(defun listen () 0)			; Doesn't exist yet.

(defun display* (&aux (ret nil) (tim 0))
  #+gc (if (eq gcflag '$all) (let (^d) (gc)))
  (setq tim (runtime)
	ret (let ((errset 'errbreak2) (thistime -1))
	      (errset (displa (list '(mlable) linelable $%)))))
  (if (null ret) (mtell "~%Error during display~%"))
  (if $disptime (mtell-open "Displaytime= ~A msec.~%" (- (runtime) tim)))
  ret)


(defmfun rubout* (stg)
  (let (#.ttyoff #.writefilep)
    (cond (rubout-tty
	   (cond ((or reprint (null stg)
		      (char= (car stg) #\return) (char= (car stg) #\tab))
		  (cond (smart-tty
			 (cursorpos (car pos) (cdr pos)) (cursorpos 'l)
			 (if (cdr stg) (princ (maknam (reverse (cdr stg)))))
			 (setq reprint nil))
			((or reprint stg) (reprint (cdr stg) nil))))
		 (t (cursorpos 'x))))
	  (stg (tyo (car stg))))))


(defmfun reprint (stg ffp)
  (let (#.ttyoff #.writefilep)
    (if (not ffp) (mterpri))
    (case (car state-pdl)
      (macsyma-toplevel (printlabel))
      (retrieve (if (eq mrg-punt 'break) (princ (stripdollar $prompt)))))
    (setq pos (cursorpos))
    (if stg (princ (maknam (reverse stg))))
    (setq reprint nil)))

;; The PDP10 is one of the only systems which autoload.
;; The definition for non-autoloading systems is in MAXMAC. - CWH
;; For now we'll let a USER put autoload properties on symbols
;; and at least let them get found on Multics. - Jim 3/24/81
;; Franz also autoloads -- jkf
;;

#+cl
(defun generic-autoload (file &aux type)
  (setq file (pathname (cdr file)))
  (setq type (pathname-type file))
  (cond ((member type
		 '(nil "BIN" "O" "o" "XFASL" "QFASL" "LISP" "LSP") :test 'equalp)
	 (load file))
	(t ($batchload file))))
#+cl
(defvar autoload 'generic-autoload)


#+(or franz maclisp nil cl)
(defmfun load-function (func mexprp)	; The dynamic loader
  (let ((file (get func 'autoload)))
    (if file (funcall autoload (cons func file)))))

(defmfun load-file (file) ($load (to-macsyma-namestring file)))

(defmspec $loadfile (form)
  (loadfile (filestrip (cdr form)) nil
	    (not (memq $loadprint '(nil $autoload)))))



;;#-(or Franz cl cl)
;;(DEFMSPEC $SETUP_AUTOLOAD (L)
;;  (SETQ L (CDR L))
;;  (show l)
;;  (IF (NULL (CDR L)) (WNA-ERR '$SETUP_AUTOLOAD))
;;  (LET ((FILE #-PDP10 ($FILE_SEARCH ($FILENAME_MERGE 
;;				     (CAR L)
;;				     (USER-WORKINGDIR-PATHNAME)))
;;	      #+PDP10 (NAMELIST (MERGEF ($FILENAME_MERGE (CAR L))
;;					`((DSK ,(STATUS UDIR)) NOFILE)))))					
;;    (DOLIST (FUNC (CDR L))
;;	    (NONSYMCHK FUNC '$SETUP_AUTOLOAD)
;;	    (PUTPROP (SETQ FUNC (DOLLARIFY-NAME FUNC)) FILE 'AUTOLOAD)
;;	    (ADD2LNC FUNC $PROPS)))
;;  '$DONE)

#+cl
(defun $setup_autoload (filename &rest functions)
  (let ((file  (string-trim "&$" filename)))
    (dolist (func functions)
      (nonsymchk func '$setup_autoload)
      (putprop (setq func (dollarify-name func)) file 'autoload)
      (add2lnc func $props)))
  '$done)

(defmfun dollarify (l)
  (let ((errset 'errbreak1))
    (cons '(mlist simp)
	  (mapcar #'(lambda (x)
		      (let (y)
			(cond ((numberp x) x)
			      ((numberp (setq y (car (errset
						      (readlist
						       (mexploden x))
						      nil))))
			       y)
			      (t (makealias x)))))
		  l))))

(defmfun mfboundp (func)
  (or (mgetl func '(mexpr mmacro))
      (getl func '(translated-mmacro mfexpr* mfexpr*s))))

(defmfun filenamel (file)
  (cond ((atom file) (setq file (ncons file)))
	(($listp file) (setq file (cdr file)))
	(t (merror "Not a proper filename ~M" file)))
  (filestrip file))


#+cl		   ; This is quite different from the Maclisp version.
(defmfun loadfile (file findp printp &aux (saveno 0))
  (and findp (memq $loadprint '(nil $loadfile)) (setq printp nil))
  ;; Should really get the truename of FILE.
  (if printp (format t "~%~A being loaded.~%" file))
  (let* ((path (pathname file))
	 (tem (errset (load (pathname file)))))
    (or tem (merror "Load failed for ~A" (namestring path)))
    (namestring path)))

(defun $directory (path)
  (cons '(mlist) (mapcar 'namestring (directory ($filename_merge path))))
  )

(defmfun truefname (file)
  (probe-file file))
					;#-cl (CLOSE (OPEN FILE '(IN FIXNUM)))
					; The OPEN is to generate the appropriate error handling.
					; The CLOSE is just to be nice.
					;#+Multics FILE
					; The Multics CLOSE function returns T always. 
					; At least we know we can open and close the file.
					; On Multics PROBE-FILE calls ALLFILES which demands access to
					; the directory. 


#+cl
(defmfun mtruename (stream)
  (declare (ignore stream))
  ;;  (MFILE-OUT (UNEXPAND-PATHNAME (FUNCALL STREAM ':NAME))))
  (merror "Unimplemented!"))

(defmfun carfile (file)		       ; FILE is in OldIO list format.
  (if (= (length file) 3) (cdr file) file))

;; SPECP is T if the file is being batched for TRANSL, or $LOAD, 
;;	or some other special purpose.
#-franz
(defmacro filepos-check () `(if specp (setq filepos (filepos file-obj))))

(defmspec $kill (form) (mapc #'kill1 (cdr form)) #+gc (gctwa) '$done)

(defvar $dont_kill_symbols_with_lisp_source_files  t "Prevents killing functional properties 
 of items which have been translated and loaded")

(defmfun kill1 (x)
  (funcall #'(lambda (z)
	       (cond ((and allbutl (memq x allbutl)))
		     ((eq (setq x (getopr x)) '$labels)
		      (dolist (u (cdr $labels))
			(cond ((and allbutl (memq u allbutl))
			       (setq z (nconc z (ncons u))))
			      (t (makunbound u) (remprop u 'time)
				 (remprop u 'nodisp))))
		      (setq $labels (cons '(mlist simp) z) $linenum 0 dcount 0))
		     ((memq x '($values $arrays $aliases $rules $props $let_rule_packages))
		      (mapc #'kill1 (cdr (symbol-value x))))
		     ((memq x '($functions $macros $gradefs $dependencies))
		      (mapc #'(lambda (y) (kill1 (caar y))) (cdr (symbol-value x))))
		     ((eq x '$myoptions))
		     ((eq x '$tellrats) (setq tellratlist nil))
		     ((eq x '$ratweights) (setq *ratweights nil $ratweights '((mlist simp))))
		     ((eq x '$features)
		      (cond ((not (equal (cdr $features) featurel))
			     (setq $features (cons '(mlist simp) (copy-top-level featurel ))))))
		     ((or (eq x t) (eq x '$all))
		      (mapc #'kill1 (cdr $infolists))
		      (setq $ratvars '((mlist simp)) varlist nil genvar nil
			    checkfactors nil greatorder nil lessorder nil $gensumnum 0
			    $weightlevels '((mlist)) *ratweights nil $ratweights '((mlist simp))
			    tellratlist nil $dontfactor '((mlist)) $setcheck nil)
		      (killallcontexts))
		     ((setq z (assq x '(($clabels . $inchar) ($dlabels . $outchar)
					($elabels . $linechar))))
		      (mapc #'(lambda (y) (remvalue y '$kill)) (getlabels* (eval (cdr z)) nil)))
		     ((and (eq (ml-typep x) 'fixnum) (not (< x 0))) (remlabels x))
		     ((and $dont_kill_symbols_with_lisp_source_files
			   (symbolp x)(or (get x 'translated)
					  (and (fboundp x)
					       #-sbcl
					       (compiled-function-p
						(symbol-function x))
					       #+sbcl
					       (symbolp
						(nth-value 
						 2
						 (function-lambda-expression
						  (symbol-function x))))))))
		     ((atom x)
		      (setq z (or (and (memq x (cdr $aliases)) (get x 'noun)) (get x 'verb)))
		      (cond ((or (null allbutl) (not (memq z allbutl)))
			     (remvalue x '$kill) (remcompary x)
			     (if (memq x (cdr $contexts)) ($killcontext x))
			     (if (mget x '$rule)
				 (let ((y (ruleof x)))
				   (cond (y ($remrule y x))
					 (t (fmakunbound x)
					    (delq x $rules 1)))))
			     (if (and (get x 'operators) (rulechk x)) ($remrule x '$all))
			     (if (mget x 'trace) (macsyma-untrace x))
			     (when  (get x 'translated)
			       (remove-transl-fun-props x) 
			       (remove-transl-array-fun-props x))
			     (if (not (get x 'sysconst)) (remprop x 'mprops))
			     (dolist (u '(bindtest nonarray evfun evflag opers special mode))
			       (remprop x u))
			     (dolist (u opers)
			       (if (and (remprop x u)
					(eq (get x 'operators) 'simpargs1))
				   (remprop x 'operators)))
			     (when (memq x (cdr $props))
			       (remprop x 'sp2) (killframe x)
			       (let ((y (stripdollar x)))
				 (remprop y 'alphabet) (zl-delete (getcharn y 1) alphabet 1)))
			     (let ((y (get x 'op)))
			       (if (and y (not (memq y mopl)) (memq y (cdr $props)))
				   (kill-operator x)))
			     (remalias x nil) (delq x $arrays 1) (rempropchk x)
			     ;;		 #+MACLISP (ARGS X NIL)
			     (zl-delete (zl-assoc (ncons x) $functions) $functions 1)
			     (zl-delete (zl-assoc (ncons x) $macros) $macros 1)
			     (let ((y (zl-assoc (ncons x) $gradefs)))
			       (when y (remprop x 'grad) (zl-delete y $gradefs 1)))
			     (zl-delete (zl-assoc (ncons x) $dependencies) $dependencies 1)
			     (if z (kill1 z)))))
		     ((and (eq (caar x) 'mlist) (eq (ml-typep (cadr x)) 'fixnum)
			   (or (and (null (cddr x)) (setq x (append x (ncons (cadr x)))))
			       (and (eq (ml-typep (caddr x)) 'fixnum) (not (> (cadr x) (caddr x))))))
		      (let (($linenum (caddr x))) (remlabels (f- (caddr x) (cadr x)))))
		     ((setq z (mgetl (caar x) '(hashar array))) (remarrelem z x))
		     ((and (eq (caar x) '$allbut)
			   (not (dolist (u (cdr x)) (if (not (symbolp u)) (return t)))))
		      (let ((allbutl (cdr x))) (kill1 t)))
		     (t (improper-arg-err x '$kill))))
	   nil))


(defmfun remlabels (n)
  (prog (l x)
     (setq l (list (exploden $inchar) (exploden $outchar) (exploden $linechar)))
     loop (setq x (mexploden $linenum))
     (do ((l l (cdr l)))( (null l)) (remvalue (implode (append (car l) x)) '$kill))
     (if (or (minusp (setq n (f1- n))) (= $linenum 0)) (return nil))
     (setq $linenum (f1- $linenum))
     (go loop)))

(defmfun remvalue (x fn)
  (cond ((not (symbolp x)) (improper-arg-err x fn))
	((boundp x)
	 (let (y)
	   (cond ((or (setq y (memq x (cdr $values))) (memq x (cdr $labels)))
		  (cond (y (delq x $values 1))
			(t (delq x $labels 1)
			   (remprop x 'time) (remprop x 'nodisp)
			   (if (not (zerop dcount)) (setq dcount (f1- dcount)))))
		  (makunbound x) t)
		 ((get x 'special) (makunbound x) t)
		 (transp (set x x) t)
		 ((eq x '$default_let_rule_package) t)
		 (t (mtell "Warning: Illegal REMVALUE attempt:~%~M" x) nil))))))

(defmfun ruleof (rule)
  (or (mget rule 'ruleof)
      (let ((op (caaadr (mget rule '$rule))) l)
	(and (setq l (get op 'rules)) (memq rule l) op))))

(defmfun $debugmode (x) (debugmode1 nil x))


#-nil
(defun debugmode1 (assign-var y)
  (declare (ignore assign-var))
  ;; #+MACLISP (SETQ DEBUG (COND (Y (*RSET T) Y) (T (*RSET NIL))))
  ;; #+Franz   (prog2 (setq debug y) (debugging y))
  #+akcl (if (eq y '$lisp) (si::use-fast-links y))
  #+cl  (setq *mdebug* (setq *rset y)))

#+cl
(defun retrieve1 (a b &aux (eof '(nil)))
  (let ((*mread-prompt* b) r )
    (declare (special *mread-prompt*))
    (catch 'macsyma-quit
      (tagbody
       top
	 (setq r    (dbm-read (or a *terminal-io*) nil eof))
	 (cond ((and (consp r) (keywordp (car r)))
		(let ((value (break-call (car r) (cdr r) 'break-command)))
		  (if (eq value :resume) (return-from retrieve1 '$exit))
		  (go top))))
	      
	 )
      )
    (nth 2 r)
    ))

#-nil
(defmfun errbreak (y)		       ; The ERRSET interrupt function
  (cond
    (*mdebug*
     ((lambda (brklvl varlist genvar errbrkl linelable)
	(declare (special $help))
	(prog (x ^q #.ttyoff o^r  ;#+MACLISP ERRSET #+LISPM ERROR-CALL
	       tim $%% 
	       ;;	    #+Franz errset
	       $backtrace  retval oldst ($help $help))
	   #+ (or franz maclisp cl)
	   (setq  errset 'errbreak1)
	   ;;	   #+LISPM (setq ERROR-CALL 'ERRBREAK1)
	   (setq tim (runtime) $%% '$%%
		 ;; just in case baktrcl is cons'd on the stack
		 $backtrace (cons '(mlist simp) (copy-list baktrcl)))
	   (setq o^r #.writefilep #.writefilep (and #.writefilep (not dskfnp)))
	   (cond ((eq y 'noprint))
		 (t 
		  (mterpri)
		  (if y (princ 'macsyma-break) (princ 'error-break))
		  (unless (zerop brklvl) (princ " level ") (princ brklvl))
		  (princ " Type EXIT; to quit, HELP; for more help.")))
	   (setq $help
		 "BACKTRACE; will give a successive list of forms 
 (you must have already set ?DEBUG:ALL; for BACKTRACE to record) 
     LISP; goes to lisp 
     TOPLEVEL; goes all the way to top level 
     EXIT; exits one level of the error break")
	   (mterpri)
	   a    (cond
		  ((null
		    (catch 'macsyma-break
		      (let ((state-pdl (cons 'macsyma-break state-pdl)))
			(errset
			 (cond ((eq (setq x
					  (retrieve1 nil
						     (if y "_ " "(debug) "
							 ))) '$exit)
				(timeorg tim)
				(setq retval 'exit) (go end))
			       ((eq x '$lisp)
				;;#+MACLISP			   (LET ((STATE-PDL (CONS 'LISP-BREAK STATE-PDL)))
				;;					(*BREAK T 'LISP) (MTERPRI))  ; ^B also works
				(setq retval 'lisp)
				(go end))
			       ((eq x '$toplevel)
				(cond ((catch 'mbreak
					 (let (st oldst rephrase
						  (mbreak (cons bindlist loclist)))
					   (setq $linenum (f1+ $linenum))
					   (continue)))
				       (go end))
				      (t (mtell-open "Back to the break~%"))))
			       (t (let (($dispflag dispflag)) (setq $%% (meval x)))
				  (if dispflag (displa $%%) (mterpri))))))))
		   (errlfun1 errbrkl)
		   (mtell-open "~%(Still in break loop)~%")))
	   (go a)
	   end  (unless (eq y 'noprint)
		  (princ "Exited from the break ")
		  (if (not (zerop brklvl)) (princ brklvl))
		  (mterpri)
		  )
	   (if o^r (setq #.writefilep t))
	   ;;#+(or Franz MACLISP)   (RETURN NIL)
	   #+cl (return retval)))
      (f1+ brklvl) varlist genvar (cons bindlist loclist) linelable))))

#-nil
(defun errbreak1 (ign)
  (declare (ignore ign))
  nil)					; Used to nullify ERRSETBREAKs

#-nil
(defun errbreak2 (ign) ;; An alternate ERRSET interr. function
  ;; used by PARSE and DISPLAY
  (declare (ignore ign))
  (let ((state-pdl (cons 'lisp-break state-pdl)))
    (*break errbrksw 'erst)))

;; The ^B interrupt function
;;(DEFUN MPAUSE (X)
;;  X ;Ignored       
;;  (LET ((STATE-PDL (LIST* 'LISP-BREAK '^B-BREAK STATE-PDL))
;;	(MOREMSG "--Pause--"))
;;       #+PDP10 (ENDPAGEFN T 'MORE-FUN)
;;       #+PDP10 (BUFFCLEAR NIL)
;;       ; (TIMESOFAR T)
;;       #+MACLISP (NOINTERRUPT NIL)
;;       (*BREAK T ^BMSG))
;;  #+PDP10 (TTYRETFUN T))



(defmspec $tobreak (x)
  (if mbreak (throw 'mbreak (cdr x))
      (merror "TOBREAK may be used only within a MACSYMA break.")))

(defun errlfun (x)
  (when (null
	 (errset
	  (progn #-lispm (setq ^s nil)
					;#+PDP10 (CLOSE SAVEFILE)
		 #-lispm (if loadf (setq defaultf loadf loadf nil))
					;#+PDP10 (ENDPAGEFN T 'MORE-FUN)
		 )))
    #-lispm (setq ^q nil) (mtell-open "~%ERRLFUN has been clobbered."))
  (if $errorfun (if (null (errset (mapply1 $errorfun nil $errorfun nil)))
		    (mtell "~%Incorrect ERRORFUN")))
  (when (null
	 (errset
	  (progn (if (not (eq x 'mquit)) (supunbind)) (clearsign))))
    #-lispm (setq ^q nil) (mtell-open "~%ERRLFUN has been clobbered."))
  (when (null x) (princ quitmsg) (setq quitmsg " ")))

(defun supunbind nil
  (munbind (reverse bindlist)) (do nil ((null loclist)) (munlocal)))

(defmfun errlfun1 (mpdls)
  (do ((l bindlist (cdr l)) (l1)) ((eq l (car mpdls)) (munbind l1))
    (setq l1 (cons (car l) l1)))
  (do nil ((eq loclist (cdr mpdls))) (munlocal)))

(defmfun getalias (x) (cond ((get x 'alias)) ((eq x '$false) nil) (t x)))

(defmfun makealias (x) (implode (cons #\$ (exploden x))))

;; (DEFMSPEC $F (FORM) (SETQ FORM (FEXPRCHECK FORM)) ...)
;; makes sure that F was called with exactly one argument and
;; returns that argument.

(defmfun fexprcheck (form)
  (if (or (null (cdr form)) (cddr form))
      (merror "~:M takes just one argument." (caar form))
      (cadr form)))

(defmfun nonsymchk (x fn)
  (unless (symbolp x)
    (merror "The argument to ~:M must be a symbolic name:~%~M" fn x)))

;;(DEFMFUN NONVARCHK (X FN FLAG 2NDP)
;;  (WHEN (OR (MNUMP X) (INTEGERP X) (AND FLAG (ATOM X) (CONSTANT X))
;;	    (AND (NOT (ATOM X)) (NOT (EQ (CAAR X) 'MQAPPLY)) (MOPP1 (CAAR X))))
;;	(MERROR "Non-variable~Margument to ~:M: ~M"
;;		(IF 2NDP '|& 2nd | '|& |) FN X)))

(defmfun prinl (l) (dolist (x l) (princ x) (tyo #\space)))

(defmfun $print n
  (if (= n 0)
      '((mlist simp))
      (let ((l (listify n)))
	(do ((l l (cddr l)))( (null l)) (rplacd l (cons '| | (cdr l))))
	(displa (setq printmsg (cons '(mtext) l)))
	(cadr (reverse l)))))

(defmspec $playback (x) (setq x (cdr x))
	  (let ((state-pdl (cons 'playback state-pdl)))
	    (prog (l l1 l2 numbp slowp nostringp inputp timep grindp inchar largp)
	       (setq inchar (getlabcharn $inchar))
					; Only the 1st alphabetic char. of $INCHAR is tested
	       (setq timep $showtime grindp $grind)
	       (do ((x x (cdr x)))( (null x))
		 (cond ((eq (ml-typep (car x)) 'fixnum) (setq numbp (car x)))
		       ((eq (car x) '$all))
		       ((eq (car x) '$slow) (setq slowp t))
		       ((eq (car x) '$nostring) (setq nostringp t))
		       ((eq (car x) '$grind) (setq grindp t))
		       ((eq (car x) '$input) (setq inputp t))
		       ((memq (car x) '($showtime $time)) (setq timep (or timep t)))
		       ((memq (car x) '($gctime $totaltime)) (setq timep '$all))
		       ((setq l2 (listargp (car x)))
			(setq l1 (nconc l1 (getlabels (car l2) (cdr l2) nil)) largp t))
		       (t (improper-arg-err (car x) '$playback))))
	       (cond ((and largp (null numbp)) (go loop))
		     ((and (setq l (cdr $labels)) (not $nolabels)) (setq l (cdr l))))
	       (when (or (null numbp) (< (length l) numbp))
		 (setq l1 (reverse l)) (go loop))
	       (do ((i numbp (f1- i)) (l2)) ((zerop i) (setq l1 (nconc l1 l2)))
		 (setq l2 (cons (car l) l2) l (cdr l)))
	       loop (if (null l1) (return '$done))
	       ((lambda (errset incharp)
		  (errset
		   (cond ((and (not nostringp) incharp)
			  (let ((linelable (car l1))) (mterpri) (printlabel))
			  (if grindp (mgrind (meval1 (car l1)) nil)
			      (mapc #'tyo (mstring (meval1 (car l1)))))
			  (if (get (car l1) 'nodisp) (princ '$) (princ '|;|))
			  (mterpri))
			 ((or incharp
			      (prog2 (when (and timep (setq l (get (car l1) 'time)))
				       (setq x (gctimep timep (cdr l)))
				       (mtell-open "~A msec." (car l))
				       #+gc (if x (mtell-open "  GCtime= ~A msec." (cdr l)))
				       (mterpri))
				  (not (or inputp (get (car l1) 'nodisp)))))
			  (mterpri) (displa (list '(mlable) (car l1) (meval1 (car l1)))))
			 (t (go a)))))
		'errbreak2 (char= (getlabcharn (car l1)) inchar))
	       (if (and slowp (cdr l1) (not (continuep))) (return '$terminated))
	       a    (setq l1 (cdr l1))
	       (go loop))))

(defun listargp (x)
  (let (high)
    (if (and ($listp x) (eq (ml-typep (cadr x)) 'fixnum)
	     (or (and (null (cddr x)) (setq high (cadr x)))
		 (and (eq (ml-typep (setq high (caddr x))) 'fixnum)
		      (not (> (cadr x) high)))))
	(cons (cadr x) high))))

(defmspec $alias (form)
  (if (oddp (length (setq form (cdr form))))
      (merror "ALIAS takes an even number of arguments."))
  (do ((l nil (cons (alias (pop form) (pop form))
		    l)))
      ((null form)
       `((mlist simp),@(nreverse l)))))

(defmfun alias (x y)
  (cond ((nonsymchk x '$alias))
	((nonsymchk y '$alias))
	((not (eq (getchar x 1) '$))
	 (merror "-ed symbols may not be aliased. ~M" x))
	((get x 'reversealias)
	 (if (not (eq x y))
	     (merror "~M already is aliased." x)))
	(t (putprop x y'alias)
	   (putprop y (stripdollar x) 'reversealias)
	   (add2lnc y $aliases)
	   y)))

(defmfun remalias (x &optional remp)
  (let ((y (and (or remp (memq x (cdr $aliases))) (get x 'reversealias))))
    (cond ((and y (eq x '%derivative))
	   (remprop x 'reversealias) (delq x $aliases 1)
	   (remprop '$diff 'alias) '$diff)
	  (y (remprop x 'reversealias) (remprop x 'noun) (delq x $aliases 1)
	     (remprop (setq x (makealias y)) 'alias) (remprop x 'verb) x))))

(defmfun stripdollar (x)
  (cond ((not (atom x))
	 (cond ((and (eq (caar x) 'bigfloat) (not (minusp (cadr x)))) (implode (fpformat x)))
	       (t (merror "Atomic arg required:~%~M" x))))
	((numberp x) x)
	((null x) 'false)
	((eq x t) 'true)
	((memq (getchar x 1) '($ % &))
	 #-(or franz nil cl) (implode (cdr (exploden x)))
	 #+cl (intern (subseq (string x) 1))
	 #+nil (intern (substring x 1))
	 #+franz (concat (substring x 2)) ;Nice start/end conventions.
	 )
	(t x)))

(defmfun fullstrip (x) (mapcar #'fullstrip1 x))

(defmfun fullstrip1 (x)
  (or (and (numberp x) x)
      (get x 'reversealias)
      (let ((u (assqr x aliaslist))) (if u (implode (string*1 (car u)))))
      (stripdollar x)))

(defun string* (x)
  (or (and (numberp x) (exploden x))
      (let ((u (assqr x aliaslist))) (if u (string*1 (car u))))
      (string*1 x)))

(defun string*1 (x) (let (stringdisp $lispdisp) (makestring x)))

(defun makstring* (x)
  (setq x (string* x))
  (do ((l x (cdr l)))( (null l)) (rplaca l (ascii (car l))))
  x)

(defmfun $nounify (x)
  (let (y u)
    (nonsymchk x '$nounify)
    (setq x (amperchk x))
    (cond ((get x 'verb))
	  ((get x 'noun) x)
	  ((or (setq u (memq (car (setq y (explodec x))) '($ m)))
	       (not (eq (car y) '%)))
	   (setq y (implode (cons '% (if u (cdr y) y))))
	   (putprop y x 'noun) (putprop x y 'verb))
	  (t x))))

(defmfun $verbify (x)
  (nonsymchk x '$verbify)
  (setq x (amperchk x))
  (cond ((get x 'noun))
	((and (char= (getcharn x 1) #\%)
	      (prog2 ($nounify #+nil (let ((s (copy-seq (symbol-name x))))
				       (setf (schar s 0) #.(code-char #\$))
				       (intern s))
			       #-nil (implode (cons #\$ (cdr (exploden x)))))
		  (get x 'noun))))
	(t x)))

;;(DEFMFUN AMPERCHK (NAME)
;; (IF (char= (GETCHARN NAME 1) #\&)
;;     (OR (GET NAME 'OPR)
;;	 #+NIL (intern (nstring-upcase (string-append "$" name)))
;;	 #-NIL (IMPLODE (CONS #\$ (CASIFY-EXPLODEN NAME))))
;;     NAME))

(defmfun dollarify-name (name)
  (let ((n (getcharn name 1)))
    (cond ((char= n #\&)
	   (or (get name 'opr)
	       (let ((namel (casify-exploden name)) ampname dolname)
		 (cond ((get (setq ampname (implode (cons #\& namel))) 'opr))
		       (t (setq dolname (implode (cons #\$ namel)))
			  (putprop dolname ampname 'op)
			  (putprop ampname dolname 'opr)
			  (add2lnc ampname $props)
			  dolname)))))
	  ((char= n #\%) ($verbify name))
	  (t name))))

#-nil
(defmfun $random n (apply #'random (listify n)))

(defmspec $string (form)
  (setq form (strmeval (fexprcheck form)))
  (setq form (if $grind (strgrind form) (mstring form)))
  (setq st (reverse form) rephrase t)
  (implode (cons #\& form)))

(defmfun makstring (x)
  (setq x (mstring x)) (do ((l x (cdr l)))( (null l)) (rplaca l (ascii (car l)))) x)

(defmfun strmeval (x)
  (cond ((atom x) (meval1 x))
	((memq (caar x) '(msetq mdefine mdefmacro)) x)
	(t (meval x))))

(prog1 '(alias properties)
  (mapc #'(lambda (x) (putprop (car x) (cadr x) 'alias)
		  (putprop (cadr x) (caddr x) 'reversealias))
	'(($block mprog block) ($lambda lambda lambda)
	  ($abs mabs abs) ($subst $substitute subst)
	  ($go mgo go) ($signum %signum signum)
	  ($return mreturn return) ($factorial mfactorial factorial)
	  ($nouuo nouuo nouuo) ($rset *rset rset)
	  ($ibase *read-base* *read-base*) ($obase *print-base* obase) ($nopoint *nopoint nopoint)
	  ($modulus modulus modulus) ($zunderflow zunderflow zunderflow)
	  ($ttyoff #.ttyoff ttyoff) ($writefile_on #.writefilep writefile_on)
	  ($mode_declare $modedeclare mode_declare)))
  (mapc #'(lambda (x) (putprop (car x) (cadr x) 'alias))
	'(($ratcoeff $ratcoef) ($ratnum $ratnumer) ($true t)
	  ($binom %binomial) ($derivative $diff) ($prod $product)
	  ($bothcoeff $bothcoef))))

(defmfun amperchk (name)
  " $AB ==> $AB,
   $aB ==> $aB,
   &aB ==> $AB,
   |aB| ==> |aB| "
  (if (char= (getcharn name 1) #\&)
      (or (get name 'opr)
	  ;;note the nil version does something else
	  #+ nil   
	  (nstring-upcase (string-append "$" name))
	  #-nil(implode (cons #\$ (casify-exploden name))))
      name))


#+cl
(defun casify-exploden (x)
  (cond ((char= (getcharn x 1) #\&)
	 (cdr (exploden (string-upcase (string x)))))
	(t (exploden x))))
#-cl
(defmfun casify-exploden (x)
  (setq x (exploden x))
  (if (char= (car x) #\&) (mapcar #'casify (cdr x)) (cdr x)))

(defmspec $stringout (x)  (setq x (cdr x))
	  (let (file maxima-error l1 truename)
	    (setq file ($filename_merge (car x)))
	    (setq x (cdr x))
	    (with-open-file (savefile file :direction :output)
	      (cond ((null
		      (errset
		       (do ((l x (cdr l)))( (null l))
			 (cond ((memq (car l) '($all $input))
				(setq l (nconc (getlabels* $inchar t) (cdr l))))
			       ((eq (car l) '$values)
				(setq l (nconc (mapcan
						#'(lambda (x)
						    (if (boundp x)
							(ncons (list '(msetq) x (symbol-value x)))))
						(cdr $values))
					       (cdr l))))
			       ((eq (car l) '$functions)
				(setq l (nconc (mapcar
						#'(lambda (x) (consfundef (caar x) nil nil))
						(cdr $functions))
					       (mapcan
						#'(lambda (x)
						    (if (mget x 'aexpr)
							(ncons (consfundef x t nil))))
						(cdr $arrays))
					       (mapcar
						#'(lambda (x) (consfundef (caar x) nil nil))
						(cdr $macros))
					       (cdr l))))
			       ((setq l1 (listargp (car l)))
				(setq l (nconc (getlabels (car l1) (cdr l1) t) (cdr l)))))
			 (if (null l) (return nil))
			 (terpri savefile)
			 (if $grind (mgrind (strmeval (car l)) savefile)
			     #-franz (princ (maknam (mstring (strmeval (car l))))
					    savefile)
			     #+franz (mapc #'(lambda (ch) (tyo ch savefile))
					   (mstring (strmeval (car l)))))
			 (if (or (and (atom (car l)) (get (car l) 'nodisp)) (not $strdisp))
			     (tyo #\$ savefile)
			     (tyo semi-colon-char savefile)))))
		     (setq maxima-error t)))
	      (setq truename (truename savefile))
	      (terpri savefile))
	    (if maxima-error (let ((errset 'errbreak1)) (merror "Error in STRINGOUT attempt")))
	    (cl:namestring truename)))
(defmspec $labels (char)
  (setq char (fexprcheck char))
  (nonsymchk char '$labels)
  (cons '(mlist simp) (nreverse (getlabels* char nil))))

(defmfun $%th (x)
  (prog (l outchar)
     (if (or (not (eq (ml-typep x) 'fixnum)) (= x 0))
	 (improper-arg-err x '$%th))
     (if (> x 0) (setq x (f- x)))
     (if (cdr $labels)
	 (setq l (cddr $labels) outchar (getlabcharn $outchar)))
     loop (if (null l) (merror "Improper call to %TH"))
     (if (and (char= (getlabcharn (car l)) outchar) (= (setq x (f1+ x)) 0))
					; Only the 1st alphabetic character of $OUTCHAR is tested.
	 (return (meval (car l))))
     (setq l (cdr l))
     (go loop)))

(defmfun getlabels (n1 n2 flag)	; FLAG = T for STRINGOUT, = NIL for PLAYBACK and SAVE.
  (do ((i n1 (f1+ i)) (l1)
       (l (if flag (list (exploden $inchar))
	      (list (exploden $inchar) (exploden $linechar)
		    (exploden $outchar)))))
      ((> i n2) (nreverse l1))
    (do ((l l (cdr l)) (x (mexploden i)) (z)) ((null l))
      (if (boundp (setq z (implode (append (car l) x))))
	  (setq l1 (cons z l1))))))

(defmfun getlabels* (char flag)		; FLAG = T only for STRINGOUT
  (do ((l (if flag (cddr $labels) (cdr $labels)) (cdr l))
       (char (getlabcharn char)) (l1))
      ((null l) l1)
    (if (char= (getlabcharn (car l)) char)
					; Only the 1st alphabetic character is tested.
	(setq l1 (cons (car l) l1)))))

(defmfun getlabcharn (label)
  (let ((char (getcharn label 2))) (if (char= char #\%) (getcharn label 3) char)))
(defmspec $errcatch (form)
  (let ((errcatch (cons bindlist loclist)) ret)
    (if (null (setq ret (let (*mdebug*)
			  (errset (mevaln (cdr form)) lisperrprint))))
	(errlfun1 errcatch))
    (cons '(mlist) ret)))

;;(DEFMFUN $ERROR N  ; Moved to MAXSRC;MERROR
;; (LET ((MSG (LISTIFY N)))
;;      (IF (> N 0) (APPLY #'$PRINT MSG))
;;      (IF ERRCATCH (ERROR))
;;      (IF DEBUG (LET (($ERROR (CONS '(MLIST SIMP) (FSTRINGC MSG))))
;;		      (ERRBREAK NIL)))
;;      (MQUIT T)))


(defmspec $catch (form)
  (let ((mcatch (cons bindlist loclist)))
    (prog1 (catch 'mcatch (mevaln (cdr form))) (errlfun1 mcatch))))

(defmfun $throw (exp)
  (if (null mcatch) (merror "THROW not within CATCH:~%~M" exp))
  (throw 'mcatch exp))

(defmspec $time (l) (setq l (cdr l))
	  #-cl
	  (mtell-open "TIME or [TOTALTIME, GCTIME] in msecs.:~%")
	  #+cl
	  (format t "~&Time:")
	  (cons '(mlist simp)
		(mapcar
		 #'(lambda (x)
		     (or (and (setq x (or (get x 'time)
					  (and (eq x '$%) (cons (cadr $lasttime)
								(caddr $lasttime)))))
			      (if (= (cdr x) 0)
				  (car x)
				  (list '(mlist simp) (car x) (cdr x))))
			 '$unknown))
		 l)))

(defmfun timeorg (tim)
  (if (> thistime 0) (setq thistime (f+ thistime (f- (runtime) tim)))))

;; Take difference of two times, return result in milliseconds.
;;#+LISPM
;;(DEFMFUN COMPUTIME (N1 N2) (// (f* 50. (TIME-DIFFERENCE
;;					N1 N2)) 3.))


#+cl (progn 'compile
	    (defmfun $quit () 
	      nil 
	      (princ *maxima-epilog*)
  #+kcl (lisp::bye) 
	      #+cmu (ext:quit) 
	      #+sbcl (sb-ext:quit) 
	      #+clisp (ext:quit) 
	      #+mcl (ccl::quit)
	      #+gcl (quit)
	      #+excl "don't know quit function")

	    (defmfun $logout () (bye))
	    )
(defmfun fileprint (fname)	  ; Takes filename in NAMELIST format.
  (cond ($file_string_print (princ (namestring fname)) (princ "  "))
	(t (princ "[")
	   (princ (cadr fname)) (princ ", ")
	   (princ (caddr fname)) (princ ", ")
	   (when (cdddr fname) (princ (cadddr fname)) (princ ", ")) ; For TOPS-20
	   (princ (caar fname)) (princ ", ")
	   (princ (cadar fname)) (princ "]  "))))

(defmfun mfile-out (fname) ; Takes filename in NAMELIST or OldIO list format.
  (if $file_string_print
      (implode (cons #\& (exploden (namestring fname))))
      (dollarify (if (atom (car fname)) fname (append (cdr fname) (car fname))))))

;; File-processing stuff.  Lisp Machine version in MC:LMMAX;LMSUP.



(defun mfile nil
  (fullstrip (list $filename (setq $filenum (f1+ $filenum)) $device $direc)))


;; This prevents single blank lines from appearing at the top of video 
;; terminals.  If at the upper left corner and we want to print a blank 
;; line, leave the cursor there and send the blank line to transcript 
;; files only.

#+(or pdp10 nil cl)
(defmfun mterpri (&aux x)
  #-nocp (setq x  (cursorpos))
  (if (and smart-tty x (equal x '(0 . 0)))
      (let ((#.ttyoff t)) (terpri))
      (terpri)))

;;#+lispm
;;(DECLARE-top (SPECIAL TV:MORE-PROCESSING-GLOBAL-ENABLE))

;;#+LISPM
;;(DEFMFUN MORE-FUN (FILE)
;;  FILE ;ignored
;; (send  *terminal-io* :send-if-handles :more-exception))


;;#+LISPM
;;(DEFUN MORE-FUN-INTERNAL (*terminal-io*
;;			  &AUX (*standard-input* *terminal-io*))
;;;				 'SI:TERMINAL-IO-SYN-STREAM))
;;                          ;; SI:SYN-TERMINAL-IO))
;; ; This clears the rest of the screen, unless we're at the bottom
;; ; or too close to the top.
;; (COND ((NOT (OR (< (CAR (CURSORPOS)) 10.)
;;		 (= (- TTYHEIGHT 2) (CAR (CURSORPOS)))))
;;	(CURSORPOS 'E)))
;; ; Now go to the bottom of the screen and cause a more, unless disabled.
;; (COND (TV:MORE-PROCESSING-GLOBAL-ENABLE
;;	(CURSORPOS 'Z) (CURSORPOS 'L)
;;	((LAMBDA (^Q)
;;	  ((LAMBDA (#.WRITEFILEP #.TTYOFF STATE-PDL)
;;	    (PRINC MOREMSG) (TYIPEEK)
;;	    ; Now see what the user feels like typing in.
;;	    (COND ($MOREWAIT
;;		   (DO ((L (COND ((EQ $MOREWAIT '$ALL) '(#\SPACE #\RETURN))
;;				 (T '(#\SPACE #\RETURN #\RUBOUT)))))
;;		       ((zl-MEMBER (TYIPEEK) L))
;;		      (TYI T))) ; eat other characters
;;		  (T (DO () ((NOT (zl-MEMBER (TYIPEEK) '(4 19. 21. 22. 29.))))
;;			 (TYI T)))) ; eat ^], etc. 
;;	    ; Now erase the MORE message
;;	    (COND (SMART-TTY (CURSORPOS 'Z) (CURSORPOS 'L)) (T (TERPRI))))
;;	   NIL NIL (CONS 'MORE-WAIT STATE-PDL))
;;	  ; Now decide whether to continue or flush
;;	  (COND ((char= #\Space (TYIPEEK))
;;		 (IF MORECONTINUE (LET (#.WRITEFILEP #.TTYOFF) (PRINC MORECONTINUE)))
;;		 (TYI T)) ; eat the space
;;		((char= #\RUBOUT (TYIPEEK))
;;		 (LET ((#.TTYOFF T)) (TERPRI))
;;		 (IF MOREFLUSH (PRINC MOREFLUSH))
;;		 (TYI T)  ; eat the rubout
;;		 (SETQ MORE-^W (OR MORE-^W (AND MOREFLUSH T))
;;		       #.WRITEFILEP (AND #.WRITEFILEP (NULL MOREFLUSH))))
;;		(T (COND ((OR (MEMQ 'BATCH STATE-PDL)
;;			      (AND (char< (TYIPEEK) #\SPACE)
;;				   (NOT (zl-MEMBER (TYIPEEK)
;;					  #.(cons 'list	(mapcar    'code-char '(2 7 11. 12. 25. 27. 28. 29. 30.))))
;;						   '(#\Alpha #\Pi
;;						    #\Up-Arrow
;;						    #\Plus-Minus
;;						    #\Right-Arrow
;;						    #\Lozenge
;;						    #\Less-Or-Equal
;;						    #\Greater-Or-Equal
;;						    #\Equivalence)
;;						   ))
;;			      (char>= (TYIPEEK) #. (code-char 128.)))
;;			  (TYI T)))  ; eat cr or other control character.
;;		   (IF MOREFLUSH (LET (#.WRITEFILEP #.TTYOFF) (PRINC MOREFLUSH)))
;;		   (SETQ MORE-^W (OR MORE-^W (AND MOREFLUSH T))))))
;;	 NIL)))
;; ; Now home up, or advance to next line, and continue display.
;; (IF SMART-TTY
;;     (COND (RUBOUT-TTY (LET (#.TTYOFF) (CURSORPOS T T) (CURSORPOS 'L)))
;;	   (T (MAXIMA-SLEEP 0.4) (FORMFEED)))
;;     (LET (#.TTYOFF #.WRITEFILEP) (TERPRI))))

(defmfun $pagepause (x) (pagepause1 nil x))


#-pdp10
(defun pagepause1 (x y)
  x y (merror "PAGEPAUSE does not exist in this system."))


#+(or cl lispm)
(defmspec $status (form)
  (setq form (cdr form))
  (let* ((keyword (car form))
	 (feature (cadr form)))
    (assert (symbolp keyword))
    (assert (symbolp feature))
    (case keyword
      ($feature (cond ((null feature) (dollarify #-cl (status features)
						 #+cl *features*))
		      ((memq (intern (symbol-name
				      (fullstrip1 feature)) 'keyword)
			     #-cl(status features)
			     #+cl *features*
			     ) t)))
      ($status '((mlist simp) $feature $status))
      (t (merror "Unknown argument - STATUS:~%~M" keyword)))))

#+(or cl lispm)
(defquote $sstatus (status-function item)
  (cond ((equal status-function '$feature)
	 (pushnew ($mkey item) *features*) t)
	((equal status-function '$nofeature)
	 (setq *features* (delete ($mkey item) *features*)) t)
	(t (error "know only how to set and remove feature status"))))

;; End of disk GC conditionalization.

#-pdp10 (progn 'compile
	       (defmfun $dskgc (x) x nil)
	       (defun dskgc1 (x y) x y nil)
	       )



#+cl (setq error-call 'errbreak)

(progn (do ((l '($sqrt $erf $sin $cos $tan $log $plog $sec $csc $cot $sinh $cosh
		 $tanh $sech $csch $coth $asin $acos $atan $acot $acsc $asec $asinh
		 $acosh $atanh $acsch $asech $acoth $binomial $gamma $genfact $del)
	       (cdr l)))( (null l))
	 ((lambda (x)
	    (putprop (car l) x 'alias)
	    (putprop x (stripdollar (car l)) 'reversealias))
	  ($nounify (car l))))
       ($nounify '$sum) ($nounify '$product)
       ($nounify '$integrate) ($nounify '$limit)
       (defprop $diff %derivative verb) (defprop %derivative $diff noun)
       '(noun properties))

(progn (mapc #'(lambda (x) (putprop (car x) (cadr x) 'assign))
	     '(($debugmode debugmode1) ($bothcases bothcases1)
	       ($pagepause pagepause1) ($dskgc dskgc1)
	       ($ttyintfun ttyintfunsetup)
	       ($fpprec fpprec1) ($poislim poislim1)
	       ($default_let_rule_package let-rule-setter)
	       ($current_let_rule_package let-rule-setter)
	       ($let_rule_packages let-rule-setter)))
       (mapc #'(lambda (x) (putprop x 'neverset 'assign)) (cdr $infolists))
       (defprop $contexts neverset assign)
       '(assign properties))



;; Undeclarations for the file:
(declare-top (notype i n n1 n2 u1))

#-(or cl nil)
(eval-when (eval compile) (setq *print-base* old-base *read-base* old-ibase))
#+cl
(eval-when (:execute :compile-toplevel)
  (setq *print-base* old-base *read-base* old-ibase))
