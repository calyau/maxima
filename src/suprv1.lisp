;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;	** (c) Copyright 1982 Massachusetts Institute of Technology **

;(macsyma-module suprv)

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


#-(or cl NIL)
(EVAL-WHEN (EVAL COMPILE)
  	   (SETQ OLD-IBASE *read-base* OLD-BASE *print-base*) 
	   (SETQ *read-base* 10. *print-base* 10.))
#+cl
(EVAL-WHEN (EVAL COMPILE)
  	   (SETQ OLD-IBASE *READ-BASE* OLD-BASE *PRINT-BASE*) 
	   (SETQ *READ-BASE* 10. *PRINT-BASE* 10.))

(defmvar mopl nil)

(declare-top 
	 (SPECIAL M$+ GCFLAG GCT $LASTTIME $PARSETIME $DISPTIME
		  BINDLIST LOCLIST ERRSET $LABELS LINELABLE $BATCOUNT $FILESIZE
		  ST REPHRASE $DISPFLAG REFCHKL BAKTRCL RUBOUT TTYHEIGHT
		  CNTLY NEWLINE DSKFNP DSKSAVEP *RSET CNTL@
		  ^W ^R ^Q ^D LF TAB FF CNTLC ALT BATCONL CR VT ^H ^S BSP
		  $VALUES $FUNCTIONS $ARRAYS $ALIASES $GRADEFS $DEPENDENCIES
		  $RULES $PROPS $RATVARS $RATVARSWITCH DEBUG ERRBRKSW ERRCATCH
		  VARLIST GENVAR $DEVICE $FILENAME $FILENUM LBP RBP
		  $GENSUMNUM CHECKFACTORS $FEATURES FEATUREL $BACKTRACE
		  $WEIGHTLEVELS TELLRATLIST $DONTFACTOR $INFOLISTS LOADFILES
		  $DSKALL ERRLIST ALLBUTL LISPERRPRINT BACKRUB
		  GC-DAEMON GC-OVERFLOW DEMONL $DYNAMALLOC ALLOCLEVEL INFILE
		  ALARMCLOCK $C18MAXTIME $FILEID DCOUNT GCLINENUM THISTIME
		  $NOLABELS $BATCHKILL DISPFLAG SAVENO MCATCH BRKLVL SAVEFILE
		  STRING ST1 STIME0 $%% $ERROR
		  *IN-$BATCHLOAD* *IN-TRANSLATE-FILE*
		  LESSORDER GREATORDER $ERRORFUN MBREAK REPRINT POS $STRDISP
		  $DSKUSE SMART-TTY RUBOUT-TTY MORE-^W OLDST ALPHABET
		  $LOADPRINT TTYINTS OPERS
		  *RATWEIGHTS $RATWEIGHTS QUITMSG MQUITMSG CONTMSG
		  LOADF DISPLAY-FILE $GRIND SCROLLP $CURSORDISP
		  STRINGDISP $LISPDISP MEXPRP DEFAULTF READING
		  BPORG GCSYML ^AMSG ^BMSG ^HMSG
		  STATE-PDL PROMPTMSG GCPROMPT COMMAND PRINTMSG MRG-PUNT
		  NEW-C-LINE-HOOK TRANSP $CONTEXTS $SETCHECK $MACROS
		  UNDF-FNCTN AUTOLOAD)
#+CL  (SPECIAL ERROR-CALL)
#+Franz  (special ptport display-to-disk)
	 (*EXPR REPRINT)
	 (*LEXPR CONCAT $FILEDEFAULTS $PRINT)
	 (FIXNUM $FILESIZE DCOUNT $BATCOUNT I N N1 N2 TTYHEIGHT
		 $FILENUM THISTIME GCT TIM GCLINENUM ALLOCLEVEL
		 BRKLVL CMTCNT BPORG BPORG0 #-cl (COMPUTIME FIXNUM FIXNUM)
		 #-cl (CASIFY FIXNUM) #-cl (GETLABCHARN))
	 (FLONUM U1 STIME0)
	 (NOTYPE (ASCII-NUMBERP FIXNUM))
	 (ARRAY* (FIXNUM DISPLAY-FILE 1)))


;; This affects the runtime environment.  ALJABR;LOADER also does this, but
;; leave it here for other systems.  On the Lisp Machine, this is bound
;; per stack group.

(defmvar $prompt
  '_
  nil
  no-reset)

(eval-when (compile eval load)

(defun control-char (ch)
  (code-char (+ (char-code #\) (- (char-code ch) (char-code #\A)))))
)

(PROGN (MAPC #'(LAMBDA (X) (PUTPROP (CAR X) (CADR X) 'OPALIAS))
	     '((+ $+) (- $-) (* $*) (// $//) (^ $^) (|.| |$.|) (< $<) (= $=)
	       (> $>) (|(| |$(|) (|)| |$)|) (|[| |$[|) (|]| |$]|) (|,| |$,|) (|:| |$:|)
	       (|!| |$!|) (|#| |$#|) (|'| |$'|) (|;| |$;|)))
       #+cl
       (MAPC #'(LAMBDA (X) (SET (CAR X)
				(cond (#-cl (< (cadr x)
						  160)
					    #+cl (char< (cadr x)
							#. (code-char 160.))
					       (ASCII (CADR X)))
					      (t (cadr x)))))
	     '( ;#-cl (CNTL@ #\)
	       (CNTLC #. (control-char #\C))
	       (BSP #\Backspace) (TAB #\TAB) (LF #\Linefeed)
;	      #-lispm (VT #\VT) ;;would not compile in lispmachine
	       (FF #\Page) (CR #\return)
	       (CNTLY #.(control-char #\Y))
	       (SP #\Space)
	       (NEWLINE #\NEWLINE) (RUBOUT #\RUBOUT)))
       (SETQ GCSYML NIL)
       (DOTIMES (I 14.) (PUSH (GENSYM) GCSYML))
   #-cl  (SETQ ALT #-MULTICS (intern (string #\Escape)) #+MULTICS '&)
#-CL (SETQ $PLOTUNDEFINED (*$ 2.0 -8.5070591E+37))
       (SETQ $LASTTIME '((MLIST) 0 0) THISTIME 0 GCT 0 GCFLAG NIL
	     $PARSETIME NIL $DISPTIME NIL MEXPRP NIL)
       (SETQ BATCONL NIL $BATCOUNT 0 $BATCHKILL NIL $STRDISP T $GRIND NIL)
       (SETQ REFCHKL NIL DEBUG NIL BAKTRCL NIL ERRBRKSW NIL MBREAK NIL $ERRORFUN NIL
	     ERRCATCH NIL DEMONL (LIST NIL) MCATCH NIL BRKLVL -1
	     ALLBUTL NIL LOADF NIL $BACKTRACE '$BACKTRACE)
       (SETQ *IN-$BATCHLOAD* NIL *IN-TRANSLATE-FILE* NIL)
       (SETQ BACKRUB #-Franz nil #+Franz t)
       (SETQ $DEBUGMODE NIL $BOTHCASES T
	     $PAGEPAUSE NIL $DSKGC NIL $POISLIM 5)
       (SETQ $LOADPRINT NIL ^S NIL LOADFILES NIL)
;      (SETQ $FILEID NIL $C18MAXTIME 150.0E6)
       (SETQ $NOLABELS NIL $ALIASES '((MLIST SIMP)) LESSORDER NIL GREATORDER NIL)
       (SETQ $INFOLISTS
	     (PURCOPY '((MLIST SIMP) $LABELS $VALUES $FUNCTIONS $MACROS $ARRAYS
				     $MYOPTIONS $PROPS $ALIASES $RULES $GRADEFS
				     $DEPENDENCIES $LET_RULE_PACKAGES)))
       (SETQ $LABELS (list '(MLIST SIMP)))
       (setq $DSKUSE NIL $DEVICE '$DSK $DISPFLAG T LINELABLE NIL)
       (SETQ REPHRASE NIL ST NIL OLDST NIL REPRINT NIL POS NIL)
       (SETQ DCOUNT 0 $FILENUM 0 $STORENUM 1000. $FILESIZE 16. $DSKALL T
	     NEW-C-LINE-HOOK NIL DSKFNP NIL TTYINTS T
	     GCLINENUM 0 DSKSAVEP NIL SAVENO 0 $DYNAMALLOC NIL ALLOCLEVEL 0)
       (SETQ QUITMSG  " "
	     MQUITMSG " (Into LISP.  Type control-G to get to MACSYMA.)" 
	     CONTMSG  "(Type <space> to continue, <return> to terminate.)"
	     ^AMSG    "  (Type EXIT; to exit.)"
	     ^BMSG   #-Multics "LISP  (Type <Alt>P<Space> to continue.)" 
		     #+Multics "LISP  (Type <Dollarsign>P<Carriage Return> to continue)"
	     ^HMSG "
 (Use the RUBOUT or DEL(ETE) key to erase a character.)" ^DMSG-ON "
 (Printout of GC statistics turned on.  Type control-D again to turn them off.)
"	     ^DMSG-OFF "
 (Printout of GC statistics turned off.)
"	     GCPROMPT "Type ALL; NONE; a level-no. or the name of the space.
"	     MORE-^W NIL 
	     LISPERRPRINT T PRINTMSG NIL PROMPTMSG NIL MRG-PUNT NIL READING NIL)
;      (SETQ $CALCOMPNUM 100.)
       (SETQ STATE-PDL (PURCOPY (NCONS 'LISP-TOPLEVEL)))
       #+MULTICS (SETQ $PLOT3DSIZE 20 $MULTGRAPH T)
   ; Slashify ':' on printout on other systems for the benefit of Lispm.
;;; Figure out how to do the above for Franz.
       '(Random properties))

(DEFMVAR $% '$% "The last D-line computed, corresponds to lisp *" NO-RESET)
(DEFMVAR $INCHAR '$C
  "The alphabetic prefix of the names of expressions typed by the user.")
(DEFMVAR $OUTCHAR '$D
  "The alphabetic prefix of the names of expressions returned by the system.")
(DEFMVAR $LINECHAR '$E
  "The alphabetic prefix of the names of intermediate displayed expressions.")
(DEFMVAR $LINENUM 1 "the line number of the last expression." FIXNUM NO-RESET)
(DEFMVAR $DIREC 'JRMU
  "The default file directory for SAVE, STORE, FASSAVE, and STRINGOUT."
  NO-RESET)
(DEFMVAR CASEP T
  "Causes translation of characters from lower to upper case on ITS, 
   and from upper to lower case on Multics and Franz.")
;(DEFMVAR $ERREXP '$ERREXP)
(DEFMVAR USER-TIMESOFAR NIL)



(DEFVAR MOREMSG "--Pause--")
(DEFVAR MORECONTINUE "--Continued--")
(DEFVAR MOREFLUSH NIL)
(DEFMVAR $MOREWAIT NIL "needs to be documented" NO-RESET)

(DEFMVAR $SHOWTIME NIL)

(DEFMVAR ALIASLIST NIL
 "is used by the MAKEATOMIC scheme which has never been completed"
 NO-RESET)

;(declare-top (SETQ *print-base* 8))


(DEFUN SYS-GCTIME ()
  #-Franz (STATUS GCTIME)
  #+Franz (cadr (ptime)))


;#.(SETQ NALT #-MULTICS #\ALT #+MULTICS #\&)

#-cl
(DEFMVAR $CHANGE_FILEDEFAULTS #+PDP10 T #-PDP10 NIL
	 "Does DDT-style file defaulting iff T")

(DEFMVAR $FILE_STRING_PRINT #+PDP10 NIL #-PDP10 T
	 "If TRUE, filenames are output as strings; if FALSE, as lists.")

(DEFMVAR $SHOWTIME #-MULTICS NIL #+MULTICS T)


(DEFMFUN MEVAL* (TEST)
 (LET (REFCHKL BAKTRCL CHECKFACTORS)
      (PROG2 (IF $RATVARSWITCH (SETQ VARLIST (CDR $RATVARS)))
	     (MEVAL TEST)
	     (CLEARSIGN))))

(DEFMFUN MAKELABEL (X)
 (WHEN (AND $DSKUSE (NOT $NOLABELS) (> (SETQ DCOUNT (f1+ DCOUNT)) $FILESIZE))
       (SETQ DCOUNT 0) (DSKSAVE))
 (SETQ LINELABLE (CONCAT X $LINENUM))
 (IF (NOT $NOLABELS)
     (IF (OR (NULL (CDR $LABELS))
	     (WHEN (MEMQ LINELABLE (CDDR $LABELS))
		   (DELQ LINELABLE $LABELS 1) T)
	     (NOT (EQ LINELABLE (CADR $LABELS))))
	 (SETQ $LABELS (CONS (CAR $LABELS) (CONS LINELABLE (CDR $LABELS))))))
 LINELABLE)

(DEFMFUN PRINTLABEL NIL
  (MTELL-OPEN "(~A) " (MAKNAM (CDR (EXPLODEN LINELABLE)))))

(DEFMFUN MEXPLODEN (X)
  (let ( #-cl(*nopoint t) #+cl *print-radix*
	#+cl (*print-base* 10)
	#+NIL (si:standard-output-radix 10) #-(or cl NIL) (*print-base* 10))
    (EXPLODEN X)))

(DEFMFUN ADDLABEL (LABEL)
 (SETQ $LABELS (CONS (CAR $LABELS) (CONS LABEL (DELQ LABEL (CDR $LABELS) 1)))))

(DEFMFUN TYI* NIL
 #+Multics (CLEAR-INPUT NIL)
 (DO ((N (TYI) (TYI))) (NIL)
     (COND ((OR (char= N #\NewLine) (AND (> N 31) (NOT (char= N #\RUBOUT))))
	    (RETURN N))
	   ((char= N #\Page) (FORMFEED) (PRINC (STRIPDOLLAR $PROMPT))))))

(DEFUN CONTINUEP NIL
 (PRINC (STRIPDOLLAR $PROMPT))
 (char= (TYI*) #-Multics #\Space #+Multics #\NewLine))

(DEFUN CHECKLABEL (X)  ; CHECKLABEL returns T iff label is not in use
 (NOT (OR $NOLABELS (= $LINENUM 0) (BOUNDP (CONCAT X $LINENUM)))))

(DEFUN GCTIMEP (TIMEP TIM)
 (COND ((AND (EQ TIMEP '$ALL) (NOT (ZEROP TIM))) (PRINC "Totaltime= ") T)
       (T (PRINC "Time= ") NIL)))

;; If $BOTHCASES is T, lower case letters will not be converted to upper case.

(DEFMFUN $BOTHCASES (X) (BOTHCASES1 NIL X))

(DEFUN BOTHCASES1 (SYMBOL VALUE)
 SYMBOL ;Always bound to $BOTHCASES.  Ignored.
 ;; This won't work with the Lisp Machine reader.
 #+MacLisp (DO ((I 97. (f1+ I))) ((> I 122.))
	       (SETSYNTAX I (IF VALUE 1 321.) (IF VALUE I (f- I 32.))))
 (SETQ CASEP (NOT VALUE)) VALUE)

;(DEFUN BACKSPACE1 (NIL X)
; (COND (X (ADD2LNC 8 ALPHABET)
;	  (SETSYNTAX 8 322. NIL))
;       (T (DELETE 8 ALPHABET 1)
;	  (SETSYNTAX 8 131392. NIL)))
; (SETQ BSPP X))

#+CL
(DEFUN LISTEN () 0)  ; Doesn't exist yet.

(DEFUN DISPLAY* (&AUX (RET NIL) (TIM 0))
 #+GC (IF (EQ GCFLAG '$ALL) (LET (^D) (GC)))
 (SETQ TIM (RUNTIME)
       RET (LET ((ERRSET 'ERRBREAK2) (THISTIME -1))
		(ERRSET (DISPLA (LIST '(MLABLE) LINELABLE $%)))))
 (IF (NULL RET) (MTELL "~%Error during display~%"))
 (IF $DISPTIME (MTELL-OPEN "Displaytime= ~A msec.~%" (COMPUTIME (RUNTIME) TIM)))
 RET)


(DEFMFUN RUBOUT* (STG)
 (LET (#.TTYOFF #.WRITEFILEP)
      (COND (RUBOUT-TTY
	     (COND ((OR REPRINT (NULL STG)
			(char= (CAR STG) #\return) (char= (CAR STG) #\tAB))
		    (COND (SMART-TTY
			   (CURSORPOS (CAR POS) (CDR POS)) (CURSORPOS 'L)
			   (IF (CDR STG) (PRINC (MAKNAM (REVERSE (CDR STG)))))
			   (SETQ REPRINT NIL))
			  ((OR REPRINT STG) (REPRINT (CDR STG) NIL))))
		   (T (CURSORPOS 'X))))
	    (STG (TYO (CAR STG))))))


(DEFMFUN REPRINT (STG FFP)
 (LET (#.TTYOFF #.WRITEFILEP)
      (IF (NOT FFP) (MTERPRI))
      (CASE (CAR STATE-PDL)
	     (MACSYMA-TOPLEVEL (PRINTLABEL))
	     (RETRIEVE (IF (EQ MRG-PUNT 'BREAK) (PRINC (STRIPDOLLAR $PROMPT)))))
      (SETQ POS (CURSORPOS))
      (IF STG (PRINC (MAKNAM (REVERSE STG))))
      (SETQ REPRINT NIL)))

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
  (cond ((MEMBER type
	   '(nil "BIN" "O" "o" "XFASL" "QFASL" "LISP" "LSP") :test 'equalp)
	 (load file))
	   (t ($batchload file))))
#+cl
(defvar autoload 'generic-autoload)


#+(or Franz MACLISP NIL cl)
(DEFMFUN LOAD-FUNCTION (FUNC MEXPRP)  ; The dynamic loader
 (LET ((FILE (GET FUNC 'AUTOLOAD)))
      (IF FILE (FUNCALL AUTOLOAD (CONS FUNC FILE)))))

(DEFMFUN LOAD-FILE (FILE) ($LOAD (TO-MACSYMA-NAMESTRING FILE)))

(DEFMSPEC $LOADFILE (FORM)
 (LOADFILE (FILESTRIP (CDR FORM)) NIL
	   (NOT (MEMQ $LOADPRINT '(NIL $AUTOLOAD)))))



#-(or Franz cl cl)
(DEFMSPEC $SETUP_AUTOLOAD (L)
  (SETQ L (CDR L))
  (show l)
  (IF (NULL (CDR L)) (WNA-ERR '$SETUP_AUTOLOAD))
  (LET ((FILE #-PDP10 ($FILE_SEARCH ($FILENAME_MERGE 
				     (CAR L)
				     (USER-WORKINGDIR-PATHNAME)))
	      #+PDP10 (NAMELIST (MERGEF ($FILENAME_MERGE (CAR L))
					`((DSK ,(STATUS UDIR)) NOFILE)))))					
    (DOLIST (FUNC (CDR L))
	    (NONSYMCHK FUNC '$SETUP_AUTOLOAD)
	    (PUTPROP (SETQ FUNC (DOLLARIFY-NAME FUNC)) FILE 'AUTOLOAD)
	    (ADD2LNC FUNC $PROPS)))
  '$DONE)
#+cl
(DEFun $SETUP_AUTOLOAD (filename &rest functions)
  (LET ((FILE  (pathname (string-trim "&$" filename))))
    (DOLIST (FUNC functions)
	    (NONSYMCHK FUNC '$SETUP_AUTOLOAD)
	    (PUTPROP (SETQ FUNC (DOLLARIFY-NAME FUNC)) FILE 'AUTOLOAD)
	    (ADD2LNC FUNC $PROPS)))
  '$DONE)

(DEFMFUN DOLLARIFY (L)
  (LET ((ERRSET 'ERRBREAK1))
      (CONS '(MLIST SIMP)
	    (MAPCAR #'(LAMBDA (X)
		       (LET (Y)
			    (COND ((NUMBERP X) X)
				  ((NUMBERP (SETQ Y (CAR (ERRSET
							  (READLIST
							   (MEXPLODEN X))
							  NIL))))
				   Y)
				  (T (MAKEALIAS X)))))
		    L))))

(DEFMFUN MFBOUNDP (FUNC)
 (OR (MGETL FUNC '(MEXPR MMACRO))
     (GETL FUNC '(TRANSLATED-MMACRO MFEXPR* MFEXPR*S))))

(DEFMFUN FILENAMEL (FILE)
 (COND ((ATOM FILE) (SETQ FILE (NCONS FILE)))
       (($LISTP FILE) (SETQ FILE (CDR FILE)))
       (T (MERROR "Not a proper filename ~M" FILE)))
 (FILESTRIP FILE))



#+CL  ; This is quite different from the Maclisp version.
(DEFMFUN LOADFILE (FILE FINDP PRINTP &AUX (SAVENO 0))
  (AND FINDP (MEMQ $LOADPRINT '(NIL $LOADFILE)) (SETQ PRINTP NIL))
  ;; Should really get the truename of FILE.
  (IF PRINTP (FORMAT T "~%~A being loaded.~%" FILE))
  (let* ((path (pathname FILE))
	 (tem (errset (LOAD (pathname FILE)))))
    (or tem (merror "Load failed for ~m" (namestring path)))
    (namestring path)))

(defun $directory (path)
  (cons '(mlist) (mapcar 'namestring (directory ($filename_merge path))))
  )

(DEFMFUN TRUEFNAME (FILE)
 (OR (PROBE-FILE FILE)
     #-cl (CLOSE (OPEN FILE '(IN FIXNUM)))
	; The OPEN is to generate the appropriate error handling.
	; The CLOSE is just to be nice.
     #+Multics FILE
	; The Multics CLOSE function returns T always. 
	; At least we know we can open and close the file.
	; On Multics PROBE-FILE calls ALLFILES which demands access to
	; the directory. 
     ))


#+CL
(DEFMFUN MTRUENAME (STREAM)
  (MFILE-OUT (UNEXPAND-PATHNAME (FUNCALL STREAM ':NAME))))

(DEFMFUN CARFILE (FILE)  ; FILE is in OldIO list format.
 (IF (= (LENGTH FILE) 3) (CDR FILE) FILE))

;; SPECP is T if the file is being batched for TRANSL, or $LOAD, 
;;	or some other special purpose.
#-Franz
(DEFMACRO FILEPOS-CHECK () `(IF SPECP (SETQ FILEPOS (FILEPOS FILE-OBJ))))


(DEFMSPEC $KILL (FORM) (MAPC #'KILL1 (CDR FORM)) #+GC (GCTWA) '$DONE)

(defvar $dont_kill_symbols_with_lisp_source_files  t "Prevents killing functional properties 
 of items which have been translated and loaded")

(DEFMFUN KILL1 (X)
 (funcall #'(LAMBDA (Z)
   (COND ((AND ALLBUTL (MEMQ X ALLBUTL)))
	 ((EQ (SETQ X (GETOPR X)) '$LABELS)
	  (DOLIST (U (CDR $LABELS))
		  (COND ((AND ALLBUTL (MEMQ U ALLBUTL))
			 (SETQ Z (NCONC Z (NCONS U))))
			(T (MAKUNBOUND U) (REMPROP U 'TIME)
			   (REMPROP U 'NODISP))))
	  (SETQ $LABELS (CONS '(MLIST SIMP) Z) $LINENUM 0 DCOUNT 0))
	 ((MEMQ X '($VALUES $ARRAYS $ALIASES $RULES $PROPS $LET_RULE_PACKAGES))
	  (MAPC #'KILL1 (CDR (SYMBOL-VALUE X))))
	 ((MEMQ X '($FUNCTIONS $MACROS $GRADEFS $DEPENDENCIES))
	  (MAPC #'(LAMBDA (Y) (KILL1 (CAAR Y))) (CDR (SYMBOL-VALUE X))))
	 ((EQ X '$MYOPTIONS))
	 ((EQ X '$TELLRATS) (SETQ TELLRATLIST NIL))
	 ((EQ X '$RATWEIGHTS) (SETQ *RATWEIGHTS NIL $RATWEIGHTS '((MLIST SIMP))))
	 ((EQ X '$FEATURES)
	  (COND ((NOT (EQUAL (CDR $FEATURES) FEATUREL))
		 (SETQ $FEATURES (CONS '(MLIST SIMP) (copy-top-level FEATUREL ))))))
	 ((OR (EQ X T) (EQ X '$ALL))
	  (MAPC #'KILL1 (CDR $INFOLISTS))
	  (SETQ $RATVARS '((MLIST SIMP)) VARLIST NIL GENVAR NIL
		CHECKFACTORS NIL GREATORDER NIL LESSORDER NIL $GENSUMNUM 0
		$WEIGHTLEVELS '((MLIST)) *RATWEIGHTS NIL $RATWEIGHTS '((MLIST SIMP))
		TELLRATLIST NIL $DONTFACTOR '((MLIST)) $SETCHECK NIL)
	  (KILLALLCONTEXTS))
	 ((SETQ Z (ASSQ X '(($CLABELS . $INCHAR) ($DLABELS . $OUTCHAR)
			    ($ELABELS . $LINECHAR))))
	  (MAPC #'(LAMBDA (Y) (REMVALUE Y '$KILL)) (GETLABELS* (EVAL (CDR Z)) NIL)))
	 ((AND (EQ (ml-typep X) 'fixnum) (NOT (< X 0))) (REMLABELS X))
	 ((and $dont_kill_symbols_with_lisp_source_files
	       (symbolp x)(or (get x 'translated)
			      (and (fboundp x)
				   (compiled-function-p
				     (symbol-function x))))))
	 ((ATOM X)
	  (SETQ Z (OR (AND (MEMQ X (CDR $ALIASES)) (GET X 'NOUN)) (GET X 'VERB)))
	  (COND ((OR (NULL ALLBUTL) (NOT (MEMQ Z ALLBUTL)))
		 (REMVALUE X '$KILL) (REMCOMPARY X)
		 (IF (MEMQ X (CDR $CONTEXTS)) ($KILLCONTEXT X))
		 (IF (MGET X '$RULE)
		     (LET ((Y (RULEOF X)))
			  (COND (Y ($REMRULE Y X))
				(T #+MACLISP (REMPROP X 'EXPR)
				   #-MACLISP (FMAKUNBOUND X)
				   (DELQ X $RULES 1)))))
		 (IF (AND (GET X 'OPERATORS) (RULECHK X)) ($REMRULE X '$ALL))
		 (IF (MGET X 'TRACE) (MACSYMA-UNTRACE X))
		 (WHEN  (GET X 'TRANSLATED)
		       (REMOVE-TRANSL-FUN-PROPS X) 
		       (REMOVE-TRANSL-ARRAY-FUN-PROPS X))
		 (IF (NOT (GET X 'SYSCONST)) (REMPROP X 'MPROPS))
		 (DOLIST (U '(BINDTEST NONARRAY EVFUN EVFLAG OPERS SPECIAL MODE))
			 (REMPROP X U))
		 (DOLIST (U OPERS)
			 (IF (AND (REMPROP X U)
				  (EQ (GET X 'OPERATORS) 'SIMPARGS1))
			     (REMPROP X 'OPERATORS)))
		 (WHEN (MEMQ X (CDR $PROPS))
		       (REMPROP X 'SP2) (KILLFRAME X)
		       (LET ((Y (STRIPDOLLAR X)))
			    (REMPROP Y 'ALPHABET) (zl-DELETE (GETCHARN Y 1) ALPHABET 1)))
		 (LET ((Y (GET X 'OP)))
		      (IF (AND Y (NOT (MEMQ Y MOPL)) (MEMQ Y (CDR $PROPS)))
			  (KILL-OPERATOR X)))
		 (REMALIAS X NIL) (DELQ X $ARRAYS 1) (REMPROPCHK X)
		 #+MACLISP (ARGS X NIL)
		 (zl-DELETE (zl-ASSOC (NCONS X) $FUNCTIONS) $FUNCTIONS 1)
		 (zl-DELETE (zl-ASSOC (NCONS X) $MACROS) $MACROS 1)
		 (LET ((Y (zl-ASSOC (NCONS X) $GRADEFS)))
		      (WHEN Y (REMPROP X 'GRAD) (zl-DELETE Y $GRADEFS 1)))
		 (zl-DELETE (zl-ASSOC (NCONS X) $DEPENDENCIES) $DEPENDENCIES 1)
		 (IF Z (KILL1 Z)))))
	 ((AND (EQ (CAAR X) 'MLIST) (EQ (ml-typep (CADR X)) 'fixnum)
	       (OR (AND (NULL (CDDR X)) (SETQ X (APPEND X (NCONS (CADR X)))))
		   (AND (EQ (ml-typep (CADDR X)) 'fixnum) (NOT (> (CADR X) (CADDR X))))))
	  (LET (($LINENUM (CADDR X))) (REMLABELS (f- (CADDR X) (CADR X)))))
	 ((SETQ Z (MGETL (CAAR X) '(HASHAR ARRAY))) (REMARRELEM Z X))
	 ((AND (EQ (CAAR X) '$ALLBUT)
	       (NOT (DOLIST (U (CDR X)) (IF (NOT (SYMBOLP U)) (RETURN T)))))
	  (LET ((ALLBUTL (CDR X))) (KILL1 T)))
	 (T (IMPROPER-ARG-ERR X '$KILL))))
  NIL))


(DEFMFUN REMLABELS (N)
       (PROG (L X)
	     (SETQ L (LIST (EXPLODEN $INCHAR) (EXPLODEN $OUTCHAR) (EXPLODEN $LINECHAR)))
	LOOP (SETQ X (MEXPLODEN $LINENUM))
	     (DO ((L L (CDR L)))( (NULL L)) (REMVALUE (IMPLODE (APPEND (CAR L) X)) '$KILL))
	     (IF (OR (MINUSP (SETQ N (f1- N))) (= $LINENUM 0)) (RETURN NIL))
	     (SETQ $LINENUM (f1- $LINENUM))
	     (GO LOOP)))

(DEFMFUN REMVALUE (X FN)
 (COND ((NOT (SYMBOLP X)) (IMPROPER-ARG-ERR X FN))
       ((BOUNDP X)
	(LET (Y)
	     (COND ((OR (SETQ Y (MEMQ X (CDR $VALUES))) (MEMQ X (CDR $LABELS)))
		    (COND (Y (DELQ X $VALUES 1))
			  (T (DELQ X $LABELS 1)
			     (REMPROP X 'TIME) (REMPROP X 'NODISP)
			     (IF (NOT (ZEROP DCOUNT)) (SETQ DCOUNT (f1- DCOUNT)))))
		    (MAKUNBOUND X) T)
		   ((GET X 'SPECIAL) (MAKUNBOUND X) T)
		   (TRANSP (SET X X) T)
		   ((EQ X '$DEFAULT_LET_RULE_PACKAGE) T)
		   (T (MTELL "Warning: Illegal REMVALUE attempt:~%~M" X) NIL))))))

(DEFMFUN RULEOF (RULE)
 (OR (MGET RULE 'RULEOF)
     (LET ((OP (CAAADR (MGET RULE '$RULE))) L)
	  (AND (SETQ L (GET OP 'RULES)) (MEMQ RULE L) OP))))

(DEFMFUN $DEBUGMODE (X) (DEBUGMODE1 NIL X))


#-NIL
(DEFUN DEBUGMODE1 (ASSIGN-VAR Y)
 ASSIGN-VAR  ; ignored
 #+MACLISP (SETQ DEBUG (COND (Y (*RSET T) Y) (T (*RSET NIL))))
 #+Franz   (prog2 (setq debug y) (debugging y))
 #+akcl (if (eq y '$lisp) (si::use-fast-links y))
 #+cl  (SETQ DEBUG (SETQ *RSET Y)))

#+cl
(defun retrieve1 (a b &aux (eof '(nil)))
  (let ((*mread-prompt* b) r )
    (declare (special *mread-prompt*))
    (catch 'macsyma-quit
      (tagbody
       top
       (SETQ R    (dbm-read (or a *terminal-io*) nil eof))
       (cond ((and (consp r) (keywordp (car r)))
	      (let ((value (break-call (car r) (cdr r) 'break-command)))
		(if (eq value :resume) (return-from retrieve1 '$exit))
		(go top))))
	      
       )
      )
    (nth 2 r)
    ))

#-NIL
(DEFMFUN ERRBREAK (Y)  ; The ERRSET interrupt function
 (COND
  (DEBUG
   ((LAMBDA (BRKLVL VARLIST GENVAR ERRBRKL LINELABLE)
	    (declare (special $help))
     (PROG (X ^Q #.TTYOFF O^R #+MACLISP ERRSET #+LISPM ERROR-CALL TIM $%% 
	    #+Franz errset
	    $BACKTRACE  RETVAL OLDST ($help $help))
	   #+ (or franz maclisp cl)
	   (SETQ  ERRSET 'ERRBREAK1)
	   #+LISPM (setq ERROR-CALL 'ERRBREAK1)
	   (SETQ TIM (RUNTIME) $%% '$%%
		 ;; just in case baktrcl is cons'd on the stack
		 $BACKTRACE (CONS '(MLIST SIMP) (copy-list BAKTRCL)))
	   (SETQ O^R #.WRITEFILEP #.WRITEFILEP (AND #.WRITEFILEP (NOT DSKFNP)))
	   (cond ((eq y 'noprint))
		 (t 
		  (MTERPRI)
		  (IF Y (PRINC 'MACSYMA-BREAK) (PRINC 'ERROR-BREAK))
		  (UNLESS (ZEROP BRKLVL) (PRINC " level ") (PRINC BRKLVL))
		  (PRINC " Type EXIT; to quit, HELP; for more help.")))
	   (setq $help
     "BACKTRACE; will give a successive list of forms 
 (you must have already set ?DEBUG:ALL; for BACKTRACE to record) 
     LISP; goes to lisp 
     TOPLEVEL; goes all the way to top level 
     EXIT; exits one level of the error break")
	   (MTERPRI)
      A    (COND
	    ((NULL
	      (CATCH 'MACSYMA-BREAK
		      (LET ((STATE-PDL (CONS 'MACSYMA-BREAK STATE-PDL)))
			   (ERRSET
			    (COND ((EQ (SETQ X
					     (RETRIEVE1 NIL
							(if y "_ " "(debug) "
							       ))) '$EXIT)
				   (TIMEORG TIM)
				   (SETQ RETVAL 'EXIT) (GO END))
				  ((EQ X '$LISP)
#+MACLISP			   (LET ((STATE-PDL (CONS 'LISP-BREAK STATE-PDL)))
					(*BREAK T 'LISP) (MTERPRI))  ; ^B also works
				   (SETQ RETVAL 'LISP)
				   (GO END))
				  ((EQ X '$TOPLEVEL)
				   (COND ((CATCH 'MBREAK
						  (LET (ST OLDST REPHRASE
							(MBREAK (CONS BINDLIST LOCLIST)))
						       (SETQ $LINENUM (f1+ $LINENUM))
						       (CONTINUE)))
					  (GO END))
					 (T (MTELL-OPEN "Back to the break~%"))))
				  (T (LET (($DISPFLAG DISPFLAG)) (SETQ $%% (MEVAL X)))
				     (IF DISPFLAG (DISPLA $%%) (MTERPRI))))))))
	     (ERRLFUN1 ERRBRKL)
	     (MTELL-OPEN "~%(Still in break loop)~%")))
	     (GO A)
      END  (unless (eq y 'noprint)
		   (PRINC "Exited from the break ")
		   (IF (NOT (ZEROP BRKLVL)) (PRINC BRKLVL))
		   (MTERPRI)
		   )
	   (IF O^R (SETQ #.WRITEFILEP T))
#+(or Franz MACLISP)   (RETURN NIL) #+cl (RETURN RETVAL)))
    (f1+ BRKLVL) VARLIST GENVAR (CONS BINDLIST LOCLIST) LINELABLE))))

#-NIL
(DEFUN ERRBREAK1 (IGN) IGN NIL)  ; Used to nullify ERRSETBREAKs

#-NIL
(DEFUN ERRBREAK2 (IGN) ign
	; An alternate ERRSET interr. function; used by PARSE and DISPLAY
  #-cl IGNORE  ; ignored
 (LET ((STATE-PDL (CONS 'LISP-BREAK STATE-PDL))) (*BREAK ERRBRKSW 'ERST)))


;; The ^B interrupt function
(DEFUN MPAUSE (X)
  X ;Ignored       
  (LET ((STATE-PDL (LIST* 'LISP-BREAK '^B-BREAK STATE-PDL))
	(MOREMSG "--Pause--"))
       #+PDP10 (ENDPAGEFN T 'MORE-FUN)
       #+PDP10 (BUFFCLEAR NIL)
       (TIMESOFAR T)
       #+MACLISP (NOINTERRUPT NIL)
       (*BREAK T ^BMSG))
  #+PDP10 (TTYRETFUN T))



(DEFMSPEC $TOBREAK (X)
 (IF MBREAK (THROW 'MBREAK (CDR X))
	    (MERROR "TOBREAK may be used only within a MACSYMA break.")))

(DEFUN ERRLFUN (X)
 (WHEN (NULL
	(ERRSET
	 (PROGN #-LISPM (SETQ ^S NIL)
		#+PDP10 (CLOSE SAVEFILE)
		#-LISPM (IF LOADF (SETQ DEFAULTF LOADF LOADF NIL))
		#+PDP10 (ENDPAGEFN T 'MORE-FUN))))
       #-LISPM (SETQ ^Q NIL) (MTELL-OPEN "~%ERRLFUN has been clobbered."))
 (IF $ERRORFUN (IF (NULL (ERRSET (MAPPLY1 $ERRORFUN NIL $ERRORFUN nil)))
		   (MTELL "~%Incorrect ERRORFUN")))
 (WHEN (NULL
	(ERRSET
	 (PROGN (IF (NOT (EQ X 'MQUIT)) (SUPUNBIND)) (CLEARSIGN))))
       #-LISPM (SETQ ^Q NIL) (MTELL-OPEN "~%ERRLFUN has been clobbered."))
 (WHEN (NULL X) (PRINC QUITMSG) (SETQ QUITMSG " ")))

(DEFUN SUPUNBIND NIL
 (MUNBIND (REVERSE BINDLIST)) (DO NIL ((NULL LOCLIST)) (MUNLOCAL)))

(DEFMFUN ERRLFUN1 (MPDLS)
       (DO ((L BINDLIST (CDR L)) (L1)) ((EQ L (CAR MPDLS)) (MUNBIND L1))
	   (SETQ L1 (CONS (CAR L) L1)))
       (DO NIL ((EQ LOCLIST (CDR MPDLS))) (MUNLOCAL)))

(DEFMFUN GETALIAS (X) (COND ((GET X 'ALIAS)) ((EQ X '$FALSE) NIL) (T X)))

(DEFMFUN MAKEALIAS (X) (IMPLODE (CONS #\$ (EXPLODEN X))))


;; (DEFMSPEC $F (FORM) (SETQ FORM (FEXPRCHECK FORM)) ...)
;; makes sure that F was called with exactly one argument and
;; returns that argument.

(DEFMFUN FEXPRCHECK (FORM)
  (IF (OR (NULL (CDR FORM)) (CDDR FORM))
      (MERROR "~:M takes just one argument." (CAAR FORM))
      (CADR FORM)))

(DEFMFUN NONSYMCHK (X FN)
  (UNLESS (SYMBOLP X)
	  (MERROR "The argument to ~:M must be a symbolic name:~%~M" FN X)))

;(DEFMFUN NONVARCHK (X FN FLAG 2NDP)
;  (WHEN (OR (MNUMP X) (INTEGERP X) (AND FLAG (ATOM X) (CONSTANT X))
;	    (AND (NOT (ATOM X)) (NOT (EQ (CAAR X) 'MQAPPLY)) (MOPP1 (CAAR X))))
;	(MERROR "Non-variable~Margument to ~:M: ~M"
;		(IF 2NDP '|& 2nd | '|& |) FN X)))

(DEFMFUN PRINL (L) (DOLIST (X L) (PRINC X) (TYO #\Space)))

(DEFMFUN $PRINT N
  (IF (= N 0)
      '((MLIST SIMP))
      (LET ((L (LISTIFY N)))
	(DO ((L L (CDDR L)))( (NULL L)) (RPLACD L (CONS '| | (CDR L))))
	(DISPLA (SETQ PRINTMSG (CONS '(MTEXT) L)))
	(CADR (REVERSE L)))))


(DEFMSPEC $PLAYBACK (X) (SETQ X (CDR X))
  (LET ((STATE-PDL (CONS 'PLAYBACK STATE-PDL)))
       (PROG (L L1 L2 NUMBP SLOWP NOSTRINGP INPUTP TIMEP GRINDP INCHAR LARGP)
	     (SETQ INCHAR (GETLABCHARN $INCHAR))
			; Only the 1st alphabetic char. of $INCHAR is tested
	     (SETQ TIMEP $SHOWTIME GRINDP $GRIND)
	     (DO ((X X (CDR X)))( (NULL X))
		 (COND ((EQ (ml-typep (CAR X)) 'fixnum) (SETQ NUMBP (CAR X)))
		       ((EQ (CAR X) '$ALL))
		       ((EQ (CAR X) '$SLOW) (SETQ SLOWP T))
		       ((EQ (CAR X) '$NOSTRING) (SETQ NOSTRINGP T))
		       ((EQ (CAR X) '$GRIND) (SETQ GRINDP T))
		       ((EQ (CAR X) '$INPUT) (SETQ INPUTP T))
		       ((MEMQ (CAR X) '($SHOWTIME $TIME)) (SETQ TIMEP (OR TIMEP T)))
		       ((MEMQ (CAR X) '($GCTIME $TOTALTIME)) (SETQ TIMEP '$ALL))
		       ((SETQ L2 (LISTARGP (CAR X)))
			(SETQ L1 (NCONC L1 (GETLABELS (CAR L2) (CDR L2) NIL)) LARGP T))
		       (T (IMPROPER-ARG-ERR (CAR X) '$PLAYBACK))))
	     (COND ((AND LARGP (NULL NUMBP)) (GO LOOP))
		   ((AND (SETQ L (CDR $LABELS)) (NOT $NOLABELS)) (SETQ L (CDR L))))
	     (WHEN (OR (NULL NUMBP) (< (LENGTH L) NUMBP))
		   (SETQ L1 (REVERSE L)) (GO LOOP))
	     (DO ((I NUMBP (f1- I)) (L2)) ((ZEROP I) (SETQ L1 (NCONC L1 L2)))
		 (SETQ L2 (CONS (CAR L) L2) L (CDR L)))
	LOOP (IF (NULL L1) (RETURN '$DONE))
	     ((LAMBDA (ERRSET INCHARP)
	       (ERRSET
		(COND ((AND (NOT NOSTRINGP) INCHARP)
		       (LET ((LINELABLE (CAR L1))) (MTERPRI) (PRINTLABEL))
		       (IF GRINDP (MGRIND (MEVAL1 (CAR L1)) NIL)
				  (MAPC #'TYO (MSTRING (MEVAL1 (CAR L1)))))
		       (IF (GET (CAR L1) 'NODISP) (PRINC '$) (PRINC '|;|))
		       (MTERPRI))
		      ((OR INCHARP
			   (PROG2 (WHEN (AND TIMEP (SETQ L (GET (CAR L1) 'TIME)))
					(SETQ X (GCTIMEP TIMEP (CDR L)))
					(MTELL-OPEN "~A msec." (CAR L))
				   #+GC (IF X (MTELL-OPEN "  GCtime= ~A msec." (CDR L)))
					(MTERPRI))
				  (NOT (OR INPUTP (GET (CAR L1) 'NODISP)))))
		       (MTERPRI) (DISPLA (LIST '(MLABLE) (CAR L1) (MEVAL1 (CAR L1)))))
		      (T (GO A)))))
	      'ERRBREAK2 (char= (GETLABCHARN (CAR L1)) INCHAR))
	     (IF (AND SLOWP (CDR L1) (NOT (CONTINUEP))) (RETURN '$TERMINATED))
	A    (SETQ L1 (CDR L1))
	     (GO LOOP))))

(DEFUN LISTARGP (X)
 (LET (HIGH)
      (IF (AND ($LISTP X) (EQ (ml-typep (CADR X)) 'fixnum)
	       (OR (AND (NULL (CDDR X)) (SETQ HIGH (CADR X)))
		   (AND (EQ (ml-typep (SETQ HIGH (CADDR X))) 'fixnum)
			(NOT (> (CADR X) HIGH)))))
	  (CONS (CADR X) HIGH))))

(DEFMSPEC $ALIAS (FORM)
  (IF (ODDP (LENGTH (SETQ FORM (CDR FORM))))
      (MERROR "ALIAS takes an even number of arguments."))
  (DO ((L NIL (CONS (ALIAS (POP FORM) (POP FORM))
		    L)))
      ((NULL FORM)
       `((MLIST SIMP),@(NREVERSE L)))))

(DEFMFUN ALIAS (X Y)
  (COND ((NONSYMCHK X '$ALIAS))
	((NONSYMCHK Y '$ALIAS))
	((NOT (EQ (GETCHAR X 1) '$))
	 (MERROR "-ed symbols may not be aliased. ~M" X))
	((GET X 'REVERSEALIAS)
	 (IF (NOT (EQ X Y))
	     (MERROR "~M already is aliased." X)))
	(T (PUTPROP X Y'ALIAS)
	   (PUTPROP Y (STRIPDOLLAR X) 'REVERSEALIAS)
	   (ADD2LNC Y $ALIASES)
	   Y)))

(DEFMFUN REMALIAS (X &optional REMP)
 (LET ((Y (AND (OR REMP (MEMQ X (CDR $ALIASES))) (GET X 'REVERSEALIAS))))
      (COND ((AND Y (EQ X '%DERIVATIVE))
	     (REMPROP X 'REVERSEALIAS) (DELQ X $ALIASES 1)
	     (REMPROP '$DIFF 'ALIAS) '$DIFF)
	    (Y (REMPROP X 'REVERSEALIAS) (REMPROP X 'NOUN) (DELQ X $ALIASES 1)
	       (REMPROP (SETQ X (MAKEALIAS Y)) 'ALIAS) (REMPROP X 'VERB) X))))

(DEFMFUN STRIPDOLLAR (X)
 (COND ((NOT (ATOM X))
	(COND ((AND (EQ (CAAR X) 'BIGFLOAT) (NOT (MINUSP (CADR X)))) (IMPLODE (FPFORMAT X)))
	      (T (MERROR "Atomic arg required:~%~M" X))))
       ((NUMBERP X) X)
       ((NULL X) 'FALSE)
       ((EQ X T) 'TRUE)
       ((MEMQ (GETCHAR X 1) '($ % &))
	  #-(or Franz NIL cl) (IMPLODE (CDR (EXPLODEN X)))
	  #+cl (intern (subseq (string x) 1))
	  #+NIL (intern (substring x 1))
	  #+Franz (concat (substring x 2))	;Nice start/end conventions.
	  )
       (T X)))

(DEFMFUN FULLSTRIP (X) (MAPCAR #'FULLSTRIP1 X))

(DEFMFUN FULLSTRIP1 (X)
 (OR (AND (NUMBERP X) X)
     (GET X 'REVERSEALIAS)
     (LET ((U (ASSQR X ALIASLIST))) (IF U (IMPLODE (STRING*1 (CAR U)))))
     (STRIPDOLLAR X)))

(DEFUN STRING* (X)
 (OR (AND (NUMBERP X) (EXPLODEN X))
     (LET ((U (ASSQR X ALIASLIST))) (IF U (STRING*1 (CAR U))))
     (STRING*1 X)))

(DEFUN STRING*1 (X) (LET (STRINGDISP $LISPDISP) (MAKESTRING X)))

(DEFUN MAKSTRING* (X)
 (SETQ X (STRING* X))
 (DO ((L X (CDR L)))( (NULL L)) (RPLACA L (ASCII (CAR L))))
 X)

(DEFMFUN $NOUNIFY (X)
 (LET (Y U)
      (NONSYMCHK X '$NOUNIFY)
      (SETQ X (AMPERCHK X))
      (COND ((GET X 'VERB))
	    ((GET X 'NOUN) X)
	    ((OR (SETQ U (MEMQ (CAR (SETQ Y (EXPLODEC X))) '($ M)))
		 (NOT (EQ (CAR Y) '%)))
	     (SETQ Y (IMPLODE (CONS '% (IF U (CDR Y) Y))))
	     (PUTPROP Y X 'NOUN) (PUTPROP X Y 'VERB))
	    (T X))))

(DEFMFUN $VERBIFY (X)
 (NONSYMCHK X '$VERBIFY)
 (SETQ X (AMPERCHK X))
 (COND ((GET X 'NOUN))
       ((AND (char= (GETCHARN X 1) #\%)
	     (PROG2 ($NOUNIFY #+NIL (let ((s (copy-seq (symbol-name x))))
				      (setf (schar s 0) #.(code-char #\$))
				      (intern s))
			      #-NIL (IMPLODE (CONS #\$ (CDR (EXPLODEN X)))))
		    (GET X 'NOUN))))
       (T X)))

;(DEFMFUN AMPERCHK (NAME)
; (IF (char= (GETCHARN NAME 1) #\&)
;     (OR (GET NAME 'OPR)
;	 #+NIL (intern (nstring-upcase (string-append "$" name)))
;	 #-NIL (IMPLODE (CONS #\$ (CASIFY-EXPLODEN NAME))))
;     NAME))

(DEFMFUN DOLLARIFY-NAME (NAME)
 (LET ((N (GETCHARN NAME 1)))
      (COND ((char= N #\&)
	     (OR (GET NAME 'OPR)
		 (LET ((NAMEL (CASIFY-EXPLODEN NAME)) AMPNAME DOLNAME)
		      (COND ((GET (SETQ AMPNAME (IMPLODE (CONS #\& NAMEL))) 'OPR))
			    (T (SETQ DOLNAME (IMPLODE (CONS #\$ NAMEL)))
			       (PUTPROP DOLNAME AMPNAME 'OP)
			       (PUTPROP AMPNAME DOLNAME 'OPR)
			       (ADD2LNC AMPNAME $PROPS)
			       DOLNAME)))))
	    ((char= N #\%) ($VERBIFY NAME))
	    (T NAME))))

#-NIL
(DEFMFUN $RANDOM N (APPLY #'RANDOM (LISTIFY N)))


(DEFMSPEC $STRING (FORM)
 (SETQ FORM (STRMEVAL (FEXPRCHECK FORM)))
 (SETQ FORM (IF $GRIND (STRGRIND FORM) (MSTRING FORM)))
 (SETQ ST (REVERSE FORM) REPHRASE T)
 (IMPLODE (CONS #\& FORM)))

(DEFMFUN MAKSTRING (X)
 (SETQ X (MSTRING X)) (DO ((L X (CDR L)))( (NULL L)) (RPLACA L (ASCII (CAR L)))) X)

(DEFMFUN STRMEVAL (X)
 (COND ((ATOM X) (MEVAL1 X))
       ((MEMQ (CAAR X) '(MSETQ MDEFINE MDEFMACRO)) X)
       (T (MEVAL X))))

(PROG1 '(ALIAS properties)
       (MAPC #'(LAMBDA (X) (PUTPROP (CAR X) (CADR X) 'ALIAS)
			   (PUTPROP (CADR X) (CADDR X) 'REVERSEALIAS))
	     '(($BLOCK MPROG BLOCK) ($LAMBDA LAMBDA LAMBDA)
	       ($ABS MABS ABS) ($SUBST $SUBSTITUTE SUBST)
	       ($GO MGO GO) ($SIGNUM %SIGNUM SIGNUM)
	       ($RETURN MRETURN RETURN) ($FACTORIAL MFACTORIAL FACTORIAL)
	       ($NOUUO NOUUO NOUUO) ($RSET *RSET RSET)
	       ($IBASE *read-base* *read-base*) ($OBASE *print-base* OBASE) ($NOPOINT *NOPOINT NOPOINT)
	       ($MODULUS MODULUS MODULUS) ($ZUNDERFLOW ZUNDERFLOW ZUNDERFLOW)
	       ($TTYOFF #.TTYOFF TTYOFF) ($WRITEFILE_ON #.WRITEFILEP WRITEFILE_ON)
	       ($MODE_DECLARE $MODEDECLARE MODE_DECLARE)))
       (MAPC #'(LAMBDA (X) (PUTPROP (CAR X) (CADR X) 'ALIAS))
	     '(($RATCOEFF $RATCOEF) ($RATNUM $RATNUMER) ($TRUE T)
	       ($BINOM %BINOMIAL) ($DERIVATIVE $DIFF) ($PROD $PRODUCT)
	       ($BOTHCOEFF $BOTHCOEF))))

(DEFMFUN AMPERCHK (NAME)
 " $AB ==> $AB,
   $aB ==> $aB,
   &aB ==> $AB,
   |aB| ==> |aB| "
 (IF (char= (GETCHARN NAME 1) #\&)
     (OR (GET NAME 'OPR)
	 ;;note the nil version does something else
	 #+ NIL   
	 (nstring-upcase (string-append "$" name))
	 #-NIL(IMPLODE (CONS #\$ (CASIFY-EXPLODEN NAME))))
     NAME))


#+cl
(defun casify-exploden (x)
  (cond ((char= (getcharn x 1) #\&)
	 (cdr (exploden (string-upcase (string x)))))
	(t (exploden x))))
#-cl
(DEFMFUN CASIFY-EXPLODEN (X)
 (SETQ X (EXPLODEN X))
 (IF (char= (CAR X) #\&) (MAPCAR #'CASIFY (CDR X)) (CDR X)))

(DEFMSPEC $STRINGOUT (X)  (SETQ X (CDR X))
 (LET (FILE MAXIMA-ERROR L1 TRUENAME)
    (SETQ FILE ($FILENAME_MERGE (CAR X)))
    (SETQ X (CDR X))
    (WITH-OPEN-FILE (SAVEFILE FILE :direction :output)
   (COND ((NULL
	   (ERRSET
	    (DO ((L X (CDR L)))( (NULL L))
		(COND ((MEMQ (CAR L) '($ALL $INPUT))
		       (SETQ L (NCONC (GETLABELS* $INCHAR T) (CDR L))))
		      ((EQ (CAR L) '$VALUES)
		       (SETQ L (NCONC (MAPCAN
					#'(LAMBDA (X)
					    (IF (BOUNDP X)
						(NCONS (LIST '(MSETQ) X (SYMBOL-VALUE X)))))
					(CDR $VALUES))
				      (CDR L))))
		      ((EQ (CAR L) '$FUNCTIONS)
		       (SETQ L (NCONC (MAPCAR
					#'(LAMBDA (X) (CONSFUNDEF (CAAR X) NIL NIL))
					(CDR $FUNCTIONS))
				      (MAPCAN
					#'(LAMBDA (X)
					    (IF (MGET X 'AEXPR)
						(NCONS (CONSFUNDEF X T NIL))))
					(CDR $ARRAYS))
				      (MAPCAR
					#'(LAMBDA (X) (CONSFUNDEF (CAAR X) NIL NIL))
					(CDR $MACROS))
				      (CDR L))))
		      ((SETQ L1 (LISTARGP (CAR L)))
		       (SETQ L (NCONC (GETLABELS (CAR L1) (CDR L1) T) (CDR L)))))
		(IF (NULL L) (RETURN NIL))
		(TERPRI SAVEFILE)
		(IF $GRIND (MGRIND (STRMEVAL (CAR L)) SAVEFILE)
		   #-Franz (PRINC (MAKNAM (MSTRING (STRMEVAL (CAR L))))
				  SAVEFILE)
		   #+Franz (mapc #'(lambda (ch) (tyo ch savefile))
				   (mstring (strmeval (car l)))))
		(IF (OR (AND (ATOM (CAR L)) (GET (CAR L) 'NODISP)) (NOT $STRDISP))
		    (TYO #\$ SAVEFILE)
		    (TYO SEMI-COLON-CHAR SAVEFILE)))))
	  (SETQ MAXIMA-ERROR T)))
   (SETQ TRUENAME (TRUENAME SAVEFILE))
   (TERPRI SAVEFILE))
   (IF MAXIMA-ERROR (LET ((ERRSET 'ERRBREAK1)) (MERROR "Error in STRINGOUT attempt")))
   (lisp::namestring TRUENAME)))
(DEFMSPEC $LABELS (CHAR)
 (SETQ CHAR (FEXPRCHECK CHAR))
 (NONSYMCHK CHAR '$LABELS)
 (CONS '(MLIST SIMP) (NREVERSE (GETLABELS* CHAR NIL))))

(DEFMFUN $%TH (X)
       (PROG (L OUTCHAR)
	     (IF (OR (NOT (EQ (ml-typep X) 'fixnum)) (= X 0))
		 (IMPROPER-ARG-ERR X '$%TH))
	     (IF (> X 0) (SETQ X (f- X)))
	     (IF (CDR $LABELS)
		 (SETQ L (CDDR $LABELS) OUTCHAR (GETLABCHARN $OUTCHAR)))
	LOOP (IF (NULL L) (MERROR "Improper call to %TH"))
	     (IF (AND (char= (GETLABCHARN (CAR L)) OUTCHAR) (= (SETQ X (f1+ X)) 0))
		   ; Only the 1st alphabetic character of $OUTCHAR is tested.
		 (RETURN (MEVAL (CAR L))))
	     (SETQ L (CDR L))
	     (GO LOOP)))

(DEFMFUN GETLABELS (N1 N2 FLAG)  ; FLAG = T for STRINGOUT, = NIL for PLAYBACK and SAVE.
 (DO ((I N1 (f1+ I)) (L1)
      (L (IF FLAG (LIST (EXPLODEN $INCHAR))
		  (LIST (EXPLODEN $INCHAR) (EXPLODEN $LINECHAR)
			(EXPLODEN $OUTCHAR)))))
     ((> I N2) (NREVERSE L1))
     (DO ((L L (CDR L)) (X (MEXPLODEN I)) (Z)) ((NULL L))
	 (IF (BOUNDP (SETQ Z (IMPLODE (APPEND (CAR L) X))))
	     (SETQ L1 (CONS Z L1))))))

(DEFMFUN GETLABELS* (CHAR FLAG)  ; FLAG = T only for STRINGOUT
 (DO ((L (IF FLAG (CDDR $LABELS) (CDR $LABELS)) (CDR L))
      (CHAR (GETLABCHARN CHAR)) (L1))
     ((NULL L) L1)
     (IF (char= (GETLABCHARN (CAR L)) CHAR)
			; Only the 1st alphabetic character is tested.
	 (SETQ L1 (CONS (CAR L) L1)))))

(DEFMFUN GETLABCHARN (LABEL)
 (LET ((CHAR (GETCHARN LABEL 2))) (IF (char= CHAR #\%) (GETCHARN LABEL 3) CHAR)))
(DEFMSPEC $ERRCATCH (FORM)
 (LET ((ERRCATCH (CONS BINDLIST LOCLIST)) RET)
      (IF (NULL (SETQ RET (LET (DEBUG)
			       (ERRSET (MEVALN (CDR FORM)) LISPERRPRINT))))
	  (ERRLFUN1 ERRCATCH))
      (CONS '(MLIST) RET)))

;(DEFMFUN $ERROR N  ; Moved to MAXSRC;MERROR
; (LET ((MSG (LISTIFY N)))
;      (IF (> N 0) (APPLY #'$PRINT MSG))
;      (IF ERRCATCH (ERROR))
;      (IF DEBUG (LET (($ERROR (CONS '(MLIST SIMP) (FSTRINGC MSG))))
;		      (ERRBREAK NIL)))
;      (MQUIT T)))


(DEFMSPEC $CATCH (FORM)
 (LET ((MCATCH (CONS BINDLIST LOCLIST)))
      (PROG1 (CATCH 'MCATCH (MEVALN (CDR FORM))) (ERRLFUN1 MCATCH))))

(DEFMFUN $THROW (EXP)
 (IF (NULL MCATCH) (MERROR "THROW not within CATCH:~%~M" EXP))
 (THROW 'MCATCH EXP))

(DEFMSPEC $TIME (L) (SETQ L (CDR L))
	  #-cl
 (MTELL-OPEN "TIME or [TOTALTIME, GCTIME] in msecs.:~%")
 #+cl
 (format t "~&Time:")
 (CONS '(MLIST SIMP)
       (MAPCAR
	#'(LAMBDA (X)
	   (OR (AND (SETQ X (OR (GET X 'TIME)
				(AND (EQ X '$%) (CONS (CADR $LASTTIME)
						      (CADDR $LASTTIME)))))
		    (IF (= (CDR X) 0)
			(CAR X)
			(LIST '(MLIST SIMP) (CAR X) (CDR X))))
	       '$UNKNOWN))
	L)))

(DEFMFUN TIMEORG (TIM)
 (IF (> THISTIME 0) (SETQ THISTIME (f+ THISTIME (f- (RUNTIME) TIM)))))

; Take difference of two times, return result in milliseconds.
#+LISPM
(DEFMFUN COMPUTIME (N1 N2) (// (f* 50. (TIME-DIFFERENCE
					N1 N2)) 3.))


#+CL (PROGN 'COMPILE
(DEFMFUN $QUIT () nil #+kcl (bye) #+lucid (quit)
   #+excl "don't know quit function")
(DEFMFUN $LOGOUT () (LOGOUT))
)
(DEFMFUN FILEPRINT (FNAME)  ; Takes filename in NAMELIST format.
 (COND ($FILE_STRING_PRINT (PRINC (NAMESTRING FNAME)) (PRINC "  "))
       (T (PRINC "[")
	  (PRINC (CADR FNAME)) (PRINC ", ")
	  (PRINC (CADDR FNAME)) (PRINC ", ")
	  (WHEN (CDDDR FNAME) (PRINC (CADDDR FNAME)) (PRINC ", "))  ; For TOPS-20
	  (PRINC (CAAR FNAME)) (PRINC ", ")
	  (PRINC (CADAR FNAME)) (PRINC "]  "))))

(DEFMFUN MFILE-OUT (FNAME)  ; Takes filename in NAMELIST or OldIO list format.
 (IF $FILE_STRING_PRINT
     (IMPLODE (CONS #\& (EXPLODEN (NAMESTRING FNAME))))
     (DOLLARIFY (IF (ATOM (CAR FNAME)) FNAME (APPEND (CDR FNAME) (CAR FNAME))))))

; File-processing stuff.  Lisp Machine version in MC:LMMAX;LMSUP.



(DEFUN MFILE NIL
   (FULLSTRIP (LIST $FILENAME (SETQ $FILENUM (f1+ $FILENUM)) $DEVICE $DIREC)))


;; This prevents single blank lines from appearing at the top of video 
;; terminals.  If at the upper left corner and we want to print a blank 
;; line, leave the cursor there and send the blank line to transcript 
;; files only.

#+(OR PDP10 NIL CL)
(DEFMFUN MTERPRI (&AUX X)
 #-nocp (setq x  (CURSORPOS))
 (IF (AND SMART-TTY X (EQUAL X '(0 . 0)))
     (LET ((#.TTYOFF T)) (TERPRI))
     (TERPRI)))

#+lispm
(DECLARE-top (SPECIAL TV:MORE-PROCESSING-GLOBAL-ENABLE))

#+LISPM
(DEFMFUN MORE-FUN (FILE)
  FILE ;ignored
 (send  *terminal-io* :send-if-handles :more-exception))


#+LISPM
(DEFUN MORE-FUN-INTERNAL (*terminal-io*
			  &AUX (*standard-input* *terminal-io*))
;;				 'SI:TERMINAL-IO-SYN-STREAM))
                          ;; SI:SYN-TERMINAL-IO))
 ; This clears the rest of the screen, unless we're at the bottom
 ; or too close to the top.
 (COND ((NOT (OR (< (CAR (CURSORPOS)) 10.)
		 (= (- TTYHEIGHT 2) (CAR (CURSORPOS)))))
	(CURSORPOS 'E)))
 ; Now go to the bottom of the screen and cause a more, unless disabled.
 (COND (TV:MORE-PROCESSING-GLOBAL-ENABLE
	(CURSORPOS 'Z) (CURSORPOS 'L)
	((LAMBDA (^Q)
	  ((LAMBDA (#.WRITEFILEP #.TTYOFF STATE-PDL)
	    (PRINC MOREMSG) (TYIPEEK)
	    ; Now see what the user feels like typing in.
	    (COND ($MOREWAIT
		   (DO ((L (COND ((EQ $MOREWAIT '$ALL) '(#\SPACE #\RETURN))
				 (T '(#\SPACE #\RETURN #\RUBOUT)))))
		       ((zl-MEMBER (TYIPEEK) L))
		      (TYI T))) ; eat other characters
		  (T (DO () ((NOT (zl-MEMBER (TYIPEEK) '(4 19. 21. 22. 29.))))
			 (TYI T)))) ; eat ^], etc. 
	    ; Now erase the MORE message
	    (COND (SMART-TTY (CURSORPOS 'Z) (CURSORPOS 'L)) (T (TERPRI))))
	   NIL NIL (CONS 'MORE-WAIT STATE-PDL))
	  ; Now decide whether to continue or flush
	  (COND ((char= #\Space (TYIPEEK))
		 (IF MORECONTINUE (LET (#.WRITEFILEP #.TTYOFF) (PRINC MORECONTINUE)))
		 (TYI T)) ; eat the space
		((char= #\RUBOUT (TYIPEEK))
		 (LET ((#.TTYOFF T)) (TERPRI))
		 (IF MOREFLUSH (PRINC MOREFLUSH))
		 (TYI T)  ; eat the rubout
		 (SETQ MORE-^W (OR MORE-^W (AND MOREFLUSH T))
		       #.WRITEFILEP (AND #.WRITEFILEP (NULL MOREFLUSH))))
		(T (COND ((OR (MEMQ 'BATCH STATE-PDL)
			      (AND (char< (TYIPEEK) #\SPACE)
				   (NOT (zl-MEMBER (TYIPEEK)
					  #.(cons 'list	(mapcar    'code-char '(2 7 11. 12. 25. 27. 28. 29. 30.))))
;						   '(#\Alpha #\Pi
;						    #\Up-Arrow
;						    #\Plus-Minus
;						    #\Right-Arrow
;						    #\Lozenge
;						    #\Less-Or-Equal
;						    #\Greater-Or-Equal
;						    #\Equivalence)
						   ))
			      (char>= (TYIPEEK) #. (code-char 128.)))
			  (TYI T)))  ; eat cr or other control character.
		   (IF MOREFLUSH (LET (#.WRITEFILEP #.TTYOFF) (PRINC MOREFLUSH)))
		   (SETQ MORE-^W (OR MORE-^W (AND MOREFLUSH T))))))
	 NIL)))
 ; Now home up, or advance to next line, and continue display.
 (IF SMART-TTY
     (COND (RUBOUT-TTY (LET (#.TTYOFF) (CURSORPOS T T) (CURSORPOS 'L)))
	   (T (MAXIMA-SLEEP 0.4) (FORMFEED)))
     (LET (#.TTYOFF #.WRITEFILEP) (TERPRI))))

(DEFMFUN $PAGEPAUSE (X) (PAGEPAUSE1 NIL X))


#-PDP10
(DEFUN PAGEPAUSE1 (X Y)
  X Y (MERROR "PAGEPAUSE does not exist in this system."))


#+(or cl LISPM)
(DEFMSPEC $STATUS (FORM)
  (setq form (cdr form))
  (LET* ((KEYWORD (car FORM))
	 (FEATURE (cadr form)))
       (assert (symbolp keyword))
       (assert (symbolp feature))
      (CASE KEYWORD
	($FEATURE (COND ((NULL FEATURE) (DOLLARIFY #-cl (STATUS FEATURES)
						   #+cl *features*))
			((MEMQ (intern (symbol-name
					 (FULLSTRIP1 FEATURE)) 'keyword)
			       #-cl(STATUS FEATURES)
			       #+cl *features*
			       ) T)))
	($STATUS '((MLIST SIMP) $FEATURE $STATUS))
	(T (MERROR "Unknown argument - STATUS:~%~M" KEYWORD)))))

#+(or cl lispm)
(defquote $sstatus (status-function item)
  (cond ((equal status-function '$feature)
	 (pushnew ($mkey item) *features*) t)
	((equal status-function '$nofeature)
	 (setq *features* (delete ($mkey item) *features*)) t)
	(t (error "know only how to set and remove feature status"))))

;; End of disk GC conditionalization.

#-PDP10 (PROGN 'COMPILE
(DEFMFUN $DSKGC (X) X NIL)
(DEFUN DSKGC1 (X Y) X Y NIL)
)



#+CL (SETQ ERROR-CALL 'ERRBREAK)

(PROGN (DO ((L '($SQRT $ERF $SIN $COS $TAN $LOG $PLOG $SEC $CSC $COT $SINH $COSH
	       $TANH $SECH $CSCH $COTH $ASIN $ACOS $ATAN $ACOT $ACSC $ASEC $ASINH
	       $ACOSH $ATANH $ACSCH $ASECH $ACOTH $BINOMIAL $GAMMA $GENFACT $DEL)
	   (CDR L)))( (NULL L))
	   ((LAMBDA (X)
	     (PUTPROP (CAR L) X 'ALIAS)
	     (PUTPROP X (STRIPDOLLAR (CAR L)) 'REVERSEALIAS))
	    ($NOUNIFY (CAR L))))
       ($NOUNIFY '$SUM) ($NOUNIFY '$PRODUCT)
       ($NOUNIFY '$INTEGRATE) ($NOUNIFY '$LIMIT)
       (DEFPROP $DIFF %DERIVATIVE VERB) (DEFPROP %DERIVATIVE $DIFF NOUN)
       '(NOUN properties))

(PROGN (MAPC #'(LAMBDA (X) (PUTPROP (CAR X) (CADR X) 'ASSIGN))
	     '(($DEBUGMODE DEBUGMODE1) ($BOTHCASES BOTHCASES1)
	       ($PAGEPAUSE PAGEPAUSE1) ($DSKGC DSKGC1)
	       ($TTYINTFUN TTYINTFUNSETUP)
	       ($FPPREC FPPREC1) ($POISLIM POISLIM1)
	       ($default_let_rule_package let-rule-setter)
	       ($current_let_rule_package let-rule-setter)
	       ($let_rule_packages let-rule-setter)))
       (MAPC #'(LAMBDA (X) (PUTPROP X 'NEVERSET 'ASSIGN)) (CDR $INFOLISTS))
       (DEFPROP $CONTEXTS NEVERSET ASSIGN)
       '(ASSIGN properties))



; Undeclarations for the file:
(declare-top (NOTYPE I N N1 N2 U1))

#-(or cl NIL)
(EVAL-WHEN (EVAL COMPILE) (SETQ *print-base* OLD-BASE *read-base* OLD-IBASE))
#+cl
(EVAL-WHEN (EVAL COMPILE) (SETQ *PRINT-BASE* OLD-BASE *READ-BASE* OLD-IBASE))

nil
