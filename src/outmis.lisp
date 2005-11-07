;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                ;;;
;;;                Miscellaneous Out-of-core Files                 ;;;
;;;                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macsyma-module outmis)

;;(declare-top (FIXNUM NN))

;;#+ITS (DECLARE (SPECIAL TTY-FILE))

;;(declare-top (SPLITFILE STATUS))

;;#+(or ITS Multics TOPS-20)
;;(declare-top (SPECIAL LINEL MATHLAB-GROUP-MEMBERS)
;;	 (*EXPR STRIPDOLLAR MEVAL)
;;	 (*LEXPR CONCAT))



;;#+(or ITS Multics TOPS-20)
;;(PROGN 'COMPILE

;;;; These are used by $SEND when sending to logged in Mathlab members
;;#-Multics
;;(SETQ MATHLAB-GROUP-MEMBERS
;;      '(JPG ELLEN GJC RZ KMP WGD MERMAN))

;;;; IOTA is a macro for doing file I/O binding, guaranteeing that
;;;;  the files it loads will get closed.
;;;;  Usage: (IOTA ((<variable1> <filename1> <modes1>)
;;;;                (<variable2> <filename2> <modes2>) ...)
;;;;		  <body>)
;;;;  Opens <filenameN> with <modesN> binding it to <variableN>. Closes
;;;;   any <variableN> which still has an open file or SFA in it when
;;;;   PDL unwinding is done.
;;;; No IOTA on Multics yet,
;;#-Multics
;;(EVAL-WHEN (EVAL COMPILE)
;;           (COND ((NOT (STATUS FEATURE IOTA))
;;                  (LOAD #+ITS '((DSK LIBLSP) IOTA FASL)
;;			#-ITS '((LISP) IOTA FASL)))))

;;;; TEXT-OUT
;;;;  Prints a list of TEXT onto STREAM.
;;;;
;;;;  TEXT must be a list of things to be printed onto STREAM.
;;;;    For each element in TEXT, A, if A is a symbol with first
;;;;    character "&", it will be fullstripped and PRINC'd into the
;;;;    stream; otherwise it will be $DISP'd onto STREAM (by binding
;;;;    OUTFILES and just calling $DISP normally).
;;;;
;;;;  STREAM must be an already-open file object.

;;(DEFUN TEXT-OUT (TEXT STREAM)
;;  (DO ((A TEXT (CDR A))
;;       (|^R| T)
;;       (|^W| T)
;;       (LINEL 69.)
;;       (OUTFILES (NCONS STREAM)))
;;      ((NULL A))
;;    (COND ((AND (SYMBOLP (CAR A))
;;		(EQ (GETCHAR (CAR A) 1.) '|&|))
;;	   (PRINC (STRIPDOLLAR (CAR A)) STREAM))
;;	  (T (TERPRI STREAM)
;;	     (MEVAL `(($DISP) ($STRING ,(CAR A))))))
;;	   (TERPRI STREAM)))

;;;; MAIL
;;;;  Sends mail to a recipient, TO, via the normal ITS mail protocol
;;;;  by writing out to DSK:.MAIL.;MAIL > and letting COMSAT pick it 
;;;;  up and deliver it. Format for what goes in the MAIL > file should
;;;;  be kept up to date with what is documented in KSC;?RQFMT >
;;;;
;;;;  TO must be a name (already STRIPDOLLAR'd) to whom the mail should
;;;;    be delivered.
;;;;
;;;;  TEXT-LIST is a list of Macsyma strings and/or general expressions
;;;;    which will compose the message.

;;#+(OR LISPM ITS) ;Do these both at once.
;;(DEFUN MAIL (TO TEXT-LIST)
;;  (IOTA ((STREAM  "DSK:.MAIL.;MAIL >" 'OUT))
;;    (mformat stream
;;       "FROM-PROGRAM:Macsyma
;;AUTHOR:~A
;;FROM-UNAME:~A
;;RCPT:~A
;;TEXT;-1~%"
;;       (STATUS USERID)
;;       (STATUS UNAME)
;;       (NCONS TO))
;;    (TEXT-OUT TEXT-LIST STREAM)))

;;;; This code is new and untested. Please report bugs -kmp
;;#+TOPS-20 
;;(DEFUN MAIL (TO TEXT-LIST)
;;  (IOTA ((STREAM "MAIL:/[--NETWORK-MAIL--/]..-1"
;;		 '(OUT ASCII DSK BLOCK NODEFAULT)))
;;    (MFORMAT STREAM
;;      "/~A
;;~A
;;/
;;From: ~A at ~A~%"
;;      (STATUS SITE) TO (STATUS USERID) (STATUS SITE))
;;    (COND ((NOT (EQ (STATUS USERID) (STATUS UNAME)))
;;	   (MFORMAT STREAM "Sender: ~A at ~A~%" (STATUS UNAME) (STATUS SITE))))
;;    (MFORMAT STREAM "Date: ~A
;;TO:   ~A~%~%"
;;	    (TIME-AND-DATE) TO)
;;    (TEXT-OUT TEXT-LIST STREAM)))

;;#+Multics
;;(defvar macsyma-mail-count 0 "The number of messages sent so far")
;;#+Multics
;;(progn 'compile
;;(DEFUN MAIL (TO TEXT-LIST)
;;  (let* ((open-file ())
;;	 (macsyma-unique-id (macsyma-unique-id 'unsent
;;					       (increment macsyma-mail-count)))
;;	 (file-name (catenate (pathname-util "pd")
;;			      ">macsyma_mail." macsyma-unique-id)))
;;    (unwind-protect
;;      (progn
;;       (setq open-file (open file-name '(out ascii block dsk)))
;;       (text-out text-list open-file)
;;       (close open-file)
;;       (cline (catenate "send_mail " to " -input_file " file-name
;;	                " -no_subject")))
;;      (deletef open-file))))

;;(defun macsyma-unique-id (prefix number)
;;  (implode (append (explode prefix) (list number))))
;;)

;;;; $BUG
;;;;  With no args, gives info on itself. With any positive number of
;;;;  args, mails all args to MACSYMA via the MAX-MAIL command.
;;;;  Returns $DONE

;;(DEFMSPEC $BUG (X) (SETQ X (CDR X))
;;       (COND ((NULL X)
;;	      (MDESCRIBE '$BUG))
;;	     (T 
;;	      (MAX-MAIL 'BUG X)))
;;       '$DONE)

;;#+MULTICS
;;(DEFMACRO CHECK-AND-STRIP-ADDRESS (ADDRESS)
;;  `(COND ((EQUAL (GETCHARN ,ADDRESS 1) #\&)
;;	  (STRIPDOLLAR ,ADDRESS))
;;	 (T (MERROR "Mail: Address field must be a string"))))
;;#-MULTICS
;;(DEFMACRO CHECK-AND-STRIP-ADDRESS (ADDRESS)
;;  `(STRIPDOLLAR ,ADDRESS))

;;;; $MAIL
;;;;  With no args, gives info on itself.
;;;;  With 1 arg, sends the MAIL to Macsyma. Like bug, only doesn't
;;;;   tag the mail as a bug to be fixed.
;;;;  With 2 or more args, assumes that arg1 is a recipient and other
;;;;   args are the text to be MAIL'd.
;;;; Works for Multics, ITS, and TOPS-20.
 
;;(DEFMSPEC $MAIL (X) (SETQ X (CDR X)) 
;;  (COND ((NULL X)
;;	 (MDESCRIBE '$MAIL))
;;	((= (LENGTH X) 1.)
;;	 (MAX-MAIL 'MAIL X))
;;	(T (LET ((NAME (CHECK-AND-STRIP-ADDRESS (CAR X))))
;;	     (MAIL NAME (CDR X))
;;    #-Multics(MFORMAT NIL "~&;MAIL'd to ~A~%" NAME))))
;;;;On Multics Mailer will do this.
;;       '$DONE)

;;;; MAX-MAIL
;;;;  Mails TEXT-LIST to MACSYMA mail. Normal ITS mail header 
;;;;  is suppressed. Header comes out as:
;;;;  From <Name> via <Source> command. <Date>
;;;;
;;;;  SOURCE is the name of the originating command (eg, BUG or 
;;;;    MAIL) to be printed in the header of the message.
;;;;
;;;;  TEXT-LIST is a list of expressions making up the message.

;;#+(OR LISPM ITS)
;;(DEFUN MAX-MAIL (SOURCE TEXT-LIST)
;; (IOTA ((MAIL-FILE "DSK:.MAIL.;_MAXIM >" '(OUT ASCII DSK BLOCK)))
;;   (LINEL MAIL-FILE 69.)
;;   (MFORMAT MAIL-FILE
;;      "FROM-PROGRAM:Macsyma
;;HEADER-FORCE:NULL
;;TO:(MACSYMA)
;;SENT-BY:~A
;;TEXT;-1
;;From ~A via ~A command. ~A~%"
;;      (STATUS UNAME) 
;;      (STATUS USERID)
;;      SOURCE
;;      (TIME-AND-DATE))
;;   (TEXT-OUT TEXT-LIST MAIL-FILE)
;;   (RENAMEF MAIL-FILE "MAIL >"))
;; (MFORMAT NIL "~&;Sent to MACSYMA~%")
;; '$DONE)

;;;; This code is new and untested. Please report bugs -kmp
;;#+TOPS-20 
;;(DEFUN MAX-MAIL (SOURCE TEXT-LIST)
;;  (IOTA ((MAIL-FILE "MAIL:/[--NETWORK-MAIL--/]..-1"
;;		    '(OUT ASCII DSK BLOCK NODEFAULT)))
;;    (MFORMAT MAIL-FILE
;;	     "/MIT-MC
;;BUG-MACSYMA
;;/From ~A at ~A via ~A command. ~A~%"
;;	  (STATUS USERID) (STATUS SITE) SOURCE (TIME-AND-DATE))
;;    (TEXT-OUT TEXT-LIST MAIL-FILE)
;;    (MFORMAT NIL "~%;Sent to MACSYMA")))

;;#+Multics
;;(defun max-mail (source text-list)
;;  (let ((address (cond ((eq source 'mail)
;;			(setq source "Multics-Macsyma-Consultant -at MIT-MC"))
;;		       (t (setq source "Multics-Macsyma-Bugs -at MIT-MC")))))
;;    (mail address text-list)))

;;); END of (or ITS Multics TOPS-20) conditionalization.


;; On ITS, this returns a list of user ids for some random reason.  On other
;; systems, just print who's logged in.  We pray that nobody uses this list for
;; value.

;;#+ITS
;;(PROGN 'COMPILE
;;(DEFMFUN $who nil
;;  (do ((tty*)
;;       (wholist nil (cond ((eq (getchar tty* 1)  ;just consoles, not device
;;			       'D)
;;			   wholist)
;;			  (t (LET ((UNAME (READUNAME)))
;;			       (COND ((MEMQ UNAME WHOLIST) WHOLIST)
;;				     (T (CONS UNAME WHOLIST)))))))
;;       (ur (crunit))
;;       (tty-file ((lambda (tty-file)
;;		    (readline tty-file)	   ;blank line
;;		    tty-file)  ;get rid of cruft
;;		  (open '((tty) |.file.| |(dir)|) 'single))))
;;      ((progn (readline tty-file)
;;	      (setq tty* (read tty-file))
;;	      (eq tty* 'free))
;;       (close tty-file)
;;       (apply 'crunit ur)
;;       (cons '(mlist simp) wholist))))

;;;; $SEND
;;;;  With no args, gives info about itself.
;;;;  With one arg, sends the info to any logged in Macsyma users.
;;;;  With 2 or more args, assumes that arg1 is a recipient and
;;;;   args 2 on are a list of expressions to make up the message.

;;(DEFMSPEC $SEND (X) (SETQ X (CDR X)) 
;;       (COND ((NULL X)
;;	      (MDESCRIBE '$SEND))
;;	     ((= (LENGTH X) 1.)
;;	      (MAX-SEND X))
;;	     (T
;;	      (MSEND (STRIPDOLLAR (CAR X)) (CDR X) T)))
;;       '$DONE)

;;;; MSEND
;;;;  Sends mail to a recipient, TO, by opening the CLI: device on the
;;;;  recipient's HACTRN.
;;;;
;;;;  TO must be a name (already FULLSTRIP'd) to whom the mail should
;;;;    be delivered. A header is printed of the form:
;;;;    [MESSAGE FROM MACSYMA USER <Uname>  <time/date>] (To: <Recipient>)
;;;;
;;;;  TEXT-LIST is a list of Macsyma strings and/or general expressions
;;;;    which will compose the message.
;;;;
;;;;  MAIL? is a flag that says whether the text should be forwarded
;;;;    as mail to the recipient if the send fails. Since the only current
;;;;    use for this is when sending to all of Mathlab, a value of NIL
;;;;    for this flag assumes a <Recipient> in the header should be
;;;;    "Mathlab Members" rather than the real name of the recipient.
;;;;    An additional flag might be used to separate these functions
;;;;    at some later time, but this should suffice for now.

;;(DEFUN MSEND (TO TEXT-LIST MAIL?)
;;  (COND ((EQ TO (STATUS UNAME))
;;	 (MERROR "You cannot SEND to yourself.  Use MAIL.")
;;	 ())
;;	((ERRSET (IOTA ((STREAM (LIST '(CLI *) TO 'HACTRN) 'OUT))
;;		    (MFORMAT STREAM
;;		       "[Message from MACSYMA User ~A] (To: ~A) ~A~%"
;;		       (STATUS UNAME)
;;		       (COND (MAIL? TO)
;;			     (T "Mathlab Members"))
;;		       (DAYTIME))
;;		    (TEXT-OUT TEXT-LIST STREAM))
;;		 NIL)
;;	 (MFORMAT NIL "~&;Sent to ~A~%" TO)
;;	 T)
;;	(MAIL? (COND ((PROBE-FILE (LIST '(USR *) TO 'HACTRN))
;;		      (MFORMAT NIL "~&;~A isn't accepting message.~%" TO))
;;		     (T (MFORMAT NIL "~&;~A isn't logged in.~%" TO)))
;;	       (MAIL TO TEXT-LIST)
;;	       (MFORMAT NIL "~&;Message `mail''d.~%")
;;	       () )
;;	(T ())))

;;;; MAX-SEND
;;;;  Send TEXT-LIST to any Mathlab members logged in.
;;;;  If no one on the list is logged in, or if the only logged in
;;;;  members are long idle, this command will forward the message
;;;;  to MACSYMA mail automatically (notifying the user).
;;;; 
;;;;  TEXT-LIST is a list of expressions or strings making up the
;;;;    message.


;;(DEFUN MAX-SEND (TEXT-LIST)				;
;;  (LET ((SUCCESS NIL)
;;	(PEOPLE (zl-DELETE (STATUS UNAME) (CDR ($WHO)))))
;;       (DO ((PERSON))
;;	   ((NULL PEOPLE))
;;	 (SETQ PERSON (PROG1 (CAR PEOPLE)
;;			     (SETQ PEOPLE (CDR PEOPLE))))
;;	 (COND ((MEMQ PERSON MATHLAB-GROUP-MEMBERS)
;;		(LET ((RESULT (MSEND PERSON TEXT-LIST NIL)))
;;		     (SETQ SUCCESS
;;			   (OR SUCCESS
;;			       (AND (< (IDLE-TIME PERSON) 9000.)
;;				    RESULT
;;				    T)))
;;		     (COND ((AND RESULT (> (IDLE-TIME PERSON) 9000.))
;;			    (MFORMAT NIL
;;				     " (but he//she is idle a long time)")))
;;		     (COND (RESULT (TERPRI)))))))
;;       (COND ((NOT SUCCESS)
;;	      (MFORMAT NIL "There's no one around to help, so I have mailed
;;your message to MACSYMA. Someone will get back
;;to you about the problem.")
;;	      (MAX-MAIL 'SEND TEXT-LIST)))
;;	    '$DONE))

;;(DEFUN READUNAME NIL 
;;       (TYI TTY-FILE)
;;       (DO ((I 1. (f1+ I)) (L) (N))
;;	   ((> I 6.) (IMPLODE (NREVERSE L)))
;;	   (SETQ N (TYI TTY-FILE))
;;	   (OR (= N 32.) (SETQ L (CONS N L)))))

;;;; IDLE-TIME
;;;;  Given an arg of UNAME (already FULLSTRIP'd) returns the idle-time
;;;;  of that user.

;;(defMACRO 6BIT (&rest X) (CAR (PNGET (CAR X) 6.)))

;;(DEFUN IDLE-TIME (UNAME)
;;  (IOTA ((USR-FILE (LIST '(USR *) UNAME 'HACTRN)))
;;    (LET ((TTY-NUMBER (SYSCALL 1 'USRVAR USR-FILE (6BIT CNSL))))
;;      (CLOSE USR-FILE)
;;      (COND ((ATOM TTY-NUMBER)
;;	     (MFORMAT NIL "USRVAR BUG in SEND. Please report this.
;;Mention MAXIMA-ERROR code: ~A~%Thank you." TTY-NUMBER)
;;	     100000.)
;;	    (T
;;	     (LET ((IDLE-TIME (SYSCALL 1 'TTYVAR
;;				       (f+ (CAR TTY-NUMBER) #O 400000)
;;				       (6BIT IDLTIM))))
;;		  (COND ((ATOM IDLE-TIME)
;;			 (MFORMAT NIL
;;			   "TTYVAR bug in SEND.  Please report this.
;;Mention MAXIMA-ERROR code:  ~A~%Thank you." IDLE-TIME)
;;			 100000.)
;;			(T (CAR IDLE-TIME)))))))))

;;) ;End of PROGN 'Compile for WHO on ITS.

;;#+Multics
;;(DEFMFUN $WHO ()
;;  (CLINE "who -long")
;;  '$DONE)

;;Turn sends into MAIL on foreign hosts.
;;#+(or Multics TOPS-20 LISPM)
;;(progn 'compile
;;#+Multics
;;(defmacro check-sendee-and-strip (sendee)
;;  `(cond ((eq (getcharn ,sendee 1) #\&)
;;	  (stripdollar ,sendee))
;;	 (t (merror "Send: 1st argument to SEND must be a string"))))
;;#-Multics
;;(defmacro check-sendee-and-strip (sendee)
;;  `(stripdollar ,sendee))
	 
;;(DEFMSPEC $SEND (X) (SETQ X (CDR X)) 

;;	    (COND ((NULL X)
;;		   (MDESCRIBE '$SEND))
;;;;O.K. we gotta get the documentation to agree with what we're doin' here.
;;		  ((= (LENGTH X) 1.)
;;		   (MAX-MAIL 'SEND X))
;;		  (T (LET ((NAME (check-sendee-and-strip (CAR X))))
;;		       (MAIL NAME (CDR X))
;;	      #-Multics(MFORMAT NIL "~&;MAIL'd to ~A~%" NAME))))
;;	    '$DONE)
;;)

(declare-top				;(SPLITFILE ISOLAT)
 (special *xvar $exptisolate $labels $dispflag errorsw)
 ;;	 (FIXNUM (GETLABCHARN))
 ) 

(defmvar $exptisolate nil)
(defmvar $isolate_wrt_times nil)

(defmfun $isolate (e *xvar) (setq *xvar (getopr *xvar)) (iso1 e)) 

(defun iso1 (e) 
  (cond ((specrepp e) (iso1 (specdisrep e)))
	((and (free e 'mplus) (or (null $isolate_wrt_times) (free e 'mtimes))) e)
	((freeof *xvar e) (mgen2 e))
	((alike1 *xvar e) *xvar)
	((memq (caar e) '(mplus mtimes)) (iso2 e))
	((eq (caar e) 'mexpt)
	 (cond ((null (atom (cadr e))) (list (car e) (iso1 (cadr e)) (caddr e)))
	       ((or (alike1 (cadr e) *xvar) (not $exptisolate)) e)
	       (t (let ((x ($rat (caddr e) *xvar)) (u 0) (h 0))
		    (setq u (ratdisrep ($ratnumer x)) x (ratdisrep ($ratdenom x)))
		    (if (not (equal x 1))
			(setq u ($multthru (list '(mexpt) x -1) u)))
		    (if (mplusp u)
			(setq u ($partition u *xvar) h (cadr u) u (caddr u)))
		    (setq u (power* (cadr e) (iso1 u)))
		    (cond ((not (equal h 0))
			   (mul2* (mgen2 (power* (cadr e) h)) u))
			  (t u))))))
	(t (cons (car e) (mapcar #'iso1 (cdr e))))))

(defun iso2 (e) 
  (prog (hasit doesnt op) 
     (setq op (ncons (caar e)))
     (do ((i (cdr e) (cdr i))) ((null i))
       (cond ((freeof *xvar (car i)) (setq doesnt (cons (car i) doesnt)))
	     (t (setq hasit (cons (iso1 (car i)) hasit)))))
     (cond ((null doesnt) (go ret))
	   ((and (null (cdr doesnt)) (atom (car doesnt))) (go ret))
	   ((prog2 (setq doesnt (simplify (cons op doesnt)))
		(and (free doesnt 'mplus)
		     (or (null $isolate_wrt_times)
			 (free doesnt 'mtimes)))))
	   (t (setq doesnt (mgen2 doesnt))))
     (setq doesnt (ncons doesnt))
     ret  (return (simplifya (cons op (nconc hasit doesnt)) nil)))) 

(defun mgen2 (h)
  (cond ((memsimilarl h (cdr $labels) (getlabcharn $linechar)))
	(t (setq h (displine h)) (and $dispflag (mterpri)) h))) 

(defun memsimilarl (item list linechar) 
  (cond ((null list) nil)
	((and (char= (getlabcharn (car list)) linechar)
	      (boundp (car list))
	      (memsimilar item (car list) (symbol-value (car list)))))
	(t (memsimilarl item (cdr list) linechar)))) 

(defun memsimilar (item1 item2 item2ev) 
  (cond ((equal item2ev 0) nil)
	((alike1 item1 item2ev) item2)
	(t (let ((errorsw t) r)
	     (setq r (catch 'errorsw (div item2ev item1)))
	     (and (mnump r) (not (zerop r)) (div item2 r))))))

(defmfun $pickapart (x lev)
  (setq x (format1 x))
  (cond ((not (fixnump lev))
	 (merror "Improper 2nd argument to `pickapart':~%~M" lev))
	((or (atom x) (and (eq (caar x) 'mminus) (atom (cadr x)))) x)
	((= lev 0) (mgen2 x))
	((and (atom (cdr x)) (cdr x)) x)
	(t (cons (car x) (mapcar #'(lambda (y) ($pickapart y (f1- lev))) (cdr x)))))) 

(defmfun $reveal (e lev) 
  (setq e (format1 e))
  (cond ((and (eq (ml-typep lev) 'fixnum) (> lev 0)) (reveal e 1 lev))
	(t (merror "Second argument to reveal must be positive integer."))))

(defun simple (x) (or (atom x) (memq (caar x) '(rat bigfloat)))) 

(defun reveal (e nn lev) 
  (cond ((simple e) e)
	((= nn lev)
	 (cond ((eq (caar e) 'mplus) (cons '(&sum simp) (ncons (length (cdr e)))))
	       ((eq (caar e) 'mtimes) (cons '(&product simp) (ncons (length (cdr e)))))
	       ((eq (caar e) 'mexpt) '&expt)
	       ((eq (caar e) 'mquotient) '&quotient)
	       ((eq (caar e) 'mminus) '&negterm)
	       (t (getop (mop e)))))
	(t (let ((u (cond ((memq 'simp (cdar e)) (car e))
			  (t (cons (caar e) (cons 'simp (cdar e))))))
		 (v (mapcar #'(lambda (x) (reveal (format1 x) (f1+ nn) lev))
			    (margs e))))
	     (cond ((eq (caar e) 'mqapply) (cons u (cons (cadr e) v)))
		   ((eq (caar e) 'mplus) (cons u (nreverse v)))
		   (t (cons u v)))))))

(declare-top				;(SPLITFILE PROPFN)
 (special atvars munbound $props $gradefs $features opers
	  $contexts $activecontexts $aliases)) 

(defmspec $properties (x)
  (nonsymchk (setq x (getopr (fexprcheck x))) '$properties)
  (let ((u (properties x)) (v (or (get x 'noun) (get x 'verb))))
    (if v (nconc u (cdr (properties v))) u)))

(defun properties (x)
  (do ((y (symbol-plist x) (cddr y))
       (l (cons '(mlist simp) (and (boundp x)
				   (if (optionp x) (ncons '|&SYSTEM VALUE|)
				       (ncons '$value)))))
       (prop))
      ((null y)
       
       (if (memq x (cdr $features)) (nconc l (ncons '$feature)))
       (if (memq x (cdr $contexts)) (nconc l (ncons '$context)))
       (if (memq x (cdr $activecontexts))
	   (nconc l (ncons '$activecontext)))
       (cond  ((null (symbol-plist x))
	       (if (fboundp x) (nconc l (list '|&SYSTEM FUNCTION|)))))
       l)
    ;; TOP-LEVEL PROPERTIES 
    (cond ((setq prop (assq (car y)
			    '((bindtest . $bindtest)
			      (sp2 . $deftaylor) (sp2subs . $deftaylor)
			      (assign . |&ASSIGN PROPERTY|)
			      (nonarray . $nonarray) (grad . $gradef)
			      (noun . $noun) (evfun . $evfun) (special . $special)
			      (evflag . $evflag) (op . $operator) (alphabet . $alphabetic))))
	   (nconc l (ncons (cdr prop))))
	  ((setq prop (memq (car y) opers)) (nconc l (list (car prop))))
	  ((and (eq (car y) 'operators) (not (eq (cadr y) 'simpargs1)))
	   (nconc l (list '$rule)))
	  ((and (memq (car y) '(fexpr fsubr mfexpr*s mfexpr*))
		(nconc l (ncons '|&SPECIAL EVALUATION FORM|))
		nil))
	  ((and (or (get (car y) 'mfexpr*) (fboundp x))
		(not (memq '|&SYSTEM FUNCTION| l)))
	   (nconc l
		  (list (cond ((get x 'translated) '$transfun)
			      ((mgetl x '($rule ruleof)) '$rule)
			      (t '|&SYSTEM FUNCTION|)))))
	  ((and (eq (car y) 'autoload) (not (memq '|&SYSTEM FUNCTION| l)))
	   (nconc l (ncons (if (memq x (cdr $props))
			       '|&USER AUTOLOAD FUNCTION|
			       '|&SYSTEM FUNCTION|))))
	  ((and (eq (car y) 'reversealias) (memq (car y) (cdr $aliases)))
	   (nconc l (ncons '$alias)))
	  ((eq (car y) 'data)
	   (nconc l (cons '|&DATABASE INFO| (cdr ($facts x)))))
	  ((eq (car y) 'mprops)
	   ;; PROPS PROPERTIES
	   (do ((y
		 (cdadr y)
		 (cddr y)))
	       ((null y))
	     (cond ((setq prop (assq (car y)
				     '((mexpr . $function)
				       (mmacro . $macro)
				       (hashar . |&HASHED ARRAY|)
				       (aexpr . |&ARRAY FUNCTION|)
				       (atvalues . $atvalue)
				       ($atomgrad . $atomgrad)
				       ($numer . $numer)
				       (depends . $dependency)
				       ($constant . $constant)
				       ($nonscalar . $nonscalar)
				       ($scalar . $scalar)
				       (matchdeclare . $matchdeclare)
				       (mode . $modedeclare))))
		    (nconc l (list (cdr prop))))
		   ((eq (car y) 'array)
		    (nconc l
			   (list (cond ((get x 'array) '|&COMPLETE ARRAY|)
				       (t '|&DECLARED ARRAY|)))))
		   ((and (eq (car y) '$props) (cdadr y))
		    (nconc l
			   (do ((y (cdadr y) (cddr y))
				(l (list '(mlist) '|&USER PROPERTIES|)))
			       ((null y) (list l))
			     (nconc l (list (car y))))))))))))


(defmspec $propvars (x)
  (setq x (fexprcheck x))
  (do ((iteml (cdr $props) (cdr iteml)) (propvars (ncons '(mlist))))
      ((null iteml) propvars)
    (and (among x (meval (list '($properties) (car iteml))))
	 (nconc propvars (ncons (car iteml))))))

(defmspec $printprops (r) (setq r (cdr r))
	  (if (null (cdr r)) (merror "`printprops' takes two arguments."))
	  (let ((s (cadr r)))
	    (setq r (car r))
	    (setq r (cond ((atom r)
			   (cond ((eq r '$all)
				  (cond ((eq s '$gradef) (mapcar 'caar (cdr $gradefs)))
					(t (cdr (meval (list '($propvars) s))))))
				 (t (ncons r))))
			  (t (cdr r))))
	    (cond ((eq s '$atvalue) (dispatvalues r))
		  ((eq s '$atomgrad) (dispatomgrads r))
		  ((eq s '$gradef) (dispgradefs r))
		  ((eq s '$matchdeclare) (dispmatchdeclares r))
		  (t (merror "Unknown `property' - `printprops':  ~:M" s)))))

(defun dispatvalues (l) 
  (do ((l
	l
	(cdr l)))
      ((null l))
    (do ((ll
	  (mget (car l) 'atvalues)
	  (cdr ll)))
	((null ll))
      (mtell-open
       "~M~%"
       (list '(mlable) nil 
	     (list '(mequal)
		   (atdecode (car l) (caar ll) (cadar ll))
		   (caddar ll)))
       )))
  '$done)

;;(declare-top (FIXNUM N))

(defun atdecode (fun dl vl) 
  (setq vl (copy-top-level vl))
  (atvarschk vl)
  ((lambda (eqs nvarl) (cond ((not (memq nil (mapcar #'(lambda (x) (signp e x)) dl)))
			      (do ((vl vl (cdr vl)) (varl atvars (cdr varl)))
				  ((null vl))
				(and (eq (car vl) munbound) (rplaca vl (car varl))))
			      (cons (list fun) vl))
			     (t (setq fun (cons (list fun)
						(do ((n (length vl) (f1- n))
						     (varl atvars (cdr varl))
						     (l nil (cons (car varl) l)))
						    ((zerop n) (nreverse l)))))
				(do ((vl vl (cdr vl)) (varl atvars (cdr varl)))
				    ((null vl))
				  (and (not (eq (car vl) munbound))
				       (setq eqs (cons (list '(mequal) (car varl) (car vl)) eqs))))
				(setq eqs (cons '(mlist) (nreverse eqs)))
				(do ((varl atvars (cdr varl)) (dl dl (cdr dl)))
				    ((null dl) (setq nvarl (nreverse nvarl)))
				  (and (not (zerop (car dl)))
				       (setq nvarl (cons (car dl) (cons (car varl) nvarl)))))
				(list '(%at) (cons '(%derivative) (cons fun nvarl)) eqs))))
   nil nil)) 

(defun dispatomgrads (l) 
  (do ((i
	l
	(cdr i)))
      ((null i))
    (do ((j
	  (mget (car i) '$atomgrad)
	  (cdr j)))
	((null j))
      (mtell-open "~M~%"
		  (list '(mlable)
			nil
			(list '(mequal)
			      (list '(%derivative)
				    (car i) (caar j) 1.)
			      (cdar j))))
      ))
  '$done) 

(defun dispgradefs (l) 
  (do ((i
	l
	(cdr i)))
      ((null i))
    (setq l (get (car i) 'grad))
    (do ((j (car l) (cdr j)) (k (cdr l) (cdr k)) (thing (cons (ncons (car i)) (car l))))
	((or (null k) (null j)))
      (mtell-open "~M~%"
		  (list '(mlable)
			nil
			(list '(mequal) (list '(%derivative) thing (car j) 1.) (car k))))
      ))
  '$done) 

(defun dispmatchdeclares (l) 
  (do ((i l (cdr i)) (ret))
      ((null i) (cons '(mlist) ret))
    (setq l (car (mget (car i) 'matchdeclare)))
    (setq ret (cons (append (cond ((atom l) (ncons (ncons l))) (t l))
			    (ncons (car i)))
		    ret))))

(declare-top				;(SPLITFILE CHANGV)
 (special trans ovar nvar tfun invfun $programmode nfun
	  *roots *failures varlist genvar $ratfac)
 ;;	 (*LEXPR $LIMIT $SOLVE SOLVABLE)
 )

(defmfun $changevar (expr trans nvar ovar) 
  (let (invfun nfun $ratfac)
    (cond ((or (atom expr) (eq (caar expr) 'rat) (eq (caar expr) 'mrat))  expr)
	  ((atom trans) (merror "2nd arg must not be atomic"))
	  ((null (atom nvar)) (merror "3rd arg must be atomic"))
	  ((null (atom ovar)) (merror "4th arg must be atomic")))
    (setq tfun (solvable (setq trans (meqhk trans)) ovar))
    (changevar expr)))

(defun solvable (l var &optional (errswitch nil))
  (let (*roots *failures)
    (solve l var 1)
    (cond (*roots ($rhs (car *roots)))
	  (errswitch
	   (merror "Unable to solve for ~M" var)
	   )
	  (t nil))))

(defun changevar (expr)
  (cond ((atom expr) expr)
	((or (not (memq (caar expr) '(%integrate %sum %product)))
	     (not (alike1 (caddr expr) ovar)))
	 (recur-apply #'changevar expr))
	(t (let ((deriv (if tfun (sdiff tfun nvar)
			    (neg (div (sdiff trans nvar) ;IMPLICIT DIFF.
				      (sdiff trans ovar))))))
	     (cond ((and (memq (caar expr) '(%sum %product))
			 (not (equal deriv 1)))
		    (merror "Illegal change in summation or product"))
		   ((setq nfun ($radcan	;NIL IF KERNSUBST FAILS
				(if tfun
				    (mul (maxima-substitute tfun ovar (cadr expr))
					 deriv)
				    (kernsubst ($ratsimp (mul (cadr expr)
							      deriv))
					       trans ovar)))) 
		    (cond ;; DEFINITE INTEGRAL,SUMMATION, OR PRODUCT
		      ((cdddr expr)
		       (or invfun (setq invfun (solvable trans nvar t)))
		       (list (ncons (caar expr)) ;THIS WAS CHANGED
			     nfun	;FROM '(%INTEGRATE)
			     nvar
			     ($limit invfun ovar (cadddr expr) '$plus)
			     ($limit invfun
				     ovar
				     (car (cddddr expr))
				     '$minus)))
		      (t		;INDEFINITE INTEGRAL
		       (list '(%integrate) nfun nvar))))
		   (t expr)))))) 

(defun kernsubst (expr form ovar)
  (let (varlist genvar nvarlist)
    (newvar expr)
    (setq nvarlist (mapcar #'(lambda (x) (if (freeof ovar x) x
					     (solvable form x)))
			   varlist))
    (if (memq nil nvarlist) nil
	(prog2 (setq expr (ratrep* expr)
		     varlist nvarlist)
	    (rdis (cdr expr))))))
	  
(declare-top				;(SPLITFILE FACSUM)
 (special $listconstvars facfun)) 

(defmfun $factorsum (e) (factorsum0 e '$factor)) 

(defmfun $gfactorsum (e) (factorsum0 e '$gfactor)) 

(defun factorsum0 (e facfun) 
  (cond ((mplusp (setq e (funcall facfun e)))
	 (factorsum1 (cdr e)))
	(t (factorsum2 e)))) 

(defun factorsum1 (e) 
  (prog (f lv llv lex cl lt c) 
   loop (setq f (car e))
   (setq lv (cdr ($showratvars f)))
   (cond ((null lv) (setq cl (cons f cl)) (go skip)))
   (do ((q llv (cdr q)) (r lex (cdr r)))
       ((null q))
     (cond ((intersect (car q) lv)
	    (rplaca q (union* (car q) lv))
	    (rplaca r (cons f (car r)))
	    (return (setq lv nil)))))
   (or lv (go skip))
   (setq llv (cons lv llv) lex (cons (ncons f) lex))
   skip (and (setq e (cdr e)) (go loop))
   (or cl (go skip2))
   (do ((q llv (cdr q)) (r lex (cdr r)))
       ((null q))
     (cond ((and (null (cdar q)) (cdar r))
	    (rplaca r (nconc cl (car r)))
	    (return (setq cl nil)))))
   skip2(setq llv nil lv nil)
   (do ((r 
	 lex
	 (cdr r)))
       ((null r))
     (cond ((cdar r)
	    (setq llv
		  (cons (factorsum2 (funcall facfun (cons '(mplus)
							  (car r))))
			llv)))
	   ((or (not (mtimesp (setq f (caar r))))
		(not (mnump (setq c (cadr f)))))
	    (setq llv (cons f llv)))
	   (t (do ((q lt (cdr q)) (s lv (cdr s)))
		  ((null q))
		(cond ((alike1 (car s) c)
		       (rplaca q (cons (dcon f) (car q)))
		       (return (setq f nil)))))
	      (and f
		   (setq lv (cons c lv) 
			 lt (cons (ncons (dcon f)) lt))))))
   (setq 
    lex
    (mapcar #'(lambda (s q) 
		(simptimes (list '(mtimes)
				 s
				 (cond ((cdr q)
					(cons '(mplus)
					      q))
				       (t (car q))))
			   1.
			   nil))
	    lv
	    lt))
   (return (simplus (cons '(mplus)
			  (nconc cl lex llv))
		    1.
		    nil)))) 

(defun dcon (mt) 
  (cond ((cdddr mt) (cons (car mt) (cddr mt))) (t (caddr mt)))) 

(defun factorsum2 (e) 
  (cond ((not (mtimesp e)) e)
	(t (cons '(mtimes)
		 (mapcar #'(lambda (f) 
			     (cond ((mplusp f)
				    (factorsum1 (cdr f)))
				   (t f)))
			 (cdr e)))))) 

(declare-top				;(SPLITFILE COMBF)
 (special $combineflag))

(defmvar $combineflag t)

(defmfun $combine (e) 
  (cond ((or (atom e) (eq (caar e) 'rat)) e)
	((eq (caar e) 'mplus) (combine (cdr e)))
	(t (recur-apply #'$combine e)))) 

(defun combine (e) 
  (prog (term r ld sw nnu d ln xl) 
   again(setq term (car e) e (cdr e))
   (when (or (not (or (ratnump term) (mtimesp term) (mexptp term)))
	     (equal (setq d ($denom term)) 1))
     (setq r (cons term r))
     (go end))
   (setq nnu ($num term))
   (and $combineflag (integerp d) (setq xl (cons term xl)) (go end))
   (do ((q ld (cdr q)) (p ln (cdr p)))
       ((null q))
     (cond ((alike1 (car q) d)
	    (rplaca p (cons nnu (car p)))
	    (return (setq sw t)))))
   (and sw (go skip))
   (setq ld (cons d ld) ln (cons (ncons nnu) ln))
   skip (setq sw nil)
   end  (and e (go again))
   (and xl (setq xl (cond ((cdr xl) ($xthru (addn xl t)))
			  (t (car xl)))))
   (mapc 
    #'(lambda (nu de) 
	(setq r (cons (mul2 (addn nu nil) (power* de -1)) r)))
    ln ld)
   (return (addn (if xl (cons xl r) r) nil))))

;;(declare-top (SPLITFILE FACOUT) (FIXNUM NUM))

(defmfun $factorout num
  (prog (e vl el fl cl l f x)
     (setq e (arg 1) vl (listify (f- 1 num)))
     (and (null vl)(merror "`factorout' called on only one argument"))
     (and (not (mplusp e)) (return e))
     (or (null vl) (mplusp e) (return e))
     (setq e (cdr e))
     loop	(setq f (car e) e (cdr e))
     (and (not (mtimesp f))(setq f (list '(mtimes) 1 f)))
     (setq fl nil cl nil)
     (do ((i (cdr f) (cdr i))) ((null i))
       (cond ((and (not (numberp (car i)))
		   (apply '$freeof (append vl (ncons (car i)))))
	      (setq fl (cons (car i) fl)))
	     (t (setq cl (cons (car i) cl)))))
     (and (null fl) (setq el (cons f el)) (go end))
     (setq fl (cond ((cdr fl) (simptimes (cons '(mtimes) fl) 1 nil))
		    (t (car fl))))
     (setq cl (cond ((null cl) 1)
		    ((cdr cl) (simptimes (cons '(mtimes) cl) 1 t))
		    (t (car cl))))
     (setq x t) (do ((i l (cdr i)))((null i))
		  (cond ((alike1 (caar i) fl) (rplacd (car i) (cons cl (cdar i))) (setq i nil x nil))))
     (and x (setq l (cons (list fl cl) l)))
     end	(and e (go loop))
     (do ((i l (cdr i))) ((null i))
       (setq el (cons (simptimes (list '(mtimes) (caar i)
				       ($factorsum (simplus (cons '(mplus) (cdar i)) 1 nil))) 1 nil) el)))
     (return (addn el nil))))

;;(declare-top (SPLITFILE SCREEN))
;; This splitfile contains primitives for manipulating the screen from MACSYMA
;; This stuff should just be stuck in STATUS.

;; $PAUSE(); does default --PAUSE--
;; $PAUSE("--FOO--") uses --FOO-- instead of --PAUSE
;; $PAUSE("--FOO--","--BAR--") is like above, but uses --BAR-- instead of
;;			       --CONTINUED--


(declare-top (special moremsg morecontinue))

(defmfun $pause (&optional (more-msg moremsg) (more-continue morecontinue))
  (let ((moremsg (stripdollar more-msg))
	(morecontinue (stripdollar more-continue)))
    ;;     (MORE-FUN NIL)
    '$done))

;; $CLEARSCREEN clears the screen.  It takes no arguments.

(defmfun $clearscreen () (cursorpos 'c) '$done)
