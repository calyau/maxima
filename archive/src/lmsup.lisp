;; -*- Mode: Lisp; Package:MAXIMA; Syntax:COMMON-LISP; Base:10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;; (c) Copyright 1982 Massachusetts Institute of Technology

;; Lisp Machine additions to the Macsyma suprvisor -- see 
;; MAXII;SYSTEM > and JPG;SUPRV > for the remaining gory details.



(DEFFLAVOR MACSYMA-LISTENER ()
	   (TV:NOTIFICATION-MIXIN TV:PROCESS-MIXIN TV:FULL-SCREEN-HACK-MIXIN TV:WINDOW)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T		
		       #-symbolics :FONT-MAP #-symbolics '(FONTS:CPTFONT FONTS:CPTFONTB)
		       :border-margin-width 2
		       :label
		       (zl-string "Utexas Maxima Listener")
		       :PROCESS '(MACSYMA-LISTEN-LOOP :REGULAR-PDL-SIZE 40000
						      :SPECIAL-PDL-SIZE 5000))
  (:DOCUMENTATION :COMBINATION #. (zl-string "Normal MACSYMA window")))

(defmethod (macsyma-listener :after :init) (ignore)
  (send self :set-borders 1)
  (send (send self  :process)
	:set-warm-boot-action   'PROCESS-WARM-BOOT-DELAYED-RESTART-no-reset)
  (send self :set-label (zl-string ( #+cl global:format #-cl format
			  nil "Utexas Affine and ~A" (send self :name)))))

;;so that the process won't necessarily be reset if you have to start the machine.
(defvar *reset-processes* t)

(DEFUN PROCESS-WARM-BOOT-DELAYED-RESTART-no-reset (PROCESS)
  (PUSH (CONS PROCESS (si::PROCESS-RUN-REASONS PROCESS)) si::DELAYED-RESTART-PROCESSES)
  (SETF (si::PROCESS-RUN-REASONS PROCESS) NIL)
  (si::PROCESS-CONSIDER-RUNNABILITY PROCESS)
  (cond ((and (boundp *reset-processes*) *reset-processes*)
	 (si::PROCESS-RESET PROCESS))
	(t (send process :interrupt 'break 'break t))))



;;; This code tries to make Macsyma deal with Lispm more exceptions correctly.
;;; You should talk to me if you think it needs tweaking, please! --HIC



(DEFMETHOD (MACSYMA-LISTENER :MORE-EXCEPTION) ()
  (cond ((null tv:more-vpos)(let ((tv:more-processing-global-enable))
			      (more-fun-internal self)))
	(t  (TV:SHEET-MORE-HANDLER ':macsyma-more nil))))


;(DEFMETHOD (MACSYMA-LISTENER :MORE-EXCEPTION) ()
;   (TV:SHEET-MORE-HANDLER ':MACSYMA-MORE nil))

(DEFMETHOD (MACSYMA-LISTENER :MACSYMA-MORE) ()
  (MORE-FUN-INTERNAL SELF))

(DEFMETHOD (MACSYMA-LISTENER :DEFER-MORE) ()
  (OR (NULL TV:MORE-VPOS)			
      ( TV:MORE-VPOS 100000)
      (INCF TV:MORE-VPOS 100000)))

;; When changing the size or fonts of a window, update the variables DISPLA looks at.  It
;; would probably be better to make all of the DISPLA variables special instance
;; variables.  These would be placed inside a listener object, so that they would be
;; accessible from both the Macsyma listener window and the supdup server.

;; Check that PROCESS is really a process, since this gets called when the :INIT
;; method is run, which is before the process has been created.

(DEFmacro REBIND-SPECIALS (&rest specials)
  (setq specials (cons 'list (sloop for (v w) on specials by 'cddr
				   collecting (list 'quote v)
	collecting w)))
  `(LET ((PROCESS (SEND SELF ':PROCESS)))
    (IF
      #+cl (memq(type-of PROCESS) '(SI:PROCESS si:coroutining-process))
      #-cl (ml-typep PROCESS 'SI:PROCESS)
	(WITHOUT-INTERRUPTS
	  (SLOOP WITH STACK-GROUP = (SEND PROCESS ':STACK-GROUP)
		FOR (SPECIAL VALUE) ON ,SPECIALS BY 'CDDR DO
		;; Though this dual code shouldn't be necessary, on the 3600
		;; it seems that it is.  Anyway, it's probably the right thing
		;; to do.  --HIC
		(IF (EQ PROCESS CURRENT-PROCESS)
		    (SET SPECIAL VALUE)
		    (DBG:REBIND-IN-STACK-GROUP SPECIAL VALUE STACK-GROUP)))))))


(DEFmacro REBIND-SIZE-SPECIALS ()
  `(MULTIPLE-VALUE-BIND (X Y)
      (SEND SELF ':SIZE-IN-CHARACTERS)
    (REBIND-SPECIALS LINEL X TTYHEIGHT Y)))

;(rebind-size-specials)

(DEFmacro REBIND-FONT-SPECIALS ()
 `(progn (REBIND-SIZE-SPECIALS)
  (REBIND-SPECIALS LG-CHARACTER-X TV:CHAR-WIDTH
		   LG-CHARACTER-Y TV:LINE-HEIGHT
		   LG-CHARACTER-X-2 (// TV:CHAR-WIDTH 2)
		   LG-CHARACTER-Y-2 (// TV:LINE-HEIGHT 2))
  (LET ((BLINKER (TV:SHEET-FOLLOWING-BLINKER SELF)))
    (AND BLINKER
	 (SEND BLINKER ':SET-SIZE (tv:FONT-BLINKER-WIDTH TV:CURRENT-FONT)
				  (tv:FONT-BLINKER-HEIGHT TV:CURRENT-FONT))))))

  
(DEFMETHOD (MACSYMA-LISTENER :AFTER :CHANGE-OF-SIZE-OR-MARGINS)(&rest ignore) (REBIND-SIZE-SPECIALS))
(DEFMETHOD (MACSYMA-LISTENER :AFTER :SET-CURRENT-FONT)(&rest ignore) (REBIND-FONT-SPECIALS))
(DEFMETHOD (MACSYMA-LISTENER :AFTER :SET-FONT-MAP) (&rest ignore)(REBIND-FONT-SPECIALS))




;(DEFUN-METHOD REBIND-SPECIALS MACSYMA-LISTENER (&REST SPECIALS)
;  (LET ((PROCESS (SEND SELF ':PROCESS)))
;    (IF (ml-typep PROCESS 'SI:PROCESS)
;	(WITHOUT-INTERRUPTS
;	  (SLOOP WITH STACK-GROUP = (SEND PROCESS ':STACK-GROUP)
;		FOR (SPECIAL VALUE) ON SPECIALS BY 'CDDR DO
;		;; Though this dual code shouldn't be necessary, on the 3600
;		;; it seems that it is.  Anyway, it's probably the right thing
;		;; to do.  --HIC
;		(IF (EQ PROCESS CURRENT-PROCESS)
;		    (SET SPECIAL VALUE)
;		    (DBG:REBIND-IN-STACK-GROUP SPECIAL VALUE STACK-GROUP)))))))


;(DEFUN-METHOD REBIND-SIZE-SPECIALS MACSYMA-LISTENER (&REST IGNORE)
;  (MULTIPLE-VALUE-BIND (X Y)
;      (SEND SELF ':SIZE-IN-CHARACTERS)
;    (REBIND-SPECIALS 'LINEL X 'TTYHEIGHT Y)))

;(DEFUN-METHOD REBIND-FONT-SPECIALS MACSYMA-LISTENER (&REST IGNORE)
;  (REBIND-SIZE-SPECIALS)
;  (REBIND-SPECIALS 'LG-CHARACTER-X TV:CHAR-WIDTH
;		   'LG-CHARACTER-Y TV:LINE-HEIGHT
;		   'LG-CHARACTER-X-2 (// TV:CHAR-WIDTH 2)
;		   'LG-CHARACTER-Y-2 (// TV:LINE-HEIGHT 2))
;  (LET ((BLINKER (TV:SHEET-FOLLOWING-BLINKER SELF)))
;    (AND BLINKER
;	 (SEND BLINKER ':SET-SIZE (FONT-BLINKER-WIDTH TV:CURRENT-FONT)
;				  (FONT-BLINKER-HEIGHT TV:CURRENT-FONT)))))
;
;(DEFMETHOD (MACSYMA-LISTENER :AFTER :CHANGE-OF-SIZE-OR-MARGINS) REBIND-SIZE-SPECIALS)
;(DEFMETHOD (MACSYMA-LISTENER :AFTER :SET-CURRENT-FONT) REBIND-FONT-SPECIALS)
;(DEFMETHOD (MACSYMA-LISTENER :AFTER :SET-FONT-MAP) REBIND-FONT-SPECIALS)

(COMPILE-FLAVOR-METHODS MACSYMA-LISTENER)

;; The top level function for Macsyma windows.  The :KILL operation resets
;; the process and buries the window.  MACSYMA-TOP-LEVEL exits when the user
;; types QUIT();.  Bind TERMINAL-IO here rather than in MACSYMA-TOP-LEVEL since
;; it is already bound in the supdup server.

(DEFUN MACSYMA-LISTEN-LOOP (*terminal-io*)
  (SLOOP DO
	(MACSYMA-TOP-LEVEL)
	(SEND *terminal-io* ':BURY)))

;; Typing (MACSYMA) causes the MACSYMA-TOP-WINDOW to be selected if typed from the Lisp
;; Machine keyboard (TERMINAL-IO is a window stream).  If typed from some other stream,
;; just enter the normal read-eval-print loop.  MACSYMA-TOP-WINDOW is analgous to
;; TV:INITIAL-LISP-LISTENER.

(DEFVAR MACSYMA-TOP-WINDOW (TV:MAKE-WINDOW 'MACSYMA-LISTENER))

(DEFUN FIND-MACSYMA-TOP-WINDOW ()
  (IF (NULL MACSYMA-TOP-WINDOW)
      (SETQ MACSYMA-TOP-WINDOW (TV:MAKE-WINDOW 'MACSYMA-LISTENER))
      MACSYMA-TOP-WINDOW))

(DEFUN MACSYMA ()
  (IF (typep *terminal-io* 'tv::sheet)
      (SEND (FIND-MACSYMA-TOP-WINDOW) ':SELECT)
      (MACSYMA-TOP-LEVEL)))

;; SMART-TTY and LINE-GRAHPICS-TTY are used by MRG;DISPLA and are set up on ITS in
;; ALJABR;LOADER.  RUBOUT-TTY is used by SUPRV and can be flushed when we start using the
;; Lisp Machine editor.  SCROLLP and SMART-TTY are equivalent for our purposes.

(declare-top (SPECIAL SMART-TTY RUBOUT-TTY LINE-GRAPHICS-TTY CHARACTER-GRAPHICS-TTY
		  LINEL TTYHEIGHT SCROLLP LG-OLD-X LG-OLD-Y
		  LG-CHARACTER-X LG-CHARACTER-Y LG-CHARACTER-X-2 LG-CHARACTER-Y-2))

(DEFUN MACSYMA-TOP-LEVEL ()
  (LET* ((*standard-output* #'MACSYMA-OUTPUT )
	 (^R NIL) (^W NIL)
	 #-cl
	 (PACKAGE (#-cl PKG-FIND-PACKAGE #+cl find-package "maxima"))
 	 #+cl
	 (*package* (find-package 'cl-maxima))
	 #-cl
	 (global:readtable (find-lisp-readtable-for-macsyma))
	 #+cl
	 (*readtable*  (find-lisp-readtable-for-macsyma))
	 (*print-base* 10.) (*read-base* 10.) ;(*NOPOINT T)
	 #+ti (si:*use-old-break* t)
	 (W-O (FUNCALL *terminal-io* ':WHICH-OPERATIONS))
	 ;; Bind for multiple instantiations -- these variables
	 ;; are stream-dependent.
	 (SMART-TTY (MEMQ ':SET-CURSORPOS W-O))
	 (RUBOUT-TTY SMART-TTY)
	 (SCROLLP (NOT SMART-TTY))
	 (LINE-GRAPHICS-TTY (MEMQ ':DRAW-LINE W-O))
	 ;; Bind for multiple instantiations -- these variables are stream-dependent.
	 (SMART-TTY (SEND *terminal-io* ':OPERATION-HANDLED-P ':SET-CURSORPOS))
	 (RUBOUT-TTY SMART-TTY)
	 (SCROLLP (NOT SMART-TTY))
	 (LINE-GRAPHICS-TTY (SEND *terminal-io* ':OPERATION-HANDLED-P ':DRAW-LINE))
	 (LINEL) (TTYHEIGHT) (LG-OLD-X) (LG-OLD-Y)
	 #+ (and cl symbolics)
	 (si:*interactive-bindings* `((*package* ,(find-package 'cl-maxima))
				      (global:package ,(find-package 'cl-maxima))
				      (*readtable* ,(find-lisp-readtable-for-macsyma))
				      (global:readtable ,(find-lisp-readtable-for-macsyma))
				      ))
	 (LG-CHARACTER-X) (LG-CHARACTER-Y) (LG-CHARACTER-X-2) (LG-CHARACTER-Y-2))
    ;; Uncomment this when somebody tries to take car of a number again
    ;; (SET-ERROR-MODE 1 1 1 1)
    ;; What happens to height on printing ttys?
    (MULTIPLE-VALUE-SETQ (LINEL TTYHEIGHT) (SEND *terminal-io* ':SIZE-IN-CHARACTERS))
    (COND (LINE-GRAPHICS-TTY
	   (SETQ LG-CHARACTER-X (SEND *terminal-io* ':CHAR-WIDTH))
	   (SETQ LG-CHARACTER-Y (SEND *terminal-io* ':LINE-HEIGHT))
	   (SETQ LG-CHARACTER-X-2 (// LG-CHARACTER-X 2))
	   (SETQ LG-CHARACTER-Y-2 (// LG-CHARACTER-Y 2))))
    (PRINT-MACSYMA-COPYRIGHT *terminal-io*)
    (apply 'format t
	   "Maxima ~a.~a ~a (with enhancements by W. Schelter).~%Licensed under the GNU Public License (see file COPYING)~%"
	   (get :maxima :version))
    (CATCH 'MACSYMA-QUIT
      (ERROR-RESTART-LOOP ((SYS:ABORT) "Macsyma Top Level~@[ in ~A~]"
			   (SEND *terminal-io* ':SEND-IF-HANDLES ':NAME))
	(UNWIND-PROTECT
	  (CONDITION-CASE ()
	     #-symbolics (SEND *terminal-io* ':FUNCALL-INSIDE-YOURSELF #'CONTINUE)
	     #+symbolics
	     (continue)
	    (MACSYMA-ERROR))
	  (SUPUNBIND))))))

;; Add "Macsyma" to the window creation menu and System key.
(PUSH '("Macsyma" :VALUE MACSYMA-LISTENER
	:DOCUMENTATION "Macsyma Symbolic Algebra System")
      TV:DEFAULT-WINDOW-TYPES-ITEM-LIST)

(cond ((boundp 'tv:*system-keys*)
       (cond ((numberp (caar tv:*system-keys*))
	      (PUSH '(#. (char-code #\A) MACSYMA-LISTENER "Macsyma" T) TV:*system-KEYS*))
	     (t
	      (PUSH '(#\A MACSYMA-LISTENER "Macsyma" T) TV:*system-KEYS*))))
      (t #+symbolics       (cond ((numberp (caar tv:*select-keys*))
		      (PUSH '(#. (char-code #\A) MACSYMA-LISTENER "Macsyma" T) TV:*select-KEYS*))
		     (t
		      (PUSH '(#\A MACSYMA-LISTENER "Macsyma" T) TV:*select-KEYS*)))))

(tv:add-to-system-menu-programs-column
  (zl-string "Macsyma")
  '(tv:select-or-create-window-of-flavor 'macsyma-listener) 
  (zl-string "Macsyma Symbolic Algebra System, with UT modifications") t)




;; Print out the Macsyma Copyright notice to the appropriate window

;(DEFUN PRINT-MACSYMA-COPYRIGHT (WINDOW &AUX TEMP)
;  (MULTIPLE-VALUE-BIND (MAJOR MINOR) (SI:GET-SYSTEM-VERSION 'cl-MAximA)
;						; '(FONTS:CPTFONT FONTS:CPTFONTB)
;    (send window :set-font-map   '(cptfont metsi tiny))
;    (send window :set-current-font 1)
;    (send window :string-out-centered 
;	  (FORMAT nil "~%MAXIMA ~D.~D" MAJOR MINOR))
;;	  (FORMAT nil "~%          MAXIMA ~D.~D" MAJOR MINOR))
;;  (FORMAT WINDOW   "~%      with enhancements by")
;;  (format window   "~%  UNIVERSITY OF TEXAS 1984,1985")
;;    (SEND WINDOW :SET-CURRENT-FONT 0)
;;    (SETQ TEMP (SEND WINDOW :FONT-MAP))
;;    (ASET FONTS:TINY TEMP 2)
;;    (SEND WINDOW :SET-FONT-MAP TEMP)
;    (SEND WINDOW :SET-CURRENT-FONT 2)
;    (send window :string-out-centered 
;	  (format nil "~%          ENHANCEMENTS BY WILLIAM SCHELTER 1984,1985"))
;    (send window :string-out-centered 
;	  (FORMAT nil "~%       COPYRIGHT 1976, 1983 MASSACHUSETTS INSTITUTE OF TECHNOLOGY~2%")	))
;    (SEND WINDOW :SET-CURRENT-FONT 0))

;; Print out the Macsyma Copyright notice to the appropriate window

(DEFUN PRINT-MACSYMA-COPYRIGHT (WINDOW &AUX TEMP)
  (MULTIPLE-VALUE-BIND (MAJOR MINOR) (SI:GET-SYSTEM-VERSION 'cl-MAximA)
    (setq major 4 minor 0)
    (send window :set-current-font 1)
    (FORMAT WINDOW "~%          MAXIMA ~D.~D" MAJOR MINOR))
;  (FORMAT WINDOW   "~%      with enhancements by")
;  (format window   "~%  UNIVERSITY OF TEXAS 1984,1985")
  #-symbolics
 (progn (SEND WINDOW :SET-CURRENT-FONT 0)
  (SETQ TEMP (SEND WINDOW :FONT-MAP))
  (ASET FONTS:TINY TEMP 2)
  (SEND WINDOW :SET-FONT-MAP TEMP)
  (SEND WINDOW :SET-CURRENT-FONT 2))
  (format window "~%          ENHANCEMENTS BY WILLIAM SCHELTER 1984,1985,1987")
  (FORMAT WINDOW "~%       COPYRIGHT 1976, 1983 MASSACHUSETTS INSTITUTE OF TECHNOLOGY~2%")
  #-symbolics
  (SEND WINDOW :SET-CURRENT-FONT 0))


;; Random garbage needed to make SUPRV happy.

(DEFUN FORMFEED () (SEND *standard-input* ':CLEAR-SCREEN))

;; This is used someplace in SUPRV.

(DEFUN FILE-OPEN (FILE-OBJ)
  (EQ (SEND FILE-OBJ ':STATUS) ':OPEN))

;; Takes a string file specification and returns an oldio list specification.  Similar
;; to MacLisp NAMELIST function. (UNEXPAND-PATHNAME "C: D; A B") --> (A B C D)

(DEFUN UNEXPAND-PATHNAME (SSTRING)
  (LET* ((PATHNAME (PATHNAME SSTRING))
	 (DEV (SEND PATHNAME ':DEVICE)))
    (IF (STRING-EQUAL DEV "DSK")
	(SETQ DEV (SEND PATHNAME ':HOST)))
    (MAPCAR 'INTERN (LIST (SEND PATHNAME ':FN1)
			  (SEND PATHNAME ':FN2)
			  (SEND DEV ':NAME-AS-FILE-COMPUTER)
			  (SEND PATHNAME ':DIRECTORY)))))

;;; Make this function callable on different types of things.
(DEFUN NAMESTRING (OLDIO-LIST)
  (cond ((symbolp oldio-list) (string oldio-list))
	((stringp oldio-list) oldio-list)
	(t (INTERN
	     (FORMAT NIL "~A/:~A/;~A ~A"
		     (THIRD OLDIO-LIST)
		     (FOURTH OLDIO-LIST)
		     (FIRST OLDIO-LIST)
		     (SECOND OLDIO-LIST))))))

;; Takes a list like in ITS Macsyma, returns a string.
;; Device defaults to MC, as does DSK device specification.
;; Directory defaults to user-id.  Hack USERSn later.
;; (FILESTRIP '($A $B $C $D)) --> "C: D; A B"
;; (FILESTRIP '($A $B $C)) --> "MC: C; A B"

(DEFUN FILESTRIP (X &AUX FN1 FN2 DEV DIR)
  (IF (AND (= (LENGTH X) 1) (SYMBOLP (CAR X)) (char= (AREF (STRING (CAR X)) 0) #\&))
      ;; A Macsyma string, use it as is
      (SUBSTRING (STRING (CAR X)) 1)
      ;; Otherwise...
      (SETQ X (FULLSTRIP X)) ;Strip the list, leave NIL as NIL.
      (SETQ FN1 (CAR X) FN2 (CADR X) DEV (CADDR X) DIR (CADDDR X))
      (IF (AND DEV (NULL DIR)) (SETQ DIR DEV DEV NIL))
      (IF (EQ DEV 'DSK) (SETQ DEV "MC"))
      ;;If case doesn't matter, don't confuse user.
      (STRING-UPCASE (FORMAT NIL "~A: ~A; ~A ~A"
			      (OR DEV "MC") (OR DIR global:USER-ID)
			      (OR FN1 "MAXOUT") (OR FN2 ">")))))

(DEFUN MAXIMA-FIND (FUNC MEXPRP)
  MEXPRP
  (COND ((safe-GET FUNC 'AUTOLOAD)
	 (ERROR  "~A is needed from file ~S, but not in core"
		 FUNC (GET FUNC 'AUTOLOAD))))
  NIL)

;(DECLARE (SPECIAL ERROR-CALL))
;(DEFUN MACSYMA-ERROR-HANDLER (&REST IGNORE)
;   (COND ((NULL ERROR-CALL) NIL)
;	 (T (LET ((SIGNAL))
;		 (SETQ SIGNAL (FUNCALL ERROR-CALL NIL))
;		 (COND ((NULL SIGNAL) NIL)
;		       ((EQ SIGNAL 'LISP)
;			(SETQ EH:ERRSET-STATUS NIL
;			      EH:ERRSET-PRINT-MSG T)
;			NIL)
;		       ((EQ SIGNAL 'EXIT) (THROW 'SI:TOP-LEVEL NIL))
;		       (T NIL))))))

(DEFUN TOP-MEVAL (FORM)
  (CATCH-ERROR-RESTART (SYS:ABORT #-ti "TOP-MEVAL")
    (NCONS (MEVAL* FORM))))

(declare-top (SPECIAL WRITEFILE-OUTPUT))
(declare-top (SPECIAL WRITEFILE-OPERATIONS))
(DEFVAR ^R NIL)
(DEFVAR ^W NIL)
(DEFVAR INFILE NIL)

;;; *STANDARD-OUTPUT* gets bound to this.
(DEFUN MACSYMA-OUTPUT (OP &REST REST)
  (CASE OP
    (:WHICH-OPERATIONS (SEND *terminal-io* ':WHICH-OPERATIONS))
    (T (IF (AND ^R (MEMQ OP WRITEFILE-OPERATIONS))
	   (APPLY WRITEFILE-OUTPUT OP REST))
       (IF (NOT ^W) (APPLY *terminal-io* OP REST)))))

;; Specify entire filename when WRITEFILE is done.
(DEFMFUN $WRITEFILE (&REST L)
  (LET ((NAME ($FILENAME_MERGE (FILENAME-FROM-ARG-LIST L)
			       "MAXOUT"
			       (FS:USER-HOMEDIR))))
  (SETQ WRITEFILE-OUTPUT #-cl (OPEN NAME  ':OUT)
	#+cl (open name :direction :output))
  (SETQ WRITEFILE-OPERATIONS (SEND WRITEFILE-OUTPUT ':WHICH-OPERATIONS))
  (SETQ ^R T)
  NAME))

(DEFMFUN $APPENDFILE (&REST L)
  (LET ((NAME ($FILENAME_MERGE (FILENAME-FROM-ARG-LIST L)
			       "MAXOUT"
			       (FS:USER-HOMEDIR))))
  (SETQ WRITEFILE-OUTPUT #-cl (OPEN NAME  ':OUT)
	#+cl (open name :direction :output :if-exists :append
		   :if-does-not-exist :create
		   ))
  (SETQ WRITEFILE-OPERATIONS (SEND WRITEFILE-OUTPUT ':WHICH-OPERATIONS))
  (SETQ ^R T)
  NAME))

(DEFUN $CLOSEFILE ()
  (SETQ ^R NIL)
  (CLOSE WRITEFILE-OUTPUT)
  '$DONE)

;; Random useful functions to call from Macsyma Toplevel.

(DEFF $ED #'ED)
(DEFF $BEEP #'TV:BEEP)

#-3600
(DEFF $GC_ON #'GC-ON)
#-3600
(DEFF $GC_OFF #'GC-OFF)

(DEFUN $SCHEDULE (&OPTIONAL (N 1)) (DOTIMES (I N) (PROCESS-ALLOW-SCHEDULE)))
(DEFUN $SLEEP (&OPTIONAL (60THS-SEC 60.)) (PROCESS-SLEEP 60THS-SEC))

(DEFMVAR EDITOR-STATE NIL "Alist of editor windows and old strings, one per Macsyma Listener")

;;; Edit a frob, then read it back in
(DEFMSPEC $EDIT (X) 
  (SETQ X (SECOND X))
  ;; Convert Macsyma expression into a string
  (AND X
       (LET (($GRIND T))
	 (declare (special $grind))
	 (SETQ X (MAPPLY '$STRING (LIST X) '$STRING))))
  (LET ((STATE (ASSQ *terminal-io* EDITOR-STATE))
	(WINDOW))
    (COND ((NULL STATE)
	   (SETQ STATE (LIST *terminal-io* NIL ""))
	   (PUSH STATE EDITOR-STATE)))
    (LET ((TV:DEFAULT-SCREEN TV:MOUSE-SHEET))
      (MULTIPLE-VALUE-SETQ (X WINDOW) (ZWEI:EDSTRING (IF X (NSUBSTRING (STRING X) 1) (THIRD STATE))
						(SECOND STATE))))
    (SETF (SECOND STATE) WINDOW)
    (SETF (THIRD STATE) X)
    (WITH-INPUT-FROM-STRING (STREAM (STRING-APPEND X ";"))
      (MEVAL* (THIRD (MREAD STREAM))))))


(DEFUN $pbi_pop_up (from-line  &aux input-symbol
			       (*print-base* 10.)(*read-base* 10.) *print-radix*
			       ;(*nopoint t)
			       )
  (cond ((null from-line)(setq from-line (max 1 (f- $linenum 40)))))
  (USING-RESOURCE (WINDOW tv:POP-UP-FINGER-WINDOW)
    (SETF (tv:SHEET-TRUNCATE-LINE-OUT-FLAG WINDOW) 1)
    (FUNCALL WINDOW ':SET-LABEL (zl-string "Macsyma Input Playback"))
    #+ti
    (funcall window :set-process current-process)
    (tv:window-call (window :deactivate)
      (let ((*standard-input* (send window :io-buffer))(stream window))
	   (setq #-ti tv:kbd-esc-time #+ti tv:kbd-terminal-time nil)
	   (format window "Playback of Macsyma input lines:  ")
	   (let ((linel 
		   (f- (quotient (send window :width)
				(send window :char-width)) 10.)))
		(sloop for i from from-line below $linenum
		      do
		      (setq input-symbol (intern (format nil "$C~A" i) 'maxima ))
		      (cond ( (boundp input-symbol) 
			     (format stream "~% C~3A: ~:M"   i (symbol-value  input-symbol)  ))))
		#+ti (tv:await-user-typeahead window)
		#-ti(tv:type-a-space-to-flush window)
		)))))


(eval-when (load)
(pushnew  (list (zl-char #\E) '$pbi_pop_up
	       #. (zl-string "Playback Maxima Input lines starting 40 lines back (or at numeric arg)"))
	 #+ti tv::*terminal-keys* #-ti tv::*function-keys* :test 'equalp)
)

;; To do:
;; Figure out some way of making $LINENUM and $% process-specific.
;; JLK suggests something like D4.23 as meaning D-line 23 in Macsyma Listener #4.
;; Make Macsyma Windows into scroll windows.

