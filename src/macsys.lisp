;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ** (c) Copyright 1981 Massachusetts Institute of Technology **
;;;
;;; SYSTEM: The ``New'' Macsyma System Stuff
;;;
;;; *** NOTE *** this file uses common-lisp read syntax.

(in-package "MAXIMA")
(macsyma-module system)


(eval-when (eval compile load) (sstatus feature maxii))

;;; Standard Kinds of Input Prompts

(DEFUN MAIN-PROMPT ()
  ;; instead off using this STRIPDOLLAR hackery, the
  ;; MREAD function should call MFORMAT to print the prompt,
  ;; and take a format string and format arguments.
  ;; Even easier and more general is for MREAD to take
  ;; a FUNARG as the prompt. -gjc
  (FORMAT () "(~A~D) " (STRIPDOLLAR $INCHAR) $LINENUM))

(DEFUN BREAK-PROMPT ()
  (declare (special $prompt))
  (STRIPDOLLAR $PROMPT))



;; there is absoletely no need to catch errors here, because
;; they are caught by the macsyma-listener window process on
;; the lisp machine, or by setting the single toplevel process in Maclisp. -gjc

(defmacro toplevel-macsyma-eval (x) `(meval* ,x))

(defmvar $_ '$_ "last thing read in, cooresponds to lisp +")
;Also defined in JPG;SUPRV
#-CL (defmvar $% '$% "last thing printed out, cooresponds to lisp *")
(defmvar $__ '$__ "thing read in which will be evaluated, cooresponds to -")

(declare-top (special *mread-prompt*  $file_search_demo))

(defvar accumulated-time 0.0)
#-cl
(defun fixnum-char-upcase (x) (char-upcase x))
;#-ti
;(defun get-internal-real-time () (time:microsecond-time))
;#-ti
;(defun get-internal-run-time ()  (* 1000 (send current-process :cpu-time)) )
;(defvar internal-time-units-per-second  1000000)

#+lispm
(defun used-area ( &optional (area working-storage-area ))
  (multiple-value-bind (nil used)(si:room-get-area-length-used area)
    used))
 
(DEFUN CONTINUE (&OPTIONAL (*standard-input* *standard-input*)
			   BATCH-OR-DEMO-FLAG)
 (if (eql BATCH-OR-DEMO-FLAG :demo)
     (format t "~% At the _ prompt, type ';' followed by enter to get next demo"))
 (catch 'abort-demo
  (DO ((R) (time-before) (time-after) (time-used) (EOF (LIST NIL))
       (etime-before) (etime-after) #+lispm (area-before)#+lispm (area-after)
       (etime-used) (c-tag) (d-tag))
      (NIL)
    (when (not (checklabel $inchar))
	  (setq $linenum (f1+ $linenum)))
    #+akcl(si::reset-stack-limits)
    (setq c-tag (makelabel $inchar))
    (LET ((*MREAD-PROMPT* (if batch-or-demo-flag nil (MAIN-PROMPT)))
	  (eof-count 0))
    (tagbody
     top
     (SETQ R      (dbm-read *standard-input* nil eof))
     (cond ((and (eq r eof) (boundp '*socket-connection*)
		 (eq *standard-input* *socket-connection*))
	    (cond ((>=  (setq eof-count (+ 1 eof-count)) 10)
		   (print "exiting on eof")
		   ($quit))
		  (t (go top)))))
		 
     (cond ((and (consp r) (keywordp (car r)))
	    (break-call (car r) (cdr r) 'break-command)
	      (go top)))
	      
     )
    )
    

    (cond (#.writefilep ;write out the c line to the dribble file
	    (let ( (#.ttyoff t) smart-tty  $linedisp)
	      (displa `((mlable) , c-tag , $__)))))
    (IF (EQ R EOF) (RETURN '$DONE))
    (fresh-line *standard-output*)
    #+lispm (SEND *standard-output* :SEND-IF-HANDLES ':FORCE-OUTPUT)
    (SETQ $__ (CADDR R))
    (SET  C-TAG $__)
    (cond (batch-or-demo-flag
	   (displa `((mlable) ,c-tag , $__))))
    (setq time-before (get-internal-run-time)
	  etime-before (get-internal-real-time))
    #+lispm (setq area-before (used-area))
    (SETQ $% (TOPLEVEL-MACSYMA-EVAL $__))
    (setq etime-after (get-internal-real-time)
	  time-after (get-internal-run-time))
    #+lispm (setq area-after (used-area))
	(setq time-used (quotient (float (difference time-after time-before))
			      internal-time-units-per-second)
	  etime-used (quotient (float (difference etime-after etime-before))
			       internal-time-units-per-second))
    (setq accumulated-time (plus accumulated-time time-used))
    (SET (setq D-TAG (makelabel $outchar)) $%)
    (SETQ $_ $__)
    (when $showtime
	  #+NIL (format t "~&Evaluation took ~$ seconds (~$ elapsed)."
		    time-used etime-used)
	  #-(or NIL cl) (mtell "Evaluation took ~S seconds (~S elapsed)."
			   time-used etime-used)
	  (format t "~&Evaluation took ~$ seconds (~$ elapsed)"
       		    time-used etime-used )
	  #+lispm (format t "using ~A words." (f-  area-after area-before))
      )
    (UNLESS $NOLABELS
		     (PUTPROP d-tag
			      (cons time-used  0)
			      'TIME))
    (fresh-line *standard-output*)
    #+never(let ((tem (read-char-no-hang)))
      (or (eql tem #\newline) (and tem (unread-char tem))))
    (IF (EQ (CAAR R) 'DISPLAYINPUT)
	(DISPLA `((MLABLE) ,D-TAG ,$%)))
    (when (eq batch-or-demo-flag ':demo)
      (mtell "~&_")
      (let (quitting)	  
       (do ((char)) (nil)
	     ;;those are common lisp characters you'r reading here
	    (case
	     (setq char (read-char *terminal-io*))
	     ((#\page) (unless (cursorpos 'c *standard-input*) (terpri *standard-output*))
	      (princ "_" *standard-output*))
	     ((#\?) (mtell "  Pausing.  Type a ';' and Enter to continue demo.~%_"))
	     ((#\space #\; #\n #\e #\x #\t))
	     ((#\newline )
	      (if quitting (throw 'abort-demo nil) (return nil))) 
	     (t (setq quitting t)
		)))))
    ;; This is sort of a kludge -- eat newlines and blanks so that they don't echo
    (AND BATCH-OR-DEMO-FLAG
	 #+lispm
	 (send *standard-input* :operation-handled-p :read-char-no-echo)
	 #+lispm
	 (send *standard-input* :operation-handled-p :unread-char-no-echo)
	 (do ((char)) (())
	   (setq char (read-char *standard-input* nil #+cl nil)) 

;;;; INSERTED BY MASAMI 
           (when (null char) 
             (throw 'MACSYMA-QUIT NIL)) 
;;;; END INSERT 

	   (unless (zl-MEMBER char '(#\space #\newline #\return #\tab))
	       (unread-char char *standard-input*)  
	     (return nil))))))) 


(DEFUN $BREAK (&REST ARG-LIST)
  (PROG1 (apply #'$PRINT ARG-LIST)
	 (MBREAK-LOOP)))



(DEFUN MBREAK-LOOP ()
  (LET ((*standard-input* #+nil (make-synonym-stream '*terminal-io*)
			#-nil *debug-io*)
	(*standard-output* *debug-io*))
    (CATCH 'BREAK-EXIT
      (format t "~%Entering a Macsyma break point. Type EXIT; to resume")
      (DO ((R)) (NIL)
	(fresh-line)
	(SETQ R (CADDR (LET ((*MREAD-PROMPT* (BREAK-PROMPT)))
			 (MREAD *standard-input*))))
	(CASE R
	  (($EXIT) (THROW 'BREAK-EXIT T))
	  (T (ERRSET (DISPLA (MEVAL R)) T)))))))

(defun merrbreak (&optional arg)
  (format *debug-io* "~%Merrbreak:~A" arg)
  (mbreak-loop))

#-cl
(DEFUN RETRIEVE (MSG FLAG &AUX (PRINT? NIL))
  (DECLARE (SPECIAL MSG FLAG PRINT?))
  (OR (EQ FLAG 'NOPRINT) (SETQ PRINT? T))
  (MREAD-TERMINAL
    (CLOSURE '(MSG FLAG)
       #'(LAMBDA (STREAM CHAR) STREAM CHAR
	   (COND ((NOT PRINT?) (SETQ PRINT? T))
		 ((NULL MSG))
		 ((ATOM MSG) (PRINC MSG) (MTERPRI))
		 ((EQ FLAG T) (MAPC #'PRINC (CDR MSG)) (MTERPRI))
		 (T (DISPLA MSG) (MTERPRI)))))))
#+cl
(DEFUN RETRIEVE (MSG FLAG &AUX (PRINT? NIL))
  (DECLARE (SPECIAL MSG FLAG PRINT?))
  (OR (EQ FLAG 'NOPRINT) (SETQ PRINT? T))
  (COND ((NOT PRINT?) (SETQ PRINT? T))
	((NULL MSG))
	((ATOM MSG) (PRINC MSG) (MTERPRI))
	((EQ FLAG T) (MAPC #'PRINC (CDR MSG)) (MTERPRI))
	(T (DISPLA MSG) (MTERPRI)))
  (mread-noprompt *query-io* nil))


(DEFMFUN $READ (&REST L)
  (MEVAL (APPLY #'$READONLY L)))

(DEFMFUN $READONLY (&REST L)
  (let ((*mread-prompt*
	  (if l (string-right-trim '(#\n)
				   (with-output-to-string (*standard-output*)
				       (apply '$print l))) "")))
  (third (mread *query-io*))))

#-cl
(DEFUN MREAD-TERMINAL (PROMPT)
  (prog1 (let (#+NIL (si:*ttyscan-dispatch-table *macsyma-ttyscan-operators*))
	    (CADDR (send *terminal-io* ':RUBOUT-HANDLER
			 `((:PROMPT ,PROMPT) #+NIL (:reprompt ,prompt))
			 #'MREAD-RAW *terminal-io*)))
	 (fresh-line *terminal-io*)))



(DEFUN MAKE-INPUT-STREAM (X Y) Y ;ignore
  X)

(DEFUN BATCH (FILENAME &OPTIONAL DEMO-P
	      &AUX (orig filename )
	      list
	      FILE-OBJ (accumulated-time 0.0) (abortp t))
  (setq list (if demo-p '$file_search_demo '$file_search_maxima))
  (setq filename ($file_search filename (symbol-value list)))
  (or filename (merror "Could not find ~M in ~M: ~M"
		       orig list (symbol-value list)))
  
  (UNWIND-PROTECT
    (progn (batch-internal (setq file-obj (open filename)) demo-p)
	   (setq abortp nil)
	   (when $showtime
	     (format t "~&Batch spent ~$ seconds in evaluation.~%"
		     accumulated-time)))
    (IF FILE-OBJ (CLOSE FILE-OBJ))
    (when abortp (format t "~&(Batch of ~A aborted.)~%" filename))))


(defun batch-internal (fileobj demo-p)
  (CONTINUE (MAKE-ECHO-INPUT-STREAM
	      (MAKE-INPUT-STREAM fileobj "Batch Input Stream"))
	      (IF DEMO-P ':DEMO ':BATCH)))
#-cl
(DEFUN $BATCH (&REST ARG-LIST)
  (BATCH (FILENAME-FROM-ARG-LIST ARG-LIST) NIL))

(DEFUN FILENAME-FROM-ARG-LIST (ARG-LIST)
  (IF (= (LENGTH ARG-LIST) 1)
      ($FILENAME_MERGE (CAR ARG-LIST))
      ($FILENAME_MERGE `((MLIST),@ARG-LIST))))

(defmspec $grindef (form)
  (eval `(grindef ,@(cdr form)))
  '$DONE)
#+cl
(DEFUN $DEMO (&REST ARG-LIST)
  (let ((tem ($file_search (car arg-list) $file_search_demo)))
    (or tem (merror "Could not find ~M in  ~M: ~M" (car arg-list) '$file_search_demo $file_search_demo   ))
    ($BATCH tem	  '$demo)))

#-cl
(DEFUN $DEMO (&REST ARG-LIST)
  (BATCH (FILENAME-FROM-ARG-LIST ARG-LIST) T))

#-lispm
(defun macsyma-top-level ()
  (let ((*package* (find-package "MAXIMA")))
    (apply 'format t
	   "Maxima ~a.~a ~a (with enhancements by W. Schelter).~%Licensed under the GNU Public License (see file COPYING)~%"
	   (get :maxima :version))
    
   (catch 'quit-to-lisp
     (in-package "MAXIMA")
     (sloop 
	 do
       (catch #+kcl si::*quit-tag* #+cmu 'continue #-(or kcl cmu) nil
	      (catch 'macsyma-quit
		(continue)(bye)))))))

#-lispm
(progn 

#+kcl
(si::putprop :t 'throw-macsyma-top 'si::break-command)

(defun throw-macsyma-top ()
  (throw 'macsyma-quit t))


(defmfun $writefile (x) (dribble (subseq (string x) 1))) 
 
(defmfun $closefile () (dribble)) 
 

(defmfun $ed (x) (ed (subseq (string x) 1))) 
 
(defmfun $cli () (process ":CLI.PR")) 
 
(defun nsubstring (x y) (subseq x y)) 
 
(defun filestrip (x) (subseq (string (car x)) 1)) 
)

(defmspec $with_stdout ( arg) (setq arg (cdr arg))
 (let ((body (cdr arg)) res)
   (with-open-file (*standard-output* (NAMESTRING (stripdollar (car arg)))
				      :direction :output)
		   (dolist (v body)
			     (setq res (meval* v)))
		   res)))



(defun $sconcat(&rest x)
  (let ((ans "") )
  (dolist (v x)
	  (setq ans (concatenate 'string ans
				   
	  (cond ((and (symbolp v) (eql (getcharn v 1)
				       #\&))
		 (subseq (symbol-name v) 1))
		((stringp v) v)
		(t
		 (coerce (mstring v) 'string))))))
  ans))
					;

#+gcl
(defun $system (&rest x) (system (apply '$sconcat x)))



