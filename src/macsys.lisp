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

#+cmu
(defun used-area (&optional unused)
  (declare (ignore unused))
  (ext:get-bytes-consed))

#+sbcl
(defun used-area (&optional unused)
  (declare (ignore unused))
  (sb-ext:get-bytes-consed))

#+openmcl
(defun used-area (&optional unused)
  (declare (ignore unused))
  (ccl::total-bytes-allocated))

#+clisp
(defun used-area (&optional unused)
  (declare (ignore unused))
  (multiple-value-bind (real1 real2 run1 run2 gc1 gc2 space1 space2 gccount)
      (sys::%%time)
    (declare (ignore real1 real2 run1 run2 gc1 gc2 gccount))
    (dpb space1 (byte 24 24) space2)))

#-(or lispm cmu sbcl clisp)
(defun used-area (&optional unused)
  (declare (ignore unused))
  0)

(DEFUN CONTINUE (&OPTIONAL (input-stream *standard-input*)
			   BATCH-OR-DEMO-FLAG)
 (if (eql BATCH-OR-DEMO-FLAG :demo)
     (format t "~% At the _ prompt, type ';' followed by enter to get next demo"))
 (catch 'abort-demo
  (DO ((R)
       (time-before)
       (time-after)
       (time-used)
       (EOF (LIST NIL))
       (etime-before)
       (etime-after)
       (area-before)
       (area-after)
       (etime-used)
       (c-tag)
       (d-tag))
      (NIL)
    (when (not (checklabel $inchar))
	  (setq $linenum (f1+ $linenum)))
    #+akcl(si::reset-stack-limits)
    (setq c-tag (makelabel $inchar))
    (LET ((*MREAD-PROMPT* (if batch-or-demo-flag nil (MAIN-PROMPT)))
	  (eof-count 0))
    (tagbody
     top
     (SETQ R      (dbm-read input-stream nil eof))
     ; This is something of a hack. If we are running in a server mode
     ; (which we determine by checking *socket-connection*) and we get
     ; an eof on an input-stream that is not *standard-input*, switch
     ; the input stream to *standard-input*.
     ; There should probably be a better scheme for server mode.
     ; jfa 10/09/2002.
     (if (and
	  (eq r eof)
	   (not (eq input-stream *standard-input*))
	   (boundp '*socket-connection*))
	 (progn
	       (setq input-stream *standard-input*)
	       (setq *mread-prompt* nil)
	       (setq r (dbm-read input-stream nil eof))))

     (cond ((and (eq r eof) (boundp '*socket-connection*)
		 (eq input-stream *socket-connection*))
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
    (setq area-before (used-area))
    (SETQ $% (TOPLEVEL-MACSYMA-EVAL $__))
    (setq etime-after (get-internal-real-time)
	  time-after (get-internal-run-time))
    (setq area-after (used-area))
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
	  #+(or cmu sbcl clisp)
	  (let ((total-bytes (- area-after area-before)))
	    (cond ((> total-bytes 1024)
		   (format t " using ~,3F KB." (/ total-bytes 1024.0))
		   )
		  ((> total-bytes (* 1024 1024))
		   (format t " using ~,3F MB." (/ total-bytes (* 1024.0 1024.0)))
		   )
		  (t
		   (format t " using ~:D bytes." total-bytes))))

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
	     ((#\page) (unless (cursorpos 'c input-stream) (terpri *standard-output*))
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
	 (send input-stream :operation-handled-p :read-char-no-echo)
	 #+lispm
	 (send input-stream :operation-handled-p :unread-char-no-echo)
	 (do ((char)) (())
	   (setq char (read-char input-stream nil #+cl nil)) 

;;;; INSERTED BY MASAMI 
           (when (null char) 
             (throw 'MACSYMA-QUIT NIL)) 
;;;; END INSERT 

	   (unless (zl-MEMBER char '(#\space #\newline #\return #\tab))
	       (unread-char char input-stream)  
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

(defmfun $bug_report ()
  (format t "~%The Maxima bug database is available at~%")
  (format t "    http://sourceforge.net/tracker/?atid=104933&group_id=4933&func=browse~%")
  (format t "Submit bug reports by following the 'Submit New' link on that page.~%")
  (format t "Please include the following build information with your bug report:~%")
  (format t "-------------------------------------------------------------~%")
  ($build_info)
  (format t "-------------------------------------------------------------~%")
  (format t "The above information is also available from the Maxima function build_info().~%~%")
  "")

(defmfun $build_info ()
  (format t "~%Maxima version: ~a~%" *autoconf-version*)
  (format t "Maxima build date: ~a:~a ~a/~a/~a~%"
	  (third user:*maxima-build-time*)
	  (second user:*maxima-build-time*)
	  (fifth user:*maxima-build-time*)
	  (fourth user:*maxima-build-time*)
	  (sixth user:*maxima-build-time*))
  (format t "host type: ~a~%" *autoconf-host*)
  (format t "lisp-implementation-type: ~a~%" (lisp-implementation-type))
  (format t "lisp-implementation-version: ~a~%~%" (lisp-implementation-version))
  "")

(defvar *maxima-started* nil)

#-lispm
(defun macsyma-top-level (&OPTIONAL (input-stream *standard-input*)
				    batch-flag)
  (let ((*package* (find-package "MAXIMA")))
    (if *maxima-started*
	(format t "Maxima restarted.~%")
      (progn
	(format t "~&Maxima ~a http://maxima.sourceforge.net~%"
		*autoconf-version*)
	(format t "Distributed under the GNU Public License. See the file COPYING.~%")
	(format t "Dedicated to the memory of William Schelter.~%")
	(format t "This is a development version of Maxima. The function bug_report()~%")
	(format t "provides bug reporting information.~%")
	(setq *maxima-started* t)))
    (if ($file_search "maxima-init.lisp") ($load "maxima-init.lisp"))
    (if ($file_search "maxima-init.mac") ($batchload "maxima-init.mac"))
    
   (catch 'quit-to-lisp
     (in-package "MAXIMA")
     (sloop 
	 do
       (catch #+kcl si::*quit-tag* #+(or cmu sbcl) 'continue #-(or kcl cmu sbcl) nil
	      (catch 'macsyma-quit
		(continue input-stream batch-flag)(bye)))))))

#-lispm
(progn 

#+kcl
(si::putprop :t 'throw-macsyma-top 'si::break-command)

(defun throw-macsyma-top ()
  (throw 'macsyma-quit t))


(defmfun $writefile (x) (dribble (subseq (string x) 1)))
(defvar $appendfile nil )
(defmfun $appendfile (name)
  (if (and (symbolp name)
	   (member (getcharn name 1) '(#\& #\$)))
      (setq name (subseq (symbol-name name) 1)))
  (if $appendfile (merror "already in appendfile, use closefile first"))
  (let ((stream  (open name :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :create)))
  (setq *appendfile-data* (list stream *terminal-io* name ))
  
  (setq $appendfile (make-two-way-stream
		     (make-echo-stream *terminal-io* stream)
		     (make-broadcast-stream *terminal-io* stream))
	*terminal-io* $appendfile)
  (multiple-value-bind (sec min hour day month year)
		       (get-decoded-time)
		       (format t
			       "~&/* Starts dribbling to ~A (~d/~d/~d, ~d:~d:~d).*/"
			       name year month day hour min sec))
  '$done))
  
(defmfun $closefile ()
  (cond ($appendfile
	 
	 (cond ((eq $appendfile *terminal-io*)
                 (format t "~&/*Finished dribbling to ~A.*/"
			 (nth 2 *appendfile-data*))
		(setq *terminal-io* (nth 1 *appendfile-data*))
		)
	       (t  (warn "*TERMINAL-IO* was rebound while APPENDFILE is on.~%~
                   You may miss some dribble output.")))
	 (close (nth 0 *appendfile-data*))
	 (setq *appendfile-data* nil $appendfile nil)
	 
	 )
	(t (dribble))))
 

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

#+clisp
(defun $system (&rest x) (ext:run-shell-command (apply '$sconcat x)))

#+cmu
(defun $system (&rest args)
  (ext:run-program "/bin/sh" (list "-c" (apply '$sconcat args))))

#+allegro
(defun $system (&rest args)
  (excl:run-shell-command (apply '$sconcat args) :wait t))

#+sbcl
(defun $system (&rest args)
  (sb-ext:run-program "/bin/sh" (list "-c" (apply '$sconcat args)) :output t))

#+openmcl
(defun $system (&rest args)
  (ccl::run-program "/bin/sh" (list "-c" (apply '$sconcat args)) :output t))

(defun $room (&optional (arg nil arg-p))
  (if arg-p
      (room arg)
      (room)))
