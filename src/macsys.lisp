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

;;(eval-when (:execute :compile-toplevel :load-toplevel)
;;  (sstatus feature maxii))

;;; Standard Kinds of Input Prompts

(defvar *prompt-prefix* "")
(defvar *prompt-suffix* "")
(defvar *general-display-prefix* "")

(defun main-prompt ()
  ;; instead off using this STRIPDOLLAR hackery, the
  ;; MREAD function should call MFORMAT to print the prompt,
  ;; and take a format string and format arguments.
  ;; Even easier and more general is for MREAD to take
  ;; a FUNARG as the prompt. -gjc
  (format () "~A(~A~D) ~A" *prompt-prefix* 
	  (stripdollar $inchar) $linenum *prompt-suffix*))

(defun break-prompt ()
  (declare (special $prompt))
  (stripdollar $prompt))


;; there is absoletely no need to catch errors here, because
;; they are caught by the macsyma-listener window process on
;; the lisp machine, or by setting the single toplevel process in Maclisp. -gjc

(defmacro toplevel-macsyma-eval (x) `(meval* ,x))

(defmvar $_ '$_ "last thing read in, cooresponds to lisp +")
;;Also defined in JPG;SUPRV
#-cl (defmvar $% '$% "last thing printed out, cooresponds to lisp *")
(defmvar $__ '$__ "thing read in which will be evaluated, cooresponds to -")

(declare-top (special *mread-prompt*  $file_search_demo))

(defvar accumulated-time 0.0)
#-cl
(defun fixnum-char-upcase (x) (char-upcase x))
;;#-ti
;;(defun get-internal-real-time () (time:microsecond-time))
;;#-ti
;;(defun get-internal-run-time ()  (* 1000 (send current-process :cpu-time)) )
;;(defvar internal-time-units-per-second  1000000)

;;#+lispm
;;(defun used-area ( &optional (area working-storage-area ))
;;  (multiple-value-bind (nil used)(si:room-get-area-length-used area)
;;    used))

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

(defun continue (&optional (input-stream *standard-input*)
		 batch-or-demo-flag)
  (declare (special *socket-connection*))
  (if (eql batch-or-demo-flag :demo)
      (format t "~% At the _ prompt, type ';' followed by enter to get next demo"))
  (catch 'abort-demo
    (do ((r)
	 (time-before)
	 (time-after)
	 (time-used)
	 (eof (list nil))
	 (etime-before)
	 (etime-after)
	 (area-before)
	 (area-after)
	 (etime-used)
	 (c-tag)
	 (d-tag))
	(nil)
      (catch 'return-from-debugger
	(when (not (checklabel $inchar))
	  (setq $linenum (f1+ $linenum)))
	#+akcl(si::reset-stack-limits)
	(setq c-tag (makelabel $inchar))
	(let ((*mread-prompt* (if batch-or-demo-flag nil (main-prompt)))
	      (eof-count 0))
	  (tagbody
	   top
	     (setq r      (dbm-read input-stream nil eof))
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
    
	(format t "~a" *general-display-prefix*)
	(cond (#.writefilep  ;write out the c line to the dribble file
	       (let ( (#.ttyoff t) smart-tty  $linedisp)
		 (displa `((mlable) , c-tag , $__)))))
	(if (eq r eof) (return '$done))
	(fresh-line *standard-output*)
	;;    #+lispm (SEND *standard-output* :SEND-IF-HANDLES ':FORCE-OUTPUT)
	(setq $__ (caddr r))
	(set  c-tag $__)
	(cond (batch-or-demo-flag
	       (displa `((mlable) ,c-tag , $__))))
	(setq time-before (get-internal-run-time)
	      etime-before (get-internal-real-time))
	(setq area-before (used-area))
	(setq $% (toplevel-macsyma-eval $__))
	(setq etime-after (get-internal-real-time)
	      time-after (get-internal-run-time))
	(setq area-after (used-area))
	(setq time-used (quotient (float (difference time-after time-before))
				  internal-time-units-per-second)
	      etime-used (quotient (float (difference etime-after etime-before))
				   internal-time-units-per-second))
	(setq accumulated-time (plus accumulated-time time-used))
	(set (setq d-tag (makelabel $outchar)) $%)
	(setq $_ $__)
	(when $showtime
	  ;;	  #+NIL (format t "~&Evaluation took ~$ seconds (~$ elapsed)."
	  ;;		    time-used etime-used)
	  ;;	  #-(or NIL cl) (mtell "Evaluation took ~S seconds (~S elapsed)."
	  ;;			   time-used etime-used)
	  (format t "~&Evaluation took ~$ seconds (~$ elapsed)"
		  time-used etime-used )
	  ;;	  #+lispm (format t "using ~A words." (f-  area-after area-before))
	  #+(or cmu sbcl clisp)
	  (let ((total-bytes (- area-after area-before)))
	    (cond ((> total-bytes (* 1024 1024))
		   (format t " using ~,3F MB."
			   (/ total-bytes (* 1024.0 1024.0))))
		  ((> total-bytes 1024)
		   (format t " using ~,3F KB." (/ total-bytes 1024.0)))
		  (t
		   (format t " using ~:D bytes." total-bytes))))

	  )
	(unless $nolabels
	  (putprop d-tag
		   (cons time-used  0)
		   'time))
	(fresh-line *standard-output*)
	;;    #+never(let ((tem (read-char-no-hang)))
	;;      (or (eql tem #\newline) (and tem (unread-char tem))))
	(if (eq (caar r) 'displayinput)
	    (displa `((mlable) ,d-tag ,$%)))
	(when (eq batch-or-demo-flag ':demo)
	  (mtell "~&~A_~A" *prompt-prefix* *prompt-suffix*)
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
	(and batch-or-demo-flag
	     ;;	 #+lispm
	     ;;	 (send input-stream :operation-handled-p :read-char-no-echo)
	     ;;	 #+lispm
	     ;;	 (send input-stream :operation-handled-p :unread-char-no-echo)
	     (do ((char)) (())
	       (setq char (read-char input-stream nil #+cl nil)) 

;;;; INSERTED BY MASAMI 
	       (when (null char) 
		 (throw 'macsyma-quit nil)) 
;;;; END INSERT 

	       (unless (zl-member char '(#\space #\newline #\return #\tab))
		 (unread-char char input-stream)  
		 (return nil)))))))) 


(defun $break (&rest arg-list)
  (prog1 (apply #'$print arg-list)
    (mbreak-loop)))



(defun mbreak-loop ()
  (let ((*standard-input* #+nil (make-synonym-stream '*terminal-io*)
			  #-nil *debug-io*)
	(*standard-output* *debug-io*))
    (catch 'break-exit
      (format t "~%Entering a Macsyma break point. Type EXIT; to resume")
      (do ((r)) (nil)
	(fresh-line)
	(setq r (caddr (let ((*mread-prompt* (break-prompt)))
			 (mread *standard-input*))))
	(case r
	  (($exit) (throw 'break-exit t))
	  (t (errset (displa (meval r)) t)))))))

(defun merrbreak (&optional arg)
  (format *debug-io* "~%Merrbreak:~A" arg)
  (mbreak-loop))

#-cl
(defun retrieve (msg flag &aux (print? nil))
  (declare (special msg flag print?))
  (or (eq flag 'noprint) (setq print? t))
  (mread-terminal
   (closure '(msg flag)
	    #'(lambda (stream char) stream char
		      (cond ((not print?) (setq print? t))
			    ((null msg))
			    ((atom msg) (princ msg) (mterpri))
			    ((eq flag t) (mapc #'princ (cdr msg)) (mterpri))
			    (t (displa msg) (mterpri)))))))
#+cl
(defun retrieve (msg flag &aux (print? nil))
  (declare (special msg flag print?))
  (or (eq flag 'noprint) (setq print? t))
  (cond ((not print?) 
	 (setq print? t)
	 (princ *prompt-prefix*)
	 (princ *prompt-suffix*))
	((null msg)
	 (princ *prompt-prefix*)
	 (princ *prompt-suffix*))
	((atom msg) 
	 (format t "~a~a~a" *prompt-prefix* msg *prompt-suffix*) 
	 (mterpri))
	((eq flag t)
	 (princ *prompt-prefix*) 
	 (mapc #'princ (cdr msg)) 
	 (princ *prompt-suffix*)
	 (mterpri))
	(t 
	 (princ *prompt-prefix*)
	 (displa msg) 
	 (princ *prompt-suffix*)
	 (mterpri)))
  (mread-noprompt *query-io* nil))


(defmfun $read (&rest l)
  (meval (apply #'$readonly l)))

(defmfun $readonly (&rest l)
  (let ((*mread-prompt*
	 (if l (string-right-trim '(#\n)
				  (with-output-to-string (*standard-output*)
				    (apply '$print l))) "")))
    (setf *mread-prompt* (format nil "~a~a~a" *prompt-prefix* *mread-prompt* 
				 *prompt-suffix*))
    (setf answer (third (mread *query-io*)))))


;;#-cl
;;(DEFUN MREAD-TERMINAL (PROMPT)
;;  (prog1 (let (#+NIL (si:*ttyscan-dispatch-table *macsyma-ttyscan-operators*))
;;	    (CADDR (send *terminal-io* ':RUBOUT-HANDLER
;;			 `((:PROMPT ,PROMPT) #+NIL (:reprompt ,prompt))
;;			 #'MREAD-RAW *terminal-io*)))
;;	 (fresh-line *terminal-io*)))


(defun make-input-stream (x y) y	;ignore
       x)

(defun batch (filename &optional demo-p
	      &aux (orig filename )
	      list
	      file-obj (accumulated-time 0.0) (abortp t))
  (setq list (if demo-p '$file_search_demo '$file_search_maxima))
  (setq filename ($file_search filename (symbol-value list)))
  (or filename (merror "Could not find ~M in ~M: ~M"
		       orig list (symbol-value list)))
  
  (unwind-protect
       (progn (batch-internal (setq file-obj (open filename)) demo-p)
	      (setq abortp nil)
	      (when $showtime
		(format t "~&Batch spent ~$ seconds in evaluation.~%"
			accumulated-time)))
    (if file-obj (close file-obj))
    (when abortp (format t "~&(Batch of ~A aborted.)~%" filename))))


(defun batch-internal (fileobj demo-p)
  (continue (make-echo-stream
	     (make-input-stream fileobj "Batch Input Stream")
	     *standard-output*)
	    (if demo-p ':demo ':batch)))
#-cl
(defun $batch (&rest arg-list)
  (batch (filename-from-arg-list arg-list) nil))

(defun filename-from-arg-list (arg-list)
  (if (= (length arg-list) 1)
      ($filename_merge (car arg-list))
      ($filename_merge `((mlist),@arg-list))))

(defmspec $grindef (form)
  (eval `(grindef ,@(cdr form)))
  '$done)

#+cl
(defun $demo (&rest arg-list)
  (let ((tem ($file_search (car arg-list) $file_search_demo)))
    (or tem (merror "Could not find ~M in  ~M: ~M" (car arg-list) '$file_search_demo $file_search_demo   ))
    ($batch tem	  '$demo)))

#-cl
(defun $demo (&rest arg-list)
  (batch (filename-from-arg-list arg-list) t))

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
(defvar *maxima-prolog* "")
(defvar *maxima-epilog* "")
(defun meshugena-clisp-banner ()
  (format t "  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo~%")
  (format t "  I I I I I I I      8     8   8           8     8     o  8    8~%")
  (format t "  I  \\ `+' /  I      8         8           8     8        8    8~%")
  (format t "   \\  `-+-'  /       8         8           8      ooooo   8oooo~%") ;
  (format t "    `-__|__-'        8         8           8           8  8~%")
  (format t "        |            8     o   8           8     o     8  8~%")
  (format t "  ------+------       ooooo    8oooooo  ooo8ooo   ooooo   8~%")
  (format t "~%")
  (format t "Copyright (c) Bruno Haible, Michael Stoll 1992, 1993~%")
  (format t "Copyright (c) Bruno Haible, Marcus Daniels 1994-1997~%")
  (format t "Copyright (c) Bruno Haible, Pierpaolo Bernardi, Sam Steingold 1998~%")
  (format t "Copyright (c) Bruno Haible, Sam Steingold 1999-2003~%")
  (format t 
	  "--------------------------------------------------------------~%~%"))

#-lispm
(defun macsyma-top-level (&optional (input-stream *standard-input*)
			  batch-flag)
  (let ((*package* (find-package "MAXIMA")))
    (if *maxima-started*
	(format t "Maxima restarted.~%")
	(progn
	  #+clisp (meshugena-clisp-banner)
	  (format t *maxima-prolog*)
	  (format t "~&Maxima ~a http://maxima.sourceforge.net~%"
		  *autoconf-version*)
	  (format t "Using Lisp ~a ~a" (lisp-implementation-type)
		  #-clisp (lisp-implementation-version)
		  #+clisp (subseq (lisp-implementation-version)
				  0 (+ 1 (search
					  ")" (lisp-implementation-version)))))
	  #+gcl (format t " (aka GCL)")
	  (format t "~%")
	  (format t "Distributed under the GNU Public License. See the file COPYING.~%")
	  (format t "Dedicated to the memory of William Schelter.~%")
	  (format t "This is a development version of Maxima. The function bug_report()~%")
	  (format t "provides bug reporting information.~%")
	  (setq *maxima-started* t)))
    (if ($file_search "maxima-init.lisp") ($load ($file_search "maxima-init.lisp")))
    (if ($file_search "maxima-init.mac") ($batchload ($file_search "maxima-init.mac")))
    
    (catch 'quit-to-lisp
      (in-package "MAXIMA")
      (sloop 
       do
       (catch #+kcl si::*quit-tag* #+(or cmu sbcl) 'continue #-(or kcl cmu sbcl) nil
	      (catch 'macsyma-quit
		(continue input-stream batch-flag)
		(format t *maxima-epilog*)
		(bye)))))))

#-lispm
(progn 
  
  #+kcl
  (si::putprop :t 'throw-macsyma-top 'si::break-command)

  (defun throw-macsyma-top ()
    (throw 'macsyma-quit t))


  (defmfun $writefile (x) (dribble (subseq (string x) 1)))

  (defvar $appendfile nil )

  (defvar *appendfile-data*)

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
 
  (defmfun $cli () (merror "Not implemented!") )
 
  (defun nsubstring (x y) (subseq x y)) 
 
  (defun filestrip (x) (subseq (string (car x)) 1)) 
  )

(defmspec $with_stdout ( arg) (setq arg (cdr arg))
	  (let ((body (cdr arg)) res)
	    (with-open-file (*standard-output* (namestring (maxima-string (car arg)))
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

(defun $system (&rest args)
  #+gcl   (system (apply '$sconcat args))
  #+clisp (ext:run-shell-command (apply '$sconcat args))
  #+cmu   (ext:run-program "/bin/sh"
			   (list "-c" (apply '$sconcat args)) :output t)
  #+allegro (excl:run-shell-command (apply '$sconcat args) :wait t)
  #+sbcl  (sb-ext:run-program "/bin/sh"
			      (list "-c" (apply '$sconcat args)) :output t)
  #+openmcl (ccl::run-program "/bin/sh"
			      (list "-c" (apply '$sconcat args)) :output t)
  )

(defun $room (&optional (arg nil arg-p))
  (if arg-p
      (room arg)
      (room)))

(defun maxima-lisp-debugger (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (format t "~&Maxima encountered a Lisp error:~%~% ~A" condition)
  (format t "~&~%Automatically continuing.~%To reenable the Lisp debugger set *debugger-hook* to nil.~%")
  (throw 'return-from-debugger t))
