;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;;	** (c) Copyright 1982 Massachusetts Institute of Technology **

;;note in converting this file (originally suprv.lisp) to common lisp
;;for the lisp machine, I removed a lot of the old stuff which did not
;;apply, and tried to eliminate any / quoting.  Most of the relevant
;;stuff is in system.lisp for the lispm and nil friends.--wfs

(eval-when
    #+gcl (compile eval)
    #-gcl (:compile-toplevel :execute)
  (setq old-ibase *read-base* old-base *print-base*)
  (setq *read-base* 10. *print-base* 10.))

(defmvar mopl nil)

(declare-top  (special m$+ $lasttime $disptime
		       bindlist loclist errset $labels linelable $filesize
		       st rephrase $dispflag refchkl baktrcl ttyheight
		       dskfnp dsksavep *rset
		       ^w ^r ^q lf tab ff batconl cr ^h ^s
		       $values $functions $arrays $aliases $gradefs $dependencies
		       $rules $props $ratvars $ratvarswitch *mdebug* errbrksw errcatch
		       varlist genvar $device $filename $filenum lbp rbp
		       $gensumnum checkfactors $features featurel $backtrace
		       $weightlevels tellratlist $dontfactor $infolists loadfiles
		       $dskall allbutl lisperrprint
		       alarmclock dcount thistime
		       $nolabels dispflag saveno mcatch brklvl savefile
		       $%% $error
		       *in-translate-file*
		       lessorder greatorder $errorfun mbreak reprint pos $strdisp
		       $dskuse smart-tty rubout-tty more-^w oldst *alphabet*
		       $loadprint opers
		       *ratweights $ratweights quitmsg
		       loadf display-file $grind scrollp $cursordisp
		       $stringdisp $lispdisp defaultf
		       state-pdl command printmsg mrg-punt
		       transp $contexts $setcheck $macros
		       autoload))

(defmvar $prompt
    '_
  nil
  no-reset)


(mapc #'(lambda (x) (putprop (car x) (cadr x) 'opalias))
      '((+ $+) (- $-) (* $*) (// $//) (^ $^) (|.| |$.|) (< $<) (= $=)
	(> $>) (|(| |$(|) (|)| |$)|) (|[| |$[|) (|]| |$]|) (|,| |$,|) (|:| |$:|)
	(|!| |$!|) (|#| |$#|) (|'| |$'|) (|;| |$;|)))

(mapc #'(lambda (x) (setf (symbol-value (car x))
			 (cond ((char< (cadr x) #.(code-char 160.))
				(ascii (cadr x)))
			       (t (cadr x)))))
      '((tab #\tab) (lf #\linefeed) (ff #\page) (cr #\return) (sp #\space)))

(setq $lasttime '((mlist) 0 0)
      thistime 0
      $disptime nil)

(setq batconl nil $strdisp t $grind nil)

(setq refchkl nil
      *mdebug* nil
      baktrcl nil
      errbrksw nil
      mbreak nil
      $errorfun nil
      errcatch nil
      mcatch nil
      brklvl -1
      allbutl nil
      loadf nil
      $backtrace '$backtrace)

(setq *in-translate-file* nil)

(setq $debugmode nil $pagepause nil $dskgc nil $poislim 5)

(setq $loadprint nil ^s nil loadfiles nil)

(setq $nolabels nil $aliases '((mlist simp)) lessorder nil greatorder nil)

(setq $infolists '((mlist simp) $labels $values $functions $macros $arrays
		   $myoptions $props $aliases $rules $gradefs
		   $dependencies $let_rule_packages))

(setq $labels (list '(mlist simp)))

(setq $dskuse nil $device '$dsk $dispflag t linelable nil)

(setq rephrase nil st nil oldst nil reprint nil pos nil)

(setq dcount 0 $filenum 0 $storenum 1000. $filesize 16. $dskall t
      dskfnp nil dsksavep nil saveno 0)

(setq quitmsg  " "
      more-^w nil
      lisperrprint t printmsg nil mrg-punt nil)

(setq state-pdl (ncons 'lisp-toplevel))

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
  "The default file directory for `save', `store', `fassave', and `stringout'."
  no-reset)

(defmvar $file_output_append nil
  "Flag to tell file-writing functions whether to append or clobber the output file.")

(defmvar user-timesofar nil)

(defvar moremsg "--Pause--")
(defvar morecontinue "--Continued--")
(defvar moreflush nil)
(defmvar $morewait nil "needs to be documented" no-reset)

(defmvar $showtime nil)

(defmvar aliaslist nil
  "is used by the `makeatomic' scheme which has never been completed"
  no-reset)

(defun sys-gctime ()
  (status gctime))

(defmvar $file_string_print t
  "If `true', filenames are output as strings; if `false', as lists.")

(defmvar $showtime nil)

(defmfun meval* (test)
  (let (refchkl baktrcl checkfactors)
    (prog2 (if $ratvarswitch (setq varlist (cdr $ratvars)))
	(meval test)
      (clearsign))))

(defmfun makelabel (x)
  (when (and $dskuse (not $nolabels) (> (setq dcount (1+ dcount)) $filesize))
    (setq dcount 0) (dsksave))
  (setq linelable (concat x $linenum))
  (if (not $nolabels)
      (if (or (null (cdr $labels))
	      (when (member linelable (cddr $labels) :test #'equal)
		(setf $labels (delete linelable $labels :count 1 :test #'eq)) t)
	      (not (eq linelable (cadr $labels))))
	  (setq $labels (cons (car $labels) (cons linelable (cdr $labels))))))
  linelable)

(defmfun printlabel nil
  (mtell-open "(~A) " (subseq (print-invert-case linelable) 1)))

(defmfun mexploden (x)
  (let (*print-radix*
	(*print-base* 10))
    (exploden x)))

(defmfun addlabel (label)
  (setq $labels (cons (car $labels) (cons label (delete label (cdr $labels) :count 1 :test #'eq)))))

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

(defun listen () 0)			; Doesn't exist yet.

(defun display* (&aux (ret nil) (tim 0))
  (setq tim (runtime)
	ret (let ((errset 'errbreak2) (thistime -1))
	      (errset (displa (list '(mlable) linelable $%)))))
  (if (null ret) (mtell "~%Error during display~%"))
  (when $disptime
    (mtell-open "Displaytime= ~A sec.~%" (/ (float (- (runtime) tim))
					    internal-time-units-per-second)))
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
	  (stg (write-char (car stg))))))


(defmfun reprint (stg ffp)
  (let (#.ttyoff #.writefilep)
    (if (not ffp) (mterpri))
    (case (car state-pdl)
      (macsyma-toplevel (printlabel))
      (retrieve (if (eq mrg-punt 'break) (princ (stripdollar $prompt)))))
    (setq pos (cursorpos))
    (if stg (princ (maknam (reverse stg))))
    (setq reprint nil)))

; Following GENERIC-AUTOLOAD is copied from orthopoly/orthopoly-init.lisp.
; Previous version didn't take Clisp, CMUCL, or SBCL into account.

(defun generic-autoload (file &aux type)
  (setq file (pathname (cdr file)))
  (setq type (pathname-type file))
  (let ((bin-ext #+gcl "o"
	 #+cmu (c::backend-fasl-file-type c::*target-backend*)
	 #+clisp "fas"
	 #+allegro "fasl"
	 #-(or gcl cmu clisp allegro) ""))
    (if (member type (list bin-ext "lisp" "lsp")  :test 'equalp)
    (load file :verbose 't) ($batchload file))))

(defvar autoload 'generic-autoload)

(defmfun load-function (func mexprp)	; The dynamic loader
  (declare (ignore mexprp))
  (let ((file (get func 'autoload)))
    (if file (funcall autoload (cons func file)))))

(defmfun load-file (file)
  ($load (to-macsyma-namestring file)))

(defmspec $loadfile (form)
  (loadfile (filestrip (cdr form)) nil
	    (not (member $loadprint '(nil $autoload) :test #'equal))))


(defun $setup_autoload (filename &rest functions)
  (let ((file ($file_search filename)))
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
			      ((numberp (setq y (car (errset (readlist (mexploden x)) nil))))
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


(defmfun loadfile (file findp printp &aux (saveno 0))
  (and findp (member $loadprint '(nil $loadfile) :test #'equal) (setq printp nil))
  ;; Should really get the truename of FILE.
  (if printp (format t "~%~A being loaded.~%" file))
  (let* ((path (pathname file))
	 (tem (errset (load (pathname file)))))
    (or tem (merror "Load failed for ~A" (namestring path)))
    (namestring path)))

(defun $directory (path)
  (cons '(mlist) (mapcar 'namestring (directory ($filename_merge path)))))

(defmfun truefname (file)
  (probe-file file))

(defmfun mtruename (stream)
  (declare (ignore stream))
  (merror "Unimplemented!"))

(defmfun carfile (file)		       ; FILE is in OldIO list format.
  (if (= (length file) 3) (cdr file) file))

;; SPECP is T if the file is being batched for TRANSL, or $LOAD,
;;	or some other special purpose.

(defmacro filepos-check ()
  `(if specp (setq filepos (filepos file-obj))))

(defmspec $kill (form)
  (mapc #'kill1 (cdr form))
  '$done)


;;; The following *builtin- variables are used to keep/restore builtin
;;; symbols and values during kill operations. Their values are set at
;;; the end of init-cl.lisp, after all symbols have been defined.

(defvar *builtin-symbols* nil)
(defvar *builtin-symbol-props* (make-hash-table))
(defvar *builtin-$props* nil)
(defvar *builtin-$rules* nil)
(defvar *builtin-symbols-with-values* nil)
(defvar *builtin-symbol-values* (make-hash-table))

(defun kill1-atom (x)
  (let ((z (or (and (member x (cdr $aliases) :test #'equal) (get x 'noun)) (get x 'verb))))
    (when (or (null allbutl) (not (member z allbutl :test #'equal)))
      (remvalue x '$kill)
      (mget x 'array)
      (remcompary x)
      (when (member x (cdr $contexts) :test #'equal)
	($killcontext x))
      (when (mget x '$rule)
	(let ((y (ruleof x)))
	  (cond (y ($remrule y x))
		(t (when (not (member x *builtin-$rules* :test #'equal))
		     (fmakunbound x)
		     (setf $rules (delete x $rules :count 1 :test #'eq)))))))
      (when (and (get x 'operators) (rulechk x))
	($remrule x '$all))
      (when (mget x 'trace)
	(macsyma-untrace x))
      (when (get x 'translated)
	(when (not (member x *builtin-symbols* :test #'equal))
			 (remove-transl-fun-props x)
			 (remove-transl-array-fun-props x)))
      (when (not (get x 'sysconst))
	(remprop x 'mprops))
      (dolist (u '(bindtest nonarray evfun evflag opers special mode))
	(remprop x u))
      (dolist (u opers)
	(if (and (remprop x u)
		 (eq (get x 'operators) 'simpargs1))
	    (remprop x 'operators)))
      (when (member x (cdr $props) :test #'equal)
	(remprop x 'sp2)
	(killframe x))
      (let ((y (get x 'op)))
	(when (and y (not (member y mopl :test #'equal)) (member y (cdr $props) :test #'equal))
	  (kill-operator x)))
      (remalias x nil)
      (setf $arrays (delete x $arrays :count 1 :test #'eq))
      (rempropchk x)
      (setf $functions
	    (delete (assoc (ncons x) $functions :test #'equal) $functions :count 1 :test #'equal))
      (setf $macros
	    (delete (assoc (ncons x) $macros :test #'equal) $macros :count 1 :test #'equal))
      (let ((y (assoc (ncons x) $gradefs :test #'equal)))
	(when y
	  (remprop x 'grad)
	  (setf $gradefs (delete y $gradefs :count 1 :test #'equal))))
      (setf $dependencies
	    (delete (assoc (ncons x) $dependencies :test #'equal) $dependencies :count 1 :test #'equal))
      (when (and (member x *builtin-symbols* :test #'equal)
		 (gethash x *builtin-symbol-props*))
	(setf (symbol-plist x)
	      (copy-tree (gethash x *builtin-symbol-props*))))
      (if z (kill1 z)))))

(defmfun kill1 (x)
  (funcall
   #'(lambda (z)
       (cond ((and allbutl (member x allbutl :test #'equal)))
	     ((eq (setq x (getopr x)) '$labels)
	      (dolist (u (cdr $labels))
		(cond ((and allbutl (member u allbutl :test #'equal))
		       (setq z (nconc z (ncons u))))
		      (t (makunbound u) (remprop u 'time)
			 (remprop u 'nodisp))))
	      (setq $labels (cons '(mlist simp) z) $linenum 0 dcount 0))
	     ((member x '($values $arrays $aliases $rules $props
			$let_rule_packages) :test #'equal)
	      (mapc #'kill1 (cdr (symbol-value x))))
	     ((member x '($functions $macros $gradefs $dependencies) :test #'equal)
	      (mapc #'(lambda (y) (kill1 (caar y))) (cdr (symbol-value x))))
	     ((eq x '$myoptions))
	     ((eq x '$tellrats) (setq tellratlist nil))
	     ((eq x '$ratweights) (setq *ratweights nil
					$ratweights '((mlist simp))))
	     ((eq x '$features)
	      (cond ((not (equal (cdr $features) featurel))
		     (setq $features (cons '(mlist simp) (copy-list featurel))))))
	     ((or (eq x t) (eq x '$all))
	      (mapc #'kill1 (cdr $infolists))
	      (setq $ratvars '((mlist simp)) varlist nil genvar nil
		    checkfactors nil greatorder nil lessorder nil $gensumnum 0
		    $weightlevels '((mlist)) *ratweights nil $ratweights
		    '((mlist simp))
		    tellratlist nil $dontfactor '((mlist)) $setcheck nil)
	      (killallcontexts))
	     ((setq z (assoc x '(($inlabels . $inchar) ($outlabels . $outchar) ($linelabels . $linechar)) :test #'eq))
	      (mapc #'(lambda (y) (remvalue y '$kill))
		    (getlabels* (eval (cdr z)) nil)))
	     ((and (eq (ml-typep x) 'fixnum) (not (< x 0))) (remlabels x))
	     ((atom x) (kill1-atom x))
	     ((and (eq (caar x) 'mlist) (eq (ml-typep (cadr x)) 'fixnum)
		   (or (and (null (cddr x))
			    (setq x (append x (ncons (cadr x)))))
		       (and (eq (ml-typep (caddr x)) 'fixnum)
			    (not (> (cadr x) (caddr x))))))
	      (let (($linenum (caddr x))) (remlabels (- (caddr x) (cadr x)))))
	     ((setq z (mgetl (caar x) '(hashar array))) (remarrelem z x))
	     ((and (eq (caar x) '$allbut)
		   (not (dolist (u (cdr x))
			  (if (not (symbolp u)) (return t)))))
	      (let ((allbutl (cdr x))) (kill1 t)))
	     (t (improper-arg-err x '$kill))))
   nil))


(defmfun remlabels (n)
  (prog (l x)
     (setq l (list (exploden $inchar)
		   (exploden $outchar)
		   (exploden $linechar)))
     loop (setq x (mexploden $linenum))
     (do ((l l (cdr l)))
	 ((null l))
       (remvalue (implode (append (car l) x)) '$kill))
     (if (or (minusp (setq n (1- n))) (= $linenum 0)) (return nil))
     (decf $linenum)
     (go loop)))

(defmfun remvalue (x fn)
  (cond ((not (symbolp x)) (improper-arg-err x fn))
	((boundp x)
	 (let (y)
	   (cond ((or (setq y (member x (cdr $values) :test #'equal))
		      (member x (cdr $labels) :test #'equal))
		  (cond (y (setf $values (delete x $values :count 1 :test #'eq)))
			(t (setf $labels (delete x $labels :count 1 :test #'eq))
			   (remprop x 'time) (remprop x 'nodisp)
			   (if (not (zerop dcount))
			       (setq dcount (1- dcount)))))
		  (makunbound x)
		  (when (member x *builtin-symbols-with-values* :test #'equal)
		    (setf (symbol-value x)
			  (gethash x *builtin-symbol-values*)))
		  t)
		 ((get x 'special)
		  (makunbound x)
		  (when (member x *builtin-symbols-with-values* :test #'equal)
		    (setf (symbol-value x)
			  (gethash x *builtin-symbol-values*)))
		    t)
		 (transp (setf (symbol-value x) x) t)
		 ((eq x '$default_let_rule_package) t)
		 (t
		  (mtell "Warning: Illegal `remvalue' attempt:~%~M" x) nil))))))

(defmfun ruleof (rule)
  (or (mget rule 'ruleof)
      (let* ((pattern (cadr (mget rule '$rule)))
	     (op (if (atom pattern) nil (caar pattern))) l)
	(and (setq l (get op 'rules))
	     (member rule l :test #'equal) op))))

(defmfun $debugmode (x)
  (debugmode1 nil x))

(defun debugmode1 (assign-var y)
  (declare (ignore assign-var))
  (setq *mdebug* (setq *rset y)))

(defun retrieve1 (a b &aux (eof '(nil)))
  (let ((*mread-prompt* b) r )
    (declare (special *mread-prompt*))
    (catch 'macsyma-quit
      (tagbody
       top
	 (setq r (dbm-read (or a *terminal-io*) nil eof))
	 (cond ((and (consp r) (keywordp (car r)))
		(let ((value (break-call (car r) (cdr r) 'break-command)))
		  (if (eq value :resume)
		      (return-from retrieve1 '$exit))
		  (go top))))))
    (nth 2 r)))

(defmfun errbreak (y)		       ; The ERRSET interrupt function
  (cond
    (*mdebug*
     ((lambda (brklvl varlist genvar errbrkl linelable)
	(declare (special $help))
	(prog (x ^q #.ttyoff o^r tim $%% $backtrace retval oldst ($help $help))
	   (setq  errset 'errbreak1)
	   (setq tim (runtime)
		 $%% '$%%
		 ;; just in case baktrcl is cons'd on the stack
		 $backtrace (cons '(mlist simp) (copy-list baktrcl)))
	   (setq o^r #.writefilep #.writefilep (and #.writefilep (not dskfnp)))
	   (cond ((eq y 'noprint))
		 (t
		  (mterpri)
		  (if y (princ 'macsyma-break) (princ 'error-break))
		  (unless (zerop brklvl) (princ " level ") (princ brklvl))
		  (princ " Type exit; to quit, help; for more help.")))
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
				(setq retval 'lisp)
				(go end))
			       ((eq x '$toplevel)
				(cond ((catch 'mbreak
					 (let (st oldst rephrase
						  (mbreak (cons bindlist loclist)))
					   (setq $linenum (1+ $linenum))
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
	   (return retval)))
      (1+ brklvl) varlist genvar (cons bindlist loclist) linelable))))

(defun errbreak1 (ign)
  (declare (ignore ign))
  nil)					; Used to nullify ERRSETBREAKs

(defun errbreak2 (ign) ;; An alternate ERRSET interr. function used by PARSE and DISPLAY
  (declare (ignore ign))
  (let ((state-pdl (cons 'lisp-break state-pdl)))
    (*break errbrksw 'erst)))

(defmspec $tobreak (x)
  (if mbreak (throw 'mbreak (cdr x))
      (merror "`tobreak' may be used only within a Maxima break.")))

(defun errlfun (x)
  (when (null (errset (progn
			(setq ^s nil)
			(if loadf
			    (setq defaultf loadf
				  loadf nil)))))
    (setq ^q nil)
    (mtell-open "~%`errlfun' has been clobbered."))
  (if $errorfun
      (if (null (errset (mapply1 $errorfun nil $errorfun nil)))
	  (mtell "~%Incorrect `errorfun'")))
  (when (null
	 (errset (progn
		   (if (not (eq x 'mquit))
		       (supunbind))
		   (clearsign))))
    (setq ^q nil)
    (mtell-open "~%`errlfun' has been clobbered."))
  (when (null x)
    (princ quitmsg)
    (setq quitmsg " ")))

(defun supunbind nil
  (munbind (reverse bindlist))
  (do ()
      ((null loclist))
    (munlocal)))

(defmfun errlfun1 (mpdls)
  (do ((l bindlist (cdr l))
       (l1))
      ((eq l (car mpdls)) (munbind l1))
    (setq l1 (cons (car l) l1)))
  (do ()
      ((eq loclist (cdr mpdls)))
    (munlocal)))

(defmfun getalias (x)
  (cond ((get x 'alias))
	((eq x '$false) nil)
	(t x)))

(defmfun makealias (x)
  (implode (cons #\$ (exploden x))))

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

(defmfun prinl (l)
  (dolist (x l)
    (princ x)
    (write-char #\space)))

(defmfun $print n
  (if (= n 0)
      '((mlist simp))
      (let ((l (listify n))
	    ;; Don't print out strings with quotation marks!
	    $stringdisp)
	(do ((l l (cddr l)))( (null l)) (rplacd l (cons " " (cdr l))))
	(displa (setq printmsg (cons '(mtext) l)))
	(cadr (reverse l)))))

(defmspec $playback (x)
  (setq x (cdr x))
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
	       ((member (car x) '($showtime $time) :test #'equal) (setq timep (or timep t)))
	       ((member (car x) '($gctime $totaltime) :test #'equal) (setq timep '$all))
	       ((setq l2 (listargp (car x)))
		(setq l1 (nconc l1 (getlabels (car l2) (cdr l2) nil)) largp t))
	       (t (improper-arg-err (car x) '$playback))))
       (cond ((and largp (null numbp)) (go loop))
	     ((and (setq l (cdr $labels)) (not $nolabels)) (setq l (cdr l))))
       (when (or (null numbp) (< (length l) numbp))
	 (setq l1 (reverse l)) (go loop))
       (do ((i numbp (1- i)) (l2)) ((zerop i) (setq l1 (nconc l1 l2)))
	 (setq l2 (cons (car l) l2) l (cdr l)))
       loop (if (null l1) (return '$done))
       ((lambda (errset incharp)
	  (errset
	   (cond ((and (not nostringp) incharp)
		  (let ((linelable (car l1))) (mterpri) (printlabel))
		  (if grindp (mgrind (meval1 (car l1)) nil)
		      (mapc #+gcl #'tyo #-gcl #'write-char (mstring (meval1 (car l1)))))
		  (if (get (car l1) 'nodisp) (princ '$) (princ '|;|))
		  (mterpri))
		 ((or incharp
		      (prog2 (when (and timep (setq l (get (car l1) 'time)))
			       (setq x (gctimep timep (cdr l)))
			       (mtell-open "~A sec." (car l))
			       (if x (mtell-open "  GCtime= ~A sec." (cdr l)))
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
      (merror "`alias' takes an even number of arguments."))
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
  (let ((y (and (or remp (member x (cdr $aliases) :test #'equal)) (get x 'reversealias))))
    (cond ((and y (eq x '%derivative))
	   (remprop x 'reversealias)
	   (setf $aliases (delete x $aliases :count 1 :test #'eq))
	   (remprop '$diff 'alias) '$diff)
	  (y (remprop x 'reversealias)
	     (remprop x 'noun)
	     (setf $aliases (delete x $aliases :count 1 :test #'eq))
	     (remprop (setq x (makealias y)) 'alias) (remprop x 'verb) x))))

(defmfun stripdollar (x)
  (cond ((not (atom x))
	 (cond ((and (eq (caar x) 'bigfloat) (not (minusp (cadr x)))) (implode (fpformat x)))
	       (t (merror "Atomic arg required:~%~M" x))))
	((numberp x) x)
	((null x) 'false)
	((eq x t) 'true)
	((member (getchar x 1) '($ % &) :test #'equal)
	 (intern (subseq (string x) 1)))
	(t x)))

(defmfun fullstrip (x)
  (mapcar #'fullstrip1 x))

(defmfun fullstrip1 (x)
  (or (and (numberp x) x)
      (get x 'reversealias)
      (let ((u (rassoc x aliaslist :test #'eq)))
	(if u (implode (string*1 (car u)))))
      (stripdollar x)))

(defun string* (x)
  (or (and (numberp x) (exploden x))
      (let ((u (rassoc x aliaslist :test #'eq)))
	(if u (string*1 (car u))))
      (string*1 x)))

(defun string*1 (x)
  (let ($stringdisp $lispdisp)
    (makestring x)))

(defun makstring* (x)
  (setq x (string* x))
  (do ((l x (cdr l)))
      ( (null l))
    (rplaca l (ascii (car l))))
  x)

(defmfun $nounify (x)
  (let (y u)
    (nonsymchk x '$nounify)
    (setq x (amperchk x))
    (cond ((get x 'verb))
	  ((get x 'noun) x)
	  ((or (setq u (member (car (setq y (explodec x))) '($ m) :test #'equal))
	       (not (eq (car y) '%)))
	   (setq y (implode (cons '% (if u (cdr y) y))))
	   (putprop y x 'noun) (putprop x y 'verb))
	  (t x))))

(defmfun $verbify (x)
  (nonsymchk x '$verbify)
  (setq x (amperchk x))
  (cond ((get x 'noun))
	((and (char= (getcharn x 1) #\%)
	      (prog2
		  ($nounify (implode (cons #\$ (cdr (exploden x)))))
		  (get x 'noun))))
	(t x)))


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

(defmspec $string (form)
  (setq form (strmeval (fexprcheck form)))
  (setq form (if $grind (strgrind form) (mstring form)))
  (setq st (reverse form) rephrase t)
  (implode (cons #\& form)))

(defmfun makstring (x)
  (setq x (mstring x))
  (do ((l x (cdr l)))
      ((null l))
    (rplaca l (ascii (car l))))
  x)

(defmfun strmeval (x)
  (cond ((atom x) (meval1 x))
	((member (caar x) '(msetq mdefine mdefmacro) :test #'equal) x)
	(t (meval x))))


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
	($bothcoeff $bothcoef)))

(defmfun amperchk (name)
  " $AB ==> $AB,
   $aB ==> $aB,
   &aB ==> $AB,
   |aB| ==> |aB| "
  (if (char= (getcharn name 1) #\&)
      (or (get name 'opr)
	  (implode (cons #\$ (casify-exploden name))))
      name))


#+(and cl (not scl) (not allegro))
(defun casify-exploden (x)
  (cond ((char= (getcharn x 1) #\&)
	 (cdr (exploden (string-upcase (string x)))))
	(t (exploden x))))

#+(or scl allegro)
(defun casify-exploden (x)
  (cond ((char= (getcharn x 1) #\&)
	 (let ((string (string x)))
	   (cond (#+scl (eq ext:*case-mode* :lower)
		  #+allegro (eq excl:*current-case-mode* :case-sensitive-lower)
		  (setf string (string-downcase string)))
		 (t
		  (setf string (string-upcase string))))
	   (cdr (exploden string))))
	(t (exploden x))))

(defmspec $stringout (x)
  (setq x (cdr x))
  (let*
    ((file ($filename_merge (car x)))
     (filespec (if (or (eq $file_output_append '$true) (eq $file_output_append t))
	`(savefile ,file :direction :output :if-exists :append :if-does-not-exist :create)
	`(savefile ,file :direction :output :if-exists :supersede :if-does-not-exist :create))))
    (setq x (cdr x))
    (eval
      `(let (maxima-error l1 truename)
	(declare (special $grind $strdisp))
	    (with-open-file ,filespec
	      (cond ((null
		      (errset
		       (do ((l ',x (cdr l)))( (null l))
			 (cond ((member (car l) '($all $input) :test #'equal)
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
			     (princ (print-invert-case (maknam (mstring (strmeval (car l)))))
					    savefile))
			 (if (or (and (atom (car l)) (get (car l) 'nodisp)) (not $strdisp))
			     (write-char #\$ savefile)
			     (write-char semi-colon-char savefile)))))
		     (setq maxima-error t)))
	      (setq truename (truename savefile))
	      (terpri savefile))
	    (if maxima-error (let ((errset 'errbreak1)) (merror "Error in `stringout' attempt")))
	    (cl:namestring truename)))))

(defmspec $labels (char)
  (setq char (fexprcheck char))
  (nonsymchk char '$labels)
  (cons '(mlist simp) (nreverse (getlabels* char nil))))

(defmfun $%th (x)
  (prog (l outchar)
     (if (or (not (eq (ml-typep x) 'fixnum)) (= x 0))
	 (improper-arg-err x '$%th))
     (if (> x 0) (setq x (- x)))
     (if (cdr $labels)
	 (setq l (cddr $labels) outchar (getlabcharn $outchar)))
     loop (if (null l) (merror "Improper call to %th"))
     (if (and (char= (getlabcharn (car l)) outchar) (= (setq x (1+ x)) 0))
					; Only the 1st alphabetic character of $OUTCHAR is tested.
	 (return (meval (car l))))
     (setq l (cdr l))
     (go loop)))

(defmfun getlabels (n1 n2 flag)	; FLAG = T for STRINGOUT, = NIL for PLAYBACK and SAVE.
  (do ((i n1 (1+ i)) (l1)
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

(defmspec $catch (form)
  (let ((mcatch (cons bindlist loclist)))
    (prog1
	(catch 'mcatch (mevaln (cdr form)))
      (errlfun1 mcatch))))

(defmfun $throw (exp)
  (if (null mcatch) (merror "`throw' not within `catch':~%~M" exp))
  (throw 'mcatch exp))

(defmspec $time (l)
  (setq l (cdr l))
  ;;(format t "~&Time:")
  (cons '(mlist simp)
	(mapcar
	 #'(lambda (x)
	     (or (and (symbolp x)
		      (setq x (or (get x 'time)
				  (and (eq x '$%) (cons (cadr $lasttime) (caddr $lasttime)))))
		      (if (= (cdr x) 0)
			  (car x)
			  (list '(mlist simp) (car x) (cdr x))))
		 '$unknown))
	 l)))

(defmfun timeorg (tim)
  (if (> thistime 0)
      (setq thistime (+ thistime (- (runtime) tim)))))


(defmfun $quit ()
  nil
  (princ *maxima-epilog*)
  #+kcl (lisp::bye)
  #+(or cmu scl) (ext:quit)
  #+sbcl (sb-ext:quit)
  #+clisp (ext:quit)
  #+mcl (ccl::quit)
  #+gcl (quit)
  #+excl "don't know quit function")

(defmfun $logout () (bye))

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

;; File-processing stuff.

(defun mfile nil
  (fullstrip (list $filename (setq $filenum (1+ $filenum)) $device $direc)))

;; This prevents single blank lines from appearing at the top of video
;; terminals.  If at the upper left corner and we want to print a blank
;; line, leave the cursor there and send the blank line to transcript
;; files only.

(defmfun mterpri (&aux x)
  #-nocp (setq x  (cursorpos))
  (if (and smart-tty x (equal x '(0 . 0)))
      (let ((#.ttyoff t)) (terpri))
      (terpri)))

(defmfun $pagepause (x)
  (pagepause1 nil x))

(defun pagepause1 (xx yy)
  (declare (ignore xx yy))
  (merror "`pagepause' does not exist in this system."))

(defmspec $status (form)
  (setq form (cdr form))
  (let* ((keyword (car form))
	 (feature (cadr form)))
    (assert (symbolp keyword))
    (assert (symbolp feature))
    (case keyword
      ($feature (cond ((null feature) (dollarify *features*))
		      ((member (intern (symbol-name
				      (fullstrip1 feature)) 'keyword)
			     *features* :test #'equal) t)))
      ($status '((mlist simp) $feature $status))
      (t (merror "Unknown argument - `status':~%~M" keyword)))))

(defquote $sstatus (status-function item)
  (cond ((equal status-function '$feature)
	 (pushnew ($mkey item) *features*) t)
	((equal status-function '$nofeature)
	 (setq *features* (delete ($mkey item) *features*)) t)
	(t (error "know only how to set and remove feature status"))))

(defmfun $dskgc (xx)
  (declare (ignore xx))
  nil)

(defun dskgc1 (xx yy)
  (declare (ignore xx yy))
  nil)
  
(do ((l '($sqrt $erf $sin $cos $tan $log $plog $sec $csc $cot $sinh $cosh
	  $tanh $sech $csch $coth $asin $acos $atan $acot $acsc $asec $asinh
	  $acosh $atanh $acsch $asech $acoth $binomial $gamma $genfact $del)
	(cdr l)))
    ((null l))
  ((lambda (x)
     (putprop (car l) x 'alias)
     (putprop x (stripdollar (car l)) 'reversealias))
   ($nounify (car l))))

($nounify '$sum)
($nounify '$product)
($nounify '$integrate)
($nounify '$limit)

(defprop $diff %derivative verb)
(defprop %derivative $diff noun)

(mapc #'(lambda (x) (putprop (car x) (cadr x) 'assign))
      '(($debugmode debugmode1)
	($pagepause pagepause1) ($dskgc dskgc1)
	($ttyintfun ttyintfunsetup)
	($fpprec fpprec1) ($poislim poislim1)
	($default_let_rule_package let-rule-setter)
	($current_let_rule_package let-rule-setter)
	($let_rule_packages let-rule-setter)))

(mapc #'(lambda (x) (putprop x 'neverset 'assign)) (cdr $infolists))

(defprop $contexts neverset assign)

(eval-when
    #+gcl (compile eval)
    #-gcl (:compile-toplevel :execute)
    (setq *print-base* old-base *read-base* old-ibase))
