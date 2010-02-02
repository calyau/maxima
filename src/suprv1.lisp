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

(declare-top  (special
		       bindlist loclist errset *rset ^q lf tab ff cr
		       $values $functions $arrays $gradefs $dependencies
		       $rules $props $ratvars $ratvarswitch
		       varlist genvar $filename
		       $gensumnum checkfactors $features featurel
		       $weightlevels tellratlist $dontfactor
		       dispflag savefile $%% $error smart-tty
		       opers *ratweights $ratweights
		       $stringdisp $lispdisp defaultf command
		       transp $contexts $setcheck $macros autoload))

(mapc #'(lambda (x) (putprop (car x) (cadr x) 'opalias))
      '((+ $+) (- $-) (* $*) (// $//) (^ $^) (|.| |$.|) (< $<) (= $=)
	(> $>) (|(| |$(|) (|)| |$)|) (|[| |$[|) (|]| |$]|) (|,| |$,|) (|:| |$:|)
	(|!| |$!|) (|#| |$#|) (|'| |$'|) (|;| |$;|)))

(mapc #'(lambda (x) (setf (symbol-value (car x))
			 (cond ((char< (cadr x) #.(code-char 160.))
				(ascii (cadr x)))
			       (t (cadr x)))))
      '((tab #\tab) (lf #\linefeed) (ff #\page) (cr #\return) (sp #\space)))

(defvar thistime 0)
(defvar refchkl nil)
(defvar *mdebug* nil)
(defvar baktrcl nil)
(defvar errbrksw nil)
(defvar mbreak nil)
(defvar errcatch nil)
(defvar mcatch nil)
(defvar brklvl -1)
(defvar allbutl nil)
(defvar loadf nil)
(defvar lessorder nil)
(defvar greatorder nil)
(defvar *in-translate-file* nil)
(defvar linelable nil)
(defvar rephrase nil)
(defvar st nil)
(defvar oldst nil)
(defvar reprint nil)
(defvar pos nil)
(defvar dcount 0)
(defvar dskfnp nil)
(defvar dsksavep nil)
(defvar saveno 0)
(defvar quitmsg  " ")
(defvar more-^w nil)
(defvar lisperrprint t)

(defvar state-pdl (ncons 'lisp-toplevel))

(defmvar $filenum 0)
(defmvar $storenum 1000.)
(defmvar $dskall t)
(defmvar $errorfun nil)
(defmvar $disptime nil)
(defmvar $strdisp t)
(defmvar $grind nil)
(defmvar $prompt '_)
(defmvar $backtrace '$backtrace)
(defmvar $debugmode nil)
(defmvar $pagepause nil)
(defmvar $poislim 5)
(defmvar $loadprint nil)
(defmvar $nolabels nil)
(defmvar $aliases '((mlist simp)))

(defmvar $infolists
  '((mlist simp) $labels $values $functions $macros $arrays
                 $myoptions $props $aliases $rules $gradefs
                 $dependencies $let_rule_packages $structures))

(defmvar $labels (list '(mlist simp)))
(defmvar $device '$dsk)
(defmvar $dispflag t)

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

(defmvar $showtime nil)

;(defmfun meval* (test)
;  (let (refchkl baktrcl checkfactors)
;    (prog2 (if $ratvarswitch (setq varlist (cdr $ratvars)))
;	(meval test)
;      (clearsign))))

;; This version of meval* makes sure that the facts from the global variable
;; locals are cleared with a call to clearsign. The facts are added by asksign
;; and friends. meval* function is only used for top level evaluations.
;; For other cases the function meval can be used.

(defun meval* (expr)
  ;; Make sure that clearsign is called after the evaluation.
  (unwind-protect
    (let (refchkl baktrcl checkfactors)
      (if $ratvarswitch (setq varlist (cdr $ratvars)))
      (meval expr))
    ;; Clear the facts from asksign and friends.
    (clearsign)))

(defmfun makelabel (x)
  (setq linelable ($concat '|| x $linenum))
  (unless $nolabels
    (when (or (null (cdr $labels))
	      (when (member linelable (cddr $labels) :test #'equal)
		(setf $labels (delete linelable $labels :count 1 :test #'eq)) t)
	      (not (eq linelable (cadr $labels))))
      (setq $labels (cons (car $labels) (cons linelable (cdr $labels))))))
  linelable)

(defmfun printlabel ()
  (mtell-open "(~A) " (subseq (print-invert-case linelable) 1)))

(defmfun mexploden (x)
  (let (*print-radix*
	(*print-base* 10))
    (exploden x)))

(defmfun addlabel (label)
  (setq $labels (cons (car $labels) (cons label (delete label (cdr $labels) :count 1 :test #'eq)))))

(defmfun tyi* ()
  (clear-input)
  (do ((n (tyi) (tyi))) (nil)
    (cond ((or (char= n #\newline) (and (> (char-code n) 31) (char/= n #\rubout)))
	   (return n))
	  ((char= n #\page) (format t "~|") (throw 'retry nil)))))

(defun continuep ()
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
  (not (or $nolabels
	   (= $linenum 0)
	   (boundp ($concat '|| x $linenum)))))

(defun gctimep (timep tim)
  (cond ((and (eq timep '$all) (not (zerop tim))) (princ "Totaltime= ") t)
	(t (princ "Time= ") nil)))

(defun display* (&aux (ret nil) (tim 0))
  (setq tim (get-internal-run-time)
	ret (let ((errset 'errbreak2) (thistime -1))
	      (errset (displa (list '(mlable) linelable $%)))))
  (if (null ret) (mtell "~%Error during display~%"))
  (when $disptime
    (mtell-open "Displaytime= ~A sec.~%" (/ (float (- (get-internal-run-time) tim))
					    internal-time-units-per-second)))
  ret)

; Following GENERIC-AUTOLOAD is copied from orthopoly/orthopoly-init.lisp.
; Previous version didn't take Clisp, CMUCL, or SBCL into account.

(defvar *autoloaded-files* ())

(defun generic-autoload (file &aux type)
  (unless (member file *autoloaded-files* :test #'equal)
    (push file *autoloaded-files*)
    (setq file (pathname (cdr file)))
    (setq type (pathname-type file))
    (let ((bin-ext #+gcl "o"
		   #+cmu (c::backend-fasl-file-type c::*target-backend*)
		   #+clisp "fas"
		   #+allegro "fasl"
		   #+openmcl (pathname-type ccl::*.fasl-pathname*)
		   #+lispworks (pathname-type (compile-file-pathname "foo.lisp"))
		   #-(or gcl cmu clisp allegro openmcl lispworks) ""))
      (if (member type (list bin-ext "lisp" "lsp")  :test 'equalp)
	  #-sbcl (load file) #+sbcl (with-compilation-unit nil (load file))
	  ($load file)))))

(defvar autoload 'generic-autoload)

(defmfun load-function (func mexprp)	; The dynamic loader
  (declare (ignore mexprp))
  (let ((file (get func 'autoload)))
    (if file (funcall autoload (cons func file)))))

(defmfun load-file (file)
  ($load (to-macsyma-namestring file)))

(defmspec $loadfile (form)
  (loadfile (namestring (maxima-string (meval (cadr form)))) nil
	    (not (member $loadprint '(nil $autoload) :test #'equal))))


(defun $setup_autoload (filename &rest functions)
  (let ((file ($file_search filename)))
    (dolist (func functions)
      (nonsymchk func '$setup_autoload)
      (putprop (setq func ($verbify func)) file 'autoload)
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
	 ($load_pathname path)
	 (tem (errset #-sbcl (load (pathname file)) #+sbcl (with-compilation-unit nil (load (pathname file))))))
    (or tem (merror "Load failed for ~A" (namestring path)))
    (namestring path)))

(defun $directory (path)
  (cons '(mlist) (mapcar 'namestring (directory ($filename_merge path)))))

(defmfun truefname (file)
  (probe-file file))

(defmfun carfile (file)		       ; FILE is in OldIO list format.
  (if (= (length file) 3) (cdr file) file))

;; SPECP is T if the file is being batched for TRANSL, or $LOAD,
;;	or some other special purpose.

(defmacro filepos-check ()
  `(if specp (setq filepos (filepos file-obj))))

(defmspec $kill (form)
  (clear)	;; get assume db into consistent state
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
(defvar *builtin-numeric-constants* '($%e $%pi $%phi $%gamma))

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
	(killframe x)
	(i-$remove (list x $features)))
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
      (let ((y (assoc-if #'(lambda (e) (equal x (car e))) (cdr $structures))))
        (when y
          (remprop x 'dimension)
          (remprop x 'defstruct-template)
          (remprop x 'defstruct-default)
          (remprop x 'translate)
          (setf $structures (delete y $structures :count 1 :test #'equal))))
      (when (and (member x *builtin-symbols* :test #'equal)
		 (gethash x *builtin-symbol-props*))
	(setf (symbol-plist x)
	      (copy-tree (gethash x *builtin-symbol-props*))))
      (when (member x *builtin-numeric-constants*)
	(initialize-numeric-constant x))	;; reset db value for $%pi, $%e, etc
      (if z (kill1 z)))))

(defmfun kill1 (x)
  (if (and (stringp x) (not (getopr0 x))) (return-from kill1 nil))
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
	     ((member x '($functions $macros $gradefs $dependencies $structures) :test #'equal)
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
         ((eq (caar x) '$@) (mrecord-kill x))
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
		  (mtell "remvalue: warning: cannot remove value of:~%~M" x) nil))))))

(defmfun ruleof (rule)
  (or (mget rule 'ruleof)
      (let* ((pattern (cadr (mget rule '$rule)))
	     (op (if (atom pattern) nil (caar pattern))) l)
	(and (setq l (get op 'rules))
	     (member rule l :test #'equal) op))))

(defmfun $debugmode (x)
  (setq $debugmode x)
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
  (cond (*mdebug*
	 (let ((brklvl (1+ brklvl))
	       (varlist varlist)
	       (genvar genvar)
	       (errbrkl (cons bindlist loclist))
	       (linelable linelable))
	   (declare (special $help))
	   (prog (x ^q #.ttyoff o^r tim $%% $backtrace retval oldst)
	      (setq  errset 'errbreak1)
	      (setq tim (get-internal-run-time)
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
	      a    (cond ((null
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
						  (incf $linenum)
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
		     (unless (zerop brklvl) (princ brklvl))
		     (mterpri))
	      (if o^r (setq #.writefilep t))
	      (return retval))))))

(defun errbreak1 (ign)
  (declare (ignore ign))
  nil)					; Used to nullify ERRSETBREAKs

(defun errbreak2 (ign) ;; An alternate ERRSET interr. function used by PARSE and DISPLAY
  (declare (ignore ign))
  (let ((state-pdl (cons 'lisp-break state-pdl)))
    (break "erst ~S" '(errbrksw))))

(defmspec $tobreak (x)
  (if mbreak (throw 'mbreak (cdr x))
      (merror "`tobreak' may be used only within a Maxima break.")))

(defun errlfun (x)
  (when (null (errset (progn
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

(defmfun $print (&rest args)
  (if (null args)
      '((mlist simp))
      (let ((l args) $stringdisp) ;; Don't print out strings with quotation marks!
	(do ((l l (cddr l)))
	    ((null l))
	  (rplacd l (cons " " (cdr l))))
	(displa (cons '(mtext) l))
	(cadr (reverse l)))))

(defmspec $playback (x)
  (setq x (cdr x))
  (let ((state-pdl (cons 'playback state-pdl)))
    (prog (l l1 l2 numbp slowp nostringp inputp timep grindp inchar largp)
       (setq inchar (getlabcharn $inchar)) ; Only the 1st alphabetic char. of $INCHAR is tested
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
       (let ((errset 'errbreak2)
	     (incharp (char= (getlabcharn (car l1)) inchar)))
	 (errset
	  (cond ((and (not nostringp) incharp)
		 (let ((linelable (car l1))) (mterpri) (printlabel))
		 (if grindp
		     (mgrind (meval1 (car l1)) nil)
		     (mapc #'(lambda (x) (write-char x)) (mstring (meval1 (car l1))))) ;gcl doesn't like a
					; simple write-char, therefore wrapped it up in a lambda - are_muc
		 (if (get (car l1) 'nodisp) (princ "$") (princ ";"))
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
       (when (and slowp (cdr l1) (not (continuep)))
	 (return '$terminated))
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
        ((eq x y) y) ; x is already the alias of y
; Not needed. We return the alias immediately if we already have one.
;	((not (eq (getcharn x 1) #\$))
;	 (merror "-ed symbols may not be aliased. ~M" x))
	((get x 'reversealias)
	 (if (not (eq x y))
	     (merror "~M already is aliased." x)))
	(t (putprop x y'alias)
	   (putprop y x 'reversealias)
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
	     (remprop (setq x y) 'alias) (remprop x 'verb) x))))

(defmfun stripdollar (x)
  (cond ((not (atom x))
	 (cond ((and (eq (caar x) 'bigfloat) (not (minusp (cadr x)))) (implode (fpformat x)))
	       (t (merror "Atomic arg required:~%~M" x))))
	((numberp x) x)
	((null x) 'false)
	((eq x t) 'true)
    ((member (getcharn x 1) '(#\$ #\%))
     (intern (subseq (string x) 1)))
	(t x)))

(defmfun fullstrip (x)
  (mapcar #'fullstrip1 x))

(defmfun fullstrip1 (x)
  (or (and (numberp x) x)
      (let ((y (get x 'reversealias))) (if y (stripdollar y)))
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

;;; Note that this function had originally stripped a prefix of '|M|.  This
;;; was intended for operators such as 'MABS, but with the case flipping
;;; performed by explodec this test would always fail.  Dependent code has
;;; been written assuming the '|M| prefix is not stripped so this test has
;;; been disabled for now.
;;;
(defmfun $nounify (x)
  (if (not (or (symbolp x) (stringp x)))
    (merror "nounify: argument must be a symbol or a string."))
  (setq x (amperchk x))
  (cond ((get x 'verb))
	((get x 'noun) x)
	(t
	 (let* ((y (explodec x))
		(u #+nil (member (car y) '($ |M| |m|) :test 'eq)
		   (eq (car y) '$)))
	   (cond ((or u (not (eq (car y) '%)))
		  (setq y (implode (cons '% (if u (cdr y) y))))
		  (putprop y x 'noun) (putprop x y 'verb))
		 (t x))))))

(defmfun $verbify (x)
  (if (not (or (symbolp x) (stringp x)))
    (merror "verbify: argument must be a symbol or a string."))
  (setq x (amperchk x))
  (cond ((get x 'noun))
        ((eq x '||) x)
	((and (char= (char (symbol-name x) 0) #\%)
	      (prog2
		  ($nounify (implode (cons #\$ (cdr (exploden x)))))
		  (get x 'noun))))
	(t x)))

(defmspec $string (form)
  (setq form (strmeval (fexprcheck form)))
  (setq form (if $grind (strgrind form) (mstring form)))
  (setq st (reverse form) rephrase t)
  (coerce form 'string))

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
		(putprop (cadr x) (car x) 'reversealias))
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
; Cut out the alias binom for binomial
;	($binom %binomial)
        ($derivative $diff) ($prod $product)
	($bothcoeff $bothcoef)))

(defmfun amperchk (name)
  (cond
    ((symbolp name) name)
    ((stringp name)
     (getalias (or (getopr0 name) (implode (cons #\$ (coerce name 'list))))))))

(defmspec $stringout (x)
  (setq x (cdr x))
  (let*
    ((file (namestring (maxima-string (meval (car x)))))
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
			 (if (or (and (symbolp (car l)) (get (car l) 'nodisp)) (not $strdisp))
			     (write-char #\$ savefile)
			     (write-char #\; savefile)))))
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
  (let ((c (char (symbol-name label) 1)))
    (if (char= c #\%)
	(char (symbol-name label) 2)
	c)))

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
  (cons '(mlist simp)
	(mapcar
	 #'(lambda (x)
	     (or (and (symbolp x)
		      (setq x (get x 'time))
		      (if (= (cdr x) 0)
			  (car x)
			  (list '(mlist simp) (car x) (cdr x))))
		 '$unknown))
	 l)))

(defmfun timeorg (tim)
  (if (> thistime 0)
      (incf thistime (- (get-internal-run-time) tim))))


(defmfun $quit ()
  nil
  (princ *maxima-epilog*)
  #+kcl (lisp::bye)
  #+(or cmu scl) (ext:quit)
  #+sbcl (sb-ext:quit)
  #+clisp (ext:quit)
  #+(or openmcl mcl) (ccl::quit)
  #+gcl (quit)
  #+abcl (cl-user::quit)
  #+ecl (si:quit)
  #+excl "don't know quit function"
  #+lispworks (lispworks:quit))

(defmfun $logout () (bye))

;; File-processing stuff.

(defun mfile nil
  (fullstrip (list $filename (setq $filenum (1+ $filenum)) $device $direc)))

;; This prevents single blank lines from appearing at the top of video
;; terminals.  If at the upper left corner and we want to print a blank
;; line, leave the cursor there and send the blank line to transcript
;; files only.

(defmfun mterpri ()
  (if smart-tty
      (let ((#.ttyoff t))
	(terpri))
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
    (when (not (symbolp keyword))
      (merror (intl:gettext "status: First argument must be a symbol.")))
    (when (not (or (stringp feature) (symbolp feature)))
      (merror
        (intl:gettext "status: Second argument must be symbol or a string.")))
    (case keyword
      ($feature (cond ((null feature) (dollarify *features*))
                      ((member (intern (if (stringp feature)
                                           (maybe-invert-string-case feature)
                                           (symbol-name (fullstrip1 feature)))
                                       'keyword)
                               *features* :test #'equal) t)))
      (t (merror (intl:gettext "status: Unknown argument: ~M~%") keyword)))))

(defquote $sstatus (keyword item)
  (cond ((equal keyword '$feature)
         (pushnew ($mkey item) *features*) t)
        ((equal keyword '$nofeature)
         (setq *features* (delete ($mkey item) *features*)) t)
        (t
         (merror (intl:gettext "sstatus: Unknown argument: ~M~%") keyword))))

(dolist (l '($sin $cos $tan $log $plog $sec $csc $cot $sinh $cosh
	     $tanh $sech $csch $coth $asin $acos $atan $acot $acsc $asec $asinh
	     $acosh $atanh $acsch $asech $acoth $binomial $gamma $genfact $del))
  (let ((x ($nounify l)))
    (putprop l x 'alias)
    (putprop x l 'reversealias)))

($nounify '$sum)
($nounify '$product)
($nounify '$integrate)
($nounify '$limit)

(defprop $diff %derivative verb)
(defprop %derivative $diff noun)

(mapc #'(lambda (x) (putprop (car x) (cadr x) 'assign))
      '(($debugmode debugmode1)
	($pagepause pagepause1)
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
