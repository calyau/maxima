;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")

(macsyma-module nparse)
(load-macsyma-macros defcal mopers)

(proclaim '(optimize (safety 2) (speed 2) (space 2)))
(defmvar alphabet
  '(#\_ #\%)
  "alphabetic exceptions list")
;;;  Note: The following algorithms work only in environments where 
;;;        ascii codes for A,...,Z and 0,...,9 follow sequentially.
;;;	   Normal ASCII and LispM encoding makes this true. If we ever
;;;	   bring this up on EBCDIC machines, we may lose.

(defmacro imember (x l)
  `(member ,x ,l))

;#-cl ;;defined in commac or via common
;(cond ((not (fboundp 'char<=)))
; (defun char<= (a b) (<= a b))
; (defun char>= (a b) (>= a b)))


(progn

  (defmvar alphabet '(#\_ #\%))

  (defmfun alphabetp (n)
    #-cl (declare (fixnum n))
    (and (characterp n)
	 (or (and (char>= n #\A) (char<= n #\Z)) ; upper case
	     (and (char>= n #\a) (char<= n #\z)) ; lower case
	     (imember n '(#\_ #\%))
	     (imember n alphabet))))
; test for %, _, or other declared
					;    alphabetic characters.
  (defmfun ascii-numberp (num)
    (and (characterp num) (char<= num #\9) (char>= num #\0))))

 ;End of #-LISPM
 
;dbg:  ;;signals a conditition 'dbg:parse-ferror
;(DEFUN PARSE-FERROR (format-ctl-STRING &REST FORMAT-ARGS)
;  (ERROR 'PARSE-FERROR ':format-string FORMAT-ctl-STRING ':FORMAT-ARGS (COPY-LIST FORMAT-ARGS)))

(defvar *parse-window* nil)

(defun mread-synerr (sstring &rest l)
;  #+lispm (sys:parse-ferror    (format nil sstring l)  :correct-input )
;  #+lispm (dbg:parse-ferror    (format nil sstring l)  :correct-input )
  #+(or  nil) (apply #'error #+lispm nil #+nil ':read-error sstring l)
  #-(or lispm nil)
  (progn 
    (let (tem 
	  errset
	  (file "stdin"))
      (errset
       (setq tem (file-position *parse-stream*))
       (setq file  (namestring *parse-stream*)))
      (cond (tem (format t "~%~a:~a:"  file  tem))
	    (t ;(terpri)
	       ))
      (format t "Incorrect syntax: ")
      (apply 'format t sstring (mapcar #'(lambda (x)
					   (if (symbolp x)
					       (print-invert-case x)
					       x))
				       l))
      (cond ((output-stream-p *standard-input*)
	     (let ((n (get '*parse-window* 'length))
		   some ch
		   k
		   )
	       (loop for   i below 20
		      while (setq ch (nth (- n i 1) *parse-window*))
					  
		      do
		      (cond ((eql ch #\newline)
			     (push #\n some)
			     (push #\\ some))
			    ((eql ch #\tab)
			     (push #\t some)
			     (push #\\ some))
			    (t (push ch some))))
	       (setq k (length some))
	       (setq some (append some
				  (loop for i below 20 for tem =
					 nil 
					 ;(read-char-no-hang)
					 while tem collect tem)))
	       (terpri)
	       (loop for v in some do (princ v))
	       (terpri)
	       (loop for i from 2 below k do (princ #\space))
	       (princ "^")
	       
	       ;(loop while (read-char-no-hang) )
           (read-line *parse-stream* nil nil)
	       )))
      (terpri)
      (throw-macsyma-top) 
      )
    ))





 
;;; (FIXNUM-CHAR-UPCASE c)
;;;
;;;  If its argument, which must be a fixnum, represents a lowercase 
;;;  character, the uppercase representation of that character is returned.
;;;  Otherwise, it returns its argument.

#+cl
(defun fixnum-char-upcase (c)
  (char-upcase c))

;  (char-code (char-upcase (code-char c))))


(defun firstcharn (x)
  (aref (string x) 0))

(defvar *parse-stream*		()	  "input stream for Maxima parser")
(defvar macsyma-operators	()	  "Maxima operators structure")
(defvar *mread-prompt*		nil	  "prompt used by `mread'")
(defvar *mread-eof-obj* () "Bound by `mread' for use by `mread-raw'")

(defun tyi-parse-int (stream eof)
  (or *parse-window*
      (progn (setq *parse-window* (make-list 25))
	     (setf (get '*parse-window* 'length) (length *parse-window*))
	     (nconc *parse-window* *parse-window*)))
  (let ((tem (tyi stream eof)))
    (setf (car *parse-window*) tem *parse-window*
	  (cdr *parse-window*))
    (if (eql tem #\newline) (newline stream #\newline))
    tem))



;; We keep our own look-ahead state variable because the end-of-expression
;; is always a single character, and there is never need to UNTYI. --WRONG--wfs

;(DEFVAR PARSE-TYIPEEK () "T if there is a peek character.")
;(DEFVAR PARSE-TYI     () "The peek character.")
;
;(DEFUN PARSE-TYIPEEK ()
;  (COND (PARSE-TYIPEEK PARSE-TYI)
;	('ELSE
;	 (SETQ PARSE-TYIPEEK T)
;	 (SETQ PARSE-TYI (tyi-parse-int *PARSE-STREAM* -1)))))


;(DEFUN PARSE-TYI ( &aux answ)
;  (setq answ(COND (PARSE-TYIPEEK
;	 (SETQ PARSE-TYIPEEK ())
;	 PARSE-TYI)
;	('ELSE
;	 (TYI *PARSE-STREAM* -1))))
;  (princ answ) answ)
;
;
;(DEFUN PARSE-TYI ()
;  (COND (PARSE-TYIPEEK
;	 (SETQ PARSE-TYIPEEK ())
;	 PARSE-TYI)
;	('ELSE
;	 (tyi-parse-int *PARSE-STREAM* -1)
;	 )))




(defun *mread-prompt* (out-stream char)
  char
  (format out-stream "~&~A" *mread-prompt*))
  
(defun aliaslookup (op)
  (if (symbolp op)
      (or (get op 'alias) op)
      op))


;;;; Tokenizing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                    ;;;;;
;;;;;                      The Input Scanner                             ;;;;;
;;;;;                                                                    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gobble whitespace, recognize '#' comments..
(defun gobble-whitespace ( &aux saw-newline ch saw-other)
  (do () (nil) ; Gobble whitespace
      (setq ch (parse-tyipeek))
      (cond ((eql ch #\newline)
	     (setq saw-other nil)
	     (setq saw-newline t))
	    ((imember ch
		  '(#\tab #\space #\linefeed #\return ;#\control-C
			  #\page))
	     (setq saw-other t))
	    ;; allow comments to be lines which are whitespace and then
	    ;; a '#' character.
	    ;; recognize
	    ;; # 234 "jim.mac"
	    ;; to set the current line information to be line 234 of jim.mac
	    ((and (eql ch #\#) saw-newline)
	     (let ((li (read-line *parse-stream* nil)))
	       (declare (type (vector #.(array-element-type "a")) li))
	       (unread-char #\newline  *parse-stream*)
	       (setq parse-tyipeek nil)
	       (if (not saw-other) (grab-line-number li *parse-stream*))))
	    (t  (return t)))
     (parse-tyi)
     ))

(defun read-command-token (obj)
  (gobble-whitespace)
  (read-command-token-aux obj))

(defun ch-minusp (z)
  (and (numberp z) (< z 0)))

(defun safe-assoc (item lis)
  "maclisp would not complain about (car 3) it gives nil"
  (loop for v in lis
	when (and (consp v)
		  (equal (car v) item))
	do
	(return v)))
;
;(DEFUN READ-COMMAND-TOKEN-AUX (OBJ)
;  (IF (NOT (CDDR OBJ))
;      (CADADR OBJ)
;      (LET ((C (PARSE-TYIPEEK)))
;	#-cl(DECLARE (FIXNUM C))
;	(IF  #+cl (not( ch-minusp c))
;             #-cl
;	  (NOT (MINUSP C))
;	    (LET ((ANSWER (OR (safe-ASSOC C (CDDR OBJ)) (and (listp obj)(listp (cdr obj))
;							     (CADR OBJ)))))
;	      (IF (EQ (and (listp answer)(CAR ANSWER)) 'ANS)
;		  (CADR ANSWER)
;		  (PARSE-TYI)
;		  (READ-COMMAND-TOKEN-AUX ANSWER)))))))


;(setq macsyma-operators '(NIL (ANS NIL) (#\a #\b #\c (ANS |$abc|))     (#\e (ANS |$e|) (#\f (ANS |$ef|) (#\g (ANS |$efg|))))     (#\; (ANS |$;|))))		      
;;(NIL (ANS NIL) (#\a #\b #\c (ANS |$abc|))
;;     (#\e (ANS |$e|) (#\f (ANS |$ef|) (#\g (ANS |$efg|)))))


;; list contains an atom, only check
;; (parser-assoc 1 '(2 1 3)) ==>(1 3)
;; (parser-assoc 1 '(2 (1 4) 3)) ==>(1 4)

(defun parser-assoc (c lis )
  (loop for v on lis
	 do
	 (cond ((consp (car v))
		(if (eq (caar v) c)
		    (return (car v))))
	       ((eql (car v) c)
		(return v)))))

;; we need to be able to unparse-tyi an arbitrary number of
;; characters, since if you do
;; PREFIX("ABCDEFGH");
;; then ABCDEFGA should read as a symbol.
;; 99% of the time we dont have to unparse-tyi, and so there will
;; be no consing...

(defvar *parse-tyi* nil)
(defun parse-tyi ()
  (let ((tem  *parse-tyi*))
    (cond ((null tem)
	   (tyi-parse-int *parse-stream* -1))
	  ((atom tem)
	   (setq *parse-tyi* nil)
	   tem)
	  (t ;;consp
	   (setq *parse-tyi* (cdr tem))
	   (car tem)))))

;; read one character but leave it there. so next parse-tyi gets it
(defun parse-tyipeek ()
  (let ((tem  *parse-tyi*))
    (cond ((null tem)
	   (setq *parse-tyi* (tyi-parse-int *parse-stream* -1)))
	  ((atom tem) tem)
	  (t (car tem)))))

;; push characters back on the stream
(defun unparse-tyi (c)
  (let ((tem  *parse-tyi*))
    (cond ((null tem)
	   (setq *parse-tyi* c))
	  (t (setq *parse-tyi* (cons c tem))))))


;;I know that the tradition says there should be no comments
;;in tricky code in maxima.  However the operator parsing
;;gave me a bit of trouble.   It was incorrect because
;;it could not handle things produced by the extensions
;;the following was broken for prefixes 


(defun read-command-token-aux (obj)
  (let* (result
	 (ch (parse-tyipeek))
	 (lis (if (eql ch -1)
		  nil
		  (parser-assoc ch
				obj))))
    (cond ((null lis) 
	   nil)
	  (t
	   (parse-tyi)
	   (cond ((atom (cadr lis))
		  ;; INFIX("ABC"); puts into macsyma-operators
		  ;;something like: (#\A #\B #\C (ANS |$ABC|))
		  ;; ordinary things are like:
		  ;; (#\< (ANS $<) (#\= (ANS $<=)))
		  ;; where if you fail at the #\< #\X
		  ;; stage, then the previous step was permitted.
		  (setq result (read-command-token-aux (list (cdr lis) ))))
		 ((null (cddr lis))
		  ;; lis something like (#\= (ANS $<=))
		  ;; and this says there are no longer operators
		  ;; starting with this.
		  (setq result
			(and (eql (car (cadr lis)) 'ans)
			     (cadr (cadr lis)))))
		 (t
		  (let ((res   (and (eql (car (cadr lis)) 'ans)
				    (cadr (cadr lis))))
			(com-token (read-command-token-aux (cddr lis) )))
		    (setq result (or com-token res 
				     (read-command-token-aux
				      (list (cadr lis))))))
		  ))
	     (or result (unparse-tyi ch))
	     result))))


(defun scan-macsyma-token ()
  ;; note that only $-ed tokens are GETALIASed.
  (let ((tem (cons '#\$ (scan-token t))))
    (setq tem (bothcase-implode tem))
  (getalias tem)))

(defun scan-lisp-token ()
  (let ((charlist (scan-token nil)))
    (if (setq charlist (lisp-token-fixup-case charlist))
	(implode charlist)
	(mread-synerr "Lisp symbol expected."))))

;; Example: ?mismatch(x+y,x*z,?:from\-end,true); => 3
(defun scan-keyword-token ()
  (let ((charlist (cdr (scan-token nil))))
    (if (and charlist
	     (setq charlist (lisp-token-fixup-case charlist)))
	(let ((*package* (find-package "KEYWORD")))
	  (implode charlist))
	(mread-synerr "Lisp keyword expected."))))

;; The vertical bar | switches between preserving or folding case,
;; except that || is a literal |.

;; Note that this function modifies LIST destructively.
#+nil
(defun lisp-token-fixup-case (list)
  (let* ((list (cons nil list))
	 (todo list)
	 preserve)
    (loop
       (unless (cdr todo)
	 (return (cdr list)))
       (cond
	 ((char/= (cadr todo) #\|)
	  (pop todo)
	  (unless preserve
	    (setf (car todo)
		  (char-upcase (car todo)))))
	 ((setf (cdr todo) (cddr todo))
	  (if (char= (cadr todo) #\|)
	      (pop todo)
	      (setq preserve (not preserve))))))))

(defun lisp-token-fixup-case (list)
  list)

(defun scan-token (flag)
  (do ((c (parse-tyipeek) (parse-tyipeek))
       (l () (cons c l)))
      ((and flag (not (or (ascii-numberp c) (alphabetp c) (char= c #.back-slash-char)))) ;;#/\
       (nreverse (or l (ncons (parse-tyi))))) ; Read at least one char ...
    (if (char= (parse-tyi) #. back-slash-char);; #/\
	(setq c (parse-tyi)))
    (setq flag t)))

(defun scan-lisp-string ()
  (intern (scan-string)))

(defun scan-macsyma-string ()
  (intern-invert-case (scan-string #\&)))

(defun scan-string (&optional init)
  (let ((buf (or *scan-string-buffer*
		 (setq *scan-string-buffer*
		       (make-array 50 :element-type ' #.(array-element-type "abc")
				   :fill-pointer 0 :adjustable t))))
	(*scan-string-buffer* nil))
    (setf (fill-pointer buf) 0)
    (when init (vector-push-extend (coerce init 'character) buf))
    (do ((c (parse-tyipeek) (parse-tyipeek)))
	((cond ((eql c -1))
	       ((char= c #. double-quote-char)
		(parse-tyi) t))
	 (copy-seq buf))
      (if (char= (parse-tyi) #. back-slash-char) ;; #/\ )
	  (setq c (parse-tyi)))
      #-cl
      (vector-push-extend (code-char c) buf)
      #+cl
      (vector-push-extend c  buf)
      )))

(defvar *string-register* (make-array 100 :fill-pointer 0 :adjustable t :element-type '#.(array-element-type "a")))
(defun readlist (lis)
  (setf (fill-pointer *string-register*) 0)
  (loop for u in lis do (vector-push-extend u *string-register*))
  (read-from-string   *string-register*))


(defun make-number (data)
  (setq data (nreverse data))
  ;; Maxima really wants to read in any number as a double-float
  ;; (except when we have a bigfloat, of course!).  So convert an E or
  ;; S exponent marker to D.
  (when (member (car (nth 3. data)) '(#\E #\S))
    (setf (nth 3. data) (list #\D)))
  (if (not (equal (nth 3. data) '(#\B)))
      (readlist (apply #'append data))
      ;; For bigfloats, turn them into rational numbers then convert to bigfloat.
      ;; Fix for the 0.25b0 # 2.5b-1 bug.  Richard J. Fateman posted this fix to the 
      ;; Maxima list on 10 October 2005.  Without this fix, some tests in rtestrationalize
      ;; will fail.  Used with permission.
      ($bfloat (simplifya `((mtimes) ((mplus) ,(readlist (or (first data) '(#\0)))
				    ((mtimes) ,(readlist (or (third data) '(#\0)))
				     ((mexpt) 10. ,(f- (length (third data))))))
			  ((mexpt) 10. ,(funcall (if (char= (first (fifth data)) #\-) #'- #'+)
						 (readlist (sixth data))))) nil))))

;; Richard J. Fateman wrote the big float to rational code and the function 
;; cl-rat-to-maxmia.  

(defun cl-rat-to-maxima (x) (if (integerp x) x (list '(rat simp) (numerator x) (denominator x))))

(defun scan-digits (data continuation? continuation &optional exponent-p)
  (do ((c (parse-tyipeek) (parse-tyipeek))
       (l () (cons c l)))
      ((not (ascii-numberp c))
       (cond ((imember c continuation?)
	      (funcall continuation (list* (ncons (fixnum-char-upcase
						   (parse-tyi)))
					   (nreverse l)
					   data)
				   ))
	     ((and (null l) exponent-p)
	      ;; We're trying to parse the exponent part of a number,
	      ;; and we didn't get a value after the exponent marker.
	      ;; That's an error.
	      (merror "Incomplete number.  Missing exponent?"))
	     (t
	      (make-number (cons (nreverse l) data)))))
    (parse-tyi)))

;#+nil
;(DEFUN SCAN-NUMBER-BEFORE-DOT (DATA)
;  (SCAN-DIGITS DATA '(#. period-char) #'SCAN-NUMBER-AFTER-DOT))

(defun scan-number-after-dot (data)
  (scan-digits data '(#\E #\e #\B #\b #\D #\d #\S #\s) #'scan-number-exponent))

(defun scan-number-exponent (data)
  (push (ncons (if (or (char= (parse-tyipeek) #\+)
		       (char= (parse-tyipeek) #\-))
		   (parse-tyi)
		   #\+))
	data)
  (scan-digits data () () t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                    ;;;;;
;;;;;                    The Expression Parser                           ;;;;;
;;;;;                                                                    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                      ;;;
;;;	Based on a theory of parsing presented in:                       ;;;
;;;                                                                      ;;;
;;;	    Pratt, Vaughan R., ``Top Down Operator Precedence,''         ;;;
;;;	    ACM Symposium on Principles of Programming Languages         ;;;
;;;	    Boston, MA; October, 1973.                                   ;;;
;;;                                                                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;	Implementation Notes ....
;;;
;;;	JPG	Chars like ^A, ^B, ... get left around after interrupts and
;;;		should be thrown away by the scanner if not used as editting
;;;		commands.
;;;
;;;	KMP	There is RBP stuff in DISPLA, too. Probably this sort of
;;;		data should all be in one place somewhere.
;;;	
;;;	KMP	Maybe the parser and/or scanner could use their own GC scheme 
;;;		to recycle conses used in scan/parse from line to line which 
;;;		really ought not be getting dynamically discarded and reconsed.
;;;	        Alternatively, we could call RECLAIM explicitly on certain 
;;;		pieces of structure which get used over and over. A 
;;;		local-reclaim abstraction may want to be developed since this
;;;		stuff will always be needed, really. On small-address-space
;;;		machines, this could be overridden when the last DYNAMALLOC 
;;;		GC barrier were passed (indicating that space was at a premium
;;;		-- in such case, real RECLAIM would be more economical -- or 
;;;		would the code to control that be larger than the area locked 
;;;		down ...?)
;;;
;;;	KMP	GJC has a MAKE-EVALUATOR type package which could probably
;;;		replace the CALL-IF-POSSIBLE stuff used here.
;;;             [So it was written, so it was done. -gjc]
;;;
;;;	KMP	DEFINE-SYMBOL and KILL-OPERATOR need to be redefined.
;;;		Probably these shouldn't be defined in this file anyway.
;;;
;;;	KMP	The relationship of thisfile to SYNEX needs to be thought
;;;		out more carefully.
;;;
;;;	GJC	Need macros for declaring INFIX, PREFIX, etc ops
;;;
;;;	GJC	You know, PARSE-NARY isn't really needed it seems, since 
;;;		the SIMPLIFIER makes the conversion of
;;;			((MTIMES) ((MTIMES) A B) C) => ((MTIMES) A B C)
;;;		I bet you could get make "*" infix and nobody would 
;;;		ever notice.

;;; The following terms may be useful in deciphering this code:
;;;
;;; NUD -- NUll left Denotation (op has nothing to its left (prefix))
;;; LED -- LEft Denotation	(op has something to left (postfix or infix))
;;;
;;; LBP -- Left Binding Power  (the stickiness to the left)
;;; RBP -- Right Binding Power (the stickiness to the right)
;;;

;;;; Macro Support

;; "First character" and "Pop character"

(defvar scan-buffered-token (list nil)
  "put-back buffer for scanner, a state-variable of the reader")

(defun peek-one-token ()
  (peek-one-token-g nil nil))

(defun peek-one-token-g (eof-ok? eof-obj)
  (cond
   ((car scan-buffered-token)
    (cdr scan-buffered-token))
   (t (rplacd scan-buffered-token (scan-one-token-g eof-ok? eof-obj))
      (cdr (rplaca scan-buffered-token t)))))

(defun scan-one-token ()
  (scan-one-token-g nil nil))

(defun scan-one-token-g (eof-ok? eof-obj)
  (cond ((car scan-buffered-token)
	 (rplaca scan-buffered-token ())
	 (cdr scan-buffered-token))
	((read-command-token macsyma-operators))
	(t
	 (let ((test (parse-tyipeek)))
	   (cond  ((eql test -1.)
		   (parse-tyi)
		   (if eof-ok? eof-obj
		       (maxima-error "End of file while scanning expression")))
		  ((eql test forward-slash-char) ;;#//)
		   (parse-tyi)
		   (cond ((char= (parse-tyipeek) #\*)
			  (gobble-comment)
			  (scan-one-token-g eof-ok? eof-obj))
			 (t '#-cl $// #+cl $/ )))
		  ((eql test #. period-char) (parse-tyi)	; Read the dot
		   (if (ascii-numberp (parse-tyipeek))
		       (scan-number-after-dot (list (ncons #. period-char) nil))
		       '|$.|))
		  ((eql test double-quote-char );;#/")
		   (parse-tyi)
		   (scan-macsyma-string))
		  ((eql test #\?)
		   (parse-tyi)
		   (cond ((char= (parse-tyipeek) double-quote-char );;#/")
			  (parse-tyi)
			  (scan-lisp-string))
			 ((char= (parse-tyipeek) #\:)
			  (scan-keyword-token))
			 (t
			  (scan-lisp-token))))
		  (t
		   (if (ascii-numberp test)
		       (scan-number-before-dot ())
		       (scan-macsyma-token))))))))

;; nested comments are permitted.
(defun gobble-comment ()
  (prog (c depth)
	(setq depth 1)
     read
	(setq c (parse-tyipeek))
	(parse-tyi)
	(cond ((= depth 0) (return t)))
	(cond ((and (numberp c) (< c 0))(error "end of file in comment"))
	      ((char= c #\*)
	       (cond ((char= (parse-tyipeek) #. forward-slash-char)
		      (decf depth) 
		      (parse-tyi)
		      (cond ((= depth 0) (return t)))
		      (go read))))
	      ((char= c #.forward-slash-char)
	       (cond ((char= (parse-tyipeek) #\*) 
		      (incf depth) (parse-tyi)
		      (go read)))))
        (go read))
  )

(defun scan-number-rest (data)
  (let ((c (caar data)))
    (cond ((imember c '(#. period-char))
	   ;; We found a dot
	   (scan-number-after-dot data))
	  ((imember c '(#\E #\e #\B #\b #\D #\d #\S #\s))
	   ;; Dot missing but found exponent marker.  Fake it.
	   (setf data (push (ncons #\.) (rest data)))
	   (push (ncons #\0) data)
	   (push (ncons c) data)
	   (scan-number-exponent data)))))

(defun scan-number-before-dot (data)
  (scan-digits data '(#. period-char #\E #\e #\B #\b #\D #\d #\S #\s)
	       #'scan-number-rest))



(defmacro first-c () '(peek-one-token))
(defmacro pop-c   () '(scan-one-token))


(defun mstringp (x)
  (and (symbolp x) (char= (firstcharn x) #\&)))


;(DEFUN AMPERCHK (NAME)
;  (IF (MSTRINGP NAME) (DOLLARIFY-NAME NAME) NAME))
;;see suprv1

(defun inherit-propl (op-to op-from getl)
  (let ((propl (getl op-from getl)))
    (if propl
	(progn (remprop op-to (car propl))
	       (putprop op-to (cadr propl) (car propl)))
	(inherit-propl op-to
		       (maxima-error (list "has no" getl "properties.")
			      op-from
			      'wrng-type-arg)
		       getl))))


;;; (NUD <op>)
;;; (LED <op> <left>)
;;;
;;;  <op>   is the name of the operator which was just popped.
;;;  <left> is the stuff to the left of the operator in the LED case.
;;;

(eval-when (eval compile load)
  #+already-expanded-below
  (def-propl-call nud (op)
    (if (operatorp op)
	;; If first element is an op, it better have a NUD
	(mread-synerr "~A is not a prefix operator" (mopstrip op))
	;; else take it as is.
	(cons '$any op)))
;;begin expansion
  (defmacro def-nud-equiv (op equiv)
    (list 'putprop (list 'quote op) (list 'function equiv)
          (list 'quote 'nud)))
  (defmacro nud-propl () ''(nud))
  (defmacro def-nud-fun (op-name op-l . body)
    (list* 'defun-prop (list* op-name 'nud 'nil) op-l body))
  (defun nud-call (op)
    (let ((tem (and (symbolp op) (getl op '(nud)))) res)
      (setq res	 
	    (if (null tem)
		(if (operatorp op)
		    (mread-synerr "~A is not a prefix operator"
				  (mopstrip op))
		    (cons '$any op))
		(funcall (cadr tem) op)))
      res))
;;end expansion 

;;following defines def-led-equiv led-propl def-led-fun led-call
  #+already-expanded-below
  (def-propl-call led (op l)
    (mread-synerr "~A is not an infix operator" (mopstrip op))))

;;begin expansion
(defmacro def-led-equiv (op equiv)
    (list 'putprop (list 'quote op) (list 'function equiv)
          (list 'quote 'led)))

(eval-when (compile load eval)
  (defmacro led-propl () ''(led)))

(defmacro def-led-fun (op-name op-l . body)
    (list* 'defun-prop (list* op-name 'led 'nil) op-l body))
(defun led-call (op l)
  (let ((tem (and (symbolp op) (getl op '(led)))) res)
    (setq res
	  (if (null tem)
	      (mread-synerr "~A is not an infix operator" (mopstrip op))
	      (funcall (cadr tem) op l)))
    res))

;;end expansion

;;; (DEF-NUD (op lbp rbp) bvl . body)
;;;
;;;  Defines a procedure for parsing OP as a prefix operator.
;;;
;;;  OP  should be the name of the symbol as a string or symbol.
;;;  LBP is an optional left  binding power for the operator.
;;;  RBP is an optional right binding power for the operator.
;;;  BVL must contain exactly one variable, which the compiler will not
;;;      complain about if unused, since it will rarely be of use anyway.
;;;      It will get bound to the operator being parsed.
;;;  lispm:Optional args not allowed in release 5 allowed, necessary afterwards..

(defmacro def-nud ((op . lbp-rbp) bvl . body)
  (let (( lbp (nth 0 lbp-rbp))
	( rbp (nth 1 lbp-rbp)))
    `(progn 'compile 	  ,(make-parser-fun-def op 'nud bvl body)
	    (set-lbp-and-rbp ',op ',lbp ',rbp))))

;#-cl
;(DEFMACRO DEF-NUD ((OP #+nil &OPTIONAL LBP RBP) BVL . BODY)
;  `(PROGN 'COMPILE 	  ,(MAKE-PARSER-FUN-DEF OP 'NUD BVL BODY)
;	  (SET-LBP-AND-RBP ',OP ',LBP ',RBP)))

(defun set-lbp-and-rbp (op lbp rbp)
  (cond ((not (consp op))
	 (let ((existing-lbp (get op 'lbp))
	       (existing-rbp (get op 'rbp)))
	   (cond ((not lbp)
		  (comment ignore omitted arg))
		 ((not existing-lbp)
		  (putprop op lbp 'lbp))
		 ((not (equal existing-lbp lbp))
		  (maxima-error "Incompatible LBP's defined for this operator" op)))
	   (cond ((not rbp)
		  (comment ignore omitted arg))
		 ((not existing-rbp)
		  (putprop op rbp 'rbp))
		 ((not (equal existing-rbp rbp))
		  (maxima-error "Incompatible RBP's defined for this operator" op)))))
	('else
	 (mapcar #'(lambda (x) (set-lbp-and-rbp x lbp rbp))
		 op))))
				   

;;; (DEF-LED (op lbp rbp) bvl . body)
;;;
;;;  Defines a procedure for parsing OP as an infix or postfix operator.
;;;
;;;  OP  should be the name of the symbol as a string or symbol.
;;;  LBP is an optional left  binding power for the operator.
;;;  RBP is an optional right binding power for the operator.
;;;  BVL must contain exactly two variables, the first of which the compiler
;;;       will not complain about if unused, since it will rarely be of use
;;;	  anyway. Arg1 will get bound to the operator being parsed. Arg2 will
;;;	  get bound to the parsed structure which was to the left of Arg1.


(defmacro def-led((op . lbp-rbp) bvl . body)
  (let (( lbp (nth 0 lbp-rbp))
	( rbp (nth 1 lbp-rbp)))
    `(progn 'compile
	    ,(make-parser-fun-def  op 'led bvl body)
	    (set-lbp-and-rbp ',op ',lbp ',rbp))))

;#-cl
;(DEFMACRO DEF-LED ((OP #+(or cl NIL) &OPTIONAL LBP RBP) BVL . BODY)
;  `(PROGN 'COMPILE
;	  ,(MAKE-PARSER-FUN-DEF  OP 'LED BVL BODY)
;	  (SET-LBP-AND-RBP ',OP ',LBP ',RBP)))

(defmacro def-collisions (op &rest alist)
  (let ((keys (do ((i  1.    (#+cl ash #-cl lsh i 1.))
		   (lis  alist (cdr lis))
		   (nl ()    (cons (cons (caar lis) i) nl)))
		  ((null lis) nl))))
    `(progn 'compile
       (defprop ,op ,(let #+lispm ((default-cons-area working-storage-area))
			  #-lispm nil
		       (copy-tree keys )) keys)
       ,@(mapcar #'(lambda (data)
		     `(defprop ,(car data)
			       ,(do ((i 0 (logior i  (cdr (assq (car lis) keys))))
				     (lis (cdr data) (cdr lis)))
				    ((null lis) i))
			       ,op))
		 alist))))


(defun collision-lookup (op active-bitmask key-bitmask)
  (let ((result (logand active-bitmask key-bitmask)))
    (if (not (zerop result))
	(do ((l (get op 'keys) (cdr l)))
	    ((null l) (parse-bug-err 'collision-check))
	  (if (not (zerop (logand result (cdar l))))
	      (return (caar l)))))))

(defun collision-check (op active-bitmask key)
  (let ((key-bitmask (get key op)))
    (if (not key-bitmask)
	(mread-synerr "~A is an unknown keyword in a ~A statement."
		      (mopstrip key) (mopstrip op)))
    (let ((collision (collision-lookup op active-bitmask key-bitmask)))
      (if collision
	  (if (eq collision key)
	      (mread-synerr "This ~A's ~A slot is already filled."
			    (mopstrip op)
			    (mopstrip key))
	      (mread-synerr "A ~A cannot have a ~A with a ~A field."
			    (mopstrip op)
			    (mopstrip key)
			    (mopstrip collision))))
      (logior (cdr (assq key (get op 'keys))) active-bitmask))))
      

;;;; Data abstraction

;;; LBP = Left Binding Power
;;;
;;; (LBP <op>)		 - reads an operator's Left Binding Power
;;; (DEF-LBP <op> <val>) - defines an operator's Left Binding Power

(defmfun lbp (lex) (cond ((safe-get lex 'lbp)) (t 200.)))

(defmacro def-lbp (sym val) `(defprop ,sym ,val lbp))

;;; RBP = Right Binding Power
;;;
;;; (RBP <op>)		 - reads an operator's Right Binding Power
;;; (DEF-RBP <op> <val>) - defines an operator's Right Binding Power

(defmfun rbp (lex) (cond ((safe-get lex 'rbp)) (t 200.)))

(defmacro def-rbp (sym val) `(defprop ,sym ,val rbp))

(defmacro def-match (x m) `(defprop ,x ,m match))

;;; POS = Part of Speech!
;;; 
;;; (LPOS <op>)
;;; (RPOS <op>)
;;; (POS  <op>)
;;;

(defun lpos (op) (cond ((safe-get op 'lpos)) (t '$any)))
(defun rpos (op) (cond ((safe-get op 'rpos)) (t '$any)))
(defun pos (op) (cond ((safe-get op 'pos)) (t '$any)))

(defmacro def-pos  (op pos) `(defprop ,op ,pos  pos))
(defmacro def-rpos (op pos) `(defprop ,op ,pos rpos))
(defmacro def-lpos (op pos) `(defprop ,op ,pos lpos))

;;; MHEADER

(defun mheader (op) (add-lineinfo (or (safe-get op 'mheader) (ncons op))))

(defmacro def-mheader (op header) `(defprop ,op ,header mheader))


(defmvar $parsewindow 10.
	 "The maximum number of 'lexical tokens' that are printed out on
each side of the error-point when a syntax (parsing) MAXIMA-ERROR occurs.  This
option is especially useful on slow terminals.  Setting it to -1 causes the
entire input string to be printed out when an MAXIMA-ERROR occurs."
	 fixnum)


;;;; Misplaced definitions

(defmacro def-operatorp ()
  `(defmfun operatorp (lex)
     (and (symbolp lex) (getl lex '(,@(nud-propl) ,@(led-propl))))))

(def-operatorp)

(defmacro def-operatorp1 ()
  ;Defmfun -- used by SYNEX if not others.
  `(defmfun operatorp1 (lex)
     ;; Referenced outside of package: OP-SETUP, DECLARE1
     ;; Use for truth value only, not for return-value.
     (and (symbolp lex) (getl lex '(lbp rbp ,@(nud-propl) ,@(led-propl))))))

(def-operatorp1)

;;;; The Macsyma Parser

;;; (MREAD) with arguments compatible with losing maclisp READ style.
;;;
;;; Returns a parsed form of tokens read from stream.
;;;
;;; If you want rubout processing, be sure to call some stream which knows
;;; about such things. Also, I'm figuring that the PROMPT will be
;;; an atribute of the stream which somebody can hack before calling
;;; MREAD if he wants to.

;#+Lispm
;(DEFUN READ-APPLY (F READ-ARGS &AUX WHICH-OPERS)
;  (MULTIPLE-VALUE-BIND (STREAM EOF)
;		       (SI:DECODE-READ-ARGS READ-ARGS)

;    (SETQ WHICH-OPERS (FUNCALL STREAM ':WHICH-OPERATIONS))
;    (IF (MEMQ ':RUBOUT-HANDLER WHICH-OPERS)
;	(FUNCALL STREAM ':RUBOUT-HANDLER '((:PROMPT *MREAD-PROMPT*))
;		 F STREAM EOF)
;	(FUNCALL F STREAM EOF))))

;#+Maclisp
;(DEFUN READ-APPLY (F READ-ARGS &AUX WHICH-OPERS)
;  (LET ((STREAM (CAR READ-ARGS))
;	(EOF (CADR READ-ARGS)))
;    ;; apply the correction.
;    (COND ((AND (NULL (CDR READ-ARGS))
;		(NOT (OR (EQ STREAM T)
;			 (SFAP STREAM)
;			 (FILEP STREAM))))
;	   (SETQ STREAM NIL EOF STREAM)))
;    (COND ((EQ STREAM T)
;	   (SETQ STREAM TYI))
;	  ((EQ STREAM NIL)
;	   (IF ^Q (SETQ STREAM INFILE) (SETQ STREAM TYI))))
;    (SETQ WHICH-OPERS (AND (SFAP STREAM)
;			   (SFA-CALL STREAM 'WHICH-OPERATIONS NIL)))
;    (IF (MEMQ 'RUBOUT-HANDLER WHICH-OPERS)
;	(SFA-CALL STREAM 'RUBOUT-HANDLER F)
;	(FUNCALL F STREAM EOF))))

(defvar *current-line-info* nil)

;;Important for lispm rubout handler
(defun mread (&rest read-args)
  #+nil (let ((*mread-prompt-internal* *mread-prompt*)
	      (si:*ttyscan-dispatch-table *macsyma-ttyscan-operators*))
	  (declare (special *mread-prompt-internal*))
	  (si:read-apply ':mread #'mread-raw (coerce read-args 'sys:vector)
			 '(:prompt mread-prompter)
			 '(:reprompt mread-prompter)))
  #+cl (progn
	 (when *mread-prompt*
	       (and *parse-window* (setf (car *parse-window*) nil
					 *parse-window* (cdr *parse-window*)))
	       (princ *mread-prompt*)
	       (force-output))
	 (apply 'mread-raw read-args)
		    )
  #-(or nil cl)
  (read-apply #'mread-raw read-args))

(defun mread-prompter (stream char)
  (declare (special *mread-prompt-internal*))
  char ;  (declare (ignore char))
  (fresh-line stream)
  (princ *mread-prompt-internal* stream))

#+nil
(defun mread-with-prompt (prompt)
  (let ((*mread-prompt-internal* prompt)
	(si:*ttyscan-dispatch-table *macsyma-ttyscan-operators*))
    (declare (special *mread-prompt-internal*))
    (si:read-apply ':mread #'mread-raw (sys:vector)
		   '(:prompt mread-prompter)
		   '(:reprompt mread-prompter))))

;; input can look like:
;;aa && bb && jim:3;


(defun mread-raw (*parse-stream* &optional *mread-eof-obj*)
  (let ((scan-buffered-token (list nil))
	*parse-tyi*
	)
    (if (eq scan-buffered-token ;; a handly unique object for the EQ test.
	    (peek-one-token-g t scan-buffered-token))
	*mread-eof-obj*
	(do ((labels ())
	     (input (parse '$any 0.) (parse '$any 0.)))
	    (nil)
	  (case (first-c)
	    ((|$;| |$$|)
	      ;force a separate line info structure
	     (setf *current-line-info* nil)
	     (return (list (mheader (pop-c))
			   (if labels (cons (mheader '|$[|) (nreverse labels)))
			   input)))
	    ((|$&&|)
	     (pop-c)
	     (if (symbolp input)
		 (push input labels)
		 (mread-synerr "Invalid && tag. Tag must be a symbol")))
	    (t
	     (parse-bug-err 'mread-raw)))))))

;;; (PARSE <mode> <rbp>)
;;;
;;;  This will parse an expression containing operators which have a higher
;;;  left binding power than <rbp>, returning as soon as an operator of
;;;  lesser or equal binding power is seen. The result will be in the given
;;;  mode (which allows some control over the class of result expected). 
;;;  Modes used are as follows:
;;;	$ANY    = Match any type of expression
;;;	$CLAUSE = Match only boolean expressions (or $ANY)
;;;	$EXPR   = Match only mathematical expressions (or $ANY)
;;;  If a mismatched mode occurs, a syntax error will be flagged. Eg,
;;;  this is why "X^A*B" parses but "X^A and B" does not. X^A is a $EXPR
;;;  and not coercible to a $CLAUSE. See CONVERT.
;;;
;;;  <mode> is the required mode of the result.
;;;  <rbp>  is the right binding power to use for the parse. When an
;;;	     LED-type operator is seen with a lower left binding power
;;;	     than <rbp>, this parse returns what it's seen so far rather
;;;	     than calling that operator.
;;;

(defun parse (mode rbp) 
  (do ((left (nud-call (pop-c))		; Envoke the null left denotation
	     (led-call (pop-c) left)))	;  and keep calling LED ops as needed
      ((>= rbp (lbp (first-c)))		; Until next op lbp too low
       (convert left mode))))		;  in which case, return stuff seen

;;; (PARSE-PREFIX <op>)
;;;
;;;  Parses prefix forms -- eg, -X or NOT FOO.
;;;
;;;  This should be the NUD property on an operator. It fires after <op>
;;;  has been seen. It parses forward looking for one more expression
;;;  according to its right binding power, returning
;;;  ( <mode> . ((<op>) <arg1>) )

(defun parse-prefix (op)
  (list (pos op)			; Operator mode
	(mheader op)			; Standard Macsyma expression header
	(parse (rpos op) (rbp op))))	; Convert single argument for use

;;; (PARSE-POSTFIX <op> <left>)
;;;
;;;  Parses postfix forms. eg, X!.
;;;
;;;  This should be the LED property of an operator. It fires after <left>
;;;  has been accumulated and <op> has been seen and gobbled up. It returns
;;;  ( <mode> . ((<op>) <arg1>) )

(defun parse-postfix (op l)
  (list (pos op)			; Operator's mode
	(mheader op)			; Standard Macsyma expression header
	(convert l (lpos op))))		; Convert single argument for use

;;; (PARSE-INFIX <op> <left>)
;;;
;;;  Parses infix (non-nary) forms. eg, 5 mod 3.
;;;
;;;  This should be the led property of an operator. It fires after <left>
;;;  has been accumulated and <op> has been seen and gobbled up. It returns
;;;  ( <mode> . ((<op>) <arg1> <arg2>) )

(defun parse-infix (op l)
  (list (pos op)			; Operator's mode
	(mheader op)			; Standard Macsyma expression header
	(convert l (lpos op))		; Convert arg1 for immediate use
	(parse (rpos op) (rbp op))))	; Look for an arg2 

;;; (PARSE-NARY <op> <left>)
;;;
;;;  Parses nary forms. Eg, form1*form2*... or form1+form2+...
;;;  This should be the LED property on an operator. It fires after <op>
;;;  has been seen, accumulating and returning
;;;  ( <mode> . ((<op>) <arg1> <arg2> ...) )
;;;
;;;  <op>   is the being parsed.
;;;  <left> is the stuff that has been seen to the left of <op> which 
;;;         rightly belongs to <op> on the basis of parse precedence rules.

(defun parse-nary (op l)
  (list* (pos op)			    ; Operator's mode
	 (mheader op)			    ; Normal Macsyma operator header
	 (convert l (lpos op))		    ; Check type-match of arg1
	 (prsnary op (lpos op) (lbp op))))  ; Search for other args

;;; (PARSE-MATCHFIX <lop>)
;;;
;;;  Parses matchfix forms. eg, [form1,form2,...] or (form1,form2,...)
;;;
;;;  This should be the NUD property on an operator. It fires after <op>
;;;  has been seen. It parses <lop><form1>,<form2>,...<rop> returning
;;;  ( <mode> . ((<lop>) <form1> <form2> ...) ).

(defun parse-matchfix (op)
  (list* (pos op)			         ; Operator's mode
	 (mheader op)			         ; Normal Macsyma operator header
	 (prsmatch (safe-get op 'match) (lpos op))))  ; Search for matchfixed forms

;;; (PARSE-NOFIX <op>)
;;;
;;;  Parses an operator of no args. eg, @+X where @ designates a function
;;;  call (eg, @() is implicitly stated by the lone symbol @.)
;;;
;;;  This should be a NUD property on an operator which takes no args.
;;;  It immediately returns ( <mode> . ((<op>)) ).
;;;
;;;  <op> is the name of the operator.
;;;
;;;  Note: This is not used by default and probably shouldn't be used by 
;;;   someone who doesn't know what he's doing. Example lossage. If @ is 
;;;   a nofix op, then @(3,4) parses, but parses as "@"()(3,4) would -- ie, 
;;;   to ((MQAPPLY) (($@)) 3 4) which is perhaps not what the user will expect.

(defun parse-nofix (op) (list (pos op) (mheader op)))

;;; (PRSNARY <op> <mode> <rbp>)
;;;
;;;  Parses an nary operator tail Eg, ...form2+form3+... or ...form2*form3*...
;;;
;;;  Expects to be entered after the leading form and the first call to an 
;;;  nary operator has been seen and popped. Returns a list of parsed forms
;;;  which belong to that operator. Eg, for X+Y+Z; this should be called 
;;;  after the first + is popped. Returns (Y Z) and leaves the ; token
;;;  in the parser scan buffer.
;;;
;;;  <op>   is the nary operator in question.
;;;  <rbp>  is (LBP <op>) and is provided for efficiency. It is for use in
;;;	     recursive parses as a binding power to parse for.
;;;  <mode> is the name of the mode that each form must be.

(defun prsnary (op mode rbp) 
  (do ((nl (list (parse mode rbp))	   ; Get at least one form
	   (cons (parse mode rbp) nl)))	   ;  and keep getting forms
      ((not (eq op (first-c)))		   ; until a parse pops on a new op
       (nreverse nl))			   ;  at which time return forms
      (pop-c)))				   ; otherwise pop op

;;; (PRSMATCH <match> <mode>)
;;;
;;; Parses a matchfix sequence. Eg, [form1,form2,...] or (form1,form2,...)
;;; Expects to be entered after the leading token is the popped (ie, at the
;;;  point where the parse of form1 will begin). Returns (form1 form2 ...).
;;;
;;; <match> is the token to look for as a matchfix character.
;;; <mode>  is the name of the mode that each form must be.

(defun prsmatch (match mode)			  ; Parse for matchfix char
  (cond ((eq match (first-c)) (pop-c) nil)	  ; If immediate match, ()
	(t					  ; Else, ...
	 (do ((nl (list (parse mode 10.))	  ;  Get first element
		  (cons (parse mode 10.) nl)))	  ;   and Keep adding elements
	     ((eq match (first-c))		  ;  Until we hit the match.
	      (pop-c)				  ;   Throw away match.
	      (nreverse nl))			  ;   Put result back in order
	   (if (eq '|$,| (first-c))		  ;  If not end, look for ","
	       (pop-c)				  ;   and pop it if it's there
	       (mread-synerr "Missing ~A"	  ;   or give an error message.
			     (mopstrip match)))))))

;;; (CONVERT <exp> <mode>)
;;;
;;;  Parser coercion function.
;;;
;;;  <exp>  should have the form ( <expressionmode> . <expression> )
;;;  <mode> is the target mode.
;;;
;;;  If <expressionmode> and <mode> are compatible, returns <expression>.

(defun convert (item mode) 
  (if (or (eq mode (car item))		; If modes match exactly
	  (eq '$any mode)		;    or target is $ANY
	  (eq '$any (car item)))	;    or input is $ANY
      (cdr item)			;  then return expression
      (mread-synerr "Found ~A expression where ~A expression expected" 
		    (get (car item) 'english)
		    (get mode       'english))))

(defprop $any    "untyped"   english)
(defprop $clause "logical"   english)
(defprop $expr   "algebraic" english)

;;;; Parser Error Diagnostics

 ;; Call this for random user-generated parse errors

(defun parse-err () (mread-synerr "Syntax error")) 

 ;; Call this for random internal parser lossage (eg, code that shouldn't
 ;;  be reachable.)

(defun parse-bug-err (op)
  (mread-synerr
    "Parser bug in ~A. Please report this to the Maxima maintainers,~
   ~%including the characters you just typed which caused the error. Thanks."
    (mopstrip op)))

;;; Random shared error messages

(defun delim-err (op)
  (mread-synerr "Illegal use of delimiter ~A" (mopstrip op)))

(defun erb-err (op l) l ;Ignored
  (mread-synerr "Too many ~A's" (mopstrip op)))

(defun premterm-err (op)
  (mread-synerr "Premature termination of input at ~A."
		(mopstrip op)))

;;;; Operator Specific Data

(def-nud-equiv |$]| delim-err)
(def-led-equiv |$]| erb-err)
(def-lbp     |$]| 5.)

(def-nud-equiv	|$[| parse-matchfix)
(def-match	|$[| |$]|)
(def-lbp	|$[| 200.)
;No RBP
(def-mheader	|$[| (mlist))
(def-pos	|$[| $any)
(def-lpos	|$[| $any)
;No RPOS

(def-led (|$[| 200.) (op left)
  (setq left (convert left '$any))
  (if (numberp left) (parse-err))			; number[...] invalid
  (let ((header (if (atom left)
		    (add-lineinfo (list (amperchk left) 'array))
		  (add-lineinfo '(mqapply array))))
		  
	(right (prsmatch '|$]| '$any)))			; get sublist in RIGHT
    (cond ((null right)					; 1 subscript minimum
	   (mread-synerr "No subscripts given"))
	  ((atom left)					; atom[...]
	   (setq right (cons header
			     right))
	   (cons '$any (aliaslookup right)))
	  (t						; exp[...]
	   (cons '$any (cons header
			     (cons left right)))))))


(def-nud-equiv |$)| delim-err)
(def-led-equiv |$)| erb-err)
(def-lbp       |$)| 5.)

(def-mheader   |$(| (mprogn))

  ;; KMP: This function optimizes out (exp) into just exp. 
  ;;  This is useful for mathy expressions, but obnoxious for non-mathy
  ;;  expressions. I think DISPLA should be made smart about such things,
  ;;  but probably the (...) should be carried around in the internal 
  ;;  representation. This would make things like BUILDQ much easier to 
  ;;  work with.
  ;; GJC: CGOL has the same behavior, so users tend to write extensions
  ;;  to the parser rather than write Macros per se. The transformation
  ;;  "(EXP)" ==> "EXP" is done by the evaluator anyway, the problem
  ;;  comes inside quoted expressions. There are many other problems with
  ;;  the "QUOTE" concept however.

(def-nud (|$(| 200.) (op)
  (let ((right)(hdr (mheader '|$(|)))        ; make mheader first for lineinfo
    (cond ((eq '|$)| (first-c)) (parse-err))		  ; () is illegal
	  ((or (null (setq right (prsmatch '|$)| '$any))) ; No args to MPROGN??
	       (cdr right))				  ;  More than one arg.
	   (cons '$any (cons hdr right)))	  ; Return an MPROGN
	  (t (cons '$any (car right))))))		  ; Optimize out MPROGN

(def-led (|$(| 200.) (op left)
  (setq left (convert left '$any))		        ;De-reference LEFT
  (if (numberp left) (parse-err))			;number(...) illegal
  (let ((hdr (and (atom left)(mheader (amperchk left))))
	(r (prsmatch '|$)| '$any))                       ;Get arglist in R
	)
    (cons '$any						;Result is type $ANY
	  (cond ((atom left)				;If atom(...) =>
		 (cons hdr r))    ;(($atom) exp . args)
		(t				        ;Else exp(...) =>
		 (cons '(mqapply) (cons left r)))))))	;((MQAPPLY) op . args)

(def-mheader |$'| (mquote))

(def-nud (|$'|) (op)
  (let (right)
    (cond ((eq '|$(| (first-c))
	   (list '$any (mheader '|$'|) (parse '$any 190.)))
	  ((or (atom (setq right (parse '$any 190.)))
	       (memq (caar right) '(mquote mlist mprog mprogn lambda)))
	   (list '$any (mheader '|$'|) right))
	  ((eq 'mqapply (caar right))
	   (cond ((eq (caaadr right) 'lambda)
		  (list '$any (mheader '|$'|) right))
		 (t (rplaca (cdr right)
			    (cons (cons ($nounify (caaadr right))
					(cdaadr right))
				  (cdadr right)))
		    (cons '$any right))))
	  (t (cons '$any (cons (cons ($nounify (caar right)) (cdar right))
			       (cdr right)))))))

(def-nud (|$''|) (op)
  (let (right)
    (cons '$any
	  (cond ((eq '|$(| (first-c))  (meval (parse '$any 190.)))
		((atom (setq right (parse '$any 190.))) (meval1 right))
		((eq 'mqapply (caar right))
		 (rplaca (cdr right)
			 (cons (cons ($verbify (caaadr right)) (cdaadr right))
			       (cdadr right)))
		 right)
		(t (cons (cons ($verbify (caar right)) (cdar right))
			 (cdr right)))))))

(def-led-equiv |$:| parse-infix)
(def-lbp       |$:| 180.)
(def-rbp       |$:|  20.)
(def-pos       |$:| $any)
(def-rpos      |$:| $any)
(def-lpos      |$:| $any)
(def-mheader   |$:| (msetq))

(def-led-equiv |$::| parse-infix)
(def-lbp       |$::| 180.)
(def-rbp       |$::|  20.)
(def-pos       |$::| $any)
(def-rpos      |$::| $any)
(def-lpos      |$::| $any)
(def-mheader   |$::| (mset))

(def-led-equiv |$:=| parse-infix)
(def-lbp       |$:=| 180.)
(def-rbp       |$:=|  20.)
(def-pos       |$:=| $any)
(def-rpos      |$:=| $any)
(def-lpos      |$:=| $any)
(def-mheader   |$:=| (mdefine))

(def-led-equiv |$::=| parse-infix)
(def-lbp       |$::=| 180.)
(def-rbp       |$::=|  20.)
(def-pos       |$::=| $any)
(def-rpos      |$::=| $any)
(def-lpos      |$::=| $any)
(def-mheader   |$::=| (mdefmacro))

(def-led-equiv	|$!| parse-postfix)
(def-lbp	|$!| 160.)
;No RBP
(def-pos	|$!| $expr)
(def-lpos	|$!| $expr)
;No RPOS
(def-mheader	|$!| (mfactorial))

(def-mheader |$!!| ($genfact))

(def-led (|$!!| 160.) (op left)
  (list '$expr
	(mheader '$!!)
	(convert left '$expr)
	(list (mheader '#-cl $// #+cl $/ ) (convert left '$expr) 2)
	2))

(def-lbp     |$^| 140.) 
(def-rbp     |$^| 139.)
(def-pos     |$^| $expr)
(def-lpos    |$^| $expr)
(def-rpos    |$^| $expr)
(def-mheader |$^| (mexpt))

(def-led ((|$^| |$^^|)) (op left)
  (cons '$expr 
	(aliaslookup (list (mheader op)
			   (convert left (lpos op))
			   (parse (rpos op) (rbp op))))))

(mapc #'(lambda (prop) ; Make $** like $^
	  (let ((propval (get '$^ prop)))
	    (if propval (putprop '$** propval prop))))
      '(lbp rbp pos rpos lpos mheader))
(inherit-propl  '$** '$^ (led-propl))

(def-lbp     |$^^| 140.)
(def-rbp     |$^^| 139.)
(def-pos     |$^^| $expr)
(def-lpos    |$^^| $expr)
(def-rpos    |$^^| $expr)
(def-mheader |$^^| (mncexpt))

;; note y^^4.z gives an error because it scans the number 4 together with
;; the trailing '.' as a decimal place.    I think the error is correct.
(def-led-equiv	|$.| parse-infix)
(def-lbp	|$.| 130.)
(def-rbp	|$.| 129.)
(def-pos	|$.| $expr)
(def-lpos	|$.| $expr)
(def-rpos	|$.| $expr)
(def-mheader	|$.| (mnctimes))

(def-led-equiv	|$*| parse-nary)
(def-lbp	|$*| 120.)
;RBP not needed
(def-pos	|$*| $expr)
;RPOS not needed
(def-lpos	|$*| $expr)
(def-mheader	|$*| (mtimes))

(def-led-equiv	#-cl |$//| #+cl $/  parse-infix)
(def-lbp	#-cl |$//| #+cl $/  120.)
(def-rbp	#-cl |$//| #+cl $/  120.)
(def-pos	#-cl |$//| #+cl $/  $expr)
(def-rpos	#-cl |$//| #+cl $/  $expr)
(def-lpos	#-cl |$//| #+cl $/  $expr)
(def-mheader	#-cl |$//| #+cl $/  (mquotient))

(def-nud-equiv	|$+| parse-prefix)
(def-lbp	|$+| 100.)
(def-rbp	|$+| 100.)
(def-pos	|$+| $expr)
(def-rpos	|$+| $expr)
;LPOS not needed
(def-mheader	|$+| (mplus))

(def-led ((|$+| |$-|) 100.) (op left)
  (setq left (convert left '$expr))
  (do ((nl (list (if (eq op '$-)
		     (list (mheader '$-) (parse '$expr 100.))
		     (parse '$expr 100.))
		 left)
	   (cons (parse '$expr 100.) nl)))
      ((not (memq (first-c) '($+ $-)))
       (list* '$expr (mheader '$+) (nreverse nl)))
    (if (eq (first-c) '$+) (pop-c))))

(def-nud-equiv	|$-| parse-prefix)
(def-lbp	|$-| 100.)
(def-rbp	|$-| 134.)
(def-pos	|$-| $expr)
(def-rpos	|$-| $expr)
;LPOS not needed
(def-mheader	|$-| (mminus))

(def-led-equiv	|$=| parse-infix)
(def-lbp	|$=| 80.)
(def-rbp	|$=| 80.)
(def-pos	|$=| $clause)
(def-rpos	|$=| $expr)
(def-lpos	|$=| $expr)
(def-mheader	|$=| (mequal))

(def-led-equiv	|$#| parse-infix)
(def-lbp	|$#| 80.)
(def-rbp	|$#| 80.)
(def-pos	|$#| $clause)
(def-rpos	|$#| $expr)
(def-lpos	|$#| $expr)
(def-mheader	|$#| (mnotequal))

(def-led-equiv	|$>| parse-infix)
(def-lbp	|$>| 80.)
(def-rbp	|$>| 80.)
(def-pos	|$>| $clause)
(def-rpos	|$>| $expr)
(def-lpos	|$>| $expr)
(def-mheader	|$>| (mgreaterp))

(def-led-equiv	|$>=| parse-infix)
(def-lbp	|$>=| 80.)
(def-rbp	|$>=| 80.)
(def-pos	|$>=| $clause)
(def-rpos	|$>=| $expr)
(def-lpos	|$>=| $expr)
(def-mheader	|$>=| (mgeqp))

(def-led-equiv	|$<| parse-infix)
(def-lbp	|$<| 80.)
(def-rbp	|$<| 80.)
(def-pos	|$<| $clause)
(def-rpos	|$<| $expr)
(def-lpos	|$<| $expr)
(def-mheader	|$<| (mlessp))

(def-led-equiv	|$<=| parse-infix)
(def-lbp	|$<=| 80.)
(def-rbp	|$<=| 80.)
(def-pos	|$<=| $clause)
(def-rpos	|$<=| $expr)
(def-lpos	|$<=| $expr)
(def-mheader	|$<=| (mleqp))

(def-nud-equiv	|$NOT| parse-prefix)
;LBP not needed
(def-rbp	|$NOT| 70.)
(def-pos	|$NOT| $clause)
(def-rpos	|$NOT| $clause)
(def-lpos	|$NOT| $clause)
(def-mheader	|$NOT| (mnot))

(def-led-equiv	|$AND| parse-nary)
(def-lbp	|$AND| 65.)
;RBP not needed
(def-pos	|$AND| $clause)
;RPOS not needed
(def-lpos	|$AND| $clause)
(def-mheader	|$AND| (mand))

(def-led-equiv	|$OR| parse-nary)
(def-lbp	|$OR| 60.)
;RBP not needed
(def-pos	|$OR| $clause)
;RPOS not needed
(def-lpos	|$OR| $clause)
(def-mheader	|$OR| (mor))

(def-led-equiv	|$,| parse-nary)
(def-lbp	|$,| 10.)
;RBP not needed
(def-pos	|$,| $any)
;RPOS not needed
(def-lpos	|$,| $any)
(def-mheader	|$,| ($ev))

(def-nud-equiv |$THEN| delim-err)
(def-lbp |$THEN| 5.)
(def-rbp |$THEN| 25.)

(def-nud-equiv |$ELSE| delim-err)
(def-lbp |$ELSE| 5.)
(def-rbp |$ELSE| 25.)

(def-nud-equiv |$ELSEIF| delim-err)
(def-lbp  |$ELSEIF| 5.)
(def-rbp  |$ELSEIF| 45.)
(def-pos  |$ELSEIF| $any)
(def-rpos |$ELSEIF| $clause)

;No LBP - Default as high as possible
(def-rbp     $if 45.)
(def-pos     $if $any)
(def-rpos    $if $clause)
;No LPOS
(def-mheader $if (mcond))

(def-nud (|$IF|) (op)
  (list* (pos op)
	 (mheader op)
	 (parse-condition op)))

(defun parse-condition (op)
  (list* (parse (rpos op) (rbp op))
	 (if (eq (first-c) '$then)
	     (parse '$any (rbp (pop-c)))
	     (mread-synerr "Missing `then'"))
	 (case (first-c)
	   (($else)   (list t (parse '$any (rbp (pop-c)))))
	   (($elseif) (parse-condition (pop-c)))
	   (t ; Note: $FALSE instead of () makes DISPLA suppress display!
	    (list t '$false)))))

(def-mheader $do (mdo))

(defun parse-$do (lex &aux (left (make-mdo)))
  (setf (car left) (mheader 'mdo))
  (do ((op lex (pop-c))  (active-bitmask 0))
      (nil)
    (if (eq op '|$:|) (setq op '$from))
    (setq active-bitmask (collision-check '$do active-bitmask op))
    (let ((data (parse (rpos op) (rbp op))))
      (case op
	($do		(setf (mdo-body left) data) (return (cons '$any left)))
	($for		(setf (mdo-for  left) data))
	($from		(setf (mdo-from left) data))
	($in		(setf (mdo-op   left) 'mdoin)
			(setf (mdo-from left) data))
	($step		(setf (mdo-step left) data))
	($next		(setf (mdo-next left) data))
	($thru		(setf (mdo-thru left) data))
	(($unless $while)
			(if (eq op '$while)
			    (setq data (list (mheader '$not) data)))
			(setf (mdo-unless left)
			   (if (null (mdo-unless left))
			       data
			       (list (mheader '$or) data (mdo-unless left)))))
	(t (parse-bug-err '$do))))))

(def-lbp $for    25.)
(def-lbp $from   25.)
(def-lbp $step   25.)
(def-lbp $next   25.)
(def-lbp $thru   25.)
(def-lbp $unless 25.)
(def-lbp $while  25.)
(def-lbp $do	 25.)

(def-nud-equiv $for    parse-$do)
(def-nud-equiv $from   parse-$do)
(def-nud-equiv $step   parse-$do)
(def-nud-equiv $next   parse-$do)
(def-nud-equiv $thru   parse-$do)
(def-nud-equiv $unless parse-$do)
(def-nud-equiv $while  parse-$do)
(def-nud-equiv $do     parse-$do)

(def-rbp $do      25.)
(def-rbp $for    200.)
(def-rbp $from    95.)
(def-rbp $in      95.)
(def-rbp $step    95.)
(def-rbp $next    45.)
(def-rbp $thru    95.)
(def-rbp $unless  45.)
(def-rbp $while	  45.)

(def-rpos $do     $any)
(def-rpos $for    $any)
(def-rpos $from   $any)
(def-rpos $step   $expr)
(def-rpos $next   $any)
(def-rpos $thru   $expr)
(def-rpos $unless $clause)
(def-rpos $while  $clause)


(def-collisions $do
  ($do	   . ())
  ($for    . ($for))
  ($from   . ($in $from))
  ($in     . ($in $from $step $next))
  ($step   . ($in       $step $next))
  ($next   . ($in	$step $next))
  ($thru   . ($in $thru)) ;$IN didn't used to get checked for
  ($unless . ())
  ($while  . ()))

;#+ti  ;;because of a bug the preceding doesn't give this..
;(defprop $do (($WHILE . 256) ($UNLESS . 128)
;                ($THRU . 64)
;                ($NEXT . 32)
;                ($STEP . 16)
;                ($IN . 8)
;                ($FROM . 4)
;                ($FOR . 2)
;                ($DO . 1)) keys)


(def-mheader   |$$| (nodisplayinput))
(def-nud-equiv |$$| premterm-err)
(def-lbp       |$$| -1)
;No RBP, POS, RPOS, RBP, or MHEADER

(def-mheader   |$;| (displayinput))
(def-nud-equiv |$;| premterm-err)
(def-lbp       |$;| -1)
;No RBP, POS, RPOS, RBP, or MHEADER

(def-nud-equiv  |$&&| delim-err)
(def-lbp	|$&&| -1)

(defun mopstrip (x)
  ;; kludge interface function to allow the use of lisp PRINC in places.
  (cond ((null x) 'false)
	((or (eq x t) (eq x 't)) 'true)
	((numberp x) x)
	((symbolp x)
	 (or (get x 'reversealias)
	     (if (imember (firstcharn x) '(#\$ #\% #\&))
		 (implode (cdr (exploden x)))
		 x)))
	(t (maknam (mstring x)))))
	
(define-initial-symbols
  ;; * Note: /. is looked for explicitly rather than
  ;;     existing in this chart. The reason is that
  ;;     it serves a dual role (as a decimal point) and
  ;;     must be special-cased.
  ;;
  ;;     Same for // because of the /* ... */ handling
  ;;     by the tokenizer
  ;; Single character
  |+| |-| |*| |^| |<| |=| |>| |(| |)| |[| |]| |,|
  |:| |!| |#| |'| |;| |$| |&|			
  ;;Two character
  |**| |^^| |:=| |::| |!!| |<=| |>=| |''| |&&|			 
  ;; Three character
  |::=|
  )

;;; User extensibility:
(defmfun $prefix (operator &optional (rbp  180.)
			             (rpos '$any)
				     (pos  '$any))
  (def-operator operator pos ()  ()     rbp rpos () t
		'(nud . parse-prefix) 'msize-prefix 'dimension-prefix ()   )
  operator)

(defmfun $postfix (operator &optional (lbp  180.)
			             (lpos '$any)
				     (pos  '$any))
  (def-operator operator pos lbp lpos   ()  ()   t  ()
		'(led . parse-postfix) 'msize-postfix 'dimension-postfix  ()   )
  operator)

(defmfun $infix  (operator &optional (lbp  180.)
			             (rbp  180.)
				     (lpos '$any)
				     (rpos '$any)
				     (pos  '$any))
  (def-operator operator pos lbp lpos   rbp rpos t t
		'(led . parse-infix) 'msize-infix 'dimension-infix () )
  operator)

(defmfun $nary   (operator &optional (bp     180.)
			             (argpos '$any)
				     (pos    '$any))
  (def-operator operator pos bp  argpos bp  ()   t t
		'(led . parse-nary) 'msize-nary 'dimension-nary () )
  operator)

(defmfun $matchfix (operator
		    match  &optional (argpos '$any)
				     (pos    '$any))
  ;shouldn't MATCH be optional?
  (def-operator operator pos ()  argpos ()  ()  () () 
		'(nud . parse-matchfix) 'msize-matchfix 'dimension-match match)
  operator)

(defmfun $nofix  (operator &optional (pos '$any))
  (def-operator operator pos ()  ()     ()  () () ()
		'(nud . parse-nofix) 'msize-nofix 'dimension-nofix ()   )
  operator)

;;; (DEF-OPERATOR op pos lbp lpos rbp rpos sp1 sp2 
;;;	parse-data grind-fn dim-fn match)
;;; OP        is the operator name.
;;; POS       is its ``part of speech.''
;;; LBP       is its ``left binding power.''
;;; LPOS      is the part of speech of the arguments to its left, or of all.
;;;            arguments for NARY and MATCHFIX.
;;; RBP       is its ``right binding power.''
;;; RPOS      is the part of speech of the argument to its right.
;;; SP1       says if the DISSYM property needs a space on the right.
;;; SP2       says if the DISSYM property needs a space on the left.
;;; PARSE-DATA is (prop . fn) -- parser prop name dotted with function name
;;; GRIND-FN  is the grinder function for the operator.
;;; DIM-FN    is the dimension function for the operator.
;;; PARSEPROP is the property name to use for parsing. One of LED or NUD.
;;; MATCH     if non-(), ignores SP1 and SP2. Should be the match symbol.
;;;	        sets OP up as matchfix with MATCH.
;;;
;;; For more complete descriptions of these naming conventions, see
;;; the comments in GRAM package, which describe them in reasonable detail.

(defun def-operator (op pos lbp lpos rbp rpos sp1 sp2
			parse-data grind-fn dim-fn match)
  (let ((x))
    (if (or (and rbp (not (integerp (setq x rbp))))
	    (and lbp (not (integerp (setq x lbp)))))
	(merror "Binding powers must be integers.~%~M is not an integer." x))
    (if (mstringp op) (setq op (define-symbol op)))
    (op-setup op)
    (let ((noun   ($nounify op))
	  (dissym (cdr (exploden op))))
      (cond
       ((not match)
	(setq dissym (append (if sp1 '(#\space)) dissym (if sp2 '(#\space)))))
       (t (if (mstringp match) (setq match (define-symbol match)))
	  (op-setup match)
	  (putprop op    match 'match)
	  (putprop match 5.    'lbp)
	  (setq dissym (cons dissym (cdr (exploden match))))))
      (putprop op pos 'pos)
      (putprop op (cdr parse-data) (car parse-data))
      (putprop op   grind-fn  'grind)
      (putprop op   dim-fn    'dimension)
      (putprop noun dim-fn    'dimension)
      (putprop op   dissym 'dissym)
      (putprop noun dissym 'dissym)
      (when rbp
	(putprop op   rbp  'rbp)
	(putprop noun rbp  'rbp))
      (when lbp
	(putprop op   lbp  'lbp)
	(putprop noun lbp  'lbp))
      (when lpos (putprop op   lpos 'lpos))
      (when rpos (putprop op   rpos 'rpos))
      (getopr op))))

(defun op-setup (op)
  (declare (special mopl))
  (let ((dummy (or (get op 'op)
		   (implode (cons '& (string* op))))))
    (putprop op    dummy 'op )
    (putprop dummy op    'opr)
    (if (and (operatorp1 op) (not (memq dummy (cdr $props))))
	(push dummy mopl))
    (add2lnc dummy $props)))

(defun kill-operator (op)
  (undefine-symbol (stripdollar op))
  (let ((opr (get op 'op)) (noun-form ($nounify op)))
    (remprop opr 'opr)
    (rempropchk opr)
    (mapc #'(lambda (x) (remprop op x))
 	  '(nud nud-expr nud-subr			; NUD info
		     led led-expr led-subr		; LED info
		     lbp rbp			; Binding power info
		     lpos rpos pos		; Part-Of-Speech info
		     grind dimension dissym	; Display info
		     op
		     ))			; Operator info
    (mapc #'(lambda (x) (remprop noun-form x))
 	  '(dimension dissym lbp rbp))))



;; the functions get-instream etc.. are all defined in
;; gcl lsp/debug.lsp
;; they are all generic common lisp and could be used by
;; any Common lisp implementation.


#-gcl
(eval-when (:compile-toplevel :execute :load-toplevel)

  (defvar *stream-alist* nil)

  (defun stream-name (path)
    (let ((tem (errset (namestring (pathname path)))))
      (car tem)))

  (defun instream-name (instr)
    (or (instream-stream-name instr)
	(stream-name (instream-stream instr))))

  (defstruct instream
    stream
    (line 0 :type fixnum)
    stream-name)

;; (closedp stream) checks if a stream is closed.
;; how to do this in common lisp!!

  (defun cleanup ()
    #+never-clean-up-dont-know-how-to-close
    (dolist (v *stream-alist*)
      (if (closedp (instream-stream v))
	  (setq *stream-alist* (delete v *stream-alist*)))))

  (defun get-instream (str)
    (or (dolist (v *stream-alist*)
	  (cond ((eq str (instream-stream v))
		 (return v))))
	(let (name errset)
	  (errset (setq name (namestring str)))
	  (car (setq *stream-alist*
		     (cons  (make-instream :stream str :stream-name name)
			    *stream-alist*))))))



  (defun newline (str ch) ch
	 (let ((in (get-instream str)))
	   (setf (instream-line in) (the fixnum (+ 1 (instream-line in)))))
	;; if the next line begins with '(',
	;; then record all cons's eg arglist )
	;;(setq *at-newline*  (if (eql (peek-char nil str nil) #\() :all t))
	 (values)))					; end #-gcl

#+gcl
(deff newline (symbol-function 'si::newline))

(defun find-stream (stream)
   (dolist (v *stream-alist*)
	(cond ((eq stream (instream-stream v))
	       (return v))))
  )


(defun add-lineinfo (lis)
  (if (or (atom lis) (and (eq *parse-window* *standard-input*)
			  (not (find-stream *parse-stream*))))
			  lis
    (let* ((st (get-instream *parse-stream*))
 	   (n (instream-line st))
	   (nam (instream-name st))
	   )
      (or nam (return-from add-lineinfo lis))
      (setq *current-line-info*
	    (cond ((eq (cadr *current-line-info*) nam)
		   (cond ((eql (car *current-line-info*) n)
			  *current-line-info*)
			 (t  (cons n (cdr *current-line-info*)))))
		  (t (list n nam  'src))))
      (cond ((null (cdr lis))
	     (list (car lis) *current-line-info*))
	    (t (append lis (list *current-line-info*)))))))
