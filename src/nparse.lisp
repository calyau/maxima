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
(DEFMVAR ALPHABET
  '(#\_ #\%)
  "alphabetic exceptions list")
;;;  Note: The following algorithms work only in environments where 
;;;        ascii codes for A,...,Z and 0,...,9 follow sequentially.
;;;	   Normal ASCII and LispM encoding makes this true. If we ever
;;;	   bring this up on EBCDIC machines, we may lose.

(DEFMACRO IMEMBER (X L)
  `(MEMBER ,X ,L))

;#-cl ;;defined in commac or via common
;(cond ((not (fboundp 'char<=)))
; (defun char<= (a b) (<= a b))
; (defun char>= (a b) (>= a b)))


(PROGN

  (DEFMVAR ALPHABET '(#\_ #\%))

  (DEFMFUN ALPHABETP (N)
    #-cl (DECLARE (FIXNUM N))
    (and (characterp n)
	 (OR (AND (CHAR>= N #\A) (CHAR<= N #\Z)) ; upper case
	     (AND (CHAR>= N #\a) (CHAR<= N #\z)) ; lower case
	     (imember n '(#\_ #\%))
	     (IMEMBER N ALPHABET))))
; test for %, _, or other declared
					;    alphabetic characters.
  (DEFMFUN ASCII-NUMBERP (NUM)
    (AND (characterp num) (CHAR<= NUM #\9) (CHAR>= NUM #\0))))

 ;End of #-LISPM
 
;dbg:  ;;signals a conditition 'dbg:parse-ferror
;(DEFUN PARSE-FERROR (format-ctl-STRING &REST FORMAT-ARGS)
;  (ERROR 'PARSE-FERROR ':format-string FORMAT-ctl-STRING ':FORMAT-ARGS (COPY-LIST FORMAT-ARGS)))

(defvar *parse-window* nil)

(DEFUN MREAD-SYNERR (sSTRING &REST L)
;  #+lispm (sys:parse-ferror    (format nil sstring l)  :correct-input )
;  #+lispm (dbg:parse-ferror    (format nil sstring l)  :correct-input )
  #+(OR  NIL) (APPLY #'ERROR #+LISPM NIL #+NIL ':READ-ERROR sSTRING L)
  #-(OR LISPM NIL)
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
      (apply 'format t sstring l)
      (cond ((output-stream-p *standard-input*)
	     (let ((n (get '*parse-window* 'length))
		   some ch
		   k
		   )
	       (sloop for   i below 20
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
				  (sloop for i below 20 for tem =
					 nil 
					 ;(read-char-no-hang)
					 while tem collect tem)))
	       (terpri)
	       (sloop for v in some do (princ v))
	       (terpri)
	       (sloop for i from 2 below k do (princ #\space))
	       (princ "^")
	       
	       ;(sloop while (read-char-no-hang) )
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
(DEFUN FIXNUM-CHAR-UPCASE (C)
  (char-upcase c))

;  (char-code (char-upcase (code-char c))))


(DEFUN FIRSTCHARN (X)
  (aref (string x) 0))

(DEFVAR *PARSE-STREAM*		()	  "input stream for Macsyma parser")
(DEFVAR MACSYMA-OPERATORS	()	  "Macsyma operators structure")
(DEFVAR *MREAD-PROMPT*		nil	  "prompt used by MREAD")
(DEFVAR *MREAD-EOF-OBJ* () "Bound by MREAD for use by MREAD-RAW")

(defun tyi-parse-int (stream eof)
  (or *parse-window*
      (progn (setq *parse-window* (make-list 25))
	     (setf (get '*parse-window* 'length) (length *parse-window*))
	     (nconc *parse-window* *parse-window*)))
  (let ((tem (TYI stream eof)))
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




(DEFUN *MREAD-PROMPT* (OUT-STREAM CHAR)
  CHAR
  (FORMAT OUT-STREAM "~&~A" *MREAD-PROMPT*))
  
(DEFUN ALIASLOOKUP (OP)
  (IF (SYMBOLP OP)
      (OR (GET OP 'ALIAS) OP)
      OP))


;;;; Tokenizing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                    ;;;;;
;;;;;                      The Input Scanner                             ;;;;;
;;;;;                                                                    ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gobble whitespace, recognize '#' comments..
(DEFUN GOBBLE-WHITESPACE ( &aux saw-newline ch saw-other)
  (DO () (NIL) ; Gobble whitespace
      (setq ch (PARSE-TYIPEEK))
      (cond ((eql ch #\newline)
	     (setq saw-other nil)
	     (setq saw-newline t))
	    ((IMEMBER ch
		  '(#\TAB #\SPACE #\Linefeed #\return ;#\control-C
			  #\Page))
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

(DEFUN READ-COMMAND-TOKEN (OBJ)
  (GOBBLE-WHITESPACE)
  (READ-COMMAND-TOKEN-AUX OBJ))

(defun ch-minusp (z)
  (and (numberp z) (< z 0)))

(defun safe-assoc (item lis)
  "maclisp would not complain about (car 3) it gives nil"
  (sloop for v in lis
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
  (sloop for v on lis
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
	   (tyi-parse-int *PARSE-STREAM* -1))
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
	 (lis (if (eql ch -1) nil  (parser-assoc (char-upcase ch) obj))))
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


(DEFUN SCAN-MACSYMA-TOKEN ()
  ;; note that only $-ed tokens are GETALIASed.
  (let ((tem (CONS '#\$ (SCAN-TOKEN T))))
    (setq tem (if $bothcases (bothcase-implode tem) (implode1 tem nil)))
  (GETALIAS tem)))

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

(defvar $bothcases t)
(DEFUN SCAN-TOKEN (FLAG)
  (DO ((C (PARSE-TYIPEEK) (PARSE-TYIPEEK))
       (L () (CONS C L)))
      ((AND FLAG (NOT (OR (ASCII-NUMBERP C) (ALPHABETP C) (char= C #.back-slash-char)))) ;;#/\
       (NREVERSE (OR L (NCONS (PARSE-TYI))))) ; Read at least one char ...
    (IF (char= (PARSE-TYI) #. back-slash-char);; #/\
	(SETQ C (PARSE-TYI))
	(or $bothcases  (SETQ C (FIXNUM-CHAR-UPCASE C))))
    (SETQ FLAG T)))

(DEFUN SCAN-LISP-STRING () (INTERN (SCAN-STRING)))

(DEFUN SCAN-MACSYMA-STRING () (INTERN (SCAN-STRING #\&)))

(DEFUN SCAN-STRING (&optional init)
  (let ((buf (or *scan-string-buffer*
		 (setq *scan-string-buffer*
		       (make-array 50 :element-type ' #.(array-element-type "abc")
				   :fill-pointer 0 :adjustable t))))
	(*scan-string-buffer* nil))
    (setf (fill-pointer buf) 0)
    (when init (vector-push-extend (coerce init 'character) buf))
    (DO ((C (PARSE-TYIPEEK) (PARSE-TYIPEEK)))
	((cond ((eql c -1))
	       ((char= c #. double-quote-char)
		(parse-tyi) t))
	 (copy-seq buf))
      (IF (char= (PARSE-TYI) #. back-slash-char) ;; #/\ )
	  (SETQ C (PARSE-TYI)))
      #-cl
      (vector-push-extend (code-char c) buf)
      #+cl
      (vector-push-extend c  buf)
      )))

(defvar *string-register* (make-array 100 :fill-pointer 0 :adjustable t :element-type '#.(array-element-type "a")))
(defun readlist (lis)
  (setf (fill-pointer *string-register*) 0)
  (sloop for u in lis do (vector-push-extend u *string-register*))
  (read-from-string   *string-register*))


(DEFUN MAKE-NUMBER (DATA)
  (SETQ DATA (NREVERSE DATA))
  ;; Maxima really wants to read in any number as a double-float
  ;; (except when we have a bigfloat, of course!).  So convert an E or
  ;; S exponent marker to D.
  (when (member (car (nth 3. data)) '(#\E #\S))
    (setf (nth 3. data) (list #\D)))
  (IF (NOT (EQUAL (NTH 3. DATA) '(#\B)))
      (READLIST (APPLY #'APPEND DATA))
      ;; For bigfloats, turn them into rational numbers then convert to bigfloat
      ($BFLOAT `((MTIMES) ((MPLUS) ,(READLIST (or (FIRST DATA) '(#\0)))
				   ((MTIMES) ,(READLIST (or (THIRD DATA) '(#\0)))
					     ((MEXPT) 10. ,(f- (LENGTH (THIRD DATA))))))
			  ((MEXPT) 10. ,(FUNCALL (IF (char= (FIRST (FIFTH DATA)) #\-) #'- #'+)
						 (READLIST (SIXTH DATA))))))))

(DEFUN SCAN-DIGITS (DATA CONTINUATION? CONTINUATION &optional exponent-p)
  (DO ((C (PARSE-TYIPEEK) (PARSE-TYIPEEK))
       (L () (CONS C L)))
      ((NOT (ASCII-NUMBERP C))
       (COND ((IMEMBER C CONTINUATION?)
	      (FUNCALL CONTINUATION (LIST* (NCONS (FIXNUM-CHAR-UPCASE
						   (PARSE-TYI)))
					   (NREVERSE L)
					   Data)
				   ))
	     ((and (null l) exponent-p)
	      ;; We're trying to parse the exponent part of a number,
	      ;; and we didn't get a value after the exponent marker.
	      ;; That's an error.
	      (merror "Incomplete number.  Missing exponent?"))
	     (T
	      (MAKE-NUMBER (CONS (NREVERSE L) DATA)))))
    (PARSE-TYI)))

;#+nil
;(DEFUN SCAN-NUMBER-BEFORE-DOT (DATA)
;  (SCAN-DIGITS DATA '(#. period-char) #'SCAN-NUMBER-AFTER-DOT))

(DEFUN SCAN-NUMBER-AFTER-DOT (DATA)
  (SCAN-DIGITS DATA '(#\E #\e #\B #\b #\D #\d #\S #\s) #'SCAN-NUMBER-EXPONENT))

(DEFUN SCAN-NUMBER-EXPONENT (DATA)
  (PUSH (NCONS (IF (OR (char= (PARSE-TYIPEEK) #\+)
		       (char= (PARSE-TYIPEEK) #\-))
		   (PARSE-TYI)
		   #\+))
	DATA)
  (SCAN-DIGITS DATA () () t))

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

(DEFVAR SCAN-BUFFERED-TOKEN (LIST NIL)
  "put-back buffer for scanner, a state-variable of the reader")

(DEFUN PEEK-ONE-TOKEN ()
  (PEEK-ONE-TOKEN-G NIL NIL))

(DEFUN PEEK-ONE-TOKEN-G (EOF-OK? EOF-OBJ)
  (cond
   ((CAR SCAN-BUFFERED-TOKEN)
    (CDR SCAN-BUFFERED-TOKEN))
   (t (RPLACD SCAN-BUFFERED-TOKEN (SCAN-ONE-TOKEN-G EOF-OK? EOF-OBJ))
      (CDR (RPLACA SCAN-BUFFERED-TOKEN T)))))

(DEFUN SCAN-ONE-TOKEN ()
  (SCAN-ONE-TOKEN-G NIL NIL))

(DEFUN SCAN-ONE-TOKEN-G (EOF-OK? EOF-OBJ)
  (COND ((CAR SCAN-BUFFERED-TOKEN)
	 (RPLACA SCAN-BUFFERED-TOKEN ())
	 (CDR SCAN-BUFFERED-TOKEN))
	((READ-COMMAND-TOKEN MACSYMA-OPERATORS))
	(T
	 (LET ((TEST (PARSE-TYIPEEK)))
	   (cond  ((eql test -1.)
		   (PARSE-TYI)
		   (IF EOF-OK? EOF-OBJ
		       (MAXIMA-ERROR "End of file while scanning expression")))
		  ((eql test forward-slash-char) ;;#//)
		   (PARSE-TYI)
		   (COND ((char= (PARSE-TYIPEEK) #\*)
			  (GOBBLE-COMMENT)
			  (SCAN-ONE-TOKEN-G EOF-OK? EOF-OBJ))
			 (T '#-cl $// #+cl $/ )))
		  ((eql test #. period-char) (PARSE-TYI)	; Read the dot
		   (IF (ASCII-NUMBERP (PARSE-TYIPEEK))
		       (SCAN-NUMBER-AFTER-DOT (LIST (NCONS #. period-char) NIL))
		       '|$.|))
		  ((eql test double-quote-char );;#/")
		   (PARSE-TYI)
		   (SCAN-MACSYMA-STRING))
		  ((eql test #\?)
		   (PARSE-TYI)
		   (COND ((char= (PARSE-TYIPEEK) double-quote-char );;#/")
			  (PARSE-TYI)
			  (SCAN-LISP-STRING))
			 ((char= (parse-tyipeek) #\:)
			  (scan-keyword-token))
			 (t
			  (SCAN-LISP-TOKEN))))
		  (T
		   (IF (ASCII-NUMBERP TEST)
		       (SCAN-NUMBER-BEFORE-DOT ())
		       (SCAN-MACSYMA-TOKEN))))))))

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

    

(DEFMACRO FIRST-C () '(PEEK-ONE-TOKEN))
(DEFMACRO POP-C   () '(SCAN-ONE-TOKEN))


(DEFUN MSTRINGP (X)
  (AND (SYMBOLP X) (char= (FIRSTCHARN X) #\&)))


;(DEFUN AMPERCHK (NAME)
;  (IF (MSTRINGP NAME) (DOLLARIFY-NAME NAME) NAME))
;;see suprv1

(DEFUN INHERIT-PROPL (OP-TO OP-FROM GETL)
  (LET ((PROPL (GETL OP-FROM GETL)))
    (IF PROPL
	(PROGN (REMPROP OP-TO (CAR PROPL))
	       (PUTPROP OP-TO (CADR PROPL) (CAR PROPL)))
	(INHERIT-PROPL OP-TO
		       (MAXIMA-ERROR (LIST "has no" GETL "properties.")
			      OP-FROM
			      'WRNG-TYPE-ARG)
		       GETL))))


;;; (NUD <op>)
;;; (LED <op> <left>)
;;;
;;;  <op>   is the name of the operator which was just popped.
;;;  <left> is the stuff to the left of the operator in the LED case.
;;;

(eval-when (eval compile load)
  #+already-expanded-below
  (DEF-PROPL-CALL NUD (OP)
    (IF (OPERATORP OP)
	;; If first element is an op, it better have a NUD
	(MREAD-SYNERR "~A is not a prefix operator" (MOPSTRIP OP))
	;; else take it as is.
	(CONS '$ANY OP)))
;;begin expansion
  (DEFMACRO DEF-NUD-EQUIV (OP EQUIV)
    (LIST 'PUTPROP (LIST 'QUOTE OP) (LIST 'FUNCTION EQUIV)
          (LIST 'QUOTE 'NUD)))
  (DEFMACRO NUD-PROPL () ''(NUD))
  (DEFMACRO DEF-NUD-FUN (OP-NAME OP-L . BODY)
    (LIST* 'DEFUN-PROP (LIST* OP-NAME 'NUD 'NIL) OP-L BODY))
  (DEFUN NUD-CALL (OP)
    (LET ((TEM (AND (SYMBOLP OP) (GETL OP '(NUD)))) res)
      (setq res	 
	    (IF (NULL TEM)
		(IF (OPERATORP OP)
		    (MREAD-SYNERR "~A is not a prefix operator"
				  (MOPSTRIP OP))
		    (CONS '$ANY OP))
		(FUNCALL (CADR TEM) OP)))
      res))
;;end expansion 

;;following defines def-led-equiv led-propl def-led-fun led-call
  #+already-expanded-below
  (DEF-PROPL-CALL LED (OP L)
    (MREAD-SYNERR "~A is not an infix operator" (MOPSTRIP OP))))

;;begin expansion
(DEFMACRO DEF-LED-EQUIV (OP EQUIV)
    (LIST 'PUTPROP (LIST 'QUOTE OP) (LIST 'FUNCTION EQUIV)
          (LIST 'QUOTE 'LED)))

(eval-when (compile load eval)
  (DEFMACRO LED-PROPL () ''(LED)))

(DEFMACRO DEF-LED-FUN (OP-NAME OP-L . BODY)
    (LIST* 'DEFUN-PROP (LIST* OP-NAME 'LED 'NIL) OP-L BODY))
(DEFUN LED-CALL (OP L)
   
    (LET ((TEM (AND (SYMBOLP OP) (GETL OP '(LED)))) res)
	 (setq res
      (IF (NULL TEM)
          (MREAD-SYNERR "~A is not an infix operator" (MOPSTRIP OP))
          (FUNCALL (CADR TEM) OP L))
      )
	 res
      ))
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

(DEFMACRO DEF-NUD ((OP . LBP-RBP) BVL . BODY)
  (let (( lbp (nth 0 lbp-rbp))
	( rbp (nth 1 lbp-rbp)))
    `(PROGN 'COMPILE 	  ,(MAKE-PARSER-FUN-DEF OP 'NUD BVL BODY)
	    (SET-LBP-AND-RBP ',OP ',LBP ',RBP))))

;#-cl
;(DEFMACRO DEF-NUD ((OP #+nil &OPTIONAL LBP RBP) BVL . BODY)
;  `(PROGN 'COMPILE 	  ,(MAKE-PARSER-FUN-DEF OP 'NUD BVL BODY)
;	  (SET-LBP-AND-RBP ',OP ',LBP ',RBP)))

(DEFUN SET-LBP-AND-RBP (OP LBP RBP)
  (COND ((NOT (consp OP))
	 (LET ((EXISTING-LBP (GET OP 'LBP))
	       (EXISTING-RBP (GET OP 'RBP)))
	   (COND ((NOT LBP)
		  (COMMENT IGNORE OMITTED ARG))
		 ((NOT EXISTING-LBP)
		  (PUTPROP OP LBP 'LBP))
		 ((NOT (EQUAL EXISTING-LBP LBP))
		  (MAXIMA-ERROR "Incompatible LBP's defined for this operator" OP)))
	   (COND ((NOT RBP)
		  (COMMENT IGNORE OMITTED ARG))
		 ((NOT EXISTING-RBP)
		  (PUTPROP OP RBP 'RBP))
		 ((NOT (EQUAL EXISTING-RBP RBP))
		  (MAXIMA-ERROR "Incompatible RBP's defined for this operator" OP)))))
	('ELSE
	 (MAPCAR #'(LAMBDA (X) (SET-LBP-AND-RBP X LBP RBP))
		 OP))))
				   

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


(DEFMACRO DEF-LED((OP . LBP-RBP) BVL . BODY)
  (let (( lbp (nth 0 lbp-rbp))
	( rbp (nth 1 lbp-rbp)))
    `(PROGN 'COMPILE
	    ,(MAKE-PARSER-FUN-DEF  OP 'LED BVL BODY)
	    (SET-LBP-AND-RBP ',OP ',LBP ',RBP))))

;#-cl
;(DEFMACRO DEF-LED ((OP #+(or cl NIL) &OPTIONAL LBP RBP) BVL . BODY)
;  `(PROGN 'COMPILE
;	  ,(MAKE-PARSER-FUN-DEF  OP 'LED BVL BODY)
;	  (SET-LBP-AND-RBP ',OP ',LBP ',RBP)))

(DEFMACRO DEF-COLLISIONS (OP &REST ALIST)
  (LET ((KEYS (DO ((I  1.    (#+cl ash #-cl LSH I 1.))
		   (LIS  ALIST (CDR LIS))
		   (NL ()    (CONS (CONS (CAAR LIS) I) NL)))
		  ((NULL LIS) NL))))
    `(PROGN 'COMPILE
       (DEFPROP ,OP ,(let #+lispm ((default-cons-area working-storage-area))
			  #-lispm nil
		       (copy-tree KEYS )) KEYS)
       ,@(MAPCAR #'(LAMBDA (DATA)
		     `(DEFPROP ,(CAR DATA)
			       ,(DO ((I 0 (LOGIOR I  (CDR (ASSQ (CAR LIS) KEYS))))
				     (LIS (CDR DATA) (CDR LIS)))
				    ((NULL LIS) I))
			       ,OP))
		 ALIST))))


(DEFUN COLLISION-LOOKUP (OP ACTIVE-BITMASK KEY-BITMASK)
  (LET ((RESULT (LOGAND ACTIVE-BITMASK KEY-BITMASK)))
    (IF (NOT (ZEROP RESULT))
	(DO ((L (GET OP 'KEYS) (CDR L)))
	    ((NULL L) (PARSE-BUG-ERR 'COLLISION-CHECK))
	  (IF (NOT (ZEROP (LOGAND RESULT (CDAR L))))
	      (RETURN (CAAR L)))))))

(DEFUN COLLISION-CHECK (OP ACTIVE-BITMASK KEY)
  (LET ((KEY-BITMASK (GET KEY OP)))
    (IF (NOT KEY-BITMASK)
	(MREAD-SYNERR "~A is an unknown keyword in a ~A statement."
		      (MOPSTRIP KEY) (MOPSTRIP OP)))
    (LET ((COLLISION (COLLISION-LOOKUP OP ACTIVE-BITMASK KEY-BITMASK)))
      (IF COLLISION
	  (IF (EQ COLLISION KEY)
	      (MREAD-SYNERR "This ~A's ~A slot is already filled."
			    (MOPSTRIP OP)
			    (MOPSTRIP KEY))
	      (MREAD-SYNERR "A ~A cannot have a ~A with a ~A field."
			    (MOPSTRIP OP)
			    (MOPSTRIP KEY)
			    (MOPSTRIP COLLISION))))
      (LOGIOR (CDR (ASSQ KEY (GET OP 'KEYS))) ACTIVE-BITMASK))))
      

;;;; Data abstraction

;;; LBP = Left Binding Power
;;;
;;; (LBP <op>)		 - reads an operator's Left Binding Power
;;; (DEF-LBP <op> <val>) - defines an operator's Left Binding Power

(DEFMFUN LBP (LEX) (COND ((safe-GET LEX 'LBP)) (T 200.)))

(DEFMACRO DEF-LBP (SYM VAL) `(DEFPROP ,SYM ,VAL LBP))

;;; RBP = Right Binding Power
;;;
;;; (RBP <op>)		 - reads an operator's Right Binding Power
;;; (DEF-RBP <op> <val>) - defines an operator's Right Binding Power

(DEFMFUN RBP (LEX) (COND ((safe-GET LEX 'RBP)) (T 200.)))

(DEFMACRO DEF-RBP (SYM VAL) `(DEFPROP ,SYM ,VAL RBP))

(DEFMACRO DEF-MATCH (X M) `(DEFPROP ,X ,M MATCH))

;;; POS = Part of Speech!
;;; 
;;; (LPOS <op>)
;;; (RPOS <op>)
;;; (POS  <op>)
;;;

(DEFUN LPOS (OP) (COND ((safe-GET OP 'LPOS)) (T '$ANY)))
(DEFUN RPOS (OP) (COND ((safe-GET OP 'RPOS)) (T '$ANY)))
(DEFUN POS (OP) (COND ((safe-GET OP 'POS)) (T '$ANY)))

(DEFMACRO DEF-POS  (OP POS) `(DEFPROP ,OP ,POS  POS))
(DEFMACRO DEF-RPOS (OP POS) `(DEFPROP ,OP ,POS RPOS))
(DEFMACRO DEF-LPOS (OP POS) `(DEFPROP ,OP ,POS LPOS))

;;; MHEADER

(DEFUN MHEADER (OP) (add-lineinfo (OR (safe-GET OP 'MHEADER) (NCONS OP))))

(DEFMACRO DEF-MHEADER (OP HEADER) `(DEFPROP ,OP ,HEADER MHEADER))


(DEFMVAR $PARSEWINDOW 10.
	 "The maximum number of 'lexical tokens' that are printed out on
each side of the error-point when a syntax (parsing) MAXIMA-ERROR occurs.  This
option is especially useful on slow terminals.  Setting it to -1 causes the
entire input string to be printed out when an MAXIMA-ERROR occurs."
	 FIXNUM)


;;;; Misplaced definitions

(DEFMACRO DEF-OPERATORP ()
  `(DEFMFUN OPERATORP (LEX)
     (AND (SYMBOLP LEX) (GETL LEX '(,@(NUD-PROPL) ,@(LED-PROPL))))))

(DEF-OPERATORP)

(DEFMACRO DEF-OPERATORP1 ()
  ;Defmfun -- used by SYNEX if not others.
  `(DEFMFUN OPERATORP1 (LEX)
     ;; Referenced outside of package: OP-SETUP, DECLARE1
     ;; Use for truth value only, not for return-value.
     (AND (SYMBOLP LEX) (GETL LEX '(LBP RBP ,@(NUD-PROPL) ,@(LED-PROPL))))))

(DEF-OPERATORP1)

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
(DEFUN MREAD (&REST READ-ARGS)
  #+NIL (let ((*mread-prompt-internal* *mread-prompt*)
	      (si:*ttyscan-dispatch-table *macsyma-ttyscan-operators*))
	  (declare (special *mread-prompt-internal*))
	  (SI:READ-APPLY ':MREAD #'MREAD-RAW (coerce READ-ARGS 'sys:vector)
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
  #-(or NIL cl)
  (READ-APPLY #'MREAD-RAW READ-ARGS))

(defun mread-prompter (stream char)
  (declare (special *mread-prompt-internal*))
  char ;  (declare (ignore char))
  (fresh-line stream)
  (princ *mread-prompt-internal* stream))

#+NIL
(DEFUN MREAD-WITH-PROMPT (PROMPT)
  (let ((*mread-prompt-internal* prompt)
	(si:*ttyscan-dispatch-table *macsyma-ttyscan-operators*))
    (declare (special *mread-prompt-internal*))
    (SI:READ-APPLY ':MREAD #'MREAD-RAW (SYS:VECTOR)
		   '(:prompt mread-prompter)
		   '(:reprompt mread-prompter))))

;; input can look like:
;;aa && bb && jim:3;


(DEFUN MREAD-RAW (*PARSE-STREAM* &OPTIONAL *MREAD-EOF-OBJ*)
  (LET ((SCAN-BUFFERED-TOKEN (LIST NIL))
	*parse-tyi*
	)
    (IF (EQ SCAN-BUFFERED-TOKEN ;; a handly unique object for the EQ test.
	    (PEEK-ONE-TOKEN-G T SCAN-BUFFERED-TOKEN))
	*MREAD-EOF-OBJ*
	(DO ((LABELS ())
	     (INPUT (PARSE '$ANY 0.) (PARSE '$ANY 0.)))
	    (NIL)
	  (CASE (FIRST-C)
	    ((|$;| |$$|)
	      ;force a separate line info structure
	     (SETF *CURRENT-LINE-INFO* NIL)
	     (RETURN (LIST (MHEADER (POP-C))
			   (IF LABELS (CONS (MHEADER '|$[|) (NREVERSE LABELS)))
			   INPUT)))
	    ((|$&&|)
	     (POP-C)
	     (IF (SYMBOLP INPUT)
		 (PUSH INPUT LABELS)
		 (MREAD-SYNERR "Invalid && tag. Tag must be a symbol")))
	    (T
	     (PARSE-BUG-ERR 'MREAD-RAW)))))))

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
;;;  this is why "X^A*B" parses but "X^A AND B" does not. X^A is a $EXPR
;;;  and not coercible to a $CLAUSE. See CONVERT.
;;;
;;;  <mode> is the required mode of the result.
;;;  <rbp>  is the right binding power to use for the parse. When an
;;;	     LED-type operator is seen with a lower left binding power
;;;	     than <rbp>, this parse returns what it's seen so far rather
;;;	     than calling that operator.
;;;

(DEFUN PARSE (MODE RBP) 
  (DO ((LEFT (NUD-CALL (POP-C))		; Envoke the null left denotation
	     (LED-CALL (POP-C) LEFT)))	;  and keep calling LED ops as needed
      ((>= RBP (LBP (FIRST-C)))		; Until next op lbp too low
       (CONVERT LEFT MODE))))		;  in which case, return stuff seen

;;; (PARSE-PREFIX <op>)
;;;
;;;  Parses prefix forms -- eg, -X or NOT FOO.
;;;
;;;  This should be the NUD property on an operator. It fires after <op>
;;;  has been seen. It parses forward looking for one more expression
;;;  according to its right binding power, returning
;;;  ( <mode> . ((<op>) <arg1>) )

(DEFUN PARSE-PREFIX (OP)
  (LIST (POS OP)			; Operator mode
	(MHEADER OP)			; Standard Macsyma expression header
	(PARSE (RPOS OP) (RBP OP))))	; Convert single argument for use

;;; (PARSE-POSTFIX <op> <left>)
;;;
;;;  Parses postfix forms. eg, X!.
;;;
;;;  This should be the LED property of an operator. It fires after <left>
;;;  has been accumulated and <op> has been seen and gobbled up. It returns
;;;  ( <mode> . ((<op>) <arg1>) )

(DEFUN PARSE-POSTFIX (OP L)
  (LIST (POS OP)			; Operator's mode
	(MHEADER OP)			; Standard Macsyma expression header
	(CONVERT L (LPOS OP))))		; Convert single argument for use

;;; (PARSE-INFIX <op> <left>)
;;;
;;;  Parses infix (non-nary) forms. eg, 5 mod 3.
;;;
;;;  This should be the led property of an operator. It fires after <left>
;;;  has been accumulated and <op> has been seen and gobbled up. It returns
;;;  ( <mode> . ((<op>) <arg1> <arg2>) )

(DEFUN PARSE-INFIX (OP L)
  (LIST (POS OP)			; Operator's mode
	(MHEADER OP)			; Standard Macsyma expression header
	(CONVERT L (LPOS OP))		; Convert arg1 for immediate use
	(PARSE (RPOS OP) (RBP OP))))	; Look for an arg2 

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

(DEFUN PARSE-NARY (OP L)
  (LIST* (POS OP)			    ; Operator's mode
	 (MHEADER OP)			    ; Normal Macsyma operator header
	 (CONVERT L (LPOS OP))		    ; Check type-match of arg1
	 (PRSNARY OP (LPOS OP) (LBP OP))))  ; Search for other args

;;; (PARSE-MATCHFIX <lop>)
;;;
;;;  Parses matchfix forms. eg, [form1,form2,...] or (form1,form2,...)
;;;
;;;  This should be the NUD property on an operator. It fires after <op>
;;;  has been seen. It parses <lop><form1>,<form2>,...<rop> returning
;;;  ( <mode> . ((<lop>) <form1> <form2> ...) ).

(DEFUN PARSE-MATCHFIX (OP)
  (LIST* (POS OP)			         ; Operator's mode
	 (MHEADER OP)			         ; Normal Macsyma operator header
	 (PRSMATCH (SAFE-GET OP 'MATCH) (LPOS OP))))  ; Search for matchfixed forms

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

(DEFUN PARSE-NOFIX (OP) (LIST (POS OP) (MHEADER OP)))

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

(DEFUN PRSNARY (OP MODE RBP) 
  (DO ((NL (LIST (PARSE MODE RBP))	   ; Get at least one form
	   (CONS (PARSE MODE RBP) NL)))	   ;  and keep getting forms
      ((NOT (EQ OP (FIRST-C)))		   ; until a parse pops on a new op
       (NREVERSE NL))			   ;  at which time return forms
      (POP-C)))				   ; otherwise pop op

;;; (PRSMATCH <match> <mode>)
;;;
;;; Parses a matchfix sequence. Eg, [form1,form2,...] or (form1,form2,...)
;;; Expects to be entered after the leading token is the popped (ie, at the
;;;  point where the parse of form1 will begin). Returns (form1 form2 ...).
;;;
;;; <match> is the token to look for as a matchfix character.
;;; <mode>  is the name of the mode that each form must be.

(DEFUN PRSMATCH (MATCH MODE)			  ; Parse for matchfix char
  (COND ((EQ MATCH (FIRST-C)) (POP-C) NIL)	  ; If immediate match, ()
	(T					  ; Else, ...
	 (DO ((NL (LIST (PARSE MODE 10.))	  ;  Get first element
		  (CONS (PARSE MODE 10.) NL)))	  ;   and Keep adding elements
	     ((EQ MATCH (FIRST-C))		  ;  Until we hit the match.
	      (POP-C)				  ;   Throw away match.
	      (NREVERSE NL))			  ;   Put result back in order
	   (IF (EQ '|$,| (FIRST-C))		  ;  If not end, look for ","
	       (POP-C)				  ;   and pop it if it's there
	       (MREAD-SYNERR "Missing ~A"	  ;   or give an error message.
			     (MOPSTRIP MATCH)))))))

;;; (CONVERT <exp> <mode>)
;;;
;;;  Parser coercion function.
;;;
;;;  <exp>  should have the form ( <expressionmode> . <expression> )
;;;  <mode> is the target mode.
;;;
;;;  If <expressionmode> and <mode> are compatible, returns <expression>.

(DEFUN CONVERT (ITEM MODE) 
  (IF (OR (EQ MODE (CAR ITEM))		; If modes match exactly
	  (EQ '$ANY MODE)		;    or target is $ANY
	  (EQ '$ANY (CAR ITEM)))	;    or input is $ANY
      (CDR ITEM)			;  then return expression
      (MREAD-SYNERR "Found ~A expression where ~A expression expected" 
		    (GET (CAR ITEM) 'ENGLISH)
		    (GET MODE       'ENGLISH))))

(DEFPROP $ANY    "untyped"   ENGLISH)
(DEFPROP $CLAUSE "logical"   ENGLISH)
(DEFPROP $EXPR   "algebraic" ENGLISH)

;;;; Parser Error Diagnostics

 ;; Call this for random user-generated parse errors

(DEFUN PARSE-ERR () (MREAD-SYNERR "Syntax error")) 

 ;; Call this for random internal parser lossage (eg, code that shouldn't
 ;;  be reachable.)

(DEFUN PARSE-BUG-ERR (OP)
  (MREAD-SYNERR
    "Parser bug in ~A. Please report this to the Macsyma maintainers,~
   ~%including the characters you just typed which caused the error. Thanks."
    (MOPSTRIP OP)))

;;; Random shared error messages

(DEFUN DELIM-ERR (OP)
  (MREAD-SYNERR "Illegal use of delimiter ~A" (MOPSTRIP OP)))

(DEFUN ERB-ERR (OP L) L ;Ignored
  (MREAD-SYNERR "Too many ~A's" (MOPSTRIP OP)))

(DEFUN PREMTERM-ERR (OP)
  (MREAD-SYNERR "Premature termination of input at ~A."
		(MOPSTRIP OP)))

;;;; Operator Specific Data

(DEF-NUD-EQUIV |$]| DELIM-ERR)
(DEF-LED-EQUIV |$]| ERB-ERR)
(DEF-LBP     |$]| 5.)

(DEF-NUD-EQUIV	|$[| PARSE-MATCHFIX)
(DEF-MATCH	|$[| |$]|)
(DEF-LBP	|$[| 200.)
;No RBP
(DEF-MHEADER	|$[| (MLIST))
(DEF-POS	|$[| $ANY)
(DEF-LPOS	|$[| $ANY)
;No RPOS

(DEF-LED (|$[| 200.) (OP LEFT)
  (SETQ LEFT (CONVERT LEFT '$ANY))
  (IF (NUMBERP LEFT) (PARSE-ERR))			; number[...] invalid
  (LET ((header (if (atom left)
		    (add-lineinfo (LIST (AMPERCHK LEFT) 'array))
		  (add-lineinfo '(MQAPPLY ARRAY))))
		  
	(RIGHT (PRSMATCH '|$]| '$ANY)))			; get sublist in RIGHT
    (COND ((NULL RIGHT)					; 1 subscript minimum
	   (MREAD-SYNERR "No subscripts given"))
	  ((ATOM LEFT)					; atom[...]
	   (SETQ RIGHT (CONS header
			     RIGHT))
	   (CONS '$ANY (ALIASLOOKUP RIGHT)))
	  (T						; exp[...]
	   (CONS '$ANY (CONS header
			     (CONS LEFT RIGHT)))))))


(DEF-NUD-EQUIV |$)| DELIM-ERR)
(DEF-LED-EQUIV |$)| ERB-ERR)
(DEF-LBP       |$)| 5.)

(DEF-MHEADER   |$(| (MPROGN))

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

(DEF-NUD (|$(| 200.) (OP)
  (LET ((RIGHT)(hdr (MHEADER '|$(|)))        ; make mheader first for lineinfo
    (COND ((EQ '|$)| (FIRST-C)) (PARSE-ERR))		  ; () is illegal
	  ((OR (NULL (SETQ RIGHT (PRSMATCH '|$)| '$ANY))) ; No args to MPROGN??
	       (CDR RIGHT))				  ;  More than one arg.
	   (CONS '$ANY (CONS hdr RIGHT)))	  ; Return an MPROGN
	  (T (CONS '$ANY (CAR RIGHT))))))		  ; Optimize out MPROGN

(DEF-LED (|$(| 200.) (OP LEFT)
  (SETQ LEFT (CONVERT LEFT '$ANY))		        ;De-reference LEFT
  (IF (NUMBERP LEFT) (PARSE-ERR))			;number(...) illegal
  (LET ((HDR (AND (ATOM LEFT)(MHEADER (AMPERCHK LEFT))))
	(R (PRSMATCH '|$)| '$ANY))                       ;Get arglist in R
	)
    (CONS '$ANY						;Result is type $ANY
	  (COND ((ATOM LEFT)				;If atom(...) =>
		 (CONS hdr R))    ;(($atom) exp . args)
		(T				        ;Else exp(...) =>
		 (CONS '(MQAPPLY) (CONS LEFT R)))))))	;((MQAPPLY) op . args)

(DEF-MHEADER |$'| (MQUOTE))

(DEF-NUD (|$'|) (OP)
  (LET (RIGHT)
    (COND ((EQ '|$(| (FIRST-C))
	   (LIST '$ANY (MHEADER '|$'|) (PARSE '$ANY 190.)))
	  ((OR (ATOM (SETQ RIGHT (PARSE '$ANY 190.)))
	       (MEMQ (CAAR RIGHT) '(MQUOTE MLIST MPROG MPROGN LAMBDA)))
	   (LIST '$ANY (MHEADER '|$'|) RIGHT))
	  ((EQ 'MQAPPLY (CAAR RIGHT))
	   (COND ((EQ (CAAADR RIGHT) 'LAMBDA)
		  (LIST '$ANY (MHEADER '|$'|) RIGHT))
		 (T (RPLACA (CDR RIGHT)
			    (CONS (CONS ($NOUNIFY (CAAADR RIGHT))
					(CDAADR RIGHT))
				  (CDADR RIGHT)))
		    (CONS '$ANY RIGHT))))
	  (T (CONS '$ANY (CONS (CONS ($NOUNIFY (CAAR RIGHT)) (CDAR RIGHT))
			       (CDR RIGHT)))))))

(DEF-NUD (|$''|) (OP)
  (LET (RIGHT)
    (CONS '$ANY
	  (COND ((EQ '|$(| (FIRST-C))  (MEVAL (PARSE '$ANY 190.)))
		((ATOM (SETQ RIGHT (PARSE '$ANY 190.))) (MEVAL1 RIGHT))
		((EQ 'MQAPPLY (CAAR RIGHT))
		 (RPLACA (CDR RIGHT)
			 (CONS (CONS ($VERBIFY (CAAADR RIGHT)) (CDAADR RIGHT))
			       (CDADR RIGHT)))
		 RIGHT)
		(T (CONS (CONS ($VERBIFY (CAAR RIGHT)) (CDAR RIGHT))
			 (CDR RIGHT)))))))

(DEF-LED-EQUIV |$:| PARSE-INFIX)
(DEF-LBP       |$:| 180.)
(DEF-RBP       |$:|  20.)
(DEF-POS       |$:| $ANY)
(DEF-RPOS      |$:| $ANY)
(DEF-LPOS      |$:| $ANY)
(DEF-MHEADER   |$:| (MSETQ))

(DEF-LED-EQUIV |$::| PARSE-INFIX)
(DEF-LBP       |$::| 180.)
(DEF-RBP       |$::|  20.)
(DEF-POS       |$::| $ANY)
(DEF-RPOS      |$::| $ANY)
(DEF-LPOS      |$::| $ANY)
(DEF-MHEADER   |$::| (MSET))

(DEF-LED-EQUIV |$:=| PARSE-INFIX)
(DEF-LBP       |$:=| 180.)
(DEF-RBP       |$:=|  20.)
(DEF-POS       |$:=| $ANY)
(DEF-RPOS      |$:=| $ANY)
(DEF-LPOS      |$:=| $ANY)
(DEF-MHEADER   |$:=| (MDEFINE))

(DEF-LED-EQUIV |$::=| PARSE-INFIX)
(DEF-LBP       |$::=| 180.)
(DEF-RBP       |$::=|  20.)
(DEF-POS       |$::=| $ANY)
(DEF-RPOS      |$::=| $ANY)
(DEF-LPOS      |$::=| $ANY)
(DEF-MHEADER   |$::=| (MDEFMACRO))

(DEF-LED-EQUIV	|$!| PARSE-POSTFIX)
(DEF-LBP	|$!| 160.)
;No RBP
(DEF-POS	|$!| $EXPR)
(DEF-LPOS	|$!| $EXPR)
;No RPOS
(DEF-MHEADER	|$!| (MFACTORIAL))

(DEF-MHEADER |$!!| ($GENFACT))

(DEF-LED (|$!!| 160.) (OP LEFT)
  (LIST '$EXPR
	(MHEADER '$!!)
	(CONVERT LEFT '$EXPR)
	(LIST (MHEADER '#-cl $// #+cl $/ ) (CONVERT LEFT '$EXPR) 2)
	2))

(DEF-LBP     |$^| 140.) 
(DEF-RBP     |$^| 139.)
(DEF-POS     |$^| $EXPR)
(DEF-LPOS    |$^| $EXPR)
(DEF-RPOS    |$^| $EXPR)
(DEF-MHEADER |$^| (MEXPT))

(DEF-LED ((|$^| |$^^|)) (OP LEFT)
  (CONS '$EXPR 
	(ALIASLOOKUP (LIST (MHEADER OP)
			   (CONVERT LEFT (LPOS OP))
			   (PARSE (RPOS OP) (RBP OP))))))

(MAPC #'(LAMBDA (PROP) ; Make $** like $^
	  (LET ((PROPVAL (GET '$^ PROP)))
	    (IF PROPVAL (PUTPROP '$** PROPVAL PROP))))
      '(LBP RBP POS RPOS LPOS MHEADER))
(INHERIT-PROPL  '$** '$^ (LED-PROPL))

(DEF-LBP     |$^^| 140.)
(DEF-RBP     |$^^| 139.)
(DEF-POS     |$^^| $EXPR)
(DEF-LPOS    |$^^| $EXPR)
(DEF-RPOS    |$^^| $EXPR)
(DEF-MHEADER |$^^| (MNCEXPT))

;; note y^^4.z gives an error because it scans the number 4 together with
;; the trailing '.' as a decimal place.    I think the error is correct.
(DEF-LED-EQUIV	|$.| PARSE-INFIX)
(DEF-LBP	|$.| 130.)
(DEF-RBP	|$.| 129.)
(DEF-POS	|$.| $EXPR)
(DEF-LPOS	|$.| $EXPR)
(DEF-RPOS	|$.| $EXPR)
(DEF-MHEADER	|$.| (MNCTIMES))

(DEF-LED-EQUIV	|$*| PARSE-NARY)
(DEF-LBP	|$*| 120.)
;RBP not needed
(DEF-POS	|$*| $EXPR)
;RPOS not needed
(DEF-LPOS	|$*| $EXPR)
(DEF-MHEADER	|$*| (MTIMES))

(DEF-LED-EQUIV	#-cl |$//| #+cl $/  PARSE-INFIX)
(DEF-LBP	#-cl |$//| #+cl $/  120.)
(DEF-RBP	#-cl |$//| #+cl $/  120.)
(DEF-POS	#-cl |$//| #+cl $/  $EXPR)
(DEF-RPOS	#-cl |$//| #+cl $/  $EXPR)
(DEF-LPOS	#-cl |$//| #+cl $/  $EXPR)
(DEF-MHEADER	#-cl |$//| #+cl $/  (MQUOTIENT))

(DEF-NUD-EQUIV	|$+| PARSE-PREFIX)
(DEF-LBP	|$+| 100.)
(DEF-RBP	|$+| 100.)
(DEF-POS	|$+| $EXPR)
(DEF-RPOS	|$+| $EXPR)
;LPOS not needed
(DEF-MHEADER	|$+| (MPLUS))

(DEF-LED ((|$+| |$-|) 100.) (OP LEFT)
  (SETQ LEFT (CONVERT LEFT '$EXPR))
  (DO ((NL (LIST (IF (EQ OP '$-)
		     (LIST (MHEADER '$-) (PARSE '$EXPR 100.))
		     (PARSE '$EXPR 100.))
		 LEFT)
	   (CONS (PARSE '$EXPR 100.) NL)))
      ((NOT (MEMQ (FIRST-C) '($+ $-)))
       (LIST* '$EXPR (MHEADER '$+) (NREVERSE NL)))
    (IF (EQ (FIRST-C) '$+) (POP-C))))

(DEF-NUD-EQUIV	|$-| PARSE-PREFIX)
(DEF-LBP	|$-| 100.)
(DEF-RBP	|$-| 134.)
(DEF-POS	|$-| $EXPR)
(DEF-RPOS	|$-| $EXPR)
;LPOS not needed
(DEF-MHEADER	|$-| (MMINUS))

(DEF-LED-EQUIV	|$=| PARSE-INFIX)
(DEF-LBP	|$=| 80.)
(DEF-RBP	|$=| 80.)
(DEF-POS	|$=| $CLAUSE)
(DEF-RPOS	|$=| $EXPR)
(DEF-LPOS	|$=| $EXPR)
(DEF-MHEADER	|$=| (MEQUAL))

(DEF-LED-EQUIV	|$#| PARSE-INFIX)
(DEF-LBP	|$#| 80.)
(DEF-RBP	|$#| 80.)
(DEF-POS	|$#| $CLAUSE)
(DEF-RPOS	|$#| $EXPR)
(DEF-LPOS	|$#| $EXPR)
(DEF-MHEADER	|$#| (MNOTEQUAL))

(DEF-LED-EQUIV	|$>| PARSE-INFIX)
(DEF-LBP	|$>| 80.)
(DEF-RBP	|$>| 80.)
(DEF-POS	|$>| $CLAUSE)
(DEF-RPOS	|$>| $EXPR)
(DEF-LPOS	|$>| $EXPR)
(DEF-MHEADER	|$>| (MGREATERP))

(DEF-LED-EQUIV	|$>=| PARSE-INFIX)
(DEF-LBP	|$>=| 80.)
(DEF-RBP	|$>=| 80.)
(DEF-POS	|$>=| $CLAUSE)
(DEF-RPOS	|$>=| $EXPR)
(DEF-LPOS	|$>=| $EXPR)
(DEF-MHEADER	|$>=| (MGEQP))


(DEF-NUD (|$>| 80.) (OP) ; > is a single-char object
  '($ANY . |$>|))

(DEF-LED-EQUIV	|$<| PARSE-INFIX)
(DEF-LBP	|$<| 80.)
(DEF-RBP	|$<| 80.)
(DEF-POS	|$<| $CLAUSE)
(DEF-RPOS	|$<| $EXPR)
(DEF-LPOS	|$<| $EXPR)
(DEF-MHEADER	|$<| (MLESSP))

(DEF-LED-EQUIV	|$<=| PARSE-INFIX)
(DEF-LBP	|$<=| 80.)
(DEF-RBP	|$<=| 80.)
(DEF-POS	|$<=| $CLAUSE)
(DEF-RPOS	|$<=| $EXPR)
(DEF-LPOS	|$<=| $EXPR)
(DEF-MHEADER	|$<=| (MLEQP))

(DEF-NUD-EQUIV	|$NOT| PARSE-PREFIX)
;LBP not needed
(DEF-RBP	|$NOT| 70.)
(DEF-POS	|$NOT| $CLAUSE)
(DEF-RPOS	|$NOT| $CLAUSE)
(DEF-LPOS	|$NOT| $CLAUSE)
(DEF-MHEADER	|$NOT| (MNOT))

(DEF-LED-EQUIV	|$AND| PARSE-NARY)
(DEF-LBP	|$AND| 65.)
;RBP not needed
(DEF-POS	|$AND| $CLAUSE)
;RPOS not needed
(DEF-LPOS	|$AND| $CLAUSE)
(DEF-MHEADER	|$AND| (MAND))

(DEF-LED-EQUIV	|$OR| PARSE-NARY)
(DEF-LBP	|$OR| 60.)
;RBP not needed
(DEF-POS	|$OR| $CLAUSE)
;RPOS not needed
(DEF-LPOS	|$OR| $CLAUSE)
(DEF-MHEADER	|$OR| (MOR))

(DEF-LED-EQUIV	|$,| PARSE-NARY)
(DEF-LBP	|$,| 10.)
;RBP not needed
(DEF-POS	|$,| $ANY)
;RPOS not needed
(DEF-LPOS	|$,| $ANY)
(DEF-MHEADER	|$,| ($EV))

(DEF-NUD-EQUIV |$THEN| DELIM-ERR)
(DEF-LBP |$THEN| 5.)
(DEF-RBP |$THEN| 25.)

(DEF-NUD-EQUIV |$ELSE| DELIM-ERR)
(DEF-LBP |$ELSE| 5.)
(DEF-RBP |$ELSE| 25.)

(DEF-NUD-EQUIV |$ELSEIF| DELIM-ERR)
(DEF-LBP  |$ELSEIF| 5.)
(DEF-RBP  |$ELSEIF| 45.)
(DEF-POS  |$ELSEIF| $ANY)
(DEF-RPOS |$ELSEIF| $CLAUSE)

;No LBP - Default as high as possible
(DEF-RBP     $IF 45.)
(DEF-POS     $IF $ANY)
(DEF-RPOS    $IF $CLAUSE)
;No LPOS
(DEF-MHEADER $IF (MCOND))

(DEF-NUD (|$IF|) (OP)
  (LIST* (POS OP)
	 (MHEADER OP)
	 (PARSE-CONDITION OP)))

(DEFUN PARSE-CONDITION (OP)
  (LIST* (PARSE (RPOS OP) (RBP OP))
	 (IF (EQ (FIRST-C) '$THEN)
	     (PARSE '$ANY (RBP (POP-C)))
	     (MREAD-SYNERR "Missing THEN"))
	 (CASE (FIRST-C)
	   (($ELSE)   (LIST T (PARSE '$ANY (RBP (POP-C)))))
	   (($ELSEIF) (PARSE-CONDITION (POP-C)))
	   (T ; Note: $FALSE instead of () makes DISPLA suppress display!
	    (LIST T '$FALSE)))))

(DEF-MHEADER $DO (MDO))

(DEFUN PARSE-$DO (LEX &aux (left (make-mdo)))
  (setf (car LEFT) (mheader 'mdo))
  (DO ((OP LEX (POP-C))  (ACTIVE-BITMASK 0))
      (NIL)
    (IF (EQ OP '|$:|) (SETQ OP '$FROM))
    (SETQ ACTIVE-BITMASK (COLLISION-CHECK '$DO ACTIVE-BITMASK OP))
    (LET ((DATA (PARSE (RPOS OP) (RBP OP))))
      (CASE OP
	($DO		(SETF (MDO-BODY LEFT) DATA) (RETURN (CONS '$ANY LEFT)))
	($FOR		(SETF (MDO-FOR  LEFT) DATA))
	($FROM		(SETF (MDO-FROM LEFT) DATA))
	($IN		(SETF (MDO-OP   LEFT) 'MDOIN)
			(SETF (MDO-FROM LEFT) DATA))
	($STEP		(SETF (MDO-STEP LEFT) DATA))
	($NEXT		(SETF (MDO-NEXT LEFT) DATA))
	($THRU		(SETF (MDO-THRU LEFT) DATA))
	(($UNLESS $WHILE)
			(IF (EQ OP '$WHILE)
			    (SETQ DATA (LIST (MHEADER '$NOT) DATA)))
			(SETF (MDO-UNLESS LEFT)
			   (IF (NULL (MDO-UNLESS LEFT))
			       DATA
			       (LIST (MHEADER '$OR) DATA (MDO-UNLESS LEFT)))))
	(T (PARSE-BUG-ERR '$DO))))))

(DEF-LBP $FOR    25.)
(DEF-LBP $FROM   25.)
(DEF-LBP $STEP   25.)
(DEF-LBP $NEXT   25.)
(DEF-LBP $THRU   25.)
(DEF-LBP $UNLESS 25.)
(DEF-LBP $WHILE  25.)
(DEF-LBP $DO	 25.)

(DEF-NUD-EQUIV $FOR    PARSE-$DO)
(DEF-NUD-EQUIV $FROM   PARSE-$DO)
(DEF-NUD-EQUIV $STEP   PARSE-$DO)
(DEF-NUD-EQUIV $NEXT   PARSE-$DO)
(DEF-NUD-EQUIV $THRU   PARSE-$DO)
(DEF-NUD-EQUIV $UNLESS PARSE-$DO)
(DEF-NUD-EQUIV $WHILE  PARSE-$DO)
(DEF-NUD-EQUIV $DO     PARSE-$DO)

(DEF-RBP $DO      25.)
(DEF-RBP $FOR    200.)
(DEF-RBP $FROM    95.)
(DEF-RBP $IN      95.)
(DEF-RBP $STEP    95.)
(DEF-RBP $NEXT    45.)
(DEF-RBP $THRU    95.)
(DEF-RBP $UNLESS  45.)
(DEF-RBP $WHILE	  45.)

(DEF-RPOS $DO     $ANY)
(DEF-RPOS $FOR    $ANY)
(DEF-RPOS $FROM   $ANY)
(DEF-RPOS $STEP   $EXPR)
(DEF-RPOS $NEXT   $ANY)
(DEF-RPOS $THRU   $EXPR)
(DEF-RPOS $UNLESS $CLAUSE)
(DEF-RPOS $WHILE  $CLAUSE)


(DEF-COLLISIONS $DO
  ($DO	   . ())
  ($FOR    . ($FOR))
  ($FROM   . ($IN $FROM))
  ($IN     . ($IN $FROM $STEP $NEXT))
  ($STEP   . ($IN       $STEP $NEXT))
  ($NEXT   . ($IN	$STEP $NEXT))
  ($THRU   . ($IN $THRU)) ;$IN didn't used to get checked for
  ($UNLESS . ())
  ($WHILE  . ()))

;#+ti  ;;because of a bug the preceding doesn't give this..
;(defprop $do (($WHILE . 256) ($UNLESS . 128)
;                ($THRU . 64)
;                ($NEXT . 32)
;                ($STEP . 16)
;                ($IN . 8)
;                ($FROM . 4)
;                ($FOR . 2)
;                ($DO . 1)) keys)


(DEF-MHEADER   |$$| (NODISPLAYINPUT))
(DEF-NUD-EQUIV |$$| PREMTERM-ERR)
(DEF-LBP       |$$| -1)
;No RBP, POS, RPOS, RBP, or MHEADER

(DEF-MHEADER   |$;| (DISPLAYINPUT))
(DEF-NUD-EQUIV |$;| PREMTERM-ERR)
(DEF-LBP       |$;| -1)
;No RBP, POS, RPOS, RBP, or MHEADER

(DEF-NUD-EQUIV  |$&&| DELIM-ERR)
(DEF-LBP	|$&&| -1)

(defun MOPSTRIP (x)
  ;; kludge interface function to allow the use of lisp PRINC in places.
  (COND ((NULL X) 'FALSE)
	((OR (EQ X T) (EQ X 'T)) 'TRUE)
	((NUMBERP X) X)
	((SYMBOLP X)
	 (OR (GET X 'REVERSEALIAS)
	     (IF (IMEMBER (FIRSTCHARN X) '(#\$ #\% #\&))
		 (IMPLODE (CDR (EXPLODEN X)))
		 X)))
	(T (MAKNAM (MSTRING X)))))
	
(DEFINE-INITIAL-SYMBOLS
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
(defmacro upcase (operator)
 `(setq operator (intern (string-upcase (string ,operator)))))

(DEFMFUN $PREFIX (OPERATOR &OPTIONAL (RBP  180.)
			             (RPOS '$ANY)
				     (POS  '$ANY))
	 (upcase operator)
  (DEF-OPERATOR OPERATOR POS ()  ()     RBP RPOS () T
    '(NUD . PARSE-PREFIX) 'MSIZE-PREFIX 'DIMENSION-PREFIX ()   ))

(DEFMFUN $POSTFIX (OPERATOR &OPTIONAL (LBP  180.)
			             (LPOS '$ANY)
				     (POS  '$ANY))
	 	 (upcase operator)
  (DEF-OPERATOR OPERATOR POS LBP LPOS   ()  ()   T  ()
    '(LED . PARSE-POSTFIX) 'MSIZE-POSTFIX 'DIMENSION-POSTFIX  ()   ))

(DEFMFUN $INFIX  (OPERATOR &OPTIONAL (LBP  180.)
			             (RBP  180.)
				     (LPOS '$ANY)
				     (RPOS '$ANY)
				     (POS  '$ANY))
	 	 (upcase operator)
  (DEF-OPERATOR OPERATOR POS LBP LPOS   RBP RPOS T T
    '(LED . PARSE-INFIX) 'MSIZE-INFIX 'DIMENSION-INFIX () ))

(DEFMFUN $NARY   (OPERATOR &OPTIONAL (BP     180.)
			             (ARGPOS '$ANY)
				     (POS    '$ANY))
	 	 (upcase operator)
  (DEF-OPERATOR OPERATOR POS BP  ARGPOS BP  ()   T T
    '(LED . PARSE-NARY) 'MSIZE-NARY 'DIMENSION-NARY () ))

(DEFMFUN $MATCHFIX (OPERATOR
		    MATCH  &OPTIONAL (ARGPOS '$ANY)
				     (POS    '$ANY))
  ;shouldn't MATCH be optional?
	 	 (upcase operator)
  (DEF-OPERATOR OPERATOR POS ()  ARGPOS ()  ()  () () 
    '(NUD . PARSE-MATCHFIX) 'MSIZE-MATCHFIX 'DIMENSION-MATCH MATCH))

(DEFMFUN $NOFIX  (OPERATOR &OPTIONAL (POS '$ANY))
	 	 (upcase operator)
  (DEF-OPERATOR OPERATOR POS ()  ()     ()  () () ()
    '(NUD . PARSE-NOFIX) 'MSIZE-NOFIX 'DIMENSION-NOFIX ()   ))

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

(DEFUN DEF-OPERATOR (OP POS LBP LPOS RBP RPOS SP1 SP2
			PARSE-DATA GRIND-FN DIM-FN MATCH)
  (LET ((X))
    (IF (OR (AND RBP (NOT (INTEGERP (SETQ X RBP))))
	    (AND LBP (NOT (INTEGERP (SETQ X LBP)))))
	(MERROR "Binding powers must be integers.~%~M is not an integer." X))
    (IF (MSTRINGP OP) (SETQ OP (DEFINE-SYMBOL OP)))
    (OP-SETUP OP)
    (LET ((NOUN   ($NOUNIFY OP))
	  (DISSYM (CDR (EXPLODEN OP))))
      (cond
       ((NOT MATCH)
	(SETQ DISSYM (APPEND (IF SP1 '(#\Space)) DISSYM (IF SP2 '(#\Space)))))
       (t (IF (MSTRINGP MATCH) (SETQ MATCH (DEFINE-SYMBOL MATCH)))
	  (OP-SETUP MATCH)
	  (PUTPROP OP    MATCH 'MATCH)
	  (PUTPROP MATCH 5.    'LBP)
	  (SETQ DISSYM (CONS DISSYM (CDR (EXPLODEN MATCH))))))
      (PUTPROP OP POS 'POS)
      (PUTPROP OP (CDR PARSE-DATA) (CAR PARSE-DATA))
      (PUTPROP OP   GRIND-FN  'GRIND)
      (PUTPROP OP   DIM-FN    'DIMENSION)
      (PUTPROP NOUN DIM-FN    'DIMENSION)
      (PUTPROP OP   DISSYM 'DISSYM)
      (PUTPROP NOUN DISSYM 'DISSYM)
      (WHEN RBP
	(PUTPROP OP   RBP  'RBP)
	(PUTPROP NOUN RBP  'RBP))
      (WHEN LBP
	(PUTPROP OP   LBP  'LBP)
	(PUTPROP NOUN LBP  'LBP))
      (WHEN LPOS (PUTPROP OP   LPOS 'LPOS))
      (WHEN RPOS (PUTPROP OP   RPOS 'RPOS))
      (GETOPR OP))))

(DEFUN OP-SETUP (OP)
  (declare (special mopl))
  (LET ((DUMMY (OR (GET OP 'OP)
		   (IMPLODE (CONS '& (STRING* OP))))))
    (PUTPROP OP    DUMMY 'OP )
    (PUTPROP DUMMY OP    'OPR)
    (IF (AND (OPERATORP1 OP) (NOT (MEMQ DUMMY (CDR $PROPS))))
	(PUSH DUMMY MOPL))
    (ADD2LNC DUMMY $PROPS)))

(DEFUN KILL-OPERATOR (OP)
  (UNDEFINE-SYMBOL (STRIPDOLLAR OP))
  (LET ((OPR (GET OP 'OP)) (NOUN-FORM ($NOUNIFY OP)))
    (REMPROP OPR 'OPR)
    (REMPROPCHK OPR)
    (MAPC #'(LAMBDA (X) (REMPROP OP X))
 	  '(NUD-EXPR NUD-SUBR			; NUD info
		     LED LED-EXPR LED-SUBR		; LED info
		     LBP RBP			; Binding power info
		     LPOS RPOS POS		; Part-Of-Speech info
		     GRIND DIMENSION DISSYM	; Display info
		     OP
		     ))			; Operator info
    (MAPC #'(LAMBDA (X) (REMPROP NOUN-FORM X))
 	  '(DIMENSION DISSYM LBP RBP))))



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
