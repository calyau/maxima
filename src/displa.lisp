;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
(macsyma-module displa)

;; N.B. You must read the macro file before reading this file.

(load-macsyma-macros displm)

(declare-top (special $LINEL))
;; Read time parameters.  ITS only.

#+(or ITS NIL)				;sigh (inside of #.)
(eval-when (eval compile)
  (SETQ %TDCRL #o207 %TDLF #o212 %TDQOT #o215 %TDMV0 #o217))

;; Global variables defined in this file.  Most of these are switches
;; controlling display format

(DEFMVAR CHARACTER-GRAPHICS-TTY NIL
	 "If T, then console can draw lines and math symbols using
	 an extended character set.")

(DEFMVAR LINE-GRAPHICS-TTY NIL
	 "If T, then console can draw lines and math symbols using
	 vector graphics.")

(DEFMVAR $CURSORDISP T
	 "If T, causes expressions to be drawn by the displayer in logical 
	  sequence.  This only works with a console which can do cursor 
	  movement.
	  If NIL, expressions are simply printed line by line.
	  CURSORDISP is NIL when a WRITEFILE is in effect."
	 NO-RESET)

(DEFMVAR $STARDISP NIL
	 "Causes factors of products are separated by * when displayed.")

(DEFMVAR $LEFTJUST NIL
	 "Causes equations to be drawn left justified rather than centered.
	 For slow consoles.")

(DEFMVAR $DISPLAY2D T
	 "Causes equations to be drawn in two dimensions.  Otherwise, drawn
	 linearly.")

(DEFMVAR $LISPDISP NIL
	 "Causes symbols not having $ as the first character in their pnames
	 to be preceded with a ? when displayed.")

;; This may be flushed in the future if nobody dislikes the graphics crocks.

(DEFMVAR $LINEDISP T
	 "Causes quotients, matrices, and boxes to be drawn with straight
	 lines, if possible.  This will work on graphic terminals or
	 video terminals with line drawing character sets.  If enabled,
	 the values of LMXCHAR, RMXCHAR, ABSBOXCHAR, and BOXCHAR are ignored.")

(DEFMVAR $DERIVABBREV NIL)

(DEFMVAR $NOUNDISP NIL)

(DEFMVAR STRINGDISP NIL
	 "Causes strings to be bracketed in double quotes when displayed.
	 Normally this is off, but is turned on when a procedure definition is
	 being displayed.")
#+Franz
(defmvar $typeset nil
 	"Causes equations to be output in a typesetter readable form if t.")

(DEFMVAR DISPLAYP NIL "Is T when inside of DISPLA")

;; More messages which appear during the middle of display.  Different
;; from those which appear during typein.  MOREMSG and MOREFLUSH get
;; bound to these.

(DEFVAR D-MOREMSG "--More Display?--")
(DEFVAR D-MOREFLUSH "--Display Flushed--")

;; Parameters which control how boxes, absolute value signs,
;; evaluation-at-a-point bars, and matrices are drawn.

(DEFMVAR $BOXCHAR '|&"|
  "Character used for drawing boxes.")
(DEFMVAR $ABSBOXCHAR '|&!|
  "Character used for drawing absolute value signs and 'evaluation at' signs.")
(DEFMVAR $LMXCHAR '|&[|
  "Character used for drawing the left edge of a matrix.")
(DEFMVAR $RMXCHAR '|&]|
  "Character used for drawing the right edge of a matrix.")

;; These variables are bound within Macsyma Listeners since they are different
;; for each window.  Set them here, anyway, so that RETRIEVE can be called from
;; top level.  The size of TOP-WINDOW is wired in here.

#+lispm
(SETQ SMART-TTY T RUBOUT-TTY T LINE-GRAPHICS-TTY t SCROLLP NIL)

#+(and cl (not lispm))
(SETQ SMART-TTY nil RUBOUT-TTY nil LINE-GRAPHICS-TTY nil SCROLLP t)


#+cl 
(setq   LINEL 79. $LINEL 79. TTYHEIGHT 24.)
#+lispm
(multiple-value-bind (a b)
  (send *terminal-io* :send-if-handles :size-in-characters)
  (when (and a b) (setq linel a ttyheight b)
    (setq linel 79 ttyheight 24)))

#+NIL
(multiple-value-bind (a b)
  (send *terminal-io* :send-if-handles :size-in-characters)
  (when (and a b) (setq linel a ttyheight b)
    (setq linel 79 ttyheight 24))
  (setq smart-tty nil rubout-tty nil scrollp t))

;; Default settings for random systems.
#-(OR ITS LISPM cl)
(SETQ SMART-TTY NIL RUBOUT-TTY NIL SCROLLP T
      LINEL 79. $LINEL 79. TTYHEIGHT 24.)




(DEFVAR LINEARRAY (MAKE-array 80.))


(DEFMFUN MAXIMA-DISPLAY (FORM &key (stream *standard-output*) )
	 (let ((*standard-output* stream))
	   (displa form)))

(defun maxima-draw-form (form &key (stream *standard-output*) (at-x 0) (at-y 0) 
			      $linedisp $cursordisp &aux dim-list prev-x prev-y )
  "First try at getting an interface to allow one to draw a form at any 
  position. The at-x and at-y amount to the initial position which will be in
  the middle left of a matrix, or the main line for a polynomial.  On a stream
  which does no cursorpositioning it would be top left corner at the call and
  spaced over by at-y. It can't tell where it is in the line, already so you have to tell it
  where to begin, or if it occurs in a format command go back to last % to get offset."
  (desetq (prev-x . prev-y) (cursorpos))
  (let ((*standard-output* stream) )
    (unwind-protect
    (let ((MRATP (CHECKRAT FORM))
	  (#.WRITEFILEP #.WRITEFILEP)
	  (MAXHT     1) (MAXDP   0) (WIDTH   0)
	  (HEIGHT    0) (DEPTH   0) (LEVEL   0) (SIZE   2)
	  (BREAK     0) (RIGHT   0) (LINES   1) BKPT
	  (BKPTWD    0) (BKPTHT  1) (BKPTDP  0) (BKPTOUT 0)
	  (BKPTLEVEL 0) IN-P
	  (MOREFLUSH D-MOREFLUSH)
	  MORE-^W
	  (MOREMSG D-MOREMSG))
      (PROGN (SETQ dim-list (DIMENSION FORM
				       NIL 'MPAREN 'MPAREN 0 0)))
    (cond ($cursordisp  (draw-2d (nreverse dim-list) at-x at-y))
	  (t (cursorpos (f- at-x (sub1 height)) 0)
	   (draw-linear (nreverse dim-list) (f+ at-x height) at-y)
	   
	   (sloop for i downfrom (sub1 (length linearray)) to 0
		 when (aref linearray i)
		 do (output-linear-one-line i)))))
            (clear-linearray))
	    (cursorpos prev-x prev-y)))

(defmacro bind-dimension (form dim-list &rest body)
  
  `(let ((MRATP (CHECKRAT ,FORM))
	 (#.WRITEFILEP #.WRITEFILEP)
	 (MAXHT     1) (MAXDP   0) (WIDTH   0)
	 (HEIGHT    0) (DEPTH   0) (LEVEL   0) (SIZE   2)
	 (BREAK     0) (RIGHT   0) (LINES   1) BKPT
	 (BKPTWD    0) (BKPTHT  1) (BKPTDP  0) (BKPTOUT 0)
	 (BKPTLEVEL 0) IN-P
	 (MOREFLUSH D-MOREFLUSH)
	 MORE-^W
	 (MOREMSG D-MOREMSG))
     (PROGN (SETQ ,dim-list (DIMENSION ,FORM
				       NIL 'MPAREN 'MPAREN 0 0))
	    ,@ body)))

(DEFMFUN DISPLA (FORM &aux #+kcl(form form))
  (IF (OR (NOT #.TTYOFF) #.WRITEFILEP)
      (cond #+Franz ($typeset (apply #'$photot (list form)))
	    ($DISPLAY2D
	     (LET ((DISPLAYP T)
		   (LINEARRAY (IF DISPLAYP (MAKE-array 80.) LINEARRAY))
		   (MRATP (CHECKRAT FORM))
		   (#.WRITEFILEP #.WRITEFILEP)
		   (MAXHT     1) (MAXDP   0) (WIDTH   0)
		   (HEIGHT    0) (DEPTH   0) (LEVEL   0) (SIZE   2)
		   (BREAK     0) (RIGHT   0) (LINES   1) BKPT
		   (BKPTWD    0) (BKPTHT  1) (BKPTDP  0) (BKPTOUT 0)
		   (BKPTLEVEL 0) IN-P
		   (MOREFLUSH D-MOREFLUSH)
		   MORE-^W
		   (MOREMSG D-MOREMSG))
	       (UNWIND-PROTECT
		(PROGN (SETQ FORM (DIMENSION FORM
					     NIL 'MPAREN 'MPAREN 0 0))
		       (CHECKBREAK FORM WIDTH)
		       (OUTPUT FORM (IF (AND (NOT $LEFTJUST) (= 2 LINES))
					(f- LINEL (f- WIDTH BKPTOUT))
					0))
		       (IF (AND SMART-TTY (NOT (AND SCROLLP (NOT $CURSORDISP)))
				(> (CAR (CURSORPOS)) (f- TTYHEIGHT 3)))
			   (LET (#.writefilep) (MTERPRI))))
	     ;; make sure the linearray gets cleared out.
	     (CLEAR-LINEARRAY))))
	    (T (LINEAR-DISPLA FORM)))))

(defun transform-extends (x)
  (cond (($extendp x)
	 (let ((nom (send x ':macsyma-extend-type)))
	   (append `((${) ,nom)
		   (mapcar #'(lambda (u v) (list '(mequal) u (transform-extends v)))
			   #-cl
			   (cdr (mfunction-call $get nom '$accessors))
			   #+cl     ;;wouldn't compile because mfunction-call not defined
			   (cdr (funcall '$get nom '$acessors))
			   (listarray (send x ':macsyma-extend-elements))))))
	((atom x) x)
	(t
	 (do ((obj x (cdr obj))
	      (new x)
	      (slot 0 (f1+ slot)))
	     ((null obj) new)
	     (let* ((element (car obj))
		    (result (transform-extends element)))
	       (if (not (eq element result))
		   (block nil
		     (if (eq new x) (setq new (copy-seq x)))
		     (setf (nth slot new) result))))))))

(DEFMVAR $DISPLAY_FORMAT_INTERNAL NIL
	 "Setting this TRUE can help give the user a greater understanding
	 of the behavior of macsyma on certain of his problems,
	 especially those involving roots and quotients")

(DEFUN NFORMAT-CHECK (FORM)
  (IF (AND $DISPLAY_FORMAT_INTERNAL
	   (NOT (OR (ATOM FORM) (ATOM (CAR FORM)) (SPECREPP FORM))))
      FORM
      (NFORMAT FORM)))

(DEFUN DIMENSION (FORM RESULT LOP ROP W RIGHT)
  (LET ((LEVEL (f1+ LEVEL)) (BREAK (IF (AND W BREAK) (f+ W BREAK))))
    (SETQ FORM (NFORMAT-CHECK FORM))
    (COND ((ATOM FORM)
	   (DIMENSION-ATOM FORM RESULT))
	  ((AND (ATOM (CAR FORM)) (SETQ FORM (CONS '(MPROGN) FORM)) NIL))
	  ((OR (<= (LBP (CAAR FORM)) (RBP LOP)) (> (LBP ROP) (RBP (CAAR FORM))))
	   (DIMENSION-PAREN FORM RESULT))
	  ((MEMQ 'array (CAR FORM)) (DIMENSION-ARRAY FORM RESULT))
	  ((safe-GET (CAAR FORM) 'DIMENSION) 
	   (FUNCALL (GET (CAAR FORM) 'DIMENSION) FORM RESULT))
	  (T (DIMENSION-FUNCTION FORM RESULT)))))

(DEFVAR ATOM-CONTEXT 'DIMENSION-LIST)
;; bound by DIMENSION-ARRAY and DIMENSION-FUNCTION.
;; This ATOM-CONTEXT put in by GJC so that MCW could have a clean
;; hook by which to write his extensions for vector-underbars.

(declare-top (*EXPR DIMENSION-ARRAY-OBJECT)) ; to be defined someplace else.

;; Referenced externally by RAT;FLOAT.

(DEFMFUN DIMENSION-ATOM (FORM RESULT)
	 (COND ((AND (SYMBOLP FORM) (GET FORM ATOM-CONTEXT))
		(FUNCALL (GET FORM ATOM-CONTEXT) FORM RESULT))
	     #+(or CL NIL)
	       ((STRINGP FORM) (DIMENSION-STRING (MAKESTRING FORM) RESULT))
	       ((ml-typep FORM 'array)
		  (DIMENSION-ARRAY-OBJECT FORM RESULT))
	       (T (DIMENSION-STRING (MAKESTRING FORM) RESULT))))

;; Referenced externally by anyone who wants to display something as
;; a funny looking atom, e.g. Trace, Mformat.

(DEFMFUN DIMENSION-STRING (DUMMY RESULT &AUX CRP)
	 ;; N.B. String is a list of fixnums.
	 (SETQ WIDTH 0 HEIGHT 1 DEPTH 0)
	 (DO ((L DUMMY (CDR L))) ((NULL L))
	     (INCREMENT WIDTH)
	     (IF (char= (CAR L) #\NEWLINE) (SETQ CRP T)))
	 (cond
	  ((OR (AND (CHECKFIT WIDTH) (NOT CRP)) (NOT BREAK))
	   (NRECONC DUMMY RESULT))
	  (t (SETQ WIDTH 0)
	     (DO ((L DUMMY) (W (f- LINEL (f- BREAK BKPTOUT))))
		 ((NULL L) (CHECKBREAK RESULT WIDTH) RESULT)
		 (SETQ DUMMY L L (CDR L))
		 (COND ((char= (CAR DUMMY) #\NEWLINE)
			(FORCEBREAK RESULT WIDTH)
			(SETQ RESULT NIL W (f+ LINEL WIDTH)))
		       (T (INCREMENT WIDTH)
			  (WHEN (AND (= W WIDTH) L)
				(FORCEBREAK (CONS #\# RESULT) WIDTH)
				(SETQ RESULT NIL W (f+ LINEL WIDTH))
				(INCREMENT WIDTH))
			  (SETQ RESULT (RPLACD DUMMY RESULT))))))))

(DEFMFUN MAKESTRING (ATOM)
  (LET (DUMMY)
    (COND ((NUMBERP ATOM) (EXPLODEN ATOM))
	  #+(or NIL CL)
	  ((NOT (SYMBOLP ATOM)) (EXPLODEN ATOM))
	  ((AND (SETQ DUMMY (GET ATOM 'REVERSEALIAS))
		(NOT (AND (MEMQ ATOM $ALIASES) (GET ATOM 'NOUN))))
	   (EXPLODEN DUMMY))
	  ((NOT (EQ (GETOP ATOM) ATOM))
	   (SETQ DUMMY (EXPLODEN (GETOP ATOM)))
	   (IF (char= #\& (CAR DUMMY))
	       (CONS DOUBLE-QUOTE-CHAR (NCONC (CDR DUMMY) (LIST DOUBLE-QUOTE-CHAR)))
	       (CDR DUMMY)))
	  (T (SETQ DUMMY (EXPLODEN ATOM))
	     (COND ((char= #\$ (CAR DUMMY)) (CDR DUMMY))
		   ((AND STRINGDISP (char= #\& (CAR DUMMY)))
		    (CONS DOUBLE-QUOTE-CHAR (NCONC (CDR DUMMY) (LIST DOUBLE-QUOTE-CHAR))))
		   ((OR (char= #\% (CAR DUMMY)) (char= #\& (CAR DUMMY))) (CDR DUMMY))
		   ($LISPDISP (CONS #\? DUMMY))
		   (T DUMMY))))))

(DEFUN DIMENSION-PAREN (FORM RESULT)
  (SETQ RESULT (CONS RIGHT-PARENTHESES-CHAR (DIMENSION FORM (CONS LEFT-PARENTHESES-CHAR RESULT) 'MPAREN 'MPAREN 1 (f1+ RIGHT))))
  (SETQ WIDTH (f+ 2 WIDTH))
  RESULT)

(DEFUN DIMENSION-ARRAY (X RESULT)
  (PROG (DUMMY BAS (W 0) (H 0) (D 0) SUB) (DECLARE (FIXNUM W H D))
    ;(SETQ W 0)
    (IF (EQ (CAAR X) 'MQAPPLY) (SETQ DUMMY (CADR X) X (CDR X))
			       (SETQ DUMMY (CAAR X)))
    (COND ((OR (NOT $NOUNDISP) (NOT (SYMBOLP (CAAR X)))))
	  ((AND (GET (CAAR X) 'VERB) (GET (CAAR X) 'ALIAS))
	   (PUSH-STRING "''" RESULT) (SETQ W 2))
	  ((AND (GET (CAAR X) 'NOUN) (NOT (MEMQ (CAAR X) (CDR $ALIASES)))
		(NOT (GET (CAAR X) 'REVERSEALIAS)))
	   (SETQ RESULT (CONS #\' RESULT) W 1)))
    (SETQ SUB (LET ((LOP 'MPAREN) (ROP 'MPAREN) (BREAK NIL) (SIZE 1))
		   (DIMENSION-LIST X NIL))
	  W (f+ W WIDTH) H HEIGHT D DEPTH)
    (SETQ BAS (IF (AND (NOT (ATOM DUMMY)) (MEMQ 'array (CAR DUMMY)))
		  (LET ((BREAK NIL) (RIGHT 0)) (DIMENSION-PAREN DUMMY RESULT))
		  (LET ((ATOM-CONTEXT 'DIMENSION-ARRAY))
		       (DIMENSION DUMMY RESULT LOP 'MFUNCTION NIL 0))))
    (COND ((NOT (CHECKFIT (SETQ WIDTH (f+ W WIDTH))))
	   (RETURN (DIMENSION-FUNCTION (CONS '(SUBSCRIPT) (CONS DUMMY (CDR X))) RESULT)))
	  ((char= RIGHT-PARENTHESES-CHAR (CAR BAS))
	   (SETQ RESULT (CONS (CONS 0 (CONS (f- H) SUB)) BAS) DEPTH (MAX (f+ H D) DEPTH)))
	  (T (SETQ RESULT (CONS (CONS 0 (CONS (f- (f+ DEPTH H)) SUB)) BAS)
		   DEPTH (f+ H D DEPTH))))
    (UPDATE-HEIGHTS HEIGHT DEPTH)
    (RETURN RESULT)))

(DEFUN DIMENSION-FUNCTION (X RESULT)
  (PROG (FUN (W 0) (H 0) (D 0)) (DECLARE (FIXNUM W H D))
    ;(SETQ W 0)
    (COND ((or (NOT $NOUNDISP) (not (symbolp (caar x)))))
	  ((AND (GET (CAAR X) 'VERB) (GET (CAAR X) 'ALIAS))
	   (PUSH-STRING "''" RESULT) (SETQ W 2))
	  ((AND (GET (CAAR X) 'NOUN) (NOT (MEMQ (CAAR X) (CDR $ALIASES)))
		(NOT (GET (CAAR X) 'REVERSEALIAS)))
	   (SETQ RESULT (CONS #\' RESULT) W 1)))
    (IF (EQ (CAAR X) 'MQAPPLY) (SETQ FUN (CADR X) X (CDR X)) (SETQ FUN (CAAR X)))
    (SETQ RESULT (LET ((ATOM-CONTEXT 'DIMENSION-FUNCTION))
		      (DIMENSION FUN RESULT LOP 'MPAREN 0 1))
	  W (f+ W WIDTH) H HEIGHT D DEPTH)
    (COND ((NULL (CDR X))
	   (SETQ RESULT (LIST* RIGHT-PARENTHESES-CHAR LEFT-PARENTHESES-CHAR RESULT) WIDTH (f+ 2 W)))
	  (T (SETQ RESULT (LET ((LOP 'MPAREN) (ROP 'MPAREN)
				(BREAK (IF BREAK (f+ 1 W BREAK))))
			       (CONS RIGHT-PARENTHESES-CHAR (DIMENSION-LIST X (CONS LEFT-PARENTHESES-CHAR RESULT))))
		   WIDTH (f+ 2 W WIDTH) HEIGHT (MAX H HEIGHT) DEPTH (MAX D DEPTH))))
    (RETURN RESULT)))

(DEFMFUN DIMENSION-PREFIX (FORM RESULT)
  (PROG (DISSYM (SYMLENGTH 0))
	(DECLARE (FIXNUM SYMLENGTH))
	(SETQ DISSYM (safe-GET (CAAR FORM) 'DISSYM) SYMLENGTH (LENGTH DISSYM))
	(SETQ RESULT
	      (DIMENSION (CADR FORM) (RECONC DISSYM RESULT) (CAAR FORM) ROP SYMLENGTH RIGHT)
	      WIDTH (f+ SYMLENGTH WIDTH))
	(RETURN RESULT)))

(DEFUN DIMENSION-LIST (FORM RESULT)
  (PROG ((W 0) (H 0) (D 0))
	(DECLARE (FIXNUM W H D))
	(SETQ RESULT (DIMENSION (CADR FORM) RESULT LOP 'MCOMMA 0 RIGHT)
	      W WIDTH H HEIGHT D DEPTH)
	(DO ((L (CDDR FORM) (CDR L))) ((NULL L))
	    (PUSH-STRING ", " RESULT)
	    (INCREMENT W 2)
	    (CHECKBREAK RESULT W)
	    (SETQ RESULT (DIMENSION (CAR L) RESULT 'MCOMMA 'MCOMMA W RIGHT)
		  W (f+ W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH)))
	(SETQ WIDTH W HEIGHT H DEPTH D)
	(RETURN RESULT)))

(DEFMFUN DIMENSION-INFIX (FORM RESULT)
  (IF (OR (NULL (CDDR FORM)) (CDDDR FORM)) (WNA-ERR (CAAR FORM)))
  (PROG (DISSYM (SYMLENGTH 0) (W 0) (H 0) (D 0))
	(DECLARE (FIXNUM SYMLENGTH W H D))
	(SETQ DISSYM (safe-GET (CAAR FORM) 'DISSYM) SYMLENGTH (LENGTH DISSYM)
	      RESULT (DIMENSION (CADR FORM) RESULT LOP (CAAR FORM) 0 SYMLENGTH)
	      W WIDTH H HEIGHT D DEPTH)
	(SETQ RESULT (RECONC DISSYM RESULT))
	(CHECKBREAK RESULT (f+ SYMLENGTH W))
	(SETQ RESULT (DIMENSION (CADDR FORM) RESULT (CAAR FORM) ROP (f+ SYMLENGTH W) RIGHT)
	      WIDTH (f+ W SYMLENGTH WIDTH) HEIGHT (MAX H HEIGHT) DEPTH (MAX D DEPTH))
	(RETURN RESULT)))

(DEFMFUN DIMENSION-NARY (FORM RESULT)
  ;; If only 0 or 1 arguments, then print "*"() or "*"(A)
  (COND ((NULL (CDDR FORM)) (DIMENSION-FUNCTION FORM RESULT))
	(T (PROG (DISSYM (SYMLENGTH 0) (W 0) (H 0) (D 0))
	    (DECLARE (FIXNUM SYMLENGTH W H D))
	    (SETQ DISSYM (safe-GET (CAAR FORM) 'DISSYM)
		  SYMLENGTH (LENGTH DISSYM)
		  RESULT (DIMNARY (CADR FORM) RESULT LOP (CAAR FORM) (CAAR FORM) 0)
		  W WIDTH H HEIGHT D DEPTH)
	    (DO ((L (CDDR FORM) (CDR L))) (NIL)
		(CHECKBREAK RESULT W)
		(SETQ RESULT (RECONC DISSYM RESULT) W (f+ SYMLENGTH W))
		(COND ((NULL (CDR L))
		       (SETQ RESULT (DIMNARY (CAR L) RESULT (CAAR FORM) (CAAR FORM) ROP W)
			     WIDTH (f+ W WIDTH) HEIGHT (MAX H HEIGHT) DEPTH (MAX D DEPTH))
		       (RETURN T))
		      (T (SETQ RESULT (DIMNARY (CAR L) RESULT (CAAR FORM)
					       (CAAR FORM) (CAAR FORM) W)
			       W (f+ W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH)))))
	    (RETURN RESULT)))))

;; Check for (f* A (f* B C)) --> A*(B*C)

(DEFUN DIMNARY (FORM RESULT LOP OP ROP W)
  (IF (AND (NOT (ATOM FORM)) (EQ (CAAR FORM) OP))
      (DIMENSION-PAREN FORM RESULT)
      (DIMENSION FORM RESULT LOP ROP W RIGHT)))

(DEFMFUN DIMENSION-POSTFIX (FORM RESULT)
  (PROG (DISSYM (SYMLENGTH 0)) (DECLARE (FIXNUM SYMLENGTH))
	(SETQ DISSYM (safe-GET (CAAR FORM) 'DISSYM) SYMLENGTH (LENGTH DISSYM))
	(SETQ RESULT (DIMENSION (CADR FORM) RESULT LOP (CAAR FORM) 0 (f+ SYMLENGTH RIGHT))
	      WIDTH (f+ SYMLENGTH WIDTH))
	(RETURN (RECONC DISSYM RESULT))))

(DEFMFUN DIMENSION-NOFIX (FORM RESULT)
  (SETQ FORM (safe-GET (CAAR FORM) 'DISSYM) WIDTH (LENGTH FORM))
  (RECONC FORM RESULT))

(DEFUN DIMENSION-MATCH (FORM RESULT)
  (PROG (DISSYM (SYMLENGTH 0))
	(DECLARE (FIXNUM SYMLENGTH))
	(SETQ DISSYM (safe-GET (CAAR FORM) 'DISSYM) SYMLENGTH (LENGTH (CAR DISSYM)))
	(COND ((NULL (CDR FORM))
	       (SETQ WIDTH (f+ SYMLENGTH (LENGTH (CDR DISSYM))) HEIGHT 1 DEPTH 0)
	       (RETURN (RECONC (CDR DISSYM) (RECONC (CAR DISSYM) RESULT))))
	      (T (SETQ RESULT (LET ((LOP 'MPAREN)
				    (ROP 'MPAREN)
				    (BREAK (IF BREAK (f+ SYMLENGTH BREAK)))
				    (RIGHT (f+ SYMLENGTH RIGHT)))
				(DIMENSION-LIST FORM (RECONC (CAR DISSYM) RESULT))))
		 (SETQ WIDTH (f+ (LENGTH (CDR DISSYM)) SYMLENGTH WIDTH))
		 (RETURN (RECONC (CDR DISSYM) RESULT))))))

(DEFMFUN DIMENSION-SUPERSCRIPT (FORM RESULT)
  (PROG (EXP (W 0) (H 0) (D 0) BAS)
    (DECLARE (FIXNUM W H D ))
    (SETQ EXP (LET ((SIZE 1)) (DIMENSION (CADDR FORM) NIL 'MPAREN 'MPAREN NIL 0))
	  W WIDTH H HEIGHT D DEPTH)
    (COND ((AND (NOT (ATOM (CADR FORM))) (MEMQ 'array (CDAADR FORM)))
	   (PROG (SUB (W2 0) (H2 0) (D2 0))
		 (DECLARE (FIXNUM W2 H2 D2))
	     (SETQ SUB (IF (EQ 'MQAPPLY (CAAADR FORM))
			   (CDADR FORM) (CADR FORM)))
	     (SETQ SUB (LET ((LOP 'MPAREN) (BREAK NIL) (SIZE 1))
			 (DIMENSION-LIST SUB NIL))
		   W2 WIDTH H2 HEIGHT D2 DEPTH)
	     (SETQ BAS (DIMENSION (MOP (CADR FORM)) RESULT LOP 'MEXPT NIL 0))
	     (WHEN (NOT (CHECKFIT (f+ WIDTH (MAX W W2))))
	        (SETQ RESULT (DIMENSION-FUNCTION (CONS '($EXPT) (CDR FORM)) RESULT))
	        (RETURN RESULT))
	     (SETQ RESULT (CONS (CONS 0 (CONS (f+ HEIGHT D) EXP)) BAS))
	     (SETQ RESULT (CONS (CONS (f- W) (CONS (f- (f+ DEPTH H2)) SUB)) RESULT))
	     (SETQ RESULT (CONS (LIST (f- (MAX W W2) W2) 0) RESULT)
		   WIDTH (f+ WIDTH (MAX W W2)) HEIGHT (f+ H D HEIGHT) DEPTH (f+ D2 H2 DEPTH)))
	   (UPDATE-HEIGHTS HEIGHT DEPTH)
	   (RETURN RESULT))
	  ((AND (ATOM (CADDR FORM))
		(NOT (ATOM (CADR FORM)))
		(NOT (safe-GET (CAAADR FORM) 'DIMENSION))
		(PROG2 (SETQ BAS (NFORMAT-CHECK (CADR FORM)))
		       (NOT (safe-GET (CAAR BAS) 'DIMENSION))))
	   (RETURN (DIMENSION-FUNCTION
		    (LIST* '(MQAPPLY) (LIST '(MEXPT) (MOP BAS) (CADDR FORM)) (MARGS BAS))
		    RESULT)))
	  (T (SETQ BAS (DIMENSION (CADR FORM) RESULT LOP 'MEXPT NIL 0) WIDTH (f+ W WIDTH))
	     (IF (NOT (CHECKFIT WIDTH))
		 (RETURN (DIMENSION-FUNCTION (CONS '($EXPT) (CDR FORM)) RESULT)))
	     (IF
	       #-cl
	       (AND (NUMBERP (CAR BAS)) (char= RIGHT-PARENTHESES-CHAR (CAR BAS)))
	       #+cl  (eql RIGHT-PARENTHESES-CHAR (CAR BAS))
		 (SETQ RESULT (CONS (LIST* 0 (f1+ D) EXP) BAS) HEIGHT (MAX (f+ 1 H D) HEIGHT))
		 (SETQ RESULT (CONS (LIST* 0 (f+ HEIGHT D) EXP) BAS) HEIGHT (f+ H D HEIGHT)))
	     (UPDATE-HEIGHTS HEIGHT DEPTH)
	     (RETURN RESULT)))))

(DEFUN DSUMPROD (FORM RESULT D-FORM SW SH SD)
  (DECLARE (FIXNUM  SW SH SD))
  (PROG (DUMMY (W 0) (H 0) (D 0) DUMMY2)
	(DECLARE (FIXNUM W H D ))
	(SETQ DUMMY2 (DIMENSION (CADDR FORM) NIL 'MPAREN 'MEQUAL NIL 0)
	      W WIDTH H HEIGHT D DEPTH)
	(PUSH-STRING " = " DUMMY2)
	(SETQ DUMMY2 (DIMENSION (CADDDR FORM) DUMMY2 'MEQUAL 'MPAREN NIL 0)
	      W (f+ 3 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))
	(SETQ DUMMY (DIMENSION (CADR (CDDDR FORM)) NIL 'MPAREN 'MPAREN NIL 0))
	(COND ((NOT (CHECKFIT (MAX W WIDTH))) (RETURN (DIMENSION-FUNCTION FORM RESULT))))
	(SETQ DUMMY2 (CONS (CONS (f- SW) (CONS (f- (f+ SD H)) DUMMY2)) (CONS D-FORM RESULT)))
	(COND ((> WIDTH SW) (SETQ SW 0))
	      (T (SETQ SW (// (f- SW WIDTH) 2) WIDTH (f+ SW WIDTH))))
	(SETQ DUMMY (CONS (CONS (f- SW W) (CONS (f+ SH DEPTH) DUMMY)) DUMMY2)
	      W (MAX W WIDTH) D (f+ SD H D) H (f+ SH HEIGHT DEPTH))
	(UPDATE-HEIGHTS H D)
	(SETQ DUMMY (DIMENSION (CADR FORM) (CONS (LIST (f1+ (f- W WIDTH)) 0) DUMMY)
			       (CAAR FORM) ROP W RIGHT)
	      WIDTH (f+ 1 W WIDTH) HEIGHT (MAX H HEIGHT) DEPTH (MAX D DEPTH))
	(RETURN DUMMY)))

(DISPLA-DEF BIGFLOAT  DIM-BIGFLOAT)
(DISPLA-DEF MQUOTE    DIMENSION-PREFIX "'")
(DISPLA-DEF MSETQ     DIMENSION-INFIX  " : ")
(DISPLA-DEF MSET      DIMENSION-INFIX  " :: ")
(DISPLA-DEF MDEFINE   DIM-MDEFINE      " := ")
(DISPLA-DEF MDEFMACRO DIM-MDEFINE      " ::= ")

(DEFUN DIM-MDEFINE (FORM RESULT)
 (LET (($NOUNDISP T) (STRINGDISP T))
      (DIMENSION-INFIX (IF (CDDDR FORM)
			   (LIST (CAR FORM) (CADR FORM) (CONS '(MPROGN) (CDDR FORM)))
			   FORM)
			RESULT)))

(DISPLA-DEF MFACTORIAL DIMENSION-POSTFIX "!")
(DISPLA-DEF MEXPT      DIMENSION-SUPERSCRIPT)
(DISPLA-DEF MNCEXPT    DIM-MNCEXPT "^^")

(DEFUN DIM-MNCEXPT (FORM RESULT)
 (DIMENSION-SUPERSCRIPT (LIST '(MNCEXPT) (CADR FORM) (CONS '(MANGLE) (CDDR FORM)))
			RESULT))

(DISPLA-DEF MNCTIMES DIMENSION-NARY " . ")

(DISPLA-DEF %PRODUCT DIM-%PRODUCT 115.)

(DEFUN DIM-%PRODUCT (FORM RESULT) (DSUMPROD FORM RESULT '(D-PRODSIGN) 5 3 1))

(DISPLA-DEF RAT DIM-RAT #.forward-slash-string) ;;(setq forward-slash-string "//")

(DEFUN DIM-RAT (FORM RESULT)
  (IF $PFEFORMAT (DIMENSION-NARY FORM RESULT) (DIM-MQUOTIENT FORM RESULT)))

(DISPLA-DEF MQUOTIENT DIM-MQUOTIENT #.forward-slash-string)

(DEFUN DIM-MQUOTIENT (FORM RESULT)
   (IF (OR (NULL (CDDR FORM)) (CDDDR FORM)) (WNA-ERR (CAAR FORM)))
   (PROG (NUM (W 0) (H 0) (D 0) DEN)
     (DECLARE (FIXNUM W H D))
     (IF (AND (= 1 SIZE) (ATOM (CADR FORM)) (ATOM (CADDR FORM)))
	 (RETURN (DIMENSION-NARY FORM RESULT)))
     (SETQ NUM (DIMENSION (CADR FORM) NIL 'MPAREN 'MPAREN NIL RIGHT)
	   W WIDTH H HEIGHT D DEPTH)
     (IF (NOT (CHECKFIT W)) (RETURN (DIMENSION-NARY FORM RESULT)))
     (SETQ DEN (DIMENSION (CADDR FORM) NIL 'MPAREN 'MPAREN NIL RIGHT))
     (IF (NOT (CHECKFIT WIDTH)) (RETURN (DIMENSION-NARY FORM RESULT)))
     (RETURN (DRATIO RESULT NUM W H D DEN WIDTH HEIGHT DEPTH))))

;;            <--     W1     -->
;;            ------------------
;;            | ^              |
;;   <- X1 -> | | H1           |
;;            | | D1           |
;;	      | v	       |
;;            ------------------
;;    ----------------------------------
;; (Likewise for X2, H2, D2, W2 in the denominator)

;; Hack to recycle slots on the stack.  Compiler should be doing this.
;; Use different names to preserve sanity.
(eval-when (compile eval load)
    (SETQ X1 'H1 X2 'D2))

;#.(PROG2 (SETQ X1 'H1 X2 'D2) T)

(DEFUN DRATIO (RESULT NUM W1 H1 D1 DEN W2 H2 D2)
  (DECLARE (FIXNUM W1 H1 D1 W2 H2 D2))
  (SETQ WIDTH (MAX W1 W2) HEIGHT (f+ 1 H1 D1) DEPTH (f+ H2 D2))
  (SETQ #.X1 (// (f- WIDTH W1) 2) #.X2 (// (f- WIDTH W2) 2))
  (UPDATE-HEIGHTS HEIGHT DEPTH)
  (PUSH `(,#.X1 ,(f1+ D1) . ,NUM) RESULT)
  (PUSH `(,(f- #.X2 (f+ #.X1 W1)) ,(f- H2) . ,DEN) RESULT)
  (PUSH `(,(f- 0 #.X2 W2) 0) RESULT)
  (PUSH `(D-HBAR ,WIDTH) RESULT)
  RESULT)

(DISPLA-DEF MTIMES DIMENSION-NARY " ")

;; This code gets run when STARDISP is assigned a value.

(DEFPROP $STARDISP STARDISP ASSIGN)
(DEFUN STARDISP (SYMBOL VAL)
       SYMBOL ;ignored -- always bound to $STARDISP
       (PUTPROP 'MTIMES (IF VAL '(#\*) '(#\SPACE)) 'DISSYM))

(DISPLA-DEF %INTEGRATE DIM-%INTEGRATE 115.)

(DEFUN DIM-%INTEGRATE (FORM RESULT)
  (PROG (DUMMY (W 0)(H 0)(D 0) DUMMY2)
    (DECLARE (FIXNUM W H D))
    (COND ((NULL (CDDR FORM)) (WNA-ERR (CAAR FORM)))
	  ((NULL (CDDDR FORM))
	   (SETQ DUMMY `(#\SPACE (D-INTEGRALSIGN) . ,RESULT) W 2 H 3 D 2))
	  (T (SETQ DUMMY (DIMENSION (CADR (CDDDR FORM)) NIL 'MPAREN 'MPAREN NIL 0)
		   W WIDTH H HEIGHT D DEPTH)
	     (SETQ DUMMY2 (DIMENSION (CADDDR FORM) NIL 'MPAREN 'MPAREN NIL 0))
	     (IF (NOT (CHECKFIT (f+ 2 (MAX W WIDTH))))
		 (RETURN (DIMENSION-FUNCTION FORM RESULT)))
	     (SETQ DUMMY `((0 ,(f+ 3 D) . ,DUMMY) (D-INTEGRALSIGN) . ,RESULT))
	     (SETQ DUMMY (CONS (CONS (f- W) (CONS (f- (f+ 2 HEIGHT)) DUMMY2)) DUMMY)
		   W (f+ 2 (MAX W WIDTH)) H (f+ 3 H D) D (f+ 2 HEIGHT DEPTH)
		   DUMMY (CONS (LIST (f- W 1 WIDTH) 0) DUMMY))))
    (UPDATE-HEIGHTS H D)
    (SETQ DUMMY (DIMENSION (CADR FORM) DUMMY '%INTEGRATE 'MPAREN W 2)
	  W (f+ W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))
    (PUSH-STRING " d" DUMMY)
    (SETQ DUMMY (DIMENSION (CADDR FORM) DUMMY 'MPAREN ROP (f+ 2 W) RIGHT)
	  WIDTH (f+ 2 W WIDTH) HEIGHT (MAX H HEIGHT) DEPTH (MAX D DEPTH))
    (RETURN DUMMY)))

(DISPLA-DEF %DERIVATIVE DIM-%DERIVATIVE 125.)

(DEFUN DIM-%DERIVATIVE (FORM RESULT)
  (PROG ()
    (COND ((NULL (CDDR FORM))
	   (RETURN (DIMENSION-FUNCTION (CONS '(%DIFF) (CDR FORM)) RESULT))))
    (COND ((NULL (CDDDR FORM)) (SETQ FORM (APPEND FORM '(1)))))
    (COND ((AND $DERIVABBREV 
		(DO ((L (CDDR FORM) (CDDR L))) ((NULL L) T)
		    (COND ((AND (ATOM (CAR L)) (INTEGERP (CADR L)) (> (CADR L) 0)))
			  (T (RETURN NIL)))))
	   (RETURN (DMDERIVABBREV FORM RESULT)))
	  ((OR (> (RBP LOP) 130.) (> (LBP ROP) 130.)
	       (AND (NOT (ATOM (CADR FORM))) (OR (> (RBP LOP) 110.) (> (LBP ROP) 110.))))
	   (RETURN (DIMENSION-PAREN FORM RESULT)))
	  (T (RETURN (DMDERIVLONG FORM RESULT))))))

(DEFUN DMDERIVABBREV (FORM RESULT)
  (PROG (DUMMY (W 0)) (DECLARE (FIXNUM W))
	(DO ((L (CDDR FORM) (CDDR L)) (VAR))
	    ((NULL L) (SETQ DUMMY (CDR DUMMY) W (f1- W)))
	    (SETQ VAR (DIMENSION (CAR L) NIL 'MPAREN 'MPAREN NIL 0))
	    (DO ((I (CADR L) (f1- I))) ((= 1 I)) (SETQ DUMMY (CONS #\SPACE (APPEND VAR DUMMY))))
	    (SETQ DUMMY (CONS #\SPACE (NCONC VAR DUMMY)) W (f+ W (CADR L) (f* (CADR L) WIDTH))))
	(SETQ RESULT (DIMENSION (CADR FORM) RESULT LOP '%DERIV 0 RIGHT))
	(SETQ RESULT (CONS (CONS 0 (CONS (f- 0 DEPTH 1) DUMMY)) RESULT)
	      WIDTH (f+ W WIDTH) DEPTH (MAX 1 (f1+ DEPTH)))
	(UPDATE-HEIGHTS HEIGHT DEPTH)
	(RETURN RESULT)))

(DEFUN DMDERIVLONG (FORM RESULT)
  (PROG (NUM (W1 0) (H1 0) (D1 0) DEN (W2 0)( H2 0)  (D2 0))
	(DECLARE (FIXNUM W1 H1 D1 W2 H2 D2))
	(SETQ NUM (LIST (CADDDR FORM))
	      DEN (COND ((EQUAL 1 (CADDDR FORM))
			 (DIMENSION (CADDR FORM)
				    (LIST #\d) 'MPAREN 'MPAREN NIL 0))
			(T (DIMENSION-SUPERSCRIPT
			    (CONS '(DIFF)(CDDR FORM)) (LIST #\d))))
	      W2 (f1+ WIDTH) H2 HEIGHT D2 DEPTH)
	(DO ((L (CDDDDR FORM) (CDDR L))) ((NULL L))
	    (SETQ NUM (CONS (CADR L) NUM)
		  DEN (COND ((EQUAL 1 (CADR L))
			     (DIMENSION (CAR L) (CONS #\d (CONS #\SPACE DEN))
					'MPAREN 'MPAREN NIL 0))
			    (T (DIMENSION-SUPERSCRIPT
				(CONS '(DIFF) L) (CONS #\d (CONS #\SPACE DEN)))))
		  W2 (f+ 2 W2 WIDTH) H2 (MAX H2 HEIGHT) D2 (f+ D2 DEPTH)))
	(SETQ NUM (NFORMAT-CHECK (ADDN NUM T)))
	(COND ((EQUAL 1 NUM) (SETQ NUM (LIST #\d) W1 1 H1 1 D1 0))
	      (T (SETQ NUM (DIMENSION-SUPERSCRIPT (LIST '(DIFF) '|d| NUM) NIL)
		       W1 WIDTH H1 HEIGHT D1 DEPTH)))
	(COND ((ATOM (SETQ FORM (NFORMAT-CHECK (CADR FORM))))
	       (SETQ NUM (DIMENSION FORM NUM '%DERIV 'MPAREN NIL 0) W1 (f+ W1 WIDTH))
	       (RETURN (DRATIO RESULT NUM W1 H1 D1 DEN W2 H2 D2)))
	      (T (SETQ RESULT (DRATIO RESULT NUM W1 H1 D1 DEN W2 H2 D2) W1 WIDTH H1 HEIGHT D1 DEPTH)
		 (SETQ RESULT (DIMENSION FORM (CONS #\SPACE RESULT) '%DERIV ROP W1 RIGHT)
		       WIDTH (f+ 1 W1 WIDTH) HEIGHT (MAX H1 HEIGHT) DEPTH (MAX D1 DEPTH))
		 (UPDATE-HEIGHTS HEIGHT DEPTH)
		 (RETURN RESULT)))))

(DISPLA-DEF %AT DIM-%AT 105. 105.)

(DEFUN DIM-%AT (FORM RESULT)
  (PROG (EXP  EQS (W 0) (H 0) (D 0))
    (DECLARE (FIXNUM W H D))
    (IF (OR (NULL (CDDR FORM)) (CDDDR FORM)) (WNA-ERR (CAAR FORM)))
    (SETQ EXP (DIMENSION (CADR FORM) RESULT LOP '%AT NIL 0)
	  W WIDTH H HEIGHT D DEPTH)
    (SETQ EQS (DIMENSION (COND ((NOT (EQ 'MLIST (CAAR (CADDR FORM)))) (CADDR FORM))
			       ((NULL (CDDR (CADDR FORM))) (CADR (CADDR FORM)))
			       (T (CONS '(MCOMMA) (CDADDR FORM))))
			 NIL 'MPAREN 'MPAREN NIL 0))
    (COND ((NOT (CHECKFIT (f+ 1 W WIDTH))) (RETURN (DIMENSION-FUNCTION FORM RESULT))))
    (SETQ RESULT (CONS (CONS 0 (CONS (f- 0 1 D) EQS))
		       (CONS `(D-VBAR ,(f1+ H) ,(f1+ D) ,(GETCHARN $ABSBOXCHAR 2)) EXP))
	  WIDTH (f+ 1 W WIDTH) HEIGHT (f1+ H) DEPTH (f+ 1 D DEPTH))
    (UPDATE-HEIGHTS HEIGHT DEPTH)
    (RETURN RESULT)))

(DISPLA-DEF MMINUS DIMENSION-PREFIX "- ")
(DISPLA-DEF MPLUS  DIM-MPLUS)
(DEFPROP MUNARYPLUS (#\+ #\SPACE) DISSYM)

(DEFUN DIM-MPLUS (FORM RESULT)
  ;; If only 0 or 1 arguments, then print "+"() or +A
  (COND ((AND (NULL (CDDR FORM))
	      (NOT (MEMQ (CADAR FORM) '(TRUNC EXACT))))
	 (IF (NULL (CDR FORM))
	     (DIMENSION-FUNCTION FORM RESULT)
	     (DIMENSION-PREFIX (CONS '(MUNARYPLUS) (CDR FORM)) RESULT)))
	(T (SETQ RESULT (DIMENSION (CADR FORM) RESULT LOP 'MPLUS 0 0))
	   (CHECKBREAK RESULT WIDTH)
	   (DO ((L (CDDR FORM) (CDR L))
		(W WIDTH) (H HEIGHT) (D DEPTH)
		(TRUNC (MEMQ 'TRUNC (CDAR FORM))) (DISSYM))
	       ((NULL L) (COND (TRUNC (SETQ WIDTH (f+ 8 W) HEIGHT H DEPTH D)
				      (PUSH-STRING " + . . ." RESULT)))
			 RESULT)
	       (DECLARE (FIXNUM W H D))
	       (IF (MMMINUSP (CAR L))
		   (SETQ DISSYM '(#\SPACE #\- #\SPACE) FORM (CADAR L))
		   (SETQ DISSYM '(#\SPACE #\+ #\SPACE) FORM (CAR L)))
	       (COND ((AND (NOT TRUNC) (NULL (CDR L)))
		      (SETQ RESULT (DIMENSION FORM (APPEND DISSYM RESULT)
					      'MPLUS ROP (f+ 3 W) RIGHT)
			    WIDTH (f+ 3 W WIDTH)
			    HEIGHT (MAX H HEIGHT)
			    DEPTH (MAX D DEPTH))
		      (RETURN RESULT))
		     (T (SETQ RESULT
			      (DIMENSION FORM (APPEND DISSYM RESULT)
					 'MPLUS 'MPLUS (f+ 3 W) 0)
			      W (f+ 3 W WIDTH)
			      H (MAX H HEIGHT)
			      D (MAX D DEPTH))
			(CHECKBREAK RESULT W)))))))

(DISPLA-DEF %SUM   DIM-%SUM 110.)
(DISPLA-DEF %LIMIT DIM-%LIMIT 90. 90.)

(DEFUN DIM-%SUM (FORM RESULT) (DSUMPROD FORM RESULT '(D-SUMSIGN) 4 3 2))

(DEFUN DIM-%LIMIT (FORM RESULT)
  (PROG ((W 0) (H 0) (D 0) DUMMY) (DECLARE (FIXNUM W H D))
	(IF (NULL (CDDR FORM)) (RETURN (DIMENSION-FUNCTION FORM RESULT)))
	(IF (NULL (CDDDR FORM)) (WNA-ERR (CAAR FORM)))
	(SETQ DUMMY (DIMENSION (THIRD FORM) NIL 'MPAREN 'MPAREN NIL 0)
	      W WIDTH H HEIGHT D DEPTH)
	(PUSH-STRING " -> " DUMMY)
	(SETQ DUMMY (DIMENSION (FOURTH FORM) DUMMY 'MPAREN 'MPAREN NIL 0)
	      W (f+ 4 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))
	(COND ((NULL (CDDDDR FORM)))
	      ((EQ '$PLUS (FIFTH FORM))
	       (PUSH #\+ DUMMY)
	       (INCREMENT W))
	      (T (PUSH #\- DUMMY)
		 (INCREMENT W)))
	(PUSH-STRING "limit" RESULT)
	(SETQ DUMMY (CONS (LIST* -5 (f- H) DUMMY) RESULT) D (f+ H D))
	(UPDATE-HEIGHTS 1 D)
	(SETQ DUMMY (DIMENSION (CADR FORM) (CONS '(1 0) DUMMY) '%LIMIT ROP (f1+ W) RIGHT))
	(SETQ WIDTH (f+ 1 W WIDTH) DEPTH (MAX D DEPTH))
	(RETURN DUMMY)))

;; Some scheme needs to be worked out to allow use of mathematical character
;; sets on consoles which have them.

(DISPLA-DEF MARROW    DIMENSION-INFIX  " -> " 80. 80.)
(DISPLA-DEF MGREATERP DIMENSION-INFIX  " > ")
(DISPLA-DEF MGEQP     DIMENSION-INFIX  " >= ")
(DISPLA-DEF MEQUAL    DIMENSION-INFIX  " = ")
(DISPLA-DEF MNOTEQUAL DIMENSION-INFIX  " # ")
(DISPLA-DEF MLEQP     DIMENSION-INFIX  " <= ")
(DISPLA-DEF MLESSP    DIMENSION-INFIX  " < ")
(DISPLA-DEF MNOT      DIMENSION-PREFIX "NOT ")
(DISPLA-DEF MAND      DIMENSION-NARY   " AND ")
(DISPLA-DEF MOR	      DIMENSION-NARY   " OR ")
(DISPLA-DEF MCOND     DIM-MCOND)

(DEFUN DIM-MCOND (FORM RESULT)
  (PROG ((W 0) (H 0) (D 0))	(DECLARE (FIXNUM W H D))
    (PUSH-STRING "IF " RESULT)
    (SETQ RESULT (DIMENSION (CADR FORM) RESULT 'MCOND 'MPAREN 3 0)
	  W (f+ 3 WIDTH) H HEIGHT D DEPTH)
    (CHECKBREAK RESULT W)
    (PUSH-STRING " THEN " RESULT)
    (SETQ RESULT (DIMENSION (CADDR FORM) RESULT 'MCOND 'MPAREN (f+ 6 W) 0)
	  W (f+ 6 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))
    (UNLESS (EQ '$FALSE (FIFTH FORM))
	    (CHECKBREAK RESULT W)
	    (PUSH-STRING " ELSE " RESULT)
	    (SETQ RESULT (DIMENSION (FIFTH FORM) RESULT 'MCOND ROP (f+ 6 W) RIGHT)
		  W (f+ 6 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH)))
    (SETQ WIDTH W HEIGHT H DEPTH D)
    (RETURN RESULT)))


(DISPLA-DEF MDO DIM-MDO)

(DEFUN DIM-MDO (FORM RESULT)
 (PROG ((W 0) (H 0) (D 0) BRKFLAG) (DECLARE (FIXNUM W H D))
   (COND ((NOT (NULL (CADR FORM)))
	  (PUSH-STRING "FOR " RESULT)
	  (SETQ RESULT (CONS #\SPACE (DIMENSION (CADR FORM) RESULT 'MDO 'MPAREN 4 RIGHT))
		W (f+ 4 WIDTH) H HEIGHT D DEPTH BRKFLAG T)))
   (COND ((OR (NULL (CADDR FORM)) (EQUAL 1 (CADDR FORM))))
	 (T (PUSH-STRING "FROM " RESULT)
	    (SETQ RESULT
		  (CONS #\SPACE (DIMENSION (CADDR FORM) RESULT 'MDO 'MPAREN (f+ 6 W) 0))
		  W (f+ 6 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))))
   (SETQ FORM (CDDDR FORM))
   (COND ((EQUAL 1 (CAR FORM)))
	 ((NOT (NULL (CAR FORM)))
	  (PUSH-STRING "STEP " RESULT)
	  (SETQ RESULT (CONS #\SPACE (DIMENSION (CAR FORM) RESULT 'MDO 'MPAREN (f+ 6 W) 0))
		W (f+ 6 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH)))
	 ((NOT (NULL (CADR FORM)))
	  (PUSH-STRING "NEXT " RESULT)
	  (SETQ RESULT (CONS #\SPACE (DIMENSION (CADR FORM) RESULT 'MDO 'MPAREN (f+ 6 W) 0))
		W (f+ 6 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))))
   (COND ((NOT (NULL (CADDR FORM)))
	  (PUSH-STRING "THRU " RESULT)
	  (SETQ RESULT (CONS #\SPACE (DIMENSION (CADDR FORM) RESULT 'MDO 'MPAREN (f+ 6 W) 0))
		W (f+ 6 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH) BRKFLAG T)))
   (COND ((NOT (NULL (CADDDR FORM)))
	  (COND ((AND (NOT (ATOM (CADDDR FORM))) (EQ (CAAR (CADDDR FORM)) 'MNOT))
		 (PUSH-STRING "WHILE " RESULT)
		 (SETQ RESULT
		       (CONS #\SPACE (DIMENSION (CADR (CADDDR FORM)) RESULT 'MDO 'MPAREN (f+ 7 W) 0))
		       W (f+ 7 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH)))
		(T (PUSH-STRING "UNLESS " RESULT)
		   (SETQ RESULT
			 (CONS #\SPACE (DIMENSION (CADDDR FORM) RESULT 'MDO 'MPAREN (f+ 8 W) 0))
			 W (f+ 8 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))))))
   (IF BRKFLAG (CHECKBREAK RESULT W))
   (PUSH-STRING "DO " RESULT)
   (SETQ RESULT (DIMENSION (CAR (CDDDDR FORM)) RESULT 'MDO ROP (f+ 4 W) RIGHT)
	 WIDTH (f+ 4 W WIDTH) HEIGHT (MAX H HEIGHT) DEPTH (MAX D DEPTH))
   (RETURN RESULT)))


(DISPLA-DEF MDOIN DIM-MDOIN)

(DEFUN DIM-MDOIN (FORM RESULT)
  (PROG ((W 0) (H 0)  ( D 0)) (DECLARE (FIXNUM W H D))
	(PUSH-STRING "FOR " RESULT)
	(SETQ RESULT (DIMENSION (CADR FORM) RESULT 'MDO 'MPAREN 4 0)
	      W (f+ 4 WIDTH) H HEIGHT D DEPTH)
	(PUSH-STRING " IN " RESULT)
	(SETQ RESULT (DIMENSION (CADDR FORM) RESULT 'MDO 'MPAREN (f+ 4 W) 0)
	      W (f+ 4 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))
	(SETQ FORM (CDR (CDDDDR FORM)))
	(COND ((NOT (NULL (CAR FORM)))
	       (PUSH-STRING " THRU " RESULT)
	       (SETQ RESULT (DIMENSION (CAR FORM) RESULT 'MDO 'MPAREN (f+ 6 W) 0)
		     W (f+ 6 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))))
	(COND ((NOT (NULL (CADR FORM)))
	       (PUSH-STRING " UNLESS " RESULT)
	       (SETQ RESULT (DIMENSION (CADR FORM) RESULT 'MDO 'MPAREN (f+ 8 W) 0)
		     W (f+ 8 W WIDTH) H (MAX H HEIGHT) D (MAX D DEPTH))))
	(PUSH-STRING " DO " RESULT)
	(SETQ RESULT (DIMENSION (CADDR FORM) RESULT 'MDO ROP (f+ 4 W) RIGHT)
	      WIDTH (f+ 4 W WIDTH) HEIGHT (MAX H HEIGHT) DEPTH (MAX D DEPTH))
	(RETURN RESULT)))

(DISPLA-DEF MPROGN DIMENSION-MATCH "(" ")")
(DISPLA-DEF MLIST  DIMENSION-MATCH "[" "]")
(DISPLA-DEF MANGLE DIMENSION-MATCH "<" ">")
(DISPLA-DEF MCOMMA DIMENSION-NARY  ", " 20. 20.)
(DISPLA-DEF MABS   DIM-MABS)

(DEFUN DIM-MABS (FORM RESULT &AUX ARG BAR)
  (SETQ ARG (DIMENSION (CADR FORM) NIL 'MPAREN 'MPAREN NIL 0))
  (COND ((OR (> (f+ 2 WIDTH) LINEL) (AND (= 1 HEIGHT) (= 0 DEPTH)))
	 (DIMENSION-FUNCTION FORM RESULT))
	(T (SETQ WIDTH (f+ 2 WIDTH))
	   (UPDATE-HEIGHTS HEIGHT DEPTH)
	   (SETQ BAR `(D-VBAR ,HEIGHT ,DEPTH ,(GETCHARN $ABSBOXCHAR 2)))
	   (CONS BAR (NCONC ARG (CONS BAR RESULT))))))

(DISPLA-DEF $MATRIX DIM-$MATRIX)

(DEFUN DIM-$MATRIX (FORM RESULT)
  (PROG (DMSTR RSTR CSTR consp)
    (IF (OR (NULL (CDR FORM))
	    (NOT (MEMQ 'SIMP (CDAR FORM)))
	    (MEMALIKE '((MLIST SIMP)) (CDR FORM))
	    (DOLIST (ROW (CDR FORM)) (IF (NOT ($LISTP ROW)) (RETURN T))))
	(RETURN (DIMENSION-FUNCTION FORM RESULT)))
    (DO ((L (CDADR FORM) (CDR L))) ((NULL L))
	(SETQ DMSTR (CONS NIL DMSTR) CSTR (CONS 0 CSTR)))
    (DO ((R (CDR FORM) (CDR R)) (H1 0) (D1 0))
	((OR consp (NULL R))
	 (SETQ WIDTH 0)
	 (DO ((CS CSTR (CDR CS))) ((NULL CS)) (SETQ WIDTH (f+ 2 (CAR CS) WIDTH)))
	 (SETQ H1 (f1- (f+ H1 D1)) DEPTH (// H1 2) HEIGHT (f- H1 DEPTH)))
	(DECLARE (FIXNUM H1 D1))
	(DO ((C (CDAR R) (CDR C))
	     (NC DMSTR (CDR NC))
	     (CS CSTR (CDR CS)) (DUMMY) (H2 0) (D2 0))
	    ((NULL C) (SETQ D1 (f+ D1 H1 H2) H1 (f1+ D2)))
	    (DECLARE (FIXNUM H2 D2))
	    (SETQ DUMMY (DIMENSION (CAR C) NIL 'MPAREN 'MPAREN NIL 0)
		  H2 (MAX H2 HEIGHT) D2 (MAX D2 DEPTH))
	    (COND ((NOT (CHECKFIT (f+ 14. WIDTH))) (SETQ consp T) (RETURN NIL))
		  (T (RPLACA NC (CONS (LIST* WIDTH HEIGHT DEPTH DUMMY) (CAR NC)))
		     (RPLACA CS (MAX WIDTH (CAR CS))))))
	(SETQ RSTR (CONS D1 RSTR)))
    (IF (> (f+ HEIGHT DEPTH)
	   (LINEARRAY-DIM)
	   )
	(SETQ consp T))
    (RETURN
     (COND ((AND (NOT consp) (CHECKFIT (f+ 2 WIDTH)))
	    (MATOUT DMSTR CSTR RSTR RESULT))
	   ((AND (NOT consp) (<= LEVEL 2)) (COLOUT DMSTR CSTR RESULT))
	   (T (DIMENSION-FUNCTION FORM RESULT))))))

(DEFUN MATOUT (DMSTR CSTR RSTR RESULT)
  (PUSH `(D-MATRIX LEFT ,HEIGHT ,DEPTH) RESULT)
  (PUSH #\SPACE RESULT)
  (DO ((D DMSTR (CDR D)) (C CSTR (CDR C)) (W 0 0)) ((NULL D))
      (DECLARE (FIXNUM W))
      (DO ((D (CAR D) (CDR D)) (R RSTR (CDR R))) ((NULL D))
	  (RPLACA (CDDAR D) (f- HEIGHT (CAR R)))
	  (RPLACA (CDAR D) (f- (// (f- (CAR C) (CAAR D)) 2) W))
	  (SETQ W (// (f+ (CAR C) (CAAR D)) 2))
	  (RPLACA D (CDAR D)))
      (SETQ RESULT (CONS (LIST (f+ 2 (f- (CAR C) W)) 0) (NRECONC (CAR D) RESULT))))
  (SETQ WIDTH (f+ 2 WIDTH))
  (UPDATE-HEIGHTS HEIGHT DEPTH)
  (RPLACA (CAR RESULT) (f1- (CAAR RESULT)))
  (PUSH `(D-MATRIX RIGHT ,HEIGHT ,DEPTH) RESULT)
  RESULT)

(DEFUN COLOUT (DMSTR CSTR RESULT)
  (SETQ WIDTH 0 HEIGHT 1 DEPTH 0)
  (DO ((R DMSTR (CDR R)) (C CSTR (CDR C)) (COL 1 (f1+ COL)) (W 0 0) (H -1 -1) (D 0))
      ((NULL R))
      (DECLARE (FIXNUM COL W H D))
      (PUSH-STRING " Col " RESULT)
      (SETQ RESULT (NRECONC (EXPLODEN COL) RESULT))
      (PUSH-STRING " = " RESULT)
      (SETQ WIDTH (f+ 8 (FLATC COL) WIDTH))
      (DO ((R (CAR R) (CDR R))) ((NULL R))
	  (SETQ H (f+ 1 H (CADAR R) (CADDAR R)))
	  (RPLACA (CDDAR R) (f- H (CADAR R)))
	  (RPLACA (CDAR R) (f- (// (f- (CAR C) (CAAR R)) 2) W))
	  (SETQ W (// (f+ (CAR C) (CAAR R)) 2))
	  (RPLACA R (CDAR R)))
      (SETQ D (// H 2) H (f- H D))
      (PUSH `(D-MATRIX LEFT ,H ,D) RESULT)
      (PUSH #\SPACE RESULT)
      (PUSH `(0 ,(f- D) . ,(NREVERSE (CAR R))) RESULT)
      (PUSH `(,(f1+ (f- (CAR C) W)) 0) RESULT)
      (PUSH `(D-MATRIX RIGHT ,H ,D) RESULT)
      (SETQ WIDTH (f+ 4 (CAR C) WIDTH) HEIGHT (MAX H HEIGHT) DEPTH (MAX D DEPTH))
      (UPDATE-HEIGHTS H D)
      (CHECKBREAK RESULT WIDTH))
  RESULT)

(DISPLA-DEF MBOX DIM-MBOX)

(DEFUN DIM-MBOX (FORM RESULT &AUX DUMMY)
  (SETQ DUMMY (DIMENSION (CADR FORM) NIL 'MPAREN 'MPAREN NIL 0))
  (COND ((NOT (CHECKFIT (f+ 2 WIDTH)))
	 (DIMENSION-FUNCTION (CONS '($BOX) (CDR FORM)) RESULT))
	(T (PUSH `(D-BOX ,HEIGHT ,DEPTH ,WIDTH ,(NREVERSE DUMMY)) RESULT)
	   (SETQ WIDTH (f+ 2 WIDTH) HEIGHT (f1+ HEIGHT) DEPTH (f1+ DEPTH))
	   (UPDATE-HEIGHTS HEIGHT DEPTH)
	   RESULT)))

(DISPLA-DEF MLABOX DIM-MLABOX)

(DEFUN DIM-MLABOX (FORM RESULT)
  (PROG (DUMMY CH)
    (SETQ DUMMY (DIMENSION (CADR FORM) NIL 'MPAREN 'MPAREN NIL 0))
    (COND ((NOT (CHECKFIT (f+ 2 WIDTH)))
	   (RETURN (DIMENSION-FUNCTION (CONS '($BOX) (CDR FORM)) RESULT))))
    (SETQ WIDTH (f+ 2 WIDTH) HEIGHT (f1+ HEIGHT) DEPTH (f1+ DEPTH))
    (SETQ CH (GETCHARN $BOXCHAR 2))
    (SETQ RESULT
	  (CONS (DO ((L (MAPCAR #'(LAMBDA (L) (GETCHARN L 1))
				(MAKSTRING (CADDR FORM))) (CDR L))
		     (W 0) (NL))
		    ((OR (NULL L) (= WIDTH W))
		     (CONS 0 (CONS (f1- HEIGHT)
				   (COND ((< W WIDTH)
					  (CONS `(D-HBAR ,(f- WIDTH W) ,CH) NL))
					 (T NL)))))
			   (DECLARE (FIXNUM W))
			   (SETQ NL (CONS (CAR L) NL) W (f1+ W)))
		       RESULT))
    (SETQ RESULT (NCONC DUMMY (LIST* `(D-VBAR ,(f1- HEIGHT) ,(f1- DEPTH) ,CH)
				     (LIST (f- WIDTH) 0) RESULT)))
    (SETQ RESULT (CONS (LIST (f- 1 WIDTH) (f- DEPTH) `(D-HBAR ,WIDTH ,CH)) RESULT))
    (SETQ RESULT (LIST* `(D-VBAR ,(f1- HEIGHT) ,(f1- DEPTH) ,CH) '(-1 0) RESULT))
    (UPDATE-HEIGHTS HEIGHT DEPTH)
    (RETURN RESULT)))

(DISPLA-DEF MTEXT DIM-MTEXT 1 1)

(DEFUN DIM-MTEXT (FORM RESULT)
  (IF (NULL (CDDR FORM)) (DIMENSION (CADR FORM) RESULT LOP ROP 0 0)
      (DIMENSION-NARY FORM RESULT)))

(DISPLA-DEF MLABLE DIM-MLABEL 0 0)

(DEFUN DIM-MLABEL (FORM RESULT)
  (PROG (DUMMY (W 0) (H 0) (D 0)) (DECLARE (FIXNUM W H D))
    (COND ((EQ NIL (CADR FORM)) (SETQ W 0 H 0 D 0))
	  (MRATP (SETQ RESULT (APPEND MRATP (DIMENSION-PAREN (CADR FORM) RESULT))
		       W (f+ 4 WIDTH) H HEIGHT D DEPTH))
	  (T (SETQ RESULT (CONS #\SPACE (DIMENSION-PAREN (CADR FORM) RESULT))
		   W (f1+ WIDTH) H HEIGHT D DEPTH)))
    (LET ((LEVEL LINEL)) (CHECKBREAK RESULT W))
    (SETQ DUMMY (LIST 0 0))
    (SETQ RESULT (DIMENSION (CADDR FORM) (CONS DUMMY RESULT) 'MLABLE ROP W RIGHT))
    (COND ((AND (NOT $LEFTJUST) (= 0 BKPTOUT))
	   (RPLACA DUMMY (MAX 0 (f- (// (f- LINEL WIDTH) 2) W)))
	   (SETQ WIDTH (f+ (CAR DUMMY) WIDTH))))
    (SETQ WIDTH (f+ W WIDTH) HEIGHT (MAX H HEIGHT) DEPTH (MAX D DEPTH))
    (RETURN RESULT)))

(DEFPROP MPAREN -1. LBP)
(DEFPROP MPAREN -1. RBP)



(DEFUN CHECKRAT (FORM)
  (COND ((ATOM FORM) NIL)
	((AND (NOT (ATOM (CAR FORM))) (EQ (CAAR FORM) 'MRAT))
	 (IF (MEMQ 'TRUNC (CDAR FORM)) '(#\SPACE #.FORWARD-SLASH-CHAR #\T #.FORWARD-SLASH-CHAR)
				       '(#\SPACE #.FORWARD-SLASH-CHAR #\R #.FORWARD-SLASH-CHAR)))
	((AND (NOT (ATOM (CAR FORM))) (EQ (CAAR FORM) 'MPOIS))
	 '(#\space #.forward-slash-char #\P #.FORWARD-SLASH-CHAR))
	(T (DO ((L (CDR FORM) (CDR L))) ((NULL L))
	       (COND ((ATOM L)
		      (MERROR "~S has an atomic cdr - DISPLAY" FORM))
		     ((SETQ FORM (CHECKRAT (CAR L))) (RETURN FORM)))))))




(DEFUN CHECKFIT (W)
  (DECLARE (FIXNUM W))
  (OR (NOT BREAK) (<= (f- (f+ W BREAK RIGHT 1) BKPTWD) LINEL)))

(DEFUN CHECKBREAK (RESULT W)
  (DECLARE (FIXNUM W))
  (COND ((NOT BREAK))
	((> (f- (SETQ W (f+ W BREAK)) BKPTOUT) LINEL)
	 (IF (OR (NULL BKPT) (EQ RESULT BKPT))
	     (MERROR "Expression is too wide to be displayed."))
	 (DO ((L RESULT (CDR L))) ((EQ BKPT (CDR L)) (RPLACD L NIL))
	   (IF (NULL L) (MERROR "CHECKBREAK not found in DISPLAY")))
	 (OUTPUT BKPT 0) 
 	 #-Franz (LET ((#.TTYOFF (OR #.TTYOFF MORE-^W))) (MTERPRI))

	 (SETQ LINES (f1+ LINES) BKPT RESULT BKPTOUT BKPTWD BKPTWD W
	       BKPTHT MAXHT BKPTDP MAXDP BKPTLEVEL LEVEL MAXHT 1 MAXDP 0))
	((OR (NULL BKPT) (<= LEVEL BKPTLEVEL) (> (// LINEL 2) (f- BKPTWD BKPTOUT)))
	 (SETQ BKPT RESULT BKPTWD W BKPTLEVEL LEVEL
	       BKPTHT (MAX MAXHT BKPTHT) BKPTDP (MAX MAXDP BKPTDP) MAXHT 1 MAXDP 0))))

(DEFUN FORCEBREAK (RESULT W)
  (OUTPUT RESULT 0) (MTERPRI)
  (SETQ LINES (f+ 2 LINES) BKPT NIL BKPTOUT (f+ W BREAK) MAXHT 1 MAXDP 0))

(DEFUN UPDATE-HEIGHTS (HT* DP*)
  (DECLARE (FIXNUM HT* DP*))
  (IF BREAK (SETQ MAXHT (MAX MAXHT HT*) MAXDP (MAX MAXDP DP*))))

;;; BKPT	dimension structure for last breakpoint saved
;;; BKPTWD	width at last bkpt
;;; BKPTHT	height of current line to last bkpt
;;; BKPTDP	depth of current line to last bkpt
;;; BKPTOUT	width of stuff already output

;;; MAXHT	height from last bkpt saved to current point
;;; MAXDP	depth from last bkpt saved to current point

;;; BREAK	width up to last call to DIMENSION
;;; RESULT	dimension structure to current point minus output
;;; W		width from last call to DIMENSION to current point

;; Code above this point deals with dimensioning and constructing
;; up dimension strings.  Code past this point deals with printing
;; them.

;; <dimension string> ::= () | (<string element> . <dimension string>)
;; <string element> ::= character |
;; 		     (<column-relative> <row-relative> . <dimension string>) |
;; 		     (<drawing function> . args)
;; <column-relative> ::= <fixnum>
;; <row-relative>    ::= <fixnum>
;; <drawing function> ::= D-HBAR | D-VBAR | D-INTEGRALSIGN | ...

;; When a character appears in a dimension string, it is printed and
;; the cursor moves forward a single position.  (The variable OLDCOL is
;; incremented)  When a form with a fixnum car is encountered, the
;; first two elements of the form are taken to be relative displacements
;; for OLDCOL and OLDROW.  *** NOTE *** After drawing the cddr of the form,
;; OLDROW is reset to its original value, but OLDCOL is left in the new
;; position.  Why this is done is beyond me.  It only appears to complicate
;; things.

;; There are two basic output functions.  OUTPUT-2D draws equations in the same
;; order they are dimensioned, and OUTPUT-LINEAR draws equations line by line.
;; When a <drawing function> is invoked, the first argument passed to it is a
;; flag which is T for linear output and NIL for 2D output.  A
;; <drawing function> is also expected to return the new column position.

(DEFUN OUTPUT (RESULT W)
  (DECLARE (FIXNUM W))
  #+nocp (if (not (or (OR #.ttyoff MORE-^W)))(fresh-line))
  #-nocp
  (IF (NOT (OR #.ttyoff MORE-^W  (ZEROP (CHARPOS T))))
      (MTERPRI))
  (IF (AND (NOT (OR #.ttyoff MORE-^W))
	   SMART-TTY (NOT (AND SCROLLP (NOT $CURSORDISP)))
	   (< (f+ BKPTHT BKPTDP) (f1- TTYHEIGHT))
	   ;;If (STATUS TTY) is NIL, then we don't have the console.
	   #+PDP10 (STATUS TTY)
	   (> (f+ BKPTHT BKPTDP) (f- (f1- TTYHEIGHT) (CAR (CURSORPOS)))))
      (MORE-FUN T))
  (COND
   ;; If output is turned off to the console and no WRITEFILE is taking
   ;; place, then don't output anything.
   ((AND (OR #.ttyoff MORE-^W) (NOT #.writefilep)))
   ;; If the terminal can't do cursor movement, or we are writing 
   ;; to a WRITEFILE (#.writefilep is on) or the terminal is scrolling or
   ;; something else random, then draw equations line by line.
   ((> (f+ BKPTHT BKPTDP) 80.)
    (MERROR "Expression is too high to be displayed."))
   ((OR (NOT (AND SMART-TTY $CURSORDISP))
	#.writefilep SCROLLP (> (f+ BKPTHT BKPTDP) (f- TTYHEIGHT 2)))
    (OUTPUT-LINEAR (NREVERSE RESULT) W))
   ;; Otherwise, draw equations in the same order as they are dimensioned.
   (T (OUTPUT-2D (NREVERSE RESULT) W))))

;; Output function for terminals without cursor positioning capability.
;; Characters are drawn into LINEARRAY instead.  Each element of LINEARRAY is a
;; list -- the car is how many spaces to indent; the cdr is a list of
;; characters to draw.  After drawing into this array, lines are printed one at
;; a time.  This is used for printing terminals and when writing to files.
;; Block mode i/o isn't needed since PRINC is used instead of TYO and
;; CURSORPOS.

(DEFUN OUTPUT-LINEAR (RESULT W)
  (DECLARE (FIXNUM W))
  (DRAW-LINEAR RESULT BKPTDP W)
  (DO (#+PDP10 (TERPRI T) (I (f1- (f+ BKPTHT BKPTDP)) (f1- I)))
      ((< I 0))
      (DECLARE (FIXNUM I))
      (COND ((NULL (LINEARRAY I)))
	    (MORE-^W (SAFE-PRINT (OUTPUT-LINEAR-ONE-LINE I)))
	    (T (OUTPUT-LINEAR-ONE-LINE I)))))

#+NIL
(PROGN
 (DEFPARAMETER *OLOL-BUF*
   (MAKE-STRING 512))
 (DEFUN OUTPUT-LINEAR-ONE-LINE (I)
   (LET* ((LINE (NREVERSE (CDR (LINEARRAY I)))) (N (CAR LINE)))
     (SET-LINEARRAY I NIL)
     (TYOTBSP N)
     (LET ((B (OR *OLOL-BUF* (SETQ *OLOL-BUF* (MAKE-STRING 512))))
	   (*OLOL-BUF* NIL)
	   (I 0))
       (MAPC #'(LAMBDA (X) (SETF (SCHAR B I) (CODE-CHAR X)) (SETQ I (f1+ I)))
	     (CDR LINE))
       (OUSTR B NIL 0 I)
       (MTERPRI)
       NIL)))
 ) ;#+NIL
#-NIL
(DEFUN OUTPUT-LINEAR-ONE-LINE (I) (DECLARE (FIXNUM I))
  (PROG (LINE (N 0))
	(DECLARE (FIXNUM n))
    (SETQ LINE (LINEARRAY I) LINE (NREVERSE (CDR LINE)) N (CAR LINE))
    (SET-LINEARRAY I NIL)
    (TYOTBSP N)
    (sloop for v in (cdr line) do (tyo v))
;    (PRINC (MAKNAM (CDR LINE)))
    (MTERPRI)))

;; Move the cursor over N spaces to the left by outputting tabs and spaces.
;; This function assumes that the cursor is in the left margin when
;; it is called.  This is only called from OUTPUT-LINEAR, so it is
;; used only for printing terminals or for file output.

(DEFUN TYOTBSP (N)
  (DECLARE (FIXNUM N))
  (DO () ((< N (TABLEN))) (TYO #\TAB) (DECREMENT N (TABLEN)))
  (DO () ((< N 1)) (TYO #\space) (DECREMENT N)))

(DEFUN DRAW-LINEAR (DMSTR OLDROW OLDCOL)
  "This puts the LINE lists into LINEARRAY ready to be drawn.
   Each LINE consists of first an initial number of columns to space
   (or tab over) and then the characters to be printed.
   oldrow and oldcol are the starting points for the the (dx,dy) offsets
   given in the dimension string DMSTR.  It does not check that oldrow
   is big enough for possible negative y offsets in DMSTR, but BKPTDP is the
   right global to use for  oldrow (see Draw-2d)."
  (DO ((LINE)) ((NULL DMSTR))
      (COND ((ATOM (CAR DMSTR))
	     (SETQ LINE (LINEARRAY OLDROW))
	     (COND ((NULL LINE) (SETQ LINE (LIST OLDCOL)))
		   (T (PROG (n)	#-cl (DECLARE (FIXNUM N))
			(SETQ N (CAR LINE) LINE (CDR LINE))
			(DO ((M (f+ (TABLEN) (f* (TABLEN) (// N (TABLEN))))
				(f+ (TABLEN) M)))
			    ((NOT (< M OLDCOL))
			     (SETQ N (MAX N (f- M (TABLEN)))))
			    (DECLARE (FIXNUM M))
			    (SETQ LINE (CONS #\TAB LINE)))
			(DO () ((= OLDCOL N))
			    (PUSH #\space LINE)
			    (INCREMENT N)))))
	     (DO () ((OR (NULL DMSTR) (NOT (ATOM (CAR DMSTR))))
		     (SET-LINEARRAY OLDROW (CONS OLDCOL LINE)))
		 (INCREMENT OLDCOL)
		 (PUSH (CAR DMSTR) LINE)
		 (POP DMSTR)))
	    ((INTEGERP (CAAR DMSTR))
	     ;; Why update OLDCOL and not OLDROW?  Should either update both
	     ;; (requiring multiple value return) or neither (analagous to lambda
	     ;; binding).
	     (SETQ OLDCOL (DRAW-LINEAR (REVERSE (CDDAR DMSTR))
				       (f+ OLDROW (CADAR DMSTR))
				       (f+ OLDCOL (CAAR DMSTR))))
	     (POP DMSTR))
	    (T (SETQ OLDCOL (APPLY (CAAR DMSTR) T (CDAR DMSTR)))
	       (POP DMSTR))))
  ;; Be sure to return this.
  OLDCOL)


;; Output function for terminals with cursor positioning capability.  Draws
;; equations in the order they are dimensioned.  To be efficient, it does block
;; mode i/o into a stream called DISPLAY-FILE, set up in ALJABR;LOADER.
;; This function is not used if a WRITEFILE is taking place.

;; TTY interrupts are turned off for some reason, probably to protect global
;; state.  

;; Bug in COMPLR necessitates binding H to 0 initially.
;; (PROG (H) (DECLARE (FIXNUM H)) ...) doesn't try binding it to NIL as
;; this does.

#+ITS
(DEFUN OUTPUT-2D (RESULT W &AUX (H 0))
 (DECLARE (FIXNUM W H CH))
 (UNWIND-PROTECT
  (PROGN (TTYINTSOFF)
	 (SETQ OLDROW (CAR (CURSORPOS)) OLDCOL 0 H (f+ OLDROW BKPTHT BKPTDP))
	 ;; Move the cursor to the left edge of the screen.
	 (CURSORPOS* OLDROW 0)
	 ;; Then print CRLFs from the top of the expression to the bottom.
	 ;; The purpose of this is to clear the appropriate section of the
	 ;; screen.  If RUBOUT-TTY is NIL (i.e. we are using a storage tube
	 ;; display), then only print LFs since the entire screen is cleared
	 ;; anyway.  %TDCRL = carriage return, line feed.  %TDLF = line feed.
	 (DO ((CH (IF RUBOUT-TTY #.%TDCRL #.%TDLF))) ((= H OLDROW))
	     (TYO* CH) (INCREMENT OLDROW))
	 (DRAW-2D RESULT (f- OLDROW BKPTDP 1) W)
	 ;; Why is this necessary?  Presumably, we never go off the bottom
	 ;; of the screen.
	 (SETQ H (MIN (f- TTYHEIGHT 2) H))
	 ;; Leave the cursor at the bottom of the expression.
	 (CURSORPOS* H 0)
	 ;; Output is buffered for efficiency.
	 (FORCE-OUTPUT DISPLAY-FILE)
	 ;; Let ITS know where the cursor is now.  This does not do
	 ;; cursor movement.  :CALL SCPOS for information.
	 (SETCURSORPOS H 0)
	 ;; Gobble any characters the poor user may have typed during display.
	 (LISTEN))
  (TTYINTSON))
 (NOINTERRUPT NIL))

;; I/O is much simpler on the Lisp Machine.

#+(or CL NIL)
(DEFUN OUTPUT-2D (RESULT W &AUX (H 0))
  (DECLARE (FIXNUM W H ))
  (SETQ OLDROW (CAR (CURSORPOS)) OLDCOL 0 H (f+ OLDROW BKPTHT BKPTDP))
  (CURSORPOS* OLDROW 0)
  ;; Move the cursor vertically until we are at the bottom line of the
  ;; new expression.
  (DO () ((= H OLDROW)) (TYO* #\NEWLINE) (INCREMENT OLDROW))
  (DRAW-2D RESULT (f- OLDROW BKPTDP 1) W)
  (CURSORPOS* (SETQ H (MIN (f- TTYHEIGHT 2) H)) 0))

;; For now, cursor movement is only available on ITS and the Lisp
;; Machine.  But define this to catch possible errors.

#-(OR CL ITS NIL)
(DEFUN OUTPUT-2D (RESULT W)
       RESULT W ;Ignored.
       (MERROR "OUTPUT-2D called on system without display support."))

;;see +lispm below for cl fix.
#+(OR CL ITS NIL)
(DEFUN DRAW-2D (DMSTR ROW COL)
  (DECLARE (FIXNUM ROW COL))
  (CURSORPOS* ROW COL)
  (DO ((L DMSTR)) ((NULL L))
      (COND ((INTEGERP (CAR L)) (TYO* (CAR L)) (POP L))
	    ((INTEGERP (CAAR L))
	     (SETQ COL OLDCOL)
	     (DO ()
	       ((OR (INTEGERP (CAR L)) (NOT (INTEGERP (CAAR L)))))
	       (cond
		((NULL (CDDAR L)) (SETQ COL (f+ COL (CAAR L))))
		(t (DRAW-2D (REVERSE (CDDAR L))
			    (f-  ROW (CADAR L)) (f+ COL (CAAR L)))
		   (SETQ COL OLDCOL)))
	       (POP L))
	     (CURSORPOS* ROW COL))
	    (T (APPLY (CAAR L) NIL (CDAR L))
	       (POP L)))))
(defun check-dimstring (str)
  (sloop for v in str when (integerp v) do (error "bad entry ~A" v)))

#+lispm
(DEFUN DRAW-2D (DMSTR ROW COL)
  "This bypasses draw-linear and draws the Dimension-string directly
   using cursor positioning.  It won't work for files or the editor"
 #-cl (DECLARE (FIXNUM ROW COL))
  (CURSORPOS* ROW COL)
    (DO ((LL DMSTR)) ((NULL LL))
     ;;should not have a integerp (car ll) but somewhere #\space is getting 32.
      (COND ((or (integerp (car ll))
		 (characterp (CAR LL)))
	     (TYO* (CAR LL)) (POP LL))
	    ((integerp (CAAR LL))
	     (SETQ COL OLDCOL)
	     (DO ()
	       ((OR (CHARACTERP (CAR LL))  (NOT (integerp (CAAR LL)))))
	       (cond
		((NULL (CDDAR LL)) (SETQ COL (f+ COL (CAAR LL))))
		(t (DRAW-2D (REVERSE (CDDAR LL))
			    (f-  ROW (CADAR LL)) (f+ COL (CAAR LL)))
		   (SETQ COL OLDCOL)))
	       (POP LL))
	     (CURSORPOS* ROW COL)
	     )
	    (T (APPLY (CAAR LL) NIL (CDAR LL))
	       (POP LL)))))

#-(OR CL ITS NIL)
(DEFUN DRAW-2D (DMSTR ROW COL)
       DMSTR ROW COL ;Ignored.
       (MERROR "DRAW-2D called on system without display support."))


;; Crude line graphics.  The interface to a graphics device is via the
;; functions LG-SET-POINT, LG-DRAW-VECTOR, LG-END-VECTOR and via the
;; LG-CHARACTER specials.  LG-END-VECTOR is needed since many consoles
;; (including those supporting ARDS protocol) must "exit" graphics mode.
;; LG-CHARACTER-X and LG-CHARACTER-Y give the width and height of a character
;; in pixels, and the -2 variables are simply those numbers divided by 2.  LG
;; stands for "Line Graphics".  See MAXSRC;ARDS for a sample ctl.

(declare-top (*EXPR LG-SET-POINT LG-DRAW-VECTOR LG-END-VECTOR)
	 #-NIL
	 (NOTYPE (LG-SET-POINT FIXNUM FIXNUM)
		 (LG-DRAW-VECTOR FIXNUM FIXNUM)
		 (LG-END-VECTOR FIXNUM FIXNUM))
	 (SPECIAL LG-CHARACTER-X LG-CHARACTER-X-2
		  LG-CHARACTER-Y LG-CHARACTER-Y-2))

;; Make this work in the new window system at some point.

#+LISPM
(PROGN 'COMPILE

(declare-top (SPECIAL LG-OLD-X LG-OLD-Y))

(DEFUN LG-SET-POINT (X Y)
  (SETQ LG-OLD-X (f- X 1) LG-OLD-Y (f- Y 2)))

(DEFUN LG-DRAW-VECTOR (X Y)
  (SETQ X (f- X 1) Y (f- Y 2))
  (FUNCALL *standard-output* ':DRAW-LINE LG-OLD-X LG-OLD-Y X Y)
  (WHEN (> LG-CHARACTER-Y 20)
	(LET ((DELTA-X (f- X LG-OLD-X))
	      (DELTA-Y (f- Y LG-OLD-Y)))
	  (IF (> (ABS DELTA-X) (ABS DELTA-Y))
	      (FUNCALL *standard-output* ':DRAW-LINE LG-OLD-X (f1- LG-OLD-Y) X (f1- Y))
	      (FUNCALL *standard-output* ':DRAW-LINE (f1- LG-OLD-X) LG-OLD-Y (f1- X) Y))))
  (SETQ LG-OLD-X X LG-OLD-Y Y))

;; Set these so that DISPLA can be called from top-level.  The size
;; of TERMINAL-IO is wired in here.
;; These should be bound at time of call to DISPLA.

(SETQ LG-CHARACTER-X (FUNCALL tv:main-screen ':CHAR-WIDTH))
(SETQ LG-CHARACTER-Y (FUNCALL tv:main-screen ':LINE-HEIGHT))

(SETQ LG-CHARACTER-X-2 (// LG-CHARACTER-X 2))
(SETQ LG-CHARACTER-Y-2 (// LG-CHARACTER-Y 2))

) ;; End of Lispm Graphics definitions.

;; Even cruder character graphics.  Interface to the ctl is via functions
;; which draw lines and corners.  CG means "Character Graphics".  See
;; MAXSRC;VT100 for a sample ctl.  Note that these functions do not modify
;; the values of OLDROW and OLDCOL.

(declare-top (*EXPR CG-BEGIN-GRAPHICS CG-END-GRAPHICS
		CG-UL-CORNER CG-UR-CORNER CG-LL-CORNER CG-LR-CORNER
		CG-VERTICAL-BAR CG-HORIZONTAL-BAR
		CG-D-SUMSIGN CG-D-PRODSIGN))

;; Special form for turning on and turning off character graphics.
;; Be sure to turn of character graphics if we throw out of here.

;; (DEFMACRO CG-WITH-GRAPHICS (&BODY BODY)
;;   `(UNWIND-PROTECT (PROGN (CG-BEGIN-GRAPHICS) . ,BODY) (CG-END-GRAPHICS)))
;; Not needed after all. - JPG

;; Special symbol drawing functions -- lines, boxes, summation signs, etc.
;; Every drawing function must take at least one argument.  The first
;; argument is T if equations must be printed line-by-line.  Otherwise,
;; draw them using cursor movement, character graphics, or line graphics
;; if possible.

;; Most of these functions just invoke DRAW-XXX on some constant
;; list structure, so be careful about NREVERSEing.  In other cases,
;; stuff is consed only for the linear case, but direct calls are used
;; in the 2D case.  This should work for both cases.  (See end of
;; program.)

(DEFUN D-HBAR (LINEAR? W &OPTIONAL (CHAR #\-) &AUX NL)
  (DECLARE (FIXNUM W ))
  (COND (LINEAR? (DOTIMES (I W) (PUSH CHAR NL))
		 (DRAW-LINEAR NL OLDROW OLDCOL))
	((AND LINE-GRAPHICS-TTY $LINEDISP)
	 (LET ((GY (f+ (f* LG-CHARACTER-Y OLDROW) LG-CHARACTER-Y-2)))
	      (DECLARE (FIXNUM gy))
	      (LG-SET-POINT  (f* OLDCOL LG-CHARACTER-X) GY)
	      (LG-END-VECTOR (f* (f+ OLDCOL W) LG-CHARACTER-X) GY))
	 (CURSORPOS* OLDROW (f+ OLDCOL W)))
	((AND CHARACTER-GRAPHICS-TTY $LINEDISP)
	 (CG-BEGIN-GRAPHICS)
	 (DOTIMES (I W) (CG-HORIZONTAL-BAR))
	 (INCREMENT OLDCOL W)
	 (CG-END-GRAPHICS))
	(T (DOTIMES (I W) (TYO* CHAR)))))

;; Notice that in all of the height computations, an offset of 2 is added or
;; subtracted to the y-dimension.  This is to get the lines to fit within the
;; character cell precisely and not get clipped when moving things around in
;; the equation editor.

(DEFUN D-VBAR (LINEAR? H D &OPTIONAL (CHAR VERTICAL-STROKE-CHAR))
  (DECLARE (FIXNUM H D ))
  (COND (LINEAR? (SETQ D (f- D))
	 (DO ((I (f- H 2) (f1- I))
	      (NL `((0 ,(f1- H) ,CHAR))))
	     ((< I D) (DRAW-LINEAR (NREVERSE NL) OLDROW OLDCOL))
	     (PUSH `(-1 ,I ,CHAR) NL)))
	((AND LINE-GRAPHICS-TTY $LINEDISP)
	 (LET ((GX (f+ (f* LG-CHARACTER-X OLDCOL) LG-CHARACTER-X-2)))
	      (DECLARE (FIXNUM GX ))
	      (LG-SET-POINT  GX (f- (f* (f+ OLDROW D 1) LG-CHARACTER-Y) 2))
	      (LG-END-VECTOR GX (f+ (f* (f+ OLDROW 1 (f- H)) LG-CHARACTER-Y) 2)))
	 (CURSORPOS* OLDROW (f1+ OLDCOL)))
	((AND CHARACTER-GRAPHICS-TTY $LINEDISP)
	 (CURSORPOS* (f+ OLDROW 1 (f- H)) OLDCOL)
	 (CG-BEGIN-GRAPHICS)
	 (CG-VERTICAL-BAR)
	 (DOTIMES (I (f+ H D -1))
		  (CURSORPOS* (f1+ OLDROW) OLDCOL)
		  (CG-VERTICAL-BAR))
	 (CG-END-GRAPHICS)
	 (CURSORPOS* (f- OLDROW D) (f1+ OLDCOL)))
	(T (CURSORPOS* (f+ OLDROW 1 (f- H)) OLDCOL)
	   (TYO* CHAR)
	   (DOTIMES (I (f+ H D -1))
	       (CURSORPOS* (f1+ OLDROW) (f1- OLDCOL))
	       (TYO* CHAR))
	   (CURSORPOS* (f- OLDROW D) OLDCOL))))

(DEFUN D-INTEGRALSIGN (LINEAR? &AUX DMSTR)
  (COND ((AND (NOT LINEAR?) LINE-GRAPHICS-TTY $LINEDISP)
	 (LET ((X-MIN (f* LG-CHARACTER-X OLDCOL))
	       (X-1   (f1- LG-CHARACTER-X-2))
	       (X-2   LG-CHARACTER-X-2)
	       (X-MAX (f* LG-CHARACTER-X (f1+ OLDCOL)))
	       (Y-MIN (f+ (f* LG-CHARACTER-Y (f- OLDROW 2)) LG-CHARACTER-Y-2))
	       (Y-1   LG-CHARACTER-Y-2)
	       (Y-2   (f+ LG-CHARACTER-Y LG-CHARACTER-Y-2))
	       (Y-MAX (f+ (f* LG-CHARACTER-Y (f+ OLDROW 2)) LG-CHARACTER-Y-2)))
	      (DECLARE (FIXNUM X-MIN X-1 X-2 X-MAX Y-MIN Y-1 Y-2 Y-MAX))
	    (DOLIST (X '(0 -1))
	       (LG-SET-POINT   (f+ X X-MAX) Y-MIN)
	       (LG-DRAW-VECTOR (f+ X X-MAX (f- X-1)) (f+ Y-MIN Y-1))
	       (LG-DRAW-VECTOR (f+ X X-MAX (f- X-2)) (f+ Y-MIN Y-2))
	       (LG-DRAW-VECTOR (f+ X X-MIN X-2)	   (f- Y-MAX Y-2))
	       (LG-DRAW-VECTOR (f+ X X-MIN X-1)	   (f- Y-MAX Y-1))
	       (LG-END-VECTOR  (f+ X X-MIN)	   Y-MAX)))
	 (CURSORPOS* OLDROW (f1+ OLDCOL)))
	(T (SETQ DMSTR
		 `((0 2 #.forward-slash-char) (-1 1 #\[) (-1 0 #\I) (-1 -1 #\]) (-1 -2 #.FORWARD-SLASH-CHAR)))
	   (IF LINEAR?
	       (DRAW-LINEAR DMSTR OLDROW OLDCOL)
	       (DRAW-2D	    DMSTR OLDROW OLDCOL)))))

(DEFUN D-PRODSIGN (LINEAR? &AUX DMSTR)
  (COND ((AND (NOT LINEAR?) $LINEDISP (FBOUNDP 'CG-D-PRODSIGN))
	 (CG-BEGIN-GRAPHICS)
	 (CG-D-PRODSIGN)
	 (CG-END-GRAPHICS)
	 (INCREMENT OLDCOL 5))
	(T (SETQ DMSTR '((0 2 #.BACK-SLASH-CHAR (D-HBAR 3 #\=) #.forward-slash-char)
			 (-4 0) (D-VBAR 2 1 #\!) #\space (D-VBAR 2 1 #\!) (1 0)))
	   (IF LINEAR?
	       (DRAW-LINEAR DMSTR OLDROW OLDCOL)
	       (DRAW-2D DMSTR OLDROW OLDCOL)))))

(DEFUN D-SUMSIGN (LINEAR? &AUX DMSTR)
  (COND ((AND (NOT LINEAR?) $LINEDISP LINE-GRAPHICS-TTY)
	 (LET ((X-MIN  (f* LG-CHARACTER-X OLDCOL))
	       (X-HALF (f* LG-CHARACTER-X (f+ OLDCOL 2)))
	       (X-MAX  (f* LG-CHARACTER-X (f+ OLDCOL 4)))
	       (Y-MIN  (f+ (f* LG-CHARACTER-Y (f- OLDROW 2)) LG-CHARACTER-Y-2))
 	       (Y-HALF (f+ (f* LG-CHARACTER-Y OLDROW) LG-CHARACTER-Y-2))
	       (Y-MAX  (f+ (f* LG-CHARACTER-Y (f+ OLDROW 2))
			   LG-CHARACTER-Y-2)))
	      (DECLARE (FIXNUM X-MIN X-HALF X-MAX Y-MIN Y-HALF Y-MAX))

	      (LG-SET-POINT (f+ X-MAX 4) (f+ Y-MIN 6))
	      (MAPC #'(LAMBDA (X) (LG-DRAW-VECTOR (CAR X) (CDR X)))
		    `((,X-MAX . ,Y-MIN)
		      (,(f1+ X-MIN)  . ,Y-MIN)
		      (,(f1+ X-HALF) . ,Y-HALF)
		      (,(f1+ X-MIN)  . ,Y-MAX)
		      (,X-MIN	    . ,Y-MAX)
		      (,X-HALF	    . ,Y-HALF)
		      (,X-MIN	    . ,Y-MIN)
		      (,(f1- X-MIN)  . ,Y-MIN)
		      (,(f1- X-HALF) . ,Y-HALF)))
	      (LG-SET-POINT (f+ X-MAX 4) (f- Y-MAX 6))
	      (LG-DRAW-VECTOR X-MAX Y-MAX)
	      (LG-DRAW-VECTOR X-MIN Y-MAX)
	      (LG-DRAW-VECTOR X-MIN (f1- Y-MAX))
	      (LG-END-VECTOR X-MAX (f1- Y-MAX)))
	 (CURSORPOS* OLDROW (f+ OLDCOL 4)))
	((AND (NOT LINEAR?) $LINEDISP (FBOUNDP 'CG-D-SUMSIGN))
	 (CG-BEGIN-GRAPHICS)
	 (CG-D-SUMSIGN)
	 (CG-END-GRAPHICS)
	 (INCREMENT OLDCOL 4))
	(T (SETQ DMSTR '((0 2 (D-HBAR 4 #\=))
			 (-4 1 #.back-slash-char) #\> (-2 -1 #.forward-slash-char)
			 (-1 -2 (D-HBAR 4 #\=))))
	   (IF LINEAR?
	       (DRAW-LINEAR DMSTR OLDROW OLDCOL)
	       (DRAW-2D	    DMSTR OLDROW OLDCOL)))))

;; Notice how this calls D-VBAR in the non-graphic case.  The entire output
;; side should be structured this way, with no consing of intermediate
;; dimension strings.

(DEFUN D-MATRIX (LINEAR? DIRECTION H D)
  (DECLARE (FIXNUM H D))
  (COND ((AND (NOT LINEAR?) LINE-GRAPHICS-TTY $LINEDISP)
	 (LET ((X-MIN (f1+ (f* LG-CHARACTER-X OLDCOL)))
	       (X-MAX (f1- (f* LG-CHARACTER-X (f1+ OLDCOL))))
	       (Y-MIN (f+ (f* LG-CHARACTER-Y (f+ OLDROW 1 (f- H))) 2))
	       (Y-MAX (f- (f* LG-CHARACTER-Y (f+ OLDROW 1 D)) 2)))
	      (declare (fixnum  X-MIN X-MAX Y-MIN Y-MAX))
	      (IF (EQ DIRECTION 'RIGHT) (PSETQ X-MIN X-MAX X-MAX X-MIN))
	      (LG-SET-POINT   X-MAX Y-MIN)
	      (LG-DRAW-VECTOR X-MIN Y-MIN)
	      (LG-DRAW-VECTOR X-MIN Y-MAX)
	      (LG-END-VECTOR  X-MAX Y-MAX))
	 (CURSORPOS* OLDROW (f1+ OLDCOL)))
	((AND (NOT LINEAR?) CHARACTER-GRAPHICS-TTY $LINEDISP)
	 (COND ((= (f+ H D) 1)
		(TYO* (GETCHARN (IF (EQ DIRECTION 'RIGHT) $RMXCHAR $LMXCHAR)
				2)))
	       (T (CURSORPOS* (f+ OLDROW 1 (f- H)) OLDCOL)
		  (CG-BEGIN-GRAPHICS)
		  (IF (EQ DIRECTION 'RIGHT) (CG-UR-CORNER) (CG-UL-CORNER))
		  (CG-END-GRAPHICS)
		  (CURSORPOS* (f+ OLDROW -1 H) OLDCOL)
		  (COND ((> (f+ H D) 2)
			 (D-VBAR NIL (f1- H) (f1- D))
			 (CURSORPOS* (f+ OLDROW D) (f1- OLDCOL)))
			(T (CURSORPOS* (f+ OLDROW D) OLDCOL)))
		  (CG-BEGIN-GRAPHICS)
		  (IF (EQ DIRECTION 'RIGHT) (CG-LR-CORNER) (CG-LL-CORNER))
		  (CG-END-GRAPHICS)
		  (CURSORPOS* (f- OLDROW D) (f1+ OLDCOL)))))
	(T (D-VBAR LINEAR? H D 
		   (GETCHARN (IF (EQ DIRECTION 'RIGHT) $RMXCHAR $LMXCHAR)
			     2)))))

;; There is wired knowledge of character offsets here.

(DEFUN D-BOX (LINEAR? H D W BODY &AUX (CHAR 0) DMSTR)
       ;char a char?
 (DECLARE (FIXNUM H D W ))
 (COND ((AND (NOT LINEAR?) LINE-GRAPHICS-TTY $LINEDISP)
	 (LET ((X-MIN (f* LG-CHARACTER-X OLDCOL))
	       (X-MAX (f* LG-CHARACTER-X (f+ OLDCOL W 2)))
	       (Y-MIN (f+ (f* LG-CHARACTER-Y (f- OLDROW H)) 2))
	       (Y-MAX (f- (f* LG-CHARACTER-Y (f+ OLDROW D 2)) 2)))
	      (declare (fixnum X-MIN X-MAX Y-MIN Y-MAX))
	      (LG-SET-POINT X-MIN Y-MIN)
	      (LG-DRAW-VECTOR X-MAX Y-MIN)
	      (LG-DRAW-VECTOR X-MAX Y-MAX)
	      (LG-DRAW-VECTOR X-MIN Y-MAX)
	      (LG-END-VECTOR  X-MIN Y-MIN))
	 (CURSORPOS* OLDROW (f1+ OLDCOL))
	 (DRAW-2D BODY OLDROW OLDCOL)
	 (CURSORPOS* OLDROW (f+ OLDCOL 1)))
	((AND (NOT LINEAR?) CHARACTER-GRAPHICS-TTY $LINEDISP)
	 (D-MATRIX NIL 'LEFT (f1+ H) (f1+ D))
	 (CURSORPOS* (f- OLDROW H) OLDCOL)
	 (D-HBAR NIL W)
	 (CURSORPOS* (f+ OLDROW H) (f- OLDCOL W))
	 (DRAW-2D BODY OLDROW OLDCOL)
	 (CURSORPOS* (f+ OLDROW D 1) (f- OLDCOL W))
	 (D-HBAR NIL W)
	 (CURSORPOS* (f- OLDROW D 1) OLDCOL)
	 (D-MATRIX NIL 'RIGHT (f1+ H) (f1+ D)))
	(T (SETQ CHAR (GETCHARN $BOXCHAR 2))
	   (SETQ DMSTR
		 `((0 ,H (D-HBAR ,(f+ 2 W) ,CHAR))
		   (,(f- (f+ W 2)) 0)
		   (D-VBAR ,H ,D ,CHAR)
		   ,@BODY
		   (,(f- (f1+ W)) ,(f- (f1+ D)) (D-HBAR ,(f+ W 2) ,CHAR))
		   (-1 0)
		   (D-VBAR ,H ,D ,CHAR)))
	   (IF LINEAR?
	       (DRAW-LINEAR DMSTR OLDROW OLDCOL)
	       (DRAW-2D DMSTR OLDROW OLDCOL)))))


;; Primitive functions for doing equation drawing.

;; Position the cursor at a given place on the screen.  %TDMV0 does
;; absolute cursor movement.

#+ITS
(DEFUN CURSORPOS* (ROW COL)
  (DECLARE (FIXNUM ROW COL))
  (+TYO #.%TDMV0 DISPLAY-FILE)
  (+TYO ROW DISPLAY-FILE)
  (+TYO COL DISPLAY-FILE)
  (SETQ OLDROW ROW OLDCOL COL))

#-ITS
(DEFUN CURSORPOS* (ROW COL)
  (DECLARE (FIXNUM ROW COL))
  (CURSORPOS ROW COL)
  (SETQ OLDROW ROW OLDCOL COL))

;; This function is transmitting ITS output buffer codes in addition to
;; standard ascii characters.  See INFO;ITSTTY > for documentation.  This
;; should convert tabs to direct cursor positioning commands since otherwise
;; they get stuffed down the raw stream and appear as gammas on sail consoles
;; and lose completely on terminals which can't tab.  Backspace also loses,
;; but its nearly impossible to get a string with backspace in it in Macsyma.
;; Also, DISPLA can't dimension it correctly.

#+(or cl ITS NIL)
(DEFUN TYO* (CHAR)
  #-cl (DECLARE (FIXNUM CHAR))
  (COND ((char= #\Backspace CHAR) (SETQ OLDCOL (f1- OLDCOL)))	;Backspace
	((char< CHAR #. (code-char 128.)) (SETQ OLDCOL (f1+ OLDCOL))))	;Printing graphic
  #+ITS (+TYO CHAR DISPLAY-FILE)
  #-ITS (tyo char))

#-(or ITS NIL cl)
(DEFUN TYO* (CHAR)
  (DECLARE (FIXNUM CHAR))
  (IF (< CHAR 128.) (SETQ OLDCOL (f1+ OLDCOL)))	;Printing graphic
  (TYO CHAR))


;; Functions used by the packages for doing character graphics.
;; See MAXSRC;H19 or VT100.

#+ITS (PROGN 'COMPILE

(DEFMFUN CG-TYO (CHAR) (+TYO CHAR DISPLAY-FILE))

;; ITS does not change its idea of where the cursor position is when characters
;; are slipped by it using %TDQOT.  This is used for operations which just
;; change the state of the terminal without moving the cursor.  For actually
;; drawing characters, we use ordinary tyo since the cursor does indeed get
;; moved forward a position.  Fortunately, it only takes one character to draw
;; each of the special characters.

(DEFMFUN CG-IMAGE-TYO (CHAR)
	 (CG-TYO #.%TDQOT)
	 (CG-TYO CHAR))

) ;; End of conditional

#+NIL (progn
(defmfun cg-tyo (char-code)
  (send *terminal-io* :write-char (code-char char-code)))

(defmfun cg-image-tyo (char-code)
  (send *terminal-io* :write-raw-char (code-char char-code)))

;Moving the stuff to a buffer to ensure that it will get output in one blast
; is worth it on the vax...
(defvar *cg-tyo-list-buf*
  nil)

(defun cg-tyo-list (l imagep &aux (msg (if imagep :raw-oustr :oustr)))
  (prog ((b (or *cg-tyo-list-buf* (setq *cg-tyo-list-buf* (make-string 20))))
	 (*cg-tyo-list-buf* nil)
	 (i 0))
   a	(cond ((or (null l) (=& i 20))
	         (send *terminal-io* msg b 0 i)
		 (if (null l) (return nil) (setq i 0)))
	      (t (setf (schar b i) (code-char (car l)))
		 (setq l (cdr l) i (1+& i))
		 (go a)))))

) ;; End of conditional

#-(or ITS NIL) 
(PROGN 'COMPILE

(DEFMFUN CG-TYO (CHAR)
  ;Surely you jest.
  ;`(TYO ,CHAR)
  (tyo char)
  )
(DEFMFUN CG-IMAGE-TYO (CHAR)
  ;Ditto.
  ;`(TYO ,CHAR)
  (tyo char)
  )

) ;; End of conditional
(DEFMFUN CG-TYO-N (L)
  #+NIL (cg-tyo-list l nil)
  #-NIL (MAPC #'CG-TYO L))


;; Things to do:
;; * Rewrite TYO* and CURSORPOS* to be "stream" oriented, i.e. they
;;   either draw directly to the screen or into the linearray depending
;;   upon the mode of output.  This way, the HBAR and VBAR drawing functions
;;   can be written only in terms of TYO*, etc. and never cons.
;;   DRAW-LINEAR and DRAW-2D can be merged into a single function.
;; * Instead of calling NREVERSE from OUTPUT, call a function which
;;   reverses at all levels and remove calls to REVERSE from DRAW-LINEAR
;;   and DRAW-2D.
;; * Dimension functions should know whether the output must be linear.
;;   This way they can do variable sized summation and integral signs,
;;   graphical square root or SQRT(X), %PI  , >=  , etc.
;;   These are situations where the size of the dimensioned
;;   result depends upon the form of the output.
;; * Fix display of MLABOX for graphic consoles.


(DEFMFUN CG-IMAGE-TYO-N (L)
  #+NIL (cg-tyo-list l t)
  #-NIL (MAPC #'CG-IMAGE-TYO L))

