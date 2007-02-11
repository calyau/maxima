;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module displa)

;; N.B. You must read the macro file before reading this file.

(load-macsyma-macros displm)

(declare-top (special $linel))


;; Global variables defined in this file.  Most of these are switches
;; controlling display format

;;(DEFMVAR CHARACTER-GRAPHICS-TTY NIL
;;	 "If T, then console can draw lines and math symbols using
;;	 an extended character set.")

;;(DEFMVAR LINE-GRAPHICS-TTY NIL
;;	 "If T, then console can draw lines and math symbols using
;;	 vector graphics.")

(defmvar $cursordisp t
  "If T, causes expressions to be drawn by the displayer in logical 
	  sequence.  This only works with a console which can do cursor 
	  movement.
	  If NIL, expressions are simply printed line by line.
	  CURSORDISP is NIL when a WRITEFILE is in effect."
  no-reset)

(defmvar $stardisp nil
  "Causes factors of products are separated by * when displayed.")

(defmvar $leftjust nil
  "Causes equations to be drawn left justified rather than centered.
	 For slow consoles.")

(defmvar $display2d t
  "Causes equations to be drawn in two dimensions.  Otherwise, drawn
	 linearly.")

(defmvar $lispdisp nil
  "Causes symbols not having $ as the first character in their pnames
	 to be preceded with a ? when displayed.")

;; This may be flushed in the future if nobody dislikes the graphics crocks.

(defmvar $linedisp t
  "Causes quotients, matrices, and boxes to be drawn with straight
	 lines, if possible.  This will work on graphic terminals or
	 video terminals with line drawing character sets.  If enabled,
	 the values of LMXCHAR, RMXCHAR, ABSBOXCHAR, and BOXCHAR are ignored.")

(defmvar $derivabbrev nil)

(defmvar $noundisp nil)

(defmvar $stringdisp nil
  "Causes strings to be bracketed in double quotes when displayed.
	 Normally this is off, but is turned on when a procedure definition is
	 being displayed.")
;;#+Franz
;;(defmvar $typeset nil
;; 	"Causes equations to be output in a typesetter readable form if t.")

(defmvar displayp nil "Is T when inside of `displa'")

;; More messages which appear during the middle of display.  Different
;; from those which appear during typein.  MOREMSG and MOREFLUSH get
;; bound to these.

(defvar d-moremsg "--More Display?--")
(defvar d-moreflush "--Display Flushed--")

;; Parameters which control how boxes, absolute value signs,
;; evaluation-at-a-point bars, and matrices are drawn.

(defmvar $boxchar '|&"|
  "Character used for drawing boxes.")
(defmvar $absboxchar '|&!|
  "Character used for drawing absolute value signs and 'evaluation at' signs.")
(defmvar $lmxchar '|&[|
  "Character used for drawing the left edge of a matrix.")
(defmvar $rmxchar '|&]|
  "Character used for drawing the right edge of a matrix.")

;; These variables are bound within Macsyma Listeners since they are different
;; for each window.  Set them here, anyway, so that RETRIEVE can be called from
;; top level.  The size of TOP-WINDOW is wired in here.

;;#+lispm
;;(SETQ SMART-TTY T RUBOUT-TTY T LINE-GRAPHICS-TTY t SCROLLP NIL)

(setq smart-tty nil rubout-tty nil scrollp t)


(setq   linel 79. $linel 79. ttyheight 24.)

;;#+lispm
;;(multiple-value-bind (a b)
;;  (send *terminal-io* :send-if-handles :size-in-characters)
;;  (when (and a b) (setq linel a ttyheight b)
;;    (setq linel 79 ttyheight 24)))

;;#+NIL
;;(multiple-value-bind (a b)
;;  (send *terminal-io* :send-if-handles :size-in-characters)
;;  (when (and a b) (setq linel a ttyheight b)
;;    (setq linel 79 ttyheight 24))
;;  (setq smart-tty nil rubout-tty nil scrollp t))

;; Default settings for random systems.
;;#-(OR ITS LISPM cl)
;;(SETQ SMART-TTY NIL RUBOUT-TTY NIL SCROLLP T
;;      LINEL 79. $LINEL 79. TTYHEIGHT 24.)




(defvar linearray (make-array 80. :initial-element nil))

(defmfun maxima-display (form &key (stream *standard-output*) )
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
	 (let ((mratp (checkrat form))
	       (#.writefilep #.writefilep)
	       (maxht     1) (maxdp   0) (width   0)
	       (height    0) (depth   0) (level   0) (size   2)
	       (break     0) (right   0) (lines   1) bkpt
	       (bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	       (bkptlevel 0) in-p
	       (moreflush d-moreflush)
	       more-^w
	       (moremsg d-moremsg))
	   (progn (setq dim-list (dimension form
					    nil 'mparen 'mparen 0 0)))
	   (cond ($cursordisp  (draw-2d (nreverse dim-list) at-x at-y))
		 (t (cursorpos (f- at-x (sub1 height)) 0)
		    (draw-linear (nreverse dim-list) (f+ at-x height) at-y)
	   
		    (loop for i downfrom (sub1 (length linearray)) to 0
			   when (aref linearray i)
			   do (output-linear-one-line i)))))
      (clear-linearray))
    (cursorpos prev-x prev-y)))

(defmacro bind-dimension (form dim-list &rest body)
  
  `(let ((mratp (checkrat ,form))
	 (#.writefilep #.writefilep)
	 (maxht     1) (maxdp   0) (width   0)
	 (height    0) (depth   0) (level   0) (size   2)
	 (break     0) (right   0) (lines   1) bkpt
	 (bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
	 (bkptlevel 0) in-p
	 (moreflush d-moreflush)
	 more-^w
	 (moremsg d-moremsg))
    (progn (setq ,dim-list (dimension ,form
				      nil 'mparen 'mparen 0 0))
	   ,@ body)))

(defvar *alt-display2d* nil)
(defvar *alt-display1d* nil)

(defmfun displa (form &aux #+kcl(form form))
  (if (or (not #.ttyoff) #.writefilep)
      (cond #+franz ($typeset (apply #'$photot (list form)))
	    ($display2d 
	     (cond
	       (*alt-display2d* (apply *alt-display2d* form ()))
	       (t 
		(let ((displayp t)
		      (linearray (if displayp (make-array 80.) linearray))
		      (mratp (checkrat form))
		      (#.writefilep #.writefilep)
		      (maxht     1) (maxdp   0) (width   0)
		      (height    0) (depth   0) (level   0) (size   2)
		      (break     0) (right   0) (lines   1) bkpt
		      (bkptwd    0) (bkptht  1) (bkptdp  0) (bkptout 0)
		      (bkptlevel 0) in-p
		      (moreflush d-moreflush)
		      more-^w
		      (moremsg d-moremsg))
		  (unwind-protect
		       (progn (setq form (dimension form
						    nil 'mparen 'mparen 0 0))
			      (checkbreak form width)
			      (output form (if (and (not $leftjust) (= 2 lines))
					       (f- linel (f- width bkptout))
					       0))
			      ;;			    (IF (AND SMART-TTY (NOT (AND SCROLLP (NOT $CURSORDISP)))
			      ;;				     (> (CAR (CURSORPOS)) (f- TTYHEIGHT 3)))
			      ;;				(LET (#.writefilep) (MTERPRI)))
			      )
		    ;; make sure the linearray gets cleared out.
		    (clear-linearray))))))
	    (t 
	     (cond
	       (*alt-display1d* (apply *alt-display1d* form ()))
	       (t (linear-displa form)))))))

;;(defun transform-extends (x)
;;  (cond (($extendp x)
;;	 (let ((nom (send x ':macsyma-extend-type)))
;;	   (append `((${) ,nom)
;;		   (mapcar #'(lambda (u v) (list '(mequal) u (transform-extends v)))
;;			   #-cl
;;			   (cdr (mfunction-call $get nom '$accessors))
;;			   #+cl  ;;wouldn't compile because mfunction-call not defined
;;			   (cdr (funcall '$get nom '$acessors))
;;			   (listarray (send x ':macsyma-extend-elements))))))
;;	((atom x) x)
;;	(t
;;	 (do ((obj x (cdr obj))
;;	      (new x)
;;	      (slot 0 (f1+ slot)))
;;	     ((null obj) new)
;;	     (let* ((element (car obj))
;;		    (result (transform-extends element)))
;;	       (if (not (eq element result))
;;		   (block nil
;;		     (if (eq new x) (setq new (copy-seq x)))
;;		     (setf (nth slot new) result))))))))

(defmvar $display_format_internal nil
  "Setting this TRUE can help give the user a greater understanding
	 of the behavior of macsyma on certain of his problems,
	 especially those involving roots and quotients")

(defun nformat-check (form)
  (if (and $display_format_internal
	   (not (or (atom form) (atom (car form)) (specrepp form))))
      form
      (nformat form)))

(defun dimension (form result lop rop w right)
  (let ((level (f1+ level)) (break (if (and w break) (f+ w break))))
    (setq form (nformat-check form))
    (cond ((atom form)
	   (dimension-atom form result))
	  ((and (atom (car form)) (setq form (cons '(mprogn) form)) nil))
	  ((or (<= (lbp (caar form)) (rbp lop)) (> (lbp rop) (rbp (caar form))))
	   (dimension-paren form result))
	  ((memq 'array (car form)) (dimension-array form result))
	  ((safe-get (caar form) 'dimension) 
	   (funcall (get (caar form) 'dimension) form result))
	  (t (dimension-function form result)))))

(defvar atom-context 'dimension-list)
;; bound by DIMENSION-ARRAY and DIMENSION-FUNCTION.
;; This ATOM-CONTEXT put in by GJC so that MCW could have a clean
;; hook by which to write his extensions for vector-underbars.

;;(declare-top (*EXPR DIMENSION-ARRAY-OBJECT)) ;to be defined someplace else.

;; Referenced externally by RAT;FLOAT.

(defmfun dimension-atom (form result)
  (cond ((and (symbolp form) (get form atom-context))
	 (funcall (get form atom-context) form result))
	((stringp form) (dimension-string (makestring form) result))
	((ml-typep form 'array)
	 (dimension-array-object form result))
	(t (dimension-string (makestring form) result))))

;; Referenced externally by anyone who wants to display something as
;; a funny looking atom, e.g. Trace, Mformat.

(defmfun dimension-string (dummy result &aux crp)
  ;; N.B. String is a list of fixnums.
  (setq width 0 height 1 depth 0)
  (do ((l dummy (cdr l))) ((null l))
    (increment width)
    (if (char= (car l) #\newline) (setq crp t)))
  (cond
    ((or (and (checkfit width) (not crp)) (not break))
     (nreconc dummy result))
    (t (setq width 0)
       (do ((l dummy) (w (f- linel (f- break bkptout))))
	   ((null l) (checkbreak result width) result)
	 (setq dummy l l (cdr l))
	 (cond ((char= (car dummy) #\newline)
		(forcebreak result width)
		(setq result nil w (f+ linel width)))
	       (t (increment width)
		  (when (and (= w width) l)
		    (forcebreak (cons #\\ result) width)
		    (setq result nil w (f+ linel width))
		    (increment width))
		  (setq result (rplacd dummy result))))))))

(defmfun makestring (atom)
  (let (dummy)
    (cond ((numberp atom) (exploden atom))
	  #+(or nil cl)
	  ((not (symbolp atom)) (exploden atom))
	  ((and (setq dummy (get atom 'reversealias))
		(not (and (memq atom $aliases) (get atom 'noun))))
	   (exploden dummy))
	  ((not (eq (getop atom) atom))
	   (setq dummy (exploden (getop atom)))
	   (if (char= #\& (car dummy))
	       (cons double-quote-char (nconc (cdr dummy) (list double-quote-char)))
	       (cdr dummy)))
	  (t (setq dummy (exploden atom))
	     (cond ((char= #\$ (car dummy)) (cdr dummy))
		   ((and $stringdisp (char= #\& (car dummy)))
		    (cons double-quote-char (nconc (cdr dummy) (list double-quote-char))))
		   ((or (char= #\% (car dummy)) (char= #\& (car dummy))) (cdr dummy))
		   ($lispdisp (cons #\? dummy))
		   (t dummy))))))
(defun dimension-paren (form result)
  (setq result (cons right-parentheses-char (dimension form (cons left-parentheses-char result) 'mparen 'mparen 1 (f1+ right))))
  (setq width (f+ 2 width))
  result)

(defun dimension-array (x result)
  (prog (dummy bas (w 0) (h 0) (d 0) sub) (declare (fixnum w h d))
					;(SETQ W 0)
	(if (eq (caar x) 'mqapply) (setq dummy (cadr x) x (cdr x))
	    (setq dummy (caar x)))
	(cond ((or (not $noundisp) (not (symbolp (caar x)))))
	      ((and (get (caar x) 'verb) (get (caar x) 'alias))
	       (push-string "''" result) (setq w 2))
	      ((and (get (caar x) 'noun) (not (memq (caar x) (cdr $aliases)))
		    (not (get (caar x) 'reversealias)))
	       (setq result (cons #\' result) w 1)))
	(setq sub (let ((lop 'mparen) (rop 'mparen) (break nil) (size 1))
		    (dimension-list x nil))
	      w (f+ w width) h height d depth)
	(setq bas (if (and (not (atom dummy)) (memq 'array (car dummy)))
		      (let ((break nil) (right 0)) (dimension-paren dummy result))
		      (let ((atom-context 'dimension-array))
			(dimension dummy result lop 'mfunction nil 0))))
	(cond ((not (checkfit (setq width (f+ w width))))
	       (return (dimension-function (cons '(subscript) (cons dummy (cdr x))) result)))
	      ((char= right-parentheses-char (car bas))
	       (setq result (cons (cons 0 (cons (f- h) sub)) bas) depth (max (f+ h d) depth)))
	      (t (setq result (cons (cons 0 (cons (f- (f+ depth h)) sub)) bas)
		       depth (f+ h d depth))))
	(update-heights height depth)
	(return result)))

(defun dimension-function (x result)
  (prog (fun (w 0) (h 0) (d 0)) (declare (fixnum w h d))
					;(SETQ W 0)
	(cond ((or (not $noundisp) (not (symbolp (caar x)))))
	      ((and (get (caar x) 'verb) (get (caar x) 'alias))
	       (push-string "''" result) (setq w 2))
	      ((and (get (caar x) 'noun) (not (memq (caar x) (cdr $aliases)))
		    (not (get (caar x) 'reversealias)))
	       (setq result (cons #\' result) w 1)))
	(if (eq (caar x) 'mqapply) (setq fun (cadr x) x (cdr x)) (setq fun (caar x)))
	(setq result (let ((atom-context 'dimension-function))
		       (dimension fun result lop 'mparen 0 1))
	      w (f+ w width) h height d depth)
	(cond ((null (cdr x))
	       (setq result (list* right-parentheses-char left-parentheses-char result) width (f+ 2 w)))
	      (t (setq result (let ((lop 'mparen) (rop 'mparen)
				    (break (if break (f+ 1 w break))))
				(cons right-parentheses-char (dimension-list x (cons left-parentheses-char result))))
		       width (f+ 2 w width) height (max h height) depth (max d depth))))
	(return result)))

(defmfun dimension-prefix (form result)
  (prog (dissym (symlength 0))
     (declare (fixnum symlength))
     (setq dissym (safe-get (caar form) 'dissym) symlength (length dissym))
     (setq result
	   (dimension (cadr form) (reconc dissym result) (caar form) rop symlength right)
	   width (f+ symlength width))
     (return result)))

(defun dimension-list (form result)
  (prog ((w 0) (h 0) (d 0))
     (declare (fixnum w h d))
     (setq result (dimension (cadr form) result lop 'mcomma 0 right)
	   w width h height d depth)
     (do ((l (cddr form) (cdr l))) ((null l))
       (push-string ", " result)
       (increment w 2)
       (checkbreak result w)
       (setq result (dimension (car l) result 'mcomma 'mcomma w right)
	     w (f+ w width) h (max h height) d (max d depth)))
     (setq width w height h depth d)
     (return result)))

(defmfun dimension-infix (form result)
  (if (not (= (length (cdr form)) 2))
    (return-from dimension-infix (dimension-function form result)))
  (prog (dissym (symlength 0) (w 0) (h 0) (d 0))
     (declare (fixnum symlength w h d))
     (setq dissym (safe-get (caar form) 'dissym) symlength (length dissym)
	   result (dimension (cadr form) result lop (caar form) 0 symlength)
	   w width h height d depth)
     (setq result (reconc dissym result))
     (checkbreak result (f+ symlength w))
     (setq result (dimension (caddr form) result (caar form) rop (f+ symlength w) right)
	   width (f+ w symlength width) height (max h height) depth (max d depth))
     (return result)))

(defmfun dimension-nary (form result)
  ;; If only 0 or 1 arguments, then print "*"() or "*"(A)
  (cond ((null (cddr form)) (dimension-function form result))
	(t (prog (dissym (symlength 0) (w 0) (h 0) (d 0))
	      (declare (fixnum symlength w h d))
	      (setq dissym (safe-get (caar form) 'dissym)
		    symlength (length dissym)
		    result (dimnary (cadr form) result lop (caar form) (caar form) 0)
		    w width h height d depth)
	      (do ((l (cddr form) (cdr l))) (nil)
		(checkbreak result w)
		(setq result (reconc dissym result) w (f+ symlength w))
		(cond ((null (cdr l))
		       (setq result (dimnary (car l) result (caar form) (caar form) rop w)
			     width (f+ w width) height (max h height) depth (max d depth))
		       (return t))
		      (t (setq result (dimnary (car l) result (caar form)
					       (caar form) (caar form) w)
			       w (f+ w width) h (max h height) d (max d depth)))))
	      (return result)))))

;; Check for (f* A (f* B C)) --> A*(B*C)

(defun dimnary (form result lop op rop w)
  (if (and (not (atom form)) (eq (caar form) op))
      (dimension-paren form result)
      (dimension form result lop rop w right)))

(defmfun dimension-postfix (form result)
  (prog (dissym (symlength 0)) (declare (fixnum symlength))
	(setq dissym (safe-get (caar form) 'dissym) symlength (length dissym))
	(setq result (dimension (cadr form) result lop (caar form) 0 (f+ symlength right))
	      width (f+ symlength width))
	(return (reconc dissym result))))

(defmfun dimension-nofix (form result)
  (setq form (safe-get (caar form) 'dissym) width (length form))
  (reconc form result))

(defun dimension-match (form result)
  (prog (dissym (symlength 0))
     (declare (fixnum symlength))
     (setq dissym (safe-get (caar form) 'dissym) symlength (length (car dissym)))
     (cond ((null (cdr form))
	    (setq width (f+ symlength (length (cdr dissym))) height 1 depth 0)
	    (return (reconc (cdr dissym) (reconc (car dissym) result))))
	   (t (setq result (let ((lop 'mparen)
				 (rop 'mparen)
				 (break (if break (f+ symlength break)))
				 (right (f+ symlength right)))
			     (dimension-list form (reconc (car dissym) result))))
	      (setq width (f+ (length (cdr dissym)) symlength width))
	      (return (reconc (cdr dissym) result))))))

(defmfun dimension-superscript (form result)
  (prog (exp (w 0) (h 0) (d 0) bas)
     (declare (fixnum w h d ))
     (setq exp (let ((size 1)) (dimension (caddr form) nil 'mparen 'mparen nil 0))
	   w width h height d depth)
     (cond ((and (not (atom (cadr form))) (memq 'array (cdaadr form)))
	    (prog (sub (w2 0) (h2 0) (d2 0))
	       (declare (fixnum w2 h2 d2))
	       (setq sub (if (eq 'mqapply (caaadr form))
			     (cdadr form) (cadr form)))
	       (setq sub (let ((lop 'mparen) (break nil) (size 1))
			   (dimension-list sub nil))
		     w2 width h2 height d2 depth)
	       (setq bas (dimension (mop (cadr form)) result lop 'mexpt nil 0))
	       (when (not (checkfit (f+ width (max w w2))))
		 (setq result (dimension-function (cons '($expt) (cdr form)) result))
		 (return result))
	       (setq result (cons (cons 0 (cons (f+ height d) exp)) bas))
	       (setq result (cons (cons (f- w) (cons (f- (f+ depth h2)) sub)) result))
	       (setq result (cons (list (f- (max w w2) w2) 0) result)
		     width (f+ width (max w w2)) height (f+ h d height) depth (f+ d2 h2 depth)))
	    (update-heights height depth)
	    (return result))
	   ((and (atom (caddr form))
		 (not (atom (cadr form)))
		 (not (safe-get (caaadr form) 'dimension))
		 (prog2 (setq bas (nformat-check (cadr form)))
		     (not (safe-get (caar bas) 'dimension))))
	    (return (dimension-function
		     (list* '(mqapply) (list '(mexpt) (mop bas) (caddr form)) (margs bas))
		     result)))
	   (t (setq bas (dimension (cadr form) result lop 'mexpt nil 0) width (f+ w width))
	      (if (not (checkfit width))
		  (return (dimension-function (cons '($expt) (cdr form)) result)))
	      (if
	       #-cl
	       (and (numberp (car bas)) (char= right-parentheses-char (car bas)))
	       #+cl  (eql right-parentheses-char (car bas))
	       (setq result (cons (list* 0 (f1+ d) exp) bas) height (max (f+ 1 h d) height))
	       (setq result (cons (list* 0 (f+ height d) exp) bas) height (f+ h d height)))
	      (update-heights height depth)
	      (return result)))))

(defun dsumprod (form result d-form sw sh sd)
  (declare (fixnum  sw sh sd))
  (prog (dummy (w 0) (h 0) (d 0) dummy2 (lsum (eq (caar form) '%lsum)))
     (declare (fixnum w h d ))
     (setq dummy2 (dimension (caddr form) nil 'mparen 'mequal nil 0)
	   w width h height d depth)
     (if lsum (push-string " in "  dummy2)
	 (push-string " = " dummy2))
     (setq dummy2 (dimension (cadddr form) dummy2 'mequal 'mparen nil 0)
	   w (f+ 3 w width) h (max h height) d (max d depth))
     (or lsum
	 (setq dummy (dimension (cadr (cdddr form)) nil 'mparen 'mparen nil 0)))
     (cond ((not (checkfit (max w width))) (return (dimension-function form result))))
     (setq dummy2 (cons (cons (f- sw) (cons (f- (f+ sd h)) dummy2)) (cons d-form result)))
     (cond ((> width sw) (setq sw 0))
	   (t (setq sw (// (f- sw width) 2) width (f+ sw width))))
     (setq dummy (cons (cons (f- sw w) (cons (f+ sh depth) dummy)) dummy2)
	   w (max w width) d (f+ sd h d) h (f+ sh height depth))
     (update-heights h d)
     (setq dummy (dimension (cadr form) (cons (list (f1+ (f- w width)) 0) dummy)
			    (caar form) rop w right)
	   width (f+ 1 w width) height (max h height) depth (max d depth))
     (return dummy)))

(displa-def bigfloat  dim-bigfloat)
(displa-def mquote    dimension-prefix "'")
(displa-def msetq     dimension-infix  " : ")
(displa-def mset      dimension-infix  " :: ")
(displa-def mdefine   dim-mdefine      " := ")
(displa-def mdefmacro dim-mdefine      " ::= ")

(defun dim-mdefine (form result)
  (let (($noundisp t) ($stringdisp t))
    (dimension-infix (if (cdddr form)
			 (list (car form) (cadr form) (cons '(mprogn) (cddr form)))
			 form)
		     result)))

(displa-def mfactorial dimension-postfix "!")
(displa-def mexpt      dimension-superscript)
(displa-def mncexpt    dim-mncexpt "^^")

(defun dim-mncexpt (form result)
  (dimension-superscript (list '(mncexpt) (cadr form) (cons '(mangle) (cddr form)))
			 result))

(displa-def mnctimes dimension-nary " . ")

(displa-def %product dim-%product 115.)

(defun dim-%product (form result) (dsumprod form result '(d-prodsign) 5 3 1))

(displa-def rat dim-rat #.forward-slash-string)	;;(setq forward-slash-string "//")

(defun dim-rat (form result)
  (if $pfeformat (dimension-nary form result) (dim-mquotient form result)))

(displa-def mquotient dim-mquotient #.forward-slash-string)

(defun dim-mquotient (form result)
  (if (not (= (length (cdr form)) 2))
    (return-from dim-mquotient (dimension-function form result)))
  (prog (num (w 0) (h 0) (d 0) den)
     (declare (fixnum w h d))
     (if (and (= 1 size) (atom (cadr form)) (atom (caddr form)))
	 (return (dimension-nary form result)))
     (setq num (dimension (cadr form) nil 'mparen 'mparen nil right)
	   w width h height d depth)
     (if (not (checkfit w)) (return (dimension-nary form result)))
     (setq den (dimension (caddr form) nil 'mparen 'mparen nil right))
     (if (not (checkfit width)) (return (dimension-nary form result)))
     (return (dratio result num w h d den width height depth))))

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
(defvar x1)
(defvar x2)
(eval-when (compile eval load)
  (setq x1 'h1 x2 'd2))


(defun dratio (result num w1 h1 d1 den w2 h2 d2)
  (declare (fixnum w1 h1 d1 w2 h2 d2))
  (setq width (max w1 w2) height (f+ 1 h1 d1) depth (f+ h2 d2))
  (setq #.x1 (// (f- width w1) 2) #.x2 (// (f- width w2) 2))
  (update-heights height depth)
  (push `(,#.x1 ,(f1+ d1) . ,num) result)
  (push `(,(f- #.x2 (f+ #.x1 w1)) ,(f- h2) . ,den) result)
  (push `(,(f- 0 #.x2 w2) 0) result)
  (push `(d-hbar ,width) result)
  result)

(displa-def mtimes dimension-nary " ")

;; This code gets run when STARDISP is assigned a value.

(defprop $stardisp stardisp assign)
(defun stardisp (symbol val)
  symbol			 ;ignored -- always bound to $STARDISP
  (putprop 'mtimes (if val '(#\*) '(#\space)) 'dissym))

(displa-def %integrate dim-%integrate 115.)

(defun dim-%integrate (form result)
  (prog (dummy (w 0)(h 0)(d 0) dummy2)
     (declare (fixnum w h d))
     (cond ((not (or (= (length (cdr form)) 2) (= (length (cdr form)) 4)))
            (return-from dim-%integrate (dimension-function form result)))
	   ((null (cdddr form))
	    (setq dummy `(#\space (d-integralsign) . ,result) w 2 h 3 d 2))
	   (t (setq dummy (dimension (cadr (cdddr form)) nil 'mparen 'mparen nil 0)
		    w width h height d depth)
	      (setq dummy2 (dimension (cadddr form) nil 'mparen 'mparen nil 0))
	      (if (not (checkfit (f+ 2 (max w width))))
		  (return (dimension-function form result)))
	      (setq dummy `((0 ,(f+ 3 d) . ,dummy) (d-integralsign) . ,result))
	      (setq dummy (cons (cons (f- w) (cons (f- (f+ 2 height)) dummy2)) dummy)
		    w (f+ 2 (max w width)) h (f+ 3 h d) d (f+ 2 height depth)
		    dummy (cons (list (f- w 1 width) 0) dummy))))
     (update-heights h d)
     (setq dummy (dimension (cadr form) dummy '%integrate 'mparen w 2)
	   w (f+ w width) h (max h height) d (max d depth))
     (push-string " d" dummy)
     (setq dummy (dimension (caddr form) dummy 'mparen rop (f+ 2 w) right)
	   width (f+ 2 w width) height (max h height) depth (max d depth))
     (return dummy)))

(displa-def %derivative dim-%derivative 125.)

(defun dim-%derivative (form result)
  (prog ()
     (cond ((null (cddr form))
	    (return (dimension-function (cons '(%diff) (cdr form)) result))))
     (cond ((null (cdddr form)) (setq form (append form '(1)))))
     (cond ((and $derivabbrev 
		 (do ((l (cddr form) (cddr l))) ((null l) t)
		   (cond ((and (atom (car l)) (integerp (cadr l)) (> (cadr l) 0)))
			 (t (return nil)))))
	    (return (dmderivabbrev form result)))
	   ((or (> (rbp lop) 130.) (> (lbp rop) 130.)
		(and (not (atom (cadr form))) (or (> (rbp lop) 110.) (> (lbp rop) 110.))))
	    (return (dimension-paren form result)))
	   (t (return (dmderivlong form result))))))

(defun dmderivabbrev (form result)
  (prog (dummy (w 0)) (declare (fixnum w))
	(do ((l (cddr form) (cddr l)) (var))
	    ((null l) (setq dummy (cdr dummy) w (f1- w)))
	  (setq var (dimension (car l) nil 'mparen 'mparen nil 0))
	  (do ((i (cadr l) (f1- i))) ((= 1 i)) (setq dummy (cons #\space (append var dummy))))
	  (setq dummy (cons #\space (nconc var dummy)) w (f+ w (cadr l) (f* (cadr l) width))))
	(setq result (dimension (cadr form) result lop '%deriv 0 right))
	(setq result (cons (cons 0 (cons (f- 0 depth 1) dummy)) result)
	      width (f+ w width) depth (max 1 (f1+ depth)))
	(update-heights height depth)
	(return result)))

(defun dmderivlong (form result)
  (prog (num (w1 0) (h1 0) (d1 0) den (w2 0)( h2 0)  (d2 0))
     (declare (fixnum w1 h1 d1 w2 h2 d2))
     (setq num (list (cadddr form))
	   den (cond ((equal 1 (cadddr form))
		      (dimension (caddr form)
				 (list #\d) 'mparen 'mparen nil 0))
		     (t (dimension-superscript
			 (cons '(diff)(cddr form)) (list #\d))))
	   w2 (f1+ width) h2 height d2 depth)
     (do ((l (cddddr form) (cddr l))) ((null l))
       (setq num (cons (cadr l) num)
	     den (cond ((equal 1 (cadr l))
			(dimension (car l) (cons #\d (cons #\space den))
				   'mparen 'mparen nil 0))
		       (t (dimension-superscript
			   (cons '(diff) l) (cons #\d (cons #\space den)))))
	     w2 (f+ 2 w2 width) h2 (max h2 height) d2 (f+ d2 depth)))
     (setq num (nformat-check (addn num t)))
     (cond ((equal 1 num) (setq num (list #\d) w1 1 h1 1 d1 0))
	   (t (setq num (dimension-superscript (list '(diff) #\d num) nil)
		    w1 width h1 height d1 depth)))
     (cond ((atom (setq form (nformat-check (cadr form))))
	    (setq num (dimension form num '%deriv 'mparen nil 0) w1 (f+ w1 width))
	    (return (dratio result num w1 h1 d1 den w2 h2 d2)))
	   (t (setq result (dratio result num w1 h1 d1 den w2 h2 d2) w1 width h1 height d1 depth)
	      (setq result (dimension form (cons #\space result) '%deriv rop w1 right)
		    width (f+ 1 w1 width) height (max h1 height) depth (max d1 depth))
	      (update-heights height depth)
	      (return result)))))

(displa-def %at dim-%at 105. 105.)

(defun dim-%at (form result)
  (prog (exp  eqs (w 0) (h 0) (d 0))
     (declare (fixnum w h d))
     (if (not (= (length (cdr form)) 2))
       (return-from dim-%at (dimension-function form result)))
     (setq exp (dimension (cadr form) result lop '%at nil 0)
	   w width h height d depth)
     (setq eqs (dimension (cond ((not (eq 'mlist (caar (caddr form)))) (caddr form))
				((null (cddr (caddr form))) (cadr (caddr form)))
				(t (cons '(mcomma) (cdaddr form))))
			  nil 'mparen 'mparen nil 0))
     (cond ((not (checkfit (f+ 1 w width))) (return (dimension-function form result))))
     (setq result (cons (cons 0 (cons (f- 0 1 d) eqs))
			(cons `(d-vbar ,(f1+ h) ,(f1+ d) ,(getcharn $absboxchar 2)) exp))
	   width (f+ 1 w width) height (f1+ h) depth (f+ 1 d depth))
     (update-heights height depth)
     (return result)))

(displa-def mminus dimension-prefix "- ")
(displa-def mplus  dim-mplus)
(defprop munaryplus (#\+ #\space) dissym)

(defun dim-mplus (form result)
  ;; If only 0 or 1 arguments, then print "+"() or +A
  (cond ((and (null (cddr form))
	      (not (memq (cadar form) '(trunc exact))))
	 (if (null (cdr form))
	     (dimension-function form result)
	     (dimension-prefix (cons '(munaryplus) (cdr form)) result)))
	(t (setq result (dimension (cadr form) result lop 'mplus 0 0))
	   (checkbreak result width)
	   (do ((l (cddr form) (cdr l))
		(w width) (h height) (d depth)
		(trunc (memq 'trunc (cdar form))) (dissym))
	       ((null l) (cond (trunc (setq width (f+ 8 w) height h depth d)
				      (push-string " + . . ." result)))
		result)
	     (declare (fixnum w h d))
	     (if (mmminusp (car l))
		 (setq dissym '(#\space #\- #\space) form (cadar l))
		 (setq dissym '(#\space #\+ #\space) form (car l)))
	     (cond ((and (not trunc) (null (cdr l)))
		    (setq result (dimension form (append dissym result)
					    'mplus rop (f+ 3 w) right)
			  width (f+ 3 w width)
			  height (max h height)
			  depth (max d depth))
		    (return result))
		   (t (setq result
			    (dimension form (append dissym result)
				       'mplus 'mplus (f+ 3 w) 0)
			    w (f+ 3 w width)
			    h (max h height)
			    d (max d depth))
		      (checkbreak result w)))))))

(displa-def %sum   dim-%sum 110.)
(displa-def %limit dim-%limit 110. 110.)

(displa-def %lsum   dim-%lsum 110.)
(defun dim-%lsum (form result) (dsumprod form result '(d-sumsign) 4 3 2))
(defun dim-%sum (form result) (dsumprod form result '(d-sumsign) 4 3 2))

(defun dim-%limit (form result)
  (prog ((w 0) (h 0) (d 0) dummy) (declare (fixnum w h d))
	(if (not (or (= (length (cdr form)) 3) (= (length (cdr form)) 4)))
      (return-from dim-%limit (dimension-function form result)))
	(setq dummy (dimension (third form) nil 'mparen 'mparen nil 0)
	      w width h height d depth)
	(push-string " -> " dummy)
	(setq dummy (dimension (fourth form) dummy 'mparen 'mparen nil 0)
	      w (f+ 4 w width) h (max h height) d (max d depth))
	(cond ((null (cddddr form)))
	      ((eq '$plus (fifth form))
	       (push #\+ dummy)
	       (increment w))
	      (t (push #\- dummy)
		 (increment w)))
	(push-string "limit" result)
	(setq dummy (cons (list* -5 (f- h) dummy) result) d (f+ h d))
	(update-heights 1 d)
	(setq dummy (dimension (cadr form) (cons '(1 0) dummy) '%limit rop (f1+ w) right))
	(setq width (f+ 1 w width) depth (max d depth))
	(return dummy)))

;; Some scheme needs to be worked out to allow use of mathematical character
;; sets on consoles which have them.

(displa-def marrow    dimension-infix  " -> " 80. 80.)
(displa-def mgreaterp dimension-infix  " > ")
(displa-def mgeqp     dimension-infix  " >= ")
(displa-def mequal    dimension-infix  " = ")
(displa-def mnotequal dimension-infix  " # ")
(displa-def mleqp     dimension-infix  " <= ")
(displa-def mlessp    dimension-infix  " < ")
(displa-def mnot      dimension-prefix "not ")
(displa-def mand      dimension-nary   " and ")
(displa-def mor	      dimension-nary   " or ")
(displa-def mcond     dim-mcond)
(displa-def %mcond    dim-mcond)

;; MCOND or %MCOND always has an even number of arguments.
;; The first two arguments are the foo and bar in 'if foo then bar .
;; Of the remaining pairs of arguments,
;; the first is MAYBE-ELSEIF and the second is ELSE-OR-THEN in the code below.
;; MAYBE-ELSEIF is T if the construct is 'if foo then bar else quux ,
;; otherwise the construct is 'if foo then bar elseif baz then quux 
;; where baz is the value of MAYBE-ELSEIF.
;; If ELSE-OR-THEN is NIL, just omit the final "else".

;; The parser appends (T NIL) to any if-then which lacks an else.
;; DIM-MCOND renders both '((%MCOND) $A $B) and '((%MCOND) $A $B T NIL) as "if a then b".

;; Examples. The "<==>" here means that the stuff on the right parses as the stuff on the left,
;; and the stuff on the left displays as the stuff on the right.

;; ((%mcond) $a $b t nil)                    <==> 'if a then b
;; ((%mcond) $a $b t $d)                     <==> 'if a then b else d
;; ((%mcond) $a $b $c nil t nil)             <==> 'if a then b elseif c then false
;; ((%mcond) $a $b $c $d t nil)              <==> 'if a then b elseif c then d
;; ((%mcond) $a $b $c $d t $f)               <==> 'if a then b elseif c then d else f
;; ((%mcond) $a $b $c $d $e nil t nil)       <==> 'if a then b elseif c then d elseif e then false
;; ((%mcond) $a $b $c $d $e $f t nil)        <==> 'if a then b elseif c then d elseif e then f
;; ((%mcond) $a $b $c $d $e $f t $h)         <==> 'if a then b elseif c then d elseif e then f else h
;; ((%mcond) $a $b $c $d $e $f $g nil t nil) <==> 'if a then b elseif c then d elseif e then f elseif g then false
;; ((%mcond) $a $b $c $d $e $f $g $h)        <==> 'if a then b elseif c then d elseif e then f elseif g then h

(defun dim-mcond (form result)
  (prog ((w 0) (h 0) (d 0)) (declare (fixnum w h d))
    (push-string "if " result)
    (setq result (dimension (cadr form) result 'mcond 'mparen 3 0)
          w (f+ 3 width) h height d depth) 
    (checkbreak result w) 
    (push-string " then " result)
    (setq result (dimension (caddr form) result 'mcond 'mparen (f+ 6 w) 0)
          w (f+ 6 w width) h (max h height) d (max d depth))

    (let ((args (cdddr form)))
      (loop while (>= (length args) 2) do
        (let ((maybe-elseif (car args)) (else-or-then (cadr args)))
          (cond
            ((and (eq maybe-elseif t) (= (length args) 2))
             (unless (or (eq '$false else-or-then) (eq nil else-or-then))
               (checkbreak result w)
               (push-string " else " result)
               (setq result (dimension else-or-then result 'mcond rop (f+ 6 w) right)
                     w (f+ 6 w width) h (max h height) d (max d depth))))

            (t
             (checkbreak result w)
             (push-string " elseif " result)
             (setq result (dimension maybe-elseif result 'mcond rop (f+ 8 w) right)
                   w (f+ 8 w width) h (max h height) d (max d depth))
             (checkbreak result w)
             (push-string " then " result)
             (setq result (dimension else-or-then result 'mcond rop (f+ 6 w) right)
                   w (f+ 6 w width) h (max h height) d (max d depth)))))
        (setq args (cddr args))))
           
    (setq width w height h depth d)
    (return result)))


(displa-def mdo dim-mdo)
(displa-def %mdo dim-mdo)

(defun dim-mdo (form result)
  (prog ((w 0) (h 0) (d 0) brkflag) (declare (fixnum w h d))
	(cond ((not (null (cadr form)))
	       (push-string "for " result)
	       (setq result (cons #\space (dimension (cadr form) result 'mdo 'mparen 4 right))
		     w (f+ 4 width) h height d depth brkflag t)))
	(cond ((or (null (caddr form)) (equal 1 (caddr form))))
	      (t (push-string "from " result)
		 (setq result
		       (cons #\space (dimension (caddr form) result 'mdo 'mparen (f+ 6 w) 0))
		       w (f+ 6 w width) h (max h height) d (max d depth))))
	(setq form (cdddr form))
	(cond ((equal 1 (car form)))
	      ((not (null (car form)))
	       (push-string "step " result)
	       (setq result (cons #\space (dimension (car form) result 'mdo 'mparen (f+ 6 w) 0))
		     w (f+ 6 w width) h (max h height) d (max d depth)))
	      ((not (null (cadr form)))
	       (push-string "next " result)
	       (setq result (cons #\space (dimension (cadr form) result 'mdo 'mparen (f+ 6 w) 0))
		     w (f+ 6 w width) h (max h height) d (max d depth))))
	(cond ((not (null (caddr form)))
	       (push-string "thru " result)
	       (setq result (cons #\space (dimension (caddr form) result 'mdo 'mparen (f+ 6 w) 0))
		     w (f+ 6 w width) h (max h height) d (max d depth) brkflag t)))
	(cond ((not (null (cadddr form)))
	       (cond ((and (not (atom (cadddr form))) (eq (caar (cadddr form)) 'mnot))
		      (push-string "while " result)
		      (setq result
			    (cons #\space (dimension (cadr (cadddr form)) result 'mdo 'mparen (f+ 7 w) 0))
			    w (f+ 7 w width) h (max h height) d (max d depth)))
		     (t (push-string "unless " result)
			(setq result
			      (cons #\space (dimension (cadddr form) result 'mdo 'mparen (f+ 8 w) 0))
			      w (f+ 8 w width) h (max h height) d (max d depth))))))
	(if brkflag (checkbreak result w))
	(push-string "do " result)
	(setq result (dimension (car (cddddr form)) result 'mdo rop (f+ 4 w) right)
	      width (f+ 4 w width) height (max h height) depth (max d depth))
	(return result)))


(displa-def mdoin dim-mdoin)
(displa-def %mdoin dim-mdoin)

(defun dim-mdoin (form result)
  (prog ((w 0) (h 0)  ( d 0)) (declare (fixnum w h d))
	(push-string "for " result)
	(setq result (dimension (cadr form) result 'mdo 'mparen 4 0)
	      w (f+ 4 width) h height d depth)
	(push-string " in " result)
	(setq result (dimension (caddr form) result 'mdo 'mparen (f+ 4 w) 0)
	      w (f+ 4 w width) h (max h height) d (max d depth))
	(setq form (cdr (cddddr form)))
	(cond ((not (null (car form)))
	       (push-string " thru " result)
	       (setq result (dimension (car form) result 'mdo 'mparen (f+ 6 w) 0)
		     w (f+ 6 w width) h (max h height) d (max d depth))))
	(cond ((not (null (cadr form)))
	       (push-string " unless " result)
	       (setq result (dimension (cadr form) result 'mdo 'mparen (f+ 8 w) 0)
		     w (f+ 8 w width) h (max h height) d (max d depth))))
	(push-string " do " result)
	(setq result (dimension (caddr form) result 'mdo rop (f+ 4 w) right)
	      width (f+ 4 w width) height (max h height) depth (max d depth))
	(return result)))

(displa-def mprogn dimension-match "(" ")")
(displa-def mlist  dimension-match "[" "]")
(displa-def mangle dimension-match "<" ">")
(displa-def mcomma dimension-nary  ", " 20. 20.)
(displa-def mabs   dim-mabs)

(defun dim-mabs (form result &aux arg bar)
  (setq arg (dimension (cadr form) nil 'mparen 'mparen nil 0))
  (cond ((or (> (f+ 2 width) linel) (and (= 1 height) (= 0 depth)))
	 (dimension-function form result))
	(t (setq width (f+ 2 width))
	   (update-heights height depth)
	   (setq bar `(d-vbar ,height ,depth ,(getcharn $absboxchar 2)))
	   (cons bar (nconc arg (cons bar result))))))

(displa-def $matrix dim-$matrix)

(defun dim-$matrix (form result)
  (prog (dmstr rstr cstr consp)
     (if (or (null (cdr form))
	     (not (memq 'simp (cdar form)))
	     (memalike '((mlist simp)) (cdr form))
	     (dolist (row (cdr form)) (if (not ($listp row)) (return t))))
	 (return (dimension-function form result)))
     (do ((l (cdadr form) (cdr l))) ((null l))
       (setq dmstr (cons nil dmstr) cstr (cons 0 cstr)))
     (do ((r (cdr form) (cdr r)) (h1 0) (d1 0))
	 ((or consp (null r))
	  (setq width 0)
	  (do ((cs cstr (cdr cs))) ((null cs)) (setq width (f+ 2 (car cs) width)))
	  (setq h1 (f1- (f+ h1 d1)) depth (// h1 2) height (f- h1 depth)))
       (declare (fixnum h1 d1))
       (do ((c (cdar r) (cdr c))
	    (nc dmstr (cdr nc))
	    (cs cstr (cdr cs)) (dummy) (h2 0) (d2 0))
	   ((null c) (setq d1 (f+ d1 h1 h2) h1 (f1+ d2)))
	 (declare (fixnum h2 d2))
	 (setq dummy (dimension (car c) nil 'mparen 'mparen nil 0)
	       h2 (max h2 height) d2 (max d2 depth))
	 (cond ((not (checkfit (f+ 14. width))) (setq consp t) (return nil))
	       (t (rplaca nc (cons (list* width height depth dummy) (car nc)))
		  (rplaca cs (max width (car cs))))))
       (setq rstr (cons d1 rstr)))
     (if (> (f+ height depth)
	    (linearray-dim)
	    )
	 (setq consp t))
     (return
       (cond ((and (not consp) (checkfit (f+ 2 width)))
	      (matout dmstr cstr rstr result))
	     ((and (not consp) (<= level 2)) (colout dmstr cstr result))
	     (t (dimension-function form result))))))

(defun matout (dmstr cstr rstr result)
  (push `(d-matrix left ,height ,depth) result)
  (push #\space result)
  (do ((d dmstr (cdr d)) (c cstr (cdr c)) (w 0 0)) ((null d))
    (declare (fixnum w))
    (do ((d (car d) (cdr d)) (r rstr (cdr r))) ((null d))
      (rplaca (cddar d) (f- height (car r)))
      (rplaca (cdar d) (f- (// (f- (car c) (caar d)) 2) w))
      (setq w (// (f+ (car c) (caar d)) 2))
      (rplaca d (cdar d)))
    (setq result (cons (list (f+ 2 (f- (car c) w)) 0) (nreconc (car d) result))))
  (setq width (f+ 2 width))
  (update-heights height depth)
  (rplaca (car result) (f1- (caar result)))
  (push `(d-matrix right ,height ,depth) result)
  result)

(defun colout (dmstr cstr result)
  (setq width 0 height 1 depth 0)
  (do ((r dmstr (cdr r)) (c cstr (cdr c)) (col 1 (f1+ col)) (w 0 0) (h -1 -1) (d 0))
      ((null r))
    (declare (fixnum col w h d))
    (push-string " Col " result)
    (setq result (nreconc (exploden col) result))
    (push-string " = " result)
    (setq width (f+ 8 (flatc col) width))
    (do ((r (car r) (cdr r))) ((null r))
      (setq h (f+ 1 h (cadar r) (caddar r)))
      (rplaca (cddar r) (f- h (cadar r)))
      (rplaca (cdar r) (f- (// (f- (car c) (caar r)) 2) w))
      (setq w (// (f+ (car c) (caar r)) 2))
      (rplaca r (cdar r)))
    (setq d (// h 2) h (f- h d))
    (push `(d-matrix left ,h ,d) result)
    (push #\space result)
    (push `(0 ,(f- d) . ,(nreverse (car r))) result)
    (push `(,(f1+ (f- (car c) w)) 0) result)
    (push `(d-matrix right ,h ,d) result)
    (setq width (f+ 4 (car c) width) height (max h height) depth (max d depth))
    (update-heights h d)
    (checkbreak result width))
  result)

(displa-def mbox dim-mbox)

(defun dim-mbox (form result &aux dummy)
  (setq dummy (dimension (cadr form) nil 'mparen 'mparen nil 0))
  (cond ((not (checkfit (f+ 2 width)))
	 (dimension-function (cons '($box) (cdr form)) result))
	(t (push `(d-box ,height ,depth ,width ,(nreverse dummy)) result)
	   (setq width (f+ 2 width) height (f1+ height) depth (f1+ depth))
	   (update-heights height depth)
	   result)))

(displa-def mlabox dim-mlabox)

(defun dim-mlabox (form result)
  (prog (dummy ch)
     (setq dummy (dimension (cadr form) nil 'mparen 'mparen nil 0))
     (cond ((not (checkfit (f+ 2 width)))
	    (return (dimension-function (cons '($box) (cdr form)) result))))
     (setq width (f+ 2 width) height (f1+ height) depth (f1+ depth))
     (setq ch (getcharn $boxchar 2))
     (setq result
	   (cons (do ((l (mapcar #'(lambda (l) (getcharn l 1))
				 (makstring (caddr form))) (cdr l))
		      (w 0) (nl))
		     ((or (null l) (= width w))
		      (cons 0 (cons (f1- height)
				    (cond ((< w width)
					   (cons `(d-hbar ,(f- width w) ,ch) nl))
					  (t nl)))))
		   (declare (fixnum w))
		   (setq nl (cons (car l) nl) w (f1+ w)))
		 result))
     (setq result (nconc dummy (list* `(d-vbar ,(f1- height) ,(f1- depth) ,ch)
				      (list (f- width) 0) result)))
     (setq result (cons (list (f- 1 width) (f- depth) `(d-hbar ,width ,ch)) result))
     (setq result (list* `(d-vbar ,(f1- height) ,(f1- depth) ,ch) '(-1 0) result))
     (update-heights height depth)
     (return result)))

(displa-def mtext dim-mtext 1 1)

(defun dim-mtext (form result)
  (if (null (cddr form)) (dimension (cadr form) result lop rop 0 0)
      (dimension-nary form result)))

(displa-def mlable dim-mlabel 0 0)

(defvar *display-labels-p* t)

(defun dim-mlabel (form result)
  (prog (dummy (w 0) (h 0) (d 0)) (declare (fixnum w h d))
	(cond ((eq nil (cadr form)) (setq w 0 h 0 d 0))
	      (mratp (setq result (append mratp (if *display-labels-p* (dimension-paren (cadr form) result)))
			   w (f+ 4 width) h height d depth))
	      (t (setq result (cons #\space (if *display-labels-p* (dimension-paren (cadr form) result)))
		       w (f1+ width) h height d depth)))
	(let ((level linel)) (checkbreak result w))
	(setq dummy (list 0 0))
	(setq result (dimension (caddr form) (cons dummy result) 'mlable rop w right))
	(cond ((and (not $leftjust) (= 0 bkptout))
	       (rplaca dummy (max 0 (f- (// (f- linel width) 2) w)))
	       (setq width (f+ (car dummy) width))))
	(setq width (f+ w width) height (max h height) depth (max d depth))
	(return result)))

(defprop mparen -1. lbp)
(defprop mparen -1. rbp)


(defun checkrat (form)
  (cond ((atom form) nil)
	((and (not (atom (car form))) (eq (caar form) 'mrat))
	 (if (memq 'trunc (cdar form)) '(#\space #.forward-slash-char #\T #.forward-slash-char)
	     '(#\space #.forward-slash-char #\R #.forward-slash-char)))
	((and (not (atom (car form))) (eq (caar form) 'mpois))
	 '(#\space #.forward-slash-char #\P #.forward-slash-char))
	(t (do ((l (cdr form) (cdr l))) ((null l))
	     (cond ((atom l)
		    (merror "~S has an atomic cdr - `display'" form))
		   ((setq form (checkrat (car l))) (return form)))))))




(defun checkfit (w)
  (declare (fixnum w))
  (or (not break) (<= (f- (f+ w break right 1) bkptwd) linel)))

(defun checkbreak (result w)
  (declare (fixnum w))
  (cond ((not break))
	((> (f- (setq w (f+ w break)) bkptout) linel)
	 (if (or (null bkpt) (eq result bkpt))
	     (merror "Expression is too wide to be displayed."))
	 (do ((l result (cdr l))) ((eq bkpt (cdr l)) (rplacd l nil))
	   (if (null l) (merror "`checkbreak' not found in `display'")))
	 (output bkpt 0) 
 	 #-franz (let ((#.ttyoff (or #.ttyoff more-^w))))

	 (setq lines (f1+ lines) bkpt result bkptout bkptwd bkptwd w
	       bkptht maxht bkptdp maxdp bkptlevel level maxht 1 maxdp 0))
	((or (null bkpt) (<= level bkptlevel) (> (// linel 2) (f- bkptwd bkptout)))
	 (setq bkpt result bkptwd w bkptlevel level
	       bkptht (max maxht bkptht) bkptdp (max maxdp bkptdp) maxht 1 maxdp 0))))

(defun forcebreak (result w)
  (output result 0)
  (setq lines (f+ 2 lines) bkpt nil bkptout (f+ w break) maxht 1 maxdp 0))

(defun update-heights (ht* dp*)
  (declare (fixnum ht* dp*))
  (if break (setq maxht (max maxht ht*) maxdp (max maxdp dp*))))

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

(defun output (result w)
  (declare (fixnum w))
;; The following is a hack to attempt to determine if we're on an
;; interactive terminal, in which case the user's hitting of the ENTER
;; key will have caused a newline to be displayed already.
#+(or gcl clisp) (cond ((not (equal *query-io* *standard-input*)) (fresh-line)))
#-(or gcl clisp) (cond ((not (interactive-stream-p *standard-input*)) (fresh-line)))

  ;;  (IF (AND (NOT (OR #.ttyoff MORE-^W))
  ;;	   SMART-TTY (NOT (AND SCROLLP (NOT $CURSORDISP)))
  ;;	   (< (f+ BKPTHT BKPTDP) (f1- TTYHEIGHT))
  ;;	   ;;If (STATUS TTY) is NIL, then we don't have the console.
  ;;	   ;#+PDP10 (STATUS TTY)
  ;;	   (> (f+ BKPTHT BKPTDP) (f- (f1- TTYHEIGHT) (CAR (CURSORPOS)))))
  ;;      (MORE-FUN T))
  (cond
    ;; If output is turned off to the console and no WRITEFILE is taking
    ;; place, then don't output anything.
    ((and (or #.ttyoff more-^w) (not #.writefilep)))
    ;; If the terminal can't do cursor movement, or we are writing 
    ;; to a WRITEFILE (#.writefilep is on) or the terminal is scrolling or
    ;; something else random, then draw equations line by line.
    ((> (f+ bkptht bkptdp) 80.)
     (merror "Expression is too high to be displayed."))
    ((or (not (and smart-tty $cursordisp))
	 #.writefilep scrollp (> (f+ bkptht bkptdp) (f- ttyheight 2)))
     (output-linear (nreverse result) w))
    ;; Otherwise, draw equations in the same order as they are dimensioned.
    (t (output-2d (nreverse result) w))))

;; Output function for terminals without cursor positioning capability.
;; Characters are drawn into LINEARRAY instead.  Each element of LINEARRAY is a
;; list -- the car is how many spaces to indent; the cdr is a list of
;; characters to draw.  After drawing into this array, lines are printed one at
;; a time.  This is used for printing terminals and when writing to files.
;; Block mode i/o isn't needed since PRINC is used instead of TYO and
;; CURSORPOS.

(defun output-linear (result w)
  (declare (fixnum w))
  (draw-linear result bkptdp w)
  (do (#+pdp10 (terpri t) (i (f1- (f+ bkptht bkptdp)) (f1- i)))
      ((< i 0))
    (declare (fixnum i))
    (cond ((null (linearray i)))
	  (more-^w (safe-print (output-linear-one-line i)))
	  (t (output-linear-one-line i)))))

;;#+NIL
;;(PROGN
;; (DEFPARAMETER *OLOL-BUF*
;;   (MAKE-STRING 512))
;; (DEFUN OUTPUT-LINEAR-ONE-LINE (I)
;;   (LET* ((LINE (NREVERSE (CDR (LINEARRAY I)))) (N (CAR LINE)))
;;     (SET-LINEARRAY I NIL)
;;     (TYOTBSP N)
;;     (LET ((B (OR *OLOL-BUF* (SETQ *OLOL-BUF* (MAKE-STRING 512))))
;;	   (*OLOL-BUF* NIL)
;;	   (I 0))
;;       (MAPC #'(LAMBDA (X) (SETF (SCHAR B I) (CODE-CHAR X)) (SETQ I (f1+ I)))
;;	     (CDR LINE))
;;       (OUSTR B NIL 0 I)
;;       (MTERPRI)
;;       NIL)))
;; ) ;#+NIL

#-nil
(defun output-linear-one-line (i) (declare (fixnum i))
       (prog (line (n 0))
	  (declare (fixnum n))
	  (setq line (linearray i) line (nreverse (cdr line)) n (car line))
	  (set-linearray i nil)
	  (tyotbsp n)
	  (loop for v in (cdr line) do (tyo v))
	  ;;    (PRINC (MAKNAM (CDR LINE)))
	  (mterpri)))

;; Move the cursor over N spaces to the left by outputting tabs and spaces.
;; This function assumes that the cursor is in the left margin when
;; it is called.  This is only called from OUTPUT-LINEAR, so it is
;; used only for printing terminals or for file output.

(defun tyotbsp (n)
  (declare (fixnum n))
  (do () ((< n (tablen))) (tyo #\tab) (decrement n (tablen)))
  (do () ((< n 1)) (tyo #\space) (decrement n)))

(defun draw-linear (dmstr oldrow oldcol)
  "This puts the LINE lists into LINEARRAY ready to be drawn.
   Each LINE consists of first an initial number of columns to space
   (or tab over) and then the characters to be printed.
   oldrow and oldcol are the starting points for the the (dx,dy) offsets
   given in the dimension string DMSTR.  It does not check that oldrow
   is big enough for possible negative y offsets in DMSTR, but BKPTDP is the
   right global to use for  oldrow (see Draw-2d)."
  (do ((line)) ((null dmstr))
    (cond ((atom (car dmstr))
	   (setq line (linearray oldrow))
	   (cond ((null line) (setq line (list oldcol)))
		 (t (prog (n)	#-cl (declare (fixnum n))
			  (setq n (car line) line (cdr line))
			  (do ((m (f+ (tablen) (f* (tablen) (// n (tablen))))
				  (f+ (tablen) m)))
			      ((not (< m oldcol))
			       (setq n (max n (f- m (tablen)))))
			    (declare (fixnum m))
			    (setq line (cons #\tab line)))
			  (do () ((<= oldcol n))
			    (push #\space line)
			    (increment n)))))
	   (do () ((or (null dmstr) (not (atom (car dmstr))))
		   (set-linearray oldrow (cons oldcol line)))
	     (increment oldcol)
	     (push (car dmstr) line)
	     (pop dmstr)))
	  ((integerp (caar dmstr))
	   ;; Why update OLDCOL and not OLDROW?  Should either update both
	   ;; (requiring multiple value return) or neither (analagous to lambda
	   ;; binding).
	   (setq oldcol (draw-linear (reverse (cddar dmstr))
				     (f+ oldrow (cadar dmstr))
				     (f+ oldcol (caar dmstr))))
	   (pop dmstr))
	  (t (setq oldcol (apply (caar dmstr) t (cdar dmstr)))
	     (pop dmstr))))
  ;; Be sure to return this.
  oldcol)

;; Output function for terminals with cursor positioning capability.  Draws
;; equations in the order they are dimensioned.  To be efficient, it does block
;; mode i/o into a stream called DISPLAY-FILE, set up in ALJABR;LOADER.
;; This function is not used if a WRITEFILE is taking place.

;; TTY interrupts are turned off for some reason, probably to protect global
;; state.  

;; Bug in COMPLR necessitates binding H to 0 initially.
;; (PROG (H) (DECLARE (FIXNUM H)) ...) doesn't try binding it to NIL as
;; this does.

;;#+ITS
;;(DEFUN OUTPUT-2D (RESULT W &AUX (H 0))
;; (DECLARE (FIXNUM W H CH))
;; (UNWIND-PROTECT
;;  (PROGN (TTYINTSOFF)
;;	 (SETQ OLDROW (CAR (CURSORPOS)) OLDCOL 0 H (f+ OLDROW BKPTHT BKPTDP))
;;	 ;; Move the cursor to the left edge of the screen.
;;	 (CURSORPOS* OLDROW 0)
;;	 ;; Then print CRLFs from the top of the expression to the bottom.
;;	 ;; The purpose of this is to clear the appropriate section of the
;;	 ;; screen.  If RUBOUT-TTY is NIL (i.e. we are using a storage tube
;;	 ;; display), then only print LFs since the entire screen is cleared
;;	 ;; anyway.  %TDCRL = carriage return, line feed.  %TDLF = line feed.
;;	 (DO ((CH (IF RUBOUT-TTY #.%TDCRL #.%TDLF))) ((= H OLDROW))
;;	     (TYO* CH) (INCREMENT OLDROW))
;;	 (DRAW-2D RESULT (f- OLDROW BKPTDP 1) W)
;;	 ;; Why is this necessary?  Presumably, we never go off the bottom
;;	 ;; of the screen.
;;	 (SETQ H (MIN (f- TTYHEIGHT 2) H))
;;	 ;; Leave the cursor at the bottom of the expression.
;;	 (CURSORPOS* H 0)
;;	 ;; Output is buffered for efficiency.
;;	 (FORCE-OUTPUT DISPLAY-FILE)
;;	 ;; Let ITS know where the cursor is now.  This does not do
;;	 ;; cursor movement.  :CALL SCPOS for information.
;;	 (SETCURSORPOS H 0)
;;	 ;; Gobble any characters the poor user may have typed during display.
;;	 (LISTEN))
;;  (TTYINTSON))
;; (NOINTERRUPT NIL))

;; I/O is much simpler on the Lisp Machine.

(defun output-2d (result w &aux (h 0))
  (declare (fixnum w h ))
  (setq oldrow (car (cursorpos)) oldcol 0 h (f+ oldrow bkptht bkptdp))
  (cursorpos* oldrow 0)
  ;; Move the cursor vertically until we are at the bottom line of the
  ;; new expression.
  (do () ((= h oldrow)) (tyo* #\newline) (increment oldrow))
  (draw-2d result (f- oldrow bkptdp 1) w)
  (cursorpos* (setq h (min (f- ttyheight 2) h)) 0))

;; For now, cursor movement is only available on ITS and the Lisp
;; Machine.  But define this to catch possible errors.

;;#-(OR CL ITS NIL)
;;(DEFUN OUTPUT-2D (RESULT W)
;;       RESULT W ;Ignored.
;;       (MERROR "`output-2d' called on system without display support."))

;;see +lispm below for cl fix.
(defun draw-2d (dmstr row col)
  (declare (fixnum row col))
  (cursorpos* row col)
  (do ((l dmstr)) ((null l))
    (cond ((integerp (car l)) (tyo* (car l)) (pop l))
	  ((integerp (caar l))
	   (setq col oldcol)
	   (do ()
	       ((or (integerp (car l)) (not (integerp (caar l)))))
	     (cond
	       ((null (cddar l)) (setq col (f+ col (caar l))))
	       (t (draw-2d (reverse (cddar l))
			   (f-  row (cadar l)) (f+ col (caar l)))
		  (setq col oldcol)))
	     (pop l))
	   (cursorpos* row col))
	  (t (apply (caar l) nil (cdar l))
	     (pop l)))))

(defun check-dimstring (str)
  (loop for v in str when (integerp v) do (error "bad entry ~A" v)))

;;#+lispm
;;(DEFUN DRAW-2D (DMSTR ROW COL)
;;  "This bypasses draw-linear and draws the Dimension-string directly
;;   using cursor positioning.  It won't work for files or the editor"
;; #-cl (DECLARE (FIXNUM ROW COL))
;;  (CURSORPOS* ROW COL)
;;    (DO ((LL DMSTR)) ((NULL LL))
;;     ;;should not have a integerp (car ll) but somewhere #\space is getting 32.
;;      (COND ((or (integerp (car ll))
;;		 (characterp (CAR LL)))
;;	     (TYO* (CAR LL)) (POP LL))
;;	    ((integerp (CAAR LL))
;;	     (SETQ COL OLDCOL)
;;	     (DO ()
;;	       ((OR (CHARACTERP (CAR LL))  (NOT (integerp (CAAR LL)))))
;;	       (cond
;;		((NULL (CDDAR LL)) (SETQ COL (f+ COL (CAAR LL))))
;;		(t (DRAW-2D (REVERSE (CDDAR LL))
;;			    (f-  ROW (CADAR LL)) (f+ COL (CAAR LL)))
;;		   (SETQ COL OLDCOL)))
;;	       (POP LL))
;;	     (CURSORPOS* ROW COL)
;;	     )
;;	    (T (APPLY (CAAR LL) NIL (CDAR LL))
;;	       (POP LL)))))

;;#-(OR CL ITS NIL)
;;(DEFUN DRAW-2D (DMSTR ROW COL)
;;       DMSTR ROW COL ;Ignored.
;;       (MERROR "`draw-2d' called on system without display support."))

;; Crude line graphics.  The interface to a graphics device is via the
;; functions LG-SET-POINT, LG-DRAW-VECTOR, LG-END-VECTOR and via the
;; LG-CHARACTER specials.  LG-END-VECTOR is needed since many consoles
;; (including those supporting ARDS protocol) must "exit" graphics mode.
;; LG-CHARACTER-X and LG-CHARACTER-Y give the width and height of a character
;; in pixels, and the -2 variables are simply those numbers divided by 2.  LG
;; stands for "Line Graphics".  See MAXSRC;ARDS for a sample ctl.

;;(declare-top ;(*EXPR LG-SET-POINT LG-DRAW-VECTOR LG-END-VECTOR)
;; (NOTYPE (LG-SET-POINT FIXNUM FIXNUM)
;;	 (LG-DRAW-VECTOR FIXNUM FIXNUM)
;;	 (LG-END-VECTOR FIXNUM FIXNUM))
;; (SPECIAL LG-CHARACTER-X LG-CHARACTER-X-2
;;	  LG-CHARACTER-Y LG-CHARACTER-Y-2))

;; Make this work in the new window system at some point.

;;#+LISPM
;;(PROGN 'COMPILE

;;(declare-top (SPECIAL LG-OLD-X LG-OLD-Y))

;;(DEFUN LG-SET-POINT (X Y)
;;  (SETQ LG-OLD-X (f- X 1) LG-OLD-Y (f- Y 2)))

;;(DEFUN LG-DRAW-VECTOR (X Y)
;;  (SETQ X (f- X 1) Y (f- Y 2))
;;  (FUNCALL *standard-output* ':DRAW-LINE LG-OLD-X LG-OLD-Y X Y)
;;  (WHEN (> LG-CHARACTER-Y 20)
;;	(LET ((DELTA-X (f- X LG-OLD-X))
;;	      (DELTA-Y (f- Y LG-OLD-Y)))
;;	  (IF (> (ABS DELTA-X) (ABS DELTA-Y))
;;	      (FUNCALL *standard-output* ':DRAW-LINE LG-OLD-X (f1- LG-OLD-Y) X (f1- Y))
;;	      (FUNCALL *standard-output* ':DRAW-LINE (f1- LG-OLD-X) LG-OLD-Y (f1- X) Y))))
;;  (SETQ LG-OLD-X X LG-OLD-Y Y))

;;; Set these so that DISPLA can be called from top-level.  The size
;;; of TERMINAL-IO is wired in here.
;;; These should be bound at time of call to DISPLA.

;;(SETQ LG-CHARACTER-X (FUNCALL tv:main-screen ':CHAR-WIDTH))
;;(SETQ LG-CHARACTER-Y (FUNCALL tv:main-screen ':LINE-HEIGHT))

;;(SETQ LG-CHARACTER-X-2 (// LG-CHARACTER-X 2))
;;(SETQ LG-CHARACTER-Y-2 (// LG-CHARACTER-Y 2))

;;) ;; End of Lispm Graphics definitions.

;; Even cruder character graphics.  Interface to the ctl is via functions
;; which draw lines and corners.  CG means "Character Graphics".  See
;; MAXSRC;VT100 for a sample ctl.  Note that these functions do not modify
;; the values of OLDROW and OLDCOL.

;;(declare-top (*EXPR CG-BEGIN-GRAPHICS CG-END-GRAPHICS
;;		CG-UL-CORNER CG-UR-CORNER CG-LL-CORNER CG-LR-CORNER
;;		CG-VERTICAL-BAR CG-HORIZONTAL-BAR
;;		CG-D-SUMSIGN CG-D-PRODSIGN))

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

(defun d-hbar (linear? w &optional (char #\-) &aux nl)
  (declare (fixnum w ))
  (cond (linear? (dotimes (i w) (push char nl))
		 (draw-linear nl oldrow oldcol))
	;;	((AND LINE-GRAPHICS-TTY $LINEDISP)
	;;	 (LET ((GY (f+ (f* LG-CHARACTER-Y OLDROW) LG-CHARACTER-Y-2)))
	;;	      (DECLARE (FIXNUM gy))
	;;	      (LG-SET-POINT  (f* OLDCOL LG-CHARACTER-X) GY)
	;;	      (LG-END-VECTOR (f* (f+ OLDCOL W) LG-CHARACTER-X) GY))
	;;	 (CURSORPOS* OLDROW (f+ OLDCOL W)))
	;;       ((AND CHARACTER-GRAPHICS-TTY $LINEDISP)
	;;	 (CG-BEGIN-GRAPHICS)
	;;	 (DOTIMES (I W) (CG-HORIZONTAL-BAR))
	;;	 (INCREMENT OLDCOL W)
	;;	 (CG-END-GRAPHICS))
	(t (dotimes (i w) (tyo* char)))))

;; Notice that in all of the height computations, an offset of 2 is added or
;; subtracted to the y-dimension.  This is to get the lines to fit within the
;; character cell precisely and not get clipped when moving things around in
;; the equation editor.

(defun d-vbar (linear? h d &optional (char vertical-stroke-char))
  (declare (fixnum h d ))
  (cond (linear? (setq d (f- d))
		 (do ((i (f- h 2) (f1- i))
		      (nl `((0 ,(f1- h) ,char))))
		     ((< i d) (draw-linear (nreverse nl) oldrow oldcol))
		   (push `(-1 ,i ,char) nl)))
	;;	((AND LINE-GRAPHICS-TTY $LINEDISP)
	;;	 (LET ((GX (f+ (f* LG-CHARACTER-X OLDCOL) LG-CHARACTER-X-2)))
	;;	      (DECLARE (FIXNUM GX ))
	;;	      (LG-SET-POINT  GX (f- (f* (f+ OLDROW D 1) LG-CHARACTER-Y) 2))
	;;	      (LG-END-VECTOR GX (f+ (f* (f+ OLDROW 1 (f- H)) LG-CHARACTER-Y) 2)))
	;;	 (CURSORPOS* OLDROW (f1+ OLDCOL)))
	;;	((AND CHARACTER-GRAPHICS-TTY $LINEDISP)
	;;	 (CURSORPOS* (f+ OLDROW 1 (f- H)) OLDCOL)
	;;	 (CG-BEGIN-GRAPHICS)
	;;	 (CG-VERTICAL-BAR)
	;;	 (DOTIMES (I (f+ H D -1))
	;;		  (CURSORPOS* (f1+ OLDROW) OLDCOL)
	;;		  (CG-VERTICAL-BAR))
	;;	 (CG-END-GRAPHICS)
	;;	 (CURSORPOS* (f- OLDROW D) (f1+ OLDCOL)))
	(t (cursorpos* (f+ oldrow 1 (f- h)) oldcol)
	   (tyo* char)
	   (dotimes (i (f+ h d -1))
	     (cursorpos* (f1+ oldrow) (f1- oldcol))
	     (tyo* char))
	   (cursorpos* (f- oldrow d) oldcol))))

(defun d-integralsign (linear? &aux dmstr)
  (cond		     ;((AND (NOT LINEAR?) LINE-GRAPHICS-TTY $LINEDISP)
    ;;	 (LET ((X-MIN (f* LG-CHARACTER-X OLDCOL))
    ;;	       (X-1   (f1- LG-CHARACTER-X-2))
    ;;	       (X-2   LG-CHARACTER-X-2)
    ;;	       (X-MAX (f* LG-CHARACTER-X (f1+ OLDCOL)))
    ;;	       (Y-MIN (f+ (f* LG-CHARACTER-Y (f- OLDROW 2)) LG-CHARACTER-Y-2))
    ;;	       (Y-1   LG-CHARACTER-Y-2)
    ;;	       (Y-2   (f+ LG-CHARACTER-Y LG-CHARACTER-Y-2))
    ;;	       (Y-MAX (f+ (f* LG-CHARACTER-Y (f+ OLDROW 2)) LG-CHARACTER-Y-2)))
    ;;	      (DECLARE (FIXNUM X-MIN X-1 X-2 X-MAX Y-MIN Y-1 Y-2 Y-MAX))
    ;;	    (DOLIST (X '(0 -1))
    ;;	       (LG-SET-POINT   (f+ X X-MAX) Y-MIN)
    ;;	       (LG-DRAW-VECTOR (f+ X X-MAX (f- X-1)) (f+ Y-MIN Y-1))
    ;;	       (LG-DRAW-VECTOR (f+ X X-MAX (f- X-2)) (f+ Y-MIN Y-2))
    ;;	       (LG-DRAW-VECTOR (f+ X X-MIN X-2)	   (f- Y-MAX Y-2))
    ;;	       (LG-DRAW-VECTOR (f+ X X-MIN X-1)	   (f- Y-MAX Y-1))
    ;;	       (LG-END-VECTOR  (f+ X X-MIN)	   Y-MAX)))
    ;;	 (CURSORPOS* OLDROW (f1+ OLDCOL)))
    (t (setq dmstr
	     `((0 2 #.forward-slash-char) (-1 1 #\[) (-1 0 #\I) (-1 -1 #\]) (-1 -2 #.forward-slash-char)))
       (if linear?
	   (draw-linear dmstr oldrow oldcol)
	   (draw-2d	    dmstr oldrow oldcol)))))

(defun d-prodsign (linear? &aux dmstr)
  (cond	      ;((AND (NOT LINEAR?) $LINEDISP (FBOUNDP 'CG-D-PRODSIGN))
    ;;	 (CG-BEGIN-GRAPHICS)
    ;;	 (CG-D-PRODSIGN)
    ;;	 (CG-END-GRAPHICS)
    ;;	 (INCREMENT OLDCOL 5))
    (t (setq dmstr '((0 2 #.back-slash-char (d-hbar 3 #\=) #.forward-slash-char)
		     (-4 0) (d-vbar 2 1 #\!) #\space (d-vbar 2 1 #\!) (1 0)))
       (if linear?
	   (draw-linear dmstr oldrow oldcol)
	   (draw-2d dmstr oldrow oldcol)))))

(defun d-sumsign (linear? &aux dmstr)
  (cond		     ;((AND (NOT LINEAR?) $LINEDISP LINE-GRAPHICS-TTY)
    ;;	 (LET ((X-MIN  (f* LG-CHARACTER-X OLDCOL))
    ;;	       (X-HALF (f* LG-CHARACTER-X (f+ OLDCOL 2)))
    ;;	       (X-MAX  (f* LG-CHARACTER-X (f+ OLDCOL 4)))
    ;;	       (Y-MIN  (f+ (f* LG-CHARACTER-Y (f- OLDROW 2)) LG-CHARACTER-Y-2))
    ;; 	       (Y-HALF (f+ (f* LG-CHARACTER-Y OLDROW) LG-CHARACTER-Y-2))
    ;;	       (Y-MAX  (f+ (f* LG-CHARACTER-Y (f+ OLDROW 2))
    ;;			   LG-CHARACTER-Y-2)))
    ;;	      (DECLARE (FIXNUM X-MIN X-HALF X-MAX Y-MIN Y-HALF Y-MAX))

    ;;	      (LG-SET-POINT (f+ X-MAX 4) (f+ Y-MIN 6))
    ;;	      (MAPC #'(LAMBDA (X) (LG-DRAW-VECTOR (CAR X) (CDR X)))
    ;;		    `((,X-MAX . ,Y-MIN)
    ;;		      (,(f1+ X-MIN)  . ,Y-MIN)
    ;;		      (,(f1+ X-HALF) . ,Y-HALF)
    ;;		      (,(f1+ X-MIN)  . ,Y-MAX)
    ;;		      (,X-MIN	    . ,Y-MAX)
    ;;		      (,X-HALF	    . ,Y-HALF)
    ;;		      (,X-MIN	    . ,Y-MIN)
    ;;		      (,(f1- X-MIN)  . ,Y-MIN)
    ;;		      (,(f1- X-HALF) . ,Y-HALF)))
    ;;	      (LG-SET-POINT (f+ X-MAX 4) (f- Y-MAX 6))
    ;;	      (LG-DRAW-VECTOR X-MAX Y-MAX)
    ;;	      (LG-DRAW-VECTOR X-MIN Y-MAX)
    ;;	      (LG-DRAW-VECTOR X-MIN (f1- Y-MAX))
    ;;	      (LG-END-VECTOR X-MAX (f1- Y-MAX)))
    ;;	 (CURSORPOS* OLDROW (f+ OLDCOL 4)))
    ;;	((AND (NOT LINEAR?) $LINEDISP (FBOUNDP 'CG-D-SUMSIGN))
    ;;	 (CG-BEGIN-GRAPHICS)
    ;;	 (CG-D-SUMSIGN)
    ;;	 (CG-END-GRAPHICS)
    ;;	 (INCREMENT OLDCOL 4))
    (t
     (setq dmstr '((0 2 (d-hbar 4 #\=))
		   (-4 1 #.back-slash-char) #\> (-2 -1 #.forward-slash-char)
		   (-1 -2 (d-hbar 4 #\=))))
     (if linear?
	 (draw-linear dmstr oldrow oldcol)
	 (draw-2d dmstr oldrow oldcol)))))

;; Notice how this calls D-VBAR in the non-graphic case.  The entire output
;; side should be structured this way, with no consing of intermediate
;; dimension strings.

(defun d-matrix (linear? direction h d)
  (declare (fixnum h d))
  (cond		     ;((AND (NOT LINEAR?) LINE-GRAPHICS-TTY $LINEDISP)
    ;;	 (LET ((X-MIN (f1+ (f* LG-CHARACTER-X OLDCOL)))
    ;;	       (X-MAX (f1- (f* LG-CHARACTER-X (f1+ OLDCOL))))
    ;;	       (Y-MIN (f+ (f* LG-CHARACTER-Y (f+ OLDROW 1 (f- H))) 2))
    ;;	       (Y-MAX (f- (f* LG-CHARACTER-Y (f+ OLDROW 1 D)) 2)))
    ;;	      (declare (fixnum  X-MIN X-MAX Y-MIN Y-MAX))
    ;;	      (IF (EQ DIRECTION 'RIGHT) (PSETQ X-MIN X-MAX X-MAX X-MIN))
    ;;	      (LG-SET-POINT   X-MAX Y-MIN)
    ;;	      (LG-DRAW-VECTOR X-MIN Y-MIN)
    ;;	      (LG-DRAW-VECTOR X-MIN Y-MAX)
    ;;	      (LG-END-VECTOR  X-MAX Y-MAX))
    ;;	 (CURSORPOS* OLDROW (f1+ OLDCOL)))
    ;;	((AND (NOT LINEAR?) CHARACTER-GRAPHICS-TTY $LINEDISP)
    ;;	 (COND ((= (f+ H D) 1)
    ;;		(TYO* (GETCHARN (IF (EQ DIRECTION 'RIGHT) $RMXCHAR $LMXCHAR)
    ;;				2)))
    ;;	       (T (CURSORPOS* (f+ OLDROW 1 (f- H)) OLDCOL)
    ;;		  (CG-BEGIN-GRAPHICS)
    ;;		  (IF (EQ DIRECTION 'RIGHT) (CG-UR-CORNER) (CG-UL-CORNER))
    ;;		  (CG-END-GRAPHICS)
    ;;		  (CURSORPOS* (f+ OLDROW -1 H) OLDCOL)
    ;;		  (COND ((> (f+ H D) 2)
    ;;			 (D-VBAR NIL (f1- H) (f1- D))
    ;;			 (CURSORPOS* (f+ OLDROW D) (f1- OLDCOL)))
    ;;			(T (CURSORPOS* (f+ OLDROW D) OLDCOL)))
    ;;		  (CG-BEGIN-GRAPHICS)
    ;;		  (IF (EQ DIRECTION 'RIGHT) (CG-LR-CORNER) (CG-LL-CORNER))
    ;;		  (CG-END-GRAPHICS)
    ;;		  (CURSORPOS* (f- OLDROW D) (f1+ OLDCOL)))))
    (t (d-vbar linear? h d 
	       (getcharn (if (eq direction 'right) $rmxchar $lmxchar) 2)))))

;; There is wired knowledge of character offsets here.

(defun d-box (linear? h d w body &aux (char 0) dmstr)
					;char a char?
  (declare (fixnum h d w ))
  (cond		     ;((AND (NOT LINEAR?) LINE-GRAPHICS-TTY $LINEDISP)
    ;;	 (LET ((X-MIN (f* LG-CHARACTER-X OLDCOL))
    ;;	       (X-MAX (f* LG-CHARACTER-X (f+ OLDCOL W 2)))
    ;;	       (Y-MIN (f+ (f* LG-CHARACTER-Y (f- OLDROW H)) 2))
    ;;	       (Y-MAX (f- (f* LG-CHARACTER-Y (f+ OLDROW D 2)) 2)))
    ;;	      (declare (fixnum X-MIN X-MAX Y-MIN Y-MAX))
    ;;	      (LG-SET-POINT X-MIN Y-MIN)
    ;;	      (LG-DRAW-VECTOR X-MAX Y-MIN)
    ;;	      (LG-DRAW-VECTOR X-MAX Y-MAX)
    ;;	      (LG-DRAW-VECTOR X-MIN Y-MAX)
    ;;	      (LG-END-VECTOR  X-MIN Y-MIN))
    ;;	 (CURSORPOS* OLDROW (f1+ OLDCOL))
    ;;	 (DRAW-2D BODY OLDROW OLDCOL)
    ;;	 (CURSORPOS* OLDROW (f+ OLDCOL 1)))
    ;;	((AND (NOT LINEAR?) CHARACTER-GRAPHICS-TTY $LINEDISP)
    ;;	 (D-MATRIX NIL 'LEFT (f1+ H) (f1+ D))
    ;;	 (CURSORPOS* (f- OLDROW H) OLDCOL)
    ;;	 (D-HBAR NIL W)
    ;;	 (CURSORPOS* (f+ OLDROW H) (f- OLDCOL W))
    ;;	 (DRAW-2D BODY OLDROW OLDCOL)
    ;;	 (CURSORPOS* (f+ OLDROW D 1) (f- OLDCOL W))
    ;;	 (D-HBAR NIL W)
    ;;	 (CURSORPOS* (f- OLDROW D 1) OLDCOL)
    ;;	 (D-MATRIX NIL 'RIGHT (f1+ H) (f1+ D)))
    (t (setq char (getcharn $boxchar 2))
       (setq dmstr
	     `((0 ,h (d-hbar ,(f+ 2 w) ,char))
	       (,(f- (f+ w 2)) 0)
	       (d-vbar ,h ,d ,char)
	       ,@body
	       (,(f- (f1+ w)) ,(f- (f1+ d)) (d-hbar ,(f+ w 2) ,char))
	       (-1 0)
	       (d-vbar ,h ,d ,char)))
       (if linear?
	   (draw-linear dmstr oldrow oldcol)
	   (draw-2d dmstr oldrow oldcol)))))

;; Primitive functions for doing equation drawing.

;; Position the cursor at a given place on the screen.  %TDMV0 does
;; absolute cursor movement.

;;#+ITS
;;(DEFUN CURSORPOS* (ROW COL)
;;  (DECLARE (FIXNUM ROW COL))
;;  (+TYO #.%TDMV0 DISPLAY-FILE)
;;  (+TYO ROW DISPLAY-FILE)
;;  (+TYO COL DISPLAY-FILE)
;;  (SETQ OLDROW ROW OLDCOL COL))

#-its
(defun cursorpos* (row col)
  (declare (fixnum row col))
  (cursorpos row col)
  (setq oldrow row oldcol col))

;; This function is transmitting ITS output buffer codes in addition to
;; standard ascii characters.  See INFO;ITSTTY > for documentation.  This
;; should convert tabs to direct cursor positioning commands since otherwise
;; they get stuffed down the raw stream and appear as gammas on sail consoles
;; and lose completely on terminals which can't tab.  Backspace also loses,
;; but its nearly impossible to get a string with backspace in it in Macsyma.
;; Also, DISPLA can't dimension it correctly.

(defun tyo* (char)
  (cond ((char= #\backspace char)
	 (decf oldcol))			;Backspace
	((char< char #.(code-char 128))
	 (incf oldcol)))		;Printing graphic
  (tyo char))

;;#-(or ITS NIL cl)
;;(DEFUN TYO* (CHAR)
;;  (DECLARE (FIXNUM CHAR))
;;  (IF (< CHAR 128.) (SETQ OLDCOL (f1+ OLDCOL)))	;Printing graphic
;;  (TYO CHAR))

;; Functions used by the packages for doing character graphics.
;; See MAXSRC;H19 or VT100.

;;#+ITS (PROGN 'COMPILE

;;(DEFMFUN CG-TYO (CHAR) (+TYO CHAR DISPLAY-FILE))

;;; ITS does not change its idea of where the cursor position is when characters
;;; are slipped by it using %TDQOT.  This is used for operations which just
;;; change the state of the terminal without moving the cursor.  For actually
;;; drawing characters, we use ordinary tyo since the cursor does indeed get
;;; moved forward a position.  Fortunately, it only takes one character to draw
;;; each of the special characters.

;;(DEFMFUN CG-IMAGE-TYO (CHAR)
;;	 (CG-TYO #.%TDQOT)
;;	 (CG-TYO CHAR))

;;) ;; End of conditional

;;#+NIL (progn
;;(defmfun cg-tyo (char-code)
;;  (send *terminal-io* :write-char (code-char char-code)))

;;(defmfun cg-image-tyo (char-code)
;;  (send *terminal-io* :write-raw-char (code-char char-code)))

;;Moving the stuff to a buffer to ensure that it will get output in one blast
;; is worth it on the vax...
;;(defvar *cg-tyo-list-buf*
;;  nil)

;;(defun cg-tyo-list (l imagep &aux (msg (if imagep :raw-oustr :oustr)))
;;  (prog ((b (or *cg-tyo-list-buf* (setq *cg-tyo-list-buf* (make-string 20))))
;;	 (*cg-tyo-list-buf* nil)
;;	 (i 0))
;;   a	(cond ((or (null l) (=& i 20))
;;	         (send *terminal-io* msg b 0 i)
;;		 (if (null l) (return nil) (setq i 0)))
;;	      (t (setf (schar b i) (code-char (car l)))
;;		 (setq l (cdr l) i (1+& i))
;;		 (go a)))))

;;) ;; End of conditional

(progn 'compile

       (defmfun cg-tyo (char)		;Surely you jest. `(TYO ,CHAR)
	 (tyo char))
       
       (defmfun cg-image-tyo (char)	;Ditto. `(TYO ,CHAR)
	 (tyo char)))


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


(defmfun cg-tyo-n (l)
  (mapc #'cg-tyo l))

(defmfun cg-image-tyo-n (l)
  (mapc #'cg-image-tyo l))

