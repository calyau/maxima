;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;;; Low level line drawing routines for the MACSYMA plot package on the LISP machine
;;; THIS NEEDS TO BE FLUSHED SOMEDAY

;;; Todo:
;;;   correct pointi
;;;   finish $PLOT_COMMAND and related functions
;;;   see how PLOTMODE should affect the scaling
;;;   see if the clipping in VECTOR can be replaced by the standard methods
;;;   figure out what pnt-status does and see if it is necessary
;;;   replace the gross exploden, stripdollar stufff with something more reasonable
;;;   flush crufty declare specials

(declare-top (special plot-last-hi-y plot-last-lo-y plot-last-hi-x plot-mode
		  print-mode $loadprint $values $myoptions plot-opts
		  plot-vals char-type $charratio $plotmode dasharray
		  print-dasharray print-symbolarray symbolarray
		  dashl odashl beamon drawn print-line1 char-width char-height
		  min-x max-x min-y max-y last-x last-y size-x size-y pnt-status
		  screen-last-x screen-last-y DISPLAY-MODE))

;;; Macsyma Plot Frames are defined in LMMAX;PLTWIN >

;;; The default plotting stream is a plot frame.

#+lispm
(DEFVAR PLOT-STREAM (MAKE-PLOT-WINDOW-STREAM))   


#+LISPM
(DEFVAR PLOT-FONT FONTS:CPTFONT)		;maybe Timesroman would be better?

;;; *** This gets redone when the change options feature gets implemented
(defun plot-startup nil
   (setq plot-opts '($clear $wait $plotbell)
	 plot-vals '(t t t nil))
   (mapcar #'(lambda (plotvar plotval) 
		     (cond ((boundp plotvar)
			    (and (memq plotvar $values)
				 (set plotvar (prog2 nil (eval plotvar)
						     (remvalue plotvar 'PLOT-STARTUP)
						     (add2lnc plotvar $myoptions)))))
			   (t (set plotvar plotval))))

	   ;; these guys aren't reset by PLOTRESET
	   (append plot-opts '($plotthick $plotscale $charratio $plotlftmar $plotbotmar))
	   (append plot-vals '(2. 1.75 2.5 150. 150.)))
   t)

(plot-startup)

;;; **** This should also change the x and y relative scales???

(DEFVAR $PLOTSCALE 1.0)

(defmspec $plotmode (l) (setq l (cdr l))
  (SETQ $PLOTMODE NIL DISPLAY-MODE NIL)
  (IF (memq '$DISPLAY l)
      #+lispm
      (setq char-height (font-char-height plot-font)
	    char-width (font-char-width plot-font)
	    DISPLAY-MODE T)
      #-lispm nil
      )
  (IF (memq '$PAPER l)
      (setq char-height (fix (+$ 0.5 (//$ 25.0 (float $plotscale))))
	    char-width (fix (+$ 0.5 (//$ 16.0 (float $plotscale))))))
  (setq $plotmode `((mlist) ,@ l)))

(meval '(($plotmode) $DISPLAY))

;;; Enter and exit graphics mode.  Process commands at the end of plotting.

(declare-top (special graphic-mode $clear $wait
		  $plotbell $plotthick $plotscale
		  $plotheight $plotlftmar $plotbotmar))

(setq graphic-mode nil)
(defvar $plotnum 20)

(defun $entergraph nil 
   (if graphic-mode (let (($wait)) ($exitgraph)))
   (setq graphic-mode t)
   (if $clear (send plot-stream ':clear-screen))
   (send plot-stream ':init-for-plotting)
   (SETQ screen-last-x 0)
   (SETQ screen-last-y 0)
   T)

(defun $exitgraph (&AUX (CMD -1))
  (SETQ GRAPHIC-MODE NIL)
  (if (AND $WAIT DISPLAY-MODE)
      (progn
       ;	(if $plotbell (FUNCALL TERMINAL-IO ':BEEP))
	(SETQ CMD (FUNCALL PLOT-STREAM #+Lispm ':any-TYI #-lispm :tyio))))
  (send plot-stream ':end-plotting)
  CMD)

(DEFUN GET-PLOTTING-RANGE () (list 0 0 1000 1000)
  #+lispm (FUNCALL PLOT-STREAM ':GET-PLOTTING-RANGE))

(DEFUN $INITGRAPH () (FUNCALL PLOT-STREAM ':INIT-FOR-PLOTTING))

(DEFUN $ENDGRAPH () (FUNCALL PLOT-STREAM ':END-PLOTTING))

#+lispm
(progn
(defun $clear () (FUNCALL PLOT-STREAM ':CLEAR-SCREEN))



(defun $hardcopy () (send plot-stream ':hardcopy) '$done)






(defun macsyma-print (v &optional stream) (aformat stream "~:M" v))

(remprop :macsyma-expression 'tv:choose-variable-values-keyword)

(DEFPROP :macsyma-expression
	 (macsyma-print mread-noprompt nil
			nil nil 
			"Click left to input a new macsyma expression from keyboard,middle to edit current value. TERMINATE input with ;"
	 tv::choose-variable-values-keyword))



  
(define-user-option-alist *Original-plot-3d-options* Def-plt-3d1)
(defmacro def-plt-3d (var val type description)
  `(def-plt-3d1 ,var ,val ,type ,(zl-string description)))
 
(def-plt-3d *xlow* -2 :number "Plot from x")
(def-plt-3d *xhigh* 2 :number "Plot to x")
(def-plt-3d *ylow* -2 :number "Plot from y")
(def-plt-3d *yhigh* 2 :number "Plot to y")
(def-plt-3d  *expression-to-plot* nil :macsyma-expression
	     "Expression in x and y or lisp function to plot")
(def-plt-3d $plotscale 1.0 :number "Plot Scale")
;(def-plt-3d $plotnum (meval* '$plotnum) :macsyma-expression "Number of pts on x axis used") 
(def-plt-3d $plotnum0 (meval* '$plotnum0) :macsyma-expression "Number of pts on x axis used")
(def-plt-3d $plotnum1 (meval* '$plotnum1) :macsyma-expression "Number of pts on y axis used")
(DEF-PLT-3d $window (meval* '$WINDOW1) :macsyma-expression "Plot window")
(DEF-plt-3d  $centerplot (meval* '$centerplot1) :Macsyma-expression "Centerplot")
(DEF-PLT-3d $viewpt (meval* '$viewpt1) :macsyma-expression "View point")
(def-plt-3d three-d (meval* '$hide) :macsyma-expression "Hide or contour")
)


(defvar *function-to-plot*)

; ;
;;
;;(defmacro compile-define-function (x-y-expr function-name  &aux varl)
;;  `(progn
;;;  (setq x-y-expr (list 'quote (eval ,x-y-expr)))	
;;  (setq varl ($list_variables ,x-y-expr))
;;	       (meval*
;;		 '((MDEFINE )
;;		   ((,function-name) ,@ (cdr varl))
;;		   ((MPROGN) (($MODEDECLARE) ,varl $float)
;;		     x-y-expr)))
;;	       (mfuncall '$compile ',function-name)
;;	       ',function-name))
;;



;;(defmacro compile-define-function (x-y-expr function-name &aux varl)
;;;    (setq x-y-expr (eval x-y-expr))
;;  (setq  varl ($list_variables x-y-expr))
;;  `(progn
;;	       (meval*
;;		 '((MDEFINE )
;;		   ((,function-name) ,@ (cdr varl))
;;		   ((MPROGN) (($MODEDECLARE) ,varl $float)
;;		    ,x-y-expr)))
;;	       (mfuncall '$compile ',function-name)
;;	       ',function-name))
;;

	

;(defun plot-3d (&aux ff options ( $replotting t) (expr *expression-to-plot*)
;		($viewpt (meval '$viewpt))
;		($window (meval '$WINDOW1) )
;		($zmin (meval '$zmin) )
;		($zmax (meval '$zmax) )
;		($ymin (meval '$ymin) )
;		($ymax (meval '$ymax) )
;		($xmin (meval '$xmin) )
;		($xmax (meval '$xmax) )
;		($centerplot(meval '$centerplot)))
;  "Calls up a choice box in which you can enter a macsyma expression in x and y
; to be graphed, give a range, and have various options.  It compiles the macsyma
; expression as a function funtoplot(x,y):=.. .  It also accepts a lisp function.
; Look on the who line for the type of data to input."
;  (cond ((and (boundp ' $viewpt)(atom $viewpt))
;	 ($plotreset)))
;  (let ($replotting) (plot-input-choose))
;  (cond ((null expr) (format plot-stream "No function to plot")))
;  (setq *function-to-plot*
;	(cond ((and (atom *expression-to-plot*) (functionp *expression-to-plot*))
;	       *expression-to-plot*)
;	      ((null *expression-to-plot*) (fsignal "need an expression to plot"))
;	      (t
;	       (compile-define-function *expression-to-plot* 'funtoplot)
;;	       (meval*
;;		 `((MDEFINE SIMP)
;;		   ((funtoplot) $X $Y)
;;		   ((MPROGN) (($MODEDECLARE) $X $float $Y $float)
;;		    ,*expression-to-plot*)))
;;	       (let ((*standard-output* plot-stream ))(mfuncall '$compile 'funtoplot))
;	       (setq *function-to-plot* 'funtoplot))))
;  (setq options nil)
;;  (mfunction-call $plot3d *function-to-plot* *xlow* *xhigh* *ylow* *yhigh*
;;	    )
;  (meval* '(($plot3d)    *FUNCTION-TO-PLOT* *XLOW* *XHIGH* *YLOW* 
;						 *YHIGH*)))
	

(defun plot-3d (&aux ff options ( $replotting t) (expr *expression-to-plot*))
  "Calls up a choice box in which you can enter a macsyma expression in x and y
 to be graphed, give a range, and have various options.  It compiles the macsyma
 expression as a function funtoplot(x,y):=.. .  It also accepts a lisp function.
 Look on the who line for the type of data to input."
	(sloop for v in '(	$viewpt
		$window
		$zmin
		$zmax
		$ymin
		$ymax
		$xmin
		$xmax
		$centerplot)
	      do (set v (meval* v)))

;  (cond ((and (boundp ' $viewpt)(atom $viewpt))
;	 ($plotreset)))
  (let ($replotting) (plot-input-choose))
  (cond ((null expr) (format plot-stream "No function to plot")))
  (setq *function-to-plot*
	(cond ((and (atom *expression-to-plot*) (functionp *expression-to-plot*))
	       *expression-to-plot*)
	      ((null *expression-to-plot*) (error "need an expression to plot"))
	      (t
	       (compile-define-function *expression-to-plot* 'funtoplot)
;	       (meval*
;		 `((MDEFINE SIMP)
;		   ((funtoplot) $X $Y)
;		   ((MPROGN) (($MODEDECLARE) $X $float $Y $float)
;		    ,*expression-to-plot*)))
;	       (let ((*standard-output* plot-stream ))(mfuncall '$compile 'funtoplot))
	       (setq *function-to-plot* 'funtoplot))))
  (setq options nil)
;  (mfunction-call $plot3d *function-to-plot* *xlow* *xhigh* *ylow* *yhigh*
;	    )
  (meval* '(($plot3d)    *FUNCTION-TO-PLOT* *XLOW* *XHIGH* *YLOW* 
						 *YHIGH*)))

#+lispm
(progn
(DEFINE-USER-OPTION-ALIST *MACSYMA-PLOT-OPTIONS-ALIST* DEF-MC-PLT-OP1)  
;;for those windows wanting zeta lisp strings..
(defmacro def-mc-plt-op (var val type description)
  `(def-mc-plt-op1 ,var ,val ,type ,(zl-string description)))

; These four commented out 11/24/82 by CWH.  $XMIN1, etc are not bound at load time.

(DEFVAR *MACSYMA-PLOT-OPTIONS-ALIST* NIL)
(setq  *MACSYMA-PLOT-OPTIONS-ALIST* NIL)
(DEF-MC-PLT-OP $XMIN (meval* '$XMIN1) :macsyma-expression
	       "Minimum value for the x range" )
(DEF-MC-PLT-OP $xmax (meval* '$XMAX1) :macsyma-expression
	       "Maximum value for the x range")
(DEF-MC-PLT-OP $ymin (meval*  '$YMIN1) :macsyma-expression
	       "Minimum value for the y range")
(DEF-MC-PLT-OP $ymax (meval* '$YMAX1) :macsyma-expression
	       "Maximum value for the y range")
(def-mc-plt-op 3d  (cond ((boundp ' 3d)
			    3d)(t t)) :sexp "3d" )
(def-mc-plt-op Typel (meval* 'typel) :macsyma-expression "Typel"   )
(def-mc-plt-op $xlabel nil :macsyma-expression "Xlabel"   )
(def-mc-plt-op $ylabel nil :macsyma-expression "Ylabel"   )
(def-mc-plt-op $Title (meval* '$title) :macsyma-expression "Title"   )
(def-mc-plt-op $perspective (meval* '$perspective) :boolean "Perspective")
(def-mc-plt-op  $reverse (meval* '$reverse):boolean "reverse")
(def-mc-plt-op 	  $underside (meval* '$underside) :boolean  "Underside" )
(def-mc-plt-op  $howclose (meval*  '$howclose) :macsyma-expression "howclose"   )
(def-mc-plt-op  $crosshatch (meval* '$crosshatch)  :macsyma-expression "Crosshatch")
(def-mc-plt-op 	  $zmax (meval* '$zmax1) :macsyma-expression "Zmax")
(def-mc-plt-op  $zmin (meval*  '$zmin1) :macsyma-expression "Zmin")
(def-mc-plt-op $txtype (meval* '$txtype) :macsyma-expression
		 "$txtype One of polar, %log, loglin, linlog, lin, special")
		 
(def-mc-plt-op  $labelcontours (meval '$labelcontours) :macsyma-expression "labelcontours"     )
(def-mc-plt-op  $plotnumprec (meval '$plotnumprec) :macsyma-expression "plotnumprec ")
(def-mc-plt-op  $plotnum (meval* '$plotnum) :macsyma-expression "The number of points on an axis used (next time)")
(def-mc-plt-op  $zigzag (meval '$zigzag) :macsyma-expression "Zigzag")
;(def-mc-plt-op scale-x scale-x :number "scale-x" )
 ; (def-mc-plt-op  scale-y  scale-y :number "scale-y" )
  ;(def-mc-plt-op  max-xf  max-xf :number "max-xf" )
;(def-mc-plt-op  min-xf  min-xf :number "max-xf" )
  ;(DEF-MC-PLT-OP $zmax 0.0 :NUMBER "Maximum value for the z range")
  ;(DEF-MC-PLT-OP $zmin 0.0 :NUMBER "Minimum value for the z range")
(DEF-MC-PLT-OP $window (meval* '$WINDOW1) :macsyma-expression "Plot window")
(DEF-MC-PLT-OP $viewpt (meval* '$viewpt1) :macsyma-expression "View point")
(DEF-MC-PLT-OP  $centerplot (meval* '$centerplot1) :Macsyma-expression "Centerplot")
;(SETQ *MACSYMA-PLOT-OPTIONS-ALIST* (NREVERSE *MACSYMA-PLOT-OPTIONS-ALIST*))
 
;(DEFUN $PLOT_COMMAND (CMD)
;  (IF (AND (LISTP CMD) (EQ ':MENU (FIRST CMD)) (EQ ':KBD (SECOND (SECOND CMD))))
;      (SETQ CMD (THIRD (SECOND CMD))))
;  (CASE CMD
;    ((#/C #/c #\SPACE) ($ENDGRAPH)
;     '$DONE)
;    ((#/O #/o) (FUNCALL TERMINAL-IO ':BEEP) ($ENDGRAPH) 
;     '$DONE)
;    ((#/H #/h) ':HARDCOPY)
;    ((#/N #/n) '$DONE)
;    ((#/V #/v) (TV:CHOOSE-USER-OPTIONS *MACSYMA-PLOT-OPTIONS-ALIST*
;				   ':LABEL "Change Macsyma Plot Options")
;     ':REPLOT)
;
;    (OTHERWISE ($ENDGRAPH)
;		(IF (AND (INTEGERP CMD) (> CMD 0)) (FUNCALL TERMINAL-IO ':UNTYI CMD))
;		'$DONE))
;)

(defun plot-options-choose ()
				  (TV:CHOOSE-variable-values *MACSYMA-PLOT-OPTIONS-ALIST*
					   ':LABEL (zl-string "Change Macsyma Plot Options")
					   ':superior plot-stream))
;;bind these in plot-3d;
;(defun plot-options-choose ()
;  (let (($centerplot(meval '$centerplot))
;	($viewpt (meval '$viewpt))
;	($window (meval '$WINDOW1) )
;        ($zmin (meval '$zmin) )
;	($zmax (meval '$zmax) )
;	($ymin (meval '$ymin) )
;	($ymax (meval '$ymax) )
;	($xmin (meval '$xmin) )
;	($xmax (meval '$xmax) )
;	)
;    (declare (special $window $zmin))
;   
;				  (TV:CHOOSE-variable-values *MACSYMA-PLOT-OPTIONS-ALIST*
;					   ':LABEL (zl-string "Change Macsyma Plot Options")
;					   ':superior plot-stream)))

(defun plot-input-choose ()
  (let (($viewpt (meval '$viewpt))
	($centerplot (meval  '$centerplot))
	($xmin (meval '$xmin))
	($ymin (meval  '$ymin)))
	
				  (TV:CHOOSE-variable-values  *Original-plot-3d-options*
					   ':LABEL (zl-string "Plot Input Defaults")
					   ':superior plot-stream)))

(defun plot-input-choose ()
  (sloop for v in 
     '( $viewpt
	$centerplot
	$xmin
	$ymin $plotnum0 $plotnum1) do (set v (meval* v)))
	
				  (TV:CHOOSE-variable-values  *Original-plot-3d-options*
					   ':LABEL (zl-string "Plot Input Defaults")
					   ':superior plot-stream))




(DEFUN $PLOT_COMMAND (CMD)
  (cond ((and  (consp cmd)
	       (eq ':menu (first cmd))
	       (eq ':value (second (second cmd))))
	 (setq cmd (second (third (second cmd)))))
	(t nil))

;  (IF (AND (LISTP CMD) (EQ ':MENU (FIRST CMD)) (EQ ':KBD (SECOND (SECOND CMD))))
;      (SETQ CMD (THIRD (SECOND CMD))))
  (and (numberp cmd) (setf cmd (code-char cmd)))
  (CASE CMD
    ((#\P #\p) (plot-3d) 'continue)
    ((#\E #\e #\SPACE) (setq $replotting nil) ($ENDGRAPH)
     '$DONE)
    ((#\O #\o) (plot-options-choose)
     ':replot)
    ((#\H #\h) ':HARDCOPY)
    ((#\N #\n) '$DONE)
    ((#\R #\r) (plot-input-choose) :replot)
    ((#\M #\m  #\V #\v #\O #\o) (plot-options-choose)
     ':replot)
    ((otherwise 'continue))))


)

;;; drawing primitives

;;; **** change this back at somepoint.  Do this via definesymbol.
(defun pointi (x y)
   (FUNCALL PLOT-STREAM ':plot-POINT X Y)
   (setq pnt-status t last-x x last-y y screen-last-x x screen-last-y y))

(defun vectori (x y)
  (FUNCALL PLOT-STREAM ':plot-LINE LAST-X LAST-Y X Y)
  (setq pnt-status t last-x x last-y y screen-last-x x screen-last-y y))

#|
(DEFUN PLOT-DRAW-CHAR (CHAR X Y)
  (MULTIPLE-VALUE-BIND (WIDTH HEIGHT) (FUNCALL PLOT-STREAM ':SIZE)
    (FUNCALL PLOT-STREAM ':DRAW-CHAR (FUNCALL PLOT-STREAM ':CURRENT-FONT)
	     CHAR X (f- HEIGHT Y))))
|#

;;; Line, Vector, Point primitives

(DEFMACRO CHECKPNT (X Y) `(not (or (< ,x min-x) (> ,x max-x) (< ,y min-y) (> ,y max-y))))

;;; setpoint

(defun $setpoint (xf yf) (setpoint (plot-x xf) (plot-y yf))) 

(defun setpoint (x y) 
   (IF (checkpnt x y) (setpointi x y)
       (setq pnt-status nil last-x x last-y y)))

(defun setpointi (x y)
   (IF (NOT (and pnt-status (= screen-last-x x) (= screen-last-y y)))
       (setq pnt-status t last-x x last-y y screen-last-x x screen-last-y y)))

;;; draw point

(defun plot-point (x y) 
  (IF (checkpnt x y) (pointi x y)
      (setq pnt-status nil last-x x last-y y)))

(defun $point (xf yf) (plot-point (plot-x xf) (plot-y yf))) 


;;; vectors

(setq dashl nil odashl nil beamon t drawn 0.
      dasharray (zl-make-array 10)
      )

;;; patterned (dashed) vectors

(defun vectord (x y) 
  (cond ((null dashl) (vectori x y))		;simple case, solid lines
	((not (eq (ml-typep dashl) 'list)) (setpoint x y))	;otherwise, just points
	(t (let ((save-x last-x) (save-y last-y) (del-xf (float (f- x last-x)))
				 (del-yf (float (f- y last-y))) (lenf 0.0) (len 0.))
		(setq lenf (sqrt (+$ (*$ del-xf del-xf) (*$ del-yf del-yf))) 
		      len (fix (+$ lenf 0.5)) 
		      lenf (if (= 0 lenf) 1.0 lenf)
		      del-xf (//$ del-xf lenf) del-yf (//$ del-yf lenf))
		(do ((runl (f- (car dashl) drawn) (f+ runl (car dashl)))
		     (targ-x) (targ-y))
		    (nil)
		    (setq targ-x (f+ save-x (fix (+$ (*$ (float runl) del-xf) 0.5))) 
			  targ-y (f+ save-y (fix (+$ (*$ (float runl) del-yf) 0.5))))
		    (cond (beamon (cond ((< runl len)
					 (vectori targ-x targ-y)
					 (setq drawn 0. dashl (cdr dashl)
					       beamon nil))
					(t (vectori x y)
					   (cond ((= runl len)
						  (setq drawn 0. dashl (cdr dashl)
							beamon nil))
						 (t (setq drawn
							  (f- (car dashl)
							     (f- runl len)))))
					   (return nil))))
			  (t (cond ((< runl len)
				    (setpoint targ-x targ-y)
				    (setq drawn 0. dashl (cdr dashl) beamon t))
				   (t (setpoint x y)
				      (cond ((= runl len)
					     (setq drawn 0. dashl (cdr dashl) 
						   beamon t))
					    (t (setq drawn (f- (car dashl)
							      (f- runl len)))))
				      (return nil))))))))))

(defun $definedash (l1 l)
       (or (and (integerp l1) (< l1 10.) (> l1 -1.))
	   (Error  "First arg to DEFINEDASH must lie between 0 and 9: ~A" L1))
       (prog2 nil
	      (list '(mlist simp) l1 l)
	      (cond ((or (null l) (eq l t) (eq (ml-typep l) 'list))
		     (and (eq (ml-typep l) 'list)
			  (consp (car l))
			  (eq (caar l) 'mlist) (setq l (cdr l)))
		     (or (eq l t) (setq l (mapcar 'fix l)))
		     (aset l dasharray l1)))))

(defun $changedash (x) 
   (setq dashl (aref dasharray x) odashl nil drawn 0. beamon t)
   (cond ((eq (ml-typep dashl) 'list)
	  (setq dashl (copy-top-level dashl))
	  (rplacd (last dashl) dashl)))
   nil)

(defun $pushdash nil (setq odashl dashl dashl nil)) 

(defun $popdash nil (setq dashl odashl odashl nil))

(defun init-dashes nil
       ($definedash 1. '(40. 8.)) ($definedash 2. '(15. 8.)) ($definedash 3. '(1. 7.))
       ($definedash 4. '(30. 8. 1. 8.)) ($definedash 5. '(30. 8. 1. 8. 1. 8.))
       ($definedash 6. '(40. 8. 1. 8. 5. 8. 1. 8.)) ($definedash 7. '(8. 30.))
       ($definedash 8. '(1. 20.)) ($definedash 9. t))

(init-dashes)

;;; Can this be done with the built-in methods?
;;; ****

;;; vectors with clipping

(defun maxima-vector (x y) 
       (cond ((and pnt-status (checkpnt x y)) (vectord x y))
	     (t (prog (del-x del-y save-x save-y) 
		      (setq save-x x save-y y del-x (f- x last-x) del-y (f- y last-y))
		      (cond ((> last-y max-y)
			     (cond ((> y max-y) (go no-vector))
				   (t (setq last-x (intercept last-x last-y del-x del-y max-y)
					    last-y max-y)
				      (cond ((< y min-y)
					     (setq x (intercept x y del-x del-y min-y)
						   y min-y))))))
			    ((< last-y min-y)
			     (cond ((< y min-y) (go no-vector))
				   (t (setq last-x (intercept last-x last-y del-x del-y min-y)
					    last-y min-y)
				      (cond ((> y max-y)
					     (setq x (intercept x y del-x del-y max-y)
						   y max-y))))))
			    ((> y max-y)
			     (setq x (intercept x y del-x del-y max-y) y max-y))
			    ((< y min-y)
			     (setq x (intercept x y del-x del-y min-y) y min-y)))
		      (cond ((> last-x max-x)
			     (cond ((> x max-x) (go no-vector))
				   (t (setq last-y (intercept last-y last-x del-y del-x max-x)
					    last-x max-x)
				      (cond ((< x min-x)
					     (setq y (intercept y x del-y del-x min-x)
						   x min-x))))))
			    ((< last-x min-x)
			     (cond ((< x min-x) (go no-vector))
				   (t (setq last-y (intercept last-y last-x del-y del-x min-x)
					    last-x min-x)
				      (cond ((> x max-x)
					     (setq y (intercept y x del-y del-x max-x)
						   x max-x))))))
			    ((> x max-x)
			     (setq y (intercept y x del-y del-x max-x) x max-x))
			    ((< x min-x)
			     (setq y (intercept y x del-y del-x min-x) x min-x)))
		      (cond ((not pnt-status) (setpoint last-x last-y)))
		      (vectord x y)
		      no-vector
		      (cond ((not (checkpnt save-x save-y))
			     (setq pnt-status nil last-x save-x last-y save-y))))))
       nil) 

(defun intercept (x y del-x del-y max-y) (f- x (// (f* del-x (f- y max-y)) del-y))) 

(defun $vector (xf yf) (maxima-vector (plot-x xf) (plot-y yf))) 

;;; lines

(defun line (x1 y1 x y) (setpoint x1 y1) (maxima-vector x y)) 

(defun $line (xf1 yf1 xf yf) 
       (setpoint (plot-x xf1) (plot-y yf1)) (maxima-vector (plot-x xf) (plot-y yf))) 

;;; symbols

(setq symbolarray (zl-make-array 10))

(defun drawsymbol (x y x1) 
   ($pushdash)
   (do ((symbl0 (aref symbolarray x1) (cdr symbl0)) (draw nil (not draw)))
       ((cond ((null symbl0)) ((eq symbl0 t) (plot-point x y) t)))
       (do ((symbl1 (car symbl0) (cddr symbl1))) ((null (cdr symbl1)))
	   (cond (draw (maxima-vector (f+ x (car symbl1)) (f+ y (cadr symbl1))))
		 (t (setpoint (f+ x (car symbl1)) (f+ y (cadr symbl1)))))))
   ($popdash))

(defun $drawsymbol (xf yf x1) (drawsymbol (plot-x xf) (plot-y yf) x1)) 

(defun $definesymbol (l1 l) 
       (or (and (integerp l1) (< l1 10.) (> l1 -1.))
	   (Error "First arg to DEFINESYMBOL must lie between 0 and 9: ~A" L1))
       (prog2 nil
	      (list '(mlist simp) l1 l)
	      (cond ((or (null l) (eq l t) (eq (ml-typep l) 'list))
		     (and (eq (ml-typep l) 'list)
			  (consp (car l))
			  (eq (caar l) 'mlist) (setq l (cdr l)))
		     (or (eq l t)
			 (setq l (mapcar #'(lambda (l2) (and (consp (car l2))
							     (eq (caar l2) 'mlist)
							     (setq l2 (cdr l2)))
						   (mapcar 'fix l2))
					 l)))
		     (aset l symbolarray l1))))) 

(defun init-symbols nil
       ($definesymbol 0. nil)
       ($definesymbol 1. '((0. 6.) (0. -6.) (-6. 0.) (6. 0.) (0. 0.)))
       ($definesymbol 2. '((4. 4.) (-4. -4.) (4. -4.) (-4. 4.) (0. 0.)))
       ($definesymbol 3. '((6. 6.) (6. -6. -6. -6. -6. 6. 6. 6.) (0. 0.) (0. 0.)))
       ($definesymbol 4. '((8. 0.) (0. -8. -8. 0. 0. 8. 8. 0.) (0. 0.) (0. 0.)))
       ($definesymbol 5. '((0. 8.) (6. -4. -6. -4. 0. 8.) (0. 0.) (0. 0.)))
       ($definesymbol 6. '((0. -8.) (6. 4. -6. 4. 0. -8.) (0. 0.) (0. 0.)))
       ($definesymbol 7. '((8. 0.) (-4. 6. -4. -6. 8. 0.) (0. 0.) (0. 0.)))
       ($definesymbol 8. '((-8. 0.) (4. 6. 4. -6. -8. 0.) (0. 0.) (0. 0.)))
;      ($definesymbol 9. '((0. 9.) (4. -6. -7. 2. 7. 2. -4. -6. 0. 9.) (0. 0.)))
       ($definesymbol 9. t))

(init-symbols) 

;;; scaling functions

(declare-top (special min-xf max-xf min-yf max-yf size-xf size-yf scale-x scale-y)) 

(setq min-xf 0.0 min-yf 0.0 max-xf 1023.0 max-yf 1023.0 size-xf 1023.0 size-yf 1023.0)

(defun $screensize (x1 y1 x y) 
       (setq min-x x1 min-y y1 max-x x max-y y
	     size-x (f- max-x min-x) size-y (f- max-y min-y) 
	     scale-x (//$ size-xf (float size-x)) scale-y (//$ size-yf (float size-y)) 
	     pnt-status nil last-x min-x last-y min-y)
       nil) 

;;; what should this be?****
($screensize 0. 0. 1023. 1023.)

(defun $screensize1 (x1 y1 x y) 
       ($size (plot-xf x1) (plot-yf y1) (plot-xf x) (plot-yf y))
       ($screensize x1 y1 x y))  

(defun $size (xf1 yf1 xf yf) 
       (setq min-xf xf1 min-yf yf1 max-xf xf max-yf yf 
	     size-xf (-$ max-xf min-xf) size-yf (-$ max-yf min-yf) 
	     scale-x (//$ size-xf (float size-x)) scale-y (//$ size-yf (float size-y)))
       nil) 

(defun plot-x (xf) (f+ min-x (fix (+$ 0.5 (//$ (-$ xf min-xf) scale-x))))) 

(defun plot-y (yf) (f+ min-y (fix (+$ 0.5 (//$ (-$ yf min-yf) scale-y))))) 

(defun plot-xf (x) (+$ min-xf (*$ (float (f- x min-x)) scale-x))) 

(defun plot-yf (y) (+$ min-yf (*$ (float (f- y min-y)) scale-y))) 

(declare-top (special txfun-x txfun-y txfun-x-nargs txfun-y-nargs))

(defun call-x (&optional xf yf zf)
       (case txfun-x-nargs
		(0 xf)
		(1 (funcall txfun-x xf))
		(2 (funcall txfun-x xf yf))
		(3 (funcall txfun-x xf yf zf))
		(otherwise
		 (Error "Wrong number arguments to x transformation function"))))

(defun call-y (&optional xf yf zf)
       (case txfun-y-nargs
		(0 yf)
		(1 (funcall txfun-y xf))
		(2 (funcall txfun-y xf yf))
		(3 (funcall txfun-y xf yf zf))
		(otherwise
		 (Error  "Wrong number arguments to y transformation function"))))
(defun call-init (xfun yfun)
   (setq txfun-x xfun
	 txfun-y yfun
	 txfun-x-nargs (cond #+lispm (xfun (length (arglist xfun)))
			     (t 0))
	 txfun-y-nargs (cond #+lispm (yfun (length (arglist yfun)))
			     (t 0))))

(defun $setpoint3 (xf yf zf) ($setpoint (call-x xf yf zf) (call-y xf yf zf)))

(defun $point3 (xf yf zf) ($point (call-x xf yf zf) (call-y xf yf zf)))

(defun $vector3 (xf yf zf) ($vector (call-x xf yf zf) (call-y xf yf zf)))

(defun $line3 (xf1 yf1 zf1 xf yf zf)
       ($line (call-x xf1 yf1 zf) (call-y xf1 yf1 zf1)
	      (call-x xf yf zf) (call-y xf yf zf)))

(defun $drawsymbol3 (xf yf zf x1) ($drawsymbol (call-x xf yf zf) (call-y xf yf zf) x1))

;;; Character drawing routines
;;; (this is pretty random and needs to be redone)

(defun gterpri nil (setpoint min-x (f- last-y char-height))) 

(defun $gterpri nil (gterpri)) 

(defun ghprint (l x y a1) 
       (cond ((atom l) (setq l (exploden l)))
	     (t (setq l (apply 'append (mapcar 'exploden l)))))
       (let ((b1 (f* char-width (length l)))
	     (uline (> a1 9.)))
	    (and uline (setq a1 (f- a1 10.)))
	    (cond ((= a1 0.))
		  ((= a1 1.) (setq x (f- x (// b1 2.))))
		  ((= a1 2.) (setq x (f- x b1))))
	    (and (> (f+ x b1) max-x) (setq x (f- max-x b1)))
	    (and (< x min-x) (setq x min-x))
	    (and (> (f+ y char-height) max-y) (setq y (f- max-y char-height)))
	    (and (< y min-y) (setq y min-y))
	    (do ((l l (cdr l)) (x x (f+ x char-width)) (flg t NIL))
		((or (null l) (> x (f- max-x char-width))))
		(cond (flg (setpoint x y) (setq pnt-status nil)))
		(send plot-stream ':plot-char (car l) x y))
	    (if uline (line x (f- y 2.) (f+ x b1) (f- y 2.)))
	    (setpoint (f+ x b1) y)))

(defun $ghprint (l x y a1) 
       (setq l (cond ((and (consp l) (consp (car l)) (eq (caar l) 'mlistp))
                      (cdr l))
                     (t l)))
       (ghprint (cond ((atom l) (stripdollar l)) (t (mapcar 'stripdollar l)))
		x y a1))

(defun gvprint (l x y a1)
       (cond ((atom l) (setq l (exploden l)))
	     (t (setq l (apply 'append (mapcar 'exploden l)))))
       (let ((b1 (f* char-height (length l))))
	    (cond ((= a1 0.))
		  ((= a1 1.) (setq y (f+ y (// b1 2.))))
		  ((= a1 2.) (setq y (f+ y b1))))
	    (if (< (f- y b1) min-y) (setq y (f+ min-y b1)))
	    (if (> y max-y) (setq y max-y))
	    (if (> (f+ x char-width) max-x) (setq x (f- max-x char-width)))
	    (if (< x min-x) (setq x min-x))
	    (do ((l l (cdr l)) (y (f- y char-height) (f- y char-height)))
		((or (null l) (< y min-y)))
		(setpoint x y)(setq pnt-status nil)
		(send plot-stream ':plot-char (car l) x y))
	    (setpoint y (f- y b1))))

(defun $gvprint (l x y a1) 
       (setq l (cond ((and (consp l) (consp (car l)) (eq (caar l) 'mlistp))
                      (cdr l))
                     (t l)))
       (gvprint (cond ((atom l) (stripdollar l)) (t (mapcar 'stripdollar l)))
		x y a1))

(defun gmark (x y x1) 
  (setpoint (f- x (// char-width 2.)) (f- y (// char-height 2.)))
  (if pnt-status
      (cond ((checkpnt (f+ last-x char-width) (f+ last-y char-height))
	     (setq pnt-status nil)
	     (send plot-stream ':plot-char x1 last-x last-y))))
  (setpoint x y))

(defun $gmark (xf yf x1) (gmark (plot-x xf) (plot-y yf) x1))  

