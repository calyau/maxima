;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")
;;; Window System Interface for the Macsyma Plot Package.

;;; First define the high level structures, namely the constraint frames
;;; which contain a plotting window, a menu, and (optionally) a Macsyma listener.

;;; Some special variables needed below.



(DEFVAR PLOT-FONT-MAP 'FONTS:(CPTFONT MEDFNT BIGFNT))	;fonts used on plot windows

(proclaim '(special plot-stream))

(DEFVAR PLOT-ITEM-LIST NIL)

(DEFMACRO DEFINE-PLOT-MODE (FUNCTION CHARACTER NAME DOCUMENTATION)
  `(DEFINE-PLOT-MODE-1 ',FUNCTION ',CHARACTER ',NAME ',DOCUMENTATION))

(DEFUN DEFINE-PLOT-MODE-1 (FUNCTION CHARACTER NAME DOCUMENTATION)
  (LET ((ELEM (zl-ASSOC NAME PLOT-ITEM-LIST)))
    (IF ELEM (SETQ PLOT-ITEM-LIST (DELQ ELEM PLOT-ITEM-LIST))))
  (SETQ PLOT-ITEM-LIST
	(APPEND PLOT-ITEM-LIST ;use APPEND, not NCONC, so that it will look different!
	       (NCONS (LIST NAME
			    ':VALUE (LIST FUNCTION CHARACTER NAME DOCUMENTATION)
			    ':DOCUMENTATION DOCUMENTATION)))))
 
(DEFINE-PLOT-MODE Menu-plot   #\M "Options"
   "Menu of options for replotting")

(DEFINE-PLOT-MODE Replot-graph #\R "Replot"
   "Replot after giving a menu to adjust")

(DEFINE-PLOT-MODE plot-graph  #\P "Plot"
   "Plot prompting for function in menu window.")

(DEFINE-PLOT-MODE Hardcopy-graph  #\H "Hardcopy"
   "Hardcopy if supported")
 
(DEFINE-PLOT-MODE End-graph #\E "Exit"
   "Exit and return to Macsyma top level")


;;;; Menu item lists for the plot menu.
;(DEFVAR PLOT-MENU-ITEM-LIST  
;	'(("Continue" :KBD #/C)
;	  ("Options"  :kbd #/V)
;	  ("Name"     :KBD #/N)
;	  ("Hardcopy" :KBD #/H)
;	  ("Other"    :KBD #/O)))
;(DEFVAR PLOT-MENU-ITEM-LIST 
;	'(("Continue" :KBD #/C)
;	  ("Options"  :kbd #/V)
;	  ("Name"     :KBD #/N)
;	  ("Hardcopy" :KBD #/H)
;	  ("Other"    :menu (send self :execute '("other" :kbd #/v)))))
;;how to get the other to select the choices?

;(DEFVAR PLOT-OTHER-MENU-ITEM-LIST	;invoked by OTHER-CMDS
;	'(("Store"    . :STORE-PLOT)
;	  ("Retrieve" . :RETRIEVE-PLOT)
;	  ("Multiple Plots" . :REPLOT-MULTIPLE)
;	  ("Options" . :CHANGE-OPTIONS)
;	  ("Change Plot" . :CHANGE-PARAMETERS)))

;;; Define a Macsyma plot frame.

(DEFFLAVOR MACSYMA-PLOT-FRAME ((MACSYMA-LISTENER-WINDOW NIL))
	   (tv:BORDERED-CONSTRAINT-FRAME)
  (:DEFAULT-INIT-PLIST :SAVE-BITS T)
  (:INITABLE-INSTANCE-VARIABLES MACSYMA-LISTENER-WINDOW)
  )

(defmethod (macsyma-plot-frame :after :init) (ignore &aux menu)
 (setq menu (car  (send self :exposed-panes)))
  #+obsolete  ;;this is now done in $plot3d by the set-command... message.
  (let ((win (car (send current-process :run-reasons))))
    (cond ((symbolp win))
	  (t  (send menu :set-io-buffer (send win :io-buffer)))))
  )

(defmethod (macsyma-plot-frame :set-command-menu-io-buffer-to-plot-stream-buffer)
	   ( &aux menu)
 (setq menu (car  (send self :exposed-panes)))

; (send menu :set-io-buffer (send (car (send current-process :run-reasons))
;				 :io-buffer)))

 (send menu :set-io-buffer (send plot-stream
				 :io-buffer)))

			       
;;; Defines the panes contained in the frame and the constaints they must satisfy.
;;; Note, if MACSYMA-LISTENER-WINDOW is non-nil, it is assumed that the thus
;;; specified MACSYMA-LISTENER is to be included at the bottom of the frame.

(DEFMETHOD (MACSYMA-PLOT-FRAME :BEFORE :INIT) (IGNORE &AUX LISTENER-ITEM
							   LISTENER-PANE-ITEM
						           LISTENER-CONSTRAINT
							   PLOT-PANE-CONSTRAINT)
  (SETQ PLOT-PANE-CONSTRAINT '((PLOT-PANE :EVEN)))
  (IF MACSYMA-LISTENER-WINDOW
      (SETQ LISTENER-ITEM '(LISTENER-PANE)
	    LISTENER-PANE-ITEM '((LISTENER-PANE MACSYMA-LISTENER))
	    LISTENER-CONSTRAINT '(((LISTENER-PANE :EVEN)))
	    PLOT-PANE-CONSTRAINT '((PLOT-PANE 0.75s0))))
  (setq win (global:format nil  "Macsyma Plotting Window"))
  (SETQ TV:PANES `((PLOT-PANE MACSYMA-PLOT-PANE :LABEL  ,win
			      #-symbolics :FONT-MAP #-symbolics ,PLOT-FONT-MAP
			      )
		   (PLOT-MENU TV:COMMAND-MENU-PANE :ITEM-LIST ,PLOT-ITEM-LIST
			      #-symbolics :FONT-MAP #-symbolics (FONTS:MEDFNT)
			      )
		   .,LISTENER-PANE-ITEM)
	TV:CONSTRAINTS `((MAIN . ((PLOT-PANE PLOT-MENU .,LISTENER-ITEM)
				  ((PLOT-MENU :ASK :PANE-SIZE))
				  ,PLOT-PANE-CONSTRAINT
				  .,LISTENER-CONSTRAINT)))))

;;; Now define the MACSYMA-PLOT-PANE and MACSYMA-PLOT-MENU flavors.
(DEFFLAVOR MACSYMA-PLOT-PANE () (TV:PANE-MIXIN TV:WINDOW))
(DEFFLAVOR macsyma-plot-menu () (TV:pane-MIXIN  tv:command-menu))
;; Compute some global variables which the LGP, etc. needs
;; to know about.
(DEFVAR PLOT-FONT (SEND tv:main-screen ':CURRENT-FONT))
(DEFVAR PLOT-WIDTH 762)
(DEFVAR PLOT-HEIGHT 854)
(DEFVAR CHAR-HEIGHT 12)
(DEFVAR CHAR-WIDTH 8)

(DEFMETHOD (MACSYMA-PLOT-PANE :UPDATE-VARIABLES) ()
  (MULTIPLE-VALUE-SETQ (PLOT-WIDTH PLOT-HEIGHT) (send self ':INSIDE-SIZE))
  (SETQ PLOT-FONT (send SELF ':CURRENT-FONT)
	CHAR-WIDTH (FONT-CHAR-WIDTH PLOT-FONT)
	CHAR-HEIGHT (FONT-CHAR-HEIGHT PLOT-FONT)))

;;; Window of the plotting area
(DEFMETHOD (MACSYMA-PLOT-PANE :GET-PLOTTING-RANGE) ()
  (SEND SELF ':UPDATE-VARIABLES)
  (LIST 10 10 (f- (f1- PLOT-WIDTH) 10) (f- (f1- PLOT-HEIGHT) 10)))

;;; Clear the plot pane, make sure it has the same IO-BUFFER as
;;; the Macsyma listener (TERMINAL-IO), select the plotting pane,
;;; and expose the whole plotting frame.
;;; (It is unclear if TERMINAL-IO is the right thing here, but...)


(DEFMETHOD (MACSYMA-PLOT-PANE :INIT-FOR-PLOTTING)
	   (&AUX (IO-BUFFER (FUNCALL *terminal-io* ':IO-BUFFER))
		 (MENU (FUNCALL TV:SUPERIOR ':GET-PANE 'PLOT-MENU)))
;;   (SEND SELF ':CLEAR-SCREEN)		;clear the plot pane before exposing
   (SETQ TV:IO-BUFFER IO-BUFFER)		;plot pane's io-buffer
   #-genera (SET-IN-INSTANCE MENU 'TV:IO-BUFFER IO-BUFFER)
   #+genera (send menu :set-io-buffer io-buffer)
   ;;plot menu's io-buffer
   (SEND SELF ':EXPOSE-PLOT))

;(DEFMETHOD (MACSYMA-PLOT-PANE :INIT-FOR-PLOTTING) ()
;;	   (&AUX (IO-BUFFER (FUNCALL TERMINAL-IO ':IO-BUFFER))
;;		 (MENU (FUNCALL TV:SUPERIOR ':GET-PANE 'PLOT-MENU)))
;;;   (SEND SELF ':CLEAR-SCREEN)		;clear the plot pane before exposing
;;   (SETQ TV:IO-BUFFER IO-BUFFER)		;plot pane's io-buffer
;;   (SET-IN-INSTANCE MENU 'TV:IO-BUFFER IO-BUFFER)	;plot menu's io-buffer
;   (SEND SELF ':EXPOSE-PLOT))

;;; Select the original Macsyma listener (TERMINAL-IO) and bury
;;; if necessary.
(defvar $replotting t)
(DEFMETHOD (MACSYMA-PLOT-PANE :END-PLOTTING) ()
  (cond ((null $replotting)   (TV:DESELECT-AND-MAYBE-BURY-WINDOW TV:SUPERIOR)
	 (FUNCALL *terminal-io* ':SELECT))))

;; Define the primitive functions for drawing etc.
(DEFMETHOD (MACSYMA-PLOT-PANE :EXPOSE-PLOT) ()
  (SEND SELF ':UPDATE-VARIABLES)
  (FUNCALL TV:SUPERIOR ':EXPOSE)		;expose the whole frame
  (SEND SELF ':SELECT))			;and select the plot pane

(DEFMETHOD (MACSYMA-PLOT-PANE :PLOT-LINE) (X0 Y0 X1 Y1)
  (SEND SELF ':DRAW-LINE X0 (f- PLOT-HEIGHT Y0 1) X1 (f- PLOT-HEIGHT Y1 1)))

(DEFMETHOD (MACSYMA-PLOT-PANE :PLOT-POINT) (X Y)
    (LET ((X X) (Y (f- PLOT-HEIGHT Y 1)))
      (SEND SELF ':DRAW-POINT X Y)
      (SEND SELF ':DRAW-POINT (f1+ X) Y)
      (SEND SELF ':DRAW-POINT (f1+ X) (f1+ Y))
      (SEND SELF ':DRAW-POINT X (f1+ Y))))

(DEFMETHOD (MACSYMA-PLOT-PANE :PLOT-CHAR) (CHAR X Y)
  (SEND SELF ':DRAW-CHAR #-symbolics PLOT-FONT 
        CHAR X (f- PLOT-HEIGHT (f+ Y CHAR-HEIGHT) 1)))

(DEFMETHOD (MACSYMA-PLOT-PANE :HARDCOPY) ()
  (LET ((TV:SELECTED-WINDOW SELF))
    (TV:KBD-ESC-Q 1)))

(DEFVAR PLOT-FRAME)			;handy for debugging

;; generalize this to include a Macsyma Listener option
(DEFUN MAKE-PLOT-WINDOW-STREAM ()
  (SETQ PLOT-FRAME (TV:MAKE-WINDOW 'MACSYMA-PLOT-FRAME  ':SAVE-BITS T))
  (FUNCALL PLOT-FRAME ':GET-PANE 'PLOT-PANE))

(DEFVAR SPLIT-SCREEN-PLOT-FRAME NIL)

;;; Setup for plotting and Macsyma listening
(DEFUN $PLOT_SPLITSCREEN ()
  (IF (NOT SPLIT-SCREEN-PLOT-FRAME)
      (SETQ SPLIT-SCREEN-PLOT-FRAME
	    (TV:MAKE-WINDOW 'MACSYMA-PLOT-FRAME ':MACSYMA-LISTENER-WINDOW T)))
  (SETQ PLOT-STREAM (FUNCALL SPLIT-SCREEN-PLOT-FRAME ':GET-PANE 'PLOT-PANE))
  (FUNCALL SPLIT-SCREEN-PLOT-FRAME ':EXPOSE) 
  (FUNCALL (FUNCALL SPLIT-SCREEN-PLOT-FRAME ':GET-PANE 'LISTENER-PANE) ':SELECT)
  '$DONE)

(DEFUN $PLOT_FULLSCREEN ()
  (SETQ PLOT-STREAM (IF (NOT (BOUNDP 'PLOT-FRAME)) (MAKE-PLOT-WINDOW-STREAM)
			(FUNCALL PLOT-FRAME ':GET-PANE 'PLOT-PANE)))
  (FUNCALL MACSYMA-TOP-WINDOW ':EXPOSE)
  (FUNCALL MACSYMA-TOP-WINDOW ':SELECT))

(COMPILE-FLAVOR-METHODS MACSYMA-PLOT-PANE MACSYMA-PLOT-FRAME)



