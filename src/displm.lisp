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
(macsyma-module displm macro)

(declare-top
 ;; evaluate for declarations
 (special
  ^w			;If T, then no output goes to the console.
  ^r			;If T, then output also goes to any
			;file opened by UWRITE.  People learning
			;Lisp: there are better ways of doing IO
			;than this -- don't copy this scheme.
  smart-tty		;LOADER sets this flag.  If T, then
			;then this console can do cursor movement
			;and equations can be drawn in two dimensions.
  rubout-tty	        ;If T, then console either selectively erasable
			;or is a glass tty.  Characters can be rubbed
			;out in either case.
  scrollp		;If T, then the console is scrolling.
			;This should almost always be equal to
			;(NOT SMART-TTY) except when somebody has
			;done :TCTYP SCROLL on a display console.
			;This is the %TSROL bit of the TTYSTS word.

  linel			;Width of screen.
  ttyheight		;Height of screen.

  width height depth maxht maxdp level size lop rop break right
  bkpt bkptwd bkptht bkptdp bkptlevel bkptout lines 
  oldrow oldcol display-file in-p
  moremsg moreflush more-^w mratp $aliases aliaslist)

 (fixnum width height depth maxht maxdp level size right 
	 bkptwd bkptht bkptdp bkptlevel bkptout
	 linel ttyheight oldrow oldcol)

 (notype (tyo* fixnum) (setcursorpos fixnum fixnum))

 (*expr +tyo setcursorpos mterpri force-output linear-displa
	ttyintson ttyintsoff more-fun getop
	lbp rbp nformat fullstrip1 makstring $listp)

 ;; stuff other packages might want to reference selectively.
 (*expr displa dimension checkrat checkbreak)
 ;; looks like missplaced declarations to me.
 ;; does DISPLA really call $integrate?
 (*lexpr $box $diff $expand $factor $integrate $multthru $ratsimp)
 )

;;; macros for the DISPLA package.

(defmacro tablen () #-(or franz cl) (status tabsize) #+(or franz cl) 8)

;; macros to handle systemic array differences.
;; NIL has various types of arrays, and supports *ARRAY in compatibility,
;; but might as well use the natural thing here, a vector.

(defmacro make-linearray (size)
  #+(or maclisp franz) `(*array nil t ,size)
  #+(or cl nil) `(make-array ,size :initial-element nil)
  )

(defmacro set-linearray (i x)
  #+(or maclisp franz) `(store (arraycall t linearray ,i) ,x)
  #+(or cl nil) `(setf (svref linearray ,i) ,x)
  )

(defmacro linearray (j)
  #+cl `(aref linearray ,j)
  #+(or maclisp franz) `(arraycall t linearray ,j)
  #+nil `(svref linearray ,j)
  )

(defmacro linearray-dim ()
  #+(or  maclisp franz) '(array-dimension-n 1 linearray)
  #+(or cl nil) '(length (the vector linearray)))

(defmacro clear-linearray ()
  #+(or  maclisp franz) '(fillarray linearray '(nil))
  #+(or cl nil) '(fill linearray nil))

;; (PUSH-STRING "foo" RESULT) --> (SETQ RESULT (APPEND '(#/o #/o #/f) RESULT))
;; CHECK-ARG temporarily missing from Multics.

(defmacro push-string (string symbol)
  #-(or franz multics) (check-arg string stringp "a string")
  #-(or franz multics) (check-arg symbol symbolp "a symbol")
					;`(SETQ ,SYMBOL (APPEND ',(NREVERSE (EXPLODEN STRING)) ,SYMBOL))
					;The string is usually short.  Do it out...
  `(setq ,symbol (list* ,@(nreverse (exploden string)) ,symbol))
  )

;; Macros for setting up dispatch table.
;; Don't call this DEF-DISPLA, since it shouldn't be annotated by
;; TAGS and @.  Syntax is:
;; (DISPLA-DEF [<operator>] [<dissym> | <l-dissym> <r-dissym>] [<lbp>] [<rbp>])
;; If only one integer appears in the form, then it is taken to be an RBP.

;; This should be modified to use GJC's dispatch scheme where the subr
;; object is placed directly on the symbol's property list and subrcall
;; is used when dispatching.

(defmacro displa-def (operator dim-function &rest rest
		      &aux l-dissym r-dissym lbp rbp)
  (dolist (x rest)
    (cond ((stringp x)
	   (if l-dissym (setq r-dissym x) (setq l-dissym x)))
	  ((integerp x)
	   (if rbp (setq lbp rbp))
	   (setq rbp x))
	  (t (maxima-error "Random object in `displa-def' form" x))))
  (if l-dissym
      (setq l-dissym
	    (if r-dissym
		(cons (exploden l-dissym) (exploden r-dissym))
		(exploden l-dissym))))
  `(progn 
    (defprop ,operator ,dim-function dimension)
    ,(if l-dissym  `(defprop ,operator ,l-dissym dissym))
    ,(if lbp       `(defprop ,operator ,lbp lbp))
    ,(if rbp       `(defprop ,operator ,rbp rbp))))

;; Why must interrupts be turned off?  Is there some problem with keeping
;; internal state consistent?  If this is the case, then scheduling should be
;; inhibited on the Lispm as well.
;; Who's comment? It is obvious that there is this global array LINEARRAY,
;; which gets bashed during DISPLA. Seems like the best thing to do is
;; to use AREF and ASET on a special variable bound to an array pointer.
;; If a reentrant call to DISPLA is made, then just bind this variable
;; to a new array. -GJC
;; So it was written, so it shall be done, eventually.
;; Ah, got around to it... 9:32pm  Wednesday, 2 December 1981

(defmacro safe-print (print-form)
  ;;`(WITHOUT-INTERRUPTS (LET ((^W T)) ,PRINT-FORM))
  ;; Still can't figure out what the ^W is bound for. - GJC
  ;;	Answer: SAFE-PRINT is used when the user types <RETURN> to 
  ;;	--More Display?-- but has a WRITEFILE open.  In that case,
  ;;	you want to write out to the file but not to the TTY. - JPG
  #+pdp10 `(let ((^w t)) ,print-form)
  #-pdp10  print-form)

(defmacro lg-end-vector (x y) `(lg-draw-vector ,x ,y))



