;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module merror)

;;; Macsyma error signalling. 
;;; 2:08pm  Tuesday, 30 June 1981 George Carrette.

(defmfun $error (&rest l)
  "Signals a Maxima user error."
  (apply #'merror (fstringc l)))

(defmfun $warning (&rest l)
  "Signals a Maxima warning."
  (apply #'mwarning l))

(defmvar $error_size 60.
  "Expressions greater in SOME size measure over this value
  are replaced by symbols {ERREXP1, ERREXP2,...} in the MAXIMA-ERROR
  display, the symbols being set to the expressions, so that one can
  look at them with expression editing tools. The default value of
  this variable may be determined by factors of terminal speed and type.")

(defun error-size (exp)
  ; Call SPECREPCHECK on the argument in case it's a specrep. Ugh.
  ; But this simplifies its argument, which is a no-no if we got here
  ; because some simplification code is complaining, so inhibit simplification. Double ugh.
  (let (($simp nil))
    (declare (special $simp))
    (setq exp (specrepcheck exp)))

  (if (atom exp)
      0
      (do ((l (cdr exp) (cdr l))
	   (n 1 (1+ (+ n (error-size (car l))))))
	  ((or (atom l)
	       ;; no need to go any further, and this will save us
	       ;; from circular structures. (Which the display
	       ;; package would have a hell of a time with too.)
	       (> n $error_size))
	   n))))

;;; Problem: Most macsyma users do not take advantage of break-points
;;; for debugging. Therefore they need to have the error variables
;;; SET (as the old ERREXP was), and not PROGV bound. The problem with
;;; this is that recursive errors will bash the old value of the
;;; error variables. However, since we do bind the value of the
;;; variable $ERROR, calling the function $ERRORMSG will always
;;; set things back. It would be better to bind these variables,
;;; for, among other things, then the values could get garbage
;;; collected.

(define-condition maxima-$error (error)
  ((message :initform $error :reader the-$error))
  (:documentation "Muser error, to be signalled by MERROR, usually.")
  (:report (lambda (c stream)
	     (declare (ignore c))
	     (let ((*standard-output* stream))
	       ($errormsg)))))

(defvar *merror-signals-$error-p* nil
  "When T, MERROR will signal a MAXIMA-$ERROR condition.")

;; Sample:
;; (defun h (he)
;;   (merror "hi there ~:M and ~:M" he he))
;; This will signal a MAXIMA-$ERROR condition:
;; (with-$error (h '$you))

(defmacro with-$error (&body body)
  "Let MERROR signal a MAXIMA-$ERROR condition."
  `(let ((*merror-signals-$error-p* t))
     (declare (special *merror-signals-$error-p*))
     ,@body))

(defun merror (sstring &rest l)
  (setq $error `((mlist simp) ,sstring ,@ l))
  (cond (*merror-signals-$error-p*
	 (error 'maxima-$error))
	((eq *mdebug* '$lisp)
	 ; Go immediately into the lisp debugger
	 (let ((*debugger-hook* nil))
	   (invoke-debugger (make-condition 'maxima-$error))))
	(*mdebug*
	 (let ((dispflag t) ret)
	   (declare (special dispflag))
	   (when $errormsg
	     ($errormsg))
	   (format t (intl:gettext " -- an error.  Entering the Maxima debugger.~%~
                       Enter ':h' for help.~%"))
	   (progn
	     (setq ret (break-dbm-loop nil))
	     (cond ((eql ret :resume)
		    (break-quit))))))
	(errcatch
	 (when $errormsg
	   ($errormsg))
	 (error 'maxima-$error))
	(t
	 (when $errormsg
	   ($errormsg))
	 (fresh-line *standard-output*)
	 ($backtrace 3)
	 (format t (intl:gettext "~& -- an error. To debug this try: debugmode(true);~%"))
	 (finish-output)
	 (throw 'macsyma-quit 'maxima-error))))

(defun mwarning (&rest l)
  (format t "Warning: ~{~a~^ ~}~%" (mapcar #'$sconcat l)))

(defmvar $error_syms '((mlist) $errexp1 $errexp2 $errexp3)
  "Symbols to bind the too-large `maxima-error' expressions to"
  :properties ((assign 'assign-symbols)))

(defun assign-symbols (var val)
  (if (not (and ($listp val)
		(do ((l (cdr val) (cdr l)))
		    ((null l) (return t))
		  (if (not (symbolp (car l))) (return nil)))))
      (merror (intl:gettext "assignment: assignment to ~M must be a list of symbols; found: ~M")
	      var val)))

(defun process-error-argl (l)
  ;; This returns things so that we could set or bind.
  (do ((error-symbols nil)
       (error-values nil)
       (new-argl nil)
       (symbol-number 0))
      ((null l)
       (list (nreverse error-symbols)
	     (nreverse error-values)
	     (nreverse new-argl)))
    (let ((form (pop l)))
      (cond ((> (error-size form) $error_size)
	     (incf symbol-number)
	     (let ((sym (nthcdr symbol-number $error_syms)))
	       (cond (sym
		      (setq sym (car sym)))
		     (t
		      (setq sym (intern (format nil "~A~D" '$errexp
						symbol-number)))
		      (tuchus $error_syms sym)))
	       (push sym error-symbols)
	       (push form error-values)
	       (push sym new-argl)))
	    (t
	     (push form new-argl))))))

(defmfun $errormsg ()
  "errormsg() redisplays the maxima-error message while in a `maxima-error' break."
  ;; Don't optimize out call to PROCESS-ERROR-ARGL in case of
  ;; multiple calls to $ERRORMSG, because the user may have changed
  ;; the values of the special variables controlling its behavior.
  ;; The real expense here is when MFORMAT calls the DISPLA package.
  (let ((the-jig (process-error-argl (cddr $error))))
    (mapc #'(lambda (v x) (setf (symbol-value v) x)) (car the-jig) (cadr the-jig))
    (fresh-line)
    (let ((errset nil))
      (if (null (errset
		 (apply #'mformat nil
			(cadr $error) (caddr the-jig))))
	  (mtell (intl:gettext "~%** error while printing error message **~%~A~%")
		 (cadr $error)
		 )))
    (fresh-line))
  '$done)

;; RAT-ERROR (function)
;;
;; Throw to the nearest enclosing RAT-ERR tag (set by IGNORE-RAT-ERROR or
;; RAT-ERROR-TO-MERROR). If ERROR-ARGS is nonzero, they are thrown. The
;; RAT-ERROR-TO-MERROR form applies the MERROR function to them.
;;
;; The obvious way to make RAT-ERROR work is to raise a condition. On the lisp
;; implementations we support other than CMUCL, this runs perfectly
;; fast. Unfortunately, on CMUCL there's a performance bug which turns out to be
;; very costly when you raise lots of the condition. There are lots and lots of
;; rat-error calls running the test suite (10s of thousands), and this turns out
;; to be hilariously slow.
;;
;; Thus we do the (catch .... (throw .... )) thing instead. Other error handling
;; should be able to use conditions with impunity: the only reason that the
;; performance was so critical with rat error is the sheer number of them that
;; are thrown.
(defun rat-error (&rest error-args)
  (throw 'rat-err error-args))

;; IGNORE-RAT-ERR
;;
;; Evaluate BODY with the RAT-ERR tag set. If something in BODY throws to
;; RAT-ERR (happens upon calling the RAT-ERROR function), this form evaluates to
;; NIL.
(defmacro ignore-rat-err (&body body)
  (let ((result (gensym)) (error-p (gensym)))
    `(let ((,result) (,error-p t))
       (catch 'rat-err
         (setf ,result (progn ,@body))
         (setf ,error-p nil))
       (unless ,error-p ,result))))

(defmacro rat-error-to-merror (&body body)
  (let ((result (gensym)) (error-args (gensym)) (error-p (gensym)))
    `(let ((,result) (,error-p t))
       (let ((,error-args
              (catch 'rat-err
                (setf ,result (progn ,@body))
                (setf ,error-p nil))))
         (when ,error-p
           (apply #'merror ,error-args)))
       ,result)))

;;; The user-error function is called on "strings" and expressions.
;;; Cons up a format string so that $ERROR can be bound.
;;; This might also be done at code translation time.
;;; This is a bit crude.

(defun fstringc (l)
  (do ((sl nil) (s) (sb)
       (se nil))
      ((null l)
       (setq sl (maknam sl))
       (cons sl (nreverse se)))
    (setq s (pop l))
    (cond ((stringp s)
	   (setq sb (mapcan #'(lambda (x)
				(if (char= x #\~)
				    (list x x)
				    (list x)))
			    (coerce s 'list))))
	  (t
	   (push s se)
	   (setq sb (list #\~ #\M))))
    (setq sl (nconc sl sb (if (null l) nil (list #\space))))))
