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
(macsyma-module mformt)
(load-macsyma-macros mforma)

(eval-when (eval)
  (setq macro-expansion-use 'displace))


#+lispm
(record-source-file-name 'mformat-loop 'macro t)
;;macro expand the following!!

(def-mformat)
;;macro expansion of the (def-mformat)  --wfs
;;(PROGN 'COMPILE
;;       (DEFMACRO DEF-MFORMAT-OP (CHAR &REST BODY)
;;           `(+DEF-MFORMAT-OP || ,CHAR . ,BODY))
;;       (DEFMACRO DEF-MFORMAT-VAR (VAR VAL INIT)
;;           `(+DEF-MFORMAT-VAR || ,VAR ,VAL ,INIT))
;;       (DEFMACRO MFORMAT-LOOP (&REST ENDCODE)
;;           `(+MFORMAT-LOOP || . ,ENDCODE)))
#+lispm
(record-source-file-name 'def-format-var 'defmacro t)
;;see above-wfs


(def-mformat-var |:-FLAG|     nil t)
(def-mformat-var |@-FLAG|     nil t)
(def-mformat-var parameter   0  t)	; Who can read "~33,34,87A" ?
(def-mformat-var parameter-p nil t)
(def-mformat-var text       nil nil)
(def-mformat-var text-temp nil nil)
(def-mformat-var displa-p nil nil)
(def-mformat-var pre-%-p nil nil)
(def-mformat-var post-%-p nil nil)

#-pdp10
(defmfun check-out-of-core-string (sstring) sstring)

(defmacro push-text-temp ()
  '(if text-temp (setq text (cons (cons '(text-string) (nreverse text-temp))
			     text)
		  text-temp nil)))

(defmacro output-text ()
  '(progn (push-text-temp)
    (output-text* stream text displa-p pre-%-p post-%-p)
    (setq text nil displa-p nil pre-%-p nil post-%-p nil)))

(def-mformat-op (#\% #\&)
    (cond ((or text text-temp)
	   (setq post-%-p t)
	   ;; there is text to output.
	   (output-text))
	  (t
	   (setq pre-%-p t))))

(def-mformat-op #\M
    (push-text-temp)
  (let ((arg (pop-mformat-arg)))
    (and @-flag (atom arg) 
	 (setq arg (or (get arg 'op) arg)))
    (cond (|:-FLAG|
	   (push (cons '(text-string) (mstring arg)) text))
	  (t
	   (setq displa-p t)
	   (push arg text)))))

(def-mformat-op #\A
    (push-text-temp)
  (push (cons '(text-string) (exploden (pop-mformat-arg))) text))

(def-mformat-op #\S
    (push-text-temp)
  (push (cons '(text-string)
	      (mapl #'(lambda (c)
			(rplaca c (getcharn (car c) 1)))
		    (explode (pop-mformat-arg))))
	text))

(defmfun mformat n
  (or (> n 1)
      ;; make error message without new symbols.
      ;; This error should not happen in compiled code because
      ;; this check is done at compile time too.
      (maxima-error 'wrng-no-args 'mformat))
  (let* ((stream (arg 1))
	 (sstring (exploden (check-out-of-core-string (arg 2))))
	 (arg-index 2))
					;(or (eql (car sstring) #\&) (push #\& sstring))
	
    (and (or (null stream)
	     (eq t stream))
	 (setq stream *standard-output*))
    ;; This is all done via macros to save space,
    ;; (No functions, no special variable symbols.)
    ;; If the lack of flexibilty becomes an issue then
    ;; it can be changed easily.
    (mformat-loop (output-text))
    ;; Keep from getting bitten by buffering.
    (force-output stream)
    ))

;;can't change mformat since there are various places where stream = nil means
;; standard output not a string  
;;note: compile whole file, incremental compiling will not work.


(defmfun aformat n
  (or (> n 1)
      ;; make error message without new symbols.
      ;; This error should not happen in compiled code because
      ;; this check is done at compile time too.
      (maxima-error 'wrng-no-args 'mformat))
  (let ((stream (arg 1))
	(sstring (exploden (check-out-of-core-string (arg 2))))
	(arg-index 2))
    #+nil
    (and (or (null stream)
	     (eq t stream))
	 (setq stream *standard-output*))

    (cond((null stream)
	  (with-output-to-string (stream)
	    (mformat-loop (output-text))))
	 (t (mformat-loop (output-text))))
    ;; This is all done via macros to save space,
    ;; (No functions, no special variable symbols.)
    ;; If the lack of flexibilty becomes an issue then
    ;; it can be changed easily.
    #+multics
    (force-output stream)
    ))


(defun output-text* (stream text displa-p pre-%-p post-%-p)
  (setq text (nreverse text))
  ;; outputs a META-LINE of text.
  (cond (displa-p (displaf (cons '(mtext) text) stream))
	(t
	 (if pre-%-p (terpri stream))
	 (do ()
	     ((null text))
	   (do ((l (cdr (pop text)) (cdr l)))
	       ((null l))
	     (tyo (car l) stream)))
	 (if post-%-p (terpri stream)))))

(defun-prop (text-string dimension) (form result)
  ;; come up with something more efficient later.
  (dimension-atom (maknam (cdr form)) result))

(defmfun displaf (object stream)
  ;; for DISPLA to a file. actually this works for SFA's and
  ;; other streams in maclisp.
  #-(or cl nil)
  (if (eq stream nil)
      (displa object)
      (let ((|^R| t)
	    (|^W| t)
	    (outfiles (ncons stream))
	    )
	(displa object)))
  #+(or cl nil)
  ;; a bit of a kludge here. ^R and ^W still communicate something
  ;; to the displa package, but OUTFILES has not been implemented/hacked.
  (if (or (eq stream nil)
	  (eq stream *standard-output*))
      (displa object)
      (let ((*standard-output* stream)
	    (|^R| t)
	    (|^W| t))
	(displa object))))

(defmfun mtell (&rest l)
  (apply #'mformat nil l))


;; Calling-sequence optimizations.
#+pdp10
(progn 'compile
       (let ((x (getl 'mformat '(expr lsubr))))
	 (remprop '*mformat (car x))
	 (putprop '*mformat (cadr x) (car x)))
       (declare (*lexpr *mformat))
       (defmfun *mformat-2 (a b) (*mformat a b))
       (defmfun *mformat-3 (a b c) (*mformat a b c))
       (defmfun *mformat-4 (a b c d) (*mformat a b c d))
       (defmfun *mformat-5 (a b c d e) (*mformat a b c d e))

       (let ((x (getl 'mtell '(expr lsubr))))
	 (remprop '*mtell (car x))
	 (putprop '*mtell (cadr x) (car x)))
       (declare (*lexpr *mtell))
       (defmfun mtell1 (a)         (*mtell a))
       (defmfun mtell2 (a b)       (*mtell a b))
       (defmfun mtell3 (a b c)     (*mtell a b c))
       (defmfun mtell4 (a b c d)   (*mtell a b c d))
       (defmfun mtell5 (a b c d e) (*mtell a b c d e))
       )



