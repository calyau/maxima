;;;;;; -*- Mode: LISP; Package: zwei; Base: 8; Syntax: Zetalisp -*-;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  (c) Copyright 1976, 1983 Massachusetts Institute of Technology      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(import '(sloop::SLOOP lisp::multiple-value-setq) 'zwei)


;;note in zwei *standard-output* for the moment *standard-output* replace later.
#+ti
(defflavor macsyma-mode nil nil )

;;the (KM) is so that the region will be marked.
(defcom com-forward-macsyma-expression "Forward over one expression"
	(KM -R) 
 (move-bp (point)  (forward-macsyma-expression (point) 1))
  dis-bps)

;;until get zwei into cl.

(defmacro lisp-eval (form &optional nohook)
   nohook
  `(#+ti cli:eval #-ti eval ,form))

(defun get-maxima-package ()
  #+cl (#+symbolics cl:find-package #-symbolics find-package 'maxima)
  #-cl (pkg-find-package 'macsyma))

(defcom com-backward-macsyma-expression "backward over one expression"
	(KM -R)
  (let	((point (point)))
    (move-bp point (forward-macsyma-expression point -1))
    dis-bps))

(defun forward-macsyma-expression
       (bp &optional (times 1)
	fixup-p (level 0) stop-bp (move-over-singlequotes-p t) n-up-p (signal-error t)
	&aux bp1 new-line ;(region? (window-mark-p *window*))
	)
  (setq bp (copy-bp bp))
  (sloop for i from 0 below (abs times)
	do (cond ((and (> times 0) (member (bp-char bp) *maxima-form-delimiters*))
		  (setq bp (forward-char bp times t))))
	    (cond ((zerop times) (copy-bp bp))
		  (t
		   (setq bp1 (macsyma-skip-comments bp (< times 0) stop-bp))
		   (setq new-line (forward-sexp
				    bp1 (signum times)
				    fixup-p level stop-bp
				    move-over-singlequotes-p n-up-p))
		   (cond (new-line 
			  (setq bp1 (setq bp1 new-line)))
			 (t
			  (and signal-error(barf)) (return bp1)))))
	    finally   (return bp1))
   )


(defun macsyma-skip-comments (bp &optional reversep stop-bp &aux  (found-comment t))
  (sloop while found-comment
	do (multiple-value-setq
	     (bp found-comment)
             (macsyma-skip-comment bp reversep stop-bp)))
  bp)

;;a terrible kludge, which won't work if strings are half in comments etc.
(defun in-macsyma-string (bp)
       (MULTIPLE-VALUE-BIND (IN-STRING SLASHIFIED IN-COMMENT)
	   (LISP-BP-SYNTACTIC-CONTEXT (FORWARD-CHAR BP -1))
	  in-string))


(defmacro show (&rest l)
  (sloop for x in l
	collecting 
	`(format *typein-window* "~%Value of ~A is ~A." ',x ,x)
	 into bod
	finally (return `(prog ((default-cons-area working-storage-area))
				,@ bod))))
#+ti
(defmacro with-common-lisp (&body body)
  `(let ((si:*READTABLE*     si:COMMON-LISP-READTABLE)
	(si:*READER-SYMBOL-SUBSTITUTIONS*    si:*COMMON-LISP-SYMBOL-SUBSTITUTIONS*)
	(si:*lisp-mode* :common-lisp))
    ,@ body))

#-ti  ;;what should be done here??
(defmacro with-common-lisp (&body body)
  `(progn ,@ body))

;for release 5 we need to use setf to assign the symbol-macro *default-major-mode*

(defmacro with-marked-region (&body body)
  `(let ((bp1 ())
	(bp2 ()))
    (cond ((window-mark-p *window*)
	    (setq bp1 (mark) bp2 (point)))
	  ((mark-current-macsyma-form)
	   (setq bp1 (copy-bp (mark))  bp2 (copy-bp (point))))
;	  	   (setq bp1 (mark)  bp2  (point)))
	  (t (barf "~&~a" "No region")))
    ,@ body))


(defcom com-macsyma-evaluate-region
  "Call the Macsyma evaluator on a region in the buffer. With a
  numeric arg different from 2 let the region be from the beginning of the current
  line to the next $ or ;, otherwise a region that has been marked,
  or if no region then between the last and next $ or ;.  Thus without a numeric arg
  of 2 one one need not have a $ after the previous form, but parsing will begin
  at the beginning of the current line and continue to the  next $."
 
	     ()
  (let ((bp1 ())
	(bp2 ()))
    (cond ((not (eql *numeric-arg* 1))
           (setq bp1 (copy-bp (point)))
	   (setf (bp-index bp1) 0)
	   (setq bp2 (zwei::search-set bp1 *maxima-form-delimiters*
						nil))
	   (zwei::move-bp (point) bp2))
	  ((window-mark-p *window*)
	   (setq bp1 (mark) bp2 (point)))
	  ((mark-current-macsyma-form)
	   (setq bp1 (mark) bp2 (point)))
	  (t (barf "~&~a" "No region")))
      (macsyma-evaluate-for-editor bp1 bp2 "Region"))
  dis-text)

(defcom com-macsyma-compile-region
	     "Call the Macsyma evaluator on a region in the buffer and~
              then translate and compile the result."
	     ()
      (with-marked-region (macsyma-compile-for-editor bp1 bp2  "Region"))
  dis-all)

(defcom com-macsyma-translate-region
	     "Call the Macsyma evaluator on a region in the buffer and  and translate the result. ~
             with a numeric argument bigger than 1 it displays the translated result
             and less than 1 it grinds it into the buffer" 
	     ()
 (with-marked-region (macsyma-translate-for-editor bp1 bp2 "Region"))
  dis-all)

(defcom com-macsyma-evaluate-buffer 
	     "Call the Macsyma evaluator on the entire buffer"
	    ()
  (macsyma-evaluate-for-editor (interval-first-bp *interval*) 
			       (interval-last-bp *interval*)
			       "Buffer")
  dis-none)

(defcom com-macsyma-compile-buffer 
	     "Call the Macsyma evaluator on the entire buffer"
	    ()
  (macsyma-compile-for-editor (interval-first-bp *interval*) 
			       (interval-last-bp *interval*)
			       "Buffer")
  dis-none)

(defcom com-macsyma-translate-buffer 
	     "Call the Macsyma evaluator on the entire buffer"
	    ()
  (macsyma-translate-for-editor (interval-first-bp *interval*) 
			       (interval-last-bp *interval*)
			       "Buffer")
  dis-none)




(defmacro move-on-read-error (str &body body)
  #+ti
     (condition-case (condit)
	`(progn  ,@ body)
       (fs:parse-error (beep)
		       (format *typein-window* (send condit :format-string))))
     #+symbolics
     `(move-point-on-read-error #+genera (,str) ,@ body))
     

#+ti
(defmacro with-possible-read-error ( body what-am-i-evaluating)
  `(let (read-error)
     (funcall 'prompt-line "~&Evaluating ~A" ,what-am-i-evaluating)
     (condition-case (condit)
	 ,body
       (fs:parse-error (beep) (setq read-error t)
;	(point-pdl-push (send editor-stream :read-bp) *window*)
;		       (move-bp bp2 (setq li (send editor-stream :read-bp)))
       (format *typein-window* (send condit :format-string))))
     (cond ((null read-error)
	    (funcall 'typein-line "~&~A Evaluated" ,what-am-i-evaluating)))))

#+ti
(defun macsyma-evaluate-for-editor (bp2 bp1 what-am-i-evaluating)
    (let ((package (get-maxima-package))
	  (editor-stream
	    (INTERVAL-STREAM bp2 bp1 ()))
	  (eof (list nil)))
       (with-possible-read-error
      (sloop for expr = (cl-macsyma::mread editor-stream eof)
	    while (not (equal expr    eof))
	    do	    (cl-macsyma::meval* (caddr expr))) what-am-i-evaluating)
    ))
#-ti
(defun macsyma-evaluate-for-editor (bp2 bp1 what-am-i-evaluating)

    (funcall 'prompt-line "~&Evaluating ~A" what-am-i-evaluating)
    (let ((package(get-maxima-package)
		  		   )
	  (editor-stream
	    (INTERVAL-STREAM bp2 bp1 ()))
	  (eof (list nil)))
      (or (eql  package (cl:find-package 'cl-maxima))
	  (error "Set package to maxima package"))
       (move-on-read-error editor-stream
      (sloop for expr = (cl-macsyma::mread editor-stream eof)
	    while (not (equal expr eof))
	    do	    (cl-macsyma::meval* (caddr expr))))
    (funcall 'typein-line "~&~A Evaluated" what-am-i-evaluating)
    ))

(defcom com-macsyma-display-on-typeout
	"Displays current expression or region in scroll down  window (standard-output)"
	()
  (with-marked-region
    (let ((stream (interval-stream bp2 bp1 nil))
	  (eof (list nil)))
      (sloop for expr = (cl-macsyma::mread stream eof)
	    while (not (equal expr    eof))
	    do (cl-macsyma::displa (setq cl-macsyma::$% (cl-macsyma::meval* (third expr))))
	    finally (com-down-real-line))))
  dis-none)

 
(defmacro macsyma-apply-into-buffer (bp2 bp1 what-am-i-evaluating expr &rest body)
  "The expr will run thru the cl-macsyma::mread of things in the region bp2
   to bp1.  Usually you want the (caddr expr) to get the actual macsyma
   expression minus the cl-macsyma::displa output etc."
  `(let ((window (car (send current-process :run-reasons))))
     (setq cl-macsyma::linel (- (send window :size-in-characters) 3))

     (funcall #'prompt-line "~&Evaluating ~A" ,what-am-i-evaluating)
     (let ((package (get-maxima-package))
	   (stream
	     (interval-stream ,bp2 ,bp1 ()))
	   (eof (list nil)))
       (move-on-read-error
	 stream
	 (sloop for ,expr = (cl-macsyma::mread stream eof)
		while (not (eq ,expr eof))
		do
		(with-editor-stream (standard-output :window *window*
						     :start :point)
				    ,@ body)
		(funcall #'typein-line "~&~A Evaluated" ,what-am-i-evaluating))))))

(defcom com-macsyma-display-into-buffer
	"Displays current expression or region into buffer"
	() 
  (let ((cl-maxima::$linedisp nil)(cl-maxima::$cursordisp nil)(cl-maxima::^w t)
	(cl-maxima::^r t  )(cl-maxima::$display2d t) to-display
	(package (get-maxima-package)))
    (with-marked-region
      (let ((stream (interval-stream bp2 bp1 nil)))
	(sloop with eof = (list nil)
	      for expr = (cl-maxima::mread stream eof)
	      until (equal expr eof)
	      collecting (third expr) into for-display
	      finally (setq to-display for-display))))
    (with-editor-stream
      (standard-output :window *window*
		       :start :point)
      (sloop for v in to-display
	    do       (format t "~%")
		     (cl-maxima::displa (cl-maxima::meval* v))
		     (format t "$"))))
  dis-all)


(defun zdel (&optional n)
 (let ((*numeric-arg* n))
  (com-delete-forward)))

(defun zfor (&optional ( n 1))
   (let ((*numeric-arg* n))
  (com-forward)))

(defun zbac (&optional (n 1))
   (let ((*numeric-arg* n))
  (com-backward)))

(defun zrub (&optional ( n 1))
   (let ((*numeric-arg* n))
  (com-tab-hacking-rubout)))

(defun zsearch (sstring &optional (reverse nil) (how-many 1) &aux point)
  "searches forward and moves to end of sstring (or back if reverse is t )"
  (sloop for i below how-many
	do
  (setq point (search (point) sstring reverse  ))
  (move-bp (point) point)))

;(defun kill-return ()
;  (zsearch (string 215) nil)
;  (zrub 1))

(defun macsyma-evaluate-with-arrow (bp2 bp1 what-am-i-evaluating &aux me label (dollars 1))
  (macsyma-apply-into-buffer
    bp2 bp1 what-am-i-evaluating expres
    (setq me (caddr expres))
    (cond ((symbolp me)
	   (format standard-output ":")
	   (cond ((member me cl-macsyma::$labels)
		  (cond ((string-search "c" (string me))
			 (incf dollars)
			 (cl-maxima::i-$grind me )
			 
			 (incf cl-maxima::$edlinenum)
			 (setq label   (cl-maxima::$new_concat '$ed cl-maxima::$edlinenum))
			 (format standard-output "  ==>~80T $")
			 (format standard-output "~%ED~A:" cl-maxima::$edlinenum)))))
	   (cl-maxima::i-$grind (cl-maxima::meval* me) ))
	  (t (format standard-output "  ==> ~80T  $")
	     (incf  cl-maxima::$edlinenum)
	     (setq label   (cl-maxima::$new_concat '$ed cl-maxima::$edlinenum))
	     (format standard-output "~%ED~A:" cl-maxima::$edlinenum)
	     (cl-maxima::i-$grind (cl-maxima::meval* me)) nil))
    (cond (label (set label me) (incf dollars)))
    (zrub 1)
						;(zsearch (string 215) )
						;(zrub 1)
    (zsearch "$" nil dollars)
    (format t "~%~% ")))


;(defvar *my-stream* (make-array 200 :type 'art-string :fill-pointer 0))

(defcom com-macsyma-parse-string-into-buffer
	"parses string and inserts into buffer"
  ()

    (let ( expr (stream (interval-stream (mark) (point) ())))
        (move-on-read-error
	  stream
	  (setq expr (caddr (cl-macsyma::mread stream)))
	  (with-editor-stream (standard-output :window *window*
					       :start :point)
			      (format t "`~A" expr))))
  dis-text)



(defcom com-macsyma-parse-region-into-buffer
	"parses region string and inserts into buffer"
  ()
    (let ( expr (stream (interval-stream (mark) (point) ())))
  (move-on-read-error
    stream
    (setf (fill-pointer *my-stream*) 0)
    (with-output-to-string  (st *my-stream*  )
	
			    (stream-copy-until-eof stream  st))
    (with-editor-stream (standard-output :window *window*
					 :start :mark)
			(setq expr (cl-maxima::parse-string *my-stream*))
			(format t "`~A ; " expr ))))
    
	  dis-text)


(defcom com-evaluate-with-arrow
	"Evaluates into buffer with arrow"
  ()
  (with-marked-region (macsyma-evaluate-with-arrow bp1 bp2 "region"))
			      dis-all)

	
(defun kill-region (bp1 bp2 ignore)
  (kill-interval bp1 bp2 t t t))

(defcom com-macsyma-grind-expression
	     "Grinds current expression or region into buffer"
         ()
	(with-marked-region (macsyma-evaluate-into-buffer1 bp1 bp2 "region"))
	(with-marked-region (kill-region bp1 bp2 'ignore))
	dis-all)

;;;the following work but have abbreviations elsewhere

(defcom com-macsyma-evaluate-into-buffer 
  "Evaluates the current Macsyma  expression or region into the end of buffer
 With a numeric arg > 1 it puts any output during  the evaluation into the buffer as well"
  () 
  (let ((bp1 ()) cl-macsyma::$display2d
	(bp2 ()))
    (cond  ((window-mark-p *window*)
	   (setq bp1 (mark) bp2 (point)))
	  ((mark-current-macsyma-form)
	   (setq bp1 (mark) bp2 (point)))
	  (t (barf "~&~a" "No region")))
      (macsyma-evaluate-into-buffer1 bp1 bp2 "Region"))
  dis-text)

(defun macsyma-evaluate-into-buffer1 (bp2 bp1 what-am-i-evaluating &aux val)
    (funcall #'prompt-line "~&Evaluating ~A" what-am-i-evaluating)
    (let ((package (get-maxima-package))
	  (stream
	    (INTERVAL-STREAM bp2 bp1 ()))
	  (eof (list nil)))
  (move-on-read-error
    stream
      (sloop for expr = (cl-macsyma::mread stream eof)
	    while (not (equal expr    eof))
	    do
	    (with-editor-stream (str :window *window*
				     :start :point)
	      (format str "~%")
	      (cond ((and (boundp '*numeric-arg*)
			  (> *numeric-arg* 1))
		     (let ((standard-output str))
		       (setq val (cl-macsyma::meval* (caddr expr)))))
		    (t  (setq val (cl-macsyma::meval* (caddr expr)))))
	      (cl-macsyma::mgrind val				 str )
	      (format str "$")
	      )))
    (funcall #'typein-line "~&~A Evaluated" what-am-i-evaluating)))

(defvar *grind-definition-only* t)

(defmacro bind-and-translate-form  (to-transl)
  `(let (( cl-maxima::*translation-msgs-files* *typein-window*))
     (cl-maxima::bind-transl-state
       (setq cl-maxima::*in-translate-file* t)
       (cond(function-to-translate (cl-maxima::tr-mdefine-toplevel ,to-transl))
	    (t (cl-maxima::translate-macexpr-toplevel ,to-transl))))))
	       

(defun macsyma-translate-for-editor (bp2 bp1 what-am-i-evaluating
				     &aux transl to-transl function-to-translate)
  (with-common-lisp
    (funcall #'prompt-line "~&Evaluating ~A" what-am-i-evaluating)
    (let ((package (get-maxima-package))
	  (stream (INTERVAL-STREAM bp2 bp1 ()))
	  (eof (list nil)))
    (move-on-read-error
      stream
      (sloop for expr = (cl-maxima::mread stream eof)
	    while (not (equal expr    eof))
	       do
	(setq function-to-translate nil)
						;(cl-maxima::meval* (caddr expr))
						;     (show (third expr))
	(setq to-transl (third expr))
	(cond ((member (caar to-transl) '(cl-maxima::mdefine cl-maxima::mdefmacro))
	       (setq function-to-translate (caar (second to-transl)))))
	(and function-to-translate
	     (funcall #'prompt-line "~&translating ~A" function-to-translate))
	(setq transl (bind-and-translate-form to-transl))

	(lisp-eval transl)
        (cond ((and *grind-definition-only*
	       function-to-translate)
	       (setq transl (fifth transl))))
	(cond ((> *numeric-arg* 1)
	       (pprint transl))
	      ((< *numeric-arg* 1)
	       (terpri stream)
               (pprint transl nil stream)
	       (princ '$ stream)
	      (return nil)))
      (funcall #'prompt-line "~&Translated ~A" function-to-translate))))))

(defun macsyma-compile-for-editor (bp2 bp1 what-am-i-evaluating
				   &aux function-to-translate to-transl transl
				   rule-to-translate)
    (funcall #'prompt-line "~&Evaluating ~A" what-am-i-evaluating)
    (let ((package(get-maxima-package))
;	 (package  (send *interval* :get ':package);(get-maxima-package)
;		    )
	  (stream (INTERVAL-STREAM bp2 bp1 ()))
	  (eof (list nil)))
  (move-on-read-error
    stream      
      (sloop for expr = (cl-macsyma::mread stream eof)
	    while (not (equal expr    eof))
	    do (setq function-to-translate nil)
	       (setq to-transl (third expr))
	       (cond ((member (caar to-transl) '(cl-maxima::mdefine  ))
		      (setq function-to-translate (caar (second to-transl)) ))
		     ((member (caar to-transl) '(cl-maxima::$defrule  ))
		      (setq rule-to-translate  (second to-transl)) ))
	       (setq transl (bind-and-translate-form to-transl))
	       (lisp-eval transl)
	       (cond (function-to-translate
		      (funcall #'prompt-line "~&compiling ~A"  function-to-translate)
		      (compiler:compile function-to-translate)
		      (and function-to-translate
			   (prompt-line "~&Compiled ~A" function-to-translate))))
	       (cond (rule-to-translate (compiler:compile rule-to-translate)))
;	       (show cl-maxima::forms-to-compile-queue)
	       (compile-forms cl-maxima::forms-to-compile-queue)
	       (setq cl-maxima::forms-to-compile-queue nil)))))

(defun compile-forms (list)
  (sloop for v in list
	when (and v (listp v))
	 do
	   (#+symbolics
	    cl:case #-symbolics case
	    (car v)
	     (defun (compile (second v) v))
	     (deprop (eval v))
	     (defmacro (compile (second v) v))
	     (macro  (compile (second v) v))
	     (t (lisp-eval v)))))

(defcom com-macsyma-translate-region-to-buffer
	     "Call the Macsyma evaluator on a region in the buffer and  and translate the result and grind." 
	     ()
  (let ((bp1 ())
	(bp2 ()))
    (cond ((window-mark-p *window*)
	   (setq bp1 (mark) bp2 (point)))
	  ((mark-current-macsyma-form)
	   (setq bp1 (mark) bp2 (point)))
	  (t (barf "~&~a" "No region")))
      (macsyma-translate-to-buffer bp1 bp2 "Region"))
  dis-all)

(defun macsyma-translate-to-buffer (bp2 bp1 what-am-i-evaluating
					    &aux function-to-translate)
    (funcall #'prompt-line "~&Evaluating ~A" what-am-i-evaluating)
    (let ((package (get-maxima-package))
	  (stream (INTERVAL-STREAM bp2 bp1 ()))
	  (eof (list nil))
	  (temp *default-major-mode*))
  (move-on-read-error stream
      
      (setf *default-major-mode* :lisp) 
      (with-editor-stream (standard-output ':buffer-name  "Translated")
	
	(sloop for expr = (cl-macsyma::mread stream eof)
	      while (not (equal expr    eof))
	      do (cl-macsyma::meval* (caddr expr)) (SETQ FUNCTION-TO-TRANSLATE
					     (FIRST (FIRST (SECOND (THIRD EXPR)))))
	      (funcall #'prompt-line "~&translating ~A" function-to-translate)
	      (CL-MACSYMA::MEVAL* `(($TRANSLATE) ,FUNCTION-TO-TRANSLATE))
	      (pprint `(defprop ,function-to-translate ,t translated))
	      (pprint `(add2lnc ,`(,function-to-translate ) $props))
	      (let ((fdef (symbol-function function-to-translate)))
		(pprint
		(append `(defmtrfun ,`(,function-to-translate
				      	    $any mdefine nil nil))
			(cddr fdef))))
	      (format t "~&")))
            (funcall #'prompt-line
		     "&~Translated ~A in buffer: Translated" function-to-translate)
    (setf *default-major-mode* temp))))
    
(defun skip-back-over-whitespace ()
  (sloop for char = (bp-char (point))
	until (not (member char *whitespace-chars*))
	do  (dbp (point))))

;;in release 5 at the beginning we needed the mode-settable-p property
;;but now in release 6 since some of these are symbol macros that is not necessary

(eval-when (load eval compile)
  (sloop for u in
	'(*LIST-SYNTAX-TABLE*
	   *MODE-LIST-SYNTAX-TABLE*
	   *atom-word-syntax-table*
	   *SPACE-INDENT-FLAG*   ;;these two are symbol-macros 
	   *PARAGRAPH-DELIMITER-LIST*
	   *COMMENT-COLUMN*
	   *COMMENT-START* 
	   *COMMENT-BEGIN* 
	   *COMMENT-END*)
	when (null (get u 'si:symbol-macro))
	do  (putprop u t 'mode-settable-p)
	else do (remprop u 'mode-settable-p)))

(defun reset-comtab-for-lisp ()
  "In case the comtab gets mucked by macsyma buffer eval this function
   to get going again in lisp buffer"
       (set-comtab *comtab* (list #\c-sh-e 'com-evaluate-region
			  #\c-sh-c 'com-compile-region)))


   
;si:
;(define-ie-command rh-macsyma-arglist (#\control-meta-a)
;   (cl-macsyma::macsyma-print-arglist tv:rubout-handler-buffer))

;;these need work for release 6 since the interactive-stream flavor is now
;;used.

;(DEFSTRUCT (INPUT-EDITOR-BUFFER (:TYPE :ARRAY-LEADER)
;				(:MAKE-ARRAY (:LENGTH 512. :TYPE ART-FAT-STRING))
;				(:DEFAULT-POINTER INPUT-EDITOR-BUFFER)
;				(:CONC-NAME IEB-))
;  (FILL-POINTER 0)
;  (SCAN-POINTER 0)
;  (TYPEIN-POINTER 0)
;  (NOISE-STRINGS NIL))

;;                    Input Editor Commands

;;Use define-input-editor-command to define and add a function which will be
;;on the main rubout handler command table used by macsyma listener.  The 
;;name of the buffer is tv:rubout-handler-buffer and tv:(rhb-typein-pointer) 
;;yields the current position.  Symbolics changed the names from "rubout-handler-"
;;to "input-editor-" and "rhb" to "ieb" and the package from tv to si.  How
;;helpful.
                              
#+symbolics
(defmacro define-input-editor-command (&rest l)
  (cond ((fboundp 'si:define-ie-command)
	 (setq l(subst  'sI:input-editor-buffer 'tv:rubout-handler-buffer l))
	 `(si:define-ie-command ,@ l))
	(t `(tv:define-rh-command ,@ l))))

#+ti
(defmacro define-input-editor-command (name keys &body body)
  `(progn 'compile
	  (defun ,name (ignore)
	       ,@ body)
	  (ucl:add-command ',name
			   '(:keys ,keys
				  :names ,(string name)
				  :command-flavor tv:rh-command)
			   'tv:rh-command-table t)))
 

(define-input-editor-command com-macsyma-arglist (#\control-meta-a)
   (macsyma-print-arglist tv:rubout-handler-buffer) )
 
(define-input-editor-command com-macsyma-documentation (#\control-meta-d)
  (macsyma-print-documentation tv:rubout-handler-buffer))

#+ti  ;;definition ok for symbolics but would a zetalisp string be a sequence??
(defun string-reverse-search-some (set strg &optional start)
  "set and strg are sequences and this gives the element position of the first (coming in reverse
   from star) occurence of and element of set in strg"
  (cond (start
	 (position-if #'(lambda (x)  (position x set)) strg :from-end t
		      :end start))
	(t  (position-if #'(lambda (x)  (position x set)) strg :from-end t))))

#-ti
(deff string-reverse-search-some #'string-reverse-search-set)

(defun macsyma-print-arglist (a-string &aux start fun end ignore)
  #+ti (setq a-string   (coerce a-string 'string)) ;; until rubout-handler-buffer is a string..
  (setq end (string-reverse-search-some "([" a-string #+ti (tv:rhb-typein-pointer ) #-ti (si:ieb-typein-pointer )))
  (setq start (string-reverse-search-some " (*+.-[" a-string end))
  (cond ((null start)(setq  start 0))
	(t (setq start (add1 start))))
  (send standard-input :fresh-line)
  (setq me (list start end a-string a-string #+ti (tv:rhb-typein-pointer ) #-ti (si:ieb-typein-pointer )))
  (setq fun  (intern (string-upcase
		       (string-append "$"
				      (substring a-string start end))) 'cl-maxima))

  ( #-ti si:ie-display-info
 #+ti tv:rh-display-info  #-ti (ignore ignore si:*numeric-arg-p* fun)
    (let ((arg))
      (cond ((and (sys:validate-function-spec fun)
		  (or (fdefinedp fun)
		      (and (symbolp fun)
			   (get fun 'compiled-only-arglist))))
	     (print-arglist fun standard-output))
	    ((setq arg (macsyma-arglist fun))
	     (format t "~A : ~A" fun (macsyma-arglist fun)))
	    (t (format t "Can't seem to find definition of ~A" fun))))))



(defun macsyma-print-documentation (a-string &aux start fun end)
  (declare (special tv:rubout-handler-buffer))
  #+ti (setq a-string (coerce a-string 'string))
  (setq end (string-reverse-search-some"([" a-string (tv:rhb-typein-pointer )))
  (setq start (string-reverse-search-some " (*+.-[" a-string end))
  (cond ((null start)(setq  start 0))
	(t (setq start (add1 start))))
  (send standard-input :fresh-line)
  (setq fun  (intern (string-upcase
		       (string-append "$"
				      (substring a-string start end))) 'macsyma))
  ( #-ti si:ie-display-info
   #+ti tv:rh-display-info  #-ti (ignore ignore tv:rh-numeric-arg-p fun)
    (let ((arg))
      (cond ((and (sys:validate-function-spec fun)
		  (or (fdefinedp fun)
		      (and (symbolp fun)
			   (get fun 'compiled-only-arglist))))
	     (format  standard-output (function-documentation fun)))
	    ((setq arg (macsyma-arglist fun))  
	     (format t "~A : ~A" fun (macsyma-arglist fun)))
	    (t (format t "Can't seem to MAXIMA-FIND definition of ~A" fun))))))


(DEFMACRO macsyma-LIST-SYNTAX (CHAR)
  `(CHAR-SYNTAX ,CHAR *macsyma-LIST-SYNTAX-TABLE*))

(DEFMETHOD (macsyma-mode :MATCHING-CHAR-TO-BLINK) (BP LIMIT-BP)
  (AND (= (macsyma-LIST-SYNTAX (BP-CHAR-BEFORE BP)) LIST-CLOSE)
       (MULTIPLE-VALUE-BIND (IN-STRING SLASHIFIED IN-COMMENT)
	   (LISP-BP-SYNTACTIC-CONTEXT (FORWARD-CHAR BP -1))
       	 (NOT (OR IN-STRING SLASHIFIED IN-COMMENT (in-macsyma-comment bp) )))
       ;;don't signal error if have to return nil
       (FORWARD-macsyma-expression BP -1 NIL 0 LIMIT-BP t nil nil)))



(DEFMACRO macsyma-LIST-SYNTAX (CHAR)
  `(CHAR-SYNTAX ,CHAR *macsyma-LIST-SYNTAX-TABLE*))

(defvar *Macsyma-MATCHING-DELIMITERS* "()[]{}")
(DEFUN Macsyma-OPENING-DELIMITER (CHAR)
  (SLOOP WITH STRING = *macsyma-MATCHING-DELIMITERS*
	FOR I FROM 1 BELOW (length (the string  STRING)) BY 2
	WHEN (EQL CHAR (AREF STRING I))
	RETURN (AREF STRING (f1- I))
	FINALLY (ERROR "~C has no tex matching delimiter" CHAR)))

(SETQ *MACSYMA-LIST-SYNTAX-TABLE* (MAKE-SYNTAX-TABLE *MACSYMA-LIST-SYNTAX-LIST*))

(defcom com-macsyma-describe
    "Searches the documentation for keys containing a given string 
    with a numeric argument different from 1 it puts the result in the current buffer."
      ()
  (let ((str (zwei::typein-line-read "String to describe")))
    (cl-macsyma::$describe str :editor (not (eql *numeric-arg* 1)))
    (cond ((not (eql *numeric-arg* 1))
	   (format t "~%Type a space to see return to the documentation in the buffer:"))))
  dis-all)

(defun non-trivial-lines (point &optional ( n 1) reversep
			  &aux  (str ""))
  (let* ((nex (cond (reversep #'line-previous)
		   (t #'line-next)))
	 (line  (car point)))
  (cond ((null reversep) (setq line (line-previous line))))
  (sloop while (setq line (funcall nex line))
	collecting line into lines
	when (not(eq 0 (length (the string  line))))
	count 1 into m
	while (< m n)
	finally (cond (reversep (setq lines(nreverse lines))))
		(sloop for v in lines
		      do (setq str (string-append str (format nil "~A~%" v)))))
  str))


(defvar *record-changes* nil)

(defun  get-record-changes-buffer ()
  (cond (*record-changes*)
	(t (with-editor-stream (str :buffer-name "Record Changes")
			       (format str ";;;*-changes - ~A" (time:print-current-time nil)))
	   (sloop for v in zwei::*zmacs-buffer-list*
		 when (string-search "record changes" v)
		 do (return (setq *record-changes*  v))))))

;;at present you must manually make *record-changes* by 
;;going to a buffer and setq *record-changes* to it.

						
(defvar *buffer-changes-recorded* nil)

#+symbolics

(defun record-changes (&optional bp1 bp2 &aux (m 3) an an2 an3)
  "Puts the current region into the buffer *record-changes* and records the surrounding information.  "
      (get-record-changes-buffer)
		(let((bp (copy-bp (interval-last-bp *record-changes*)))
			(name (or (and (send *interval* :pathname)
				       (send (send *interval* :pathname) :truename))
					(string *interval*))))
  (cond ((fboundp 'user:push-new)  (user:push-new *interval* *buffer-changes-recorded*)))
    (cond ((bp-< bp1 bp2) nil)
	  (t (swapf bp1 bp2)))
  
  (setq an
	(insert-moving bp (format nil
				  "~%;;****************~%~%;;From file ~A~%~%~
                                   ;;****preceding ~A lines***~%#|~A|#~%;;**BEGIN REPLACEMENT TEXT:~%"
			   name m
		       (non-trivial-lines bp1 M t)
		       )))

 (setq an2   (insert-interval-moving an bp1 bp2 t))

   (setq an3 (insert-moving an2 (format nil "~%;;**END REPLACEMENT.~%;;****subsequent ~A lines***~
                                             ~%#|~A|#~%~%" m
		       (non-trivial-lines bp2 m nil))))
 
   (let ((*interval* *record-changes*))  (move-bp bp an3)
	)))

#+symbolics

(defcom com-record-changes
  "Records changes of the current region into a buffer called record changes"
  ( )
  (let (bp1 bp2 error-p definition-name node)
    (COND ((WINDOW-REGION-P) (format t "hi")
	   ;; there is a region, use it.
	   (SETQ BP1 (MARK) BP2 (POINT))
	   (cond ((BP-< BP1 BP2) nil)
		 (t(swapf  BP1 BP2)))
	   (SETQ DEFINITION-NAME "the region"))
	  ((WINDOW-MARK-P *WINDOW*)
	   (BARF "The region is empty"))
	  (T					;No region, get relevant definition
	   (CHECK-INTERVAL-SECTIONS *INTERVAL*)
	   (MULTIPLE-VALUE-SETQ (NODE DEFINITION-NAME ERROR-P)
	     (SEND *MAJOR-MODE* ':DEFINITION-INTERVAL
		   (POINT) T))		
	   (WHEN ERROR-P (BARF ERROR-P))
	   (SETQ BP1 (INTERVAL-FIRST-BP NODE)
		 BP2 (INTERVAL-LAST-BP NODE))))
    (record-changes bp1 bp2))
    dis-mark-goes)





