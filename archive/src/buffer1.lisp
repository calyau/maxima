;;; -*- Mode: Lisp; package:maxima; syntax:common-lisp -*- ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;     Copyright (c) 1984 by William Schelter,University of Texas     ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar $default_buffer_name '&Macsyma_Buffer)



(defun into-buffer-name (args)
  (let ((name (cond ((zl-NLISTP args)		;
		     $default_buffer_name)
		    ((and ($listp (car args))
			  (not (null (cadr args))))
		     (cadr args))
		    (($listp (cadr args))
		     (car args))
		    (t $default_buffer_name))))
    (substring (strmeval name) 1)))



(defun into-buffer-objs (args)
  (let ((objs (cond ((zl-NLISTP args)
		     args)
		    (($listp (car args))
		     (cdar args))
		    (($listp (cadr args))
		     (cdadr args))
		    (t args))))
    (mapcar #'meval objs)))



(defmspec $into_buffer (args)
  (let ((buffer-name (into-buffer-name (cdr args)))
	(things-to-output (into-buffer-objs (cdr args))))
    (if (null things-to-output)
	(merror "INTO_BUFFER: No objects given to put in to buffer"))
    (let ((temp zwei::*default-major-mode* ))
      ;(setf zwei::*default-major-mode* :macsyma)
      (zwei::with-editor-stream (*standard-output* ':buffer-name buffer-name)
			       (loop for element in things-to-output
				     do (cl-macsyma::i-$grind element)
				     (format t "~&")))
      ;(setf zwei::*default-major-mode* temp )
      )
  '$done))



(defvar $edlinenum 1)



(defun zwei::macsyma-arglist (fun &aux  spec)
  (cond
    ((setq spec (get fun 'mfexpr*))(arglist spec))
    ((setq spec (mgetl fun '(mexpr aexpr fexpr)))
	 (cdr (second (second spec))))))




(defun new-concat (&rest l) (intern (lexpr-funcall 'string-append "$" (mapcar 'string l))
       'macsyma))
    
(defun $new_concat (&rest l) (intern (lexpr-funcall 'string-append "$" (mapcar 'string l))
       'macsyma))

