;;;; command-line.lisp -- Application command line argument retrieval
;;;;                      and processing for Common Lisp.

;;;; Copyright (C) 2003 James F. Amundson

;;;; command-line.lisp is free software; you can redistribute it
;;;; and/or modify it under the terms of the GNU General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 2, or (at your option) any later version.

;;;; command-line.lisp is distributed in the hope that it will be
;;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;; See the GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with command-line.lisp; see the file COPYING.  If not,
;;;; write to the Free Software Foundation, Inc., 59 Temple Place -
;;;; Suite 330, Boston, MA 02111-1307, USA.

;; Defined in maxima-package.
;; (defpackage "COMMAND-LINE"
;;   (:use "COMMON-LISP")
;;   (:nicknames "CMDLINE")
;;   (:export "CL-OPTION" "MAKE-CL-OPTION" "LIST-CL-OPTIONS" "PROCESS-ARGS"
;; 	   "GET-APPLICATION-ARGS"))

(in-package "COMMAND-LINE")

(defstruct cl-option
  (names nil)
  (argument nil)
  (action nil)
  (help-string nil))

(defun cl-option-description (name arg)
  (if arg
      (cond ((= (length name) 1) (format nil "~a ~a" name arg))
	    ((equal (subseq name 0 2) "--") (format nil "~a=~a" name arg))
	    (t (format nil "~a ~a" name arg)))
      name))

(defun list-cl-options (cl-option-list)
  (format t "options:~%")
  (dolist (opt cl-option-list)
    (let ((help-string (cl-option-help-string opt))
	  (names (cl-option-names opt))
	  (arg (cl-option-argument opt)))
      (format t "    ~a" (cl-option-description (first names) arg))
      (dolist (name (rest names))
	(format t ", ~a" (cl-option-description name arg)))
      (format t ":")
      (if help-string
	  (format t " ~a" help-string))
      (format t "~%"))))

(defun in-list (item list)
  (dolist (element list)
    (if (equal item element)
	(return-from in-list t)))
  nil)

(defun parse-args (args cl-option-list)
  (if (null args)
      nil
      (let ((arg (pop args))
	    (arg-matched nil))
	(dolist (opt cl-option-list)
	  (if (in-list arg (cl-option-names opt))
	      (progn
		(cond ((and (cl-option-action opt) (cl-option-argument opt))
		       (funcall (cl-option-action opt) (pop args)))
		      ((cl-option-action opt)
		       (funcall (cl-option-action opt)))
		      ((cl-option-argument opt)
		       (pop args)))
		(setf arg-matched t)
		(return t))))
	(if (and (not arg-matched) (not (equal arg "")))
	    (format t "Warning: argument ~a not recognized~%" arg))
	(parse-args args cl-option-list))))

(defun expand-compound-arg (arg)
  (map 'list  #'(lambda (char) (concatenate 'string "-" (string char)))
       (subseq arg 1)))

(defun expand-equals-arg (arg)
  (let ((equals-position (search "=" arg)))
    (list (subseq arg 0 equals-position) (subseq arg (+ 1 equals-position)))))

(defun expand-args (args)
  (if (null args)
      nil
      (let* ((arg (car args))
	     (rest (expand-args (cdr args)))
	     (listarg (list arg)))
	(cond ((< (length arg) 2) nil)
	      ((and (equal (subseq arg 0 2) "--") (search "=" arg))
	       (setf listarg (expand-equals-arg arg)))
	      ((equal (subseq arg 0 2) "--") nil)
	      ((equal (char arg 0) #\-)
	       (if (> (length arg) 2)
		   (setf listarg (expand-compound-arg arg)))))
	(append listarg rest))))

(defun process-args (args cl-option-list)
  (parse-args (expand-args args) cl-option-list))

(defun get-application-args ()
  #+clisp (rest ext:*args*)
    
  #+cmu (let ((result lisp::lisp-command-line-list))
	  (do ((removed-arg nil (pop result)))
	      ((or (equal removed-arg "--") (equal nil result)) result)))

  #+sbcl (rest sb-ext:*posix-argv*)

  #+gcl  (let ((result  si::*command-args*))
	   (do ((removed-arg nil (pop result)))
	       ((or (equal removed-arg "--") (equal nil result)) result)))

  #+allegro
  (let ((args (system:command-line-arguments :application t)))
    ;; Skip the first arg, which is the full path to alisp.
    (rest args))
      
      

  ;; FIXME: openmcl version missing
  )
