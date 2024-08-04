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
;;;; write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA

;; Defined in maxima-package.
;; (defpackage "COMMAND-LINE"
;;   (:use "COMMON-LISP")
;;   (:nicknames "CMDLINE")
;;   (:export "CL-OPTION" "MAKE-CL-OPTION" "LIST-CL-OPTIONS" "PROCESS-ARGS"
;; 	   "GET-APPLICATION-ARGS"))

(in-package :command-line)

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

(defvar *wrap-help-string* t
  "Wrap the help string when non-NIL")

(defun print-help-string (help-string)
  "Print the help string neatly by breaking long lines as needed.
  This assumes that the HELP-STRING doesn't have any kind of manually
  inserted formatting."
  (cond
    (*wrap-help-string*
     ;; Break the help string into a list of words and print the list
     ;; of words individually with a single space after, and inserting
     ;; a newline as needed.
     (let ((words (pregexp::pregexp-split "\\s+" help-string)))
       ;; This format string is a modified version of the example in
       ;; https://www.lispworks.com/documentation/HyperSpec/Body/22_cfb.htm.
       ;; Each line is prefixed by 8 spaces and we wrap the line at 80
       ;; columns.
       (format t "        ~{~<~%        ~1,80:; ~A~>~^~}" words)))
    (t
     (format t "        ~a" help-string))))

(defun list-cl-options (cl-option-list &key texi-table-form)
  "Prints all the command line options for Maxima in a neat form.  If
  :TEXI-TABLE-FORM is non-NIL, the output is suitable for adding a
  table to commandline-options.texi."
  (if texi-table-form
      (format t "@need 100~%@table @code~%")
      (format t "options:~%"))
  (dolist (opt cl-option-list)
    (let ((help-string (cl-option-help-string opt))
	  (names (cl-option-names opt))
	  (arg (cl-option-argument opt)))
      (when texi-table-form
        (format t "@need 150~%@item "))
      (let ((options (mapcar #'(lambda (name)
                                 (cl-option-description name arg))
                             names)))
        ;; Wrap any long list of options, except we don't when
        ;; producing output for the texi file.
        (cond (texi-table-form
               (format t "~{~A~^, ~}~%" options))
              (t
               ;; The output is 4 spaces, then each of the options
               ;; separated by a ", ".  The output is wrapped at
               ;; column 80.  This format string is a modified version
               ;; of the example from
               ;; https://www.lispworks.com/documentation/HyperSpec/Body/22_cfb.htm.
               (format t "    ~{~<~%      ~1,80:;~A~>~^, ~}~%"
                       options))))

      (when help-string
	(print-help-string help-string))
      (when texi-table-form
        (format t "~2%"))
      (terpri)))
  (when texi-table-form
    (format t "@end table~%"))
  (finish-output))

(defun process-args (args cl-option-list)
  (flet ((fixup (options)
	   ;; Massage cl-option into the format wanted by getopt.
	   ;; Basically, remove any leading dashes, and if the
	   ;; cl-option includes an argument, treat it as a required
	   ;; argument.
	   (let ((opts nil))
	     (dolist (o options)
	       (dolist (name (cl-option-names o))
		 (push (list (string-left-trim "-" name)
			     (if (cl-option-argument o)
				 :required
				 :none)
			     nil)
		       opts)))
	     (nreverse opts))))
    (let ((options (fixup cl-option-list)))
      (multiple-value-bind (non-opts opts errors)
	  (getopt:getopt args options :allow-exact-match t)
	(declare (ignore non-opts))	;non-opts ignored for now
	;; Look over all of opts and run the action
	#+nil (format t "opts = ~S~%" opts)
	(dolist (o opts)
	  ;; Try to find the corresponding cl-option.
	  (let ((cl-opt (find (car o)
			      cl-option-list
			      :test #'(lambda (desired e)
					;; Strip off any leading
					;; dashes from the option name
					;; and compare with the
					;; desired option.
					(member desired (cl-option-names e)
						:test #'equal
						:key #'(lambda (e)
							 (string-left-trim "-" e)))))))
	    #+nil (format t "Processing ~S -> ~S~%" o cl-opt)
	    (if cl-opt
		(cond ((and (cl-option-action cl-opt) (cl-option-argument cl-opt))
		       (funcall (cl-option-action cl-opt) (cdr o)))
		      ((cl-option-action cl-opt)
		       (funcall (cl-option-action cl-opt))))
		(warn "Could not find option ~S in cl-options: ~S.~%Please report this bug."
		      o cl-option-list))))
	(format t "~{Warning: argument ~A not recognized.~%~}" errors)
	;; What do we do about non-option arguments?  We just ignore them for now.
	))))


(defun get-application-args ()
  ;; -- is used to distinguish between options for a lisp implementation
  ;; and for Maxima.
  (flet ((remove-implementation-args (arglist)
           (let ((dashes (member "--" arglist :test #'equal)))
             (if dashes
                 (cdr dashes)
                 arglist))))
    (remove-implementation-args
     #+clisp
     (rest ext:*args*)
    
     #+ecl
     (rest (ext:command-args))

     #+cmu
     (if (boundp 'ext::*command-line-application-arguments*)
	 ext::*command-line-application-arguments*
	 (rest ext:*command-line-strings*))
     
     #+scl
     (rest ext:*command-line-strings*)

     #+sbcl
     (rest sb-ext:*posix-argv*)

     #+gcl
     (rest si:*command-args*)

     #+allegro
     (rest (system:command-line-arguments :application t))
      
     #+lispworks
     (rest system:*line-arguments-list*)

     #+openmcl
     (rest ccl:*command-line-argument-list*)

     #+abcl
     ext:*command-line-argument-list*)))
