(in-package "MAXIMA")
;(require "MAKE" "make.lisp")
;;The following can be used to help building a list of auto declarations.
;;They certainly add to many names for a translated file.

(defun grab-externals (file &aux tem ans (eof '(nil))
			    (file-name (intern (pathname-name
						 (pathname file)))))
  (declare (special ans file-name))
  (with-open-file (st file)
    (sloop while (not (eq eof (setq tem (read st nil eof))))
	   when  (consp tem)
	   do  (grab-externals1 tem)))
    ans)
				       
(defun grab-externals1 (form )
  (declare (special ans file-name))
  (cond ((consp form)
	 (case (car form)
	   (defmfun	    
	    (push `(autof ',(second form)',file-name) ans))
	   (defmtrfun	    
	    (push `(autof ',(car(second form))',file-name) ans))
	   
	   (defmspec (push `(auto-mspec ',(second form)',file-name)
			   ans))
	   (eval-when  (dolist (v (cddr form))(grab-externals1 v)))
	   (progn  (dolist (v (cdr form))(grab-externals1 v)))))))

(defun print-maxima-externals (file &optional
				    (files (make::system-files :maxima)))
  (let ((decls
	   (sloop for v in  appending
		  (grab-externals (format nil "~(~a~).lisp" v)))))
    (with-open-file (st file :direction :output)
      (format st ";;Autoloads for maxima~%(in-package \"MAXIMA\")~2%")
      (sloop for w in decls
	     do (format st "~%~s" w)))))

;An emacs-lisp function for figuring out which of the translated functions
;are really important.  Put in one window the doc file and all test files
;and all manual concatted together.  Then go through the forms in this
;window and if you can't find the form in the documentation, then it
;is not external.  Comment it out.
#|
(defun delete-some-autoloads ()
  (interactive)
  (while (re-search-forward " '" nil t)
    (let ((tem (read (current-buffer))))
      (other-window 1)
      (goto-char (point-min))
      (cond ((search-forward (substring (format "%s" tem) 1) nil t) (other-window 1))
	    (t (other-window 1) (beginning-of-line )
	       
	       (insert ";")))
      (beginning-of-line )
      (forward-line 1))))
	     
|#


