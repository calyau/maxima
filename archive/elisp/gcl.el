;; Copyright  William F. Schelter.   1994
;; Licensed by GNU public license.

;; You should copy isp-complete.el to the emacs/lisp directory.

;; Some commands and macros for dealing with lisp
;; M-X run : run gcl or another lisp
;; m-c-x ; evaluate defun in the other window or in the last lisp which you were using.
;; m-c-x ; with a numeric arg : compile the current defun in the other window
;; m-c-d ; disassemble in other window.
;; M-x macroexpand-next : macro expand the next sexp in other window.
;; C-h d Find documentation on symbol where the cursor is.
;; C-h / Find documentation on all strings containing a given string.
;; M-p complete the current input by looking back through the buffer to see what was last typed
;;        using this prompt and this beginning.   Useful in shell, in lisp, in gdb,...


(setq lisp-mode-hook  'remote-lisp)

(autoload 'lisp-complete "lisp-complete" nil t)
(autoload 'smart-complete "smart-complete" nil t)

;(global-set-key "p" 'lisp-complete)
(global-set-key "p" 'smart-complete)

(defun remote-lisp (&rest l)
  (and (boundp 'lisp-mode-map)
       lisp-mode-map
       (progn
  (define-key lisp-mode-map "\e\C-d" 'lisp-send-disassemble)
  (define-key lisp-mode-map "\e\C-x" 'lisp-send-defun-compile)
  (make-local-variable 'lisp-package)
  (setq lisp-package nil)
  (and (boundp 'remote-lisp-hook) (funcall remote-lisp-hook))
  )))


(defvar search-back-for-lisp-package-p nil)

;; look at the beginning of buffer to try to find an in package statement
(defun get-buffer-package ()

  "Returns what it thinks is the lisp package for the current buffer.
It caches this information in the local variable `lisp-package'.  It
obtains the information from searching for the first in-package from
the beginning of the file.  Since in common lisp, there is only
supposed to be one such statement, it should be able to determine
this.  By setting lisp-package to t, you may disable its search.  This
will also disable the automatic inclusion of an in-package statement
in the tmp-lisp-file, used for sending forms to the current
lisp-process."

  (cond ((eq lisp-package t) nil)
	(search-back-for-lisp-package-p
	 (save-excursion
	   (cond ((re-search-backward "^[ \t]*(in-package "  nil t)
		  (goto-char (match-end 0))
		  (read (current-buffer))))))
	(lisp-package lisp-package)
	(t
	 (setq
	  lisp-package
	  (let (found success)
	    (save-excursion
	      (goto-char (point-min))
	      (while (not found)
		(if (and (setq success (search-forward "(in-package " 1000 t))
			 (not (save-excursion
				(beginning-of-line)
				(looking-at "[ \t]*;"))))
		    (setq found  (read (current-buffer))))
		(if (>= (point) 980) (setq found t))
		(or success (setq found t))
		))
	    found)))))


(defun run (arg)
  "Run an inferior Lisp process, input and output via buffer *lisp*."
  (interactive "sEnter name of file to run: ")
  (require 'sshell)
  ;; in emacs 19 uncomment:
  ;;(require 'inf-lisp)
  (setq lisp-mode-hook 'remote-lisp)
  (switch-to-buffer  (make-sshell (concat arg "-lisp") arg nil "-i"))
  (make-local-variable 'shell-prompt-pattern)
    (setq sshell-prompt-pattern "^[^#%)>]*[#%)>]+ *")
    (cond ((or (string-match "maxima" arg) (string-match "affine" arg)
	   (save-excursion     (sleep-for 2)
			       (re-search-backward "maxima"
						   (max 1 (- (point) 300))
						   t)))
	   (require 'maxima-mode)
	   (inferior-maxima-mode)
	   (goto-char (point-max))
	   )
	  (t
	   (if (boundp 'inferior-lisp-mode)
	       (inferior-lisp-mode)
	     (funcall lisp-mode-hook))
	      )))

(defun lisp-send-disassemble (arg)
  (interactive "P")
   (if  arg 
       ( lisp-send-defun-compile "disassemble-h")
            ( lisp-send-defun-compile "disassemble"))
     )

(defvar time-to-throw-away nil)
(defvar telnet-new-line "")

(defun lisp-send-defun-compile (arg)

  "Send the current defun (or other form) to the lisp-process.  If there
is a numeric arg, the form (compile function-name) is also sent.  The
value of lisp-process will be the process of the other exposed window (if
there is one) or else the global value of lisp-process.  If the
...received message is not received, probably either the reading of
the form caused an error.   If the process does not have telnet in
its name, then we write a tmp file and load it.
If :sdebug is in *features*, then si::nload is used instead of
ordinary load, in order to record line information for debugging.

The value of `lisp-package' if non nil, will be used in putting an
in-package statement at the front of the tmp file to be loaded.
`lisp-package' is determined automatically on a per file basis,
by get-buffer-package.
"

  (interactive "P")
  (other-window 1)
  (let* ((proc (or (get-buffer-process (current-buffer)) lisp-process))
	 def beg
	 (this-lisp-process proc)
	 (lisp-buffer (process-buffer this-lisp-process))
	 fun)
    (other-window 1)
    (save-excursion
      (end-of-defun)
      (let ((end (dot)) (buffer (current-buffer))
	    (proc (get-process this-lisp-process)))
	(setq lisp-process proc)
	(beginning-of-defun)
	(save-excursion
	  (cond ((and arg (looking-at "(def")) (setq def t))
		(t (setq arg nil)))
	  (cond (def (forward-char 2)(forward-sexp 1)
		     (setq fun (read buffer))
		     (setq fun (prin1-to-string fun))
		     (message (format
			       "For the lisp-process %s: %s"
			       (prin1-to-string this-lisp-process) fun)))))
	(cond ((equal (char-after (1- end)) ?\n)
	       (setq end (1- end)) ))
	(setq beg (dot))
	(my-send-region this-lisp-process beg end)
	))
    

    (send-string this-lisp-process
		 (concat ";;end of form" "\n" telnet-new-line))
    (cond (arg
	   (if (numberp arg) (setq arg "compile"))
	   (send-string this-lisp-process (concat "(" arg "'" fun ")"
						  telnet-new-line))))
    (and time-to-throw-away
	 (string-match "telnet"(buffer-name (process-buffer proc)))
	 (dump-output proc time-to-throw-away))
    (cond (nil  (get-buffer-window lisp-buffer)
		  (select-window (get-buffer-window lisp-buffer))
		  (goto-char (point-max)))
	  (t nil))))

(fset 'lisp-eval-defun (symbol-function 'lisp-send-defun-compile))

(defvar telnet-new-line "")
(defvar tmp-lisp-file (concat "/tmp/" (user-login-name) ".lsp"))

(defun get-buffer-clear (name)
  (let ((cb (current-buffer))
	(buf (get-buffer-create name)))
    (set-buffer buf)
    (erase-buffer)
    (set-buffer cb)
    buf))

(defmacro my-with-output-to-temp-buffer  (name &rest body)
  (append (list
	   'let
	   (list (list 'standard-output (list 'get-buffer-clear name))))
	   body))
	      

(defun my-send-region (proc beg end)
  (cond ((or (string-match "telnet" (process-name proc)))
	 (send-region proc beg end))
	(t
	 (let ((package (get-buffer-package)))
	   (save-excursion
	       (my-with-output-to-temp-buffer  "*tmp-gcl*"
		 (if (and package (not (eq package t)))
		     (prin1 (list 'in-package  package)))
		 (princ ";!(:line ")
		 (prin1
		   (let ((na (buffer-file-name (current-buffer))))
		     (if na (expand-file-name na)
		       (buffer-name (current-buffer))))
		   )
		 (princ (- (count-lines (point-min) (+ beg 5)) 1))
		 (princ ")\n")
		 (set-buffer "*tmp-gcl*")
		 (write-region (point-min) (point-max) tmp-lisp-file nil nil)))
	   (write-region beg end tmp-lisp-file t nil)
	   (message "sending ..")
	   (send-string
	    proc
	    (concat "(lisp::let ((*load-verbose* nil)) (#+sdebug si::nload #-sdebug load \""
		    tmp-lisp-file
		    "\")#+gcl(setq si::*no-prompt* t)(values))\n  ")
			)
	   (message (format "PACKAGE: %s ..done"
			    (if (or (not package) (eq package t))
				"none"
			      package)))


	   ))))

(defun dump-output (proc seconds)
  "dump output for PROCESS for SECONDS or to \";;end of form\""
 (let ((prev-filter (process-filter proc)) (already-waited 0))
       (unwind-protect (progn (set-process-filter proc 'dump-filter)
			      (while (< already-waited seconds)
			      (sleep-for 1)(setq already-waited
						 (1+ already-waited))))
	 (set-process-filter proc prev-filter))))



(defun dump-filter (proc string)
;  (setq she (cons string she))
  (let ((ind (string-match ";;end of form" string)))
    (cond (ind (setq string (substring
			     string
			     (+ ind (length
				     ";;end of form"))))

	       (message "... received.")
	       (setq already-waited 1000)
	       (set-process-filter proc prev-filter)
	       (cond (prev-filter (funcall prev-filter proc string))
		     (t string)))
	  (t ""))))


;;(process-filter (get-process "lisp"))
(defun macroexpand-next ()
  "macroexpand current form"
  (interactive)
  (save-excursion
    (let ((beg (point)))
      (forward-sexp )
      (message "sending macro")

      (let* ((current-lisp-process
	      (or (get-buffer-process (current-buffer))
		       (prog2 (other-window 1)
			      (get-buffer-process (current-buffer))
			      (other-window 1)))))
	(send-string current-lisp-process "(macroexpand '")
	(send-region current-lisp-process  beg (point) )
	(send-string current-lisp-process ")\n")))))

(defun delete-comment-char (arg) 
  (while (and (> arg 0) (looking-at comment-start)) (delete-char 1) 
	 (setq arg (1- arg))))

(defun mark-long-comment ()
  (interactive)
  (let ((at (point)))
    (beginning-of-line)
    (while(and (not (eobp))
	       (or  (looking-at comment-start)
		    ;(looking-at "[ 	]*\n")
		    ))
      (forward-line 1))
    (set-mark (point))
    (goto-char at)
    (while(and (not (bobp))
	       (or  (looking-at comment-start)
		    ;(looking-at "[ 	]*\n")
		    ))
      (forward-line -1))
    (or (bobp )(forward-line 1))))
    

(defun fill-long-comment ()
  (interactive)
  (mark-long-comment)
  (let ((beg (min (dot) (mark)))
	(end (max (dot) (mark))) (n 0)m)
    (narrow-to-region beg end)
    (goto-char (point-min))    
    (while (looking-at ";")
      (forward-char 1))
    (setq n (- (point) beg))
    (goto-char (point-min))    
    (while (not (eobp))
      (setq m n)
      (while (> m  0)
	(cond ((looking-at ";")
	       (delete-char 1)
	       (cond ((looking-at " ")(delete-char 1)(setq m 0)))
	       (setq m (- m 1)))
	      (t (setq m 0))))
      (forward-line 1))
    (fill-region (dot-min) (dot-max))
    (goto-char (point-min))
    (while (not (eobp))
      (cond ((looking-at "\n")
	     nil)
	    (t(insert ";; ")))
      (forward-line 1))
   (goto-char (point-min))
   (set-mark (point-max))
   (widen)))

(defun comment-region (arg) 
  "Comments the region, with a numeric arg deletes up to arg comment 
characters from the beginning of each line in the region.  The region stays, 
so a second comment-region adds another comment character" 
 (interactive "P") 
 (save-excursion 
   (let ((beg (dot)) 
	 (ok t)(end (mark)))
          (comment-region1 beg end arg))))

(defun comment-region1 (beg end arg)
  (let ((ok t))
    (cond((> beg end) 
	  (let ((oth end)) 
	    (setq end beg beg oth)))) 
    (narrow-to-region beg end) 
    (goto-char beg) 
       (unwind-protect 
	   (while ok 
	     (cond (arg 
		    (delete-comment-char arg)) 
		   (t   (insert-string comment-start)))
	     (if (< end (dot)) (setq ok nil)
	       (if  (search-forward "\n" end t) nil (setq ok nil))) )
	 (widen))))

(defun trace-expression ()
  (interactive)
  (save-excursion
     (forward-sexp )
    (let ((end (point)))
           (forward-sexp -1)
      (other-window 1)
      (let* ((proc (get-buffer-process (current-buffer)))
	     (current-lisp-process (or  proc lisp-process)))
	(other-window 1)
	(message "Tracing: %s" (buffer-substring (point) end))
	(send-string current-lisp-process "(trace ")
	(send-region current-lisp-process (point) end)
	(send-string current-lisp-process ")\n")))))

(provide 'gcl)