;; This file is part of GNU Emacs.
;; Copyright (C) 1998 William F. Schelter

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.


;; Maxima mode by W. Schelter Code for running maxima under something
;; like shell mode (inferior-maxima-mode). A mode for editing a buffer
;; of maxima code is called maxima-mode.

(require 'sshell)
(provide 'maxima-mode)
(defvar maxima-mode-syntax-table nil "")
(defvar maxima-mode-abbrev-table nil "")
(if (not maxima-mode-syntax-table)
    (let ((i 0))
      (setq maxima-mode-syntax-table (make-syntax-table))
      (while (< i ?0)
	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?9))
      (while (< i ?A)
	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?Z))
      (while (< i ?a)
	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
	(setq i (1+ i)))
      (setq i (1+ ?z))
      (while (< i 128)
	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
	(setq i (1+ i)))
  (modify-syntax-entry ?  "    " maxima-mode-syntax-table)
  (modify-syntax-entry ?\t "   " maxima-mode-syntax-table)
  (modify-syntax-entry ?` "'   " maxima-mode-syntax-table)
  (modify-syntax-entry ?' "'   " maxima-mode-syntax-table)
  (modify-syntax-entry ?, "'   " maxima-mode-syntax-table)
  (modify-syntax-entry ?. "'   " maxima-mode-syntax-table)
  (modify-syntax-entry ?# "'   " maxima-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" maxima-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" maxima-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" maxima-mode-syntax-table)
  (modify-syntax-entry ?+ "." maxima-mode-syntax-table)
  (modify-syntax-entry ?- "." maxima-mode-syntax-table)
  (modify-syntax-entry ?= "." maxima-mode-syntax-table)
  (modify-syntax-entry ?% "." maxima-mode-syntax-table)
  (modify-syntax-entry ?< "." maxima-mode-syntax-table)
  (modify-syntax-entry ?> "." maxima-mode-syntax-table)
  (modify-syntax-entry ?& "." maxima-mode-syntax-table)
  (modify-syntax-entry ?| "." maxima-mode-syntax-table)
  (modify-syntax-entry ?\" "\"    " maxima-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\   " maxima-mode-syntax-table)
  (modify-syntax-entry ?\( "()  " maxima-mode-syntax-table)
  (modify-syntax-entry ?\) ")(  " maxima-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]  " maxima-mode-syntax-table)
  (modify-syntax-entry ?\] ")[  " maxima-mode-syntax-table)
			   ))

(defun maxima-mode-variables ()
  (set-syntax-table maxima-mode-syntax-table)
  (setq local-abbrev-table maxima-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'maxima-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\*+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'maxima-process)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'maxima-comment-indent))


(defun maxima-mode-commands (map)
  (define-key map "\e\C-q" 'indent-maxima-sexp)
  (define-key map "\177" 'backward-delete-char-untabify)
  (define-key map "\t" 'maxima-indent-line)
  (define-key map "\C-j" 'maxima-newline-and-indent)
  (define-key map "\C-x " 'maxima-add-breakpoint)
  (define-key map "\e\C-h" 'mark-maxima-form)
  )


(defun maxima-add-breakpoint ()
  "Add a break point"
  (interactive )
  (let ((buf (current-buffer)) name (at (point)) found (looking t))
  (save-excursion
    (beginning-of-maxima-form)
    (while looking
      (and (re-search-forward "\\b\\([a-z_A-Z0-9]+\\)[ \t\n]*(" nil t)
	   (save-match-data
	     (not (or  (is-sharp-comment-line) (in-maxima-comment-p))))
	   (setq found t)
	   (setq looking nil))) 
    (cond (found
	   (setq name (buffer-substring (match-beginning 1) (match-end 1)))
	   (setq line (count-lines (match-beginning 1) at))
	   (send-string (get-maxima-process)
			(concat ":br " name " " line "\n"))))
    
    )))

(defun get-maxima-process ()
  (let (proc)
  (cond ((and (boundp 'maxima-process) maxima-process))
	((setq proc (get-buffer-process (current-buffer)))
	 (setq maxima-process proc))
	(t (save-excursion
	     (other-window 1)
	     (cond ((setq proc (get-buffer-process (current-buffer)))
		    (setq maxima-process proc))
		   (t (error "cant find the maxima process"))))))))
			   


(defun in-maxima-comment-p ()
  (let ((ok t) result)
  (save-excursion
    (while ok
    (cond ((search-backward "/" (- (point) 2000) t)
	   (cond ((looking-at "/\\*")
		  (setq result t ok nil))
		 ((bobp) (setq ok nil ))
		 ((progn (forward-char -1)
			 (looking-at "\\*/"))
		  (setq ok nil))))
	  (t (setq ok nil))))
    result)))

(defvar maxima-mode-map ())

(if maxima-mode-map
    ()
  (setq maxima-mode-map (make-sparse-keymap))
  (define-key maxima-mode-map "\e\C-x" 'maxima-send-maxima-form-compile)
   (maxima-mode-commands maxima-mode-map))

(defun maxima-mode ()
  "Major mode for editing Maxima code.
Commands:
Tab indents the current line relative to previous lines.
Linefeed (c-j) enters a new line and the indents appropriately.       
Blank lines separate paragraphs.  Comments are delimited by /* and */
\\{maxima-mode-map}
Entry to this mode calls the value of maxima-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map maxima-mode-map)
  (setq major-mode 'maxima-mode)
  (setq mode-name "Maxima")
  (maxima-mode-variables)
  (run-hooks 'maxima-mode-hook))

;; This will do unless sshell.el is loaded.



(defun maxima-indent-line (&optional whole-exp)
  "Indent current line as Maxima code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (calculate-maxima-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at "[ \t]*/\\*")
	;; Don't alter indentation of a ;;; comment line.
	nil
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
	  nil
	(delete-region beg (point))
	(indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
	   (save-excursion
	     (goto-char beg)
	     (forward-sexp 1)
	     (setq end (point))
	     (goto-char beg)
	     (forward-line 1)
	     (setq beg (point))
	     (> end beg))
	   (indent-code-rigidly beg end shift-amt)))))

(defun forward-over-comment-whitespace ()
  (interactive)
  (let ((ok t))
    (while  ok
    (skip-chars-forward " \t")
    (cond ((looking-at "/\\*")
	   (search-forward "*/" nil t )
	   )
	  ((looking-at "\n[\t ]*#")
	   (forward-line 1)
	   (end-of-line))
	  ((looking-at "\n")
	   (forward-char 1))
	  (t  (setq ok nil))))))



(defun back-over-comment-whitespace ()
  (let ((ok t))
    (while  ok
      (skip-chars-backward " \t\n")
      (cond ((> (point) 2)
	     (forward-char -2)))
      (cond ((looking-at "\\*/")
	     (search-backward "/*" )
	     )
	    (t (forward-char 2) (setq ok nil))))))

(defun calculate-maxima-indent (&optional parse-start)
  "Return appropriate indentation for current line as Maxima code.
In usual case returns an integer: the column to indent to.
"
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  state paren-depth
	  ;; setting this to a number inhibits calling hook
	  (desired-indent nil)
	  (retry t)
	  indent-col begin-char
	  last-sexp containing-sexp)
      (save-excursion
	(beginning-of-line)
	(cond ((looking-at "[a-zA-Z]")
	       (setq indent-col 0))
	      ((progn (skip-chars-forward " \t")
		      (setq begin-char (char-after (point)))
		      (looking-at "else"))
	       (backward-sexp)
	       (re-search-backward "then")
	       (setq indent-col (current-column)))
	      (t
	       (back-over-comment-whitespace)
	       (beginning-of-line)
		 (cond
		  ((looking-at "[a-zA-Z]")
		   (setq indent-col 2))
		  (t
		   (skip-chars-forward " \t")
		   (let ((col (current-column))col1
			 )
		     (end-of-line)
		     (back-over-comment-whitespace)
		     (forward-char -1)
		     (cond ((looking-at "[,]")
			    (let ((pt (begin-paren)))
			      (cond (pt
				      (goto-char pt)
				      (setq indent-col (+ (current-column) 1))
				      )
				    (t (setq indent-col 2)))
			      (forward-char -5)
			      (cond ((looking-at "BLOCK(")
				     (setq indent-col
					   (+ (current-column) 2))))))
			   ((or (looking-at "[*---+]")
				(memq begin-char '(?\* ?\+ ?\-)))
			    (cond ((looking-at "[])]")
				   (forward-char 1)))
			    (setq indent-col (begin-paren-column)))
			   ((looking-at "(")
			    (setq indent-col (current-column)))
			   ((looking-at ")")
			    (forward-char 1)
			    (setq indent-col (begin-paren-column)))
			   (t (beginning-of-line)
			      (skip-chars-forward " \t")
			      (setq indent-col (+ (current-column) 2))
			      ))))))))
;      (message "col %d" indent-col )
      indent-col)))

(defun begin-paren-column ()
  (save-excursion
  (let ((pt (begin-paren)))
    (if pt (progn (goto-char pt) (current-column))
      2))))

(defun maxima-newline-and-indent ()
  (interactive)
  (newline)
  (insert " ")
  (maxima-indent-line))

(defvar maxima-paren-nest (make-vector 20 nil))

;(defun fo ()(interactive) (save-excursion
;   (goto-char(begin-paren)) (sit-for 1)))


(defun begin-paren ()
  (interactive)
  (let ((ok t)(level 0)(here (point)) last (max 0))
  (save-excursion
    (cond ((re-search-backward "[;$]" nil t)
	   (skip-chars-forward ";$ \t\n"))
	  (t (goto-char (point-min))
	      (forward-over-comment-whitespace)))
    (while ok
      (setq last (point))
      (cond ((re-search-forward "[][()]" here t)
	     (setq max (max level max))
	     (let ((ch (char-after (- (point) 1))))
	     (cond ((memq ch '( ?( ?[))    ;)
		    (setq level (+ level 1))
		    (aset maxima-paren-nest level  (1- (point))))
		   (t
		    (setq level (- level 1))))))
	    (t (setq ok nil))))
      (aref maxima-paren-nest level)
      )))

;(setq stack-trace-on-error t)
      



;; (put 'progn 'maxima-indent-hook 0), say, causes progn to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).


;; Of course this should be done in one pass but..
(defun indent-maxima-sexp ()
  (interactive)
  (save-excursion
    (let ((beg (progn (beginning-of-maxima-form)
		      (point)))
	  (end (progn (end-of-maxima-form) (point))))
      (goto-char beg)
      (while (< (point) end)
	(maxima-indent-line)
	(forward-line 1)))))
      



(defun is-sharp-comment-line ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*#")))


  
(defun beginning-of-maxima-form ()
  (let ((tem (re-search-backward "[;$]" (point-min) t)))
    (cond (tem
	   (cond ((in-maxima-comment-p)
		  (search-backward "/\\*" nil t)
		  (beginning-of-maxima-form))
		 ((is-sharp-comment-line)
		  (beginning-of-line)
		  (or (equal (point) (point-min)) (forward-char -1))
		  (beginning-of-maxima-form))
		 (t  (forward-char 1))))
	  (t (goto-char  (point-min))))
    (forward-over-comment-whitespace)))

(defun end-of-maxima-form ()
   (let ((tem (re-search-forward "[;$]" (point-max) t)))
     (cond (tem
	    (cond ((in-maxima-comment-p)
		   (search-forward "*/" nil t)
		   (end-of-maxima-form))
		  ((is-sharp-comment-line)
		   (end-of-line) (forward-char 1)
		   (end-of-maxima-form)
		   )
		  ))
	   (t (error "No ; or $ at end ")))))

(defun run-maxima ()
  (interactive)
  (make-sshell "maxima" "maxima")
     (switch-to-buffer "*maxima*")
     (inferior-maxima-mode))

(defun mark-maxima-form ()
  (interactive)
  (beginning-of-maxima-form)
  (let ((tem (point)))
    (end-of-maxima-form)
    (set-mark (point))
    (goto-char tem)))

(defun maxima-send-maxima-form-compile (arg)
  "Send the current maxima-form to the maxima-process and compile it if there
is a numeric arg and (compile 'foo) is understood by maxima-process The
value of maxima-process will be the process of the other exposed window
if there is one or else the global value of maxima-process.  If the
...received message is not received, probably either the reading of
the form c form caused an error, or time-to-throw-away needs increasing."
  (interactive "P")
  (other-window 1)
  (let* ((proc (get-maxima-process))
	 def
	 (this-maxima-process proc)
	 (maxima-buffer (process-buffer this-maxima-process))
	 fun)
    (other-window 1)
    (save-excursion
      (end-of-maxima-form)
      (let ((end (dot)) (buffer (current-buffer))
	    (proc (get-process this-maxima-process)))
	(setq maxima-process proc)
	(forward-char -1)
	(beginning-of-maxima-form)
	(cond ((equal (char-after (1- end)) ?\n)
	       (setq end (1- end)) ))
	(setq bill (buffer-substring (dot) end))
	(send-region this-maxima-process (dot) end)))
;    (send-string this-maxima-process
;		 (concat ";;end of form" "\n" telnet-new-line))
    (cond ((string-match "maxima" (buffer-name (process-buffer proc)))
	   (send-string proc "\n")
	   (sleep-for 1)
	   (other-window 1)
  	   (cond ((eq (window-buffer) maxima-buffer)
		  (forward-char (- (dot-max) (dot)))))
	   (other-window 1))
	  (t
;    (send-string this-maxima-process telnet-new-line)
    (send-string this-maxima-process "\n")
    (and (boundp 'time-to-throw-away) time-to-throw-away (dump-output proc time-to-throw-away))
    (set-buffer maxima-buffer)
    (set-window-dot (get-buffer-window maxima-buffer) (dot-max))))))


(defun maxima-send-maxima-form-compile (arg)
  "Send the current maxima-form to the maxima-process and compile it if there
is a numeric arg and (compile 'foo) is understood by maxima-process The
value of maxima-process will be the process of the other exposed window
if there is one or else the global value of maxima-process.  If the
...received message is not received, probably either the reading of
the form c form caused an error, or time-to-throw-away needs increasing."
  (interactive "P")
  (save-excursion
    (other-window 1)
    (let* ((proc (or (get-buffer-process (current-buffer)) maxima-process))
	   def
	   filename line-number
	   (this-maxima-process proc)
	   (maxima-buffer (process-buffer this-maxima-process))
	   fun)
      (setq maxima-process proc)
      (other-window 1)
      (save-excursion
	(end-of-maxima-form)
	(let ((end (point)) (buffer (current-buffer))
	      (proc (get-process this-maxima-process)))
	  (setq maxima-process proc)
	  (forward-char -1)
	  (beginning-of-maxima-form)
	  (setq filename (buffer-file-name buffer))
	  (setq line-number (count-lines (point-min) (point)))
	  (setq bill (buffer-substring (point) end))
	  (setq com 		    (concat ":l (setq *mread-prompt* \"\")(princ #\\newline) (values) \n"
					    " \n# " line-number " \"" filename "\"\n"
					    bill
					    "\n"))
	  (send-string this-maxima-process com)
	  ;(set-buffer maxima-buffer)
	  '(message "hi")
	  ;(sit-for 1)
	  ;(message (concat "at " (point-max)))
	  ;(goto-char (point-max))
	  

	  )
	  
	))))

(defvar inferior-maxima-mode-map nil)

(if inferior-maxima-mode-map
    nil
  (setq inferior-maxima-mode-map (copy-alist sshell-mode-map))
  ;(maxima-mode-commands inferior-maxima-mode-map)
  (define-key inferior-maxima-mode-map "\e\C-x" 'maxima-send-defun))

(defvar inferior-maxima-prompt
  "\\(^([C][0-9]*) \\)\\|\\(^[^#%)>]*[#%)>]+ *\\)"
  "*Regexp to recognize prompts from the inferior Maxima or lisp
")

(defun inferior-maxima-mode ()
  "Major mode for interacting with an inferior Maxima process.

The following commands are available:
\\{inferior-maxima-mode-map}

Entry to this mode calls the value of maxima-mode-hook with no arguments,
if that value is non-nil.  Likewise with the value of sshell-mode-hook.
maxima-mode-hook is called after sshell-mode-hook.

You can send text to the inferior Maxima from other buffers
using the commands process-send-region, process-send-string
and \\[maxima-send-defun].

Commands:
Delete converts tabs to spaces as it moves back.
Tab indents for Maxima; with argument, shifts rest
 of expression rigidly with the current line.
Meta-Control-Q does Tab on each line starting within following expression.
Paragraphs are separated only by blank lines.

Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.
C-d at end of buffer sends end-of-file as input.
C-d not at end or with arg deletes or kills characters.
C-u and C-w are kill commands, imitating normal Unix input editing.
C-c interrupts the sshell or its current subjob if any.
C-z stops, likewise.  C-\\ sends quit signal, likewise.

C-x C-k deletes last batch of output from sshell.
C-x C-v puts top of last batch of output at top of window."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'inferior-maxima-mode)
  (setq mode-name "Inferior Maxima")
  (setq mode-line-process '(": %s"))
  (maxima-mode-variables)
  (use-local-map inferior-maxima-mode-map)
  (make-local-variable 'last-input-start)
  (setq last-input-start (make-marker))
  (make-local-variable 'last-input-end)
  (setq last-input-end (make-marker))
  (make-local-variable 'sshell-prompt-pattern)
  (setq sshell-prompt-pattern inferior-maxima-prompt)
  (run-hooks 'sshell-mode-hook 'maxima-mode-hook))

