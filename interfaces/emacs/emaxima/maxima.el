;;; maxima.el --- Major modes for writing Maxima code

;; Copyright (C) 1998,1999 William F. Schelter
;; Copyright (C) 2001 Jay Belanger

;; Author: William F. Schelter
;;         Jay Belanger
;; Maintainer: Jay Belanger <belanger@truman.edu>
;; Keywords: maxima

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;
;; Please send suggestions and bug reports to <belanger@truman.edu>. 
;; The latest version of this package should be available at
;; ftp://vh213601.truman.edu/pub/Maxima
;; You will need both maxima.el and maxima-font-lock.el

;;; Commentary:

;; This is a branch of William Schelter's maxima-mode.el
;;
;; Quick intro
;;
;; To install, put this file (as well as maxima-font-lock.el)
;; somewhere in your emacs load path.
;; To make sure that `maxima.el' is loaded when necessary, whether to
;; edit a file in maxima mode or interact with Maxima in an Emacs buffer,
;; put the lines
;;  (autoload 'maxima-mode "maxima" "Maxima mode" t)
;;  (autoload 'maxima "maxima" "Maxima interaction" t)
;; in your `.emacs' file.  If you want any file ending in `.max' to begin
;; in `maxima-mode', for example, put the line
;;  (setq auto-mode-alist (cons '("\\.max" . maxima-mode) auto-mode-alist))
;; to your `.emacs' file.
;;
;; Some variables that may have to be set in order to use the maxima help:
;;  maxima-info-dir   
;;         This should be the directory where the maxima info files are kept.
;;         By default, it is "/usr/local/info/"
;;  maxima-info-index-file
;;         This should be the name of the maxima info file that contains
;;         the index, by default, it is "maxima.info-15"
;; To allow M-x maxima-mode to put a buffer into maxima mode, add the line
;; (autoload 'maxima-mode "maxima" "Maxima editing mode" t)
;; to your .emacs file, to allow M-x maxima to start an interactive
;; maxima process, add
;; (autoload 'maxima "maxima" "Running Maxima interactively" t)
;; to your .emacs file.
;;
;; ** Maxima mode **
;; To put the current buffer into maxima-mode, type M-x maxima-mode

;; Maxima mode provides the following motion commands:
;; M-C-a: Move to the beginning of the form.
;; M-C-e: Move to the end of the form.
;; M-C-b: Move to the beginning of the sexp.
;; M-C-f: Move to the end of the sexp.

;; and the following miscellaneous commands.
;; M-h: Mark the current form
;; C-c): Check the current region for balanced parentheses.
;; C-cC-): Check the current form for balanced parentheses.

;; Maxima mode has the following completions command:
;; M-TAB: Complete the Maxima symbol as much as possible, providing
;;      a completion buffer if there is more than one possible completion.
;;      If the variable `maxima-use-dynamic-complete' is non-nil, then
;;      M-TAB will cycle through possible completions.

;; Portions of the buffer can be sent to a Maxima process.  (If a process is 
;; not running, one will be started.)
;; C-cC-r: Send the region to Maxima.
;; C-cC-b: Send the buffer to Maxima.
;; C-cC-c: Send the line to Maxima.
;; C-cC-e: Send the form to Maxima.
;; C-RET: Send the smallest set of lines which contains
;;        the cursor and contains no incomplete forms, and go to the next form.
;; M-RET:  As above, but with the region instead of the current line.
;; C-cC-l: Prompt for a file name to load into Maxima.
;;
;; When something is sent to Maxima, a buffer running an inferior Maxima 
;; process will appear.  It can also be made to appear by using the command
;; C-c C-p.
;; When a command is given to send information to Maxima, the region
;; (buffer, line, form) is first checked to make sure the parentheses
;; are balanced.  With an argument, they will not be checked first.
;; The Maxima process can be killed, after asking for confirmation 
;; with C-cC-k.  To kill without confirmation, give C-cC-k
;; an argument.

;; By default, indentation will be to the same level as the 
;; previous line, with an additional space added for open parentheses.
;; The behaviour of indent can be changed by the command 
;; M-x maxima-change-indent-style.  The possibilities are:
;; Standard:      Simply indent
;; Perhaps smart: Tries to guess an appropriate indentation, based on
;;                open parentheses, "do" loops, etc.
;; The default can be set by setting the value of the variable 
;; "maxima-indent-style" to either 'standard or 'perhaps-smart.
;; In both cases, M-x maxima-untab will remove a level of indentation.

;; To get help on a Maxima topic, use:
;; C-c C-d.
;; To read the Maxima info manual, use:
;; C-c C-m.
;; To get help with the symbol under point, use:
;; C-cC-h or f12.
;; To get apropos with the symbol under point, use:
;; C-cC-a or M-f12.


;; ** Running Maxima interactively **
;; To run Maxima interactively in a buffer, type M-x maxima
;; In the Maxima process buffer,
;; return will check the line for balanced parentheses, and send line as input.
;; Control return will send the line as input without checking for balanced
;; parentheses.

;; <M-tab> will complete the Maxima symbol as much as possible, providing
;;      a completion buffer if there is more than one possible completion.
;;      If `maxima-use-dynamic-complete' is non-nil, then <M-tab> will
;;      cycle through possible completions.

;; <C-M-tab> will complete the input line, based on previous input lines.
;; C-c C-d will get help on a Maxima topic.
;; C-c C-m will bring up the Maxima info manual.
;; C-cC-k will kill the process and the buffer, after asking for
;;   confirmation.  To kill without confirmation, give C-M-k an
;;   argument.

;; To scroll through previous commands,
;; M-p will bring the previous input to the current prompt,
;; M-n will bring the next input to the prompt.
;; M-r will bring the previous input matching
;;   a regular expression to the prompt,
;; M-s will bring the next input matching
;;   a regular expression to the prompt.

;; Debugging:
;; Maxima's source level debugging capabilities can be accessed through
;; Emacs.  A user can set a breakpoint at a line in a file (which has 
;; been loaded using, say, batchload), and then step line by line
;; from there.  The call stack may be examined, together with the 
;; variables bound at that level.  The current position in the source 
;; file will be displayed in the other half of the window.
;; The following keys are available to assist.
;; C-x C-a C-s Step one source line with display. ("Step")
;; C-x C-a C-n Step one source line (skip functions). ("Next")
;; C-x C-a TAB Step one instruction with display. ("Stepi")
;; C-x C-a C-r Continue with display.  ("Resume")
;; C-x C-a C-f Finish executing current function. ("Finish")
;; C-x C-a < Go up stack frames. ("Up")
;; C-x C-a > Go down stack frames. ("Down")
;; M-x maxima-debug-refresh Redraw the source file buffer.

;; A breakpoint can be set to the current line in the source file
;; with the command C-x SPC.  (The command can be 
;; entered either in the source file buffer or the Maxima process
;; buffer.)

;;; Code:

;;;; First
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;;;; The requires and provides

(require 'comint)
(require 'easymenu)
(require 'maxima-font-lock)
(provide 'maxima)

;;;; The variables that the user may wish to change

(defgroup maxima nil
  "Maxima mode"
  :prefix "maxima-"
  :tag    "Maxima")

(defcustom maxima-indent-amount 2
  "*The amount of each indentation level in `maxima-mode'."
  :group 'maxima
  :type '(integer))

(defcustom maxima-use-dynamic-complete nil
  "*If non-nil, then M-TAB will complete words dynamically."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-indent-style 'standard
  "*Determines how `maxima-mode' will handle tabs.
Choices are 'standard, 'perhaps-smart"
  :group 'maxima
  :type '(choice :menu-tag "Indent style"
                 :tag      "Indent style"
                 (const standard) 
                 (const perhaps-smart)))

(defvar maxima-newline-style nil
  "For compatability.")

(defcustom maxima-info-dir "/usr/local/info/"
  "*The directory where the maxima info files are kept."
  :group 'maxima
  :type '(directory))

(defcustom maxima-info-index-file "maxima.info-15"
  "*The info file containing the index."
  :group 'maxima
  :type '(file))

(defcustom maxima-command "maxima"
  "*The command used to start Maxima."
  :group 'maxima
  :type 'file)

(defcustom maxima-use-tabs t
  "*If non-nil, indentation will use tabs."
  :group 'maxima
  :type 'boolean)

;;;; The other variables

(defconst maxima-temp-suffix 0
  "Temporary filename suffix.  Incremented by 1 for each filename.")

(defvar inferior-maxima-process nil
  "The Maxima process.")

(defvar inferior-maxima-computing-p nil
  "Non-nil if Maxima process is computing.")

(defvar inferior-maxima-question-p nil)

(defvar inferior-maxima-result nil
  "The last output of the Maxima process.")

(defvar inferior-maxima-lisp-level-flag nil
  "Non-nil means that the inferior Maxima process is in Lisp mode.")

(defvar inferior-maxima-debug-level-flag nil
  "Non-nil means that the inferior Maxima process is in debug mode.")

(defvar inferior-maxima-top-level-flag t
  "Non-nil means that the inferior Maxima process is at the top level.")

(defvar inferior-maxima-use-debug nil
  "Non-nil means have the debugger available.")

(defvar maxima-debug-last-frame nil)

(defvar maxima-debug-last-frame-displayed-flag t)

(defvar maxima-debug-delete-prompt-marker nil)

(defvar maxima-debug-filter-accumulator nil)

(defvar maxima-debug-overlay nil)

(defvar maxima-debug-highlight-face 'highlight)

(defvar maxima-debug-downcase-filenames
  (string-match "nt[45]" system-configuration)
  "Force the case to be lower when sending a break command." )

(defvar maxima-debug-dirs  nil)

(defvar maxima-name-letters "?%_a-zA-Z0-9")

(defvar maxima-special-symbol-letters "!:='")

(defvar maxima-input-end 0
  "The end of the latest input that was sent to Maxima.")

(defvar maxima-real-input-end 0
  "The end of the latest input that was sent to Maxima.
This doesn't include answers to questions.")

(defvar inferior-maxima-exit-hook '())

;;;; Utility functions

(defun maxima-line-beginning-position ()
  (if (not (fboundp 'line-beginning-position))
      (save-excursion
	(beginning-of-line)
	(point))
    (line-beginning-position)))

(defun maxima-line-end-position ()
  (if (not (fboundp 'line-end-position))
      (save-excursion
	(end-of-line)
	(point))
    (line-end-position)))

(defun maxima-form-beginning-position ()
  (save-excursion
    (maxima-beginning-of-form)
    (point)))

(defun maxima-name-beginning ()
  (save-excursion
    (skip-chars-backward maxima-name-letters)
    (point)))

(defun maxima-name-end ()
  (save-excursion
    (skip-chars-forward maxima-name-letters)
    (point)))

(defun maxima-special-symbol-beginning ()
  (save-excursion
    (skip-chars-backward maxima-special-symbol-letters)
    (point)))

(defun maxima-special-symbol-end ()
  (save-excursion
    (skip-chars-forward maxima-special-symbol-letters)
    (point)))

(defun maxima-make-temp-name ()
  "Return a unique filename."
  (setq maxima-temp-suffix (+ maxima-temp-suffix 1))
  (concat (concat (make-temp-name "#mz") "-")
          (int-to-string maxima-temp-suffix)
          ".max"))

;;;; Maxima mode functions

;;; Position functions

(defun maxima-in-comment-p ()
  "Non-nil means that the point is in a comment."
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

(defun maxima-in-multiline-comment-p ()
  "Non-nil means that the point is in a multiline comment."
  (let ((ok t) 
	(bl (maxima-line-beginning-position))
	(result nil))
    (save-excursion
      (while ok
	(cond ((search-backward "/" (- (point) 2000) t)
	       (cond ((looking-at "/\\*")
		      (if (< (point) bl)
			  (setq result t))
		      (setq ok nil))
		     ((bobp)(setq ok nil))
		     ((progn (forward-char -1)
			     (looking-at "\\*/"))
		      (setq ok nil))))
	      (t (setq ok nil)))))
    ;; One more thing:  if the current-line begins with a close comment,
    ;; don't indent it with the comment, so this should return nil.
    (if result
	(save-excursion
	  (beginning-of-line)
	  (if (looking-at "[ \t]*\\*/")
	      (setq result nil))))
    result))

(defun maxima-paren-opens-block-p ()
  "Non-nil means that opening parenthesis begins a \"block\" statement."
  (save-excursion
    (skip-chars-backward " \t")
    (forward-char -5)			;"block"
    (if (looking-at "block")
	t
      nil)))

(defvar maxima-paren-nest (make-vector 20 nil))

(defun maxima-begin-paren ()
  "Find the point of the opening parenthesis for the point."
  (interactive)
  (let ((ok t)(level 0)(here (point)) last (max 0))
  (save-excursion
    (cond ((re-search-backward "[;$]" nil t)
	   (skip-chars-forward ";$ \t\n")
	   (if (> (point) here)
	       (goto-char here)))
	  (t (goto-char (point-min))
	      (maxima-forward-over-comment-whitespace)
	      (if (> (point) here)
		  (goto-char here))))
    (while ok
      (setq last (point))
      (cond ((re-search-forward "[][()]" here t)
	     (setq max (max level max))
	     (let ((ch (char-after (- (point) 1))))
	     (cond ((memq ch '( ?( ?[))  
		    (setq level (+ level 1))
		    (aset maxima-paren-nest level  (1- (point))))
		   (t
		    (setq level (- level 1))))))
	    (t (setq ok nil))))
      (aref maxima-paren-nest level))))

(defun maxima-within-loop-p (pt in-paren then-loop)
  "Non-nil means that the pt is within a loop that begins at the point.
This means either within parenthesis, a \"do\" loop, in a \"then\"
statement, or an \"else\" statement.
The arguments are pt, in-paren (non-nil means that the current loop
is a parentheses loop), then-loop (non-nil means that the current loop
is a \"then\" statement.
Okay, \"loop\" is being used loosely here."
  (save-excursion
    (if (maxima-end-of-loop in-paren then-loop)
	(if (< pt (point))
	    t
	  nil)
      t)))

(defun maxima-in-sharp-comment-line-p ()
  "Non-nil means that the point is in a comment line beginning with a sharp"
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*#")))


;;;; Motion functions

(defun maxima-forward-over-comment-whitespace ()
  "Move forward over comments and whitespace."
  (let ((ok t))
    (while  ok
      (skip-chars-forward " \t\n")
      (cond ((looking-at "/\\*")
	     (search-forward "*/" nil t ))
	    ((looking-at "\n[\t ]*#")
	     (forward-line 1)
	     (end-of-line))
	    ((looking-at "\n")
	     (forward-char 1))
	    (t  (setq ok nil))))))

(defun maxima-back-over-comment-whitespace ()
  "Move backward over comments and whitespace."
  (let ((ok t))
    (while  ok
      (skip-chars-backward " \t\n")
      (cond ((> (point) 2)
	     (forward-char -2)))
      (cond ((looking-at "\\*/")
	     (search-backward "/*" ))
	    (t (forward-char 2) (setq ok nil))))))

(defun maxima-end-of-loop (in-paren then-loop)
  "Go to the end of the current loop.
The arguments are in-paren (non-nil means the loop is a parenthesis
loop), and then-loop (non-nil means the loop is a then statement."
  (let ((search-for "\\<do\\>\\|\\<then\\>\\|")
	(keep-looking t)
	end
	end-mark)
    (if in-paren
	(setq end-mark ",")
      (setq end-mark "[;$]"))
    (if then-loop
	(setq end-mark (concat "\\<else\\>\\|" end-mark)))
    (setq search-for (concat search-for end-mark))
    (skip-chars-forward " \t\n")
    (if (looking-at "(")
	(if (maxima-forward-sexp)
	    (setq end nil)
	  (setq end (point)))
      (while keep-looking
	(if (re-search-forward search-for nil t)
	    (if (string-match end-mark (match-string 0))
		(progn
		  (setq end (point))
		  (setq keep-looking nil))
	      (if (not (maxima-end-of-loop in-paren 
					   (string= (match-string 0) "then")))
		  (progn
		    (setq end nil)
		    (setq keep-looking nil))))
	  (setq end nil)
	  (setq keep-looking nil))))
    end))
	
;;; This next one is stolen from gud.el
(defun maxima-forward-sexp ()
  "Version of `forward-sexp' that catches errors."
  (condition-case nil
      (forward-sexp)
    (error t)))

(defun maxima-beginning-of-form ()
  "Move to the beginning of the form."
  (interactive)
  (let ((tem (re-search-backward "[;$]" (point-min) t)))
    (cond (tem
	   (cond ((maxima-in-comment-p)
		  (search-backward "/\\*" nil t)
		  (maxima-beginning-of-form))
		 ((maxima-in-sharp-comment-line-p)
		  (beginning-of-line)
		  (or (equal (point) (point-min)) (forward-char -1))
		  (maxima-beginning-of-form))
		 (t  (forward-char 1))))
	  (t (goto-char  (point-min))))
    (maxima-forward-over-comment-whitespace)))

(defun maxima-end-of-form ()
  "Move to the end of the form."
  (interactive)
  (let ((tem (re-search-forward "[;$]" (point-max) t)))
    (cond (tem
	   (cond ((maxima-in-comment-p)
		  (search-forward "*/" nil t)
		  (maxima-end-of-form))
		 ((maxima-in-sharp-comment-line-p)
		  (end-of-line) (forward-char 1)
		  (maxima-end-of-form))))
	  (t (error "No ; or $ at end.")))))

(defun maxima-beginning-of-sexp ()
  "Move to the beginning of the current sexp."
  (interactive)
  (let ((pt (maxima-begin-paren)))
    (if pt
	(goto-char (maxima-begin-paren))
      (message "No opening parenthesis."))))

(defun maxima-end-of-sexp ()
  "Move to the end of the current sexp."
  (interactive)
  (let ((keep-looking t)
	(found nil)
	pt)
    (save-excursion
      (while (and keep-looking (search-backward "(" nil t))
	(if (not (maxima-in-comment-p))
	    (setq keep-looking nil)))
      (if keep-looking
	  (message "Not in parentheses.")
	(setq pt (point))
	(maxima-forward-sexp)
	(if (= pt (point))
	    (message "No end parentheses.")
	  (setq pt (- (point) 1))
	  (setq found t))))
    (if found
	(goto-char pt))))


;;;; Newlines and indents

(defun maxima-indent-form ()
  "Indent the entire form."
  (interactive)
  (save-excursion
    (let ((beg (progn (maxima-beginning-of-form)
		      (point)))
	  (end (progn (maxima-end-of-form) (point))))
      (goto-char beg)
      (while (< (point) end)
	(maxima-indent-line)
	(forward-line 1)))))

;;; 'standard
(defun maxima-standard-newline ()
  "Insert a newline and indent following line like previous line.
Unless the previous line opened a parenthesis, in which case 1 is
added to the indentation, or if the previous line closed a parenthesis, 
then 1 is subtracted."
  (interactive)
  (let ((indent (current-indentation)))
    (let ((pt (maxima-line-beginning-position)))
      (save-excursion
	(while (re-search-backward "[()]" pt t)
	  (cond ((string= (match-string 0) ")")
		 (setq indent (- indent 1)))
		((string= (match-string 0) "(")
		 (setq indent (+ indent 1)))))))
    (newline)
    (indent-line-to (max indent 0))))

(defun maxima-standard-indent ()
  "Add a level of indentation"
  (interactive)
  (indent-to (+ (current-indentation) maxima-indent-amount)))

(defun maxima-untab ()
  "Delete a level of indentation"
  (interactive)
  ;; Check to see if the previous maxima-indent-amount spaces are 
  ;; on the same line and blank
  (let ((i maxima-indent-amount)
	(ok t)
	(pt (point)))
    (save-excursion
      (while (and (> i 0) ok)
	(setq i (- i 1))
	(forward-char -1)
	(if (not (looking-at " "))
	    (setq ok nil))
	(if (looking-at "\n")
	    (setq ok nil))))
    (if ok
	(delete-region pt (- pt maxima-indent-amount)))))

;;; 'perhaps-smart
(defun maxima-perhaps-smart-calculate-indent ()
  "Return appropriate indentation for current line as Maxima code.
Returns an integer: the column to indent to."
  (save-excursion
    ;; Start off with an indentation of 0, then work from there.
    (let ((indent 0)
	  (begin-construct nil)
	  (close-char "[;\\$()]")
	  (keep-looking t)
	  current-point
	  pt
	  mstring)
      (save-excursion
	(beginning-of-line)
	(setq pt (maxima-begin-paren))
	(setq current-point (point))
	(if pt (setq close-char "[,()]"))
	;; Now, start looking backwards, find the first loop that the
	;; current point is in.
	(while (and keep-looking 
		    (re-search-backward 
		     "\\<then\\>\\|\\<do\\>\\|\\<else\\>" pt t))
	  (setq mstring (match-string 0))
	  (forward-char (length mstring))
	  (if (maxima-within-loop-p current-point pt 
				    (string= (match-string 0) "then"))
	      (progn 
		(setq begin-construct (point))
		(setq keep-looking nil)))
	  (backward-char (length mstring)))
	;; If begin-construct in non-nil, it tells us where the line
	;; that begins the current-construct it
	(if begin-construct
	    (progn
	      (goto-char begin-construct)
	      (setq indent (+ maxima-indent-amount 
			      (current-indentation))))
	  (if pt
	      (progn 
		(goto-char pt)
		(setq indent (+ 1 (current-column)))
		(if (maxima-paren-opens-block-p)
		    (setq indent (- indent 4)))))))
      ;;current-indentation?
      ;; Now, we need to possibly do some adjustments.
      ;; If the previous column doesn't end with close-char or a 
      ;; parenthesis, assume that the current line in a continuation 
      ;; of that line, and add 2 to the indentation, unless the 
      ;; previous line was the beginning of the construct containing 
      ;; the point or only an open parenthesis.
      (save-excursion
	(if (not (and begin-construct 
		      (< (count-lines begin-construct current-point) 2)))
	    (progn
	      (forward-line -1)
	      (end-of-line)
	      (skip-chars-backward " \t\n")
	      (if (not (looking-at "^"))
		  (progn 
		    (forward-char -1)
		    (if (not (looking-at close-char))
			(setq indent (+ maxima-indent-amount indent))))))))
      ;; Adjust for comments
      ;; In a multiline comment, add two spaces on indentation
      (if (maxima-in-multiline-comment-p)
	  (setq indent (+ indent maxima-indent-amount)))
      ;;Next, if the current line begins with an "else", then indent
      ;; just like the beginning "then"
      (save-excursion
	(beginning-of-line)
	(if (looking-at "[ \t]*else")
	    (progn
	      (maxima-back-over-comment-whitespace)
	      (forward-char -1)
	      (setq current-point (point))
	      (setq keep-looking t)
	      (while keep-looking
		(if (re-search-backward "then" pt t)
		    (progn
		      (forward-char 4)
		      (if (maxima-within-loop-p current-point pt t)
			  (progn
			    (setq indent (current-indentation))
			    (setq keep-looking nil))))
		  (setq keep-looking nil))))))
      ;; Finally, if the line is the first line, don't indent at all
      (save-excursion
	(beginning-of-line)
	(if (= (point) 1)
	    (setq indent 0)))
      indent)))

(defun maxima-perhaps-smart-indent (&optional whole-exp)
  "Indent current line as Maxima code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive)
  (let ((indent (maxima-perhaps-smart-calculate-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
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
	 (indent-code-rigidly beg end shift-amt))))

(defun maxima-perhaps-smart-newline ()
  "Indent current line, insert newline and go to the appropriate column."
  (interactive)
  (maxima-perhaps-smart-indent)
  (newline)
  (maxima-perhaps-smart-indent))

(cond
; ((eq maxima-newline-style 'basic)
;  (defun maxima-indent-line ()
;    (interactive)
;    (maxima-standard-indent))
;  (defun maxima-newline ()
;    (interactive)
;    (newline)))
 ((eq maxima-indent-style 'standard)
  (defun maxima-indent-line ()
    (interactive)
    (maxima-standard-indent))
  (defun maxima-newline ()
    (interactive)
    (maxima-standard-newline)))
 ((eq maxima-indent-style 'perhaps-smart)
  (defun maxima-indent-line ()
    (interactive)
    (maxima-perhaps-smart-indent))
  (defun maxima-newline ()
    (interactive)
    (maxima-perhaps-smart-newline))))

(defun maxima-change-indent-style (new-style)
  "Change the newline style."
  (interactive "sNewline style (insert \"s\" for standard, or \"p\" for perhaps-smart): ")
  (cond
;   ((string= new-style "b")
;    (setq maxima-indent-style 'basic)
;    (defun maxima-indent-line ()
;      (interactive)
;      (maxima-standard-indent))
;    (defun maxima-newline ()
;      (interactive)
;      (newline)))
   ((string= new-style "s")
    (setq maxima-indent-style 'standard)
    (defun maxima-indent-line ()
      (interactive)
      (maxima-standard-indent))
    (defun maxima-newline ()
      (interactive)
      (maxima-standard-newline)))
   ((string= new-style "p")
    (setq maxima-indent-style 'perhaps-smart)
    (defun maxima-indent-line ()
      (interactive)
      (maxima-perhaps-smart-indent))
    (defun maxima-newline ()
      (interactive)
      (maxima-perhaps-smart-newline)))))

(defun maxima-reindent-line ()
  "Reindent the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (maxima-indent-line))
  (if (looking-at " *$")
      (end-of-line)))

;;;; Commenting

(defun maxima-insert-short-comment ()
  "Prompt for a comment."
  (interactive)
  (let ((comment (read-string "Comment: ")))
    (insert "/* " comment " */")
    (maxima-newline)))

(defun maxima-insert-long-comment ()
  "Insert a comment enviroment"
  (interactive)
  (insert "/*")
  (maxima-newline)
  (maxima-newline)
  (insert "*/")
  (forward-line -1)
  (insert "  "))

(defun maxima-uncomment-region (beg end)
  "`uncomment-region' to use with the menu."
  (interactive "r")
  (comment-region beg end (universal-argument)))


;;;; Help functions

(defun maxima-get-help ()
  "Get help on a given subject"
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "* ")
    (setq pt (point))
    (search-forward ":")
    (skip-chars-backward ": ")
    (setq name (buffer-substring-no-properties pt (point)))
    (skip-chars-forward ": ")
    (setq pt (point))
    (end-of-line)
    (skip-chars-backward ". ")
    (setq place (buffer-substring-no-properties pt (point))))
  (if (not running-xemacs)
      (info (concat "(" maxima-info-dir "maxima.info)" place))
    (info (concat maxima-info-dir "maxima.info"))
    (search-forward place)
    (Info-follow-nearest-node (point)))
  (re-search-forward (concat "-.*: *" name "\\>"))
  (beginning-of-line))

(defun maxima-get-fast-help (expr)
  "Go directly to the help item if possible." 
  (let 
      ((maxima-help-buffer 
	(get-buffer-create (concat "*Maxima Help*")))
       (index-buffer (find-file-noselect 
		      (concat maxima-info-dir maxima-info-index-file)))
       pt
       place)
    (save-excursion
      (set-buffer index-buffer)
      (goto-char 1)
      (search-forward "Menu:")
      (forward-line 1)
      (beginning-of-line)
      (re-search-forward (concat "\\* " expr ":") nil t)
      (skip-chars-forward ": ")
      (setq pt (point))
      (end-of-line)
      (skip-chars-backward ". ")
      (setq place (buffer-substring-no-properties pt (point)))
      (info (concat "(" maxima-info-dir "maxima.info)" place))
      (re-search-forward (concat "-.*: " expr))
      (beginning-of-line)
      (kill-buffer index-buffer))))

(defun maxima-help (&optional expr1)
  "Get help on a certain subject"
  (interactive)
  (let* ((maxima-help-buffer 
	  (get-buffer-create (concat "*Maxima Help*")))
	 (index-buffer (find-file-noselect 
			(concat maxima-info-dir maxima-info-index-file)))
	 (have-info nil)
	 expr
	 expr-line
	 (lmark))
    (if (not expr1) 
        (setq expr (read-string "Maxima Help: ")) (setq expr expr1)) 
    (set-buffer maxima-help-buffer)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Maxima help for " expr "\n\n")
    (insert "[RET] will get help on the subject on the given line\n")
    (insert "q in the *info* buffer will return you here.\n")
    (insert "q in this buffer will exit Maxima help\n\n")
    (set-buffer index-buffer)
    (goto-char 1)
    (search-forward "Menu:")
    (forward-line 1)
    (beginning-of-line)
    (while (re-search-forward (concat "\\*.*" expr ".*:") nil t)
      (setq have-info t)
      (setq expr-line  (buffer-substring-no-properties 
			(maxima-line-beginning-position) 
			(maxima-line-end-position)))
      (save-excursion
	(set-buffer maxima-help-buffer)
	(insert expr-line "\n")))
    (if have-info 
	(progn
	  (set-buffer maxima-help-buffer)
	  (defun maxima-help-subject ()
	    (interactive)
	    (maxima-get-help))
	  (defun maxima-kill-help ()
	    (interactive)
	    (let ((buf (current-buffer)))
	      (delete-window)
	      (kill-buffer buf)))
	  (use-local-map (make-sparse-keymap))
	  (define-key (current-local-map) "\C-m" 'maxima-help-subject)
	  (define-key (current-local-map) "q" 'maxima-kill-help)
	  (goto-char 1)
	  (pop-to-buffer maxima-help-buffer)
          (setq buffer-read-only t))
      (kill-buffer maxima-help-buffer)
      (message (concat "No help for *" expr "*")))))

(defun maxima-apropos-help ()
  (interactive)
  (maxima-help-dispatcher nil nil))

(defun maxima-completion-help ()
  (interactive)
  (maxima-help-dispatcher nil t))

(defun maxima-help-dispatcher (&optional arg1 arg2)
  (interactive)
  (cond
   ((or (looking-at "[a-zA-Z_]")
	(looking-at "?[a-zA-Z]")
	(looking-at "%[a-zA-Z]"))
    (if arg2 
	(maxima-context-help)
      (maxima-help (buffer-substring-no-properties
		 (maxima-name-beginning) (maxima-name-end)))))
   ((looking-at "?")
    (maxima-get-fast-help "\"\\?\""))
   ((looking-at "#")
    (maxima-get-fast-help "\"#\""))
   ((looking-at "\\.")
    (maxima-get-fast-help "\"\\.\""))
   ((looking-at "[:=!%']")
    (let ((expr (buffer-substring-no-properties
             (maxima-special-symbol-beginning) (maxima-special-symbol-end))))
	  (cond
	   ((or (string= expr "%") (string= expr "%%"))
	    (maxima-get-fast-help expr)) ; % and %% are without double quotes
	   ((string= expr "''")
	    (maxima-get-fast-help "\"")) ; "''" is called """ in the manual
	   ((or (string= expr ":") (string= expr "::") 
                (string= expr ":=") (string= expr "::=") 
                (string= expr "=") (string= expr "!") (string= expr "!!"))  
	    (maxima-get-fast-help (concat "\"" expr "\"")))
	   (t (error "no help for %s" expr)))))
   (arg1
    (error "No help for %s" (char-to-string (char-after (point)))))
   (t					; point is behind a name? 
    (save-excursion
      (progn
	(backward-char 1)
	(maxima-help-dispatcher t arg2))))))

(defun maxima-context-help ()
  (interactive)
  (let* ((stub  (buffer-substring-no-properties 
		 (maxima-name-beginning) (maxima-name-end)))
	 (completions (all-completions (downcase stub) maxima-symbols)))
    (setq completions 
	  (mapcar 
	   (function upcase) completions))
    (if (member (upcase stub) completions)
	(setq completions (list (upcase stub))))
    (cond ((null completions)
	   (message "No help for %s" stub))
	  ((= 1 (length completions))
	   (maxima-get-fast-help (car completions)))
	  (t				; There's no unique completion.
	   (maxima-help-variation completions)))))

(defun maxima-help-variation (completions)
  "Get help on certain subjects."
  (let* ((maxima-help-buffer 
	  (get-buffer-create (concat "*Maxima Help*")))
	 (index-buffer (find-file-noselect 
			(concat maxima-info-dir maxima-info-index-file)))
	 expr-line
	 (lmark))
    (set-buffer maxima-help-buffer)
    (erase-buffer)
    (insert "Maxima help\n")
    (insert "[RET] will get help on the subject on the given line\n")
    (insert "q in the *info* buffer will return you here.\n")
    (insert "q in this buffer will exit Maxima help\n\n")
    (set-buffer index-buffer)
    (goto-char 1)
    (search-forward "Menu:")
    (forward-line 1)
    (beginning-of-line)
    (defun maxima-help-insert-line (expr)
      (re-search-forward (concat "\\* " expr ":") nil t)
      (setq expr-line  (buffer-substring-no-properties 
			(maxima-line-beginning-position) 
			(maxima-line-end-position)))
      (save-excursion
	(set-buffer maxima-help-buffer)
	(insert expr-line "\n")))
    (mapcar (function maxima-help-insert-line) completions) 
    (set-buffer maxima-help-buffer)
    (defun maxima-help-subject ()
      (interactive)
      (maxima-get-help))
    (defun maxima-kill-help ()
      (interactive)
      (let ((buf (current-buffer)))
	(delete-window)
	(kill-buffer buf)))
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) "\C-m" 'maxima-help-subject)
    (define-key (current-local-map) "q" 'maxima-kill-help)
    (goto-char 1)
    (pop-to-buffer maxima-help-buffer)))

(defun maxima-info ()
  "Read the info file for Maxima."
  (interactive)
  (info (concat maxima-info-dir "maxima")))

;;;; Completion
;;; Use hippie-expand to help with completions
(require 'hippie-exp)
;(require 'maxima-symbols)

;;; This next one was mostly stolen from comint.el
(defun maxima-complete ()
  "Complete word from list of candidates.
A completions listing will be shown in a help buffer 
if completion is ambiguous."
  (interactive)
  (let* ((stub  (buffer-substring-no-properties 
                 (maxima-name-beginning) (point)))
	 (completions (all-completions (downcase stub) maxima-symbols)))
    (setq completions 
          (mapcar 
           (function (lambda (x) (he-transfer-case stub x))) completions))
    (cond ((null completions)
	   (message "No completions of %s" stub))
	  ((= 1 (length completions))	; Gotcha!
	   (let ((completion (car completions)))
	     (if (string-equal completion stub)
		 (message "Sole completion")
	       (insert (substring completion (length stub)))
	       (message "Completed"))))
	  (t				; There's no unique completion.
             (comint-dynamic-list-completions completions)))))

(defun maxima-he-try (old)
  (interactive)
  (if (not old)
      ;;; let beg be the beginning of the word
      (progn
        (he-init-string (maxima-name-beginning) (point))
        (setq he-expand-list 
              (all-completions (downcase he-search-string) maxima-symbols))
        (setq he-expand-list 
              (mapcar (function 
                      (lambda (x) (he-transfer-case he-search-string x)))
                      he-expand-list))
        (if he-expand-list
            (he-substitute-string (car he-expand-list))
          nil))
    (setq he-expand-list (cdr he-expand-list))
    (if he-expand-list
        (he-substitute-string (car he-expand-list))
      (he-reset-string)
      nil)))

(fset 'maxima-dynamic-complete 
      (make-hippie-expand-function '(maxima-he-try)))

;;;; Miscellaneous

(defun maxima-mark-form ()
  "Make the current form the region."
  (interactive)
  (maxima-beginning-of-form)
  (let ((tem (point)))
    (maxima-end-of-form)
    (set-mark (point))
    (goto-char tem)))

(defun maxima-check-parens (beg end)
  "Check to make sure that the parentheses are balanced in the region."
  (interactive "r")
  (let ((keep-going t)
	(level 0)
	pt
        paren)
    (save-excursion
      (goto-char beg)
      (while (and keep-going (re-search-forward "[()]" end t))
        (setq paren (char-to-string (char-before (point))))
	(if (not (maxima-in-comment-p))
	    (progn
	      (cond
	       ((string= paren "(")
		(setq level (+ level 1)))
	       ((string= paren ")")
		(setq level (- level 1))
		(setq pt (- (point) 1))
		(if (< level 0)
		    (setq keep-going nil))))))))
    (cond
     ((= level 0)			; All's fine
      t)
     ((< level 0)			; Too many close parens
      (goto-char pt)
      (message "Mismatched parentheses")
      nil)
     ((> level 0)			; Too many open parens
      ;; First of all, go to the last paren that isn't in a comment.
      (setq keep-going t)
      (goto-char end)
      (while keep-going
	(re-search-backward "[()]")
	(if (not (maxima-in-comment-p))
	    (setq keep-going nil)))
      ;; If it is an open paren, then there's an error.
      (if (string= (match-string 0) "(")
	  (progn
	    (message "Mismatched parentheses")
	    nil)
	;; If it is a close paren, find the matching open paren
	(goto-char (maxima-begin-paren))
	;; Next, find the previous open paren that isn't in a comment
	(setq keep-going t)
	(while keep-going
	  (search-backward "(")
	  (if (not (maxima-in-comment-p))
	      (setq keep-going nil)))
	(message "Mismatched parentheses")
	nil)))))

(defun maxima-check-form-parens ()
  "Check to see if the parentheses in the current form are balanced."
  (interactive)
  (let (beg end)
    (save-excursion
      (maxima-beginning-of-form)
      (setq beg (point))
      (maxima-end-of-form)
      (setq end (point)))
    (maxima-check-parens beg end)))

(defun maxima-load-file (file)
  "Prompt for a Maxima file to load."
  (interactive "fMaxima file: ")
  (maxima-string (concat "load(\"" (expand-file-name file) "\");")))

;;;; Syntax table

(defvar maxima-mode-syntax-table nil "")

(if (not maxima-mode-syntax-table)
    (let ((i 0))
      (setq maxima-mode-syntax-table (make-syntax-table))
      (modify-syntax-entry ?_ "w" maxima-mode-syntax-table)
      (modify-syntax-entry ?% "w" maxima-mode-syntax-table)
      (modify-syntax-entry ?? "w" maxima-mode-syntax-table)
;;       (while (< i ?0)
;; 	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
;; 	(setq i (1+ i)))
;;       (setq i (1+ ?9))
;;       (while (< i ?A)
;; 	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
;; 	(setq i (1+ i)))
;;       (setq i (1+ ?Z))
;;       (while (< i ?a)
;; 	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
;; 	(setq i (1+ i)))
;;       (setq i (1+ ?z))
;;       (while (< i 128)
;; 	(modify-syntax-entry i "_   " maxima-mode-syntax-table)
;; 	(setq i (1+ i)))
      (modify-syntax-entry ?  "    " maxima-mode-syntax-table)
      (modify-syntax-entry ?\t "   " maxima-mode-syntax-table)
      (modify-syntax-entry ?` "'   " maxima-mode-syntax-table)
      (modify-syntax-entry ?' "'   " maxima-mode-syntax-table)
      (modify-syntax-entry ?, "'   " maxima-mode-syntax-table)
      (modify-syntax-entry ?. "w" maxima-mode-syntax-table)
      (modify-syntax-entry ?# "'   " maxima-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\" maxima-mode-syntax-table)
      (modify-syntax-entry ?/ ". 14" maxima-mode-syntax-table)
      (modify-syntax-entry ?* ". 23" maxima-mode-syntax-table)
      (modify-syntax-entry ?+ "." maxima-mode-syntax-table)
      (modify-syntax-entry ?- "." maxima-mode-syntax-table)
      (modify-syntax-entry ?= "." maxima-mode-syntax-table)
      (modify-syntax-entry ?< "." maxima-mode-syntax-table)
      (modify-syntax-entry ?> "." maxima-mode-syntax-table)
      (modify-syntax-entry ?& "." maxima-mode-syntax-table)
      (modify-syntax-entry ?| "." maxima-mode-syntax-table)
      (modify-syntax-entry ?\" "\"    " maxima-mode-syntax-table)
      (modify-syntax-entry ?\\ "\\   " maxima-mode-syntax-table)
      (modify-syntax-entry ?\( "()  " maxima-mode-syntax-table)
      (modify-syntax-entry ?\) ")(  " maxima-mode-syntax-table)
      (modify-syntax-entry ?\[ "(]  " maxima-mode-syntax-table)
      (modify-syntax-entry ?\] ")[  " maxima-mode-syntax-table)))


;;;; Keymap

(defvar maxima-mode-map nil
  "The keymap for maxima-mode")

(if maxima-mode-map
    nil
  (let ((map (make-sparse-keymap)))
    ;; Motion
    (define-key map "\M-\C-a" 'maxima-beginning-of-form)
    (define-key map "\M-\C-e" 'maxima-end-of-form)
    (define-key map "\M-\C-b" 'maxima-beginning-of-sexp)
    (define-key map "\M-\C-f" 'maxima-end-of-sexp)
    ;; Process
    (define-key map "\C-c\C-p" 'maxima-display-buffer)
    (define-key map "\C-c\C-r" 'maxima-send-region)
    (define-key map "\C-c\C-b" 'maxima-send-buffer)
    (define-key map "\C-c\C-c" 'maxima-send-line)
    (define-key map "\C-c\C-e" 'maxima-send-form)
    (define-key map [(control return)] 
      'maxima-send-full-line-and-goto-next-form)
    (define-key map [(meta return)] 
      'maxima-send-completed-region-and-goto-next-form)
    (define-key map [(control meta return)] 'maxima-send-buffer)
    (define-key map "\C-c\C-k" 'maxima-stop)
    (define-key map "\C-c\C-l" 'maxima-load-file)
    ;; Completion
    (if maxima-use-dynamic-complete
        (define-key map (kbd "M-TAB") 'maxima-dynamic-complete)        
      (define-key map (kbd "M-TAB") 'maxima-complete))
    ;; Commenting
    (define-key map "\C-c;" 'comment-region)
    (define-key map "\C-c:" 'maxima-uncomment-region)
    (define-key map "\M-;" 'maxima-insert-short-comment)
    (define-key map "\C-c*" 'maxima-insert-long-comment)
    ;; Indentation
;    (define-key map "\t" 'maxima-reindent-line)
;    (define-key map "\C-m" 'maxima-newline)
    (define-key map "\M-\C-q" 'maxima-indent-form)
;    (define-key map [(control tab)] 'maxima-untab)
    ;; Help
    (define-key map "\C-c\C-d" 'maxima-help)
    (define-key map "\C-c\C-m" 'maxima-info)
    (define-key map [(f12)] 'maxima-completion-help)
    (define-key map "\C-c\C-h" 'maxima-completion-help)
    (define-key map [(meta f12)] 'maxima-apropos-help)
    (define-key map "\C-c\C-a" 'maxima-apropos-help)
    ;; Misc
    (define-key map "\M-h" 'maxima-mark-form)
    (define-key map "\C-c\)" 'maxima-check-parens)
    (define-key map "\C-cC-\)" 'maxima-check-form-parens)
    (define-key map "\177" 'backward-delete-char-untabify)
    (define-key map "\C-x " 'maxima-debug-set-break)
    (setq maxima-mode-map map)))

;;;; Menu

(easy-menu-define maxima-mode-menu maxima-mode-map "Maxima mode menu"
  '("Maxima"
    ("Motion"
     ["Beginning of form" maxima-beginning-of-form t]
     ["End of form" maxima-end-of-form t]
     ["Beginning of sexp" maxima-beginning-of-sexp t]
     ["End of sexp" maxima-end-of-sexp t])
    ("Process"
     ["Start process" maxima-start t]
     ["Send region" maxima-send-region t]
     ["Send buffer" maxima-send-buffer t]
     ["Send line" maxima-send-line t]
     ["Send form" maxima-send-form t]
     ["Load file" maxima-load-file t]
     "----"
     ["Display buffer" maxima-display-buffer t]
     "----"
     ["Kill process" maxima-stop t])
    ("Indentation"
;     ["Change to basic" (maxima-change-indent-style "b")  
;      (not (eq maxima-newline-style 'basic))]
     ["Change to standard" (maxima-change-indent-style "s")  
      (not (eq maxima-indent-style 'standard))]
     ["Change to smart" (maxima-change-indent-style "p")  
      (not (eq maxima-indent-style 'perhaps-smart))])
    ("Misc"
     ["Mark form" maxima-mark-form t]
     ["Check parens in region" maxima-check-parens t]
     ["Check parens in form" maxima-check-form-parens t]
     ["Comment region" comment-region t]
     ["Uncomment region" maxima-uncomment-region t])
    ("Help"
     ["Maxima info" maxima-info t]
     ["Help" maxima-help t])))


;;;; Variable setup
;;;; (These are used in both maxima-mode and inferior-maxima-mode).

(defvar maxima-mode-abbrev-table nil "")

(defun maxima-mode-variables ()
  "Sets all the necessary variables for maxima-mode"
  (set-syntax-table maxima-mode-syntax-table)
  (setq local-abbrev-table maxima-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'maxima-reindent-line)
  (make-local-variable 'indent-tabs-mode)
  (unless maxima-use-tabs
    (setq indent-tabs-mode nil))
  (make-local-variable 'comment-start)
  (setq comment-start "/*")
  (make-local-variable 'comment-end)
  (setq comment-end "*/")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\*+ *")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'maxima-comment-indent))


;;;; Maxima mode

(defun maxima-mode ()
  "Major mode for editing Maxima code.

Maxima mode provides the following motion commands:
\\[maxima-beginning-of-form]: Move to the beginning of the form.
\\[maxima-end-of-form]: Move to the end of the form.
\\[maxima-beginning-of-sexp]: Move to the beginning of the sexp.
\\[maxima-end-of-sexp]: Move to the end of the sexp.

and the following miscellaneous commands.
\\[maxima-mark-form]: Mark the current form
\\[maxima-check-parens]: Check the current region for balanced parentheses.
\\[maxima-check-form-parens]: Check the current form for balanced parentheses.

Maxima mode has the following completions commands:
M-TAB: Complete the Maxima symbol as much as possible, providing
     a completion buffer if there is more than one possible completion.
     If `maxima-use-dynamic-complete' is non-nil, then simple cycle 
     through possible completions.

Portions of the buffer can be sent to a Maxima process.  (If a process is 
not running, one will be started.)
\\[maxima-send-region]: Send the region to Maxima.
\\[maxima-send-buffer]: Send the buffer to Maxima.
\\[maxima-send-line]: Send the line to Maxima.
\\[maxima-send-form]: Send the form to Maxima.
\\[maxima-send-full-line-and-goto-next-form]: Send the smallest set of lines which contains
   the cursor and contains no incomplete forms, and go to the next form.
\\[maxima-send-completed-region-and-goto-next-form]:  As above, but with
   the region instead of the current line.
\\[maxima-load-file] will prompt for a filename and load it into Maxima
When something is sent to Maxima, a buffer running an inferior Maxima 
process will appear.  It can also be made to appear by using the command
\\[maxima-display-buffer].
If an argument is given to a command to send information to Maxima,
the region (buffer, line, form) will first be checked to make sure
the parentheses are balanced.
The Maxima process can be killed, after asking for confirmation 
with \\[maxima-stop].  To kill without confirmation, give \\[maxima-stop]
an argument.

By default, indentation will be to the same level as the 
previous line, with an additional space added for open parentheses.
The behaviour of indent can be changed by the command 
\\[maxima-change-indent-style].  The possibilities are:
Standard:      Standard indentation.
Perhaps smart: Tries to guess an appropriate indentation, based on
               open parentheses, \"do\" loops, etc.
The default can be set by setting the value of the variable 
\"maxima-indent-style\" to either 'standard or 'perhaps-smart.
In both cases, \\[maxima-untab] will remove a level of indentation.

To get help on a Maxima topic, use:
\\[maxima-help].
To read the Maxima info manual, use:
\\[maxima-info].
To get help with the symbol under point, use:
\\[maxima-completion-help].
To get apropos with the symbol under point, use:
\\[maxima-apropos-help].

\\{maxima-mode-map}
"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'maxima-mode)
  (setq mode-name "Maxima")
  (use-local-map maxima-mode-map)
  (maxima-mode-variables)
  (cond
   ((eq maxima-newline-style 'basic)
    (setq maxima-indent-style 'standard))
   ((eq maxima-newline-style 'standard)
    (setq maxima-indent-style 'standard))
   ((eq maxima-newline-style 'perhaps-smart)
    (setq maxima-indent-style 'perhaps-smart)))
  (easy-menu-add maxima-mode-menu maxima-mode-map)
  (run-hooks 'maxima-mode-hook))


;;;; Interacting with the Maxima process

;;;; Starting and stopping

(defun maxima-start ()
  "Start the Maxima process."
  (interactive)
  (setq inferior-maxima-computing-p t)
  (setq inferior-maxima-question-p nil)
  (if (processp inferior-maxima-process)
      (unless (eq (process-status inferior-maxima-process) 'run)
        (delete-process inferior-maxima-process)
        (save-excursion
          (set-buffer "*maxima*")
          (erase-buffer))
        (setq inferior-maxima-process nil)))
  (unless (processp inferior-maxima-process)
    (setq maxima-input-end 0)
    (let ((mbuf (make-comint "maxima" maxima-command)))
      (save-excursion
        (set-buffer mbuf)
        (setq inferior-maxima-process (get-buffer-process mbuf))
        (while (not (maxima-new-prompt-p))
          (sleep-for 0.100))
        (inferior-maxima-mode)))))

(defun maxima-stop (&optional arg)
  "Kill the currently running Maxima process."
  (interactive "P")
  (if (processp inferior-maxima-process)
      (if arg
	  (progn 
	    (delete-process inferior-maxima-process)
	    (kill-buffer "*maxima*")
	    (setq inferior-maxima-process nil))
	(if (y-or-n-p "Really quit Maxima? ")
	    (progn
	      (delete-process inferior-maxima-process)
	      (kill-buffer "*maxima*")
	      (setq inferior-maxima-process nil))))))

;;;; Sending information to the process

(defun maxima-prompt ()
  "Return the point of the last prompt in the maxima process buffer."
  (save-excursion
    (set-buffer (process-buffer inferior-maxima-process))
    (goto-char (point-max))
    (re-search-backward inferior-maxima-prompt)
    (match-end 0)))

(defun maxima-new-prompt-p ()
  "Check to see if there is a new prompt after the last input."
  (save-excursion
    (set-buffer (process-buffer inferior-maxima-process))
    (goto-char maxima-input-end)
    (or
     (re-search-forward inferior-maxima-prompt (point-max) t)
     (re-search-forward "?" (point-max) t)
     (re-search-forward "Inferior Maxima Finished" (point-max) t))))

(defun maxima-question-p ()
  "Check to see if there is a question"
  (let ((prompt))
    (save-excursion
      (set-buffer (process-buffer inferior-maxima-process))
      (goto-char (point-max))
      (re-search-backward "?" maxima-input-end t))))

(defun maxima-finished-p ()
  "Check to see if the Maxima process has halted"
  (not (maxima-running)))

(defun maxima-strip-string (string)
  "Remove any spaces or newlines at the beginning and end of the string"
  (while (or
          (string= "\n" (substring string -1))
          (string= " " (substring string -1)))
    (setq string (substring string 0 -1)))
  (while (or
          (string= "\n" (substring string 0 1))
          (string= " " (substring string 0 1)))
    (setq string (substring string 1)))
  string)

(defun maxima-single-string (string &optional nonewinput)
  "Send a string to the Maxima process."
  (setq string (maxima-strip-string string))
  (let ((prompt))
    (maxima-start)
    (save-excursion
      (set-buffer (process-buffer inferior-maxima-process))
      (goto-char (point-max))
      (insert string)
      (setq maxima-input-end (point))
      (unless nonewinput
        (setq maxima-real-input-end (point)))
      (comint-send-input);)
      (while (not (maxima-new-prompt-p))
        (sit-for 0.100))
      (goto-char (point-max)))
    (when (maxima-question-p)
      (let ((ans (read-string (maxima-question))))
        (unless (string-match "[;$]" ans)
          (setq ans (concat ans ";")))
        (maxima-single-string ans t)))))

(defun maxima-question ()
  "Return inferior-maxima-result with whitespace trimmed off the ends.
For use when the process asks a question."
  (let ((qn))
    (save-excursion
      (set-buffer (get-buffer "*maxima*"))
      (goto-char (point-max))
      (search-backward "?")
      (setq qn (buffer-substring-no-properties 
                (maxima-line-beginning-position) (maxima-line-end-position)))
      (goto-char (point-max)))
    (concat qn " ")))

;(defun maxima-get-lisp-end (string)
;  (with-temp-buffer
;    (insert string)
;    (beginning-of-buffer)
;    (search-forward ":lisp")
;    (forward-sexp)
;    (- (point) 1)))

(defun maxima-get-lisp-end (string)
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (end))
    (save-excursion
      (set-buffer tmpbuf)
      (make-local-hook 'kill-buffer-hook)
      (setq kill-buffer-hook nil)
      (insert string)
      (beginning-of-buffer)
      (search-forward ":lisp")
      (forward-sexp)
      (setq end (- (point) 1)))
    (kill-buffer tmpbuf)
    end))

(defun maxima-send-block (stuff)
  "Send a block of code to Maxima."
  (maxima-start)
  (while (or
          (string-match "[$;]" stuff)
          (eq (string-match "[ \n]*:lisp" stuff) 0))
    (if (eq (string-match "[ \n]*:lisp" stuff) 0)
        (setq end (maxima-get-lisp-end stuff))
      (setq end (1+ (string-match "[$;]" stuff))))
    (maxima-single-string (substring stuff 0 end))
    (setq stuff (substring stuff end))))

;;; Sending information to the process should be done through these
;; next four commands

(defun maxima-string (string)
  "Send a string to the Maxima process."
  (setq inferior-maxima-computing-p t)
  (setq inferior-maxima-question-p nil)
  (maxima-send-block string)
  (maxima-display-buffer))

(defun maxima-string-nodisplay (string)
  "Send a string to the Maxima process."
  "Send a string to the Maxima process."
  (setq inferior-maxima-computing-p t)
  (setq inferior-maxima-question-p nil)
  (maxima-send-block string))
    
(defun maxima-region (beg end)
  "Send the region to the Maxima process."
  (maxima-string
   (buffer-substring-no-properties beg end)))

(defun maxima-region-nodisplay (beg end)
  "Send the region to the Maxima process,
do not display the maxima-buffer."
  (maxima-string-nodisplay
   (buffer-substring-no-properties beg end)))

(defun maxima-send-region (beg end &optional arg)
  "Send the current region to the Maxima process.
With an argument, check the parentheses first."
  (interactive "r\nP")
  (if arg
    (maxima-region beg end)
    (if (maxima-check-parens beg end)
        (maxima-region beg end))))


(defun maxima-send-buffer (&optional arg)
  "Send the buffer to the Maxima process, after checking the parentheses.
With an argument, don't check the parentheses."
  (interactive "P")
  (maxima-send-region (point-min) (point-max) arg))

(defun maxima-send-line (&optional arg)
  "Send the current line to the Maxima process, after checking parentheses.
With an argument, don't check parentheses."
  (interactive "P")
  (let ((b (maxima-line-beginning-position))
	(e (maxima-line-end-position)))
    (maxima-send-region b e arg)))

(defun maxima-send-line-nodisplay ()
  "Send the current line to the Maxima process."
  (interactive)
  (maxima-region-nodisplay (maxima-line-beginning-position)
                           (maxima-line-end-position)))

(defun maxima-send-form (&optional arg)
  "Send the current form to the Maxima process, checking parentheses.
With an argument, don't check parentheses."
  (interactive "P")
  (let (beg end)
    (save-excursion
      (maxima-beginning-of-form)
      (setq beg (point))
      (maxima-end-of-form)
      (setq end (point)))
    (maxima-send-region beg end arg)))

(defun maxima-send-full-line ()
  "Send the minimum number of lines such that the current is one of them,
and such that no line contains an incomplete form."
  (interactive)
  (let ((beg (point)) (end (point)))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (maxima-beginning-of-form)
      (while (< (point) beg) 
	(progn 
	  (beginning-of-line)
	  (setq beg (point))
	  (maxima-beginning-of-form)))
      (goto-char end)
      (end-of-line)
      (setq end (point))
      (while (and (< (maxima-form-beginning-position) end) (< end (point-max)))
	(progn
	  (forward-line 1)
	  (end-of-line)
	  (setq end (point))))
      (skip-chars-backward "[ \t;]")
      (if (looking-at "[;$]")
	  (maxima-send-region beg (1+ (point)))
	(error "; or $ at end"))
      end))) 

(defun maxima-send-full-line-and-goto-next-form ()
  "Do a maxima-send-full-line and go to the beginning of the next form."
  (interactive)
  (goto-char (maxima-send-full-line))
  (maxima-beginning-of-form))

(defun maxima-send-completed-region (beg end)
  "Send the marked region, but complete possibly non-complete forms at the bounderies."
  (interactive "r\nP")
  (let (beg1 end1)
    (save-excursion
      (goto-char beg)
      (maxima-beginning-of-form)
      (setq beg1 (point))
      (goto-char end)
      (maxima-end-of-form)
      (setq end1 (point))
      (maxima-send-region beg1 end1)
      end1)))

(defun maxima-send-completed-region-and-goto-next-form (beg end)
  "Do a maxima-send-completed-region and go to the beginning of the next form."
  (interactive "r\nP")
  (goto-char (maxima-send-completed-region beg end))
  (maxima-beginning-of-form))

(defun maxima-display-buffer ()
  "Display the inferior-maxima-process buffer so the recent output is visible."
  (interactive)
  (let ((origbuffer (current-buffer)))
    (if (not (processp inferior-maxima-process))
	(maxima-start))
    (pop-to-buffer (process-buffer inferior-maxima-process))
    (goto-char (point-max))
;    (recenter (universal-argument))
    (pop-to-buffer origbuffer)))

;;;; The inferior Maxima process

;;;; Smart complete (this was taken from W. Schelter's smart-complete.el)

;; Completion on forms in the buffer.   Does either a line or an sexp.
;; Uses the current prompt and the beginning of what you have typed.
;; Thus If the buffer contained

;; (dbm:3) load("jo"
;; (C11) lo("ji")
;; (gdb) last
;; maxima>>4
;; /home/bil# ls 
;; then if you are at a prompt 
;; "(C15) l"  would match lo("ji")  only, not "last", not "ls" nor load("
;; and the commands with the (gdb) prompt would only match ones
;; starting with (gdb) ..

(defun maxima-get-match-n (i )
  (buffer-substring (match-beginning i) (match-end i)))

(defun maxima-smart-complete ()
  "Complete the command.
Begin to type the command and then type M-p.  You will be
offered in the minibuffer a succession of choices, which
you can say 'n' to to get the next one, or 'y' or 'space'
to grab the current one.

 Thus to get the last command starting with 'li' you type
 liM-py
"
  (interactive )
  (let ((point (point)) new str tem prompt)
    (save-excursion
      (forward-line 0)
      (cond ((looking-at inferior-maxima-prompt)
	     (setq prompt (maxima-get-match-n 0))
	     (setq str (buffer-substring (match-end 0) point)))
	    (t (error "Your prompt on this line does not match prompt-pattern.")))
      (setq new (maxima-smart-complete2 prompt str)))
    (cond (new
	   (delete-region (setq tem (- point (length str))) point)
	   (goto-char tem)
	   (insert new)))))

(defun maxima-smart-complete2 (prompt str)
  (let ((pt (point)) found
	(pat (concat (maxima-regexp-for-this-prompt prompt)
		     "\\(" (regexp-quote str) "\\)" ))
	offered (not-yet t))
    (while (and  not-yet
		 (re-search-backward pat nil t))
      (goto-char (match-beginning 1))
      (setq at (match-beginning 1))
      (goto-char at)
      (setq this (buffer-substring at
				   (save-excursion (end-of-line) (point))))
      (or  (member this offered)
	   (equal this str)
	   (progn (setq offered (cons this offered))
		  ;; do this so the display does not shift...
		  (goto-char pt)
		  (setq not-yet
			(not (y-or-n-p (concat "Use: " this " "))))))
      (cond (not-yet (goto-char at) (beginning-of-line) (forward-char -1))
	    (t (setq found
		     (save-excursion
		       (buffer-substring
			at
			(progn (goto-char at)
			       (max (save-excursion
				      (end-of-line) (point))
				    (save-excursion
				      (forward-sexp 1)(point))))))))))
    (or found (message "No more matches"))
    found))


;; return a regexp for this prompt but with numbers replaced.

(defun maxima-split-string (s bag)
  (cond ((equal (length s) 0) '(""))
	((string-match bag s)
	 (if (= (match-beginning  0) 0)
	     (cons "" (maxima-split-string (substring s (match-end 0)) bag))
	   (cons (substring s 0 (match-beginning 0))
		 (maxima-split-string (substring s (match-end 0)) bag))))
	(t (cons s nil))))

;; Return a regexp which matches the current prompt, and which
;; allows things like
;; "/foo/bar# " to match  "any# "
;; "(C12) " to match  "(C1002) " but not (gdb) nor "(D12) "
;; if the prompt appears to be a pathname (ie has /) then
;; allow any beginning, otherwise numbers match numbers...
(defun maxima-regexp-for-this-prompt (prompt)
  (let ((wild (cond ((string-match "/" prompt) "[^ >#%()]+")
		    (t "[0-9]+"))))
    (let ((tem (maxima-split-string prompt wild)) (ans ""))
      (while tem
	(setq ans (concat ans (regexp-quote (car tem))))
	(cond ((cdr tem) (setq ans (concat ans wild))))
	(setq tem (cdr tem)))
      ans)))

;;;; Filter and sentinel
;;;; (Much of this was taken from dbl.el, by W. Schelter.

;; You may easily create additional commands and bindings to interact
;; with the display.  For example to put the dbl command next on \M-n
;; (maxima-define-debug :next "\M-n")

;; This causes the emacs command maxima-debug-:next to be defined, and runs
;; maxima-debug-display-frame after the command.

;; maxima-debug-display-frame is the basic display function.  
;; It tries to display
;; in the other window, the file and line corresponding to the current
;; position in the dbl window.  For example after a dbl-step, it would
;; display the line corresponding to the position for the last step.  Or
;; if you have done a backtrace in the dbl buffer, and move the cursor
;; into one of the frames, it would display the position corresponding to
;; that frame.

;; maxima-debug-display-frame is invoked automatically when a 
;; filename-and-line-number appears in the output.

(defun maxima-running ()
  (and (processp inferior-maxima-process)
       (eq (process-status inferior-maxima-process) 'run)))

(defun maxima-check-level (string)
  "Tell when the process is done computing (by looking for the prompt)."
  (setq inferior-maxima-result string)
  (if (string-match inferior-maxima-prompt string) ;"^(C[0-9]+).*" string)
      (progn
        (setq inferior-maxima-computing-p nil)
        (setq inferior-maxima-question-p nil)
        (cond ((string-match "MAXIMA>>" string)
               (setq inferior-maxima-lisp-level-flag t)
               (setq inferior-maxima-top-level-flag nil)
               (setq inferior-maxima-debug-level-flag nil)) 
              ((string-match "\(dbm.*" string)
               (setq inferior-maxima-lisp-level-flag nil)
               (setq inferior-maxima-top-level-flag nil)
               (setq inferior-maxima-debug-level-flag t)) 
              (t
               (setq inferior-maxima-lisp-level-flag nil)
               (setq inferior-maxima-top-level-flag t)
               (setq inferior-maxima-debug-level-flag nil))))
    (if (string-match "?" string)
        (setq inferior-maxima-question-p t))))

(defun inferior-maxima-filter (string)
  (maxima-check-level string)
  (if (not inferior-maxima-debug-level-flag)
      (progn
	(if (overlayp maxima-debug-overlay)
	    (delete-overlay maxima-debug-overlay))
	(setq overlay-arrow-position nil)))
  (let ((inhibit-quit t))
    (if maxima-debug-filter-accumulator
	(inferior-maxima-filter-accumulate-marker 
	 (concat maxima-debug-filter-accumulator string))
      (inferior-maxima-filter-scan-input string)))
  "")

(defun inferior-maxima-filter-accumulate-marker (string)
  (setq maxima-debug-filter-accumulator nil)
  (if (> (length string) 1)
      (if (= (aref string 1) ?\032)
	  (let ((end (string-match "\n" string)))
	    (if end
		(progn
		  (setq me string)
		  (cond ((string-match
         		 "\032\032\\([A-Za-z]?:?[^:]*\\):\\([0-9]*\\):[^\n]+\n"
			  string)
			 (setq maxima-debug-last-frame
			       (cons
				(match-string 1 string)
				(string-to-int  (match-string 2 string))))
			 (cond 
			  ((equal (cdr maxima-debug-last-frame)  0)
					;(message "got 0")
					;(sit-for 1)
			   (delete-overlay maxima-debug-overlay)
			   (setq overlay-arrow-position nil)
			   (setq maxima-debug-last-frame nil))
			  (t (setq maxima-debug-last-frame-displayed-flag nil)))))
		  (inferior-maxima-filter-scan-input 
		   (substring string (1+ end))))
	      (setq maxima-debug-filter-accumulator string)))
	(inferior-maxima-filter-insert "\032")
	(inferior-maxima-filter-scan-input (substring string 1)))
    (setq maxima-debug-filter-accumulator string)))

(defun inferior-maxima-filter-scan-input (string)
  (if (equal string "")
      (setq maxima-debug-filter-accumulator nil)
    (let ((start (string-match "\032" string)))
      (if start
	  (progn
	    ;; to do fix this so that if maxima-debug-last-frame
	    ;; changed, then set the current  text property..
	    ;;
	    (inferior-maxima-filter-insert (substring string 0 start))
	    (inferior-maxima-filter-accumulate-marker 
	     (substring string start)))
	(inferior-maxima-filter-insert string)))))

(defun inferior-maxima-filter-insert (string)
  (let (moving
	output-after-point 
	(old-buffer (current-buffer))
	start)
    (set-buffer (process-buffer inferior-maxima-process))
    ;; test to see if we will move the point.   We want that the
    ;; window-point of the buffer, should be equal to process-mark. 
    (setq moving (>= (window-point 
		      (get-buffer-window (process-buffer inferior-maxima-process)))
		     (- (process-mark inferior-maxima-process) 0)))
    (setq output-after-point (< (point) (process-mark inferior-maxima-process)))
    (unwind-protect
	(save-excursion
	  ;; Insert the text, moving the process-marker.
	  (goto-char (process-mark inferior-maxima-process))
	  (setq start (point))
	  (insert string)
	  (set-marker (process-mark inferior-maxima-process) (point))
	  (maxima-debug-maybe-delete-prompt)
	  ;; Check for a filename-and-line number.
	  (maxima-debug-display-frame
	   ;; Don't display the specified file
	   ;; unless (1) point is at or after the position where output appears
	   ;; and (2) this buffer is on the screen.
	   (or output-after-point
	       (not (get-buffer-window (current-buffer))))
	   ;; Display a file only when a new filename-and-line-number appears.
	   t))
      (if moving
	  (set-window-point
	   (get-buffer-window (process-buffer inferior-maxima-process))
	   (process-mark inferior-maxima-process)))
      (set-buffer old-buffer))))

(defun inferior-maxima-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	 (if (overlayp maxima-debug-overlay)
	     (delete-overlay maxima-debug-overlay))
	 (setq overlay-arrow-position nil)
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; Stop displaying an arrow in a source file.
	 (if (overlayp maxima-debug-overlay)
	     (delete-overlay maxima-debug-overlay))
	 (setq overlay-arrow-position nil)
	 ;; Fix the mode line.
	 (setq mode-line-process
	       (concat ": "
		       (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the dbl buffer.
	     (set-buffer obuf))))))

(defun maxima-debug-refresh ()
  "Fix a possibly garbled display, and redraw the arrow."
  (interactive)
  (redraw-display)
  (maxima-debug-display-frame))

(defun maxima-debug-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker from DBL.
The marker looks like \\032\\032FILENAME:LINE:CHARPOS\\n.
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (and maxima-debug-last-frame (not nodisplay)
       (or (not maxima-debug-last-frame-displayed-flag) (not noauto))
       (progn (maxima-debug-display-line (car maxima-debug-last-frame) 
					 (cdr maxima-debug-last-frame))
	      (setq maxima-debug-last-frame-displayed-flag t))))

	     
;;; Make sure the file named TRUE-FILE is in a buffer that appears on
;; the screen and that its line LINE is visible.  Put the
;; overlay-arrow on the line LINE in that buffer.

(defun maxima-debug-find-file (file)
  (cond ((file-exists-p file)
	 (find-file-noselect file))
	((get-buffer file))
	(t (find-file-noselect file))))

(defun maxima-debug-search-path (file dirs)
  (let ((paths (symbol-value dirs))
	true-file)
    (cond ((file-exists-p file) (setq true-file file))
	  (t
	   (while paths
	     (let ((tem (expand-file-name file 
					  (or (car paths) default-directory))))
	       (if  (file-exists-p tem) (setq true-file tem))
	       (setq paths (cdr paths))))))
    (cond (true-file)
	  (t (setq paths (symbol-value dirs))
	     (set dirs
		  (append paths
			  (list 
			   (file-name-directory
			    (read-file-name
			     (format "%s = %s, add path :" dirs paths))))))
	     (maxima-debug-search-path file dirs)))))

(defun maxima-debug-find-line ()
  "Look for a file-line property.
If none is found, search for a regexp.
If the buffer is a non process buffer, just return current file 
and line number."
  (interactive)
  (save-excursion
    (end-of-line)
    (cond ((get-buffer-process (current-buffer))
	   (cond
	    ((save-excursion
	       (beginning-of-line)
	       (get-text-property (point) 'file-line)))
	    ((progn (end-of-line) 
		    (re-search-backward " \\([^: ]+\\):\\([0-9]+\\)" 300 nil))
	     (setq file (buffer-substring (match-beginning 1) (match-end 1)))
	     (setq line (buffer-substring (match-beginning 2) (match-end 2)))
	     (setq line (read line))
	     (and (integerp line)
		  (setq file 
			(maxima-debug-search-path file 'maxima-debug-dirs))
		  (list file line)))))
	  (t (list (buffer-file-name) (+ 1 (count-lines (point))))))))

(defun maxima-debug-find-and-display-line ()
  (interactive)
  (let ((res (maxima-debug-find-line)))
    (and res (apply 'maxima-debug-display-line res))))

(defun maxima-debug-display-line (true-file line)
  (let* ((buffer (maxima-debug-find-file true-file))
	 (window (display-buffer buffer t))
	 (pos)
	 (end))
    (save-excursion
      (set-buffer buffer)
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq end (line-end-position))
	(if (overlayp maxima-debug-overlay)
	    (delete-overlay maxima-debug-overlay))
	(setq maxima-debug-overlay (make-overlay pos end))
	(overlay-put maxima-debug-overlay 'face 
		     maxima-debug-highlight-face)
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position)))


(defun maxima-debug-maybe-delete-prompt ()
  (if (and maxima-debug-delete-prompt-marker
	   (> (point-max) (marker-position maxima-debug-delete-prompt-marker)))
      (let (start)
	(goto-char maxima-debug-delete-prompt-marker)
	(setq start (point))
	(beginning-of-line)
	(delete-region (point) start)
	(setq maxima-debug-delete-prompt-marker nil))))

;;;; Help functions

(defun maxima-debug-level-help ()
  "Get help on the debug level"
  (interactive)
  (let ((maxima-debug-help-buffer (get-buffer-create "Help on lisp level")))
    (set-buffer maxima-debug-help-buffer)
    (erase-buffer)
    (insert "Help on the Maxima debug level\n\n")
    (insert
"Maxima's source level debugging capabilities can be accessed through
Emacs.  A user can set a breakpoint at a line in a file (which has 
been loaded using, say, batchload), and then step line by line
from there.  The call stack may be examined, together with the 
variables bound at that level.  The current position in the source 
file will be displayed in the other half of the window.
The following keys are available to assist.
C-x C-a C-s Step one source line with display. (\"Step\")
C-x C-a C-n Step one source line (skip functions). (\"Next\")
C-x C-a TAB Step one instruction with display. (\"Stepi\")
C-x C-a C-r Continue with display.  (\"Resume\")
C-x C-a C-f Finish executing current function. (\"Finish\")
C-x C-a < Go up stack frames. (\"Up\")
C-x C-a > Go down stack frames. (\"Down\")
M-x maxima-debug-refresh Redraw the source file buffer.

A breakpoint can be set to the current line in the source file
with the command C-x SPC. (The command can be entered either in the 
source file buffer or the Maxima process buffer.)

The following is an example session with a file /tmp/joe.mac.

     (C1) batchload(\"/tmp/joe.mac\");
     (D1) 				 /tmp/joe.mac
     (C2) :br joe
     Turning on debugging debugmode(true)
     Bkpt 0 for joe (in /tmp/joe.mac line 8)
     (C2) foo(2,3);
     Bkpt 0:(joe.mac 8)
     (dbm:1) :bt                       <-- :bt typed here gives a backtrace
     #0: joe(y=5)(joe.mac line 8)
     #1: foo(x=2,y=3)(joe.mac line 5)
     (joe.mac 9)                       <-- Here type C-xC-aC-n to advance line
     (joe.mac 10)                      <-- Here type C-xC-aC-n to advance line
                                       In the other buffer the source code
                                       appears with an arrow.
     (dbm:1) u;                        Investigate value of 'u
     28
     (dbm:1) u:33;                     Alter it to be 33
     (dbm:1) :r                        C-xC-aC-r Resumes the computation
     (D3) 				     1094

The actual file /tmp/joe.mac is the following:

     foo(x,y):=(
       x:x+2,
       y:y+2,
       x:joe(y),
       x+y);
     
     joe(y):=block([u:y^2],
       u:u+3,
       u:u^2,
        u);

If you are looking at the file joe.mac, you may set a break point at a
certain line of that file by typing C-x SPC.  This figures out which
function your cursor is in, and then it sees which line of that
function you are on.  If you are on say line 2 of joe, then it will
insert in the other window `:br joe 2' the command to break joe at its
second line.

The above keystrokes are shortcuts for some keyword commands.  A
complete list of keyword commands follows; the number of arguments
taken depends on the particular command.  Also you need not type the
whole command, just enough to be unique among the keyword commands.
Thus `:br' would suffice for `:break'.  The current commands are:

`:break'
     Set a breakpoint in the specified FUNCTION at the specified LINE
     offset from the beginning of the function.  If FUNCTION is given
     as a string, then it is presumed to be a FILE and LINE is the
     offset from the beginning of the file.

`:bt'
     Backtrace.

`:continue'
     Continue the computation.

`:delete'
     Delete all breakpoints, or if arguments are supplied delete the
     specified breakpoints

`:disable'
     Disable the specified breakpoints, or all if none are specified

`:enable'
     Enable the specified breakpoints, or all if none are specified

`:frame'
     With an argument print the selected stack frame.  Otherwise the
     current frame.

`:help'
     Print help on a break command or with no arguments on all break
     commands

`:info'
     Undocumented

`:lisp'
     Evaluate the lisp form following on the line

`:lisp-quiet'
     Evaluate its arg as a lisp form without printing a prompt.

`:next'     
     Like `:step', except that subroutine calls are stepped over

`:quit'
     Quit this level

`:resume'
     Continue the computation.

`:step'
     Step program until it reaches a new source line

`:top'
     Throw to top level
")
    (goto-char 1)
    (use-local-map (append (make-sparse-keymap) (current-local-map)))
    (define-key (current-local-map) (kbd "SPC") 'scroll-up)
    (define-key (current-local-map) "b" 'scroll-down)
    (defun maxima-kill-debug-help ()
      (interactive)
      (let ((buf (current-buffer)))
	(delete-window)
	(kill-buffer buf)))
    (define-key (current-local-map) "q" 'maxima-kill-debug-help)
    (pop-to-buffer maxima-debug-help-buffer)))


;;;; Miscellaneous

(defun maxima-debug-call (command numeric)
  "Invoke COMMAND displaying source in other window."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    ;; to do put in hook here to recognize whether at
    ;; maxima or lisp level.
    (setq command (maxima-debug-subtitute-% command numeric))
    (goto-char (point-max))
    (setq maxima-debug-delete-prompt-marker (point-marker))
    (maxima-string command)))

(defun maxima-debug-subtitute-% (command n)
  (let* (result
	 (in-dbl (get-buffer-process (current-buffer)))
	 file-line)
    (cond ((string-match "%[fl]" command)
	   (cond (in-dbl (setq file-line (maxima-debug-find-line)))
		 (t (setq file-line
			  (list (buffer-file-name)
				(+ 1 (count-lines
				      (point)))))))))
    (while (and command (string-match "\\([^%]*\\)%\\([adeflp]\\)" command))
      (let ((letter (string-to-char (substring command (match-beginning 2))))
	    subst)
	(cond ((eq letter ?p)
	       (setq subst (if n (int-to-string n) "")))
	      ((eq letter ?f)
	       (setq subst (or (car file-line) "unknown-file")))
	      ((eq letter ?l)
	       (setq subst (if (cadr file-line)
			       (int-to-string (cadr file-line))
			     "unknown-line")))
	      ((eq letter ?a)
	       (setq subst (maxima-debug-read-address))))
	(setq result (concat 
		      result
		      (substring command (match-beginning 1) (match-end 1))
			     subst)))
      (setq command (substring command (match-end 2))))
    (concat result command)))

(defun inferior-maxima-check-and-send-line ()
  "Check the lines for mis-matched parentheses, then send the line."
  (interactive)
  (let ((ok nil)
	(pt (point))
	pt1)
    (save-excursion
      (end-of-line)
      (skip-chars-backward " \t")
      (forward-char -1)
      (when (looking-at "[$;]")
        (setq pt (point))
        (setq ok t)))
    (if ok
	(progn
	  (save-excursion
	    (re-search-backward inferior-maxima-prompt)
	    (setq pt1 (point)))
	  (if (maxima-check-parens pt1 pt)
              (progn
                (setq inferior-maxima-computing-p t)
                (setq inferior-maxima-question-p nil)
                (comint-send-input))))
      (setq inferior-maxima-computing-p t)
      (setq inferior-maxima-question-p nil)
      (comint-send-input))))

(defun inferior-maxima-send-line ()
  "Send the line to the Maxima process."
  (interactive)
  (setq inferior-maxima-computing-p t)
  (setq inferior-maxima-question-p nil)
  (comint-send-input))

(defun inferior-maxima-bol ()
  "Go to the beginning of the line, but past the prompt."
  (interactive)
  (let ((eol (save-excursion (end-of-line) (point))))
    (forward-line 0)
    (if (and (looking-at inferior-maxima-prompt)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))


(defun maxima-debug-set-break ()
  "Set breakpoint at this source line."
  (interactive)
  (let ((file-name (file-name-nondirectory buffer-file-name))
	(line (save-restriction
		(widen)
		(count-lines 1 (point)))))
    (and maxima-debug-downcase-filenames
	 (setq file-name (downcase file-name)))
    (maxima-string (concat ":break \"" file-name "\" " line))))

	
(defun maxima-debug-read-address()
  "Return a string containing the core-address found in the buffer at point."
  (save-excursion
    (let ((pt (dot)) found begin)
      (setq found (if (search-backward "0x" (- pt 7) t)(dot)))
      (cond (found 
	     (forward-char 2)
	     (setq result
		   (buffer-substring found
				     (progn (re-search-forward "[^0-9a-f]")
					    (forward-char -1)
					    (dot)))))
	    (t (setq begin (progn 
			     (re-search-backward "[^0-9]") (forward-char 1)
				  (dot)))
	       (forward-char 1)
	       (re-search-forward "[^0-9]")
	       (forward-char -1)
	       (buffer-substring begin (dot)))))))


(defvar maxima-debug-commands nil
  "List of strings or functions used by maxima-send-maxima-debug-command.
It is for customization by you.")

(defun maxima-send-maxima-debug-command (arg)
  "This command reads the number where the cursor is positioned.  It
 then inserts this ADDR at the end of the buffer.  A numeric arg
 selects the ARG'th member COMMAND of the list dbl-print-command.  If
 COMMAND is a string, (format COMMAND ADDR) is inserted, otherwise
 (funcall COMMAND ADDR) is inserted.  eg. \"p (rtx)%s->fld[0].rtint\"
 is a possible string to be a member of maxima-debug-commands.  "
  (interactive "P")
  (let (comm addr)
    (if arg (setq comm (nth arg maxima-debug-commands)))
    (setq addr (maxima-debug-read-address))
    (if (eq (current-buffer) (process-buffer inferior-maxima-process))
	(set-mark (point)))
    (cond (comm
	   (setq comm
		 (if (stringp comm) (format comm addr) (funcall comm addr))))
	  (t (setq comm addr)))
    (switch-to-buffer (process-buffer inferior-maxima-process))
    (goto-char (dot-max))
    (insert-string comm)))


;;;; Keymap

(defvar inferior-maxima-mode-map nil)

(if inferior-maxima-mode-map
    nil
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map "\C-a" 'inferior-maxima-bol)
    (define-key map "\C-m"  'inferior-maxima-check-and-send-line)
    (define-key map [(control return)] 'inferior-maxima-send-line)
    (if maxima-use-dynamic-complete
        (define-key map [(meta tab)] 'maxima-dynamic-complete)
      (define-key map [(meta tab)] 'maxima-complete))
    (define-key map [(meta control tab)] 'maxima-smart-complete)
    (define-key map "\C-c\C-d" 'maxima-help)
    (define-key map "\C-c\C-m" 'maxima-info)
;    (define-key map "\C-c\C-d" 'maxima-debug-level-help)
    (define-key map "\177" 'backward-delete-char-untabify)
    (define-key map "\C-c\C-k" 'maxima-stop)
    (define-key map "\C-c\C-l" 'maxima-debug-find-and-display-line)
    (define-key map "\C-x " 'maxima-debug-set-break)
    (setq inferior-maxima-mode-map map)))

(defmacro maxima-define-debug (name key &optional doc)
  (let* ((fun (intern (format "maxima-debug-%s" (read name)))))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'maxima-debug-call name 'arg))
	  (list 'define-key 'inferior-maxima-mode-map key  
		(list 'quote fun)))))

(maxima-define-debug ":step %p"   "\C-x\C-a\C-s" 
  "Step one source line with display")
(maxima-define-debug ":stepi %p"  "\C-x\C-a\C-i" 
  "Step one instruction with display")
(maxima-define-debug ":next %p"   "\C-x\C-a\C-n" 
  "Step one source line (skip functions)")
(maxima-define-debug ":r"   "\C-x\C-a\C-r" 
  "Continue with display")
(maxima-define-debug ":finish" "\C-x\C-a\C-f" 
  "Finish executing current function")
(maxima-define-debug ":up %p"     "\C-x\C-a<"   
  "Go up N stack frames (numeric arg) with display")
(maxima-define-debug ":down %p"   "\C-x\C-a>"   
  "Go down N stack frames (numeric arg) with display")


;;;; Menu

(easy-menu-define inferior-maxima-mode-menu inferior-maxima-mode-map 
		  "Maxima mode menu"
  '("Maxima"
    ("Debugging"
     ["Step one line" maxima-debug-:step inferior-maxima-debug-level-flag]
     ["Step one instruction" maxima-debug-:stepi inferior-maxima-debug-level-flag]
     ["Step one source line" maxima-debug-:next inferior-maxima-debug-level-flag]
     ["Continue" maxima-debug-:r inferior-maxima-debug-level-flag]
     ["Finish" maxima-debug-:finish inferior-maxima-debug-level-flag]
     ["Up frames" maxima-debug-:up inferior-maxima-debug-level-flag]
     ["Down frames" maxima-debug-:down inferior-maxima-debug-level-flag])
    ("Help"
     ["Maxima info" maxima-info t]
     ["Help" maxima-help t]
     ["Debug level help" maxima-debug-level-help t])
    ("Quit"
     ["Kill process" maxima-stop t])))

;;;; Inferior Maxima mode

(defvar inferior-maxima-prompt
  "\\(^([C][0-9]*) \\)\\|\\(^MAXIMA>+\\)\\|\\(^(dbm:[0-9]*) \\)" 
					; \\(^[^#%)>]*[#%)>]+ *\\)"
  "*Regexp to recognize prompts from the inferior Maxima") ; or lisp")

(defun inferior-maxima-mode ()
  "Major mode for interacting with an inferior Maxima process.

Return will check the line for balanced parentheses, and send line as input.
Control return will send the line as input without checking for balanced
parentheses.

M-TAB will complete the Maxima symbol as much as possible, providing
     a completion buffer if there is more than one possible completion.
     If `maxima-use-dynamic-complete' is non-nil, simply cycle through 
     possible completions.

\\[maxima-smart-complete] will complete the input line, based on previous input lines.
\\[maxima-help] will get help on a Maxima topic.
\\[maxima-info] will bring up the Maxima info manual.
\\[maxima-stop] will kill the process and the buffer, after asking for
  confirmation.  To kill without confirmation, give \\[maxima-stop] an
  argument.

To scroll through previous commands,
\\[comint-previous-input] will bring the previous input to the current prompt,
\\[comint-next-input] will bring the next input to the prompt.
\\[comint-previous-matching-input] will bring the previous input matching
  a regular expression to the prompt,
\\[comint-next-matching-input] will bring the next input matching
  a regular expression to the prompt.

Debugging:
Maxima's source level debugging capabilities can be accessed through
Emacs.  A user can set a breakpoint at a line in a file (which has 
been loaded using, say, batchload), and then step line by line
from there.  The call stack may be examined, together with the 
variables bound at that level.  The current position in the source 
file will be displayed in the other half of the window.
The following keys are available to assist.
\\[maxima-debug-:step] Step one source line with display. (\"Step\")
\\[maxima-debug-:next] Step one source line (skip functions). (\"Next\")
\\[maxima-debug-:stepi] Step one instruction with display. (\"Stepi\")
\\[maxima-debug-:r] Continue with display.  (\"Resume\")
\\[maxima-debug-:finish] Finish executing current function. (\"Finish\")
\\[maxima-debug-:up] Go up stack frames. (\"Up\")
\\[maxima-debug-:down] Go down stack frames. (\"Down\")
\\[maxima-debug-refresh] Redraw the source file buffer.

A breakpoint can be set to the current line in the source file
with the command \\[maxima-debug-set-break].  (The command can be 
entered either in the source file buffer or the Maxima process
buffer.)

A short help page on debugging can be accessed through
\\[maxima-debug-level-help]


The following commands are available:
\\{inferior-maxima-mode-map}
"
  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp inferior-maxima-prompt)
  (setq major-mode 'inferior-maxima-mode)
  (setq mode-name "Inferior Maxima")
  (setq mode-line-process '(": %s"))
  (maxima-mode-variables)
  (use-local-map inferior-maxima-mode-map)
  ;; Debugging stuff
  (setq maxima-debug-last-frame nil)
  (setq maxima-debug-last-frame-displayed-flag t)
  (setq maxima-debug-delete-prompt-marker nil)
  (setq maxima-debug-filter-accumulator nil)
  (if running-xemacs
      (add-to-list 'comint-output-filter-functions 'maxima-check-level)
    (if inferior-maxima-use-debug
       (add-to-list 'comint-preoutput-filter-functions 'inferior-maxima-filter)
      (add-to-list 'comint-output-filter-functions 'maxima-check-level)))
  (unless running-xemacs
    (set-process-sentinel inferior-maxima-process 'inferior-maxima-sentinel))
  (if running-xemacs
      (add-local-hook 'kill-buffer-hook
                      (function
                       (lambda ()
                         (if (processp inferior-maxima-process)
                             (delete-process inferior-maxima-process))
                         (setq inferior-maxima-process nil)
                         (run-hooks 'inferior-maxima-exit-hook))))
    (add-hook 'kill-buffer-hook
              (function
               (lambda ()
                 (if (processp inferior-maxima-process)
                     (delete-process inferior-maxima-process))
                 (setq inferior-maxima-process nil)
                 (run-hooks 'inferior-maxima-exit-hook))) t t))
  (run-hooks 'inferior-maxima-mode-hook))

;;;; Running Maxima

(defun maxima ()
  "Run Maxima interactively inside a buffer."
  (interactive)
  (maxima-start)
  (switch-to-buffer (process-buffer inferior-maxima-process)))

(provide 'maxima)

;;; maxima.el ends here
