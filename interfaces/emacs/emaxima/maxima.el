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

;; ** Running Maxima from the minibuffer **
;; The command M-x maxima-minibuffer
;; will allow you to interact with Maxima from the minibuffer.  
;; The arrows will allow you to scroll through previous inputs.
;; The line
;;  (autoload 'maxima-minibuffer "maxima" "Maxima in a minibuffer" t)
;; in your .emacs will make sure the function is available.
;; If the variable maxima-minibuffer-2d is non-nil, then the output
;; will be in Maxima's 2d output form, otherwise it will be in 
;; Maxima's 1d output form.  (For XEmacs, only the 1d form is available,
;; since the minibuffer isn't resizable.)
;; The command maxima-insert-last-minibuffer-output will insert
;; the last minibuffer output; if the output is in 2d, this will look
;; unpleasant.  With an argument, maxima-insert-last-minibuffer-output 
;; will insert the TeX form of the output.  (The variable 
;; maxima-minibuffer-get-tex need to be non-nil for this to work.)

;;; Code:

;;;; First
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;;;; The requires and provides

(require 'comint)
(require 'easymenu)
(require 'maxima-font-lock)

;;;; The variables that the user may wish to change

(defgroup maxima nil
  "Maxima mode"
  :prefix "maxima-"
  :tag    "Maxima")

(defcustom maxima-inchar "%i"
  "*The character used for an input prompt."
  :group 'maxima
  :type 'string)

(defcustom maxima-outchar "%o"
  "*The character used for an output prompt."
  :group 'maxima
  :type 'string)

(defcustom maxima-linechar "%t"
  "*The character used for an intermediate output prompt."
  :group 'maxima
  :type 'string)

(defcustom maxima-indent-amount 2
  "*The amount of each indentation level in `maxima-mode'.
This is used after `then', etc."
  :group 'maxima
  :type '(integer))

(defcustom maxima-paren-indent-amount 1
  "*The amount of indentation after a parenthesis."
  :group 'maxima
  :type '(integer))

(defcustom maxima-blockparen-indent-amount -3
  "*The amount of indentation after a `block('.
By default, it is negative, so the next line starts under the `block'."
  :group 'maxima
  :type '(integer))

(defcustom maxima-continuation-indent-amount 2
  "*The amount of extra indentation given when a line is continued."
  :group 'maxima
  :type '(integer))

(defcustom maxima-multiline-comment-indent-amount 2
  "*The amount of extra indentation inside a multiline comment."
  :group 'maxima
  :type '(integer))

(defcustom maxima-if-extra-indent-amount 0
  "*The amount of extra indentation to give a \"then\" following an \"if\"."
  :group 'maxima
  :type 'integer)

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

(defcustom maxima-return-style 'newline-and-indent
  "*Determines how `maxima-mode' will handle RET.
Choices are 'newline, 'newline-and-indent, and 'reindent-then-newline-and-indent"
  :group 'maxima
  :type '(choice :menu-tag "Return style"
                 :tag      "Return style"
                 (const newline) 
                 (const newline-and-indent)
                 (const reindent-then-newline-and-indent)))

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
  :type 'string)

(defcustom maxima-args nil
  "*Extra arguments to pass to the maxima-command."
  :group 'maxima
  :type 'string)

(defcustom maxima-use-tabs nil
  "*If non-nil, indentation will use tabs."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-minibuffer-2d nil
  "*If non-nil, use 2D output for maxima-minibuffer."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-use-full-color-in-process-buffer nil
  "*If non-nil, font-lock the maxima process buffer."
  :group 'maxima
  :type 'boolean)

(defcustom maxima-fix-double-prompt nil
  "*If non-nil, fix the double prompt that sometimes appears in XEmacs."
  :group 'maxima
  :type 'boolean)

;;;; The other variables

;; This variable seems to be necessary ...
(defvar inferior-maxima-after-output-wait 100)

(defvar maxima-temp-suffix 0
  "Temporary filename suffix.  Incremented by 1 for each filename.")

(defvar maxima-special-symbol-letters "!:='")

(defvar maxima-minibuffer-history nil)

(defvar maxima-block "")

(defvar inferior-maxima-process nil
  "The Maxima process.")

(defvar inferior-maxima-input-end 0
  "The end of the latest input that was sent to Maxima.")

(defvar inferior-maxima-output-end 0)

(defvar inferior-maxima-waiting-for-output nil)

(defvar inferior-maxima-exit-hook nil)

;;;; Utility functions

(defun maxima-replace-in-string (regexp rep string)
  "Replace all matches for REGEXP with REP in STRING.
Taken from `replace-regexp-in-string' from subr.el in GNU emacs."
  (let ((l (length string))
	(start 0)
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	(when (= me mb) (setq me (min l (1+ mb))))
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   nil nil str)
		    (cons (substring string start mb)       ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

(defun maxima-remove-kill-buffer-hooks ()
  "Remove the kill-buffer-hooks locally"
  (if (or running-xemacs (< emacs-major-version 21))
      (progn
        (make-local-hook 'kill-buffer-hook)
        (setq kill-buffer-hook nil))
    (let ((hooks kill-buffer-hook))
      (while hooks
        (remove-hook 'kill-buffer-hook (car hooks) t)
        (setq hooks (cdr hooks))))))

(defun maxima-make-temp-name ()
  "Return a unique filename."
  (setq maxima-temp-suffix (+ maxima-temp-suffix 1))
  (concat (concat (make-temp-name "#mz") "-")
          (int-to-string maxima-temp-suffix)
          ".max"))

(defun maxima-strip-string-beginning (string)
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (out))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-remove-kill-buffer-hooks)
;      (make-local-hook 'kill-buffer-hook)
;      (setq kill-buffer-hook nil)
      (insert string)
      (goto-char (point-min))
      (maxima-forward-over-comment-whitespace)
      (setq out (buffer-substring-no-properties (point) (point-max))))
    (kill-buffer tmpbuf)
    out))

(defun maxima-strip-string-end (string)
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (out))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-remove-kill-buffer-hooks)
;      (make-local-hook 'kill-buffer-hook)
;      (setq kill-buffer-hook nil)
      (insert string)
      (goto-char (point-max))
      (maxima-back-over-comment-whitespace)
      (setq out (buffer-substring-no-properties (point-min) (point))))
    (kill-buffer tmpbuf)
    out))

(defun maxima-strip-string (string)
  "Remove any spaces, tabs or newlines at the beginning and end of the string"
  (maxima-strip-string-beginning (maxima-strip-string-end string)))

;;;; Functions that query position

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
    (if (< (point) (+ (point-min) 5))
        nil
      (forward-char -5)			;"block"
      (if (looking-at "block")
          t
        nil))))

;;; Functions that search

(defun maxima-re-search-forward (regexp &optional pmax)
  "Search forward for REGEXP, bounded by PMAX.
Ignore matches found in comments."
  (let ((keep-looking t)
        (match nil)
        (pt (point)))
    (while (and keep-looking (re-search-forward regexp pmax t))
      (setq match (match-string 0))
      (if (maxima-in-comment-p)
          (maxima-forward-over-comment-whitespace)
        (setq keep-looking nil)))
    (if keep-looking
        (progn
          (goto-char pt)
          nil)
      match)))

(defun maxima-find-next-nonnested-close-char ()
  "Search forward for next , ; $ or closing ), skipping over balanced parens."
  (let ((keep-looking t)
        (level 0)
        (pt (point))
        (match))
    (while (and keep-looking 
                (setq match (maxima-re-search-forward "[,;$()]")))
      (cond ((string= match "(")
             (setq level (1+ level)))
            ((string= match ")")
             (setq level (1- level))
             (when (< level 0)
               (setq keep-looking nil)))
            (t
             (if (= level 0)
                 (setq keep-looking nil)))))
    (if (and match
             (or
              (and
               (string-match match ",;$")
               (= level 0))
              (and
               (string-match match ")")
               (= level -1))))
        (point)
      (goto-char pt)
      nil)))

(defun maxima-re-search-backward (regexp &optional pmin)
  "Search forward for REGEXP, bounded by PMIN.
Ignore matches found in comments."
  (let ((keep-looking t)
        (match nil)
        (pt (point)))
    (while (and keep-looking (re-search-backward regexp pmin t))
      (setq match (match-string 0))
      (if (maxima-in-comment-p)
          (maxima-back-over-comment-whitespace)
        (setq keep-looking nil)))
    (if keep-looking
        (progn
          (goto-char pt)
          nil)
      match)))

;;;; Functions that return special positions

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

(defun maxima-name-beginning ()
  (save-excursion
    (backward-word 1)
    (point)))

(defun maxima-special-symbol-beginning ()
  (save-excursion
    (skip-chars-backward maxima-special-symbol-letters)
    (point)))

(defun maxima-special-symbol-end ()
  (save-excursion
    (skip-chars-forward maxima-special-symbol-letters)
    (point)))

(defun maxima-form-beginning-position ()
  (save-excursion
    (maxima-goto-beginning-of-form)
    (point)))

(defun maxima-form-end-position ()
  (save-excursion
    (if (maxima-goto-end-of-form)
        (point)
      nil)))

(defun maxima-form-end-position-or-point-max ()
  (let ((mfep (maxima-form-end-position)))
    (if mfep
        mfep
      (point-max))))

(defun maxima-expression-end-position ()
  "Return the point where the current expression ends,
or nil."
  (save-excursion
    (if (maxima-goto-end-of-expression)
        (point)
      nil)))

(defun maxima-construct-beginning-position ()
  "Return the point which begins the current construct."
  (save-excursion
    (maxima-goto-beginning-of-construct)
    (point)))

(defun maxima-paren-begin-position ()
  "Find the point of the opening parenthesis for the point."
  (save-excursion
    (if (maxima-goto-beginning-of-sexp)
        (point)
      nil)))

(defun maxima-begin-if-position ()
  "Find the point of the opening \"if\" for the point."
  (let ((nest 0)
        (match)
        (keep-looking t)
        (pt (point)))
    (save-excursion
      (while (and keep-looking 
                  (setq match (maxima-re-search-backward "\\<if\\>\\|\\<then\\>" nil)))
        (setq match (downcase match))
        (cond ((string= match "if") (setq nest (1+ nest)))
              (t (setq nest (1- nest))))
        (when (= nest 1)
          (setq pt (point))
          (setq keep-looking nil))))
    (if keep-looking
        nil
      pt)))

(defun maxima-begin-then-position ()
  "Find the point of the opening \"then\" for the current \"else\"."
  (let ((keep-looking t)
        (pt (point))
        (begin-then nil))
    (save-excursion
      (while (and keep-looking 
                  (maxima-re-search-backward "\\<then\\>" nil))
        (let ((meep (maxima-expression-end-position)))
          (when (or (not meep) (<= pt (maxima-expression-end-position)))
            (setq begin-then (point))
            (setq keep-looking nil)))))
    begin-then))

;;;; Functions that move the position

(defun maxima-forward-over-comment-whitespace ()
  "Move forward over comments and whitespace."
  (let ((ok t))
    (while  ok
      (skip-chars-forward " \t\n")
      (cond ((looking-at "/\\*")
             (if (not (search-forward "*/" nil t ))
                 (setq ok nil)))
	    ((looking-at "\n")
	     (forward-char 1))
	    (t  (setq ok nil))))))

(defun maxima-back-over-comment-whitespace ()
  "Move backward over comments and whitespace."
  (let ((ok t))
    (while  ok
      (if (or (maxima-in-comment-p) 
              (maxima-in-multiline-comment-p))
          (search-backward "/*"))
      (skip-chars-backward " \t\n")
      (if (> (point) 2)
          (progn
            (forward-char -2)
            (unless (looking-at "\\*/")
              (setq ok nil)
              (forward-char 2)))
        (setq ok nil)))))

(defun maxima-goto-beginning-of-form ()
  "Move to the beginning of the form."
  (let ((pt (point)))
    (if (maxima-re-search-backward "[;$]" nil)
        (forward-char 1)
      (goto-char (point-min)))
    (maxima-forward-over-comment-whitespace)
    (if (< pt (point)) (goto-char pt))
    (point)))

(defun maxima-goto-beginning-of-form-interactive ()
  "Move to the beginning of the form."
  (interactive)
  (maxima-goto-beginning-of-form))

(defun maxima-goto-end-of-form ()
  "Move to the end of the form."
  (if (maxima-re-search-forward "[;$]" nil)
      (point)
    nil))

(defun maxima-goto-end-of-form-interactive ()
  "Move to the end of the form."
  (interactive)
  (unless (maxima-goto-end-of-form)
    (error "No end of form")))

(defun maxima-forward-sexp ()
  "Skip over the next complete sexp.
Ignores parens inside comments. Return nil if there is no following sexp."
  (let ((nest 0)
        (match)
        (keep-looking t)
        (pt (point)))
    (while (and keep-looking 
                (setq match (maxima-re-search-forward "[()]" nil)))
      (cond ((string= match "(") (setq nest (1+ nest)))
            (t (setq nest (1- nest))))
      (when (<= nest 0)
        (setq pt (point))
        (setq keep-looking nil)))
    (if keep-looking
        (progn
          (goto-char pt)
          nil)
      pt)))

(defun maxima-backward-sexp ()
  "Skip over the previous sexp.
Ignores parens inside comments. Return nil if there is no previous sexp."
  (let ((nest 0)
        (match)
        (keep-looking t)
        (pt (point)))
    (while (and keep-looking 
                (setq match (maxima-re-search-backward "[()]" nil)))
      (cond ((string= match ")") (setq nest (1+ nest)))
            ((string= match "(") (setq nest (1- nest))))
      (when (= nest 0)
          (setq pt (point))
          (setq keep-looking nil)))
    (if keep-looking
        (progn
          (goto-char pt)
          nil)
      pt)))

(defun maxima-goto-end-of-expression ()
  "Find the point that ends the expression that begins at point.
The expression is assumed to begin with \"if\", \"then\", \"do\"
\"else\" or \"(\".  Return nil if the end is not found."
;  (interactive)
  ;; To find the end of the expression:
  ;;  if looking at ( or block(, look for )
  ;;  otherwise look for a , ; or $ at the same nesting level of
  ;;  parentheses or a closing ).
  (cond ((or (looking-at "(") (looking-at "block("))
         (maxima-forward-sexp))
        (t
         (maxima-find-next-nonnested-close-char))))

(defun maxima-goto-beginning-of-construct ()
  "Go to the point the begins the current construct."
  (interactive)
 (let ((keep-looking t)
        (pmin (maxima-form-beginning-position))
        (pt (point)))
    (while (and keep-looking 
                (maxima-re-search-backward 
                 "\\<if\\>\\|\\<then\\>\\|\\<do\\>\\|\\<else\\>\\|(" pmin))
      (save-excursion
        (when (or (not (maxima-goto-end-of-expression)) (<= pt (point)))
          (setq keep-looking nil))))
    (if keep-looking 
        (goto-char pmin)
      (point))))

(defun maxima-goto-beginning-of-sexp ()
  "Move to the beginning of the current sexp.
Ignores parens inside comments.  Returns nil if not in sexp."
  (let ((nest 0)
        (match)
        (keep-looking t)
        (pt (point)))
    (while (and keep-looking 
                (setq match (maxima-re-search-backward "[()]" nil)))
      (cond ((string= match "(") (setq nest (1+ nest)))
            ((string= match ")") (setq nest (1- nest))))
      (when (= nest 1)
          (setq pt (point))
          (setq keep-looking nil)))
    (if keep-looking
        (progn
          (goto-char pt)
          nil)
      pt)))

(defun maxima-goto-beginning-of-sexp-interactive ()
  "Move to the beginning of the current sexp."
  (interactive)
  (unless (maxima-goto-beginning-of-sexp)
    (error "No beginning of sexp")))

(defun maxima-goto-end-of-sexp ()
  "Move to the end of the current sexp."
  (let ((keep-looking t)
	(found nil)
        (match)
	pt)
    (save-excursion
      (setq match (maxima-re-search-backward "(" nil))
      (when (and match (maxima-forward-sexp))
        (setq found t)
        (setq pt (point))))
    (if (not found)
        nil
      (goto-char pt)
      pt)))

(defun maxima-goto-end-of-sexp-interactive ()
  "Move to the end of the current sexp."
  (interactive)
  (unless (maxima-goto-end-of-sexp)
    (error "No end of sexp")))

;;; Newlines and indents
(defun maxima-indent-form ()
  "Indent the entire form."
  (interactive)
  (indent-region
   (maxima-form-beginning-position)
   (maxima-form-end-position-or-point-max)))

;;; 'standard
(defun maxima-standard-indent ()
  "Indent the line based on the previous line.
If the previous line opened a parenthesis, `maxima-indent-amount' is
added to the indentation, if the previous line closed a parenthesis, 
`maxima-indent-amount' is subtracted, otherwise the indentation 
is the same as the previous line."
  (interactive)
  (let ((indent 0)
        (match)
        (pt))
    (save-excursion
      (when (= (forward-line -1) 0)
        (progn
          (setq indent (current-indentation))
          (setq pt (maxima-line-end-position))
          (while (setq match (maxima-re-search-forward "[()]" pt))
            (cond ((string= match ")")
                   (setq indent (- indent maxima-indent-amount)))
                  ((string= match "(")
                   (setq indent (+ indent maxima-indent-amount))))))))
    (save-excursion
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-line-to (max indent 0)))
    (skip-chars-forward " \t")))

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
  (let ((indent 0)
        (close-char "[,;$]")
        (begin-construct)
        (pmin)
        (pt)
        (bc))
    (save-excursion
      (beginning-of-line)
      (cond
       ;; First, take care of the cases where the indentation is clear
       ;; No indentation at the beginning of the buffer
       ((= (point) (point-min)) 
        (setq indent 0))
       ;; A line beginning with a close comment is indented like the open comment
       ((looking-at "[ \t]*\\*/")
        (re-search-backward "/\\*")
        (setq indent (current-column)))
       ;; deal with comments separately
       ((maxima-in-comment-p)
        (setq indent (maxima-perhaps-smart-comment-calculate-indent)))
       ;; A line beginning "then" is indented like the opening "if"
       ((looking-at "[ \t]*\\<then\\>")
        (goto-char (maxima-begin-if-position))
        (setq indent (+ maxima-if-extra-indent-amount (current-column))))
       ;; A line beginning "else" is indented like the corresponding "then"
       ((looking-at "[ \t]*\\<else\\>")
        (goto-char (maxima-begin-then-position))
        (setq indent (current-column)))
       ;; A line beginning with a closing paren is indented like the open paren
       ((looking-at "[ \t]*)")
        (goto-char (maxima-paren-begin-position))
        (setq indent (current-column)))
       ;; Otherwise, the correct indentation needs to be computed.
       (t 
        ;; Find where to begin computing the indentation.
        ;; That should be the end of the previous statement (; or $)
        ;; or the beginning of the current sexp
        (setq pmin (maxima-form-beginning-position))
        ;; Find out if the point is within a paren group
        (setq pt (maxima-paren-begin-position))
        (if (and pt (< pt pmin))   ;; This shouldn't happen
              (setq pt nil)
            (if pt (setq pmin pt)))
;        (setq current-point (point))
;        (if pt 
;            (setq close-char "[,]") 
;          (setq close-char "[;\\$]"))
;        (setq close-char "[,;$]")
        ;; Now, find the indentation of beginning of the current construct
        (setq begin-construct (maxima-construct-beginning-position))
        ;; If begin-construct is nil, indent according to the opening paren
        (save-excursion
          (cond
           ;; The construct begins with a paren
           ((and pt (= begin-construct pt))
            (goto-char pt)
            (if (maxima-paren-opens-block-p)
                (setq indent (+ maxima-blockparen-indent-amount (current-column)))
              (setq indent (+ maxima-paren-indent-amount (current-column)))))
           ;; The construct does not begin with a paren
           (t
            (goto-char begin-construct)
            (setq indent (current-column)))))
        ;; Now, we need to possibly do some adjustments.
        ;; If the previous column doesn't end with close-char or a 
        ;; parenthesis, assume that the current line in a continuation 
        ;; of that line, and add to the indentation, unless the 
        ;; previous line was the beginning of the construct containing 
        ;; the point or only an open parenthesis.
        (maxima-back-over-comment-whitespace)
        (when (not (looking-at "^"))
          (forward-char -1)
          (if (not (or (looking-at "(") (looking-at close-char)))
              (setq indent (+ maxima-continuation-indent-amount indent)))))))
    indent))

(defun maxima-perhaps-smart-comment-calculate-indent ()
  (let ((indent)
        (ml 0))
    (if (maxima-in-multiline-comment-p) 
        (setq ml maxima-multiline-comment-indent-amount))
    (save-excursion
      (maxima-back-over-comment-whitespace)
      (if (looking-at "$") (forward-char 1))
      (setq indent (+ ml (maxima-perhaps-smart-calculate-indent))))
    indent))

(defun maxima-perhaps-smart-indent-line ()
  "Reindent the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-horizontal-space)
    (indent-to (maxima-perhaps-smart-calculate-indent)))
  (skip-chars-forward " \t"))

;;;; Indentation according to style

(defun maxima-indent-line ()
  (interactive)
  (cond
   ((eq maxima-newline-style 'basic)
    (maxima-standard-indent))
   ((eq maxima-indent-style 'standard)
    (maxima-standard-indent))
   ((eq maxima-indent-style 'perhaps-smart)
    (maxima-perhaps-smart-indent-line))))

(defun maxima-change-indent-style (new-style)
  "Change the newline style."
  (interactive "sNewline style (insert \"b\" for basic, \"s\" for standard, or \"p\" for perhaps-smart): ")
  (cond
   ((string= new-style "b")
    (setq maxima-indent-style 'basic))
   ((string= new-style "s")
    (setq maxima-indent-style 'standard))
   ((string= new-style "p")
    (setq maxima-indent-style 'perhaps-smart))))

(defun maxima-return ()
  (interactive)
  (cond
   ((eq maxima-return-style 'newline)
    (newline))
   ((eq maxima-return-style 'newline-and-indent)
    (newline-and-indent))
   ((eq maxima-return-style 'reindent-then-newline-and-indent)
    (reindent-then-newline-and-indent))))

;;;; Commenting

(defun maxima-insert-short-comment ()
  "Prompt for a comment."
  (interactive)
  (let ((comment (read-string "Comment: ")))
    (insert "/* " comment " */")
    (newline-and-indent)))

(defun maxima-insert-long-comment ()
  "Insert a comment enviroment"
  (interactive)
  (indent-for-tab-command)
  (insert "/*")
  (newline)
  (newline)
  (insert "*/")
  (indent-for-tab-command)
  (forward-line -1)
  (indent-for-tab-command))

(defun maxima-uncomment-region (beg end)
  "`uncomment-region' to use with the menu."
  (interactive "r")
  (comment-region beg end (universal-argument)))

;;;; Help functions

(defun maxima-get-help ()
  "Get help on a given subject"
  (let ((pt)
        (place))
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
      (Info-follow-nearest-node))
    (re-search-forward (concat "-.*: *" name "\\>"))
    (beginning-of-line)))

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
      (maxima-remove-kill-buffer-hooks)
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
    (maxima-remove-kill-buffer-hooks)
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
              (maxima-remove-kill-buffer-hooks)
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
      (maxima-help (current-word))))
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
  (let* ((stub  (current-word))
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
        (maxima-remove-kill-buffer-hooks)
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
  "Make the current form as the region."
  (interactive)
  (maxima-goto-beginning-of-form)
  (set-mark (maxima-form-end-position-or-point-max)))

(defun maxima-check-parens (beg end)
  "Check to make sure that the parentheses are balanced in the region."
  (interactive "r")
  (let ((keep-going t)
	(level 0)
	(pt)
        (match))
    (save-excursion
      (goto-char beg)
      (while (and keep-going (setq match (maxima-re-search-forward "[()]" end)))
        (cond
         ((string= match "(")
          (setq level (1+ level)))
          (t
           (setq level (1- level))))
        (when (< level 0)
            (setq keep-going nil)
            (setq pt (1- (point))))))
    (cond
     ((= level 0)			; All's fine
      (message "Parentheses match")
      t)
     ((< level 0)                       ; Too many close parens
      (message "Unmatched close parenthesis")
      (goto-char pt)
      nil)
     (t                                 ; Too many open parens
      ;; Go to the end, and repeat the above
      ;; close-parens of the open-parens have matches.  Go to the next 
      ;; open paren.
      (goto-char end)
      (setq keep-going t)
      (setq level 0)
      (while (and keep-going (setq match (maxima-re-search-backward "[()]" beg)))
        (cond
         ((string= match "(")
          (setq level (1+ level)))
         (t
          (setq level (1- level))))
        (when (> level 0)
          (setq keep-going nil)))
      (message "Unmatched open parenthesis")
      nil))))

(defun maxima-check-form-parens ()
  "Check to see if the parentheses in the current form are balanced."
  (interactive)
  (maxima-check-parens (maxima-form-beginning-position)
                       (maxima-form-end-position-or-point-max)))

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
    (define-key map "\M-\C-a" 'maxima-goto-beginning-of-form-interactive)
    (define-key map "\M-\C-e" 'maxima-goto-end-of-form-interactive)
    (define-key map "\M-\C-b" 'maxima-goto-beginning-of-sexp-interactive)
    (define-key map "\M-\C-f" 'maxima-goto-end-of-sexp-interactive)
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
    (define-key map "\C-c\C-q" 'maxima-clear-queue)
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
    (define-key map "\C-m" 'maxima-return)
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
    (define-key map [(control c) (control \))] 'maxima-check-form-parens)
;    (define-key map "\C-cC-\)" 'maxima-check-form-parens)
    (define-key map "\177" 'backward-delete-char-untabify)
    (setq maxima-mode-map map)))

;;;; Menu

(easy-menu-define maxima-mode-menu maxima-mode-map "Maxima mode menu"
  '("Maxima"
    ("Motion"
     ["Beginning of form" maxima-goto-beginning-of-form-interactive t]
     ["End of form" maxima-goto-end-of-form-interactive t]
     ["Beginning of sexp" maxima-goto-beginning-of-sexp-interactive t]
     ["End of sexp" maxima-goto-end-of-sexp-interactive t])
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
  (setq indent-line-function 'maxima-indent-line)
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
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'maxima-comment-indent))


;;;; Maxima mode

(defun maxima-mode ()
  "Major mode for editing Maxima code.

Maxima mode provides the following motion commands:
\\[maxima-goto-beginning-of-form-interactive]: Move to the beginning of the form.
\\[maxima-goto-end-of-form-interactive]: Move to the end of the form.
\\[maxima-goto-beginning-of-sexp-interactive]: Move to the beginning of the sexp.
\\[maxima-goto-end-of-sexp-interactive]: Move to the end of the sexp.

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

;;; Checking on the process
(defun inferior-maxima-running ()
  (and (processp inferior-maxima-process)
       (eq (process-status inferior-maxima-process) 'run)))

;;; Sending the information
(defun inferior-maxima-comint-send-input ()
  "Take note of position, then send the input"
  (setq inferior-maxima-input-end (point))
  (setq inferior-maxima-waiting-for-output t)
  (comint-send-input))

;;; This next function is a modified version of comint-strip-ctrl-m
(defun inferior-maxima-remove-double-prompt (&optional string)
  "Fix the double prompt that occasionally appears in XEmacs."
  (let ((pmark (process-mark inferior-maxima-process))
	(pos))
    (set-buffer (process-buffer inferior-maxima-process))
    (setq pos comint-last-output-start)
    (if (marker-position pos)
	(save-excursion
	  (goto-char pos)
          (beginning-of-line)
	  (while (re-search-forward 
                  (concat "(" maxima-inchar "[0-9]+).*\r") pmark t)
	    (replace-match "" t t))))))

(defun inferior-maxima-wait-for-output ()
  "Wait for output from the Maxima process."
  (while (and 
          inferior-maxima-waiting-for-output
          (inferior-maxima-running))
    (accept-process-output inferior-maxima-process))
  (if running-xemacs
      (sleep-for 0.1)
    (sit-for 0 inferior-maxima-after-output-wait)))

(defun inferior-maxima-output-filter (str)
  "Look for a new input prompt"
  (cond ((string-match "?" str)
         (maxima-ask-question str))
        ((string-match inferior-maxima-prompt str)
         (if (and inferior-maxima-process (not (string= maxima-block "")))
             (maxima-single-string (maxima-get-command))
           (maxima-clear-queue)
           (setq inferior-maxima-waiting-for-output nil)))))

(defun maxima-start ()
  "Start the Maxima process."
  (interactive)
  (if (processp inferior-maxima-process)
      (unless (eq (process-status inferior-maxima-process) 'run)
        (delete-process inferior-maxima-process)
        (save-excursion
          (set-buffer "*maxima*")
          (erase-buffer))
        (setq inferior-maxima-process nil)))
  (unless (processp inferior-maxima-process)
    (setq inferior-maxima-input-end 0)
    (setq inferior-maxima-waiting-for-output t)
    (let ((mbuf)
          (cmd))
      (if maxima-args
          (setq cmd 
                (append (list 'make-comint "maxima" maxima-command
                              nil) (split-string maxima-args))) 
        (setq cmd (list 'make-comint "maxima" maxima-command)))
      (setq mbuf (eval cmd))
      (save-excursion
        (set-buffer mbuf)
        (setq inferior-maxima-process (get-buffer-process mbuf))
        (add-to-list 'comint-output-filter-functions
                     'inferior-maxima-output-filter)
	(if maxima-fix-double-prompt
            (add-to-list 'comint-output-filter-functions
                         'inferior-maxima-remove-double-prompt))
        (inferior-maxima-wait-for-output)
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

;;; Sending information to the process

(defun maxima-single-string (string)
  "Send a string to the Maxima process."
  (setq string (maxima-strip-string string))
  (maxima-start)
;  (inferior-maxima-wait-for-output)
  (save-current-buffer
    (set-buffer (process-buffer inferior-maxima-process))
    (goto-char (point-max))
    (insert string)
    (inferior-maxima-comint-send-input)
    (goto-char (point-max))))

(defun maxima-ask-question (string)
  "Ask the question maxima wants answered."
  (let ((ans (read-string 
              (concat (maxima-strip-string string) " " ))))
    (unless (string-match "[;$]" ans)
      (setq ans (concat ans ";")))
    (setq ans (maxima-strip-string ans))
    (save-current-buffer
      (set-buffer (process-buffer inferior-maxima-process))
      (goto-char (point-max))
      (insert ans)
      (inferior-maxima-comint-send-input)
      (goto-char (point-max)))))

(defun maxima-get-command ()
  "Return the maxima command that's at the front of maxima-block.
Remove it from the front of maxima-block."
  (let* ((tmpfile (maxima-make-temp-name))
         (tmpbuf (get-buffer-create tmpfile))
         (pt)
         (command))
    (save-excursion
      (set-buffer tmpbuf)
      (maxima-remove-kill-buffer-hooks)
      (insert maxima-block)
      (beginning-of-buffer)
      (maxima-forward-over-comment-whitespace)
      (setq pt (point))
      (if (string-match "[$;]\\|:lisp"
                          (buffer-substring-no-properties (point) (point-max)))
          (progn
            (if (looking-at ":lisp")
                (progn
                  (search-forward ":lisp")
                  (forward-sexp)
                  (setq command (buffer-substring-no-properties pt (point))))
              (maxima-goto-end-of-form)
              (setq command (buffer-substring-no-properties pt (point))))
            (maxima-forward-over-comment-whitespace)
            (setq maxima-block (buffer-substring-no-properties (point) (point-max))))
        (setq command (buffer-substring-no-properties pt (point-max)))
        (setq maxima-block ""))
      (kill-buffer tmpbuf))
    command))

(defun maxima-send-block (stuff)
  "Send a block of code to Maxima."
  (maxima-start)
  (setq stuff (maxima-strip-string stuff))
  (if (string= maxima-block "")
      (progn
        (setq maxima-block stuff)
        (maxima-single-string (maxima-get-command)))
    (setq maxima-block (concat maxima-block stuff))))

(defun maxima-clear-queue ()
  "Clear out the queue of commands to send to the maxima process."
  (interactive)
  (setq maxima-block ""))

;;; Getting information back from Maxima.

(defun maxima-last-output ()
  "Get the most recent output from Maxima."
  (interactive)
  (inferior-maxima-wait-for-output)
  (save-excursion
    (set-buffer (process-buffer inferior-maxima-process))
    (let* ((pt (point))
           (pmark (progn (goto-char (process-mark inferior-maxima-process))
                         (forward-line 0)
                         (point-marker)))
           (output (buffer-substring-no-properties inferior-maxima-input-end pmark)))
      (goto-char pt)
      output)))

(defun maxima-last-output-noprompt ()
  "Return the last Maxima output, without the prompts"
  (interactive)
  (if (not (inferior-maxima-running))
      (maxima-last-output)
    (let* ((output (maxima-last-output))
           (newstring)
           (i 0)
           (beg)
           (end)
           (k))
    ;; Replace the output prompt with spaces
      (setq beg (string-match 
                 (concat "\\(^(" maxima-outchar "[0-9]*) \\)") output))
      (if (not beg)
          output
        (setq end (1+ (string-match ")" output beg)))
        (setq newstring (substring output 0 beg))
        (setq k (- end beg))
        (while (< i k)
          (setq newstring (concat newstring " "))
          (setq i (1+ i)))
        (concat newstring 
                (substring output 
                           end))))))

(defun maxima-last-output-tex-noprompt ()
  "Return the last Maxima output, between the dollar signs."
  (interactive)
  (let* ((output (maxima-last-output))
         (begtex (string-match "\\$\\$" output))
         (endtex (string-match "\\$\\$" output (1+ begtex))))
    (concat
     (substring output begtex (+ endtex 2))
     "\n")))


;;; Sending information to the process should be done through these
;; next five commands

(defun maxima-single-string-wait (string)
  "Send a single string to the maxima process,
waiting for output after."
  (inferior-maxima-wait-for-output)
  (maxima-single-string string)
  (inferior-maxima-wait-for-output))

(defun maxima-string (string)
  "Send a string to the Maxima process."
  (maxima-send-block string)
  (maxima-display-buffer))

(defun maxima-string-nodisplay (string)
  "Send a string to the Maxima process."
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

;;; Some functions to send commands to the process.

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
  (maxima-send-region (maxima-form-beginning-position)
                      (maxima-form-end-position-or-point-max) arg))

(defun maxima-send-full-line ()
  "Send the minimum number of lines such that the current is one of them,
and such that no line contains an incomplete form."
  (interactive)
  (let ((beg (point)) (end (point)))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (maxima-goto-beginning-of-form)
      (while (< (point) beg) 
	(progn 
	  (beginning-of-line)
	  (setq beg (point))
	  (maxima-goto-beginning-of-form)))
      (goto-char end)
      (end-of-line)
      (setq end (point))
      (while (and (< (maxima-form-beginning-position) end) (< end (point-max)))
	(progn
	  (forward-line 1)
	  (end-of-line)
	  (setq end (point))))
      (skip-chars-backward " \t;$")
      (if (re-search-forward "[;$]" end t)
	  (maxima-send-region beg (point))
	(error "No ; or $ at end"))
      end)))

(defun maxima-send-full-line-and-goto-next-form ()
  "Do a maxima-send-full-line and go to the beginning of the next form."
  (interactive)
  (goto-char (maxima-send-full-line))
  (maxima-goto-beginning-of-form))

(defun maxima-send-completed-region (beg end)
  "Send the marked region, but complete possibly non-complete forms at the bounderies."
  (interactive "r\nP")
  (let ((beg1)
        (end1))
    (save-excursion
      (goto-char beg)
      (setq beg1 (maxima-form-beginning-position))
      (goto-char end)
      (setq end1 (maxima-form-end-position-or-point-max))
      (maxima-send-region beg1 end1)
      end1)))

(defun maxima-send-completed-region-and-goto-next-form (beg end)
  "Do a maxima-send-completed-region and go to the beginning of the next form."
  (interactive "r\nP")
  (goto-char (maxima-send-completed-region beg end))
  (maxima-goto-beginning-of-form))

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
  (let ((at)
        (this)
        (pt (point)) found
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

;;; Sending a line to the process while in the process buffer

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
              (inferior-maxima-comint-send-input)))
      (inferior-maxima-comint-send-input))))

(defun inferior-maxima-send-line ()
  "Send the line to the Maxima process."
  (interactive)
  (inferior-maxima-comint-send-input))

(defun inferior-maxima-bol ()
  "Go to the beginning of the line, but past the prompt."
  (interactive)
  (let ((eol (save-excursion (end-of-line) (point))))
    (forward-line 0)
    (if (and (looking-at inferior-maxima-prompt)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))


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
    (define-key map "\177" 'backward-delete-char-untabify)
    (define-key map "\C-c\C-k" 'maxima-stop)
    (setq inferior-maxima-mode-map map)))

;;;; Menu

(easy-menu-define inferior-maxima-mode-menu inferior-maxima-mode-map 
		  "Maxima mode menu"
  '("Maxima"
    ("Help"
     ["Maxima info" maxima-info t]
     ["Help" maxima-help t])
    ("Quit"
     ["Kill process" maxima-stop t])))

;;;; Inferior Maxima mode

(defvar inferior-maxima-prompt
  (concat "\\(^(" maxima-inchar 
          "[0-9]*) \\)\\|\\(^MAXIMA>+\\)\\|\\(^(dbm:[0-9]*) \\)")
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

The following commands are available:
\\{inferior-maxima-mode-map}
"
  (interactive)
  (comint-mode)
  (if maxima-use-full-color-in-process-buffer
      (inferior-maxima-font-setup))
  (setq comint-prompt-regexp inferior-maxima-prompt)
  (setq major-mode 'inferior-maxima-mode)
  (setq mode-name "Inferior Maxima")
  (setq mode-line-process '(": %s"))
  (maxima-mode-variables)
  (use-local-map inferior-maxima-mode-map)
  (setq tab-width 8)
  (if (and (not running-xemacs) (< emacs-major-version 21))
      (make-local-hook 'kill-buffer-hook))
  (if running-xemacs
      (add-local-hook 'kill-buffer-hook
                      (function
                       (lambda ()
                         (maxima-clear-queue)
                         (if (processp inferior-maxima-process)
                             (delete-process inferior-maxima-process))
                         (setq inferior-maxima-process nil)
                         (run-hooks 'inferior-maxima-exit-hook))))
    (add-hook 'kill-buffer-hook
              (function
               (lambda ()
                 (maxima-clear-queue)
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

;;; Interacting with Maxima outside of a maxima buffer

(defun maxima-minibuffer ()
  "Communicate with Maxima through the minibuffer"
  (interactive)
  (maxima-start)
  (let ((input (read-string "Maxima: " nil maxima-minibuffer-history))
        (output nil)
        (twod (and maxima-minibuffer-2d (not running-xemacs))))
    (setq input (maxima-strip-string input))
    (unless (string= (substring input -1) ";")
      (setq input (concat input ";")))
    (if twod
        (maxima-single-string-wait 
         "block(emacsdisplay:display2d,display2d:true,linenum:linenum-1,%);")
      (maxima-single-string-wait
       "block(emacsdisplay:display2d,display2d:false,linenum:linenum-1,%);"))
    (maxima-single-string-wait input)
    (setq output (maxima-last-output-noprompt))
    (maxima-single-string-wait "block(display2d:emacsdisplay,linenum:linenum-1,%);")
    (if (not twod)
        (setq output (maxima-strip-string output))
      ;; Strip the beginning and trailing newline
      (while (string-match "\\` *\n" output)
        (setq output (substring output (match-end 0))))
      (while (string-match "\n *\\'" output)
        (setq output (substring output 0 (match-beginning 0)))))
    (setq output (maxima-replace-in-string "%" "%%" output))
    (message output)))

(defun maxima-insert-last-output ()
  (interactive)
  (maxima-single-string-wait
            "block(emacsdisplay:display2d,display2d:false,linenum:linenum-1,%);")
  (let ((output (maxima-last-output-noprompt)))
    (maxima-single-string "block(display2d:emacsdisplay,linenum:linenum-1,%);")
    (insert (maxima-strip-string output))))

(defun maxima-insert-last-output-tex ()
  (interactive)
  (maxima-single-string-wait "tex(%);")
  (let ((output (substring (maxima-last-output-tex-noprompt) 2 -3)))
    (maxima-single-string "block(linenum:linenum-2,%th(2));")
    (insert output)))

(provide 'maxima)
;;; maxima.el ends here
