;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;
;;; This file is part of the Maxima computer algebra project
;;; (https://sourceforge.net/projects/maxima/) 
;;; SPDX-License-Identifier: GPL-2.0-or-later 
;;;
;;; Maxima is copyrighted by its authors and licensed under the GNU
;;; General Public License.  This program is distributed WITHOUT ANY
;;; WARRANTY. See COPYING and AUTHORS for details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PROMPT: prompt-formatting cluster.  Holds the input-prompt
;;; specials, the format-prompt machinery, the main-prompt /
;;; break-prompt printers, and the stripdollar utility.  Extracted
;;; into its own file so that suprv1 (which calls break-prompt from
;;; continuep) and macsys (which calls format-prompt and stripdollar
;;; from the REPL) can both depend on it, without a cycle.

(in-package :maxima)

(macsyma-module prompt)

;;; Standard Kinds of Input Prompts

(defmvar $prompt '_
  "Prompt symbol of the demo function, playback, and the Maxima break loop.")


;; A prefix and suffix that are wrapped around every prompt that Maxima
;; emits. This is designed for use with text-based interfaces that drive Maxima
;; through standard input and output and need to decorate prompts to make the
;; output easier to parse. There are some more notes in
;; doc/implementation/external-interface.txt.
(defvar *prompt-prefix* "")
(defvar *prompt-suffix* "")
(defvar *general-display-prefix* "")
(defvar *parallel-input-forbidden* nil
  "Bound by each parallel runner. Refuse interactive input before prompting
or reading; a closed input stream alone can silently return NIL.")
(defvar $alt_format_prompt nil "If NIL, use DEFAULT-FORMAT-PROMPT to print input prompt; if a function, use it to print input prompt.")

(defun format-prompt (destination control-string &rest arguments)
  "If $ALT_FORMAT_PROMPT is NIL, use DEFAULT-FORMAT-PROMPT to print
prompt; otherwise MFUNCALL $ALT_FORMAT_PROMPT to print prompt."
  (funcall (if $alt_format_prompt #'alt-format-prompt #'default-format-prompt)
	   destination control-string arguments))

(defun alt-format-prompt (destination control-string arguments)
  "MFUNCALL $ALT_FORMAT_PROMPT with a heavy coating of error protection."
  (handler-bind ((error (lambda(msg) (setq $alt_format_prompt nil)
			       (format t (intl:gettext "Error in printing prompt; reverting to default.~%~a") msg)
			       (throw 'macsyma-quit 'maxima-error))))
    (with-$error (let ((prompt (mfuncall $alt_format_prompt destination control-string arguments)))
		   (if (stringp prompt) prompt (merror "alt_format_prompt returned an object of type ~a, needed a string." (type-of prompt)))))))

(defun default-format-prompt (destination control-string arguments)
  "Like AFORMAT, but add the prefix and suffix configured for a prompt. This
function deals correctly with the ~M control character, but only when
DESTINATION is an actual stream (rather than nil for a string)."
  (let ((*print-circle* nil) (*print-base* 10.) *print-radix*)
    (let ((text
           (if (string= control-string "~{~A~}")
               ;; RETRIEVE uses this flat list of literal fragments. AFORMAT
               ;; has no iteration directive; retain its ~A formatting for
               ;; each fragment without changing the custom prompt protocol.
               (destructuring-bind (pieces &rest ignored) arguments
                 (declare (ignore ignored))
                 (with-output-to-string (output)
                   (dolist (piece pieces)
                     (aformat output "~A" piece))))
               (apply #'aformat nil control-string arguments))))
      (if (null destination)
          (concatenate 'string *prompt-prefix* text *prompt-suffix*)
          (format destination "~A~A~A" *prompt-prefix* text *prompt-suffix*)))))
    

(defvar $default_format_prompt (symbol-function 'default-format-prompt))

;;  "When time began" (or at least the start of version control history),
;;  the following comment was made at this point:
;;
;;     instead of using this STRIPDOLLAR hackery, the
;;     MREAD function should call MFORMAT to print the prompt,
;;     and take a format string and format arguments.
;;     Even easier and more general is for MREAD to take
;;     a FUNARG as the prompt. -gjc
;;
;;  I guess we're still failing miserably, but unfortunately MFORMAT/AFORMAT
;;  don't deal correctly with ~M plus a string output stream.
(defun main-prompt ()
  (if (and *display-labels-p* (not *suppress-input-echo*))
      (format-prompt nil "(~A~A) "
                     (print-invert-case (stripdollar $inchar))
                     $linenum)
      ""))

(defun break-prompt ()
  (format-prompt nil "~A"
                 (print-invert-case (stripdollar $prompt))))

(defun stripdollar (x)
  (cond ((not (atom x))
	 (cond ((and (eq (caar x) 'bigfloat) (not (minusp (cadr x)))) (implode (fpformat x)))
	       (t (merror (intl:gettext "STRIPDOLLAR: argument must be an atom; found: ~M") x))))
	((numberp x) x)
	((null x) 'false)
	((eq x t) 'true)
        ((member (get-first-char x) '(#\$ #\%) :test #'char=)
         (intern (subseq (string x) 1)))
	(t x)))
