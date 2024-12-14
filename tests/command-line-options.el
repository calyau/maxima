;;; command-line-options.el --- Regression tests for Maxima's command-line options  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Leo Butler

;; Author: Leo Butler <leo.butler@umanitoba.ca>
;; Maintainer: Leo Butler <leo.butler@umanitoba.ca>
;; Created: 15 Jul 2024
;; Keywords: command-line options; regression tests
;; Package-Requires: (ert maxima)
;; URL: https://maxima.sourceforge.io/

;; This file is part of Maxima.

;; This file is free software. It is released under the terms of the
;; GPLv2. For more information, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Exercise various command-line options with various
;; Lisps. Command-line options are defined in
;; `maxima-cli--tests'. `maxima-cli-deftest' defines a single set of
;; tests, one per Lisp; `maxima-cli-deftests' defines all tests.
;;
;; To execute the tests, do the following (in an *ielm* buffer, for
;; example):
;;   (maxima-cli-deftests maxima-cli--tests)  ;; define tests
;;   (ert t)
;;
;; Note that `ert' is an Emacs command, so M-x ert RET also works.
;;
;; Currently, a bug in GCL, which prevents newlines from being printed
;; in some cases, causes two test failures. See the definition of
;; `maxima-cli-tests'.

;;; Code:
(require 'ert)
(require 'maxima)

(defvar maxima-cli--filter-regexps
  '("^;;; Loading #P"         ;; ECL -- (setq *load-verbose* nil) creates its own problems
    "^$"                      ;; ECL -- blank lines
    )
  "Regexps to remove extraneous lines from Maxima's output.")

(defun maxima-cli--output-filter (line)
  "Filter empty or undesired lines from Maxima output.
Return nil if LINE is zero-length or it matches a regexp in
`org-babel-maxima--output-filter'; otherwise, return LINE."
  (unless (or (= 0 (length line))
              (cl-some #'(lambda(r) (string-match r line))
                       maxima-cli--filter-regexps))
    line))

(defun maxima-cli--buffer-string (buffer)
  (with-current-buffer buffer
    (string-join
     (delq nil
	   (mapcar #'maxima-cli--output-filter
		   (string-split
		    (buffer-substring-no-properties (point-min) (point-max))
		    "[\r\n]")))
     "\n")))

(defun maxima-cli-run-test (cmd)
  (let ((stdout (get-buffer-create " *maxima-cli-test-out*" t))
	(stderr (get-buffer-create " *maxima-cli-test-err*" t)))
    (with-current-buffer stdout (erase-buffer))
    (with-current-buffer stderr (erase-buffer))
    (shell-command cmd stdout stderr)
    (list (maxima-cli--buffer-string stdout) (maxima-cli--buffer-string stderr))))

(defun maxima-cli-deftest (plist &optional lisps)
  (let* ((name (car plist))
	 (plist (cadr plist))
	 (lisps (or (plist-get plist :lisps) lisps))
	 (results (or (plist-get plist :results) lisps)))
    (mapcar #'eval
	    (cl-map 'list #'(lambda(lisp result)
			       `(ert-deftest ,(intern (format "%s-%s" name lisp)) ()
				  ,@(if (null result) '(:expected-result :failed) nil)
				  (let* ((cmd ,(format (plist-get plist :cmd) maxima-command lisp))
					 (results (maxima-cli-run-test cmd)))
				    (should (equal (car results) ,(plist-get plist :stdout)))
				    (should (equal (cadr results) ,(plist-get plist :stderr))))))
			lisps results))))

(defun maxima-cli-deftests (tests)
  (let (results)
    (cl-do ((test tests (cddr test)))
	((null test) results)
      (setq results (cons (maxima-cli-deftest test) results)))))

(defvar maxima-cli--tests
  '(maxima-cli--vvq+batch-string
    (:cmd "%s --lisp=%s --very-very-quiet --no-init --batch-string='printf(true, \"HELLO~%%\");1/0;printf(true,\"Integral xdx = ~a~%%\",integrate(x,x));';"
	  :stdout "HELLO\nIntegral xdx = x^2/2" :stderr "expt: undefined: 0 to a negative exponent.\n -- an error. To debug this try: debugmode(true);"
	  :exit 0 :lisps (gcl sbcl ecl clisp) :results (nil t t t))
    maxima-cli--vvq+batch-string2
    (:cmd "%s --lisp=%s --quit-on-error --very-very-quiet --no-init --batch-string='printf(true, \"HELLO~%%\");1/0;printf(true,\"Integral xdx = ~a~%%\",integrate(x,x));';"
	  :stdout "HELLO" :stderr "expt: undefined: 0 to a negative exponent.\n -- an error. To debug this try: debugmode(true);"
	  :exit 1 :lisps (gcl sbcl ecl clisp) :results (nil t t t))
    maxima-cli--vq+batch-string-question
    (:cmd "%s --lisp=%s --very-quiet --no-init --batch-string='(display2d:false, integrate(x^n,x)); y;'"
	  :stdout "(display2d:false,integrate(x^n,x))\nlog(x)" :stderr ""
	  :exit 1 :lisps (gcl sbcl ecl clisp) :results (t t t t))
    maxima-cli--vvq+batch-syntax-error
    (:cmd "%s --lisp=%s --no-init --very-very-quiet --quit-on-error --batch-string='x:;'"
	  :stdout "" :stderr "incorrect syntax: Premature termination of input at ;.\nx:;\n ^"
	  :exit 1 :lisps (gcl sbcl ecl clisp) :results (t t t t))
    maxima-cli--vvq+batch-syntax-error2
    (:cmd "%s --lisp=%s --no-init --very-very-quiet --quit-on-error --batch-string='x'"
	  :stdout "" :stderr "incorrect syntax: end of file while scanning expression.\n ^"
	  :exit 1 :lisps (gcl sbcl ecl clisp) :results (nil t t t))
    )
  "List of tests of command-line switches.

A list of property lists of the form (:cmd MAXIMA-COMMAND :stdout
EXPECTED-OUTPUT :stderr EXPECTED-ERROR-OUPUT :exit
EXPECTED-EXIT-CODE :lisps LIST-OF-LISPS :results
LIST-OF-EXPECTED-RESULTS). To indicate an expected test failure,
place NIL in LIST-OF-EXPECTED-RESULTS; otherwise, place T.")

;;; command-line-options.el ends here
