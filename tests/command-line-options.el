(require 'ert)

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

(defun maxima-cli-deftest (plist lisps &optional results)
  (cond ((null plist)
	 results)
	(t
	 (maxima-cli-deftest (cddr plist) lisps (append (maxima-cli-deftest* plist lisps) results)))))
(defun maxima-cli-deftest* (plist &optional lisps)
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

(defvar maxima-cli--tests
  '(maxima-cli--vvq+batch-string
    (:cmd "%s --lisp=%s --very-very-quiet --no-init --batch-string='printf(true, \"HELLO~%%\");1/0;printf(true,\"Integral xdx = ~a~%%\",integrate(x,x));';"
	  :stdout "HELLO\nIntegral xdx = x^2/2" :stderr "expt: undefined: 0 to a negative exponent.\n -- an error. To debug this try: debugmode(true);"
	  :exit 0 :lisps (gcl sbcl ecl clisp) :results (nil t t t))
    maxima-cli--vvq+batch-string2
    (:cmd "%s --lisp=%s --quit-on-error --very-very-quiet --no-init --batch-string='printf(true, \"HELLO~%%\");1/0;printf(true,\"Integral xdx = ~a~%%\",integrate(x,x));';"
	  :stdout "HELLO" :stderr "expt: undefined: 0 to a negative exponent.\n -- an error. To debug this try: debugmode(true);"
	  :exit 1 :lisps (gcl sbcl ecl clisp) :results (nil t t t))
    maxima-cli--vvq+batch-string-question
    (:cmd "%s --lisp=%s --very-quiet --no-init --batch-string='(display2d:false, integrate(x^n,x)); y;'"
	  :stdout "(display2d:false,integrate(x^n,x))\nlog(x)" :stderr ""
	  :exit 1 :lisps (gcl sbcl ecl clisp) :results (t t t t))
    )
  "A list of property lists of the form (:cmd MAXIMA-COMMAND :stdout EXPECTED-OUTPUT :stderr EXPECTED-ERROR-OUPUT :exit EXPECTED-EXIT-CODE).")

(defun maxima-cli-call-process (maxima-command name args)
  ""
  (let ((stdout (format "*%s-stdout*" name))
	(stderr.out (make-temp-file (format "%s-stderr.out" name)))
	(stderr (format "%s-stderr.out" name))
	(args (cons "-c" (cons maxima-command args))))
    (apply #'call-process (append (list "/usr/bin/echo" nil (list stdout stderr.out) nil) args))
    (save-excursion
      (find-file-literally stderr.out)
      (list (maxima-cli--buffer-string stdout) (maxima-cli--buffer-string (current-buffer))))))
    
	
;;(maxima-cli-call-process maxima-command "test1" (split-string "--lisp=sbcl --very-very-quiet --no-init --batch-string='printf(true,\"HELLO~%%\");1/0;printf(true,\"Integral_xdx=~a~%%\",integrate(x,x));'"))
