;;;
;;; Alien interface to GNU regex, for CMUCL
;;;
;;;

(in-package "REGEX")

(export '(
	  ;; Constants
	  +re-backslash-escape-in-lists+
	  +re-bk-plus-qm+
	  +re-char-classes+
	  +re-context-indep-anchors+
	  +re-context-indep-ops+
	  +re-context-invalid-ops+
	  +re-dot-newline+
	  +re-dot-not-null+
	  +re-hat-lists-not-newline+
	  +re-intervals+
	  +re-limited-ops+
	  +re-newline-alt+
	  +re-no-bk-braces+
	  +re-no-bk-parens+
	  +re-no-bk-refs+
	  +re-no-bk-vbar+
	  +re-no-empty-ranges+
	  +re-unmatched-right-paren-ord+
	  ;; Common regexp syntaxes
	  +re-syntax-emacs+
	  +re-syntax-egrep+
	  +re-syntax-posix-common+
	  +re-syntax-posix-basic+
	  +re-syntax-posix-extended+
	  +re-syntax-spencer+
	  ;; Variables
	  *match-data*
	  *case-fold-search*
	  ;; Functions
	  match-data-start
	  match-data-end
	  string-match
	  match-beginning
	  match-end
	  ))

(use-package "ALIEN")
(use-package "C-CALL")

#+nil
(eval-when (:load-toplevel :compile-toplevel :execute)
  (ext:load-foreign "/apps/gnu/src/regex-0.12/regex.o")
  )

;;; From regex.h

;; GNU interface
(def-alien-type reg-syntax-t unsigned)
(def-alien-type re-pattern-buffer
  (struct re-pattern-buffer
	  (buffer (* unsigned-char))
	  (allocated unsigned)
	  (used unsigned)
	  (syntax unsigned)
	  (fastmap (* unsigned-char))
	  (translate (* unsigned-char))
	  (re-nsub int)
	  (bit-fields int)))

(def-alien-type re-registers
  (struct re-registers
	  (num-regs unsigned)
	  (start (* int))
	  (end (* int))))


(declaim (inline re-compile-pattern))
(def-alien-routine ("re_compile_pattern" re-compile-pattern) c-string
    (pattern c-string)
    (length int)
    (buffer (* re-pattern-buffer)))

(declaim (inline re-match))
(def-alien-routine ("re_match" re-match) int
  (buffer (* re-pattern-buffer))
  (string c-string)
  (length int)
  (start int)
  (regs (* re-registers)))

(declaim (inline re-set-registers))
(def-alien-routine ("re_set_registers" re-set-registers) void
  (buffer (* re-pattern-buffer))
  (regs (* re-registers))
  (num-regs unsigned)
  (starts (* int))
  (ends (* int)))

(declaim (inline re-regfree))
(def-alien-routine ("regfree" re-regfree) void
  (regs (* re-pattern-buffer)))

(declaim (inline re-search))
(def-alien-routine ("re_search" re-search) int
  (buffer (* re-pattern-buffer))
  (string c-string)
  (length int)
  (start int)
  (range int)
  (regs (* re-registers)))

(def-alien-variable ("re_syntax_options" re-syntax-options) reg-syntax-t)


;;; POSIX interface
#|
(def-alien-type regex-t re-pattern-buffer)
(def-alien-type regoff-t int)
(def-alien-type regmatch-t
  (struct regmatch-t
	  (rm-so int)
	  (rm-eo int)))

(declaim (inline re-regcomp))
(def-alien-routine ("regcomp" re-regcomp) int
  (preg (* regex-t))
  (regex c-string)
  (cflags int))

(declaim (inline re-regexec))
(def-alien-routine ("regexec" re-regexec) int
  (preg (* regex-t))
  (string c-string)
  (nmatch int)
  (pmatch (array regmatch-t) :in-out)
  (eflags int))

(declaim (inline re-regerror))
(def-alien-routine ("regerror" re-regerror) int
  (errcode int)
  (preg (* regex_t))
  (errbuf c-string)
  (errbuf-size int))
|#

(macrolet ((frob (&rest name-desc-list)
	     `(progn
		,@(let ((bit 1))
		    (mapcar #'(lambda (name-desc)
				(prog1
				    `(defconstant ,(first name-desc) ,bit ,(second name-desc))
				  (setf bit (ash bit 1))))
			    name-desc-list)))))
  (frob (+re-backslash-escape-in-lists+
	 "If this bit is not set, then \\ inside a bracket expression is
literal. If set, then such a \\ quotes the following character. ")
	(+re-bk-plus-qm+
	 "If this bit is not set, then + and ? are operators, and \\+ and \\? 
are literals. If set, then \\+ and \\? are operators and + and ? are
literals.")
	(+re-char-classes+
	 "If this bit is set, then character classes are supported.  They are:
[:alpha:], [:upper:], [:lower:], [:digit:], [:alnum:],
[:xdigit:],[:space:], [:print:], [:punct:], [:graph:], and
[:cntrl:]. If not set, then character classes are not supported.")
        (+re-context-indep-anchors+
         "If this bit is set, then ^ and $ are always anchors (outside bracket
     expressions, of course).
   If this bit is not set, then it depends:
        ^  is an anchor if it is at the beginning of a regular
           expression or after an open-group or an alternation operator;
        $  is an anchor if it is at the end of a regular expression, or
           before a close-group or an alternation operator.  
")
	(+re-context-indep-ops+ "")
	(+re-context-invalid-ops+ "")
	(+re-dot-newline+ "")
	(+re-dot-not-null+ "")
	(+re-hat-lists-not-newline+ "")
	(+re-intervals+ "")
	(+re-limited-ops+ "")
	(+re-newline-alt+ "")
	(+re-no-bk-braces+ "")
	(+re-no-bk-parens+ "")
	(+re-no-bk-refs+ "")
	(+re-no-bk-vbar+ "")
	(+re-no-empty-ranges+ "")
	(+re-unmatched-right-paren-ord+ "")))

(defconstant +re-syntax-emacs+ 0)
(defconstant +re-syntax-egrep+
  (logior +re-char-classes+ +re-context-indep-anchors+
	  +re-context-indep-ops+ +re-hat-lists-not-newline+
	  +re-newline-alt+ +re-no-bk-parens+
	  +re-no-bk-vbar+))

(defconstant +re-syntax-posix-common+
  (logior +re-char-classes+ +re-dot-newline+ +re-dot-not-null+
	  +re-intervals+ +re-no-empty-ranges+))

(defconstant +re-syntax-posix-basic+
  (logior +re-syntax-posix-common+ +re-bk-plus-qm+))

(defconstant +re-syntax-posix-extended+
  (logior +re-syntax-posix-common+ +re-context-indep-anchors+
	  +re-context-indep-ops+   +re-no-bk-braces+        
	  +re-no-bk-parens+        +re-no-bk-vbar+          
	  +re-unmatched-right-paren-ord+))

(defconstant +re-syntax-spencer+
  (logior +re-no-bk-parens+ +re-no-bk-vbar+))


#+nil
(defun allocate-re-regs (compiled-pattern-buffer)
  (declare (type (alien (* re-pattern-buffer)) compiled-pattern-buffer))
  (let* ((nregs (1+ (slot compiled-pattern-buffer 're-nsub)))
	 (re-regs (make-alien re-registers 1))
	 (reg-start (make-alien int nregs))
	 (reg-end (make-alien int nregs)))
    (re-set-registers compiled-pattern-buffer re-regs
		      nregs reg-start reg-end)
    (ext:finalize re-regs
		  #'(lambda ()
		      (format t "~&freeing re-regs~%")
		      (free-alien (slot (deref re-regs 0) 'start))
		      (free-alien (slot (deref re-regs 0) 'end))
		      ))
    re-regs))

(defun allocate-re-regs ()
  (let ((regs (make-alien re-registers 1)))
    (setf (slot (deref regs) 'num-regs) 0)
    regs))


(defun free-re-regs (re-regs)
  (declare (type (alien (* re-registers)) re-regs))

  (let ((r (deref re-regs)))
    ;;(format t "freeing ~A:~%" re-regs)
    ;;(format t " num-regs:    ~A~%" (slot r 'num-regs))
    (when (plusp (slot r 'num-regs))
      ;;(format t " free start:  ~A~%" (slot r 'start))
      ;;(format t " free end:    ~A~%" (slot r 'end))
      (free-alien (slot r 'start))
      (free-alien (slot r 'end))
      (free-alien re-regs)
      )))

(defun make-case-fold-table ()
  "Translation table to fold all uppercase ASCII characters to lower
case characters"
  (let ((tab (make-alien (unsigned 8) 256)))
    ;; Initialize the table to the 256 ASCII characters
    (dotimes (k 256)
      (setf (deref tab k) k))
    ;; Translate the upper case characters to lower case
    (loop for k from (char-int #\A) to (char-int #\Z)
	  do (setf (deref tab k) (- k #.(- (char-int #\A) (char-int #\a)))))
    tab))


(defvar *match-data* nil
  "The match-data from the latest successful string-match")
(declaim (type (or null (simple-array t (*))) *match-data*))

(defvar *case-fold-search* nil
  "Non-NIL if the character case should be folded during searchs")


(defun allocate-re-pattern-buffer ()
  (let* ((pat-buf-ptr (make-alien re-pattern-buffer 1))
	 (pat-buf (deref pat-buf-ptr 0)))
    ;; Set BUFFER to NIL and ALLOCATED to 0 so re_compile_pattern
    ;; allocates space for us.
    (setf (slot pat-buf 'buffer) nil)
    (setf (slot pat-buf 'allocated) 0)
    ;; We don't support fastmap
    (setf (slot pat-buf 'fastmap) (make-alien unsigned-char 256))
    ;; Set case folding appropriately
    (setf (slot pat-buf 'translate)
	  (if *case-fold-search* (make-case-fold-table) nil))
    pat-buf-ptr))


(defun dump-compiled-pattern (compiled-pattern)
  (declare (type (alien (* re-pattern-buffer)) compiled-pattern))
  (let ((pat-buf (deref compiled-pattern)))
    (format t "buffer      = ~S~%" (slot pat-buf 'buffer))
    (format t "allocated   = ~S~%" (slot pat-buf 'allocated))
    (format t "used        = ~S~%" (slot pat-buf 'used))
    (format t "syntax      = ~S~%" (slot pat-buf 'syntax))
    (format t "fastmap     = ~S~%" (slot pat-buf 'fastmap))
    (format t "re-nsub     = ~S~%" (slot pat-buf 're-nsub))
    (format t "translate   = ~S~%" (slot pat-buf 'translate))
    (format t "bit-fields  = ~:42,' ,' ,4B~%" (slot pat-buf 'bit-fields))
    ))

(defun compile-pattern (pattern-string)
  (declare (type string pattern-string))
  (let* ((pat-buf (allocate-re-pattern-buffer))
	 (comp (re-compile-pattern pattern-string
				   (length pattern-string)
				   pat-buf)))
    (when comp
      (unwind-protect
	   (error "~A in regexp ~S" comp pattern-string)
	;; Free up the pattern buffer
	(re-regfree pat-buf)
	(free-alien pat-buf)))
    pat-buf))

(defstruct match-data
  (start 0 :type (unsigned-byte 32))
  (end 0 :type (unsigned-byte 32)))

;; Copy the data in the alien re-register to a lisp array
(defun lispify-match-data (nsub re-regs)
  (declare (fixnum nsub)
	   (type (alien (* re-registers)) re-regs))
  (let* ((regs (deref re-regs))
	 (start (slot regs 'start))
	 (end (slot regs 'end))
	 (matches (make-array nsub)))
    (dotimes (k nsub)
      (setf (aref matches k)
	    (make-match-data :start (deref start k) :end (deref end k))))
    matches))
    
(defun string-match (pattern string
			     &optional (start 0) end
			     (syntax +re-syntax-spencer+))
  "Search the string STRING for the first pattern that matches the
regexp PATTERN.  The syntax used for the pattern is specified by
SYNTAX.  The search may start in the string at START and ends at END,
which default to 0 and the end of the string.

If there is a match, returns the index of the start of the match and
an array of match-data.  If there is no match, -1 is returned and
nil."
  (declare (type string pattern string))
  (setf re-syntax-options syntax)
  (let* ((comp-result (compile-pattern pattern)))
    ;; Make sure we free up the space for the pattern buffer.
    (unwind-protect
	 (progn
	   (cond (comp-result
		  (let* ((re-regs (allocate-re-regs)))
		    ;; Make sure we free up the space for the registers
		    (unwind-protect 
			 (progn
			   (let ((search-result
				  (re-search comp-result string (length string)
					     start (or end (length string))
					     re-regs)))
			     (cond ((>= search-result 0)
				    (let ((matches
					   (lispify-match-data
					    (1+ (slot (deref comp-result) 're-nsub))
					    re-regs)))
				      ;; Save the last match in the global var
				      (setf *match-data* matches)
				      (values search-result matches)))
				   (t
				    (values search-result nil)))))
		      ;; Free up the re-register since we're done with it now.
		      (free-re-regs re-regs))))
		 (t
		  (setf *match-data* nil)
		  (values -1 nil))))
      ;; Free the pattern buffer
      (re-regfree comp-result)
      (free-alien comp-result))))

(defun match-beginning (index &optional (match-data *match-data*))
  (if (and match-data (< index (length match-data)))
      (match-data-start (aref match-data index))
      -1))

(defun match-end (index &optional (match-data *match-data*))
  (if (and match-data (< index (length match-data)))
      (match-data-end (aref match-data index))
      -1))

