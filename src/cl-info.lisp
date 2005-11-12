;;; This is port of GCL's info.lsp to Clisp.  This should basically be
;;; portable Common Lisp, but I haven't tested it with anything else.

;; CAUTION: This file contains non-printing characters!
;;
;; The regexp syntax used in this file is the syntax used by nregex.
;;
;; In summary:
;;
;; .       (a period) matches any single character
;; []      character set
;; ^       beginning of line
;; $       end of line
;; ( )     grouping
;; *       zero or more
;; ?       zero or one matches
;; +       one or more
;;


(in-package "CL-INFO")

(defvar *match-data*)
(defvar *case-fold-search* nil)

(defun match-start (n)
  (first (aref *match-data* n)))
(defun match-beginning (n)
  (match-start n))
(defun match-end (n)
  (second (aref *match-data* n)))
(defun get-match (s n)
  (subseq s (match-start n) (match-end n)))


;; Compile the regex pattern in PAT for use by the string matcher.  We
;; precompile three regex's that are used for all queries. If there
;; were any more, I would have put them in a hash table 
;; -- jfa 07/24/04
(let* ((string1 (format nil "Node: ([^~a]*index[^~a]*)~a"
			(code-char 127) (code-char 127) (code-char 127)))
       (string2 (format nil "Node: Function and Variable Index~a([0-9]+)"
			(code-char 127)))
       (string3 (format nil "~a[~a~a][^~a]*Node:[~a~a]+Function and Variable Index[,~a~a][^~a]*~a"
			(code-char 31) (code-char 10) (code-char 12) 
			(code-char 10) (code-char 32) (code-char 9) 
			(code-char 9) (code-char 10) (code-char 10)
			(code-char 10)))
       (precomp-nil-string1
	(compile nil
		 (regex-compile 
		  string1
		  :case-sensitive nil)))
       (precomp-t-string2
	(compile nil
		 (regex-compile 
		  string2
		  :case-sensitive nil)))
       (precomp-t-string3
	(compile nil
		 (regex-compile 
		  string3
		  :case-sensitive nil))))
  (defun compile-regex (pat &key (case-sensitive t))
    (cond
      ((and (equal case-sensitive nil)
	    (string= pat string1))
       precomp-nil-string1)
      ((and (equal case-sensitive t)
	    (string= pat string2))
       precomp-t-string2)
      ((and (equal case-sensitive t)
	    (string= pat string3))
       precomp-t-string3)
      (t
       (let ((*compile-print* nil)
	     (*compile-verbose* nil)
	     #+(or cmu scl)
	     (*compile-progress* nil)
	     #+sbcl
	     (sb-ext:*compile-progress* nil)
	     #+gcl
	     (compiler:*compile-verbose* nil)
	     )
	 (compile nil
		  (regex-compile 
		   pat 
		   :case-sensitive case-sensitive))))))
  )
;; Search the string STRING for the pattern PAT.  Only the part of the
;; string bounded by START and END are searched.  PAT may either be a
;; string or a compiled regex (which is a function).
;;
;; If a match is not found, -1 is returned.  Otherwise, the index of
;; the start of the match is returned.  *match-data* contains
;; information about the matches for any groups in the pattern.
(defun string-match (pat string &optional (start 0) (end (length string)))
  (when (stringp pat)
    (setf pat (compile-regex pat :case-sensitive (not *case-fold-search*))))
  (if (funcall pat string :start start :end end)
      (progn
	(setf *match-data* (make-array *regex-groupings*))
	(dotimes (k *regex-groupings*)
	  (setf (aref *match-data* k) (aref *regex-groups* k)))
	(match-start 0))
      -1))

(eval-when (compile load eval)
  #-allegro
  (defmacro while (test &body body)
    `(loop while ,test do ,@ body))
  #+nil
  (defmacro f (op x y)
    `(,op (the fixnum ,x) (the fixnum ,y))))

;; #u"" is a C-style string where \n, \t, and \r are converted just as
;; in C.
(eval-when (compile eval load)
  (defun sharp-u-reader (stream subchar arg)
    (declare (ignore subchar arg))
    (let ((tem (make-array 10 :element-type 'base-char
			   :fill-pointer 0 :adjustable t)))
      (unless (eql (read-char stream) #\")
	(error "sharp-u-reader reader needs a \"right after it"))
      (loop
       (let ((ch (read-char stream)))
	 (cond ((eql ch #\") (return tem))
	       ((eql ch #\\)
		(setq ch (read-char stream))
		(setq ch (or (cdr (assoc ch '((#\n . #\newline)
					      (#\t . #\tab)
					      (#\r . #\return))))
			     ch))))
	 (vector-push-extend ch tem)))
      (coerce tem '(simple-array base-char (*)))))

  (set-dispatch-macro-character #\# #\u 'sharp-u-reader)
  )

(defvar *info-data* nil)
(defvar *current-info-data* nil)
(defvar *info-paths*
  '("" "/usr/info/" "/usr/local/lib/info/" "/usr/local/info/"
    "/usr/local/gnu/info/" "/usr/share/info/" ))


;; Read the contents of a file FILE starting at position START into a
;; string.
(defun file-to-string (file &optional (start 0))
  (with-open-file (st file)
    (let ((len (file-length st)))
      (unless (<= 0 start len)
	(error "illegal file start ~a" start))
      #-gcl
      (let ((tem (make-array (- len start)
			     :element-type 'base-char)))
	(when (> start 0)
	  (file-position st start))
	(read-sequence tem st :start 0 :end (length tem))
	tem)
      #+gcl
      (let ((tem (make-array (- len start)
			     :element-type 'string-char)))
	(if (> start 0) (file-position st start))
	(si::fread tem 0 (length tem) st) tem)
      )))

(defun atoi (string start)
  (parse-integer string :start start :junk-allowed t))

;; FILE is the main (first) info file. Search for the Indirect nodes
;; and the tag table which exists if the info files are split into
;; several files.
;;
;; Return a list of the tag table text itself and an alist of the
;; starting index for each file and the name of the corresponding
;; file.
(let ((pat-indirect-start (compile-regex #u"[\n]+Indirect:"
					 :case-sensitive t))
      (pat-end-ind (compile-regex #u""
				  :case-sensitive t))
      (pat-indirect (compile-regex #u"\n([^\n]+): ([0-9]+)"
				   :case-sensitive t))
      (pat-tag-table (compile-regex #u"[\n]+Tag Table:"
				    :case-sensitive t))
      )
  (defun info-get-tags (file)
    (let ((lim 0)
	  (*case-fold-search* t)
	  *match-data* tags files)
      (declare (fixnum lim))
      (let ((s (file-to-string file))
	    (i 0))
	(declare (fixnum i)
		 (string s))
	;;(format t "match = ~a~%" (string-match #u"[\n]+Indirect:" s 0))
	(when (>= (string-match pat-indirect-start s 0) 0)
	  ;; The file has multiple parts, so save the filename and the
	  ;; offset of each part.
	  (setq i (match-end 0))
	  ;;(format t "looking for end of Indirect, from ~a~%" i)
	  (setq lim (string-match pat-end-ind s i))
	  ;;(format t "found Indirect at ~a.  limit = ~a~%" i lim)
	  (while (>= (string-match pat-indirect s i lim)
		     0)
	    ;;(format t "found entry at ~a.~%" (match-start 0))
	    (setq i (match-end 0))
	    (setq files
		  (cons (cons
			 (atoi s (match-beginning 2))
			 (get-match s 1)
			 )
			files))))
	;;(format t "looking for tag table~%")
	(when (>=  (string-match pat-tag-table s i) 0)
	  (setq i (match-end 0))
	  ;;(format t "found tag table:  ~a ~a~%" (match-start 0) i)
	  (when (>= (string-match pat-end-ind s i) 0)
	    ;;(format t "found end at ~a ~a~%" i (match-start 0))
	    (setq tags (subseq s i (match-end 0)))))
	(if files
	    (or tags (info-error "need tags if have multiple files")))
	(list* tags (nreverse files)))))
  )

;; Quote the given string, protecting any special regexp characters so
;; that they stand for themselves.
(defun re-quote-string (x)
  (let ((i 0)
	(len (length x))
	ch
	(extra 0))
    (declare (fixnum i len extra)
	     (string x))
    (let (tem)
      (tagbody
       AGAIN
	 (while (< i len)
	   (setq ch (aref x i))
	   ;; (cond ((position ch "\\()[]+.*|^$?")
	   (when (position ch "\\()[].*|^$")
	     (if tem 
		 (vector-push-extend #\\ tem)
		 (incf extra)))
	   (when tem
	     (vector-push-extend ch tem))
	   (setq i (+ i 1)))
	 (cond (tem )
	       ((> extra 0)
		(setq tem 
		      (make-array (+ (length x) extra)
				  :element-type 'base-char :fill-pointer 0))
		(setq i 0)
		(go AGAIN))
	       (t (setq tem x)))
	 )
      tem)))

(defun string-concatenate (&rest strings)
  (apply #'concatenate 'string strings))

(defun get-nodes (pat node-string)
  (let ((i 0)
	(*case-fold-search* t)
	(ans '())
	(*match-data* nil))
    (declare (fixnum i))
    (when node-string
      (let ((compiled-pat
	     (compile-regex
	      (string-concatenate "Node: ([^]*"
				  (re-quote-string pat)
				  "[^]*)")
	      :case-sensitive (not *case-fold-search*))))
	(while (>= (string-match compiled-pat node-string i) 0)
	  (setq i (match-end 0))
	  (setq ans (cons (get-match node-string 1) 
			  ans)))
	(nreverse ans)))))

(defun get-index-node ()
  (or (third *current-info-data*) 
      (let* (s
	     (node-string (car (nth 1 *current-info-data*)))
	     (node
	      (and node-string (car (get-nodes "index" node-string)))))
	(when node
	  (setq s (show-info node nil))
	  (setf (third *current-info-data*) s)))))

;; Most of the cost of retrieving documentation is here.  This should
;; be fast.
(defun nodes-from-index (pat)
  (let ((i 0)
	ans
	(*case-fold-search* t)
	*match-data*
	(index-string (get-index-node)))
    (when index-string
      (let ((compiled-pat
	     (compile-regex
	      (string-concatenate #u"\n\\* ([^:\n]*"
				  (re-quote-string pat)
				  #u"[^:\n]*):[ \t]+([^\t\n,.]+)")
	      :case-sensitive (not *case-fold-search*))))
	(while (>= (string-match compiled-pat index-string i) 0)
	  (setq i (match-end 0))
	  (push (cons (get-match index-string 1) (get-match index-string 2))
		ans))
	(nreverse ans)))))

(defun get-node-index (pat node-string)
  (let ((node pat)
	*match-data*)
    (cond ((null node-string) 0)
	  (t
	   (setq pat
		 (compile-regex
		  (string-concatenate "Node: "
				      (re-quote-string pat)
				      "([0-9]+)")
		  :case-sensitive (not *case-fold-search*)))
	   (cond ((>= (string-match pat node-string) 0)
		  (atoi node-string (match-beginning 1)))
		 (t
		  (info-error "can't find node ~s" node) 0))))))

(defun all-matches (pat st)
  (let ((start 0)
	*match-data*)
    (declare (fixnum start))
    (loop while (>= (setq start (string-match pat st start)) 0)
	  collect (list start (setq start (match-end 0))))))



(defmacro node (prop x)
  `(nth ,(position prop '(string begin end header name
			  info-subfile
			  file tags)) ,x)) 

(defun node-offset (node)
  (+ (car (node info-subfile node)) (node begin node)))

(defun file-search (name &optional (dirs *info-paths*) extensions (fail-p t))
  "search for the first occurrence of a file in the directory list dirs
that matches the name name with extention ext"
  (dolist (dir dirs)
    (let ((base-name (make-pathname :directory (pathname-directory dir))))
      (dolist (type extensions)
	(let ((pathname (make-pathname :name name
				       :type (if (equalp type "")
						 nil
						 type)
				       :defaults base-name)))
	  (when (probe-file pathname)
	    (return-from file-search pathname))))))
  ;; We couldn't find the file
  (when fail-p
    (error "lookup failed in directores: ~s for name ~s with extensions ~s"
	   dirs name extensions))
  nil)

(defvar *old-lib-directory* nil)

(defun setup-info (name)
  (let (tem file)
    (when (equal name "DIR")
      (setq name "dir"))
    (setq file (file-search name *info-paths* '("" "info") nil))
    (cond (file
	   (let* ((na (namestring (truename file))))
	     (cond ((setq tem (assoc na *info-data* :test 'equal))
		    (setq *current-info-data* tem))
		   (t
		    (setq *current-info-data*
			  (list na (info-get-tags na) nil))
		    (setq *info-data* (cons *current-info-data* *info-data*))))))
      ; Don't bother to complain if name is not found; 
      ; SETUP-INFO is called over and over again.
	  ; (t (format t "(not found ~s)" name))
      )
    nil))
			  
(defun get-info-choices (pat type)
  (if (eql type 'index)
      (nodes-from-index pat )
      (get-nodes pat (car (nth 1 *current-info-data*)))))

(defun add-file (v file &aux (lis v))
  (while lis
    (setf (car lis) (list (car lis) file))
    (setq lis (cdr lis)))
  v)

(defun info-error (&rest l)
  (apply #'error l))

;; Cache the last file read to speed up lookup since it may be
;; gzipped. However, we don't support gzipped info files at this time.
(defvar *last-info-file* nil)

(defun info-get-file (pathname)
  (setq pathname
	(if (stringp (car *current-info-data*))
	    (merge-pathnames pathname
			     (car *current-info-data*))
	    pathname))
  (cdr 
   (cond ((equal (car *last-info-file*) pathname)
	  *last-info-file*)
	 (t (setq *last-info-file*
		  (cons pathname (file-to-string pathname)))))))

(defun info-subfile (n)
  ;;  "for an index n return (start . file) for info subfile
  ;; which contains N.   A second value bounding the limit if known
  ;; is returned.   At last file this limit is nil."
  (let ((lis (cdr (nth 1 *current-info-data*)))
	ans lim)
    (when (and lis (>= n 0))
      (dolist (v lis)
	(cond ((> (car v) n )
	       (setq lim (car v))
	       (return nil)))
	(setq ans v)
	))
    (values (or ans (cons 0 (car *current-info-data*))) lim)))

;;used by search
(let ((pat-marker (compile-regex #u"^_" :case-sensitive t))
      (pat-node (compile-regex
		 #u"[\n][^\n]*Node:[ \t]+([^\n\t,]+)[\n\t,][^\n]*\n"
		 :case-sensitive t))
      (pat-marker2 (compile-regex "[]")))
  (defun info-node-from-position (n &aux  (i 0))
    (let* ((info-subfile (info-subfile n))
	   (s (info-get-file (cdr info-subfile)))
	   (end (- n (car info-subfile))))
      (while (>=  (string-match pat-marker s i end) 0)
	(setq i (match-end 0)))
      (setq i (- i 1))
      (if (>= (string-match pat-node s i) 0)
	  (let* ((i (match-beginning 0))
		 (beg (match-end 0))
		 (name (get-match s 1))
		 (end (if (>= (string-match pat-marker2 s beg) 0)
			  (match-beginning 0)
			  (length s)))
		 (node (list* s beg end i name info-subfile
			      *current-info-data*)))
	    node))))
  )
    
;; SHOW-INFO is the main routine to find the desired documentation.

(let ((pat-file-node (compile-regex "^(\\([^(]+\\))([^)]*)"))
      ;; This is the pattern for the beginning of a node
      (pat-markers (compile-regex "[]"))
      ;; This is the pattern for a subnode.  That is the documention
      ;; for some function or variable.
      (pat-subnode (compile-regex #u"\n -+ [a-zA-Z]"))
      ;; This pattern is used to match where the documentation of a
      ;; subnode starts.
      (doc-start (compile-regex #u"\n   ")))
  (defun show-info (name &optional position-pattern)
    (let ((*match-data* nil)
	  (initial-offset 0)
	  (subnode -1)
	  info-subfile file)
      (declare (fixnum subnode initial-offset))
      (when (and (consp name) (consp (cdr name)))
	(setq file (cadr name)
	      name (car name)))
      (when (consp name)
	(setq position-pattern (car name) name (cdr name)))
      (unless (stringp name)
	(info-error "bad arg"))
      (when (>= (string-match pat-file-node name) 0)
	;; (file)node
	(setq file (get-match name 1))
	(setq name (get-match name 2))
	(when (equal name "")
	  (setq name "Top")))
      (when file
	(setup-info file))
      (let ((indirect-index (get-node-index name
					    (car (nth 1 *current-info-data*)))))
	(when (null indirect-index)
	  (format t "~%Sorry, Can't find node ~a" name)
	  (return-from show-info nil))
	
	(setq info-subfile (info-subfile indirect-index))

	(let* ((s (info-get-file (cdr info-subfile)))
	       (start (- indirect-index (car info-subfile))))
	  (unless (>= (string-match
		       (string-concatenate
			#u"[\n][^\n]*Node:[ \t]+"
			(re-quote-string name)
			#u"[,\t\n][^\n]*\n") 
		       (or s "") start)
		      0)
	    (info-error "Can't find node  ~a?" name))
	  (let* ((beg (match-end 0))
		 (end (if (>= (string-match pat-markers s beg) 0)
			  (match-beginning 0)
			  (length s))))

	    (when position-pattern
	      (setq position-pattern (re-quote-string position-pattern))

	      ;; This looks for the desired pattern.  A typical entry
	      ;; looks like
	      ;;
	      ;; " - Function: plot2d <random stuff>"
	      ;;
	      ;; So we look for the beginning of a line, the string " -
	      ;; ", followed by at least one letter or spaces and then a
	      ;; colon.  After that is our desired pattern and a space
	      ;; or new line.
	      (let (*case-fold-search*)
		(when (or (>= (setq subnode
				    (string-match
				     (string-concatenate
				      #u"\n -+ [A-Za-z ]+: "
				      position-pattern
				      #u"[ \n]"
				      )
				     s beg end))
			      0)
			  (>= (string-match position-pattern s beg end)
			      0))
		  (setq initial-offset
			(- (match-beginning 0) beg)))))
	  
	    ;; We now need to find the end of the documentation.
	    ;; Usually, the end is where the next node begins. However,
	    ;; sometimes several nodes are given in a row without
	    ;; separate documentation for each, whereby the same
	    ;; documentation is used to describe these nodes.
	    ;;
	    ;; So, what we do is look for the beginning of the
	    ;; documentation, which starts on a new line with at least 4
	    ;; spaces.  Then we look for the next node.  The is where
	    ;; our documentation ends.  If there is no next node, the
	    ;; end is where the marker is.
	    (let ((e (if (minusp subnode)
			 end
			 (let* ((start-doc
				 (string-match doc-start s
					       (+ beg 1 initial-offset) end)))
			   ;;(format t "start-doc = ~A~%" start-doc)
			   (cond ((>= (string-match pat-subnode s
						    (if (>= start-doc 0)
							start-doc
							(+ beg 1 initial-offset))
						    end)
				      0)
				  ;;(format t "end at ~A~%" (match-beginning 0))
				  (match-beginning 0))
				 (t
				  ;; No next node, so the end point we
				  ;; found is really the end point we
				  ;; want.
				  end))))))

	      (subseq s (+ initial-offset beg) e)))))))
  )

(defvar *default-info-files* '("maxima.info"))

(defun info-aux (x dirs)
  (loop for v in dirs
	do
	(setup-info v)
	append (add-file (get-info-choices x 'node) v)
	append (add-file (get-info-choices x 'index) v)))

(defun info-search (pattern &optional (start 0) end)
  "search for PATTERN from START up to END where these are indices in
the general info file.  The search goes over all files."
  (let ((limit 0))
    (while start
      (multiple-value-bind
	    (file lim)
	  (info-subfile start)
	(setq limit lim)
	(and end limit (<  end limit) (setq limit end))

	(let* ((s  (info-get-file (cdr  file)))
	       (beg (car file))
	       (i (- start beg))
	       (leng (length s)))
	  (when (>= (string-match pattern s i (if limit (- limit beg) leng)) 0)
	    (return-from info-search (+ beg (match-beginning 0)))))
	(setq start lim)))
    -1))

#+debug					; try searching
(defun try (pat &aux (tem 0) s )
  (while (>= tem 0)
    (cond ((>= (setq tem (info-search pat tem)) 0)
	   (setq s (cdr *last-info-file*))
	   (print (list
		   tem
		   (list-matches s 0 1 2)
		   (car *last-info-file*)
		   (subseq s
			   (max 0 (- (match-beginning 0) 50))
			   (min (+ (match-end 0) 50) (length s)))))
	   (setq tem (+ tem (- (match-end 0) (match-beginning 0))))))))
   
(defun idescribe (name)
  (let* ((items (info-aux name *default-info-files*)))
    (dolist (v items)
      (when (cond ((consp (car v))
		   (equalp (caar v) name))
		  (t (equalp (car v) name)))
	(format t "~%From ~a:~%" v)
	(princ (show-info v nil))))))

;; Main entry point.  This looks up the desired entry and prompts the
;; user to select the desired entries when multiple matches are found.
(defun info (x &optional (dirs *default-info-files*) (info-paths *info-paths*)
	     &aux *current-info-data*)
  (let (wanted
	file
	position-pattern
	tem
	(*info-paths* info-paths))
    (setf tem (info-aux x dirs))
    (when tem
      (let ((nitems (length tem)))
	(loop for i from 0 for name in tem with prev
	      do
	      (setq file nil
		    position-pattern nil)
	      (progn
		;; decode name
		(when (and (consp name) (consp (cdr name)))
		  (setq file (cadr name)
			name (car name)))
		(when (consp name)
		  (setq position-pattern (car name) name (cdr name))))
	      (when (> nitems 1)
		(format t "~% ~d: ~@[~a :~]~@[(~a)~]~a." i
			position-pattern
			(if (eq file prev) nil (setq prev file)) name)))
	(setq wanted
	      (if (> nitems 1)
		  (loop
		   for prompt-count from 0
		   thereis (progn
			     (finish-output *debug-io*)
			     (print-prompt prompt-count)
			     (force-output)
			     (clear-input)
			     (select-info-items
			      (parse-user-choice nitems) tem)))
		  tem))
	(clear-input)
	(finish-output *debug-io*)
	(when (consp wanted)
	  (loop for item in wanted
		do (princ (show-info item)))))))
  (values))

(defvar *prompt-prefix* "")
(defvar *prompt-suffix* "")

(defun print-prompt (prompt-count)
  (format t "~&~a~a~a"
	  *prompt-prefix*
	  (if (zerop prompt-count)
	      "Enter space-separated numbers, `all' or `none': "
	      "Still waiting: ")
	  *prompt-suffix*))

(defvar +select-by-keyword-alist+
  '((noop "") (all "a" "al" "all") (none "n" "no" "non" "none")))

(defun parse-user-choice (nitems)
  (loop
   with line = (read-line) and nth and pos = 0
   while (multiple-value-setq (nth pos)
	   (parse-integer line :start pos :junk-allowed t))
   if (or (minusp nth) (>= nth nitems))
   do (format *debug-io*
	      "~&Discarding invalid number ~d." nth)
   else collect nth into list
   finally
   (let ((keyword
	  (car (rassoc
		(string-right-trim
		 '(#\space #\tab #\newline #\;) (subseq line pos))
		+select-by-keyword-alist+
		:test #'(lambda (item list)
			  (member item list :test #'string-equal))))))
     (unless keyword
       (setq keyword 'noop)
       (format *debug-io* "~&Ignoring trailing garbage in input."))
     (return (cons keyword list)))))

(defun select-info-items (selection items)
  (case (pop selection)
    (noop (loop
	   for i in selection
	   collect (nth i items)))
    (all items)
    (none 'none)))

					#||
;; idea make info_text window have previous,next,up bindings on keys
;; and on menu bar.    Have it bring up apropos menu. allow selection
;; to say spawn another info_text window.   The symbol that is the window
;; will carry on its plist the prev,next etc nodes, and the string-to-file
;; cache the last read file as well.   Add look up in index file, so that can
;; search an indtqex as well.   Could be an optional arg to show-node
;; 



(defun default-info-hotlist()
  (namestring (merge-pathnames "hotlist" (user-homedir-pathname))))

(defun add-to-hotlist (node )
  (if (symbolp node) (setq node (get node 'node)))
  (cond
    (node
     (with-open-file
	 (st (default-info-hotlist)
	     :direction :output
	     :if-exists :append
	     :if-does-not-exist :create)
       (cond ((< (file-position st) 10)
	      (princ  #u"\nFile:\thotlist\tNode: Top\n\n* Menu: Hot list of favrite info items.\n\n" st)))
       (format st "* (~a)~a::~%" 
	       (node file node)(node name node))))))

(defun list-matches (s &rest l)
  (loop for i in l 
	collect
	(and (>= (match-beginning i) 0)
	     (get-match s i))))
||#


