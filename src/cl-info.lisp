;;; This is port of GCL's info.lsp to Clisp.  This should basically be
;;; portable Common Lisp, but I haven't tested it with anything else.

;; CAUTION: This file contains non-printing characters!
;;
;; The regexp syntax used in this file is the syntax used by Clisp's
;; regexp module, which claims to use Posix regexps.
;;
;; In summary:
;;
;; .       matches any single character
;; []      character set
;; ^       beginning of line
;; $       end of line
;; \( \)   grouping
;; *       zero or more
;; \?      zero or one matches
;; \+      one or more
;; \|      alternative


(in-package "SI")
#+clisp
(progn
(defvar *match-data*)
(defvar *case-fold-search* nil)
)

(eval-when (compile eval)
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
      (let ((tem (make-array (- len start)
			     :element-type 'base-char)))
	(when (> start 0)
	  (file-position st start))
	(read-sequence tem st :start 0 :end (length tem))
	tem))))

(defun atoi (string start)
  (parse-integer string :start start :junk-allowed t))

;; FILE is the main (first) info file. Search for the Indirect nodes
;; and the tag table which exists if the info files are split into
;; several files.
;;
;; Return a list of the tag table text itself and an alist of the
;; starting index for each file and the name of the corresponding
;; file.
(defun info-get-tags (file)
  (let ((lim 0)
	(*case-fold-search* t)
	*match-data* tags files)
    (declare (fixnum lim))
    (let ((s (file-to-string file))
	  (i 0))
      (declare (fixnum i)
	       (string s))
      ;;(format t "match = ~A~%" (string-match #u"[\n]\\+Indirect:" s 0))
      (when (>= (string-match #u"[\n]\\+Indirect:" s 0) 0)
	;; The file has multiple parts, so save the filename and the
	;; offset of each part.
	(setq i (match-end 0))
	(setq lim (string-match #u"" s i))
	(while (>= (string-match #u"\n\\([^\n]\\+\\): \\([0-9]\\+\\)" s i lim)
		   0)
	     
	  (setq i (match-end 0))
	  (setq files
		(cons (cons
		       (atoi s (match-beginning 2))
		       (get-match s 1)
		       )
		      files))))
      (when (>=  (string-match #u"[\n]\\+Tag Table:" s i) 0)
	(setq i (match-end 0))
	(when (>= (string-match "" s i) 0)
	  (setq tags (subseq s i (match-end 0)))))
      (if files
	  (or tags (info-error "Need tags if have multiple files")))
      (list* tags (nreverse files)))))

;; Quote the given string, protecting any special regexp characters so
;; that they stand for themselves.
#-clisp
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
	   (when (position ch "\\[].*|^$")
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
#+clisp
(defun re-quote-string (x)
  (regexp:regexp-quote x))

(defun get-match (string i)
  (subseq string (match-beginning i) (match-end i)))

(defun string-concatenate (&rest strings)
  (apply #'concatenate 'string strings))

(defun get-nodes (pat node-string)
  (let ((i 0)
	(*case-fold-search* t)
	(ans '())
	(*match-data* nil))
    (declare (fixnum i))
    (when node-string
      (setq pat
	    (string-concatenate "Node: \\([^]*" (re-quote-string
						  pat) "[^]*\\)"))
      (while (>= (string-match pat node-string i) 0)
	(setq i (match-end 0))
	(setq ans (cons (get-match node-string 1) 
			ans)))
      (nreverse ans))))

(defun get-index-node ()
  (or (third *current-info-data*) 
      (let* (s
	     (node-string (car (nth 1 *current-info-data*)))
	     (node
	      (and node-string (car (get-nodes "index" node-string)))))
	(when node
	  (setq s (show-info node nil))
	  (setf (third *current-info-data*) s)))))

(defun nodes-from-index (pat)
  (let ((i 0)
	ans
	(*case-fold-search* t)
	*match-data*
	(index-string (get-index-node)))
    (when index-string
      (setq pat 
	    (string-concatenate #u"\n\\* \\([^:\n]*" (re-quote-string
						    pat)
				#u"[^:\n]*\\):[ \t]\\+\\([^\t\n,.]\\+\\)"))
      (while (>= (string-match pat index-string i) 0)
	(setq i (match-end 0))
	(push (cons (get-match index-string 1) (get-match index-string 2))
	      ans))
      (nreverse ans))))

(defun get-node-index (pat node-string)
  (let ((node pat)
	*match-data*)
    (cond ((null node-string) 0)
	  (t
	   (setq pat
		 (string-concatenate "Node: "
				     (re-quote-string pat)
				     "\\([0-9]\\+\\)"))
	   (cond ((>= (string-match pat node-string) 0)
		  (atoi node-string (match-beginning 1)))
		 (t
		  (info-error "can't find node ~s" node) 0))))))

(defun all-matches (pat st)
  (let ((start 0)
	*match-data*)
    (declare (fixnum start))
    (loop while (>= (setq start (string-match pat st start)) 0)
      do
      collect (list start (setq start (match-end 0))))))



(defmacro node (prop x)
  `(nth ,(position prop '(string begin end header name
				 info-subfile
				 file tags)) ,x)) 

(defun node-offset (node)
  (+ (car (node info-subfile node)) (node begin node)))

(defun file-search (name &optional (dirs *info-paths*) extensions (fail-p t))
  "Search for the first occurrence of a file in the directory list DIRS
that matches the name NAME with extention EXT"
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
    (error "Lookup failed in directores: ~S for name ~S with extensions ~S"
	   dirs name extensions))
  nil)

(defvar *old-lib-directory* nil)
(defun setup-info (name)
  (let (tem file)
    (when (equal name "DIR")
      (setq name "dir"))
    (setq file (file-search name *info-paths* '("" "info") nil))
    (cond ((and (null file)
		(not (equal name "dir")))
	   (let* ((tem (show-info "(dir)Top" nil))
		  *case-fold-search*)
	     (cond ((>= (string-match
			 (string-concatenate "\\(([^(]*"
					     (re-quote-string name)
					     "(.info)?)\\)")
			 tem)
			0)
		    (setq file (get-match tem 1)))))))
    (cond (file
	   (let* ((na (namestring (truename file))))
	     (cond ((setq tem (assoc na *info-data* :test 'equal))
		    (setq *current-info-data* tem))
		   (t
		    (setq *current-info-data*
			  (list na (info-get-tags na) nil))
		    (setq *info-data* (cons *current-info-data* *info-data*))))))
	  (t
	   (format t "(not found ~s)" name)))
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
;  "For an index N return (START . FILE) for info subfile
; which contains N.   A second value bounding the limit if known
; is returned.   At last file this limit is nil."
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
(defun info-node-from-position (n &aux  (i 0))
  (let* ((info-subfile (info-subfile n))
	 (s (info-get-file (cdr info-subfile)))
	 (end (- n (car info-subfile))))
    (while (>=  (string-match #u"" s i end) 0)
      (setq i (match-end 0)))
    (setq i (- i 1))
    (if (>= (string-match
	       #u"[\n][^\n]*Node:[ \t]\\+\\([^\n\t,]\\+\\)[\n\t,][^\n]*\n"  s i) 0)
	(let* ((i (match-beginning 0))
	       (beg (match-end 0))
	       (name (get-match s 1))
	       (end(if (>= (string-match "[]" s beg) 0)
		       (match-beginning 0)
		     (length s)))
	       (node (list* s beg end i name info-subfile
				 *current-info-data*)))
	  node))))
    
#+nil
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
    (when (>= (string-match "^\\(([^(]+)\\)([^)]*)" name) 0)
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
	(cond ((>= (string-match
		      (string-concatenate
		       #u"[\n][^\n]*Node:[ \t]\\+"
		       (re-quote-string name)
		       #u"[,\t\n][^\n]*\n") 
		      s start)
		   0)
	       (let* ((i (match-beginning 0))
		      (beg (match-end 0))
		      (end (if (>= (string-match "[]" s beg) 0)
			       (match-beginning 0)
			       (length s))))

		 (when position-pattern
		    (setq position-pattern (re-quote-string position-pattern))

		    (let (*case-fold-search*)
		      (when (or (>= (setq subnode
					  (string-match
					   (string-concatenate
					    #u"\n - [A-Za-z ]\\+: "
					    position-pattern
					    #u"[ \n]"
					    )
					   s beg end))
				    0)
				(>= (string-match position-pattern s beg end)
				    0))
			(setq initial-offset
			      (- (match-beginning 0) beg)))))

		 (let ((e
			(if (and (>= subnode 0)
				 (>=
				  (string-match #u"\n - [A-Z]"
						s (+ beg 1
						     initial-offset)
						end)
				  0))
			    (match-beginning 0)
			    end)))
		   (subseq s (+ initial-offset beg) e )
		   )
		 ))
	      (t
	       (info-error "Can't find node  ~a?" name)))))))

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
    (when (>= (string-match "^\\(([^(]+)\\)([^)]*)" name) 0)
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
		      #u"[\n][^\n]*Node:[ \t]\\+"
		      (re-quote-string name)
		      #u"[,\t\n][^\n]*\n") 
		     (or s "") start)
		    0)
	  (info-error "Can't find node  ~a?" name))
	(let* ((i (match-beginning 0))
	       (beg (match-end 0))
	       (end (if (>= (string-match "[]" s beg) 0)
			(match-beginning 0)
			(length s))))

	  (when position-pattern
	    (setq position-pattern (re-quote-string position-pattern))

	    (let (*case-fold-search*)
	      (when (or (>= (setq subnode
				  (string-match
				   (string-concatenate
				    #u"\n - [A-Za-z ]\\+: "
				    position-pattern
				    #u"[ \n]"
				    )
				   s beg end))
			    0)
			(>= (string-match position-pattern s beg end)
			    0))
		(setq initial-offset
		      (- (match-beginning 0) beg)))))

	  (let ((e (if (and (>= subnode 0)
			    (>=
			     (string-match #u"\n - [A-Z]"
					   s (+ beg 1
						initial-offset)
					   end)
			     0))
		       (match-beginning 0)
		       end)))
	    (subseq s (+ initial-offset beg) e )))))))

(defvar *default-info-files* '( "gcl-si.info" "gcl-tk.info" "gcl.info"))

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

#+debug ; try searching
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
	(if (> (length tem) 1)
	    (format t "~%Enter n, all, none, or multiple choices eg 1 3 : ")
	    (terpri))
	(let ((line (if (> (length tem) 1)
			(read-line)
			"0"))
	      (start 0)
	      val)
	  (while (equal line "")
	    (setq line (read-line)))
	  (while (multiple-value-setq
		     (val start)
		   (read-from-string line nil nil :start start))
	    (cond ((numberp val)
		   (setq wanted (cons val wanted)))
		  (t
		   (setq wanted val)
		   (return nil))))
	  (cond ((consp wanted)
		 (setq wanted (nreverse wanted)))
		((symbolp wanted)
		 (setq wanted (and
			       (equal (symbol-name wanted) "ALL")
			       (loop for i below (length tem)
				     collect i)))))
	  (when wanted
	    ;; Remove invalid (numerical) answers
	    (setf wanted (remove-if #'(lambda (x)
					(and (integerp x) (>= x nitems)))
				    wanted))
	    (format t "~%Info from file ~a:" (car *current-info-data*)))
	  (loop for i in wanted
		do (princ (show-info (nth i tem))))))))
  (values))

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


