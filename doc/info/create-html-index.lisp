(in-package :maxima)

(defvar *texinfo-version-string* nil
  "The texinfo version string used to generate the HTML files.")

(defvar *texinfo-version* nil
  "The texinfo version arranged as an integer.  Version 7.0.3 is
  represented as 70003.")

(defvar *html-index*
  (make-hash-table :test #'equal)
  "Hash table for looking up which html file contains the
  documentation.  The key is the topic we're looking for and the value
  is the html file containing the documentation for the topic.")

(defvar *log-file* nil
  "Log file containing info for each entry that is added to the index
  table.")

(let ((maxima_nnn-pattern (pregexp:pregexp "^maxima_[0-9][0-9]*$")))
  (defun maxima_nnn-p (f)
    "Determine if F is a pathname-name that looks like
    \"maxima_<digits>\".  Return non-NIL if so."
    (pregexp:pregexp-match maxima_nnn-pattern f)))

(defun grep-l (content-pattern f-path)
  "Similar to grep -l: returns F-PATH if file contains CONTENT-PATTERN, otherwise NIL."
  (with-open-file (s f-path)
    (loop for line = (read-line s nil)
          while line
            do (when (pregexp:pregexp-match content-pattern line)
                 (return f-path)))))

(defun add-entry (item item-id file line &key replace-dash-p prefix)
  "Add entry to the html hash table.  ITEM is the key, FILE is the html
  file where this item is found, ITEM-ID is the html id of this item.
  LINE is the line where this was found and is used for information if
  a duplicate key was found."

  ;; Replace any special chars that texinfo has encoded.
  (setf item (handle-special-chars item))

  (let ((posn-of-dash (position #\- item)))
    (when (and posn-of-dash (< posn-of-dash (1- (length item))))
      (cond
	((digit-char-p (aref item (1+ posn-of-dash)))
	 ;; We have something like "ztics-1".  This is the info entry
	 ;; named "ztics <1>", so rename the item appropriately.
	  (setf item (pregexp:pregexp-replace "-([[:digit:]])" item " <\\1>")))
	(replace-dash-p
	 ;; Replace "-" with a space (if requested).  The HTML output
	 ;; puts "-" where spaces were originally in the texi name.
	 ;; But this isn't perfect.  See HANDLE-SPECIAL-CASES where we
	 ;; shouldn't have done this.
	 (setf item (pregexp:pregexp-replace* "-([^[:digit:]])" item " \\1"))))))

  ;; Check if the entry already exists and print a message.  This
  ;; shouldn't happen, so print a message if it does.
  (when (gethash item *html-index*)
    (format t "Already added entry ~S ~S: ~S~%"
	    item (gethash item *html-index*)
	    line))
  (format *log-file* "~A: ~S -> ~S ~S~%"
	  prefix item file item-id)

  (setf (gethash item *html-index*)
	(cons file item-id)))

(defun process-line (line matcher &key replace-dash-p (prefix "Add:"))
  "Process the LINE using the function MATCHER to determine if this line
  contains something interesting to add to the index. REPLACE-DASH-P
  and PREFIX are passed to ADD-ENTRY."
  (multiple-value-bind (item item-id file line)
      (funcall matcher line)
    (when item
      (add-entry item item-id file line
		 :replace-dash-p replace-dash-p
		 :prefix prefix))))

(defun process-one-html-file (file matcher replace-dash-p prefix)
  "Process one html file named FILE using MATCHER to determine matches.
  REPLACE-DASH-P and PREFIX are passed to PROCESS-LINE which will
  handle these."
  (format *debug-io*  "Processing: ~S~%" file)
  (with-open-file (s file :direction :input)
    (loop for line = (read-line s nil)
          while line
	  do
	     (process-line line matcher
			   :replace-dash-p replace-dash-p
			   :prefix prefix))))

(defun handle-special-cases ()
  "These HTML topics need special handling because we didn't quite
  process them correctly previously because we accidentally changed
  #\- to #\space."
  (flet ((update-entry (old new)
	   ;; Set the new index entry with the value of the old index
	   ;; entry and remove the old entry from the index.
	   (when (gethash old *html-index*)
	     (setf (gethash new *html-index*)
		   (gethash old *html-index*))
	     (remhash old *html-index*))))
    ;; List of special cases.  The car of each item is the index entry
    ;; that should not have had the hyphen replaced.  The cdr is what
    ;; it should be.
    (dolist (items '(("Quote quote operator" "Quote-quote operator")
		     ("Assignment operator (evaluates left hand side)"
		      "Assignment operator (evaluates left-hand side)")
		     ("Euler Mascheroni constant"
		      "Euler-Mascheroni constant")))
      (destructuring-bind (old new)
	  items
	(update-entry old new)))

    ;; This is messy.  Texinfo 6.8 uses plain apostrophes in the info
    ;; file.  But with texinfo 7.0.3, some entries in HTML use an
    ;; apostrophe (U+27) character, but the info file uses
    ;; Right_Single_Quotation_Mark (U+2019).  And apparently, the next version of Texinfo will not.
    ;;
    ;; Convert these only for the cases we know this is a problem.
    ;;
    ;; This current implementation will very likely not work with gcl
    ;; only supports 8-bit characters.
    (when (and *texinfo-version*
	       (= *texinfo-version* 70003))
      (dolist (item '("Euler's number"
		      "Introduction to Maxima's Database"))
	(update-entry item
		      (pregexp::pregexp-replace* "'" item (string (code-char #x2019))))))))

;; Find entries from the function and variable index.  An example of
;; what we're looking for:
;;
;;   <a href="maxima_55.html#index-asinh">
;;
;; We extract the file name and the stuff after "#index-" which is the
;; html id that we need.  It's also the key we need for the hash
;; table.
(let ((href (pregexp:pregexp "<a href=\"(maxima_[[:digit:]]+\.html)#index-([^\"]*)\">")))
  (defun match-entries (line)
    (let ((match (pregexp:pregexp-match href line)))
      (when match
	(destructuring-bind (whole file item-id)
	    match
	  (declare (ignore whole))
	  (values item-id item-id file line))))))

;; Find entries from the TOC.  An example of what we're looking for:
;;
;;  <a id="toc-Bessel-Functions-1" href="maxima_14.html#Bessel-Functions">15.2 Bessel Functions</a>
;;
;; We extract the file name and then the title of the subsection.
;; Further subsections are ignored.
(let ((regexp (pregexp:pregexp "<a id=\"toc-.*\" href=\"(maxima_[^\"]+)\">[[:digit:]]+\.[[:digit:]]+ ([^\"]+?)<")))
  (defun match-toc (line)
    (let ((match (pregexp:pregexp-match regexp line)))
      (when match
	;;(format t "match: ~S: ~A~%" match line)
	(destructuring-bind (whole file item)
	    match
	  (declare (ignore whole))
	  ;; Replace "&rsquo;" with "'"
	  (when (find #\& item :test #'char=)
	    (setf item (pregexp:pregexp-replace* "&rsquo;" item (string (code-char #x27)))))

	  (format *log-file* "TOC: ~S -> ~S~%" item file)

	  (values item "" file line))))))

(defun find-index-file (dir)
  "Find the name of HTML file containing the function and variable
  index."
  (let ((f-a-v-i (merge-pathnames "Function-and-Variable-Index.html"
				  dir)))
    (when (probe-file f-a-v-i)
      (return-from find-index-file f-a-v-i)))
  (let ((files (directory dir)))
    ;; Keep just the files of the form "maxima_nnn", where "nnn" are
    ;; digits.  These are the only files that can contain the function
    ;; and variable index that we want.
    (setf files (remove-if-not
		 #'(lambda (path-name)
		     (maxima_nnn-p path-name))
		 files
		 :key #'pathname-name))

    ;; Now sort them in numerical order.
    (setf files
	  (sort files #'<
		:key #'(lambda (p)
			 (let ((name (pathname-name p)))
			   ;; Everything else is the number in the
			   ;; file name, which starts with 1.
			   (if (> (length name) 7)
			       (parse-integer (subseq name 7))
			       0)))))

    ;; Check if the last 2 files to see if one of them contains the
    ;; function and variable index we want.  Return the first one that
    ;; matches.
    (format t "Looking for function and variable index~%")
    (dolist (file (last files 2))
      (when (grep-l "<title>(Function and Variable Index)" file)
	(format t "Function index: ~S.~%"
		(namestring file))
	(return-from find-index-file file)))))
  
(defun get-texinfo-version (file)
  "Get the texinfo version from FILE"
  (let ((version-line
	  (with-open-file (f file :direction :input)
	    ;; Texinfo write a comment line containing the version number of
	    ;; the texinfo used to build the html file.  It's at the
	    ;; beginnning so search just the first 5 lines or so.
	    (loop for count from 1 to 5
		  for line = (read-line f nil) then (read-line f nil)
		  when (and line (search "Created by GNU Texinfo" line))
		    return line))))
    (unless version-line
      (warn "Could not find GNU Texinfo version in ~S~%" file)
      (return-from get-texinfo-version))
    (setf *texinfo-version-string*
	  (second (pregexp:pregexp-match "GNU Texinfo \(.*?\)," version-line)))
    (when *texinfo-version-string*
      (let ((posn 0)
	    (len (length *texinfo-version-string*))
	    (version 0))
	(dotimes (k 3)
	  (cond
	    ((<= posn len)
	     (multiple-value-bind (digits end)
		 (parse-integer *texinfo-version-string*
				:start posn
				:junk-allowed t)
	       (when digits
		 (setf version (+ digits (* version 100))))
	       (setf posn (1+ end))))
	    (t
	     (setf version (* version 100)))))
	(setf *texinfo-version* version)))))

(defun build-html-index (dir)
  (clrhash *html-index*)
  (let ((index-file (find-index-file dir)))
    (unless index-file
      (error "Could not find HTML file containing the function and variable index."))

    (with-open-file (*log-file* "build-html-index.log"
				:direction :output :if-exists :supersede)
      (get-texinfo-version (truename "maxima_toc.html"))
      (format t "Texinfo Version ~A: ~D~%" *texinfo-version-string* *texinfo-version*)
      (process-one-html-file index-file #'match-entries t "Add")
      (process-one-html-file (truename "maxima_toc.html") #'match-toc nil "TOC")
      (handle-special-cases))))

;; Run this to build a hash table from the topic to the HTML file
;; containing the documentation.  The single argument DIR should be a
;; directory that contains the html files to be searched for the
;; topics.  For exapmle it can be "<maxima-dir>/doc/info/*.html"
(defmfun $build_and_dump_html_index (dir)
  (build-html-index dir)
  (let (entries)
    (maphash #'(lambda (k v)
		 (push (list k (namestring (car v)) (cdr v)) entries))
	     *html-index*)
    (format t "HTML index has ~D entries~%" (hash-table-count *html-index*))
    ;; Hash table can't be empty unless we screwed up somewhere.
    (assert (plusp (hash-table-count *html-index*)))

    (with-open-file (s "maxima-index-html.lisp"
		       :direction :output
		       :if-exists :supersede)
      (with-standard-io-syntax
	;; Set up printer settings to print the output the way we want.
	;;
	;; *package* set to :cl-info so that the symbols aren't
	;; preceded by the cl-info package marker.
	;;
	;; *print-length* is NIL because the list of entries is very
	;; long.
	;;
	;; *print-case* is :downcase just to make it look more
	;; natural; not really needed.
	;;
	;; *print-readably* is nil so base-strings and strings can be
	;; printed without any kind of special syntax for base-strings
	;; for lisps that distinguish between strings and
	;; base-strings.
	(let ((*package* (find-package :cl-info))
	      (*print-length* nil)
	      (*print-case* :downcase)
	      (*print-readably* nil))
	  (format s ";;; Do not edit; automatically generated via build-html-index.lisp~2%")
	  (pprint '(in-package :cl-info)
		  s)

	  (pprint `(let ((cl-info::html-index ',entries))
		     (cl-info::load-html-index cl-info::html-index))
		  s))))))

;;(build-and-dump-html-index "./*.html")
