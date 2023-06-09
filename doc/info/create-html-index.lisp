(in-package :maxima)

(defvar *html-index*
  (make-hash-table :test #'equal)
  "Hash table for looking up which html file contains the
  documentation.  The key is the topic we're looking for and the value
  is the html file containing the documentation for the topic.")

(defvar *log-file* nil)

(let ((maxima_nnn-pattern (pregexp:pregexp "^maxima_[0-9][0-9]*$")))
  (defun maxima_nnn-p (f)
    (pregexp:pregexp-match maxima_nnn-pattern f)))

;; Similar to grep -l: returns F-PATH if file contains CONTENT-PATTERN, otherwise NIL.

(defun grep-l (content-pattern f-path)
  (with-open-file (s f-path)
    (loop for line = (read-line s nil)
          while line
            do (when (pregexp:pregexp-match content-pattern line)
                 (return f-path)))))

(defun add-entry (item item-id file line &key replace-dash-p prefix)
  ;; Add entry to the hash table.
  ;;
  ;; Replace any special chars that texinfo has encoded.
  (setf item (handle-special-chars item))

  ;; Replace "-" with space, but only if "-" is not followed by a
  ;; digit.  "foo-1" is the info index named "foo <1>", so we don't
  ;; want to change that.
  (when replace-dash-p
    (setf item (pregexp:pregexp-replace* "-([^[:digit:]])" item " \\1")))

  ;; Check if the entry already exists and print
  ;; a message.  Presumably, this shouldn't happen, so warn if it
  ;; does.
  (when (gethash item *html-index*)
    (format t "Already added entry ~S ~S: ~S~%"
	    item (gethash item *html-index*)
	    line))
  (format *log-file* "~A: ~S -> ~S ~S~%"
	  prefix item file item-id)
  (setf (gethash item *html-index*)
	(cons file item-id)))

(defun process-line (line matcher &key replace-dash-p (prefix "Add:"))
  (multiple-value-bind (item item-id file line)
      (funcall matcher line)
    (when item
      (add-entry item item-id file line
		 :replace-dash-p replace-dash-p
		 :prefix prefix))))

(defun process-one-html-file (file matcher replace-dash-p prefix)
  (format *debug-io*  "Processing: ~S~%" file)
  (with-open-file (s file :direction :input)
    (loop for line = (read-line s nil)
          while line
	  do
	     (process-line line matcher
			   :replace-dash-p replace-dash-p
			   :prefix prefix))))

(defun handle-special-cases ()
  ;; These HTML topics need special handling because we didn't quite
  ;; process them correctly previously because we accidentally changed
  ;; #\- to #\space.
  (flet ((update-entry (old new)
	   (when (gethash old *html-index*)
	     (setf (gethash new *html-index*)
		   (gethash old *html-index*))
	     (remhash old *html-index*))))
    (dolist (items '(("Quote quote operator" "Quote-quote operator")
		     ("Assignment operator (evaluates left hand side)"
		      "Assignment operator (evaluates left-hand side)")
		     ("Euler Mascheroni constant"
		      "Euler-Mascheroni constant")))
      (destructuring-bind (old new)
	  items
	(update-entry old new)))))

(let ((href (pregexp:pregexp "<a href=\"(maxima_[[:digit:]]+\.html)#index-([^\"]*)\">")))
  (defun match-entries (line)
    (let ((match (pregexp:pregexp-match href line)))
      (when match
	(destructuring-bind (whole file item-id)
	    match
	  (declare (ignore whole))
	  (values item-id item-id file line))))))

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
  (let ((files (directory dir)))

    ;; Ensure that the call to SORT below succeeds:
    ;; the only allowable names are "maxima_toc.html", "maxima.html", or "maxima_nnn.html",
    ;; where nnn is an integer;
    ;; also, if a file with an otherwise-allowable name is function and variable index,
    ;; or a list of documentation categories, exclude it too.

    #+nil
    (setf files (remove-if-not #'(lambda (f-path) 
                                   (let ((f-name (pathname-name f-path)))
                                     #+nil (format t "BUILD-HTML-INDEX: F-PATH = ~a, F-NAME = ~a.~%" f-path f-name)
                                     (or
                                      (string-equal f-name "maxima_toc")
                                      (string-equal f-name "maxima")
                                      (and 
                                       (maxima_nnn-p f-name)
                                       #+nil
				       (not (grep-l "<title>(Function and Variable Index|Documentation Cat)" f-path)))
                                      (format t "BUILD-HTML-INDEX: omit ~S.~%"
					      (namestring f-path)))))
			       files))
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
  
;; Run this build a hash table from the topic to the HTML file
;; containing the documentation.  The single argument DIR should be a
;; directory that contains the html files to be searched for the
;; topics.  For exapmle it can be "<maxima-dir>/doc/info/*.html"
(defun build-html-index (dir)
  (clrhash *html-index*)
  (let ((index-file (find-index-file dir)))
    (unless index-file
      (error "Could not find HTML file containing the function and variable index."))

    (with-open-file (*log-file* "build-html-index.log" :direction :output :if-exists :supersede)

      (process-one-html-file index-file #'match-entries t "Add")
      (handle-special-cases)
      (process-one-html-file (truename "maxima_toc.html") #'match-toc nil "TOC"))))

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
