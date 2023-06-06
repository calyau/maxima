(in-package :maxima)

(defvar *html-index*
  (make-hash-table :test #'equal)
  "Hash table for looking up which html file contains the
  documentation.  The key is the topic we're looking for and the value
  is the html file containing the documentation for the topic.")

(defvar *html-section-index*
  (make-hash-table :test #'equal))

(defvar *log-file*)

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

(defun add-entry (item item-id file line)
  ;; Add entry to the hash table.
  ;;
  ;; Replace any special chars that texinfo has encoded.
  (setf item (handle-special-chars item))

  ;; Replace "-" with space, but only if "-" is not followed by a
  ;; digit.  "foo-1" is the info index named "foo <1>", so we don't
  ;; want to change that.
  (setf item (pregexp:pregexp-replace* "-([^[:digit:]])" item " \\1"))

  ;; Check if the entry already exists and print
  ;; a message.  Presumably, this shouldn't happen, so warn if it
  ;; does.
  (when (gethash item *html-index*)
    (format t "Already added entry ~S ~S: ~S~%"
	    item (gethash item *html-index*)
	    line))
  (format *log-file* "Add: ~S -> ~S ~S~%"
	  item file item-id)
  (setf (gethash item *html-index*)
	(cons file item-id)))

(defun process-line (line matcher)
  (multiple-value-bind (item item-id file line)
      (funcall matcher line)
    (when item
      (add-entry item item-id file line))))

(defun process-one-html-file (file matcher)
  (format *debug-io*  "Processing: ~S~%" file)
  (let ((base-name (make-pathname :name (pathname-name file)
                                  :type (pathname-type file))))
    (with-open-file (s file :direction :input)
      (loop for line = (read-line s nil)
            while line
	    do
	       (process-line line matcher)))))

(defun process-toc-line (line matcher)
  (multiple-value-bind (item item-id file line)
      (funcall matcher line)))

(defun process-toc (file matcher)
  (format *debug-io*  "Processing: ~S~%" file)
  (with-open-file (s file :direction :input)
    (loop for line = (read-line s nil)
              while line
	      do
		 (process-toc-line line matcher))))

(defun handle-special-cases ()
  ;; These HTML topics need special handling because we didn't quite
  ;; process them correctly previously.
  (flet ((update-entry (old new)
	   (when (gethash old *html-index*)
	     (setf (gethash new *html-index*)
		   (gethash old *html-index*))
	     (remhash old *html-index*))))
    (update-entry "Quote quote operator" "Quote-quote operator")

    (update-entry "Assignment operator (evaluates left hand side)"
		  "Assignment operator (evaluates left-hand side)")

    (update-entry "Euler Mascheroni constant"
		  "Euler-Mascheroni constant")))

(defun match-entries (line)
  (let ((href (pregexp:pregexp "<a href=\"(maxima_[[:digit:]]+\.html)#index-([^\"]*)\">"))
	match)
    (setf match (pregexp:pregexp-match href line))
    (when match
      (destructuring-bind (whole file item-id)
	  match
	(declare (ignore whole))
	(values item-id item-id file line)))))

(defun match-toc (line)
  (let* ((regexp (pregexp:pregexp "<a id=\"toc-.*\" href=\"(maxima_[^\"]+)\">[[:digit:]]+\.[[:digit:]]+ ([^\"]+?)<"))
	 (match (pregexp:pregexp-match regexp line)))
    (when match
      ;;(format t "match: ~S: ~A~%" match line)
      (destructuring-bind (whole file item)
	  match
	(declare (ignore whole))
	;; Replace "&rsquo;" with "'"
	(when (find #\& item :test #'char=)
	  (setf item (pregexp:pregexp-replace* "&rsquo;" item (string (code-char #x27)))))
	(format *log-file* "TOC: ~S -> ~S~%" item file)
	(when (gethash item *html-section-index*)
	  (format t "Duplicate section index key: ~S: ~S~%" item file))
	(setf (gethash item *html-section-index*)
	      (cons file ""))))))

;; Run this build a hash table from the topic to the HTML file
;; containing the documentation.  The single argument DIR should be a
;; directory that contains the html files to be searched for the
;; topics.  For exapmle it can be "<maxima-dir>/doc/info/*.html"
(defun build-html-index (dir)
  (clrhash *html-index*)
  (let ((files (directory dir)))

    ;; Ensure that the call to SORT below succeeds:
    ;; the only allowable names are "maxima_toc.html", "maxima.html", or "maxima_nnn.html",
    ;; where nnn is an integer;
    ;; also, if a file with an otherwise-allowable name is function and variable index,
    ;; or a list of documentation categories, exclude it too.

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

    ;; Now sort them in numerical order.
    (setf files
	  (sort files #'<
		:key #'(lambda (p)
			 (let ((name (pathname-name p)))
			   (cond ((string-equal name "maxima_toc")
				  ;; maxima_toc.html is first
				  -1)
				 ((string-equal name "maxima")
				  ;; maxima.html is second.
				  0)
				 (t
				  ;; Everything else is the number
				  ;; in the file name, which starts
				  ;; with 1.
				  (if (> (length name) 7)
				      (parse-integer (subseq name 7))
				      0)))))))
    ;; Check if the last 2 files contain the indices.  If they do,
    ;; we can ignore them.  We do want to ignore these, but if we're
    ;; wrong, it's not terrible.  We just waste time scanning files
    ;; that won't have anything to include in the index.
    (format t "Looking for indices~%")
    (let ((index-file
	    (dolist (file (last files 2))
	      (when (grep-l "<title>(Function and Variable Index)" file)
		(format t "BUILD-HTML-INDEX:  Function index: ~S.~%"
			(namestring file))
		(return file)))))

      (with-open-file (*log-file* "build-html-index.log" :direction :output :if-exists :supersede)

	(process-one-html-file index-file #'match-entries)
	(handle-special-cases)
	(process-toc "maxima_toc.html" #'match-toc)
	(format t "html-section-index len: ~D~%" (hash-table-count *html-section-index*))
	(maphash #'(lambda (k v)
		     (when (gethash k *html-index*)
		       (warning "TOC entry ~S already exists in html index with value ~S~%"
				k (gethash *html-index*)))
		     (setf (gethash k *html-index*) v))
		 *html-section-index*)
	(format t "Intro: ~S~%"
		(gethash "Introduction to Simplification" *html-index*))))))

(defmfun $build_and_dump_html_index (dir)
  (build-html-index dir)
  (let (entries)
    (maphash #'(lambda (k v)
		 (push (list k (namestring (car v)) (cdr v)) entries))
	     *html-index*)
    (format t "Intro: ~S~%"
		(gethash "Introduction to Simplification" *html-index*))
    (format t "HTML index has ~D entries~%" (hash-table-count *html-index*))
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
