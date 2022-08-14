(in-package #:maxima)

(defvar *html-index*
  (make-hash-table :test #'equal)
  "Hash table for looking up which html file contains the
  documentation.  The key is the topic we're looking for and the value
  is the html file containing the documentation for the topic.")


;; This might be rather slow.  Perhaps an alternative solution is to
;; leave these alone and have $hdescribe encode any special characters
;; before looking them up.  Since ? only used occasionally, we don't
;; incur the cost here and move it to ? where the impact is lower.
;;
;; However, a test run where this function was removed made virtually
;; no difference in runtime (with cmucl).  (31.97 sec with and 31.62
;; sec without; well within timing noise probably.)  Note, however,
;; that this file is not normally compiled before running, but earlier
;; tests showed that compiling didn't make much difference either.  I
;; think this is because most of the cost is in pregexp, which is
;; compiled.
(defun handle-special-chars (item)
  "Handle special encoded characters in HTML file.  Texinfo encodes
  special characters to hexadecimal form and this needs to be undone
  so we know what the actual character is when looking up the
  documentation."
  ;; This is probably not the best way to do this.  Regexp searches
  ;; are probably pretty expensive.
  (dolist (spec-char '(#\% #\$ #\? #\. #\< #\> #\#
		       #\= #\: #\* #\- #\\ #\^ #\+ #\/ #\'
		       #\( #\)))
    (let ((code (string-downcase
		 (format nil "_~4,'0x" (char-code spec-char)))))
      (setf item
	    (pregexp:pregexp-replace* code item (string spec-char)))))
  item)

(defun process-one-html-file (file entry-regexp section-regexp fnindex-regexp)
  "Process one html file to find all the documentation entries.
  ENTRY-REGEXP is the regexp to use for find function and variable
  items.  SECTION-REGEXP is the regexp to find sections to include."
  (format *debug-io*  "Processing: ~S~%" file)
  (let ((base-name (make-pathname :name (pathname-name file)
                                  :type (pathname-type file))))
    (flet ((add-entry (item item-id file line)
	     ;; Add entry to the hash table.
	     ;;
	     ;; Replace any special chars that texinfo has encoded.
	     (setf item (handle-special-chars item))

	     ;; Check if the entry already exists and print a message.
	     ;; Presumably, this shouldn't happen, so warn if it does.
	     (when (gethash item *html-index*)
	       (format t "Already added entry ~S ~S: ~S~%"
				item (gethash item *html-index*)
				line))
	     (setf (gethash item *html-index*)
		   (cons file item-id))))
	     
      (with-open-file (s file :direction :input)
	(loop for line = (read-line s nil)
              while line
	      do
		 (let (match)
		   (cond
		     ((setf match (pregexp:pregexp-match-positions entry-regexp line))
		      (let ((item-id (subseq line
					     (car (elt match 1))
					     (cdr (elt match 1))))
			    item)
			;; Remove "005f" which texinfo adds before every "_".
			#+nil
			(format t "item-id = ~A~%" item-id)
			(setf item
			      (pregexp:pregexp-replace* "005f" item-id ""))
			#+nil
			(format t "match = ~S ~A~%" match item)
			(add-entry item item-id base-name line)))
		     ((setf match (pregexp:pregexp-match-positions section-regexp line))
		      (let ((item-id (subseq line
					     (car (elt match 1))
					     (cdr (elt match 1))))
			    (item (subseq line
					  (car (elt match 2))
					  (cdr (elt match 2)))))
			#+nil
			(format t "section item = ~A~%" item)
			(add-entry item item-id base-name line)))
		     ((setf match (pregexp:pregexp-match-positions fnindex-regexp line))
		      (let* ((item-id (subseq line
					      (car (elt match 1))
					      (cdr (elt match 1))))
			     (item (pregexp::pregexp-replace* "-" item-id " ")))
			;; However if the item ends in digits, we
			;; replaced too many "-" with spaces.  So if
			;; it ends with a space followed by digits, we
			;; need to replace the space with "-" again.
			(setf item (pregexp::pregexp-replace* " \(\\d+\)$" item "-\\1"))
			(setf item (handle-special-chars item))
			(add-entry item item-id base-name line))))))))))

;; Run this build a hash table from the topic to the HTML file
;; containing the documentation.  The single argument DIR should be a
;; directory that contains the html files to be searched for the
;; topics.  For exapmle it can be "<maxima-dir>/doc/info/*.html"
(defun build-html-index (dir)
  (clrhash *html-index*)
  ;; entry-regexp searches for entries for functions and variables.
  ;; We're looking for something like
  ;;
  ;;   <dt id="index-<foo>"
  ;;
  ;; and extracting "foo".
  ;;
  ;; section-regexp searches for section headings so we can get to
  ;; things like "Functions and Variables for...".  We're looking for
  ;;
  ;;   <span id="<id>">...<h3 class="section">12.2 <heading><
  ;;
  ;; where <heading> is the heading we want, and <id> is the id we can
  ;; use to link to this item.
  ;;
  ;; fnindex-regexp searches for id's that are associated with
  ;; @fnindex.  These look like
  ;;
  ;;   <span id="index-<id>"></span>
  ;;
  ;; all on one line.  The <id> is is the id we can use to link to
  ;; this item.
  (let ((entry-regexp (pregexp:pregexp "<dt id=\"index-([^\"]+)\""))
	(section-regexp
	  (pregexp:pregexp "<span id=\"\([^\"]+\)\">.*<h3 class=\"section\">[0-9.,]+ *\(.*\)<"))
	(fnindex-regexp
	  (pregexp:pregexp "<span id=\"index-\([^\"]+\)\"></span>$")))
    ;; Get a list of the files in the directory.  Remove all the ones
    ;; that don't start with "maxima".  Then sort them all in
    ;; numerical order.
    (let ((files (directory dir)))
      ;; First, look at any files in html-exceptions.txt and remove them.
      (with-open-file (s "html-exceptions.txt" :direction :input :if-does-not-exist nil)
	(when s
	  (loop for filename = (read-line s nil nil)
		while filename
		do
		   (format t "Skipping ~S~%" filename)
		   (setf files (remove (pathname-name filename) files
				       :test #'equal
				       :key #'pathname-name)))))
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
      (dolist (file files)
	;; We want to ignore maxima_singlepage.html for now.
	(unless (string-equal (pathname-name file)
                              "maxima_singlepage")
	  (process-one-html-file file entry-regexp section-regexp fnindex-regexp))))))

(defun build-and-dump-html-index (dir)
  (build-html-index dir)
  (let (entries)
    (maphash #'(lambda (k v)
		 (push (list k (namestring (car v)) (cdr v)) entries))
	     *html-index*)
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

(build-and-dump-html-index "./*.html")
