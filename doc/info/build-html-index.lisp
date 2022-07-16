(in-package #:maxima)

(defvar *html-index*
  (make-hash-table :test #'equalp)
  "Hash table for looking up which html file contains the
  documentation.  The key is the topic we're looking for and the value
  is the html file containing the documentation for the topic.")


;; This might be rather slow.  Perhaps an alternative solution is to
;; leave these alone and have $hdescribe encode any special characters
;; before looking them up.  Since ? only used occasionally, we don't
;; incur the cost here and move it to ? where the impact is lower.
(defun handle-special-chars (item)
  "Handle special encoded characters in HTML file.  Texinfo encodes
  special characters to hexadecimal form and this needs to be undone
  so we know what the actual character is when looking up the
  documentation."
  ;; This is probably not the best way to do this.  Regexp searches
  ;; are probably pretty expensive.
  (dolist (spec-char '(#\% #\$ #\? #\. #\< #\> #\#
		       #\= #\: #\* #\- #\\ #\^ #\+ #\/ #\'))
    (let ((code (string-downcase
		 (format nil "_~4,'0x" (char-code spec-char)))))
      (setf item
	    (pregexp:pregexp-replace* code item (string spec-char)))))
  item)

(defun process-one-html-file (file entry-regexp section-regexp)
  "Process one html file to find all the documentation entries.
  ENTRY-REGEXP is the regexp to use for find function and variable
  items.  SECTION-REGEXP is the regexp to find sections to include."
  (format *debug-io*  "Processing: ~S~%" file)
  (let ((base-name (make-pathname :name (pathname-name file)
                                  :type (pathname-type file))))
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
		      (setf item (handle-special-chars item))
                      #+nil
                      (format t "match = ~S ~A~%" match item)
                      (setf (gethash item
                                     *html-index*)
			    (cons base-name item-id))))
		   ((setf match (pregexp:pregexp-match-positions section-regexp line))
		    (let ((item-id (subseq line
					   (car (elt match 1))
					   (cdr (elt match 1))))
			  (item (subseq line
					(car (elt match 2))
					(cdr (elt match 2)))))
		      #+nil
		      (format t "section item = ~A~%" item)
		      (setf (gethash item *html-index*)
			    (cons base-name item-id))))))))))

;; Run this build a hash table from the topic to the HTML file
;; containing the documentation.  The single argument DIR should be a
;; directory that contains the html files to be searched for the
;; topics.  For exapmle it can be "<maxima-dir>/doc/info/*.html"
(defun build-html-index (dir)
  (setf *html-index* (make-hash-table :test #'equalp))
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
  (let ((entry-regexp (pregexp:pregexp "<dt id=\"index-([^\"]+)\""))
	(section-regexp
	  (pregexp:pregexp "<span id=\"\([^\"]+\)\">.*<h3 class=\"section\">[0-9.]+ *\(.*\)<")))
    (dolist (file (directory dir))
      ;; We want to ignore maxima_singlepage.html for now.
      (unless (string-equal (pathname-name file)
                        "maxima_singlepage")
	(process-one-html-file file entry-regexp section-regexp)))))

(defun build-and-dump-html-index (dir)
  (build-html-index dir)
  (let (entries)
    (maphash #'(lambda (k v)
		 (push (list k (namestring (car v)) (cdr v)) entries))
	     *html-index*)
    (with-open-file (s "maxima-index-html.lisp" :direction :output)
      (with-standard-io-syntax
	(let ((*package* (find-package :cl-info))
	      (*print-length* nil)
	      (*print-case* :downcase))
	  (format s ";;; Do not edit; automatically generated via build-html-index.lisp~2%")
	  (pprint '(in-package :cl-info)
		  s)

	  (pprint `(let ((cl-info::html-index ',entries))
		     (cl-info::load-html-index cl-info::html-index))
		  s))))))

(build-and-dump-html-index "./*.html")
