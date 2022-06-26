(in-package #:maxima)

(defvar *html-index*
  (make-hash-table :test #'equalp)
  "Hash table for looking up which html file contains the
  documentation.  The key is the topic we're looking for and the value
  is the html file containing the documentation for the topic.")

;; Run this build a hash table from the topic to the HTML file
;; containing the documentation.  The single argument DIR should be a
;; directory that contains the html files to be searched for the
;; topics.  For exapmle it can be "<maxima-dir>/doc/info/*.html"
(defun build-html-index (dir)
  (setf *html-index* (make-hash-table :test #'equalp))
  (let ((regexp (pregexp:pregexp "<dt id=\"index-([^\"]+)\"")))
    (dolist (file (directory dir))
      ;; We want to ignore maxima_singlepage.html for now.
      (unless (string-equal (pathname-name file)
                            "maxima_singlepage")
        (format *debug-io*  "Processing = ~S~%" file)
        (let ((base-name (make-pathname :name (pathname-name file)
                                        :type (pathname-type file))))
          (with-open-file (s file :direction :input)
            (loop for line = (read-line s nil)
                  while line
                  for match = (pregexp:pregexp-match-positions regexp line)
                  when match
                    do
                       (let ((item (subseq line
                                           (car (elt match 1))
                                           (cdr (elt match 1)))))
			 ;; Remove "005f" which texinfo adds before every "_".
                         (setf item
                               (pregexp:pregexp-replace* "005f" item ""))
			 ;; Replace "_0025" which texinfo uses to represent "%".
			 (setf item
			       (pregexp:pregexp-replace* "_0025" item "%"))
                         #+nil
                         (format t "match = ~S ~A~%" match item)
                         (setf (gethash item
                                        *html-index*)
                               base-name)))))))))

(defun build-and-dump-html-index (dir)
  (build-html-index dir)
  (let (entries)
    (maphash #'(lambda (k v)
		 (push (list k (namestring v)) entries))
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
