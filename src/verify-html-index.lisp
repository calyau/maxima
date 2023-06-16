;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Verification of HTML index entries.
;;;
;;; Copyright (C) 2023 Raymond Toy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)


;;;; Verification of the HTML index.  Use command-line option
;;;; --verify-html-index to perform this verification.

(defvar *text-topics*
  (make-hash-table :test #'equal)
  "All available text topics")

(defvar *html-topics* nil
  "All available html topics")

(defvar *extra-html-entries* nil
  "Full list of all the HTML entries that are not in the text entries.
  Ideally should be empty.")

(defvar *missing-html-entries* nil
  "Full list of all the text entries that are not the the HTML entries.
  Ideally should be empty.")

;; NOTE: This function should only be called by build-html-index.lisp
;; to build the html index.  build-html-index.lisp isn't compiled and
;; this function might be slow without being compiled so leave it here
;; so that it's compiled.
;;
;; This might be rather slow.  Perhaps an alternative solution is to
;; leave these alone and have $hdescribe encode any special characters
;; before looking them up.  Since "?" is only used occasionally, we
;; don't incur the cost here and move it to "?" where the impact is
;; lower.
;;
;; However, a test run where this function was removed made virtually
;; no difference in runtime (with cmucl).  (31.97 sec with and 31.62
;; sec without; well within timing noise probably.)  Note, however,
;; that this file is not normally compiled before running, but earlier
;; tests showed that compiling didn't make much difference either.  I
;; think this is because most of the cost is in pregexp, which is
;; compiled.

;; spec-chars-string are the special characters that texinfo converted
;; to lower-case hex digits representing the char-code of the
;; character.
(let* ((spec-chars-string "_.%$?,<>#=:;*-^+/'()[]!@|`~\\")
       (regexp-quoted (map 'list
			   #'(lambda (c)
			       ;; Bug in pregexp?  If we pregexp-quote
			       ;; "$" (to "\\$"), when we try to
			       ;; replace the match with "\\$", we end
			       ;; up with an empty string.  So don't
			       ;; quote this character.
			       (if (char= c #\$)
				   (string c)
				   (pregexp:pregexp-quote (string c))))
			   spec-chars-string))
       (codes (map 'list #'(lambda (spec-char)
			     (pregexp:pregexp-quote
			      (string-downcase
			       (format nil "_~4,'0x" (char-code spec-char)))))
		   spec-chars-string)))
  (defun handle-special-chars (item)
    "Handle special encoded characters in HTML file.  Texinfo encodes
    special characters to hexadecimal form and this needs to be undone
    so we know what the actual character is when looking up the
    documentation."
    (loop for code in codes
	  and replacement in regexp-quoted
	  ;; Exit early if there are not "_" characters left in the topic
	  while (find #\_ item :test #'char=)
	  do (setf item
		     (pregexp:pregexp-replace* code item
					       replacement)))
    item))

(defun get-html-topics ()
  ;; Find all the HTML entries and place in a list.
  #+nil
  (format t "Get html topics: table size ~D~%" (hash-table-count cl-info::*html-index*))

  (setf *html-topics*
	(loop for topic being the hash-keys of cl-info::*html-index*
	      collect topic))
  #+nil
  (format t "html topic length ~D~%" (length *html-topics*)))

(defun get-text-topics ()
  ;; Find all the text entries and place in a list.
  (clrhash *text-topics*)
  (maphash #'(lambda (k v)
	       (declare (ignore k))
	       (dolist (table v)
		 (loop for topic being the hash-keys of table
		       ;; We don't care what the value is.
		       do (setf (gethash topic *text-topics*) t))))
	   cl-info::*info-tables*))

(defun verify-html-index ()
  ;; Make sure the hash table has the correct test!  This is important.
  (unless (eql (hash-table-test cl-info::*html-index*) #-clisp 'equal
						       #+clisp 'ext:fasthash-equal)
    (mwarning (format nil
		      (intl:gettext "HTML index hash table test must be `equal not ~S~%")
		      (hash-table-test cl-info::*html-index*))))
  (get-html-topics)
  (get-text-topics)
  
  ;; The text entries are the source of truth about documentation.
  ;; Print out differences between the html entries and the text
  ;; entries.
  (unless (= (hash-table-count cl-info::*html-index*)
	     (hash-table-count *text-topics*))
    (mwarning
     (format nil
	     (intl:gettext "Number of HTML entries (~A) does not match text entries (~A)~%")
	     (hash-table-count cl-info::*html-index*)
	     (hash-table-count *text-topics*))))
		      
  ;; If the set of topics differs between HTML and text, print out
  ;; the differences.
  (setf *extra-html-entries*
	(loop for key being the hash-keys of cl-info::*html-index*
	      unless (gethash key *text-topics*)
		collect key))

  (setf *missing-html-entries*
	(loop for key being the hash-keys of *text-topics*
	      unless (gethash key cl-info::*html-index*)
		collect key))

  (flet
      ((maybe-print-warning (prefix-msg diffs)
	 (let ((max-display-length 20))
	   (when diffs
	     (let* ((diff-length (length diffs))
		    (displayed-length (min diff-length max-display-length))
		    (message
		      (with-output-to-string (s)
			(format s
				"~D ~A:~% ~{~S~^ ~}"
				(length diffs)
				prefix-msg
				(subseq diffs 0 displayed-length))
			(when (> diff-length max-display-length)
			  (format s "... plus ~D more" (- diff-length max-display-length)))
			(format s "~%"))))
	       (mwarning message))))))
    (maybe-print-warning (intl:gettext "HTML entries not in text entries")
			 *extra-html-entries*)
    (maybe-print-warning (intl:gettext "Text entries not in HTML entries")
			 *missing-html-entries*))
  ;; Return true if there a no extra or missing entries.
  (not (or *extra-html-entries* *missing-html-entries*)))

;; Undocumented Maxima function to verify the HTML index so it can be
;; easily run from the REPL.  Mostly for debugging.  The optional arg
;; TIMEP will call TIME to show how long it took to run the
;; verification.  This is also for debugging in case the time seems to
;; have gotten too long on startup.
(defmfun $verify_html_index (&optional timep)
  (if timep
      (time (verify-html-index))
      (verify-html-index)))
