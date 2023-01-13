(in-package :cl-info)

(defvar *info-tables* (make-hash-table :test 'equal))

;; Gcl doesn't like equalp hashtables.
(defvar *html-index*
  (make-hash-table :test #'equalp)
  "Hash table for looking up which html file contains the
  documentation.  The key is the topic we're looking for and the value
  is a cons consisting of the html file and the id for the key.")


(defun print-prompt (prompt-count)
  (fresh-line)
  (maxima::format-prompt
   t "~a"
   (if (zerop prompt-count)
	      (intl:gettext "Enter space-separated numbers, `all' or `none': ")
	      (intl:gettext "Still waiting: "))))

(defvar +select-by-keyword-alist+
  '((noop "") (all "a" "al" "all") (none "n" "no" "non" "none")))

(defun parse-user-choice (nitems)
  (loop
   with line = (read-line #+(or sbcl cmu) *standard-input*) and nth and pos = 0
   while (multiple-value-setq (nth pos)
	   (parse-integer line :start pos :junk-allowed t))
   if (or (minusp nth) (>= nth nitems))
   do (format *debug-io* (intl:gettext "~&Discarding invalid number ~d.") nth)
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
       (format *debug-io* (intl:gettext "~&Ignoring trailing garbage in input.")))
     (return (cons keyword list)))))

(defun select-info-items (selection items)
  (case (pop selection)
    (noop (loop
	   for i in selection
	   collect (nth i items)))
    (all items)
    (none 'none)))

; ------------------------------------------------------------------
; STUFF ABOVE SALVAGED FROM PREVIOUS INCARNATION OF SRC/CL-INFO.LISP
; STUFF BELOW IS NEW, BASED ON LOOKUP TABLE BUILT AHEAD OF TIME
; ------------------------------------------------------------------

; ------------------ search help topics ------------------

(defun maxima::combine-path (&rest list)
  "splice a '/' between the path components given as arguments"
  (format nil "~{~A~^/~}" list))

(defun load-primary-index ()
  ;; Is with-standard-io-syntax too much for what we want?
  (let*
      ((subdir-bit (or maxima::*maxima-lang-subdir* "."))
       (path-to-index (maxima::combine-path maxima::*maxima-infodir* subdir-bit "maxima-index.lisp"))
       (path-to-html-index
	 (maxima::combine-path maxima::*maxima-infodir* subdir-bit "maxima-index-html.lisp")))
    ;; Set the default of the html URL base to be a file URL pointing
    ;; to the info dir.
    (setf maxima::$url_base (concatenate 'string
			     "file://"
			     (if maxima::*maxima-lang-subdir*
				 (maxima::combine-path maxima::*maxima-htmldir*
						       maxima::*maxima-lang-subdir*)
				 maxima::*maxima-htmldir*)))
			     
    (handler-case
      (with-standard-io-syntax (load path-to-index))
      (error (condition) (warn (intl:gettext (format nil "~&Maxima is unable to set up the help system.~&(Details: CL-INFO::LOAD-PRIMARY-INDEX: ~a)~&" condition)))))
    (handler-case
	(with-standard-io-syntax (load path-to-html-index))
      (error (condition) (warn (intl:gettext (format nil "~&Maxima is unable to set up the help system.~&(Details: CL-INFO::LOAD-PRIMARY-INDEX: ~a)~&" condition)))))))

  
(defun info-exact (x)
  (setq x (strip-quotes x))
  (let ((exact-matches (exact-topic-match x)))
    (if (not (some-exact x exact-matches))
      (progn
        (format t (intl:gettext "  No exact match found for topic `~a'.~%  Try `?? ~a' (inexact match) instead.~%~%") x x)
        nil)
      (progn
        (display-items exact-matches)
        (if (some-inexact x (inexact-topic-match x))
          (format t (intl:gettext "  There are also some inexact matches for `~a'.~%  Try `?? ~a' to see them.~%~%") x x))
        t))))

(defun some-exact (x matches)
  (some #'identity (flatten-matches x matches)))

(defun some-inexact (x matches)
  (some #'null (flatten-matches x matches)))

(defun flatten-matches (x matches)
  ;; OH GODS, SPARE YOUR SERVANT FROM YOUR FIERY WRATH ...
  (mapcar #'(lambda (y) (equal y x)) (mapcar #'first (apply #'append (mapcar #'second matches)))))

(defun exact-topic-match (topic)
  (setq topic (regex-sanitize topic))
  (loop for dir-name being the hash-keys of *info-tables*
    collect (list dir-name (exact-topic-match-1 topic dir-name))))

(defun exact-topic-match-1 (topic d)
  (let*
    ((section-table (first (gethash d *info-tables*)))
     (defn-table (second (gethash d *info-tables*)))
     (regex1 (concatenate 'string "^" topic "$"))
     (regex2 (concatenate 'string "^" topic " *<[0-9]+>$")))
    (append
      (find-regex-matches regex1 section-table)
      (find-regex-matches regex1 defn-table)
      (find-regex-matches regex2 section-table)
      (find-regex-matches regex2 defn-table))))

(defun info-inexact (x)
  (setq x (strip-quotes x))
  (let ((inexact-matches (inexact-topic-match x)))
    (when inexact-matches
      (display-items inexact-matches))
    (not (null inexact-matches))))

;; MATCHES looks like ((D1 (I11 I12 I12 ...)) (D2 (I21 I22 I23 ...)))
;; Rearrange it to ((D1 I11) (D1 I12) (D1 I13) ... (D2 I21) (D2 I22) (D2 I23) ...)
(defun rearrange-matches (matches)
  (apply #'append (mapcar #'(lambda (di) (let ((d (first di)) (i (second di))) (mapcar #'(lambda (i1) (list d i1)) i))) matches)))

(defun display-items (items)
  (let*
    ((items-list (rearrange-matches items))
     (nitems (length items-list))
     wanted)

    (loop for i from 0 for item in items-list do
      (when (> nitems 1)
        (let
          ((heading-title (nth 4 (second item)))
           (item-name (first (second item))))
          (format t "~% ~d: ~a~@[  (~a)~]" i item-name heading-title))))

    (setq wanted
          (if (> nitems 1)
            (prog1
              (loop
                for prompt-count from 0
                thereis (progn
                          (finish-output *debug-io*)
                          (print-prompt prompt-count)
                          (finish-output)
                          #-(or sbcl cmu) (clear-input)
                          (select-info-items
                            (parse-user-choice nitems) items-list)))
              #-(or sbcl cmu) (clear-input))
            items-list))
    (finish-output *debug-io*)
    (when (consp wanted)
      (format t "~%")
      (funcall maxima::*help-display-function* wanted)
      #+nil
      (cond
	(maxima::$describe_uses_html
	 (when maxima::*debug-display-html-help*
	   (format *debug-io* "wanted = ~A~%" wanted))
	 (loop for (dir entry) in wanted
	       do (maxima::display-html-help (car entry))))
	(t
	  (loop for item in wanted
		do (let ((doc (read-info-text (first item) (second item))))
		     (if doc
			 (format t "~A~%~%" doc)
			 (format t "Unable to find documentation for `~A'.~%~
                                Possible bug maxima-index.lisp or build_index.pl?~%"
				 (first (second item)))))))))))

(defun inexact-topic-match (topic)
  (setq topic (regex-sanitize topic))
  (let ((foo (loop for dir-name being the hash-keys of *info-tables*
    collect (list dir-name (inexact-topic-match-1 topic dir-name)))))
    (remove-if #'(lambda (x) (null (second x))) foo)))

(defun inexact-topic-match-1 (topic d)
  (let*
    ((section-table (first (gethash d *info-tables*)))
     (defn-table (second (gethash d *info-tables*))))
    (append
      (find-regex-matches topic section-table)
      (find-regex-matches topic defn-table))))

;; If S is enclosed in single quotes or double quotes,
;; return the quoted string.
(defun strip-quotes (s)
  (let ((n (length s)))
    (if (<= n 2) s ;; incidentally return "" or '' verbatim
      (let ((first-char (aref s 0)) (last-char (aref s (1- n))))
        (if (or (and (eql first-char #\') (eql last-char #\'))
                (and (eql first-char #\") (eql last-char #\")))
          (subseq s 1 (1- n))
          s)))))

(defun regex-sanitize (s)
  "Precede any regex special characters with a backslash."
  (pregexp:pregexp-quote s))

(defun find-regex-matches (regex-string hashtable)
  (let*
      ;; Do the search ignoring case by wrapping the regex-string in
      ;; "(?i:...)"
      ((regex (concatenate 'string "(?i:" regex-string ")"))
       (regex-matches nil))
    (maphash
     #'(lambda (key value)
         (when (pregexp:pregexp-match-positions regex key)
	   #+nil
	   (format t "key value: ~S ~S: match ~A~%"
		   key value (pregexp:pregexp-match-positiions regex key))
	   (setq regex-matches (cons `(,key . ,value) regex-matches))))
      hashtable)
    (stable-sort regex-matches #'string-lessp :key #'car)))

(defun read-info-text (dir-name parameters)
  (let*
    ((value (cdr parameters))
     (filename (car value))
     (byte-offset (cadr value))
     (char-count (caddr value))
     (text (make-string char-count))
     (path+filename (merge-pathnames (make-pathname :name filename) dir-name)))
    (handler-case
	(with-open-file (in path+filename :direction :input)
	  (unless (plusp byte-offset)
	    ;; If byte-offset isn't positive there must be some error in
	    ;; the index.  Return nil and let the caller deal with it.
	    (return-from read-info-text nil))
	  (file-position in byte-offset)
	  (read-sequence text in :start 0 :end char-count)
	  text)
      (error () (maxima::merror "Cannot find documentation for `~M': missing info file ~M~%"
				(car parameters) (namestring path+filename))))))

; --------------- build help topic indices ---------------

(defun load-info-hashtables (dir-name deffn-defvr-pairs section-pairs)
  (if (and (zerop (length section-pairs)) 
           (zerop (length deffn-defvr-pairs)))
    (format t (intl:gettext "warning: ignoring an empty documentation index in ~a~%") dir-name)
    (destructuring-bind
      (section-hashtable deffn-defvr-hashtable)
      (ensure-info-tables dir-name)
      (mapc #'(lambda (x) (setf (gethash (car x) section-hashtable) (cdr x))) section-pairs)
      (mapc #'(lambda (x) (setf (gethash (car x) deffn-defvr-hashtable) (cdr x))) deffn-defvr-pairs))))

(defun ensure-info-tables (dir-name)
  (or (gethash dir-name *info-tables*)
    (let
      ((t1 (make-hash-table :test 'equal))
       (t2 (make-hash-table :test 'equal)))
      (setf (gethash dir-name *info-tables*) (list t1 t2)))))

(defun load-html-index (entries)
  (dolist (entry entries)
    (destructuring-bind (item path id)
	entry
      (setf (gethash item *html-index*) (cons path id)))))

