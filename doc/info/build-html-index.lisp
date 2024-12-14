;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Build an index of all the relevant links in the HTML version of
;;; the manual so that it can be used via help instead of the text
;;; version.
;;;
;;; Copyright (C) 2023 Raymond Toy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun texinfo-version-number (major minor &optional (patch 0))
  "Convert the major, minor, and patch to an integer."
  (+ (* 10000 major)
     (* 100 minor)
     patch))

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

  (when (find #\- item)
    (when replace-dash-p
      ;; Replace "-" with a space (if requested).  The HTML output
      ;; puts "-" where spaces were originally in the texi name.
      ;; But this isn't perfect.  See HANDLE-SPECIAL-CASES where we
      ;; shouldn't have done this.
      (setf item (pregexp:pregexp-replace* "-([^[:digit:]])" item " \\1")))
      
    ;; Now replace things like "ztics-1" with "ztics <1>".  The
    ;; former is the HTML index item for the info item with the
    ;; latter name.
    (setf item (pregexp:pregexp-replace "-([[:digit:]])" item " <\\1>")))

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

(defun process-line (line matcher path &key replace-dash-p (prefix "Add:") truenamep)
  "Process the LINE using the function MATCHER to determine if this line
  contains something interesting to add to the index. REPLACE-DASH-P
  and PREFIX are passed to ADD-ENTRY.  If TRUENAMEP is non-NIL, the
  entry is the full path to the file specified in the line based on
  the value of PATH."
  (multiple-value-bind (item item-id file line)
      (funcall matcher line)
    (when item
      #+nil
      (format t "process-line: file, path = ~A ~A~%" file path)
      (when truenamep
        (setf file (truename (merge-pathnames file path))))
      (add-entry item item-id
                 file
                 line
		 :replace-dash-p replace-dash-p
		 :prefix prefix))))

(defun process-one-html-file (file matcher replace-dash-p prefix truenamep)
  "Process one html file named FILE using MATCHER to determine matches.
  REPLACE-DASH-P and PREFIX are passed to PROCESS-LINE which will
  handle these."
  (format *debug-io*  "build-html-index: processing: ~S~%" file)
  (with-open-file (s file :direction :input)
    (loop for line = (read-line s nil)
          while line
	  do
	     (process-line line matcher
                           file
			   :replace-dash-p replace-dash-p
			   :prefix prefix
                           :truenamep truenamep))))

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
		      "Euler-Mascheroni constant")
                     ("maxima init.lisp" "maxima-init.lisp")
                     ("maxima init.mac" "maxima-init.mac")))
      (destructuring-bind (old new)
	  items
	(update-entry old new)))

    ;; This is messy.  Texinfo 6.8 uses plain apostrophes in the info
    ;; file.  But with texinfo 7.0.[23], some entries in HTML use an
    ;; apostrophe (U+27) character, but the info file uses
    ;; Right_Single_Quotation_Mark (U+2019).  And apparently, the
    ;; Texinfo 7.1 will not.
    ;;
    ;; Convert these only for the cases we know this is a problem.
    ;;
    ;; This current implementation will very likely not work with gcl
    ;; only supports 8-bit characters.
    (when *texinfo-version*
      (cond ((>= *texinfo-version* (texinfo-version-number 7 1))
             ;; Don't need anything special for texinfo 7.1 and greater.
             t)
            ((>= *texinfo-version* (texinfo-version-number 7 0 2))
             (dolist (item '("Catalan's Constant"
			     "Euler's number"
		             "Introduction to Maxima's Database"))
	       (update-entry item
		             (pregexp::pregexp-replace* "'" item (string (code-char #x2019))))))))))

;; Find entries from the function and variable index.  An example of
;; what we're looking for:
;;
;;   <a href="Elementary-Functions.html#index-asinh">
;;
;; We extract the file name, "Elementary-Functions.html", and the
;; stuff starting with "#index-".  This gives the item-id,
;; "index-asinh" that we want to use for the link (without the "#"),
;; and the stuff after "#index-", "asinh" is the id topic we need.
(let ((href (pregexp:pregexp "<a href=\"([[:alnum:]_-]+\.html)#(index-([^\"]*))\">")))
  (defun match-entries (line)
    (let ((match (pregexp:pregexp-match href line)))
      (when match
	(destructuring-bind (whole file item-id id)
	    match
	  (declare (ignore whole))
	  (values id item-id file line))))))

;; Find entries from the TOC.  An example of what we're looking for:
;;
;;  <a id="toc-Bessel-Functions-1" href="Special-Functions.html#Bessel-Functions">15.2 Bessel Functions</a></li>
;;
;; We extract the file name, "Special-Functions.html" and the id,
;; "Bessel-Functions" and then the title of the subsection, without
;; the section numbers, "Bessel Functions".  
;; Further subsections are ignored.
;; Conditional expression allows us to handle earlier versions of Texinfo.
(defun match-toc (line)
  (let ((regexp (cond 
          ((>= *texinfo-version* (texinfo-version-number 6 8 )) (pregexp:pregexp "<a id=\"toc-.*\" href=\"([^#\"]+)(#([^\"]+))\">[[:digit:]]+\.[[:digit:]]+ ([^\"]+?)</a>"))
          (t (pregexp:pregexp "<a (?:name|id)=\"toc-.*\" href=\"([^#\"]+)(#([^\"]+))\">[[:digit:]]+\\.[[:digit:]]+ ([^\"]+?)</a>(?!.*maxima_100.html)")))
       ))
    (let ((match (pregexp:pregexp-match regexp line)))
      (when match
	(destructuring-bind (whole file item# id item)
	    match
	  (declare (ignore whole item#))
	  (when (find #\& item :test #'char=)
	    ;; Replace HTML entities with the corresponding char.
	    ;; See, for example,
	    ;; https://lilith.fisica.ufmg.br/~wag/TRANSF/codehtml.html
	    ;;
	    ;; Perhaps we should use the language to reduce the number
	    ;; of calls to pregexp?
	    (dolist (replacement '(("&rsquo;" #x27) ; Right single-quote -> apostrophe
				   ;; From the de manual
				   ("&uuml;" 252)
				   ("&Uuml;" 220)
				   ("&auml;" 228)
				   ;; From the pt manual
				   ("&ccedil;" 231)
				   ("&atilde;" 227)
				   ("&oacute;" 243)
				   ;; From the pt_BR manual
				   ("&aacute;" 225)
				   ("&ouml;" 246)
				   ))
	      (destructuring-bind (html-entity char-code)
		  replacement
		(setf item (pregexp:pregexp-replace* html-entity
						     item
						     (string (code-char char-code)))))))

	  (format *log-file* "TOC: ~S -> ~S~%" item file)

	  (values item id file line))))))

(defparameter *index-file-name*
  (make-hash-table :test 'equal)
  "Hash table whose key is the lang and whose value is a list of the
  file name of function and variable index and the title of the
  index.")

;; Setup the hashtable.  The default (English) is the empty string.
;; Each entry should be a list consisting of the language id, the name
;; of the html file containing the function and variable index and
;; finally the title of section, ignoring any section numbers/names.
(dolist (entry 
	 '(("" "Function-and-Variable-Index.html" "Function and Variable Index")
	   ("de" "Index-der-Variablen-und-Funktionen.html" "Index der Variablen und Funktionen")
	   ("pt" "Indice-de-Funcoes-e-Variaveis.html" "Índice de Funções e Variáveis")
	   ("es" "Indice-de-Funciones-y-Variables.html" "Índice de Funciones y Variables")
	   ("pt_BR" "Indice-de-Funcoes-e-Variaveis.html" "Índice de Funções e Variáveis")
           ("ja" "Function-and-Variable-Index.html" "Function and Variable Index")
           ("ru" "Ukazatelx-funkcii-i-peremennykh.html" "Указатель функций и переменных")))
  (destructuring-bind (key &rest value)
      entry
    (setf (gethash key *index-file-name*) value)))

(defun get-index-file-name (lang)
  (first (gethash lang *index-file-name*)))

(defun get-index-title (lang)
  (second (gethash lang *index-file-name*)))

(defun find-index-file (dir lang)
  "Find the name of HTML file containing the function and variable
  index."
  (unless (gethash lang *index-file-name*)
    (merror "Unknown or unsupported language for HTML help: ~A" lang))
  (let ((f-a-v-i (merge-pathnames (get-index-file-name lang)
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
    (let* ((title (get-index-title lang))
	   (search-item (format nil "<title>.*~A" title)))
      (format t "Looking for function and variable index: ~A~%" title)
      (dolist (file (last files 2))
	(when (grep-l search-item file)
	  (format t "Function index: ~S.~%"
		  (namestring file))
	  (return-from find-index-file file))))))

;; Parse the texinfo version string.  It should look something like
;; "M.m.p", where M and m are a sequence of (base 10) digits and ".p"
;; is the optional patch version.
(defun parse-texinfo-version (string)
  (when string
    (let ((posn 0)
	  (len (length string))
	  version)
      (dotimes (k 3)
	(when (<= posn len)
	  (multiple-value-bind (digits end)
	      (parse-integer string
			     :start posn
			     :junk-allowed t)
	    (push digits version)
	    (setf posn (1+ end)))))
      (apply #'texinfo-version-number (nreverse version)))))

(defun get-texinfo-version (file)
  "Get the texinfo version from FILE"
  (let ((version-line
	  (with-open-file (f file :direction :input)
	    ;; Texinfo writes a comment line containing the version number of
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
    (setf *texinfo-version*
	  (parse-texinfo-version *texinfo-version-string*))))

(defun find-toc-file (dir)
  ;; List of possible filenames that contain the table of contents.
  ;; The first is used by texinfo 6.8 and later.  The second is used
  ;; by texinfo 5.1.  Return the first one found.
  (dolist (toc '("index.html" "maxima_0.html"))
    (let ((toc-path (merge-pathnames toc dir)))
      (when (probe-file toc-path)
	(return-from find-toc-file toc-path)))))

(defun build-html-index (dir lang truenamep)
  (clrhash *html-index*)
  (let ((index-file (find-index-file dir lang)))
    (unless index-file
      (error "Could not find HTML file containing the function and variable index."))

    (with-open-file (*log-file* "build-html-index.log"
				:direction :output :if-exists :supersede)
      (let ((toc-path (find-toc-file dir)))
	(get-texinfo-version toc-path)
	(format t "Texinfo Version ~A: ~D~%" *texinfo-version-string* *texinfo-version*)
	(process-one-html-file index-file #'match-entries t "Add" truenamep)
	(process-one-html-file toc-path #'match-toc nil "TOC" truenamep)
	(handle-special-cases)))))

;; Run this to build a hash table from the topic to the HTML file
;; containing the documentation.  It is written to the file given by
;; OUTPUT_FILE.  The output can then subsequently be read back in to
;; update Maxima's database of available HTML documentation.  However,
;; fot this to work Maxima must have also have the updated index to
;; the info files for the documentation.
(defmfun $build_and_dump_html_index (dir &key
                                         (output_file "maxima-index-html.lisp")
                                         (lang "")
                                         (truenamep nil))
  "Creates a file that contains data that maxima can use to produce
 HTML documentation.  The parameters are:

   DIR
     Pathname to where the html files are.  This is usually a wildcard
     pathname of the form \"<path>/*.html\".
   :OUTPUT-FILE
     Specifies the name of the file where the data is written.
     Defaults to \"maxima-index-html.lisp\".
   :LANG
     Specifies the language to use.  Defaults to \"\" for English.
     This is used primarly when building Maxima's user manual to
     determine the name of file containing the function and variable
     index.
   :TRUENAMEP
     If non-NIL, the data will use the full pathname to the html
     files.  Defaults to NIL.  Otherwise, the data will be a relative
     path.  This MUST be set to NIL when building Maxima's user
     manual.
"
  (build-html-index dir lang truenamep)
  (let (entries)
    (maphash #'(lambda (k v)
		 (push (list k (namestring (car v)) (cdr v)) entries))
	     *html-index*)
    (with-open-file (s output_file
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
		  s))))
    (format t "HTML index has ~D entries~%" (hash-table-count *html-index*))
    ;; Hash table can't be empty unless we screwed up somewhere.
    (assert (plusp (hash-table-count *html-index*)))
    (plusp (hash-table-count *html-index*))))

;;(build-and-dump-html-index "./*.html")
