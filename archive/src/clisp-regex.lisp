(eval-when (:compile-toplevel :load-toplevel :execute)
(defpackage "SI"
  (:use "COMMON-LISP")
  (:export
   "STRING-MATCH"
   "MATCH-END"
   "MATCH-BEGINNING"
   "*MATCH-DATA*"
   "*CASE-FOLD-SEARCH*"))
)

(in-package "SI")
(defvar *match-data*)
(defvar *case-fold-search* nil)

#+nil
(defun string-match (pattern string
			     &optional (start 0) end)
  "Search the string STRING for the first pattern that matches the
regexp PATTERN.  The syntax used for the pattern is specified by
SYNTAX.  The search may start in the string at START and ends at END,
which default to 0 and the end of the string.

If there is a match, returns the index of the start of the match and
an array of match-data.  If there is no match, -1 is returned and
nil."
  
  (let ((result
	 (multiple-value-list
	   #+case-fold-search
	   (regexp:match pattern string :start start :end end 
	       :case-insensitive *case-fold-search*)
	   #+case-fold-search-not
	   (regexp:match pattern string :start start :end end 
	       :case-sensitive (not *case-fold-search*))
	       )))
    (setf *match-data* result)
    (if (first result)
	(regexp:match-start (first result))
	-1)))

(defun string-match (pattern string
			     &optional (start 0) end)
  "Search the string STRING for the first pattern that matches the
regexp PATTERN.  The syntax used for the pattern is specified by
SYNTAX.  The search may start in the string at START and ends at END,
which default to 0 and the end of the string.

If there is a match, returns the index of the start of the match and
an array of match-data.  If there is no match, -1 is returned and
nil."
  
  (let* ((compiled-pattern (regexp:regexp-compile pattern
			      #+case-fold-search  *case-fold-search*
			      #+case-fold-search-not (not  *case-fold-search*)
			      ))
	 (result
	  (multiple-value-list
	      (regexp:regexp-exec compiled-pattern string
				  :start start :end end))))
    (setf *match-data* result)
    (if (first result)
	(regexp:match-start (first result))
	-1)))

(defun match-beginning (index &optional (match-data *match-data*))
  (if (and match-data (< index (length match-data)))
      (regexp:match-start (elt match-data index))
      -1))

(defun match-end (index &optional (match-data *match-data*))
  (if (and match-data (< index (length match-data)))
      (regexp:match-end (elt match-data index))
      -1))

