;;;;
;;;;                              ~*~  STRINGPROC  ~*~
;;;;
;;;;  Maxima string processing package.
;;;;
;;;;  Version       : 1.2 (march 2006)
;;;;  Copyright     : 2005-2006 Volker van Nek
;;;;  Licence       : GPL2
;;;;
;;;;  Test file     : rteststringproc.mac
;;;;  Documentation : stringproc.texi
;;;;


;;  1. I/O
;;  2. characters
;;  3. strings

(in-package :maxima)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  1. I/O

(defun $openw (file)
   (open
      file
      :direction :output
      :if-exists :supersede
      :if-does-not-exist :create))

(defun $opena (file)
   (open
      file
      :direction :output
      :if-exists :append
      :if-does-not-exist :create))

(defun $openr (file) (open file))

(defun $make_string_input_stream (s &optional (start 0) (end nil))
  (unless (stringp s)
    (merror "make_string_input_stream: argument must be a string."))
  (make-string-input-stream s start end))

(defun $make_string_output_stream ()
  (make-string-output-stream))

;; Ignore :element-type keyword. 
;; So we get the default here, namely :element-type character.
(defun $get_output_stream_string (s)
  (if (not (streamp s))
    (merror "get_output_stream_string: argument must be a stream."))
  (get-output-stream-string s))

(defun $close (stream) (close stream))

(defun $flength (stream) (file-length stream))

(defun $fposition (stream &optional pos)
   (if pos
      (file-position stream (1- pos))
      (1+ (file-position stream))))


(defun $readline (stream)
   (let ((line (read-line stream nil nil)))
      (if line
	 line)))

(defun $freshline (&optional (stream)) (fresh-line stream))

(defun $newline (&optional (stream)) (terpri stream))


;;  $printf makes almost all features of CL-function format available
(defmacro $printf (stream mstring &rest args)
  (let (
	;; needs a fix for directives like ~20s
	(string (meval mstring))
	(bracket ($ssearch "~{" (meval mstring)))
	body)
    (dolist (arg args)
       (progn
	 (setq arg (meval arg))
	 (setq arg
	   (cond ((numberp arg) arg)
		 ((stringp arg) arg)
		 ((and (symbolp arg) (not (boundp arg)))
		    `(quote ,(maybe-invert-string-case (subseq (string arg) 1))))
		 ((and (listp arg) (listp (car arg)) (eq (caar arg) 'mlist))
		    (if bracket
		       `(quote ,(cltree arg))
		       (merror
			  "printf: For printing lists use ~M in the control string."
			  "\~\{ and \~\}")))
		 (t ($sconcat arg))))
	 (setq body (append body (list arg)))))
   (if stream
      `(format ,stream ,string ,@body)
      `(format ,stream ,string ,@body))))

;;  cltree converts a Maxima-tree into a CL-tree on lisp level
;;  helper-function for $printf
(defun cltree (mtree)
  (labels
    ((clt (todo done)
       (if (null todo)
	 (nreverse done)
	 (clt (cdr todo)
	      (cons (let ((x (car todo)))
		      (if (and (listp x) (listp (car x)) (eq (caar x) 'mlist))
			(cltree x)
			(mhandle x)))
		    done))))
     (mhandle (obj)
       (progn
	  (setq obj (meval obj))
	  (cond ((numberp obj) obj)
		((stringp obj) obj)
		(t (if (and (symbolp obj) (not (boundp obj)))
		      (maybe-invert-string-case (subseq (string obj) 1))
		      ($sconcat obj)))))))
   (clt (cdr mtree) nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  2. characters

;;  converts a maxima-string of length 1 into a lisp-character
(defun $lchar (mch) (l-char mch));; for testing only

(defun l-char (mch)
  (let ((smch mch))
    (if (= (length smch) 1)
      (character smch)
      (merror
	"stringproc: ~:M cannot be converted into a character."
	  mch))))

;;  converts a lisp-character into a maxima-string of length 1
(defun $cunlisp (lch) (m-char lch));; for testing only

(defun m-char (lch)
  (make-string 1 :initial-element lch))

;;  tests, if object is lisp-character
(defun $lcharp (obj) (characterp obj));; for testing only

;;  tests, if object is maxima-character
(defun $charp (obj)
   (and (stringp obj) (= 1 (length obj))))

;;  tests for different maxima-characters
(defun $constituent (mch)   (constituent (l-char mch)))
(defun $alphanumericp (mch) (alphanumericp (l-char mch)))
(defun $alphacharp (mch)    (alpha-char-p (l-char mch)))
(defun $lowercasep (mch)    (lower-case-p (l-char mch)))
(defun $uppercasep (mch)    (upper-case-p (l-char mch)))
(defun $digitcharp (mch)
   (let ((nr (char-int (l-char mch))))
      (and (> nr 47) (< nr 58))))

;;  ascii-char <-> index
(defun $cint (mch) (char-code (l-char mch)))
(defun $ascii (int) (m-char (code-char int)))

;;  comparison - test functions - at Maxima level
(defun $cequal (ch1 ch2)          (char= (l-char ch1) (l-char ch2)))
(defun $cequalignore (ch1 ch2)    (char-equal (l-char ch1) (l-char ch2)))
(defun $clessp (ch1 ch2)          (char< (l-char ch1) (l-char ch2)))
(defun $clesspignore (ch1 ch2)    (char-lessp (l-char ch1) (l-char ch2)))
(defun $cgreaterp (ch1 ch2)       (char> (l-char ch1) (l-char ch2)))
(defun $cgreaterpignore (ch1 ch2) (char-greaterp (l-char ch1) (l-char ch2)))

;;  comparison - test functions - at Lisp level
(defun cequal (ch1 ch2)          (char= ch1 ch2))
(defun cequalignore (ch1 ch2)    (char-equal ch1 ch2))
(defun clessp (ch1 ch2)          (char< ch1 ch2))
(defun clesspignore (ch1 ch2)    (char-lessp ch1 ch2))
(defun cgreaterp (ch1 ch2)       (char> ch1 ch2))
(defun cgreaterpignore (ch1 ch2) (char-greaterp ch1 ch2))

#|
 $newline    (definitions placed beneath string functions)
 $tab
 $space
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  3. strings

;;  tests, if object is a string
(defun $stringp (obj) (stringp obj))


;;  copy
(defun $scopy (mstr) (copy-seq mstr))

;;  make
(defun $smake (n mch)
  (make-string n :initial-element (l-char mch)))


;;  returns a maxima-string of length 1
(defun $charat (mstr index) ;; 1-indexed!
  (subseq mstr (1- index) index))

(defun $charlist (s)
  (cons '(mlist) (mapcar #'(lambda (c) (m-char c)) (coerce s 'list))))

(putprop '$sexplode '$charlist 'alias)


;;  $tokens implements Paul Grahams function tokens in Maxima
(defun $tokens (mstr &optional (test '$constituent))
  (cons '(mlist) (tokens mstr (stripdollar test) 0)))

(defun tokens (str test start) ;; Author: Paul Graham - ANSI Common Lisp, 1996, page 67
  (let ((p1 (position-if test str :start start)))
   (if p1
       (let ((p2 (position-if #'(lambda (ch)
				  (not (funcall test ch)))
			      str :start p1)))
	 (cons (subseq str p1 p2)
	       (if p2
		   (tokens str test p2)
		   nil)))
       nil)))

;;  test functions for $tokens:
(defun constituent (ch) ;; Author: Paul Graham - ANSI Common Lisp, 1996, page 67
  (and (graphic-char-p ch)
       (not (char= ch #\  ))))

(defun alphacharp (ch) (alpha-char-p ch))
(defun digitcharp (ch) (digit-char-p ch))
(defun lowercasep (ch) (lower-case-p ch))
(defun uppercasep (ch) (upper-case-p ch))
(defun charp (ch) (characterp ch))
;;     characterp (ch)
;;     alphanumericp (ch)


;;  splits string at an optional user defined delimiter character
;;  optional flag for multiple delimiter chars
(defun $split (mstr &optional (dc " ") (m t))
  (cons '(mlist)
	(split mstr
	       (character dc)
	       m)))

(defun split (str dc &optional (m t))
  (labels
    ((splitrest (str dc m start)
       (let ((p1 (position dc str :start start)))
	  (if p1
	    (let* ((p2 (position dc str :start (1+ p1)))
		   (ss (subseq str (1+ p1) p2)))
	       (if (and m (string= ss ""))
		 (if p2 (splitrest str dc m p2) nil)
		 (cons ss (if p2 (splitrest str dc m p2) nil))))
	    nil))))
   (let ((p1 (position dc str)))
     (if p1
	(let ((ss (subseq str 0 p1)))
	   (if (and m (string= ss ""))
	      (splitrest str dc m p1)
	      (cons ss (splitrest str dc m p1))))
	(list str)))))

;;  $sconcat for lists, allows an optional user defined separator string
;;  returns maxima-string
(defun $simplode (lis &optional (ds ""))
   (setq lis (cdr lis))
   (let ((res ""))
      (dolist (mstr lis)
	 (setq res (concatenate 'string res ($sconcat mstr) ds)))
      (string-right-trim ds res)))


;;  modified version of $sconcat, returns maxima-string
(defun $sconc (&rest args)
  (format t "$SCONC: PUNT TO $CONCAT. REPLACE CALLS TO THIS FUNCTION WITH CALLS TO CONCAT.~%")
  (apply #'$concat args))



(defun $slength (mstr)
   (length mstr))

(defun $sposition (mch mstr) ;; 1-indexed!
   (let ((pos (position (l-char mch) mstr)))
     (if pos (1+ pos))))

(defun $sreverse (mstr)
  (reverse mstr))

(defun $substring (mstr start &optional (end)) ;; 1-indexed!
  (subseq mstr (1- start) (if end (1- end))))


;;  comparison - test functions - at Maxima level
(defun $sequalignore (mstr1 mstr2)
   (string-equal mstr1 mstr2))

(defun $sequal (mstr1 mstr2)
   (string= mstr1 mstr2))

;;  comparison - test functions - at Lisp level
(defun sequalignore (str1 str2)
   (string-equal str1 str2))

(defun sequal (str1 str2)
   (string= str1 str2))


;;  functions for string manipulation
(defun $ssubstfirst (news olds mstr &optional (test '$sequal) (s 1) (e)) ;; 1-indexed!
   (let* ((str mstr)
	  (new news)
	  (old olds)
	  (len (length old))
	  (pos (search old str
		  :test (if (numberp test)
			   (merror
			     "ssubstfirst: Order of optional arguments: test, start, end")
			   (stripdollar test))
		  :start2 (1- s)
		  :end2 (if e (1- e)))))
	 (if (null pos)
	    str
	    (concatenate 'string
	       (subseq str 0 pos)
	       new
	       (subseq str (+ pos len))))))

(defun $ssubst (news olds mstr &optional (test '$sequal) (s 1) (e)) ;; 1-indexed!
   (let* ((str mstr)
	  (new news)
	  (old olds)
	  (pos (search old str
		  :test (if (numberp test)
			   (merror
			     "ssubst: Order of optional arguments: test, start, end")
			   (stripdollar test))
		  :start2 (1- s)
		  :end2 (if e (1- e)))))
      (if (null pos)
        str
	 ($ssubst
	    new
	    old
	    ($ssubstfirst new old
			  mstr (stripdollar test) (1+ pos) (if e (1+ e)))
	    (stripdollar test)
	    (1+ pos)
	    (if e (1+ e)) ))))


(defun $sremove (seq mstr &optional (test '$sequal) (s 1) (e))  ;; 1-indexed!
  (labels ((sremovefirst (seq str &optional (test '$sequal) (s 0) (e))
     (let* ((len (length seq))
	    (pos (search seq str
		    :test (stripdollar test)
		    :start2 s
		    :end2 e))
	    (sq1 (subseq str 0 pos))
	    (sq2 (subseq str (+ pos len))))
	(concatenate 'string sq1 sq2))))
   (let* ((str mstr)
	  (sss seq)
	  (end (if e (1- e)))
	  (start (search sss str
		    :test (if (numberp test)
			     (merror
			       "sremove: Order of optional arguments: test, start, end")
			     (stripdollar test))
		    :start2 (1- s)
		    :end2 end)))
      (do ()
	  ((null start) str)
	  (progn
	     (setq str (sremovefirst sss str (stripdollar test) start end))
	     (setq start (search sss str :test (stripdollar test) :start2 start :end2 end)))))))

(defun $sremovefirst (seq mstr &optional (test '$sequal) (s 1) (e))  ;; 1-indexed!
   (let* ((str mstr)
	  (sss seq)
	  (len (length sss))
	  (pos (search sss str
		  :test (if (numberp test)
			   (merror
			     "sremovefirst: Order of optional arguments: test, start, end")
			   (stripdollar test))
		  :start2 (1- s)
		  :end2 (if e (1- e))))
	  (sq1 (subseq str 0 pos))
	  (sq2 (if pos (subseq str (+ pos len)) "")))
      (concatenate 'string sq1 sq2)))


(defun $sinsert (seq mstr pos)  ;; 1-indexed!
   (let* ((str mstr)
	  (sq1 (subseq str 0 (1- pos)))
	  (sq2 (subseq str (1- pos))))
      (concatenate 'string sq1 seq sq2)))


(defun $ssort (mstr &optional (test '$clessp))
   (let ((copy (copy-seq mstr)))
      (stable-sort copy (stripdollar test))))


(defun $smismatch (mstr1 mstr2 &optional (test '$sequal))  ;; 1-indexed!
   (let ((pos 
      (mismatch mstr1 mstr2 :test (stripdollar test))))
     (if pos (1+ pos)) ))

(defun $ssearch (seq mstr &optional (test '$sequal) (s 1) (e))  ;; 1-indexed!
   (let ((pos
	   (search
	     seq
	     mstr
	     :test (if (numberp test)
		     (merror
		       "ssearch: Order of optional arguments: test, start, end")
		     (stripdollar test))
	     :start2 (1- s)
	     :end2 (if e (1- e)))))
     (if pos (1+ pos))))



(defun $strim (seq mstr)
  (string-trim seq mstr))

(defun $striml (seq mstr)
  (string-left-trim seq mstr))

(defun $strimr (seq mstr)
  (string-right-trim seq mstr))



(defun $supcase (mstr &optional (s 1) (e))  ;; 1-indexed!
  (string-upcase mstr :start (1- s) :end (if e (1- e))))

(defun $sdowncase (mstr &optional (s 1) (e))  ;; 1-indexed!
  (string-downcase mstr :start (1- s) :end (if e (1- e))))


(defun invert-char (ch) (setf ch (character ch))
   (if (upper-case-p ch)
      (char-downcase ch)
      (char-upcase ch)))

(defun invert-string-case (str)
   (let* ((cl1 (explode str))
	  (cl2 (cdr (butlast cl1))))
      (concatenate 'string (map 'list #'invert-char cl2))))

(defun $sinvertcase (mstr &optional (s 1) (e))  ;; 1-indexed!
   (let* ((str mstr)
	  (s1 (subseq str 0 (1- s)))
	  (s2 (subseq str (1- s) (if e (1- e))))
	  (s3 (if e (subseq str (1- e)) "")))
      (concatenate 'string s1 (invert-string-case s2) s3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this character definitions must be placed beneath the definition of m-char
(defmvar $newline  (m-char #\newline))
(defmvar $tab      (m-char #\tab))
(defmvar $space    (m-char #\space))
