;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: INTL -*-

;;; $Revision: 1.10 $
;;; Copyright 1999 Paul Foley (mycroft@actrix.gen.nz)
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this Software to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; provided that the above copyright notice and this permission notice
;;; are included in all copies or substantial portions of the Software.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.
#+CMU (ext:file-comment "$Header: /home/lbutler/maxima/sandbox/cvs/maxima/maxima/src/intl.lisp,v 1.10 2009-03-29 08:17:25 andrejv Exp $")

(in-package :intl)

(defvar *locale-directories* '(#p"/usr/share/locale/"))
(defvar *locale* "C")

(defvar *default-domain* "maxima")
(defvar *loaded-domains* (make-hash-table :test #'equal))
(defvar *locale-aliases* (make-hash-table :test #'equal))
(defvar *locale-encoding* (make-hash-table :test #'equal))

(defstruct domain-entry
  (domain "" :type simple-base-string)
  (locale "" :type simple-base-string)
  (file #p"" :type pathname)
  (plurals nil :type (or null function))
  (hash (make-hash-table :test #'equal) :type hash-table)
  (encoding nil))

(declaim (inline read-lelong)
	 (ftype (function (stream) (unsigned-byte 32)) read-lelong))
(defun read-lelong (stream)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (+ (the (unsigned-byte 8) (read-byte stream))
     (ash (the (unsigned-byte 8) (read-byte stream)) 8)
     (ash (the (unsigned-byte 8) (read-byte stream)) 16)
     (ash (the (unsigned-byte 8) (read-byte stream)) 24)))

(defun locate-domain-file (domain locale locale-dir)
  (flet ((path (locale base)
	   (merge-pathnames (make-pathname :directory (list :relative locale
							    "LC_MESSAGES")
					   :name domain :type "mo")
			    base)))
    (let ((locale (or (gethash locale *locale-aliases*) locale)))
      (dolist (base (if (listp locale-dir) locale-dir (list locale-dir)))
	(let ((probe
	       (or (probe-file (path locale base))
		   (let ((dot (position #\. locale)))
		     (and dot (probe-file (path (subseq locale 0 dot) base))))
		   (let ((at (position #\@ locale)))
		     (and at (probe-file (path (subseq locale 0 at) base))))
		   (let ((us (position #\_ locale)))
		     (and us (probe-file (path (subseq locale 0 us) base)))))))
	  (when probe (return probe)))))))

(defun find-encoding (domain)
  (when (null (domain-entry-encoding domain))
    (setf (domain-entry-encoding domain) :iso-8859-1)
    (let* ((header (domain-lookup "" domain))
	   (ctype (search "Content-Type: " header))
	   (eoln (and ctype (position #\Newline header :start ctype)))
	   (charset (and ctype (search "; charset=" header
				       :start2 ctype :end2 eoln))))
      (when charset
	(incf charset 10)
	(loop for i upfrom charset below eoln as c = (char header i)
            while (or (alphanumericp c) (eql c #\-))
          finally (setf (domain-entry-encoding domain)
		      (intern (subseq header charset i) "KEYWORD"))))))
  domain)

(defun parse-plurals (domain)
  (let* ((header (domain-lookup "" domain))
	 (plurals (search "Plural-Forms: " header))
	 (default (lambda (n) (if (= n 1) 0 1))))
    (if (and plurals
	     (> (length header) (+ plurals 36))
	     (string= header "nplurals="
		      :start1 (+ plurals 14) :end1 (+ plurals 23)))
	(let ((nplurals
	       (parse-integer header :start (+ plurals 23) :junk-allowed t))
	      (point (+ (position #\; header :start (+ plurals 23)) 2)))
	  (if (and (> (length header) (+ point 10))
		   (string= header "plural=" :start1 point :end1 (+ point 7)))
	      (values (parse-expr header (+ point 7)) nplurals)
	      (values default 2)))
	(values default 2))))

(defun parse-expr (string pos)
  (labels ((next ()
	     (loop while (member (char string pos) '(#\Space #\Tab #\Newline))
		   do (incf pos))
	     (case (char string (1- (incf pos)))
	       (#\n 'n)
	       (#\? 'IF)
	       (#\: 'THEN)
	       (#\( 'LPAR)
	       (#\) 'RPAR)
	       (#\^ 'LOGXOR)
	       (#\+ 'ADD)
	       (#\- 'SUB)
	       (#\* 'MUL)
	       (#\/ 'FLOOR)
	       (#\% 'MOD)
	       (#\~ 'LOGNOT32)
	       (#\; 'END)
	       (#\| (if (char= (char string pos) #\|)
			(progn (incf pos) 'COR)
			'LOGIOR))
	       (#\& (if (char= (char string pos) #\&)
			(progn (incf pos) 'CAND)
			'LOGAND))
	       (#\= (if (char= (char string pos) #\=)
			(progn (incf pos) 'CMP=)
			(error "Encountered illegal token: =")))
	       (#\! (if (char= (char string pos) #\=)
			(progn (incf pos) 'CMP/=)
			'NOT))
	       (#\< (case (char string pos)
		      (#\= (incf pos) 'CMP<=)
		      (#\< (incf pos) 'SHL)
		      (otherwise 'CMP<)))
	       (#\> (case (char string pos)
		      (#\= (incf pos) 'CMP>=)
		      (#\> (incf pos) 'SHR)
		      (otherwise 'CMP>)))
	       (otherwise (let ((n (digit-char-p (char string (1- pos)))))
			    (if n
				(loop for nx = (digit-char-p (char string pos))
				      while nx
				   do (setq n (+ (* n 10) nx)) (incf pos)
				   finally (return n))
				(error "Encountered illegal token: ~C"
				       (char string (1- pos))))))))
	   (conditional (tok &aux tree)
	     (multiple-value-setq (tree tok) (logical-or tok))
	     (when (eql tok 'IF)
	       (multiple-value-bind (right next) (logical-or (next))
		 (unless (eql next 'THEN)
		   (error "Expected : in ?: construct"))
		 (multiple-value-bind (else next) (conditional (next))
		   (setq tree (list tok (list 'zerop tree) else right)
			 tok next))))
	     (values tree tok))
	   (logical-or (tok &aux tree)
	     (multiple-value-setq (tree tok) (logical-and tok))
	     (loop while (eql tok 'COR) do
		(multiple-value-bind (right next) (logical-and (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (logical-and (tok &aux tree)
	     (multiple-value-setq (tree tok) (inclusive-or tok))
	     (loop while (eql tok 'CAND) do
		(multiple-value-bind (right next) (inclusive-or (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (inclusive-or (tok &aux tree)
	     (multiple-value-setq (tree tok) (exclusive-or tok))
	     (loop while (eql tok 'LOGIOR) do
		(multiple-value-bind (right next) (exclusive-or (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (exclusive-or (tok &aux tree)
	     (multiple-value-setq (tree tok) (bitwise-and tok))
	     (loop while (eql tok 'LOGXOR) do
		(multiple-value-bind (right next) (bitwise-and (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (bitwise-and (tok &aux tree)
	     (multiple-value-setq (tree tok) (equality tok))
	     (loop while (eql tok 'LOGAND) do
		(multiple-value-bind (right next) (equality (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (equality (tok &aux tree)
	     (multiple-value-setq (tree tok) (relational tok))
	     (loop while (member tok '(CMP= CMP/=)) do
		(multiple-value-bind (right next) (relational (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (relational (tok &aux tree)
	     (multiple-value-setq (tree tok) (shift tok))
	     (loop while (member tok '(CMP< CMP> CMP<= CMP>=)) do
		(multiple-value-bind (right next) (shift (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (shift (tok &aux tree)
	     (multiple-value-setq (tree tok) (additive tok))
	     (loop while (member tok '(SHL SHR)) do
		(multiple-value-bind (right next) (additive (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (additive (tok &aux tree)
	     (multiple-value-setq (tree tok) (multiplicative tok))
	     (loop while (member tok '(ADD SUB)) do
		(multiple-value-bind (right next) (multiplicative (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (multiplicative (tok &aux tree)
	     (multiple-value-setq (tree tok) (unary tok))
	     (loop while (member tok '(MUL FLOOR MOD)) do
		(multiple-value-bind (right next) (unary (next))
		  (setq tree (list tok tree right)
			tok next)))
	     (values tree tok))
	   (unary (tok &aux tree)
	     (cond ((eq tok 'LPAR)
		    (multiple-value-setq (tree tok) (conditional (next)))
		    (unless (eq tok 'RPAR)
		      (error "Expected close-paren."))
		    (values tree (next)))
		   ((numberp tok)
		    (values tok (next)))
		   ((eql tok 'n)
		    (values tok (next)))
		   ((eql tok 'ADD)
		    (unary (next)))
		   ((eql tok 'SUB)
		    (multiple-value-setq (tree tok) (unary (next)))
		    (values (list '- tree) tok))
		   ((eql tok 'LOGNOT32)
		    (multiple-value-setq (tree tok) (unary (next)))
		    (values (list 'LOGNOT32 tree) tok))
		   ((eql tok 'NOT)
		    (multiple-value-setq (tree tok) (unary (next)))
		    (values (list 'CNOT tree) tok))
		   (t
		    (error "Unexpected token: ~S." tok)))))
    (multiple-value-bind (tree end) (conditional (next))
      (unless (eq end 'END)
	(error "Expecting end of expression.  ~S." end))
      (let ((*compile-print* nil))
	(compile nil
		 `(lambda (n)
		    (declare (type (unsigned-byte 32) n)
			     (optimize (space 3)))
		    (flet ((add   (a b) (ldb (byte 32 0) (+ a b)))
			   (sub   (a b) (ldb (byte 32 0) (- a b)))
			   (mul   (a b) (ldb (byte 32 0) (* a b)))
			   (shl   (a b) (ldb (byte 32 0) (ash a b)))
			   (shr   (a b) (ash a (- b)))
			   (cmp=  (a b) (if (= a b) 1 0))
			   (cmp/= (a b) (if (/= a b) 1 0))
			   (cmp<  (a b) (if (< a b) 1 0))
			   (cmp<= (a b) (if (<= a b) 1 0))
			   (cmp>  (a b) (if (> a b) 1 0))
			   (cmp>= (a b) (if (>= a b) 1 0))
			   (cand  (a b) (if (or (zerop a) (zerop b)) 0 1))
			   (cor   (a b) (if (and (zerop a) (zerop b)) 0 1))
			   (cnot  (a)   (if a 0 1))
			   (lognot32 (a) (ldb (byte 32 0) (lognot a))))
		      (declare (ignorable #'add #'sub #'mul #'shr #'shl
					  #'cmp= #'cmp/=
					  #'cmp< #'cmp<= #'cmp> #'cmp>=
					  #'cand #'cor #'cnot #'lognot32))
		      ,tree)))))))

(defun load-domain (domain locale &optional (locale-dir *locale-directories*))
  (let ((file (locate-domain-file domain locale locale-dir)))
    (unless file (return-from load-domain nil))
    (with-open-file (stream file :direction :input :if-does-not-exist nil
			    :element-type '(unsigned-byte 8))
      (unless stream (return-from load-domain nil))
      (unless (= (read-lelong stream) #x950412de)
	(error "Bad magic number in \"~A.mo\"." domain))
      (let ((version (read-lelong stream))
	    (messages (read-lelong stream))
	    (master (read-lelong stream))
	    (translation (read-lelong stream))
	    (entry (make-domain-entry)))
	(declare (ignore version))
	(setf (domain-entry-domain entry) domain)
	(setf (domain-entry-locale entry) locale)
	(setf (domain-entry-file entry) file)
	(dotimes (msg messages)
	  (file-position stream (+ master (* 8 msg)))
	  (let ((length (read-lelong stream))
		(start (read-lelong stream)))
	    (setf (gethash length (domain-entry-hash entry))
		  (acons start (+ translation (* 8 msg))
			 (gethash length (domain-entry-hash entry))))))
	(setf (gethash domain *loaded-domains*) entry)
	(find-encoding entry)))))

(defun find-domain (domain locale &optional (locale-dir *locale-directories*))
  (let ((found (gethash domain *loaded-domains*)))
    (if (and found (string= (domain-entry-locale found) locale))
	found
	(load-domain domain locale locale-dir))))

(defun string-to-octets (string encoding)
  (declare (ignorable encoding))
  #+(and CMU Unicode)
  (ext:string-to-octets string :external-format encoding)
  ;;@@ add other implementations
  #-(or (and CMU Unicode) #|others|#)
  (map-into (make-array (length string) :element-type '(unsigned-byte 8))
	    #'char-code string))

(defun octets-to-string (octets encoding)
  (declare (ignorable encoding))
  #+(and CMU Unicode)
  (ext:octets-to-string octets :external-format encoding)
  ;;@@ add other implementations
  #-(or (and CMU Unicode) #|others|#)
  (map-into (make-string (length octets)) #'code-char octets))

(defun octets= (a b &key (start1 0) (end1 (length a))
			 (start2 0) (end2 (length b)))
  (declare (type (simple-array (unsigned-byte 8) (*)) a b)
	   (type (integer 0 #.array-dimension-limit) start1 end1 start2 end2)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (loop
    (unless (= (aref a start1) (aref b start2)) (return nil))
    (when (or (= (incf start1) end1) (= (incf start2) end2)) (return t))))

(defun search-domain (octets domain pos)
  (declare (type (simple-array (unsigned-byte 8) (*)) octets)
	   (type domain-entry domain)
	   (type list pos)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (when pos
    (let ((temp (make-array 120 :element-type '(unsigned-byte 8)))
	  (length (length octets)))
      (with-open-file (stream (domain-entry-file domain)
			      :direction :input
			      :element-type '(unsigned-byte 8))
	(dolist (entry pos)
	  (file-position stream (car entry))
	  (let ((off 0)
		(end (read-sequence temp stream
				    :end (min 120 length))))
	    (declare (type (integer 0 #.array-dimension-limit) off end))
	    (loop while (octets= octets temp
			  :start1 off
			  :end1 (min (+ off 120) length)
			  :end2 end)
	      do
		(incf off end)
		(when (< off length)
		  (setf end (read-sequence temp stream
					   :end (min 120 (- length off))))))
	    (when (= off length)
	      (file-position stream (cdr entry))
	      (let* ((len (read-lelong stream))
		     (off (read-lelong stream))
		     (tmp (make-array len :element-type '(unsigned-byte 8))))
		(file-position stream off)
		(read-sequence tmp stream)
		(return (values tmp entry))))))))))

(defun domain-lookup (string domain)
  (declare (type string string) (type domain-entry domain)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (or (if (null (domain-entry-encoding domain)) string)
      (gethash string (domain-entry-hash domain))
      (let* ((octets (string-to-octets string
				       (domain-entry-encoding domain)))
	     (length (length octets))
	     (pos (gethash length (domain-entry-hash domain))))
	(declare (type vector octets))
	(multiple-value-bind (tmp entry) (search-domain octets domain pos)
	  (declare (type (or null (simple-array (unsigned-byte 8) (*))) tmp))
	  (when tmp
	    (let ((temp (delete entry pos :test #'eq)))
	      (if temp
		  (setf (gethash length (domain-entry-hash domain)) temp)
		  (remhash length (domain-entry-hash domain))))
	    (setf (gethash (copy-seq string) (domain-entry-hash domain))
		(octets-to-string tmp (domain-entry-encoding domain))))))))

(defun domain-lookup-plural (singular plural domain)
  (declare (type string singular plural) (type domain-entry domain)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (or (if (null (domain-entry-encoding domain)) nil)
      (gethash (cons singular plural) (domain-entry-hash domain))
      (let* ((octets (let* ((a (string-to-octets singular
					       (domain-entry-encoding domain)))
			    (b (string-to-octets plural
					       (domain-entry-encoding domain)))
			    (c (make-array (+ (length a) (length b) 1)
					   :element-type '(unsigned-byte 8))))
		       (declare (type (simple-array (unsigned-byte 8) (*))
				      a b c))
		       (replace c a)
		       (setf (aref c (length a)) 0)
		       (replace c b :start1 (+ (length a) 1))
		       c))
	     (length (length octets))
	     (pos (gethash length (domain-entry-hash domain))))
	(declare (type vector octets) (type list pos))
	(multiple-value-bind (tmp entry) (search-domain octets domain pos)
	  (declare (type (or null (simple-array (unsigned-byte 8) (*))) tmp))
	  (when tmp
	    (prog1
		(setf (gethash (cons (copy-seq singular) (copy-seq plural))
			       (domain-entry-hash domain))
		    (loop for i = 0 then (1+ j)
			   as j = (position 0 tmp :start i)
		      collect (octets-to-string (subseq tmp i j)
						(domain-entry-encoding domain))
		      while j))
	      (let ((temp (delete entry pos :test #'eq)))
		(if temp
		    (setf (gethash length (domain-entry-hash domain)) temp)
		    (remhash length (domain-entry-hash domain))))
	      (when (null (domain-entry-plurals domain))
		(setf (domain-entry-plurals domain)
		    (parse-plurals domain)))))))))

(declaim (inline getenv)
	 (ftype (function (string) (or null string)) getenv))
(defun getenv (var)
  (let ((val #+(or CMU SCL) (cdr (assoc (intern var "KEYWORD")
					ext:*environment-list*))
	     #+SBCL (sb-ext:posix-getenv var)
	     #+Allegro (system:getenv var)
	     #+LispWorks (hcl:getenv var)
	     #+clisp (ext:getenv var)
	     #+(or openmcl mcl) (ccl::getenv var)
	     #+(or gcl ecl) (si::getenv var)))
    (if (equal val "") nil val)))

(defun setlocale (&optional locale)
  (setf *locale* (or locale
		     (getenv "LANGUAGE")
		     (getenv "LC_ALL")
		     (getenv "LC_MESSAGES")
		     (getenv "LANG")
		     *locale*)))

(defmacro textdomain (domain)
  `(eval-when (:compile-toplevel :execute)
     (setf *default-domain* ,domain)))

(defmacro gettext (string)
  `(dgettext ,*default-domain* ,string))

(defmacro ngettext (singular plural n)
  `(dngettext ,*default-domain* ,singular ,plural ,n))

(declaim (inline dgettext))
(defun dgettext (domain string)
  "Look up STRING in the specified message domain and return its translation."
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (let ((domain (find-domain domain *locale*)))
    (or (and domain (domain-lookup string domain)) string)))

(defun dngettext (domain singular plural n)
  "Look up the singular or plural form of a message in the specified domain."
  (declare (type integer n)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (let* ((domain (find-domain domain *locale*))
	 (list (and domain (domain-lookup-plural singular plural domain))))
    (if list
	(nth (the integer
	       (funcall (the function (domain-entry-plurals domain)) n))
	     list)
	(if (= n 1) singular plural))))

(defun po-output-string (stream string)
  (format stream "~&msgid ~S~%msgstr \"\"~2%" string))

(defvar *translatable-dump-stream* nil)

(defun read-translatable-string (stream subchar arg)
  (let ((string (funcall (get-macro-character #\") stream subchar)))
    (when *translatable-dump-stream*
      (po-output-string *translatable-dump-stream* string))
    (if (eql arg 0)
	string
	`(dgettext ,*default-domain* ,string))))

(defun install ()
  (unless #+CMU (eq (get-macro-character #\_) #'lisp::read-dispatch-char)
	  #-CMU (get-macro-character #\_)
    (make-dispatch-macro-character #\_ t))
  (set-dispatch-macro-character #\_ #\" #'read-translatable-string)
  t)
