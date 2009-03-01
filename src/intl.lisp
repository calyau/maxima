;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: INTL -*-

;;; $Revision: 1.6 $
;;; Copyright © 1999 Paul Foley (mycroft@actrix.gen.nz)
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
#+CMU (ext:file-comment "$Header: /home/lbutler/maxima/sandbox/cvs/maxima/maxima/src/intl.lisp,v 1.6 2009-03-01 17:56:32 andrejv Exp $")

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

(defun guess-locale-encoding (locale)
  (let ((locale (or (gethash locale *locale-aliases*) locale)))
    (or (let ((dot (position #\. locale)))
	  (and dot (intern (string-upcase (subseq locale (1+ dot)))
			   "KEYWORD")))
	(gethash locale *locale-encoding*)
	(let ((at (position #\@ locale)))
	  (and at (gethash (subseq locale 0 at) *locale-encoding*)))
	(let ((us (position #\_ locale)))
	  (and us (gethash (subseq locale 0 us) *locale-encoding*)))
	:utf-8)))

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
	(setf (domain-entry-encoding entry) (guess-locale-encoding locale))
	(setf (gethash domain *loaded-domains*) entry)))))

(defun find-domain (domain locale &optional (locale-dir *locale-directories*))
  (let ((found (gethash domain *loaded-domains*)))
    (if (and found (string= (domain-entry-locale found) locale))
	found
	(load-domain domain locale locale-dir))))

(defun string-to-octets (string encoding)
  #+(and CMU Unicode)
  (ext:string-to-octets string :external-format encoding)
  ;;@@ add other implementations
  #-(or (and CMU Unicode) #|others|#)
  (map-into (make-array (length string) :element-type '(unsigned-byte 8))
	    #'char-code string))

(defun octets-to-string (octets encoding)
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

(defun domain-lookup (string domain)
  (declare (type string string) (type domain-entry domain)
	   (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (or (if (null (domain-entry-encoding domain)) string)
      (gethash string (domain-entry-hash domain))
      (let* ((octets (string-to-octets string
				       (domain-entry-encoding domain)))
	     (length (length octets))
	     (pos (gethash length (domain-entry-hash domain)))
	     (temp (make-array 120 :element-type '(unsigned-byte 8))))
	(declare (type (simple-array (unsigned-byte 8) (*)) octets)
		 (type list pos))
	(when pos
	  (with-open-file (stream (domain-entry-file domain)
				  :direction :input
				  :element-type '(unsigned-byte 8))
	    (dolist (entry pos)
	      (file-position stream (car entry))
	      (let ((off 0)
		    (end (read-sequence temp stream
					:end (min 120 length))))
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
			 (tmp (make-array len
					  :element-type '(unsigned-byte 8)))
			 str)
		    (file-position stream off)
		    (read-sequence tmp stream)
		    (setq str (octets-to-string tmp
						(domain-entry-encoding
						 domain)))
		    (setf (gethash (copy-seq string)
				   (domain-entry-hash domain))
			  str)
		    (let ((temp (delete entry pos :test #'eq)))
		      (if temp
			  (setf (gethash (length string)
					 (domain-entry-hash domain))
				temp)
			  (remhash (length string)
				   (domain-entry-hash domain))))
		    (return-from domain-lookup str))))))))))

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

;; This is a macro so that it can take effect at compile-time, in order
;; to allow the _" reader macro to define the correct domain.
(defmacro textdomain (domain)
  `(eval-when (:compile-toplevel :execute)
    (setf *default-domain* ,domain)))

;; This is a macro so that it can capture the correct value of *default-domain*
(defmacro gettext (string)
  `(dgettext ,*default-domain* ,string))

(declaim (inline dgettext))
(defun dgettext (domain string)
  "Look up STRING in the specified message domain and return its translation."
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (let ((domain (find-domain domain *locale*)))
    (or (and domain (domain-lookup string domain)) string)))

#+(or)
(define-compiler-macro dgettext (domain string)
  (let ((cache (gensym))
	(clean (gensym))
	(dom (gensym))
	(str (gensym)))
    `(let ((,cache (load-time-value (vector nil nil nil)))
	   (,clean t)
	   (,dom ,domain)
	   (,str ,string))
       (declare (optimize (speed 3) (space 2) (safety 0) (debug 0))
		(type (simple-array t (3)) ,cache)
		(ignorable ,str))
       (unless (and (aref ,cache 0)
		    (string= (domain-entry-domain
			      (the domain-entry (aref ,cache 0)))
			     ,dom)
		    (string= (domain-entry-locale
			      (the domain-entry (aref ,cache 0)))
			     *locale*))
	 (setf (aref ,cache 0) (find-domain ,dom *locale*)
	       ,clean nil))
       ,(if (stringp string)
	    `(if ,clean
		 (aref ,cache 1)
		 (setf (aref ,cache 1)
		     (or (and (aref ,cache 0)
			      (domain-lookup ,string (aref ,cache 0)))
		         ,string)))
	    `(if (and ,clean (string= (aref ,cache 2) ,str))
		 (aref ,cache 1)
		 (setf (aref ,cache 1) (or (and (aref ,cache 0)
						(domain-lookup ,str
							      (aref ,cache 0)))
					   ,str)
		       (aref ,cache 2) ,str))))))

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
