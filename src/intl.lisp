;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: INTL -*-

;;; $Revision: 1.1 $
;;; Copyright © 1999 Paul Foley (mycroft@actrix.gen.nz)

;;; Released under terms of the GNU General Public License, version 2 or later,
;;; by permission of Paul Foley to Robert Dodier as shown in this message:

#|
On Sat, Feb 7, 2009 at 7:57 AM, <robert.dodier@gmail.com> wrote:

    Hello Paul,

    I have found your program intl.lisp at:
    http://users.actrix.co.nz/mycroft/intl.lisp

    I am considering incorporating intl.lisp into another
    program (namely Maxima). As it stands, the license for
    intl.lisp as stated in the file itself might already allow that.
    However, since Maxima as a whole is licensed under terms
    of the GNU General Public License,
    I would like to ask you if I can license intl.lisp under terms of
    the GNU General Public License, version 2 or later.


Sure; no problem at all!
|#

#+CMU (ext:file-comment "$Header: /home/lbutler/maxima/sandbox/cvs/maxima/maxima/src/intl.lisp,v 1.1 2009-02-09 03:09:49 robert_dodier Exp $")

(defpackage "INTL"
  (:export "SETLOCALE" "TEXTDOMAIN" "GETTEXT" "DGETTEXT"
           "*TRANSLATABLE-DUMP-STREAM*"))

(in-package "INTL")

(defvar *locale-directory* "/usr/share/locale/")
(defvar *locale* "C")

(defvar *default-domain* "maxima")
(defvar *loaded-domains* (make-hash-table :test #'equal))
(defvar *locale-aliases* (make-hash-table :test #'equal))

(defstruct domain-entry
  (domain "" :type simple-base-string)
  (locale "" :type simple-base-string)
  (file #p"" :type pathname)
  (hash (make-hash-table :test #'equal) :type hash-table))

(declaim (inline read-lelong)
	 (ftype (function (stream) (unsigned-byte 32)) read-lelong))
(defun read-lelong (stream)
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (let ((a (read-char stream))
	(b (read-char stream))
	(c (read-char stream))
	(d (read-char stream)))
    (+ (ash (char-code d) 24) (ash (char-code c) 16) (ash (char-code b) 8)
       (char-code a))))

(defun locate-domain-file (domain locale locale-dir)
  (flet ((path (locale)
	   (merge-pathnames (make-pathname :directory (list :relative locale
							    "LC_MESSAGES")
					   :name domain :type "mo")
			    locale-dir)))
    (let ((locale (or (gethash locale *locale-aliases*) locale)))
      (or (probe-file (path locale))
          (let ((dot (position #\. locale)))
            (and dot (probe-file (path (subseq locale 0 dot)))))
          (let ((us (position #\_ locale)))
            (and us (probe-file (path (subseq locale 0 us)))))))))

(defun load-domain (domain locale &optional (locale-dir *locale-directory*))
  (let ((file (locate-domain-file domain locale locale-dir)))
    (unless file (return-from load-domain nil))
    (with-open-file (stream file :direction :input :if-does-not-exist nil)
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
	(setf (gethash domain *loaded-domains*) entry)))))

(defun find-domain (domain locale &optional (locale-dir *locale-directory*))
  (let ((found (gethash domain *loaded-domains*)))
    (if (and found (string= (domain-entry-locale found) locale))
	found
	(load-domain domain locale locale-dir))))

(defun domain-lookup (string domain)
  (or (gethash string (domain-entry-hash domain))
      (let ((pos (gethash (length string) (domain-entry-hash domain)))
	    (length (length string))
	    (temp (make-string 120)))
	(when pos
	  (with-open-file (stream (domain-entry-file domain)
				  :direction :input)
	    (dolist (entry pos)
	      (file-position stream (car entry))
	      (let ((off 0)
		    (end (read-sequence temp stream
					:end (min 120 length))))
		(loop while (string= string temp
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
			 (str (make-string len)))
		    (file-position stream off)
		    (read-sequence str stream)
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

#+CMU
(declaim (inline getenv)
	 (ftype (function (string) (or null string)) getenv))
#+CMU
(defun getenv (var)
  (cdr (assoc (intern var "KEYWORD") ext:*environment-list*)))
#+Allegro
(import 'sys:getenv)

(defun setlocale (&optional locale)
  (setf *locale* (or locale
		     (getenv "LANGUAGE")
		     (getenv "LC_ALL")
		     (getenv "LC_MESSAGES")
		     (getenv "LANG"))))

;; This is a macro so that it can take effect at compile-time, in order
;; to allow the _" reader macro to define the correct domain.
(defmacro textdomain (domain)
  `(eval-when (:compile-toplevel :execute)
    (setf *default-domain* ,domain)))

;; This is a macro so that it can capture the correct value of *default-domain*
(defmacro gettext (string)
  `(dgettext ,*default-domain* ,string))
#||
(declaim (inline gettext))
(defun gettext (string)
  "Look up STRING in the default message domain and return its translation."
  (declare (optimize (speed 3) (space 2) (safety 0) (debug 0)))
  (unless (and *current-domain*
	       (eq (domain-entry-domain *current-domain*) *default-domain*)
	       (eq (domain-entry-locale *current-domain*) *locale*))
    (setf *current-domain* (find-domain *default-domain* *locale*)))
  (or (and *current-domain* (domain-lookup string *current-domain*)) string))
||#

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
  (unless #+(or CMU SBCL) (eq (get-macro-character #\_)
			      #'lisp::read-dispatch-char)
	  #-(or CMU SBCL) (get-macro-character #\_)
    (make-dispatch-macro-character #\_ t))
  (set-dispatch-macro-character #\_ #\" #'read-translatable-string)
  (set-dispatch-macro-character #\# #\" #'read-translatable-string)
  t)
