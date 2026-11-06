;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1981 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module utils)

;;; General purpose Lisp utilities.  This file contains runtime functions which
;;; are simple extensions to Lisp.  The functions here are not very general, 
;;; but generalized forms would be useful in future Lisp implementations.
;;;
;;; No knowledge of the Macsyma system is kept here.  
;;;
;;; Every function in this file is known about externally.

(defun maxima-getenv (envvar)
  #+gcl     (si::getenv envvar)
  #+ecl     (si::getenv envvar)
  #+allegro (system:getenv envvar)
  #+(or cmu scl) (cdr (assoc envvar ext:*environment-list* :test #'string=))
  #+sbcl    (sb-ext:posix-getenv envvar)
  #+clisp   (ext:getenv envvar)
  #+(or openmcl mcl)     (ccl::getenv envvar)
  #+lispworks (lw:environment-variable envvar)
  #+abcl (ext:getenv envvar)
  )

;; CMUCL needs because when maxima reaches EOF, it calls BYE, not $QUIT.

(defun bye (&optional (exit-code 0))
  (declare (ignorable exit-code))
  #+scl       (ext:quit)
  #+clisp              (ext:quit exit-code)
  #+sbcl               (sb-ext:quit :unix-status exit-code)
  #+allegro            (excl:exit exit-code :quiet t)
  #+(or mcl openmcl)   (ccl:quit exit-code)
  #+ecl                (si:quit exit-code)
  #+lispworks          (lispworks:quit :status exit-code)
  #+abcl               (ext:quit :status exit-code)
  #+gcl                (system:bye exit-code)
  #+cmucl
  (handler-case (ext:quit nil exit-code)
    ;; Only the most recent versions of cmucl support an exit code.
    ;; If it doesn't, we get a program error (wrong number of args),
    ;; so catch that and just call quit without the arg.
    (program-error ()
      (ext:quit)))
  )


;;; F is assumed to be a function of two arguments.  It is mapped down L
;;; and applied to consecutive pairs of elements of the list.
;;; Useful for iterating over property lists.

(defun map2c (f l)
  (do ((llt l (cddr llt)) (lans))
      ((null llt) lans)
    (push (funcall f (car llt) (cadr llt)) lans)))

;;; Like MAPCAR, except if an application of F to any of the elements of L
;;; returns NIL, then the function returns NIL immediately.

(defun andmapcar (f l &aux d answer)
  (do ((l l (cdr l)))
      ((null l) (nreverse answer))
    (setq d (funcall f (car l)))
    (if d (push d answer) (return nil))))

;;; Returns T if either A or B is NIL, but not both.

(defun xor (a b)
  (or (and (not a) b) (and (not b) a)))
  
;;; Similar to AMONG, but takes a list of objects to look for.  If any
;;; are found in L, returns T.

(defun amongl (x l) 
  (cond ((null l) nil)
	((atom l) (member l x :test #'eq))
	(t (or (amongl x (car l)) (amongl x (cdr l)))))) 

;;; Tests to see whether one tree is a subtree of another.
;;;
;;; Both arguments should be well-formed cons trees (so no cycles). If supplied,
;;; TEST is used as an equality predicate.

(defun subtree-p (branch tree &key (test #'eql))
  (or (funcall test branch tree)
      (and (not (atom tree))
           (member branch tree
                   :test (lambda (x y) (subtree-p x y :test test))))))

;;; Takes a list in "alist" form and converts it to one in
;;; "property list" form, i.e. ((A . B) (C . D)) --> (A B C D).
;;; All elements of the list better be conses.

(defun dot2l (l)
  (cond ((null l) nil)
	(t (list* (caar l) (cdar l) (dot2l (cdr l))))))

;;; (C-PUT  sym value    selector)
;;;
;;;  Make a symbol's property list look like a structure.
;;;
;;;  If the value to be stored is NIL,
;;;     then flush the property.
;;;     else store the value under the appropriate property.
;;;

(defun cput (base val sel)
  (cond ((null val)
	 (zl-remprop base sel)
	 nil)
	(t
	 (putprop base val sel))))

(defun prefixp (prefix sequence &key (test #'eql))
  "Returns T if PREFIX is a prefix of SEQUENCE.
  Accepts any sequence type and an optional :TEST function (defaults to EQL).
  E.g. '(1 2) is a prefix of '(1 2 3), and \"hell\" is a prefix of \"hello\".
  Any sequence is a prefix of itself."
  (let ((m (mismatch sequence prefix :test test)))
    (or (not m) (>= m (length prefix)))))

;;; The process ID, and names for temporary files that differ between
;;; Maxima processes running at the same time.

#-gcl
(defun getpid-from-environment ()
  (handler-case
      (values (parse-integer (maxima-getenv "PID")))
    ((or type-error parse-error) () -1)))

;;; For gcl, getpid imported from system in maxima-package.lisp
#-gcl
(defun getpid ()
#+clisp (os:process-id)
#+(or cmu scl) (unix:unix-getpid)
#+sbcl (sb-unix:unix-getpid)
#+gcl (system:getpid)
#+openmcl (ccl::getpid)
#+lispworks (system::getpid)
#+ecl (si:getpid)
#+ccl (ccl::getpid)
#+allegro (excl::getpid)
#+abcl
(or (ignore-errors
     ;; ProcessHandle needs Java 9.
     (java:jcall "pid" (java:jstatic "current" "java.lang.ProcessHandle")))
    (ignore-errors
     ;; Java 8 names the JVM "<pid>@<host>".
     (let ((name (java:jcall
                  "getName"
                  (java:jstatic "getRuntimeMXBean"
                                "java.lang.management.ManagementFactory"))))
       (parse-integer name :end (position #\@ name))))
    (getpid-from-environment))
#-(or clisp cmu scl sbcl gcl openmcl lispworks ecl ccl allegro abcl)
  (getpid-from-environment)
)

;; Names of temporary files. PROCESS-TAG tells this Maxima process apart
;; from others running at the same time on this machine: it is the process
;; ID, or random characters made once where the ID is unknown. RANDOM-NAME
;; adds the number of the call, so that the names of one process differ,
;; and COUNT random alphanumeric characters from a newly seeded random
;; state, which make the name hard to guess unless the Lisp seeds random
;; states from the clock alone. TEMP-FILE-NAME puts PREFIX and a dash
;; before such a name, and the file type TYPE after it, unless TYPE is NIL
;; or empty.
(defvar *random-process-tag* nil)
(defvar *random-name-calls* 0)

(defun random-chars (count)
  (let ((chars "0123456789abcdefghijklmnopqrstuvwxyz") (name "")
        (state (make-random-state t)))
    (dotimes (i count name)
      (setq name (format nil "~a~a" name (aref chars (random 36 state)))))))

(defun process-tag ()
  (let ((pid (getpid)))
    (if (and (integerp pid) (plusp pid))
      (format nil "~d" pid)
      (or *random-process-tag*
          (setq *random-process-tag* (random-chars 8))))))

(defun random-name (count)
  (format nil "~a-~d-~a"
          (process-tag) (incf *random-name-calls*) (random-chars count)))

(defun temp-file-name (prefix &optional type)
  (format nil "~a-~a~@[.~a~]" prefix (random-name 16)
          (and type (plusp (length type)) type)))
