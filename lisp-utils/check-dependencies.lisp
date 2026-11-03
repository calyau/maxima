;;;; check-dependencies.lisp: check that no file in src/maxima.system
;;;; leaves the global compiler policy changed
;;;;
;;;; A top-level (DECLAIM (OPTIMIZE ...)) is global: it stays in force for
;;;; every file compiled after it, not just the rest of its own file. That
;;;; changes how every later file is compiled, which no :DEPENDS-ON can
;;;; express and no cross-reference records, so it is checked directly.
;;;; The convention this enforces is that a file which changes the policy
;;;; restores it before its end, so the proclamations come in pairs.
;;;;
;;;; Run it with SBCL in a built Maxima image:
;;;;
;;;;     sbcl --core src/binary-sbcl/maxima.core   ; or ./maxima-local
;;;;     (load "lisp-utils/check-dependencies.lisp")
;;;;     (maxima-depcheck:check-and-exit "/path/to/maxima/")

(defpackage #:maxima-depcheck
  (:use #:common-lisp)
  (:export #:check #:check-and-exit))

(in-package #:maxima-depcheck)

;;; ------------------------------------------------------------------
;;; Reaching into DEFSYSTEM. Its package does not exist when this file
;;; is read, so every name is looked up at run time.

(defun mkf (name)
  (or (and (find-package "MK") (find-symbol name "MK"))
      (error "check-dependencies: MK::~A not found; is defsystem.lisp loaded?"
             name)))

(defun cname (c) (funcall (mkf "COMPONENT-NAME") c))
(defun ctype (c) (funcall (mkf "COMPONENT-TYPE") c))
(defun ckids (c) (funcall (mkf "COMPONENT-COMPONENTS") c))
(defun cfile (c)
  (ignore-errors (funcall (mkf "COMPONENT-FULL-PATHNAME") c :source)))

(defun load-maxima-system (root)
  "Load defsystem from ROOT and return the maxima system's component tree."
  (unless (find-package "MK")
    (load (merge-pathnames "lisp-utils/defsystem.lisp" root)))
  (funcall (mkf "ADD-REGISTRY-LOCATION") (merge-pathnames "src/" root))
  (funcall (mkf "FIND-SYSTEM") 'maxima :load))

;;; ------------------------------------------------------------------
;;; Global compiler policy

(defun optimize-proclamations (path)
  "Line numbers of top-level OPTIMIZE proclamations in PATH.
Scanned textually rather than with READ: every top-level form in src/
starts at column 0, and reading these files needs the Maxima readtable."
  (let ((hits '()) (n 0))
    (with-open-file (s path :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil nil)
              while line
              do (incf n)
                 (when (and (plusp (length line))
                            (char= (char line 0) #\()
                            (or (search "(declaim (optimize" line)
                                (search "(proclaim '(optimize" line)
                                (search "(proclaim (quote (optimize" line)))
                   (push n hits)))))
    (nreverse hits)))

(defun policy-leaks (system)
  "Files that change the global policy an odd number of times, i.e. leave
it changed for whatever the build compiles next."
  (let ((out '()) (files 0))
    (labels ((walk (c)
               (if (eq (ctype c) :file)
                   (let ((path (cfile c)))
                     (when path
                       (incf files)
                       (let ((hits (optimize-proclamations path)))
                         (when (oddp (length hits))
                           (push (list (cname c) hits) out)))))
                   (mapc #'walk (ckids c)))))
      (walk system))
    (when (zerop files)
      (error "check-dependencies: no source files resolved from ~
              maxima.system, so the check would pass vacuously."))
    (nreverse out)))

;;; ------------------------------------------------------------------
;;; Reporting

(defun report-policy-leaks (leaks stream)
  (if (null leaks)
      (format stream "~&check-dependencies: no file leaves the global ~
                      compiler policy changed.~%")
      (progn
        (format stream "~&check-dependencies: ~D file~:P change~:[~;s~] the ~
                        global compiler policy without restoring it.~2%"
                (length leaks) (= 1 (length leaks)))
        (dolist (l leaks)
          (destructuring-bind (name lines) l
            (format stream "  ~A.lisp proclaims OPTIMIZE at line~P ~{~D~^, ~}~%"
                    name (length lines) lines)
            (format stream "      DECLAIM is global: every file compiled ~
                            after this one inherits~%      the policy, and ~
                            no :DEPENDS-ON can express that. Restore it ~
                            with a~%~6Tsecond proclamation at the end of the ~
                            file.~2%"))))))

(defun check (root &key (stream *standard-output*))
  "Check maxima.system under ROOT. Returns the files that leak a policy."
  (let ((leaks (policy-leaks (load-maxima-system (pathname root)))))
    (report-policy-leaks leaks stream)
    leaks))

(defun check-and-exit (root)
  (let ((leaks (handler-case (check root)
                 (error (e)
                   (format *error-output* "~&check-dependencies: ~A~%" e)
                   (sb-ext:exit :code 2 :abort t)))))
    (sb-ext:exit :code (if leaks 1 0) :abort t)))
