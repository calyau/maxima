;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;; Maxima's entry point to MK:DEFSYSTEM.
;;;
;;; mk-defsystem.lisp next to this file is the unmodified upstream
;;; MK:DEFSYSTEM. This file loads it and adds what Maxima needs by
;;; wrapping a few upstream functions. No upstream code is copied, so
;;; upgrading MK:DEFSYSTEM means replacing mk-defsystem.lisp.

(in-package "CL-USER")

;;; ECL started with its bytecodes compiler would make upstream derive
;;; the bytecode fasl type. Load the C compiler first.
#+ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'cmp))

;;; MAKE is created here, as upstream would, with a few CL names shadowed.
;;; Upstream then skips creating it.
;;;  - ENSURE-DIRECTORIES-EXIST: upstream calls it with the binary's
;;;    directory but without its device, which on Windows is the drive.
;;;    MAKE's version puts the device back (see below). On GCL this also
;;;    keeps upstream's no-op definition away from CL's.
;;;  - GCL 2.6 rejects (SETF (DOCUMENTATION package T) ...), which upstream
;;;    skips because the package exists.
;;;  - GCL 2.6 cannot DEFUN a (SETF NAME) function. MAKE::DEFUN turns
;;;    one into a helper function and a DEFSETF.
;;;  - GCL 2.7 rejects (SETF NAME) in FTYPE proclamations, and prints
;;;    BAD-TYPE for types not defined yet. MAKE::DECLAIM drops FTYPE.
;;;  - GCL 2.6 has no (SETF FDEFINITION). MAKE::FDEFINITION has one.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "MAKE")
    (make-package "MAKE" :nicknames '("MK") :use '("COMMON-LISP")))
  (shadow '("ENSURE-DIRECTORIES-EXIST"
            #+gcl "DEFUN" #+gcl "DECLAIM" #+gcl "FDEFINITION")
          "MAKE")
  ;; CL's until MAKE's own is installed after loading upstream.
  (setf (symbol-function (find-symbol "ENSURE-DIRECTORIES-EXIST" "MAKE"))
        #'ensure-directories-exist))

#+gcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro make::defun (name lambda-list &body body)
    (if (atom name)
        `(cl:defun ,name ,lambda-list ,@body)
        (let ((helper (intern (format nil "(SETF ~A)" (second name))
                              (symbol-package (second name)))))
          `(progn
             (cl:defun ,helper ,lambda-list ,@body)
             (defsetf ,(second name) (&rest args) (new-value)
               (list* ',helper new-value args))
             ',name))))
  (defmacro make::declaim (&rest specifiers)
    `(cl:declaim ,@(remove 'ftype specifiers
                           :key (lambda (s) (and (consp s) (first s))))))
  (cl:defun make::fdefinition (name)
    (symbol-function name))
  (defsetf make::fdefinition (name) (function)
    `(setf (symbol-function ,name) ,function)))

;;; Upstream's default *CENTRAL-REGISTRY* calls PROBE-FILE on a directory,
;;; which is an error in CLISP. DEFVAR leaves a bound variable alone, so
;;; bind it first, to the list upstream would build.
#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((symbol (intern "*CENTRAL-REGISTRY*" "MAKE"))
         (global "/usr/local/lisp/mkdefsystem/Registry/"))
    (unless (boundp symbol)
      (proclaim `(special ,symbol))
      (setf (symbol-value symbol)
            `("./"
              (ext:default-directory)
              ,(concatenate 'string (namestring (user-homedir-pathname))
                            "lisp/systems/")
              ,@(and (ext:probe-directory global)
                     (list (pathname global))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Upstream prints a banner while loading. Earlier MK:DEFSYSTEM did
  ;; not, and Maxima loads it at every ECL or ABCL start.
  (let ((*standard-output* (make-broadcast-stream)))
  (load (merge-pathnames "mk-defsystem.lisp"
                         #.(or (and (boundp '*compile-file-truename*)
                                    *compile-file-truename*)
                               *load-truename*))
        :verbose nil :print nil)))

(defpackage "MAXIMA-DEFSYSTEM"
  (:use "COMMON-LISP")
  (:export "*SEQUENTIAL-DEPENDENCIES*" "*GC-AFTER-COMPILE-GROWTH*"))

(in-package "MAXIMA-DEFSYSTEM")

(defparameter *tested-version* "3.9.4.2 (More Modern), 2026-04-26")

(unless (equal mk::*defsystem-version* *tested-version*)
  (warn "MK:DEFSYSTEM is version ~A, but Maxima's extensions were ~
         tested with ~A." mk::*defsystem-version* *tested-version*))

(defvar *sequential-dependencies* t
  "If true, once OPERATE-ON-SYSTEM has recompiled a file it recompiles
every file it compiles after it, declared dependency or not. A file can
depend on macros, inline functions, constants, structures and types
defined by any file compiled before it.")

(defvar *gc-after-compile-growth* (* 32 1024 1024)
  "Collect garbage after compiling a file that grew the heap by more than
this many bytes. NIL disables it.")

;;; State of the outermost OPERATE-ON-SYSTEM call.
(defvar *in-oos* nil)
(defvar *recompiled* nil)
(defvar *outer-load-verbose* nil)
(defvar *outer-compile-verbose* nil)

(defvar *wrappers* (make-hash-table :test 'eq)
  "Maps an upstream function name to the wrapper installed on it.")

(defun wrap (name wrapper)
  "Make NAME call WRAPPER with NAME's current definition and the arguments."
  (if (not (fboundp name))
      (warn "MK:DEFSYSTEM has no ~S; Maxima extension skipped." name)
      (let ((original (symbol-function name)))
        (unless (eq original (gethash name *wrappers*))
          (let ((new (lambda (&rest args) (apply wrapper original args))))
            (setf (gethash name *wrappers*) new
                  (symbol-function name) new))))))

(defmacro with-outer-verbosity (&body body)
  "Run BODY with the caller's *LOAD-VERBOSE* and *COMPILE-VERBOSE*.
OPERATE-ON-SYSTEM binds both to T."
  `(if *in-oos*
       (let ((*load-verbose* *outer-load-verbose*)
             (*compile-verbose* *outer-compile-verbose*))
         ,@body)
       (progn ,@body)))

(defun heap-bytes-in-use ()
  #+sbcl (sb-kernel:dynamic-usage)
  #+cmu (lisp::dynamic-usage)
  #-(or sbcl cmu) nil)

(defun collect-garbage-after-compile (bytes-before)
  (let ((bytes-now (and bytes-before *gc-after-compile-growth*
                        (heap-bytes-in-use))))
    (when (and bytes-now
               (> (- bytes-now bytes-before) *gc-after-compile-growth*))
      #+sbcl (sb-ext:gc :full t)
      #+cmu (ext:gc :full t))))

(defvar *binary* nil
  "While COMPILE-FILE-OPERATION runs, the binary it is about to write.")

(defun with-binary-device (pathspec)
  "PATHSPEC as a pathname, with the device of *BINARY* when PATHSPEC is
the directory of *BINARY* without a device."
  (let ((pathname (pathname pathspec))
        (binary (and *binary* (pathname *binary*))))
    (if (and binary
             (member (pathname-device pathname) '(nil :unspecific))
             (equal (pathname-directory pathname)
                    (pathname-directory binary)))
        (make-pathname :name nil :type nil :version nil :defaults binary)
        pathname)))

(defun ensure-binary-directory-exists (pathspec &rest args)
  "MAKE's ENSURE-DIRECTORIES-EXIST. Upstream passes the directory of
*BINARY* without its device, which on Windows is the drive."
  (apply #'ensure-directories-exist (with-binary-device pathspec) args))

;;; Replaces upstream's no-op on GCL too.
(setf (symbol-function (find-symbol "ENSURE-DIRECTORIES-EXIST" "MAKE"))
      #'ensure-binary-directory-exists)

(defun same-file-p (a b)
  (and a b (equal (namestring (pathname a)) (namestring (pathname b)))))

(defun load-or-recompile (loader component binary args)
  "Load BINARY with LOADER. If that fails, recompile COMPONENT and
load again once."
  (handler-case (apply loader binary args)
    (error (condition)
      (let ((*print-pretty* nil))
        (format *error-output* "~&MK:DEFSYSTEM: Error loading ~A: ~A~%~
                                ; Deleting binary and recompiling...~%"
                binary condition))
      (handler-case (delete-file binary)
        (error (condition)
          (format *error-output* "~&MK:DEFSYSTEM: Could not delete ~A: ~A~%"
                  binary condition)))
      (mk::compile-file-operation component t)
      (apply loader binary args))))

;;; Installation.

(wrap 'mk::operate-on-system
      (lambda (original &rest args)
        (if *in-oos*
            (apply original args)
            (let ((*in-oos* t)
                  (*recompiled* nil)
                  (*outer-load-verbose* *load-verbose*)
                  (*outer-compile-verbose* *compile-verbose*))
              (apply original args)))))

;;; Upstream makes OOS a copy of the function object, not an alias.
(setf (symbol-function 'mk::oos) (symbol-function 'mk::operate-on-system))

(wrap 'mk::find-system
      (lambda (original &rest args)
        (with-outer-verbosity (apply original args))))

;;; Upstream gives each component without a :DEVICE of its own the device
;;; of *DEFAULT-PATHNAME-DEFAULTS*, and that wins over the device of the
;;; component's pathnames. On Windows the device is the drive, so a file
;;; on another drive than the current directory is looked for on the
;;; current one. Keep only a :DEVICE from the definition, so pathnames
;;; keep their own.
(wrap 'mk::init-component-pathnames
      (lambda (original component parent)
        (let ((device (mk::component-device component)))
          (funcall original component parent)
          (unless device
            (setf (mk::component-device component)
                  (and parent (mk::component-device parent)))))))

(wrap 'mk::load-file-operation
      (lambda (original component force)
        (with-outer-verbosity (funcall original component force))))

(wrap 'mk::compile-file-operation
      (lambda (original component force)
        (when (and *in-oos* *sequential-dependencies* *recompiled*)
          (cond ((eq force :new-source-and-dependents)
                 (setq force :new-source-all))
                ((consp force)
                 (setq force :all))))
        (let* ((*binary* (mk::component-full-pathname component :binary))
               (before (heap-bytes-in-use))
               (compiled (with-outer-verbosity
                           (funcall original component force))))
          (when compiled
            (when *in-oos* (setq *recompiled* t))
            (collect-garbage-after-compile before))
          compiled)))

(wrap 'mk::load-function
      (lambda (original component)
        (let ((loader (funcall original component)))
          (lambda (pathname &rest args)
            (if (and (same-file-p pathname
                                  (mk::component-full-pathname component
                                                               :binary))
                     (probe-file (mk::component-full-pathname component
                                                              :source)))
                (load-or-recompile loader component pathname args)
                (apply loader pathname args))))))
