;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER -*-

;;; Checks what lisp-utils/defsystem.lisp adds to the upstream
;;; MK:DEFSYSTEM in lisp-utils/mk-defsystem.lisp, using a small
;;; throwaway system. Run it after replacing mk-defsystem.lisp, with
;;; every Lisp at hand, for example
;;;
;;;   sbcl --non-interactive --load lisp-utils/test-defsystem.lisp
;;;   clisp -norc lisp-utils/test-defsystem.lisp
;;;
;;; It prints one PASS or FAIL line per check, then a summary. The
;;; system's files go to defsystem-test/ under MAXIMA_DEFSYSTEM_TEST_DIR,
;;; or else next to this file, and are deleted afterwards. On Windows,
;;; point MAXIMA_DEFSYSTEM_TEST_DIR at a drive other than the current
;;; one to check that files there are found and binaries go there.

(in-package "CL-USER")

(defpackage "DEFSYSTEM-TEST" (:use "COMMON-LISP"))

(in-package "DEFSYSTEM-TEST")

(defvar *failures* 0)
(defvar *checks* 0)
(defvar *compiled* nil
  "Names of the toy files compiled, pushed by the files themselves.")

(defun check (name ok &optional detail)
  (incf *checks*)
  (unless ok (incf *failures*))
  (format t "~&~:[FAIL~;PASS~]: ~A~@[ (~A)~]~%" ok name detail)
  (finish-output))

(defun getenv (name)
  #+sbcl (sb-ext:posix-getenv name)
  #+(or clisp ecl abcl) (ext:getenv name)
  #+gcl (si:getenv name)
  #+ccl (ccl:getenv name)
  #+cmu (cdr (assoc name ext:*environment-list* :test #'string=))
  #+allegro (sys:getenv name)
  #+lispworks (lw:environment-variable name))

(defvar *here*
  (make-pathname :name nil :type nil :version nil :defaults *load-truename*))

(load (merge-pathnames "defsystem.lisp" *here*))

(defvar *dir*
  (let ((dir (getenv "MAXIMA_DEFSYSTEM_TEST_DIR")))
    (merge-pathnames (make-pathname :directory '(:relative "defsystem-test"))
                     (cond ((or (null dir) (zerop (length dir)))
                            *here*)
                           ((find (char dir (1- (length dir))) "/\\")
                            (pathname dir))
                           (t
                            (pathname (concatenate 'string dir "/")))))))

(defun toy-file (name &optional (type "lisp"))
  (make-pathname :name name :type type :defaults *dir*))

(defun write-toy-file (name macro-value)
  (with-open-file (out (toy-file name) :direction :output
                                       :if-exists :supersede)
    (format out "(eval-when (:compile-toplevel)~%  ~
                   (push ~S defsystem-test::*compiled*))~%" name)
    (cond ((string= name "a")
           (format out "(defmacro toy-value () ~D)~%" macro-value))
          ((string= name "d")
           (format out "(defun defsystem-test::d-value () (toy-value))~%")))))

(defun write-toy-system ()
  (ensure-directories-exist *dir*)
  (dolist (name '("a" "b" "c" "d"))
    (write-toy-file name 1))
  ;; Two modules. D uses A's macro without declaring that.
  (eval `(mk:defsystem "defsystem-test"
           :source-pathname ,*dir*
           :binary-pathname ,(merge-pathnames
                              (make-pathname :directory
                                             '(:relative "bin" "sub"))
                              *dir*)
           :source-extension "lisp"
           :components
           ((:module "m1" :source-pathname ""
             :components ((:file "a") (:file "b" :depends-on ("a"))))
            (:module "m2" :source-pathname ""
             :components ((:file "c") (:file "d" :depends-on ("c"))))))))

(defun compile-toy ()
  "Compile the toy system. Return what OPERATE-ON-SYSTEM printed."
  (setq *compiled* nil)
  (let ((*load-verbose* nil)
        (*compile-verbose* nil))
    (with-output-to-string (*standard-output*)
      (mk:oos "defsystem-test" :compile :verbose nil))))

(defun compiled ()
  (sort (copy-list *compiled*) #'string<))

(defun d-binary (&optional (type :binary))
  (mk::component-full-pathname
   (mk:find-component "defsystem-test" '("m2" "d")) type))

(defun toy-result ()
  "What the toy file d computes. That file defines D-VALUE."
  (funcall 'd-value))

(defun chatter (output)
  "Lines of OUTPUT that look like messages from COMPILE-FILE or LOAD."
  (with-input-from-string (in output)
    (loop for line = (read-line in nil)
          while line
          when (or (search "oading" line) (search "ompil" line)
                   (search "rote" line))
            collect line)))

(defun run ()
  (format t "~&MK:DEFSYSTEM ~A, Maxima's extensions tested with ~A~%"
          mk::*defsystem-version*
          (symbol-value (find-symbol "*TESTED-VERSION*" "MAXIMA-DEFSYSTEM")))
  (write-toy-system)
  (let ((output (compile-toy)))
    (check "fresh build compiles every file"
           (equal (compiled) '("a" "b" "c" "d")) (compiled))
    (check "fresh build prints no compile or load messages"
           (null (chatter output)) (first (chatter output))))
  (check "binary directory created" (probe-file (d-binary)))
  (check "result before the change" (eql (toy-result) 1) (toy-result))
  (compile-toy)
  (check "rebuild with nothing changed compiles nothing"
         (null (compiled)) (compiled))
  ;; File write dates have a resolution of a second.
  (sleep 2)
  (write-toy-file "a" 2)
  (compile-toy)
  (check "a changed macro recompiles every later file"
         (equal (compiled) '("a" "b" "c" "d")) (compiled))
  (check "result after the change" (eql (toy-result) 2) (toy-result))
  (sleep 2)
  (with-open-file (out (d-binary) :direction :output :if-exists :supersede)
    (write-line "garbage" out))
  (let ((error (let ((*error-output* (make-broadcast-stream)))
                 (handler-case (progn (compile-toy) nil)
                   (error (condition) condition)))))
    (check "a corrupt binary is recompiled"
           (and (null error) (member "d" (compiled) :test #'equal))
           (or error (compiled))))
  (check "result after recompiling" (eql (toy-result) 2) (toy-result))
  (if (stringp (pathname-device *dir*))
      (check "source and binary keep the drive of the system"
             (every (lambda (type)
                      (equal (pathname-device (d-binary type))
                             (pathname-device *dir*)))
                    '(:source :binary))
             (list (pathname-device *dir*)
                   (pathname-device (d-binary :source))
                   (pathname-device (d-binary :binary))))
      (format t "~&SKIP: drive of the system (its pathname has no device)~%"))
  (let ((device (ignore-errors
                 (pathname-device
                  (let ((maxima-defsystem::*binary*
                          (make-pathname :device "D"
                                         :directory '(:absolute "x" "y")
                                         :name "z" :type "fasl")))
                    (maxima-defsystem::with-binary-device
                        (make-pathname :directory '(:absolute "x" "y"))))))))
    (if device
        (check "binary directory keeps its drive" (equal device "D") device)
        (format t "~&SKIP: drive letters (pathnames here have no device)~%"))))

(unwind-protect (handler-case (run)
                  (error (condition)
                    (check "no unexpected error" nil condition)))
  (ignore-errors (mk:oos "defsystem-test" :delete-binaries))
  (dolist (name '("a" "b" "c" "d"))
    (ignore-errors (delete-file (toy-file name)))))

(format t "~&~D checks, ~D failed~%" *checks* *failures*)
