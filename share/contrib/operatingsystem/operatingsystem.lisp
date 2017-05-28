;;; Functions from http://clocc.sourceforge.net/
;;;
;;; Copyright (C) 1999-2010 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; Added to Maxima by Wolfgang Dautermann



; tested with clisp, ccl, sbcl, ecl
(defun os-chdir (dir)
  "Change the working directory."
  #+allegro (excl:chdir dir)
  #+clisp (ext:cd dir)
  #+cmu (setf (ext:default-directory) dir)
  #+cormanlisp (ccl:set-current-directory dir)
  #+gcl (si::chdir dir)
  #+lispworks (hcl:change-directory dir)
  #+lucid (lcl:working-directory dir)
  #+sbcl (sb-posix:chdir dir)
  #+sbcl (setf *default-pathname-defaults* (sb-ext:native-pathname (format nil "~A~A" (sb-posix:getcwd) "/")))
  #+ccl (ccl:cwd dir)
  #+ecl (si:chdir dir)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl ccl ecl)
  (error 'not-implemented :proc (list 'chdir dir)))

; tested with clisp, ccl, sbcl, ecl
(defun os-mkdir (dir)
  "Create a directory."
  #+allegro (excl:make-directory dir)
  #+clisp (ext:make-directory dir)
  #+cmu (unix:unix-mkdir (directory-namestring dir) #o777)
  #+lispworks (system:make-directory dir)
  #+sbcl (sb-unix:unix-mkdir (directory-namestring dir) #o777)
  #+ccl (ensure-directories-exist dir)
  #+ecl (ensure-directories-exist dir)
  #-(or allegro clisp cmu lispworks sbcl ccl ecl)
  (error 'not-implemented :proc (list 'mkdir dir)))
  
; tested with clisp, ccl, sbcl, ecl
(defun os-rmdir (dir)
  "Delete a directory."
  #+allegro (excl:delete-directory dir)
  #+clisp (ext:delete-directory dir)
  #+cmu (unix:unix-rmdir dir)
  #+sbcl (zerop (sb-posix:rmdir (namestring dir)))
  #+ccl (ccl:delete-directory dir)
  #+ecl (si:rmdir dir)
  #+lispworks
  ;; `lw:delete-directory' is present in LWW 4.1.20 but not on LWL 4.1.0
  (if (fboundp 'lw::delete-directory)
      (lw::delete-directory dir)
      (delete-file dir))
  #-(or allegro clisp cmu lispworks sbcl ecl ccl) (delete-file dir))
  
  
; tested with clisp, ccl, sbcl, ecl
(defun os-getcurrentdirectory ()
  "Return the current directory."
  (namestring
  #+allegro (excl:current-directory)
  #+clisp (ext:default-directory)
  #+cmu (ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #+sbcl (sb-unix:posix-getcwd/)
  #-(or allegro clisp cmu cormanlisp lispworks lucid sbcl) (truename ".")))
  
; The copy function below was written by StackOverflow user user224021 and is licensed
; under CC BY-SA 3.0 (http://creativecommons.org/licenses/by-sa/3.0/).
; http://stackoverflow.com/a/15813006
(defun os-copy-file (from-file to-file)
  (with-open-file (input-stream from-file
                :direction :input
                :element-type '(unsigned-byte 8))
    (with-open-file (output-stream to-file
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :element-type '(unsigned-byte 8))
      (let ((buf (make-array 4096 :element-type (stream-element-type input-stream))))
    (loop for pos = (read-sequence buf input-stream)
       while (plusp pos)
       do (write-sequence buf output-stream :end pos))))))
