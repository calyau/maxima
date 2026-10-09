;; Round 2, does not work as is. load() of a Lisp source file loads it as
;; before, then compiles it into a fasl under maxima_objdir, and later loads
;; use that fasl while it is newer than the source. Compiling before the load
;; would miscompile fourier_elim.lisp, which loads to_poly.lisp, home of its
;; OPAPPLY macro, only at load time. Compiling after the load gets that right
;; but lets declarations later in a file apply to forms before them, and the
;; full suite then fails in rtest_itensor (heap exhausted in DELTA). Files that
;; mention their load pathname, or do not compile without warnings, are never
;; cached.
(in-package :maxima)

(defun fasl-cache-path (source)
  (let ((dir (pathname-directory (truename source))))
    (merge-pathnames
     (make-pathname :directory (append (pathname-directory
					(pathname (combine-path *maxima-objdir* "")))
				       '("fasl-cache") (rest dir))
		    :name (pathname-name source))
     (compile-file-pathname "x.lisp"))))

(defun source-mentions-load-path-p (source)
  (with-open-file (s source :external-format :utf-8)
    (let ((text (make-string (file-length s))))
      (setq text (subseq text 0 (read-sequence text s)))
      (some (lambda (k) (search k text :test #'char-equal))
	    '("load-pathname" "load-truename" "load_pathname")))))

(defun fresh-cached-fasl (source)
  (ignore-errors
   (let ((fasl (fasl-cache-path source)))
     (and (probe-file fasl)
	  (> (file-write-date fasl) (file-write-date source))
	  fasl))))

(defun cache-fasl (source)
  (ignore-errors
   (unless (source-mentions-load-path-p source)
     (let ((fasl (fasl-cache-path source))
	   (null (make-broadcast-stream)))
       (ensure-directories-exist fasl)
       (multiple-value-bind (out warnings-p failure-p)
	   (let ((*standard-output* null) (*error-output* null)
		 (*compile-verbose* nil) (*compile-print* nil))
	     (with-compilation-unit (:override t)
	       (compile-file source :output-file fasl)))
	 (declare (ignore warnings-p))
	 (when (and out failure-p)
	   (delete-file out)))))))

(defun loadfile (file findp printp)
  (and findp (member $loadprint '(nil $loadfile) :test #'equal) (setq printp nil))
  ;; Should really get the truename of FILE.
  (if printp (format t (intl:gettext "loadfile: loading ~A.~%") file))
  (let* ((path (pathname file))
	 (*package* (find-package :maxima))
	 ($load_pathname path)
	 (*read-base* 10.)
	 (*print-base* 10.)
	 (lisp-source-p (equalp (pathname-type path) "lisp"))
	 (fasl (and lisp-source-p (fresh-cached-fasl path)))
	 (tem (errset (with-compilation-unit nil (load (or fasl path))))))
    (or tem (merror (intl:gettext "loadfile: failed to load ~A") (namestring path)))
    (when (and lisp-source-p (not fasl))
      (cache-fasl path))
    (namestring path)))
