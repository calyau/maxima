(load "sysdef.lisp")
(load "version.lisp")
(SYSTEM:ALLOCATE-CONTIGUOUS-PAGES (round 1100 (/ si::lisp-pagesize 2048)) t)
(make::make :maxima)
;; clean up a bit.
(sloop::sloop for v in-package 'make do (unintern v))
(remprop :maxima :make)
(remprop :minima :make)
(sloop::sloop for v in-package 'maxima do (remprop v 'si::debug))
(load #"../src/autol.lisp") 
(load #"../src/max_ext.lisp")
(gbc t)
(gbc 1)
(defun run () (maxima::macsyma-top-level))
(defun system::kcl-top-level nil
  (when (> (system:argc) 1)
        (setq system:*system-directory* (system:argv 1)))
  (when (>= (system:argc) 5)
        (let ((system::*quit-tag* (cons nil nil))
              (system::*quit-tags* nil) (system::*break-level* '())
              (system::*break-env* nil) (system::*ihs-base* 1)
              (system::*ihs-top* 1) (system::*current-ihs* 1)
              (*break-enable* nil))
             (system:error-set
              '(let ((system::flags (system:argv 4)))
                    (setq system:*system-directory*
                          (pathname (system:argv 1)))
                    (compile-file (system:argv 2) :output-file
                     (system:argv 3) :o-file
                     (case (schar system::flags 1) (#\0 nil) (#\1 t)
                           (t (system:argv 5)))
                     :c-file
                     (case (schar system::flags 2) (#\0 nil) (#\1 t)
                           (t (system:argv 6)))
                     :h-file
                     (case (schar system::flags 3) (#\0 nil) (#\1 t)
                           (t (system:argv 7)))
                     :data-file
                     (case (schar system::flags 4) (#\0 nil) (#\1 t)
                           (t (system:argv 8)))
                     :system-p
                     (if (char-equal (schar system::flags 0) #\S) t
                         nil))))
             (bye (if compiler::*error-p* 1 0))))
  (format t "AKCL (Austin Kyoto Common Lisp)  ~A~%~a~%" "DATE"
        "Maxima 4.0 (with enhancements by W. Schelter).
Type (run) to start Maxima.")
     (setq si::*ihs-top* 1)

  (in-package 'system::user) (incf system::*ihs-top* 2)
  (funcall system::*old-top-level*))

(in-package "MAXIMA")
(setf $version (cons '(mlist) (get :maxima :version)))

 (setq *DESCRIBE-INDEX-DIRECTORY*
      (namestring
       (make-pathname :directory
		      (pathname-directory $DESCRIBE_DOCUMENTATION))))


; (setq maxima::$file_search (list '(maxima::mlist)
;    #"SYSDIR/share/foo.o"
;    #"SYSDIR/share/foo.lisp"
;    #"SYSDIR/share1/foo.o"     #"SYSDIR/share1/foo.lisp"
;    #"SYSDIR/tensor/foo.o"     #"SYSDIR/tensor/foo.lisp"
;))

 (setq maxima::$file_search (list '(maxima::mlist)
    #"SYSDIR/[stm][hryea]*/foo.[lmo]*"))

 )

(si::save-system "../unixport/saved_maxima")



