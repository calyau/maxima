
(in-package "MAKE")

;;define a function USER::USER-INIT, which will run the init code for a set
;;of files which are linked into an image.

(clines "#define init_or_load(fn,file) do {extern int fn(); init_or_load1(fn,file);}  while(0)")

#.  
(let ((files  (make::system-files :maxima))
      (object-path (if (boundp 'object-path) object-path
		     "foo.o")))
    (declare (special object-path))

    (with-open-file (st "maxobjs" :direction :output)
    `(progn
       (clines "object user_init() {")
       (clines #.(Format nil "load(\"~a\");"
		       (namestring (truename "init_max0.lisp"))))

     ,@(sloop::sloop for x  in files
	for f  = (substitute #\_ #\-  (lowcase x))
	for ff =  (namestring (truename (object x)))
	do (princ ff st) (princ " " st)
	collect
	`(clines ,(Format nil "init_or_load(init_~a,\"~a\");" f ff))
	finally
	(princ (namestring (truename "sysinit.o")) st)
	(terpri st)
	)
     (clines #.(Format nil "load(\"~a\");"
		       (namestring (truename "init_max1.lisp"))))
     )
    
    ))

(clines "return Cnil;}")

;; invoke this to initialize maxima.




  
  
