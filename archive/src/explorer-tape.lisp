   
;(fs:set-logical-pathname-host
;  "Cl-maxima-source" ':physical-host (si::parse-host "R20")
;  ':translations '(("maxima" "AUX:<ATP.SCHELTER.MACSYMA>")  ;bout  2000 blocks if you keep sources
;		   ("demo" "PS:<MACSYM.LSP-TEMP>" )  ;small
;		   ))
;;if creating directories with any name you choose is no problem,
;;you might want to use something like the following so that the only
;;changes necessary are to replace "max" by your physical host
;(fs:set-logical-pathname-host
;  "Cl-maxima-source" ':physical-host "MAX"
;  ':translations '(("maxima" "MAXIMA.SOURCE;")  ;about  2000 blocks if you keep sources
;		   ("demo" "MAXIMA.DEMO;" )  ;small
;		   ))

#+ti 
(DEFUN restore-MACSYMA-FILES ()
  (MT::PREPARE-TAPE 6)
  (MT::REWIND)
  (MT::RESTORE-MULTIPLE-FILE :HOST "SYS" :DIRECTORY '("SITE"))
  (MT::RESTORE-MULTIPLE-FILE :HOST "CL-MAXIMA-OBJECT" :DIRECTORY '( "MAXIMA"))
  (MT::RESTORE-MULTIPLE-FILE :HOST "CL-MAXIMA-PATCH" :DIRECTORY '("PATCH"))
  (MT::RESTORE-MULTIPLE-FILE :HOST "MAXIMA-DOCUMENTATION" :DIRECTORY '("TEST"))
  (MT::RESTORE-MULTIPLE-FILE :HOST "MAXIMA-DOCUMENTATION" :DIRECTORY '( "MAXIMA"))
  (MT::RESTORE-MULTIPLE-FILE :HOST "CL-MAXIMA-SOURCE" :DIRECTORY '("MAXIMA"))
  (MT::RESTORE-MULTIPLE-FILE :HOST "CL-MAXIMA-SOURCE" :DIRECTORY '("MAXIMA"))
  (MT::RESTORE-MULTIPLE-FILE :HOST "CL-MAXIMA-SOURCE" :DIRECTORY '("MAXIMA"))
  (MT::REWIND)
  (MT::UNLOAD))

;;eval the logical defs above first!

#+ti
(defun dump-macsyma (&optional (sources t) verify &aux (fs:character-file-types
							 (append '("ERR" "MAC" ) fs:character-file-types
							 )))
   (mt::prepare-tape 6)
   (mt::backup-file-named "sys:site;cl-maxima.system")
   (mt::write-eof)
   (mt::multiple-dump-files "cl-maxima-object:maxima;*.*.0")
   (mt::write-eof)
   (mt::multiple-dump-files "cl-maxima-patch:patch;*.*.0")
   (mt::write-eof)
   (mt::multiple-dump-files "maxima-documentation:test;*.*.0")
   (mt::write-eof)
   (mt::multiple-dump-files "maxima-documentation:maxima;*.*.0")
   (mt::write-eof)
   (cond (sources
	  (loop for v in (si:system-source-files 'cl-maxima)
		do (mt::backup-file-named v))))
    (mt::backup-file-named "cl-maxima-source:maxima;explorer-changes.lisp")
   (mt::multiple-dump-files "cl-maxima-source:maxima;*.mac.0")
   (mt::backup-file-named "cl-maxima-source:maxima;cl-maxima-sysdef.lisp.0")
   (mt::write-eof)
   (mt::rewind)
   (with-open-file (st "max:wfs;maxima-part-dump.lst" :out)
     (global::print-herald st)
     (format st "~2%Listing of dump made on") (time::print-current-time st)
     (loop for i below 8 do (format st "~2%Contents of section ~A are:~3%" i)
       (cond (verify
	      (let ((*standard-output* st))
		(format t "~%Compare made with originals:")
	      (mt::verify-multiple-file )))
	     (t(mt::list-contents st)
	   ))))
   (mt::rewind)
   (mt::unload)
   )


;;;finally here is the actual pointer to the system definition.
;(si:set-system-source-file "CL-MAXIMA" "cl-maxima-source:maxima;cl-maxima-sysdef.lisp")





(defun verify-sources ()
  (with-open-file (st "max:wfs;maxima-part-dump.lst" :out)
    (loop for i below 8 do (format st "~2%Contents of section ~A verified are:~3%" i)
	  (let ((*standard-output* st))
	    ;; (unless (memq i '(5)) (mt::skip-to-double-eof))
	    (mt::verify-multiple-file )))
    ))


;;useful for skipping over a set of files, 
;;I don't know what the proper function for this is
mt::
(defun skip-to-double-eof (&optional (times 1))
  "skips over a logical set of files separted by double eof, like list-contents and not much faster"
  (loop for i below times
	with stream 
	do
	(loop do  (SETQ STREAM (MAKE-MT-FILE-STREAM ':DIRECTION ':INPUT ':ERROR NIL ':unit *CURRENT-UNIT*))
	  (cond	((streamp stream)
;		 (display-file-header stream standard-output)
		 (mt::space-to-eof)
		 (send stream :close :raw))
		(t (return 'done))))))

