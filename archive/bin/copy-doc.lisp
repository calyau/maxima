;;; -*- Mode: LISP -*-

;;Since file positioning may not work for your connection to
;;the unix host where the documentation was stored, you must
;;copy the documentation to lisp machine host.
;;Also the error files from the tests, will go to the directory where the
;;the rtest files are stored.  Note there will probably be floating
;;point discrepancies in the rtest results.


(defvar *maxima-host* "rascal:/usr2/maxima/")

(loop for v in '("rtest1" "rtest2" "rtest3" "rtest4" "rtest5" "rtest6" "rtest6a" "rtest6b" "rtest7" "rtest8" "rtest9" "rtest9a" "rtest10" "rtest11" "rtest12" "rtest13"  "rtest13s") do (copy-file
							 (format nil  "~adoc/~a.mac" *maxima-host* v)
							 (format nil "maxima-documentation:maxima;~a.mac" v)))
 
(loop for v in '("macsym" "documentation")	do (copy-file (format nil "~adoc/~a.doc" *maxima-host* v)
							 (format nil "maxima-documentation:maxima;~a.doc" v)))


;;for u in rest(["rtest1","rtest2","rtest3","rtest4","rtest5","rtest6","rtest6a","rtest6b","rtest7","rtest8","rtest9","rtest9a","rtest10","rtest11","rtest12","rtest13","rtest13s"], 0) do (print (["for test suite",u]),batch(concat("maxima-documentation:maxima;",u,".mac"),test));
