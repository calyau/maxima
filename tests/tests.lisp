(in-package "MAXIMA") 
(set-pathnames)

;(load (pathname "../src/nusum"))
;(load (pathname "../src/ode2"))
;(load (pathname "../src/elim"))
;(load (pathname "../src/trgsmp"))
 


(setf (get 'maxima::%cosh 'maxima::translated) t);;so that kill won't remprop.
(in-package 'maxima)
(dolist (v  '(|nusum| |ode2| |elim| |trgsmp|)) (aload v))

;; bang on sgc if we have it.
#+sgc (si::sgc-on t)

;;we won't bother collecting the errors
;(setq *collect-errors* nil)
(time 
  (sloop for v in
	 '(
	   "rtest1" "rtest1a" "rtest2" "rtest3" "rtest4" "rtest5"
	   "rtest6" "rtest6a" "rtest6b" "rtest7"
	   "rtest8"
	   "rtest9"
	   "rtest9a" "rtest10" "rtest11" "rtest12" ")rtest13" "rtest13s"
	   "rtest14"
	   )
	 do
	 (format t "~%Testing ~a.mac" v)
	 (or (errset (test-batch (format nil
			     "~a~a.mac" (if (boundp 'doc-path) doc-path "")
			     v)))
	     (format t "~%Caused a error break: ~a.mac" v))
	 
	 ))
;;check the run command
(macsyma-top-level)

























