

(load (pathname "../src/nusum"))
(load (pathname "../src/ode2"))
(load (pathname "../src/elim"))
(load (pathname "../src/trgsmp"))
(setf (get 'maxima::%cosh 'maxima::translated) t);;so that kill won't remprop.
(in-package 'maxima)
(time 
  (sloop for v in
	 '("rtest1" "rtest1a" "rtest2" "rtest3" "rtest4" "rtest5"
	   "rtest6" "rtest6a" "rtest6b" "rtest7" )
	 do (test-batch (format nil "~a.mac" v))))

























