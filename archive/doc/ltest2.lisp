

(load (pathname "../src/nusum"))
(load (pathname "../src/ode2"))
(load (pathname "../src/elim"))
(load (pathname "../src/trgsmp"))
(setf (get 'maxima::%cosh 'maxima::translated) t);;so that kill won't remprop.
(in-package 'maxima)
(time 
  (sloop for v in
	 '(
	   
	   "rtest9"
	   "rtest9a" "rtest10" "rtest11" "rtest12" "rtest13" "rtest13s")
	 do (test-batch (format nil "~a.mac" v))))

























