(in-package "MAXIMA") 

(set-pathnames)


#+gmp (si::set-gmp-allocate-relocatable t)

(setf (get 'maxima::%cosh 'maxima::translated) t);;so that kill won't remprop.

;(dolist (v  '(|nusum| |ode2| |elim| |trgsmp|)) (aload v))

;; bang on sgc if we have it.
;#+sgc (si::sgc-on t)
;; allocate some more space..
;#+gcl (progn (si::allocate-relocatable-pages 2000 t) (si::allocate 'cfun 200 t)  (si::allocate 'fixnum 200 t) (si::allocate 'cons 400 t) (si::allocate 'symbol 100 t))


;; bang on relocatable bignums
#+gmp(si::set-gmp-allocate-relocatable t)

;;we won't bother collecting the errors

(setq *collect-errors* nil)
(time 
 (sloop with errs = '() for testv in 
	'(
	  "rtestode_murphy1" 
	  "rtestode_murphy2"
          "rtest_sym" 
          "rtest_sym2"
          "rtest_ode1_riccati"
	  "rtest_ode1_abel"
	  )
	do
	(format t "~%Testing ~a.mac" testv)
	(or (errset
	     (progn
	       (setq testresult (rest (test-batch
				       (format nil
					       "~a~a.mac"
					       (if (boundp 'doc-path)
						   doc-path "")
					       testv))))
	     (if testresult
		 (setq errs (append errs (list testresult))))))
	    (progn
	      (setq error-break-file (format nil "~a.mac" testv))
	      (setq errs (append errs (list (list error-break-file "error break"))))
	      (format t "~%Caused an error break: ~a.mac~%" testv)))
	finally (cond ((null errs) (format t "~%No Errors Found"))
		      (t (format t "~%Error summary:~%")
			 (mapcar
			  #'(lambda (x)
			      (format t "Error(s) found in ~a: ~a~%"
				      (first x) (sort (rest x) #'<)))
			  errs)))))
