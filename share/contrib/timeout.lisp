;; stopme command, only for sbcl
(in-package :maxima)

;; stopme and  time_constrained are the same except that
;; time_constrained, in case of timeout simply returns
;; the expression Aborted.
;; stopme gives a message, and returns it. The message contains the unevaluated expr.
;; There are too many other variants possible to cover them all.
;; You may want a slightly different behavior. e.g. return unevaluated expr only.
;; (Mathematica returns "$Aborted" for timeout.)

#+sbcl
(defmspec $stopme(args)	;; usage:  stopme (do x:x+1, 2) ; stops infinite loop after 2 sec
  (let* ((evalme(cadr args))		  ;  not evaluated !
	 (timelimit (meval (caddr args))) ; evaluate the time limit
	 (v (sb-ext::make-timer ;; the timer program
	     (lambda()
	       (let ((msg `((mtext) ,evalme "   timed out after " ,timelimit " seconds")))
	       (mformat t "~m" msg)
	       (throw 'stopme msg ))))))
    (catch 'stopme
      (progn
	(sb-ext::schedule-timer v timelimit)
	(prog1 (meval evalme)
	  (sb-ext::unschedule-timer v))	))))

#+sbcl
(defmspec $time_constrained(args)	;; usage:  stopme (do x:x+1, 2) ; stops infinite loop after 2 sec
  (let* ((evalme(cadr args))		  ;  not evaluated !
	 (timelimit (meval (caddr args))) ; evaluate the time limit
	 (v (sb-ext::make-timer ;; the timer program
	     (lambda()(throw 'stopme '$Aborted
			     ;;evalme ;; just return unevaluated. 
			     ) ))))  
    (catch 'stopme
      (progn
	(sb-ext::schedule-timer v timelimit)
	(prog1 (meval evalme)
	  (sb-ext::unschedule-timer v))	))))

#-sbcl (format t "~%time_constrained and stopme 
are currently unimplemented
in your lisp. 
Available in sbcl versions of Maxima. ~%")
