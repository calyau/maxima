(in-package "MAXIMA")
#|
Sample usage:
(C24) f(x):=2^x+3;
					 X
(D24) 			        F(X) := 2  + 3
(C25) nutmeg_plot_fun(f,-2,2);
Loading raw file 'max.raw' ... done.

Title:  Maxima plot
Name: ourplot
Date: Sun Oct 29 17:51:15 1989


This is a sample news file, and will be printed
whenever spice or nutmeg is started.

nutmeg 1 -> plot y
nutmeg 2 -> 
quit
So long.
(D25) 		     #/rascal/public/tmp/spice3c1/max.raw
|#

;; The left button if held down while moving between two points,
;; will print the difference.
;; 

(defun print-date (&optional(stream *standard-output*)
			    (time (get-universal-time)))
  (multiple-value-bind (sec min hr day mon yr wkday)
		       (decode-universal-time time)
	(format stream "~a ~a ~a ~d:~2,'0d:~2,'0d ~a"
		(nth wkday '( "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
		(nth (1- mon) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
			   "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
		day
		hr min sec yr)))
  
(defun plot-points (variables pts &optional (st *standard-output* )
			      &uax (npoints (length (elt pts 0))))
  (format st "~%Title: Maxima plot")
  (format st "~%Date: ~a" (print-date nil))
  (format st "~%Plotname: ourplot")
  (format st "~%Flags: real")
  (format st "~%No. Variables: ~a ~%No. Points: ~a" (length variables) npoints)
  (format st "~%Variables:")
  (sloop for i from 0 for v in variables
	 do (format st "~%~a ~a ~a" i v v))
  (format st "~%Values:")
  (sloop for j below npoints
	 do (format st "~%~d   " j)
	 (cond ((arrayp (car pts))
		(sloop for v in pts
		       do (princ (float (aref v j)) st)
		       (princ " " st)
		       )))))
	       
(defun $nutmeg_plot_fun (f min max &aux file )
  (let ((x (make-array $plotnum))
	(y (make-array $plotnum)))
    (translate-function f)
    (sloop for i below $plotnum 
	   with del = (/ (- max min) (float $plotnum))
	   do
	   (setf (aref x i) (+ min (* i del)))
	   (setf (aref y i) (funcall f (aref x i))))
    (with-open-file(st "max.raw" :direction :output)
		   (plot-points '(x y) (list x y) st)
		   (setq file (truename st)))
    (system "nutmeg max.raw")
    file))


(defun $nutmeg_plot(vars vals)
  (setq vals
	(sloop for v in (cdr vals)
	       collect (cond ((listp v) (coerce v 'vector))
			     (t v))))
  (plot-points (cdr variables) (cdr pts)))
  



  