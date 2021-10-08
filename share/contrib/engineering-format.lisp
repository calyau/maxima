;; copyright 2014 by Robert Dodier
;; I release this work under terms of the GNU GPL
;; Small additions 2018 by Gunter Königsmann

;; Format floats for display with exponent which is a multiple of 3.
;; fpprintprec is honored. The global flag engineering_format_floats
;; (true by default) enables this format which can be disabled for
;; certain number ranges using engineering_format_min and
;; engineering_format_max.
;;
;; If engineering format floats aren't welcome for numbers
;; between 0.01 and 1000 this can be signalled to engineering-format
;; by setting
;;
;;   engineering_format_min: .01$
;;   engineering_format_max: 1000$
;;
;; Example:
;;
;; load ("engineering-format.lisp");
;; for fpprintprec : 2 thru 6 do print (makelist (float(%pi) * 10^n, n, -10, 10));
;;
;; [310.0E-12, 3.1E-9, 31.0E-9, 310.0E-9, 3.1E-6, 31.0E-6, 310.0E-6, 3.1E-3, 
;; 31.0E-3, 310.0E-3, 3.1E+0, 31.0E+0, 310.0E+0, 3.1E+3, 31.0E+3, 310.0E+3, 
;; 3.1E+6, 31.0E+6, 310.0E+6, 3.1E+9, 31.0E+9] 
;; [314.0E-12, 3.14E-9, 31.4E-9, 314.0E-9, 3.14E-6, 31.4E-6, 314.0E-6, 3.14E-3, 
;; 31.4E-3, 314.0E-3, 3.14E+0, 31.4E+0, 314.0E+0, 3.14E+3, 31.4E+3, 314.0E+3, 
;; 3.14E+6, 31.4E+6, 314.0E+6, 3.14E+9, 31.4E+9] 
;; [314.2E-12, 3.142E-9, 31.42E-9, 314.2E-9, 3.142E-6, 31.42E-6, 314.2E-6, 
;; 3.142E-3, 31.42E-3, 314.2E-3, 3.142E+0, 31.42E+0, 314.2E+0, 3.142E+3, 
;; 31.42E+3, 314.2E+3, 3.142E+6, 31.42E+6, 314.2E+6, 3.142E+9, 31.42E+9] 
;; [314.16E-12, 3.1416E-9, 31.416E-9, 314.16E-9, 3.1416E-6, 31.416E-6, 314.16E-6, 
;; 3.1416E-3, 31.416E-3, 314.16E-3, 3.1416E+0, 31.416E+0, 314.16E+0, 3.1416E+3, 
;; 31.416E+3, 314.16E+3, 3.1416E+6, 31.416E+6, 314.16E+6, 3.1416E+9, 31.416E+9] 
;; [314.159E-12, 3.14159E-9, 31.4159E-9, 314.159E-9, 3.14159E-6, 31.4159E-6, 
;; 314.159E-6, 3.14159E-3, 31.4159E-3, 314.159E-3, 3.14159E+0, 31.4159E+0, 
;; 314.159E+0, 3.14159E+3, 31.4159E+3, 314.159E+3, 3.14159E+6, 31.4159E+6, 
;; 314.159E+6, 3.14159E+9, 31.4159E+9] 

(defmvar $engineering_format_floats t)
(defmvar $engineering_format_min 0.0)
(defmvar $engineering_format_max 0.0)

(defvar *debug-eng-format* nil
  "Set to non-NIL to enable some debugging prints for engineering format")

(defun engineering-format (x)
  (if (= x 0.0)
      (format nil "~e" x)
      (flet ((log10 (x)
	       ;; Cmucl has an accurate implementation of log10, which
	       ;; can be accessed via (log x 10).  For all other
	       ;; lisps, we can use it too instead of doing
	       ;; log(x)/log(10) which can cause an extra round-off.
	       (let* ((integer-log (floor (log x 10.0))))
		 ;; Let n = integer-log.  If things worked correctly, then 10^n
		 ;; <= x < 10^(n+1).  However if some rounding errors occurred,
		 ;; it's possible that n is too small.  Check for that and adjust
		 ;; n appropriately.
		 ;;
		 ;; This assumes (expt 10d0 n) works accurately.  We can't do
		 ;; (expt 10 n) because (expt 10 -7) is actually slightly larger
		 ;; than 1d-7.
		 (unless (and (<= (expt 10d0 integer-log)
				  x)
			      (< x (expt 10d0 (1+ integer-log))))
		   (incf integer-log))
		 integer-log)))
	(let* ((integer-log (log10 x))
               (scale (1+ (mod integer-log 3)))
               (effective-fpprintprec (if (= $fpprintprec 0) 16 $fpprintprec))
               (digits (1- effective-fpprintprec))
               (result (format nil "~,v,,ve" digits scale x)))
          (declare (special $fpprintprec))
	  (flet ((maybe-fix-up-result ()
		   ;; Check that the printed result does what we want.  This
		   ;; accounts for some roundoff in lisp's format function.
		   ;; For example, with ecl, printing 1d5 produces the wrong
		   ;; thing despite ecl correctly computing correct integer-log
		   ;; of 5.  (format nil "~,v,,ve" 15 3 1d5) => 1000.0e2.  This
		   ;; should be 100.0e3.  The following code checks for this
		   ;; condition and tries to correct for it.
		   (let* (
			  ;; Find the location of the decimal point in the printed result.
			  (dot-posn (position #\. result :test #'char-equal))
			  ;; Find the value of the exponent in the printed result.
			  (expo (parse-integer result
					       :start (1+ (position #\e result
								    :test #'char-equal)))))
		     (unless (and (< dot-posn 4)
				  (zerop (rem expo 3)))
		       (when *debug-eng-format*
			 (format t "Expo ~A posn ~A: Result is wrong:  ~A~%" expo dot-posn result))
	  
		       ;; For all the cases I've seen, if the exponent is not a
		       ;; multiple of 3, decrementing the scale by 1 will get
		       ;; lisp to print out the right thing.  Except ccl64:
		       ;; (format t "~,v,,ve" 15 2 1d10) => 10d+8.  The printed
		       ;; result isn't 1d10.
		       (setf result (format nil "~,v,,ve" digits (1- scale) x))

		       (when *debug-eng-format* 
			 (format t "New result: ~A~%" result))))))

	    (when *debug-eng-format*
              (format t "X = ~A log = ~A scale = ~A digits ~A result ~A~%"
                      x integer-log scale digits result))

	    (maybe-fix-up-result)
            result)))))

(let ((foo (symbol-function 'exploden)))
  (defun exploden (x)
    (if (and (floatp x) $engineering_format_floats
             (or (< (abs x) $engineering_format_min)
                 (> (abs x) $engineering_format_max)))
      (let ((s (engineering-format x)) s1)
        (declare (special *exploden-strip-float-zeros*))
        (setq s1 (if *exploden-strip-float-zeros* (or (strip-float-zeros s) s) s))
        (funcall foo s1))
      (funcall foo x))))
