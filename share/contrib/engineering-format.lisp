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

(defun engineering-format (x)
  (if (= x 0.0)
    (format nil "~e" x)
    (let*
      ((integer-log (floor (/ (log (abs x)) (log 10.0))))
       (scale (1+ (mod integer-log 3)))
       (effective-fpprintprec (if (= $fpprintprec 0) 16 $fpprintprec))
       (digits (1- effective-fpprintprec)))
      (declare (special $fpprintprec))
      (format nil "~,v,,ve" digits scale x))))

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
