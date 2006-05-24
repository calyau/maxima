;;; -*-  Mode: Lisp -*-                                                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;;  Author:  Andrej Vodopivec <andrejv@users.sourceforge.net>                ;;
;;  Licence: GPL2                                                            ;;
;;                                                                           ;;
;;  Usage:                                                                   ;;
;;   implicit_plot(expr, xrange, yrange, [options]);                         ;;
;;      Plots the curve `expr' in the region given by `xrange' and `yrange'. ;;
;;   `expr' is a curve in plane defined in implicit form or a list of such   ;;
;;   curves. If `expr' is not an equality, then `expr=0' is assumed. Works   ;;
;;   by tracking sign changes, so it will fail if expr is something like     ;;
;;   `(y-x)^2=0'.                                                            ;;
;;      Optional argument `options' can be anything that is recognized by    ;;
;;   `plot2d'. Options can also be set using `set_plot_option'.              ;;
;;      Works only with gnuplot!                                             ;;
;;                                                                           ;;
;;  Examples:                                                                ;;
;;   implicit_plot(y^2=x^3-2*x+1, [x,-4,4], [y,-4,4],                        ;;
;;                 [gnuplot_preamble, "set zeroaxis"])$                      ;;
;;   implicit_plot([x^2-y^2/9=1,x^2/4+y^2/9=1], [x,-2.5,2.5], [x,-3.5,3.5]); ;;
;;   implicit_plot(x^2+2*y^3=15, [x,-10, 10], [y,-5,5])$                     ;;
;;   implicit_plot(x^2*y^2=(y+1)^2*(4-y^2), [x,-10, 10], [y,-3,3]);          ;;
;;   implicit_plot(x^3+y^3 = 3*x*y^2-x-1, [x,-4,4], [y,-4,4]);               ;;
;;   implicit_plot(x^2*sin(x+y)+y^2*cos(x-y)=1, [x,-10,10], [y,-10,10]);     ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module implicit_plot)

(defmvar $ip_grid `((mlist simp)  75  75)
  "Grid for the first sampling.")
(defmvar $ip_grid_in `((mlist simp)  10  10)
  "Grid for the second sampling.")

(defmvar $ip_epsilon  0.0000001
  "Epsilon for implicit plot routine.")

(defun contains-zeros (i j sample)
  (not (and (> (* (aref sample i j) (aref sample (1+ i)     j  )) 0)
	    (> (* (aref sample i j) (aref sample     i  (1+ j) )) 0)
	    (> (* (aref sample i j) (aref sample (1+ i) (1+ j) )) 0) )))

(defun sample-data (expr xmin xmax ymin ymax sample grid)
  (let* ((xdelta (/ (- xmax xmin) ($first grid)))
	 (ydelta (/ (- ymax ymin) ($second grid))))
    (do ((x-val xmin (+ x-val xdelta))
	 (i 0 (1+ i)))
	((> i ($first grid)))
      (do ((y-val ymin (+ y-val ydelta))
	   (j 0 (1+ j)))
	  ((> j ($second grid)))
	(let ((fun-val (funcall expr x-val y-val)))
	  (if (or (eq fun-val t) (>= fun-val $ip_epsilon))
	      (setf (aref sample i j) 1)
	      (setf (aref sample i j) -1)))))))

(defun print-mid (file point xmin xdelta ymin ydelta)
  (let ((x (+ xmin (/ (* xdelta (+ (car point) (caddr point))) 2)))
	(y (+ ymin (/ (* ydelta (+ (cadr point) (cadddr point))) 2))))
    (format file "~f ~f~%" x y)))

(defun print-square (file xmin xmax ymin ymax sample grid)
  (let* ((xdelta (/ (- xmax xmin) ($first grid)))
	 (ydelta (/ (- ymax ymin) ($second grid))))
    (do ((i 0 (1+ i)))
	((>= (+ i 2) ($first grid)))    
      (do ((j 0 (1+ j)))
	  ((>= (+ j 2) ($second grid)))
	(if (contains-zeros i j sample)
	    (let ((points ()))
	      (if (< (* (aref sample i j) (aref sample (1+ i) j)) 0)
		  (setq points (cons `(,i ,j ,(1+ i) ,j) points)))
	      (if (< (* (aref sample (1+ i) j) (aref sample (1+ i) (1+ j))) 0)
		  (setq points (cons `(,(1+ i) ,j ,(1+ i) ,(1+ j)) points)))
	      (if (< (* (aref sample i (1+ j)) (aref sample (1+ i) (1+ j))) 0)
		  (setq points (cons `(,i ,(1+ j) ,(1+ i) ,(1+ j)) points)))
	      (if (< (* (aref sample i j) (aref sample i (1+ j))) 0)
		  (setq points (cons `(,i ,j ,i ,(1+ j)) points)))
	      (print-mid file (car points) xmin xdelta ymin ydelta)
	      (print-mid file (cadr points) xmin xdelta ymin ydelta)
	      (format file "~%")))))
    ))

(defun $implicit_plot (expr xrange yrange &rest options)
  (let* (($numer t) ($plot_options $plot_options)
	 (plot-name)
	 (i 0)
	 (xmin ($second xrange))
	 (xmax ($third xrange))
	 (xdelta (/ (- xmax xmin) ($first $ip_grid)))
	 (ymin ($second yrange))
	 (ymax ($third yrange))
	 (ydelta (/ (- ymax ymin) ($second $ip_grid)))
	 (sample (make-array `(,(1+ ($first $ip_grid))
			       ,(1+ ($second $ip_grid)))))
	 (ssample (make-array `(,(1+ ($first $ip_grid_in))
				,(1+ ($second $ip_grid_in)))))
	 file-name gnuplot-out-file gnuplot-term)
    
    (dolist (v options) ($set_plot_option v))
    (setq xrange (check-range xrange))
    (setq yrange (check-range yrange))
    
    (if (not ($listp expr))
	(setq expr `((mlist simp) ,expr)))

    (setf gnuplot-term ($get_plot_option '$gnuplot_term 2))
    
    (if ($get_plot_option '$gnuplot_out_file 2)
	(setf gnuplot-out-file (get-plot-option-string '$gnuplot_out_file)))
    
    (if (and (eq gnuplot-term '$default) 
	     gnuplot-out-file)
	(setf file-name gnuplot-out-file)
	(setf file-name (plot-temp-file "maxout.gnuplot")))
    
    ;; output data
    (with-open-file
	(file file-name :direction :output :if-exists :supersede)
      (gnuplot-print-header file)
      (format file "set xrange [~d:~d]~%" (caddr xrange) (cadddr xrange))
      (format file "set yrange [~d:~d]~%" (caddr yrange) (cadddr yrange))
      (format file "set style data lines~%")
      (format file "plot")
      (dolist (v (cdr expr))
	(incf i)
	(setq plot-name
	      (let ((string ""))
		(if (atom v) 
		    (setf string (coerce (mstring v) 'string))
		    (setf string (coerce (mstring v) 'string)))
		(if (< (length string) 80)
		    string
		    (format nil "fun~a" i))))
	(if (> i 1)
	    (format file ","))
	(let ((title (get-plot-option-string '$gnuplot_curve_titles i)))
	  (if (equal title "default")
	      (setf title (format nil "title '~a'" plot-name)))
	  (format file " '-' ~a ~a" title 
		  (get-plot-option-string '$gnuplot_curve_styles i))))
      (format file "~%")
      (dolist (e (cdr expr))
	(setq e (coerce-float-fun ($float (m- ($rhs e) ($lhs e)))
				  `((mlist simp)
				    ,($first xrange)
				    ,($first yrange))))
	(sample-data e xmin xmax ymin ymax sample $ip_grid)
	(do ((i 0 (1+ i)))
	    ((= (1+ i) ($first $ip_grid)))
	  (do ((j 0 (1+ j)))
	      ((= (1+ j) ($second $ip_grid)))
	    (if (contains-zeros i j sample)
		(let* ((xxmin (+ xmin (* i xdelta)))
		       (xxmax (+ xxmin xdelta xdelta))
		       (yymin (+ ymin (* j ydelta)))
		       (yymax (+ yymin ydelta ydelta)))
		  (sample-data e xxmin xxmax yymin yymax
			       ssample $ip_grid_in)
		  (print-square file xxmin xxmax yymin yymax
				ssample $ip_grid_in) )) ))
	(format file "e~%") ))
    
    ;; call gnuplot
    (gnuplot-process file-name) ))
