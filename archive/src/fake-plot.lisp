(in-package "MAXIMA")

(setq plot-stream 'plot-stream-fun)
(defun plot-stream-fun  (&rest l)
  (case (car l)
    ((:any-tyi :tyi) (read-char))
    (t  (show (list 'plot-stream-fun l)))))
(defun pointi (x y) 
   (FUNCALL PLOT-STREAM ':plot-POINT X Y)
   (setq pnt-status t last-x x last-y y screen-last-x x screen-last-y y))


(defvar *plot-file* t)
(defun vectori (x y)
  (cond ((not (and (f= screen-last-x last-x)
		   (f= screen-last-y last-y)))
	 (format *plot-file* "~% ~d ~d ~d ~d" last-x last-y x y))
	(t (format *plot-file* "~% ~d ~d" x y)))
	 
;  (FUNCALL PLOT-STREAM ':plot-LINE LAST-X LAST-Y X Y)
  (setq pnt-status t last-x x last-y y screen-last-x x screen-last-y y)
)

(setq char-height 8)
(DEFVAR PLOT-WIDTH 762)
(DEFVAR PLOT-HEIGHT 854)
(DEFVAR CHAR-HEIGHT 12)
(DEFVAR CHAR-WIDTH 8)
(defun send (&rest l)
  (cond ((member :any-tyi l) (print "hi:") (read-char))
	(t   (show (list 'send l)))))




(defun quot (a &rest b)
  (cond ((null b)
	 (quot 1 a))
	
	((null (cdr b))
	 (setq b (car b))
	 (cond ((and (integerp a) (integerp b))
		(values (truncate a b)))
	       (t (if (= b 0) (float most-positive-single-float)
		 ( / a b)))))
	(t (apply 'quot (quot a (car b)) (cdr b)))))


(DEFUN $PLOT_COMMAND (CMD)
  (cond ((and  (consp cmd)
	       (eq ':menu (first cmd))
	       (eq ':value (second (second cmd))))
	 (setq cmd (second (third (second cmd)))))
	(t nil))

;  (IF (AND (LISTP CMD) (EQ ':MENU (FIRST CMD)) (EQ ':KBD (SECOND (SECOND CMD))))
;      (SETQ CMD (THIRD (SECOND CMD))))
  (and (numberp cmd) (>= cmd 0) (setf cmd (code-char cmd)))
  (CASE CMD
    ((#\P #\p) (plot-3d) 'continue)
    ((#\E #\e #\SPACE) (setq $replotting nil) ($ENDGRAPH)
     '$DONE)
    ((#\O #\o) (plot-options-choose)
     ':replot)
    ((#\H #\h) ':HARDCOPY)
    ((#\N #\n) '$DONE)
    ((#\R #\r) (plot-input-choose) :replot)
    ((#\M #\m  #\V #\v #\O #\o) (plot-options-choose)
     ':replot)
    (-1 'continue)
    ((otherwise 'continue))))

#$window:[0,0,700,700]$
