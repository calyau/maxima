#| Existing documentation from the reference manual:

 -- Function: plot2d_ps (<expr>, <range>)
     Writes to pstream a sequence of PostScript commands which plot
     <expr> over <range>.

     <expr> is an expression.  <range> is a list of the form `[<x>,
     <min>, <max>]' in which <x> is a variable which appears in <expr>.

     See also `closeps'.

 -- Function: closeps ()
     This should usually becalled at the end of a sequence of plotting
     commands.   It closes the current output stream <pstream>, and sets
     it to nil.   It also may be called at the start of a plot, to
     ensure pstream is closed if it was open.    All commands which
     write to pstream, open it if necessary.   `closeps' is separate
     from the other plotting commands, since we may want to plot 2
     ranges or superimpose several plots, and so must keep the stream
     open.

 -- Function: psdraw_curve (<ptlist>)
     Draws a curve connecting the points in <ptlist>.   The latter may
     be of the form `[x0, y0, x1, y1, ...]' or `[[x0, y0], [x1, y1],
     ...]'

     The function `join' is handy for taking a list of x's and a list
     of y's and splicing them together.

     <psdraw_curve> simply invokes the more primitive function
     <pscurve>.   Here is the definition:

          (defun $psdraw_curve (lis)
            (p "newpath")
            ($pscurve lis)
            (p "stroke"))

 -- Function: pscom (<cmd>)
     <cmd> is inserted in the PostScript file.  Example:
          pscom ("4.5 72 mul 5.5 72 mul translate 14 14 scale");

   End of stuff from the reference manual.
 |#

                                        #|
Here is what the user inputs to draw the lattice picture.

/*Initially 1 unit = 1 pt = 1/72 inch
This makes 1 unit be 50/72 inch
*/
ps_scale:[50,50]                        ;

/*This moves the origin to 400/72 inches up and over from bottom left corner
[ie roughly center of page]
*/
ps_translate:[8,8]                      ;


f(x):=if (x = 0) then 100  else 1/x     ;

foo():=block([],
closeps(),
ps_translate:[6,6],
ps_scale:[50,50],
psdraw_curve(join(xcord,map(f,xcord))),
psdraw_curve(join(-xcord,map(f,xcord))),
psdraw_curve(join(xcord,-map(f,xcord))),
psdraw_curve(join(-xcord,-map(f,xcord))),
psdraw_points(lattice),
psaxes(8))                              ;


And here is the output .ps file which you should be
able to print on a laserwriter, or view on screen if you have
ghostscript (or another postscript screen previewer).

|#

;;(defvar $viewps_command  "(gs -I. -Q  ~a)")

;;(defvar  $viewps_command "echo /def /show {pop} def |  cat - ~a | x11ps")

;; If your gs has custom features for understanding mouse clicks


;;Your gs will loop for ever if you don't have showpage at the end of it!!
;;(defvar $viewps_command   "echo '/showpage { .copypage readmouseclick /ke exch def ke 1 eq { erasepage initgraphics} {ke 5 ne {quit} if} ifelse} def  {(~a) run } loop' | gs  -title 'Maxima  (click left to exit,middle to redraw)' > /dev/null 2>/dev/null &")

(defvar $viewps_command  "(ghostview \"~a\")")

(defvar $window_size '((mlist)
                       #.(* 8.5 72) #. (* 11 72) ))

(defun $getrange (x &optional xrange &aux yrange)
  (setq yrange  (cdr (get-range (cddr x))))
  (or xrange (setq xrange (cdr (get-range (cdr x)))))
  (setup-for-ps-range xrange yrange nil))
 
(defun $paramplot (f g range &optional (delta .1 supplied) &aux pts ($numer t))
  (setq f (coerce-float-fun f))
  (setq g (coerce-float-fun g))
  (setq range (meval* range))
  (or supplied (setq delta (/ (- (nth   2 range) (nth 1 range)) (nth 2 ($get_plot_option '$nticks)))))
  (setq pts(cons '(mlist)
                 (loop with tt = (coerce-float (nth 1 range))
                        with end = (coerce-float (nth 2 range))
                        while (float-< tt end)
                        collect (funcall f tt)
                        collect (funcall g tt)
                        do (setq tt ($+ tt delta)))))
  ($closeps)
  ($getrange pts)
  ($psdraw_curve pts)
  ($closeps)
  ($viewps))
  
(defun $plot2d_ps(fun range &rest options &aux ($numer t)  $display2d
                  ($plot_options $plot_options))
  (dolist (v options) ($set_plot_option v))
  (setq range (check-range range))
  (let ((tem (draw2d fun range )))
    ($closeps)
    ($getrange tem (cddr range))
    ($psdraw_curve tem)
    ($psaxes ($rest range))
    (p "showpage") ;; IS THIS NEEDED ??? ($viewps WILL APPEND A showpage TOO.)
    ($viewps)))

;; do-ps-created-date was cribbed from the Common Lisp Cookbook -- Dates and Times.
;; See: http://cl-cookbook.sourceforge.net/dates_and_times.html

(defun do-ps-created-date (my-stream)
  (let ((day-names #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))
    (multiple-value-bind 
          (second minute hour date month year day-of-week dst-p tz)
        (get-decoded-time)
      (declare (ignore dst-p))
      (format my-stream "%%CreatedDate: ~2,'0d:~2,'0d:~2,'0d ~a, ~d/~2,'0d/~d (GMT~@d)~%"
              hour minute second (aref day-names day-of-week) month date year (- tz)))))


(defun do-ps-trailer ()
  (p "showpage")
  (p "%%Trailer")
  (p "%%EOF"))

;;When we initialize we move the origin to the middle of $window_size
;;Then to offset from that use translate.
(defvar $ps_translate '((mlist) 0 0))

;; initially 1/72 of inch is the scale
(defvar $ps_scale '((mlist) 72  72))


(defun $pscom (&rest l)
  (apply 'p l))

;;-10 to 10
(defun psx (x) x)
(defun psy (y) y)

(defun p (&rest l)
  (assureps)
  (loop for v in l do
         (if (symbolp v) (setq v (maxima-string v)))
                                        ; (if (numberp v) (setq v (* 70 v)))
         (cond ((consp v)
                (loop for w in (cdr v) do (p w)))
               ((floatp v) (format $pstream "~,4g" v))
               (t(princ v $pstream)))
         (princ " " $pstream))
  (terpri $pstream))

(defun psapply (f lis)
  (if ($listp lis) (setq lis (cdr lis)))
  (apply 'p lis)
  (p f))

(defun $moveto (x y)
  (p (psx x) (psy y) "moveto ")) 

(defun $psline (a b c d)
  (p (psx a) (psy b) "moveto ")
  (p  (psx c) (psy d) "lineto"))

(defun setup-for-ps-range (xrange yrange do-prolog)
  (let* ((cy (/ (+ (nth 1 yrange) (nth 0 yrange)) 2.0))
         (cx (/ (+ (nth 1 xrange) (nth 0 xrange)) 2.0))
         (scaley (/ (nth 2 $window_size)
                    (* 1.2  (- (nth 1 yrange) (nth 0 yrange)))))
         (scalex (/ (nth 1 $window_size)
                    (* 1.2 (- (nth 1 xrange) (nth 0 xrange)))))
         ($ps_scale
          (progn
            (cond ((< scalex scaley)
                   (setq scaley scalex)))
            `((mlist) , scaley ,scaley)))
         ($ps_translate `((mlist) , cx ,cy)))
    (assureps do-prolog)))

(defun assureps (&optional do-prolog)
  (cond ((streamp $pstream))
        (t (setq do-prolog t)))
  (or $pstream (setq $pstream (open (plot-temp-file "maxout.ps") :direction :output :if-exists :supersede)))
  (cond (do-prolog
            (p "%!PS-Adobe-2.0")
          (p "%%Title: Maxima 2d plot") ;; title could be filename and/or plot equation
          (p "%%Creator: Maxima version:" *autoconf-version* "on" (lisp-implementation-type))
          (do-ps-created-date $pstream)
          (p "%%DocumentFonts: Helvetica")

          ;; Put the lower left corner of the bounding box at $ps_translate,
          ;; and put the upper right corner at ($ps_translate + $window_size).
          (p "%%BoundingBox:" 
             (round (nth 1 $ps_translate))
             (round (nth 2 $ps_translate))
             (round (+ (nth 1 $ps_translate) (nth 1 $window_size)))
             (round (+ (nth 2 $ps_translate) (nth 2 $window_size))))

          (p "%%Orientation: Portrait")
          (p "%%Pages: 1")
          (p "%%EndComments")
          (p "%%BeginPrologue")
          (p "%%EndPrologue")
          (p "%%BeginSetup")
          (p "%%PaperSize: Letter")
          (p "%%EndSetup")
          (p "%%Page: 1 1")
          (p  (* .5 (nth 1 $window_size))
              (* .5 (nth 2 $window_size))
              "translate"
              )
          (psapply "scale" $ps_scale)
          (p  (- (nth 1 $ps_translate))
              (- (nth 2 $ps_translate))
              "translate"
              )
          (p " 1.5 " (second $ps_scale) "div setlinewidth
/Helvetica findfont 14 " (second $ps_scale) " div scalefont setfont
/dotradius .05 def
/drawdot {
 /yy exch def
 /xx exch def
  gsave
  xx yy dotradius 0 360 arc
  fill
  grestore
}def

/ticklength  .03 def
/axiswidth  .01 def
/drawtick {
 /yy exch def
 /xx exch def
  gsave
  xx ticklength sub yy moveto
  xx ticklength add yy lineto
  stroke        
  xx yy  ticklength sub  moveto
  xx yy  ticklength add  lineto
  stroke
  grestore
} def
"))))


(defun $closeps ()
  (prog1
      (when (and (streamp $pstream)
                 )
        (p "showpage")
        (close  $pstream))
    (setq $pstream nil)))
        
(defun ps-fixup-points(lis)
  (assert ($listp lis))
  (setq lis (cdr lis))
  (cond ((numberp (car lis)))
        ((and ($listp (car lis)) (numberp (nth 1 (car lis))))
         (setq lis (loop for w in lis collect
                          (nth 1 w)
                          collect (nth 2 w))))
        (t (error
            "pscurve:Neither [x0,y0,x1,y1,...] nor [[x0,y0],[x1,y1],...]")))
  lis)

(defun $psdraw_curve (lis &aux (n 0))
  (declare (fixnum n))
  (setq lis (ps-fixup-points lis))
  (p "newpath" (nth 0 lis) (nth 1 lis) "moveto")
  (loop while lis with second
         do
         (cond ((eq (car lis) 'moveto)
                (or (eql n 0) (p "stroke"))
                (setq n 0) (setq lis (cddr lis))))
         (or (setq second (cadr lis)) (error "odd length list of points"))
         (cond ((eql n 0)
                (p (car lis) second "moveto"))
               (t  (p (car lis) second "lineto")
                   ))
         (setq n (+ n 1))
         (cond ((eql 0 (mod n 20))
                (p "stroke")
                (setq n 0))
               (t (setq lis (cddr lis)))))
  (or (eql n 0) (p "stroke")))



(defun $viewps ( &optional file)
  (cond  ((and (streamp $pstream))
          (do-ps-trailer)
          (force-output $pstream)))
  (cond (file (setq file (maxima-string file)))
        (t(setq file (plot-temp-file "maxout.ps"))
         
          (if (and (streamp $pstream))
              (force-output $pstream))))
  (if (equal $viewps_command "(gs -I. -Q  ~a)")
      (format t "~%type `quit' to exit back to affine or maxima
  To reprint a page do
  GS>showpage
  GS>(maxout.ps)run
  GS> -150 -150 translate 1.2 1.2 scale (maxout.ps)run
  would print it moved 150/72 inches to left, and down, and scaled by 1.2 times
  showpage clears scaling."))

  ($system    (format nil $viewps_command file)))

(defun $chkpt (a)
  (or (and ($listp a)
           (numberp (nth 1 a))
           (numberp (nth 2 a)))
      (error "illegal pt ~a" a))
  t)
       
(defvar $pslineno nil)
(defun $psdrawline (a &optional b c d)
  (cond ((null b)
         (assert (and ($listp a)
                      ($chkpt (nth 1 a))))
         (setq b (nth 2 a))
         (setq a (nth 1 a))))
  (cond ((null c)
         ($chkpt b)
         (setq c (nth 1 b))
         (setq d (nth 2 b))))
  (cond (($listp a)
         (setq b (nth 2 a) a (nth 1 a))))
  (cond ((null c)
         (setq c (nth 1 b))
         (setq c (nth 2 b))))

  (when $pslineno
    (incf $pslineno)
    ($pslabelline a b c d $pslineno))
      
  (p a b "moveto")
  (p c d "lineto")
  (p "stroke"))

(defun $pslabelline (a b c d $pslineno)
  (p (/ (+ a c) 2)
     (/ (+ b d) 2)
     "moveto"
     (format nil "(<--L~a)show" $pslineno)))

(defun $sort_polys (lis)
  (let ((tem
         (loop for v in (cdr lis)
                collect (cons
                         (loop for w in (cdr v)
                                maximize (nth 3 w))
                         v))))
    (print 'next)
    (cons '(mlist) (mapcar 'cdr (sortcar tem '<)))))

(defun $drawpoly (x)
  (p "gsave")
  (p (cdadr x) "moveto")
  (setq x (cddr x))
  (loop for edge in x
         do (p (cdr edge) "lineto")
         finally (p "1 setgray fill"))
  ($psdrawline x)
  (p "grestore"))

(defun $psdrawpolys (polys)
  (dolist (v (cdr polys))
    ($drawpoly v)))
      
;; draw the axes through $psdef
(defun $psaxes (leng &optional (lengy leng))
  (p "gsave axiswidth setlinewidth")
  (let (begx begy endx endy)
    (cond ((numberp leng)
           (setq begx (- leng))
           (setq endx leng))
          (t (setq begx (nth 1 leng))
             (setq endx (nth 2 leng))))
    (cond ((numberp lengy)
           (setq begy (- lengy))
           (setq endy lengy))
          (t (setq begy (nth 1 lengy))
             (setq endy (nth 2 lengy))))

    (loop for i from (floor begx) below (ceiling endx)
           do 
           ($psdrawline i 0 (+ i 1) 0)
           (p i 0 "drawtick")
           (p (+ i 1) 0 "drawtick"))

    (loop for i from (floor begy) below (ceiling endy)
           do
           ($psdrawline 0 i 0 (+ i 1) )
           (p 0 i "drawtick")
           (p 0 (+ i 1)  "drawtick"))

  
    (p "grestore")))

(defun $psdraw_points(lis)
  (assert (and ($listp lis)
               ($listp (cadr lis))))
  (loop for w in (cdr lis)
         do (p (nth 1 w)
               (nth 2 w)
               "drawdot")))

(defun $ps_axes ( rot )
  (let ((tem (make-array 9 :element-type 'double-float :initial-element 0d0)))
    (setf (aref tem 0) 4.0)
    (setf (aref tem 4) 4.0)
    (setf (aref tem 8) 4.0)
    ($rotate_pts tem rot)
    (p 0 0 "moveto")
    (p (aref tem 0) (aref tem 1) "lineto stroke")
    (p  (aref tem 0) (aref tem 1) "moveto (x) show")
    (p 0 0 "moveto")
    (p (aref tem 3) (aref tem 4) "lineto stroke")
    (p  (aref tem 3) (aref tem 4) "moveto (y) show")    
    (p 0 0 "moveto")
    (p (aref tem 6) (aref tem 7) "lineto stroke")
    (p  (aref tem 6) (aref tem 7) "moveto (z) show")    
    ))

;; if this is '$polar_to_xy then the x y coordinates are interpreted as r theta

(defun add-ps-finish (opts)
  (p (if opts
         "/xr .30 def
/xb 0.60 def
/xg .60  def
/myset { .005 mul dup xr add exch
 dup xb add exch
 xg add
setrgbcolor} def

/myfinish { myset  gsave fill grestore 0 setgray stroke  } def"

         "/myfinish {.9 setgray gsave fill grestore .1 setgray stroke  } def")))


(defun $draw_ngons (pts ngons number_edges &aux (i 0) (j 0) (s 0)
                    (opts *original-points*)
                    (maxz  most-negative-double-float))
  (declare (type (cl:array double-float) pts)
           #-(or cmu scl sbcl) (type (cl:array double-float) opts)
           (type (cl:array (mod 64000)) ngons)
           (fixnum number_edges i s j number_edges)
           (double-float maxz))
  (setq j (length ngons))
  (add-ps-finish opts)
  (loop while (< i j) 
         do 
         (loop initially (setq s number_edges)
                do
                                        ;(print-pt (aref pts (f* 3 (aref ngons i))))
                (print-pt (x-pt pts  (aref ngons i)))
                                        ;(print-pt (aref pts (f+ 1 (f* 3 (aref ngons i)))))
                (print-pt (y-pt pts  (aref ngons i)))
                
                (cond (opts (if (> (z-pt opts (aref ngons i)) maxz)
                                (setq maxz (z-pt opts (aref ngons i))))))
                (cond ((eql number_edges s) (p " moveto %"
                                        ;(aref pts (f+ 2 (f* 3 (aref ngons i))))
                                               
                                               ))
                      (t (p "lineto %" ;(aref pts (f+ 2 (f* 3 (aref ngons i))))
                            )))
                (setq i (f+ i 1))
                while (> (setq s (f- s 1)) 0))
         (setq i (f+ i 1))
         (cond (opts
                (p (f+ 1 (round ($* 100.0 ($/ ($- maxz (car *z-range*))
                                              (or (third *z-range*)
                                                  ($- (second *z-range*) (car *z-range*))))))))
                (setq maxz most-negative-double-float)
                ))
         (p " myfinish")
         ))

