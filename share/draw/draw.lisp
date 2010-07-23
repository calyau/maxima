;;;                 COPYRIGHT NOTICE
;;;  
;;;  Copyright (C) 2007-2010 Mario Rodriguez Riotorto
;;;  
;;;  This program is free software; you can redistribute
;;;  it and/or modify it under the terms of the
;;;  GNU General Public License as published by
;;;  the Free Software Foundation; either version 2 
;;;  of the License, or (at your option) any later version. 
;;;  
;;;  This program is distributed in the hope that it
;;;  will be useful, but WITHOUT ANY WARRANTY;
;;;  without even the implied warranty of MERCHANTABILITY
;;;  or FITNESS FOR A PARTICULAR PURPOSE. See the 
;;;  GNU General Public License for more details at
;;;  http://www.gnu.org/copyleft/gpl.html

;;; This is a maxima-gnuplot interface. Loads the picture package.

;;; Visit
;;; http://www.telefonica.net/web2/biomates/maxima/gpdraw
;;; for examples

;;; Some portions of this package were written by 
;;; Andrej Vodopivec and Joan Pau Beltran. Thanks!

;;; For questions, suggestions, bugs and the like, feel free
;;; to contact me at
;;; mario @@@ edu DOT xunta DOT es


($put '$draw 1 '$version)

;; the following variable will be removed in the future,
;; since some packages are still using it. 
(defvar $draw_loaded t)

(defvar $draw_compound t)

(defvar *windows-OS* (string= *autoconf-win32* "true"))

(defmacro write-font-type ()
   '(if (string= (get-option '$font) "")
      ""
      (format nil "font '~a,~a'" (get-option '$font) (get-option '$font_size))))


;; one-window multiplot: consecutive calls
;; to draw allways plot on the same window
(defvar *multiplot-is-active* nil)
(defun $multiplot_mode (term)
  (when (not *windows-OS*)
    (case term
      ($screen
        ($multiplot_mode '$none)
        (send-gnuplot-command
          (format nil "set terminal x11 ~a~%set multiplot~%" (write-font-type)))
        (setf *multiplot-is-active* t))
      ($wxt
        ($multiplot_mode '$none)
        (send-gnuplot-command
          (format nil "set terminal wxt ~a~%set multiplot~%" (write-font-type)))
        (setf *multiplot-is-active* t))
      ($none
        (send-gnuplot-command
          (format nil "unset multiplot~%"))
        (setf *multiplot-is-active* nil))
      (otherwise
        (merror "draw: ~M is not recognized as a multiplot mode" term)))))


;; This variable stores actual graphics options
(defvar *gr-options* (make-hash-table))


;; This variable stores user defaults
(defvar *user-gr-default-options* '())


(defun $set_draw_defaults (&rest opts)
   (setf *user-gr-default-options* opts)
   (cons '(mlist) opts))

;; Sets user default values of graphics options
(defun user-defaults ()
   (dolist (x *user-gr-default-options*)
      (if (equal ($op x) "=")
         (update-gr-option ($lhs x) ($rhs x))
         (merror "draw: item ~M is not recognized as an option assignment" x))))


;; Sets default values of graphics options
(defun ini-gr-options ()
  (setf
      ; global options to control general aspects of graphics
      (gethash '$proportional_axes *gr-options*) '$none  ; three possible options: none, xy, xyz
      (gethash '$xrange *gr-options*)           nil      ; nil => automatic computation
      (gethash '$xrange_secondary *gr-options*) nil      ; nil => automatic computation
      (gethash '$yrange *gr-options*)           nil      ; nil => automatic computation
      (gethash '$yrange_secondary *gr-options*) nil      ; nil => automatic computation
      (gethash '$zrange *gr-options*)           nil      ; nil => automatic computation
      (gethash '$cbrange *gr-options*)          nil      ; nil => automatic computation
      (gethash '$logx *gr-options*)             nil
      (gethash '$logy *gr-options*)             nil
      (gethash '$logz *gr-options*)             nil
      (gethash '$logcb *gr-options*)            nil
      (gethash '$title *gr-options*)            ""
      (gethash '$rot_vertical *gr-options*)     60   ; range: [0,180] (vertical rotation)
      (gethash '$rot_horizontal *gr-options*)   30   ; range: [0,360] (horizontal rotation)
      (gethash '$xy_file *gr-options*)          ""
      (gethash '$user_preamble *gr-options*)    ""
      (gethash '$xyplane *gr-options*)          nil
      (gethash '$font *gr-options*)             "";
      (gethash '$font_size *gr-options*)        12;

      ; colors are specified by name
      (gethash '$color *gr-options*)      "rgb 'black'"   ; for lines, points, borders and labels
      (gethash '$fill_color *gr-options*) "rgb 'red'"     ; for filled regions
      (gethash '$fill_density *gr-options*) 0       ; in [0,1], only for object 'bars

      ; implicit plot options
      (gethash '$ip_grid *gr-options*)    '((mlist simp) 50 50)
      (gethash '$ip_grid_in *gr-options*) '((mlist simp) 5 5)
      (gethash '$x_voxel *gr-options*)       10
      (gethash '$y_voxel *gr-options*)       10
      (gethash '$z_voxel *gr-options*)       10

      ; tics
      (gethash '$grid *gr-options*)            nil
      (gethash '$xtics *gr-options*)           "autofreq"
      (gethash '$xtics_secondary *gr-options*) nil   ; no tics in top x-axis
      (gethash '$ytics *gr-options*)           "autofreq"
      (gethash '$ytics_secondary *gr-options*) nil   ; no tics in right y-axis
      (gethash '$ztics *gr-options*)           "autofreq"
      (gethash '$cbtics *gr-options*)          "autofreq"
      (gethash '$xtics_rotate *gr-options*)    nil
      (gethash '$xtics_secondary_rotate *gr-options*) nil
      (gethash '$ytics_rotate *gr-options*)    nil
      (gethash '$ytics_secondary_rotate *gr-options*) nil
      (gethash '$ztics_rotate *gr-options*)    nil
      (gethash '$xtics_axis *gr-options*)      nil
      (gethash '$xtics_secondary_axis *gr-options*)   nil
      (gethash '$ytics_axis *gr-options*)      nil
      (gethash '$ytics_secondary_axis *gr-options*)   nil
      (gethash '$ztics_axis *gr-options*)      nil

      ; axis
      (gethash '$axis_bottom *gr-options*) t
      (gethash '$axis_left *gr-options*)   t
      (gethash '$axis_top *gr-options*)    t
      (gethash '$axis_right *gr-options*)  t
      (gethash '$axis_3d *gr-options*)     t
      (gethash '$xaxis *gr-options*)       nil   ; no xaxis by default
      (gethash '$xaxis_width *gr-options*) 1
      (gethash '$xaxis_type *gr-options*)  0    ; two options: 1 (solid) and 0 (dots)
      (gethash '$xaxis_color *gr-options*) "rgb 'black'"
      (gethash '$yaxis *gr-options*)       nil  ; no yaxis by default
      (gethash '$yaxis_width *gr-options*) 1
      (gethash '$yaxis_type *gr-options*)  0    ; two options: 1 (solid) and 0 (dots)
      (gethash '$yaxis_color *gr-options*) "rgb 'black'"
      (gethash '$zaxis *gr-options*)       nil  ; no zaxis by default
      (gethash '$zaxis_width *gr-options*) 1
      (gethash '$zaxis_type *gr-options*)  0    ; two options: 1 (solid) and 0 (dots)
      (gethash '$zaxis_color *gr-options*) "rgb 'black'"
      (gethash '$xlabel *gr-options*) ""
      (gethash '$ylabel *gr-options*) ""
      (gethash '$zlabel *gr-options*) ""

      ; point options
      (gethash '$point_size *gr-options*)    1
      (gethash '$point_type *gr-options*)    1
      (gethash '$points_joined *gr-options*) nil ; other options are: true and $impulses

      ; polygon  options
      (gethash '$transparent *gr-options*) nil
      (gethash '$border *gr-options*)      t

      ; vector  options
      (gethash '$head_both *gr-options*)   nil
      (gethash '$head_length *gr-options*) 2        ; in x-axis units
      (gethash '$head_angle *gr-options*)  45       ; with respect to the segment
      (gethash '$head_type *gr-options*)   '$filled ; other options are: $empty and $nofilled
      (gethash '$unit_vectors *gr-options*) nil

      ; label options
      (gethash '$label_alignment *gr-options*)   '$center     ; other options are: $left and $right
      (gethash '$label_orientation *gr-options*) '$horizontal ; the other option is $vertical

      ; line options
      (gethash '$line_width *gr-options*) 1
      (gethash '$line_type *gr-options*)  1    ; two options: 1 (solid) and 0 (dots)

      ; function options
      (gethash '$nticks *gr-options*)          29
      (gethash '$adapt_depth *gr-options*)     10
      (gethash '$key *gr-options*)             ""          ; by default, no keys
      (gethash '$filled_func *gr-options*)     nil         ; false, true (y axis) or an expression
      (gethash '$xaxis_secondary *gr-options*) nil
      (gethash '$yaxis_secondary *gr-options*) nil

      ; transformation option
      (gethash '$transform *gr-options*) '$none

      ; 3d options
      (gethash '$xu_grid *gr-options*)        30
      (gethash '$yv_grid *gr-options*)        30
      (gethash '$surface_hide *gr-options*)   nil
      (gethash '$enhanced3d *gr-options*)     '$none
      (gethash '$contour *gr-options*)        '$none  ; other options are: $base, $surface, $both and $map
      (gethash '$contour_levels *gr-options*) 5       ; 1-50, [lowest_level,step,highest_level] or {z1,z2,...}
      (gethash '$colorbox *gr-options*)       t       ; in pm3d mode, always show colorbox
      (gethash '$palette  *gr-options*)       '$color ; '$color is a short cut for [7,5,15]
                                                      ; and '$gray is a short cut for [3,3,3].
                                                      ; See command 'show palette rgbformulae' in gnuplot.
      (gethash '$tube_extremes *gr-options*) '((mlist simp) '$open '$open) ; or '$closed
  ) )



;; Returns value of option
(defun get-option (opt) (gethash opt *gr-options*))






;; update options color, fill_color, xaxis_color, yaxis_color, $zaxis_color
;; ------------------------------------------------------------------------
;; defined as a color name or hexadecimal #rrggbb
(defun update-color (opt val)
  (let ((str (substitute
                #\- #\_ 
                (string-downcase
                  (string-trim 
                     "\""
                     (coerce (mstring val) 'string))))))
      (cond
        ((some #'(lambda (z) (string= z str))
               '("white" "black" "gray0" "grey0" "gray10" "grey10" "gray20" "grey20"
                 "gray30" "grey30"   "gray40" "grey40" "gray50" "grey50" "gray60" 
                 "grey60" "gray70" "grey70" "gray80" "grey80" "gray90" "grey90" 
                 "gray100" "grey100" "gray" "grey" "light-gray" "light-grey" 
                 "dark-gray" "dark-grey" "red" "light-red" "dark-red" "yellow" 
                 "light-yellow" "dark-yellow" "green" "light-green" "dark-green" 
                 "spring-green" "forest-green" "sea-green" "blue" "light-blue" 
                 "dark-blue" "midnight-blue" "navy" "medium-blue" "royalblue" 
                 "skyblue" "cyan" "light-cyan" "dark-cyan" "magenta" "light-magenta"
                 "dark-magenta" "turquoise" "light-turquoise" "dark-turquoise"
                 "pink" "light-pink" "dark-pink" "coral" "light-coral" "orange-red"
                 "salmon" "light-salmon" "dark-salmon" "aquamarine" "khaki" 
                 "dark-khaki" "goldenrod" "light-goldenrod" "dark-goldenrod" "gold"
                 "beige" "brown" "orange" "dark-orange" "violet" "dark-violet"
                 "plum" "purple"))
           (setf (gethash opt *gr-options*) (format nil "rgb '~a'" str)))
        ((and (= (length str) 7)
              (char= (schar str 0) #\#)
              (every #'(lambda (z) (position z "0123456789abcdef"))
                     (subseq str 1)))
           (setf (gethash opt *gr-options*) (format nil "rgb '~a'" str)))
        (t
           (merror "draw: illegal color specification: ~M" str)))))






;; update option terminal
;; ----------------------
;; *draw-terminal-number* is used when working with
;; multiple windows. Empty string means
;; we are working with only one window
(defvar *draw-terminal-number* "")

(defun update-terminal (val)
  (let ((terms '($screen $png $jpg $gif $eps $eps_color $svg
                 $pdf $pdfcairo $wxt $animated_gif $aquaterm)))
     (cond
       ((member val terms)
          (setf (gethash '$terminal *gr-options*) val
                *draw-terminal-number* ""))
       ((and ($listp val)
             (= ($length val) 2)
             (member (cadr val) '($screen $wxt $aquaterm))
             (integerp (caddr val))
             (>= (caddr val) 0))
          (setf (gethash '$terminal *gr-options*) (cadr val)
                *draw-terminal-number* (caddr val)))
       (t
          (merror "draw: illegal terminal specification: ~M" val)))))







;; update option transform
;; -----------------------
(defvar *draw-transform-dimensions* 0)
(defvar *draw-transform-f1* nil)
(defvar *draw-transform-f2* nil)
(defvar *draw-transform-f3* nil)

(defmacro transform-point (n)
  (if (= n 2)
    '(when (> *draw-transform-dimensions* 0)
        (let ((xold xx)
              (yold yy))
          (setf xx (funcall *draw-transform-f1* xold yold)
                yy (funcall *draw-transform-f2* xold yold))))
    '(when (> *draw-transform-dimensions* 0)
        (let ((xold xx)
              (yold yy)
              (zold zz))
          (setf xx (funcall *draw-transform-f1* xold yold zold)
                yy (funcall *draw-transform-f2* xold yold zold)
                zz (funcall *draw-transform-f3* xold yold zold)  )))))

(defmacro transform-lists (n)
  (if (= n 2)
    '(when (> *draw-transform-dimensions* 0)
        (let ((xold x)
              (yold y))
          (setf x (mapcar #'(lambda (u1 u2) (funcall *draw-transform-f1* u1 u2))
                          xold yold)
                y (mapcar #'(lambda (u1 u2) (funcall *draw-transform-f2* u1 u2))
                          xold yold)) ))
    '(when (> *draw-transform-dimensions* 0)
        (let ((xold x)
              (yold y)
              (zold z))
          (setf x (mapcar #'(lambda (u1 u2 u3) (funcall *draw-transform-f1* u1 u2 u3))
                          xold yold zold)
                y (mapcar #'(lambda (u1 u2 u3) (funcall *draw-transform-f2* u1 u2 u3))
                          xold yold zold)
                z (mapcar #'(lambda (u1 u2 u3) (funcall *draw-transform-f3* u1 u2 u3))
                          xold yold zold))) )))

(defun update-transform (val)
  (let (vars)
    (cond
      ((equal val '$none)
        (setf (gethash '$transform *gr-options*) '$none
              *draw-transform-dimensions* 0
              *draw-transform-f1* nil
              *draw-transform-f2* nil
              *draw-transform-f3* nil))
      ((and ($listp val)
            (= ($length val) 4)
            ($subsetp ($union ($setify ($listofvars ($first val)))
                              ($setify ($listofvars ($second val))))
                      ($setify (list '(mlist) ($third val) ($fourth val)))) )
         ; transformation in 2d
         (setf vars (list '(mlist) ($third val) ($fourth val)))
         (setf (gethash '$transform *gr-options*) val
               *draw-transform-dimensions* 2
               *draw-transform-f1* (coerce-float-fun ($first val) vars)
               *draw-transform-f2* (coerce-float-fun ($second val) vars) ))
      ((and ($listp val)
            (= ($length val) 6)
            ($subsetp ($union ($setify ($listofvars ($first val)))
                              ($setify ($listofvars ($second val)))
                              ($setify ($listofvars ($third val))))
                      ($setify (list '(mlist) ($fourth val) ($fifth val) ($sixth val)))) )
         ; transformation in 3d
         (setf vars (list '(mlist) ($fourth val) ($fifth val) ($sixth val)))
         (setf (gethash '$transform *gr-options*) val
               *draw-transform-dimensions* 3
               *draw-transform-f1* (coerce-float-fun ($first val) vars)
               *draw-transform-f2* (coerce-float-fun ($second val) vars)
               *draw-transform-f3* (coerce-float-fun ($third val) vars)))
      (t
         (merror "draw: illegal transform definition")) )))








;; update option enhanced3d
;; ------------------------
(defvar *draw-enhanced3d-type* 0)
(defvar *draw-enhanced3d-fun* nil)

(defun check-enhanced3d-model (grobj lis)
  (when (null (position *draw-enhanced3d-type* lis))
    (merror (format nil "draw (~a): unacceptable enhanced3d model" grobj))))

(defun update-enhanced3d-expression (vars)
  (let ((texture (gethash '$enhanced3d *gr-options*)))
    (when (not ($subsetp ($setify ($listofvars texture))
                         ($setify vars)))
      (merror "draw: incompatible variables in enhanced3d expression"))
    (setf *draw-enhanced3d-fun* (coerce-float-fun texture vars))))

(defun update-enhanced3d (val)
  (cond
    ((or (null val)
         (equal val '$none))
      (setf (gethash '$enhanced3d *gr-options*) '$none
            *draw-enhanced3d-type* 0
            *draw-enhanced3d-fun* nil))
    ((equal val t)
      (let ((model '((mlist) $z $x $y $z)))
        (setf (gethash '$enhanced3d *gr-options*) model
              *draw-enhanced3d-type* 3
              *draw-enhanced3d-fun* (coerce-float-fun
                              ($first model)
                              (list '(mlist) ($second model) ($third model) ($fourth model))))))
    ((and ($listp val)
          ($subsetp ($setify ($listofvars ($first val)))
                    ($setify ($rest val))))
       (case ($length val)
         (2 (setf (gethash '$enhanced3d *gr-options*) val
                  *draw-enhanced3d-type* 1
                  *draw-enhanced3d-fun* (coerce-float-fun
                              ($first val)
                              (list '(mlist) ($second val)))))
         (3 (setf (gethash '$enhanced3d *gr-options*) val
                  *draw-enhanced3d-type* 2
                  *draw-enhanced3d-fun* (coerce-float-fun
                                  ($first val)
                                  (list '(mlist) ($second val) ($third val)))))
         (4 (setf (gethash '$enhanced3d *gr-options*) val
                  *draw-enhanced3d-type* 3
                  *draw-enhanced3d-fun* (coerce-float-fun
                                  ($first val)
                                  (list '(mlist) ($second val) ($third val) ($fourth val)))))
         (otherwise (merror "draw: illegal length of enhanced3d"))))
    ((not ($listp val))
       ; enhanced3d is given an expression without 
       ; explicit declaration of its variables.
       ; Each graphic object must check for them.
       (setf (gethash '$enhanced3d *gr-options*) val
             *draw-enhanced3d-type* 99
             *draw-enhanced3d-fun* nil))
    (t
        (merror "draw: illegal enhanced3d definition")) ) )



(defun ini-local-option-variables ()
  (setf ; global variables
       *draw-transform-dimensions* 0
       *draw-transform-f1* nil
       *draw-transform-f2* nil
       *draw-transform-f3* nil
       *draw-enhanced3d-type* 0
       *draw-enhanced3d-fun* nil)  )




;; Sets default values to global options
(defun ini-global-options ()
  (setf ; global options
      (gethash '$columns *gr-options*)      1
      (gethash '$terminal *gr-options*)     '$screen  ; defined as screen, png, jpg, gif, svg,
                                                      ; eps, eps_color, pdf, pdfcairo, wxt or
                                                      ; aquaterm. A list of type [term, number]
                                                      ; is also admited if term is screen, wxt
                                                      ; or aquaterm
      (gethash '$pic_width *gr-options*)    640    ; points for bitmap pictures
      (gethash '$pic_height *gr-options*)   480    ; points for bitmap pictures
      (gethash '$eps_width *gr-options*)    12     ; cm for eps pictures
      (gethash '$eps_height *gr-options*)   8      ; cm for eps pictures
      (gethash '$pdf_width *gr-options*)    21.0   ; cm for pdf pictures (A4 portrait width)
      (gethash '$pdf_height *gr-options*)   29.7   ; cm for pdf pictures (A4 portrait height)
      (gethash '$file_name *gr-options*)         "maxima_out"
      (gethash '$gnuplot_file_name *gr-options*) "maxout.gnuplot"
      (gethash '$data_file_name *gr-options*)    "data.gnuplot"
      (gethash '$file_bgcolor *gr-options*)      "xffffff"
      (gethash '$delay *gr-options*)        5      ; delay for animated gif's, default 5*(1/100) sec
   )
 )


;; Sets new values to graphic options
(defun update-gr-option (opt val)
   (case opt
      ($rot_vertical ; in range [0, 180]
            (setf val ($float val))
            (if (and (numberp val)
                     (>= val 0 )
                     (<= val 180 ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: rot_vertical must be angle in [0, 180]")))
      ($rot_horizontal ; in range [0, 360]
            (setf val ($float val))
            (if (and (numberp val)
                     (>= val 0 )
                     (<= val 360 ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: rot_horizontal must be angle in [0, 360]")))
      ($fill_density ; in range [0, 1]
            (setf val ($float val))
            (if (and (numberp val)
                     (>= val 0 )
                     (<= val 1 ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: fill_density must be a number in [0, 1]")))
      (($line_width $head_length $head_angle $eps_width $eps_height $pdf_width $pdf_height
        $xaxis_width $yaxis_width $zaxis_width) ; defined as positive numbers
            (setf val ($float val))
            (if (and (numberp val)
                     (> val 0 ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: Non positive number: ~M " val)))
      ($xyplane ; defined as real number or false
            (setf val ($float val))
            (if (or (numberp val)
                    (null val ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal xyplane allocation: ~M " val)))
      ($point_size ; defined as non negative numbers
            (setf val ($float val))
            (if (and (numberp val)
                     (>= val 0 ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: negative number: ~M " val)))
      ($points_joined ; defined as true, false or $impulses
            (if (member val '(t nil $impulses))
              (setf (gethash opt *gr-options*) val)
              (merror "draw: illegal points_joined option: ~M " val)) )
      ($colorbox ; defined as true, false or string
            (if (or (member val '(t nil))
                    (stringp val) )
              (setf (gethash opt *gr-options*) val)
              (merror "draw: illegal colorbox option: ~M " val)) )
      (($line_type $xaxis_type $yaxis_type $zaxis_type) ; defined as $solid or $dots
            (case val
               ($solid (setf (gethash opt *gr-options*) 1))
               ($dots  (setf (gethash opt *gr-options*) 0))
               (otherwise  (merror "draw: illegal line type: ~M" val))) )
      (($tube_extremes) ; defined as a list of two elements, $open and/or $closed
            (if (and ($listp val)
                     (= ($length val) 2)
                     (member ($first val) '($open $closed))
                     (member ($second val) '($open $closed)))
               (setf (gethash opt *gr-options*) val)
               (merror "draw: illegal tube extreme types: ~M" val)))
      ($point_type ; numbers >= -1 or shape names
            (cond
              ((and (integerp val) (>= val -1 ))
                 (setf (gethash opt *gr-options*) val))
              (t (let ((shapes '($none $dot $plus $multiply $asterisk
                                 $square $filled_square $circle $filled_circle
                                 $up_triangle $filled_up_triangle $down_triangle
                                 $filled_down_triangle $diamant $filled_diamant)))
                    (if (member val shapes)
                        (setf (gethash opt *gr-options*) (- (position val shapes) 1))
                        (merror "draw: illegal point type: ~M " val))))) )
      (($columns $nticks $adapt_depth $pic_width $pic_height     ; defined as positive integers
        $xu_grid $yv_grid $delay $x_voxel $y_voxel $z_voxel $font_size)
            (if (and (integerp val)
                     (> val 0 ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: non positive integer: ~M " val)))
      ($contour_levels    ; positive integer, increment or set of points
            (cond ((and (integerp val) (> val 0 ))
                    (setf (gethash opt *gr-options*) val))
                  ((and ($listp val) (= ($length val) 3) )
                     (let ((ini  ($float (nth 1 val)))
                           (step ($float (nth 2 val)))
                           (end  ($float (nth 3 val))))
                        (cond ((and (< ini end)
                                    (< step (- end ini)))
                                (setf (gethash opt *gr-options*) (format nil "incremental ~a,~a,~a" ini step end)))
                              (t
                                (merror "draw: illegal contour level incremental description: ~M " val))) ))
                  ((and ($setp val) (not ($emptyp val)))
                     (let ((pts (map 'list #'$float (rest val)))
                           (str "discrete ") )
                       (dolist (num pts 'done)
                         (setf str (concatenate 'string str " " (format nil "~a," num))))
                       (setf (gethash opt *gr-options*) (string-trim '(#\,) str) ) ))
                  (t
                    (merror "draw: unknown contour level description: ~M " val))))
      (($transparent $border $logx $logy $logz $logcb $head_both $grid
        $xaxis_secondary $yaxis_secondary $axis_bottom $axis_left $axis_top
        $axis_right $axis_3d $surface_hide $xaxis $yaxis $zaxis $unit_vectors
        $xtics_rotate $ytics_rotate $xtics_secondary_rotate $ytics_secondary_rotate
        $ztics_rotate $xtics_axis $ytics_axis $xtics_secondary_axis
        $ytics_secondary_axis $ztics_axis) ; true or false
            (if (or (equal val t)
                    (equal val nil))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: non boolean value: ~M " val)))
      ($filled_func  ; true, false or an expression
         (setf (gethash opt *gr-options*) val))
      ($transform
         (update-transform val))
      ($enhanced3d
         (update-enhanced3d val))
      (($xtics $ytics $xtics_secondary $ytics_secondary $ztics $cbtics)
        ; $auto or t, $none or nil, number, increment, set, set of pairs
            (cond ((member val '($none nil))   ; nil is maintained for back-portability
                     (setf (gethash opt *gr-options*) nil))
                  ((member val '($auto t))     ; t is maintained for back-portability
                     (setf (gethash opt *gr-options*) "autofreq"))
                  ((and (numberp (setf val ($float val)))  ; increment
                        (> val 0 ))
                     (setf (gethash opt *gr-options*) val))
                  ((and ($listp val)                          ; [ini,incr,end]
                        (= ($length val) 3)
                        (< (cadr val) (cadddr val))
                        (> (caddr val) 0)
                        (< (caddr val) (- (cadddr val) (cadr val))) )
                     (setf (gethash opt *gr-options*) 
                           (format nil "~a,~a,~a" (cadr val) (caddr val) (cadddr val))))
                  ((and ($setp val)
                        (every #'(lambda (z) (numberp z))
                               (cdr val)) )                   ; {n1,n2,n3,...}
                     (setf 
                        (gethash opt *gr-options*)
                        (do ((k (cdr val) (cdr k))
                             (str "" (concatenate 'string str (format nil "~a," (car k)))) )
                            ((null k) (concatenate 
                                         'string
                                         "("
                                         (string-right-trim "," str)
                                         ")")))))
                  ((and ($setp val)
                        (every #'(lambda (z) (and ($listp z)
                                                  (= ($length z) 2)
                                                  (numberp (caddr z)) ))
                               (cdr val)) )                     ; {[lab1,n1],[lab2,n2],...}
                     (setf 
                        (gethash opt *gr-options*)
                        (do ((k (cdr val) (cdr k))
                             (str "" (concatenate
                                        'string
                                         str
                                        (format nil "\"~a\" ~a," (cadar k) (caddar k)))))
                            ((null k) (concatenate
                                         'string
                                         "("
                                         (string-right-trim "," str)
                                         ")")))))
                  (t
                     (merror "draw: illegal tics allocation: ~M" val)) ))
      ($terminal
        (update-terminal val))
      ($head_type ; defined as $filled, $empty and $nofilled
            (if (member val '($filled $empty $nofilled))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal head type for vectors: ~M" val)))
      ($contour ; defined as $none, $base, $surface, $both and $map
            (if (member val '($base $surface $both $map))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal contour allocation: ~M" val)))
      ($proportional_axes ; defined as $none, $xy and $xyz
            (if (member val '($none $xy $xyz))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal proportional_axes specification")))
      ($label_alignment ; defined as $center, $left and $right
            (if (member val '($center $left $right))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal label alignment: ~M" val)))
      ($label_orientation ; defined as $horizontal and $vertical
            (if (member val '($horizontal $vertical))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal label orientation: ~M" val)))
      (($key $file_name $xy_file $title $xlabel $ylabel $zlabel $font
        $gnuplot_file_name $data_file_name)  ; defined as strings
            (setf (gethash opt *gr-options*) val))
      ($user_preamble ; defined as a string or a Maxima list of strings
            (let ((str ""))
              (cond
                (($atom val)
                  (setf str val))
                (($listp val)
                  (dolist (st (rest val))
                    (if (not ($atom st))
                        (merror "draw: user preamble ~M should be a string" st))
                    (setf str (concatenate 'string
                                            str
                                            (format nil (if (string= str "") "~a" "~%~a") st)))))
                (t (merror "draw: illegal user preamble especification")))
              (setf (gethash opt *gr-options*) str))  )
      (($xrange $yrange $xrange_secondary $yrange_secondary
        $zrange $cbrange) ; defined as a Maxima list with two numbers in increasing order
            (cond ((member val '($auto nil))     ; nil is maintained for back-portability
                     (setf (gethash opt *gr-options*) nil))
                  ((or (not ($listp val))
                       (/=  ($length val) 2))
                     (merror "draw: illegal range: ~M " val))
                  (t
                     (let ((fval1 ($float (cadr val)))
                           (fval2 ($float (caddr val))))
                       (cond
                         ((or (not (floatp fval1))
                              (not (floatp fval2))
                              (< fval2 fval1))
                            (merror "draw: illegal values in range specification"))
                         ((= ($length val) 2)  ; it's a trick: length 2 => user change
                            (setf (gethash opt *gr-options*) (list fval1 fval2)))
                         (t  ; should be length 3 or nil option => automatic computation of ranks
                            (setf (gethash opt *gr-options*) (list fval1 fval2 0)) ))  ))) )
      (($ip_grid $ip_grid_in)
       (if (not ($listp val))
           (merror "draw: illegal value for grid")
           (if (not (and (integerp ($first val))
                         (integerp ($second val))))
               (merror "draw: illegal value for grid")
               (setf (gethash opt *gr-options*) val))))
      ($palette ; defined as $color, $gray or [f1,f2,f3], with -36<=fi<=36
            (cond ((member val '($color $gray))
                    (setf (gethash opt *gr-options*) val))
                  ((and ($listp val)
                        (= ($length val) 3)
                        (every #'(lambda (x) (and (integerp x) (<= (abs x) 36)))
                               (cdr val)) )
                    (setf (gethash opt *gr-options*) (list (cadr val) (caddr val) (cadddr val))))
                  (t
                    (merror "draw: illegal palette description: ~M" val)))  )
      (($color $fill_color $xaxis_color $yaxis_color
        $zaxis_color)
        (update-color opt val))
      ($file_bgcolor ; defined as hexadecimal #rrggbb
        (let ((str (string-downcase (string-trim "\"" (coerce (mstring val) 'string)))))
            (if (and (= (length str) 7)
                     (char= (schar str 0) #\#)
                     (every #'(lambda (z) (position z "0123456789abcdef"))
                            (subseq str 1)))
              (setf (gethash opt *gr-options*) (concatenate 'string "x" (subseq str 1)))
              (merror "draw: illegal color background specification: ~M" str))) )

      (otherwise (merror "draw: unknown option ~M " opt))  ) )








;; This function is called from the graphic objects constructors
;; (points, rectangle, etc.). When a new object is created, and if
;; the user hasn't especified an x or y range, ranges are computed
;; automaticallly by calling this function. There is a trick so
;; that object constructors know if they can modify global variables
;; xrange and yrange; if these lists are of length 2, it means that
;; it was a user selection and they can't be altered; if they are of
;; length 3 (with a dummy 0), object constructors should make the necessary changes
;; to fit the objects in the window; if they are nil, default
;; value, constructors are also allowed to make changes.
(defmacro update-range (axi vmin vmax)
   `(case (length (gethash ,axi *gr-options*))
          (0 (setf (gethash ,axi *gr-options*) (list ,vmin ,vmax 0)))
          (3 (setf (gethash ,axi *gr-options*) (list (min ,vmin (first  (gethash ,axi *gr-options*)))
                                                     (max ,vmax (second (gethash ,axi *gr-options*)))
                                                     0))) ))

(defun update-ranges-2d (xmin xmax ymin ymax)
   (if (get-option '$xaxis_secondary)
      (update-range '$xrange_secondary xmin xmax)
      (update-range '$xrange xmin xmax))
   (if (get-option '$yaxis_secondary)
      (update-range '$yrange_secondary ymin ymax)
      (update-range '$yrange ymin ymax)) )

(defun update-ranges-3d (xmin xmax ymin ymax zmin zmax)
   (update-ranges-2d xmin xmax ymin ymax)
   (update-range '$zrange zmin zmax))

(defmacro check-extremes-x ()
  '(let ()
    (when (< xx xmin) (setf xmin xx))
    (when (> xx xmax) (setf xmax xx))))

(defmacro check-extremes-y ()
  '(let ()
    (when (< yy ymin) (setf ymin yy))
    (when (> yy ymax) (setf ymax yy))))

(defmacro check-extremes-z ()
  '(let ()
    (when (< zz zmin) (setf zmin zz))
    (when (> zz zmax) (setf zmax zz))))







;; Controls whether the actual graphics object must
;; be plotted against the primary or the secondary axes,
;; both horizontal and vertical. Secondary axes in 3D
;; are not yet supported.
(defun axes-to-plot ()
   (format nil "~a~a"
           (if (get-option '$xaxis_secondary)
               "x2"
               "x1")
           (if (get-option '$yaxis_secondary)
               "y2"
               "y1")))





(defstruct gr-object
   name command groups points)

(defun make-obj-title (str)
  (if (> (length str) 80)
      (concatenate 'string "t '" (subseq str 0 75) " ...'"))
      (concatenate 'string "t '" str "'"))


     


;; Object: 'points'
;; Usage:
;;     points([[x1,y1], [x2,y2], [x3,y3],...])
;;     points([x1,x2,x3,...], [y1,y2,y3,...])
;;     points([y1,y2,y3,...]), abscissas are automatically chosen: 1,2,3,...
;;     points(matrix), one-column, one-row, two-column or two-row matrix
;;     points(array1d)
;;     points(array1d, array1d)
;;     points(array2d), two-column or two-row array
;; Options:
;;     point_size
;;     point_type
;;     points_joined
;;     line_width
;;     key
;;     line_type
;;     color
;;     xaxis_secondary
;;     yaxis_secondary
;;     transform
(defun points-command ()
  (let ((opt (get-option '$points_joined)))
    (cond
      ((null opt) ; draws isolated points
         (format nil " ~a w p ps ~a pt ~a lc ~a axis ~a"
                 (make-obj-title (get-option '$key))
                 (get-option '$point_size)
                 (get-option '$point_type)
                 (get-option '$color)
                 (axes-to-plot)))
      ((eq opt t) ; draws joined points
         (format nil " ~a w lp ps ~a pt ~a lw ~a lt ~a lc ~a axis ~a"
                 (make-obj-title (get-option '$key))
                 (get-option '$point_size)
                 (get-option '$point_type)
                 (get-option '$line_width)
                 (get-option '$line_type)
                 (get-option '$color)
                 (axes-to-plot)))
      (t  ; draws impulses
         (format nil " ~a w i lw ~a lt ~a lc ~a axis ~a"
                 (make-obj-title (get-option '$key))
                 (get-option '$line_width)
                 (get-option '$line_type)
                 (get-option '$color)
                 (axes-to-plot))))) )

(defun points-array-2d (arg)
   (let ((xmin most-positive-double-float)
         (xmax most-negative-double-float)
         (ymin most-positive-double-float)
         (ymax most-negative-double-float)
         (pos -1)
         (dim (array-dimensions arg))
         n xx yy pts twocolumns)
      (cond
         ((and (= (length dim) 2)   ; two-column array
               (= (cadr dim) 2))
            (setf n (car dim))
            (setf twocolumns t))
         ((and (= (length dim) 2)   ; two-row array
               (= (car dim) 2))
            (setf n (cadr dim))
            (setf twocolumns nil))
         (t (merror "draw (points2d): bad 2d array input format")))
      (setf pts (make-array (* 2 n) :element-type 'flonum))
      (loop for k below n do
         (if twocolumns
            (setf xx ($float (aref arg k 0))
                  yy ($float (aref arg k 1)))
            (setf xx ($float (aref arg 0 k))
                  yy ($float (aref arg 1 k))))
         (transform-point 2)
         (check-extremes-x)
         (check-extremes-y)
         (setf (aref pts (incf pos)) xx)
         (setf (aref pts (incf pos)) yy))
      (update-ranges-2d xmin xmax ymin ymax)
      (make-gr-object
         :name 'points
         :command (points-command)
         :groups '((2 0)) ; numbers are sent to gnuplot in groups of 2
         :points (list pts))))

(defun points-array-1d (arg1 &optional (arg2 nil))
   (let ((xmin most-positive-double-float)
         (xmax most-negative-double-float)
         (ymin most-positive-double-float)
         (ymax most-negative-double-float)
         (pos -1)
         (dim (array-dimensions arg1))
         n x y xx yy pts)
      (cond
         ((and (null arg2)
               (= (length dim) 1))  ; y format
            (setf n (car dim))
            (setf x (make-array n
                                :element-type 'flonum
                                :initial-contents (loop for k from 1 to n collect ($float k)) ))
            (setf y (make-array n
                                :element-type 'flonum
                                :initial-contents (loop for k below n collect ($float (aref arg1 k))))))
         ((and (arrayp arg2)   ; xx yy format
               (= (length dim) 1)
               (equal dim (array-dimensions arg2)))
            (setf n (car dim))
            (setf x arg1
                  y arg2))
         (t (merror "draw (points2d): bad 1d array input format")))
      (setf pts (make-array (* 2 n) :element-type 'flonum))
      (loop for k below n do
         (setf xx ($float (aref x k))
               yy ($float (aref y k)))
         (transform-point 2)
         (check-extremes-x)
         (check-extremes-y)
         (setf (aref pts (incf pos)) xx)
         (setf (aref pts (incf pos)) yy))
      (update-ranges-2d xmin xmax ymin ymax)
      (make-gr-object
         :name 'points
         :command (points-command)
         :groups '((2 0)) ; numbers are sent to gnuplot in groups of 2
         :points (list pts))))

(defun points-list (arg1 &optional (arg2 nil))
   (let (x y xmin xmax ymin ymax pts)
      (cond 
            ((and ($listp arg1)
                  (null arg2)
                  (every #'$listp (rest arg1)))     ; xy format
               (let ((tmp (mapcar #'rest (rest arg1))))
                  (setf x (map 'list #'$float (map 'list #'first tmp))
                        y (map 'list #'$float (map 'list #'second tmp)))) )
            ((and ($matrixp arg1)
                  (= (length (cadr arg1)) 3)
                  (null arg2))                 ; two-column matrix
               (let ((tmp (mapcar #'rest (rest arg1))))
                  (setf x (map 'list #'$float (map 'list #'first tmp))
                        y (map 'list #'$float (map 'list #'second tmp)) ) ) )
            ((and ($listp arg1)
                  (null arg2)
                  (notany #'$listp (rest arg1)))   ; y format
               (setf x (loop for xx from 1 to (length (rest arg1)) collect ($float xx))
                     y (map 'list #'$float (rest arg1))))
            ((and ($matrixp arg1)
                  (= (length (cadr arg1)) 2)
                  (null arg2))                 ; one-column matrix
               (setf x (loop for xx from 1 to (length (rest arg1)) collect ($float xx))
                     y (map 'list #'$float (map 'list #'second (rest arg1)))))
            ((and ($matrixp arg1)
                  (= ($length arg1) 1)
                  (null arg2))                 ; one-row matrix
               (setf x (loop for xx from 1 to (length (cdadr arg1)) collect ($float xx))
                     y (map 'list #'$float (cdadr arg1))))
            ((and ($listp arg1)
                  ($listp arg2)
                  (= (length arg1) (length arg2)))  ; xx yy format
               (setf x (map 'list #'$float (rest arg1))
                     y (map 'list #'$float (rest arg2))))
            ((and ($matrixp arg1)
                  (= ($length arg1) 2)
                  (null arg2))            ; two-row matrix
               (setf x (map 'list #'$float (cdadr arg1))
                     y (map 'list #'$float (cdaddr arg1))))
            (t (merror "draw (points2d): bad input format")))
      (transform-lists 2)
      (setf xmin ($tree_reduce 'min (cons '(mlist simp) x))
            xmax ($tree_reduce 'max (cons '(mlist simp) x))
            ymin ($tree_reduce 'min (cons '(mlist simp) y))
            ymax ($tree_reduce 'max (cons '(mlist simp) y)) )
      (setf pts (make-array (* 2 (length x)) :element-type 'flonum
                                             :initial-contents (mapcan #'list x y)))
      ;; update x-y ranges if necessary
      (update-ranges-2d xmin xmax ymin ymax)
      (make-gr-object
         :name 'points
         :command (points-command)
         :groups '((2 0)) ; numbers are sent to gnuplot in groups of 2
         :points (list pts) ) ))

(defun points (arg1 &optional (arg2 nil))
   (if (arrayp arg1)
      (if (= (length (array-dimensions arg1)) 2)
         (points-array-2d arg1)
         (points-array-1d arg1 arg2))
      (points-list arg1 arg2)))







;; Object: 'points3d'
;; Usage:
;;     points([[x1,y1,z1], [x2,y2,z2], [x3,y3,z3],...])
;;     points([x1,x2,x3,...], [y1,y2,y3,...], [z1,z2,z3,...])
;;     points(matrix), three-column or three-row matrix
;;     points(array2d),  three-column or three-row array
;;     points(array1d, array1d, array1d, array1d)
;; Options:
;;     point_size
;;     point_type
;;     points_joined
;;     line_width
;;     key
;;     line_type
;;     color
;;     enhanced3d
;;     transform
(defun points3d-command ()
  (let ((opt (get-option '$points_joined))
        (pal (if (> *draw-enhanced3d-type* 0) "palette" (get-option '$color) )))
    (cond
      ((null opt) ; draws isolated points
         (format nil " ~a w p ps ~a pt ~a lc ~a"
                 (make-obj-title (get-option '$key))
                 (get-option '$point_size)
                 (get-option '$point_type)
                 pal ))
      ((eq opt t) ; draws joined points
         (format nil " ~a w lp ps ~a pt ~a lw ~a lt ~a lc ~a"
                 (make-obj-title (get-option '$key))
                 (get-option '$point_size)
                 (get-option '$point_type)
                 (get-option '$line_width)
                 (get-option '$line_type)
                 pal ))
      (t  ; draws impulses
         (format nil " ~a w i lw ~a lt ~a lc ~a"
                 (make-obj-title (get-option '$key))
                 (get-option '$line_width)
                 (get-option '$line_type)
                 pal )))))

(defun points3d (arg1 &optional (arg2 nil) (arg3 nil))
   (let (pts x y z xmin xmax ymin ymax zmin zmax ncols col)
      (check-enhanced3d-model "points" '(0 1 3))
      (cond (($listp arg1)   ; list input
               (cond ((and (every #'$listp (rest arg1))   ; xyz format
                           (null arg2)
                           (null arg3))
                        (let ((tmp (mapcar #'rest (rest arg1))))
                        (setf x (map 'list #'$float (map 'list #'first tmp))
                              y (map 'list #'$float (map 'list #'second tmp))
                              z (map 'list #'$float (map 'list #'third tmp)) ) ) )
                     ((and ($listp arg2)                  ; xx yy zz format
                           ($listp arg3)
                           (= (length arg1) (length arg2) (length arg3)))
                        (setf x (map 'list #'$float (rest arg1))
                              y (map 'list #'$float (rest arg2))
                              z (map 'list #'$float (rest arg3)) )) 
                     (t (merror "draw (points3d): bad list input format"))))
            (($matrixp arg1)   ; matrix input
               (cond ((and (= (length (cadr arg1)) 4)     ; three-column matrix
                           (null arg2)
                           (null arg3))
                        (let ((tmp (mapcar #'rest (rest arg1))))
                        (setf x (map 'list #'$float (map 'list #'first tmp))
                              y (map 'list #'$float (map 'list #'second tmp))
                              z (map 'list #'$float (map 'list #'third tmp)) ) ) )
                     ((and (= ($length arg1) 3)           ; three-row matrix
                           (null arg2)
                           (null arg3))
                        (setf x (map 'list #'$float (cdadr arg1))
                              y (map 'list #'$float (cdaddr arg1))
                              z (map 'list #'$float (cdaddr (rest arg1)) ) ) )
                     (t (merror "draw (points3d): bad matrix input format"))))
            ((arrayp arg1)   ; array input
               (let ((dim (array-dimensions arg1))) 
               (cond ((and (= (length dim) 2)   ; three-row array
                           (= (first dim) 3)
                           (null arg2)
                           (null arg3))
                        (setf x (loop for k from 0 below (second dim)
                                    collect ($float (aref arg1 0 k)))
                              y (loop for k from 0 below (second dim)
                                    collect ($float (aref arg1 1 k)))
                              z (loop for k from 0 below (second dim)
                                    collect ($float (aref arg1 2 k))) ))
                     ((and (= (length dim) 2)   ; three-column array
                           (= (second dim) 3)
                           (null arg2)
                           (null arg3))
                        (setf x (loop for k from 0 below (first dim)
                                    collect ($float (aref arg1 k 0)))
                              y (loop for k from 0 below (first dim)
                                    collect ($float (aref arg1 k 1)))
                              z (loop for k from 0 below (first dim)
                                    collect ($float (aref arg1 k 2))) ))
                     ((and (= (length dim) 1)   ; three 1d arrays
                           (arrayp arg2)
                           (arrayp arg3)
                           (equal dim (array-dimensions arg2))
                           (equal dim (array-dimensions arg3)))
                        (setf x (map 'list #'$float arg1)
                              y (map 'list #'$float arg2)
                              z (map 'list #'$float arg3) ) )
                     (t (merror "draw (points3d): bad array input format")) ) ) )
            (t (merror "draw (points3d): bad input format")))
      (transform-lists 3)
      ; set pm3d colors
      (cond ((= *draw-enhanced3d-type* 0)
                (setf ncols 3)
                (setf pts (make-array (* ncols (length x))
                                      :element-type 'flonum
                                      :initial-contents (mapcan #'list x y z))))
            ((= *draw-enhanced3d-type* 1)
                (setf col (loop for k from 1 to (length x)
                                collect (funcall *draw-enhanced3d-fun* k)))
                (setf ncols 4)
                (setf pts (make-array (* ncols (length x))
                                      :element-type 'flonum
                                      :initial-contents (mapcan #'list x y z col))))
            ((= *draw-enhanced3d-type* 3)
                (setf col (mapcar #'(lambda (xx yy zz) (funcall *draw-enhanced3d-fun* xx yy zz)) x y z))
                (setf ncols 4)
                (setf pts (make-array (* ncols (length x))
                                      :element-type 'flonum
                                      :initial-contents (mapcan #'list x y z col)))) )
      (setf xmin ($tree_reduce 'min (cons '(mlist simp) x))
            xmax ($tree_reduce 'max (cons '(mlist simp) x))
            ymin ($tree_reduce 'min (cons '(mlist simp) y))
            ymax ($tree_reduce 'max (cons '(mlist simp) y))
            zmin ($tree_reduce 'min (cons '(mlist simp) z))
            zmax ($tree_reduce 'max (cons '(mlist simp) z)) )
      ;; update x-y-y ranges if necessary
      (update-ranges-3d xmin xmax ymin ymax zmin zmax)
      (make-gr-object
         :name 'points
         :command (points3d-command)
         :groups `((,ncols 0)) ; numbers are sent to gnuplot in groups of 4 or 3 
                               ; (depending on colored 4th dimension or not), without blank lines
         :points (list pts) )  ))






;; Object: 'polygon'
;; Usage:
;;     polygon([[x1,y1], [x2,y2], [x3,y3],...])
;;     polygon([x1,x2,x3,...], [y1,y2,y3,...])
;; Options:
;;     transparent
;;     fill_color
;;     border
;;     line_width
;;     line_type
;;     color
;;     key
;;     xaxis_secondary
;;     yaxis_secondary
;;     transform
(defun polygon (arg1 &optional (arg2 nil))
   (if (and (gethash '$transparent  *gr-options*)
            (not (gethash '$border  *gr-options*)))
       (merror "draw (polygon): transparent is true and border is false; this is not consistent"))
   (let (pltcmd pts grps x y xmin xmax ymin ymax)
      (cond ((and ($listp arg1)
                  (every #'$listp (rest arg1))
                  (null arg2) )                    ; xy format
               (let ((tmp (mapcar #'rest (rest arg1))))
                  (setf x (map 'list #'(lambda (z) ($float (first z))) tmp)
                        y (map 'list #'(lambda (z) ($float (second z))) tmp) ) )  )
            ((and ($listp arg1)
                  ($listp arg2)
                  (= (length arg1) (length arg2)))  ; xx yy format
               (setf x (map 'list #'$float (rest arg1))
                     y (map 'list #'$float (rest arg2))) )
            (t (merror "draw (polygon): bad input format"))  )
      (transform-lists 2)
      (setf xmin ($tree_reduce 'min (cons '(mlist simp) x))
            xmax ($tree_reduce 'max (cons '(mlist simp) x))
            ymin ($tree_reduce 'min (cons '(mlist simp) y))
            ymax ($tree_reduce 'max (cons '(mlist simp) y)) )
      ;; update x-y ranges if necessary
      (update-ranges-2d xmin xmax ymin ymax)
      (cond
         ((get-option '$transparent)  ; if transparent, draw only the border
             (setf pltcmd (format nil " ~a  w l lw ~a lt ~a lc ~a axis ~a"
                                      (make-obj-title (get-option '$key))
                                      (get-option '$line_width)
                                      (get-option '$line_type)
                                      (get-option '$color)
                                      (axes-to-plot)))
             (setf grps '((2 0)))  ; numbers are sent to gnuplot in groups of 2
             (setf pts (list (make-array (+ (* 2 (length x)) 2)
                                         :element-type 'flonum
                                         :initial-contents (append (mapcan #'list x y)
                                                                   (list (first x) (first y))) )) ) )
         ((not (get-option '$border)) ; no transparent, no border
             (setf pltcmd (format nil " ~a w filledcurves lc ~a axis ~a"
                                      (make-obj-title (get-option '$key))
                                      (get-option '$fill_color)
                                      (axes-to-plot)))
             (setf grps '((2 0)))  ; numbers are sent to gnuplot in groups of 2
             (setf pts (list (make-array (* 2 (length x))
                                         :element-type 'flonum
                                         :initial-contents (mapcan #'list x y)) ) ))
         (t ; no transparent with border
             (setf pltcmd (list (format nil " ~a w filledcurves lc ~a axis ~a"
                                        (make-obj-title (get-option '$key))
                                        (get-option '$fill_color)
                                        (axes-to-plot))
                                (format nil " t '' w l lw ~a lt ~a lc ~a axis ~a"
                                        (get-option '$line_width)
                                        (get-option '$line_type)
                                        (get-option '$color)
                                        (axes-to-plot))))
             (setf grps '((2 0) (2 0)))  ; both sets of vertices (interior and border)
                                     ; are sent to gnuplot in groups of 2
             (setf pts (list (make-array (* 2 (length x))
                                         :element-type 'flonum
                                         :initial-contents (mapcan #'list x y))
                             (make-array (+ (* 2 (length x)) 2)
                                         :element-type 'flonum
                                         :initial-contents (append (mapcan #'list x y)
                                                                   (list (first x) (first y))))))))
      (make-gr-object
         :name   'polygon
         :command pltcmd
         :groups  grps
         :points  pts )))







;; Object: 'triangle'
;; Usage:
;;     triangle([x1,y1], [x2,y2], [x3,y3])
;; Options:
;;     transparent
;;     fill_color
;;     border
;;     line_width
;;     line_type
;;     color
;;     key
;;     xaxis_secondary
;;     yaxis_secondary
;;     transform
(defun triangle (arg1 arg2 arg3)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 2))
           (not ($listp arg2))
           (not (= ($length arg2) 2))
           (not ($listp arg3))
           (not (= ($length arg3) 2)))
       (merror "draw2d (triangle): vertices are not correct"))
   (let* ((x1 ($float (cadr arg1)))
          (y1 ($float (caddr arg1)))
          (x2 ($float (cadr arg2)))
          (y2 ($float (caddr arg2)))
          (x3 ($float (cadr arg3)))
          (y3 ($float (caddr arg3)))
          (grobj (polygon `((mlist simp)
                            ((mlist simp) ,x1 ,y1)
                            ((mlist simp) ,x2 ,y2)
                            ((mlist simp) ,x3 ,y3)
                            ((mlist simp) ,x1 ,y1)))))
      (setf (gr-object-name grobj) 'triangle)
      grobj))






;; Object: 'quadrilateral'
;; Usage:
;;     quadrilateral([x1,y1], [x2,y2], [x3,y3], [x4,y4])
;; Options:
;;     transparent
;;     fill_color
;;     border
;;     line_width
;;     line_type
;;     color
;;     key
;;     xaxis_secondary
;;     yaxis_secondary
;;     transform
(defun quadrilateral (arg1 arg2 arg3 arg4)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 2))
           (not ($listp arg2))
           (not (= ($length arg2) 2))
           (not ($listp arg3))
           (not (= ($length arg3) 2))
           (not ($listp arg4))
           (not (= ($length arg4) 2)))
       (merror "draw2d (quadrilateral): vertices are not correct"))
   (let* ((x1 ($float (cadr arg1)))
          (y1 ($float (caddr arg1)))
          (x2 ($float (cadr arg2)))
          (y2 ($float (caddr arg2)))
          (x3 ($float (cadr arg3)))
          (y3 ($float (caddr arg3)))
          (x4 ($float (cadr arg4)))
          (y4 ($float (caddr arg4)))
          (grobj (polygon `((mlist simp)
                            ((mlist simp) ,x1 ,y1)
                            ((mlist simp) ,x2 ,y2)
                            ((mlist simp) ,x3 ,y3)
                            ((mlist simp) ,x4 ,y4)
                            ((mlist simp) ,x1 ,y1)))))
      (setf (gr-object-name grobj) 'quadrilateral)
      grobj))







;; Object: 'rectangle'
;; Usage:
;;     rectangle([x1,y1], [x2,y2]), being [x1,y1] & [x2,y2] opposite vertices
;; Options:
;;     transparent
;;     fill_color
;;     border
;;     line_width
;;     line_type
;;     color
;;     key
;;     xaxis_secondary
;;     yaxis_secondary
;;     transform
(defun rectangle (arg1 arg2)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 2))
           (not ($listp arg2))
           (not (= ($length arg2) 2)))
       (merror "draw2d (rectangle): vertices are not correct"))
   (let* ((x1 ($float (cadr arg1)))
          (y1 ($float (caddr arg1)))
          (x2 ($float (cadr arg2)))
          (y2 ($float (caddr arg2)))
          (grobj (polygon `((mlist simp)
                            ((mlist simp) ,x1 ,y1)
                            ((mlist simp) ,x2 ,y1)
                            ((mlist simp) ,x2 ,y2)
                            ((mlist simp) ,x1 ,y2)
                            ((mlist simp) ,x1 ,y1)))))
      (setf (gr-object-name grobj) 'rectangle)
      grobj))







;; Object: 'ellipse'
;; Usage:
;;     ellipse(xc, yc, a, b, ang1 ang2)
;; Options:
;;     nticks
;;     transparent
;;     fill_color
;;     border
;;     line_width
;;     line_type
;;     key
;;     color
;;     xaxis_secondary
;;     yaxis_secondary
;;     transform
(defun ellipse (xc yc a b ang1 ang2)
  (if (and (gethash '$transparent  *gr-options*)
           (not (gethash '$border  *gr-options*)))
      (merror "draw2d (ellipse): transparent is true and border is false; this is not consistent"))
  (let ((fxc ($float xc))
        (fyc ($float yc))
        (fa ($float a))
        (fb ($float b))
        (fang1 ($float ang1))
        (fang2 ($float ang2))
        (nticks (gethash '$nticks  *gr-options*))
        (xmin most-positive-double-float)
        (xmax most-negative-double-float)
        (ymin most-positive-double-float)
        (ymax most-negative-double-float)
        (result nil)
        pts grps tmin tmax eps xx yy tt pltcmd)
    (when (or (notevery #'floatp (list fxc fyc fa fb fang1 fang2))
              (<= fa 0.0)
              (<= fb 0.0))
       (merror "draw (ellipse): illegal argument(s)"))
    ; degrees to radians
    (setf fang1 (* 0.017453292519943295 fang1)
          fang2 (* 0.017453292519943295 fang2))
    (setf tmin (min fang1 (+ fang1 fang2))
          tmax (max fang1 (+ fang1 fang2))
          eps (/ (- tmax tmin) (- nticks 1)))
    (setf tt tmin)
    (loop
      (setf xx (+ fxc (* fa (cos tt))))
      (setf yy (+ fyc (* fb (sin tt))))
      (transform-point 2)
      (check-extremes-x)
      (check-extremes-y)
      (setf result (append (list xx yy) result))
      (if (>= tt tmax) (return))
      (setf tt (+ tt eps))
      (if (>= tt tmax) (setq tt tmax)) )
    (when (> *draw-transform-dimensions* 0)
      (let ((xold fxc)
            (yold fyc))
        (setf fxc (funcall *draw-transform-f1* xold yold)
              fyc (funcall *draw-transform-f2* xold yold))) )
    ; update x-y ranges if necessary
    (setf xmin (min fxc xmin)
          xmax (max fxc xmax)
          ymin (min fyc ymin)
          ymax (max fyc ymax))
    (update-ranges-2d xmin xmax ymin ymax)
    (cond
       ((get-option '$transparent)  ; if transparent, draw only the border
           (setf pltcmd (format nil " ~a w l lw ~a lt ~a lc ~a axis ~a"
                                    (make-obj-title (get-option '$key))
                                    (get-option '$line_width)
                                    (get-option '$line_tyype)
                                    (get-option '$color)
                                    (axes-to-plot)))
           (setf grps '((2 0)))
           (setf pts `( ,(make-array (length result) :element-type 'flonum
                                                    :initial-contents result)))  )
       ((not (get-option '$border)) ; no transparent, no border
           (setf pltcmd (format nil " ~a w filledcurves xy=~a,~a lc ~a axis ~a"
                                    (make-obj-title (get-option '$key))
                                    fxc fyc
                                    (get-option '$fill_color)
                                    (axes-to-plot)))
           (setf grps '((2 0)))
           (setf pts `( ,(make-array (length result) :element-type 'flonum
                                                    :initial-contents result)))  )
       (t ; no transparent with border
             (setf pltcmd (list (format nil " ~a w filledcurves xy=~a,~a lc ~a axis ~a"
                                            (make-obj-title (get-option '$key))
                                            fxc fyc
                                            (get-option '$fill_color)
                                            (axes-to-plot))
                                (format nil " t '' w l lw ~a lt ~a lc ~a axis ~a"
                                            (get-option '$line_width)
                                            (get-option '$line_type)
                                            (get-option '$color)
                                            (axes-to-plot))))
           (setf grps '((2 0) (2 0)))
           (setf pts (list (make-array (length result) :element-type 'flonum
                                                       :initial-contents result)
                           (make-array (length result) :element-type 'flonum
                                                       :initial-contents result)))  ))
    (make-gr-object
       :name    'ellipse
       :command pltcmd
       :groups  grps
       :points  pts ) ))








;; Object: 'label'
;; Usage in 2d:
;;     label([string1,xx1,y1],[string2,xx2,y2],...)
;; Usage in 3d:
;;     label([string1,x1,y1,z1],[string2,x2,y2,z2],...)
;; Options:
;;     label_alignment
;;     label_orientation
;;     color
;;     xaxis_secondary
;;     yaxis_secondary
(defun label (&rest lab)
  (let ((n (length lab))
        (result nil)
        is2d)
    (cond ((= n 0)
            (merror "draw (label): no arguments in object labels"))
          ((every #'$listp lab)
            (cond ((every #'(lambda (z) (= 3 ($length z))) lab)   ; labels in 2d
                    (setf is2d t))
                  ((every #'(lambda (z) (= 4 ($length z))) lab)   ; labels in 3d
                    (setf is2d nil))
                  (t
                    (merror "draw (label): arguments of not equal length")))
            (cond (is2d
                    (let (fx fy text)
                      (dolist (k lab)
                        (setf fx   ($float ($second k))
                              fy   ($float ($third k))
                              text (format nil "\"~a\"" ($first k))  )
                        (if (or (not (floatp fx)) 
                                (not (floatp fy)))
                            (merror "draw (label): non real 2d coordinates"))
                        (update-ranges-2d fx fx fy fy)
                        (setf result (append (list fx fy text) result)))))
                  (t ; labels in 3d
                    (let (fx fy fz text)
                      (dolist (k lab)
                        (setf fx   ($float ($second k))
                              fy   ($float ($third k))
                              fz   ($float ($fourth k))
                              text (format nil "\"~a\"" ($first k)) )
                        (if (or (not (floatp fx)) 
                                (not (floatp fy))
                                (not (floatp fz)))
                            (merror "draw (label): non real 3d coordinates"))
                        (update-ranges-3d fx fx fy fy fz fz)
                        (setf result (append (list fx fy fz text) result)))))) )
          (t (merror "draw (label): illegal arguments")))
    (make-gr-object
       :name 'label
       :command (format nil " t '' w labels ~a ~a tc ~a ~a"
                              (case (get-option '$label_alignment)
                                 ($center "center")
                                 ($left   "left")
                                 ($right  "right"))
                              (case (get-option '$label_orientation)
                                 ($horizontal "norotate")
                                 ($vertical  "rotate"))
                              (get-option '$color)
                              (if is2d
                                 (format nil "axis ~a" (axes-to-plot))
                                 "") )
       :groups (if is2d '((3 0)) '((4 0)))
       :points (list (make-array (length result) :initial-contents result))) ))







;; Object: 'bars'
;;     bars([x1,h1,w1],[x2,h2,w2],...), x, height and width 
;; Options:
;;     key
;;     fill_color
;;     fill_density
;;     line_width
;;     xaxis_secondary
;;     yaxis_secondary
(defun bars (&rest boxes)
  (let ((n (length boxes))
        (count -1)
        (xmin most-positive-double-float)
        (xmax most-negative-double-float)
        (ymin most-positive-double-float)
        (ymax most-negative-double-float)
        result x h w w2)
    (when (= n 0) 
      (merror "draw2d (bars): no arguments in object bars"))
    (when (not (every #'(lambda (z) (and ($listp z) (= 3 ($length z)))) boxes))
      (merror "draw2d (bars): arguments must be lists of length three"))
    (setf result (make-array (* 3 n) :element-type 'flonum))
    (dolist (k boxes)
       (setf x ($float ($first k))
             h ($float ($second k))
             w ($float ($third k)))
       (setf w2 (/ w 2))
       (setf (aref result (incf count)) x
             (aref result (incf count)) h
             (aref result (incf count)) w)
       (setf xmin (min xmin (- x w2))
             xmax (max xmax (+ x w2))
             ymin (min ymin h)
             ymax (max ymax h)) )
    (update-ranges-2d xmin xmax ymin ymax)
    (make-gr-object
       :name 'bars
       :command (format nil " ~a w boxes fs solid ~a lw ~a lc ~a axis ~a"
                            (make-obj-title (get-option '$key))
                            (get-option '$fill_density)
                            (get-option '$line_width)
                            (get-option '$fill_color)
                            (axes-to-plot) )
       :groups '((3 0))  ; numbers are sent to gnuplot in groups of 3, without blank lines
       :points (list (make-array (length result) :initial-contents result))) ))








;; Object: 'vector'
;; Usage:
;;     vector([x,y], [dx,dy]), represents vector from [x,y] to [x+dx,y+dy]
;; Options:
;;     head_both
;;     head_length
;;     head_angle
;;     head_type
;;     line_width
;;     line_type
;;     key
;;     color
;;     unit_vectors
;;     xaxis_secondary
;;     yaxis_secondary
(defun vect (arg1 arg2)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 2))
           (not ($listp arg2))
           (not (= ($length arg2) 2)))
       (merror "draw (vector): coordinates are not correct"))
   (let* ((x ($float (cadr arg1)))
          (y ($float (caddr arg1)))
          (dx ($float (cadr arg2)))
          (dy ($float (caddr arg2)))
          xdx ydy)
      (when (and (get-option '$unit_vectors)
                 (or (/= dx 0) (/= dy 0)))
         (let ((module (sqrt (+ (* dx dx) (* dy dy)))))
            (setf dx (/ dx module)
                  dy (/ dy module)  )))
      (setf xdx ($float (+ x dx))
            ydy ($float (+ y dy)))
      (update-ranges-2d (min x xdx) (max x xdx) (min y ydy) (max y ydy))
      (make-gr-object
         :name 'vector
         :command (format nil " ~a w vect ~a size ~a, ~a ~a lw ~a lt ~a lc ~a axis ~a"
                              (make-obj-title (get-option '$key))
                              (if (get-option '$head_both) "heads" "head")
                              (get-option '$head_length)
                              (get-option '$head_angle)
                              (case (get-option '$head_type)
                                 ($filled   "filled")
                                 ($empty    "empty")
                                 ($nofilled "nofilled"))
                              (get-option '$line_width)
                              (get-option '$line_type)
                              (get-option '$color)
                              (axes-to-plot) )
         :groups '((4 0))
         :points `(,(make-array 4 :element-type 'flonum
                                  :initial-contents (list x y dx dy))) ) ))







;; Object: 'vector3d'
;; Usage:
;;     vector([x,y,z], [dx,dy,dz]), represents vector from [x,y,z] to [x+dx,y+dy,z+dz]
;; Options:
;;     head_both
;;     head_length
;;     head_angle
;;     head_type
;;     line_width
;;     line_type
;;     key
;;     color
;;     unit_vectors
(defun vect3d (arg1 arg2)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 3))
           (not ($listp arg2))
           (not (= ($length arg2) 3)))
       (merror "draw (vector): coordinates are not correct"))
   (let* ((x ($float (cadr arg1)))
          (y ($float (caddr arg1)))
          (z ($float (cadddr arg1)))
          (dx ($float (cadr arg2)))
          (dy ($float (caddr arg2)))
          (dz ($float (cadddr arg2)))
          xdx ydy zdz )
      (when (and (get-option '$unit_vectors)
                 (or (/= dx 0) (/= dy 0) (/= dz 0)))
         (let ((module (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
            (setf dx (/ dx module)
                  dy (/ dy module)
                  dz (/ dz module)  )))
      (setf xdx ($float (+ x dx))
            ydy ($float (+ y dy))
            zdz ($float (+ z dz)) )
      (update-ranges-3d (min x xdx) (max x xdx) (min y ydy) (max y ydy) (min z zdz) (max z zdz))
      (make-gr-object
         :name 'vector
         :command (format nil " ~a w vect ~a ~a size ~a, ~a lw ~a lt ~a lc ~a"
                              (make-obj-title (get-option '$key))
                              (if (get-option '$head_both) "heads" "head")
                              (case (get-option '$head_type)
                                 ($filled   "filled")
                                 ($empty    "empty")
                                 ($nofilled "nofilled"))
                              (get-option '$head_length)
                              (get-option '$head_angle)
                              (get-option '$line_width)
                              (get-option '$line_type)
                              (get-option '$color) )
         :groups '((6 0))
         :points `(,(make-array 6 :element-type 'flonum
                                  :initial-contents (list x y z dx dy dz))) ) ))








;; Object: 'explicit'
;; Usage:
;;     explicit(fcn,var,minval,maxval)
;; Options:
;;     nticks
;;     adapt_depth
;;     line_width
;;     line_type
;;     color
;;     filled_func
;;     fill_color
;;     key
;;     xaxis_secondary
;;     yaxis_secondary
(defun explicit (fcn var minval maxval)
  (let* ((nticks (gethash '$nticks  *gr-options*))
         (depth (gethash '$adapt_depth  *gr-options*))
         ($numer t)
         (xmin ($float minval))
         (xmax ($float maxval))
         (x-step (/ (- xmax xmin) ($float nticks) 2))
         (ymin most-positive-double-float)
         (ymax most-negative-double-float)
         x-samples y-samples yy result pltcmd result-array)
    (setq fcn (coerce-float-fun fcn `((mlist) ,var)))
    (if (< xmax xmin)
       (merror "draw2d (explicit): illegal range"))
    (flet ((fun (x) (funcall fcn x)))
      (dotimes (k (1+ (* 2 nticks)))
        (let* ((x (+ xmin (* k x-step)))
               (y (fun x)))
          (when (numberp y)    ; check for non numeric y, as in 1/0
             (push x x-samples)
             (push y y-samples)  ) ))
      (setf x-samples (nreverse x-samples))
      (setf y-samples (nreverse y-samples))
      ;; For each region, adaptively plot it.
      (do ((x-start x-samples (cddr x-start))
           (x-mid (cdr x-samples) (cddr x-mid))
           (x-end (cddr x-samples) (cddr x-end))
           (y-start y-samples (cddr y-start))
           (y-mid (cdr y-samples) (cddr y-mid))
           (y-end (cddr y-samples) (cddr y-end)))
          ((null x-end))

        ;; The region is x-start to x-end, with mid-point x-mid.
        (let ((sublst (adaptive-plot #'fun (car x-start) (car x-mid) (car x-end)
                                           (car y-start) (car y-mid) (car y-end)
                                           depth 1e-5)))
          (when (not (null result))
            (setf sublst (cddr sublst)))
          ;; clean non numeric pairs
          (do ((lst sublst (cddr lst)))
              ((null lst) 'done)
            (when (numberp (second lst))
              (setf result (append result (list (first lst) (second lst)))))))))

      (cond ((null (get-option '$filled_func))
               (cond
                 ((> *draw-transform-dimensions* 0)
                    ; With geometric transformation.
                    ; When option filled_func in not nil,
                    ; geometric transformation is ignored
                    (setf result-array (make-array (length result)
                                                   :element-type 'flonum))
                    (setf xmin most-positive-double-float
                          xmax most-negative-double-float)
                    (let (xold yold x y (count -1))
                      (do ((lis result (cddr lis)))
                          ((null lis))
                        (setf xold (first lis)
                              yold (second lis))
                        (setf x (funcall *draw-transform-f1* xold yold)
                              y (funcall *draw-transform-f2* xold yold))
                        (if (> x xmax) (setf xmax x))
                        (if (< x xmin) (setf xmin x))
                        (if (> y ymax) (setf ymax y))
                        (if (< y ymin) (setf ymin y))
                        (setf (aref result-array (incf count)) x)
                        (setf (aref result-array (incf count)) y)  )  ) )
                 (t
                    ; No geometric transformation invoked.
                    (do ((y (cdr result) (cddr y)))
                        ((null y))
                      (setf yy (car y))
                      (check-extremes-y))
                    (setf result-array (make-array (length result)
                                                   :element-type 'flonum 
                                                   :initial-contents result))))
               (update-ranges-2d xmin xmax ymin ymax)
               (setf pltcmd (format nil " ~a w l lw ~a lt ~a lc ~a axis ~a"
                                        (make-obj-title (get-option '$key))
                                        (get-option '$line_width)
                                        (get-option '$line_type)
                                        (get-option '$color)
                                        (axes-to-plot)))
               (make-gr-object
                  :name   'explicit
                  :command pltcmd
                  :groups '((2 0))  ; numbers are sent to gnuplot in groups of 2
                  :points  (list result-array )) )
            ((equal (get-option '$filled_func) t)
               (do ((y (cdr result) (cddr y)))
                   ((null y))
                  (setf yy (car y))
                  (check-extremes-y))
               (update-ranges-2d xmin xmax ymin ymax)
               (setf result-array (make-array (length result)
                                              :element-type 'flonum 
                                              :initial-contents result))
               (setf pltcmd (format nil " ~a w filledcurves x1 lc ~a axis ~a"
                                        (make-obj-title (get-option '$key))
                                        (get-option '$fill_color)
                                        (axes-to-plot)))
               (make-gr-object
                  :name   'explicit
                  :command pltcmd
                  :groups '((2 0))  ; numbers are sent to gnuplot in groups of 2
                  :points  (list result-array )))
            (t
               (let (fcn2 yy2 (count -1))
                  (setf result-array (make-array (* (/ (length result) 2) 3) :element-type 'flonum))
                  (setq fcn2 (coerce-float-fun (get-option '$filled_func) `((mlist), var)))
                  (flet ((fun (x) (funcall fcn2 x)))
                    (do ((xx result (cddr xx)))
                      ((null xx))
                      (setf yy  (second xx)
                            yy2 (fun (first xx)))
                      (setf ymax (max ymax yy yy2)
                            ymin (min ymin yy yy2))
                      (setf (aref result-array (incf count)) (first xx)
                            (aref result-array (incf count)) yy
                            (aref result-array (incf count)) yy2) )  ))
               (update-ranges-2d xmin xmax ymin ymax)
               (setf pltcmd (format nil " ~a w filledcurves lc ~a axis ~a"
                                        (make-obj-title (get-option '$key))
                                        (get-option '$fill_color)
                                        (axes-to-plot)  ))
               (make-gr-object
                  :name   'explicit
                  :command pltcmd
                  :groups '((3 0))  ; numbers are sent to gnuplot in groups of 3
                  :points  (list result-array ))))  ))








;; Object: 'implicit'
;; Usage:
;;     implicit(fcn,x-var,x-minval,x-maxval,y-var,y-minval,y-maxval)
;; Options:
;;     ip_grid
;;     ip_grid_in
;;     line_width
;;     line_type
;;     key
;;     color
;;     xaxis_secondary
;;     yaxis_secondary
;; Note: taken from implicit_plot.lisp

(defvar pts ())

(defun contains-zeros (i j sample)
  (not (and (> (* (aref sample i j) (aref sample (1+ i)     j  )) 0)
	    (> (* (aref sample i j) (aref sample     i  (1+ j) )) 0)
	    (> (* (aref sample i j) (aref sample (1+ i) (1+ j) )) 0) )))

(defun sample-data (expr xmin xmax ymin ymax sample grid)
  (let* ((xdelta (/ (- xmax xmin) ($first grid)))
	 (ydelta (/ (- ymax ymin) ($second grid)))
	 (epsilon 1e-6))
    (do ((x-val xmin (+ x-val xdelta))
	 (i 0 (1+ i)))
	((> i ($first grid)))
      (do ((y-val ymin (+ y-val ydelta))
	   (j 0 (1+ j)))
	  ((> j ($second grid)))
	(let ((fun-val (funcall expr x-val y-val)))
	  (if (or (eq fun-val t) (>= fun-val epsilon))
	      (setf (aref sample i j) 1)
	      (setf (aref sample i j) -1)))))))

(defun print-segment (points xmin xdelta ymin ydelta)
  (let* ((point1 (car points)) (point2 (cadr points))
	 (x1 (coerce (+ xmin (/ (* xdelta (+ (car point1) (caddr point1))) 2)) 'flonum) )
	 (y1 (coerce (+ ymin (/ (* ydelta (+ (cadr point1) (cadddr point1))) 2)) 'flonum) )
	 (x2 (coerce (+ xmin (/ (* xdelta (+ (car point2) (caddr point2))) 2)) 'flonum) )
	 (y2 (coerce (+ ymin (/ (* ydelta (+ (cadr point2) (cadddr point2))) 2)) 'flonum) ))
    (setq pts (nconc (list x1 y1 x2 y2) pts))))	

(defun print-square (xmin xmax ymin ymax sample grid)
  (let* ((xdelta (/ (- xmax xmin) ($first grid)))
	 (ydelta (/ (- ymax ymin) ($second grid))))
    (do ((i 0 (1+ i)))
	((= i ($first grid)))
      (do ((j 0 (1+ j)))
	  ((= j ($second grid)))
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
	      (print-segment points xmin xdelta ymin ydelta)) )))))

(defun imp-pl-prepare-factor (expr)
  (cond 
    ((or ($numberp expr) (atom expr))
     expr)
    ((eq (caar expr) 'mexpt)
     (cadr expr))
    (t
     expr)))

(defun imp-pl-prepare-expr (expr)
  (let ((expr1 ($factor (m- ($rhs expr) ($lhs expr)))))
    (cond ((or ($numberp expr) (atom expr1)) expr1)
	  ((eq (caar expr1) 'mtimes)
	   `((mtimes simp factored 1)
	     ,@(mapcar #'imp-pl-prepare-factor (cdr expr1))))
	  ((eq (caar expr) 'mexpt)
	   (imp-pl-prepare-factor expr1))
	  (t
	   expr1))))

(defun implicit (expr x xmin xmax y ymin ymax)
  (let* (($numer t) ($plot_options $plot_options)
         (pts ())
         (expr (m- ($rhs expr) ($lhs expr)))
         (ip-grid (gethash '$ip_grid *gr-options*))
         (ip-grid-in (gethash '$ip_grid_in *gr-options*))
         e pltcmd
         (xmin ($float xmin))
         (xmax ($float xmax))
         (ymin ($float ymin))
         (ymax ($float ymax))
         (xdelta (/ (- xmax xmin) ($first ip-grid)))
         (ydelta (/ (- ymax ymin) ($second ip-grid)))
         (sample (make-array `(,(1+ ($first ip-grid))
			       ,(1+ ($second ip-grid)))))
	 (ssample (make-array `(,(1+ ($first ip-grid-in))
				,(1+ ($second ip-grid-in))))) )
    
    (setq e (coerce-float-fun (imp-pl-prepare-expr expr)
			      `((mlist simp)
				,x ,y)))
    (update-ranges-2d xmin xmax ymin ymax)

    (sample-data e xmin xmax ymin ymax sample ip-grid)
    (do ((i 0 (1+ i)))
	((= i ($first ip-grid)))
      (do ((j 0 (1+ j)))
	  ((= j ($second ip-grid)))
	(if (contains-zeros i j sample)
	    (let* ((xxmin (+ xmin (* i xdelta)))
		   (xxmax (+ xxmin xdelta))
		   (yymin (+ ymin (* j ydelta)))
		   (yymax (+ yymin ydelta)))
	      (sample-data e xxmin xxmax yymin yymax
			   ssample ip-grid-in)
	      (print-square xxmin xxmax yymin yymax
			    ssample ip-grid-in) )) ))
    (setf pltcmd (format nil " ~a w l lw ~a lt ~a lc ~a axis ~a"
                              (make-obj-title (get-option '$key))
                              (get-option '$line_width)
                              (get-option '$line_type)
                              (get-option '$color)
                              (axes-to-plot)))
    (make-gr-object
       :name   'implicit
       :command pltcmd
       :groups '((2 2))
       :points  `(,(make-array (length pts) :element-type 'flonum
                                            :initial-contents pts)) ) ))






;; Object: 'implicit3d'
;; Usage:
;;     implicit(expr,x,xmin,xmax,y,ymin,ymax,z,zmin,zmax)
;; Options:
;;     key
;;     x_voxel
;;     y_voxel
;;     z_voxel
;;     line_width
;;     line_type
;;     color
;;     enhanced3d
(simplify ($load "implicit3d.lisp"))

;; Copies multidimensional arrays.
(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
      (make-array
         dims
         :element-type (array-element-type array)
         :displaced-to array)
      dims)))

;; Calculates surface-edge intersection by interpolation
(defun edge-interpolation (x1 y1 z1 x2 y2 z2 v1 v2)
  (cond ((or (< (abs v1) 0.00001)
             (< (abs (- v1 v2)) 0.00001))
          (list x1 y1 z1))
        ((< (abs v2) 0.00001)
          (list x2 y2 z2))
        (t
          (let ((m (/ (- v1) (- v2 v1))))
            (list
              (+ x1 (* m (- x2 x1)))
              (+ y1 (* m (- y2 y1)))
              (+ z1 (* m (- z2 z1))))))))

(defmacro make-triangle-vertices (i1 j1 k1 i2 j2 k2 e1 e2)
  `(edge-interpolation
       (aref px ,i1) (aref py ,j1) (aref pz ,k1)
       (aref px ,i2) (aref py ,j2) (aref pz ,k2)
       (aref val ,e1) (aref val ,e2)))

(defun flatten (lis)
  (cond ((atom lis) lis)
        ((listp (car lis))
          (append (flatten (car lis)) (flatten (cdr lis))))
        (t
          (append (list (car lis)) (flatten (cdr lis))))))

(defun implicit3d (expr par1 xmin xmax par2 ymin ymax par3 zmin zmax)
  (let* ((nx (gethash '$x_voxel  *gr-options*))
         (ny (gethash '$y_voxel  *gr-options*))
         (nz (gethash '$z_voxel  *gr-options*))
         (xmin ($float xmin))
         (xmax ($float xmax))
         (ymin ($float ymin))
         (ymax ($float ymax))
         (zmin ($float zmin))
         (zmax ($float zmax))
         (dx (/ (- xmax xmin) nx))
         (dy (/ (- ymax ymin) ny))
         (dz (/ (- zmax zmin) nz))
         (fcn (coerce-float-fun (m- ($lhs expr) ($rhs expr)) `((mlist) ,par1 ,par2 ,par3)))
         ($numer t)
         (vertices '())
         (pts '())
         pltcmd
         (grouping '())
         ncols
         (px (make-array (+ nx 1) :element-type 'flonum))
         (py (make-array (+ ny 1) :element-type 'flonum))
         (pz (make-array (+ nz 1) :element-type 'flonum))
         (oldval (make-array `(,(+ nx 1) ,(+ ny 1)) :element-type 'flonum))
         (newval (make-array `(,(+ nx 1) ,(+ ny 1)) :element-type 'flonum)) )
    (check-enhanced3d-model "implicit" '(0 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1 par2 par3)))
    (setf ncols (if (= *draw-enhanced3d-type* 0) 3 4))
    ; initialize coordinate arrays
    (loop for i to nx do (setf (aref px i) (+ xmin (* i dx))))
    (loop for j to ny do (setf (aref py j) (+ ymin (* j dy))))
    (loop for k to nz do (setf (aref pz k) (+ zmin (* k dz))))

    ; initialize first layer
    (loop for i to nx do
      (loop for j to ny do
        (let ((fxy (funcall fcn (aref px i) (aref py j) (aref pz 0))))
          (if (floatp fxy)
            (setf (aref oldval i j) fxy)
            (merror "draw3d (implicit): non real value")))))

    ; begin triangularization process
    (loop for k from 1 to nz do

      ; calculate node values in new layer
      (loop for i to nx do
        (loop for j to ny do
          (let ((fxy (funcall fcn (aref px i) (aref py j) (aref pz k))))
            (if (floatp fxy)
              (setf (aref newval i j) fxy)
              (merror "draw3d (implicit): check surface definition; non real value")))))

      ; analyze voxels in this slide
      (loop for i below nx do
        (loop for j below ny do
          (let* (triangles
                 (cubidx 0)
                 (k-1 (- k 1))
                 (i+1 (+ i 1))
                 (j+1 (+ j 1))
                 (val (make-array 8 :element-type
                                       'flonum
                                    :initial-contents
                                       `(,(aref oldval i j+1) ,(aref oldval i+1 j+1)
                                         ,(aref oldval i+1 j) ,(aref oldval i j)
                                         ,(aref newval i j+1) ,(aref newval i+1 j+1)
                                         ,(aref newval i+1 j) ,(aref newval i j)))))
            (when (< (aref val 0) 0.0) (setf cubidx (logior cubidx 1)))
            (when (< (aref val 1) 0.0) (setf cubidx (logior cubidx 2)))
            (when (< (aref val 2) 0.0) (setf cubidx (logior cubidx 4)))
            (when (< (aref val 3) 0.0) (setf cubidx (logior cubidx 8)))
            (when (< (aref val 4) 0.0) (setf cubidx (logior cubidx 16)))
            (when (< (aref val 5) 0.0) (setf cubidx (logior cubidx 32)))
            (when (< (aref val 6) 0.0) (setf cubidx (logior cubidx 64)))
            (when (< (aref val 7) 0.0) (setf cubidx (logior cubidx 128)))
            (setf triangles (aref *i3d_triangles* cubidx))   ; edges intersecting the surface
            (do ((e 0 (1+ e)))
                ((= (aref triangles e) -1) 'done)
              (push 
                (case (aref triangles e)
                  (0  (make-triangle-vertices i j+1 k-1 i+1 j+1 k-1 0 1))
                  (1  (make-triangle-vertices i+1 j+1 k-1 i+1 j k-1 1 2))
                  (2  (make-triangle-vertices i+1 j k-1 i j k-1 2 3))
                  (3  (make-triangle-vertices i j k-1 i j+1 k-1 3 0))
                  (4  (make-triangle-vertices i j+1 k i+1 j+1 k 4 5))
                  (5  (make-triangle-vertices i+1 j+1 k i+1 j k 5 6))
                  (6  (make-triangle-vertices i+1 j k i j k 6 7))
                  (7  (make-triangle-vertices i j k i j+1 k 7 4))
                  (8  (make-triangle-vertices i j+1 k-1 i j+1 k 0 4))
                  (9  (make-triangle-vertices i+1 j+1 k-1 i+1 j+1 k 1 5))
                  (10 (make-triangle-vertices i+1 j k-1 i+1 j k 2 6))
                  (11 (make-triangle-vertices i j k-1 i j k 3 7)) )
                vertices)))))

      ; make oldval a copy of newval
      (setf oldval (copy-array newval)))

    (when (null vertices)
      (merror "draw3d (implicit): no surface within these ranges"))
    (update-ranges-3d xmin xmax ymin ymax zmin zmax)
    (setf pltcmd
          (cons (format nil " ~a w ~a lw ~a lt ~a lc ~a"
                        (make-obj-title (get-option '$key))
                        (if (equal (get-option '$enhanced3d) '$none) "l" "pm3d")
                        (get-option '$line_width)
                        (get-option '$line_type)
                        (get-option '$color))
                (make-list (- (/ (length vertices) 3) 1)
                           :initial-element (format nil " t '' w ~a lw ~a lt ~a lc ~a"
                                              (if (equal (get-option '$enhanced3d) '$none) "l" "pm3d")
                                              (get-option '$line_width)
                                              (get-option '$line_type)
                                              (get-option '$color) ))))
    (do ((v vertices (cdddr v)))
        ((null v) 'done)
      (case ncols
        (3 (push (make-array 12 :element-type 'flonum
                                :initial-contents (flatten (list (first v) (second v) (first v) (third v))))
                 pts))
        (4 (let (v1 v2 v3
                 color1 color2 color3)
             (setf v1 (first v)
                   v2 (second v)
                   v3 (third v))
             (setf color1 (funcall *draw-enhanced3d-fun* (car v1) (cadr v1) (caddr v1))
                   color2 (funcall *draw-enhanced3d-fun* (car v2) (cadr v2) (caddr v2))
                   color3 (funcall *draw-enhanced3d-fun* (car v3) (cadr v3) (caddr v3)) )
             (push (make-array 16 :element-type 'flonum
                                  :initial-contents (flatten (list v1 color1 v2 color2 v1 color1 v3 color3)))
                    pts))) )
      (push `(,ncols 2)
            grouping) )
    (make-gr-object
       :name    'implicit
       :command pltcmd
       :groups  grouping
       :points  pts)))







;; Object: 'explicit3d'
;; Usage:
;;     explicit(fcn,par1,minval1,maxval1,par2,minval2,maxval2)
;; Options:
;;     xu_grid
;;     yv_grid
;;     line_type
;;     line_width
;;     color
;;     key
;;     enhanced3d
;;     surface_hide
;;     transform
(defun explicit3d (fcn par1 minval1 maxval1 par2 minval2 maxval2)
  (let* ((xu_grid (gethash '$xu_grid  *gr-options*))
         (yv_grid (gethash '$yv_grid  *gr-options*))
         (fminval1 ($float minval1))
         (fminval2 ($float minval2))
         (fmaxval1 ($float maxval1))
         (fmaxval2 ($float maxval2))
         (epsx (/ (- fmaxval1 fminval1) xu_grid))
         (epsy (/ (- fmaxval2 fminval2) yv_grid))
         (xx 0.0) (uu 0.0)
         (yy 0.0) (vv 0.0)
         (zz 0.0)
         (xmin most-positive-double-float)
         (xmax most-negative-double-float)
         (ymin most-positive-double-float)
         (ymax most-negative-double-float)
         (zmin most-positive-double-float)
         (zmax most-negative-double-float)
         (nx (+ xu_grid 1))
         (ny (+ yv_grid 1))
         ($numer t)
         (count -1)
         ncols result)
    (check-enhanced3d-model "explicit" '(0 2 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1 par2)))
    (setq fcn (coerce-float-fun fcn `((mlist) ,par1 ,par2)))
    (setf ncols (if (= *draw-enhanced3d-type* 0) 3 4))
    (setf result (make-array (* ncols nx ny) :element-type 'flonum))
    (loop for j below ny
           initially (setf vv fminval2)
           do (setf uu fminval1)
           (loop for i below nx
                  do
                  (setf xx uu
                        yy vv)
                  (setf zz (funcall fcn xx yy))
                  (transform-point 3)
                  (when (> *draw-transform-dimensions* 0)
                    (check-extremes-x) 
                    (check-extremes-y))
                  (check-extremes-z)
                  (setf (aref result (incf count)) xx
                        (aref result (incf count)) yy
                        (aref result (incf count)) zz)
                  ; check texture model
                  (case *draw-enhanced3d-type*
                    ((2 99) (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* xx yy)))
                    (3  (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* xx yy zz))) )
                  (setq uu (+ uu epsx)))
           (setq vv (+ vv epsy)))
    (when (> *draw-transform-dimensions* 0)
      (setf fminval1 xmin
            fmaxval1 xmax
            fminval2 ymin
            fmaxval2 ymax))
    (update-ranges-3d fminval1 fmaxval1 fminval2 fmaxval2 zmin zmax)
    (make-gr-object
       :name   'explicit
       :command (format nil " ~a w ~a lw ~a lt ~a lc ~a"
                            (make-obj-title (get-option '$key))
                            (if (> *draw-enhanced3d-type* 0) "pm3d" "l")
                            (get-option '$line_width)
                            (get-option '$line_type)
                            (get-option '$color))
       :groups `((,ncols ,nx))
       :points  (list result))))








;; Object: 'elevation_grid'
;; Usage:
;;     elevation_grid(mat,x0,y0,width,height)
;; Options:
;;     line_type
;;     line_width
;;     color
;;     key
;;     enhanced3d
;;     transform
(defun elevation_grid (mat x0 y0 width height)
  (let ( (fx0 ($float x0))
         (fy0 ($float y0))
         (fwidth ($float width))
         (fheight ($float height))
         (xmin most-positive-double-float)
         (xmax most-negative-double-float)
         (ymin most-positive-double-float)
         (ymax most-negative-double-float)
         (zmin most-positive-double-float)
         (zmax most-negative-double-float)
         ncols-file result nrows ncols)
    (check-enhanced3d-model "elevation_grid" '(0 2 3))
    (cond (($matrixp mat)
             (let ((xi 0.0)
                   (yi (+ fy0 fheight))
                   (xx 0.0)
                   (yy 0.0)
                   (zz 0.0)
                   (count -1)
                   dx dy)
                (setf ncols (length (cdadr mat))
                      nrows (length (cdr mat)))
                (setf dx (/ fwidth ncols)
                      dy (/ fheight nrows))
                (setf ncols-file (if (= *draw-enhanced3d-type* 0) 3 4))
                (setf result (make-array (* ncols nrows ncols-file) :element-type 'flonum))
                (loop for row on (cdr mat) by #'cdr do
                   (setf xi fx0)
                   (loop for col on (cdar row) by #'cdr do
                      (setf xx xi
                            yy yi)
                      (setf zz ($float (car col)))
                      (transform-point 3)
                      (when (> *draw-transform-dimensions* 0) 
                        (check-extremes-x)
                        (check-extremes-y))
                      (check-extremes-z)
                      (setf (aref result (incf count)) xx
                            (aref result (incf count)) yy
                            (aref result (incf count)) zz)
                      ; check texture model
                      (case *draw-enhanced3d-type*
                        (2 (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* xx yy)))
                        (3 (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* xx yy zz))) )
                      (setf xi (+ xi dx)))
                   (setf yi (- yi dy)))))
          (t
             (merror "draw3d (elevation_grid): Argument not recognized")))
    (when (= *draw-transform-dimensions* 0)
       (setf xmin fx0
             xmax (+ fx0 fwidth)
             ymin fy0
             ymax (+ fy0 fheight)))
    (update-ranges-3d xmin xmax ymin ymax zmin zmax)
    (make-gr-object
       :name   'elevation_grid
       :command (format nil " ~a w ~a lw ~a lt ~a lc ~a"
                            (make-obj-title (get-option '$key))
                            (if (> *draw-enhanced3d-type* 0) "pm3d" "l")
                            (get-option '$line_width)
                            (get-option '$line_type)
                            (get-option '$color))
       :groups `((,ncols-file ,ncols))
       :points  (list result)) ))








;; Object: 'mesh'
;; Usage:
;;     mesh([[x_11,y_11,z_11], ...,[x_1n,y_1n,z_1n]],
;;          [[x_21,y_21,z_21], ...,[x_2n,y_2n,z_2n]], 
;;          ...,
;;          [[x_m1,y_m1,z_m1], ...,[x_mn,y_mn,z_mn]])
;; Options:
;;     line_type
;;     line_width
;;     color
;;     key
;;     enhanced3d
;;     transform
(defun mesh (&rest row)
  (let (result xx yy zz
        (xmin most-positive-double-float)
        (xmax most-negative-double-float)
        (ymin most-positive-double-float)
        (ymax most-negative-double-float)
        (zmin most-positive-double-float)
        (zmax most-negative-double-float)
        m n ncols-file col-num row-num
        (count -1))
    (cond
      ; let's see if the user wants to use mesh in the old way,
      ; what we now call elevation_grid
      ((and (= (length row) 5)
            ($matrixp (first row)))
        (print "WARNING: Seems like you want to draw an elevation_grid object...")
        (print "         Please, see documentation for object elevation_grid.")
        (apply #'elevation_grid row))
      (t
        (check-enhanced3d-model "mesh" '(0 2 3))
        (when (or (< (length row) 2)
                  (not (every #'$listp row)))
          (merror "draw3d (mesh): Arguments must be two or more lists"))
        (setf ncols-file (if (= *draw-enhanced3d-type* 0) 3 4))
        (setf m (length row)
              n ($length (first row)))
        (setf result (make-array (* m n ncols-file) :element-type 'flonum))
        (setf row-num 0)
        (dolist (r row)
          (incf row-num)
          (setf col-num 0)
          (dolist (c (rest r))
            (incf col-num)
            (setf xx ($float ($first c))
                  yy ($float ($second c))
                  zz ($float ($third c)))
            (transform-point 3)
            (check-extremes-x)
            (check-extremes-y)
            (check-extremes-z)
            (setf (aref result (incf count)) xx
                  (aref result (incf count)) yy
                  (aref result (incf count)) zz)
            ; check texture model
            (case *draw-enhanced3d-type*
              (2 (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* row-num col-num)))
              (3 (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* xx yy zz))))  )   )
        (update-ranges-3d xmin xmax ymin ymax zmin zmax)
        (make-gr-object
          :name   'mesh
          :command (format nil " ~a w ~a lw ~a lt ~a lc ~a"
                           (make-obj-title (get-option '$key))
                           (if (> *draw-enhanced3d-type* 0) "pm3d" "l")
                           (get-option '$line_width)
                           (get-option '$line_type)
                           (get-option '$color))
          :groups `((,ncols-file ,n))
          :points  (list result))))))







;; Object: 'triangle3d'
;; Usage:
;;     triangle([x1,y1,z1], [x2,y2,z2], [x3,y3,z3])
;; Options:
;;     line_type
;;     line_width
;;     color
;;     key
;;     enhanced3d
;;     transform
(defun triangle3d (arg1 arg2 arg3)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 3))
           (not ($listp arg2))
           (not (= ($length arg2) 3))
           (not ($listp arg3))
           (not (= ($length arg3) 3)))
       (merror "draw3d (triangle): vertices are not correct"))
   (let* ((x1 ($float (cadr arg1)))
          (y1 ($float (caddr arg1)))
          (z1 ($float (cadddr arg1)))
          (x2 ($float (cadr arg2)))
          (y2 ($float (caddr arg2)))
          (z2 ($float (cadddr arg2)))
          (x3 ($float (cadr arg3)))
          (y3 ($float (caddr arg3)))
          (z3 ($float (cadddr arg3)))
          (grobj (mesh `((mlist simp)
                            ((mlist simp) ,x1 ,y1 ,z1)
                            ((mlist simp) ,x1 ,y1 ,z1) )

                       `((mlist simp)
                            ((mlist simp) ,x2 ,y2 ,z2)
                            ((mlist simp) ,x3 ,y3 ,z3) ) )))
      (setf (gr-object-name grobj) 'triangle)
      grobj))







;; Object: 'quadrilateral3d'
;; Usage:
;;     quadrilateral([x1,y1,z1], [x2,y2,z2], [x3,y3,z3], [x4,y4,z4])
;; Options:
;;     line_type
;;     line_width
;;     color
;;     key
;;     enhanced3d
;;     transform
(defun quadrilateral3d (arg1 arg2 arg3 arg4)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 3))
           (not ($listp arg2))
           (not (= ($length arg2) 3))
           (not ($listp arg3))
           (not (= ($length arg3) 3))
           (not ($listp arg4))
           (not (= ($length arg4) 3)))
       (merror "draw3d (quadrilateral): vertices are not correct"))
   (let* ((x1 ($float (cadr arg1)))
          (y1 ($float (caddr arg1)))
          (z1 ($float (cadddr arg1)))
          (x2 ($float (cadr arg2)))
          (y2 ($float (caddr arg2)))
          (z2 ($float (cadddr arg2)))
          (x3 ($float (cadr arg3)))
          (y3 ($float (caddr arg3)))
          (z3 ($float (cadddr arg3)))
          (x4 ($float (cadr arg4)))
          (y4 ($float (caddr arg4)))
          (z4 ($float (cadddr arg4)))
          (grobj (mesh `((mlist simp)
                         ((mlist simp) ,x1 ,y1 ,z1)
                         ((mlist simp) ,x2 ,y2 ,z2))
                       `((mlist simp)
                         ((mlist simp) ,x3 ,y3 ,z3)
                         ((mlist simp) ,x4 ,y4 ,z4)))))
      (setf (gr-object-name grobj) 'quadrilateral)
      grobj))







;; Object: 'parametric'
;; Usage:
;;     parametric(xfun,yfun,par,parmin,parmax)
;; Options:
;;     nticks
;;     line_width
;;     line_type
;;     key
;;     color
;;     xaxis_secondary
;;     yaxis_secondary
;;     transform
;; Note: similar to draw2d-parametric in plot.lisp
(defun parametric (xfun yfun par parmin parmax)
  (let* ((nticks (gethash '$nticks  *gr-options*))
         ($numer t)
         (tmin ($float parmin))
         (tmax ($float parmax))
         (xmin most-positive-double-float)
         (xmax most-negative-double-float)
         (ymin most-positive-double-float)
         (ymax most-negative-double-float)
         (tt ($float parmin))
         (eps (/ (- tmax tmin) (- nticks 1)))
         result f1 f2 xx yy)
    (if (< tmax tmin)
       (merror "draw2d (parametric): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist), par)))
    (setq f2 (coerce-float-fun yfun `((mlist), par)))
    (setf result
       (loop
          do (setf xx ($float (funcall f1 tt)))
             (setf yy ($float (funcall f2 tt)))
             (transform-point 2)
             (check-extremes-x)
             (check-extremes-y)
          collect xx
          collect yy
          when (>= tt tmax) do (loop-finish)
          do (setq tt (+ tt eps))
             (if (>= tt tmax) (setq tt tmax)) ))
    ; update x-y ranges if necessary
    (update-ranges-2d xmin xmax ymin ymax)
    (make-gr-object
       :name 'parametric
       :command (format nil " ~a w l lw ~a lt ~a lc ~a axis ~a"
                            (make-obj-title (get-option '$key))
                            (get-option '$line_width)
                            (get-option '$line_type)
                            (get-option '$color)
                            (axes-to-plot))
       :groups '((2 0))
       :points `(,(make-array (length result) :element-type 'flonum
                                              :initial-contents result)))   ) )







;; Object: 'polar'
;; Usage:
;;     polar(radius,ang,minang,maxang)
;; Options:
;;     nticks
;;     line_width
;;     line_type
;;     key
;;     color
;;     xaxis_secondary
;;     yaxis_secondary
;; This object is constructed as a parametric function
(defun polar (radius ang minang maxang)
  (let ((grobj (parametric `((mtimes simp) ,radius ((%cos simp) ,ang))
                            `((mtimes simp) ,radius ((%sin simp) ,ang))
                            ang minang maxang)))
    (setf (gr-object-name grobj) 'polar)
    grobj ))







;; Object: 'spherical'
;; Usage:
;;     spherical(radius,az,minazi,maxazi,zen,minzen,maxzen)
;; Options:
;;     xu_grid
;;     yv_grid
;;     line_type
;;     color
;;     key
;;     enhanced3d
;; This object is constructed as a parametric surface in 3d.
;; Functions are defined in format r=r(azimuth,zenith),
;; where, normally, azimuth is an angle in [0,2*%pi] and zenith in [0,%pi]
(defun spherical (radius azi minazi maxazi zen minzen maxzen)
  (let ((grobj (parametric_surface
                     `((mtimes simp) ,radius ((%sin simp) ,zen) ((%cos simp) ,azi))
                     `((mtimes simp) ,radius ((%sin simp) ,zen) ((%sin simp) ,azi))
                     `((mtimes simp) ,radius ((%cos simp) ,zen))
                     azi minazi maxazi
                     zen minzen maxzen)))
    (setf (gr-object-name grobj) 'spherical)
    grobj ))








;; Object: 'cylindrical'
;; Usage:
;;     cylindrical(r,z,minz,maxz,azi,minazi,maxazi)
;; Options:
;;     xu_grid
;;     yv_grid
;;     line_type
;;     color
;;     key
;; This object is constructed as a parametric surface in 3d.
;; Functions are defined in format z=z(radius,azimuth), where,
;; normally, azimuth is an angle in [0,2*%pi] and r any real
(defun cylindrical (r z minz maxz azi minazi maxazi)
  (let ((grobj (parametric_surface
                     `((mtimes simp) ,r ((%cos simp) ,azi))
                     `((mtimes simp) ,r ((%sin simp) ,azi))
                     z 
                     z minz maxz
                     azi minazi maxazi)))
    (setf (gr-object-name grobj) 'cylindrical)
    grobj ))








;; Object: 'parametric3d'
;; Usage:
;;     parametric(xfun,yfun,zfun,par1,parmin,parmax)
;; Options:
;;     nticks
;;     line_width
;;     line_type
;;     color
;;     key
;;     enhanced3d
;;     surface_hide
;;     transform
(defun parametric3d (xfun yfun zfun par1 parmin parmax)
  (let* ((nticks (gethash '$nticks  *gr-options*))
         ($numer t)
         (tmin ($float parmin))
         (tmax ($float parmax))
         (xmin most-positive-double-float)
         (xmax most-negative-double-float)
         (ymin most-positive-double-float)
         (ymax most-negative-double-float)
         (zmin most-positive-double-float)
         (zmax most-negative-double-float)
         (tt tmin)
         (eps (/ (- tmax tmin) (- nticks 1)))
         (count -1)
         ncols result f1 f2 f3 xx yy zz)
    (check-enhanced3d-model "parametric" '(0 1 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1)))
    (if (< tmax tmin)
       (merror "draw3d (parametric): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist) ,par1)))
    (setq f2 (coerce-float-fun yfun `((mlist) ,par1)))
    (setq f3 (coerce-float-fun zfun `((mlist) ,par1)))
    (setf ncols (if (= *draw-enhanced3d-type* 0) 3 4))
    (setf result (make-array (* ncols nticks) :element-type 'flonum))
    (dotimes (k nticks)
      (setf xx (funcall f1 tt))
      (setf yy (funcall f2 tt))
      (setf zz (funcall f3 tt))
      (transform-point 3)
      (check-extremes-x)
      (check-extremes-y)
      (check-extremes-z)
      (setf (aref result (incf count)) xx)
      (setf (aref result (incf count)) yy)
      (setf (aref result (incf count)) zz)
      ; check texture model
      (case *draw-enhanced3d-type*
        ((1 99) (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* tt)))
        (3      (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* xx yy zz))))
      (setf tt (+ tt eps)) )
    ; update x-y ranges if necessary
    (update-ranges-3d xmin xmax ymin ymax zmin zmax)
    (make-gr-object
       :name 'parametric
       :command (format nil " ~a w l lw ~a lt ~a lc ~a"
                            (make-obj-title (get-option '$key))
                            (get-option '$line_width)
                            (get-option '$line_type)
                            (if (> *draw-enhanced3d-type* 0) "palette" (get-option '$color)) )
       :groups `((,ncols 0))
       :points (list result) )) )








;; Object: 'parametric_surface'
;; Usage:
;;     parametric_surface(xfun,yfun,zfun,par1,par1min,par1max,par2,par2min,par2max)
;; Options:
;;     xu_grid
;;     yv_grid
;;     line_type
;;     line_width
;;     color
;;     key
;;     enhanced3d
;;     surface_hide
;;     transform
(defun parametric_surface (xfun yfun zfun par1 par1min par1max par2 par2min par2max)
  (let* ((ugrid (gethash '$xu_grid  *gr-options*))
         (vgrid (gethash '$yv_grid  *gr-options*))
         ($numer t)
         (umin ($float par1min))
         (umax ($float par1max))
         (vmin ($float par2min))
         (vmax ($float par2max))
         (xmin most-positive-double-float)
         (xmax most-negative-double-float)
         (ymin most-positive-double-float)
         (ymax most-negative-double-float)
         (zmin most-positive-double-float)
         (zmax most-negative-double-float)
         (ueps (/ (- umax umin) (- ugrid 1)))
         (veps (/ (- vmax vmin) (- vgrid 1)))
         (nu (+ ugrid 1))
         (nv (+ vgrid 1))
         (count -1)
         ncols result f1 f2 f3 xx yy zz uu vv)
    (check-enhanced3d-model "parametric_surface" '(0 2 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1 par2)))
    (if (or (< umax umin)
            (< vmax vmin))
       (merror "draw3d (parametric_surface): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist) ,par1 ,par2)))
    (setq f2 (coerce-float-fun yfun `((mlist) ,par1 ,par2)))
    (setq f3 (coerce-float-fun zfun `((mlist) ,par1 ,par2)))
    (setf ncols (if (= *draw-enhanced3d-type* 0) 3 4))
    (setf result (make-array (* ncols nu nv) :element-type 'flonum))
    (loop for j below nv
           initially (setq vv vmin)
           do (setq uu umin)
           (loop for i below nu
                  do
                  (setf xx (funcall f1 uu vv))
                  (setf yy (funcall f2 uu vv))
                  (setf zz (funcall f3 uu vv))
                  (transform-point 3)
                  (check-extremes-x)
                  (check-extremes-y)
                  (check-extremes-z)
                  (setf (aref result (incf count)) xx)
                  (setf (aref result (incf count)) yy)
                  (setf (aref result (incf count)) zz)
                  ; check texture model
                  (case *draw-enhanced3d-type*
                    ((2 99) (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* uu vv)))
                    (3      (setf (aref result (incf count)) (funcall *draw-enhanced3d-fun* xx yy zz))) )
                  (setq uu (+ uu ueps))
                  (if (> uu umax) (setf uu umax)))
           (setq vv (+ vv veps))
           (if (> vv vmax) (setf vv vmax)))
    ; update x-y-z ranges if necessary
    (update-ranges-3d xmin xmax ymin ymax zmin zmax)
    (make-gr-object
       :name 'parametric_surface
       :command (format nil " ~a w ~a lw ~a lt ~a lc ~a"
                            (make-obj-title (get-option '$key))
                            (if (> *draw-enhanced3d-type* 0) "pm3d" "l")
                            (get-option '$line_width)
                            (get-option '$line_type)
                            (get-option '$color))
       :groups `((,ncols ,nu)) ; ncols is 4 or 3, depending on colored 4th dimension or not
       :points (list result))))







;; Object: 'tube'
;; Usage:
;;     tube(xfun,yfun,zfun,rad,par1,parmin,parmax)
;; Options:
;;     xu_grid
;;     yv_grid
;;     line_type
;;     line_width
;;     color
;;     key
;;     enhanced3d
;;     surface_hide
;;     transform
(defmacro check-tube-extreme (ex cx cy cz circ)
    `(when (equal (nth ,ex (get-option '$tube_extremes)) '$closed)
       (let ((cxx ,cx)
             (cyy ,cy)
             (czz ,cz))
          (when (> *draw-transform-dimensions* 0)
            (setf cxx (funcall *draw-transform-f1* ,cx ,cy ,cz)
                  cyy (funcall *draw-transform-f2* ,cx ,cy ,cz)
                  czz (funcall *draw-transform-f3* ,cx ,cy ,cz)))
          (case *draw-enhanced3d-type*
            (0      (setf ,circ (list cxx cyy czz)))
            ((1 99) (setf ,circ (list cxx cyy czz (funcall *draw-enhanced3d-fun* tt))))
            (3      (setf ,circ (list cxx cyy czz (funcall *draw-enhanced3d-fun* cxx cyy czz)))))
          (dotimes (k vgrid)
            (setf result (append result ,circ))))))

(defun tube (xfun yfun zfun rad par1 parmin parmax)
  (let* ((ugrid (get-option '$xu_grid))
         (vgrid (get-option '$yv_grid))
         ($numer t)
         (tmin ($float parmin))
         (tmax ($float parmax))
         (vmax 6.283185307179586) ; = float(2*%pi)
         (xmin most-positive-double-float)
         (xmax most-negative-double-float)
         (ymin most-positive-double-float)
         (ymax most-negative-double-float)
         (zmin most-positive-double-float)
         (zmax most-negative-double-float)
         (teps (/ (- tmax tmin) (- ugrid 1)))
         (veps (/ vmax (- vgrid 1)))
         (tt tmin)
         ncols circ result
         f1 f2 f3 radius
         cx cy cz nx ny nz
         ux uy uz vx vy vz
         xx yy zz module r vv rcos rsin
         cxold cyold czold
         uxold uyold uzold ttnext)
    (when (< tmax tmin)
       (merror "draw3d (tube): illegal range"))
    (check-enhanced3d-model "tube" '(0 1 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1)))
    (setq f1 (coerce-float-fun xfun `((mlist) ,par1)))
    (setq f2 (coerce-float-fun yfun `((mlist) ,par1)))
    (setq f3 (coerce-float-fun zfun `((mlist) ,par1)))
    (setf ncols (if (= *draw-enhanced3d-type* 0) 3 4))
    (setf radius
          (coerce-float-fun rad `((mlist) ,par1)))
    (loop do
      ; calculate center and radius of circle
      (cond
        ((= tt tmin)  ; 1st iteration
           (setf cx (funcall f1 tt)
                 cy (funcall f2 tt)
                 cz (funcall f3 tt)
                 ttnext (+ tt teps))
           (check-tube-extreme 1 cx cy cz circ)
           (setf nx (- (funcall f1 ttnext) cx)
                 ny (- (funcall f2 ttnext) cy)
                 nz (- (funcall f3 ttnext) cz)))
        (t  ; all next iterations along the parametric curve
           (setf cxold cx
                 cyold cy
                 czold cz)
           (setf cx (funcall f1 tt)
                 cy (funcall f2 tt)
                 cz (funcall f3 tt))
           (setf nx (- cx cxold)
                 ny (- cy cyold)
                 nz (- cz czold))))
      (setf r (funcall radius tt))
      ; calculate the unitary normal vector      
      (setf module (sqrt (+ (* nx nx) (* ny ny) (* nz nz))))
      (setf nx (/ nx module)
            ny (/ ny module)
            nz (/ nz module))
      ; calculate unitary vector perpendicular to n=(nx,ny,nz)
      ; ux.nx+uy.ny+uz.nz=0 => ux=-t(ny+nz)/nx, uy=uz=t
      ; let's take t=1
      (cond
        ((= nx 0.0)
           (setf ux 1.0 uy 0.0 uz 0.0))
        ((= ny 0.0)
           (setf ux 0.0 uy 1.0 uz 0.0))
        ((= nz 0.0)
           (setf ux 0.0 uy 0.0 uz 1.0))
        (t  ; all other cases
           (setf ux (- (/ (+ ny nz) nx))
                 uy 1.0
                 uz 1.0)))
      (setf module (sqrt (+ (* ux ux) (* uy uy) (* uz uz))))
      (setf ux (/ ux module)
            uy (/ uy module)
            uz (/ uz module))
      (when (and (> tt tmin)
                 (< (+ (* uxold ux)
                       (* uyold uy)
                       (* uzold uz))
                    0))
        (setf ux (- ux)
              uy (- uy)
              uz (- uz)))
      (setf uxold ux
            uyold uy
            uzold uz)
      ; vector v = n times u
      (setf vx (- (* ny uz) (* nz uy))
            vy (- (* nz ux) (* nx uz))
            vz (- (* nx uy) (* ny ux)))
      ; parametric equation of the circumference of radius
      ; r and centered at c=(cx,cy,cz):
      ; x(t) = c + r(cos(t)u + sin(t)v),
      ; for t in (0, 2*%pi)
      (setf vv 0.0)
      (setf circ '())
      (loop for i below vgrid do
        (setf rcos (* r (cos vv))
              rsin (* r (sin vv)))
        (setf xx (+ cx (* rcos ux) (* rsin vx))
              yy (+ cy (* rcos uy) (* rsin vy))
              zz (+ cz (* rcos uz) (* rsin vz)))
        (transform-point 3)
        (check-extremes-x)
        (check-extremes-y)
        (check-extremes-z)
        ; check texture model
        (case *draw-enhanced3d-type*
          (0      (setf circ (cons (list xx yy zz) circ)))
          ((1 99) (setf circ (cons (list xx yy zz (funcall *draw-enhanced3d-fun* tt)) circ)))
          (3      (setf circ (cons (list xx yy zz (funcall *draw-enhanced3d-fun* xx yy zz)) circ))))
        (setf vv (+ vv veps))
        (when (> vv vmax) (setf vv vmax))  ) ; loop for
      (setf result (append result (apply #'append circ)))
      when (>= tt tmax) do (loop-finish)
      do (setf tt (+ tt teps))
         (when (> tt tmax) (setf tt tmax))  ) ; loop do
      (check-tube-extreme 2 cx cy cz circ)
    ; update x-y-z ranges
    (update-ranges-3d xmin xmax ymin ymax zmin zmax)
    (make-gr-object
       :name 'tube
       :command (format nil " ~a w ~a lw ~a lt ~a lc ~a"
                            (make-obj-title (get-option '$key))
                            (if (> *draw-enhanced3d-type* 0) "pm3d" "l")
                            (get-option '$line_width)
                            (get-option '$line_type)
                            (get-option '$color))
       :groups `((,ncols ,vgrid))
       :points `(,(make-array (length result) :element-type 'flonum
                                              :initial-contents result)))))







;; Object: 'image'
;; Usages:
;;     image(matrix_of_numbers,x0,y0,width,height)
;;     image(matrix_of_[r,g,b],x0,y0,width,height)
;;     image(picture_object,x0,y0,width,height)
;; Options:
;;     colorbox
;;     palette
(simplify ($load "picture.lisp"))  ; loads picture package
(defun image (mat x0 y0 width height)
  (let ( (fx0 ($float x0))
         (fy0 ($float y0))
         (fwidth ($float width))
         (fheight ($float height))
         result nrows ncols dx dy n)
    (cond (($matrixp mat)
             (setf nrows (length (cdr mat))
                   ncols (length (cdadr mat)))
             (setf dx (/ fwidth ncols)
                   dy (/ fheight nrows))
             (if (not ($listp (cadadr mat)))  ; it's a matrix of reals
                 (setf n 3)   ; 3 numbers to be sent to gnuplot: x,y,value
                 (setf n 5))  ; 5 numbers to be sent: x,y,r,g,b
             (case n
               (3 (setf result (make-array (* 3 nrows ncols) :element-type 'flonum))
                  (let ((yi (+ fy0 height (* dy -0.5)))
                        (counter -1)
                         xi)
                     (loop for row on (cdr mat) by #'cdr do
                       (setf xi (+ fx0 (* dx 0.5)))
                       (loop for col on (cdar row) by #'cdr do
                         (setf (aref result (incf counter)) xi
                               (aref result (incf counter)) yi
                               (aref result (incf counter)) ($float (car col)))
                         (setf xi (+ xi dx)))
                       (setf yi (- yi dy)) )))
               (5 (setf result (make-array (* 5 nrows ncols) :element-type 'flonum))
                  (let ((yi (+ fy0 height (* dy -0.5)))
                        (counter -1)
                         xi colors)
                     (loop for row on (cdr mat) by #'cdr do
                       (setf xi (+ fx0 (* dx 0.5)))
                       (loop for col on (cdar row) by #'cdr do
                         (setf colors (cdar col))
                         (setf (aref result (incf counter)) xi
                               (aref result (incf counter)) yi
                               (aref result (incf counter)) ($float (car colors))
                               (aref result (incf counter)) ($float (cadr colors))
                               (aref result (incf counter)) ($float (caddr colors)))
                         (setf xi (+ xi dx)))
                       (setf yi (- yi dy)) )))))
          (($picturep mat)
             (setf nrows (nth 3 mat)   ; picture height
                   ncols (nth 2 mat))  ; picture width
             (setf dx (/ fwidth ncols)
                   dy (/ fheight nrows))
             (if (equal (nth 1 mat) '$level)  ; gray level picture
                 (setf n 3)   ; 3 numbers to be sent to gnuplot: x,y,value
                 (setf n 5))  ; 5 numbers to be sent: x,y,r,g,b
             (setf result (make-array (* n nrows ncols) :element-type 'flonum))
             (let ((yi (+ fy0 height (* dy -0.5)))
                   (count1 -1)
                   (count2 -1)
                   xi)
                (loop for r from 0 below nrows do
                  (setf xi (+ fx0 (* dx 0.5)))
                  (loop for c from 0 below ncols do
                    (setf (aref result (incf count1)) xi)
                    (setf (aref result (incf count1)) yi)
                    (loop for q from 3 to n do
                      (setf (aref result (incf count1))
                            ($float (aref (nth 4 mat) (incf count2)))))
                    (setf xi (+ xi dx)))
                  (setf yi (- yi dy)))))
          (t
             (merror "draw2d (image): Argument not recognized")))
    ; update x-y ranges if necessary
    (update-ranges-2d fx0 (+ fx0 fwidth) fy0 (+ fy0 fheight))
    (make-gr-object
       :name 'image
       :command (case n
                   (3 (format nil " t '' w image"))
                   (5 (format nil " t '' w rgbimage")))
       :groups (case n
                   (3 '((3 0)))   ; numbers are sent to gnuplot in gropus of 3, no blank lines
                   (5 '((5 0))  ))  ; numbers in groups of 5, no blank lines
       :points (list result)) ) )










;; transforms arguments to make-scene-2d, make-scene-3d
;; and draw to a unique list. With this piece of code,
;; gr2d, gr3d and draw admit as arguments lists of options
;; and graphic objects.
(defmacro make-list-of-arguments ()
   '(setf largs (rest (apply 
                         #'$append
                        (map 
                          'list #'(lambda (z) (if ($listp z) z (list '(mlist) z)))
                          args)))))

(defvar *2d-graphic-objects* (make-hash-table))

; table of basic 2d graphic objects
(setf (gethash '$points        *2d-graphic-objects*) 'points
      (gethash '$polygon       *2d-graphic-objects*) 'polygon
      (gethash '$ellipse       *2d-graphic-objects*) 'ellipse
      (gethash '$triangle      *2d-graphic-objects*) 'triangle
      (gethash '$rectangle     *2d-graphic-objects*) 'rectangle
      (gethash '$quadrilateral *2d-graphic-objects*) 'quadrilateral
      (gethash '$explicit      *2d-graphic-objects*) 'explicit
      (gethash '$implicit      *2d-graphic-objects*) 'implicit
      (gethash '$parametric    *2d-graphic-objects*) 'parametric
      (gethash '$vector        *2d-graphic-objects*) 'vect
      (gethash '$label         *2d-graphic-objects*) 'label
      (gethash '$bars          *2d-graphic-objects*) 'bars
      (gethash '$polar         *2d-graphic-objects*) 'polar
      (gethash '$image         *2d-graphic-objects*) 'image
      (gethash '$points        *2d-graphic-objects*) 'points )

(defun make-scene-2d (args)
   (let ((objects nil)
         plotcmd largs)
      (ini-gr-options)
      (ini-local-option-variables)
      (user-defaults)
      (make-list-of-arguments)
      ; update option values and detect objects to be plotted
      (dolist (x largs)
         (cond ((equal ($op x) "=")
                   (update-gr-option ($lhs x) ($rhs x)))
               (t  (setf objects
                         (append
                            objects 
                            (list (apply (gethash (caar x) *2d-graphic-objects*) (rest x))))))))
      ; save in plotcmd the gnuplot preamble
      (setf plotcmd
         (concatenate 'string
            (if (equal (get-option '$proportional_axes) '$none)
               (format nil "set size noratio~%")
               (format nil "set size ratio -1~%") )
            ; this let statement is to prevent error messages from gnuplot when
            ; the amplitude of the ranges equals zero
            (let ((xi (first  (get-option '$xrange)))
                  (xf (second (get-option '$xrange)))
                  (yi (first  (get-option '$yrange)))
                  (yf (second (get-option '$yrange)))
                  (x2i (first  (get-option '$xrange_secondary)))
                  (x2f (second (get-option '$xrange_secondary)))
                  (y2i (first  (get-option '$yrange_secondary)))
                  (y2f (second (get-option '$yrange_secondary))) )
               (when (and (get-option '$xrange) (= xi xf))
                  (setf xi (- xi 0.01)
                        xf (+ xf 0.01)))
               (when (and (get-option '$xrange_secondary) (= x2i x2f))
                  (setf x2i (- x2i 0.01)
                        x2f (+ x2f 0.01)))
               (when (and (get-option '$yrange) (= yi yf))
                  (setf yi (- yi 0.01)
                        yf (+ yf 0.01)))
               (when (and (get-option '$yrange_secondary) (= y2i y2f))
                  (setf y2i (- y2i 0.01)
                        y2f (+ y2f 0.01)))
               (format nil "~a~a~a~a"
                       (if (get-option '$xrange)
                         (format nil "set xrange [~a:~a]~%" xi xf)
                         "")
                       (if (get-option '$xrange_secondary)
                         (format nil "set x2range [~a:~a]~%" x2i x2f)
                         "")
                       (if (get-option '$yrange)
                         (format nil "set yrange [~a:~a]~%" yi yf)
                         "")
                       (if (get-option '$yrange_secondary)
                         (format nil "set y2range [~a:~a]~%" y2i y2f)
                         "") ) )
            (if (get-option '$cbrange)
               (format nil "set cbrange [~a:~a]~%"
                  (first (get-option '$cbrange))
                  (second (get-option '$cbrange)))
               (format nil "set cbrange [*:*]~%") )
            (if (get-option '$logx)
               (format nil "set logscale x~%")
               (format nil "unset logscale x~%"))
            (if (get-option '$logy)
               (format nil "set logscale y~%")
               (format nil "unset logscale y~%"))
            (if (get-option '$logcb)
               (format nil "set logscale cb~%")
               (format nil "unset logscale cb~%") )
            (if (get-option '$grid)
                (format nil "set grid~%")
                (format nil "unset grid~%"))
            (format nil "set title '~a'~%"  (get-option '$title))
            (format nil "set xlabel '~a'~%" (get-option '$xlabel))
            (format nil "set ylabel '~a'~%" (get-option '$ylabel))
            (let ((suma 0))
              (if (get-option '$axis_bottom)  (setf suma (+ suma 1)))
              (if (get-option '$axis_left) (setf suma (+ suma 2)))
              (if (get-option '$axis_top) (setf suma (+ suma 4)))
              (if (get-option '$axis_right) (setf suma (+ suma 8)))
              (format nil "set border ~a~%" suma) )
            (if (get-option '$xaxis)
               (format nil "set xzeroaxis lw ~a lt ~a lc ~a~%"
                           (get-option '$xaxis_width)
                           (get-option '$xaxis_type)
                           (get-option '$xaxis_color) )
               (format nil "unset xzeroaxis~%"))
            (if (get-option '$yaxis)
               (format nil "set yzeroaxis lw ~a lt ~a lc ~a~%"
                           (get-option '$yaxis_width)
                           (get-option '$yaxis_type)
                           (get-option '$yaxis_color) )
               (format nil "unset yzeroaxis~%"))
            (if (null (get-option '$xtics))
               (format nil "unset xtics~%")
               (format nil "set xtics ~a ~a ~a~%" 
                       (if (get-option '$xtics_rotate) "rotate" "norotate")
                       (if (get-option '$xtics_axis) "axis" "border")
                       (get-option '$xtics)))
            (if (null (get-option '$xtics_secondary))
               (format nil "unset x2tics~%")
               (format nil "set xtics nomirror~%set x2tics ~a ~a ~a~%"
                       (if (get-option '$xtics_secondary_rotate) "rotate" "norotate")
                       (if (get-option '$xtics_secondary_axis) "axis" "border")
                       (get-option '$xtics_secondary)))
            (if (null (get-option '$ytics))
               (format nil "unset ytics~%")
               (format nil "set ytics ~a ~a ~a~%"
                       (if (get-option '$ytics_rotate) "rotate" "norotate")
                       (if (get-option '$ytics_axis) "axis" "border")
                       (get-option '$ytics)))
            (if (null (get-option '$ytics_secondary))
               (format nil "unset y2tics~%")
               (format nil "set ytics nomirror~%set y2tics ~a ~a ~a~%"
                       (if (get-option '$ytics_secondary_rotate) "rotate" "norotate")
                       (if (get-option '$ytics_secondary_axis) "axis" "border")
                       (get-option '$ytics_secondary)))
            (if (null (get-option '$cbtics))
               (format nil "unset cbtics~%")
               (format nil "set cbtics ~a~%" (get-option '$cbtics) ))
            (if (get-option '$colorbox)
               (format nil "set colorbox~%")
               (format nil "unset colorbox~%"))
            (format nil "set cblabel '~a'~%" 
                        (if (stringp (get-option '$colorbox))
                          (get-option '$colorbox)
                          ""))
            (let ((pal (get-option '$palette)))
              (case pal
                 ($gray     (format nil "set palette gray~%"))
                 ($color    (format nil "set palette rgbformulae 7,5,15~%"))
                 (otherwise (format nil "set palette rgbformulae ~a,~a,~a~%"
                                        (car pal) (cadr pal) (caddr pal))) ))
            (if (not (string= (get-option '$user_preamble) ""))
               (format nil "~a~%" (get-option '$user_preamble))) ) )
      ; scene description: (dimensions, gnuplot preamble in string format, list of objects)
      (list
         2       ; it's a 2d scene
         plotcmd ; gnuplot preamble
         objects ; list of objects to be plotted
         )))






(defvar *3d-graphic-objects* (make-hash-table))

; table of basic 3d graphic objects
(setf (gethash '$points             *3d-graphic-objects*) 'points3d
      (gethash '$elevation_grid     *3d-graphic-objects*) 'elevation_grid
      (gethash '$mesh               *3d-graphic-objects*) 'mesh
      (gethash '$triangle           *3d-graphic-objects*) 'triangle3d
      (gethash '$quadrilateral      *3d-graphic-objects*) 'quadrilateral3d
      (gethash '$explicit           *3d-graphic-objects*) 'explicit3d
      (gethash '$implicit           *3d-graphic-objects*) 'implicit3d
      (gethash '$parametric         *3d-graphic-objects*) 'parametric3d
      (gethash '$vector             *3d-graphic-objects*) 'vect3d
      (gethash '$label              *3d-graphic-objects*) 'label
      (gethash '$parametric_surface *3d-graphic-objects*) 'parametric_surface
      (gethash '$tube               *3d-graphic-objects*) 'tube
      (gethash '$spherical          *3d-graphic-objects*) 'spherical
      (gethash '$cylindrical        *3d-graphic-objects*) 'cylindrical )

;; This function builds a 3d scene by calling the 
;; graphic objects constructors.
(defun make-scene-3d (args)
   (let ((objects nil)
         plotcmd largs)
      (ini-gr-options)
      (ini-local-option-variables)
      (user-defaults)
      (make-list-of-arguments)
      ; update option values and detect objects to be plotted
      (dolist (x largs)
         (cond ((equal ($op x) "=")
                  (update-gr-option ($lhs x) ($rhs x)))
               (t  (setf objects
                         (append
                            objects 
                            (list (apply (gethash (caar x) *3d-graphic-objects*) (rest x))))))))
      ; save in plotcmd the gnuplot preamble
      (setf plotcmd
         (concatenate 'string
            ; this let statement is to prevent error messages in gnuplot when
            ; the amplitude of the ranges equals zero
            (let ((xi (first  (get-option '$xrange)))
                  (xf (second (get-option '$xrange)))
                  (yi (first  (get-option '$yrange)))
                  (yf (second (get-option '$yrange)))
                  (zi (first  (get-option '$zrange)))
                  (zf (second (get-option '$zrange))))
               (when (= xi xf)
                  (setf xi (- xi 0.01)
                        xf (+ xf 0.01)))
               (when (= yi yf)
                  (setf yi (- yi 0.01)
                        yf (+ yf 0.01)))
               (when (= zi zf)
                  (setf zi (- zi 0.01)
                        zf (+ zf 0.01)))
               (format nil "set xrange [~a:~a]~%set yrange [~a:~a]~%set zrange [~a:~a]~%"
                           xi xf yi yf zi zf))
            (if (get-option '$cbrange)
               (format nil "set cbrange [~a:~a]~%" 
                  (first (get-option '$cbrange))
                  (second (get-option '$cbrange)) )
               (format nil "set cbrange [*:*]~%") )
            (case (get-option '$contour)
               ($surface (format nil "set contour surface;set cntrparam levels ~a~%"
                                      (get-option '$contour_levels) ))
               ($base    (format nil "set contour base;set cntrparam levels ~a~%"
                                      (get-option '$contour_levels) ))
               ($both    (format nil "set contour both;set cntrparam levels ~a~%"
                                      (get-option '$contour_levels) ))
               ($map     (format nil "set contour base~%unset surface~%set cntrparam levels ~a~%"
                                      (get-option '$contour_levels))) )
            (format nil "set title '~a'~%"  (get-option '$title))
            (format nil "set xlabel '~a'~%" (get-option '$xlabel))
            (format nil "set ylabel '~a'~%" (get-option '$ylabel))
            (format nil "set zlabel '~a'~%" (get-option '$zlabel))
            (if (get-option '$logx)
               (format nil "set logscale x~%")
               (format nil "unset logscale x~%"))
            (if (get-option '$logy)
               (format nil "set logscale y~%")
               (format nil "unset logscale y~%"))
            (if (get-option '$logz)
               (format nil "set logscale z~%")
               (format nil "unset logscale z~%"))
            (if (get-option '$logcb)
               (format nil "set logscale cb~%")
               (format nil "unset logscale cb~%") )
            (if (get-option '$grid)
                (format nil "set grid~%")
                (format nil "unset grid~%"))
            (if (get-option '$xaxis)
               (format nil "set xzeroaxis lw ~a lt ~a lc ~a~%"
                           (get-option '$xaxis_width)
                           (get-option '$xaxis_type)
                           (get-option '$xaxis_color) )
               (format nil "unset xzeroaxis~%"))
            (if (get-option '$yaxis)
               (format nil "set yzeroaxis lw ~a lt ~a lc ~a~%"
                           (get-option '$yaxis_width)
                           (get-option '$yaxis_type)
                           (get-option '$yaxis_color) )
               (format nil "unset yzeroaxis~%"))
            (if (get-option '$zaxis)
               (format nil "set zzeroaxis lw ~a lt ~a lc ~a~%"
                           (get-option '$zaxis_width)
                           (get-option '$zaxis_type)
                           (get-option '$zaxis_color) )
               (format nil "unset zzeroaxis~%"))
            (if (null (get-option '$xtics))
               (format nil "unset xtics~%")
               (format nil "set xtics ~a ~a ~a~%" 
                       (if (get-option '$xtics_rotate) "rotate" "norotate")
                       (if (get-option '$xtics_axis) "axis" "border")
                       (get-option '$xtics)))
            (if (null (get-option '$ytics))
               (format nil "unset ytics~%")
               (format nil "set ytics ~a ~a ~a~%"
                       (if (get-option '$ytics_rotate) "rotate" "norotate")
                       (if (get-option '$ytics_axis) "axis" "border")
                       (get-option '$ytics)))
            (if (null (get-option '$ztics))
               (format nil "unset ztics~%")
               (format nil "set ztics ~a ~a ~a~%"
                       (if (get-option '$ztics_rotate) "rotate" "norotate")
                       (if (get-option '$ztics_axis) "axis" "border")
                       (get-option '$ztics)))
            (if (null (get-option '$cbtics))
               (format nil "unset cbtics~%")
               (format nil "set cbtics ~a~%"
                       (get-option '$cbtics)) )
            (if (eql (get-option '$contour) '$map)  ; if contour = map
               (format nil "set view map~%~a~%"
                            (if (equal (get-option '$proportional_axes) '$none)
                               "set size noratio"
                               "set size ratio -1") )
               (format nil "set view ~a, ~a, 1, 1~%~a~%"
                            (get-option '$rot_vertical)
                            (get-option '$rot_horizontal)
                            (case (get-option '$proportional_axes)
                               ($xy       "set view equal xy" )
                               ($xyz      "set view equal xyz")
                               (otherwise ""))))
            (if (not (get-option '$axis_3d))
                (format nil "set border 0~%"))
            (format nil "set pm3d at s depthorder explicit~%")
            (if (get-option '$surface_hide)
               (format nil "set hidden3d nooffset~%"))
            (if (get-option '$xyplane)
               (format nil "set xyplane at ~a~%" (get-option '$xyplane)))
            (if (get-option '$colorbox)
               (format nil "set colorbox~%")
               (format nil "unset colorbox~%"))
            (format nil "set cblabel '~a'~%" 
                        (if (stringp (get-option '$colorbox))
                          (get-option '$colorbox)
                          ""))
            (let ((pal (get-option '$palette)))
              (case pal
                 ($gray     (format nil "set palette gray~%"))
                 ($color    (format nil "set palette rgbformulae 7,5,15~%"))
                 (otherwise (format nil "set palette rgbformulae ~a,~a,~a~%"
                                        (car pal) (cadr pal) (caddr pal))) ) )
            (if (not (string= (get-option '$user_preamble) ""))
               (format nil "~a~%" (get-option '$user_preamble)))  ))

      ; scene description: (dimensions, gnuplot preamble in string format, list of objects)
      (list
         3       ; it's a 3d scene
         plotcmd ; gnuplot preamble
         objects ; list of objects to be plotted
         ) ))







(defmacro write-subarray (arr str)
  `(format ,str
           "~a~%"
           (apply 
             #'concatenate 'string 
             (map 
                'list 
                #'(lambda (z) (format nil "~a " z))
                ,arr))))



;; This is the function to be called at Maxima level.
;; Some examples:
;;   draw(gr2d(opt & obj))$ => a 2d plot, equivalent to draw2d(opt & obj)
;;   draw(gr3d(opt & obj))$ => a 2d plot, equivalent to draw3d(opt & obj)
;;   draw(gr2d(opt & obj),gr2d(opt & obj),gr3d(opt & obj),gr2d(opt & obj))$
;;                            => four plots in one column, one of them is a 3d plot
;;   draw(terminal=png,columns=2,gr2d(opt & obj),gr3d(opt & obj))$
;;                            => png file with two plots (2d and 3d) side by side
;; See bellow for $draw2d and $draw3d
(defun $draw (&rest args)
  (ini-global-options)
  (user-defaults)
  (let ((counter 0)
        (scenes-list '((mlist simp)))  ; these two variables will be used
        gfn ; gnuplot_file_name
        dfn ; data_file_name
        scene-short-description        ; to build the text output
        scenes
        cmdstorage  ; file maxout.gnuplot
        datastorage ; file data.gnuplot
        datapath    ; path to data.gnuplot
        ncols nrows width height ; multiplot parameters
        isanimatedgif is1stobj biglist grouplist largs)

    (make-list-of-arguments)
    (dolist (x largs)
      (cond ((equal ($op x) "=")
              (case ($lhs x)
                ($terminal          (update-gr-option '$terminal ($rhs x)))
                ($columns           (update-gr-option '$columns ($rhs x)))
                ($pic_width         (update-gr-option '$pic_width ($rhs x)))
                ($pic_height        (update-gr-option '$pic_height ($rhs x)))
                ($eps_width         (update-gr-option '$eps_width ($rhs x)))
                ($eps_height        (update-gr-option '$eps_height ($rhs x)))
                ($pdf_width         (update-gr-option '$pdf_width ($rhs x)))
                ($pdf_height        (update-gr-option '$pdf_height ($rhs x)))
                ($file_name         (update-gr-option '$file_name ($rhs x)))
                ($gnuplot_file_name (update-gr-option '$gnuplot_file_name ($rhs x)))
                ($data_file_name    (update-gr-option '$data_file_name ($rhs x)))
                ($file_bgcolor      (update-gr-option '$file_bgcolor ($rhs x)))
                ($delay             (update-gr-option '$delay ($rhs x)))
                (otherwise (merror "draw: unknown global option ~M " ($lhs x)))))
            ((equal (caar x) '$gr3d)
              (setf scenes (append scenes (list (funcall #'make-scene-3d (rest x))))))
            ((equal (caar x) '$gr2d)
              (setf scenes (append scenes (list (funcall #'make-scene-2d (rest x))))))
            (t
              (merror "draw: item ~M is not recognized" x)))   )

    (setf isanimatedgif
          (equal (get-option '$terminal) '$animated_gif))

    (setf
       gfn (plot-temp-file (get-option '$gnuplot_file_name))
       dfn (plot-temp-file (get-option '$data_file_name)))

    ; we now create two files: maxout.gnuplot and data.gnuplot
    (setf cmdstorage
          (open gfn
                :direction :output :if-exists :supersede))
    (setf datastorage
          (open dfn
                :direction :output :if-exists :supersede))
    (setf datapath (format nil "'~a'" dfn))

    ; when one multiplot window is active, change of terminal is not allowed
    (if (not *multiplot-is-active*)
    (case (gethash '$terminal *gr-options*)
        ($png (format cmdstorage "set terminal png truecolor ~a size ~a, ~a ~a~%set out '~a.png'"
                           (write-font-type)
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_bgcolor)
                           (get-option '$file_name) ) )
        ($eps (format cmdstorage "set terminal postscript eps enhanced ~a size ~acm, ~acm~%set out '~a.eps'"
                           (write-font-type)
                           (get-option '$eps_width)
                           (get-option '$eps_height)
                           (get-option '$file_name)))
        ($eps_color (format cmdstorage "set terminal postscript eps enhanced ~a color size ~acm, ~acm~%set out '~a.eps'"
                           (write-font-type)
                           (get-option '$eps_width)
                           (get-option '$eps_height)
                           (get-option '$file_name)))
        ($pdf (format cmdstorage "set terminal pdf enhanced ~a color size ~acm, ~acm~%set out '~a.pdf'"
                           (write-font-type)
                           (get-option '$pdf_width)
                           (get-option '$pdf_height)
                           (get-option '$file_name)))
        ($pdfcairo (format cmdstorage "set terminal pdfcairo enhanced ~a color size ~acm, ~acm~%set out '~a.pdf'"
                           (write-font-type)
                           (get-option '$pdf_width)
                           (get-option '$pdf_height)
                           (get-option '$file_name)))
        ($jpg (format cmdstorage "set terminal jpeg ~a size ~a, ~a ~a~%set out '~a.jpg'"
                           (write-font-type)
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_bgcolor)
                           (get-option '$file_name)))
        ($gif (format cmdstorage "set terminal gif ~a size ~a, ~a ~a~%set out '~a.gif'"
                           (write-font-type)
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_bgcolor)
                           (get-option '$file_name)))
        ($svg (format cmdstorage "set terminal svg enhanced ~a size ~a, ~a~%set out '~a.svg'"
                           (write-font-type)
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_name)))
        ($animated_gif (format cmdstorage "set terminal gif animate ~a size ~a, ~a delay ~a ~a~%set out '~a.gif'"
                           (write-font-type)
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$delay)
                           (get-option '$file_bgcolor)
                           (get-option '$file_name)))
        ($aquaterm (format cmdstorage "set terminal aqua ~a ~a~%"
                           *draw-terminal-number*
                           (write-font-type)))
        ($wxt (format cmdstorage "set terminal wxt ~a ~a~%"
                           *draw-terminal-number*
                           (write-font-type)))
        (otherwise ; default screen output
          (cond
            (*windows-OS*  ; running on windows operating system
              (format cmdstorage "set terminal windows ~a~%"
                          (write-font-type)))
            (t  ; other platforms
              (format cmdstorage "set terminal x11 ~a ~a~%"
                           *draw-terminal-number*
                           (write-font-type))))) ))

    ; compute some parameters for multiplot
    (when (not isanimatedgif)
      (setf ncols (get-option '$columns))
      (setf nrows (ceiling (/ (length scenes) ncols)))
      (setf width (/ 1.0 ncols))
      (setf height (/ 1.0 nrows))
      (if (> (length scenes) 1)
        (format cmdstorage "~%set size 1.0, 1.0~%set origin 0.0, 0.0~%set multiplot~%")) )

    ; write descriptions of 2d and 3d scenes
    (let ((i -1))
      (dolist (scn scenes)
        ; write size and origin if necessary
        (cond (isanimatedgif
                (format cmdstorage "~%set size 1.0, 1.0~%") )
              (t ; it's not an animated gif
                (format cmdstorage "~%set size ~a, ~a~%"
                                   width
                                   height)
                (format cmdstorage "set origin ~a, ~a~%" 
                                   (* width (mod counter ncols))
                                   (* height (- nrows 1.0 (floor (/ counter ncols)))))))
        (setf is1stobj t
              biglist '()
              grouplist '())
        (format cmdstorage "~a" (second scn))
        (cond ((= (first scn) 2)    ; it's a 2d scene
                 (setf scene-short-description '(($gr2d simp)))
                 (format cmdstorage "plot "))
              ((= (first scn) 3)    ; it's a 3d scene
                 (setf scene-short-description '(($gr3d simp)))
                 (format cmdstorage "splot ")))
        (dolist (obj (third scn))
           (setf scene-short-description
                 (cons (gr-object-name obj) scene-short-description))
           (if is1stobj
             (setf is1stobj nil)
             (format cmdstorage ", \\~%")  )
           (let ((pcom (gr-object-command obj)))
             (cond
               ((listp pcom)
                  (while (consp pcom)
                    (format cmdstorage "~a~a~a~a"
                                       datapath
                                       (format nil " index ~a" (incf i))
                                       (pop pcom)
                                       (if (null pcom)
                                           ""
                                           "," )) ) )
               (t (format cmdstorage "~a~a~a"
                                     datapath
                                     (format nil " index ~a" (incf i))
                                     pcom) )))
           (setf grouplist (append grouplist (gr-object-groups obj)))
           (setf biglist (append biglist (gr-object-points obj))) )

        ; let's write data in data.gnuplot
        (do ( (blis biglist (cdr blis))
              (glis grouplist (cdr glis) ))
            ((null blis) 'done)
          (let* ((vect (car blis))
                 (k (length vect))
                 (ncol (caar glis))
                 (l 0)
                 (m (cadar glis)))
             (cond
                ((= m 0)     ; no blank lines
                   (do ((cont 0 (+ cont ncol)))
                       ((= cont k) 'done)
                     (write-subarray (subseq vect cont (+ cont ncol)) datastorage))  )
                (t           ; blank lines every m lines
                   (do ((cont 0 (+ cont ncol)))
                       ((= cont k) 'done)
                     (when (eql l m)
                           (format datastorage "~%")
                           (setf l 0) )
                     (write-subarray (subseq vect cont (+ cont ncol)) datastorage)
                     (incf l)))))
          (format datastorage "~%~%") )
        (incf counter)
        (setf scenes-list (cons (reverse scene-short-description) scenes-list)) ))  ; end let-dolist scenes
    (close datastorage)

    (cond (isanimatedgif  ; this is an animated gif
             (format cmdstorage "~%quit~%~%")
             (close cmdstorage)
             ($system (format nil "~a \"~a\"" 
                                  $gnuplot_command
                                  gfn) ))
          (t ; non animated gif
             ; command file maxout.gnuplot is now ready
             (format cmdstorage "~%")
             (cond ((> (length scenes) 1)
                      (format cmdstorage "unset multiplot~%"))
                   ; if we want to save the coordinates in a file,
                   ; print them when hitting the x key after clicking the mouse button
                   ((not (string= (gethash '$xy_file *gr-options*) ""))
                      (format cmdstorage
                              "set print \"~a\" append~%bind x \"print MOUSE_X,MOUSE_Y\"~%"
                              (gethash '$xy_file *gr-options*))) )

             ; in svg format, unset output to force
             ; Gnuplot to write </svg> at the end of the file
             (when (equal (get-option '$terminal) '$svg)
                (format cmdstorage "unset output~%"))
             (close cmdstorage)

             ; get the plot
             (cond
                (*windows-OS*
                   ($system (if (equal (gethash '$terminal *gr-options*) '$screen)
                                   (format nil "~a ~a"
                                               $gnuplot_command
                                               (format nil $gnuplot_view_args gfn))
                                   (format nil "~a \"~a\"" 
                                               $gnuplot_command
                                               gfn))) )
                (t  ; non windows operating system
                   (check-gnuplot-process)
                   (when (not *multiplot-is-active*) ; not in a one window multiplot
                     (send-gnuplot-command "unset output"))
                   (send-gnuplot-command "reset")
                   (send-gnuplot-command
                        (format nil "load '~a'" gfn))  ))))

    ; the output is a simplified description of the scene(s)
    (reverse scenes-list)) )



;; Equivalent to draw2d(opt & obj)
(defun $draw2d (&rest args)
   ($draw (cons '($gr2d) args)) )

;; Equivalent to draw3d(opt & obj)
(defun $draw3d (&rest args)
   ($draw (cons '($gr3d) args)) )

;; This function transforms an integer number into
;; a string, adding zeros at the left side until the
;; length of the string equals 10. This function is
;; useful to name a sequence of frames.
(defun $add_zeroes (num)
   (format nil "~10,'0d" ($sconcat num)) )


;; copies current plot in window into a file
(defun $draw_file (&rest opts)
 (let (str)
   (dolist (x opts)
      (if (equal ($op x) "=")
         (update-gr-option ($lhs x) ($rhs x))
         (merror "draw: item ~M is not recognized as an option assignment" x)))
   (case (gethash '$terminal *gr-options*)
      ($png (setf str (format nil "set terminal png ~a size ~a, ~a ~a~%set out '~a.png'"
                           (write-font-type)
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_bgcolor)
                           (get-option '$file_name) ) ))
      ($eps (setf str (format nil "set terminal postscript eps enhanced ~a size ~acm, ~acm~%set out '~a.eps'"
                           (write-font-type) ; other alternatives are Arial, Courier
                           (get-option '$eps_width)
                           (get-option '$eps_height)
                           (get-option '$file_name))))
      ($eps_color (setf str (format nil "set terminal postscript eps enhanced ~a color size ~acm, ~acm~%set out '~a.eps'"
                           (write-font-type)
                           (get-option '$eps_width)
                           (get-option '$eps_height)
                           (get-option '$file_name))))
      ($pdf (setf str (format nil "set terminal pdf enhanced ~a color size ~acm, ~acm~%set out '~a.pdf'"
                           (write-font-type)
                           (get-option '$pdf_width)
                           (get-option '$pdf_height)
                           (get-option '$file_name))))
      ($pdfcairo (setf str (format nil "set terminal pdfcairo enhanced ~a color size ~acm, ~acm~%set out '~a.pdf'"
                           (write-font-type)
                           (get-option '$pdf_width)
                           (get-option '$pdf_height)
                           (get-option '$file_name))))
      ($jpg (setf str (format nil "set terminal jpeg ~a size ~a, ~a ~a~%set out '~a.jpg'"
                           (write-font-type)
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_bgcolor)
                           (get-option '$file_name))))
      ($gif (setf str (format nil "set terminal gif ~a size ~a, ~a ~a~%set out '~a.gif'"
                           (write-font-type)
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_bgcolor)
                           (get-option '$file_name))))
      ($svg (format cmdstorage "set terminal svg enhanced ~a size ~a, ~a~%set out '~a.svg'"
                           (write-font-type)
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_name)))
      (otherwise (merror "draw: unknown file format" )))
   (send-gnuplot-command (format nil "~a~%replot" str)) ))


;; When working with multiple windows, for example
;; terminal = [wxt, 5], only the newest window is active.
;; This function activates any other previous window at will.
(defun $activate_window (term num)
   (when (or (not (member term '($screen $wxt $aquaterm)))
             (not (integerp num))
             (< num 0) )
      (merror "draw: Incorrect terminal or window number"))
   (when *windows-OS*
      (merror "draw: Multiple windows are not allowed in Windows systems"))
   (let (str)
      (case term
         ($wxt      (setf str "wxt"))
         ($aquaterm (setf str "aquaterm"))
         (otherwise (setf str "x11")))
      (send-gnuplot-command (format nil "set terminal ~a ~a~%" str num))   ))

