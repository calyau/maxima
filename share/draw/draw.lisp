;;;                 COPYRIGHT NOTICE
;;;  
;;;  Copyright (C) 2007 Mario Rodriguez Riotorto
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

;;; This is a maxima-gnuplot interface.

;;; Visit
;;; http://www.telefonica.net/web2/biomates/maxima/gpdraw
;;; for examples

;;; The code for implicit functions was written by
;;; Andrej Vodopivec. Thanks!

;;; For questions, suggestions, bugs and the like, feel free
;;; to contact me at
;;; mario @@@ edu DOT xunta DOT es
;;; www.biomates.net







(defvar $draw_pipes (not (string= *autoconf-win32* "true")))


(defvar $draw_command (if (string= *autoconf-win32* "true")
                              "wgnuplot"
                              "gnuplot"))



;; This variable stores actual global options
(defvar *gr-options* (make-hash-table))


;; Sets default values of graphics options
(defun ini-gr-options ()
  (setf ; graphics options
      ; header options
      (gethash '$xrange *gr-options*) nil      ; nil => automatic computation
      (gethash '$yrange *gr-options*) nil      ; nil => automatic computation
      (gethash '$zrange *gr-options*) nil      ; nil => automatic computation
      (gethash '$logx *gr-options*)   nil
      (gethash '$logy *gr-options*)   nil
      (gethash '$logz *gr-options*)   nil
      (gethash '$grid *gr-options*)   nil
      (gethash '$title *gr-options*)  ""
      (gethash '$xlabel *gr-options*) ""
      (gethash '$ylabel *gr-options*) ""
      (gethash '$zlabel *gr-options*) ""
      (gethash '$xtics *gr-options*)  t
      (gethash '$ytics *gr-options*)  t
      (gethash '$ztics *gr-options*)  t
      (gethash '$rot_vertical *gr-options*)   60   ; range: [0,180] (vertical rotation)
      (gethash '$rot_horizontal *gr-options*) 30   ; range: [0,360] (horizontal rotation)
      (gethash '$xy_file *gr-options*)        ""
      (gethash '$user_preamble *gr-options*)  ""

      ; colors are specified by name
      (gethash '$color *gr-options*)      "black"   ; for lines, points, borders and labels
      (gethash '$fill_color *gr-options*) "red"     ; for filled regions

      ; implicit plot options
      (gethash '$ip_grid *gr-options*) '((mlist simp) 50 50)
      (gethash '$ip_grid_in *gr-options*) '((mlist simp) 5 5)

      ; 2d-axis
      (gethash '$axis_bottom *gr-options*) t
      (gethash '$axis_left *gr-options*)   t
      (gethash '$axis_top *gr-options*)    t
      (gethash '$axis_right *gr-options*)  t
      (gethash '$axis_3d *gr-options*)     t

      ; point options
      (gethash '$point_size *gr-options*)    1
      (gethash '$point_type *gr-options*)    1
      (gethash '$points_joined *gr-options*) nil

      ; polygon  options
      (gethash '$transparent *gr-options*) nil
      (gethash '$border *gr-options*)      t

      ; vector  options
      (gethash '$head_both *gr-options*)   nil
      (gethash '$head_length *gr-options*) 2        ; in x-axis units
      (gethash '$head_angle *gr-options*)  45       ; with respect to the segment
      (gethash '$head_type *gr-options*)   '$filled ; other options are: $empty and $nofilled

      ; label options
      (gethash '$label_alignment *gr-options*)   '$center ; other options are: $left and $right
      (gethash '$label_orientation *gr-options*) '$horizontal ; the other option is $vertical

      ; line options
      (gethash '$line_width *gr-options*) 1
      (gethash '$line_type *gr-options*)  1    ; two options: 1 (solid) and 0 (dots)

      ; function options
      (gethash '$nticks *gr-options*)      30
      (gethash '$adapt_depth *gr-options*) 10
      (gethash '$key *gr-options*)         ""    ; by default, no keys
      (gethash '$filled_func *gr-options*) nil   ; true or false

      ; 3d options
      (gethash '$xu_grid *gr-options*)        30
      (gethash '$yv_grid *gr-options*)        30
      (gethash '$surface_hide *gr-options*)   nil
      (gethash '$enhanced3d *gr-options*)     nil
      (gethash '$contour *gr-options*)        '$none  ; other options are: $base, $surface, $both and $map
      (gethash '$contour_levels *gr-options*) 5       ; maximum: 50
      (gethash '$colorbox *gr-options*)       t       ; in pm3d mode, always show colorbox
      (gethash '$palette  *gr-options*)       '$color ; '$color is a short cut for [7,5,15]
                                                      ; and '$gray is a short cut for [3,3,3].
                                                      ; See command 'show palette rgbformulae' in gnuplot.
  ) )


;; Sets default values to global options
(defun ini-global-options ()
  (setf ; global options
      (gethash '$columns *gr-options*)    1
      (gethash '$terminal *gr-options*)   '$screen
      (gethash '$pic_width *gr-options*)  640    ; points for bitmap pictures
      (gethash '$pic_height *gr-options*) 480    ; points for bitmap pictures
      (gethash '$eps_width *gr-options*)  12     ; cm for eps pictures
      (gethash '$eps_height *gr-options*) 8      ; cm for eps pictures
      (gethash '$file_name *gr-options*)  "maxima_out"         ) )


;; Initialize defaults
(ini-gr-options)
(ini-global-options)


;; Gives value of option(s)
(defun get-option (opt) (gethash opt *gr-options*))


(defun convert-to-float (expr)
   (meval `($float ,expr)))


;; Sets new values to global options
(defun update-gr-option (opt val)
   (case opt
      ($rot_vertical ; in range [0, 180]
            (if (and (numberp val)
                     (>= val 0 )
                     (<= val 180 ))
                (setf (gethash opt *gr-options*) val)
                (merror "~M must be angle in [0, 180]" val)))
      ($rot_horizontal ; in range [0, 360]
            (if (and (numberp val)
                     (>= val 0 )
                     (<= val 360 ))
                (setf (gethash opt *gr-options*) val)
                (merror "~M must be angle in [0, 360]" val)))
      (($line_width $head_length $head_angle $eps_width $eps_height) ; defined as positive numbers
            (if (and (numberp val)
                     (> val 0 ))
                (setf (gethash opt *gr-options*) val)
                (merror "Non positive number: ~M " val)))
      ($point_size ; defined as non negative numbers
            (if (and (numberp val)
                     (>= val 0 ))
                (setf (gethash opt *gr-options*) val)
                (merror "Negative number: ~M " val)))
      ($line_type ; defined as $solid and $dots
            (case val
               ($solid (setf (gethash opt *gr-options*) 1))
               ($dots  (setf (gethash opt *gr-options*) 0))
               (otherwise  (merror "Illegal line type: ~M" val))) )
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
                        (merror "Illegal point type: ~M " val))))) )
      (($columns $nticks $adapt_depth $pic_width $pic_height     ; defined as positive integers
        $xu_grid $yv_grid $contour_levels)
            (if (and (integerp val)
                     (> val 0 ))
                (setf (gethash opt *gr-options*) val)
                (merror "Non positive integer: ~M " val)))
      (($points_joined $transparent $border $logx $logy $logz $head_both $grid $xtics $ytics $ztics
        $axis_bottom $axis_left $axis_top $axis_right $axis_3d $surface_hide $colorbox $filled_func
        $enhanced3d ) ; true or false
            (if (or (equal val t)
                    (equal val nil))
                (setf (gethash opt *gr-options*) val)
                (merror "Non boolean value: ~M " val)))
      ($terminal ; defined as screen, png, jpg, eps or eps_color
            (if (member val '($screen $png $jpg $eps $eps_color))
                (setf (gethash opt *gr-options*) val)
                (merror "This is not a terminal: ~M" val)))
      ($head_type ; defined as $filled, $empty and $nofilled
            (if (member val '($filled $empty $nofilled))
                (setf (gethash opt *gr-options*) val)
                (merror "Illegal head type for vectors: ~M" val)))
      ($contour ; defined as $none, $base, $surface, $both and $map
            (if (member val '($base $surface $both $map))
                (setf (gethash opt *gr-options*) val)
                (merror "Illegal contour allocation: ~M" val)))
      ($label_alignment ; defined as $center, $left and $right
            (if (member val '($center $left $right))
                (setf (gethash opt *gr-options*) val)
                (merror "Illegal label alignment: ~M" val)))
      ($label_orientation ; defined as $horizontal and $vertical
            (if (member val '($horizontal $vertical))
                (setf (gethash opt *gr-options*) val)
                (merror "Illegal label orientation: ~M" val)))
      (($key $title $xlabel $ylabel $zlabel $file_name $xy_file)  ; defined as strings
            (setf (gethash opt *gr-options*) (string-trim "\"" (coerce (mstring val) 'string))))
      ($user_preamble ; defined as a string or a Maxima list of strings
            (let ((str ""))
              (cond
                (($atom val)
                  (setf str (string-trim "\"" (coerce (mstring val) 'string))))
                (($listp val)
                  (dolist (st (rest val))
                    (if (not ($atom st))
                        (merror "User preamble ~M should be a string" st))
                    (setf str (concatenate 'string
                                            str
                                            (format nil (if (string= str "") "~a" "~%~a")
                                                    (string-trim "\"" (coerce (mstring st) 'string)) )))))
                (t (merror "Illegal user preamble especification")))
              (setf (gethash opt *gr-options*) str))  )
      (($xrange $yrange $zrange) ; defined as a Maxima list with two numbers in increasing order
            (if (or (not ($listp val))
                    (not (member ($length val) '(2 3))))
                (merror "Illegal range: ~M " val))
            (let ((fval1 (convert-to-float (cadr val)))
                  (fval2 (convert-to-float (caddr val))))
               (cond
                  ((or (not (floatp fval1))
                       (not (floatp fval2))
                       (< fval2 fval1))
                     (merror "Illegal values in range specification"))
                  ((= ($length val) 2)  ; it's a trick: length 2 => user change
                     (setf (gethash opt *gr-options*) (list fval1 fval2)))
                  (t  ; should be length 3 or nil option => automatic computation of ranks
                     (setf (gethash opt *gr-options*) (list fval1 fval2 0))))  ) )
      (($ip_grid $ip_grid_in)
       (if (not ($listp val))
	   (merror "Illegal value for grid")
	   (if (not (and (integerp ($first val))
			 (integerp ($second val))))
	       (merror "Illegal value for grid")
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
                    (merror "Illegal palette description: ~M" val)))  )
      (($color $fill_color)  ; defined as a color name or hexadecimal #rrggbb
         (let ((str (string-downcase (string-trim "\"" (coerce (mstring val) 'string)))))
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
                 (setf (gethash opt *gr-options*) str))
              ((and (= (length str) 7)
                    (char= (schar str 0) #\#)
                    (every #'(lambda (z) (position z "0123456789abcdef"))
                           (subseq str 1)))
                 (setf (gethash opt *gr-options*) str))
              (t
                 (merror "Illegal color specification: ~M" str)))))

      (otherwise (merror "Unknown option ~M " opt))  ) )








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
(defun update-ranges (xmin xmax ymin ymax &optional zmin zmax)
   ; update x-ranges if necessary
   (case (length (gethash '$xrange *gr-options*))
      (0 (setf (gethash '$xrange *gr-options*) (list xmin xmax 0)))
      (3 (setf (gethash '$xrange *gr-options*) (list (min xmin (first  (gethash '$xrange *gr-options*)))
                                                    (max xmax (second (gethash '$xrange *gr-options*)))
                                                    0))) )
   ; update y-ranges if necessary
   (case (length (gethash '$yrange *gr-options*))
      (0 (setf (gethash '$yrange *gr-options*) (list ymin ymax 0)))
      (3 (setf (gethash '$yrange *gr-options*) (list (min ymin (first  (gethash '$yrange *gr-options*)))
                                                    (max ymax (second (gethash '$yrange *gr-options*)))
                                                    0))) )

   ; update z-ranges if necessary
   (if (and zmin zmax)
       (case (length (gethash '$zrange *gr-options*))
          (0 (setf (gethash '$zrange *gr-options*) (list zmin zmax 0)))
          (3 (setf (gethash '$zrange *gr-options*) (list (min zmin (first  (gethash '$zrange *gr-options*)))
                                                        (max zmax (second (gethash '$zrange *gr-options*)))
                                                        0))) )))





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
;; Options:
;;     point_size
;;     point_type
;;     points_joined
;;     line_width
;;     key
;;     line_type
;;     color
(defun points (arg1 &optional (arg2 nil))
   (let (x y xmin xmax ymin ymax pts)
      (cond ((and ($listp arg1)
                  (every #'$listp (rest arg1)))     ; xy format
               (let ((tmp (mapcar #'rest (rest arg1))))
                  (setf x (map 'list #'convert-to-float (map 'list #'first tmp))
                        y (map 'list #'convert-to-float (map 'list #'second tmp)) ) ) )
            ((and ($listp arg1)
                  ($listp arg2)
                  (= (length arg1) (length arg2)))  ; xx yy format
               (setf x (map 'list #'convert-to-float (rest arg1))
                     y (map 'list #'convert-to-float (rest arg2))))
            (t (merror "draw (points2d): bad input format"))  )
      (setf pts (make-array (* 2 (length x)) :element-type 'double-float
                                             :initial-contents (mapcan #'list x y)))
      (setf xmin ($tree_reduce 'min (cons '(mlist simp) x))
            xmax ($tree_reduce 'max (cons '(mlist simp) x))
            ymin ($tree_reduce 'min (cons '(mlist simp) y))
            ymax ($tree_reduce 'max (cons '(mlist simp) y)) )
      ;; update x-y ranges if necessary
      (update-ranges xmin xmax ymin ymax)
      (make-gr-object
         :name 'points
         :command (if (get-option '$points_joined)
                     (format nil " ~a w lp ps ~a pt ~a lw ~a lt ~a lc rgb '~a'"
                                 (make-obj-title (get-option '$key))
                                 (get-option '$point_size)
                                 (get-option '$point_type)
                                 (get-option '$line_width)
                                 (get-option '$line_type)
                                 (get-option '$color))
                     (format nil " ~a w p ps ~a pt ~a lc rgb '~a'"
                                 (make-obj-title (get-option '$key))
                                 (get-option '$point_size)
                                 (get-option '$point_type)
                                 (get-option '$color)) )
         :groups '((2)) ; numbers are sent to gnuplot in groups of 2
         :points (list pts) ) ))






;; Object: 'points3d'
;; Usage:
;;     points([[x1,y1,z1], [x2,y2,z2], [x3,y3,z3],...])
;;     points([x1,x2,x3,...], [y1,y2,y3,...], [z1,z2,z3,...])
;; Options:
;;     point_size
;;     point_type
;;     points_joined
;;     line_width
;;     key
;;     line_type
;;     color
(defun points3d (arg1 &optional (arg2 nil) (arg3 nil))
   (let (pts x y z xmin xmax ymin ymax zmin zmax)
      (cond ((and ($listp arg1)
                  (every #'$listp (rest arg1)))     ; xyz format
               (let ((tmp (mapcar #'rest (rest arg1))))
                  (setf x (map 'list #'convert-to-float (map 'list #'first tmp))
                        y (map 'list #'convert-to-float (map 'list #'second tmp))
                        z (map 'list #'convert-to-float (map 'list #'third tmp)) ) ) )
            ((and ($listp arg1)
                  ($listp arg2)
                  ($listp arg3)
                  (= (length arg1) (length arg2) (length arg3)))  ; xx yy zz format
               (setf x (map 'list #'convert-to-float (rest arg1))
                     y (map 'list #'convert-to-float (rest arg2))
                     z (map 'list #'convert-to-float (rest arg3))  ))
            (t (merror "draw (points3d): bad input format"))  )
      (setf pts (make-array (* 3 (length x)) :element-type 'double-float
                                             :initial-contents (mapcan #'list x y z)))
      (setf xmin ($tree_reduce 'min (cons '(mlist simp) x))
            xmax ($tree_reduce 'max (cons '(mlist simp) x))
            ymin ($tree_reduce 'min (cons '(mlist simp) y))
            ymax ($tree_reduce 'max (cons '(mlist simp) y))
            zmin ($tree_reduce 'min (cons '(mlist simp) z))
            zmax ($tree_reduce 'max (cons '(mlist simp) z)) )
      ;; update x-y-y ranges if necessary
      (update-ranges xmin xmax ymin ymax zmin zmax)
      (make-gr-object
         :name 'points
         :command (if (get-option '$points_joined)
                     (format nil " ~a w lp ps ~a pt ~a lw ~a lt ~a lc rgb '~a'"
                                 (make-obj-title (get-option '$key))
                                 (get-option '$point_size)
                                 (get-option '$point_type)
                                 (get-option '$line_width)
                                 (get-option '$line_type)
                                 (get-option '$color))
                     (format nil " ~a w p ps ~a pt ~a lc rgb '~a'"
                                 (make-obj-title (get-option '$key))
                                 (get-option '$point_size)
                                 (get-option '$point_type)
                                 (get-option '$color)) )
         :groups '((3 0)) ; numbers are sent to gnuplot in groups of 3, without blank lines
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
(defun polygon (arg1 &optional (arg2 nil))
   (if (and (gethash '$transparent  *gr-options*)
            (not (gethash '$border  *gr-options*)))
       (merror "draw (polygon): transparent is true and border is false; this is not consistent"))
   (let (pltcmd pts grps x y xmin xmax ymin ymax)
      (cond ((and ($listp arg1)
                  (every #'$listp (rest arg1))
                  (null arg2) )                    ; xy format
               (let ((tmp (mapcar #'rest (rest arg1))))
                  (setf x (map 'list #'(lambda (z) (convert-to-float (first z))) tmp)
                        y (map 'list #'(lambda (z) (convert-to-float (second z))) tmp) ) )  )
            ((and ($listp arg1)
                  ($listp arg2)
                  (= (length arg1) (length arg2)))  ; xx yy format
               (setf x (map 'list #'convert-to-float (rest arg1))
                     y (map 'list #'convert-to-float (rest arg2))) )
            (t (merror "draw (polygon): bad input format"))  )
      (setf xmin ($tree_reduce 'min (cons '(mlist simp) x))
            xmax ($tree_reduce 'max (cons '(mlist simp) x))
            ymin ($tree_reduce 'min (cons '(mlist simp) y))
            ymax ($tree_reduce 'max (cons '(mlist simp) y)) )
      ;; update x-y ranges if necessary
      (update-ranges xmin xmax ymin ymax)
      (cond
         ((get-option '$transparent)  ; if transparent, draw only the border
             (setf pltcmd (format nil " ~a  w l lw ~a lt ~a lc rgb '~a'"
                                      (make-obj-title (get-option '$key))
                                      (get-option '$line_width)
                                      (get-option '$line_type)
                                      (get-option '$color)))
             (setf grps '((2)))  ; numbers are sent to gnuplot in groups of 2
             (setf pts (list (make-array (+ (* 2 (length x)) 2)
                                         :element-type 'double-float
                                         :initial-contents (append (mapcan #'list x y)
                                                                   (list (first x) (first y))) )) ) )
         ((not (get-option '$border)) ; no transparent, no border
             (setf pltcmd (format nil " ~a w filledcurves lc rgb '~a'"
                                      (make-obj-title (get-option '$key))
                                      (get-option '$fill_color)))
             (setf grps '((2)))  ; numbers are sent to gnuplot in groups of 2
             (setf pts (list (make-array (* 2 (length x))
                                         :element-type 'double-float
                                         :initial-contents (mapcan #'list x y)) ) ))
         (t ; no transparent with border
             (setf pltcmd (list (format nil " ~a w filledcurves lc rgb '~a'"
                                        (make-obj-title (get-option '$key))
                                        (get-option '$fill_color))
                                (format nil " t '' w l lw ~a lt ~a lc rgb '~a'"
                                        (get-option '$line_width)
                                        (get-option '$line_type)
                                        (get-option '$color))))

             (setf grps '((2) (2)))  ; both sets of vertices (interior and border)
                                     ; are sent to gnuplot in groups of 2
             (setf pts (list (make-array (* 2 (length x))
                                         :element-type 'double-float
                                         :initial-contents (mapcan #'list x y))
                             (make-array (+ (* 2 (length x)) 2)
                                         :element-type 'double-float
                                         :initial-contents (append (mapcan #'list x y)
                                                                   (list (first x) (first y))))))))
      (make-gr-object
         :name   'polygon
         :command pltcmd
         :groups  grps
         :points  pts )))








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
(defun rectangle (arg1 arg2)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 2))
           (not ($listp arg2))
           (not (= ($length arg2) 2)))
       (merror "draw (rectangle): vertices are not correct"))
   (let* ((x1 (convert-to-float (cadr arg1)))
          (y1 (convert-to-float (caddr arg1)))
          (x2 (convert-to-float (cadr arg2)))
          (y2 (convert-to-float (caddr arg2)))
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
;;     ellipse(xc, yc, a, b, ang1 ang2),  both angles in degrees [-360, 360]
;; Options:
;;     nticks
;;     transparent
;;     fill_color
;;     border
;;     line_width
;;     line_type
;;     key
;;     color
(defun ellipse (xc yc a b ang1 ang2)
  (if (and (gethash '$transparent  *gr-options*)
           (not (gethash '$border  *gr-options*)))
      (merror "draw (ellipse): transparent is true and border is false; this is not consistent"))
  (let ((fxc (convert-to-float xc))
        (fyc (convert-to-float yc))
        (fa (convert-to-float a))
        (fb (convert-to-float b))
        (fang1 (convert-to-float ang1))
        (fang2 (convert-to-float ang2))
        (nticks (gethash '$nticks  *gr-options*))
        (xmin 1.75555970201398d+305)
        (xmax -1.75555970201398d+305)
        (ymin 1.75555970201398d+305)
        (ymax -1.75555970201398d+305)
        (result nil)
        pts grps tmin tmax eps x y tt pltcmd)
    (if (or (notevery #'floatp (list fxc fyc fa fb fang1 fang2))
            (< fang1 -360.0) (< fang2 -360.0)
            (> fang1  360.0) (> fang2  360.0))
       (merror "draw (ellipse): illegal argument(s)"))
    ; degrees to radians
    (setf fang1 (* 0.017453292519943295 fang1)
          fang2 (* 0.017453292519943295 fang2))
    (setf tmin (min fang1 (+ fang1 fang2))
          tmax (max fang1 (+ fang1 fang2))
          eps (/ (- tmax tmin) (- nticks 1)))
    (setf tt tmin)
    (loop
      (setf x (+ fxc (* fa (cos tt))))
      (if (> x xmax) (setf xmax x))
      (if (< x xmin) (setf xmin x))
      (setf y (+ fyc (* fb (sin tt))))
      (if (> y ymax) (setf ymax y))
      (if (< y ymin) (setf ymin y))
      (setf result (append (list x y) result))
      (if (>= tt tmax) (return))
      (setf tt (+ tt eps))
      (if (>= tt tmax) (setq tt tmax)) )
    ; update x-y ranges if necessary
    (setf xmin (min fxc xmin)
          xmax (max fxc xmax)
          ymin (min fyc ymin)
          ymax (max fyc ymax))
    (update-ranges xmin xmax ymin ymax)
    (cond
       ((get-option '$transparent)  ; if transparent, draw only the border
           (setf pltcmd (format nil " ~a  w l lw ~a lt ~a lc rgb '~a'"
                                    (make-obj-title (get-option '$key))
                                    (get-option '$line_width)
                                    (get-option '$line_type)
                                    (get-option '$color)))
           (setf grps '((2)))
           (setf pts `( ,(make-array (length result) :element-type 'double-float
                                                    :initial-contents result)))  )
       ((not (get-option '$border)) ; no transparent, no border
           (setf pltcmd (format nil " ~a w filledcurves xy=~a,~a lc rgb '~a'"
                                    (make-obj-title (get-option '$key))
                                    fxc fyc
                                    (get-option '$fill_color)))
           (setf grps '((2)))
           (setf pts `( ,(make-array (length result) :element-type 'double-float
                                                    :initial-contents result)))  )
       (t ; no transparent with border
             (setf pltcmd (list (format nil " ~a w filledcurves xy=~a,~a lc rgb '~a'"
                                            (make-obj-title (get-option '$key))
                                            fxc fyc
                                            (get-option '$fill_color))
                                (format nil " t '' w l lw ~a lt ~a lc rgb '~a'"
                                            (get-option '$line_width)
                                            (get-option '$line_type)
                                            (get-option '$color))))
           (setf grps '((2) (2)))
           (setf pts (list (make-array (length result) :element-type 'double-float
                                                       :initial-contents result)
                           (make-array (length result) :element-type 'double-float
                                                       :initial-contents result)))  ))
    (make-gr-object
       :name    'ellipse
       :command pltcmd
       :groups  grps
       :points  pts ) ))








;; Object: 'label'
;; Usage:
;;     label(string,x,y)
;; Options:
;;     label_alignment
;;     label_orientation
;;     color
(defun label (str x y)
   (let ((fx (convert-to-float x))
         (fy (convert-to-float y))
         (text (coerce (mstring str) 'string)))
      (if (or (not (floatp fx)) 
              (not (floatp fy)))
         (merror "draw (label): non real coordinates"))
      (update-ranges fx fx fy fy)
      (make-gr-object
         :name 'label
         :command (format nil "set label ~a at ~a, ~a ~a ~a tc rgb '~a'~%"
                              text fx fy
                              (case (get-option '$label_alignment)
                                 ($center "center")
                                 ($left   "left")
                                 ($right  "right"))
                              (case (get-option '$label_orientation)
                                 ($horizontal "norotate")
                                 ($vertical  "rotate"))
                              (get-option '$color))
         :groups nil
         :points nil)  ))







;; Object: 'label3d'
;; Usage:
;;     label(string,x,y,z)
;; Options:
;;     label_alignment
;;     label_orientation
;;     color
(defun label3d (str x y z)
   (let ((fx (convert-to-float x))
         (fy (convert-to-float y))
         (fz (convert-to-float z))
         (text (coerce (mstring str) 'string)))
      (if (or (not (floatp fx)) 
              (not (floatp fy))
              (not (floatp fz)))
         (merror "draw (label): non real coordinates"))
      (update-ranges fx fx fy fy fz fz)
      (make-gr-object
         :name 'label
         :command (format nil "set label ~a at ~a, ~a, ~a ~a ~a tc rgb '~a'~%"
                              text fx fy fz
                              (case (get-option '$label_alignment)
                                 ($center "center")
                                 ($left   "left")
                                 ($right  "right"))
                              (case (get-option '$label_orientation)
                                 ($horizontal "norotate")
                                 ($vertical  "rotate"))
                              (get-option '$color))
         :groups nil
         :points nil)  ))







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
(defun vect (arg1 arg2)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 2))
           (not ($listp arg2))
           (not (= ($length arg2) 2)))
       (merror "draw (vector): coordinates are not correct"))
   (let* ((x (convert-to-float (cadr arg1)))
          (y (convert-to-float (caddr arg1)))
          (dx (convert-to-float (cadr arg2)))
          (dy (convert-to-float (caddr arg2)))
          (xdx (convert-to-float (+ x dx)))
          (ydy (convert-to-float (+ y dy))))
      (update-ranges (min x xdx) (max x xdx) (min y ydy) (max y ydy))
      (make-gr-object
         :name 'vector
         :command (format nil " ~a w vect ~a size ~a, ~a ~a lw ~a lt ~a lc rgb '~a'"
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
                              (get-option '$color) )
         :groups '((4))
         :points `(,(make-array 4 :element-type 'double-float
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
(defun vect3d (arg1 arg2)
   (if (or (not ($listp arg1))
           (not (= ($length arg1) 3))
           (not ($listp arg2))
           (not (= ($length arg2) 3)))
       (merror "draw (vector): coordinates are not correct"))
   (let* ((x (convert-to-float (cadr arg1)))
          (y (convert-to-float (caddr arg1)))
          (z (convert-to-float (cadddr arg1)))
          (dx (convert-to-float (cadr arg2)))
          (dy (convert-to-float (caddr arg2)))
          (dz (convert-to-float (cadddr arg2)))
          (xdx (convert-to-float (+ x dx)))
          (ydy (convert-to-float (+ y dy)))
          (zdz (convert-to-float (+ z dz))))
      (update-ranges (min x xdx) (max x xdx) (min y ydy) (max y ydy) (min z zdz) (max z zdz))
      (make-gr-object
         :name 'vector
         :command (format nil " ~a w vect ~a ~a size ~a, ~a lw ~a lt ~a lc rgb '~a'"
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
         :groups '((6))
         :points `(,(make-array 6 :element-type 'double-float
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
;; Note: implements a clon of draw2d (plot.lisp) with some
;;       mutations to fit the draw environment.
;;       Read source in plot.lisp for more information
(defun explicit (fcn var minval maxval)
  (let* ((nticks (gethash '$nticks  *gr-options*))
         (depth (gethash '$adapt_depth  *gr-options*))
         ($numer t))
    (setq fcn (coerce-float-fun fcn `((mlist), var)))
    (let* ((xstart (convert-to-float minval))
           (xend (convert-to-float maxval))
           (x-step (/ (- xend xstart) (convert-to-float nticks) 2))
           (ymin 1.75555970201398d+305)
           (ymax -1.75555970201398d+305)
           x-samples y-samples result yy pltcmd)
      (if (< xend xstart)
         (merror "draw2d (explicit): illegal range"))
      (flet ((fun (x) (funcall fcn x)))
        (dotimes (k (1+ (* 2 nticks)))
          (let* ((x (+ xstart (* k x-step)))
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
          (setf result
                (if result
                    (append result
                            (cddr
                             (adaptive-plot #'fun (car x-start) (car x-mid) (car x-end)
                                            (car y-start) (car y-mid) (car y-end)
                                            depth 1d-5)))
                    (adaptive-plot #'fun (car x-start) (car x-mid) (car x-end)
                                   (car y-start) (car y-mid) (car y-end)
                                   depth 1d-5)))  ))
        ;; update x-y ranges if necessary
        (do ((y (cdr result) (cddr y)))
           ((null y))
          (setf yy (car y))
          (if (> yy ymax) (setf ymax yy))
          (if (< yy ymin) (setf ymin yy)) )
        (update-ranges xstart xend ymin ymax)
        (setf pltcmd
              (if (get-option '$filled_func)
                  (format nil " ~a w filledcurves x1 lc rgb '~a'"
                              (make-obj-title (get-option '$key))
                              (get-option '$fill_color))
                  (format nil " ~a w l lw ~a lt ~a lc rgb '~a'"
                              (make-obj-title (get-option '$key))
                              (get-option '$line_width)
                              (get-option '$line_type)
                              (get-option '$color)))  )
        (make-gr-object
           :name   'explicit
           :command pltcmd
           :groups '((2))  ; numbers are sent to gnuplot in groups of 2
           :points  `(,(make-array (length result) :element-type 'double-float
                                                   :initial-contents result))    ) )))







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
	 (x1 (coerce (+ xmin (/ (* xdelta (+ (car point1) (caddr point1))) 2)) 'double-float) )
	 (y1 (coerce (+ ymin (/ (* ydelta (+ (cadr point1) (cadddr point1))) 2)) 'double-float) )
	 (x2 (coerce (+ xmin (/ (* xdelta (+ (car point2) (caddr point2))) 2)) 'double-float) )
	 (y2 (coerce (+ ymin (/ (* ydelta (+ (cadr point2) (cadddr point2))) 2)) 'double-float) ))
    (setq pts (nconc (list x1 y1 x2 y2 t t) pts))))
	

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
	 (xdelta (/ (- xmax xmin) ($first ip-grid)))
	 (ydelta (/ (- ymax ymin) ($second ip-grid)))
	 (sample (make-array `(,(1+ ($first ip-grid))
			       ,(1+ ($second ip-grid)))))
	 (ssample (make-array `(,(1+ ($first ip-grid-in))
				,(1+ ($second ip-grid-in))))) )
    
    (setq e (coerce-float-fun (convert-to-float (imp-pl-prepare-expr expr))
			      `((mlist simp)
				,x ,y)))

    (update-ranges xmin xmax ymin ymax)

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
    (setf pltcmd (format nil " ~a w l lw ~a lt ~a lc rgb '~a'"
                              (make-obj-title (get-option '$key))
                              (get-option '$line_width)
                              (get-option '$line_type)
                              (get-option '$color)))
    (make-gr-object
       :name   'implicit
       :command pltcmd
       :groups '((2))
       :points  `(,(make-array (length pts) ; element-type 'double-float removed,
                                            ; since pts contains non floats
                                            :initial-contents pts)) ) ))







;; Object: 'explicit3d'
;; Usage:
;;     explicit(fcn,var1,minval1,maxval1,var2,minval2,maxval2)
;; Options:
;;     xu_grid
;;     yv_grid
;;     line_type
;;     color
;;     key
;; Note: implements a clon of draw3d (plot.lisp) with some
;;       mutations to fit the draw environment.
;;       Read source in plot.lisp for more information
(defun explicit3d (fcn var1 minval1 maxval1 var2 minval2 maxval2)
  (let* ((xu_grid (gethash '$xu_grid  *gr-options*))
         (yv_grid (gethash '$yv_grid  *gr-options*))
         (fminval1 (convert-to-float minval1))
         (fminval2 (convert-to-float minval2))
         (fmaxval1 (convert-to-float maxval1))
         (fmaxval2 (convert-to-float maxval2))
         (epsx (convert-to-float (/ (- fmaxval1 fminval1) xu_grid)))
         (epsy (convert-to-float (/ (- fmaxval2 fminval2) yv_grid)))
         (x 0.0)
         (y 0.0)
         (zmin 1.75555970201398d+305)
         (zmax -1.75555970201398d+305)
         (nx (+ xu_grid 1))
         (ny (+ yv_grid 1))
         ($numer t)
         (count -1)
         result z)
    (setq fcn (coerce-float-fun fcn `((mlist),var1 ,var2)))
    (setf result (make-array (* 3 nx ny) :element-type 'double-float))
    (loop for j below ny
           initially (setq y fminval2)
           do (setq x fminval1)
           (loop for i below nx
                  do
                  (setf z (funcall fcn x y))
                  (if (> z zmax) (setf zmax z))
                  (if (< z zmin) (setf zmin z))
                  (setf (aref result (incf count)) x)
                  (setf (aref result (incf count)) y)
                  (setf (aref result (incf count)) z)
                  (setq x (+ x epsx))
                  )
           (setq y (+ y epsy)))
    (update-ranges fminval1 fmaxval1 fminval2 fmaxval2 zmin zmax)
    (make-gr-object
       :name   'explicit
       :command (format nil " ~a w l lt ~a lc rgb '~a'"
                            (make-obj-title (get-option '$key))
                            (get-option '$line_type)
                            (get-option '$color))
       :groups `((3 ,nx))
       :points  (list result)  )))









;; Object: 'parametric'
;; Usage:
;;     parametric(xfun,yfun,par,parmin,parmax)
;; Options:
;;     nticks
;;     line_width
;;     line_type
;;     key
;;     color
;; Note: similar to draw2d-parametric in plot.lisp
(defun parametric (xfun yfun par parmin parmax)
  (let* ((nticks (gethash '$nticks  *gr-options*))
         ($numer t)
         (tmin (convert-to-float parmin))
         (tmax (convert-to-float parmax))
         (xmin 1.75555970201398d+305)
         (xmax -1.75555970201398d+305)
         (ymin 1.75555970201398d+305)
         (ymax -1.75555970201398d+305)
         (tt (convert-to-float parmin))
         (eps (/ (- tmax tmin) (- nticks 1)))
         result f1 f2 x y)
    (if (< tmax tmin)
       (merror "draw2d (parametric): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist), par)))
    (setq f2 (coerce-float-fun yfun `((mlist), par)))
    (setf result
       (loop
          do (setf x (convert-to-float (funcall f1 tt)))
             (if (> x xmax) (setf xmax x))
             (if (< x xmin) (setf xmin x))
             (setf y (convert-to-float (funcall f2 tt)))
             (if (> y ymax) (setf ymax y))
             (if (< y ymin) (setf ymin y))
          collect x
          collect y
          when (>= tt tmax) do (loop-finish)
          do (setq tt (+ tt eps))
             (if (>= tt tmax) (setq tt tmax)) ))
    ; update x-y ranges if necessary
    (update-ranges xmin xmax ymin ymax)
    (make-gr-object
       :name 'parametric
       :command (format nil " ~a w l lw ~a lt ~a lc rgb '~a'"
                            (make-obj-title (get-option '$key))
                            (get-option '$line_width)
                            (get-option '$line_type)
                            (get-option '$color))
       :groups '((2))
       :points `(,(make-array (length result) :element-type 'double-float
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
;; This object is constructed as a parametric function
(defun polar (radius ang minang maxang)
  (let ((grobj (parametric `((mtimes simp) ,radius ((%cos simp) ,ang))
                            `((mtimes simp) ,radius ((%sin simp) ,ang))
                            ang minang maxang)))
    (setf (gr-object-name grobj) 'polar)
    grobj ))







;; Object: 'parametric3d'
;; Usage:
;;     parametric(xfun,yfun,zfun,par,parmin,parmax)
;; Options:
;;     nticks
;;     line_width
;;     line_type
;;     color
;;     key
(defun parametric3d (xfun yfun zfun par parmin parmax)
  (let* ((nticks (gethash '$nticks  *gr-options*))
         ($numer t)
         (tmin (convert-to-float parmin))
         (tmax (convert-to-float parmax))
         (xmin 1.75555970201398d+305)
         (xmax -1.75555970201398d+305)
         (ymin 1.75555970201398d+305)
         (ymax -1.75555970201398d+305)
         (zmin 1.75555970201398d+305)
         (zmax -1.75555970201398d+305)
         (tt parmin)
         (eps (/ (- tmax tmin) (- nticks 1)))
         result f1 f2 f3 x y z)
    (if (< tmax tmin)
       (merror "draw3d (parametric): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist), par)))
    (setq f2 (coerce-float-fun yfun `((mlist), par)))
    (setq f3 (coerce-float-fun zfun `((mlist), par)))
    (setf result
       (loop
          do (setf x (funcall f1 tt))
             (if (> x xmax) (setf xmax x))
             (if (< x xmin) (setf xmin x))
             (setf y (funcall f2 tt))
             (if (> y ymax) (setf ymax y))
             (if (< y ymin) (setf ymin y))
             (setf z (funcall f3 tt))
             (if (> z zmax) (setf zmax z))
             (if (< z zmin) (setf zmin z))
          collect x
          collect y
          collect z
          when (>= tt tmax) do (loop-finish)
          do (setq tt (+ tt eps))
             (if (>= tt tmax) (setq tt tmax)) ))
    ; update x-y ranges if necessary
    (update-ranges xmin xmax ymin ymax zmin zmax)
    (make-gr-object
       :name 'parametric
       :command (format nil " ~a w l lw ~a lt ~a lc rgb '~a'"
                            (make-obj-title (get-option '$key))
                            (get-option '$line_width)
                            (get-option '$line_type)
                            (get-option '$color))
       :groups '((3 0))  ; numbers are sent to gnuplot in groups of 3, without blank lines
       :points `(,(make-array (length result) :element-type 'double-float
                                              :initial-contents result))  )) )








;; Object: 'parametric_surface'
;; Usage:
;;     parametric_surface(xfun,yfun,zfun,par1,par1min,par1max,par2,par2min,par2max)
;; Options:
;;     xu_grid
;;     yv_grid
;;     line_type
;;     color
;;     key
(defun parametric_surface (xfun yfun zfun par1 par1min par1max par2 par2min par2max)
  (let* ((ugrid (gethash '$xu_grid  *gr-options*))
         (vgrid (gethash '$yv_grid  *gr-options*))
         ($numer t)
         (umin (convert-to-float par1min))
         (umax (convert-to-float par1max))
         (vmin (convert-to-float par2min))
         (vmax (convert-to-float par2max))
         (xmin 1.75555970201398d+305)
         (xmax -1.75555970201398d+305)
         (ymin 1.75555970201398d+305)
         (ymax -1.75555970201398d+305)
         (zmin 1.75555970201398d+305)
         (zmax -1.75555970201398d+305)
         (ueps (/ (- umax umin) (- ugrid 1)))
         (veps (/ (- vmax vmin) (- vgrid 1)))
         (nu (+ ugrid 1))
         (nv (+ vgrid 1))
         result f1 f2 f3 x y z uu vv)
    (if (or (< umax umin)
            (< vmax vmin))
       (merror "draw3d (parametric_surface): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist), par1 ,par2)))
    (setq f2 (coerce-float-fun yfun `((mlist), par1 ,par2)))
    (setq f3 (coerce-float-fun zfun `((mlist), par1 ,par2)))
    (loop for j below nv
           initially (setq vv vmin)
           do (setq uu umin)
           (loop for i below nu
                  do
                  (setf x (funcall f1 uu vv))
                  (if (> x xmax) (setf xmax x))
                  (if (< x xmin) (setf xmin x))
                  (setf y (funcall f2 uu vv))
                  (if (> y ymax) (setf ymax y))
                  (if (< y ymin) (setf ymin y))
                  (setf z (funcall f3 uu vv))
                  (if (> z zmax) (setf zmax z))
                  (if (< z zmin) (setf zmin z))
                  (setf result (append result (list x y z)))
                  (setq uu (+ uu ueps))
                  (if (> uu umax) (setf uu umax))
                  )
           (setq vv (+ vv veps))
           (if (> vv vmax) (setf vv vmax)))
    ; update x-y ranges if necessary
    (update-ranges xmin xmax ymin ymax zmin zmax)
    (make-gr-object
       :name 'parametric_surface
       :command (format nil " ~a w l lt ~a lc rgb '~a'"
                            (make-obj-title (get-option '$key))
                            (get-option '$line_type)
                            (get-option '$color))
       :groups `((3 ,nu))  ; numbers are sent to gnuplot in groups of 3, with blank lines every nu lines
       :points `(,(make-array (length result) :element-type 'double-float
                                              :initial-contents result))   )) )







;; Object: 'image'
;; Usages:
;;     image(matrix_of_reals,x0,y0,width,height)
;;     image(matrix_of_[r,g,b],x0,y0,width,height)
;; Options:
;;     colorbox (this is global)
;;     palette  (this is global)
(defun image (mat x0 y0 width height)
  (let ( (fx0 (convert-to-float x0))
         (fy0 (convert-to-float y0))
         (fwidth (convert-to-float width))
         (fheight (convert-to-float height))
         result nrows ncols dx dy n
         )
    (if (not ($matrixp mat))
      (merror "draw2d (image): first argument is not a matrix") )
    (setf nrows (length (cdr mat))
          ncols (length (cdadr mat)))
    (setf dx (/ fwidth ncols)
          dy (/ fheight nrows))
    (if (not ($listp (cadadr mat)))  ; it's a matrix of reals
        (setf n 3)   ; 3 numbers to be sent to gnuplot: x,y,value
        (setf n 5))  ; 5 numbers to be sent: x,y,r,g,b
    (case n
      (3 (setf result (make-array (* 3 nrows ncols) :element-type 'double-float))
         (let ((yi (+ fy0 height (* dy -0.5)))
               (counter -1)
                xi)
            (loop for row on (cdr mat) by #'cdr do
              (setf xi (+ fx0 (* dx 0.5)))
              (loop for col on (cdar row) by #'cdr do
                (setf (aref result (incf counter)) xi
                      (aref result (incf counter)) yi
                      (aref result (incf counter)) (convert-to-float (car col)))
                (setf xi (+ xi dx)))
              (setf yi (- yi dy)) )))
      (5 (setf result (make-array (* 5 nrows ncols) :element-type 'double-float))
         (let ((yi (+ fy0 height (* dy -0.5)))
               (counter -1)
                xi colors)
            (loop for row on (cdr mat) by #'cdr do
              (setf xi (+ fx0 (* dx 0.5)))
              (loop for col on (cdar row) by #'cdr do
                (setf colors (cdar col))
                (setf (aref result (incf counter)) xi
                      (aref result (incf counter)) yi
                      (aref result (incf counter)) (convert-to-float (car colors))
                      (aref result (incf counter)) (convert-to-float (cadr colors))
                      (aref result (incf counter)) (convert-to-float (caddr colors)))
                (setf xi (+ xi dx)))
              (setf yi (- yi dy)) ))))
    ; update x-y ranges if necessary
    (update-ranges fx0 (+ fx0 fwidth) fy0 (+ fy0 fheight))
    (make-gr-object
       :name 'image
       :command (case n
                   (3 (format nil " t '' w image"))
                   (5 (format nil " t '' w rgbimage")))
       :groups (case n
                   (3 '((3 0)))   ; numbers are sent to gnuplot in gropus of 3, no blank lines
                   (5 '((5))  ))  ; numbers in groups of 5
       :points (list result)) ) )







;; This function builds a 2d scene by calling the 
;; graphic objects constructors.
(defun make-scene-2d (args)
   (let ((objects nil)
         plotcmd)
      (ini-gr-options)
      ; update option values and detect objects to be plotted
      (dolist (x args)
         (cond ((equal ($op x) '&=)
                   (update-gr-option ($lhs x) ($rhs x)))
               (t  (setf objects
                         (append
                            objects 
                            (list (case (caar x)
                                     ($points      (apply #'points (rest x)))
                                     ($polygon     (apply #'polygon (rest x)))
                                     ($ellipse     (apply #'ellipse (rest x)))
                                     ($rectangle   (apply #'rectangle (rest x)))
                                     ($explicit    (apply #'explicit (rest x)))
				     ($implicit    (apply #'implicit (rest x)))
                                     ($parametric  (apply #'parametric (rest x)))
                                     ($vector      (apply #'vect (rest x)))
                                     ($label       (apply #'label (rest x)))
                                     ($polar       (apply #'polar (rest x)))
                                     ($image       (apply #'image (rest x)))
                                     (otherwise (merror "Graphical 2d object ~M is not recognized" x)))))))))
      ; save in plotcmd the plot command to be sent to gnuplot
      (setf plotcmd
         (concatenate 'string
            ; this let statement is to prevent error messages from gnuplot when
            ; the amplitude of the ranges equals zero
            (let ((xi (first  (get-option '$xrange)))
                  (xf (second (get-option '$xrange)))
                  (yi (first  (get-option '$yrange)))
                  (yf (second (get-option '$yrange))))
               (when (= xi xf)
                  (setf xi (- xi 0.01)
                        xf (+ xf 0.01)))
               (when (= yi yf)
                  (setf yi (- yi 0.01)
                        yf (+ yf 0.01)))
               (format nil "set xrange [~a:~a]~%set yrange [~a:~a]~%" xi xf yi yf)  )
            (if (get-option '$logx)
               (format nil "set logscale x~%")
               (format nil "unset logscale x~%"))
            (if (get-option '$logy)
               (format nil "set logscale y~%")
               (format nil "unset logscale y~%"))
            (if (get-option '$grid)
                (format nil "set grid~%")
                (format nil "unset grid~%"))
            (format nil "set title \"~a\"~%" (get-option '$title))
            (format nil "set xlabel \"~a\"~%" (get-option '$xlabel))
            (format nil "set ylabel \"~a\"~%" (get-option '$ylabel))
            (let ((suma 0))
              (if (get-option '$axis_bottom)  (setf suma (+ suma 1)))
              (if (get-option '$axis_left) (setf suma (+ suma 2)))
              (if (get-option '$axis_top) (setf suma (+ suma 4)))
              (if (get-option '$axis_right) (setf suma (+ suma 8)))
              (format nil "set border ~a~%" suma) )
            (if (get-option '$xtics)
               (format nil "set xtics~%")
               (format nil "unset xtics~%"))
            (if (get-option '$ytics)
               (format nil "set ytics~%")
               (format nil "unset ytics~%"))
            (if (get-option '$colorbox)
               (format nil "set colorbox~%")
               (format nil "unset colorbox~%"))
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
         )  ))








;; This function builds a 3d scene by calling the 
;; graphic objects constructors.
(defun make-scene-3d (args)
   (let ((objects nil)
         plotcmd)
      (ini-gr-options)
      ; update option values and detect objects to be plotted
      (dolist (x args)
         (cond ((equal ($op x) '&=)
                  (update-gr-option ($lhs x) ($rhs x)))
               (t  (setf objects
                         (append
                            objects 
                            (list (case (caar x)
                                     ($points             (apply #'points3d (rest x)))
                                     ($explicit           (apply #'explicit3d (rest x)))
                                     ($vector             (apply #'vect3d (rest x)))
                                     ($parametric         (apply #'parametric3d (rest x)))
                                     ($parametric_surface (apply #'parametric_surface (rest x)))
                                     ($label              (apply #'label3d (rest x)))
                                     (otherwise (merror "Graphical 3d object ~M is not recognized" x)))))))))
      ; save in plotcmd the plot command to be sent to gnuplot
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
            (case (get-option '$contour)
               ($surface (format nil "set contour surface;set cntrparam levels ~a~%"
                                      (get-option '$contour_levels) ))
               ($base    (format nil "set contour base;set cntrparam levels ~a~%"
                                      (get-option '$contour_levels) ))
               ($both    (format nil "set contour both;set cntrparam levels ~a~%"
                                      (get-option '$contour_levels) ))
               ($map     (format nil "set contour base~%unset surface~%set cntrparam levels ~a~%"
                                      (get-option '$contour_levels))) )
            (format nil "set title \"~a\"~%"  (get-option '$title))
            (format nil "set xlabel \"~a\"~%" (get-option '$xlabel))
            (format nil "set ylabel \"~a\"~%" (get-option '$ylabel))
            (format nil "set zlabel \"~a\"~%" (get-option '$zlabel))
            (if (get-option '$logx)
               (format nil "set logscale x~%")
               (format nil "unset logscale x~%"))
            (if (get-option '$logy)
               (format nil "set logscale y~%")
               (format nil "unset logscale y~%"))
            (if (get-option '$logz)
               (format nil "set logscale z~%")
               (format nil "unset logscale z~%"))
            (if (get-option '$grid)
                (format nil "set grid~%")
                (format nil "unset grid~%"))
            (if (get-option '$xtics)
               (format nil "set xtics~%")
               (format nil "unset xtics~%"))
            (if (get-option '$ytics)
               (format nil "set ytics~%")
               (format nil "unset ytics~%"))
            (if (get-option '$ztics)
               (format nil "set ztics~%")
               (format nil "unset ztics~%"))
            (if (eql (get-option '$contour) '$map)  ; if contour = map
               (format nil "set view map~%")
               (format nil "set view ~a, ~a, 1, 1~%"
                            (get-option '$rot_vertical)
                            (get-option '$rot_horizontal))  )
            (if (not (get-option '$axis_3d))
                (format nil "set border 0~%"))
            (if (get-option '$enhanced3d)
               (format nil "set pm3d~%"))
            (if (get-option '$surface_hide)
               (format nil "set hidden3d~%"))
            (if (get-option '$colorbox)
               (format nil "set colorbox~%")
               (format nil "unset colorbox~%"))
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
  (let ((scenes nil)
        (counter 0)
        (scenes-list '((mlist simp)))   ; these two variables will be used
        scene-short-description         ; to build the output of function draw
        pltcmd   ; the entire plot command is stored here
        dest datastorage dataplace is1stobj biglist grouplist ncols nrows width height)
    (dolist (x args)
      (cond ((equal ($op x) '&=)
              (case ($lhs x)
                ($terminal   (update-gr-option '$terminal ($rhs x)))
                ($columns    (update-gr-option '$columns ($rhs x)))
                ($pic_width  (update-gr-option '$pic_width ($rhs x)))
                ($pic_height (update-gr-option '$pic_height ($rhs x)))
                ($eps_width  (update-gr-option '$eps_width ($rhs x)))
                ($eps_height (update-gr-option '$eps_height ($rhs x)))
                ($file_name  (update-gr-option '$file_name ($rhs x)))
                (otherwise (merror "Unknown global option ~M " ($lhs x)))  ))
            ((equal (caar x) '$gr3d)
              (setf scenes (append scenes (list (funcall #'make-scene-3d (rest x))))))
            ((equal (caar x) '$gr2d)
              (setf scenes (append scenes (list (funcall #'make-scene-2d (rest x))))))
            (t
              (merror "draw: item ~M is not recognized" x)))   )

    (cond ((null $draw_pipes)   ; no pipes
             (setf dest (open (plot-temp-file "maxout.gnuplot")
                              :direction :output :if-exists :supersede))
             (setf datastorage dest)
             (setf dataplace "'-'"))
          (t ; if pipes, start gnuplot process
             (setf $gnuplot_command $draw_command)
             (check-gnuplot-process)
             ($gnuplot_reset)
             (setf dest *gnuplot-stream*)
             (cond ((> (length scenes) 1)   ; multiplot => pipe code + coordinates 
                      (setf datastorage dest)
                      (setf dataplace "'-'"))
                   (t                       ; one plot => pipe code, file coordinates 
                      (setf datastorage
                            (open (plot-temp-file "maxout.gnuplot_pipes")
                            :direction :output :if-exists :supersede))
                      (setf dataplace 
                            (format nil "'~a'" (plot-temp-file "maxout.gnuplot_pipes"))))) ))

    ; write global options
    (case (gethash '$terminal *gr-options*)
      ($png (format dest "set terminal png size ~a, ~a~%set out '~a.png'~%"
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_name)) )
      ($eps (format dest "set terminal postscript eps size ~acm, ~acm~%set out '~a.eps'~%"
                           (get-option '$eps_width)
                           (get-option '$eps_height)
                           (get-option '$file_name)))
      ($eps_color (format dest "set terminal postscript eps color size ~acm, ~acm~%set out '~a.eps'~%"
                           (get-option '$eps_width)
                           (get-option '$eps_height)
                           (get-option '$file_name)))
      ($jpg (format dest "set terminal jpeg size ~a, ~a~%set out '~a.jpg'~%"
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_name)))  )

    ; compute some parameters for multiplot
    (setf ncols (get-option '$columns))
    (setf nrows (ceiling (/ (length scenes) ncols)))
    (setf width (/ 1.0 ncols))
    (setf height (/ 1.0 nrows))
    (if (> (length scenes) 1)
      (format dest "set size 1.0, 1.0~%set origin 0.0, 0.0~%set multiplot~%"))

    ; write descriptions of 2d and 3d scenes
    (let ((i 0))
      (dolist (scn scenes)
        ; write size and origin
        (format dest "set size ~a, ~a~%" width height)
        (format dest "set origin ~a, ~a~%" (* width (mod counter ncols))
                                         (* height (- nrows 1.0 (floor (/ counter ncols)))))
        (setf is1stobj t
              biglist '()
              grouplist '())
        (format dest "~a" (second scn))
        (cond ((= (first scn) 2)    ; it's a 2d scene
                 (setf scene-short-description '(($gr2d simp)))
                 (setf pltcmd (format nil "plot ")))
              ((= (first scn) 3)    ; it's a 3d scene
                 (setf scene-short-description '(($gr3d simp)))
                 (setf pltcmd (format nil "splot "))))

          (dolist (obj (third scn))
             (setf scene-short-description
                   (cons (gr-object-name obj) scene-short-description))
             (cond ((not (eql (gr-object-name obj) 'label))
                    (if is1stobj
                      (setf is1stobj nil)
                      (setf pltcmd (concatenate 'string pltcmd (format nil ", "))))
                    (setf pltcmd (concatenate 'string
                                               pltcmd
                                               (let ((pcom (gr-object-command obj))
                                                     (pipe1scene (and $draw_pipes (= (length scenes) 1))))
                                                 (cond
                                                    ((listp pcom)
                                                       (format nil "~a~a~a, ~a~a~a"
                                                               dataplace
                                                               (if pipe1scene
                                                                 (format nil " index ~a" (incf i))
                                                                 "")
                                                               (car pcom)
                                                               dataplace
                                                               (if pipe1scene
                                                                 (format nil " index ~a" i)
                                                                 "")
                                                               (cadr pcom)))
                                                    (t (format nil "~a~a~a"
                                                               dataplace
                                                               (if pipe1scene
                                                                 (format nil " index ~a" i)
                                                                 "")
                                                               pcom)
                                                        )))  ))
                    (incf i)
                    (setf grouplist (append grouplist (gr-object-groups obj)))
                    (setf biglist (append biglist (gr-object-points obj))) )
                 (t ; if it's a label, write directly to dest, since in gnuplot 4.0
                    ; labels are in the preamble; this should be changed in future versions
                    (format dest "~a" (gr-object-command obj))))    )

        ; don't remove this print statement
        ; I use it to check the plot command to be sent to gnuplot
        ;(print pltcmd)

        (if (and $draw_pipes (= (length scenes) 1))
           (format dest "~%")
           (format dest "~a~%" pltcmd) )

        (do ( (blis biglist (cdr blis))
              (glis grouplist (cdr glis) ))
            ((null blis) 'done)
          (let* ((vect (car blis))
                 (k (length vect)))
            (case (caar glis)
             (2  ; 2d points
                 (do ((cont 0 (+ cont 2)))
                   ((= cont k) 'done)
                   (if (and (numberp (aref vect cont)) (numberp (aref vect (1+ cont))))
                       (format datastorage "~a ~a ~%" (aref vect cont) (aref vect (1+ cont)))
                       (format datastorage "~%")))   )
             (3  ; 3d points, gray image and palette image
                (let ((l 0)
                      (m (cadar glis)))
                   (cond
                     ((= m 0)     ; 3d points without blank lines
                        (do ((cont 0 (+ cont 3)))
                            ((= cont k) 'done)
                          (format datastorage "~a ~a ~a ~%" (aref vect cont)
                                                     (aref vect (1+ cont))
                                                     (aref vect (+ 2 cont)))  )  )
                     (t           ; 3d points with blank lines every m lines
                        (do ((cont 0 (+ cont 3)))
                            ((= cont k) 'done)
                          (when (eql l m)
                                (format datastorage "~%")
                                (setf l 0) )
                          (format datastorage "~a ~a ~a ~%" (aref vect cont)
                                                     (aref vect (1+ cont))
                                                     (aref vect (+ 2 cont)))
                          (incf l)  ))))  )
             (4  ; for 2d vectors (x,y,dx,dy)
                 (do ((cont 0 (+ cont 4)))
                     ((= cont k) 'done)
                   (format datastorage "~a ~a ~a ~a~%" (aref vect cont)
                                                (aref vect (1+ cont))
                                                (aref vect (+ 2 cont))
                                                (aref vect (+ 3 cont))) ) )
             (5  ; for rgb images
                 (do ((cont 0 (+ cont 5)))
                     ((= cont k) 'done)
                   (format datastorage "~a ~a ~a ~a ~a~%" (aref vect cont)
                                                   (aref vect (1+ cont))
                                                   (aref vect (+ 2 cont))
                                                   (aref vect (+ 3 cont))
                                                   (aref vect (+ 4 cont)))))
             (6  ; for 3d vectors (x,y,z,dx,dy,dz)
                 (do ((cont 0 (+ cont 6)))
                     ((= cont k) 'done)
                   (format datastorage "~a ~a ~a ~a ~a ~a~%" (aref vect cont)
                                                (aref vect (1+ cont))
                                                (aref vect (+ 2 cont))
                                                (aref vect (+ 3 cont)) 
                                                (aref vect (+ 4 cont))
                                                (aref vect (+ 5 cont)) )))   ))
          (if (and $draw_pipes (= (length scenes) 1))
            (if (cdr blis) (format datastorage "~%~%"))
            (format datastorage "e~%")) )

        (incf counter)
        (setf scenes-list (cons (reverse scene-short-description) scenes-list)) ))  ; end let-dolist scenes

    (cond ((> (length scenes) 1)
             (format dest "unset multiplot~%"))
          ; now, if gnuplot is running in interactive mode
          ; and we want to save the coordinates in a file,
          ; print them when hitting the x key after clicking the mouse button
          ((and (not (string= (gethash '$xy_file *gr-options*) ""))
                (or (string= *autoconf-win32* "true") $draw_pipes))
             (format dest "set print \"~a\" append~%bind x \"print MOUSE_X,MOUSE_Y\"~%"
                          (gethash '$xy_file *gr-options*))) )

    ; get the plot
    (cond ((null $draw_pipes)   ; if no pipes, close the file & call gnuplot
         (close dest)
         ($system (if (equal (gethash '$terminal *gr-options*) '$screen)
                          (format nil "~a ~a"
                                      $draw_command
                                      (format nil $gnuplot_view_args (plot-temp-file "maxout.gnuplot")))
                          (format nil "~a \"~a\"" $draw_command (plot-temp-file "maxout.gnuplot")))) )
         ((= (length scenes) 1)   ; pipe 1 plot
            (close datastorage)
            (send-gnuplot-command pltcmd))
         (t                       ; pipe multiplot
            (force-output dest)) )

    ; the output is a simplified description of the scene(s)
    (reverse scenes-list)    ) )



;; Equivalent to draw2d(opt & obj)
(defun $draw2d (&rest args)
   ($draw (cons '($gr2d) args)) )


;; Equivalent to draw3d(opt & obj)
(defun $draw3d (&rest args)
   ($draw (cons '($gr3d) args)) )


;; This function transforms an integer number into
;; a string, adding zeros at the left side until the
;; length of the string equals 10. This function is
;; useful for animations.
(defun $add_zeroes (num)
   (format nil "~10,'0d" num) )

