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


;; Sets default values to graphics options
(defun ini-gr-options ()
  (setf ; graphics options
      ; header options
      (gethash '$xrange *gr-options*) nil      ; nil => automatic computation
      (gethash '$yrange *gr-options*) nil      ; nil => automatic computation
      (gethash '$zrange *gr-options*) nil      ; nil => automatic computation
      (gethash '$logx *gr-options*)  nil
      (gethash '$logy *gr-options*)  nil
      (gethash '$logz *gr-options*)  nil
      (gethash '$terminal *gr-options*) '$screen
      (gethash '$grid *gr-options*) nil
      (gethash '$title *gr-options*) ""
      (gethash '$xlabel *gr-options*) ""
      (gethash '$ylabel *gr-options*) ""
      (gethash '$zlabel *gr-options*) ""
      (gethash '$xtics *gr-options*) t
      (gethash '$ytics *gr-options*) t
      (gethash '$ztics *gr-options*) t
      (gethash '$rot_vertical *gr-options*) 60     ; range: [0,180] (vertical rotation)
      (gethash '$rot_horizontal *gr-options*) 30   ; range: [0,360] (horizontal rotation)
      (gethash '$xy_file *gr-options*) ""
      (gethash '$user_preamble *gr-options*) ""

      ; implicit plot options
      (gethash '$ip_grid *gr-options*) '((mlist simp) 50 50)
      (gethash '$ip_grid_in *gr-options*) '((mlist simp) 5 5)

      ; pictures
      (gethash '$file_name *gr-options*) "maxima_out"
      (gethash '$pic_width *gr-options*)  640    ; only for bitmap pictures
      (gethash '$pic_height *gr-options*) 480    ; only for bitmap pictures

      ; 2d-axis
      (gethash '$axis_bottom *gr-options*) t
      (gethash '$axis_left *gr-options*) t
      (gethash '$axis_top *gr-options*) t
      (gethash '$axis_right *gr-options*) t
      (gethash '$axis_3d *gr-options*) t

      ; point options
      (gethash '$point_size *gr-options*) 1
      (gethash '$point_type *gr-options*) 1
      (gethash '$points_joined *gr-options*) nil

      ; polygon  options
      (gethash '$fill_type *gr-options*) 1
      (gethash '$transparent *gr-options*) nil
      (gethash '$border *gr-options*) t

      ; vector  options
      (gethash '$head_both *gr-options*) nil
      (gethash '$head_length *gr-options*) 2       ; in x-axis units
      (gethash '$head_angle *gr-options*) 45       ; with respect to the segment
      (gethash '$head_type *gr-options*) '$filled  ; other options are: $empty and $nofilled

      ; label options
      (gethash '$label_alignment *gr-options*) '$center ; other options are: $left and $right
      (gethash '$label_orientation *gr-options*) '$horizontal ; the other option is $vertical
      (gethash '$label_color *gr-options*) 1 ; as in line_type

      ; line options
      (gethash '$line_width *gr-options*) 1
      (gethash '$line_type *gr-options*) 1

      ; function options
      (gethash '$nticks *gr-options*) 30
      (gethash '$adapt_depth *gr-options*) 10
      (gethash '$key *gr-options*) ""                  ; by default, no keys
      (gethash '$function_style *gr-options*) '$lines  ; other options are: $filled and $dots

      ; 3d options
      (gethash '$xu_grid *gr-options*) 30
      (gethash '$yv_grid *gr-options*) 30
      (gethash '$surface_hide *gr-options*) nil
      (gethash '$contour *gr-options*) '$none  ; other options are: $base, $surface, $both and $map
      (gethash '$contour_levels *gr-options*) 5  ; maximum: 50

  ) )


;; Sets default values to global options
(defun ini-global-options ()
  (setf ; global options
      (gethash '$columns *gr-options*) 1
  ) )


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
      (($line_width $head_length $head_angle) ; defined as positive numbers
            (if (and (numberp val)
                     (> val 0 ))
                (setf (gethash opt *gr-options*) val)
                (merror "Non positive number: ~M " val)))
      ($point_size ; defined as non negative numbers
            (if (and (numberp val)
                     (>= val 0 ))
                (setf (gethash opt *gr-options*) val)
                (merror "Negative number: ~M " val)))

      (($fill_type $point_type $line_type $label_color) ; numbers >= -1
            (if (and (numberp val)
                     (>= val -1 ))
                (setf (gethash opt *gr-options*) val)
                (merror "Negative number: ~M " val)))
      (($columns $nticks $adapt_depth $pic_width $pic_height     ; defined as positive integers
        $xu_grid $yv_grid $contour_levels)
            (if (and (integerp val)
                     (> val 0 ))
                (setf (gethash opt *gr-options*) val)
                (merror "Non positive integer: ~M " val)))
      (($points_joined $transparent $border $logx $logy $logz $head_both $grid $xtics $ytics $ztics
        $axis_bottom $axis_left $axis_top $axis_right $axis_3d $surface_hide) ; defined as true or false
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
      ($function_style ; defined as $filled, $dots and $lines
            (if (member val '($lines $filled $dots))
                (setf (gethash opt *gr-options*) val)
                (merror "Illegal function style: ~M" val)))
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
      (($key $title $xlabel $ylabel $zlabel $file_name $xy_file $user_preamble)  ; defined as strings
            (setf (gethash opt *gr-options*) (string-trim "\"" (coerce (mstring val) 'string))))
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
   name command points)

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
      (setf pts (mapcan #'list x y))
      (setf xmin ($tree_reduce 'min (cons '(mlist simp) x))
            xmax ($tree_reduce 'max (cons '(mlist simp) x))
            ymin ($tree_reduce 'min (cons '(mlist simp) y))
            ymax ($tree_reduce 'max (cons '(mlist simp) y)) )
      ;; update x-y ranges if necessary
      (update-ranges xmin xmax ymin ymax)
      (make-gr-object
         :name 'points
         :command (if (get-option '$points_joined)
                     (format nil "'-' ~a w lp ps ~a pt ~a lw ~a lt ~a"
                                 (make-obj-title (get-option '$key))
                                 (get-option '$point_size)
                                 (get-option '$point_type)
                                 (get-option '$line_width)
                                 (get-option '$line_type))
                     (format nil "'-' ~a w p ps ~a pt ~a"
                                 (make-obj-title (get-option '$key))
                                 (get-option '$point_size)
                                 (get-option '$point_type)) )
         :points (list (cons 2 pts))) ; 2 => how many numbers are sent together to gnuplot
       ))












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
      (setf pts (mapcan #'list x y z))
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
                     (format nil "'-' ~a w lp ps ~a pt ~a lw ~a lt ~a"
                                 (make-obj-title (get-option '$key))
                                 (get-option '$point_size)
                                 (get-option '$point_type)
                                 (get-option '$line_width)
                                 (get-option '$line_type))
                     (format nil "'-' ~a w p ps ~a pt ~a"
                                 (make-obj-title (get-option '$key))
                                 (get-option '$point_size)
                                 (get-option '$point_type)) )
         :points (list (append '(3 0) pts))) ; 3 => how many numbers are sent together to gnuplot
                                             ; 0 => no blank lines
          ))







;; Object: 'polygon'
;; Usage:
;;     polygon([[x1,y1], [x2,y2], [x3,y3],...])
;;     polygon([x1,x2,x3,...], [y1,y2,y3,...])
;; Options:
;;     transparent
;;     fill_type
;;     border
;;     line_width
;;     line_type
;;     key
(defun polygon (arg1 &optional (arg2 nil))
   (if (and (gethash '$transparent  *gr-options*)
            (not (gethash '$border  *gr-options*)))
       (merror "draw (polygon): transparent is true and border is false; this is not consistent"))
   (let (pltcmd pts x y xmin xmax ymin ymax)
      (cond ((and ($listp arg1)
                  (every #'$listp (rest arg1)))     ; xy format
               (let ((tmp (mapcar #'rest (rest arg1))))
                  (setf x (map 'list #'first tmp)
                        y (map 'list #'second tmp) ) ) )
            ((and ($listp arg1)
                  ($listp arg1)
                  (= (length arg1) (length arg2)))  ; xx yy format
               (setf x (map 'list #'convert-to-float (rest arg1))
                     y (map 'list #'convert-to-float (rest arg2))))
            (t (merror "draw (polygon): bad input format"))  )
      (setf pts (mapcan #'list x y))
      (setf xmin ($tree_reduce 'min (cons '(mlist simp) x))
            xmax ($tree_reduce 'max (cons '(mlist simp) x))
            ymin ($tree_reduce 'min (cons '(mlist simp) y))
            ymax ($tree_reduce 'max (cons '(mlist simp) y)) )
      ;; update x-y ranges if necessary
      (update-ranges xmin xmax ymin ymax)
      (cond
         ((get-option '$transparent)  ; if transparent, draw only the border
             (setf pltcmd (format nil "'-' ~a  w l lw ~a lt ~a"
                                      (make-obj-title (get-option '$key))
                                      (get-option '$line_width)
                                      (get-option '$line_type)))
             (setf pts (list (append '(2) pts (list (first pts) (second pts))))))
         ((not (get-option '$border)) ; no transparent, no border
             (setf pltcmd (format nil "'-' ~a w filledcurves ~a"
                                      (make-obj-title (get-option '$key))
                                      (get-option '$fill_type)))
             (setf pts (list (cons 2 pts))) )
         (t ; no transparent with border
             (setf pltcmd (format nil "'-' ~a w filledcurves ~a, '-' t '' w l lw ~a lt ~a" 
                                      (make-obj-title (get-option '$key))
                                      (get-option '$fill_type)
                                      (get-option '$line_width)
                                      (get-option '$line_type)))
             (setf pts (list (cons 2 pts)
                             (append '(2) pts (list (first pts) (second pts))))) ))
      (make-gr-object
         :name   'polygon
         :command pltcmd
         :points  pts )))






;; Object: 'rectangle'
;; Usage:
;;     rectangle([x1,y1], [x2,y2]), being [x1,y1] & [x2,y2] opposite vertices
;; Options:
;;     transparent
;;     fill_type
;;     border
;;     line_width
;;     line_type
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
;;     ellipse(xc, yc, a, b, startang, endang),  both angles in degrees [-360, 360]
;; Options:
;;     nticks
;;     transparent
;;     fill_type
;;     border
;;     line_width
;;     line_type
;;     key
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
         tmin tmax eps x y tt pltcmd)
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
      (setf x (+ xc (* fa (cos tt))))
      (if (> x xmax) (setf xmax x))
      (if (< x xmin) (setf xmin x))
      (setf y (+ yc (* fb (sin tt))))
      (if (> y ymax) (setf ymax y))
      (if (< y ymin) (setf ymin y))
      (setf result (append (list x y) result))
      (if (>= tt tmax) (return))
      (setf tt (+ tt eps))
      (if (>= tt tmax) (setq tt tmax)) )
    ; update x-y ranges if necessary
    (setf xmin (min xc xmin)
          xmax (max xc xmax)
          ymin (min yc ymin)
          ymax (max yc ymax))
    (update-ranges xmin xmax ymin ymax)
    (cond
       ((get-option '$transparent)  ; if transparent, draw only the border
           (setf pltcmd (format nil "'-' ~a  w l lw ~a lt ~a"
                                    (make-obj-title (get-option '$key))
                                    (get-option '$line_width)
                                    (get-option '$line_type)))
           (setf result (list (cons 2 result))))
       ((not (get-option '$border)) ; no transparent, no border
           (setf pltcmd (format nil "'-' ~a w filledcurves xy=~a,~a ~a"
                                    (make-obj-title (get-option '$key))
                                    fxc fyc
                                    (get-option '$fill_type)))
           (setf result (list (cons 2 result))) )
       (t ; no transparent with border
           (setf pltcmd (format nil "'-' ~a w filledcurves xy=~a,~a ~a, '-' t '' w l lw ~a lt ~a" 
                                    (make-obj-title (get-option '$key))
                                    fxc fyc
                                    (get-option '$fill_type)
                                    (get-option '$line_width)
                                    (get-option '$line_type)))
           (setf result (list (cons 2 result) (cons 2 result))) ))
    (make-gr-object
       :name   'ellipse
       :command pltcmd
       :points  result ) ))









;; Object: 'label'
;; Usage:
;;     label(string,x,y)
;; Options:
;;     label_alignment
;;     label_orientation
;;     label_color
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
         :command (format nil "set label ~a at ~a, ~a ~a ~a tc lt ~a~%"
                              text fx fy
                              (case (get-option '$label_alignment)
                                 ($center "center")
                                 ($left   "left")
                                 ($right  "right"))
                              (case (get-option '$label_orientation)
                                 ($horizontal "norotate")
                                 ($vertical  "rotate"))
                              (get-option '$label_color))
         :points nil) ; labels are in the preamble, this will change in gnuplot 4.2
         ))







;; Object: 'label3d'
;; Usage:
;;     label(string,x,y,z)
;; Options:
;;     label_alignment
;;     label_orientation
;;     label_color
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
         :command (format nil "set label ~a at ~a, ~a, ~a ~a ~a tc lt ~a~%"
                              text fx fy fz
                              (case (get-option '$label_alignment)
                                 ($center "center")
                                 ($left   "left")
                                 ($right  "right"))
                              (case (get-option '$label_orientation)
                                 ($horizontal "norotate")
                                 ($vertical  "rotate"))
                              (get-option '$label_color))
         :points nil) ; labels are in the preamble, this will change in gnuplot 4.2
         ))







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
         :command (format nil "'-' ~a w vect ~a ~a size ~a, ~a lw ~a lt ~a"
                              (make-obj-title (get-option '$key))
                              (if (get-option '$head_both) "heads" "head")
                              (case (get-option '$head_type)
                                 ($filled   "filled")
                                 ($empty    "empty")
                                 ($nofilled "nofilled"))
                              (get-option '$head_length)
                              (get-option '$head_angle)
                              (get-option '$line_width)
                              (get-option '$line_type) )
         :points (list (list 4 x y dx dy))) ))








;; Object: 'explicit'
;; Usage:
;;     explicit(fcn,var,minval,maxval)
;; Options:
;;     nticks
;;     adapt_depth
;;     line_width
;;     line_type
;;     key
;;     function_style
;;     fill_type
;;     point_type
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
                                   depth 1d-5)))))

        ;; update x-y ranges if necessary
        (do ((y (cdr result) (cddr y)))
           ((null y))
          (setf yy (car y))
          (if (> yy ymax) (setf ymax yy))
          (if (< yy ymin) (setf ymin yy)) )
        (update-ranges xstart xend ymin ymax)
        (case (get-option '$function_style)
           ($lines
               (setf pltcmd (format nil "'-' ~a w l lw ~a lt ~a"
                                        (make-obj-title (get-option '$key))
                                        (get-option '$line_width)
                                        (get-option '$line_type))) )
           ($filled
               (setf pltcmd (format nil "'-' ~a w filledcurves y1=0 ~a"
                                        (make-obj-title (get-option '$key))
                                        (get-option '$fill_type)))  )    
           ($dots
               (setf pltcmd (format nil "'-' ~a w d ~a" 
                                        (make-obj-title (get-option '$key))    
                                        (get-option '$point_type)))  ))
        (make-gr-object
           :name   'explicit
           :command pltcmd
           :points  (list (cons 2 result))) )))







;; Object: 'implicit'
;; Usage:
;;     implicit(fcn,x-var,x-minval,x-maxval,y-var,y-minval,y-maxval)
;; Options:
;;     ip_grid
;;     ip_grid_in
;;     line_width
;;     line_type
;;     key
;;     filled_function
;;     fill_type
;;     point_type
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
    (case (get-option '$function_style)
       ($lines
           (setf pltcmd (format nil "'-' ~a w l lw ~a lt ~a"
                                    (make-obj-title (get-option '$key))
                                    (get-option '$line_width)
                                    (get-option '$line_type))) )
       ($filled
           (setf pltcmd (format nil "'-' ~a w filledcurves y1=0 ~a"
                                    (make-obj-title (get-option '$key))
                                    (get-option '$fill_type)))  )    
       ($dots
           (setf pltcmd (format nil "'-' ~a w d ~a" 
                                    (make-obj-title (get-option '$key))    
                                    (get-option '$point_type)))  ))
    (make-gr-object
       :name   'implicit
       :command pltcmd
       :points  (list (cons 2 pts))) ))







;; Object: 'explicit3d'
;; Usage:
;;     explicit(fcn,var1,minval1,maxval1,var2,minval2,maxval2)
;; Options:
;;     xu_grid
;;     yv_grid
;;     line_type
;;     key
;; Note: implements a clon of draw3d (plot.lisp) with some
;;       mutations to fit the draw environment.
;;       Read source in plot.lisp for more information
(defun explicit3d (fcn var1 minval1 maxval1 var2 minval2 maxval2)
  (let* ((xu_grid (gethash '$xu_grid  *gr-options*))
         (yv_grid (gethash '$yv_grid  *gr-options*))
         (epsx (convert-to-float (/ (- maxval1 minval1) xu_grid)))
         (epsy (convert-to-float (/ (- maxval2 minval2) yv_grid)))
         (x 0.0)
         (y 0.0)
         (zmin 1.75555970201398d+305)
         (zmax -1.75555970201398d+305)
         (nx (+ xu_grid 1))
         (ny (+ yv_grid 1))
         (result nil)
         ($numer t) z)
    (setq fcn (coerce-float-fun fcn `((mlist),var1 ,var2)))
    (loop for j below ny
           initially (setq y minval2)
           do (setq x minval1)
           (loop for i below nx
                  do
                  (setf z (funcall fcn x y))
                  (if (> z zmax) (setf zmax z))
                  (if (< z zmin) (setf zmin z))
                  (setf result (append result (list x y z)))
                  (setq x (+ x epsx))
                  )
           (setq y (+ y epsy)))
    (update-ranges minval1 maxval1 minval2 maxval2 zmin zmax)
    (make-gr-object
       :name   'explicit
       :command (format nil "'-' ~a w l lt ~a"
                            (make-obj-title (get-option '$key))
                            (get-option '$line_type))
       :points  (list (append `(3 ,nx) result)))   ))









;; Object: 'parametric'
;; Usage:
;;     parametric(xfun,yfun,par,parmin,parmax)
;; Options:
;;     nticks
;;     line_width
;;     line_type
;;     key
;;     function_style
;;     point_type
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
       :command (case (get-option '$function_style)
                   ('$dots (format nil "'-' ~a w d ~a"
                                       (make-obj-title (get-option '$key)) 
                                       (get-option '$point_type)) )
                   (otherwise ; filled option not yet implemented
                           (format nil "'-' ~a w l lw ~a lt ~a"
                                       (make-obj-title (get-option '$key)) 
                                       (get-option '$line_width)
                                       (get-option '$line_type)) ) )
       :points (list (cons 2 result))) ; 2 => how many numbers are sent together to gnuplot
       ) )








;; Object: 'polar'
;; Usage:
;;     polar(radius,ang,minang,maxang)
;; Options:
;;     nticks
;;     line_width
;;     line_type
;;     key
;;     function_style
;;     point_type
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
       :command (format nil "'-' ~a w l lw ~a lt ~a"
                            (make-obj-title (get-option '$key))
                            (get-option '$line_width)
                            (get-option '$line_type))
       :points (list (append '(3 0) result))) ; 3 => how many numbers are sent together to gnuplot
                                              ; 0 => no blank lines
        ) )








;; Object: 'parametric_surface'
;; Usage:
;;     parametric_surface(xfun,yfun,zfun,par1,par1min,par1max,par2,par2min,par2max)
;; Options:
;;     xu_grid
;;     yv_grid
;;     line_type
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
       :command (format nil "'-' ~a w l lt ~a"
                            (make-obj-title (get-option '$key))
                            (get-option '$line_type))
       :points (list (append `(3 ,nu) result))) ; 3 => how many numbers are sent together to gnuplot
                                               ; nu => blank line every nu lines
       ) )









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
                                     ($points     (apply #'points (rest x)))
                                     ($polygon    (apply #'polygon (rest x)))
                                     ($ellipse    (apply #'ellipse (rest x)))
                                     ($rectangle  (apply #'rectangle (rest x)))
                                     ($explicit   (apply #'explicit (rest x)))
				     ($implicit   (apply #'implicit (rest x)))
                                     ($parametric (apply #'parametric (rest x)))
                                     ($vector     (apply #'vect (rest x)))
                                     ($label      (apply #'label (rest x)))
                                     ($polar      (apply #'polar (rest x)))
                                     (otherwise (merror "Graphical 2d object ~M is not recognized" x)))))))))
      ; save in plotcmd the plot command to be sent to gnuplot
      (setf plotcmd
         (concatenate 'string
            ; this let statement is to prevent error messages in gnuplot when
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
            (if (get-option '$surface_hide)
               (format nil "set hidden3d~%"))
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
        dest is1stobj biglist ncols nrows width height)
    (dolist (x args)
      (cond ((equal ($op x) '&=)
              (case ($lhs x)
                ($terminal  (update-gr-option '$terminal ($rhs x)))
                ($columns   (update-gr-option '$columns ($rhs x)))
                (otherwise (merror "Unknown global option ~M " ($lhs x)))  ))
            ((equal (caar x) '$gr3d)
              (setf scenes (append scenes (list (funcall #'make-scene-3d (rest x))))))
            ((equal (caar x) '$gr2d)
              (setf scenes (append scenes (list (funcall #'make-scene-2d (rest x))))))
            (t
              (merror "draw: item ~M is not recognized" x)))   )

    (cond ((null $draw_pipes)   ; no pipes
             (setf dest (open (plot-temp-file "maxout.gnuplot")
                              :direction :output :if-exists :supersede)))
          (t ; if pipes, start gnuplot process
             (setf $gnuplot_command $draw_command)
             (check-gnuplot-process)
             ($gnuplot_reset)
             (setf dest *gnuplot-stream*)))

    ; write global options
    (case (gethash '$terminal *gr-options*)
      ($png (format dest "set terminal png size ~a, ~a~%set out '~a.png'~%"
                           (get-option '$pic_width)
                           (get-option '$pic_height)
                           (get-option '$file_name)) )
      ($eps (format dest "set terminal postscript eps~%set out '~a.eps'~%"
                           (get-option '$file_name)))
      ($eps_color (format dest "set terminal postscript eps color~%set out '~a.eps'~%"
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
    (dolist (scn scenes)
      ; write size and origin
      (format dest "set size ~a, ~a~%" width height)
      (format dest "set origin ~a, ~a~%" (* width (mod counter ncols))
                                       (* height (- nrows 1.0 (floor (/ counter ncols)))))
      (setf is1stobj t
            biglist nil)
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
                  (setf pltcmd (concatenate 'string pltcmd (gr-object-command obj)))
                  (setf biglist (append biglist (gr-object-points obj))))
               (t ; if it's a label, write directly to dest, since in gnuplot 4.0
                  ; labels are in the preamble; this will be changed in gnuplot 4.2
                  (format dest "~a" (gr-object-command obj)))))

      ; write sets of points
      (format dest "~a" pltcmd)
      (format dest "~%")
      (dolist (blis biglist 'done)
         ; the first element of blis indicates how many numbers are
         ; sent together to gnuplot
         (case (car blis)
             (2 (loop for (v w) on (cdr blis) by #'cddr do    ; 2d points
		      (if (and (numberp v) (numberp w))
                              (format dest "~a ~a ~%" v w)
			      (format dest "~%"))) )
             (3 (let ((l 0)                                   ; 3d points
                      (m (cadr blis)))
                   (cond
                      ((= m 0)     ; 3d points without blank lines
                         (loop for (v w y) on (cddr blis) by #'cdddr do
                                    (format dest "~a ~a ~a ~%" v w y)) )
                      (t          ; 3d points with blank lines every m lines
                         (loop for (v w y) on (cddr blis) by #'cdddr do
                                    (when (eql l m)
                                       (format dest "~%")
                                       (setf l 0) )
                                    (format dest "~a ~a ~a ~%" v w y)
                                    (incf l))) )))
             (4 (loop for (v w y z) on (cdr blis) by #'cddddr do     ; for vectors (x,y,dx,xy)
                              (format dest "~a ~a ~a ~a~%" v w y z)  ))    )
         (format dest "e~%")  )
      (setf counter (1+ counter))
      (setf scenes-list (cons (reverse scene-short-description) scenes-list))     )    ; end of dolist

    (cond ((> (length scenes) 1)
             (format dest "unset multiplot~%"))
          ; now, if gnuplot is running in interactive mode
          ; and we want to save the coordinates in a file,
          ; print them when hitting the x key after clicking the mouse button
          ((and (not (string= (gethash '$xy_file *gr-options*) ""))
                (or (string= *autoconf-win32* "true") $draw_pipes))
             (format dest "set print \"~a\" append~%bind x \"print MOUSE_X,MOUSE_Y\"~%"
                          (gethash '$xy_file *gr-options*))))

    ; get the figure
    (cond ((null $draw_pipes)   ; if not pipes, close the file & call gnuplot
         (close dest)
         ($system (if (equal (gethash '$terminal *gr-options*) '$screen)
                          (format nil "~a ~a"
                                      $draw_command
                                      (format nil $gnuplot_view_args (plot-temp-file "maxout.gnuplot")))
                          (format nil "~a \"~a\"" $draw_command (plot-temp-file "maxout.gnuplot")))) )
         (t (force-output dest)) )

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


