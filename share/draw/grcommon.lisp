;;;                 COPYRIGHT NOTICE
;;;  
;;;  Copyright (C) 2007-2017 Mario Rodriguez Riotorto
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
;;; http://tecnostats.net/Maxima/gnuplot
;;; and 
;;; http://tecnostats.net/Maxima/vtk
;;; for examples

;;; Some portions of this package were written by 
;;; Andrej Vodopivec and Joan Pau Beltran.
;;; Some other people has also helped with bug reports,
;;; comments, complains and feature requests.
;;; Thanks to everybody!!

;;; For questions, suggestions, bugs and the like, feel free
;;; to contact me at

;;; riotorto @@@ yahoo DOT com


;; Possible draw renderers:
;;      gnuplot_pipes (default)
;;      gnuplot
;;      vtk or vtk6
;;      vtk7
(defvar $draw_renderer '$gnuplot_pipes)

(defvar $draw_use_pngcairo nil "If true, use pngcairo terminal when png is requested.")


;; Stores user defaults
(defvar *user-gr-default-options* nil)
(defun $set_draw_defaults (&rest opts)
   (setf *user-gr-default-options* opts)
   (cons '(mlist) opts))
(defun user-defaults ()
   (dolist (x *user-gr-default-options*)
      (if (equal ($op x) "=")
         (update-gr-option ($lhs x) ($rhs x))
         (merror "draw: item ~M is not recognized as an option assignment" x))))

;; Stores user defined allocations of scenes
(defvar *allocations* nil)


;; Sets default values of graphics options
(defvar *gr-options* (make-hash-table))
(defun ini-gr-options ()
  (setf
      ; global options to control general aspects of graphics
      (gethash '$allocation *gr-options*)       nil      ; or user-defined allocation
      (gethash '$proportional_axes *gr-options*) '$none  ; three possible options: none, xy, xyz
      (gethash '$xrange *gr-options*)           nil      ; nil => automatic computation
      (gethash '$xrange_secondary *gr-options*) nil      ; nil => automatic computation
      (gethash '$yrange *gr-options*)           nil      ; nil => automatic computation
      (gethash '$yrange_secondary *gr-options*) nil      ; nil => automatic computation
      (gethash '$zrange *gr-options*)           nil      ; nil => automatic computation
      (gethash '$cbrange *gr-options*)          nil      ; nil => automatic computation
      (gethash '$logx *gr-options*)             nil
      (gethash '$logx_secondary *gr-options*)   nil
      (gethash '$logy *gr-options*)             nil
      (gethash '$logy_secondary *gr-options*)   nil
      (gethash '$logz *gr-options*)             nil
      (gethash '$logcb *gr-options*)            nil
      (gethash '$title *gr-options*)            ""
      (gethash '$view *gr-options*)             '(60 30) ; in [0,180] x [0,360]
      (gethash '$xy_file *gr-options*)          ""
      (gethash '$user_preamble *gr-options*)    ""
      (gethash '$xyplane *gr-options*)          nil
      (gethash '$font *gr-options*)             "";
      (gethash '$font_size *gr-options*)        10;
      (gethash '$key_pos *gr-options*)          nil

      ; colors are specified by name
      (gethash '$background_color *gr-options*) "#ffffff"
      (gethash '$color *gr-options*)            "#0000ff" ; for lines, points, borders and labels
      (gethash '$fill_color *gr-options*)       "#ff0000" ; for filled regions
      (gethash '$fill_density *gr-options*)     0         ; in [0,1], only for object 'bars

      ; implicit plot options
      (gethash '$ip_grid *gr-options*)    '((mlist simp) 50 50)
      (gethash '$ip_grid_in *gr-options*) '((mlist simp) 5 5)
      (gethash '$x_voxel *gr-options*)       10
      (gethash '$y_voxel *gr-options*)       10
      (gethash '$z_voxel *gr-options*)       10

      ; tics
      (gethash '$grid *gr-options*)            (list 0 0)
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

      ; axes
      (gethash '$axis_bottom *gr-options*) t
      (gethash '$axis_left *gr-options*)   t
      (gethash '$axis_top *gr-options*)    t
      (gethash '$axis_right *gr-options*)  t
      (gethash '$axis_3d *gr-options*)     t
      (gethash '$xaxis *gr-options*)       nil   ; no xaxis by default
      (gethash '$xaxis_width *gr-options*) 1
      (gethash '$xaxis_type *gr-options*)  0    ; two options: 1 (solid) and 0 (dots)
      (gethash '$xaxis_color *gr-options*) "#000000"
      (gethash '$yaxis *gr-options*)       nil  ; no yaxis by default
      (gethash '$yaxis_width *gr-options*) 1
      (gethash '$yaxis_type *gr-options*)  0    ; two options: 1 (solid) and 0 (dots)
      (gethash '$yaxis_color *gr-options*) "#000000"
      (gethash '$zaxis *gr-options*)       nil  ; no zaxis by default
      (gethash '$zaxis_width *gr-options*) 1
      (gethash '$zaxis_type *gr-options*)  0    ; two options: 1 (solid) and 0 (dots)
      (gethash '$zaxis_color *gr-options*) "#000000"
      (gethash '$xlabel *gr-options*)      ""
      (gethash '$xlabel_secondary *gr-options*) ""
      (gethash '$ylabel *gr-options*)      ""
      (gethash '$ylabel_secondary *gr-options*) ""
      (gethash '$zlabel *gr-options*)      ""
      (gethash '$zlabel_rotate *gr-options*) '$auto

      ; point options
      (gethash '$point_size *gr-options*)    1
      (gethash '$point_type *gr-options*)    1
      (gethash '$points_joined *gr-options*) nil ; other options are: true and $impulses

      ; error bars option
      (gethash '$error_type *gr-options*)   '$y

      ; polygon  options
      (gethash '$transparent *gr-options*) nil
      (gethash '$opacity *gr-options*)     1
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
      (gethash '$draw_realpart *gr-options*)   t

      ; transformation option
      (gethash '$transform *gr-options*) '$none

      ; 3d options
      (gethash '$xu_grid *gr-options*)           50
      (gethash '$yv_grid *gr-options*)           50
      (gethash '$surface_hide *gr-options*)      nil
      (gethash '$interpolate_color *gr-options*) "depthorder"
      (gethash '$enhanced3d *gr-options*)        '$none
      (gethash '$isolines *gr-options*)          '$none
      (gethash '$wired_surface *gr-options*)     nil
      (gethash '$contour *gr-options*)           '$none ; other options are: $base, $surface, $both and $map
      (gethash '$contour_levels *gr-options*)     5     ; 1-50, [lowest_level,step,highest_level] or {e1,e2,...}
      (gethash '$isolines_levels *gr-options*)    10    ; 1-50, [lowest_level_fraction,step,highest_level_fraction],
                                                        ; or {e1_fraction,e2_fraction,...}. Only for VTK.
      (gethash '$colorbox *gr-options*)           t       ; in pm3d mode, always show colorbox
      (gethash '$palette  *gr-options*)           '$color ; '$color is a short cut for [7,5,15]
                                                          ; and '$gray is a short cut for [3,3,3].
                                                          ; See command 'show palette rgbformulae' in gnuplot.
      (gethash '$capping *gr-options*)            '((mlist simp) nil nil)
  ) )

;; Returns option value
(defun get-option (opt) (gethash opt *gr-options*))






;; update options color, fill_color, xaxis_color, yaxis_color,
;; $zaxis_color, and $background_color
;; -----------------------------------------------------------
;; defined as a color name or hexadecimal #rrggbb
(defun atom-to-downcased-string (val)
   ; also, remove spaces, minus symbols, and underscores
   (remove-if
      #'(lambda (z) (member (char-int z) '(32 45 95)))
      (string-downcase
        (string-trim 
           "\""
           (coerce (mstring val) 'string)))))

(defvar *color-table* (make-hash-table :test 'equal))
(setf (gethash "snow" *color-table*) "#FFFAFA" 
      (gethash "ghostwhite" *color-table*) "#F8F8FF" 
      (gethash "whitesmoke" *color-table*) "#F5F5F5" 
      (gethash "gainsboro" *color-table*) "#DCDCDC" 
      (gethash "floralwhite" *color-table*) "#FFFAF0" 
      (gethash "oldlace" *color-table*) "#FDF5E6" 
      (gethash "linen" *color-table*) "#FAF0E6" 
      (gethash "antiquewhite" *color-table*) "#FAEBD7" 
      (gethash "papayawhip" *color-table*) "#FFEFD5" 
      (gethash "blanchedalmond" *color-table*) "#FFEBCD" 
      (gethash "bisque" *color-table*) "#FFE4C4" 
      (gethash "peachpuff" *color-table*) "#FFDAB9" 
      (gethash "navajowhite" *color-table*) "#FFDEAD" 
      (gethash "moccasin" *color-table*) "#FFE4B5" 
      (gethash "cornsilk" *color-table*) "#FFF8DC" 
      (gethash "ivory" *color-table*) "#FFFFF0" 
      (gethash "lemonchiffon" *color-table*) "#FFFACD" 
      (gethash "seashell" *color-table*) "#FFF5EE" 
      (gethash "honeydew" *color-table*) "#F0FFF0" 
      (gethash "mintcream" *color-table*) "#F5FFFA" 
      (gethash "azure" *color-table*) "#F0FFFF" 
      (gethash "aliceblue" *color-table*) "#F0F8FF" 
      (gethash "lavender" *color-table*) "#E6E6FA" 
      (gethash "lavenderblush" *color-table*) "#FFF0F5" 
      (gethash "mistyrose" *color-table*) "#FFE4E1" 
      (gethash "white" *color-table*) "#FFFFFF" 
      (gethash "black" *color-table*) "#000000" 
      (gethash "darkslategray" *color-table*) "#2F4F4F" 
      (gethash "darkslategrey" *color-table*) "#2F4F4F" 
      (gethash "dimgray" *color-table*) "#696969" 
      (gethash "dimgrey" *color-table*) "#696969" 
      (gethash "slategray" *color-table*) "#708090" 
      (gethash "slategrey" *color-table*) "#708090" 
      (gethash "lightslategray" *color-table*) "#778899" 
      (gethash "lightslategrey" *color-table*) "#778899" 
      (gethash "gray" *color-table*) "#BEBEBE" 
      (gethash "grey" *color-table*) "#BEBEBE" 
      (gethash "lightgrey" *color-table*) "#D3D3D3" 
      (gethash "lightgray" *color-table*) "#D3D3D3" 
      (gethash "midnightblue" *color-table*) "#191970" 
      (gethash "navy" *color-table*) "#000080" 
      (gethash "navyblue" *color-table*) "#000080" 
      (gethash "cornflowerblue" *color-table*) "#6495ED" 
      (gethash "darkslateblue" *color-table*) "#483D8B" 
      (gethash "slateblue" *color-table*) "#6A5ACD" 
      (gethash "mediumslateblue" *color-table*) "#7B68EE" 
      (gethash "lightslateblue" *color-table*) "#8470FF" 
      (gethash "mediumblue" *color-table*) "#0000CD" 
      (gethash "royalblue" *color-table*) "#4169E1" 
      (gethash "blue" *color-table*) "#0000FF" 
      (gethash "dodgerblue" *color-table*) "#1E90FF" 
      (gethash "deepskyblue" *color-table*) "#00BFFF" 
      (gethash "skyblue" *color-table*) "#87CEEB" 
      (gethash "lightskyblue" *color-table*) "#87CEFA" 
      (gethash "steelblue" *color-table*) "#4682B4" 
      (gethash "lightsteelblue" *color-table*) "#B0C4DE" 
      (gethash "lightblue" *color-table*) "#ADD8E6" 
      (gethash "powderblue" *color-table*) "#B0E0E6" 
      (gethash "paleturquoise" *color-table*) "#AFEEEE" 
      (gethash "darkturquoise" *color-table*) "#00CED1" 
      (gethash "mediumturquoise" *color-table*) "#48D1CC" 
      (gethash "turquoise" *color-table*) "#40E0D0" 
      (gethash "cyan" *color-table*) "#00FFFF" 
      (gethash "lightcyan" *color-table*) "#E0FFFF" 
      (gethash "cadetblue" *color-table*) "#5F9EA0" 
      (gethash "mediumaquamarine" *color-table*) "#66CDAA" 
      (gethash "aquamarine" *color-table*) "#7FFFD4" 
      (gethash "darkgreen" *color-table*) "#006400" 
      (gethash "darkolivegreen" *color-table*) "#556B2F" 
      (gethash "darkseagreen" *color-table*) "#8FBC8F" 
      (gethash "seagreen" *color-table*) "#2E8B57" 
      (gethash "mediumseagreen" *color-table*) "#3CB371" 
      (gethash "lightseagreen" *color-table*) "#20B2AA" 
      (gethash "palegreen" *color-table*) "#98FB98" 
      (gethash "springgreen" *color-table*) "#00FF7F" 
      (gethash "lawngreen" *color-table*) "#7CFC00" 
      (gethash "green" *color-table*) "#00FF00" 
      (gethash "chartreuse" *color-table*) "#7FFF00" 
      (gethash "mediumspringgreen" *color-table*) "#00FA9A" 
      (gethash "greenyellow" *color-table*) "#ADFF2F" 
      (gethash "limegreen" *color-table*) "#32CD32" 
      (gethash "yellowgreen" *color-table*) "#9ACD32" 
      (gethash "forestgreen" *color-table*) "#228B22" 
      (gethash "olivedrab" *color-table*) "#6B8E23" 
      (gethash "darkkhaki" *color-table*) "#BDB76B" 
      (gethash "khaki" *color-table*) "#F0E68C" 
      (gethash "palegoldenrod" *color-table*) "#EEE8AA" 
      (gethash "lightgoldenrodyellow" *color-table*) "#FAFAD2" 
      (gethash "lightyellow" *color-table*) "#FFFFE0" 
      (gethash "yellow" *color-table*) "#FFFF00" 
      (gethash "gold" *color-table*) "#FFD700" 
      (gethash "lightgoldenrod" *color-table*) "#EEDD82" 
      (gethash "goldenrod" *color-table*) "#DAA520" 
      (gethash "darkgoldenrod" *color-table*) "#B8860B" 
      (gethash "rosybrown" *color-table*) "#BC8F8F" 
      (gethash "indianred" *color-table*) "#CD5C5C" 
      (gethash "saddlebrown" *color-table*) "#8B4513" 
      (gethash "sienna" *color-table*) "#A0522D" 
      (gethash "peru" *color-table*) "#CD853F" 
      (gethash "burlywood" *color-table*) "#DEB887" 
      (gethash "beige" *color-table*) "#F5F5DC" 
      (gethash "wheat" *color-table*) "#F5DEB3" 
      (gethash "sandybrown" *color-table*) "#F4A460" 
      (gethash "tan" *color-table*) "#D2B48C" 
      (gethash "chocolate" *color-table*) "#D2691E" 
      (gethash "firebrick" *color-table*) "#B22222" 
      (gethash "brown" *color-table*) "#A52A2A" 
      (gethash "darksalmon" *color-table*) "#E9967A" 
      (gethash "salmon" *color-table*) "#FA8072" 
      (gethash "lightsalmon" *color-table*) "#FFA07A" 
      (gethash "orange" *color-table*) "#FFA500" 
      (gethash "darkorange" *color-table*) "#FF8C00" 
      (gethash "coral" *color-table*) "#FF7F50" 
      (gethash "lightcoral" *color-table*) "#F08080" 
      (gethash "tomato" *color-table*) "#FF6347" 
      (gethash "orangered" *color-table*) "#FF4500" 
      (gethash "red" *color-table*) "#FF0000" 
      (gethash "hotpink" *color-table*) "#FF69B4" 
      (gethash "deeppink" *color-table*) "#FF1493" 
      (gethash "pink" *color-table*) "#FFC0CB" 
      (gethash "lightpink" *color-table*) "#FFB6C1" 
      (gethash "palevioletred" *color-table*) "#DB7093" 
      (gethash "maroon" *color-table*) "#B03060" 
      (gethash "mediumvioletred" *color-table*) "#C71585" 
      (gethash "violetred" *color-table*) "#D02090" 
      (gethash "magenta" *color-table*) "#FF00FF" 
      (gethash "violet" *color-table*) "#EE82EE" 
      (gethash "plum" *color-table*) "#DDA0DD" 
      (gethash "orchid" *color-table*) "#DA70D6" 
      (gethash "mediumorchid" *color-table*) "#BA55D3" 
      (gethash "darkorchid" *color-table*) "#9932CC" 
      (gethash "darkviolet" *color-table*) "#9400D3" 
      (gethash "blueviolet" *color-table*) "#8A2BE2" 
      (gethash "purple" *color-table*) "#A020F0" 
      (gethash "mediumpurple" *color-table*) "#9370DB" 
      (gethash "thistle" *color-table*) "#D8BFD8" 
      (gethash "snow1" *color-table*) "#FFFAFA" 
      (gethash "snow2" *color-table*) "#EEE9E9" 
      (gethash "snow3" *color-table*) "#CDC9C9" 
      (gethash "snow4" *color-table*) "#8B8989" 
      (gethash "seashell1" *color-table*) "#FFF5EE" 
      (gethash "seashell2" *color-table*) "#EEE5DE" 
      (gethash "seashell3" *color-table*) "#CDC5BF" 
      (gethash "seashell4" *color-table*) "#8B8682" 
      (gethash "antiquewhite1" *color-table*) "#FFEFDB" 
      (gethash "antiquewhite2" *color-table*) "#EEDFCC" 
      (gethash "antiquewhite3" *color-table*) "#CDC0B0" 
      (gethash "antiquewhite4" *color-table*) "#8B8378" 
      (gethash "bisque1" *color-table*) "#FFE4C4" 
      (gethash "bisque2" *color-table*) "#EED5B7" 
      (gethash "bisque3" *color-table*) "#CDB79E" 
      (gethash "bisque4" *color-table*) "#8B7D6B" 
      (gethash "peachpuff1" *color-table*) "#FFDAB9" 
      (gethash "peachpuff2" *color-table*) "#EECBAD" 
      (gethash "peachpuff3" *color-table*) "#CDAF95" 
      (gethash "peachpuff4" *color-table*) "#8B7765" 
      (gethash "navajowhite1" *color-table*) "#FFDEAD" 
      (gethash "navajowhite2" *color-table*) "#EECFA1" 
      (gethash "navajowhite3" *color-table*) "#CDB38B" 
      (gethash "navajowhite4" *color-table*) "#8B795E" 
      (gethash "lemonchiffon1" *color-table*) "#FFFACD" 
      (gethash "lemonchiffon2" *color-table*) "#EEE9BF" 
      (gethash "lemonchiffon3" *color-table*) "#CDC9A5" 
      (gethash "lemonchiffon4" *color-table*) "#8B8970" 
      (gethash "cornsilk1" *color-table*) "#FFF8DC" 
      (gethash "cornsilk2" *color-table*) "#EEE8CD" 
      (gethash "cornsilk3" *color-table*) "#CDC8B1" 
      (gethash "cornsilk4" *color-table*) "#8B8878" 
      (gethash "ivory1" *color-table*) "#FFFFF0" 
      (gethash "ivory2" *color-table*) "#EEEEE0" 
      (gethash "ivory3" *color-table*) "#CDCDC1" 
      (gethash "ivory4" *color-table*) "#8B8B83" 
      (gethash "honeydew1" *color-table*) "#F0FFF0" 
      (gethash "honeydew2" *color-table*) "#E0EEE0" 
      (gethash "honeydew3" *color-table*) "#C1CDC1" 
      (gethash "honeydew4" *color-table*) "#838B83" 
      (gethash "lavenderblush1" *color-table*) "#FFF0F5" 
      (gethash "lavenderblush2" *color-table*) "#EEE0E5" 
      (gethash "lavenderblush3" *color-table*) "#CDC1C5" 
      (gethash "lavenderblush4" *color-table*) "#8B8386" 
      (gethash "mistyrose1" *color-table*) "#FFE4E1" 
      (gethash "mistyrose2" *color-table*) "#EED5D2" 
      (gethash "mistyrose3" *color-table*) "#CDB7B5" 
      (gethash "mistyrose4" *color-table*) "#8B7D7B" 
      (gethash "azure1" *color-table*) "#F0FFFF" 
      (gethash "azure2" *color-table*) "#E0EEEE" 
      (gethash "azure3" *color-table*) "#C1CDCD" 
      (gethash "azure4" *color-table*) "#838B8B" 
      (gethash "slateblue1" *color-table*) "#836FFF" 
      (gethash "slateblue2" *color-table*) "#7A67EE" 
      (gethash "slateblue3" *color-table*) "#6959CD" 
      (gethash "slateblue4" *color-table*) "#473C8B" 
      (gethash "royalblue1" *color-table*) "#4876FF" 
      (gethash "royalblue2" *color-table*) "#436EEE" 
      (gethash "royalblue3" *color-table*) "#3A5FCD" 
      (gethash "royalblue4" *color-table*) "#27408B" 
      (gethash "blue1" *color-table*) "#0000FF" 
      (gethash "blue2" *color-table*) "#0000EE" 
      (gethash "blue3" *color-table*) "#0000CD" 
      (gethash "blue4" *color-table*) "#00008B" 
      (gethash "dodgerblue1" *color-table*) "#1E90FF" 
      (gethash "dodgerblue2" *color-table*) "#1C86EE" 
      (gethash "dodgerblue3" *color-table*) "#1874CD" 
      (gethash "dodgerblue4" *color-table*) "#104E8B" 
      (gethash "steelblue1" *color-table*) "#63B8FF" 
      (gethash "steelblue2" *color-table*) "#5CACEE" 
      (gethash "steelblue3" *color-table*) "#4F94CD" 
      (gethash "steelblue4" *color-table*) "#36648B" 
      (gethash "deepskyblue1" *color-table*) "#00BFFF" 
      (gethash "deepskyblue2" *color-table*) "#00B2EE" 
      (gethash "deepskyblue3" *color-table*) "#009ACD" 
      (gethash "deepskyblue4" *color-table*) "#00688B" 
      (gethash "skyblue1" *color-table*) "#87CEFF" 
      (gethash "skyblue2" *color-table*) "#7EC0EE" 
      (gethash "skyblue3" *color-table*) "#6CA6CD" 
      (gethash "skyblue4" *color-table*) "#4A708B" 
      (gethash "lightskyblue1" *color-table*) "#B0E2FF" 
      (gethash "lightskyblue2" *color-table*) "#A4D3EE" 
      (gethash "lightskyblue3" *color-table*) "#8DB6CD" 
      (gethash "lightskyblue4" *color-table*) "#607B8B" 
      (gethash "slategray1" *color-table*) "#C6E2FF" 
      (gethash "slategray2" *color-table*) "#B9D3EE" 
      (gethash "slategray3" *color-table*) "#9FB6CD" 
      (gethash "slategray4" *color-table*) "#6C7B8B" 
      (gethash "lightsteelblue1" *color-table*) "#CAE1FF" 
      (gethash "lightsteelblue2" *color-table*) "#BCD2EE" 
      (gethash "lightsteelblue3" *color-table*) "#A2B5CD" 
      (gethash "lightsteelblue4" *color-table*) "#6E7B8B" 
      (gethash "lightblue1" *color-table*) "#BFEFFF" 
      (gethash "lightblue2" *color-table*) "#B2DFEE" 
      (gethash "lightblue3" *color-table*) "#9AC0CD" 
      (gethash "lightblue4" *color-table*) "#68838B" 
      (gethash "lightcyan1" *color-table*) "#E0FFFF" 
      (gethash "lightcyan2" *color-table*) "#D1EEEE" 
      (gethash "lightcyan3" *color-table*) "#B4CDCD" 
      (gethash "lightcyan4" *color-table*) "#7A8B8B" 
      (gethash "paleturquoise1" *color-table*) "#BBFFFF" 
      (gethash "paleturquoise2" *color-table*) "#AEEEEE" 
      (gethash "paleturquoise3" *color-table*) "#96CDCD" 
      (gethash "paleturquoise4" *color-table*) "#668B8B" 
      (gethash "cadetblue1" *color-table*) "#98F5FF" 
      (gethash "cadetblue2" *color-table*) "#8EE5EE" 
      (gethash "cadetblue3" *color-table*) "#7AC5CD" 
      (gethash "cadetblue4" *color-table*) "#53868B" 
      (gethash "turquoise1" *color-table*) "#00F5FF" 
      (gethash "turquoise2" *color-table*) "#00E5EE" 
      (gethash "turquoise3" *color-table*) "#00C5CD" 
      (gethash "turquoise4" *color-table*) "#00868B" 
      (gethash "cyan1" *color-table*) "#00FFFF" 
      (gethash "cyan2" *color-table*) "#00EEEE" 
      (gethash "cyan3" *color-table*) "#00CDCD" 
      (gethash "cyan4" *color-table*) "#008B8B" 
      (gethash "darkslategray1" *color-table*) "#97FFFF" 
      (gethash "darkslategray2" *color-table*) "#8DEEEE" 
      (gethash "darkslategray3" *color-table*) "#79CDCD" 
      (gethash "darkslategray4" *color-table*) "#528B8B" 
      (gethash "aquamarine1" *color-table*) "#7FFFD4" 
      (gethash "aquamarine2" *color-table*) "#76EEC6" 
      (gethash "aquamarine3" *color-table*) "#66CDAA" 
      (gethash "aquamarine4" *color-table*) "#458B74" 
      (gethash "darkseagreen1" *color-table*) "#C1FFC1" 
      (gethash "darkseagreen2" *color-table*) "#B4EEB4" 
      (gethash "darkseagreen3" *color-table*) "#9BCD9B" 
      (gethash "darkseagreen4" *color-table*) "#698B69" 
      (gethash "seagreen1" *color-table*) "#54FF9F" 
      (gethash "seagreen2" *color-table*) "#4EEE94" 
      (gethash "seagreen3" *color-table*) "#43CD80" 
      (gethash "seagreen4" *color-table*) "#2E8B57" 
      (gethash "palegreen1" *color-table*) "#9AFF9A" 
      (gethash "palegreen2" *color-table*) "#90EE90" 
      (gethash "palegreen3" *color-table*) "#7CCD7C" 
      (gethash "palegreen4" *color-table*) "#548B54" 
      (gethash "springgreen1" *color-table*) "#00FF7F" 
      (gethash "springgreen2" *color-table*) "#00EE76" 
      (gethash "springgreen3" *color-table*) "#00CD66" 
      (gethash "springgreen4" *color-table*) "#008B45" 
      (gethash "green1" *color-table*) "#00FF00" 
      (gethash "green2" *color-table*) "#00EE00" 
      (gethash "green3" *color-table*) "#00CD00" 
      (gethash "green4" *color-table*) "#008B00" 
      (gethash "chartreuse1" *color-table*) "#7FFF00" 
      (gethash "chartreuse2" *color-table*) "#76EE00" 
      (gethash "chartreuse3" *color-table*) "#66CD00" 
      (gethash "chartreuse4" *color-table*) "#458B00" 
      (gethash "olivedrab1" *color-table*) "#C0FF3E" 
      (gethash "olivedrab2" *color-table*) "#B3EE3A" 
      (gethash "olivedrab3" *color-table*) "#9ACD32" 
      (gethash "olivedrab4" *color-table*) "#698B22" 
      (gethash "darkolivegreen1" *color-table*) "#CAFF70" 
      (gethash "darkolivegreen2" *color-table*) "#BCEE68" 
      (gethash "darkolivegreen3" *color-table*) "#A2CD5A" 
      (gethash "darkolivegreen4" *color-table*) "#6E8B3D" 
      (gethash "khaki1" *color-table*) "#FFF68F" 
      (gethash "khaki2" *color-table*) "#EEE685" 
      (gethash "khaki3" *color-table*) "#CDC673" 
      (gethash "khaki4" *color-table*) "#8B864E" 
      (gethash "lightgoldenrod1" *color-table*) "#FFEC8B" 
      (gethash "lightgoldenrod2" *color-table*) "#EEDC82" 
      (gethash "lightgoldenrod3" *color-table*) "#CDBE70" 
      (gethash "lightgoldenrod4" *color-table*) "#8B814C" 
      (gethash "lightyellow1" *color-table*) "#FFFFE0" 
      (gethash "lightyellow2" *color-table*) "#EEEED1" 
      (gethash "lightyellow3" *color-table*) "#CDCDB4" 
      (gethash "lightyellow4" *color-table*) "#8B8B7A" 
      (gethash "yellow1" *color-table*) "#FFFF00" 
      (gethash "yellow2" *color-table*) "#EEEE00" 
      (gethash "yellow3" *color-table*) "#CDCD00" 
      (gethash "yellow4" *color-table*) "#8B8B00" 
      (gethash "gold1" *color-table*) "#FFD700" 
      (gethash "gold2" *color-table*) "#EEC900" 
      (gethash "gold3" *color-table*) "#CDAD00" 
      (gethash "gold4" *color-table*) "#8B7500" 
      (gethash "goldenrod1" *color-table*) "#FFC125" 
      (gethash "goldenrod2" *color-table*) "#EEB422" 
      (gethash "goldenrod3" *color-table*) "#CD9B1D" 
      (gethash "goldenrod4" *color-table*) "#8B6914" 
      (gethash "darkgoldenrod1" *color-table*) "#FFB90F" 
      (gethash "darkgoldenrod2" *color-table*) "#EEAD0E" 
      (gethash "darkgoldenrod3" *color-table*) "#CD950C" 
      (gethash "darkgoldenrod4" *color-table*) "#8B6508" 
      (gethash "rosybrown1" *color-table*) "#FFC1C1" 
      (gethash "rosybrown2" *color-table*) "#EEB4B4" 
      (gethash "rosybrown3" *color-table*) "#CD9B9B" 
      (gethash "rosybrown4" *color-table*) "#8B6969" 
      (gethash "indianred1" *color-table*) "#FF6A6A" 
      (gethash "indianred2" *color-table*) "#EE6363" 
      (gethash "indianred3" *color-table*) "#CD5555" 
      (gethash "indianred4" *color-table*) "#8B3A3A" 
      (gethash "sienna1" *color-table*) "#FF8247" 
      (gethash "sienna2" *color-table*) "#EE7942" 
      (gethash "sienna3" *color-table*) "#CD6839" 
      (gethash "sienna4" *color-table*) "#8B4726" 
      (gethash "burlywood1" *color-table*) "#FFD39B" 
      (gethash "burlywood2" *color-table*) "#EEC591" 
      (gethash "burlywood3" *color-table*) "#CDAA7D" 
      (gethash "burlywood4" *color-table*) "#8B7355" 
      (gethash "wheat1" *color-table*) "#FFE7BA" 
      (gethash "wheat2" *color-table*) "#EED8AE" 
      (gethash "wheat3" *color-table*) "#CDBA96" 
      (gethash "wheat4" *color-table*) "#8B7E66" 
      (gethash "tan1" *color-table*) "#FFA54F" 
      (gethash "tan2" *color-table*) "#EE9A49" 
      (gethash "tan3" *color-table*) "#CD853F" 
      (gethash "tan4" *color-table*) "#8B5A2B" 
      (gethash "chocolate1" *color-table*) "#FF7F24" 
      (gethash "chocolate2" *color-table*) "#EE7621" 
      (gethash "chocolate3" *color-table*) "#CD661D" 
      (gethash "chocolate4" *color-table*) "#8B4513" 
      (gethash "firebrick1" *color-table*) "#FF3030" 
      (gethash "firebrick2" *color-table*) "#EE2C2C" 
      (gethash "firebrick3" *color-table*) "#CD2626" 
      (gethash "firebrick4" *color-table*) "#8B1A1A" 
      (gethash "brown1" *color-table*) "#FF4040" 
      (gethash "brown2" *color-table*) "#EE3B3B" 
      (gethash "brown3" *color-table*) "#CD3333" 
      (gethash "brown4" *color-table*) "#8B2323" 
      (gethash "salmon1" *color-table*) "#FF8C69" 
      (gethash "salmon2" *color-table*) "#EE8262" 
      (gethash "salmon3" *color-table*) "#CD7054" 
      (gethash "salmon4" *color-table*) "#8B4C39" 
      (gethash "lightsalmon1" *color-table*) "#FFA07A" 
      (gethash "lightsalmon2" *color-table*) "#EE9572" 
      (gethash "lightsalmon3" *color-table*) "#CD8162" 
      (gethash "lightsalmon4" *color-table*) "#8B5742" 
      (gethash "orange1" *color-table*) "#FFA500" 
      (gethash "orange2" *color-table*) "#EE9A00" 
      (gethash "orange3" *color-table*) "#CD8500" 
      (gethash "orange4" *color-table*) "#8B5A00" 
      (gethash "darkorange1" *color-table*) "#FF7F00" 
      (gethash "darkorange2" *color-table*) "#EE7600" 
      (gethash "darkorange3" *color-table*) "#CD6600" 
      (gethash "darkorange4" *color-table*) "#8B4500" 
      (gethash "coral1" *color-table*) "#FF7256" 
      (gethash "coral2" *color-table*) "#EE6A50" 
      (gethash "coral3" *color-table*) "#CD5B45" 
      (gethash "coral4" *color-table*) "#8B3E2F" 
      (gethash "tomato1" *color-table*) "#FF6347" 
      (gethash "tomato2" *color-table*) "#EE5C42" 
      (gethash "tomato3" *color-table*) "#CD4F39" 
      (gethash "tomato4" *color-table*) "#8B3626" 
      (gethash "orangered1" *color-table*) "#FF4500" 
      (gethash "orangered2" *color-table*) "#EE4000" 
      (gethash "orangered3" *color-table*) "#CD3700" 
      (gethash "orangered4" *color-table*) "#8B2500" 
      (gethash "red1" *color-table*) "#FF0000" 
      (gethash "red2" *color-table*) "#EE0000" 
      (gethash "red3" *color-table*) "#CD0000" 
      (gethash "red4" *color-table*) "#8B0000" 
      (gethash "debianred" *color-table*) "#D70751" 
      (gethash "deeppink1" *color-table*) "#FF1493" 
      (gethash "deeppink2" *color-table*) "#EE1289" 
      (gethash "deeppink3" *color-table*) "#CD1076" 
      (gethash "deeppink4" *color-table*) "#8B0A50" 
      (gethash "hotpink1" *color-table*) "#FF6EB4" 
      (gethash "hotpink2" *color-table*) "#EE6AA7" 
      (gethash "hotpink3" *color-table*) "#CD6090" 
      (gethash "hotpink4" *color-table*) "#8B3A62" 
      (gethash "pink1" *color-table*) "#FFB5C5" 
      (gethash "pink2" *color-table*) "#EEA9B8" 
      (gethash "pink3" *color-table*) "#CD919E" 
      (gethash "pink4" *color-table*) "#8B636C" 
      (gethash "lightpink1" *color-table*) "#FFAEB9" 
      (gethash "lightpink2" *color-table*) "#EEA2AD" 
      (gethash "lightpink3" *color-table*) "#CD8C95" 
      (gethash "lightpink4" *color-table*) "#8B5F65" 
      (gethash "palevioletred1" *color-table*) "#FF82AB" 
      (gethash "palevioletred2" *color-table*) "#EE799F" 
      (gethash "palevioletred3" *color-table*) "#CD6889" 
      (gethash "palevioletred4" *color-table*) "#8B475D" 
      (gethash "maroon1" *color-table*) "#FF34B3" 
      (gethash "maroon2" *color-table*) "#EE30A7" 
      (gethash "maroon3" *color-table*) "#CD2990" 
      (gethash "maroon4" *color-table*) "#8B1C62" 
      (gethash "violetred1" *color-table*) "#FF3E96" 
      (gethash "violetred2" *color-table*) "#EE3A8C" 
      (gethash "violetred3" *color-table*) "#CD3278" 
      (gethash "violetred4" *color-table*) "#8B2252" 
      (gethash "magenta1" *color-table*) "#FF00FF" 
      (gethash "magenta2" *color-table*) "#EE00EE" 
      (gethash "magenta3" *color-table*) "#CD00CD" 
      (gethash "magenta4" *color-table*) "#8B008B" 
      (gethash "orchid1" *color-table*) "#FF83FA" 
      (gethash "orchid2" *color-table*) "#EE7AE9" 
      (gethash "orchid3" *color-table*) "#CD69C9" 
      (gethash "orchid4" *color-table*) "#8B4789" 
      (gethash "plum1" *color-table*) "#FFBBFF" 
      (gethash "plum2" *color-table*) "#EEAEEE" 
      (gethash "plum3" *color-table*) "#CD96CD" 
      (gethash "plum4" *color-table*) "#8B668B" 
      (gethash "mediumorchid1" *color-table*) "#E066FF" 
      (gethash "mediumorchid2" *color-table*) "#D15FEE" 
      (gethash "mediumorchid3" *color-table*) "#B452CD" 
      (gethash "mediumorchid4" *color-table*) "#7A378B" 
      (gethash "darkorchid1" *color-table*) "#BF3EFF" 
      (gethash "darkorchid2" *color-table*) "#B23AEE" 
      (gethash "darkorchid3" *color-table*) "#9A32CD" 
      (gethash "darkorchid4" *color-table*) "#68228B" 
      (gethash "purple1" *color-table*) "#9B30FF" 
      (gethash "purple2" *color-table*) "#912CEE" 
      (gethash "purple3" *color-table*) "#7D26CD" 
      (gethash "purple4" *color-table*) "#551A8B" 
      (gethash "mediumpurple1" *color-table*) "#AB82FF" 
      (gethash "mediumpurple2" *color-table*) "#9F79EE" 
      (gethash "mediumpurple3" *color-table*) "#8968CD" 
      (gethash "mediumpurple4" *color-table*) "#5D478B" 
      (gethash "thistle1" *color-table*) "#FFE1FF" 
      (gethash "thistle2" *color-table*) "#EED2EE" 
      (gethash "thistle3" *color-table*) "#CDB5CD" 
      (gethash "thistle4" *color-table*) "#8B7B8B" 
      (gethash "gray0" *color-table*) "#000000" 
      (gethash "grey0" *color-table*) "#000000" 
      (gethash "gray1" *color-table*) "#030303" 
      (gethash "grey1" *color-table*) "#030303" 
      (gethash "gray2" *color-table*) "#050505" 
      (gethash "grey2" *color-table*) "#050505" 
      (gethash "gray3" *color-table*) "#080808" 
      (gethash "grey3" *color-table*) "#080808" 
      (gethash "gray4" *color-table*) "#0A0A0A" 
      (gethash "grey4" *color-table*) "#0A0A0A" 
      (gethash "gray5" *color-table*) "#0D0D0D" 
      (gethash "grey5" *color-table*) "#0D0D0D" 
      (gethash "gray6" *color-table*) "#0F0F0F" 
      (gethash "grey6" *color-table*) "#0F0F0F" 
      (gethash "gray7" *color-table*) "#121212" 
      (gethash "grey7" *color-table*) "#121212" 
      (gethash "gray8" *color-table*) "#141414" 
      (gethash "grey8" *color-table*) "#141414" 
      (gethash "gray9" *color-table*) "#171717" 
      (gethash "grey9" *color-table*) "#171717" 
      (gethash "gray10" *color-table*) "#1A1A1A" 
      (gethash "grey10" *color-table*) "#1A1A1A" 
      (gethash "gray11" *color-table*) "#1C1C1C" 
      (gethash "grey11" *color-table*) "#1C1C1C" 
      (gethash "gray12" *color-table*) "#1F1F1F" 
      (gethash "grey12" *color-table*) "#1F1F1F" 
      (gethash "gray13" *color-table*) "#212121" 
      (gethash "grey13" *color-table*) "#212121" 
      (gethash "gray14" *color-table*) "#242424" 
      (gethash "grey14" *color-table*) "#242424" 
      (gethash "gray15" *color-table*) "#262626" 
      (gethash "grey15" *color-table*) "#262626" 
      (gethash "gray16" *color-table*) "#292929" 
      (gethash "grey16" *color-table*) "#292929" 
      (gethash "gray17" *color-table*) "#2B2B2B" 
      (gethash "grey17" *color-table*) "#2B2B2B" 
      (gethash "gray18" *color-table*) "#2E2E2E" 
      (gethash "grey18" *color-table*) "#2E2E2E" 
      (gethash "gray19" *color-table*) "#303030" 
      (gethash "grey19" *color-table*) "#303030" 
      (gethash "gray20" *color-table*) "#333333" 
      (gethash "grey20" *color-table*) "#333333" 
      (gethash "gray21" *color-table*) "#363636" 
      (gethash "grey21" *color-table*) "#363636" 
      (gethash "gray22" *color-table*) "#383838" 
      (gethash "grey22" *color-table*) "#383838" 
      (gethash "gray23" *color-table*) "#3B3B3B" 
      (gethash "grey23" *color-table*) "#3B3B3B" 
      (gethash "gray24" *color-table*) "#3D3D3D" 
      (gethash "grey24" *color-table*) "#3D3D3D" 
      (gethash "gray25" *color-table*) "#404040" 
      (gethash "grey25" *color-table*) "#404040" 
      (gethash "gray26" *color-table*) "#424242" 
      (gethash "grey26" *color-table*) "#424242" 
      (gethash "gray27" *color-table*) "#454545" 
      (gethash "grey27" *color-table*) "#454545" 
      (gethash "gray28" *color-table*) "#474747" 
      (gethash "grey28" *color-table*) "#474747" 
      (gethash "gray29" *color-table*) "#4A4A4A" 
      (gethash "grey29" *color-table*) "#4A4A4A" 
      (gethash "gray30" *color-table*) "#4D4D4D" 
      (gethash "grey30" *color-table*) "#4D4D4D" 
      (gethash "gray31" *color-table*) "#4F4F4F" 
      (gethash "grey31" *color-table*) "#4F4F4F" 
      (gethash "gray32" *color-table*) "#525252" 
      (gethash "grey32" *color-table*) "#525252" 
      (gethash "gray33" *color-table*) "#545454" 
      (gethash "grey33" *color-table*) "#545454" 
      (gethash "gray34" *color-table*) "#575757" 
      (gethash "grey34" *color-table*) "#575757" 
      (gethash "gray35" *color-table*) "#595959" 
      (gethash "grey35" *color-table*) "#595959" 
      (gethash "gray36" *color-table*) "#5C5C5C" 
      (gethash "grey36" *color-table*) "#5C5C5C" 
      (gethash "gray37" *color-table*) "#5E5E5E" 
      (gethash "grey37" *color-table*) "#5E5E5E" 
      (gethash "gray38" *color-table*) "#616161" 
      (gethash "grey38" *color-table*) "#616161" 
      (gethash "gray39" *color-table*) "#636363" 
      (gethash "grey39" *color-table*) "#636363" 
      (gethash "gray40" *color-table*) "#666666" 
      (gethash "grey40" *color-table*) "#666666" 
      (gethash "gray41" *color-table*) "#696969" 
      (gethash "grey41" *color-table*) "#696969" 
      (gethash "gray42" *color-table*) "#6B6B6B" 
      (gethash "grey42" *color-table*) "#6B6B6B" 
      (gethash "gray43" *color-table*) "#6E6E6E" 
      (gethash "grey43" *color-table*) "#6E6E6E" 
      (gethash "gray44" *color-table*) "#707070" 
      (gethash "grey44" *color-table*) "#707070" 
      (gethash "gray45" *color-table*) "#737373" 
      (gethash "grey45" *color-table*) "#737373" 
      (gethash "gray46" *color-table*) "#757575" 
      (gethash "grey46" *color-table*) "#757575" 
      (gethash "gray47" *color-table*) "#787878" 
      (gethash "grey47" *color-table*) "#787878" 
      (gethash "gray48" *color-table*) "#7A7A7A" 
      (gethash "grey48" *color-table*) "#7A7A7A" 
      (gethash "gray49" *color-table*) "#7D7D7D" 
      (gethash "grey49" *color-table*) "#7D7D7D" 
      (gethash "gray50" *color-table*) "#7F7F7F" 
      (gethash "grey50" *color-table*) "#7F7F7F" 
      (gethash "gray51" *color-table*) "#828282" 
      (gethash "grey51" *color-table*) "#828282" 
      (gethash "gray52" *color-table*) "#858585" 
      (gethash "grey52" *color-table*) "#858585" 
      (gethash "gray53" *color-table*) "#878787" 
      (gethash "grey53" *color-table*) "#878787" 
      (gethash "gray54" *color-table*) "#8A8A8A" 
      (gethash "grey54" *color-table*) "#8A8A8A" 
      (gethash "gray55" *color-table*) "#8C8C8C" 
      (gethash "grey55" *color-table*) "#8C8C8C" 
      (gethash "gray56" *color-table*) "#8F8F8F" 
      (gethash "grey56" *color-table*) "#8F8F8F" 
      (gethash "gray57" *color-table*) "#919191" 
      (gethash "grey57" *color-table*) "#919191" 
      (gethash "gray58" *color-table*) "#949494" 
      (gethash "grey58" *color-table*) "#949494" 
      (gethash "gray59" *color-table*) "#969696" 
      (gethash "grey59" *color-table*) "#969696" 
      (gethash "gray60" *color-table*) "#999999" 
      (gethash "grey60" *color-table*) "#999999" 
      (gethash "gray61" *color-table*) "#9C9C9C" 
      (gethash "grey61" *color-table*) "#9C9C9C" 
      (gethash "gray62" *color-table*) "#9E9E9E" 
      (gethash "grey62" *color-table*) "#9E9E9E" 
      (gethash "gray63" *color-table*) "#A1A1A1" 
      (gethash "grey63" *color-table*) "#A1A1A1" 
      (gethash "gray64" *color-table*) "#A3A3A3" 
      (gethash "grey64" *color-table*) "#A3A3A3" 
      (gethash "gray65" *color-table*) "#A6A6A6" 
      (gethash "grey65" *color-table*) "#A6A6A6" 
      (gethash "gray66" *color-table*) "#A8A8A8" 
      (gethash "grey66" *color-table*) "#A8A8A8" 
      (gethash "gray67" *color-table*) "#ABABAB" 
      (gethash "grey67" *color-table*) "#ABABAB" 
      (gethash "gray68" *color-table*) "#ADADAD" 
      (gethash "grey68" *color-table*) "#ADADAD" 
      (gethash "gray69" *color-table*) "#B0B0B0" 
      (gethash "grey69" *color-table*) "#B0B0B0" 
      (gethash "gray70" *color-table*) "#B3B3B3" 
      (gethash "grey70" *color-table*) "#B3B3B3" 
      (gethash "gray71" *color-table*) "#B5B5B5" 
      (gethash "grey71" *color-table*) "#B5B5B5" 
      (gethash "gray72" *color-table*) "#B8B8B8" 
      (gethash "grey72" *color-table*) "#B8B8B8" 
      (gethash "gray73" *color-table*) "#BABABA" 
      (gethash "grey73" *color-table*) "#BABABA" 
      (gethash "gray74" *color-table*) "#BDBDBD" 
      (gethash "grey74" *color-table*) "#BDBDBD" 
      (gethash "gray75" *color-table*) "#BFBFBF" 
      (gethash "grey75" *color-table*) "#BFBFBF" 
      (gethash "gray76" *color-table*) "#C2C2C2" 
      (gethash "grey76" *color-table*) "#C2C2C2" 
      (gethash "gray77" *color-table*) "#C4C4C4" 
      (gethash "grey77" *color-table*) "#C4C4C4" 
      (gethash "gray78" *color-table*) "#C7C7C7" 
      (gethash "grey78" *color-table*) "#C7C7C7" 
      (gethash "gray79" *color-table*) "#C9C9C9" 
      (gethash "grey79" *color-table*) "#C9C9C9" 
      (gethash "gray80" *color-table*) "#CCCCCC" 
      (gethash "grey80" *color-table*) "#CCCCCC" 
      (gethash "gray81" *color-table*) "#CFCFCF" 
      (gethash "grey81" *color-table*) "#CFCFCF" 
      (gethash "gray82" *color-table*) "#D1D1D1" 
      (gethash "grey82" *color-table*) "#D1D1D1" 
      (gethash "gray83" *color-table*) "#D4D4D4" 
      (gethash "grey83" *color-table*) "#D4D4D4" 
      (gethash "gray84" *color-table*) "#D6D6D6" 
      (gethash "grey84" *color-table*) "#D6D6D6" 
      (gethash "gray85" *color-table*) "#D9D9D9" 
      (gethash "grey85" *color-table*) "#D9D9D9" 
      (gethash "gray86" *color-table*) "#DBDBDB" 
      (gethash "grey86" *color-table*) "#DBDBDB" 
      (gethash "gray87" *color-table*) "#DEDEDE" 
      (gethash "grey87" *color-table*) "#DEDEDE" 
      (gethash "gray88" *color-table*) "#E0E0E0" 
      (gethash "grey88" *color-table*) "#E0E0E0" 
      (gethash "gray89" *color-table*) "#E3E3E3" 
      (gethash "grey89" *color-table*) "#E3E3E3" 
      (gethash "gray90" *color-table*) "#E5E5E5" 
      (gethash "grey90" *color-table*) "#E5E5E5" 
      (gethash "gray91" *color-table*) "#E8E8E8" 
      (gethash "grey91" *color-table*) "#E8E8E8" 
      (gethash "gray92" *color-table*) "#EBEBEB" 
      (gethash "grey92" *color-table*) "#EBEBEB" 
      (gethash "gray93" *color-table*) "#EDEDED" 
      (gethash "grey93" *color-table*) "#EDEDED" 
      (gethash "gray94" *color-table*) "#F0F0F0" 
      (gethash "grey94" *color-table*) "#F0F0F0" 
      (gethash "gray95" *color-table*) "#F2F2F2" 
      (gethash "grey95" *color-table*) "#F2F2F2" 
      (gethash "gray96" *color-table*) "#F5F5F5" 
      (gethash "grey96" *color-table*) "#F5F5F5" 
      (gethash "gray97" *color-table*) "#F7F7F7" 
      (gethash "grey97" *color-table*) "#F7F7F7" 
      (gethash "gray98" *color-table*) "#FAFAFA" 
      (gethash "grey98" *color-table*) "#FAFAFA" 
      (gethash "gray99" *color-table*) "#FCFCFC" 
      (gethash "grey99" *color-table*) "#FCFCFC" 
      (gethash "gray100" *color-table*) "#FFFFFF" 
      (gethash "grey100" *color-table*) "#FFFFFF" 
      (gethash "darkgrey" *color-table*) "#A9A9A9" 
      (gethash "darkgray" *color-table*) "#A9A9A9" 
      (gethash "darkblue" *color-table*) "#00008B" 
      (gethash "darkcyan" *color-table*) "#008B8B" 
      (gethash "darkmagenta" *color-table*) "#8B008B" 
      (gethash "lightred" *color-table*) "#f03232" 
      (gethash "darkred" *color-table*) "#8B0000" 
      (gethash "lightgreen" *color-table*) "#90EE90")

(defun correct-color-name (str)
  (some #'(lambda (z) (string= z str))
        (loop for k being the hash-keys of *color-table* collect k)))

;; Hex colors can either read
;;   #rrggbb or #rrggbbaa
;; with rr, bb, gg and aa being two-digit hex numbers.
;; Numbers containing a transparency aa only work if the gnuplot version
;; that is being used supports it which should be any gnuplot >=5.0.
(defun correct-color-hex (str)
  (and ( or (= (length str) 7) (= (length str) 9))
       (char= (schar str 0) #\#)
       (every #'(lambda (z) (position z "0123456789abcdef"))
              (subseq str 1))))

(defun correct-color (str)
  (or (correct-color-name str)
      (correct-color-hex str)
      (string= str "auto")))

(defun hex-to-numeric-list (str)
  (let ((hex1 (subseq str 1 3))
        (hex2 (subseq str 3 5))
        (hex3 (subseq str 5)))
    (list
      (/ (parse-integer hex1 :radix 16) 255.0)
      (/ (parse-integer hex2 :radix 16) 255.0)
      (/ (parse-integer hex3 :radix 16) 255.0))))

;; Interprets a rgb string or a rgba string and converts the latter to argb
;; as gnuplot needs this format
(defun hex-to-rgb (str)
  (if (= (length str) 7)
    (format nil "rgb '~a'" str)
    (format nil "rgb '#~a~a'" (subseq str 7 9) (subseq str 1 7))))

(defun update-color (opt val)
  (if (numberp val)
      (case val 
	    (0      (setf val "black"))
	    (1      (setf val "red"))
	    (2      (setf val "blue"))
	    (3      (setf val "green"))
	    (4      (setf val "pink"))
	    (5      (setf val "orange"))
	    (6      (setf val "violet"))
	    (7      (setf val "yellow"))
	    (8      (setf val "magenta"))
	    (9      (setf val "cyan"))
	    (10     (setf val "gray"))
	    (11     (setf val "springgreen"))
	    (12     (setf val "darkcyan"))
	    (13     (setf val "darkmagenta"))
	    (14     (setf val "darkred"))
	    (15     (setf val "mintcream"))
	    (16     (setf val "azure"))
	    (17     (setf val "navy"))
	    (otherwise (merror "model3d: unknown color ~M" val))))
  (let ((str (atom-to-downcased-string val)))
    (unless (correct-color str)
      (merror "model3d: unknown color ~M" val))
    (when (correct-color-name str)
      (setf str (gethash str *color-table*)))
    (setf (gethash opt *gr-options*) str)))




;; update option key_pos
;; ---------------------
(defun update-key_pos (val)
  (case val
    ($top_left      (setf (gethash '$key_pos *gr-options*) "top left"))
    ($top_center    (setf (gethash '$key_pos *gr-options*) "top center"))
    ($top_right     (setf (gethash '$key_pos *gr-options*) "top right"))
    ($center_left   (setf (gethash '$key_pos *gr-options*) "center left"))
    ($center        (setf (gethash '$key_pos *gr-options*) "center center"))
    ($center_right  (setf (gethash '$key_pos *gr-options*) "center right"))
    ($bottom_left   (setf (gethash '$key_pos *gr-options*) "bottom left"))
    ($bottom_center (setf (gethash '$key_pos *gr-options*) "bottom center"))
    ($bottom_right  (setf (gethash '$key_pos *gr-options*) "bottom right"))
    (otherwise (merror "draw: illegal key position specification"))))


;; update option terminal
;; ----------------------
;; *draw-terminal-number* is used when working with
;; multiple windows. Empty string means
;; we are working with only one window
(defvar *draw-terminal-number* "")

(defun update-terminal (val)
  (let ((terms '($screen $png $pngcairo $jpg $gif $eps $eps_color $canvas
                 $epslatex $epslatex_standalone $svg $x11 $qt
                 $dumb $dumb_file $pdf $pdfcairo $wxt $animated_gif $windows
                 $multipage_pdfcairo $multipage_pdf $multipage_eps 
                 $multipage_eps_color $aquaterm $tiff $vrml $obj $stl $pnm $ply)))
     (cond
       ((member val terms)
          (when (and (eq val '$png) $draw_use_pngcairo)
            (setq val '$pngcairo))
          (setf (gethash '$terminal *gr-options*) val
                *draw-terminal-number* ""))
       ((and ($listp val)
             (= ($length val) 2)
             (member (cadr val) '($screen $wxt $aquaterm $qt $windows))
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



;; update option isolines
;; ----------------------
(defvar *draw-isolines-type* 0)
(defvar *draw-isolines-fun* nil)

(defun check-isolines-model (grobj lis)
  (when (null (position *draw-isolines-type* lis))
    (merror (format nil "draw (~a): unacceptable isolines model" grobj))))

(defun update-isolines-expression (vars)
  (let ((texture (gethash '$isolines *gr-options*)))
    (when (not ($subsetp ($setify ($listofvars texture))
                         ($setify vars)))
      (merror "draw: incompatible variables in isolines expression"))
    (setf *draw-isolines-fun* (coerce-float-fun texture vars))))

(defun update-isolines (val)
  (cond
    ((or (null val)
         (equal val '$none))
      (setf (gethash '$isolines *gr-options*) '$none
            *draw-isolines-type* 0
            *draw-isolines-fun* nil))
    ((equal val t)
      (let ((model '((mlist) $z $x $y $z)))
        (setf (gethash '$isolines *gr-options*) model
              *draw-isolines-type* 3
              *draw-isolines-fun* (coerce-float-fun
                                    ($first model)
                                    (list '(mlist) ($second model) ($third model) ($fourth model))))))
    ((and ($listp val)
          ($subsetp ($setify ($listofvars ($first val)))
                    ($setify ($rest val))))
       (case ($length val)
         (2 (setf (gethash '$isolines *gr-options*) val
                  *draw-isolines-type* 1
                  *draw-isolines-fun* (coerce-float-fun
                              ($first val)
                              (list '(mlist) ($second val)))))
         (3 (setf (gethash '$isolines *gr-options*) val
                  *draw-isolines-type* 2
                  *draw-isolines-fun* (coerce-float-fun
                                  ($first val)
                                  (list '(mlist) ($second val) ($third val)))))
         (4 (setf (gethash '$isolines *gr-options*) val
                  *draw-isolines-type* 3
                  *draw-isolines-fun* (coerce-float-fun
                                  ($first val)
                                  (list '(mlist) ($second val) ($third val) ($fourth val)))))
         (otherwise (merror "draw: illegal length of isolines"))))
    ((not ($listp val))
       ; isolines is given an expression without 
       ; explicit declaration of its variables.
       ; Each graphic object must check for them.
       (setf (gethash '$isolines *gr-options*) val
             *draw-isolines-type* 99
             *draw-isolines-fun* nil))
    (t
        (merror "draw: illegal isolines definition")) ) )



;; update contour (Gnuplot) and isolines (VTK) levels
;; ---------------------------------------------------
(defun update-contour-isolines (opt val)
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



;; update boolean type options
;; ---------------------------
(defun update-boolean-option (opt val)
  (if (or (equal val t)
          (null val))
      (setf (gethash opt *gr-options*) val)
      (merror "draw: non boolean value: ~M " val)))



;; update positive integer type options
;; ------------------------------------
(defun update-positive-integer (opt val)
  (if (and (integerp val)
           (> val 0 ))
      (setf (gethash opt *gr-options*) val)
      (merror "draw: non positive integer: ~M " val)))



;; update positive float type options
;; ----------------------------------
(defun update-positive-float (opt val)
  (setf val ($float val))
  (if (and (numberp val)
           (> val 0 ))
      (setf (gethash opt *gr-options*) val)
      (merror "draw: Non positive number: ~M " val)))



;; update non negative float type options
;; --------------------------------------
(defun update-nonnegative-float (opt val)
  (setf val ($float val))
  (if (and (numberp val)
           (>= val 0 ))
      (setf (gethash opt *gr-options*) val)
      (merror "draw: Not a non-negative number: ~M " val)))



;; update option: view
;; -------------------
(defun update-view (val)
  (cond
    ((eql val '$map)
      (setf (gethash '$view *gr-options*) val))
    ((and ($listp val)
          (= ($length val) 2))
       (let ((rv ($float (cadr val)))
             (rh ($float (caddr val))) )
         (unless
           (and (numberp rv) (>= rv 0) (<= rv 360) )
           (merror "draw: vertical rotation angle must be in [0, 360]"))
         (unless
           (and (numberp rh) (>= rh 0) (<= rh 360) )
           (merror "draw: horizontal rotation angle must be in [0, 360]"))
         (setf (gethash '$view *gr-options*)
               (list rv rh))))
    (t
      (merror "draw: illegal view specification ~M" val)) ))



;; update option: interpolate_color
;; --------------------------------
(defun update-interpolate_color (val)
  (cond
    ((null val)
      (setf (gethash '$interpolate_color *gr-options*) "depthorder"))
    ((equal val t)
      (setf (gethash '$interpolate_color *gr-options*) "interpolate 0, 0"))
    ((and ($listp val)
          (= ($length val) 2))
       (let ((x ($float (cadr val)))
             (y ($float (caddr val))) )
         (unless
           (and (numberp x) (numberp y))
           (merror "draw: interpolate_color parameters must be numbers"))
         (setf (gethash '$interpolate_color *gr-options*)
               (format nil "interpolate ~a,~a" x y))))
    (t
      (merror "draw: illegal interpolate_color specification ~M" val)) ))



;; update option allocation
;; ------------------------
(defun update-allocation (val)
  (let (fval cls)
    (cond
      ((and ($listp val)
            (= ($length val) 2)
            (every #'(lambda (z) (and ($listp z)
                                      (= ($length z) 2)))
                   (rest val))
         (setf fval (rest ($float val)))
         (setf cls (list (cadar fval) (caddar fval) (cadadr fval) (caddr (cadr fval))))
         (if (every #'(lambda (z) (and (float z) (>= z 0.0) (<= z 1.0))) cls)
           (setf (gethash '$allocation *gr-options*) cls)
           (merror "draw: allocations must me given in relative values")) ))
      (t
         (merror "draw: illegal allocation format ~M" val)))))



;; update option dimensions
;; ------------------------
(defun update-dimensions (val)
  (let (cls)
    (cond
      ((and ($listp val)
            (= ($length val) 2))
         (setf cls (rest val))
         (if (every #'(lambda (z) (and (numberp z) (> z 0))) cls)
             (setf (gethash '$dimensions *gr-options*) cls)
             (merror "draw: illegal dimensions")))
      (t
         (merror "draw: illegal dimensions format ~M" val)))))



;; update option point_type
;; ------------------------
(defun update-pointtype (val)
  (cond
    ((and (integerp val) (>= val -1 ))
       (setf (gethash '$point_type *gr-options*) val))
    (t (let ((shapes '($none $dot $plus $multiply $asterisk
                       $square $filled_square $circle $filled_circle
                       $up_triangle $filled_up_triangle $down_triangle
                       $filled_down_triangle $diamant $filled_diamant
                       $sphere $cube $cylinder $cone)))
          (if (member val shapes)
              (setf (gethash '$point_type *gr-options*) (- (position val shapes) 1))
              (merror "draw: illegal point type: ~M " val)))))  )



;; update string type options
;; --------------------------
(defun update-string (opt val)
  (setf (gethash opt *gr-options*) ($sconcat val)))



;; update opacity option
;; ---------------------
(defun update-opacity (val)
  (let ((value ($float val)))
    (when (or (not (floatp value))
              (> value 1)
              (< value 0))
      (merror "draw: illegal opacity value: ~M " val))
    (setf (gethash '$opacity *gr-options*) value)))



;; update palette option
;; ---------------------

(defun update-palette (val)
  ; defined as $color, $gray, [f1,f2,f3], with -36<=fi<=36,
  ; or a list of triplets defining a user palette.
  (cond ((member val '($color $gray))
          (setf (gethash '$palette *gr-options*) val))

        ((and ($listp val)
              (= ($length val) 3)
              (every #'(lambda (x) (and (integerp x) (<= (abs x) 36)))
                     (cdr val)) )
          (setf (gethash '$palette *gr-options*) (rest val)))

        ((and ($listp val)  ; user defined palette without transparency
              (not ($listp (cadr val)) ))
           (let* ((palette (cdr val))
                  (n (length palette))
                  str color)
             (setf (gethash '$palette *gr-options*)
                   (loop for k below n
                      do (setf str (atom-to-downcased-string (nth k palette)))
                         (cond ((correct-color-hex str)
                                  (setf color (hex-to-numeric-list str)))
                               ((correct-color-name str)
                                  (setf color (hex-to-numeric-list (gethash str *color-table*))))
                               (t
                                  (merror "draw: illegal color in palette description")))
                      collect color))))

        ((and ($listp val)  ; user defined palette with transparency
              (every #'(lambda (x) (and ($listp x) (= (length x) 3)))
                     (cdr val)))
           (let* ((palette (cdr val))
                  (n (length palette))
                  str color transparency)
             (setf (gethash '$palette *gr-options*)
                   (loop for k below n
                      do (setf str (atom-to-downcased-string ($first (nth k palette))))
                         (cond ((correct-color-hex str)
                                  (setf color (hex-to-numeric-list str)))
                               ((correct-color-name str)
                                  (setf color (hex-to-numeric-list (gethash str *color-table*))))
                               (t
                                  (merror "draw: illegal color in palette description")))
                         (setf transparency ($float ($second (nth k palette))))
                         (when (or (< transparency 0)
                                   (> transparency 1)
                                   (not (floatp transparency)))
                           (merror "draw: illegal transparency in palette description"))
                      collect (append color (list transparency))))))

        (t
          (merror "draw: illegal palette description: ~M" val))) )



;; update line_type and line-axes
;; Negative indices indicate the number of faces to be drawn in case of tubes.
;; Default number of faces is 8.
;; --------------------------------------------------------------------------
(defun update-linestyle (opt val)
  (cond
    ((atom val)
       (case val
         ($dots                    (setf (gethash opt *gr-options*) 0))
         ($solid                   (setf (gethash opt *gr-options*) 1))
         ($dashes                  (setf (gethash opt *gr-options*) 2))
         ($short_dashes            (setf (gethash opt *gr-options*) 3))
         ($short_long_dashes       (setf (gethash opt *gr-options*) 4))
         ($short_short_long_dashes (setf (gethash opt *gr-options*) 5))
         ($dot_dash                (setf (gethash opt *gr-options*) 6))
         ($tube                    (setf (gethash opt *gr-options*) -8))
         (otherwise
	  (if (numberp val)
	      (setf (gethash opt *gr-options*) val) (merror "draw: illegal line type: ~M" val)))))
    ((and ($listp val)
          (= ($length val) 2)
          (equal ($first val) '$tube)
          (integerp ($second val))
          (> ($second val) 2))
       (setf (gethash opt *gr-options*) (- ($second val)) ) )
    (t
       (merror "draw: unknown line type: ~M" val))))



;; update points_joined option
;; ---------------------------
(defun update-pointsjoined (val)
  (if (member val '(t nil $impulses))
    (setf (gethash '$points_joined *gr-options*) val)
    (merror "draw: illegal points_joined option: ~M " val)) )



;; update capping option
;; a list of two elements, false and/or true
;; -----------------------------------------
(defun update-capping (val)
  (cond
    ((and ($listp val)
          (= ($length val) 2)
          (member ($first val) '(nil t))
          (member ($second val) '(nil t)))
       (setf (gethash '$capping *gr-options*) val))
    ((member val '(nil t))
       (setf (gethash '$capping *gr-options*) (list '(mlist simp) val val)))
    (t
      (merror "draw: illegal capping: ~M " val))) )



(defun ini-local-option-variables ()
  (setf ; global variables
       *draw-transform-dimensions* 0
       *draw-transform-f1* nil
       *draw-transform-f2* nil
       *draw-transform-f3* nil
       *draw-enhanced3d-type* 0
       *draw-enhanced3d-fun* nil
       *draw-isolines-type* 0
       *draw-isolines-fun* nil)  )



;; Sets default values to global options
(defun ini-global-options ()
  (setf ; global options
      (gethash '$columns *gr-options*)      1
      (gethash '$terminal *gr-options*)     '$screen  ; defined as screen, png, jpg, gif, svg,
                                                      ; eps, eps_color, pdf, pdfcairo, wxt or
                                                      ; aquaterm. A list of type [term, number]
                                                      ; is also admitted if term is screen, wxt
                                                      ; or aquaterm
      (gethash '$key_pos *gr-options*)            nil
      (gethash '$dimensions *gr-options*)        '(600 500)
      (gethash '$file_name *gr-options*)         "maxima_out"
      (gethash '$gnuplot_file_name *gr-options*) (format nil "maxout~d.gnuplot" (getpid))
      (gethash '$data_file_name *gr-options*)    (format nil "data~d.gnuplot" (getpid))
      (gethash '$delay *gr-options*)             5 ; delay for animated gif's, default 5*(1/100) sec
   ))


;; Sets new values to graphic options
(defun update-gr-option (opt val)
   (case opt
      ($allocation
        (update-allocation val))
      ($dimensions
        (update-dimensions val))
      ($fill_density ; in range [0, 1]
            (setf val ($float val))
            (if (and (numberp val)
                     (>= val 0 )
                     (<= val 1 ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: fill_density must be a number in [0, 1]")))
      (($line_width $head_length $head_angle $xaxis_width $yaxis_width $zaxis_width $font_size)
            (update-positive-float opt val))
      ($xyplane ; defined as real number or false
            (setf val ($float val))
            (if (or (numberp val)
                    (null val ))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal xyplane allocation: ~M " val)))
      ($point_size ; defined as non negative numbers
        (update-nonnegative-float opt val))
      ($points_joined ; defined as true, false or $impulses
        (update-pointsjoined val))
      ($colorbox ; defined as true, false or string
            (if (or (member val '(t nil))
                    (stringp val) )
              (setf (gethash opt *gr-options*) val)
              (merror "draw: illegal colorbox option: ~M " val)) )
      (($line_type $xaxis_type $yaxis_type $zaxis_type) ; defined as $solid or $dots
         (update-linestyle opt val) )
      ($capping
         (update-capping val))
      ($point_type
         (update-pointtype val))
      (($columns $nticks $adapt_depth $xu_grid $yv_grid $delay $x_voxel $y_voxel $z_voxel)
            (update-positive-integer opt val))
      (($contour_levels $isolines_levels)   ; positive integer, increment or set
         (update-contour-isolines opt val))
      ($opacity
         (update-opacity val))
      (($transparent $border $logx $logx_secondary $logy $logy_secondary
        $logz $logcb $head_both
        $xaxis_secondary $yaxis_secondary $axis_bottom $axis_left $axis_top
        $axis_right $axis_3d $surface_hide $xaxis $yaxis $zaxis $unit_vectors
        $xtics_rotate $ytics_rotate $xtics_secondary_rotate $ytics_secondary_rotate
        $ztics_rotate $xtics_axis $ytics_axis $xtics_secondary_axis
        $ytics_secondary_axis $ztics_axis $draw_realpart $wired_surface) ; true or false
          (update-boolean-option opt val))
      ($filled_func  ; true, false or an expression
         (setf (gethash opt *gr-options*) val))
      ($transform
         (update-transform val))
      ($enhanced3d
         (update-enhanced3d val))
      ($isolines
         (update-isolines val))
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
                                        (format nil "'~a' ~a," (cadar k) (caddar k)))))
                            ((null k) (concatenate
                                         'string
                                         "("
                                         (string-right-trim "," str)
                                         ")")))))
                  (t
                     (merror "draw: illegal tics allocation: ~M" val)) ))
      ($terminal
        (update-terminal val))
      ($key_pos
        (update-key_pos val))
      ($zlabel_rotate
            (if (member val '(t nil $auto))
		(setf (gethash opt *gr-options*) val)
	        (merror "draw: illegal zlabel_rotate option: ~M" val)))
      ($head_type ; defined as $filled, $empty and $nofilled
            (if (member val '($filled $empty $nofilled))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal head type for vectors: ~M" val)))
      ($contour ; defined as $none, $base, $surface, $both and $map
            (if (member val '($none $base $surface $both $map))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal contour allocation: ~M" val)))
      ($proportional_axes ; defined as $none, $xy and $xyz
            (if (member val '($none $xy $xyz))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal proportional_axes specification")))
      ($error_type ; defined as $x, $y and $xy
            (if (member val '($x $y $xy $boxes))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal error_type specification")))
      ($label_alignment ; defined as $center, $left and $right
            (if (member val '($center $left $right))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal label alignment: ~M" val)))
      ($label_orientation ; defined as $horizontal and $vertical
            (if (member val '($horizontal $vertical))
                (setf (gethash opt *gr-options*) val)
                (merror "draw: illegal label orientation: ~M" val)))
      (($key $file_name $xy_file $title $xlabel $ylabel $zlabel $xlabel_secondary $ylabel_secondary
        $font $gnuplot_file_name $data_file_name)
            (update-string opt val))
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
      (($grid) ; defined as a Maxima list with two numbers.
	       ; 0   0 means "off",
               ; >0 >0 means "on with n grid lines per tick",
            (cond ((null val)
		   (setf (gethash opt *gr-options*) (list 0 0)))
		  ((eq val t)
		   (setf (gethash opt *gr-options*) (list 1 1)))
                  ((or (not ($listp val))
                       (/=  ($length val) 2))
                     (merror "draw: illegal grid lines specification: ~M " val))
                  (t
                     (let ((fval1 ($float (cadr val)))
                           (fval2 ($float (caddr val))))
                       (cond
                         ((or (not (floatp fval1))
                              (not (floatp fval2))
                              (< fval1 1)
			      (< fval2 1))
                            (merror "grid: illegal grid lines specification"))
                         (t
			   (setf (gethash opt *gr-options*) (list ($round fval1) ($round fval2))) )
			 )  ))) )
      (($ip_grid $ip_grid_in)
       (if (not ($listp val))
           (merror "draw: illegal value for grid")
           (if (not (and (integerp ($first val))
                         (integerp ($second val))))
               (merror "draw: illegal value for grid")
               (setf (gethash opt *gr-options*) val))))
      ($palette
        (update-palette val))
      (($color $fill_color $xaxis_color $yaxis_color
        $zaxis_color $background_color)
        (update-color opt val))
      ($view
        (update-view val))
      ($interpolate_color
        (update-interpolate_color val))


      ; DEPRECATED OPTIONS
      ($tube_extremes
        ($print "Warning: 'tube_extremes' is deprecated, using 'capping' instead...")
        (update-capping (substitute nil '$open (substitute t '$closed val))))
      ($file_bgcolor
        ($print "Warning: 'file_bgcolor' is deprecated, using 'background_color' instead...")
        (update-color '$background_color val))
      ($rot_vertical
        ($print "Warning: 'rot_vertical' is deprecated, using 'view' instead...")
        (update-view (list '(mlist) val (second (gethash '$view *gr-options*)))))
      ($rot_horizontal
        ($print "Warning: 'rot_horizontal' is deprecated, using 'view' instead...")
        (update-view (list '(mlist) (first (gethash '$view *gr-options*)) val)))
      ($pic_width
        ($print "Warning: 'pic_width' is deprecated, using 'dimensions' instead...")
        (update-dimensions (list '(mlist) val (second (gethash '$dimensions *gr-options*)))))
      ($pic_height
        ($print "Warning: 'pic_height' is deprecated, using 'dimensions' instead...")
        (update-dimensions (list '(mlist) (first (gethash '$dimensions *gr-options*)) val)))
      (($eps_width $pdf_width)
        ($print "Warning: 'eps_width' is deprecated, using 'dimensions' instead...")
        (update-dimensions (list '(mlist) (* 100 val) (second (gethash '$dimensions *gr-options*)))))
      (($eps_height $pdf_height)
        ($print "Warning: 'eps_height' is deprecated, using 'dimensions' instead...")
        (update-dimensions (list '(mlist) (first (gethash '$dimensions *gr-options*)) (* 100 val))))

      (otherwise (merror "draw: unknown option ~M " opt))  ) )








;;; OTHER COMMON GNUPLOT - VTK AUXILIARY FUNCTIONS


(defun near-equal (a b)
  (let ((eps 10.0d-14))
    (< (abs (- a b)) eps)))

;; Transforms arguments to make-scene-2d, make-scene-3d,
;; draw, and vtk3d to a unique list. With this piece of code,
;; gr2d, gr3d, draw, and model3d admit as arguments nested lists
;; of options and graphic objects
(defmacro listify-arguments (args)
   `(rest ($flatten
             ($tree_reduce 
               '$append
               (cons '(mlist)
                     (map 
                       'list #'(lambda (z) (if ($listp z) z (list '(mlist) z)))
                       ,args))))))

;; The following functions implement the marching cubes algorithm
;; for implicit functions in 3d.
(defvar *i3d_triangles*
  '#(#(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 8 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 1 9 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 8 3 9 8 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 2 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 8 3 1 2 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(9 2 10 0 2 9 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(2 8 3 2 10 8 10 9 8 -1 -1 -1 -1 -1 -1 -1)
     #(3 11 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 11 2 8 11 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 9 0 2 3 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 11 2 1 9 11 9 8 11 -1 -1 -1 -1 -1 -1 -1)
     #(3 10 1 11 10 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 10 1 0 8 10 8 11 10 -1 -1 -1 -1 -1 -1 -1)
     #(3 9 0 3 11 9 11 10 9 -1 -1 -1 -1 -1 -1 -1)
     #(9 8 10 10 8 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(4 7 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(4 3 0 7 3 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 1 9 8 4 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(4 1 9 4 7 1 7 3 1 -1 -1 -1 -1 -1 -1 -1)
     #(1 2 10 8 4 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(3 4 7 3 0 4 1 2 10 -1 -1 -1 -1 -1 -1 -1)
     #(9 2 10 9 0 2 8 4 7 -1 -1 -1 -1 -1 -1 -1)
     #(2 10 9 2 9 7 2 7 3 7 9 4 -1 -1 -1 -1)
     #(8 4 7 3 11 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(11 4 7 11 2 4 2 0 4 -1 -1 -1 -1 -1 -1 -1)
     #(9 0 1 8 4 7 2 3 11 -1 -1 -1 -1 -1 -1 -1)
     #(4 7 11 9 4 11 9 11 2 9 2 1 -1 -1 -1 -1)
     #(3 10 1 3 11 10 7 8 4 -1 -1 -1 -1 -1 -1 -1)
     #(1 11 10 1 4 11 1 0 4 7 11 4 -1 -1 -1 -1)
     #(4 7 8 9 0 11 9 11 10 11 0 3 -1 -1 -1 -1)
     #(4 7 11 4 11 9 9 11 10 -1 -1 -1 -1 -1 -1 -1)
     #(9 5 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(9 5 4 0 8 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 5 4 1 5 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(8 5 4 8 3 5 3 1 5 -1 -1 -1 -1 -1 -1 -1)
     #(1 2 10 9 5 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(3 0 8 1 2 10 4 9 5 -1 -1 -1 -1 -1 -1 -1)
     #(5 2 10 5 4 2 4 0 2 -1 -1 -1 -1 -1 -1 -1)
     #(2 10 5 3 2 5 3 5 4 3 4 8 -1 -1 -1 -1)
     #(9 5 4 2 3 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 11 2 0 8 11 4 9 5 -1 -1 -1 -1 -1 -1 -1)
     #(0 5 4 0 1 5 2 3 11 -1 -1 -1 -1 -1 -1 -1)
     #(2 1 5 2 5 8 2 8 11 4 8 5 -1 -1 -1 -1)
     #(10 3 11 10 1 3 9 5 4 -1 -1 -1 -1 -1 -1 -1)
     #(4 9 5 0 8 1 8 10 1 8 11 10 -1 -1 -1 -1)
     #(5 4 0 5 0 11 5 11 10 11 0 3 -1 -1 -1 -1)
     #(5 4 8 5 8 10 10 8 11 -1 -1 -1 -1 -1 -1 -1)
     #(9 7 8 5 7 9 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(9 3 0 9 5 3 5 7 3 -1 -1 -1 -1 -1 -1 -1)
     #(0 7 8 0 1 7 1 5 7 -1 -1 -1 -1 -1 -1 -1)
     #(1 5 3 3 5 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(9 7 8 9 5 7 10 1 2 -1 -1 -1 -1 -1 -1 -1)
     #(10 1 2 9 5 0 5 3 0 5 7 3 -1 -1 -1 -1)
     #(8 0 2 8 2 5 8 5 7 10 5 2 -1 -1 -1 -1)
     #(2 10 5 2 5 3 3 5 7 -1 -1 -1 -1 -1 -1 -1)
     #(7 9 5 7 8 9 3 11 2 -1 -1 -1 -1 -1 -1 -1)
     #(9 5 7 9 7 2 9 2 0 2 7 11 -1 -1 -1 -1)
     #(2 3 11 0 1 8 1 7 8 1 5 7 -1 -1 -1 -1)
     #(11 2 1 11 1 7 7 1 5 -1 -1 -1 -1 -1 -1 -1)
     #(9 5 8 8 5 7 10 1 3 10 3 11 -1 -1 -1 -1)
     #(5 7 0 5 0 9 7 11 0 1 0 10 11 10 0 -1)
     #(11 10 0 11 0 3 10 5 0 8 0 7 5 7 0 -1)
     #(11 10 5 7 11 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(10 6 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 8 3 5 10 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(9 0 1 5 10 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 8 3 1 9 8 5 10 6 -1 -1 -1 -1 -1 -1 -1)
     #(1 6 5 2 6 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 6 5 1 2 6 3 0 8 -1 -1 -1 -1 -1 -1 -1)
     #(9 6 5 9 0 6 0 2 6 -1 -1 -1 -1 -1 -1 -1)
     #(5 9 8 5 8 2 5 2 6 3 2 8 -1 -1 -1 -1)
     #(2 3 11 10 6 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(11 0 8 11 2 0 10 6 5 -1 -1 -1 -1 -1 -1 -1)
     #(0 1 9 2 3 11 5 10 6 -1 -1 -1 -1 -1 -1 -1)
     #(5 10 6 1 9 2 9 11 2 9 8 11 -1 -1 -1 -1)
     #(6 3 11 6 5 3 5 1 3 -1 -1 -1 -1 -1 -1 -1)
     #(0 8 11 0 11 5 0 5 1 5 11 6 -1 -1 -1 -1)
     #(3 11 6 0 3 6 0 6 5 0 5 9 -1 -1 -1 -1)
     #(6 5 9 6 9 11 11 9 8 -1 -1 -1 -1 -1 -1 -1)
     #(5 10 6 4 7 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(4 3 0 4 7 3 6 5 10 -1 -1 -1 -1 -1 -1 -1)
     #(1 9 0 5 10 6 8 4 7 -1 -1 -1 -1 -1 -1 -1)
     #(10 6 5 1 9 7 1 7 3 7 9 4 -1 -1 -1 -1)
     #(6 1 2 6 5 1 4 7 8 -1 -1 -1 -1 -1 -1 -1)
     #(1 2 5 5 2 6 3 0 4 3 4 7 -1 -1 -1 -1)
     #(8 4 7 9 0 5 0 6 5 0 2 6 -1 -1 -1 -1)
     #(7 3 9 7 9 4 3 2 9 5 9 6 2 6 9 -1)
     #(3 11 2 7 8 4 10 6 5 -1 -1 -1 -1 -1 -1 -1)
     #(5 10 6 4 7 2 4 2 0 2 7 11 -1 -1 -1 -1)
     #(0 1 9 4 7 8 2 3 11 5 10 6 -1 -1 -1 -1)
     #(9 2 1 9 11 2 9 4 11 7 11 4 5 10 6 -1)
     #(8 4 7 3 11 5 3 5 1 5 11 6 -1 -1 -1 -1)
     #(5 1 11 5 11 6 1 0 11 7 11 4 0 4 11 -1)
     #(0 5 9 0 6 5 0 3 6 11 6 3 8 4 7 -1)
     #(6 5 9 6 9 11 4 7 9 7 11 9 -1 -1 -1 -1)
     #(10 4 9 6 4 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(4 10 6 4 9 10 0 8 3 -1 -1 -1 -1 -1 -1 -1)
     #(10 0 1 10 6 0 6 4 0 -1 -1 -1 -1 -1 -1 -1)
     #(8 3 1 8 1 6 8 6 4 6 1 10 -1 -1 -1 -1)
     #(1 4 9 1 2 4 2 6 4 -1 -1 -1 -1 -1 -1 -1)
     #(3 0 8 1 2 9 2 4 9 2 6 4 -1 -1 -1 -1)
     #(0 2 4 4 2 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(8 3 2 8 2 4 4 2 6 -1 -1 -1 -1 -1 -1 -1)
     #(10 4 9 10 6 4 11 2 3 -1 -1 -1 -1 -1 -1 -1)
     #(0 8 2 2 8 11 4 9 10 4 10 6 -1 -1 -1 -1)
     #(3 11 2 0 1 6 0 6 4 6 1 10 -1 -1 -1 -1)
     #(6 4 1 6 1 10 4 8 1 2 1 11 8 11 1 -1)
     #(9 6 4 9 3 6 9 1 3 11 6 3 -1 -1 -1 -1)
     #(8 11 1 8 1 0 11 6 1 9 1 4 6 4 1 -1)
     #(3 11 6 3 6 0 0 6 4 -1 -1 -1 -1 -1 -1 -1)
     #(6 4 8 11 6 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(7 10 6 7 8 10 8 9 10 -1 -1 -1 -1 -1 -1 -1)
     #(0 7 3 0 10 7 0 9 10 6 7 10 -1 -1 -1 -1)
     #(10 6 7 1 10 7 1 7 8 1 8 0 -1 -1 -1 -1)
     #(10 6 7 10 7 1 1 7 3 -1 -1 -1 -1 -1 -1 -1)
     #(1 2 6 1 6 8 1 8 9 8 6 7 -1 -1 -1 -1)
     #(2 6 9 2 9 1 6 7 9 0 9 3 7 3 9 -1)
     #(7 8 0 7 0 6 6 0 2 -1 -1 -1 -1 -1 -1 -1)
     #(7 3 2 6 7 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(2 3 11 10 6 8 10 8 9 8 6 7 -1 -1 -1 -1)
     #(2 0 7 2 7 11 0 9 7 6 7 10 9 10 7 -1)
     #(1 8 0 1 7 8 1 10 7 6 7 10 2 3 11 -1)
     #(11 2 1 11 1 7 10 6 1 6 7 1 -1 -1 -1 -1)
     #(8 9 6 8 6 7 9 1 6 11 6 3 1 3 6 -1)
     #(0 9 1 11 6 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(7 8 0 7 0 6 3 11 0 11 6 0 -1 -1 -1 -1)
     #(7 11 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(7 6 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(3 0 8 11 7 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 1 9 11 7 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(8 1 9 8 3 1 11 7 6 -1 -1 -1 -1 -1 -1 -1)
     #(10 1 2 6 11 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 2 10 3 0 8 6 11 7 -1 -1 -1 -1 -1 -1 -1)
     #(2 9 0 2 10 9 6 11 7 -1 -1 -1 -1 -1 -1 -1)
     #(6 11 7 2 10 3 10 8 3 10 9 8 -1 -1 -1 -1)
     #(7 2 3 6 2 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(7 0 8 7 6 0 6 2 0 -1 -1 -1 -1 -1 -1 -1)
     #(2 7 6 2 3 7 0 1 9 -1 -1 -1 -1 -1 -1 -1)
     #(1 6 2 1 8 6 1 9 8 8 7 6 -1 -1 -1 -1)
     #(10 7 6 10 1 7 1 3 7 -1 -1 -1 -1 -1 -1 -1)
     #(10 7 6 1 7 10 1 8 7 1 0 8 -1 -1 -1 -1)
     #(0 3 7 0 7 10 0 10 9 6 10 7 -1 -1 -1 -1)
     #(7 6 10 7 10 8 8 10 9 -1 -1 -1 -1 -1 -1 -1)
     #(6 8 4 11 8 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(3 6 11 3 0 6 0 4 6 -1 -1 -1 -1 -1 -1 -1)
     #(8 6 11 8 4 6 9 0 1 -1 -1 -1 -1 -1 -1 -1)
     #(9 4 6 9 6 3 9 3 1 11 3 6 -1 -1 -1 -1)
     #(6 8 4 6 11 8 2 10 1 -1 -1 -1 -1 -1 -1 -1)
     #(1 2 10 3 0 11 0 6 11 0 4 6 -1 -1 -1 -1)
     #(4 11 8 4 6 11 0 2 9 2 10 9 -1 -1 -1 -1)
     #(10 9 3 10 3 2 9 4 3 11 3 6 4 6 3 -1)
     #(8 2 3 8 4 2 4 6 2 -1 -1 -1 -1 -1 -1 -1)
     #(0 4 2 4 6 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 9 0 2 3 4 2 4 6 4 3 8 -1 -1 -1 -1)
     #(1 9 4 1 4 2 2 4 6 -1 -1 -1 -1 -1 -1 -1)
     #(8 1 3 8 6 1 8 4 6 6 10 1 -1 -1 -1 -1)
     #(10 1 0 10 0 6 6 0 4 -1 -1 -1 -1 -1 -1 -1)
     #(4 6 3 4 3 8 6 10 3 0 3 9 10 9 3 -1)
     #(10 9 4 6 10 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(4 9 5 7 6 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 8 3 4 9 5 11 7 6 -1 -1 -1 -1 -1 -1 -1)
     #(5 0 1 5 4 0 7 6 11 -1 -1 -1 -1 -1 -1 -1)
     #(11 7 6 8 3 4 3 5 4 3 1 5 -1 -1 -1 -1)
     #(9 5 4 10 1 2 7 6 11 -1 -1 -1 -1 -1 -1 -1)
     #(6 11 7 1 2 10 0 8 3 4 9 5 -1 -1 -1 -1)
     #(7 6 11 5 4 10 4 2 10 4 0 2 -1 -1 -1 -1)
     #(3 4 8 3 5 4 3 2 5 10 5 2 11 7 6 -1)
     #(7 2 3 7 6 2 5 4 9 -1 -1 -1 -1 -1 -1 -1)
     #(9 5 4 0 8 6 0 6 2 6 8 7 -1 -1 -1 -1)
     #(3 6 2 3 7 6 1 5 0 5 4 0 -1 -1 -1 -1)
     #(6 2 8 6 8 7 2 1 8 4 8 5 1 5 8 -1)
     #(9 5 4 10 1 6 1 7 6 1 3 7 -1 -1 -1 -1)
     #(1 6 10 1 7 6 1 0 7 8 7 0 9 5 4 -1)
     #(4 0 10 4 10 5 0 3 10 6 10 7 3 7 10 -1)
     #(7 6 10 7 10 8 5 4 10 4 8 10 -1 -1 -1 -1)
     #(6 9 5 6 11 9 11 8 9 -1 -1 -1 -1 -1 -1 -1)
     #(3 6 11 0 6 3 0 5 6 0 9 5 -1 -1 -1 -1)
     #(0 11 8 0 5 11 0 1 5 5 6 11 -1 -1 -1 -1)
     #(6 11 3 6 3 5 5 3 1 -1 -1 -1 -1 -1 -1 -1)
     #(1 2 10 9 5 11 9 11 8 11 5 6 -1 -1 -1 -1)
     #(0 11 3 0 6 11 0 9 6 5 6 9 1 2 10 -1)
     #(11 8 5 11 5 6 8 0 5 10 5 2 0 2 5 -1)
     #(6 11 3 6 3 5 2 10 3 10 5 3 -1 -1 -1 -1)
     #(5 8 9 5 2 8 5 6 2 3 8 2 -1 -1 -1 -1)
     #(9 5 6 9 6 0 0 6 2 -1 -1 -1 -1 -1 -1 -1)
     #(1 5 8 1 8 0 5 6 8 3 8 2 6 2 8 -1)
     #(1 5 6 2 1 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 3 6 1 6 10 3 8 6 5 6 9 8 9 6 -1)
     #(10 1 0 10 0 6 9 5 0 5 6 0 -1 -1 -1 -1)
     #(0 3 8 5 6 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(10 5 6 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(11 5 10 7 5 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(11 5 10 11 7 5 8 3 0 -1 -1 -1 -1 -1 -1 -1)
     #(5 11 7 5 10 11 1 9 0 -1 -1 -1 -1 -1 -1 -1)
     #(10 7 5 10 11 7 9 8 1 8 3 1 -1 -1 -1 -1)
     #(11 1 2 11 7 1 7 5 1 -1 -1 -1 -1 -1 -1 -1)
     #(0 8 3 1 2 7 1 7 5 7 2 11 -1 -1 -1 -1)
     #(9 7 5 9 2 7 9 0 2 2 11 7 -1 -1 -1 -1)
     #(7 5 2 7 2 11 5 9 2 3 2 8 9 8 2 -1)
     #(2 5 10 2 3 5 3 7 5 -1 -1 -1 -1 -1 -1 -1)
     #(8 2 0 8 5 2 8 7 5 10 2 5 -1 -1 -1 -1)
     #(9 0 1 5 10 3 5 3 7 3 10 2 -1 -1 -1 -1)
     #(9 8 2 9 2 1 8 7 2 10 2 5 7 5 2 -1)
     #(1 3 5 3 7 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 8 7 0 7 1 1 7 5 -1 -1 -1 -1 -1 -1 -1)
     #(9 0 3 9 3 5 5 3 7 -1 -1 -1 -1 -1 -1 -1)
     #(9 8 7 5 9 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(5 8 4 5 10 8 10 11 8 -1 -1 -1 -1 -1 -1 -1)
     #(5 0 4 5 11 0 5 10 11 11 3 0 -1 -1 -1 -1)
     #(0 1 9 8 4 10 8 10 11 10 4 5 -1 -1 -1 -1)
     #(10 11 4 10 4 5 11 3 4 9 4 1 3 1 4 -1)
     #(2 5 1 2 8 5 2 11 8 4 5 8 -1 -1 -1 -1)
     #(0 4 11 0 11 3 4 5 11 2 11 1 5 1 11 -1)
     #(0 2 5 0 5 9 2 11 5 4 5 8 11 8 5 -1)
     #(9 4 5 2 11 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(2 5 10 3 5 2 3 4 5 3 8 4 -1 -1 -1 -1)
     #(5 10 2 5 2 4 4 2 0 -1 -1 -1 -1 -1 -1 -1)
     #(3 10 2 3 5 10 3 8 5 4 5 8 0 1 9 -1)
     #(5 10 2 5 2 4 1 9 2 9 4 2 -1 -1 -1 -1)
     #(8 4 5 8 5 3 3 5 1 -1 -1 -1 -1 -1 -1 -1)
     #(0 4 5 1 0 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(8 4 5 8 5 3 9 0 5 0 3 5 -1 -1 -1 -1)
     #(9 4 5 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(4 11 7 4 9 11 9 10 11 -1 -1 -1 -1 -1 -1 -1)
     #(0 8 3 4 9 7 9 11 7 9 10 11 -1 -1 -1 -1)
     #(1 10 11 1 11 4 1 4 0 7 4 11 -1 -1 -1 -1)
     #(3 1 4 3 4 8 1 10 4 7 4 11 10 11 4 -1)
     #(4 11 7 9 11 4 9 2 11 9 1 2 -1 -1 -1 -1)
     #(9 7 4 9 11 7 9 1 11 2 11 1 0 8 3 -1)
     #(11 7 4 11 4 2 2 4 0 -1 -1 -1 -1 -1 -1 -1)
     #(11 7 4 11 4 2 8 3 4 3 2 4 -1 -1 -1 -1)
     #(2 9 10 2 7 9 2 3 7 7 4 9 -1 -1 -1 -1)
     #(9 10 7 9 7 4 10 2 7 8 7 0 2 0 7 -1)
     #(3 7 10 3 10 2 7 4 10 1 10 0 4 0 10 -1)
     #(1 10 2 8 7 4 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(4 9 1 4 1 7 7 1 3 -1 -1 -1 -1 -1 -1 -1)
     #(4 9 1 4 1 7 0 8 1 8 7 1 -1 -1 -1 -1)
     #(4 0 3 7 4 3 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(4 8 7 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(9 10 8 10 11 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(3 0 9 3 9 11 11 9 10 -1 -1 -1 -1 -1 -1 -1)
     #(0 1 10 0 10 8 8 10 11 -1 -1 -1 -1 -1 -1 -1)
     #(3 1 10 11 3 10 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 2 11 1 11 9 9 11 8 -1 -1 -1 -1 -1 -1 -1)
     #(3 0 9 3 9 11 1 2 9 2 11 9 -1 -1 -1 -1)
     #(0 2 11 8 0 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(3 2 11 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(2 3 8 2 8 10 10 8 9 -1 -1 -1 -1 -1 -1 -1)
     #(9 10 2 0 9 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(2 3 8 2 8 10 0 1 8 1 10 8 -1 -1 -1 -1)
     #(1 10 2 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(1 3 8 9 1 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 9 1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(0 3 8 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)
     #(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1)) )

; Copies multidimensional arrays.
(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
      (make-array
         dims
         :element-type (array-element-type array)
         :displaced-to array)
      dims)))

; Calculates surface-edge intersection by interpolation
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

(defun find-triangles (expr par1 xmin xmax par2 ymin ymax par3 zmin zmax)
  (let* ((nx (get-option '$x_voxel))
         (ny (get-option '$y_voxel))
         (nz (get-option '$z_voxel))
         (dx (/ (- xmax xmin) nx))
         (dy (/ (- ymax ymin) ny))
         (dz (/ (- zmax zmin) nz))
         (fcn (coerce-float-fun (m- ($lhs expr) ($rhs expr)) `((mlist) ,par1 ,par2 ,par3)))
         (vert '())
         (px (make-array (+ nx 1) :element-type 'flonum))
         (py (make-array (+ ny 1) :element-type 'flonum))
         (pz (make-array (+ nz 1) :element-type 'flonum))
         (oldval (make-array `(,(+ nx 1) ,(+ ny 1)) :element-type 'flonum))
         (newval (make-array `(,(+ nx 1) ,(+ ny 1)) :element-type 'flonum))    )
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
                 (val (make-array 8 :element-type 'flonum
                                    :initial-contents
                                       `(,(aref oldval i j+1) ,(aref oldval i+1 j+1)
                                         ,(aref oldval i+1 j) ,(aref oldval i j)
                                         ,(aref newval i j+1) ,(aref newval i+1 j+1)
                                         ,(aref newval i+1 j) ,(aref newval i j)))))
            (when (<= (aref val 0) 0.0) (setf cubidx (logior cubidx 1)))
            (when (<= (aref val 1) 0.0) (setf cubidx (logior cubidx 2)))
            (when (<= (aref val 2) 0.0) (setf cubidx (logior cubidx 4)))
            (when (<= (aref val 3) 0.0) (setf cubidx (logior cubidx 8)))
            (when (<= (aref val 4) 0.0) (setf cubidx (logior cubidx 16)))
            (when (<= (aref val 5) 0.0) (setf cubidx (logior cubidx 32)))
            (when (<= (aref val 6) 0.0) (setf cubidx (logior cubidx 64)))
            (when (<= (aref val 7) 0.0) (setf cubidx (logior cubidx 128)))
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
                vert)))))
      ; make oldval a copy of newval
      (setf oldval (copy-array newval)))
    vert))






;; This is the function to be called at Maxima level.
;; Some examples:
;;   draw(gr2d(opt & obj))$ => a 2d plot, equivalent to draw2d(opt & obj)
;;   draw(gr3d(opt & obj))$ => a 2d plot, equivalent to draw3d(opt & obj)
;;   draw(gr2d(opt & obj),gr2d(opt & obj),gr3d(opt & obj),gr2d(opt & obj))$
;;                            => four plots in one column, one of them is a 3d plot
;;   draw(terminal=png,columns=2,gr2d(opt & obj),gr3d(opt & obj))
;;                            => png file with two plots (2d and 3d) side by side
;; See bellow for $draw2d and $draw3d
(defun $draw (&rest args)
  (cond ((member $draw_renderer '($gnuplot $gnuplot_pipes))
           (apply 'draw_gnuplot args))
        ((member $draw_renderer '($vtk $vtk6 $vtk7))
           (apply 'draw_vtk args))
        (t
           (merror "draw: unknown renderer ~M" $draw_renderer))))

;; Equivalent to draw2d(opt & obj)
(defun $draw2d (&rest args)
   ($draw (cons '($gr2d) args)) )

;; Equivalent to draw3d(opt & obj)
(defun $draw3d (&rest args)
  (cond ((member $draw_renderer '($gnuplot $gnuplot_pipes))
           (draw_gnuplot (cons '($gr3d) args)))
        ((member $draw_renderer '($vtk $vtk6 $vtk7))
           (draw_vtk (cons '($gr3d) args)))
        (t
           (merror "draw: unknown renderer ~M" $draw_renderer))))

