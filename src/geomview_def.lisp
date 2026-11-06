;; gnuplot.lisp: routines for Maxima's interface to gnuplot
;; Copyright (C) 2021 J. Villate
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA  02110-1301, USA

(in-package :maxima)

(defun geomview-palette (palette)
"Given a valid palette, it returns its definition for Geomview."
(let (colors)
  (if (atom palette)
    (if (getf *plot-palettes* palette)
      (setf colors (mapcar #'hex-to-numeric-list (mapcar #'rgb-color
                           (cdr (getf *plot-palettes* palette)))))
      (unless (setf colors (rgb-color palette)) (setf colors "#ffffff")))
    (case
     (car palette)
     ($gradient
      (setf colors
            (mapcar #'hex-to-numeric-list 
                    (mapcar #'rgb-color (cdr palette)))))
     (($hue $saturation $value)
      (let ((h (second palette)) (s (third palette)) (v (fourth palette))
            (d (/ (fifth palette) 6.0)))
        (push (hex-to-numeric-list ($hsv h s v)) colors)
        (case (car palette)
              ($hue
               (dotimes (i 6)
                 (setq h (mod (+ h d) 1))
                 (push (hex-to-numeric-list ($hsv h s v)) colors)))
              ($saturation
               (dotimes (i 6)
                 (setq s (mod (+ s d) 1))
                 (push (hex-to-numeric-list ($hsv h s v)) colors)))
              ($value
               (dotimes (i 6)
                 (setq v (mod (+ v d) 1))
                 (push (hex-to-numeric-list ($hsv h s v)) colors))))
        (setq colors (reverse colors))))
     (otherwise
      (setf colors
            (mapcar #'hex-to-numeric-list (mapcar #'rgb-color palette))))))
  colors))

(defun geomview-z-color (z colors)
"Interpolate within a list of colors, given as lists with three elements
between 0 and 1 (red, blue and green), according to the value of z,
between 0 and 1, where 0 gives the first color and 1 the last one."
  (let* ((l (length colors)) n r c1 c2)
    (multiple-value-setq (n r) (floor (* z (1- l))))
    (if (= n (1- l))
      (nth n colors)
      (progn
        (setq c1 (nth n colors))
        (setq c2 (nth (1+ n) colors))
        (mapcar #'+ c1 (mapcar #'(lambda (x) (* r x)) (mapcar #'- c2 c1)))))))

(defmethod plot-preamble ((plot geomview-plot) options)
  (let ((bgcolor (getf options '$background_color)))
    (setf (slot-value plot 'data)
        (with-output-to-string
          (st)
          ;; record Maxima version and date-time
          (format st "# Created by Maxima ~a~%" *autoconf-version*)
          (format st "# ~a~%" ($timedate))
          (format st "(ui-panel tools off)~%(bbox-draw targetgeom no)~%")
          (if bgcolor
            (format st "(backcolor targetcam  ~{~,6f~^ ~})~%"
                    (hex-to-numeric-list (rgb-color bgcolor)))
            (format st "(backcolor targetcam 1 1 1)~%"))
          (format st "(camera targetcam {perspective 0})~%")
          (format st "(geometry plot3d {~%")
          (format st "INST~%transform~%")
          (geomview-matrix
           st (getf options '$azimuth) (getf options '$elevation)))))
  (values))

(defmethod plot3d-command ((plot geomview-plot) functions options titles)
  (declare (ignore titles))
  (let ((i 0) colors (palette (getf options '$palette))
        (nxint (first (getf options '$grid)))
        (nyint (second (getf options '$grid)))
        (meshcolor (getf options '$mesh_lines_color))
        (lighting (getf options '$lighting)))
    (when palette (setf colors (geomview-palette palette)))
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string (st)
        ;; generate the mesh points for each surface in the functions stack
        (dolist (f functions)
          (let* ((fun (first f)) (xrange (second f)) (yrange (third f))
                 (xvar (second xrange)) (yvar (second yrange))
                 (x0 (third xrange)) (x1 (fourth xrange))
                 (y0 (third yrange)) (y1 (fourth yrange))
                 lvars trans type faces texture (material ""))
            (incf i)
            (format st "geom {~%LIST~%{~%")
            (if palette
              (progn
                (if (listp colors)
                  (setf type "COFF")
                  (progn
                    (setf type "OFF")
                    (setf material
                          (format nil "diffuse ~{~,6f~^ ~}"
                                  (hex-to-numeric-list colors)))))
                (if lighting
                  (setf texture "+smooth")
                  (setf texture "+csmooth"))
               (when meshcolor
                  (setf texture (concatenate 'string texture " +edge"))
                  (setf material
                      (concatenate 'string material
                         (format nil " edgecolor ~{~,6f~^ ~}"
                             (hex-to-numeric-list (rgb-color meshcolor)))))))
              (progn
                (setf type "OFF")
                (setf texture "-face +edge")
                (if meshcolor
                  (setf material
                      (concatenate 'string material
                         (format nil " edgecolor ~{~,6f~^ ~}"
                             (hex-to-numeric-list (rgb-color meshcolor)))))
                  (concatenate 'string material " edgecolor 0 0 0"))))
            (format st "appearance { ~a material {~a} {linewidth 2}}~%"
                    texture material)
            (if ($listp fun)
              (progn
                (setq lvars `((mlist) ,xvar ,yvar $z))
                (setq trans
                      ($make_transform lvars (second fun) (third fun)
                                       (fourth fun)))
                (setq fun '$zero_fun))
              (progn
                (setq lvars `((mlist) ,xvar ,yvar))
                (setq fun (coerce-float-fun fun lvars))
                (check-3dfun fun
                             (+ x0 (/ (- x1 x0) 2)) (+ y0 (/ (- y1 y0) 2)))))
            (multiple-value-bind
             (points points-count)
             (draw3d fun x0 x1 y0 y1 nxint nyint)
             (when trans (mfuncall trans points))
             (when (getf options '$transform_xy)
               (mfuncall (getf options '$transform_xy) points))
             (setq faces (polygons (vertices-matrix points nxint nyint)))
             ;; Finds the minimum and maximum values of x, y and z
             (let* ((maxx +most-negative-flonum+) (minx +most-positive-flonum+)
                    (maxy maxx) (miny minx) (maxz maxx) (minz minx))
               (loop for i below (length points) by 3 do
                     (let ((x (elt points i)) (y (elt points (1+ i)))
                           (z (elt points (+ i 2))))
                       (when (floatp x)
                         (if (< x minx) (setq minx x))
                         (if (> x maxx) (setq maxx x)))
                       (when (floatp y)
                         (if (< y miny) (setq miny y))
                         (if (> y maxy) (setq maxy y)))
                       (when (floatp z)
                         (if (< z minz) (setq minz z))
                         (if (> z maxz) (setq maxz z)))))
               ;; Outputs the points coordinates, normalized from 0 to 1
               (format st type)
               (format st " ~a ~a 0~%" points-count (length faces))
               (let ((rangex (- maxx minx)) (rangey (- maxy miny))
                     (rangez (- maxz minz)))
                 (loop for i below (length points) by 3 do
                       (let (pt)
                         (dotimes (n 3) (push (elt points (+ i n)) pt))
                         (when (= (length (remove t pt)) 3)
                           (setf pt (list
                                     (/ (- (third pt) minx) rangex)
                                     (/ (- (second pt) miny) rangey)
                                     (/ (- (first pt) minz) rangez)))
                          (format st "~{~,6f~^ ~}" pt)
                          (if (string= type "COFF")
                            (format st " ~{~,6f ~}1~%"
                                    (geomview-z-color (third pt) colors))
                            (format st "~%"))))))
              (dolist (face faces)
                 (format st "~d ~{~d~^ ~}~%" (length face) face)))
             (format st "}~%"))))
        (unless (and (member 'mbox options) (not (getf options 'mbox)))
          (geomview-bbox st))
        (format st "}~%})~%(zoom targetcam 1.5)~%"))))))

(defmethod plot-shipout ((plot geomview-plot) options &optional output-file)
  (declare (ignore options))
  (let ((file (plot-file-path (temp-file-name "maxout" "gcl"))))
    (with-open-file (fl
                     #+sbcl (sb-ext:native-namestring file)
                     #-sbcl file
                     :direction :output :if-exists :supersede)
                    (format fl "~a" (slot-value plot 'data)))
    ($system $geomview_command
             #-(or (and sbcl win32) (and sbcl win64) (and ccl windows))
             (format nil " ~s &" file)
             #+(or (and sbcl win32) (and sbcl win64) (and ccl windows))
             file)
    (cons '(mlist) (cons file output-file))))

(defun check-3dfun (fun x y)
"Evaluate FUN at the middle point of the range.
Looking at a single point is somewhat unreliable.
Call FUN with numerical arguments (symbolic arguments may
fail due to trouble computing real/imaginary parts for 
complicated expressions, or it may be a numerical function)"
  (when (cdr ($listofvars (mfuncall fun x y)))
    (mtell
     (intl:gettext
      "plot3d: expected <expr. of v1 and v2>, [v1,min,max], [v2,min,max]~%"))
    (mtell
     (intl:gettext
      "plot3d: keep going and hope for the best.~%"))))
         
(defun geomview-matrix (st az el)
  (if (and (floatp az) (floatp el))
    (let* ((p (coerce-float '$%pi)) (a (* (/ az 180d0) p))
           (e (* (/ el 180d0) p)) (ca (cos a)) (sa (sin a))
           (ce (cos e)) (se (sin e)))
      (format st "{~,7f ~,7f ~,7f 0~%" ca (- (* sa ce)) (* sa se))
      (format st " ~,7f ~,7f ~,7f 0~%" sa (* ca ce) (- (* ca se)))
      (format st " 0 ~,7f ~,7f 0~%" se ce)
      (format st " 0 0 0 1}~%"))
    (progn 
      (format st "{0.8660254 -0.25 0.4330127 0~%")
      (format st " 0.5 0.4330127 -0.75 0~%")
      (format st " 0 0.8660254 0.5 0~%")
      (format st " 0 0 0 1}~%"))))
 
(defun geomview-bbox (st)
  (format st "# bounding box~%LIST~%{ appearance {linewidth 2}~%VECT~%")
  (format st "4 16 0 10 2 2 2 0 0 0 0~%")
  (format st "0 0 0 0 1 0 1 1 0 1 0 0 0 0 0~%")
  (format st "0 0 1 1 0 1 1 1 1 0 1 1 0 0 1~%")
  (format st "0 1 0 0 1 1 1 0 0 1 0 1 1 1 0 1 1 1~%}~%")
  (format st "# x, y and z labels~%LIST~%{ appearance {linewidth 2}~%VECT~%")
  (format st "5 13 0 2 2 2 3 4 0 0 0 0 0~%")
  (format st "# letter x~%")
  (format st "0.47 0 -0.03 0.53 0 -0.09 0.53 0 -0.03 0.47 0 -0.09~%")
  (format st "# letter y~%")
  (format st "0 0.5 -0.06 0 0.5 -0.09 0 0.47 -0.03 0 0.5 -0.06 0 0.53 -0.03~%")
  (format st "# letter z~%")
  (format st "-0.09 0 0.53 -0.03 0 0.53 -0.09 0 0.47 -0.03 0 0.47~%}~%"))
