;; visualization.lisp
;;   
;; Copyright (c) 2011-2014, Jaime E. Villate <villate@fe.up.pt>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA  02110-1301, USA.
;;

(in-package :maxima)

;; translates Maxima scene object's option values into Xmaxima input syntax
(defun tcl-vtk-option-value (name values)
  (when ($listp (car values)) (setq values (cdar values)))
  (with-output-to-string (st)
    (format st "{~a" name)
    (dolist (num values)
      (cond
        ((plotcolorp num)
         (format st "~{ ~d~}" (hexrgb-to-decimal (rgb-color num)))) 
        ((integerp num) (format st " ~d" num))
        ((floatp num) (format st " ~f" num))
        (($numberp num) (format st " ~f " (coerce-float num)))
        ((and ($constantp num) ($freeof '$%i num) (not (member num '(t nil)))
              (not ($listp num)))
         (format st " ~f " (coerce-float num)))
        (($listp num) (format st "~a" (tcl-output-number-list num)))
        (t
         (merror
          (intl:gettext "scene: Wrong value for option ~M~%Expecting a number; found: ~M")
          name num))))
    (format st "} ")))

;; converts a Maxima list into a floating-point Tcl list string
(defun tcl-output-number-list (maxlist)
  (with-output-to-string (st)
    (format st "{ ")
    (dolist (num (rest maxlist))
      (cond
        ((floatp num) (format st "~f " num))
        (($numberp num) (format st "~f " (coerce-float num)))
        ((and ($constantp num) ($freeof '$%i num) (not (member num '(t nil)))
              (not ($listp num)))
         (format st "~f " (coerce-float num)))
        (($listp num) (format st "~a" (tcl-output-number-list num)))
        (t
         (merror
          (intl:gettext "scene: Wrong value in animation list: ~M")
          num))))
    (format st "} ")))

;; converts a hexadecimal rgb color string into list of numbers from 0 to 1
(defun hexrgb-to-decimal (color)
  (list (/ (parse-integer (subseq color 1 3) :radix 16) 255.0)
        (/ (parse-integer (subseq color 3 5) :radix 16) 255.0)
        (/ (parse-integer (subseq color 5) :radix 16) 255.0)))

;; parses a scene option into a command-line option passed to Xmaxima
(defun scene-option-to-tcl (option)
  (let (v vv (name (car option)))
    (case name
      ($azimuth (if (cadr option)
                    (setf (cadr option) (parse-azimuth (cadr option))))
                (setq v (check-option option #'realp "a real number" 1)))
      ($elevation (if (cadr option)
                      (setf (cadr option) (parse-elevation (cadr option))))
                  (setq v (check-option option #'realp "a real number" 1)))
      ($tstep
       (setq v (check-option option #'realp "a real number" 1))
       (setq option (list name v)))
      (($width $height)
       (setq v (check-option option #'naturalp "a natural number" 1))
       (setq option (list name v)))
      ($restart
       (setq v (check-option-boole option))
       (setq option (list name (if v 1 0))))
      ($background
       (setq v (check-option option #'plotcolorp "a color"))
       (setq option (cons name (hexrgb-to-decimal (rgb-color v)))))
      (($windowtitle $windowname)
       (setq v (check-option option #'string "a string" 1))
       (setq option (list name v)))
      (t (merror (intl:gettext "scene: Unknown option ~M") name)))
    (setq vv (mapcar #'(lambda (a) (if (symbolp a) (ensure-string a) a)) option))
    (with-output-to-string (st)
      (format st "-~(~a~) " (first vv))
      (format st "{~{~a~^ ~}}" (rest vv)))))

(defun $scene (&rest arguments)
  (let (objects options file (objs "") (opts " ") vtkname
                (lf (format NIL "~%"))
        (classes '(($cube . "Cube") ($sphere . "Sphere")
                     ($cylinder . "Cylinder") ($cone . "Cone")))
        ;; VTK methods for the objects in classes
        (cmethods '(($center . "Center") ($radius . "Radius")
                    ($height . "Height") ($resolution . "Resolution")
                    ($latlongtessellation . "LatLongTessellation")
                    ($thetaresolution . "ThetaResolution")
                    ($phiresolution . "PhiResolution")
                    ($starttheta . "StartTheta") ($endtheta . "EndTheta")
                    ($startphi . "StartTheta") ($endphi . "EndTheta")
                    ($capping . "Capping") ($direction . "Direction")
                    ($xlength . "XLength") ($ylength . "YLength")
                    ($zlength . "ZLength") ($bounds . "Bounds")
                    ($angle . "Angle")))
        ;; VTK methods for properties
        (pmethods '(($color . "Color") ($opacity . "Opacity")
                    ($ambient . "Ambient") ($ambientcolor . "AmbientColor")
                    ($specular . "Specular") ($specularcolor . "SpecularColor") 
                    ($diffuse . "Diffuse") ($diffusecolor . "DiffuseColor")
                    ($edgevisibility . "EdgeVisibility")
                    ($edgecolor . "EdgeColor") ($linewidth . "LineWidth")
                    ($pointsize . "PointSize") ($lightning . "Lightning")
                    ($shading . "Shading") ($texture . "Texture")
                    ($representation . "Representation")
                    ($points . "RepresentationToPoints")
                    ($wireframe . "RepresentationToWireframe")
                    ($surface . "RepresentationToSurface")
                    ($interpolation . "Interpolation")
                    ($flat . "InterpolationToFlat")
                    ($gourand . "InterpolationToGourand")
                    ($phong . "InterpolationToPhong")
                    ($stipplepattern . "LineStipplePattern")
                    ($stipplerepeat . "LineStippleRepeatFactor")
                    ($frontculling . "FrontFaceCulling")
                    ($backculling . "BackFaceCulling")))
        ;; VTK methods for actors
        (amethods '(($origin . "Origin") ($scale . "Scale")
                    ($position . "Position") ($orientation . "Orientation")
                    ($usertransform . "UserTransform"))))
    ;; separates arguments between objects and options
    (dolist (v arguments)
      (if (listp v) (setq v (cdr v)) (setq v (list v))) 
      (if (assoc (car v) classes)
          (setq objects (append objects (list v)))
          (setq options (append options (list v)))))
    ;; sets up output file name to pass to Xmaxima
    (setq file (plot-temp-file (format nil "maxout~d.xmaxima" (getpid))))

    ;; parses objects
    (dolist (v objects)
      (let ((copts "") (popts "") (aopts "") animate prop)
        (setq vtkname (cdr (assoc (car v) classes)))
        ;; parses object properties 
        (dolist (w (cdr v))
          (unless ($listp w)
            (merror
             (intl:gettext "scene: Wrong option; expecting a list; found:  ~M")
             w))
          (cond
            ((setq prop (cdr (assoc (second w) cmethods)))
             (setq copts
                   (concatenate 'string copts
                                (tcl-vtk-option-value prop (cddr w)))))
            ((setq prop (cdr (assoc (second w) pmethods)))
             (setq popts
                   (concatenate 'string popts
                   (tcl-vtk-option-value prop (cddr w)))))
            ((setq prop (cdr (assoc (second w) amethods)))
             (setq aopts
                   (concatenate 'string aopts
                                (tcl-vtk-option-value prop (cddr w)))))
            ((eql (second w) '$animate)
             (unless (setq prop (cdr (assoc (third w) amethods)))
               (merror (intl:gettext "scene: ~M cannot be animated.")
                       (third w)))
             (setq animate
                   (concatenate 'string "{" prop " 0 "
                          (tcl-output-number-list (fourth w)) "}")))
            ((eql (second w) '$track)
             (setq animate
                   (concatenate 'string "{Position 1 "
                          (tcl-output-number-list (third w)) "}")))
            (t (mtell (intl:gettext "scene: Ignored option: ~M")
                      (second w)))))
        ;; save object name and properties in string objs
        (setq objs 
              (concatenate 'string objs "{" vtkname lf "{" copts "}" lf
                           "{" popts "}" lf "{" aopts "}" lf animate "}" lf))))

    ;; parse scene options and copy them to string opts
    (cond (options
           (dolist (v options) 
             (setq opts (concatenate 'string opts " "
                                     (scene-option-to-tcl v))))))
    (show-open-plot
     (with-output-to-string (st)
                  (cond ($show_openplot
                         (format st "scene ~a -objects {~a}~%" opts objs))
                        (t (format st "{scene ~a -objects {~a}}" opts objs))))
     file)))

