;;;                 COPYRIGHT NOTICE
;;;  
;;;  Copyright (C) 2011 Mario Rodriguez Riotorto
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

;;; This is a maxima-vtk interface.

;;; Visit
;;; http://riotorto.users.sf.net/vtk
;;; for examples

;;; For questions, suggestions, bugs and the like, feel free
;;; to contact me at
;;; mario @@@ edu DOT xunta DOT es


($put '$vtk 1 '$version)


;;; AUXILIARY FUNCTIONS

;; Global variables
(defvar *vtk-source-counter* 0)
(defvar *vtk-mapper-counter* 0)
(defvar *vtk-actor-counter* 0)
(defvar *vtk-trans-counter* 0)
(defvar *vtk-filter-counter* 0)
(defvar *vtk-data-file-counter* 0)
(defvar *vtk-points-counter* 0)
(defvar *vtk-polydata-counter* 0)
(defvar *vtk-cellarray-counter* 0)
(defvar *vtk-polydatamapper-counter* 0)
(defvar *vtk-solidsource-counter* 0)
(defvar *vtk-triangle-counter* 0)
(defvar *vtk-label-counter* 0)
(defvar *vtk-tube-counter* 0)
(defvar *lookup-tables* nil)
(defvar *unitscale-already-defined* nil)
(defvar *label-actors* nil)

(defun get-source-name ()
  (format nil "source~a" (incf *vtk-source-counter*)))

(defun get-mapper-name ()
  (format nil "mapper~a" (incf *vtk-mapper-counter*)))

(defun get-actor-name ()
  (format nil "actor~a" (incf *vtk-actor-counter*)))

(defun get-trans-name ()
  (format nil "trans~a" (incf *vtk-trans-counter*)))

(defun get-filter-name ()
  (format nil "filter~a" (incf *vtk-filter-counter*)))

(defun get-data-file-name ()
  (format nil "data~a.vtk" (incf *vtk-data-file-counter*)))

(defun get-points-name ()
  (format nil "points~a" (incf *vtk-points-counter*)))

(defun get-polydata-name ()
  (format nil "polydata~a" (incf *vtk-polydata-counter*)))

(defun get-cellarray-name ()
  (format nil "cellarray~a" (incf *vtk-cellarray-counter*)))

(defun get-polydatamapper-name ()
  (format nil "polydatamapper~a" (incf *vtk-polydatamapper-counter*)))

(defun get-solidsource-name ()
  (format nil "solidsource~a" (incf *vtk-solidsource-counter*)))

(defun get-triangle-name ()
  (format nil "triangle~a" (incf *vtk-triangle-counter*)))

(defun get-label-name ()
  (setf *label-actors* (cons *vtk-actor-counter* *label-actors*))
  (format nil "label~a" (incf *vtk-label-counter*)))

(defun get-tube-name ()
  (format nil "tube~a" (incf *vtk-tube-counter*)))

(defun vtktransform-code (tn)
  (format nil "vtkTransform ~a~%" tn))

(defun vtktransformpolydatafilter-code (fn sn tn)
  (concatenate 'string
    (format nil "vtkTransformPolyDataFilter ~a~%" fn)
    (format nil "  ~a SetInput [~a GetOutput]~%" fn sn)
    (format nil "  ~a SetTransform ~a~%" fn tn) ))

(defun vtktransformfilter-code (fn sn tn)
  (concatenate 'string
    (format nil "vtkTransformFilter ~a~%" fn)
    (format nil "  ~a SetInput [~a GetOutput]~%" fn sn)
    (format nil "  ~a SetTransform ~a~%" fn tn) ))

(defun vtkpolydatamapper-code (mn fn &optional tube)
  (concatenate 'string
    (format nil "vtkPolyDataMapper ~a~%" mn)
    (if tube
      (format nil "  ~a SetInputConnection [~a GetOutputPort]~%" mn fn)
      (format nil "  ~a SetInput [~a GetOutput]~%" mn fn) )))

(defun vtkactor-code (an mn col)
  (let ((colist (hex-to-numeric-list col)))
    (concatenate 'string
      (format nil "vtkActor ~a~%" an)
      (format nil "  ~a SetMapper ~a~%" an mn)
      (format nil "  [~a GetProperty] SetColor ~a ~a ~a~%"
              an
              (first  colist)
              (second colist)
              (third  colist))
      (format nil "  [~a GetProperty] SetOpacity ~a~%"
              an
              (gethash '$opacity *gr-options*) )
      (format nil "  [~a GetProperty] SetLineWidth ~a~%~%"
              an
              (gethash '$line_width *gr-options*)) )))

(defun vtktubefilter-code (tn fn lt)
  (concatenate 'string
    (format nil "vtkTubeFilter ~a~%" tn)
    (format nil "  ~a SetInputConnection [~a GetOutputPort]~%" tn fn)
    (format nil "  ~a SetNumberOfSides ~a~%" tn (- lt))
    (format nil "  ~a SetRadius ~a~%" tn (gethash '$line_width *gr-options*)) ))








; Checks if lookup table is already created.
; Returns its position number in *lookup-tables*
(defun lookup-table-exists (pal)
  (let (pos)
    (setf pos (position pal *lookup-tables* :test #'equal))
    (when (null pos)
      (setf pos -1))
    (1+ pos)))


; Writes tcl code for color transform functions
(defun color-transform-function (c n f)
  (let (pf expr)
    (if (< f 0)
      (setf pf (- f))
      (setf pf f))
    (case pf
      (0  (setf expr "set x 0"))
      (1  (setf expr "set x 0.5"))
      (2  (setf expr "set x 1"))
      (3  (setf expr "set x $x"))
      (4  (setf expr "set x [expr $x * $x]"))
      (5  (setf expr "set x [expr $x * $x * $x]"))
      (6  (setf expr "set x [expr $x * $x * $x * $x]"))
      (7  (setf expr "set x [expr sqrt($x)]"))
      (8  (setf expr "set x [expr sqrt(sqrt($x))]"))
      (9  (setf expr "set x [expr sin(1.570796326794897 * $x)]")) ; %pi/2
      (10 (setf expr "set x [expr cos(1.570796326794897 * $x)]"))
      (11 (setf expr "set x [expr abs($x - 0.5)]"))
      (12 (setf expr "set x [expr (2.0 * $x - 1.0) * (2.0 * $x - 1.0)]"))
      (13 (setf expr "set x [expr sin(3.141592653589793 * $x)]")) ; %pi
      (14 (setf expr "set x [expr abs(cos(3.141592653589793 * $x))]"))
      (15 (setf expr "set x [expr sin(6.283185307179586 * $x)]")) ; 2*%pi
      (16 (setf expr "set x [expr cos(6.283185307179586 * $x)]"))
      (17 (setf expr "set x [expr abs(sin(6.283185307179586 * $x))]"))
      (18 (setf expr "set x [expr abs(cos(6.283185307179586 * $x))]"))
      (19 (setf expr "set x [expr abs(sin(12.56637061435917 * $x))]")) ; 4*%pi
      (20 (setf expr "set x [expr abs(cos(12.56637061435917 * $x))]"))
      (21 (setf expr "set x [expr 3.0 * $x]"))
      (22 (setf expr "set x [expr 3.0 * $x - 1.0]"))
      (23 (setf expr "set x [expr 3.0 * $x - 2.0]"))
      (24 (setf expr "set x [expr abs(3.0 * $x - 1.0)]"))
      (25 (setf expr "set x [expr abs(3.0 * $x - 2.0)]"))
      (26 (setf expr "set x [expr 1.5 * $x - 0.5]"))
      (27 (setf expr "set x [expr 1.5 * $x - 1.0]"))
      (28 (setf expr "set x [expr abs(1.5 * $x - 0.5)]"))
      (29 (setf expr "set x [expr abs(1.5 * $x - 1.0)]"))
      (30 (setf expr "set x [expr [interval $x 0.25 0.57] / 0.32 - 0.78125]"))
      (31 (setf expr "set x [expr 2 * [interval $x 0.42 0.92] - 0.84]"))
      (32 (setf expr (concatenate 'string
                       (format nil "  if {$x <= 0.42} {~%")
                       (format nil "    set x [expr 4.0 * $x]~%")
                       (format nil "    } elseif {$x <= 0.92} {~%")
                       (format nil "    set x [expr -2.0 * $x + 1.84]~%")
                       (format nil "    } else {~%")
                       (format nil "    set x [expr $x / 0.08 - 11.5]}"))))
      (33 (setf expr "set x [expr abs(2.0 * $x - 0.5)]"))
      (34 (setf expr "set x [expr 2 * $x]"))
      (35 (setf expr "set x [expr 2.0 * $x - 0.5]"))
      (36 (setf expr "set x [expr 2.0 * $x - 1.0]")))
    (concatenate 'string
      (format nil "proc f~a~a {k} {~%" c n)
      (format nil "  set x [unitscale $k]~%" )
      (format nil "  ~a~%" expr)
      (format nil "  return [interval $x 0 1] }~%~%"))   ))

; Creates lookup table. See info for option 'palette'.
; Returns list with lookup table name and the string.
(defun check-lookup-table ()
  (let ((palette (gethash '$palette *gr-options*))
        (lut "")
        pos palette-name lutn)
    (cond ((equal palette '$gray)
             (setf palette '(3 3 3)))
          ((equal palette '$color)
             (setf palette '(7 5 15))) )
    (setf pos (lookup-table-exists palette))
    (setf lutn (1+ (length *lookup-tables*)))
    (setf *lookup-tables* (append *lookup-tables* (list palette)))
    (setf palette-name (format nil "lut~a" lutn))
    (cond ((> pos 0)  ; lookup table already defined
             (list (format nil "lut~a" pos)
                   nil))
          ((and (listp palette)  ; build lookup table with transform functions
                (= (length palette) 3)
                (every #'(lambda (x) (and (integerp x) (<= (abs x) 36))) palette) )
             ; if *unitscale-already-defined* is null, write
             ; tcl functions 'unitscale' and 'unitinterval'
             (when (null *unitscale-already-defined*)
               (setf lut
                 (concatenate 'string
                   (format nil "proc unitscale {k} {~%")
                   (format nil "  set n 256.0~%")
                   (format nil "  return [expr $k/($n-1)] }~%~%")
                   (format nil "proc interval {x x0 x1} {~%")
                   (format nil "  if { $x <= $x0} {return 0}~%")
                   (format nil "  if { $x >= $x1} {return 1}~%")
                   (format nil "  return $x }~%~%")))
                 (setf *unitscale-already-defined* t))
             ; write tcl r-g-b transform functions
             (setf lut
               (concatenate 'string
                 lut
                 (color-transform-function "R" lutn (car palette))
                 (color-transform-function "G" lutn (cadr palette))
                 (color-transform-function "B" lutn (caddr palette))))
             ; create lookup table
             (setf lut
                   (concatenate 'string
                     lut
                     (format nil "vtkLookupTable ~a~%" palette-name)
                     (format nil "  ~a SetNumberOfColors 256~%" palette-name)
                     (format nil "  ~a Build~%" palette-name)
                     (format nil "  for {set i 0} {$i<256} {incr i 1} {~%")
                     (format nil "    eval ~a SetTableValue $i [fR~a $i] [fG~a $i] [fB~a $i] 1  }~%~%"
                             palette-name lutn lutn lutn)))
             (list palette-name lut))

          ((and (listp palette)  ; build user defined lookup table without transparency
             (every #'(lambda (x) (and (listp x) (= (length x) 3))) palette) )
            (list
              palette-name
              (let (triplete
                    (n (length palette)))
                (with-output-to-string (stream)
                  (format stream "vtkLookupTable ~a~%" palette-name)
                  (format stream "  ~a SetNumberOfColors ~a~%" palette-name n)
                  (dotimes (k n)
                    (setf triplete (nth k palette))
                    (format stream "  ~a SetTableValue ~a ~a ~a ~a 1~%"
                            palette-name k (car triplete) (cadr triplete) (caddr triplete)))))))

          ((and (listp palette)  ; build user defined lookup table with transparency
             (every #'(lambda (x) (and (listp x) (= (length x) 4))) palette) )
            (list
              palette-name
              (let (triplete
                    (n (length palette)))
                (with-output-to-string (stream)
                  (format stream "vtkLookupTable ~a~%" palette-name)
                  (format stream "  ~a SetNumberOfColors ~a~%" palette-name n)
                  (dotimes (k n)
                    (setf triplete (nth k palette))
                    (format stream "  ~a SetTableValue ~a ~a ~a ~a ~a~%"
                            palette-name k (car triplete) (cadr triplete) (caddr triplete) (cadddr triplete))))))))))






;;; FILE WRITERS


;; Writes data file for objects parametric_surface, explicit and elevation grid
(defmacro write-polydata-file ()
  '(let ()
    ; write vtk data file
    (setf data-file-path (plot-temp-file data-file-name))
    (setf datastorage
      (open data-file-path
            :direction :output :if-exists :supersede))
    (format datastorage "~a~%~a~%~a~%~a~%~a ~a ~a~%"
      "# vtk DataFile Version 2.0"
      "  File generated by Maxima's draw package."
      "ASCII"
      "DATASET POLYDATA"
      "POINTS " (* nx ny) "float")
    (dotimes (k (array-dimension result 0))
      (format datastorage "~a " (aref result k)))
    (format datastorage
            "~%~%POLYGONS ~a ~a~%"
            (* (1- nx) (1- ny))
            (* 5 (1- nx) (1- ny)))
    (dotimes (f (1- ny))
      (setf result-count (* f nx))
      (dotimes (c (1- nx))
        (format datastorage
                "4 ~a ~a ~a ~a~%"
                (+ result-count c)
                (+ result-count c 1)
                (+ result-count nx c 1)
                (+ result-count nx c))))
    (format datastorage "~%~%")

    ; write scalars for lookup table when enhanced3d is active
    (when (> *draw-enhanced3d-type* 0)
      (format datastorage "~a~a~%~a~a~a~%~a~%"
        "POINT_DATA " (* nx ny)
        "SCALARS " data-file-name " float"
        "LOOKUP_TABLE default")
      (dotimes (k (array-dimension scalars 0))
        (format datastorage "~a " (* slope (- (aref scalars k) minscalar)))))
    (close datastorage)))


;; Generates code for objects parametric_surface, explicit and elevation grid
(defmacro write-polydata-reader ()
 '(let ()
    ; lookup table
    (when (> *draw-enhanced3d-type* 0)
      (let ((lut (check-lookup-table)))
        (setf lookup-table-name (car lut))
        (setf output-string (cadr lut))))
    ; return tcl-vtk code
    (concatenate 'string
      output-string
      (format nil "vtkPolyDataReader ~a~%" source-name)
      (format nil "  ~a SetFileName \"~a\"~%" source-name data-file-path)
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name)
      (vtkpolydatamapper-code mapper-name filter-name)
      (if (> *draw-enhanced3d-type* 0)
        (format nil "  ~a SetLookupTable ~a~%" mapper-name lookup-table-name)
        "")
      (vtkactor-code actor-name mapper-name color))))


;; Writes data file for object implicit
(defmacro write-polydata-file-triangles ()
  '(let* ((num-points (/ (array-dimension result 0) 3))
          (num-triangles (/ num-points 3))
          (count -1) )
    ; write vtk data file
    (setf data-file-path (plot-temp-file data-file-name))
    (setf datastorage
      (open data-file-path
            :direction :output :if-exists :supersede))
    (format datastorage "~a~%~a~%~a~%~a~%~a ~a ~a~%"
      "# vtk DataFile Version 2.0"
      "  File generated by Maxima's draw package."
      "ASCII"
      "DATASET POLYDATA"
      "POINTS " num-points "float")
    (dotimes (k (array-dimension result 0))
      (format datastorage "~a " (aref result k)))
    (format datastorage
            "~%~%POLYGONS ~a ~a~%"
            num-triangles
            (* 4 num-triangles))
    (dotimes (tri num-triangles)
      (format datastorage
              "3 ~a ~a ~a~%" (incf count) (incf count) (incf count)))
    (format datastorage "~%~%")
    ; write scalars for lookup table when enhanced3d is active
    (when (> *draw-enhanced3d-type* 0)
      (format datastorage "~a~a~%~a~a~a~%~a~%"
        "POINT_DATA " (array-dimension scalars 0)
        "SCALARS " data-file-name " float"
        "LOOKUP_TABLE default")
      (dotimes (k (array-dimension scalars 0))
        (format datastorage "~a " (* slope (- (aref scalars k) minscalar)))))
    (close datastorage)))





;;; OBJECT BUILDERS

;; cone(height, radius, center, direction, resolution, capping)
;; ------------------------------------------------------------
(defun vtk3d-cone (hei rad cen dir res cap)
  (let ((color (gethash '$color *gr-options*))
        (fhei ($float hei))
        (frad ($float rad))
        (fcen ($float cen))
        (fdir ($float dir))
        (source-name (get-source-name))
        (mapper-name (get-mapper-name))
        (actor-name  (get-actor-name))
        (trans-name  (get-trans-name))
        (filter-name (get-filter-name))
        capn)
    (when (or (not (floatp fhei))
              (< fhei 0.0))
          (merror "vtk3d: cone height must be a real equal or greater than zero"))
    (when (or (not (floatp frad))
              (< frad 0.0))
          (merror "vtk3d: cone radius must be a real equal or greater than zero"))
    (when (or (not ($listp fcen))
              (not (= ($length fcen) 3))
              (not (every #'floatp (rest fcen))) )
          (merror "vtk3d: cone center must be a list of three floats"))
    (when (or (not ($listp fdir))
              (not (= ($length fdir) 3))
              (not (every #'floatp (rest fdir))) )
          (merror "vtk3d: cone direction must be a list of three floats"))
    (when (or (not (integerp res))
              (< res 0))
          (merror "vtk3d: cone resolution must be a positive integer"))
    (when (and (not (equal cap nil))
              (not (equal cap t)) )
          (merror "vtk3d: cone capping must be 'true' or 'false'"))
    (if cap
        (setf capn 1)
        (setf capn 0))
    (concatenate 'string
      (format nil "vtkConeSource ~a~%" source-name)
      (format nil "  ~a SetHeight ~a~%" source-name fhei)
      (format nil "  ~a SetRadius ~a~%" source-name frad)
      (format nil "  ~a SetCenter ~a ~a ~a~%"
              source-name
              (cadr fcen)
              (caddr fcen)
              (cadddr fcen))
      (format nil "  ~a SetDirection ~a ~a ~a~%"
              source-name
              (cadr fdir)
              (caddr fdir)
              (cadddr fdir))
      (format nil "  ~a SetResolution ~a~%" source-name res)
      (format nil "  ~a SetCapping ~a~%" source-name capn)
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name)
      (vtkpolydatamapper-code mapper-name filter-name)
      (vtkactor-code actor-name mapper-name color)  )))



;; cylinder(height, radius, center, resolution, capping)
;; -----------------------------------------------------
(defun vtk3d-cylinder (hei rad cen res cap)
  (let ((color (gethash '$color *gr-options*))
        (fhei ($float hei))
        (frad ($float rad))
        (fcen ($float cen))
        (source-name (get-source-name))
        (mapper-name (get-mapper-name))
        (actor-name  (get-actor-name))
        (trans-name  (get-trans-name))
        (filter-name (get-filter-name))
        capn)
    (when (or (not (floatp fhei))
              (< fhei 0.0))
          (merror "vtk3d: cylinder height must be a real equal or greater than zero"))
    (when (or (not (floatp frad))
              (< frad 0.0))
          (merror "vtk3d: cylinder radius must be a real equal or greater than zero"))
    (when (or (not ($listp fcen))
              (not (= ($length fcen) 3))
              (not (every #'floatp (rest fcen))) )
          (merror "vtk3d: cylinder center must be a list of three floats"))
    (when (or (not (integerp res))
              (< res 0))
          (merror "vtk3d: cylinder resolution must be a positive integer"))
    (when (and (not (equal cap nil))
              (not (equal cap t)) )
          (merror "vtk3d: cylinder capping must be 'true' or 'false'"))
    (if cap
        (setf capn 1)
        (setf capn 0))
    (concatenate 'string
      (format nil "vtkCylinderSource ~a~%" source-name)
      (format nil "  ~a SetHeight ~a~%" source-name fhei)
      (format nil "  ~a SetRadius ~a~%" source-name frad)
      (format nil "  ~a SetCenter ~a ~a ~a~%"
              source-name
              (cadr fcen)
              (caddr fcen)
              (cadddr fcen))
      (format nil "  ~a SetResolution ~a~%" source-name res)
      (format nil "  ~a SetCapping ~a~%" source-name capn)
      (format nil "  ~a SetResolution ~a~%" source-name res)
      (format nil "  ~a SetCapping ~a~%" source-name capn)
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name)
      (vtkpolydatamapper-code mapper-name filter-name)
      (vtkactor-code actor-name mapper-name color) )))



;; cube(xlength, ylength, zlength, center)
;; ---------------------------------------
(defun vtk3d-cube (xlen ylen zlen cen)
  (let ((color (gethash '$color *gr-options*))
        (fxlen ($float xlen))
        (fylen ($float ylen))
        (fzlen ($float zlen))
        (fcen ($float cen))
        (source-name (get-source-name))
        (mapper-name (get-mapper-name))
        (actor-name  (get-actor-name))
        (trans-name  (get-trans-name))
        (filter-name (get-filter-name)))
    (when (or (not (floatp fxlen))
              (< fxlen 0.0))
          (merror "vtk3d: cube x-length must be a real equal or greater than zero"))
    (when (or (not (floatp fylen))
              (< fylen 0.0))
          (merror "vtk3d: cube y-length must be a real equal or greater than zero"))
    (when (or (not (floatp fzlen))
              (< fzlen 0.0))
          (merror "vtk3d: cube z-length must be a real equal or greater than zero"))
    (when (or (not ($listp fcen))
              (not (= ($length fcen) 3))
              (not (every #'floatp (rest fcen))) )
          (merror "vtk3d: cube center must be a list of three floats"))
    (concatenate 'string
      (format nil "vtkCubeSource ~a~%" source-name)
      (format nil "  ~a SetXLength ~a~%" source-name fxlen)
      (format nil "  ~a SetYLength ~a~%" source-name fylen)
      (format nil "  ~a SetZLength ~a~%" source-name fzlen)
      (format nil "  ~a SetCenter ~a ~a ~a~%"
              source-name
              (cadr fcen)
              (caddr fcen)
              (cadddr fcen))
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name)
      (vtkpolydatamapper-code mapper-name filter-name)
      (vtkactor-code actor-name mapper-name color) )))



;; sphere(radius, center, theta_resolution, phi_resolution, 
;;        theta_start, theta_end, phi_start, phi_end, tesselation)
;; --------------------------------------------------------------
;; theta is longitude and phi is latitude. With tesselation=false,
;; surface is generated by triangles, if true, with quadrilaterals.
;; Angles are measured in degrees.
(defun vtk3d-sphere (rad cen th-res ph-res th-sta th-end ph-sta ph-end tes)
  (let ((color (gethash '$color *gr-options*))
        (frad ($float rad))
        (fcen ($float cen))
        (fth-sta ($float th-sta))
        (fth-end ($float th-end))
        (fph-sta ($float ph-sta))
        (fph-end ($float ph-end))
        (source-name (get-source-name))
        (mapper-name (get-mapper-name))
        (actor-name  (get-actor-name))
        (trans-name  (get-trans-name))
        (filter-name (get-filter-name))
        tesn)
    (when (or (not (floatp frad))
              (< frad 0.0))
          (merror "vtk3d: sphere radius must be a real equal or greater than zero"))
    (when (or (not ($listp fcen))
              (not (= ($length fcen) 3))
              (not (every #'floatp (rest fcen))) )
          (merror "vtk3d: sphere center must be a list of three floats"))
    (when (or (not (integerp th-res))
              (< th-res 0))
          (merror "vtk3d: sphere theta resolution must be a positive integer"))
    (when (or (not (integerp ph-res))
              (< ph-res 0))
          (merror "vtk3d: sphere phi resolution must be a positive integer"))
    (when (or (not (floatp fth-sta))
              (< fth-sta 0.0)
              (> fth-sta 360.0))
          (merror "vtk3d: sphere theta start must be a real equal or greater than zero"))
    (when (or (not (floatp fth-end))
              (< fth-end 0.0)
              (> fth-end 360.0))
          (merror "vtk3d: sphere theta end must be a real equal or greater than zero"))
    (when (< fth-end fth-sta)
          (merror "vtk3d: sphere theta start must be less than theta end"))
    (when (< fph-end fph-sta)
          (merror "vtk3d: sphere phi start must be less than phi end"))
    (when (or (not (floatp fph-sta))
              (< fph-sta 0.0)
              (> fph-sta 180.0))
          (merror "vtk3d: sphere phi start must be a real equal or greater than zero"))
    (when (or (not (floatp fph-end))
              (< fph-end 0.0)
              (> fph-end 180.0))
          (merror "vtk3d: sphere phi end must be a real equal or greater than zero"))
    (when (and (not (equal tes nil))
               (not (equal tes t)) )
          (merror "vtk3d: sphere tesselation must be 'true' or 'false'"))
    (if tes
        (setf tesn 1)
        (setf tesn 0))
    (concatenate 'string
      (format nil "vtkSphereSource ~a~%" source-name)
      (format nil "  ~a SetRadius ~a~%" source-name frad)
      (format nil "  ~a SetCenter ~a ~a ~a~%"
              source-name
              (cadr fcen)
              (caddr fcen)
              (cadddr fcen))
      (format nil "  ~a SetThetaResolution ~a~%" source-name th-res)
      (format nil "  ~a SetPhiResolution ~a~%" source-name ph-res)
      (format nil "  ~a SetStartTheta ~a~%" source-name fth-sta)
      (format nil "  ~a SetEndTheta ~a~%" source-name fth-end)
      (format nil "  ~a SetStartPhi ~a~%" source-name fph-sta)
      (format nil "  ~a SetEndPhi ~a~%" source-name fph-end)
      (format nil "  ~a SetLatLongTessellation ~a~%" source-name tesn)
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name)
      (vtkpolydatamapper-code mapper-name filter-name)
      (vtkactor-code actor-name mapper-name color) )))



;; parallelogram(origin, point1, point2)
;; ------------------------------------
;; The parallelogram is defined by the origin and two points

(defun vtk3d-parallelogram (ori p1 p2)
  (let ((color (gethash '$color *gr-options*))
        (source-name (get-source-name))
        (trans-name  (get-trans-name))
        (filter-name (get-filter-name))
        (mapper-name (get-mapper-name))
        (actor-name  (get-actor-name))
        fori fp1 fp2  )
    (setf fori (map 'list #'$float (rest ori))
          fp1  (map 'list #'$float (rest p1))
          fp2  (map 'list #'$float (rest p2)) )
    (when (notevery #'(lambda (z) (floatp z))
                    (append fori fp1 fp2))
      (merror "vtk3d: arguments to parallelogram must be lists of floats"))
    (concatenate 'string
      (format nil "vtkPlaneSource ~a~%" source-name)
      (format nil "  ~a SetOrigin ~a ~a ~a~%"
              source-name
              (car fori)
              (cadr fori)
              (caddr fori))
      (format nil "  ~a SetPoint1 ~a ~a ~a~%"
              source-name
              (car fp1)
              (cadr fp1)
              (caddr fp1))
      (format nil "  ~a SetPoint2 ~a ~a ~a~%"
              source-name
              (car fp2)
              (cadr fp2)
              (caddr fp2))
      (format nil "  ~a SetXResolution ~a~%" source-name 10)
      (format nil "  ~a SetYResolution ~a~%" source-name 10)
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name)
      (vtkpolydatamapper-code mapper-name filter-name)
      (vtkactor-code actor-name mapper-name color))))



;; triangle(vertex1, vertex2, vertex3)
;; ------------------------------------
;; The triangle is defined by three vertices

(defun vtk3d-triangle (v1 v2 v3)
  (let ((color (gethash '$color *gr-options*))
        (points-name   (get-points-name))
        (triangle-name (get-triangle-name))
        (polydata-name (get-polydata-name))
        (trans-name    (get-trans-name))
        (filter-name   (get-filter-name))
        (mapper-name   (get-mapper-name))
        (actor-name    (get-actor-name))
        fv1 fv2 fv3  )
    (setf fv1 (map 'list #'$float (rest v1))
          fv2 (map 'list #'$float (rest v2))
          fv3 (map 'list #'$float (rest v3)) )
    (when (notevery #'(lambda (z) (floatp z))
                    (append fv1 fv2 fv3))
      (merror "vtk3d: arguments to triangle must be lists of floats"))
    (concatenate 'string
      (format nil "vtkPoints ~a~%" points-name)
      (format nil "  ~a SetNumberOfPoints 3~%" points-name)
      (format nil "  ~a InsertPoint 0 ~a ~a ~a~%"
              points-name
              (car fv1)
              (cadr fv1)
              (caddr fv1))
      (format nil "  ~a InsertPoint 1 ~a ~a ~a~%"
              points-name
              (car fv2)
              (cadr fv2)
              (caddr fv2))
      (format nil "  ~a InsertPoint 2 ~a ~a ~a~%"
              points-name
              (car fv3)
              (cadr fv3)
              (caddr fv3))
      (format nil "vtkTriangle ~a~%" triangle-name)
      (format nil "  [~a GetPointIds] SetId 0 0~%" triangle-name)
      (format nil "  [~a GetPointIds] SetId 1 1~%" triangle-name)
      (format nil "  [~a GetPointIds] SetId 2 2~%" triangle-name)
      (format nil "vtkPolyData ~a~%" polydata-name)
      (format nil "  ~a Allocate 1 1~%" polydata-name)
      (format nil "  ~a InsertNextCell [~a GetCellType] [~a GetPointIds]~%"
              polydata-name
              triangle-name
              triangle-name)
      (format nil "  ~a SetPoints ~a~%" polydata-name points-name)
      (vtktransform-code trans-name)
      (format nil "vtkTransformPolyDataFilter ~a~%" filter-name)
      (format nil " ~a SetInput ~a~%" filter-name polydata-name)
      (format nil " ~a SetTransform ~a~%" filter-name trans-name)
      (format nil "vtkPolyDataMapper ~a~%" mapper-name)
      (format nil " ~a SetInput ~a~%" mapper-name polydata-name)
      (vtkactor-code actor-name mapper-name color))))



;; vector([x,y,z], [dx,dy,dz])
;; ---------------------------
(defun vtk3d-vector (arg1 arg2)
  (when (or (not ($listp arg1))
            (not (= ($length arg1) 3))
            (not ($listp arg2))
            (not (= ($length arg2) 3)))
      (merror "vtk3d (vector): coordinates are not correct"))
  (let ((color        (gethash '$color        *gr-options*))
        (head-length  (gethash '$head_length  *gr-options*))
        (head-angle   (gethash '$head_angle   *gr-options*))
        (line-width   (gethash '$line_width   *gr-options*))
        (x ($float (cadr arg1)))
        (y ($float (caddr arg1)))
        (z ($float (cadddr arg1)))
        (dx ($float (cadr arg2)))
        (dy ($float (caddr arg2)))
        (dz ($float (cadddr arg2)))
        (source-name (get-source-name))
        (trans-name  (get-trans-name))
        (filter-name (get-filter-name))
        (mapper-name (get-mapper-name))
        (actor-name  (get-actor-name))
        ndx ndy ndz radians tiplength module radius rotangle)
    ; unitary vector
    (setf module (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
    (setf ndx (/ dx module)
          ndy (/ dy module)
          ndz (/ dz module))
    ; transform into unitary vector when unit_vectors=true
    (setf dx ndx
          dy ndy
          dz ndz
          module 1)
    ; head parameters
    (setf radians (* head-angle 0.0174532925199433)) ; 0.017..=%pi/180
    (setf tiplength (* head-length ($float ($cos radians))))
    (setf radius (* head-length ($float ($sin radians))))
    ; rotation angle
    (setf rotangle
          (* 57.29577951308232  ; 57.29..=180/%pi
            ($float ($asin (sqrt (+ (* ndz ndz) (* ndy ndy)))))))
    ; check if rotation angle is obtuse
    (when (< ndx 0)
      (setf rotangle (- 180.0 rotangle)))
    (when (and (= ndz 0.0) (= ndy 0.0))
      (setf ndy 0.0
            ndz -1.0))
    (concatenate 'string
      (format nil "vtkArrowSource ~a~%" source-name)
      (format nil "  ~a SetTipResolution ~a~%" source-name 20)
      (format nil "  ~a SetTipRadius ~a~%" source-name (/ radius module))
      (format nil "  ~a SetTipLength ~a~%" source-name (/ tiplength module))
      (format nil "  ~a SetShaftResolution ~a~%" source-name 10)
      (format nil "  ~a SetShaftRadius ~a~%" source-name line-width)
      (vtktransform-code trans-name)
      (format nil "  ~a Translate ~a ~a ~a~%" trans-name x y z)
      (format nil "  ~a RotateWXYZ ~a ~a ~a ~a~%" trans-name rotangle 0 (- ndz) ndy)
      (format nil "  ~a Scale ~a ~a ~a~%" trans-name module module module)
      (vtktransformfilter-code filter-name source-name trans-name)
      (vtkpolydatamapper-code mapper-name filter-name)
      (vtkactor-code actor-name mapper-name color) )))






;; points([[x1,y1,z1], [x2,y2,z2], [x3,y3,z3],...])
;; ------------------------------------------------

(defun build-vtkCellArray (nc np cellarrayname polydataname ispolyline)
  (let ((cntr -1)
        (nc1 (1- nc))
        (str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0)))
    (format str "vtkCellArray ~a~%" cellarrayname)
    (dotimes (c nc)
      (format str "  ~a InsertNextCell ~a~%" cellarrayname np)
      (when (and ispolyline (> c 0))
        (setf cntr (1- cntr)))
      (dotimes (p np)
        (when (and ispolyline
                   (= cntr nc1))
          (setf cntr -1))
        (format str "  ~a InsertCellPoint ~a~%" cellarrayname (incf cntr))))
    (format str "  ~a ~a ~a~%"
      polydataname
      (case np
        (1         "SetVerts")
        (2         "SetLines")
        (otherwise "SetPolys"))
      cellarrayname)
    str))

;; builds vtkPoints objects.
(defun build-vtkPoints (pointsname &rest coord)
  (let ((n (/ (length coord) 3))
        (cntr -1)
        (str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0)))
    (format str "vtkPoints ~a~%" pointsname)
    (format str "  ~a SetNumberOfPoints ~a~%" pointsname n)
    (dotimes (k n)
      (format str "  ~a InsertPoint ~a ~a ~a ~a~%"
              pointsname
              k
              (nth (incf cntr) coord)
              (nth (incf cntr) coord)
              (nth (incf cntr) coord)))
    str))

(defun vtk3d-points (arg)
  (let ((points-joined (gethash '$points_joined *gr-options*))
        (point-type    (gethash '$point_type    *gr-options*))
        (point-size    (gethash '$point_size    *gr-options*))
        (line-type     (gethash '$line_type     *gr-options*))
        (color         (gethash '$color         *gr-options*))
        (tmp (mapcar #'rest (rest arg)))
        source-name
        filter-name
        trans-name
        mapper-name
        actor-name
        points-name
        polydata-name
        cellarray-name
        polydatamapper-name
        solidsource-name
        lookup-table-name
        tube-name
        (data-file-name (get-data-file-name))
        (datastorage "")
        (output-string "")
        (minscalar most-positive-double-float)
        (maxscalar most-negative-double-float)
        newscalar slope scalars
        data-file-path
        x y z n pts)
    (setf x (map 'list #'$float (map 'list #'first  tmp))
          y (map 'list #'$float (map 'list #'second tmp))
          z (map 'list #'$float (map 'list #'third  tmp)))
    (setf n (length x))
    (setf pts (make-array (* 3 n) :element-type 'flonum
                                  :initial-contents (mapcan #'list x y z)))
    ; check enhanced3d model
    (check-enhanced3d-model "points" '(0 1 3))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array n :element-type 'flonum))
      (cond
        ((= *draw-enhanced3d-type* 1)
           (dotimes (k n)
             (setf newscalar (funcall *draw-enhanced3d-fun* k))
             (cond
               ((< newscalar minscalar)
                  (setf minscalar newscalar))
               ((> newscalar maxscalar)
                  (setf maxscalar newscalar)))
             (setf (aref scalars k) newscalar )) )
        ((= *draw-enhanced3d-type* 3)
           (dotimes (k n)
             (setf newscalar (funcall *draw-enhanced3d-fun* (nth k x) (nth k y) (nth k z)))
             (cond
               ((< newscalar minscalar)
                  (setf minscalar newscalar))
               ((> newscalar maxscalar)
                  (setf maxscalar newscalar)))
             (setf (aref scalars k) newscalar))))
      (if (< minscalar maxscalar)
        (setf slope (/ 1.0 (- maxscalar minscalar)))
        (setf slope 0.0)) )
    ; write vtk data file.
    (setf data-file-path (plot-temp-file data-file-name))
    (setf datastorage
      (open data-file-path
            :direction :output :if-exists :supersede))
    (format datastorage "~a~%~a~%~a~%~a~%~a ~a ~a~%"
      "# vtk DataFile Version 2.0"
      "  File generated by Maxima's draw package."
      "ASCII"
      (if points-joined
        "DATASET POLYDATA"
        "DATASET UNSTRUCTURED_GRID")
      "POINTS " n "double")
    (dotimes (k (* 3 n))
      (format datastorage "~a " (aref pts k)))
    (when points-joined
      (format datastorage "~%~%LINES 1 ~a ~%~a " (1+ n) n)
      (dotimes (k n)
        (format datastorage "~a " k)))
    (format datastorage "~%~%")
    ; write normalised scalars when enhanced3d is active
    (when (> *draw-enhanced3d-type* 0)
      (format datastorage "~a~a~%~a~a~a~%~a~%"
        "POINT_DATA " n
        "SCALARS " data-file-name " float"
        "LOOKUP_TABLE default")
      (dotimes (k n)
        (format datastorage "~a " (* slope (- (aref scalars k) minscalar)))))
    (close datastorage)

    ; lookup table
    (when (> *draw-enhanced3d-type* 0)
      (let ((lut (check-lookup-table)))
        (setf lookup-table-name (car lut))
        (setf output-string (cadr lut))))
    ; tcl-vtk code
    (setf source-name (get-source-name))
    (cond
      (points-joined
        (setf trans-name  (get-trans-name)
              filter-name (get-filter-name)
              mapper-name (get-mapper-name)
              actor-name  (get-actor-name))
        (setf output-string
          (concatenate 'string
            output-string
            (format nil "vtkPolyDataReader ~a~%" source-name)
            (format nil "  ~a SetFileName \"~a\"~%" source-name data-file-path)
            (vtktransform-code trans-name)
            (vtktransformpolydatafilter-code filter-name source-name trans-name)
            (cond
               ((< line-type 0) ; line type is a tube
                  (setf tube-name (get-tube-name))
                  (concatenate 'string
                    (vtktubefilter-code tube-name filter-name line-type)
                    (vtkpolydatamapper-code mapper-name tube-name t)))
               (t
                  (vtkpolydatamapper-code mapper-name filter-name)))
            (if (> *draw-enhanced3d-type* 0)
              (format nil "  ~a SetLookupTable ~a~%" mapper-name lookup-table-name)
              "")
            (vtkactor-code actor-name mapper-name color)
            (if (>= line-type 0) ; when line type is not a tube, set line pattern
              (format nil "  [~a GetProperty] SetLineStipplePattern ~a~%~%"
                      actor-name
                      (case line-type
                         (0 "0x0001")
                         (1 "0xFFFF")
                         (2 "0xFF00")
                         (3 "0xFE10")))
              "")     )))
      (t ; isolated points
        (setf output-string
          (concatenate 'string
            output-string
            (format nil "vtkUnstructuredGridReader ~a~%" source-name)
            (format nil "  ~a SetFileName \"~a\"~%" source-name data-file-path)))))

    ; draw glyphs according to point-type
    (cond
      ((and (>= point-type 0)
            (<= point-type 5))
         (setf points-name         (get-points-name)
               polydata-name       (get-polydata-name)
               cellarray-name      (get-cellarray-name)
               filter-name         (get-filter-name)
               polydatamapper-name (get-polydatamapper-name)
               actor-name          (get-actor-name)
               color               (hex-to-numeric-list color))
         (when (not points-joined)
           (setf source-name (get-source-name))
           (setf output-string
                 (concatenate 'string
                   output-string
                   (format nil "~a ~a~%  ~a ~a \"~a\"~%"
                     "vtkUnstructuredGridReader" source-name
                     source-name "SetFileName" data-file-path))))
         (setf output-string
           (concatenate 'string
             output-string
             (case point-type
               (0 (build-vtkPoints points-name
                    0 0 0))
               (1 (build-vtkPoints points-name
                     (- point-size) 0  0
                     point-size 0  0
                     0          0  (- point-size)
                     0          0  point-size))
               (2 (build-vtkPoints points-name
                     point-size     0 point-size
                     (- point-size) 0 (- point-size)
                     (- point-size) 0 point-size
                     point-size     0 (- point-size)))
               (3 (build-vtkPoints points-name
                     point-size     0 point-size
                     (- point-size) 0 (- point-size)
                     (- point-size) 0 point-size
                     point-size     0 (- point-size)
                     (- point-size) 0 0
                     point-size     0 0
                     0              0 (- point-size)
                     0              0 point-size))
               (4 (build-vtkPoints points-name
                     point-size     0 point-size
                     (- point-size) 0 point-size
                     (- point-size) 0 (- point-size)
                     point-size     0 (- point-size)))
               (5 (build-vtkPoints points-name
                     (- point-size) 0  point-size
                     point-size     0  point-size
                     point-size     0 (- point-size)
                     (- point-size) 0 (- point-size))) )
             (format nil "~a ~a~%  ~a ~a ~a~%"
               "vtkPolyData" polydata-name
               polydata-name "SetPoints" points-name)
             (case point-type
               (0     (build-vtkCellArray 1 1 cellarray-name polydata-name nil))
               ((1 2) (build-vtkCellArray 2 2 cellarray-name polydata-name nil))
               (3     (build-vtkCellArray 4 2 cellarray-name polydata-name nil))
               (4     (build-vtkCellArray 4 2 cellarray-name polydata-name t))
               (5     (build-vtkCellArray 1 4 cellarray-name polydata-name nil))
               (otherwise ""))
             (format nil "~a ~a~%  ~a ~a~a ~a~%  ~a ~a ~a~%  ~a ~a~%"
               "vtkGlyph3D" filter-name
               filter-name "SetInput [" source-name "GetOutput]"
               filter-name "SetSource" polydata-name
               filter-name "ScalingOff")
             (format nil "~a ~a~%  ~a ~a~a ~a~%"
               "vtkPolyDataMapper" polydatamapper-name
               polydatamapper-name "SetInput [" filter-name "GetOutput]")
             (if (> *draw-enhanced3d-type* 0)
               (format nil "  ~a SetLookupTable ~a~%" polydatamapper-name lookup-table-name)
               "")
             (format nil "~a ~a~%  ~a ~a ~a~%  ~a ~a ~a ~a ~a ~a~%~%"
               "vtkActor" actor-name
               actor-name "SetMapper" polydatamapper-name
               "[" actor-name "GetProperty] SetColor" (first color) (second color) (third color)))))
      ((and (>= point-type 14)
            (<= point-type 17))
         (setf solidsource-name    (get-solidsource-name)
               filter-name         (get-filter-name)
               polydatamapper-name (get-polydatamapper-name)
               actor-name          (get-actor-name)
               color               (hex-to-numeric-list color))
         (setf output-string
           (concatenate 'string
             output-string
             (case point-type
               (14 ; sphere glyph
                 (format nil "~a ~a~%  ~a ~a ~a~%"
                         "vtkSphereSource" solidsource-name
                         solidsource-name "SetRadius" (/ point-size 2.0)) )
               (15 ; cube glyph
                 (format nil "~a ~a~%  ~a ~a ~a~%  ~a ~a ~a~%  ~a ~a ~a~%"
                         "vtkCubeSource" solidsource-name
                         solidsource-name "SetXLength" point-size
                         solidsource-name "SetYLength" point-size
                         solidsource-name "SetZLength" point-size) )
               (16 ; cylinder glyph
                 (format nil "~a ~a~%  ~a ~a ~a~%  ~a ~a ~a~%"
                         "vtkCylinderSource" solidsource-name
                         solidsource-name "SetRadius" (/ point-size 2.0)
                         solidsource-name "SetHeight" point-size) )
               (17 ; cone glyph
                 (format nil "~a ~a~%  ~a ~a ~a~%  ~a ~a ~a~%"
                         "vtkConeSource" solidsource-name
                         solidsource-name "SetRadius" (/ point-size 2.0)
                         solidsource-name "SetHeight" point-size) ))
             (format nil "~a ~a~%  ~a ~a~a ~a~%  ~a ~a ~a ~a~%  ~a ~a~%"
                    "vtkGlyph3D" filter-name
                    filter-name "SetInput [" source-name "GetOutput]"
                    filter-name "SetSource [" solidsource-name "GetOutput]"
                    filter-name "ScalingOff")
             (format nil "~a ~a~%  ~a ~a~a ~a~%"
                     "vtkPolyDataMapper" polydatamapper-name
                     polydatamapper-name "SetInput [" filter-name "GetOutput]")
             (if (> *draw-enhanced3d-type* 0)
               (format nil "  ~a SetLookupTable ~a~%" polydatamapper-name lookup-table-name)
               "")
             (format nil "~a ~a~%  ~a ~a ~a~%  ~a ~a ~a ~a ~a ~a~%~%"
                     "vtkActor" actor-name
                      actor-name "SetMapper" polydatamapper-name
                      "[" actor-name "GetProperty] SetColor" (first color) (second color) (third color)))))
      (t
         (merror "vtk3d: not recognized point_type")))))







;; parametric(xfun,yfun,zfun,par1,parmin,parmax)
;; ---------------------------------------------
(defun vtk3d-parametric (xfun yfun zfun par1 parmin parmax)
  (let* ((nticks    (gethash '$nticks    *gr-options*))
         (color     (gethash '$color     *gr-options*))
         (line-type (gethash '$line_type *gr-options*))
         ($numer t)
         (tmin ($float parmin))
         (tmax ($float parmax))
         (tt tmin)
         (eps (/ (- tmax tmin) (- nticks 1)))
         (source-name (get-source-name))
         (mapper-name (get-mapper-name))
         (actor-name  (get-actor-name))
         (trans-name  (get-trans-name))
         (filter-name (get-filter-name))
         (data-file-name (get-data-file-name))
         lookup-table-name
         tube-name
         (output-string "")
         (datastorage "")
         data-file-path
         (count -1)
         (minscalar most-positive-double-float)
         (maxscalar most-negative-double-float)
         newscalar slope scalars
         result f1 f2 f3 xx yy zz)
    (check-enhanced3d-model "parametric" '(0 1 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1)))
    (if (< tmax tmin)
       (merror "vtk3d (parametric): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist) ,par1)))
    (setq f2 (coerce-float-fun yfun `((mlist) ,par1)))
    (setq f3 (coerce-float-fun zfun `((mlist) ,par1)))
    (setf result (make-array (* 3 nticks) :element-type 'flonum))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array nticks :element-type 'flonum)))
    (dotimes (k nticks)
      (setf xx (funcall f1 tt))
      (setf yy (funcall f2 tt))
      (setf zz (funcall f3 tt))
      (case *draw-enhanced3d-type*
        ((1 99) (setf newscalar (funcall *draw-enhanced3d-fun* tt))
                (cond
                  ((< newscalar minscalar)
                     (setf minscalar newscalar))
                  ((> newscalar maxscalar)
                     (setf maxscalar newscalar)))
               (setf (aref scalars k) newscalar))
        (3      (setf newscalar (funcall *draw-enhanced3d-fun* xx yy zz))
                (cond
                  ((< newscalar minscalar)
                     (setf minscalar newscalar))
                  ((> newscalar maxscalar)
                     (setf maxscalar newscalar)))
                (setf (aref scalars k) newscalar)))
      (transform-point 3)
      (setf (aref result (incf count)) xx)
      (setf (aref result (incf count)) yy)
      (setf (aref result (incf count)) zz)
      (setf tt (+ tt eps)) )
    (if (< minscalar maxscalar)
      (setf slope (/ 1.0 (- maxscalar minscalar)))
      (setf slope 0.0))

    ; write vtk data file
    (setf data-file-path (plot-temp-file data-file-name))
    (setf datastorage
      (open data-file-path
            :direction :output :if-exists :supersede))
    (format datastorage "~a~%~a~%~a~%~a~%~a ~a ~a~%"
      "# vtk DataFile Version 2.0"
      "  File generated by Maxima's draw package."
      "ASCII"
      "DATASET POLYDATA"
      "POINTS " nticks "float")
    (dotimes (k (array-dimension result 0))
      (format datastorage "~a " (aref result k)))
    (format datastorage "~%~%LINES 1 ~a ~%~a " (1+ nticks) nticks)
    (dotimes (k nticks)
      (format datastorage "~a " k))
    (format datastorage "~%~%")
    ; write scalars for lookup table when enhanced3d is active
    (when (> *draw-enhanced3d-type* 0)
      (format datastorage "~a~a~%~a~a~a~%~a~%"
        "POINT_DATA " nticks
        "SCALARS " data-file-name " float"
        "LOOKUP_TABLE default")
      (dotimes (k (array-dimension scalars 0))
        (format datastorage "~a " (* slope (- (aref scalars k) minscalar)))))
    (close datastorage)

    ; lookup table
    (when (> *draw-enhanced3d-type* 0)
      (let ((lut (check-lookup-table)))
        (setf lookup-table-name (car lut))
        (setf output-string (cadr lut))))
    ; return tcl-vtk code
    (concatenate 'string
      output-string
      (format nil "vtkPolyDataReader ~a~%" source-name)
      (format nil "  ~a SetFileName \"~a\"~%" source-name data-file-path)
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name)
      (cond
         ((< line-type 0) ; line type is a tube
            (setf tube-name (get-tube-name))
            (concatenate 'string
              (vtktubefilter-code tube-name filter-name line-type)
              (vtkpolydatamapper-code mapper-name tube-name t)))
         (t
            (vtkpolydatamapper-code mapper-name filter-name)))
      (if (> *draw-enhanced3d-type* 0)
        (format nil "  ~a SetLookupTable ~a~%" mapper-name lookup-table-name)
        "")
      (vtkactor-code actor-name mapper-name color)
      (if (>= line-type 0) ; when line type is not a tube, set line pattern
        (format nil "  [~a GetProperty] SetLineStipplePattern ~a~%~%"
                actor-name
                (case line-type
                   (0 "0x0001")
                   (1 "0xFFFF")
                   (2 "0xFF00")
                   (3 "0xFE10")))
        ""))) )



;; parametric_surface(xfun,yfun,zfun,par1,par1min,par1max,par2,par2min,par2max)
;; -----------------------------------------------------------------------------------------
(defun vtk3d-parametric_surface (xfun yfun zfun par1 par1min par1max par2 par2min par2max)
  (let* ((xu_grid    (gethash '$xu_grid    *gr-options*))
         (yv_grid    (gethash '$yv_grid    *gr-options*))
         (color (gethash '$color *gr-options*))
         (umin ($float par1min))
         (umax ($float par1max))
         (vmin ($float par2min))
         (vmax ($float par2max))
         (epsu (/ (- umax umin) xu_grid))
         (epsv (/ (- vmax vmin) yv_grid))
         (source-name (get-source-name))
         (mapper-name (get-mapper-name))
         (actor-name  (get-actor-name))
         (trans-name  (get-trans-name))
         (filter-name (get-filter-name))
         (data-file-name (get-data-file-name))
         lookup-table-name
         (output-string "")
         (datastorage "")
         data-file-path
         (xx 0.0) (uu 0.0)
         (yy 0.0) (vv 0.0)
         (zz 0.0)
         (nx (+ xu_grid 1))
         (ny (+ yv_grid 1))
         ($numer t)
         (result-count -1)
         (scalars-count -1)
         (minscalar most-positive-double-float)
         (maxscalar most-negative-double-float)
         newscalar slope scalars
         result f1 f2 f3)
    (check-enhanced3d-model "parametric_surface" '(0 2 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1 par2)))
    (when (or (< umax umin)
              (< vmax vmin))
       (merror "vtk3d (parametric_surface): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist) ,par1 ,par2)))
    (setq f2 (coerce-float-fun yfun `((mlist) ,par1 ,par2)))
    (setq f3 (coerce-float-fun zfun `((mlist) ,par1 ,par2)))
    (setf result (make-array (* 3 nx ny) :element-type 'flonum))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array (* nx ny) :element-type 'flonum)))
    (loop for j below ny
           initially (setf vv vmin)
           do (setf uu umin)
           (loop for i below nx
                  do
                  (setf xx (funcall f1 uu vv))
                  (setf yy (funcall f2 uu vv))
                  (setf zz (funcall f3 uu vv))
                  ; check texture model
                  (case *draw-enhanced3d-type*
                    ((2 99) (setf newscalar (funcall *draw-enhanced3d-fun* uu vv))
                            (cond
                              ((< newscalar minscalar)
                               (setf minscalar newscalar))
                              ((> newscalar maxscalar)
                               (setf maxscalar newscalar)))
                            (setf (aref scalars (incf scalars-count)) newscalar))
                    (3 (setf newscalar (funcall *draw-enhanced3d-fun* xx yy zz))
                       (cond
                         ((< newscalar minscalar)
                          (setf minscalar newscalar))
                         ((> newscalar maxscalar)
                          (setf maxscalar newscalar)))
                       (setf (aref scalars (incf scalars-count)) newscalar)) )
                  (transform-point 3)
                  (setf (aref result (incf result-count)) xx
                        (aref result (incf result-count)) yy
                        (aref result (incf result-count)) zz)
                  (setq uu (+ uu epsu)))
           (setq vv (+ vv epsv)))
    (if (< minscalar maxscalar)
      (setf slope (/ 1.0 (- maxscalar minscalar)))
      (setf slope 0.0))

    (write-polydata-file)
    (write-polydata-reader)) )



;; explicit(fcn,par1,minval1,maxval1,par2,minval2,maxval2)
;; -------------------------------------------------------
(defun vtk3d-explicit (fcn par1 minval1 maxval1 par2 minval2 maxval2)
  (let* ((xu_grid (gethash '$xu_grid *gr-options*))
         (yv_grid (gethash '$yv_grid *gr-options*))
         (color   (gethash '$color   *gr-options*))
         (fminval1 ($float minval1))
         (fminval2 ($float minval2))
         (fmaxval1 ($float maxval1))
         (fmaxval2 ($float maxval2))
         (epsx (/ (- fmaxval1 fminval1) xu_grid))
         (epsy (/ (- fmaxval2 fminval2) yv_grid))
         (source-name (get-source-name))
         (mapper-name (get-mapper-name))
         (actor-name  (get-actor-name))
         (trans-name  (get-trans-name))
         (filter-name (get-filter-name))
         (data-file-name (get-data-file-name))
         lookup-table-name
         (output-string "")
         (datastorage "")
         data-file-path
         (xx 0.0) (uu 0.0)
         (yy 0.0) (vv 0.0)
         (zz 0.0)
         (nx (+ xu_grid 1))
         (ny (+ yv_grid 1))
         ($numer t)
         (result-count -1)
         (scalars-count -1)
         (minscalar most-positive-double-float)
         (maxscalar most-negative-double-float)
         (scalars nil)
         newscalar slope result)
    (check-enhanced3d-model "explicit" '(0 2 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1 par2)))
    (setq fcn (coerce-float-fun fcn `((mlist) ,par1 ,par2)))
    (setf result (make-array (* 3 nx ny) :element-type 'flonum))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array (* nx ny) :element-type 'flonum)))
    (loop for j below ny
           initially (setf vv fminval2)
           do (setf uu fminval1)
           (loop for i below nx
                  do
                  (setf xx uu
                        yy vv)
                  (setf zz (funcall fcn xx yy))
                  ; check texture model
                  (case *draw-enhanced3d-type*
                    ((2 99) (setf newscalar (funcall *draw-enhanced3d-fun* xx yy))
                            (cond
                              ((< newscalar minscalar)
                                (setf minscalar newscalar))
                              ((> newscalar maxscalar)
                                (setf maxscalar newscalar)))
                            (setf (aref scalars (incf scalars-count)) newscalar))
                    (3      (setf newscalar (funcall *draw-enhanced3d-fun* xx yy zz))
                            (cond
                              ((< newscalar minscalar)
                                (setf minscalar newscalar))
                              ((> newscalar maxscalar)
                                (setf maxscalar newscalar)))
                            (setf (aref scalars (incf scalars-count)) newscalar)) )
                  (transform-point 3)
                  (setf (aref result (incf result-count)) xx
                        (aref result (incf result-count)) yy
                        (aref result (incf result-count)) zz)
                  (setq uu (+ uu epsx)))
           (setq vv (+ vv epsy)))
    (if (< minscalar maxscalar)
      (setf slope (/ 1.0 (- maxscalar minscalar)))
      (setf slope 0.0))

    (write-polydata-file)
    (write-polydata-reader) ) )



;; elevation_grid(mat x0 y0 width height)
;; --------------------------------------
(defun vtk3d-elevation_grid (mat x0 y0 width height)
  (let* ((fx0 ($float x0))
         (fy0 ($float y0))
         (fwidth ($float width))
         (fheight ($float height))
         (color (gethash '$color *gr-options*))
         (source-name (get-source-name))
         (mapper-name (get-mapper-name))
         (actor-name  (get-actor-name))
         (trans-name  (get-trans-name))
         (filter-name (get-filter-name))
         (data-file-name (get-data-file-name))
         lookup-table-name
         (output-string "")
         (datastorage "")
         (xi 0.0)
         (yi (+ fy0 fheight))
         (xx 0.0)
         (yy 0.0)
         (zz 0.0)
         (result-count -1)
         (scalars-count -1)
         (minscalar most-positive-double-float)
         (maxscalar most-negative-double-float)
         (result nil)
         data-file-path ny nx scalars newscalar dx dy slope )

    (check-enhanced3d-model "elevation_grid" '(0 2 3))
    (when (null ($matrixp mat))
      (merror "draw3d (elevation_grid): Argument not recognized"))
    (setf nx (length (cdadr mat))
          ny (length (cdr mat)))
    (setf dx (/ fwidth (1- nx))
          dy (/ fheight (1- ny)))
    (setf result (make-array (* 3 nx ny) :element-type 'flonum))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array (* nx ny) :element-type 'flonum)))
    (loop for row on (cdr mat) by #'cdr do
       (setf xi fx0)
       (loop for col on (cdar row) by #'cdr do
          (setf xx xi
                yy yi)
          (setf zz ($float (car col)))
          ; check texture model
          (case *draw-enhanced3d-type*
            (2 (setf newscalar (funcall *draw-enhanced3d-fun* xx yy))
               (cond
                 ((< newscalar minscalar)
                   (setf minscalar newscalar))
                 ((> newscalar maxscalar)
                   (setf maxscalar newscalar)))
               (setf (aref scalars (incf scalars-count)) newscalar))
            (3 (setf newscalar (funcall *draw-enhanced3d-fun* xx yy zz))
               (cond
                 ((< newscalar minscalar)
                   (setf minscalar newscalar))
                 ((> newscalar maxscalar)
                   (setf maxscalar newscalar)))
               (setf (aref scalars (incf scalars-count)) newscalar)))
          (transform-point 3)
          (setf (aref result (incf result-count)) xx
                (aref result (incf result-count)) yy
                (aref result (incf result-count)) zz)
          (setf xi (+ xi dx)))
       (setf yi (- yi dy)))
    (if (< minscalar maxscalar)
      (setf slope (/ 1.0 (- maxscalar minscalar)))
      (setf slope 0.0))

    (write-polydata-file)
    (write-polydata-reader)))



;; implicit(expr,x,xmin,xmax,y,ymin,ymax,z,zmin,zmax)
;; --------------------------------------------------
(defun vtk3d-implicit (expr par1 xmin xmax par2 ymin ymax par3 zmin zmax)
  (let ((fxmin ($float xmin))
        (fxmax ($float xmax))
        (fymin ($float ymin))
        (fymax ($float ymax))
        (fzmin ($float zmin))
        (fzmax ($float zmax))
        (color     (gethash '$color *gr-options*))
        (transform (gethash '$transform *gr-options*))
        (source-name (get-source-name))
        (mapper-name (get-mapper-name))
        (actor-name  (get-actor-name))
        (trans-name  (get-trans-name))
        (filter-name (get-filter-name))
        (data-file-name (get-data-file-name))
        (minscalar most-positive-double-float)
        (maxscalar most-negative-double-float)
        lookup-table-name
        (output-string "")
        (datastorage "")
        vertices data-file-path numvert result scalars slope newscalar xx yy zz)
    (check-enhanced3d-model "implicit" '(0 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1 par2 par3)))
    (setf vertices (find-triangles expr par1 fxmin fxmax par2 fymin fymax par3 fzmin fzmax))
    (when (null vertices)
      (merror "draw3d (implicit): no surface within these ranges"))
    (setf numvert (length vertices))
    (setf result (make-array (* 3 numvert) :element-type 'flonum
                                                     :initial-contents (flatten vertices)))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array numvert :element-type 'flonum)))
    (do ((nf 0 (+ 3 nf)))
        ((= nf (array-dimension result 0)) 'done)
      (setf xx (aref result nf)
            yy (aref result (+ 1 nf))
            zz (aref result (+ 2 nf)))
      (when (> *draw-enhanced3d-type* 0)
        (setf newscalar (funcall *draw-enhanced3d-fun* xx yy zz))
        (cond
          ((< newscalar minscalar)
             (setf minscalar newscalar))
          ((> newscalar maxscalar)
             (setf maxscalar newscalar)))
        (setf (aref scalars (/ nf 3)) newscalar))
      (when (not (eq transform '$none))
        (transform-point 3)
        (setf (aref result nf)       xx
              (aref result (+ 1 nf)) yy
              (aref result (+ 2 nf)) zz)) )
    (if (< minscalar maxscalar)
      (setf slope (/ 1.0 (- maxscalar minscalar)))
      (setf slope 0.0))
    (write-polydata-file-triangles)
    (write-polydata-reader) ))



;; label([string1,x1,y1,z1],[string2,x2,y2,z2],...)
;; ------------------------------------------------
(defun vtk3d-label (&rest lab)
  (let ((font-size (gethash '$font_size *gr-options*))
        (colist (hex-to-numeric-list (gethash '$color *gr-options*)))
        (out "")
        label-name
        polydatamapper-name
        actor-name
        text fx fy fz)
    (cond ((null lab)
             (merror "vtk (label): no arguments in object labels"))
          ((or (notevery #'$listp lab)
               (notevery #'(lambda (z) (= 4 ($length z))) lab))
             (merror "vtk3d (label): arguments must be lists of length 4"))
          (t
             (dolist (k lab)
               (setf text (format nil "\"~a\"" ($first k))
                     fx   ($float ($second k))
                     fy   ($float ($third k))
                     fz   ($float ($fourth k)))
               (when (or (not (floatp fx)) 
                         (not (floatp fy))
                         (not (floatp fz)))
                   (merror "vtk3d (label): non real 3d coordinates"))
               (setf actor-name          (get-actor-name)
                     label-name          (get-label-name)
                     polydatamapper-name (get-polydatamapper-name))
               (setf out
                     (concatenate 'string
                       out
                       (format nil "vtkVectorText ~a~%" label-name)
                       (format nil "  ~a SetText ~a~%" label-name text)
                       (format nil "vtkPolyDataMapper ~a~%" polydatamapper-name)
                       (format nil "  ~a SetInputConnection [~a GetOutputPort]~%"
                               polydatamapper-name
                               label-name)
                       (format nil "  ~a ScalarVisibilityOff~%" polydatamapper-name)
                       (format nil "vtkFollower ~a~%" actor-name)
                       (format nil "  ~a SetMapper ~a~%" actor-name polydatamapper-name)
                       (format nil "  ~a SetScale ~a ~a ~a~%" actor-name font-size font-size font-size)
                       (format nil "  ~a AddPosition ~a ~a ~a~%" actor-name fx fy fz)
                       (format nil "  [~a GetProperty] SetColor ~a ~a ~a~%~%"
                               actor-name
                               (first colist)
                               (second colist)
                               (third colist)))))
             out))))






;; AUXILIARY FUNCTIONS FOR 3D SCENE BUILDER

(defun camera-settings (cmds)
  (let ((rv (car  (gethash '$view *gr-options*)))
        (rh (cadr (gethash '$view *gr-options*)))
        (k 0.0174532925199433) ; %pi/180
        x y z)
    (setf rv (* k rv)
          rh (* k rh))
    (setf x (* (sin rv) (sin rh))
          y (- (* (sin rv) (cos rh)))
          z (cos rv))
    (format
       cmds 
       "~a~%~a ~a ~a ~a~%~a~%~a~%~a~%"
       "vtkCamera camera"
       "  camera SetPosition " x y z
       "  camera SetViewUp 0 0 1"
       "  ren1 SetActiveCamera camera"
       "  ren1 ResetCamera")))

(defun label-followers (cmds)
  (dolist (k *label-actors*)
    (format cmds "  actor~a SetCamera [ren1 GetActiveCamera]~%" k)))

(defun vtk-terminal (cmds)
  (let ((dimensions (gethash '$dimensions *gr-options*))
        (terminal   (gethash '$terminal *gr-options*))
        (filename   (gethash '$file_name *gr-options*))
        (binaryterms '($png $pngcairo $jpg $eps $eps_color $tiff $pnm))
        (extension "")
        (classformat ""))
    (format cmds "~%~%~a~%~a~%~a ~a ~a~%~%"
      "vtkRenderWindow renWin"
      "   renWin AddRenderer ren1"
      "   renWin SetSize"
      (car  dimensions)
      (cadr dimensions))
    (cond
      ((member terminal binaryterms)
         (case terminal
           (($png $pngcairo)
              (setf extension   "png"
                    classformat "vtkPNGWriter"))
           ($jpg
             (setf extension   "jpg"
                   classformat "vtkJPEGWriter"))
           ($tiff
             (setf extension   "tif"
                   classformat "vtkTIFFWriter"))
           ($pnm
             (setf extension   "pnm"
                   classformat "vtkPNMWriter"))
           (($eps $eps_color)
              (setf extension   "eps"
                    classformat "vtkPostScriptWriter")))
         (format cmds "~a~%~a~%~a ~a~%~a~%~a \"~a.~a\"~%~a~%~a~%~a~%"
           "vtkWindowToImageFilter w2if"
           "  w2if SetInput renWin"
           classformat "writer"
           "  writer SetInput [w2if GetOutput]"
           "  writer SetFileName" filename extension
           "  writer Write"
           "vtkCommand DeleteAllObjects"
           "exit"))
      ((eq terminal '$vrml)
         (format cmds "~a~%~a~%~a \"~a.~a\"~%~a~%~a~%~a~%~a~%"
           "vtkVRMLExporter vrml"
           "  vrml SetInput renWin"
           "  vrml SetFileName" filename "wrl"
           "  vrml SetSpeed 5.5"
           "  vrml Write"
           "vtkCommand DeleteAllObjects"
           "exit"))
      ((eq terminal '$obj)
         (format cmds "~a~%~a~%~a ~a~%~a~%~a~%~a~%"
           "vtkOBJExporter obj"
           "  obj SetInput renWin"
           "  obj SetFilePrefix" filename
           "  obj Write"
           "vtkCommand DeleteAllObjects"
           "exit"))
      ((eq terminal '$screen)
         (format cmds "~a~%~a~%~a~%~a~%"
           "vtkRenderWindowInteractor iren"
           "   iren SetRenderWindow renWin"
           "   iren Initialize"
           "wm withdraw ."))
      (t
         (merror "vtk3d: unknown terminal")))))


;; 3D SCENE BUILDER

(defvar *vtk3d-graphic-objects* (make-hash-table))

; table of 3d graphic objects
(setf (gethash '$cone               *vtk3d-graphic-objects*) 'vtk3d-cone
      (gethash '$cylinder           *vtk3d-graphic-objects*) 'vtk3d-cylinder
      (gethash '$cube               *vtk3d-graphic-objects*) 'vtk3d-cube
      (gethash '$elevation_grid     *vtk3d-graphic-objects*) 'vtk3d-elevation_grid
      (gethash '$implicit           *vtk3d-graphic-objects*) 'vtk3d-implicit
      (gethash '$sphere             *vtk3d-graphic-objects*) 'vtk3d-sphere
      (gethash '$parallelogram      *vtk3d-graphic-objects*) 'vtk3d-parallelogram
      (gethash '$triangle           *vtk3d-graphic-objects*) 'vtk3d-triangle
      (gethash '$vector             *vtk3d-graphic-objects*) 'vtk3d-vector
      (gethash '$points             *vtk3d-graphic-objects*) 'vtk3d-points
      (gethash '$parametric         *vtk3d-graphic-objects*) 'vtk3d-parametric
      (gethash '$parametric_surface *vtk3d-graphic-objects*) 'vtk3d-parametric_surface
      (gethash '$explicit           *vtk3d-graphic-objects*) 'vtk3d-explicit
      (gethash '$label              *vtk3d-graphic-objects*) 'vtk3d-label )

(defun vtk3d (&rest args)
  (let ((objects nil)
        (cmdstorage "")
        gfn largs obj)
    (ini-gr-options)
    (ini-global-options)
    (ini-local-option-variables)
    (user-defaults)
    (setf *vtk-source-counter* 0
          *vtk-mapper-counter* 0
          *vtk-actor-counter* 0
          *vtk-trans-counter* 0
          *vtk-filter-counter* 0
          *vtk-data-file-counter* 0
          *vtk-points-counter* 0
          *vtk-polydata-counter* 0
          *vtk-cellarray-counter* 0
          *vtk-polydatamapper-counter* 0
          *vtk-solidsource-counter* 0
          *vtk-triangle-counter* 0
          *vtk-label-counter* 0
          *vtk-tube-counter* 0
          *lookup-tables* nil
          *unitscale-already-defined* nil
          *label-actors* nil)

    (setf largs (listify-arguments))
    (dolist (x largs)
      (cond ((equal ($op x) "=")
              (case ($lhs x)
                ($color            (update-color             '$color            ($rhs x)))
                ($file_name        (update-string            '$file_name        ($rhs x)))
                ($font_size        (update-positive-float    '$font_size        ($rhs x)))
                ($head_angle       (update-positive-float    '$head_angle       ($rhs x)))
                ($head_length      (update-positive-float    '$head_length      ($rhs x)))
                ($background_color (update-color             '$background_color ($rhs x)))
                ($dimensions       (update-dimensions                           ($rhs x)))
                ($axis_3d          (update-boolean-option    '$axis_3d          ($rhs x)))
                ($view             (update-view                                 ($rhs x)))
                ($nticks           (update-positive-integer  '$nticks           ($rhs x)))
                ($line_width       (update-positive-float    '$line_width       ($rhs x)))
                ($line_type        (update-linestyle         '$line_type        ($rhs x)))
                ($xu_grid          (update-positive-integer  '$xu_grid          ($rhs x)))
                ($yv_grid          (update-positive-integer  '$yv_grid          ($rhs x)))
                ($opacity          (update-opacity                              ($rhs x)))
                ($palette          (update-palette                              ($rhs x)))
                ($transform        (update-transform                            ($rhs x)))
                ($points_joined    (update-boolean-option    '$points_joined    ($rhs x)))
                ($point_type       (update-pointtype                            ($rhs x)))
                ($point_size       (update-nonnegative-float '$point_size       ($rhs x)))
                ($enhanced3d       (update-enhanced3d                           ($rhs x)))
                ($terminal         (update-terminal                             ($rhs x)))
                ($x_voxel          (update-positive-integer  '$x_voxel          ($rhs x)))
                ($y_voxel          (update-positive-integer  '$y_voxel          ($rhs x)))
                ($z_voxel          (update-positive-integer  '$z_voxel          ($rhs x)))
                ($unit_vectors     (update-boolean-option    '$unit_vectors     ($rhs x)))
                (otherwise (merror "vtk3d: unknown option ~M " ($lhs x)))))

            ((setf obj (gethash (caar x) *vtk3d-graphic-objects*))
               (setf objects
                     (append objects (list (apply obj (rest x))))))
            (t
              (merror "vtk3d: item ~M is not recognized" x))))

    ;; prepare script file
    (setf gfn (plot-temp-file "maxout.tcl"))
    (setf cmdstorage
      (open gfn
            :direction :output :if-exists :supersede))

    ; requiered packages
    (format cmdstorage "~a~%~a~%~%"
      "package require vtk"
      "package require vtkinteraction")

    ; write objects
    (dolist (obj objects)
      (format cmdstorage "~a" obj) )

    ; packaging scene objects
    (format cmdstorage "vtkAppendPolyData appendData~%")
    (dotimes (n *vtk-filter-counter*)
      (format cmdstorage "  appendData AddInput [filter~a GetOutput]~%" (+ 1 n)))
    (format cmdstorage "  appendData Update~%~%")

    ; plot bounding box
    (when (gethash '$axis_3d *gr-options*)
      (format cmdstorage "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~%"
        "vtkOutlineFilter outline"
        "  outline SetInput [appendData GetOutput]"
        "vtkPolyDataMapper outlineMapper"
        "  outlineMapper SetInput [outline GetOutput]"
        "vtkActor outlineActor"
        "  outlineActor SetMapper outlineMapper"
        "  [outlineActor GetProperty] SetColor 1 0 0")

      ; axes code
      (format cmdstorage "~a~%~a~%~a~%"
        "vtkTextProperty tprop"
        "  tprop SetColor 0 0 0"
        "#  tprop ShadowOn")
      (format cmdstorage "~a~%~a~%~a ~a~%~a~%~a~%~a~%~a~%~%"
        "vtkCubeAxesActor2D axes"
        "  axes SetInput [appendData GetOutput]"
        "  axes SetLabelFormat " "%6.4g"
        "  axes SetFlyModeToOuterEdges"
        "  axes SetFontFactor 0.8"
        "  axes SetAxisTitleTextProperty tprop"
        "  axes SetAxisLabelTextProperty tprop"))

    ; renderer and window
    (let ((bgcolor (hex-to-numeric-list (gethash '$background_color *gr-options*))))
      (format cmdstorage "~a~%~a ~a ~a ~a~%"
        "vtkRenderer ren1"
        "  ren1 SetBackground "
        (first  bgcolor)
        (second bgcolor)
        (third  bgcolor)))
    ; adding actors
    (dotimes (n *vtk-actor-counter*)
      (format cmdstorage "  ren1 AddActor actor~a~%" (+ 1 n)))

    (when (gethash '$axis_3d *gr-options*)
      (format cmdstorage "~a~%~a~%~a~%~%" 
              "  axes SetCamera [ren1 GetActiveCamera]"
              "  ren1 AddActor outlineActor"
              "  ren1 AddViewProp axes"))
    (camera-settings cmdstorage)
    (label-followers cmdstorage)
    (vtk-terminal cmdstorage)

    ; close script file and call vtk
    (close cmdstorage)
    ($system (format nil "~a \"~a\"" "vtk " gfn))
    '$done))

