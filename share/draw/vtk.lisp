;;;                 COPYRIGHT NOTICE
;;;  
;;;  Copyright (C) 2012-2018 Mario Rodriguez Riotorto
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

;;; For examples, visit
;;; http://tecnostats.net/Maxima/vtk

;;; For questions, suggestions, bugs and the like, feel free
;;; to contact me at
;;; mario @@@ edu DOT xunta DOT es


;;; AUXILIARY FUNCTIONS

;; Global variables
(defvar *vtk-appenddata-counter* 0)
(defvar *vtk-extract-counter* 0)
(defvar *vtk-outline-counter* 0)
(defvar *vtk-polydatamapper-counter* 0)
(defvar *vtk-outlineactor-counter* 0)
(defvar *vtk-textproperty-counter* 0)
(defvar *vtk-cubeaxesactor2d-counter* 0)
(defvar *vtk-camera-counter* 0)
(defvar *vtk-renderer-counter* 0)
(defvar *vtk-source-counter* 0)
(defvar *vtk-mapper-counter* 0)
(defvar *vtk-actor-counter* 0)
(defvar *vtk-labelactor-counter* 0)
(defvar *vtk-trans-counter* 0)
(defvar *vtk-filter-counter* 0)
(defvar *vtk-floatarray-counter* 0)
(defvar *vtk-data-file-counter* 0)
(defvar *vtk-points-counter* 0)
(defvar *vtk-glyphpoints-counter* 0)
(defvar *vtk-polydata-counter* 0)
(defvar *vtk-cellarray-counter* 0)
(defvar *vtk-solidsource-counter* 0)
(defvar *vtk-triangle-counter* 0)
(defvar *vtk-label-counter* 0)
(defvar *vtk-chart-counter* 0)
(defvar *vtk-table-counter* 0)
(defvar *vtk-arrayX-counter* 0)
(defvar *vtk-arrayY-counter* 0)
(defvar *vtk-2dkey-counter* 0)
(defvar *vtk-isolines-counter* 0)
(defvar *lookup-tables* nil)
(defvar *unitscale-already-defined* nil)
(defparameter *enhanced3d-or-isolines-code* (make-hash-table))

(defun get-appenddata-name ()
  (format nil "appenddata~a" (incf *vtk-appenddata-counter*)))

(defun get-extract-name ()
  (format nil "extract~a" (incf *vtk-extract-counter*)))

(defun get-outline-name ()
  (format nil "outline~a" (incf *vtk-outline-counter*)))

(defun get-outlineactor-name ()
  (format nil "outlineactor~a" (incf *vtk-outlineactor-counter*)))

(defun get-textproperty-name ()
  (format nil "textproperty~a" (incf *vtk-textproperty-counter*)))

(defun get-cubeaxesactor2d-name ()
  (format nil "cubeaxesactor2d~a" (incf *vtk-cubeaxesactor2d-counter*)))

(defun get-camera-name ()
  (format nil "camera~a" (incf *vtk-camera-counter*)))

(defun get-renderer-name ()
  (format nil "renderer~a" (incf *vtk-renderer-counter*)))

(defun get-source-name ()
  (format nil "source~a" (incf *vtk-source-counter*)))

(defun get-mapper-name ()
  (format nil "mapper~a" (incf *vtk-mapper-counter*)))

(defun get-actor-name ()
  (format nil "actor~a" (incf *vtk-actor-counter*)))

(defun get-labelactor-name ()
  (format nil "labelactor~a" (incf *vtk-labelactor-counter*)))

(defun get-trans-name ()
  (format nil "trans~a" (incf *vtk-trans-counter*)))

(defun get-filter-name ()
  (format nil "filter~a" (incf *vtk-filter-counter*)))

(defun get-floatarray-name ()
  (format nil "floatarray~a" (incf *vtk-floatarray-counter*)))

(defun get-data-file-name ()
  (format nil "data~a.vtk" (incf *vtk-data-file-counter*)))

(defun get-points-name ()
  (format nil "points~a" (incf *vtk-points-counter*)))

(defun get-glyphpoints-name ()
  (format nil "glyphpoints~a" (incf *vtk-glyphpoints-counter*)))

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
  (format nil "label~a" (incf *vtk-label-counter*)))

(defun get-chart-name ()
  (format nil "chart~a" (incf *vtk-chart-counter*)))

(defun get-table-name ()
  (format nil "table~a" (incf *vtk-table-counter*)))

(defun get-arrayX-name ()
  (format nil "arrayX~a" (incf *vtk-arrayX-counter*)))

(defun get-arrayY-name ()
  (format nil "arrayY~a" (incf *vtk-arrayY-counter*)))

(defun get-isolines-name ()
  (format nil "isolines~a" (incf *vtk-isolines-counter*)))

(defun scenebounds ()
  (let ((xrange (get-option '$xrange))
        (yrange (get-option '$yrange))
        (zrange (get-option '$zrange))
        code)
    (setf code
      (concatenate 'string
        (if xrange
          (format nil "mib[0]=~a~%mxb[1]=~a~%" (car xrange) (cadr xrange))
          "")
        (if yrange
          (format nil "mib[2]=~a~%mxb[3]=~a~%" (car yrange) (cadr yrange))
          "")
        (if zrange
          (format nil "mib[4]=~a~%mxb[5]=~a~%" (car zrange) (cadr zrange))
          "")))
    (format nil "~a~%~a~%~a~%~a~%~a~%~a~%~a~%~a~a~%~%"
      "if sys.version_info[0] < 3:"
      "    trb = zip(*bounds)"
      "else:"
      "    trb = list(zip(*bounds))"
      "mib = [min(i) for i in trb]"
      "mxb = [max(i) for i in trb]"
      "ranges = vtk.vtkBox()"
      code
      "ranges.SetBounds(mib[0],mxb[1],mib[2],mxb[3],mib[4],mxb[5])") ))

(defun vtkappendpolydata-code (an fe fl)
  (let ((str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0)))
    (when (> (hash-table-count *enhanced3d-or-isolines-code*) 0)
      (format str (format nil "~V@{~a~:*~}~%" 18 "~a~%")
              "def rescalearray( pts, arr ):"
              "   mini = 1.0"
              "   maxi = 0.0"
              "   n = arr.GetNumberOfTuples()"
              "   for i in range(0, n):"
              "       v = pts.GetPoint(i)"
              "       f = arr.GetValue(i)"
              "       if (v[0] >= mib[0] and v[0] <= mxb[1] and"
              "           v[1] >= mib[2] and v[1] <= mxb[3] and"
              "           v[2] >= mib[4] and v[2] <= mxb[5]):"
              "           if f < mini:"
              "              mini = f"
              "           if f > maxi:"
              "              maxi = f"
              "   if maxi > mini:"
              "      for i in range(0, n):"
              "          arr.SetValue(i, (arr.GetValue(i)-mini)/(maxi-mini))"
              "   return;" ))
    (format str "~a=vtk.vtkAppendPolyData()~%" an)
    (loop for n from fe to *vtk-extract-counter* do
      (format str "~%extract~a.SetImplicitFunction(ranges)~%" n)
      (format str "extract~a.SetInputConnection(filter~a.GetOutputPort())~%" n n)
      (format str "mapper~a.SetInputConnection(extract~a.GetOutputPort())~%" n n)
      (when (gethash n *enhanced3d-or-isolines-code*)
        (format str "~a" (gethash n *enhanced3d-or-isolines-code*))
        (remhash n *enhanced3d-or-isolines-code*))
      (format str "actor~a.SetMapper(mapper~a)~%" n n)
      (format str "~a.AddInputConnection(extract~a.GetOutputPort())~%" an n))
    (loop for n from fl to *vtk-label-counter* do
      (format str "~a.AddInputConnection(label~a.GetOutputPort())~%" an n))
    (format str "~%~a.Update()~%~%" an)
    str))

(defun vtkoutlinefilter-code (on an)
  (concatenate 'string
    (format nil "~a=vtk.vtkOutlineFilter()~%" on)
    (format nil "~a.SetInputConnection(~a.GetOutputPort())~%" on an)))

(defun vtkpolydatamapper-code (mn fn con)
  (concatenate 'string
    (format nil "~a=vtk.vtkPolyDataMapper()~%" mn)
    (format nil "~a.GlobalImmediateModeRenderingOn()~%" mn) 
    (if con
      (format nil "~a.SetInputConnection(~a.GetOutputPort())~%" mn fn)
      "") ))

;; Isolines mapper
(defun vtkpolydatamapper-isoline-code (mn sn fn)
  (concatenate 'string
    (format nil "~a=vtk.vtkPolyDataMapper()~%" mn)
    (format nil "~a.GetPointData().SetActiveScalars(\"name~a\")~%" sn fn)
    (format nil "~a.ScalarVisibilityOn()~%" mn)  ))

(defun vtkextractpolydatageometry-code (en cell poin)
  (concatenate 'string
    (format nil "~a=vtk.vtkExtractPolyDataGeometry()~%" en)
    (if cell
      (format nil "~a.ExtractBoundaryCellsOn()~%" en)
      (format nil "~a.ExtractBoundaryCellsOff()~%" en))
    (if poin
      (format nil "~a.PassPointsOn()~%" en)
      (format nil "~a.PassPointsOff()~%" en))  ))

(defun vtktextproperty-code (tn)
  (concatenate 'string
    (format nil "~a=vtk.vtkTextProperty()~%" tn)
    (format nil "~a.SetColor(0,0,0)~%" tn)))

(defun vtkcubeaxesActor2d-code (can adn tn)
  (concatenate 'string
    (format nil "~a=vtk.vtkCubeAxesActor2D()~%" can)
    (format nil "~a.SetInputConnection(~a.GetOutputPort())~%" can adn)
    (format nil "~a.SetLabelFormat(\"%6.4g\")~%" can)
    (format nil "~a.SetFlyModeToOuterEdges()~%" can)
    (format nil "~a.SetFontFactor(0.8)~%" can)
    (format nil "~a.SetAxisTitleTextProperty(~a)~%" can tn)
    (format nil "~a.SetAxisLabelTextProperty(~a)~%" can tn)
    (format nil "~a.SetXLabel(\"~a\")~%"   can (get-option '$xlabel))
    (format nil "~a.SetYLabel(\"~a\")~%"   can (get-option '$ylabel))
    (format nil "~a.SetZLabel(\"~a\")~%~%" can (get-option '$zlabel)) ))

(defun vtkrenderer3d-code (rn an on bgcol cn rv rh fa fl)
  (let* ((k 0.0174532925199433) ; %pi/180
         (rvk (* k rv))
         (rhk (* k rh))
         (x (* ($sin rvk) ($sin rhk)))
         (y (- (* ($sin rvk) ($cos rhk))))
         (z ($cos rvk))
         (colist (hex-to-numeric-list bgcol))
         (str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0)) )
    (format str "~a=vtk.vtkCamera()~%" cn)
    (format str "~a.SetPosition(~a,~a,~a)~%" cn x y z)
    (format str "~a=vtk.vtkRenderer()~%" rn)
    (format str "~a.SetBackground(~a,~a,~a)~%" rn (first colist) (second colist) (third colist))
    (loop for n from fa to *vtk-actor-counter* do
      (format str "~a.AddActor(actor~a)~%" rn n))
    (loop for n from fl to *vtk-labelactor-counter* do
      (format str "~a.AddActor(labelactor~a)~%" rn n))
    (format str "~a.SetCamera(~a.GetActiveCamera())~%" an rn)
    (when (get-option '$axis_3d)
      (format str "~a.AddActor(~a)~%" rn on)     ; add box
      (format str "~a.AddViewProp(~a)~%" rn an)) ; add axes tics
    (format str "~a.SetActiveCamera(~a)~%" rn cn)
    (format str "~a.ResetCamera()~%~%" rn)
    str))

(defun vtkrenderer2d-code (cn rn)
  (let ((colist (hex-to-numeric-list (get-option '$background_color)))
        (xrange (get-option '$xrange))
        (yrange (get-option '$yrange))
        (str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0)) )
    (when yrange
      (format str "~a.GetAxis(0).SetBehavior(1)~%" cn)
      (format str "~a.GetAxis(0).SetRange(~a,~a)~%~%" cn (first yrange) (second yrange)) )
    (when xrange
      (format str "~a.GetAxis(1).SetBehavior(1)~%" cn)
      (format str "~a.GetAxis(1).SetRange(~a,~a)~%~%" cn (first xrange) (second xrange)) )
    (format str "scene~a=vtk.vtkContextScene()~%scene~a.AddItem(~a)~%" cn cn cn)
    (format str "actor~a=vtk.vtkContextActor()~%actor~a.SetScene(scene~a)~%" cn cn cn)
    (format str "~a.SetShowLegend(~a)~%"
      cn
      (if (> *vtk-2dkey-counter* 0) 1 0))
    (format str "~a.GetAxis(0).SetTitle(\"~a\")~%" cn (get-option '$ylabel))
    (format str "~a.GetAxis(1).SetTitle(\"~a\")~%" cn (get-option '$xlabel))
    (when (get-option '$logx)
      (format str "~a.GetAxis(1).SetLogScale(1)~%" cn))
    (when (get-option '$logy)
      (format str "~a.GetAxis(0).SetLogScale(1)~%" cn))
    (let ((pos (get-option '$key_pos))
          vp hp)
      (when pos
        (setf pos (rest (mfunction-call $split pos)))
        (setf vp (first  pos)
              hp (second pos))
        (format str "~a.GetLegend().SetVerticalAlignment(~a)~%"
          cn
          (cond
            ((string= vp "top")    3)
            ((string= vp "center") 1)
            ((string= vp "bottom") 4)))
        (format str "~a.GetLegend().SetHorizontalAlignment(~a)~%"
          cn
          (cond
            ((string= hp "left")   0)
            ((string= hp "center") 1)
            ((string= hp "right")  2)))  ))
    (format str "~a=vtk.vtkRenderer()~%" rn)
    (format str "~a.SetBackground(~a,~a,~a)~%" rn (first colist) (second colist) (third colist))
    (format str "~a.AddActor(actor~a)~%" rn cn)
    (format str "scene~a.SetRenderer(~a)~%" cn rn)
    str))

(defun vtkchartxy-code (cn)
  (let ((str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0)) )
    (format str "~a = vtk.vtkChartXY()~%" cn)
    (format str "~a.GetAxis(0).SetGridVisible(~a)~%"
       cn
       (case (first (get-option '$grid))
         (0 0)
         (otherwise 1))  )
    (format str "~a.GetAxis(1).SetGridVisible(~a)~%~%"
       cn
       (case (second (get-option '$grid))
         (0 0)
         (otherwise 1))  )
    str))

(defun vtkcellarray-code (cn pn celldim ind)
  (let ((str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0)))
    (format str "~a=vtk.vtkCellArray()~%" cn)
    (loop for c in ind do
      (format str "~a.InsertNextCell(~a)~%" cn (length c))
      (loop for i in c do
        (format str "~a.InsertCellPoint(~a)~%" cn i)) )
    (format str "~a.~a(~a)~%"
       pn
       (case celldim
        (0         "SetVerts")
        (1         "SetLines")
        (otherwise "SetPolys"))
       cn)
    str ))

(defun vtkfloatarray-code (fan sn values &optional (addarr t))
  (let ((n (length values))
        (str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0)))
    (format str "~a=vtk.vtkFloatArray()~%" fan)
    (loop for k from 0 below n do
      (format str "~a.InsertNextValue(~a)~%" fan (aref values k)))
    (format str "~a.SetName(\"name~a\")~%" fan fan)
    (format str "~a.GetPointData().~a(~a)~%"
       sn
       (if addarr
          "AddArray"
          "SetScalars")
       fan)
    str))

(defun vtkglyph3d-code (fn sn pdn)
  (concatenate 'string
    (format nil "~a=vtk.vtkGlyph3D()~%" fn)
    (format nil "~a.SetInputData(~a)~%" fn sn)
    (format nil "~a.~a~%" fn pdn)
    (format nil "~a.ScalingOff()~%" fn)))

(defun vtkpoints-code (pn sn x y z)
  (let ((n (length x))
        (str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0)))
    (format str "~a=vtk.vtkPoints()~%" pn)
    (format str "~a.SetNumberOfPoints(~a)~%" pn n)
    (loop for k from 0 below n do
      (format str "~a.InsertPoint(~a,~a,~a,~a)~%" pn k (aref x k) (aref y k) (aref z k)) )
    (when (not (null sn))
      (format str "~a.SetPoints(~a)~%" sn pn))
    str))

(defun vtktransform-code (tn)
  (format nil "~a=vtk.vtkTransform()~%" tn))

(defun vtktransformfilter-code (fn sn tn)
  (concatenate 'string
    (format nil "~a=vtk.vtkTransformFilter()~%" fn)
    (format nil "~a.SetInputConnection(~a.GetOutputPort())~%" fn sn)
    (format nil "~a.SetTransform(~a)~%" fn tn) ))

(defun vtktransformpolydatafilter-code (fn sn tn ds)
  (concatenate 'string
    (format nil "~a=vtk.vtkTransformPolyDataFilter()~%" fn)
    (if ds
      (format nil "~a.SetInputData(~a)~%" fn sn)
      (format nil "~a.SetInputConnection(~a.GetOutputPort())~%" fn sn))
    (format nil "~a.SetTransform(~a)~%" fn tn) ))

(defun vtkactor-code (an mn col op lw ws)
  (let ((colist (hex-to-numeric-list col)))
    (concatenate 'string
      (format nil "~a=vtk.vtkActor()~%" an)
      (if mn
        (format nil "~a.SetMapper(~a)~%" an mn)
        "")
      (format nil "~a.GetProperty().SetColor(~a,~a,~a)~%"
              an
              (first  colist)
              (second colist)
              (third  colist))
      (format nil "~a.GetProperty().SetOpacity(~a)~%" an op)
      (format nil "~a.GetProperty().SetLineWidth(~a)~%" an lw)
      (if (not (null ws))
        (format nil "~a.GetProperty().EdgeVisibilityOn()~%~a.GetProperty().SetEdgeColor(0,0,0)~%" an an)
        (format nil "~%")) )))

;; Isolines actor
(defun vtkactor-isoline-code (an lw)
  (concatenate 'string
    (format nil "~a=vtk.vtkActor()~%" an)
    (format nil "# ~a.GetProperty().SetColor(1.0,0.0,0.0)~%" an)
    (format nil "~a.GetProperty().SetLineWidth(~a)~%" an lw) ))

;; Glyph actor
(defun vtkactor-glyph-code (an col op)
  (let ((colist (hex-to-numeric-list col)))
    (concatenate 'string
      (format nil "~a=vtk.vtkActor()~%" an)
      (format nil "~a.GetProperty().SetColor(~a,~a,~a)~%"
              an
              (first  colist)
              (second colist)
              (third  colist))
      (format nil "~a.GetProperty().SetOpacity(~a)~%" an op) )))

(defun vtktubefilter-code (tn fn lt)
  (concatenate 'string
    (format nil "~a=vtk.vtkTubeFilter()~%" tn)
    (format nil "~a.SetInputConnection(~a.GetOutputPort())~%" tn fn)
    (format nil "~a.SetNumberOfSides(~a)~%" tn (- lt))
    (format nil "~a.SetRadius(~a)~%" tn (get-option '$line_width)) ))


(defun vtkContourFilter-code (in fn)
  (let ((str (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (contours (get-option '$isolines_levels)))
    (format str "~a=vtk.vtkContourFilter()~%" in)
    (format str "~a.SetInputConnection(~a.GetOutputPort())~%" in fn)
    (cond
      ((numberp contours) ; number of isolines
         (format str "~a.GenerateValues(~a,~a.GetOutput().GetScalarRange())~%" in contours fn))
      ((and (stringp contours)
            (search "incremental" contours)); list with min, step, max
         (format str "u = [~a]~%" (subseq contours 12))
         (format str "~a.GenerateValues(int(round((u[2]-u[0]) / u[1])),u[0],u[2])~%" in) )
      (t ; set of user specified contour values 
         (format str "u = [~a]~%" (subseq contours 9))
         (format str "for i in range(0, len(u)):~%")
         (format str "   ~a.SetValue(i,u[i])~%" in)  ))
    str) )


(defun vtkrendererwindow-code (ns)
  (let* ((dim  (get-option '$dimensions))
         (ncol (get-option '$columns))
         (str (make-array 0 
                :element-type 'character 
                :adjustable t 
                :fill-pointer 0))
         x1 y1 x2 y2
         (alloc (reverse *allocations*))
         (nilcounter 0)
         nrow dx dy thisalloc)
    (setf nrow (ceiling (/ (count nil alloc) ncol)))
    (when (> nrow 0)
      (setf dx (/ 1.0 ncol)
            dy (/ 1.0 nrow)))
    ; place scenes on the graphic window
    (loop for counter from 1 to ns do
      (setf thisalloc (car alloc))
      (setf alloc (cdr alloc))
      (cond
        (thisalloc ; user defined scene allocation
            (setf x1 (first thisalloc)
                  y1 (second thisalloc))
            (setf x2 (+ x1 (third thisalloc))
                  y2 (+ y1 (fourth thisalloc))))
        (t ; automatic scene allocation
            (incf nilcounter)
            (setf x1 (* (mod (- nilcounter 1) ncol) dx)
                  x2 (+ x1 dx)
                  y1 (* (- nrow (ceiling nilcounter ncol)) dy)
                  y2 (+ y1 dy))))
      (format str "renderer~a.SetViewport(~a,~a,~a,~a)~%" counter x1 y1 x2 y2))
    (format str "renWin=vtk.vtkRenderWindow()~%renWin.SetMultiSamples(0)~%")
    (format str "renWin.SetSize(~a,~a)~%" (car dim) (cadr dim))
    (loop for k from 1 to ns do
      (format str "renWin.AddRenderer(renderer~a)~%" k))
    str))


; code for file output
(defun vtk-terminal ()
  (let ((terminal  (get-option '$terminal))
        (filename  (get-option '$file_name))
        (offscreenterms '($png $pngcairo $jpg $eps $eps_color $tiff $pnm))
        (extension "")
        (classformat ""))
      (cond
        ((member terminal offscreenterms)
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
          (format nil "~a~%~a~%~a~%~a~%~a~a()~%~a~%~a(\"~a.~a\")~%~a~%~a~%"
            "renWin.OffScreenRenderingOn()"
            "renWin.Render()"
            "w2if=vtk.vtkWindowToImageFilter()"
            "w2if.SetInput(renWin)"
            "writer=vtk." classformat
            "writer.SetInputConnection(w2if.GetOutputPort())"
            "writer.SetFileName" filename extension
            "writer.Write()"
            "exit()"))
       ((eq terminal '$vrml)
          (format nil "~a~%~a~%~a(\"~a.~a\")~%~a~%~a~%~a~%"
            "vrml=vtk.vtkVRMLExporter()"
            "vrml.SetInput(renWin)"
            "vrml.SetFileName" filename "wrl"
            "vrml.SetSpeed(5.5)"
            "vrml.Write()"
            "exit()"))
       ((eq terminal '$obj)
          (format nil "~a~%~a~%~a(\"~a\")~%~a~%~a~%"
            "obj=vtk.vtkOBJExporter()"
            "obj.SetInput(renWin)"
            "obj.SetFilePrefix" filename
            "obj.Write()"
            "exit()"))
       ((eq terminal '$screen)
          (format nil "~a~%~a~%~a~%~a~%~a~%~a~%~a~%"
            "iren=vtk.vtkRenderWindowInteractor()"
            "iren.SetRenderWindow(renWin)"
            "iren.Initialize()"
            "renderer1.ResetCamera()"
            "renderer1.GetActiveCamera().Zoom(1.01)"
            "renWin.Render()"
            "iren.Start()"))
       ((eq terminal '$stl)
          (format nil "~a~%~a~%~a~%~a~%~a~a~a~%~a~%~a~%"
            "triangulator=vtk.vtkTriangleFilter()"
            "triangulator.SetInputConnection(appenddata1.GetOutputPort())"
            "stl=vtk.vtkSTLWriter()"
            "stl.SetInputConnection(triangulator.GetOutputPort())"
            "stl.SetFileName(\"" filename ".stl\")" 
            "stl.Write()"
            "exit()" ))
       ((eq terminal '$ply)
          (format nil "~a~%~a~%~a~%~a~%~a~a~a~%~a~%~a~%"
            "triangulator=vtk.vtkTriangleFilter()"
            "triangulator.SetInputConnection(appenddata1.GetOutputPort())"
            "stl=vtk.vtkPLYWriter()"
            "stl.SetInputConnection(triangulator.GetOutputPort())"
            "stl.SetFileName(\"" filename "\")" 
            "stl.Write()"
            "exit()" ))
       (t
          (merror "draw: unknown terminal for vtk")))))


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
      (0  (setf expr "x = 0"))
      (1  (setf expr "x = 0.5"))
      (2  (setf expr "x = 1"))
      (3  (setf expr "x = x"))
      (4  (setf expr "x = x*x"))
      (5  (setf expr "x = x*x*x"))
      (6  (setf expr "x = x*x*x*x"))
      (7  (setf expr "x = math.sqrt(x)"))
      (8  (setf expr "x = math.sqrt(math.sqrt(x))"))
      (9  (setf expr "x = math.sin(1.570796326794897*x)")) ; %pi/2
      (10 (setf expr "x = math.cos(1.570796326794897*x)"))
      (11 (setf expr "x = math.fabs(x-0.5)"))
      (12 (setf expr "x = (2.0*x-1.0)*(2.0*x-1.0)"))
      (13 (setf expr "x = math.sin(3.141592653589793*x)")) ; %pi
      (14 (setf expr "x = math.fabs(math.cos(3.141592653589793*x))"))
      (15 (setf expr "x = math.sin(6.283185307179586*x)")) ; 2*%pi
      (16 (setf expr "x = math.cos(6.283185307179586*x)"))
      (17 (setf expr "x = math.fabs(math.sin(6.283185307179586*x))"))
      (18 (setf expr "x = math.fabs(math.cos(6.283185307179586*x))"))
      (19 (setf expr "x = math.fabs(math.sin(12.56637061435917*x))")) ; 4*%pi
      (20 (setf expr "x = math.fabs(math.cos(12.56637061435917*x))"))
      (21 (setf expr "x = 3.0*x"))
      (22 (setf expr "x = 3.0*x-1.0"))
      (23 (setf expr "x = 3.0*x-2.0"))
      (24 (setf expr "x = math.fabs(3.0*x-1.0)"))
      (25 (setf expr "x = math.fabs(3.0*x-2.0)"))
      (26 (setf expr "x = 1.5*x-0.5"))
      (27 (setf expr "x = 1.5*x-1.0"))
      (28 (setf expr "x = math.fabs(1.5*x-0.5)"))
      (29 (setf expr "x = math.fabs(1.5*x-1.0)"))
      (30 (setf expr "x = interval(x,0.25,0.57)/0.32-0.78125"))
      (31 (setf expr "x = 2*interval(x,0.42,0.92)-0.84"))
      (32 (setf expr (concatenate 'string
                       (format nil "if x <= 0.42:~%")
                       (format nil "    x = 4.0*x~%")
                       (format nil "  elif x <= 0.92:~%")
                       (format nil "    x = -2.0*x+1.84~%")
                       (format nil "  else:~%")
                       (format nil "    x = x/0.08-11.5"))))
      (33 (setf expr "x = math.fabs(2.0*x-0.5)"))
      (34 (setf expr "x = 2*x"))
      (35 (setf expr "x = 2.0*x-0.5"))
      (36 (setf expr "x = 2.0*x-1.0")))
    (concatenate 'string
      (format nil "def f~a~a (k):~%" c n)
      (format nil "  x = unitscale(k)~%" )
      (format nil "  ~a~%" expr)
      (format nil "  return interval(x,0,1)~%~%"))   ))


; Creates lookup table. See info for option 'palette'.
; Returns list with lookup table name and the string.
(defun check-lookup-table ()
  (let ((palette (get-option '$palette))
        (lut "")
        palette-name lutn)
    (cond ((equal palette '$gray)
             (setf palette '(3 3 3)))
          ((equal palette '$color)
             (setf palette '(7 5 15))) )
    (setf lutn (1+ (length *lookup-tables*)))
    (setf *lookup-tables* (append *lookup-tables* (list palette)))
    (setf palette-name (format nil "lut~a" lutn))
    (cond ((and (listp palette)  ; build lookup table with transform functionsunitscale
                (= (length palette) 3)
                (every #'(lambda (x) (and (integerp x) (<= (abs x) 36))) palette) )
             ; if *unitscale-already-defined* is null, write
             ; tcl functions 'unitscale' and 'unitinterval'
             (when (null *unitscale-already-defined*)
               (setf lut
                 (concatenate 'string
                   (format nil "import math~%~%")
                   (format nil "def unitscale (k):~%")
                   (format nil "  return k/255.0~%~%")
                   (format nil "def interval (x,x0,x1):~%")
                   (format nil "  if x <= x0:~%")
                   (format nil "    return 0~%")
                   (format nil "  if x >= x1:~%")
                   (format nil "    return 1~%")
                   (format nil "  return x~%~%")))
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
                     (format nil "~a=vtk.vtkLookupTable()~%" palette-name)
                     (format nil "~a.SetNumberOfColors(256)~%" palette-name)
                     (format nil "~a.Build()~%" palette-name)
                     (format nil "for i in range (0,256):~%")
                     (format nil "  ~a.SetTableValue(i,fR~a(i),fG~a(i),fB~a(i),1)~%~%"
                             palette-name lutn lutn lutn)))
             (list palette-name lut))

          ((and (listp palette)  ; build user defined lookup table without transparency
             (every #'(lambda (x) (and (listp x) (= (length x) 3))) palette) )
            (list
              palette-name
              (let (triplete
                    (n (length palette)))
                (with-output-to-string (stream)
                  (format stream "~a=vtk.vtkLookupTable()~%" palette-name)
                  (format stream "~a.SetNumberOfColors(~a)~%" palette-name n)
                  (dotimes (k n)
                    (setf triplete (nth k palette))
                    (format stream "~a.SetTableValue(~a,~a,~a,~a,1)~%"
                            palette-name k (car triplete) (cadr triplete) (caddr triplete)))))))

          ((and (listp palette)  ; build user defined lookup table with transparency
             (every #'(lambda (x) (and (listp x) (= (length x) 4))) palette) )
            (list
              palette-name
              (let (triplete
                    (n (length palette)))
                (with-output-to-string (stream)
                  (format stream "~a=vtk.vtkLookupTable()~%" palette-name)
                  (format stream "~a.SetNumberOfColors(~a)~%" palette-name n)
                  (dotimes (k n)
                    (setf triplete (nth k palette))
                    (format stream "~a.SetTableValue(~a,~a,~a,~a,~a)~%"
                            palette-name k (car triplete) (cadr triplete) (caddr triplete) (cadddr triplete))))))))))


(defun build-surface-grid (nx ny)
  (let ((poly nil)
        cont)
    (dotimes (f (1- ny))
      (setf cont (* f nx))
      (dotimes (c (1- nx))
         (setf poly (cons  (list (+ cont c) (+ cont c 1) (+ cont nx c 1) (+ cont nx c)) poly))))
    (reverse poly)))




;;; OBJECT BUILDERS

;; 3d: cone(center, radius, height, direction)
;; -------------------------------------------
(defun vtk3d-cone (cen rad hei dir)
  (let ((color        (get-option '$color))
        (opacity      (get-option '$opacity))
        (linewidth    (get-option '$line_width))
        (wiredsurface (get-option '$wired_surface))
        (capping      (rest (get-option '$capping)))
        (source-name  (get-source-name))
        (mapper-name  (get-mapper-name))
        (actor-name   (get-actor-name))
        (trans-name   (get-trans-name))
        (filter-name  (get-filter-name))
        (fcen         ($float cen))
        (fhei         ($float hei))
        (frad         ($float rad))
        (fdir         ($float dir))
        capn )
    (when (or (not ($listp fcen))
              (not (= ($length fcen) 3))
              (not (every #'floatp (rest fcen))) )
          (merror "draw3d: cone center must be a list of three numbers"))
    (when (or (not (floatp frad))
              (<= frad 0.0))
          (merror "draw3d: cone radius must be a number greater than zero"))
    (when (or (not (floatp fhei))
              (<= fhei 0.0))
          (merror "draw3d: cone height must be a number greater than zero"))
    (when (or (not ($listp fdir))
              (not (= ($length fdir) 3))
              (not (every #'floatp (rest fdir))) )
          (merror "draw3d: cone direction must be a list of three numbers"))
    (if (first capping)
      (setf capn 1)
      (setf capn 0))
    (concatenate 'string
      (format nil "~a=vtk.vtkConeSource()~%" source-name)
      (format nil "~a.SetHeight(~a)~%" source-name fhei)
      (format nil "~a.SetRadius(~a)~%" source-name frad)
      (format nil "~a.SetCenter(~a,~a,~a)~%"
              source-name
              (cadr fcen)
              (caddr fcen)
              (cadddr fcen))
      (format nil "~a.SetDirection(~a,~a,~a)~%"
              source-name
              (cadr fdir)
              (caddr fdir)
              (cadddr fdir))
      (format nil "~a.SetResolution(~a)~%" source-name 30)
      (format nil "~a.SetCapping(~a)~%" source-name capn)
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name nil)
      (vtkpolydatamapper-code mapper-name filter-name t)
      (format nil "bounds.append(~a.GetBounds())~%" mapper-name)
      (vtkactor-code actor-name mapper-name color opacity linewidth wiredsurface)
      (format nil "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))  )))



;; 3d: prism(center, n, edgepoint, height, direction)
;; --------------------------------------------------

; rotates point (x,y,z) rad radians around the line with unitary direction
; vector (u,v,w) containing point (a,b,c)
(defun rotate (x y z a b c u v w rad)
    (list
      (+ (* (cos rad) x)
         (* ($sin rad) (+ (* -1 c v) (* b w) (* -1 w y) (* v z)))
         (* (+ 1 (* -1 ($cos rad)))
            (+ (* a (+ (expt v 2) (expt w 2)))
               (* -1 u (+ (* b v) (* c w) (* -1 u x) (* -1 v y) (* -1 w z)))))) 
      (+ (* ($cos rad) y)
         (* ($sin rad) (+ (* c u) (* -1 a w) (* w x) (* -1 u z)))
         (* (+ 1 (* -1 ($cos rad)))
            (+ (* b (+ (expt u 2) (expt w 2)))
               (* -1 v (+ (* a u) (* c w) (* -1 u x) (* -1 v y) (* -1 w z)))))) 
      (+ (* ($sin rad) (+ (* -1 b u) (* a v) (* -1 v x) (* u y)))
         (* ($cos rad) z)
         (* (+ 1 (* -1 ($cos rad)))
            (+ (* c (+ (expt u 2) (expt v 2)))
               (* -1 w (+ (* a u) (* b v) (* -1 u x) (* -1 v y) (* -1 w z))))))   ))

(defun vtk3d-prism (cen n edgp hei dir)
  (let ((color          (get-option '$color))
        (opacity        (get-option '$opacity))
        (linewidth      (get-option '$line_width))
        (wiredsurface   (get-option '$wired_surface))
        (capping        (rest (get-option '$capping)))
        (source-name    (get-source-name))
        (points-name    (get-points-name))
        (cellarray-name (get-cellarray-name))
        (mapper-name    (get-mapper-name))
        (actor-name     (get-actor-name))
        (trans-name     (get-trans-name))
        (filter-name    (get-filter-name))
        (str            (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (fcen           ($float cen))
        (fedgp          ($float edgp))
        (fhei           ($float hei))
        (fdir           ($float dir))
        (ang            (/ 6.283185307179586 n)) ; = 2*%pi/n
        (xcount -1)
        (ycount -1)
        (zcount -1)
        dirmod c1 c2 c3 d1 d2 d3 p1 p2 p3 du1 du2 du3 v v1 v2 v3 x y z)
    (when (or (not ($listp fcen))
              (not (= ($length fcen) 3))
              (not (every #'floatp (rest fcen))) )
          (merror "draw3d: prism center must be a list of three numbers"))
    (when (or (not ($listp fedgp))
              (not (= ($length fedgp) 3))
              (not (every #'floatp (rest fedgp))) )
          (merror "draw3d: point on prism edge must be a list of three numbers"))
    (when (or (not (floatp fhei))
              (<= fhei 0.0))
          (merror "draw3d: prism height must be a number greater than zero"))
    (when (or (not ($listp fdir))
              (not (= ($length fdir) 3))
              (not (every #'floatp (rest fdir))) )
          (merror "draw3d: cylinder direction must be a list of three numbers"))
    ; direction vector with module = height/2
    (setf c1 (cadr fcen)  c2 (caddr fcen)  c3 (cadddr fcen)
          p1 (cadr fedgp) p2 (caddr fedgp) p3 (cadddr fedgp)
          d1 (cadr fdir)  d2 (caddr fdir)  d3 (cadddr fdir))
    (setf dirmod (sqrt (+ (* d1 d1) (* d2 d2) (* d3 d3))))
    (when (= dirmod 0.0)
      (setf d1 0 d2 0 d3 1 dirmod 1))
    (setf du1 (/ (* d1 fhei) (* 2 dirmod))
          du2 (/ (* d2 fhei) (* 2 dirmod))
          du3 (/ (* d3 fhei) (* 2 dirmod)))
    ; intersection between edge and perpendicular plane passing through center:
    ; linsolve([d1*v1+d2*v2+d3*v3=d1*c1+d2*c2+d3*c3,
    ;           d2*v1-d1*v2      =d2*p1-d1*p2,
    ;                 d3*v2-d2*v3=d3*p2-d2*p3], [v1,v2,v3]);
    (let ((den (* dirmod dirmod)))
      (setf v1 (/ (+ (* c1 d1 d1) (* c2 d1 d2) (* c3 d1 d3) (* d2 d2 p1) (* d3 d3 p1) (* -1 d1 d2 p2) (* -1 d1 d3 p3))
                  den)
            v2 (/ (+ (* c1 d1 d2) (* c2 d2 d2) (* c3 d2 d3) (* -1 d1 d2 p1) (* d1 d1 p2) (* d3 d3 p2) (* -1 d2 d3 p3))
                  den)
            v3 (/ (+ (* c1 d1 d3) (* c2 d2 d3) (* c3 d3 d3) (* -1 d1 d3 p1) (* -1 d2 d3 p2) (* d1 d1 p3) (* d2 d2 p3)) 
                  den)))
    ; rotate n times 2 pi/n and save the vertices of the lateral rectangles
    (setf x (make-array (* (+ 2 (count t capping)) (+ n 1) ) :element-type 'flonum))
    (setf y (make-array (* (+ 2 (count t capping)) (+ n 1) ) :element-type 'flonum))
    (setf z (make-array (* (+ 2 (count t capping)) (+ n 1) ) :element-type 'flonum))
    (loop for s from 0 to n do
      ; bottom capping?
      (when (equal (second capping) t)
        (setf (aref x (incf xcount)) (- c1 du1))
        (setf (aref y (incf ycount)) (- c2 du2))
        (setf (aref z (incf zcount)) (- c3 du3)))
      (setf v (rotate v1 v2 v3 c1 c2 c3 (/ d1 dirmod) (/ d2 dirmod) (/ d3 dirmod) (* s ang)) )
      (setf (aref x (incf xcount)) (- (first v) du1))
      (setf (aref y (incf ycount)) (- (second v) du2))
      (setf (aref z (incf zcount)) (- (third v) du3))
      (setf (aref x (incf xcount)) (+ (first v) du1))
      (setf (aref y (incf ycount)) (+ (second v) du2))
      (setf (aref z (incf zcount)) (+ (third v) du3))
      ; top capping?
      (when (equal (first capping) t)
        (setf (aref x (incf xcount)) (+ c1 du1))
        (setf (aref y (incf ycount)) (+ c2 du2))
        (setf (aref z (incf zcount)) (+ c3 du3))) ) ; end loop

    ; python code
    (format str "~a=vtk.vtkPolyData()~%" source-name)
    (format str "~a~%" (vtkpoints-code points-name source-name x y z))
    (format str "~a~%" (vtkcellarray-code cellarray-name source-name 2
                                         (build-surface-grid (+ 2 (count t capping)) (+ n 1))))
    (format str "~a~%" (vtktransform-code trans-name))
    (format str "~a~%" (vtktransformpolydatafilter-code filter-name source-name trans-name t))
    (format str "~a~%" (vtkpolydatamapper-code mapper-name filter-name t))
    (format str "bounds.append(~a.GetBounds())~%" mapper-name)
    (format str "~a~%" (vtkactor-code actor-name mapper-name color opacity linewidth wiredsurface))
    (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))
  str))



;; 3d: cylinder(center, radius, height, direction)
;; -----------------------------------------------
(defun vtk3d-cylinder (cen rad hei dir)
  (let ((color        (get-option '$color))
        (opacity      (get-option '$opacity))
        (linewidth    (get-option '$line_width))
        (wiredsurface (get-option '$wired_surface))
        (capping      (rest (get-option '$capping)))
        (source-name  (get-source-name))
        (mapper-name  (get-mapper-name))
        (actor-name   (get-actor-name))
        (trans-name   (get-trans-name))
        (filter-name  (get-filter-name))
        (fcen         ($float cen))
        (frad         ($float rad))
        (fhei         ($float hei))
        (fdir         ($float dir))
        capn dirmod xrot yrot zrot)
    (when (or (not ($listp fcen))
              (not (= ($length fcen) 3))
              (not (every #'floatp (rest fcen))) )
          (merror "draw3d: cylinder center must be a list of three numbers"))
    (when (or (not (floatp fhei))
              (<= fhei 0.0))
          (merror "draw3d: cylinder height must be a number greater than zero"))
    (when (or (not (floatp frad))
              (<= frad 0.0))
          (merror "draw3d: cylinder radius must be a number greater than zero"))
    (when (or (not ($listp fdir))
              (not (= ($length fdir) 3))
              (not (every #'floatp (rest fdir))) )
          (merror "draw3d: cylinder direction must be a list of three numbers"))
    (if (first capping)
      (setf capn 1)
      (setf capn 0))
    (setf dirmod (sqrt (+ (* (cadr fdir)   (cadr fdir))
                          (* (caddr fdir)  (caddr fdir))
                          (* (cadddr fdir) (cadddr fdir)))))
    (cond
      ((= dirmod 0.0)
         ; we use the same default direction used by cones,
         ; which is the positive X-direction
         (setf xrot 0.0
               yrot 0.0
               zrot -90.0))
      ((= (caddr fdir) 0.0)
         (setf xrot 90.0
               yrot (* 57.29577951308232
                       ($acos (/ (cadddr fdir)
                                 (sqrt (+ (* (cadr fdir)   (cadr fdir))
                                          (* (cadddr fdir) (cadddr fdir)) )))))
               zrot 0.0))
      ((< (caddr fdir) 0.0)
         (setf xrot (* 57.29577951308232 ($asin (/ (cadddr fdir) dirmod)))
               yrot 0.0
               zrot (* 57.29577951308232 (+ -3.141592653589793 (- ($atan (/ (cadr fdir) (caddr fdir))))))))
      (t
         (setf xrot (* 57.29577951308232 ($asin (/ (cadddr fdir) dirmod)))
               yrot 0.0
               zrot (* 57.29577951308232 (- ($atan (/ (cadr fdir) (caddr fdir)))))))  )
    ; python code
    (concatenate 'string
      (format nil "~a=vtk.vtkCylinderSource()~%" source-name)
      (format nil "~a.SetHeight(~a)~%" source-name fhei)
      (format nil "~a.SetRadius(~a)~%" source-name frad)
      (format nil "~a.SetResolution(~a)~%" source-name 30)
      (format nil "~a.SetCapping(~a)~%" source-name capn)
      (vtktransform-code trans-name)
      (format nil "~a.Translate(~a,~a,~a)~%"
              trans-name
              (cadr fcen)
              (caddr fcen)
              (cadddr fcen))
      ; rotations are made in reverse order as indicated here
      (format nil "~a.RotateZ(~a)~%" trans-name zrot) ; azimuth
      (format nil "~a.RotateY(~a)~%" trans-name yrot)
      (format nil "~a.RotateX(~a)~%" trans-name xrot) ; elevation
      (vtktransformpolydatafilter-code filter-name source-name trans-name nil)
      (vtkpolydatamapper-code mapper-name filter-name t)
      (format nil "bounds.append(~a.GetBounds())~%" mapper-name)
      (vtkactor-code actor-name mapper-name color opacity linewidth wiredsurface)
      (format nil "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))  )))



;; 3d: cube(xlength, ylength, zlength, center)
;; -------------------------------------------
(defun vtk3d-cube (xlen ylen zlen cen)
  (let ((color        (get-option '$color))
        (opacity      (get-option '$opacity))
        (linewidth    (get-option '$line_width))
        (wiredsurface (get-option '$wired_surface))
        (source-name  (get-source-name))
        (mapper-name  (get-mapper-name))
        (actor-name   (get-actor-name))
        (trans-name   (get-trans-name))
        (filter-name  (get-filter-name))
        (fxlen        ($float xlen))
        (fylen        ($float ylen))
        (fzlen        ($float zlen))
        (fcen         ($float cen)) )
    (when (or (not (floatp fxlen))
              (<= fxlen 0.0))
          (merror "draw3d: cube x-length must be a number greater than zero"))
    (when (or (not (floatp fylen))
              (<= fylen 0.0))
          (merror "draw3d: cube y-length must be a number greater than zero"))
    (when (or (not (floatp fzlen))
              (<= fzlen 0.0))
          (merror "draw3d: cube z-length must be a number greater than zero"))
    (when (or (not ($listp fcen))
              (not (= ($length fcen) 3))
              (not (every #'floatp (rest fcen))) )
          (merror "draw3d: cube center must be a list of three floats"))
    ; python code
    (concatenate 'string
      (format nil "~a=vtk.vtkCubeSource()~%" source-name)
      (format nil "~a.SetXLength(~a)~%" source-name fxlen)
      (format nil "~a.SetYLength(~a)~%" source-name fylen)
      (format nil "~a.SetZLength(~a)~%" source-name fzlen)
      (format nil "~a.SetCenter(~a,~a,~a)~%"
              source-name
              (cadr fcen)
              (caddr fcen)
              (cadddr fcen))
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name nil)
      (vtkpolydatamapper-code mapper-name filter-name t)
      (format nil "bounds.append(~a.GetBounds())~%" mapper-name) 
      (vtkactor-code actor-name mapper-name color opacity linewidth wiredsurface)
      (format nil "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil)) )))



;; 3d: sphere(center, radius)
;; --------------------------
(defun vtk3d-sphere (cen rad)
  (let ((color        (get-option '$color))
        (opacity      (get-option '$opacity))
        (linewidth    (get-option '$line_width))
        (wiredsurface (get-option '$wired_surface))
        (source-name  (get-source-name))
        (mapper-name  (get-mapper-name))
        (actor-name   (get-actor-name))
        (trans-name   (get-trans-name))
        (filter-name  (get-filter-name))
        (frad         ($float rad))
        (fcen         ($float cen))  )
    (when (or (not ($listp fcen))
              (not (= ($length fcen) 3))
              (not (every #'floatp (rest fcen))) )
          (merror "draw3d: sphere center must be a list of three numbers"))
    (when (or (not (floatp frad))
              (<= frad 0.0))
          (merror "draw3d: sphere radius must be a number greater than zero"))

    ; python code
    (concatenate 'string
      (format nil "~a=vtk.vtkSphereSource()~%" source-name)
      (format nil "~a.SetRadius(~a)~%" source-name frad)
      (format nil "~a.SetCenter(~a,~a,~a)~%"
              source-name
              (cadr fcen)
              (caddr fcen)
              (cadddr fcen))
      (format nil "~a.SetThetaResolution(~a)~%" source-name 30)
      (format nil "~a.SetPhiResolution(~a)~%" source-name 30)
      (vtktransform-code trans-name)
      (vtktransformpolydatafilter-code filter-name source-name trans-name nil)
      (vtkpolydatamapper-code mapper-name filter-name t)
      (format nil "bounds.append(~a.GetBounds())~%" mapper-name)
      (vtkactor-code actor-name mapper-name color opacity linewidth wiredsurface)
      (format nil "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil)) )))



;; 3d: parallelogram(origin, point1, point2)
;; -----------------------------------------
;; The parallelogram is defined by one vertex and the two other adjacent vertices
(defun vtk3d-parallelogram (ori p1 p2)
  (let ((color        (get-option '$color))
        (opacity      (get-option '$opacity))
        (linewidth    (get-option '$line_width))
        (wiredsurface (get-option '$wired_surface))
        (source-name  (get-source-name))
        (trans-name   (get-trans-name))
        (filter-name  (get-filter-name))
        (mapper-name  (get-mapper-name))
        (actor-name   (get-actor-name))
        (str          (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (fori         (map 'list #'$float (rest ori)))
        (fp1          (map 'list #'$float (rest p1)))
        (fp2          (map 'list #'$float (rest p2)))
        xx yy zz a1 b1 c1 a2 b2 c2)
    (when (notevery #'(lambda (z) (floatp z))
                    (append fori fp1 fp2))
      (merror "vtk3d: arguments to parallelogram must be lists of floats"))

    ; python code
    (format str "~a=vtk.vtkPlaneSource()~%" source-name)
    (setf xx (car   fori)
          yy (cadr  fori)
          zz (caddr fori))
    (transform-point 3)
    (setf a1 xx
          b1 yy
          c1 zz)
    (format str "~a.SetOrigin(~a,~a,~a)~%" source-name xx yy zz)
    (setf xx (car   fp1)
          yy (cadr  fp1)
          zz (caddr fp1))
    (transform-point 3)
    (if (and (= a1 xx) (= b1 yy) (= c1 zz))
      (merror "vtk3d (parallelogram): three distinct vertices are needed.")
      (setf a2 xx
            b2 yy
            c2 zz) )
    (format str "~a.SetPoint1(~a,~a,~a)~%" source-name xx yy zz)
    (setf xx (car   fp2)
          yy (cadr  fp2)
          zz (caddr fp2))
    (transform-point 3)
    (when (or (and (= a1 xx) (= b1 yy) (= c1 zz))
              (and (= a2 xx) (= b2 yy) (= c2 zz)) )
      (merror "vtk3d: we need three distinct vertices to draw a parallelogram.") )
    (format str "~a.SetPoint2(~a,~a,~a)~%" source-name xx yy zz)
    (format nil "~a.SetXResolution(~a)~%" source-name 10)
    (format str "~a.SetYResolution(~a)~%" source-name 10)
    (format str "~a" (vtktransform-code trans-name))
    (format str "~a" (vtktransformpolydatafilter-code filter-name source-name trans-name nil))
    (format str "~a" (vtkpolydatamapper-code mapper-name filter-name t))
    (format str "bounds.append(~a.GetBounds())~%" mapper-name)
    (format str "~a" (vtkactor-code actor-name mapper-name color opacity linewidth wiredsurface))
    (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))
  str))



;; 3d: triangle(vertex1, vertex2, vertex3)
;; ---------------------------------------
;; The triangle is defined by three vertices
(defun vtk3d-triangle (v1 v2 v3)
  (let ((color         (get-option '$color))
        (opacity       (get-option '$opacity))
        (linewidth     (get-option '$line_width))
        (wiredsurface  (get-option '$wired_surface))
        (points-name   (get-points-name))
        (triangle-name (get-triangle-name))
        (polydata-name (get-polydata-name))
        (trans-name    (get-trans-name))
        (filter-name   (get-filter-name))
        (mapper-name   (get-mapper-name))
        (actor-name    (get-actor-name))
        (str           (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (fv1           (map 'list #'$float (rest v1)))
        (fv2           (map 'list #'$float (rest v2)))
        (fv3           (map 'list #'$float (rest v3)))
        xx yy zz)
    (when (notevery #'(lambda (z) (floatp z))
                    (append fv1 fv2 fv3))
      (merror "vtk3d (triangle): arguments must be lists of three numbers"))
    ; python code
    (format str "~a=vtk.vtkPoints()~%" points-name)
    (format str "~a.SetNumberOfPoints(3)~%" points-name)
    (setf xx (car   fv1)
          yy (cadr  fv1)
          zz (caddr fv1))
    (transform-point 3)
    (format str "~a.InsertPoint(0,~a,~a,~a)~%" points-name xx yy zz)
    (setf xx (car   fv2)
          yy (cadr  fv2)
          zz (caddr fv2))
    (transform-point 3)
    (format str "~a.InsertPoint(1,~a,~a,~a)~%" points-name xx yy zz)
    (setf xx (car   fv3)
          yy (cadr  fv3)
          zz (caddr fv3))
    (transform-point 3)
    (format str "~a.InsertPoint(2,~a,~a,~a)~%" points-name xx yy zz)
    (format str "~a=vtk.vtkTriangle()~%" triangle-name)
    (format str "~a.GetPointIds().SetId(0,0)~%" triangle-name)
    (format str "~a.GetPointIds().SetId(1,1)~%" triangle-name)
    (format str "~a.GetPointIds().SetId(2,2)~%" triangle-name)
    (format str "~a=vtk.vtkPolyData()~%" polydata-name)
    (format str "~a.Allocate(1,1)~%" polydata-name)
    (format str "~a.InsertNextCell(~a.GetCellType(),~a.GetPointIds())~%"
            polydata-name
            triangle-name
            triangle-name)
    (format str "~a.SetPoints(~a)~%" polydata-name points-name)
    (format str "~a" (vtktransform-code trans-name))
    (format str "~a" (vtktransformpolydatafilter-code filter-name polydata-name trans-name t))
    (format str "~a=vtk.vtkPolyDataMapper()~%" mapper-name)
    (format str "~a.SetInputData(~a)~%" mapper-name polydata-name)
    (format str "bounds.append(~a.GetBounds())~%" mapper-name)
    (format str "~a" (vtkactor-code actor-name mapper-name color opacity linewidth wiredsurface))
    (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))
  str))



;; 3d: vector([x,y,z], [dx,dy,dz])
;; ---------------------------
(defun vtk3d-vector (arg1 arg2)
  (when (or (not ($listp arg1))
            (not (= ($length arg1) 3))
            (not ($listp arg2))
            (not (= ($length arg2) 3)))
      (merror "vtk3d (vector): coordinates are not correct"))
  (let ((color        (get-option '$color))
        (head-length  (get-option '$head_length))
        (head-angle   (get-option '$head_angle))
        (line-width   (get-option '$line_width))
        (unit-vectors (get-option '$unit_vectors))
        (opacity      (get-option '$opacity))
        (wiredsurface (get-option '$wired_surface))
        (source-name  (get-source-name))
        (trans-name   (get-trans-name))
        (filter-name  (get-filter-name))
        (mapper-name  (get-mapper-name))
        (actor-name   (get-actor-name))
        (x  ($float   (cadr arg1)))
        (y  ($float   (caddr arg1)))
        (z  ($float   (cadddr arg1)))
        (dx ($float   (cadr arg2)))
        (dy ($float   (caddr arg2)))
        (dz ($float   (cadddr arg2)))
        ndx ndy ndz radians tiplength module radius rotangle)
    ; unitary vector
    (setf module (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
    (setf ndx (/ dx module)
          ndy (/ dy module)
          ndz (/ dz module))
    ; transform into unitary vector when unit_vectors=true
    (when unit-vectors
      (setf module 1))
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

    ; python code
    (concatenate 'string
      (format nil "~a=vtk.vtkArrowSource()~%" source-name)
      (format nil "~a.SetTipResolution(~a)~%" source-name 20)
      (format nil "~a.SetTipRadius(~a)~%" source-name (/ radius module))
      (format nil "~a.SetTipLength(~a)~%" source-name (/ tiplength module))
      (format nil "~a.SetShaftResolution(~a)~%" source-name 10)
      (format nil "~a.SetShaftRadius(~a)~%" source-name (/ line-width module))
      (vtktransform-code trans-name)
      (format nil "~a.Translate(~a,~a,~a)~%" trans-name x y z)
      (format nil "~a.RotateWXYZ(~a,~a,~a,~a)~%" trans-name rotangle 0 (- ndz) ndy)
      (format nil "~a.Scale(~a,~a,~a)~%" trans-name module module module)
      (vtktransformfilter-code filter-name source-name trans-name)
      (vtkpolydatamapper-code mapper-name filter-name t)
      (format nil "bounds.append(~a.GetBounds())~%" mapper-name)
      (vtkactor-code actor-name mapper-name color opacity line-width wiredsurface)
      (format nil "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil)) )))



;; 3D: points([[x1,y1,z1], [x2,y2,z2], [x3,y3,z3],...])
;; ----------------------------------------------------
(defun vtk3d-points (arg)
  (let ((points-joined   (get-option '$points_joined))
        (point-type      (get-option '$point_type))
        (point-size      ($float (get-option '$point_size)))
        (line-type       (get-option '$line_type))
        (color           (get-option '$color))
        (opacity         (get-option '$opacity))
        (linewidth       (get-option '$line_width))
        (wiredsurface    (get-option '$wired_surface))
        (source-name     (get-source-name))
        (points-name     (get-points-name))
        (floatarray-name (get-floatarray-name))
        (tmp             (mapcar #'rest (rest arg)))
        (output-string   "")
        (minscalar       most-positive-double-float)
        (maxscalar       most-negative-double-float)
        source-name2 points-name2 filter-name trans-name mapper-name actor-name polydata-name 
        cellarray-name cellarray-name2 solidsource-name lookup-table-name glyphpoints-name
        newscalar slope scalars x y z ax ay az n)

    (setf n ($length arg))
    ; create array of points
    (setf x (map 'list #'$float (map 'list #'first tmp))
          y (map 'list #'$float (map 'list #'second tmp))
          z (map 'list #'$float (map 'list #'third tmp)) )
    (transform-lists 3)
    (setf ax (make-array n :element-type 'flonum :initial-contents x)
          ay (make-array n :element-type 'flonum :initial-contents y)
          az (make-array n :element-type 'flonum :initial-contents z))

    ; check enhanced3d model
    (check-enhanced3d-model "points" '(0 1 3))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array n :element-type 'flonum))
      (cond
        ((= *draw-enhanced3d-type* 1)
           (dotimes (k n)
             (setf newscalar (funcall *draw-enhanced3d-fun* k))
             (when (< newscalar minscalar) (setf minscalar newscalar))
             (when (> newscalar maxscalar) (setf maxscalar newscalar))
             (setf (aref scalars k) newscalar )) )
        ((= *draw-enhanced3d-type* 3)
           (dotimes (k n)
             (setf newscalar (funcall *draw-enhanced3d-fun* (aref ax k) (aref ay k) (aref az k)))
             (when (< newscalar minscalar) (setf minscalar newscalar))
             (when (> newscalar maxscalar) (setf maxscalar newscalar))
             (setf (aref scalars k) newscalar))))
      (if (< minscalar maxscalar)
        (setf slope (/ 1.0 (- maxscalar minscalar)))
        (setf slope 0.0)) 
      ; rescale array of scalars to interval [0,1]
      (loop for s from 0 below (length scalars) do
        (setf (aref scalars s) (* slope (- (aref scalars s) minscalar))))
      (let ((lut (check-lookup-table)))
        (setf lookup-table-name (car lut))
        (setf output-string (cadr lut))) )

    ; python code
    (setf output-string
      (concatenate 'string
        output-string
        (format nil "~a=vtk.vtkPolyData()~%" source-name)
        (vtkpoints-code points-name source-name ax ay az)
        (when (> *draw-enhanced3d-type* 0)
          (vtkfloatarray-code floatarray-name source-name scalars nil) )))
    (when points-joined ; true or impulses
      (setf trans-name       (get-trans-name)
            filter-name      (get-filter-name)
            mapper-name      (get-mapper-name)
            polydata-name    (get-polydata-name)
            cellarray-name   (get-cellarray-name)
            actor-name       (get-actor-name))
      (setf output-string
        (concatenate 'string
          output-string
          (cond
            ((eql points-joined '$impulses)
               (setf source-name2    (get-source-name)
                     points-name2    (get-points-name)
                     cellarray-name2 (get-cellarray-name))
               (concatenate 'string
                 (format nil "~a=vtk.vtkPolyData()~%" source-name2)
                 (let ((xx (make-array (* 2 n) :element-type 'flonum))
                       (yy (make-array (* 2 n) :element-type 'flonum))
                       (zz (make-array (* 2 n) :element-type 'flonum))
                       (ind 0))
                   (loop for k from 0 below n do
                     (setf (aref xx ind) (aref ax k)
                           (aref yy ind) (aref ay k)
                           (aref zz ind) 0.0)
                     (setf ind (1+ ind))
                     (setf (aref xx ind) (aref ax k)
                           (aref yy ind) (aref ay k)
                           (aref zz ind) (aref az k))
                     (setf ind (1+ ind)) )
                   (vtkpoints-code points-name2 source-name2 xx yy zz))
                 (vtkcellarray-code cellarray-name2 source-name2 1 
                                 (loop for k from 0 below n collect (list (* 2 k) (+ (* 2 k) 1))))
                 (vtktransform-code trans-name)

                 (cond
                   ((< line-type 0) ; line type is a tube
                      (concatenate 'string
                        (vtktransformpolydatafilter-code "auxfiltertube" source-name2 trans-name t)
                        (vtktubefilter-code filter-name "auxfiltertube" line-type)
                        (vtkpolydatamapper-code mapper-name filter-name t)))
                   (t
                      (concatenate 'string
                        (vtktransformpolydatafilter-code filter-name source-name2 trans-name t)
                        (vtkpolydatamapper-code mapper-name filter-name t))))))
            (t
               (concatenate 'string
                 (vtkcellarray-code cellarray-name source-name 1 (list (loop for k from 0 below n collect k)))
                 (vtktransform-code trans-name)
                 (cond
                   ((< line-type 0) ; line type is a tube
                      (concatenate 'string
                        (vtktransformpolydatafilter-code "auxfiltertube" source-name trans-name t)
                        (vtktubefilter-code filter-name "auxfiltertube" line-type)
                        (vtkpolydatamapper-code mapper-name filter-name t)))
                   (t
                      (concatenate 'string
                        (vtktransformpolydatafilter-code filter-name source-name trans-name t)
                        (vtkpolydatamapper-code mapper-name filter-name t))) ) ) ))

          (if (> *draw-enhanced3d-type* 0)
            (format nil "~a.SetLookupTable(~a)~%" mapper-name lookup-table-name)
            "")

          (format nil "bounds.append(~a.GetBounds())~%" mapper-name)
          (format nil "~a~%" (vtkactor-code actor-name nil color opacity linewidth wiredsurface))
          (format nil "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))

          (if (>= line-type 0) ; when line type is not a tube, set line pattern
            (format nil "~a.GetProperty().SetLineStipplePattern(~a)~%~%"
                    actor-name
                    (case line-type
                       (0 "0x0001")
                       (1 "0xFFFF")
                       (2 "0xFF00")
                       (6 "0xFE10")))
            ""))))

    ; draw glyphs according to point-type
    (cond
      ((and (>= point-type 0)
            (<= point-type 5))
         (setf glyphpoints-name (get-glyphpoints-name)
               polydata-name    (get-polydata-name)
               cellarray-name   (get-cellarray-name)
               filter-name      (get-filter-name)
               mapper-name      (get-mapper-name)
               actor-name       (get-actor-name)
               color            (get-option '$color)
               opacity          (get-option '$opacity)
               linewidth        (get-option '$line_width) )

         (setf output-string
           (concatenate 'string
             output-string
             (case point-type
               (0 (vtkpoints-code glyphpoints-name nil
                     (make-array 1 :element-type 'flonum :initial-element 0.0)
                     (make-array 1 :element-type 'flonum :initial-element 0.0)
                     (make-array 1 :element-type 'flonum :initial-element 0.0)))
               (1 (vtkpoints-code glyphpoints-name nil 
                     (make-array 4 :element-type 'flonum
                                   :initial-contents (list (- point-size) point-size 0.0 0.0))
                     (make-array 4 :element-type 'flonum
                                   :initial-contents (list 0.0 0.0 0.0 0.0))
                     (make-array 4 :element-type 'flonum
                                   :initial-contents (list 0.0 0.0 (- point-size) point-size))))
               (2 (vtkpoints-code glyphpoints-name nil
                     (make-array 4 :element-type 'flonum
                                   :initial-contents (list point-size (- point-size) (- point-size) point-size))
                     (make-array 4 :element-type 'flonum 
                                   :initial-contents (list 0.0 0.0 0.0 0.0))
                     (make-array 4 :element-type 'flonum 
                                   :initial-contents (list point-size (- point-size) point-size (- point-size)))))
               (3 (vtkpoints-code glyphpoints-name nil
                     (make-array 8 :element-type 'flonum 
                                   :initial-contents (list point-size (- point-size) (- point-size) point-size 
                                                           (- point-size) point-size 0.0 0.0))
                     (make-array 8 :element-type 'flonum 
                                   :initial-contents (list 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))
                     (make-array 8 :element-type 'flonum 
                                   :initial-contents (list point-size (- point-size) point-size (- point-size)
                                                           0.0 0.0 (- point-size) point-size))))
               (4 (vtkpoints-code glyphpoints-name nil
                     (make-array 4 :element-type 'flonum 
                                   :initial-contents (list point-size (- point-size) (- point-size) point-size))
                     (make-array 4 :element-type 'flonum 
                                   :initial-contents (list 0.0 0.0 0.0 0.0))
                     (make-array 4 :element-type 'flonum 
                                   :initial-contents (list point-size point-size (- point-size) (- point-size)))))
               (5 (vtkpoints-code glyphpoints-name nil
                     (make-array 4 :element-type 'flonum 
                                   :initial-contents (list (- point-size) point-size point-size (- point-size)))
                     (make-array 4 :element-type 'flonum 
                                   :initial-contents (list 0.0 0.0 0.0 0.0))
                     (make-array 4 :element-type 'flonum 
                                   :initial-contents (list point-size point-size (- point-size) (- point-size))))))
             (format nil "~a=vtk.vtkPolyData()~%" polydata-name)
             (format nil "~a.SetPoints(~a)~%" polydata-name glyphpoints-name)
             (case point-type
               (0     (vtkcellarray-code cellarray-name polydata-name 0 '((0))))
               ((1 2) (vtkcellarray-code cellarray-name polydata-name 1 '((0 1) (2 3))))
               (3     (vtkcellarray-code cellarray-name polydata-name 1 '((0 1) (2 3) (4 5) (6 7))))
               (4     (vtkcellarray-code cellarray-name polydata-name 1 '((0 1) (1 2) (2 3) (3 0))))
               (5     (vtkcellarray-code cellarray-name polydata-name 2 '((0 1 2 3))))
               (otherwise ""))
             (vtkglyph3d-code filter-name source-name (format nil "SetSourceData(~a)" polydata-name))
             (format nil "~a~%" (vtkpolydatamapper-code mapper-name filter-name t))
             (if (> *draw-enhanced3d-type* 0)
               (format nil "~a.SetLookupTable(~a)~%" mapper-name lookup-table-name)
               "")
             (format nil "bounds.append(~a.GetBounds())~%" mapper-name)
             (format nil "~a~%" (vtkactor-code actor-name nil color opacity linewidth nil))
             (format nil "~a~%" (vtkextractpolydatageometry-code (get-extract-name) t t))   )))

      ((and (>= point-type 14)
            (<= point-type 17))
         (setf solidsource-name (get-solidsource-name)
               filter-name      (get-filter-name)
               mapper-name      (get-mapper-name)
               actor-name       (get-actor-name)
               color            (get-option '$color)
               opacity          (get-option '$opacity) )
         (setf output-string
           (concatenate 'string
             output-string
             (case point-type
               (14 ; sphere glyph
                 (format nil "~a=vtk.vtkSphereSource()~%~a.SetRadius(~a)~%"
                         solidsource-name
                         solidsource-name
                         (/ point-size 2.0)))
               (15 ; cube glyph
                 (format nil "~a=vtk.vtkCubeSource()~%~a.SetXLength(~a)~%~a.SetYLength(~a)~%~a.SetZLength(~a)~%"
                         solidsource-name
                         solidsource-name
                         point-size
                         solidsource-name
                         point-size
                         solidsource-name
                         point-size))
               (16 ; cylinder glyph
                 (format nil "~a=vtk.vtkCylinderSource()~%~a.SetRadius(~a)~%~a.SetHeight(~a)~%"
                         solidsource-name
                         solidsource-name
                         (/ point-size 2.0)
                         solidsource-name
                         point-size))
               (17 ; cone glyph
                 (format nil "~a=vtk.vtkConeSource()~%~a.SetRadius(~a)~%~a.SetHeight(~a)~%"
                         solidsource-name
                         solidsource-name
                         (/ point-size 2.0)
                         solidsource-name
                         point-size)))
             (vtkglyph3d-code filter-name
                              source-name 
                             (format nil "SetSourceConnection(~a.GetOutputPort())" solidsource-name))
             (format nil "~a~%" (vtkpolydatamapper-code mapper-name filter-name t))
             (if (> *draw-enhanced3d-type* 0)
               (format nil "~a.SetLookupTable(~a)~%" mapper-name lookup-table-name)
               "")
             (format nil "bounds.append(~a.GetBounds())~%" mapper-name)
             (format nil "~a~%" (vtkactor-glyph-code actor-name color opacity))
             (format nil "~a~%" (vtkextractpolydatageometry-code (get-extract-name) t t)) )))
      (t
         (merror "vtk3d: not recognized point_type")))))



;; 2D: points([[x1,y1], [x2,y2], [x3,y3],...])
;; -------------------------------------------
;; Options:
;;     point_size
;;     point_type
;;     points_joined
;;     line_width
;;     key
;;     line_type
;;     color
;;     transform
(defun vtk2d-points (arg1 &optional (arg2 nil))
  (let ((pointsjoined  (get-option '$points_joined))
        (pointtype     (get-option '$point_type))
        (pointsize     ($float  (get-option '$point_size)))
        (linetype      (get-option '$line_type))
        (color         (hex-to-numeric-list (get-option '$color)))
        (linewidth     (get-option '$line_width))
        (key           (get-option '$key))
        (arrayX-name   (get-arrayX-name))
        (arrayY-name   (get-arrayY-name))
        (table-name    (get-table-name))
        (str           (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        tmp x y ax ay n)

    (when (not (string= (string-trim " " key) ""))
       (incf *vtk-2dkey-counter*) )
    ; check type of input
    (cond 
       ((and ($listp arg1)
             (null arg2)
             (every #'$listp (rest arg1)))     ; xy format
          (setf tmp (mapcar #'rest (rest arg1)))
          (setf x (map 'list #'$float (map 'list #'first tmp))
                y (map 'list #'$float (map 'list #'second tmp))) )
       ((and ($matrixp arg1)
             (= (length (cadr arg1)) 3)
             (null arg2))                 ; two-column matrix
          (setf tmp (mapcar #'rest (rest arg1)))
          (setf x (map 'list #'$float (map 'list #'first tmp))
                y (map 'list #'$float (map 'list #'second tmp))) )
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
       (t (merror "draw (points2d): incorrect input format")))
    (setf n (length x))
    (transform-lists 2)
    (setf ax (make-array n :element-type 'flonum :initial-contents x)
          ay (make-array n :element-type 'flonum :initial-contents y))

    ; python code
    (format str "~a=vtk.vtkFloatArray()~%" arrayX-name)
    (format str "~a.SetName(\"~a\")~%" arrayX-name arrayX-name)
    (format str "~a=vtk.vtkFloatArray()~%" arrayY-name)
    (format str "~a.SetName(\"~a\")~%" arrayY-name key)
    (loop for i from 0 below n do
      (format str "~a.InsertNextValue(~a)~%" arrayX-name (aref ax i))
      (format str "~a.InsertNextValue(~a)~%" arrayY-name (aref ay i))  )
    (format str "~a=vtk.vtkTable()~%" table-name)
    (format str "~a.AddColumn(~a)~%" table-name arrayX-name)
    (format str "~a.AddColumn(~a)~%~%" table-name arrayY-name)
    (cond
      ((equal pointsjoined t)
        (format str "line = chart~a.AddPlot(~a)~%" *vtk-chart-counter* 0)
        (format str "line.SetInputData(~a,0,1)~%" table-name)
        (format str "line.SetColor(~a,~a,~a,255)~%"
          (round (* 255 (first color)))
          (round (* 255 (second color)))
          (round (* 255 (third color))) )
        (format str "line.SetWidth(~a)~%" linewidth)
        (format str "#line.SetLegendVisibility(0)~%")
        (format str "line.SetMarkerStyle(0)~%")
        (format str "line.GetPen().SetLineType(~a)~%"
          (case linetype ; translate some gnuplot codes into vtk codes
            (0 3)
            (6 4)
            (otherwise linetype) ) ))
      ((equal pointsjoined '$impulses)
        (let (tbl impx impy)
          (loop for i from 0 below n do
            (setf tbl (get-table-name)
                  impx (get-arrayX-name)
                  impy (get-arrayY-name))
            (format str "~a=vtk.vtkTable()~%" tbl)
            (format str "~a=vtk.vtkFloatArray()~%" impx)
            (format str "~a.SetNumberOfTuples(2)~%" impx)
            (format str "~a.SetName(\"~a\")~%" impx impx)
            (format str "~a=vtk.vtkFloatArray()~%" impy)
            (format str "~a.SetNumberOfTuples(2)~%" impy)
            (format str "~a.SetName(\"~a\")~%" impy impy)
            (format str "~a.InsertValue(0,~a.GetColumn(0).GetValue(~a))~%" impx table-name i)
            (format str "~a.InsertValue(1,~a.GetColumn(0).GetValue(~a))~%" impx table-name i)
            (format str "~a.InsertValue(0,~a.GetColumn(1).GetValue(~a))~%" impy table-name i)
            (format str "~a.InsertValue(1,0.0)~%" impy)
            (format str "~a.AddColumn(~a)~%" tbl impx)
            (format str "~a.AddColumn(~a)~%" tbl impy)
            (format str "line = chart~a.AddPlot(0)~%" *vtk-chart-counter*)
            (format str "line.SetInputData(~a,0,1)~%" tbl)
            (format str "line.SetColor(~a,~a,~a,255)~%"
              (round (* 255 (first color)))
              (round (* 255 (second color)))
              (round (* 255 (third color))) )
            (format str "line.SetWidth(~a)~%" linewidth)
            (format str "#line.SetLegendVisibility(0)~%~%") )) ) )
    (format str "line = chart~a.AddPlot(~a)~%" *vtk-chart-counter* 1)
    (format str "line.SetInputData(~a,0,1)~%" table-name)
    (format str "line.SetColor(~a,~a,~a,255)~%"
      (round (* 255 (first color)))
      (round (* 255 (second color)))
      (round (* 255 (third color))) )
    (format str "line.SetWidth(~a)~%" pointsize)
    (format str "#line.SetLegendVisibility(~a)~%" 
      (if (string= (string-trim " " key) "")
       0
       1 ))
    (format str "line.SetMarkerStyle(~a)~%~%"
      (case pointtype
        ((-1 0) 0)
        (1 2)
        ((2 3) 1)
        ((4 5 15) 3)
        ((6 7 14) 4)
        ((12 13) 5)
        (otherwise 3) ))
    str  ))



;; 3D: parametric(xfun,yfun,zfun,par1,parmin,parmax)
;; -------------------------------------------------
(defun vtk3d-parametric (xfun yfun zfun par1 parmin parmax)
  (let* ((nticks          (get-option '$nticks))
         (color           (get-option '$color))
         (line-type       (get-option '$line_type))
         (opacity         (get-option '$opacity))
         (linewidth       (get-option '$line_width))
         (wiredsurface    (get-option '$wired_surface))
         (source-name     (get-source-name))
         (points-name     (get-points-name))
         (cellarray-name  (get-cellarray-name))
         (floatarray-name (get-floatarray-name))
         (trans-name      (get-trans-name))
         (filter-name     (get-filter-name))
         (mapper-name     (get-mapper-name))
         (actor-name      (get-actor-name))
         ($numer          t)
         (tmin            ($float parmin))
         (tmax            ($float parmax))
         (tt              tmin)
         (eps             (/ (- tmax tmin) (- nticks 1)))
         (str             (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (count           -1)
         lookup-table-name scalars f1 f2 f3 x y z xx yy zz)
    (check-enhanced3d-model "parametric" '(0 1 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1)))
    (if (< tmax tmin)
       (merror "vtk3d (parametric): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist) ,par1)))
    (setq f2 (coerce-float-fun yfun `((mlist) ,par1)))
    (setq f3 (coerce-float-fun zfun `((mlist) ,par1)))
    (setf x (make-array nticks :element-type 'flonum)
          y (make-array nticks :element-type 'flonum)
          z (make-array nticks :element-type 'flonum))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array nticks :element-type 'flonum)))
    (dotimes (k nticks)
      (setf xx (funcall f1 tt))
      (setf yy (funcall f2 tt))
      (setf zz (funcall f3 tt))
      (case *draw-enhanced3d-type*
        ((1 99) (setf (aref scalars k) (funcall *draw-enhanced3d-fun* tt)))
        (3      (setf (aref scalars k) (funcall *draw-enhanced3d-fun* xx yy zz))))
      (transform-point 3)
      (setf (aref x (incf count)) xx)
      (setf (aref y count) yy)
      (setf (aref z count) zz)
      (setf tt (+ tt eps)) )
    (when (> *draw-enhanced3d-type* 0)
      (let ((lut (check-lookup-table)))
        (setf lookup-table-name (car lut))
        (format str "~a~%" (cadr lut))))

    ; phyton code
    (format str "~a=vtk.vtkPolyData()~%" source-name)
    (format str "~a~%" (vtkpoints-code points-name source-name x y z))
    (format str "~a~%" (vtkcellarray-code cellarray-name source-name 1 
                                          (list (loop for k from 0 below nticks collect k))))
    (format str "~a~%" (vtktransform-code trans-name))
    (if (< line-type 0) ; line type is a tube
      (format str "~a~%" (vtktransformpolydatafilter-code "auxfiltertube" source-name trans-name t))
      (format str "~a~%" (vtktransformpolydatafilter-code filter-name source-name trans-name t) ) )
    (cond
      ((< line-type 0) ; line type is a tube
         (format str "~a~%" (vtktubefilter-code filter-name "auxfiltertube" line-type))
         (format str "~a~%" (vtkpolydatamapper-code mapper-name filter-name t)))
      (t
        (format str "~a~%" (vtkpolydatamapper-code mapper-name filter-name t))))
    (format str "bounds.append(~a.GetBounds())~%" mapper-name)
    (format str "~a~%" (vtkactor-code actor-name nil color opacity linewidth wiredsurface))
    (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))
    (when (> *draw-enhanced3d-type* 0)
        (format str "~a~%" (vtkfloatarray-code floatarray-name source-name scalars nil))
        (format str "~a.SetLookupTable(~a)~%" mapper-name lookup-table-name)
        (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
              (format nil "rescalearray(~a,~a)~%" points-name floatarray-name)) )
    (when (>= line-type 0) ; when line type is not a tube, set line pattern
        (format str "~a.GetProperty().SetLineStipplePattern(~a)~%~%"
                actor-name
                (case line-type
                   (0 "0x0001")
                   (1 "0xFFFF")
                   (2 "0xFF00")
                   (6 "0xFE10"))) )
    str) )




;; 2D: parametric(xfun,yfun,par,parmin,parmax)
;; -----------------------------------
;; Options:
;;     nticks
;;     line_width
;;     line_type
;;     color
;;     key
(defun vtk2d-parametric (xfun yfun par parmin parmax)
  (let* ((nticks      (get-option '$nticks))
         (linewidth   (get-option '$line_width))
         (linetype    (get-option '$line_type))
         (color       (hex-to-numeric-list (get-option '$color)))
         (key         (get-option '$key))
         (arrayX-name (get-arrayX-name))
         (arrayY-name (get-arrayY-name))
         (table-name  (get-table-name))
         (tmin        ($float parmin))
         (tmax        ($float parmax))
         (str         (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         ($numer      t)
         (eps         (/ (- tmax tmin) (- ($float nticks) 1)))
         (tt          ($float parmin))
         (*plot-realpart* *plot-realpart*)
         result f1 f2 xx yy result-array)

    (when (< tmax tmin)
       (merror "draw2d (parametric): illegal range"))
    (when (not (string= (string-trim " " key) ""))
       (incf *vtk-2dkey-counter*) )
    (when (not (subsetp (append (rest ($listofvars xfun)) (rest ($listofvars yfun))) (list par)))
       (merror "draw2d (parametric): non defined variable"))
    (setq *plot-realpart* (get-option '$draw_realpart))
    (setq f1 (coerce-float-fun xfun `((mlist) ,par)))
    (setq f2 (coerce-float-fun yfun `((mlist) ,par)))
    (setf result
       (loop
          do (setf xx ($float (funcall f1 tt)))
             (setf yy ($float (funcall f2 tt)))
             (transform-point 2)
          collect xx
          collect yy
          when (>= tt tmax) do (loop-finish)
          do (setq tt (+ tt eps))
             (if (>= tt tmax) (setq tt tmax)) ))
    (setf result-array (make-array (length result) :initial-contents result))

    ; phyton code
    (format str "~a=vtk.vtkFloatArray()~%" arrayX-name)
    (format str "~a.SetName(\"~a\")~%" arrayX-name arrayX-name)
    (format str "~a=vtk.vtkFloatArray()~%" arrayY-name)
    (format str "~a.SetName(\"~a\")~%" arrayY-name key)
    (loop for i from 0 below (length result) by 2 do
      (format str "~a.InsertNextValue(~a)~%" arrayX-name (aref result-array i))
      (format str "~a.InsertNextValue(~a)~%" arrayY-name (aref result-array (+ i 1)))  )
    (format str "~a=vtk.vtkTable()~%" table-name)
    (format str "~a.AddColumn(~a)~%" table-name arrayX-name)
    (format str "~a.AddColumn(~a)~%" table-name arrayY-name)
    (format str "line=chart~a.AddPlot(0)~%" *vtk-chart-counter*)
    (format str "line.SetInputData(~a,0,1)~%" table-name)
    (format str "line.SetColor(~a,~a,~a,255)~%"
      (round (* 255 (first color)))
      (round (* 255 (second color)))
      (round (* 255 (third color))) )
    (format str "line.SetWidth(~a)~%" linewidth)
    (format str "#line.SetLegendVisibility(~a)~%" 
      (if (string= (string-trim " " key) "")
       0
       1 ))
    (format str "line.GetPen().SetLineType(~a)~%~%"
      (case linetype ; translate some gnuplot codes into vtk codes
        (0 3)
        (6 4)
        (otherwise linetype) ) )
    str ))



;; 2D: polar(radius,ang,minang,maxang)
;; -----------------------------------
;; Options:
;;     nticks
;;     line_width
;;     line_type
;;     color
;;     key
(defun vtk2d-polar (radius ang minang maxang)
  (vtk2d-parametric `((mtimes simp) ,radius ((%cos simp) ,ang))
                    `((mtimes simp) ,radius ((%sin simp) ,ang))
                    ang minang maxang) )



;; 3D: parametric_surface(xfun,yfun,zfun,par1,par1min,par1max,par2,par2min,par2max)
;; --------------------------------------------------------------------------------
(defun vtk3d-parametric_surface (xfun yfun zfun par1 par1min par1max par2 par2min par2max)
  (let* ((xu_grid        (get-option '$xu_grid))
         (yv_grid        (get-option '$yv_grid))
         (color          (get-option '$color))
         (opacity        (get-option '$opacity))
         (linewidth      (get-option '$line_width))
         (wiredsurface   (get-option '$wired_surface))
         (umin           ($float par1min))
         (umax           ($float par1max))
         (vmin           ($float par2min))
         (vmax           ($float par2max))
         (epsu           (/ (- umax umin) xu_grid))
         (epsv           (/ (- vmax vmin) yv_grid))
         (source-name    (get-source-name))
         (points-name    (get-points-name))
         (cellarray-name (get-cellarray-name))
         (mapper-name    (get-mapper-name))
         (actor-name     (get-actor-name))
         (trans-name     (get-trans-name))
         (filter-name    (get-filter-name))
         (str            (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (scalars2-count -1)
         (scalars-count  -1)
         (nx             (+ xu_grid 1))
         (ny             (+ yv_grid 1))
         ($numer         t)
         (count          -1)
         (scalars        nil) ; used for coloring
         (scalars2       nil) ; used for isolines
         (xx 0.0) (uu 0.0)
         (yy 0.0) (vv 0.0)
         (zz 0.0)
         floatarray-name mapper2-name filter2-name floatarray2-name lookup-table-name
         f1 f2 f3 x y z)
    (check-enhanced3d-model "parametric_surface" '(0 2 3 99))
    (check-isolines-model "parametric_surface" '(0 2 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1 par2)))
    (when (= *draw-isolines-type* 99)
       (update-isolines-expression (list '(mlist) par1 par2)))
    (when (or (< umax umin)
              (< vmax vmin))
       (merror "vtk3d (parametric_surface): illegal range"))
    (setq f1 (coerce-float-fun xfun `((mlist) ,par1 ,par2)))
    (setq f2 (coerce-float-fun yfun `((mlist) ,par1 ,par2)))
    (setq f3 (coerce-float-fun zfun `((mlist) ,par1 ,par2)))
    (setf x (make-array (* nx ny) :element-type 'flonum)
          y (make-array (* nx ny) :element-type 'flonum)
          z (make-array (* nx ny) :element-type 'flonum))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array (* nx ny) :element-type 'flonum)))
    (when (> *draw-isolines-type* 0)
      (setf scalars2 (make-array (* nx ny) :element-type 'flonum)))
    (loop for j below ny
           initially (setf vv vmin)
           do (setf uu umin)
           (loop for i below nx
                  do
                  (setf xx (funcall f1 uu vv))
                  (setf yy (funcall f2 uu vv))
                  (setf zz (funcall f3 uu vv))
                  ; geometric transformation
                  (transform-point 3)
                  (setf (aref x (incf count)) xx)
                  (setf (aref y count) yy)
                  (setf (aref z count) zz)
                  ; check texture model
                  (case *draw-enhanced3d-type*
                    ((2 99) (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* uu vv)))
                    (3      (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* xx yy zz))) )
                  ; check isolines model
                  (case *draw-isolines-type*
                    ((2 99) (setf (aref scalars2 (incf scalars2-count)) (funcall *draw-isolines-fun* uu vv)))
                    (3      (setf (aref scalars2 (incf scalars2-count)) (funcall *draw-isolines-fun* xx yy zz))) )
                  (setq uu (+ uu epsu)))
           (setq vv (+ vv epsv)))
    (when (> *draw-enhanced3d-type* 0)
      (let ((lut (check-lookup-table)))
        (setf lookup-table-name (car lut))
        (format str "~a~%" (cadr lut))))

    ; python code
    (format str "~a=vtk.vtkPolyData()~%" source-name)
    (format str "~a~%" (vtkpoints-code points-name source-name x y z))
    (format str "~a~%" (vtkcellarray-code cellarray-name source-name 2 (build-surface-grid nx ny)))
    (format str "~a~%" (vtktransform-code trans-name))
    (format str "~a~%" (vtktransformpolydatafilter-code filter-name source-name trans-name t))
    (format str "~a~%" (vtkpolydatamapper-code mapper-name filter-name nil))

    (format str "bounds.append(~a.GetBounds())~%" points-name)
    (format str "~a~%" (vtkactor-code actor-name nil color opacity linewidth wiredsurface))
    (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil t))

    (when (> *draw-enhanced3d-type* 0)
      (format str "~a.SetLookupTable(~a)~%" mapper-name lookup-table-name)
      (format str "~a.SetScalarModeToUsePointFieldData()~%" mapper-name)
      (format str "~a.ScalarVisibilityOn()~%~%" mapper-name)
      (setf floatarray-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray-name source-name scalars))
      ; remove next string if we want isolines and solid color when enhanced3d is not active
      (format str "~a.SelectColorArray(\"name~a\")~%" mapper-name floatarray-name)
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray-name)) )

    (when (> *draw-isolines-type* 0)
      (setf floatarray2-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray2-name source-name scalars2))
      (setf filter2-name (get-filter-name))
      (format str "~a~%" (vtkContourFilter-code filter2-name filter-name))
      (setf mapper2-name (get-mapper-name))
      (format str "~a" (vtkpolydatamapper-isoline-code mapper2-name source-name floatarray2-name))
      (format str "~a~%" (vtkactor-isoline-code (get-actor-name) linewidth))
      (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray2-name)) )

    str ))



;; spherical(radius,az,minazi,maxazi,zen,minzen,maxzen)
;; ----------------------------------------------------
(defun vtk3d-spherical (radius azi minazi maxazi zen minzen maxzen)
  (vtk3d-parametric_surface
    `((mtimes simp) ,radius ((%sin simp) ,zen) ((%cos simp) ,azi))
    `((mtimes simp) ,radius ((%sin simp) ,zen) ((%sin simp) ,azi))
    `((mtimes simp) ,radius ((%cos simp) ,zen))
    azi minazi maxazi
    zen minzen maxzen))



;; cylindrical(r,z,minz,maxz,azi,minazi,maxazi)
;; --------------------------------------------
(defun vtk3d-cylindrical (r z minz maxz azi minazi maxazi)
  (vtk3d-parametric_surface
    `((mtimes simp) ,r ((%cos simp) ,azi))
    `((mtimes simp) ,r ((%sin simp) ,azi))
    z 
    z minz maxz
    azi minazi maxazi))



;; 3D: explicit(fcn,par1,minval1,maxval1,par2,minval2,maxval2)
;; -----------------------------------------------------------
(defun vtk3d-explicit (fcn par1 minval1 maxval1 par2 minval2 maxval2)
  (let* ((xu_grid        (get-option '$xu_grid))
         (yv_grid        (get-option '$yv_grid))
         (color          (get-option '$color))
         (opacity        (get-option '$opacity))
         (linewidth      (get-option '$line_width))
         (wiredsurface   (get-option '$wired_surface))
         (fminval1       ($float minval1))
         (fminval2       ($float minval2))
         (fmaxval1       ($float maxval1))
         (fmaxval2       ($float maxval2))
         (epsx           (/ (- fmaxval1 fminval1) xu_grid))
         (epsy           (/ (- fmaxval2 fminval2) yv_grid))
         (source-name    (get-source-name))
         (points-name    (get-points-name))
         (cellarray-name (get-cellarray-name))
         (mapper-name    (get-mapper-name))
         (actor-name     (get-actor-name))
         (trans-name     (get-trans-name))
         (filter-name    (get-filter-name))
         (str            (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         ($numer         t)
         (count          -1)
         (scalars        nil) ; used for coloring
         (scalars2       nil) ; used for isolines
         (scalars-count  -1)
         (scalars2-count -1)
         (nx             (+ xu_grid 1))
         (ny             (+ yv_grid 1))
         (xx 0.0) (uu 0.0)
         (yy 0.0) (vv 0.0)
         (zz 0.0)
         floatarray-name floatarray2-name mapper2-name filter2-name lookup-table-name
         x y z)
    (check-enhanced3d-model "explicit" '(0 2 3 99))
    (check-isolines-model "explicit" '(0 2 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1 par2)))
    (when (= *draw-isolines-type* 99)
       (update-isolines-expression (list '(mlist) par1 par2)))
    (setq fcn (coerce-float-fun fcn `((mlist) ,par1 ,par2)))
    (setf x (make-array (* nx ny) :element-type 'flonum)
          y (make-array (* nx ny) :element-type 'flonum)
          z (make-array (* nx ny) :element-type 'flonum))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array (* nx ny) :element-type 'flonum)))
    (when (> *draw-isolines-type* 0)
      (setf scalars2 (make-array (* nx ny) :element-type 'flonum)))
    (loop for j below ny
           initially (setf vv fminval2)
           do (setf uu fminval1)
           (loop for i below nx
                  do
                  (setf xx uu
                        yy vv)
                  (setf zz (funcall fcn xx yy))
                  ; geometric transformation
                  (transform-point 3)
                  (setf (aref x (incf count)) xx)
                  (setf (aref y count) yy)
                  (setf (aref z count) zz)
                  ; check texture model
                  (case *draw-enhanced3d-type*
                    ((2 99) (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* xx yy)))
                    (3      (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* xx yy zz))) )
                  ; check isolines model
                  (case *draw-isolines-type*
                    ((2 99) (setf (aref scalars2 (incf scalars2-count)) (funcall *draw-isolines-fun* xx yy)))
                    (3      (setf (aref scalars2 (incf scalars2-count)) (funcall *draw-isolines-fun* xx yy zz))) )
                  (setq uu (+ uu epsx)))
           (setq vv (+ vv epsy)))
    (when (> *draw-enhanced3d-type* 0)
      (let ((lut (check-lookup-table)))
        (setf lookup-table-name (car lut))
        (format str "~a~%" (cadr lut))))

    ; python code
    (format str "~a=vtk.vtkPolyData()~%" source-name)
    (format str "~a~%" (vtkpoints-code points-name source-name x y z))
    (format str "~a~%" (vtkcellarray-code cellarray-name source-name 2 (build-surface-grid nx ny)))
    (format str "~a~%" (vtktransform-code trans-name))
    (format str "~a~%" (vtktransformpolydatafilter-code filter-name source-name trans-name t))
    (format str "~a~%" (vtkpolydatamapper-code mapper-name filter-name nil))

    (format str "bounds.append(~a.GetBounds())~%" points-name)
    (format str "~a~%" (vtkactor-code actor-name nil color opacity linewidth wiredsurface))
    (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil t))

    (when (> *draw-enhanced3d-type* 0)
      (format str "~a.SetLookupTable(~a)~%" mapper-name lookup-table-name)
      (format str "~a.SetScalarModeToUsePointFieldData()~%" mapper-name)
      (format str "~a.ScalarVisibilityOn()~%~%" mapper-name)
      (setf floatarray-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray-name source-name scalars))
      ; remove next string if we want isolines and solid color when enhanced3d is not active
      (format str "~a.SelectColorArray(\"name~a\")~%" mapper-name floatarray-name)
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray-name)) )

    (when (> *draw-isolines-type* 0)
      (setf floatarray2-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray2-name source-name scalars2))
      (setf filter2-name (get-filter-name))
      (format str "~a~%" (vtkContourFilter-code filter2-name filter-name))
      (setf mapper2-name (get-mapper-name))
      (format str "~a" (vtkpolydatamapper-isoline-code mapper2-name source-name floatarray2-name))
      (format str "~a~%" (vtkactor-isoline-code (get-actor-name) linewidth))
      (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray2-name))  )

    str ))



;; 2D: explicit(fcn,var,minval,maxval)
;; -----------------------------------
;; Options:
;;     nticks
;;     adapt_depth
;;     line_width
;;     line_type
;;     color
;;     key
(defun vtk2d-explicit (fcn var minval maxval)
  (let* ((nticks      (get-option '$nticks))
         (adaptdepth  (get-option '$adapt_depth))
         (linewidth   (get-option '$line_width))
         (linetype    (get-option '$line_type))
         (color       (hex-to-numeric-list (get-option '$color)))
         (fillcolor   (hex-to-numeric-list (get-option '$fill_color)))
         (key         (get-option '$key))
         (xmin        ($float minval))
         (xmax        ($float maxval))
         (x-step      (/ (- xmax xmin) ($float nticks) 2))
         (arrayX-name (get-arrayX-name))
         (arrayY-name (get-arrayY-name))
         (table-name  (get-table-name))
         (str         (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (*plot-realpart* *plot-realpart*)
         ($numer t)
         x-samples y-samples result result-array )
    (when (< xmax xmin)
       (merror "draw2d (explicit): illegal range"))
    (when (not (string= (string-trim " " key) ""))
       (incf *vtk-2dkey-counter*) )
    (when (get-option '$logx)
      (setf xmin (log xmin))
      (setf xmax (log xmax))
      (setf x-step (/ (- xmax xmin) ($float nticks) 2)))
    (setq *plot-realpart* (get-option '$draw_realpart))
    (setq fcn (coerce-float-fun fcn `((mlist) ,var)))
    (flet ((fun (x)
               (let ((y (if (get-option '$logx)
                            (funcall fcn (exp x))
                            (funcall fcn x))))
                 (if (and (get-option '$logy)
                          (numberp y))
                     (if (> y 0)
                       (log y)
                       (merror "draw2d (explicit): logarithm of negative number"))
                     y))))
      (dotimes (k (1+ (* 2 nticks)))
        (let ((x (+ xmin (* k x-step))))
          (push x x-samples)
          (push (fun x) y-samples)))
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
                                           adaptdepth 1e-5)))
          (when (notevery #'(lambda (x) (or (numberp x) (eq x t) )) sublst)
            (let ((items sublst) (item 'nil))
	      ;; Search for the item in sublist that is the undefined variable
	      (while items
		(when (not (or (numberp (car items)) (eq (car items) t) ))
		    (setq item (car items)) )
		(setq items (cdr items)) )
	      (merror "draw2d (explicit): non defined variable in term ~M" item) ) )
          (when (not (null result))
            (setf sublst (cddr sublst)))
          (do ((lst sublst (cddr lst)))
              ((null lst) 'done)
            (setf result (append result
                                 (list
                                   (if (and (get-option '$logx)
                                            (numberp (first lst)))
                                     (exp (first lst))
                                     (first lst))
                                   (if (and (get-option '$logy)
                                            (numberp (second lst)))
                                     (exp (second lst))
                                     (second lst)))))))))
    (cond
      ((> *draw-transform-dimensions* 0)
         ; With geometric transformation.
         ; When option filled_func in not nil,
         ; geometric transformation is ignored
         (setf result-array (make-array (length result)))
         (setf xmin most-positive-double-float
               xmax most-negative-double-float)
         (let (xold yold x y (count -1))
           (do ((lis result (cddr lis)))
               ((null lis))
             (setf xold (first lis)
                   yold (second lis))
             (setf x (funcall *draw-transform-f1* xold yold)
                   y (funcall *draw-transform-f2* xold yold))
             (setf (aref result-array (incf count)) x)
             (setf (aref result-array (incf count)) y)  )  ) )
      (t
         ; No geometric transformation invoked.
         (setf result-array (make-array (length result)
                                        :initial-contents result))))
    ; python code
    (format str "~a=vtk.vtkFloatArray()~%" arrayX-name)
    (format str "~a.SetName(\"~a\")~%" arrayX-name arrayX-name)
    (format str "~a=vtk.vtkFloatArray()~%" arrayY-name)
    (format str "~a.SetName(\"~a\")~%" arrayY-name key)
    (loop for i from 0 below (length result) by 2 do
      (when (not (equal (aref result-array (+ i 1)) t))
        ; in case of division by zero, do not insert next value.
        ; A vertical line will be plotted, which should be fixed.
        (format str "~a.InsertNextValue(~a)~%" arrayX-name (aref result-array i))
        (format str "~a.InsertNextValue(~a)~%" arrayY-name (aref result-array (+ i 1))) ) )
    (format str "~a=vtk.vtkTable()~%" table-name)
    (format str "~a.AddColumn(~a)~%" table-name arrayX-name)
    (format str "~a.AddColumn(~a)~%" table-name arrayY-name)
    (format str "line=chart~a.AddPlot(~a)~%"
      *vtk-chart-counter*
      (if (get-option '$filled_func) 3 0))
    (format str "line.SetInputData(~a,0,1)~%" table-name)
    (let (col)
      (if (get-option '$filled_func)
        (setf col fillcolor)
        (setf col color))
      (format str "line.SetColor(~a,~a,~a,255)~%"
        (round (* 255 (first  col)))
        (round (* 255 (second col)))
        (round (* 255 (third  col))) ))
    (format str "line.SetWidth(~a)~%" linewidth)
; not present in 6.2
;    (format str "line.SetLegendVisibility(~a)~%" 
;      (if (string= (string-trim " " key) "")
;       0
;       1 ))
    (format str "line.GetPen().SetLineType(~a)~%~%"
      (case linetype ; translate some gnuplot codes into vtk codes
        (0 3)
        (6 4)
        (otherwise linetype) ) )
    str ))



;; 3d: elevation_grid(mat x0 y0 width height)
;; ------------------------------------------
(defun vtk3d-elevation_grid (mat x0 y0 width height)
  (let* ((fx0            ($float x0))
         (fy0            ($float y0))
         (fwidth         ($float width))
         (fheight        ($float height))
         (color          (get-option '$color))
         (opacity        (get-option '$opacity))
         (linewidth      (get-option '$line_width))
         (wiredsurface   (get-option '$wired_surface))
         (source-name    (get-source-name))
         (points-name    (get-points-name))
         (cellarray-name (get-cellarray-name))
         (mapper-name    (get-mapper-name))
         (actor-name     (get-actor-name))
         (trans-name     (get-trans-name))
         (filter-name    (get-filter-name))
         (str            (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (xi             0.0)
         (yi             (+ fy0 fheight))
         (xx             0.0)
         (yy             0.0)
         (zz             0.0)
         (count          -1)
         (scalars        nil) ; used for coloring
         (scalars2       nil) ; used for isolines
         (scalars-count  -1)
         (scalars2-count -1)
         floatarray-name floatarray2-name mapper2-name filter2-name lookup-table-name
         ny nx dx dy x y z)
    (check-enhanced3d-model "elevation_grid" '(0 2 3))
    (check-isolines-model   "elevation_grid" '(0 2 3))
    (when (null ($matrixp mat))
      (merror "draw3d (elevation_grid): Argument not recognized"))
    (setf nx (length (cdadr mat))
          ny (length (cdr mat)))
    (setf dx (/ fwidth (1- nx))
          dy (/ fheight (1- ny)))
    (setf x (make-array (* nx ny) :element-type 'flonum)
          y (make-array (* nx ny) :element-type 'flonum)
          z (make-array (* nx ny) :element-type 'flonum))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array (* nx ny) :element-type 'flonum)))
    (when (> *draw-isolines-type* 0)
      (setf scalars2 (make-array (* nx ny) :element-type 'flonum)))
    (loop for row on (cdr mat) by #'cdr do
       (setf xi fx0)
       (loop for col on (cdar row) by #'cdr do
          (setf xx xi
                yy yi)
          (setf zz ($float (car col)))
          ; geometric transformation
          (transform-point 3)
          (setf (aref x (incf count)) xx)
          (setf (aref y count) yy)
          (setf (aref z count) zz)
          ; check texture model
          (case *draw-enhanced3d-type*
            (2 (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* xx yy)))
            (3 (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* xx yy zz))))
          ; check isolines model
          (case *draw-isolines-type*
            (2 (setf (aref scalars2 (incf scalars2-count)) (funcall *draw-isolines-fun* xx yy)))
            (3 (setf (aref scalars2 (incf scalars2-count)) (funcall *draw-isolines-fun* xx yy zz))) )
          (setf xi (+ xi dx)))
       (setf yi (- yi dy)))
    (when (> *draw-enhanced3d-type* 0)
      (let ((lut (check-lookup-table)))
        (setf lookup-table-name (car lut))
        (format str "~a~%" (cadr lut))))

    ; python code
    (format str "~a=vtk.vtkPolyData()~%" source-name)
    (format str "~a~%" (vtkpoints-code points-name source-name x y z))
    (format str "~a~%" (vtkcellarray-code cellarray-name source-name 2 (build-surface-grid nx ny)))
    (format str "~a~%" (vtktransform-code trans-name))
    (format str "~a~%" (vtktransformpolydatafilter-code filter-name source-name trans-name t))
    (format str "~a~%" (vtkpolydatamapper-code mapper-name filter-name nil))

    (format str "bounds.append(~a.GetBounds())~%" points-name)
    (format str "~a~%" (vtkactor-code actor-name nil color opacity linewidth wiredsurface))
    (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil t))

    (when (> *draw-enhanced3d-type* 0)
      (format str "~a.SetLookupTable(~a)~%" mapper-name lookup-table-name)
      (format str "~a.SetScalarModeToUsePointFieldData()~%" mapper-name)
      (format str "~a.ScalarVisibilityOn()~%~%" mapper-name)
      (setf floatarray-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray-name source-name scalars))
      ; remove next string if we want isolines and solid color when enhanced3d is not active
      (format str "~a.SelectColorArray(\"name~a\")~%" mapper-name floatarray-name)
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray-name)) )

    (when (> *draw-isolines-type* 0)
      (setf floatarray2-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray2-name source-name scalars2))
      (setf filter2-name (get-filter-name))
      (format str "~a~%" (vtkContourFilter-code filter2-name filter-name))
      (setf mapper2-name (get-mapper-name))
      (format str "~a" (vtkpolydatamapper-isoline-code mapper2-name source-name floatarray2-name))
      (format str "~a~%" (vtkactor-isoline-code (get-actor-name) linewidth))
      (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray2-name)) )

    str ))



;; 3d: implicit(expr,x,xmin,xmax,y,ymin,ymax,z,zmin,zmax)
;; ------------------------------------------------------
(defun build-surface-triangular-grid (ntri)
  (let ((poly nil)
        (cont -1)
        (p (/ ntri 3)))
    (dotimes (tri p)
      (setf poly (cons (list (incf cont) (incf cont) (incf cont)) poly)))
    (reverse poly)))

(defun vtk3d-implicit (expr par1 xmin xmax par2 ymin ymax par3 zmin zmax)
  (let ((fxmin          ($float xmin))
        (fxmax          ($float xmax))
        (fymin          ($float ymin))
        (fymax          ($float ymax))
        (fzmin          ($float zmin))
        (fzmax          ($float zmax))
        (color          (get-option '$color))
        (opacity        (get-option '$opacity))
        (linewidth      (get-option '$line_width))
        (wiredsurface   (get-option '$wired_surface))
        (transform      (get-option '$transform))
        (source-name    (get-source-name))
        (points-name    (get-points-name))
        (cellarray-name (get-cellarray-name))
        (mapper-name    (get-mapper-name))
        (actor-name     (get-actor-name))
        (trans-name     (get-trans-name))
        (filter-name    (get-filter-name))
        (str            (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (scalars        nil) ; used for coloring
        (scalars2       nil) ; used for isolines
        floatarray-name floatarray2-name mapper2-name filter2-name lookup-table-name
        vertices numvert xx yy zz x y z)
    (check-enhanced3d-model "implicit" '(0 3 99))
    (check-isolines-model   "implicit" '(0 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par1 par2 par3)))
    (when (= *draw-isolines-type* 99)
       (update-isolines-expression (list '(mlist) par1 par2 par3)))
    (setf vertices (find-triangles expr par1 fxmin fxmax par2 fymin fymax par3 fzmin fzmax))
    (when (null vertices)
      (merror "draw3d (implicit): no surface within these ranges"))
    (setf numvert (length vertices))
    (setf x (make-array numvert :element-type 'flonum :initial-contents (map 'list #'first vertices))
          y (make-array numvert :element-type 'flonum :initial-contents (map 'list #'second vertices))
          z (make-array numvert :element-type 'flonum :initial-contents (map 'list #'third vertices)))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array numvert :element-type 'flonum)))
    (when (> *draw-isolines-type* 0)
      (setf scalars2 (make-array numvert :element-type 'flonum)))
    (do ((nf 0 (1+ nf)))
        ((= nf numvert) 'done)
      (setf xx (aref x nf)
            yy (aref y nf)
            zz (aref z nf))
      ; geometric transformation
      (when (not (eq transform '$none))
        (transform-point 3)
        (setf (aref x nf) xx)
        (setf (aref y nf) yy)
        (setf (aref z nf) zz))
      ; check texture model
      (when (> *draw-enhanced3d-type* 0)
        (setf (aref scalars nf) (funcall *draw-enhanced3d-fun* xx yy zz)))
      ; check isolines model
      (when (> *draw-isolines-type* 0)
        (setf (aref scalars2 nf) (funcall *draw-isolines-fun* xx yy zz))) )
    (let ((lut (check-lookup-table)))
      (setf lookup-table-name (car lut))
      (format str "~a~%" (cadr lut)))

    ; python code
    (format str "~a=vtk.vtkPolyData()~%" source-name)
    (format str "~a~%" (vtkpoints-code points-name source-name x y z))
    (format str "~a~%" (vtkcellarray-code cellarray-name source-name 2 (build-surface-triangular-grid numvert)))
    (format str "~a~%" (vtktransform-code trans-name))
    (format str "~a~%" (vtktransformpolydatafilter-code filter-name source-name trans-name t))
    (format str "~a~%" (vtkpolydatamapper-code mapper-name filter-name nil))

    (format str "bounds.append(~a.GetBounds())~%" points-name)
    (format str "~a~%" (vtkactor-code actor-name nil color opacity linewidth wiredsurface))
    (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil t))

    (when (> *draw-enhanced3d-type* 0)
      (format str "~a.SetLookupTable(~a)~%" mapper-name lookup-table-name)
      (format str "~a.SetScalarModeToUsePointFieldData()~%" mapper-name)
      (format str "~a.ScalarVisibilityOn()~%~%" mapper-name)
      (setf floatarray-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray-name source-name scalars))
      ; remove next string if we want isolines and solid color when enhanced3d is not active
      (format str "~a.SelectColorArray(\"name~a\")~%" mapper-name floatarray-name)
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray-name)) )

    (when (> *draw-isolines-type* 0)
      (setf floatarray2-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray2-name source-name scalars2))
      (setf filter2-name (get-filter-name))
      (format str "~a~%" (vtkContourFilter-code filter2-name filter-name))
      (setf mapper2-name (get-mapper-name))
      (format str "~a" (vtkpolydatamapper-isoline-code mapper2-name source-name floatarray2-name))
      (format str "~a~%" (vtkactor-isoline-code (get-actor-name) linewidth))
      (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray2-name)) )

    str ))



;; 3d: label([string1,x1,y1,z1],[string2,x2,y2,z2],...)
;; ----------------------------------------------------
(defun vtk3d-label (&rest lab)
  (let ((font-size (get-option '$font_size))
        (colist    (hex-to-numeric-list (get-option '$color)))
        (out "")
        label-name
        polydatamapper-name
        labelactor-name
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
               (setf labelactor-name     (get-labelactor-name)
                     label-name          (get-label-name)
                     polydatamapper-name (get-polydatamapper-name))
               (setf out
                     (concatenate 'string
                       out
                       (format nil "~a=vtk.vtkVectorText()~%" label-name)
                       (format nil "~a.SetText(~a)~%" label-name text)
                       (format nil "~a=vtk.vtkPolyDataMapper()~%" polydatamapper-name)
                       (format nil "~a.SetInputConnection(~a.GetOutputPort())~%"
                               polydatamapper-name
                               label-name)
                       (format nil "~a.ScalarVisibilityOff()~%" polydatamapper-name)
                       (format nil "~a=vtk.vtkFollower()~%" labelactor-name)
                       (format nil "~a.SetMapper(~a)~%" labelactor-name polydatamapper-name)
                       (format nil "~a.SetScale(~a,~a,~a)~%" labelactor-name font-size font-size font-size)
                       (format nil "~a.AddPosition(~a,~a,~a)~%" labelactor-name fx fy fz)
                       (format nil "~a.GetProperty().SetColor(~a,~a,~a)~%~%"
                               labelactor-name
                               (first colist)
                               (second colist)
                               (third colist)))))
             out))))



;; 3d: tube(xfun,yfun,zfun,rad,par,parmin,parmax)
;; ----------------------------------------------
(defmacro vtk-check-tube-extreme (ex cx cy cz)
    `(when (equal (nth ,ex (get-option '$capping)) t)
       (let ((cxx ,cx)
             (cyy ,cy)
             (czz ,cz))
         (when (> *draw-transform-dimensions* 0)
           (setf cxx (funcall *draw-transform-f1* ,cx ,cy ,cz)
                 cyy (funcall *draw-transform-f2* ,cx ,cy ,cz)
                 czz (funcall *draw-transform-f3* ,cx ,cy ,cz)))

        ; check texture model
        (when (> *draw-enhanced3d-type* 0)
          (case *draw-enhanced3d-type*
            ((1 99) (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* tt)))
            (3      (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* cxx cyy czz)))) )
        (dotimes (k vgrid)
          (setf (aref x (incf count)) cxx)
          (setf (aref y count) cyy)
          (setf (aref z count) czz) ))))

(defun vtk3d-tube (xfun yfun zfun rad par parmin parmax)
  (let* ((ugrid          (get-option '$xu_grid))
         (vgrid          (get-option '$yv_grid))
         (color          (get-option '$color))
         (opacity        (get-option '$opacity))
         (linewidth      (get-option '$line_width))
         (wiredsurface   (get-option '$wired_surface))
         (capping        (rest (get-option '$capping)))
         (tmin           ($float parmin))
         (tmax           ($float parmax))
         (vmax           6.283185307179586) ; = float(2*%pi)
         (teps           (/ (- tmax tmin) (- ugrid 1)))
         (veps           (/ vmax (- vgrid 1)))
         (nu             (+ ugrid (count t capping)))
         (nv             vgrid)
         (source-name    (get-source-name))
         (points-name    (get-points-name))
         (cellarray-name (get-cellarray-name))
         (trans-name     (get-trans-name))
         (filter-name    (get-filter-name))
         (mapper-name    (get-mapper-name))
         (actor-name     (get-actor-name))
         (str            (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (count          -1)
         (scalars        nil) ; used for coloring
         (scalars2       nil) ; used for isolines
         (scalars-count  -1)
         (scalars2-count -1)
         f1 f2 f3 radius
         cx cy cz nx ny nz
         ux uy uz vx vy vz
         xx yy zz module r vv rcos rsin
         cxold cyold czold
         uxold uyold uzold ttnext
         floatarray-name floatarray2-name mapper2-name filter2-name lookup-table-name
         x y z tt)
    (when (< tmax tmin)
       (merror "draw3d (tube): illegal parametric range"))
    (when (not (subsetp (rest ($append ($listofvars xfun) ($listofvars yfun)
                                       ($listofvars zfun) ($listofvars rad)))
                        (list par)))
       (merror "draw3d (tube): non defined variable"))
    (check-enhanced3d-model "tube" '(0 1 3 99))
    (check-isolines-model "tube" '(0 1 3 99))
    (when (= *draw-enhanced3d-type* 99)
       (update-enhanced3d-expression (list '(mlist) par)))
    (when (= *draw-isolines-type* 99)
       (update-isolines-expression (list '(mlist) par)))
    (setq f1 (coerce-float-fun xfun `((mlist) ,par)))
    (setq f2 (coerce-float-fun yfun `((mlist) ,par)))
    (setq f3 (coerce-float-fun zfun `((mlist) ,par)))
    (setf radius (coerce-float-fun rad `((mlist) ,par)))
    (setf x (make-array (* nu nv) :element-type 'flonum)
          y (make-array (* nu nv) :element-type 'flonum)
          z (make-array (* nu nv) :element-type 'flonum))
    (when (> *draw-enhanced3d-type* 0)
      (setf scalars (make-array (* nu nv) :element-type 'flonum)))
    (when (> *draw-isolines-type* 0)
      (setf scalars2 (make-array (* nu nv) :element-type 'flonum)))
    (loop for j from 0 below ugrid do
      (setf tt (+ tmin (* j teps)))
      ; calculate center and radius of circle
      (cond
        ((= j 0)  ; 1st circle
           (setf cx (funcall f1 tt)
                 cy (funcall f2 tt)
                 cz (funcall f3 tt)
                 ttnext (+ tt teps))
           (vtk-check-tube-extreme 1 cx cy cz)
           (setf nx (- (funcall f1 ttnext) cx)
                 ny (- (funcall f2 ttnext) cy)
                 nz (- (funcall f3 ttnext) cz)))
        (t  ; all next circles
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
                 (< (+ (* uxold ux) (* uyold uy) (* uzold uz)) 0))
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
      ; calculate points of one circumference
      (loop for i below vgrid do
        (setf rcos (* r (cos vv))
              rsin (* r (sin vv)))
        (setf xx (+ cx (* rcos ux) (* rsin vx))
              yy (+ cy (* rcos uy) (* rsin vy))
              zz (+ cz (* rcos uz) (* rsin vz)))
        ; geometric translation
        (transform-point 3)
        (setf (aref x (incf count)) xx)
        (setf (aref y count) yy)
        (setf (aref z count) zz)
        ; check texture model
        (when (> *draw-enhanced3d-type* 0)
          (case *draw-enhanced3d-type*
            ((1 99) (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* tt)))
            (3      (setf (aref scalars (incf scalars-count)) (funcall *draw-enhanced3d-fun* xx yy zz)))) )
        ; check isolines model
        (when (> *draw-isolines-type* 0)
          (case *draw-isolines-type*
            ((1 99) (setf (aref scalars2 (incf scalars2-count)) (funcall *draw-isolines-fun* tt)) )
            (3      (setf (aref scalars2 (incf scalars2-count)) (funcall *draw-isolines-fun* xx yy zz))) ))
        (setf vv (+ vv veps))
        (when (> vv vmax) (setf vv vmax))  ) ) ; end both loops
    (vtk-check-tube-extreme 2 cx cy cz)
    (let ((lut (check-lookup-table)))
      (setf lookup-table-name (car lut))
      (format str "~a~%" (cadr lut)))

    ; python code
    (format str "~a=vtk.vtkPolyData()~%" source-name)
    (format str "~a~%" (vtkpoints-code points-name source-name x y z))
    (format str "~a~%" (vtkcellarray-code cellarray-name source-name 2 (build-surface-grid nv nu)))
    (format str "~a~%" (vtktransform-code trans-name))
    (format str "~a~%" (vtktransformpolydatafilter-code filter-name source-name trans-name t))
    (format str "~a~%" (vtkpolydatamapper-code mapper-name filter-name nil))

    (format str "bounds.append(~a.GetBounds())~%" points-name)
    (format str "~a~%" (vtkactor-code actor-name nil color opacity linewidth wiredsurface))
    (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil t))

    (when (> *draw-enhanced3d-type* 0)
      (format str "~a.SetLookupTable(~a)~%" mapper-name lookup-table-name)
      (format str "~a.SetScalarModeToUsePointFieldData()~%" mapper-name)
      (format str "~a.ScalarVisibilityOn()~%~%" mapper-name)
      (setf floatarray-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray-name source-name scalars))
      ; remove next string if we want isolines and solid color when enhanced3d is not active
      (format str "~a.SelectColorArray(\"name~a\")~%" mapper-name floatarray-name)
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray-name)) )

    (when (> *draw-isolines-type* 0)
      (setf floatarray2-name (get-floatarray-name))
      (format str "~a~%" (vtkfloatarray-code floatarray2-name source-name scalars2))
      (setf filter2-name (get-filter-name))
      (format str "~a~%" (vtkContourFilter-code filter2-name filter-name))
      (setf mapper2-name (get-mapper-name))
      (format str "~a" (vtkpolydatamapper-isoline-code mapper2-name source-name floatarray2-name))
      (format str "~a~%" (vtkactor-isoline-code (get-actor-name) linewidth))
      (format str "~a~%" (vtkextractpolydatageometry-code (get-extract-name) nil nil))
      (setf (gethash *vtk-extract-counter* *enhanced3d-or-isolines-code*)
            (format nil "rescalearray(~a,~a)~%" points-name floatarray2-name)) )

    str ))





;; 2D SCENE BUILDER

(defvar *vtk2d-graphic-objects* (make-hash-table))

; table of 2d graphic objects
(setf (gethash '$explicit           *vtk2d-graphic-objects*) 'vtk2d-explicit
      (gethash '$parametric         *vtk2d-graphic-objects*) 'vtk2d-parametric
      (gethash '$points             *vtk2d-graphic-objects*) 'vtk2d-points
      (gethash '$polar              *vtk2d-graphic-objects*) 'vtk2d-polar        )

(defun make-vtk-scene-2d (args)
  (let ((objects "")
        (chart-name     (get-chart-name))
        (renderer-name  (get-renderer-name))
        largs obj)
    (ini-gr-options)
    (ini-local-option-variables)
    (user-defaults)
    (setf largs (listify-arguments args))
    (dolist (x largs)
      (cond ((equal ($op x) "=")
              (case ($lhs x)
                ($allocation       (update-allocation                           ($rhs x)))
                ($color            (update-color             '$color            ($rhs x)))
                ($fill_color       (update-color             '$fill_color       ($rhs x)))
                ($file_name        (update-string            '$file_name        ($rhs x)))
                ($font_size        (update-positive-float    '$font_size        ($rhs x)))
                ($head_angle       (update-positive-float    '$head_angle       ($rhs x)))
                ($head_length      (update-positive-float    '$head_length      ($rhs x)))
                ($background_color (update-color             '$background_color ($rhs x)))
                ($dimensions       (update-dimensions                           ($rhs x)))
                ($nticks           (update-positive-integer  '$nticks           ($rhs x)))
                ($line_width       (update-positive-float    '$line_width       ($rhs x)))
                ($line_type        (update-linestyle         '$line_type        ($rhs x)))
                ($key              (update-string            '$key              ($rhs x)))
                ($key_pos          (update-key_pos                              ($rhs x)))
                ($logx             (update-boolean-option    '$logx             ($rhs x)))
                ($logy             (update-boolean-option    '$logy             ($rhs x)))
                ($filled_func      (update-gr-option         '$filled_func      ($rhs x)))
                ($grid             (update-gr-option         '$grid             ($rhs x)))
                ($transform        (update-transform                            ($rhs x)))
                ($points_joined    (update-pointsjoined                         ($rhs x)))
                ($point_type       (update-pointtype                            ($rhs x)))
                ($point_size       (update-nonnegative-float '$point_size       ($rhs x)))
                ($terminal         (update-terminal                             ($rhs x)))
                ($unit_vectors     (update-boolean-option    '$unit_vectors     ($rhs x)))
                ($xlabel           (update-string            '$xlabel           ($rhs x)))
                ($ylabel           (update-string            '$ylabel           ($rhs x)))

                ; options not yet implemented for 2D-vtk
                ; they are included here to avoid error messages
                ($xrange           (update-gr-option         '$xrange           ($rhs x)))
                ($yrange           (update-gr-option         '$yrange           ($rhs x)))

                (otherwise (merror "vtk2d: unknown option ~M " ($lhs x)))))

            ((setf obj (gethash (caar x) *vtk2d-graphic-objects*))
               (setf objects
                     (concatenate 'string
                       objects
                       (apply obj (rest x)))))

            (t
              (merror "vtk2d: item ~M is not recognized" x))))
    ; scene allocation
    (setf *allocations* (cons (get-option '$allocation) *allocations*))
    (concatenate 'string
      (vtkchartxy-code chart-name)
      objects
      (vtkrenderer2d-code
         chart-name
         renderer-name)  )))



;; 3D SCENE BUILDER

(defvar *vtk3d-graphic-objects* (make-hash-table))

; table of 3d graphic objects
(setf (gethash '$cone               *vtk3d-graphic-objects*) 'vtk3d-cone
      (gethash '$cylinder           *vtk3d-graphic-objects*) 'vtk3d-cylinder
      (gethash '$cube               *vtk3d-graphic-objects*) 'vtk3d-cube
      (gethash '$prism              *vtk3d-graphic-objects*) 'vtk3d-prism
      (gethash '$elevation_grid     *vtk3d-graphic-objects*) 'vtk3d-elevation_grid
      (gethash '$implicit           *vtk3d-graphic-objects*) 'vtk3d-implicit
      (gethash '$sphere             *vtk3d-graphic-objects*) 'vtk3d-sphere
      (gethash '$parallelogram      *vtk3d-graphic-objects*) 'vtk3d-parallelogram
      (gethash '$triangle           *vtk3d-graphic-objects*) 'vtk3d-triangle
      (gethash '$vector             *vtk3d-graphic-objects*) 'vtk3d-vector
      (gethash '$points             *vtk3d-graphic-objects*) 'vtk3d-points
      (gethash '$parametric         *vtk3d-graphic-objects*) 'vtk3d-parametric
      (gethash '$parametric_surface *vtk3d-graphic-objects*) 'vtk3d-parametric_surface
      (gethash '$spherical          *vtk3d-graphic-objects*) 'vtk3d-spherical
      (gethash '$cylindrical        *vtk3d-graphic-objects*) 'vtk3d-cylindrical
      (gethash '$explicit           *vtk3d-graphic-objects*) 'vtk3d-explicit
      (gethash '$label              *vtk3d-graphic-objects*) 'vtk3d-label
      (gethash '$tube               *vtk3d-graphic-objects*) 'vtk3d-tube )

(defun make-vtk-scene-3d (args)
  (let ((objects              "")
        (appenddata-name      (get-appenddata-name))
        (outline-name         (get-outline-name))
        (polydatamapper-name  (get-polydatamapper-name))
        (outlineactor-name    (get-outlineactor-name))
        (textproperty-name    (get-textproperty-name))
        (cubeaxesactor2d-name (get-cubeaxesactor2d-name))
        (renderer-name        (get-renderer-name))
        (camera-name          (get-camera-name))
        (first-actor          (+ *vtk-actor-counter* 1))
        (first-labelactor     (+ *vtk-labelactor-counter* 1))
        (first-extract        (+ *vtk-extract-counter* 1))
        (first-label          (+ *vtk-label-counter* 1))
        largs obj)
    (ini-gr-options)
    (ini-local-option-variables)
    (user-defaults)
    (setf largs (listify-arguments args))
    (dolist (x largs)
      (cond ((equal ($op x) "=")
              (case ($lhs x)
                ($allocation        (update-allocation                           ($rhs x)))
                ($axis_3d           (update-boolean-option    '$axis_3d          ($rhs x)))
                ($background_color  (update-color             '$background_color ($rhs x)))
                ($capping           (update-capping                              ($rhs x)))
                ($color             (update-color             '$color            ($rhs x)))
                ($isolines_color    (update-color             '$isolines_color   ($rhs x)))
                ($isolines_levels   (update-contour-isolines  '$isolines_levels  ($rhs x)))
                ($dimensions        (update-dimensions                           ($rhs x)))
                ($enhanced3d        (update-enhanced3d                           ($rhs x)))
                ($file_name         (update-string            '$file_name        ($rhs x)))
                ($font_size         (update-positive-float    '$font_size        ($rhs x)))
                ($head_angle        (update-positive-float    '$head_angle       ($rhs x)))
                ($head_length       (update-positive-float    '$head_length      ($rhs x)))
                ($isolines          (update-isolines                             ($rhs x)))
                ($line_type         (update-linestyle         '$line_type        ($rhs x)))
                ($line_width        (update-positive-float    '$line_width       ($rhs x)))
                ($nticks            (update-positive-integer  '$nticks           ($rhs x)))
                ($opacity           (update-opacity                              ($rhs x)))
                ($palette           (update-palette                              ($rhs x)))
                ($points_joined     (update-pointsjoined                         ($rhs x)))
                ($point_size        (update-nonnegative-float '$point_size       ($rhs x)))
                ($point_type        (update-pointtype                            ($rhs x)))
                ($terminal          (update-terminal                             ($rhs x)))
                ($transform         (update-transform                            ($rhs x)))
                ($unit_vectors      (update-boolean-option    '$unit_vectors     ($rhs x)))
                ($view              (update-view                                 ($rhs x)))
                ($wired_surface     (update-boolean-option    '$wired_surface    ($rhs x)))
                ($xu_grid           (update-positive-integer  '$xu_grid          ($rhs x)))
                ($yv_grid           (update-positive-integer  '$yv_grid          ($rhs x)))
                ($x_voxel           (update-positive-integer  '$x_voxel          ($rhs x)))
                ($y_voxel           (update-positive-integer  '$y_voxel          ($rhs x)))
                ($z_voxel           (update-positive-integer  '$z_voxel          ($rhs x)))
                ($xlabel            (update-string            '$xlabel           ($rhs x)))
                ($ylabel            (update-string            '$ylabel           ($rhs x)))
                ($zlabel            (update-string            '$zlabel           ($rhs x)))

                ; must be changed to script_file_name
                ($gnuplot_file_name (update-string            '$gnuplot_file_name           ($rhs x)))

                ; options not yet implemented for 3D-vtk
                ; they are included here to avoid error messages
                ($surface_hide      (update-boolean-option    '$surface_hide      ($rhs x)))
                ($label_alignment   (update-string            '$label_alignment   ($rhs x)))
                ($label_orientation (update-string            '$label_orientation ($rhs x)))
                ($xrange            (update-gr-option         '$xrange            ($rhs x)))
                ($yrange            (update-gr-option         '$yrange            ($rhs x)))
                ($zrange            (update-gr-option         '$zrange            ($rhs x)))

                (otherwise (merror "vtk3d: unknown option ~M " ($lhs x)))))

            ((setf obj (gethash (caar x) *vtk3d-graphic-objects*))
               (setf objects
                     (concatenate 'string
                       objects
                       (apply obj (rest x)))))

            (t
              (merror "vtk3d: item ~M is not recognized" x))))    
    ; scene allocation
    (setf *allocations* (cons (get-option '$allocation) *allocations*))
    (concatenate 'string
      objects
      (if (> *vtk-extract-counter* 0)
        (scenebounds)
        "")
      (vtkappendpolydata-code appenddata-name first-extract first-label)
      (vtkoutlinefilter-code outline-name appenddata-name)
      (vtkpolydatamapper-code polydatamapper-name outline-name t)
      (vtkactor-code outlineactor-name polydatamapper-name "#ff0000" 1 1 nil)
      (vtktextproperty-code textproperty-name)
      (vtkcubeaxesactor2d-code cubeaxesactor2d-name appenddata-name textproperty-name)
      (vtkrenderer3d-code
        renderer-name
        cubeaxesactor2d-name
        outlineactor-name
        (get-option '$background_color)
        camera-name
        (car  (get-option '$view))
        (cadr (get-option '$view))
        first-actor
        first-labelactor ) )))



(defun draw_vtk (&rest args)
  (let ((scenes nil)
        (cmdstorage "")
        gfn largs)
    (ini-gr-options)
    (ini-global-options)
    (ini-local-option-variables)
    (user-defaults)
    (setf *allocations* nil)
    (setf *vtk-appenddata-counter* 0
          *vtk-extract-counter* 0
          *vtk-outline-counter* 0
          *vtk-polydatamapper-counter* 0
          *vtk-outlineactor-counter* 0
          *vtk-textproperty-counter* 0
          *vtk-cubeaxesactor2d-counter* 0
          *vtk-camera-counter* 0
          *vtk-renderer-counter* 0
          *vtk-source-counter* 0
          *vtk-mapper-counter* 0
          *vtk-actor-counter* 0
          *vtk-labelactor-counter* 0
          *vtk-trans-counter* 0
          *vtk-filter-counter* 0
          *vtk-data-file-counter* 0
          *vtk-floatarray-counter* 0
          *vtk-points-counter* 0
          *vtk-glyphpoints-counter* 0
          *vtk-polydata-counter* 0
          *vtk-cellarray-counter* 0
          *vtk-polydatamapper-counter* 0
          *vtk-solidsource-counter* 0
          *vtk-triangle-counter* 0
          *vtk-label-counter* 0
          *vtk-chart-counter* 0
          *vtk-table-counter* 0
          *vtk-arrayX-counter* 0
          *vtk-arrayY-counter* 0
          *vtk-2dkey-counter* 0
          *vtk-isolines-counter* 0
          *lookup-tables* nil
          *unitscale-already-defined* nil)
    (setf largs (listify-arguments args))
    (dolist (x largs)
      (cond ((equal ($op x) "=")
              (case ($lhs x)
                ($terminal         (update-terminal                             ($rhs x)))
                ($columns          (update-positive-integer  '$columns          ($rhs x)))
                ($dimensions       (update-dimensions                           ($rhs x)))
                ($file_name        (update-string            '$file_name        ($rhs x)))
                ($background_color (update-color             '$background_color ($rhs x)))
                (otherwise (merror "draw: unknown global option ~M " ($lhs x)))))
            ((equal (caar x) '$gr3d)
              (setf scenes (append scenes (list (funcall #'make-vtk-scene-3d (rest x))))))
            ((equal (caar x) '$gr2d)
              (setf scenes (append scenes (list (funcall #'make-vtk-scene-2d (rest x))))))
            (t
              (merror "draw: item ~M is not recognized" x))) )

    ;; prepare script file
    (setf gfn (plot-temp-file (format nil "maxout~d.py" (getpid))))
    (setf cmdstorage
      (open gfn
            :direction :output :if-exists :supersede))
    (when (eql cmdstorage nil)
      (merror "draw: Cannot create file '~a'. Probably maxima_tempdir doesn't point to a writable directory." gfn))

    ;; pull in requiered packages
    (format cmdstorage "~a~%~a~%~%~a~%~a~%~%~a~%~%"
      "#!/usr/bin/env python"
      "# -*- coding: UTF-8 -*-"
      "import vtk"
      "import sys"
      "bounds=[]")

    ;: write scenes
    (dolist (scn scenes)
      (format cmdstorage "~a" scn) )

    ;; the renderer window
    (format cmdstorage "~a" (vtkrendererwindow-code (length scenes)))
    (format cmdstorage "~a" (vtk-terminal))

    ;; close script file
    (close cmdstorage)

    #+(or windows win32 win64)
    ($system "vtkpython " gfn)
    #-(or windows win32 win64)
    (if (member $draw_renderer '($vtk $vtk6))
      ($system (format nil "(python \"~a\")&" gfn))
      ($system (format nil "(python3 \"~a\")&" gfn)))

    '$done))
