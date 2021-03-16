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

(defmethod plot-preamble ((plot geomview-plot) options)
  (setf (slot-value plot 'data) (format nil "LIST~%"))
  (values))

(defmethod plot3d-command ((plot geomview-plot) functions options titles)
  (let ((i 0) fun xrange yrange lvars trans)
    (setf
     (slot-value plot 'data)
     (concatenate
      'string
      (slot-value plot 'data)
      (with-output-to-string ($pstream)
        ;; generate the mesh points for each surface in the functions stack
        (dolist (f functions)
          (setq i (+ 1 i))
          (setq fun (first f))
          (setq xrange (second f))
          (setq yrange (third f))
          (if ($listp fun)
              (progn
                (setq trans
                      ($make_transform `((mlist) ,(second xrange)
                                         ,(second yrange) $z)
                                       (second fun) (third fun) (fourth fun)))
                (setq fun '$zero_fun))
              (let*
                  ((x0 (third xrange))
                   (x1 (fourth xrange))
                   (y0 (third yrange))
                   (y1 (fourth yrange))
                   (xmid (+ x0 (/ (- x1 x0) 2)))
                   (ymid (+ y0 (/ (- y1 y0) 2))))
                (setq lvars `((mlist) ,(second xrange) ,(second yrange)))
                (setq fun (coerce-float-fun fun lvars))
                ;; Evaluate FUN at the middle point of the range.
                ;; Looking at a single point is somewhat unreliable.
                ;; Call FUN with numerical arguments (symbolic arguments may
                ;; fail due to trouble computing real/imaginary parts for 
                ;; complicated expressions, or it may be a numerical function)
                (when (cdr ($listofvars (mfuncall fun xmid ymid)))
                  (mtell
                   (intl:gettext
                    "plot3d: expected <expr. of v1 and v2>, [v1,min,max], [v2,min,max]~%"))
                  (mtell
                   (intl:gettext
                    "plot3d: keep going and hope for the best.~%")))))
          (let* ((pl
                  (draw3d
                   fun (third xrange) (fourth xrange) (third yrange)
                   (fourth yrange) (first (getf options :grid))
                   (second (getf options :grid))))
                 (ar (polygon-pts pl)))
            (declare (type (cl:array t) ar))
            (when trans (mfuncall trans ar))
            (when (getf options :transform_xy)
                (mfuncall (getf options :transform_xy) ar))
            (format $pstream "{ appearance { +smooth }~%MESH ~a ~a ~%"
                    (+ 1 (first (getf options :grid)))
                    (+ 1 (second (getf options :grid))))
            (output-points pl nil)
            (format $pstream "}~%"))))))))

(defmethod plot-shipout ((plot geomview-plot) options &optional output-file)
  (let ((file (plot-file-path (format nil "maxout~d.geomview" (getpid)))))
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
