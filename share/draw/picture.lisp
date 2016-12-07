;;                 COPYRIGHT NOTICE
;;  
;;  Copyright (C) 2007-2016 Mario Rodriguez Riotorto
;;  
;;  This program is free software; you can redistribute
;;  it and/or modify it under the terms of the
;;  GNU General Public License as published by
;;  the Free Software Foundation; either version 2 
;;  of the License, or (at your option) any later version. 
;;  
;;  This program is distributed in the hope that it
;;  will be useful, but WITHOUT ANY WARRANTY;
;;  without even the implied warranty of MERCHANTABILITY
;;  or FITNESS FOR A PARTICULAR PURPOSE. See the 
;;  GNU General Public License for more details at
;;  http://www.gnu.org/copyleft/gpl.html

;;  The picture package.  UNSTABLE !!

;; For questions, suggestions, bugs and the like, feel free
;; to contact me at
;; mario @@@ edu DOT xunta DOT es
;; http://tecnostats.net/Maxima/gnuplot



(defun cut-and-round (seq)
  (map 'list
       #'(lambda (z)
            (let ((fz (meval `($float ,z))))
              (cond
                ((< fz 0) 0)
                ((> fz 255) 255)
                (t (round fz)))))
       seq) )



;; Constructs a levels picture. This object contains four parts:
;; 1) symbol 'level
;; 2) image width
;; 3) image height
;; 4) an integer array with pixel data ranging from 0 to 255.
;; Argument data must contain only numbers ranged from 0 to 255;
;; negative numbers are substituted by 0, and those which are
;; greater than 255 are set to 255. If data is a Maxima list,
;; width and height must be given.
(defun $make_level_picture (data &optional (wi nil) (he nil))
   (let (width height picarray)
      (cond
        (($matrixp data)
           (setf width  (length (cdadr data))
                 height (length (cdr data)))
           (setf picarray (make-array (* height width)
                             :element-type  'integer
                             :initial-contents (cut-and-round (rest ($flatten ($args data)))))) )
        ((and ($listp data)
              (integerp wi)
              (integerp he)
              (= (* wi he) ($length data)))
           (setf width  wi
                 height he
                 picarray (make-array (* wi he)
                             :element-type  'integer
                             :initial-contents (cut-and-round (rest data)))))
        (t
           (merror "Argument should be a matrix or a list of numbers")))
   (list '(picture simp) '$level width height picarray) ))



;; Returns true if the argument is a well formed image,
;; and false otherwise
(defun $picturep (im)
   (cond ((atom im)
            nil)
         ((and (= (length im) 5)
               (equal (car im) '(picture simp)))
            t)
         (t
            (and (equal (length im) 5)
                 (equal (car im) '(picture ))
                 (or (member (cadr im) '($level $rgb)))
                 (arrayp (nth 4 im))
                 (cond ((equal (nth 1 im) '$level)
                         (= (array-dimension (nth 4 im) 0)
                            (* (nth 2 im) (nth 3 im))))
                       (t ; rgb image
                         (= (array-dimension (nth 4 im) 0)
                            (* 3 (nth 2 im) (nth 3 im)))))
                 (every #'(lambda (z) (and (integerp z) (>= z 0) (<= z 255))) (nth 4 im))  ))))



;; Returns true in case of equal pictures, and false otherwise.
(defun $picture_equalp (pic1 pic2)
  (if (and ($picturep pic1) ($picturep pic2))
     (equalp pic1 pic2)
     (merror "Two picture objects are required")))



;; Constructs a coloured rgb picture. This object contains four parts:
;; 1) symbol 'rgb
;; 2) image width
;; 3) image height
;; 4) an integer array of length 3*width*height with pixel data ranging
;;   from 0 to 255. Each pixel is represented by three consecutive numbers
;;  (red, green, blue). Arguments must contain the three channels in
;;  level_picture.
(defun $make_rgb_picture (redlevel greenlevel bluelevel)
   (when (not (and ($picturep redlevel)
                   (equal (cadr redlevel)   '$level)
                   ($picturep greenlevel)
                   (equal (cadr greenlevel) '$level)
                   ($picturep bluelevel)
                   (equal (cadr bluelevel)  '$level)))
      (merror "Color channel is not a levels picture object"))
   (when (not (and (= (caddr redlevel) (caddr greenlevel) (caddr bluelevel))
                   (= (cadddr redlevel) (cadddr greenlevel) (cadddr bluelevel)) ))
      (merror "Color channels are not of equal dimensions"))
   (let (width height leng picarray i3)
      (setf width  (caddr redlevel)
            height (cadddr redlevel))
      (setf leng (* width height))
      (setf picarray (make-array (* 3 leng) :element-type  'integer))
      (loop for i from 0 below leng do
        (setf i3 (* 3 i))
        (setf (aref picarray i3)        (aref (nth 4 redlevel)   i))
        (setf (aref picarray (incf i3)) (aref (nth 4 greenlevel) i))
        (setf (aref picarray (incf i3)) (aref (nth 4 bluelevel)  i)))
      (list '(picture simp) '$rgb width height picarray) ))



;; Extracts color channel ('red, 'green or 'blue) from a coloured picture.
;; Returns a levels picture.
(defun $take_channel (pic chn)
  (when (not (and ($picturep pic)
                  (equal (cadr pic) '$rgb)))
    (merror "Argument is not a coloured picture"))
  (when (not (member chn '($red $green $blue)))
    (merror "Incorrect colour channel"))
  (let* ((width  (caddr  pic))
         (height (cadddr pic))
         (dim (* width height))
         (img (make-array dim :element-type 'integer))
         idx)
    (setf idx
          (case chn
            ($red   0)
            ($green 1)
            ($blue  2)))
    (loop for i from 0 below dim do
      (setf (aref img i) (aref (nth 4 pic) (+ (* 3 i) idx))))
    (list '(picture simp) '$level width height img) ))



;; Returns the negative of a (level or rgb) picture
(defun $negative_picture (pic)
  (if (not ($picturep pic))
      (merror "Argument is not a picture"))
  (let ((dim (array-dimension (nth 4 pic) 0))
        (arr (make-array (array-dimension (nth 4 pic) 0) :element-type 'integer)))
    (loop for i from 0 below dim do
      (setf (aref arr i) (- 255 (aref (nth 4 pic) i))))
    (list '(picture simp)
          (nth 1 pic)
          (nth 2 pic)
          (nth 3 pic)
          arr)))



;; Transforms an rgb picture into a level one by
;; averaging the red, green and blue values. 
(defun $rgb2level (pic)
  (if (or (not ($picturep pic))
          (not (equal (nth 1 pic) '$rgb)))
      (merror "Argument is not an rgb picture"))
  (let* ((dim (* (nth 2 pic) (nth 3 pic)))
         (arr (make-array dim :element-type 'integer))
         (k -1))
    (loop for i from 0 below dim do
      (setf (aref arr i) (round (/ (+ (aref (nth 4 pic) (incf k))
                                      (aref (nth 4 pic) (incf k))
                                      (aref (nth 4 pic) (incf k)))
                                   3))))
    (list '(picture simp)
          '$level
          (nth 2 pic)
          (nth 3 pic)
          arr)))



;; Returns pixel from picture. Coordinates x and y range from 0 to
;; (width-1) and (height-1), respectively. We are working
;; with arrays, not with lists.
(defun $get_pixel (pic x y)
  (when (not ($picturep pic))
    (merror "Argument is not a well formed picture"))
  (when (not (and (integerp x) (integerp y)))
    (merror "Pixel coordinates must be positive integers"))
  (when (not (and (> x -1)
                  (< x (nth 2 pic))
                  (> y -1)
                  (< y (nth 3 pic))))
    (merror "Pixel coordinates out of range"))
  (case (nth 1 pic)
    ($level (aref (nth 4 pic) (+ x (* y (nth 2 pic)))))
    ($rgb   (let ((pos (* 3 (+ x (* y (nth 2 pic))))))
               (list
                 '(mlist simp)
                 (aref (nth 4 pic) pos)
                 (aref (nth 4 pic) (incf pos))
                 (aref (nth 4 pic) (incf pos)))))))








;;;    XPM   I M A G E   F O R M A T   S U P P O R T

;; The following functions have been taken from
;; http://common-lisp.net/project/gamelib/ (MIT license)
;; Changes have been made to fit the Maxima environment.


(defvar *xpm-readtable* nil)


(defun init-readtable ()
  (unless *xpm-readtable*
    (setf *xpm-readtable* (copy-readtable))
    (set-syntax-from-char #\, #\Space *xpm-readtable*)))


(defun skip-whitespace (f)
  (loop for c = (read-char f)
	while (member c '(#\space #\tab) :test #'char=)
	finally (unread-char c f)
	))


(defun read-colour (f)
  (let ((ctype (read-char f)))
    (case ctype

      ; color in hexadecimal format
      (#\#
        (let ((*read-base* 16))
          (read f)))

      ; color name:
      ; 0. read the rest of the name and append the first letter
      ; 1. get the hexadecimal code from *color-table* defined in grcommon.lisp
      ; 2. remove # and transform the code to an integer in base 10
      (otherwise
        (parse-integer
          (subseq (gethash (atom-to-downcased-string (format nil "~a~a" ctype (read f))) *color-table*) 1)
          :radix 16)) )))


(defun read-charspec (f cnt)
  (format nil "~{~c~}"
	  (loop for n from 0 below cnt
		collect (read-char f))))


(defun read-cspec (str cnt hash)
  (with-input-from-string (cs str)
     (let ((chars (read-charspec cs cnt)))
       (skip-whitespace cs)
       (let ((c (read-char cs)))
	 (if (char= c #\c)
	     (let ((col (progn
			  (skip-whitespace cs)
			  (read-colour cs))))
	       (setf (gethash chars hash) col))
	   (merror "Unknown colourspec"))))))


(defun $read_xpm (mfspec)
  (init-readtable)
  (let ((*readtable* *xpm-readtable*)
        (fspec (string-trim "\"" (coerce (mstring mfspec) 'string))) )
    (with-open-file (image fspec :direction :input)
      (read-line image) ; Skip initial comment
      (read-line image) ; Skip C code
      (let ((colspec (read image))
	    width
	    height
	    (chartab (make-hash-table :test #'equal))
	    img)
	(with-input-from-string (cspec colspec)
	  (setf width (read cspec))
	  (setf height (read cspec))
	  (let ((colours (read cspec))
		(cpp (read cspec))
                rgb (counter -1))
	    (loop for cix from 0 below colours
		  for c = (read image)
		  do (read-cspec c cpp chartab))
	    (setf img (make-array (* 3 width height) :element-type 'integer))
	    (loop for y from 0 below height
		  for line = (read image)
		  do (progn
		       (with-input-from-string (data line)
		       (loop for x from 0 below width
		             for cs = (read-charspec data cpp) do
                           (setf rgb (gethash cs chartab))
                           (setf (aref img (incf counter)) (/ (logand rgb 16711680) 65536))
                           (setf (aref img (incf counter)) (/ (logand rgb 65280) 256))
                           (setf (aref img (incf counter)) (logand rgb 255)))))   )
	    (list '(picture simp) '$rgb width height img)))))))

