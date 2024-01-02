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
           (merror "make_level_picture: argument must be a matrix, or a list of integers with length = width*height.")))
   (list '($picture) '$level width height picarray) ))



;; Returns true if the argument is a well formed image,
;; and false otherwise

(defun $picturep (x)
  (and
    (consp x)
    (eq (caar x) '$picture)
    (= (length x) 5)
    (member (second x) '($level $rgb $rgb_alpha))
    (integerp (third x))
    (integerp (fourth x))
    (vectorp (fifth x))
    (cond
      ((eq (second x) '$level)
       (= (length (fifth x)) (* (third x) (fourth x))))
      ((eq (second x) '$rgb)
       (= (length (fifth x)) (* 3 (third x) (fourth x))))
      ((eq (second x) '$rgb_alpha)
       (= (length (fifth x)) (* 4 (third x) (fourth x)))))
    (every #'(lambda (z) (and (integerp z) (>= z 0) (<= z 255))) (fifth x))))



;; Returns true in case of equal pictures, and false otherwise.
(defun $picture_equalp (pic1 pic2)
  (if (and ($picturep pic1) ($picturep pic2))
     (alike1 pic1 pic2)
     (merror "picture_equalp: both arguments must be picture objects.")))



;; Constructs a coloured rgb picture. This object contains four parts:
;; 1) symbol 'rgb
;; 2) image width
;; 3) image height
;; 4) an integer array of length 3*width*height with pixel data ranging
;;   from 0 to 255. Each pixel is represented by three consecutive numbers
;;  (red, green, blue). Arguments must contain the three channels in
;;  level_picture.
(defun $make_rgb_picture (redlevel greenlevel bluelevel) ;; TODO: let alpha be an optional argument
   (when (not (and ($picturep redlevel)
                   (equal (cadr redlevel)   '$level)
                   ($picturep greenlevel)
                   (equal (cadr greenlevel) '$level)
                   ($picturep bluelevel)
                   (equal (cadr bluelevel)  '$level)))
      (merror "make_rgb_picture: every color channel must be a picture object with type = level."))
   (when (not (and (= (caddr redlevel) (caddr greenlevel) (caddr bluelevel))
                   (= (cadddr redlevel) (cadddr greenlevel) (cadddr bluelevel)) ))
      (merror "make_rgb_picture: color channels are not of equal dimensions."))
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
      (list '($picture) '$rgb width height picarray) ))



;; Extracts color channel ('red, 'green or 'blue) from a coloured picture.
;; TODO: handle CHN = '$ALPHA as well.
;; Returns a levels picture.
(defun $take_channel (pic chn)
  (when (not (and ($picturep pic)
                  (or (equal (cadr pic) '$rgb) (equal (cadr pic) '$rgb_alpha))))
    (merror "take_channel: first argument must be a picture object with type = rgb or rgb_alpha."))
  (when (not (member chn '($red $green $blue)))
    (merror "take_channel: color channel must be red, green, or blue."))
  (let* ((width  (caddr  pic))
         (height (cadddr pic))
         (dim (* width height))
         (img (make-array dim :element-type 'integer))
         (stride (if (eq (second pic) '$rgb) 3 4))
         idx)
    (setf idx
          (case chn
            ($red   0)
            ($green 1)
            ($blue  2)))
    (loop for i from 0 below dim do
      (setf (aref img i) (aref (nth 4 pic) (+ (* stride i) idx))))
    (list '($picture) '$level width height img) ))



;; Returns the negative of a (level or rgb) picture.
;;
;; For RGB images with alpha channel, take the negative of RGB channels,
;; and return alpha channel unmodified.

(defun $negative_picture (pic)
  (if (not ($picturep pic))
      (merror "negative_picture: argument must be a picture object."))
  (let ((dim (array-dimension (nth 4 pic) 0))
        (stride (if (eq (second pic) '$rgb) 3 (if (eq (second pic) '$level) 1 4)))
        (arr (copy-seq (nth 4 pic))))

    (loop for i from 0 below (/ dim stride) do
          (let ((base (* i stride)))
            (if (eq (second pic) '$level)
              (setf (aref arr (+ base 0)) (- 255 (aref arr (+ base 0))))
              (setf
                (aref arr (+ base 0)) (- 255 (aref arr (+ base 0)))
                (aref arr (+ base 1)) (- 255 (aref arr (+ base 1)))
                (aref arr (+ base 2)) (- 255 (aref arr (+ base 2)))))))

    (list '($picture)
          (nth 1 pic)
          (nth 2 pic)
          (nth 3 pic)
          arr)))



;; Transforms an rgb picture into a level one by
;; averaging the red, green and blue values. 
;; The alpha channel, if present, is ignored.
;; TODO: I wonder if it makes sense to copy alpha into a would-be
;; "level + alpha" image.
(defun $rgb2level (pic)
  (if (or (not ($picturep pic))
          (not (or (equal (nth 1 pic) '$rgb) (equal (nth 1 pic) '$rgb_alpha))))
      (merror "rgb2level: argument must be a picture object with type = rgb or rgb_alpha."))
  (let* ((level-dim (* (nth 2 pic) (nth 3 pic)))
         (rgb-stride (if (eq (nth 1 pic) '$rgb) 3 4))
         (rgb-array (nth 4 pic))
         (level-array (make-array level-dim :element-type 'integer)))

    (loop for i from 0 below level-dim do
          (let ((rgb-base (* rgb-stride i)))
            (setf (aref level-array i)
              (round (/ (+
                          (aref rgb-array (+ rgb-base 0))
                          (aref rgb-array (+ rgb-base 1))
                          (aref rgb-array (+ rgb-base 2)))
                        3)))))

    (list '($picture)
          '$level
          (nth 2 pic)
          (nth 3 pic)
          level-array)))



;; Returns pixel from picture. Coordinates x and y range from 0 to
;; (width-1) and (height-1), respectively. We are working
;; with arrays, not with lists.
;;
;; For RGB pictures, return a list of 3 elements, R, G, and B.
;; For RGB + alpha pictures, return a list of 4 elements, R, G, B, and alpha.

(defun $get_pixel (pic x y)
  (when (not ($picturep pic))
    (merror "get_pixel: first argument must be a picture object."))
  (when (not (and (integerp x) (integerp y)))
    (merror "get_pixel: pixel coordinates must be integers."))
  (when (not (and (> x -1)
                  (< x (nth 2 pic))
                  (> y -1)
                  (< y (nth 3 pic))))
    (merror "get_pixel: pixel coordinates out of range."))
  (let ((stride (if (eq (second pic) '$rgb) 3 (if (eq (second pic) '$level) 1 4))))
    (case (nth 1 pic)
      ($level
        (aref (nth 4 pic) (+ x (* y (nth 2 pic)))))
      ($rgb
       (let
         ((pos (* stride (+ x (* y (nth 2 pic))))))
         (list
           '(mlist)
           (aref (nth 4 pic) pos)
           (aref (nth 4 pic) (incf pos))
           (aref (nth 4 pic) (incf pos)))))
      ($rgb_alpha
       (let
         ((pos (* stride (+ x (* y (nth 2 pic))))))
         (list
           '(mlist)
           (aref (nth 4 pic) pos)
           (aref (nth 4 pic) (incf pos))
           (aref (nth 4 pic) (incf pos))
           (aref (nth 4 pic) (incf pos))))))))








;;;    XPM   I M A G E   F O R M A T   S U P P O R T

;; The following functions have been taken from
;; http://common-lisp.net/project/gamelib/ (MIT license)
;; Changes have been made to fit the Maxima environment.


(defvar *xpm-readtable* nil)


(defun init-readtable ()
  (unless *xpm-readtable*
    (setf *xpm-readtable* (copy-readtable))
    (set-macro-character #\/ #'(lambda (s c) (declare (ignore c)) (let ((*parse-stream* s)) (gobble-comment)) (values)) nil *xpm-readtable*)
    (set-syntax-from-char #\, #\Space *xpm-readtable*)))


(defun skip-whitespace (f)
  (loop for c = (read-char f nil)
	while (and c (member c '(#\space #\tab) :test #'char=))
	finally (when c (unread-char c f))
	))


(defun extract-r-g-b-bits (n-bits-per-color-output n-hex-digits-input value)
  (let*
    ((n-bits-per-color-input (ceiling (/ (* n-hex-digits-input 4) 3)))
     (r-bits-input (ash value (- (* n-bits-per-color-input 2))))
     (g-bits-input (mod (ash value (- n-bits-per-color-input)) (ash 1 n-bits-per-color-input)))
     (b-bits-input (mod value (ash 1 n-bits-per-color-input)))
     (r-bits-output (ash r-bits-input (- n-bits-per-color-output n-bits-per-color-input)))
     (g-bits-output (ash g-bits-input (- n-bits-per-color-output n-bits-per-color-input)))
     (b-bits-output (ash b-bits-input (- n-bits-per-color-output n-bits-per-color-input))))
    (list r-bits-output g-bits-output b-bits-output)))


(defun read-colour (f)
  (let ((ctype (read-char f)))
    (case ctype

      (#\#

        ; We have encountered an RGB color in hexadecimal format.
        ; Return a list of two elements comprising packed rgb bits and alpha = 255 (fully opaque color).

        (let (a)
          (loop for c = (read-char f nil) while (and c (digit-char-p c 16)) finally (when c (unread-char c f)) do (push c a))
          (multiple-value-bind (value ndigits)
            (parse-integer (coerce (reverse a) 'string) :radix 16)
            ; Some XPM images contain 12 hex digits per color;
            ; the XPM spec itself seems to be silent about how many are allowed.
            ; To accommodate code for picture objects here in share/draw,
            ; truncate colors to 8 bits (i.e., 2 hex digits per color, 6 hex digits in all).
            ; When NDIGITS = 6, EXTRACT-R-G-B-BITS just splits COLOR-BITS without shifting.
            (list (extract-r-g-b-bits 8 ndigits value) 255))))

      ; color name:
      ; 0. read the rest of the name and append the first letter
      ; 1. get the hexadecimal code from *color-table* defined in grcommon.lisp
      ; 2. remove # and transform the code to an integer in base 10
      (otherwise
        (let ((color-name (atom-to-downcased-string (format nil "~a~a" ctype (read f)))) alpha color-bits)
          (if (string= color-name "none")
            (setq alpha 0 color-bits 0)
            (let ((color-table-value (gethash color-name *color-table*)))
              (setq alpha 255)
              (if (null color-table-value)
                (merror "read_xpm: unrecognized color name ~M" color-name)
                (setq color-bits (parse-integer (subseq color-table-value 1) :radix 16)))))
          ; For call to EXTRACT-R-G-B-BITS, assume all color table items are 6 hex digits.
          ; Given these arguments, EXTRACT-R-G-B-BITS just splits COLOR-BITS without shifting.
          (list (extract-r-g-b-bits 8 6 color-bits) alpha))))))


(defun read-charspec (f cnt)
  (format nil "~{~c~}"
	  (loop for n from 0 below cnt
		collect (read-char f))))


(defun read-color-spec-for-type (str cnt hash type-char)
  (with-input-from-string (cs str)
     (let (c (chars (read-charspec cs cnt)))
       (skip-whitespace cs)
       (setq c (read-char cs))
       (loop while (and c (char/= c type-char))
             do (read cs nil)
                (skip-whitespace cs)
                (setq c (read-char cs nil)))
       (if (and c (char= c type-char))
         (let ((color-spec (progn (skip-whitespace cs) (read-colour cs))))
           (setf (gethash chars hash) color-spec))))))

(defun read-color-spec (str cnt hash)
  (or (read-color-spec-for-type str cnt hash #\c)
      (read-color-spec-for-type str cnt hash #\m)
      (read-color-spec-for-type str cnt hash #\g)
      (merror "read_xpm: failed to parse color specification ''~M''" str)))

(defun $read_xpm (mfspec)
  (cond
    ((streamp mfspec)
     (read-xpm-from-stream mfspec))
    (t
      (let ((fspec (string-trim "\"" (coerce (mstring mfspec) 'string))))
        (with-open-file (image fspec :direction :input)
          (read-xpm-from-stream image))))))

(defun read-xpm-from-stream (image)
  (init-readtable)
  (let ((*readtable* *xpm-readtable*))
      (let
        ((first-line-raw (ignore-errors (read-line image))))
        (if first-line-raw
          (let ((first-line (string-trim '(#\Space #\Tab #\Return #\Newline) first-line-raw)))
            (when (string/= first-line "/* XPM */")
              (if (string= first-line "! XPM2")
                (merror "read_xpm: I don't know how to read XPM2 format.")
                (merror "read_xpm: input doesn't appear to be XPM3 format; first line: ~M" first-line))))
          (merror "read_xpm: failed to read first line; are you sure this is an XPM image?")))
      ; Burn off any additional comment or comments, and C code ending in left curly brace.
      (loop for x = (read image) while (not (eq x '{)))
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
                rgb+alpha (counter -1))
	    (loop for cix from 0 below colours
		  for c = (read image)
		  do (read-color-spec c cpp chartab))
	    (setf img (make-array (* 4 width height) :element-type 'integer))
	    (loop for y from 0 below height
		  for line = (read image)
		  do (progn
               (when (not (stringp line))
                 (merror "read_xpm: failed to read ~M'th line of image; found: ~M" (1+ y) line))
		       (with-input-from-string (data line)
		       (loop for x from 0 below width
		             for cs = (read-charspec data cpp) do
                           (setf rgb+alpha (gethash cs chartab))
                           (let
                             ((rgb (first rgb+alpha))
                              (alpha (second rgb+alpha)))
                             (setf (aref img (incf counter)) (first rgb))
                             (setf (aref img (incf counter)) (second rgb))
                             (setf (aref img (incf counter)) (third rgb))
                             (setf (aref img (incf counter)) alpha))))   ))
	    (list '($picture) '$rgb_alpha width height img))))))

