;;;                 COPYRIGHT NOTICE
;;;  
;;;  Copyright (C) 2009-2015 Mario Rodriguez Riotorto
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

;;; This is a Maxima sound package.

;;; For questions, suggestions, bugs and the like, feel free
;;; to contact me at
;;; mario @@@ edu DOT xunta DOT es


($put '$sound 0.0 '$version)


;; load package 'draw'
(when (null ($get '$draw '$version))
   ($load "draw"))

;; load package 'distrib'
(when (null ($get '$distrib '$version))
   ($load "distrib"))

;; load package 'numericalio'
($load "numericalio")

;; load package 'stringproc'
($load "stringproc")



(defvar $sound_sample_rate 16384)
(defvar $sound_sample nil)

(defun $sound_sample_size ()
    (when (arrayp $sound_sample)
       (second (array-dimensions $sound_sample))))

(defun $sound_sample_channels ()
    (when (arrayp $sound_sample)
       (first (array-dimensions $sound_sample))))

(defun $sound_sample_list (chn)
   (when (or (not (integerp chn))
             (< chn 0)
             (> chn ($sound_sample_channels)))
      (merror "sound: incorrect number of channels"))
   (let* ((n ($sound_sample_size))
          (arr (make-array n
                          :element-type 'flonum
                          :initial-element 0.0)))
      (declare (type fixnum n)
               (type (simple-array flonum (*)) arr))
      (dotimes (s n)
         (setf (aref arr s) (aref $sound_sample 0 s)))
      ($listarray arr)))



;; This variable stores actual sound options
(defvar *sound-options* (make-hash-table))

(defstruct a-wave
   sample      ; an array to store the samples of this wave
   channel     ; channel number
   att-coef    ; attenuation coefficients for the main wave and its delays
   repeat-at)  ; when must the wave be repeated



;; This variable stores user defaults
(defvar *user-sound-default-options* '())

(defun $set_sound_defaults (&rest opts)
   (setf *user-sound-default-options* opts)
   (cons '(mlist) opts))



;; Sets default values of sound options
(defun ini-sound-options ()
  (setf
    (gethash '$channel *sound-options*)           1
    (gethash '$file_name *sound-options*)         "maxout"
    (gethash '$file_format *sound-options*)       '$wav
    (gethash '$oscillator *sound-options*)        '(($sine) 1.0)
    (gethash '$envelope *sound-options*)          '$none
    (gethash '$noise_generator *sound-options*)   '$none
    (gethash '$attenuation_coef *sound-options*)  '((mlist) 1.0)
    (gethash '$normalize *sound-options*)         '$auto
    (gethash '$player *sound-options*)            '$none
    (gethash '$player_options *sound-options*)    '$none
    (gethash '$draw_wave_options *sound-options*) '((mlist))
    (gethash '$draw_wave *sound-options*)         nil
))



(ini-sound-options)



;; Gives value of option
(defun get-sound-option (opt) (gethash opt *sound-options*))



(defun update-sound-option (opt val)
   (case opt

      ($channel ; defined as a non negative integer
         (if (and (integerp val)
                  (plusp val))
            (setf (gethash opt *sound-options*) val)
            (merror "sound: illegal channel: ~M " val)))

      ($file_name
         (setf (gethash opt *sound-options*) ($sconcat val)))

      ($file_format ; defined as a wav or txt
         (setf val ($concat val))
         (if (member val '($wav $txt))
            (setf (gethash opt *sound-options*) val)
            (merror "sound: illegal file_format option: ~M " val)))

      ($oscillator
         (when ($atom val)
            (merror "sound: oscillator must be an expression"))
         (cond
            ((equal ($op val) '$sine)
               (let ((param (rest ($float ($args val)))))
                  (unless (every #'(lambda (z) (or (floatp z)
                                                   (and ($listp z)
                                                        (= ($length z) 2)
                                                        (floatp (cadr z))
                                                        (> (cadr z) 0)
                                                        (floatp (caddr z))
                                                        (<= 0.0 (caddr z))
                                                        (<= (caddr z) 1.0))))
                                     param)
                     (merror "sound: incorrect argument(s) in sine oscillator"))
                  (setf (gethash opt *sound-options*)
                        (cons (list '$sine 'simp) param)))  )
            ((member ($op val) '($rectangle $triangle))
               (let ((param ($float (cadr val))))
                 (unless (and (floatp param )
                              (> param 0.0)
                              (< param 1.0))
                    (merror "sound: incorrect argument in ~M oscillator" ($op val)))
                 (setf (gethash opt *sound-options*) val)))
            (t
               (merror "sound: oscillator not recognized"))))

      ($envelope
         (cond
            ((and ($atom val)
                  (equal val '$none))
               (setf (gethash opt *sound-options*) val))
            (($atom val)
               (merror "sound: unknown envelope"))
            ((equal ($op val) '$pairs)
               (let ((param (rest ($float ($args val)))))
                  (when (some #'(lambda (z) (or (not ($listp z))
                                                (/= ($length z) 2)
                                                (not (floatp (cadr z)))
                                                (not (floatp (caddr z))) ))
                              param)
                     (merror "sound: incorrect arguments to pairs envelope"))
                  (setf (gethash opt *sound-options*) (cons (list '$pairs 'simp)
                                                            (map 'list #'rest param))) ))

            ((equal ($op val) '$adsr)
                (let ((param (rest ($float ($args val)))))
                   (when (or (/= (length param) 4)
                             (some #'(lambda (z) (or (not (floatp z)) (< z 0.0)))
                                   param))
                      (merror "sound: adsr envelope needs four non negative arguments")  )
                   (let ((attack (car param))
                         (decay  (cadr param))
                         (sustain-level (caddr param))
                         (release (cadddr param)))
                      (when (or (> (+ attack decay release) 1.0)
                                (> sustain-level 1.0))
                         (merror "sound: incorrect arguments to adsr envelope"))
                      (setf (gethash opt *sound-options*) (list '($pairs simp)
                                                                (list 0.0 0.0)
                                                                (list attack 1.0)
                                                                (list (+ attack decay) sustain-level)
                                                                (list (- 1.0 release) sustain-level)
                                                                (list 1.0 0.0) )))))

            ((equal ($op val) '$function)
               (let ((param (rest ($float ($args val)))))
                  (when (or (/= ($length param) 3)
                            (not (floatp (nth 2 param)))
                            (not (floatp (nth 3 param)))
                            (not (< (nth 2 param) (nth 3 param))))
                     (merror "sound: incorrect arguments to function envelope"))
                  (setf (gethash opt *sound-options*) (cons (list '$function 'simp) param))))
            (t
               (merror "sound: unknown envelope type"))))

      ($noise_generator
         (cond
            ((and ($atom val)
                  (equal val '$none))
               (setf (gethash opt *sound-options*) val))
            (($atom val)
               (merror "sound: unknown noise generator"))
            ((equal ($op val) '$gaussian)
               (let ((param (rest ($float ($args val)))))
                  (when (or (/= (length param) 2)
                            (not (floatp (car param)))
                            (not (floatp (cadr param)))
                            (<= (cadr param) 0.0))
                     (merror "sound: gaussian noise generator is not correctly defined"))
                  (setf (gethash opt *sound-options*) (cons '($gaussian simp) param))))
            ((equal ($op val) '$uniform)
               (let ((param (rest ($float ($args val)))))
                  (when (or (/= (length param) 2)
                            (not (floatp (car param)))
                            (not (floatp (cadr param)))
                            (< (cadr param) (car param)))
                     (merror "sound: uniform noise generator is not correctly defined"))
                  (setf (gethash opt *sound-options*) (cons '($uniform simp) param))))
            (t
               (merror "sound: unknown noise generator"))))

      ($attenuation_coef
         (let ((coefs ($float val)))
            (cond
               ((and ($listp val)
                     (every #'floatp (rest coefs)))
                   (setf (gethash opt *sound-options*) coefs))
               (t
                   (merror "sound: illegal attenuation coefficients specification")))))

      ($player
         (setf (gethash opt *sound-options*) val))

      ($player_options
         (setf (gethash opt *sound-options*) val))

      ($draw_wave_options
         (if ($listp val)
            (setf (gethash opt *sound-options*) val)
            (merror "sound: draw_wave_options must be a list of draw options")))

      ($normalize
         (cond
            ((or (equal val '$auto)
                 (equal val '$none)
                 (and (integerp val)
                      (plusp val)
                      (<= val 32767)))
                (setf (gethash opt *sound-options*) val))
            (t
                (merror "sound: illegal normalize option: ~M " val))))

      ($draw_wave  ; defined as true or false
         (if (or (equal val t)
                 (null val))
            (setf (gethash opt *sound-options*) val)
            (merror "sound: non boolean value: ~M " val)))
))



;; Sets user default values of sound options
(defun sound-user-defaults ()
   (dolist (x *user-sound-default-options*)
      (if (equal ($op x) "=")
         (update-sound-option ($lhs x) ($rhs x))
         (merror "sound: item ~M is not recognized as an option assignment" x))))



;;;;;;;;;;;;;;;;;;;;;;;;
;;                    ;;
;;   File functions   ;;
;;                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;



;; Saves sound sample in plain text file, with one row per channel,
;; and as many columns as samples.
(defun $save_sound_txt ()
   ($write_data
      $sound_sample
      ($sconcat
         (get-sound-option '$file_name)
         ".txt")))



;; wav format info : http://www.sonicspot.com/guide/wavefiles.html
(defun $save_sound_wav ()
   (let ((num-chn (array-dimension $sound_sample 0))
         (num-sam (array-dimension $sound_sample 1))
         fname)
      (declare (type fixnum num-chn num-sam))
      (setf fname (get-sound-option '$file_name))
      (with-open-file (out (plot-temp-file ($sconcat fname ".wav"))
                      :direction :output
                      :if-exists :supersede
                      :element-type '(unsigned-byte 8))
        (flet ((write16 (i)
                  (write-byte (logand #xff i) out)
                  (write-byte (logand #xff (ash i -8)) out))
               (write32 (i)
                  (write-byte (logand #xff i) out)
                  (write-byte (logand #xff (ash i -8))  out)
                  (write-byte (logand #xff (ash i -16)) out)
                  (write-byte (logand #xff (ash i -24)) out)))
            (write32 #x46464952) ; string "RIFF"
            (write32 (+ (* 2 num-chn num-sam) 36)) ; filesize-8
            (write32 #x45564157) ; string "WAVE"
            (write32 #x20746d66) ; string "fmt "
            (write32 16) ; format bytes
            (write16 1)  ; compression code
            (write16 num-chn)  ; number of channels
            (write32 (round $sound_sample_rate)) ; sample rate
            (write32 (round (* 2.0 $sound_sample_rate num-chn))) ; average bytes per second
            (write16 (* 2 num-chn))   ; block align
            (write16 16)  ; significant bits per sample
            (write32 #x61746164) ; string "data"
            (write32 (* 2 num-chn num-sam))
            (dotimes (s num-sam)
               (dotimes (c num-chn)
                  (write16 (round (coerce (aref $sound_sample c s) 'single-float)))))))))



(defun $load_sound_wav (fname &optional (verbose t))
   (declare (type simple-string fname))
   (let ((file-size 0)
         (compression-code 0)
         (num-channels 0)
         (n-samples-per-sec 0)
         (average-bytes-per-second 0)
         (block-align 0)
         (n-bits-per-sample 0)  ; bits for one sample
         (total-bytes 0)        ; bytes occupied by the wave
         (n-bits-header 0)      ; sample data position
         (total-num-samples 0)
         (num-samples 0))
   (declare (type (unsigned-byte 16) compression-code num-channels
                         block-align n-bits-per-sample num-samples)
            (type (unsigned-byte 32) file-size n-samples-per-sec
                         average-bytes-per-second total-bytes)
            (type fixnum n-bits-header total-num-samples num-samples))
   (with-open-file (in fname
                    :direction :input
                    :element-type '(unsigned-byte 8))
      (flet ((read16 ()
                (let ((dat1 (read-byte in))
                      (dat2 (read-byte in)))
                   (setf (ldb (byte 8 8) dat1) dat2)
                   dat1))
             (read32 ()
                (let ((dat1 (read-byte in))
                      (dat2 (read-byte in))
                      (dat3 (read-byte in))
                      (dat4 (read-byte in)))
                   (setf (ldb (byte 8 8) dat1) dat2)
                   (setf (ldb (byte 8 16) dat1) dat3)
                   (setf (ldb (byte 8 24) dat1) dat4)
                   dat1)) )
         (unless (= (read32) #x46464952)
            (merror "sound: file to read is not of RIFF structure"))
         (setf file-size (read32))
         (unless (= (read32) #x45564157)
            (merror "sound: file to read is not of WAVE format"))

         ; look for format specification and sample length
         (loop
            (let* ((next-header (read32))
                   (bytes (read32)))
               (cond ((= next-header #x20746d66)
                        (setf compression-code (read16))
                        (setf num-channels (read16))
                        (setf n-samples-per-sec (read32))
                        (setf average-bytes-per-second (read32))
                        (setf block-align (read16))
                        (setf n-bits-per-sample (read16))
                        ;; possible extra (ignored) format bytes
                        (dotimes (i (- bytes 16)) (read-byte in)))
                     ((= next-header #x61746164)
                        (setf total-bytes bytes)
                        (return))
                     (t
                        (dotimes (i bytes) (read-byte in))))))
         (setf n-bits-header (* 8 (file-position in)))))

   ; with available parameters, let's read the file
   (setf total-num-samples (/ (* 8 total-bytes) n-bits-per-sample))
   (setf num-samples (/ total-num-samples num-channels))

   (when verbose
      (print (format nil "Number of channels.: ~a" num-channels))
      (print (format nil "Samples per second.: ~a" n-samples-per-sec))
      (print (format nil "Bits per sample....: ~a" n-bits-per-sample))
      (print (format nil "Number of samples..: ~a" total-num-samples)))

   (let ((sample-sequence (make-array total-num-samples
                                      :element-type 'fixnum
                                      :initial-element 0)))
      (declare (type (simple-array fixnum *) sample-sequence))
      (with-open-file (in fname
                       :direction :input
                       :element-type (if (= n-bits-per-sample 8)
                                        `(unsigned-byte ,n-bits-per-sample)
                                        `(signed-byte ,n-bits-per-sample)))
         (file-position in (/ n-bits-header n-bits-per-sample))
         (read-sequence sample-sequence in))
      (list
         '(mlist simp)
         n-samples-per-sec
         (cons
            '(mlist simp)
            (loop for k below num-samples collect
               (cons
                  '(mlist simp)
                  (loop for j below num-channels collect
                     (aref sample-sequence (+ j (* k num-channels)))))))))))



(defun save-sound ()
   (case (get-sound-option '$file_format)
      ($wav ($save_sound_wav))
      ($txt ($save_sound_txt))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           ;;
;;   Draw & play functions   ;;
;;                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun $draw_sound ()
   (let* ((num-chn (array-dimension $sound_sample 0))
          (num-sam (array-dimension $sound_sample 1))
          (time (coerce (/ num-sam $sound_sample_rate) 'flonum))
          (array1d (make-array num-sam :element-type 'flonum)))
      (declare (type fixnum num-chn num-sam)
               (type flonum time)
               (type (simple-array flonum *) array1d))
      (sound-user-defaults)
      ($apply
         '$draw
         (cons '(mlist simp)
               (loop for c from 0 below num-chn
                do (loop for s from 0 below num-sam
                      do (setf (aref array1d s) (aref $sound_sample c s)))
                collect ($apply 
                            '$gr2d
                            ($append
                                `((mlist)
                                  ((mequal) $points_joined t)
                                  ((mequal) $point_size 0)
                                  ((mequal) $xrange_secondary ((mlist) 0 ,time))
                                  ((mequal) $xtics_secondary $auto)
                                  ((mequal) $color $blue)
                                  ((mequal) $title ,($sconcat
                                                       "Sound wave. Channel-"
                                                       (1+ c)))  )
                                (get-sound-option '$draw_wave_options)
                                (list
                                   '(mlist simp)
                                   (list '($points) ($listarray array1d))))))))))



;; draw_sound for wxMaxima
(defun $wxdraw_sound ()
   (let* ((num-chn (array-dimension $sound_sample 0))
          (num-sam (array-dimension $sound_sample 1))
          (time (coerce (/ num-sam $sound_sample_rate) 'flonum))
          (array1d (make-array num-sam :element-type 'flonum)))
      (declare (type fixnum num-chn num-sam)
               (type flonum time)
               (type (simple-array flonum *) array1d))
      (sound-user-defaults)
      ($apply
         '$wxdraw
         (cons '(mlist simp)
               (loop for c from 0 below num-chn
                do (loop for s from 0 below num-sam
                      do (setf (aref array1d s) (aref $sound_sample c s)))
                collect ($apply 
                            '$gr2d
                            ($append
                                `((mlist)
                                  ((mequal) $points_joined t)
                                  ((mequal) $point_size 0)
                                  ((mequal) $xrange_secondary ((mlist) 0 ,time))
                                  ((mequal) $xtics_secondary $auto)
                                  ((mequal) $color $blue)
                                  ((mequal) $title ,($sconcat
                                                       "Sound wave. Channel-"
                                                       (1+ c)))  )
                                (loop for x in (get-sound-option '$draw_wave_options) 
                                    unless (or (equal '$terminal (nth 1 x)) 
                                               (equal '$file_name (nth 1 x))) 
                                    collect x)
                                (list
                                   '(mlist simp)
                                   (list '($points) ($listarray array1d))))))))))




(defun $play_sound ()
   (when (and (equal (get-sound-option '$file_format) '$wav)
              (not (equal (get-sound-option '$player) '$none)))
       (let 
          ((str (get-sound-option '$player))
           (res1 nil)
           (res2 nil))
          (cond 
              ((string= *autoconf-windows* "true")
                 (setf res1 ($ssearch ":" str))
                 (setf res2 ($ssearch "\\" str))
                 (if (and res2 (>= res2 1) (not (and res1 (= res1 2))))
                    (setf res1 1)
                    (setf res1 nil)))
              (t 
                 (setf res1 ($ssearch "/" str))
                 (if (and res1 (> res1 1) (not (= res1 1)))
                    (setf res1 1)
                    (setf res1 nil))))
          (setf res2 (get-sound-option '$player_options))
          (if (equal res2 '$none)
             (setf res2 ""))
          (if res1
             ($system (format nil "\"~a~a\" ~a \"~a.wav\""
                            ($first ($directory ($pathname_directory str)))
                            ($sconcat ($pathname_name str)
                                (if (null ($pathname_type str))
                                    ""
                                    ($sconcat "." ($pathname_type str))))
                            res2
                            (plot-temp-file (get-sound-option '$file_name))))
             ($system (format nil "\"~a\" ~a \"~a.wav\""
                            str
                            res2
                            (plot-temp-file (get-sound-option '$file_name))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;;   Envelope functions   ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun pairs-envelope (samp)
   (let* ((n (length samp))
          (args (rest ($args (get-sound-option '$envelope))))
          (d (/ 1.0 n))
          (np 0)
          (counter 0)
          (x 0.0)
          (x1 0.0)
          (x2 0.0)
          (y 0.0)
          (y1 0.0)
          (y2 0.0)
          (m 0.0))
      (declare (type flonum d x x1 x2 y y1 y2 m)
               (type fixnum n np counter))
      ; order with respect to 1st coordinate
      (setf args
            (sort args
                  #'(lambda (p1 p2) (<= (first p1) (first p2)))))

      ; is args a list of pairs in [0, 1]^2 ?
      (when (some #'(lambda (z) (or (< (car z) 0.0)
                                    (> (car z) 1.0)
                                    (< (cadr z) 0.0)
                                    (> (cadr z) 1.0) ))
                  args)
         (merror "sound: arguments in pairs envelope must be in [0, 1]^2"))

      ; add extremes x=0 and y=0 if not already present
      (setf args
            (append (if (= (caar args) 0.0)
                       nil
                       '((0.0 0.0)))
                    args
                    (if (= (caar (last args)) 1.0)
                       nil
                       '((1.0 0.0)) )))

      ; apply envelope joining pairs with linear segments
      (setf np (length args))
      (setf counter 1)
      (dotimes (k n)
         (loop
            (setf x1 (car (nth (1- counter) args)))
            (setf x2 (car (nth counter args)))

            (when (and (<= x1 x)
                       (< x x2))
               (setf y1 (cadr (nth (1- counter) args)))
               (setf y2 (cadr (nth counter args)))
               (setf m (/ (- y2 y1) (- x2 x1)))
               (setf y (+ y1 (* m (- x x1))))
               (setf (aref samp k) (* (aref samp k) y))
               (return))
            (incf counter))
         (setf x (+ x d)))))



(defun function-envelope (samp)
   (let* ((n (length samp))
          (args (rest ($args (get-sound-option '$envelope))))
          (fcn (car args))
          (var (cadr args))
          (lim1 (caddr args))
          (lim2 (cadddr args))
          (d (/ (- lim2 lim1) n))
          (xx lim1)
          (y 0.0))
      (declare (type fixnum n)
               (type flonum lim1 lim2 d xx y))
      (setq fcn (coerce-float-fun (meval `($float ,fcn)) `((mlist) ,var)))
      (flet ((fun (x) (funcall fcn x)))
         (dotimes (k n)
            (setf y (fun xx))
            (setf (aref samp k) (* (aref samp k) y))
            (setf xx (+ xx d))))))



(defun apply-envelope (samp)
   (unless (equal (get-sound-option '$envelope) '$none)
      (let* ((env (get-sound-option '$envelope)))
         (case ($op env)
            ($pairs    (pairs-envelope samp))
            ($function (function-envelope samp)) ))))



;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     ;;
;;   Noise functions   ;;
;;                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;



(defun gaussian-noise (samp)
   (let* ((n (length samp))
          (param (rest (get-sound-option '$noise_generator)))
          (gaussian-sample (rest (mfunction-call $random_normal (first param) (second param) n))) )
      (dotimes (k n)
         (setf (aref samp k) (+ (aref samp k) (nth k gaussian-sample))))))



(defun uniform-noise (samp)
   (let* ((n (length samp))
          (param (rest (get-sound-option '$noise_generator)))
          (uniform-sample (rest (mfunction-call
                                    $random_continuous_uniform
                                    (first param)
                                    (second param)
                                    n))) )
      (dotimes (k n)
         (setf (aref samp k) (+ (aref samp k) (nth k uniform-sample))))))



(defun apply-noise (samp)
   (unless (equal (get-sound-option '$noise_generator) '$none)
      (let* ((noise (get-sound-option '$noise_generator)))
         (case ($op noise)
            ($gaussian (gaussian-noise samp))
            ($uniform  (uniform-noise samp))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          ;;
;;   Oscillator functions   ;;
;;                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun apply-oscillator-model (samp per1-samp ampl)
   (let ((xx 0.0))
      (declare (type flonum xx))
      (case ($op (get-sound-option '$oscillator))
         ($sine
           (let ((harmonics (cdr ($args (get-sound-option '$oscillator))))
                 (d (/ (* 2 pi) per1-samp))
                 (rampl 0.0)
                 (s 0.0)
                 harm)
             (declare (type flonum d rampl s))
             (dotimes (k per1-samp)
               (setf s 0.0)
               (dolist (h harmonics)
                  (cond
                     ((floatp h)  ; user gives only the harmonic number
                         (setf harm h
                               rampl 1.0))
                     (t    ; user gives a list with harmonic number and amplitude fraction
                         (setf harm (cadr h)
                               rampl (caddr h))))
                  (setf s (+ s  (* rampl (sin (* harm xx))))))
               (setf (aref samp k) (coerce (* ampl s) 'flonum))
               (setf xx (+ xx d)))))

         ($rectangle
            (let ((param ($float (cadr ($args (get-sound-option '$oscillator)))))
                  (d (/ 1.0 per1-samp)))
              (declare (type flonum d param))
              (dotimes (k per1-samp)
                (setf (aref samp k)
                      (coerce (if (< xx param) (- ampl) ampl) 'flonum))
                (setf xx (+ xx d))) ))

         ($triangle
            (let ((param ($float (cadr ($args (get-sound-option '$oscillator)))))
                  (d (/ 1.0 per1-samp)))
              (declare (type flonum d param))
              (dotimes (k per1-samp)
                (setf (aref samp k)
                      (coerce
                        (if (< xx param)
                          (- (/ (* 2.0 ampl xx) param) ampl)
                          (- (* 2.0 ampl (- xx 1.0) (/ 1.0 (- param 1.0))) ampl))
                        'flonum))
                (setf xx (+ xx d))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         ;;
;;   Auxiliary functions   ;;
;;                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Calculates the frequency associated to a note, according
;; to the equal tempered scale:
;;
;;                     note - 10
;;               oct + --------- - 1
;;                        12
;;           55 2
;;
;;   freq:
;;   1  = DO   = C
;;   2  = DO#  = C#
;;   3  = RE   = D
;;   4  = RE#  = D#
;;   5  = MI   = E
;;   6  = FA   = F
;;   7  = FA#  = F#
;;   8  = SOL  = G
;;   9  = SOL# = G#
;;   10 = LA   = A
;;   11 = LA#  = A#
;;   12 = SI   = B
(defun $note_freq (note oct)
   (when (or (not (integerp oct))
             (< oct 1)
             (> oct 8))
      (merror "sound (note_freq): octave is not correct"))
   (when (not (stringp note))
      (merror "sound (note_freq): note must be a string"))
   (let ((tone (string-upcase note))
         (freq 13))
      (setf freq (- 13 (length (member tone
                                       '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B")
                                       :test #'string= ))))
      (when (= freq 13)
         (setf freq (- 13 (length (member tone
                                          '("DO" "DO#" "RE" "RE#" "MI" "FA" "FA#"
                                            "SOL" "SOL#" "LA" "LA#" "SI")
                                          :test #'string= )))))
      (when (= freq 13)
         (merror "sound (note_freq): illegal note"))
      `((mtimes) 55
                 ((mexpt) 2
                          ((mplus) -1
                                   ((mtimes) ((rat) 1 12)
                                             ((mplus) -10 ,freq))
                                   ,oct)))))



;;;;;;;;;;;;;;;;;;;;;;;
;;                   ;;
;;   Sound objects   ;;
;;                   ;;
;;;;;;;;;;;;;;;;;;;;;;;



(defun wave (fcn var ini end &rest sample-positions)
   (let* (($numer t)
          ($%enumer t)
          (nsec (- ($float end) ($float ini)))
          (xx ($float ini))
          (d (/ 1.0 $sound_sample_rate))
          (num-samples (round (* nsec $sound_sample_rate)))
          (samples (make-array num-samples
                               :element-type 'flonum))
          (funxx 0.0)
          wave-initials)
      (declare (type flonum nsec xx d)
               (type fixnum num-samples)
               (type (simple-array flonum *) samples))
      (when (null sample-positions)
         (setf sample-positions '(0)))
      (setf wave-initials               
         (map 'list
              #'(lambda (z) (ceiling (* z $sound_sample_rate)))
              (sort (map 'list #'$float sample-positions) #'<)))

      (setq fcn (coerce-float-fun (meval `($float ,fcn)) `((mlist) ,var)))
      (flet ((fun (x) (funcall fcn x)))
         (dotimes (k num-samples)
            (setf funxx (fun xx))
            (setf (aref samples k) (coerce funxx 'flonum))
            (setf xx (+ xx d))))
      (apply-noise samples)
      (apply-envelope samples)
      (make-a-wave
         :sample    samples
         :channel   (get-sound-option '$channel)
         :att-coef  (rest (get-sound-option '$attenuation_coef))
         :repeat-at wave-initials)))



(defun note (freq ampl dur &rest sample-positions)
   (let* (($numer t)
          ($%enumer t)
          (fdur ($float dur))
          (ffreq ($float freq))
          (per1-samples (round (/ $sound_sample_rate ffreq))) ; sample size for one period
          (tot-samples (round (* $sound_sample_rate fdur))) ; total samples for dur seconds
          (samples (make-array tot-samples
                               :initial-element 0.0
                               :element-type 'flonum))
          wave-initials)
      (declare (type boolean $numer $%enumer)
               (type flonum fdur ffreq)
               (type fixnum per1-samples tot-samples)
               (type (simple-array flonum *) samples))
      (when (< (* ffreq fdur) 1)
         (merror "sound (note): frequence times duration must be greater than 1"))
      (when (null sample-positions)
         (setf sample-positions '(0)))
      (setf wave-initials
            (map 'list
                 #'(lambda (z) (ceiling (* z $sound_sample_rate)))
                 (sort (map 'list #'$float sample-positions) #'<)))
      (apply-oscillator-model samples per1-samples ampl)
      ; fill the complete sample repeating the basic period
      (do ((k 0 (1+ k))
           (m per1-samples (1+ m)))
          ((= m tot-samples) 'done)
         (setf (aref samples m)
               (aref samples (mod k per1-samples))))
      (apply-noise samples)
      (apply-envelope samples)
      (make-a-wave
         :sample    samples
         :channel   (get-sound-option '$channel)
         :att-coef  (rest (get-sound-option '$attenuation_coef))
         :repeat-at wave-initials)))



(defun sample-from-list (dat pos)
   (let* ((tot-samples ($length dat))
          (samples (make-array tot-samples
                               :initial-element 0.0
                               :element-type 'flonum))
          wave-initials)
      (declare (type fixnum tot-samples)
               (type (simple-array flonum *) samples))
      ($fillarray samples ($float dat))
      (when (null pos) (setf pos '(0)))
      (setf wave-initials
            (map 'list
                 #'(lambda (z) (ceiling (* z $sound_sample_rate)))
                 (sort (map 'list #'$float pos) #'<)))
      (apply-noise samples)
      (apply-envelope samples)
      (make-a-wave
         :sample    samples
         :channel   (get-sound-option '$channel)
         :att-coef  (rest (get-sound-option '$attenuation_coef))
         :repeat-at wave-initials)))



(defun sample-from-array (dat pos)
   (let ((tot-samples (array-dimension dat 0))
         samples
         wave-initials)
      (declare (type fixnum tot-samples))
      (setf samples (adjust-array (make-array tot-samples :displaced-to dat) tot-samples))
      (when (null pos) (setf pos '(0)))
      (setf wave-initials
            (map 'list
                 #'(lambda (z) (ceiling (* z $sound_sample_rate)))
                 (sort (map 'list #'$float pos) #'<)))
      (apply-noise samples)
      (apply-envelope samples)
      (make-a-wave
         :sample    samples
         :channel   (get-sound-option '$channel)
         :att-coef  (rest (get-sound-option '$attenuation_coef))
         :repeat-at wave-initials)))



(defun sample (data &rest sample-positions)
   (cond
      (($listp data)
          (sample-from-list data sample-positions))
      ((arrayp data)
          (sample-from-array data sample-positions))
      (t
          (merror "sound: unknown format for sampled data"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                      ;;
;;   Play and its auxiliary functions   ;;
;;                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Adds another wave to the complete sample.
;; guest: data array
;; chn: channel number
;; att: attenuation coefficient
;; ini: starting point
(defun add-wave (guest chn att ini)
   (declare (type fixnum chn ini)
            (type flonum att))
   (let ((chn-1 (1- chn))
         (indx 0))
      (declare (type fixnum chn-1 indx))
      (dotimes (n (length guest))
         (setf indx (+ n ini))
         (setf (aref $sound_sample chn-1 indx)
               (+ (aref $sound_sample chn-1 indx)
                  (* att (aref guest n)))))))



(defun sound-normalize ()
   (let ((fa ($float (get-sound-option '$normalize)))
         (max-abs-sample 0.0)
         (num-chn (array-dimension $sound_sample 0))
         (num-sam (array-dimension $sound_sample 1)))
      (declare (type flonum max-abs-sample)
               (type fixnum num-chn num-sam))
      (unless (equal fa '$none)
         (when (equal fa '$auto) (setf fa 32767.0))
         (dotimes (c num-chn)
            (dotimes (s num-sam)
               (let ((value (abs (aref $sound_sample c s))))
                  (when (> value max-abs-sample)
                     (setf max-abs-sample value)))))
         (dotimes (c num-chn)
            (dotimes (s num-sam)
               (setf (aref $sound_sample c s)
                     (coerce (* fa (/ (aref $sound_sample c s) max-abs-sample)) 'flonum)))))))



(defun $play (&rest args)
   (ini-sound-options)
   (sound-user-defaults)
   (let ((wave-storage nil)
         (total-samples 0)
         (num-channels 1)
         (latest-wave-sample 0))
      (declare (type fixnum total-samples num-channels latest-wave-sample))

      ; see what we have to play
      (dolist (x args)
         (cond
            ((equal ($op x) "=")   ; update play option
               (update-sound-option ($lhs x) ($rhs x)))
            (t                     ; create sound
               (case (caar x)
                  ($wave
                     (setf wave-storage
                           (cons (apply #'wave (rest x)) wave-storage)))

                  ($note
                     (setf wave-storage
                           (cons (apply #'note (rest x)) wave-storage)))

                  ($sample
                     (setf wave-storage
                           (cons (apply #'sample (rest x)) wave-storage)))

                  (otherwise
                     (merror "sound: sound object ~M is not recognized" x)))
               ; adjust total number of samples
               (setf latest-wave-sample
                     (+ (car (last (a-wave-repeat-at (first wave-storage))))
                        (length (a-wave-sample (first wave-storage)))))
               (when (< total-samples latest-wave-sample)
                  (setf total-samples latest-wave-sample))
               ; update number of channels
               (setf num-channels 
                     (max num-channels
                          (a-wave-channel (first wave-storage)))))))

      ; now compose the complete wave
      (setf $sound_sample
            (make-array (list num-channels total-samples)
                        :element-type 'flonum
                        :initial-element 0.0))
      (dolist (awave wave-storage)
         (dotimes (k (length (a-wave-repeat-at awave)))
            (let* ((att (a-wave-att-coef awave))
                  (len-1 (1- (length att))))
               (add-wave
                  (a-wave-sample awave)
                  (a-wave-channel awave)
                  (nth (min k len-1) att)
                  (nth k (a-wave-repeat-at awave))))))

      ; normalize sample
      (sound-normalize)

      ; save sound sample
      (save-sound)

      ; draw the wave in case we want to see the waveform
      (when (get-sound-option '$draw_wave)
         ($draw_sound))

      ; call the player in case we want to hear the sound
      ($play_sound)

      '$done))



;; get $draw_wave from user defaults
(defun get-draw-wave-from-user-defaults ()
   (loop for x in *user-sound-default-options*
         unless (not (equal '$draw_wave (nth 1 x))) collect x))

;; get other options from user defaults
(defun get-others-from-user-defaults ()
   (loop for x in *user-sound-default-options*
         unless (equal '$draw_wave (nth 1 x)) collect x))

;; play & draw_sound for wxMaxima
(defun $wxplay (&rest args)
   (cond 
      ((nth 2 (nth 0 (get-draw-wave-from-user-defaults)))
         (setf *user-sound-default-options* 
            (append
               '(((mequal simp) $draw_wave nil))
               (get-others-from-user-defaults)))
         (apply #'$play (nth 0 (list args)))
         (setf *user-sound-default-options* 
            (append
               '(((mequal simp) $draw_wave t))
               (get-others-from-user-defaults))))
      (t
         (apply #'$play (nth 0 (list args)))))
   ; always draw the waveform
   ($wxdraw_sound))
