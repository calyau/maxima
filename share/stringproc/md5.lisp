
#|
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.
   
   
**** md5sum ********************************************************************
   
   Copyright Volker van Nek, 2013
   
   md5sum(string) returns the md5 checksum of a string. 
   
   The return value is a string to guarantee 32 hex characters. To parse it 
   into an integer please set the input base to 16 and prefix the string by zero.
   
   (%i2) string : md5sum("foo bar baz");
   (%o2)                  ab07acbb1e496801937adfa772424bf7
   (%i3) ibase : obase : 16.$
   
   (%i4) integer : parse_string(sconcat(0, string));
   (%o4)                 0ab07acbb1e496801937adfa772424bf7

|#

(defvar *a5* 0)
(defvar *b5* 0) 
(defvar *c5* 0) 
(defvar *d5* 0)

(defvar *m5* nil)

(defvar *k5* (make-array 64. :element-type 'integer :initial-element 0))

(do ((i 0 (1+ i)))
    ((= i 64.) *k5*)
  (setf (svref *k5* i) (floor (* (abs (sin (1+ i))) #x100000000))) )

(defvar *s5* 
  #( 7. 12. 17. 22.   7. 12. 17. 22.   7. 12. 17. 22.   7. 12. 17. 22.
     5.  9. 14. 20.   5.  9. 14. 20.   5.  9. 14. 20.   5.  9. 14. 20.
     4. 11. 16. 23.   4. 11. 16. 23.   4. 11. 16. 23.   4. 11. 16. 23.
     6. 10. 15. 21.   6. 10. 15. 21.   6. 10. 15. 21.   6. 10. 15. 21. )) 


(declaim (inline md5-not md5+ md5-left-rotation))

(defun md5-not (i32)
  (logand (lognot i32) #xffffffff) )

(defun md5+ (&rest args)
  (logand (apply #'+ args) #xffffffff) )

(defun md5-left-rotation (i32 k)
  (logior (logand (ash i32 k) #xffffffff) (ash i32 (- k 32.))) )


(defun md5-worker ()
  (let ((%a *a5*) (%b *b5*) (%c *c5*) (%d *d5*) 
        %f g tmp hlp )
    (do ((i 0 (1+ i)))
          ((= i 64.))
      (cond
        ((< i 16.)
          (setq %f (logior (logand %b %c) (logand (md5-not %b) %d))
                g i ))
        ((< i 32.)
          (setq %f (logior (logand %b %d) (logand %c (md5-not %d)))
                g (mod (+ (* 5. i) 1) 16.) ))
        ((< i 48.)
          (setq %f (logxor %b %c %d)
                g (mod (+ (* 3. i) 5.) 16.) ))
        (t
          (setq %f (logxor %c (logior %b (md5-not %d)))
                g (mod (* 7. i) 16.) )))
      (setq tmp %d
            %d %c
            %c %b
            hlp (md5+ %a %f (svref *k5* i) (svref *m5* g))
            hlp (md5-left-rotation hlp (svref *s5* i))
            %b (md5+ %b hlp)
            %a tmp ))  
    (setq *a5* (md5+ %a *a5*) *b5* (md5+ %b *b5*) *c5* (md5+ %c *c5*) *d5* (md5+ %d *d5*)) ))


(defun swap-endian64 (i64) ;; little endian <--> big endian
  (let (w)
    (do ((masq #xff (ash masq 8.))
         (sh 0 (- sh 8.)) )
        ((= sh -64.))
      (push (ash (logand i64 masq) sh) w) )
    (nreverse w) ))      

(defun swap-endian32 (i32)
  (logior (ash (logand i32 #xff) 24.)
          (ash (logand i32 #xff00) 8.)
          (ash (logand i32 #xff0000) -8.)
          (ash (logand i32 #xff000000) -24.) ))

(defun md5-hash (w)
  (setq w (mapcar #'swap-endian32 w))
  (logior (ash (first  w) 96.)
          (ash (second w) 64.)
          (ash (third  w) 32.)
               (fourth w)     ))


(defun md5-words (vec)
  (let ((w (make-array 16. :element-type 'list :initial-element nil))
        (inc -1) )
    (do ((i 0 (1+ i)))
        ((= i 16.) w)
      (setf (svref w i) 
        (logior      (svref vec (incf inc))
                (ash (svref vec (incf inc))  8.)
                (ash (svref vec (incf inc)) 16.)
                (ash (svref vec (incf inc)) 24.) )))))


(defun md5-update (bytes)
  (setq *m5* (md5-words (coerce bytes 'vector )))
  (md5-worker) )


(defun md5-final (bytes off len)
  (when bytes (rplacd (last bytes) '(#x80)))
  (when (= 0 off) (setq bytes '(#x80)))
  (if (<= off 55.)
    (let* ((bits (ash len 3))
           (len64 (swap-endian64 bits))
           (pad (make-list (- 55. off) :initial-element 0)) )
      (md5-update (append bytes pad len64)) )
    (let ((pad (make-list (- 63. off) :initial-element 0)))
      (md5-update (append bytes pad))
      (md5-final nil -1 len) )))


(defmfun $md5sum (s)
  (unless (stringp s)
    (merror "`md5sum': Argument must be a string.") )
  (let* ((bytes (mapcar #'char-code (coerce s 'list)))
         (len (length bytes)) )
    (setq *a5* #x67452301
          *b5* #xEFCDAB89 
          *c5* #x98BADCFE 
          *d5* #x10325476 )
    (do ((off len)) 
        ((< off 64.) (md5-final bytes off len))
      (setq off (- off 64.))
      (md5-update (butlast bytes off))
      (setq bytes (last bytes off)) )
    (format nil "~32,'0x" (md5-hash (list *a5* *b5* *c5* *d5*))) ))

