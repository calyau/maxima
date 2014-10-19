
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

   Note that in case the string contains German umlauts or other non-ASCII 
   characters the md5 checksum is platform dependend.
   
   To check the md5sum of a *small* file the file can be streamed into a string.

   (%i2) ostream : make_string_output_stream();
   (%o2)                  #<string-output stream 00f06ae8>
   (%i3) fos : openr("/home/volker/pub_key_temp.pem");
   (%o3)            #<input stream /home/volker/pub_key_temp.pem>
   (%i4) while (c : readchar(fos)) # false do printf(ostream, "~a", c);
   (%o4)                                done
   (%i5) close(fos);
   (%o5)                                true
   (%i6) string : get_output_stream_string(ostream);
   (%o6) -----BEGIN PUBLIC KEY-----
   MIIBCgKCAQEA7CCxZOFAoZ7khi0TiwIxU8cHEZJnIQb96ONrPbSqq/s3CVwU1eLH
   9QEaZb8viFhe6/Db66DjR6RqCO3uIfx2siAb8SaTo0PYZz8JQ5IenjBDJAGA56gE
   6OX8JadgPCLEZTdJ2Q0axqPHwoWsZkn56Pt4UlJfUcW7cNPNIihgy2DwE1PpbHCY
   IdhYcT/iYA6C+TiYdYNcAFUsQyGExBxOTOXrMGFknjALedkLoq9IN3Djnw4kxGYv
   vl3hVYBJpixusUgOK5LhYwowQayeczPoMA0Ef5KAZwJY9lUZ2UYBKMqdoNpdJuDz
   q4QOxlkqUvZxWTEHqNmlfX4/2w71ZiAqpwIDAQAB
   -----END PUBLIC KEY-----
   (%i7) close(ostream);
   (%o7)                                true
   (%i8) md5sum(string);
   (%o8)                  b5f2033ccb6f4066874aa5a2308bd561
   
   The result is checked against openssl in GNU/Linux.
   
   (%i9) system("openssl dgst -md5 '/home/volker/pub_key_temp.pem' > temp ; cat temp");
   MD5(/home/volker/pub_key_temp.pem)= b5f2033ccb6f4066874aa5a2308bd561
   (%o9)                                  0

|#

(eval-when
  #+gcl (compile eval)
  #-gcl (:compile-toplevel :execute)
    (defvar old-ibase *read-base*)
    (setq *read-base* 10.) )


(defvar *h5* nil)
(defvar *w5* nil)

(defvar *k5* (make-array 64. :element-type 'integer :initial-element 0))
(do ((i 0 (1+ i)))
    ((= i 64.))
  (setf (svref *k5* i) (floor (* (abs (sin (+ i 1.0))) #x100000000))) )

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
  (multiple-value-bind (a b c d) (apply #'values *h5*)
    (let (f g tmp hlp)
      (do ((i 0 (1+ i)))
          ((= i 64.))
        (cond
          ((< i 16.)
            (setq f (logior (logand b c) (logand (md5-not b) d))
                  g i ))
          ((< i 32.)
            (setq f (logior (logand b d) (logand c (md5-not d)))
                  g (logand (+ (* 5 i) 1) #xf) ))
          ((< i 48.)
            (setq f (logxor b c d)
                  g (logand (+ (* 3 i) 5) #xf) ))
          (t
            (setq f (logxor c (logior b (md5-not d)))
                  g (logand (* 7 i) #xf) )))
        (setq tmp d
              d c
              c b
              hlp (md5+ a f (svref *k5* i) (svref *w5* g))
              hlp (md5-left-rotation hlp (svref *s5* i))
              b (md5+ b hlp)
              a tmp ))  
      (setq *h5* (mapcar #'md5+ (list a b c d) *h5*)) )))


(defun swap-endian64 (i64) ;; little-endian <--> big-endian
  (do ((masq #xff (ash masq 8))
       (sh 0 (- sh 8)) w )
      ((= sh -64.) (nreverse w))
    (push (ash (logand i64 masq) sh) w) ))

(defun swap-endian32 (i32)
  (logior (ash (logand i32 #xff) 24.)
          (ash (logand i32 #xff00) 8.)
          (ash (logand i32 #xff0000) -8.)
          (ash (logand i32 #xff000000) -24.) ))


(defun md5-words (vec) ;; 32 bit little-endian
  (let ((w (make-array 16. :element-type 'integer :initial-element 0))
        (inc -1) )
    (do ((i 0 (1+ i)))
        ((= i 16.) w)
      (setf (svref w i) 
        (logior      (svref vec (incf inc))
                (ash (svref vec (incf inc))  8.)
                (ash (svref vec (incf inc)) 16.)
                (ash (svref vec (incf inc)) 24.) )))))

(defun md5-update (bytes)
  (setq *w5* (md5-words (coerce bytes 'vector )))
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
    (setq *h5* '(#x67452301 #xefcdab89 #x98badcfe #x10325476))
    (do ((off len)) 
        ((< off 64.) (md5-final bytes off len))
      (setq off (- off 64.))
      (md5-update (butlast bytes off))
      (setq bytes (last bytes off)) )
    (nstring-downcase (format nil "~{~8,'0x~}" (mapcar #'swap-endian32 *h5*))) ))


(eval-when
  #+gcl (compile eval)
  #-gcl (:compile-toplevel :execute)
    (setq *read-base* old-ibase) )
