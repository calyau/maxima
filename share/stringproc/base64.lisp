
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
   
   
**** base64 ********************************************************************
   
   Copyright Volker van Nek, 2013
   
   base64(string) returns a base 64 representation of a string. 

   base64_decode(base64) returns a decoded base 64 string. 
   
   (%i2) base64 : base64("foo bar baz");
   (%o2)                          Zm9vIGJhciBiYXo=
   (%i3) string : base64_decode(base64);
   (%o3)                             foo bar baz
   
   Note that if the string contains umlauts the base64 string is platform 
   dependend. But in every case the decoded string is equal to the original.

|#


(defvar *str64* "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")


;; encode :  0 (6bit) --> base64 = *chr64*[0] = 'A' = 65  : number --> character

(defvar *chr64* (make-array 64. :element-type 'character :initial-element #\0))

(do ((i 0 (1+ i)) (ch (coerce *str64* 'list) (cdr ch)))
    ((= i 64.))
  (setf (char *chr64* i) (car ch)) )


;; decode :  'A' = 65 --> *num64*[65] = 0 (6bit)  : character --> number

;; range of *num64* :  '+' = 43 ... 'z' = 122

(defvar *num64* (make-array 123. :element-type 'integer :initial-element 0))

(do ((i 0 (1+ i)))
    ((= i 64.))
  (setf (svref *num64* (char-code (char *chr64* i))) i) )


(defmfun $base64 (s) 
  (unless (stringp s)
    (merror "`base64': Argument must be a string.") )
  (let* ((bytes (mapcar #'char-code (coerce s 'list)))
         (len (length s))
         (base64 (make-array (* 4. (floor (+ len 2.) 3.)) :element-type 'character :initial-element #\0))
         (k 0) b ind )
    (do ()
        ((null bytes))
      (setq b `#(,(pop bytes)
                 ,(if (null bytes) 0 (pop bytes))
                 ,(if (null bytes) 0 (pop bytes)) ))
      (setq ind `#(
                                                  ,(ash (svref b 0) -2)
        ,(logior (logand (ash (svref b 0) 4) #x30) (ash (svref b 1) -4))
        ,(logior (logand (ash (svref b 1) 2) #x3c) (ash (svref b 2) -6))
                ,(logand      (svref b 2)    #x3f)                                    ))
      (do ((i 0 (1+ i)))
          ((= i 4.))
        (setf (char base64 k) (char *chr64* (svref ind i)))
        (incf k) ))
    (setq len (mod len 3))
    (unless (= len 0) (setf (char base64 (decf k)) #\=))
    (when (= len 1) (setf (char base64 (decf k)) #\=))
    (coerce base64 'string) ))


(defmfun $base64_decode (s) 
  (unless (stringp s)
    (merror "`base64_decode': Argument must be a string.") )
  (let* ((len (length s))
         (nrof= (count-if #'(lambda (c) (char= c #\=)) (subseq s (- len 2.))))
         (size (- (ash (* 3. len) -2.) nrof=))
         (res (make-array size :element-type 'integer :initial-element 0))
         (w (make-array 4. :element-type 'integer :initial-element 0))
         (bytes (mapcar #'char-code (coerce s 'list))) )
    (prog ((j 0))
      a
      (setf (svref w 0) (svref *num64* (pop bytes))
            (svref w 1) (svref *num64* (pop bytes))
            (svref res j) 
              (logior (logand (ash (svref w 0) 2.) #xff) (ash (svref w 1)  -4.)) )
      (when (= (incf j) size) (return))
      
      (setf (svref w 2.) (svref *num64* (pop bytes))
            (svref res j) 
              (logior (logand (ash (svref w 1) 4.) #xff) (ash (svref w 2.) -2.)) )
      
      (when (= (incf j) size) (return))
      
      (setf (svref w 3.) (svref *num64* (pop bytes))
            (svref res j) 
              (logior (logand (ash (svref w 2.) 6.) #xff)     (svref w 3.)     ) )
      (when (= (incf j) size) (return))
      (go a) )
    (coerce (map 'vector #'code-char res) 'string) ))
