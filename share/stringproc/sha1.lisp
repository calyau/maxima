
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
   
   
**** sha1 **********************************************************************
   
   Copyright Volker van Nek, 2014
   
   sha1sum(string) returns the sha1 fingerprint of a string. 
   
   The return value is a string to guarantee 48 hex characters. To parse it 
   into an integer please set the input base to 16 and prefix the string by zero.
   
   (%i2) string : sha1sum("foo bar baz");
   (%o2)              c7567e8b39e2428e38bf9c9226ac68de4c67dc39
   (%i3) ibase : obase : 16.$
   
   (%i4) integer : parse_string(sconcat(0, string));
   (%o4)              0c7567e8b39e2428e38bf9c9226ac68de4c67dc39

   Note that in case the string contains German umlauts or other non-ASCII 
   characters the sha1 fingerprint is platform dependend.
   
   The following code streams the base64 contents of a X509 certificate into a 
   string, decodes base64 to DER format and returns the SHA1 fingerprint. 
   The result is checked against openssl in GNU/Linux.
   
   (%i2) ostream : make_string_output_stream();
   (%o2)                  #<string-output stream 00f065e8>
   (%i3) fos : openr("/home/volker/Deutsche_Telekom_Root_CA_2.crt");
   (%o3)        #<input stream /home/volker/Deutsche_Telekom_Root_CA_2.crt>
   (%i4) while (z : readline(fos)) # false do 
   if not cequal(charat(z, 1), "-") then printf(ostream, "~a", z);
   (%o4)                                done
   (%i5) close(fos);
   (%o5)                                true
   (%i6) string : get_output_stream_string(ostream);
   (%o6) MIIDnzCCAoegAwIBAgIBJjANBgkqhkiG9w0BAQUFADBxMQswCQYDVQQGEwJERTEcMBoGA1UE\
   ChMTRGV1dHNjaGUgVGVsZWtvbSBBRzEfMB0GA1UECxMWVC1UZWxlU2VjIFRydXN0IENlbnRlcjEjMC\
   EGA1UEAxMaRGV1dHNjaGUgVGVsZWtvbSBSb290IENBIDIwHhcNOTkwNzA5MTIxMTAwWhcNMTkwNzA5\
   MjM1OTAwWjBxMQswCQYDVQQGEwJERTEcMBoGA1UEChMTRGV1dHNjaGUgVGVsZWtvbSBBRzEfMB0GA1\
   UECxMWVC1UZWxlU2VjIFRydXN0IENlbnRlcjEjMCEGA1UEAxMaRGV1dHNjaGUgVGVsZWtvbSBSb290\
   IENBIDIwggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCrC6M14IspFLEUha88EOQ5bzVdSq\
   7d6mGNlUn0b2SjGmBmpKlAIoTZ1KXleJMOaAGtuU1cOs7TuKhCQN/Po7qCWWqSG6wcmtoIKyUn+Wkj\
   R/Hg6yx6m/UTAtB+NHzCnjwAWav12gz1MjwrrFDa1sPeg5TKqAyZMg4ISFZbavva4VhYAUlfckE8FQ\
   YBjl2tqriTtM2e66foai1SNNs671x1Udrb8zH57nGYMsRUFUQM+ZtV7a3fGAigo4aKSe5TBY8ZTNXe\
   WHmb0mocQqvF1afPaA+W5OFhmHZhyJF81j4A4pFQh+GdCuatl9Idxjp9y7zaAzTVjlsB9WoHtxa2bk\
   p/AgMBAAGjQjBAMB0GA1UdDgQWBBQxw3kbuvVT1xfgiXotF2wKsyudMzAPBgNVHRMECDAGAQH/AgEF\
   MA4GA1UdDwEB/wQEAwIBBjANBgkqhkiG9w0BAQUFAAOCAQEAlGRZrTlk5ynrE/5aw4sTV8gEJPB0d8\
   Bg42f76Ymmg7+Wgnxu1MM9756AbrsptJh6sTtU6zkXR34ajgv8HzFZMQSyzhfzLMdiNlXiItiJVbSY\
   SKpk+tYcNthEeFpaIzpXl/V6ME+un2pMSyuOoAPjPuCp1NJ70rOo4nI8rZ7/gFnkm0W09juwzTkZmD\
   Ll6iFhkOQxIY40sfcvNUqFENrnijchvllj4PKFiDFT1FQUhXB59C4Gdyd1Lx+4ivn+xbrYNuSD7Odl\
   t79jWvNGr4GUN9RBjNYj1h7P9WgbRGOiWrqnNVmh5XAFmw4jV5mUCm26OWMohpLzGITY+9HPBVZkVw\
   ==
   (%i7) close(ostream);
   (%o7)                                true
   (%i8) sha1sum(base64_decode(string));
   (%o8)              85a408c09c193e5d51587dcdd61330fd8cde37bf
   (%i9) system("openssl x509 -fingerprint -noout -in '/home/volker/Deutsche_Telekom_Root_CA_2.crt' > temp ; cat temp");
   SHA1 Fingerprint=85:A4:08:C0:9C:19:3E:5D:51:58:7D:CD:D6:13:30:FD:8C:DE:37:BF
   (%o9)                                  0

|#

(defvar *a1* 0)
(defvar *b1* 0) 
(defvar *c1* 0) 
(defvar *d1* 0)
(defvar *e1* 0)

(defvar *m1* nil)


(declaim (inline sha1-not sha1+ sha1-left-rotation))

(defun sha1-not (i32)
  (logand (lognot i32) #xffffffff) )

(defun sha1+ (&rest args)
  (logand (apply #'+ args) #xffffffff) )

(defun sha1-left-rotation (i32 k)
  (logior (logand (ash i32 k) #xffffffff) (ash i32 (- k 32.))) )


(defun sha1-worker ()
  (let ((*a* *a1*) (*b* *b1*) (*c* *c1*) (*d* *d1*) (*e* *e1*)
         *f* k tmp )
    (do ((i 0 (1+ i)))
        ((= i 80.))
      (cond
        ((< i 20.)
          (setq *f* (logior (logand *b* *c*) (logand (sha1-not *b*) *d*))
                k #x5A827999 ))
        ((< i 40.)
          (setq *f* (logxor *b* *c* *d*)
                k #x6ED9EBA1 ))
        ((< i 60.)
          (setq *f* (logior (logand *b* *c*) (logand *b* *d*) (logand *c* *d*))
                k #x8F1BBCDC ))
        (t
          (setq *f* (logxor *b* *c* *d*)
                k #xCA62C1D6 )))
      (setq tmp (sha1+ (sha1-left-rotation *a* 5) *f* *e* k (svref *m1* i))
            *e* *d*
            *d* *c*
            *c* (sha1-left-rotation *b* 30.)
            *b* *a*
            *a* tmp ) )
    (setq *a1* (sha1+ *a1* *a*)
          *b1* (sha1+ *b1* *b*)
          *c1* (sha1+ *c1* *c*)
          *d1* (sha1+ *d1* *d*)
          *e1* (sha1+ *e1* *e*) )))


(defun sha1-words (vec) ;; 32 bit big-endian
  (let ((w (make-array 16. :element-type 'integer :initial-element 0))
        (inc -1) )
    (do ((i 0 (1+ i)))
        ((= i 16.) w)
      (setf (svref w i) 
        (logior (ash (svref vec (incf inc)) 24.)
                (ash (svref vec (incf inc)) 16.)
                (ash (svref vec (incf inc))  8.)
                     (svref vec (incf inc))     )))))

(defun sha1-expand (words)
  (setq *m1* (make-array 80. :element-type 'integer :initial-element 0))
  (do ((i 0 (1+ i)))
      ((= i 16.))
    (setf (svref *m1* i) (svref words i)) )
  (do ((i 16. (1+ i)))
      ((= i 80.))
    (setf (svref *m1* i) 
      (sha1-left-rotation 
        (logxor (svref *m1* (- i 3.))
                (svref *m1* (- i 8.))
                (svref *m1* (- i 14.))
                (svref *m1* (- i 16.)) )
        1 ))))

(defun sha1-update (bytes)
  (sha1-expand (sha1-words (coerce bytes 'vector )))
  (sha1-worker) )


(defun sha1-final (bytes off len)
  (when bytes (rplacd (last bytes) '(#x80)))
  (when (= 0 off) (setq bytes '(#x80)))
  (if (<= off 55.)
    (let* ((bits (ash len 3))
           (len64 (list 0 0 0 0 0 0 (ash bits -8.) (logand bits #xff)))
           (pad (make-list (- 55. off) :initial-element 0)) )
      (sha1-update (append bytes pad len64)) )
    (let ((pad (make-list (- 63. off) :initial-element 0)))
      (sha1-update (append bytes pad))
      (sha1-final nil -1 len) )))


(defun sha1-hash (w)
  (logior (ash (first  w) 128.)
          (ash (second w)  96.)
          (ash (third  w)  64.)
          (ash (fourth w)  32.)
               (fifth  w)      ))


(defmfun $sha1sum (s)
  (unless (stringp s)
    (merror "`sha1': Argument must be a string.") )
  (let* ((bytes (mapcar #'char-code (coerce s 'list)))
         (len (length bytes)) )
    (setq *a1* #x67452301
          *b1* #xEFCDAB89 
          *c1* #x98BADCFE 
          *d1* #x10325476
          *e1* #xC3D2E1F0 )
    (do ((off len)) 
        ((< off 64.) (sha1-final bytes off len))
      (setq off (- off 64.))
      (sha1-update (butlast bytes off))
      (setq bytes (last bytes off)) )
    (format nil "~40,'0x" (sha1-hash (list *a1* *b1* *c1* *d1* *e1*))) ))

