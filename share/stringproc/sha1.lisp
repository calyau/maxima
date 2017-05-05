
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
   
   
**** sha1, sha256, mgf1_sha1 ***************************************************
   
   Copyright Volker van Nek, 2014 - 2015
   
   sha1sum returns the sha1 fingerprint of a string, a non-negative integer or 
   a list of octets. 
   
   sha256sum returns the sha256 fingerprint.
   
   The default return value is a string that guarantees 40 (64) hex characters. 
   An optional argument allows sha1sum and sha256sum to return the corresponding 
   number or list of octets.
   
   (%i1) ibase: obase: 16.$
   (%i2) msg: "foo bar baz"$
   (%i3) string: sha1sum(msg);
   (%o3)              c7567e8b39e2428e38bf9c9226ac68de4c67dc39
   (%i4) integer: sha1sum(msg, 'number);
   (%o4)             0c7567e8b39e2428e38bf9c9226ac68de4c67dc39
   (%i5) octets: sha1sum(msg, 'list);
   (%o5)  [0C7,56,7E,8B,39,0E2,42,8E,38,0BF,9C,92,26,0AC,68,0DE,4C,67,0DC,39]
   (%i6) sdowncase( printf(false, "~{~2,'0x~^:~}", octets) );
   (%o6)     c7:56:7e:8b:39:e2:42:8e:38:bf:9c:92:26:ac:68:de:4c:67:dc:39

   Note that in case the string contains German umlauts or other non-ASCII 
   characters the fingerprint is platform dependend.
   
   The following code streams the base64 contents of a X509 certificate into a 
   string, decodes base64 to DER format and returns the SHA1 fingerprint. 
   The result is checked against openssl in GNU/Linux.
   
   (%i1) ostream : make_string_output_stream();
   (%o1)                  #<string-output stream 00f065e8>
   (%i2) fos : openr("/home/volker/Deutsche_Telekom_Root_CA_2.crt");
   (%o2)        #<input stream /home/volker/Deutsche_Telekom_Root_CA_2.crt>
   (%i3) while (z : readline(fos)) # false do 
   if not cequal(charat(z, 1), "-") then printf(ostream, "~a", z);
   (%o3)                                done
   (%i4) close(fos);
   (%o4)                                true
   (%i5) string : get_output_stream_string(ostream);
   (%o5) MIIDnzCCAoegAwIBAgIBJjANBgkqhkiG9w0BAQUFADBxMQswCQYDVQQGEwJERTEcMBoGA1UE\
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
   (%i6) close(ostream);
   (%o6)                                true
   (%i7) sha1sum(base64_decode(string));
   (%o7)              85a408c09c193e5d51587dcdd61330fd8cde37bf
   (%i8) system("openssl x509 -fingerprint -noout -in '/home/volker/Deutsche_Telekom_Root_CA_2.crt' > temp ; cat temp");
   SHA1 Fingerprint=85:A4:08:C0:9C:19:3E:5D:51:58:7D:CD:D6:13:30:FD:8C:DE:37:BF
   (%o8)                                  0


   mgf1_sha1(seed, length) resp. mgf1_sha1(seed, length, return_type) returns 
   a pseudo random number of octet-length length.
   The returned value is a number (default) or a list of octets.
   See RFC 3447,appendix B.2.1.

   (%i1) ibase: obase: 16.$
   (%i2) number: mgf1_sha1(0cafe, 8);
   (%o2)                          0b097d3c8328001ee
   (%i3) octets: mgf1_sha1(0cafe, 8, 'list);
   (%o3)                    [0B0,97,0D3,0C8,32,80,1,0EE]

|#

(in-package :maxima)

(eval-when
  #+gcl (compile eval)
  #-gcl (:compile-toplevel :execute)
    (defvar old-ibase-sha1 *read-base*)
    (setq *read-base* 10.) )


(declaim (inline sha-not sha+ sha-left-rotation sha-right-rotation))

(defun sha-not (i32)
  (logand (lognot i32) #xffffffff) )

(defun sha+ (&rest args)
  (logand (apply #'+ args) #xffffffff) )

(defun sha-left-rotation (i32 k)
  (logior (logand (ash i32 k) #xffffffff) (ash i32 (- k 32.))) )

(defun sha-right-rotation (i32 k)
  (logior (ash i32 (- k)) (logand (ash i32 (- 32. k)) #xffffffff)) )


(defun sha-update (bytes nr)
  (setq bytes (coerce bytes 'vector))
  (cond
    ((= nr 160.)
      (sha1-words bytes)
      (sha1-worker) )
    ((= nr 256.)
      (sha256-words bytes)
      (sha256-worker) )))

(defun sha-len64 (bits)
  (do ((i 1 (1+ i)) lst) (nil)
    (push (logand bits #xff) lst)
    (when (= i 8) (return lst))
    (setq bits (ash bits -8)) ))

(defun sha-final (bytes off len nr)
  (when bytes (setq bytes (append bytes '(#x80)))) ;; don't modify bytes
  (when (= 0 off) (setq bytes '(#x80)))
  (if (<= off 55.)
    (let* ((bits (ash len 3))
           (len64 (sha-len64 bits))
           (pad (make-list (- 55. off) :initial-element 0)) )
      (sha-update (append bytes pad len64) nr) )
    (let ((pad (make-list (- 63. off) :initial-element 0)))
      (sha-update (append bytes pad) nr)
      (sha-final nil -1 len nr) )))


;; *** SHA1 ***************************************************************** ;;

(defvar *h1* nil)
(defvar *w1* nil)

(defun sha1-worker ()
  (multiple-value-bind (a b c d e) (apply #'values *h1*)
    (let (f k tmp)
      (do ((i 0 (1+ i)))
          ((= i 80.))
        (cond
          ((< i 20.)
            (setq f (logior (logand b c) (logand (sha-not b) d))
                  k #x5a827999 ))
          ((< i 40.)
            (setq f (logxor b c d)
                  k #x6ed9eba1 ))
          ((< i 60.)
            (setq f (logior (logand b c) (logand b d) (logand c d))
                  k #x8f1bbcdc ))
          (t
            (setq f (logxor b c d)
                  k #xca62c1d6 )))
        (setq tmp (sha+ (sha-left-rotation a 5) f e k (svref *w1* i))
              e d
              d c
              c (sha-left-rotation b 30.)
              b a
              a tmp ) )
      (setq *h1* (mapcar #'sha+ (list a b c d e) *h1*)) )))

(defun sha1-words (vec)
  (setq *w1* (make-array 80. :element-type 'integer :initial-element 0))
  ;; copy 512 bit message into 32 bit big-endian words:
  (do ((i 0 (1+ i)) (inc -1))
      ((= i 16.))
    (setf (svref *w1* i) 
      (logior (ash (svref vec (incf inc)) 24.)
              (ash (svref vec (incf inc)) 16.)
              (ash (svref vec (incf inc))  8.)
                   (svref vec (incf inc))     )))
  ;; expand:
  (do ((i 16. (1+ i)))
      ((= i 80.))
    (setf (svref *w1* i) 
      (sha-left-rotation 
        (logxor (svref *w1* (- i 3.))
                (svref *w1* (- i 8.))
                (svref *w1* (- i 14.))
                (svref *w1* (- i 16.)) )
        1 ))))

(defmfun $sha1sum (s &optional (rtype '$string))
  (let (bytes len)
    (cond
      ((stringp s)
        (setq bytes (string-to-octets s)) )
      ((and (integerp s) (>= s 0))
        (setq bytes (number-to-octets s)) )
      (($listp s)
        (setq bytes (cdr s)) )
      (t 
        (gf-merror (intl:gettext 
          "`sha1sum': Argument must be a string, a non-negative integer or a list of octets." ))))
    (setq len (length bytes) 
          *h1* '(#x67452301 #xefcdab89 #x98badcfe #x10325476 #xc3d2e1f0) )
    (do ((off len)) 
        ((< off 64.) (sha-final bytes off len 160.))
      (setq off (- off 64.))
      (sha-update (butlast bytes off) 160.)
      (setq bytes (last bytes off)) )
    (cond
      ((equal rtype '$list)
        (cons '(mlist simp)
          (reduce #'nconc (mapcar #'word-to-octets *h1*)) ))
      ((equal rtype '$number)
        (reduce #'(lambda (x y) (logior (ash x 32.) y)) *h1*) )
      ((equal rtype '$string)
        (nstring-downcase (format nil "~{~8,'0x~}" *h1*)) )
      (t  
        (gf-merror (intl:gettext 
          "`sha1sum': Optional argument must be 'list, 'number or 'string." ))))))


;; *** MGF1_SHA1 ************************************************************ ;;
;;
;; Generation of a pseudorandom number according to RFC 3447, appendix B.2.1 .
;;
(defmfun $mgf1_sha1 (seed len &optional (rtype '$number))
  (let ((err-msg (intl:gettext "Unsuitable arguments to `mgf1_sha1': ~m, ~m"))
         s s+i res )
    (cond
      ((and (integerp seed) (>= seed 0)) (setq s (number-to-octets seed)))
      (($listp seed) (setq s (cdr seed)))
      (t (gf-merror err-msg seed len)) )
    (unless (and (integerp len) (> len 0)) (gf-merror err-msg seed len))
    (do ((i 0 (1+ i))
         (ii (ceiling (/ len 20.0))))
        ((= i ii))
      (setq s+i (cons '(mlist simp) (append s (word-to-octets i)))
            res (nconc res (cdr ($sha1sum s+i '$list))) ))
    (setq res (subseq res 0 len))
    (cond
      ((equal rtype '$number) (octets-to-number res))
      ((equal rtype '$list) (cons '(mlist simp) res))
      (t (gf-merror (intl:gettext 
           "`mgf1_sha1': Optional argument must be 'number or 'list." ))))))


;; *** SHA256 *************************************************************** ;;

(defvar *k2* 
  (coerce               ;; the first 32 bits of the fractional parts of ..
    (mapcar #'(lambda (i) (floor (* (rem (expt (coerce i 'double-float) 1/3) 1.0) #x100000000))) 
            (subseq *small-primes* 0 64.) ) ;; .. the cube roots of the first 64 primes (2,..,311)
    'vector ))

(defvar *h2* nil)
(defvar *w2* nil)

(defun sha256-worker ()
  (multiple-value-bind (a b c d e f g h) (apply #'values *h2*)
    (let (s1 t1 s2 t2)
      (do ((i 0 (1+ i)))
          ((= i 64.))
        (setq s1 (logxor (sha-right-rotation e 6)
                         (sha-right-rotation e 11.)
                         (sha-right-rotation e 25.) )
              t1 (logxor (logand e f) (logand (sha-not e) g))
              t1 (sha+ h s1 t1 (svref *k2* i) (svref *w2* i))
              s2 (logxor (sha-right-rotation a 2)
                         (sha-right-rotation a 13.)
                         (sha-right-rotation a 22.) )
              t2 (logxor (logand a b) (logand a c) (logand b c))
              t2 (sha+ s2 t2) )
        (setq h g
              g f
              f e
              e (sha+ d t1)
              d c
              c b
              b a
              a (sha+ t1 t2) ))
      (setq *h2* (mapcar #'sha+ (list a b c d e f g h) *h2*)) )))

(defun sha256-words (vec)
  (setq *w2* (make-array 64. :element-type 'integer :initial-element 0))
  ;; copy 512 bit message into 32 bit big-endian words:
  (do ((i 0 (1+ i)) (inc -1))
      ((= i 16.))
    (setf (svref *w2* i) 
      (logior (ash (svref vec (incf inc)) 24.)
              (ash (svref vec (incf inc)) 16.)
              (ash (svref vec (incf inc))  8.)
                   (svref vec (incf inc))     )))
  ;; expand:
  (do ((i 16. (1+ i)) w s0 s1)
      ((= i 64.))
    (setq w (svref *w2* (- i 15.))
          s0 (logxor (sha-right-rotation w 7)
                     (sha-right-rotation w 18.)
                     (ash w -3) )
          w (svref *w2* (- i 2))
          s1 (logxor (sha-right-rotation w 17.)
                     (sha-right-rotation w 19.)
                     (ash w -10.) ))
    (setf (svref *w2* i) 
      (sha+ (svref *w2* (- i 16.)) s0 (svref *w2* (- i 7)) s1) )))

(defmfun $sha256sum (s &optional (rtype '$string))
  (let (bytes len)
    (cond
      ((stringp s)
        (setq bytes (string-to-octets s)) )
      ((and (integerp s) (>= s 0))
        (setq bytes (number-to-octets s)) )
      (($listp s)
        (setq bytes (cdr s)) )
      (t 
        (gf-merror (intl:gettext 
          "`sha256sum': Argument must be a string, a non-negative integer or a list of octets." ))))
    (setq len (length bytes) 
          *h2* ;; the first 32 bits of the fractional parts of the square roots of the first 8 primes (2,..,19)
            '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19) )
    (do ((off len)) 
        ((< off 64.) (sha-final bytes off len 256.))
      (setq off (- off 64.))
      (sha-update (butlast bytes off) 256.)
      (setq bytes (last bytes off)) )
    (cond
      ((equal rtype '$list)
        (cons '(mlist simp)
          (reduce #'nconc (mapcar #'word-to-octets *h2*)) ))
      ((equal rtype '$number)
        (reduce #'(lambda (x y) (logior (ash x 32.) y)) *h2*) )
      ((equal rtype '$string)
        (nstring-downcase (format nil "~{~8,'0x~}" *h2*)) )
      (t  
        (gf-merror (intl:gettext 
          "`sha256sum': Optional argument must be 'list, 'number or 'string." ))))))


(eval-when
  #+gcl (compile eval)
  #-gcl (:compile-toplevel :execute)
    (setq *read-base* old-ibase-sha1) )
