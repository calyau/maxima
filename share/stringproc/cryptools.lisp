
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
   
   
**** tools for cryptography ****************************************************
   
   Copyright Volker van Nek, 2015
   
   1. string_to_octets(string [,encoding]) and octets_to_string(octets [,encoding]).
   The default encoding depends on the underlying Lisp, the platform and the 
   application. The following example shows Maxima/GCL in a GNU/Linux terminal.
   GCL uses no format definition and simply passes through the utf-8-octets encoded 
   by the terminal.

   (%i1) octets: string_to_octets("abc");
   (%o1)                            [61, 62, 63]
   (%i2) octets_to_string(octets);
   (%o2)                                 abc
   (%i3) ibase : obase : 16.$
   (%i4) string: unicode(3bb);
   (%o4)                                 λ
   (%i5) octets: string_to_octets(string);
   (%o5)                             [0CE, 0BB]
   (%i6) octets_to_string(octets);
   (%o6)                                 λ
   (%i7) utf8_to_unicode(octets);
   (%o7)                                 3BB
   
   Maxima/SBCL in a GNU/Linux terminal using the optional encoding argument.
   
   (%i1) ibase : obase : 16.$
   (%i2) string: simplode(map(unicode, [3b1,3b2,3b3]));
   (%o2)                                 αβγ
   (%i3) octets: string_to_octets(string, "ucs-2be");
   (%o3)                      [3, 0B1, 3, 0B2, 3, 0B3]
   (%i4) octets_to_string(octets, "ucs-2be");
   (%o4)                                 αβγ
   (%i5) octets: string_to_octets(string, "iso-8859-7");
   (%o5)                           [0E1, 0E2, 0E3]
   (%i6) octets_to_string(octets, "iso-8859-7");
   (%o6)                                 αβγ
         
   Examples of supported encodings:
   CCL,CLISP,SBCL: utf-8, ucs-2be, iso-8859-1, cp1252, cp850
   CMUCL: utf-8, utf-16-be, iso8859-1, cp1252
   ECL: utf-8, ucs-2be, iso-8859-1, windows-cp1252, dos-cp850
   
   2. number_to_octets(number) and octets_to_number(octets).

   (%i1) ibase : obase : 16.$
   (%i2) octets: [0ca,0fe,0ba,0be]$
   (%i3) number: octets_to_number(octets);
   (%o3)                            0cafebabe
   (%i4) number_to_octets(number);
   (%o4)                      [0CA, 0FE, 0BA, 0BE]

   
   3. octets_to_oid(octets) and oid_to_octets(string) compute object identifiers 
   (OIDs) from lists of octets and convert OIDs back to octets.
   
   (%i1) ibase : obase : 16.$
   (%i2) oid: octets_to_oid([2A,86,48,86,0F7,0D,1,1,1]);
   (%o2)                      1.2.840.113549.1.1.1
   (%i3) oid_to_octets(oid);
   (%o3)               [2A, 86, 48, 86, 0F7, 0D, 1, 1, 1]

   
   4. crc24sum(octets) returns the crc24 checksum of an octet-list. The returned 
   value is a string (default), a number or a list of octets.
   
   Example: OpenPGP uses crc24
   
      -----BEGIN PGP SIGNATURE-----
   Version: GnuPG v2.0.22 (GNU/Linux)
   
   iQEcBAEBAgAGBQJVdCTzAAoJEG/1Mgf2DWAqCSYH/AhVFwhu1D89C3/QFcgVvZTM
   wnOYzBUURJAL/cT+IngkLEpp3hEbREcugWp+Tm6aw3R4CdJ7G3FLxExBH/5KnDHi
   rBQu+I7+3ySK2hpryQ6Wx5J9uZSa4YmfsNteR8up0zGkaulJeWkS4pjiRM+auWVe
   vajlKZCIK52P080DG7Q2dpshh4fgTeNwqCuCiBhQ73t8g1IaLdhDN6EzJVjGIzam
   /spqT/sTo6sw8yDOJjvU+Qvn6/mSMjC/YxjhRMaQt9EMrR1AZ4ukBF5uG1S7mXOH
   WdiwkSPZ3gnIBhM9SuC076gLWZUNs6NqTeE3UzMjDAFhH3jYk1T7mysCvdtIkms=
   =WmeC
   -----END PGP SIGNATURE-----
   
   (%i1) ibase : obase : 16.$
   (%i2) sig64 : sconcat(
     "iQEcBAEBAgAGBQJVdCTzAAoJEG/1Mgf2DWAqCSYH/AhVFwhu1D89C3/QFcgVvZTM",
     "wnOYzBUURJAL/cT+IngkLEpp3hEbREcugWp+Tm6aw3R4CdJ7G3FLxExBH/5KnDHi",
     "rBQu+I7+3ySK2hpryQ6Wx5J9uZSa4YmfsNteR8up0zGkaulJeWkS4pjiRM+auWVe",
     "vajlKZCIK52P080DG7Q2dpshh4fgTeNwqCuCiBhQ73t8g1IaLdhDN6EzJVjGIzam",
     "/spqT/sTo6sw8yDOJjvU+Qvn6/mSMjC/YxjhRMaQt9EMrR1AZ4ukBF5uG1S7mXOH",
     "WdiwkSPZ3gnIBhM9SuC076gLWZUNs6NqTeE3UzMjDAFhH3jYk1T7mysCvdtIkms=" )$
   (%i3) octets: base64_decode(sig64, 'list)$
   (%i4) crc24: crc24sum(octets, 'list);
   (%o4)                          [5A, 67, 82]
   (%i5) base64(crc24);
   (%o5)                              WmeC

|#

(in-package :maxima)

(eval-when
  #+gcl (compile eval)
  #-gcl (:compile-toplevel :execute)
    (defvar old-ibase-cryptools *read-base*)
    (setq *read-base* 10.) )


;; -- string-octet-conversions: --------------------------------------------- ;;

(defun $string_to_octets (str &optional enc) 
  (unless (stringp str) 
    (gf-merror (intl:gettext "`string_to_octets': argument must be a string.")) )
    (cons '(mlist simp) (string-to-octets str enc)) )

(defun string-to-octets (str &optional enc) 
  (setq enc (get-encoding enc "string_to_octets"))
  (let ((ov #+ccl (ccl:encode-string-to-octets str :external-format enc) ;; maybe these (CCL, ECL) .. 
            #+clisp (ext:convert-string-to-bytes str enc)
            #+ecl (ecl-string-to-octets str enc)                         ;; .. could move to intl.lisp
            #- (or ccl clisp ecl) (intl::string-to-octets str enc) ))    ;; GCL ignores enc
    (coerce ov 'list) ))
;;
#+ecl
(defun ecl-string-to-octets (str enc)
  (let ((a (make-array 
             (ceiling (* 1.2 (length str))) ;; initially add 20 % for non-ascii chars
             :element-type '(unsigned-byte 8)
             :adjustable t 
             :fill-pointer 0 )))
    (with-open-stream 
      (stream (ext:make-sequence-output-stream a :external-format enc))
      (format stream str)
      a )))


(defun $octets_to_string (ol &optional enc) 
  (unless ($listp ol)
    (gf-merror (intl:gettext 
      "`octets_to_string': argument must be a list of octets." )))
  (octets-to-string (cdr ol) enc) )

(defun octets-to-string (ol &optional enc) 
  (setq enc (get-encoding enc "octets_to_string"))
  (let ((ov (map-into 
              (make-array (length ol) :element-type '(unsigned-byte 8))
              #'identity
              ol )))
    #+ccl (ccl:decode-string-from-octets ov :external-format enc)
    #+clisp (ext:convert-string-from-bytes ov enc)
    #+ecl (ecl-octets-to-string ov enc)
    #- (or ccl clisp ecl) (intl::octets-to-string ov enc) ))
;;
#+ecl
(defun ecl-octets-to-string (ov enc)
  (with-open-stream 
    (stream (ext:make-sequence-input-stream ov :external-format enc))
    (read-line stream) ))


;; -- number-octet-conversions: --------------------------------------------- ;;

(defun word-to-octets (n) ;; assume that n fits into a word
  (do ((k 4 (1- k)) octs) 
      ((= k 0) octs)
    (push (logand n #xff) octs)
    (setq n (ash n -8.)) ))

(defun number-to-octets (n)
  (do (octs) (())
    (push (logand n #xff) octs)
    (setq n (ash n -8.))
    (when (= n 0) (return octs)) ))

(defun octets-to-number (octs)
  (reduce #'(lambda (x y) (logior (ash x 8.) y)) octs) )
  
(defmfun $number_to_octets (n) 
  (unless (and (integerp n) (>= n 0))
    (gf-merror (intl:gettext 
      "`number_to_octets': Argument must be a non-negative integer." )))
  (cons '(mlist simp) (number-to-octets n)) )

(defmfun $octets_to_number (octs) 
  (unless ($listp octs) 
    (gf-merror (intl:gettext 
      "`octets_to_number': Argument must be a list of octets." )))
  (octets-to-number (cdr octs)) )  


;; -- object identifiers (OID): --------------------------------------------- ;; 

;; examples:

;; 1.2.840.113549.1.1.1    -  RSA encryption
;; 1.3.14.3.2.26           -  SHA-1 hash algorithm
;; 2.16.840.1.101.3.4.2.1  -  SHA-256 hash algorithm
;; 1.3.132.0.6             -  secp112r1 - SEC 2 recommended elliptic curve


(defun oid-number-to-7bit (n)
  (let (lst res)
    (do () (())
      (setq lst (cons (logand n 127.) lst)
            n (ash n -7) )
      (when (= n 0) (return)) )
    (setq lst (nreverse lst)
          res (list (car lst)) )
    (dolist (bit7 (cdr lst) res)
      (setq res (cons (logior bit7 128.) res)) )))
;;
(defun oid-to-octets (str)
  (let* ((*read-base* 10.)
         (lst (mapcar #'(lambda (s) (with-input-from-string (a s) (read a))) 
                      (split str ".") )) ;; stringproc.lisp/split
          oct octs )
    (when (< (length lst) 2)
      (gf-merror (intl:gettext "No valid OID: ~m") str) )
    (setq oct (+ (* 40. (car lst)) (cadr lst))
          octs (list oct) )
    (dolist (n (cddr lst) octs)
      (setq octs (append octs (oid-number-to-7bit n))) )))
;;
(defmfun $oid_to_octets (str) 
  (cons '(mlist simp) (oid-to-octets str)) )



(defun oid-split (lst) 
  (do (res n) (())
    (setq n (car lst)
          res (cons n res)
          lst (cdr lst) )
    (when (or (null lst) (not (logbitp 7 n))) 
      (return (values (nreverse res) lst)) )))
;;
(defun octets-to-oid-1 (lst) 
  (let ((res 0))
    (dolist (n lst)
      (setq res (logior (logand n 127.) res))
      (unless (logbitp 7 n) (return res))
      (setq res (ash res 7)) )))
;;
(defun octets-to-oid (lst) 
  (if (= (length lst) 0)
    ""
    (let (str (tmp (car lst)) (*print-base* 10.))
      (setq lst (cdr lst))
      (multiple-value-bind (q r) (truncate tmp 40.)
        (setq str ($sconcat q "." r))
        (do () ((null lst) str)
          (multiple-value-setq (tmp lst) (oid-split lst)) 
          (setq str ($sconcat str "." (octets-to-oid-1 tmp))) )))))
;;
(defmfun $octets_to_oid (octets) 
  (octets-to-oid (cdr octets)) )


;; -- CRC24 checksum: ------------------------------------------------------- ;; 

(defun crc24sum (lst)
  (let ((r #xb704ce)
        (poly #x1864cfb) )
    (dolist (oct lst r)
      (setq r (logxor r (ash oct 16.)))
      (dotimes (i 8)
        (setq r (ash r 1))
        (when (logbitp 24. r) (setq r (logxor r poly))) ))))
;;
(defmfun $crc24sum (octets &optional (rtype '$string))
  (if ($listp octets)
    (let ((crc24 (crc24sum (cdr octets))))
      (cond
        ((equal rtype '$list)
          (cons '(mlist simp) (number-to-octets crc24)) )
        ((equal rtype '$number)
          crc24 )
        ((equal rtype '$string)
          (nstring-downcase (format nil "~6,'0x" crc24)) )
        (t  
          (gf-merror (intl:gettext 
            "`crc24sum': Optional argument must be 'list, 'number or 'string." )))))
    (gf-merror (intl:gettext 
      "`crc24sum': Argument must be a list of octets." ))))


(eval-when
  #+gcl (compile eval)
  #-gcl (:compile-toplevel :execute)
    (setq *read-base* old-ibase-cryptools) )
