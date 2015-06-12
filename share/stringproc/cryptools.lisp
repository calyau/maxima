
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
   
   1. number_to_octets(number) and octets_to_number(octets).

   (%i1) ibase : obase : 16.$
   (%i2) octets: [0ca,0fe,0ba,0be]$
   (%i3) number: octets_to_number(octets);
   (%o3)                            0cafebabe
   (%i4) number_to_octets(number);
   (%o4)                      [0CA, 0FE, 0BA, 0BE]

   
   2. octets_to_oid(octets) and oid_to_octets(string) compute object identifiers 
   (OIDs) from lists of octets and convert OIDs back to octets.
   
   (%i1) ibase : obase : 16.$
   (%i2) oid: octets_to_oid([2A,86,48,86,0F7,0D,1,1,1]);
   (%o2)                      1.2.840.113549.1.1.1
   (%i3) oid_to_octets(oid);
   (%o3)               [2A, 86, 48, 86, 0F7, 0D, 1, 1, 1]

   
   3. crc24sum(octets) returns the crc24 checksum of an octet-list. The returned 
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

(eval-when
  #+gcl (compile eval)
  #-gcl (:compile-toplevel :execute)
    (defvar old-ibase *read-base*)
    (setq *read-base* 10.) )


;; -- number-octet-conversions: --------------------------------------------- ;;

(defun word-to-octets (n) ;; used by stringproc/sha1.lisp and md5.lisp
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
         ;~ (lst (mapcar #'$parse_string (cdr ($split str "."))))
         ;~ (lst (mapcar #'parse-string (split str ".")))
         (lst (mapcar #'(lambda (s) (with-input-from-string (a s) (read a))) 
                      (split str ".") ))
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
    (setq *read-base* old-ibase) )
