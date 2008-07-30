;; encode-decode-float.lisp
;; Copyright 2007 by Robert Dodier

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License.

;; This program has NO WARRANTY, not even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;; These functions encode integers into 64 bit IEEE 754 floats
;; and decode 64 bit floats into 64 bit integers.
;; These functions cannot handle any other size of float.
;; 
;; Encode float-64 to integer:  SMASH-FLOAT-64-INTO-INTEGER
;; Decode integer to float-64:  CONSTRUCT-FLOAT-64-FROM-INTEGER
;;
;; Write float-64 to output stream:  WRITE-FLOAT-64
;; Read float-64 from input stream:  READ-FLOAT-64
;; Read an unsigned integer (of any size) from input stream: READ-UNSIGNED-INTEGER
;; Write an unsigned integer (of any size) to output stream: WRITE-UNSIGNED-INTEGER
;; Set assumed external byte order for input and output:  DEFINE-EXTERNAL-BYTE-ORDER

(in-package :maxima)

(defun smash-float-64-into-integer (x)
  (multiple-value-bind
    (significand exponent sign)
    (integer-decode-float x)
    (smash-decoded-float-64-into-integer significand exponent sign)))

(defun smash-decoded-float-64-into-integer (significand exponent sign)
  (if (and (= significand 0) (= exponent 0))
    0
    (dpb
      (if (> sign 0) 0 1)
      (byte 1 (+ 52 11))
      (dpb
        (+ exponent 1023 52)
        (byte 11 52)
        (ldb
          (byte 52 0)
          significand)))))

(defun construct-float-64-from-integer (x)
  (multiple-value-bind
    (significand exponent sign)
    (extract-smashed-float-64-from-integer x)
    (* sign (scale-float (float significand 1d0) exponent))))

(defun extract-smashed-float-64-from-integer (x)
  (if (eq x 0)
    (values 0 0 0)
    (let
      ((significand (dpb x (byte 52 0) #x10000000000000))
       (exponent (- (ldb (byte 11 52) x) 1023 52))
       (sign (if (eq (ldb (byte 1 63) x) 0) 1 -1)))
      (values significand exponent sign))))

;; Stream input and output

(defun write-float-64 (x s)
  (write-unsigned-integer (smash-float-64-into-integer x) 8 s))

(defun read-float-64 (s)
  (let ((x (read-unsigned-integer 8 s)))
    (if (eq x 'eof) 'eof (construct-float-64-from-integer x))))

;; READ-UNSIGNED-INTEGER, WRITE-UNSIGNED-INTEGER, and associated
;; byte order stuff adapted from read-bytes-standalone.lisp,
;; by Martin Raspaud and Robert Strandh,
;; which was released under terms of GNU GPL v2 or later.

(deftype external-byte-order ()
  "Defines the legal values for *EXTERNAL-BYTE-ORDER*."
  '(member :msb :lsb))

(defvar *external-byte-order* :msb
  "*EXTERNAL-BYTE-ORDER* must be either :msb or :lsb")

(defun define-external-byte-order (x)
  (check-type x external-byte-order)
  (setf *external-byte-order* x))

(defun read-unsigned-integer (nb-bytes s)
  "Read an unsigned integer of size NB-BYTES bytes from input stream S."
  (if (zerop nb-bytes) 0
    (let (bytes (y 0))
      (dotimes (i nb-bytes)
        (let ((x (read-byte s nil 'eof)))
          (if (eq x 'eof)
            (return-from read-unsigned-integer 'eof)
            (setq bytes (nconc bytes (list x))))))
      (case *external-byte-order*
        (:lsb
          (mapc #'(lambda (x) (setq y (+ x (ash y 8)))) (nreverse bytes)))
        (:msb
          (mapc #'(lambda (x) (setq y (+ x (ash y 8)))) bytes)))
      y)))

(defun write-unsigned-integer (quantity nb-bytes s)
  "Write an unsigned integer of size NB-BYTES bytes to output stream S."
  (case *external-byte-order*
    (:lsb
      (unless (zerop nb-bytes)
        (write-byte (logand quantity #xff) s)
        (write-unsigned-integer
          (ash quantity -8)
          (1- nb-bytes)
          s)))
    (:msb
      (unless (zerop nb-bytes)
        (write-unsigned-integer
          (ash quantity -8)
          (1- nb-bytes)
          s)
        (write-byte (logand quantity #xff) s)))))
