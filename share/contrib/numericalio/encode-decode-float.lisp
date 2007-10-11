;;  Copyright 2007 by Robert Dodier

;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License, version 2.

;;  This program has NO WARRANTY, not even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

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
;; endianness stuff adapted from read-bytes-standalone.lisp,
;; by Martin Raspaud and Robert Strandh,
;; which was released under terms of GNU GPL v2 or later.

(deftype endianness ()
  "Defines the legal values for *IO-ENDIANNESS*."
  '(member :big-endian :little-endian))

(defvar *io-endianness* :big-endian
  "*IO-ENDIANNESS* must be either :big-endian or :little-endian")

(defun define-io-endianness (endian)
  (check-type endian endianness)
  (setf *io-endianness* endian))

(defun read-unsigned-integer (nb-bytes s)
  "Read an unsigned integer of size NB-BYTES bytes from input stream S."
  (if (zerop nb-bytes) 0
    (let (bytes (y 0))
      (dotimes (i nb-bytes)
        (let ((x (read-byte s nil 'eof)))
          (if (eq x 'eof)
            (return-from read-unsigned-integer 'eof)
            (setq bytes (nconc bytes (list x))))))
      (case *io-endianness*
        (:little-endian
          (mapc #'(lambda (x) (setq y (+ x (ash y 8)))) (nreverse bytes)))
        (:big-endian
          (mapc #'(lambda (x) (setq y (+ x (ash y 8)))) bytes)))
      y)))

(defun write-unsigned-integer (quantity nb-bytes s)
  "Write an unsigned integer of size NB-BYTES bytes to output stream S."
  (case *io-endianness*
    (:little-endian
      (unless (zerop nb-bytes)
        (write-byte (logand quantity #xff) s)
        (write-unsigned-integer
          (ash quantity -8)
          (1- nb-bytes)
          s)))
    (:big-endian
      (unless (zerop nb-bytes)
        (write-unsigned-integer
          (ash quantity -8)
          (1- nb-bytes)
          s)
        (write-byte (logand quantity #xff) s)))))
