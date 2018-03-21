;; unicode-sniffer.lisp -- attempt to determine Unicode encoding by inspecting bytes
;; copyright 2018 by Robert Dodier
;; I release this work under terms of the GNU General Public License, version 2

;; Adapted from: https://en.wikipedia.org/wiki/Byte_order_mark
(defparameter unicode-signatures
  ;; Sort signatures in order of decreasing length,
  ;; so longer signatures are tested first.
  ;; This really only makes a difference for UTF-16le vs UTF-32le,
  ;; but it is harmless in other cases.
  (sort
    '(((#xEF #xBB #xBF) . #+clisp charset:utf-8 #-clisp :utf-8)

      ((#xFE #xFF) . #+clisp charset:unicode-16-big-endian #+ecl :|utf-16be| #-(or clisp ecl) :utf-16be)
      ((#xFF #xFE) . #+clisp charset:unicode-16-little-endian #+ecl :|utf-16le| #-(or clisp ecl) :utf-16le)

      ((#x00 #x00 #xFE #xFF) . #+clisp charset:unicode-32-big-endian #+ecl :|utf-32be| #-(or clisp ecl) :utf-32be)
      ((#xFF #xFE #x00 #x00) . #+clisp charset:unicode-32-little-endian #+ecl :|utf-32le| #-(or clisp ecl) :utf-32le)

      ;; UTF-7 not known to SBCL, CCL, ECL, or CMUCL
      ((#x2B #x2F #x76 #x38) . #+clisp charset:utf-7 #-clisp :utf-7)
      ((#x2B #x2F #x76 #x39) . #+clisp charset:utf-7 #-clisp :utf-7)
      ((#x2B #x2F #x76 #x2B) . #+clisp charset:utf-7 #-clisp :utf-7)
      ((#x2B #x2F #x76 #x2F) . #+clisp charset:utf-7 #-clisp :utf-7)
      ((#x2B #x2F #x76 #x38 #x2D) . #+clisp charset:utf-7 #-clisp :utf-7)

      ;; UTF-1 not known to Clisp, SBCL, CCL, ECL, or CMUCL
      ((#xF7 #x64 #x4C) . :utf-1)

      ;; UTF-EBCDIC not known to Clisp, SBCL, CCL, ECL, or CMUCL
      ;; SBCL knows "US-EBCDIC" but UTF-EBCDIC is different (right?) so not known to SBCL either
      ((#xDD #x73 #x66 #x73) . :utf-ebcdic)

      ;; SCSU not known to Clisp, SBCL, CCL, ECL, or CMUCL
      ((#x0E #xFE #xFF) . :scsu)

      ;; BOCU not known to Clisp, SBCL, CCL, ECL, or CMUCL
      ((#xFB #xEE #x28) . :bocu-1)

      ;; :CP936 is a subset of :GB-18030 according to Wikipedia, so this is a "best fit"
      ;; GB-18030 and CP936 not known to CMUCL
      ((#x84 #x31 #x95 #x33) . #+clisp charset:cp936 #+(or ccl sbcl) :cp936 #+ecl :|cp936| #-(or clisp ccl sbcl ecl) :gb-18030))
    #'(lambda (a b) (> (length (car a)) (length (car b))))))

(defun sniffer-match (initial-bytes signature-bytes)
  (let*
    ((m (length signature-bytes))
     (initial-bytes-subseq (subseq initial-bytes 0 m))
     (byte-pairs (mapcar #'(lambda (a b) (list a b)) initial-bytes-subseq signature-bytes)))
    (loop for p in byte-pairs
          do (if (not (equal (first p) (second p)))
               (return-from sniffer-match nil)))
    t))

(defun sniffer-match-search (initial-bytes)
  (loop for x in unicode-signatures
        do (if (sniffer-match initial-bytes (car x))
             (return-from sniffer-match-search (cdr x)))))

;; Given a file name F, returns a Unicode encoding designator
;; if the initial bytes of F match any signature in the UNICODE-SIGNATURES table,
;; otherwise NIL.

(defun unicode-sniffer (f)
  (with-open-file (s f :element-type '(unsigned-byte 8))
    (let*
      ((signature-length-max (apply #'max (mapcar #'(lambda (x) (length (car x))) unicode-signatures)))
       (initial-bytes (loop repeat signature-length-max collect (read-byte s nil))))
      (sniffer-match-search initial-bytes))))

;; Try to verify that the inferred encoding is among
;; the encodings known to this Lisp implementation.
;; If there is no known method to check the encoding
;; for this Lisp implementation, return T.
;; Otherwise this function returns a generalized Boolean.

(defun check-encoding (e)
  #+ecl (member e (ext:all-encodings))
  #+ccl (ccl:lookup-character-encoding e)
  #+clisp (equal (symbol-package e) (find-package :charset))
  #+cmucl (member e (ext:list-all-external-formats))
  ;; SBCL ?? dunno how to check encoding at run time
  #-(or ecl ccl clisp cmucl) t) ;; say it's OK and hope for the best!
