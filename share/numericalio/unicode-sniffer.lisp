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
    '(((#xEF #xBB #xBF) . :utf-8)
      ((#xFE #xFF) . :utf-16be)
      ((#xFF #xFE) . :utf-16le)
      ((#x00 #x00 #xFE #xFF) . :utf-32be)
      ((#xFF #xFE #x00 #x00) . :utf-32le)
      ((#x2B #x2F #x76 #x38) . :utf-7)
      ((#x2B #x2F #x76 #x39) . :utf-7)
      ((#x2B #x2F #x76 #x2B) . :utf-7)
      ((#x2B #x2F #x76 #x2F) . :utf-7)
      ((#x2B #x2F #x76 #x38 #x2D) . :utf-7)
      ((#xF7 #x64 #x4C) . :utf-1)
      ((#xDD #x73 #x66 #x73) . :utf-ebcdic)
      ((#x0E #xFE #xFF) . :scsu)
      ((#xFB #xEE #x28) . :bocu-1)
      ((#x84 #x31 #x95 #x33) . :gb-18030))
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

