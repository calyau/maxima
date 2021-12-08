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

      ((#xFE #xFF) . #+clisp charset:unicode-16-big-endian #+ecl :UCS-2BE #+cmucl :utf-16-be #-(or clisp ecl cmucl) :utf-16be)
      ((#xFF #xFE) . #+clisp charset:unicode-16-little-endian #+ecl :UCS-2LE #+cmucl :utf-16-le #-(or clisp ecl cmucl) :utf-16le)

      ((#x00 #x00 #xFE #xFF) . #+clisp charset:unicode-32-big-endian #+ecl :UCS-4BE #+cmucl :utf-32-be #-(or clisp ecl cmucl) :utf-32be)
      ((#xFF #xFE #x00 #x00) . #+clisp charset:unicode-32-little-endian #+ecl :UCS-4LE #+cmucl :utf-32-le #-(or clisp ecl cmucl) :utf-32le)

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
      ((#x84 #x31 #x95 #x33) . #+clisp charset:cp936 #+(or ccl sbcl) :cp936 #+ecl :|cp936| #+abcl :gb18030 #-(or clisp ccl sbcl ecl abcl) :gb-18030))
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

;; Expose UNICODE-SNIFFER to Maxima user.
;; Returns the symbol name (i.e., a string) of the encoding,
;; if any was found, otherwise false.

(defun $inferred_encoding (f)
  (let ((e (unicode-sniffer f)))
    (if e (symbol-name e) "DEFAULT")))

;; Try to verify that the inferred encoding is among
;; the encodings known to this Lisp implementation.
;; If there is no known method to check the encoding
;; for this Lisp implementation, return 'UNKNOWN.
;; Otherwise this function returns a generalized Boolean.

(defun check-encoding (e)
  ;; work around ECL bug #435: "UCS-4LE not on list of basic encodings"
  #+ecl (or (eq e ':ucs-4le) (member e (ext:all-encodings)))
  #+ccl (ccl:lookup-character-encoding e)
  #+clisp (equal (symbol-package e) (find-package :charset))
  ;; CMUCL: flatten table of encodings and look for E among preferred names and their synonyms
  #+cmucl (member e (apply #'append (mapcar (lambda (l) (if (cdr l) (cons (car l) (cadr l)) l)) (ext:list-all-external-formats))))
  #+sbcl (check-encoding-sbcl e)
  #+gcl nil ;; GCL 2.6.12 does not recognize :external-format in OPEN
  ;; work around ABCL bug: "SYSTEM:AVAILABLE-ENCODINGS symbols strangeness" (https://github.com/armedbear/abcl/issues/82)
  #+abcl (member (symbol-name e) (mapcar #'symbol-name (system:available-encodings)) :test #'string=)
  #-(or ecl ccl clisp cmucl sbcl gcl abcl) 'unknown)

#+sbcl (defun check-encoding-sbcl (e)
         (let ((x sb-impl::*external-formats*))
           (cond
             ;; not sure when SBCL switched over from hash table to array ... try to handle both
             ((hash-table-p x) (gethash e x))
             ((arrayp x)
              (some
                #'identity
                (mapcar (lambda (l) (member e l))
                        (loop for ef across x
                              when (sb-impl::external-format-p ef)
                              collect (sb-impl::ef-names ef)))))
             (t (merror "CHECK-ENCODING: I don't know how to check encoding for this version of SBCL.")))))

;; Expose CHECK-ENCODING to Maxima user.
;; Argument is an encoding symbol name, such as that returned by $INFERRED_ENCODING.
;; Returns true if the encoding is recognized by the Lisp implementation,
;; false if the encoding is not recognized or the argument is null;
;; if there is no known method to check the encoding, print an error message.

;; CMUCL: symbols for encodings aren't known until this function is called.
#+cmucl (ext:list-all-external-formats)

(defun $recognized_encoding_p (e)
  (let ((e-up (string-upcase e)) (e-down (string-downcase e)))
    (or
      (not (null (string= e-up "DEFAULT")))
      (let ((s (or
                 (find-symbol e #+clisp :charset #-clisp :keyword)
                 (find-symbol e-up #+clisp :charset #-clisp :keyword)
                 (find-symbol e-down #+clisp :charset #-clisp :keyword))))
        (when s
          (let ((x (check-encoding s)))
            (cond
              ((eq x 'unknown)
               (merror (intl:gettext "recognized_encoding_p: I don't know how to verify encoding for this Lisp implementation.")))
              (t
                (not (null x))))))))))
