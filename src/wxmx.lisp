;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Reading the Maxima input out of a wxMaxima .wxmx worksheet.     ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A .wxmx file is a .zip container whose member content.xml holds the
;;; whole worksheet: text cells, input cells and the output wxMaxima
;;; last displayed.  wxMaxima writes the mimetype, format.txt and
;;; content.xml members uncompressed and ahead of the images, so the
;;; input cells can be recovered by walking the local file headers of
;;; the archive -- no decompressor and no complete .zip reader needed.
;;; A member that is compressed, or that defers its size to a data
;;; descriptor, is diagnosed rather than guessed at.

(in-package :maxima)
(macsyma-module wxmx)

;; The signature of a .zip local file header, "PK\3\4" read as a
;; little-endian 32 bit integer.
(defconstant +wxmx-local-file-header+ #x04034b50)

(defun wxmx-read-u16 (stream)
  "Read a little-endian 16 bit unsigned integer.  NIL at end of file."
  (let* ((low (read-byte stream nil nil))
         (high (and low (read-byte stream nil nil))))
    (and high (logior low (ash high 8)))))

(defun wxmx-read-u32 (stream)
  "Read a little-endian 32 bit unsigned integer.  NIL at end of file."
  (let* ((low (wxmx-read-u16 stream))
         (high (and low (wxmx-read-u16 stream))))
    (and high (logior low (ash high 16)))))

(defun wxmx-read-octets (stream count)
  "Read COUNT octets.  NIL if the stream does not hold that many -- a
  size a damaged header overstates is not to be allocated first and
  found out about afterwards."
  (let ((length (ignore-errors (file-length stream))))
    (unless (and length (<= count (- length (file-position stream))))
      (return-from wxmx-read-octets nil)))
  (let ((octets (make-array count :element-type '(unsigned-byte 8))))
    (and (eql count (read-sequence octets stream)) octets)))

(defun wxmx-skip (stream count)
  "Move COUNT octets forward in STREAM."
  (or (file-position stream (+ (file-position stream) count))
      (dotimes (i count t)
        (unless (read-byte stream nil nil) (return nil)))))

(defun wxmx-member-name (octets)
  "The name a .zip local file header stores, as a string.  Member names
  are UTF-8 or CP437; the ones we look for are plain ASCII, where the
  two agree."
  (map 'string #'code-char octets))

(defun wxmx-stored-member (stream name filename)
  "Return the contents of the archive member NAME of the .zip file open
  on STREAM as a vector of octets, or NIL if there is no such member.
  FILENAME names the archive in error messages."
  (loop
    (unless (eql (wxmx-read-u32 stream) +wxmx-local-file-header+)
      ;; Either the end of the last member's data, where the central
      ;; directory begins, or a truncated file.  Either way there is no
      ;; further local file header to read.
      (return nil))
    (wxmx-read-u16 stream)                      ; version needed to extract
    (let ((flags (wxmx-read-u16 stream))
          (method (wxmx-read-u16 stream)))
      (wxmx-read-u32 stream)                    ; modification time and date
      (wxmx-read-u32 stream)                    ; CRC-32
      (let* ((compressed-size (wxmx-read-u32 stream))
             (uncompressed-size (wxmx-read-u32 stream))
             (name-length (wxmx-read-u16 stream))
             (extra-length (wxmx-read-u16 stream))
             (member (and extra-length
                          (wxmx-read-octets stream name-length))))
        (declare (ignore uncompressed-size))
        (unless member
          (return nil))
        (wxmx-skip stream extra-length)
        ;; Bit 3 of the flags moves the sizes behind the member's data,
        ;; into a data descriptor, and leaves zeroes here.  Then there
        ;; is nothing to skip the member by.
        (cond ((string= name (wxmx-member-name member))
               (cond ((not (eql method 0))
                      (merror (intl:gettext "Cannot read the .wxmx worksheet ~A: its ~A is stored compressed, which needs a .zip decompressor.")
                              (namestring filename) name))
                     ((logbitp 3 flags)
                      (merror (intl:gettext "Cannot read the .wxmx worksheet ~A: its ~A does not record its size, which needs a full .zip reader.")
                              (namestring filename) name))
                     (t
                      (return (wxmx-read-octets stream compressed-size)))))
              ((logbitp 3 flags)
               (merror (intl:gettext "Cannot read the .wxmx worksheet ~A: the member ~A ahead of ~A does not record its size, which needs a full .zip reader.")
                       (namestring filename) (wxmx-member-name member) name))
              (t
               (wxmx-skip stream compressed-size)))))))

(defun wxmx-code-char (code)
  "The character of code point CODE, or a question mark where this Lisp
  has none."
  (or (and code
           (< code char-code-limit)
           (code-char code))
      #\?))

;; INTL::OCTETS-TO-STRING would do this for a handful of Lisps and fall
;; back to Latin-1 for the rest, so decode UTF-8 here instead: content.xml
;; is UTF-8 whatever Lisp Maxima was built with.
(defun wxmx-utf-8-string (octets)
  "Decode OCTETS, a UTF-8 encoded archive member, into a string.  A
  malformed sequence, and a character this Lisp has no CODE-CHAR for,
  become a question mark."
  (let* ((length (length octets))
         (string (make-array length :element-type 'character
                                    :adjustable t :fill-pointer 0))
         (i 0))
    (loop while (< i length)
          do (let* ((byte (aref octets i))
                    ;; How many continuation bytes the leading byte
                    ;; promises; -1 marks a byte that cannot lead one.
                    (continuations (cond ((< byte #x80) 0)
                                         ((< byte #xc0) -1)
                                         ((< byte #xe0) 1)
                                         ((< byte #xf0) 2)
                                         ((< byte #xf8) 3)
                                         (t -1)))
                    (code (cond ((minusp continuations) nil)
                                ((zerop continuations) byte)
                                (t (logand byte
                                           (ash #x1f (- 1 continuations)))))))
               (incf i)
               (loop repeat (max continuations 0)
                     while code
                     do (let ((continuation (if (< i length)
                                                (aref octets i)
                                                0)))
                          (cond ((eql (logand continuation #xc0) #x80)
                                 (setq code (logior (ash code 6)
                                                    (logand continuation #x3f)))
                                 (incf i))
                                (t
                                 (setq code nil)))))
               (vector-push-extend (wxmx-code-char code) string)))
    (coerce string 'simple-string)))

(defun wxmx-parse-integer (digits radix)
  "The integer DIGITS spells in RADIX, or NIL if it spells none."
  (ignore-errors (parse-integer digits :radix radix)))

(defun wxmx-push-reference (name string)
  "Push the character the XML reference &NAME; stands for onto STRING.
  A reference we do not know is copied through as it was written."
  (let* ((digits (and (> (length name) 1) (char= (char name 0) #\#)))
         (hexadecimal (and digits
                           (> (length name) 2)
                           (member (char name 1) '(#\x #\X))))
         (code (cond ((string= name "lt") 60)
                     ((string= name "gt") 62)
                     ((string= name "amp") 38)
                     ((string= name "quot") 34)
                     ((string= name "apos") 39)
                     (hexadecimal (wxmx-parse-integer (subseq name 2) 16))
                     (digits (wxmx-parse-integer (subseq name 1) 10))
                     (t nil))))
    (cond ((and code (< code char-code-limit) (code-char code))
           (vector-push-extend (code-char code) string))
          (t
           (vector-push-extend #\& string)
           (loop for character across name
                 do (vector-push-extend character string))
           (vector-push-extend #\; string)))))

;; An XML reference is short: &thisisnotareference; in a text cell must
;; not swallow the text that follows it looking for a semicolon.
(defconstant +wxmx-reference-length+ 12)

(defun wxmx-push-text (xml start end string)
  "Push the text XML holds between START and END onto STRING, resolving
  the XML references in it."
  (let ((i start))
    (loop while (< i end)
          do (let* ((limit (min end (+ i +wxmx-reference-length+)))
                    (semicolon (and (char= (char xml i) #\&)
                                    (position #\; xml :start i :end limit))))
               (cond (semicolon
                      (wxmx-push-reference (subseq xml (1+ i) semicolon) string)
                      (setq i (1+ semicolon)))
                     (t
                      (vector-push-extend (char xml i) string)
                      (incf i)))))))

(defun wxmx-push-lines (xml start end string)
  "Push the text of the <line> elements XML holds between START and END
  onto STRING, one line each."
  (let ((i start))
    (loop
      (let ((line (search "<line>" xml :start2 i :end2 end)))
        (unless line
          (return))
        (let ((close (or (search "</line>" xml :start2 line :end2 end) end)))
          (wxmx-push-text xml (+ line 6) close string)
          (vector-push-extend #\Newline string)
          (setq i (min end (+ close 7))))))))

(defun wxmx-content-input (xml)
  "The Maxima input of the worksheet whose content.xml is XML: the text
  of the <line> elements of every <input> element, in the order the
  worksheet has them.  Everything else -- title, section and text cells,
  and the stored output -- is not input and is left behind."
  (let ((string (make-array (length xml) :element-type 'character
                                         :adjustable t :fill-pointer 0))
        (i 0))
    (loop
      (let ((input (search "<input>" xml :start2 i)))
        (unless input
          (return))
        (let ((close (or (search "</input>" xml :start2 input) (length xml))))
          (wxmx-push-lines xml (+ input 7) close string)
          (setq i close))))
    (coerce string 'simple-string)))

(defun wxmx-input-string (filename)
  "The Maxima input stored in the .wxmx worksheet FILENAME."
  (let ((content (with-open-file (stream filename
                                         :element-type '(unsigned-byte 8))
                   (wxmx-stored-member stream "content.xml" filename))))
    (unless content
      (merror (intl:gettext "~A holds no content.xml; it is not a .wxmx worksheet.")
              (namestring filename)))
    (wxmx-content-input (wxmx-utf-8-string content))))
