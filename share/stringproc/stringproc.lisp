;;
;;                                  ~*~  STRINGPROC  ~*~
;;
;; Maxima string processing
;;
;; Copyright     : 2005-2015 Volker van Nek
;; Licence       : GPL2
;;
;; Test file     : rteststringproc.mac
;; Documentation : stringproc.texi
;;

#|
Strings of length 1 are denoted as a Maxima character.

Position indices in strings are 1-indexed like in Maxima lists. This results to 
the following consistency.

               charlist(str)[i] = charat(str, i), i >= 1.
|#

(in-package :maxima)


#+ (and (or cmucl gcl) unix) 
(declare-top (special *read-utf-8*))

#|
The following refers to CMUCL and GCL builds on Linux only:

In a terminal which encodes characters in UTF-8 a string of length 1 which 
contains a non-us-ascii character like an umlaut is read as a sequence of two 
or more octets, e.g. auml -> #(195 164), i.e. as two or more characters.

When the flag *read-utf-8* is set to false (the default) the string processing 
functions in this file decode and restructure octet sequences in a way that 
octet groups like '(195 164) are coded back into the original string of length 1.

In addition to this position indices in strings are fixed accordingly.

Functions like $alphacharp which need a Lisp character representation of a 
Maxima character do not work for non-us-ascii characters, e.g. auml is not 
recognized as alphabetic.

CMUCL (GNU/Linux terminal) reads ISO8859-1 by default. 
But unlike GCL CMUCL may be set to UTF-8 by adjusting the used external format 
(see $adjust_external_format below).
|#

#+ (and (or cmucl gcl) unix)
(defvar *read-utf-8* 
  #+gcl nil 
  #+cmucl (eq (stream-external-format *standard-output*) :utf-8) 
  ;; CMUCL: wxMaxima: format is :utf-8
  ;;        terminal: -output* is :iso8859-1 resp. :utf-8, -input* is 'default'
  "The external format has been set to UTF-8." )


;; Setting this flag to true saves CMUCL and GCL from parsing UTF-8 encoding.
;;   (CMUCL: Adjusting the external format should be preferred.)
;;
#+ (and (or cmucl gcl) unix) 
(defvar $us_ascii_only nil "Promise to use only US-ASCII characters.")
;;
#+ (and (or cmucl gcl) unix)
(putprop '$us_ascii_only 'set-*read-utf-8* 'assign)
;;
#+ (and (or cmucl gcl) unix)
(defun set-*read-utf-8* (assign-var arg) 
  (declare (ignore assign-var))
  (setq *read-utf-8* 
    (if arg 
      t
      #+gcl nil 
      #+cmucl (eq (stream-external-format *standard-output*) :utf-8) )))


;; frequently used error messages:

(defun io-error (name which)
  (gf-merror (intl:gettext "`~m': ~m argument must be a stream.") name which) )

(defun s-error1 (name which)
  (gf-merror (intl:gettext "`~m': ~m argument must be a string.") name which) )

(defun s-error2 (name which)
  (gf-merror (intl:gettext "`~m': ~m arguments must be strings.") name which) )
 
(defun s-pos-error1 (name pos)
  (gf-merror (intl:gettext "`~m': unsuitable position index ~m") name pos) )

(defun s-pos-error2 (name)
  (gf-merror (intl:gettext "`~m': unsuitable start or end position.") name) )


;;  1. I/O
;;  2. characters
;;  3. strings


;; -------------------------------------------------------------------------- ;;
;;  1. I/O


;; $openw_binary, $opena_binary, $openr_binary 
;;    are placed in share/numercalio/numercalio.lisp.


(defun $openw (file &optional enc)
  #+gcl (declare (ignore enc))
  (unless (stringp file) (s-error1 "openw" "the"))
  (open file
        :direction :output
        #-gcl :external-format #-gcl (get-encoding enc "openw")
        :if-exists :supersede
        :if-does-not-exist :create ))


(defun $opena (file &optional enc)
  #+gcl (declare (ignore enc))
  (unless (stringp file) (s-error1 "opena" "the"))
  (open file
        :direction :output
        #-gcl :external-format #-gcl (get-encoding enc "opena")
        :if-exists :append
        :if-does-not-exist :create ))


(defun $openr (file &optional enc) 
  #+gcl (declare (ignore enc))
  (unless (stringp file) (s-error1 "openr" "the"))
  (unless (probe-file file)
    (gf-merror (intl:gettext "`openr': file does not exist: ~m") file) )
  #+gcl (open file)
  #-gcl (open file :external-format (get-encoding enc "openr")) )


(defun $make_string_input_stream (str &optional (start 1) (end nil)) ;; use 1-indexing
  (unless (stringp str) (s-error1 "make_string_input_stream" "first"))
  (decf start)
  (when end (decf end))
  (or (ignore-errors
        (make-string-input-stream str start end) )
      (s-pos-error2 "make_string_input_stream") ))


(defun $make_string_output_stream ()
  (make-string-output-stream) )


;; Ignore the :element-type keyword. 
;; So we get the default here, namely :element-type character.
;;
(defun $get_output_stream_string (stream)
  (unless (streamp stream) (io-error "get_output_stream_string" "the"))
  (get-output-stream-string stream) )


(defun $close (stream) 
  (unless (streamp stream) (io-error "close" "the"))
  (close stream))


(defun $flush_output (stream) 
  (unless (streamp stream) (io-error "flush_output" "the"))
  (not (finish-output stream)) ) ;; so $flush_output and $close both return t


(defun $flength (stream) 
  (unless (streamp stream) (io-error "flength" "the"))
  (file-length stream))


(defun $fposition (stream &optional pos)
  (unless (streamp stream) (io-error "fposition" "first"))
  (or (ignore-errors 
        (if pos
          (file-position stream (1- pos))     ;; set file-pos, return t   (or nil)
          (progn 
            (setq pos (file-position stream)) ;; get file-pos, return pos (or nil)
            (when pos (1+ pos)) )))
      (s-pos-error1 "fposition" pos) ))


(defun $readline (stream)
  (unless (streamp stream) (io-error "readline" "the"))
  (let ((line (read-line stream nil nil)))
    (if line line) ))


(defun $readchar (stream) 
  (unless (streamp stream) (io-error "readchar" "the"))
  (let ((lc (read-char stream nil nil)))
    (when lc (string lc)) ))


(defun $readbyte (stream) 
  (unless (and (streamp stream)
               (equal (stream-element-type stream) '(unsigned-byte 8)) )
    (gf-merror (intl:gettext "`readbyte': argument must be a binary stream.")) )
  (read-byte stream nil nil) )


(defun $writebyte (i stream) 
  (unless (and (streamp stream)
               (equal (stream-element-type stream) '(unsigned-byte 8)) )
    (gf-merror (intl:gettext "`writebyte': argument must be a binary stream.")) )
  (write-byte i stream) )


(defun $freshline (&optional (stream)) 
  (and stream (not (streamp stream)) (io-error "freshline" "optional"))
  (fresh-line stream))


(defun $newline (&optional (stream)) 
  (and stream (not (streamp stream)) (io-error "newline" "optional"))
  (terpri stream) )


;; $printf is in printf.lisp


;; -- get a suitable encoding (hopefully) ----------------------------------- ;;
;;
(defun get-encoding (enc name) 
  (cond
    (enc 
      (unless (stringp enc) 
        (gf-merror (intl:gettext 
          "`~m': the optional second argument must be a string." ) name ))
      (setq enc (intern (string-upcase enc) :keyword))
      ;;
      #+ccl (progn
        #+unix enc
        #-unix (cond 
          ((stream-external-format *standard-input*) ;; nil by default in a terminal
            enc )
          (t 
            (is-ignored enc name "to get some help")
            :utf-8 )))
      ;;
      #+clisp (progn
        #+unix (ext:make-encoding :charset (symbol-name enc) :line-terminator :unix)
        #-unix (let ((ef (stream-external-format *standard-input*)))
          (cond 
            ((search "UTF" (mfuncall '$string ef))
              (ext:make-encoding :charset (symbol-name enc) :line-terminator :dos) )
            (t 
              (is-ignored enc name "to enable the encoding argument")
              ef ))))
      ;;
      #+cmucl (progn
        #+unix (let ((ef (stream-external-format *standard-output*))) ;; -input* remains 'default'
          (cond 
            ((eq ef :utf-8)
              enc )
            (t 
              (is-ignored enc name "to enable the encoding argument")
              ef )))
        #-unix enc )
      ;;
      #+gcl (format t "`~a': GCL ignores the argument ~s.~%" name 
        (string-downcase (symbol-name enc)) )
      ;;
      #+sbcl (progn
        #+unix enc
        #-unix (cond 
          ((eq (stream-external-format *standard-input*) :cp1252) 
            (is-ignored enc name "to enable the encoding argument")
            (stream-external-format *standard-input*) )
          (t enc) ))
      ;;
      #- (or ccl clisp cmucl gcl sbcl) enc ) ;; ECL and others
      ;;
    (t
      #+cmucl (stream-external-format *standard-output*) ;; -input* remains 'default' in terminal
      #+ (or ccl gcl) :utf-8 ;; ignored by GCL
      #- (or ccl cmucl gcl) (stream-external-format *standard-input*) )))


#+ (or cmucl (and (not unix) (or ccl clisp sbcl)))
(defun is-ignored (enc name adds)
  (format t "`~a': The argument ~s is ignored. Enter~%" name 
    (string-downcase (symbol-name enc)) )
  (format t "adjust_external_format();~%~a.~%" adds) )


;; -- adjust the external format -------------------------------------------- ;; 
#|
Linux/Unix:

       terminal, wxMaxima              Lisp reader            string_to_octets
string_0 ----encode----> UTF-8-octets ----decode----> string_1 ----encode----> octets

wMaxima and commonly used terminals read characters in UTF-8 
which means the characters are encoded as UTF-8-octets.

From the Lisp's point of view this is the external format of the input.
Any Lisp reader should read and decode this input in UTF-8.
Then all characters are read as they are entered in wMaxima or terminal.

This is the default for SBCL, CLISP, ECL and CCL.

CMUCL uses :iso8859-1 as the default external format 
and GCL has no format definition. 
So these two read characters each corresponding to a single UTF-8-octet.
Characters encoded in more than one octet are misinterpreted.

The result of e.g. cryptools.lisp/string_to_octets is based on this pre-processing. 

In SBCL, CLISP, ECL and CCL string_0 is equal to string_1 and string_to_octets 
can use any valid encoding format to encode the string.

GCL applies no format definition on the UTF-8-octets read from input 
and the octets effectively remain UTF-8-encoded.

If CMUCL uses the default external format :iso8859-1 then like in GCL 
string_to_octets returns the UTF-8-octets from input unchanged.
But if string_to_octets should use any other format for encoding 
CMUCL must switch to the external format :utf-8 for reading.

CMUCL: adjust_external_format sets the external format to UTF-8.
All others: adjust_external_format prints a message and does nothing.

Observations based on Maxima 5.37post from git (Nov 2015).

Windows:

Like in Linux the external format of the Lisp reader should meet the format 
used by the terminal resp. by wxMaxima. 

If the terminal uses cp850 it should be set to cp1252.
The font should be set to true type. 
Both changes enable the full range of cp1252 and are assumed in the following.

CCL(terminal) reads UTF-8 but the input from terminal is cp1252. 
  The UTF-8 reader misinterprets codepoints > 127. Adjustment needed. 
  Switch to ISO-8859-1 via Lisp option in 'maxima.bat'. cp1252 is not supported.

CCL(wxMaxima) reads UTF-8 and the input from wxMaxima is UTF-8. Do nothing.

CLISP(terminal) reads cp1252 and the input is cp1252. Do nothing.

CLISP(wxMaxima) reads cp1252 but the input is UTF-8. Adjustment needed. 
  Switch to UTF-8 via Lisp command for the current session.

GCL has no format definition. Input from terminal and wxMaxima is cp1252. Do nothing.

SBCL(terminal) reads UCS-2LE and the input is UCS-2LE. Do nothing.

SBCL(wxMaxima) reads cp1252 but the input is UTF-8. Adjustment needed. 
  Switch to UTF-8 via Lisp command in init file.

Observations based on Maxima 5.36.1(ccl), 5.37.2(clisp), 5.28.0(gcl), 5.37.2(sbcl)
in Windows 7.
|#

(defun $adjust_external_format () 
  #+ccl (let ((ef (stream-external-format *standard-input*)))
    (format t "The external format is currently ~a" ef)
    #+unix (format t " (i.e. utf-8)~%and has not been changed.~%")
    #-unix (progn
      (format t ".~%Changing the external format is not necessary when you are using wxMaxima.~%")
      (use-cp1252)
      (format t "The external format for Maxima in a terminal is settable by a Lisp option~%")
      (format t "in 'maxima.bat'. Please change the line~%set lisp_options=~%to~%") 
      (format t "set lisp_options=-K :iso-8859-1~%") 
      (format t "(The terminal encodings cp850 and cp1252 are not supported by CCL.)~%") ))
  ;;
  #+clisp (let ((ef (stream-external-format *standard-input*)))
    #-unix (cond 
      ((search "UTF" (mfuncall '$string ef))
        (format t "The external format is already ~a.~%Any change is not necessary.~%" ef) )
      (t
        (format t "The external format has been changed~%from ~a to ~a for this session only.~%" ef
          (setf custom:*terminal-encoding* 
            (ext:make-encoding :charset (symbol-name :utf-8) :line-terminator :dos) ))
        (use-cp1252)
        t ))
    #+unix (format t "The external format is currently ~a~%and has not been changed.~%" ef) )
  ;;
  #+cmucl (let ((ef (stream-external-format *standard-output*))) ;; -input* might be 'default'
    (setq *read-utf-8* t)
    (cond
      ((eq ef :utf-8) 
        (format t "The external format is already ~a.~%Any change is not necessary.~%" ef) )
      (t 
        (format t "The external format has been changed~%from ~a to UTF-8 for this session only.~%" ef)
        (stream:set-system-external-format :utf-8) ))) ;; returns t
  ;;
  #+ecl
    (format t "The external format is currently ~a~%and has not been changed.~%"
      (stream-external-format *standard-input*) )
  ;;
  #+gcl (progn
    (format t "Changing the external format is not possible.~%")
    #-unix (use-cp1252) )
  ;;
  #+sbcl (let ((ef (stream-external-format *standard-input*)))
    (cond
      ((eq ef :cp1252)
        (let ((path (sb-impl::userinit-pathname))
              (cmd "(setf sb-impl::*default-external-format* :utf-8)") )
          (with-open-file (stream 
                           path 
                           :direction :output 
                           :if-exists :append 
                           :if-does-not-exist :create ) 
            (format stream "~a~%" cmd) )
          (format t "The line~%~a~%has been appended to the init file~%~a~%" cmd path)
          (format t "Please restart Maxima to set the external format to UTF-8.~%") ))
      (t
        (format t "The external format is currently ~a~%and has not been changed.~%" ef) )))
  ;;
  #- (or ccl clisp cmucl ecl gcl sbcl) ;; all others
    (format t "Please file a report if adjusting the external format is necessary.~%") )


#+ (and (not unix) (or ccl clisp gcl))
(defun use-cp1252 () 
  (format t "If you work in a terminal you should consider to insert a line~%")
  (format t "chcp 1252~%immediately below of '@echo off' in~%~s~%" 
    (combine-path *maxima-prefix* "bin" "maxima.bat") )
  (format t "and to change the font of the terminal window to a true type font.~%") )


;; -------------------------------------------------------------------------- ;;
;; 2. characters

;; A Maxima character is a string of length 1. (Lisp strings are Maxima strings.)


;; Check if object is a Maxima character.
;;
(defun $charp (obj)
  (and (stringp obj) 
       (= 1 #- (and (or cmucl gcl) unix) (length obj)
            #+ (and (or cmucl gcl) unix) 
              (if *read-utf-8* (length obj) (utf-8-slength obj)) )))


;; Convert a string of length 1 into a Lisp character.
;;
(defun $lchar (mc) ;; at Maxima level only for testing
  (l-char mc) )
;;
(defun l-char (mc)
  (unless (and (stringp mc) (= 1 (length mc)))
    (gf-merror "package stringproc: ~m cannot be converted into a Lisp character." mc) )
  (character mc) )


;; Convert a Lisp character into a string of length 1.
;;
(defun $cunlisp (lc) ;; at Maxima level only for testing
  (unless (characterp lc)
    (gf-merror "cunlisp: argument must be a Lisp character") )
  (string lc) )


;; Check if object is a Lisp character.
;;
(defun $lcharp (obj) (characterp obj)) ;; for testing only


;; Tests for Lisp characters at Maxima level (Lisp level functions see below).
;;
;;   GCL, non-utf-8-CMUCL: 
;;     If code point is larger than 127 these functions throw an error via l-char.
;;
(defun $constituent (mc)   (constituent (l-char mc)))
(defun $alphanumericp (mc) (alphanumericp (l-char mc)))
(defun $alphacharp (mc)    (alpha-char-p (l-char mc)))
(defun $lowercasep (mc)    (lower-case-p (l-char mc)))
(defun $uppercasep (mc)    (upper-case-p (l-char mc)))
;;
(defun $digitcharp (mc)
  (let ((nr (char-int (l-char mc))))
    (and (> nr 47.) (< nr 58.)) ))


;; Maxima character <--> code point or character name
;;
;;  $cint returns the corresponding unicode code point.
;;  $ascii returns a Maxima us-ascii-character for code points < 128.
;;  $unicode returns a Maxima character for a given code point or name.
;;
;;  The conversion Maxima character to name is possible in clisp, ecl, sbcl
;;  via printf(false, "~@c", mc);
;;  
;;  GCL in Linux/Unix: 
;;    A non-ASCII-character is encoded in UTF-8 by wxMaxima or terminal.
;;    GCL just passes them through octet by octet. Process these octets.
;;
;;  CMUCL in Linux/Unix: (Windows ?) 
;;    It is assumed that the external format has been adjusted to UTF-8
;;    (see $adjust_external_format above for more).
;;    $cint recognizes 16 bit characters only. 
;;    utf8_to_unicode(string_to_octets(mc)); works where $cint fails.
;;
(defun $cint (mc) 
  (unless ($charp mc)
    (gf-merror (intl:gettext "`cint': argument must be a Maxima character.")) )
  #- (and (or cmucl gcl) unix) (char-code (character mc))
  #+ (and (or cmucl gcl) unix) 
    (if *read-utf-8*
      (char-code (character mc))
      (utf8-to-uc (mapcar #'char-code (coerce mc 'list))) ))
;;
(defun $ascii (int) 
  (unless (and (integerp int) (< int 128.))
    (gf-merror (intl:gettext 
      "`ascii': argument must be a non-negative integer less than 128.
Please use `unicode' for code points larger than 127." )))
  (string (code-char int)) )
;;
;; Code points as arguments are not checked for validity. 
;; Names as arguments work in clisp, ecl, sbcl.
;; In allegro, cmucl code points and names are limited to 16 bit.
;; abcl, ccl, gcl, lispworks: unicode(name) returns false.
;; octets_to_string(unicode_to_utf8(code_point)); often works where unicode(code_point) fails.
;;
(defun $unicode (arg) 
  (cond
    ((integerp arg)
      (ignore-errors ;; cmucl errors for arguments larger than 16 bit
        #- (and (or cmucl gcl) unix) (string (code-char arg))
        #+ (and (or cmucl gcl) unix) 
          (if *read-utf-8*
            (string (code-char arg))
            (let ((ol (uc-to-utf8 arg)))
              (utf-8-m-char (length ol) ol) ))))
    ((stringp arg)
      (setq arg (concatenate 'string "#\\" ($ssubst "_" " " arg)))
      (let ((*standard-input* (make-string-input-stream arg)))
        (ignore-errors (string (eval (read)))) ))
    (t
      (gf-merror (intl:gettext 
        "`unicode': argument must be a string or a non-negative integer." )))))

;; Code point conversion utf-8 <--> unicode
;;
(defun $utf8_to_unicode (utf8)
  (unless (listp utf8) (utf8_to_unicode-error))
  (utf8-to-uc (cdr utf8)) )
;;
(defun $unicode_to_utf8 (uc)
  (unless (integerp uc) 
    (gf-merror (intl:gettext "`unicode_to_utf8': argument must be a non-negative integer.")) )
  (cons '(mlist simp) (uc-to-utf8 uc)) )
;;
(defun utf8_to_unicode-error ()
  (gf-merror (intl:gettext 
    "`utf8_to_unicode': argument must be a list of octets representing a single character." )))
;;
(defun utf8-to-uc (u) ;; u = utf8
  (let ((l (length u)))
    (cond
      ((= 1 l) 
        (when (logbitp 7 (car u)) (utf8_to_unicode-error))
        (car u) ) 
      ((= 2 l)
        (unless (= (ldb (byte 3 5) (car u)) #b110) (utf8_to_unicode-error)) ;; check the first octet only
        (logior (ash (ldb (byte 5 0) (car u)) 6)
                (ldb (byte 6 0) (cadr u)) ))
      ((= 3 l)
        (unless (= (ldb (byte 4 4) (car u)) #b1110) (utf8_to_unicode-error))
        (logior (ash (ldb (byte 4 0) (car u)) 12.)
                (ash (ldb (byte 6 0) (cadr u)) 6.)
                (ldb (byte 6 0) (caddr u)) ))
      ((= 4 l)
        (unless (= (ldb (byte 5 3) (car u)) #b11110) (utf8_to_unicode-error))
        (logior (ash (ldb (byte 3 0) (car u)) 18.)
                (ash (ldb (byte 6 0) (cadr u)) 12.)
                (ash (ldb (byte 6 0) (caddr u)) 6.)
                (ldb (byte 6 0) (cadddr u)) ))
      (t (utf8_to_unicode-error)) )))
;;
(defun uc-to-utf8 (uc) ;; uc = unicode
  (let (utf8)
    (cond
      ((< uc #x7f)
        (push uc utf8) )
      ((< uc #x7ff)
        (push (logior 128. (ldb (byte 6 0) uc)) utf8)
        (push (logior 192. (ldb (byte 5 6) uc)) utf8) )
      ((< uc #xffff)
        (dolist (i '(0 6))
          (push (logior 128. (ldb (byte 6 i) uc)) utf8) )
        (push (logior 224. (ldb (byte 4 12.) uc)) utf8) )
      (t
        (dolist (i '(0 6 12.))
          (push (logior 128. (ldb (byte 6 i) uc)) utf8) )
        (push (logior 240. (ldb (byte 3 18.) uc)) utf8) ))
    utf8 ))


;; Comparison - test functions - at Maxima level
;;
;;   GCL (maybe CMUCL): If code point is larger than 127 these functions throw an error.
;;
(defun $cequal          (mc1 mc2)  (char=         (l-char mc1) (l-char mc2)))
(defun $cequalignore    (mc1 mc2)  (char-equal    (l-char mc1) (l-char mc2)))
(defun $clessp          (mc1 mc2)  (char<         (l-char mc1) (l-char mc2)))
(defun $clesspignore    (mc1 mc2)  (char-lessp    (l-char mc1) (l-char mc2)))
(defun $cgreaterp       (mc1 mc2)  (char>         (l-char mc1) (l-char mc2)))
(defun $cgreaterpignore (mc1 mc2)  (char-greaterp (l-char mc1) (l-char mc2)))


;; Comparison - test functions - at Lisp level
;;
(defun cequal          (c1 c2)  (char=         c1 c2))
(defun cequalignore    (c1 c2)  (char-equal    c1 c2))
(defun clessp          (c1 c2)  (char<         c1 c2))
(defun clesspignore    (c1 c2)  (char-lessp    c1 c2))
(defun cgreaterp       (c1 c2)  (char>         c1 c2))
(defun cgreaterpignore (c1 c2)  (char-greaterp c1 c2))


;; Special Maxima characters
;;
(defmvar $newline
  (string #\newline) "Maxima newline character" string)
;;  
(defmvar $tab
  (string #\tab)     "Maxima tab character"     string)
;;  
(defmvar $space
  (string #\space)   "Maxima space character"   string)


(defun $tab () $tab) ;; returns Maxima tab character; can be autoloaded

;; -------------------------------------------------------------------------- ;;
;; 3. strings

;; Position indices in strings are 1-indexed, i.e.
;;
;;   charlist(str)[i] = charat(str, i), i >= 1.

;; -------------------------------------------------------------------------- ;;
;; 3.0 some tools for GCL/CMUCL reading UTF-8
;;
;; Remove the first n octets which form an UTF-8 character from a list of octets.
;; Values: 1. A reference to the rest of the list.
;;         2. The first n octets (we do not always need them). 
#+ (and (or cmucl gcl) unix)
(defun rm-first-utf-8-char (ol) ;; ol is an octet list of utf-8 coded characters
  (let ((oct (car ol)))
    (if (logbitp 7 oct)
      (if (logbitp 6 oct)
        (if (logbitp 5 oct)
          (if (logbitp 4 oct)
            (values (cddddr ol) (firstn 4 ol))
            (values (cdddr ol) (firstn 3 ol)) )
          (values (cddr ol) (firstn 2 ol)) )
        (gf-merror (intl:gettext "error while encoding")) )
      (values (cdr ol) (firstn 1 ol)) )))

;; Retrieve an UTF-8 character from a list of octets.
#+ (and (or cmucl gcl) unix)
(defun utf-8-m-char (len ol)
  (if (= len 1) 
    (string (code-char (car ol)))
    (map-into (make-string len) #'code-char ol) ))


;; With GCL/UTF-8 positions are counted in octets.
;; We want them in numbers of characters to find the right position in a string.
;; utf-8-pos-dec returns the decrement we need to adjust.
;;   (string position = octet position - decrement)
#+ (and (or cmucl gcl) unix)
(defun utf-8-pos-dec (str pos)
  (do ((ov (intl::string-to-octets str :iso8859-1))
       (i 0 (1+ i))
       (n 0) ) 
      ((= i pos) n)
    (when (= (logand (aref ov i) 192.) 128.)
      (incf n) )))

;; Fix start and end character positions according to given UTF-8 octets.
#+ (and (or cmucl gcl) unix)
(defun utf-8-fix-start-end (ov args) ;; args contain start and end positions.
  (let ((start (cadr args))
        (end (caddr args))
         inc )
    (setq inc (utf-8-pos-inc ov 0 start))
    (incf start inc)
    (rplaca (cdr args) start)
    (when end
      (incf end (+ inc (utf-8-pos-inc ov start end)))
      (rplaca (cddr args) end) )
    args ))

;; Compute the position increment we need to find the right octet position.
;;   (octet position = string position + increment)
#+ (and (or cmucl gcl) unix)
(defun utf-8-pos-inc (ov off pos) ;; begin to count at a given offset
  (do ((i off (1+ i))
       (pos0 pos)
        oct ) 
      ((= i pos) (- pos pos0))
    (setq oct (aref ov i))
    (when (and (logbitp 7 oct) (logbitp 6 oct))
      (if (logbitp 5 oct)
        (if (logbitp 4 oct) 
          (incf pos 3)
          (incf pos 2) )
        (incf pos) ))))

;; -------------------------------------------------------------------------- ;;
;; 3.1 functions for strings


(defun $stringp (obj) (stringp obj))


(defun $scopy (s) 
  (unless (stringp s) (s-error1 "scopy" ""))
  (copy-seq s) )


(defun $smake (n mc)
  (unless (integerp n)
    (gf-merror (intl:gettext "`smake': first argument must be an integer.")) )
  (unless ($charp mc)
    (gf-merror (intl:gettext "`smake': second argument must be a Maxima character.")) )
  #- (and (or cmucl gcl) unix) (make-string n :initial-element (character mc))
  #+ (and (or cmucl gcl) unix) 
    (if *read-utf-8*
      (make-string n :initial-element (character mc))
      (eval `(concatenate 'string ,@(make-list n :initial-element mc))) ))


(defun $charat (str pos)
  (unless (stringp str) (s-error1 "charat" "first"))
  (let ((end pos))
    (decf pos)
    (or (ignore-errors 
          #+ (and (or cmucl gcl) unix) 
            (unless *read-utf-8*
              (let* ((ov (intl::string-to-octets str :iso8859-1))
                     (args (utf-8-fix-start-end ov (list nil pos end))) )
                (setq pos (cadr args)
                      end (caddr args) )))
          (subseq str pos end) )
      (s-pos-error1 "charat" pos) )))


(defun $charlist (str)
  (unless (stringp str) (s-error1 "charlist" ""))
  (let ((cl (coerce str 'list)))
    (cons '(mlist) 
      #- (and (or cmucl gcl) unix) (mapcar #'string cl)
      #+ (and (or cmucl gcl) unix) 
        (if *read-utf-8* (mapcar #'string cl) (utf-8-charlist cl)) )))
;;
#+ (and (or cmucl gcl) unix) 
(defun utf-8-charlist (cl)
  (do ((ol (mapcar #'char-code cl)) 
        ch m-chars ) 
      ((null ol) (nreverse m-chars))
    (multiple-value-setq (ol ch) (rm-first-utf-8-char ol))
    (push (utf-8-m-char (length ch) ch) m-chars) ))


(putprop '$sexplode '$charlist 'alias)


;; $tokens is an interface to `tokens' by Paul Graham.
;;
;;   GCL/utf-8: Aside from $charp these test functions recognize only characters 
;;              which are coded in one single octet, i.e. $cint(mc) < 128.
;;
(defun $tokens (str &optional (test '$constituent))
  (unless (stringp str) (s-error1 "tokens" "first"))
  (setq test (stripdollar test))
  (unless (member test '(constituent alphanumericp alphacharp digitcharp 
                         lowercasep uppercasep charp ))
    (gf-merror (intl:gettext "`tokens': optional second argument must be one of ~%
constituent, alphanumericp, alphacharp, digitcharp, lowercasep, uppercasep, charp" )))
  (cons '(mlist) (tokens str test 0)) )
;;
(defun tokens (str test start) ;; this is the original version by Paul Graham
                               ;;   (ANSI Common Lisp, 1996, page 67)
  (let ((pos1 (position-if test str :start start)))
    (when pos1
      (let ((pos2 (position-if #'(lambda (ch) (not (funcall test ch)))
                               str 
                               :start pos1 )))
        (cons (subseq str pos1 pos2)
              (when pos2 (tokens str test pos2)) )))))
;;
;; test functions for $tokens on Lisp level:
;;
(defun constituent (ch) ;; Paul Graham - ANSI Common Lisp, 1996, page 67
  (and (graphic-char-p ch)
       (not (char= ch #\  ))))

(defun alphacharp    (ch)  (alpha-char-p ch))
(defun digitcharp    (ch)  (digit-char-p ch))
(defun lowercasep    (ch)  (lower-case-p ch))
(defun uppercasep    (ch)  (upper-case-p ch))
(defun charp         (ch)  (characterp   ch))
;;     characterp    (ch)  
;;     alphanumericp (ch)  no renaming needed


;; Split a string at an optional user defined delimiter string.
;; In case of a multiple delimiter string an optional flag may be set.
;;
(defun $split (str &rest args)
  (unless (stringp str) (s-error1 "split" "first"))
  (let ((ds " ")        ;; delimiter string
        (multiple? t) ) ;; treat multiple occurencies of ds as one?
    (dolist (a args)
      (cond
        ((stringp a) (setq ds a))
        ((member a '(t nil)) (setq multiple? a))
        (t (gf-merror (intl:gettext "`split': unsuitable optional arguments."))) ))
    (if (string= ds "")
      ($charlist str)
      (cons '(mlist) (split str ds multiple?)) )))
;;
(defun split (str ds &optional (multiple? t))
  (let ((pos1 (search ds str)))
    (if pos1
      (let ((ss (subseq str 0 pos1)) lst)
        (unless (and multiple? (string= ss ""))
          (push ss lst) )
        (do ((pos2 pos1) off) 
            ((null pos2) 
              (when (and multiple? (string= ss "")) 
                (push ss lst) )
              (nreverse lst) )
          (setq off (+ pos1 (length ds))
                pos2 (search ds str :start2 off)
                ss (subseq str off pos2)
                pos1 pos2 )
          (unless (and multiple? (string= ss ""))
            (push ss lst) )))
      (list str) )))


;; $sconcat for lists
;;
;;   optional: insert a user defined delimiter string
;; 
(defun $simplode (li &optional (ds ""))
  (unless (listp li)
    (gf-merror (intl:gettext "`simplode': first argument must be a list.")) )
  (unless (stringp ds) 
    (s-error1 "simplode" "optional second") )
  (setq li (cdr li))
  (cond 
    ((string= ds "")
      (reduce #'$sconcat li) )
    (t
      (do (acc) (())
        (push ($sconcat (pop li)) acc)
        (when (null li)
          (return (eval `(concatenate 'string ,@(nreverse acc)))) )
        (push ds acc) ))))


(defun $slength (str)
  (unless (stringp str) (s-error1 "slength" ""))
  #- (and (or cmucl gcl) unix) (length str)
  #+ (and (or cmucl gcl) unix) 
    (if *read-utf-8* (length str) (utf-8-slength str)) )
;;
;; GCL: if we don't know the number of non-ascii characters, we have to count
#+ (and (or cmucl gcl) unix) 
(defun utf-8-slength (str)
  (do* ((ov (intl::string-to-octets str :iso8859-1)) 
        (i 0 (1+ i))
        (n 0)
        (len (array-dimension ov 0)) ) 
       ((= i len) n)
    (when (/= (logand (aref ov i) 192.) 128.)
      (incf n) )))


(defun $sposition (mc str)
  (unless (and (stringp mc) 
               (= 1 #- (and (or cmucl gcl) unix) (length mc)
                    #+ (and (or cmucl gcl) unix) ($slength mc) ))
    (gf-merror (intl:gettext 
      "`sposition': first argument must be a Maxima character." )))
  (unless (stringp str)
    (s-error1 "sposition" "second") )
  #- (and (or cmucl gcl) unix) 
    (let ((pos (position (character mc) str))) 
      (when pos (1+ pos)) )
  #+ (and (or cmucl gcl) unix) 
    (if *read-utf-8*
      (let ((pos (position (character mc) str))) 
        (when pos (1+ pos)) )
      ($ssearch mc str) ))


(defun $sreverse (str)
  (unless (stringp str) (s-error1 "sreverse" ""))
  #- (and (or cmucl gcl) unix) (reverse str)
  #+ (and (or cmucl gcl) unix) 
    (if *read-utf-8* (reverse str) (utf-8-sreverse str)) )
;;
#+ (and (or cmucl gcl) unix) 
(defun utf-8-sreverse (str)
  (do ((ol (mapcar #'char-code (coerce str 'list)))
        ch m-chars )
      ((null ol) 
        (eval `(concatenate 'string ,@m-chars)) )
    (multiple-value-setq (ol ch) (rm-first-utf-8-char ol))
    (push (utf-8-m-char (length ch) ch) m-chars) ))


(defun $substring (str start &optional (end nil))
  (unless (stringp str) (s-error1 "substring" "first"))
  (decf start)
  (when end (decf end))
  (or (ignore-errors 
        #+ (and (or cmucl gcl) unix) 
          (unless *read-utf-8*
            (let* ((ov (intl::string-to-octets str :iso8859-1))
                   (args (utf-8-fix-start-end ov (list nil start end))) )
              (setq start (cadr args)
                    end (caddr args) )))
        (subseq str start end) )
      (s-pos-error2 "substring") ))


;; comparison - test functions - at Maxima and Lisp level
;;
;;   for the sake of efficiency omit checkings here
;;
(defun $sequal (s1 s2) (string= s1 s2))
;;
(defun $sequalignore (s1 s2) (string-equal s1 s2))


(defun $smismatch (s1 s2 &optional (test '$sequal))
  (unless (and (stringp s1) (stringp s2)) (s-error2 "smismatch" "first two"))
  (unless (member test '($sequal $sequalignore))
    (gf-merror (intl:gettext 
      "`smismatch': optional third argument must be `sequal' or `sequalignore'." )))
  (let ((pos (mismatch s1 s2 :test test)))
    (when pos 
      #- (and (or cmucl gcl) unix) (1+ pos)
      #+ (and (or cmucl gcl) unix) 
        (if *read-utf-8* 
          (1+ pos) 
          (- (1+ pos) (utf-8-pos-dec s1 pos)) ))))


;; searching
;;
;;   the optional args are test, start, end (see s-optional-args below)
;;
(defun $ssearch (seq str &rest args) 
  (unless (and (stringp seq) (stringp str)) (s-error2 "ssearch" "first two"))
  (setq args (s-optional-args "ssearch" str args))
  (or (ignore-errors 
        (let ((pos (apply #'ssearch `(,seq ,str ,@args))))
          (if pos 
            #- (and (or cmucl gcl) unix) (1+ pos)
            #+ (and (or cmucl gcl) unix) 
              (if *read-utf-8* 
                (1+ pos) 
                (- (1+ pos) (utf-8-pos-dec str pos)) )
            (return-from $ssearch nil) )))
      (s-pos-error2 "ssearch") ))
;;
(defun ssearch (seq str &optional (test '$sequal) (start 0) (end nil))
  (search seq str :test test :start2 start :end2 end) )


;; allow arbitrary order of the optional args test, start, end 
;;  (where start is of course the first integer in sequence)
;;
(defun s-optional-args (name str args) 
  #- (and (or cmucl gcl) unix) (declare (ignore str))
  (let ((test '$sequal) 
        (start 1) 
        (end nil) 
        (i 0) )
    (dolist (a args)
      (cond
        ((and (= i 0) (integerp a))               (setq start a i 1))
        ((and (= i 1) (or (integerp a) (null a))) (setq end   a i 2))
        ((member a '($sequal $sequalignore))      (setq test  a    ))
        (t (gf-merror (intl:gettext "~m: unsuitable optional arguments.") name)) ))
    (setq args (list test (1- start) (if end (1- end) nil)))
    #+ (and (or cmucl gcl) unix) 
      (unless *read-utf-8*
        (or (ignore-errors 
              (setq args 
                (utf-8-fix-start-end (intl::string-to-octets str :iso8859-1) args) ))
            (s-pos-error2 name) ))
    args ))


;; functions for string manipulation


(defun $ssubstfirst (new old str &rest args)
  (unless (every #'stringp `(,new ,old ,str)) 
    (s-error2 "ssubstfirst" "first three") )
  (setq args (s-optional-args "ssubstfirst" str args))
  (or (ignore-errors 
        (apply #'ssubstfirst `(,new ,old ,str ,@args)) )
      (s-pos-error2 "ssubstfirst") ))
;;
(defun ssubstfirst (new old str &optional (test '$sequal) (start 0) (end nil))
  (let ((len (length old))
        (pos (search old str :test test :start2 start :end2 end)) )
    (if (null pos)
      str
      (concatenate 'string (subseq str 0 pos) new (subseq str (+ pos len))) )))
       

(defun $ssubst (new old str &rest args)
  (unless (every #'stringp `(,new ,old ,str)) 
    (s-error2 "ssubst" "first three") )
  (setq args (s-optional-args "ssubst" str args))
  (or (ignore-errors 
        (apply #'ssubst `(,new ,old ,str ,@args)))
      (s-pos-error2 "ssubst") ))
;;
(defun ssubst (new old str &optional (test '$sequal) (start 0) (end nil))
  (let ((pos (search old str :test test :start2 start :end2 end)))
    (if (null pos)
       str
       (ssubst new
               old
               (ssubstfirst new old str test pos end)
               test
               (+ pos (length new))
               (when end (- (+ end (length new)) (length old))) ))))


(defun $sremovefirst (seq str &rest args)
  (unless (and (stringp seq) (stringp str))
    (s-error2 "sremovefirst" "first two") )
  (setq args (s-optional-args "sremovefirst" str args))
  (or (ignore-errors 
        (apply #'sremovefirst `(,seq ,str ,@args)) )
      (s-pos-error2 "sremovefirst") ))
;;
(defun sremovefirst (seq str &optional (test '$sequal) (start 0) (end nil))
  (let* ((len (length seq))
         (pos (search seq str :test test :start2 start :end2 end))
         (sq1 (subseq str 0 pos))
         (sq2 (if pos (subseq str (+ pos len)) "")) )
    (concatenate 'string sq1 sq2)))


(defun $sremove (seq str &rest args)
  (unless (and (stringp seq) (stringp str)) (s-error2 "sremove" "first two"))
  (setq args (s-optional-args "sremove" str args))
  (or (ignore-errors 
        (apply #'sremove `(,seq ,str ,@args)) )
      (s-pos-error2 "sremove") ))
;;
(defun sremove (seq str &optional (test '$sequal) (start 0) (end nil))
  (let ((pos (search seq str :test test :start2 start :end2 end)))
    (do () 
        ((null pos) str)
      (setq str (sremovefirst seq str test pos end))
      (when end (decf end (length seq)))
      (setq pos (search seq str :test test :start2 pos :end2 end))  )))


(defun $sinsert (seq str pos)
  (decf pos)
  (unless (and (stringp seq) (stringp str)) (s-error2 "sinsert" "first two"))
  (or (ignore-errors 
        #+ (and (or cmucl gcl) unix) 
          (unless *read-utf-8*
            (incf pos (utf-8-pos-inc (intl::string-to-octets str :iso8859-1) 0 pos)) )
        (let ((sq1 (subseq str 0 pos))
              (sq2 (subseq str pos)) )
          (concatenate 'string sq1 seq sq2) ))
      (s-pos-error1 "sinsert" (1+ pos)) ))


(defun $ssort (str &optional (test '$clessp))
  (unless (stringp str) (s-error1 "ssort" "first"))
  #+ (and (or cmucl gcl) unix) 
    (unless (or *read-utf-8* (string= test '$clessp))
      (let ((alt #+gcl "" #+cmucl "and the external format is not adjusted to UTF-8"))
        (gf-merror (intl:gettext 
          "`ssort': when us_ascii_only is false ~a the optional second argument must be `clessp'." ) alt )))
  (unless 
    (member test '($clessp $cgreaterp $cequal $clesspignore $cgreaterpignore $cequalignore))
      (gf-merror (intl:gettext 
        "`ssort': optional second argument must be one of ~% 
clessp[ignore], cequal[ignore], cgreaterp[ignore]" )))
  (setq test (stripdollar test))
  (let ((copy (copy-seq str)))
    #- (and (or cmucl gcl) unix) (stable-sort copy test)
    #+ (and (or cmucl gcl) unix) 
      (if *read-utf-8* (stable-sort copy test) (utf-8-ssort copy)) ))
;;
#+ (and (or cmucl gcl) unix) 
(defun utf-8-ssort (str)
  (labels ((l< (a b)
            (cond 
              ((null a) (not (null b)))
              ((null b) nil)
              ((= (car a) (car b)) (l< (cdr a) (cdr b)))
              (t (< (car a) (car b))) )))
    (do ((ol (mapcar #'char-code (coerce str 'list))) 
          ch chars ) 
        ((null ol) 
          (eval 
            `(concatenate 'string
              ,@(mapcar #'(lambda (ch) (utf-8-m-char (length ch) ch))
                        (sort chars #'l<) ))))
      (multiple-value-setq (ol ch) (rm-first-utf-8-char ol))
      (push ch chars) )))


(defun $strim (seq str)
  (unless (and (stringp seq) (stringp str)) (s-error2 "strim" ""))
  (string-trim seq str) )

(defun $striml (seq str)
  (unless (and (stringp seq) (stringp str)) (s-error2 "striml" ""))
  (string-left-trim seq str) )

(defun $strimr (seq str)
  (unless (and (stringp seq) (stringp str)) (s-error2 "strimr" ""))
  (string-right-trim seq str) )


(defun $supcase (str &optional (start 1) (end nil))
  (change-case str "supcase" #'string-upcase start end) )

(defun $sdowncase (str &optional (start 1) (end nil))
  (change-case str "sdowncase" #'string-downcase start end) )

(defun change-case (str name sfun start end)
  (unless (stringp str) (s-error1 name "first"))
  (decf start)
  (when end (decf end))
  (or (ignore-errors 
        #- (and (or cmucl gcl) unix) (funcall sfun str :start start :end end)
        #+ (and (or cmucl gcl) unix) 
          (if *read-utf-8*
            (funcall sfun str :start start :end end)
            (let* ((ov (intl::string-to-octets str :iso8859-1))
                   (args (utf-8-fix-start-end ov (list nil start end))) )
              (funcall sfun str :start (cadr args) :end (caddr args)) )))
      (s-pos-error2 name) ))


(defun $sinvertcase (str &optional (start 1) (end nil))
  (unless (stringp str) (s-error1 "sinvertcase" "first"))
  (decf start)
  (when end (decf end))
  (or (ignore-errors 
        #+ (and (or cmucl gcl) unix) 
          (unless *read-utf-8*
            (let* ((ov (intl::string-to-octets str :iso8859-1))
                   (args (utf-8-fix-start-end ov (list nil start end))) )
              (setq start (cadr args)
                    end (caddr args) )))
        (let ((sq1 (subseq str 0 start))
              (sq2 (s-invert-case (subseq str start end)))
              (sq3 (if end (subseq str end) "")) )
          (concatenate 'string sq1 sq2 sq3) ))
      (s-pos-error2 "sinvertcase") ))
;;
(defun s-invert-case (str)
  (concatenate 'string 
    (mapcar #'(lambda (s) (c-invert-case (character s)))
            (coerce str 'list) )))
;;
(defun c-invert-case (ch) 
  (if (upper-case-p ch)
    (char-downcase ch)
    (char-upcase ch) ))

;; -------------------------------------------------------------------------- ;;

