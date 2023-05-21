;;
;;                                  ~*~  STRINGPROC  ~*~
;;
;; Maxima string processing
;;
;; Copyright     : 2005-2016 Volker van Nek
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


(declare-top (special *parse-utf-8-input*))
#|
In an application which encodes characters in UTF-8 and the external format of 
the Lisp reader is set to e.g. cp1252 (or has no format definition like in GCL)
a string of length 1 which contains a non-us-ascii character like an umlaut 
is read as a sequence of two or more octets, e.g. auml -> #(195 164), and 
misinterpreted as a sequence of two or more characters.

When the flag *parse-utf-8-input* is set to true the string processing 
functions in stringproc.lisp and sregex.lisp decode and restructure octet 
sequences in a way that octet groups like '(195 164) are coded back into the 
original string of length 1.

In addition to this position indices in strings are fixed accordingly.

Functions like $alphacharp which need a Lisp character representation of a 
Maxima character do not work for non-us-ascii characters, e.g. auml is not 
recognized as alphabetic.

See comments to $adjust_external_format below for a detailed description.
|#

;; adjust the external format where necessary and possible
;;
#-gcl (eval-when (:load-toplevel :execute)
  ;;
  #+ (and clisp (not unix))
    (when (boundp 'maxima::$wxplot_size)
      (setf custom:*terminal-encoding* 
        (ext:make-encoding :charset (symbol-name :utf-8) :line-terminator :dos) ))
)

;; find the right value for *parse-utf-8-input*
;;
(defun init-*parse-utf-8-input* ()
  #+unix
  (progn
    #+gcl t
    #-(or gcl) nil
    )
  #-unix (progn
    #+gcl (boundp 'maxima::$wxplot_size)  ;; we are in wxMaxima
    #+ccl nil
    #- (or ccl gcl)
      (and (boundp 'maxima::$wxplot_size) ;; we are in wxMaxima and 
           (not (search "UTF"             ;; the external format is not utf-8 (e.g. SBCL)
                        (format nil "~s" (stream-external-format *standard-output*))
                        :test 'string-equal ))) ))

(defvar *parse-utf-8-input* 
  (init-*parse-utf-8-input*)
  "Maxima itself parses the utf-8 input." )


;; when *parse-utf-8-input* is t read raw bytes according to the default external format
;;
(defun string-to-raw-bytes (str) 
  (intl::string-to-octets str #+cmucl :iso8859-1 ;; GNU/Linux terminal             ;; should not be needed
                              #-cmucl :cp1252 )) ;; wxMaxima on Windows (GCL,SBCL) ;; CLISP: should not be needed
                                                 ;; and GCL in GNU/Linux (GCL ignores encoding arg)


;; Setting this flag to true saves Maxima from parsing UTF-8 encoding.
;;   (If possible adjusting the external format should be preferred.)
;;
(defvar $us_ascii_only nil "Promise to use only US-ASCII characters.")
;;
(putprop '$us_ascii_only 'set-*parse-utf-8-input* 'assign)
;;
(defun set-*parse-utf-8-input* (assign-var arg) 
  (declare (ignore assign-var))
  (setq *parse-utf-8-input* 
    (if arg nil (init-*parse-utf-8-input*)) ))


;; -------------------------------------------------------------------------- ;;
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
;; -------------------------------------------------------------------------- ;;


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
  #+gcl (open file :direction :output :if-exists :append :if-does-not-exist :create)
  #-gcl (let (encoding-to-use inferred-encoding encoding-from-argument)
          (declare (ignorable inferred-encoding encoding-from-argument))
          (if enc
            (setq encoding-to-use (setq encoding-from-argument (get-encoding enc "opena")))
            (progn
              (setq inferred-encoding (unicode-sniffer file))
              (if inferred-encoding
                (let ((checked-encoding (check-encoding inferred-encoding)))
                  (when (null checked-encoding)
                    (merror (intl:gettext "opena: inferred encoding ~M for file ~M is not recognized by this Lisp implementation.") inferred-encoding file))
                  (when (eq checked-encoding 'unknown)
                    (mtell (intl:gettext "opena: warning: I don't know how to verify encoding for this Lisp implementation."))
                    (mtell (intl:gettext "opena: warning: go ahead with inferred encoding ~M and hope for the best.") inferred-encoding))
                  (setq encoding-to-use inferred-encoding))
                (setq encoding-to-use (setq encoding-from-argument (get-encoding enc "opena"))))))
          (open file :direction :output :if-exists :append :if-does-not-exist :create :external-format encoding-to-use)))


(defun $openr (file &optional enc) 
  #+gcl (declare (ignore enc))
  (unless (stringp file) (s-error1 "openr" "the"))
  (unless (probe-file file)
    (gf-merror (intl:gettext "`openr': file does not exist: ~m") file) )
  #+gcl (open file)
  #-gcl (let (encoding-to-use inferred-encoding encoding-from-argument)
          (declare (ignorable inferred-encoding encoding-from-argument))
          (if enc
            (setq encoding-to-use (setq encoding-from-argument (get-encoding enc "openr")))
            (progn
              (setq inferred-encoding (unicode-sniffer file))
              (if inferred-encoding
                (let ((checked-encoding (check-encoding inferred-encoding)))
                  (when (null checked-encoding)
                    (merror (intl:gettext "openr: inferred encoding ~M for file ~M is not recognized by this Lisp implementation.") inferred-encoding file))
                  (when (eq checked-encoding 'unknown)
                    (mtell (intl:gettext "openr: warning: I don't know how to verify encoding for this Lisp implementation."))
                    (mtell (intl:gettext "openr: warning: go ahead with inferred encoding ~M and hope for the best.") inferred-encoding))
                  (setq encoding-to-use inferred-encoding))
                (setq encoding-to-use (setq encoding-from-argument (get-encoding enc "openr"))))))
          (let ((s (open file :external-format encoding-to-use)))
            (when (eql (peek-char nil s nil) #+clisp #\ZERO_WIDTH_NO-BREAK_SPACE 
                                             #+(or abcl sbcl) #\UFEFF 
                                             #-(or clisp abcl sbcl) #\U+FEFF)
              (read-char s))
            s)))


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


;; -- get or set a suitable encoding (hopefully) ---------------------------- ;;
;;
(defun get-encoding (enc name) 
  (cond
    (enc ;; set encoding:
     (unless (stringp enc) 
       (gf-merror (intl:gettext 
		   "`~m': the optional second argument must be a string." ) name ))

     ;; All Lisps must recognize :default, per CLHS.
     (when (string= enc "DEFAULT")
       (return-from get-encoding :default))

     (setq enc (intern (string-upcase enc) :keyword))
     ;;
     #+ccl
     (progn
       #+unix enc
       #-unix
       (cond 
         ((boundp 'maxima::$wxplot_size) enc)
         (t (is-ignored enc name "to get some help") 
            :utf-8 )))
     ;;
     #+clisp
     (progn
       #+unix
       (ext:make-encoding :charset (symbol-name enc) :line-terminator :unix)
       #-unix
       (let ((ef (stream-external-format *standard-input*)))
         (cond 
           ((search "UTF" (format nil "~s" ef) :test 'string-equal)
            (ext:make-encoding :charset (symbol-name enc) :line-terminator :dos) )
           (t (is-ignored enc name "to enable the encoding argument")
              ef))))
     ;;
     #+cmucl
     (progn
       #+unix
       (let ((ef (stream-external-format *standard-output*))) ;; input format remains 'default'
         (cond 
           ((check-encoding enc) enc)
           (t (is-ignored enc name "to enable the encoding argument") 
              ef)))
       #-unix
       enc)
     ;;
     #+gcl
     (format t "`~a': GCL ignores the argument ~s.~%" name 
             (string-downcase (symbol-name enc)) )
     ;;
     #+sbcl
     (progn
       #+unix enc
       #-unix
       (let ((ef (stream-external-format *standard-input*)))
         (cond 
           ((eq ef :cp1252) 
            (is-ignored enc name "to enable the encoding argument")
            ef)
           (t enc))))
     ;;
     #-(or ccl clisp cmucl gcl sbcl)
     enc) ;; ECL and others
    ;;
    (t ;; get encoding:
     #+ (or ecl ccl gcl)
     :utf-8 ;; ignored by GCL
     #+sbcl
     sb-impl::*default-external-format*
     #+cmucl
     stream:*default-external-format*
     #-(or ecl ccl gcl sbcl cmucl)
     (stream-external-format *standard-output*))))


(defun is-ignored (enc name adds)
  (format t "`~a': The argument ~s is ignored. Enter~%" name 
    (string-downcase (symbol-name enc)) )
  (format t "adjust_external_format();~%~a.~%" adds) )


;; -- adjust the external format -------------------------------------------- ;; 
#|
Linux/Unix:

          terminal, GUI                Lisp reader            string_to_octets
string_0 ----encode----> UTF-8-octets ----decode----> string_1 ----encode----> octets

wMaxima, Xmaxima and commonly used terminals read characters in UTF-8 
which means the characters are encoded as UTF-8-octets.

From the Lisp's point of view this is the external format of the input.
Any Lisp reader should read and decode this input in UTF-8 too.
Then all characters are read as they are entered in a GUI or terminal. 
string_0 is equal to string_1. This is necessary for all stringproc functions 
like e.g. cryptools.lisp/string_to_octets to work properly.

UTF-8 is the default external format for SBCL, CLISP, ECL, CCL and CMUCL(GUI).

GCL has no format definition and Maxima itself parses the UTF-8 octets.

By default CMUCL uses ISO8859-1 in a terminal. The format is changed to UTF-8 
when loading stringproc.lisp. 

So in GNU/Linux adjust_external_format prints a message and does nothing.

Observations based on Maxima 5.37post from git (Feb 2016).

Windows:

Like in Linux the external format of the Lisp reader should meet the format 
used by the terminal resp. by the GUI. 

If the terminal uses cp850 it should be set to cp1252 (or ISO-8859-1).
The font should be set to true type. Both changes enable the full range of 
cp1252 (resp. ISO-8859-1) and are assumed in the following.

CCL(terminal) reads UTF-8 and the input from terminal is (assumed to be) ISO-8859-1. 
  The UTF-8 reader misinterprets codepoints > 127. Adjustment needed. 
  Switch to ISO-8859-1 via Lisp option in 'maxima.bat'. 
  (CCL does not support cp1252. Both encodiings should be iso8859-1.)

CCL(wxMaxima) reads UTF-8 and the input from wxMaxima is UTF-8. Do nothing.

CLISP(terminal) reads cp1252 and the input is (assumed to be) cp1252. Do nothing.

CLISP(wxMaxima) reads cp1252 and the input is UTF-8. 
  cp1252 is changed to to UTF-8 when loading stringproc.lisp. Nothing left to do.

GCL has no format definition. Input from terminal and wxMaxima is (assumed to be) cp1252. Do nothing.

SBCL(terminal) reads UCS-2LE and the input is UCS-2LE. Do nothing.

SBCL(wxMaxima) reads cp1252 but the input is UTF-8. Adjustment needed. 
  Switch to UTF-8 via Lisp command in init file.
  Update (Maxima 5.40.0): SBCL(wxMaxima) reads UTF-8 and the input is UTF-8. Do nothing.

Observations based on Maxima 5.36.1(ccl), 5.37.2/5.40.0(clisp), 5.37.3(gcl), 
5.37.2/5.40.0(sbcl) in Windows 7.

TODO: Comments on Xmaxima in Windows.
|#

(defun $adjust_external_format () 
  #+ccl
  (progn
    #+unix
    (format t "The external format is utf-8 and has not been changed.~%")
    #-unix
    (let ((ef (stream-external-format *standard-input*)))
      (format t "The external format is ~a and has not been changed.~%" ef)
      (unless (boundp 'maxima::$wxplot_size)
        (format t "Command line: The external format is settable by an option in~%~a~%"
		(combine-path *maxima-prefix* "bin" "maxima.bat") )
        (format t "Change the line~%set lisp_options=~%to~%") 
        (format t "set lisp_options=-K :iso-8859-1~%") 
        (format t "(cp850 and cp1252 are not supported by CCL.)~%")
        (use-cp "iso-8859-1" 28591) )))
  ;;
  #+clisp
  (let ((ef (stream-external-format *standard-input*)))
    #-unix
    (cond 
      ((search "UTF" (format nil "~s" ef) :test 'string-equal)
       (format t "The external format is ~a.~%and has not been changed.~%" ef) )
      ((boundp 'maxima::$wxplot_size)
       ;; this should not happen
       ;; format should be adjusted when loading stringproc.lisp
       (format t "The external format has been changed to ~a~%"
               (setf custom:*terminal-encoding* 
		     (ext:make-encoding :charset (symbol-name :utf-8) :line-terminator :dos) ))
       (format t "for this session only. For a permanent change put the lines~%")
       (format t "(setf custom:*terminal-encoding*~%")
       (format t "  (ext:make-encoding :charset (symbol-name :utf-8) :line-terminator :dos) )~%")
       (format t "into the init file .clisprc in your home directory. ")
       (format t "The file is probably~%~a~%" (combine-path *maxima-tempdir* ".clisprc"))
       (setq *parse-utf-8-input* nil)
       t )
      (t
       (format t "The external format is ~a~%and has not been changed.~%" ef)
       (use-cp "cp1252" 1252) )) 
    #+unix
    (format t "The external format is ~a~%and has not been changed.~%" ef) )
  ;;
  #+cmucl
  (let ((ef (stream-external-format *standard-output*))) ;; format of ..
    (cond ;; .. *standard-input* might be 'default'
      ((eq ef :utf-8) 
       (format t "The external format is ~a~%and has not been changed.~%" ef) )
      (t ;; this should not happen
       ;; format should be adjusted when loading stringproc.lisp
       (format t "The external format has been changed to utf-8~%")
       (format t "for this session only. For a permanent change put the line~%")
       (format t "(stream:set-system-external-format :utf-8)~%")
       (format t "into the init file .cmucl-init in your home directory. ")   
       (format t "The file is probably~%~a~%" (combine-path *maxima-tempdir* ".cmucl-init"))
       (setq *parse-utf-8-input* nil)
       (stream:set-system-external-format :utf-8) ))) ;; returns t
  ;;
  #+ecl
  (format t "The external format is ~a~%and has not been changed.~%"
	  (stream-external-format *standard-input*) )
  ;;
  #+gcl
  (progn
    (format t "There is no settable external format.~%")
    #-unix
    (use-cp "cp1252" 1252) )
  ;;
  #+sbcl
  (let ((ef (stream-external-format *standard-input*)))
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
         (format t "The external format is cp1252 and has not been changed.~%")
         (format t "The line~%~a~%has been appended to the init file~%~a~%" cmd path)
         (format t "Please restart Maxima to change the external format to utf-8.~%") ))
      (t
       (format t "The external format is ~a~%and has not been changed.~%" ef) )))
  ;;
  #-(or ccl clisp cmucl ecl gcl sbcl) ;; all others
    (format t "Please file a report if adjusting the external format seems necessary.~%") )


(defun use-cp (name id) 
  (format t "Command line: To change the terminal encoding to ~a insert a line~%" name)
  (format t "chcp ~a~%immediately below of '@echo off' in~%~s~%" 
    id (combine-path *maxima-prefix* "bin" "maxima.bat") )
  (format t "and in the properties of the terminal window set the font to a true type font.~%") )


;; -------------------------------------------------------------------------- ;;
;; 2. characters

;; A Maxima character is a string of length 1. (Lisp strings are Maxima strings.)


;; Check if object is a Maxima character.
;;
(defun $charp (obj)
  (and (stringp obj) 
       (= 1 (if *parse-utf-8-input* (utf-8-slength obj) (length obj)) )))


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
;;  These functions assume that we know what alphabetic characters are.
;;  If mc is a non-US-ASCII character and we don't have Unicode support 
;;  i.e. *parse-utf-8-input* is t, an error is thrown via l-char.
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
;;  GCL: 
;;    A non-ASCII-character is encoded in UTF-8 by wxMaxima or a Linux terminal.
;;    GCL just passes them through octet by octet. Process these octets.
;;
;;  CMUCL (Linux, wxMaxima): 
;;    $cint recognizes 16 bit characters only. 
;;    utf8_to_unicode(string_to_octets(mc)); works where $cint fails.
;;
;;  SBCL (Windows, wxMaxima): 
;;    It is assumed that the external format has been adjusted to UTF-8.
;;
(defun $cint (mc) 
  (unless ($charp mc)
    (gf-merror (intl:gettext "`cint': argument must be a Maxima character.")) )
  (mc2int mc) ) 
;;
(defun mc2int (mc) 
  (if *parse-utf-8-input*
    (ignore-errors ;; arguments larger than 16 bit might cause errors
      (utf8-to-uc (coerce (string-to-raw-bytes mc) 'list)) )
    (char-code (character mc)) ))
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
;; In allegro, cmuc code points and names are limited to 16 bit.
;; abcl, ccl, gcl, lispworks: unicode(name) returns false.
;; octets_to_string(unicode_to_utf8(code_point)); often works where unicode(code_point) fails.
;;
(defun $unicode (arg) 
  (cond
    ((integerp arg)
      (ignore-errors ;; arguments larger than 16 bit might cause errors
        (if *parse-utf-8-input*
          (let ((ol (uc-to-utf8 arg)))
            (utf-8-m-char (length ol) ol) )
          (string (code-char arg)) )))
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
(defun $cequal (mc1 mc2) 
  (if *parse-utf-8-input*
    (= (mc2int mc1) (mc2int mc2))
    (char= (l-char mc1) (l-char mc2)) ))
;;
(defun $clessp (mc1 mc2)  
  (if *parse-utf-8-input*
    (< (mc2int mc1) (mc2int mc2))
    (char< (l-char mc1) (l-char mc2)) ))
;;
(defun $cgreaterp (mc1 mc2)  
  (if *parse-utf-8-input*
    (> (mc2int mc1) (mc2int mc2))
    (char> (l-char mc1) (l-char mc2)) ))
;;
;;  Ignoring case assumes alphabetic characters. But we can't check for 
;;  non-US-ASCII alphabetic characters when we don't have Unicode support and 
;;  *parse-utf-8-input* is t. Throw an error via l-char for non-US-ASCII chars.
;;
(defun $cequalignore    (mc1 mc2)  (char-equal    (l-char mc1) (l-char mc2))) 
(defun $clesspignore    (mc1 mc2)  (char-lessp    (l-char mc1) (l-char mc2)))
(defun $cgreaterpignore (mc1 mc2)  (char-greaterp (l-char mc1) (l-char mc2)))


;; Comparison - test functions - at Lisp level
;;
(defun cequal          (c1 c2)  (char=         c1 c2)) ;; Lisp chars assumed 
(defun clessp          (c1 c2)  (char<         c1 c2))
(defun cgreaterp       (c1 c2)  (char>         c1 c2))
;;
(defun cequalignore    (c1 c2)  (char-equal    c1 c2))
(defun clesspignore    (c1 c2)  (char-lessp    c1 c2))
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
;; 3.0 tools for parsing UTF-8 encoded strings
;;
;; Remove the first n octets which form an UTF-8 character from a list of octets.
;; Values: 1. A reference to the rest of the list.
;;         2. The first n octets (we do not always need them). 
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
(defun utf-8-m-char (len ol)
  (if (= len 1) 
    (string (code-char (car ol)))
    (map-into (make-string len) #'code-char ol) ))


;; We want positions in numbers of characters (not just octets) to find the 
;;   right position in a string.
;; utf-8-pos-dec returns the decrement we need to adjust.
;;   (string position = octet position - decrement)
(defun utf-8-pos-dec (str pos)
  (do ((ov (string-to-raw-bytes str))
       (i 0 (1+ i))
       (n 0) ) 
      ((= i pos) n)
    (when (= (logand (aref ov i) 192.) 128.)
      (incf n) )))

;; Fix start and end character positions according to given UTF-8 octets.
(defun utf-8-fix-start-end (ov args) ;; args contain start and end positions.
  (let ((start (cadr args))
        (end (caddr args))
         inc )
    (setq inc (utf-8-pos-inc ov 0 start))
    (incf start inc)
    (rplaca (cdr args) start)
    (when end
      (incf end inc) 
      (incf end (utf-8-pos-inc ov start end))
      (rplaca (cddr args) end) )
    args ))

;; Compute the position increment we need to find the right octet position.
;;   (octet position = string position + increment)
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
  (if *parse-utf-8-input*
    (reduce #'$sconcat (make-list n :initial-element mc)) 
    (make-string n :initial-element (character mc)) ))


(defun $charat (str pos)
  (unless (stringp str) (s-error1 "charat" "first"))
  (let ((end pos))
    (decf pos)
    (or (ignore-errors 
          (when *parse-utf-8-input*
            (let* ((ov (string-to-raw-bytes str))
                   (args (utf-8-fix-start-end ov (list nil pos end))) )
              (setq pos (cadr args)
                    end (caddr args) )))
          (subseq str pos end) )
      (s-pos-error1 "charat" (1+ pos)) ))) 


(defun $charlist (str)
  (unless (stringp str) (s-error1 "charlist" ""))
  (let ((cl (coerce str 'list)))
    (cons '(mlist simp) 
      (if *parse-utf-8-input* (utf-8-charlist cl) (mapcar #'string cl)) )))
;;
(defun utf-8-charlist (cl)
  (do ((ol (mapcar #'char-code cl)) 
        ch m-chars ) 
      ((null ol) (nreverse m-chars))
    (multiple-value-setq (ol ch) (rm-first-utf-8-char ol))
    (push (utf-8-m-char (length ch) ch) m-chars) ))


(putprop '$sexplode '$charlist 'alias)


;; $tokens is an interface to `tokens' by Paul Graham.
;;
;;   When *parse-utf-8-input* is t 
;;     then aside from $charp the test functions recognize us-ascii characters only.
;;
(defun $tokens (str &optional (test '$constituent))
  (unless (stringp str) (s-error1 "tokens" "first"))
  (setq test (stripdollar test))
  (unless (member test '(constituent alphanumericp alphacharp digitcharp 
                         lowercasep uppercasep charp ))
    (gf-merror (intl:gettext "`tokens': optional second argument must be one of ~%
constituent, alphanumericp, alphacharp, digitcharp, lowercasep, uppercasep, charp" )))
  (cons '(mlist simp) (tokens str test 0)) ) 
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
      (cons '(mlist simp) (split str ds multiple?)) ))) 
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
    ((null li)
      ($sconcat) )
    ((null (cdr li))
      ($sconcat (car li)) )
    ((string= ds "")
      (reduce #'$sconcat li) )
    (t
      (do (acc) (())
        (push ($sconcat (pop li)) acc)
        (when (null li)
          (return (reduce #'(lambda (s0 s1) (concatenate 'string s0 s1)) (nreverse acc) :initial-value "")))
        (push ds acc) ))))


(defun $slength (str)
  (unless (stringp str) (s-error1 "slength" ""))
  (if *parse-utf-8-input* (utf-8-slength str) (length str)) )
;;
;; if we don't know the number of non-ascii characters, we have to count
(defun utf-8-slength (str)
  (do* ((ov (string-to-raw-bytes str))
        (i 0 (1+ i))
        (n 0)
        (len (array-dimension ov 0)) ) 
       ((= i len) n)
    (when (/= (logand (aref ov i) 192.) 128.)
      (incf n) )))


(defun $sposition (mc str)
  (unless (and (stringp mc) 
               (= 1 (if *parse-utf-8-input* ($slength mc) (length mc))) )
    (gf-merror (intl:gettext 
      "`sposition': first argument must be a Maxima character." )))
  (unless (stringp str)
    (s-error1 "sposition" "second") )
  (if *parse-utf-8-input*
    ($ssearch mc str) 
    (let ((pos (position (character mc) str))) 
      (when pos (1+ pos)) )))


(defun $sreverse (str)
  (unless (stringp str) (s-error1 "sreverse" ""))
  (if *parse-utf-8-input* (utf-8-sreverse str) (reverse str)) )
;;
(defun utf-8-sreverse (str)
  (do ((ol (mapcar #'char-code (coerce str 'list)))
        ch m-chars )
      ((null ol) 
       (reduce #'(lambda (s0 s1) (concatenate 'string s0 s1)) m-chars :initial-value ""))
    (multiple-value-setq (ol ch) (rm-first-utf-8-char ol))
    (push (utf-8-m-char (length ch) ch) m-chars) ))


(defun $substring (str start &optional (end nil))
  (unless (stringp str) (s-error1 "substring" "first"))
  (decf start)
  (when end (decf end))
  (or (ignore-errors 
        (when *parse-utf-8-input*
          (let* ((ov (string-to-raw-bytes str))
                 (args (utf-8-fix-start-end ov (list nil start end))) )
            (setq start (cadr args)
                  end (caddr args) )))
        (subseq str start end) )
      (s-pos-error2 "substring") ))


;; comparison - test functions - at Maxima and Lisp level
;;
;;
(defun $sequal       (s1 s2) (string= s1 s2))      ;; for the sake of efficiency 
(defun $sequalignore (s1 s2) (string-equal s1 s2)) ;;   omit checkings here
;;
(defun $slessp    (s1 s2) (scompare s1 s2 "slessp"    '$sequal t   #'$clessp))
(defun $sgreaterp (s1 s2) (scompare s1 s2 "sgreaterp" '$sequal nil #'$cgreaterp))
;;
(defun $slesspignore    (s1 s2) (scompare s1 s2 "slesspignore"    '$sequalignore t   #'$clesspignore))
(defun $sgreaterpignore (s1 s2) (scompare s1 s2 "sgreaterpignore" '$sequalignore nil #'$cgreaterpignore))
;;
(defun scompare (s1 s2 name test lessp? ccomp)
  (unless (and (stringp s1) (stringp s2)) (s-error2 name "the two"))
  (let ((pos (mismatch s1 s2 :test test)))
    (cond 
      ((or (not pos) (>= pos (length (if lessp? s2 s1)))) nil)
      ((>= pos (length (if lessp? s1 s2))) t)
      (t (apply ccomp (chars-to-compare s1 s2 pos))) )))
;;
(defun chars-to-compare (s1 s2 pos)
  (let ((l1 1) (l2 1))
    (when *parse-utf-8-input*
      ;; When mismatch finds a pos somewhere in an utf-8 octet sequence 
      ;;   we have to identify the beginning and the length.
      (while (= (logand (char-code (elt (subseq s1 pos (1+ pos)) 0)) 192.) 128.)
        (decf pos) ) ;; the beginning
      (setq l1 (parse-utf-8-header s1 pos) ;; the length
            l2 (parse-utf-8-header s2 pos) ))
    (list (subseq s1 pos (+ pos l1)) (subseq s2 pos (+ pos l2))) ))
;;
(defun parse-utf-8-header (str start) 
  ;; The position start is the beginning of an utf-8 octet sequence.
  ;; parse-utf-8-header then returns the length of this sequence.
  (let ((h (char-code (elt (subseq str start (1+ start)) 0))))
    (cond
      ((not (logbitp 7 h)) 1)
      ((= (ldb (byte 3 5) h) #b110) 2)
      ((= (ldb (byte 4 4) h) #b1110) 3)
      ((= (ldb (byte 5 3) h) #b11110) 4)
      (t (gf-merror (intl:gettext "`parse-utf-8-header': ~m is no utf-8 header") h)) )))


(defun $smismatch (s1 s2 &optional (test '$sequal))
  (unless (and (stringp s1) (stringp s2)) (s-error2 "smismatch" "first two"))
  (unless (member test '($sequal $sequalignore))
    (gf-merror (intl:gettext 
      "`smismatch': optional third argument must be `sequal' or `sequalignore'." )))
  (let ((pos (mismatch s1 s2 :test test)))
    (when pos 
      (if *parse-utf-8-input* 
        (- (1+ pos)
           (utf-8-pos-dec s1 (if (= pos (length s1)) pos (1+ pos))) )
        (1+ pos) ))))


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
            (if *parse-utf-8-input* 
              (- (1+ pos) (utf-8-pos-dec str pos))
              (1+ pos) )
            (return-from $ssearch nil) )))
      (s-pos-error2 "ssearch") ))
;;
(defun ssearch (seq str &optional (test '$sequal) (start 0) (end nil))
  (search seq str :test test :start2 start :end2 end) )


;; allow arbitrary order of the optional args test, start, end 
;;  (where start is of course the first integer in sequence)
;;
(defun s-optional-args (name str args) 
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
    (when *parse-utf-8-input*
      (or (ignore-errors 
            (setq args 
              (utf-8-fix-start-end (string-to-raw-bytes str) args) ))
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
(defun ssubstfirst (new old str &optional (test '$sequal) (start 0) (end nil) (matched? nil))
  (let ((len (length old))
        (pos (if matched? start (search old str :test test :start2 start :end2 end))))
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
  (let ((pos nil) (n (length new)) (o (length old)))
    (while (setq pos (search old str :test test :start2 start :end2 end))
      (setq str (ssubstfirst new old str test pos end t)
	    start (+ pos n)
	    end (when end (- (+ end n) o))))
    str))


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
        (when *parse-utf-8-input*
          (incf pos (utf-8-pos-inc (string-to-raw-bytes str) 0 pos)) )
        (let ((sq1 (subseq str 0 pos))
              (sq2 (subseq str pos)) )
          (concatenate 'string sq1 seq sq2) ))
      (s-pos-error1 "sinsert" (1+ pos)) ))


(defun $ssort (str &optional (test '$clessp)) 
  (unless (stringp str) (s-error1 "ssort" "first"))
  (if *parse-utf-8-input* 
    (unless (member test '($clessp $cgreaterp))
      (let ((alt #+gcl "" 
                 #-gcl "and the external format is not adjusted to UTF-8" ))
        (gf-merror (intl:gettext 
          "`ssort': when us_ascii_only is false ~a 
the optional second argument must be `clessp' or `cgreaterp'." ) alt )))
    (unless (member test '($clessp $cgreaterp $clesspignore $cgreaterpignore))
      (gf-merror (intl:gettext 
        "`ssort': optional second argument must be one of ~%clessp[ignore], cgreaterp[ignore]" ))))
  (setq test (stripdollar test))
  (let ((copy (copy-seq str)))
    (if *parse-utf-8-input* (utf-8-ssort copy test) (stable-sort copy test)) ))
;;
(defun utf-8-ssort (str &optional (test 'clessp)) 
  (setq test 
    (if (equal test 'clessp) #'< #'> ))
  (do ((ol (coerce (string-to-raw-bytes str) 'list))
        utf8 code-pts ) 
      ((null ol) 
       (let ((l (mapcar #'(lambda (n) ($unicode n)) (stable-sort code-pts test))))
         (reduce #'(lambda (s0 s1) (concatenate 'string s0 s1)) l :initial-value "")))
    (multiple-value-setq (ol utf8) (rm-first-utf-8-char ol))
    (push (utf8-to-uc utf8) code-pts) ))


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
        (if *parse-utf-8-input*
          (let* ((ov (string-to-raw-bytes str))
                 (args (utf-8-fix-start-end ov (list nil start end))) )
            (funcall sfun str :start (cadr args) :end (caddr args)) )
          (funcall sfun str :start start :end end) ))
      (s-pos-error2 name) ))


(defun $sinvertcase (str &optional (start 1) (end nil))
  (unless (stringp str) (s-error1 "sinvertcase" "first"))
  (decf start)
  (when end (decf end))
  (or (ignore-errors 
        (when *parse-utf-8-input*
          (let* ((ov (string-to-raw-bytes str))
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

