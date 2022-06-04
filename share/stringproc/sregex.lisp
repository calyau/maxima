#|
                              ~*~  sregex  ~*~
  
    Maxima interface to pregexp.lisp (a portable regex parser by Dorai Sitaram)
  
    Copyright : 2008 - 2016 Volker van Nek

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

   Functions: 
   
   regex_compile - returns a Lisp structure containing a compiled regex.
   
   regex_match_pos, regex_match, regex_split, regex_subst_first, regex_subst -
      the regular expression used as an argument may be a string or 
      compiled by regex_compile.
   
   string_to_regex - masking special characters.
   
   Examples:
   
   str : "his hay needle stack -- my hay needle stack -- her hay needle stack"$
   
   regex : regex_compile("ne{2}dle")$
   
   regex_match_pos(regex, str);
                                               [[9, 15]]
   regex_match_pos("ne{2}dle", str);
                                               [[9, 15]]
   regex_match_pos("ne{2}dle", str, 25, 44);
                                               [[32, 38]]
   regex_match("ne{2}dle", "hay needle stack");
                                               ["needle"]
   regex_match("ne{2}dle", "hay needle stack", 10);
                                                 false
   regex_split("[,;]+", "split,pea;;;soup");
                                        ["split", "pea", "soup"]
   regex_subst_first("ty", "t.", "liberte egalite fraternite");
                                      "liberty egalite fraternite"
   regex_subst("ty", "t.\\b", "liberte egalite fraternite");
                                      "liberty egality fraternity"
   string_to_regex(". :");
                                                 "\. :"

--------------------------------------------------------------------------------

   The pregexp manual by Dorai Sitaram is at
   http://ds26gte.github.io/pregexp/index.html.

--------------------------------------------------------------------------------

   Like in stringproc.lisp we use 1-indexed position specifications. 
   
   When the external format is not utf-8 (unicode) positions are counted in 
   octets at Lisp level and in Maxima characters at Maxima level. 
   See remarks in stringproc.lisp.
   Without unicode support non-us-ascii Maxima characters are not recognized 
   by regular expressions, e.g. the regex "." doesn't match to an umlaut.

|#

(in-package :maxima)


(declare-top (special *parse-utf-8-input*))



(defstruct (compiled-regex (:print-function compiled-regex-print))
  parse-tree )

(defun compiled-regex-print (struct stream i) 
  (declare (ignore struct i))
  (format stream "Structure [COMPILED-REGEX]") ) ;; wxMaxima prints this
                                                 ;; terminal should print this too

(defun $regex_compile (regex)
  (make-compiled-regex
    :parse-tree (pregexp:pregexp regex) ))


(defun regex-check-and-maybe-coerce (name regex &rest args)
  (cond
    ((compiled-regex-p regex)
      (setq regex (compiled-regex-parse-tree regex)) )
    ((not (stringp regex))
      (gf-merror (intl:gettext "`~m': first arg must be a compiled regex or a string.") name) ))
  (unless (every #'stringp args)
    (gf-merror (intl:gettext "Unsuitable arguments to `~m'.") name) )
  regex )


(defun regex-index-error (name)
  (gf-merror (intl:gettext "`~m': improper start or end index.") name) )


;; When the external format is not utf-8 (unicode) positions are counted in octets.
;; We want them in numbers of characters to find the right position in a string.
;; utf-8-pos-dec returns the decrement we need to adjust.
;;   (string position = octet position - decrement)
(defun regex-utf-8-pos-dec (ov off pos) ;; begin to count at a given offset
  (do ((i off (1+ i))
       (n 0)) 
     ((= i pos) n)
    (when (= (logand (aref ov i) 192.) 128.)
      (incf n) )))


(defun $regex_match_pos (regex str &optional (start 1) (end nil)) ;; 1-based indexing!
  (setq regex (regex-check-and-maybe-coerce "regex_match_pos" regex str))
  (decf start)
  (when end (decf end))
  (let (ov)
    (or (ignore-errors 
          (when *parse-utf-8-input*
            (setq ov (string-to-raw-bytes str))
            (let ((args (utf-8-fix-start-end ov (list nil start end))))
              (setq start (cadr args)
                    end (caddr args) )))
          (let ((pos-list (pregexp:pregexp-match-positions regex str start end))
                (pos-mlist nil) )
            (if pos-list 
              (dolist (pos pos-list (cons '(mlist simp) (nreverse pos-mlist)))
                (when *parse-utf-8-input*
                  (let ((dec (regex-utf-8-pos-dec ov 0 (car pos))))
                    (decf (cdr pos) (+ dec (regex-utf-8-pos-dec ov (car pos) (cdr pos))))
                    (decf (car pos) dec) ))
                (push `((mlist simp) ,(1+ (car pos)) ,(1+ (cdr pos))) pos-mlist) )
              (return-from $regex_match_pos nil) )))
        (regex-index-error "regex_match_pos") )))


(defun $regex_match (regex str &optional (start 1) (end nil))
  (setq regex (regex-check-and-maybe-coerce "regex_match" regex str))
  (or (ignore-errors 
        (when *parse-utf-8-input*
          (let* ((ov (string-to-raw-bytes str))
                 (args (utf-8-fix-start-end ov (list nil start end))) )
            (setq start (cadr args)
                  end (caddr args) )))
        (let ((match 
                (pregexp:pregexp-match regex str (1- start) (if end (1- end) nil)) ))
          (if match 
            (cons '(mlist simp) match)
            (return-from $regex_match nil) )))
      (regex-index-error "regex_match") ))


(defun $regex_split (regex str)
  (setq regex (regex-check-and-maybe-coerce "regex_split" regex str))
  (cons '(mlist simp) (pregexp:pregexp-split regex str)) )


(defun $regex_subst_first (replacement regex str)
  (setq regex (regex-check-and-maybe-coerce "regex_subst_first" regex str replacement))
  (pregexp:pregexp-replace regex str replacement) )
;;
;; Argument order different to the order of pregexp-replace.
;; Use order like in $ssubst or substitute: new, old, str.
;;
(defun $regex_subst (replacement regex str)
  (setq regex (regex-check-and-maybe-coerce "regex_subst" regex str replacement))
  (pregexp:pregexp-replace* regex str replacement) )


(defun $string_to_regex (str)
  (unless (stringp str)
    (gf-merror (intl:gettext "`string_to_regex': Argument must be a string.")) )
  (pregexp:pregexp-quote str) )

