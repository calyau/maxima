


;;First pass with string-delimited-search = string-search
;;and also to fix the package and syntax header. Do any 
;;copyright changes.
;;
(defvar *first-pass*
 '(("package: macsyma" . "mode: lisp; package: cl-maxima; syntax: common-lisp")
 ("package:macsyma" . "mode: lisp; package: cl-maxima; syntax: common-lisp")
 ("#//"  . "#\")))

;;Second pass 
;;evaluate the following in zetalisp syntax!!.
(defvar *second-pass*
'(
  ("#\;" .  "#. semi-colon-char")
  ("#\\" .  "#. back-slash-char")
  ("#\//" .  "#. forward-slash-char")
  ("#\/"" .  "#. double-quote-char")
  ("#\)" . "#. right-parentheses-char")
  ("#\(" . "#. left-parentheses-char")
  ("#\." . "#. period-char")
  ("#\|" .  "#. vertical-stroke-char")
  ("DELETE" . "zl-DELETE")
  ("EVERY"  . "ZL-EVERY") ;;actually all occurrences are now cl:every, there were no zl-every.
  ("MAKE-ARRAY" . "ZL-MAKE-ARRAY")
  ("MEMBER" . "zl-MEMBER")
  ("ASSOC" . "zl-ASSOC")
  ("LISTP" . "zl-LISTP")
  ("MAP" . "MAPL")
  ("MEMBER" . "zl-MEMBER")
  ("multiple-value" . "multiple-value-setq")
  ("NLISTP" . "zl-NLISTP")
  ("NINTERSECTION" . "zl-NINTERSECTION")
  ("NUNION" . "zl-NUNION")
  ("RASSOC" . "zl-RASSOC")
  ("REM" . "zl-REM")
  ("REMOVE" . "zl-REMOVE")
  ("SOME" . "zl-SOME")  ;;check were any  cl:some?
  ("UNION" . "zl-UNION")
  ("\" . "fixnum-remainder")
  ("\\" . "fixnum-gcd")
  ("//\" . "fixnum-remainder")
  ("typep" . "ml-typep")
  ( "terminal-io" .  "*terminal-io*")
  ( "standard-input" . "*standard-input*")
  ( "standard-output" . "*standard-output*")
  ( "query-io" . "*query-io*")
  ( "base" . "*print-base*")
  ( "ibase" . "*read-base*")
  ( "(*nopoint t)" . "print-radix*") ;;catch a few..
  ("CONSTANTP" . "MAXIMA-CONSTANTP")    ;;"CL-MAXIMA-SOURCE: MAXIMA; SIMP" ;different
  ("ERROR" . "MAXIMA-ERROR")            ;;"CL-MAXIMA-SOURCE: MAXIMA; COMMAC"
  ("FIND" . "MAXIMA-FIND")              ;;"CL-MAXIMA-SOURCE: MAXIMA; LMSUP" ;different
  ("float-precision" . "MAXIMA-float-precision") ;;"CL-MAXIMA-SOURCE: MAXIMA; cpoly" ;?
  ("INTEGERP" . "MAXIMA-INTEGERP")      ;;"CL-MAXIMA-SOURCE: MAXIMA; compar" ;different
  ("RATIONALIZE" . "MAXIMA-RATIONALIZE");;"CL-MAXIMA-SOURCE: MAXIMA; RAT3E" ;different
  ("RATIONALP" . "MAXIMA-RATIONALP")    ;;"CL-MAXIMA-SOURCE: MAXIMA; LAPLAC" ;different
  ("REDUCE" . "MAXIMA-REDUCE")          ;;"CL-MAXIMA-SOURCE: MAXIMA; trigi" ;different
  ("REM" . "MAXIMA-REM")                ;;"CL-MAXIMA-SOURCE: MAXIMA; MRGMAC" ;different
  ("REMF" . "MAXIMA-REMF")              ;;"CL-MAXIMA-SOURCE: MAXIMA; DB"  ;different
  ("SLEEP" . "MAXIMA-SLEEP")            ;;"CL-MAXIMA-SOURCE: MAXIMA; COMPAT" 
  ("SUBSTITUTE" . "MAXIMA-SUBSTITUTE")  ;;"CL-MAXIMA-SOURCE: MAXIMA; COMM" ;different
  ("TYPECASE" . "MAXIMA-TYPECASE")      ;;"CL-MAXIMA-SOURCE: MAXIMA; COMMAC" ;remove old occur
  ))

(defvar *third-pass*
	'(("plist" . "symbol-plist")
	  ))

;;Additional changes:
;;"(declare"  .  "(top-level-declare"  ;;when char before is #\newline

(defun first-pass (file)
  (let ((*replace-comments* t) (*delimit-test* #'any-ok))
    (file-multiple-string-delimited-replace *first-pass* file)))

(defun second-pass (file)
    (file-multiple-string-delimited-replace *second-pass* file))


;;;;Explorer specific settings
#+ti
(setq  compiler:QC-FILE-CHECK-INDENTATION nil)

