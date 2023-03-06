;;
;;                                  ~*~  PRINTF  ~*~
;;
;; Formatted printing to character streams
;;
;; Copyright     : 2005-2015 Volker van Nek
;; Licence       : GPL2
;;
;; Test file     : rtestprintf.mac (lots of examples)
;; Documentation : stringproc.texi 
;;

#|

`$printf' is an interface to the Lisp function `format'.
`$printf' provides a directive ~w,d,e,x,o,p@h to process bigfloat numbers.
Arguments which are passed to d,f,e,g or h-directives are converted to that type.

Before passing to `format' ctrls and args are both visited and modified:
E.g. `prepare-ctrls' replaces the directive ~w,d,e,x,o,p@h by ~@a 
and `prepare-args' replaces the corresponding bigfloat argument by a string. 
As a consequence of this no arg can be used twice. The goto-directive ~* is lost.

In GCL/utf-8 builds mincol specifications do not work correctly 
if non-us-ascii characters are used as arguments to ~s and ~a directives.

|#

(in-package :maxima)

;; This is an implementation of the formerly src/plot.lisp/$sprint 
;; as a simple wrapper for printf(true,"~@{~a ~}",x,y,z,...)
;; keeping the original return value: the first argument resp. false
;;
(defun $sprint (&rest args)
  (loop for v in args do
    ($printf t "~a " v))
  (car args) )


(defvar $stderr)
(defvar $stdout)
(defvar $stdin)

;; Make stdout, stdin and stderr user-accessible
;;
(setq $stderr *error-output*
      $stdout *standard-output*
      $stdin  *standard-input* )

(defvar *tilde-m-args*)
(defvar *tilde-m-params*)
(defvar *tilde-m-placeholders*)

(defun reconstitute-tilde-m (param)
  ($sconcat "~" param "m"))

(defun generate-tilde-m-placeholder ()
  (coerce (mapcar #'code-char
                  (loop for i from 1 to 6
                        collect (if (eq ($random 2) 0) (+ ($random (- 92 33)) 33) (+ ($random (- 126 93)) 93))))
          'string))

(defun $printf (stream ctrls &rest args)
  (cond 
    ((and (not (member stream '(t nil))) (not (streamp stream)))
      (gf-merror (intl:gettext 
        "`printf': first argument must be `true', `false' or a stream." )))
    ((not (stringp ctrls))
      (gf-merror (intl:gettext "`printf': second argument must be a string.")) ))
  (let (*tilde-m-args* *tilde-m-params* *tilde-m-placeholders*)
    (setq args (prepare-args ctrls args nil))
    (let ((body (mapcar #'(lambda (x) (if (listp x) `(quote ,x) x)) args)))
      (setq ctrls (prepare-ctrls ctrls))
      (if *tilde-m-args*
        ;; Capture output from FORMAT, and then call AFORMAT to handle ~M directives.
        (let*
          ((first-pass (apply 'format nil ctrls body))
           (second-pass (ssubst "~~" "~" first-pass))
           (third-pass second-pass))
          ;; Substitute ~m directives back in.
          (loop for param in *tilde-m-params* for placeholder in *tilde-m-placeholders*
                do (setq third-pass (ssubst (reconstitute-tilde-m param) placeholder third-pass)))
          (apply 'aformat stream third-pass (reverse *tilde-m-args*)))
        ;; No ~m directives present, punt directly to FORMAT.
        (eval `(format ,stream ,ctrls ,@body)) ))))


(defun prepare-args (ctrls todo done &optional (loop nil))
  (let ((start 0) pos1 pos2 pos1a
         params spec subctrls 
        (skip 0) (loops nil) (index 0) )
    (do ((arg (car todo)))
        ((null todo)
          (setq pos1 (search "~" ctrls :start2 start))
          (when pos1
            (setq pos2 (spec-position ctrls pos1)
                  spec (subseq ctrls (1- pos2) pos2) ))
          (if (and (zerop skip) (or (not pos1) (search spec "^{}[]<>%;&~tpTP
")));; newline possible spec
            (reverse done)
            (gf-merror (intl:gettext "`printf': arguments exhausted.")) ))
      (prog ()
       tag1
;; recognize the directive:
        (setq pos1 (search "~" ctrls :start2 start))
        (unless pos1
          (cond 
            (loop
              (setq start 0 
                    pos1 (search "~" ctrls :start2 start) ))
            (t
              (push arg done)
              (go tag3) )))
        (setq pos2 (setq pos1a (spec-position ctrls pos1))
              spec (subseq ctrls (1- pos2) pos2) )
        (when (search spec "}]>;%&t~") (setq start pos2) (go tag1))
        (setq params (subseq ctrls (1+ pos1) (1- pos2)))
;;
;; pre-test for ~nr, ~vr, ~#r :
;; check if radix is legal (Maxima 5.14 gcl segfaults when radix is 1)
        (when (and (string-equal spec "r") (string/= "" params))
          (let ((ch (subseq params 0 1)) (n "") (len (length params)) radix)
            (when (or ($digitcharp ch) (search ch "v#V")) ;; stringproc.lisp/$digitcharp 
              (do ((p 1 (1+ p)))
                  ((or (= p len) (search ch ",@:v#V")))
                (setq n (concatenate 'string n ch)
                      ch (subseq params p (1+ p)) ))
              (setq radix
                (cond
                  ((string= ch ",") 10.)
                  ((string-equal ch "v") arg)
                  ((string= ch "#") (length todo))
                  ((or (string= ch "@") (string= ch ":")) (parse-integer n))
                  (t (parse-integer (concatenate 'string n ch))) ))
              (when (or (< radix 2.) (> radix 36.))
                (gf-merror (intl:gettext "`printf': illegal radix in r-directive: ~m") radix)) )))
;;
;; handle some special directives:
        (cond
;; ~v,v,v<spec>, spec=ABDEFGORSTX<~&%$   (# needs no special care; ~v[ not supported, see below)
          ((search spec "abdefgorstx<~&%$" :test #'string-equal)
            (when (> (setq skip (count-v params)) 0)
              (do () ((zerop skip))
                (push (if (stringp arg) (character arg) arg) done)
                (setq arg (car (setq todo (cdr todo))))
                (incf index)
                (decf skip) )
              (push (prepare-arg params spec arg) done)
              (go tag2) ))
;; ~v,#,vH
          ((string-equal spec "h")
            (when (check-v# params)
              (let ((prms (split-at-comma params))
                     prm (new-params "") )
                (dolist (p prms)
                  (setq prm
                    (cond
                      ((string-equal "v" p)
                        (prog1
                          ($sconcat arg)
                          (setq arg (car (setq todo (cdr todo))))
                          (incf index) ))
                      ((string= "#" p) ($sconcat (length todo)))
                      (t p) ))
                  (setq new-params (concatenate 'string new-params prm ",")) )
                (push (prepare-arg new-params spec arg) done)
                (go tag2) )))
;; ~@[, ~#[, ~n[
          ((string= "[" spec)
            (cond
              ((string= "" params)) ;; don't check another condition
              ((or (and (string= "@" params) arg) ;; if arg is not nil, arg is not consumed
                   (string= "#" params) ) 
                (setq start pos2)
                (go tag1) )
              ((or (string= "v" params)
                   (every #'digit-char-p (coerce params 'list)))
                (gf-merror (intl:gettext "`printf': not supported directive ~~~m[") params) ))) ;; 0- vs. 1-based indexing
;; ~?
          ((string= "?" spec)
            (cond
              ((string= "" params)
                (let ((ind-ctrls arg)) ;; arg is a string
                  (push arg done)
                  (setq arg (car (setq todo (cdr todo))))
                  (push (prepare-args ind-ctrls (cdr arg) nil nil) done)
                  (incf index)
                  (go tag2) ))
              ((string= "@" params)
                (setq ctrls 
                  (concatenate 'string (subseq ctrls 0 pos1) arg (subseq ctrls pos2)) )
                (push arg done)
                (go tag3) )
              (t 
                (gf-merror (intl:gettext "`printf': illegal directive ~~~m?") params)) ))
;; ~^
          ((string= "^" spec)
            (when (search "@" params)
              (gf-merror (intl:gettext "`printf': illegal directive ~~~m^") params) )
            (when (> (setq skip (count-v params)) 0)
              (do () ((= skip 0))
                (push (if (stringp arg) (character arg) arg) done)
                (setq arg (car (setq todo (cdr todo))))
                (incf index)
                (decf skip) )
              (push (prepare-arg params spec arg) done) )
            (setq start pos2)
            (go tag1) )
;; ~:P and ~:@P
          ((and (string-equal "p" spec) (search ":" params)) ;; ':' backs up
             (setq start pos2)
             (go tag1) ))
;; default part:
;;
;; loop ... 
        (cond 
          ((string= "{" spec)
;; ~n{ and ~v{ etc. , set number of loops
            (and (string/= "" params) 
                 (string/= ":" params) (string/= "@" params) (string/= ":@" params) 
              (let ((ch (subseq params 0 1)) (n "") (len (length params)))
                (do ((p 1 (1+ p)))
                    ((or (= p len)(search ch "@:v#V"))
                       (setq params
                         (if (or (string= ch "@") (string= ch ":"))
                           (subseq params (1- p))
                           (subseq params p) )))
                  (setq n (concatenate 'string n ch)
                        ch (subseq params p (1+ p))) )
                (unless loops 
                  (cond
                    ((string-equal ch "v") 
                      (push arg done) 
                      (incf index)
                      (setq loops arg 
                            arg (car (setq todo (cdr todo))) ))
                    ((string= ch "#") 
                      (setq loops (length todo)) )
                    ((or (string= ch "@") (string= ch ":")) 
                      (setq loops (parse-integer n)) )
                    (t 
                      (setq loops (parse-integer (concatenate 'string n ch))) ))) ))
;; ~{ and ~:{ and ~@{ and  ~:@{ 
            (cond 
              ((string= "" params)
                (setq pos2 (cadr (iter-positions ctrls pos1))
                      subctrls (subseq ctrls pos1a (- pos2 2.))
                      loops nil )
                (push (prepare-args subctrls (cdr arg) nil t) done) )
              ((string= ":" params)
                (setq pos2 (cadr (iter-positions ctrls pos1))
                      subctrls (concatenate 'string "~{" (subseq ctrls pos1a pos2))
                      loops nil )
                (push (prepare-args subctrls (cdr arg) nil t) done) )
              ((string= "@" params)
                (setq pos2 (cadr (iter-positions ctrls pos1))
                      subctrls (concatenate 'string "~{" (subseq ctrls pos1a pos2)) )
                (setq done 
                  (append 
                    (reverse 
                      (car 
                        (prepare-args 
                          subctrls 
                          (list (cons '(mlist) 
                            (if loops (butlast todo (max 0 (- (length todo) loops))) todo) ))
                          nil 
                          nil )))
                    done ))
                (setq todo (if loops (nthcdr loops todo) nil)
                      arg (car todo)
                      start pos2 )
                (when loops 
                  (incf index loops) 
                  (setq loops nil) )
                (go tag4) )
              ((string= ":@" params)
                (setq pos2 (cadr (iter-positions ctrls pos1)))
                (setq subctrls 
                  (concatenate 'string 
                    "~" (if loops ($sconcat loops) "") ":{" (subseq ctrls pos1a pos2) ))
                (setq done 
                  (append 
                    (reverse 
                      (car 
                        (prepare-args 
                          subctrls 
                          (list (cons '(mlist) 
                            (if loops (butlast todo (max 0 (- (length todo) loops))) todo) ))
                          nil 
                          nil ))) 
                    done ))
                (setq todo (if loops (nthcdr loops todo) nil)
                      arg (car todo)
                      start pos2 )
                (when loops 
                  (incf index loops) 
                  (setq loops nil) )
                (go tag4) )))
;; ... or don't loop ...
          (t 
            (push (prepare-arg params spec arg) done) )) 
;; ... set the position in ctrls ...
       tag2
        (setq start pos2)
;; ... and take the next argument
       tag3 
        (setq arg (car (setq todo (cdr todo))))
        (incf index)
       tag4 ))))


(defun prepare-ctrls (ctrls)
  (let ((start 0) (pos1 nil) (pos2 0)
        (new-ctrls "") 
         spec ) 
;; ~w,d,e,x,o,p@H
    (setq pos1 (search "~" ctrls :start2 start))
    (do () 
        ((not pos1)
          (concatenate 'string new-ctrls (subseq ctrls pos2)) )
      (setq pos2 (spec-position ctrls pos1)
            spec (subseq ctrls (1- pos2) pos2) )
      (setq new-ctrls 
        (cond
          ((string-equal spec "h")
           (concatenate 'string new-ctrls (subseq ctrls start pos1) "~@a"))
          ((string-equal spec "m")
           (concatenate 'string new-ctrls (subseq ctrls start pos1) "~a"))
          (t (concatenate 'string new-ctrls (subseq ctrls start pos2)) )))
      (setq start pos2  
            pos1 (search "~" ctrls :start2 start) ))))


(defun spec-position (ctrls pos1)
  (do ((p (1+ pos1) (1+ p))) (())
    (and (search (subseq ctrls p (1+ p)) "abcdefghmoprstx%{}^&$[]?~<>;ABCDEFGHMOPRSTX
") ;;  newline possible spec
         (not (string= "'" (subseq ctrls (1- p) p)))
      (return (1+ p)) )))


;;  helper for ~v,v,v<spec>
;;
(defun count-v (params)
  (if (string= "" params)
    0
    (do ((p 0 (1+ p)) (len (length params)) (n 0))
        ((= p len) n)
      (and (string-equal "v" (subseq params p (1+ p)))
           (or (zerop p) (not (string= "'" (subseq params (1- p) p))))
        (setq n (1+ n)) ))))


;;  helper for ~v,#,vH
;;
(defun check-v# (params)
  (unless (string= "" params)
    (do ((p 0 (1+ p)) (len (length params)))
        ((= p len) nil)
      (and (search (subseq params p (1+ p)) "vV#")
           (or (zerop p) (not (string= "'" (subseq params (1- p) p))))
        (return t) ))))


;;  find positions of matching braces
;;
(defun iter-positions (ctrls start)
  (let (pos1 pos2 (end (+ start 2))
        spec (n 1) )
    (do ()
        ((zerop n) (list start end))
      (setq pos1 (search "~" ctrls :start2 end) 
            pos2 (spec-position ctrls pos1)
            spec (subseq ctrls (1- pos2) pos2) )
      (when (string-equal spec "{") (incf n))
      (when (string-equal spec "}") (decf n))
      (setq end pos2) )))


(defun split-at-comma (params)
  (do ((p 0 (1+ p)) (len (length params)) (prms nil) (prm ""))
      ((= p len) (reverse (cons prm prms)))
    (cond 
      ((and (search (subseq params p (1+ p)) ",@")
            (or (zerop p) (not (string= "'" (subseq params (1- p) p)))) )
        (setq prms (cons prm prms)
              prm "" ))
      ((and (string= (subseq params p (1+ p)) ",")
            (not (zerop p))
            (string= "'" (subseq params (1- p) p)) )
        (setq prms (cons "," prms)
              prm "" ))
      ((string= (subseq params p (1+ p)) "'")
        nil )
      (t
        (setq prm (concatenate 'string prm (subseq params p (1+ p)))) ))))


(defun prepare-arg (params spec arg)
  (cond
;; ~w,d,e,x,o,p@H
    ((string-equal "h" spec)
      (unless (bigfloatp arg)
        (cond
          (($numberp arg) ;; Maxima rational, float
            (setq arg ($bfloat arg)) )
          ((and ($constantp arg) ;; %pi, sqrt(2), ...
                ($freeof '$%i arg) (not (member arg '(t nil))) (not ($listp arg)))
            (setq arg ($bfloat arg)) )
          (t
            (gf-merror (intl:gettext 
              "`printf': argument can't be supplied to h-directive: ~m" ) arg ))))
      (let ((prms (split-at-comma params))
             wd dd ed xp ov pc at smarap )
        (multiple-value-setq (wd dd ed xp ov pc) (apply #'values prms))
        (setq smarap (reverse params))
        (and (string/= "" smarap) 
             (string= (subseq smarap 0 1) "@") 
             (or (= 1 (length smarap)) (string/= (subseq smarap 1 2.) "'"))
          (setq at t) )
        (setq arg 
          (let ((wd (and wd (string/= "" wd) (parse-integer wd)))
                (dd (and dd (string/= "" dd) (parse-integer dd)))
                (ed (and ed (string/= "" ed) (parse-integer ed))) 
                (xp (and xp (string/= "" xp) (parse-integer xp))) )
            (bprintf arg wd dd ed xp ov pc at) ))))
;; ~E, ~F, ~G
    ((search spec "efg" :test #'string-equal)
      (unless (floatp arg)
        (cond
          (($numberp arg) ;; Maxima rational, bigfloat
            (setq arg ($float arg)) )
          ((and ($constantp arg) ;; %pi, sqrt(2), ...
                (not (member arg '(t nil)))
                (not ($listp arg))
                (let ((would-be-arg ($float arg)))
                  (and ($numberp would-be-arg) (setq arg would-be-arg)))))
          (t
            (gf-merror (intl:gettext 
              "`printf': argument can't be supplied to ~m-directive: ~m" ) spec arg )))))
;; ~D
    ((string-equal "d" spec)
      (unless (integerp arg)
        (cond
          (($numberp arg) ;; Maxima rational, (big)float
            (setq arg ($truncate arg)) )
          ((and ($constantp arg) ;; %pi, sqrt(2), ...
                ($freeof '$%i arg) (not (member arg '(t nil))) (not ($listp arg)))
            (setq arg ($truncate arg)) )
          (t
            (gf-merror (intl:gettext 
              "`printf': argument can't be supplied to d-directive: ~m" ) arg )))))
;; ~A, ~S
    ((search spec "as" :test #'string-equal)
      (setq arg ($sconcat arg)) )
;; ~C
    ((string-equal "c" spec)
      (setq arg (character arg)) ) ;; conversion to Lisp char
;; ~M
    ((string-equal "m" spec)
     (push arg *tilde-m-args*)
     (push params *tilde-m-params*)
     (let ((placeholder (generate-tilde-m-placeholder)))
       (push placeholder *tilde-m-placeholders*)
       (setq arg placeholder)))
;; ~[
    ((string= "[" spec) 
      (unless (or (string= "@" params) (string= ":" params))
        (when (integerp arg) (setq arg (1- arg))) ))) ;; 1-based indexing!
  arg ) 


;; bf: bigfloat
;; wd: nil or width
;; dd: nil or decimal digits behind floating point
;; ed: nil or minimal exponent digits
;; xp: nil or preferred exponent
;; ov: nil or overflow character
;; pc: nil or padding character
;; at: nil or true; sets "+" if true
;;
(defun bprintf (bf wd dd ed xp ov pc at);; ~w,d,e,x,o,p@H
;;
  (labels ((cs* (&rest args) (eval `(concatenate 'string ,@args))) ;; functions to shortcut 
           (ms* (len ie) (make-string len :initial-element ie)) )  ;;   some parts of the code 
          (declare (inline cs* ms*))
;;
    (let ((fpprec (caddar bf))) ;; keep old fpprec in case it differs to current fpprec
      (and xp (not (zerop xp))
        (setq bf (bcons (if (minusp xp) 
                           (fptimes* (cdr bf) (intofp (expt 10. (- xp))))
                           (fpquotient (cdr bf) (intofp (expt 10. xp))) ))))
;;    
      (and dd
        (let ((m (intofp (expt 10. dd))))
          (setq bf (fptimes* (cdr bf) m)
                bf (meval `((%round) ,(bcons bf)))
                bf (bcons (fpquotient (intofp bf) m)) )))) 
;;  
    (let* ((s (string-left-trim "-" ($sconcat bf)))
           (sgn (signum (cadr bf)))
           (part1 (subseq s 0 1))
           (pos (position #\b s))
           (part2 (string-right-trim "0" (subseq s 2. pos)))
           (len (length part2))
           (pow (parse-integer (subseq s (1+ pos) nil))) )
      (cond 
        ((and (> pow 0) (> len pow))
          (setq s (cs* part1 (subseq part2 0 pow) "." (subseq part2 pow nil)))
          (and dd (> (+ dd pow) len)
            (setq s (cs* s (ms* (+ dd pow (- len)) #\0))) ))
        ((> pow 0)
          (setq s (cs* part1 part2 (ms* (- pow len) #\0) ".0"))
          (and dd (> dd 0)
            (setq s (cs* s (ms* (1- dd) #\0))) ))
        ((= pow 0)
          (setq s (cs* part1 (if (= len 0) ".0" ".") part2))
          (when (= len 0) (incf len))
          (and dd (> dd len)
            (setq s (cs* s (ms* (- dd len) #\0))) ))
        ((< pow 0)
          (setq pow (- pow)
                s (cs* "0." (ms* (1- pow) #\0) part1 part2) )
          (and dd (> dd (+ len pow))
            (setq s (cs* s (ms* (- dd pow len) #\0))))))
;;  
      (and dd (= dd 0)
        (setq s (string-right-trim "0" s)))
      (if (< sgn 0) 
        (setq s (cs* "-" s))
        (when at (setq s (cs* "+" s))) )
;;  
      (when (or ed xp)
        (let (xps xpl)
          (unless xp (setq xp 0))
          (setq xps (mfuncall '$string xp))
          (when (< xp 0) (setq xps (subseq xps 1)))
          (setq xpl (length xps))
          (and ed (> ed xpl)
            (setq xps (cs* (ms* (- ed xpl) #\0) xps)) )
          (setq s (cs* s "b" (if (< xp 0) "-" "+") xps)) ))
;;  
      (setq len (length s))
      (and wd ov (= (length ov) 1) (> len wd) 
        (setq s (ms* wd (character ov))) )
      (and wd (> wd len)
        (setq pc (if pc (character pc) #\ ) 
              s (cs* (ms* (- wd len) pc) s) ))
;;  
      s )))
;;
;; -------------------------------------------------------------------------- ;;
