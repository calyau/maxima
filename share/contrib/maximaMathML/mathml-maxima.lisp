;;;;;;;;;;;;;;;;; File:  mathml-maxima.lsp  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Purpose:   Enabling maxima to receive mathml contentent-coded input
;;
;; Usage:     compile this file with UNIX command
;;                %mc maxima-mp.lsp
;;             which produces mathml-maxima.o
;;
;;            load into MAXIMA by MAXIMA top-level comamnd
;;                loadfile("mathml-maxima.lsp");
;;
;; Author: Paul S. Wang
;; Date: 3/06/2000
;
; Authors:  Paul S. Wang, Kent State University
; This work was supported by NSF/USA.
; Permission to use this work for any purpose is granted provided that
; the copyright notice, author and support credits above are retained.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; uses these fuctions from nparse.lisp
;;;    SCAN-MACSYMA-TOKEN 
;;;    SCAN-NUMBER-BEFORE-DOT
;;;; with look-ahead disabled

(in-package :maxima)
(declaim (special *tag* *special-proc* *PARSE-STREAM* *in* parse-tyipeek))
(defvar *in* *PARSE-STREAM*  "input stream to read")
(setq parse-tyipeek nil)   ;; look-ahead in nparse.lisp
(defvar *tag* nil "tag of element returned by ml2ma")


;;;;; mltoma is the top-level mathml input function

(defun $mathml()
  (meval (mltoma *PARSE-STREAM*))
)

(defun mltoma(&optional *in*)
(prog(ans)
   (if (null *in*) (setq *in* t))
   (setq g (get-tag))
   (if (not (eq g 'math)) (return nil))
   (setq ans (ml2ma))
   (setq g (get-tag)) ;; this should be </math>
   (return ans)
))

(defun ml2ma ()
(prog(tag)
   (setq *tag* (setq tag (get-tag)))
   (return 
     (cond ((eq tag 'ci) (ml-ci))
	 ((eq tag 'cn) (ml-cn))
	 ((eq tag 'apply) (ml-apply))
	 ((memq tag '(bvar lowlimit uplimit))
	     (setq ans (ml2ma)) (get-tag) ans)
	 ((eq tag '/apply) nil)
	 (t (merror "unknown or invalid mathml tag: ~A" tag))
     )
   )
))

(defun ml-apply()
(prog(op *special-proc* ans)
    (setq op (get-op))
    (cond ((null op)
	 (if *special-proc* (return (apply *special-proc* nil))
		   (merror "internal error: null mct-proc"))
    ))
    (do ((operand (ml2ma) (ml2ma))) ;; returns nil after seeing </apply>
        ((null operand) (setq ans (cons op (nreverse ans)))) ;; done
        (setq ans (cons operand ans))
    )
    (return ans)
))

;; <apply>
;; <diff/> <apply> <fn>G</fn><ci>X</ci> </apply>
;; <bvar><ci>X</ci><degree><cn type="integer">2</cn></degree></bvar>
;; </apply>
;;(($DIFF) (($F) $X) $X 2)

(defun mctdiff()
  (let ((fn (ml2ma)) (var-deg (diff-bvar))) 
       (get-tag) ;; lose </apply>
       (cons (list '$diff) (cons fn var-deg)))
)

(defun mctintegrate()
(prog(var nt ll up grand)
    (setq var (get-bvar))
    (setq nt (ml2ma)) 
    (cond ((eq *tag* 'lowlimit)
	     (setq ll nt)
	     (setq up (ml2ma))
	     (cond ((eq *tag* 'uplimit)
	         (setq grand (ml2ma))
	         (get-tag) ;; lose </apply> for <int\>  
	         (return (list '($integrate) grand var ll up)))
	      (t (merror "definite intergral error"))
            ))
    )
    ;; indefinte integral
    (setq grand nt)
    (get-tag) ;; lose </apply>
    (return (list '($integrate) grand var))
))
	  
(defun get-bvar()
(prog(tag v)
     (setq tag (get-tag))
     (if (not (eq tag 'bvar)) (merror "Expecting bvar but got ~A" tag))
     (setq v (ml2ma))
     (get-tag) ;; lose </bvar>
     (return v)
))

(defun diff-bvar()
(prog(tag v d)
     (setq tag (get-tag))
     (if (not (eq tag 'bvar)) (merror "Expecting bvar but got ~A" tag))
     (setq v (ml2ma))
     (setq tag (get-tag))
     (if (not (eq tag 'degree)) (merror "Expecting degree but got ~A" tag))
     (setq d (ml2ma))
     (get-tag)(get-tag) ;; skip closing tags
     (return (list v d))
))
      
(defun ml-cn()
(prog(type number)
   (setq type (get-type))  ;; always has type
   (if (or (eq type 'integer) (equal type "integer")
		 (eq type 'float) (equal type "float"))
       (setq number (get-number))
   ) 
   (return number)
))

;; (DEFVAR PARSE-TYIPEEK () "T if there is a peek character.")
;; (DEFVAR PARSE-TYI	() "The peek character.")

(defun ml-ci()
(prog(parse-tyipeek a *PARSE-STREAM* type)
   (setq type (get-type)) ;; may or may not have type
   (cond ((or (eq type 'constant) (equal type "constant")) ;; math constants
	    (setq a (implode (get-token)))
            (get-token) ;; skip to >
	    (cond ((eq a '|&pi;|) (return '$%PI))
	          ((eq a '|&ee;|) (return '$%E))
	          ((eq a '|&ii;|) (return '$%i))
	          ((eq a '|&gamma;|) (return '$%gamma))
	    )
   ))
;; normal identifier
;;(setq *PARSE-STREAM* *in*)
;;(setq a (SCAN-MACSYMA-TOKEN))  ;; $ prefixed
  (setq a (read-from-string
	   (concatenate 'string "$"  type)))  ;; type is string
   (get-token) ;; skip to >
   (return a)
))

(defun get-number()
(prog(parse-tyipeek n  *PARSE-STREAM*)
   (setq *PARSE-STREAM* *in*)
   (setq n (SCAN-number-before-dot nil))
   (get-token) ;; skip to >
   (return n)
))

(defun get-type()
(prog(tk)
   (setq tk (get-str #\=))
   (if (not (eq (read-from-string tk) 'type)) 
       (return tk)  ;; string for next token
       (return (get-atag))    ;; atom for type
   )
))
   
;; returns next non-white non #\> char (or -1?)
(DEFUN next-char ()
(DO (c) (NIL)      ; Gobble whitespace
    (IF (IMEMBER (setq c (TYI *in* -1))
           '(#\> #\TAB #\SPACE #\Linefeed #\return #\Page))
        nil
        (RETURN c)
    )
))

(defun get-tag(&optional endc)
(prog(tag c)
   (setq c (next-char))
   (if (not (char= c #\<)) (return nil))
   (return (get-atag endc))
))

(defun get-atag(&optional endc)
(prog(str)
    (setq str (get-str endc))
    (if (null str) (return nil))
    (return (read-from-string str))
))

(defun get-str(&optional endc)
(prog(str)
   (setq str (get-token endc))
   (if str
      (return (string-downcase
           (symbol-name (IMPLODE str))))
      (return nil)
   )
))

;; returns list of chars for next token
(defun get-token (&optional endc)
  (DO ((C (TYI *in* -1) (TYI *in* -1))
       (L () (CONS C L)))
      ( (or (equal c -1) (and endc (char= C endc))
           (imember c '(#\< #\> #\TAB #\SPACE #\Linefeed #\return #\Page))
        )
        (NREVERSE (OR L (NCONS (TYI *in* -1))))
      ) ; Read at least one char ...
  )
)

(defun get-op ()
(prog(op mop opa)
    (setq op (get-tag))
    (cond ((eq op 'fn) 
             (setq op (get-str))
	     (cond ((null op)
		(merror "get-op: invalid null function")
	     ))
	     (setq opa (read-from-string op))
             (setq opa (get opa 'mmfun))
             (get-token)
	     (if opa (return (list opa)))
             (return (list (read-from-string
		  (concatenate 'string "$" op))))
          )
	  ((setq proc (get op 'mct-proc)) 
	     (setq *special-proc* proc)
	     (return nil)
	  )
    )
    (setq mop (get op 'mmfun))
    (if mop (return (list mop))
	    (return (list op))   ;; should not reach here
    )
))

;;;(defmacro upcase (operator)
;;;`(setq operator (intern (string-upcase (string ,operator)))))

(defun set-table (arg)
(prog(a b)
  (cond ((equal (length arg) 2)
          (setq a (cadadr arg))
	  (setq b (caadr arg))
          (if (stringp a) (setq a (read-from-string a)))
          (setf (get a b) (car arg))
	)
        ((equal (length arg) 3)
	  (setq arg (cdr arg))
	  (setq b (car arg) a (cadr arg))
	  (setq a (cadr a))
          (if (stringp a) (setq a (read-from-string a)))
	  (setf (get a (car b)) (cadr b))
	)
  )
))

;;;;;;;;;;; tables ;;;;;;;;;;;;
;;(set-table '(%sin  (mmfun "sin/")))
;;(set-table '(%cos  (mmfun "cos/")))
;;(set-table '(%tan  (mmfun "tan/")))
;;(set-table '(%cot  (mmfun "cot/")))
;;(set-table '(%sec  (mmfun "sec/")))
;;(set-table '(%csc  (mmfun "csc/")))

;;(set-table '(%asin  (mmfun "arcsin/")))
;;(set-table '(%acos  (mmfun "arccos/")))
;;(set-table '(%atan  (mmfun "arctan/")))
;;(set-table '(%acot  (mmfun "acot/")))
;;(set-table '(%asec  (mmfun "asec/")))
;;(set-table '(%acsc  (mmfun "acsc/")))
;;(set-table '(%sinh  (mmfun "sinh/")))
;;(set-table '(%cosh  (mmfun "cosh/")))
;;(set-table '(%tanh  (mmfun "tanh/")))
;;(set-table '(%coth  (mmfun "coth/")))
;;(set-table '(%sech  (mmfun "sec/")))
;;(set-table '(%csch  (mmfun "csch/")))


;;(set-table '(%asinh  (mmfun "asinh/")))
;;(set-table '(%acosh  (mmfun "acosh/")))
;;(set-table '(%atanh  (mmfun "atanh/")))
;;(set-table '(%acoth  (mmfun "acoth/")))
;;(set-table '(%asech  (mmfun "asec/")))
;;(set-table '(%acsch  (mmfun "acsch/")))

(set-table '(%ln  (mmfun "ln/")))
(set-table '(%log  (mmfun "log/")))

(set-table '($sin  (mmfun "sin/")))
(set-table '($cos  (mmfun "cos/")))
(set-table '($tan  (mmfun "tan/")))
(set-table '($cot  (mmfun "cot/")))
(set-table '($sec  (mmfun "sec/")))
(set-table '($csc  (mmfun "csc/")))

(set-table '($asin  (mmfun "arcsin/")))
(set-table '($acos  (mmfun "arccos/")))
(set-table '($atan  (mmfun "arctan/")))
(set-table '($acot  (mmfun "acot/")))
(set-table '($asec  (mmfun "asec/")))
(set-table '($acsc  (mmfun "acsc/")))

(set-table '($sinh  (mmfun "sinh/")))
(set-table '($cosh  (mmfun "cosh/")))
(set-table '($tanh  (mmfun "tanh/")))
(set-table '($coth  (mmfun "coth/")))
(set-table '($sech  (mmfun "sec/")))
(set-table '($csch  (mmfun "csch/")))

(set-table '($asinh  (mmfun "asinh/")))
(set-table '($acosh  (mmfun "acosh/")))
(set-table '($atanh  (mmfun "atanh/")))
(set-table '($acoth  (mmfun "acoth/")))
(set-table '($asech  (mmfun "asec/")))
(set-table '($acsch  (mmfun "acsch/")))
(set-table '($ln  (mmfun "ln/")))
(set-table '($log  (mmfun "log/")))


;;;;; containers
;;(set-table '(mlist (mct-proc mctlist)))
;;(set-table '($matrix (mct-proc  mctmatrix)))
;;(set-table '($vector (mct-proc  mctvector)))

;;;;;;; Operators and functions
(set-table '(mand  (mmfun "and/")))
(set-table '(mor  (mmfun "or/")))
(set-table '(mnot  (mmfun "not/")))
(set-table '($xor  (mmfun "xor/")))

(set-table '(mplus  (mmfun "plus/")))
(set-table '(mminus  (mmfun "minus/")))
;;(set-table '($minus (mmfun "minus/")))
;;(set-table '(mdif  (mmfun "minus/")))
(set-table '($remainder  (mmfun "rem/")))
(set-table '($max  (mmfun "max/")))
(set-table '($min  (mmfun "min/")))
(set-table '(mfactorial  (mmfun "factorial/")))
(set-table '(mabs (mmfun "abs/")))
(set-table '(%abs (mct-proc abs)))
;;(set-table '(mnctimes  (mmfun "times/ type=\"noncommutative\"")))
(set-table '(mtimes  (mmfun "times/")))
(set-table '(mexpt (mmfun "power/")))
(set-table '(mquotient (mmfun "quotient/"))) 
(set-table '(%sqrt (mmfun "sqrt/")))
(set-table '(mquote  (mmfun "quote/")))

(set-table '(mgreaterp  (mct-proc relation) (mmfun "gt/")))
(set-table '(mgeqp (mct-proc relation)  (mmfun "geq/")))
(set-table '(mequal (mct-proc relation)  (mmfun "eq/")))
(set-table '(mnotequal (mct-proc relation)  (mmfun "neq/")))
(set-table '(mleqp (mct-proc relation)  (mmfun "leq/")))
(set-table '(mlessp (mct-proc relation)  (mmfun "lt/")))

(set-table '(mdefine (mct-proc def-fun)))

(set-table '(msetq  (mmfun "&Assign;")))
;;(set-table '(mset  (mmfun "&Assign;")))  ;;; This is not math
;;(set-table '(marrow  (mmfun "&RightArrow;")))
;;(set-table '(mrarrow  (mmfun "&RightArrow;")))
;;(set-table '(%at (mct-proc mPr-at)))
;;(set-table '($at (mct-proc mPr-at)))
;;(set-table '($det (mct-proc mPr-det)))
;;(set-table '(%determinant (mct-proc det)))
;;(set-table '($binomial (mct-proc binomial)))
;;(set-table '(%binomial (mct-proc binomial)))

(set-table '(%sum (mct-proc sumprod)(mmfun "sum/")))
;;(set-table '($sum (mct-proc sumprod)(mmfun "sum/")))
;;(set-table '($product (mct-proc sumprod)(mmfun "product/")))
(set-table '(%product (mct-proc sumprod)(mmfun "product/")))
;;(set-table '($integrate (mct-proc mctintegrate)(mmfun "int/")))
(set-table '(%integrate (mct-proc mctintegrate)(mmfun "int/")))
(set-table '($diff (mct-proc mctdiff)(mmfun "diff/")))
;;(set-table '(%derivative (mct-proc mctdiff)(mmfun "diff/")))
(set-table '($limit (mct-proc mctlimit)(mmfun "limit/")))
;;(set-table '(%limit (mct-proc mctlimit)(mmfun "limit/")))

;;(set-table '(mprog (mmfun "block")))
;;(set-table '($block (mmfun "block")))
;;(set-table '($$boldif (mmfun "if/")))
;;(set-table '($$boldthen (mmfun "then/")))
;;(set-table '($$boldelse (mmfun "else/")))
