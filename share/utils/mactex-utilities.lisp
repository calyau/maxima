
;; Additional mactex utilities.
;; All functions except texput are based on code by Richard J. Fateman, 
;; copyright 1987. Fateman's code was ported to Common Lisp by William 
;; Schelter.  The texput function was written by Barton Willis.


;; texput is a maxima-level interface to putprop that allows a user
;; to modify the behavior of maxima's tex function. Here are a few eamples:

;;        (C1) texput(me,"\\mu_e");
;;        (D1) 				     \mu_e
;;        (C2) tex(me);
;;                $$\mu_e$$

;;        (C4) texput("<<",[" \\langle ", " \\rangle "],matchfix);
;;        (D4) 		         \langle ( \rangle)
;;        (C5) matchfix("<<",">>");
;;        (D5) 				     "<<"
;;        (C6) tex(<<a>>);
;;              $$ \langle a \rangle $$
;;        (D6) 				     FALSE
;;        (C7) tex(<<a,b>>);
;;              $$ \langle a , b \rangle $$

;;  To change the argument separtor from a comma to a character ch,
;;  make the third element of the second argument of texput that character.  For
;;  example, to make Dirac brackets, use

;;        (C12) texput("<<",[" \\langle ", " \\rangle "," \\, | \\,"],matchfix);
;;        (D12) 		         \langle ( \rangle ,  \, | \,)
;;        (C13) tex(<<a>>);
;;                    $$ \langle a \rangle $$
;;        (D13) 				     FALSE
;;        (C14) tex(<<a,b>>);
;;                      $$ \langle a \, | \,b \rangle $$

;;        (C15) texput(grad," \\nabla ",prefix);
;;        (D15) 				      70
;;        (C16) prefix("grad");
;;        (D16) 				    "GRAD"
;;        (C17) tex(grad f);
;;                    $$ \nabla f$$

;;        (C19) texput("~"," \\times ",infix);
;;        (D19) 				      180
;;        (C20) tex(a~b);
;;                    $$a \times b$$

;;        (C22) postfix(@);
;;        (D22) 				      "@"
;;        (C23) texput("@","!!",postfix);
;;        (D23) 				      160
;;        (C24) tex(x @);
;;              $$x!!$$



;; If you want LaTeX style quotients, first load mactex and second
;; define tex-mquotient as follows

(defun tex-mquotient (x l r)
  (if (or (null (cddr x)) (cdddr x)) (wna-err (caar x)))
  (setq l (tex (cadr x) (append l '("\\frac{")) nil 'mparen 'mparen)
	r (tex (caddr x) (list "}{") (append '("}") r) 'mparen 'mparen))
  (append l r))

;; The following defprop controls the space between terms in a product. 
;; To use thin spaces, use 
;;    (defprop mtrimes "\\, " texsym)
;; and to use no space at all, use

(defprop mtimes " " texsym)

;; To use amsmath style matrices, define tex-matrix as
;; (to TeX the code, you'll need to \usepackage{amsmath})

(defun tex-matrix (x l r) ;; matrix looks like ((mmatrix)((mlist) a b) ...)
  (append l `("\\begin{pmatrix}")
	 (mapcan #'(lambda(y)
			  (tex-list (cdr y) nil (list " \\\\ ") " & "))
		 (cdr x))
	 '("\\end{pmatrix}") r))

;; Here is a version of tex-limit that doesn't ignore the direction of
;; the limit; it uses down and up arrows to indicate the direction.

(defun tex-limit(x l r)
  (let* ((s1 (tex (cadr x) nil nil 'mparen rop))
	 (arrow
	  (cond ((eq (nth 4 x) '$minus)
		 " \\,  \\downarrow  \\,")
		((eq (nth 4 x) '$plus)
		 " \\, \\uparrow \\, " )
		(t
		  " \\rightarrow ")))
	 (subfun  (subst arrow '=
			 (tex `((mequal simp) ,(caddr x),(cadddr x))
			      nil nil 'mparen 'mparen))))
    (append l `("\\lim_{" ,@subfun "}{" ,@s1 "}") r)))

;; If you use texput with the optional argument separator 
;; argument, you'll need to use this hacked version of tex-matchfix.

(defun tex-matchfix (x l r)
  (setq l (append l (car (texsym (caar x))))
	;; car of texsym of a matchfix operator is the lead op
	r (append (list (nth 1 (texsym (caar x)))) r)
	;; cdr is the trailing op
	x (tex-list (cdr x) nil r (or (nth 2 (texsym (caar x))) " , ")))
  (append l x))

