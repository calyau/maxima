;;; -*- Mode:LISP; Package:MACSYMA -*-
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; Comments:
;;
;; The ;tensor package was downcased, cleaned up, and moving frames
;; functionality was added by Viktor Toth (http://www.vttoth.com/).
;;
;; As of November, 2004, the naming conventions in this package now
;; correspond with the naming conventions in commercial MACSYMA.
;;

(in-package "MAXIMA")

(macsyma-module itensor) ;; added 9/24/82 at UCB

(cond (($get '$itensor '$version) (merror "ITENSOR already loaded"))
      (t ($put '$itensor '$v20041126 '$version))
)
;	** (c) Copyright 1981 Massachusetts Institute of Technology **

;    Various functions in Itensor have been parceled out to separate files. A
;    function in one of these files will only be loaded in (automatically) if
;    explicitly used in the Macsyma. (It is necessary to have first loaded in
;    ITENSOR FASL for this autoloading to take place.) The current status of
;    these separate files are:

;    Filename          Macsyma Functions
;    --------          -----------------
;    CANTEN FASL       CANTEN, CONCAN, IRPMON
;    GENER FASL        IC_CONVERT, MAKEBOX, AVERAGE, CONMETDERIV, FLUSH1DERIV,
;                      IGEODESIC_COORDS
;    SYMTRY FASL       CANFORM, DECSYM, DISPSYM, REMSYM

#+maclisp(progn
(putprop '$IC_CONVERT '((dsk tensor) gener fasl) 'autoload)
(putprop '$DECSYM '((dsk tensor) symtry fasl) 'autoload)
(putprop '$CANFORM '((dsk tensor) symtry fasl) 'autoload)
(putprop '$CANTEN '((dsk tensor) canten fasl) 'autoload)
(putprop '$MAKEBOX '((dsk tensor) gener fasl) 'autoload)
(putprop '$IGEODESIC_COORDS '((dsk tensor) gener fasl) 'autoload)
(putprop '$CONMETDERIV '((dsk tensor) gener fasl) 'autoload))
#+Franz (progn
(putprop '$IC_CONVERT (concat vaxima-main-dir '|//tensor//gener|) 'autoload)
(putprop '$DECSYM (concat vaxima-main-dir  '|//tensor//symtry| )'autoload)
(putprop '$CANFORM (concat vaxima-main-dir  '|//tensor//symtry| )'autoload)
(putprop '$CANTEN (concat vaxima-main-dir  '|//tensor//canten| )'autoload)
(putprop '$MAKEBOX (concat vaxima-main-dir  '|//tensor//gener| )'autoload)
(putprop '$IGEODESIC_COORDS  (concat vaxima-main-dir '|//tensor//gener| )'autoload)
(putprop '$CONMETDERIV  (concat vaxima-main-dir '|//tensor//gener| )'autoload))

#+cl
(progn
(autof '$IC_CONVERT '|gener|)
(autof '$DECSYM '|symtry|)
(autof '$CANFORM '|symtry|)
(autof '$CANTEN '|canten|)
(autof '$MAKEBOX '|gener|)
(autof '$IGEODESIC_COORDS '|gener|)
(autof '$CONMETDERIV '|gener|)
(autof '$NAME '|canten|)
(autof '$CONTI '|canten|)
(autof '$COVI '|canten|)
(autof '$DERI '|canten|)
)
#+cl
(eval-when (eval compile)
	   (defmacro fixp (x) `(typep ,x 'fixnum)))



#+maclisp ($UUO) 	                        ;Restore calls to SDIFF so it can be redefined	

(DECLARE-TOP (SPECIAL SMLIST $IDUMMYX $VECT_COORDS $IMETRIC $ICOUNTER $DIM
		  $CONTRACTIONS $COORD $ALLSYM $METRICCONVERT $IFRAME_FLAG
          $ITORSION_FLAG)
	 (*LEXPR $RENAME $DIFF $COORD $REMCOORD $LORENTZ_GAUGE))

(SETQ $IDUMMYX '$%                   ;Prefix for dummy indices
      $ICOUNTER 0.                   ;Dummy variable numeric indexs
      SMLIST '(MLIST SIMP)           ;Simplified MLIST header
      $VECT_COORDS NIL               ;Used when differentiating w.r.t. a number
      $COORD '((MLIST SIMP))         ;Objects treated liked coordinates in DIFF
      $ALLSYM NIL                    ;If T then all indexed objects symmetric
      $METRICCONVERT T               ;Flag used by $IC_CONVERT
      $IFRAME_FLAG NIL
      $ITORSION_FLAG NIL)

;(DEFUN IFNOT MACRO (CLAUSE) (CONS 'OR (CDR CLAUSE)))
(DEFmacro IFNOT  (&rest CLAUSE) `(or ,@ clause))

;(DEFUN M+OR*OR^P MACRO (CL)
(defmacro M+OR*OR^P (&whole cl &rest ign) ign
       (SUBST (CADR CL)
	      'X
	      '(MEMQ (CAAR X) '(MTIMES MPLUS MEXPT))))

(DEFMFUN $IDUMMY nil                              ;Sets arguments to dummy indices
       (progn (setq $ICOUNTER (1+ $ICOUNTER))
              (concat $IDUMMYX $ICOUNTER)))

(DEFPROP $KDELTA ((/  . / )) CONTRACTIONS)

;KDELTA has special contraction property because it contracts with any indexed
;object.

(meval '(($DECLARE) %KDELTA $CONSTANT))                        ;So derivative will be zero

(SETQ $DIM 4. $CONTRACTIONS '((MLIST SIMP))) 

(DEFMFUN $DEFCON N            ;Defines contractions: A contracts with B to form C
       ((LAMBDA (A)
	 (ADD2LNC A $CONTRACTIONS)
	 (PUTPROP
	  A
	  (CONS (COND ((= N 1.) '(/  . / ))
		      ((= N 3.) (CONS (ARG 2.) (ARG 3.)))
		      (T (merror "DEFCON takes 1 or 3 arguments")))
		(ZL-GET A 'CONTRACTIONS))
	  'CONTRACTIONS)
	 '$DONE)
	(ARG 1.))) 

(DEFMSPEC $DISPCON (A) (SETQ A (CDR A))
  ;;Displays contraction definitions
       ((LAMBDA (TMP) 
	 (AND (EQ (CAR A) '$ALL) (SETQ A (CDR $CONTRACTIONS)))
	 (CONS
	  SMLIST
	  (MAPCAR 
	   #'(LAMBDA (E) 
	     (COND ((SETQ TMP (ZL-GET E 'CONTRACTIONS))
		    (CONS SMLIST
			  (MAPCAR #'(LAMBDA (Z) 
					   (COND ((EQ (CAR Z)
						      '/ )
						  (LIST SMLIST E))
						 (T (LIST SMLIST
							  E
							  (CAR Z)
							  (CDR Z)))))
				  TMP)))
		   (T '((MLIST SIMP)))))
	   A)))
	NIL)) 

(DEFMSPEC $REMCON (A) (SETQ A (CDR A))
  ;;Removes contraction definitions
       (AND (EQ (CAR A) '$ALL) (SETQ A (CDR $CONTRACTIONS)))
       (CONS SMLIST (MAPC (FUNCTION (LAMBDA (E) (ZL-REMPROP E 'CONTRACTIONS)
					    (DELQ E $CONTRACTIONS)))
			  A)))

(DEFUN GETCON (E)
  ;; Helper to obtain contractions on both the noun and verb form of E
	(COND ((AND (SYMBOLP E) (EQ (GETCHAR E 1) '%))  (ZL-GET ($VERBIFY E) 'CONTRACTIONS))
		(T (ZL-GET E 'CONTRACTIONS))
	)
)

(DEFUN RPOBJ (E)                  ;"True" if an indexed object and not a matrix
       (COND ((AND (NOT (ATOM E)) (EQ (CAAR E) 'MQAPPLY)) (RPOBJ (CDR E)))
	     (T 
       (AND (NOT (ATOM E))
	    (NOT (EQ (CAAR E) '$MATRIX))
	    ($LISTP (CADR E))
	    (COND ((CDDR E) ($LISTP (CADDR E)))
		  (T (NCONC E '(((MLIST SIMP))))))))))
                                          ;Transforms F([...]) into F([...],[])

;RPOBJ is the predicate for indexed objects. In the case of no contravariant
;components, it tacks a null list on.

(deff $tenpr #'rpobj)

(DEFMFUN $IMETRIC (V) (SETQ $IMETRIC V) ($DEFCON V) ($DEFCON V V '$KDELTA))

(DEFUN MYSUBST0 (NEW OLD)                  ;To reuse subparts of old expression
       (COND ((ALIKE1 NEW OLD) OLD) (T NEW))) 

(DEFUN COV (A B)                            ;COV gives covariant form of metric
       (COND ((BOUNDP '$IMETRIC)
	      (MEVAL (LIST (NCONS $IMETRIC)
			   (LIST SMLIST A B)
			   '((MLIST SIMP)))))
	     (T (merror "Name of metric must be specified"))))

(DEFUN CONTR (A B)                      ;CONTR gives contraviant form of metric
       (COND ((BOUNDP '$IMETRIC)
	      (MEVAL (LIST (NCONS $IMETRIC)
			   '((MLIST SIMP))
			   (LIST SMLIST A B))))
	     (T (merror "Name of metric must be specified"))))

(DEFUN DIFFCOV (A B D)
	(COND ((BOUNDP '$IMETRIC)
		(MEVAL (LIST (NCONS $IMETRIC)
			   (LIST SMLIST A B)
			   '((MLIST SIMP))
				D
			)

		))
		(T (merror "Name of metric must be specified"))))

(defmfun $ichr1 nargs                   ; Christoffel-symbol of the first kind
  (prog (a b c)
    (cond 
      (
        (> nargs 2) ; Derivative indices present; use idiff() to resolve
        (return
          (meval
            (cons
              '$idiff
              (cons
                ($ichr1 (arg 1) (arg 2))
                (apply
                  #'append
                  (mapcar #'(lambda (e) (list e 1)) (cddr (listify nargs)))
                )
              )
            )
          )
        )
      )
      (
        (> nargs 1)
        (and (eq 1 (length (arg 2))) (return ($ichr1 (arg 1))))
        (merror "ichr1 cannot have contravariant indices")
      )
      (t            ; G_abc = 1/2*(g_ba,c+g_ca,b-g_bc,a)
        (setq a (cadddr (arg 1)) b (cadr (arg 1)) c (caddr (arg 1)))
        (return
          (list
            '(mtimes)
            '((rat simp) 1. 2.)
            (list
              '(mplus)
              (diffcov b a c)
              (diffcov c a b)
              (list '(mtimes) -1. (diffcov b c a))
            )
          )
        )
      )
    )
  )
)

(defmfun $ichr2 nargs                   ; Christoffel-symbol of the second kind
  (prog (a b c d) 
    (cond
      (
        (> nargs 2) ; Derivative indices present; use idiff() to resolve
        (return
          (meval
            (cons
              '$idiff
              (cons
                ($ichr2 (arg 1) (arg 2))
                (apply
                  #'append
                  (mapcar #'(lambda (e) (list e 1)) (cddr (listify nargs)))
                )
              )
            )
          )
        )
      )
      (t            ; G_ab^c=g^cd*G_abd
        (setq a (cadr (arg 1)) b (caddr (arg 1)) c (cadr (arg 2)))
        (return
          (do
            ((flag) (l (append (cdr (arg 1)) (cdr (arg 2)))))
            (flag
              (list '(mtimes) (contr c d) ($ichr1 (list smlist a b d)))
            )
            (setq d ($idummy))
            (and (not (memq d l)) (setq flag t))
          )
        )
      )
    )
  )
)

(defmfun $icurvature (l1 l2) 
  (prog (i j k h r) 
    (setq r ($idummy) i (cadr l1) k (caddr l1) h (cadddr l1) j (cadr l2))
    (return
      (list
        '(mplus)
        (idiff (list (diffop) (list smlist i k) l2) h)
        (list
          '(mtimes) -1.
          (idiff (list (diffop) (list smlist i h) (list smlist j)) k)
        )
        (list
          '(mtimes)
          (list (diffop) (list smlist i k) (list smlist r))
          (list (diffop) (list smlist r h) l2)
        )
        (list
          '(mtimes)
          -1.
          (list (diffop) (list smlist i h) (list smlist r))
          (list (diffop) (list smlist r k) l2)
        )
        (cond
          (
            $iframe_flag
            (list
              '(mtimes) -1.
              (list '($ifb) (list smlist k h) (list smlist r))
              (list '($icc2) (list smlist r i) (list smlist j))
            )
          )
          (t 0.)
        )
      )
    )
  )
) 

(DEFUN COVSUBST (X Y RP)       ;Substitutes X for Y in the covariant part of RP
       (CONS (CAR RP) (CONS (SUBST X Y (CADR RP)) (CDDR RP)))) 

(DEFUN CONSUBST (X Y RP)   ;Substitutes X for Y in the contravariant part of RP
       (CONS (CAR RP)
	     (CONS (CADR RP)
		   (CONS (SUBST X Y (CADDR RP)) (CDDDR RP))))) 

(DEFUN DERSUBST (X Y RP)   ;Substitutes X for Y in the derivative indices of RP
       (NCONC (LIST (CAR RP) (CADR RP) (CADDR RP))
	      (SUBST X Y (CDDDR RP)))) 

;; COVARIANT DIFFERENTIATION
;; As of November, 2004, COVDIFF now takes into account the value of
;; iframe_flag. If true, COVDIFF uses the coefficients icc2 in place
;; of the Christoffel-symbols ichr2.

(defun diffop ()                ; ichr2 or icc2 depending on iframe_flag
  (cond
    (
      (or $iframe_flag $itorsion_flag)
      '($icc2 simp)
    ) 
    (t '($ichr2 simp))
  )
)

(declare-top (special x temp d)) 

(defmfun $covdiff nargs
  (prog
    (x e temp d i)
    (and (< nargs 2) (merror "COVDIFF must have at least 2 args"))
    (setq i 2 e (arg 1))
    again (setq x (arg i) e (covdiff e) i (1+ i))
    (and (> i nargs) (return e))
    (go again)
  )
)

(defun covdiff (e)                      ; The covariant derivative...
  (setq d ($idummy))
  (cond
    (               ; is the partial derivative for scalars (*** torsion?)
      (or (atom e) (eq (caar e) 'rat))
      (idiff e x)
    )
    (
      (rpobj e)
      (setq temp
        (mapcar 
          #'(lambda (v)
            (list '(mtimes)
              (list (diffop) (list smlist d x) (list smlist v))
              (consubst d v e)
            )
          )
          (cdaddr e)
        )
      )
      (simplus
        (cons
          '(mplus)
          (cons
            (idiff e x)
              (cond
                (
                  (or (cdadr e) (cdddr e))
                  (cons (list '(mtimes) -1.  (cons '(mplus)
                    (nconc
                      (mapcar 
                        #'(lambda (v) 
                          (list '(mtimes)
                              (list
                                (diffop)
                                (list smlist v x)
                                (list smlist d)
                              )
                              (covsubst d v e)
                            )
                          )
                          (cdadr e)
                        )
                        (mapcar 
                          #'(lambda (v) 
                            (list
                              '(mtimes)
                              (list 
                                (diffop)
                                (list smlist v x)
                                (list smlist d)
                              )
                              (dersubst d v e)
                            )
                          )
                          (cdddr e)
                        )
                      )
                    )
                  )
                  temp
                )
              )
              (t temp)
            )
          )
        )
        1. t
      )
    )
    (
      (eq (caar e) 'mtimes)     ; (a*b)'
      (simplus
        (covdifftimes (cdr e) X)
        1 T
      )
    )
    (
      (eq (caar e) 'mplus)      ; (a+b)'=a'+b'
      (simplifya
        (cons
          '(mplus)
          (mapcar 'covdiff (cdr e))
        )
        nil
      )
    )
    (
      (eq (caar e) 'mexpt)      ; (a^b)'=b*a^(b-1)*a'
      (simptimes
        (list
          '(mtimes)
          (caddr e)
          (list
            '(mexpt)
            (cadr e)
            (list '(mplus) -1. (caddr e))
          )
          ($covdiff (cadr e) x)
        )
        1. nil
      )
    )
    (
      (eq (caar e) 'mequal)
      (list (car e) (covdiff (cadr e)) (covdiff (caddr e)))
    )
    (t (merror "Not acceptable to COVDIFF: ~M" (ishow e)))
  )
)

(defun covdifftimes (l x) 
  (prog (sp left out) 
    (setq out (ncons '(mplus)))
    loop (setq sp (car l) l (cdr l))
    (nconc out
      (list
        (simptimes
          (cons '(mtimes) (cons ($covdiff sp x) (append left l)))
          1. t
        )
      )
    )
    (cond ((null l) (return out)))
    (setq left (nconc left (ncons sp)))
    (go loop)
  )
) 

(declare-top (unspecial r temp d)) 

(defun vecdiff (v i j d) ;Add frame bracket contribution when iframe_flag:true
  (cond
    (
      $iframe_flag
      (cons
        '(mplus simp)
        (list
          (list (list v) '((mlist)) (list '(mlist) i) j)
          (list
            '(mtimes simp)
            (list (list v) '((mlist)) (list '(mlist) d))
            (list
              '(mtimes simp)
              -1.
              (list '(%ifb) (list '(mlist) d j) (list '(mlist) i))
            )
          )
        )
      )
    )
    (t
      (list (list v) '((mlist)) (list '(mlist) i) j)
    )
  )
)

(defun liediff (v e n)
  (cond
    ((not (symbolp v)) (merror "~M is not a symbol" v))
    (
      (or (atom e) (eq (caar e) 'rat)) ; Scalar field
                                       ; v([],[%1])*idiff(e,%1)
      (let
        ((dummy (implode (nconc (exploden $idummyx) (exploden n)))))
        (list
          '(mtimes) (list (list v) '((mlist)) (list '(mlist) dummy))
          ($idiff e dummy)
        )
      )
    )
    (
      (rpobj e)                        ; Tensor field

;     Dummy implementation for logic tests
;     (list '(%liediff) v e)

;     Shall the dummy index be in ICOUNTER sequence? Probably yes.
;     (let ((dummy (implode (nconc (exploden $idummyx) (exploden n)))))
      (let
        (
          (dummy ($idummy))
          (dummy2
            (cond
              ($iframe_flag ($idummy))
              (t nil)
            )
          )
        )
        (
          append
          (list
            '(mplus) 0
            (list
              '(mtimes)                ; e([...],[...],%1)*v([],[%1])
              (list (list v) '((mlist)) (list '(mlist) dummy))
              ($idiff e dummy)
            )
          )
          (maplist
            #'(lambda (s)              ; e([..%1..],[...])*v([],[%1],k)
              (list
                '(mtimes)
                (append
                  (list
                    (car e)
                    (cons
                      '(mlist)
                      (append
                        (subseq (cdadr e) 0 (- (length (cdadr e)) (length s)))
                        (cons dummy (cdr s))
                      )
                    )
                    (caddr e)
                  )
                  (cdddr e)
                )
;                (list (list v) '((mlist)) (list '(mlist) dummy) (car s))
                (vecdiff v dummy (car s) dummy2)
              )
            )
            (cdadr e)
          )
          (maplist
            #'(lambda (s)              ; +e([...],[...],..%1..)*v([],[%1],k)
              (list
                '(mtimes)
                (append
                  (list (car e) (cadr e) (caddr e))
                  (subseq (cdddr e) 0 (- (length (cdddr e)) (length s)))
                  (cons dummy (cdr s))
                )
;                (list (list v) '((mlist)) (list '(mlist) dummy) (car s))
                (vecdiff v dummy (car s) dummy2)
              )
            )
            (cdddr e)
          )
          (maplist
            #'(lambda (s)             ; -e([...],[..%1..])*v([],[k],%1)
              (list
                '(mtimes) -1
                (append
                  (list (car e) (cadr e)
                    (cons
                      '(mlist)
                      (append
                        (subseq (cdaddr e) 0 (- (length (cdaddr e)) (length s)))
                        (cons dummy (cdr s))
                      )
                    )
                  )
                  (cdddr e)
                )
;                (list (list v) '((mlist)) (list '(mlist) (car s)) dummy)
                (vecdiff v (car s) dummy dummy2)
              )
            )
            (cdaddr e)
          )
        )
      )
    )
    (
      (eq (caar e) 'mtimes)           ; Leibnitz rule
                                      ; Lv(cadr e)*(cddr e)+(cadr e)*Lv(cddr e)
      (list
        '(mplus)
        (cons '(mtimes) (cons (liediff v (cadr e) n) (cddr e)))
        (cons
          '(mtimes)
          (list
            (cadr e)
            (liediff
              v
              (cond ((cdddr e) (cons '(mtimes) (cddr e))) (t (caddr e)))
              n
            )
          )
        )
      )
    )
    (
      (eq (caar e) 'mplus)            ; Linearity
;     We prefer mapcar to iteration, but the recursive code also works
;     (list
;       '(mplus)
;       (liediff v (cadr e) n)
;       (liediff v (cond ((cdddr e) (cons '(mplus) (cddr e))) (t (caddr e))) n)
;     )
      (cons '(mplus) (mapcar #'(lambda (u) (liediff v u n)) (cdr e)))
    )
    (t (merror "~M is not a tensorial expression liediff can handle" e))
  )
)

(defmfun $liediff (v e) (liediff v e 1))

(defmfun $rediff (x) (meval '(($ev) x $idiff)))

(defmfun $evundiff (x) ($rediff ($undiff x)))

(defmfun $undiff (x) 
  (cond
    ((atom x) x)
    (
      (rpobj x)
      (cond
        (
          (cdddr x)
          (nconc
            (list '(%idiff) (list (car x) (cadr x) (caddr x)))
            (putinones (cdddr x))
          )
        )
        (t x)
      )
    )
    (t
      (mysubst0
        (simplifya (cons (ncons (caar x)) (mapcar '$undiff (cdr x))) t)
        x
      )
    )
  )
)

(defun putinones (e) 
  (cond
    ((cdr e) (cons (car e) (cons 1. (putinones (cdr e)))))
    (t (list (car e) 1.))
  )
) 



(DEFMFUN $LORENTZ_GAUGE n
       (cond ((equal n 0) (merror "LORENTZ_GAUGE requires at least one argument"))
	     ((equal n 1) (lorentz (arg 1) nil))
	     (t (lorentz (arg 1)
			 ((lambda (l) (cond ((sloop for v in  l
						     always (symbolp v)) l)
					    (t (merror
"Invalid tensor name(s) in argument to LORENTZ_GAUGE"))))
			  (listify (f- 1 n)))))))

;Lorentz contraction of E: indexed objects with a derivative index matching a
;contravariant index become 0. If L is NIL then do this for all indexed objects
;otherwise do this only for those indexed objects whose names are members of L.

(defun LORENTZ (e l)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((and (or (null l) (memq (caar e) l))
			  (intersect (cdaddr e) (cdddr e)))
		     0.)
		    (t e)))
	     (t (mysubst0
		 (simplifya
		  (cons (ncons (caar e))
			(mapcar (function (lambda (q) (lorentz q l)))
				(cdr e)))
		  t) e))))

(DEFUN LESS (X Y)                                         ;Alphanumeric compare
       (COND ((NUMBERP X)
	      (COND ((NUMBERP Y) (< X Y))
		    (T (ALPHALESSP (ASCII X) Y))))
	     (T (COND ((NUMBERP Y) (ALPHALESSP X (ASCII Y)))
		      (T (ALPHALESSP X Y)))))) 

;; Christoffels contains all Christoffel-like symbols: i.e., symbols
;; that make sense only with certain index patterns. These symbols are
;; excluded from contractions, because those would produce illegal
;; index combinations (e.g., ichr1([a,b],[c])). However, special rules
;; exist to convert a covariant symbol into a mixed symbol and vice
;; versa; for instance, g^ad*ichr1_bcd will contract to ichr2_bc^a.
(declare-top (special christoffels christoffels1 christoffels2))

(setq christoffels1 '($ichr1 %ichr1 $icc1 %icc1 $ifc1 %ifc1
                      $inmc1 %inmc1 $ikt1 %ikt1))
(setq christoffels2 '($ichr2 %ichr2 $icc2 %icc2 $ifc2 %ifc2
                      $inmc2 %inmc2 $ikt2 %ikt2))
(setq christoffels (append christoffels1 christoffels2 '(%ifb $ifb)))

(DEFMFUN $CONTRACT (E)                                 ;Main contraction function
       (COND ((ATOM E) E)
	     ((RPOBJ E) (CONTRACT5 E))
	     ((EQ (CAAR E) 'MTIMES)
	      (MYSUBST0 (SIMPLIFYA (CONS '(MTIMES) (CONTRACT4 E))
				   T)
			E))
	     ((EQ (CAAR E) 'MPLUS)
	      (MYSUBST0 (SIMPLUS (CONS '(MPLUS)
				       (MAPCAR '$CONTRACT
					       (CDR E)))
				 1.
				 T)
			E))
	     (T (MYSUBST0 (SIMPLIFYA (CONS (CAR E) (MAPCAR '$CONTRACT (CDR E)))
				     NIL) E))))

(DEFUN CONTRACT5 (E) 
       ((LAMBDA (K) (COND ((AND (NOT (MEMQ (CAAR E) CHRISTOFFELS)) K)
			   (NCONC (LIST (CAR E)
					(CONS SMLIST (CAR K))
					(CONS SMLIST (CDR K)))
				  (CDDDR E)))
			  (T E)))
	(CONTRACT2 (CDADR E) (CDADDR E)))) 

;L1 and L2 are lists. This function removes all like members from L1 and L2 and
;returns their cons or returns NIL if there aren't any like members.

(DEFUN CONTRACT2 (L1 L2)             
       ((LAMBDA (I) (AND I (CONS (SETDIFF L1 I) (SETDIFF L2 I))))
	(INTERSECT L1 L2))) 

(DEFUN SETDIFF (S1 S2)                             ;Set difference of S1 and S2
       (DO ((J S1 (CDR J)) (A))
	   ((NULL J) A)
	   (OR (AND (NOT (NUMBERP (CAR J))) (MEMQ (CAR J) S2)) (SETQ A (CONS (CAR J) A)))))

(DEFUN CONTRACT3 (IT LST)      ;Tries to contract IT with some element of LST.
       (PROG (FRST R REST)     ;If none occurs then return NIL otherwise return
			       ;a list whose first member is the result of
			       ;contraction and whose cdr is a top-level copy
		               ;of LST with the element which contracted
			       ;removed.
	LOOP (SETQ FRST (CAR LST) LST (CDR LST))
;;	     (AND (EQ (CAAR FRST) '%KDELTA) (GO SKIP))
	     (AND (SETQ R (CONTRACT1 IT FRST))
		  (RETURN (CONS R (NCONC (NREVERSE REST) LST))))
			       ;Try contraction in reverse order since the
			       ;operation is commutative.
;;	SKIP (AND (ZL-GET (CAAR FRST) 'CONTRACTIONS)
	SKIP (AND (GETCON (CAAR FRST))
		  (SETQ R (CONTRACT1 FRST IT))
		  (RETURN (CONS R (NCONC (NREVERSE REST) LST))))
	     (AND (NULL LST) (RETURN NIL))
	     (SETQ REST (CONS FRST REST))
	     (GO LOOP))) 

(DEFUN CONTRACT4 (L)                                        ;Contracts products
       (PROG (L1 L2 L3 F CL SF) 
	     (SETQ CL (CDR L)) ;Following loop sets up 3 lists from the factors
		               ;on L: L1 - atoms or the contraction of non
		               ;indexed objects (the contraction is to handle
			       ;sub-expressions in case E is not fully expanded
			       ;as in A*B*(C*D+E*F). ), L2 - indexed objects in
	                       ;L with contraction property, L3 - indexed
                               ;objects in L without contraction property
	AGAIN(SETQ F (CAR CL) CL (CDR CL))
	     (COND ((ATOM F) (SETQ L1 (CONS F L1)))
		   ((RPOBJ F)
		    (SETQ F (CONTRACT5 F))
;;		    (COND ((ZL-GET (CAAR F) 'CONTRACTIONS)
		    (COND ((GETCON (CAAR F))
			   (SETQ L2 (CONS F L2)))
			  (T (SETQ L3 (CONS F L3)))))
		   (T (SETQ L1 (CONS ($CONTRACT F) L1))))
	     (AND CL (GO AGAIN))
	     (AND (NULL L2) (RETURN (NCONC L1 L3)))
	     (AND (NULL (CDR L2)) (SETQ CL L2) (GO LOOP2+1))
                               ;If L2 is empty then no more contractions are
                               ;needed. If L2 has only 1 member then just
	                       ;contract it with L3 otherwise contract the
		               ;members of L2 with themselves. The following
		               ;loop goes down L2 trying to contract members
		               ;with other members according to the following
		               ;method: moving from front to end take current
	                       ;member (F) and see if it contracts with any
		               ;elements in the rest of the list (this is done
		               ;by CONTRACT3). If it doesn't then add it to CL.
		               ;If it does then take result of contraction and
			       ;add to L1, L2, or L3 as above.
	LOOP1(SETQ F (CAR L2) L2 (CDR L2))
	     (COND ((NULL (SETQ SF (CONTRACT3 F L2)))
		    (SETQ CL (CONS F CL)))
		   (T (SETQ L2 (CDR SF) SF (CAR SF))
		      (COND ((ATOM SF) (SETQ L1 (CONS SF L1)))
			    ((RPOBJ SF)
;;			     (COND ((ZL-GET (CAAR SF)
;;					 'CONTRACTIONS)
			     (COND ((GETCON (CAAR SF))
				    (SETQ L2 (CONS SF L2)))
				   (T (SETQ L3 (CONS SF L3)))))
			    (T (SETQ L1 (CONS SF L1))))))
			       ;If L2 has at least 2 elements left then
		               ;continue loop. If L2 has 1 element and CL
			       ;is not empty and there were some contractions
			       ;performed last time then add CL to L2 and try
	                       ;again. Otherwise add L2 to CL and quit.
	     (AND L2
		  (COND ((CDR L2) (GO LOOP1))
			((AND CL SF)
			 (SETQ SF NIL L2 (CONS (CAR L2) CL) CL NIL)
			 (GO LOOP1))
			(T (SETQ CL (NCONC L2 CL)))))
			       ;The following loop goes down CL trying to
	                       ;contract each member with some member in L3. If
		               ;there is not a contraction then the element
			       ;from CL is added onto L3 (this causes elements
	                       ;of CL to be contracted with each other). If
	                       ;there is a contraction then the result is added
			       ;onto L3 by setting L3 to the result of
			       ;CONTRACT3 here if CL is known not to be null.
			       ;If L3 is empty then there is nothing left to
			       ;contract.
	LOOP2(AND (NULL CL) (RETURN (NCONC L1 L3)))
	LOOP2+1
	     (AND (NULL L3) (RETURN (NCONC L1 CL)))
	     (SETQ F (CAR CL) CL (CDR CL))
	     (COND ((SETQ SF (CONTRACT3 F L3)) (SETQ L3 SF))
		   (T (SETQ L3 (CONS F L3))))
	     (GO LOOP2))) 

(DEFUN CONTRACT1 (F G)           ;This does the actual contraction of F with G.
       (PROG (A B C D E CF) 	 ;If f has any derivative indices then it can't
				 ;contract G. If F is Kronecker delta then see
                                 ;which of the covariant, contravariant, or
                                 ;derivative indices matches those in G.
	     (WHEN (CDDDR F) (RETURN NIL))
	     (SETQ A (CDADR F)
		   B (CDADDR F)
		   C (CADR G)
		   D (CADDR G)
		   E (CDDDR G))
	     (COND
	      ((OR (EQ (CAAR F) '%KDELTA) (EQ (CAAR F) '$KDELTA))
	       (AND (> (LENGTH A) 1) (RETURN NIL))
	       (SETQ A (CAR A) B (CAR B))
	       (RETURN
		(SIMPLIFYA (COND ((AND (CDR C) (AND (NOT (NUMBERP B)) (MEMQ B (CDR C))))
				  (SETQ C (SUBST A B (CDR C)))
				  (AND (NOT (MEMQ (CAAR G) CHRISTOFFELS))
				       (CDR D)
				       (SETQ A (CONTRACT2 C (CDR D)))
				       (SETQ C (CAR A) 
					     D (CONS SMLIST (CDR A))))
				  (NCONC (LIST (CAR G)
					       (CONS SMLIST C)
					       D)
					 E))
				 ((AND E (AND (NOT (NUMBERP B)) (MEMQ B E)))
				  (NCONC (LIST (CAR G) C D)
					 (itensor-SORT (SUBST A B E))))
				 ((AND (CDR D) (AND (NOT (NUMBERP A)) (MEMQ A (CDR D))))
				  (SETQ D (SUBST B A (CDR D)))
				  (AND (CDR C)
				       (SETQ A (CONTRACT2 (CDR C) D))
				       (SETQ D (CDR A) 
					     C (CONS SMLIST (CAR A))))
				  (NCONC (LIST (CAR G)
					       C
					       (CONS SMLIST D))
					 E))
				 (T NIL))
			   NIL))))
;; VTT: No tensor should be able to contract LC or KDELTA.
	     (AND (OR (EQ (CAAR G) '$KDELTA) (EQ (CAAR G) '%KDELTA) (EQ (CAAR G) '$Levi_Civita) (EQ (CAAR G) '%Levi_Civita)) (RETURN NIL))

				    ;If G has derivative indices then F must be
	     (and e                 ;constant in order to contract it.
		  (NOT (MGET (CAAR F) '$CONSTANT))
		  (RETURN NIL))
				;Contraction property of F is a list of (A.B)'S
;;	     (COND ((SETQ CF (ZL-GET (CAAR F) 'CONTRACTIONS)))
	     (COND ((SETQ CF (GETCON (CAAR F))))
		   (T (RETURN NIL)))
                          ;If G matches an A then use the B for name of result.
			  ;If an A is a space use name of G for result.
	MORE (COND ((EQ (CAAR CF) '/ ) (SETQ CF (CAR G)))
		   ((EQ (CAAR CF) (CAAR G))
		    (SETQ CF (NCONS (CDAR CF))))
		   (T (OR (SETQ CF (CDR CF)) (RETURN NIL)) (GO MORE)))
	     (SETQ C (CDR C) D (CDR D))
			        ;If CONTRACT2 of F's contravariant and G's
			        ;covariant or F's covariant and G's
                                ;contravariant indicies is NIL then return NIL.
	     (COND ((AND B C (SETQ F (CONTRACT2 B C)))
		    (SETQ B (CAR F) C (CDR F)))
		   ((AND A D (SETQ F (CONTRACT2 A D)))
		    (SETQ A (CAR F) D (CDR F)))
		   (T (RETURN NIL)))
					       ;Form combined indices of result
	     (AND D (SETQ B (APPEND B D)))
	     (AND C (SETQ A (APPEND C A)))
						       ;Zl-remove repeated indices
	     (AND (SETQ F (CONTRACT2 A B)) (SETQ A (CAR F) B (CDR F)))
;; VTT: Special handling of Christoffel symbols. We can only contract them
;; when we turn ICHR1 into ICHR2 or vice versa; other index combinations are
;; illegal. This code checks if the index pattern is a valid one and replaces
;; ICHR1 with ICHR2 or vice versa as appropriate.
;;	     (COND ((OR (EQ (CAR CF) '$ICHR1) (EQ (CAR CF) '%ICHR1))
	     (COND ((member (car CF) christoffels1)
		      (COND	 ((AND (EQ (LENGTH A) 2) (EQ (LENGTH B) 1))
;;				  (SETQ CF (CONS (COND ((EQ (CAR CF) '$ICHR1) '$ICHR2) (T '%ICHR2)) (CDR CF))))
				  (SETQ CF (CONS (elt christoffels2 (position (car CF) christoffels1)) (CDR CF))))
				 ((NOT (AND (EQ (LENGTH A) 3) (EQ (LENGTH B) 0))) (RETURN NIL)))
		   )
;;		   ((OR (EQ (CAR CF) '$ICHR2) (EQ (CAR CF) '%ICHR2))
		   ((member (car CF) christoffels2)
		      (COND	 ((AND (EQ (LENGTH A) 3) (EQ (LENGTH B) 0))
;;				  (SETQ CF (CONS (COND ((EQ (CAR CF) '$ICHR2) '$ICHR1) (T '%ICHR1)) (CDR CF)))
				  (SETQ CF (CONS (elt christoffels1 (position (car CF) christoffels2))  (CDR CF)))
				 )
				 ((NOT (AND (EQ (LENGTH A) 2) (EQ (LENGTH B) 1))) (RETURN NIL)))
		   )
           ((member (car CF) christoffels) (return nil))
	     )

	     (SETQ F (MEVAL (LIST CF (CONS SMLIST A) (CONS SMLIST B))))
	     (AND E
;		  (DO E E (CDR E)
;		      (NULL E)
;		      (SETQ F (IDIFF F (CAR E))))
		  (DO ((E E (CDR E)))
		      ((NULL E) )
		      (SETQ F (IDIFF F (CAR E))))

		  )
	     (RETURN F)))


(DEFMFUN $KDELTA (L1 L2)
       (COND ((NULL (AND ($LISTP L1)
			 ($LISTP L2)
			 (= (LENGTH L1) (LENGTH L2))))
	      (merror "Improper arg to DELTA: ~M"
		      (LIST '(%KDELTA) L1 L2)
		      ))
	     (T (DELTA (CDR L1) (CDR L2))))) 

;kdels defines the symmetric combination of the Kronecker symbols

(DEFMFUN $KDELS (L1 L2)
       (COND ((NULL (AND ($LISTP L1)
			 ($LISTP L2)
			 (= (LENGTH L1) (LENGTH L2))))
	      (merror "Improper arg to DELTA: ~M"
		      (LIST '(%KDELS) L1 L2)
		      ))
	     (T (DELTA (CDR L1) (CDR L2) 1)))) 
;;
;;(DECLARE-TOP (FIXNUM I)) 
;;
;;(DEFUN DELTA (LOWER UPPER &optional (eps -1))
;;       (COND ((NULL LOWER) $DIM)
;;	     ((NULL (CDR LOWER))
;;	      (COND ((EQUAL (CAR UPPER) (CAR LOWER))
;;		     (COND ((NUMBERP (CAR UPPER)) 1.) (T $DIM)))
;;		    ((AND (NUMBERP (CAR UPPER)) (NUMBERP (CAR LOWER))) 0.)
;;		    (T (LIST '(%KDELTA)
;;			     (CONS SMLIST LOWER)
;;			     (CONS SMLIST UPPER)))))
;;	     (T (DO ((I (LENGTH LOWER) (1- I))
;;		     (SL LOWER)
;;		     (TERM)
;;		     (RESULT)
;;		     (F (NCONS (CAR UPPER)))
;;		     (R (CDR UPPER))
;;		     (SIGN (ODDP (LENGTH LOWER))))
;;		    ((= I 0.)
;;		     (SIMPLUS (CONS '(MPLUS) RESULT) 1. T))
;;		    (SETQ TERM (LIST (DELTA (NCONS (CAR SL)) F eps)
;;				     (DELTA (CDR SL) R eps)))
;;		    (SETQ SL (CDR (APPEND SL (NCONS (CAR SL)))))
;;		    (SETQ RESULT
;;			  (CONS (SIMPTIMES (CONS '(MTIMES)
;;						 (COND ((OR SIGN
;;							    (ODDP I))
;;							(CONS eps
;;							      TERM))
;;						       (T TERM)))
;;					   1.
;;					   NIL)
;;				RESULT)))))) 
(DEFUN DELTA (LOWER UPPER &optional (eps -1))
  (COND ((NULL LOWER) $DIM)
        ((NULL (CDR LOWER))
         (COND ((EQUAL (CAR UPPER) (CAR LOWER))
                (COND ((NUMBERP (CAR UPPER)) 1.) (T $DIM)))
               ((AND (NUMBERP (CAR UPPER)) (NUMBERP (CAR LOWER))) 0.)
               (T (LIST '(%KDELTA) (CONS SMLIST LOWER) (CONS SMLIST UPPER)))))
        (T (DO ((LEFT NIL (APPEND LEFT (NCONS (CAR RIGHT))))
		(RIGHT LOWER (CDR RIGHT))
                (RESULT))
               ((NULL RIGHT) (SIMPLUS (CONS '(MPLUS) RESULT) 1. T))
               (SETQ RESULT (CONS (SIMPTIMES
                                   (LIST '(MTIMES) (DELTA (NCONS (CAR RIGHT)) (NCONS (CAR UPPER)) eps)
                                         (DELTA (APPEND LEFT (CDR RIGHT)) (CDR UPPER) eps)
                                         (COND ((ODDP (LENGTH LEFT)) eps) (T 1))
                                   ) 1. T
                                  ) RESULT)
              )))))

(DECLARE-TOP (NOTYPE I))

(DECLARE-TOP (SPECIAL $OUTCHAR $DISPFLAG LINELABLE FOOBAR DERIVLIST))

;Displays P([L1],[L2],I1,I2,...) by making the elements of L2 into a single
;atom which serves as the exponent and the elements of L1 and I1,I2,... into a
;single atom with a comma in between which serves as the subscript.

(DEFMFUN $ISHOW (f)
       (progn (makelabel $LINECHAR)
              (cond ($DISPFLAG
                     (displa (list '(MLABLE) LINELABLE (ishow (specrepcheck f))))
;                     (setq $DISPFLAG nil)
))
              (SET LINELABLE f)))

(DEFUN ISHOW (F) 
       ((LAMBDA (FOOBAR)                              ;FOOBAR intialized to NIL
		(COND ((ATOM F) F)
		      ((RPOBJ F)                      ;If an indexed object ...
		       (SETQ FOOBAR
			     (COND ((OR (CDADR F) (CDDDR F))   ;If covariant or
				    (CONS (LIST (CAAR F)    ;derivative indices
						'ARRAY)
					  (NCONS (MAKNAM (CONS '$ (SPLICE (CDADR F)
							 (CDDDR F)))))))
				   (T (CAAR F))))
		       (COND ((CDADDR F)              ;If contravariant indices
			      (LIST '(MEXPT SIMP)
				    FOOBAR
				     (CONS '(MTIMES SIMP)  ;Make indices appear
					  (CDADDR F))))    ;as exponents for
			     (T FOOBAR)))                  ;proper display
		      (T
		       (CONS (CAR F) (MAPCAR 'ISHOW (CDR F))))))
	NIL))                                           ;Map onto subparts of F

(DEFUN SPLICE (L1 L2) 
       (COND (L2 (SETQ L2 (CONS '|,| (SPLICE1 L2)))
		 (AND L1 (SETQ L2 (NCONC (SPLICE1 L1) L2)))
		 L2)
	     (T (SPLICE1 L1)))) 

(DEFUN SPLICE1 (L)
  (COND ((NULL (CDR L))(SPLICE2 (CAR L)))
	(T (NCONC (SPLICE2 (CAR L))(CONS '| | (SPLICE1 (CDR L)))))))

(DEFUN SPLICE2 (X)
  (COND ((FIXP X)(EXPLODE X))
	(T (CDR (EXPLODEc X)))))
;	(T (CDR (EXPLODEc (print-invert-case X))))))

(DEFUN DERIV (E) 
       (PROG (EXP Z COUNT V) 
	     (COND ((NULL (CDR E)) (RETURN (STOTALDIFF (CAR E))))
		   ((NULL (CDDR E)) (NCONC E '(1.))))
	     (SETQ EXP (CAR E) Z (SETQ E (APPEND E NIL)))
	LOOP (COND ((OR (NULL DERIVLIST) (ZL-MEMBER (CADR Z) DERIVLIST))
		    (GO DOIT)))
						       ;DERIVLIST is set by $EV
	     (SETQ Z (CDR Z))
	LOOP2(COND ((CDR Z) (GO LOOP))
		   ((NULL (CDR E)) (RETURN EXP))
		   (T (GO NOUN)))
	DOIT (COND ((NULL (CDDR Z))
		    (merror "Wrong number of args to DERIVATIVE"))
		   ((NOT (FIXP (SETQ COUNT (CADDR Z)))) (GO NOUN))
		   ((< COUNT 0.)
		    (merror "Improper count to DIFF: ~M"
			    COUNT)))
	LOOP1(SETQ V (CADR Z))
	     (AND (FIXP V)
		  $VECT_COORDS
		  (> V 0.)
		  (NOT (> V $DIM))
		  (SETQ V
			(COND ((ATOM $VECT_COORDS)
			       (MEVAL1 (LIST (LIST $VECT_COORDS 'SIMP 'ARRAY)
					     V)))
			      ((EQ (CAAR $VECT_COORDS) 'MLIST)
			       (COND ((NOT (< V
					      (LENGTH $VECT_COORDS)))
				      (merror
"Coordinate list too short for derivative index"))
				     (T (NTH V $VECT_COORDS))))
			      (T V))))
	     (COND ((ZEROP COUNT) (RPLACD Z (CDDDR Z)) (GO LOOP2))
		   ((ZEROP1 (SETQ EXP (SDIFF EXP V))) (RETURN 0.)))
	     (SETQ COUNT (1- COUNT))
	     (GO LOOP1)
	NOUN (RETURN (DIFF%DERIV (CONS EXP (CDR E))))))

(DEFUN CHAINRULE1 (E X)					; --YS 15.02.02
	(PROG (Y)
		(COND ((AND (ATOM E) (EQ (SETQ Y (CAR (MGET E 'DEPENDS)))
			(CADR $COORD))) (RETURN (SUBST X Y (CHAINRULE E Y))))
		(T (RETURN (CHAINRULE E X))))))

;Redefined so that the derivative of any indexed object appends on the
;coordinate index in sorted order unless the indexed object was declared
;constant in which case 0 is returned.
#+Franz (sstatus translink nil) ; make sdiff take hold
#+Franz (sstatus translink t)
(DEFUN SDIFF (E X) 
       (COND ((MNUMP E) 0.)
	     ((ALIKE1 E X) 1.)
	     ((OR (ATOM E) (MEMQ 'ARRAY (CDAR E)))
	      (CHAINRULE1 E X))
	     ((MGET (CAAR E) '$CONSTANT) 0.)                    ;New line added
	     ((EQ (CAAR E) 'MRAT) (RATDX E X))
	     ((EQ (CAAR E) 'MPLUS)
	      (SIMPLUS (CONS '(MPLUS) (SDIFFMAP (CDR E) X))
		       1.
		       T))
	     ((EQ (CAAR E) 'MEQUAL)
	      (LIST (CAR E) (SDIFF (CADR E) X) (SDIFF (CADDR E) X)))
	     ((EQ (CAAR E) '$MATRIX)
	      (CONS (CAR E)
		    (MAPCAR 
		     (FUNCTION (LAMBDA (Y) 
				       (CONS (CAR Y)
					     (SDIFFMAP (CDR Y) X))))
		     (CDR E))))
	     ((EQ (CAAR E) 'MTIMES)
 	      (ADDN (SDIFFTIMES (CDR E) X) T))
	     ((EQ (CAAR E) 'MEXPT) (DIFFEXPT E X))
;;	     ((RPOBJ E) (DIFFRPOBJ E X))                        ;New line added
;;	     ((AND (BOUNDP '$IMETRIC) (EQ (CAAR E) '%DETERMINANT);New line added
;;		   (EQ (CADR E) $IMETRIC))
;;	      ((LAMBDA (DUMMY)
;;		       (setq dummy ($idummy))
;;		       (COND ((EQ DUMMY X) (setq dummy ($idummy))))
;;		       (LIST '(MTIMES SIMP) 2. E
;;			     (LIST '($ICHR2 SIMP) (CONS SMLIST (LIST DUMMY X))
;;				   (CONS SMLIST (NCONS DUMMY)))))
;;	       NIL))
	     ((NOT (DEPENDS E X))
	      (COND ((FIXP X) (LIST '(%DERIVATIVE) E X))
		    ((ATOM X) 0.)
		    (T (LIST '(%DERIVATIVE E X)))))
							  ;This line moved down
	     ((EQ (CAAR E) 'MNCTIMES)
	      (SIMPLUS (LIST '(MPLUS)
			     (LIST '(MNCTIMES)
				   (SDIFF (CADR E) X)
				   (CADDR E))
			     (LIST '(MNCTIMES)
				   (CADR E)
				   (SDIFF (CADDR E) X)))
		       1.
		       NIL))
	     ((EQ (CAAR E) 'MNCEXPT) (DIFFNCEXPT E X))
	     ((EQ (CAAR E) '%INTEGRATE) (DIFFINT E X))
	     ((EQ (CAAR E) '%DERIVATIVE)
	      (COND ((OR (ATOM (CADR E))
			 (MEMQ 'ARRAY (CDAADR E)))
		     (CHAINRULE1 E X))
		    ((FREEL (CDR E) X) 0.)
		    (T (DIFF%DERIV (LIST E X 1.)))))
	     ((MEMQ (CAAR E) '(%SUM %PRODUCT)) (DIFFSUMPROD E X))
	     (T (SDIFFGRAD E X)))) 

; VTT: several of these functions have been copied verbatim from comm.lisp and
; comm2.lisp, in order to implement indicial differentiation as distinct from
; differentiation with respect to an external variable.

(defun idiffmap (e x) (mapcar #'(lambda (term) (idiff term x)) e))

(defun idifftimes (l x)
  (prog (term left out)
   loop (setq term (car l) l (cdr l))
   (setq out (cons (muln (cons (idiff term x) (append left l)) t) out))
   (if (null l) (return out))
   (setq left (cons term left))
   (go loop)))

(defun idiffexpt (e x)
  (if (mnump (caddr e))
      (mul3 (caddr e) (power (cadr e) (addk (caddr e) -1)) (idiff (cadr e) x))
      (mul2 e (add2 (mul3 (power (cadr e) -1) (caddr e) (idiff (cadr e) x))
            (mul2 (simplifya (list '(%log) (cadr e)) t)
              (idiff (caddr e) x))))))

(defmfun idiffint (e x)
  (let (a)
    (cond ((null (cdddr e))
       (cond ((alike1 x (caddr e)) (cadr e))
         ((and (not (atom (caddr e))) (atom x) (not (free (caddr e) x)))
          (mul2 (cadr e) (idiff (caddr e) x)))
         ((or ($constantp (setq a (idiff (cadr e) x)))
              (and (atom (caddr e)) (free a (caddr e))))
          (mul2 a (caddr e)))
         (t (simplifya (list '(%integrate) a (caddr e)) t))))
      ((alike1 x (caddr e)) (addn (idiffint1 (cdr e) x x) t))
      (t (addn (cons (if (equal (setq a (idiff (cadr e) x)) 0)
                 0
                 (simplifya (list '(%integrate) a (caddr e)
                          (cadddr e) (car (cddddr e)))
                    t))
             (idiffint1 (cdr e) x (caddr e)))
           t)))))

(defun idiffint1 (e x y)
  (let ((u (idiff (cadddr e) x)) (v (idiff (caddr e) x)))
    (list (if (pzerop u) 0 (mul2 u (maxima-substitute (cadddr e) y (car e))))
      (if (pzerop v) 0 (mul3 v (maxima-substitute (caddr e) y (car e)) -1)))))

(defun idiff%deriv (e) (let (derivflag) (simplifya (cons '(%idiff) e) t)))

(defun ideriv (e)
  (prog (exp z count)
     (cond ((null e) (wna-err '$idiff))
       ((null (cdr e)) (return (itotaldiff (car e))))
       ((null (cddr e)) (nconc e '(1))))
     (setq exp (car e) z (setq e (copy-top-level e)))
     loop (if (or (null derivlist) (zl-member (cadr z) derivlist)) (go doit))
                    ; DERIVLIST is set by $EV
     (setq z (cdr z))
     loop2(cond ((cdr z) (go loop))
        ((null (cdr e)) (return exp))
        (t (go noun)))
     doit (cond ((nonvarcheck (cadr z) '$idiff))
        ((null (cddr z)) (wna-err '$idiff))
        ((not (eq (ml-typep (caddr z)) 'fixnum)) (go noun))
        ((minusp (setq count (caddr z)))
         (merror "Improper count to IDIFF:~%~M" count)))
     loop1(cond ((zerop count) (rplacd z (cdddr z)) (go loop2))
        ((equal (setq exp (idiff exp (cadr z))) 0) (return 0)))
     (setq count (f1- count))
     (go loop1)
     noun (return (idiff%deriv (cons exp (cdr e))))))


(defmfun idiffncexpt (e x)
  ((lambda (base* pow)
     (cond ((and (mnump pow) (or (not (eq (ml-typep pow) 'fixnum)) (< pow 0))) ; POW cannot be 0
        (idiff%deriv (list e x 1)))
       ((and (atom base*) (eq base* x) (free pow base*))
        (mul2* pow (list '(mncexpt) base* (add2 pow -1))))
       ((ml-typep pow 'fixnum)
        ((lambda (deriv ans)
           (do ((i 0 (f1+ i))) ((= i pow))
         (setq ans (cons (list '(mnctimes) (list '(mncexpt) base* i)
                       (list '(mnctimes) deriv
                         (list '(mncexpt) base* (f- pow 1 i))))
                 ans)))
           (addn ans nil))
         (idiff base* x) nil))
       ((and (not (depends pow x)) (or (atom pow) (and (atom base*) (free pow base*))))
        ((lambda (deriv index)
           (simplifya
        (list '(%sum)
              (list '(mnctimes) (list '(mncexpt) base* index)
                (list '(mnctimes) deriv
                  (list '(mncexpt) base*
                    (list '(mplus) pow -1 (list '(mtimes) -1 index)))))
              index 0 (list '(mplus) pow -1)) nil))
         (idiff base* x) (gensumindex)))
       (t (idiff%deriv (list e x 1)))))
   (cadr e) (caddr e)))

(defmfun idiffsumprod (e x)
  (cond ((or (not (atom x)) (not (free (cadddr e) x)) (not (free (car (cddddr e)) x)))
     (idiff%deriv (list e x 1)))
    ((eq (caddr e) x) 0)
    (t (let ((u (idiff (cadr e) x)))
         (setq u (simplifya (list '(%sum)
                      (if (eq (caar e) '%sum) u (div u (cadr e)))
                      (caddr e) (cadddr e) (car (cddddr e)))
                t))
         (if (eq (caar e) '%sum) u (mul2 e u))))))

(defun idiffgrad (e x)
  (let ((fun (caar e)) grad args)
    (cond ((and (eq fun 'mqapply) (oldget (caaadr e) 'grad))
       (idiffgrad (cons (cons (caaadr e) nil) (append (cdadr e) (cddr e)))
              x))
      ((or (eq fun 'mqapply) (null (setq grad (oldget fun 'grad))))
       (if (not (depends e x)) 0 (idiff%deriv (list e x 1))))
      ((not (= (length (cdr e)) (length (car grad))))
       (merror "Wrong number of arguments for ~:M" fun))
      (t (setq args (idiffmap (cdr e) x))
         (addn (mapcar
            #'mul2
            (cdr (substitutel
              (cdr e) (car grad)
              (do ((l1 (cdr grad) (cdr l1))
                   (args args (cdr args)) (l2))
                  ((null l1) (cons '(mlist) (nreverse l2)))
                (setq l2 (cons (cond ((equal (car args) 0) 0)
                         (t (car l1)))
                       l2)))))
            args)
           t)))))

(defmfun $idiff n (let (derivlist) (ideriv (listify n))))

(DEFMFUN IDIFF (E X)
  (COND
         (($constantp E) 0.)
	     ((ALIKE1 E X) 1.)
	     ((OR (ATOM E) (MEMQ 'ARRAY (CDAR E)))
;;	      (ICHAINRULE E X))
;;        (idiff%deriv (list e x 1)))
          0)
	     ((MGET (CAAR E) '$CONSTANT) 0.)                    ;New line added
	     ((EQ (CAAR E) 'MRAT) (RATDX E X))
	     ((EQ (CAAR E) 'MPLUS)
	      (SIMPLUS (CONS '(MPLUS) (IDIFFMAP (CDR E) X))
		       1.
		       T))
	     ((EQ (CAAR E) 'MEQUAL)
	      (LIST (CAR E) ($IDIFF (CADR E) X) ($IDIFF (CADDR E) X)))
	     ((EQ (CAAR E) '$MATRIX)
	      (CONS (CAR E)
		    (MAPCAR 
		     (FUNCTION (LAMBDA (Y) 
				       (CONS (CAR Y)
					     (IDIFFMAP (CDR Y) X))))
		     (CDR E))))
	     ((EQ (CAAR E) 'MTIMES)
 	      (ADDN (IDIFFTIMES (CDR E) X) T))
	     ((EQ (CAAR E) 'MEXPT) (IDIFFEXPT E X))
	((RPOBJ E) (DIFFRPOBJ E X))
    ((AND (BOUNDP '$IMETRIC) (EQ (CAAR E) '%DETERMINANT)
      (EQ (CADR E) $IMETRIC))
      ((LAMBDA (DUMMY)
       (setq dummy ($idummy))
       (COND ((EQ DUMMY X) (setq dummy ($idummy))))
       (LIST '(MTIMES SIMP) 2. E
       (LIST '($ICHR2 SIMP) (CONS SMLIST (LIST DUMMY X))
       (CONS SMLIST (NCONS DUMMY)))))
       NIL))
	     ((EQ (CAAR E) 'MNCTIMES)
	      (SIMPLUS (LIST '(MPLUS)
			     (LIST '(MNCTIMES)
				   ($IDIFF (CADR E) X)
				   (CADDR E))
			     (LIST '(MNCTIMES)
				   (CADR E)
				   ($IDIFF (CADDR E) X)))
		       1.
		       NIL))
	     ((EQ (CAAR E) 'MNCEXPT) (IDIFFNCEXPT E X))
	     ((EQ (CAAR E) '%INTEGRATE) (IDIFFINT E X))
	     ((EQ (CAAR E) '%DERIVATIVE)
	      (COND ((OR (ATOM (CADR E))
			 (MEMQ 'ARRAY (CDAADR E)))
;;		     (ICHAINRULE E X))
;;           (idiff%deriv (list e x 1)))
             0)
		    ((FREEL (CDR E) X) 0.)
		    (T (DIFF%DERIV (LIST E X 1.)))))
	     ((MEMQ (CAAR E) '(%SUM %PRODUCT)) (IDIFFSUMPROD E X))
	     (T (IDIFFGRAD E X))
  )
)

(defun diffrpobj (e x)                  ;Derivative of an indexed object
  (cond
    (               ; Special case: functions declared with coord()
      (and
        (memq (caar e) $COORD) (null (cdadr e))
        (equal (length (cdaddr e)) 1) (null (cdddr e))
      )
      (delta (ncons x) (cdaddr e))
    )
    (t              ; Everything else
      (nconc
        (list (car e) (cadr e) (caddr e))
        (cond
          (
            (null (cdddr e))
            (ncons x)
          )
          (         ; Derivative indices do not commute when frames are used
            (or $iframe_flag $itorsion_flag)
            (append (cdddr e) (ncons x))
          )
          (t
            (itensor-sort (append (cdddr e) (ncons x)))
          )
        )
      )
    )
  )
)


(DEFMFUN $LC0 (L1) 
       (PROG (A B C SIGN) 
	     (SETQ A (CDR L1))
	     (IFNOT (AND A (CDR A)) (RETURN (LIST '(%Levi_Civita) L1)))
	     (SETQ B A)
	LOOP1(IFNOT (FIXP (CAR A)) (RETURN (LIST '(%Levi_Civita) L1)))
	     (AND (SETQ A (CDR A)) (GO LOOP1))
	LOOP3(SETQ A (CAR B) B (CDR B) C B)
	LOOP2(COND ((= (CAR C) A) (RETURN 0.))
		   ((< (CAR C) A) (SETQ SIGN (NOT SIGN))))
	     (AND (SETQ C (CDR C)) (GO LOOP2))
	     (AND (CDR B) (GO LOOP3))
	     (RETURN (COND (SIGN -1.) (T 1.))))) 
(DEFMFUN $Levi_Civita (L1 &optional (L2 nil))
	(COND
		((EQ L2 nil) ($LC0 L1))
		((LIKE L1 '((MLIST)))
		(PROG (l) (SETQ l nil)
		  (DO ((I ($LENGTH L2) (1- I))) ((< I 1)) (SETQ l (CONS I l)))
		  (RETURN (LIST '($KDELTA SIMP) (CONS SMLIST l) L2))
		 ))
		((LIKE L2 '((MLIST)))
		(PROG (l) (SETQ l nil)
		  (DO ((I ($LENGTH L1) (1- I))) ((< I 1)) (SETQ l (CONS I l)))
		  (RETURN (LIST '($KDELTA SIMP) L1 (CONS SMLIST l)))
		))
		(T (MERROR "Mixed-index Levi-Civita symbols not supported"))
	)
)

;; simplification rules for the totally antisymmetric LC symbol
(DEFUN $LC_L (E)
    (PROG (L1 L2 L N)
	(CATCH 'MATCH
	  (COND ((ATOM E) (MATCHERR)))
	  (COND ((ATOM (CAR E)) (MATCHERR)))
	  (COND ((NOT (OR (EQ (CAAR E) '$levi_civita) (EQ (CAAR E) '%levi_civita))) (MATCHERR)))
	  (COND ((NOT ($LISTP (SETQ L1 (CADR E)))) (MATCHERR)))
	  (COND ((NOT (ALIKE1 '((MLIST SIMP)) (SETQ L2 (CADDR E)))) (MATCHERR)))
	  (COND ((CDDDR E) (MATCHERR)))
	  (SETQ N ($LENGTH L1))
	  (SETQ L NIL)
	  (DO ((I N (1- I))) ((< I 1)) (SETQ l (CONS ($IDUMMY) L)))
	  (RETURN (LIST '(MTIMES SIMP) ($KDELTA L1 (CONS SMLIST L))
	        (LIST (CONS (CAAR E) '(SIMP)) (CONS SMLIST L) (NCONS SMLIST))
	        (LIST '(MEXPT SIMP) (MEVAL (LIST 'MFACTORIAL N)) -1))
	  )
	)
    )
)

(DEFUN $LC_U (E)
    (PROG (L1 L2 L N)
	(CATCH 'MATCH
	  (COND ((ATOM E) (MATCHERR)))
	  (COND ((ATOM (CAR E)) (MATCHERR)))
	  (COND ((NOT (OR (EQ (CAAR E) '$levi_civita) (EQ (CAAR E) '%levi_civita))) (MATCHERR)))
	  (COND ((NOT (ALIKE1 '((MLIST SIMP)) (SETQ L1 (CADR E)))) (MATCHERR)))
	  (COND ((NOT ($LISTP (SETQ L2 (CADDR E)))) (MATCHERR)))
	  (COND ((CDDDR E) (MATCHERR)))
	  (SETQ N ($LENGTH L2))
	  (SETQ L NIL)
	  (DO ((I N (1- I))) ((< I 1)) (SETQ l (CONS ($IDUMMY) L)))
	  (RETURN (LIST '(MTIMES SIMP) ($KDELTA (CONS SMLIST L) L2)
	        (LIST (CONS (CAAR E) '(SIMP)) (NCONS SMLIST) (CONS SMLIST L))
	        (LIST '(MEXPT SIMP) (MEVAL (LIST 'MFACTORIAL N)) -1))
	  )
	)
    )
)

(ADD2LNC '$LC_L $RULES)
(ADD2LNC '$LC_U $RULES)

(DECLARE-TOP (SPECIAL E EMPTY $FLIPFLAG))

(SETQ $FLIPFLAG NIL EMPTY '((MLIST SIMP) ((MLIST SIMP)) ((MLIST SIMP)))) 

(DEFUN NONUMBER (L)
	(COND
		((NUMBERP (CAR L)) (NONUMBER (CDR L)))
		((EQ L NIL) ())
		(T (CONS (CAR L) (NONUMBER (CDR L))))
	)
)

(DEFUN REMOVEINDEX (E L)
 (COND	((NULL L) NIL)
	((ATOM E)
         (COND ((EQ E (CAR L)) (CDR L))
              (T (CONS (CAR L) (REMOVEINDEX E (CDR L))))
        ))
	(T (REMOVEINDEX (CDR E) (REMOVEINDEX (CAR E) L)))
 )
)

(defun indices (E)
  (prog (top bottom x y p q r)
    (setq top nil bottom nil)
    (cond
      (
        (rpobj e)
        (setq top (nonumber (cdaddr e))
              bottom (nonumber (append (cdadr e) (cdddr e))))
      )
      ((atom e))
      (
        (memq (caar e) '(mtimes mnctimes mncexpt))
        (dolist (v (cdr e))
          (setq x (indices v) bottom (append bottom (cadr x))
                              top (append top (car x)))
        )
      )
      (
        (memq (caar e) '(mplus mequal))
        (setq top (indices (cadr e)) bottom (cadr top) top (car top))
        (setq p (intersect top bottom) q (removeindex p bottom)
              p (removeindex p top))
        (dolist (v (cddr e))
          (setq x (indices v) y (cadr x) x (car x))
          (setq r (intersect x y) x (removeindex r x) y (removeindex r y))
          (when
            (not (and (samelists x p) (samelists y q)))
            (merror "Improper indices in ~M" v)
          )
          (setq top (union top r) bottom (union bottom r))
        )
      )
      (
        (memq (caar e) '($sum %sum))
        (setq top (list (caddr e)) bottom (list (caddr e)))
      )
      (
        (memq (caar e) '(%idiff $idiff))
;;; This code would count derivative indices as covariant. However, it is
;;; not needed. If the user wants to count derivative indices, those should
;;; be part of the tensor expression; if the expression is undiff'd, there
;;; must be a reason!
;;        (do
;;          ((f (cddr e) (cddr f)))
;;          ((null f))
;;          (do
;;            ((i 1 (1+ i)))
;;            ((> i (cond ((cadr f) (cadr f)) (t 1))))
;;            (setq bottom (cons (car f) bottom))
;;          )
;;        )
        (setq x (indices (cadr e)) bottom (append bottom (cadr x))
              top (append top (car x)))
      )
      (
        (memq (caar e) '(%derivative $diff))
        (setq x (indices (cadr e)) bottom (append bottom (cadr x))
              top (append top (car x)))
      )
    )
    (return (list top bottom))
  )
)

(DEFMFUN $INDICES (E)
 (PROG (TOP BOTTOM X)
;;	(SETQ TOP (INDICES E) BOTTOM (CADR TOP) TOP (CAR TOP) X (INTERSECT TOP BOTTOM))
	(SETQ TOP (INDICES E) BOTTOM (CADR TOP) TOP (CAR TOP) X (COND ($FLIPFLAG (INTERSECT BOTTOM TOP)) (T (INTERSECT TOP BOTTOM))))
	(SETQ TOP (REMOVEINDEX X TOP) BOTTOM (REMOVEINDEX X BOTTOM))
	(RETURN (CONS SMLIST (LIST (CONS SMLIST (APPEND TOP BOTTOM)) (CONS SMLIST X))))
 )
)

(DEFUN SAMELISTS (A B)       ;"True" if A and B have the same distinct elements
       (AND (= (LENGTH A) (LENGTH B))
	    (DO ((L
		A
		(CDR L)))
		(NIL)
		(COND ((NULL L) (RETURN T))
		      ((MEMQ (CAR L) B))
		      (T (RETURN NIL)))))) 

(DEFMFUN $FLUSH n           ;Replaces the given (as arguments to FLUSH) indexed
       (prog (l)          ;objects by zero if they have no derivative indices.
	     (cond ((< n 2) (merror "FLUSH takes at least 2 arguments"))
		   ((not
		      (sloop for v in (setq l (listify (f- 1 n)))
			     always (symbolp v)))
;		      (apply 'and (mapcar 'symbolp
;					    (setq l (listify (f- 1 n))) ))
		    (merror "All arguments but the first must be names of
indexed objects")) (t (return (flush (arg 1) l t))))))

(DEFMFUN $FLUSHD n          ;Replaces the given (as arguments to FLUSHD) indexed
       (prog (l)          ;objects by zero if they have any derivative indices.
	     (cond ((< n 2) (merror "FLUSH takes at least 2 arguments"))
		   ((not
		      (sloop for v in (setq l (listify (f- 1 n)))
			     always (symbolp v))
;		      (apply 'and (mapcar 'symbolp
;					     (setq l (listify (f- 1 n)))))
		      )
		    (merror "All arguments but the first must be names of
indexed objects")) (t (return (flush (arg 1) l nil))))))

(defun FLUSH (e l flag)
       (cond ((atom e) e)
	     ((rpobj e)
	      (cond ((not (memq (caar e) l)) e)
		    ((not (null (cdddr e)))
		     (cond (flag e)
			   (t 0)))
		    (t (cond (flag 0)
			     (t e)))))
	     (t (subst0 (cons (ncons (caar e))
			      (mapcar (function (lambda (q) (flush q l flag)))
				      (cdr e))) e))))

(DEFMFUN $FLUSHND (e name n)              ;Replaces by zero all indexed objects
       (cond ((atom e) e)               ;that have n or more derivative indices
	     ((rpobj e)
	      (cond ((and (equal (caar e) name)
			  (> (length (cdddr e)) (1- n)))
		     0)
		    (t e)))
	     (t (subst0 (cons (ncons (caar e))
			      (mapcar (function
				       (lambda (q) ($flushnd q name n)))
				      (cdr e))) e))))

(DECLARE-TOP (FIXNUM INDEX N) (SPECIAL INDEX N DUMX))

;(DEFMFUN $RENAME NARGS ((LAMBDA (INDEX) (RENAME (ARG 1)))
;       (COND ((= NARGS 1) 1) (T (ARG 2))))) ;Sets INDEX to 1 or 2nd argument of
;                                            ;$RENAME
(DEFMFUN $RENAME NARGS
 (cond ((= NARGS 1) (setq INDEX 1)) (t (setq INDEX (arg 2)))) (rename (arg 1)))

(DEFUN RENAME (E)                           ;Renames dummy indices consistently
       (COND
	((ATOM E) E)
	((OR (RPOBJ E) (EQ (CAAR E) 'MTIMES));If an indexed object or a product
	 ((LAMBDA  (L) 
	(SIMPTIMES (REORDER (COND (L (SUBLIS (itensor-CLEANUP L (SETQ N INDEX)) E))(T E))) 1 T))
	  (CDADDR ($INDICES E))                     ;Gets list of dummy indices
	  ))
	(T            ;Otherwise map $RENAME on each of the subparts e.g. a sum
	 (MYSUBST0 (SIMPLIFYA  (CONS (NCONS (CAAR E))
				  (MAPCAR 'RENAME (CDR E)))
			    T)
		   E))
	))

(DEFUN REORDER (E)       ;Reorders contravariant, covariant, derivative indices
       (MYSUBST0         ;Example: F([A,B],[C,D],E,F)
	(CONS
	 '(MTIMES)
	 (MAPCAR
	  #'(LAMBDA (X) 
	    (COND ((RPOBJ X)
		   (NCONC (LIST (CAR X)                              ;($F SIMP)
				(CONS SMLIST
				      (COND ($ALLSYM (itensor-SORT (COPY (CDADR X))))
					    (T (CDADR X))))          ;($A $B)
				(CONS SMLIST
				      (COND ($ALLSYM
					     (itensor-SORT (COPY (CDADDR X))))
					    (T (CDADDR X)))))        ;($C $D)
			  (itensor-SORT (COPY (CDDDR X)))))                ;($E $F)
		  (T X)))
	  (COND ((EQ (CAAR E) 'MTIMES) (CDR E))
		(T (NCONS E)))))
	E))

(DEFUN itensor-CLEANUP (A N)((LAMBDA (DUMX)(CLEANUP1 A)) NIL))        ;Sets DUMX to NIL
 
(DEFUN CLEANUP1 (A)
  (AND A (SETQ DUMX (IMPLODE (NCONC (EXPLODEN $IDUMMYX)    ;Keep proper order of
				    (EXPLODEN N))) N (1+ N))          ;indices
	(COND ((EQ DUMX (CAR A)) (CLEANUP1 (CDR A)))
	      (T (CONS (CONS (CAR A) DUMX) (CLEANUP1 (CDR A)))))))
;Make list of dotted pairs indicating substitutions i.e. ((A . #1) (B . #2))

(DECLARE-TOP (NOTYPE N INDEX)(UNSPECIAL N DUMX INDEX))

(DEFUN itensor-SORT (L) (COND ((CDR L) (SORT L 'LESS)) (T L)))
;Sort into ascending order

(DEFMFUN $REMCOMPS (TENSOR)
       (ZL-REMPROP TENSOR 'EXPR) (ZL-REMPROP TENSOR 'CARRAYS)
       (ZL-REMPROP TENSOR 'TEXPRS) (ZL-REMPROP TENSOR 'INDEXED)
       (ZL-REMPROP TENSOR 'TSUBR) '$DONE)

(DEFMFUN $INDEXED_TENSOR (TENSOR)
  (LET (FP NEW)
    (AND (ZL-GET TENSOR 'EXPR) 
	 (merror "~M has expr" tensor))
    (ARGS TENSOR  NIL)
    (AND (SETQ FP (ZL-GET TENSOR 'SUBR))
	 (PROGN (SETQ NEW (GENSYM))(PUTPROP NEW FP 'SUBR)
		(ZL-REMPROP TENSOR 'SUBR)(PUTPROP TENSOR NEW 'TSUBR)))
    (PUTPROP TENSOR T 'INDEXED)
    (PUTPROP TENSOR (SUBST TENSOR 'G '(LAMBDA N (TENSOREVAL (QUOTE G)(LISTIFY N)))) 'EXPR)
		(eval (subst tensor 'g (quote (defmfun g n (tensoreval 'g (listify n))))))
    '$DONE))


(DEFUN ALLFIXED (L) 
       (AND L (FIXP (CAR L)) (OR (NULL (CDR L)) (ALLFIXED (CDR L))))) 

;;(DEFUN TENSOREVAL (TENSOR INDXS)
;;  ((LAMBDA (DER CON)
;;    (AND (CDR INDXS) (SETQ CON (CDADR INDXS) DER (CDDR INDXS)))
;;  (SETQ TENSOR (SELECT TENSOR (CDAR INDXS) CON))
;;  (COND (DER (APPLY '$DIFF (CONS TENSOR (PUTINONES DER))))
;;	(T TENSOR))) NIL NIL))
(DEFUN TENSOREVAL (TENSOR INDXS)
  ((LAMBDA (DER CON)
    (AND (CDR INDXS) (SETQ CON (CDADR INDXS) DER (CDDR INDXS)))
  (SETQ TENSOR (SELECT TENSOR (CDAR INDXS) CON DER))
  ) NIL NIL))

;;(DEFMFUN $COMPONENTS (TENSOR COMP)
;;  ((LAMBDA (LEN1 LEN2 NAME PROP)
;;    (COND ((OR (NOT (RPOBJ TENSOR))(CDDDR TENSOR))
;;	   (merror "Improper 1st arg to COMPONENTS: ~M"
;;		   TENSOR
;;		   )))
;;    (SETQ LEN1 (LENGTH (CDADR TENSOR)) LEN2 (LENGTH (CDADDR TENSOR)))
;;    (AND (NOT (ATOM COMP))(EQ (CAAR COMP) '$MATRIX)
;;	 (COND ((= (f+ LEN1 LEN2) 2)(SETQ NAME (GENSYM))
;;		(SET NAME COMP)(SETQ COMP NAME))
;;	       (T 
;;		(merror "Needs two indices for COMPONENTS from matrix:~%~M"
;;			TENSOR))))
;;    (COND ((AND (EQ (ML-TYPEP COMP) 'SYMBOL) (> (f+ LEN1 LEN2) 0))
;;	   (SETQ PROP 'CARRAYS))
;;	  ((SAMELISTS (SETQ NAME (APPEND (CDADR TENSOR) (CDADDR TENSOR)))
;;		      (CDADR ($INDICES COMP)))
;;	   (SETQ PROP 'TEXPRS COMP (CONS COMP NAME)))
;;	  (T (merror "Args to COMPONENTS do not have the same free indices")))
;;    (SETQ TENSOR (CAAR TENSOR) LEN1 (CONS LEN1 LEN2))
;;    (COND ((AND (SETQ NAME (ZL-GET TENSOR PROP))
;;		(SETQ LEN2 (ZL-ASSOC LEN1 NAME))) (RPLACD LEN2 COMP))
;;	  (T (PUTPROP TENSOR (CONS (CONS LEN1 COMP) NAME) PROP)))
;;    (OR (ZL-GET TENSOR 'INDEXED) ($INDEXED_TENSOR TENSOR))
;;    '$DONE) NIL NIL NIL NIL))
(DEFMFUN $COMPONENTS (TENSOR COMP)
  ((LAMBDA (LEN1 LEN2 LEN3 NAME PROP)
    (COND ((NOT (RPOBJ TENSOR))
	   (merror "Improper 1st arg to COMPONENTS: ~M"
		   TENSOR
		   )))
    (SETQ LEN1 (LENGTH (CDADR TENSOR)) LEN2 (LENGTH (CDADDR TENSOR)) LEN3 (LENGTH (CDDDR TENSOR)))
    (AND (NOT (ATOM COMP))(EQ (CAAR COMP) '$MATRIX)
	 (COND ((= (f+ (f+ LEN1 LEN2) LEN3) 2)(SETQ NAME (GENSYM))
		(SET NAME COMP)(SETQ COMP NAME))
	       (T 
		(merror "Needs two indices for COMPONENTS from matrix:~%~M"
			TENSOR))))
    (COND ((AND (EQ (ML-TYPEP COMP) 'SYMBOL) (> (f+ (f+ LEN1 LEN2) LEN3) 0))
	   (SETQ PROP 'CARRAYS))
	  ((SAMELISTS (SETQ NAME (APPEND (CDADR TENSOR) (CDADDR TENSOR) (CDDDR TENSOR)))
		      (CDADR ($INDICES COMP)))
	   (SETQ PROP 'TEXPRS COMP (CONS COMP NAME)))
	  (T (merror "Args to COMPONENTS do not have the same free indices")))
    (SETQ TENSOR (CAAR TENSOR) LEN1 (LIST LEN1 LEN2 LEN3))
    (COND ((AND (SETQ NAME (ZL-GET TENSOR PROP))
		(SETQ LEN2 (ZL-ASSOC LEN1 NAME))) (RPLACD LEN2 COMP))
	  (T (PUTPROP TENSOR (CONS (CONS LEN1 COMP) NAME) PROP)))
    (OR (ZL-GET TENSOR 'INDEXED) ($INDEXED_TENSOR TENSOR))
    '$DONE) NIL NIL NIL NIL NIL))

;;(DEFUN SELECT (TENSOR L1 L2)
;;  ((LAMBDA (PROP SUBS INDEX)
;;	(COND ((AND (ALLFIXED SUBS) (SETQ PROP (ZL-GET TENSOR 'CARRAYS))
;;		    (SETQ PROP (ZL-ASSOC INDEX PROP)))
;;	       (COND ((ALIKE1 (SETQ PROP (CONS (LIST (CDR PROP) 'ARRAY) SUBS))
;;			      (SETQ SUBS (MEVAL PROP))) 0)
;;		     (T SUBS)))
;;	      ((SETQ PROP (ZL-ASSOC INDEX (ZL-GET TENSOR 'TEXPRS)))
;;	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) (CADR PROP)))
;;	      ((SETQ PROP (ZL-GET TENSOR 'TSUBR))
;;	       (APPLY PROP (LIST (CONS SMLIST L1)(CONS SMLIST L2))))
;;	      (T (LIST (LIST TENSOR 'SIMP)(CONS SMLIST L1)(CONS SMLIST L2)))))
;;	NIL (APPEND L1 L2)(CONS (LENGTH L1)(LENGTH L2))))

;;vtt: inconstant was an attempt to remove constant indices, but it really doesn't work out.
;;(DEFUN INCONSTANT (L)
;;  (COND 
;;    ((EQ L NIL) NIL)
;;    (($CONSTANTP (CAR L)) (AND (NOT (EQ NIL (CDR L))) (INCONSTANT (CDR L))))
;;    (T (CONS (CAR L) (AND (NOT (EQ NIL (CDR L))) (INCONSTANT (CDR L)))))
;;  )
;;)

(DEFUN SELECT (TENSOR L1 L2 L3)
  ((LAMBDA (PROP SUBS INDEX)
	(COND ((AND (ALLFIXED SUBS) (SETQ PROP (ZL-GET TENSOR 'CARRAYS))
		    (SETQ PROP (ZL-ASSOC INDEX PROP)))
	       (COND ((ALIKE1 (SETQ PROP (CONS (LIST (CDR PROP) 'ARRAY) SUBS))
			      (SETQ SUBS (MEVAL PROP))) 0)
		     (T SUBS)))
	      ((SETQ PROP (ZL-ASSOC INDEX (ZL-GET TENSOR 'TEXPRS)))
;;;VTT	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) (CADR PROP)))
;;;	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) ($RENAME (CADR PROP) (COND ((BOUNDP 'N) N) (T 1)))))
;; VTT: What is this business with N instead of $ICOUNTER anyway?
;;;	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) (CAR (CONS ($RENAME (CADR PROP) (1+ $ICOUNTER)) (COND ((BOUNDP 'N) (SETQ $ICOUNTER (1- N))) )))))
;;;	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) (CAR (CONS ($RENAME (CADR PROP) (1+ $ICOUNTER)) (COND ((BOUNDP 'N) (SETQ $ICOUNTER N)) )))))
;;;        (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) (CAR (CONS ($RENAME (CADR PROP) (1+ $COUNTER)) (SETQ $COUNTER (1- (COND ((BOUNDP 'N) N) (T 1))))))))
	       (SUBLIS (MAPCAR (FUNCTION CONS)(CDDR PROP) SUBS) ($RENAME (CADR PROP) (COND ((BOUNDP 'N) N) (T 1)))))


	      ((SETQ PROP (ZL-GET TENSOR 'TSUBR))
;;	       (APPLY PROP (LIST (CONS SMLIST (INCONSTANT L1))(CONS SMLIST (INCONSTANT L2))(CONS SMLIST L3))))
;;	      ((NOT (EQ L3 NIL)) (APPLY '$DIFF (SELECT TENSOR (INCONSTANT L1) (INCONSTANT L2) (CDR L3)) (LIST (CAR L3))))
;;	      (T (APPEND (LIST (LIST TENSOR 'SIMP)(CONS SMLIST (INCONSTANT L1))(CONS SMLIST (INCONSTANT L2))) L3))))
;;	NIL (APPEND (INCONSTANT L1) (INCONSTANT L2) L3)(LIST (LENGTH (INCONSTANT L1))(LENGTH (INCONSTANT L2))(LENGTH L3))))
	       (APPLY PROP (LIST (CONS SMLIST L1)(CONS SMLIST L2)(CONS SMLIST L3))))
	      ((NOT (EQ L3 NIL)) (APPLY '$IDIFF (SELECT TENSOR L1 L2 (CDR L3)) (LIST (CAR L3))))
	      (T (APPEND (LIST (LIST TENSOR 'SIMP)(CONS SMLIST L1)(CONS SMLIST L2)) L3))))
	NIL (APPEND L1 L2 L3)(LIST (LENGTH L1)(LENGTH L2)(LENGTH L3))))


(DEFMFUN $ENTERTENSOR nargs 
  (prog (fun contr cov deriv)
    (cond ((> nargs 1)
	   (merror "ENTERTENSOR takes 0 or 1 arguments only"))
	  ((= nargs 0)
	   (mtell "Enter tensor name: ") 
	   (setq fun (meval (retrieve nil nil))))
	  ((setq fun (arg 1))))
    (mtell "~%Enter a list of the covariant indices: ")
    (setq cov (checkindex (meval (retrieve nil nil)) fun))
    (cond ((atom cov) (setq cov (cons smlist (ncons cov)))))
    (mtell "~%Enter a list of the contravariant indices: ")
    (setq contr (checkindex (meval (retrieve nil nil)) fun))
    (cond ((atom contr) (setq contr (cons smlist (ncons contr)))))
    (mtell "~%Enter a list of the derivative indices: ")
    (setq deriv (checkindex (meval (retrieve nil nil)) fun))
    (setq deriv (cond ((atom deriv) (ncons deriv))
		      (t (cdr deriv))))
    (cond ((memberl (cdr cov) deriv)
	   (mtell "~%Warning - there are indices that are both covariant ~
		  and derivative%")))
    (return ($ISHOW (nconc (list (list fun 'SIMP) cov contr)
				       deriv)))))

(defun CHECKINDEX (e f)
  (cond ((and (atom e) (not (eq e f))) e)
	((and (eq (caar e) 'MLIST)
	      (sloop for v in (cdr e) always (atom v))
;	      (apply 'and (mapcar 'atom (cdr e)))
	      (not (memq f e))) e)
	(t (merror "Indices must be atoms different from the tensor name"))))

(defun MEMBERL (a b)
  (do ((l a (cdr l))
       (carl))
      ((null l) nil)
    (setq carl (car l))
    (cond ((and (eq (ml-typep carl) 'SYMBOL)
		(zl-member carl b)) (return t)))))

(defun CONSMLIST (l) (cons smlist l))			;Converts from Lisp list to Macsyma list

;$INDICES2 is similar to $INDICES except that here dummy indices are picked off
;as they first occur in going from left to right through the product or indexed
;object. Also, $INDICES2 works only on the top level of a product and will
;miss indices for products of sums (which is used to advantage by $IC_CONVERT).

(DEFMFUN $INDICES2 (e)
  (cond ((atom e) empty)
	((not (or (memq (caar e) '(MTIMES MNCTIMES)) (rpobj e)))
	 ($indices e))
	(t ((lambda (indices)
	      (do ((ind indices) (free) (dummy) (index))
		  ((null ind)
		   (consmlist (list (consmlist (nreverse free))
				    (consmlist (nreverse dummy)))))
		(setq index (car ind))
		(cond ((zl-member index dummy)
		       (merror "~M has improper indices"
			       (ishow e)))
		      ((zl-member index (cdr ind))
		       (setq dummy (cons index dummy)
			     ind (zl-delete index (copy (cdr ind))
					 1)))
		      (t (setq free (cons index free)
			       ind (cdr ind))))))
	    (do ((e (cond ((memq (caar e) '(MTIMES MNCTIMES)) (cdr e))
			  (t (ncons e))) (cdr e))
		 (a) (l))
		((null e) l)
	      (setq a (car e))
	      (and (rpobj a) (setq l (append l (cdadr a) (cdaddr a)
					     (cdddr a)))))))))

(DEFMFUN $CHANGENAME (a b e)				;Change the name of the indexed object A to B in E
  (prog (old indspec ncov ncontr)			;INDSPEC is INDex SPECification flag
    (cond ((not (or (and (eq (ml-typep a) 'SYMBOL) (setq old a))
		    (and ($listp a) (equal (length (cdr a)) 3)
			 (eq (ml-typep (setq old (cadr a))) 'SYMBOL)
			 (eq (ml-typep (setq ncov (caddr a))) 'FIXNUM)
			 (eq (ml-typep (setq ncontr (cadddr a))) 'FIXNUM)
			 (setq indspec t))))
	   (merror "Improper first argument to CHANGENAME: ~M" a))
	  ((not (eq (ml-typep b) 'SYMBOL))
	   (merror "Second argument to CHANGENAME must be a symbol"))
	  (t (return (changename old indspec ncov ncontr b e))))))

(defun CHANGENAME (a indspec ncov ncontr b e)
  (cond ((or (atom e) (eq (caar e) 'RAT)) e)
	((rpobj e)
	 (cond ((and (eq (caar e) a)
		     (cond (indspec (and (equal (length (cdadr e)) ncov)
					 (equal (length (cdaddr e))
						ncontr)))
			   (t t)))
		(cons (cons b (cdar e)) (cdr e)))
	       (t e)))
	(t (mysubst0 (cons (car e)
			   (mapcar (function
				    (lambda (q)
				      (changename a indspec ncov
						  ncontr b q)))
				   (cdr e))) e))))

(DEFMFUN $COORD n
  (do ((l (listify n) (cdr l)) (a))
      ((null l) '$DONE)
    (setq a (car l))
    (cond ((not (eq (ml-typep a) 'SYMBOL))
	   (merror "~M is not a valid name." a))
	  (t (add2lnc a $COORD)))))

(DEFMFUN $REMCOORD n
  (cond ((and (equal n 1) (eq (arg 1) '$ALL))
	 (setq $COORD '((MLIST))) '$DONE)
	(t (do ((l (listify n) (cdr l)))
	       ((null l) '$DONE)
	     (delq (car l) $COORD)))))


;; Additions on 5/19/2004 -- VTT

(DEFUN MEMBERLIST (E L)
	(COND ((NULL L) NIL)
	      ((EQUAL E (CAR L)) T)
	      (T (MEMBERLIST E (CDR L)))
	)
)

(DEFUN UNIONLIST (L1 L2)
	(COND ((NULL L1) L2)
	      ((MEMBERLIST (CAR L1) L2) (UNIONLIST (CDR L1) L2))
	      (T (CONS (CAR L1) (UNIONLIST (CDR L1) L2)))
	)
)

(DEFMFUN $LISTOFTENS (E) (itensor-sort (CONS SMLIST (LISTOFTENS E))))
(DEFUN LISTOFTENS (E)
	(COND
	  ((ATOM E) NIL)
	  ((RPOBJ E) (LIST E))
	  (T (PROG (L) (SETQ L NIL)
		(MAPCAR (LAMBDA (X) (SETQ L (UNIONLIST L (LISTOFTENS X)))) (CDR E))
		(RETURN L)
	     )
	  )
	)
)

(DEFUN NUMLIST (&optional (n '1)) (COND ((>= n $DIM) (LIST n)) (T (CONS n (NUMLIST (1+ n))))))

;;SHOWCOMPS(tensor):=BLOCK([i1,i2,ind:INDICES(tensor)[1]],
;;	IF LENGTH(ind)=0 THEN ISHOW(EV(tensor))
;;	ELSE IF LENGTH(ind)=1 THEN ISHOW(MAKELIST(EV(tensor,ind[1]=i1),i1,1,DIM))
;;	ELSE IF LENGTH(ind)=2 THEN ISHOW(tensor=APPLY('MATRIX,MAKELIST(MAKELIST(EV(tensor,[ind[1]=i1,ind[2]=i2]),i1,1,DIM),i2,1,DIM)))
;;	ELSE FOR i1 THRU DIM DO (SHOWCOMPS(SUBST(i1,LAST(ind),tensor)),IF LENGTH(ind)=3 AND i1<DIM THEN LINENUM:LINENUM+1)
;;);
(DEFMFUN $SHOWCOMPS (E)
 (PROG (IND)
  (SETQ IND (CDADR ($INDICES E)))
  (COND ((> 1 (LENGTH IND)) ($ISHOW (MEVAL (LIST '($EV) E))))
	((> 2 (LENGTH IND)) ($ISHOW (CONS SMLIST (MAPCAR (LAMBDA (I) (MEVAL (LIST '($EV) E (LIST '(MEQUAL) (CAR IND) I)))) (NUMLIST)))))
	((> 3 (LENGTH IND)) ($ISHOW (LIST '(MEQUAL) E (CONS '($MATRIX SIMP) (MAPCAR (LAMBDA (J) (CONS SMLIST (MAPCAR (LAMBDA (I) (MEVAL (LIST '($EV) E (LIST '(MEQUAL) (CAR IND) I) (LIST '(MEQUAL) (CADR IND) J)))) (NUMLIST)))) (NUMLIST))))))
	(T (MAPCAR (LAMBDA (I)  ($SHOWCOMPS ($SUBSTITUTE I (CAR (LAST IND)) E)) (AND (> 4 (LENGTH IND)) (< I $DIM) (SETQ $LINENUM (1+ $LINENUM)))) (NUMLIST)))
  )
 )
)

($load '$ex_calc)
($load 'lckdt)
($load 'iframe)
