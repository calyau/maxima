;=============================================================================
;    (c) copyright 1988	 Kent State University  kent, ohio 44242 
;		all rights reserved.
;
; Authors:  Paul S. Wang, Barbara Gates
; Permission to use this work for any purpose is granted provided that
; the copyright notice, author and support credits above are retained.
;=============================================================================

(declare (special piport poport))

(defun foreach macro (m)
;                                                                              ;
; (foreach elt   -->  (progn (mapc    (function (lambda (elt) stmt)) lst) nil) ;
;       in lst                                                                 ;
;       do stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->  (progn (map     (function (lambda (elt) stmt)) lst) nil) ;
;       on lst                                                                 ;
;       do stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->         (mapcar  (function (lambda (elt) stmt)) lst)      ;
;       in lst                                                                 ;
;  collect stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->         (maplist (function (lambda (elt) stmt)) lst)      ;
;       on lst                                                                 ;
;  collect stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->         (mapcan  (function (lambda (elt) stmt)) lst)      ;
;       in lst                                                                 ;
;     conc stmt)                                                               ;
;                                                                              ;
; (foreach elt   -->         (mapcon  (function (lambda (elt) stmt)) lst)      ;
;       on lst                                                                 ;
;     conc stmt)                                                               ;
;                                                                              ;
  (let (((foreach elt kw1 lst kw2 stmt) m) fcn)
       (setq fcn (cdr (assoc kw2 (cdr (assoc kw1 '((in (do . mapc)
						       (collect . mapcar)
						       (conc . mapcan))
						   (on (do . map)
						       (collect . maplist)
						       (conc . mapcon))))))))
       (cond ((member fcn '(mapc map))
	      `(progn (,fcn (function (lambda (,elt) ,stmt))
			    ,lst)
		      nil))
	     (t
	      `(,fcn (function (lambda (,elt) ,stmt))
		     ,lst)))))


(defun aconc macro (m)
;                                              ;
; (aconc lst elt)  -->  (nconc lst (list elt)) ;
;                                              ;
  `(nconc ,(cadr m) (list ,(caddr m))))


(defun compress macro (m)
;                                    ;
; (compress lst)  -->  (implode lst) ;
;                                    ;
  `(implode ,(cadr m)))


(defun de macro (m)
;                                                                        ;
; (de fname (p1 ... pn) body...)  -->  (defun fname (p1 ... pn) body...) ;
;                                                                        ;
  (cons 'defun (cdr m)))


(defun delete1 macro (m)
;                                            ;
; (delete1 elt lst)  -->  (delete elt lst 1) ;
;                                            ;
  (append1 (cons 'delete (cdr m)) 1))


(defun dskin macro (m)
;                                        ;
; (dskin filename)  -->  (load filename) ;
;                                        ;
  (cons 'load (cdr m)))


(defun error1 macro (m)
;                        ;
; (error1)  -->  (error) ;
;                        ;
  '(error))


(defun explode2 macro (m)
;                                     ;
; (explode2 arg)  -->  (explodec arg) ;
;                                     ;
  (cons 'explodec (cdr m)))


(defun filep macro (m)
;                                ;
; (filep str)  -->  (probef str) ;
;                                ;
  (cons 'probef (cdr m)))


(defun flag macro (m)
;                                                   ;
; (flag varlst fname)  -->  (foreach v in varlst do ;
;                             (putprop v t fname))  ;
;                                                   ;
  `(foreach v in ,(cadr m) do
     (putprop v t ,(caddr m))))


(defun flagp macro (m)
;                                         ;
; (flagp var fname)  -->  (get var fname) ;
;                                         ;
  (cons 'get (cdr m)))


(defun geq macro (m)
;                              ;
; (geq n1 n2)  -->  (>= n1 n2) ;
;                              ;
  (cons '>= (cdr m)))


(defun global macro (m)
;                                                     ;
; (global varlst)  -->  (declare (special v1 ... vn)) ;
;                                                     ;
  `(declare ,(cons 'special (cdr m))))


(defun idp macro (m)
;                               ;
; (idp exp)  -->  (symbolp exp) ;
;                               ;
  (cons 'symbolp (cdr m)))


(defun mkfil macro (m)
;                                     ;
; (mkfil arg)  -->  (stripdollar arg) ;
;                                     ;
  (cons 'stripdollar (cdr m)))


(defun posn macro (m)
;                       ;
; (posn)  -->  (nwritn) ;
;                       ;
  '(nwritn))


(defun prettyprint macro (m)
;                                                      ;
; (prettyprint exp)  -->  (prog1 ($prpr exp) (terpri)) ;
;                                                      ;
  `(prog1 ($prpr ,(cadr m)) (terpri)))


(defun prin2 macro (m)
;                               ;
; (prin2 exp)  -->  (patom exp) ;
;                               ;
  (cons 'patom (cdr m)))


(defun prin2t macro (m)
;                                                 ;
; (prin2t exp)  -->  (prog1 (patom exp) (terpri)) ;
;                                                 ;
  `(prog1 (patom ,(cadr m)) (terpri)))


(defun put macro (m)
;                                                               ;
; (put id proptype propval)  -->  (putprop id propval proptype) ;
;                                                               ;
  `(putprop ,(cadr m) ,(cadddr m) ,(caddr m)))


(defun rds macro (m)
;                                                   ;
; (rds arg)  -->   (prog1 piport (setq piport arg)) ;
;                                                   ;
  `(prog1 piport (setq piport ,(cadr m))))


(defun rederr macro (m)
;                                ;
; (rederr msg)  -->  (error msg) ;
;                                ;
  (cons 'error (cdr m)))


(defun remflag macro (m)
;                                                      ;
; (remflag varlst fname)  -->  (foreach v in varlst do ;
;                                (remprop v fname))    ;
;                                                      ;
  `(foreach v in ,(cadr m) do
     (remprop v ,(caddr m))))


(defun repeat macro (m)
;                                                             ;
; (repeat stmt exp)  -->  (prog ()                            ;
;                               loop                          ;
;                               stmt                          ;
;                               (cond ((not exp) (go loop)))) ;
;                                                             ;
  `(prog ()
	 loop
	 ,(cadr m)
	 (cond ((not ,(caddr m)) (go loop)))))


(defun spaces macro (m)
;                                       ;
; (spaces n)  -->  (do ((i n (sub1 i))) ;
;		       ((lessp i 1))    ;
;		       (princ " "))     ;
;                                       ;
  `(do ((i ,(cadr m) (sub1 i)))
       ((lessp i 1))
       (princ " ")))


(defun while macro (m)
;                                                          ;
; (while exp stmt)  -->  (prog ()                          ;
;                              loop                        ;
;                              (cond (exp                  ;
;                                     stmt                 ;
;                                     (go loop))))         ;
;                                                          ;
  `(prog ()
	 loop
	 (cond (,(cadr m)
	        ,(caddr m)
	        (go loop)))))


(defun wrs macro (m)
;                                                  ;
; (wrs arg)  -->  (prog1 poport (setq poport arg)) ;
;                                                  ;
  `(prog1 poport (setq poport ,(cadr m))))
