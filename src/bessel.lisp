;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")

;; Temporarily we establish an array convention for conversion
;; of this file to new type arrays.

(eval-when (compile eval)

;; It is more efficient to use the value cell, and we can probably
;; do this everywhere, but for now just use it in this file.
	   
(defmacro nsymbol-array (x) `(symbol-value ,x))
;(defmacro nsymbol-array (x) `(get ,x 'array))

(defmacro narray (x typ &rest dims) typ
  `(setf (nsymbol-array ',x)
	 (make-array
	   ,(if (cdr dims) `(mapcar '1+ (list ,@ dims))
	      `(1+ ,(car dims))))))
)

(declare-top(flonum (j[0]-bessel flonum) (j[1]-bessel flonum)
		 (j[n]-bessel flonum fixnum) (i[0]-bessel flonum)
		 (i[1]-bessel flonum) (i[n]-bessel flonum fixnum)
		 (g[0]-bessel flonum) (g[1]-bessel flonum)
		 (g[n]-bessel flonum fixnum))
	 (flonum x z y xa sx0 sq co si q p)
	 (special $jarray $iarray $garray)
	 (array* (flonum j-bessel-array 1. i-bessel-array 1.
			 g-bessel-array 1.))
	 (array* (flonum $jarray 1. $iarray 1. $garray 1.))
	 (*fexpr $array)) 

#-(or cl NIL)
(and (not (get '*f 'subr)) 
     (mapc #'(lambda (x) (putprop x '(arith fasl dsk liblsp) 'autoload))
	   '(*f //f _f +f -f)))

#-NIL
(declare-top(flonum (*f flonum flonum) (//f flonum flonum) 
		 (_f flonum fixnum) (+f flonum flonum) (-f flonum flonum))
	 (*expr *f //f _f +f -f))

#+(or cl NIL)
(eval-when (eval compile)
  (defmacro *f (a b) `(*$ ,a ,b))
  (defmacro //f (a b) `(//$ ,a ,b))
  (defmacro +f (a b) `(+$ ,a ,b))
  (defmacro -f (a b) `(-$ ,a ,b))
  ;_f isn't used here.  That would be scale-float, no open-code version.
  )


#+nil
(defun j[0]-bessel (x) 
       ((lambda (xa p q si co sq sx0 y z) 
		(cond ((> xa 8.0)
		       (setq y (+$ -0.7853982 xa) si (sin y) co (cos y))
		       (setq y (//$ 8.0 xa) z (*$ y y) sq (sqrt y))
		       (setq p (+$ 0.2820948
				    (*$ z (+$ -3.096437e-4
					      (*$ 6.943574e-6 z)))))
		       (setq q (//$ (*$ y (+$ 7.030992 (*$ 0.7550996 z)))
				    (+$ 1595.15 (*$ z (+$ 185.9156 z)))))
		       (*$ (+$ (*$ co p) (*$ q si)) sq))
		      ((> xa 4.0)
		       (setq y (*$ 0.015625 (*$ xa xa)) z (-$ (1-$ y)))
		       (setq sx0 5.5200781 p (-$ xa sx0))
		       (//$ (*$ p
				(+$ sx0 xa)
				(+$ 0.1920038
				     (*$ z
					 (+$ 0.2025329
					      y
					      (*$ z
						  (+$ 0.2290394
						       y
						       (*$ z
							   (+$ -0.3228404
								(*$ -0.70066 z)))))))))
			    (+$ 12.18896
				 (*$ y
				     (+$ 13.64497
					  (*$ y
					      (+$ 7.894887
						   (*$ y (+$ 2.775489 y)))))))))
		      (t (setq y (*$ 0.0625 (*$ xa xa)) sx0 2.40482554)
			 (setq p (-$ xa sx0))
			 (//$ (*$ p
				  (+$ sx0 xa)
				  (+$ -6.171667
				       (*$ y
					   (+$ 5.953519
						(*$ y
						    (+$ -1.754611
							 (*$ 0.173663 y)))))))
			      (+$ 35.6919 (*$ y (+$ 9.590446 y)))))))
	(abs x) 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))


(eval-when (compile eval load)
  ;; These are some macros and functions stolen from f2cl that we
  ;; need.
  (defmacro fdo (do_vble_clause predicate_clause &rest body)
    (let ((step (gensym))
	  (iteration_count (gensym)))
      `(prog* ((,step ,(third (third do_vble_clause)))
	       (,iteration_count 
		(max 0 (truncate (+ (- ,(third (first predicate_clause))
				       ,(second do_vble_clause))
				    ,step)
				 ,step)
		     )))
	(declare (type integer4 ,step ,iteration_count))
	;; initialise loop variable
	(setq ,(first do_vble_clause) ,(second do_vble_clause))
	loop
	(return
	  (cond				; all iterations done
	    ((zerop ,iteration_count) nil)
	    ;; execute loop, in/de-crement loop vble and decrement cntr
	    ,(cons 't 
		   (append 
		    (append body
			    `((setq ,(first do_vble_clause) 
			       ,(third do_vble_clause)
			       ,iteration_count 
			       (1- ,iteration_count))))
		    '((go loop)))))))))

  (defun col-major-index (indices dims)
    (flet ((get-offset (n bound)
	     (let ((lo (first bound)))
	       (if (and (numberp lo) (zerop lo))
		   n
		   `(- ,n ,lo))))
	   (get-size (bound)
	     (destructuring-bind (lo hi)
		 bound
	       (cond ((numberp lo)
		      (cond ((numberp hi)
			     (1+ (- hi lo)))
			    ((= lo 1)
			     hi)
			    (t
			     `(- ,hi ,(- lo 1)))))
		     (t
		      `(- ,hi (- ,lo 1)))))))
      (let* ((rev-idx (reverse indices))
	     (rev-dim (reverse dims))
	     (idx (get-offset (first rev-idx) (first rev-dim))))
	(do ((d (rest rev-dim) (rest d))
	     (n (rest rev-idx) (rest n)))
	    ((endp d)
	     idx)
	  (setf idx `(+ ,(get-offset (first n) (first d))
		      (* ,(get-size (first d)) ,idx)))))))

  (defmacro fref (arr indices bounds)
    `(aref ,arr ,(col-major-index indices bounds)))


  )					; eval-when

;; This is the Bessel function from TOMS 715 translated from Fortran
;; to Lisp via f2cl.
(let ((zero 0.0d0)
      (one 1.0d0)
      (three 3.0d0)
      (four 4.0d0)
      (eight 8.0d0)
      (five5 5.5d0)
      (sixty4 64.0d0)
      (oneov8 0.125d0)
      (p17 0.1716d0)
      (two56 256.0d0)
      (cons -0.11593151565841245d0)
      (pi2 0.6366197723675814d0)
      (twopi 6.283185307179586d0)
      (twopi1 6.28125d0)
      (twopi2 0.001935307179586477d0)
      (xmax 2.68d+8)
      (xsmall 3.72d-9)
      (xinf 1.79d+308)
      (xj0 2.404825557695773d0)
      (xj1 5.520078110286311d0)
      (xy0 0.8935769662791675d0)
      (xy1 3.957678419314858d0)
      (xy2 7.086051060301773d0)
      (xj01 616.0d0)
      (xj02 -0.0014244423042272315d0)
      (xj11 1413.0d0)
      (xj12 5.468602863106498d-4)
      (xy01 228.0d0)
      (xy02 0.002951966279167522d0)
      (xy11 1013.0d0)
      (xy12 6.471693148578683d-4)
      (xy21 1814.0d0)
      (xy22 1.1356030177269763d-4)
      (plg
       (make-array 4
                   :element-type
                   'double-float
                   :initial-contents
                   '(-24.562334077563243d0 236.42701335621504d0
                     -549.8995689585792d0 356.875484680715d0)))
      (qlg
       (make-array 4
                   :element-type
                   'double-float
                   :initial-contents
                   '(-35.553900764052415d0 194.00230218539474d0
                     -334.42903192607537d0 178.4377423403575d0)))
      (pj0
       (make-array 7
                   :element-type
                   'double-float
                   :initial-contents
                   '(6630299.790483379d0 -6.214070042354013d+8
                     2.7282507878605944d+10 -4.1298668500990865d+11
                     -0.1211703616459353d0 103.44222815443189d0
                     -36629.81465510708d0)))
      (qj0
       (make-array 5
                   :element-type
                   'double-float
                   :initial-contents
                   '(456126.9622421994d0 1.3985097372263436d+8
                     2.632819830085965d+10 2.388378799633229d+12
                     936.1402239233771d0)))
      (pj1
       (make-array 8
                   :element-type
                   'double-float
                   :initial-contents
                   '(4417.670702532509d0 11725.046279757104d0
                     10341.910641583727d0 -7287.970246446462d0
                     -12254.07816137899d0 -1831.9397969392085d0
                     48.5917033559165d0 743.2119668062425d0)))
      (qj1
       (make-array 7
                   :element-type
                   'double-float
                   :initial-contents
                   '(333.0731077464907d0 -2945.876654550934d0
                     18680.99000835919d0 -84055.06259116957d0
                     245991.0226258631d0 -357834.78026152303d0
                     -25.258076240801554d0)))
      (py0
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(10102.532948020907d0 -2128754.8474401794d0
                     2.0422274357376619d+8 -8.37162554512605d+9
                     1.0723538782003177d+11 -18.402381979244993d0)))
      (qy0
       (make-array 5
                   :element-type
                   'double-float
                   :initial-contents
                   '(664.7598668924019d0 238893.93209447252d0
                     5.566295662427825d+7 8.161718777729037d+9
                     5.887386573899703d+11)))
      (py1
       (make-array 7
                   :element-type
                   'double-float
                   :initial-contents
                   '(-14566.865832663636d0 4690528.861167863d0
                     -6.959043939461962d+8 4.360009863860306d+10
                     -5.510743520672264d+11 -2.221397696756619d+13
                     17.427031242901595d0)))
      (qy1
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(830.3085761207029d0 406699.8235253955d0
                     1.3960202770986832d+8 3.401510384997124d+10
                     5.426682441941235d+12 4.3386146580707263d+14)))
      (py2
       (make-array 8
                   :element-type
                   'double-float
                   :initial-contents
                   '(21363.5341693139d0 -1.0085539923498214d+7
                     2.1958827170518103d+9 -1.9363051266772086d+11
                     -1.2829912364088686d+11 6.701664186917323d+14
                     -8.072872690515022d+15 -17.439661319197498d0)))
      (qy2
       (make-array 7
                   :element-type
                   'double-float
                   :initial-contents
                   '(879.0336216812844d0 539247.3920976806d0
                     2.4727219475672305d+8 8.692612110420982d+10
                     2.25983779240429d+13 3.927242556964031d+15
                     3.4563724628846454d+17)))
      (p0
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(3480.6486443249273d0 21170.523380864946d0
                     41345.38663958077d0 22779.090197304686d0
                     0.8896154842421047d0 153.76201909008353d0)))
      (q0
       (make-array 5
                   :element-type
                   'double-float
                   :initial-contents
                   '(3502.8735138235606d0 21215.350561880117d0
                     41370.41249551042d0 22779.090197304686d0
                     157.11159858080893d0)))
      (p1
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(-22.300261666214197d0 -111.83429920482737d0
                     -185.91953644342993d0 -89.22660020080009d0
                     -0.008803330304868075d0 -1.244102674583564d0)))
      (q1
       (make-array 5
                   :element-type
                   'double-float
                   :initial-contents
                   '(1488.7231232283757d0 7264.278016921102d0
                     11951.131543434614d0 5710.502412851207d0
                     90.59376959499312d0))))
  (declare (type (array double-float (6)) p1 p0 qy1 py0)
           (type (array double-float (8)) py2 pj1)
           (type (array double-float (5)) q1 q0 qy0 qj0)
           (type (array double-float (7)) qy2 py1 qj1 pj0)
           (type (array double-float (4)) qlg plg)
           (type double-float xy22 xy21 xy12 xy11 xy02 xy01 xj12 xj11 xj02 xj01
            xy2 xy1 xy0 xj1 xj0 xinf xsmall xmax twopi2 twopi1 twopi pi2 cons
            two56 p17 oneov8 sixty4 five5 eight four three one zero))
  (defun caljy0 (arg result jint)
    (declare (type integer4 jint) (type double-float result arg))
    (prog ((ax 0.0d0) (down 0.0d0) (prod 0.0d0) (resj 0.0d0) (r0 0.0d0)
           (r1 0.0d0) (up 0.0d0) (w 0.0d0) (wsq 0.0d0) (xden 0.0d0)
           (xnum 0.0d0) (xy 0.0d0) (z 0.0d0) (zsq 0.0d0) (i 0))
      (declare (type integer4 i)
               (type double-float zsq z xy xnum xden wsq w up r1 r0 resj prod
                down ax))
      (setf ax (coerce (abs arg) 'double-float))
      (cond
       ((and (= jint 1) (<= arg zero)) (setf result (- xinf)) (go label2000))
       ((> ax xmax) (setf result zero) (go label2000)))
      (if (> ax eight) (go label800))
      (cond
       ((<= ax xsmall)
        (cond ((= jint 0) (setf result one))
              (t (setf result (* pi2 (+ (flog ax) cons)))))
        (go label2000)))
      (setf zsq (* ax ax))
      (cond
       ((<= ax four)
        (setf xnum
                (+
                 (* (+ (* (fref pj0 (5) ((1 7))) zsq) (fref pj0 (6) ((1 7))))
                    zsq)
                 (fref pj0 (7) ((1 7)))))
        (setf xden (+ zsq (fref qj0 (5) ((1 5)))))
        (fdo (i 1 (+ i 1))
             ((> i 4) nil)
             (tagbody
               (setf xnum (+ (* xnum zsq) (fref pj0 (i) ((1 7)))))
               (setf xden (+ (* xden zsq) (fref qj0 (i) ((1 5)))))
              label50))
        (setf prod (* (- (+ ax (/ (- xj01) two56)) xj02) (+ ax xj0))))
       (t (setf wsq (+ one (/ (- zsq) sixty4)))
        (setf xnum (+ (* (fref pj1 (7) ((1 8))) wsq) (fref pj1 (8) ((1 8)))))
        (setf xden (+ wsq (fref qj1 (7) ((1 7)))))
        (fdo (i 1 (+ i 1))
             ((> i 6) nil)
             (tagbody
               (setf xnum (+ (* xnum wsq) (fref pj1 (i) ((1 8)))))
               (setf xden (+ (* xden wsq) (fref qj1 (i) ((1 7)))))
              label220))
        (setf prod (* (+ ax xj1) (- (+ ax (/ (- xj11) two56)) xj12)))))
      (setf result (/ (* prod xnum) xden))
      (if (= jint 0) (go label2000))
      (cond
       ((<= ax three) (setf up (- (+ ax (/ (- xy01) two56)) xy02))
        (setf xy xy0))
       ((<= ax five5) (setf up (- (+ ax (/ (- xy11) two56)) xy12))
        (setf xy xy1))
       (t (setf up (- (+ ax (/ (- xy21) two56)) xy22)) (setf xy xy2)))
      (setf down (+ ax xy))
      (cond
       ((< (abs up) (* p17 down)) (setf w (/ up down)) (setf wsq (* w w))
        (setf xnum (fref plg (1) ((1 4))))
        (setf xden (+ wsq (fref qlg (1) ((1 4)))))
        (fdo (i 2 (+ i 1))
             ((> i 4) nil)
             (tagbody
               (setf xnum (+ (* xnum wsq) (fref plg (i) ((1 4)))))
               (setf xden (+ (* xden wsq) (fref qlg (i) ((1 4)))))
              label320))
        (setf resj (/ (* pi2 result w xnum) xden)))
       (t (setf resj (* pi2 result (flog (/ ax xy))))))
      (cond
       ((<= ax three)
        (setf xnum (+ (* (fref py0 (6) ((1 6))) zsq) (fref py0 (1) ((1 6)))))
        (setf xden (+ zsq (fref qy0 (1) ((1 5)))))
        (fdo (i 2 (+ i 1))
             ((> i 5) nil)
             (tagbody
               (setf xnum (+ (* xnum zsq) (fref py0 (i) ((1 6)))))
               (setf xden (+ (* xden zsq) (fref qy0 (i) ((1 5)))))
              label340)))
       ((<= ax five5)
        (setf xnum (+ (* (fref py1 (7) ((1 7))) zsq) (fref py1 (1) ((1 7)))))
        (setf xden (+ zsq (fref qy1 (1) ((1 6)))))
        (fdo (i 2 (+ i 1))
             ((> i 6) nil)
             (tagbody
               (setf xnum (+ (* xnum zsq) (fref py1 (i) ((1 7)))))
               (setf xden (+ (* xden zsq) (fref qy1 (i) ((1 6)))))
              label360)))
       (t (setf xnum (+ (* (fref py2 (8) ((1 8))) zsq) (fref py2 (1) ((1 8)))))
        (setf xden (+ zsq (fref qy2 (1) ((1 7)))))
        (fdo (i 2 (+ i 1))
             ((> i 7) nil)
             (tagbody
               (setf xnum (+ (* xnum zsq) (fref py2 (i) ((1 8)))))
               (setf xden (+ (* xden zsq) (fref qy2 (i) ((1 7)))))
              label380))))
      (setf result (+ resj (/ (* up down xnum) xden)))
      (go label2000)
     label800
      (setf z (/ eight ax))
      (setf w (/ ax twopi))
      (setf w (+ (aint w) oneov8))
      (setf w (- ax (* w twopi1) (* w twopi2)))
      (setf zsq (* z z))
      (setf xnum (+ (* (fref p0 (5) ((1 6))) zsq) (fref p0 (6) ((1 6)))))
      (setf xden (+ zsq (fref q0 (5) ((1 5)))))
      (setf up (+ (* (fref p1 (5) ((1 6))) zsq) (fref p1 (6) ((1 6)))))
      (setf down (+ zsq (fref q1 (5) ((1 5)))))
      (fdo (i 1 (+ i 1))
           ((> i 4) nil)
           (tagbody
             (setf xnum (+ (* xnum zsq) (fref p0 (i) ((1 6)))))
             (setf xden (+ (* xden zsq) (fref q0 (i) ((1 5)))))
             (setf up (+ (* up zsq) (fref p1 (i) ((1 6)))))
             (setf down (+ (* down zsq) (fref q1 (i) ((1 5)))))
            label850))
      (setf r0 (/ xnum xden))
      (setf r1 (/ up down))
      (cond
       ((= jint 0)
        (setf result
                (* (fsqrt (/ pi2 ax))
                   (+ (* r0 (cos w)) (* (- z) r1 (sin w))))))
       (t
        (setf result
                (* (fsqrt (/ pi2 ax)) (+ (* r0 (sin w)) (* z r1 (cos w)))))))
     label2000
      (go end_label)
     end_label
      (return (values arg result jint)))))

(defun besj0 (x)
  (declare (type double-float x))
  (prog ((jint 0) (besj0 0.0d0) (result 0.0d0))
    (declare (type double-float result besj0) (type integer4 jint))
    (declare
     (ftype (function (double-float double-float integer4) (values &rest t))
      caljy0))
    (setf jint 0)
    (multiple-value-bind
        (var-0 var-1 var-2)
        (caljy0 x result jint)
      (declare (ignore))
      (when var-0 (setf x var-0))
      (when var-1 (setf result var-1))
      (when var-2 (setf jint var-2)))
    (setf besj0 result)
    (go end_label)
   end_label
    (return (values besj0 x))))

;;
;; Bessel function of the first kind of order 0.
;;
;; One definition is
;;
;;         INF
;;         ====       k  2 k
;;         \     (- 1)  z
;;          >    -----------
;;         /       2 k   2
;;         ====   2    k!
;;         k = 0
;;
;; We only support computing this for real z.
;;
(defun j[0]-bessel (x) 
   (besj0 (float x 1d0)))

(defun $j0 ($x)
  (cond ((numberp $x) (j[0]-bessel (float $x)))
	(t (list '($j0 simp) $x))))


#+nil
(defun j[1]-bessel (x) 
       (declare (flonum x))
       ((lambda (xa p q si co sq sx0 rj1 y z) 
     	 (declare (flonum z y xa rj1 sx0 sq co si q p))
	 (setq xa (abs x))
	 (cond ((> xa 8.0)
		(setq y (+$ -2.356194 xa) si (sin y) co (cos y))
		(setq y (//$ 8.0 xa) z (*$ y y) sq (sqrt y))
		(setq p (+$ 0.2820948 (*$ z (+$ 5.162034e-4 (*$ -9.002696e-6 z)))))
		(setq q (//$ (*$ y (+$ 50.53199 (*$ 4.999898 z)))
			     (+$ 3821.467 (*$ z (+$ 394.4419 z)))))
		(setq rj1 (*$ (-$ (*$ co p) (*$ q si)) sq)))
	       ((> xa 4.0)
		(setq y (*$ 0.015625 (*$ xa xa)) z (-$ (1-$ y)))
		(setq sx0 7.0155867 p (-$ xa sx0))
		(setq rj1
		      (//$ (*$ p
			       xa
			       (+$ sx0 xa)
			       (+$ 0.04297259
				    (*$ z
					(+$ 0.06689943
					     (*$ z
						 (+$ -0.05380065
						      (*$ z
							  (+$ -0.1045012
							       (*$ -0.04185412
								   z)))))))))
			   (+$ 8.886393
				(*$ y (+$ 8.204713 (*$ y (+$ 3.566279 y))))))))
	       (t (setq y (*$ 0.0625 (*$ xa xa)) sx0 3.83170596 p (-$ xa sx0))
		  (setq rj1
			(//$ (*$ p
				 xa
				 (+$ sx0 xa)
				 (+$ -4.665107
				      (*$ y (+$ 2.497075 (*$ -0.3222962 y)))))
			     (+$ 136.9859
				  (*$ y (+$ 51.3648 (*$ y (+$ 9.447542 y)))))))))
	 (and (< x 0.0) (setq rj1 (-$ rj1)))
	 rj1)
	(abs x) 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))

;; TOMS 715 routine translated from Fortran via f2cl.
(let ((eight 8.0d0)
      (four 4.0d0)
      (half 0.5d0)
      (throv8 0.375d0)
      (pi2 0.6366197723675814d0)
      (p17 0.1716d0)
      (twopi 6.283185307179586d0)
      (zero 0.0d0)
      (twopi1 6.28125d0)
      (twopi2 0.001935307179586477d0)
      (two56 256.0d0)
      (rtpi2 0.7978845608028654d0)
      (xmax 2.68d+8)
      (xsmall 3.72d-9)
      (xinf 1.79d+308)
      (xj0 3.8317059702075125d0)
      (xj1 7.015586669815619d0)
      (xy0 2.197141326031017d0)
      (xy1 5.429681040794135d0)
      (xj01 981.0d0)
      (xj02 -3.252797924876844d-4)
      (xj11 1796.0d0)
      (xj12 -3.833018438124646d-5)
      (xy01 562.0d0)
      (xy02 0.0018288260310170353d0)
      (xy11 1390.0d0)
      (xy12 -6.459205864867228d-6)
      (plg
       (make-array 4
                   :element-type
                   'double-float
                   :initial-contents
                   '(-24.562334077563243d0 236.42701335621504d0
                     -549.8995689585792d0 356.875484680715d0)))
      (qlg
       (make-array 4
                   :element-type
                   'double-float
                   :initial-contents
                   '(-35.553900764052415d0 194.00230218539474d0
                     -334.42903192607537d0 178.4377423403575d0)))
      (pj0
       (make-array 7
                   :element-type
                   'double-float
                   :initial-contents
                   '(980629.0409895827d0 -1.1548696764841278d+8
                     6.678104126149239d+9 -1.4258509801366642d+11
                     -4461.579298277508d0 10.650724020080236d0
                     -0.010767857011487301d0)))
      (qj0
       (make-array 5
                   :element-type
                   'double-float
                   :initial-contents
                   '(591176.144941748d0 2.0228375140097027d+8
                     4.2091902282580135d+10 4.186860446082018d+12
                     1074.227223951738d0)))
      (pj1
       (make-array 8
                   :element-type
                   'double-float
                   :initial-contents
                   '(4.6179191852758255d0 -7132.900687256095d0
                     4503965.810574908d0 -1.443771771836324d+9
                     2.3569285397217157d+11 -1.6324168293282543d+13
                     1.1357022719979468d+14 1.0051899717115284d+15)))
      (qj1
       (make-array 7
                   :element-type
                   'double-float
                   :initial-contents
                   '(1126712.5065029138d0 6.48725028995964d+8
                     2.762277728624409d+11 8.489934616548143d+13
                     1.7128800897135812d+16 1.725390588844768d+18
                     1388.6978985861358d0)))
      (py0
       (make-array 7
                   :element-type
                   'double-float
                   :initial-contents
                   '(221579.5322228026d0 -5.91574799974084d+7
                     7.214454821450256d+9 -3.7595974497819595d+11
                     5.470861171652542d+12 4.053572661257954d+13
                     -317.1442466004613d0)))
      (qy0
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(820.7990816839387d0 381364.7075305257d0
                     1.2250435122182964d+8 2.780035273869058d+10
                     4.1272286200406455d+12 3.073787392107929d+14)))
      (py1
       (make-array 9
                   :element-type
                   'double-float
                   :initial-contents
                   '(1915380.6858264203d0 -1.1957961912070617d+9
                     3.7453673962438494d+11 -5.953071312974199d+13
                     4.068627528980475d+15 -2.363840849704313d+16
                     -5.680809457472421d+18 1.1514276357909012d+19
                     -1233.7180442012955d0)))
      (qy1
       (make-array 8
                   :element-type
                   'double-float
                   :initial-contents
                   '(1285.516484932161d0 1045374.8201934079d0
                     6.355031808708892d+8 3.02217668529604d+11
                     1.118701006585697d+14 3.0837179548112886d+16
                     5.696819882285717d+18 5.332184431331618d+20)))
      (p0
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(-109824.05543459347d0 -1523529.3511811374d0
                     -6603373.248364938d0 -9942246.505077641d0
                     -4435757.816794128d0 -1611.61664432461d0)))
      (q0
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(-107263.85991103819d0 -1511809.5066341609d0
                     -6585339.4797230875d0 -9934124.389934584d0
                     -4435757.816794128d0 -1455.0094401904962d0)))
      (p1
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(1706.375429020768d0 18494.262873223866d0
                     66178.83658127084d0 85145.1606753357d0
                     33220.913409857225d0 35.26513384663603d0)))
      (q1
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(37890.2297457722d0 400294.43582266977d0
                     1419460.6696037208d0 1819458.042243997d0
                     708712.8194102874d0 863.836776960499d0))))
  (declare (type (array double-float (9)) py1)
           (type (array double-float (6)) q1 p1 q0 p0 qy0)
           (type (array double-float (8)) qy1 pj1)
           (type (array double-float (5)) qj0)
           (type (array double-float (7)) py0 qj1 pj0)
           (type (array double-float (4)) qlg plg)
           (type double-float xy12 xy11 xy02 xy01 xj12 xj11 xj02 xj01 xy1 xy0
            xj1 xj0 xinf xsmall xmax rtpi2 two56 twopi2 twopi1 zero twopi p17
            pi2 throv8 half four eight))
  (defun caljy1 (arg result jint)
    (declare (type integer4 jint) (type double-float result arg))
    (prog ((i 0) (zsq 0.0d0) (z 0.0d0) (xy 0.0d0) (xnum 0.0d0) (xden 0.0d0)
           (wsq 0.0d0) (w 0.0d0) (up 0.0d0) (r1 0.0d0) (r0 0.0d0) (resj 0.0d0)
           (prod 0.0d0) (down 0.0d0) (ax 0.0d0))
      (declare
       (type double-float ax down prod resj r0 r1 up w wsq xden xnum xy z zsq)
       (type integer4 i))
      (declare (ftype (function (double-float) (values single-float)) abs))
      (declare (ftype (function (double-float) (values integer4)) log))
      (declare (ftype (function (double-float) (values single-float)) aint))
      (declare (ftype (function (double-float) (values single-float)) sqrt))
      (declare (ftype (function (double-float) (values integer4)) cos))
      (declare (ftype (function (double-float) (values integer4)) sin))
      (setf ax (coerce (abs arg) 'double-float))
      (cond
       ((and (= jint 1)
             (or (<= arg zero) (and (< arg half) (< (* ax xinf) pi2))))
        (setf result (- xinf)) (go label2000))
       ((> ax xmax) (setf result zero) (go label2000)))
      (cond ((> ax eight) (go label800))
            ((<= ax xsmall)
             (cond ((= jint 0) (setf result (* arg half)))
                   (t (setf result (/ (- pi2) ax))))
             (go label2000)))
      (setf zsq (* ax ax))
      (cond
       ((<= ax four)
        (setf xnum
                (+
                 (* (+ (* (fref pj0 (7) ((1 7))) zsq) (fref pj0 (6) ((1 7))))
                    zsq)
                 (fref pj0 (5) ((1 7)))))
        (setf xden (+ zsq (fref qj0 (5) ((1 5)))))
        (fdo (i 1 (+ i 1))
             ((> i 4) nil)
             (tagbody
               (setf xnum (+ (* xnum zsq) (fref pj0 (i) ((1 7)))))
               (setf xden (+ (* xden zsq) (fref qj0 (i) ((1 5)))))
              label50))
        (setf prod (* arg (- (+ ax (/ (- xj01) two56)) xj02) (+ ax xj0))))
       (t (setf xnum (fref pj1 (1) ((1 8))))
        (setf xden
                (+ (* (+ zsq (fref qj1 (7) ((1 7)))) zsq)
                   (fref qj1 (1) ((1 7)))))
        (fdo (i 2 (+ i 1))
             ((> i 6) nil)
             (tagbody
               (setf xnum (+ (* xnum zsq) (fref pj1 (i) ((1 8)))))
               (setf xden (+ (* xden zsq) (fref qj1 (i) ((1 7)))))
              label220))
        (setf xnum
                (+ (* xnum (- ax eight) (+ ax eight)) (fref pj1 (7) ((1 8)))))
        (setf xnum (+ (* xnum (- ax four) (+ ax four)) (fref pj1 (8) ((1 8)))))
        (setf prod (* arg (- (+ ax (/ (- xj11) two56)) xj12) (+ ax xj1)))))
      (setf result (* prod (/ xnum xden)))
      (if (= jint 0) (go label2000))
      (cond
       ((<= ax four) (setf up (- (+ ax (/ (- xy01) two56)) xy02))
        (setf xy xy0))
       (t (setf up (- (+ ax (/ (- xy11) two56)) xy12)) (setf xy xy1)))
      (setf down (+ ax xy))
      (cond
       ((< (abs up) (* p17 down)) (setf w (/ up down)) (setf wsq (* w w))
        (setf xnum (fref plg (1) ((1 4))))
        (setf xden (+ wsq (fref qlg (1) ((1 4)))))
        (fdo (i 2 (+ i 1))
             ((> i 4) nil)
             (tagbody
               (setf xnum (+ (* xnum wsq) (fref plg (i) ((1 4)))))
               (setf xden (+ (* xden wsq) (fref qlg (i) ((1 4)))))
              label320))
        (setf resj (/ (* pi2 result w xnum) xden)))
       (t (setf resj (* pi2 result (flog (/ ax xy))))))
      (cond
       ((<= ax four)
        (setf xnum (+ (* (fref py0 (7) ((1 7))) zsq) (fref py0 (1) ((1 7)))))
        (setf xden (+ zsq (fref qy0 (1) ((1 6)))))
        (fdo (i 2 (+ i 1))
             ((> i 6) nil)
             (tagbody
               (setf xnum (+ (* xnum zsq) (fref py0 (i) ((1 7)))))
               (setf xden (+ (* xden zsq) (fref qy0 (i) ((1 6)))))
              label340)))
       (t (setf xnum (+ (* (fref py1 (9) ((1 9))) zsq) (fref py1 (1) ((1 9)))))
        (setf xden (+ zsq (fref qy1 (1) ((1 8)))))
        (fdo (i 2 (+ i 1))
             ((> i 8) nil)
             (tagbody
               (setf xnum (+ (* xnum zsq) (fref py1 (i) ((1 9)))))
               (setf xden (+ (* xden zsq) (fref qy1 (i) ((1 8)))))
              label360))))
      (setf result (+ resj (/ (* (/ (* up down) ax) xnum) xden)))
      (go label2000)
     label800
      (setf z (/ eight ax))
      (setf w (+ (aint (/ ax twopi)) throv8))
      (setf w (- ax (* w twopi1) (* w twopi2)))
      (setf zsq (* z z))
      (setf xnum (fref p0 (6) ((1 6))))
      (setf xden (+ zsq (fref q0 (6) ((1 6)))))
      (setf up (fref p1 (6) ((1 6))))
      (setf down (+ zsq (fref q1 (6) ((1 6)))))
      (fdo (i 1 (+ i 1))
           ((> i 5) nil)
           (tagbody
             (setf xnum (+ (* xnum zsq) (fref p0 (i) ((1 6)))))
             (setf xden (+ (* xden zsq) (fref q0 (i) ((1 6)))))
             (setf up (+ (* up zsq) (fref p1 (i) ((1 6)))))
             (setf down (+ (* down zsq) (fref q1 (i) ((1 6)))))
            label850))
      (setf r0 (/ xnum xden))
      (setf r1 (/ up down))
      (cond
       ((= jint 0)
        (setf result
                (* (/ rtpi2 (fsqrt ax))
                   (+ (* r0 (cos w)) (* (- z) r1 (sin w))))))
       (t
        (setf result
                (* (/ rtpi2 (fsqrt ax)) (+ (* r0 (sin w)) (* z r1 (cos w)))))))
      (if (and (= jint 0) (< arg zero)) (setf result (- result)))
     label2000
      (go end_label)
     end_label
      (return (values arg result jint)))))

(defun besj1 (x)
  (declare (type double-float x))
  (prog ((jint 0) (besj1 0.0d0) (result 0.0d0))
    (declare (type double-float result besj1) (type integer4 jint))
    (declare
     (ftype (function (double-float double-float integer4) (values &rest t))
      caljy1))
    (setf jint 0)
    (multiple-value-bind
        (var-0 var-1 var-2)
        (caljy1 x result jint)
      (declare (ignore))
      (when var-0 (setf x var-0))
      (when var-1 (setf result var-1))
      (when var-2 (setf jint var-2)))
    (setf besj1 result)
    (go end_label)
   end_label
    (return (values besj1 x))))

;; Bessel function of the first kind of order 1.
;;
;; One definition is
;;
;;      INF
;;      ====   - 2 k - 1      k  2 k + 1
;;      \     2          (- 1)  z
;;       >    --------------------------
;;      /            k! (k + 1)!
;;      ====
;;      k = 0

(defun j[1]-bessel (x) 
   (besj1 (float x 1d0)))

(defun $j1 ($x)
  (cond ((numberp $x) (j[1]-bessel (float $x)))
	(t (list '($j1 simp) $x))))

(defun j[n]-bessel (x n) 
       (declare (fixnum n) (flonum x)) 
       (prog (a0 a1 ak b0 b1 bk ck den fm fi gn ns qk rj0 rjn m) 
	     (declare #-cl (fixnum ns m)
		      (flonum rjn rj0 qk gn fi fm den ck bk b1 b0 ak a1 a0))
	     (setq ns (cond ((< n 0.) -1.) (t 1.)) n (abs n))
	     (*rearray 'j-bessel-array)
	     (narray j-bessel-array flonum (f1+ n))
	     (setq fm (float (f1+ n)) fi (*$ 1.25 (abs x)))
	     (setq m (fix (cond ((> fm fi) fm) (t fi))))
	     (setq fi (float (f+ m m)) fm fi qk (//$ x fm))
	     (cond ((> (abs x) 2.0e-4)
		    (setq a0 -1.0 a1 0.0 b0 0.0 b1 1.0)
		    (do nil
			(nil)
			(setq fi (+$ 2.0 fi) 
			      ck (//$ fi (abs x)) 
			      ak (-$ (*$ a1 ck) a0))
			(setq bk (-$ (*$ b1 ck) b0) gn qk a0 a1 a1 ak b0 b1 b1 bk)
			(setq qk (//$ ak bk))
			(or (> (abs (//$ (-$ qk gn) qk)) 1.0e-6) (return nil)))
		    (and (< x 0.0) (setq qk (-$ qk)))))
	     (do ((i m (f1- i)))
		 ((> 1. i))
		 (declare (fixnum i))
		 (setq den (-$ fm (*$ qk x)))
		 (and (= den 0.0) (setq den (*$ 1.0e-7 fm)))
		 (setq qk (//$ x den))
		 (or (< n i) (setf (arraycall 'flonum
					      (nsymbol-array 'j-bessel-array)
					      i) qk))
		 (setq fm (+$ -2.0 fm)))
	     (cond ((> 1.0 (abs qk)) (setq rj0 (j[0]-bessel x) rjn (*$ qk rj0)))
		   (t (setq rjn (j[1]-bessel x) rj0 (//$ rjn qk))))
	     (setf (arraycall 'flonum (nsymbol-array 'j-bessel-array) 0.) rj0)
	     (or (> n 0.) (return (arraycall 'flonum (nsymbol-array 'j-bessel-array) 0.)))
	     (setf (arraycall 'flonum (nsymbol-array 'j-bessel-array) 1.) rjn)
	     (or (> n 1.)
		 (return (setf (arraycall 'flonum
					  (nsymbol-array 'j-bessel-array) 1.)
				(*$ (float ns)
				    (arraycall 'flonum
					       (nsymbol-array
						 'j-bessel-array) 1.)))))
	     (and (= x 0.0) (return 0.0))
	     (do ((i 2. (f1+ i)))
		 ((> i n))
		 (declare (fixnum i))
		 (cond ((or (> (abs (arraycall 'flonum
					       (nsymbol-array
						 'j-bessel-array) i)) 1.0)
			    (> (abs rjn) (//$ #+cl *small-flonum*
					      #-cl 1.0e-38 ;won't read
					      (abs (arraycall 'flonum (nsymbol-array 'j-bessel-array) i)))))
			(setq rjn (*$ (arraycall 'flonum (nsymbol-array 'j-bessel-array) i) rjn)))
		       (t (setq rjn 0.0)))
		 (setf (arraycall 'flonum (nsymbol-array 'j-bessel-array) i) rjn))
	     (and (< ns 0.)
		  (do ((i 1. (f+ i 2.)))
		      ((> i n))
		      (setf (arraycall 'flonum (nsymbol-array 'j-bessel-array) i) (-$ (arraycall 'flonum (nsymbol-array 'j-bessel-array) i)))))
	     (return (arraycall 'flonum (nsymbol-array 'j-bessel-array) n)))) 

(defun $jn ($x $n)
  (cond ((and (numberp $x) (integerp $n))
	 (j[n]-bessel (float $x) $n)
	 (narray $jarray $float (abs $n))
	 ;(apply '$array (list '$jarray '$float (abs $n)))
	 (fillarray (nsymbol-array '$jarray) (nsymbol-array 'j-bessel-array))
	 (arraycall 'flonum (nsymbol-array '$jarray) (abs $n)))
	(t (list '($jn simp) $x $n))))



#+nil
(defun i[0]-bessel (x) 
       (declare (flonum x)) 
       ((lambda (xa y z) 
		(declare (flonum z y xa)) 
		(setq xa (abs x))
		(cond ((> 4.0 xa)
		       (setq z (*$ 0.0625 (*$ xa xa)))
		       (//$ (+$ -162.6391
				 (*$ z
				     (+$ -585.5938
					  (*$ z (+$ -402.5407 (*$ -75.72017 z))))))
			    (+$ -162.6391
				 (*$ z (+$ 64.96299 (*$ z (+$ -11.84469 z)))))))
		      (t (setq y (//$ 4.0 xa) z (-$ (1-$ y)))
			 (*$ (exp xa)
			     (sqrt y)
			     (//$ (+$ 2.67093
				       (*$ z (+$ 2.470948 (*$ z (+$ 6.271432 z))))))
			     (+$ 0.5528884
				  (*$ z
				      (+$ 0.4861227
					   (*$ z
					       (+$ 1.281496 (*$ 0.1555914 z))))))))))
	(abs x) 0.0 0.0))

;; TOMS 715 routine translated from Fortran via f2cl.
(let ((one 1.0d0)
      (one5 15.0d0)
      (exp40 2.3538526683702d+17)
      (forty 40.0d0)
      (rec15 0.06666666666666667d0)
      (two25 225.0d0)
      (xsmall 5.55d-17)
      (xinf 1.79d+308)
      (xmax 713.986d0)
      (p
       (make-array 15
                   :element-type
                   'double-float
                   :initial-contents
                   '(-5.24878666279457d-18 -1.5982226675653183d-14
                     -2.6843448573468487d-11 -3.051722645045107d-8
                     -2.5172644670688976d-5 -0.01545397779178685d0
                     -7.093534744921055d0 -2412.51958760419d0
                     -595456.260198479d0 -1.0313066708737981d+8
                     -1.1912746104985236d+10 -8.492510124711417d+11
                     -3.294008762740775d+13 -5.505036967301842d+14
                     -2.2335582639474377d+15)))
      (q
       (make-array 5
                   :element-type
                   'double-float
                   :initial-contents
                   '(-3727.756017996277d0 6515850.641865517d0
                     -6.562656074083388d+9 3.7604188704092956d+12
                     -9.708794617959403d+14)))
      (pp
       (make-array 8
                   :element-type
                   'double-float
                   :initial-contents
                   '(-0.3984375d0 2.920538459633679d0 -2.4708469169133953d0
                     0.4791488942285682d0 -0.003738499192606897d0
                     -0.0026801520353328633d0 9.916877767098369d-5
                     -2.1877128189032727d-6)))
      (qq
       (make-array 7
                   :element-type
                   'double-float
                   :initial-contents
                   '(-31.44669027513549d0 85.53956325801293d0
                     -60.228002066743336d0 13.982595353892851d0
                     -1.1151759188741313d0 0.03254769759481961d0
                     -5.519433023100548d-4))))
  (declare (type (array double-float (7)) qq)
           (type (array double-float (8)) pp)
           (type (array double-float (5)) q)
           (type (array double-float (15)) p)
           (type double-float xmax xinf xsmall two25 rec15 forty exp40 one5
            one))
  (defun calci0 (arg result jint)
    (declare (type integer4 jint) (type double-float result arg))
    (prog ((a 0.0d0) (b 0.0d0) (sump 0.0d0) (sumq 0.0d0) (x 0.0d0) (xx 0.0d0)
           (i 0))
      (declare (type integer4 i) (type double-float xx x sumq sump b a))
      (declare (ftype (function (double-float) (values single-float)) abs))
      (declare (ftype (function (double-float) (values integer4)) exp))
      (declare (ftype (function (double-float) (values single-float)) sqrt))
      (setf x (coerce (abs arg) 'double-float))
      (cond ((< x xsmall) (setf result one))
            ((< x one5) (setf xx (* x x)) (setf sump (fref p (1) ((1 15))))
             (fdo (i 2 (+ i 1))
                  ((> i 15) nil)
                  (tagbody
                    (setf sump (+ (* sump xx) (fref p (i) ((1 15)))))
                   label50))
             (setf xx (- xx two25))
             (setf sumq
                     (+
                      (*
                       (+
                        (*
                         (+
                          (*
                           (+ (* (+ xx (fref q (1) ((1 5)))) xx)
                              (fref q (2) ((1 5))))
                           xx)
                          (fref q (3) ((1 5))))
                         xx)
                        (fref q (4) ((1 5))))
                       xx)
                      (fref q (5) ((1 5)))))
             (setf result (/ sump sumq))
             (if (= jint 2) (setf result (* result (exp (- x))))))
            ((>= x one5)
             (cond ((and (= jint 1) (> x xmax)) (setf result xinf))
                   (t (setf xx (- (/ one x) rec15))
                    (setf sump
                            (+
                             (*
                              (+
                               (*
                                (+
                                 (*
                                  (+
                                   (*
                                    (+
                                     (*
                                      (+
                                       (*
                                        (+ (* (fref pp (1) ((1 8))) xx)
                                           (fref pp (2) ((1 8))))
                                        xx)
                                       (fref pp (3) ((1 8))))
                                      xx)
                                     (fref pp (4) ((1 8))))
                                    xx)
                                   (fref pp (5) ((1 8))))
                                  xx)
                                 (fref pp (6) ((1 8))))
                                xx)
                               (fref pp (7) ((1 8))))
                              xx)
                             (fref pp (8) ((1 8)))))
                    (setf sumq
                            (+
                             (*
                              (+
                               (*
                                (+
                                 (*
                                  (+
                                   (*
                                    (+
                                     (*
                                      (+ (* (+ xx (fref qq (1) ((1 7)))) xx)
                                         (fref qq (2) ((1 7))))
                                      xx)
                                     (fref qq (3) ((1 7))))
                                    xx)
                                   (fref qq (4) ((1 7))))
                                  xx)
                                 (fref qq (5) ((1 7))))
                                xx)
                               (fref qq (6) ((1 7))))
                              xx)
                             (fref qq (7) ((1 7)))))
                    (setf result (/ sump sumq))
                    (cond
                     ((= jint 2)
                      (setf result
                              (/ (- result (fref pp (1) ((1 8)))) (fsqrt x))))
                     (t
                      (cond
                       ((<= x (+ xmax (- one5))) (setf a (exp x)) (setf b one))
                       (t (setf a (exp (- x forty))) (setf b exp40)))
                      (setf result
                              (*
                               (/ (- (* result a) (* (fref pp (1) ((1 8))) a))
                                  (fsqrt x))
                               b))))))))
      (go end_label)
     end_label
      (return (values arg result jint)))))

(defun besi0 (x)
  (declare (type double-float x))
  (prog ((jint 0) (besi0 0.0d0) (result 0.0d0))
    (declare (type double-float result besi0) (type integer4 jint))
    (declare
     (ftype (function (double-float double-float integer4) (values &rest t))
      calci0))
    (setf jint 1)
    (multiple-value-bind
        (var-0 var-1 var-2)
        (calci0 x result jint)
      (declare (ignore))
      (when var-0 (setf x var-0))
      (when var-1 (setf result var-1))
      (when var-2 (setf jint var-2)))
    (setf besi0 result)
    (go end_label)
   end_label
    (return (values besi0 x))))

;; Bessel function of the second kind of order 0.  This is related to
;; J[0] via
;;
;; I[0](z) = J[0](z*exp(%pi*%i/2))
;;
;; and
;;
;;        INF
;;        ====         2 k
;;        \           z
;;         >    ----------------
;;        /         2 k   2 
;;        ====     2    k!
;;        k = 0

(defun i[0]-bessel (x)
   (besi0 (float x 1d0)))

(defun $i0 ($x)
  (cond ((numberp $x) (i[0]-bessel (float $x)))
	(t (list '($i0 simp) $x))))

#+nil
(defun i[1]-bessel (x) 
       (declare (flonum x)) 
       ((lambda (xa y z ri1) 
		(declare (flonum z y xa ri1)) 
		(cond ((> 4.0 xa)
		       (setq y (*$ 0.25 xa) z (*$ y y))
		       (setq ri1
			     (//$ (*$ y
				      (+$ -569.784
					   (*$ z
					       (+$ -947.9975
						    (*$ z
							(+$ -405.4861
							     (*$ -53.66977 z)))))))
				  (+$ -284.892
				       (*$ z
					   (+$ 95.78535 (*$ z (+$ -14.45951 z))))))))
		      (t (setq z (1+$ (//$ -4.0 xa)))
			 (setq ri1
			       (*$ (exp xa)
				   (//$ (sqrt xa))
				   (//$ (+$ 0.9980789
					     (*$ z
						 (+$ -0.3663376
						      (*$ z (+$ 2.818702 z))))))
				   (+$ 0.3568149
					(*$ z
					    (+$ -0.08379694
						 (*$ z
						     (+$ 0.9826178
							  (*$ z
							      (+$ 0.4946486
								   (*$ 0.0251859
								       z))))))))))))
		(and (< x 0.0) (setq ri1 (-$ ri1)))
		ri1)
	(abs x) 0.0 0.0 0.0))

;; TOMS 715 routine translated from Fortran via f2cl
(let ((one 1.0d0)
      (one5 15.0d0)
      (exp40 2.3538526683702d+17)
      (forty 40.0d0)
      (rec15 0.06666666666666667d0)
      (two25 225.0d0)
      (half 0.5d0)
      (zero 0.0d0)
      (xsmall 5.55d-17)
      (xinf 1.79d+308)
      (xmax 713.987d0)
      (p
       (make-array 15
                   :element-type
                   'double-float
                   :initial-contents
                   '(-1.970529180253514d-19 -6.52455155831519d-16
                     -1.1928788903603238d-12 -1.483190493599465d-9
                     -1.3466829827635154d-6 -9.174644328781751d-4
                     -0.4720709082731016d0 -182.25946631657314d0
                     -51894.09198230802d0 -1.0588550724769348d+7
                     -1.4828267606612366d+9 -1.3357437682275491d+11
                     -6.987677964801009d+12 -1.7732037840791595d+14
                     -1.4577180278143467d+15)))
      (q
       (make-array 5
                   :element-type
                   'double-float
                   :initial-contents
                   '(-4007.6864679904193d0 7481058.035665507d0
                     -8.005951899861977d+9 4.854471425827362d+12
                     -1.3218168307321443d+15)))
      (pp
       (make-array 8
                   :element-type
                   'double-float
                   :initial-contents
                   '(-0.0604371590561376d0 0.45748122901933463d0
                     -0.4284376690330481d0 0.0973560001508866d0
                     -0.0032457723974465566d0 -3.6395264712121794d-4
                     1.6258661867440838d-5 -3.634757840460822d-7)))
      (qq
       (make-array 6
                   :element-type
                   'double-float
                   :initial-contents
                   '(-3.8806586721556595d0 3.2593714889036995d0
                     -0.8501747646321793d0 0.07421201081318653d0
                     -0.0022835624489492513d0 3.7510433111922825d-5)))
      (pbar 0.3984375d0))
  (declare (type (array double-float (6)) qq)
           (type (array double-float (8)) pp)
           (type (array double-float (5)) q)
           (type (array double-float (15)) p)
           (type double-float pbar xmax xinf xsmall zero half two25 rec15 forty
            exp40 one5 one))
  (defun calci1 (arg result jint)
    (declare (type integer4 jint) (type double-float result arg))
    (prog ((a 0.0d0) (b 0.0d0) (sump 0.0d0) (sumq 0.0d0) (x 0.0d0) (xx 0.0d0)
           (j 0))
      (declare (type integer4 j) (type double-float xx x sumq sump b a))
      (declare (ftype (function (double-float) (values single-float)) abs))
      (declare (ftype (function (double-float) (values integer4)) exp))
      (declare (ftype (function (double-float) (values single-float)) sqrt))
      (setf x (coerce (abs arg) 'double-float))
      (cond ((< x xsmall) (setf result (* half x)))
            ((< x one5) (setf xx (* x x)) (setf sump (fref p (1) ((1 15))))
             (fdo (j 2 (+ j 1))
                  ((> j 15) nil)
                  (tagbody
                    (setf sump (+ (* sump xx) (fref p (j) ((1 15)))))
                   label50))
             (setf xx (- xx two25))
             (setf sumq
                     (+
                      (*
                       (+
                        (*
                         (+
                          (*
                           (+ (* (+ xx (fref q (1) ((1 5)))) xx)
                              (fref q (2) ((1 5))))
                           xx)
                          (fref q (3) ((1 5))))
                         xx)
                        (fref q (4) ((1 5))))
                       xx)
                      (fref q (5) ((1 5)))))
             (setf result (* (/ sump sumq) x))
             (if (= jint 2) (setf result (* result (exp (- x))))))
            ((and (= jint 1) (> x xmax)) (setf result xinf))
            (t (setf xx (- (/ one x) rec15))
             (setf sump
                     (+
                      (*
                       (+
                        (*
                         (+
                          (*
                           (+
                            (*
                             (+
                              (*
                               (+
                                (*
                                 (+ (* (fref pp (1) ((1 8))) xx)
                                    (fref pp (2) ((1 8))))
                                 xx)
                                (fref pp (3) ((1 8))))
                               xx)
                              (fref pp (4) ((1 8))))
                             xx)
                            (fref pp (5) ((1 8))))
                           xx)
                          (fref pp (6) ((1 8))))
                         xx)
                        (fref pp (7) ((1 8))))
                       xx)
                      (fref pp (8) ((1 8)))))
             (setf sumq
                     (+
                      (*
                       (+
                        (*
                         (+
                          (*
                           (+
                            (*
                             (+ (* (+ xx (fref qq (1) ((1 6)))) xx)
                                (fref qq (2) ((1 6))))
                             xx)
                            (fref qq (3) ((1 6))))
                           xx)
                          (fref qq (4) ((1 6))))
                         xx)
                        (fref qq (5) ((1 6))))
                       xx)
                      (fref qq (6) ((1 6)))))
             (setf result (/ sump sumq))
             (cond ((/= jint 1) (setf result (/ (+ result pbar) (fsqrt x))))
                   (t
                    (cond
                     ((> x (+ xmax (- one5))) (setf a (exp (- x forty)))
                      (setf b exp40))
                     (t (setf a (exp x)) (setf b one)))
                    (setf result
                            (* (/ (+ (* result a) (* pbar a)) (fsqrt x))
                               b))))))
      (if (< arg zero) (setf result (- result)))
      (go end_label)
     end_label
      (return (values arg result jint)))))

(defun besi1 (x)
  (declare (type double-float x))
  (prog ((jint 0) (besi1 0.0d0) (result 0.0d0))
    (declare (type double-float result besi1) (type integer4 jint))
    (declare
     (ftype (function (double-float double-float integer4) (values &rest t))
      calci1))
    (setf jint 1)
    (multiple-value-bind
        (var-0 var-1 var-2)
        (calci1 x result jint)
      (declare (ignore))
      (when var-0 (setf x var-0))
      (when var-1 (setf result var-1))
      (when var-2 (setf jint var-2)))
    (setf besi1 result)
    (go end_label)
   end_label
    (return (values besi1 x))))

;; Bessel function of the second kind of order 1.  This is related to
;; J[1] via
;;
;; I[1](z) = exp(-%pi*%I/2)*J[0](z*exp(%pi*%i/2))
;;
;; and
;;
;;       INF
;;       ====         2 k
;;       \           z
;;        >    ----------------
;;       /      2 k
;;       ====  2    k! (k + 1)!
;;       k = 0

(defun i[1]-bessel (x)
  (besi1 (float x 1d0)))

(defun $i1 ($x)
  (cond ((numberp $x) (i[1]-bessel (float $x)))
	(t (list '($i1 simp) $x))))


(defun i[n]-bessel (x n) 
       (declare (fixnum n) (flonum x)) 
       (prog (a a0 a1 an b b0 b1 fi fn q0 q1) 
	     (declare (flonum q1 q0 fn fi b1 b0 b an a1 a0 a))
	     (setq n (abs n))
	     (*rearray 'i-bessel-array)
	     (narray i-bessel-array flonum (f1+ n))
	     (and (= n 0.) (go $l9))
	     (setq fn (float (f+ n n)))
	     (setq q1 (//$ x fn))
	     (cond ((> (abs x) 3.0e-4)
		    (setq a0 1.0 a1 0.0 b0 0.0 b1 1.0 fi fn)
		    (do nil
			(nil)
			(setq fi (+$ 2.0 fi) 
			      an (//$ fi (abs x)) 
			      a (+$ a0 (*$ a1 an)))
			(setq b (+$ b0 (*$ an b1)))
			(setq a0 a1 b0 b1 a1 a b1 b q0 q1 q1 (//$ a b))
			(or (> (abs (//$ (-$ q1 q0) q1)) 1.0e-6) (return nil)))
		    (and (< x 0.0) (setq q1 (-$ q1)))))
	     (do ((i n (f1- i)))
		 ((> 0. i))
		 (declare (fixnum i))
		 (setq q1 (//$ x (+$ fn (*$ q1 x))))
		 (setf (arraycall 'flonum (nsymbol-array 'i-bessel-array) i) q1)
		 (setq fn (+$ -2.0 fn)))
	$l9  (setq fi (i[0]-bessel x))
	     (setf (arraycall 'flonum (nsymbol-array 'i-bessel-array) 0.) fi)
	     (and (or (= x 0.0) (= n 0.)) (return (arraycall 'flonum (nsymbol-array 'i-bessel-array) n)))
	     (do ((i 1. (f1+ i)))
		 ((> i n))
		 (declare (fixnum i))
		 (cond ((or (> (abs (arraycall 'flonum (nsymbol-array 'i-bessel-array) i)) 1.0)
			    (> (abs fi) (//$ #-cl 1.0e-38 #+cl *small-flonum*  (abs (arraycall 'flonum (nsymbol-array 'i-bessel-array) i)))))
			(setq fi (*$ fi (arraycall 'flonum (nsymbol-array 'i-bessel-array) i))))
		       (t (setq fi 0.0)))
		 (setf (arraycall 'flonum (nsymbol-array 'i-bessel-array) i) fi))
	     (return (arraycall 'flonum (nsymbol-array 'i-bessel-array) n)))) 

(defun $in ($x $n)
  (cond ((and (numberp $x) (integerp $n))
	 (i[n]-bessel (float $x) $n)
	 (narray $iarray $float (abs $n))
;	 (apply '$array (list '$iarray '$float (abs $n)))
	 (fillarray (nsymbol-array '$iarray) (nsymbol-array 'i-bessel-array))
	 (arraycall 'flonum (nsymbol-array '$iarray) (abs $n)))
	(t (list '($in simp) $x $n))))

;; I think this is exp(-x)*I[0](x), based on some simple numerical
;; evaluations.
(defun g[0]-bessel (x) 
       (declare (flonum x))
       ((lambda (xa y z) 
		(declare (flonum z y xa))
		(cond ((> 4.0 xa)
		       (setq z (*$ 0.0625 (*$ xa xa)))
		       (//$ (+$ -162.6391
				 (*$ z
				     (+$ -585.5938
					  (*$ z (+$ -402.5407 (*$ -75.72017 z))))))
			    (+$ -162.6391
				 (*$ z (+$ 64.96299 (*$ z (+$ -11.84469 z)))))
			    (exp xa)))
		      (t (setq y (//$ 4.0 xa))
			 (setq z (-$ (1-$ y)))
			 (*$ (sqrt y)
			     (//$ (+$ 2.67093
				       (*$ z (+$ 2.470948 (*$ z (+$ 6.271432 z))))))
			     (+$ 0.5528884
				  (*$ z
				      (+$ 0.4861227
					   (*$ z
					       (+$ 1.281496 (*$ 0.1555914 z))))))))))
	(abs x) 0.0 0.0)) 

(defun $g0 ($x)
  (cond ((numberp $x) (g[0]-bessel (float $x)))
	(t (list '($g0 simp) $x))))

;; I think this is exp(-x)*I[1](x), based on some numerical
;; calculations.
(defun g[1]-bessel (x) 
       (declare (flonum x))
       ((lambda (xa y z ri1) 
		(declare (flonum z y xa ri1))
		(cond ((> 4.0 xa)
		       (setq y (*$ 0.25 xa) z (*$ y y))
		       (setq ri1
			     (//$ (*$ y
				      (+$ -569.784
					   (*$ z
					       (+$ -947.9975
						    (*$ z
							(+$ -405.4861
							     (*$ -53.66977 z)))))))
				  (+$ -284.892
				       (*$ z (+$ 95.78535 (*$ z (+$ -14.45951 z)))))
				  (exp xa))))
		      (t (setq z (1+$ (//$ -4.0 xa)))
			 (setq ri1
			       (//$ (+$ 0.3568149
					 (*$ z
					     (+$ -0.08379694
						  (*$ z
						      (+$ 0.9826178
							   (*$ z
							       (+$ 0.4946486
								    (*$ 0.0251859
									z))))))))
				    (+$ 0.9980789
					 (*$ z
					     (+$ -0.3663376 (*$ z (+$ 2.818702 z)))))
				    (sqrt xa)))))
		(and (< x 0.0) (setq ri1 (-$ ri1)))
		ri1)
	(abs x) 0.0 0.0 0.0)) 

(defun $g1 ($x)
  (cond ((numberp $x) (g[1]-bessel (float $x)))
	(t (list '($g1 simp) $x))))


(declare-top (fixnum i n) (flonum x q1 q0 fn fi b1 b0 b an a1 a0 a)) 

(defun g[n]-bessel (x n) 
       (prog (a a0 a1 an b b0 b1 fi fn q0 q1) 
	     (setq n (abs n))
	     (*rearray 'g-bessel-array)
	     (narray g-bessel-array flonum (f1+ n))
	     (and (= n 0.) (go $l9))
	     (setq fn (float (f+ n n)) q1 (//$ x fn))
	     (cond ((> (abs x) 3.0e-4)
		    (setq a0 1.0 a1 0.0 b0 0.0 b1 1.0 fi fn)
		    (do nil
			(nil)
			(setq fi (+$ 2.0 fi) 
			      an (//$ fi (abs x)) 
			      a (+$ a0 (*$ a1 an)))
			(setq b (+$ b0 (*$ an b1)))
			(setq a0 a1 b0 b1 a1 a b1 b q0 q1 q1 (//$ a b))
			(or (> (abs (//$ (-$ q1 q0) q1)) 1.0e-6) (return nil)))
		    (and (< x 0.0) (setq q1 (-$ q1)))))
	     (do ((i n (f1- i)))
		 ((> 0. i))
		 (setq q1 (//$ x (+$ fn (*$ q1 x))))
		 (setf (arraycall 'flonum (nsymbol-array 'g-bessel-array) i) q1)
		 (setq fn (+$ -2.0 fn)))
	$l9  (setq fi (g[0]-bessel x))
	     (setf (arraycall 'flonum (nsymbol-array 'g-bessel-array) 0.) fi)
	     (and (or (= x 0.0) (= n 0.)) (return (arraycall 'flonum (nsymbol-array 'g-bessel-array) n)))
	     (do ((i 1. (f1+ i)))
		 ((> i n))
		 (cond ((or (> (abs (arraycall 'flonum (nsymbol-array 'g-bessel-array) i)) 1.0)
			    (> (abs fi) (//$ #-cl 1.0e-38 #+cl *small-flonum*
					     (abs (arraycall 'flonum (nsymbol-array 'g-bessel-array) i)))))
			(setq fi (*$ fi (arraycall 'flonum (nsymbol-array 'g-bessel-array) i))))
		       (t (setq fi 0.0)))
		 (setf (arraycall 'flonum (nsymbol-array 'g-bessel-array) i) fi))
	     (return (arraycall 'flonum (nsymbol-array 'g-bessel-array) n)))) 

(defun $gn ($x $n)
  (cond ((and (numberp $x) (integerp $n))
	 (g[n]-bessel (float $x) $n)
	 (narray $garray $float (abs $n))
	 ;(apply '$array (list '$garray '$float (abs $n)))
	 (fillarray (nsymbol-array '$garray) (nsymbol-array 'g-bessel-array))
	 (arraycall 'flonum (nsymbol-array '$garray) (abs $n)))
	(t (list '(gn simp) $x $n))))

(declare-top(flonum rz cz a y $t t0 t1 d r1 rp sqrp rnpa r2 ta rn rl rnp rr cr rs cs rlam
		 clam qlam s phi rsum csum)
	 (fixnum n k1 k m mpo ln l ind)
	 (notype ($bessel notype notype) (bessel flonum flonum flonum))
	 (array* (flonum rj-bessel-array 1. cj-bessel-array 1.)
		 (notype $besselarray 1.))
	 (*fexpr $array))

(defun bessel (rz cz a) 
  (prog (n y $t t0 t1 k1 d r1 rp sqrp rnpa r2 m ta rn rl mpo ln rnp rr cr rs cs
	 rlam clam qlam ind s phi rsum csum l) 
	(setq n (fix a) a (-$ a (float n)) ln (f1+ n) y (abs cz))
	(narray rj-bessel-array flonum ln)
	(narray cj-bessel-array flonum ln)
	(go l13)
l9	(cond
	 ((not (< 10.0 $t))
	  (setq $t (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ $t 5.794e-5)
						       -1.76148e-3)
						       $t)
						   0.0208641)
					      $t)
					  -0.129013)
				     $t)
				 0.85777)
			    $t)
			1.0125)))
	 (t (setq t0  (+$ -0.5 (log $t))
		  t1 (//$  (-$ 0.5 (log t0)) (1+$ t0))
		  $t (//$ $t (+$ (*$ t1 t0) t0)))))
	(cond ((> k1 0.) (setq r2 (*$ $t r2)) (go l25))
	      (t (setq r1 (*$ $t (float n))) (go l22)))
   l13  (cond ((and (= rz 0.0) (= y 0.0))
	       (or (= a 0.0) (MAXIMA-ERROR '|&bessel function evaluated at branch point|))
	       (setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.) 1.0)
	       (return (cond ((= n 0.) 1.0) (t 0.0)))))
	(setq d 17.5045 r1 0.0)
	(cond ((> n 0.) (setq $t (//$ d (float (f* 2 n))) k1 0.) (go l9)))
   l22	(setq rp (+$ (*f y y) (*f rz rz)) sqrp (sqrt rp) r2 (*$ sqrp 1.3591))
	(cond ((> d y)
	       (setq $t (//$ (*$ (-$ d y) 0.356) sqrp) k1 1.)
	       (go l9)))
l25	(cond ((> r2 r1) (setq r1 r2)))
	(setq m (f1+ (fix r1)) ta (*$ a 2.0) rn 1.0 rl 1.0)
	(setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.) 1.0)
	(setq mpo (f1+ m))
	(do ((k 2. (f+ k 1.)))
	    ((> k mpo))
	    (setq rnp (1+$ rn) rl (//$ (*$ (+$ ta rn) rl) rnp))
	    (cond ((not (< ln k)) (setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) (f1- k)) rl)))
	    (setq rn rnp))
	(setq rr 0.0 cr 0.0 rs 0.0 cs 0.0)
	(do
	 ((k 1. (f1+ k)))
	 ((> k m))
	 (setq l (f- mpo k) rn (1-$ rnp) rnpa (+$ ta (*$ rn 2.0)))
	 (setq rlam (-$ (+$ (*f y cr) rnpa) (*f rz rr)) clam (+$ (*f y rr) (*f rz cr)))
	 (setq qlam (+$ (*f rlam rlam) (*f clam clam)))
	 (cond ((= qlam 0.0) (setq qlam (*f rnpa 1.0e-17))))
	 (setq rr (//$ (-$ (*f rz rnpa) (*f rr rp)) qlam)
	       cr (//$ (+$ (*f y rnpa) (*f rp cr)) qlam))
	 (cond ((> l ln) (setq rl (*$ rnp rl (//$ (+$ ta rn)))))
	       (t (cond ((> ln l)
			 (setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) l) rr)
			 (setf (arraycall 'flonum (nsymbol-array 'cj-bessel-array) l) cr)))
		  (setq rl (arraycall 'flonum (nsymbol-array 'rj-bessel-array) (f1- l)))))
	 (setq qlam (*f rnpa rl) rlam 0.0 clam 0.0 ind (fixnum-remainder l 4.))
	 (cond ((= ind 0.) (setq rlam qlam))
	       ((= ind 1.) (setq clam (setq qlam (-$ qlam))))
	       ((= ind 2.) (setq rlam (setq qlam (-$ qlam))))
	       (t (setq clam qlam)))
	 (setq s (-$ (*$ (+$ rs rlam) rr) (*$ (+$ cs clam) cr)))
	 (setq cs (+$ (*$ (+$ rs rlam) cr) (*$ rr (+$ cs clam))))
	 (setq rs s rnp rn))
	(setq 
	 qlam
	 (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ a -13.812104)
							     a)
							 50.569126)
						     a)
						 122.48542)
					     a)
					 -968.33451)
				     a)
				 -203.72512)
			     a)
			 5452.1006)
		     a)
		 4630.389)
	     (//$
	      (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ a
								      0.12949573)
								  1.61000405)
							      a)
							  12.473999)
						      a)
						  78.03884)
					      a)
					  339.71559)
				      a)
				  1228.9483)
			      a)
			  2779.373)
		      a)
		  4630.3895))
	     (exp y)
	     (expt (*$ 0.5 sqrp) a)))
	(cond ((> 1.0e-36 (abs rz)) (setq phi (*$ a 1.57079632)))
	      (t (setq phi (atan y rz) phi (-$ (*$ phi a) (*$ rz)))))
	(setq rsum (*$ (cos phi) qlam) csum (*$ (sin phi) qlam))
	(setq rs (1+$ rs) s (+$ (*f rs rs) (*f cs cs)))
	(setq rr (//$ (+$ (*f rsum rs) (*f csum cs)) s))
	(setq cr (//$ (-$ (*f rs csum) (*f rsum cs)) s))
	(setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.) rr)
	(setf (arraycall 'flonum (nsymbol-array 'cj-bessel-array) 0.) cr)
	(cond ((> n 0.)
	       (do ((k 1. (f+ k 1.)))
		   ((> k n))
		   (setq rs (arraycall 'flonum (nsymbol-array 'rj-bessel-array) k) cs (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k))
		   (setq s (-$ (*f rs rr) (*f cs cr)))
		   (setq cr (+$ (*f rs cr) (*f rr cs)))
		   (setq rr s)
		   (setf (arraycall 'flonum (nsymbol-array 'rj-bessel-array) k) rr)
		   (setf (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k) cr))))
	(cond ((> 0.0 cz) 
	       (do ((k 0. (f1+ k)))
		   ((> k n))
		   (setf (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k) (-$ (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k))))))
	(return (list '(mlist simp) (arraycall 'flonum (nsymbol-array 'rj-bessel-array) n) (arraycall 'flonum (nsymbol-array 'cj-bessel-array) n)))))

(defun $bessel ($arg $order)
  ((lambda (a)
	   (cond ((not (and (numberp $order)
			    (not (< (setq a (float $order)) 0.0))
			    (numberp ($realpart $arg))
			    (numberp ($imagpart $arg))))
		  (list '($bessel simp) $arg $order))
		 (t (bessel (float ($realpart $arg))
			    (float ($imagpart $arg))
			    a)
		    (narray $besselarray $complete (fix a))
		    ;(apply '$array (list '$besselarray '$complete (fix a)))
		    (do ((k 0. (f1+ k)) (n (fix a)))
			((> k n) (arraycall 'flonum (nsymbol-array '$besselarray) n))
			(setf (arraycall 'flonum (nsymbol-array '$besselarray) k)
			       (simplify (list '(mplus)
					       (simplify (list '(mtimes)
							       '$%i
							       (arraycall 'flonum (nsymbol-array 'cj-bessel-array) k)))
					       (arraycall 'flonum (nsymbol-array 'rj-bessel-array) k))))))))
   0.0)) 

(declare-top(flonum rz y rs cs third sin60 term sum fi cossum sinsum sign (airy flonum)))

;here is Ai'
;airy1(z):=if z = 0. then -1/(gamma(1/3.)*3.^(1/3.))
;else block([zz],z:-z,zz:2./3.*z^(3./2.),bessel(zz,4./3.),
;j:realpart(2/(3.*zz)*besselarray[0]-besselarray[1]),
;-1/3.*z*(j-realpart(bessel(zz,2./3.))));

(defun airy (rz)
       ((lambda (y rs cs third sin60)
		(setq y (sqrt (abs rz)) rz (*$ 2.0 third y rz))
		       (cond ((= rz 0.0) 0.35502805) ;;;avoids branch point probs
			     ((> rz 3.3333333)
			      (do ((fi 1.0 (1+$ fi))
				   (term 1.0)
				   (sum 1.0))
				  ((> fi 7.0)
				   (setq sum (-$ sum (*$ 0.5 term)))
				   (//$ (*$ (exp (-$ rz)) sum)
					(*$ 2.0 (sqrt (*$ y (atan 0 -1))))))
				  (setq term (//$ (*$ term -0.5
						      (-$ fi 0.83333333)
						      (-$ fi 0.166666666))
						  (*$ fi rz))
					sum (+$ sum term))))
			     ((< rz -7.5)
			      (setq rz (-$ rz))
			      (do ((fi 1.0 (1+$ fi))
				   (term 1.0)
				   (cossum 0.0)
				   (sinsum 1.0)
				   (sign -1.0)
				   (even nil (not even)))
				  ((> fi 6.0)
				   (setq rz (+$ rz (atan 1. 1.)))
				   (//$ (+$ (*$ (sin rz) sinsum)
					    (*$ (cos rz) cossum))
					(sqrt (*$ y (atan 0 -1.)))))
				  (setq term (//$ (*$ term 0.5
						      (-$ fi 0.83333333)
						      (-$ fi 0.166666666))
						  (*$ fi rz)))
				  (cond (even (setq sinsum (+$ sinsum (*$ sign term))
						    sign (-$ sign)))
					(t (setq cossum (+$ cossum (*$ sign term)))))))
			     ((< rz 0.0)
			      (setq rz (-$ rz))
			      (bessel rz 0.0 (*$ 5.0 third))
			      (setq rs (-$ (//$ (*$ 4.0 (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.))
						(*$ 3.0 rz)) (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 1.)))
			      (bessel rz 0.0 third)
			      (*$ y third (+$ rs (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.))))
			     (t (bessel 0.0 rz (*$ 5.0 third))
				(setq rs (-$ (//$ (*$ 4.0 (arraycall 'flonum (nsymbol-array 'cj-bessel-array) 0.))
						  (*$ 3.0 rz))
					     (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 1.))
				      cs (-$ (//$ (*$ -4.0 (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.))
						  (*$ 3.0 rz))
					     (arraycall 'flonum (nsymbol-array 'cj-bessel-array) 1.))
				      rs (-$ (*$ sin60 rs) (*$ 0.5 cs)))
				(bessel 0.0 rz third)
				(setq cs (+$ (*$ sin60 (arraycall 'flonum (nsymbol-array 'rj-bessel-array) 0.))
					     (*$ 0.5 (arraycall 'flonum (nsymbol-array 'cj-bessel-array) 0.))))
				(*$ y third (-$ rs cs)))))
	0.0 0.0 0.0 (//$ 3.0) (sqrt 0.75)))

(defun $airy ($arg)
       (cond ((numberp $arg) (airy (float $arg)))
	     (t (list '($airy simp) $arg))))

(declare-top (flonum im re ys xs y x c t2 t1 s2 s1 s r2 r1 lamb h2 h)
	 (fixnum np1 n nu capn)
	 (notype (z-function flonum flonum))) 

(defun z-function (x y) 
       ((lambda (xs ys capn nu np1 h h2 lamb r1 r2 s s1 s2 t1 t2 c bool re im) 
		(setq xs (cond ((> 0.0 x) -1.0) (t 1.0)))
		(setq ys (cond ((> 0.0 y) -1.0) (t 1.0)))
		(setq x (abs x) y (abs y))
		(cond ((and (> 4.29 y) (> 5.33 x))
		       (setq s (*$ (1+$ (*$ -0.23310023 y))
				   (sqrt (1+$ (*$ -0.035198873 x x)))))
		       (setq h (*$ 1.6 s) h2 (*$ 2.0 h) capn (f+ 6. (fix (*$ 23.0 s))))
		       (setq nu (f+ 9. (fix (*$ 21.0 s)))))
		      (t (setq h 0.0) (setq capn 0.) (setq nu 8.)))
		(and (> h 0.0) (setq lamb (^$ h2 capn)))
		(setq bool (or (= h 0.0) (= lamb 0.0)))
		(do ((n nu (f1- n)))
		    ((> 0. n))
		    (setq np1 (f1+ n))
		    (setq t1 (+$ h (*$ (float np1) r1) y))
		    (setq t2 (-$ x (*$ (float np1) r2)))
		    (setq c (//$ 0.5 (+$ (*$ t1 t1) (*$ t2 t2))))
		    (setq r1 (*$ c t1) r2 (*$ c t2))
		    (cond ((and (> h 0.0) (not (< capn n)))
			   (setq t1 (+$ s1 lamb) s1 (-$ (*$ r1 t1) (*$ r2 s2)))
			   (setq s2 (+$ (*$ r1 s2) (*$ r2 t1)) lamb (//$ lamb h2)))))
		(setq im (cond ((= y 0.0) (*$ 1.77245384 (exp (-$ (*$ x x)))))
			       (t (*$ 2.0 (cond (bool r1) (t s1))))))
		(setq re (*$ -2.0 (cond (bool r2) (t s2))))
		(cond ((> ys 0.0) (setq re (*$ re xs)))
		      (t (setq r1 (*$ 3.5449077 (exp (-$ (*$ y y) (*$ x x)))))
			 (setq r2 (*$ 2.0 x y))
			 (setq re (*$ (-$ re (*$ r1 (sin r2))) xs))
			 (setq im (-$ (*$ r1 (cos r2)) im))))
		(list '(mlist simp) re im))
	(cond ((> 0.0 x) -1.0) (t 1.0))
	(cond ((> 0.0 x) -1.0) (t 1.0))
	0. 0. 0. 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 nil 0.0 0.0)) 

(defun $nzeta ($z) 
  (prog ($x $y $w) 
	(cond ((and (numberp (setq $x ($realpart $z)))
		    (numberp (setq $y ($imagpart $z))))
	       (setq $w (z-function (float $x) (float $y)))
	       (return (simplify (list '(mplus)
				       (simplify (list '(mtimes)
						       (meval1 '$%i)
						       (caddr $w)))
				       (cadr $w)))))
	      (t (return (list '($nzeta simp) $z))))))


(defun $nzetar ($z)
  (prog ($x $y $w) 
	(cond ((and (numberp (setq $x ($realpart $z)))
		    (numberp (setq $y ($imagpart $z))))
	       (setq $w (z-function (float $x) (float $y)))
	       (return (cadr $w)))
	      (t (return (list '($nzetar simp) $z))))))


(defun $nzetai ($z)
  (prog ($x $y $w) 
	(cond ((and (numberp (setq $x ($realpart $z)))
		    (numberp (setq $y ($imagpart $z))))
	       (setq $w (z-function (float $x) (float $y)))
	       (return (caddr $w)))
	      (t (return (list '($nzetai simp) $z))))))


(declare-top (fixnum i) (flonum (gauss) te)) 

(defun gauss nil
  (do ((i 0. (f1+ i))
       ;;are these random numbers supposed to be negative too?
       (te 0.0 (+$ te (*$ (float (random #+cl most-positive-fixnum
					 #-cl #. (^ 2 30))) 1.45519152e-11))))
      ((= i 12.) te)))


(defun $gauss ($mean $sd)
  (cond ((and (numberp $mean) (numberp $sd))
	 (+$ (float $mean) (*$ (float $sd) (gauss))))
	(t (list '($gauss simp) $mean $sd))))


(declare-top (flonum x w y (expint flonum)))

;; I think this is the function E1(x).  At least some simple numerical
;; tests show that this expint matches the function eone from TOMS
;; 715.  Unfortunately, the source for TOMS 715 doesn't actually
;; define what eone is.
(defun expint (x)
       (cond ((< x 1.0)
	      (-$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ (+$ (*$ 1.07857e-3 x)
	       -9.76004e-3) x) 0.05519968) x) -0.24991055) x) 0.99999193)
	       x) 0.57721565 (log x)))
	     (t ((lambda (w y)
			 (setq y (+$ (*$ (+$ (*$ (+$ (*$ (+$ x 8.57332873)
				  x) 18.059017) x) 8.63476085) x) 0.26777373)
			       w (+$ (*$ (+$ (*$ (+$ (*$ (+$ x 9.5733224)
				  x) 25.6329562) x) 21.099653) x) 3.95849696))
			 (*$ (//$ (exp (-$ x)) x) (//$ y w)))
		 0.0  0.0))))

(defun $expint ($x)
       (cond ((numberp $x) (expint (float $x)))
	     (t (list '($expint simp) $x))))
