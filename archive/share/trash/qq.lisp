;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

(macsyma-module qq)

;; (load-macsyma-macros Numerm)

;;; 10:55 pm Sept 25, 1981 - LPH & GJC
;;; added $numer and $float set to T so that quanc8(x,x,0,1/2); works. this
;;; is done inside the prog for $quanc8 so that they are put back when done.
;;; 3:30 pm Feb 12, 1982 - JPG, LPH, & GJC
;;; removed the $numer:$float:true and replaced with mto-float which is
;;; defined in maxsrc;numer > .
;;; 3:45 am April 11, 1982 - LPH
;;; fixed an error in estimating TOLERR due to neglect of sub-interval size
;;; when abserr check is made.

(defmvar $quanc8_flag 0.0
  "integer part-how many regions failed to converge. fractional part-
  how much of the interval was left when it failed")
(defmvar $quanc8_errest 0.0
  "an estimated error based on the difference between one- and two-panel
  calculation of an interval, summed over the whole region")
(defmvar $quanc8_abserr 1.0e-8
  "absolute error tolerance for reasonable single-precision answers")
(defmvar $quanc8_relerr 1.0e-4
  "relative error condition for reasonable run-times")	

;; (DEFUN ($QUANC8 TRANSLATED-MMACRO) (F A B &OPTIONAL (C NIL C-P))
;;   (IF (NOT C-P)
;;       `((CALL-QUANC8) ,F ,A ,B)
;;       `((CALL-QUANC8) ((LAMBDA) ((MLIST) ,A) ,F) ,B ,C)))

(defmspec $quanc8 (form)
  (if (cdddr (setq form (cdr form))) 
      (apply #'call-quanc8
	     (meval `((lambda) ((mlist) ,(cadr form)) ,(car form)))
	     (mapcar #'meval (cddr form)))
      (apply #'call-quanc8 (mapcar #'meval form))))

(def%tr $quanc8 (form)
  `($float
    ,@(cdr (tr-lisp-function-call
	    (if (cdddr (setq form (cdr form)))
		`((call-quanc8)
		  ((lambda) ((mlist) ,(cadr form)) ,(car form)) ,@(cddr form))
		`((call-quanc8) ,@form)) nil))))

(defvar quanc8-free-list ()
  "For efficient calls to quanc8 keep arrays we need on a free list.")

(defvar quanc8-|^]| ())
(defun quanc8-|^]| () (setq quanc8-|^]| t))

(defun call-quanc8 (fun a b)
  (bind-tramp1$
   fun fun
   (let ((vals (if quanc8-free-list
		   (pop quanc8-free-list)
		   (list (*array nil 'flonum 17.)
			 (*array nil 'flonum 17.)
			 (*array nil 'flonum 9. 31.)
			 (*array nil 'flonum 9. 31.)
			 (*array nil 'flonum 32.))))
	 (user-timesofar (cons #'quanc8-|^]| user-timesofar))) 
     (prog1
      (quanc8 fun
	      (mto-float a)
	      (mto-float b)
	      (car vals)
	      (car (cdr vals))
	      (car (cddr vals))
	      (car (cdddr vals))
	      (car (cddddr vals)))
      (push vals quanc8-free-list)))))

(defun quanc8 (fun a b x-arr f-arr xsave-arr fsave-arr qright-arr)
  (declare (type (simple-array cl:float)
		 x-arr f-arr xsave-arr fsave-arr qright-arr))
  ;; local macros for typing convenience.
  (macrolet ((x (j) `(arraycall flonum x-arr ,j))
	     (f (j) `(arraycall flonum f-arr ,j))
	     (xsave (j k) `(arraycall flonum xsave-arr ,j ,k))
	     (fsave (j k) `(arraycall flonum fsave-arr ,j ,k))
	     (qright (j) `(arraycall flonum qright-arr ,j))) 
    ;; Rudimentary (non-ansi GCL compatible) error handling.
    (let (errset)
      (or (car (errset
    (prog ((levmin 1.) (levmax 30.) (levout 6.) (nomax 5000.) (nofin 0)
	 (w0 (//$ 3956.0 14175.0)) (w1 (//$ 23552.0 14175.0))
	 (w2 (//$ -3712.0 14175.0)) (w3 (//$ 41984.0 14175.0))
	 (w4 (//$ -18160.0 14175.0))
	 (result 0.0) (cor11 0.0) (area 0.0)
	 (nofun 0.) (lev 0.) (nim 1.) (qprev 0.0)
	 (stone (//$ (-$ b a) 16.0))
	 (i 0.)
	 (step 0.0) (qleft 0.0) (qnow 0.0) (qdiff 0.0)
	 (esterr 0.0) (tolerr 0.0) (temp 0.0)
	 ($numer t) ($float t))
	
	(declare (cl:float w0 w1 w2 w3 w4 result cor11 area qprev stone step
			 qleft qnow qdiff esterr tolerr temp)
		 (fixnum i levmin levmax levout nomax nofin nofun lev nim)
		 (boolean $numer $float))

	(setq
	 $quanc8_flag 0.0
	 $quanc8_errest 0.0
	 nofin (- nomax (* 8 (+ levmax (* -1. levout) (expt 2. (1+ levout))))))
	      
	(cond ((= a b)
	       (return 0.0)))

	(setf (x 0.)
	      a)

	(setf (x 16.) 
	      b)

	(setf (x 8.)
	      (*$ 0.5 (+$ (x 0.)
			  (x 16.))))

	(setf (x 4.)
	      (*$ 0.5 (+$ (x 0.)
			  (x 8.))))

	(setf (x 12.)
	      (*$ 0.5 (+$ (x 16.)
			  (x 8.))))

	(setf (x 2.)
	      (*$ 0.5 (+$ (x 0.)
			  (x 4.))))

	(setf (x 6.)
	      (*$ 0.5 (+$ (x 4.)
			  (x 8.))))

	(setf (x 10.)
	      (*$ 0.5 (+$ (x 12.)
			  (x 8.))))

	(setf (x 14.)
	      (*$ 0.5 (+$ (x 12.)
			  (x 16.))))


  do-25
        (when quanc8-|^]|
	      (mtell "QUANC8 calculating at X= ~S" (x i))
	      (setq quanc8-|^]| nil))
	      
	(setf (f i)
	      (fcall$ fun (x i)))
	
	(setq i (+ 2. i))

	(if (> i 16.)
	    (go do-25-done))

	(go do-25)

do-25-done
	(setq nofun 9.)

 tag-30
	(setq i 1.)

  do-30
	(setf (x i)
	      (*$ 0.5 (+$ (x (1- i))
			  (x (1+ i)))))
        (when quanc8-|^]|
	      (mtell "QUANC8 calculating at X= ~S" (x i))
	      (setq quanc8-|^]| nil))

	(setf (f i)
	      (fcall$ fun (x i)))

	(setq i (+ 2. i))

	(if (> i 15.)
	    (go do-30-done))

	(go do-30)

do-30-done
	(setq nofun (+ 8. nofun))
	
	(setq step (//$ (-$ (x 16.)
			    (x 0.))
			16.0))

	(setq qleft (*$ step (+$ (*$ w0 (+$ (f 0.)
					    (f 8.)))
				 (*$ w1 (+$ (f 1.)
					    (f 7.)))
				 (*$ w2 (+$ (f 2.)
					    (f 6.)))
				 (*$ w3 (+$ (f 3.)
					    (f 5.)))
				 (*$ w4 (f 4.)))))

	(setf (qright (1+ lev))
	      (*$ step (+$ (*$ w0 (+$ (f 8.)
				      (f 16.)))
			   (*$ w1 (+$ (f 9.)
				      (f 15.)))
			   (*$ w2 (+$ (f 10.)
				      (f 14.)))
			   (*$ w3 (+$ (f 11.)
				      (f 13.)))
			   (*$ w4 (f 12.)))))
	
	(setq qnow (+$ qleft (qright (1+ lev))))

	(setq qdiff (-$ qnow qprev))

	(setq area (+$ area qdiff))

	(setq esterr (//$ (abs qdiff)
			  1023.0))

	(setq tolerr (*$
		      (//$ step stone)
		      (max $quanc8_abserr
			   (*$ (abs area)
			       $quanc8_relerr))))

	(if (< lev levmin)
	    (go tag-50))

	(if (or (> lev levmax)
		(= lev levmax))
	    (go tag-62))

	(if (> nofun nofin)
	    (go tag-60))

	(if (< esterr tolerr)
	    (go tag-70))

 tag-50
	(setq nim (* 2. nim)
	      lev (1+ lev))

	(setq i 1.)

  do-52
	(setf (fsave i lev)
	      (f (+ 8. i)))

	(setf (xsave i lev)
	      (x (+ 8. i)))

	(setq i (1+ i))

	(if (> i 8.)
	    (go do-52-done))

	(go do-52)

do-52-done
	(setq qprev qleft)

	(setq i 1.)

  do-55
	(setf (f (+ 18. (* -2. i)))
	      (f (+ 9. (* -1. i))))

	(setf (x (+ 18. (* -2. i)))
	      (x (+ 9. (* -1. i))))

	(setq i (1+ i))

	(if (> i 8.)
	    (go tag-30))

	(go do-55)

 tag-60
	(setq nofin (* 2. nofin))

	(setq levmax levout)

	(setq $quanc8_flag (+$ $quanc8_flag (//$ (-$ b (x 0.))
				 (-$ b a))))

	(go tag-70)

 tag-62
	(setq $quanc8_flag (+$ $quanc8_flag 1.0))

 tag-70
	(setq result (+$ result qnow)
	      $quanc8_errest (+$ $quanc8_errest esterr)
	      cor11 (+$ cor11 (//$ qdiff 1023.0)))

 tag-72
	(if (= nim (* 2. (// nim 2.)))
	    (go tag-75))

	(setq nim (// nim 2.))

	(setq lev (1- lev))

	(go tag-72)

 tag-75
	(setq nim (1+ nim))

	(if (or (< lev 0.)
		(= lev 0.))
	    (go tag-80))

	(setq qprev (qright lev))

	(setq i 1.)

	(setf (x 0.)
	      (x 16.))

	(setf (f 0.)
	      (f 16.))

  do-78
	(setf (f (* 2. i))
	      (fsave i lev))

	(setf (x (* 2. i))
	      (xsave i lev))
	
	(setq i (1+ i))

	(if (> i 8.)
	    (go tag-30))
	
	(go do-78)

 tag-80
	(setq result (+$ result cor11))

	(if (= $quanc8_errest 0.0)
	    (return result))
	
 tag-82
    (setq temp (+$ $quanc8_errest (abs result)))
	
	(if (not (= temp (abs result)))
	    (return result))

	(setq $quanc8_errest (*$ 2.0 $quanc8_errest))
	
	(go tag-82))))

	  ;; For whatever reason and with a suitable
	  ;; discrete meaning of convergence...
	  (merror "QUANC8 failed to converge.")))))
