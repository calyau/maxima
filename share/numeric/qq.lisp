;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")

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

(DEFMVAR $QUANC8_FLAG 0.0
  "integer part-how many regions failed to converge. fractional part-
  how much of the interval was left when it failed")
(DEFMVAR $QUANC8_ERREST 0.0
  "an estimated error based on the difference between one- and two-panel
  calculation of an interval, summed over the whole region")
(DEFMVAR $QUANC8_ABSERR 1.0e-8
  "absolute error tolerance for reasonable single-precision answers")
(DEFMVAR $QUANC8_RELERR 1.0e-4
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

(DEFVAR QUANC8-FREE-LIST ()
  "For efficient calls to quanc8 keep arrays we need on a free list.")

(DEFVAR QUANC8-|^]| ())
(DEFUN QUANC8-|^]| () (SETQ QUANC8-|^]| T))

(DEFUN CALL-QUANC8 (FUN A B)
  (BIND-TRAMP1$
   FUN FUN
   (LET ((VALS (IF QUANC8-FREE-LIST
		   (POP QUANC8-FREE-LIST)
		   (LIST (*array nil 'flonum 17.)
			 (*array nil 'flonum 17.)
			 (*array nil 'flonum 9. 31.)
			 (*array nil 'flonum 9. 31.)
			 (*array nil 'flonum 32.))))
	 (USER-TIMESOFAR (CONS #'QUANC8-|^]| USER-TIMESOFAR))) 
     (PROG1
      (QUANC8 FUN
	      (MTO-FLOAT A)
	      (MTO-FLOAT B)
	      (CAR VALS)
	      (CAR (CDR VALS))
	      (CAR (CDDR VALS))
	      (CAR (CDDDR VALS))
	      (CAR (CDDDDR VALS)))
      (PUSH VALS QUANC8-FREE-LIST)))))

(DEFUN QUANC8 (FUN A B X-ARR F-ARR XSAVE-ARR FSAVE-ARR QRIGHT-ARR)
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
    (PROG ((LEVMIN 1.) (LEVMAX 30.) (LEVOUT 6.) (NOMAX 5000.) (NOFIN 0)
	 (W0 (//$ 3956.0 14175.0)) (W1 (//$ 23552.0 14175.0))
	 (W2 (//$ -3712.0 14175.0)) (W3 (//$ 41984.0 14175.0))
	 (W4 (//$ -18160.0 14175.0))
	 (RESULT 0.0) (COR11 0.0) (AREA 0.0)
	 (NOFUN 0.) (LEV 0.) (NIM 1.) (QPREV 0.0)
	 (STONE (//$ (-$ B A) 16.0))
	 (I 0.)
	 (STEP 0.0) (QLEFT 0.0) (QNOW 0.0) (QDIFF 0.0)
	 (ESTERR 0.0) (TOLERR 0.0) (TEMP 0.0)
	 ($NUMER t) ($FLOAT t))
	
	(DECLARE (cl:float W0 W1 W2 W3 W4 RESULT COR11 AREA QPREV STONE STEP
			 QLEFT QNOW QDIFF ESTERR TOLERR TEMP)
		 (FIXNUM I LEVMIN LEVMAX LEVOUT NOMAX NOFIN NOFUN LEV NIM)
		 (boolean $NUMER $FLOAT))

	(SETQ
	 $QUANC8_FLAG 0.0
	 $QUANC8_ERREST 0.0
	 NOFIN (- NOMAX (* 8 (+ LEVMAX (* -1. LEVOUT) (EXPT 2. (1+ LEVOUT))))))
	      
	(COND ((= A B)
	       (RETURN 0.0)))

	(SETF (X 0.)
	      A)

	(SETF (X 16.) 
	      B)

	(SETF (X 8.)
	      (*$ 0.5 (+$ (X 0.)
			  (X 16.))))

	(SETF (X 4.)
	      (*$ 0.5 (+$ (X 0.)
			  (X 8.))))

	(SETF (X 12.)
	      (*$ 0.5 (+$ (X 16.)
			  (X 8.))))

	(SETF (X 2.)
	      (*$ 0.5 (+$ (X 0.)
			  (X 4.))))

	(SETF (X 6.)
	      (*$ 0.5 (+$ (X 4.)
			  (X 8.))))

	(SETF (X 10.)
	      (*$ 0.5 (+$ (X 12.)
			  (X 8.))))

	(SETF (X 14.)
	      (*$ 0.5 (+$ (X 12.)
			  (X 16.))))


  DO-25
        (WHEN QUANC8-|^]|
	      (MTELL "QUANC8 calculating at X= ~S" (X I))
	      (SETQ QUANC8-|^]| NIL))
	      
	(SETF (F I)
	      (FCALL$ FUN (X I)))
	
	(SETQ I (+ 2. I))

	(IF (> I 16.)
	    (GO DO-25-DONE))

	(GO DO-25)

DO-25-DONE
	(SETQ NOFUN 9.)

 TAG-30
	(SETQ I 1.)

  DO-30
	(SETF (X I)
	      (*$ 0.5 (+$ (X (1- I))
			  (X (1+ I)))))
        (WHEN QUANC8-|^]|
	      (MTELL "QUANC8 calculating at X= ~S" (X I))
	      (SETQ QUANC8-|^]| NIL))

	(SETF (F I)
	      (FCALL$ FUN (X I)))

	(SETQ I (+ 2. I))

	(IF (> I 15.)
	    (GO DO-30-DONE))

	(GO DO-30)

DO-30-DONE
	(SETQ NOFUN (+ 8. NOFUN))
	
	(SETQ STEP (//$ (-$ (X 16.)
			    (X 0.))
			16.0))

	(SETQ QLEFT (*$ STEP (+$ (*$ W0 (+$ (F 0.)
					    (F 8.)))
				 (*$ W1 (+$ (F 1.)
					    (F 7.)))
				 (*$ W2 (+$ (F 2.)
					    (F 6.)))
				 (*$ W3 (+$ (F 3.)
					    (F 5.)))
				 (*$ W4 (F 4.)))))

	(SETF (QRIGHT (1+ LEV))
	      (*$ STEP (+$ (*$ W0 (+$ (F 8.)
				      (F 16.)))
			   (*$ W1 (+$ (F 9.)
				      (F 15.)))
			   (*$ W2 (+$ (F 10.)
				      (F 14.)))
			   (*$ W3 (+$ (F 11.)
				      (F 13.)))
			   (*$ W4 (F 12.)))))
	
	(SETQ QNOW (+$ QLEFT (QRIGHT (1+ LEV))))

	(SETQ QDIFF (-$ QNOW QPREV))

	(SETQ AREA (+$ AREA QDIFF))

	(SETQ ESTERR (//$ (ABS QDIFF)
			  1023.0))

	(SETQ TOLERR (*$
		      (//$ STEP STONE)
		      (MAX $QUANC8_ABSERR
			   (*$ (ABS AREA)
			       $QUANC8_RELERR))))

	(IF (< LEV LEVMIN)
	    (GO TAG-50))

	(IF (OR (> LEV LEVMAX)
		(= LEV LEVMAX))
	    (GO TAG-62))

	(IF (> NOFUN NOFIN)
	    (GO TAG-60))

	(IF (< ESTERR TOLERR)
	    (GO TAG-70))

 TAG-50
	(SETQ NIM (* 2. NIM)
	      LEV (1+ LEV))

	(SETQ I 1.)

  DO-52
	(SETF (FSAVE I LEV)
	      (F (+ 8. I)))

	(SETF (XSAVE I LEV)
	      (X (+ 8. I)))

	(SETQ I (1+ I))

	(IF (> I 8.)
	    (GO DO-52-DONE))

	(GO DO-52)

DO-52-DONE
	(SETQ QPREV QLEFT)

	(SETQ I 1.)

  DO-55
	(SETF (F (+ 18. (* -2. I)))
	      (F (+ 9. (* -1. I))))

	(SETF (X (+ 18. (* -2. I)))
	      (X (+ 9. (* -1. I))))

	(SETQ I (1+ I))

	(IF (> I 8.)
	    (GO tag-30))

	(GO DO-55)

 TAG-60
	(SETQ NOFIN (* 2. NOFIN))

	(SETQ LEVMAX LEVOUT)

	(SETQ $QUANC8_FLAG (+$ $QUANC8_FLAG (//$ (-$ B (X 0.))
				 (-$ B A))))

	(GO TAG-70)

 TAG-62
	(SETQ $QUANC8_FLAG (+$ $QUANC8_FLAG 1.0))

 TAG-70
	(SETQ RESULT (+$ RESULT QNOW)
	      $QUANC8_ERREST (+$ $QUANC8_ERREST ESTERR)
	      COR11 (+$ COR11 (//$ QDIFF 1023.0)))

 TAG-72
	(IF (= NIM (* 2. (// NIM 2.)))
	    (GO TAG-75))

	(SETQ NIM (// NIM 2.))

	(SETQ LEV (1- LEV))

	(GO TAG-72)

 TAG-75
	(SETQ NIM (1+ NIM))

	(IF (OR (< LEV 0.)
		(= LEV 0.))
	    (GO TAG-80))

	(SETQ QPREV (QRIGHT LEV))

	(SETQ I 1.)

	(SETF (X 0.)
	      (X 16.))

	(SETF (F 0.)
	      (F 16.))

  DO-78
	(SETF (F (* 2. I))
	      (FSAVE I LEV))

	(SETF (X (* 2. I))
	      (XSAVE I LEV))
	
	(SETQ I (1+ I))

	(IF (> I 8.)
	    (GO tag-30))
	
	(GO DO-78)

 TAG-80
	(SETQ RESULT (+$ RESULT COR11))

	(IF (= $QUANC8_ERREST 0.0)
	    (RETURN RESULT))
	
 TAG-82
    (SETQ TEMP (+$ $QUANC8_ERREST (ABS RESULT)))
	
	(IF (NOT (= TEMP (ABS RESULT)))
	    (RETURN RESULT))

	(SETQ $QUANC8_ERREST (*$ 2.0 $QUANC8_ERREST))
	
	(GO TAG-82))))

	  ;; For whatever reason and with a suitable
	  ;; discrete meaning of convergence...
	  (merror "QUANC8 failed to converge.")))))
