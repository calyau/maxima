;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp; Package: Macsyma -*- ;;;;;;;;;;;;;;;;;;;
;;;     (c) Copyright 1982 Massachusetts Institute of Technology         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "maxima")

(defgrad $ai (x)
  (($dai) x))

(defgrad $dai (x)
  ((mtimes) x (($ai) x)))

(defgrad $bi (x)
  (($dbi) x))

(defgrad $dbi (x)
  ((mtimes) x (($bi) x)))

(proclaim '(ftype (function (double-float) double-float) dairy airy dbairy))

(defun airy (rz)
  (declare (double-float rz))
  (cond ((< (abs rz) 3.0)
	 (let ((temp (*$ rz rz rz)))
	   (declare (double-float temp)) 
	   (-$ (+$ .3550281
		   (*$ temp
		       (+$ .05917135
			   (*$ temp
			       (+$ 1.97237834e-3
				   (*$ temp
				       (+$ 2.73941433e-5
					   (*$ temp
					       (+$ 2.07531393e-7
						   (*$ temp
						       (+$ 9.8824472e-10
							   (*$ temp 3.22955787e-12))))))))))))
	       (+$ (*$ rz
		       (+$ .2588194
			   (*$ temp
			       (+$ .0215682834
				   (*$ temp
				       (+$ 5.1353055e-4
					   (*$ temp
					       (+$ 5.70589507e-6
						   (*$ temp
						       (+$ 3.65762505e-8
							   (*$ temp
							       (+$ 1.52401045e-10
								   (*$ temp 4.45617086e-13)))))))))))))))))
	((> 0.0 rz)
	 (let ((temp1 (*$ .66666667 (expt (*$ -1.0 rz) 1.5)))
	       (temp2 (//$ -2.25 (*$ rz rz rz))))
	   (declare (double-float temp1 temp2)) 
	   (*$ (//$ 1.0 (sqrt (*$ 3.1415926535 (sqrt (*$ -1.0 rz)))))
	       (-$ (*$ (sin (+$ temp1 .78539816))
		       (+$ 1.0
			   (*$ temp2
			       (+$ -.037133488
				   (*$ temp2 .0576491905)))))
		   (*$ (//$ (cos (+$ temp1 .78539816)) temp1)
		       (+$ .069444444
			   (*$ temp2
			       (+$ -.037993195
				   (*$ temp2 .116099063)))))))))
	(t
	 (let ((temp2 (//$ 1.5 (expt rz 1.5))))
	   (declare (double-float   temp2))
	   (*$ (//$ .5 (sqrt (*$ 3.1415926535 (sqrt rz))))
	       (exp (*$ -1.0 (*$ .6666667 (expt rz 1.5))))
	       (+$ 1.0
		   (*$ temp2
		       (+$ -.069444444
			   (*$ temp2
			       (+$ .037133488
				   (*$ temp2
				       (+$ -.037993195
					   (*$ temp2
					       (+$ .0576491905
						   (*$ temp2
						       (+$ -.116099063
							   (*$ temp2 .2915914)))))))))))))))))

(defun $ai ($arg)
  (cond ((numberp $arg) (airy (float $arg)))
	(t (list '($ai simp) $arg))))

;;;(declare (flonum (bi flonum)))

(defun bairy (rz)
  (declare (double-float rz))
  (cond ((< (abs rz) 3.0)
	 (let ((temp (*$ rz rz rz)))
	   (declare (double-float temp))
	   (+$ (+$ .61492671
		   (*$ temp
		       (+$ .102487785
			   (*$ temp
			       (+$ 3.4162595e-3
				   (*$ temp
				       (+$ 4.74480486e-5
					   (*$ temp
					       (+$ 3.59454915e-7
						   (*$ temp
						       (+$ 1.71169007e-9
							   (*$ temp 5.59375834e-12))))))))))))
	       (*$ rz
		   (+$ .44828835
		       (*$ temp
			   (+$ .0373573625
			       (*$ temp
				   (+$ 8.8946102e-4
				       (*$ temp
					   (+$ 9.8829001e-6
					       (*$ temp
						   (+$ 6.3351924e-8
						       (*$ temp
							   (+$ 2.6396635e-10
							       (*$ temp 7.71831435e-13))))))))))))))))
	((> 0.0 rz)
	 (let ((temp1 (*$ .66666667 (expt (*$ -1.0 rz) 1.5)))
	       (temp2 (//$ -2.25 (*$ rz rz rz))))
	   (declare (double-float  temp1 temp2))
	   (*$ (//$ 1.0 (sqrt (*$ 3.1415926535 (sqrt (*$ -1.0 rz)))))
	       (+$
		(*$ (cos (+$ temp1 .78539816))
		    (+$ 1.0
			(*$ temp2
			    (+$ -.037133488
				(*$ .0576491905 temp2)))))
		(*$ (//$ (sin (+$ temp1 .78539816)) temp1)
		    (+$ .069444444
			(*$ temp2
			    (+$ -.037993195
				(*$ .116099063 temp2)))))))))
	(t
	 (let ((temp2 (//$ 1.5 (expt rz 1.5))))
	   (declare (double-float   temp2))
	   (*$ (//$ (exp (*$ .66666667 (expt rz 1.5)))
		    (sqrt (*$ 3.1415926535 (sqrt rz))))
	       (+$ 1.0
		   (*$ temp2
		       (+$ .069444444
			   (*$ temp2
			       (+$ .037133488
				   (*$ temp2
				       (+$ .037993195
					   (*$ temp2
					       (+$ .0576491905
						   (*$ temp2
						       (+$ .116099063
							   (*$ temp2 .2915914)))))))))))))))))

(defun $bi ($arg)
  (cond ((numberp $arg) (bairy (float $arg)))
	(t (list '($bi simp) $arg))))


;;;(declare (flonum (dairy flonum)))

(defun dairy (rz)
  (declare (double-float rz))
  (cond ((< (abs rz) 3.0)
	 (let ((temp (*$ rz rz rz)))
	   (declare (double-float temp)) 
	   (-$ (*$ rz
		   rz
		   (+$ .17751405
		       (*$ temp
			   (+$ .01183427
			       (*$ temp
				   (+$ 2.4654294e-4
				       (*$ temp
					   (+$ 2.49037668e-6
					       (*$ temp
						   (+$ 1.48236707e-8
						       (*$ temp 5.8132042e-11)))))))))))
	       (+$ .2588194
		   (*$ temp
		       (+$ .086273134
			   (*$ temp
			       (+$ 3.5947139e-3
				   (*$ temp
				       (+$ 5.70589507e-5
					   (*$ temp
					       (+$ 4.7549126e-7
						   (*$ temp
						       (+$ 2.43841672e-9
							   (*$ temp 8.4667248e-12)))))))))))))))
	((> 0.0 rz)
	 (let ((temp1 (*$ .66666667 (expt (*$ -1.0 rz) 1.5)))
	       (temp2 (//$ -2.25 (*$ rz rz rz))))
	   (declare (double-float temp1 temp2)) 
	   (*$ -1.0
	       (sqrt (//$ (sqrt (*$ -1.0 rz)) 3.1415926535))
	       (+$ (*$ (cos (+$ temp1 .78539816))
		       (+$ 1.0
			   (*$ temp2
			       (+$ .043885031
				   (*$ temp2
				       (+$ -.062662164
					   (*$ temp2 3.08253244)))))))
		   (*$ (//$ (sin (+$ temp1 .78539816)) temp1)
		       (+$ -.097222222
			   (*$ temp2	
			       (+$ .042462831
				   (*$ temp2
				       (+$ -.124105896
					   (*$ temp2 .92047998)))))))))))
	(t
	 (let ((temp2 (//$ 1.5 (expt rz 1.5))))
	   (declare (double-float   temp2))
	   (*$ -.5				
	       (sqrt (//$ (sqrt rz) 3.1415926535))
	       (exp (*$ -.66666667 (expt rz 1.5)))
	       (+$ 1.0
		   (*$ temp2
		       (+$ .097222222
			   (*$ temp2
			       (+$ -.043885031
				   (*$ temp2
				       (+$ .042462831
					   (*$ temp2
					       (+$ -.062662164
						   (*$ temp2
						       (+$ .124105896
							   (*$ temp2 -3.08253244)))))))))))))))))

(defun $dai ($arg)
  (cond ((numberp $arg) (dairy (float $arg)))
	(t (list '($dai simp) $arg))))

;;;(declare (flonum (dbairy flonum)))

(defun dbairy (rz)
  (declare (double-float rz))
  (cond ((< (abs rz) 3.0)
	 (let ((temp (*$ rz rz rz)))
	   (declare (double-float temp))
	   (+$ (*$ rz
		   rz
		   (+$ .307463355
		       (*$ temp
			   (+$ .020497557
			       (*$ temp
				   (+$ 4.2703244e-4
				       (*$ temp
					   (+$ 4.313459e-6
					       (*$ temp
						   (+$ 2.5675351e-8
						       (*$ temp 1.00687651e-10)))))))))))
	       (+$ .44828835
		   (*$ temp
		       (+$ .14942945
			   (*$ temp
			       (+$ 6.2262271e-3
				   (*$ temp
				       (+$ 9.8829001e-5
					   (*$ temp
					       (+$ 8.2357502e-7
						   (*$ temp
						       (+$ 4.22346157e-9
							   (*$ temp 1.46647972e-11)))))))))))))))
	((> 0.0 rz)
	 (let ((temp1 (*$ .66666667 (expt (*$ -1.0 rz) 1.5)))
	       (temp2 (//$ -2.25 (*$ rz rz rz))))
	   (declare (double-float  temp1 temp2))
	   (*$ (sqrt (//$ (sqrt (*$ -1.0 rz)) 3.14159265353))
	       (-$ (*$ (sin (+$ temp1 .78539816))
		       (+$ 1.0
			   (*$ temp2
			       (+$ .043885031
				   (*$ temp2
				       (+$ -.062662164
					   (*$ temp2 3.08253244)))))))
		   (*$ (//$ (cos (+$ temp1 .78539816)) temp1)
		       (+$ -.097222222
			   (*$ temp2
			       (+$ .042462831
				   (*$ temp2
				       (+$ -.124105896
					   (*$ temp2 .92037998)))))))))))
	(t
	 (let ((temp2 (//$ 1.5 (expt rz 1.5))))
	   (declare (double-float   temp2))
	   (*$ (sqrt (//$ (sqrt rz) 3.1415926535))
	       (exp (*$ .66666667 (expt rz 1.5)))
	       (+$ 1.0
		   (*$ temp2
		       (+$ -.097222222
			   (*$ temp2
			       (+$ -.043885031
				   (*$ temp2
				       (+$ -.042462831
					   (*$ temp2
					       (+$ -.062662164
						   (*$ temp2
						       (+$ -.124105896
							   (*$ temp2 -3.08253244)))))))))))))))))

(defun $dbi ($arg)
  (cond ((numberp $arg) (dbairy (float $arg)))
	(t (list '($dbi simp) $arg))))
