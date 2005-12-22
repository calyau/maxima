;; A Maxima ring stucture
;; Copyright (C) 2005, Barton Willis

;; Barton Willis
;; Department of Mathematics
;; University of Nebraska at Kearney
;; Kearney NE 68847
;; willisb@unk.edu

;; This source code is licensed under the terms of the Lisp Lesser 
;; GNU Public License (LLGPL). The LLGPL consists of a preamble, published
;; by Franz Inc. (http://opensource.franz.com/preamble.html), and the GNU 
;; Library General Public License (LGPL), version 2, or (at your option)
;; any later version.  When the preamble conflicts with the LGPL, 
;; the preamble takes precedence. 

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Library General Public License for details.

;; You should have received a copy of the GNU Library General Public
;; License along with this library; if not, write to the
;; Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301, USA.

;; Let's have version numbers 1,2,3,...

(eval-when (:compile-toplevel :load-toplevel :execute)
  ($put '$mring 1 '$version))

;; (1) In maxima-grobner.lisp, there is a structure 'ring.'  

;; (2) Some functions in this structure, for example 'great' might 
;;     not be defined for a ring; when this is the case, a function 
;;     can signal an error.

;; (3) Floating point addition isn't associative; so a mring needn't
;;     be a ring.  But a mring is 'close' to being a ring.

;; Description of the mring fields:

(defstruct mring
  name
  coerce-to-lisp-float
  abs
  great
  add
  div
  rdiv
  mult
  sub
  negate
  add-id
  mult-id
  fzerop
  maxima-to-mring
  mring-to-maxima)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmvar $%mrings `((mlist) $floatfield $complexfield $rationalfield $crering $generalring $bigfloatfield
		      $runningerror $noncommutingring)))
	
;;(defun $require_ring (f pos fun)
;;  (if (not (mring-p f)) (merror "The ~:M argument of the function ~:M must be a ring" pos fun)))
	
(defun $require_ring (ringname pos fun)
  (if ($member ringname $%mrings) (get ringname 'ring)
    (merror "The ~:M argument of the function '~:M' must be the name of a ring" pos fun)))

(defparameter *floatfield*
  (make-mring
   :name '$floatfield
   :coerce-to-lisp-float #'identity
   :abs #'abs
   :great #'>
   :add #'+
   :div #'/
   :rdiv #'/
   :mult #'*
   :sub #'-
   :negate #'-
   :add-id #'(lambda () 0.0)
   :mult-id #'(lambda () 1.0)
   :fzerop #'(lambda (s) (= 0.0 s))
   :mring-to-maxima #'identity
   :maxima-to-mring #'(lambda (s) 
			(setq s ($float s))
			(if (floatp s) s (merror "Unable to convert ~:M to a double float" s)))))

(setf (get '$floatfield 'ring) *floatfield*)

(defparameter *complexfield*
  (make-mring
   :name '$complexfield
   :coerce-to-lisp-float #'identity
   :abs #'abs
   :great #'>
   :add #'+
   :div #'/
   :mult #'*
   :sub #'-
   :negate #'-
   :add-id #'(lambda () 0.0)
   :mult-id #'(lambda () 1.0)
   :fzerop #'(lambda (s) (= s 0.0))
   :mring-to-maxima #'(lambda (s) (add (realpart s) (mul '$%i (imagpart s))))
   :maxima-to-mring #'(lambda (s) 
			(setq s ($rectform (meval s)))
			(if (and (floatp ($float ($realpart s))) (floatp ($float ($imagpart s))))
			    (complex ($float ($realpart s)) ($float ($imagpart s)))
			  (merror "Unable to convert ~:M to a complex double float" s)))))

(setf (get '$complexfield 'ring) *complexfield*)

(defparameter *rationalfield*
  (make-mring
   :name '$rationalfield 
   :coerce-to-lisp-float #'(lambda (s) ($float s))
   :abs #'abs
   :great #'>
   :add #'+
   :div #'/
   :mult #'*
   :sub #'-
   :negate #'-
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (= s 0))
   :mring-to-maxima #'(lambda (s) (simplify `((rat) ,(numerator s) ,(denominator s))))
   :maxima-to-mring 
   #'(lambda (s) 
       (if (or (floatp s) ($bfloatp s)) (setq s ($rationalize s)))
       (if ($ratnump s) (if (integerp s) s (/ ($num s) ($denom s)))
	 (merror "Unable to convert ~:M to a rational number" s)))))

(setf (get '$rationalfield 'ring) '*rationalfield*)

(defparameter *crering*
  (make-mring
   :name '$crering
   :coerce-to-lisp-float nil
   :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
   :great #'(lambda (a b) (like b 0))
   :add #'add
   :div #'div
   :rdiv #'div
   :mult #'mult
   :sub #'sub
   :negate #'(lambda (s) (mult -1 s))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (like s 0))
   :mring-to-maxima #'(lambda (s) s)
   :maxima-to-mring #'(lambda (s) ($rat s))))
		
(setf (get '$crering 'ring) *crering*)

(defparameter *generalring*
  (make-mring
   :name '$generalring
   :coerce-to-lisp-float nil
   :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
   :great #'(lambda (a b) (like b 0))
   :add #'(lambda (a b) ($rectform (add a b)))
   :div #'(lambda (a b) ($rectform (div a b)))
   :rdiv #'(lambda (a b) ($rectform (div a b)))
   :mult #'(lambda (a b) ($rectform (mult a b)))
   :sub #'(lambda (a b) ($rectform (sub a b)))
   :negate #'(lambda (a) (mult -1 a))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (like s 0))
   :mring-to-maxima #'(lambda (s) s)
   :maxima-to-mring #'(lambda (s) s)))

(setf (get '$generalring 'ring) *generalring*)

(defparameter *bigfloatfield*
  (make-mring
   :name '$bigfloatfield
   :coerce-to-lisp-float #'(lambda (s) 
			     (setq s ($rectform ($float s)))
			     (complex ($realpart s) ($imagpart s)))
   
   :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
   :great #'mgrp
   :add #'(lambda (a b) ($rectform (add a b)))
   :div #'(lambda (a b) ($rectform (div a b)))
   :rdiv #'(lambda (a b) ($rectform (div a b)))
   :mult #'(lambda (a b) ($rectform (mult a b)))
   :sub #'(lambda (a b) ($rectform (sub a b)))
   :negate #'(lambda (a) (mult -1 a))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (like s bigfloatzero))
   :mring-to-maxima #'(lambda (s) s)
   :maxima-to-mring #'(lambda (s) 
			(setq s ($bfloat s))
			(if ($bfloatp s) s
			  (merror "Unable to convert matrix entry to a big float")))))

(setf (get '$bigfloatfield 'ring) *bigfloatfield*)

(defun fp-abs (a)
  (list (abs (first a)) (second a)))

(defun fp+ (a b)
  (cond ((= (first a) 0.0) b)
	((= (first b) 0.0) a)
	(t
	 (let ((s (+ (first a) (first b))))
	   (if (= 0.0 s) (merror "floating point divide by zero"))
	   (list s (ceiling (+ 1
			       (abs (/ (* (first a) (second a)) s))
			       (abs (/ (* (first b) (second b)) s)))))))))
				
(defun fp- (a b)
  (cond ((= (first a) 0.0) (list (- (first b)) (second b)))
	((= (first b) 0.0) a)
	(t
	 (let ((s (- (first a) (first b))))
	   (if (= 0.0 s) (merror "floating point divide by zero"))
	   (list s (ceiling (+ 1
			       (abs (/ (* (first a) (second a)) s))
			       (abs (/ (* (first b) (second b)) s)))))))))

(defun fp* (a b)
  (if (or (= (first a) 0.0) (= (first b) 0.0)) (list 0.0 0)
    (list (* (first a) (first b)) (+ 1 (second a) (second b)))))

(defun fp/ (a b)
  (if (= (first a) 0) (list 0.0 0)
    (list (/ (first a) (first b)) (+ 1 (second a) (second b)))))

(defun $fpadd (&rest a)
  (let ((fadd (mring-add *runningerror*))
	(add-id (funcall (mring-add-id *runningerror*)))
	(fconvert (mring-maxima-to-mring *runningerror*))
	(frevert (mring-mring-to-maxima *runningerror*)))
    (funcall frevert (reduce #'(lambda (a b) (funcall fadd a b)) a
			     :key #'(lambda (a) (funcall fconvert a))
			     :initial-value add-id))))
			  
(defun $fpmult (&rest a)
  (let ((fmult (mring-mult *runningerror*))
	(mult-id (funcall (mring-mult-id *runningerror*)))
	(fconvert (mring-maxima-to-mring *runningerror*))
	(frevert (mring-mring-to-maxima *runningerror*)))
    (funcall frevert (reduce #'(lambda (a b) (funcall fmult a b)) a
			     :key #'(lambda (a) (funcall fconvert a))
			     :initial-value mult-id))))

(defun $addmatrices(a b fn)
  ($matrixmap fn a b))

(defparameter *runningerror*
  (make-mring 
   :name '$runningerror
   :coerce-to-lisp-float #'(lambda (s) (if (consp s) (first s) s))
   :abs #'fp-abs
   :great #'(lambda (a b) (> (first a) (first b)))
   :add #'fp+
   :div #'fp/
   :rdiv #'fp/
   :mult #'fp*
   :sub #'fp-
   :negate #'(lambda (s) (list (- (first s)) (second s)))
   :add-id #'(lambda () (list 0 0))
   :mult-id #'(lambda () (list 1 0))
   :fzerop #'(lambda (s) (like (first s) 0))
   :mring-to-maxima #'(lambda (s) `((mlist) ,@s))
   :maxima-to-mring #'(lambda (s) (if ($listp s) (cdr s) (list ($float s) 1)))))

(setf (get '$runningerror 'ring) *runningerror*)


(defparameter *noncommutingring* 
  (make-mring
   :name '$noncommutingring
   :coerce-to-lisp-float nil
   :abs #'(lambda (s) (simplify (mfuncall '$cabs s)))
   :great #'(lambda (a b) (like b 0))
   :add #'(lambda (a b) (add a b))
   :div #'(lambda (a b) (simplify `((mnctimes) ,a ((mncexpt) ,b -1))))  
   :rdiv #'(lambda (a b) (simplify `((mnctimes) ((mncexpt) ,b -1) ,a)))
   :mult #'(lambda (a b) (simplify `((mnctimes) ,a ,b)))
   :sub #'(lambda (a b) (sub a b))
   :negate #'(lambda (a) (mult -1 a))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (like s 0))
   :mring-to-maxima #'identity
   :maxima-to-mring #'identity))

(setf (get '$noncommutingring 'ring) *noncommutingring*)

(defun op-equalp (e &rest op)
  (and (consp e) (consp (car e)) (some #'(lambda (s) (equal (caar e) s)) op)))

