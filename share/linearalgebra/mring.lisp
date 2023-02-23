;; A Maxima ring structure
;; Copyright (C) 2005, 2007, 2021, 2022 Barton Willis

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

(in-package :maxima)

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

;; For floating point numbers (both real and complex), the lu_factor code uses
;; partial pivoting. Thus lu_factor needs to be able to compare the magnitudes.
;; Thus we include a function `great` in the mring structure. The function 'great`
;; is always supposed be be called on the magnitude of a ring element.

;; For nonordered fields (such as generalring) where partial pivoting isn't needed,
;; we can define
;;   :great #'(lambda (a b) (declare (ignore a)) (eq t (meqp b 0)))
;; This inhibits row swapping unless it is absolutely needed to avoid dividing
;; by zero.

;; The function coerce-to-lisp-float is only used by lu_factor to estimate the 
;; matrix condition number. This is only important for the floating point case. 
;; Setting :coerce-to-lisp-float to nil inhibits estimating the matrix condition 
;; number. Except for matrices of floating point numbers, it's best to set 
;;coerce-to-lisp-float to nil.
(defstruct mring
  name
  coerce-to-lisp-float
  abs
  great
  add
  div
  rdiv
  reciprocal
  mult
  sub
  negate
  psqrt
  add-id
  mult-id
  fzerop
  adjoint
  maxima-to-mring
  mring-to-maxima)

(eval-when 
    (:compile-toplevel :load-toplevel :execute)
  (defmvar $%mrings `((mlist) $floatfield $complexfield $rationalfield $crering 
                      $generalring $bigfloatfield $runningerror $noncommutingring ))
  (defvar *gf-rings* '(gf-coeff-ring gf-ring ef-ring)) )

(defun $require_ring (ringname pos fun)
  (if (or ($member ringname $%mrings) (member ringname *gf-rings*))
    (get ringname 'ring)
    (merror (intl:gettext "The ~:M argument of the function '~:M' must be the name of a ring") pos fun)))

;; flonum-epsilon is defined in clmacs
(defparameter *floatfield*
  (make-mring
   :name '$floatfield
   :coerce-to-lisp-float #'cl:identity
   :abs #'abs
   :great #'>
   :add #'+
   :div #'/
   :rdiv #'/
   :reciprocal #'/
   :mult #'*
   :sub #'-
   :negate #'-
   :psqrt #'(lambda (s) (if (>= s 0) (cl:sqrt s) nil))
   :add-id #'(lambda () 0.0)
   :mult-id #'(lambda () 1.0)
   :fzerop #'(lambda (s)  (< (abs s) (* 4 +flonum-epsilon+)))
   :adjoint #'cl:identity
   :mring-to-maxima #'cl:identity
   :maxima-to-mring #'(lambda (s) 
			(setq s ($float s))
			(if (floatp s) s (merror (intl:gettext "Unable to convert ~:M to a float") s)))))

(setf (get '$floatfield 'ring) *floatfield*)

(defun coerce-expr-to-clcomplex (ex)
  (let* (($numer t)
         ($float t)         
         (split (risplit ex))
         ;; convert int, rat, and bfloat to float
         (re ($float (car split)))
         (im ($float (cdr split))))
    (declare (special $numer $float))
    (if (and (floatp re) (floatp im))
        (complex re im)
      (merror (intl:gettext "Unable to convert ~:M to a complex float") ex))))

(defparameter *complexfield*
  (make-mring
   :name '$complexfield
   :coerce-to-lisp-float #'cl:identity
   :abs #'cl:abs
   :great #'>
   :add #'+
   :div #'/
   :rdiv #'/
   :reciprocal #'/
   :mult #'*
   :sub #'-
   :negate #'-
   :psqrt #'(lambda (s) (if (and (= 0 (imagpart s)) (>= (realpart s) 0)) (cl:sqrt s) nil))
   :add-id #'(lambda () 0.0)
   :mult-id #'(lambda () 1.0)
   :fzerop #'(lambda (s) (< (abs s) (* 4 +flonum-epsilon+)))
   :adjoint #'cl:conjugate
   :mring-to-maxima #'(lambda (s) (add (realpart s) (mul '$%i (imagpart s)))) ;; was complexify
   :maxima-to-mring #'coerce-expr-to-clcomplex))

(setf (get '$complexfield 'ring) *complexfield*)

(defun rational-square-root (z)
  (let ((re (realpart z)) (im (imagpart z)) (q))
    (setq re (div ($num re) ($denom re)))
    (setq im (div ($num im) ($denom im)))
    (setq q (risplit (ftake '%sqrt (add re (mul '$%i im)))))
    (setq re (car q)
          im (cdr q))
    (cond ((and ($ratnump re) ($ratnump im))
           (complex (/ ($num re) ($denom re)) 
                    (/ ($num im) ($denom im))))
          (t nil))))

;; We allow both real and complex rational numbers.
(defparameter *rationalfield*
  (make-mring
   :name '$rationalfield
   :coerce-to-lisp-float nil
   :abs #'cl:abs
   :great #'(lambda (a b) (declare (ignore a)) (eql b 0))
   :add #'+
   :div #'/
   :rdiv #'/
   :reciprocal #'/
   :mult #'*
   :sub #'-
   :negate #'-
   :psqrt #'rational-square-root
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (eql s 0))
   :adjoint #'cl:identity
   :mring-to-maxima #'(lambda (s) 
      (let ((re (realpart s))
            (im (imagpart s)))
          (add (div (numerator re) (denominator re))
               (mul '$%i (div (numerator im) (denominator im))))))
   :maxima-to-mring 
   #'(lambda (s) 
        (setq s ($rationalize s))
        (let* ((z (trisplit s))
               (re (car z))
               (im (cdr z)))
       (if (and ($ratnump re) ($ratnump im))
         (complex (/ ($num re) ($denom re)) 
                  (/ ($num im) ($denom im)))
        (merror (intl:gettext "Unable to convert ~:M to a rational number") s))))))
(setf (get '$rationalfield 'ring) *rationalfield*)

(defparameter *crering*
  (make-mring
   :name '$crering
   :coerce-to-lisp-float nil
   :abs #'$cabs
   :great #'(lambda (a b) (declare (ignore a)) (eq t (meqp b 0)))
   :add #'add
   :div #'div
   :rdiv #'div
   :reciprocal #'(lambda (s) (div 1 s))
   :mult #'mult
   :sub #'sub
   :negate #'(lambda (s) (mul -1 s))
   :psqrt #'(lambda (s) (ftake 'mexpt s (div 1 2)))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (eq t (meqp s 0)))
   :adjoint #'(lambda (s) (ftake '$conjugate s))
   :mring-to-maxima #'cl:identity
   :maxima-to-mring #'(lambda (s) ($rat s))))
		
(setf (get '$crering 'ring) *crering*)

(defparameter *generalring*
  (make-mring
   :name '$generalring
   :coerce-to-lisp-float nil
   :abs #'$cabs
   :great #'(lambda (a b) (declare (ignore a)) (eq t (meqp b 0)))
   :add #'(lambda (a b) (add a b))
   :div #'(lambda (a b) (div a b))
   :rdiv #'(lambda (a b) (div a b))
   :reciprocal #'(lambda (s) (div 1 s))
   :mult #'(lambda (a b) (mul a b))
   :sub #'(lambda (a b) (sub a b))
   :negate #'(lambda (a) (mul -1 a))
   :psqrt #'(lambda (s) (ftake 'mexpt s (div 1 2)))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (eq t (meqp s 0)))
   :adjoint #'(lambda (s) (ftake '$conjugate s))
   :mring-to-maxima #'cl:identity
   :maxima-to-mring #'cl:identity))

(setf (get '$generalring 'ring) *generalring*)

(defparameter *bigfloatfield*
  (make-mring
   :name '$bigfloatfield
   :coerce-to-lisp-float #'(lambda (s) 
			     (setq s ($rectform ($float s)))
			     (complex ($realpart s) ($imagpart s)))
   
   :abs #'$cabs
   :great #'mgrp
   :add #'(lambda (a b) ($rectform (add a b)))
   :div #'(lambda (a b) ($rectform (div a b)))
   :rdiv #'(lambda (a b) ($rectform (div a b)))
   :reciprocal #'(lambda (s) (div 1 s))
   :mult #'(lambda (a b) ($rectform (mul a b)))
   :sub #'(lambda (a b) ($rectform (sub a b)))
   :negate #'(lambda (a) (mul -1 a))
   :psqrt #'(lambda (s) (if (mlsp s 0) nil (ftake '%sqrt s)))
   :add-id #'(lambda () bigfloatzero)
   :mult-id #'(lambda () bigfloatone)
   :fzerop #'(lambda (s) (like s bigfloatzero))
   :adjoint #'cl:identity
   :mring-to-maxima #'cl:identity
   :maxima-to-mring #'(lambda (s) 
			(setq s ($rectform ($bfloat s)))
			(if (complex-number-p s #'bigfloat-or-number-p) s
				(merror (intl:gettext "Unable to convert matrix entry to a big float"))))))

(setf (get '$bigfloatfield 'ring) *bigfloatfield*)

;; --- *gf-rings* --- (used by src/numth.lisp) ------------------------------ ;;
;;                                                                            ;;
(defparameter *gf-coeff-ring*
  (make-mring
   :name 'gf-coeff-ring
   :coerce-to-lisp-float nil
   :abs #'gf-cmod
   :great #'(lambda (a b) (declare (ignore a)) (= 0 b))
   :add #'gf-cplus-b
   :div #'(lambda (a b) (gf-ctimes a (gf-cinv b)))
   :rdiv #'(lambda (a b) (gf-ctimes a (gf-cinv b)))
   :reciprocal #'gf-cinv
   :mult #'gf-ctimes
   :sub #'(lambda (a b) (gf-cplus-b a (gf-cminus-b b)))
   :negate #'gf-cminus-b
   :psqrt #'(lambda (a) (let ((rs (zn-nrt a 2 *gf-char*))) (when rs (car rs))))
   :add-id #'(lambda () 0)
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (= 0 s))
   :adjoint nil
   :mring-to-maxima #'cl:identity
   :maxima-to-mring #'cl:identity ))
;;
(setf (get 'gf-coeff-ring 'ring) *gf-coeff-ring*)
;;
(defparameter *gf-ring*
  (make-mring
   :name 'gf-ring
   :coerce-to-lisp-float nil
   :abs #'gf-mod
   :great #'(lambda (a b) (declare (ignore a)) (null b))
   :add #'gf-plus
   :div #'(lambda (a b) (gf-times a (gf-inv b *gf-red*) *gf-red*))
   :rdiv #'(lambda (a b) (gf-times a (gf-inv b *gf-red*) *gf-red*))
   :reciprocal #'(lambda (a) (gf-inv a *gf-red*))
   :mult #'(lambda (a b) (gf-times a b *gf-red*))
   :sub #'(lambda (a b) (gf-plus a (gf-minus b)))
   :negate #'gf-minus
   :psqrt #'(lambda (a) 
              (let ((rs (gf-nrt-exit (gf-nrt a 2 *gf-red* *gf-ord*)))) 
                (when rs (cadr rs)) ))
   :add-id #'(lambda () nil)
   :mult-id #'(lambda () '(0 1))
   :fzerop #'(lambda (s) (null s))
   :adjoint nil
   :mring-to-maxima #'gf-x2p
   :maxima-to-mring #'gf-p2x ))
;;
(setf (get 'gf-ring 'ring) *gf-ring*)
;;
(defparameter *ef-ring*
  (make-mring
   :name 'ef-ring
   :coerce-to-lisp-float nil
   :abs #'gf-mod
   :great #'(lambda (a b) (declare (ignore a)) (null b))
   :add #'gf-plus
   :div #'(lambda (a b) (gf-times a (gf-inv b *ef-red*) *ef-red*))
   :rdiv #'(lambda (a b) (gf-times a (gf-inv b *ef-red*) *ef-red*))
   :reciprocal #'(lambda (a) (gf-inv a *ef-red*))
   :mult #'(lambda (a b) (gf-times a b *ef-red*))
   :sub #'(lambda (a b) (gf-plus a (gf-minus b)))
   :negate #'gf-minus
   :psqrt #'(lambda (a) 
              (let ((rs (gf-nrt-exit (gf-nrt a 2 *ef-red* *ef-ord*)))) 
                (when rs (cadr rs)) ))
   :add-id #'(lambda () nil)
   :mult-id #'(lambda () '(0 1))
   :fzerop #'(lambda (s) (null s))
   :adjoint nil
   :mring-to-maxima #'gf-x2p
   :maxima-to-mring #'gf-p2x ))

(setf (get 'ef-ring 'ring) *ef-ring*)
;;                                                                            ;;
;; -------------------------------------------------------------------------- ;;

(defun fp-abs (a)
  (list (abs (first a)) (second a)))

(defun fp+ (a b)
  (cond ((= (first a) 0.0) b)
	((= (first b) 0.0) a)
	(t
	 (let ((s (+ (first a) (first b))))
	   (if (= 0.0 s) (merror (intl:gettext "floating point divide by zero")))
	   (list s (ceiling (+ 1
			       (abs (/ (* (first a) (second a)) s))
			       (abs (/ (* (first b) (second b)) s)))))))))
				
(defun fp- (a b)
  (cond ((= (first a) 0.0) (list (- (first b)) (second b)))
	((= (first b) 0.0) a)
	(t
	 (let ((s (- (first a) (first b))))
	   (if (= 0.0 s) (merror (intl:gettext "floating point divide by zero")))
	   (list s (ceiling (+ 1
			       (abs (/ (* (first a) (second a)) s))
			       (abs (/ (* (first b) (second b)) s)))))))))

(defun fp* (a b)
  (if (or (= (first a) 0.0) (= (first b) 0.0)) (list 0.0 0)
    (list (* (first a) (first b)) (+ 1 (second a) (second b)))))

(defun fp/ (a b)
  (if (= (first a) 0) (list 0.0 0)
    (list (/ (first a) (first b)) (+ 1 (second a) (second b)))))

(defun $addmatrices(fn &rest m)
  (mfuncall '$apply '$matrixmap `((mlist) ,fn ,@m)))

(defparameter *runningerror*
  (make-mring 
   :name '$runningerror
   :coerce-to-lisp-float #'(lambda (s) (if (consp s) (first s) s))
   :abs #'fp-abs
   :great #'(lambda (a b) (> (first a) (first b)))
   :add #'fp+
   :div #'fp/
   :rdiv #'fp/
   :reciprocal #'(lambda (s) (fp/ (list 1 0) s))
   :mult #'fp*
   :sub #'fp-
   :negate #'(lambda (s) (list (- (first s)) (second s)))
   :psqrt #'(lambda (s) (if (> (first s) 0) (list (cl:sqrt (first s)) (+ 1 (second s))) nil))
   :add-id #'(lambda () (list 0 0))
   :mult-id #'(lambda () (list 1 0))
   :fzerop #'(lambda (s) (like (first s) 0))
   :adjoint #'cl:identity
   :mring-to-maxima #'(lambda (s) `((mlist) ,@s))
   :maxima-to-mring #'(lambda (s) (if ($listp s) (cdr s) (list ($float s) 1)))))

(setf (get '$runningerror 'ring) *runningerror*)

(defparameter *noncommutingring* 
  (make-mring
   :name '$noncommutingring
   :coerce-to-lisp-float nil
   :abs #'$cabs
   :great #'(lambda (a b) (declare (ignore a)) (not (invertible-matrixp b '$noncommutingring)))
   :add #'(lambda (a b) (add a b))
   :div #'(lambda (a b) (progn
			  (let (($matrix_element_mult ".")
				($matrix_element_transpose '$transpose))
			    (setq b (if ($matrixp b) ($invert_by_lu b '$noncommutingring)
				      (take '(mncexpt) b -1)))
			    (take '(mnctimes) a b))))
   
   :rdiv #'(lambda (a b) (progn
			   (let (($matrix_element_mult ".")
				 ($matrix_element_transpose '$transpose))
			     (setq b (if ($matrixp b) ($invert_by_lu b '$noncommutingring)
				       (take '(mncexpt) b -1)))
			     (take  '(mnctimes) b a))))
				

   :reciprocal #'(lambda (s) (progn
			       (let (($matrix_element_mult ".")
				     ($matrix_element_transpose '$transpose))
				 (if ($matrixp s) ($invert_by_lu s '$noncommutingring) 
				   (take '(mncexpt) s -1)))))

   :mult #'(lambda (a b) (progn 
			   (let (($matrix_element_mult ".")
				 ($matrix_element_transpose '$transpose))
			     (take  '(mnctimes) a b))))


   :sub #'(lambda (a b) (sub a b))
   :negate #'(lambda (a) (mul -1 a))
   :add-id #'(lambda () 0)
   :psqrt #'(lambda (s) (take '(%sqrt) s))
   :mult-id #'(lambda () 1)
   :fzerop #'(lambda (s) (not (invertible-matrixp s '$noncommutingring)))
   :adjoint #'(lambda (s) ($transpose (take '($conjugate) s)))
   :mring-to-maxima #'cl:identity
   :maxima-to-mring #'cl:identity))

(setf (get '$noncommutingring 'ring) *noncommutingring*)

(defun ring-eval (e fld)
  (let ((fadd (mring-add fld))
	(fnegate (mring-negate fld))
	(fmult (mring-mult fld))
	(fdiv (mring-div fld))
	(fabs (mring-abs fld))
	(fconvert (mring-maxima-to-mring fld)))
    
    (cond ((or ($numberp e) (symbolp e)) 
	   (funcall fconvert (meval e)))
	  
	  ;; I don't think an empty sum or product is possible here. If it is, append
	  ;; the appropriate initial-value to reduce. Using the :inital-value isn't
	  ;; a problem, but (fp* (a b) (1 0)) --> (a (+ b 1)).  A better value is
	  ;; (fp* (a b) (1 0)) --> (a b).

	  ((op-equalp e 'mplus) 
	   (reduce fadd (mapcar #'(lambda (s) (ring-eval s fld)) (margs e)) :from-end t))
	  
	  ((op-equalp e 'mminus)
	   (funcall fnegate (ring-eval (first (margs e)) fld)))
	  
	  ((op-equalp e 'mtimes) 
	   (reduce fmult (mapcar #'(lambda (s) (ring-eval s fld)) (margs e)) :from-end t))
		 
	  ((op-equalp e 'mquotient)
	   (funcall fdiv (ring-eval (first (margs e)) fld)(ring-eval (second (margs e)) fld)))
	   
	  ((op-equalp e 'mabs) (funcall fabs (ring-eval (first (margs e)) fld)))
	
	  ((and (or (eq (mring-name fld) '$floatfield) (eq (mring-name fld) '$complexfield))
		(consp e) (consp (car e)) (gethash (mop e) *flonum-op*))
	   (apply (gethash (mop e) *flonum-op*) (mapcar #'(lambda (s) (ring-eval s fld)) (margs e))))
	  
	  (t (merror (intl:gettext "Unable to evaluate ~:M in the ring '~:M'") e (mring-name fld))))))
  
(defmspec $ringeval (e)
  (let ((fld (get (or (car (member (meval (nth 2 e)) $%mrings)) '$generalring) 'ring)))
    (funcall (mring-mring-to-maxima fld) (ring-eval (nth 1 e) fld))))
