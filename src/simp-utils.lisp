;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;
;;; This file is part of the Maxima computer algebra system
;;; (https://sourceforge.net/projects/maxima/)
;;;
;;; Maxima is copyrighted by its authors and licensed under the GNU
;;; General Public License.  See COPYING and AUTHORS for details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;; These are common utililites needed by both simp.lisp and mutils.lisp.

(defun delsimp (e)
  (remove 'simp e))

(defmfun ($bfloatp :inline-impl t) (x)
  "Returns true if X is a bigfloat"
  (and (consp x)
       (consp (car x))
       (eq (caar x) 'bigfloat)))

(declaim (inline zerop1))
(defun zerop1 (x)
  "Returns non-NIL if X is Lisp number or bfloat that is equal to 0"
  (cond
    ((numberp x) (zerop x))
    (($bfloatp x) (= 0 (cadr x)))))

(defun ratdisrep (e)
  (simplifya (locally 
                 (declare (notinline $ratdisrep))
               ($ratdisrep e))
             nil))

(defun specdisrep (e)
  (cond ((eq (caar e) 'mrat)
         (ratdisrep e))
	(t
         (locally
             (declare (notinline $outofpois))
           ($outofpois e)))))

(defun specrepcheck (e)
  (if (specrepp e)
      (specdisrep e)
      e))


;; Compares two Macsyma expressions ignoring SIMP flags and all other
;; items in the header except for the ARRAY flag.

(defun like (x y)
  (alike1 (specrepcheck x) (specrepcheck y)))

;; Trivial function used only in ALIKE1.
;; Should be defined as an open-codable subr.

(defmacro memqarr (l)
  `(if (member-eq 'array ,l) t))

(defun lisp-vector-alike1 (x y)
  (let ((lx (length x)))
    (when (eql lx (length y))
      (lisp-array-elements-alike1 x y lx))))

(defun lisp-array-alike1 (x y)
  (when (equal (array-dimensions x) (array-dimensions y))
    (lisp-array-elements-alike1 x y (array-total-size x))))

(defun lisp-array-elements-alike1 (x y n)
  (dotimes (i n t)
    (unless (alike1 (row-major-aref x i) (row-major-aref y i))
      (return-from lisp-array-elements-alike1 nil))))

(defun alike1 (x y)
  ;; Clauses are ordered based on frequency of the case
  ;; cons, integer, and symbol are very common
  ;; everything else is rare
  (cond ((eq x y) t)
        ((consp x)
         (let (car-x car-y op)
         (if (and (consp y)
                  (not (atom (setq car-x (car x))))
                  (not (atom (setq car-y (car y))))
                  (eq (setq op (car car-x)) (car car-y)))
             (cond
              ((eq op 'mrat) (like x y))
              ((eq op 'mpois) (equal (cdr x) (cdr y)))
              ((eq op 'bigfloat)
                ;; Bigfloats need special treatment because their precision
                ;; and an optional DECIMAL flag are stored in the CAR,
                ;; which would otherwise be ignored.
                ;; A bigfloat looks like this, [...] means optional:
                ;; ((BIGFLOAT [SIMP] <PRECISION> [DECIMAL]) <MANTISSA> <EXPONENT>)
                ;; Compare mantissas and exponents first.
                (when (and (= (cadr x) (cadr y)) (= (caddr x) (caddr y)))
                  ;; Mantissas and exponents are the same.
                  ;; If the CARs are EQ (see BCONS), we're done. Otherwise, we
                  ;; still need to compare precision and maybe radix (binary/decimal).
                  ;; If there's a SIMP flag, it must be ignored.
                  (if (eq car-x car-y)
                  t
                  (let ((rest-x (if (eq 'simp (cadar x)) (cddar x) (cdar x)))
                        (rest-y (if (eq 'simp (cadar y)) (cddar y) (cdar y))))
                    (and (= (car rest-x) (car rest-y))
                         (eq (cadr rest-x) (cadr rest-y)))))))
              ;; General case: First check for CARs being EQ (see EQTEST).
              ;; If not, just check whether both have or don't have the ARRAY flag.
              ((or (eq car-x car-y) (eq (memqarr (cdar x)) (memqarr (cdar y))))
               (alike (cdr x) (cdr y)))
              (t nil))
           ;; (foo) and (foo) test non-alike because the car's aren't standard
           nil)))
        ((consp y) nil)
        ((or (symbolp x) (symbolp y)) nil)
        ((integerp x) (and (integerp y) (= x y)))
        ;; uncommon cases from here down
        ((floatp x) (and (floatp y) (= x y)))
        ((stringp x) (and (stringp y) (string= x y)))
        ((vectorp x) (and (vectorp y) (lisp-vector-alike1 x y)))
        ((arrayp x) (and (arrayp y) (lisp-array-alike1 x y)))
        (t nil)
        ))

;; Maps ALIKE1 down two lists.

(defun alike (x y)
  (do ((x x (cdr x)) (y y (cdr y))) ((atom x) (equal x y))
    (cond ((or (atom y) (not (alike1 (car x) (car y))))
	   (return nil)))))


;; Check if EXP is a multiple of VAR.
(defun multiplep (exp var)
  (let (c)
    (and (not (zerop1 exp))
         (not (zerop1 (setq c (coeff exp var 1))))
         (zerop1 (sub exp (mul var c))))))

(defun margs (form)
  (if (eq (caar form) 'mqapply)
      (cddr form)
      (cdr form)))

(defun atomchk (e fun 2ndp)
  (if (or (atom e) (eq (caar e) 'bigfloat))
      (merror (intl:gettext "~:M: ~Margument must be a non-atomic expression; found ~M") fun (if 2ndp "2nd " "") e)))

(defmfun $member (x e)
  (atomchk e '$member t)
  (setq x (specrepcheck x))
  (dolist (i (margs e))
    (when (alike1 x (specrepcheck i))
      (return t))))

