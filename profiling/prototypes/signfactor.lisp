(in-package :maxima)
;; SIGN-MPLUS factors every sum whose sign it cannot decide. For a sum
;; of total degree 1 in its kernels, FACTOR can only pull out a numeric
;; content, so do just that instead of calling FACTOR.
(defun signfactor-kernelp (f)
  (or (atom f) (not (or (mexptp f) (mtimesp f) (mplusp f)))))
(defun signfactor-linear-coeffs (x)
  ;; List of the rational coefficients of the sum X if X is linear with
  ;; rational coefficients, else :NO.
  (let ((coeffs nil))
    (dolist (term (cdr x) coeffs)
      (cond ((or (integerp term) (ratnump term)) (push term coeffs))
            ((mnump term) (return :no))
            ((signfactor-kernelp term) (push 1 coeffs))
            ((and (mtimesp term) (null (cdddr term))
                  (or (integerp (cadr term)) (ratnump (cadr term)))
                  (signfactor-kernelp (caddr term)))
             (push (cadr term) coeffs))
            (t (return :no))))))
(defun signfactor-content (coeffs)
  (let ((g 0) (l 1))
    (dolist (c coeffs)
      (if (integerp c)
          (setq g (gcd g c))
          (setq g (gcd g (cadr c)) l (lcm l (caddr c)))))
    (if (= l 1) g (list '(rat simp) g l))))
(defun signfactor (x)
  (let (y (factored t) (coeffs (signfactor-linear-coeffs x)))
    (setq y (if (eq coeffs :no)
                (factor-if-small x)
                ;; Like FACTOR, make the coefficient of the greatest term,
                ;; the last one, positive. COEFFS is in reverse order.
                (let ((c (signfactor-content coeffs)))
                  (when (mminusp (car coeffs)) (setq c (neg c)))
                  (if (eql c 1)
                      x
                      ;; As FACTOR returns it: MUL would distribute -1.
                      (list '(mtimes simp) c
                            (addn (mapcar #'(lambda (term) (div term c)) (cdr x)) t))))))
    (cond ((or (mplusp y) (> (conssize y) 50.))
	   (setq sign '$pnz)
	   nil)
	  (t (sign y) nil))))
