;;;; This file outlines a very basic Maxima-Intermediate Represendation-Python Transpiler, which is capable of handling:
;;;; 1. Function definition -> f(<list of variables>):=<expression of variables>;
;;;; 2. + operator -> <expression 1> + <expression 2> + <expression 3>
;;;; 3. sin operator -> sin(<expression>)
;;;; 4. exponentiation operator -> <expression 1> ^ <expression 2>
;;;; It has been tested on SBCL SLIME, with the internal S-exp representation of f(x,t):=x^2+sin(x)+t+t^2+sin(t);
;;;; transformed to Intermediate Representation:
;;;;     (DEF "F" ("X" "T") (SUM ((POW ($X 2)) (MATH.SIN ($T)) $T(POW ($T 2)) (MATH.SIN ($T)))))
;;;; transformed to Python:
;;;;     def F(X, T):
;;;;         return(pow(X, 2) + math.sin(T) + T + pow(T, 2) + math.sin(T))


;;; This is a very simple test whether the form f is a function definition
(defun func_defp (f)
  (cond
    ((and (consp f)
      (consp (car f)))
    (eq (caar f) 'mdefine))
    (t nil)))

;;; Test whether the form f is a sum
(defun sump (f)
  (cond
    ((and (consp f)
      (consp (car f))
      (consp (caar f)) (eq (caaar f) 'mplus)))
    (t nil)))

;;; Test whether the form f is an exponentiation
(defun expp (form)
  (cond
    ((and (consp form)) (eq (caar form) 'mexpt))))

;;; Test whether the form f is a sin operation
(defun sinp (form)
  (cond ((and (consp form)) (eq (caar form) '%sin))))
 
;;; A very simple Maxima S-exp -> Python IR converter
(defun m_ir (form)
  (cond
    ((func_defp form) 
     `(def
          ,(subseq (symbol-name (caaadr form)) 1)
          ,(mapcar (lambda (x) (subseq x 1)) (mapcar 'symbol-name (cdadr form)))
          ,(m_ir (cddr form))))
    ((sump form) `(sum ,(mapcar 'm_ir (cdar form))))
    ((expp form) `(pow ,(mapcar 'm_ir (cdr form))))
    ((sinp form) `(math.sin ,(mapcar 'm_ir (cdr form))))
    (t form)))

;;; Test whether form is an IR function definition
(defun defp (form)
  (cond
    ((consp form) (eq (car form) 'def))))

;;; Test whether form is an IR sum
(defun sump_ir (form)
  (cond
    ((and (consp form) (consp (car form))) (eq (caar form) 'sum))))

;;; A simple IR -> Python source function
(defun ir_p (form)
  (cond
    ((defp form) (format nil "def ~A(~A):~%    return(~A)" (cadr form) (comma-list (caddr form)) (ir_p (cdddr form))))
    ((sump_ir form) (format nil "~A" (sum-list (mapcar 'ir_p (cadar form)))))
    ((and (consp form) (consp (cadr form))) (format nil "~A(~A)" (string-downcase (string (car form))) (comma-list (mapcar 'ir_p (cadr form)))))
    ((symbolp form) (format nil "~A" (subseq (symbol-name form) 1)))
    (t (format nil "~A" form))))


;;; Driver function for the translator, calls the m_ir and then ir_p
(defun $transpile (form)
  (ir_p (m_ir form)))

;;;Adapted from http://cybertiggyr.com/fmt/fmt.pdf Creates a comma separated list string.
(defun comma-list (lst) (format nil "~{~A~#[~:;, ~]~}" lst))

;;; Adapted from above. Creates a + separated list.
;;; The 2 forms can be merged into a Macro.
(defun sum-list (lst) (format nil "~{~A~#[~:; + ~]~}" lst))
