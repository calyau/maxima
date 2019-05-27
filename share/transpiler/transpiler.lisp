(defparameter *maxima-direct-ir-map* (make-hash-table))
(setf (gethash 'mtimes *maxima-direct-ir-map*) '(op *))
(setf (gethash 'mplus *maxima-direct-ir-map*) '(op +))
(setf (gethash 'mexpt *maxima-direct-ir-map*) '(funcall pow))
(setf (gethash 'mfactorial *maxima-direct-ir-map*) '(funcall math.factorial))
(setf (gethash 'rat *maxima-direct-ir-map*) '(op /))
(setf (gethash 'msetq *maxima-direct-ir-map*) '(op =))
(setf (gethash 'mlist *maxima-direct-ir-map*) '(struct list))

(defparameter *maxima-special-ir-map* (make-hash-table))
(setf (gethash 'mdefine *maxima-special-ir-map*) 'func-def-to-ir)

(defun func-def-to-ir (form)
  `(func-def ,(maxima-to-ir (caaadr form)) ,(mapcar #'maxima-to-ir (cdadr form)) ,(maxima-to-ir (caddr form))))

(defun atom-to-ir (form)
  (cond
    ((not (symbolp form)) form)
    ((eq form '$%i) '(num 0 1)) ; iota complex number
    ((eq form '$%pi) '(num pi 0)) ; Pi
    ((eq form '$%e) '(num e 0)) ; Euler's Constant
    (t (list 'symbol (subseq (symbol-name form) 1)))
    ))

(defun cons-to-ir (form)
  (cond
    ((atom (caar form))
     (progn
       (setf type (gethash (caar form) *maxima-direct-ir-map*))
       (cond
	 (type
	  (append type (mapcar #'maxima-to-ir (cdr form))))
	 ((setf type (gethash (caar form) *maxima-special-ir-map*))
	  (funcall type form))
	 (t (cons 'no-convert form))
	 )))))

(defun maxima-to-ir (form)
  (cond
    ((atom form) (atom-to-ir form))
    ((and (consp form) (consp (car form))) (cons-to-ir form))
    (t (cons 'no-convert form))
    ))

(defun ir-to-python (form)
  (print form))

;;; Driver function for the translator, calls the maxima-to-ir and then ir-to-python
(defun $transpile (form)
  (ir-to-python (maxima-to-ir form)))

;;;Adapted from http://cybertiggyr.com/fmt/fmt.pdf Creates a comma separated list string.
(defun comma-list (lst) (format nil "~{~A~#[~:;, ~]~}" lst))

;;; Adapted from above. Creates a + separated list.x
;;; The 2 forms can be merged into a Macro.
(defun sum-list (lst) (format nil "~{~A~#[~:; + ~]~}" lst))

(defun $show_form (form)
  (print form))
*maxima-special-ir-map*
