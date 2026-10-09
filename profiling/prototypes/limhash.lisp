;; Round 2. The limit cache LIMIT-ANSWERS keeps a hash code with each entry,
;; so lookups call ALIKE1 only on entries whose code matches. Entries become
;; (code key . value).
(in-package :maxima)

;; A hash code for E such that (ALIKE1 X Y) implies equal codes. It ignores
;; header flags, and looks at most DEPTH levels deep.
(defun alike1-hash (e depth)
  (cond ((or (symbolp e) (integerp e) (stringp e)) (sxhash e))
	((atom e) 1)
	((atom (car e)) 1)
	((or (member (caar e) '(mrat mpois bigfloat)) (zerop depth))
	 (sxhash (caar e)))
	(t (let ((h (sxhash (caar e))))
	     (do ((l (cdr e) (cdr l)))
		 ((atom l) h)
	       (setq h (logand most-positive-fixnum
			       (+ (* 31 h) (alike1-hash (car l) (1- depth))))))))))

(defun limit-answer (key)
  (let ((code (alike1-hash key 4)))
    (dolist (entry limit-answers (values nil code))
      (when (and (eql (car entry) code) (alike1 key (cadr entry)))
	(return (values (cddr entry) code))))))

(defun putlimval (e v &aux exp)
  (setq exp `((%limit) ,e ,var ,val))
  (multiple-value-bind (old code) (limit-answer exp)
    (unless old
      (push (list* code exp v) limit-answers)))
  v)

(defun getlimval (e)
  (values (limit-answer (list '(%limit) e var val))))
