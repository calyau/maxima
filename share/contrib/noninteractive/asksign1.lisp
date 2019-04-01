(defvar $e% nil)

(defmvar $no_questions t)

;; ADAPTED FROM $CATCH IN SRC/SUPRV1.LISP
;; CLEARSIGN CALL COPIED FROM MEVAL* IN SRC/SUPRV1.LISP
(defun meval* ($e%)
  (declare (special $e%))
  (let
    ((mcatch (cons bindlist loclist)))
    (prog1
      (catch 'mcatch (mfuncall '|$meval1| $e%))
      ;; Clear the facts from asksign and friends.
      (clearsign)
      (errlfun1 mcatch))))

;; ADAPTED FROM MEVALN IN SRC/MLISP.LISP
;; CALL MEVAL* INSTEAD OF MEVAL
(defmfun meval*n (l) ;; called in a few places externally.
  (do ((body l (cdr body))
       ($%% '$%%))
      ((null (cdr body)) (meval* (car body)))
    (setq $%% (meval (car body)))))

;; ADAPTED FROM $ERRCATCH IN SRC/SUPRV1.LISP
;; CALL MEVAL*N INSTEAD OF MEVALN
;; ALSO SPECIAL CASE FOR MERROR
(defmspec $errcatch (form)
  (let ((errcatch (cons bindlist loclist)) ret)
    (if (null (setq ret (let (*mdebug*)
			  (errset (meval*n (cdr form))))))
	(errlfun1 errcatch))
    (if (and (consp (car ret)) (eq (caar (car ret)) 'merror))
      (progn
        (apply 'mtell (cdr (car ret)))
        (fresh-line)
        '((mlist)))
      (cons '(mlist) ret))))

;; Replaces ENSURE-SIGN in compar.lisp
(defvar *interactive-ensure-sign* (symbol-function 'ensure-sign))
(defun ensure-sign (expr &optional domain squared)
  (if $no_questions
      (or (match-sign sign domain expr squared)
          (meval `(($throw) '(($asksign) ,expr ,(or domain '$pnz))))) ;; PUT SQUARED HERE AS WELL ??
      (funcall *interactive-ensure-sign*)))

;; Replaces ASK-PROP in askp.lisp
(defvar *real-ask-prop* (symbol-function 'ask-prop))
(defun ask-prop (object property fun-or-number)
  (declare (ignore fun-or-number))
  (if $no_questions
      (meval `(($throw) '(($askprop) ,object ,property)))
      (funcall *real-ask-prop*)))

;; Replaces $ASKEQUAL in src/compar.lisp
(defvar *real-askequal* (symbol-function '$askequal))
(defun $askequal (a b)
  (if $no_questions
    ;; Throw askequal(a, b) only if askequal would actually ask the user
    ;; (i.e., askequal can't figure out the answer by itself).
    ;; Following is adapted from $ASKEQUAL in src/compar.lisp.
    (let ((answer (meqp (sratsimp a) (sratsimp b))))
      (cond
        ((eq answer t) '$yes)
        ((eq answer nil) '$no)
        (t (meval `(($throw) '(($askequal) ,a ,b))))))
    (funcall *real-askequal* a b)))
