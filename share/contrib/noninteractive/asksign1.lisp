(defvar $e% nil)

(defmvar $no_questions t)

;; ADAPTED FROM $CATCH IN SRC/SUPRV1.LISP
;; CLEARSIGN CALL COPIED FROM MEVAL* IN SRC/SUPRV1.LISP
(defun meval* ($e%)
  (declare (special $e%))
  (let
    ((mcatch (cons bindlist loclist)))
    (prog1
      (catch 'mcatch (mfuncall '|$meval1|))
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
			  (errset (meval*n (cdr form)) lisperrprint))))
	(errlfun1 errcatch))
    (if (and (consp (car ret)) (eq (caar (car ret)) 'merror))
      (progn
        (apply 'mtell (cdr (car ret)))
        (fresh-line)
        '((mlist)))
      (cons '(mlist) ret))))

;; Replaces ENSURE-SIGN in compar.lisp
(defvar *interactive-ensure-sign* (symbol-function 'ensure-sign))
(defun ensure-sign (expr &optional domain)
  (if $no_questions
      (or (match-sign sign domain expr)
          (meval `(($throw) '(($asksign) ,expr ,(or domain '$pnz)))))
      (funcall *interactive-ensure-sign*)))

;;; Asks the user a question about the property of an object.
;;; Returns only $yes, $no or $unknown.
(defun ask-prop (object property fun-or-number)
  (if $no_questions (meval `(($throw) '(($askprop) ,object ,property))))

  (if fun-or-number (setq fun-or-number (list '| | fun-or-number)))
  (do ((end-flag) (answer))
      (end-flag (cond ((member answer '($yes |$Y| |$y|) :test #'eq) '$yes)
		      ((member answer '($no |$N| |$n|) :test #'eq) '$no)
		      ((member answer '($unknown $uk) :test #'eq) '$unknown)))
    (setq answer (retrieve
		  `((mtext) "Is " ,object 
		    ,(if (member (char (symbol-name property) 0)
				 '(#\a #\e #\i #\o #\u) :test #'char-equal)
			 " an "
			 " a ")
		    ,property ,@fun-or-number "?")
		  nil))
    (cond ((member answer '($yes |$Y| |$y| |$N| |$n| $no $unknown $uk) :test #'eq)
	   (setq end-flag t))
	  (t (mtell "~%Acceptable answers are Yes, Y, No, N, Unknown, Uk~%")))))
