



make meval1 do following

(cond ((atom form)
       (if (symbolp form) (if mrefchecking (mrefcheck form)
                              (if (boundp form) (symbol-value form)
				;; unbound variable is itself.
				form))))
      ;; handle all the things which eval their args in this way
      ;; dispatching way.   This covers things like a call to
      ;; a lisp function.  A call to an mexpr will also pass this
      ;; way, since we will have at definition time done
      ;; (fset '$foo #'mexpr)
      ;; and that guy will be responsible for actually doing the call
      ;; he will have all he needs accessible from the *last-form*
      ;; and he will have his args already eval'd. 
      ((fboundp (caar form))
       (let ((tem (cdr form)) a b c d)
	 (return-from
	  meval1 
	  (cond ((null tem)
		 (setq *last-form* form)
		 (funcall (caar form))
		 )
		((progn (setq a (meval1 (car tem)) tem (cdr tem))(null tem))
		 (setq *last-form* form)
		 (funcall (caar form) a))
		((progn (setq b (meval1 (car tem)) tem (cdr tem))(null tem))
		 (setq *last-form* form)
		 (funcall (caar form) a b))
		((progn (setq c (meval1 (car tem)) tem (cdr tem))(null tem))
		 (setq *last-form* form)
		 (funcall (caar form) a b c))
		((progn (setq d (meval1 (car tem)) tem (cdr tem))(null tem))
		 (setq *last-form* form)
		 (funcall (caar form) a b c d))
		(t (let ((rest (mapcar 'meval1 tem)))
		     (setq *last-form* form)
		     (apply (caar form) a b c d rest)))))))
      ;; the special forms...
      ;; msetq these could be done using the symbol-value cell of
      ;; have a global 
      (defvar *cdr-of-mfexpr* '(nil))
      ;; and  (setf mset (cons #'(lambda (x) (handle-mset x)) *cdr-of-mfexpr*))
            ;; 
      ((and (boundp (caar form))
	    (let ((tem (symbol-value (caar form))))
	      (cond ((eq (cdr tem) *cdr-of-mfexpr*)
		     ;; actually maybe not necessary since
		     ;; the child has it and should be responsible..
		     ;;(setq *last-form* form)
		     (return-from meval1 	     (funcall (car tem) form))
		nil)))))
      
      
       
	     
	     
	      
	      