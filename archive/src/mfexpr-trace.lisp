(in-package "MAXIMA")

(defmacro mytrace (f)
  (cond ((symbolp f)
	 (cond ((fboundp f)
		(eval `(trace ,f)))
	       ((get f 'mfexpr*)
		(let ((new (make-symbol (symbol-name f))))
		  (setf (symbol-function new)
			(let ((tem (get f 'mfexpr*)))
			  (cond ((symbolp tem)
				 (symbol-function tem))
				(t tem))))
		  (setf (get f 'mfexpr*) new)
		  (list 'quote (eval `(trace ,new)))))))

	(t (format t "~%unknown item to trace"))))
		  
(defmacro myuntrace (sym &aux tem)
  (cond ((fboundp sym)   (eval `(untrace ,f)))
	(t (cond ((symbolp (setq tem (get sym 'mfexpr*)))
		  (eval `(untrace ,tem))
		  (setf tem
			(symbol-function tem)))))))
		    


