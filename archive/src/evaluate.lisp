(in-package "MAXIMA")

;; echo "integrate(1/(1+x^2),x);" | maxima  -load /home/wfs/cvs/maxima/src/evaluate.lisp -batch
;; echo "separator:\"\";integrate(1/(1+x^3),x);2+4;" | maxima  -load /home/wfs/cvs/maxima/src/evaluate.lisp -batch

(setq $separator "xxxxxx")

(defun read-eval ()
   (let ((eof '(nil)) tem val $display2d  more)
    (sloop  while (not (eq (setq tem (dbm-read *terminal-io* nil eof)) eof))
        do
	(if more ($print $separator) (setq more t))
	($print (meval* (third tem)))
         
     )

   ($quit)
))

(read-eval)