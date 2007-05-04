(in-package :maxima)

(defun $listofei (e )
   (declare (special $d2 $lg $lexp))
	(setq $d2 (copy-tree (car e)))
	(setq $lg ())
	(setq $lexp ())
	(do ((lvar (caddr $d2) (cdr lvar))
	     (lg (cadddr $d2) (cdr lg))
	     (var))
	    ((null lvar)(setq $lg (cons '(mlist) $lg))
	    		(setq $lexp (cons '(mlist) $lexp))
			(setq $d2 (cons $d2  (cdr e))) )
	    (setq var (car lvar))
	    (cond ((and
		    (mexptp var)
		    (equal (cadr var) '$%e)
		    (mtimesp (caddr var))
		    (eq (cadr (caddr var)) '$%i)) 
		   (setq $lexp (cons var $lexp))
		   (setq var  (intern (format nil "$_~a" (car lg))))
		   (setq $lg (cons var $lg))
		   (rplaca lvar var)))))

#$trigrat(exp):=	block([e,n,d,lg,f,lexp,ls,d2,l2,alg,gcd1],
		declare(d2,special,lg,special,lexp,special),
		alg:algebraic,gcd1:gcd,
		algebraic:true,gcd:subres,
		e:ev(exp,exponentialize=true,ratsimp),
		e:rat(e),
		n:num(e),d:denom(e),
		listofei(d),
		l2:map(lambda([u,v],u^((hipow(d2,v)+lopow(d2,v))/2)),
				lexp,lg),
		f:if length(lexp)=0 then 1
		  else if length(lexp)=1 then part(l2,1)
		  else apply("*",l2),
  		n:rectform(ratexpand(n/f)),
	        d:rectform(ratexpand(d/f)),
		e:ratsimp(n/d,%i),
		algebraic:alg,gcd:gcd1,
		e)$

; written by D. Lazard, august 1988

