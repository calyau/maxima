# Proposed fix: csign of a function with no sign rule of its own is complex when an argument is

`BUG-csign-function-of-complex-argument.md` reports the problem: with `z` declared `complex`, `csign(gamma(z))`, `csign(tan(z))`, `csign(erfc(z))`, `csign(f(z))` and many more are `pnz`, real of unknown sign, and the `abs` rules of the simplifier, which are guarded by `csign`, act on them. This is a fix, prototyped by redefining the functions at runtime in a built image and run through the full test suite that way, and not yet committed to `src/`.

## Where the answer comes from

`sign` in `src/compar.lisp` dispatches on the operator of an application: a `sign-function` property (`mtimes`, `mplus`, `mexpt`, `%log`, `mabs`, `%sin`, `%cos`, `%gamma`, `$floor`, ...), then a declared `posfun` or `oddfun` kind, else `sign-any`. `sign-any` looks at the operator alone: declared `imaginary` or `complex` gives that answer, anything else goes to the fact database with `dcompare`, which knows nothing about `f(z)` and answers `pnz`. The arguments are never looked at. `sign-oddfun` ends in `sign-any` too, which is how `tan(z)` gets there, and `gamma-sign` in `src/csimp2.lisp` computes `csign` of its argument itself and then folds `complex` into `pnz` along with everything else it does not recognize.

## The fix

Four parts, all in complex mode only, so that `sign`, `is` and `asksign` in real mode do not change, except that a declared `posfun` or `oddfun` now applies to a subscripted function in both modes.

**1. `sign-any`** in `src/compar.lisp` consults the arguments when nothing else decided: an application, the database answering `pnz`, the function not declared `real`, and an argument that is complex or imaginary, is `complex`. The database comes first, so `assume(g(z) > 0)` still gives `pos`; `declare(f, real)`, which `featurep` already understands for a function, opts a user function out. A function is real-valued when `risplit`'s `real-valued` property says so. A symbol argument is complex when declared so. An expression is first tested by its shape, without asking anything, and the test claims real only where `csign` itself would: a real number; a symbol not declared complex or imaginary; an integer power of such an expression, or a power of `%e` or of a positive number with such an exponent; and an application whose operator is `real-valued`, or commutes with the conjugate, which a function real for a real argument does and which `sin`, `cos`, `gamma`, `erf` and the arithmetic operators are marked with in `src/conjugate.lisp`, or has no sign rule and no conjugate rule of its own, which `csign` takes as real for real arguments, with arguments and subscripts that pass the same test. Everything else, a root, a logarithm, `acosh`, a non-integer power, a declared complex function, is asked with `sign*`, the helper of `src/compar.lisp` that runs `sign1` under fresh bindings of the four sign specials, so that the description of `x` being built in them is left alone; lists, matrices and equations are passed over, and an error there, as `hstep(z)` raises one, counts as not known; a number is real and a string has no sign. The shape test is what keeps the cost down, and part 6 is what makes it sound.

Find this:

```lisp
 (let ((complex-kind (decl-complex-kind (if (atom x)
                                          x
                                          (if (mqapplyp x)
                                            (subfunname x)
                                            (caar x))))))
```

replace it with this:

```lisp
 (let* ((op (if (atom x) x (if (mqapplyp x) (subfunname x) (caar x))))
        (complex-kind (decl-complex-kind op)))
```

Then the last clause of the same `cond`. Find this:

```lisp
	(t
	 (dcompare x 0)
	 (if (and $assume_pos
		  (member sign '($pnz $pz $pn) :test #'eq)
		  (if $assume_pos_pred
		      (let ((*x* x))
			(declare (special *x*))
			(is '(($assume_pos_pred) *x*)))
		      (mapatom x)))
	     (setq sign '$pos))
	 (setq minus nil evens nil
	       odds (if (not (member sign '($pos $neg $zero) :test #'eq))
			(ncons x)))))))
```

replace it with this:

```lisp
	(t
	 (dcompare x 0)
	 (cond ((and *complexsign*
		     (not (atom x))
		     (eq sign '$pnz)
		     (not (or (get op 'real-valued) (kindp op '$real)))
		     (complex-argument-p x))
		;; An application with no rule of its own, about which the
		;; database knows nothing, of a function that is neither
		;; real-valued, as RISPLIT knows it, nor declared real, with
		;; a complex or imaginary argument: complex.
		(setq sign '$complex minus nil odds nil evens nil))
	       (t
		(if (and $assume_pos
			 (member sign '($pnz $pz $pn) :test #'eq)
			 (if $assume_pos_pred
			     (let ((*x* x))
			       (declare (special *x*))
			       (is '(($assume_pos_pred) *x*)))
			     (mapatom x)))
		    (setq sign '$pos))
		(setq minus nil evens nil
		      odds (if (not (member sign '($pos $neg $zero) :test #'eq))
			       (ncons x)))))))))
```

With these two helpers before `sign-any`:

```lisp
;; True when X is real by its shape, without asking csign, and only where
;; csign itself would answer so: a real number, a symbol not declared
;; complex or imaginary, an integer power of such an expression or a power
;; of %e or of a positive number with such an exponent, and an application
;; of an operator that is real-valued, or that commutes with the conjugate,
;; which a function that is real for a real argument does, or that has no
;; sign rule and no conjugate rule of its own, which csign takes as real
;; for real arguments, with arguments and subscripts that are real by the
;; same test.  Anything else, a root, a logarithm, acosh, a declared
;; complex function, is left to csign.
(defun surely-real-p (x)
  (cond ((realp x) t)
        ((symbolp x)
         (not (or (member x '($%i $infinity $und $ind))
                  (decl-complex-kind x))))
        ((atom x) nil)
        ((member (caar x) '(rat bigfloat)) t)
        ((specrepp x) nil)
        ((mexptp x)
         (and (surely-real-p (cadr x))
              (or (integerp (caddr x))
                  (and (or (eq (cadr x) '$%e)
                           (and (realp (cadr x)) (plusp (cadr x))))
                       (surely-real-p (caddr x))))))
        ((mbagp x) nil)
        (t
         (let ((op (if (mqapplyp x) (subfunname x) (caar x))))
           (and (symbolp op)
                (or (get op 'real-valued)
                    (and (not (decl-complex-kind op))
                         (or (get op 'commutes-with-conjugate)
                             (not (or (get op 'sign-function)
                                      (get op 'conjugate-function))))
                         (every #'surely-real-p
                                (if (mqapplyp x) (subfunsubs x) nil))
                         (every #'surely-real-p (margs x)))))))))

;; True when an argument of the application X, a subscript of a subscripted
;; function included, is complex or imaginary: a symbol when it is declared
;; so, an expression that is not a list, matrix or equation and not real by
;; its shape by SIGN*, which leaves the sign specials of the caller alone,
;; with an error there, as for hstep(z), counting as not known.  A number
;; is real, and a string or another atom has no sign.
(defun complex-argument-p (x)
  (some #'(lambda (arg)
            (cond ((symbolp arg) (decl-complex-kind arg))
                  ((and (consp arg) (not (mbagp arg)) (not (surely-real-p arg)))
                   (member (car (let (($errormsg nil)) (errcatch (sign* arg))))
                           '($complex $imaginary)))))
        (if (mqapplyp x)
            (append (subfunsubs x) (margs x))
            (margs x))))
```

`sign*` is the idiom of `src/compar.lisp` for a sign asked in the middle of computing another, in `sign-mexpt` among others; `gamma-sign` and `hstep-sign`, in other files, call `$csign` instead, which binds the specials as well.

**2. `gamma-sign`** in `src/csimp2.lisp` passes a complex or imaginary argument through. Find this:

```lisp
(let ((sgn ($csign (second x)))) ;; careful! x = ((%gamma) XXX)
		(setq sign
			  (cond ((eql sgn '$pos) '$pos)
				    ((or (eql sgn '$neg) (eql sgn '$pn)) '$pn)
				    (t '$pnz)))))
```

replace it with this:

```lisp
(let ((sgn ($csign (second x)))) ;; careful! x = ((%gamma) XXX)
		(setq sign
			  (cond ((eql sgn '$pos) '$pos)
				    ((or (eql sgn '$neg) (eql sgn '$pn)) '$pn)
				    ((and *complexsign* (member sgn '($complex $imaginary))) '$complex)
				    (t '$pnz)))))
```

**4. Subscripted functions.** `sign-any` already took the name of a subscripted function for the declarations, and `margs` gives the arguments of `f[1](z)` as it does for `f(z)`; the helper looks at the subscripts as well, so `g[z](x)` is complex for a complex `z`. Two things next to it were not subscript-aware and are made so, since they decide the same question: the lookup of a declared `posfun` or `oddfun` in `sign`, which read `(caar x)`, `mqapply` for a subscripted function, so that `p[1](x)` was `pnz` where `p(x)` is `pos`; and `sign-oddfun`, which took `(cadr x)` for the argument, the subscripted operator in that case. `sign-posfun` also answers `complex` for a complex argument in complex mode, so that `p(z)` and `p[1](z)` agree with each other and with the rest. In `src/compar.lisp`, in `sign`. Find this:

```lisp
	(t
	  (let ((kind (kind-any-of (caar x) '($posfun $oddfun))))
		(cond
		  ((eq kind '$posfun) (sign-posfun x))
		  ((eq kind '$oddfun) (sign-oddfun x))
		  (t (sign-any x)))))))
```

replace it with this:

```lisp
	(t
	  (let ((kind (kind-any-of (if (mqapplyp x) (subfunname x) (caar x))
	                           '($posfun $oddfun))))
		(cond
		  ((eq kind '$posfun) (sign-posfun x))
		  ((eq kind '$oddfun) (sign-oddfun x))
		  (t (sign-any x)))))))
```

Find this:

```lisp
(defun sign-posfun (xx)
  (declare (ignore xx))
  (setq sign '$pos
	minus nil
	odds nil
	evens nil))
```

replace it with this:

```lisp
(defun sign-posfun (xx)
  (setq sign (if (and *complexsign* (complex-argument-p xx)) '$complex '$pos)
	minus nil
	odds nil
	evens nil))
```

Find this:

```lisp
(defun sign-oddfun (x)
 (let ((kind (kind-any-of (caar x) '($increasing $decreasing))))
  (cond ((eq kind '$increasing)
         ; Take the sign of the argument
         (sign (cadr x)))
        ((eq kind '$decreasing)
         ; Take the sign of negative of the argument
         (sign (neg (cadr x))))
        (t
         ; If the sign of the argument is zero, then we're done (the sign of
         ; the function value is the same).  Otherwise, punt to SIGN-ANY.
         (sign (cadr x))
         (unless (eq sign '$zero)
           (sign-any x))))))
```

replace it with this:

```lisp
(defun sign-oddfun (x)
 (let ((kind (kind-any-of (if (mqapplyp x) (subfunname x) (caar x))
                          '($increasing $decreasing)))
       (arg (car (margs x))))
  (cond ((eq kind '$increasing)
         ; Take the sign of the argument
         (sign arg))
        ((eq kind '$decreasing)
         ; Take the sign of negative of the argument
         (sign (neg arg)))
        (t
         ; If the sign of the argument is zero, then we're done (the sign of
         ; the function value is the same).  Otherwise, punt to SIGN-ANY.
         (sign arg)
         (unless (eq sign '$zero)
           (sign-any x))))))
```

**5. `risplit` follows.** It reads the subscripted name for a declaration `complex` but took the subscripted operator itself, not a symbol, when it looked for `real-valued` or a declaration `real`, so `rectform(f[1](z))` gave nouns for an `f` declared real where `csign` now answers `pnz`. In `src/rpart.lisp`, in `risplit`. Find this:

```lisp
          ((or (safe-get (mop l) 'real-valued)
               (decl-realp (mop l)))
           ;; Simplification for a real-valued function
           (cons l 0))
```

replace it with this:

```lisp
          ((let ((op (if (mqapplyp l) (subfunname l) (caar l))))
             (or (safe-get op 'real-valued)
                 (decl-realp op)))
           ;; Simplification for a real-valued function, a subscripted one
           ;; under its name, as csign takes it.
           (cons l 0))
```

With it `rectform(f[1](z))` is `f[1](z)` for an `f` declared real, `realpart(f[1](z))` is `f[1](z)` and `imagpart(f[1,2](z, x))` is 0, while `g[1](z)` and `li[2](z)` keep their nouns. For `tests/rtest16.mac`, in the `rectform` block before the final facts check:

```
(declare(f, real), declare(z, complex), [rectform(f[1](z)), realpart(f[1](z)), imagpart(f[1,2](z, x))]);
[f[1](z), f[1](z), 0];

(remove(f, real), remove(z, complex), 0);
0;
```

**6. `sin`, `cos` and `sinc` decide by the argument.** Their sign rules decided whether the value is complex by taking `rectform` of the argument and looking at its imaginary part. For an unknown function that part is the noun `imagpart(f(x))`, so `csign(sin(f(x)))` was `complex` while `csign(f(x))` was `pnz`: `csign` contradicted itself one level up, and a shape test that trusts `sin` of a real argument contradicted `csign` with it, as the check below found. The three rules now take `sign*` of the argument once, at the top, the helper of `src/compar.lisp` that returns a sign and leaves the four sign specials alone; a complex or imaginary argument in complex mode gives `complex`, and otherwise the sign already in hand settles one bound of the interval test for free, `x >= 0` gives `x >= -%pi` and `x <= 0` gives `x <= %pi`, so only the other bound is asked with `mgqp`. The imaginary part `y` that the old code tested is gone with the `rectform`. The interval logic that gives `sin(x)` a sign for `0 < x < %pi` is otherwise as it was, and every branch now sets the four specials itself to describe the sine, where the old code called `sign` on the argument inside the interval branch and left the rest as it found them; `asksign(sin(y))` still stores a fact about `sin(y)`. On the unmodified build and with the rewrite, a probe of 32 arguments, 17 assumed intervals and 5 `asksign` questions differs in exactly the four intended values, `sin`, `cos` and `sinc` of an unknown function. In `src/compar.lisp`. Find this:

```lisp
(defun sign-sin (e) ; e = sin(x)
     (let ((x (cadr e)) (y 0))
       ;; When *complexsign* is true, find the rectangular form of 
       ;; the argument to sin.
       (when *complexsign*
          (setq x (risplit x))
          (setq y (cdr x)
                x (car x)))
       (cond 
             ;; When y = 0 and -%pi <= x <= %pi, sign(sin(x)) = sign(x) 
             ((and (eql y 0)
                   (eq t (mgqp x (mul -1 '$%pi))) 
                   (eq t (mgqp '$%pi x)))
                (sign x)
                ;; sin(x) = 0 at the closed endpoints x = -%pi and %pi, so a
                ;; strict interior sign weakens to include zero when x can reach
                ;; the nearer endpoint; a nonzero-but-unsigned x ($pn) that can
                ;; reach an endpoint likewise weakens to $pnz.
                (cond ((and (eq sign '$pos) (not (eq t (mgrp '$%pi x))))
                       (setq sign '$pz odds (ncons e) evens nil minus nil))
                      ((and (eq sign '$neg) (not (eq t (mgrp x (mul -1 '$%pi)))))
                       (setq sign '$nz odds (ncons e) evens nil minus nil))
                      ((eq sign '$pn)
                       (unless (and (eq t (mgrp '$%pi x))
                                    (eq t (mgrp x (mul -1 '$%pi))))
                         (setq sign '$pnz))
                       (setq odds (ncons e) evens nil minus nil))
                      ((member sign '($pz $nz $pnz))
                       (setq odds (ncons e) evens nil minus nil))))
              ;; When *complexsign* is true & y # 0, set sign to complex.
              ;; To test y # 0, we'll use (not (eql y 0)))
              ((and *complexsign* (not (eql y 0)))
                (setf sign '$complex))
			        (t (setf sign '$pnz))))
		nil)
```

replace it with this:

```lisp
(defun sign-sin (e) ; e = sin(x)
     (let* ((x (cadr e))
            (s (sign* x)))
       (cond
             ;; When *complexsign* is true and x is complex or imaginary,
             ;; so is sin(x).
             ((and *complexsign* (member s '($complex $imaginary)))
                (setq sign '$complex odds (ncons e) evens nil minus nil))
             ;; When -%pi <= x <= %pi, sign(sin(x)) = sign(x).  The sign of
             ;; x settles one bound: x >= 0 gives x >= -%pi, and x <= 0
             ;; gives x <= %pi.
             ((and (or (member s '($pos $pz $zero))
                       (eq t (mgqp x (mul -1 '$%pi))))
                   (or (member s '($neg $nz $zero))
                       (eq t (mgqp '$%pi x))))
                ;; sin(x) = 0 at the closed endpoints x = -%pi and %pi, so a
                ;; strict interior sign weakens to include zero when x can reach
                ;; the nearer endpoint; a nonzero-but-unsigned x ($pn) that can
                ;; reach an endpoint likewise weakens to $pnz.
                (setq sign (cond ((and (eq s '$pos) (not (eq t (mgrp '$%pi x))))
                                  '$pz)
                                 ((and (eq s '$neg)
                                       (not (eq t (mgrp x (mul -1 '$%pi)))))
                                  '$nz)
                                 ((and (eq s '$pn)
                                       (not (and (eq t (mgrp '$%pi x))
                                                 (eq t (mgrp x (mul -1 '$%pi))))))
                                  '$pnz)
                                 (t s))
                      odds (if (member sign '($pos $neg $zero)) nil (ncons e))
                      evens nil
                      minus nil))
             (t (setq sign '$pnz odds (ncons e) evens nil minus nil))))
     nil)
```

Find this:

```lisp
(defun sign-cos (e) ; e = cos(x)
     (let ((x (cadr e)) (y 0))
       ;; When *complexsign* is true, find the rectangular form of 
       ;; the argument to cos.
       (when *complexsign* 
          (setq x (risplit x))
          (setq y (cdr x)
                x (car x)))
       (cond 
          ;; When y = 0 and -%pi/2 <= x <= 3 %pi/2, sign(cos(x)) = sign(%pi/2-x)
          ((and (eql y 0)
                (eq t (mgqp x (div '$%pi -2))) 
                (eq t (mgqp (div (mul 3 '$%pi) 2) x)))
            (sign (sub (div '$%pi 2) x))
            ;; cos(x) = 0 at the closed endpoints x = -%pi/2 and 3*%pi/2, so a
            ;; strict interior sign weakens to include zero when x can reach
            ;; that endpoint; a nonzero-but-unsigned x ($pn) that can reach an
            ;; endpoint likewise weakens to $pnz.
            (cond ((and (eq sign '$pos) (not (eq t (mgrp x (div '$%pi -2)))))
                   (setq sign '$pz odds (ncons e) evens nil minus nil))
                  ((and (eq sign '$neg) (not (eq t (mgrp (div (mul 3 '$%pi) 2) x))))
                   (setq sign '$nz odds (ncons e) evens nil minus nil))
                  ((eq sign '$pn)
                   (unless (and (eq t (mgrp x (div '$%pi -2)))
                                (eq t (mgrp (div (mul 3 '$%pi) 2) x)))
                     (setq sign '$pnz))
                   (setq odds (ncons e) evens nil minus nil))
                  ((member sign '($pz $nz $pnz))
                   (setq odds (ncons e) evens nil minus nil))))
          ((and *complexsign* (not (eql y 0)))
              (setf sign '$complex))
			    (t (setf sign '$pnz))))
		nil)
```

replace it with this:

```lisp
(defun sign-cos (e) ; e = cos(x)
     (let* ((x (cadr e))
            (s (sign* x)))
       (cond
          ;; When *complexsign* is true and x is complex or imaginary, so
          ;; is cos(x).
          ((and *complexsign* (member s '($complex $imaginary)))
             (setq sign '$complex odds (ncons e) evens nil minus nil))
          ;; When -%pi/2 <= x <= 3 %pi/2, sign(cos(x)) = sign(%pi/2-x).  The
          ;; sign of x settles one bound: x >= 0 gives x >= -%pi/2, and
          ;; x <= 0 gives x <= 3 %pi/2.
          ((and (or (member s '($pos $pz $zero))
                    (eq t (mgqp x (div '$%pi -2))))
                (or (member s '($neg $nz $zero))
                    (eq t (mgqp (div (mul 3 '$%pi) 2) x))))
            (setq s (sign* (sub (div '$%pi 2) x)))
            ;; cos(x) = 0 at the closed endpoints x = -%pi/2 and 3*%pi/2, so a
            ;; strict interior sign weakens to include zero when x can reach
            ;; that endpoint; a nonzero-but-unsigned x ($pn) that can reach an
            ;; endpoint likewise weakens to $pnz.
            (setq sign (cond ((and (eq s '$pos) (not (eq t (mgrp x (div '$%pi -2)))))
                              '$pz)
                             ((and (eq s '$neg)
                                   (not (eq t (mgrp (div (mul 3 '$%pi) 2) x))))
                              '$nz)
                             ((and (eq s '$pn)
                                   (not (and (eq t (mgrp x (div '$%pi -2)))
                                             (eq t (mgrp (div (mul 3 '$%pi) 2) x)))))
                              '$pnz)
                             (t s))
                  odds (if (member sign '($pos $neg $zero)) nil (ncons e))
                  evens nil
                  minus nil))
          (t (setq sign '$pnz odds (ncons e) evens nil minus nil))))
     nil)
```

And in `src/sinc.lisp`. Find this:

```lisp
(defun sign-sinc (e) ; e = sinc(x)
     (let ((x (cadr e)) (y 0))
       ;; When *complexsign* is true, find the rectangular form of 
       ;; the argument to sin.
       (when *complexsign* 
          (setq x (risplit x))
          (setq y (cdr x)
                x (car x)))
       (cond 
             ;; When y = 0 and -%pi < x < %pi, sign(sinc(x)) = $pos
             ((and (eql y 0)
                   (eq t (mgrp x (mul -1 '$%pi))) 
                   (eq t (mgrp '$%pi x)))
                (setf sign '$pos))

            ;; When y = 0 and -%pi <= x <= %pi, sign(sinc(x)) = $pz
             ((and (eql y 0)
                   (eq t (mgqp x (mul -1 '$%pi))) 
                   (eq t (mgqp '$%pi x)))
                (setf sign '$pz))
           
              ;; When *complexsign* is true & y # 0, set sign to complex.
              ;; To test y # 0, we'll use (not (eql y 0)))
              ((and *complexsign* (not (eql y 0)))
                (setf sign '$complex))
			        (t (setf sign '$pnz))))
		nil)
```

replace it with this:

```lisp
(defun sign-sinc (e) ; e = sinc(x)
     (let* ((x (cadr e))
            (s (sign* x)))
       (cond
             ;; When *complexsign* is true and x is complex or imaginary,
             ;; so is sinc(x).
             ((and *complexsign* (member s '($complex $imaginary)))
                (setq sign '$complex odds (ncons e) evens nil minus nil))
             ;; When -%pi < x < %pi, sign(sinc(x)) = $pos.  The sign of x
             ;; settles one bound: x >= 0 gives x > -%pi, and x <= 0 gives
             ;; x < %pi.
             ((and (or (member s '($pos $pz $zero))
                       (eq t (mgrp x (mul -1 '$%pi))))
                   (or (member s '($neg $nz $zero))
                       (eq t (mgrp '$%pi x))))
                (setq sign '$pos odds nil evens nil minus nil))
             ;; When -%pi <= x <= %pi, sign(sinc(x)) = $pz
             ((and (or (member s '($pos $pz $zero))
                       (eq t (mgqp x (mul -1 '$%pi))))
                   (or (member s '($neg $nz $zero))
                       (eq t (mgqp '$%pi x))))
                (setq sign '$pz odds (ncons e) evens nil minus nil))
             (t (setq sign '$pnz odds (ncons e) evens nil minus nil))))
     nil)
```

One value moves the other way: `csign(sin(z*conjugate(z)))` was `pnz`, since `rectform` sees the product as real, and is `complex` now, since `csign(z*conjugate(z))` is; the product is the place to fix that.

**7. A conjugate rule for `acosh`**, separate from the rest, since it changes `conjugate` and `rectform` rather than `csign`. `acosh` has its branch cut on the real interval `x <= 1` and is analytic and real elsewhere on the real line, so it commutes with the conjugate off the cut (Schwarz reflection, DLMF 4.37), like `asin` and `atanh`, which already have such a rule. In `src/conjugate.lisp`, after `conjugate-atanh`:

```lisp
;; acosh is analytic off the real interval x <= 1, where it has its branch
;; cut, and real on x > 1, so it commutes with the conjugate off the cut
;; (Schwarz reflection); DLMF 4.37.
(defun off-the-acosh-cutp (z)
  (setq z (trisplit z))	          ; split into real and imaginary
  (or (eq t (mnqp (cdr z) 0))     ; y # 0
      (eq t (mgrp (car z) 1))))   ; x > 1

(defun conjugate-acosh (x)
  (setq x (car x))
  (if (off-the-acosh-cutp x) (take '(%acosh) (take '($conjugate) x))
    (list '($conjugate simp) (take '(%acosh) x))))

(setf (get '%acosh 'conjugate-function) 'conjugate-acosh)
```

With it `conjugate(acosh(2+%i))` is `acosh(2-%i)`, `conjugate(acosh(a))` is `acosh(a)` for `a > 1`, and `conjugate(acosh(-2))`, `conjugate(acosh(1/2))` and `conjugate(acosh(x))` for an `x` of unknown sign stay as they are; numerically `acosh(-2)` is `1.317 + %pi*%i` and its conjugate is not `acosh(-2)`. `acoth`, `asech`, `asinh`, `acot` and `acsch` lack such a rule as well, and `acoth` and `asech` also lack a sign rule, so `csign(acoth(x))` is `pnz` for the interval where the value is complex; each is the same small job with its own cut. For `tests/rtestconjugate.mac`, before the final `kill(all)`:

```
/* acosh commutes with the conjugate off its cut, the real x <= 1 */
(declare(z, complex), assume(a > 1, b < 1), 0);
0$

[conjugate(acosh(2+%i)), conjugate(acosh(3)), conjugate(acosh(a)), conjugate(acosh(a+%i*y))];
[acosh(2-%i), acosh(3), acosh(a), acosh(a-%i*y)]$

[conjugate(acosh(-2)), conjugate(acosh(1/2)), conjugate(acosh(b)), conjugate(acosh(x)), conjugate(acosh(z))];
[conjugate(acosh(-2)), conjugate(acosh(1/2)), conjugate(acosh(b)), conjugate(acosh(x)), conjugate(acosh(z))]$

(forget(a > 1, b < 1), remove(z, complex), 0);
0$
```

**3. Functions real for any argument** need nothing: the clause reads the `real-valued` property that `risplit` reads for the same purpose, set in `src/conjugate.lisp` for `realpart`, `imagpart`, `carg`, `abs`, `hstep`, `kron_delta` and `charfun`, so the two keep one list.

## Agreement with risplit

`risplit` in `src/rpart.lisp` decides the same question for `rectform`, in this order: a `risplit-function` of its own; the `real-valued` property or a declaration `real` gives a real value; a `commutes-with-conjugate` or `conjugate-function` property lets it write the real and imaginary parts through `conjugate` when that simplifies, which is how `gamma(x)` comes out real and `gamma(z)` does not; a declaration `complex` gives `realpart` and `imagpart` nouns; and an unknown function gives the nouns whatever its arguments are. The fix follows the same list where `csign` has an answer for it, part 5 brings `risplit` level on subscripted names, and the two now agree on every class but the last:

| | `rectform` | `csign` before | `csign` after |
| --- | --- | --- | --- |
| `real-valued` or declared real: `realpart(z)`, `charfun(z > 0)`, `h(z)`, and with part 5 `h[1](z)` | real | `pnz` | `pnz` |
| declared complex: `k(z)`, `k(x)` | nouns | `complex` | `complex` |
| conjugate property, real arguments: `gamma(x)`, `erfc(x)`, `tan(x)` | real | `pnz` | `pnz` |
| conjugate property, complex argument: `gamma(z)`, `gamma(%i*x)`, `tan(z)`, `zeta(z)`, `bessel_j(0,z)` | nouns | `pnz` | `complex` |
| no property, complex argument: `f(z)`, `lambert_w(z)`, `expintegral_e1(z)` | nouns | `pnz` | `complex` |
| no property, real arguments: `f(x)`, `lambert_w(x)` | nouns | `pnz` | `pnz` |

The last row is the one remaining difference, and it is deliberate. `rectform(f(x))` is `'realpart(f(x)) + %i*'imagpart(f(x))`, while `csign(f(x))` has always been `pnz`, and `abs(f(x))^2` is `f(x)^2` on that account; the comment above that clause of `risplit` records that the integrators assume an unknown function real. Making `csign` return `complex` for every `f(x)` would follow `risplit` to the letter and change a great deal for the sake of an argument that is real. Making `risplit` treat `f(x)` as real would follow `csign` and is the other way to close the gap; it is a separate change with its own suite run.

`csign` decides a real argument by its own means rather than by `conjugate`, which is slightly more cautious: `conjugate(x^(1/3))` does not simplify with `domain : real`, so `rectform(gamma(x^(1/3)))` gives nouns, while `csign(x^(1/3))` is `pnz` there and `csign(gamma(x^(1/3)))` stays `pnz`.

## What changes

With `z` declared `complex`, before and after:

| expression | before | after |
| --- | --- | --- |
| `gamma(z)`, `tan(z)`, `erfc(z)`, `zeta(z)`, `bessel_j(0,z)`, `psi[0](z)`, `expintegral_e1(z)`, `log_gamma(z)`, `beta(z,w)`, `gamma_incomplete(2,z)`, `lambert_w(z)`, `airy_ai(z)`, `f(z)`, `f(x, z)`, `f(z^2)`, `f(f(z))` | `pnz` | `complex` |
| `gamma(%i*x)`, `gamma(x+%i)`, `f(%i)`, `f(x+%i*y)`, `f(sqrt(x))`, `f(log(x))` | `pnz` | `complex` |
| `realpart(z)`, `imagpart(z)`, `carg(z)`, `charfun(z > 0)` | `pnz` | `pnz` |
| `f(x)`, `f(x, y)`, `f([z])`, `f("s")`, `f(abs(z))`, `f(hstep(z))`, `gamma(x)`, `gamma(x+1)` | `pnz` | `pnz` |
| `csign(h(z))` after `declare(h, real)` | `pnz` | `pnz` |
| `csign(g(z))` after `assume(g(z) > 0)` | `pos` | `pos` |
| `sign(f(z))`, real mode | `pnz` | `pnz` |
| `g[1](z)`, `g[z](x)`, `psi[0](z)`, `f[1](x, z)` | `pnz` | `complex` |
| `g[1](x)`, `psi[0](x)`, `h[1](z)` with `h` declared real | `pnz` | `pnz` |
| `p[1](x)` with `p` declared `posfun` | `pnz` | `pos` |
| `p(z)`, `p[1](z)` with `p` declared `posfun`, `cosh(z)` | `pos` | `complex` |
| `sin(f(x))`, `cos(g[1](x))`, `sinc(f(x))` | `complex` | `pnz` |
| `sin(z*conjugate(z))` | `pnz` | `complex` |
| `abs(gamma(z))^3` | `gamma(z)^2*abs(gamma(z))` | unchanged |
| `abs(f(z))^(2/3)`, `abs(f(z))/f(z)`, `f(z)^(2/3)*abs(f(z))^(1/3)` | `f(z)^(2/3)`, `f(z)/abs(f(z))`, `abs(f(z))` | unchanged |
| `abs(f(x))^3` | `f(x)^2*abs(f(x))` | `f(x)^2*abs(f(x))` |
| `sqrt(f(z)^2*conjugate(f(z))^2)` | `abs(f(z))*sqrt(conjugate(f(z))^2)` | unchanged |

`f(sqrt(x))` becoming `complex` is by design: `csign(sqrt(x))` is `complex` for an undeclared `x`, and a function of it cannot be assumed real. The last row is the one test the suite pins the other way, from SF bug #2549: `sqrt(f(z)^2)` becoming `abs(f(z))` was the same mistake one level up, and the old answer is `%i` rather than 1 where `f(z)` is `%i`; the fixed one is right but not reduced (`abs(f(z))^2` would be).

The randomized check of products of powers of a base and of its `abs`, 300 products with `q1` and `q2` declared complex at complex points, had 4 wrong results on `master` and 10 with the `x^(2/3)*abs(x)^(1/3)` combination, every one of them a function of `q1` that `csign` called real; with this fix it has none.

## Tests

For `tests/rtest_sign.mac`, before its final facts check; the file is registered in `src/testsuite.lisp` with the known failures 21, 25, 30, 40 and 145, all far below the appended block.

```
/* csign of a function with no sign rule of its own and a complex or
   imaginary argument is complex, unless the database knows the sign of the
   application or the function is declared real */

(declare(z, complex), map(csign, [gamma(z), tan(z), erfc(z), zeta(z), bessel_j(0, z), f(z), psi[0](z), f(x, z)]));
[complex, complex, complex, complex, complex, complex, complex, complex];

map(csign, [gamma(%i*x), gamma(x+%i), f(%i), f(x+%i*y), f(sqrt(x))]);
[complex, complex, complex, complex, complex];

map(csign, [realpart(z), imagpart(z), carg(z), charfun(z > 0), f(x), f([z]), f("s"), gamma(x)]);
[pnz, pnz, pnz, pnz, pnz, pnz, pnz, pnz];

(declare(h, real), csign(h(z)));
pnz;

(assume(g(z) > 0), csign(g(z)));
pos;

sign(f(z));
pnz;

[abs(gamma(z))^3, abs(f(z))^(2/3), abs(f(x))^3];
[abs(gamma(z))^3, abs(f(z))^(2/3), f(x)^2*abs(f(x))];

/* subscripted functions, the subscript included, and posfun */

map(csign, [g[1](z), g[1](x), g[z](x), h[1](z), psi[0](z), psi[0](x)]);
[complex, pnz, complex, pnz, complex, pnz];

(declare(p, posfun), map(csign, [p(x), p[1](x), p(z), p[1](z)]));
[pos, pos, complex, complex];

/* sin, cos and sinc of an unknown function of a real argument are real,
   as the function is; of a complex one, complex */

map(csign, [sin(f(x)), cos(g[1](x)), sinc(f(x)), sin(f(z)), cos(x+%i), sin(sqrt(x)), sin(gamma(x))]);
[pnz, pnz, pnz, complex, complex, complex, pnz];

(assume(x > 0, x < 3), [csign(sin(x)), csign(cos(x)), csign(sinc(x))]);
[pos, pnz, pos];

(forget(x > 0, x < 3), 0);
0;

(forget(g(z) > 0), remove(z, complex), remove(h, real), remove(p, posfun), 0);
0;
```

And in `tests/rtest_abs.mac`, problems 126 and 127 re-pinned. Find this:

```
sqrt(foo);
/* a better result would be abs('diff(z(q),q,1))^2 but at least this result is not incorrect */
abs('diff(z(q),q,1))*sqrt(conjugate('diff(z(q),q,1))^2);

sqrt(f(z)^2*conjugate(f(z))^2);
abs(f(z))*sqrt(conjugate(f(z))^2);
```

replace it with this:

```
sqrt(foo);
/* a better result would be abs('diff(z(q),q,1))^2; the earlier result
   abs('diff(z(q),q,1))*sqrt(conjugate('diff(z(q),q,1))^2) took the
   derivative of the complex z(q) for a real quantity, and is %i rather
   than 1 where the derivative is %i */
sqrt(('diff(z(q),q,1))^2*('diff(conjugate(z(q)),q,1))^2);

sqrt(f(z)^2*conjugate(f(z))^2);
sqrt(f(z)^2*conjugate(f(z))^2);
```

## Not part of this fix

- `max(1, z)` is `pos` for a complex `z`: `sign-minmax` never looks at the arguments. `cosh(z)` was `pos` through the `posfun` path and is `complex` with part 4.
- `csign(hstep(z))` signals an error for a complex `z`, before and after, and so does `csign(sin(hstep(z)))`, whose interval test asks the sign of `hstep(z) + %pi`; the helper only keeps the error from spreading to `f(hstep(z))`, at the price of leaving the message in the `error` variable.
- `csign(f(z)*conjugate(f(z)))` is `complex`, although the product is real; that is the sign of a product, not of an application.
- In real mode `sign(z)` is `pnz` for a `z` declared complex, as it always was; this fix only touches complex mode.

## Safety of the shape test

The shape test may say "real" only where `csign` would, or the fix would answer `pnz` where the plain rule would have answered `complex`. Checked with random expressions: atoms among `x`, `y`, a declared complex `z`, a declared imaginary `w`, a declared even `n`, a declared real `k`, numbers, `%pi`, `%e` and `%i`; unary constructors among the trigonometric, hyperbolic and inverse functions, `log`, `sqrt`, `exp`, `gamma`, `erf`, `abs`, `floor`, `signum`, `conjugate`, `realpart`, powers with integer, rational, symbolic and negative bases, an unknown `f`, a subscripted `g[1]`, `li[2]`, `psi[0]`, `bessel_j`, `gamma_incomplete`, `max`, `atan2`, a `posfun`, `integrate` and `diff` nouns, `charfun`, factorial and `binomial`; binary constructors among sum, product, power, quotient, `f`, `max`, `atan2`, `bessel_j`, `gamma_incomplete`, `beta` and `g[a](b)`; depth up to 3. For every expression the test's answer was compared with `csign`: with `domain : real`, 3,809 expressions, 1,629 called real by shape, no contradiction; with `domain : complex`, 3,818 expressions, 1,647 called real, no contradiction. The first version of the test did produce six, all `sin`, `cos` or `sinc` of an unknown function, which is what part 6 resolves; before part 6, `csign` contradicted itself on those.

Two things the check ran into on the unmodified build, unrelated to the fix: `csign(erfc(inf/hypergeometric([%e],[2],%i)))` never returns and eventually exhausts the heap, so `inf` and `hypergeometric` were left out of the constructors; and `plog` was left out because its simplifier can ask a question.

## Cost

Measured on the full core plus share suite over one warmed object directory, the configurations interleaved round by round, the first round discarded, medians of the remaining five. First loaded at runtime into the image of the branch, before part 6 existed, to isolate the shape test:

| configuration | rounds 2 to 6 | median |
| --- | --- | --- |
| without the fix | 134.1, 134.7, 143.5, 134.4, 133.9 | 134.4 s |
| the fix, arguments always asked with `csign` | 146.7, 157.4, 151.4, 147.7, 146.4 | 147.7 s |
| the fix with the shape test | 139.2, 136.8, 144.6, 138.5, 136.6 | 138.5 s |

Every run without the shape test is slower than every run without the fix, a tenth: `csign` was being asked about every composite argument of every application without a rule, and `csign(sin(x))` computed a `rectform`. Then parts 1 to 6 in their final form compiled into the image with `make`, which is how they would ship, against the unchanged build, six rounds, first discarded:

| configuration | rounds 2 to 6 | median |
| --- | --- | --- |
| unchanged build | 143.2, 139.7, 137.5, 136.6, 138.2 | 138.2 s |
| the fix compiled in | 138.6, 139.3, 136.9, 140.2, 136.3 | 138.6 s |

The paired differences, fix less baseline in each round, are -4.6, -0.4, -0.6, 3.6 and -1.9 s: no measurable cost. Before part 6 the same measurement showed about 2%, the shape test walking and `csign` being asked about a root or a logarithm; part 6 gives that back, since `csign` of a `sin` or `cos` no longer computes a `rectform` anywhere, and the sign of the argument, once in hand, spares one of the two endpoint comparisons whenever it is definite. On a loop of 20,000 calls `csign(f(x))` and `abs(f(x))^3` time the same as before, `csign(f(z))` for a `z` declared complex takes 3.1 rather than 2.6 microseconds, one declaration lookup per argument, and `csign(f(x, y, sin(x), x+1, x^2))` 6 against 5 microseconds, where the version without the shape test took 30.

## Suite

With parts 1 to 6 compiled into the image with `make`, `run_testsuite(share_tests=true)` reports 21,099 tests and, besides the environmental `share/stringproc/rtestprintf.mac` 38, exactly the two `rtest_abs.mac` problems re-pinned above, 126 and 127, in all six runs of the timing below; the same with part 7 loaded on top. With the re-pins in place and the new blocks appended, `rtest_abs` passes 182/182, `rtestconjugate` 276/276, and `rtest_sign` at its registered known failures only. An earlier version of the helper asked `csign` of every argument and broke `rtestnset.mac` 592, where the argument is a string; the guard on atoms is what fixed that. A trial of `rtest_sign.mac` run through `batch(file, test)` from a `-b` file rather than through `run_testsuite` stalls at problem 567 waiting for an `asksign` answer, before and after; that is `batch_answers_from_file`, not the fix.
