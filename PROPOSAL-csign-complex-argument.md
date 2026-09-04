# Proposed fix: csign of a function with no sign rule of its own is complex when an argument is

`BUG-csign-function-of-complex-argument.md` reports the problem: with `z` declared `complex`, `csign(gamma(z))`, `csign(tan(z))`, `csign(erfc(z))`, `csign(f(z))` and many more are `pnz`, real of unknown sign, and the `abs` rules of the simplifier, which are guarded by `csign`, act on them. This is a fix, prototyped by redefining the functions at runtime in a built image and run through the full test suite that way, and not yet committed to `src/`.

## Where the answer comes from

`sign` in `src/compar.lisp` dispatches on the operator of an application: a `sign-function` property (`mtimes`, `mplus`, `mexpt`, `%log`, `mabs`, `%sin`, `%cos`, `%gamma`, `$floor`, ...), then a declared `posfun` or `oddfun` kind, else `sign-any`. `sign-any` looks at the operator alone: declared `imaginary` or `complex` gives that answer, anything else goes to the fact database with `dcompare`, which knows nothing about `f(z)` and answers `pnz`. The arguments are never looked at. `sign-oddfun` ends in `sign-any` too, which is how `tan(z)` gets there, and `gamma-sign` in `src/csimp2.lisp` computes `csign` of its argument itself and then folds `complex` into `pnz` along with everything else it does not recognize.

## The fix

Four parts, all in complex mode only, so that `sign`, `is` and `asksign` in real mode do not change, except that a declared `posfun` or `oddfun` now applies to a subscripted function in both modes.

**1. `sign-any`** in `src/compar.lisp` consults the arguments when nothing else decided: an application, the database answering `pnz`, the function not declared `real`, and an argument that is complex or imaginary, is `complex`. The database comes first, so `assume(g(z) > 0)` still gives `pos`; `declare(f, real)`, which `featurep` already understands for a function, opts a user function out. A function is real-valued when `risplit`'s `real-valued` property says so. A symbol argument is complex when declared so. An expression is first tested by its shape, without asking anything: a number, an undeclared symbol, an integer power, or an application of an operator without the `conjugate-function` property with parts that pass the same test, is real, since that property marks exactly the functions that are complex for some real argument, `log`, `sqrt` and the inverse trigonometric functions among them, and it is what `risplit` reads for the same decision. Only an argument that fails the shape test is asked with `csign`, with lists, matrices and equations passed over and an error there, as `csign(hstep(z))` raises one, counting as not known; a number is real and a string has no sign. The shape test is what keeps the cost down, since `csign` of `sin(x)` in complex mode computes a `rectform`.

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
;; True when X is real by its shape, without asking csign: a number, a
;; symbol not declared complex or imaginary, an integer power, or an
;; application of an operator that has no CONJUGATE-FUNCTION, the property
;; of the functions that are complex for some real argument, as log and
;; the inverse trigonometric functions are, with parts that are real by
;; the same test.  An unknown function of real arguments is real, as csign
;; has always taken it.
(defun surely-real-p (x)
  (cond ((numberp x) t)
        ((symbolp x)
         (not (or (member x '($%i $infinity $und $ind))
                  (decl-complex-kind x))))
        ((atom x) nil)
        ((member (caar x) '(rat bigfloat)) t)
        ((specrepp x) nil)
        ((mexptp x)
         (and (integerp (caddr x)) (surely-real-p (cadr x))))
        ((mqapplyp x)
         (let ((op (subfunname x)))
           (or (get op 'real-valued)
               (and (not (get op 'conjugate-function))
                    (not (decl-complex-kind op))
                    (every #'surely-real-p (subfunsubs x))
                    (every #'surely-real-p (margs x))))))
        ((and (symbolp (caar x)) (not (mbagp x)))
         (let ((op (caar x)))
           (or (get op 'real-valued)
               (and (not (get op 'conjugate-function))
                    (not (decl-complex-kind op))
                    (every #'surely-real-p (cdr x))))))
        (t nil)))

;; True when an argument of the application X, a subscript of a subscripted
;; function included, is complex or imaginary: a symbol when it is declared
;; so, an expression that is not a list, matrix or equation and not real by
;; its shape by $csign, with an error there, as for hstep(z), counting as
;; not known.  A number is real, and a string or another atom has no sign.
(defun complex-argument-p (x)
  (some #'(lambda (arg)
            (cond ((symbolp arg) (decl-complex-kind arg))
                  ((and (consp arg) (not (mbagp arg)) (not (surely-real-p arg)))
                   (member (car (let (($errormsg nil)) (errcatch ($csign arg))))
                           '($complex $imaginary)))))
        (if (mqapplyp x)
            (append (subfunsubs x) (margs x))
            (margs x))))
```

`$csign` binds the four sign specials afresh, so calling it from inside `sign-any` does not disturb the answer being built; `gamma-sign` has always done the same.

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
- `csign(hstep(z))` signals an error for a complex `z`, before and after; the helper only keeps it from spreading to `f(hstep(z))`, at the price of leaving the message in the `error` variable.
- `csign(f(z)*conjugate(f(z)))` is `complex`, although the product is real; that is the sign of a product, not of an application.
- In real mode `sign(z)` is `pnz` for a `z` declared complex, as it always was; this fix only touches complex mode.

## Cost

Measured twice each, alternating, on the full core plus share suite: 137.3 s and 135.0 s without the fix, 137.1 s and 137.3 s with it, the same within the noise of the runs. Without the shape test the same suite took 145 to 150 s, a tenth more, because `csign` was asked about every composite argument of every application without a rule, and `csign(sin(x))` in complex mode computes a `rectform`. With it, `csign(f(x))` and `abs(f(x))^3` time the same as before on a loop of 20,000 calls, `csign(f(z))` for a `z` declared complex takes 3.1 rather than 2.6 microseconds, the cost of one declaration lookup per argument, and `csign(f(x, y, sin(x), x+1, x^2))` takes 6 microseconds against 5 before, where the first version took 30. What still pays is an argument that fails the shape test, such as `sqrt(x)` or `log(x)`, one `csign` each, which is the question being asked.

## Suite

With the fix loaded at runtime into the built image of the branch, `run_testsuite(share_tests=true)` reports 21,099 tests and, besides the environmental `share/stringproc/rtestprintf.mac` 38, exactly the two `rtest_abs.mac` problems re-pinned above, 126 and 127; with the re-pins in place and the `rtest_sign.mac` block appended, both files pass in full (`rtest_abs` 182/182, `rtest_sign` at its registered known failures only). An earlier version of the helper asked `csign` of every argument and broke `rtestnset.mac` 592, where the argument is a string; the guard on atoms is what fixed that. A trial of `rtest_sign.mac` run through `batch(file, test)` from a `-b` file rather than through `run_testsuite` stalls at problem 567 waiting for an `asksign` answer, before and after; that is `batch_answers_from_file`, not the fix.
