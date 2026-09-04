# Handover: odd roots under `domain : real`, from `rectform` to `integrate`

Branch `claude/maxima-rectform-root-branch-pp1qu1` on the GitHub mirror, on top of upstream `c5762fd` (*Fixes for bugs: #2596, #5091*). The mirror is overwritten on every sync, so this document carries the whole change as find-and-replace patches against upstream, the tests, and a commit message, ready for SourceForge.

Everything below was built and verified on SBCL with `./configure --enable-sbcl && make`. The full core plus share suite, `run_testsuite(share_tests=true)`, passes: 21,108 tests, the only failure the pre-existing environmental one in `share/stringproc/rtestprintf.mac` problem 38.

## 1. The problem

With `domain : real`, the default, the simplifier takes the real root of a negative quantity raised to a power with an odd denominator: `(-8)^(1/3)` is `-2`, `(x^3)^(1/3)` is `x`, `(-x^3)^(-1/3)` is `-1/x`. Four other parts of Maxima each committed to a branch of their own, and the results contradicted one another.

```
(%i1) x^(1/3), x = -8;
(%o1) -2
(%i2) rectform(x^(1/3)), x = -8;
(%o2) sqrt(3)*%i+1
(%i3) assume(xn < 0)$ [csign(xn^(2/3)), csign(xn^(1/3))];
(%o3) [pos, neg]                          /* the simplifier's branch */
(%i4) declare(n, odd)$ csign(xn^(2/n));
(%o4) complex                             /* but not for a declared odd n */
(%i5) F : integrate(%e^(x^3), x);
(%o5) -gamma_incomplete(1/3,-x^3)/3
(%i6) float(rectform(subst(x = 2, F) - subst(x = 1, F))), quad_qags(%e^(x^3), x, 1, 2)[1];
(%o6) 0.773*%i-2.0,  276.4               /* wrong for x > 0 */
(%i7) diff(gamma_incomplete(1/3, -x^3), x);
(%o7) 3*%e^x^3                            /* the derivative of a different function */
```

`gamma_incomplete` is evaluated on the principal branch, so its derivative and the power `(-x^3)^(-1/3)` that the integrator attaches to it are principal-branch quantities too. Under `domain : real` the simplifier rewrote both to the real root. The two mistakes cancelled in `diff(integrate(%e^(x^3), x), x)`, which is why the wrong antiderivative went unnoticed, while its values, the definite integrals that go through it, and `integrate(sin(x^3), x)` were wrong.

## 2. The design

Three decisions cover everything; the third was added last and made the other two simpler.

**`rectform`, `polarform`, `carg` and `csign` follow the simplifier.** Under `domain : real` a power of a real quantity with an integer numerator and an odd denominator is real, with the sign of its base raised to the numerator. The rectangular form is the power itself, the modulus is the power of the modulus of the base, the argument is 0 or `%pi`. A denominator merely declared `odd` counts, since `evod` knows it; an unknown numerator does not. `domain : complex` is untouched.

**Wherever `gamma_incomplete` is involved, the principal branch is the only right one, so the power is written in a form the simplifier leaves alone.** For `z = k*w^n` with a constant `k` and a real `w`, the argument of `z` is that of `k` for a positive `w^n` and that of `-k` for a negative one, so

```
z^s = abs(k)^s * abs(w)^(n*s) * (alpha + beta*w/abs(w))
```

with `alpha + beta` the phase `%e^(%i*s*carg(k))` and `alpha - beta` the phase `%e^(%i*s*carg(-k))`. The sign `w/abs(w)` and the powers of `abs(w)` are rational in `w` and `abs(w)`, and the simplifier reduces `abs(w)^2` to `w^2`, `(w/abs(w))^2` to 1 and `w^2/abs(w)^3` to `1/abs(w)`, so the product of two such powers, as of an antiderivative and of its derivative, reduces under `expand` or `ratsimp`. An `atan2(0, w)` or a `signum(w)` in an exponent would not: `signum` has no derivative rule, and `risplit` turns an `atan2` with a zero second argument into logarithms. The same construction serves the integrator (types 1a, 2 and 2-1 of `integrate-exp-special`), the `z` derivatives of `gamma_incomplete`, `gamma_incomplete_lower` and `gamma_incomplete_generalized`, and the `gamma_expand` recurrences for a rational order, which had the same bug. Where the simplifier commits to no branch, `(k*x)^s` with `k > 0`, nothing changes.

**The sign of a real `x` gets one normal form, `x/abs(x)`.** The simplifier already cancels even powers between `abs(x)` and `x` (`abs(x)^3` is `x^2*abs(x)`, `abs(x)/x^2` is `1/abs(x)`), but left `abs(x)/x` and `x/abs(x)` as two forms of the same thing, so a phase written by one route and a phase written by another cancelled under `expand` only if both happened to write the sign the same way. Two comparisons in `timesin` admit the exponent 1 in the clauses that move `abs` into the denominator; the clauses in the opposite direction keep their guard, so the rules do not chase each other; the existing `csign` test keeps a declared-complex `z` out of it, where `abs(z)/z` and `z/abs(z)` differ. This was verified separately against master before it was taken onto the branch; with it, the integrator multiplies `var^m` through the phase and lets the simplifier normalize, and the derivative template needs no case for a negative `n`.

Two consequences worth knowing before reading the patches. With the phase a step at `x = 0`, the antiderivative, `gamma_incomplete(alpha, 0)` times the phase, would jump there, so `gamma(alpha)` is taken from `gamma_incomplete` where `alpha > 0` and `z` goes to 0 with `x`: the antiderivative of `%e^(x^3)` is 0 at 0 from both sides and real for real `x`. And the integrator's substitution variable for an even root, which it declares complex, is treated as nonnegative, which it is wherever the integrand is real; that is what makes `exp(sqrt(x^3))` and `expintegral_ei(x^(-3/4))` work.

## 3. The patches

Apply them to upstream `master`. Each block quotes the upstream text with three lines of context and gives the line where it starts; the replacement keeps the context. The blocks preserve the tabs of the surrounding code.

### 3.1 `src/rpart.lisp`

Four hunks. The first adds `*risplit-domain*` (`risplit` binds `$domain` to `$complex` while it works, so its helpers read the user's domain from here), `odd-root-p`, `real-odd-root-p`, and the principal-power machinery: `principal-phase`, `nonneg-internal-p`, `real-power-factors`, `sign-varies-p`, `principal-power`, and `principal-power-times` (for the integrator: `var^m` times the power, multiplied through the phase so that the sign of `var^m` folds into it); the derivative of `gamma_incomplete` calls `principal-power` itself. The second adds the clause of `risplit-expt` that keeps an odd root of a real base real, simplified with `$domain` bound back to `$real`. The third records the domain in `risplit`. The fourth adds the `absarg` clause for such a power, placed before the general `mexpt` clause because that one would send a power of a negative base back through `simpabs` and `cabs` and loop.

**Hunk 1.** Insert the helpers after the `defvar` block at the top of the file.

**Find this** (upstream line 23):

```lisp
(declare-top (special $radexpand
		      $keepfloat))

;;; Realpart gives the real part of an expr.

(defun risplit-signum (x) ;rectangular form for a signum expression
```

**Replace it with this:**

```lisp
(declare-top (special $radexpand
		      $keepfloat))

;; $domain as it was when RISPLIT was entered.  RISPLIT binds $domain to
;; $complex while it works, so its helpers read this to learn whether the
;; user works in the real domain.
(defvar *risplit-domain* nil)

;; True when POW is a root with an integer numerator and an odd denominator,
;; as 1/3, 2/3 or 1/n with n declared odd.  With domain : real the
;; simplifier takes the real root of a real quantity raised to such a
;; power: (-8)^(1/3) simplifies to -2 and (x^3)^(1/3) to x.  An integer
;; power, with the denominator 1, is not one.
(defun odd-root-p (pow)
  (let ((den ($denom pow)))
    (and (not (eql den 1))
         (maxima-integerp ($num pow))
         (eq (evod den) '$odd))))

;; ODD-ROOT-P with the user's $domain real: a real quantity raised to the
;; power is then real, and csign, abs and signum agree with it.  With
;; domain : complex, or an even denominator, the power is on the principal
;; branch and may be complex.
(defun real-odd-root-p (pow)
  (and (eq (or *risplit-domain* $domain) '$real) (odd-root-p pow)))

;; The functions below write z^s on the principal branch for a z = k*w^n
;; with a constant k, a real w and a rational n, where with domain : real
;; the simplifier takes the real root instead, as (-x^3)^(-1/3) to -1/x:
;; the power of z that comes with gamma_incomplete(a, z), in the
;; antiderivatives of INTEGRATE-EXP-SPECIAL, in the derivative and in the
;; recurrences of gamma_expand, is on the principal branch as the function
;; is.  The argument of z is that of k for a positive w^n and that of -k
;; for a negative one, so
;;
;;   z^s = abs(k)^s * abs(w)^(n*s) * (alpha + beta*w/abs(w))
;;
;; with alpha + beta the phase %e^(%i*s*carg(k)) and alpha - beta the
;; phase %e^(%i*s*carg(-k)).  The sign w/abs(w) and the powers of abs(w)
;; are rational in w and abs(w), abs(w)^2 simplifies to w^2 and the sign
;; has that one form, so that the product of two such powers, as of an
;; antiderivative and of its derivative, reduces under expand or ratsimp,
;; where an atan2(0, w) in an exponent would not.

;; %e^(%i*s*arg(z)) as alpha + beta*SIGMA for SIGMA the sign w/abs(w) of w
;; in z = k*w^n, with RE and IM the real and imaginary parts of k.  The
;; constants are expanded to give them one form wherever they come from,
;; as one instance has to cancel another.
(defun principal-phase (s re im sigma)
  (let ((plus (power '$%e (mul '$%i s (take '(%atan2) im re))))
        (minus (power '$%e (mul '$%i s (take '(%atan2) (neg im) (neg re))))))
    (add ($expand (div (add plus minus) 2))
         (mul ($expand (div (sub plus minus) 2)) sigma))))

;; The integrator substitutes an internal variable for an even root and
;; declares it complex, as the root is complex for a negative radicand;
;; where the integrand is real, the variable is a nonnegative real.
(defun nonneg-internal-p (w)
  (and (symbolp w) (get w 'internal) (kindp w '$complex)))

;; Z as k*w^n, for a constant k and a real w that is not constant, with n
;; an integer or a rational number: (values k w n nonneg), NONNEG saying
;; that w is known to be nonnegative; nil when z is not of this form, or
;; when n is 1 and k positive, where the simplifier leaves z^s as k^s*w^s
;; and commits to no branch.
(defun real-power-factors (z)
  (let ((k 1) (w z) (n 1))
    (when (mtimesp z)
      (let ((consts (remove-if-not #'$constantp (cdr z))))
        (when consts
          (setq k (muln consts t)
                w (muln (remove-if #'$constantp (cdr z)) t)))))
    (when (and (mexptp w) (or (integerp (caddr w)) (ratnump (caddr w))))
      (setq n (caddr w) w (cadr w)))
    (cond (($constantp w) nil)
          ((and (eql n 1) (member ($csign k) '($pos $pz))) nil)
          ((nonneg-internal-p w) (values k w n t))
          ((member ($csign w) '($complex $imaginary)) nil)
          (t (values k w n nil)))))

;; True when the sign of w^n, for a real w, is that of w: n is an odd
;; integer, or with domain : real an odd numerator over an odd denominator,
;; where the root is the real one.  Otherwise w^n is nonnegative wherever
;; it is real.
(defun sign-varies-p (n)
  (and (oddp ($num n)) (oddp ($denom n))))

;; z^s on the principal branch, as above, or Z^S itself where the
;; treatment does not apply.
(defun principal-power (z s)
  (multiple-value-bind (k w n nonneg) (real-power-factors z)
    (if (and k (odd-root-p s))
        (destructuring-bind (re . im) (trisplit k)
          (let ((qodd (and (not nonneg) (oddp ($denom n)))))
            (mul (power (cabs k) s)
                 (if qodd
                     (power (take '(mabs) w) (mul n s))
                     (power (power w n) s))
                 (principal-phase s re im
                                  (if (and qodd (sign-varies-p n))
                                      (div w (take '(mabs) w))
                                      1)))))
        (power z s))))

;; w^m * z^s for the integrator, multiplied through the phase for an
;; integer m, so that the sign of w^m folds into it: the sign of a real w
;; has the one form w/abs(w), whose square is 1.
(defun principal-power-times (w m z s)
  (let ((p (mul (power w m) (principal-power z s))))
    (if (integerp m) ($multthru p) p)))

;;; Realpart gives the real part of an expr.

(defun risplit-signum (x) ;rectangular form for a signum expression
```

**Hunk 2.** In `risplit-expt`, after the `fixnump` clause.

**Find this** (upstream line 227):

```lisp
      ((fixnump pow)
       (risplit-expt-fixnum-pow sp pow))

      ((and (ratnump pow)
            (fixnump (cadr pow))
            (not (< (cadr pow) (- $maxnegex)))
```

**Replace it with this:**

```lisp
      ((fixnump pow)
       (risplit-expt-fixnum-pow sp pow))

      ((and (=0 (cdr sp)) (real-odd-root-p pow))
       ;; A real base to a rational power with an odd denominator is real
       ;; with domain : real.  Simplify the power in that domain, since
       ;; RISPLIT binds $domain to $complex.
       (cons (let (($domain '$real)) (power (car sp) pow)) 0))

      ((and (ratnump pow)
            (fixnump (cadr pow))
            (not (< (cadr pow) (- $maxnegex)))
```

**Hunk 3.** The header of `risplit`.

**Find this** (upstream line 397):

```lisp
;;; (<Real part> . <imaginary part>).

(defun risplit (l)
  (let (($domain '$complex) ($m1pbranch t) $logarc op)
    (cond ((atom l)
           ;; Symbols are assumed to represent real values, unless they have
           ;; been declared to be complex. If they have been declared to be both
```

**Replace it with this:**

```lisp
;;; (<Real part> . <imaginary part>).

(defun risplit (l)
  (let* ((*risplit-domain* (or *risplit-domain* $domain))
         ($domain '$complex) ($m1pbranch t) $logarc op)
    (cond ((atom l)
           ;; Symbols are assumed to represent real values, unless they have
           ;; been declared to be complex. If they have been declared to be both
```

**Hunk 4.** In `absarg`, before the clause that starts `((eq (caar l) 'mexpt)`.

**Find this** (upstream line 762):

```lisp
	   (unless n
	     (return (cons (muln absl t) (2pistrip (addn argl t)))))
	   (setq abars (absarg (car n) absflag))))
        ((eq (caar l) 'mexpt)
         ;; An expression z^a
         (let ((aa (absarg (cadr l) nil)) ; (abs(z) . arg(z))
```

**Replace it with this:**

```lisp
	   (unless n
	     (return (cons (muln absl t) (2pistrip (addn argl t)))))
	   (setq abars (absarg (car n) absflag))))
        ((and (eq (caar l) 'mexpt)
              (real-odd-root-p (caddr l))
              (=0 (cdr (risplit (cadr l)))))
         ;; With domain : real an odd root of a real quantity is real: its
         ;; modulus is the root of the modulus of the base, and its
         ;; argument is 0 or %pi, by its sign.  The last clause must not
         ;; handle it: its ABSARG-MABS would call SIMPABS, which sends a
         ;; power of a negative base back to CABS.
         (cons (power (car (absarg (cadr l) absflag)) (caddr l))
               (if absflag 0 (genatan 0 l))))
        ((eq (caar l) 'mexpt)
         ;; An expression z^a
         (let ((aa (absarg (cadr l) nil)) ; (abs(z) . arg(z))
```

### 3.2 `src/simp.lisp`

Six hunks. Hunk 1 is a bug fix in `simpexpt`: the clause that rewrites `abs(x)^pot` as `x^pot` accepted an even-numerator fraction such as `2/3` in both domains, but the identity holds only for the real root, so with `domain : complex` `abs(x)^(2/3)` became `x^(2/3)`, which is `4*(-1)^(2/3)` rather than 4 at `x = -8`; the fraction is now accepted with `domain : real` only, an even integer and a declared even exponent in both. The other five are in `timesin`, two simplifier improvements that were each verified against upstream `master` in a session of their own before they were taken here. Hunks 2 and 4 are one comparison each: the exponent 1 is admitted in the two clauses that move `abs` into the denominator, so that the sign of a real `x` has the one form `x/abs(x)`. The two clauses in the opposite direction, commented `x^n/abs(x) -> x^(n-2)*abs(x)` and `1/abs(x)*x^n -> x^(n-2)*abs(x)`, must keep their `(> ... 1)`. Hunks 3, 5 and 6 combine `u^a*abs(u)^b` into `abs(u)^(a+b)` with `domain : real` for a rational `a` that is not an integer and has an even numerator, since `u^a` is then the real root `abs(u)^a`: `(x^2*abs(x))^(1/3)`, the distributed root of `abs(x^3)`, is `abs(x)`, and `abs(-x^3)^(-1/3)` is `1/abs(x)`. Three clauses, since either factor can arrive first and the `abs` in the product list can be a bare `abs(u)`; all three end in the new `absmerge` label, which rescans the product with the merged power after `simpexpt` has normalized it. An integer `a` is left to the clauses that split `abs(x)^3` into `x^2*abs(x)`, so no rule undoes another; the `csign` guard is the one the neighbouring clauses use.

**Hunk 1.** In `simpexpt`, the `mabs` clause before the one commented `abs(x)^(2*n+1) -> abs(x)*x^(2*n)`.

**Find this** (upstream line 2168):

```lisp
            (go cont))
           ((atom gr) (go atgr))
           ((and (eq (caar gr) 'mabs)
                 (or (evnump pot)
                     (mevenp pot))
                 (or (and (eq $domain '$real) (not (apparently-complex-to-judge-by-$csign-p (cadr gr))))
                     (and (eq $domain '$complex) (apparently-real-to-judge-by-$csign-p (cadr gr)))))
            (return (power (cadr gr) pot)))
```

**Replace it with this:**

```lisp
            (go cont))
           ((atom gr) (go atgr))
           ((and (eq (caar gr) 'mabs)
                 (or (mevenp pot)
                     ;; abs(x)^(2*m/n) -> x^(2*m/n) only for the real root
                     ;; of x^(1/n), which the principal branch is not.
                     (and (eq $domain '$real) (evnump pot)))
                 (or (and (eq $domain '$real) (not (apparently-complex-to-judge-by-$csign-p (cadr gr))))
                     (and (eq $domain '$complex) (apparently-real-to-judge-by-$csign-p (cadr gr)))))
            (return (power (cadr gr) pot)))
```

**Hunk 2.** The clause commented `1/x^n*abs(x) -> 1/(x^(n-2)*abs(x))`.

**Find this** (upstream line 2600):

```lisp
                        (eq (caar (car x)) 'mabs)
                        (equal (cadr x) 1)
                        (integerp (caddr (cadr fm)))
                        (< (caddr (cadr fm)) -1)
                        (alike1 (cadr (car x)) (cadr (cadr fm)))
                        (not (member ($csign (cadr (car x)))
                                     '($complex $imaginary))))
```

**Replace it with this:**

```lisp
                        (eq (caar (car x)) 'mabs)
                        (equal (cadr x) 1)
                        (integerp (caddr (cadr fm)))
                        (< (caddr (cadr fm)) 0)
                        (alike1 (cadr (car x)) (cadr (cadr fm)))
                        (not (member ($csign (cadr (car x)))
                                     '($complex $imaginary))))
```

**Hunk 3.** Two clauses inserted before the `great` fallthrough that follows the clause commented `1/abs(x)*x^n -> x^(n-2)*abs(x)`.

**Find this** (upstream line 2658):

```lisp
                   (setq w (cadr x))
                   (go start))
                  
                  ((or (maxima-constantp (car x))
                       (maxima-constantp (cadadr fm)))
                   (if (great temp (cadr fm))
```

**Replace it with this:**

```lisp
                   (setq w (cadr x))
                   (go start))
                  
                  ((and (eq $domain '$real)
                        (not (atom (car x)))
                        (eq (caar (car x)) 'mabs)
                        (or (integerp (cadr x))
                            (ratnump (cadr x)))
                        (ratnump (caddr (cadr fm)))
                        (evenp (cadr (caddr (cadr fm))))
                        (alike1 (cadr (car x)) (cadadr fm))
                        (not (member ($csign (cadr (car x)))
                                     '($complex $imaginary))))
                   ;; abs(x)^b*x^a -> abs(x)^(a+b), where a a ratio with an
                   ;; even numerator, so that with domain : real x^a is the
                   ;; real root abs(x)^a, and b any rational number.
                   ;; Remove x^a.
                   (setq x (power (car x) (add (cadr x) (caddr (cadr fm)))))
                   (rplacd fm (cddr fm))
                   (go absmerge))

                  ((and (eq $domain '$real)
                        (not (atom (cadr (cadr fm))))
                        (eq (caaadr (cadr fm)) 'mabs)
                        (or (integerp (caddr (cadr fm)))
                            (ratnump (caddr (cadr fm))))
                        (ratnump (cadr x))
                        (evenp (cadr (cadr x)))
                        (alike1 (cadadr (cadr fm)) (car x))
                        (not (member ($csign (cadadr (cadr fm)))
                                     '($complex $imaginary))))
                   ;; x^a*abs(x)^b -> abs(x)^(a+b), where a a ratio with an
                   ;; even numerator and b any rational number.
                   ;; Remove abs(x)^b.
                   (setq x (power (cadr (cadr fm))
                                  (add (cadr x) (caddr (cadr fm)))))
                   (rplacd fm (cddr fm))
                   (go absmerge))

                  ((or (maxima-constantp (car x))
                       (maxima-constantp (cadadr fm)))
                   (if (great temp (cadr fm))
```

**Hunk 4.** The clause commented `abs(x)/x^n -> 1/(x^(n-2)*abs(x))`.

**Find this** (upstream line 2675):

```lisp
           ((and (not (atom (cadr fm)))
                 (eq (caar (cadr fm)) 'mabs)
                 (integerp (cadr x))
                 (< (cadr x) -1)
                 (alike1 (cadr (cadr fm)) (car x))
                 (not (member ($csign (cadr (cadr fm)))
                                     '($complex $imaginary))))
```

**Replace it with this:**

```lisp
           ((and (not (atom (cadr fm)))
                 (eq (caar (cadr fm)) 'mabs)
                 (integerp (cadr x))
                 (< (cadr x) 0)
                 (alike1 (cadr (cadr fm)) (car x))
                 (not (member ($csign (cadr (cadr fm)))
                                     '($complex $imaginary))))
```

**Hunk 5.** One clause inserted before the `great` fallthrough that follows it, outside the `mexptp` branch.

**Find this** (upstream line 2690):

```lisp
            (setq w (cadr x))
            (go start))
           
           ((maxima-constantp (car x))
            (when (great temp (cadr fm))
              (go gr)))
```

**Replace it with this:**

```lisp
            (setq w (cadr x))
            (go start))
           
           ((and (eq $domain '$real)
                 (not (atom (cadr fm)))
                 (eq (caar (cadr fm)) 'mabs)
                 (ratnump (cadr x))
                 (evenp (cadr (cadr x)))
                 (alike1 (cadr (cadr fm)) (car x))
                 (not (member ($csign (cadr (cadr fm)))
                              '($complex $imaginary))))
            ;; x^a*abs(x) -> abs(x)^(a+1), where a a ratio with an even
            ;; numerator.  Remove abs(x).
            (setq x (power (cadr fm) (add (cadr x) 1)))
            (rplacd fm (cddr fm))
            (go absmerge))

           ((maxima-constantp (car x))
            (when (great temp (cadr fm))
              (go gr)))
```

**Hunk 6.** The `absmerge` label, between `const` and `times`.

**Find this** (upstream line 2837):

```lisp
     (rplacd fm (cddr fm))
     (setq x (car x) check nil)
     (go top)
  times
     (setq z (tms x 1 (setq temp (cons '(mtimes) y))))
     (return (cond ((eq z temp)
```

**Replace it with this:**

```lisp
     (rplacd fm (cddr fm))
     (setq x (car x) check nil)
     (go top)
  absmerge
     ;; Rescan the list of products with the merged power of abs(x).  SIMPEXPT
     ;; has normalized it as it does any power of abs: an even numerator gives
     ;; a power of x, an odd integer gives x^(2*n)*abs(x).
     (cond ((mnump x)
            (return (rplaca y (timesk (car y) x))))
           ((mtimesp x)
            (go times))
           (t
            (setq temp x
                  x (if (mexptp x) (cdr x) (list x 1)))
            (setq w (cadr x)
                  fm y)
            (go start)))
  times
     (setq z (tms x 1 (setq temp (cons '(mtimes) y))))
     (return (cond ((eq z temp)
```

### 3.3 `src/compar.lisp`

One hunk at the top of `sign-mexpt`: with `domain : real`, or in real mode, an exponent `m/n` with an integer `m` and an odd `n` is judged by its numerator. `sign-expt` keeps the sign of `m/n`, which decides whether the power can be zero.

**Hunk 1.** After the `let*` bindings of `sign-mexpt`.

**Find this** (upstream line 1920):

```lisp
  (let* ((expt (caddr x)) (base1 (cadr x))
	 (sign-expt (sign1 expt)) (sign-base (sign1 base1))
	 (evod (evod expt)))
    ;; The variable sign is now equal to sign-base. This is used below
    ;; in some places to avoid an assignment operation for sign.
    (cond ((and (eq sign-base '$zero)
```

**Replace it with this:**

```lisp
  (let* ((expt (caddr x)) (base1 (cadr x))
	 (sign-expt (sign1 expt)) (sign-base (sign1 base1))
	 (evod (evod expt)))
    ;; With domain : real, or in real mode, x^(m/n) with an integer m and an
    ;; odd n is (x^(1/n))^m, whose sign is that of x^m: judge the exponent
    ;; by its numerator.  SIGN-EXPT stays the sign of m/n, which decides
    ;; whether the power can be zero.  A rational exponent has its own
    ;; clause below.
    (when (and (or (not *complexsign*) (eq $domain '$real))
	       (not (mnump expt)))
      (let ((den ($denom expt)))
	(when (and (not (eql den 1))
		   (maxima-integerp ($num expt))
		   (eq (evod den) '$odd))
	  (setq expt ($num expt)
		evod (evod expt)))))
    ;; The variable sign is now equal to sign-base. This is used below
    ;; in some places to avoid an assignment operation for sign.
    (cond ((and (eq sign-base '$zero)
```

### 3.4 `src/comm.lisp`

One hunk, a bug fix in `sdiffgrad`. A derivative in a `grad` property is either an expression in the placeholder names of the `defgrad` or a function of the arguments. `sdiffgrad` substituted the arguments for the placeholder names into the results of both, so a function could not build its result from the arguments it was given: an argument named like a placeholder would have been substituted a second time. The only such function in the tree, the derivative of `gamma_incomplete` with respect to its order, therefore evaluated a template in the placeholder names `a` and `z` and picked up the values of the Maxima variables `a` and `z` on the way; with `a : 5`, `diff(gamma_incomplete(b, y), b)` came out with `gamma_incomplete(5,y)` in it. A function may now return `t` as a second value, and `sdiffgrad` then takes its result as it is; a function returning one value is substituted into as before, so nothing outside the tree changes. That derivative builds its result from its arguments and returns it that way (hunk 1 of section 3.5), and so do the `z` derivatives, which call `principal-power` directly. The `defgrad` docstring in `src/mopers.lisp` describes both conventions (section 3.6). Committed on its own at the base of the branch, with the first part of the block in section 4.3 as its test.

**Hunk 1.** The body of `sdiffgrad` after the `mqapply` and argument-count clauses.

**Find this** (upstream line 376):

```lisp
	           (length (car grad))))
	  (t
           (setq args (sdiffmap (cdr e) x))
           (setq result
                 (addn
                   (mapcar 
                     #'mul2
                     (cdr 
                       ;; Need to substitute in parallel to avoid trouble when
                       ;; function arguments match the placeholder names of the
                       ;; DEFGRAD expression.
                       ($psubstitute
                         (append '((mlist)) (mapcar #'(lambda (a b)
                                                        (list '(mequal) a b))
                                                    (car grad)
                                                    (cdr e)))
                         (do ((l1 (cdr grad) (cdr l1))
                              (args args (cdr args)) 
                              (l2))
                             ((null l1) (cons '(mlist) (nreverse l2)))
                           (setq l2
                                 (cons (cond ((equal (car args) 0) 0)
                                             ((functionp (car l1))
                                              ;; Evaluate a lambda expression
                                              ;; given as a derivative.
                                              (apply (car l1) (cdr e)))
                                             (t (car l1)))
                                       l2)))))
                     args)
                   t))
           (if (or (null result) (not (freeof nil result)))
               ;; A derivative has returned NIL. Return a noun form.
               (if (not (depends e x))
```

**Replace it with this:**

```lisp
	           (length (car grad))))
	  (t
           (setq args (sdiffmap (cdr e) x))
           ;; A derivative given as an expression is in the placeholder
           ;; names of the DEFGRAD and gets the arguments substituted for
           ;; them, in parallel, as an argument may itself be a placeholder
           ;; name.  A derivative given as a function gets the arguments;
           ;; its result is substituted into the same way, unless it is
           ;; returned with a second value of T, which says that it was
           ;; built from the arguments and is final.
           (let ((subst (append '((mlist))
                                (mapcar #'(lambda (a b) (list '(mequal) a b))
                                        (car grad)
                                        (cdr e)))))
             (setq result
                   (addn
                     (mapcar
                       #'mul2
                       (do ((l1 (cdr grad) (cdr l1))
                            (args args (cdr args))
                            (l2))
                           ((null l1) (nreverse l2))
                         (setq l2
                               (cons (cond ((equal (car args) 0) 0)
                                           ((functionp (car l1))
                                            (multiple-value-bind (d final)
                                                (apply (car l1) (cdr e))
                                              (if final
                                                  d
                                                  ($psubstitute subst d))))
                                           (t ($psubstitute subst (car l1))))
                                     l2)))
                       args)
                     t)))
           (if (or (null result) (not (freeof nil result)))
               ;; A derivative has returned NIL. Return a noun form.
               (if (not (depends e x))
```

### 3.5 `src/gamma.lisp`

Six hunks. Hunk 1 rewrites the derivative of `gamma_incomplete` with respect to its order to build its result from its arguments and return it with the second value (the `sdiffgrad` fix of section 3.4), adds `gamma-incomplete-z-derivative`, which builds the `z` derivative the same way with the power written by `principal-power` when `$domain` is real and as the plain power otherwise, and makes the `z` derivative a function that calls it; hunks 2 and 5 do the same for `gamma_incomplete_lower` and for `z1` and `z2` of `gamma_incomplete_generalized`. `subst-power-order` and `subst-rational-order` serve the three `gamma_expand` clauses for a rational order: the recurrence is now expanded with a fresh symbol for `z` as well as for the order, so that the powers `z^(ord + m)` can be put back as `z^m` times the principal `z^order` before the order is substituted; with `domain : complex` the plain power is used.

**Hunk 1.** The `defgrad` of `%gamma_incomplete`, with `gamma-incomplete-z-derivative` inserted before it.

**Find this** (upstream line 321):

```lisp
      ((mexpt) z ((mplus) -1 a))))
  'grad)

(defgrad %gamma_incomplete ($a $z)
  ;; wrt a
  #'(lambda ($a $z)
      ;; Variable names MUST be $A and $Z because we use #$$...$ to
      ;; define the derivative.
      ;;
      ;; Compiler may not see that $z is used, so declare it ignorable
      ;; to get rid of a warning that it's unused.
      (declare (ignorable $z))
      (cond ((member ($sign $a) '($pos $pz))
             ;; The derivative wrt a in terms of hypergeometric_regularized 2F2
             ;; function and the Generalized Incomplete Gamma function 
             ;; (functions.wolfram.com), only for a>0.
             ;;
             ;; We need to call meval ourselves here to make sure the
             ;; expression is simplified as expected.
             (meval
              #$$ (gamma_incomplete(a,z)-gamma(a))*log(z)+gamma(a)^2
                                        *hypergeometric_regularized(
                                         [a,a],[a+1,a+1],-z)*z^a
                                       +psi[0](a)*gamma(a)$
             ))
            (t
             ;; No derivative. Maxima generates a noun form.
             nil)))
  ;; The derivative wrt z
  #$$ -(%e^-z*z^(a-1))$
  )

;;; Integral of the Incomplete Gamma function
```

**Replace it with this:**

```lisp
      ((mexpt) z ((mplus) -1 a))))
  'grad)

;; The derivative of gamma_incomplete(a, z) with respect to z, times SIGN,
;; built from the actual argument and returned with a second value of T,
;; so that SDIFFGRAD takes it as it is.  With domain : real the
;; simplifier takes the real root of z^(a-1) for a z such as -x^3 or x^3,
;; where the function is on the principal branch, so the power is written
;; by PRINCIPAL-POWER: the antiderivatives of INTEGRATE-EXP-SPECIAL,
;; written the same way, are then differentiated back to their integrands
;; by expand.
(defun gamma-incomplete-z-derivative (a z sign)
  (let ((s (sub a 1)))
    (values (mul sign
                 (power '$%e (neg z))
                 (if (eq $domain '$real) (principal-power z s) (power z s)))
            t)))

(defgrad %gamma_incomplete ($a $z)
  ;; wrt a
  #'(lambda ($a $z)
      ;; The result is built from the actual arguments and returned with
      ;; a second value of T, so that SDIFFGRAD takes it as it is.
      (cond ((member ($sign $a) '($pos $pz))
             ;; The derivative wrt a in terms of hypergeometric_regularized 2F2
             ;; function and the Generalized Incomplete Gamma function 
             ;; (functions.wolfram.com), only for a>0.
             (let ((g (ftake '%gamma $a)))
               (values
                 (add (mul (sub (ftake '%gamma_incomplete $a $z) g)
                           (ftake '%log $z))
                      (mul (power g 2)
                           (take '($hypergeometric_regularized)
                                 (list '(mlist) $a $a)
                                 (list '(mlist) (add $a 1) (add $a 1))
                                 (neg $z))
                           (power $z $a))
                      (mul (take '(mqapply) '(($psi array) 0) $a) g))
                 t)))
            (t
             ;; No derivative. Maxima generates a noun form.
             nil)))
  ;; The derivative wrt z
  #'(lambda ($a $z) (gamma-incomplete-z-derivative $a $z -1))
  )

;;; Integral of the Incomplete Gamma function
```

**Hunk 2.** The `z` derivative in the `defgrad` of `%gamma_incomplete_lower`, and right after it the two recurrence helpers, inserted before `(def-simplifier gamma_incomplete_lower (a z)`.

**Find this** (upstream line 487):

```lisp
  nil
  ;; wrt z
  ;; Obvious from the definition of gamma_incomplete_lower
  #$$ z^(a-1)*exp(-z) $
  )

;;
;; Handles some special cases for the order a and simplifies it to an
;; equivalent form, possibly involving erf and gamma_incomplete_lower
;; to a lower order.
(def-simplifier gamma_incomplete_lower (a z)
  (cond
    ((or
```

**Replace it with this:**

```lisp
  nil
  ;; wrt z
  ;; Obvious from the definition of gamma_incomplete_lower
  #'(lambda ($a $z) (gamma-incomplete-z-derivative $a $z 1))
  )

;;
;; Handles some special cases for the order a and simplifies it to an
;; equivalent form, possibly involving erf and gamma_incomplete_lower
;; to a lower order.
;; gamma_incomplete(ord + n, z) and its relatives, expanded by the clauses
;; for an integer n added to the order, have powers z^(ord + m) in them
;; that the simplifier, with domain : real, takes as real roots once the
;; rational ORDER is put in for the symbol ORD, where the recurrence is on
;; the principal branch: so they are expanded with the symbol ZZ for z,
;; and each power is put back as z^m times the principal z^order of
;; PRINCIPAL-POWER.
(defun subst-power-order (pp ord z zz e)
  (cond ((atom e) e)
        ((and (mexptp e) (eq (cadr e) zz) (not ($freeof ord (caddr e))))
         (mul (power z (sub (caddr e) ord)) pp))
        (t (simplifya (cons (remove 'simp (car e))
                            (mapcar #'(lambda (x)
                                        (subst-power-order pp ord z zz x))
                                    (cdr e)))
                      nil))))

(defun subst-rational-order (order ord z zz g)
  ($substitute order ord
               ($substitute z zz
                            (subst-power-order (if (eq $domain '$real)
                                                   (principal-power z order)
                                                   (power z order))
                                               ord z zz g))))

(def-simplifier gamma_incomplete_lower (a z)
  (cond
    ((or
```

**Hunk 3.** The rational-order clause of the `gamma_incomplete_lower` simplifier.

**Find this** (upstream line 612):

```lisp
		;; Use gamma_incomplete(a+n,z) above. and then substitute
		;; a=order.  This works for n positive or negative.
		(let* ((ord (gensym))
		       (g (simplify (list '(%gamma_incomplete_lower) (add ord n) z))))
		  ($substitute rat-order ord g)))))))
	(t
	 ;; No expansion so return nil to indicate that
	 nil)))
```

**Replace it with this:**

```lisp
		;; Use gamma_incomplete(a+n,z) above. and then substitute
		;; a=order.  This works for n positive or negative.
		(let* ((ord (gensym))
		       (zz (gensym))
		       (g (simplify (list '(%gamma_incomplete_lower) (add ord n) zz))))
		  (subst-rational-order rat-order ord z zz g)))))))
	(t
	 ;; No expansion so return nil to indicate that
	 nil)))
```

**Hunk 4.** The rational-order clause of the `gamma_incomplete` simplifier.

**Find this** (upstream line 915):

```lisp
	      ;; Use gamma_incomplete(a+n,z) above. and then substitute
	      ;; a=order.  This works for n positive or negative.
	      (let* ((ord (gensym))
		     (g (simplify (list '(%gamma_incomplete) (add ord n) z))))
		($substitute rat-order ord g)))))))

      ($hypergeometric_representation
       ;; See http://functions.wolfram.com/06.06.26.0002.01
```

**Replace it with this:**

```lisp
	      ;; Use gamma_incomplete(a+n,z) above. and then substitute
	      ;; a=order.  This works for n positive or negative.
	      (let* ((ord (gensym))
		     (zz (gensym))
		     (g (simplify (list '(%gamma_incomplete) (add ord n) zz))))
		(subst-rational-order rat-order ord z zz g)))))))

      ($hypergeometric_representation
       ;; See http://functions.wolfram.com/06.06.26.0002.01
```

**Hunk 5.** The `z1` and `z2` derivatives in the `defgrad` of `%gamma_incomplete_generalized`.

**Find this** (upstream line 1419):

```lisp
 +gamma(a)^2*hypergeometric_regularized([a,a],[a+1,a+1],-z1)*z1^a$

  ;; The derivative wrt z1
  #$$-(z1^(a-1)*%e^-z1)$

  ;; The derivative wrt z2
  #$$z2^(a-1)*%e^-z2$)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```

**Replace it with this:**

```lisp
 +gamma(a)^2*hypergeometric_regularized([a,a],[a+1,a+1],-z1)*z1^a$

  ;; The derivative wrt z1
  #'(lambda ($a $z1 $z2)
      (declare (ignore $z2))
      (gamma-incomplete-z-derivative $a $z1 -1))

  ;; The derivative wrt z2
  #'(lambda ($a $z1 $z2)
      (declare (ignore $z1))
      (gamma-incomplete-z-derivative $a $z2 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```

**Hunk 6.** The rational-order clause of the `gamma_incomplete_regularized` simplifier.

**Find this** (upstream line 1775):

```lisp
	      ;; then substitute a=order.  This works for n positive or
	      ;; negative.
	      (let* ((ord (gensym))
		     (g (simplify (list '(%gamma_incomplete_regularized) (add ord n) z))))
		($substitute rat-order ord g)))))))

      ($hypergeometric_representation
       ;; gamma_incomplete_regularized(a,z)
```

**Replace it with this:**

```lisp
	      ;; then substitute a=order.  This works for n positive or
	      ;; negative.
	      (let* ((ord (gensym))
		     (zz (gensym))
		     (g (simplify (list '(%gamma_incomplete_regularized) (add ord n) zz))))
		(subst-rational-order rat-order ord z zz g)))))))

      ($hypergeometric_representation
       ;; gamma_incomplete_regularized(a,z)
```

### 3.6 `src/mopers.lisp`

Two hunks, the comment above `defgrad` and its docstring: a lambda returning `t` as a second value has its result taken as it is.

**Hunk 1.**

**Find this** (upstream line 200):

```lisp
;;
;; The derivative forms can also be lambda's.  See gamma_incomplete.
;; But if the lambda also uses #$$...$, it MUST call meval* itself to
;; make sure the result is appropriately simplified.

(defmacro defgrad (name arguments &body body)
  "DEFGRAD defines derivatives for the function NAME having arguments ARGUMENTS.
```

**Replace it with this:**

```lisp
;;
;; The derivative forms can also be lambda's.  See gamma_incomplete.
;; But if the lambda also uses #$$...$, it MUST call meval* itself to
;; make sure the result is appropriately simplified.  A lambda that
;; builds its result from the actual arguments returns T as a second
;; value, so that SDIFFGRAD substitutes nothing into it.

(defmacro defgrad (name arguments &body body)
  "DEFGRAD defines derivatives for the function NAME having arguments ARGUMENTS.
```

**Hunk 2.**

**Find this** (upstream line 218):

```lisp
  quoted list of the maxima internal representation of the derivative.

  The derivative may also be a lambda expression that returns the
  derivative or NIL."
  ;; Check that the argument variables show up somewhere in the body.
  ;; Otherwise, the defintion of the derivative is potentially
  ;; incorrect.
```

**Replace it with this:**

```lisp
  quoted list of the maxima internal representation of the derivative.

  The derivative may also be a lambda expression that returns the
  derivative or NIL.  Its result is substituted into like an expression
  in the placeholder names, unless it returns T as a second value: the
  result is then taken as it is, which lets it be built from the actual
  arguments."
  ;; Check that the argument variables show up somewhere in the body.
  ;; Otherwise, the defintion of the derivative is potentially
  ;; incorrect.
```

### 3.7 `src/sin.lisp`

Three hunks. `gamma-incomplete-and-power` returns the `gamma_incomplete` factor (less `gamma(alpha)` where the phase is a step), the power, and the `var^m` factor, which is 1 when it has been folded into the power. Types 1a, 2 and 2-1 of `integrate-exp-special` call it in place of building the two factors themselves; the products keep the same factors in the same order as before, and the exponent of `z` is computed exactly as each clause computed it, since the form of a symbolic exponent decides how the simplifier combines the factors (rtest_integrate 98, 572 and 573 depend on it).

**Hunk 1.** Insert before `(defun integrate-exp-special (expr var2 &aux w const)`.

**Find this** (upstream line 2485):

```lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun integrate-exp-special (expr var2 &aux w const)

  ;; First factor the expression.
```

**Replace it with this:**

```lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gamma_incomplete(alpha, z) and var^m*z^s, for s = -alpha, of the
;; antiderivatives below: (values gamma power var^m), with var^m folded
;; into the power and 1 in its place where the treatment applies.  A power
;; of z = k*var^n does not survive the simplifier when alpha has an odd
;; denominator: with domain : real it takes the real root of a negative
;; base, as (-x^3)^(-1/3) becomes -1/x, and its (a*b)^s = a^s*b^s leaves
;; the principal branch for a negative var, while gamma_incomplete stays
;; on that branch; so the power is written by PRINCIPAL-POWER-TIMES.  Its
;; phase is then one constant for a positive var and another for a
;; negative one when the sign of var^n is that of var, and the
;; antiderivative, gamma(alpha) times the phase at var = 0 where alpha > 0
;; and z goes to 0 with var, would jump there: gamma(alpha) is taken from
;; gamma_incomplete to make it 0 at var = 0 from both sides.
(defun gamma-incomplete-and-power (alpha s z var m)
  (multiple-value-bind (k w n nonneg) (real-power-factors z)
    (declare (ignore w))
    (if (and k (odd-root-p alpha))
        (let ((g (take '(%gamma_incomplete) alpha z)))
          (values (if (and (not nonneg)
                           (sign-varies-p n)
                           (eq ($sign n) '$pos)
                           (eq ($sign alpha) '$pos))
                      (sub g (take '(%gamma) alpha))
                      g)
                  (principal-power-times var m z s)
                  1))
        (values (take '(%gamma_incomplete) alpha z)
                (power z s)
                (power var m)))))

(defun integrate-exp-special (expr var2 &aux w const)

  ;; First factor the expression.
```

**Hunk 2.** Type 1a.

**Find this** (upstream line 2501):

```lisp
     (when *debug-integrate*
       (format t "~&Type 1a: (a^(c*(z^r)^p+d)^v : w = ~A~%" w))

     (mul -1
	  const
	  ;; 1/(p*r*(a^(c*v*(var2^r)^p)))
	  (inv (mul p r (power a (mul c v (power (power var2 r) p)))))
	  var2
	  ;; (a^(d+c*(var2^r)^p))^v
	  (power (power a (add d (mul c (power (power var2 r) p)))) v)
	  ;; gamma_incomplete(1/(p*r), -c*v*(var2^r)^p*log(a))
	  (take '(%gamma_incomplete)
		(inv (mul p r))
		(mul -1 c v (power (power var2 r) p) (take '(%log) a)))
	  ;; (-c*v*(var2^r)^p*log(a))^(-1/(p*r))
	  (power (mul -1 c v (power (power var2 r) p) (take '(%log) a))
		 (div -1 (mul p r)))))

    ((m2-exp-type-2 (facsum-exponent expr var2) var2)
     (a b d v r)
```

**Replace it with this:**

```lisp
     (when *debug-integrate*
       (format t "~&Type 1a: (a^(c*(z^r)^p+d)^v : w = ~A~%" w))

     (multiple-value-bind (g pw vp)
	 (gamma-incomplete-and-power
	  (inv (mul p r))
	  (div -1 (mul p r))
	  (mul -1 c v (power (power var2 r) p) (take '(%log) a))
	  var2 1)
       (mul -1
	    const
	    ;; 1/(p*r*(a^(c*v*(var2^r)^p)))
	    (inv (mul p r (power a (mul c v (power (power var2 r) p)))))
	    vp
	    ;; (a^(d+c*(var2^r)^p))^v
	    (power (power a (add d (mul c (power (power var2 r) p)))) v)
	    g pw)))

    ((m2-exp-type-2 (facsum-exponent expr var2) var2)
     (a b d v r)
```

**Hunk 3.** Types 2 and 2-1, which are adjacent.

**Find this** (upstream line 2522):

```lisp
     (when *debug-integrate*
       (format t "~&Type 2: z^v*a^(b*z^r+d) : w = ~A~%" w))

     (mul
      const
      (div -1 r)
      (power a d)
      (power var2 (add v 1))
      ($gamma_incomplete
       (div (add v 1) r)
       (mul -1 b (power var2 r) ($log a)))
      (power
       (mul -1 b (power var2 r) ($log a))
       (mul -1 (div (add v 1) r)))))

    ((m2-exp-type-2-1 (facsum-exponent expr var2) var2)
     (a b v r u)
     (when *debug-integrate*
       (format t "~&Type 2-1: z^v*(%e^(a*z^r+b))^u : w = ~A~%" w))

     (mul const
          -1
          (inv r)
          (power '$%e (mul -1 a u (power var2 r)))
          (power (power '$%e (add (mul a (power var2 r)) b)) u)
          (power var2 (add v 1))
          (power (mul -1 a u (power var2 r)) (div (mul -1 (add v 1)) r))
          (take '(%gamma_incomplete)
                (div (add v 1) r)
                (mul -1 a u (power var2 r)))))
    
    ((m2-exp-type-3 (expand-base-of-exp (facsum-exponent expr var2) var2) var2)
      (a b c d p)
```

**Replace it with this:**

```lisp
     (when *debug-integrate*
       (format t "~&Type 2: z^v*a^(b*z^r+d) : w = ~A~%" w))

     (multiple-value-bind (g pw vp)
         (gamma-incomplete-and-power (div (add v 1) r)
                                     (mul -1 (div (add v 1) r))
                                     (mul -1 b (power var2 r) ($log a))
                                     var2 (add v 1))
       (mul const (div -1 r) (power a d) vp g pw)))

    ((m2-exp-type-2-1 (facsum-exponent expr var2) var2)
     (a b v r u)
     (when *debug-integrate*
       (format t "~&Type 2-1: z^v*(%e^(a*z^r+b))^u : w = ~A~%" w))

     (multiple-value-bind (g pw vp)
         (gamma-incomplete-and-power (div (add v 1) r)
                                     (div (mul -1 (add v 1)) r)
                                     (mul -1 a u (power var2 r))
                                     var2 (add v 1))
       (mul const
            -1
            (inv r)
            (power '$%e (mul -1 a u (power var2 r)))
            (power (power '$%e (add (mul a (power var2 r)) b)) u)
            vp pw g)))
    
    ((m2-exp-type-3 (expand-base-of-exp (facsum-exponent expr var2) var2) var2)
      (a b c d p)
```

### 3.8 `doc/info/Simplification.texi`

The entry of the `domain` option variable gets a paragraph. Check the manual with `make` and `?? domain` afterwards.

**Hunk 1.** After the sentence about `sqrt (x^2)`.

**Find this** (upstream line 605):

```texinfo
When @code{domain} is set to @code{complex}, @code{sqrt (x^2)} will remain
@code{sqrt (x^2)} instead of returning @code{abs(x)}.

@c PRESERVE EDITORIAL COMMENT -- MAY HAVE SOME SIGNIFICANCE NOT YET UNDERSTOOD !!!
@c The notion of a "domain" of simplification is still in its infancy,
@c and controls little more than this at the moment.
```

**Replace it with this:**

```texinfo
When @code{domain} is set to @code{complex}, @code{sqrt (x^2)} will remain
@code{sqrt (x^2)} instead of returning @code{abs(x)}.

When @code{domain} is set to @code{real}, the default, a real expression
raised to a power with an integer numerator and an odd denominator, as in
@code{x^(1/3)}, @code{x^(2/3)} or @code{x^(1/n)} with @code{n} declared
@code{odd}, is taken to be the real root: @code{(-8)^(1/3)} simplifies to
@code{-2} and @code{(x^3)^(1/3)} to @code{x}, and @mref{rectform},
@mref{realpart}, @mref{imagpart}, @mref{polarform} and @mref{carg} treat
such a power of a real quantity as real.  @mref{integrate}, the derivative
of @mref{gamma_incomplete} and its expansion with @code{gamma_expand} keep
to the principal branch of @code{gamma_incomplete}, and write the power of
its argument that comes with it, as @code{(-x^3)^(-1/3)} with
@code{gamma_incomplete(1/3, -x^3)}, in terms of @code{x/abs(x)}.  When
@code{domain} is set to @code{complex}, such a power is on the principal
branch, and @code{(-8)^(1/3)} is @code{1 + sqrt(3) %i}; see also
@mrefdot{m1pbranch}

@c PRESERVE EDITORIAL COMMENT -- MAY HAVE SOME SIGNIFICANCE NOT YET UNDERSTOOD !!!
@c The notion of a "domain" of simplification is still in its infancy,
@c and controls little more than this at the moment.
```

### 3.9 `ChangeLog`

Two hunks: under *Bug fixes for unnumbered bugs*, and two lines under *Other changes*.

**Hunk 1.**

**Find this** (upstream line 72):

```text

Bug fixes for unnumbered bugs:
------------------------------
* tlimit never asks the sign questions that limit asks
* limits of atanh at an infinity ignore the imaginary part of the argument

Changes in the Windows installer:
---------------------------------
```

**Replace it with this:**

```text

Bug fixes for unnumbered bugs:
------------------------------
* abs(x)^(2/3) no longer simplifies to x^(2/3) with domain : complex, where x^(2/3) is on the principal branch
* rectform, realpart, imagpart, polarform and carg take the principal branch of an odd root of a negative real quantity, where the simplifier takes the real root with domain : real
* csign(x^(m/n)) with an integer m and n declared odd is complex for x < 0 with domain : real, where the power is the real root (x^(1/n))^m
* integrate(%e^(x^3), x) and integrate(sin(x^3), x) are wrong for x > 0: the (-x^3)^(-1/3) of the integrator is taken as the real root while gamma_incomplete stays on the principal branch
* integrate(x*%e^(x^3), x), and the definite integrals that substitute in the integrand, as integrate(%e^(x^3), x, -1, 1) and integrate(exp(sqrt(x^3)), x, 0, 1), take the real root of the power that comes with gamma_incomplete the same way
* diff(gamma_incomplete(a, z), x) and gamma_incomplete(a, z) with gamma_expand take the real root of z^(a-1) with domain : real for a z such as -x^3, where the function is on the principal branch: the antiderivative of %e^(x^3) did not differentiate back to it
* tlimit never asks the sign questions that limit asks
* limits of atanh at an infinity ignore the imaginary part of the argument
* the derivative of gamma_incomplete with respect to its first argument picks up values of the variables a and z

Changes in the Windows installer:
---------------------------------
```

**Hunk 2.**

**Find this** (upstream line 82):

```text
Other changes:
--------------
* Bigfloat optimizations and less memory consumption
```

**Replace it with this:**

```text
Other changes:
--------------
* Bigfloat optimizations and less memory consumption
* The sign of a real x has one normal form: abs(x)/x now simplifies to x/abs(x)
* x^(2/3)*abs(x)^(1/3) simplifies to abs(x), so (x^2*abs(x))^(1/3) and abs(x^3)^(1/3) do too
```

## 4. Tests

Each block is given as the same kind of patch. New blocks in `rtest16.mac`, `rtest_sign.mac` and `rtest_integrate.mac` are appended, before the final environment check of the file where it has one, so that the registered problem numbers of `src/testsuite.lisp` do not move; the changed expectations replace a single answer each.

### 4.1 `tests/rtest16.mac`

Two hunks. Problem 990 (bug #3082) pinned the real-root derivative `-7*%e^(x^7+4)/x^5`; the principal-branch one replaces it, and the numerical check that tells them apart is in `rtest_gamma.mac`. The block appended before the final facts check covers `rectform`, `realpart`, `imagpart`, `polarform`, `carg` and `abs` of odd roots, declared-odd denominators, even denominators, and `domain : complex`.


**Hunk 1.**

**Find this** (upstream line 4011):

```maxima
/* Bug #3082: "incorrect simplification of MTIMES expression containing MEXPT" */

block([x], diff(%e^4*gamma_incomplete(-4/7, -x^7), x));
-7*%e^(x^7+4)/x^5;

block([x], 2*x*%e^x*%e^2);
```

**Replace it with this:**

```maxima
/* Bug #3082: "incorrect simplification of MTIMES expression containing MEXPT" */

/* with domain : real the (-x^7)^(-11/7) of the derivative is on the principal
   branch of gamma_incomplete, as an expression in x/abs(x): see rtest_gamma */
block([x], diff(%e^4*gamma_incomplete(-4/7, -x^7), x));
(7*%e^(x^7+4)*(((%e^((3*%i*%pi)/7)/2-1/2)*x)/abs(x)+%e^((3*%i*%pi)/7)/2+1/2))/(x^4*abs(x));

block([x], 2*x*%e^x*%e^2);
```

**Hunk 2.**

**Find this** (upstream line 4291):

```maxima

/**************************************/
/* Leave this at the end of the file! */
/**************************************/
```

**Replace it with this:**

```maxima

/**************************************/
/* rectform, realpart, imagpart, polarform and carg take the same branch as
   the simplifier for an odd root of a real quantity with domain : real:
   (-8)^(1/3) simplifies to -2, so x^(1/3) is real for every real x. */

(assume(xn < 0, yn < 0), 0);
0$

rectform(xn^(1/3));
xn^(1/3)$

[realpart(xn^(1/3)), imagpart(xn^(1/3))];
[xn^(1/3), 0]$

[rectform(xn^(2/3)), rectform(xn^(-1/3)), rectform(xn^(7/5))];
[xn^(2/3), 1/xn^(1/3), xn^(7/5)]$

/* carg and polarform agree with each other and with the simplifier */
[carg(xn^(1/3)), cabs(xn^(1/3)),
 block([%emode : false], is(polarform(xn^(1/3)) = -%e^(%i*%pi)*xn^(1/3)))];
[%pi, -xn^(1/3), true]$

/* The simplifier distributes an odd root over a product on the real
   branch only, and the rectangular form has to be on the same branch. */
rectform((xn*yn)^(1/3));
xn^(1/3)*yn^(1/3)$

subst(xn = -8, rectform(xn^(1/3)));
-2$

/* an unknown sign */
[rectform(x^(1/3)), carg(x^(1/3))];
[x^(1/3), atan2(0, x^(1/3))]$

/* A power that was simplified with domain : complex */
rectform(block([domain : complex], (-8)^(1/3)));
-2$

/* Even denominators stay on the principal branch */
[rectform(sqrt(xn)), rectform(xn^(1/6))];
[%i*sqrt(-xn), sqrt(3)*(-xn)^(1/6)/2 + %i*(-xn)^(1/6)/2]$

/* domain : complex is unchanged */
block([domain : complex],
      [rectform(xn^(1/3)) - ((-xn)^(1/3)/2 + sqrt(3)*%i*(-xn)^(1/3)/2),
       rectform((-8)^(1/3)) - (1 + sqrt(3)*%i)]);
[0, 0]$

/* An exponent whose denominator is only declared odd, with an integer
   numerator, is a real root too, as csign says; an even or an unknown
   parity is not. */
(declare(nodd, odd, mint, integer), 0);
0$

[rectform(xn^(1/nodd)), imagpart(xn^(mint/nodd)), carg(xn^(1/nodd)),
 rectform(xn^(2/nodd))];
[xn^(1/nodd), 0, %pi, xn^(2/nodd)]$

subst(nodd = 3, rectform(xn^(1/nodd)));
xn^(1/3)$

[is(imagpart(xn^(1/(2*mint))) = 0), is(imagpart(xn^(1/mint)) = 0)];
[false, false]$

/* abs, cabs, signum, polarform and carg follow csign */
[abs(xn^(2/nodd)), abs(xn^(mint/nodd)), cabs(xn^(mint/nodd)),
 signum(xn^(2/nodd)), carg(xn^(2/nodd)), polarform(xn^(2/nodd)),
 is(xn^(2/nodd) > 0)];
[xn^(2/nodd), (-xn)^(mint/nodd), (-xn)^(mint/nodd), 1, 0, (-xn)^(2/nodd), true];

(remove(nodd, odd), remove(mint, integer), forget(xn < 0, yn < 0), 0);
0$


/* Leave this at the end of the file! */
/**************************************/
```

### 4.2 `tests/rtest_sign.mac`

One block appended before the final facts check: `csign` and `sign` of `b^(m/n)` for a declared odd `n`, in real mode and under both domains.


**Hunk 1.**

**Find this** (upstream line 2475):

```maxima

/**************************************/
/* Leave this at the end of the file! */
/**************************************/
```

**Replace it with this:**

```maxima

/**************************************/
/* With domain : real, x^(m/n) with an integer m and an odd n is the real
   root (x^(1/n))^m, and csign judges it by the parity of m, as it does
   x^(2/3) and x^(1/3); a denominator merely declared odd counts, an
   unknown numerator does not. */

block([b, n, m, p, u],
 local(b, n, m, p, u),
 declare(n, odd, m, integer, p, odd),
 assume(b < 0),
 [csign(b^(2/n)), csign(b^(m/n)), csign(b^(p/n)), csign(b^(-2/n)),
  csign(b^(1/n)), csign(b^(u/n)), csign(b^(1/m)), csign(b^(1/(2*n))),
  csign((-8)^(2/n)), csign((-8)^(p/n))]);
[pos, pnz, pn, pos, neg, complex, complex, complex, pos, pn];

block([b, n, m, p],
 local(b, n, m, p),
 declare(n, odd, m, integer, p, odd),
 [csign(b^(2/n)), csign(b^(m/n)), csign(b^(p/n)), csign(b^(-2/n)),
  sign(b^(2/n)), sign(b^(p/n))]);
[pz, pnz, pnz, pz, pz, pnz];

/* sign answers the same in real mode, and there regardless of domain, as
   it does for x^(2/3); csign returns to complex with domain : complex */
block([b, n, p],
 local(b, n, p),
 declare(n, odd, p, odd),
 assume(b < 0),
 [sign(b^(2/n)), sign(b^(p/n)),
  block([domain : complex], [sign(b^(2/n)), csign(b^(2/n))])]);
[pos, pn, [pos, complex]];


/* Leave this at the end of the file! */
/**************************************/
```

### 4.3 `tests/rtest_gamma.mac`

One block appended at the end, in two parts. The first belongs to the `sdiffgrad` fix: the derivative of `gamma_incomplete` with respect to its order with the variables `a` and `z` given values, and with the arguments named `z` and `a`. The second is the derivative of `gamma_incomplete(1/3, -x^3)`, checked at `x = 2` against `3*%e^8*%e^(-2*%i*%pi/3)`, the cases that must stay as they were, the `gamma_expand` recurrence checked at `x = -2` against `gamma_incomplete` itself, and the derivative again with the variable named `z`.


**Hunk 1.**

**Find this** (upstream line 4354):

```maxima
limit(erf_generalized(x, 1/x), x, 0, minus);
-1;
```

**Replace it with this:**

```maxima
limit(erf_generalized(x, 1/x), x, 0, minus);
-1;

/* The derivative of gamma_incomplete with respect to its first argument is
   built from the actual arguments.  A value of a or z, the placeholders in
   its definition, must not leak into it, and arguments named like them
   must not be confused with them. */
(kill(a, b, y, z), a : 5, z : 7, assume(b > 0), 0);
0$

diff(gamma_incomplete(b, y), b);
(gamma_incomplete(b,y)-gamma(b))*log(y)+gamma(b)^2*hypergeometric_regularized([b,b],[b+1,b+1],-y)*y^b+psi[0](b)*gamma(b)$

diff(gamma_incomplete(b, y), y);
-(%e^-y*y^(b-1))$

(remvalue(a, z), assume(z > 0), 0);
0$

diff(gamma_incomplete(z, a), z);
(gamma_incomplete(z,a)-gamma(z))*log(a)+gamma(z)^2*hypergeometric_regularized([z,z],[z+1,z+1],-a)*a^z+psi[0](z)*gamma(z)$

diff(gamma_incomplete(z, a), a);
-(%e^-a*a^(z-1))$

(forget(b > 0, z > 0), 0);
0$

/* With domain : real, diff and gamma_expand keep gamma_incomplete(a, z) on
   the principal branch of z^(a-1) for a z, as -x^3 or x^3, that the
   simplifier would take the real root of: the power is written as an
   expression in x/abs(x). */
diff(gamma_incomplete(1/3, -x^3), x);
3*%e^x^3*(((-((sqrt(3)*%i)/4)-3/4)*x)/abs(x)-(sqrt(3)*%i)/4+1/4)$

is(cabs(float(rectform(subst(x = 2, diff(gamma_incomplete(1/3, -x^3), x))
                       - 3*%e^8*(-1/2 - sqrt(3)*%i/2)))) < 1e-6);
true$

/* nothing to do for a symbolic order, an even denominator, or x alone */
[diff(gamma_incomplete(a, -x^3), x), diff(gamma_incomplete(1/2, -x^3), x),
 diff(gamma_incomplete(1/3, x), x)];
[3*%e^x^3*x^2*(-x^3)^(a-1), (3*%e^x^3*x^2)/sqrt(-x^3), -(%e^-x/x^(2/3))]$

block([gamma_expand : true], gamma_incomplete(-1/3, x^3));
(3*%e^-x^3*(((3/4-(sqrt(3)*%i)/4)*x)/abs(x)+(sqrt(3)*%i)/4+1/4))/x-3*gamma_incomplete(2/3,x^3)$

is(cabs(float(rectform(subst(x = -2, block([gamma_expand : true],
                                            gamma_incomplete(-1/3, x^3)))
                       - gamma_incomplete(-1/3, -8)))) < 1e-9);
true$

/* the variable named like a placeholder of the derivative rule */
diff(gamma_incomplete(1/3, -z^3), z);
3*%e^z^3*(((-((sqrt(3)*%i)/4)-3/4)*z)/abs(z)-(sqrt(3)*%i)/4+1/4)$
```

### 4.4 `tests/rtest_integrate.mac`

Two blocks appended, one before and one after the final facts check that closes the file. The first pins the antiderivatives of `%e^(x^3)` and `sin(x^3)` for a positive variable and checks them, and `%e^(-1/x^3)`, against `quad_qags` on both sides of 0, across 0 and under `domain : complex`. The second, extended at its end by the round trips with a constant coefficient in the exponent and with the variable named `z` or `a`, checks that `expand` differentiates the antiderivatives back to their integrands and `trigrat` does for `sin(x^3)`, pins `integrate(x*%e^(x^3), x)`, checks the definite integrals through 0 and of `exp(sqrt(x^3))` and `expintegral_ei(x^(-3/4))` numerically, and that infinite limits are as they were.


**Hunk 1.**

**Find this** (upstream line 6862):

```maxima


/**************************************/
/* Leave this at the end of the file! */
```

**Replace it with this:**

```maxima


/* integrate took the real root of the (-x^3)^(-1/3) in its gamma_incomplete
   formula, which gamma_incomplete itself does not, so the antiderivatives of
   %e^(x^3) and sin(x^3) were wrong for x > 0, and that of sin(x^3) for x < 0
   with domain : complex as well. */

(assume(xp > 0), 0);
0$

integrate(%e^(xp^3), xp);
-(((1/2-(sqrt(3)*%i)/2)*(gamma_incomplete(1/3,-xp^3)-gamma(1/3)))/3)$

integrate(sin(xp^3), xp);
-(((sqrt(3)*%i+1)*gamma_incomplete(1/3,%i*xp^3)
   +(1-sqrt(3)*%i)*gamma_incomplete(1/3,-(%i*xp^3))-2*gamma(1/3))/12)$

(forget(xp > 0), 0);
0$

/* on both sides of 0 and across it, where the antiderivative must not jump,
   and with domain : complex, against quad_qags */
(chk(I, f) := block([a : float(rectform(subst(x = 1.5, I) - subst(x = 0.5, I))),
                     b : float(rectform(subst(x = -0.5, I) - subst(x = -1.5, I))),
                     c : float(rectform(subst(x = 0.5, I) - subst(x = -0.5, I)))],
              [is(abs(a - quad_qags(f, x, 0.5, 1.5)[1]) < 1e-9),
               is(abs(b - quad_qags(f, x, -1.5, -0.5)[1]) < 1e-9),
               is(abs(c - quad_qags(f, x, -0.5, 0.5)[1]) < 1e-9)]), 0);
0$

[chk(integrate(%e^(x^3), x), %e^(x^3)), chk(integrate(sin(x^3), x), sin(x^3))];
[[true, true, true], [true, true, true]]$

block([domain : complex],
      [chk(integrate(%e^(x^3), x), %e^(x^3)), chk(integrate(sin(x^3), x), sin(x^3))]);
[[true, true, true], [true, true, true]]$

/* a negative odd power: the gamma_incomplete of 1/x^3 is 0 at x = 0 already */
block([r : integrate(%e^(-1/x^3), x)],
      is(abs(float(rectform(subst(x = -0.5, r) - subst(x = -1.5, r)))
             - quad_qags(%e^(-1/x^3), x, -1.5, -0.5)[1]) < 1e-9));
true$

(remfunction(chk), 0);
0$


/**************************************/
/* Leave this at the end of the file! */
```

**Hunk 2.**

**Find this** (upstream line 6868):

```maxima
[facts(), contexts];
[[], [initial, global]]$
```

**Replace it with this:**

```maxima
[facts(), contexts];
[[], [initial, global]]$

/* The antiderivatives above differentiate back to their integrands: with
   domain : real, diff keeps gamma_incomplete(a, z) on the principal branch
   of z^(a-1) too, as an expression in x/abs(x) that expand multiplies out;
   sin(x^3) comes back through complex exponentials, which trigrat folds. */
expand(diff(integrate(%e^(x^3), x), x) - %e^(x^3));
0$

expand(diff(integrate(x*%e^(-x^3), x), x) - x*%e^(-x^3));
0$

expand(diff(integrate(%e^(-1/x^3), x), x) - %e^(-1/x^3));
0$

trigrat(diff(integrate(sin(x^3), x), x) - sin(x^3));
0$

/* x*%e^(x^3), and definite integrals through 0 and of exp(sqrt(x^3)), which
   substitute in the integrand and go through the type 2 formula */
integrate(x*%e^(x^3), x);
-(((gamma_incomplete(2/3,-x^3)-gamma(2/3))*(((-((sqrt(3)*%i)/4)-3/4)*x)/abs(x)-(sqrt(3)*%i)/4+1/4))/3)$

is(cabs(float(rectform(integrate(%e^(x^3), x, -1, 1)))
        - quad_qags(%e^(x^3), x, -1, 1)[1]) < 1e-9);
true$

is(cabs(float(rectform(integrate(x*%e^(x^3), x, -1, 1)))
        - quad_qags(x*%e^(x^3), x, -1, 1)[1]) < 1e-9);
true$

is(cabs(float(rectform(integrate(exp(sqrt(x^3)), x, 0, 1)))
        - quad_qags(exp(sqrt(x^3)), x, 0, 1)[1]) < 1e-9);
true$

block([r : integrate(expintegral_ei(x^(-3/4)), x)],
      is(cabs(float(rectform(subst(x = 3, r) - subst(x = 2, r)))
              - quad_qags(expintegral_ei(x^(-3/4)), x, 2, 3)[1]) < 1e-9));
true$

/* infinite limits are as they were */
[integrate(%e^(-x^3), x, 0, inf), integrate(x*%e^(-x^3), x, 0, inf)];
[gamma(1/3)/3, gamma(2/3)/3]$

/* a constant coefficient in the exponent: the derivative of gamma_incomplete
   keeps it outside the root, in the form the integrator gives it */
expand(diff(integrate(%e^(2*x^3), x), x) - %e^(2*x^3));
0$

expand(diff(integrate(%e^(-x^3/2), x), x) - %e^(-x^3/2));
0$

expand(diff(integrate(%e^(3*x^5), x), x) - %e^(3*x^5));
0$

trigrat(diff(integrate(sin(2*x^3), x), x) - sin(2*x^3));
0$

/* the variable named like a placeholder of the derivative rule */
[expand(diff(integrate(%e^(z^3), z), z)), trigrat(diff(integrate(sin(a^3), a), a))];
[%e^z^3, sin(a^3)]$
```

### 4.5 `tests/rtest_integrate_special.mac`

Problems 27 and 31 pinned antiderivatives that are wrong for `x > 0`; the derivative of the pinned answer of 31 at `x = 2` equals the integrand only because `diff` made the same mistake.


**Hunk 1.** Problem 27.

**Find this** (upstream line 96):

```maxima

integrate(expintegral_ei(x^3),x);
x*expintegral_ei(x^3)-gamma_incomplete(1/3,-x^3);

integrate(expintegral_ei(x^-2),x);
```

**Replace it with this:**

```maxima

integrate(expintegral_ei(x^3),x);
x*expintegral_ei(x^3)+(gamma_incomplete(1/3,-x^3)-gamma(1/3))*(-((sqrt(3)*%i*x)/(4*abs(x)))+(3*x)/(4*abs(x))-(sqrt(3)*%i)/4-1/4);

integrate(expintegral_ei(x^-2),x);
```

**Hunk 2.** Problem 31.

**Find this** (upstream line 108):

```maxima

integrate(expintegral_ei(x^(-3/4)),x);
4*(expintegral_ei(1/x^(3/4))*x/4+gamma_incomplete(-4/3,-1/x^(3/4))/4);

integrate(expintegral_ei((x+1)^(1/2)),x);
```

**Replace it with this:**

```maxima

integrate(expintegral_ei(x^(-3/4)),x);
4*((expintegral_ei(1/x^(3/4))*x)/4+((-((sqrt(3)*%i)/2)-1/2)*gamma_incomplete(-(4/3),-(1/x^(3/4))))/4);

integrate(expintegral_ei((x+1)^(1/2)),x);
```

### 4.6 `tests/rtestint.mac`

Problem 100 runs under `domain : complex`; its answer was numerically right and only changes form, the `(-1)^(1/3)` of the integrator now multiplied out.


**Hunk 1.** Problem 100.

**Find this** (upstream line 852):

```maxima
/* [ 1731624 ] asked about sign of yx in integral containing only z */
integrate(exp(sqrt(x^3)),x,0,1);
2*((-1)^(1/3)*gamma_incomplete(2/3,-1)-(-1)^(1/3)*gamma(2/3))/3;
/* with radexpand:true and domain:real we get
   integrate(exp(sqrt(x^3)),x)  ->  -2*gamma_incomplete(2/3,-x^(3/2))/3
```

**Replace it with this:**

```maxima
/* [ 1731624 ] asked about sign of yx in integral containing only z */
integrate(exp(sqrt(x^3)),x,0,1);
/* 2*((-1)^(1/3)*gamma_incomplete(2/3,-1)-(-1)^(1/3)*gamma(2/3))/3 with the
   phase (-1)^(1/3) of the integrator multiplied out */
-((-(sqrt(3)*%i*gamma_incomplete(2/3,-1))-gamma_incomplete(2/3,-1)+(sqrt(3)*%i+1)*gamma(2/3))/3);
/* with radexpand:true and domain:real we get
   integrate(exp(sqrt(x^3)),x)  ->  -2*gamma_incomplete(2/3,-x^(3/2))/3
```

### 4.7 `tests/rtest_abs.mac`

Three blocks appended before the final leak guard: the `simpexpt` fix (`abs(x)^(2/3)` stays with `domain : complex`, is 4 at `x = -8` there, and even integer and declared even powers still become powers of `x`), the normal form of the sign, and the combination of `x^(2/3)*abs(x)^(1/3)`, with the declared-complex `z` and `domain : complex` left alone. The file is registered without known-failure numbers, so the shift of the two guard problems is harmless. Where an expected value would be simplified into something else with the default domain, the problem returns a string, because the harness simplifies the expected side at comparison time.


**Hunk 1.**

**Find this** (upstream line 546):

```maxima


/***********************************************************/
/* Guard against leaks, leave this at the end of the file! */
```

**Replace it with this:**

```maxima


/* abs(x)^(2/3) is x^(2/3) for the real root, which x^(2/3) is with
 * domain : real and not with domain : complex; an even integer power and
 * a declared even one are x^n in both.  The expected value of the first
 * problem is a string, as the comparison would simplify it with the
 * default domain.
 */

block([domain : complex], string(abs(x)^(2/3)));
"abs(x)^(2/3)";

block([domain : complex], subst(x = -8, abs(x)^(2/3)));
4;

block([domain : complex], [abs(x)^2, abs(x)^4, abs(x)^(-2)]);
[x^2, x^4, 1/x^2];

(declare(n, even), block([domain : complex], abs(x)^n));
x^n;

remove(n, even);
done;

[abs(x)^(2/3), subst(x = -8, abs(x)^(2/3)), abs(x)^(4/5)];
[x^(2/3), 4, x^(4/5)];

/* One normal form for the sign of a real x: abs(x)*x^(-1) is x*abs(x)^(-1) */

abs(x)/x;
x/abs(x);

x/abs(x) - abs(x)/x;
0;

expand((1 + x/abs(x))*(1 - abs(x)/x));
0;

abs(x-1)/(x-1);
(x-1)/abs(x-1);

abs(sin(x))/sin(x);
sin(x)/abs(sin(x));

/* A declared complex z keeps both forms, they are not equal for it. */

(declare(z, complex), [abs(z)/z, z/abs(z)]);
[abs(z)/z, z/abs(z)];

remove(z, complex);
done;

/* x^a*abs(x)^b combines into abs(x)^(a+b) for a ratio a with an even
 * numerator, so that a root of abs(x^n) is a power of abs(x) again.
 */

(x^2*abs(x))^(1/3);
abs(x);

abs(-x^3)^(-1/3);
1/abs(x);

abs(x^3)^(2/3);
x^2;

x^(2/3)*abs(x)^(1/3);
abs(x);

x^(2/3)/abs(x);
1/abs(x)^(1/3);

subst(x = -8, (x^2*abs(x))^(1/3));
8;

/* The normal forms of an integer power are not touched. */

[x^2*abs(x), abs(x)^(5/3)];
[x^2*abs(x), abs(x)^(5/3)];

/* A declared complex z keeps the two factors apart. */

(declare(z, complex), [(z^2*abs(z))^(1/3), z^(2/3)*abs(z)^(1/3), abs(z^3)^(1/3)]);
[z^(2/3)*abs(z)^(1/3), z^(2/3)*abs(z)^(1/3), abs(z)];

remove(z, complex);
done;

/* With domain : complex x^(2/3) is on the principal branch and is not
 * abs(x)^(2/3); the expected value is a string, as the comparison would
 * simplify it with the default domain.
 */

block([domain : complex], string(x^(2/3)*abs(x)^(1/3)));
"x^(2/3)*abs(x)^(1/3)";


/***********************************************************/
/* Guard against leaks, leave this at the end of the file! */
```

### 4.8 `share/contrib/integration/rtest_abs_integrate.mac`

Problem 219 pinned its answer with the sign as `abs(x-a)/(x-a)`; same value, new form.


**Hunk 1.** Problem 219.

**Find this** (upstream line 730):

```maxima

hypergeometric_simp(hyper_int(x*abs(x-a),x));
-(a*x^2*(1-(2*x)/(3*a))*abs(x-a))/(2*(x-a))$

/* #3863 Wrong result for abs_integrate applied to unit_step */
```

**Replace it with this:**

```maxima

hypergeometric_simp(hyper_int(x*abs(x-a),x));
-(a*x^2*(x-a)*(1-(2*x)/(3*a)))/(2*abs(x-a))$

/* #3863 Wrong result for abs_integrate applied to unit_step */
```

## 5. Verifying

```sh
make
test src/rpart.lisp -nt src/binary-sbcl/maxima.core && echo 'EDIT NOT IN IMAGE'
./maxima-local --no-init -q --batch-string='run_testsuite(tests=["rtest_abs","rtest16","rtest_sign","rtest_gamma","rtestint","rtest_integrate","rtest_integrate_special","rtest_abs_integrate"], share_tests=true);'
./maxima-local --no-init -q --batch-string='run_testsuite(share_tests=true);'
```

The share run matters: `share/contrib/diffequations/rtest_odelin.mac` problem 11 is the test that showed why the case `k > 0`, `n = 1` has to be left to the simplifier, and `rtest_gamma.mac` problem 789 is the one that needs the `gamma_expand` recurrence on the same branch as the integrator.

A few things to try by hand:

```
integrate(%e^(x^3), x);
   -(((gamma_incomplete(1/3,-x^3)-gamma(1/3))*(-((sqrt(3)*%i*x)/(4*abs(x)))+(3*x)/(4*abs(x))-(sqrt(3)*%i)/4-1/4))/3)
expand(diff(%, x));                       %e^x^3
trigrat(diff(integrate(sin(x^3), x), x)); sin(x^3)
float(rectform(integrate(%e^(x^3), x, -1, 1)));   2.1494156001170914
assume(x > 0)$ integrate(%e^(x^3), x);
   -(((1/2-(sqrt(3)*%i)/2)*(gamma_incomplete(1/3,-x^3)-gamma(1/3)))/3)
```

## 6. What changes for users, and what does not

- Results of `integrate` that involve `gamma_incomplete(a, z)` with an odd denominator of `a` now carry a factor rational in `x` and `abs(x)`, such as `(-((sqrt(3)*%i*x)/(4*abs(x)))+(3*x)/(4*abs(x))-(sqrt(3)*%i)/4-1/4)`, and `gamma(a)` is subtracted from `gamma_incomplete(a, z)` where the phase is a step at 0. With the sign of the variable known the factor collapses to a constant and the result looks as it did, up to the `gamma(a)`.
- `diff(gamma_incomplete(a, z), x)` and `gamma_incomplete(a, z)` with `gamma_expand : true` change the same way, only with `domain : real`, a numeric `a`, and a `z` for which the simplifier commits to the real root: `diff(gamma_incomplete(1/3, x), x)`, a symbolic order, and an even denominator are unchanged.
- The round trip `diff(integrate(f, x), x)` needs `expand` in general, since Maxima never multiplies out a product of two sums by itself, `trigrat` for the trigonometric integrands, and one more `ratsimp` if `ratsimp` is used instead of `expand`, because `abs(x)^2` is reduced to `x^2` only on the way out of the rational form. `integrate(2^(x^3), x)` differentiates to `%e^(log(2)*x^3)`, which `ratsimp` does not identify with `2^(x^3)`; that is how the old code behaved too.
- `rectform`, `polarform` and `carg` of an odd root of a negative real quantity give the real root with `domain : real`, and `csign` of `b^(2/n)` for a declared odd `n` is `pos` for `b < 0`. `domain : complex` is unchanged everywhere.
- With `domain : complex` `abs(x)^(2/3)` stays as it is; it used to become `x^(2/3)`, which is on the principal branch there and differs for a negative `x` (`BUG-abs-even-numerator-domain-complex.md`). `abs(x)^2` is still `x^2` in both domains.
- Two simplifier improvements, valid for any `x` that `csign` does not report complex: `abs(x)/x` is `x/abs(x)`, and with `domain : real` a rational power of `x` with an even numerator combines with a power of `abs(x)`, so `(x^2*abs(x))^(1/3)`, `abs(x^3)^(1/3)` and `x^(2/3)*abs(x)^(1/3)` are `abs(x)` and `abs(-x^3)^(-1/3)` is `1/abs(x)`. Both were checked on `master` with randomized products of powers of a base and of its `abs`, evaluated numerically at real points, with the base declared complex at complex points, and under `domain : complex`; no result changed value. One weakness they share with every `abs` rule of `timesin` and `simpexpt`: `csign` reports `pnz` for `gamma(z)`, `tan(z)`, `erfc(z)`, `zeta(z)`, a `bessel_j`, an undefined `f(z)` and others when `z` is declared complex, so those rules fire on such an argument, as `abs(gamma(z))^3` becoming `gamma(z)^2*abs(gamma(z))` already shows on `master`; `BUG-csign-function-of-complex-argument.md` describes it.
- The derivative of `gamma_incomplete(a, z)` with respect to `a` no longer picks up values of the variables `a` and `z` (section 3.4). A `defgrad` lambda in code outside the tree keeps its meaning; returning `t` as a second value is the new option.
- Things noticed but left alone are in section 7, with bug reports for the three that are bugs.

## 7. Workarounds, and what a fix would have simplified

Things the change works around rather than fixes, roughly in the order of how much a fix would have shortened the work. The first three are bugs; reports ready for the SourceForge tracker are in `BUG-rectform-atan2.md`, `BUG-carg-product-argument.md` and `BUG-gamma_incomplete_lower-float.md` next to this file, and item 11 is a fourth, fixed here. Items 4 and 7 are fixed on this branch too: item 4 turned out to be a bug as well, item 7 was a missed simplification; `ENHANCEMENT-sdiffgrad-lambda-derivatives.md` and `ENHANCEMENT-abs-sign-normal-form.md` put them as requests, with reproducers.

1. **`rectform(atan2(y, 0))` is wrong for `y < 0` and carries a spurious imaginary part.** `rectform(atan2(y,0))` is `(2*%pi*ceiling((2*atan2(0,y)-%pi)/(2*%pi))+%pi)/2-%i*log(abs(y)/sqrt(y^2))`, which is `3*%pi/2` at `y = -2` where `atan2(-2, 0)` is `-%pi/2`; the imaginary part is zero but is not simplified, since `sqrt(y^2)` was produced with `$domain` bound to `complex` inside `risplit`. This is what broke the `atan2` phase for the imaginary constant behind `sin(x^3)` and forced two redesigns. Even fixed, an `atan2` in an exponent would not cancel against the derivative under `expand`, so the rational form is the better end point; the detour would have been shorter.

2. **`absarg` does not reduce the argument of a product.** `carg(-x)` is `atan2(0,x)+%pi`, which is `2*%pi` at `x = -8`; `rectform(sqrt(-x))` gives `-2^(3/2)` at `x = -8`; with `domain : complex`, `rectform((-x)^(1/3))` gives `sqrt(3)*%i-1` there, where the value is 2. A power gets the `ceiling` reduction, a product does not. So `carg(k)` could not be used for the phase of a symbolic constant `k`; `principal-phase` takes `atan2` of the real and imaginary parts of `k` and `-k` instead. With `absarg` fixed, `polarform` could have written the principal power directly.

3. **`gamma_incomplete_lower(a, z)` with a rational `a` and a float `z` is not evaluated**, `gamma_incomplete_lower(1/3, 8.0)` is `gamma(1/3)-7.799182611869946e-5`, and it has no conjugate property, so `rectform` of it gives `realpart` and `imagpart` noun forms where `gamma_incomplete` gets the mirror symmetry. The natural continuous antiderivative of `%e^(x^3)` is `gamma_incomplete_lower(1/3, -x^3)` times the phase; the change writes `gamma_incomplete(1/3, -x^3) - gamma(1/3)` instead.

4. **`sdiffgrad` re-substituted the result of a lambda derivative, fixed at the base of this branch (section 3.4).** A `defgrad` lambda gets the actual arguments, but its result was run through `psubstitute` with the placeholder symbols `a` and `z`, so a derivative built from the actual `z` was corrupted whenever the user's variable was named `z` or `a`; the one lambda in the tree, the derivative of `gamma_incomplete` with respect to its order, evaluated a template in the placeholders instead, and picked up values of the Maxima variables `a` and `z` on the way. Until the fix, `gamma-incomplete-z-derivative` returned an unsimplified template in the placeholder, with the modulus written as `abs(k)^s*(z^2/k^2)^(s/2)`, `k` checked with `freeof` against the placeholders and a symbolic order refused. Now a lambda returning `t` as a second value has its result taken as it is, as it already was for the special case of `hypergeometric` in `sdiffgrad`, a lambda returning one value is treated as before, the derivative is one call of `principal-power`, and the template and its guards are gone. `ENHANCEMENT-sdiffgrad-lambda-derivatives.md` has the reproducers.

5. **`risplit` cannot run with `$domain` left as `real`.** Removing its binding of `$domain` to `$complex` exhausts the control stack in rtestint problem 300. Hence `*risplit-domain*`, which the helpers read to learn the user's domain.

6. **The integrator declares its substitution variable for an even root complex** (`make-new-var` in sin.lisp, because `csign` of the root is `complex`), although it is a nonnegative real wherever the integrand is real. Hence `nonneg-internal-p`. Without it `exp(sqrt(x^3))` and `expintegral_ei(x^(-3/4))` would keep the wrong branch.

7. **Two normal forms for one sign, now fixed on this branch.** The simplifier gave `abs(x)/x` for `x^(-1)*abs(x)` but `x/abs(x)` for `x*abs(x)^(-1)`, which is why `principal-power-times` used to multiply the sign into the phase by the parity of `m` by hand, and why the derivative template had a separate case for a negative `n`; the recombination of `exp(-x^3)/x^4` in rtest_gamma 789 under `expand` depended on that care. `ENHANCEMENT-abs-sign-normal-form.md` asked for one canonical form; the two-comparison change in `timesin` (section 3.2) gives it, was verified against master in a separate session, and let both workarounds go. The second member of the family, `abs(-x^3)^(-1/3)` becoming `x^(-2/3)*abs(x)^(-1/3)`, is fixed on this branch too (hunks 2, 4 and 5 of section 3.2, verified the same way), but it simplified no code: the derivative template of the time wrote its modulus through `z^2` because of the even denominators, where `abs(x^(3/2))` does not reduce and `(z^2/k^2)^(s/2)` does. With the template gone (item 4) the derivative writes the modulus exactly as the integrator does, `abs(k)^s*abs(w)^(n*s)` with the constant outside the root, and the round trips with a constant coefficient in the exponent are exact (rtest_integrate, last block). What remains: `ratsimp` does not know `abs(x)^2 = x^2` inside the rational form, which is why the trigonometric round trips need `trigrat`, or a second `ratsimp`.

8. **`(x^3)^y` simplifies to `x^(3*y)` with `domain : real`** for a symbolic `y`, so the powers of `z` in the `gamma_expand` recurrence could not be recognized once expanded; they are expanded with a symbol for `z` and put back afterwards. Intended behaviour of the real domain, not a bug.

9. **`diff(signum(x), x)` has no rule**, so the first variant of the phase, with `signum`, could never differentiate back; `signum` is also rarely produced by Maxima. A rule would not have made it cancel under `expand` either.

10. **Numeric evaluation of an expression with `gamma_incomplete(1/3, %i)` in it**, as `float(rectform(integrate(sin(x^3), x, 0, 1)))`, leaves products of floats and sums unexpanded; the values were checked by substituting float points into the antiderivative and against `quad_qags` instead. A nuisance in verification, not a wrong result.

11. **`abs(x)^(2/3)` became `x^(2/3)` with `domain : complex`, fixed on this branch.** The `simpexpt` clause accepted an even-numerator fraction in both domains, although the identity holds only for the real root; hunk 1 of section 3.2 restricts the fraction to `domain : real`. It came to light while checking the root combination of item 7 under `domain : complex`: the combination was wrong there, and the other session had justified it by this rule. `BUG-abs-even-numerator-domain-complex.md` has the report; `BUG-csign-function-of-complex-argument.md` records a weakness of the `csign` guard that all the `abs` rules share and that is not fixed here.

## 8. Proposed commit messages

Two commits: the `sdiffgrad` fix first, on its own, then the rest.

```
SDIFFGRAD: let a derivative given as a function return a final result

A derivative in a GRAD property is either an expression in the
placeholder names of the DEFGRAD or a function of the arguments.
SDIFFGRAD substituted the arguments for the placeholder names into
both, so a function could not build its result from the arguments it
was given: an argument named like a placeholder would then have been
substituted a second time.  The only such function, the derivative of
gamma_incomplete with respect to its first argument, therefore
evaluated a template in the placeholder names a and z instead, and
picked up the values of the Maxima variables a and z on the way:

    a : 5$
    assume(b > 0)$
    diff(gamma_incomplete(b, y), b);
    /* (gamma_incomplete(5,y)-24)*log(y)+576*hypergeometric_regularized(
        [5,5],[6,6],-y)*y^5+24*(25/12-%gamma) */

A function may now return T as a second value, and SDIFFGRAD then
takes its result as it is; a function returning one value is
substituted into as before.  The derivative of gamma_incomplete builds
its result from its arguments with ADD, MUL and friends and returns it
that way.  Tests appended to rtest_gamma.mac.
```

```
Take the principal branch of odd roots consistently with domain : real

With domain : real the simplifier takes the real root of a negative
quantity, (-8)^(1/3) is -2, but rectform, csign, the integrator and the
derivative of gamma_incomplete each committed to a branch of their own.

rectform, polarform and carg now follow the simplifier for such a power
of a real quantity, and csign agrees with them, a denominator declared
odd included.  Where gamma_incomplete is involved the principal branch
is the only right one, so the power that comes with it, in integrate,
in diff and in the recurrences of gamma_expand, is written as an
expression in x/abs(x) that the simplifier leaves alone.  So
integrate(%e^(x^3), x), integrate(sin(x^3), x), integrate(x*%e^(x^3), x)
and definite integrals such as integrate(%e^(x^3), x, -1, 1) are right,
and expand brings the derivative of each antiderivative back to its
integrand.

The sign of a real x gets the one form x/abs(x) in the simplifier,
which the phases rely on, and with domain : real a rational power of x
with an even numerator combines with a power of abs(x), so that
(x^2*abs(x))^(1/3) and abs(-x^3)^(-1/3) are abs(x) and 1/abs(x).
With domain : complex abs(x)^(2/3) no longer becomes x^(2/3), which is
on the principal branch there.

Tests in rtest16, rtest_sign, rtest_gamma, rtest_integrate and
rtest_abs; the answers of rtestint 100, rtest_integrate_special 27 and
31 and rtest_abs_integrate 219 change.
```
