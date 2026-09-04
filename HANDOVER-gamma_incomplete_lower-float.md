# Handover: the gamma, error and beta functions at their special points, for float arguments

A stand-alone change, the first commit of branch `claude/maxima-rectform-root-branch-pp1qu1` on the GitHub mirror, on top of upstream `d24eaa0` (*Combine powers of x and abs(x) when possible*). Nothing else on the branch depends on it. The mirror is overwritten on every sync, so this document carries the change as find-and-replace patches against upstream, the tests, and the commit message, ready for SourceForge; `BUG-gamma_incomplete_lower-float.md` is the report of the case it started from.

Built and verified on SBCL. The full core plus share suite, `run_testsuite(share_tests=true)`, passes with it: 21,063 tests, the only failure the pre-existing environmental one in `share/stringproc/rtestprintf.mac` problem 38.

## 1. The problem

The simplifiers of the incomplete gamma, error and incomplete beta functions in `src/gamma.lisp` test for special arguments before their numerical clauses and put in an exact value there, so a float argument did not always give a number:

```
(%i1) display2d : false$
(%i2) gamma_incomplete_lower(1/3, 8.0);
(%o2) gamma(1/3)-7.799182611869946e-5
(%i3) gamma_incomplete_lower(3/2, 8.0);
(%o3) sqrt(%pi)/2-0.001004967410648176
(%i4) gamma_incomplete_lower(1/3, 8.0*%i);
(%o4) gamma(1/3)+0.10901538887260959*%i+0.221572520567445
(%i5) gamma_incomplete(1/3, 0.0);
(%o5) gamma(1/3)
(%i6) [gamma_incomplete_regularized(1/3, 0.0), gamma_incomplete_generalized(1/3, 8.0, 8.0)];
(%o6) [1,0]
(%i7) [erfc(0.0), beta_incomplete_regularized(1/3, 1/2, 0.0)];
(%o7) [1,0]
(%i8) [gamma_incomplete(1/3, 8.0), erf(0.0), gamma_incomplete_regularized(1/3, 8.0)];
(%o8) [7.799182611869946e-5,0.0,2.911295840059569e-5]
```

`gamma_incomplete_lower` is the worst case, since it delegates to `gamma_incomplete_generalized(a, 0, z)`: it evaluates only for a float order, or a positive integer one under `gamma_expand`. It also has no `conjugate-function` property, so `rectform(gamma_incomplete_lower(1/3, %i*x^3))` returns `realpart` and `imagpart` noun forms where `gamma_incomplete` gets the mirror symmetry of the function.

## 2. The cause, and the design

The numerical clause of the `gamma_incomplete_lower` simplifier delegates to `gamma_incomplete_generalized(a, 0, z)`. That simplifier, like the one of `gamma_incomplete`, tests for a zero argument before its numerical clauses, and the zero clauses build `gamma(a)` from the exact `a`: `gamma_incomplete(a, z2)` becomes a number, `gamma(a)` does not, and the sum stays symbolic.

The clauses cannot simply be reordered. The numerical routine `gamma-incomplete` divides by zero for an order at or below zero at `z = 0.0`, where the zero clause raises the domain error `gamma_incomplete(-1, 0.0) is undefined`.

So the special-value clauses keep their place and return their value converted to the precision of the arguments, by `number-for-numerical-eval` at the head of the file: a float where the float clauses would fire, a bigfloat where the bigfloat clauses would, the complex kinds included, and the value itself otherwise, so a symbolic order and exact arguments are untouched. The zero clauses of `gamma_incomplete` and `gamma_incomplete_generalized` convert the order and take `gamma` of it, the latter using the converted order in both terms of the difference, so that `gamma_incomplete_lower(1/3, 0.0)` is `0.0` and not `gamma(1/3)` less a float; the clauses of `gamma_incomplete_regularized`, `erfc`, `erf_generalized` and `beta_incomplete_regularized` convert their constant 0 or 1. `erf_generalized(z, z)` is 0 by the function's `antisymmetric` declaration, a rule of the simplifier that fires before any clause, for a float `z` too; that is left alone. The `conjugate-function` of `gamma_incomplete_lower` is that of `gamma_incomplete` with the name changed: mirror symmetry off the negative real axis of `z`, the branch cut the two functions share.

## 3. The patches

Apply them to upstream `master`. Each block quotes the upstream text with three lines of context and gives the line where it starts; the replacement keeps the context. The blocks preserve the tabs of the surrounding code.

### 3.1 `src/gamma.lisp`

Nine hunks.

**Hunk 1.** `number-for-numerical-eval`, inserted after the option variables at the head of the file.

**Find this** (upstream line 94):

```lisp
	(mheader '$!!)
	(convert left '$expr)))

;; Pretty-printer display double_factorial(n) as n!! .
;; Apply display properties to both noun and verb forms; that matches current behavior of ordinary factorial.
```

**Replace it with this:**

```lisp
	(mheader '$!!)
	(convert left '$expr)))

;;; The value of a function at a special point is a number where the
;;; arguments are: X as a float or a bigfloat where the numerical clauses of
;;; the simplifiers below would evaluate a function of ARGS, and X itself
;;; otherwise.  So gamma(a) for gamma_incomplete(a, 0), 1 for erfc(0) and the
;;; like come out as floats, bigfloats or their complex kinds for such
;;; arguments, and exact for exact or symbolic ones.

(defun number-for-numerical-eval (x &rest args)
  (cond ((or (apply #'float-numerical-eval-p x args)
             (apply #'complex-float-numerical-eval-p x args))
         ($float x))
        ((or (apply #'bigfloat-numerical-eval-p x args)
             (apply #'complex-bigfloat-numerical-eval-p x args))
         ($bfloat x))
        (t x)))

;; Pretty-printer display double_factorial(n) as n!! .
;; Apply display properties to both noun and verb forms; that matches current behavior of ordinary factorial.
```

**Hunk 2.** The `conjugate-function` of `%gamma_incomplete_lower`, inserted after its `distribute_over` property.

**Find this** (upstream line 476):

```lisp

(defprop %gamma_incomplete_lower (mlist $matrix mequal) distribute_over)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgrad %gamma_incomplete_lower ($a $z)
```

**Replace it with this:**

```lisp

(defprop %gamma_incomplete_lower (mlist $matrix mequal) distribute_over)

;;; Lower Incomplete Gamma function has not mirror symmetry for z on the
;;; negative real axis, as gamma_incomplete has not, being gamma(a) less it.
;;; We support a conjugate-function which test this case.

(defprop %gamma_incomplete_lower conjugate-gamma-incomplete-lower conjugate-function)

(defun conjugate-gamma-incomplete-lower (args)
  (let ((a (first args)) (z (second args)))
    (cond ((off-negative-real-axisp z)
           ;; Definitely not on the negative real axis for z. Mirror symmetry.
           (simplify
             (list
              '(%gamma_incomplete_lower)
               (simplify (list '($conjugate) a))
               (simplify (list '($conjugate) z)))))
          (t
           ;; On the negative real axis or no information. Unsimplified.
           (list
            '($conjugate simp)
             (simplify (list '(%gamma_incomplete_lower) a z)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgrad %gamma_incomplete_lower ($a $z)
```

**Hunk 3.** The zero clause of the `gamma_incomplete` simplifier.

**Find this** (upstream line 639):

```lisp
                  (intl:gettext 
                    "gamma_incomplete: gamma_incomplete(~:M,~:M) is undefined.")
                    a z))
               ((member sgn '($pos $pz)) ($gamma a))
               (t (give-up)))))
              
      ((eq z '$inf) 0)
```

**Replace it with this:**

```lisp
                  (intl:gettext 
                    "gamma_incomplete: gamma_incomplete(~:M,~:M) is undefined.")
                    a z))
               ((member sgn '($pos $pz))
                ($gamma (number-for-numerical-eval a z)))
               (t (give-up)))))
              
      ((eq z '$inf) 0)
```

**Hunk 4.** The `(zerop1 z2)` clause of the `gamma_incomplete_generalized` simplifier.

**Find this** (upstream line 1438):

```lisp
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((member sgn '($pos $pz))
            (sub
              (simplify (list '(%gamma_incomplete) a z1))
              (simplify (list '(%gamma) a))))
           (t 
            (give-up)))))
```

**Replace it with this:**

```lisp
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((member sgn '($pos $pz))
            (let ((a (number-for-numerical-eval a z1 z2)))
              (sub
                (simplify (list '(%gamma_incomplete) a z1))
                (simplify (list '(%gamma) a)))))
           (t 
            (give-up)))))
```

**Hunk 5.** The `(zerop1 z1)` clause and the equal-limits clause of the same simplifier.

**Find this** (upstream line 1448):

```lisp
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((member sgn '($pos $pz))
            (sub
              (simplify (list '(%gamma) a))
              (simplify (list '(%gamma_incomplete) a z2))))
           (t 
            (give-up)))))

      ((zerop1 (sub z1 z2)) 0)

      ((eq z2 '$inf) (simplify (list '(%gamma_incomplete) a z1)))
      ((eq z1 '$inf) (mul -1 (simplify (list '(%gamma_incomplete) a z2))))
```

**Replace it with this:**

```lisp
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((member sgn '($pos $pz))
            (let ((a (number-for-numerical-eval a z1 z2)))
              (sub
                (simplify (list '(%gamma) a))
                (simplify (list '(%gamma_incomplete) a z2)))))
           (t 
            (give-up)))))

      ((zerop1 (sub z1 z2)) (number-for-numerical-eval 0 a z1 z2))

      ((eq z2 '$inf) (simplify (list '(%gamma_incomplete) a z1)))
      ((eq z1 '$inf) (mul -1 (simplify (list '(%gamma_incomplete) a z2))))
```

**Hunk 6.** The zero clauses of the `gamma_incomplete_regularized` simplifier.

**Find this** (upstream line 1619):

```lisp
                  (intl:gettext 
                    "gamma_incomplete_regularized: gamma_incomplete_regularized(~:M,~:M) is undefined.")
                    a z))
               ((member sgn '($pos $pz)) 1)
               (t (give-up)))))  

      ((zerop1 a) 0)
      ((eq z '$inf) 0)

      ;; Check for numerical evaluation in Float or Bigfloat precision
```

**Replace it with this:**

```lisp
                  (intl:gettext 
                    "gamma_incomplete_regularized: gamma_incomplete_regularized(~:M,~:M) is undefined.")
                    a z))
               ((member sgn '($pos $pz)) (number-for-numerical-eval 1 a z))
               (t (give-up)))))  

      ((zerop1 a) (number-for-numerical-eval 0 a z))
      ((eq z '$inf) 0)

      ;; Check for numerical evaluation in Float or Bigfloat precision
```

**Hunk 7.** The zero clause of `erfc`.

**Find this** (upstream line 2372):

```lisp
      
    ;; Check for specific values
      
    ((and (zerop1 z1) (zerop1 z2)) 0)
    ((zerop1 z1) (take '(%erf) z2))
    ((zerop1 z2) (mul -1 (take '(%erf) z1)))
    ((or (eq z2 '$inf)
```

**Replace it with this:**

```lisp
      
    ;; Check for specific values
      
    ((and (zerop1 z1) (zerop1 z2)) (number-for-numerical-eval 0 z1 z2))
    ((zerop1 z1) (take '(%erf) z2))
    ((zerop1 z2) (mul -1 (take '(%erf) z1)))
    ((or (eq z2 '$inf)
```

**Hunk 8.** The double-zero clause of `erf_generalized`.

**Find this** (upstream line 2499):

```lisp

    ;; Check for specific values

    ((zerop1 z) 1)
    ((eq z '$inf) 0)
    ((eq z '$minf) 2)
```

**Replace it with this:**

```lisp

    ;; Check for specific values

    ((zerop1 z) (number-for-numerical-eval 1 z))
    ((eq z '$inf) 0)
    ((eq z '$minf) 2)
```

**Hunk 9.** The zero clause of `beta_incomplete_regularized`.

**Find this** (upstream line 3936):

```lisp
                    "beta_incomplete_regularized: beta_incomplete_regularized(~:M,~:M,~:M) is undefined.") 
                    a b z))
               ((member sgn '($pos $pz)) 
                0)
               (t 
                (give-up)))))
```

**Replace it with this:**

```lisp
                    "beta_incomplete_regularized: beta_incomplete_regularized(~:M,~:M,~:M) is undefined.") 
                    a b z))
               ((member sgn '($pos $pz)) 
                (number-for-numerical-eval 0 a b z))
               (t 
                (give-up)))))
```

### 3.2 `ChangeLog`

One line under *Bug fixes for unnumbered bugs*.

**Hunk 1.**

**Find this** (upstream line 77):

```text
------------------------------
* tlimit never asks the sign questions that limit asks
* limits of atanh at an infinity ignore the imaginary part of the argument

Changes in the Windows installer:
---------------------------------
```

**Replace it with this:**

```text
------------------------------
* tlimit never asks the sign questions that limit asks
* limits of atanh at an infinity ignore the imaginary part of the argument
* the special values of the incomplete gamma, error and incomplete beta functions are exact where the arguments are floats, as gamma_incomplete_lower(1/3, 8.0), gamma_incomplete(1/3, 0.0) and erfc(0.0); gamma_incomplete_lower gets the mirror symmetry of gamma_incomplete for conjugate

Changes in the Windows installer:
---------------------------------
```

## 4. Tests

### 4.1 `tests/rtest_gamma.mac`

One block appended at the end, after the `erf_generalized` limits: the `gamma_incomplete_lower` values through the file's own `closeto`, the bigfloat and complex cases, `gamma_incomplete_lower(1/3, 0.0)`, the cases that stay exact or symbolic, the two domain errors, the conjugate rule with `assume`, `realpart` and an unknown `z`, and then the other special points of the family, each next to its exact counterpart. The file is registered with known-failure numbers under Allegro only, all far below.

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

/* With an exact order and a float or bigfloat argument, gamma_incomplete
   at zero, gamma_incomplete_generalized with a zero limit and so
   gamma_incomplete_lower are numbers, as gamma_incomplete is elsewhere:
   the value gamma(a) is taken at the precision of the arguments.  Exact
   arguments stay exact, and so does a symbolic order. */
(kill(a, x, z), 0);
0$

closeto(gamma_incomplete_lower(1/3, 8.0), 2.6788605428816297, 1e-14);
true$

closeto(gamma_incomplete_lower(3/2, 8.0), 0.8852219580421099, 1e-14);
true$

closeto(gamma_incomplete_lower(1/3, 8.0*%i), 0.10901538887260959*%i + 2.900511055275193, 1e-14);
true$

closeto(gamma_incomplete_lower(1/3, 8.0b0), 2.678860542881629b0, 1b-14);
true$

closeto(gamma_incomplete(1/3, 0.0), 2.6789385347077483, 1e-14);
true$

closeto(gamma_incomplete_generalized(1/3, 0, 8.0), 2.6788605428816297, 1e-14);
true$

closeto(gamma_incomplete_generalized(1/3, 8.0, 0), -2.6788605428816297, 1e-14);
true$

[gamma_incomplete_lower(1/3, 0.0), gamma_incomplete_generalized(1/3, 0.0, 0)];
[0.0, 0.0]$

[gamma_incomplete(1/3, 0), gamma_incomplete_lower(1/3, 8), gamma_incomplete_lower(a, 8.0)];
[gamma(1/3), gamma_incomplete_lower(1/3, 8), gamma_incomplete_lower(a, 8.0)]$

/* undefined at zero for an order that is not positive, float or not */
[errcatch(gamma_incomplete(-1, 0.0)), errcatch(gamma_incomplete(0, 0.0))];
[[], []]$

/* mirror symmetry off the negative real axis, as for gamma_incomplete */
[conjugate(gamma_incomplete_lower(1/3, %i*x)), conjugate(gamma_incomplete_lower(a, z))];
[gamma_incomplete_lower(1/3, -%i*x), conjugate(gamma_incomplete_lower(a, z))]$

(assume(x > 0), conjugate(gamma_incomplete_lower(1/3, x)));
gamma_incomplete_lower(1/3, x)$

(forget(x > 0), realpart(gamma_incomplete_lower(1/3, %i*x)));
(gamma_incomplete_lower(1/3, %i*x) + gamma_incomplete_lower(1/3, -%i*x))/2$

/* the same at the other special points of the family */
[gamma_incomplete_regularized(1/3, 0.0), gamma_incomplete_regularized(1/3, 0.0b0),
 gamma_incomplete_regularized(0, 8.0), gamma_incomplete_regularized(1/3, 0)];
[1.0, 1.0b0, 0.0, 1]$

[gamma_incomplete_generalized(1/3, 8.0, 8.0), gamma_incomplete_generalized(1/3, 8.0b0, 8),
 gamma_incomplete_generalized(1/3, x, x)];
[0.0, 0.0b0, 0]$

[erfc(0.0), erfc(0.0b0), erfc(0), erf_generalized(0.0, 0)];
[1.0, 1.0b0, 1, 0.0]$

[beta_incomplete_regularized(1/3, 1/2, 0.0), beta_incomplete_regularized(1/3, 1/2, 0)];
[0.0, 0]$
```

## 5. Verifying

```sh
make
test src/gamma.lisp -nt src/binary-sbcl/maxima.core && echo 'EDIT NOT IN IMAGE'
./maxima-local --no-init -q --batch-string='run_testsuite(tests=["rtest_gamma"]);'
./maxima-local --no-init -q --batch-string='run_testsuite(share_tests=true);'
```

By hand:

```
gamma_incomplete_lower(1/3, 8.0);          2.6788605428816297
gamma_incomplete_lower(1/3, 8.0b0);        2.678860542881629b0
gamma_incomplete_lower(1/3, 0.0);          0.0
gamma_incomplete(1/3, 0.0);                2.6789385347077483
gamma_incomplete(-1, 0.0);                 gamma_incomplete: gamma_incomplete(-1,0.0) is undefined.
gamma_incomplete_lower(1/3, 8);            gamma_incomplete_lower(1/3,8)
[gamma_incomplete_regularized(1/3, 0.0), erfc(0.0), beta_incomplete_regularized(1/3, 1/2, 0.0)];
                                           [1.0,1.0,0.0]
[gamma_incomplete_regularized(1/3, 0), erfc(0), erfc(x)];
                                           [1,1,erfc(x)]
realpart(gamma_incomplete_lower(1/3, %i*x));
   (gamma_incomplete_lower(1/3,%i*x)+gamma_incomplete_lower(1/3,-(%i*x)))/2
```

## 6. What changes for users, and what does not

- `gamma_incomplete_lower(a, z)`, `gamma_incomplete(a, 0.0)`, `gamma_incomplete_generalized` with a zero or with equal limits, `gamma_incomplete_regularized(a, 0.0)` and `gamma_incomplete_regularized(0, z)`, `erfc(0.0)`, `erf_generalized(0.0, 0)` and `beta_incomplete_regularized(a, b, 0.0)` give a number, float, bigfloat or complex, whenever the numerical clauses of the function would for those arguments. With `numer : true` the same happens for exact arguments, as the numerical clauses already behave.
- Exact arguments, a symbolic order, the domain errors at zero for an order that is not positive, and `gamma_incomplete_lower` for an order at or below zero, which is left as `gamma_incomplete_generalized`, are unchanged.
- `conjugate`, `rectform`, `realpart` and `imagpart` of `gamma_incomplete_lower` follow the mirror symmetry off the negative real axis, as for `gamma_incomplete`; on the axis or for an unknown `z` the `conjugate` stays a noun form.
- Not touched: `erf_generalized(z, z)` is an exact 0 for a float `z` too, by the `antisymmetric` declaration of the function, which the simplifier applies before the function's own clauses; and the values at `inf` and `minf`, which are exact for exact arguments and where no float is involved.

## 7. Proposed commit message

```
gamma.lisp: the value at a special point is a number where the arguments are

The simplifiers of the incomplete gamma, error and incomplete beta
functions test for special arguments before their numerical clauses
and put in an exact value there, so that a float argument did not
always give a number:

    gamma_incomplete_lower(1/3, 8.0);           /* gamma(1/3)-7.799182611869946e-5 */
    gamma_incomplete(1/3, 0.0);                 /* gamma(1/3) */
    gamma_incomplete_regularized(1/3, 0.0);     /* 1 */
    gamma_incomplete_generalized(1/3, 8.0, 8.0); /* 0 */
    erfc(0.0);                                  /* 1 */
    beta_incomplete_regularized(1/3, 1/2, 0.0); /* 0 */

gamma_incomplete_lower delegates to gamma_incomplete_generalized(a, 0, z)
and so had the flaw for every exact order but a positive integer.  The
special-value clauses now return their value converted by
NUMBER-FOR-NUMERICAL-EVAL to the precision the numerical clauses would
use, float, bigfloat or their complex kinds, and the exact value where
the arguments are exact or symbolic; the zero clauses of
gamma_incomplete_generalized use the converted order in both terms of
their difference.  Reordering the clauses instead would have turned
the domain error of gamma_incomplete(-1, 0.0) into a division by zero
in the numerical routine.  erf_generalized(z, z) is 0 by its
antisymmetric declaration, a rule of the simplifier, and stays so.

gamma_incomplete_lower also gets the CONJUGATE-FUNCTION of
gamma_incomplete, mirror symmetry off the negative real axis of z, so
that rectform and realpart treat the two functions alike.

Tests appended to rtest_gamma.mac.
```
