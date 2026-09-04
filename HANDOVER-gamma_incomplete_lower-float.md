# Handover: gamma_incomplete_lower with an exact order and a float argument

A stand-alone fix, the first commit of branch `claude/maxima-rectform-root-branch-pp1qu1` on the GitHub mirror, on top of upstream `d24eaa0` (*Combine powers of x and abs(x) when possible*). Nothing else on the branch depends on it. The mirror is overwritten on every sync, so this document carries the change as find-and-replace patches against upstream, the tests, and the commit message, ready for SourceForge; `BUG-gamma_incomplete_lower-float.md` is the report.

Built and verified on SBCL. The full core plus share suite, `run_testsuite(share_tests=true)`, passes with it: 21,059 tests, the only failure the pre-existing environmental one in `share/stringproc/rtestprintf.mac` problem 38.

## 1. The problem

A float or bigfloat argument gives a number everywhere in the incomplete gamma family, except where an exact `gamma(a)` is put in on the way:

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
(%i6) gamma_incomplete(1/3, 8.0);
(%o6) 7.799182611869946e-5
```

`gamma_incomplete_lower` evaluates only for a float order, or a positive integer one under `gamma_expand`. It also has no `conjugate-function` property, so `rectform(gamma_incomplete_lower(1/3, %i*x^3))` returns `realpart` and `imagpart` noun forms where `gamma_incomplete` gets the mirror symmetry of the function.

## 2. The cause, and the design

The numerical clause of the `gamma_incomplete_lower` simplifier delegates to `gamma_incomplete_generalized(a, 0, z)`. That simplifier, like the one of `gamma_incomplete`, tests for a zero argument before its numerical clauses, and the zero clauses build `gamma(a)` from the exact `a`: `gamma_incomplete(a, z2)` becomes a number, `gamma(a)` does not, and the sum stays symbolic.

The clauses cannot simply be reordered. The numerical routine `gamma-incomplete` divides by zero for an order at or below zero at `z = 0.0`, where the zero clause raises the domain error `gamma_incomplete(-1, 0.0) is undefined`.

So the zero clauses keep their place and take `gamma` of the order converted to the precision of the arguments, by `order-for-numerical-eval`: a float where the float clauses would fire, a bigfloat where the bigfloat clauses would, the complex kinds included, and the order itself otherwise, so a symbolic order and exact arguments are untouched. In `gamma_incomplete_generalized` the converted order goes into both terms of the difference, so that `gamma_incomplete_lower(1/3, 0.0)` is `0.0` and not `gamma(1/3)` less a float. The `conjugate-function` of `gamma_incomplete_lower` is that of `gamma_incomplete` with the name changed: mirror symmetry off the negative real axis of `z`, the branch cut the two functions share.

## 3. The patches

Apply them to upstream `master`. Each block quotes the upstream text with three lines of context and gives the line where it starts; the replacement keeps the context. The blocks preserve the tabs of the surrounding code.

### 3.1 `src/gamma.lisp`

Five hunks.

**Hunk 1.** The `conjugate-function` of `%gamma_incomplete_lower`, inserted after its `distribute_over` property.

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

**Hunk 2.** `order-for-numerical-eval`, inserted before `(def-simplifier gamma_incomplete (a z)`.

**Find this** (upstream line 622):

```lisp

;;; Incomplete Gamma function is a simplifying function

(def-simplifier gamma_incomplete (a z)
  (let (($simpsum t)
        (ratorder))
```

**Replace it with this:**

```lisp

;;; Incomplete Gamma function is a simplifying function

;; The order A as a float or a bigfloat where the numerical clauses of the
;; simplifiers below would evaluate the function of A and ZS, and A itself
;; otherwise: for the value at an exact point, gamma(a) for
;; gamma_incomplete(a, 0), which is to be a number where the arguments are.
(defun order-for-numerical-eval (a &rest zs)
  (cond ((or (apply #'float-numerical-eval-p a zs)
             (apply #'complex-float-numerical-eval-p a zs))
         ($float a))
        ((or (apply #'bigfloat-numerical-eval-p a zs)
             (apply #'complex-bigfloat-numerical-eval-p a zs))
         ($bfloat a))
        (t a)))

(def-simplifier gamma_incomplete (a z)
  (let (($simpsum t)
        (ratorder))
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
                ($gamma (order-for-numerical-eval a z)))
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
            (let ((a (order-for-numerical-eval a z1 z2)))
              (sub
                (simplify (list '(%gamma_incomplete) a z1))
                (simplify (list '(%gamma) a)))))
           (t 
            (give-up)))))
```

**Hunk 5.** The `(zerop1 z1)` clause of the same simplifier.

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
```

**Replace it with this:**

```lisp
       (let ((sgn ($sign ($realpart a))))
         (cond 
           ((member sgn '($pos $pz))
            (let ((a (order-for-numerical-eval a z1 z2)))
              (sub
                (simplify (list '(%gamma) a))
                (simplify (list '(%gamma_incomplete) a z2)))))
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
* gamma_incomplete_lower(a, z) with an exact order a and a float z, as gamma_incomplete(a, 0.0), keeps gamma(a) exact instead of giving a number; gamma_incomplete_lower gets the mirror symmetry of gamma_incomplete for conjugate

Changes in the Windows installer:
---------------------------------
```

## 4. Tests

### 4.1 `tests/rtest_gamma.mac`

One block appended at the end, after the `erf_generalized` limits: the values of section 1 through the file's own `closeto`, the bigfloat and complex cases, `gamma_incomplete_lower(1/3, 0.0)`, the cases that stay exact or symbolic, the two domain errors, and the conjugate rule with `assume`, `realpart` and a declared-nothing `z`. The file is registered with known-failure numbers under Allegro only, all far below.

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
realpart(gamma_incomplete_lower(1/3, %i*x));
   (gamma_incomplete_lower(1/3,%i*x)+gamma_incomplete_lower(1/3,-(%i*x)))/2
```

## 6. What changes for users, and what does not

- `gamma_incomplete_lower(a, z)`, `gamma_incomplete(a, 0.0)` and `gamma_incomplete_generalized` with a zero limit give a number, float, bigfloat or complex, whenever `gamma_incomplete` with the same arguments would. With `numer : true` the same happens for exact arguments, as the numerical clauses already behave.
- Exact arguments, a symbolic order, the domain errors at zero for an order that is not positive, and `gamma_incomplete_lower` for an order at or below zero, which is left as `gamma_incomplete_generalized`, are unchanged.
- `conjugate`, `rectform`, `realpart` and `imagpart` of `gamma_incomplete_lower` follow the mirror symmetry off the negative real axis, as for `gamma_incomplete`; on the axis or for an unknown `z` the `conjugate` stays a noun form.
- Not touched: `gamma_incomplete_regularized(a, 0.0)` still returns an exact 1, and `gamma_incomplete_generalized(a, z, z)` an exact 0, for float arguments; the same flaw in a milder form, left for another day.

## 7. Proposed commit message

```
gamma_incomplete_lower(a, z): a number for an exact a and a float z

The simplifiers of gamma_incomplete and gamma_incomplete_generalized
test for a zero argument before their numerical clauses, and the zero
clauses put in gamma(a) of the exact order, so that with a float
argument one term of the result was a number and the other was not:

    gamma_incomplete_lower(1/3, 8.0);    /* gamma(1/3)-7.799182611869946e-5 */
    gamma_incomplete(1/3, 0.0);          /* gamma(1/3) */

gamma_incomplete_lower delegates to gamma_incomplete_generalized(a, 0, z)
and so had the flaw for every exact order but a positive integer.  The
zero clauses now take gamma of the order converted to the precision of
the arguments by ORDER-FOR-NUMERICAL-EVAL, float, bigfloat or their
complex kinds, and use the same order in both terms; a symbolic order
and exact arguments are as they were.  Reordering the clauses instead
would have turned the domain error of gamma_incomplete(-1, 0.0) into a
division by zero in the numerical routine.

gamma_incomplete_lower also gets the CONJUGATE-FUNCTION of
gamma_incomplete, mirror symmetry off the negative real axis of z, so
that rectform and realpart treat the two functions alike.

Tests appended to rtest_gamma.mac.
```
