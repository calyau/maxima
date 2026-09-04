# gamma_incomplete_lower(a, z) with a rational order a and a float z is not evaluated

**Version:** Maxima git master (5.50post), SBCL.

For an exact order that is not a positive integer, `gamma_incomplete_lower` with a float or bigfloat argument returns `gamma(a) - gamma_incomplete(a, z)` with `gamma(a)` left exact, instead of a number: `gamma_incomplete_lower(3/2, 8.0)` is `sqrt(%pi)/2-0.001004967410648176`. The upper function evaluates, and so does `gamma_incomplete_lower` for a float order or, with `gamma_expand`, a positive integer one.

```
(%i1) display2d : false$
(%i2) gamma_incomplete_lower(1/3, 8.0);
(%o2) gamma(1/3)-7.799182611869946e-5
(%i3) gamma_incomplete_lower(1/3, -8.0);
(%o3) gamma(1/3)+719.2849028282662*%i+412.60039373722566
(%i4) gamma_incomplete_lower(1/3, 8.0*%i);
(%o4) gamma(1/3)+0.10901538887260959*%i+0.221572520567445
(%i5) gamma_incomplete_lower(1/3, 8.0b0*%i);
(%o5) gamma(1/3)+1.090153888726096b-1*%i+2.215725205674452b-1
(%i6) gamma_incomplete_lower(0.5, 8.0*%i);
(%o6) 0.22561853882998203*%i+2.040645378736624
(%i7) gamma_incomplete(1/3, 8.0*%i);
(%o7) -(0.10901538887260959*%i)-0.221572520567445
(%i8) float(gamma_incomplete_lower(1/3, 8*%i));
(%o8) 0.10901538887260959*%i+2.900511055275193
```

**Expected:** a float in each of (%o2) to (%o5), as in (%o8): `2.6788605428816297` for `gamma_incomplete_lower(1/3, 8.0)`.

**Where:** in `src/gamma.lisp`. The numerical clause of the `gamma_incomplete_lower` simplifier delegates to `gamma_incomplete_generalized(a, 0, z)`, and the simplifier of that function tests for a zero argument before its numerical clauses: the `(zerop1 z1)` clause returns `gamma(a) - gamma_incomplete(a, z2)` with `gamma(a)` built from the exact `a`, so `gamma_incomplete(a, z2)` becomes a number and `gamma(a)` does not. The `(zerop1 z2)` clause has the same flaw, and so has the `(zerop1 z)` clause of `gamma_incomplete` itself, which is why `gamma_incomplete(1/3, 0.0)` is `gamma(1/3)` rather than `2.6789385347077483`.

**Fix:** not a reordering of the clauses, since the numerical routine `gamma-incomplete` divides by zero for `a <= 0` at `z = 0.0`, where the zero clause raises the domain error. Instead the three zero clauses take `gamma` of the order converted to the precision of the arguments, by a small helper:

```lisp
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
```

with `($gamma (order-for-numerical-eval a z))` in the zero clause of `gamma_incomplete`, and in the two zero clauses of `gamma_incomplete_generalized` the order converted once, `(let ((a (order-for-numerical-eval a z1 z2))) ...)`, and used in both terms, so that `gamma_incomplete_lower(1/3, 0.0)` is `0.0` and not `gamma(1/3) - 2.6789385347077483`. Verified at runtime against the current tree: every case above becomes a number, `gamma_incomplete_lower(3/2, 8.0)` is `0.8852219580421099`, the domain errors for `gamma_incomplete(-1, 0.0)` and `gamma_incomplete(0, 0.0)` stay, a symbolic order is untouched, and the full core plus share suite passes with it loaded, so no test pins the old forms.

A related omission: `gamma_incomplete_lower` has no `conjugate-function` property, so `rectform(gamma_incomplete_lower(1/3, %i*x^3))` returns `realpart` and `imagpart` noun forms, where `rectform(gamma_incomplete(1/3, %i*x^3))` uses the mirror symmetry of the function. A `conjugate-gamma-incomplete-lower` written like `conjugate-gamma-incomplete`, mirror symmetry off the negative real axis of `z`, gives it the same treatment; verified with the fix above.
