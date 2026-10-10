# Handover: is() and abs() of expressions with %i

## 1. Summary

**Stop is() from taking %i inside abs() as a sign of a non-real expression.**
CSIGN, the sign helper behind `is(a > b)` and `is(a >= b)`, treated every expression containing `%i` as complex, so comparisons involving `abs(z)` with `%i` in `z` returned `false`, even when true or undecidable. Now `%i` inside `abs()`, which is always real, no longer counts, and `$SIGN` decides as for any other real expression.

## 2. Reproducer/Demo

**Old behavior** (pristine `HEAD` build):

```
(%i1) is(abs(b - %i*a) >= 0);
(%o1)                                false
(%i2) is(abs(b - %i*a) > -1);
(%o2)                                false
(%i3) sign(abs(b - %i*a));
(%o3)                                 pz
(%i4) is(abs(f(x) - %i*g(x)) > 1/1000);
(%o4)                                false
(%i5) check(ans, key) := if is(abs(ans - key) > 1/1000) then wrong else right$

(%i6) check(f(x), %i*g(x));
(%o6)                                right
```

**New behavior** (patched build):

```
(%i1) is(abs(b - %i*a) >= 0);
(%o1)                                true
(%i2) is(abs(b - %i*a) > -1);
(%o2)                                true
(%i3) sign(abs(b - %i*a));
(%o3)                                 pz
(%i4) is(abs(f(x) - %i*g(x)) > 1/1000);
(%o4)                               unknown
(%i5) check(ans, key) := if is(abs(ans - key) > 1/1000) then wrong else right$

(%i6) check(f(x), %i*g(x));
(%o6)                  if unknown then wrong else right
```

**Internals.** MGRP-GENERAL and MGQP-GENERAL return `false` when CSIGN returns T ("appears to be complex"), and CSIGN returned T as soon as FREE found `%i` anywhere. The new helper %I-OUTSIDE-ABS-P makes the same test but skips the arguments of `mabs`. CRE and Poisson forms still go through FREE, so they behave as before. When `%i` occurs only inside `abs()`, real-mode `$SIGN` now runs. It rectforms the expression (`abs(b - %i*a)` becomes `sqrt(b^2 + a^2)`) and still throws to SIGN-IMAG-ERR, which CSIGN turns into T, if a non-real part is left.

Dropping the `%i` shortcut altogether was tried first. It changed 43 CSIGN results in the test suite, some wrongly (`%i - inf` got the sign `neg`, and CSIGN calls nested in `$CSIGN` returned `$COMPLEX`). With this patch, no CSIGN result in the test suite changes. Covering all functions with the REAL-VALUED property (which would also handle the `'realpart`/`'imagpart` nouns that `cabs(f(%i))` produces) was rejected too: `is(hstep(%i*x) > 0)` would turn from `false` into an error, because the sign function of `hstep` refuses non-real arguments.

## 3. Bug report

**Title:**

```
is(abs(z) >= 0) returns false when z contains %i
```

**Body:**

````markdown
`is` returns `false` for comparisons involving `abs` of an expression that contains `%i`, although `abs` is always real and `sign` knows the result:

```
(%i1) is(abs(b - %i*a) >= 0);
(%o1)                                false
(%i2) is(abs(b - %i*a) > -1);
(%o2)                                false
(%i3) sign(abs(b - %i*a));
(%o3)                                 pz
```

`(%o1)` and `(%o2)` should be `true`. Where the answer is not known, `is` should return `unknown`, but it returns `false` as well:

```
(%i4) is(abs(f(x) - %i*g(x)) > 1/1000);
(%o4)                                false
```

So a check like the following accepts a wrong answer when abstract functions are involved:

```
(%i5) check(ans, key) := if is(abs(ans - key) > 1/1000) then wrong else right$

(%i6) check(f(x), %i*g(x));
(%o6)                                right
```
````

## 4. Code patching instructions

### `src/compar.lisp`: CSIGN

Adds %I-OUTSIDE-ABS-P and uses it in CSIGN instead of FREE.

**Replace** this block:

```lisp
;; csign returns t if x appears to be complex.
;; Else, it returns the sign.
(defun csign (x)
  (or (not (free x '$%i))
      (let (sign-imag-errp limitp) (catch 'sign-imag-err ($sign x)))))
```

**with:**

```lisp
;; True if %i occurs in X outside of abs, whose value is always real.
(defun %i-outside-abs-p (x)
  (cond ((atom x) (eq x '$%i))
        ((specrepp x) (not (free x '$%i)))
        ((eq (caar x) 'mabs) nil)
        (t (some #'%i-outside-abs-p (cdr x)))))

;; csign returns t if x appears to be complex.
;; Else, it returns the sign.
(defun csign (x)
  (or (%i-outside-abs-p x)
      (let (sign-imag-errp limitp) (catch 'sign-imag-err ($sign x)))))
```

## 5. Proposed test cases

**New**, in `tests/rtest_sign.mac`. **Insert** before the final block, so that the `[facts(), contexts]` check stays last:

```
/**************************************/
/* Leave this at the end of the file! */
```

```
/* Bug #NNNN: "is(abs(z) >= 0) returns false when z contains %i" */

[is(abs(b - %i*a) >= 0), is(abs(b - %i*a) > -1), is(abs(b - %i*a) > 0),
  is(abs(f(x) - %i*g(x)) > 1/1000)];
[true, true, unknown, unknown]$
```

Ran in a scratch copy of `tests/rtest_sign.mac`: the new problem (789) gives `[false, false, false, false]` on a pristine build and passes on the patched one, and the whole file passes (785/785, not counting 5 expected errors). No registered problem number moves. `run_testsuite(share_tests = true)` on the patched build: 1 of 21,676 tests failed, `rtestprintf.mac` problem 38, which fails the same way on the pristine build (SBCL 2.2.9 float printing). `tests/depcheck.sh` is clean.

## 6. Proposed Git commit message

```
Fix is() for abs() of expressions with %i

is() treated every expression containing %i as non-real, even when %i
occurs only inside abs(), which is always real. So
is(abs(b - %i*a) >= 0) returned false, and undecidable comparisons such
as is(abs(f(x) - %i*g(x)) > 1/1000) returned false instead of unknown.
Code that checks answers with is(abs(answer - key) > tolerance) thus
accepted wrong answers involving abstract functions.

Now %i inside abs() is ignored when is() decides whether an expression
is real.

This fixes bug #NNNN.

AI-Assisted-By: Claude Opus 5.5
```
