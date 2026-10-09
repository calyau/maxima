# Speed-ups found by profiling the test suites

## 1. Summary

Four patches from the sb-sprof profiling of `run_testsuite()` with SBCL
2.6.9: `sign()` of linear sums without `factor()`, a correctly rounded
bigfloat square root, no arity-error name built on every simplification,
and cheaper binding of local variables. Together they take the full test
suite from 127.6 to 111.9 s (−12.3%) and the core suite from 72.4 to
61.2 s (−15.4%). The square root changes eight test expectations and
exposes an existing `limit()` bug. Three more changes gave no measurable
gain and are listed at the end for reference.

| # | change | file | effect | tests |
|---|--------|------|--------|-------|
| 1 | `sign()` of linear sums without `factor()` | `src/compar.lisp` | −5.0% full, −7.1% core | pass |
| 2 | correctly rounded bigfloat square root | `src/float.lisp` | −34% on bigfloat-heavy tests, −3.8% core | 8 change |
| 3 | arity-error name only on error | `src/defmfun-check.lisp` | ≈ −1.5% full (2.0M calls × 1 µs) | pass |
| 4 | cheaper binding of local variables | `src/mlisp.lisp` | −3.3% on the integration tests | pass |
| 5 | `SIMPLIFYA`: one pass over the plist | `src/simp.lisp` | none measurable | pass |
| 6 | larger GC nursery | `src/init-cl.lisp` | none measurable | pass |
| 7 | genvars via `MAKE-SYMBOL` | `src/rat3e.lisp` | none measurable | 1 changes |

Effects per change come from the runtime prototypes in `profiling/prototypes`
(see `profiling/README.md`), timed with a discarded warm-up and interleaved
rounds. The totals above and the demo below were measured on these source
patches against an unmodified build of the same commit.

## 2. Demo

Executed, median of three alternating runs on each build. Old is HEAD
(`4e18758`), new is HEAD with patches 1 to 4.

```
timeit(e) ::= buildq([e], block([t0 : elapsed_run_time()], e, elapsed_run_time() - t0))$
timeit(for i thru 20000 do sign(a + b + 1));
(fpprec : 100, x : bfloat(2), timeit(for i thru 20000 do sqrt(x)));
(fpprec : 16, timeit(for i thru 200000 do sin(y)));
(f(x) := block([u, v], u : x, v : u, v), timeit(for i thru 100000 do f(i)));
```

| loop | old | new | patch |
|------|-----|-----|-------|
| `sign(a + b + 1)`, 20,000 times | 0.256 s | 0.100 s | 1 |
| `sqrt(x)` at `fpprec : 100`, 20,000 times | 0.266 s | 0.058 s | 2 |
| `sin(y)`, 200,000 times | 0.497 s | 0.336 s | 3 |
| `f(i)` with two block locals, 100,000 times | 0.317 s | 0.284 s | 4 |

## 3. Patches

Each diff is against `4e18758` and applies on its own with `git apply`.

### 3.1 `sign()` of linear sums without `factor()` (`src/compar.lisp`)

**Idea.** When `SIGN-MPLUS` cannot decide the sign of a sum, `SIGNFACTOR`
runs the full `factor()` on it, if it is small, and tries the factors. Over
the full suite that happens 924,000 times. 782,000 of these sums are linear
in their kernels, factoring them took 12 s, and it gained a sign in 2,900
cases. For a linear sum with rational coefficients `factor()` can only pull
out the numeric content, so `FACTOR-LINEAR-SUM` does that directly: gcd of
the numerators over lcm of the denominators, with the coefficient of the
last term made positive as `factor()` does. Any other sum still goes to
`factor()`. The product is built without simplifying it, since `MUL` would
distribute a content of `-1` back into the sum, and it only goes to `SIGN`.

**Effect.** Full suite −6.5 ± 0.4 s (−5.0%), core −5.1 ± 0.8 s (−7.1%).
Running old and new side by side on every call in the full suite, the sign
differed in 7 of 924,000 calls, all sums with a `log` whose argument
`factor()` rewrites: old `complex`, new `pnz`. No test result changes.

```diff
diff --git a/src/compar.lisp b/src/compar.lisp
index 360d968..d4ae78b 100644
--- a/src/compar.lisp
+++ b/src/compar.lisp
@@ -1944,7 +1944,7 @@ TDNEG TDZERO TDPN) to store it, and also sets SIGN."
 
 (defun signfactor (x)
   (let (y (factored t))
-    (setq y (factor-if-small x))
+    (setq y (or (factor-linear-sum x) (factor-if-small x)))
     (cond ((or (mplusp y) (> (conssize y) 50.))
 	   (setq sign '$pnz)
 	   nil)
@@ -1958,6 +1958,31 @@ TDNEG TDZERO TDPN) to store it, and also sets SIGN."
 	(declare (special $ratprint)) ; Really necessary? $RATPRINT is in globals.lisp!
 	(factor x)) x))
 
+;; For a sum with rational coefficients that is linear in its kernels, as most
+;; sums that reach SIGNFACTOR are, FACTOR can only pull out the numeric content
+;; and make the coefficient of the last term positive. This does the same much
+;; faster. It returns X when that changes nothing, and NIL for any other sum.
+;; The product stays unsimplified, as MUL would distribute a content of -1.
+(defun factor-linear-sum (x)
+  (let ((g 0) (l 1) c)
+    (dolist (term (cdr x))
+      (setq c (cond ((mnump term) term)
+                    ((linear-kernelp term) 1)
+                    ((and (mtimesp term) (null (cdddr term))
+                          (linear-kernelp (caddr term)))
+                     (cadr term))))
+      (unless (or (integerp c) (ratnump c))
+        (return-from factor-linear-sum nil))
+      (setq g (gcd g (num1 c)) l (lcm l (denom1 c))))
+    (setq g (div (if (minusp (num1 c)) (- g) g) l))
+    (if (eql g 1)
+        x
+        (list '(mtimes) g
+              (addn (mapcar #'(lambda (term) (div term g)) (cdr x)) t)))))
+
+(defun linear-kernelp (e)
+  (or (atom e) (not (member (caar e) '(mplus mtimes mexpt)))))
+
 (defun sign-mexpt (x)
   (let* ((expt (caddr x)) (base1 (cadr x))
 	 (sign-expt (sign1 expt)) (sign-base (sign1 base1))
```

**Examples,** executed, next to what `factor()` gives. The products of
`FACTOR-LINEAR-SUM` are unsimplified, hence `-1*(x-1)`:

| sum | `FACTOR-LINEAR-SUM` | `factor()` |
|-----|---------------------|------------|
| `2*a-2*b` | `-2*(b-a)` | `-2*(b-a)` |
| `a/2-b/2` | `(-1/2)*(b-a)` | `-((b-a)/2)` |
| `3/2-(3*x)/2` | `(-3/2)*(x-1)` | `(-3*(x-1))/2` |
| `4*sin(y)+6*x+2` | `2*(2*sin(y)+3*x+1)` | `2*(2*sin(y)+3*x+1)` |
| `1-x` | `-1*(x-1)` | `-(x-1)` |
| `b+a+1` | the sum itself | `b+a+1` |
| `x^2-1` | NIL, a power | `(x-1)*(x+1)` |
| `a*b+2*a` | NIL, a product of kernels | `a*(b+2)` |
| `c+2*(b+a)` | NIL, a sum in a product | `c+2*b+2*a` |
| `0.5*x+1` | NIL, a float | `(x+2)/2` |
| `sqrt(2)*x+2` | NIL, an irrational coefficient | `sqrt(2)*x+2` |

**Proposed test,** in `tests/rtest_sign.mac` before the block marked "Leave
this at the end of the file!". Same result before and after the patch:

```
/* sign() of a linear sum takes the numeric content out without factor() */

block([ans],
  local(a, b, x),
  assume(a > b, x < 1),
  ans : [sign(2*a - 2*b), sign(2*b - 2*a), sign(a/2 - b/2), sign(1 - x),
         sign(4 - 4*x), sign(3/2 - 3*x/2)],
  ans);
[pos, neg, pos, pos, pos, pos]$
```

**Commit message:**

```
Speed up sign() of sums by skipping factor()

When sign() cannot decide the sign of a sum, it factors the sum and
tries again. For a sum that is linear in its terms, factoring can only
pull out a common numeric factor, so now this is done directly instead
of calling factor(). sign() of such sums gets about 2.5 times faster,
and the test suite 5 to 7% faster, with the same results.

AI-Assisted-By: Claude Opus 5.5
```

### 3.2 Correctly rounded bigfloat square root (`src/float.lisp`)

**Idea.** `FPROOT` computes `a^(1/n)` by Newton iteration from a power of two,
with a full-precision division per step, and is 1 ulp off for about 19% of
arguments. Nearly all calls are square roots (bigfloat `sqrt`, `abs` of
complex bigfloats). For `n = 2`, `FPROOT-SQRT` takes `ISQRT` of the mantissa,
shifted so the root has two guard bits, adds a sticky bit if the root is
inexact, and rounds once with `FPROUND`: always correctly rounded, 11 to 20
times faster per call from 5 to 100,000 digits, for random arguments and
for exact squares. It only does integer arithmetic on the exponent, while
`FPROOT` builds its start value `2^(e/2)` by repeated squaring, which takes
about 1 s for an exponent near `2^100000`. Zero, negative arguments and
decimal bigfloats still go the old way. The name `FPSQRT` is taken in
`src/cpoly.lisp`.

**Effect.** −2.6 ± 0.1 s (−34%) on `rtest14`, `rtest_hg` and `rtest_gamma`,
where nearly all square roots happen, −2.7 ± 0.8 s (−3.8%) on the core suite.

**Accuracy.** Over 32,000 arguments (5,000 non-square integers and 5,000
random values in [1, 4) at `fpprec` 16, 30 and 100, 1,000 of each at
1000), the old root is correctly rounded for 81% of them, with errors up
to 0.70 ulp, and the new one for all. Squaring back with `^2` is a weak
check, since even a correctly rounded root gives back `x` only about half
the time. Both do that equally often, but the old root misses by 2 ulp in
2 to 4% of the cases and the new one never by more than 1. Squared
exactly, without rounding, the new root is always the closest bigfloat.

**Tests.** Eight tests fail with the patch. None of the failures is a
wrong result from the new square root. The values move by a few ulps,
and the tests compared two computed values with each other, compared
with a value Maxima once printed, or used a tolerance below the accuracy
of the function. The changes below compare with correct values instead,
computed with mpmath at 90 digits and checked against Maxima at
`fpprec : 90`, which agrees to 78 digits. Tolerances are at least twice
the largest error measured with either square root. Each test stays one
test, so no problem numbers move.

| test | why it fails | error before → after the patch |
|------|--------------|--------------------------------|
| `rtest_gamma` 742, 743 | round trip `erf(inverse_erf(±2b0))`, tolerance `5.9b-24` at `fpprec : 24`, below the error of `erf()` | `inverse_erf` 2.97e-24 → 2.86e-24, `erf` 1.42e-23 → 1.42e-23 |
| `rtest_elliptic` 75 | `elliptic_kc(1/2)` against its closed form at `fpprec : 100`, tolerance `2b-100`, about 6 ulp | 0.81e-100 → 2.24e-100 |
| `rtest_elliptic` 235 | `jacobi_am(100b0, .5b0)` against a correct literal, tolerance `1b-35`, below 1 ulp (2e-31) | 0.08 → 0.92 ulp |
| `rtest_elliptic` 245 | identity `sin(jacobi_am(z, m)) = jacobi_sn(z, m)` at `m = .5b0` | `jacobi_sn` 1.9e-32 → 7.4e-32 |
| `rtest_elliptic` 250 | the same identity at `m = 1.75b0+%i` | both 1.8e-20 → 2.3e-20, see the `jacobi_sn` bug report below |
| `rtest_elliptic` 254 | `elliptic_e(1, 2b0)` against a literal whose imaginary part is off by 7.3e-33, tolerance `3.1055b-33` | 8.8e-33 → 4.8e-33 |
| `rtest_limit_extra` 187 | the `limit()` bug in the bug report below | |

`inverse_erf` is as accurate as before: over 40 arguments from 1.05 to 3,
its mean error is 2.6e-24 with both square roots. `erf()` is only
accurate to about 1e-23 at `fpprec : 24`, because the continued fraction
in `complex-bfloat-gamma-incomplete` stops at a relative change of
`10^-fpprec` and rounding errors add up over hundreds of iterations. The
Newton iteration of `inverse_erf` starts from a value built with complex
square roots, so a different last bit there makes it stop a few ulps
away, where the error of `erf()` differs. `jacobi_sn` at `m = .5b0` gets
less accurate because its descending Landen transform
(`DESCENDING-TRANSFORM` in `src/ellipt.lisp`) computes `1 - sqrt(1 - m)`
by subtraction, which makes it sensitive to how the root rounds. A
cancellation-free form would be a separate change.

**Proposed test changes,** all **modified**. With them, `rtest_gamma` and
`rtest_elliptic` pass with and without the patch, and `rtest_limit_extra`
fails only at 187 with it.

If the `limit()` fix in `handover-limit-bigfloat-point.md` goes in first,
187 passes with this patch and needs no registry entry. If the
`jacobi_sn()` fix in `handover-jacobi-sn-digits.md` goes in first, its
versions of 245 and 250 pass with this patch, so skip those two here. The
other changes are needed either way.

`tests/rtest_gamma.mac` 742 and 743: check both functions against correct
values instead of the round trip.

Old:

```
relerror(
 erf(inverse_erf(2b0)),
 2b0,
 5.9b-24);
true;

relerror(
 erf(inverse_erf(-2b0)),
 -2b0,
 5.9b-24);
true;
```

New:

```
block([w : 8.69417565326184121325376032827b-1 - 1.34059481853027711609362687433b0*%i],
  [relerror(inverse_erf(2b0), w, 1b-23),
   relerror(erf(w), 2b0, 3b-23)]);
[true, true];

block([w : -8.69417565326184121325376032827b-1 + 1.34059481853027711609362687433b0*%i],
  [relerror(inverse_erf(-2b0), w, 1b-23),
   relerror(erf(w), -2b0, 3b-23)]);
[true, true];
```

`tests/rtest_elliptic.mac` 75: tolerance about 15 ulp, and the second
argument written without the cancellation in `17-12*sqrt(2)`, which made
the result depend on the last bit of `sqrt(2)`.

Old:

```
block([oldfpprec : fpprec, fpprec:100],
  test_table('elliptic_kc,
	     [[[1/2], 8*%pi^(3/2)/gamma(-1/4)^2],
	      [[17-12*sqrt(2)], 2*(2+sqrt(2))*%pi^(3/2)/gamma(-1/4)^2],
	      [[-1], gamma(1/4)^2/4/sqrt(2*%pi)]],
	     2b-100));
[];
```

New:

```
block([oldfpprec : fpprec, fpprec:100],
  test_table('elliptic_kc,
	     [[[1/2], 8*%pi^(3/2)/gamma(-1/4)^2],
	      [[1/(17+12*sqrt(2))], 2*(2+sqrt(2))*%pi^(3/2)/gamma(-1/4)^2],
	      [[-1], gamma(1/4)^2/4/sqrt(2*%pi)]],
	     5b-100));
[];
```

`tests/rtest_elliptic.mac` 235: tolerance about 5 ulp. The literal is
correct.

Old:

```
closeto(jacobi_am(100b0, .5b0) - 84.7031127241138244025523773764166798b0, 1b-35);
true$
```

New:

```
closeto(jacobi_am(100b0, .5b0) - 84.7031127241138244025523773764166798b0, 1b-30);
true$
```

`tests/rtest_elliptic.mac` 245: check `jacobi_sn` and `sin(jacobi_am)`
against correct values of `sn(2*k, 1/2)`.

Old:

```
makelist(block([z : 2*k, m : .5b0],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 1.8489b-32)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
block([ref : [0,
               9.94662325358017683601254302053498830b-1,
               -2.85777915711237958898072295463821503b-1,
               -9.50974164149004587670557756887183760b-1,
               5.38215011375607382129700566218767082b-1,
               8.58812505952778731596037769874678293b-1,
               -7.36572976119784415625874479434183301b-1,
               -7.11120606110816077849800848330029327b-1,
               8.75695285305898245412937680262292166b-1,
               5.04142728502709650306522974280727257b-1,
               -9.60287786721909890934860303094132890b-1]],
  makelist(block([z : 2*k, m : .5b0],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 1.5b-31),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 1.5b-31)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

`tests/rtest_elliptic.mac` 250: the same for `m = 1.75b0+%i`. The
tolerance is large because `jacobi_sn` loses up to 12 digits here, see
the bug report below. Fill in its number in the comment.

Old:

```
makelist(block([z : 2*k*%i, m : 1.75b0+%i],
    closeto(sin(jacobi_am(z, m))-jacobi_sn(z, m), 4.8135b-32)),
  k, 0, 10);
[true, true, true, true, true, true, true, true, true, true, true];
```

New:

```
/* jacobi_sn loses up to 12 digits here as abs(z) grows (bug #NNNN) */
block([ref : [0,
               -5.72103423535998727958019373033266031b-1 - 6.02110905520123281469893500645172105b-1*%i,
               -1.43624556086199438442502525331485902b0 + 1.17007597919997767395941561207354807b-1*%i,
               -7.62467854352286456198305764923936017b-1 + 3.02324185363929479211313523082241917b-1*%i,
               -6.91325022821222063674474488732064470b-1 + 1.57129015423914619697483681220484576b-1*%i,
               -9.65794333174415071357345450678257393b-1 + 1.26800493159222275355516292611889998b-1*%i,
               -9.83336472112777656992190024923096366b-1 - 1.77608133015453039714665379644542393b-1*%i,
               -4.58553715646803605307335052496046927b-1 - 7.85457625331070626791189048860725482b-2*%i,
               -1.76317410880759372631708702431579915b-1 + 4.99006579516169313026664590060526183b-1*%i,
               1.59732274948654412318481916287919482b0 + 2.48966529521992193162783920166078882b0*%i,
               8.28024493078028417849365960338059298b-1 - 8.73821661974562686608211314376421172b-1*%i]],
  makelist(block([z : 2*k*%i, m : 1.75b0+%i],
      [closeto(jacobi_sn(z, m) - ref[k + 1], 5b-20),
       closeto(sin(jacobi_am(z, m)) - ref[k + 1], 5b-20)]),
    k, 0, 10));
[[true, true], [true, true], [true, true], [true, true], [true, true], [true, true],
 [true, true], [true, true], [true, true], [true, true], [true, true]];
```

`tests/rtest_elliptic.mac` 254: imaginary part of the literal corrected
(`...615034` → `...615041`), tolerance about 13 ulp.

Old:

```
closeto(elliptic_e(1,2b0) - (9.3112921772178507209815461615034b-2*%i+5.9907011736779610371996124614016b-1), 3.1055b-33);
true;
```

New:

```
closeto(elliptic_e(1,2b0) - (9.3112921772178507209815461615041b-2*%i+5.9907011736779610371996124614016b-1), 2b-32);
true;
```

`src/testsuite.lisp`: list `rtest_limit_extra` 187 as an expected failure
until the `limit()` bug is fixed. This goes into the same commit as the
patch, since 187 passes without it.

```diff
diff --git a/src/testsuite.lisp b/src/testsuite.lisp
index 8fe4463..2c9be77 100644
--- a/src/testsuite.lisp
+++ b/src/testsuite.lisp
@@ -139,6 +139,7 @@
           ((mlist simp)  42 59 61 82 83 84 89 
                          96 104 
                          124 125 126 127 132 133 135 136 137
+                         187
                          240 243 244 245 246 249
                          267 272
                          357 358))
```

```diff
diff --git a/src/float.lisp b/src/float.lisp
index 486bcd9..1ab46e5 100644
--- a/src/float.lisp
+++ b/src/float.lisp
@@ -1855,6 +1855,36 @@
 	((< n 0) (invertbigfloat (exptbigfloat p (- n))))
 	(t (bcons (fpexpt (cdr p) n)))))
 
+;; Correctly rounded square root of the positive bigfloat A, given and returned
+;; like the argument and the result of FPROOT.
+;;
+;; A is M*2^(E-FPPREC). With M shifted left by SHIFT bits,
+;;
+;;   sqrt(A) = sqrt(M*2^SHIFT) * 2^((E-FPPREC-SHIFT)/2),
+;;
+;; so the square root of A is the integer square root of M*2^SHIFT, scaled by
+;; a power of 2.
+(defun fproot-sqrt (a)
+  (destructuring-bind (m e) (cdr (bigfloatp a))
+    (let* (;; Enough bits for the integer root to have FPPREC+2 bits: FPPREC
+           ;; for the result and two guard bits for the rounding.
+           (shift (max 0 (- (+ (* 2 fpprec) 4) (integer-length m))))
+           ;; One more if needed, so that the exponent can be halved.
+           (shift (if (oddp (- e fpprec shift)) (1+ shift) shift))
+           (scaled (ash m shift))
+           ;; The root, rounded down to an integer.
+           (root (isqrt scaled))
+           ;; 1 if ROOT is not exact. Without this bit, FPROUND could take a
+           ;; root just above a tie between two results for the tie itself.
+           (sticky (if (= (* root root) scaled) 0 1))
+           ;; Round ROOT with the sticky bit appended, which is 2*ROOT+STICKY,
+           ;; to FPPREC bits. FPROUND sets *M to the number of bits it cut off.
+           (mantissa (fpround (+ (* 2 root) sticky))))
+      ;; The result is MANTISSA*2^(*M-1+(E-FPPREC-SHIFT)/2), where -1 undoes
+      ;; the appended bit. A bigfloat (MANTISSA X) has the value
+      ;; MANTISSA*2^(X-FPPREC), hence the FPPREC.
+      (list mantissa (+ *m -1 (/ (- e fpprec shift) 2) fpprec)))))
+
 (defun fproot (a n)  ; computes a^(1/n)  see Fitch, SIGSAM Bull Nov 74
 
   ;; Special case for a = 0b0. General algorithm loops endlessly in that case.
@@ -1864,6 +1894,11 @@
   ;; '((BIGFLOAT ...) FOO BAR) instead of '(FOO BAR).
   ;; However FPROOT does return something like '(FOO BAR).
 
+  ;; Square roots, the most common case by far, are much faster and
+  ;; correctly rounded with FPROOT-SQRT.
+  (when (and (eql n 2) (null *decfp) (plusp (cadr a)))
+    (return-from fproot (fproot-sqrt a)))
+
   (if (eql (cadr a) 0)
       '(0 0)
       (progn
```

**Proposed test,** in `tests/rtest16.mac` before the block marked "Leave
this at the end of the file!". It gives `[3, 10, 12, 17, 33, 40]` before the
patch and `[]` after it:

```
/* sqrt() of a bigfloat is correctly rounded */

block([fpprec : 80, hi],
  hi : makelist(sqrt(bfloat(k)), k, 2, 40),
  fpprec : 40,
  sublist(makelist(k, k, 2, 40),
          lambda([k], is(sqrt(bfloat(k)) # bfloat(hi[k - 1])))));
[]$
```

**Bug report** for the `limit()` bug behind `rtest_limit_extra` 187, which
exists without this patch:

**Title:**
```
limit() at a bigfloat point returns ind for a continuous function
```

**Body:**
````markdown
`limit(atan(x), x, 1.1b0)` returns `ind`, although `atan` is continuous at `1.1b0`. Both one-sided limits give the right value, and with the float `1.1` the limit is fine:

```
(%i1) limit(atan(x), x, 1.1b0);
(%o1)                                 ind
(%i2) [limit(atan(x), x, 1.1b0, plus), limit(atan(x), x, 1.1b0, minus)];
(%o2)            [8.329812666744316b-1, 8.329812666744316b-1]
(%i3) limit(atan(x), x, 1.1);
(%o3)                         0.8329812666744317
```

The two one-sided limits differ in the last bit of the bigfloat, and the two-sided limit treats them as different. At the default `fpprec`, the same happens for `bfloat(6/5)`, `bfloat(13/10)`, `bfloat(7/5)` and `bfloat(3/2)`.
````

**Bug report** for `jacobi_sn`, found while checking tests 245 and 250
against correct values. It exists without this patch:

**Title:**
```
jacobi_sn and jacobi_am lose many digits for larger arguments
```

**Body:**
````markdown
`jacobi_sn` returns wrong digits for moderately large arguments, with floats and with bigfloats:

```
(%i1) display2d:false$
(%i2) jacobi_sn(20.0, 2.5);
(%o2) 0.2282499166210549
(%i3) jacobi_sn(20.0*%i, 0.75);
(%o3) -0.2325375293373911*%i
(%i4) jacobi_sn(20.0*%i, 1.75+%i);
(%o4) 0.827993704767369-0.8735275803124387*%i
```

The correct values are `0.2282498061811287`, `-0.2325715776527406*%i` and `0.8280244930780284-0.8738216619745627*%i` (computed with mpmath, and Maxima agrees at `fpprec : 90`). So the first result is wrong from the 7th digit on, and the others from the 4th or 5th. `sin(jacobi_am(z, m))` gives the same wrong values. With bigfloats at `fpprec : 32`, `jacobi_sn(20b0*%i, 1.75b0+%i)` has only about 20 correct digits. The error grows quickly with `abs(z)`.

The tests 240 to 250 in `rtest_elliptic.mac` do not notice, because they only check that `sin(jacobi_am(z, m))` equals `jacobi_sn(z, m)`.
````

**Commit message:**

```
Compute bigfloat square roots exactly and faster

Bigfloat square roots came from a Newton iteration with a full
division in every step, and about one in five was off in the last
bit. Now they come from an integer square root of the mantissa and
are always correctly rounded, and sqrt() of a bigfloat at fpprec : 100
is about 4.5 times faster. This speeds up bigfloat hypergeometric and
elliptic functions.

The old tolerances were fitted to the rounding errors of the old
results, some even below one ulp, so any change in the last bits broke
them, even one toward the correct value. The tests now compare with
correct values, and their tolerances reflect the real accuracy of the
tested functions, which depends on far more than the square root.
rtest_limit_extra 187 fails because of an existing limit() bug (#NNNN)
and is listed as an expected failure.

AI-Assisted-By: Claude Opus 5.5
```

### 3.3 Arity-error name only on error (`src/defmfun-check.lisp`)

**Idea.** Every simplifier defined with `DEF-SIMPLIFIER` (`%log`, `%sin`,
`%cos`, `%atan2`, `cabs` and many more) built the function's name for a
wrong-number-of-arguments error on each call, through `DOLLARIFY`, which
prints every parameter name and reads it back (`MEXPLODEN`, `READLIST`,
`MAKEALIAS`). Now the name is built only when the argument count is wrong.
The error message is unchanged.

**Effect.** The full suite makes 2.0 million such calls at about 1 µs each,
so about 2 s or 1.5% per full run. That is too small for the interleaved
whole-suite runs to resolve, but it shows directly: simplifying `sin(y)` is
1.5 times faster (demo above).

```diff
diff --git a/src/defmfun-check.lisp b/src/defmfun-check.lisp
index 20a0bba..617af47 100644
--- a/src/defmfun-check.lisp
+++ b/src/defmfun-check.lisp
@@ -821,11 +821,11 @@
 	    (defun ,simp-name (,form-arg ,unused-arg ,z-arg)
 	      (declare (ignore ,unused-arg)
 		       (ignorable ,z-arg))
-              (let ((pretty-name `((,',noun-name) ,@(rest (dollarify ',lambda-list)))))
-                ;;(format t "pretty-name = ~A~%" pretty-name)
-	        (arg-count-check ,(length lambda-list)
-			         ,form-arg
-                                 pretty-name))
+              ;; Build the name for the error message only when it is needed:
+              ;; DOLLARIFY is slow, and this runs on every simplification.
+              (unless (= ,(length lambda-list) (length (rest ,form-arg)))
+                (wna-err ,form-arg ,(length lambda-list)
+                         `((,',noun-name) ,@(rest (dollarify ',lambda-list)))))
 	      (let ,arg-forms
 	        ;; Allow args to give-up if the default args won't work.
 	        ;; Useful for the (rare?) case like genfact where we want
```

**Proposed test,** in `tests/rtest16.mac` before the final block. Same result
before and after the patch:

```
/* A simplifying function still names itself in an arity error */

[(errcatch(log(1, 2)), second(error)),
 (errcatch(jacobi_sn(1, 2, 3)), second(error))];
[log(y), jacobi_sn(u, m)]$
```

**Commit message:**

```
Speed up simplification of sin(), log() and others

Simplifying functions such as sin() and log() built the name to show
in a wrong-number-of-arguments error on every call, even when the
number was right, and building it means printing and reading back
symbols. Now it is built only for the error message. Simplifying
sin(x) gets about 1.5 times faster.

AI-Assisted-By: Claude Opus 5.5
```

### 3.4 Cheaper binding of local variables (`src/mlisp.lisp`)

**Idea.** Every Maxima function call and `block` binds its locals with
`MBIND-DOIT`, which ran `$listofvars` on each variable just to check that it
is a symbol. A symbol always passes that check, so now only other
variables get it. On the way out, `MUNBIND-MAKUNBOUND` removed the variable
from `values` with the generic `DELETE` and its keyword arguments, and now
uses a plain loop.

**Effect.** −0.9 ± 0.3 s (−3.3%) on `rtest_integrate` and
`rtest_abs_integrate`, which carry 60% of the binding cost. Calls of a
function with two block locals get 1.1 times faster (demo above).

```diff
diff --git a/src/mlisp.lisp b/src/mlisp.lisp
index 48aa607..78b9490 100644
--- a/src/mlisp.lisp
+++ b/src/mlisp.lisp
@@ -538,7 +538,7 @@ wrapper for this."
 			    (cons (ncons fnname) lamvars))
 			(cons '(mlist) fnargs)))))
     (let ((var (car vars)))
-      (when (not (every 'symbolp (cdr ($listofvars var))))
+      (unless (or (symbolp var) (every 'symbolp (cdr ($listofvars var))))
 	  (merror (intl:gettext "Only symbols can be bound; found: ~M") var))
       (let ((value (symbol-values-in var)))
 	(let ((mbindp t))
@@ -585,7 +585,13 @@ wrapper for this."
 
 (defun munbind-makunbound (var)
   (makunbound var)
-  (setf $values (delete var $values :count 1 :test #'eq)))
+  ;; Remove VAR from $VALUES, like (DELETE VAR $VALUES :COUNT 1), but without
+  ;; the overhead of the generic DELETE, as this runs for every unbound local.
+  (do ((l $values (cdr l)))
+      ((atom (cdr l)))
+    (when (eq (cadr l) var)
+      (rplacd l (cddr l))
+      (return))))
 
 (defun munbind (vars)
   (dolist (var (reverse vars))
```

**Proposed test,** in `tests/rtest16.mac` before the final block. Same result
before and after the patch:

```
/* Locals that were unbound before leave values when the block exits */

(kill(aa, bb, cc),
 block([aa, bb, cc], bb : 1),
 [member(aa, values), member(bb, values), member(cc, values)]);
[false, false, false]$
```

**Commit message:**

```
Speed up function calls with local variables

Binding a local variable checked in a roundabout way that it is a
symbol, and unbinding it removed it from values with a slow generic
function. Both are direct now, which makes calls of Maxima functions
with local variables about 10% faster.

AI-Assisted-By: Claude Opus 5.5
```

## 4. Tried without measurable gain

These were measured like the others and are not recommended. The diffs are
here for reference.

### 4.1 `SIMPLIFYA`: one pass over the plist (`src/simp.lisp`)

**Idea.** `SIMPLIFYA` looks up `distribute_over`, `opers` and `operators`
with three `GET`s per dispatch. For operators that lack the first two
(`MLIST`, `MEQUAL`, `MABS`, the relational operators) each lookup walks the
whole plist, 42 to 47 cells per call. One pass collects all three.

**Effect.** −0.3 ± 0.3 s on `rtest_integrate` and `rtest_abs_integrate`,
−1.7 ± 1.7 s core, −2.6 ± 2.2 s full: within the noise. The cost of `GET`
there is the calls, not the walk. Note that a local named `opers` would bind
Maxima's special `OPERS`.

```diff
diff --git a/src/simp.lisp b/src/simp.lisp
index 86d989c..870f06c 100644
--- a/src/simp.lisp
+++ b/src/simp.lisp
@@ -274,28 +274,43 @@
 		    (and (not (atom (caaar x))) (eq (caaaar x) 'lambda)))
 		(mapply1 op (cdr x) op x))
 	       (t (merror (intl:gettext "simplifya: operator is neither an atom nor a lambda expression: ~S") x))))
-        ((and $distribute_over
-              (get op 'distribute_over)
-              ;; A function with the property 'distribute_over.
-              (distribute-over x y)))
-	((get op 'opers)
-	 (let ((opers-list *opers-list)) (oper-apply x y)))
-	((and (eq op 'mqapply)
-	      (or (atom (cadr x))
-		  (and (eq substp 'mqapply)
-		       (or (eq (car (cadr x)) 'lambda)
-			   (eq (caar (cadr x)) 'lambda)))))
-	 (cond ((or (symbolp (cadr x)) (not (atom (cadr x))))
-		(simplifya (cons (cons (cadr x) (cdar x)) (cddr x)) y))
-	       ((or (not (member-eq 'array (cdar x))) (not $subnumsimp))
-		(merror (intl:gettext "simplifya: I don't know how to simplify this operator: ~M") x))
-	       (t (cadr x))))
-	(t (let ((w (get op 'operators)))
-	     (cond ((and w
-	                 (or (not (member-eq 'array (cdar x)))
-	                     (rulechk op)))
-		    (funcall w x 1 y))
-		   (t (simpargs x y))))))))
+	(t
+	 ;; Look up DISTRIBUTE_OVER, OPERS and OPERATORS in one pass over the
+	 ;; plist instead of three GETs. As with GET, the first one counts.
+	 (let (dist-prop oper-prop simp-fn (found 0))
+	   (do ((pl (symbol-plist op) (cddr pl)))
+	       ((or (atom pl) (= found 7)))
+	     (case (car pl)
+	       (distribute_over
+	        (unless (logtest found 1)
+	          (setq dist-prop (cadr pl) found (logior found 1))))
+	       (opers
+	        (unless (logtest found 2)
+	          (setq oper-prop (cadr pl) found (logior found 2))))
+	       (operators
+	        (unless (logtest found 4)
+	          (setq simp-fn (cadr pl) found (logior found 4))))))
+	   (cond ((and $distribute_over
+	               dist-prop
+	               ;; A function with the property 'distribute_over.
+	               (distribute-over x y)))
+	         (oper-prop
+	          (let ((opers-list *opers-list)) (oper-apply x y)))
+	         ((and (eq op 'mqapply)
+	               (or (atom (cadr x))
+	                   (and (eq substp 'mqapply)
+	                        (or (eq (car (cadr x)) 'lambda)
+	                            (eq (caar (cadr x)) 'lambda)))))
+	          (cond ((or (symbolp (cadr x)) (not (atom (cadr x))))
+	                 (simplifya (cons (cons (cadr x) (cdar x)) (cddr x)) y))
+	                ((or (not (member-eq 'array (cdar x))) (not $subnumsimp))
+	                 (merror (intl:gettext "simplifya: I don't know how to simplify this operator: ~M") x))
+	                (t (cadr x))))
+	         ((and simp-fn
+	               (or (not (member-eq 'array (cdar x)))
+	                   (rulechk op)))
+	          (funcall simp-fn x 1 y))
+	         (t (simpargs x y))))))))
 
 ;; EQTEST returns an expression which is the same as X
 ;; except that it is marked with SIMP and maybe other flags from CHECK.
```

### 4.2 Larger GC nursery (`src/init-cl.lisp`)

**Idea.** Four times SBCL's default `bytes-consed-between-gcs` (5% of the
dynamic space, 51 MB here).

**Effect.** GC time in the full suite drops from 8.2 to 2.9 s, but total CPU
time moves within the noise (−1.9 ± 2.4 s full, −1.4 ± 0.5 s core,
−0.2 ± 0.3 s on the integration tests). The time saved in GC is apparently
lost elsewhere.

```diff
diff --git a/src/init-cl.lisp b/src/init-cl.lisp
index fb850e4..dd9bb6a 100644
--- a/src/init-cl.lisp
+++ b/src/init-cl.lisp
@@ -692,6 +692,9 @@ maxima [options] --batch-string='batch_answers_from_file:false; ...'
     (setf *read-default-float-format* 'lisp::double-float))
 
   #+sbcl (setf *read-default-float-format* 'double-float)
+  ;; A nursery of 4 times SBCL's default of 5% of the dynamic space.
+  #+sbcl (setf (sb-ext:bytes-consed-between-gcs)
+               (* 4 (sb-ext:bytes-consed-between-gcs)))
 
   ;; GCL: disable readline symbol completion,
   ;; leaving other functionality (line editing, anything else?) enabled.
```

### 4.3 Genvars via `MAKE-SYMBOL` (`src/rat3e.lisp`)

**Idea.** `ORDERPOINTER` makes a fresh `GENSYM` for every variable of each
rational conversion, and `PRENUMBER` numbers all genvars with `SET`, which
goes through SBCL's `ABOUT-TO-MODIFY-SYMBOL-VALUE`. `MAKE-SYMBOL` skips the
counter, and the global value can be written directly.

**Effect.** −0.2 s on the full suite in an early screening, and the result
of `rtest15` #49 (`trigrat`) changes, so genvar names matter somewhere. Not
worth it.

```diff
diff --git a/src/rat3e.lisp b/src/rat3e.lisp
index cacbb02..b05fa75 100644
--- a/src/rat3e.lisp
+++ b/src/rat3e.lisp
@@ -518,12 +518,12 @@
 (defun gensym-readable (symname)
  (if (or (and (eq *use-readable-gensyms* :debug) (not *mdebug*) *debugger-hook*)
          (not *use-readable-gensyms*))
-  (gensym)
+  (make-symbol "G")
   (cond ((symbolp symname)
-	 (gensym (string-trim "$" (string symname))))
+	 (make-symbol (string-trim "$" (string symname))))
 	(t
 	 (setq symname (aformat nil "~:M" symname))
-	 (if symname (gensym symname) (gensym))))))
+	 (if symname (make-symbol symname) (make-symbol "G"))))))
 
 (defun orderpointer (l)
   (loop for v in l
@@ -540,7 +540,10 @@
   (do ((vl v (cdr vl))
        (i n (1+ i)))
       ((null vl) nil)
-    (setf (symbol-value (car vl)) i)))
+    ;; Genvars are never declared or bound, so SBCL can skip the checks of
+    ;; SET and write the global value directly.
+    #+sbcl (sb-kernel:%set-symbol-global-value (car vl) i)
+    #-sbcl (setf (symbol-value (car vl)) i)))
 
 (defun rget (genv)
   (cons (if (and $ratwtlvl
```

## 5. Verification

- **Builds.** HEAD with patches 1 to 4, and HEAD with all seven, compile
  with the same warnings as HEAD itself (SBCL 2.6.9).
- **`make check`.** With patches 1 to 4 only the eight tests in 3.2 fail,
  and the dependency check passes. With all seven, `rtest15` #49
  (`trigrat`, see 4.3) fails as well.
- **Proposed tests.** Run in place in `tests/rtest_sign.mac` and
  `tests/rtest16.mac`. With patches 1 to 4, and with all seven, both files
  pass (`rtest_sign` 781/781 plus 5 expected errors, `rtest16` 1085/1085).
  On HEAD the `sqrt()` test fails with `[3, 10, 12, 17, 33, 40]` and the
  other three pass.
- **Test changes in 3.2.** Applied to `rtest_gamma.mac` and
  `rtest_elliptic.mac`, both files pass with and without patch 2 (910/910
  and 258/258), and `rtest_limit_extra` fails only at 187 with it.
- **Totals.** CPU time of `run_testsuite()` from SBCL's `time`, HEAD
  against HEAD with patches 1 to 4, both pinned to the same CPU. One
  warm-up run of each build was discarded, then three rounds in the order
  AB, BA, AB. Full suite 127.6 → 111.9 s (−15.6 ± 1.0 s), core
  72.4 → 61.2 s (−11.1 ± 0.9 s), ± being the standard error of the
  per-round differences.
