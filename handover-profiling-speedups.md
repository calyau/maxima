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
index 360d968..3bf4281 100644
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
@@ -1958,6 +1958,39 @@ TDNEG TDZERO TDPN) to store it, and also sets SIGN."
 	(declare (special $ratprint)) ; Really necessary? $RATPRINT is in globals.lisp!
 	(factor x)) x))
 
+;; Most sums that reach SIGNFACTOR are linear in their kernels, and FACTOR can
+;; only pull a numeric content out of those. FACTOR-LINEAR-SUM does just that,
+;; much faster: for a sum X with rational coefficients, it returns X itself or
+;; the content times the primitive sum, with the coefficient of the last term
+;; made positive as FACTOR does. It returns NIL for any other sum. The product
+;; is left unsimplified, since MUL would distribute a content of -1 again, and
+;; it only goes to SIGN.
+(defun factor-linear-sum (x)
+  (let ((g 0) (l 1) (c 1))
+    (dolist (term (cdr x))
+      (setq c (cond ((or (integerp term) (ratnump term)) term)
+                    ((mnump term) (return-from factor-linear-sum nil))
+                    ((linear-kernelp term) 1)
+                    ((and (mtimesp term) (null (cdddr term))
+                          (or (integerp (cadr term)) (ratnump (cadr term)))
+                          (linear-kernelp (caddr term)))
+                     (cadr term))
+                    (t (return-from factor-linear-sum nil))))
+      (if (integerp c)
+          (setq g (gcd g c))
+          (setq g (gcd g (cadr c)) l (lcm l (caddr c)))))
+    (setq g (div g l))
+    ;; C is the coefficient of the last term now.
+    (when (mminusp c)
+      (setq g (neg g)))
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
inexact, and rounds once with `FPROUND`: always correctly rounded, 8 to 19
times faster per call at 56 to 3,333 bits. Zero, negative arguments and
decimal bigfloats still go the old way. The name `FPSQRT` is taken in
`src/cpoly.lisp`.

**Effect.** −2.6 ± 0.1 s (−34%) on `rtest14`, `rtest_hg` and `rtest_gamma`,
where nearly all square roots happen, −2.7 ± 0.8 s (−3.8%) on the core suite.

**Needs a decision before committing.** Eight tests change. Errors against
the same functions evaluated at twice the precision:

| test | compares | old error | new error |
|------|----------|-----------|-----------|
| `rtest_gamma` 742, 743 | `erf(inverse_erf(±2b0))` with `±2b0`, tolerance `5.9b-24`, `fpprec : 24` | `inverse_erf` 2.97e-24, round trip 5.8e-24 | 2.86e-24, round trip 7.7e-24 |
| `rtest_elliptic` 75 | `elliptic_kc` at `17-12*sqrt(2)` with its closed form, `fpprec : 100`, tolerance `2b-100` | 1.3e-100 | 4.1e-101 |
| `rtest_elliptic` 235 | `jacobi_am(100b0, .5b0)` with a 36-digit literal, tolerance `1b-35`, `fpprec : 32` | 1.9e-34 | 2.1e-33 |
| `rtest_elliptic` 254 | `elliptic_e(1, 2.0b0)` with a 32-digit literal, tolerance `3.1055b-33` | 1.4e-32 | 8.0e-33 |
| `rtest_elliptic` 245, 250 | `sin(jacobi_am(z, m))` with `jacobi_sn(z, m)`, largest gap for `m = .5b0` and `1.75b0+%i` | 1.8e-32, 4.81e-32 | 7.4e-32, 4.82e-32 |
| `rtest_limit_extra` 187 | `limit(atan(x), x, bfloat(sqrt(2)))` with `atan(bfloat(sqrt(2)))` | equal | `ind`, see the bug report below |

So `inverse_erf`, `elliptic_kc` and `elliptic_e` get more accurate, and
`jacobi_am` stays within 1 ulp, where the old result happened to be exact.
The tests at 235 and 254 allow less than 2 ulp of difference to a literal.
`jacobi_sn` gets worse for some `m`: over 80 arguments its mean relative
error at `fpprec : 32` goes from 1.8e-32 to 7.1e-32 for `m = 0.1`, from
3.6e-32 to 1.8e-31 for `m = 0.5`, and stays at 9.2e-32 for `m = 0.9`. Its
descending Landen transform (`DESCENDING-TRANSFORM` in `src/ellipt.lisp`)
computes `1 - sqrt(1 - m)` by subtraction, which makes it sensitive to how
the root rounds. With the cancellation-free `m/(1 + sqrt(1 - m))^2` both
square roots give the same `jacobi_sn` accuracy, but that form is better
for some `m` and worse for others, so it is a separate change. The diff
below does not touch the tests.

```diff
diff --git a/src/float.lisp b/src/float.lisp
index 486bcd9..da515af 100644
--- a/src/float.lisp
+++ b/src/float.lisp
@@ -1855,6 +1855,18 @@
 	((< n 0) (invertbigfloat (exptbigfloat p (- n))))
 	(t (bcons (fpexpt (cdr p) n)))))
 
+;; Square root of the positive bigfloat A, given like the argument of FPROOT,
+;; correctly rounded: ISQRT of the mantissa, shifted so that the root has two
+;; guard bits, and a sticky bit if the root is inexact, rounded by FPROUND.
+(defun fproot-sqrt (a)
+  (destructuring-bind (m e) (cdr (bigfloatp a))
+    (let* ((k (max 0 (- (+ fpprec fpprec 4) (integer-length m))))
+           (k (if (oddp (- e fpprec k)) (1+ k) k))
+           (n (ash m k))
+           (q (isqrt n)))
+      (list (fpround (+ (ash q 1) (if (= (* q q) n) 0 1)))
+            (+ *m (/ (- e fpprec k) 2) -1 fpprec)))))
+
 (defun fproot (a n)  ; computes a^(1/n)  see Fitch, SIGSAM Bull Nov 74
 
   ;; Special case for a = 0b0. General algorithm loops endlessly in that case.
@@ -1864,6 +1876,11 @@
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

**Commit message,** for when the tests are settled:

```
Compute bigfloat square roots exactly and faster

Bigfloat square roots came from a Newton iteration with a full
division in every step, and about one in five was off in the last
bit. Now they come from an integer square root of the mantissa and
are always correctly rounded, and sqrt() of a bigfloat at fpprec : 100
is about 4.5 times faster. This speeds up bigfloat hypergeometric and
elliptic functions.

Some tests compared bigfloat results to the last bit, or with
tolerances fitted to the old rounding, and are adjusted.

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
- **Totals.** CPU time of `run_testsuite()` from SBCL's `time`, HEAD
  against HEAD with patches 1 to 4, both pinned to the same CPU. One
  warm-up run of each build was discarded, then three rounds in the order
  AB, BA, AB. Full suite 127.6 → 111.9 s (−15.6 ± 1.0 s), core
  72.4 → 61.2 s (−11.1 ± 0.9 s), ± being the standard error of the
  per-round differences.
