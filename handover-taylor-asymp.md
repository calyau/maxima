# Handover: taylor() asymptotic expansions at a finite point

## 1. Summary

**Fix taylor() with 'asymp dropping terms at a finite point**

`taylor(expr, [x, a, n, 'asymp])` lost terms at every finite point: `taylor(x^2,[x,a,0,'asymp])` missed `a^2`, `taylor(x^2,[x,a,-2,'asymp])` gave 0 and `taylor(x^2+x+1,[x,0,0,'asymp])` gave 1. Both causes are in VAR-EXPAND: at a point other than 0 it raised a too short expansion of `x` to a power, and at the point 0 it compared the wrong degree with the truncation level. This builds on `handover-taylor-cancelling-constants.md` and `handover-taylor-nonzero-point.md`, which must be applied first.

## 2. Reproducer/Demo

### Old: master 8c68d35ac with the cancelling-constants and nonzero-point fixes (executed)

```
(%i1) display2d:false
(%i2) taylor(x^2,[x,a,-2,'asymp])
(%o2) +0
(%i3) taylor(x^2,[x,a,-1,'asymp])
(%o3) +(x-a)^2
(%i4) taylor(x^2,[x,a,0,'asymp])
(%o4) (x-a)^2+2*a*(x-a)
(%i5) taylor(x^3/(x^2-1),[x,1,0,'asymp])
(%o5) +0
(%i6) taylor(x^3/(x^2-1),[x,1,1,'asymp])
(%o6) +(x-1)
(%i7) taylor(x^2+x+1,[x,0,0,'asymp])
(%o7) +1
(%i8) taylor(x^2/(x-1),[x,0,1,'asymp])
(%o8) +x
(%i9) taylor(1/x,[x,1,2,'asymp])
(%o9) 1/(x-1)-1/(x-1)^2
```

Master 8c68d35ac without those fixes gives the same results.

### New: the same with this fix (executed)

```
(%i1) display2d:false
(%i2) taylor(x^2,[x,a,-2,'asymp])
(%o2) +(x-a)^2
(%i3) taylor(x^2,[x,a,-1,'asymp])
(%o3) (x-a)^2+2*a*(x-a)
(%i4) taylor(x^2,[x,a,0,'asymp])
(%o4) (x-a)^2+2*a*(x-a)+a^2
(%i5) taylor(x^3/(x^2-1),[x,1,0,'asymp])
(%o5) (x-1)+1
(%i6) taylor(x^3/(x^2-1),[x,1,1,'asymp])
(%o6) (x-1)+1+1/(x-1)
(%i7) taylor(x^2+x+1,[x,0,0,'asymp])
(%o7) x^2+x+1
(%i8) taylor(x^2/(x-1),[x,0,1,'asymp])
(%o8) x+1+1/x
(%i9) taylor(1/x,[x,1,2,'asymp])
(%o9) 1/(x-1)-1/(x-1)^2
```

### Internals

**What the order means.** The manual says that `taylor(expr, [x, a, n, 'asymp])` expands in negative powers of `x-a` with highest order term `(x-a)^-n`. So the expansion variable is u = 1/(x-a), the order counts powers of u, and x = a + 1/u. Then x^2 = 1/u^2 + 2*a/u + a^2, so the right answers above follow, and x^3/(x^2-1) = 1/u + 1 + u + ... gives `(%o5)` and `(%o6)`.

**At a point other than 0.** VAR-EXPAND builds x as the series u^-1 + a, truncated at the current level, and raises it to the power with PSEXPT. A power of a series of leading degree -1 is known to exp-1 degrees less than the series itself. Tracing `taylor(x^2,[x,a,0,'asymp])` shows it: PSEXPT gets `(PS ... ((0 . 1)) ((-1 . 1) 1 . 1) ((0 . 1) a . 1))` and returns a series truncated at -1, without `a^2`. At a negative level TAYLOR2 also drops the constant `a` before PSEXPT sees it. That loses `2*a*(x-a)` at order -1, and at order -2 the term `(x-a)^2` ended up beyond the truncation of the result. In `(%i5)`, TSTIMES re-expands `x^3` at level -2, so even order 0 came out as 0. Now, for an exponent above 1, VAR-EXPAND builds x with its truncation level raised by exp-1, through LET-PW and a recursive call, so that both the constant and the degrees PSEXPT loses are there. x is exact, so the extra terms are right. The multivariable branch of VAR-EXPAND compensates the same loss of PSEXPT with LET-PW.

**At the point 0.** The kernel is 1/x and x^exp has degree -exp. The zero-point branch built that term correctly but compared exp instead of -exp with the truncation level, so `x^2` was dropped at any order below 2, as in `(%o7)` and `(%o8)`. It now negates exp once at the top of the branch, as the branch for an infinite point already does.

**Unchanged.** Exponents up to 1 (PSEXPT loses nothing there, `(%o9)`), expansions at infinity (an asymptotic one still signals an error), multivariable expansions and everything without `'asymp`.

**RATDISREP.** Before ffb699c5b (bug #5834), RATDISREP of such a series at a point other than 0 could return an unsimplified sum flagged as simplified: `ratdisrep(taylor(x,[x,1,0,'asymp]))` gave `-1+x+1`. On master it gives `x`. One test below still compares through `ratsimp`, which is harmless.

**Checks.** The expected values were checked numerically, independent of `taylor()`: at x = a + 1/h, (f-S)/h^(n+1) evaluated in 80-digit bigfloats for h = 1e-6, 1e-9, 1e-12 stays bounded (it tends to the next coefficient: 6, 9, 0, 1, 1, 1, 0, 1 and 1/8, with a = 3 for the symbolic point), while for the old results 1 and `x-1` in rows 4 and 6 it grows like 1/h^2. Measured before the two earlier fixes were revised: on master 8c68d35ac with the two earlier fixes, this fix makes 65 more of the 1,122 generated expansions right, all of them asymptotic ones, and none that were right got worse.

Measured on master 8c68d35ac with the two earlier fixes as they are now: of the 4,870 generated expansions of the first handover, 870 are asymptotic ones (29 expressions at 5 points and 6 orders). With this fix 326 more of them are right, none got worse, and no other result changed. All 870 are right now. For the 21 with `z^(3/2)`, whose terms come in half-integer steps, that was checked by hand. The benchmarks in the first handover ran with all three fixes.

## 3. Bug report

**Title:**
```
taylor(x^2,[x,a,0,'asymp]) misses the term a^2
```

**Body:**
````markdown
Asymptotic expansions at a finite point lose terms. With `'asymp`, `taylor(expr, [x, a, n, 'asymp])` expands in powers of `u = 1/(x-a)` through `u^n`. Since `x = a + 1/u`, `x^2` is `(x-a)^2 + 2*a*(x-a) + a^2` with the terms of order `-2`, `-1` and `0` in `u`, but:

```
(%i1) display2d:false$
(%i2) taylor(x^2, [x, a, -2, 'asymp]);
(%o2) +0
(%i3) taylor(x^2, [x, a, -1, 'asymp]);
(%o3) +(x-a)^2
(%i4) taylor(x^2, [x, a, 0, 'asymp]);
(%o4) (x-a)^2+2*a*(x-a)
```

Rational functions are affected as well. Here `x^3/(x^2-1)` is `(x-1) + 1 + 1/(x-1) + ...` in powers of `1/(x-1)`:

```
(%i5) taylor(x^3/(x^2-1), [x, 1, 0, 'asymp]);
(%o5) +0
(%i6) taylor(x^3/(x^2-1), [x, 1, 1, 'asymp]);
(%o6) +(x-1)
```

At the point `0` a power of `x` is dropped whenever its exponent is larger than the order, although it has a negative order in `1/x`:

```
(%i7) taylor(x^2+x+1, [x, 0, 0, 'asymp]);
(%o7) +1
(%i8) taylor(x^2/(x-1), [x, 0, 1, 'asymp]);
(%o8) +x
```

The right answers are `x^2+x+1` and `x+1+1/x`.
````

## 4. Code patching instructions

### `src/hayat.lisp`: VAR-EXPAND with `'asymp` at a finite point

Rewrites the zero-point clause of VAR-EXPAND and adds a clause after it for other finite points.

**Replace** this block:

```lisp
	   ((signp e (exp-pt temp))
	    (let ((exp>trunc? () ))
	       (if (and (e> exp (current-trunc temp)) (setq exp>trunc? 't)
			(not dont-truncate?))
		   (rczero)
		  (make-ps (int-var temp)
			   (ncons (if exact-poly (inf)
				     (if exp>trunc? exp (current-trunc temp))))
			   (ncons (term (if (switch '$asymp temp) (rcminus exp)
					   exp)
					(rcone)))))))
```

**with** this block:

```lisp
	   ((signp e (exp-pt temp))
	    ;; With $ASYMP the kernel is 1/VAR2, so VAR2^EXP has degree -EXP.
	    (when (switch '$asymp temp) (setq exp (rcminus exp)))
	    (let ((exp>trunc? () ))
	       (if (and (e> exp (current-trunc temp)) (setq exp>trunc? 't)
			(not dont-truncate?))
		   (rczero)
		  (make-ps (int-var temp)
			   (ncons (if exact-poly (inf)
				     (if exp>trunc? exp (current-trunc temp))))
			   (ncons (term exp (rcone)))))))
	   ((and (switch '$asymp temp) (e> exp (rcone)))
	    ;; With $ASYMP the kernel is 1/(VAR2-EXP-PT), of degree -1, and
	    ;; PSEXPT knows a power of a series of degree -1 to EXP-1 degrees
	    ;; less than the series. VAR2 is exact, so expand it that much
	    ;; further, also keeping the constant EXP-PT at a negative level.
	    (psexpt (let-pw temp (e+ (current-trunc temp) (e1- exp))
		      (var-expand var2 1 dont-truncate?))
		    exp))
```

### `tests/rtest_taylor.mac`: new tests

Adds the tests of section 5, after those of the two earlier handovers.

**Insert** the block of section 5, followed by two blank lines, **before** this line:

```
/* clean up & checks */
```

## 5. Proposed test cases

**New**, in `tests/rtest_taylor.mac`, before its final clean-up checks and after the tests of the two earlier handovers. Replace `PPPP` by the number the bug report gets. The first pair compares through `ratsimp`, a leftover from before ffb699c5b, when RATDISREP returned `-1+x+1` for a series at the point 1 (see section 2). It is harmless.

```
/* Bug #PPPP: "taylor(x^2,[x,a,0,'asymp]) misses the term a^2" */

ratsimp(map('ratdisrep,
             [taylor(x^2, [x, a, -2, 'asymp]), taylor(x^2, [x, a, -1, 'asymp]), taylor(x^2, [x, a, 0, 'asymp]),
              taylor(x^3/(x^2-1), [x, 1, 0, 'asymp]), taylor(x^3/(x^2-1), [x, 1, 1, 'asymp])])
        - [(x-a)^2, (x-a)^2+2*a*(x-a), (x-a)^2+2*a*(x-a)+a^2, x, x+1/(x-1)]);
[0, 0, 0, 0, 0]$

map('ratdisrep,
    [taylor(x^2+x+1, [x, 0, -1, 'asymp]), taylor(x^2+x+1, [x, 0, 0, 'asymp]),
     taylor(x^2/(x-1), [x, 0, 1, 'asymp]), taylor(sqrt(x^2+1), [x, 0, 2, 'asymp])]);
[x^2+x, x^2+x+1, x+1+1/x, x+1/(2*x)]$
```

`run_testsuite(share_tests=true)` on SBCL 2.2.9, master 8c68d35ac with all three fixes: 1 failure out of 22,397 tests (`rtestprintf.mac` problem 38, the same float printing failure as on master). `rtest_taylor` passes 272/272, and its 2 new problems both fail without this patch.

## 6. Proposed Git commit message

```
Fix taylor() 'asymp expansions at a finite point

taylor() with 'asymp lost terms at a finite point: for example
taylor(x^2,[x,a,0,'asymp]) missed a^2, taylor(x^2,[x,a,-2,'asymp])
gave 0, and taylor(x^2+x+1,[x,0,0,'asymp]) gave 1 instead of
x^2+x+1.

At a point other than 0, taylor() raised too short an expansion of x
to a power, which loses terms when the expansion is in powers of
1/(x-a). At the point 0 it dropped a power of x whenever the exponent
was larger than the order, although such a power has a negative order
in 1/x. Both are fixed.

This fixes bug #PPPP.

AI-Assisted-By: Claude Opus 5.5
```
