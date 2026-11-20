# Handover: taylor() of quotients whose constants cancel

## 1. Summary

**Fix taylor() returning too short a series when a denominator's constant terms cancel**

`taylor()` gave too short a series, often 0, for quotients such as `z^2/((z+1)*(z-1)+1)`. On the way to the result, TSTIMES re-expands the reciprocal of the denominator at a negative truncation level, where TAYLOR2 drops the constant `1` but the product `(z+1)*(z-1)` keeps its constant `-1` (and a logarithm, exponential or cosine can keep one too), so the constants stop cancelling. TAYLOR2 now drops a constant result at a negative truncation level, as it already drops a constant input, and TRY-EXPANSION no longer takes a function at 0 there when its argument expands to a constant.

## 2. Reproducer/Demo

### Old: master 8c68d35ac (executed)

```
(%i1) display2d:false
(%i2) taylor(z^2/((z+1)*(z-1)+1),z,0,1)
(%o2) +0
(%i3) C:(z+1)*(z-1)+1
(%i4) taylor(sin(C)/C,z,0,1)
(%o4) +0
(%i5) taylor(log(1+C)/C,z,0,1)
(%o5) +0
(%i6) taylor(z^2/((z+a)*(z-a)+a^2),z,0,1)
(%o6) +0
(%i7) taylor(1/C,z,0,-2)
(%o7) +0
(%i8) taylor(z/(log(2+z)-log(2)),z,0,0)
(%o8) +0
(%i9) residue((z^2*(1+z))/(z^3-z^2+(z-1)*(z+1)+1),z,0)
(%o9) 0
(%i10) taylor(cot((z+1)*(z-1))/z^3,z,0,-1)
(%o10) -1/(tan(1)*z^3)-(tan(1)^2+1)/(tan(1)^2*z)
(%i11) taylor(cot(z^2-1)/z^3,z,0,-1)
cot: argument 0 isn't in the domain of cot.
 -- an error. To debug this try: debugmode(true);
```

### New: master 8c68d35ac with this fix (executed)

```
(%i1) display2d:false
(%i2) taylor(z^2/((z+1)*(z-1)+1),z,0,1)
(%o2) +1
(%i3) C:(z+1)*(z-1)+1
(%i4) taylor(sin(C)/C,z,0,1)
(%o4) +1
(%i5) taylor(log(1+C)/C,z,0,1)
(%o5) +1
(%i6) taylor(z^2/((z+a)*(z-a)+a^2),z,0,1)
(%o6) +1
(%i7) taylor(1/C,z,0,-2)
(%o7) +1/z^2
(%i8) taylor(z/(log(2+z)-log(2)),z,0,0)
(%o8) +2
(%i9) residue((z^2*(1+z))/(z^3-z^2+(z-1)*(z+1)+1),z,0)
(%o9) 1
(%i10) taylor(cot((z+1)*(z-1))/z^3,z,0,-1)
(%o10) -1/(tan(1)*z^3)-(tan(1)^2+1)/(tan(1)^2*z)
(%i11) taylor(cot(z^2-1)/z^3,z,0,-1)
(%o11) -1/(tan(1)*z^3)-(tan(1)^2+1)/(tan(1)^2*z)
```

`(%i10)` is right on master and has to stay right. `(%i11)` is the same function with its constant written as a sum (see "Constant arguments in TRY-EXPANSION" below).

### Inputs whose cancellation needs `%i^2 = -1` (executed, each input in its own session)

Setup in every session:

```
(%i1) display2d:false
(%i2) A:(z+%i)*(z-%i)-z^2-1+z^2
(%i3) A1:(1+%i*z)*(1-%i*z)-1-z^2+z^3
```

On master as it is, both inputs fail before this bug matters, with and without this fix. On master 8c68d35ac the first one kills SBCL, in three runs out of three:

```
(%i4) taylor(tan(A)/A,z,0,1)
fatal error encountered in SBCL pid 8314 tid 8314:
Control stack exhausted while pseudo-atomic, fault: 0x7f419ab8ffb8, PC: 0x52a0045f
```

With this fix it overflows the control stack as well, here as a Lisp error that Maxima survives:

```
(%i4) taylor(tan(A)/A,z,0,1)
Maxima encountered a Lisp error:

 Control stack exhausted (no more space for function call frames).
```

The second input gives the same Lisp error with and without this fix:

```
(%i4) taylor(log(1+A1)/A1,z,0,1)
Maxima encountered a Lisp error:

 The value
   1
 is not of type
   LIST
```

Old: master 8c68d35ac with an unconditional `%i` reduction emulated (see Internals), executed:

```
(%i4) taylor(tan(A)/A,z,0,1)
(%o4) +0
```

```
(%i4) taylor(log(1+A1)/A1,z,0,1)
(%o4) +0
```

New: the same with this fix, executed:

```
(%i4) taylor(tan(A)/A,z,0,1)
(%o4) +1
```

```
(%i4) taylor(log(1+A1)/A1,z,0,1)
(%o4) +1
```

### Internals

**Where it goes wrong.** For `taylor(z^2/C,z,0,1)`, TSTIMES first expands each factor at truncation 1 and gets the orders right: 2 for `z^2`, -2 for `1/C`. So the denominator's leading order is not misjudged and the numerator is expanded far enough. TSTIMES then re-expands each factor to the truncation the product needs, which for `1/C` is 1 + (-2) - 0 = -1. At that level TAYLOR2 drops the constant `1` of `C` as a term of degree 0. The product `(z+1)*(z-1)` goes another way. Its factors vanish at truncation -1, so TSTIMES expands them with GET-LEXP to truncation 1 to learn their orders and multiplies those, getting `-1+0*z`. PSCHECK turns that series into the bare constant `-1`, and the final PSTRUNC cannot truncate a constant. So `C` comes out as `-1`, TSEXPT1 sees a nonzero constant base and returns `(-1)^-1 = -1`, and `z^2*(-1)` truncated to order 1 is 0. The collapse to a constant is the trigger: `taylor(1/((z+1)*(z-2)+2+z),z,0,-2)`, also `1/z^2`, keeps a `z` term in the product and was already right.

**Why the fix is in TAYLOR2.** TAYLOR* binds `maxtayorder` to false because constants carry no truncation level, so every series has to stay truncated to the current level before a constant is added to it. Constants themselves are the hole in that rule. Logging every nonzero constant that TAYLOR2 returns while the truncation is negative (SBCL `encapsulate`, no source change) found four producers: TSTIMES as above, TSLOG (`log(2+z)` gives `log(2)`), TRY-EXPANSION (`cos(1+z^5)` gives even `cos(0) = 1`, from its constant-argument branch) and TSEXPT-RED (`exp(1+z)` gives `%e`). Each gives a wrong series once a sum is involved, as in `(%i8)` above, `taylor(1/(exp(1+z)-%e),z,0,-1)` and `taylor(1/(cos(1+z^5)-cos(1)),z,0,-4)`. Dropping constant results in TAYLOR2 covers them all, and matches what TAYLOR2 already does for a constant input and what TAYLOR_SIMPLIFY_RECURSE already does for a constant at the top level. In the whole suite such constants turn up only in `rtestint` (one expansion at `x = 3*%i*%pi/2`) and `rtest_orthopoly` (limits at infinity), and both files pass unchanged.

**Constant arguments in TRY-EXPANSION.** When GET-LEXP finds that a function's argument expands to a constant, TRY-EXPANSION expanded the argument once more with TAYLOR2 at the current truncation level and took the function at the result. At a negative level that result is 0 whenever TAYLOR2 drops the constant. For a sum like `z^2-1` it does so on master, which is why `(%i11)` stops with an error about `cot(0)` there. For a product like `(z+1)*(z-1)` it does so only with the TAYLOR2 change, and with that change alone `(%i10)` stopped with the same error, as did `csc`, `coth`, `gamma` and `psi[0]` of such a product: 18 of 4,870 generated expansions that are right on master. TRY-EXPANSION now keeps the constant GET-LEXP found when the truncation level is negative, so the function is taken at that constant. That keeps `(%i10)` right and repairs `(%i11)`.

**The `%i` inputs.** TAYLOR1 runs RATSETUP before ORDERPOINTER creates the gensyms, so the gensym of `%i` never gets the TELLRAT property and `%i^2` stays unreduced in taylor's CRE arithmetic. For `tan(A)/A`, TRIG-CONST splits off the constant term `-%i^2-1`, which is nonzero as a CRE, but `A` minus that constant simplifies back to `A`, so PSTANA+B expands `tan(A)` again without end. I emulated an unconditional `%i` reduction with a variant TAYLOR1 that, right after `(orderpointer varlist)`, gives that gensym the property as RATSETUP2 would:

```lisp
(mapl #'(lambda (v lg)
          (when (eq (car v) '$%i)
            (let ((p (algpget '$%i)))
              (algordset p lg)
              (putprop (car lg) p 'tellrat))))
      varlist genvar)
```

With it, both inputs reach this bug unpatched (0) and give 1 patched. Their tests (section 5) go in only together with an unconditional `%i` reduction like this one. The pending H29 fix as handed over is not enough: it reduces `%i` only inside the Gruntz series, through `*taylor-algebraic-%i*`, so with it `taylor(tan(u)/u,z,0,1)` still overflows the control stack at user level.

**Residues.** Residues of rational functions with an expanded denominator at an algebraic pole are unchanged and correct in all four configurations (unpatched and patched, each with and without the emulation), for example `residue(1/(z^2-2)^2,z,sqrt(2))` = `-1/2^(7/2)`, `residue(1/(z^4+4),z,1+%i)` = `-(%i+1)/16` and `residue((z-%i)^2/(z^6+3*z^4+3*z^2+1),z,%i)` = `%i/8`. `residue()` expands to order 1, which keeps most residues away from negative truncation levels. `(%i9)` above is one that is not.

**Not covered.** `taylor(s/(s^2-a^2),s,a,-1)` still gives 0 instead of `1/(2*(s-a))`, unpatched and patched. That is a different defect. At a negative truncation level VAR-EXPAND returns `s` at the point `a` as the series `s-a` alone, with the constant `a` dropped, so TSTIMES takes order 1 for `s`. It is why RESM1 in `src/residu.lisp` expands to order 1, as its comment says. It is fixed separately, see `handover-taylor-nonzero-point.md`.

**A known change for `sec(c+s)-sec(c)`.** A quotient with such a denominator, or the same with `csc`, `cot`, `sech`, `csch` or `coth`, can now stop with "quotient by zero" at a negative order, as it already does on master at orders 0 and up. `taylor((1+z)/(sec(2+z^2)-sec(2)),z,0,-3)` is 0 on master, which is right because the series starts at order -2, and is an error now. At order -2 master's 0 is wrong, and at order 0 master stops with the same error. The cause is older and separate: taylor expands these functions through their reciprocals and does not see that `1/cos(2)-sec(2)` is zero, so `taylor(sec(2+z^2)-sec(2),z,0,2)` keeps the constant `-((cos(2)*sec(2)-1)/cos(2))` on master. At a negative order master took `sec` at 0 instead and so never met that constant. Of the 4,870 generated expansions below, this turns 10 right results and 6 wrong ones into errors. They are the only results that got worse.

**Checks.** Every expected value was checked numerically, independent of `taylor()`: the unexpanded function evaluated in 80-digit bigfloats at h = 1e-6, 1e-8, 1e-10, with (f(h)-S(h))/h^(n+1) staying bounded. For the old answer 0 it grows like 1/h^2. The residue in row 9 was checked by the trapezoid rule on |z| = 1/2, giving 1.0000000000000004. A taylor-heavy benchmark run interleaved on master and on all three fixes showed no slowdown (2.46 s and 2.45 s on average).

Measured with the TAYLOR2 change alone, before the TRY-EXPANSION change: on master 8c68d35ac, this fix makes 79 more of 1,122 generated expansions right (negative orders, cancelling denominators, asymptotic expansions, residues and tlimits) and 29 more of 857 expansions with `%i`, and none that were right got worse. Corpora of 3,462 trigonometric expansions and residues at multiples of `%pi`, 2,500 general expansions, 1,500 residues and 222 definite integrals are unchanged, apart from one series at order -1 that no longer keeps a degree-0 term: `taylor(tan(z)+1/(z-%pi/2),z,3*%pi/2,-1)` is now `-1/(z-3*%pi/2)`, without the `1/%pi` it had.

Measured with both changes, on master 8c68d35ac, each result judged numerically as above: of 4,870 generated expansions (products, quotients and sums of functions with and without poles, arguments with hidden constants, 9 expansion points, orders -4 to 2, 870 of them asymptotic), 168 more are right, 83 that were wrong and 85 that stopped with an error. The 16 that got worse are those of the `sec(c+s)-sec(c)` kind above. 3 that stopped with an error give a wrong result with this fix alone and the right one with the nonzero-point fix. With the TAYLOR2 change alone, 81 more were right and 28 right ones became errors, the 10 above and the 18 that TRY-EXPANSION now handles. Of 900 generated residues, checked by a contour sum, one more is right and none got worse. Of 1,000 expansions in a list of two variables, 32 more are right and none got worse. Of 1,500 expansions in two separately given variables one changed: `taylor(1/z^3*(w^2+1)/w+1/((z+1)*(z-1)+1),z,0,-1,w,0,-1)` now has the term `1/z^2`, as master has when that term is written as `1/z^2`.

## 3. Bug report

**Title:**
```
taylor(z^2/((z+1)*(z-1)+1),z,0,1) gives 0 instead of 1
```

**Body:**
````markdown
`taylor` returns too short a series, often `0`, for a quotient whose denominator has a constant term that cancels only once its products are expanded:

```
(%i1) display2d:false$
(%i2) taylor(z^2/((z+1)*(z-1)+1), z, 0, 1);
(%o2) +0
(%i3) C : (z+1)*(z-1)+1$
(%i4) taylor(sin(C)/C, z, 0, 1);
(%o4) +0
(%i5) taylor(log(1+C)/C, z, 0, 1);
(%o5) +0
(%i6) taylor(1/C, z, 0, -2);
(%o6) +0
```

The denominator `(z+1)*(z-1)+1` is `z^2`, so the right answers are `1`, `1`, `1` and `1/z^2`. With the denominator written as `z^2`, the results are right. A logarithm or an exponential can supply the cancelling constant too:

```
(%i7) taylor(z/(log(2+z)-log(2)), z, 0, 0);
(%o7) +0
(%i8) taylor(1/(exp(1+z)-%e), z, 0, -1);
(%o8) +0
```

Here `log(2+z)-log(2)` is `z/2-z^2/8+...` and `exp(1+z)-%e` is `%e*z+...`, so the answers are `2` and `%e^-1/z`. `residue` is affected as well. The denominator below is `z^3`, so the residue is `1`, not `0`:

```
(%i9) residue(z^2*(1+z)/(z^3-z^2+(z-1)*(z+1)+1), z, 0);
(%o9) 0
```

A related failure: a function with a pole at `0` is taken at `0` when its argument has a constant term and the expansion needs a negative order on the way. `cot(z^2-1)` is `-cot(1)-csc(1)^2*z^2+...`, so the answer below is `-cot(1)/z^3-csc(1)^2/z`:

```
(%i10) taylor(cot(z^2-1)/z^3, z, 0, -1);
cot: argument 0 isn't in the domain of cot.
```

This happens in 5.47.0, 5.50.0 and on master.
````

## 4. Code patching instructions

### `src/hayat.lisp`: drop constant results in TAYLOR2

Wraps the old TAYLOR2, renamed TAYLOR2-1, with the check.

**Replace** this line (it has two spaces before `(e)`):

```lisp
(defun taylor2  (e)
```

**with** this block:

```lisp
(defun taylor2 (e)
  ;; A constant is a term of degree 0, so a negative truncation level drops
  ;; it. TAYLOR2-1 does that for a constant E, and this does it for a
  ;; constant result. Such a result can come from a series whose other terms
  ;; cancelled or lie beyond its truncation, since PSCHECK makes that series
  ;; a bare constant, which PSTRUNC cannot truncate. Kept, it would spoil
  ;; sums: expanded to degree -1, (x-1)*(x+1) gives -1 but 1 gives 0, so
  ;; 1+(x-1)*(x+1) would get the constant term -1, and TSEXPT1 would take
  ;; that for the leading term of its base.
  (let ((ans (taylor2-1 e)))
    (if (and (pscoefp ans) (not (rczerop ans))
             (e> (rczero) (current-trunc mainvar-datum)))
      (pszero (data-gvar-o mainvar-datum) (current-trunc mainvar-datum))
      ans)))

(defun taylor2-1 (e)
```

### `src/hayat.lisp`: RETURN-FROM the renamed function

Keeps the early return in the old body working after the rename.

**Replace** this line:

```lisp
            (cond ((equal (sratsimp e) (sratsimp ee)) (return-from taylor2 (prep1 ee)))
```

**with** this line:

```lisp
            (cond ((equal (sratsimp e) (sratsimp ee)) (return-from taylor2-1 (prep1 ee)))
```

### `src/hayat.lisp`: keep a constant argument in TRY-EXPANSION

Skips the second expansion of a constant argument at a negative truncation level.

**Replace** this line:

```lisp
     (when (pscoefp psarg) (setq psarg (taylor2 arg)))
```

**with** this block:

```lisp
     ;; ARG expands to the constant PSARG. Expand it again for the terms the
     ;; current truncation level adds, unless that level is negative: there
     ;; TAYLOR2 gives 0, and FUNC would be taken at 0 instead of at PSARG.
     (when (and (pscoefp psarg)
		(not (e> (rczero) (current-trunc mainvar-datum))))
       (setq psarg (taylor2 arg)))
```

### `tests/rtest_taylor.mac`: new tests

Adds the tests of section 5.

**Insert** the first block of section 5, followed by two blank lines, **before** this line:

```
/* clean up & checks */
```

## 5. Proposed test cases

**New**, in `tests/rtest_taylor.mac`, before its final clean-up checks. Replace `NNNN` by the number the bug report gets. The last pair covers TRY-EXPANSION: its first expansion is right on master, the other two stop with an error there.

```
/* Bug #NNNN: "taylor(z^2/((z+1)*(z-1)+1),z,0,1) gives 0 instead of 1" */

block([c : (z+1)*(z-1)+1],
  map('ratdisrep,
      [taylor(z^2/c, z, 0, 1), taylor(sin(c)/c, z, 0, 1),
       taylor(sin(c)/c, z, 0, 2), taylor(log(1+c)/c, z, 0, 1),
       taylor(1/c, z, 0, -2), taylor(z^2*exp(z)/c^2, z, 0, 1)]));
[1, 1, 1, 1, 1/z^2, 1/z^2+1/z+1/2+z/6]$

ratdisrep(taylor(z^2/((z+a)*(z-a)+a^2), z, 0, 1));
1$

block([d : z^3-z^2+(z-1)*(z+1)+1],
  [ratdisrep(taylor(z^4/d, z, 0, 1)), residue(z^2*(1+z)/d, z, 0)]);
[z, 1]$

map('ratdisrep,
    [taylor(z/(log(2+z)-log(2)), z, 0, 0),
     taylor(1/(exp(1+z)-%e), z, 0, -1),
     taylor(1/(cos(1+z^5)-cos(1)), z, 0, -4)]);
[2, %e^-1/z, -1/(sin(1)*z^5)]$

block([e : -1/(tan(1)*z^3)-(tan(1)^2+1)/(tan(1)^2*z)],
  ratsimp(map('ratdisrep,
              [taylor(cot((z+1)*(z-1))/z^3, z, 0, -1), taylor(cot(z^2-1)/z^3, z, 0, -1),
               taylor(gamma(z^2+2)/z^3, z, 0, -1)])
          - [e, e, 1/z^3+(1-%gamma)/z]));
[0, 0, 0]$
```

**New**, in `tests/rtest_taylor.mac`, to be appended only together with an unconditional `%i` reduction in `taylor()`, such as the emulation in section 2 (reducing every constant algebraic kernel works too). Not with the pending H29 fix as handed over: that reduces `%i` only inside the Gruntz series, so the first input still overflows the control stack at user level. On master it does too, which can kill SBCL and with it the whole test run. Run only against the emulation, where it passes, and fails without this patch.

```
block([u : (z+%i)*(z-%i)-z^2-1+z^2, v : (1+%i*z)*(1-%i*z)-1-z^2+z^3],
  map('ratdisrep,
      [taylor(tan(u)/u, z, 0, 1), taylor(tan(u)/u, z, 0, 4),
       taylor(log(1+v)/v, z, 0, 1), taylor(log(1+v)/v, z, 0, 4), taylor(1/u, z, 0, -2)]));
[1, 1+z^4/3, 1, 1-z^3/2, 1/z^2]$
```

`run_testsuite(share_tests=true)` on SBCL 2.2.9, master 8c68d35ac with this fix: 1 failure out of 22,385 tests (`rtestprintf.mac` problem 38, a float printing difference of this SBCL), the same failure as master out of 22,380. `rtest_taylor` passes 260/260, and its 5 new problems all fail on master. The second block passes on master with the emulation and this fix, and fails with the emulation alone.

## 6. Proposed Git commit message

```
Fix taylor() of quotients whose constants cancel

taylor() returned too short a series, often 0, for a quotient whose
denominator has a constant term that cancels only once its products
are expanded: taylor(z^2/((z+1)*(z-1)+1),z,0,1) gave 0 instead of 1.
The same happened when a log(), exp() or cos() term supplied the
cancelling constant, as in taylor(z/(log(2+z)-log(2)),z,0,0), and
residue() could return 0 for such a quotient.

On the way, taylor() expands the denominator to a negative order,
where constant terms are dropped. A constant that came out of a
product or a function was kept, though, so the constant terms no
longer cancelled. Now it is dropped as well.

At such an order taylor() also took a function at 0 when its
argument was a constant plus higher terms, so
taylor(cot(z^2-1)/z^3,z,0,-1) stopped with an error about cot(0).
Now it takes the function at that constant.

This fixes bug #NNNN.

AI-Assisted-By: Claude Opus 5.5
AI-Assisted-By: Claude Fable 5.2
```
