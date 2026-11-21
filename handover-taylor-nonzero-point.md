# Handover: taylor() at a point other than 0 to a negative order

## 1. Summary

**Fix taylor() losing terms at a nonzero point and a negative order**

`taylor()` to a negative order at a point other than 0 lost terms, often all of them, as in `taylor(s/(s-1),s,1,-1)`, which gave 0 instead of `1/(s-1)`: at a negative truncation level VAR-EXPAND drops the constant `a` from `s = a+(s-a)` and returns `s-a` alone, so TSTIMES takes `s` for a quantity of order 1 and expands the other factors too short. VAR-EXPAND now returns 0 there for the main variable, the whole of `s` being beyond the truncation, and TSLOG no longer takes the logarithm of a series that lies beyond the truncation, which turned `log(4+s)` into `log(s-1)`. This builds on `handover-taylor-cancelling-constants.md`, which must be applied first, and like it leaves expansions with a kernel such as `%e^-(1/z^2)` or `log(z)` as they are on master.

## 2. Reproducer/Demo

### Old: master 8c68d35ac with the cancelling-constants fix (executed)

```
(%i1) display2d:false
(%i2) taylor(s/(s-1),s,1,-1)
(%o2) +0
(%i3) taylor(s/(s^2-a^2),s,a,-1)
(%o3) +0
(%i4) taylor((s*exp(s))/(s-1)^2,s,1,-1)
(%o4) +%e/(s-1)^2
(%i5) taylor(s/(s^2+1),s,%i,-1)
(%o5) +0
(%i6) taylor(s/(s-1),[s],1,-1)
(%o6) +0
(%i7) taylor((x*y)/((x-1)*(y-2)),[x,y],[1,2],-2)
(%o7) +0
(%i8) taylor(s/(s-1),s,1,0)
(%o8) 1/(s-1)+1
(%i9) ev(taylor(s,s,a,-1),taylor_truncate_polynomials:false)
(%o9) +(s-a)
(%i10) taylor(z/((z-1)*(w-1)),z,1,-1,w,1,1)
(%o10) +0
(%i11) taylor(5+1/(w-1),z,1,0,w,1,-1)
(%o11) +1/(w-1)
(%i12) taylor(z/((z+sqrt(2))*(z-sqrt(2))+2)+z^2*(1+z),z,0,2)
(%o12) +1/z
(%i13) taylor(log(4+w),z,1,0,w,2,-1)
(%o13) +log(w-2)
(%i14) taylor((z^2*w*log(2+w))/((w+3)*(w-2)*sin(z-2)),z,2,-1,w,2,-2)
(%o14) +0
(%i15) taylor(((log(4+z)/(z-1))*(w+2))/(w-2),z,1,-1,w,2,-1)
taylor: taylor: Unhandled multivar datum comparison.
log(z+4)
 -- an error. To debug this try: debugmode(true);
(%i16) taylor(((1-cos(z))/z)/(2+sin(exp(-1/z^2))),z,0,0)
(%o16) +0
```

Master 8c68d35ac without that fix gives the same results.

### New: the same with this fix (executed)

```
(%i1) display2d:false
(%i2) taylor(s/(s-1),s,1,-1)
(%o2) +1/(s-1)
(%i3) taylor(s/(s^2-a^2),s,a,-1)
(%o3) +1/(2*(s-a))
(%i4) taylor((s*exp(s))/(s-1)^2,s,1,-1)
(%o4) %e/(s-1)^2+(2*%e)/(s-1)
(%i5) taylor(s/(s^2+1),s,%i,-1)
(%o5) +1/(2*(s-%i))
(%i6) taylor(s/(s-1),[s],1,-1)
(%o6) +1/(s-1)
(%i7) taylor((x*y)/((x-1)*(y-2)),[x,y],[1,2],-2)
(%o7) +2/((y-2)*(x-1))
(%i8) taylor(s/(s-1),s,1,0)
(%o8) 1/(s-1)+1
(%i9) ev(taylor(s,s,a,-1),taylor_truncate_polynomials:false)
(%o9) +0
(%i10) taylor(z/((z-1)*(w-1)),z,1,-1,w,1,1)
(%o10) +(+1/(w-1))/(z-1)
(%i11) taylor(5+1/(w-1),z,1,0,w,1,-1)
(%o11) +1/(w-1)
(%i12) taylor(z/((z+sqrt(2))*(z-sqrt(2))+2)+z^2*(1+z),z,0,2)
(%o12) 1/z+z^2
(%i13) taylor(log(4+w),z,1,0,w,2,-1)
(%o13) +0
(%i14) taylor((z^2*w*log(2+w))/((w+3)*(w-2)*sin(z-2)),z,2,-1,w,2,-2)
(%o14) +0
(%i15) taylor(((log(4+z)/(z-1))*(w+2))/(w-2),z,1,-1,w,2,-1)
(%o15) +(+(4*log(5))/(w-2))/(z-1)
(%i16) taylor(((1-cos(z))/z)/(2+sin(exp(-1/z^2))),z,0,0)
(%o16) +0
```

`(%i11)`, `(%i14)` and `(%i16)` are right on master and have to stay right (see "Only at the main level", "Logarithms" and "Kernels" below).

### Internals

**Where it goes wrong.** For `taylor(s/(s^2-a^2),s,a,-1)`, TSTIMES expands each factor at truncation -1. VAR-EXPAND builds `s` as `s-a` plus TAYLOR2 of the point `a`, and TAYLOR2 drops that constant at a negative truncation level, so `s` comes back as `(PS ... ((-1 . 1)) ((1 . 1) 1 . 1))`: `s-a` alone, a series of order 1. With orders 1 and -1, TSTIMES re-expands the reciprocal at truncation -1 + (-1) - 0 = -2, where it is 0 because its true order is -1, and the product is 0. With the true order 0 for `s`, the reciprocal is re-expanded at -1 and the product comes out right.

**The fix.** `(s-a+a)^exp` starts with the constant `a^exp`, so a negative truncation level leaves nothing of it, and VAR-EXPAND now returns 0. TSTIMES then finds the true order of `s` through GET-LEXP, as for any factor that vanishes at the truncation level. This agrees with the zero-point branch of VAR-EXPAND, which returns 0 for a power beyond the truncation, and with the TAYLOR2 rule of the first handover. The new clause comes before the multivariable branch, which builds a variable as the point plus a multiple of the label variable and lost the point the same way (`(%i6)` and `(%i7)` above).

**Only at the main level.** The clause applies at the main truncation level only, the level of MAINVAR-DATUM: to the first variable given or, when a list such as `[x,y]` comes first, to the variables of that list, which share one level. That is the level at which TAYLOR2 drops constants, so the constants around `s` are gone when `s` is. A variable given separately after the first one, such as `w` in `(%i11)`, has a truncation level of its own. While the main level is not negative a constant survives there, and a 0 for `w` would combine with it and turn `w-1` into `-1`. With the clause applied at each variable's own level, `(%i11)` gave 4, `taylor(1/((z-1)*(w-1)),z,1,-1,w,1,-1)` gave `-1/(z-1)`, and 104 of 1,500 generated expansions in two separately given variables that are right on master came out wrong. So such a variable is left as it is on master. Where the main level is negative as well, the first handover already returns 0 for an expression that has only such variables, which is why `(%i15)` gets the factor `(w+2)/(w-2)` right.

**TSEXPT1.** When its base expands to 0, TSEXPT1 cannot tell the base's variable and takes the truncation level PW from the first datum of TLIST. When the base first looks like a nonzero constant, PW is 0. TSEXPT1 then finds the base's leading term with GET-LEXP but kept that guess. This mattered little while a base seldom expanded to 0. Now a base that contains the main variable does so at every negative level, and with a second variable the guess is that variable's level: in `taylor(1/(z-1)^3+log(1+(w-2))/(w-2)^2,z,1,-3,w,2,2)` the first term came back truncated at 2, the level of `w`, and the sum gained a wrong term `log(w-2)/(w-2)^2`. TSEXPT1 now takes PW from the variable of the series GET-LEXP returns, as it does when the base does not vanish. This also corrects `(%i12)`, where the base looks like the constant `2-sqrt(2)^2` at first and PW was 0.

**Logarithms.** A variable given after the first one keeps, at a negative level of its own, the series master gives it: `w-2`, whose only term lies beyond the truncation. Sums and products do no harm with it. TSLOG took its logarithm, so `log(4+w)` at `w = 2` became `log(w-2)`, a singularity that is not there (`(%o13)`). On master the main variable went the same way, and that is where `(%i15)` stops there. TSLOG now treats an argument without a term within the truncation like one that expands to 0, and gets its leading term from GET-LEXP. When the logarithm of that series comes out constant, the constant is the term of degree 0 of a series in the argument's variable. TSLOG drops it if the level of that variable is negative, as TAYLOR2 does at the main level. This is needed for the other changes here: without it `(%i14)`, which is right on master, stopped with an error. With the orders right, TSTIMES re-expands `log(2+w)` at a negative level of `w`, and the spurious `log(w-2)` upset its count of truncation levels.

**Kernels.** As in the first handover, none of the changes here applies to an expansion with a kernel such as `%e^-(1/z^2)` or `log(z)`. Each asks NO-KERNELS? or CONSTANTS-BEYOND-TRUNC? of that handover and records with LEVEL-RULE-USED that it has changed a result, so that TAYLOR1 expands once more without the changes when a kernel turns up later. The TSEXPT1 change needs that. For the base `2+sin(exp(-1/z^2))` in `(%i16)`, GET-LEXP returns a series in the kernel, whose level is not negative while that of `z` is. With PW taken from the kernel, TSEXPT1 would no longer return 0. It would expand the base again at the negative level of `z`, get 0 and divide by it, and `(%i16)` and `tlimit((sqrt(x+1)-sqrt(x))/(1+(exp(-x)*x^10)^2)+x*exp(-x),x,inf)`, both 0 on master, would stop with "quotient by zero". The VAR-EXPAND and TSLOG changes are off with a kernel for the reason given in the first handover: the levels they rely on mean something else there. So with a kernel the bug of this handover stays as on master: `taylor(z*log((z-1)^2)/(z-1),z,1,-1)` is still 0 instead of `2*log(z-1)/(z-1)`.

**What it leaves alone.** Asymptotic expansions (`asymp`) are excluded: there the expansion is in powers of `1/(s-a)`, and losing the constant is a different defect, fixed separately (see `handover-taylor-asymp.md`). Kernels adjoined during an expansion are not the main variable, so they are left alone too. Their datum's EXP-PT holds a limit such as `$zeroa` instead of a point, and returning 0 for them broke `rtest_taylor` problem 226 (bug #4806), an expansion at `inf` that adjoins such kernels.

**Side effect.** With `taylor_truncate_polynomials:false`, `taylor(s,s,a,-1)` gave `s-a`, which is not `s` in any sense, and now gives 0 (`(%i9)`). That is what the zero-point branch already gives for a power beyond the truncation with exact polynomials (`taylor(x^5,x,0,3)` is 0 there too). There is no test for it, since the manual does not say what an exact polynomial gives at a negative order.

**residue().** RESM1 in `src/residu.lisp` expands to order 1 because, its comment says, `residue(s/(s^2-a^2),s,a)` fails at order -1. That was this bug. RESM1 is unchanged here and should stay at order 1. With all three fixes, order -1 gives the same results as order 1 for 900 generated residues, but of 1,200 generated residues at algebraic poles it turns 87 errors and 6 right results into wrong results.

**Not covered.**

- `taylor((z+w)/(z-1)^2,[z,w],[1,0],-1)` still gives `1/(z-1)^2` instead of `1/(z-1)^2+1/(z-1)+w/(z-1)^2`, as in 5.47.0, although `z/(z-1)^2` and `w/(z-1)^2` on their own now come out right.
- A variable given separately after the first one is not fixed where the order of the first one is not negative: `taylor(w/(w-1),z,1,0,w,1,-1)` and `taylor(y*s/(s-1),y,0,1,s,1,-1)` still give 0, and `taylor((w+3)/(w-2)^2,z,1,0,w,2,-1)` still lacks its `1/(w-2)`.
- A term without the second variable is kept whatever the order of that variable, as on master: `taylor(1/(z-1),z,1,-1,w,0,-2)` is `1/(z-1)` there and here. `taylor(z/(z-1),z,1,-1,w,0,-2)` was 0 through this bug and is `1/(z-1)` now. Read strictly, an order of -2 in `w` leaves nothing of either. The counts are under "Checks".
- Expanding an existing series again to a negative order still gives 0, as in `taylor(taylor(1/(s-1)^2+1/(s-1),s,1,2),s,1,-1)`. That is a separate defect in RE-TAYLOR-RECURSE, on master too (bug #5847).
- `taylor(cos(z+w),z,1,0,w,2,-1)` overflows the control stack, on master too.
- `taylor(log(5+z+w)/(z-1)^2,z,1,-1,w,2,-2)` still stops with "Unhandled multivar datum comparison", as on master. On the way taylor adjoins `log(w-2)` as a kernel, and an expansion with a kernel is done as on master.

**Checks.** The expected values were checked numerically, independent of `taylor()`: f-S evaluated in 80-digit bigfloats at s = p+h for h = 1e-6, 1e-9, 1e-12 stays bounded (1, 1/12 for a = 3, 3*%e/2 and 1/4 in absolute value), while for the old answer 0 it grows like 1/h. The two-variable and the separate-order cases are exact identities, checked with `ratsimp`. `(%o15)` is the product of the leading terms `log(5)/(z-1)` and `4/(w-2)`.

Measured with the clause at the variable's own level and without the TSEXPT1 change: on master 8c68d35ac with the cancelling-constants fix, this fix makes 63 more of the 1,122 generated expansions right (58 at points other than 0, 5 in two variables), and none that were right got worse. The other corpora named in that handover are unchanged.

Measured with the code as it is here, on master 8c68d35ac with the cancelling-constants fix, against results that do not come from the tested build:

- Of the 4,870 generated single-variable expansions of that handover, judged numerically, 130 more are right, 127 that were wrong and 3 that stopped with an error, and none got worse.
- Of 1,000 expansions in a list of two variables, checked against a single-variable expansion along `z = a+t*(z-a)`, `w = b+t*(w-b)`, 114 more are right and none got worse.
- Of 2,700 expansions in two separately given variables (1,200 of them with logarithms and mostly negative orders), checked against iterated single-variable expansions, 217 more are right, 64 that were wrong and 153 that stopped with an error. No result that was right became an error. 24 right results, 23 of them 0, and 7 errors became results with a term without `w` although the order of `w` is negative, as in the third point of "Not covered". Master returns each of these 31 series unchanged when given it explicitly, so they follow master's own rule. 18 wrong results or errors changed without becoming right. No other result that was right changed.
- Of the 4,000 generated expansions of special and unknown functions of that handover, 1,000 are at the point 1. 189 more are right, and no result that was right changed. Among the rest are 28 with unknown functions or sums that cannot be judged numerically. Those checked by hand are right now.
- The 2,100 generated expansions with constant arguments and with `f(c+s)-f(c)` denominators of that handover are unchanged, and so are the 900 generated residues and the 1,200 at algebraic poles.
- The 17,223 expansions with kernels, the 6,048 that combine kernels with quotients, the 585 at `inf` and `minf`, the 1,975 limits with kernels and the 1,492 ordinary limits of that handover are unchanged. With the TSEXPT1 change applied regardless of kernels, 284 of the expansions with a flat kernel stop with "quotient by zero". Master returns 0 for them, which is right for some, like `(%i16)`, and wrong for most (bug #5858).

## 3. Bug report

**Title:**
```
taylor(s/(s-1),s,1,-1) gives 0 instead of 1/(s-1)
```

**Body:**
````markdown
`taylor` to a negative order at an expansion point other than `0` loses terms, often all of them:

```
(%i1) display2d:false$
(%i2) taylor(s/(s-1), s, 1, -1);
(%o2) +0
(%i3) taylor(s/(s-1), s, 1, 0);
(%o3) 1/(s-1)+1
(%i4) taylor(s/(s^2-a^2), s, a, -1);
(%o4) +0
(%i5) taylor(s*exp(s)/(s-1)^2, s, 1, -1);
(%o5) +%e/(s-1)^2
(%i6) taylor(s/(s-1), [s], 1, -1);
(%o6) +0
```

Since `s/(s-1)` is `1/(s-1)+1`, the expansion to order `-1` must be `1/(s-1)`, and the expansion to order `0` shows that `taylor` knows it. Likewise `s/(s^2-a^2)` has the pole term `1/(2*(s-a))`, and `s*exp(s)/(s-1)^2` is `%e/(s-1)^2+2*%e/(s-1)+...`, so the term `2*%e/(s-1)` is missing. The list form of the variable fails the same way.

With a second variable, a logarithm can come out with a singularity it does not have, or `taylor` stops:

```
(%i7) taylor(log(4+w), z, 1, 0, w, 2, -1);
(%o7) +log(w-2)
(%i8) taylor(log(4+z)/(z-1)*(w+2)/(w-2), z, 1, -1, w, 2, -1);
taylor: taylor: Unhandled multivar datum comparison.
log(z+4)
```

`log(4+w)` is regular at `w = 2`, so the first answer is `0`, and the second is `4*log(5)/((z-1)*(w-2))`.

The source of `residue` works around this by expanding to order `1`: a comment there says that `residue(s/(s^2-a^2),s,a)` fails at order `-1`.
````

## 4. Code patching instructions

### `src/hayat.lisp`: VAR-EXPAND returns 0 where constants lie beyond the main truncation

Adds a clause to the COND of VAR-EXPAND, before its multivariable branch.

**Insert** this block:

```lisp
	   ((and (not (signp e (exp-pt temp))) (null (switch '$asymp temp))
		 (not dont-truncate?) (constants-beyond-trunc?)
		 (eq mainvar-datum
		     (if (switch 'multi temp)
		       (get-datum (car (switch 'multi temp)))
		       temp)))
	    ;; Expanded at a point other than 0, VAR2^EXP starts with the
	    ;; constant EXP-PT^EXP, so nothing of it is left where constants
	    ;; lie beyond the truncation. The code below would drop only that
	    ;; constant and return a power of VAR2-EXP-PT, and TSTIMES would
	    ;; take its order for the order of VAR2^EXP, which is 0. This is
	    ;; done only where VAR2 is truncated at the main level, the one
	    ;; at which TAYLOR2 drops constants. At the level of another
	    ;; variable a constant can survive, and a 0 for VAR2 would turn
	    ;; VAR2-1 into -1.
	    (level-rule-used)
	    (rczero))
```

**before** this line (it is indented with a tab and three spaces):

```lisp
	   ;; multivar expansion does not work at infinity, so
```

### `src/hayat.lisp`: TSEXPT1 takes the truncation level from the base's variable

Sets PW again once GET-LEXP has found the base's series.

**Replace** this line (it is indented with a tab and six spaces):

```lisp
	      (setq tb (get-lexp b () 't)))
```

**with** this block:

```lisp
	      (setq tb (get-lexp b () 't))
	      ;; PW was a guess as long as TB had no variable. With kernels
	      ;; it stays one: TB can be a series in a kernel whose level
	      ;; is not negative while the main level is.
	      (when (and (psp tb) (no-kernels?))
		(let ((pw-tb (t-o-var (gvar tb))))
		  (unless (e= pw pw-tb)
		    (level-rule-used)
		    (setq pw pw-tb)))))
```

### `src/hayat.lisp`: TSLOG looks for the leading term of an argument beyond the truncation

Treats such an argument like one that expands to 0.

**Replace** this line (it is indented with three spaces):

```lisp
   (when (rczerop psarg) (setq psarg (get-lexp arg () 't)))
```

**with** this block:

```lisp
   ;; PSARG can lie beyond the truncation altogether: a variable other than
   ;; the main one, expanded at a point other than 0 to a negative level,
   ;; comes from VAR-EXPAND as the series w-2, say, whose only term lies
   ;; beyond that level. Taken as it is, log(w+2) at w = 2 would become
   ;; log(w-2). With kernels such a series is kept.
   (when (or (rczerop psarg)
	     (and (no-kernels?) (rczerop (pstrunc psarg)) (level-rule-used)))
     (setq psarg (get-lexp arg () 't)))
```

### `src/hayat.lisp`: TSLOG drops a constant logarithm at a negative level

Applies to the logarithm of a series, at a negative level of the variable of that series.

**Replace** this line (it is indented with a tab):

```lisp
	(pslog psarg))
```

**with** this block:

```lisp
	;; A log that comes out constant is the constant term of a series in
	;; the variable of PSARG, so a negative level of that variable
	;; drops it, unless there are kernels.
	(let ((ans (pslog psarg)))
	  (if (and (pscoefp ans) (not (rczerop ans)) (psp psarg) (no-kernels?)
		   (e> (rczero) (t-o-var (gvar psarg))) (level-rule-used))
	    (rczero)
	    ans)))
```

### `tests/rtest_taylor.mac`: new tests

Adds the tests of section 5, after those of the first handover.

**Insert** the block of section 5, followed by two blank lines, **before** this line:

```
/* clean up & checks */
```

## 5. Proposed test cases

**New**, in `tests/rtest_taylor.mac`, before its final clean-up checks and after the tests of the first handover. Replace `MMMM` by the number the bug report gets. In the third pair the first two expansions are right on master and guard the variable given second, the fourth pair covers TSEXPT1 and the fifth TSLOG, where the third expansion is right on master. The last pair is right on master and guards expansions with a kernel.

```
/* Bug #MMMM: "taylor(s/(s-1),s,1,-1) gives 0 instead of 1/(s-1)" */

map('ratdisrep,
    [taylor(s/(s-1), s, 1, -1), taylor(s/(s^2-a^2), s, a, -1),
     taylor(s*exp(s)/(s-1)^2, s, 1, -1), taylor(s/(s^2+1), s, %i, -1),
     taylor(s/(s-1), [s], 1, -1), taylor(y*s/(s-1), s, 1, -1, y, 0, 1)]);
[1/(s-1), 1/(2*(s-a)), %e/(s-1)^2+2*%e/(s-1), 1/(2*(s-%i)), 1/(s-1), y/(s-1)]$

ratsimp(ratdisrep(taylor(x*y/((x-1)*(y-2)), [x, y], [1, 2], -1)) - (2/((x-1)*(y-2))+1/(x-1)+2/(y-2)));
0$

ratsimp(map('ratdisrep,
            [taylor(1/((z-1)*(w-1)), z, 1, -1, w, 1, -1), taylor(5+1/(w-1), z, 1, 0, w, 1, -1),
             taylor(z/((z-1)*(w-1)), z, 1, -1, w, 1, 1)])
        - [1/((z-1)*(w-1)), 1/(w-1), 1/((z-1)*(w-1))]);
[0, 0, 0]$

ratdisrep(taylor(z/((z+sqrt(2))*(z-sqrt(2))+2)+z^2*(1+z), z, 0, 2));
1/z+z^2$

ratsimp(map('ratdisrep,
            [taylor(log(4+z)/(z-1)*(w+2)/(w-2), z, 1, -1, w, 2, -1),
             taylor(log(z+4)*log(w+4)/((z-1)*(w-2)), z, 1, -1, w, 2, -1),
             taylor(z^2*w*log(2+w)/((w+3)*(w-2)*sin(z-2)), z, 2, -1, w, 2, -2),
             taylor(log(4+w), z, 1, 0, w, 2, -1)])
        - [4*log(5)/((z-1)*(w-2)), log(5)*log(6)/((z-1)*(w-2)), 0, 0]);
[0, 0, 0, 0]$

[ratdisrep(taylor(((1-cos(z))/z)/(2+sin(exp(-1/z^2))), z, 0, 0)),
 tlimit((sqrt(x+1)-sqrt(x))/(1+(exp(-x)*x^10)^2)+x*exp(-x), x, inf)];
[0, 0]$
```

`run_testsuite(share_tests=true)` on SBCL 2.2.9, master 8c68d35ac with the cancelling-constants fix and this one: 1 failure out of 22,395 tests (`rtestprintf.mac` problem 38, the same float printing failure as on master). `rtest_taylor` passes 270/270. Of its 6 new problems, the first 5 fail without this patch and the last one passes.

## 6. Proposed Git commit message

```
Fix taylor() at nonzero points to negative orders

taylor() lost terms, often all of them, when it expanded to a negative
order at a point other than 0: taylor(s/(s-1),s,1,-1) gave 0 instead
of 1/(s-1), and taylor(s*exp(s)/(s-1)^2,s,1,-1) lost its 1/(s-1)
term. Expansions in several variables, as in taylor(s/(s-1),[s],1,-1),
went wrong the same way.

At a negative order taylor() drops constant terms. Near a point a, s
is a+(s-a), and dropping a left s-a, so taylor() took s for a
quantity of order 1 rather than 0 and expanded the rest of the
expression too short. Now the whole of s is dropped there, as a is,
and taylor() finds its true order.

This holds for the first variable and for variables given as a list.
A variable given separately after the first one has an order of its
own and is left as it was. Its logarithms are not: log(4+w) at w = 2
was expanded as log(w-2), and now it is expanded as it should be.
An expansion that involves a term like exp(-1/z^2) or log(z), which
taylor() treats as a variable of its own, is left as it was, too.

A power whose base vanishes at the current order was expanded to the
order of the first variable in taylor's list, or to order 0. Now it
gets the order of its own variable. This also restores the z^2 in
taylor(z/((z+sqrt(2))*(z-sqrt(2))+2)+z^2*(1+z),z,0,2).

This fixes bug #MMMM.

AI-Assisted-By: Claude Opus 5.5
AI-Assisted-By: Claude Fable 5.2
```
