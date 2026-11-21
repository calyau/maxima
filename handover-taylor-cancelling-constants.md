# Handover: taylor() of quotients whose constants cancel

## 1. Summary

**Fix taylor() returning too short a series when a denominator's constant terms cancel**

`taylor()` gave too short a series, often 0, for quotients such as `z^2/((z+1)*(z-1)+1)`: on the way, TSTIMES re-expands the reciprocal of the denominator at a negative truncation level, where TAYLOR2 drops the constant `1` but the product `(z+1)*(z-1)` keeps its constant `-1` (a logarithm, exponential or cosine can keep one too), so the constants stop cancelling. TAYLOR2 now drops a constant result at such a level, and an expression without the main variable, as it already drops a constant input, and TRY-EXPANSION, GAM-CONST and GET-LEXP are adapted to that. None of this applies to an expansion that adjoins a kernel such as `%e^-(1/z^2)` or `log(z)`: TAYLOR1 does it again without these changes, so it gives what master gives.

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
(%i11) taylor(gamma((z+1)*(z-1))/z^3,z,0,-3)
(%o11) -1/z^5+(%gamma-1)/z^3
(%i12) taylor((w+2)/(z*(w-1)),z,0,-1,w,1,-1)
(%o12) +0
(%i13) taylor(1+z+log(1+w)/w^2,z,0,-1,w,0,-1)
(%o13) z+(+(+log(w))/w^2)
(%i14) taylor(cot(z^2-1)/z^3,z,0,-1)
cot: argument 0 isn't in the domain of cot.
 -- an error. To debug this try: debugmode(true);
(%i15) taylor(1/((1-z)*(1-w)),z,0,-1,w,0,0)
`quotient' by `zero'
 -- an error. To debug this try: debugmode(true);
(%i16) taylor(1/(sec(2+z^2)-sec(2)),z,0,-2)
(%o16) +0
(%i17) taylor(1/(csc(2+z^3)-csc(2)),z,0,-2)
csc: argument 0 isn't in the domain of csc.
 -- an error. To debug this try: debugmode(true);
(%i18) taylor((exp(exp(-1/z^2)/z^4)*sin(z))/z,z,0,3)
(%o18) 1-z^2/6
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
(%i11) taylor(gamma((z+1)*(z-1))/z^3,z,0,-3)
(%o11) -1/z^5+(%gamma-1)/z^3
(%i12) taylor((w+2)/(z*(w-1)),z,0,-1,w,1,-1)
(%o12) +(+3/(w-1))/z
(%i13) taylor(1+z+log(1+w)/w^2,z,0,-1,w,0,-1)
(%o13) +0
(%i14) taylor(cot(z^2-1)/z^3,z,0,-1)
(%o14) -1/(tan(1)*z^3)-(tan(1)^2+1)/(tan(1)^2*z)
(%i15) taylor(1/((1-z)*(1-w)),z,0,-1,w,0,0)
(%o15) +0
(%i16) taylor(1/(sec(2+z^2)-sec(2)),z,0,-2)
taylor: unable to tell whether a coefficient vanishes in:
1/(sec(z^2+2)-sec(2))
 -- an error. To debug this try: debugmode(true);
(%i17) taylor(1/(csc(2+z^3)-csc(2)),z,0,-2)
taylor: unable to tell whether a coefficient vanishes in:
1/(csc(z^3+2)-csc(2))
 -- an error. To debug this try: debugmode(true);
(%i18) taylor((exp(exp(-1/z^2)/z^4)*sin(z))/z,z,0,3)
(%o18) 1-z^2/6
```

`(%i10)` and `(%i11)` are right on master and have to stay right. `(%i14)` is `(%i10)` with its constant written as a sum (see "Constant arguments in TRY-EXPANSION" below). `(%i12)`, `(%i13)` and `(%i15)` have two variables (see "Expressions without the main variable"). `(%i16)` and `(%i17)` are not solved, but they stop with an error that says why, where master gives a wrong 0 for `(%i16)` (see "Hidden zeros"). The right answers are `cos(2)^2/(sin(2)*z^2)` and `-sin(2)^2/(cos(2)*z^3)`. `(%i18)` is right on master and has to stay right (see "Kernels").

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

The TAYLOR2 change has consequences in three places, the next three paragraphs. The fourth, "Hidden zeros", is what the second of them uncovers.

**Expressions without the main variable.** With variables given one after the other, as in `taylor(e,z,0,-1,w,0,2)`, TAYLOR2 drops constants by the level of the first one, the main level. An expression that has only the other variables is a constant as far as that level goes. Master expanded it all the same, with the constants in it dropped: at order -1 of `z`, `log(1+w)` came out as `log(w)` (`(%o13)`), and `w+2` at `w = 1` as `w-1` (`(%o12)`). TAYLOR2-1 now returns 0 for such an expression at a negative main level, as it does for a constant, and TSTIMES learns its order from GET-LEXP. Nothing changes for a single variable or a single list of variables: there an expression without the main variables is a constant. The change is needed, not only useful. With constant results dropped and that wrong `log(w)` still there, `taylor(exp(z)*z^2*log(1+w)*log(2+w)/w,z,1,-1,w,0,0)`, which is 0 on master, stopped with an error.

**Constant arguments in TRY-EXPANSION.** When GET-LEXP finds that a function's argument expands to a constant, TRY-EXPANSION expanded the argument once more with TAYLOR2 at the current truncation level and took the function at the result. At a negative level that result is 0 whenever TAYLOR2 drops the constant. For a sum like `z^2-1` it does so on master, which is why `(%i14)` stops with an error about `cot(0)` there. For a product like `(z+1)*(z-1)` it does so only with the TAYLOR2 change, and with that change alone `(%i10)` stopped with the same error, as did `csc`, `coth`, `gamma` and `psi[0]` of such a product: 18 of 4,870 generated expansions that are right on master. TRY-EXPANSION now keeps the constant GET-LEXP found when the truncation level is negative, so the function is taken at that constant. That keeps `(%i10)` right and repairs `(%i14)`.

**GAM-CONST.** At a pole, GAM-CONST in `src/specfn.lisp` writes `gamma(a)` as `gamma(a+n)` divided by the product `a*(a+1)*...*(a+n-1)`, and expands that product at the current level plus the order of `a+n`. That level can be negative. The product has a constant term, and at a negative level it came out as a bare constant on master. With the TAYLOR2 change it is 0 there, and the division stopped `(%i11)` and `taylor(gamma((z+1)*(z-1))/z^2,z,0,-3)`, both right on master, with an error. GAM-CONST now expands the product at level 0 at least. Where that makes a difference `gamma(a+n)` expands to 0, so the result is the 0 master has.

**Hidden zeros.** taylor expands `sec`, `csc`, `cot`, `sech`, `csch` and `coth` through the reciprocal function, so `csc(2+z^3)` gets the constant term `1/sin(2)`, and taylor's rational arithmetic does not know that this is `csc(2)`. In `(%i17)` the denominator therefore expands to 0 at truncation 1 and 2, but to the constant `1/sin(2)-csc(2)` plus a `z^3` term at truncation 4. GET-LEXP, which doubles the truncation until the expansion is not 0, returned that series, and its callers took the constant for the leading term. Master does not get that far at a negative order. It takes the function at 0 before, as described for TRY-EXPANSION, and stops with an error about `csc(0)`, or returns 0 for `sec`, which is finite at 0. That 0 is right only where the requested order lies below the leading term: in `(%i16)` it is wrong. With the TRY-EXPANSION change the function is no longer taken at 0, and without more both `(%i17)` and `taylor(1/(cot(1/2+z^2)-cot(1/2)),z,0,-1)` gave 0 instead of an error. A wrong result in place of an error is worse, so GET-LEXP now checks the series it returns. If expansions to a lower truncation came out 0 and this one starts with a constant, that constant should have been there before, and taylor stops with "unable to tell whether a coefficient vanishes in:". The check is that narrow on purpose. A term of higher degree can turn up late for good reasons: `taylor(asin(1+z^2),z,0,1)` is `%pi/2` on master, without the term `-sqrt(2)*%i*z` that order 2 has, and `taylor(1/(asin(1+z^2)-asin(1)),z,0,1)` is right because GET-LEXP goes on to it. The check applies where GET-LEXP is asked for a series that must not be 0, as by TSEXPT1 for the base of a power. Where it only estimates an order, as for TSTIMES, nothing changes.

This is a stopgap. Such a quotient now stops with that error at every order, also where master's 0 happened to be right: `taylor((1+z)/(sec(2+z^2)-sec(2)),z,0,-3)` is 0 on master and an error now, while master's 0 at order -2 is wrong and at order 0 master stops with "quotient by zero". The same holds with `csc`, `cot`, `sech`, `csch` or `coth`. The right results need the constant term to be recognized, as in `taylor(sec(2+z^2)-sec(2),z,0,2)`, which keeps the constant `-((cos(2)*sec(2)-1)/cos(2))` on master and with this fix. That is a separate change in how these six functions are expanded, the subject of bug #5848. `sin`, `cos`, `tan`, `sinh`, `cosh`, `tanh`, `exp` and `log` are not affected.

Generated tests turned up two more kinds of zeros that were right by accident and are errors now, one case of each. `taylor(exp(z)/(li[2]((1+z)*(1-z))-li[2](1)),z,0,-3)` is 0 on master, which took `li[2]` at 0. Now `li[2]` is taken at 1, where taylor cannot expand it, and the error is "encountered an unfamiliar singularity", as for `taylor(li[2](1-z^2),z,0,3)` on master. And `taylor(signum((z-2)*z+1)/(z-1)+z/(z-1)^2,z,1,-3)` is 0 on master and stops with the hidden-zero error now: `abs((z-2)*z+1)` expands to 0 at truncation 1 and comes back as a constant at truncation 2. At order -1 master's result for it is wrong.

**Polygamma functions.** `taylor(psi[1](1+z),z,0,2)` is `%pi^2/6-zeta(3)*z+%pi^4*z^2/90` on master, where it should be `%pi^2/6-2*zeta(3)*z+%pi^4*z^2/30`: for `psi[n]` with n >= 1 every coefficient after the first is wrong. That is bug #5529 and is not touched here. But `taylor(psi[1](1+z^2)/z^3,z,0,-1)`, which stops with "Pole encountered in: psi[1](0)" on master, now goes through and shows it: the result is `%pi^2/(6*z^3)-zeta(3)/z`, and the second term should be `-2*zeta(3)/z`.

**Kernels.** For `exp(-1/z^2)`, `log(z)` and the like, taylor adjoins a kernel to TLIST and expands in it as in one more variable. Then the main truncation level no longer means what the changes above assume, and applied there they do harm. In `(%i18)`, TSTIMES re-expands the factor `exp(exp(-1/z^2)/z^4)` at the main level -9, because its kernel terms carry powers of `z` down to `z^-12`, and the constant 1 that comes back is the leading term of that factor. With that constant dropped the product would be 0, and `tlimit(exp(exp(-1/x^2)/x^4)*((1-cos(x))/x^2),x,0)`, which is 1/2, would run out of memory, because `tlimit` raises the order until the series is not 0. The check in GET-LEXP would go wrong as well. With a flat kernel an expansion is not cut by degree alone, so a leading constant after an earlier 0 can be right: `tlimit(sin(K/(1+K^2)),x,2,plus)` with `K = exp(-1/(x-2)^2)/(x-2)^4` is 0 and would come out as `ind`.

So an expansion with a kernel is done without the changes. Each of them asks NO-KERNELS?, directly or through CONSTANTS-BEYOND-TRUNC?, and NO-KERNELS? is false once TLIST holds a kernel. That alone is not enough, because a kernel can turn up after a change has already taken effect. `taylor((exp(exp(-1/z^2))-1)*(cot(z^2-1)/z^3),z,0,-3)` stops with the error about `cot(0)` on master. With only that test, the second factor comes out right and the first as on master, which has `-1` for it, so the result would be `cot(1)/z^3`, which is wrong. So each change records that it has changed a result (LEVEL-RULE-USED). When that has happened and TLIST holds a kernel in the end, TAYLOR1-1 returns the symbol KERNELS-AHEAD, and TAYLOR1 expands once more with `*KERNELS-AHEAD?*` true, which keeps the changes off from the start. That second expansion is master's, wrong results included: `taylor(z^2*log(z)/((z+1)*(z-1)+1),z,0,1)` is still 0 instead of `log(z)`, and `taylor(z^2/((z+1)*(z-1)+1)+log(z),z,0,1)` is still `log(z)` instead of `log(z)+1`. An expansion without a kernel, and one in which no change took effect, is done once. The price is that the changes help no expansion with a kernel. With the changes merely switched off once a kernel had turned up, 142 more of the 6,048 mixed expansions under "Checks" were right, but 12 errors became wrong results, as did some of 24 more that the numeric judge could not evaluate, such as `taylor(cos(exp(-1/z^2))*cot(z^2-1)/z^3,z,0,-3)`, which became 0.

What can still differ from master with a kernel is an error that stops the first expansion before it is over. One is the error for a hidden zero in a part without kernels: `taylor(exp(-1/z^2)/(sec(2+z^2)-sec(2)),z,0,-2)` is 0 on master and stops with that error now. The other is master's own failure for the part with the kernel, reached because the other part is no longer 0. `taylor((1/((z+1)*(z-1)+1))/(exp(z^2*exp(-1/z^2))-1),z,0,-2)` is 0 on master, which is wrong, and stops with "quotient by zero" now, as it does on master with the first factor written as `1/z^2`. `taylor(cos(z*log(z))/(cos(1+z^2)-cos(1)),z,0,-2)`, also a wrong 0 on master, overflows the control stack now, as `taylor(cos(z*log(z))/z^2,z,0,-2)` does on master.

Master's own zeros for quotients like `1/(z*(1+exp(-1/z^2)/z^4))`, which should be `1/z`, are a separate defect and stay: TSTIMES lowers the level of a factor by the powers of `z` in its kernel terms. That is bug #5858, and it is also what sends the factor in `(%i18)` to the level -9.

**The `%i` inputs.** TAYLOR1 runs RATSETUP before ORDERPOINTER creates the gensyms, so the gensym of `%i` never gets the TELLRAT property and `%i^2` stays unreduced in taylor's CRE arithmetic. For `tan(A)/A`, TRIG-CONST splits off the constant term `-%i^2-1`, which is nonzero as a CRE, but `A` minus that constant simplifies back to `A`, so PSTANA+B expands `tan(A)` again without end. I emulated an unconditional `%i` reduction with a variant of the function that calls ORDERPOINTER, TAYLOR1 on master and TAYLOR1-1 with this fix. Right after `(orderpointer varlist)` it gives that gensym the property as RATSETUP2 would:

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

**Checks.** Every expected value was checked numerically, independent of `taylor()`: the unexpanded function evaluated in 80-digit bigfloats at h = 1e-6, 1e-8, 1e-10, with (f(h)-S(h))/h^(n+1) staying bounded. For the old answer 0 it grows like 1/h^2. The residue in row 9 was checked by the trapezoid rule on |z| = 1/2, giving 1.0000000000000004. The two-variable results in rows 12, 13 and 15 follow from the single-variable expansions in each variable. A taylor-heavy benchmark run interleaved on master and on all three fixes showed no slowdown (2.29 s and 2.31 s on average over five runs each, with single runs between 2.19 s and 2.39 s).

Measured with the TAYLOR2 change alone, before the other changes: on master 8c68d35ac, it makes 79 more of 1,122 generated expansions right (negative orders, cancelling denominators, asymptotic expansions, residues and tlimits) and 29 more of 857 expansions with `%i`, and none that were right got worse. Corpora of 3,462 trigonometric expansions and residues at multiples of `%pi`, 2,500 general expansions, 1,500 residues and 222 definite integrals are unchanged, apart from one series at order -1 that no longer keeps a degree-0 term: `taylor(tan(z)+1/(z-%pi/2),z,3*%pi/2,-1)` is now `-1/(z-3*%pi/2)`, without the `1/%pi` it had.

Measured with the code as it is here, on master 8c68d35ac, against results that do not come from the tested build:

- Of 4,870 generated expansions (products, quotients and sums of functions with and without poles, arguments with hidden constants, 9 expansion points, orders -4 to 2, 870 of them asymptotic), each judged numerically as above, 167 more are right, 82 that were wrong and 85 that stopped with an error. 10 zeros that were right are errors now, and so are 6 zeros that were wrong. All 16 are hidden zeros. 3 that stopped with an error give a wrong result with this fix alone and the right one with the nonzero-point fix. All 3 are at a point other than 0.
- Of 1,100 generated expansions at 0 of a function of a constant plus higher terms, divided by a power of `z`, at orders -5 to 1 (17 functions, among them `cot`, `csc`, `coth`, `csch`, `gamma` and `psi[0]`, and 28 arguments such as `(z+1)*(z-1)`, `z^2-1` and `%pi+z^2`), 178 stop with an error on master. All 1,100 are right now.
- Of 1,000 generated quotients `X/(f(c+s)-f(c))` at orders -4 to 2 (16 functions `f`), 107 more are right, 73 that were wrong and 34 that stopped with an error. 34 wrong results and 28 right zeros are errors now, all of them hidden zeros. No error and no right result became a wrong result.
- Of 4,000 generated expansions of 57 special and unknown functions, sums and products (`erf`, `zeta`, `gamma_incomplete`, `li[2]`, `bessel_j`, `asin`, `signum`, `f(...)` and so on) of a constant plus higher terms, 3,000 at 0 and 1,000 at 1, at orders -4 to 2, 103 more are right, 68 that stopped with an error and 35 that were wrong. The numeric judge confirms 93 of them, the other 10 are sums and products it cannot evaluate and were checked by hand. 2 zeros that were right are errors now, the `li[2]` and the `signum` case above. 9 that stopped with an error give a wrong result: 7 at the point 1, which the nonzero-point fix makes right, and 2 with `psi[1]`, see "Polygamma functions". 6 wrong results became errors.
- Of 900 generated residues, checked by a contour sum, one more is right and none got worse. 1,200 generated residues at algebraic poles are unchanged.
- Of 1,000 expansions in a list of two variables, checked against a single-variable expansion along a line through the point, 32 more are right and none got worse.
- Of 2,700 expansions in two separately given variables, checked against iterated single-variable expansions, 45 more are right, 33 that were wrong and 12 that stopped with an error, and no result that was right became an error. One changed: `taylor(1/z^3*(w^2+1)/w+1/((z+1)*(z-1)+1),z,0,-1,w,0,-1)` now has the term `1/z^2`, as master has when that term is written as `1/z^2`. 6 results that were wrong or errors changed without becoming right, none of them from an error to a result. Without the change for expressions without the main variable, none of the 45 is right and one result that is right on master is an error, the one named in that paragraph.
- With kernels: 17,223 generated expansions with a flat kernel at 0 (`exp(-1/z^2)`, `exp(-1/z^2)/z^4` and three more, inside `exp`, `cos`, `1/(1+K)`, `sqrt(1+K)` and four more forms, multiplied with, divided into and added to 15 ordinary functions, orders -2 to 4), the same at `inf` with `exp(-z)`, `z^10*exp(-z)` and three more, and with `log(z)` at 0 (16 forms such as `z*log(z)`, `z^z` and `log(sin(z))` combined with 21 functions, orders -3 to 3). Every result is the one master gives, and the expansions that fail on master fail here, and no others. 585 expansions at `inf` and `minf` without a kernel are unchanged but for one that was wrong and is right. With the changes applied regardless of kernels, 73 of the 14,535 expansions with a flat kernel differ from master: 70 right series are 0 and 3 are errors.
- Of 6,048 generated expansions that combine a kernel form (four flat kernels and `log(z)`, `z*log(z)` and `1/log(z)`, in 9 forms such as `exp(K)-1` and `1/(1+K)`) with one of 12 quotients and functions of the kinds above, as a product, a quotient or a sum, at orders -3 to 3, 5,883 give the result master gives or fail on both. 45 have no kernel, since `exp(log(z))` is `z`, and are right now, 31 that were wrong and 14 that stopped with an error. The other 120 are errors now, as described under "Kernels". 117 stop with the error for a hidden zero, all with `1/(sec(2+z^2)-sec(2))`. On master 113 of them are 0, which is right in 32, wrong in 52 and could not be judged in 29, and 4 overflow the control stack. 2 stop with "quotient by zero" and one overflows the stack, all three wrong zeros on master. Among the expansions with a kernel, no error became a result and no result changed to another one.
- 1,975 limits with those kernel forms, 1,006 by `tlimit` and 969 by `limit`, give what master gives, 61 failures of `tlimit` included (crashes, timeouts and errors). So do 1,492 ordinary limits by `limit` and `tlimit` at 0, 1 and `inf`.

## 3. Bug report

Bug #5856 reports the same defect with other examples, and this fix repairs all of them. So the report below can go there as a comment instead of a ticket of its own. The fix also repairs #288, and the examples of #5848 at negative orders that have no hidden zero.

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

Another one, with two variables: when the order of the first variable is negative, an expression without that variable is expanded with its constants dropped, so `log(1+w)` becomes `log(w)`. Nothing below has a negative power of `z`, so the answer is `0`:

```
(%i11) taylor(1+z+log(1+w)/w^2, z, 0, -1, w, 0, -1);
(%o11) z+(+(+log(w))/w^2)
```

This happens in 5.47.0, 5.50.0 and on master.
````

## 4. Code patching instructions

### `src/hayat.lisp`: TAYLOR1 expands again when a kernel turns up

Moves the body of TAYLOR1 to TAYLOR1-1 and calls it a second time, with the level rules off, when the first expansion asks for that.

**Replace** this block (its second line has a space before the closing parentheses):

```lisp
(defun taylor1 (e tlist)
  (declare (special *within-srf?* ))
  (setq tlist (tlist-merge (nconc (find-tlists e) tlist)))
```

**with** this block:

```lisp
;; The rules that drop what lies beyond a negative main truncation level
;; (see CONSTANTS-BEYOND-TRUNC?) hold only for an expansion that adjoins no
;; kernel such as %e^-(1/x) or log(x) to TLIST. When a kernel turns up after
;; such a rule has changed a result, TAYLOR1 expands once more, with
;; *KERNELS-AHEAD?* true, which keeps those rules off from the start.

(defvar *kernels-ahead?* ())
(defvar *level-rule-used?* ())

(defun taylor1 (e tlist)
  (setq tlist (tlist-merge (nconc (find-tlists e) tlist)))
  ;; TAYLOR1-1 changes the data of TLIST, so a second expansion gets a copy.
  (let* ((tlist-copy (copy-tree tlist))
         (ans (let ((*kernels-ahead?* ()) (*level-rule-used?* ()))
                (taylor1-1 e tlist))))
    (if (eq ans 'kernels-ahead)
      (let ((*kernels-ahead?* 't) (*level-rule-used?* ()))
        (taylor1-1 e tlist-copy))
      ans)))

(defun taylor1-1 (e tlist)
  (declare (special *within-srf?* ))
```

### `src/hayat.lisp`: TAYLOR1-1 asks for the second expansion

Returns the symbol KERNELS-AHEAD when a level rule changed a result and a kernel was adjoined.

**Replace** this line (it is indented with a tab):

```lisp
	(setq ans (catch 'tay-err (taylor3 e)))
```

**with** this block:

```lisp
	(setq ans (catch 'tay-err (taylor3 e)))
	(when (and *level-rule-used?* (kernels-in-tlist?))
	  (return 'kernels-ahead))
```

### `src/hayat.lisp`: TAYLOR2 drops constant results and expressions without the main variable

Wraps the old TAYLOR2, renamed TAYLOR2-1, with the check for a constant result, and gives TAYLOR2-1 a first clause for an expression without the main variables.

**Replace** this block (its first line has two spaces before `(e)`, and its second line has a tab before the comment):

```lisp
(defun taylor2  (e)
 (let ((last-exp e))	    ;; lexp-non0 should be bound here when needed
  (cond ((assolike e tlist) (var-expand e 1 () ))
```

**with** this block:

```lisp
(defun taylor2 (e)
  ;; A constant is a term of degree 0, so a negative truncation level drops
  ;; it (see CONSTANTS-BEYOND-TRUNC?). TAYLOR2-1 does that for a constant E,
  ;; and this does it for a constant result. Such a result can come from a
  ;; series whose other terms cancelled or lie beyond its truncation, since
  ;; PSCHECK makes that series a bare constant, which PSTRUNC cannot
  ;; truncate. Kept, it would spoil sums: expanded to degree -1, (x-1)*(x+1)
  ;; gives -1 but 1 gives 0, so 1+(x-1)*(x+1) would get the constant term
  ;; -1, and TSEXPT1 would take that for the leading term of its base.
  (let ((ans (taylor2-1 e)))
    (if (and (pscoefp ans) (not (rczerop ans)) (constants-beyond-trunc?)
             (level-rule-used))
      (pszero (data-gvar-o mainvar-datum) (current-trunc mainvar-datum))
      ans)))

(defun taylor2-1 (e)
 (let ((last-exp e))	    ;; lexp-non0 should be bound here when needed
  (cond ((and (constants-beyond-trunc?) (mfree e (main-tvars)))
	 ;; E is free of the main variables, so it is a term of degree 0
	 ;; in them and lies beyond the truncation like a constant. Its
	 ;; own expansion would be wrong there, since the constants in
	 ;; it are dropped: log(w+1) would become log(w).
	 (unless (mfree e tvars) (level-rule-used))
	 (pszero (data-gvar-o mainvar-datum) (current-trunc mainvar-datum)))
	((assolike e tlist) (var-expand e 1 () ))
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

### `src/hayat.lisp`: new functions MAIN-TVARS, KERNELS-IN-TLIST?, NO-KERNELS?, LEVEL-RULE-USED and CONSTANTS-BEYOND-TRUNC?

MAIN-TVARS returns the variables that are truncated at the main level. The others say when the level rules apply and record their use.

**Insert** this block:

```lisp
;; The variables truncated at the main level: the variable given first to
;; taylor, or the variables of a list given first.

(defun main-tvars ()
  (let ((multivar (switch 'multivar mainvar-datum)))
    (if multivar
      (cadr multivar)
      (ncons (datum-var mainvar-datum)))))

;; Has a kernel such as %e^-(1/x) or log(x) been adjoined to TLIST?

(defun kernels-in-tlist? ()
  (notevery #'(lambda (d) (tvar? (datum-var d))) tlist))

;; Is this an expansion without kernels, as far as is known (see TAYLOR1)?

(defun no-kernels? ()
  (not (or *kernels-ahead?* (kernels-in-tlist?))))

;; Records for TAYLOR1 that a rule which asks NO-KERNELS? has changed a
;; result. Returns true.

(defun level-rule-used ()
  (setq *level-rule-used?* 't))

;; Does a constant lie beyond the truncation? It is a term of degree 0, so
;; it does when the main truncation level is negative. That holds only
;; without kernels. With one, TSTIMES can expand a factor like
;; %e^(%e^-(1/x)/x^4) at a negative main level because of the powers of x
;; in its kernel terms, and the constant 1 it gets is the leading term.

(defun constants-beyond-trunc? ()
  (and (e> (rczero) (current-trunc mainvar-datum)) (no-kernels?)))

```

**before** this line:

```lisp
(defun compatvarlist (a b c d)
```

### `src/hayat.lisp`: keep a constant argument in TRY-EXPANSION

Skips the second expansion of a constant argument where constants lie beyond the truncation.

**Replace** this line:

```lisp
     (when (pscoefp psarg) (setq psarg (taylor2 arg)))
```

**with** this block:

```lisp
     ;; ARG expands to the constant PSARG. Expand it again for the terms the
     ;; current truncation level adds, unless constants lie beyond that
     ;; level: there TAYLOR2 gives 0, and FUNC would be taken at 0 instead
     ;; of at PSARG.
     (when (pscoefp psarg)
       (if (constants-beyond-trunc?)
         (level-rule-used)
         (setq psarg (taylor2 arg))))
```

### `src/hayat.lisp`: GET-LEXP remembers that an expansion came out 0

Adds a variable to the DO loop of GET-LEXP.

**Replace** this line (it is indented with a tab):

```lisp
	(do ((psexp) (i (1+ $taylordepth) (1- i)))
```

**with** this line:

```lisp
	(do ((psexp) (i (1+ $taylordepth) (1- i)) (zero?))
```

### `src/hayat.lisp`: GET-LEXP checks the leading term it returns

Stops with an error when the series starts with a constant after an expansion that came out 0.

**Replace** this block (both lines are indented with two tabs):

```lisp
		(t (tlist-mapc d (pop-pw d))
		   (return psexp)))
```

**with** this block:

```lisp
		(t (tlist-mapc d (pop-pw d))
		   ;; PSEXP is the first expansion that is not 0. If it starts
		   ;; with a constant, the expansions before it, which came
		   ;; out 0, should have had that constant too. So it may be
		   ;; a 0 that was not seen to vanish, like 1/sin(2)-csc(2),
		   ;; and the leading term is not known. Expansions with
		   ;; kernels are left alone.
		   (when (and zerocheck? zero? (not (rczerop psexp))
			      (no-kernels?) (starts-with-constant? psexp))
		     (level-rule-used)
		     (zero-test-err))
		   (return psexp)))
	  (setq zero? 't)
```

### `src/hayat.lisp`: new function STARTS-WITH-CONSTANT?

The test GET-LEXP uses.

**Insert** this block:

```lisp
;; Does the series PS start with a constant, a term of degree 0 in each of
;; its variables?

(defun starts-with-constant? (ps)
  (or (pscoefp ps)
      (and (ezerop (ps-le ps)) (starts-with-constant? (ps-lc ps)))))

```

**before** this line:

```lisp
(defun 1p (x)
```

### `src/hayat.lisp`: new function ZERO-TEST-ERR

The error GET-LEXP signals.

**Insert** this block:

```lisp
(defun zero-test-err ()
  (tay-err
   (intl:gettext "unable to tell whether a coefficient vanishes in:")))

```

**before** this line (it has two tabs and a space after the semicolons):

```lisp
;;;		 Subtitle TAYLORINFO
```

### `src/specfn.lisp`: GAM-CONST expands its product at level 0 at least

Keeps the product that GAM-CONST divides by from vanishing at a negative level.

**Replace** this block (the first line is indented with three tabs and six spaces, the second with four tabs and six spaces):

```lisp
			      (let-pw datum (e+ func ord)
				      (psexpt (tsprsum (m+t a '%%taylor-index%%)
```

**with** this block:

```lisp
			      ;; The product has a constant term, so it takes a
			      ;; level of at least 0 to be invertible. With
			      ;; kernels its level is left alone (see TAYLOR1).
			      (let-pw datum (if (and (e> (rczero) (e+ func ord))
						 (no-kernels?) (level-rule-used))
					    (rczero)
					    (e+ func ord))
				      (psexpt (tsprsum (m+t a '%%taylor-index%%)
```

### `tests/rtest_taylor.mac`: new tests

Adds the tests of section 5.

**Insert** the first block of section 5, followed by two blank lines, **before** this line:

```
/* clean up & checks */
```

## 5. Proposed test cases

**New**, in `tests/rtest_taylor.mac`, before its final clean-up checks. Replace `NNNN` by the number the bug report gets. The fifth pair covers TRY-EXPANSION: its first expansion is right on master, the other two stop with an error there. The sixth pair is right on master and guards GAM-CONST. The seventh accepts an error or the right leading term, so it stays valid once hidden zeros are solved. On master its third result is false, because of the 0 in `(%o16)` of section 2. The eighth pair has the two-variable cases, and its last expansion is right on master. The ninth is right on master and guards expansions with a kernel (see "Kernels" in section 2).

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

ratsimp(map('ratdisrep,
            [taylor(gamma((z+1)*(z-1))/z^2, z, 0, -3), taylor(gamma((z+1)*(z-1))/z^3, z, 0, -3)])
        - [-1/z^4, (%gamma-1)/z^3-1/z^5]);
[0, 0]$

/* The denominators hide a 0, like 1/sin(2)-csc(2). Right here is an error
   or the leading term, but not 0. */
map(lambda([c],
      block([r : errcatch(taylor(c[1], z, 0, c[2]))],
        is(r = [] or abs(float(subst(z = 1/2, ratdisrep(r[1]) - c[3]))) < 1e-9))),
    [[1/(csc(2+z^3)-csc(2)), -2, -sin(2)^2/(cos(2)*z^3)],
     [1/(cot(1/2+z^2)-cot(1/2)), -1, -sin(1/2)^2/z^2],
     [1/(sec(2+z^2)-sec(2)), -2, cos(2)^2/(sin(2)*z^2)]]);
[true, true, true]$

ratsimp(map('ratdisrep,
            [taylor((w+2)/(z*(w-1)), z, 0, -1, w, 1, -1),
             taylor(1/((1-z)*(1-w)), z, 0, -1, w, 0, 0),
             taylor(1+z+log(1+w)/w^2, z, 0, -1, w, 0, -1),
             taylor(exp(z)*z^2*log(1+w)*log(2+w)/w, z, 1, -1, w, 0, 0)])
        - [3/(z*(w-1)), 0, 0, 0]);
[0, 0, 0, 0]$

/* With a kernel like %e^-(1/z^2), a constant that comes back at a negative
   order of z can be a leading term. */
[map('ratdisrep,
     [taylor(exp(exp(-1/z^2)/z^4)*sin(z)/z, z, 0, 3), taylor((z/sin(z))/exp(exp(-1/z^2)/z^3), z, 0, 4),
      taylor(z*sin(1/z)/exp(z^10*exp(-z)), z, inf, 3)]),
 tlimit(sin((exp(-1/(x-2)^2)/(x-2)^4)/(1+(exp(-1/(x-2)^2)/(x-2)^4)^2)), x, 2, plus)];
[[1-z^2/6, 1+z^2/6+7*z^4/360, 1-1/(6*z^2)], 0]$
```

**New**, in `tests/rtest_taylor.mac`, to be appended only together with an unconditional `%i` reduction in `taylor()`, such as the emulation in section 2 (reducing every constant algebraic kernel works too). Not with the pending H29 fix as handed over: that reduces `%i` only inside the Gruntz series, so the first input still overflows the control stack at user level. On master it does too, which can kill SBCL and with it the whole test run. Run only against the emulation, where it passes, and fails without this patch.

```
block([u : (z+%i)*(z-%i)-z^2-1+z^2, v : (1+%i*z)*(1-%i*z)-1-z^2+z^3],
  map('ratdisrep,
      [taylor(tan(u)/u, z, 0, 1), taylor(tan(u)/u, z, 0, 4),
       taylor(log(1+v)/v, z, 0, 1), taylor(log(1+v)/v, z, 0, 4), taylor(1/u, z, 0, -2)]));
[1, 1+z^4/3, 1, 1-z^3/2, 1/z^2]$
```

`run_testsuite(share_tests=true)` on SBCL 2.2.9, master 8c68d35ac with this fix: 1 failure out of 22,389 tests (`rtestprintf.mac` problem 38, a float printing difference of this SBCL), the same failure as master out of 22,380. `rtest_taylor` passes 264/264. Of its 9 new problems, 7 fail on master, and the sixth and the ninth pass there. The second block passes on master with the emulation and this fix, and fails with the emulation alone.

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
longer cancelled. Now it is dropped as well. With several variables,
so is an expression without the first variable. It was expanded with
its constants missing, so log(1+w) became log(w).

At such an order taylor() also took a function at 0 when its
argument was a constant plus higher terms, so
taylor(cot(z^2-1)/z^3,z,0,-1) stopped with an error about cot(0).
Now it takes the function at that constant.

taylor() cannot tell that sec(2+z^2)-sec(2) has no constant term,
and likewise for csc(), cot(), sech(), csch() and coth(). A quotient
with such a denominator gave 0, often wrongly, or stopped with an
error about csc(0). Now it stops with an error that says what
taylor() could not tell.

All of this holds for ordinary power series. Once an expansion
involves a term like exp(-1/z^2) or log(z), which taylor() treats as
a variable of its own, it works as before.

This fixes bugs #NNNN, #5856 and #288.

AI-Assisted-By: Claude Opus 5.5
AI-Assisted-By: Claude Fable 5.2
```
