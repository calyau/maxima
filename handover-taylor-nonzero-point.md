# Handover: taylor() at a point other than 0 to a negative order

## 1. Summary

**Fix taylor() losing terms at a nonzero point and a negative order**

`taylor()` to a negative order at a point other than 0 lost terms, often all of them: `taylor(s/(s-1),s,1,-1)` gave 0 instead of `1/(s-1)`. At a negative truncation level VAR-EXPAND drops the constant `a` from `s = a+(s-a)` and returns `s-a` alone, so TSTIMES takes `s` for a quantity of order 1 and expands the other factors too short. VAR-EXPAND now returns 0 there for the main variable, the whole of `s` being beyond the truncation, and TSTIMES finds the true order 0. This builds on `handover-taylor-cancelling-constants.md`, which must be applied first.

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
```

`(%i11)` is right on master and has to stay right (see "Only at the main level" below).

### Internals

**Where it goes wrong.** For `taylor(s/(s^2-a^2),s,a,-1)`, TSTIMES expands each factor at truncation -1. VAR-EXPAND builds `s` as the kernel `s-a` plus TAYLOR2 of the point `a`, and TAYLOR2 drops that constant at a negative truncation level, so `s` comes back as `(PS ... ((-1 . 1)) ((1 . 1) 1 . 1))`: the kernel alone, a series of order 1. With orders 1 and -1, TSTIMES re-expands the reciprocal at truncation -1 + (-1) - 0 = -2, where it is 0 because its true order is -1, and the product is 0. With the true order 0 for `s`, the reciprocal is re-expanded at -1 and the product comes out right.

**The fix.** `(s-a+a)^exp` starts with the constant `a^exp`, so a negative truncation level leaves nothing of it, and VAR-EXPAND now returns 0. TSTIMES then finds the true order of `s` through GET-LEXP, as for any factor that vanishes at the truncation level. This agrees with the zero-point branch of VAR-EXPAND, which returns 0 for a power beyond the truncation, and with the TAYLOR2 rule of the first handover. The new clause comes before the multivariable branch, which builds a variable as the point plus a multiple of the label kernel and lost the point the same way (`(%i6)` and `(%i7)` above).

**Only at the main level.** The clause applies at the main truncation level only, the level of MAINVAR-DATUM: to the first variable given or, when a list such as `[x,y]` comes first, to the variables of that list, which share one level. That is the level at which TAYLOR2 drops constants, so the constants around `s` are gone when `s` is. A variable given separately after the first one, such as `w` in `(%i11)`, has a truncation level of its own. While the main level is not negative a constant survives there, and a 0 for `w` would combine with it and turn `w-1` into `-1`. With the clause applied at each variable's own level, `(%i11)` gave 4, `taylor(1/((z-1)*(w-1)),z,1,-1,w,1,-1)` gave `-1/(z-1)`, and 104 of 1,500 generated expansions in two separately given variables that are right on master came out wrong. So such a variable is left as it is on master.

**TSEXPT1.** When its base expands to 0, TSEXPT1 cannot tell the base's variable and takes the truncation level PW from the first datum of TLIST. When the base first looks like a nonzero constant, PW is 0. TSEXPT1 then finds the base's leading term with GET-LEXP but kept that guess. This mattered little while a base seldom expanded to 0. Now a base that contains the main variable does so at every negative level, and with a second variable the guess is that variable's level: in `taylor(1/(z-1)^3+log(1+(w-2))/(w-2)^2,z,1,-3,w,2,2)` the first term came back truncated at 2, the level of `w`, and the sum gained a wrong term `log(w-2)/(w-2)^2`. TSEXPT1 now takes PW from the variable of the series GET-LEXP returns, as it does when the base does not vanish. This also corrects `(%i12)`, where the base looks like the constant `2-sqrt(2)^2` at first and PW was 0.

**What it leaves alone.** Asymptotic expansions (`asymp`) are excluded: there the constant multiplies negative powers of the kernel, and dropping it is a different defect, fixed separately (see `handover-taylor-asymp.md`). Kernels adjoined during an expansion are not the main variable, so they are left alone too. Their datum's EXP-PT holds a limit such as `$zeroa` instead of a point, and returning 0 for them broke `rtest_taylor` problem 226 (bug #4806), an expansion at `inf` that adjoins such kernels.

**Side effect.** With `taylor_truncate_polynomials:false`, `taylor(s,s,a,-1)` gave `s-a`, which is not `s` in any sense, and now gives 0 (`(%i9)`). That is what the zero-point branch already gives for a power beyond the truncation with exact polynomials (`taylor(x^5,x,0,3)` is 0 there too). There is no test for it, since the manual does not say what an exact polynomial gives at a negative order.

**residue().** RESM1 in `src/residu.lisp` expands to order 1 because, its comment says, `residue(s/(s^2-a^2),s,a)` fails at order -1. That was this bug. RESM1 is unchanged here, and `handover-residue-order.md` switches it. With all three fixes on master 8c68d35ac, RESM1 at order -1 gives the same value as at order 1 for all 900 generated residues below, among them `cot(z)` at `%pi`, `tan(z)` at `%pi/2` and `gamma(z)` at -1 and -2, which commit f0e672f made work at order -1. On master alone, order -1 breaks 80 of the 826 residues that are right.

**Not covered.**

- `taylor((z+w)/(z-1)^2,[z,w],[1,0],-1)` still gives `1/(z-1)^2` instead of `1/(z-1)^2+1/(z-1)+w/(z-1)^2`, as in 5.47.0, although `z/(z-1)^2` and `w/(z-1)^2` on their own now come out right.
- A variable given separately after the first one is not fixed: `taylor(w/(w-1),z,1,0,w,1,-1)` and `taylor(y*s/(s-1),y,0,1,s,1,-1)` still give 0.
- Expanding an existing series again to a negative order still gives 0, as in `taylor(taylor(1/(s-1)^2+1/(s-1),s,1,2),s,1,-1)`. That is a separate defect in RE-TAYLOR-RECURSE, on master too.

**Checks.** The expected values were checked numerically, independent of `taylor()`: f-S evaluated in 80-digit bigfloats at s = p+h for h = 1e-6, 1e-9, 1e-12 stays bounded (1, 1/12 for a = 3, 3*%e/2 and 1/4 in absolute value), while for the old answer 0 it grows like 1/h. The two-variable and the separate-order cases are exact identities, checked with `ratsimp`.

Measured with the clause at the variable's own level and without the TSEXPT1 change: on master 8c68d35ac with the cancelling-constants fix, this fix makes 63 more of the 1,122 generated expansions right (58 at points other than 0, 5 in two variables), and none that were right got worse. The other corpora named in that handover are unchanged.

Measured with the code as it is here, on master 8c68d35ac with the cancelling-constants fix:

- Of the 4,870 generated single-variable expansions of that handover, judged numerically, 126 more are right and none got worse. All but 2 results are the same as with the clause at the variable's own level. Those 2 were wrong and are right through the TSEXPT1 change, `(%i12)` among them.
- Of 1,000 expansions in a list of two variables, checked against a single-variable expansion along `z = a+t*(z-a)`, `w = b+t*(w-b)`, 114 more are right and none got worse. All 1,000 results are the same as with the clause at the variable's own level.
- Of 1,500 expansions in two separately given variables, checked against iterated single-variable expansions, 46 more are right (20 stopped with an error before). 18 that were 0 now keep a term free of `w` although the order of `w` is negative. Master returns each of these 18 series unchanged when given it explicitly, as it returns `taylor(1/(z-1),z,1,-1,w,0,-2)` = `1/(z-1)`. So they follow master's own rule, and their 0 was the bug. 19 wrong results changed. No other result that was right changed.
- The 900 generated residues are unchanged.

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

The source of `residue` works around this by expanding to order `1`: a comment there says that `residue(s/(s^2-a^2),s,a)` fails at order `-1`.
````

## 4. Code patching instructions

### `src/hayat.lisp`: VAR-EXPAND returns 0 at a negative main truncation level

Adds a clause to the COND of VAR-EXPAND, before its multivariable branch.

**Insert** this block:

```lisp
	   ((and (not (signp e (exp-pt temp))) (null (switch '$asymp temp))
		 (not dont-truncate?) (e> (rczero) (current-trunc temp))
		 (eq mainvar-datum
		     (if (switch 'multi temp)
		       (get-datum (car (switch 'multi temp)))
		       temp)))
	    ;; Expanded at a point other than 0, VAR2^EXP starts with the
	    ;; constant EXP-PT^EXP, so a negative truncation level leaves
	    ;; nothing of it. The code below would drop only that constant and
	    ;; return a power of the kernel, and TSTIMES would take its order
	    ;; for the order of VAR2^EXP, which is 0. This is done only where
	    ;; VAR2 is truncated at the main level, the one at which TAYLOR2
	    ;; drops constants. At the level of another variable a constant
	    ;; can survive, and a 0 for VAR2 would turn VAR2-1 into -1.
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
	      ;; PW was a guess as long as TB had no variable.
	      (when (psp tb)
		(setq pw (current-trunc (get-datum
					 (get-key-var (gvar tb)))))))
```

### `tests/rtest_taylor.mac`: new tests

Adds the tests of section 5, after those of the first handover.

**Insert** the block of section 5, followed by two blank lines, **before** this line:

```
/* clean up & checks */
```

## 5. Proposed test cases

**New**, in `tests/rtest_taylor.mac`, before its final clean-up checks and after the tests of the first handover. Replace `MMMM` by the number the bug report gets. In the third pair the first two expansions are right on master and guard the variable given second, and the last pair covers TSEXPT1.

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
```

`run_testsuite(share_tests=true)` on SBCL 2.2.9, master 8c68d35ac with the cancelling-constants fix and this one: 1 failure out of 22,389 tests (`rtestprintf.mac` problem 38, the same float printing failure as on master). `rtest_taylor` passes 264/264, and its 4 new problems all fail without this patch.

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
own and is left as it was.

A power whose base vanishes at the current order was expanded to the
order of the first variable in taylor's list, or to order 0. Now it
gets the order of its own variable. This also restores the z^2 in
taylor(z/((z+sqrt(2))*(z-sqrt(2))+2)+z^2*(1+z),z,0,2).

This fixes bug #MMMM.

AI-Assisted-By: Claude Opus 5.5
AI-Assisted-By: Claude Fable 5.2
```
