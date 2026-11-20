# Handover: taylor() at a point other than 0 to a negative order

## 1. Summary

**Fix taylor() losing terms at a nonzero point and a negative order**

`taylor()` to a negative order at a point other than 0 lost terms, often all of them: `taylor(s/(s-1),s,1,-1)` gave 0 instead of `1/(s-1)`. At a negative truncation level VAR-EXPAND drops the constant `a` from `s = a+(s-a)` and returns `s-a` alone, so TSTIMES takes `s` for a quantity of order 1 and expands the other factors too short. VAR-EXPAND now returns 0 there, the whole of `s` being beyond the truncation, and TSTIMES finds the true order 0. This builds on the first handover (`handover-taylor-cancelling-constants.md`, commit 5b387d9), which must be applied first.

## 2. Reproducer/Demo

### Old: pristine build of 719d816, which has the first fix (executed)

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
```

Master (749af2f) gives the same wrong results for these inputs. I checked that by loading master's TAYLOR2 and VAR-EXPAND, the only functions that differ, into the patched image.

### New: patched build (executed)

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
```

### Internals

**Where it goes wrong.** For `taylor(s/(s^2-a^2),s,a,-1)`, TSTIMES expands each factor at truncation -1. VAR-EXPAND builds `s` as the kernel `s-a` plus TAYLOR2 of the point `a`, and TAYLOR2 drops that constant at a negative truncation level, so `s` comes back as `(PS ... ((-1 . 1)) ((1 . 1) 1 . 1))`: the kernel alone, a series of order 1. With orders 1 and -1, TSTIMES re-expands the reciprocal at truncation -1 + (-1) - 0 = -2, where it is 0 because its true order is -1, and the product is 0. With the true order 0 for `s`, the reciprocal is re-expanded at -1 and the product comes out right.

**The fix.** `(s-a+a)^exp` starts with the constant `a^exp`, so a negative truncation level leaves nothing of it, and VAR-EXPAND now returns 0. TSTIMES then finds the true order of `s` through GET-LEXP, as for any factor that vanishes at the truncation level. This agrees with the zero-point branch of VAR-EXPAND, which returns 0 for a power beyond the truncation, and with the TAYLOR2 rule of the first handover. The new clause comes before the multivariable branch, which builds a variable as the point plus a multiple of the label kernel and lost the point the same way (`(%i6)` and `(%i7)` above).

**What it leaves alone.** Asymptotic expansions (`asymp`) are excluded: there the constant multiplies negative powers of the kernel, and dropping it is a different, separate defect (queued as its own task). Kernels adjoined during an expansion are excluded through TVAR?: their datum's EXP-PT holds a limit such as `$zeroa` instead of a point. A first version without that check broke `rtest_taylor` problem 226 (bug #4806), an expansion at `inf` that adjoins such kernels.

**Side effect.** With `taylor_truncate_polynomials:false`, `taylor(s,s,a,-1)` gave `s-a`, which is not `s` in any sense, and now gives 0 (`(%i9)`). That is what the zero-point branch already gives for a power beyond the truncation with exact polynomials (`taylor(x^5,x,0,3)` is 0 there too). There is no test for it, since the manual does not say what an exact polynomial gives at a negative order.

**residue().** RESM1 in `src/residu.lisp` expands to order 1 because, its comment says, `residue(s/(s^2-a^2),s,a)` fails at order -1. That was this bug. In an experiment with RESM1 at order -1 (a load file, no source change), the full suite passes with only the usual `rtestprintf` failure, and the example gives `1/2` (0 without this fix). RESM1 is unchanged. Switching it is for the maintainers to decide.

**Checks.** The expected values were checked numerically, independent of `taylor()`: f-S evaluated in 80-digit bigfloats at s = p+h for h = 1e-6, 1e-9, 1e-12 stays bounded (1, 1/12 for a = 3, 3*%e/2 and 1/4 in absolute value), while for the old answer 0 it grows like 1/h. The two-variable and the separate-order cases are exact identities, checked with `ratsimp`.

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

### `src/hayat.lisp`: VAR-EXPAND returns 0 at a negative truncation level

Adds a clause to the COND of VAR-EXPAND, before its multivariable branch.

**Insert** this block:

```lisp
	   ((and (tvar? var2) (not (signp e (exp-pt temp)))
		 (null (switch '$asymp temp)) (not dont-truncate?)
		 (e> (rczero) (current-trunc temp)))
	    ;; Expanded at a point other than 0, VAR2^EXP starts with the
	    ;; constant EXP-PT^EXP, so a negative truncation level leaves
	    ;; nothing of it. The code below would drop only that constant and
	    ;; return a power of the kernel, and TSTIMES would take its order
	    ;; for the order of VAR2^EXP, which is 0. A kernel adjoined during
	    ;; the expansion is not TVAR?, and its EXP-PT is a limit such as
	    ;; $ZEROA rather than a point, so it is left alone.
	    (rczero))
```

**before** this line (it is indented with a tab and three spaces):

```lisp
	   ;; multivar expansion does not work at infinity, so
```

### `tests/rtest_taylor.mac`: new tests

Adds the tests of section 5, after those of the first handover.

**Insert** the block of section 5, followed by two blank lines, **before** this line:

```
/* clean up & checks */
```

## 5. Proposed test cases

**New**, in `tests/rtest_taylor.mac`, before its final clean-up checks and after the tests of the first handover. Replace `MMMM` by the number the bug report gets.

```
/* Bug #MMMM: "taylor(s/(s-1),s,1,-1) gives 0 instead of 1/(s-1)" */

map('ratdisrep,
    [taylor(s/(s-1), s, 1, -1), taylor(s/(s^2-a^2), s, a, -1),
     taylor(s*exp(s)/(s-1)^2, s, 1, -1), taylor(s/(s^2+1), s, %i, -1),
     taylor(s/(s-1), [s], 1, -1), taylor(y*s/(s-1), s, 1, -1, y, 0, 1)]);
[1/(s-1), 1/(2*(s-a)), %e/(s-1)^2+2*%e/(s-1), 1/(2*(s-%i)), 1/(s-1), y/(s-1)]$

ratsimp(ratdisrep(taylor(x*y/((x-1)*(y-2)), [x, y], [1, 2], -1)) - (2/((x-1)*(y-2))+1/(x-1)+2/(y-2)));
0$
```

`run_testsuite(share_tests=true)` on SBCL 2.2.9: 1 failure out of 22,356 tests (`rtestprintf.mac` problem 38, the same float printing failure as the unpatched baseline). `rtest_taylor` passes 253/253, and its 2 new problems both fail on the build without this patch.

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

This fixes bug #MMMM.

AI-Assisted-By: Claude Opus 5.5
```
