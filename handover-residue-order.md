# Handover: residue() expands to order -1

## 1. Summary

**Expand to order -1 in residue(), which is all it needs**

`residue()` expanded to order 1 because `taylor()` to order -1 failed at poles away from 0, as its comment says for `residue(s/(s^2-a^2),s,a)`. Those failures are fixed by commit f0e672f and by the pending taylor fixes, so RESM1 and RESM1-VAR now expand to order -1. This must land after `handover-taylor-cancelling-constants.md` and `handover-taylor-nonzero-point.md`, which fix `taylor()` at order -1. It was tested on top of these two and `handover-taylor-asymp.md`.

## 2. Reproducer/Demo

### Old: master 8c68d35ac with the three taylor fixes (executed)

```
(%i1) display2d:false
(%i2) residue(s/(s^2-a^2),s,a)
(%o2) 1/2
(%i3) residue(cot(z),z,%pi)
(%o3) 1
(%i4) residue(gamma(z),z,-1)
(%o4) -1
(%i5) residue((s*exp(s))/(s-1)^2,s,1)
(%o5) 2*%e
(%i6) residue((z^2*(1+z))/(z^3-z^2+(z-1)*(z+1)+1),z,0)
(%o6) 1
(%i7) residue(coth(1+(z-%pi)^2)/(z-%pi),z,%pi)
(%o7) 1/tanh(1)
(%i8) residue(sech(3+((z-%pi)+1)*((z-%pi)-1))/(z-%pi),z,%pi)
(%o8) 1/cosh(2)
(%i9) integrate(1/(x^4+4),x,minf,inf)
(%o9) %pi/4
```

### New: the same with this change (executed)

```
(%i1) display2d:false
(%i2) residue(s/(s^2-a^2),s,a)
(%o2) 1/2
(%i3) residue(cot(z),z,%pi)
(%o3) 1
(%i4) residue(gamma(z),z,-1)
(%o4) -1
(%i5) residue((s*exp(s))/(s-1)^2,s,1)
(%o5) 2*%e
(%i6) residue((z^2*(1+z))/(z^3-z^2+(z-1)*(z+1)+1),z,0)
(%o6) 1
(%i7) residue(coth(1+(z-%pi)^2)/(z-%pi),z,%pi)
(%o7) coth(1)
(%i8) residue(sech(3+((z-%pi)+1)*((z-%pi)-1))/(z-%pi),z,%pi)
(%o8) sech(2)
(%i9) integrate(1/(x^4+4),x,minf,inf)
(%o9) %pi/4
```

The values are the same. Only the form of `(%o7)` and `(%o8)` changes.

### Internals

RESM1 and RESM1-VAR call `taylor(e, var, pole, 1)` and take the coefficient of `1/(var-pole)` from the result. The expansion to order -1 ends with that term, so two orders of work were spent on terms nobody reads. The comment there gives the reason: order -1 failed for `residue(s/(s^2-a^2),s,a)`. That was the bug of `handover-taylor-nonzero-point.md`.

Applied to master 8c68d35ac without the taylor fixes (as a load file), the change gives 0 in `(%i2)`, `(%i5)` and `(%i6)`. `(%i3)` and `(%i4)` are right there since commit f0e672f, which made `taylor()` keep the pole of `cot`, `tan` or `gamma` at order -1.

The form changes because the last factor is now needed to order 0 only. There `coth(1+(z-%pi)^2)` is the constant `coth(1)`, while to order 2 taylor expands it through `1/tanh`.

**Checks.** On master 8c68d35ac with the three taylor fixes: 900 generated residues (products, quotients and sums of functions with and without poles at 9 points, each judged by a contour sum on a circle of radius 1/8) have the same value with and without this change. 892 are the same expression, 4 differ as above (`coth(1)` for `1/tanh(1)`, `cot(1)` for `1/tan(1)`, `csch(2)` for `1/sinh(2)`, `sech(2)` for `1/cosh(2)`), and 4 are arranged differently with a difference that `ratsimp` reduces to 0. 30 definite integrals of the kinds that use residues give the same expression. A loop of 8 residues and 2 such integrals took 1.22 s on average, against 1.37 s before. On master alone, without the taylor fixes, the change breaks 80 of the 826 generated residues that are right.

## 3. Bug report

**Title:**
```
residue() expands to order 1 where order -1 is enough
```

**Body:**
````markdown
`residue(e, z, p)` takes the coefficient of `1/(z-p)` from `taylor(e, z, p, 1)`. The expansion to order `-1` has that coefficient too and needs two orders less. A comment in `src/residu.lisp` gives the reason for order `1`:

```
;; Things like residue(s/(s^2-a^2),s,a) fails if use -1.
```

That failure is a bug in `taylor`, which gives `0` for `taylor(s/(s^2-a^2), s, a, -1)` (bug #MMMM). Once it is fixed, `residue` can expand to order `-1`. Besides the saved work, some residues then come out in a simpler form:

```
(%i1) display2d:false$
(%i2) residue(coth(1+(z-%pi)^2)/(z-%pi), z, %pi);
(%o2) 1/tanh(1)
```

With order `-1` this is `coth(1)`.
````

This is a clean-up more than a bug. If it goes in without a ticket, drop the "This fixes" line of the commit message and the bug number in the test heading.

## 4. Code patching instructions

### `src/residu.lisp`: RESM1 expands to order -1

Changes the order and the comment.

**Replace** this block:

```lisp
                    ;; Things like residue(s/(s^2-a^2),s,a) fails if use -1.
                    ($taylor e var pole 1))))
```

**with** this block:

```lisp
                    ;; The expansion to order -1 ends with the term whose
                    ;; coefficient is the residue.
                    ($taylor e var pole -1))))
```

### `src/residu.lisp`: RESM1-VAR expands to order -1

Changes the order and the comment.

**Replace** this block:

```lisp
                    ;; Things like residue(s/(s^2-a^2),s,a) fails if use -1.
                    ($taylor e var1 pole 1))))
```

**with** this block:

```lisp
                    ;; The expansion to order -1 ends with the term whose
                    ;; coefficient is the residue.
                    ($taylor e var1 pole -1))))
```

### `tests/rtest_taylor.mac`: new tests

Adds the tests of section 5, after those of the taylor handovers.

**Insert** the block of section 5, followed by two blank lines, **before** this line:

```
/* clean up & checks */
```

## 5. Proposed test cases

**New**, in `tests/rtest_taylor.mac`, before its final clean-up checks and after the tests of the taylor handovers. Replace `QQQQ` by the number the bug report gets. These residues need `taylor()` to work at order -1 now. They pass with order 1 as well, so they guard the taylor fixes more than this change: applied to master without those fixes, the change makes the first, fifth and seventh 0.

```
/* Bug #QQQQ: "residue() expands to order 1 where order -1 is enough" */

[residue(s/(s^2-a^2), s, a), residue(cot(z), z, %pi), residue(tan(z), z, %pi/2), residue(gamma(z), z, -1),
 residue(s*exp(s)/(s-1)^2, s, 1), residue(1/(z^2-2)^2, z, sqrt(2)), residue(z^2*(1+z)/(z^3-z^2+(z-1)*(z+1)+1), z, 0)];
[1/2, 1, -1, -1, 2*%e, -1/2^(7/2), 1]$
```

`run_testsuite(share_tests=true)` on SBCL 2.2.9, master 8c68d35ac with the three taylor fixes and this change: 1 failure out of 22,392 tests (`rtestprintf.mac` problem 38, the same float printing failure as on master). `rtest_taylor` passes 267/267.

## 6. Proposed Git commit message

```
Expand to order -1 in residue()

residue() expanded to order 1, two orders more than it needs, because
taylor() to order -1 failed at poles away from 0, as in
residue(s/(s^2-a^2),s,a). Those taylor() bugs are fixed, so residue()
now expands to order -1.

Residues keep their values. A few change their form, such as coth(1)
for 1/tanh(1), and they are computed a little faster.

This fixes bug #QQQQ.

AI-Assisted-By: Claude Opus 5.5
AI-Assisted-By: Claude Fable 5.2
```
