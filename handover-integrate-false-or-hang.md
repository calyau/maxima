# Handover: integrate() giving false or never returning

## 1. Summary

**Fix `integrate()` returning `false`, or running forever, for some square-root integrands**

`CASE1` in `src/irinte.lisp` integrates `sqrt(c*x^2+b*x)/x^m` for a whole list of powers `m`. After doing `m = 1` or `m = 2` it went on to the next power `m >= 3` with `M1` still set for the earlier one: with `m = 2` first it gave up and returned NIL, which `INTIRA` multiplied into the answer (`(3*false)/25`), and with `m = 1` first it looped forever. Moving one `(setq m1 ...)` above the dispatch fixes both, and the reported antiderivative now matches `quad_qags` on intervals across `-3/5 < x <= 1/2`.

## 2. Reproducer/Demo

### Old behavior

Executed with SBCL 2.2.9 on a build of the unmodified HEAD (`45469d8`):

```
(%i1) display2d:false
(%i2) F:integrate(((2+3*x)*sqrt(1-2*x))/(3+5*x)^(5/2),x)
(%o2) 2*((4*sqrt(-10*x^2-x+3))/(825*x+495)
        -(2*sqrt(-10*x^2-x+3))/(375*x^2+450*x+135))
 +(3*false)/25
(%i3) G:subst(0,false,F)
(%i4) float(subst(x = 0.4,G)-subst(x = 0,G))
(%o4) 0.02235758663392428
(%i5) quad_qags(((2+3*x)*sqrt(1-2*x))/(3+5*x)^(5/2),x,0,0.4)
(%o5) [0.026983453908954597,3.3999571366179126e-15,21,0]
(%i6) integrate(((x+2)*sqrt(1-x))/(x+1)^(5/2),x)
(%o6) 'integrate((sqrt(1-x)*x)/(sqrt(x+1)*(x^2+2*x+1)),x)
 +2*(sqrt(1-x^2)/(3*x+3)-(2*sqrt(1-x^2))/(3*x^2+6*x+3))
```

A separate process, killed by `timeout 60`:

```
(%i1) display2d:false
(%i2) integrate(((x^2+1)*sqrt(x^2+x))/x^3,x)
Terminated
```

### New behavior

Executed with the patched build:

```
(%i1) display2d:false
(%i2) F:integrate(((2+3*x)*sqrt(1-2*x))/(3+5*x)^(5/2),x)
(%o2) (-3*sqrt(2)*asin((20*x)/11+1/11))/5^(5/2)
 -(2*sqrt(-10*x^2-x+3))/(1875*x^2+2250*x+675)
 -(194*sqrt(-10*x^2-x+3))/(4125*x+2475)
(%i3) float(subst(x = 0.4,F)-subst(x = 0,F))
(%o3) 0.02698345390895464
(%i4) quad_qags(((2+3*x)*sqrt(1-2*x))/(3+5*x)^(5/2),x,0,0.4)
(%o4) [0.026983453908954597,3.3999571366179126e-15,21,0]
(%i5) integrate(((x+2)*sqrt(1-x))/(x+1)^(5/2),x)
(%o5) -asin(x)-(2*sqrt(1-x^2))/(3*x^2+6*x+3)-(5*sqrt(1-x^2))/(3*x+3)
(%i6) integrate(((x^2+1)*sqrt(x^2+x))/x^3,x)
(%o6) log(2*sqrt(x^2+x)+2*x+1)/2-(2*sqrt(x^2+x))/(3*x)-(2*sqrt(x^2+x))/(3*x^2)
                                +sqrt(x^2+x)
```

`F(b)-F(a)` against `quad_qags` for the reported integrand, all with flag 0:

| interval | `F(b)-F(a)` | `quad_qags` | difference |
|---|---|---|---|
| [0, 0.4] | 0.02698345390895464 | 0.026983453908954597 | 4.5e-17 |
| [-0.5, 0] | 0.3564384268893435 | 0.3564384268893437 | 1.7e-16 |
| [-0.55, 0.45] | 0.7843219039911886 | 0.7843219039911964 | 7.8e-15 |
| [-0.3, 0.5] | 0.10619739576478854 | 0.10619739576478837 | 1.7e-16 |
| [0.1, 0.2] | 0.007563081517242385 | 0.007563081517242411 | 2.6e-17 |
| [-0.59, -0.58] | 2.7360528134212445 | 2.736052813419095 | 2.1e-12 |

### Internals

The integrand reaches `INTI`, which substitutes `y = 5*x+3`. The radicand becomes `-2/5*y^2+11/5*y`, a quadratic without constant term, so `INTIRA` goes through `DENMNUMN` and `NONCONSTQUADENUM` to `CASE1`. For the `3*x` part of the integrand the power list is `((2 1) (3 -3))`, that is `1/y^2-3/y^3`.

`CASE1` does `m = 2` with a direct formula, then jumps to the general recurrence for `m = 3`. That recurrence saves an intermediate result when it reaches `M1 = m-2` and finishes one step later, but `M1` was only updated on the path that follows an earlier `m >= 3`, so it still held 0 from `m = 2`. The recurrence then found no saved intermediate result and returned NIL. After `m = 1` first, `M1` is -1, which the recurrence never reaches, so it loops forever.

The NIL turns into `false` in `INTIRA`, which returns `(augmult (mul d (denmnumn ...)))` without checking for NIL. With `D = 1/5` this gives `false/5`, `INTI` scales it by `1/5`, and the caller by 3. With `D = 1` the NIL survives as NIL and `integrate()` falls back to a noun form for that part, which is why `(x+2)*sqrt(1-x)/(x+1)^(5/2)` came back partly unevaluated.

This is the kind of leak `6413ce0` fixed in `defint.lisp`, where a NIL meaning "can't integrate" was added into a sum, here reached through a product in `INTIRA`. This time, though, the NIL was itself a bug, since `CASE1` can do these integrals. With `CASE1` fixed I found no other input that gets a NIL to the multiplication in `INTIRA`, so I left `INTIRA` alone.

## 3. Bug report

**Title:**
```
integrate((2+3*x)*sqrt(1-2*x)/(3+5*x)^(5/2), x) contains false
```

**Body:**
````markdown
The indefinite integral `integrate((2+3*x)*sqrt(1-2*x)/(3+5*x)^(5/2), x)` (Rubi test suite 1.1.1.3, #2309) returns a result containing `false`. No sign questions are asked.

```
(%i1) display2d : false$
(%i2) F : integrate((2+3*x)*sqrt(1-2*x)/(3+5*x)^(5/2), x);
(%o2) 2*((4*sqrt(-10*x^2-x+3))/(825*x+495)
        -(2*sqrt(-10*x^2-x+3))/(375*x^2+450*x+135))
 +(3*false)/25
```

The rest of the result is wrong as well. With `false` dropped, `F(0.4)-F(0)` is `0.02236`, but the integral from `0` to `0.4` is `0.02698`:

```
(%i3) G : subst(0, false, F)$
(%i4) float(subst(x = 0.4, G) - subst(x = 0, G));
(%o4) 0.02235758663392428
(%i5) quad_qags((2+3*x)*sqrt(1-2*x)/(3+5*x)^(5/2), x, 0, 0.4);
(%o5) [0.026983453908954597,3.3999571366179126e-15,21,0]
```

Similar integrals are partly returned unevaluated, or never return:

```
(%i6) integrate((x+2)*sqrt(1-x)/(x+1)^(5/2), x);
(%o6) 'integrate((sqrt(1-x)*x)/(sqrt(x+1)*(x^2+2*x+1)),x)
 +2*(sqrt(1-x^2)/(3*x+3)-(2*sqrt(1-x^2))/(3*x^2+6*x+3))
(%i7) integrate((x^2+1)*sqrt(x^2+x)/x^3, x);
```

All three have elementary antiderivatives, such as `log(2*sqrt(x^2+x)+2*x+1)/2-2*sqrt(x^2+x)/(3*x)-2*sqrt(x^2+x)/(3*x^2)+sqrt(x^2+x)` for the last one.
````

Replace `NNNN` in the test comment (section 5) and the commit message (section 6) with the number this report gets.

## 4. Code patching instructions

### `src/irinte.lisp`

In `CASE1`, update `M1` for the new power before dispatching on `COUNT`.

**Replace** this block (seven spaces of indentation, no tabs):

```lisp
       (when (= count 5) (go jump5))
       (when (= count 1) (go jump1))
       (when (= count 2) (go jump2))
       (when (= count 3) (go jump3))
       (setq m1 (- controlpow 2))
```

with:

```lisp
       (setq m1 (- controlpow 2))
       (when (= count 5) (go jump5))
       (when (= count 1) (go jump1))
       (when (= count 2) (go jump2))
       (when (= count 3) (go jump3))
```

### `tests/rtest_integrate.mac`

Add the regression tests.

**Insert** the test block from section 5, followed by two blank lines, **before** this block:

```
/**************************************/
/* Leave this at the end of the file! */
/**************************************/
```

## 5. Proposed test cases

New, in `tests/rtest_integrate.mac` (see section 4 for the position):

```
/* Bug #NNNN: "integrate((2+3*x)*sqrt(1-2*x)/(3+5*x)^(5/2), x) contains false" */

/* The second integral was partly returned unevaluated, the third never returned: */
[integrate((2+3*x)*sqrt(1-2*x)/(3+5*x)^(5/2), x), integrate((x+2)*sqrt(1-x)/(x+1)^(5/2), x),
 integrate((x^2+1)*sqrt(x^2+x)/x^3, x)];
[-3*sqrt(2)*asin(20*x/11+1/11)/5^(5/2)-2*sqrt(-10*x^2-x+3)/(1875*x^2+2250*x+675)-194*sqrt(-10*x^2-x+3)/(4125*x+2475),
 -asin(x)-2*sqrt(1-x^2)/(3*x^2+6*x+3)-5*sqrt(1-x^2)/(3*x+3),
 log(2*sqrt(x^2+x)+2*x+1)/2-2*sqrt(x^2+x)/(3*x)-2*sqrt(x^2+x)/(3*x^2)+sqrt(x^2+x)]$

/* Compare with quad_qags on intervals where the integrands are real: */
map(lambda([f, ivs],
  block([F : integrate(f, x)],
    map(lambda([iv],
      is(abs(float(rectform(subst(x = iv[2], F) - subst(x = iv[1], F))) - quad_qags(f, x, iv[1], iv[2])[1]) < 1e-10)),
      ivs))),
  [(2+3*x)*sqrt(1-2*x)/(3+5*x)^(5/2), (x+2)*sqrt(1-x)/(x+1)^(5/2), (x^2+1)*sqrt(x^2+x)/x^3],
  [[[-1/2, 0], [0, 2/5], [-11/20, 1/2]], [[-1/2, 9/10], [0, 1/2]], [[-3, -3/2], [1, 2]]]);
[[true, true, true], [true, true], [true, true]]$
```

The intervals lie inside the real domains (`-3/5 < x <= 1/2`, `-1 < x <= 1`, and `x <= -1` or `x > 0`), and include negative `x` for each integrand.

Results, all with SBCL 2.2.9:

- `run_testsuite(tests=["rtest_integrate"])`: 1033/1033 passed, not counting 2 expected errors (1031/1031 before). With the old `CASE1` loaded at runtime instead, the run never finishes: it hangs in the first new problem.
- `run_testsuite(share_tests=true)`: 1 failure out of 22,216 tests (22,214 before), `share/stringproc/rtestprintf.mac` problem 38, which fails the same way before the patch (SBCL 2.2.9 prints `~e` floats differently). Every other file gives the same counts as before.
- `tests/depcheck.sh`: exit 0 before and after.
- Not part of the patch: `(2+3*x)^j*(1-2*x)^h/(3+5*x)^k` for `j = 0..3`, `h = 1/2, 3/2` and `k = 1/2, ..., 9/2`, 40 integrands. Before, 3 gave `false` and 3 never returned. Now all 40 are integrated, and `F(b)-F(a)` agrees with `quad_qags` on `[-1/2, 0]`, `[0, 2/5]` and `[-11/20, 1/2]`. The same holds for `p*sqrt(R)/x^m` with 6 polynomials `p`, `m = 1..5` and `R` in `x^2+x`, `x-x^2`, `2*x^2-3*x` (150 intervals).

## 6. Proposed Git commit message

```
Fix integrate() giving false or never returning

Some integrals with square roots, such as
integrate((2+3*x)*sqrt(1-2*x)/(3+5*x)^(5/2), x), returned a result
containing false that was numerically wrong as well. Similar ones were
partly left unevaluated or never returned, such as
integrate((x^2+1)*sqrt(x^2+x)/x^3, x).

These integrals lead to sqrt(c*x^2+b*x)/x^m for several powers m at
once. The helper that does this (CASE1) mixed up the powers when
m = 1 or m = 2 came first, so it gave up or ran forever. Now it uses
the right power.

This fixes bug #NNNN.

AI-Assisted-By: Claude Opus 5.5
```
