# Handover: skip the cubic and quartic formulas in DINTEXP's change of variable

## 1. Summary

**`integrate()` no longer spends minutes, or the whole heap, on definite
integrals of `exp(p(x))` with a cubic or quartic `p`.** `DINTEXP` substitutes
`y = exp(p(x))` and inverts it with `solve`, which for such `p` answers with
the cubic or quartic formula. The integrand rewritten in those radicals then
takes seconds to minutes to simplify, or exhausts the heap, and the integral
fails anyway. `INTCV` now skips roots with nested radicals when `DINTEXP` asks
it to, which makes `rtest_integrate` about 6 s (a third) faster.

## 2. Reproducer/Demo

Run as batch files with `./maxima-local --no-init` (SBCL 2.6.9), each block in
its own process. The lines of the `batch()` call itself are left out.

**Old** (master `6f25daa24`):

```
(%i2) display2d:false
(%i3) showtime:true
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i4) integrate(exp(-(1-x)^2-x^4),x,minf,inf)
Evaluation took 5.6918 seconds (5.8240 elapsed) using 487.742 MB.
(%o4) 'integrate(%e^(-x^4-(1-x)^2),x,minf,inf)
(%i5) integrate(exp(-x^3-x),x,0,inf)
Evaluation took 97.0415 seconds (98.3000 elapsed) using 110260.136 MB.
(%o5) 'integrate(%e^(-x^3-x),x,0,inf)
```

```
(%i2) display2d:false
(%i3) showtime:true
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i4) integrate(exp(-x^4-x),x,0,inf)
Heap exhausted during garbage collection: 0 bytes available, 16 requested.
[... GC table ...]
fatal error encountered in SBCL pid 409 tid 409:
Heap exhausted, game over.
[... backtrace ...]
  22: fp=0x7f0400f26c60 pc=0xb8006fbc03 MAXIMA::REAL-ROOTS
  23: fp=0x7f0400f26cc0 pc=0xb8006f9a6a MAXIMA::POLES-IN-INTERVAL
  24: fp=0x7f0400f26cf8 pc=0xb8006d2574 MAXIMA::INITIAL-ANALYSIS
  25: fp=0x7f0400f26d78 pc=0xb8006d15c4 MAXIMA::DEFINT
  26: fp=0x7f0400f26dc8 pc=0xb8006d0038 MAXIMA::INTCV
  27: fp=0x7f0400f26e00 pc=0xb8006f3de4 MAXIMA::DINTEXP
[...]
```

The second session dies: SBCL exits, and everything in the Maxima session is
lost.

**New** (same master with the patch below):

```
(%i2) display2d:false
(%i3) showtime:true
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i4) integrate(exp(-(1-x)^2-x^4),x,minf,inf)
Evaluation took 0.0350 seconds (0.0320 elapsed) using 5.247 MB.
(%o4) 'integrate(%e^(-x^4-(1-x)^2),x,minf,inf)
(%i5) integrate(exp(-x^3-x),x,0,inf)
Evaluation took 0.0125 seconds (0.0160 elapsed) using 1.843 MB.
(%o5) 'integrate(%e^(-x^3-x),x,0,inf)
```

```
(%i2) display2d:false
(%i3) showtime:true
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i4) integrate(exp(-x^4-x),x,0,inf)
Evaluation took 0.0257 seconds (0.0280 elapsed) using 5.184 MB.
(%o4) 'integrate(%e^(-x^4-x),x,0,inf)
```

**Internals.** `DINTEGRATE` tries `DINTEXP` before the antiderivative for any
integrand with `exp` in it. When the integrand is a function of `exp(p(x))`
alone, `FUNCLOGOR%E` returns `exp(p(x))`, and `INTCV` solves
`yx = exp(p(x))` for `x`. For `p = -x^4-(1-x)^2` (the test for bug #3781) the
first root it uses is Ferrari's quartic formula, `INTCV2` ratsimps the
integrand rewritten in it, and `DEFINT` then looks for poles of the result
(`POLES-IN-INTERVAL`, `REAL-ROOTS`), which is where the heap runs out for
`exp(-x^4-x)`. The new integrand still contains the derivative of the inverse,
so these radicals cannot cancel, and the integral fails after all. Over the full
test suite `INTCV` is called 65 times. With the patch every call ends as
before, but the failing `DINTEXP` calls take 0.03 s instead of 4.9 s.

`LOGX1` uses `INTCV` too, for `y = p(x)` with `log(p(x))` in the integrand,
and there nested radicals in the inverse do work: for
`log(p(x))*p'(x)*h(p(x))`, `p'(x)` cancels the derivative of the inverse.
The second group of tests in section 5 has four such integrals (one inverts a
cubic with the cubic formula). So `LOGX1` does not pass the new argument.

## 3. Bug report

**Title:**
```
integrate(exp(p(x)),x,a,b) with a cubic or quartic p takes minutes or exhausts the heap
```

**Body:**
````markdown
Definite integrals of `exp(p(x))` where `p` is a cubic or quartic polynomial take a long time and then return the noun form. Some of them crash Maxima. With SBCL 2.6.9:

```
(%i1) display2d : false$
(%i2) showtime : true$
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i3) integrate(exp(-x^3-x), x, 0, inf);
Evaluation took 97.0415 seconds (98.3000 elapsed) using 110260.136 MB.
(%o3) 'integrate(%e^(-x^3-x),x,0,inf)
```

`integrate(exp(-x^3-x^2), x, 0, inf)` does not finish within 4 minutes, and `integrate(exp(-x^4-x), x, 0, inf)` ends with `Heap exhausted, game over.`, which kills the whole session. The test for bug #3781 in `rtest_integrate.mac`, `integrate(exp(-(1-x)^2-x^4), x, minf, inf)`, takes 5.7 s and 488 MB, a third of the time of the whole file.

Maxima cannot do these integrals in closed form, so the noun form is the right answer, but it should come at once. The time goes into a change of variable `y = exp(p(x))` whose inverse is the cubic or quartic formula, and the integral rewritten in those radicals cannot be done either.
````

## 4. Code patching instructions

All three hunks are in `src/defint.lisp`.

### 4.1 `src/defint.lisp`

New helper `NESTED-RADICAL-P`, and the new optional argument of `INTCV`.

**Replace** this block:

```lisp
;; integration change of variable
(defun intcv (nv flag ivar ll ul)
```

**with:**

```lisp
;; True if E contains a radical in VAR, a power of an expression in VAR to a
;; non-integer rational exponent, whose base contains another such radical.
(defun nested-radical-p (e var)
  (labels ((radical-p (e)
             (and (mexptp e) (ratnump (caddr e)) (among var (cadr e))))
           (has-radical-p (e)
             (and (consp e) (consp (car e))
                  (or (radical-p e) (some #'has-radical-p (cdr e)))))
           (nested-p (e)
             (and (consp e) (consp (car e))
                  (or (and (radical-p e) (has-radical-p (cadr e)))
                      (some #'nested-p (cdr e))))))
    (nested-p e)))

;; integration change of variable
;;
;; With SKIP-NESTED, roots with nested radicals in 'YX, which the cubic and
;; quartic formulas give, are not used. DINTEXP passes it: its integrand is
;; a function of NV alone, so nothing in it cancels such radicals, and the
;; new integrand is too big to integrate. LOGX1 does not: its integrand can
;; contain the derivative of NV, which cancels them.
(defun intcv (nv flag ivar ll ul &optional skip-nested)
```

### 4.2 `src/defint.lisp`

`INTCV` passes over roots with nested radicals when `SKIP-NESTED` is true.

**Replace** this line (in `INTCV`):

```lisp
				    (if (and (or (real-infinityp ll)
```

**with:**

```lisp
				    (if (and (not (and skip-nested
						       (nested-radical-p root 'yx)))
					     (or (real-infinityp ll)
```

### 4.3 `src/defint.lisp`

`DINTEXP` asks for it.

**Replace** this line (at the end of `DINTEXP`):

```lisp
	   (intcv ans nil ivar ll ul)))))
```

**with:**

```lisp
	   (intcv ans nil ivar ll ul t)))))
```

## 5. Proposed test cases

`tests/rtest_integrate.mac`, **new**, before the block marked "Leave this at
the end of the file!":

```
/* integrate(exp(p(x)), x, a, b) with a cubic or quartic p took minutes
   or exhausted the heap */

[integrate(exp(-x^3-x), x, 0, inf),
  integrate(exp(-x^3-x^2), x, 0, inf),
  integrate(exp(-x^4-x), x, 0, inf)];
['integrate(%e^(-x^3-x), x, 0, inf),
  'integrate(%e^(-x^3-x^2), x, 0, inf),
  'integrate(%e^(-x^4-x), x, 0, inf)]$

/* The change of variable y = p(x) for log(p(x)) still works when the
   inverse of p has nested radicals */

[integrate(log(x^4-2*x^2+1)*(4*x^3-4*x)/sqrt((x^4-2*x^2+1)*(1-(x^4-2*x^2+1))), x, 0, 1),
  integrate(log(x^6+2*x^3)*(6*x^5+6*x^2)/(1+x^6+2*x^3), x, 0, 1),
  integrate(log(x^3+x)*(3*x^2+1)/(1+(x^3+x)^2), x, 0, inf)];
[2*%pi*log(2), log(3)*log(4)+li[2](-3), 0]$
```

The first test takes minutes on master, and its third integral exhausts the
heap, which ends the test run. The second test passes on master too. It is
there because a guard on every use of `INTCV`, not only `DINTEXP`'s, breaks
all three of its integrals (the first returns the noun form instead of
`2*%pi*log(2)`). `quad_qags` agrees with `2*%pi*log(2)` to 9 digits.

Both tests run in place, after the file's own settings (`domain : complex`,
`radexpand : true`): `rtest_integrate` 972/972 with the 2 registered expected
errors. The full suite (`share_tests=true`) passes, 21,676 tests.

## 6. Proposed Git commit message

```
Speed up integrate() of exp(cubic or quartic)

For a definite integral of a function of exp(p(x)), integrate() tries
the substitution y = exp(p(x)), which needs the inverse of p. When p is
a cubic or quartic, that inverse is the cubic or quartic formula.
Rewriting the integrand in it took seconds to minutes, or exhausted the
heap, and then the integral failed anyway. Now such inverses are not
used for this substitution. The substitution for log(p(x)) still uses
them, because there they can lead to a result.

For example, integrate(exp(-x^3-x), x, 0, inf) took 97 s, and
integrate(exp(-x^4-x), x, 0, inf) crashed Maxima. Both now return the
noun form at once. The test for bug #3781 in rtest_integrate runs 5 s
faster.

This fixes bug #NNNN.

AI-Assisted-By: Claude Opus 5.5
```

`#NNNN` is the number the report in section 3 gets.
