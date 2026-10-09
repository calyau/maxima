# Fix limit() at a bigfloat point returning ind

## 1. Summary

**`limit()` of a continuous function at a bigfloat point no longer returns
`ind`.** The two one-sided limits are computed in different ways, the left
one by reflecting the variable, so with bigfloats they can differ in the
last bits, or one of them can still contain `%pi`. `BOTH-SIDE` compared
them exactly and returned `ind`. Now it accepts floating-point one-sided
limits that agree up to rounding.

## 2. Reproducer/Demo

Old behavior (HEAD):

```
(%i1) limit(atan(x),x,1.1b0)
(%o1)                                 ind
(%i2) [limit(atan(x),x,1.1b0,plus),limit(atan(x),x,1.1b0,minus)]
(%o2)            [8.329812666744316b-1, 8.329812666744316b-1]
(%i3) limit(acos(x),x,3.0b-1)
rat: replaced 3.141592653589793b0 by 80143857/25510582 = 3.141592653589793b0
(%o3)                                 ind
(%i4) limit(acos(x),x,3.0b-1,minus)
(%o4)                      %pi - 1.875488980810294b0
(%i5) limit(atan(x),x,1.1)
(%o5)                         0.8329812666744317
```

New behavior:

```
(%i1) limit(atan(x),x,1.1b0)
(%o1)                        8.329812666744316b-1
(%i2) [limit(atan(x),x,1.1b0,plus),limit(atan(x),x,1.1b0,minus)]
(%o2)            [8.329812666744316b-1, 8.329812666744316b-1]
(%i3) limit(acos(x),x,3.0b-1)
rat: replaced 3.141592653589793b0 by 80143857/25510582 = 3.141592653589793b0
(%o3)                         1.266103672779499b0
(%i4) limit(acos(x),x,3.0b-1,minus)
(%o4)                      %pi - 1.875488980810294b0
(%i5) limit(atan(x),x,1.1)
(%o5)                         0.8329812666744317
```

Internals: for `atan`, the right side calls `BIG-FLOAT-ATAN` with `1.1b0`,
the left side calls it with `-1.1b0` and negates the result, and the two
mantissas differ in the last bit (`...181` and `...180`). For `acos`, the
left side comes back as `%pi - 1.875488980810294b0`. `MEQP` compares both
exactly.

Over 23 elementary functions at 156 points from `-3.95` to `3.95`, at
`fpprec` 16, 32 and 60, 21 limits returned `ind` before the fix (`acos`
everywhere, `atan` for `abs(x) >= 1.1`), and none after it. The largest
gap between the two sides was 2.8 units in the last place. The new test
allows 32 units, the factor `bfloat_approx_equal()` uses. With doubles no
limit returned `ind` before or after.

## 3. Bug report

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

The two one-sided limits differ in the last bit of the bigfloat, and the two-sided limit treats them as different. The same happens for `acos` at every bigfloat point, for example `limit(acos(x), x, 0.3b0)` returns `ind` while `limit(acos(x), x, 0.3b0, minus)` returns `%pi - 1.875488980810294b0`.
````

## 4. Code patching instructions

### `src/limit.lisp`, accept one-sided limits that agree up to rounding

In `BOTH-SIDE`, **insert** this block

```
		   ;; At a floating-point number, the two sides can differ by rounding.
		   ((float-limits-agree-p ra rb) ra)
```

**after** this line:

```
		   ((or (and (eq ra '$ind) (eq rb '$ind)) (eq t (meqp ra rb))) ra)
```

### `src/limit.lisp`, the comparison

**Insert** this block

```
;; The one-sided limits at a floating-point number are computed in different
;; ways, for example the left one by reflecting the variable. So they can differ
;; by rounding, and one of them can still contain %pi. Return true when A and B
;; are constants with floating-point numbers that agree to within 32 units in
;; the last place of the least precise of these numbers, relative to
;; max(1, abs(A)).
(defun float-limits-agree-p (a b)
  (and (has-float a) (has-float b) ($constantp a) ($constantp b)
       (let ((d (mfuncall '$bfloat (take '(mabs) (sub a b))))
             (m (mfuncall '$bfloat (take '(mabs) a)))
             (bits (min (min-float-bits a) (min-float-bits b))))
         (and ($bfloatp d) ($bfloatp m)
              (eq t (mgqp (mul (power 2 (- 5 bits)) (if (eq t (mgrp m 1)) m 1))
                          d))))))

;; The precision in bits of the least precise floating-point number in E.
(defun min-float-bits (e)
  (cond ((floatp e) (float-digits e))
        (($bfloatp e) (caddar e))
        ((atom e) most-positive-fixnum)
        (t (reduce #'min (mapcar #'min-float-bits (cdr e))
                   :initial-value most-positive-fixnum))))
```

**before** this line:

```
(defun limunknown (e var)
```

## 5. Proposed test cases

`tests/rtest_limit_extra.mac`, **new**, inserted **before** the comment
`/* Did any values, facts, or contexts leak?*/` so that the leak checks
stay last. Only those two checks are renumbered, and they have no
registry entries.

```
/* Bug #NNNN: "limit() at a bigfloat point returns ind for a continuous function" */

[float_approx_equal(limit(atan(x), x, 1.1b0), atan(1.1b0)),
 float_approx_equal(limit(atan(x), x, -1.4b0), atan(-1.4b0)),
 float_approx_equal(limit(acos(x), x, 0.3b0), acos(0.3b0)),
 float_approx_equal(limit(acos(x), x, -0.7b0), acos(-0.7b0)),
 block([fpprec : 32],
   float_approx_equal(limit(atan(x), x, bfloat(11/10)), atan(bfloat(11/10))))];
[true, true, true, true, true]$
```

With the fix, `rtest_limit_extra` passes 475/475. With HEAD's `BOTH-SIDE`
the new test fails. `rtest_limit`, `rtest_limit_gruntz` and
`rtest_limit_wester` pass, and `make check` passes with this and the
`jacobi_sn()` fix (21,671 tests, dependency check included).

With this fix, `rtest_limit_extra` 187 also passes with the bigfloat
square root patch in `handover-profiling-speedups.md`. If this commit goes
in first, that patch needs no expected-failure entry for 187.

## 6. Proposed Git commit message

```
Fix limit() at a bigfloat point returning ind

limit() of a continuous function at a bigfloat point could return ind,
for example limit(atan(x), x, 1.1b0) and limit(acos(x), x, 0.3b0).
The two one-sided limits are computed in different ways, so with
bigfloats they can differ in the last bits, or one of them can still
contain %pi, and limit() compared them exactly. Now one-sided limits
that agree up to rounding count as equal.

This fixes bug #NNNN.

AI-Assisted-By: Claude Opus 5.5
```
