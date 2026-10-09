# Handover: five speed-ups for sign(), limit() and nonscalarp()

## 1. Summary

**Faster `sign()`, `limit()` and `nonscalarp()` by not repeating work.**
Five independent changes from the second profiling round on master
`eec2fa780` (`profiling/README.md`, "Round 2"). None changes a result.
Together they make the core suite 10.9% and the full suite 10.3% faster.

| item | change | hunks | tests |
|------|--------|-------|-------|
| 1 | `SIGNDIFF-SPECIAL`: cheap sign tests before `EXPT-OF-BASE` | 4.4, 4.5 | `rtest_sign.mac` |
| 2 | `SIGN-MABS`: no repeated `csign` when ratsimp changes nothing | 4.1–4.3, 4.7 | `rtest_sign.mac` |
| 3 | `SIGN-LOG`: each comparison with 1 once | 4.6 | `rtest_sign.mac` |
| 4 | limit cache looked up by hash code | 4.8, 4.9 | existing limit tests |
| 5 | `SCALARCLASS` without re-walking constant subtrees | 4.10–4.16 | `rtest_scalarp.mac` |

## 2. Reproducer/Demo

Run as a batch file with `./maxima-local` (SBCL 2.6.9). The `tlimit()` call
gains from items 1–3, the `limit()` call from item 4, `nonscalarp()` from
item 5.

**Old** (master `eec2fa780`):

```
(%i2) showtime:true
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i3) tlimit((asin(x+h)-asin(x))/h,h,0)
Evaluation took 0.5667 seconds (0.5720 elapsed) using 310.789 MB.
                                            2
                                  sqrt(1 - x )
(%o3)                           - ────────────
                                      2
                                     x  - 1
(%i4) limit(exp(gamma(x-exp(-x))*exp(1/x))-exp(gamma(x)),x,inf)
Evaluation took 2.1167 seconds (2.1360 elapsed) using 349.520 MB.
(%o4)                                  0
(%i5) e:1
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i6) for i thru 2000 do e:sqrt(e+1)
Evaluation took 0.0032 seconds (0.0040 elapsed) using 1.280 MB.
(%i7) nonscalarp(e)
Evaluation took 0.6572 seconds (0.6640 elapsed) using 0 bytes.
(%o7)                                false
```

**New** (with the patch):

```
(%i2) showtime:true
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i3) tlimit((asin(x+h)-asin(x))/h,h,0)
Evaluation took 0.2682 seconds (0.2680 elapsed) using 161.564 MB.
                                            2
                                  sqrt(1 - x )
(%o3)                           - ────────────
                                      2
                                     x  - 1
(%i4) limit(exp(gamma(x-exp(-x))*exp(1/x))-exp(gamma(x)),x,inf)
Evaluation took 0.9020 seconds (0.9080 elapsed) using 349.653 MB.
(%o4)                                  0
(%i5) e:1
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i6) for i thru 2000 do e:sqrt(e+1)
Evaluation took 0.0059 seconds (0.0080 elapsed) using 1.287 MB.
(%i7) nonscalarp(e)
Evaluation took 0.0012 seconds (0.0000 elapsed) using 0 bytes.
(%o7)                                false
```

With depth 1000 instead of 2000, the old `nonscalarp(e)` takes 0.17 s, a
quarter of the time: it is quadratic in the depth.

**Internals.**

- Item 1: in `rtest_limit_extra`, `rtest_trig`, `rtest_limit`, `rtest16` and
  `rtest_integrate`, `EXPT-OF-BASE` ran 200,384 times, each time a `MEQP`
  (ratsimp and `csign` of a difference). In the `Q^m - Q^n` rule `Q` was not
  known to be positive in 82,800 of 89,800 cases, so both calls there were
  wasted. With the patch, 7,823 calls.
- Item 2: `sign(abs(e))` calls `mnqp(0, e)` when `sign(e)` is `pnz`. The full
  suite makes 132,116 such calls, and none succeeds. In 98% of them (counted
  on four test files) `sratsimp(-e)` is `-e` again, so the `csign` only
  repeats the `sign` that just failed. The new optional argument of `MEQP`
  and `MNQP` keeps the old behavior for all other callers. With it false,
  `MEQP` still makes all its cheap checks (structure, fact database), so
  `sign(abs(x+y))` stays `pos` under `assume(notequal(x+y, 0))`, and
  `sign(abs((x+1)^2-x^2-2*x+y^2))` stays `pos` because ratsimp turns the
  argument into `y^2+1`. This item is not strictly equivalent: `$csign`
  rebinds `limitp` and `factored`, so in principle it could decide where the
  `sign` just before it did not. That never happened in either suite.
- Item 3: for a positive `arg`, `SIGN-LOG` tried `mgrp(1, arg)`,
  `meqp(arg, 1)`, `mgqp(1, arg)`, `mgrp(arg, 1)`, `mgqp(arg, 1)` and
  `mnqp(arg, 1)`, that is `csign(1 - arg)`, `meqp(arg, 1)` and
  `csign(arg - 1)` twice each. Each is now computed once, under the condition
  of its first use, so the result is the same.
- Item 4: `limit-answers` was searched with `ALIKE1` on every key, 3.0% of the
  core suite. Each entry now carries `ALIKE1-HASH` of its key, which ignores
  header flags, so `ALIKE1`-equal keys get equal codes.
- Item 5: `CONSTTERMP` ran `$constantp` over a term and then `SCALARCLASS` on
  it, which ran `CONSTTERMP` on each argument again, and so on down. Problem
  36 of `rtest_cholesky` made 36.6 million `$constantp` calls, 5.4 million
  with the patch. When `$constantp` of an expression is known to be true, so
  is `$constantp` of its arguments (that is how `$constantp` is defined), and
  the new optional argument of `SCALARCLASS` says so.

**Measurements**, CPU time, three interleaved rounds after a discarded
warm-up, pinned to one CPU. The single items were measured as runtime
redefinitions in the unpatched build, the whole suites with the patched build
against the same build with the old definitions loaded.

| change | measured on | baseline | change |
|--------|-------------|----------|--------|
| item 1 | `rtest_limit_extra`, `rtest_trig`, `rtest_limit`, `rtest16`, `rtest_integrate` | 32.1 s | −2.6 ± 0.1 s (−8.1%) |
| items 1–3 | same | 31.2 s | −4.2 ± 0.3 s (−13.3%) |
| item 4 | `rtest_limit_extra`, `rtest_limit_gruntz`, `rtest_limit` | 17.2 s | −1.5 ± 0.3 s (−8.9%) |
| item 5 | `rtest_cholesky`, `rtest_matrixexp` | 12.1 s | −1.9 ± 0.3 s (−15.9%) |
| all five | core suite | 55.9 s | −6.1 ± 0.2 s (−10.9%) |
| all five | full suite | 111.3 s | −11.5 ± 0.8 s (−10.3%) |

## 3. Bug report

**Title:**

```
sign(), limit() and nonscalarp() repeat expensive work
```

**Body:**

````markdown
Profiling the test suites (SBCL 2.6.9) shows that `sign()`, `limit()` and `nonscalarp()` spend much of their time repeating expensive computations. Three examples, with master at `eec2fa780`:

```
(%i2) showtime:true
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i3) tlimit((asin(x+h)-asin(x))/h,h,0)
Evaluation took 0.5667 seconds (0.5720 elapsed) using 310.789 MB.
                                            2
                                  sqrt(1 - x )
(%o3)                           - ────────────
                                      2
                                     x  - 1
(%i4) limit(exp(gamma(x-exp(-x))*exp(1/x))-exp(gamma(x)),x,inf)
Evaluation took 2.1167 seconds (2.1360 elapsed) using 349.520 MB.
(%o4)                                  0
(%i5) e:1
Evaluation took 0.0000 seconds (0.0000 elapsed) using 0 bytes.
(%i6) for i thru 2000 do e:sqrt(e+1)
Evaluation took 0.0032 seconds (0.0040 elapsed) using 1.280 MB.
(%i7) nonscalarp(e)
Evaluation took 0.6572 seconds (0.6640 elapsed) using 0 bytes.
(%o7)                                false
```

`nonscalarp()` is quadratic in the nesting depth of a constant expression: with depth 1000 instead of 2000 it takes `0.17` s, a quarter of the time. Each level checks the whole expression below it for constness again.

`sign()` of a difference like `x^2 - x` first tests whether one side is a power of the other, an expensive equality test, and only then whether the base is positive, which it mostly is not known to be. `sign(log(x))` computes the same comparisons of `x` with `1` twice, and `sign(abs(x))` computes the sign of `x` a second time when the first attempt found nothing. `limit()` looks up the cached answers to its subproblems by comparing the new subproblem with every cached one.

None of this changes results, but together it is about a tenth of the time of the test suites.
````

## 4. Code patching instructions

Each item can be applied on its own, see the table in section 1. Indentation
uses tabs where the original does, so copy the blocks as they are.

### 4.1 `src/compar.lisp` (item 2)

`MEQP` takes an optional `csign`, `t` by default.

**Replace** this block:

```lisp
(defun meqp (a b)
  ;; Check for some particular types before falling into the general case.
```

**with:**

```lisp
(defun meqp (a b &optional (csign t))
  ;; Check for some particular types before falling into the general case.
  ;; With CSIGN false, the final csign test is made only if ratsimp and the
  ;; equality facts rewrite A - B (see SIGN-MABS).
```

### 4.2 `src/compar.lisp` (item 2)

Without `csign`, skip the final `csign` test if ratsimp changed nothing.

**Replace** this block:

```lisp
		 (t (meqp-by-csign (equal-facts-simp (sratsimp (sub a b))) a b)))))))
```

**with:**

```lisp
		 (t (let* ((d (sub a b))
			   (z (equal-facts-simp (sratsimp d))))
		      (if (or csign (not (alike1 z d)))
			  (meqp-by-csign z a b)
			  `(($equal) ,a ,b)))))))))
```

### 4.3 `src/compar.lisp` (item 2)

`MNQP` takes `csign` too and passes it on.

**Replace** this block:

```lisp
(defun mnqp (x y)
  (let ((b (meqp x y)))
```

**with:**

```lisp
(defun mnqp (x y &optional (csign t))
  (let ((b (meqp x y csign)))
```

### 4.4 `src/compar.lisp` (item 1)

`SIGNDIFF-SPECIAL`, `Q^R - S`: the `EXPT-OF-BASE` guard after the sign tests.

**Replace** this block:

```lisp
			;; Do NOT apply when Spos is itself a power of Qpos (e.g. S = Q):
			;; The reduction would just toggle the exponent R <-> 1/R and recurse forever.
			;; That same-base case is handled by the exponent-comparison rule further below.
			(not (expt-of-base xrhs (cadr xlhs)))
			(eq (sign* (cadr xlhs)) '$pos)
			(eq (sign* xrhs) '$pos)
```

**with:**

```lisp
			(eq (sign* (cadr xlhs)) '$pos)
			(eq (sign* xrhs) '$pos)
			;; Do NOT apply when Spos is itself a power of Qpos (e.g. S = Q):
			;; The reduction would just toggle the exponent R <-> 1/R and recurse forever.
			;; That same-base case is handled by the exponent-comparison rule further below.
			;; EXPT-OF-BASE costs a MEQP, so it comes after the sign tests.
			(not (expt-of-base xrhs (cadr xlhs)))
```

### 4.5 `src/compar.lisp` (item 1)

`SIGNDIFF-SPECIAL`, `Q^m - Q^n`: compare `Q` with 1 before `EXPT-OF-BASE`.

**Replace** this block:

```lisp
          (let ((m (expt-of-base xlhs q))
                (n (expt-of-base xrhs q)))
            (when (and m n
                       (zerop1 ($imagpart m))
                       (zerop1 ($imagpart n)))
              (let* ((qcmp (and (eq (sign* q) '$pos) (sign* (sub q 1))))
                     (diff-sign (cond ((eq qcmp '$pos) (sign* (sub m n)))
                                      ((eq qcmp '$neg) (sign* (sub n m)))
                                      (t '$pnz))))
                (unless (eq diff-sign '$pnz)
                  (setq sgn diff-sign))))))))
```

**with:**

```lisp
          ;; Compare Q with 1 first, as each EXPT-OF-BASE costs a MEQP.
          (let ((qcmp (and (eq (sign* q) '$pos) (sign* (sub q 1)))))
            (when (member qcmp '($pos $neg))
              (let ((m (expt-of-base xlhs q))
                    (n (expt-of-base xrhs q)))
                (when (and m n
                           (zerop1 ($imagpart m))
                           (zerop1 ($imagpart n)))
                  (let ((diff-sign (if (eq qcmp '$pos)
                                       (sign* (sub m n))
                                       (sign* (sub n m)))))
                    (unless (eq diff-sign '$pnz)
                      (setq sgn diff-sign))))))))))
```

### 4.6 `src/compar.lisp` (item 3)

`SIGN-LOG` computes each comparison of `arg` with 1 once.

**Replace** this block:

```lisp
	       (cond ((eq t (mgrp 1 arg)) '$neg)
		     ((eq t (meqp arg 1)) '$zero);; log(1) = 0.
		     ((eq t (mgqp 1 arg)) '$nz)
		     ((eq t (mgrp arg 1)) '$pos)
		     ((eq t (mgqp arg 1)) '$pz)
		     ((eq t (mnqp arg 1)) '$pn)
		     (t '$pnz)))
```

**with:**

```lisp
	       ;; The signs of 1 - ARG and ARG - 1, and whether ARG = 1, as
	       ;; MGRP, MGQP and MEQP would find them, each computed once.
	       (let* ((below (csign (sub 1 arg)))
		      (one (unless (eq below '$pos) (meqp arg 1)))
		      (above (unless (or (eq below '$pos) (eq one t)
					 (member below '($zero $pz)))
			       (csign (sub arg 1)))))
		 (cond ((eq below '$pos) '$neg)
		       ((eq one t) '$zero) ; log(1) = 0.
		       ((member below '($zero $pz)) '$nz)
		       ((eq above '$pos) '$pos)
		       ((member above '($zero $pz)) '$pz)
		       ((null one) '$pn)
		       (t '$pnz))))
```

### 4.7 `src/compar.lisp` (item 2)

`SIGN-MABS`: `csign` false when `SIGN` gave `$pnz`.

**Replace** this block:

```lisp
	  ((eq t (mnqp 0 (cadr x))) (setq sign '$pos)) ; abs(nonzero) > 0
```

**with:**

```lisp
	  ;; abs(nonzero) > 0. If SIGN found nothing, the csign in MEQP would
	  ;; only repeat it, unless ratsimp rewrites the argument.
	  ((eq t (mnqp 0 (cadr x) (not (eq sign '$pnz)))) (setq sign '$pos))
```

### 4.8 `src/limit.lisp` (item 4)

Docstring of `limit-answers`: the new entry format.

**Replace** this block:

```lisp
(defmvar limit-answers ()
  "An association list for storing limit answers.")
```

**with:**

```lisp
(defmvar limit-answers ()
  "A list of limit answers, as entries (CODE KEY . ANSWER), where CODE is
  ALIKE1-HASH of KEY.")
```

### 4.9 `src/limit.lisp` (item 4)

Look answers up by hash code (new `ALIKE1-HASH` and `LIMIT-ANSWER`).

**Replace** this block:

```lisp
(defun putlimval (e v &aux exp)
  (setq exp `((%limit) ,e ,var ,val))
  (unless (assolike exp limit-answers)
    (push (cons exp v) limit-answers))
  v)

(defun getlimval (e)
  (let ((exp (cons '(%limit) (list e var val))))
    (assolike exp limit-answers)))
```

**with:**

```lisp
(defun putlimval (e v &aux exp)
  (setq exp `((%limit) ,e ,var ,val))
  (multiple-value-bind (answer code) (limit-answer exp)
    (unless answer
      (push (list* code exp v) limit-answers)))
  v)

(defun getlimval (e)
  (values (limit-answer (list '(%limit) e var val))))

;; A hash code for E such that (ALIKE1 X Y) implies equal codes. It ignores
;; header flags and looks at most DEPTH levels deep.
(defun alike1-hash (e depth)
  (cond ((or (symbolp e) (integerp e) (stringp e)) (sxhash e))
	((or (atom e) (atom (car e))) 1)
	((or (member (caar e) '(mrat mpois bigfloat)) (zerop depth))
	 (sxhash (caar e)))
	(t (let ((h (sxhash (caar e))))
	     (do ((l (cdr e) (cdr l)))
		 ((atom l) h)
	       (setq h (logand most-positive-fixnum
			       (+ (* 31 h) (alike1-hash (car l) (1- depth))))))))))

;; Looks KEY up in LIMIT-ANSWERS. Returns the answer, or NIL, and KEY's hash
;; code. ALIKE1 compares KEY only with entries of the same code.
(defun limit-answer (key)
  (let ((code (alike1-hash key 4)))
    (dolist (entry limit-answers (values nil code))
      (when (and (eql (car entry) code) (alike1 key (cadr entry)))
	(return (values (cddr entry) code))))))
```

### 4.10 `src/simp.lisp` (item 5)

`CONSTTERMP` and `SCALARCLASS` take an optional `constant`.

**Replace** this block:

```lisp
(defun consttermp (x) (and ($constantp x) (not ($nonscalarp x))))

(defun scalarclass (exp) ;  Returns $SCALAR, $NONSCALAR, or NIL (unknown).
```

**with:**

```lisp
(defun consttermp (x &optional constant)
  (and (or constant ($constantp x))
       (not (eq (scalarclass x t) '$nonscalar))))

;; CONSTANT true says that ($CONSTANTP EXP) is known to be true. Then the
;; arguments of EXP are constant too, so CONSTTERMP need not check them again.
(defun scalarclass (exp &optional constant) ;  Returns $SCALAR, $NONSCALAR, or NIL (unknown).
```

### 4.11 `src/simp.lisp` (item 5)

`SCALARCLASS`: a constant atom is scalar without asking `$constantp` again.

**Replace** this block:

```lisp
	            ;; Include constant atoms which are not declared nonscalar.
```

**with:**

```lisp
	            ;; Include constant atoms which are not declared nonscalar.
	            constant
```

### 4.12 `src/simp.lisp` (item 5)

`SCALARCLASS`: pass `constant` on for special representations.

**Replace** this block:

```lisp
	((specrepp exp) (scalarclass (specdisrep exp)))
```

**with:**

```lisp
	((specrepp exp) (scalarclass (specdisrep exp) constant))
```

### 4.13 `src/simp.lisp` (item 5)

`SCALARCLASS`: pass `constant` on for sums and products.

**Replace** this block:

```lisp
	   (if (not (consttermp (car l)))
	       (return (scalarclass-list l)))))
```

**with:**

```lisp
	   (if (not (consttermp (car l) constant))
	       (return (scalarclass-list l constant)))))
```

### 4.14 `src/simp.lisp` (item 5)

`SCALARCLASS`: pass `constant` on for other operators.

**Replace** this block:

```lisp
	      ((null exp) (scalarclass-list l))
	    (if (not (consttermp (car exp)))
```

**with:**

```lisp
	      ((null exp) (scalarclass-list l constant))
	    (if (not (consttermp (car exp) constant))
```

### 4.15 `src/simp.lisp` (item 5)

`SCALARCLASS-LIST` takes `constant` and passes it on.

**Replace** this block:

```lisp
(defun scalarclass-list (llist)
  (cond ((null llist) nil)
	((null (cdr llist)) (scalarclass (car llist)))
	(t (let ((sc-car (scalarclass (car llist)))
		 (sc-cdr (scalarclass-list (cdr llist))))
```

**with:**

```lisp
(defun scalarclass-list (llist &optional constant)
  (cond ((null llist) nil)
	((null (cdr llist)) (scalarclass (car llist) constant))
	(t (let ((sc-car (scalarclass (car llist) constant))
		 (sc-cdr (scalarclass-list (cdr llist) constant)))
```

### 4.16 `src/mdot.lisp` (item 5)

`SIMPNCT-CONSTANTP` uses `CONSTTERMP` and so the faster path.

**Replace** this block:

```lisp
	   (and ($constantp term) (not ($nonscalarp term))))))
```

**with:**

```lisp
	   (consttermp term))))
```

## 5. Proposed test cases

All **new**. They pin down results the patch must not change, so they pass
before and after it. Checked by inserting them into the test files and
running `run_testsuite(tests=[rtest_sign, rtest_scalarp])` with the patched
build (`rtest_sign` 784/784 not counting the 5 expected errors,
`rtest_scalarp` 22/22), and again with the old definitions of all changed
functions loaded into it (same result). `make check` with the patch: no
unexpected errors in the full suite (21,670 tests), and the SBCL dependency
check passes. Item 4 has no test of its own: it changes no result, and
`rtest_limit`, `rtest_limit_extra` and `rtest_limit_gruntz` cover it.

`tests/rtest_sign.mac`, before the block marked "Leave this at the end of the
file!" (items 1–3):

```
/* sign() of Q^R - S, Q^m - Q^n, abs() and log() */

block([a, m, n, x, y],
  local(a, m, n, x, y),
  assume(a > 2, x > 1, 0 < y, y < 1, m > n),
  [sign(a^2 - 4), sign(a^2 - a), sign(x^3 - x^2), sign(y^3 - y^2),
   sign(x^m - x^n), sign(y^m - y^n), sign(%e^x - 1)]);
[pos, pos, pos, neg, pos, neg, pos]$

[sign(abs((x + 1)^2 - x^2 - 2*x + y^2)), sign(abs(x - y))];
[pos, pz]$

block([r],
  assume(notequal(x + y, 0)),
  r : sign(abs(x + y)),
  forget(notequal(x + y, 0)),
  r);
pos$

block([a, b, c, d, e, f, g],
  local(a, b, c, d, e, f, g),
  assume(a > 1, 0 < b, b < 1, c >= 1, 0 < d, d <= 1, e > 0, notequal(e, 1),
    f > 0, equal(g, 1)),
  [sign(log(a)), sign(log(b)), sign(log(c)), sign(log(d)), sign(log(e)),
   sign(log(f)), sign(log(g))]);
[pos, neg, pz, nz, pn, pnz, zero]$
```

`tests/rtest_scalarp.mac`, at the end (item 5):

```
/* scalarp() and nonscalarp() of constant expressions */

block([c, e : 1],
  local(c),
  declare(c, constant, c, nonscalar),
  for i thru 30 do e : sqrt(e + 1),
  [scalarp(e), nonscalarp(e), scalarp(e + c), nonscalarp(e + c),
   nonscalarp(e*c^2), scalarp(%pi^2 + sqrt(2)),
   nonscalarp(sqrt(2)*matrix([1, 2]))]);
[true, false, false, true, true, true, true]$

block([c],
  local(c),
  declare(c, constant, c, nonscalar),
  [2 . matrix([1, 2]), %pi . matrix([1]), c . matrix([1])]);
[matrix([2, 4]), matrix([%pi]), c . matrix([1])]$
```

## 6. Proposed Git commit messages

One commit per item. Item 1: hunks 4.4 and 4.5 with the first
`rtest_sign.mac` test. Item 2: hunks 4.1–4.3 and 4.7 with the second and
third. Item 3: hunk 4.6 with the fourth. Item 4: hunks 4.8 and 4.9. Item 5:
hunks 4.10–4.16 with the `rtest_scalarp.mac` tests.

```
Speed up sign() of differences of powers

For a difference like x^2 - x or a^2 - 4, sign() first tested whether
one side is a power of the other, an expensive equality test, and only
then whether the base is positive. Mostly the base was not known to be
positive, so the expensive answer was not needed. Now the cheap tests
come first.

This makes the test files that use sign() most (rtest_limit_extra,
rtest_trig, rtest_limit, rtest16, rtest_integrate) 8% faster.

AI-Assisted-By: Claude Opus 5.5
```

```
Speed up sign(abs(x)) for x of unknown sign

When sign() cannot tell the sign of x, sign(abs(x)) still checks
whether x can be zero, and that check computes the sign of x once more.
In the full test suite this happened 132,000 times and never helped.
Now the check computes it again only if ratsimp() rewrites x. Facts
like notequal(x, 0) are still used.

AI-Assisted-By: Claude Opus 5.5
```

```
Speed up sign() of logarithms

sign(log(x)) for positive x compares x with 1 in up to six ways, which
took each of three sign computations twice. Now each runs once.

AI-Assisted-By: Claude Opus 5.5
```

```
Speed up limit() by finding cached answers faster

limit() remembers the answers to the subproblems of a computation and
looked each new subproblem up by comparing it with every stored one.
Now each stored answer keeps a hash code, and only answers with the
same code are compared. The limit test files run 9% faster.

AI-Assisted-By: Claude Opus 5.5
```

```
Make nonscalarp() linear for constant expressions

scalarp() and nonscalarp() checked each part of a constant expression
for constness again at every level above it, so the time grew with the
square of the nesting depth. For sqrt(1 + sqrt(1 + ...)) nested 2000
deep, nonscalarp() took 0.7 s, now 0.001 s. Products with "." of
matrices with constant entries profit too: rtest_cholesky and
rtest_matrixexp run 16% faster.

AI-Assisted-By: Claude Opus 5.5
```
