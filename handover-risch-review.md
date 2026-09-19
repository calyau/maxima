# Handover: reviewing the Risch logarithmic part

**This patch applies on top of `handover-risch-lazard-rioboo-trager.md`.**
Its anchors are lines that document leaves in `src/risch.lisp`, so apply
that one (and the Rothstein-Trager document it in turn sits on) first.

## 1. Summary

A review pass over the Lazard-Rioboo-Trager logarithmic part.  It changes
no integral: every case the helpers solved before they solve now, in the
same form, and every case they declined they still decline.  The one
behavioural difference is on an input that makes them signal, which is an
error out of `rischint` before the patch and a noun after it; no such
input is known.  What the patch does is remove a special case that the
remainder sequence can carry itself, make `RISCHLOGEPROG` catch what
`RISCH-RT-LOGPART` may signal, delete a dead subexpression, put right
five claims that the tree does not bear out, and add a test for the one
path the algorithm exists for and that nothing covered.

### What this supersedes in the earlier two documents

Five claims in them do not hold.  Their code blocks are left alone --
each document has to keep reproducing its own commit -- so this is where
they are put right.

* `SOLVE` was said to read `$realonly`, in the comment on
  `RISCH-RT-ROOTS` (Rothstein-Trager document, section 3.2).  It does
  not; that flag belongs to `ALGSYS`.
* The factor by which a remainder-sequence member differs from the
  subresultant was said to cost at most the logarithm of a constant
  (Lazard-Rioboo-Trager document, sections 2 and 3.1 and 3.3).  It
  carries the variable of integration, and `RISCH-RT-MONIC` removing it
  is load-bearing.
* `RISCH-RT-SOLVABLE-P` was described as admitting quadratics and
  binomials.  It also admits biquadratics, and it turns away
  `z^6+z^3+1`, which is a quadratic in a power.
* A quartic `SOLVE` cannot crack was said to give an antiderivative of a
  million conses (Lazard-Rioboo-Trager document, sections 2 and 3.3, and
  the comment in `rtest_integrate.mac` about "a form too big to compute
  a gcd with").  Measured, the cubic gives 1800 characters and the
  quartic gives a division by zero; and there has been no gcd since
  Lazard-Rioboo-Trager.
* The `ChangeLog` entry promises residues of degree up to four.  A
  general quartic is not reached.

The test comment about the sextic, added with the Rothstein-Trager
change, attributes the split to the resultant.  It is the denominator
that splits, before the resultant is taken.

## 2. Reproducer/Demo

### Behaviour is unchanged

The patch is not visible from the top level.  A battery of 53 integrals
(31 integrands plus seven option settings -- `logabs`, `domain`,
`keepfloat`, `ratfac`, `algebraic`, `realonly`, `radexpand` -- applied to
three of them) gives character-identical output before and after, and so
do 350 randomly generated log-part integrands, of which 141 are solved
and none wrongly.  There is no old-versus-new transcript to show, and
inventing one would misrepresent the change.

### The claim about `$realonly` (executed, current build)

```
(%i1) display2d:false
(%i2) realonly:true
(%i3) solve(z^2+1 = 0,z)
(%o3) [z = -%i,z = %i]
(%i4) risch(1/(x*(log(x)^2+1)),x)
(%o4) atan(log(x))
```

`$realonly` is read in `algsys.lisp` only (`grep -n realonly src/*.lisp`),
by `ALGSYS` and `$ALGSYS`; `SOLVE` never consults it.  Setting it does not
drop the complex roots, and `RISCH-RT-ROOTS` never sees the short list its
comment described.

### The claim about the subresultant factor (internals)

`RISCH-RT-MONIC` divides a substituted sequence member by its leading
coefficient in the monomial.  That coefficient is where the difference
between the member and the subresultant sits, and for a tower with more
than one generator it is not a constant.  Redefining `RISCH-RT-MONIC` to
return its argument unnormalized and integrating
`2*sqrt(2)*(1-log(x))/(log(x)^2-2*x^2)` gives an antiderivative whose
derivative misses the integrand by `-2^(3/2)/(2*x^2-1)`; with the
normalization it is exact.  So the factor may not be carried along, and
the comment is wrong to say it could be.

### What each half of `RISCH-RT-SOLVABLE-P` is worth (internals)

Removing `(<= d (* 2 g))` lets `SOLVE` use Cardano and Ferrari:
`1/(x*(log(x)^3+log(x)+1))` then integrates to 1800 characters, and
`1/(x*(log(x)^4+log(x)+1))` raises `` `quotient' by `zero' `` from the
substitution.  Removing `(<= d 4)` instead leaves binomials of higher
degree, which `SOLVE` still cracks and quickly (12 ms for degree five,
108 ms for degree seven), but whose roots `RECTFORM` no longer reduces to
a recognizable conjugate pair: `1/(x*(log(x)^5-2))` comes back in 1882
characters carrying `%i`, against 159 characters and no `%i` for the
quartic `1/(x*(log(x)^4+1))`.  That answer is correct -- it agrees with
`quad_qags` on [4, 5] to fifteen digits -- so the bound is about the form
of the answer, not about getting one.

### The path the test covers (executed, current build)

```
(%i5) (log(x)^3+log(x))/(x*(log(x)^4+1))
(%i6) risch(%,x)
(%o6) log(log(x)^4+1)/4+atan(log(x)^2)/2
(%i7) ratsimp(radcan(diff(%,x)-%th(2)))
(%o7) 0
```

Two of the four residues coincide, so `R` has two double roots and the
logarithms come off the member of degree two.  Every other case in
`rtest_integrate.mac` has simple residues, where the member wanted is the
last one and the sequence is doing no more than an ordinary gcd.

## 3. Code patching instructions

### 3.1 `src/risch.lisp` -- the note on the subresultant factor

The claim is false: the factor carries the variable of integration.

**Replace** this block:

```lisp
;; The members of the sequence are the subresultants only up to a factor
;; free of the monomial, which is harmless: RISCH-RT-MONIC divides it out,
;; and what survives changes a logarithm by the logarithm of a constant.
```

with:

```lisp
;; The members of the sequence are the subresultants only up to a factor
;; free of the monomial.  That factor is not free of the variable of
;; integration, so it cannot just be carried along: RISCH-RT-MONIC divides
;; it out with the rest of the leading coefficient, which leaves the monic
;; gcd.  Leave it in and the integral of
;; 2*sqrt(2)*(1-log(x))/(log(x)^2-2*x^2) comes back wrong.
```

### 3.2 `src/risch.lisp` -- `risch-rt-prs` carries A, and the degree must fall

The sequence now names DEN itself, so nothing else has to.

**Replace** this block:

```lisp
;; The remainder sequence of A and B in V as an alist of (degree . member),
;; taken over the field the coefficients lie in.  Each member is the
;; subresultant of its degree up to a factor free of V, which is harmless:
;; it changes the sum below by the logarithm of a constant.
(defun risch-rt-prs (a b v)
  (let ((seq nil))
    (do () (nil seq)
      (when (or (null b) (zerop1 b)) (return seq))
      (let ((d (risch-rt-degree b v)))
        (when (minusp d) (return seq))
```

with:

```lisp
;; The remainder sequence of A and B in V, A included, as an alist of
;; (degree . member) taken over the field the coefficients lie in.  Each
;; member is the subresultant of its degree up to a factor free of V, which
;; RISCH-RT-MONIC divides out of the substituted member.
;; A division drops the degree, so one degree names one member, and the
;; sequence is no longer than the degree of A.
(defun risch-rt-prs (a b v)
  (let ((seq (list (cons (risch-rt-degree a v) a))))
    (do () (nil seq)
      (when (or (null b) (zerop1 b)) (return seq))
      (let ((d (risch-rt-degree b v)))
        (unless (< -1 d (caar seq)) (return seq))
```

### 3.3 `src/risch.lisp` -- `risch-rt-solvable-p`

`(max g 1)` is dead, and the comment described a different predicate.

**Replace** this block:

```lisp
;; T when the roots of F are radicals of the roots of a polynomial of
;; degree at most two, that is when F is a quadratic or a binomial in some
;; power of V.  SOLVE writes those as a surd times a root of unity.  For
;; anything else it reaches for Cardano or Ferrari, and substituting what
;; comes back, while correct, is of no use to anybody: the roots of one
;; quartic that does not factor give an antiderivative of a million conses.
(defun risch-rt-solvable-p (f v)
  (let ((d (risch-rt-degree f v))
        (g 0))
    (do ((k 1 (1+ k)))
        ((> k d))
      (unless (zerop1 ($ratcoef f v k))
        (setq g (gcd g k))))
    (and (plusp d) (<= d 4) (<= d (* 2 (max g 1))))))
```

with:

```lisp
;; T when F has degree at most four and is linear or quadratic in some
;; power of V -- a quadratic, a binomial, or a biquadratic.  SOLVE writes
;; the roots of those as a surd, or a surd times a root of unity, and both
;; halves of the test earn their place.
;;
;; Drop the second and SOLVE reaches for Cardano or Ferrari on the rest of
;; degree three and four.  Substituting what comes back is of no use to
;; anybody: 1/(x*(log(x)^3+log(x)+1)) integrates to 1800 characters, and
;; 1/(x*(log(x)^4+log(x)+1)) does not integrate at all -- the substitution
;; ends in a division by zero.
;;
;; Drop the first and SOLVE still cracks a binomial of higher degree, and
;; quickly.  What it cannot do is write the roots so that RECTFORM reduces
;; them to a recognizable conjugate pair, so RISCH-RT-TERMS pairs nothing
;; and %i survives: 1/(x*(log(x)^5-2)) comes back correct -- it agrees
;; with QUAD_QAGS to fifteen digits -- in 1882 characters carrying %i,
;; where the quartic 1/(x*(log(x)^4+1)) takes 159 and is real.
(defun risch-rt-solvable-p (f v)
  (let ((d (risch-rt-degree f v))
        (g 0))
    (do ((k 1 (1+ k)))
        ((> k d))
      (unless (zerop1 ($ratcoef f v k))
        (setq g (gcd g k))))
    (and (plusp d) (<= d 4) (<= d (* 2 g)))))
```

### 3.4 `src/risch.lisp` -- the note on `solve` and `$realonly`

`solve` does not read that flag; `algsys` does.

**Replace** this block:

```lisp
;; dependency on solve.lisp, which is compiled after this file.  A
;; $REALONLY set by the user drops the complex roots, and the count below
;; then rejects what is left rather than integrating with part of the sum
;; missing.
```

with:

```lisp
;; dependency on solve.lisp, which is compiled after this file.  The count
;; rejects a list that is short of the degree rather than integrating with
;; part of the sum missing.
```

### 3.5 `src/risch.lisp` -- `risch-rt-logpart` -- no case for deg(DEN)

The sequence names that member.

**Replace** this block:

```lisp
               (s (if (= mult degq) qm (cdr (assoc mult prs))))
```

with:

```lisp
               (s (cdr (assoc mult prs)))
```

### 3.6 `src/risch.lisp` -- catch what the helper may signal

Tabs, as the surrounding function uses.

**Replace** this block:

```lisp
	      (let ((rt (risch-rt-logpart p1e p2e my-divisor risch-intvar
					  risch-var)))
		(when rt (return (cons (rzero) rt)))))
```

with:

```lisp
	      ;; RISCH-RT-SOLVABLE-P turns away every factor SOLVE does not
	      ;; write down usefully, and nothing else guards the divisions
	      ;; that substituting a root does: loosen it and a quartic
	      ;; ends in a division by zero.  Caught here that is a noun
	      ;; and a message, not an error out of RISCHINT.
	      (let ((rt (car (errcatch
			      (risch-rt-logpart p1e p2e my-divisor
						risch-intvar risch-var)))))
		(when rt (return (cons (rzero) rt)))))
```

### 3.7 `tests/rtest_integrate.mac` -- the comment on the sextic

R is never of degree six; the denominator is what splits.

**Replace** this block:

```maxima
/* R factors into pieces of degree at most four here, although it is of
 * degree six.
 */
```

with:

```maxima
/* Two calls, not one: the denominator splits before the resultant is
 * taken, so R comes out of degree two and then of degree four, never of
 * degree six.
 */
```

### 3.8 `tests/rtest_integrate.mac` -- the comment on the two declined cases

There is no gcd since Lazard-Rioboo-Trager, and size is not the reason.

**Replace** this block:

```maxima
/* A factor of degree five is left alone, and so is a quartic whose roots
 * SOLVE only writes down in a form too big to compute a gcd with.
 */
```

with:

```maxima
/* Both are turned away by RISCH-RT-SOLVABLE-P.  R is an irreducible
 * quintic in the first; in the second it is an irreducible quartic that
 * is not quadratic in any power of z, which is where SOLVE reaches for
 * Ferrari.
 */
```

### 3.9 `tests/rtest_integrate.mac` -- the new case

Insert the block; it stays before the closing banner.

**Replace** this block:

```maxima
risch(1/(log(x)^2-2), x);
'integrate(1/(log(x)^2-2), x);
```

with:

```maxima
risch(1/(log(x)^2-2), x);
'integrate(1/(log(x)^2-2), x);

/* Two of the four residues coincide here, so R has two double roots and
 * the logarithms come off the member of degree two of the remainder
 * sequence, not off its last member.
 */

block([f : (log(x)^3+log(x))/(x*(log(x)^4+1)), F],
 F : risch(f, x),
 [F, ratsimp(radcan(diff(F, x) - f))]);
[log(log(x)^4+1)/4+atan(log(x)^2)/2, 0];
```

### 3.10 `ChangeLog` -- what the entry promises

A general quartic is not reached, so do not say degree four.

**Replace** this block:

```text
  x) is atan(log(x)) rather than a noun.  Residues of degree up to four
  over the rationals are reached, and conjugate pairs are recombined so
  that the answer stays real
```

with:

```text
  x) is atan(log(x)) rather than a noun.  The residues reached are the
  roots of a quadratic, a binomial or a biquadratic of degree at most
  four, and conjugate pairs are recombined so that the answer stays real
```

Nothing in `$testsuite_files` moves: the new case goes in before the
`/* Leave this at the end of the file! */` banner, so the
`[facts(), contexts]` pair stays last, and the registered failures for
this file are 826 and 827, both far above.

## 4. Proposed test cases

One new pair, for `tests/rtest_integrate.mac`.  Run: the file passes
961/961 with it (960 before), `No unexpected errors found out of 963
tests`.

```maxima
/* Two of the four residues coincide here, so R has two double roots and
 * the logarithms come off the member of degree two of the remainder
 * sequence, not off its last member.
 */

block([f : (log(x)^3+log(x))/(x*(log(x)^4+1)), F],
 F : risch(f, x),
 [F, ratsimp(radcan(diff(F, x) - f))]);
[log(log(x)^4+1)/4+atan(log(x)^2)/2, 0];

```

The pair asserts the answer and not just its derivative on purpose.  A
wrong index into the remainder sequence gives a member of the wrong
degree, `RISCH-RT-LOGPART`'s `total` check then fails and the result is a
noun, so `freeof('integrate, ...)` alone would catch that; but the
closed form is what says the degree-two member was the one taken.

No existing expectation changes.

## 5. Proposed Git commit message

```
rischint: fold DEN into the remainder sequence

RISCH-RT-PRS now carries A as the member of its own degree, so
RISCH-RT-LOGPART needs no case for a multiplicity equal to deg(DEN): the
sequence names that member like any other.  The degree test becomes a
test that the degree falls, which is what ASSOC relies on and what
bounds the loop.

RISCHLOGEPROG catches what RISCH-RT-LOGPART may signal.  Nothing but
RISCH-RT-SOLVABLE-P stands between SOLVE's output and the divisions that
substituting a root does, and that predicate judges how useful an answer
would be rather than whether one can be computed: with it removed,
1/(x*(log(x)^4+log(x)+1)) ends in a division by zero instead of a noun.

RISCH-RT-SOLVABLE-P: (MAX G 1) was dead, G being the gcd of exponents
one of which is the degree, and the degree positive there.  Its comment
described the predicate as quadratics and binomials, which is neither
what the code accepts -- a biquadratic passes -- nor what it rejects:
z^6+z^3+1 is a quadratic in a power and is turned away.  Say instead
what each half of the test is for, from what removing that half does.
The ChangeLog promised the same too much and now promises less.

RISCH-RT-ROOTS: SOLVE does not read $REALONLY.  That flag belongs to
ALGSYS (algsys.lisp), and SOLVE returns both roots of z^2+1 with it set.

The note about the subresultants said the factor RISCH-RT-MONIC divides
out would only change a logarithm by the logarithm of a constant.  It
does not: the factor carries the variable of integration, and keeping it
costs the integral of 2*sqrt(2)*(1-log(x))/(log(x)^2-2*x^2) its
correctness.

Two comments in rtest_integrate.mac as well.  The resultant for
1/(x*(log(x)^6+1)) is never of degree six -- the denominator splits
before it is taken, and R comes out of degree two and then four -- and
the quartic left alone there is left alone because it is not quadratic
in any power of z, not because a gcd with its roots would be too big.
There has been no gcd since Lazard-Rioboo-Trager.

Test the multiplicity the remainder sequence exists for.  Every case in
the file so far has simple residues, where the member wanted is the last
one and the sequence is only an ordinary gcd;
(log(x)^3+log(x))/(x*(log(x)^4+1)) has two double residues and takes the
member of degree two.
```
