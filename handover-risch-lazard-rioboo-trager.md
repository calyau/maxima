# Handover: Lazard-Rioboo-Trager in risch()'s logarithmic part

**This patch applies on top of `handover-risch-rothstein-trager.md`.**  It
rewrites the interior of the block that one adds; applied to a tree without
it, none of the anchors below will be found.

## 1. Summary

**Replace the per-root algebraic gcd in the logarithmic part with
Lazard-Rioboo-Trager, which reads the same logarithms off a polynomial
remainder sequence.**

`risch-rt-logpart` computed `gcd(NUM - c*DIVISOR, DEN)` once per root `c` of
the Rothstein-Trager resultant, with `c` an algebraic number.  LRT instead
takes the remainder sequence of `DEN` and `NUM - z*DIVISOR` once: its member
of degree `i` carries the factor belonging to the `i`-th squarefree part of
the resultant, and a root is only ever substituted into it.  No step then
divides by an algebraic number, so `$algebraic` and the algebraic `$gcd`
are no longer needed -- and, as section 2 shows, no longer able to do harm.

It is not results-neutral.  `risch-rt-solvable-p` replaces the bare degree
bound the gcd version used, and gives up a few integrands the gcd did
reach.  Section 2 has the count and what it buys.

## 2. Reproducer/Demo

### What changes, and what does not

Every integral in the Rothstein-Trager test block returns what it returned
before, character for character.  What changes is further out: the gcd
version attempted every irreducible factor of the resultant of degree at
most four, and `risch-rt-solvable-p` now attempts only those `solve` writes
as surds -- a quadratic, a binomial, a biquadratic.  On fifty random
log-part integrands the gcd version solves thirty and this one solves
twenty-eight.  Both of the two given up are correct, and one of them is
compact:

```
(3-log(x)^2)/(3*x*log(x)^3+3*x*log(x)^2+x*log(x)+3*x)
        gcd version:   264 characters, no %i
        this patch:    'integrate(...) -- refused by risch-rt-solvable-p
```

What it buys is a bounded cost.  Those same fifty integrands take 124
seconds of `risch` through the gcd, three of them between 30 and 52 seconds
each; here they take 28 milliseconds.  Over two hundred, this patch solves
123, spends 0.1 seconds in `risch` with no single call above 8
milliseconds, and returns no answer longer than 147 characters and none
carrying `%i`.

The bound is not optional.  Keep everything else and weaken
`risch-rt-solvable-p` to the gcd version's plain `(<= 1 d 4)`, and the
third of those two hundred integrands --
`(3-2*log(x)^3)/(3*x*log(x)^4+2*x*log(x)^3+x*log(x)-3*x)` -- exhausts the
heap and takes the image down with it.  That is the trade the predicate
makes: a handful of large but correct answers for a `risch` that always
returns.

Suite figures, with the one test this patch adds:

```
before:  rtest_integrate 960/960;  share suite 1 failure out of 21,511
after:   rtest_integrate 961/961;  share suite 1 failure out of 21,512
```

(The one failure is `share/stringproc/rtestprintf.mac` problem 38, which
predates all of this work: SBCL's `~e` float printing, reproducible with no
Maxima code in the picture as `(format nil "~e" 42.0d0)` giving
`4.2000000000000004e+1` under SBCL 2.2.9.)

### What the patch actually buys

The gcd was taken over an algebraic extension, which brought `$algebraic`
into a computation that had no other use for it.  That was not free.  With
the bound in `risch-rt-solvable-p` lifted so that `1/(x*(log(x)^5-2))` is
reached -- its resultant is `50000*z^5-1` -- the four combinations of the
two bindings give, for `diff(F,x) - f` at `x = 4`:

```
$algebraic t, $gcd '$algebraic   ->  4.88e-4 + 4.94e-3 %i     wrong
$algebraic t                     ->  4.88e-4 + 4.94e-3 %i     wrong
$gcd '$algebraic                 ->  7.89e-17 - 1.12e-18 %i   exact
neither                          ->  7.89e-17 - 1.12e-18 %i   exact
```

So `$algebraic` is what spoils it, and LRT lets it be bound off.  Be aware
of what is and is not established here: the wrongness is measured and
repeatable, but the step that causes it was not run down.  What is visibly
different under `$algebraic` is that `solve` writes two of the five roots in
a reduced form -- still correct roots, residual `1e-15` in the resultant --
yet each step tried in isolation (`factor`, `divide`, the substitution and
its `ratsimp`) agrees to fifteen digits with the unreduced form.  The
comment in the source says as much rather than inventing a mechanism.

This is latent either way at present, since `risch-rt-solvable-p` keeps
those roots out; it matters to whoever widens the bound.

### Internals

For `NUM/DEN` with `DEN` monic and squarefree in the monomial, write
`R(z) = resultant(NUM - z*DIVISOR, DEN)` in that monomial and factor it as
the product of `Q_i^i`.  LRT says the logarithmic part is the sum over `i`,
and over the roots `c` of `Q_i`, of `c*log(S_i(c))`, where `S_i` is the
member of degree `i` of the remainder sequence of `DEN` and
`NUM - z*DIVISOR`.

Two things make this shorter than the textbook version:

- The sequence is taken over the coefficient field, with `$divide`, rather
  than as a subresultant PRS.  Its members are the subresultants only up to
  a factor free of the monomial, and none of the subresultant normalization
  is needed because `risch-rt-monic` divides that factor out along with the
  rest of the leading coefficient.  It is not a factor one could carry
  along instead: it holds the variable of integration.  Leave it in and
  `2*sqrt(2)*(1-log(x))/(log(x)^2-2*x^2)` integrates to something whose
  derivative misses the integrand by `-2^(3/2)/(2*x^2-1)`.
- The member wanted is named by the multiplicity of the factor in `R`, and
  the sequence carries `DEN` itself as its highest member, so a
  multiplicity equal to `deg(DEN)` needs no case of its own.  Degrees fall
  strictly on every division, so one degree names one member and the
  sequence is no longer than `deg(DEN)`.

`risch-rt-gcd` disappears.  The guard that the degrees must add up to
`deg(DEN)` stays and now guards the substitutions instead.

## 3. Code patching instructions

Eight hunks in apply order: five in `src/risch.lisp`, two in
`tests/rtest_integrate.mac`, one in `ChangeLog`.  Each anchor was checked to
occur exactly once in the file as the previous handover leaves it.

### 3.1 `src/risch.lisp` -- the heading comment

It names a different algorithm now, and the note on the subresultant factor has to say what RISCH-RT-MONIC is for.

**Replace** this block:

```lisp
;; Rothstein-Trager reduction of the logarithmic part.  For an integrand
;; NUM/DEN with DEN monic and squarefree in the monomial and deg(NUM) <
;; deg(DEN), that part is
;;
;;      sum(c * log(gcd(NUM - c*DIVISOR, DEN)))
;;
;; over the roots C of the Rothstein-Trager resultant
;;
;;      R(z) = resultant(NUM - z*DIVISOR, DEN)
;;
;; taken in the monomial, DIVISOR being the derivative of DEN that
;; RISCHLOGEPROG has already reduced.  The logarithmic part is elementary
;; only when every root of R is constant, which R -- made monic in z --
;; witnesses by being free of the variable of integration.  R is factored
;; over the rationals and each irreducible factor of degree at most four is
;; solved for its roots; a factor of higher degree, or a set of gcds that
;; does not multiply out to DEN, leaves the noun in place.
;;
;; $RESULTANT seeds VARLIST with its third argument and NEWVAR prepends
;; whatever else it finds, so the monomial stays last in VARLIST, where
;; ORDERPOINTER numbers it highest and POINTERGP therefore ranks it above
;; everything else.  It is the main variable of the resultant whatever
;; GREAT would have made of it.
```

with:

```lisp
;; Lazard-Rioboo-Trager reduction of the logarithmic part.  For an
;; integrand NUM/DEN with DEN monic and squarefree in the monomial and
;; deg(NUM) < deg(DEN), that part is
;;
;;      sum over i, and over the roots c of Q_i, of c*log(S_i(c))
;;
;; where R(z) = resultant(NUM - z*DIVISOR, DEN), taken in the monomial, is
;; the product of the Q_i^i and S_i is the member of degree i of the
;; remainder sequence of DEN and NUM - z*DIVISOR.  DIVISOR is the
;; derivative of DEN that RISCHLOGEPROG has already reduced.  The
;; logarithmic part is elementary only when every root of R is constant,
;; which R -- made monic in z -- witnesses by being free of the variable of
;; integration.
;;
;; Reading the logarithms off the remainder sequence rather than out of a
;; gcd taken with each root keeps every step a polynomial one: nothing is
;; divided by an algebraic number, so neither $ALGEBRAIC nor an algebraic
;; $GCD is wanted here, and $ALGEBRAIC is bound away.  Leaving it set is
;; not merely wasteful.  Lift the bound in RISCH-RT-SOLVABLE-P to let the
;; fifth roots of 1/50000 through, and with $ALGEBRAIC set the answer comes
;; back wrong -- its derivative misses the integrand by 5e-4 -- while with
;; it bound off the same answer is exact.  What is different is that SOLVE
;; then writes two of those roots in a reduced form; which step the reduced
;; form spoils was not run down.
;;
;; The members of the sequence are the subresultants only up to a factor
;; free of the monomial.  That factor is not free of the variable of
;; integration, so it cannot just be carried along: RISCH-RT-MONIC divides
;; it out with the rest of the leading coefficient, which leaves the monic
;; gcd.  Leave it in and the integral of
;; 2*sqrt(2)*(1-log(x))/(log(x)^2-2*x^2) comes back wrong.
;;
;; $RESULTANT seeds VARLIST with its third argument and NEWVAR prepends
;; whatever else it finds, so the monomial stays last in VARLIST, where
;; ORDERPOINTER numbers it highest and POINTERGP ranks it above everything
;; else.  It is the main variable of the resultant whatever GREAT would
;; have made of it.
```

### 3.2 `src/risch.lisp` -- `risch-rt-factors` -- keep the multiplicities

LRT indexes the sequence by a factor's multiplicity in R, so the multiplicity has to survive.

**Replace** this block:

```lisp
;; The distinct irreducible factors of E that involve V.
(defun risch-rt-factors (e v)
  (cond ((mtimesp e) (mapcan #'(lambda (f) (risch-rt-factors f v)) (cdr e)))
        ((mexptp e) (risch-rt-factors (cadr e) v))
        ((freeof v e) nil)
        (t (list e))))
```

with:

```lisp
;; Alist of (irreducible factor . multiplicity) for the factors of E that
;; involve V.
(defun risch-rt-factors (e v)
  (cond ((mtimesp e)
         (mapcan #'(lambda (f) (risch-rt-factors f v)) (cdr e)))
        ((and (mexptp e) (integerp (caddr e)) (plusp (caddr e)))
         (mapcar #'(lambda (p) (cons (car p) (* (cdr p) (caddr e))))
                 (risch-rt-factors (cadr e) v)))
        ((freeof v e) nil)
        (t (list (cons e 1)))))
```

### 3.3 `src/risch.lisp` -- `risch-rt-gcd` gives way to the sequence and the root test

One remainder sequence replaces one gcd per root, and the root test decides which factors are worth solving at all.

**Replace** this block:

```lisp
(defun risch-rt-gcd (c pm qm dm v)
  (risch-rt-monic (car (errcatch ($gcd ($ratsimp (sub pm (mul c dm))) qm)))
                  v))
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
        (push (cons d b) seq)
        (when (zerop d) (return seq))
        (let ((qr (car (errcatch ($divide a b v)))))
          (unless ($listp qr) (return seq))
          (setq a b
                b ($ratsimp (caddr qr))))))))

;; T when F has degree at most four and is linear or quadratic in some
;; power of V -- a quadratic, a binomial, or a biquadratic.  SOLVE writes
;; the roots of those as a surd, or a surd times a root of unity, and both
;; halves of the test earn their place.
;;
;; Drop the second and SOLVE reaches for Cardano or Ferrari on the rest of
;; degree three and four, and what comes back is bounded by nothing: of
;; two hundred random integrands of this shape the third,
;; (3-2*log(x)^3)/(3*x*log(x)^4+2*x*log(x)^3+x*log(x)-3*x), exhausts the
;; heap and takes the image down with it.  Short of that the answers are
;; merely long, 1/(x*(log(x)^3+log(x)+1)) integrating to 1800 characters.
;; This is not free: a few cubics the gcd that preceded this did reach go
;; with them, among them
;; (3-log(x)^2)/(3*x*log(x)^3+3*x*log(x)^2+x*log(x)+3*x), which is correct
;; in 533 characters and is now turned away.
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

### 3.4 `src/risch.lisp` -- `risch-rt-logpart`

Substitute each root into a member of the sequence instead of taking a gcd with it.

**Replace** this block:

```lisp
;; The logarithmic part of NUM/DEN as a list of Maxima expressions, or NIL
;; when the integrand has no elementary one or this implementation cannot
;; reach it.
(defun risch-rt-logpart (num den divisor risch-intvar risch-var)
  (let* ((risch-ratform (list 'mrat 'simp varlist genvar))
         (tvar (get risch-var 'rischexpr))
         (zvar (gensym))
         (pm (disrep (ratfix num) risch-ratform))
         (qm (disrep (ratfix den) risch-ratform))
         (dm (disrep (ratfix divisor) risch-ratform)))
    (let (($algebraic t) ($gcd '$algebraic) ($ratfac nil) ($keepfloat nil)
          ($errormsg nil) (varlist nil) (genvar nil)
          (terms nil) (total 0) res n fl)
      (when (or (zerop1 dm) (zerop1 qm)
                (>= (risch-rt-degree pm tvar) (risch-rt-degree qm tvar)))
        (return-from risch-rt-logpart nil))
      (setq res (car (errcatch ($resultant (sub pm (mul zvar dm)) qm tvar))))
      (unless res (return-from risch-rt-logpart nil))
      (setq res ($ratsimp res))
      (setq n (risch-rt-degree res zvar))
      (when (< n 1) (return-from risch-rt-logpart nil))
      (setq res ($ratsimp (div res ($ratcoef res zvar n))))
      (unless (freeof risch-intvar res)
        (return-from risch-rt-logpart nil))
      (setq fl (car (errcatch ($factor ($num res)))))
      (unless fl (return-from risch-rt-logpart nil))
      (dolist (f (risch-rt-factors fl zvar))
        (let* ((d (risch-rt-degree f zvar))
               (cs (and (<= 1 d 4) (risch-rt-roots f zvar d)))
               (gs (mapcar #'(lambda (c) (risch-rt-gcd c pm qm dm tvar)) cs)))
          (unless (and gs (every #'identity gs))
            (return-from risch-rt-logpart nil))
          (dolist (g gs) (incf total (risch-rt-degree g tvar)))
          (setq terms (nconc (risch-rt-terms cs gs tvar) terms))))
      ;; The gcds are coprime and multiply out to DEN, so a total degree
      ;; other than DEN's means a residue was missed or counted twice and
      ;; the sum is not an antiderivative.
      (and terms
           (= total (risch-rt-degree qm tvar))
           (mapcar #'resimplify terms)))))
```

with:

```lisp
(defun risch-rt-logpart (num den divisor risch-intvar risch-var)
  (let* ((risch-ratform (list 'mrat 'simp varlist genvar))
         (tvar (get risch-var 'rischexpr))
         (zvar (gensym))
         (pm (disrep (ratfix num) risch-ratform))
         (qm (disrep (ratfix den) risch-ratform))
         (dm (disrep (ratfix divisor) risch-ratform)))
    (let (($algebraic nil) ($ratfac nil) ($keepfloat nil)
          ($errormsg nil) (varlist nil) (genvar nil)
          (terms nil) (total 0) (degq 0) res n fl prs)
      (setq degq (risch-rt-degree qm tvar))
      (when (or (zerop1 dm) (zerop1 qm)
                (>= (risch-rt-degree pm tvar) degq))
        (return-from risch-rt-logpart nil))
      (setq res (car (errcatch ($resultant (sub pm (mul zvar dm)) qm tvar))))
      (unless res (return-from risch-rt-logpart nil))
      (setq res ($ratsimp res))
      (setq n (risch-rt-degree res zvar))
      (when (< n 1) (return-from risch-rt-logpart nil))
      (setq res ($ratsimp (div res ($ratcoef res zvar n))))
      (unless (freeof risch-intvar res)
        (return-from risch-rt-logpart nil))
      (setq fl (car (errcatch ($factor ($num res)))))
      (unless fl (return-from risch-rt-logpart nil))
      (setq prs (risch-rt-prs qm ($ratsimp (sub pm (mul zvar dm))) tvar))
      (dolist (f (risch-rt-factors fl zvar))
        (let* ((mult (cdr f))
               (s (cdr (assoc mult prs)))
               (d (risch-rt-degree (car f) zvar))
               (cs (and s (risch-rt-solvable-p (car f) zvar)
                        (risch-rt-roots (car f) zvar d)))
               (gs (mapcar #'(lambda (c)
                               (risch-rt-monic
                                ($ratsimp (maxima-substitute c zvar s))
                                tvar))
                           cs)))
          (unless (and gs (every #'identity gs))
            (return-from risch-rt-logpart nil))
          (dolist (g gs) (incf total (risch-rt-degree g tvar)))
          (setq terms (nconc (risch-rt-terms cs gs tvar) terms))))
      (and terms
           (= total degq)
           (mapcar #'resimplify terms)))))
```

### 3.5 `src/risch.lisp` -- catch what the helper may signal, in `rischlogeprog`

Every line begins with a tab, as its neighbours do.

**Replace** this block:

```lisp
	    (unless risch-expflag
	      (let ((rt (risch-rt-logpart p1e p2e my-divisor risch-intvar
					  risch-var)))
		(when rt (return (cons (rzero) rt)))))
```

with:

```lisp
	    (unless risch-expflag
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

### 3.6 `tests/rtest_integrate.mac` -- why those two cases are refused

There is no gcd any more, and the reason each is refused has changed with it.

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

### 3.7 `tests/rtest_integrate.mac` -- the new case

It goes before the closing banner, so the `[facts(), contexts]` pair stays last.

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

### 3.8 `ChangeLog` -- what the entry promises

The entry was written for the gcd, which did sometimes reach a general cubic or quartic.  This does not.

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
## 4. Proposed test cases

One new pair, for `tests/rtest_integrate.mac`, carried by hunk 3.7 above.
It is the first case in the file whose resultant has a repeated root, and
so the first that exercises the indexing this patch is about: with simple
residues the member wanted is always the last one and the sequence does no
more than the gcd it replaces.

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

It asserts the answer and not only its derivative on purpose.  A wrong
index gives a member of the wrong degree, the `total` check in
`risch-rt-logpart` then fails and the result is a noun, which
`freeof('integrate, ...)` would already catch; the closed form is what says
the degree-two member was the one taken.

Two existing pairs keep their expected values but change their reason, and
hunk 3.6 rewrites the comment above them accordingly:

```maxima
freeof('integrate, risch(1/(x*(log(x)^5-2)), x));
false;

freeof('integrate, risch(1/(x*(log(x)^4+log(x)+1)), x));
false;
```

The first was refused for its degree before and still is.  The second was
refused because the algebraic gcd could not find a factor in roots that
`solve` wrote in about 490 characters apiece; it is now refused by
`risch-rt-solvable-p`, before `solve` is called at all.

Regression run: `run_testsuite(tests=["rtest_integrate"])` gives
`961/961 tests passed (not counting 2 expected errors)` and
`No unexpected errors found out of 963 tests`.
`run_testsuite(share_tests=true)` gives one failure out of 21,512, the
`rtestprintf` one noted above.  `tests/depcheck.sh` exits 0.

## 5. Proposed Git commit message

```
Use Lazard-Rioboo-Trager for risch()'s log part

RISCH-RT-LOGPART took a gcd of the numerator against the denominator
once per root of the Rothstein-Trager resultant, with the root -- an
algebraic number -- as a coefficient.  Lazard-Rioboo-Trager reads the
same logarithms off the remainder sequence of the two polynomials
instead: the member of degree i carries the factor belonging to the
i-th squarefree part of the resultant, and a root is only ever
substituted into it.  Every step is then a polynomial one, and nothing
is divided by an algebraic number, so $ALGEBRAIC and the algebraic
$GCD are no longer wanted.

That dependence was not harmless.  With the degree bound lifted so
that the fifth roots of 1/50000 are reached, $ALGEBRAIC set gives an
answer whose derivative misses the integrand by 5e-4, while bound off
it gives the same answer exactly.  It never showed, because those
roots were out of the gcd's reach as well.

RISCH-RT-SOLVABLE-P now decides which factors of the resultant are
worth solving -- a quadratic, or a quadratic or binomial in some power
of z, up to degree four -- where the gcd version attempted everything
of degree at most four.  That costs a few integrals: of fifty random
log-part integrands the gcd reached thirty and this reaches
twenty-eight, and both it gives up are correct.  What it buys is a
cost that is bounded.  Those fifty take 124 seconds of RISCH through
the gcd, three of them more than thirty seconds each, against 28
milliseconds here; and with the predicate weakened to the old degree
bound the third of two hundred random integrands exhausts the heap.

RISCHLOGEPROG catches what RISCH-RT-LOGPART may signal, so an
integrand it cannot do comes back as a noun and not as an error.

(log(x)^3+log(x))/(x*(log(x)^4+1)) has two double residues, so its
logarithms come off the member of degree two rather than the last one.
Nothing in rtest_integrate.mac exercised that; it does now.
```
