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
the resultant, and a root is only ever substituted into it.  No integral
changes; the gain is that no step divides by an algebraic number, so
`$algebraic` and the algebraic `$gcd` are no longer needed -- and, as
section 2 shows, no longer able to do harm.

## 2. Reproducer/Demo

### User-visible behaviour: unchanged

This is a change of method, not of results, so there is no old-versus-new
transcript to show.  Every integral in the Rothstein-Trager test block
returns what it returned before, character for character, and the suite
figures are identical either side of the patch:

```
before:  rtest_integrate 960/960;  share suite 1 failure out of 21,511
after:   rtest_integrate 960/960;  share suite 1 failure out of 21,511
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
  a factor free of the monomial, and that factor is harmless: `risch-rt-monic`
  divides it out, and anything surviving would change a logarithm by the
  logarithm of a constant.  So none of the subresultant normalization is
  needed.
- The degree-0 member of the sequence is the resultant itself, which is a
  free consistency check on the whole construction.

`risch-rt-gcd` disappears.  The guard that the degrees of the gcds must add
up to `deg(DEN)` stays and now guards the substitutions instead.

## 3. Code patching instructions

Four hunks, in file order, all in `src/risch.lisp`, all inside the block the
previous handover adds.  Each anchor was checked to occur exactly once in
the file as that handover leaves it.

### 3.1 The heading comment

**Replace** the whole comment block that begins with this line, down to and
including the line `;; GREAT would have made of it.` (both lines are unique
in the file):

```lisp
;; Rothstein-Trager reduction of the logarithmic part.  For an integrand
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
;; free of the monomial, which is harmless: RISCH-RT-MONIC divides it out,
;; and what survives changes a logarithm by the logarithm of a constant.
;;
;; $RESULTANT seeds VARLIST with its third argument and NEWVAR prepends
;; whatever else it finds, so the monomial stays last in VARLIST, where
;; ORDERPOINTER numbers it highest and POINTERGP ranks it above everything
;; else.  It is the main variable of the resultant whatever GREAT would
;; have made of it.
```

### 3.2 `risch-rt-factors` -- keep the multiplicities

LRT needs each factor's multiplicity, which selects the member of the
remainder sequence.  **Replace** this block:

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

### 3.3 `risch-rt-gcd` gives way to the sequence and the root test

**Replace** this block:

```lisp
(defun risch-rt-gcd (c pm qm dm v)
  (risch-rt-monic (car (errcatch ($gcd ($ratsimp (sub pm (mul c dm))) qm)))
                  v))
```

with these two functions:

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
        (push (cons d b) seq)
        (when (zerop d) (return seq))
        (let ((qr (car (errcatch ($divide a b v)))))
          (unless ($listp qr) (return seq))
          (setq a b
                b ($ratsimp (caddr qr))))))))

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

### 3.4 `risch-rt-logpart`

**Replace** the whole function beginning with this line -- unique in the
file -- down to and including its final closing parenthesis:

```lisp
(defun risch-rt-logpart (num den divisor risch-intvar risch-var)
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
               (s (if (= mult degq) qm (cdr (assoc mult prs))))
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

Then **delete** the three-line comment that sat immediately above that
function; what it said is in the heading comment now:

```lisp
;; The logarithmic part of NUM/DEN as a list of Maxima expressions, or NIL
;; when the integrand has no elementary one or this implementation cannot
;; reach it.
```

## 4. Proposed test cases

**None.**  The patch changes no integral, so there is nothing new to assert,
and no existing expectation changes.  The cases that pin the behaviour this
patch reorganizes are already in the Rothstein-Trager block that the
previous handover adds to `tests/rtest_integrate.mac`, including the two
that pin the bound:

```
freeof('integrate, risch(1/(x*(log(x)^5-2)), x));
false;

freeof('integrate, risch(1/(x*(log(x)^4+log(x)+1)), x));
false;
```

Both still hold, for a different reason than before: the first is now
refused by the degree bound in `risch-rt-solvable-p` and the second by the
test on the gcd of the exponents, where previously the algebraic gcd simply
failed to find a factor.

Regression run: `run_testsuite(tests=["rtest_integrate"])` gives
`960/960 tests passed (not counting 2 expected errors)` and
`No unexpected errors found out of 962 tests`, the same as before the patch.
`run_testsuite(share_tests=true)` gives one failure out of 21,511, the
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
is divided by an algebraic number.

No integral changes.  What changes is that the code no longer depends
on $ALGEBRAIC or on an algebraic $GCD, and that dependence was not
harmless: with the degree bound lifted so that the fifth roots of
1/50000 are reached, $ALGEBRAIC set gives an answer whose derivative
misses the integrand by 5e-4, and $ALGEBRAIC bound off gives the same
answer exactly.  It never showed, because those roots were out of the
gcd's reach as well, but it was waiting for whoever widened the bound.

RISCH-RT-SOLVABLE-P now decides which factors of the resultant are
worth solving, from the degree divided by the gcd of the exponents.  A
quadratic, or a binomial in some power of z, has roots that SOLVE
writes as a surd times a root of unity, and the answer stays readable.
Anything else sends SOLVE to Cardano or Ferrari, and substituting what
comes back gives an antiderivative of a million conses.  The bound of
four stays alongside it: past the fourth root of unity RECTFORM writes
the roots with cosines that RATSIMP will not match up, the conjugate
pairs go unrecognized, and the answer keeps an %i in it.
```
