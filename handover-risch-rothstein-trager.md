# Handover: Rothstein-Trager reduction of risch()'s logarithmic part

## 1. Summary

**Teach `risch()` to finish the logarithmic part of a transcendental integral
when the residues are not already in the integrand's own field.**

`rischlogeprog` divided the numerator by the reduced derivative of the
denominator and gave up unless that quotient was constant, so
`1/(x*(log(x)^2+1))` came back unevaluated although its integral is
`atan(log(x))`.  A new `risch-rt-logpart` supplies those logarithms from the
Rothstein-Trager resultant, factoring it over the rationals, solving each
irreducible factor of degree at most four, and recombining conjugate pairs so
the answer stays real.  The change is additive: it is reached only where
`rischlogeprog` previously returned a noun.

## 2. Reproducer/Demo

Both blocks were executed.  The old one is a build of the unmodified base
commit in a separate worktree, the new one is this branch's build.

**Old** -- pristine tree, `./maxima-local --no-init -q`:

```
(%i3) risch(1/(x*(log(x)^2+1)),x)
(%o3) 'integrate(1/(x*log(x)^2+x),x)
(%i4) risch(1/(x*(log(x)^2-2)),x)
(%o4) 'integrate(1/(x*log(x)^2-2*x),x)
(%i5) risch(1/(x*(log(x)^2+a)),x)
(%o5) 'integrate(1/(x*log(x)^2+a*x),x)
(%i6) risch(1/(x*(log(x)^4+1)),x)
(%o6) 'integrate(1/(x*log(x)^4+x),x)
(%i7) risch(1/(x*log(x)*(log(log(x))^2+1)),x)
(%o7) 'integrate(1/(x*log(x)*log(log(x))^2+x*log(x)),x)
(%i8) risch((2*sqrt(2)*(1-log(x)))/(log(x)^2-2*x^2),x)
(%o8) -'integrate((2^(3/2)*log(x)-2^(3/2))/(log(x)^2-2*x^2),x)
```

**New** -- this branch, same inputs:

```
(%i3) risch(1/(x*(log(x)^2+1)),x)
(%o3) atan(log(x))
(%i4) risch(1/(x*(log(x)^2-2)),x)
(%o4) log(log(x)-sqrt(2))/2^(3/2)-log(log(x)+sqrt(2))/2^(3/2)
(%i5) risch(1/(x*(log(x)^2+a)),x)
(%o5) atan(log(x)/sqrt(a))/sqrt(a)
(%i6) risch(1/(x*(log(x)^4+1)),x)
(%o6) log(log(x)^2+sqrt(2)*log(x)+1)/2^(5/2)
 -log(log(x)^2-sqrt(2)*log(x)+1)/2^(5/2)
 +atan((2*log(x)+sqrt(2))/sqrt(2))/2^(3/2)
 +atan((2*log(x)-sqrt(2))/sqrt(2))/2^(3/2)
(%i7) risch(1/(x*log(x)*(log(log(x))^2+1)),x)
(%o7) atan(log(log(x)))
(%i8) risch((2*sqrt(2)*(1-log(x)))/(log(x)^2-2*x^2),x)
(%o8) log(log(x)-sqrt(2)*x)-log(log(x)+sqrt(2)*x)
```

The last one is not reachable by substitution -- two generators occur in the
denominator -- so `integrate()` gains it too, not only `risch()`:

```
old:  integrate(2*sqrt(2)*(1-log(x))/(log(x)^2-2*x^2), x);
      2^(3/2)*'integrate((1-log(x))/(log(x)^2-2*x^2),x)
new:  2^(3/2)*(log(log(x)-sqrt(2)*x)/2^(3/2)-log(log(x)+sqrt(2)*x)/2^(3/2))
```

### Internals

For `NUM/DEN` with `DEN` monic and squarefree in the topmost monomial and
`deg(NUM) < deg(DEN)`, the logarithmic part is the sum of
`c*log(gcd(NUM - c*DIVISOR, DEN))` over the roots `c` of the resultant
`R(z) = resultant(NUM - z*DIVISOR, DEN)` taken in that monomial, `DIVISOR`
being the reduced derivative `rischlogeprog` has already formed.  `R` made
monic in `z` must be free of the variable of integration, which is exactly
the criterion for the logarithmic part to be elementary; otherwise the noun
stands, now for a reason rather than for want of trying.

Three points a maintainer should not have to rediscover:

- `$resultant` seeds `varlist` with its third argument and `newvar` prepends
  whatever else it finds, so the monomial stays last in `varlist`, where
  `orderpointer` numbers it highest and `pointergp` ranks it above everything
  else.  It is the main variable whatever `great` would have made of it, so
  nothing needs rewriting to arrange that.
- The gcds are taken with the roots exactly as `solve` writes them.
  `rectform` is applied afterwards, to pair the roots and to keep the
  constants readable; feeding a rectified root to `$gcd` is what does not
  terminate, because on the roots of a quartic that does not factor
  `rectform` grows the expression from about 290 to about 10250 conses.
- A conjugate pair is emitted as `s*log(G1*G2) + 2*u*atan(A/B)`, with `s, u`
  the real and imaginary parts of the root and `A, B` those of the gcd, each
  taken as a half sum or half difference of the pair.  Differentiated this is
  an identity for *any* two roots, so the pairing decides only whether the
  answer comes out real, never whether it is correct.

A set of gcds whose degrees do not add up to `deg(DEN)` means a residue was
missed, and the noun stands rather than a sum that is not an antiderivative.

## 3. Code patching instructions

Five hunks, in apply order.  Every anchor below was checked to occur exactly
once in the unmodified file with `git show <base>:<path> | grep -c -F`.

### 3.1 `src/risch.lisp` -- declare the specials `solve` reports through

**Replace** this block:

```lisp
(declare-top (special *mosesflag
                      context *in-risch-p*))
```

with:

```lisp
(declare-top (special *mosesflag
                      context *in-risch-p*
                      ;; SOLVE leaves its results on these.  They are
                      ;; declared, never defined, so each caller declares
                      ;; them for itself.
                      *roots *failures))
```

Without this the `let` in `risch-rt-roots` binds lexically, `solve` pushes
onto the global pair, and no root is ever seen.  These names are earmuffed so
SBCL would warn; an earmuff-free special would have failed silently.

### 3.2 `src/risch.lisp` -- the Rothstein-Trager reduction itself

**Insert** the block below **before** this line, which is unique in the
unmodified file:

```lisp
(defun rischlogeprog (p risch-ratform risch-switch1 risch-intvar risch-expstuff
```

The block, indented with spaces as newly written functions should be:

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

(defun risch-rt-degree (e v)
  (let ((d (car (errcatch ($hipow e v)))))
    (if (integerp d) d -1)))

;; The distinct irreducible factors of E that involve V.
(defun risch-rt-factors (e v)
  (cond ((mtimesp e) (mapcan #'(lambda (f) (risch-rt-factors f v)) (cdr e)))
        ((mexptp e) (risch-rt-factors (cadr e) v))
        ((freeof v e) nil)
        (t (list e))))

(defun risch-rt-monic (g v)
  (let ((d (risch-rt-degree g v)))
    (if (< d 1) nil ($ratsimp (div g ($ratcoef g v d))))))

(defun risch-rt-gcd (c pm qm dm v)
  (risch-rt-monic (car (errcatch ($gcd ($ratsimp (sub pm (mul c dm))) qm)))
                  v))

;; E in rectangular form, which is how SOLVE's (-1)^(1/4) and the like
;; become ordinary numbers.  Only a constant is rewritten: on an expression
;; carrying a symbolic parameter RECTFORM yields atan2 and abs, and on one
;; carrying a logarithm it takes that apart under $DOMAIN complex.
(defun risch-rt-rectform (e)
  (if ($constantp e)
      (or (car (errcatch ($rectform e))) e)
      e))

;; E with each coefficient of a power of V rewritten by RISCH-RT-RECTFORM.
(defun risch-rt-realify (e v)
  (let ((d (risch-rt-degree e v)))
    (if (minusp d)
        e
        (do ((k 0 (1+ k))
             (sum 0))
            ((> k d) sum)
          (setq sum (add sum (mul (risch-rt-rectform ($ratcoef e v k))
                                  (power v k))))))))

;; C with %i negated, which is its conjugate as long as every other symbol
;; in it is real.
(defun risch-rt-conjugate (c)
  ($ratsimp (maxima-substitute (neg '$%i) '$%i c)))

;; Position of an unused root conjugate to the one at I, or NIL.  A real
;; root is its own conjugate and the roots of an irreducible factor are
;; distinct, so a real root never finds a partner.
(defun risch-rt-partner (i cs used)
  (do ((j (1+ i) (1+ j)))
      ((>= j (length cs)) nil)
    (when (and (not (aref used j))
               (zerop1 ($ratsimp (sub (nth i cs)
                                      (risch-rt-conjugate (nth j cs))))))
      (return j))))

;; The terms C*log(G) over the roots of one irreducible factor of R.  A
;; conjugate pair is rewritten as
;;
;;     s*log(G1*G2) + 2*u*atan(A/B)
;;
;; with s, u the real and imaginary parts of C1 and A, B those of G1, each
;; taken as a half sum or half difference of the pair, so that splitting a
;; G apart needs no RECTFORM.  Differentiated, the rewriting is an identity
;; for any two roots; the pairing decides only whether the answer comes out
;; real.
(defun risch-rt-terms (cs gs v)
  (let* ((n (length cs))
         ;; The gcds were taken with the roots as SOLVE wrote them, which is
         ;; the form they divide fastest in.  From here on the rectangular
         ;; form is the one wanted, both to recognize a conjugate pair and
         ;; to keep the constants in the answer readable.
         (cs (mapcar #'risch-rt-rectform cs))
         (used (make-array n :initial-element nil))
         (terms nil))
    (dotimes (i n terms)
      (unless (aref used i)
        (let ((j (risch-rt-partner i cs used)))
          (setf (aref used i) t)
          (cond
            (j
             (setf (aref used j) t)
             (let* ((g1 (nth i gs)) (g2 (nth j gs))
                    (s ($ratsimp (div (add (nth i cs) (nth j cs)) 2)))
                    (u ($ratsimp (div (sub (nth i cs) (nth j cs))
                                      (mul 2 '$%i))))
                    (aa (risch-rt-realify ($ratsimp (div (add g1 g2) 2)) v))
                    (bb (risch-rt-realify
                         ($ratsimp (div (sub g1 g2) (mul 2 '$%i))) v)))
               (unless (zerop1 s)
                 (push (mul s (logmabs (risch-rt-realify
                                        ($ratsimp (mul g1 g2)) v)))
                       terms))
               (unless (or (zerop1 u) (zerop1 bb))
                 (push (mul 2 u (ftake '%atan ($ratsimp (div aa bb))))
                       terms))))
            (t
             (push (mul (nth i cs)
                        (logmabs (risch-rt-realify (nth i gs) v)))
                   terms))))))))

;; The roots of the irreducible F, or NIL when SOLVE cannot list them all.
;; The internal SOLVE leaves them on *ROOTS, alternating with their
;; multiplicities, and whatever it could not solve on *FAILURES.  It is
;; taken in preference to $SOLVE, whose contract is the user's: it assigns
;; $MULTIPLICITIES and reads $PROGRAMMODE, $BREAKUP and the null warnings,
;; every one of which a caller has to bind out of the way.  Being an
;; ordinary DEFUN it is also late bound, so it adds no compile-time
;; dependency on solve.lisp, which is compiled after this file.  A
;; $REALONLY set by the user drops the complex roots, and the count below
;; then rejects what is left rather than integrating with part of the sum
;; missing.
(defun risch-rt-roots (f zvar d)
  (let ((*roots nil) (*failures nil))
    (errcatch (solve f zvar 1))
    (when (and (null *failures) (= (length *roots) (* 2 d)))
      (do ((r *roots (cddr r))
           (roots nil))
          ((null r) roots)
        (let ((e (car r)))
          (unless (and (mequalp e) (eq (cadr e) zvar)
                       (freeof zvar (caddr e)))
            (return nil))
          (push (caddr e) roots))))))

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

### 3.3 `src/risch.lisp` -- call it from `rischlogeprog`

One call, on the path that used to fall straight through to the noun.
**Insert** this block (each line begins with a tab, as its neighbours do):

```lisp
	    (unless risch-expflag
	      (let ((rt (risch-rt-logpart p1e p2e my-divisor risch-intvar
					  risch-var)))
		(when rt (return (cons (rzero) rt)))))
```

**before** this block, whose first line is unique in the unmodified file:

```lisp
	    (if (and risch-expflag
		     $liflag
		     *changevp*)
```

The guard keeps the reduction off exponential monomials, which already have
the `$changevar` branch below to fall back on.

### 3.4 `tests/rtest_integrate.mac` -- the new cases

**Insert** the block given in section 4 **before** this banner, whose middle
line is unique in the unmodified file:

```
/**************************************/
/* Leave this at the end of the file! */
/**************************************/
```

It has to go *before* the banner rather than at the end of the file, so that
the closing `[facts(), contexts]` pair stays last.  That shifts only this one
pair's problem number, and no entry in `src/testsuite.lisp` refers to it --
`rtest_integrate`'s registered failures are 826 and 827.

### 3.5 `ChangeLog` -- one entry under "Other changes"

**Insert** this block:

```
* risch reduces the logarithmic part of a transcendental integral with
  the Rothstein-Trager resultant, so residues outside the field the
  integrand is written over no longer stop it: risch(1/(x*(log(x)^2+1)),
  x) is atan(log(x)) rather than a noun.  Residues of degree up to four
  over the rationals are reached, and conjugate pairs are recombined so
  that the answer stays real
```

**before** this line, unique in the unmodified file:

```
* New function read_struct: reads a .csv file whose first line names
```

## 4. Proposed test cases

All new, all for `tests/rtest_integrate.mac`, all executed -- the expected
results below are observed output, not predictions.  This file sets
`domain:complex` and `radexpand:true` near the top, so the block runs under
those.

```
/* Rothstein-Trager reduction of the logarithmic part in rischint.  The
 * residues of these integrands are not in the field the integrand is
 * written over, so RISCHLOGEPROG's test for a constant coefficient fails
 * and RISCH-RT-LOGPART supplies the logarithms.
 */

risch(1/(x*(log(x)^2-2)), x);
log(log(x)-sqrt(2))/2^(3/2)-log(log(x)+sqrt(2))/2^(3/2);

risch(1/(x*(log(x)^2+1)), x);
atan(log(x));

risch(1/(x*(log(x)^2+log(x)+1)), x);
2*atan((2*log(x)+1)/sqrt(3))/sqrt(3);

risch(1/(x*log(x)*(log(log(x))^2+1)), x);
atan(log(log(x)));

/* Hermite reduction first, then Rothstein-Trager on what is left. */

risch(1/(x*(log(x)^2+1)^2), x);
atan(log(x))/2+log(x)/(2*log(x)^2+2);

/* Two generators, so the answer is not reachable by substitution. */

block([f : 2*sqrt(2)*(1-log(x))/(log(x)^2-2*x^2), F],
 F : risch(f, x),
 [freeof('integrate, F), ratsimp(radcan(diff(F, x) - f))]);
[true, 0];

/* A quadratic factor whose roots carry a symbolic parameter. */

risch(1/(x*(log(x)^2+a)), x);
atan(log(x)/sqrt(a))/sqrt(a);

/* Factors of degree three and four: SOLVE supplies the roots and the
 * conjugate pairs are recombined, so the answer stays real.
 */

risch(1/(x*(log(x)^4+1)), x);
log(log(x)^2+sqrt(2)*log(x)+1)/2^(5/2)
 -log(log(x)^2-sqrt(2)*log(x)+1)/2^(5/2)
 +atan((2*log(x)+sqrt(2))/sqrt(2))/2^(3/2)
 +atan((2*log(x)-sqrt(2))/sqrt(2))/2^(3/2);

block([f : 1/(x*(log(x)^3-2)), F],
 F : risch(f, x),
 [freeof('integrate, F), freeof(%i, F), ratsimp(radcan(diff(F, x) - f))]);
[true, true, 0];

/* R factors into pieces of degree at most four here, although it is of
 * degree six.
 */

block([f : 1/(x*(log(x)^6+1)), F],
 F : risch(f, x),
 [freeof('integrate, F), ratsimp(radcan(diff(F, x) - f))]);
[true, 0];

/* A factor of degree five is left alone, and so is a quartic whose roots
 * SOLVE only writes down in a form too big to compute a gcd with.
 */

freeof('integrate, risch(1/(x*(log(x)^5-2)), x));
false;

freeof('integrate, risch(1/(x*(log(x)^4+log(x)+1)), x));
false;

/* No elementary logarithmic part: the Rothstein-Trager resultant has roots
 * that are not constants.
 */

risch(1/(log(x)^2-2), x);
'integrate(1/(log(x)^2-2), x);
```

Regression run: `run_testsuite(tests=["rtest_integrate"])` gives
`960/960 tests passed (not counting 2 expected errors)` and
`No unexpected errors found out of 962 tests`; it was 947 before, so the
thirteen problems above are the whole difference and no pre-existing
expectation needed changing.  The whole suite including share tests,
`run_testsuite(share_tests=true)`, gives one failure out of 21,511, which is
`share/stringproc/rtestprintf.mac` problem 38 and predates this work: it is
SBCL's `~e` float printing, reproducible with no Maxima code in the picture
as `(format nil "~e" 42.0d0)` returning `4.2000000000000004e+1` under SBCL
2.2.9.  `tests/depcheck.sh` exits 0.

## 5. Proposed Git commit message

```
Rothstein-Trager for risch()'s logarithmic part

risch() could take the logarithmic part of a transcendental integral
only when the residue already lay in the field the integrand was
written over.  An integrand as ordinary as 1/(x*(log(x)^2+1)) came
back unevaluated, although its integral is atan(log(x)): the residues
are +-%i/2, and the test for a constant coefficient does not see them.
Exponential monomials had the changevar() branch to fall back on;
logarithmic ones had nothing.

RISCH-RT-LOGPART now recovers those logarithms from the
Rothstein-Trager resultant of the integrand's numerator over its
denominator, taken in the topmost monomial.  Every root of that
resultant has to be a constant for the logarithmic part to be
elementary, and each root contributes the logarithm of one gcd.  Roots
of an irreducible factor of degree up to four are reached, and a
conjugate pair is recombined so that the answer comes out real instead
of as two complex logarithms.

Two things are worth knowing for anyone changing this.  $RESULTANT
already makes its third argument the main variable of the
rational-function ordering, by seeding VARLIST with it, so nothing has
to be rewritten to put the monomial on top.  And the gcds are taken
with the roots exactly as SOLVE writes them: RECTFORM is wanted for the
answer and for pairing the roots, but on the roots of a quartic that
does not factor it grows the expression thirtyfold, and the gcds then
do not finish.

*ROOTS and *FAILURES are declared and never defined, so this file
declares them for itself.  Without that the binding around SOLVE would
be lexical and the roots would never be seen.
```
