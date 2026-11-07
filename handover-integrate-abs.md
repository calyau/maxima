# Handover: `integrate()` of integrands with `abs()` of a linear polynomial

## 1. Summary

**`integrate()` now integrates integrands with `abs(p)`, p linear in x, such as `x^2*abs(x)`, `1/abs(5*x+3)` and the many Rubi integrands with `sqrt(c*x^2)` or `(c*(a+b*x)^2)^(k/2)`.**
On either side of the zero r of p, `abs(p)` is `p` or `-p`. When the integrator leaves an integral, `SININT` integrates both versions, joins them with `abs(p)/p`, continuously at r where the antiderivative is finite there, and gives up in every case it cannot do safely (nothing changes with `domain : complex`). `defint` does not use the new rule for principal values.
Based on SourceForge master `45469d8` ("Don't simplify unevaluated block() and (...)", 2026-11-07), built with SBCL 2.2.9 (the Ubuntu package, since a newer SBCL could not be installed in this environment).

## 2. Reproducer/Demo

User-visible behavior, executed with `./maxima-local --no-init -q --batch=demo.mac` (labels as `batch` prints them).

**Old (master `45469d8`):**

```
(%i2) display2d:false
(%i3) integrate(x^2*abs(x),x)
(%o3) 'integrate(x^2*abs(x),x)
(%i4) integrate(1/abs(5*x+3),x)
(%o4) 'integrate(1/abs(5*x+3),x)
(%i5) integrate(sqrt(c*x^2)/(x*(a+b*x)),x)
(%o5) sqrt(c)*'integrate(x/((b*x+a)*abs(x)),x)
(%i6) integrate((c*x^2)^(5/2)/x^5,x)
(%o6) c^(5/2)*'integrate(x/abs(x),x)
(%i7) integrate(x*sqrt(a+b*sqrt(c*x^2)),x)
(%o7) 'integrate(x*sqrt(b*sqrt(c)*abs(x)+a),x)
(%i8) integrate(1/(1+(x^2)^(3/2)),x)
(%o8) 'integrate(1/(x^2*abs(x)+1),x)
(%i9) integrate(abs(x-1)/(x+1),x)
(%o9) 'integrate(abs(x-1)/(x+1),x)
(%i10) integrate(x^2*abs(x),x,-1,1)
(%o10) 'integrate(x^2*abs(x),x,-1,1)
(%i11) integrate(abs(x-1)/(x+1),x,0,2)
(%o11) 'integrate(abs(x-1)/(x+1),x,0,2)
(%i12) integrate(abs(sin(x)),x)
(%o12) 'integrate(abs(sin(x)),x)
```

**New:**

```
(%i2) display2d:false
(%i3) integrate(x^2*abs(x),x)
(%o3) (x^3*abs(x))/4
(%i4) integrate(1/abs(5*x+3),x)
(%o4) ((5*x+3)*log(abs(5*x+3)))/(5*abs(5*x+3))
(%i5) integrate(sqrt(c*x^2)/(x*(a+b*x)),x)
(%o5) (x*((sqrt(c)*log(b*x+a))/b-(log(a)*sqrt(c))/b))/abs(x)
(%i6) integrate((c*x^2)^(5/2)/x^5,x)
(%o6) c^(5/2)*abs(x)
(%i7) integrate(x*sqrt(a+b*sqrt(c*x^2)),x)
(%o7) (2*(b*sqrt(c)*abs(x)+a)^(5/2))/(5*b^2*c)
 -(2*a*(b*sqrt(c)*abs(x)+a)^(3/2))/(3*b^2*c)
(%i8) integrate(1/(1+(x^2)^(3/2)),x)
(%o8) (x*(atan((2*abs(x)-1)/sqrt(3))/sqrt(3)
         +log(abs(x)+1)/3-log(-abs(x)+x^2+1)/6+%pi/(2*3^(3/2))))
 /abs(x)
(%i9) integrate(abs(x-1)/(x+1),x)
(%o9) ((x-1)*(-2*log(x+1)+x+2*log(2)-1))/abs(x-1)
(%i10) integrate(x^2*abs(x),x,-1,1)
(%o10) 1/2
(%i11) integrate(abs(x-1)/(x+1),x,0,2)
(%o11) 4*log(2)-2*log(3)
(%i12) integrate(abs(sin(x)),x)
(%o12) 'integrate(abs(sin(x)),x)
```

**Internals.** When `INTEGRATOR` leaves an integral, `SININT` calls the new `INTEGRATE-ABS`. It applies only if the integrand has exactly one x-dependent `abs(p)`, p = b*x+a, `$csign` finds p real, `domain` is `real`, and it is neither inside another integration (`*integrate-abs*`) nor inside Risch's re-evaluation of nouns (`*in-risch-p*`). It integrates `FP` (abs(p) → p) and, where needed, `GM(y) = FM(2*r-y)` (abs(p) → -p, reflected to the side p > 0), evaluates the antiderivatives only at `y = (abs(p)-a)/b`, and combines them with `s = abs(p)/p`:

- Symmetry about r gives the short forms `s*(F(y)-F(r))` and `F(y)`, which also turn `log(p)` into `log(abs(p))`.
- If `FM = -FP`, it uses `s*(F-F(r))`, unless F has radicands that FP lacks (the integrator may have split a root into roots that hold only for p > 0).
- Antiderivatives with `%e^(q*log(u))`, u depending on x, are rejected (split or merged powers that disagree with Maxima's powers where u < 0).
- F(r) comes from plain substitution with errors caught. It only affects continuity, never correctness, because `s` is constant on each side, and it never asks a question.

`PRINCIPAL-VALUE-INTEGRAL` in `defint.lisp` binds `*integrate-abs*` while it gets the antiderivative: `TAKE-PRINCIPAL` can ask for the sign of the variable while looking for discontinuities (`RADMABS` under `LIMITP`), and the answer would make `abs(p)/p` constant on the whole interval.

## 3. Bug report

**Title:**
```
integrate() fails on integrands with abs() of a linear polynomial, e.g. sqrt(c*x^2)
```

**Body:**
````markdown
`integrate()` integrates `abs(x)`, but not `x^2*abs(x)`, `x/abs(x)`, `1/abs(x)` or `1/abs(5*x+3)`, although they are elementary on either side of the zero of the argument of `abs`:

```
(%i1) integrate(x^2*abs(x), x);
(%o1) 'integrate(x^2*abs(x),x)
(%i2) integrate(1/abs(5*x+3), x);
(%o2) 'integrate(1/abs(5*x+3),x)
```

Such integrands are common because the simplifier creates them: with the default `radexpand : true`, `sqrt(c*x^2)` becomes `sqrt(c)*abs(x)` and `(c*x^2)^(5/2)` becomes `c^(5/2)*x^4*abs(x)`. So most integrals with `sqrt(c*x^2)` or `(c*(a+b*x)^2)^p` from the Rubi test suite (sections 1.1.1.2 and 1.1.3.2) stay unevaluated, even simple ones:

```
(%i3) integrate((c*x^2)^(5/2)/x^5, x);
(%o3) c^(5/2)*'integrate(x/abs(x),x)
(%i4) integrate(sqrt(c*x^2)/(x*(a+b*x)), x);
(%o4) sqrt(c)*'integrate(x/((b*x+a)*abs(x)),x)
```

Definite integrals across the zero fail the same way:

```
(%i5) integrate(x^2*abs(x), x, -1, 1);
(%o5) 'integrate(x^2*abs(x),x,-1,1)
```
````

## 4. Code patching instructions

All anchors are grep-verified to occur exactly once in master `45469d8`. Keep the tabs in the `src/sin.lisp` and `src/defint.lisp` lines that have them.

### `src/sin.lisp`: guard variable

New special `*INTEGRATE-ABS*`. **Insert** this block

```lisp
(defvar *integrate-abs* nil
  "True while SININT or INTEGRATE-ABS integrates, so that INTEGRATE-ABS is
  tried only on the integrand of an outermost SININT.")
```

**after** this block, with a blank line between them:

```lisp
(defvar *risch-use-triginv-all* nil
  "Dynamically bound flag set to T by TRIGINT to tell INTEGRATOR to use
  $TRIGINVERSES = '$ALL when ultimately trying RISCHINT")
```

### `src/sin.lisp`: `EXP-LOG-P` and `INTEGRATE-ABS`

The new rule. **Insert** this block, followed by a blank line,

```lisp
;; Does E contain %e^q with a log of something depending on VAR2 in q? The
;; integrator writes some powers it splits or merges that way, and then they
;; can disagree with Maxima's powers where their base is negative.
(defun exp-log-p (e var2)
  (and (not (atom e))
       (if (and (eq (caar e) 'mexpt) (eq (cadr e) '$%e))
         (involve-var (caddr e) var2 '(%log))
         (some #'(lambda (u) (exp-log-p u var2)) (cdr e)))))

;; Integrate EXPR, which contains abs(p) for a single real p = b*VAR2+a, over
;; the reals, or return NIL. Let r be the zero of p and s = abs(p)/p. Where
;; p > 0, EXPR is FP, EXPR with p for abs(p), with antiderivative F. Where
;; p < 0, it is FM, EXPR with -p for abs(p), and GM(y) = FM(2*r-y), with
;; antiderivative G, moves that side to y where p > 0. Taking F and G only at
;; y = (abs(p)-a)/b, which is where p > 0, (F(y)-G(y))/2 + s*(F(y)+G(y))/2 is
;; an antiderivative on either side of r, and subtracting s*(F(r)+G(r))/2
;; makes it continuous at r. This is s*(F(y)-F(r)) if GM = FP and F(y) if
;; GM = -FP. If FM = -FP, s*(F-F(r)) is one, too, unless F has radicands that
;; FP lacks, as when the integrator splits a root into roots that hold only
;; where p > 0. With domain : complex, VAR2 may be complex: nothing is done.
(defun integrate-abs (expr var2)
  (let* ((ps (gather-args-of expr 'mabs var2))
         (p (car ps))
         (ab (and (eq $domain '$real)
                  (not (or *integrate-abs* *in-risch-p*))
                  p (null (cdr ps))
                  (m2-b*x+a p var2))))
    (when (and ab (not (member ($csign p) '($complex $imaginary))))
      (let* ((*integrate-abs* t)
             (a (cdras 'a ab))
             (b (cdras 'b ab))
             (r (div (neg a) b))
             (abs-p (ftake 'mabs p))
             (s (div abs-p p))
             (fp (maxima-substitute p abs-p expr))
             (fm (maxima-substitute (neg p) abs-p expr))
             (gm (maxima-substitute (sub (mul 2 r) var2) var2 fm)))
        (labels ((integ (e)
                   (let ((*integrator-level* 0)
                         $opsubst varlist genvar *integrator-stack*)
                     (declare (special *integrator-level*))
                     (let ((v (simplify (integrator e var2 nil))))
                       (unless (or (isinop v '%integrate) (exp-log-p v var2))
                         v))))
                 (equiv (e1 e2)
                   (zerop1 (let (($ratprint nil)) ($ratsimp (sub e1 e2)))))
                 (at-r (e)
                   (or (car (let (($errormsg nil))
                              (errcatch (maxima-substitute r var2 e))))
                       0))
                 (at-abs (e)
                   (maxima-substitute (div (sub abs-p a) b) var2 e))
                 (radicands (e)
                   (cond ((atom e) nil)
                         ((and (mexptp e)
                               (not (integerp (caddr e)))
                               (not (freeof var2 (cadr e))))
                          (cons (cadr e) (radicands (cadr e))))
                         (t (mapcan #'radicands (cdr e))))))
          (let ((f (integ fp))
                g)
            (cond ((null f) nil)
                  ((equiv gm fp)
                   (mul s (sub (at-abs f) (at-r f))))
                  ((equiv gm (neg fp))
                   (at-abs f))
                  ((and (equiv fm (neg fp))
                        (subsetp (radicands f) (radicands fp) :test #'alike1))
                   (mul s (sub f (at-r f))))
                  ((setq g (integ gm))
                   (let ((u (at-abs f))
                         (v (at-abs g)))
                     (add (div (sub u v) 2)
                          (mul s (sub (div (add u v) 2)
                                      (at-r (div (add f g) 2))))))))))))))
```

**before** this line:

```lisp
;; This is the top level of the integrator
```

### `src/sin.lisp`: try the rule in `SININT`

Bind the guard during the normal integration and try `INTEGRATE-ABS` when it leaves an integral. **Replace** this block (the end of `SININT`)

```lisp
      ((let ((ans (simplify
                     (let ($opsubst varlist genvar *integrator-stack*)
		       (integrator expr var2 nil)))))
	     (if (sum-of-intsp ans var2)
		 (list '(%integrate) expr var2)
		 ans)))))))
```

**with** this block:

```lisp
      ((let ((ans (simplify
                     (let ($opsubst varlist genvar *integrator-stack*
                           (*integrate-abs* t))
		       (integrator expr var2 nil)))))
	 (cond ((and (isinop ans '%integrate) (integrate-abs expr var2)))
	       ((sum-of-intsp ans var2) (list '(%integrate) expr var2))
	       (t ans))))))))
```

### `src/defint.lisp`: no rule for principal values

In `PRINCIPAL-VALUE-INTEGRAL`, **replace** this line

```lisp
	((not (null (setq anti-deriv (antideriv exp ivar))))
```

**with** this block:

```lisp
	;; Not by INTEGRATE-ABS: looking for discontinuities, TAKE-PRINCIPAL
	;; can ask for the sign of IVAR, which would turn abs(p)/p in the
	;; antiderivative into a constant on the whole interval.
	((not (null (setq anti-deriv (let ((*integrate-abs* t))
				       (antideriv exp ivar)))))
```

### `tests/rtest_integrate.mac`: new tests

**Insert** the new tests of section 5, followed by two blank lines, **before** this block:

```
/**************************************/
/* Leave this at the end of the file! */
/**************************************/
```

### `share/contrib/integration/rtest_abs_integrate.mac`: re-baselined expectations

**Replace** the 13 expected results listed in section 5 (each old expected line occurs once in the file).

### The whole change as a unified diff

Against `45469d8`. `git apply --check` accepts it on a pristine checkout of that commit.

```diff
diff --git a/share/contrib/integration/rtest_abs_integrate.mac b/share/contrib/integration/rtest_abs_integrate.mac
index da7cd58..088df80 100644
--- a/share/contrib/integration/rtest_abs_integrate.mac
+++ b/share/contrib/integration/rtest_abs_integrate.mac
@@ -29,10 +29,10 @@ integrate(abs(%pi * x + 1),x);
 ((%pi*x+1)*abs(%pi*x+1))/(2*%pi)$
 
 integrate(x*abs(x),x);
-(x^3*signum(x))/3$
+(x^2*abs(x))/3$
 
 (assume(n > 2), integrate(abs(x) * x^n,x));
-(x^(n+2)*signum(x))/(n+2)$
+x^(n+3)/((n+2)*abs(x))$
 
 (forget(n > 2),0);
 0$
@@ -151,7 +151,7 @@ integrate(x*abs(x),x,-a,a);
 0$
 
 integrate(x^2*abs(x),x,-a,a);
-(a^4*signum(a))/2$
+(a^3*abs(a))/2$
 
 integrate(unit_step(x),x,-1,0);
 0$
@@ -164,7 +164,7 @@ abs(b)/2+b/2-abs(a)/2-a/2$
 
 /* With hyper_int, we get an interesting result... */
 (i : integrate(f(x) * abs(x-a),x), ev(i, f(x) := x, 'integrate));
-(((2*x^3)/3-a*x^2+a^3/3)*signum(x-a))/2$
+((x-a)*((2*x^3-3*a*x^2)/6+a^3/6))/abs(x-a)$
 
 diff(i,x);
 f(x)*abs(x-a)$
@@ -225,19 +225,19 @@ is(equal(op(integrate(signum(x + sqrt(1-x)),x)), nounify('integrate)));
 true$
 
 i : integrate(abs(x-1)/ ( 1 + x^2),x);
-signum(x-1)*(log(x^2+1)/2-atan(x)-log(2)/2+%pi/4)$
+((x-1)*(log(x^2+1)/2-atan(x)-log(2)/2+%pi/4))/abs(x-1)$
 
 [unwind_protect((assume(x<1), ratsimp(diff(i,x))), forget(x < 1)), unwind_protect((assume(x>1), ratsimp(diff(i,x))), forget(x>1))];
 [-((x-1)/(x^2+1)),(x-1)/(x^2+1)]$
 
 integrate(exp(-abs(x)), x);
-((-%e^x-%e^(-x)+2)*signum(x))/2+(%e^x-%e^(-x))/2$
+((1-%e^-abs(x))*x)/abs(x)$
 
 ratsimp(integrate(cos(x + abs(x)),x));
-((signum(x)+1)*sin(2*x)-2*x*signum(x)+2*x)/4$
+((abs(x)+x)*sin(2*abs(x))+2*x*abs(x)-2*x^2)/(4*abs(x))$
 
 signum_to_abs(integrate(diff(abs(x) * cos(x),x),x));
-cos(x)*abs(x)$
+abs(x)*cos(abs(x))$
 
 integrate(abs(x) * exp(-abs(x-9)),x,minf,inf);
 18 + 2 * exp(-9)$
@@ -430,7 +430,7 @@ integrate(1/(1+abs(x)),x,-a,a),ratsimp;
 0$
 
 i : integrate(1/(1+abs(x)),x);
-signum(x)*(log(x+1)+log(1-x))/2+(log(x+1)-log(1-x))/2$
+(x*log(abs(x)+1))/abs(x)$
 
 [limit(i,x,-1,minus)-limit(i,x,-1,plus), limit(i,x,1,minus)-limit(i,x,1,plus)];
 [0,0]$
@@ -462,7 +462,7 @@ integrate(signum(abs(x)),x,0,2);
 /* SF bug ID: 3034415; the antidervative is non-ideal, but at least it no longer signals an error. */
 
 integrate(1/abs(x),x);
-'integrate(1/abs(x),x)$
+(x*log(abs(x)))/abs(x)$
 
 (extra_integration_methods : cons('conditional_integrate,extra_integration_methods),0);
 0$
@@ -538,7 +538,7 @@ integrate(signum(abs(x)),x,-2,2), prederror : false;
 4$
 
 integrate(sqrt(1+abs(x)),x);
-(((2*(x+1)^(3/2))/3+(2*(1-x)^(3/2))/3-4/3)*signum(x))/2+((2*(x+1)^(3/2))/3-(2*(1-x)^(3/2))/3)/2$
+(x*((2*(abs(x)+1)^(3/2))/3-2/3))/abs(x)$
 
 (i : integrate(sqrt(max(5,x)),x),0);
 0$
@@ -700,10 +700,10 @@ integrate(1/(1+abs(x)),x,-1,1);
 2*log(2)$
 
 integrate(1/(1+abs(x)),x,-1/2,1/2);
-(log(2)+log(3/2))/2-log(2)-((-log(2)-log(3/2))/2)+log(3/2)$
+2*log(3/2)$
 
 integrate(1/(1+abs(x)),x,-107,107);
-(log(108)-log(106)-%i*%pi)/2+log(108)-((-log(108)+log(106)+%i*%pi)/2)+log(106)+%i*%pi$
+2*log(108)$
 
 /* bugs #3519 & #3641 */
 integrate(abs(sin(x)),x,0,2*%pi);
diff --git a/src/defint.lisp b/src/defint.lisp
index 5934d06..ca96a68 100644
--- a/src/defint.lisp
+++ b/src/defint.lisp
@@ -673,7 +673,11 @@ in the interval of integration.")
 	   (let ((*pcprntd* t))
 	     (principal-value-integral exp ivar ll ul poles)))
 	  ((and (symmetric-intervalp ll ul) (oddfn exp ivar)) (principal) 0) ;odd integrand, symmetric interval
-	((not (null (setq anti-deriv (antideriv exp ivar))))
+	;; Not by INTEGRATE-ABS: looking for discontinuities, TAKE-PRINCIPAL
+	;; can ask for the sign of IVAR, which would turn abs(p)/p in the
+	;; antiderivative into a constant on the whole interval.
+	((not (null (setq anti-deriv (let ((*integrate-abs* t))
+				       (antideriv exp ivar)))))
 	   (cond ((not (null poles))
 		  (multiple-value-bind (ignore new-ll new-ul)
                       (order-limits 'ask ivar ll ul)
diff --git a/src/sin.lisp b/src/sin.lisp
index bbed4ce..6d50263 100644
--- a/src/sin.lisp
+++ b/src/sin.lisp
@@ -31,6 +31,10 @@
   "Dynamically bound flag set to T by TRIGINT to tell INTEGRATOR to use
   $TRIGINVERSES = '$ALL when ultimately trying RISCHINT")
 
+(defvar *integrate-abs* nil
+  "True while SININT or INTEGRATE-ABS integrates, so that INTEGRATE-ABS is
+  tried only on the integrand of an outermost SININT.")
+
 (defvar *integrator-stack* nil
   "The integrals that the enclosing calls of INTEGRATOR are computing for an
   integration by parts, each as a cons of the integrand and its variable. It
@@ -1885,6 +1889,82 @@
 (defmvar $integration_constant_counter 0)
 (defmvar $integration_constant '$%c)
 
+;; Does E contain %e^q with a log of something depending on VAR2 in q? The
+;; integrator writes some powers it splits or merges that way, and then they
+;; can disagree with Maxima's powers where their base is negative.
+(defun exp-log-p (e var2)
+  (and (not (atom e))
+       (if (and (eq (caar e) 'mexpt) (eq (cadr e) '$%e))
+         (involve-var (caddr e) var2 '(%log))
+         (some #'(lambda (u) (exp-log-p u var2)) (cdr e)))))
+
+;; Integrate EXPR, which contains abs(p) for a single real p = b*VAR2+a, over
+;; the reals, or return NIL. Let r be the zero of p and s = abs(p)/p. Where
+;; p > 0, EXPR is FP, EXPR with p for abs(p), with antiderivative F. Where
+;; p < 0, it is FM, EXPR with -p for abs(p), and GM(y) = FM(2*r-y), with
+;; antiderivative G, moves that side to y where p > 0. Taking F and G only at
+;; y = (abs(p)-a)/b, which is where p > 0, (F(y)-G(y))/2 + s*(F(y)+G(y))/2 is
+;; an antiderivative on either side of r, and subtracting s*(F(r)+G(r))/2
+;; makes it continuous at r. This is s*(F(y)-F(r)) if GM = FP and F(y) if
+;; GM = -FP. If FM = -FP, s*(F-F(r)) is one, too, unless F has radicands that
+;; FP lacks, as when the integrator splits a root into roots that hold only
+;; where p > 0. With domain : complex, VAR2 may be complex: nothing is done.
+(defun integrate-abs (expr var2)
+  (let* ((ps (gather-args-of expr 'mabs var2))
+         (p (car ps))
+         (ab (and (eq $domain '$real)
+                  (not (or *integrate-abs* *in-risch-p*))
+                  p (null (cdr ps))
+                  (m2-b*x+a p var2))))
+    (when (and ab (not (member ($csign p) '($complex $imaginary))))
+      (let* ((*integrate-abs* t)
+             (a (cdras 'a ab))
+             (b (cdras 'b ab))
+             (r (div (neg a) b))
+             (abs-p (ftake 'mabs p))
+             (s (div abs-p p))
+             (fp (maxima-substitute p abs-p expr))
+             (fm (maxima-substitute (neg p) abs-p expr))
+             (gm (maxima-substitute (sub (mul 2 r) var2) var2 fm)))
+        (labels ((integ (e)
+                   (let ((*integrator-level* 0)
+                         $opsubst varlist genvar *integrator-stack*)
+                     (declare (special *integrator-level*))
+                     (let ((v (simplify (integrator e var2 nil))))
+                       (unless (or (isinop v '%integrate) (exp-log-p v var2))
+                         v))))
+                 (equiv (e1 e2)
+                   (zerop1 (let (($ratprint nil)) ($ratsimp (sub e1 e2)))))
+                 (at-r (e)
+                   (or (car (let (($errormsg nil))
+                              (errcatch (maxima-substitute r var2 e))))
+                       0))
+                 (at-abs (e)
+                   (maxima-substitute (div (sub abs-p a) b) var2 e))
+                 (radicands (e)
+                   (cond ((atom e) nil)
+                         ((and (mexptp e)
+                               (not (integerp (caddr e)))
+                               (not (freeof var2 (cadr e))))
+                          (cons (cadr e) (radicands (cadr e))))
+                         (t (mapcan #'radicands (cdr e))))))
+          (let ((f (integ fp))
+                g)
+            (cond ((null f) nil)
+                  ((equiv gm fp)
+                   (mul s (sub (at-abs f) (at-r f))))
+                  ((equiv gm (neg fp))
+                   (at-abs f))
+                  ((and (equiv fm (neg fp))
+                        (subsetp (radicands f) (radicands fp) :test #'alike1))
+                   (mul s (sub f (at-r f))))
+                  ((setq g (integ gm))
+                   (let ((u (at-abs f))
+                         (v (at-abs g)))
+                     (add (div (sub u v) 2)
+                          (mul s (sub (div (add u v) 2)
+                                      (at-r (div (add f g) 2))))))))))))))
+
 ;; This is the top level of the integrator
 (defun sinint (expr var2)
  (with-new-context (context)
@@ -1932,11 +2012,12 @@
        0)
       
       ((let ((ans (simplify
-                     (let ($opsubst varlist genvar *integrator-stack*)
+                     (let ($opsubst varlist genvar *integrator-stack*
+                           (*integrate-abs* t))
 		       (integrator expr var2 nil)))))
-	     (if (sum-of-intsp ans var2)
-		 (list '(%integrate) expr var2)
-		 ans)))))))
+	 (cond ((and (isinop ans '%integrate) (integrate-abs expr var2)))
+	       ((sum-of-intsp ans var2) (list '(%integrate) expr var2))
+	       (t ans))))))))
 
 ;; SUM-OF-INTSP
 ;;
diff --git a/tests/rtest_integrate.mac b/tests/rtest_integrate.mac
index f5f5341..410a4a2 100644
--- a/tests/rtest_integrate.mac
+++ b/tests/rtest_integrate.mac
@@ -7532,6 +7532,50 @@ block([s : make_string_output_stream()],
 ['integrate(x/(x^5-x-1)+1, x, 0, 1), 'integrate((x^6-1)/(x^5-x-1), x, 0, 1)]$
 
 
+/* integrate() with abs(p), p linear in x: abs(p) is p or -p on either side of the zero of p.
+   sqrt(c*x^2) simplifies to sqrt(c)*abs(x). */
+
+[integrate(x^2*abs(x), x), integrate(x/abs(x), x), integrate(1/abs(x), x), integrate(1/abs(5*x+3), x),
+ integrate(sqrt(abs(x)), x)];
+[x^3*abs(x)/4, abs(x), x*log(abs(x))/abs(x), (5*x+3)*log(abs(5*x+3))/(5*abs(5*x+3)), 2*x*sqrt(abs(x))/3]$
+
+[integrate(sqrt(c*x^2)/(x*(a+b*x)^2), x), integrate(sqrt(c*x^2)/(x*(a+b*x)), x)];
+[x*(sqrt(c)/(a*b)-sqrt(c)/(b*(b*x+a)))/abs(x), x*(sqrt(c)*log(b*x+a)/b-log(a)*sqrt(c)/b)/abs(x)]$
+
+integrate(x^n*abs(x), x), ask_hide_prompt; no;
+x^(n+3)/((n+2)*abs(x))$
+
+[integrate(1/(1+abs(x)^3), x), integrate(x*sqrt(1+abs(x)), x)];
+[x*(atan((2*abs(x)-1)/sqrt(3))/sqrt(3)+log(abs(x)+1)/3-log(-abs(x)+x^2+1)/6+%pi/(2*3^(3/2)))/abs(x),
+ 2*(abs(x)+1)^(5/2)/5-2*(abs(x)+1)^(3/2)/3]$
+
+/* Continuous at the zero of p */
+
+block([F : integrate(abs(x-1)/(x+1), x)],
+  [F, limit(F, x, 1, minus), limit(F, x, 1, plus)]);
+[(x-1)*(-2*log(x+1)+x+2*log(2)-1)/abs(x-1), 0, 0]$
+
+/* Antiderivatives on both sides of 0 */
+
+map(lambda([f], block([F : integrate(f, x)],
+    map(lambda([s], radcan(subst(abs(x) = s*x, diff(F, x) - f))), [1, -1]))),
+  [1/(1+abs(x)^3), x*sqrt(1+abs(x)), (x-sqrt(x^6))/(x*(1-x^4)), abs(x)/sqrt(x^2+x)]);
+[[0, 0], [0, 0], [0, 0], [0, 0]]$
+
+/* Definite integrals across the zero of p */
+
+[integrate(x^2*abs(x), x, -1, 1), integrate(abs(x-1)/(x+1), x, 0, 2), integrate(sqrt(abs(x)), x, -1, 4),
+ integrate(x/abs(x), x, -1, 2), integrate(abs(x)/(1+x^2), x, -1, 2)];
+[1/2, 4*log(2)-2*log(3), 6, 1, log(5)/2+log(2)/2]$
+
+/* Not done: abs of a non-polynomial, two abs, p not linear, p maybe complex, domain : complex */
+
+[integrate(abs(sin(x)), x), integrate(abs(x)*abs(x-1), x), integrate(abs(x^2-1), x),
+ integrate(x^2*abs(x+sqrt(a)), x), block([domain : complex], integrate(x^2*abs(x), x))];
+['integrate(abs(sin(x)), x), 'integrate(abs(x)*abs(x-1), x), 'integrate(abs(x^2-1), x),
+ 'integrate(x^2*abs(x+sqrt(a)), x), 'integrate(x^2*abs(x), x)]$
+
+
 /**************************************/
 /* Leave this at the end of the file! */
 /**************************************/
```

## 5. Proposed test cases

**New**, in `tests/rtest_integrate.mac`, before the "Leave this at the end of the file!" block:

```
/* integrate() with abs(p), p linear in x: abs(p) is p or -p on either side of the zero of p.
   sqrt(c*x^2) simplifies to sqrt(c)*abs(x). */

[integrate(x^2*abs(x), x), integrate(x/abs(x), x), integrate(1/abs(x), x), integrate(1/abs(5*x+3), x),
 integrate(sqrt(abs(x)), x)];
[x^3*abs(x)/4, abs(x), x*log(abs(x))/abs(x), (5*x+3)*log(abs(5*x+3))/(5*abs(5*x+3)), 2*x*sqrt(abs(x))/3]$

[integrate(sqrt(c*x^2)/(x*(a+b*x)^2), x), integrate(sqrt(c*x^2)/(x*(a+b*x)), x)];
[x*(sqrt(c)/(a*b)-sqrt(c)/(b*(b*x+a)))/abs(x), x*(sqrt(c)*log(b*x+a)/b-log(a)*sqrt(c)/b)/abs(x)]$

integrate(x^n*abs(x), x), ask_hide_prompt; no;
x^(n+3)/((n+2)*abs(x))$

[integrate(1/(1+abs(x)^3), x), integrate(x*sqrt(1+abs(x)), x)];
[x*(atan((2*abs(x)-1)/sqrt(3))/sqrt(3)+log(abs(x)+1)/3-log(-abs(x)+x^2+1)/6+%pi/(2*3^(3/2)))/abs(x),
 2*(abs(x)+1)^(5/2)/5-2*(abs(x)+1)^(3/2)/3]$

/* Continuous at the zero of p */

block([F : integrate(abs(x-1)/(x+1), x)],
  [F, limit(F, x, 1, minus), limit(F, x, 1, plus)]);
[(x-1)*(-2*log(x+1)+x+2*log(2)-1)/abs(x-1), 0, 0]$

/* Antiderivatives on both sides of 0 */

map(lambda([f], block([F : integrate(f, x)],
    map(lambda([s], radcan(subst(abs(x) = s*x, diff(F, x) - f))), [1, -1]))),
  [1/(1+abs(x)^3), x*sqrt(1+abs(x)), (x-sqrt(x^6))/(x*(1-x^4)), abs(x)/sqrt(x^2+x)]);
[[0, 0], [0, 0], [0, 0], [0, 0]]$

/* Definite integrals across the zero of p */

[integrate(x^2*abs(x), x, -1, 1), integrate(abs(x-1)/(x+1), x, 0, 2), integrate(sqrt(abs(x)), x, -1, 4),
 integrate(x/abs(x), x, -1, 2), integrate(abs(x)/(1+x^2), x, -1, 2)];
[1/2, 4*log(2)-2*log(3), 6, 1, log(5)/2+log(2)/2]$

/* Not done: abs of a non-polynomial, two abs, p not linear, p maybe complex, domain : complex */

[integrate(abs(sin(x)), x), integrate(abs(x)*abs(x-1), x), integrate(abs(x^2-1), x),
 integrate(x^2*abs(x+sqrt(a)), x), block([domain : complex], integrate(x^2*abs(x), x))];
['integrate(abs(sin(x)), x), 'integrate(abs(x)*abs(x-1), x), 'integrate(abs(x^2-1), x),
 'integrate(x^2*abs(x+sqrt(a)), x), 'integrate(x^2*abs(x), x)]$
```

**Modified**, in `share/contrib/integration/rtest_abs_integrate.mac`. With `abs_integrate` loaded, `integrate()` now answers these itself, before the package's `tellsimpafter` rule on the noun form gets a chance. Each new result is the same function as the old one (checked on each side of the zero with `abs(p) = ±p`, `signum(p) = ±1`), except problem 144, whose old expectation was the unevaluated integral. Old expectation → new expectation:

```
integrate(x*abs(x),x);
(x^3*signum(x))/3$  →  (x^2*abs(x))/3$

(assume(n > 2), integrate(abs(x) * x^n,x));
(x^(n+2)*signum(x))/(n+2)$  →  x^(n+3)/((n+2)*abs(x))$

integrate(x^2*abs(x),x,-a,a);
(a^4*signum(a))/2$  →  (a^3*abs(a))/2$

(i : integrate(f(x) * abs(x-a),x), ev(i, f(x) := x, 'integrate));
(((2*x^3)/3-a*x^2+a^3/3)*signum(x-a))/2$  →  ((x-a)*((2*x^3-3*a*x^2)/6+a^3/6))/abs(x-a)$

i : integrate(abs(x-1)/ ( 1 + x^2),x);
signum(x-1)*(log(x^2+1)/2-atan(x)-log(2)/2+%pi/4)$  →  ((x-1)*(log(x^2+1)/2-atan(x)-log(2)/2+%pi/4))/abs(x-1)$

integrate(exp(-abs(x)), x);
((-%e^x-%e^(-x)+2)*signum(x))/2+(%e^x-%e^(-x))/2$  →  ((1-%e^-abs(x))*x)/abs(x)$

ratsimp(integrate(cos(x + abs(x)),x));
((signum(x)+1)*sin(2*x)-2*x*signum(x)+2*x)/4$  →  ((abs(x)+x)*sin(2*abs(x))+2*x*abs(x)-2*x^2)/(4*abs(x))$

signum_to_abs(integrate(diff(abs(x) * cos(x),x),x));
cos(x)*abs(x)$  →  abs(x)*cos(abs(x))$

i : integrate(1/(1+abs(x)),x);
signum(x)*(log(x+1)+log(1-x))/2+(log(x+1)-log(1-x))/2$  →  (x*log(abs(x)+1))/abs(x)$

integrate(1/abs(x),x);
'integrate(1/abs(x),x)$  →  (x*log(abs(x)))/abs(x)$

integrate(sqrt(1+abs(x)),x);
(((2*(x+1)^(3/2))/3+(2*(1-x)^(3/2))/3-4/3)*signum(x))/2+((2*(x+1)^(3/2))/3-(2*(1-x)^(3/2))/3)/2$  →  (x*((2*(abs(x)+1)^(3/2))/3-2/3))/abs(x)$

integrate(1/(1+abs(x)),x,-1/2,1/2);
(log(2)+log(3/2))/2-log(2)-((-log(2)-log(3/2))/2)+log(3/2)$  →  2*log(3/2)$

integrate(1/(1+abs(x)),x,-107,107);
(log(108)-log(106)-%i*%pi)/2+log(108)-((-log(108)+log(106)+%i*%pi)/2)+log(106)+%i*%pi$  →  2*log(108)$
```

Paste-ready replacements:

```
integrate(x*abs(x),x);
(x^2*abs(x))/3$

(assume(n > 2), integrate(abs(x) * x^n,x));
x^(n+3)/((n+2)*abs(x))$

integrate(x^2*abs(x),x,-a,a);
(a^3*abs(a))/2$

(i : integrate(f(x) * abs(x-a),x), ev(i, f(x) := x, 'integrate));
((x-a)*((2*x^3-3*a*x^2)/6+a^3/6))/abs(x-a)$

i : integrate(abs(x-1)/ ( 1 + x^2),x);
((x-1)*(log(x^2+1)/2-atan(x)-log(2)/2+%pi/4))/abs(x-1)$

integrate(exp(-abs(x)), x);
((1-%e^-abs(x))*x)/abs(x)$

ratsimp(integrate(cos(x + abs(x)),x));
((abs(x)+x)*sin(2*abs(x))+2*x*abs(x)-2*x^2)/(4*abs(x))$

signum_to_abs(integrate(diff(abs(x) * cos(x),x),x));
abs(x)*cos(abs(x))$

i : integrate(1/(1+abs(x)),x);
(x*log(abs(x)+1))/abs(x)$

integrate(1/abs(x),x);
(x*log(abs(x)))/abs(x)$

integrate(sqrt(1+abs(x)),x);
(x*((2*(abs(x)+1)^(3/2))/3-2/3))/abs(x)$

integrate(1/(1+abs(x)),x,-1/2,1/2);
2*log(3/2)$

integrate(1/(1+abs(x)),x,-107,107);
2*log(108)$
```

**Results.** `rtest_integrate`: 1039/1039 passed, not counting the 2 registered expected errors (master: 1031/1031). `rtest_abs_integrate`: 252/252, not counting 2 expected errors (with the patch but the old expectations: 239/252). Full suite in section 6.

## 6. Results

### Test lists (indefinite integrals, `check.sh` as given)

The task's lists, plus all 596 integrands of the Rubi suite (72,254 integrals in Maxima syntax) that contain `abs` after simplification with `domain : real`. Of those, 420 have an x-dependent polynomial argument, all linear except three with `x^2-1`, and none has two distinct x-dependent `abs` arguments. "ok" and "bad" are `check.sh`'s numerical verdicts. "Questions" gives the number of integrals that ask, and in parentheses the driver's count, in which a yes/no question answered `pos` counts 41. CPU is the sum of `elapsed_run_time()` over the list, in seconds.

**`domain : real`, questions answered `pos`:**

| list | n | build | answers | ok | bad | timeouts | questions | CPU |
|---|---|---|---|---|---|---|---|---|
| targets | 314 | master | 0 | 0 | 0 | 0 | 0 (0) | 3.49 |
| | | new | 268 | 268 | 0 | 0 | 21 (341) | 3.89 |
| controls | 21 | master | 21 | 21 | 0 | 0 | 0 (0) | 0.05 |
| | | new | 21 | 21 | 0 | 0 | 0 (0) | 0.05 |
| negatives | 8 | master | 3 | 3 | 0 | 0 | 0 (0) | 0.07 |
| | | new | 7 | 7 | 0 | 0 | 0 (0) | 0.07 |
| all suite integrands with `abs` | 596 | master | 21 | 17 | 4 | 0 | 3 (7) | 11.70 |
| | | new | 330 | 326 | 4 | 0 | 45 (733) | 13.12 |

- The 4 bad answers are master's own and unchanged (4.7.5 #40, #108, #180, #222, where `abs` has only the arguments `m+1` and `n`).
- No answer of master was lost or changed in any run, and the controls keep their answers verbatim.
- Negatives: `x/abs(x)`, `1/abs(x)`, `sqrt(abs(x))` and `abs(x)^(1/3)` are new, and `abs(sin(x))` stays unevaluated.
- **Questions answered `neg`:** the same numbers in every row (targets: 268 answers, all ok. All 596: 330 answers, 326 ok, the same 4 bad).
- **`domain : complex`:** identical answers and verdicts on both builds in every list (all 596: 488 answers and the same one timeout on both), as intended.
- **New questions:** 42 integrals ask questions that master did not ask. All come from the integrator working on the substituted integrand and all are about the user's symbols: `Is a positive or negative?` (14), `Is n equal to -1?` (6), `Is a*b positive or negative?` (5), `Is m equal to -1?` (2), `Is 2*p+k equal to -1?` for various k (10), and a few about `b`, `1/b`, `a` and `4*a*c-b^2`. 39 lead to answers, 3 end unevaluated (1.1.1.2#972, #978, 1.1.3.2#66) because the integrator's result has the exp-log form the rule rejects.
- **Exact checks:** for 312 of the 313 new answers, `radcan(subst(abs(p) = s*p, diff(F, x) - f))` is 0 for s = 1 and s = -1 (for 4 of them after substituting once more, since `radcan` brings `abs(x)` back from `sqrt(x^2)`). The other one, 1.1.3.2#68 `x^(-1-2*p)*(x^2)^p`, is right numerically. `radcan` treats `(-x)^(2*p)` as `(-1)^(2*p)*x^(2*p)`, which is not branch-correct for x < 0.
- **Continuity at r** (one-sided limits of the new answers): 227 continuous, 76 singular at r (not integrable there), 1 with a jump (1.1.4.2#412: `atan(sqrt(a-b*x^4)/(sqrt(b)*x^2))` has a finite limit at 0, but substitution divides by zero), 9 undecided by `limit`.
- **CPU, interleaved** (rounds of master then new, first round discarded), all 617 integrals: master 12.42 s and 13.98 s, new 13.85 s and 13.61 s. For the 267 that both leave unevaluated: master 8.48 s and 9.28 s, new 9.83 s and 9.05 s. That is within run-to-run noise, and the slowest new answer takes 0.02 s.
- 20 extra integrands with roots vanishing at r (`abs(x)*sqrt(x^3+x^2)`, `abs(x-1)*sqrt(x^2-1)`, `x^n*abs(x)`, ...): master 6 answers, new 19, all ok.

### Definite integrals

458 integrands (the above without the trigonometric and hyperbolic sections, plus 15 examples), with `a=2, b=3, c=5, d=7/2, m=1/2, n=3/2, p=1/3`, on [-1, 2], [-1/2, 1], [-3, -1], [1/2, 2] and [-2, -1/3]: 2290 cases. New values were compared with `quad_qags` of the real and imaginary parts, piecewise between singular points where `quad_qags` complained.

| build | values | nouns | errors | timeouts (30 s) |
|---|---|---|---|---|
| master | 1346 | 866 | 70 | 8 |
| new | 1708 | 498 | 76 | 8 (the same) |

- No value of master was lost or changed, and there are no new timeouts or questions.
- 362 new values: 357 right, 5 wrong. The 5 are `defint` errors that master makes on the corresponding integrands without `abs` as well: #981 to #984 (`(d*x)^m*(c*x^2)^(k/2)*(a+b*x)^n` with half-integer m and n) on [-1, 2] get the sign of the imaginary part wrong, as master does for `integrate(sqrt(x)*x^5*(3*x+2)^(3/2), x, -1, 2)`. 1.1.4.2#419 on [-1/2, 1] is off by `4*%pi/sqrt(3)`, as master is for `integrate(1/(sqrt(2/sqrt(x)-3)*x), x, 1/2, 1)` (`limit` of `atan` at an imaginary argument).
- 6 new errors, all `defint: integral is divergent.`, rightly (poles at an end of the interval).
- The task's cases: `integrate(x^2*abs(x), x, -1, 1)` = `1/2` (master: noun). Unchanged: `integrate(abs(x-1), x, 0, 2)` = `1`, `integrate(x*abs(x^2-1), x, 0, 2)` = `5/2`, `integrate(x*abs(x^2-1), x, -2, 3)` = `55/4`, and with `0 < a < 1`, `integrate(abs(x-a), x, 0, 1)` = `(a^2-2*a+1)/2+a^2/2`. `integrate(abs(x^2-1), x, -2, 2)` stays unevaluated.

### Full test suite and depcheck

`run_testsuite(share_tests = true)`: master 1 failure out of 22,214 tests, new 1 failure out of 22,222. It is `rtestprintf` problem 38 (float printing with SBCL 2.2.9), byte-identical on both. Without the re-baselining, `rtest_abs_integrate` had the 13 failures listed in section 5. `tests/depcheck.sh`: exit 0 on both builds ("no file leaves the global compiler policy changed").

## 7. Tried and rejected, open issues

**Tried and rejected:**

- **A symbol s for the sign** (abs(p) → s*p, then integrate). The integrator asks about s, e.g. for `1/(1+s*x^3)`, and builds the result on that sign. Integrating the concrete ±p versions needs no internal symbol.
- **Using F on both sides whenever FM = -FP.** In `defint` the integrator splits `sqrt(3*x^4+2*x^3)` into roots valid only for x > 0, and `integrate(abs(x)/sqrt(3*x^4+2*x^3), x, -1/2, 1)` came out wrong. The radicand test sends such cases to the reflected form.
- **That form only for rational integrands.** Safe, but `x^n*abs(x)`, `abs(x)/sqrt(x^2+x)` and `sqrt((a+b*x)/x^2)` got results several times longer.
- **Accepting exp-log antiderivatives.** 47 new answers with symbolic exponents (`(a+b*x)^n*sqrt(c*x^2)`, `(d*x)^m*...`) failed the check at some real points, because the integrator writes `(b*x+a)^n` as `%e^(n*log(b*x+a))` or splits `(d*x)^m`. Master's answers for the integrands without `abs` have the same defect, but new wrong answers are not wanted.
- **Trying the rule before the normal integrator.** Answers master already gives, such as `sqrt(c)*(b*x+a)*abs(b*x+a)/(2*b)`, would change form.
- **A clause inside `INTEGRATOR`, at any depth.** `abs(sin(x))` then got a huge answer through the half-angle substitution and a nested `SININT`. The rule runs only for the outermost integrand.
- **F(r) by `limit`.** It would make 1.1.4.2#412 continuous, but `limit` can ask questions and be slow, while substitution never asks.
- **Stopping `defint` from asking for the sign of the variable** by binding `LIMITP` to nil in `REAL-ROOTS`. Without it, `defint` asked `Is x positive or negative?` for 12 divergent integrals such as `integrate(1/(abs(x)*sqrt(x^4-3*x^2+3)), x, -1, 2)` and returned a wrong "Principal Value" in 6 of them. The binding fixed that, but on master it turned 3 right results out of 400 sampled definite integrals without `abs` into errors (`sign: argument cannot be non-real`). Keeping the rule out of `PRINCIPAL-VALUE-INTEGRAL` instead leaves 64 principal-value cases unevaluated as on master, and changes nothing without `abs`.
- **p of higher degree and several `abs`.** Continuity at several zeros needs one constant per zero and exact real roots, and `defint`'s `REAL-ROOTS` drops the real roots of `x^3-3*x+1` (casus irreducibilis), so a jump could go unnoticed. Only one `abs` of a linear p is handled. In the Rubi suite, no integrand has two distinct `abs` arguments, and all polynomial arguments but three are linear.
- **`domain : complex`.** There variables may be complex, `abs(p) = ±p` needs p real, and `$csign` cannot tell (it says `pnz` for undeclared symbols in both domains), so the rule is off. The simplifier creates no `abs` from `sqrt(c*x^2)` there anyway.
- **The `abs_integrate` approach** (`signum`, `unit_step`) was not used. Results contain only `abs`.

**Open issues:**

- 46 of the 314 targets stay unevaluated: 37 with symbolic exponents whose antiderivatives the rule rejects (1.1.1.2#923 to #980, 1.1.3.2#62 to #65), the rest need integrals the integrator cannot do (`x^k*sqrt(a+b*(c*x^2)^(3/2))`, `sqrt(a*x^2)/sqrt(1+x^3)`). On a branch whose integrator returns power forms there, the rule should solve more of them without change.
- 5 new definite values are wrong, from `defint` problems that master shows without `abs` as well (above).
- `defint` asking for the sign of its variable in `REAL-ROOTS` remains a master bug. Only the new rule is kept away from it.
- Three integrals ask a question and then stay unevaluated (above).
- Continuity is not reached when F is finite at r only as a limit (1 of 313).
- `abs(x^2-1)`, `abs(x)*abs(x-1)` and the like are left as before.
- Tested with SBCL 2.2.9 only.

## 8. Proposed Git commit message

```
integrate(): handle abs() of linear polynomials

integrate() could not do simple integrands with abs(p) for p linear in
x, such as x^2*abs(x) or 1/abs(5*x+3). They are common, because the
simplifier turns sqrt(c*x^2) into sqrt(c)*abs(x) and
(c*(a+b*x)^2)^(5/2) into c^(5/2)*(b*x+a)^4*abs(b*x+a).

On either side of the zero of p, abs(p) is p or -p. When nothing else
works, integrate() now integrates both versions and joins them with
abs(p)/p, where possible continuously at the zero, so
integrate(x^2*abs(x), x) gives x^3*abs(x)/4, and definite integrals
across the zero such as integrate(x^2*abs(x), x, -1, 1) work too.
Nothing changes with domain : complex. Principal values do not use
this, since defint may ask for the sign of the variable there.

This solves 268 of 314 such integrals from the Rubi test suite. The
abs_integrate tests now get these results from integrate() itself.

AI-Assisted-By: Claude Opus 5.5
```
