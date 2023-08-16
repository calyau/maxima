@c -*- mode: texinfo -*-
@menu
* Introduction to orthogonal polynomials::
* Functions and Variables for orthogonal polynomials::
@end menu

@node Introduction to orthogonal polynomials, Functions and Variables for orthogonal polynomials, Package orthopoly, Package orthopoly
@section Introduction to orthogonal polynomials

@code{orthopoly} is a package for symbolic and numerical evaluation of
several kinds of orthogonal polynomials, including Chebyshev,
Laguerre, Hermite, Jacobi, Legendre, and ultraspherical (Gegenbauer) 
polynomials. Additionally, @code{orthopoly} includes support for the spherical Bessel, 
spherical Hankel, and spherical harmonic functions.

For the most part, @code{orthopoly} follows the conventions of Abramowitz and Stegun
@i{Handbook of Mathematical Functions}, Chapter 22 (10th printing, December 1972);
additionally, we use Gradshteyn and Ryzhik, 
@i{Table of Integrals, Series, and Products} (1980 corrected and 
enlarged edition), and Eugen Merzbacher @i{Quantum Mechanics} (2nd edition, 1970).

@c INSTALLATION INSTRUCTIONS NO LONGER RELEVANT
@c BUT MAYBE SOME OF THESE FILES SHOULD BE MENTIONED IN ANOTHER CONTEXT
@c This will create a directory @code{orthopoly_x} (again x is the release 
@c identifier) that contains the source file @code{orthopoly.lisp}, user 
@c documentation in html and texi formats, a sample maxima initialization file 
@c @code{orthopoly-init.lisp}, a README file, a testing routine 
@c @code{test_orthopoly.mac}, and two demonstration files.

@c Start Maxima and compile orthopoly. To do this, use the command
@c 
@c (c1) compile_file("orthopoly.lisp");

Barton Willis of the University of Nebraska at Kearney (UNK) wrote
the @code{orthopoly} package and its documentation. The package 
is released under the GNU General Public License (GPL).

@opencatbox{Categories:}
@category{Orthogonal polynomials}
@category{Share packages}
@category{Package orthopoly}
@closecatbox

@subsection Getting Started with orthopoly

@code{load ("orthopoly")} loads the @code{orthopoly} package.

To find the third-order Legendre polynomial,

@c ===beg===
@c legendre_p (3, x);
@c ===end===
@example
(%i1) legendre_p (3, x);
                      3             2
             5 (1 - x)    15 (1 - x)
(%o1)      - ---------- + ----------- - 6 (1 - x) + 1
                 2             2
@end example

To express this as a sum of powers of @var{x}, apply @var{ratsimp} or @var{rat}
to the result.

@c CONTINUING PREVIOUS EXAMPLE HERE
@c ===beg===
@c [ratsimp (%), rat (%)];
@c ===end===
@example
(%i2) [ratsimp (%), rat (%)];
                        3           3
                     5 x  - 3 x  5 x  - 3 x
(%o2)/R/            [----------, ----------]
                         2           2
@end example

Alternatively, make the second argument to @code{legendre_p} (its ``main'' variable) 
a canonical rational expression (CRE).

@c ===beg===
@c legendre_p (3, rat (x));
@c ===end===
@example
(%i1) legendre_p (3, rat (x));
                              3
                           5 x  - 3 x
(%o1)/R/                   ----------
                               2
@end example

For floating point evaluation, @code{orthopoly} uses a running error analysis
to estimate an upper bound for the error. For example,

@c ===beg===
@c jacobi_p (150, 2, 3, 0.2);
@c ===end===
@example
(%i1) jacobi_p (150, 2, 3, 0.2);
(%o1) interval(- 0.062017037936715, 1.533267919277521E-11)
@end example

Intervals have the form @code{interval (@var{c}, @var{r})}, where @var{c} is the
center and @var{r} is the radius of the interval. Since Maxima
does not support arithmetic on intervals, in some situations, such
as graphics, you want to suppress the error and output only the 
center of the interval. To do this, set the option
variable @code{orthopoly_returns_intervals} to @code{false}.

@c ===beg===
@c orthopoly_returns_intervals : false;
@c jacobi_p (150, 2, 3, 0.2);
@c ===end===
@example
(%i1) orthopoly_returns_intervals : false;
(%o1)                         false
(%i2) jacobi_p (150, 2, 3, 0.2);
(%o2)                  - 0.062017037936715
@end example

Refer to the section @pxref{Floating point Evaluation} for more information.

Most functions in @code{orthopoly} have a @code{gradef} property; thus

@c ===beg===
@c diff (hermite (n, x), x);
@c diff (gen_laguerre (n, a, x), x);
@c ===end===
@example
(%i1) diff (hermite (n, x), x);
(%o1)                     2 n H     (x)
                               n - 1
(%i2) diff (gen_laguerre (n, a, x), x);
              (a)               (a)
           n L   (x) - (n + a) L     (x) unit_step(n)
              n                 n - 1
(%o2)      ------------------------------------------
                               x
@end example

The unit step function in the second example prevents an error that would
otherwise arise by evaluating with @var{n} equal to 0.

@c CONTINUING PREVIOUS EXAMPLE HERE
@c ===beg===
@c ev (%, n = 0);
@c ===end===
@example
(%i3) ev (%, n = 0);
(%o3)                           0
@end example

The @code{gradef} property only applies to the ``main'' variable; derivatives with 
respect other arguments usually result in an error message; for example

@c ===beg===
@c diff (hermite (n, x), x);
@c diff (hermite (n, x), n);
@c ===end===
@example
(%i1) diff (hermite (n, x), x);
(%o1)                     2 n H     (x)
                               n - 1
(%i2) diff (hermite (n, x), n);

Maxima doesn't know the derivative of hermite with respect the first
argument
 -- an error.  Quitting.  To debug this try debugmode(true);
@end example

Generally, functions in @code{orthopoly} map over lists and matrices. For
the mapping to fully evaluate, the option variables 
@code{doallmxops} and @code{listarith} must both be @code{true} (the defaults).
To illustrate the mapping over matrices, consider

@c ===beg===
@c hermite (2, x);
@c m : matrix ([0, x], [y, 0]);
@c hermite (2, m);
@c ===end===
@example
(%i1) hermite (2, x);
                                     2
(%o1)                    - 2 (1 - 2 x )
(%i2) m : matrix ([0, x], [y, 0]);
                            [ 0  x ]
(%o2)                       [      ]
                            [ y  0 ]
(%i3) hermite (2, m);
               [                             2  ]
               [      - 2        - 2 (1 - 2 x ) ]
(%o3)          [                                ]
               [             2                  ]
               [ - 2 (1 - 2 y )       - 2       ]
@end example

In the second example, the @code{i, j} element of the value
is @code{hermite (2, m[i,j])}; this is not the same as computing
@code{-2 + 4 m . m}, as seen in the next example.

@c CONTINUING PREVIOUS EXAMPLE HERE
@c ===beg===
@c -2 * matrix ([1, 0], [0, 1]) + 4 * m . m;
@c ===end===
@example
(%i4) -2 * matrix ([1, 0], [0, 1]) + 4 * m . m;
                    [ 4 x y - 2      0     ]
(%o4)               [                      ]
                    [     0      4 x y - 2 ]
@end example

If you evaluate a function at a point outside its domain, generally
@code{orthopoly} returns the function unevaluated. For example,

@c ===beg===
@c legendre_p (2/3, x);
@c ===end===
@example
(%i1) legendre_p (2/3, x);
(%o1)                        P   (x)
                              2/3
@end example

@code{orthopoly} supports translation into TeX; it also does two-dimensional
output on a terminal.

@c ===beg===
@c spherical_harmonic (l, m, theta, phi);
@c tex (%);
@c jacobi_p (n, a, a - b, x/2);
@c tex (%);
@c ===end===
@example
(%i1) spherical_harmonic (l, m, theta, phi);
                          m
(%o1)                    Y (theta, phi)
                          l
(%i2) tex (%);
$$Y_@{l@}^@{m@}\left(\vartheta,\varphi\right)$$
(%o2)                         false
(%i3) jacobi_p (n, a, a - b, x/2);
                          (a, a - b) x
(%o3)                    P          (-)
                          n          2
(%i4) tex (%);
$$P_@{n@}^@{\left(a,a-b\right)@}\left(@{@{x@}\over@{2@}@}\right)$$
(%o4)                         false
@end example

@subsection Limitations

When an expression involves several orthogonal polynomials with
symbolic orders, it's possible that the expression actually
vanishes, yet Maxima is unable to simplify it to zero. If you
divide by such a quantity, you'll be in trouble. For example,
the following expression vanishes for integers @var{n} greater than 1, yet Maxima
is unable to simplify it to zero.

@c ===beg===
@c (2*n - 1) * legendre_p (n - 1, x) * x - n * legendre_p (n, x) 
@c       + (1 - n) * legendre_p (n - 2, x);
@c ===end===
@example
(%i1) (2*n - 1) * legendre_p (n - 1, x) * x - n * legendre_p (n, x)
      + (1 - n) * legendre_p (n - 2, x);
(%o1)  (2 n - 1) P     (x) x - n P (x) + (1 - n) P     (x)
                  n - 1           n               n - 2
@end example

For a specific @var{n}, we can reduce the expression to zero.

@c CONTINUING PREVIOUS EXAMPLE HERE
@c ===beg===
@c ev (% ,n = 10, ratsimp);
@c ===end===
@example
(%i2) ev (% ,n = 10, ratsimp);
(%o2)                           0
@end example

Generally, the polynomial form of an orthogonal polynomial is ill-suited
for floating point evaluation. Here's an example.

@c ACTUALLY NEEDS load("orthopoly"); BEFORE ANYTHING ELSE
@c ===beg===
@c p : jacobi_p (100, 2, 3, x)$
@c subst (0.2, x, p);
@c jacobi_p (100, 2, 3, 0.2);
@c float(jacobi_p (100, 2, 3, 2/10));
@c ===end===
@example 
(%i1) p : jacobi_p (100, 2, 3, x)$

(%i2) subst (0.2, x, p);
(%o2)                3.4442767023833592E+35
(%i3) jacobi_p (100, 2, 3, 0.2);
(%o3)  interval(0.18413609135169, 6.8990300925815987E-12)
(%i4) float(jacobi_p (100, 2, 3, 2/10));
(%o4)                   0.18413609135169
@end example

The true value is about 0.184; this calculation suffers from extreme
subtractive cancellation error. Expanding the polynomial and then
evaluating, gives a better result.
@c CONTINUING PREVIOUS EXAMPLE HERE
@c ===beg===
@c p : expand (p)$
@c subst (0.2, x, p);
@c ===end===
@example
(%i5) p : expand(p)$
(%i6) subst (0.2, x, p);
(%o6) 0.18413609766122982
@end example

This isn't a general rule; expanding the polynomial does not always
result in an expression that is better suited for numerical evaluation.
By far, the best way to do numerical evaluation is to make one or more
of the function arguments floating point numbers. By doing that, 
specialized floating point algorithms are used for evaluation.

Maxima's @code{float} function is somewhat indiscriminate; if you apply 
@code{float} to an expression involving an orthogonal polynomial with a
symbolic degree or order parameter, these parameters may be 
converted into floats; after that, the expression will not evaluate 
fully. Consider

@c ===beg===
@c assoc_legendre_p (n, 1, x);
@c float (%);
@c ev (%, n=2, x=0.9);
@c ===end===
@example
(%i1) assoc_legendre_p (n, 1, x);
                               1
(%o1)                         P (x)
                               n
(%i2) float (%);
                              1.0
(%o2)                        P   (x)
                              n
(%i3) ev (%, n=2, x=0.9);
                             1.0
(%o3)                       P   (0.9)
                             2
@end example

The expression in (%o3) will not evaluate to a float; @code{orthopoly} doesn't
recognize floating point values where it requires an integer. Similarly, 
numerical evaluation of the @code{pochhammer} function for orders that
exceed @code{pochhammer_max_index} can be troublesome; consider

@c ===beg===
@c x :  pochhammer (1, 10), pochhammer_max_index : 5;
@c ===end===
@example
(%i1) x :  pochhammer (1, 10), pochhammer_max_index : 5;
(%o1)                         (1)
                                 10
@end example

Applying @code{float} doesn't evaluate @var{x} to a float

@c CONTINUING PREVIOUS EXAMPLE HERE
@c ===beg===
@c float (x);
@c ===end===
@example
(%i2) float (x);
(%o2)                       (1.0)
                                 10.0
@end example

To evaluate @var{x} to a float, you'll need to bind
@code{pochhammer_max_index} to 11 or greater and apply @code{float} to @var{x}.

@c CONTINUING PREVIOUS EXAMPLE HERE
@c ===beg===
@c float (x), pochhammer_max_index : 11;
@c ===end===
@example
(%i3) float (x), pochhammer_max_index : 11;
(%o3)                       3628800.0
@end example

The default value of @code{pochhammer_max_index} is 100;
change its value after loading @code{orthopoly}.

Finally, be aware that reference books vary on the definitions of the 
orthogonal polynomials; we've generally used the conventions 
of Abramowitz and Stegun.

Before you suspect a bug in orthopoly, check some special cases 
to determine if your definitions match those used by @code{orthopoly}. 
Definitions often differ by a normalization; occasionally, authors
use ``shifted'' versions of the functions that makes the family
orthogonal on an interval other than @math{(-1, 1)}. To define, for example,
a Legendre polynomial that is orthogonal on @math{(0, 1)}, define

@c ===beg===
@c shifted_legendre_p (n, x) := legendre_p (n, 2*x - 1)$
@c shifted_legendre_p (2, rat (x));
@c legendre_p (2, rat (x));
@c ===end===
@example
(%i1) shifted_legendre_p (n, x) := legendre_p (n, 2*x - 1)$

(%i2) shifted_legendre_p (2, rat (x));
                            2
(%o2)/R/                 6 x  - 6 x + 1
(%i3) legendre_p (2, rat (x));
                               2
                            3 x  - 1
(%o3)/R/                    --------
                               2
@end example

@anchor{Floating point Evaluation}
@subsection Floating point Evaluation

Most functions in @code{orthopoly} use a running error analysis to 
estimate the error in floating point evaluation; the 
exceptions are the spherical Bessel functions and the associated Legendre 
polynomials of the second kind. For numerical evaluation, the spherical 
Bessel functions call SLATEC functions. No specialized method is used
for numerical evaluation of the associated Legendre polynomials of the
second kind.

The running error analysis ignores errors that are second or higher order
in the machine epsilon (also known as unit roundoff). It also
ignores a few other errors. It's possible (although unlikely) 
that the actual error exceeds the estimate.

Intervals have the form @code{interval (@var{c}, @var{r})}, where @var{c} is the 
center of the interval and @var{r} is its radius. The 
center of an interval can be a complex number, and the radius is always a positive real number.

Here is an example.

@c ===beg===
@c fpprec : 50$
@c y0 : jacobi_p (100, 2, 3, 0.2);
@c y1 : bfloat (jacobi_p (100, 2, 3, 1/5));
@c ===end===

@example
(%i1) fpprec : 50$

(%i2) y0 : jacobi_p (100, 2, 3, 0.2);
(%o2) interval(0.1841360913516871, 6.8990300925815987E-12)
(%i3) y1 : bfloat (jacobi_p (100, 2, 3, 1/5));
(%o3) 1.8413609135168563091370224958913493690868904463668b-1
@end example

Let's test that the actual error is smaller than the error estimate

@c CONTINUING PREVIOUS EXAMPLE HERE
@c ===beg===
@c is (abs (part (y0, 1) - y1) < part (y0, 2));
@c ===end===
@example
(%i4) is (abs (part (y0, 1) - y1) < part (y0, 2));
(%o4)                         true
@end example

Indeed, for this example the error estimate is an upper bound for the
true error.

Maxima does not support arithmetic on intervals.

@c ===beg===
@c legendre_p (7, 0.1) + legendre_p (8, 0.1);
@c ===end===
@example
(%i1) legendre_p (7, 0.1) + legendre_p (8, 0.1);
(%o1) interval(0.18032072148437508, 3.1477135311021797E-15)
        + interval(- 0.19949294375000004, 3.3769353084291579E-15)
@end example

A user could define arithmetic operators that do interval math. To
define interval addition, we can define

@c ===beg===
@c infix ("@+")$
@c "@+"(x,y) := interval (part (x, 1) + part (y, 1), part (x, 2) 
@c       + part (y, 2))$
@c legendre_p (7, 0.1) @+ legendre_p (8, 0.1);
@c ===end===
@example
(%i1) infix ("@@+")$

(%i2) "@@+"(x,y) := interval (part (x, 1) + part (y, 1), part (x, 2)
      + part (y, 2))$

(%i3) legendre_p (7, 0.1) @@+ legendre_p (8, 0.1);
(%o3) interval(- 0.019172222265624955, 6.5246488395313372E-15)
@end example

The special floating point routines get called when the arguments
are complex.  For example,

@c ===beg===
@c legendre_p (10, 2 + 3.0*%i);
@c ===end===
@example
(%i1) legendre_p (10, 2 + 3.0*%i);
(%o1) interval(- 3.876378825E+7 %i - 6.0787748E+7, 
                                           1.2089173052721777E-6)
@end example

Let's compare this to the true value.

@c ===beg===
@c float (expand (legendre_p (10, 2 + 3*%i)));
@c ===end===
@example
(%i1) float (expand (legendre_p (10, 2 + 3*%i)));
(%o1)          - 3.876378825E+7 %i - 6.0787748E+7
@end example

Additionally, when the arguments are big floats, the special floating point
routines get called; however, the big floats are converted into double floats
and the final result is a double.

@c ===beg===
@c ultraspherical (150, 0.5b0, 0.9b0);
@c ===end===
@example
(%i1) ultraspherical (150, 0.5b0, 0.9b0);
(%o1) interval(- 0.043009481257265, 3.3750051301228864E-14)
@end example

@subsection Graphics and @code{orthopoly}

To plot expressions that involve the orthogonal polynomials, you 
must do two things:
@enumerate
@item 
Set the option variable @code{orthopoly_returns_intervals} to @code{false},
@item
Quote any calls to @code{orthopoly} functions.
@end enumerate
If function calls aren't quoted, Maxima evaluates them to polynomials before 
plotting; consequently, the specialized floating point code doesn't get called.
Here is an example of how to plot an expression that involves
a Legendre polynomial.

@c ===beg===
@c plot2d ('(legendre_p (5, x)), [x, 0, 1]), 
@c                         orthopoly_returns_intervals : false;
@c ===end===
@example
(%i1) plot2d ('(legendre_p (5, x)), [x, 0, 1]),
                        orthopoly_returns_intervals : false;
(%o1)
@end example

@ifnotinfo
@image{figures/orthopoly1,8cm}
@end ifnotinfo

The @i{entire} expression @code{legendre_p (5, x)} is quoted; this is 
different than just quoting the function name using @code{'legendre_p (5, @var{x})}.

@opencatbox{Categories:}
@category{Plotting}
@closecatbox


@subsection Miscellaneous Functions

The @code{orthopoly} package defines the
Pochhammer symbol and a unit step function. @code{orthopoly} uses
the Kronecker delta function and the unit step function in
@code{gradef} statements.

To convert Pochhammer symbols into quotients of gamma functions,
use @code{makegamma}.

@c ===beg===
@c makegamma (pochhammer (x, n));
@c makegamma (pochhammer (1/2, 1/2));
@c ===end===
@example
(%i1) makegamma (pochhammer (x, n));
                          gamma(x + n)
(%o1)                     ------------
                            gamma(x)
(%i2) makegamma (pochhammer (1/2, 1/2));
                                1
(%o2)                       ---------
                            sqrt(%pi)
@end example

Derivatives of the Pochhammer symbol are given in terms of the @code{psi}
function.

@c ===beg===
@c diff (pochhammer (x, n), x);
@c diff (pochhammer (x, n), n);
@c ===end===
@example
(%i1) diff (pochhammer (x, n), x);
(%o1)             (x)  (psi (x + n) - psi (x))
                     n     0             0
(%i2) diff (pochhammer (x, n), n);
(%o2)                   (x)  psi (x + n)
                           n    0
@end example

You need to be careful with the expression in (%o1); the difference of the
@code{psi} functions has polynomials when @code{@var{x} = -1, -2, .., -@var{n}}. These polynomials
cancel with factors in @code{pochhammer (@var{x}, @var{n})} making the derivative a degree
@code{@var{n} - 1} polynomial when @var{n} is a positive integer.

The Pochhammer symbol is defined for negative orders through its
representation as a quotient of gamma functions. Consider

@c ===beg===
@c q : makegamma (pochhammer (x, n));
@c sublis ([x=11/3, n= -6], q);
@c ===end===
@example
(%i1) q : makegamma (pochhammer (x, n));
                          gamma(x + n)
(%o1)                     ------------
                            gamma(x)
(%i2) sublis ([x=11/3, n= -6], q);
                               729
(%o2)                        - ----
                               2240
@end example

Alternatively, we can get this result directly.

@c ===beg===
@c pochhammer (11/3, -6);
@c ===end===
@example
(%i1) pochhammer (11/3, -6);
                               729
(%o1)                        - ----
                               2240
@end example

The unit step function is left-continuous; thus

@c ===beg===
@c [unit_step (-1/10), unit_step (0), unit_step (1/10)];
@c ===end===
@example
(%i1) [unit_step (-1/10), unit_step (0), unit_step (1/10)];
(%o1)                       [0, 0, 1]
@end example

If you need a unit step function that is neither left or right continuous
at zero, define your own using @code{signum}; for example,

@c ===beg===
@c xunit_step (x) := (1 + signum (x))/2$
@c [xunit_step (-1/10), xunit_step (0), xunit_step (1/10)];
@c ===end===
@example
(%i1) xunit_step (x) := (1 + signum (x))/2$

(%i2) [xunit_step (-1/10), xunit_step (0), xunit_step (1/10)];
                                1
(%o2)                       [0, -, 1]
                                2
@end example

Do not redefine @code{unit_step} itself; some code in @code{orthopoly}
requires that the unit step function be left-continuous.

@subsection Algorithms

Generally, @code{orthopoly} does symbolic evaluation by using a hypergeometic 
representation of the orthogonal polynomials. The hypergeometic 
functions are evaluated using the (undocumented) functions @code{hypergeo11} 
and @code{hypergeo21}. The exceptions are the half-integer Bessel functions 
and the associated Legendre function of the second kind. The half-integer Bessel functions are
evaluated using an explicit representation, and the associated Legendre 
function of the second kind is evaluated using recursion.

For floating point evaluation, we again convert most functions into
a hypergeometic form; we evaluate the hypergeometic functions using 
forward recursion. Again, the exceptions are the half-integer Bessel functions 
and the associated Legendre function of the second kind. Numerically, 
the half-integer Bessel functions are evaluated using the SLATEC code.


@node Functions and Variables for orthogonal polynomials,  , Introduction to orthogonal polynomials, Package orthopoly
@section Functions and Variables for orthogonal polynomials

@anchor{assoc_legendre_p}
@deffn {Function} assoc_legendre_p (@var{n}, @var{m}, @var{x})
The associated Legendre function of the first kind of degree @math{n} and
order @math{m}, 
m4_mathcomma(<<<P_{n}^{m}(z)>>>,<<<assoc_legendre_p(n,m,z)>>>)
is a solution of the differential equation:

m4_displaymath(
<<<(1-z^2){d^2 w\over dz^2} - 2z{dw\over dz} + \left[n(n+1)-{m^2\over 1-z^2}\right] w = 0>>>,
<<<(1-z^2)*diff(w,z,2) - 2*z*diff(w,z) + (n*(n+1)-m^2/(1-z^2))*w = 0>>>)

This is related to the Legendre polynomial, 
m4_math(<<<P_n(x)>>>, <<<legendre_p(n,x)>>>)
via

m4_displaymath(
<<<P_n^m(x) = (-1)^m\left(1-x^2\right)^{m/2} {d^m\over dx^m} P_n(x)>>>,
<<<assoc_legendre_p(n,m,x) = (-1)^m*(1-x^2)^(m/2) diff(legendre_p(n,x),x,m)>>>)

Reference: @urlaands{eqn 22.5.37, 779}, @urlaands{eqn 8.6.6, 334}, and @urlaands{eqn 8.2.5, 333}.

Some examples:
@c ===beg===
@c assoc_legendre_p(2,0,x);
@c factor(%);
@c factor(assoc_legendre_p(2,1,x));
@c (-1)^1*(1-x^2)^(1/2)*diff(legendre_p(2,x),1);
@c factor(%);
@c ===end===
@example
(%i1) assoc_legendre_p(2,0,x);
                                                 2
                                        3 (1 - x)
(%o1)                   (- 3 (1 - x)) + ---------- + 1
                                            2
(%i2) factor(%);
                                      2
                                   3 x  - 1
(%o2)                              --------
                                      2
(%i3) factor(assoc_legendre_p(2,1,x));
                                              2
(%o3)                         - 3 x sqrt(1 - x )

(%i4) (-1)^1*(1-x^2)^(1/2)*diff(legendre_p(2,x),x);
                                                    2
(%o4)                   - (3 - 3 (1 - x)) sqrt(1 - x )

(%i5) factor(%);
                                              2
(%o5)                         - 3 x sqrt(1 - x )
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{assoc_legendre_q}
@deffn {Function} assoc_legendre_q (@var{n}, @var{m}, @var{x})
The associated Legendre function of the second kind of degree @math{n}
and order @math{m}, 
m4_mathcomma(<<<Q_{n}^{m}(z)>>>,<<<assoc_legendre_q(n,m,z)>>>) 
is a solution of the differential equation:

m4_displaymath(
<<<(1-z^2){d^2 w\over dz^2} - 2z{dw\over dz} + \left[n(n+1)-{m^2\over 1-z^2}\right] w = 0>>>,
<<<(1-z^2)*diff(w,z,2) - 2*z*diff(w,z) + (n*(n+1)-m^2/(1-z^2))*w = 0>>>)

Reference: Abramowitz and Stegun, equation 8.5.3 and 8.1.8.

Some examples:
@c ===beg===
@c assoc_legendre_q(0,0,x);
@c assoc_legendre_q(1,0,x);
@c assoc_legendre_q(1,1,x);
@c ===end===
@example
(%i1) assoc_legendre_q(0,0,x);
                                       x + 1
                                 log(- -----)
                                       x - 1
(%o1)                            ------------
                                      2
(%i2) assoc_legendre_q(1,0,x);
                                    x + 1
                              log(- -----) x - 2
                                    x - 1
(%o2)/R/                      ------------------
                                      2
(%i3) assoc_legendre_q(1,1,x);
(%o3)/R/ 
          x + 1            2   2               2            x + 1            2
    log(- -----) sqrt(1 - x ) x  - 2 sqrt(1 - x ) x - log(- -----) sqrt(1 - x )
          x - 1                                             x - 1
  - ---------------------------------------------------------------------------
                                        2
                                     2 x  - 2
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{chebyshev_t}
@deffn {Function} chebyshev_t (@var{n}, @var{x})
The Chebyshev polynomial of the first kind of degree @math{n}, 
m4_math(<<<T_n(x).>>>,<<<chebyshev_t(n,x).>>>)

Reference: @urlaands{eqn 22.5.47, 779}.

The polynomials 
m4_math(<<<T_n(x)>>>,<<<chebyshev_t(n,x)>>>) 
can be written in terms of a hypergeometric function:

m4_displaymath(
<<<T_n(x) = {_{2}}F_{1}\left(-n, n; {1\over 2}; {1-x\over 2}\right)>>>,
<<<hypergeometric([-n,n],[1/2],(1-x)/2)>>>)

The polynomials can also be defined in terms of the sum

m4_displaymath(
<<<T_n(x) = {n\over 2} \sum_{r=0}^{\lfloor {n/2}\rfloor} {(-1)^r\over n-r} {n-r\choose k}(2x)^{n-2r}>>>,
<<<chebyshev_t(n,x) = n/2*sum((-1)^r/(n-r)*binomial(n-r,r)*(2*x)^(n-2*r), r, 0, floor(n/2))>>>)

or the Rodrigues formula

m4_displaymath(
<<<T_n(x) = {1\over \kappa_n  w(x)} {d^n\over dx^n}\left(w(x)(1-x^2)^n\right)>>>,
<<<@math{chebyshev_t(n,x) = 1/(k(n)*w(x))*diff(w(x)*(1-x^2)^n, x, n)}>>>
)

where

m4_displaymath(
<<<\eqalign{
w(x) &= 1/\sqrt{1-x^2} \cr
\kappa_n &= (-2)^n\left(1\over 2\right)_n
}>>>,
<<<@math{w(x) = 1/sqrt(1-x^2)}

@math{k_n = (-2)^n*pochhammer(1/2,n)}>>>)

Some examples:
@c ===beg===
@c chebyshev_t(2,x);
@c factor(%);
@c factor(chebyshev_t(3,x));
@c factor(hgfred([-3,3],[1/2],(1-x)/2));
@c ===end===
@example
(%i1) chebyshev_t(2,x);
                                                 2
(%o1)                   (- 4 (1 - x)) + 2 (1 - x)  + 1
(%i2) factor(%);
                                      2
(%o2)                              2 x  - 1
(%i3) factor(chebyshev_t(3,x));
                                       2
(%o3)                            x (4 x  - 3)
(%i4) factor(hgfred([-3,3],[1/2],(1-x)/2));
                                       2
(%o4)                            x (4 x  - 3)
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{chebyshev_u}
@deffn {Function} chebyshev_u (@var{n}, @var{x})
The Chebyshev polynomial of the second kind of degree @math{n}, 
m4_mathdot(<<<U_n(x)>>>,<<<chebyshev_u(n,x)>>>)

Reference: @urlaands{eqn 22.5.48,779}.

The polynomials 
m4_math(<<<U_n(x)>>>,<<<chebyshev_u(n,x)>>>) 
can be written in terms of a hypergeometric function:

m4_displaymath(
<<<U_n(x) = (n+1)\; {_{2}F_{1}}\left(-n, n+2; {3\over 2}; {1-x\over 2}\right)>>>,
<<<chebyshev_u(n,x) = (n+1)*hypergeometric([-n,n+1],[3/2],(1-x)/2)>>>)

The polynomials can also be defined in terms of the sum

m4_displaymath(
<<<U_n(x) = \sum_{r=0}^{\lfloor n/2 \rfloor} (-1)^r {n-r \choose r} (2x)^{n-2r}>>>,
<<<chebyshev_u(n,x) = sum((-1)^r*binomial(n-r,r)*(2*x)^(n-2*r), r, 0, floor(n/2))>>>)

or the Rodrigues formula

m4_displaymath(
<<<U_n(x) = {1\over \kappa_n  w(x)} {d^n\over dx^n}\left(w(x)(1-x^2)^n\right)>>>,
<<<@math{chebyshev_u(n,x) = 1/(k(n)*w(x))*diff(w(x)*(1-x^2)^n, x, n)}>>>
)

where

m4_displaymath(
<<<\eqalign{
w(x) &= \sqrt{1-x^2} \cr
\kappa_n &= {(-2)^n\left({3\over 2}\right)_n \over n+1}
}>>>,
<<<@math{w(x) = sqrt(1-x^2)}

@math{k(n) = (-2)^n*pochhammer(3/2,n)/(n+1)}>>>).

@c ===beg===
@c chebyshev_u(2,x);
@c expand(%);
@c expand(chebyshev_u(3,x));
@c expand(4*hgfred([-3,5],[3/2],(1-x)/2));
@c ===end===
@example
(%i1) chebyshev_u(2,x);
                                                  2
                            8 (1 - x)    4 (1 - x)
(%o1)                 3 ((- ---------) + ---------- + 1)
                                3            3
(%i2) expand(%);
                                      2
(%o2)                              4 x  - 1
(%i3) expand(chebyshev_u(3,x));
                                     3
(%o3)                             8 x  - 4 x
(%i4) expand(4*hgfred([-3,5],[3/2],(1-x)/2));
                                     3
(%o4)                             8 x  - 4 x
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{gen_laguerre}
@deffn {Function} gen_laguerre (@var{n}, @var{a}, @var{x})
The generalized Laguerre polynomial of degree @math{n}, 
m4_mathdot(<<<L_n^{(\alpha)}(x)>>>,<<<gen_laguerre(n,a,x)>>>)

These can be defined by

m4_displaymath(
<<<L_n^{(\alpha)}(x) = {n+\alpha \choose n}\; {_1F_1}(-n; \alpha+1; x)>>>,
<<<gen_laguerre(n, a, x) = binomial(n+a,n)*hypergeometric([-n], [a+1], x)>>>)

The polynomials can also be defined by the sum

m4_displaymath(
<<<L_n^{(\alpha)}(x) = \sum_{k=0}^n {(\alpha + k + 1)_{n-k} \over (n-k)! k!} (-x)^k>>>,
<<<@math{gen_laguerre(n, a, x) = sum(pochhammer(a+k+1,n-k)/((n-k)!*k!)*(-x)^k, k, 0, n)}>>>)

or the Rodrigues formula

m4_displaymath(
<<<L_n^{(\alpha)}(x) = {1\over \kappa_n  w(x)} {d^n\over dx^n}\left(w(x)x^n\right)>>>,
<<<@math{gen_laguerre(n, a, x) = 1/(k(n)*w(x))*diff(w(x)*x^n, x, n)}>>>
)

where

m4_displaymath(
<<<\eqalign{
w(x) &= e^{-x}x^{\alpha} \cr
\kappa_n &= n!
}>>>,
<<<@math{w(x) = %e^(-x)*x^a}

@math{k(n) = n!}>>>)

Reference: @urlaands{eqn 22.5.54,780}.

Some examples:
@c ===beg===
@c gen_laguerre(1,k,x);
@c gen_laguerre(2,k,x);
@c binomial(2+k,2)*hgfred([-2],[1+k],x);
@c ===end===
@example
(%i1) gen_laguerre(1,k,x);
                                             x
(%o1)                         (k + 1) (1 - -----)
                                           k + 1
(%i2) gen_laguerre(2,k,x);
                                         2
                                        x            2 x
                 (k + 1) (k + 2) (--------------- - ----- + 1)
                                  (k + 1) (k + 2)   k + 1
(%o2)            ---------------------------------------------
                                       2
(%i3) binomial(2+k,2)*hgfred([-2],[1+k],x);
                                         2
                                        x            2 x
                 (k + 1) (k + 2) (--------------- - ----- + 1)
                                  (k + 1) (k + 2)   k + 1
(%o3)            ---------------------------------------------
                                       2

@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{hermite}
@deffn {Function} hermite (@var{n}, @var{x})
The Hermite polynomial of degree @math{n}, 
m4_mathdot(<<<H_n(x)>>>,<<<hermite(n,x)>>>)

These polynomials may be defined by a hypergeometric function

m4_displaymath(
<<<H_n(x) = (2x)^n\; {_2F_0}\left(-{1\over 2} n, -{1\over 2}n+{1\over 2};;-{1\over x^2}\right)>>>,
<<<hermite(n,x) = (2*x)^n * hypergeometric([-n/2, -n/2+1/2],[], -1/x^2)>>>)

or by the series

m4_displaymath(
<<<H_n(x) = n! \sum_{k=0}^{\lfloor n/2 \rfloor} {(-1)^k(2x)^{n-2k} \over k! (n-2k)!}>>>,
<<<hermite(n,x) = n!*sum((-1)^k*(2*x)^(n-2*k)/(k!*(n-2*k)!), k, 0, floor(n/2))>>>)

or the Rodrigues formula

m4_displaymath(
<<<H_n(x) = {1\over \kappa_n  w(x)} {d^n\over dx^n}\left(w(x)\right)>>>,
<<<@math{hermite(n,x) = 1/(k(n)*w(x))*diff(w(x), x, n)}>>>
)

where

m4_displaymath(
<<<\eqalign{
w(x) &= e^{-{x^2/2}} \cr
\kappa_n &= (-1)^n
}>>>,
<<<@math{w(x) = %e(-x^2/2)}

@math{k(n) = (-1)^n}>>>)

Reference: @urlaands{eqn 22.5.55,780}.

Some examples:
@c ===beg===
@c hermite(3,x);
@c expand(%);
@c expand(hermite(4,x));
@c expand((2*x)^4*hgfred([-2,-2+1/2],[],-1/x^2));
@c expand(4!*sum((-1)^k*(2*x)^(4-2*k)/(k!*(4-2*k)!),k,0,floor(4/2)));
@c ===end===
@example
(%i1) hermite(3,x);
                                              2
                                           2 x
(%o1)                          - 12 x (1 - ----)
                                            3
(%i2) expand(%);
                                     3
(%o2)                             8 x  - 12 x
(%i3) expand(hermite(4,x));
                                  4       2
(%o3)                         16 x  - 48 x  + 12
(%i4) expand((2*x)^4*hgfred([-2,-2+1/2],[],-1/x^2));
                                  4       2
(%o4)                         16 x  - 48 x  + 12
(%i5) expand(4!*sum((-1)^k*(2*x)^(4-2*k)/(k!*(4-2*k)!),k,0,floor(4/2)));
                                  4       2
(%o5)                         16 x  - 48 x  + 12
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{intervalp}
@deffn {Function} intervalp (@var{e})
Return @code{true} if the input is an interval and return false if it isn't. 

@opencatbox{Categories:}
@category{Package orthopoly}
@category{Predicate functions}
@closecatbox

@end deffn

@anchor{jacobi_p}
@deffn {Function} jacobi_p (@var{n}, @var{a}, @var{b}, @var{x})
The Jacobi polynomial, 
m4_mathdot(<<<P_n^{(a,b)}(x)>>>, <<<jacobi_p(n,a,b,x)>>>)

The Jacobi polynomials are actually defined for all
@math{a} and @math{b}; however, the Jacobi polynomial
weight @math{(1 - x)^a (1 + x)^b} isn't integrable
for 
m4_math(<<<a \le -1>>>, <<<a <= -1>>>) 
or 
m4_mathdot(<<<b \le -1>>>, <<<b <= -1>>>) 

Reference: @urlaands{eqn 22.5.42,779}.

The polynomial may be defined in terms of hypergeometric functions:

m4_displaymath(
<<<P_n^{(a,b)}(x) = {n+a\choose n} {_1F_2}\left(-n, n + a + b + 1; a+1; {1-x\over 2}\right)>>>,
<<<jacobi_p(n,a,b,x) = binomial(n+a,n)*hypergeometric([-n,n+a+b+1],[a+1],(1-x)/2)>>>)

or the Rodrigues formula

m4_displaymath(
<<<P_n^{(a, b)}(x) = {1\over \kappa_n  w(x)} {d^n\over dx^n}\left(w(x)\left(1-x^2\right)^n\right)>>>,
<<<@math{jacobi_p(n,a,b,x) = 1/(k(n)*w(x))*diff(w(x)*(1-x^2)^n, x, n)}>>>
)

where

m4_displaymath(
<<<\eqalign{
w(x) &= (1-x)^a(1-x)^b \cr
\kappa_n &= (-2)^n n!
}>>>,
<<<@math{w(x) = (1-x)^a*(1-x)^b}

@math{k(n) = (-2)^n*n!}>>>)

Some examples:
@c ===beg===
@c jacobi_p(0,a,b,x);
@c jacobi_p(1,a,b,x);
@c ===end===
@example
(%i1) jacobi_p(0,a,b,x);
(%o1)                                  1
(%i2) jacobi_p(1,a,b,x);
                                    (b + a + 2) (1 - x)
(%o2)                  (a + 1) (1 - -------------------)
                                         2 (a + 1)
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{laguerre}
@deffn {Function} laguerre (@var{n}, @var{x})
The Laguerre polynomial, 
m4_math(<<<L_n(x)>>>,<<<laguerre(n,x)>>>) 
of degree @math{n}.

Reference: @urlaands{eqn 22.5.16, 778} and @urlaands{eqn 22.5.54, 780}.

These are related to the generalized Laguerre polynomial by

m4_displaymath(
<<<L_n(x) = L_n^{(0)}(x)>>>,
<<<laguerre(n,x) = gen_laguerre(n,0,x)>>>)

The polynomials are given by the sum

m4_displaymath(
<<<L_n(x) = \sum_{k=0}^{n} {(-1)^k\over k!}{n \choose k} x^k>>>,
<<<laguerre(n,x) = sum((-1)^k/k!*binomial(n,k)*x^k,k,0,n)>>>)

Some examples:
@c ===beg===
@c laguerre(1,x);
@c laguerre(2,x);
@c gen_laguerre(2,0,x);
@c sum((-1)^k/k!*binomial(2,k)*x^k,k,0,2);
@c ===end===
@example
(%i1) laguerre(1,x);
(%o1)                                1 - x
(%i2) laguerre(2,x);
                                  2
                                 x
(%o2)                            -- - 2 x + 1
                                 2
(%i3) gen_laguerre(2,0,x);
                                  2
                                 x
(%o3)                            -- - 2 x + 1
                                 2
(%i4) sum((-1)^k/k!*binomial(2,k)*x^k,k,0,2);
                                  2
                                 x
(%o4)                            -- - 2 x + 1
                                 2
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{legendre_p}
@deffn {Function} legendre_p (@var{n}, @var{x})
The Legendre polynomial of the first kind, 
m4_mathcomma(P_n(x),legendre(n,x)) 
of degree @math{n}.

Reference: @urlaands{eqn 22.5.50, 779} and @urlaands{eqn 22.5.51, 779}.

The Legendre polynomial is related to the Jacobi polynomials by

m4_displaymath(
<<<P_n(x) = P_n^{(0,0)}(x)>>>,
<<<legendre_p(n,x) = jacobi_p(n,0,0,x)>>>)

or the Rodrigues formula

m4_displaymath(
<<<P_n(x) = {1\over \kappa_n  w(x)} {d^n\over dx^n}\left(w(x)\left(1-x^2\right)^n\right)>>>,
<<<@math{legendre_p(n,x) = 1/(k(n)*w(x))*diff(w(x)*(1-x^2)^n, x, n)}>>>
)

where

m4_displaymath(
<<<\eqalign{
w(x) &= 1 \cr
\kappa_n &= (-2)^n n!
}>>>,
<<<@math{w(x) = 1}

@math{k(n) = (-2)^n*n!}>>>)

Some examples:
@c ===beg===
@c legendre_p(1,x);
@c legendre_p(2,x);
@c expand(%);
@c expand(legendre_p(3,x));
@c expand(jacobi_p(3,0,0,x));
@c ===end===
@example
(%i1) legendre_p(1,x);
(%o1)                                  x
(%i2) legendre_p(2,x);
                                                 2
                                        3 (1 - x)
(%o2)                   (- 3 (1 - x)) + ---------- + 1
                                            2
(%i3) expand(%);
                                      2
                                   3 x    1
(%o3)                              ---- - -
                                    2     2
(%i4) expand(legendre_p(3,x));
                                     3
                                  5 x    3 x
(%o4)                             ---- - ---
                                   2      2
(%i5) expand(jacobi_p(3,0,0,x));
                                     3
                                  5 x    3 x
(%o5)                             ---- - ---
                                   2      2
@end example
@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{legendre_q}
@deffn {Function} legendre_q (@var{n}, @var{x})
The Legendre function of the second kind, 
m4_math(<<<Q_n(x)>>>, <<<legendre_q(n,x)>>>) 
of degree @math{n}.

Reference: Abramowitz and Stegun, equations 8.5.3 and 8.1.8.

These are related to 
m4_math(<<<Q_n^m(x)>>>,<<<assoc_legendre_q(n,m,x)>>>) 
by

m4_displaymath(
<<<Q_n(x) = Q_n^0(x)>>>,
<<<legendre_q(n,x) = assoc_legendre_q(n,0,x)>>>)

Some examples:
@c ===beg===
@c legendre_q(0,x);
@c legendre_q(1,x);
@c assoc_legendre_q(1,0,x);
@c ===end===
@example
(%i1) legendre_q(0,x);
                                       x + 1
                                 log(- -----)
                                       x - 1
(%o1)                            ------------
                                      2
(%i2) legendre_q(1,x);
                                    x + 1
                              log(- -----) x - 2
                                    x - 1
(%o2)/R/                      ------------------
                                      2
(%i3) assoc_legendre_q(1,0,x);
                                    x + 1
                              log(- -----) x - 2
                                    x - 1
(%o3)/R/                      ------------------
                                      2
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{orthopoly_recur}
@deffn {Function} orthopoly_recur (@var{f}, @var{args})
Returns a recursion relation for the orthogonal function family
@var{f} with arguments @var{args}. The recursion is with 
respect to the polynomial degree.

@c ===beg===
@c orthopoly_recur (legendre_p, [n, x]);
@c ===end===
@example
(%i1) orthopoly_recur (legendre_p, [n, x]);
                    (2 n + 1) P (x) x - n P     (x)
                               n           n - 1
(%o1)   P     (x) = -------------------------------
         n + 1                   n + 1
@end example

The second argument to @code{orthopoly_recur} must be a list with the 
correct number of arguments for the function @var{f}; if it isn't, 
Maxima signals an error.

@c ===beg===
@c orthopoly_recur (jacobi_p, [n, x]);
@c ===end===
@example
(%i1) orthopoly_recur (jacobi_p, [n, x]);

Function jacobi_p needs 4 arguments, instead it received 2
 -- an error.  Quitting.  To debug this try debugmode(true);
@end example

Additionally, when @var{f} isn't the name of one of the 
families of orthogonal polynomials, an error is signalled.

@c ===beg===
@c orthopoly_recur (foo, [n, x]);
@c ===end===
@example
(%i1) orthopoly_recur (foo, [n, x]);

A recursion relation for foo isn't known to Maxima
 -- an error.  Quitting.  To debug this try debugmode(true);
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@defvr {Variable} orthopoly_returns_intervals
Default value: @code{true}

When @code{orthopoly_returns_intervals} is @code{true}, floating point results are returned in
the form @code{interval (@var{c}, @var{r})}, where @var{c} is the center of an interval
and @var{r} is its radius. The center can be a complex number; in that
case, the interval is a disk in the complex plane.

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end defvr

@anchor{orthopoly_weight}
@deffn {Function} orthopoly_weight (@var{f}, @var{args})

Returns a three element list; the first element is 
the formula of the weight for the orthogonal polynomial family
@var{f} with arguments given by the list @var{args}; the 
second and third elements give the lower and upper endpoints
of the interval of orthogonality. For example,

@c ===beg===
@c w : orthopoly_weight (hermite, [n, x]);
@c integrate (w[1] * hermite (3, x) * hermite (2, x), x, w[2], w[3]);
@c ===end===
@example
(%i1) w : orthopoly_weight (hermite, [n, x]);
                            2
                         - x
(%o1)                 [%e    , - inf, inf]
(%i2) integrate(w[1]*hermite(3, x)*hermite(2, x), x, w[2], w[3]);
(%o2)                           0
@end example

The main variable of @var{f} must be a symbol; if it isn't, Maxima
signals an error. 

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{pochhammer}
@deffn {Function} pochhammer (@var{x}, @var{n})
The Pochhammer symbol, 
m4_mathdot(<<<(x)_n>>>, <<<pochhammer(x,n)>>>)
(See @urlaands{eqn 6.1.22, 256} and @urldlmf{5.2.iii}).

For nonnegative
integers @var{n} with @code{@var{n} <= pochhammer_max_index}, the
expression 
m4_math(<<<(x)_n>>>, <<<pochhammer(x, n)>>>) 
evaluates to the
product 
m4_math(<<<x(x+1)(x+2)\cdots(x+n-1)>>>, <<<x (x + 1) (x + 2) ... (x +
n - 1)>>>) 
when
m4_math(<<<n > 0>>>, n > 0) 
and
to 1 when @math{n = 0}.
For negative @math{n}, 
m4_math(<<<(x)_n>>>, <<<pochhammer (x, n)>>>) 
is
defined as 
m4_math(<<<(-1)^n/(1-x)_{-n}.>>>, <<<(-1)^n / pochhammer (1 - x, -n).>>>)
Thus

@c ===beg===
@c pochhammer (x, 3);
@c pochhammer (x, -3);
@c ===end===
@example
(%i1) pochhammer (x, 3);
(%o1)                   x (x + 1) (x + 2)
(%i2) pochhammer (x, -3);
                                 1
(%o2)               - -----------------------
                      (1 - x) (2 - x) (3 - x)
@end example

To convert a Pochhammer symbol into a quotient of gamma functions,
(see @urlaands{eqn 6.1.22, 256}) use @code{makegamma}; for example 

@c ===beg===
@c makegamma (pochhammer (x, n));
@c ===end===
@example
(%i1) makegamma (pochhammer (x, n));
                          gamma(x + n)
(%o1)                     ------------
                            gamma(x)
@end example

When @var{n} exceeds @code{pochhammer_max_index} or when @var{n} 
is symbolic, @code{pochhammer} returns a noun form.

@c ===beg===
@c pochhammer (x, n);
@c ===end===
@example
(%i1) pochhammer (x, n);
(%o1)                         (x)
                                 n
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@category{Gamma and factorial functions}
@closecatbox

@end deffn

@defvr {Variable} pochhammer_max_index
Default value: 100

@code{pochhammer (@var{n}, @var{x})} expands to a product if and only if
@code{@var{n} <= pochhammer_max_index}.

Examples:

@c ===beg===
@c pochhammer (x, 3), pochhammer_max_index : 3;
@c pochhammer (x, 4), pochhammer_max_index : 3;
@c ===end===
@example
(%i1) pochhammer (x, 3), pochhammer_max_index : 3;
(%o1)                   x (x + 1) (x + 2)
(%i2) pochhammer (x, 4), pochhammer_max_index : 3;
(%o2)                         (x)
                                 4
@end example

Reference: @urlaands{eqn 6.1.16,256}.

@opencatbox{Categories:}
@category{Package orthopoly}
@category{Gamma and factorial functions}
@closecatbox

@end defvr

@anchor{spherical_bessel_j}
@deffn {Function} spherical_bessel_j (@var{n}, @var{x})
The spherical Bessel function of the first kind, 
m4_math(<<<j_n(x).>>>, <<<spherical_bessel_j(n,x).>>>)

Reference: @urlaands{eqn 10.1.8, 437} and @urlaands{eqn 10.1.15, 439}.

It is related to the Bessel function by

m4_displaymath(
<<<j_n(x) = \sqrt{\pi\over 2x} J_{n+1/2}(x)>>>,
<<<spherical_bessel_j(n,x) = sqrt(%pi/(2*x))*bessel_j(n+1/2,x)>>>)

Some examples:
@c ===beg===
@c spherical_bessel_j(1,x);
@c spherical_bessel_j(2,x);
@c expand(%);
@c expand(sqrt(%pi/(2*x))*bessel_j(2+1/2,x)),besselexpand:true;
@c ===end===
@example
(%i1) spherical_bessel_j(1,x);
                                sin(x)
                                ------ - cos(x)
                                  x
(%o1)                           ---------------
                                       x
(%i2) spherical_bessel_j(2,x);
                                3             3 cos(x)
                        (- (1 - --) sin(x)) - --------
                                 2               x
                                x
(%o2)                   ------------------------------
                                      x
(%i3) expand(%);
                          sin(x)    3 sin(x)   3 cos(x)
(%o3)                  (- ------) + -------- - --------
                            x           3          2
                                       x          x
(%i4) expand(sqrt(%pi/(2*x))*bessel_j(2+1/2,x)),besselexpand:true;
                          sin(x)    3 sin(x)   3 cos(x)
(%o4)                  (- ------) + -------- - --------
                            x           3          2
                                       x          x
@end example
@opencatbox{Categories:}
@category{Package orthopoly}
@category{Bessel functions}
@closecatbox

@end deffn

@anchor{spherical_bessel_y}
@deffn {Function} spherical_bessel_y (@var{n}, @var{x})
The spherical Bessel function of the second kind, 
m4_math(<<<y_n(x).>>>, <<<<spherical_bessel_y(n,x).>>>)

Reference: @urlaands{eqn 10.1.9, 437} and @urlaands{eqn 10.1.15, 439}.

It is related to the Bessel function by

m4_displaymath(
<<<y_n(x) = \sqrt{\pi\over 2x} Y_{n+1/2}(x)>>>,
<<<spherical_bessel_y(n,x) = sqrt(%pi/(2*x))*bessel_y(n+1/2,x)>>>)

@c ===beg===
@c spherical_bessel_y(1,x);
@c spherical_bessel_y(2,x);
@c expand(%);
@c expand(sqrt(%pi/(2*x))*bessel_y(2+1/2,x)),besselexpand:true;
@c ===end===
@example
(%i1) spherical_bessel_y(1,x);
                                           cos(x)
                              (- sin(x)) - ------
                                             x
(%o1)                         -------------------
                                       x
(%i2) spherical_bessel_y(2,x);
                           3 sin(x)        3
                           -------- - (1 - --) cos(x)
                              x             2
                                           x
(%o2)                    - --------------------------
                                       x
(%i3) expand(%);
                          3 sin(x)    cos(x)   3 cos(x)
(%o3)                  (- --------) + ------ - --------
                              2         x          3
                             x                    x
(%i4) expand(sqrt(%pi/(2*x))*bessel_y(2+1/2,x)),besselexpand:true;
                          3 sin(x)    cos(x)   3 cos(x)
(%o4)                  (- --------) + ------ - --------
                              2         x          3
                             x                    x
@end example
@opencatbox{Categories:}
@category{Package orthopoly}
@category{Bessel functions}
@closecatbox

@end deffn

@anchor{spherical_hankel1}
@deffn {Function} spherical_hankel1 (@var{n}, @var{x})
The spherical Hankel function of the
first kind, 
m4_math(<<<h_n^{(1)}(x).>>>, <<<spherical_hankel1(n,x).>>>)

Reference: @urlaands{eqn 10.1.36,439}.

This is defined by

m4_displaymath(
<<<h_n^{(1)}(x) = j_n(x) + iy_n(x)>>>,
<<<spherical_hankel1(n,x) = spherical_bessel_j(n,x) + %i*spherical_bessel_y(n,x)>>>)

@opencatbox{Categories:}
@category{Package orthopoly}
@category{Bessel functions}
@closecatbox

@end deffn

@anchor{spherical_hankel2}
@deffn {Function} spherical_hankel2 (@var{n}, @var{x})
The spherical Hankel function of the
second kind, 
m4_math(<<<h_n^{(2)}(x).>>>, <<<spherical_hankel2(n,x).>>>)

Reference: @urlaands{eqn 10.1.17,439}.

This is defined by

m4_displaymath(
<<<h_n^{(2)}(x) = j_n(x) + iy_n(x)>>>,
<<<spherical_hankel2(n,x) = spherical_bessel_j(n,x) - %i*spherical_bessel_y(n,x)>>>)

@opencatbox{Categories:}
@category{Package orthopoly}
@category{Bessel functions}
@closecatbox

@end deffn

@anchor{spherical_harmonic}
@deffn {Function} spherical_harmonic (@var{n}, @var{m}, @var{theta}, @var{phi})
The spherical harmonic function, 
m4_mathdot(<<<Y_n^m(\theta, \phi)>>>, <<<spherical_harmonic(n,m,theta,phi)>>>)

Spherical harmonics satisfy the angular part of Laplace's equation in spherical coordinates.

For integers @math{n} and @math{m} such
that 
m4_math(<<<n \geq |m|>>>,<<<n <= abs(m)>>>) 
and
for 
m4_mathdot(<<<\theta \in [0, \pi]>>>,<<<theta in [0, %pi]>>>) 
Maxima’s
spherical harmonic function can be defined by

m4_displaymath(
<<<Y_n^m(\theta, \phi) = (-1)^m \sqrt{{2n+1\over 4\pi} {(n-m)!\over (n+m)!}} P_n^m(\cos\theta) e^{im\phi}>>>,
<<<@math{spherical_harmonic(n, m, theta, phi)  :=  (-1)^m *  (((n-m)!*(2*n+1))/(4*%pi*(n+m)!))^(1/2)* exp(%i*m*phi)*assoc_legendre_p(n,m,cos(theta))}>>>)
 

Further, when 
m4_mathcomma(<<<n < |m|>>>, <<<n < abs(m)>>>) 
the
spherical harmonic function vanishes.

The factor @math{(-1)^m}, frequently used in Quantum mechanics, is
called the @url{https://en.wikipedia.org/wiki/Spherical_harmonics#Condon%E2%80%93Shortley_phase, Condon-Shortely phase}.
Some references, including @emph{NIST Digital Library of Mathematical Functions} omit
this factor; see @url{http://dlmf.nist.gov/14.30.E1}.

Reference: Merzbacher 9.64.

Some examples:
@c ===beg===
@c spherical_harmonic(1,0,theta,phi);
@c spherical_harmonic(1,1,theta,phi);
@c spherical_harmonic(1,-1,theta,phi);
@c spherical_harmonic(2,0,theta,phi);
@c factor(%);
@c ===end===
@example
(%i1) spherical_harmonic(1,0,theta,phi);
                              sqrt(3) cos(theta)
(%o1)                         ------------------
                                 2 sqrt(%pi)
(%i2) spherical_harmonic(1,1,theta,phi);
                                    %i phi
                          sqrt(3) %e       sin(theta)
(%o2)                     ---------------------------
                                 3/2
                                2    sqrt(%pi)
(%i3) spherical_harmonic(1,-1,theta,phi);
                                    - %i phi
                          sqrt(3) %e         sin(theta)
(%o3)                   - -----------------------------
                                  3/2
                                 2    sqrt(%pi)
(%i4) spherical_harmonic(2,0,theta,phi);
                                                              2
                                            3 (1 - cos(theta))
          sqrt(5) ((- 3 (1 - cos(theta))) + ------------------- + 1)
                                                     2
(%o4)     ----------------------------------------------------------
                                 2 sqrt(%pi)
(%i5) factor(%);
                                        2
                          sqrt(5) (3 cos (theta) - 1)
(%o5)                     ---------------------------
                                  4 sqrt(%pi)
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn

@anchor{unit_step}
@deffn {Function} unit_step (@var{x})
The left-continuous unit step function; thus
@code{unit_step (@var{x})} vanishes for @code{@var{x} <= 0} and equals
1 for @code{@var{x} > 0}.

If you want a unit step function that takes on the value 1/2 at zero,
use @mrefdot{hstep}

@opencatbox{Categories:}
@category{Package orthopoly}
@category{Mathematical functions}
@closecatbox

@end deffn

@anchor{ultraspherical}
@deffn {Function} ultraspherical (@var{n}, @var{a}, @var{x})
The ultraspherical polynomial, 
m4_math(<<<C_n^{(a)}(x)>>>, <<<ultraspherical(n,a,x)>>>) 
(also known as the Gegenbauer polynomial).

Reference: @urlaands{eqn 22.5.46,779}.

These polynomials can be given in terms of Jacobi polynomials:

m4_displaymath(
<<<C_n^{(\alpha)}(x) = {\Gamma\left(\alpha + {1\over 2}\right) \over \Gamma(2\alpha)}
   {\Gamma(n+2\alpha) \over \Gamma\left(n+\alpha + {1\over 2}\right)}
   P_n^{(\alpha-1/2, \alpha-1/2)}(x)>>>,
<<<ultraspherical(n,a,x) = gamma(a+1/2)/gamma(2*a)*gamma(n+2*a)/gamma(n+a+1/2)*jacobi_p(n,a-1/2,a-1/2,x)>>>)

or the series

m4_displaymath(
<<<C_n^{(\alpha)}(x) = \sum_{k=0}^{\lfloor n/2 \rfloor} {(-1)^k (\alpha)_{n-k} \over k! (n-2k)!}(2x)^{n-2k}>>>,
<<<@math{ultraspherical(n,a,x) = sum((-1)^k*pochhammer(a,n-k)/k!/(n-2*k)!*(2*x)^(n-2*k),k, 0, floor(n/2))}>>>)

or the Rodrigues formula

m4_displaymath(
<<<C_n^{(\alpha)}(x) = {1\over \kappa_n  w(x)} {d^n\over dx^n}\left(w(x)\left(1-x^2\right)^n\right)>>>,
<<<@math{ultraspherical(n,x) = 1/(k(n)*w(x))*diff(w(x)*(1-x^2)^n, x, n)}>>>
)

where

m4_displaymath(
<<<\eqalign{
w(x) &= \left(1-x^2\right)^{\alpha-{1\over 2}} \cr
\kappa_n &= {(-2)^n\left(\alpha + {1\over 2}\right)_n n!\over (2\alpha)_n} \cr
}>>>,
<<<@math{w(x) = (1-x^2)^(a-1/2)}

@math{k(n) = (-2)^n*pochhammer(a+1/2,n)*n!/pochhammer(2*a,n)}>>>)

Some examples:
@c ===beg===
@c ultraspherical(1,a,x);
@c factor(%);
@c factor(ultraspherical(2,a,x));
@c ===end===
@example
(%i1) ultraspherical(1,a,x);
                                   (2 a + 1) (1 - x)
(%o1)                     2 a (1 - -----------------)
                                              1
                                       2 (a + -)
                                              2
(%i2) factor(%);
(%o2)                                2 a x
(%i3) factor(ultraspherical(2,a,x));
                                     2      2
(%o3)                        a (2 a x  + 2 x  - 1)
@end example

@opencatbox{Categories:}
@category{Package orthopoly}
@closecatbox

@end deffn
