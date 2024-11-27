@c -*- mode: texinfo -*-
@menu
* Functions for Numbers::
* Functions for Complex Numbers::
* Combinatorial Functions::
* Root Exponential and Logarithmic Functions::
* Trigonometric Functions::
* Random Numbers::
@end menu

@c -----------------------------------------------------------------------------
@node Functions for Numbers, Functions for Complex Numbers, , Elementary Functions
@section Functions for Numbers
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{abs}
@deffn {Function} abs (@var{z})

The @code{abs} function represents the mathematical absolute value function and
works for both numerical and symbolic values. If the argument, @var{z}, is a
real or complex number, @code{abs} returns the absolute value of @var{z}. If
possible, symbolic expressions using the absolute value function are
also simplified.

Maxima can differentiate, integrate and calculate limits for expressions
containing @code{abs}. The @code{abs_integrate} package further extends
Maxima's ability to calculate integrals involving the abs function. See
(%i12) in the examples below.

When applied to a list or matrix, @code{abs} automatically distributes over
the terms. Similarly, it distributes over both sides of an
equation. To alter this behaviour, see the variable @mrefdot{distribute_over}

See also @mrefdot{cabs}

Examples:

Calculation of @code{abs} for real and complex numbers, including numerical
constants and various infinities. The first example shows how @code{abs}
distributes over the elements of a list.

@c ===beg===
@c abs([-4, 0, 1, 1+%i]);
@c abs((1+%i)*(1-%i));
@c abs(%e+%i);
@c abs([inf, infinity, minf]);
@c ===end===
@example
@group
(%i1) abs([-4, 0, 1, 1+%i]);
(%o1)                  [4, 0, 1, sqrt(2)]
@end group
@group
(%i2) abs((1+%i)*(1-%i));
(%o2)                           2
@end group
@group
(%i3) abs(%e+%i);
                                 2
(%o3)                     sqrt(%e  + 1)
@end group
@group
(%i4) abs([inf, infinity, minf]);
(%o4)                    [inf, inf, inf]
@end group
@end example

Simplification of expressions containing @code{abs}:

@c ===beg===
@c abs(x^2);
@c abs(x^3);
@c abs(abs(x));
@c abs(conjugate(x));
@c ===end===
@example
@group
(%i1) abs(x^2);
                                2
(%o1)                          x
@end group
@group
(%i2) abs(x^3);
                             2
(%o2)                       x  abs(x)
@end group
@group
(%i3) abs(abs(x));
(%o3)                        abs(x)
@end group
@group
(%i4) abs(conjugate(x));
(%o4)                        abs(x)
@end group
@end example

Integrating and differentiating with the @code{abs} function. Note that more
integrals involving the @code{abs} function can be performed, if the
@code{abs_integrate} package is loaded. The last example shows the Laplace
transform of @code{abs}: see @mrefdot{laplace}

@c ===beg===
@c diff(x*abs(x),x),expand;
@c integrate(abs(x),x);
@c integrate(x*abs(x),x);
@c load("abs_integrate")$
@c integrate(x*abs(x),x);
@c integrate(abs(x),x,-2,%pi);
@c laplace(abs(x),x,s);
@c ===end===
@example
@group
(%i1) diff(x*abs(x),x),expand;
(%o1)                       2 abs(x)
@end group
@group
(%i2) integrate(abs(x),x);
                            x abs(x)
(%o2)                       --------
                               2
@end group
@group
(%i3) integrate(x*abs(x),x);
                          /
                          |
(%o3)                     | x abs(x) dx
                          |
                          /
@end group
(%i4) load("abs_integrate")$
@group
(%i5) integrate(x*abs(x),x);
                           3
                          x  signum(x)
(%o5)                     ------------
                               3
@end group
@group
(%i6) integrate(abs(x),x,-2,%pi);
                               2
                            %pi
(%o6)                       ---- + 2
                             2
@end group
@group
(%i7) laplace(abs(x),x,s);
                               1
(%o7)                          --
                                2
                               s
@end group
@end example

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{ceiling}
@deffn {Function} ceiling (@var{x})

When @var{x} is a real number, return the least integer that 
is greater than or equal to @var{x}.

If @var{x} is a constant expression (@code{10 * %pi}, for example), 
@code{ceiling} evaluates @var{x} using big floating point numbers, and 
applies @code{ceiling} to the resulting big float.  Because @code{ceiling} uses
floating point evaluation, it's possible, although unlikely, that @code{ceiling}
could return an erroneous value for constant inputs.  To guard against errors,
the floating point evaluation is done using three values for @mrefdot{fpprec}

For non-constant inputs, @code{ceiling} tries to return a simplified value.
Here are examples of the simplifications that @code{ceiling} knows about:

@c ===beg===
@c ceiling (ceiling (x));
@c ceiling (floor (x));
@c declare (n, integer)$
@c [ceiling (n), ceiling (abs (n)), ceiling (max (n, 6))];
@c assume (x > 0, x < 1)$
@c ceiling (x);
@c tex (ceiling (a));
@c ===end===
@example
@group
(%i1) ceiling (ceiling (x));
(%o1)                      ceiling(x)
@end group
@group
(%i2) ceiling (floor (x));
(%o2)                       floor(x)
@end group
(%i3) declare (n, integer)$
@group
(%i4) [ceiling (n), ceiling (abs (n)), ceiling (max (n, 6))];
(%o4)                [n, abs(n), max(6, n)]
@end group
(%i5) assume (x > 0, x < 1)$
@group
(%i6) ceiling (x);
(%o6)                           1
@end group
@group
(%i7) tex (ceiling (a));
$$\left \lceil a \right \rceil$$
(%o7)                         false
@end group
@end example

The @code{ceiling} function distributes over lists, matrices and equations.
See @mrefdot{distribute_over}

Finally, for all inputs that are manifestly complex, @code{ceiling} returns 
a noun form.

If the range of a function is a subset of the integers, it can be declared to
be @code{integervalued}.  Both the @code{ceiling} and @mref{floor} functions
can use this information; for example:

@c ===beg===
@c declare (f, integervalued)$
@c floor (f(x));
@c ceiling (f(x) - 1);
@c ===end===
@example
(%i1) declare (f, integervalued)$
@group
(%i2) floor (f(x));
(%o2)                         f(x)
@end group
@group
(%i3) ceiling (f(x) - 1);
(%o3)                       f(x) - 1
@end group
@end example

Example use:

@c ===beg===
@c unitfrac(r) := block([uf : [], q],
@c     if not(ratnump(r)) then 
@c        error("unitfrac: argument must be a rational number"),
@c     while r # 0 do (
@c         uf : cons(q : 1/ceiling(1/r), uf),
@c         r : r - q),
@c     reverse(uf));
@c unitfrac (9/10);
@c apply ("+", %);
@c unitfrac (-9/10);
@c apply ("+", %);
@c unitfrac (36/37);
@c apply ("+", %);
@c ===end===
@example
@group
(%i1) unitfrac(r) := block([uf : [], q],
    if not(ratnump(r)) then
       error("unitfrac: argument must be a rational number"),
    while r # 0 do (
        uf : cons(q : 1/ceiling(1/r), uf),
        r : r - q),
    reverse(uf));
(%o1) unitfrac(r) := block([uf : [], q], 
if not ratnump(r) then error("unitfrac: argument must be a rational number"
                                     1
), while r # 0 do (uf : cons(q : ----------, uf), r : r - q), 
                                         1
                                 ceiling(-)
                                         r
reverse(uf))
@end group
@group
(%i2) unitfrac (9/10);
                            1  1  1
(%o2)                      [-, -, --]
                            2  3  15
@end group
@group
(%i3) apply ("+", %);
                               9
(%o3)                          --
                               10
@end group
@group
(%i4) unitfrac (-9/10);
                                  1
(%o4)                       [- 1, --]
                                  10
@end group
@group
(%i5) apply ("+", %);
                                9
(%o5)                         - --
                                10
@end group
@group
(%i6) unitfrac (36/37);
                        1  1  1  1    1
(%o6)                  [-, -, -, --, ----]
                        2  3  8  69  6808
@end group
@group
(%i7) apply ("+", %);
                               36
(%o7)                          --
                               37
@end group
@end example

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{entier}
@deffn {Function} entier (@var{x})

Returns the largest integer less than or equal to @var{x} where @var{x} is
numeric.  @mref{fix} (as in @code{fixnum}) is a synonym for this, so
@code{fix(@var{x})} is precisely the same.

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{floor}
@deffn {Function} floor (@var{x})

When @var{x} is a real number, return the largest integer that is less than or
equal to @var{x}.

If @var{x} is a constant expression (@code{10 * %pi}, for example), @code{floor}
evaluates @var{x} using big floating point numbers, and applies @code{floor} to
the resulting big float. Because @code{floor} uses floating point evaluation,
it's possible, although unlikely, that @code{floor} could return an erroneous
value for constant inputs.  To guard against errors, the floating point
evaluation is done using three values for @mrefdot{fpprec}

For non-constant inputs, @code{floor} tries to return a simplified value.  Here
are examples of the simplifications that @code{floor} knows about:

@c ===beg===
@c floor (ceiling (x));
@c floor (floor (x));
@c declare (n, integer)$
@c [floor (n), floor (abs (n)), floor (min (n, 6))];
@c assume (x > 0, x < 1)$
@c floor (x);
@c tex (floor (a));
@c ===end===
@example
@group
(%i1) floor (ceiling (x));
(%o1)                      ceiling(x)
@end group
@group
(%i2) floor (floor (x));
(%o2)                       floor(x)
@end group
(%i3) declare (n, integer)$
@group
(%i4) [floor (n), floor (abs (n)), floor (min (n, 6))];
(%o4)                [n, abs(n), min(6, n)]
@end group
(%i5) assume (x > 0, x < 1)$
@group
(%i6) floor (x);
(%o6)                           0
@end group
@group
(%i7) tex (floor (a));
$$\left \lfloor a \right \rfloor$$
(%o7)                         false
@end group
@end example

The @code{floor} function distributes over lists, matrices and equations.
See @mrefdot{distribute_over}

Finally, for all inputs that are manifestly complex, @code{floor} returns 
a noun form.

If the range of a function is a subset of the integers, it can be declared to
be @code{integervalued}.  Both the @mref{ceiling} and @code{floor} functions
can use this information; for example:

@c ===beg===
@c declare (f, integervalued)$
@c floor (f(x));
@c ceiling (f(x) - 1);
@c ===end===
@example
(%i1) declare (f, integervalued)$
@group
(%i2) floor (f(x));
(%o2)                         f(x)
@end group
@group
(%i3) ceiling (f(x) - 1);
(%o3)                       f(x) - 1
@end group
@end example

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{fix}
@deffn {Function} fix (@var{x})

A synonym for @code{entier (@var{x})}.

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{hstep}
@deffn {Function} hstep (@var{x})
The Heaviside unit step function, equal to 0 if @var{x} is negative,
equal to 1 if @var{x} is positive and equal to 1/2 if @var{x} is equal
to zero.

If you want a unit step function that takes on the value of 0 at @var{x}
equal to zero, use @mrefdot{unit_step}

@opencatbox{Categories:}
@category{Laplace transform}
@category{Mathematical functions}
@closecatbox

@end deffn

@c -----------------------------------------------------------------------------
@anchor{lmax}
@deffn {Function} lmax (@var{L})

When @var{L} is a list or a set, return @code{apply ('max, args (@var{L}))}.
When @var{L} is not a list or a set, signal an error.
See also @mref{lmin} and @mrefdot{max}

@opencatbox{Categories:}
@category{Mathematical functions}
@category{Lists}
@category{Sets}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{lmin}
@deffn {Function} lmin (@var{L})

When @var{L} is a list or a set, return @code{apply ('min, args (@var{L}))}.
When @var{L} is not a list or a set, signal an error.
See also @mref{lmax} and @mrefdot{min}

@opencatbox{Categories:}
@category{Mathematical functions}
@category{Lists}
@category{Sets}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{max}
@deffn {Function} max (@var{x_1}, @dots{}, @var{x_n})

Return a simplified value for the numerical maximum of the expressions @var{x_1} 
through @var{x_n}. For an empty argument list, @code{max} yields @code{minf}.

The option variable @code{maxmin_effort} controls which simplification methods are 
applied. Using the default value of @emph{twelve} for @code{maxmin_effort}, 
@code{max} uses @emph{all} available simplification methods. To to inhibit all 
simplifications, set @code{maxmin_effort} to zero.

When @code{maxmin_effort} is one or more, for an explicit list of real numbers, 
@code{max} returns a number. 

Unless @code{max} needs to simplify a lengthy list of expressions, we suggest using 
the default value of @code{maxmin_effort}. Setting @code{maxmin_effort} to zero 
(no simplifications), will cause problems for some Maxima functions; accordingly, 
generally @code{maxmin_effort} should be nonzero.

See also @mref{min}, @mrefdot{lmax}, and @mrefdot{lmin}.

@b{Examples:}

In the first example, setting @code{maxmin_effort} to zero suppresses simplifications.
@example
(%i1) block([maxmin_effort : 0], max(1,2,x,x, max(a,b)));
(%o1) max(1,2,max(a,b),x,x)

(%i2) block([maxmin_effort : 1], max(1,2,x,x, max(a,b)));
(%o2) max(2,a,b,x)
@end example

When @code{maxmin_effort} is two or more, @code{max} compares pairs of members:
@example
(%i1) block([maxmin_effort : 1], max(x,x+1,x+3));
(%o1) max(x,x+1,x+3)

(%i2) block([maxmin_effort : 2], max(x,x+1,x+3));
(%o2) x+3
@end example

Finally, when @code{maxmin_effort} is three or more, @code{max} compares triples 
members and excludes those that are in between; for example
@example
(%i1) block([maxmin_effort : 4], max(x, 2*x, 3*x, 4*x));
(%o1) max(x,4*x)
@end example

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@anchor{min}
@deffn {Function} min (@var{x_1}, @dots{}, @var{x_n})

Return a simplified value for the numerical minimum of the expressions @var{x_1} 
through @var{x_n}. For an empty argument list, @code{minf} yields @code{inf}.

The option variable @code{maxmin_effort} controls which simplification methods are 
applied. Using the default value of @emph{twelve} for @code{maxmin_effort}, 
@code{max} uses @emph{all} available simplification methods. To to inhibit all 
simplifications, set @code{maxmin_effort} to zero.

When @code{maxmin_effort} is one or more, for an explicit list of real numbers, 
@code{min} returns a number. 

Unless @code{min} needs to simplify a lengthy list of expressions, we suggest using 
the default value of @code{maxmin_effort}. Setting @code{maxmin_effort} to zero 
(no simplifications), will cause problems for some Maxima functions; accordingly, 
generally @code{maxmin_effort} should be nonzero.

See also @mref{max}, @mrefdot{lmax}, and @mrefdot{lmin}.

@b{Examples:}

In the first example, setting @code{maxmin_effort} to zero suppresses simplifications.
@example
(%i1) block([maxmin_effort : 0], min(1,2,x,x, min(a,b)));
(%o1) min(1,2,a,b,x,x)

(%i2) block([maxmin_effort : 1], min(1,2,x,x, min(a,b)));
(%o2) min(1,a,b,x)
@end example

When @code{maxmin_effort} is two or more, @code{min} compares pairs of members:
@example
(%i1) block([maxmin_effort : 1], min(x,x+1,x+3));
(%o1) min(x,x+1,x+3)

(%i2) block([maxmin_effort : 2], min(x,x+1,x+3));
(%o2) x
@end example

Finally, when @code{maxmin_effort} is three or more, @code{min} compares triples 
members and excludes those that are in between; for example
@example
(%i1) block([maxmin_effort : 4], min(x, 2*x, 3*x, 4*x));
(%o1) max(x,4*x)
@end example

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{round}
@deffn {Function} round (@var{x})

When @var{x} is a real number, returns the closest integer to @var{x}.
Multiples of 1/2 are rounded to the nearest even integer.  Evaluation of
@var{x} is similar to @mref{floor} and @mrefdot{ceiling}

The @code{round} function distributes over lists, matrices and equations.
See @mrefdot{distribute_over}

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{signum}
@deffn {Function} signum (@var{x})

For either real or complex numbers @var{x}, the signum function returns
0 if @var{x} is zero; for a nonzero numeric input @var{x}, the signum function
returns @code{x/abs(x)}.

For non-numeric inputs, Maxima attempts to determine the sign of the input.
When the sign is negative, zero, or positive, @code{signum} returns -1,0, 1,
respectively.  For all other values for the sign, @code{signum} a simplified but
equivalent form.  The simplifications include reflection (@code{signum(-x)}
gives @code{-signum(x)}) and multiplicative identity (@code{signum(x*y)} gives
@code{signum(x) * signum(y)}).

The @code{signum} function distributes over a list, a matrix, or an
equation.  See @mref{sign} and @mrefdot{distribute_over}

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{truncate}
@deffn {Function} truncate (@var{x})

When @var{x} is a real number, return the closest integer to @var{x} not
greater in absolute value than @var{x}.  Evaluation of @var{x} is similar
to @mref{floor} and @mrefdot{ceiling}

The @code{truncate} function distributes over lists, matrices and equations.
See @mrefdot{distribute_over}

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@node Functions for Complex Numbers, Combinatorial Functions, Functions for Numbers, Elementary Functions
@section Functions for Complex Numbers
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{cabs}
@deffn {Function} cabs (@var{expr})

Calculates the absolute value of an expression representing a complex
number.  Unlike the function @mrefcomma{abs} the @code{cabs} function always
decomposes its argument into a real and an imaginary part.  If @code{x} and
@code{y} represent real variables or expressions, the @code{cabs} function
calculates the absolute value of @code{x + %i*y} as

@c ===beg===
@c cabs (1);
@c cabs (1 + %i);
@c cabs (exp (%i));
@c cabs (exp (%pi * %i));
@c cabs (exp (3/2 * %pi * %i));
@c cabs (17 * exp (2 * %i));
@c ===end===
@example
@group
(%i1) cabs (1);
(%o1)                           1
@end group
@group
(%i2) cabs (1 + %i);
(%o2)                        sqrt(2)
@end group
@group
(%i3) cabs (exp (%i));
(%o3)                           1
@end group
@group
(%i4) cabs (exp (%pi * %i));
(%o4)                           1
@end group
@group
(%i5) cabs (exp (3/2 * %pi * %i));
(%o5)                           1
@end group
@group
(%i6) cabs (17 * exp (2 * %i));
(%o6)                          17
@end group
@end example

If @code{cabs} returns a noun form this most commonly is caused by
some properties of the variables involved not being known:

@c ===beg===
@c cabs (a+%i*b);
@c declare(a,real,b,real);
@c cabs (a+%i*b);
@c assume(a>0,b>0);
@c cabs (a+%i*b);
@c ===end===
@example
@group
(%i1) cabs (a+%i*b);
                                2    2
(%o1)                     sqrt(b  + a )
@end group
@group
(%i2) declare(a,real,b,real);
(%o2)                         done
@end group
@group
(%i3) cabs (a+%i*b);
                                2    2
(%o3)                     sqrt(b  + a )
@end group
@group
(%i4) assume(a>0,b>0);
(%o4)                    [a > 0, b > 0]
@end group
@group
(%i5) cabs (a+%i*b);
                                2    2
(%o5)                     sqrt(b  + a )
@end group
@end example

The @code{cabs} function can use known properties like symmetry properties of
complex functions to help it calculate the absolute value of an expression.  If
such identities exist, they can be advertised to @code{cabs} using function
properties.  The symmetries that @code{cabs} understands are: mirror symmetry,
conjugate function and complex characteristic.

@code{cabs} is a verb function and is not suitable for symbolic
calculations.  For such calculations (including integration,
differentiation and taking limits of expressions containing absolute
values), use @mrefdot{abs}

The result of @code{cabs} can include the absolute value function,
@mrefcomma{abs} and the arc tangent, @mrefdot{atan2}

When applied to a list or matrix, @code{cabs} automatically distributes over
the terms.  Similarly, it distributes over both sides of an equation.

For further ways to compute with complex numbers, see the functions
@mrefcomma{rectform} @mrefcomma{realpart} @mrefcomma{imagpart}@w{}
@mrefcomma{carg} @mref{conjugate} and @mrefdot{polarform}

Examples:

Examples with @mref{sqrt} and @mrefdot{sin}

@c ===beg===
@c cabs(sqrt(1+%i*x));
@c cabs(sin(x+%i*y));
@c ===end===
@example
@group
(%i1) cabs(sqrt(1+%i*x));
                             2     1/4
(%o1)                      (x  + 1)
@end group
@group
(%i2) cabs(sin(x+%i*y));
                    2        2         2        2
(%o2)       sqrt(cos (x) sinh (y) + sin (x) cosh (y))
@end group
@end example

The error function, @mrefcomma{erf} has mirror symmetry, which is used here in
the calculation of the absolute value with a complex argument:

@c ===beg===
@c cabs(erf(x+%i*y));
@c ===end===
@example
@group
(%i1) cabs(erf(x+%i*y));
                                          2
           (erf(%i y + x) - erf(%i y - x))
(%o1) sqrt(--------------------------------
                          4
                                                               2
                              (- erf(%i y + x) - erf(%i y - x))
                            - ----------------------------------)
                                              4
@end group
@end example

Maxima knows complex identities for the Bessel functions, which allow
it to compute the absolute value for complex arguments.  Here is an
example for @mrefdot{bessel_j}

@c ===beg===
@c cabs(bessel_j(1,%i));
@c ===end===
@example
@group
(%i1) cabs(bessel_j(1,%i));
(%o1)                    bessel_i(1, 1)
@end group
@end example

@opencatbox{Categories:}
@category{Complex variables}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{carg}
@deffn {Function} carg (@var{z})

Returns the complex argument of @var{z}.  The complex argument is an angle
@code{theta} in @code{(-%pi, %pi]} such that @code{r exp (theta %i) = @var{z}}
where @code{r} is the magnitude of @var{z}.

@code{carg} is a computational function, not a simplifying function.
@c PROBABLY NEED TO EXPLAIN IMPLICATIONS OF ABOVE STATEMENT

See also @mref{abs} (complex magnitude), @mrefcomma{polarform}@w{}
@mrefcomma{rectform} @mrefcomma{realpart} and @mrefdot{imagpart}

Examples:

@c ===beg===
@c carg (1);
@c carg (1 + %i);
@c carg (exp (%i));
@c carg (exp (%pi * %i));
@c carg (exp (3/2 * %pi * %i));
@c carg (17 * exp (2 * %i));
@c ===end===
@example
@group
(%i1) carg (1);
(%o1)                           0
@end group
@group
(%i2) carg (1 + %i);
                               %pi
(%o2)                          ---
                                4
@end group
@group
(%i3) carg (exp (%i));
                               sin(1)
(%o3)                     atan(------)
                               cos(1)
@end group
@group
(%i4) carg (exp (%pi * %i));
(%o4)                          %pi
@end group
@group
(%i5) carg (exp (3/2 * %pi * %i));
                                %pi
(%o5)                         - ---
                                 2
@end group
@group
(%i6) carg (17 * exp (2 * %i));
                            sin(2)
(%o6)                  atan(------) + %pi
                            cos(2)
@end group
@end example

If @code{carg} returns a noun form this most commonly is caused by
some properties of the variables involved not being known:

@c ===beg===
@c carg (a+%i*b);
@c declare(a,real,b,real);
@c carg (a+%i*b);
@c assume(a>0,b>0);
@c carg (a+%i*b);
@c ===end===
@example
@group
(%i1) carg (a+%i*b);
(%o1)                      atan2(b, a)
@end group
@group
(%i2) declare(a,real,b,real);
(%o2)                         done
@end group
@group
(%i3) carg (a+%i*b);
(%o3)                      atan2(b, a)
@end group
@group
(%i4) assume(a>0,b>0);
(%o4)                    [a > 0, b > 0]
@end group
@group
(%i5) carg (a+%i*b);
                                  b
(%o5)                        atan(-)
                                  a
@end group
@end example

@opencatbox{Categories:}
@category{Complex variables}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{conjugate}
@deffn {Function} conjugate (@var{x})

Returns the complex conjugate of @var{x}.

@c ===beg===
@c declare ([aa, bb], real, cc, complex, ii, imaginary);
@c conjugate (aa + bb*%i);
@c conjugate (cc);
@c conjugate (ii);
@c conjugate (xx + yy);
@c ===end===
@example
@group
(%i1) declare ([aa, bb], real, cc, complex, ii, imaginary);
(%o1)                         done
@end group
@group
(%i2) conjugate (aa + bb*%i);
(%o2)                      aa - %i bb
@end group
@group
(%i3) conjugate (cc);
(%o3)                     conjugate(cc)
@end group
@group
(%i4) conjugate (ii);
(%o4)                         - ii
@end group
@group
(%i5) conjugate (xx + yy);
(%o5)                        yy + xx
@end group
@end example

@opencatbox{Categories:}
@category{Complex variables}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{imagpart}
@deffn {Function} imagpart (@var{expr})

Returns the imaginary part of the expression @var{expr}.

@code{imagpart} is a computational function, not a simplifying function.
@c PROBABLY NEED TO EXPLAIN IMPLICATIONS OF ABOVE STATEMENT
@c SEE ALSO SF BUG REPORT # 902290

See also @mrefcomma{abs} @mrefcomma{carg} @mrefcomma{polarform}@w{}
@mrefcomma{rectform} and @mrefdot{realpart}

Example:

@c ===beg===
@c imagpart (a+b*%i);
@c imagpart (1+sqrt(2)*%i);
@c imagpart (1);
@c imagpart (sqrt(2)*%i);
@c ===end===
@example
@group
(%i1) imagpart (a+b*%i);
(%o1)                           b
@end group
@group
(%i2) imagpart (1+sqrt(2)*%i);
(%o2)                        sqrt(2)
@end group
@group
(%i3) imagpart (1);
(%o3)                           0
@end group
@group
(%i4) imagpart (sqrt(2)*%i);
(%o4)                        sqrt(2)
@end group
@end example

@opencatbox{Categories:}
@category{Complex variables}
@closecatbox
@end deffn

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{polarform}
@deffn {Function} polarform (@var{expr})

Returns an expression @code{r %e^(%i theta)} equivalent to @var{expr},
such that @code{r} and @code{theta} are purely real.

Example:

@c ===beg===
@c polarform(a+b*%i);
@c polarform(1+%i);
@c polarform(1+2*%i);
@c ===end===
@example
@group
(%i1) polarform(a+b*%i);
                       2    2    %i atan2(b, a)
(%o1)            sqrt(b  + a ) %e
@end group
@group
(%i2) polarform(1+%i);
                                  %i %pi
                                  ------
                                    4
(%o2)                   sqrt(2) %e
@end group
@group
(%i3) polarform(1+2*%i);
                                %i atan(2)
(%o3)                 sqrt(5) %e
@end group
@end example

@opencatbox{Categories:}
@category{Complex variables}
@category{Exponential and logarithm functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{realpart}
@deffn {Function} realpart (@var{expr})

Returns the real part of @var{expr}.  @code{realpart} and @mref{imagpart} will
work on expressions involving trigonometric and hyperbolic functions,
as well as square root, logarithm, and exponentiation.

Example:

@c ===beg===
@c realpart (a+b*%i);
@c realpart (1+sqrt(2)*%i);
@c realpart (sqrt(2)*%i);
@c realpart (1);
@c ===end===
@example
@group
(%i1) realpart (a+b*%i);
(%o1)                           a
@end group
@group
(%i2) realpart (1+sqrt(2)*%i);
(%o2)                           1
@end group
@group
(%i3) realpart (sqrt(2)*%i);
(%o3)                           0
@end group
@group
(%i4) realpart (1);
(%o4)                           1
@end group
@end example

@opencatbox{Categories:}
@category{Complex variables}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{rectform}
@deffn {Function} rectform (@var{expr})

Returns an expression @code{a + b %i} equivalent to @var{expr},
such that @var{a} and @var{b} are purely real.

Example:

@c ===beg===
@c rectform(sqrt(2)*%e^(%i*%pi/4));
@c rectform(sqrt(b^2+a^2)*%e^(%i*atan2(b, a)));
@c rectform(sqrt(5)*%e^(%i*atan(2)));
@c ===end===
@example
@group
(%i1) rectform(sqrt(2)*%e^(%i*%pi/4));
(%o1)                        %i + 1
@end group
@group
(%i2) rectform(sqrt(b^2+a^2)*%e^(%i*atan2(b, a)));
(%o2)                       %i b + a
@end group
@group
(%i3) rectform(sqrt(5)*%e^(%i*atan(2)));
(%o3)                       2 %i + 1
@end group
@end example

@opencatbox{Categories:}
@category{Complex variables}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@node Combinatorial Functions, Root Exponential and Logarithmic Functions, Functions for Complex Numbers, Elementary Functions
@section Combinatorial Functions
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{!!}
@fnindex Double factorial
@deffn {Operator} !!

The double factorial operator.

For an integer, float, or rational number @code{n}, @code{n!!} evaluates to the
product @code{n (n-2) (n-4) (n-6) ... (n - 2 (k-1))} where @code{k} is equal to
@code{entier (n/2)}, that is, the largest integer less than or equal to
@code{n/2}.  Note that this definition does not coincide with other published
definitions for arguments which are not integers.
@c REPORTED TO BUG TRACKER AS BUG # 1093138 !!!

For an even (or odd) integer @code{n}, @code{n!!} evaluates to the product of
all the consecutive even (or odd) integers from 2 (or 1) through @code{n}
inclusive.

For an argument @code{n} which is not an integer, float, or rational, @code{n!!}
yields a noun form @code{genfact (n, n/2, 2)}.
@c n!! IS NEITHER SIMPLIFIED NOR EVALUATED IN THIS CASE 
@c -- MENTION THAT? OR TOO MUCH DETAIL ???

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Operators}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{binomial}
@deffn {Function} binomial (@var{x}, @var{y})

The binomial coefficient @code{@var{x}!/(@var{y}! (@var{x} - @var{y})!)}.
If @var{x} and @var{y} are integers, then the numerical value of the binomial
coefficient is computed.  If @var{y}, or @var{x - y}, is an integer, the
binomial coefficient is expressed as a polynomial.

Examples:

@c ===beg===
@c binomial (11, 7);
@c 11! / 7! / (11 - 7)!;
@c binomial (x, 7);
@c binomial (x + 7, x);
@c binomial (11, y);
@c ===end===
@example
@group
(%i1) binomial (11, 7);
(%o1)                          330
@end group
@group
(%i2) 11! / 7! / (11 - 7)!;
(%o2)                          330
@end group
@group
(%i3) binomial (x, 7);
        (x - 6) (x - 5) (x - 4) (x - 3) (x - 2) (x - 1) x
(%o3)   -------------------------------------------------
                              5040
@end group
@group
(%i4) binomial (x + 7, x);
      (x + 1) (x + 2) (x + 3) (x + 4) (x + 5) (x + 6) (x + 7)
(%o4) -------------------------------------------------------
                               5040
@end group
@group
(%i5) binomial (11, y);
(%o5)                    binomial(11, y)
@end group
@end example

@opencatbox{Categories:}
@category{Number theory}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{factcomb}
@deffn {Function} factcomb (@var{expr})

Tries to combine the coefficients of factorials in @var{expr}
with the factorials themselves by converting, for example, @code{(n + 1)*n!}
into @code{(n + 1)!}.

@mref{sumsplitfact} if set to @code{false} will cause @mref{minfactorial} to be
applied after a @code{factcomb}.

Example:

@c ===beg===
@c sumsplitfact;
@c (n + 1)*(n + 1)*n!;
@c factcomb (%);
@c sumsplitfact: not sumsplitfact;
@c (n + 1)*(n + 1)*n!;
@c factcomb (%);
@c ===end===
@example
@group
(%i1) sumsplitfact;
(%o1)                         true
@end group
@group
(%i2) (n + 1)*(n + 1)*n!;
                                  2
(%o2)                      (n + 1)  n!
@end group
@group
(%i3) factcomb (%);
(%o3)                  (n + 2)! - (n + 1)!
@end group
@group
(%i4) sumsplitfact: not sumsplitfact;
(%o4)                         false
@end group
@group
(%i5) (n + 1)*(n + 1)*n!;
                                  2
(%o5)                      (n + 1)  n!
@end group
@group
(%i6) factcomb (%);
(%o6)                 n (n + 1)! + (n + 1)!
@end group
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{!}
@anchor{factorial}
@deffn  {Function} factorial
@deffnx {Operator} !

Represents the factorial function.  Maxima treats @code{factorial (@var{x})}
the same as @code{@var{x}!}.

For any complex number @code{x}, except for negative integers, @code{x!} is 
defined as @code{gamma(x+1)}.

For an integer @code{x}, @code{x!} simplifies to the product of the integers 
from 1 to @code{x} inclusive.  @code{0!} simplifies to 1.  For a real or complex 
number in float or bigfloat precision @code{x}, @code{x!} simplifies to the 
value of @code{gamma (x+1)}.  For @code{x} equal to @code{n/2} where @code{n} is 
an odd integer, @code{x!} simplifies to a rational factor times 
@code{sqrt (%pi)} (since @code{gamma (1/2)} is equal to @code{sqrt (%pi)}).

The option variables @mref{factlim} and @mref{gammalim} control the numerical
evaluation of factorials for integer and rational arguments.  The functions 
@mref{minfactorial} and @mref{factcomb} simplifies expressions containing
factorials.

The functions @mrefcomma{gamma} @mrefcomma{bffac} and @mref{cbffac} are
varieties of the gamma function.  @code{bffac} and @code{cbffac} are called
internally by @code{gamma} to evaluate the gamma function for real and complex
numbers in bigfloat precision.

@mref{makegamma} substitutes @code{gamma} for factorials and related functions.

Maxima knows the derivative of the factorial function and the limits for 
specific values like negative integers.

The option variable @mref{factorial_expand} controls the simplification of
expressions like @code{(n+x)!}, where @code{n} is an integer.

See also @mrefdot{binomial}

The factorial of an integer is simplified to an exact number unless the operand 
is greater than @code{factlim}.  The factorial for real and complex numbers is 
evaluated in float or bigfloat precision.

@c ===beg===
@c factlim : 10;
@c [0!, (7/2)!, 8!, 20!];
@c [4,77!, (1.0+%i)!];
@c [2.86b0!, (1.0b0+%i)!];
@c ===end===
@example
@group
(%i1) factlim : 10;
(%o1)                          10
@end group
@group
(%i2) [0!, (7/2)!, 8!, 20!];
                     105 sqrt(%pi)
(%o2)            [1, -------------, 40320, 20!]
                          16
@end group
@group
(%i3) [4,77!, (1.0+%i)!];
(%o3) [4, 77!, 0.3430658398165453 %i + 0.6529654964201667]
@end group
@group
(%i4) [2.86b0!, (1.0b0+%i)!];
(%o4) [5.046635586910012b0, 3.430658398165454b-1 %i
                                          + 6.529654964201667b-1]
@end group
@end example

The factorial of a known constant, or general expression is not simplified.
Even so it may be possible to simplify the factorial after evaluating the
operand.

@c ===beg===
@c [(%i + 1)!, %pi!, %e!, (cos(1) + sin(1))!];
@c ev (%, numer, %enumer);
@c ===end===
@example
@group
(%i1) [(%i + 1)!, %pi!, %e!, (cos(1) + sin(1))!];
(%o1)      [(%i + 1)!, %pi!, %e!, (sin(1) + cos(1))!]
@end group
@group
(%i2) ev (%, numer, %enumer);
(%o2) [0.3430658398165453 %i + 0.6529654964201667, 
         7.188082728976031, 4.260820476357003, 1.227580202486819]
@end group
@end example

@c REMOVING THIS EXAMPLE. IT IS NOT SPECIFIC FOR THE FACTORIAL FUNCTION:
@c The factorial of an unbound symbol is not simplified.

@c @c ===beg===
@c @c kill (foo);
@c @c foo!;
@c @c ===end===
@c @example
@c (%i1) kill (foo);
@c (%o1)                         done
@c (%i2) foo!;
@c (%o2)                         foo!
@c @end example

Factorials are simplified, not evaluated.
Thus @code{x!} may be replaced even in a quoted expression.

@c ===beg===
@c '([0!, (7/2)!, 4.77!, 8!, 20!]);
@c ===end===
@example
@group
(%i1) '([0!, (7/2)!, 4.77!, 8!, 20!]);
          105 sqrt(%pi)
(%o1) [1, -------------, 81.44668037931197, 40320, 
               16
                                             2432902008176640000]
@end group
@end example

Maxima knows the derivative of the factorial function.

@c ===beg===
@c diff(x!,x);
@c ===end===
@example
@group
(%i1) diff(x!,x);
(%o1)                    x! psi (x + 1)
                               0
@end group
@end example

The option variable @code{factorial_expand} controls expansion and 
simplification of expressions with the factorial function.

@c ===beg===
@c (n+1)!/n!,factorial_expand:true;
@c ===end===
@example
@group
(%i1) (n+1)!/n!,factorial_expand:true;
(%o1)                         n + 1
@end group
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Operators}
@closecatbox
@end deffn

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{factlim}
@defvr {Option variable} factlim
Default value: 100000

@code{factlim} specifies the highest factorial which is
automatically expanded.  If it is -1 then all integers are expanded.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{factorial_expand}
@defvr {Option variable} factorial_expand
Default value: false

The option variable @code{factorial_expand} controls the simplification of 
expressions like @code{(x+n)!}, where @code{n} is an integer.
See @mref{factorial} for an example.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end defvr

@c IS THIS DEFINITION CORRECT ??

@c -----------------------------------------------------------------------------
@anchor{genfact}
@deffn {Function} genfact (@var{x}, @var{y}, @var{z})

Returns the generalized factorial, defined as
@code{x (x-z) (x - 2 z) ... (x - (y - 1) z)}.  Thus, when @var{x} is an integer,
@code{genfact (x, x, 1) = x!} and @code{genfact (x, x/2, 2) = x!!}.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{minfactorial}
@deffn {Function} minfactorial (@var{expr})

Examines @var{expr} for occurrences of two factorials
which differ by an integer.
@code{minfactorial} then turns one into a polynomial times the other.

@c I CAN'T TELL WHAT THIS IS SUPPOSED TO MEAN. !!!
@c minfactorial DOESN'T SEEM TO DO ANYTHING binomial DOESN'T DO BY ITSELF !!!
@c LOOKING AT THE minfactorial CODE DOESN'T HELP !!!
@c If exp involves binomial coefficients then they will be
@c converted into ratios of factorials.

@c ===beg===
@c n!/(n+2)!;
@c minfactorial (%);
@c ===end===
@example
@group
(%i1) n!/(n+2)!;
                               n!
(%o1)                       --------
                            (n + 2)!
@end group
@group
(%i2) minfactorial (%);
                                1
(%o2)                    ---------------
                         (n + 1) (n + 2)
@end group
@end example

@opencatbox{Categories:}
@category{Number theory}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{sumsplitfact}
@defvr {Option variable} sumsplitfact
Default value: @code{true}

When @code{sumsplitfact} is @code{false},
@c "IS APPLIED" -- UNDER WHAT CIRCUMSTANCES EXACTLY ??
@mref{minfactorial} is applied after a @mrefdot{factcomb}

@c ===beg===
@c sumsplitfact;
@c n!/(n+2)!;   
@c factcomb(%); 
@c sumsplitfact: not sumsplitfact ;
@c n!/(n+2)!;
@c factcomb(%);
@c ===end=== 
@example
@group
(%i1) sumsplitfact;
(%o1)                         true
@end group
@group
(%i2) n!/(n+2)!;
                               n!
(%o2)                       --------
                            (n + 2)!
@end group
@group
(%i3) factcomb(%);
                               n!
(%o3)                       --------
                            (n + 2)!
@end group
@group
(%i4) sumsplitfact: not sumsplitfact ;
(%o4)                         false
@end group
@group
(%i5) n!/(n+2)!;
                               n!
(%o5)                       --------
                            (n + 2)!
@end group
@group
(%i6) factcomb(%);
                                1
(%o6)                    ---------------
                         (n + 1) (n + 2)
@end group
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@node Root Exponential and Logarithmic Functions, Trigonometric Functions, Combinatorial Functions, Elementary Functions
@section Root, Exponential and Logarithmic Functions
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{%e_to_numlog}
@defvr {Option variable} %e_to_numlog
Default value: @code{false}

When @code{true}, @code{r} some rational number, and @code{x} some expression,
@code{%e^(r*log(x))} will be simplified into @code{x^r} .  It should be noted
that the @code{radcan} command also does this transformation, and more
complicated transformations of this ilk as well.  The @code{logcontract}
command "contracts" expressions containing @code{log}.

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%emode}
@defvr {Option variable} %emode
Default value: @code{true}

When @code{%emode} is @code{true}, @code{%e^(%pi %i x)} is simplified as
follows.

@code{%e^(%pi %i x)} simplifies to @code{cos (%pi x) + %i sin (%pi x)} if
@code{x} is a floating point number, an integer, or a multiple of 1/2, 1/3, 1/4,
or 1/6, and then further simplified.

For other numerical @code{x}, @code{%e^(%pi %i x)} simplifies to
@code{%e^(%pi %i y)} where @code{y} is @code{x - 2 k} for some integer @code{k}
such that @code{abs(y) < 1}.

When @code{%emode} is @code{false}, no special simplification of
@code{%e^(%pi %i x)} is carried out.

@c ===beg===
@c %emode;
@c %e^(%pi*%i*1);
@c %e^(%pi*%i*216/144);
@c %e^(%pi*%i*192/144);
@c %e^(%pi*%i*180/144);
@c %e^(%pi*%i*120/144);
@c %e^(%pi*%i*121/144);
@c ===end===
@example
@group
(%i1) %emode;
(%o1)                         true
@end group
@group
(%i2) %e^(%pi*%i*1);
(%o2)                          - 1
@end group
@group
(%i3) %e^(%pi*%i*216/144);
(%o3)                         - %i
@end group
@group
(%i4) %e^(%pi*%i*192/144);
                          sqrt(3) %i   1
(%o4)                   - ---------- - -
                              2        2
@end group
@group
(%i5) %e^(%pi*%i*180/144);
                           %i         1
(%o5)                  - ------- - -------
                         sqrt(2)   sqrt(2)
@end group
@group
(%i6) %e^(%pi*%i*120/144);
                          %i   sqrt(3)
(%o6)                     -- - -------
                          2       2
@end group
@group
(%i7) %e^(%pi*%i*121/144);
                            121 %i %pi
                            ----------
                               144
(%o7)                     %e
@end group
@end example

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%enumer}
@defvr {Option variable} %enumer
Default value: @code{false}

When @code{%enumer} is @code{true}, @code{%e} is replaced by its numeric value
2.718@dots{}  whenever @code{numer} is @code{true}.

When @code{%enumer} is @code{false}, this substitution is carried out
only if the exponent in @code{%e^x} evaluates to a number.

See also @mref{ev} and @mrefdot{numer}

@c ===beg===
@c %enumer;
@c numer;
@c 2*%e;
@c %enumer: not %enumer;
@c 2*%e;
@c numer: not numer;
@c 2*%e;    
@c 2*%e^1;  
@c 2*%e^x;  
@c ===end===
@example
@group
(%i1) %enumer;
(%o1)                         false
@end group
@group
(%i2) numer;
(%o2)                         false
@end group
@group
(%i3) 2*%e;
(%o3)                         2 %e
@end group
@group
(%i4) %enumer: not %enumer;
(%o4)                         true
@end group
@group
(%i5) 2*%e;
(%o5)                         2 %e
@end group
@group
(%i6) numer: not numer;
(%o6)                         true
@end group
@group
(%i7) 2*%e;
(%o7)                   5.43656365691809
@end group
@group
(%i8) 2*%e^1;
(%o8)                   5.43656365691809
@end group
@group
(%i9) 2*%e^x;
                                         x
(%o9)                 2 2.718281828459045
@end group
@end example

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Evaluation flags}
@closecatbox
@end defvr

@c PROBABLY MORE TO BE SAID HERE

@c -----------------------------------------------------------------------------
@anchor{exp}
@deffn {Function} exp (@var{x})

Represents the exponential function.  Instances of @code{exp (@var{x})} in input
are simplified to @code{%e^@var{x}}; @code{exp} does not appear in simplified
expressions.

@code{demoivre} if @code{true} causes @code{%e^(a + b %i)} to simplify to
@code{%e^(a (cos(b) + %i sin(b)))} if @code{b} is free of @code{%i}.
See @mrefdot{demoivre}

@code{%emode}, when @code{true}, causes @code{%e^(%pi %i x)} to be simplified.
See @mrefdot{%emode}

@code{%enumer}, when @code{true} causes @code{%e} to be replaced by
2.718@dots{} whenever @code{numer} is @code{true}.  See @mrefdot{%enumer}

@c ===beg===
@c demoivre;
@c %e^(a + b*%i);
@c demoivre: not demoivre;
@c %e^(a + b*%i);
@c ===end===
@example
@group
(%i1) demoivre;
(%o1)                         false
@end group
@group
(%i2) %e^(a + b*%i);
                             %i b + a
(%o2)                      %e
@end group
@group
(%i3) demoivre: not demoivre;
(%o3)                         true
@end group
@group
(%i4) %e^(a + b*%i);
                      a
(%o4)               %e  (%i sin(b) + cos(b))
@end group
@end example

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@closecatbox
@end deffn


@c -----------------------------------------------------------------------------
@anchor{li}
@deffn {Function} li [@var{s}] (@var{z})

Represents the polylogarithm function of order @var{s} and argument @var{z},
defined by the infinite series

m4_displaymath(
<<<{\rm Li}_s \left(z\right) = \sum_{k=1}^\infty {z^k \over k^s}>>>,
<<<
@example
                                 inf
                                 ====   k
                                 \     z
                        Li (z) =  >    --
                          s      /      s
                                 ====  k
                                 k = 1
@end example
>>>)


@code{li[1](z)} is
m4_mathdot(-\log(1 - z),-log(1-z))
@code{li[2]} and @code{li[3]} are the
dilogarithm and trilogarithm functions, respectively.

When the order is 1, the polylogarithm simplifies to @code{- log (1 - z)}, which
in turn simplifies to a numerical value if @var{z} is a real or complex floating
point number or the @code{numer} evaluation flag is present.

When the order is 2 or 3,
the polylogarithm simplifies to a numerical value
if @var{z} is a real floating point number
or the @code{numer} evaluation flag is present.

Examples:

@c ===beg===
@c assume (x > 0);
@c integrate ((log (1 - t)) / t, t, 0, x);
@c li[4](1);
@c li[5](1);
@c li[2](1/2);
@c li[2](%i);
@c li[2](1+%i);
@c li [2] (7);
@c li [2] (7), numer;
@c li [3] (7);
@c li [2] (7), numer;
@c L : makelist (i / 4.0, i, 0, 8);
@c map (lambda ([x], li [2] (x)), L);
@c map (lambda ([x], li [3] (x)), L);
@c ===end===
@example
@group
(%i1) assume (x > 0);
(%o1)                        [x > 0]
@end group
@group
(%i2) integrate ((log (1 - t)) / t, t, 0, x);
(%o2)                       - li (x)
                                2
@end group
@group
(%i3) li[4](1);
                                 4
                              %pi
(%o3)                         ----
                               90
@end group
@group
(%i4) li[5](1);
(%o4)                        zeta(5)
@end group
@group
(%i5) li[2](1/2);
                            2      2
                         %pi    log (2)
(%o5)                    ---- - -------
                          12       2
@end group
@group
(%i6) li[2](%i);
                                        2
                                     %pi
(%o6)                  %catalan %i - ----
                                      48
@end group
@group
(%i7) li[2](1+%i);
                                  2
               %i %pi log(2)   %pi
(%o7)          ------------- + ---- + %catalan %i
                     4          16
@end group
@group
(%i8) li [2] (7);
(%o8)                        li (7)
                               2
@end group
@group
(%i9) li [2] (7), numer;
(%o9)      1.2482731820994244 - 6.1132570288179915 %i
@end group
@group
(%i10) li [3] (7);
(%o10)                       li (7)
                               3
@end group
@group
(%i11) li [2] (7), numer;
(%o11)     1.2482731820994244 - 6.1132570288179915 %i
@end group
@group
(%i12) L : makelist (i / 4.0, i, 0, 8);
(%o12)  [0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0]
@end group
@group
(%i13) map (lambda ([x], li [2] (x)), L);
(%o13) [0.0, 0.2676526390827326, 0.5822405264650125, 
0.978469392930306, 1.6449340668482264, 
2.1901770114416452 - 0.7010261415046585 %i, 
2.3743952702724798 - 1.2738062049196004 %i, 
2.448686765338203 - 1.7580848482107874 %i, 
2.4674011002723395 - 2.177586090303602 %i]
@end group
@group
(%i14) map (lambda ([x], li [3] (x)), L);
(%o14) [0.0, 0.25846139579657335, 0.5372131936080402, 
0.8444258088622044, 1.2020569031595942, 
1.6428668813178295 - 0.07821473138972386 %i, 
2.0608775073202805 - 0.258241985293288 %i, 
2.433418898226189 - 0.49192601879440423 %i, 
2.762071906228924 - 0.7546938294602477 %i]
@end group
@end example

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{log}
@deffn {Function} log (@var{x})

Represents the natural (base @math{e}) logarithm of @var{x}.

Maxima does not have a built-in function for the base 10 logarithm or other 
bases. @code{log10(x) := log(x) / log(10)} is a useful definition.

Simplification and evaluation of logarithms is governed by several global flags:

@table @code
@item @code{logexpand}
causes @code{log(a^b)} to become @code{b*log(a)}. If it is 
set to @code{all}, @code{log(a*b)} will also simplify to @code{log(a)+log(b)}.
If it is set to @code{super}, then @code{log(a/b)} will also simplify to 
@code{log(a)-log(b)} for rational numbers @code{a/b}, @code{a#1}. 
(@code{log(1/b)}, for @code{b} integer, always simplifies.) If it is set to 
@code{false}, all of these simplifications will be turned off.

@item @code{logsimp}
if @code{false} then no simplification of @code{%e} to a power containing 
@code{log}'s is done.

@item @code{lognegint}
if @code{true} implements the rule @code{log(-n) -> log(n)+%i*%pi} for
@code{n} a positive integer.

@item @code{%e_to_numlog}
when @code{true}, @code{r} some rational number, and @code{x} some expression,
the expression @code{%e^(r*log(x))} will be simplified into @code{x^r}.  It
should be noted that the @code{radcan} command also does this transformation,
and more complicated transformations of this as well. The @code{logcontract} 
command "contracts" expressions containing @code{log}.
@end table

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{logabs}
@defvr {Option variable} logabs
Default value: @code{false}

When doing indefinite integration where logs are generated, e.g.
@code{integrate(1/x,x)}, the answer is given in terms of @code{log(abs(...))}
if @code{logabs} is @code{true}, but in terms of @code{log(...)} if
@code{logabs} is @code{false}.  For definite integration, the @code{logabs:true}
setting is used, because here "evaluation" of the indefinite integral at the
endpoints is often needed.

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Integral calculus}
@category{Global flags}
@closecatbox
@end defvr

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
@deffn {Function} logarc (@var{expr})

The function @code{logarc(@var{expr})} carries out the replacement of
inverse circular and hyperbolic functions with equivalent logarithmic
functions for an expression @var{expr} without setting the global
variable @code{logarc}.

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Simplification flags and variables}
@category{Simplification functions}
@closecatbox
@end deffn

@anchor{logarc}
@defvr  {Option variable} logarc

When the global variable @code{logarc} is @code{true},
inverse circular and hyperbolic functions are replaced by
equivalent logarithmic functions.
The default value of @code{logarc} is @code{false}.

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Simplification flags and variables}
@category{Simplification functions}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{logconcoeffp}
@defvr {Option variable} logconcoeffp
Default value: @code{false}

Controls which coefficients are
contracted when using @code{logcontract}.  It may be set to the name of a
predicate function of one argument.  E.g. if you like to generate
SQRTs, you can do @code{logconcoeffp:'logconfun$
logconfun(m):=featurep(m,integer) or ratnump(m)$} .  Then
@code{logcontract(1/2*log(x));} will give @code{log(sqrt(x))}.

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{logcontract}
@deffn {Function} logcontract (@var{expr})

Recursively scans the expression @var{expr}, transforming
subexpressions of the form @code{a1*log(b1) + a2*log(b2) + c} into
@code{log(ratsimp(b1^a1 * b2^a2)) + c}

@c ===beg===
@c 2*(a*log(x) + 2*a*log(y))$
@c logcontract(%);
@c ===end===
@example
(%i1) 2*(a*log(x) + 2*a*log(y))$
@group
(%i2) logcontract(%);
                                 2  4
(%o2)                     a log(x  y )
@end group
@end example

The declaration @code{declare(n,integer)} causes
@code{logcontract(2*a*n*log(x))} to simplify to @code{a*log(x^(2*n))}.  The
coefficients that "contract" in this manner are those such as the 2 and the
@code{n} here which satisfy @code{featurep(coeff,integer)}.  The user can
control which coefficients are contracted by setting the option
@code{logconcoeffp} to the name of a predicate function of one argument.
E.g. if you like to generate SQRTs, you can do @code{logconcoeffp:'logconfun$
logconfun(m):=featurep(m,integer) or ratnump(m)$} .  Then
@code{logcontract(1/2*log(x));} will give @code{log(sqrt(x))}.

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{logexpand}
@defvr {Option variable} logexpand
Default value: @code{true}

If @code{true}, that is the default value, causes @code{log(a^b)} to become
@code{b*log(a)}.  If it is set to @code{all}, @code{log(a*b)} will also simplify
to @code{log(a)+log(b)}.  If it is set to @code{super}, then @code{log(a/b)}
will also simplify to @code{log(a)-log(b)} for rational numbers @code{a/b},
@code{a#1}.  (@code{log(1/b)}, for integer @code{b}, always simplifies.) If it
is set to @code{false}, all of these simplifications will be turned off.

When @code{logexpand} is set to @code{all} or @code{super},
the logarithm of a product expression simplifies to a summation of logarithms.

Examples:

When @code{logexpand} is @code{true},
@code{log(a^b)} simplifies to @code{b*log(a)}.

@c ===beg===
@c log(n^2), logexpand=true;
@c ===end===
@example
@group
(%i1) log(n^2), logexpand=true;
(%o1)                       2 log(n)
@end group
@end example

When @code{logexpand} is @code{all},
@code{log(a*b)} simplifies to @code{log(a)+log(b)}.

@c ===beg===
@c log(10*x), logexpand=all;
@c ===end===
@example
@group
(%i1) log(10*x), logexpand=all;
(%o1)                   log(x) + log(10)
@end group
@end example

When @code{logexpand} is @code{super},
@code{log(a/b)} simplifies to @code{log(a)-log(b)}
for rational numbers @code{a/b} with @code{a#1}.

@c ===beg===
@c log(a/(n + 1)), logexpand=super;
@c ===end===
@example
@group
(%i1) log(a/(n + 1)), logexpand=super;
(%o1)                  log(a) - log(n + 1)
@end group
@end example

When @code{logexpand} is set to @code{all} or @code{super},
the logarithm of a product expression simplifies to a summation of logarithms.

@c ===beg===
@c my_product : product (X(i), i, 1, n);
@c log(my_product), logexpand=all;
@c log(my_product), logexpand=super;
@c ===end===
@example
@group
(%i1) my_product : product (X(i), i, 1, n);
                             n
                           _____
                           |   |
(%o1)                      |   | X(i)
                           |   |
                           i = 1
@end group
@group
(%i2) log(my_product), logexpand=all;
                          n
                         ____
                         \
(%o2)                     >    log(X(i))
                         /
                         ----
                         i = 1
@end group
@group
(%i3) log(my_product), logexpand=super;
                          n
                         ____
                         \
(%o3)                     >    log(X(i))
                         /
                         ----
                         i = 1
@end group
@end example

When @code{logexpand} is @code{false},
these simplifications are disabled.

@c ===beg===
@c logexpand : false $
@c log(n^2);
@c log(10*x);
@c log(a/(n + 1));
@c log ('product (X(i), i, 1, n));
@c ===end===
@example
(%i1) logexpand : false $
@group
(%i2) log(n^2);
                                  2
(%o2)                        log(n )
@end group
@group
(%i3) log(10*x);
(%o3)                       log(10 x)
@end group
@group
(%i4) log(a/(n + 1));
                                 a
(%o4)                      log(-----)
                               n + 1
@end group
@group
(%i5) log ('product (X(i), i, 1, n));
                               n
                             _____
                             |   |
(%o5)                    log(|   | X(i))
                             |   |
                             i = 1
@end group
@end example

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{lognegint}
@defvr {Option variable} lognegint
Default value: @code{false}

If @code{true} implements the rule
@code{log(-n) -> log(n)+%i*%pi} for @code{n} a positive integer.

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{logsimp}
@defvr {Option variable} logsimp
Default value: @code{true}

If @code{false} then no simplification of @code{%e} to a
power containing @code{log}'s is done.

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{plog}
@deffn {Function} plog (@var{x})

Represents the principal branch of the complex-valued natural
logarithm with @code{-%pi < carg(@var{x}) <= +%pi} .

@opencatbox{Categories:}
@category{Exponential and logarithm functions}
@category{Complex variables}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{sqrt}
@deffn {Function} sqrt (@var{x})

The square root of @var{x}.  It is represented internally by
@code{@var{x}^(1/2)}.  See also @mref{rootscontract} and @mrefdot{radexpand}

@opencatbox{Categories:}
@category{Mathematical functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@page
@node Trigonometric Functions, Random Numbers, Root Exponential and Logarithmic Functions, Elementary Functions
@section Trigonometric Functions
@c -----------------------------------------------------------------------------

@menu
* Introduction to Trigonometric::
* Functions and Variables for Trigonometric::
@end menu

@c -----------------------------------------------------------------------------
@node Introduction to Trigonometric, Functions and Variables for Trigonometric, Trigonometric Functions, Trigonometric Functions
@subsection Introduction to Trigonometric
@c -----------------------------------------------------------------------------

Maxima has many trigonometric functions defined.  Not all trigonometric
identities are programmed, but it is possible for the user to add many
of them using the pattern matching capabilities of the system.  The
trigonometric functions defined in Maxima are: @mref{acos},
@mref{acosh}, @mref{acot}, @mref{acoth}, @mref{acsc},
@mref{acsch}, @mref{asec}, @mref{asech}, @mref{asin},
@mref{asinh}, @mref{atan}, @mref{atanh}, @mref{cos},
@mref{cosh}, @mref{cot}, @mref{coth}, @mref{csc}, @mref{csch},
@mref{sec}, @mref{sech}, @mref{sin}, @mref{sinh}, @mref{tan},
and @mref{tanh}.  There are a number of commands especially for
handling trigonometric functions, see @mref{trigexpand},
@mref{trigreduce}, and the switch @mref{trigsign}.  Two share
packages extend the simplification rules built into Maxima,
@mref{ntrig} and @mref{atrig1}.  Do @code{describe(@var{command})}
for details.

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox

@c -----------------------------------------------------------------------------
@node Functions and Variables for Trigonometric,  , Introduction to Trigonometric, Trigonometric Functions
@subsection Functions and Variables for Trigonometric
@c -----------------------------------------------------------------------------

@menu
* Trigonometric and Hyperbolic Functions::
* Options Controlling Simplification::
* Explicit Simplifications Using Identities::
* Additional Functions::
@end menu

@node Trigonometric and Hyperbolic Functions, Options Controlling Simplification, Functions and Variables for Trigonometric, Functions and Variables for Trigonometric
@subsubsection Trigonometric and Hyperbolic Functions

@c Use this to make the same cross-references to variables that
@c control simplification of trig (and hyperbolic) functions.
@macro xreftrigvars{}
For variables that control simplification @pxref{%piargs},
@ref{%iargs}, @ref{halfangles}, @ref{triginverses}, and
@ref{trigsign}.

@end macro
@c -----------------------------------------------------------------------------
@anchor{acos}
@deffn {Function} acos (@var{x})

-- Arc Cosine.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{acosh}
@deffn {Function} acosh (@var{x})

-- Hyperbolic Arc Cosine.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{acot}
@deffn {Function} acot (@var{x})

-- Arc Cotangent.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{acoth}
@deffn {Function} acoth (@var{x})

-- Hyperbolic Arc Cotangent.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{acsc}
@deffn {Function} acsc (@var{x})

-- Arc Cosecant.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{acsch}
@deffn {Function} acsch (@var{x})

-- Hyperbolic Arc Cosecant.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{asec}
@deffn {Function} asec (@var{x})

-- Arc Secant.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{asech}
@deffn {Function} asech (@var{x})

-- Hyperbolic Arc Secant.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{asin}
@deffn {Function} asin (@var{x})

-- Arc Sine.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{asinh}
@deffn {Function} asinh (@var{x})

-- Hyperbolic Arc Sine.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{atan}
@deffn {Function} atan (@var{x})

-- Arc Tangent.

See also @mref{atan2}.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{atan2}
@deffn {Function} atan2 (@var{y}, @var{x})

-- yields the value of
m4_math(<<<\tan^{-1}(y/x)>>>, atan(y/x))
in the interval
m4_math(-\pi, -%pi)
to
m4_math(\pi, %pi)
taking into
consideration the quadrant of the point 
m4_mathdot(<<<(x,y)>>>, <<<(x,y)>>>)

Along the branch cut with @math{y = 0} and @math{x < 0}, @code{atan2}
is continuous with the second quadrant.
See also @mref{atan}.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{atanh}
@deffn {Function} atanh (@var{x})

-- Hyperbolic Arc Tangent.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{cos}
@deffn {Function} cos (@var{x})

-- Cosine.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{cosh}
@deffn {Function} cosh (@var{x})

-- Hyperbolic Cosine.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{cot}
@deffn {Function} cot (@var{x})

-- Cotangent.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{coth}
@deffn {Function} coth (@var{x})

-- Hyperbolic Cotangent.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{csc}
@deffn {Function} csc (@var{x})

-- Cosecant.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{csch}
@deffn {Function} csch (@var{x})

-- Hyperbolic Cosecant.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{sec}
@deffn {Function} sec (@var{x})

-- Secant.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{sech}
@deffn {Function} sech (@var{x})

-- Hyperbolic Secant.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{sin}
@deffn {Function} sin (@var{x})

-- Sine.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{sinh}
@deffn {Function} sinh (@var{x})

-- Hyperbolic Sine.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{tan}
@deffn {Function} tan (@var{x})

-- Tangent.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Trigonometric functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{tanh}
@deffn {Function} tanh (@var{x})

-- Hyperbolic Tangent.

@xreftrigvars{}

@opencatbox{Categories:}
@category{Hyperbolic functions}
@closecatbox
@end deffn

@unmacro xreftrigvars

@node Options Controlling Simplification, Explicit Simplifications Using Identities, Trigonometric and Hyperbolic Functions, Functions and Variables for Trigonometric
@subsubsection Options Controlling Simplification
@c -----------------------------------------------------------------------------
@anchor{%piargs}
@defvr {Option variable} %piargs
Default value: @code{true}

When @code{%piargs} is @code{true},
trigonometric functions are simplified to algebraic constants
when the argument is an integer multiple
of
m4_mathcomma(\pi,%pi)
m4_mathcomma(\pi/2,%pi/2)
m4_mathcomma(\pi/4,%pi/4)
or
m4_mathdot(\pi/6,%pi/6)
@c @iftex
@c @math{\pi}, @math{\pi/2}, @math{\pi/3}, @math{\pi/4}, or @math{\pi/6}.
@c @end iftex
@c @ifnottex
@c @math{%pi}, @math{%pi/2}, @math{%pi/3}, @math{%pi/4}, or @math{%pi/6}.
@c @end ifnottex

Maxima knows some identities which can be applied when
m4_mathcomma(\pi,%pi)
etc.,
@c @iftex
@c Maxima knows some identities which can be applied when @math{\pi}, etc.,
@c @end iftex
@c @ifnottex
@c Maxima knows some identities which can be applied when @math{%pi}, etc.,
@c @end ifnottex
are multiplied by an integer variable (that is, a symbol declared to be
integer).

Examples:

@c ===beg===
@c %piargs : false$
@c [sin (%pi), sin (%pi/2), sin (%pi/3)];
@c [sin (%pi/4), sin (%pi/5), sin (%pi/6)];
@c %piargs : true$
@c [sin (%pi), sin (%pi/2), sin (%pi/3)];
@c [sin (%pi/4), sin (%pi/5), sin (%pi/6)];
@c [cos (%pi/3), cos (10*%pi/3), tan (10*%pi/3),
@c        cos (sqrt(2)*%pi/3)];
@c ===end===
@example
(%i1) %piargs : false$
@group
(%i2) [sin (%pi), sin (%pi/2), sin (%pi/3)];
                                %pi       %pi
(%o2)            [sin(%pi), sin(---), sin(---)]
                                 2         3
@end group
@group
(%i3) [sin (%pi/4), sin (%pi/5), sin (%pi/6)];
                      %pi       %pi       %pi
(%o3)            [sin(---), sin(---), sin(---)]
                       4         5         6
@end group
(%i4) %piargs : true$
@group
(%i5) [sin (%pi), sin (%pi/2), sin (%pi/3)];
                                sqrt(3)
(%o5)                    [0, 1, -------]
                                   2
@end group
@group
(%i6) [sin (%pi/4), sin (%pi/5), sin (%pi/6)];
                         1         %pi   1
(%o6)                [-------, sin(---), -]
                      sqrt(2)       5    2
@end group
@group
(%i7) [cos (%pi/3), cos (10*%pi/3), tan (10*%pi/3),
       cos (sqrt(2)*%pi/3)];
                1    1               sqrt(2) %pi
(%o7)          [-, - -, sqrt(3), cos(-----------)]
                2    2                    3
@end group
@end example

Some identities are applied when 
m4_math(\pi,%pi)
and 
m4_math(\pi/2,%pi/2)
are
multiplied by an integer variable.
@c @iftex
@c Some identities are applied when @math{\pi} and @math{\pi/2} are multiplied by
@c an integer variable.
@c @end iftex
@c @ifnottex
@c Some identities are applied when @math{%pi} and @math{%pi/2} are multiplied by
@c an integer variable.
@c @end ifnottex

@c ===beg===
@c declare (n, integer, m, even)$
@c [sin (%pi * n), cos (%pi * m), sin (%pi/2 * m),
@c        cos (%pi/2 * m)];
@c ===end===
@example
(%i1) declare (n, integer, m, even)$
@group
(%i2) [sin (%pi * n), cos (%pi * m), sin (%pi/2 * m),
       cos (%pi/2 * m)];
                                      m/2
(%o2)                  [0, 1, 0, (- 1)   ]
@end group
@end example

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%iargs}
@defvr {Option variable} %iargs
Default value: @code{true}

When @code{%iargs} is @code{true},
trigonometric functions are simplified to hyperbolic functions
when the argument is apparently a multiple of the imaginary
unit 
m4_mathdot(i, %i)
@c @iftex
@c when the argument is apparently a multiple of the imaginary unit @math{i}.
@c @end iftex
@c @ifnottex
@c when the argument is apparently a multiple of the imaginary unit @math{%i}.
@c @end ifnottex

Even when the argument is demonstrably real, the simplification is applied;
Maxima considers only whether the argument is a literal multiple
of 
m4_mathdot(i,%i)
@c @iftex
@c Maxima considers only whether the argument is a literal multiple of @math{i}.
@c @end iftex
@c @ifnottex
@c Maxima considers only whether the argument is a literal multiple of @math{%i}.
@c @end ifnottex

Examples:

@c ===beg===
@c %iargs : false$
@c [sin (%i * x), cos (%i * x), tan (%i * x)];
@c %iargs : true$
@c [sin (%i * x), cos (%i * x), tan (%i * x)];
@c ===end===
@example
(%i1) %iargs : false$
@group
(%i2) [sin (%i * x), cos (%i * x), tan (%i * x)];
(%o2)           [sin(%i x), cos(%i x), tan(%i x)]
@end group
(%i3) %iargs : true$
@group
(%i4) [sin (%i * x), cos (%i * x), tan (%i * x)];
(%o4)           [%i sinh(x), cosh(x), %i tanh(x)]
@end group
@end example

Even when the argument is demonstrably real, the simplification is applied.

@c ===beg===
@c declare (x, imaginary)$
@c [featurep (x, imaginary), featurep (x, real)];
@c sin (%i * x);
@c ===end===
@example
(%i1) declare (x, imaginary)$
@group
(%i2) [featurep (x, imaginary), featurep (x, real)];
(%o2)                     [true, false]
@end group
@group
(%i3) sin (%i * x);
(%o3)                      %i sinh(x)
@end group
@end example

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Hyperbolic functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{halfangles}
@defvr {Option variable} halfangles
Default value: @code{false}

When @code{halfangles} is @code{true}, trigonometric functions of arguments 
@code{@var{expr}/2} are simplified to functions of @var{expr}.

For a real argument @math{x} in the interval
m4_mathcomma(<<<0 \le x < 2\pi>>>, 0 <= x < 2*%pi)
m4_math(<<<\sin{x\over 2}>>>, sin(x/2))
simplifies to a simple formula:
m4_displaymath(
{\sqrt{1-\cos x}\over\sqrt{2}},
@example
                         sqrt(1 - cos(x))
                         ----------------
                             sqrt(2)
@end example
)

A complicated factor is needed to make this formula correct for all complex 
arguments @math{z = x+iy}:
m4_displaymath(
(-1)^{\lfloor{x/(2\pi)}\rfloor}
\left[1-\rm{unit\_step}(-y)
\left(1+(-1)^{\lfloor{x/(2\pi)}\rfloor - \lceil{x/(2\pi)}\rceil}\right)\right]
,
<<<@verbatim
              x                       x                x
      floor(-----)            floor(-----) - ceiling(-----)
            2 %pi                   2 %pi            2 %pi
 (- 1)             (1 - ((- 1)                              + 1)
                                                          unit_step(- y))
@end verbatim
>>>)

Maxima knows this factor and similar factors for the functions @code{sin}, 
@code{cos}, @code{sinh}, and @code{cosh}.  For special values of the argument 
@math{z} these factors simplify accordingly.

Examples:

@c ===beg===
@c halfangles : false$
@c sin (x / 2);
@c halfangles : true$
@c sin (x / 2);
@c assume(x>0, x<2*%pi)$
@c sin(x / 2);
@c ===end===
@example
(%i1) halfangles : false$
@group
(%i2) sin (x / 2);
                                 x
(%o2)                        sin(-)
                                 2
@end group
(%i3) halfangles : true$
@group
(%i4) sin (x / 2);
                            x
                    floor(-----)
                          2 %pi
               (- 1)             sqrt(1 - cos(x))
(%o4)          ----------------------------------
                            sqrt(2)
@end group
(%i5) assume(x>0, x<2*%pi)$
@group
(%i6) sin(x / 2);
                        sqrt(1 - cos(x))
(%o6)                   ----------------
                            sqrt(2)
@end group
@end example

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{trigsign}
@defvr {Option variable} trigsign
Default value: @code{true}

When @code{trigsign} is @code{true}, it permits simplification of negative
arguments to trigonometric functions.  E.g., 
m4_math(\sin(-x),@code{sin(-x)}) 
will
become 
m4_math(-\sin x, @code{-sin(x)})
only if @code{trigsign} is @code{true}.

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr


@node Explicit Simplifications Using Identities, Additional Functions, Options Controlling Simplification, Functions and Variables for Trigonometric
@subsubsection Explicit Simplifications Using Identities
@c NEEDS CLARIFICATION AND EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{trigexpand}
@deffn {Function} trigexpand (@var{expr})

Expands trigonometric and hyperbolic functions of
sums of angles and of multiple angles occurring in @var{expr}.  For best
results, @var{expr} should be expanded.  To enhance user control of
simplification, this function expands only one level at a time,
expanding sums of angles or multiple angles.  To obtain full expansion
into sines and cosines immediately, set the switch @code{trigexpand: true}.

@code{trigexpand} is governed by the following global flags:

@table @asis
@item @mref{trigexpand}
If @code{true} causes expansion of all
expressions containing sin's and cos's occurring subsequently.
@item @mref{halfangles}
If @code{true} causes half-angles to be simplified
away.
@item @mref{trigexpandplus}
Controls the "sum" rule for @code{trigexpand},
expansion of sums (e.g. @code{sin(x + y)}) will take place only if
@code{trigexpandplus} is @code{true}.
@item @mref{trigexpandtimes}
Controls the "product" rule for @code{trigexpand},
expansion of products (e.g. @code{sin(2 x)}) will take place only if
@code{trigexpandtimes} is @code{true}.
@end table

Examples:

@c ===beg===
@c x+sin(3*x)/sin(x),trigexpand=true,expand;
@c trigexpand(sin(10*x+y));
@c ===end===
@example
@group
(%i1) x+sin(3*x)/sin(x),trigexpand=true,expand;
                         2           2
(%o1)               - sin (x) + 3 cos (x) + x
@end group
@group
(%i2) trigexpand(sin(10*x+y));
(%o2)          cos(10 x) sin(y) + sin(10 x) cos(y)
@end group
@end example

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{trigexpandplus}
@defvr {Option variable} trigexpandplus
Default value: @code{true}

@code{trigexpandplus} controls the "sum" rule for
@mref{trigexpand}.  Thus, when the @mref{trigexpand} command is used or the
@mref{trigexpand} switch set to @code{true}, expansion of sums
(e.g. @code{sin(x+y))} will take place only if @code{trigexpandplus} is
@code{true}.

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{trigexpandtimes}
@defvr {Option variable} trigexpandtimes
Default value: @code{true}

@code{trigexpandtimes} controls the "product" rule for @mref{trigexpand}.
Thus, when the @mref{trigexpand} command is used or the @mref{trigexpand}
switch set to @code{true}, expansion of products (e.g. @code{sin(2*x)})
will take place only if @code{trigexpandtimes} is @code{true}.

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{triginverses}
@defvr {Option variable} triginverses
Default value: @code{true}

@code{triginverses} controls the simplification of the
composition of trigonometric and hyperbolic functions with their inverse
functions.

If @code{all}, both e.g. @code{atan(tan(@var{x}))}
and @code{tan(atan(@var{x}))} simplify to @var{x}.

If @code{true}, the @code{@var{arcfun}(@var{fun}(@var{x}))}
simplification is turned off.

If @code{false}, both the
@code{@var{arcfun}(@var{fun}(@var{x}))} and
@code{@var{fun}(@var{arcfun}(@var{x}))}
simplifications are turned off.

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{trigreduce}
@deffn  {Function} trigreduce @
@fname{trigreduce} (@var{expr}, @var{x}) @
@fname{trigreduce} (@var{expr})

Combines products and powers of trigonometric
and hyperbolic sin's and cos's of @var{x} into those of multiples of @var{x}.
It also tries to eliminate these functions when they occur in
denominators.  If @var{x} is omitted then all variables in @var{expr} are used.

See also @mref{poissimp}.

@c ===beg===
@c trigreduce(-sin(x)^2+3*cos(x)^2+x);
@c ===end===
@example
@group
(%i1) trigreduce(-sin(x)^2+3*cos(x)^2+x);
               cos(2 x)      cos(2 x)   1        1
(%o1)          -------- + 3 (-------- + -) + x - -
                  2             2       2        2
@end group
@end example

@c 
@c     OBSOLETE
@c     The behavior was changed in order to avoid calling expand in the core
@c     simplifier (trigi.lisp rev 1.31)
@c     See http://www.math.utexas.edu/pipermail/maxima/2008/010919.html.
@c 
@c The trigonometric simplification routines will use declared
@c information in some simple cases.  Declarations about variables are
@c used as follows, e.g.
@c 
@c ---beg---
@c declare(j, integer, e, even, o, odd)$
@c sin(x + (e + 1/2)*%pi);
@c sin(x + (o + 1/2)*%pi);
@c ---end---
@c @example
@c (%i1) declare(j, integer, e, even, o, odd)$
@c (%i2) sin(x + (e + 1/2)*%pi);
@c (%o2)                        cos(x)
@c (%i3) sin(x + (o + 1/2)*%pi);
@c (%o3)                       - cos(x)
@c @end example

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{trigsimp}
@deffn {Function} trigsimp (@var{expr})


Employs the identities
m4_math(<<<\sin\left(x\right)^2 + \cos\left(x\right)^2 = 1>>>,
<<<sin(x)^2+cos(x)^2 = 1>>>)
and 
m4_math(<<<\cosh\left(x\right)^2 - \sinh\left(x\right)^2 = 1>>>,
<<<cosh(x)^2-sinh(x)^2 = 1>>>)
to
@c @iftex
@c @tex
@c $\sin\left(x\right)^2 + \cos\left(x\right)^2 = 1$
@c @end tex
@c and
@c @tex
@c $\cosh\left(x\right)^2 - \sinh\left(x\right)^2 = 1$
@c @end tex
simplify expressions containing @code{tan}, @code{sec},
etc., to @code{sin}, @code{cos}, @code{sinh}, @code{cosh}.
@c @end iftex
@c @ifnottex
@c Employs the identities @math{sin(x)^2 + cos(x)^2 = 1} and
@c @math{cosh(x)^2 - sinh(x)^2 = 1} to simplify expressions containing @code{tan},
@c @code{sec}, etc., to @code{sin}, @code{cos}, @code{sinh}, @code{cosh}.
@c @end ifnottex

@mref{trigreduce}, @mref{ratsimp}, and @mref{radcan} may be
able to further simplify the result.

@code{demo ("trgsmp.dem")} displays some examples of @code{trigsimp}.
@c MERGE EXAMPLES INTO THIS ITEM

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification functions}
@closecatbox
@end deffn

@c NEEDS CLARIFICATION

@c -----------------------------------------------------------------------------
@anchor{trigrat}
@deffn {Function} trigrat (@var{expr})

Gives a canonical simplified quasilinear form of a trigonometrical expression;
@var{expr} is a rational fraction of several @code{sin}, @code{cos} or
@code{tan}, the arguments of them are linear forms in some variables (or
kernels) and @code{%pi/@var{n}} (@var{n} integer) with integer coefficients.
The result is a simplified fraction with numerator and denominator linear in
@code{sin} and @code{cos}.  Thus @code{trigrat} linearize always when it is
possible.

@c ===beg===
@c trigrat(sin(3*a)/sin(a+%pi/3));
@c ===end===
@example
@group
(%i1) trigrat(sin(3*a)/sin(a+%pi/3));
(%o1)            sqrt(3) sin(2 a) + cos(2 a) - 1
@end group
@end example

The following example is taken from
Davenport, Siret, and Tournier, @i{Calcul Formel}, Masson (or in English,
Addison-Wesley), section 1.5.5, Morley theorem.

@c ===beg===
@c c : %pi/3 - a - b$
@c bc : sin(a)*sin(3*c)/sin(a+b);
@c ba : bc, c=a, a=c;
@c ac2 : ba^2 + bc^2 - 2*bc*ba*cos(b);
@c trigrat (ac2);
@c ===end===
@example
(%i1) c : %pi/3 - a - b$
@group
(%i2) bc : sin(a)*sin(3*c)/sin(a+b);
                                          %pi
                  sin(a) sin(3 (- b - a + ---))
                                           3
(%o2)             -----------------------------
                           sin(b + a)
@end group
@group
(%i3) ba : bc, c=a, a=c;
                                         %pi
                    sin(3 a) sin(b + a - ---)
                                          3
(%o3)               -------------------------
                                  %pi
                          sin(a - ---)
                                   3
@end group
@group
(%i4) ac2 : ba^2 + bc^2 - 2*bc*ba*cos(b);
         2         2         %pi
      sin (3 a) sin (b + a - ---)
                              3
(%o4) ---------------------------
                2     %pi
             sin (a - ---)
                       3
                                       %pi
 - (2 sin(a) sin(3 a) sin(3 (- b - a + ---)) cos(b)
                                        3
             %pi            %pi
 sin(b + a - ---))/(sin(a - ---) sin(b + a))
              3              3
      2       2              %pi
   sin (a) sin (3 (- b - a + ---))
                              3
 + -------------------------------
                2
             sin (b + a)
@end group
@group
(%i5) trigrat (ac2);
(%o5) - (sqrt(3) sin(4 b + 4 a) - cos(4 b + 4 a)
 - 2 sqrt(3) sin(4 b + 2 a) + 2 cos(4 b + 2 a)
 - 2 sqrt(3) sin(2 b + 4 a) + 2 cos(2 b + 4 a)
 + 4 sqrt(3) sin(2 b + 2 a) - 8 cos(2 b + 2 a) - 4 cos(2 b - 2 a)
 + sqrt(3) sin(4 b) - cos(4 b) - 2 sqrt(3) sin(2 b) + 10 cos(2 b)
 + sqrt(3) sin(4 a) - cos(4 a) - 2 sqrt(3) sin(2 a) + 10 cos(2 a)
 - 9)/4
@end group
@end example

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Simplification functions}
@closecatbox
@end deffn


@node Additional Functions,  , Explicit Simplifications Using Identities, Functions and Variables for Trigonometric
@subsubsection Additional Functions
@c IS THIS DESCRIPTION ACCURATE ??
@c LET'S BE EXPLICIT ABOUT EXACTLY WHAT ARE THE RULES IMPLEMENTED BY THIS PACKAGE

@c -----------------------------------------------------------------------------
@anchor{atrig1}
@defvr {Package} atrig1

The @code{atrig1} package contains several additional simplification rules
for inverse trigonometric functions.  Together with rules
already known to Maxima, the following angles are fully implemented:
@math{0},
m4_mathcomma(\pi/6,%pi/6)
m4_mathcomma(\pi/4,%pi/4)
m4_mathcomma(\pi/3,%pi/3)
and
m4_mathdot(\pi/2,%pi/2)
Corresponding angles in the other three quadrants are also available.
Do @code{load("atrig1");} to use them.

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Package atrig1}
@closecatbox
@end defvr

@c IS THIS DESCRIPTION ACCURATE ??
@c LET'S BE EXPLICIT ABOUT EXACTLY WHAT ARE THE RULES IMPLEMENTED BY THIS PACKAGE

@c -----------------------------------------------------------------------------
@anchor{ntrig}
@defvr {Package} ntrig

The @code{ntrig} package contains a set of simplification rules that are
used to simplify trigonometric function whose arguments are of the form
@code{@var{f}(@var{n} %pi/10)} where @var{f} is any of the functions
@code{sin}, @code{cos}, @code{tan}, @code{csc}, @code{sec} and @code{cot}.
@c NEED TO LOAD THIS PACKAGE ??

@opencatbox{Categories:}
@category{Trigonometric functions}
@category{Package ntrig}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@page
@node Random Numbers,  , Trigonometric Functions, Elementary Functions
@section Random Numbers
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{make_random_state}
@deffn  {Function} make_random_state @
@fname{make_random_state} (@var{n}) @
@fname{make_random_state} (@var{s}) @
@fname{make_random_state} (true) @
@fname{make_random_state} (false)

@c OMIT THIS FOR NOW. SEE COMMENT BELOW.
@c @defunx make_random_state (@var{a})

A random state object represents the state of the random number generator.
The state comprises 627 32-bit words.

@code{make_random_state (@var{n})} returns a new random state object
created from an integer seed value equal to @var{n} modulo 2^32.
@var{n} may be negative.

@c OMIT THIS FOR NOW. NOT SURE HOW THIS IS SUPPOSED TO WORK.
@c @code{make_random_state (@var{a})} returns a new random state object
@c created from an array @var{a}, which must be a Lisp array of 32 unsigned bytes.

@code{make_random_state (@var{s})} returns a copy of the random state @var{s}.

@code{make_random_state (true)} returns a new random state object,
using the current computer clock time as the seed.

@code{make_random_state (false)} returns a copy of the current state
of the random number generator.

@opencatbox{Categories:}
@category{Random numbers}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{set_random_state}
@deffn {Function} set_random_state (@var{s})

Copies @var{s} to the random number generator state.

@code{set_random_state} always returns @code{done}.

@opencatbox{Categories:}
@category{Random numbers}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{random}
@deffn {Function} random (@var{x})

Returns a pseudorandom number.  If @var{x} is an integer,
@code{random (@var{x})} returns an integer from 0 through @code{@var{x} - 1}
inclusive.  If @var{x} is a floating point number, @code{random (@var{x})}
returns a nonnegative floating point number less than @var{x}.  @code{random}
complains with an error if @var{x} is neither an integer nor a float, or if
@var{x} is not positive.

The functions @code{make_random_state} and @code{set_random_state}
maintain the state of the random number generator.

The Maxima random number generator is an implementation of the Mersenne twister
MT 19937.

Examples:

@c ===beg===
@c s1: make_random_state (654321)$
@c set_random_state (s1);
@c random (1000);
@c random (9573684);
@c random (2^75);
@c s2: make_random_state (false)$
@c random (1.0);
@c random (10.0);
@c random (100.0);
@c set_random_state (s2);
@c random (1.0);
@c random (10.0);
@c random (100.0);
@c ===end===
@example
(%i1) s1: make_random_state (654321)$
@group
(%i2) set_random_state (s1);
(%o2)                         done
@end group
@group
(%i3) random (1000);
(%o3)                          768
@end group
@group
(%i4) random (9573684);
(%o4)                        7657880
@end group
@group
(%i5) random (2^75);
(%o5)                11804491615036831636390
@end group
(%i6) s2: make_random_state (false)$
@group
(%i7) random (1.0);
(%o7)                  0.2310127244107132
@end group
@group
(%i8) random (10.0);
(%o8)                  4.3945536458708245
@end group
@group
(%i9) random (100.0);
(%o9)                   32.28666704056853
@end group
@group
(%i10) set_random_state (s2);
(%o10)                        done
@end group
@group
(%i11) random (1.0);
(%o11)                 0.2310127244107132
@end group
@group
(%i12) random (10.0);
(%o12)                 4.3945536458708245
@end group
@group
(%i13) random (100.0);
(%o13)                  32.28666704056853
@end group
@end example

@opencatbox{Categories:}
@category{Random numbers}
@category{Numerical methods}
@closecatbox
@end deffn

