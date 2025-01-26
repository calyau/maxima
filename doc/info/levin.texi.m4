@menu
* Introduction to levin::
* Functions and Variables for levin::
* Examples for levin::
* References for levin::
@end menu


@node Introduction to levin, Functions and Variables for levin, Package levin, Package levin
@section Introduction to levin

Package levin provides functions that implement the Levin u-transformation
(@ref{levin-1973,,Levin 1973}).
This transformation may accelerate convergence
of the sum of infinite sequences using a small number of terms.

Methods to accelerate the convergence of infinite series do not usually return the exact sum.  There are two major sources of error: truncation error, due to
the finite number of terms used; and, rounding error, due to the finite
precision arithmetic used in the calculations.  Truncation
error dominates for a small number of terms (perhaps 5 to 10 terms).
Rounding error dominates as the number of terms increases (perhaps
above 10 to 20 terms for IEEE-754 double precision arithmetic).

This package has two functions that address these sources of error. 

Function @mref{levin_u_sum} uses rational arithmetic with a fixed number
of terms.  This eliminates rounding error.

Function @mref{bflevin_u_sum} uses bigfloat precision.
It uses error estimates to increase the number of terms and the bigfloat
arithmetic precision to meet an accuracy target. 

See @mref{Examples for levin} for examples.
See @mref{References for levin} for further information.

@opencatbox{Categories:}
@category{Share packages}
@category{Package levin}
@category{Sums and products}
@closecatbox


@node Functions and Variables for levin, Examples for levin, Introduction to levin, Package levin
@section Functions and Variables for levin


@anchor{levin_u_sum}
@deffn {Function} levin_u_sum @
@fname{levin_u_sum} (@var{a}, @var{n}, @var{n_0}, @var{nterms}, @var{mode}) @
@fname{levin_u_sum} (@var{a}, @var{n}, @var{n_0}, @var{nterms})

Estimate @code{sum(@var{a}(@var{n}), @var{n}, @var{n_0}, inf)} using at most @var{nterms}
terms using the Levin u-transform @pxref{levin-1973,,Levin 1973}.

The following values are recognized for the optional argument @var{mode}.
If @var{mode} is not supplied, it is assumed to be @code{levin_algebraic}.

@table @code

@item levin_algebraic
The calculation is performed in exact arithmetic. @code{levin_u_sum} returns the result.

@item levin_numeric
The calculation is performed in bigfloat arithmetic.
The return value is a list @code{[@var{result}, @var{variance}]}
where @var{result} is the result of the bigfloat calculation,
and @var{variance} is in units of @code{10^(-2*fpprec)}.

@end table

@code{load("levin")} loads this function.

See @mref{Examples for levin} for examples.

@opencatbox{Categories:}
@category{Package levin}
@category{Sums and products}
@category{Numerical methods}
@closecatbox

@end deffn


@anchor{bflevin_u_sum}
@deffn {Function} bflevin_u_sum (@var{a}, @var{n}, @var{n_0})

Estimate @code{sum(@var{a}(@var{n}), @var{n}, @var{n_0}, inf)} using the
Levin u-transform in bigfloat arithmetic.

@c In the precision absolute or relative?
@code{bflevin_u_sum} attempts to return the sum of the infinite series
with a precision given by the global variable @mref{fpprec} using bigfloat arithmetic.

See @mref{levin_options} for options to control this function.
@code{bflevin_u_sum} uses an adaptive algorithm to increase both the number of
terms and the bigfloat precision used for internal calculations
until the estimated error is acceptable.

@code{load("levin")} loads this function.

See @mref{Examples for levin} for examples.

@opencatbox{Categories:}
@category{Package levin}
@category{Sums and products}
@category{Numerical methods}
@closecatbox

@end deffn


@anchor{levin_options}
@deffn {Variable} levin_options

Function @mref{bflevin_u_sum} attempts to return the sum of an infinite series
with a precision given by the global variable @mref{fpprec} using bigfloat arithmetic.
@code{bflevin_u_sum} uses an adaptive algorithm to increase both the number of
terms used and the bigfloat precision used for internal calculations
until the estimated error is acceptable.

The undeclared array @code{levin_options} contains options for controlling @code{bflevin_u_sum}.
Note that the subscript values for @code{levin_options} are strings.

@table @code

@item levin_options["debug"]
When @code{true}, @code{bflevin_u_sum} generates additional output. Default: @code{false}

@item levin_options["min_terms"] 
Minimum number of terms used by @code{bflevin_u_sum}. Default: 5

@item levin_options["max_terms"]
Maximum number of terms used by @code{bflevin_u_sum}. Default: 640 (equal to 5*2^7)

@item levin_options["min_precision"]
Initial bigfloat precision for @code{bflevin_u_sum}. Default: 16

@item levin_options["max_precision"]
Maximum bigfloat precision for @code{bflevin_u_sum}. Default: 1000

@end table

@opencatbox{Categories:}
@category{Package levin}
@category{Sums and products}
@category{Numerical methods}
@closecatbox

@end deffn



@node Examples for levin, References for levin, Functions and Variables for levin, Package levin
@section Examples for levin

@subsection Example 1: The Basel problem

The Basel problem asks for the precise summation
of the reciprocals of the
squares of the natural numbers.  Leonhard Euler found, in 1734, 

m4_displaymath(
<<<{\sum_{n=1}^{\infty}{{1}\over{n^2}}}
 = {{\pi^2}\over{6}}
 = {1.6449340668482262...}>>>,
<<<@example
                inf
                ====          
                \      1    pi^2
                 >    --- = ---- = 1.6449340668482262...
                /     n^2     6
                ====
                n = 1
@end example>>>
)

The sum is evaluated using both: @mref{levin_u_sum} using modes levin_algebraic and levin_numeric; and  
@mref{bflevin_u_sum} with two values of bigfloat precision @mref{fpprec}.

@c ===beg===
@c (load("levin"), exact: %pi^2/6, done);
@c s: levin_u_sum(1/n^2, n, 1, 10);
@c float(s);
@c float(s - exact);
@c s: levin_u_sum(1/n^2, n, 1, 10, 'levin_numeric);
@c bfloat(s[1] - exact);
@c s: bflevin_u_sum(1/n^2, n, 1);
@c s - bfloat(exact);
@c /* Now increase fpprec and try the same example again. */
@c  fpprec: 32;
@c s: bflevin_u_sum(1/n^2, n, 1);
@c s - bfloat(exact);
@c ===end===
@example
@group
(%i1) (load("levin"), exact: %pi^2/6, done);
(%o1)                         done
@end group
@group
(%i2) s: levin_u_sum(1/n^2, n, 1, 10);
                           3899836039
(%o2)                      ----------
                           2370816000
@end group
@group
(%i3) float(s);
(%o3)                   1.644934081345832
@end group
@group
(%i4) float(s - exact);
(%o4)                 1.4497605782537448e-8
@end group
@group
(%i5) s: levin_u_sum(1/n^2, n, 1, 10, 'levin_numeric);
(%o5)     [1.644934081345756b0, 1.882785043290232b-12]
@end group
@group
(%i6) bfloat(s[1] - exact);
(%o6)                 1.449752928817105b-8
@end group
@group
(%i7) s: bflevin_u_sum(1/n^2, n, 1);
(%o7)                  1.644934066848226b0
@end group
@group
(%i8) s - bfloat(exact);
(%o8)                 2.775557561562891b-17
@end group
@group
(%i9) /* Now increase fpprec and try the same example again. */
 fpprec: 32;
(%o9)                          32
@end group
@group
(%i10) s: bflevin_u_sum(1/n^2, n, 1);
(%o10)         1.644934066848226436472415166646b0
@end group
@group
(%i11) s - bfloat(exact);
(%o11)       - 3.0814879110195773648895647081359b-33
@end group
@end example

Using 10 terms in the series we were able to extrapolate
to the exact sum with an error of approximately @math{10^{-8}}.
This would require
around @math{10^{8}} terms by direct summation.
In this case the two modes of @var{levin_u_sum} returned
results of similar accuracy.


The effect of the number of terms @var{nterms} on the accuracy of @var{levin_u_sum}
is shown in the following example.  The sum of the series and the approximation error
is evaluated for
increasing values of argument @var{nterms} for both values of the optional argument @var{mode}:
@var{levin_algebraic} and @var{levin_numeric}.  The numeric calcuations are performed
with bigfloat precision @var{fpprec} of 16.  Errors are calculated with @var{fpprec} equal to 64.

The results are reported in three columns:
@var{nterms}, the number of terms used;
@var{errora}, the difference between the algebraic result and the exact
sum; 
and @var{errorn}, the difference between numeric result and the exact sum.
For small values of @var{nterms} the two modes return similar results.
As @var{nterms} increases above 12, the error
for mode @var{levin_algebraic} continues to decrease.  However, for mode
@var{levin_numeric} the error increases with @var{nterms} to due to roundoff.
This behavior is commonly encountered, and can be addressed by increasing
@var{fpprec}.

@c ===beg===
@c (load("levin"), fpprec: 32, fpprintprec: 3, exact: %pi^2/6, done);
@c for nterms: 6 step 2 thru 24 do (
@c      sa: levin_u_sum(1/n^2, n, 1, nterms),
@c      sn: block([fpprec: 16], levin_u_sum(1/n^2, n, 1, nterms, 'levin_numeric)),
@c      errora: abs(bfloat(sa - exact)),
@c      errorn: abs(bfloat(sn[1] - exact)),
@c      print(nterms, "     ", errora, "   ", errorn));
@c ===end===
@example
@group
(%i1) (load("levin"), fpprec: 32, fpprintprec: 3, exact: %pi^2/6, done);
(%o1)                         done
@end group
@group
(%i2) for nterms: 6 step 2 thru 24 do (
     sa: levin_u_sum(1/n^2, n, 1, nterms),
     sn: block([fpprec: 16], levin_u_sum(1/n^2, n, 1, nterms, 'levin_numeric)),
     errora: abs(bfloat(sa - exact)),
     errorn: abs(bfloat(sn[1] - exact)),
     print(nterms, "     ", errora, "   ", errorn));
6       2.58b-4     2.58b-4 
8       3.51b-6     3.51b-6 
10       1.45b-8     1.45b-8 
12       2.44b-10     2.44b-10 
14       5.16b-12     3.78b-11 
16       3.54b-14     4.5b-10 
18       3.75b-16     3.21b-11 
20       1.36b-17     8.66b-9 
22       1.73b-19     4.48b-7 
24       4.45b-22     4.25b-6 
(%o2)                         done
@end group
@end example

@subsection Example 2: Divergent Taylor series for log(1+x) when x=2

The Levin u-transformation can find the anti-limit of certain
divergent series.
In this example we calculate the
anti-limit of the divergent Taylor series for log(1+x) when x=2.

m4_displaymath(
<<<{A_n} = {-\sum_{m=1}^{n}{{\left( -2 \right)}^{m}\over{m}}}>>>,
<<<@example
                n
                ====      m         
                \     (-2) 
        A  =  -  >    -----
         n      /       m
                ====
                m = 1
@end example>>>
)

with anti-limit equal to log(3) = 1.0986122886681098...

@c ===beg===
@c (load("levin"), done);
@c levin_u_sum(-(-2)^n/n, n, 1, 10);
@c s: float(%);
@c exact: log(3.0);
@c s - exact;
@c ===end===
@example
@group
(%i1) (load("levin"), done);
(%o1)                         done
@end group
@group
(%i2) levin_u_sum(-(-2)^n/n, n, 1, 10);
                           3641179708
(%o2)                      ----------
                           3314344635
@end group
@group
(%i3) s: float(%);
(%o3)                  1.0986122775370342
@end group
@group
(%i4) exact: log(3.0);
(%o4)                  1.0986122886681098
@end group
@group
(%i5) s - exact;
(%o5)                - 1.1131075616788166e-8
@end group
@end example

@node References for levin,  , Examples for levin, Package levin
@section References for levin

@itemize

@item @anchor{levin-1973}
  (Levin 1973) Levin, David.
  Development of Non-Linear Transformations for Improving Convergence
  of Sequences,
  International Journal of Computer Mathematics 3 (1973): 371–88.
  @url{https://doi.org/10.1080/00207167308803075,
  doi:10.1080/00207167308803075}

@item @anchor{smith-ford-1979}
  (Smith and Ford 1979)
  Smith, David A., and William F. Ford.
  Acceleration of Linear and Logarithmic Convergence,
  SIAM Journal on Numerical Analysis 16, no. 2 (1979): 223.
  @url{https://doi.org/10.1080/00207167308803075,
  doi:10.1137/0716017}

@item @anchor{smith-ford-1982}
  (Smith and Ford 1982)
  Smith, David A., and William F. Ford.
  Numerical Comparisons of Nonlinear Convergence Accelerators,
  Mathematics of Computation 38, no. 158 (1 May 1982): 481–481.
  @url{https://doi.org/10.1090/S0025-5718-1982-0645665-1,
  doi:10.1090/S0025-5718-1982-0645665-1}

@item @anchor{sidi-2003}
  (Sidi 2003) Sidi, Avram.
  Practical Extrapolation Methods: Theory and Applications,
  Cambridge University Press, 2003,
  @url{https://doi.org/10.1017/CBO9780511546815, ISBN 9780521661591}

@end itemize
