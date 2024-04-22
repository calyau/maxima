@c -*- mode: texinfo -*-
@c -----------------------------------------------------------------------------
@page
@node Constants, Lists, Strings, Data Types and Structures
@section Constants
@c -----------------------------------------------------------------------------

@menu
* Functions and Variables for Constants::
@end menu

@c -----------------------------------------------------------------------------
@node Functions and Variables for Constants,  , Constants, Constants
@subsection Functions and Variables for Constants
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{%catalan}
@defvr {Constant} %catalan
@vrindex catalan
@vrindex Catalan's Constant

@code{%catalan} represents Catalan's constant, @math{G}, defined by
m4_displaymath(
<<<G = \sum_{n=0}^\infty {(-1)^n\over (2n+1)^2}>>>,
<<<
@example
                               inf
                               ____         n
                               ╲       (- 1)
                    %catalan =  ⟩    ──────────
                               ╱              2
                               ‾‾‾‾  (2 n + 1)
                               n = 0

@end example
>>>)
(It is also sometimes denoted by @math{C}).

The numeric value of @code{%catalan} is approximately
0.915965594177219.  (See @urldlmf{25.11.E40}).
@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%e}
@defvr {Constant} %e
@vrindex e
@vrindex Euler's number
@vrindex Base of natural logarithm

@code{%e} represents the base of the natural logarithm, also known as Euler's
number.  The numeric value of @code{%e} is the double-precision floating-point
value 2.718281828459045d0.  (See @urlaands{eqn 4.1.16, 67}, @urlaands{4.1.17, 67}.)

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%i}
@vrindex i
@vrindex Imaginary unit
@defvr {Constant} %i

@code{%i} represents the imaginary unit, 
m4_mathdot(\sqrt{-1}, sqrt(- 1))

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{false}
@defvr {Constant} false

@code{false} represents the Boolean constant of the same name.
Maxima implements @code{false} by the value @code{NIL} in Lisp.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%gamma}
@vrindex Euler-Mascheroni constant
@defvr {Constant} %gamma

The Euler-Mascheroni constant, 0.5772156649015329.... It is defined by (@urlaands{eqn 6.1.3, 255} and @urldlmf{5.2.ii})
m4_displaymath(
<<<\gamma = \lim_{n \rightarrow \infty} \left(\sum_{k=1}^n {1\over k} - \log n\right)>>>,
<<<
@example
                                   n
                                  ====
                                  \     1
               %gamma = limit    ( >    - - log(n))
                        n -> inf  /     k
                                  ====
                                  k = 1
@end example
>>>)

@c DOUBTLESS THERE IS MORE TO SAY HERE.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{ind}
@vrindex Indeterminate
@defvr {Constant} ind

@code{ind} represents a bounded, indefinite result.

See also @mrefdot{limit}

Example:

@c ===beg===
@c limit (sin(1/x), x, 0);
@c ===end===
@example
(%i1) limit (sin(1/x), x, 0);
(%o1)                          ind
@end example

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{inf}
@vrindex Real infinity
@defvr {Constant} inf

@code{inf} represents real positive infinity.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{infinity}
@vrindex Complex infinity
@defvr {Constant}  infinity

@code{infinity} represents complex infinity.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr


@c -----------------------------------------------------------------------------
@anchor{least_negative_float}
@defvr {Constant} least_negative_float
The least negative floating-point number in Maxima.  That is, the
negative floating-point number closest to 0.  It is approximately
-4.94065e-324, when
@url{https://en.wikipedia.org/wiki/Subnormal_number,denormal} numbers
are supported.  Otherwise it is the same as
@mref{least_negative_normalized_float}.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{least_negative_normalized_float}
@defvr {Constant} least_negative_normalized_float
The least negative normalized floating-point number in Maxima.  That
is, the negative normalized floating-point number closest to 0.  It is
approximately -2.22507e-308.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{least_positive_float}
@defvr {Constant} least_positive_float
The least positive floating-point number in Maxima.  That is, the
positive floating-point number closest to 0.  It is approximately
4.94065e-324, when
@url{https://en.wikipedia.org/wiki/Subnormal_number,denormal} numbers
are supported.  Otherwise it is the same as
@mref{least_positive_normalized_float}.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@anchor{least_positive_normalized_float}
@defvr {Constant} least_positive_normalized_float
The least positive normalized floating-point number in Maxima.  That
is, the positive normalized floating-point number closest to 0.  It is
approximately 2.22507e-308.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{minf}
@vrindex Minus infinity
@vrindex Negative infinity
@defvr {Constant} minf

@code{minf} represents real minus (i.e., negative) infinity.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{most_negative_float}
@defvr {Constant} most_negative_float
The most negative floating-point number in Maxima.  It is
approximately -1.79769e+308.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{most_positive_float}
@defvr {Constant} most_positive_float
The most positive floating-point number in Maxima.  It is
approximately 1.797693e+308.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%phi}
@vrindex phi
@vrindex Golden mean
@defvr {Constant} %phi

@code{%phi} represents the so-called @i{golden mean}, 
m4_mathdot((1+\sqrt{5})/2, (1 + sqrt(5))/2)
The numeric value of @code{%phi} is the double-precision floating-point value
1.618033988749895d0.

@mref{fibtophi} expresses Fibonacci numbers @code{fib(n)} in terms of
@code{%phi}.

By default, Maxima does not know the algebraic properties of @code{%phi}.
After evaluating @code{tellrat(%phi^2 - %phi - 1)} and @code{algebraic: true},
@mref{ratsimp} can simplify some expressions containing @code{%phi}.

Examples:

@code{fibtophi} expresses Fibonacci numbers @code{fib(n)} in terms of @code{%phi}.

@c ===beg===
@c fibtophi (fib (n));
@c fib (n-1) + fib (n) - fib (n+1);
@c fibtophi (%);
@c ratsimp (%);
@c ===end===
@example
(%i1) fibtophi (fib (n));
                           n             n
                       %phi  - (1 - %phi)
(%o1)                  -------------------
                           2 %phi - 1
(%i2) fib (n-1) + fib (n) - fib (n+1);
(%o2)          - fib(n + 1) + fib(n) + fib(n - 1)
(%i3) fibtophi (%);
            n + 1             n + 1       n             n
        %phi      - (1 - %phi)        %phi  - (1 - %phi)
(%o3) - --------------------------- + -------------------
                2 %phi - 1                2 %phi - 1
                                          n - 1             n - 1
                                      %phi      - (1 - %phi)
                                    + ---------------------------
                                              2 %phi - 1
(%i4) ratsimp (%);
(%o4)                           0
@end example

By default, Maxima does not know the algebraic properties of @code{%phi}.
After evaluating @code{tellrat (%phi^2 - %phi - 1)} and @code{algebraic: true},
@code{ratsimp} can simplify some expressions containing @code{%phi}.

@c ===beg===
@c e : expand ((%phi^2 - %phi - 1) * (A + 1));
@c ratsimp (e);
@c tellrat (%phi^2 - %phi - 1);
@c algebraic : true;
@c ratsimp (e);
@c ===end===
@example
(%i1) e : expand ((%phi^2 - %phi - 1) * (A + 1));
                 2                      2
(%o1)        %phi  A - %phi A - A + %phi  - %phi - 1
(%i2) ratsimp (e);
                  2                     2
(%o2)        (%phi  - %phi - 1) A + %phi  - %phi - 1
(%i3) tellrat (%phi^2 - %phi - 1);
                            2
(%o3)                  [%phi  - %phi - 1]
(%i4) algebraic : true;
(%o4)                         true
(%i5) ratsimp (e);
(%o5)                           0
@end example

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{%pi}
@vrindex pi
@defvr {Constant} %pi

@code{%pi} represents the ratio of the perimeter of a circle to its diameter.
The numeric value of @code{%pi} is the double-precision floating-point value
3.141592653589793d0.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{true}
@defvr {Constant} true

@code{true} represents the Boolean constant of the same name.
Maxima implements @code{true} by the value @code{T} in Lisp.

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{und}
@vrindex Undefined
@defvr {Constant} und

@code{und} represents an undefined result.

See also @mrefdot{limit}

Example:

@c ===beg===
@c limit (x*sin(x), x, inf);
@c ===end===
@example
(%i1) limit (x*sin(x), x, inf);
(%o1)                          und
@end example

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{zeroa}
@defvr {Constant} zeroa

@code{zeroa} represents an infinitesimal above zero.  @code{zeroa} can be used
in expressions.  @code{limit} simplifies expressions which contain
infinitesimals.

See also @mref{zerob} and @mrefdot{limit}

Example:

@code{limit} simplifies expressions which contain infinitesimals:

@c ===beg===
@c limit(zeroa);
@c limit(zeroa+x);
@c ===end===
@example
(%i1) limit(zeroa);
(%o1)                           0
(%i2) limit(x+zeroa);
(%o2)                           x
@end example

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{zerob}
@defvr {Constant} zerob

@code{zerob} represents an infinitesimal below zero.  @code{zerob} can be used
in expressions.  @code{limit} simplifies expressions which contain
infinitesimals.

See also @mref{zeroa} and @mrefdot{limit}

@opencatbox{Categories:}
@category{Constants}
@closecatbox
@end defvr

