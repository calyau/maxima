The package @code{ratpow} provides functions that find the exponents
of the denominator in a CRE polynomial. If the exponents in the denominator
are needed instead @mref{ratdenom} can be used to extract this denominator
first. Returned coefficients are in CRE form except for numbers.

In order to get a list of vars in a CRE polynomial @mref{showratvars} can be
used.

For information about CREs see also @mrefcomma{rat} @mref{ratdisrep} and
@mrefdot{showratvars}
@menu
* Functions and Variables for ratpow::
@end menu

@node Functions and Variables for ratpow, , Top, Top
@section Functions and Variables for ratpow



@anchor{ratp_hipow}
@deffn {Function} ratp_hipow (@var{expr}, @var{x})

Finds the highest power of the main variable in @code{ratnumer(expr)}

@c ===beg===
@c load("ratpow")$
@c ratp_hipow( x^(5/2) + x^2 , x);
@c ratp_hipow( x^(5/2) + x^2 , sqrt(x));
@c ===end===
@example
(%i1) load("ratpow")$
@group
(%i2) ratp_hipow( x^(5/2) + x^2 , x);
(%o2)                           2
@end group
@group
(%i3) ratp_hipow( x^(5/2) + x^2 , sqrt(x));
(%o3)                           5
@end group
@end example

@opencatbox
@category{Rational expressions}
@category{Package ratpow}
@closecatbox
@end deffn


@anchor{ratp_lopow}
@deffn {Function} ratp_lopow (@var{expr}, @var{x})

Finds the lowest power of the main variable in @code{ratnumer(expr)}

@c ===beg===
@c load("ratpow")$
@c ratp_lopow( x^5 + x^2 , x);
@c ===end===
@example
(%i1) load("ratpow")$
@group
(%i2) ratp_lopow( x^5 + x^2 , x);
(%o2)                           2
@end group
@end example

The following example will return 0 since @code{1} equals @code{x^0}:
@c ===beg===
@c load("ratpow")$
@c ratp_lopow( x^5 + x^2 + 1, x);
@c ===end===
@example
(%i1) load("ratpow")$
@group
(%i2) ratp_lopow( x^5 + x^2 + 1, x);
(%o2)                           0
@end group
@end example

The CRE form of the following equation contains @code{sqrt(x)} and
@code{x}. Since they are interpreted as independent variables
@code{ratp_lopow} returns @code{0} in this case:
@c ===beg===
@c load("ratpow")$
@c g:sqrt(x)^5 + sqrt(x)^2;
@c showratvars(g);
@c ratp_lopow( g, x);
@c ratp_lopow( g, sqrt(x));
@c ===end===
@example
(%i1) load("ratpow")$
@group
(%i2) g:sqrt(x)^5 + sqrt(x)^2;
                             5/2
(%o2)                       x    + x
@end group
@group
(%i3) showratvars(g);
                              1/2
(%o3)                       [x   , x]
@end group
@group
(%i4) ratp_lopow( g, x);
(%o4)                           0
@end group
@group
(%i5) ratp_lopow( g, sqrt(x));
(%o5)                           0
@end group
@end example


@opencatbox
@category{Rational expressions}
@category{Package ratpow}
@closecatbox
@end deffn


@anchor{ratp_coeffs}
@deffn {Function} ratp_coeffs (@var{expr}, @var{x})

Generates a list of powers and coefficients of the main variable
@code{ratnumer(expr)}.
@c ===beg===
@c load("ratpow")$
@c ratp_coeffs( 4*x^3 + x + sqrt(x), x);
@c ===end===
@example
(%i1) load("ratpow")$
@group
(%i2) ratp_coeffs( 4*x^3 + x + sqrt(x), x);
(%o2)/R/         [[3, 4], [1, 1], [0, sqrt(x)]]
@end group
@end example
@opencatbox
@category{Rational expressions}
@category{Package ratpow}
@closecatbox
@end deffn

@anchor{ratp_dense_coeffs}
@deffn {Function} ratp_dense_coeffs (@var{expr}, @var{x})

Generates a list of coefficients in @code{ratnumer(expr)}; returned
coefficients are in CRE form except for numbers.

@c ===beg===
@c load("ratpow")$
@c ratp_dense_coeffs( 4*x^3 + x + sqrt(x), x);
@c ===end===
@example
(%i1) load("ratpow")$
@group
(%i2) ratp_dense_coeffs( 4*x^3 + x + sqrt(x), x);
(%o2)/R/               [4, 0, 1, sqrt(x)]
@end group
@end example

@opencatbox
@category{Rational expressions}
@category{Package ratpow}
@closecatbox
@end deffn

