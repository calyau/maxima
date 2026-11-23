\input texinfo
@c -*- mode: texinfo -*-
@c Created by Claude to describe how Maxima computes elliptic_e and
@c elliptic_ec accurately for real m.
@c
@setfilename elliptic-e-accuracy.info
@settitle Accurate Computation of elliptic_e and elliptic_ec
@documentencoding UTF-8
@allowcodebreaks false

@c Definitions so that plain TeX accepts the same formulas as MathJax.
@tex
\gdef\lvert{\vert}
\gdef\rvert{\vert}
\gdef\mathrm#1{{\rm #1}}
\gdef\mathcal#1{{\cal #1}}
\gdef\frac#1#2{{#1\over #2}}
\gdef\tfrac#1#2{{\textstyle{#1\over #2}}}
\gdef\middle#1{#1}
@end tex

@c Table borders and room for wide tables in the HTML output, as in
@c inverse-jacobi-accuracy.texi.m4.
@html
<style>
table.multitable { display: block; overflow-x: auto; border-collapse: collapse; }
table.multitable th, table.multitable td { border: 1px solid #999; padding: 0.25em 0.75em; }
</style>
@end html


@ifnottex
@node Top
@top Accurate Computation of @code{elliptic_e} and @code{elliptic_ec}
@end ifnottex

@iftex
@titlepage
@title Accurate Computation of @code{elliptic_e} and @code{elliptic_ec}
@end titlepage
@end iftex

This document records the work on bug @uref{https://sourceforge.net/p/maxima/bugs/5887/, #5887}, accuracy problems in @code{elliptic_e} and @code{elliptic_ec} for real parameters. It explains what was wrong, what the changes to @code{src/ellipt.lisp} do, and the measurements behind them. It has three parts:

@enumerate
@item
@ref{p1,,Real @math{0 < m < 1}}.
@item
@ref{p2,,Real @math{m > 1}}, including the removal of the separate double-float functions (@ref{s2-10,,section 2.10}).
@item
@ref{p3,,Complex arguments}, complex
m4_math(<<<\phi>>>, <<<φ>>>)
or
m4_mathdot(<<<m>>>, <<<m>>>)
@end enumerate

The changes are in @code{src/ellipt.lisp} and @code{tests/rtest_elliptic.mac}. Measurements are from SBCL 2.2.9 against @uref{https://mpmath.org/, mpmath} references at the exact double or bigfloat inputs. The tests of parts 1 and 2 also pass on CMUCL, CCL, ECL and GCL. On CLISP, four tests are expected failures (@ref{s2-9,,section 2.9}). Part 3 was tested on SBCL and CLISP.

Errors are given in ulps of the exact value, with 53 bits for double floats and 109 bits for bigfloats at @code{fpprec : 32}. Where the real and imaginary parts of a complex value are measured separately, each is in ulps of that part. Where a complex value is measured as a whole, the error is in ulps of its larger part.

Write
m4_math(<<<E(\phi\,|\,m)>>>, <<<E(φ | m)>>>)
for @code{elliptic_e},
m4_math(<<<E(m)>>>, <<<E(m)>>>)
for @code{elliptic_ec},
m4_math(<<<F(\phi\,|\,m)>>>, <<<F(φ | m)>>>)
for @code{elliptic_f} and
m4_math(<<<K(m)>>>, <<<K(m)>>>)
for @code{elliptic_kc}.

@menu
* Real m Between 0 and 1::
* Real m Greater Than 1::
* Complex m::
* References::
@end menu

@node Real m Between 0 and 1
@chapter Real @math{0 < m < 1}
@anchor{p1}

@section Summary of the Change
@anchor{s1-1}

@itemize @bullet
@item
A new function, @code{bf-elliptic-e-0<m<1}, in the @code{bigfloat} package, computes
m4_math(<<<E(\phi\,|\,m)>>>, <<<E(φ | m)>>>)
for real
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
and real
m4_math(<<<\phi>>>, <<<φ>>>)
in
m4_math(<<<[-\pi/2, \pi/2]>>>, <<<[−π/2, π/2]>>>)
from @uref{https://dlmf.nist.gov/19.25.E10, DLMF 19.25.10}. All of its terms have the same sign, so it has no cancellation. Like the other functions in the @code{bigfloat} package, it works on double floats and bigfloats.
@item
@code{bf-elliptic-e} (bigfloat and complex double float) and @code{elliptic-e} (real double float) use it for real
m4_math(<<<\phi>>>, <<<φ>>>)
and real
m4_mathdot(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@item
@code{bf-elliptic-ec} and @code{elliptic-ec} use the
m4_math(<<<\phi = \pi/2>>>, <<<φ = π/2>>>)
limit of the same formula for real
m4_mathdot(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@item
New regression tests, and one existing @code{jacobi_epsilon} test tolerance is loosened (@ref{s1-8,,section 1.8}).
@end itemize

Near
m4_math(<<<m = 1>>>, <<<m = 1>>>)
and
m4_mathcomma(<<<\phi = \pi/2>>>, <<<φ = π/2>>>)
the worst error went from 52.8 to 1.8 ulps for double floats, and from 65.9 to 2.7 ulps for bigfloats (@ref{s1-6,,section 1.6}). Elsewhere in
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
the errors also went down. This part does not change values for
m4_mathcomma(<<<m \le 0>>>, <<<m ≤ 0>>>)
m4_math(<<<m \ge 1>>>, <<<m ≥ 1>>>)
or complex arguments, but @code{jacobi_epsilon} for
m4_math(<<<m > 1>>>, <<<m > 1>>>)
uses the changed code internally (@ref{s1-8,,section 1.8}). @ref{p2,,Part 2} covers
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)

@section The Problem
@anchor{s1-2}

Both the double-float and bigfloat code used

m4_displaymath(
<<<E(\phi\,|\,m) = s\,R_F(c^2, y, 1) - \frac{m}{3}\,s^3 R_D(c^2, y, 1)>>>,
<<<E(φ | m) = s R_F(c², y, 1) − m/3 s³ R_D(c², y, 1)>>>)

and

m4_displaymath(
<<<E(m) = R_F(0, 1-m, 1) - \frac{m}{3}\,R_D(0, 1-m, 1),>>>,
<<<E(m) = R_F(0, 1−m, 1) − m/3 R_D(0, 1−m, 1),>>>)

where

m4_displaymath(
<<<s = \sin\phi, \qquad c = \cos\phi, \qquad y = 1 - m s^2 = c^2 + (1-m)\,s^2 .>>>,
<<<s = sin φ, c = cos φ, y = 1 − m s² = c² + (1−m) s² .>>>)

These are @uref{https://dlmf.nist.gov/19.25.E9, DLMF 19.25.9}, scaled by
m4_math(<<<\sin^2\phi>>>, <<<sin²φ>>>)
using the homogeneity of
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_mathdot(<<<R_D>>>, <<<R_D>>>)

As
m4_math(<<<m \to 1>>>, <<<m → 1>>>)
and
m4_mathcomma(<<<\phi \to \pi/2>>>, <<<φ → π/2>>>)
both
m4_math(<<<c^2>>>, <<<c²>>>)
and
m4_math(<<<y>>>, <<<y>>>)
go to 0. Both terms then grow like
m4_mathcomma(<<<K(m) \approx \tfrac12\ln\bigl(16/(1-m)\bigr)>>>, <<<K(m) ≈ ½ ln(16/(1−m))>>>)
while
m4_math(<<<E>>>, <<<E>>>)
stays near 1. For example, at
m4_mathcomma(<<<\phi = 1.570796326794>>>, <<<φ = 1.570796326794>>>)
m4_mathcomma(<<<m = 0.9999999999999999>>>, <<<m = 0.9999999999999999>>>)
the two terms are
m4_math(<<<19.7546\ldots>>>, <<<19.7546…>>>)
and
m4_mathcomma(<<<18.7546\ldots>>>, <<<18.7546…>>>)
and
m4_mathdot(<<<E = 1.0000000000000010688\ldots>>>, <<<E = 1.0000000000000010688…>>>)
The rounding errors of
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
are multiplied by about
m4_mathcomma(<<<K(m)/E>>>, <<<K(m)/E>>>)
which is about 20 here.

Double-float examples, with reference values from mpmath at the exact double inputs, rounded to 17 significant digits:

@need 3370
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.30 0.28 0.27 0.15
@headitem
Expression
@tab
Before
@tab
Exact value
@tab
Error (ulps)
@item
@code{elliptic_e(@/1.570796326794, 0.9999999999999999)}
@tab
m4_math(<<<0.9999999999999929>>>, <<<0.9999999999999929>>>)
@tab
m4_math(<<<1.0000000000000011>>>, <<<1.0000000000000011>>>)
@tab
36.8
@item
@code{elliptic_e(@/1.5707963267948966, 0.99999999)}
@tab
m4_math(<<<1.0000000504831679>>>, <<<1.0000000504831679>>>)
@tab
m4_math(<<<1.0000000504831741>>>, <<<1.0000000504831741>>>)
@tab
28.0
@item
@code{elliptic_ec(@/0.999999999999)}
@tab
m4_math(<<<1.0000000000073452>>>, <<<1.0000000000073452>>>)
@tab
m4_math(<<<1.0000000000073507>>>, <<<1.0000000000073507>>>)
@tab
24.8
@item
@code{elliptic_e(@/1.5707963267948966, 0.9999999999999999)}
@tab
m4_math(<<<0.9999999999999964>>>, <<<0.9999999999999964>>>)
@tab
m4_math(<<<1.0000000000000011>>>, <<<1.0000000000000011>>>)
@tab
20.8
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-1-1}
@noindent
@strong{Table 1.1.} Double-float examples of the cancellation for
m4_math(<<<m>>>, <<<m>>>)
near 1 and
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_math(<<<\pi/2\colon>>>, <<<π/2:>>>)
the value before the change, the exact value from mpmath, and the error before the change.

@tex
\medskip
@end tex

Bigfloats have the same problem. At @code{fpprec : 32},

@example maxima
elliptic_ec(bfloat(1-10^-30));
    → 1.0000000000000000000000000000179b0
@end example

while the value at that input is
m4_mathdot(<<<1.00000000000000000000000000001771\ldots>>>, <<<1.00000000000000000000000000001771…>>>)

For
m4_math(<<<m < 0>>>, <<<m < 0>>>)
both terms of the old formula are positive, so there is no cancellation there.

@section A Form Without Cancellation
@anchor{s1-3}

@uref{https://dlmf.nist.gov/19.25.E10, DLMF 19.25.10}, with
m4_mathcomma(<<<k^2 = m>>>, <<<k² = m>>>)
m4_math(<<<k'^2 = 1 - m>>>, <<<k'² = 1 − m>>>)
and
m4_mathcomma(<<<C = \csc^2\phi>>>, <<<C = csc²φ>>>)
is

m4_displaymath(
<<<E(\phi\,|\,m) = (1-m)\,R_F(C-1, C-m, C) + \frac{m(1-m)}{3}\,R_D(C-1, C, C-m) + m\sqrt{\frac{C-1}{C(C-m)}} ,>>>,
<<<E(φ | m) = (1−m) R_F(C−1, C−m, C) + (m(1−m))/3 R_D(C−1, C, C−m) + m√((C−1)/(C(C−m))),>>>)

for
m4_math(<<<0 \le \phi \le \pi/2>>>, <<<0 ≤ φ ≤ π/2>>>)
and
m4_mathdot(<<<C > m>>>, <<<C > m>>>)
The second condition holds for all
m4_mathcomma(<<<m < 1>>>, <<<m < 1>>>)
since
m4_mathdot(<<<C \ge 1>>>, <<<C ≥ 1>>>)
Here

m4_displaymath(
<<<C - 1 = \frac{c^2}{s^2}, \qquad C = \frac{1}{s^2}, \qquad C - m = \frac{y}{s^2} .>>>,
<<<C − 1 = c²/s², C = 1/s², C − m = y/s² .>>>)

m4_math(<<<R_F>>>, <<<R_F>>>)
is homogeneous of degree
m4_math(<<<-1/2>>>, <<<−1/2>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
of degree
m4_mathcomma(<<<-3/2>>>, <<<−3/2>>>)
so
m4_math(<<<R_F(C-1, C-m, C) = s\,R_F(c^2, y, 1)>>>, <<<R_F(C−1, C−m, C) = s R_F(c², y, 1)>>>)
and
m4_mathdot(<<<R_D(C-1, C, C-m) = s^3 R_D(c^2, 1, y)>>>, <<<R_D(C−1, C, C−m) = s³ R_D(c², 1, y)>>>)
The square root is
m4_mathdot(<<<s\,c/\sqrt y>>>, <<<s c/√y>>>)
This gives

m4_displaymath(
<<<E(\phi\,|\,m) = (1-m)\,s\,R_F(c^2, y, 1) + \frac{m(1-m)}{3}\,s^3 R_D(c^2, 1, y) + \frac{m\,s\,c}{\sqrt y} .>>>,
<<<E(φ | m) = (1−m) s R_F(c², y, 1) + (m(1−m))/3 s³ R_D(c², 1, y) + (m s c)/√y .>>>)

For
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
and
m4_mathcomma(<<<\lvert\phi\rvert \le \pi/2>>>, <<<|φ| ≤ π/2>>>)
m4_mathcomma(<<<c \ge 0>>>, <<<c ≥ 0>>>)
so all three terms have the sign of
m4_math(<<<s>>>, <<<s>>>)
and nothing cancels. Computed as
m4_mathcomma(<<<c^2 + (1-m)s^2>>>, <<<c² + (1−m)s²>>>)
m4_math(<<<y>>>, <<<y>>>)
has no cancellation either. At the example above, the three terms are
m4_mathcomma(<<<2.2\times10^{-15}>>>, <<<2.2×10⁻¹⁵>>>)
m4_math(<<<0.99991>>>, <<<0.99991>>>)
and
m4_mathdot(<<<8.5\times10^{-5}>>>, <<<8.5×10⁻⁵>>>)

At
m4_mathcomma(<<<\phi = \pi/2>>>, <<<φ = π/2>>>)
m4_math(<<<s = 1>>>, <<<s = 1>>>)
and
m4_mathcomma(<<<c = 0>>>, <<<c = 0>>>)
so the last term vanishes and

m4_displaymath(
<<<E(m) = (1-m)\,R_F(0, 1-m, 1) + \frac{m(1-m)}{3}\,R_D(0, 1, 1-m) .>>>,
<<<E(m) = (1−m) R_F(0, 1−m, 1) + (m(1−m))/3 R_D(0, 1, 1−m) .>>>)

This is also the last line of @uref{https://dlmf.nist.gov/19.25.E1, DLMF 19.25.1},
m4_mathdot(<<<E(k) - k'^2 K(k) = \tfrac13 k^2 k'^2 R_D(0, 1, k'^2)>>>, <<<E(k) − k'² K(k) = ⅓ k² k'² R_D(0, 1, k'²)>>>)
Both forms were checked against mpmath's @code{ellipe} to 40 digits.

@section Arguments Just Past ±π/2
@anchor{s1-4}

The form of @ref{s1-3,,section 1.3} needs
m4_mathdot(<<<c \ge 0>>>, <<<c ≥ 0>>>)
For
m4_math(<<<c < 0>>>, <<<c < 0>>>)
it gives
m4_mathcomma(<<<E(\pi - \phi\,|\,m)>>>, <<<E(π − φ | m)>>>)
not
m4_mathcomma(<<<E(\phi\,|\,m)>>>, <<<E(φ | m)>>>)
and the two differ by about
m4_mathdot(<<<2\lvert c\rvert/\sqrt{1-m}>>>, <<<2|c|/√(1−m)>>>)

Reducing
m4_math(<<<\phi>>>, <<<φ>>>)
by multiples of
m4_math(<<<\pi>>>, <<<π>>>)
leaves
m4_math(<<<\phi>>>, <<<φ>>>)
in
m4_math(<<<[-\pi/2, \pi/2]>>>, <<<[−π/2, π/2]>>>)
only up to rounding. Inputs can also be just past
m4_math(<<<\pm\pi/2>>>, <<<±π/2>>>)
to begin with. For example, at @code{fpprec : 32}, @code{bfloat(%pi/2)} is just above
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
and its cosine is
m4_mathdot(<<<-1.50\times10^{-33}>>>, <<<−1.50×10⁻³³>>>)
A first version of this change, without the handling below, returned an error of
m4_math(<<<3.4\times10^{-18}>>>, <<<3.4×10⁻¹⁸>>>)
for @code{elliptic_e(bfloat(%pi/2), bfloat(1-2^-100))}.

So for
m4_mathcomma(<<<c < 0>>>, <<<c < 0>>>)
the code uses

m4_displaymath(
<<<E(\phi\,|\,m) = 2\,\mathrm{sgn}(\phi)\,E(m) - E\bigl(\mathrm{sgn}(\phi)\,\pi - \phi\,\big|\,m\bigr),>>>,
<<<E(φ | m) = 2 sgn(φ) E(m) − E(sgn(φ) π − φ | m),>>>)

which follows from
m4_math(<<<E(\phi + \pi\,|\,m) = E(\phi\,|\,m) + 2E(m)>>>, <<<E(φ + π | m) = E(φ | m) + 2E(m)>>>)
and
m4_mathdot(<<<E(-\phi\,|\,m) = -E(\phi\,|\,m)>>>, <<<E(−φ | m) = −E(φ | m)>>>)
The argument
m4_math(<<<\mathrm{sgn}(\phi)\,\pi - \phi>>>, <<<sgn(φ) π − φ>>>)
has the same sine as
m4_math(<<<\phi>>>, <<<φ>>>)
and cosine
m4_mathcomma(<<<-c > 0>>>, <<<−c > 0>>>)
so no new sine or cosine is needed. Near
m4_mathcomma(<<<\pm\pi/2>>>, <<<±π/2>>>)
both terms on the right are close to
m4_math(<<<E(m)>>>, <<<E(m)>>>)
and
m4_math(<<<E>>>, <<<E>>>)
is close to
m4_mathcomma(<<<E(m)>>>, <<<E(m)>>>)
so this has no cancellation either.

@section Implementation
@anchor{s1-5}

The new function @code{bf-elliptic-e-0<m<1}, in @code{src/ellipt.lisp}, takes
m4_mathcomma(<<<s>>>, <<<s>>>)
m4_math(<<<c>>>, <<<c>>>)
and
m4_mathcomma(<<<m>>>, <<<m>>>)
evaluates the form of @ref{s1-3,,section 1.3}, and uses the reflection of @ref{s1-4,,section 1.4} when
m4_mathdot(<<<c < 0>>>, <<<c < 0>>>)

@itemize @bullet
@item
@code{bf-elliptic-e} calls it from its local function for the reduced
m4_math(<<<\phi>>>, <<<φ>>>)
when
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
are real and
m4_mathdot(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@item
@code{elliptic-e}, the real double-float function, calls it after its special cases for
m4_math(<<<m = 0>>>, <<<m = 0>>>)
and
m4_mathdot(<<<m = 1>>>, <<<m = 1>>>)
@item
@code{bf-elliptic-ec} and @code{elliptic-ec} evaluate the complete form of @ref{s1-3,,section 1.3} for real
m4_mathdot(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@end itemize

The new form costs one more square root and division than the old one. It calls
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
once each, as before.

@section Accuracy
@anchor{s1-6}

Errors below are the worst errors in ulps. References are from mpmath at 80 digits, at the exact inputs. ``Before'' is the code without this change.

@need 2800
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.42 0.10 0.12 0.12 0.12 0.12
@headitem
Inputs
@tab
Points
@tab
Double before
@tab
Double after
@tab
Bigfloat before
@tab
Bigfloat after
@item
m4_mathcomma(<<<\phi = \pi/2 - d>>>, <<<φ = π/2 − d>>>)
m4_mathcomma(<<<m = 1 - e>>>, <<<m = 1 − e>>>)
m4_mathcomma(<<<d \in \lbrace 10^{-2}, 10^{-4}, 10^{-8}, 10^{-12}, 0\rbrace>>>, <<<d ∈ @{10⁻², 10⁻⁴, 10⁻⁸, 10⁻¹², 0@}>>>)
m4_math(<<<e \in \lbrace 10^{-4}, 10^{-8}, 10^{-12}, 1.1\times10^{-16}\rbrace>>>, <<<e ∈ @{10⁻⁴, 10⁻⁸, 10⁻¹², 1.1×10⁻¹⁶@}>>>)
@tab
20
@tab
52.8
@tab
1.81
@tab
65.9
@tab
2.69
@item
Random
m4_mathcomma(<<<\lvert\phi\rvert \le \pi/2>>>, <<<|φ| ≤ π/2>>>)
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@tab
150
@tab
8.28
@tab
2.87
@tab
5.07
@tab
3.58
@item
Random
m4_mathcomma(<<<\lvert\phi\rvert \le 50>>>, <<<|φ| ≤ 50>>>)
m4_math(<<<0.5 < m < 1>>>, <<<0.5 < m < 1>>>)
@tab
40
@tab
6.47
@tab
2.96
@tab
14.0
@tab
2.71
@item
@code{elliptic_ec},
m4_mathcomma(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
including
m4_math(<<<1 - 10^{-k}>>>, <<<1 − 10⁻ᵏ>>>)
and
m4_math(<<<1 - 2^{-53}>>>, <<<1 − 2⁻⁵³>>>)
@tab
68
@tab
24.8
@tab
2.50
@tab
47.5
@tab
2.58
@item
Real
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
m4_math(<<<m \le 0>>>, <<<m ≤ 0>>>)
or
m4_math(<<<m \ge 1>>>, <<<m ≥ 1>>>)
@tab
40
@tab
6.98
@tab
6.98
@tab
12.0
@tab
12.0
@item
Complex
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
@tab
20
@tab
8.33
@tab
8.33
@tab
17.2
@tab
17.2
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-1-2}
@noindent
@strong{Table 1.2.} Worst errors in ulps for real
m4_mathcomma(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
and for other arguments, before and after the change of part 1.

@tex
\medskip
@end tex

This part leaves the last two rows bit-for-bit unchanged.

The double-float examples of @ref{s1-2,,section 1.2} after the change:

@need 2610
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.50 0.32 0.18
@headitem
Expression
@tab
After
@tab
Error (ulps)
@item
@code{elliptic_e(@/1.570796326794, 0.9999999999999999)}
@tab
m4_math(<<<1.0000000000000013>>>, <<<1.0000000000000013>>>)
@tab
1.19
@item
@code{elliptic_e(@/1.5707963267948966, 0.99999999)}
@tab
m4_math(<<<1.0000000504831739>>>, <<<1.0000000504831739>>>)
@tab
1.00
@item
@code{elliptic_ec(@/0.999999999999)}
@tab
m4_math(<<<1.0000000000073508>>>, <<<1.0000000000073508>>>)
@tab
0.19
@item
@code{elliptic_e(@/1.5707963267948966, 0.9999999999999999)}
@tab
m4_math(<<<1.0000000000000007>>>, <<<1.0000000000000007>>>)
@tab
1.81
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-1-3}
@noindent
@strong{Table 1.3.} The examples of Table 1.1 after the change.

@tex
\medskip
@end tex

@section Tests
@anchor{s1-7}

The new tests are at the end of @code{tests/rtest_elliptic.mac}, one value per test, for double floats and for bigfloats at @code{fpprec : 32} (@ref{s2-9,,section 2.9} describes the layout). References are from mpmath at 80 digits, at the exact inputs.

@itemize @bullet
@item
All inputs are dyadic rationals, written as @code{float(...)} or @code{bfloat(...)} of exact expressions, so the float and bigfloat arguments are exact.
@item
Each tolerance was first chosen as the absolute error observed in testing, rounded up to five significant digits, or
m4_math(<<<2^{-53}>>>, <<<2⁻⁵³>>>)
or
m4_math(<<<2^{-108}>>>, <<<2⁻¹⁰⁸>>>)
where the error was 0. The tests check relative errors, so each tolerance is that absolute error divided by the magnitude of the reference, rounded up to five significant digits, and written as a multiple of
m4_math(<<<2^{-53}>>>, <<<2⁻⁵³>>>)
for double floats or
m4_math(<<<2^{-109}>>>, <<<2⁻¹⁰⁹>>>)
for bigfloats.
@end itemize

Double-float tests:

@need 5650
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.33 0.38 0.29
@headitem
Expression
@tab
What it tests
@tab
Exact value
@item
@code{elliptic_e(@/float(%pi/2), float(1-2^-53))}
@tab
Complete case through @code{elliptic_e}
@tab
m4_math(<<<1.0000000000000011>>>, <<<1.0000000000000011>>>)
@item
@code{elliptic_e(@/float(%pi/2)-@/2^-40, float(1-2^-53))}
@tab
Just below
m4_math(<<<\pi/2>>>, <<<π/2>>>)
@tab
m4_math(<<<1.0000000000000011>>>, <<<1.0000000000000011>>>)
@item
@code{elliptic_e(@/float(%pi/2), float(1-2^-27))}
@tab
Moderate
m4_math(<<<1-m>>>, <<<1−m>>>)
@tab
m4_math(<<<1.0000000381610592>>>, <<<1.0000000381610592>>>)
@item
@code{elliptic_e(@/2^-20-float(%pi/2), float(1-2^-40))}
@tab
Negative
m4_math(<<<\phi>>>, <<<φ>>>)
@tab
m4_math(<<<-1.0000000000056633>>>, <<<−1.0000000000056633>>>)
@item
@code{elliptic_e(@/10.0, float(1-2^-40))}
@tab
Reduction by
m4_math(<<<\pi>>>, <<<π>>>)
@tab
m4_math(<<<6.5440211109296428>>>, <<<6.5440211109296428>>>)
@item
@code{elliptic_e(@/float(%pi/2)+@/2^-52, float(1-2^-53))}
@tab
Just past
m4_math(<<<\pi/2>>>, <<<π/2>>>)
@tab
m4_math(<<<1.0000000000000011>>>, <<<1.0000000000000011>>>)
@item
@code{elliptic_e(@/-float(%pi/2)-@/2^-52, float(1-2^-53))}
@tab
Just past
m4_math(<<<-\pi/2>>>, <<<−π/2>>>)
@tab
m4_math(<<<-1.0000000000000011>>>, <<<−1.0000000000000011>>>)
@item
@code{elliptic_ec(@/float(1-2^-53))}
@tab
@code{elliptic_ec}
@tab
m4_math(<<<1.0000000000000011>>>, <<<1.0000000000000011>>>)
@item
@code{elliptic_ec(@/float(1-2^-40))}
@tab
@code{elliptic_ec}
@tab
m4_math(<<<1.0000000000067072>>>, <<<1.0000000000067072>>>)
@item
@code{elliptic_ec(@/float(1-2^-27))}
@tab
@code{elliptic_ec}
@tab
m4_math(<<<1.0000000381610592>>>, <<<1.0000000381610592>>>)
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-1-4}
@noindent
@strong{Table 1.4.} Double-float tests added for real
m4_mathcomma(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
with exact values from mpmath.

@tex
\medskip
@end tex

Bigfloat tests:

@need 5270
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.29 0.19 0.52
@headitem
Expression
@tab
What it tests
@tab
Exact value
@item
@code{elliptic_e(@/bfloat(%pi/2), bfloat(1-2^-100))}
@tab
m4_math(<<<\cos\phi < 0>>>, <<<cos φ < 0>>>)
@tab
m4_math(<<<1.000000000000000000000000000014019>>>, <<<1.000000000000000000000000000014019>>>)
@item
@code{elliptic_e(@/bfloat(%pi/2)-@/2^-60, bfloat(1-2^-100))}
@tab
Just below
m4_math(<<<\pi/2>>>, <<<π/2>>>)
@tab
m4_math(<<<1.000000000000000000000000000014019>>>, <<<1.000000000000000000000000000014019>>>)
@item
@code{elliptic_e(@/bfloat(%pi/2), bfloat(1-2^-50))}
@tab
Moderate
m4_math(<<<1-m>>>, <<<1−m>>>)
@tab
m4_math(<<<1.000000000000008089073355640920771>>>, <<<1.000000000000008089073355640920771>>>)
@item
@code{elliptic_e(@/2^-30-bfloat(%pi/2), bfloat(1-2^-80))}
@tab
Negative
m4_math(<<<\phi>>>, <<<φ>>>)
@tab
m4_math(<<<-0.9999999999999999995663276044630384>>>, <<<−0.9999999999999999995663276044630384>>>)
@item
@code{elliptic_e(@/10b0, bfloat(1-2^-80))}
@tab
Reduction by
m4_math(<<<\pi>>>, <<<π>>>)
@tab
m4_math(<<<6.544021110889369813404818691403004>>>, <<<6.544021110889369813404818691403004>>>)
@item
@code{elliptic_e(@/-bfloat(%pi/2), bfloat(1-2^-100))}
@tab
m4_math(<<<-\pi/2>>>, <<<−π/2>>>)
with
m4_math(<<<\cos\phi < 0>>>, <<<cos φ < 0>>>)
@tab
m4_math(<<<-1.000000000000000000000000000014019>>>, <<<−1.000000000000000000000000000014019>>>)
@item
@code{elliptic_ec(@/bfloat(1-2^-100))}
@tab
@code{elliptic_ec}
@tab
m4_math(<<<1.000000000000000000000000000014019>>>, <<<1.000000000000000000000000000014019>>>)
@item
@code{elliptic_ec(@/bfloat(1-2^-50))}
@tab
@code{elliptic_ec}
@tab
m4_math(<<<1.000000000000008089073355640920771>>>, <<<1.000000000000008089073355640920771>>>)
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-1-5}
@noindent
@strong{Table 1.5.} Bigfloat tests added for real
m4_mathcomma(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
at @code{fpprec : 32}, with exact values from mpmath.

@tex
\medskip
@end tex

Without the source change, all 18 new tests fail, with errors 2.5 to 165 times the tolerance.

One existing @code{jacobi_epsilon} test gets a looser tolerance (@ref{s1-8,,section 1.8}).

With this part alone, all of @code{rtest_elliptic.mac} passed.

@section Effect on @code{jacobi_epsilon}
@anchor{s1-8}

@code{jacobi_epsilon} computes Jacobi's epsilon function
m4_math(<<<\mathcal{E}(u\,|\,m)>>>, <<<ℰ(u | m)>>>)
(@uref{https://dlmf.nist.gov/22.16.ii, DLMF §22.16(ii)}), and it does so with @code{bf-elliptic-e} and @code{bf-elliptic-ec}, so it inherits their accuracy:

@itemize @bullet
@item
For real
m4_math(<<<u>>>, <<<u>>>)
and real
m4_mathcomma(<<<m < 1>>>, <<<m < 1>>>)
@code{jacobi-epsilon} reduces
m4_math(<<<u>>>, <<<u>>>)
to
m4_mathcomma(<<<[0, 2K(m))>>>, <<<[0, 2K(m))>>>)
using
m4_mathdot(<<<\mathcal{E}(u + 2K(m)\,|\,m) = \mathcal{E}(u\,|\,m) + 2E(m)>>>, <<<ℰ(u + 2K(m) | m) = ℰ(u | m) + 2E(m)>>>)
It then computes
m4_math(<<<\mathcal{E}(u\,|\,m) = E(\mathrm{am}(u\,|\,m)\,|\,m)>>>, <<<ℰ(u | m) = E(am(u | m) | m)>>>)
with @code{bf-elliptic-e}, where
m4_math(<<<\mathrm{am}(u\,|\,m)>>>, <<<am(u | m)>>>)
is the two-argument arctangent of
m4_math(<<<\mathrm{sn}(u\,|\,m)>>>, <<<sn(u | m)>>>)
and
m4_mathdot(<<<\mathrm{cn}(u\,|\,m)>>>, <<<cn(u | m)>>>)
When the reduced
m4_math(<<<u>>>, <<<u>>>)
is at least
m4_mathcomma(<<<K(m)>>>, <<<K(m)>>>)
it does this for
m4_math(<<<u - K(m)>>>, <<<u − K(m)>>>)
and adds
m4_math(<<<E(m)>>>, <<<E(m)>>>)
and a term in sn, cn and dn.
@item
For real
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
it first uses the reciprocal-modulus transformation,
m4_mathcomma(<<<\mathcal{E}(u\,|\,m) = \sqrt m\,\mathcal{E}(\sqrt m\,u\,|\,1/m) - (m-1)\,u>>>, <<<ℰ(u | m) = √m ℰ(√m u | 1/m) − (m−1) u>>>)
so @code{bf-elliptic-e} and @code{bf-elliptic-ec} are called with
m4_mathcomma(<<<1/m>>>, <<<1/m>>>)
which is in
m4_mathdot(<<<(0, 1)>>>, <<<(0, 1)>>>)
@end itemize

So this part changes @code{jacobi_epsilon} for real
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
and, through
m4_mathcomma(<<<1/m>>>, <<<1/m>>>)
for real
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)

The double-float test of @code{jacobi_epsilon(1.0, 2.0)} got worse, even though @code{bf-elliptic-e} got more accurate at the point it uses. With
m4_math(<<<\sqrt m>>>, <<<√m>>>)
computed as @code{sqrt(2.0)}, @code{jacobi-epsilon} computes
m4_math(<<<\mathcal{E}(\sqrt2\,|\,\tfrac12)>>>, <<<ℰ(√2 | ½)>>>)
as
m4_mathcomma(<<<E(\mathrm{am}\,|\,\tfrac12)>>>, <<<E(am | ½)>>>)
with the double float
m4_mathdot(<<<\mathrm{am} = 1.254827029042835>>>, <<<am = 1.254827029042835>>>)
Errors in ulps, against mpmath at the exact inputs, with ``before'' the code without this change:

@need 2040
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.56 0.22 0.22
@headitem
Quantity
@tab
Before
@tab
After
@item
m4_math(<<<\mathrm{am}>>>, <<<am>>>)
at
m4_math(<<<u>>>, <<<u>>>)
= @code{sqrt(2.0)} and
m4_math(<<<m = \tfrac12>>>, <<<m = ½>>>)
@tab
1.35
@tab
1.35
@item
@code{bf-elliptic-e} at the double-float
m4_math(<<<\mathrm{am}>>>, <<<am>>>)
and
m4_math(<<<m = \tfrac12>>>, <<<m = ½>>>)
@tab
m4_math(<<<-1.19>>>, <<<−1.19>>>)
@tab
0.81
@item
@code{jacobi_epsilon(sqrt(2.0), 0.5)}
@tab
m4_math(<<<-0.19>>>, <<<−0.19>>>)
@tab
1.81
@item
@code{jacobi_epsilon(1.0, 2.0)}
@tab
0.89
@tab
6.89
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-1-6}
@noindent
@strong{Table 1.6.} Errors in ulps of the quantities that make up @code{jacobi_epsilon(1.0, 2.0)}, before and after the change.

@tex
\medskip
@end tex

@itemize @bullet
@item
The error of
m4_mathcomma(<<<\mathrm{am}>>>, <<<am>>>)
1.35 ulps, changes
m4_math(<<<E>>>, <<<E>>>)
by 1.00 ulp, before and after the change.
@item
Before the change, the error of @code{bf-elliptic-e},
m4_math(<<<-1.19>>>, <<<−1.19>>>)
ulps, nearly cancelled that, leaving
m4_math(<<<-0.19>>>, <<<−0.19>>>)
ulps. After the change, its error is smaller, 0.81 ulps, but has the same sign, so the two add to 1.81 ulps.
@item
The subtraction of
m4_math(<<<(m-1)\,u>>>, <<<(m−1) u>>>)
multiplies the relative error by
m4_mathdot(<<<\sqrt2\,E/(\sqrt2\,E - 1) \approx 1.589/0.589 \approx 2.70>>>, <<<√2 E/(√2 E − 1) ≈ 1.589/0.589 ≈ 2.70>>>)
That turns 1.81 ulps of
m4_math(<<<\mathcal{E}(\sqrt2\,|\,\tfrac12)>>>, <<<ℰ(√2 | ½)>>>)
into about 5.1 ulps of the result. The roundings of @code{sqrt(2.0)} and of the multiplication and subtraction account for the rest.
@end itemize

So the double-float tolerance of that test, an absolute error, goes from @code{1.3080e-16} to @code{7.7716e-16}. The bigfloat half of the test is unchanged.

Over other
m4_mathcomma(<<<u>>>, <<<u>>>)
this part does not make @code{jacobi_epsilon} worse. Errors in ulps for
m4_mathcomma(<<<u = 0.01, 0.02, \ldots, 3>>>, <<<u = 0.01, 0.02, …, 3>>>)
against Maxima's bigfloat @code{jacobi_epsilon} at @code{fpprec : 60}:

@need 1660
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.40 0.15 0.15 0.15 0.15
@headitem
Expression
@tab
Worst before
@tab
Worst after
@tab
Mean before
@tab
Mean after
@item
@code{jacobi_epsilon(u, 2.0)}
@tab
15.3
@tab
13.4
@tab
3.50
@tab
3.24
@item
@code{jacobi_epsilon(u, 0.5)}
@tab
5.56
@tab
4.24
@tab
1.05
@tab
0.88
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-1-7}
@noindent
@strong{Table 1.7.} Worst and mean errors in ulps of @code{jacobi_epsilon} for
m4_mathcomma(<<<u = 0.01, 0.02, \ldots, 3>>>, <<<u = 0.01, 0.02, …, 3>>>)
before and after the change.

@tex
\medskip
@end tex

The old tolerance was luck at
m4_mathdot(<<<u = 1>>>, <<<u = 1>>>)
For
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
the reciprocal-modulus transformation amplifies errors both before and after the change, because the subtraction of
m4_math(<<<(m-1)\,u>>>, <<<(m−1) u>>>)
cancels. That is not part of @uref{https://sourceforge.net/p/maxima/bugs/5887/, #5887}.

@node Real m Greater Than 1
@chapter Real @math{m > 1}
@anchor{p2}

Here
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
are real and
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)
As in @ref{p1,,part 1}, write

m4_displaymath(
<<<s = \sin\phi, \qquad c = \cos\phi, \qquad y = 1 - m s^2 .>>>,
<<<s = sin φ, c = cos φ, y = 1 − m s² .>>>)

For
m4_mathcomma(<<<0 \le \phi \le \pi/2>>>, <<<0 ≤ φ ≤ π/2>>>)
m4_math(<<<y>>>, <<<y>>>)
decreases from 1 to
m4_mathdot(<<<1 - m < 0>>>, <<<1 − m < 0>>>)
It is zero at the turning point

m4_displaymath(
<<<\phi_0 = \arcsin\frac{1}{\sqrt m} .>>>,
<<<φ₀ = arcsin 1/√m .>>>)

Below
m4_mathcomma(<<<\phi_0>>>, <<<φ₀>>>)
m4_math(<<<E(\phi\,|\,m)>>>, <<<E(φ | m)>>>)
is real. Above it, the integrand
m4_math(<<<\sqrt{1 - m\sin^2\theta}>>>, <<<√(1 − m sin²θ)>>>)
is imaginary, so
m4_math(<<<E>>>, <<<E>>>)
is complex.

@section Summary of the Change
@anchor{s2-1}

@itemize @bullet
@item
A new function, @code{bf-elliptic-e-m>1}, in the @code{bigfloat} package, computes
m4_math(<<<E(\phi\,|\,m)>>>, <<<E(φ | m)>>>)
for real
m4_math(<<<\phi>>>, <<<φ>>>)
and real
m4_math(<<<m > 1>>>, <<<m > 1>>>)
from forms whose terms are all positive (@ref{s2-3,,section 2.3}). Like the other functions in the @code{bigfloat} package, it works on double floats and bigfloats.
@item
@code{bf-elliptic-e} and @code{elliptic-e} call it for real
m4_math(<<<\phi>>>, <<<φ>>>)
and real
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)
@item
@code{bf-elliptic-ec} gets the
m4_math(<<<\phi = \pi/2>>>, <<<φ = π/2>>>)
limit of the same forms, and @code{elliptic-ec} calls @code{bf-elliptic-ec} for
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)
@item
@code{bf-1-m*sin^2}, added for @code{elliptic_f} in @uref{https://sourceforge.net/p/maxima/bugs/5799/, #5799}, gets an optional argument for when to use extra precision (@ref{s2-4,,section 2.4}).
@item
New regression tests, and one existing test reference is corrected (@ref{s2-9,,section 2.9}).
@end itemize

The real part of
m4_math(<<<E>>>, <<<E>>>)
for large
m4_math(<<<m>>>, <<<m>>>)
went from no correct digits to within 2.8 ulps for double floats and 5.1 ulps for bigfloats, and the imaginary part near the turning point went from errors of about
m4_math(<<<10^{13}>>>, <<<10¹³>>>)
ulps to within 3.9 ulps (@ref{s2-8,,section 2.8}). The worst error over all the
m4_math(<<<m > 1>>>, <<<m > 1>>>)
test sets is now 4.8 ulps for double floats and 6.4 ulps for bigfloats. @code{elliptic_e} and @code{elliptic_ec} no longer overflow for
m4_math(<<<m>>>, <<<m>>>)
up to about
m4_math(<<<5\times10^{307}>>>, <<<5×10³⁰⁷>>>)
(@ref{s2-6,,section 2.6}).

@section The Problem
@anchor{s2-2}

For
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
the code used the same formula as for other
m4_mathcomma(<<<m>>>, <<<m>>>)

m4_displaymath(
<<<E(\phi\,|\,m) = s\,R_F(c^2, y, 1) - \frac{m}{3}\,s^3 R_D(c^2, y, 1),>>>,
<<<E(φ | m) = s R_F(c², y, 1) − m/3 s³ R_D(c², y, 1),>>>)

with
m4_math(<<<y < 0>>>, <<<y < 0>>>)
past the turning point, so that
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
are evaluated with a negative argument and complex results. That has four problems.

@strong{The real part for large @math{m}.} For
m4_math(<<<\phi_0 \le \phi \le \pi/2>>>, <<<φ₀ ≤ φ ≤ π/2>>>)
the real part of
m4_math(<<<E>>>, <<<E>>>)
is constant (@ref{s2-3,,section 2.3}). For large
m4_math(<<<m>>>, <<<m>>>)
it is about
m4_mathcomma(<<<\pi/(4\sqrt m)>>>, <<<π/(4√m)>>>)
while the imaginary part is about
m4_mathdot(<<<\sqrt m>>>, <<<√m>>>)
The formula gets the real part as the difference of complex terms of size about
m4_mathcomma(<<<\sqrt m>>>, <<<√m>>>)
and its error grew roughly like
m4_mathdot(<<<\sqrt m>>>, <<<√m>>>)
At
m4_math(<<<\phi = 1>>>, <<<φ = 1>>>)
it was 54 ulps at
m4_mathcomma(<<<m = 10^4>>>, <<<m = 10⁴>>>)
m4_math(<<<6.2\times10^3>>>, <<<6.2×10³>>>)
ulps at
m4_math(<<<10^8>>>, <<<10⁸>>>)
and
m4_math(<<<4.1\times10^7>>>, <<<4.1×10⁷>>>)
ulps at
m4_mathdot(<<<10^{16}>>>, <<<10¹⁶>>>)
For larger
m4_math(<<<m>>>, <<<m>>>)
no digits were correct.

@strong{The imaginary part near the turning point.} Just past
m4_mathcomma(<<<\phi_0>>>, <<<φ₀>>>)
the imaginary part is proportional to
m4_mathcomma(<<<\lvert y\rvert^{3/2}>>>, <<<|y|^(3/2)>>>)
so it needs
m4_math(<<<y>>>, <<<y>>>)
to full relative accuracy. The double-float code computed
m4_math(<<<y>>>, <<<y>>>)
as
m4_mathcomma(<<<(1 - \sqrt m\,s)(1 + \sqrt m\,s)>>>, <<<(1 − √m s)(1 + √m s)>>>)
which has an absolute error of a rounding unit.

@strong{@code{elliptic_ec} for @math{m} just above 1.} The imaginary part of
m4_math(<<<E(m)>>>, <<<E(m)>>>)
goes to 0 as
m4_mathcomma(<<<m \to 1^+>>>, <<<m → 1⁺>>>)
and was computed by the same cancelling formula.

@strong{Overflow.} In double float,
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
overflowed once
m4_math(<<<m>>>, <<<m>>>)
was somewhere between
m4_math(<<<10^{200}>>>, <<<10²⁰⁰>>>)
and
m4_mathdot(<<<10^{250}>>>, <<<10²⁵⁰>>>)

Double-float examples, with reference values from mpmath at the exact inputs:

@need 4890
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.195 0.347 0.33 0.128
@headitem
Expression
@tab
Before
@tab
Exact value
@tab
Error (ulps)
@item
@code{realpart(@/elliptic_ec(@/1e16))}
@tab
m4_math(<<<7.85398152234539{\times}10^{-9}>>>, <<<7.85398152234539×10⁻⁹>>>)
@tab
m4_math(<<<7.8539816339744832{\times}10^{-9}>>>, <<<7.8539816339744832×10⁻⁹>>>)
@tab
m4_math(<<<6.8{\times}10^{7}>>>, <<<6.8×10⁷>>>)
@item
@code{realpart(@/elliptic_e(@/1.0, 1e16))}
@tab
m4_math(<<<7.853981700942021{\times}10^{-9}>>>, <<<7.853981700942021×10⁻⁹>>>)
@tab
m4_math(<<<7.8539816339744832{\times}10^{-9}>>>, <<<7.8539816339744832×10⁻⁹>>>)
@tab
m4_math(<<<4.1{\times}10^{7}>>>, <<<4.1×10⁷>>>)
@item
@code{realpart(@/elliptic_ec(@/1e50))}
@tab
m4_math(<<<-2.5121185622973506{\times}10^{-16}>>>, <<<−2.5121185622973506×10⁻¹⁶>>>)
@tab
m4_math(<<<7.8539816339744828{\times}10^{-26}>>>, <<<7.8539816339744828×10⁻²⁶>>>)
@tab
m4_math(<<<2.2{\times}10^{25}>>>, <<<2.2×10²⁵>>>)
@item
@code{imagpart(@/elliptic_e(@/float(%pi/4)+@/2^-30, 2.0))}
@tab
m4_math(<<<2.679620577077796{\times}10^{-14}>>>, <<<2.679620577077796×10⁻¹⁴>>>)
@tab
m4_math(<<<2.6796243306859025{\times}10^{-14}>>>, <<<2.6796243306859025×10⁻¹⁴>>>)
@tab
m4_math(<<<1.2{\times}10^{10}>>>, <<<1.2×10¹⁰>>>)
@item
@code{imagpart(@/elliptic_ec(@/float(1+@/2^-20)))}
@tab
m4_math(<<<7.490137887700143{\times}10^{-7}>>>, <<<7.490137887700143×10⁻⁷>>>)
@tab
m4_math(<<<7.4901378871664477{\times}10^{-7}>>>, <<<7.4901378871664477×10⁻⁷>>>)
@tab
m4_math(<<<5.0{\times}10^{5}>>>, <<<5.0×10⁵>>>)
@item
@code{elliptic_e(@/1.0, 1e250)}
@tab
overflow
@tab
m4_math(<<<7.85{\times}10^{-126} + 4.60{\times}10^{124}\,i>>>, <<<7.85×10⁻¹²⁶ + 4.60×10¹²⁴ i>>>)
@tab

@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-2-1}
@noindent
@strong{Table 2.1.} Double-float examples of the problems for real
m4_math(<<<m > 1\colon>>>, <<<m > 1:>>>)
the value before the change, the exact value from mpmath, and the error before the change.

@tex
\medskip
@end tex

In each of these examples, the part not shown is accurate. Bigfloats have the same problems, apart from the overflow.

@section Forms Without Cancellation
@anchor{s2-3}

This section takes
m4_mathdot(<<<0 \le \phi \le \pi/2>>>, <<<0 ≤ φ ≤ π/2>>>)
Other
m4_math(<<<\phi>>>, <<<φ>>>)
are covered in @ref{s2-5,,section 2.5}.

@strong{Up to the turning point.} @uref{https://dlmf.nist.gov/19.25.E11, DLMF 19.25.11}, with
m4_mathcomma(<<<k^2 = m>>>, <<<k² = m>>>)
m4_math(<<<k'^2 = 1 - m>>>, <<<k'² = 1 − m>>>)
and
m4_mathcomma(<<<C = \csc^2\phi>>>, <<<C = csc²φ>>>)
is

m4_displaymath(
<<<E(\phi\,|\,m) = -\frac{1-m}{3}\,R_D(C-m, C, C-1) + \sqrt{\frac{C-m}{C(C-1)}} .>>>,
<<<E(φ | m) = −(1−m)/3 R_D(C−m, C, C−1) + √((C−m)/(C(C−1))) .>>>)

With
m4_mathcomma(<<<C - m = y/s^2>>>, <<<C − m = y/s²>>>)
m4_mathcomma(<<<C = 1/s^2>>>, <<<C = 1/s²>>>)
m4_mathcomma(<<<C - 1 = c^2/s^2>>>, <<<C − 1 = c²/s²>>>)
and the homogeneity of
m4_math(<<<R_D>>>, <<<R_D>>>)
(degree
m4_mathpunc(<<<),>>>, <<<-3/2>>>, <<<−3/2>>>)

m4_displaymath(
<<<E(\phi\,|\,m) = \frac{m-1}{3}\,s^3 R_D(y, 1, c^2) + \frac{s\sqrt y}{c}, \qquad 0 \le \phi \le \phi_0 .>>>,
<<<E(φ | m) = (m−1)/3 s³ R_D(y, 1, c²) + (s√y)/c, 0 ≤ φ ≤ φ₀ .>>>)

For
m4_math(<<<m > 1>>>, <<<m > 1>>>)
both terms are positive. Since
m4_mathcomma(<<<\phi_0 < \pi/2>>>, <<<φ₀ < π/2>>>)
m4_math(<<<c>>>, <<<c>>>)
is bounded away from 0.

@strong{Past the turning point.} For
m4_mathcomma(<<<\phi_0 < \theta \le \pi/2>>>, <<<φ₀ < θ ≤ π/2>>>)
m4_mathcomma(<<<\sqrt{1 - m\sin^2\theta} = i\sqrt{m\sin^2\theta - 1}>>>, <<<√(1 − m sin²θ) = i√(m sin²θ − 1)>>>)
so the real part of
m4_math(<<<E>>>, <<<E>>>)
stays at its value at
m4_mathcomma(<<<\phi_0>>>, <<<φ₀>>>)
and

m4_displaymath(
<<<\mathrm{Im}\,E(\phi\,|\,m) = \int_{\phi_0}^{\phi}\sqrt{m\sin^2\theta - 1}\,d\theta .>>>,
<<<Im E(φ | m) = ∫_φ₀^φ√(m sin²θ − 1) dθ .>>>)

This is the sign used by Maxima before the change, mpmath and Wolfram|Alpha.

The real part is the form above at
m4_mathcomma(<<<\phi = \phi_0>>>, <<<φ = φ₀>>>)
where
m4_mathcomma(<<<y = 0>>>, <<<y = 0>>>)
m4_math(<<<s^2 = 1/m>>>, <<<s² = 1/m>>>)
and
m4_math(<<<c^2 = (m-1)/m\colon>>>, <<<c² = (m−1)/m:>>>)

m4_displaymath(
<<<\mathrm{Re}\,E(\phi\,|\,m) = E(\phi_0\,|\,m) = \frac{(m-1)/m}{3\sqrt m}\,R_D\!\left(0, 1, \frac{m-1}{m}\right).>>>,
<<<Re E(φ | m) = E(φ₀ | m) = ((m−1)/m)/(3√m) R_D(0, 1, (m−1)/m).>>>)

For the imaginary part, substitute
m4_mathcomma(<<<\cos\theta = \sqrt{(m-1)/m}\,\cos u>>>, <<<cos θ = √((m−1)/m) cos u>>>)
as in the @uref{https://sourceforge.net/p/maxima/bugs/5799/, #5799} change to @code{bf-elliptic-f}. Then
m4_mathcomma(<<<m\sin^2\theta - 1 = (m-1)\sin^2 u>>>, <<<m sin²θ − 1 = (m−1) sin² u>>>)
and the integral becomes

m4_displaymath(
<<<(m-1)\int_0^{U}\frac{\sin^2 u}{\sqrt{1 + (m-1)\sin^2 u}}\,du = E(U\,|\,1-m) - F(U\,|\,1-m), \qquad \sin^2 U = \frac{-y}{m-1} .>>>,
<<<(m−1)∫₀^U(sin² u)/(√(1 + (m−1) sin² u)) du = E(U | 1−m) − F(U | 1−m), sin² U = (−y)/(m−1) .>>>)

@uref{https://dlmf.nist.gov/19.25.E9, DLMF 19.25.9}, scaled as in @ref{s1-2,,section 1.2}, gives
m4_math(<<<E - F>>>, <<<E − F>>>)
in Carlson form. With
m4_mathcomma(<<<s_1 = \sin U>>>, <<<s₁ = sin U>>>)
m4_math(<<<\cos^2 U = mc^2/(m-1)>>>, <<<cos² U = mc²/(m−1)>>>)
and
m4_mathcomma(<<<1 + (m-1)s_1^2 = 1 - y = ms^2>>>, <<<1 + (m−1)s₁² = 1 − y = ms²>>>)

m4_displaymath(
<<<\mathrm{Im}\,E(\phi\,|\,m) = \frac{m-1}{3}\,s_1^3\,R_D\!\left(\frac{mc^2}{m-1},\ ms^2,\ 1\right), \qquad s_1^2 = \frac{-y}{m-1} .>>>,
<<<Im E(φ | m) = (m−1)/3 s₁³ R_D(mc²/(m−1), ms², 1), s₁² = (−y)/(m−1) .>>>)

All the terms are positive.
m4_math(<<<c>>>, <<<c>>>)
appears only as
m4_mathcomma(<<<c^2>>>, <<<c²>>>)
so a reduced
m4_math(<<<\phi>>>, <<<φ>>>)
just past
m4_math(<<<\pi/2>>>, <<<π/2>>>)
(see @ref{s1-4,,section 1.4}) needs no special handling here.

@strong{The complete integral.} At
m4_mathcomma(<<<\phi = \pi/2>>>, <<<φ = π/2>>>)
m4_math(<<<s_1 = 1>>>, <<<s₁ = 1>>>)
and
m4_math(<<<c = 0\colon>>>, <<<c = 0:>>>)

m4_displaymath(
<<<E(m) = \frac{(m-1)/m}{3\sqrt m}\,R_D\!\left(0, 1, \frac{m-1}{m}\right) + i\,\frac{m-1}{3}\,R_D(0, m, 1) .>>>,
<<<E(m) = ((m−1)/m)/(3√m) R_D(0, 1, (m−1)/m) + i (m−1)/3 R_D(0, m, 1) .>>>)

The factor
m4_math(<<<(m-1)/m>>>, <<<(m−1)/m>>>)
is not written as
m4_mathcomma(<<<1 - 1/m>>>, <<<1 − 1/m>>>)
because that cancels when
m4_math(<<<m>>>, <<<m>>>)
is near 1. For doubles,
m4_math(<<<m - 1>>>, <<<m − 1>>>)
is exact there.

These forms were checked against mpmath's @code{ellipe} to 40 digits, including the sign of the imaginary part and negative
m4_mathdot(<<<\phi>>>, <<<φ>>>)

@section Computing @math{y} Accurately
@anchor{s2-4}

The imaginary part is proportional to
m4_mathcomma(<<<s_1^3 \propto \lvert y\rvert^{3/2}>>>, <<<s₁³ ∝ |y|^(3/2)>>>)
so its relative error is
m4_math(<<<\tfrac32>>>, <<<3/2>>>)
times the relative error of
m4_mathdot(<<<y>>>, <<<y>>>)
Computed as
m4_mathcomma(<<<1 - ms^2>>>, <<<1 − ms²>>>)
m4_math(<<<y>>>, <<<y>>>)
has an absolute error of a few rounding units of
m4_mathcomma(<<<ms^2>>>, <<<ms²>>>)
from rounding
m4_mathcomma(<<<\sin\phi>>>, <<<sin φ>>>)
the multiplications and the subtraction. With
m4_math(<<<ms^2 = 1 + \lvert y\rvert>>>, <<<ms² = 1 + |y|>>>)
past the turning point, its relative error is about

m4_displaymath(
<<<\frac{\lvert\delta y\rvert}{\lvert y\rvert} \approx \kappa\,\varepsilon\left(1 + \frac{1}{\lvert y\rvert}\right),>>>,
<<<|δy|/|y| ≈ κ ε(1 + 1/|y|),>>>)

where
m4_mathcomma(<<<\kappa>>>, <<<κ>>>)
the number of rounding units in
m4_mathcomma(<<<ms^2>>>, <<<ms²>>>)
is between 1 and 2.

@code{bf-1-m*sin^2} from @uref{https://sourceforge.net/p/maxima/bugs/5799/, #5799} computes
m4_math(<<<y>>>, <<<y>>>)
and recomputes it with extra precision until it has full relative accuracy, but only when
m4_mathdot(<<<\lvert y\rvert \le 1/16>>>, <<<|y| ≤ 1/16>>>)
That is enough for @code{elliptic_f}, which depends on
m4_math(<<<\sqrt y>>>, <<<√y>>>)
near the turning point and so only needs
m4_math(<<<y>>>, <<<y>>>)
to small absolute error. For the imaginary part of
m4_math(<<<E>>>, <<<E>>>)
it leaves relative errors of up to about
m4_math(<<<\tfrac32 \cdot 17\kappa\varepsilon>>>, <<<3/2 · 17κε>>>)
just above
m4_mathdot(<<<\lvert y\rvert = 1/16>>>, <<<|y| = 1/16>>>)

Example:
m4_mathcomma(<<<\phi = 1.1545304187230139>>>, <<<φ = 1.1545304187230139>>>)
m4_mathcomma(<<<m = 1.285709708166709>>>, <<<m = 1.285709708166709>>>)
where
m4_math(<<<ms^2 = 1.0755>>>, <<<ms² = 1.0755>>>)
and
m4_math(<<<y = -0.0754995731919693829\colon>>>, <<<y = −0.0754995731919693829:>>>)

@need 1850
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.32 0.32 0.16 0.20
@headitem
How
m4_math(<<<y>>>, <<<y>>>)
is computed
@tab
m4_math(<<<y>>>, <<<y>>>)
@tab
Error in
m4_math(<<<y>>>, <<<y>>>)
(ulps)
@tab
Error in Im
m4_math(<<<E>>>, <<<E>>>)
(ulps)
@item
m4_mathcomma(<<<1 - ms^2>>>, <<<1 − ms²>>>)
since
m4_math(<<<\lvert y\rvert > 1/16>>>, <<<|y| > 1/16>>>)
@tab
m4_math(<<<-0.075499573191969205>>>, <<<−0.075499573191969205>>>)
@tab
12.8
@tab
28.2
@item
With extra precision
@tab
m4_math(<<<-0.075499573191969385>>>, <<<−0.075499573191969385>>>)
@tab
0.17
@tab
0.37
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-2-2}
@noindent
@strong{Table 2.2.} How the accuracy of
m4_math(<<<y>>>, <<<y>>>)
affects the imaginary part of
m4_mathcomma(<<<E>>>, <<<E>>>)
at
m4_mathcomma(<<<\phi = 1.1545304187230139>>>, <<<φ = 1.1545304187230139>>>)
m4_mathdot(<<<m = 1.285709708166709>>>, <<<m = 1.285709708166709>>>)

@tex
\medskip
@end tex

Before the turning point,
m4_math(<<<E>>>, <<<E>>>)
is real, but for
m4_math(<<<m>>>, <<<m>>>)
near 1 it needs
m4_math(<<<y>>>, <<<y>>>)
to full relative accuracy too. There
m4_math(<<<\phi_0>>>, <<<φ₀>>>)
is close to
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
and the term
m4_math(<<<s\sqrt y/c>>>, <<<s√y/c>>>)
of @ref{s2-3,,section 2.3} is nearly all of
m4_mathcomma(<<<E>>>, <<<E>>>)
so the relative error of
m4_math(<<<E>>>, <<<E>>>)
is about half that of
m4_mathdot(<<<y>>>, <<<y>>>)

So @code{bf-1-m*sin^2} gets an optional argument @code{limit}, the largest
m4_math(<<<\lvert y\rvert>>>, <<<|y|>>>)
for which it uses extra precision, with default
m4_mathdot(<<<1/16>>>, <<<1/16>>>)
@code{bf-elliptic-e-m>1} passes 1:

@itemize @bullet
@item
Above
m4_mathcomma(<<<\lvert y\rvert = 1>>>, <<<|y| = 1>>>)
m4_mathcomma(<<<1 + 1/\lvert y\rvert < 2>>>, <<<1 + 1/|y| < 2>>>)
so
m4_math(<<<1 - ms^2>>>, <<<1 − ms²>>>)
already has a relative error below about
m4_mathdot(<<<2\kappa\varepsilon>>>, <<<2κε>>>)
A larger limit gains at most a factor of 2.
@item
Before the turning point,
m4_mathcomma(<<<0 < y \le 1>>>, <<<0 < y ≤ 1>>>)
so a limit of 1 means that extra precision is always used there.
@end itemize

Worst errors in ulps, double float / bigfloat, for several limits. The limit applies to all
m4_mathcomma(<<<y>>>, <<<y>>>)
as in the code:

@need 2800
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.27 0.11 0.124 0.124 0.124 0.124 0.124
@headitem
Inputs
@tab
Points
@tab
m4_math(<<<1/16>>>, <<<1/16>>>)
@tab
m4_math(<<<1/4>>>, <<<1/4>>>)
@tab
m4_math(<<<1>>>, <<<1>>>)
@tab
m4_math(<<<4>>>, <<<4>>>)
@tab
Always
@item
Before the turning point,
m4_math(<<<1 < m < 10^4>>>, <<<1 < m < 10⁴>>>)
@tab
400
@tab
3.09 / 4.87
@tab
4.01 / 4.87
@tab
4.01 / 4.87
@tab
4.01 / 4.87
@tab
4.01 / 4.87
@item
Before the turning point,
m4_math(<<<1 < m < 1.1>>>, <<<1 < m < 1.1>>>)
@tab
300
@tab
5.38 / 14.4
@tab
2.80 / 3.62
@tab
2.80 / 3.62
@tab
2.80 / 3.62
@tab
2.80 / 3.62
@item
Past the turning point,
m4_math(<<<1 < m < 10^4>>>, <<<1 < m < 10⁴>>>)
@tab
60
@tab
24.8 / 21.4
@tab
11.4 / 8.52
@tab
4.60 / 6.44
@tab
4.60 / 6.44
@tab
4.88 / 8.78
@item
Past the turning point,
m4_math(<<<-1 < y < -1/16>>>, <<<−1 < y < −1/16>>>)
@tab
60
@tab
9.00 / 26.3
@tab
9.00 / 13.9
@tab
6.41 / 7.18
@tab
6.41 / 7.18
@tab
6.41 / 7.18
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-2-3}
@noindent
@strong{Table 2.3.} Worst errors in ulps, double float / bigfloat, for several values of the @code{limit} argument of @code{bf-1-m*sin^2}. Always means extra precision for every
m4_mathdot(<<<y>>>, <<<y>>>)

@tex
\medskip
@end tex

Limits below 1 leave errors of up to 26 ulps past the turning point and up to 14 ulps before it for
m4_math(<<<m>>>, <<<m>>>)
near 1. Limits above 1 bring no consistent gain. Before the turning point for general
m4_mathcomma(<<<m>>>, <<<m>>>)
the default limit had a slightly smaller worst error for double floats, 3.09 against 4.01 ulps, but the mean errors over the 400 points were 0.63 and 0.67 ulps, so this is within the scatter.

@section Large Arguments
@anchor{s2-5}

For other
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
the code uses
m4_math(<<<E(\phi\,|\,m) = E(\phi - k\pi\,|\,m) + 2k\,E(m)>>>, <<<E(φ | m) = E(φ − kπ | m) + 2k E(m)>>>)
with
m4_mathcomma(<<<k = \mathrm{round}(\phi/\pi)>>>, <<<k = round(φ/π)>>>)
and
m4_mathdot(<<<E(-\phi\,|\,m) = -E(\phi\,|\,m)>>>, <<<E(−φ | m) = −E(φ | m)>>>)

For
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
the real part grows by only about
m4_math(<<<\pi/(2\sqrt m)>>>, <<<π/(2√m)>>>)
per period, since
m4_mathdot(<<<\mathrm{Re}\,E(m) \approx \pi/(4\sqrt m)>>>, <<<Re E(m) ≈ π/(4√m)>>>)
The rounding error of
m4_mathcomma(<<<\phi - k\pi>>>, <<<φ − kπ>>>)
about
m4_math(<<<\lvert k\rvert>>>, <<<|k|>>>)
rounding units of
m4_mathcomma(<<<\pi>>>, <<<π>>>)
is multiplied by
m4_math(<<<\sqrt y>>>, <<<√y>>>)
in the real part, so relative to the real part it is magnified roughly in proportion to
m4_mathdot(<<<\sqrt m>>>, <<<√m>>>)
At
m4_mathcomma(<<<\phi = -12.54>>>, <<<φ = −12.54>>>)
m4_mathcomma(<<<m = 180.8>>>, <<<m = 180.8>>>)
a version that reduced
m4_math(<<<\phi>>>, <<<φ>>>)
first had an error of 16.3 ulps in the bigfloat real part, against 1.26 ulps with the unreduced
m4_mathdot(<<<\phi>>>, <<<φ>>>)
For double floats the errors were 5.95 and 2.05 ulps.

As in the @uref{https://sourceforge.net/p/maxima/bugs/5799/, #5799} change to @code{bf-elliptic-f}, the code takes
m4_math(<<<\sin>>>, <<<sin>>>)
and
m4_math(<<<\cos>>>, <<<cos>>>)
of the unreduced
m4_math(<<<\phi>>>, <<<φ>>>)
instead. With
m4_math(<<<\sin(\phi - k\pi) = (-1)^k\sin\phi>>>, <<<sin (φ − kπ) = (−1)ᵏ sin φ>>>)
and
m4_mathcomma(<<<\cos^2(\phi - k\pi) = \cos^2\phi>>>, <<<cos²(φ − kπ) = cos²φ>>>)
the forms of @ref{s2-3,,section 2.3} need only
m4_mathcomma(<<<\lvert\sin(\phi - k\pi)\rvert>>>, <<<| sin (φ − kπ)|>>>)
m4_mathcomma(<<<\cos^2\phi>>>, <<<cos²φ>>>)
m4_math(<<<y>>>, <<<y>>>)
and the sign of
m4_mathcomma(<<<\sin(\phi - k\pi)>>>, <<<sin (φ − kπ)>>>)
which is the sign of
m4_mathdot(<<<\phi - k\pi>>>, <<<φ − kπ>>>)
The reduced angle itself is never formed.

@section Overflow
@anchor{s2-6}

In double float, @code{bf-rd} overflowed for arguments around
m4_mathcomma(<<<10^{250}>>>, <<<10²⁵⁰>>>)
so
m4_math(<<<R_D(mc^2/(m-1), ms^2, 1)>>>, <<<R_D(mc²/(m−1), ms², 1)>>>)
and
m4_math(<<<R_D(0, m, 1)>>>, <<<R_D(0, m, 1)>>>)
would overflow for large
m4_mathdot(<<<m>>>, <<<m>>>)
Homogeneity with factor
m4_math(<<<1/m>>>, <<<1/m>>>)
keeps all arguments at most 1:

m4_displaymath(
<<<R_D\!\left(\frac{mc^2}{m-1}, ms^2, 1\right) = m^{-3/2}\,R_D\!\left(\frac{c^2}{m-1}, s^2, \frac1m\right), \qquad R_D(0, m, 1) = m^{-3/2}\,R_D\!\left(0, 1, \frac1m\right).>>>,
<<<R_D(mc²/(m−1), ms², 1) = m^(−3/2) R_D(c²/(m−1), s², 1/m), R_D(0, m, 1) = m^(−3/2) R_D(0, 1, 1/m).>>>)

The factor
m4_math(<<<\frac{m-1}{3}m^{-3/2}>>>, <<<(m−1)/3 m^(−3/2)>>>)
is computed as
m4_mathcomma(<<<\frac{(m-1)/m}{3\sqrt m}>>>, <<<((m−1)/m)/(3√m)>>>)
which does not overflow either.

@code{elliptic_e} and @code{elliptic_ec} now work for double-float
m4_math(<<<m>>>, <<<m>>>)
up to about
m4_mathdot(<<<5\times10^{307}>>>, <<<5×10³⁰⁷>>>)
Above about
m4_mathcomma(<<<6\times10^{307}>>>, <<<6×10³⁰⁷>>>)
the value of
m4_math(<<<R_D(0, 1, 1/m)>>>, <<<R_D(0, 1, 1/m)>>>)
itself, about
m4_mathcomma(<<<3m>>>, <<<3m>>>)
is larger than the largest double float. Before the change, @code{elliptic_e(1.0, 1e200)} worked but @code{elliptic_e(1.0, 1e250)} overflowed. Now @code{elliptic_e(1.0, 1e250)} returns @code{4.596976941318603e124*%i+@/7.853981633974484e-126}, with errors of 0.45 ulps in the real part and 0.38 ulps in the imaginary part.

@section Implementation
@anchor{s2-7}

The new function @code{bf-elliptic-e-m>1}, in @code{src/ellipt.lisp}, takes
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_mathdot(<<<m>>>, <<<m>>>)
It computes
m4_mathcomma(<<<k = \mathrm{round}(\phi/\pi)>>>, <<<k = round(φ/π)>>>)
takes
m4_math(<<<s>>>, <<<s>>>)
and
m4_math(<<<c>>>, <<<c>>>)
from the unreduced
m4_math(<<<\phi>>>, <<<φ>>>)
(@ref{s2-5,,section 2.5}), and computes
m4_math(<<<y>>>, <<<y>>>)
with @code{bf-1-m*sin^2} with a limit of 1 (@ref{s2-4,,section 2.4}). It then evaluates whichever form of @ref{s2-3,,section 2.3} applies, with the arguments of
m4_math(<<<R_D>>>, <<<R_D>>>)
scaled as in @ref{s2-6,,section 2.6}, and adds
m4_mathdot(<<<2k\,E(m)>>>, <<<2k E(m)>>>)
Past the turning point, it takes the real part from @code{bf-elliptic-ec}.

@itemize @bullet
@item
@code{bf-elliptic-e} and @code{elliptic-e} call it for real
m4_math(<<<\phi>>>, <<<φ>>>)
and real
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
before their own reduction of
m4_mathdot(<<<\phi>>>, <<<φ>>>)
@item
@code{bf-elliptic-ec} evaluates the complete form of @ref{s2-3,,section 2.3}, scaled as in @ref{s2-6,,section 2.6}, for real
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
and @code{elliptic-ec} calls @code{bf-elliptic-ec} for
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)
@item
@code{bf-1-m*sin^2} gets an optional argument, @code{limit}, with default
m4_mathdot(<<<1/16>>>, <<<1/16>>>)
@end itemize

For double floats with
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
the new code takes about 0.05 s per 1000 calls, against 0.26 s before, even though more inputs now go through the extra-precision path. The old code evaluated
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
in complex arithmetic past the turning point.

@section Accuracy
@anchor{s2-8}

Errors below are the worst errors in ulps of the real and imaginary parts, measured separately. References are from mpmath at 260 digits, at the exact inputs. ``Before'' is the code without the changes of either part. The change of @ref{p1,,part 1} does not affect
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)

@need 6790
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.23 0.10 0.19 0.13 0.20 0.15
@headitem
Inputs
@tab
Points
@tab
Double before
@tab
Double after
@tab
Bigfloat before
@tab
Bigfloat after
@item
Before the turning point,
m4_math(<<<1 < m < 10^4>>>, <<<1 < m < 10⁴>>>)
@tab
60
@tab
Re 3.25
@tab
Re 3.24
@tab
Re 3.40
@tab
Re 2.98
@item
Past the turning point,
m4_math(<<<1 < m < 10^4>>>, <<<1 < m < 10⁴>>>)
@tab
60
@tab
Re 103@*
Im 6900
@tab
Re 3.92@*
Im 4.60
@tab
Re 141@*
Im
m4_math(<<<1.1{\times}10^{4}>>>, <<<1.1×10⁴>>>)
@tab
Re 5.34@*
Im 6.44
@item
Before the turning point,
m4_math(<<<10^4 < m < 10^{100}>>>, <<<10⁴ < m < 10¹⁰⁰>>>)
@tab
30
@tab
Re 2.33
@tab
Re 2.82
@tab
Re 6.01
@tab
Re 1.16
@item
Past the turning point,
m4_math(<<<10^4 < m < 10^{100}>>>, <<<10⁴ < m < 10¹⁰⁰>>>)
@tab
30
@tab
Re
m4_math(<<<3.2{\times}10^{48}>>>, <<<3.2×10⁴⁸>>>)
@*
Im 3.72
@tab
Re 2.08@*
Im 4.28
@tab
Re
m4_math(<<<3.7{\times}10^{48}>>>, <<<3.7×10⁴⁸>>>)
@*
Im 4.15
@tab
Re 4.47@*
Im 5.18
@item
Within
m4_math(<<<10^{-4}>>>, <<<10⁻⁴>>>)
to
m4_math(<<<10^{-12}>>>, <<<10⁻¹²>>>)
of the turning point, relatively, on both sides
@tab
30
@tab
Re 6.49@*
Im
m4_math(<<<1.3{\times}10^{13}>>>, <<<1.3×10¹³>>>)
@tab
Re 3.48@*
Im 3.90
@tab
Re 9.34@*
Im
m4_math(<<<7.2{\times}10^{12}>>>, <<<7.2×10¹²>>>)
@tab
Re 5.04@*
Im 3.40
@item
m4_mathcomma(<<<1 < m < 1.1>>>, <<<1 < m < 1.1>>>)
before the turning point
@tab
30
@tab
Re 4.92
@tab
Re 2.08
@tab
Re 14.6
@tab
Re 2.95
@item
m4_mathcomma(<<<\phi = \pi/2>>>, <<<φ = π/2>>>)
m4_math(<<<1 < m < 10^{50}>>>, <<<1 < m < 10⁵⁰>>>)
@tab
20
@tab
Re
m4_math(<<<1.2{\times}10^{18}>>>, <<<1.2×10¹⁸>>>)
@*
Im 18.0
@tab
Re 1.89@*
Im 2.97
@tab
Re
m4_math(<<<2.0{\times}10^{18}>>>, <<<2.0×10¹⁸>>>)
@*
Im 32.5
@tab
Re 3.79@*
Im 4.05
@item
Negative and large
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
m4_mathcomma(<<<\lvert\phi\rvert < 30>>>, <<<|φ| < 30>>>)
m4_math(<<<1 < m < 10^6>>>, <<<1 < m < 10⁶>>>)
@tab
40
@tab
Re 1070@*
Im 121
@tab
Re 3.59@*
Im 4.84
@tab
Re 2010@*
Im 143
@tab
Re 4.19@*
Im 6.05
@item
@code{elliptic_ec},
m4_math(<<<m = 1 + 10^{-k}>>>, <<<m = 1 + 10⁻ᵏ>>>)
for
m4_mathcomma(<<<k = 1, 3, 6, 9, 12, 15>>>, <<<k = 1, 3, 6, 9, 12, 15>>>)
and
m4_math(<<<1 < m < 10^{100}>>>, <<<1 < m < 10¹⁰⁰>>>)
@tab
46
@tab
Re
m4_math(<<<4.4{\times}10^{48}>>>, <<<4.4×10⁴⁸>>>)
@*
Im
m4_math(<<<1.6{\times}10^{16}>>>, <<<1.6×10¹⁶>>>)
@tab
Re 2.73@*
Im 3.34
@tab
Re
m4_math(<<<3.8{\times}10^{48}>>>, <<<3.8×10⁴⁸>>>)
@*
Im
m4_math(<<<5.0{\times}10^{15}>>>, <<<5.0×10¹⁵>>>)
@tab
Re 5.05@*
Im 5.03
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-2-4}
@noindent
@strong{Table 2.4.} Worst errors in ulps of the real and imaginary parts for real
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
before and after the change.

@tex
\medskip
@end tex

The few increases are where neither the old nor the new form cancels, and are within the scatter of a few ulps. The largest remaining errors are 4.84 ulps for double floats and 6.44 ulps for bigfloats. In the prototype, at one of the worst bigfloat points
m4_mathcomma(<<<(\phi = 0.665>>>, <<<(φ = 0.665>>>)
m4_mathpunc(<<<),>>>, <<<m = 8.09>>>, <<<m = 8.09>>>)
m4_math(<<<R_D>>>, <<<R_D>>>)
itself was accurate to 0.46 ulps, and the rest came from the rounding of
m4_mathcomma(<<<\sin\phi>>>, <<<sin φ>>>)
m4_mathcomma(<<<\cos\phi>>>, <<<cos φ>>>)
m4_math(<<<y>>>, <<<y>>>)
and the products and quotients built from them.

The examples of @ref{s2-2,,section 2.2} after the change:

@need 2610
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.50 0.32 0.18
@headitem
Expression
@tab
After
@tab
Error (ulps)
@item
@code{realpart(@/elliptic_ec(@/1e16))}
@tab
m4_math(<<<7.853981633974483{\times}10^{-9}>>>, <<<7.853981633974483×10⁻⁹>>>)
@tab
0.29
@item
@code{realpart(@/elliptic_e(@/1.0, 1e16))}
@tab
m4_math(<<<7.853981633974483{\times}10^{-9}>>>, <<<7.853981633974483×10⁻⁹>>>)
@tab
0.29
@item
@code{realpart(@/elliptic_ec(@/1e50))}
@tab
m4_math(<<<7.853981633974482{\times}10^{-26}>>>, <<<7.853981633974482×10⁻²⁶>>>)
@tab
0.93
@item
@code{imagpart(@/elliptic_e(@/float(%pi/4)+@/2^-30, 2.0))}
@tab
m4_math(<<<2.679624330685902{\times}10^{-14}>>>, <<<2.679624330685902×10⁻¹⁴>>>)
@tab
1.51
@item
@code{imagpart(@/elliptic_ec(@/float(1+@/2^-20)))}
@tab
m4_math(<<<7.490137887166446{\times}10^{-7}>>>, <<<7.490137887166446×10⁻⁷>>>)
@tab
1.64
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-2-5}
@noindent
@strong{Table 2.5.} The examples of Table 2.1 after the change.

@tex
\medskip
@end tex

The change does not affect real
m4_mathcomma(<<<m \le 1>>>, <<<m ≤ 1>>>)
complex
m4_math(<<<\phi>>>, <<<φ>>>)
or complex
m4_mathdot(<<<m>>>, <<<m>>>)
It does not affect @code{jacobi_epsilon} either: for real
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
@code{jacobi-epsilon} calls @code{bf-elliptic-e} and @code{bf-elliptic-ec} only with
m4_math(<<<1/m>>>, <<<1/m>>>)
or
m4_mathcomma(<<<1 - m>>>, <<<1 − m>>>)
never with
m4_math(<<<m>>>, <<<m>>>)
itself (@ref{s1-8,,section 1.8}). On 60 points with
m4_math(<<<m \le 0>>>, <<<m ≤ 0>>>)
and
m4_mathcomma(<<<\lvert\phi\rvert < 30>>>, <<<|φ| < 30>>>)
double and bigfloat results are bit-for-bit unchanged, and the part 1 measurements are unchanged.

@section Tests
@anchor{s2-9}

The new tests are at the end of @code{tests/rtest_elliptic.mac}, after those of @ref{s1-7,,section 1.7}, for double floats and for bigfloats at @code{fpprec : 32}. References are from mpmath at 400 digits, at the exact inputs.

@itemize @bullet
@item
All inputs are dyadic rationals, so the float and bigfloat arguments are exact.
@item
The real and imaginary parts are checked separately. A check of the complex value would miss an error in a real part of
m4_math(<<<10^{-26}>>>, <<<10⁻²⁶>>>)
next to an imaginary part of
m4_mathdot(<<<10^{25}>>>, <<<10²⁵>>>)
@item
Each tolerance was first chosen as the absolute error observed in testing, rounded up to five significant digits, or
m4_math(<<<2^{-53}>>>, <<<2⁻⁵³>>>)
or
m4_math(<<<2^{-108}>>>, <<<2⁻¹⁰⁸>>>)
times the magnitude of that part where the error was 0. As in @ref{s1-7,,section 1.7}, the tests check relative errors, with the tolerance written as a multiple of
m4_math(<<<2^{-53}>>>, <<<2⁻⁵³>>>)
or
m4_mathdot(<<<2^{-109}>>>, <<<2⁻¹⁰⁹>>>)
@end itemize

Double-float tests:

@need 6220
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.255 0.18 0.2825 0.2825
@headitem
Expression
@tab
What it tests
@tab
Real part
@tab
Imaginary part
@item
@code{elliptic_e(@/0.5, 2.0)}
@tab
Before the turning point
@tab
m4_math(<<<0.4569923520755740>>>, <<<0.4569923520755740>>>)
@tab
m4_math(<<<0>>>, <<<0>>>)
@item
@code{elliptic_e(@/1.0, 2.0)}
@tab
Past the turning point
@tab
m4_math(<<<0.5990701173677961>>>, <<<0.5990701173677961>>>)
@tab
m4_math(<<<0.093112921772178507>>>, <<<0.093112921772178507>>>)
@item
@code{elliptic_e(@/float(%pi/4)-@/2^-30, 2.0)}
@tab
Just before the turning point
@tab
m4_math(<<<0.59907011736776931>>>, <<<0.59907011736776931>>>)
@tab
m4_math(<<<0>>>, <<<0>>>)
@item
@code{elliptic_e(@/float(%pi/4)+@/2^-30, 2.0)}
@tab
Just past the turning point
@tab
m4_math(<<<0.5990701173677961>>>, <<<0.5990701173677961>>>)
@tab
m4_math(<<<2.6796243306859025\times10^{-14}>>>, <<<2.6796243306859025×10⁻¹⁴>>>)
@item
@code{elliptic_e(@/1.25, 1.25)}
@tab
m4_math(<<<1/16 < \lvert y\rvert < 1>>>, <<<1/16 < |y| < 1>>>)
@tab
m4_math(<<<0.81286533580320924>>>, <<<0.81286533580320924>>>)
@tab
m4_math(<<<0.03470937738124192>>>, <<<0.03470937738124192>>>)
@item
@code{elliptic_e(@/1.0, 2.0^60)}
@tab
Large
m4_math(<<<m>>>, <<<m>>>)
@tab
m4_math(<<<7.3145903963357984\times10^{-10}>>>, <<<7.3145903963357984×10⁻¹⁰>>>)
@tab
m4_math(<<<4.9359664058573775\times10^{8}>>>, <<<4.9359664058573775×10⁸>>>)
@item
@code{elliptic_e(@/-12.5, 180.0)}
@tab
Negative
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
reduction by
m4_math(<<<\pi>>>, <<<π>>>)
@tab
m4_math(<<<-0.41259995195037558>>>, <<<−0.41259995195037558>>>)
@tab
m4_math(<<<-105.99408768196068>>>, <<<−105.99408768196068>>>)
@item
@code{elliptic_ec(@/2.0)}
@tab
@code{elliptic_ec}
@tab
m4_math(<<<0.5990701173677961>>>, <<<0.5990701173677961>>>)
@tab
m4_math(<<<0.5990701173677961>>>, <<<0.5990701173677961>>>)
@item
@code{elliptic_ec(@/float(1+@/2^-20))}
@tab
m4_math(<<<m>>>, <<<m>>>)
near 1
@tab
m4_math(<<<0.99999627219983125>>>, <<<0.99999627219983125>>>)
@tab
m4_math(<<<7.4901378871664477\times10^{-7}>>>, <<<7.4901378871664477×10⁻⁷>>>)
@item
@code{elliptic_ec(@/1e16)}
@tab
Large
m4_math(<<<m>>>, <<<m>>>)
@tab
m4_math(<<<7.8539816339744832\times10^{-9}>>>, <<<7.8539816339744832×10⁻⁹>>>)
@tab
m4_math(<<<9.9999999999999898\times10^{7}>>>, <<<9.9999999999999898×10⁷>>>)
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-2-6}
@noindent
@strong{Table 2.6.} Double-float tests added for real
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
with the parts of the exact values from mpmath.

@tex
\medskip
@end tex

Bigfloat tests, the same cases with bigfloat arguments, with their references in the notation of the test file:

@example maxima
elliptic_e(0.5b0, 2b0)
    4.569923520755739993814727170484775b-1
elliptic_e(1b0, 2b0)
    5.990701173677961037199612461401619b-1
    + 9.311292177217850720981546161504105b-2*%i
elliptic_e(bfloat(%pi/4)-2^-60, 2b0)
    5.990701173677961037199612453785669b-1
elliptic_e(bfloat(%pi/4)+2^-60, 2b0)
    5.990701173677961037199612461401619b-1
    + 7.615950786488135987070764020925057b-28*%i
elliptic_e(1.25b0, 1.25b0)
    8.128653358032092422716608772225758b-1
    + 3.470937738124191989696594617835021b-2*%i
elliptic_e(1b0, bfloat(2^60))
    7.314590396335798406049738501403849b-10
    + 4.935966405857377460710759156379883b+8*%i
elliptic_e(-12.5b0, 180b0)
    -4.125999519503755819299131038784289b-1
    - 1.059940876819606834197430913969843b+2*%i
elliptic_ec(2b0)
    5.990701173677961037199612461401619b-1
    + 5.990701173677961037199612461401619b-1*%i
elliptic_ec(bfloat(1+2^-50))
    9.999999999999919109266443590844445b-1
    + 6.975736996017261475571333796820181b-16*%i
elliptic_ec(bfloat(10^16))
    7.8539816339744831943313788828798b-9
    + 9.999999999999989846512447463871835b+7*%i
@end example

Without the source change, 18 of the 20 cases fail. The two that pass, @code{elliptic_e(0.5b0, 2b0)} and @code{elliptic_e(bfloat(%pi/4)-2^-60, 2b0)}, are before the turning point, where the bigfloat results were already accurate. They still exercise the new code.

@strong{Corrected reference.} The test for bug @uref{https://sourceforge.net/p/maxima/bugs/4352/, #4352} checked @code{elliptic_e(1, 2.0)} against @code{0.09311292177217778*%i+@/0.5990701173677968} with tolerance
m4_mathdot(<<<10^{-15}>>>, <<<10⁻¹⁵>>>)
That reference is an old, inaccurate Maxima result: both parts are off by about
m4_mathcomma(<<<7\times10^{-16}>>>, <<<7×10⁻¹⁶>>>)
and the new, accurate result differs from it by
m4_mathdot(<<<1.04\times10^{-15}>>>, <<<1.04×10⁻¹⁵>>>)
The reference is changed to @code{0.093112921772178507*%i+@/0.5990701173677961}, from mpmath, with the same tolerance. The bigfloat test for @uref{https://sourceforge.net/p/maxima/bugs/4352/, #4352} already had the correct value. The corrected test passes with and without the source change.

@strong{Test layout.} The new tests of both parts are at the end of @code{rtest_elliptic.mac}, so the existing tests keep their numbers. They are problems 452 to 508:

@itemize @bullet
@item
Problem 452 sets @code{fpprec} to 32, and problem 508 restores it. Bigfloat literals are read with the current @code{fpprec}, so the bigfloat references need @code{fpprec : 32} before they are read, not only inside the tests.
@item
Problem 453 defines @code{closeto_relerr}, as in @code{rtest_trig.mac}.
@item
Problems 454 to 507 each check one value, so a failure identifies the value.
@end itemize

@strong{CLISP.} On CLISP, problems 455, 456 and 477 fail, and are expected failures, with problem 448, an earlier @code{elliptic_f} test:

@itemize @bullet
@item
Problems 455 and 456 have relative errors of
m4_mathcomma(<<<4.0 \times 2^{-53}>>>, <<<4.0 × 2⁻⁵³>>>)
against tolerances of
m4_mathdot(<<<2.0001 \times 2^{-53}>>>, <<<2.0001 × 2⁻⁵³>>>)
CLISP's double-float @code{cos} has an absolute error of about
m4_math(<<<3\times10^{-20}>>>, <<<3×10⁻²⁰>>>)
to
m4_math(<<<4\times10^{-20}>>>, <<<4×10⁻²⁰>>>)
near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
which is a relative error of
m4_math(<<<4.5\times10^{-8}>>>, <<<4.5×10⁻⁸>>>)
in
m4_math(<<<\cos(\pi/2 - 2^{-40})>>>, <<<cos (π/2 − 2⁻⁴⁰)>>>)
and
m4_math(<<<1.2\times10^{-4}>>>, <<<1.2×10⁻⁴>>>)
in
m4_math(<<<\cos>>>, <<<cos>>>)
of @code{float(%pi/2)}. With CLISP's values of
m4_mathcomma(<<<\cos\phi>>>, <<<cos φ>>>)
the SBCL build gives CLISP's results, so the error is in the input.
@item
Problem 477, the imaginary part of @code{elliptic_e(float(%pi/4)+@/2^-30, 2.0)}, has a relative error of
m4_mathcomma(<<<3.18 \times 2^{-53}>>>, <<<3.18 × 2⁻⁵³>>>)
against a tolerance of
m4_mathdot(<<<1.0607 \times 2^{-53}>>>, <<<1.0607 × 2⁻⁵³>>>)
@code{bf-rd} ends with @code{(expt an 3/2)}, and CLISP's @code{(expt x 3/2)} has errors of up to about 2.7 ulps, against 1.2 ulps for @code{(* x (sqrt x))}. Its results match
m4_math(<<<(\sqrt x)^3>>>, <<<(√x)³>>>)
bit for bit.
@end itemize

With all the changes, all 508 tests in @code{rtest_elliptic.mac} pass on SBCL, CMUCL, CCL, ECL and GCL, and on CLISP all but the 4 expected failures pass. The full testsuite, with share tests, fails the same 3 tests with and without the change on SBCL: @code{rtest5.mac} problem 80, @code{rtest3.mac} problem 222 and @code{rtestprintf.mac} problem 38.

@section Removing @code{elliptic-e} and @code{elliptic-ec}
@anchor{s2-10}

The double-float functions @code{elliptic-e} and @code{elliptic-ec} duplicated @code{bf-elliptic-e} and @code{bf-elliptic-ec}, which, like the other functions in the @code{bigfloat} package, work on double floats. A further change removes @code{elliptic-e} and @code{elliptic-ec}. The @code{elliptic_e} and @code{elliptic_ec} simplifiers call @code{bf-elliptic-e} and @code{bf-elliptic-ec} for real double-float arguments as well. Nothing else called them.

The only case @code{elliptic-e} handled better was
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
where it used
m4_math(<<<E(\phi\,|\,1) = \sin\phi>>>, <<<E(φ | 1) = sin φ>>>)
(A&S 17.4.25) after reducing
m4_mathdot(<<<\phi>>>, <<<φ>>>)
@code{bf-elliptic-e} used the general formula, in which
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
both diverge as
m4_mathcomma(<<<\cos\phi \to 0>>>, <<<cos φ → 0>>>)
and had errors of up to 128 ulps on 34 points. This change adds the same special case to @code{bf-elliptic-e}.

The double-float results before and after that change, as worst errors in ulps:

@need 2800
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.40 0.12 0.16 0.16 0.16
@headitem
Inputs
@tab
Points
@tab
Before
@tab
After
@tab
Identical
@item
@code{elliptic_e},
m4_mathcomma(<<<m = 0>>>, <<<m = 0>>>)
m4_math(<<<\lvert\phi\rvert < 30>>>, <<<|φ| < 30>>>)
@tab
30
@tab
0
@tab
0
@tab
30
@item
@code{elliptic_e},
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
m4_mathcomma(<<<\lvert\phi\rvert < 30>>>, <<<|φ| < 30>>>)
including
m4_math(<<<\pm\pi/2>>>, <<<±π/2>>>)
and
m4_math(<<<3\pi/2>>>, <<<3π/2>>>)
@tab
34
@tab
0.96
@tab
0.96
@tab
34
@item
@code{elliptic_e},
m4_mathcomma(<<<-10^{10} < m < 0>>>, <<<−10¹⁰ < m < 0>>>)
m4_math(<<<\lvert\phi\rvert < \pi/2>>>, <<<|φ| < π/2>>>)
@tab
60
@tab
3.18
@tab
3.18
@tab
55
@item
@code{elliptic_e},
m4_mathcomma(<<<-10^{5} < m < 0>>>, <<<−10⁵ < m < 0>>>)
m4_math(<<<\lvert\phi\rvert < 50>>>, <<<|φ| < 50>>>)
@tab
40
@tab
3.68
@tab
3.68
@tab
32
@item
@code{elliptic_e},
m4_mathcomma(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
m4_math(<<<\lvert\phi\rvert < 50>>>, <<<|φ| < 50>>>)
@tab
40
@tab
2.63
@tab
2.63
@tab
40
@item
@code{elliptic_e},
m4_mathcomma(<<<1 < m < 10^{10}>>>, <<<1 < m < 10¹⁰>>>)
m4_math(<<<\lvert\phi\rvert < 50>>>, <<<|φ| < 50>>>)
@tab
40
@tab
2.89
@tab
2.89
@tab
40
@item
@code{elliptic_ec},
m4_math(<<<m>>>, <<<m>>>)
in
m4_mathcomma(<<<\lbrace 0, \frac12, 1, 2\rbrace>>>, <<<@{0, ½, 1, 2@}>>>)
m4_math(<<<-10^{10} < m < 10^{50}>>>, <<<−10¹⁰ < m < 10⁵⁰>>>)
@tab
54
@tab
2.78
@tab
2.78
@tab
50
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-2-7}
@noindent
@strong{Table 2.7.} Worst errors in ulps of double-float results before and after removing @code{elliptic-e} and @code{elliptic-ec}, and the number of points with identical results.

@tex
\medskip
@end tex

The results that changed are all for
m4_mathcomma(<<<m < 0>>>, <<<m < 0>>>)
and differ by up to 2 ulps, with the same worst errors. For @code{elliptic_e}, @code{elliptic-e} computed
m4_math(<<<y>>>, <<<y>>>)
as
m4_math(<<<(1 - \sqrt m\,s)(1 + \sqrt m\,s)>>>, <<<(1 − √m s)(1 + √m s)>>>)
in complex arithmetic, with
m4_math(<<<\sqrt m>>>, <<<√m>>>)
imaginary, while @code{bf-elliptic-e} computes
m4_math(<<<1 - ms^2>>>, <<<1 − ms²>>>)
in real arithmetic. For @code{elliptic_ec}, the two functions computed
m4_math(<<<m/3>>>, <<<m/3>>>)
differently.

Double-float @code{elliptic_e} also got faster. Per 1000 calls with
m4_mathcomma(<<<\lvert\phi\rvert < 1.5>>>, <<<|φ| < 1.5>>>)
on SBCL:

@need 1660
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.26 0.33 0.41
@headitem
m4_math(<<<m>>>, <<<m>>>)
@tab
@code{elliptic-e}
@tab
@code{bf-elliptic-e}
@item
m4_math(<<<-100 < m < 0>>>, <<<−100 < m < 0>>>)
@tab
111
@tab
52
@item
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@tab
69
@tab
46
@item
m4_math(<<<1 < m < 101>>>, <<<1 < m < 101>>>)
@tab
39
@tab
42
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-2-8}
@noindent
@strong{Table 2.8.} Time in milliseconds for 1000 calls of double-float @code{elliptic_e} with
m4_mathcomma(<<<\lvert\phi\rvert < 1.5>>>, <<<|φ| < 1.5>>>)
on SBCL, with @code{elliptic-e} and with @code{bf-elliptic-e}.

@tex
\medskip
@end tex

For
m4_math(<<<m > 1>>>, <<<m > 1>>>)
both call @code{bf-elliptic-e-m>1}, so they take about the same time.

With all the changes, the full testsuite fails the same 3 tests as above.

@node Complex m
@chapter Complex Arguments
@anchor{p3}

Here
m4_math(<<<\phi>>>, <<<φ>>>)
or
m4_math(<<<m>>>, <<<m>>>)
is complex. Each part of a result is measured in ulps of that part, and the error of a complex value is the larger of the two. A part whose exact value is 0 must be exactly 0. This is stricter than an error in ulps of the larger part: with that, a small part can be wrong in every digit.

@section Summary of the Change
@anchor{s3-1}

@itemize @bullet
@item
@code{bf-elliptic-e} for complex
m4_math(<<<\phi>>>, <<<φ>>>)
or
m4_mathcomma(<<<m>>>, <<<m>>>)
with
m4_mathcomma(<<<m \ne 1>>>, <<<m ≠1>>>)
and @code{bf-elliptic-ec} for complex
m4_mathcomma(<<<m>>>, <<<m>>>)
still use the formula of @ref{s1-2,,section 1.2}. They also compute, from the sizes of its terms, an estimate of the error of each part. Where that estimate is too large, they recompute with more precision until each part is accurate (@ref{s3-3,,section 3.3}).
@item
The test uses a threshold, @code{*accurate-parts-threshold*}, which is 2 (@ref{s3-4,,section 3.4}).
@item
Real arguments are not affected.
@item
New regression tests, and one existing @code{jacobi_epsilon} test tolerance is loosened (@ref{s3-8,,section 3.8}).
@end itemize

For double floats, the worst error of a part on the test sets went from
m4_math(<<<2.2\times10^{15}>>>, <<<2.2×10¹⁵>>>)
to 6.1 ulps, and every part that was recomputed is within 0.9 ulps (@ref{s3-5,,section 3.5}). The cost is speed: complex double-float arguments are about 9 to 21 times slower (@ref{s3-6,,section 3.6}).

@section The Problem
@anchor{s3-2}

Computed in complex arithmetic, each part of a product or sum has an absolute error of about the rounding unit times the modulus of the operands, not times that part. So each part of
m4_math(<<<E>>>, <<<E>>>)
has an absolute error of about
m4_mathcomma(<<<\varepsilon\,B>>>, <<<ε B>>>)
where
m4_math(<<<B>>>, <<<B>>>)
is the sum of the moduli of the terms of the formula. A part much smaller than
m4_math(<<<B>>>, <<<B>>>)
is inaccurate relative to itself. This happens in three ways:

@itemize @bullet
@item
@strong{Complex @math{m} near 1.} The two terms cancel as in @ref{s1-2,,section 1.2}.
@item
@strong{@math{Re m > 1} near the real axis.} The terms have the problems of @ref{s2-2,,section 2.2}, and one part is small: the real part for large
m4_mathcomma(<<<m>>>, <<<m>>>)
or the imaginary part, which is about
m4_math(<<<\mathrm{Im}\,m>>>, <<<Im m>>>)
times
m4_mathdot(<<<\partial E/\partial m>>>, <<<∂E/∂m>>>)
@item
@strong{General points.} The terms are often several times larger than
m4_mathcomma(<<<E>>>, <<<E>>>)
and for many arguments one part of
m4_math(<<<E>>>, <<<E>>>)
is much smaller than the other.
@end itemize

Double-float examples, with errors in ulps of each part, from mpmath at the exact inputs. These are the double-float tests of @ref{s3-9,,section 3.9}:

@need 3940
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.48 0.14 0.14 0.12 0.12
@headitem
Expression
@tab
Re before
@tab
Im before
@tab
Re after
@tab
Im after
@item
@code{elliptic_e(@/float(%pi/2), float(1-2^-50)-@/2^-51*%i)}
@tab
36
@tab
m4_math(<<<2.0{\times}10^{14}>>>, <<<2.0×10¹⁴>>>)
@tab
1
@tab
0
@item
@code{elliptic_e(@/float(%pi/2)-@/2^-20, float(1-2^-30)+@/2^-31*%i)}
@tab
17
@tab
m4_math(<<<1.4{\times}10^{8}>>>, <<<1.4×10⁸>>>)
@tab
1
@tab
0
@item
@code{elliptic_ec(@/float(1-2^-40)+@/2^-41*%i)}
@tab
7
@tab
m4_math(<<<2.5{\times}10^{11}>>>, <<<2.5×10¹¹>>>)
@tab
1
@tab
0
@item
@code{elliptic_e(@/0.5, 2.0+@/2^-45*%i)}
@tab
2
@tab
1
@tab
0
@tab
0
@item
@code{elliptic_e(@/1.0, 2.0^30+@/2^-10*%i)}
@tab
m4_math(<<<1.3{\times}10^{4}>>>, <<<1.3×10⁴>>>)
@tab
0
@tab
0
@tab
0
@item
@code{elliptic_ec(@/float(1+@/2^-20)+@/2^-60*%i)}
@tab
30
@tab
m4_math(<<<5.8{\times}10^{6}>>>, <<<5.8×10⁶>>>)
@tab
0
@tab
0
@item
@code{elliptic_e(@/float(%pi/2)+@/2^-30*%i, float(1-2^-40))}
@tab
1
@tab
m4_math(<<<4.4{\times}10^{12}>>>, <<<4.4×10¹²>>>)
@tab
1
@tab
0
@item
@code{elliptic_e(@/2.875-1.3125*%i, 3.0625-0.1875*%i)}
@tab
44
@tab
124
@tab
0
@tab
0
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-1}
@noindent
@strong{Table 3.1.} Errors in ulps of each part, before and after the change, for double-float examples with complex arguments.

@tex
\medskip
@end tex

The forms of @ref{p1,,part 1} and @ref{p2,,part 2} do not solve this. For example, the form of @ref{s1-3,,section 1.3} with complex
m4_math(<<<m>>>, <<<m>>>)
near 1 brings the real part to within about 1.5 ulps, but the imaginary parts of its terms cancel. At
m4_mathcomma(<<<\phi = 1.5707963267948966>>>, <<<φ = 1.5707963267948966>>>)
m4_mathcomma(<<<m = 0.999999999999999 - 2.96\times10^{-16}\,i>>>, <<<m = 0.999999999999999 − 2.96×10⁻¹⁶ i>>>)
they are
m4_mathcomma(<<<5.4\times10^{-15}>>>, <<<5.4×10⁻¹⁵>>>)
m4_math(<<<2.7\times10^{-10}>>>, <<<2.7×10⁻¹⁰>>>)
and
m4_mathcomma(<<<-2.7\times10^{-10}>>>, <<<−2.7×10⁻¹⁰>>>)
and their sum is
m4_mathdot(<<<2.6\times10^{-15}>>>, <<<2.6×10⁻¹⁵>>>)
That form also gives wrong values for some other complex arguments, so it would need a region of validity. For real
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_mathcomma(<<<m = a + ib>>>, <<<m = a + ib>>>)

m4_displaymath(
<<<\mathrm{Im}\,E(\phi\,|\,m) = -\frac{b}{2}\int_0^{\phi}\frac{\sin^2\theta}{\mathrm{Re}\sqrt{1 - m\sin^2\theta}}\,d\theta ,>>>,
<<<Im E(φ | m) = −b/2 ∫₀^φsin²θ/(Re√(1 − m sin²θ)) dθ,>>>)

so the imaginary part is well-conditioned, but
m4_math(<<<\mathrm{Re}\sqrt{\cdot}>>>, <<<Re√(·)>>>)
is not analytic, and this has no Carlson form.

@section Recomputing With Extra Precision
@anchor{s3-3}

The inputs are exact, so with enough precision both parts can be computed accurately. As in @code{bf-1-m*sin^2} (@ref{s2-4,,section 2.4}), the code recomputes with extra precision when it detects that this is needed.

@code{bf-elliptic-ec-terms} computes

m4_displaymath(
<<<E(m) = t_1 - t_2, \qquad t_1 = R_F(0, 1-m, 1), \qquad t_2 = \frac{m}{3}\,R_D(0, 1-m, 1),>>>,
<<<E(m) = t₁ − t₂, t₁ = R_F(0, 1−m, 1), t₂ = m/3 R_D(0, 1−m, 1),>>>)

with
m4_mathdot(<<<B = \lvert t_1\rvert + \lvert t_2\rvert>>>, <<<B = |t₁| + |t₂|>>>)
@code{bf-elliptic-e-terms} computes

m4_displaymath(
<<<E(\phi\,|\,m) = t_1 - t_2 + 2k\,E(m), \qquad t_1 = s\,R_F(c^2, y, 1), \qquad t_2 = \frac{m}{3}\,s^3 R_D(c^2, y, 1),>>>,
<<<E(φ | m) = t₁ − t₂ + 2k E(m), t₁ = s R_F(c², y, 1), t₂ = m/3 s³ R_D(c², y, 1),>>>)

where
m4_mathcomma(<<<k = \mathrm{round}(\mathrm{Re}\,\phi/\pi)>>>, <<<k = round(Re φ/π)>>>)
m4_math(<<<s>>>, <<<s>>>)
and
m4_math(<<<c>>>, <<<c>>>)
are the sine and cosine of
m4_mathcomma(<<<\phi - k\pi>>>, <<<φ − kπ>>>)
and
m4_mathcomma(<<<y = 1 - ms^2>>>, <<<y = 1 − ms²>>>)
with
m4_mathdot(<<<B = \lvert t_1\rvert + \lvert t_2\rvert + 2\lvert k\rvert B_{E(m)}>>>, <<<B = |t₁| + |t₂| + 2|k|B_E(m)>>>)
These are the formulas of @ref{s1-2,,section 1.2}.

@code{bf-with-accurate-parts} calls one of them. With working precision
m4_math(<<<p>>>, <<<p>>>)
bits, each part has an absolute error of about
m4_mathdot(<<<B\cdot2^{-p}>>>, <<<B·2^(−p)>>>)
The function:

@enumerate
@item
Keeps the value if, for each part that is not exactly 0,
m4_mathcomma(<<<B \le 2^T\lvert\mathrm{part}\rvert>>>, <<<B ≤ 2^T|part|>>>)
where
m4_math(<<<T>>>, <<<T>>>)
is @code{*accurate-parts-threshold*}. The error of such a part is then estimated at no more than about
m4_math(<<<2^T>>>, <<<2^T>>>)
ulps.
@item
Otherwise recomputes at
m4_math(<<<p + e + 8>>>, <<<p + e + 8>>>)
bits, with the inputs converted to bigfloats. Here
m4_mathcomma(<<<e = \lceil\log_2(B/\lvert\mathrm{smaller\ part}\rvert)\rceil + 8>>>, <<<e = ⌈log₂(B/|smaller part|)⌉ + 8>>>)
or
m4_math(<<<p + 8>>>, <<<p + 8>>>)
if the smaller part is 0.
@item
Keeps that value if
m4_math(<<<B \le 2^{e+7}\lvert\mathrm{part}\rvert>>>, <<<B ≤ 2^(e+7)|part|>>>)
for each part that is not exactly 0. At
m4_math(<<<p + e + 8>>>, <<<p + e + 8>>>)
bits the error of a part is about
m4_mathcomma(<<<B\cdot2^{-(p+e+8)}>>>, <<<B·2^(−(p+e+8))>>>)
and half an ulp of a
m4_math(<<<p>>>, <<<p>>>)
-bit part is at least
m4_mathdot(<<<\lvert\mathrm{part}\rvert\cdot2^{-(p+1)}>>>, <<<|part|·2^(−(p+1))>>>)
@item
Otherwise doubles
m4_math(<<<e>>>, <<<e>>>)
and goes back to step 3. Once
m4_math(<<<e>>>, <<<e>>>)
exceeds
m4_mathcomma(<<<16p>>>, <<<16p>>>)
it keeps the value it has.
@item
Rounds the value to the precision of the inputs.
@end enumerate

A part that comes out exactly 0, such as the real part for purely imaginary
m4_math(<<<\phi>>>, <<<φ>>>)
and real
m4_mathcomma(<<<m>>>, <<<m>>>)
is accepted as it is. Without that, the loop would never accept it, and would run to its limit.

m4_math(<<<B>>>, <<<B>>>)
is an estimate, not a bound. The values kept in step 1 have errors of up to about 6 ulps on the test sets, not
m4_mathdot(<<<2^T>>>, <<<2^T>>>)

@section The Threshold
@anchor{s3-4}

A larger
m4_math(<<<T>>>, <<<T>>>)
recomputes less, but keeps values with larger errors. On the 270 points of @ref{s3-5,,section 3.5}:

@need 2040
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.10 0.22 0.26 0.24 0.18
@headitem
m4_math(<<<T>>>, <<<T>>>)
@tab
Recomputed
@tab
Worst, random
@tab
Worst,
m4_math(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
near the axis
@tab
Time
@item
1
@tab
255
@tab
1.9
@tab
0.89
@tab
0.46 s
@item
2
@tab
232
@tab
4.3
@tab
0.89
@tab
0.37 s
@item
3
@tab
207
@tab
7.9
@tab
5.2
@tab
0.32 s
@item
4
@tab
181
@tab
7.9
@tab
23
@tab
0.29 s
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-2}
@noindent
@strong{Table 3.2.} Effect of the threshold
m4_math(<<<T>>>, <<<T>>>)
on the 270 points of Table 3.5: how many points are recomputed, the worst errors in ulps of the values kept without recomputing, and the time for all 270 points.

@tex
\medskip
@end tex

Before the change, these points took 0.04 s. Many of them were chosen because they are hard. On 400 random points in each of three general sets, with
m4_mathcomma(<<<\lvert\mathrm{Re}\,\phi\rvert \le 3>>>, <<<|Re φ| ≤ 3>>>)
m4_mathcomma(<<<\lvert\mathrm{Im}\,\phi\rvert \le 1>>>, <<<|Im φ| ≤ 1>>>)
m4_math(<<<-3 \le \mathrm{Re}\,m \le 4>>>, <<<−3 ≤ Re m ≤ 4>>>)
and
m4_math(<<<\lvert\mathrm{Im}\,m\rvert \le 2\colon>>>, <<<|Im m| ≤ 2:>>>)

@need 2800
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.10 0.30 0.30 0.30
@headitem
m4_math(<<<T>>>, <<<T>>>)
@tab
Complex
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
real
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@tab
Real
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
complex
m4_math(<<<m>>>, <<<m>>>)
@tab
Complex
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
@item
2
@tab
281 recomputed, 7.75 ulps
@tab
393 recomputed, 5.13 ulps
@tab
263 recomputed, 8.13 ulps
@item
4
@tab
121 recomputed, 32.9 ulps
@tab
158 recomputed, 22.1 ulps
@tab
79 recomputed, 28.0 ulps
@item
6
@tab
38 recomputed, 32.9 ulps
@tab
58 recomputed, 22.1 ulps
@tab
20 recomputed, 52.6 ulps
@item
8
@tab
10 recomputed, 49.1 ulps
@tab
36 recomputed, 22.1 ulps
@tab
5 recomputed, 181 ulps
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-3}
@noindent
@strong{Table 3.3.} Effect of
m4_math(<<<T>>>, <<<T>>>)
on three sets of 400 random points. Each entry gives how many points are recomputed, and the worst error in ulps of the values kept without recomputing.

@tex
\medskip
@end tex

The errors are the worst errors of the values kept without recomputing.
m4_math(<<<T = 2>>>, <<<T = 2>>>)
keeps errors comparable to those for real arguments, so it is the default.

Most of the recomputing is not caused by the formula. Since
m4_mathcomma(<<<B \ge \lvert E\rvert>>>, <<<B ≥ |E|>>>)
a part smaller than
m4_math(<<<\lvert E\rvert/2^T>>>, <<<|E|/2^T>>>)
always fails the test, even when nothing cancels. That is common:

@need 2040
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.46 0.27 0.27
@headitem
Inputs, 400 points each
@tab
@code{elliptic_e}
@tab
@code{elliptic_ec}
@item
Complex
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
real
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@tab
184
@tab
0
@item
Real
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
complex
m4_math(<<<m>>>, <<<m>>>)
@tab
247
@tab
188
@item
Complex
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
@tab
128
@tab
193
@item
Complex
m4_math(<<<m>>>, <<<m>>>)
near 1
@tab
400
@tab
400
@item
m4_mathcomma(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
m4_math(<<<\mathrm{Im}\,m>>>, <<<Im m>>>)
tiny
@tab
332
@tab
387
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-4}
@noindent
@strong{Table 3.4.} Number of points, of 400, where one part of the value is smaller than a quarter of its modulus.

@tex
\medskip
@end tex

Using the form with the smallest
m4_math(<<<B>>>, <<<B>>>)
of those of @uref{https://dlmf.nist.gov/19.25.E9, DLMF 19.25.9}, @uref{https://dlmf.nist.gov/19.25.E10, 19.25.10} and @uref{https://dlmf.nist.gov/19.25.E11, 19.25.11}, where it agreed with 19.25.9, was also tried. It reduced the recomputing by at most 21 of 400 points, and the values it kept had errors of up to 12.1 ulps.

@section Accuracy
@anchor{s3-5}

The test sets are 270 points for @code{elliptic_e} and 78 for @code{elliptic_ec}. References are Maxima's bigfloat results at @code{fpprec : 60}, which agree with mpmath to all of 40 digits compared. Errors below are the worst errors in ulps of each part, and in ulps of the larger part:

@need 3370
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.40 0.10 0.125 0.125 0.125 0.125
@headitem
@code{elliptic_e} inputs
@tab
Points
@tab
Parts before
@tab
Parts after
@tab
Whole before
@tab
Whole after
@item
Complex
m4_math(<<<m>>>, <<<m>>>)
within
m4_math(<<<10^{-4}>>>, <<<10⁻⁴>>>)
to
m4_math(<<<10^{-15}>>>, <<<10⁻¹⁵>>>)
of 1,
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_math(<<<\pi/2>>>, <<<π/2>>>)
@tab
60
@tab
m4_math(<<<1.1{\times}10^{15}>>>, <<<1.1×10¹⁵>>>)
@tab
0.74
@tab
129
@tab
0.74
@item
m4_mathcomma(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
m4_math(<<<\mathrm{Im}\,m/\mathrm{Re}\,m>>>, <<<Im m/Re m>>>)
from
m4_math(<<<10^{-14}>>>, <<<10⁻¹⁴>>>)
to
m4_mathcomma(<<<10^{-3}>>>, <<<10⁻³>>>)
real
m4_math(<<<\phi>>>, <<<φ>>>)
@tab
120
@tab
m4_math(<<<7.2{\times}10^{6}>>>, <<<7.2×10⁶>>>)
@tab
0.89
@tab
7.5
@tab
0.89
@item
m4_math(<<<\lvert m\rvert>>>, <<<|m|>>>)
from
m4_math(<<<10^2>>>, <<<10²>>>)
to
m4_math(<<<10^{30}>>>, <<<10³⁰>>>)
@tab
15
@tab
5.6
@tab
3.7
@tab
4.1
@tab
2.5
@item
Complex
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
real
m4_math(<<<m>>>, <<<m>>>)
@tab
20
@tab
63
@tab
6.1
@tab
4.5
@tab
5.1
@item
Complex
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
m4_math(<<<m>>>, <<<m>>>)
near 1
@tab
15
@tab
m4_math(<<<2.2{\times}10^{15}>>>, <<<2.2×10¹⁵>>>)
@tab
0.84
@tab
40.5
@tab
0.84
@item
Random,
m4_mathcomma(<<<\lvert\mathrm{Re}\,\phi\rvert \le 4>>>, <<<|Re φ| ≤ 4>>>)
m4_mathcomma(<<<\lvert\mathrm{Im}\,\phi\rvert \le 1.5>>>, <<<|Im φ| ≤ 1.5>>>)
m4_mathcomma(<<<-3 \le \mathrm{Re}\,m \le 4>>>, <<<−3 ≤ Re m ≤ 4>>>)
m4_math(<<<\lvert\mathrm{Im}\,m\rvert \le 2>>>, <<<|Im m| ≤ 2>>>)
@tab
40
@tab
m4_math(<<<9.9{\times}10^{3}>>>, <<<9.9×10³>>>)
@tab
4.3
@tab
33.1
@tab
4.7
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-5}
@noindent
@strong{Table 3.5.} Worst errors in ulps of double-float @code{elliptic_e} for complex arguments: of each part, in ulps of that part, and of the whole value, in ulps of the larger part.

@tex
\medskip
@end tex

@need 2040
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.52 0.12 0.18 0.18
@headitem
@code{elliptic_ec} inputs
@tab
Points
@tab
Parts before
@tab
Parts after
@item
Complex
m4_math(<<<m>>>, <<<m>>>)
within
m4_math(<<<10^{-3}>>>, <<<10⁻³>>>)
to
m4_math(<<<10^{-15}>>>, <<<10⁻¹⁵>>>)
of 1
@tab
25
@tab
m4_math(<<<3.4{\times}10^{14}>>>, <<<3.4×10¹⁴>>>)
@tab
0.84
@item
m4_math(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
near the real axis
@tab
18
@tab
m4_math(<<<1.4{\times}10^{6}>>>, <<<1.4×10⁶>>>)
@tab
0.65
@item
Large
m4_mathcomma(<<<\lvert m\rvert>>>, <<<|m|>>>)
and random
@tab
35
@tab
15
@tab
4.7
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-6}
@noindent
@strong{Table 3.6.} Worst errors of each part of double-float @code{elliptic_ec} for complex
m4_mathcomma(<<<m>>>, <<<m>>>)
in ulps of that part.

@tex
\medskip
@end tex

Every part that was recomputed is within 0.9 ulps. The errors that remain are in values kept without recomputing.

For bigfloats at @code{fpprec : 32}, on 15 of the same points, the recomputed values are within 0.44 ulps, and the others are unchanged, with errors of up to 7.0 ulps.

For
m4_mathcomma(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
the real axis is a branch cut. The value for real
m4_math(<<<m>>>, <<<m>>>)
is the limit as
m4_math(<<<\mathrm{Im}\,m \to 0>>>, <<<Im m → 0>>>)
from below. From above, the imaginary part has the opposite sign. For example, @code{elliptic_ec(2.0)} is
m4_mathcomma(<<<0.599 + 0.599\,i>>>, <<<0.599 + 0.599 i>>>)
and @code{elliptic_ec(2.0+1e-15*%i)} is
m4_mathdot(<<<0.599 - 0.599\,i>>>, <<<0.599 − 0.599 i>>>)
mpmath does the same.

@section Speed
@anchor{s3-6}

Tables 3.7 and 3.8 use the same sets of 400 random double-float points, with
m4_math(<<<m>>>, <<<m>>>)
taken from each point for @code{elliptic_ec}.

@need 2990
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.38 0.17 0.17 0.28
@headitem
Inputs
@tab
Before
@tab
After
@tab
Recomputed
@item
Real
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@tab
56
@tab
42
@tab
0
@item
Real
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
m4_math(<<<m < 0>>>, <<<m < 0>>>)
@tab
52
@tab
38
@tab
0
@item
Real
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
m4_math(<<<m > 1>>>, <<<m > 1>>>)
@tab
56
@tab
52
@tab
0
@item
Complex
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
real
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@tab
104
@tab
1306
@tab
281
@item
Real
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
complex
m4_math(<<<m>>>, <<<m>>>)
(first set)
@tab
144
@tab
2014
@tab
393
@item
Complex
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
(second set)
@tab
134
@tab
1738
@tab
263
@item
Complex
m4_math(<<<m>>>, <<<m>>>)
near 1
@tab
202
@tab
1858
@tab
400
@item
m4_mathcomma(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
m4_math(<<<\mathrm{Im}\,m>>>, <<<Im m>>>)
tiny
@tab
170
@tab
1578
@tab
400
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-7}
@noindent
@strong{Table 3.7.} Time per call in microseconds of double-float @code{elliptic_e} on SBCL, before and after the change, averaged over 400 random points and 5 passes, and how many of the 400 points needed extra precision.

@tex
\medskip
@end tex

@need 2800
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.38 0.17 0.17 0.28
@headitem
Inputs
@tab
Before
@tab
After
@tab
Recomputed
@item
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@tab
26
@tab
20
@tab
0
@item
m4_math(<<<m < 0>>>, <<<m < 0>>>)
@tab
24
@tab
18
@tab
0
@item
m4_math(<<<m > 1>>>, <<<m > 1>>>)
@tab
28
@tab
28
@tab
0
@item
Complex
m4_mathcomma(<<<m>>>, <<<m>>>)
random, first set
@tab
68
@tab
1120
@tab
341
@item
Complex
m4_mathcomma(<<<m>>>, <<<m>>>)
random, second set
@tab
66
@tab
1072
@tab
335
@item
Complex
m4_math(<<<m>>>, <<<m>>>)
near 1
@tab
90
@tab
1928
@tab
400
@item
m4_mathcomma(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
m4_math(<<<\mathrm{Im}\,m>>>, <<<Im m>>>)
tiny
@tab
76
@tab
1510
@tab
400
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-8}
@noindent
@strong{Table 3.8.} Time per call in microseconds of double-float @code{elliptic_ec} on SBCL, for the values of
m4_math(<<<m>>>, <<<m>>>)
in the sets of Table 3.7, and how many of the 400 points needed extra precision.

@tex
\medskip
@end tex

Real arguments take the same path as before, and their differences are within the scatter. Complex
m4_math(<<<m>>>, <<<m>>>)
near 1 always needs extra precision, since one part of
m4_math(<<<E>>>, <<<E>>>)
is always much smaller than the other there.

The extra precision is the
m4_math(<<<e + 8>>>, <<<e + 8>>>)
bits of @ref{s3-3,,section 3.3}, beyond the 53 bits of a double float. For the points that were recomputed:

@need 2420
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.40 0.30 0.30
@headitem
Inputs
@tab
@code{elliptic_e}
@tab
@code{elliptic_ec}
@item
Complex
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
real
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@tab
19 to 27, median 20
@tab

@item
Real
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
complex
m4_math(<<<m>>>, <<<m>>>)
(first set)
@tab
19 to 35, median 20
@tab
19 to 26, median 19
@item
Complex
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
(second set)
@tab
19 to 27, median 20
@tab
19 to 29, median 19
@item
Complex
m4_math(<<<m>>>, <<<m>>>)
near 1
@tab
29 to 72, median 50
@tab
29 to 71, median 49
@item
m4_mathcomma(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
m4_math(<<<\mathrm{Im}\,m>>>, <<<Im m>>>)
tiny
@tab
19 to 74, median 21
@tab
19 to 24, median 23
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-9}
@noindent
@strong{Table 3.9.} Extra bits, beyond the 53 of a double float, used for the points of Tables 3.7 and 3.8 that were recomputed.

@tex
\medskip
@end tex

The first estimate of the extra precision was always enough, so the loop never doubled it. For general complex arguments, about 20 extra bits are enough, and the cost of a recomputed value, about 1.5 to 2 ms, is the cost of computing in bigfloats at all, not of the number of bits.

@section Comparison With mpmath
@anchor{s3-7}

For @code{ellipe(phi, m)}, mpmath uses the same formula, with
m4_math(<<<\phi>>>, <<<φ>>>)
reduced by
m4_mathdot(<<<\mathrm{round}(\mathrm{Re}\,\phi/\pi)\,\pi>>>, <<<round(Re φ/π) π>>>)
It sums the two terms with @code{sum_accurately}, which works at 15 extra bits and adds precision while the logarithm of the largest term's modulus exceeds that of the sum's modulus by the extra precision or more. So it makes the whole value accurate, not each part. It adds the term
m4_math(<<<2k\,E(m)>>>, <<<2k E(m)>>>)
outside that check. For @code{ellipe(m)}, it uses
m4_mathcomma(<<<E(m) = (1-m)\bigl(K(m) + 2m\,K'(m)\bigr)>>>, <<<E(m) = (1−m)(K(m) + 2m K'(m))>>>)
with
m4_math(<<<K>>>, <<<K>>>)
from the arithmetic-geometric mean and
m4_math(<<<K'>>>, <<<K'>>>)
from a finite difference.

At 53 bits, mpmath's small parts can be inaccurate. Errors in ulps of each part, against mpmath at 200 bits:

@need 2800
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.38 0.17 0.17 0.28
@headitem
Input
@tab
Re
@tab
Im
@tab
Whole
@item
@code{ellipe(phi, m)}, complex
m4_math(<<<m>>>, <<<m>>>)
near 1,
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
3 points
@tab
0.05 to 0.15
@tab
m4_math(<<<1.5{\times}10^{4}>>>, <<<1.5×10⁴>>>)
to
m4_math(<<<2.4{\times}10^{6}>>>, <<<2.4×10⁶>>>)
@tab
0.05 to 0.15
@item
@code{ellipe(phi, m)}, complex
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
m4_math(<<<m>>>, <<<m>>>)
near 1
@tab
0.35
@tab
m4_math(<<<1.1{\times}10^{6}>>>, <<<1.1×10⁶>>>)
@tab
0.35
@item
@code{ellipe(m)},
m4_math(<<<m = 0.999999 + 8.4\times10^{-7}\,i>>>, <<<m = 0.999999 + 8.4×10⁻⁷ i>>>)
@tab
0.03
@tab
m4_math(<<<3.3{\times}10^{3}>>>, <<<3.3×10³>>>)
@tab
0.03
@item
@code{ellipe(m)},
m4_math(<<<m = 1 + 8.4\times10^{-10}\,i>>>, <<<m = 1 + 8.4×10⁻¹⁰ i>>>)
@tab
4.3
@tab
m4_math(<<<1.7{\times}10^{9}>>>, <<<1.7×10⁹>>>)
@tab
7.6
@item
Other test points
@tab
up to 0.44
@tab
up to 1.56
@tab
up to 0.37
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-10}
@noindent
@strong{Table 3.10.} Errors in ulps of mpmath's @code{ellipe} at 53 bits, against mpmath at 200 bits: of each part, and of the whole value.

@tex
\medskip
@end tex

The references in this part were computed at 40 digits or more, where these errors are far below double precision.

@section Effect on @code{jacobi_epsilon}
@anchor{s3-8}

@code{jacobi_epsilon} for complex arguments calls @code{bf-elliptic-e} and @code{bf-elliptic-ec} with complex arguments, so this part changes it. For @code{jacobi_epsilon(2.75-2.25*%i, 3.0+@/0.5*%i)}, it calls @code{bf-elliptic-ec} with
m4_math(<<<3 + 0.5\,i>>>, <<<3 + 0.5 i>>>)
and
m4_mathdot(<<<-2 - 0.5\,i>>>, <<<−2 − 0.5 i>>>)
Before the change, those values were off by a few ulps, and after it they are correct to the last digit. But the error of the result went from
m4_math(<<<1.2\times10^{-16}>>>, <<<1.2×10⁻¹⁶>>>)
in the real part and
m4_math(<<<7\times10^{-18}>>>, <<<7×10⁻¹⁸>>>)
in the imaginary part to
m4_math(<<<5.7\times10^{-16}>>>, <<<5.7×10⁻¹⁶>>>)
and
m4_mathcomma(<<<2.7\times10^{-15}>>>, <<<2.7×10⁻¹⁵>>>)
because the old errors happened to offset other errors, as in @ref{s1-8,,section 1.8}. The double-float tolerance of that test goes from @code{1.2948e-15} to @code{2.9206e-15}, which also covers CLISP, where the error is
m4_mathdot(<<<2.9205\times10^{-15}>>>, <<<2.9205×10⁻¹⁵>>>)

On 150 random points with
m4_mathcomma(<<<\lvert\mathrm{Re}\,u\rvert \le 8>>>, <<<|Re u| ≤ 8>>>)
m4_mathcomma(<<<\lvert\mathrm{Im}\,u\rvert \le 4>>>, <<<|Im u| ≤ 4>>>)
m4_math(<<<-4 \le \mathrm{Re}\,m \le 4>>>, <<<−4 ≤ Re m ≤ 4>>>)
and
m4_mathcomma(<<<\lvert\mathrm{Im}\,m\rvert \le 2>>>, <<<|Im m| ≤ 2>>>)
against Maxima's bigfloat @code{jacobi_epsilon} at @code{fpprec : 60}, the median error of a part went from 7.8 to 6.8 ulps, and the mean from 43.6 to 39.1. The change made 60 points more accurate and 42 less. @code{jacobi_epsilon} has large errors of its own for complex arguments, up to 2700 ulps in a part and 276 ulps as a whole, both before and after the change. They are not part of this change.

@section Tests
@anchor{s3-9}

The new tests are at the end of @code{tests/rtest_elliptic.mac}, after those of @ref{p1,,part 1} and @ref{p2,,part 2}, with @code{fpprec} set to 32 around them and their own copy of @code{closeto_relerr}. References are from mpmath at 90 digits, at the exact inputs. Each part is checked separately, with a relative-error tolerance written as a multiple of
m4_math(<<<2^{-53}>>>, <<<2⁻⁵³>>>)
or
m4_math(<<<2^{-109}\colon>>>, <<<2⁻¹⁰⁹:>>>)
the error observed in testing, rounded up to five significant digits, or
m4_math(<<<1.0>>>, <<<1.0>>>)
times
m4_math(<<<2^{-53}>>>, <<<2⁻⁵³>>>)
or
m4_math(<<<2^{-109}>>>, <<<2⁻¹⁰⁹>>>)
where the error was 0.

Double-float tests, with the double-float examples of @ref{s3-2,,section 3.2} and the purely imaginary
m4_math(<<<\phi>>>, <<<φ>>>)
case:

@need 4130
@tex
\bigskip
@end tex
@raggedright
@multitable @columnfractions 0.50 0.50
@headitem
Expression
@tab
What it tests
@item
@code{elliptic_e(@/float(%pi/2), float(1-2^-50)-@/2^-51*%i)}
@tab
Complex
m4_math(<<<m>>>, <<<m>>>)
near 1
@item
@code{elliptic_e(@/float(%pi/2)-@/2^-20, float(1-2^-30)+@/2^-31*%i)}
@tab
Complex
m4_math(<<<m>>>, <<<m>>>)
near 1,
m4_math(<<<\phi>>>, <<<φ>>>)
below
m4_math(<<<\pi/2>>>, <<<π/2>>>)
@item
@code{elliptic_ec(@/float(1-2^-40)+@/2^-41*%i)}
@tab
@code{elliptic_ec}, complex
m4_math(<<<m>>>, <<<m>>>)
near 1
@item
@code{elliptic_e(@/0.5, 2.0+@/2^-45*%i)}
@tab
m4_mathcomma(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
before the turning point
@item
@code{elliptic_e(@/1.0, 2.0^30+@/2^-10*%i)}
@tab
m4_mathcomma(<<<\mathrm{Re}\,m > 1>>>, <<<Re m > 1>>>)
large
m4_math(<<<m>>>, <<<m>>>)
@item
@code{elliptic_ec(@/float(1+@/2^-20)+@/2^-60*%i)}
@tab
@code{elliptic_ec},
m4_math(<<<\mathrm{Re}\,m>>>, <<<Re m>>>)
just above 1
@item
@code{elliptic_e(@/float(%pi/2)+@/2^-30*%i, float(1-2^-40))}
@tab
Complex
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
m4_math(<<<m>>>, <<<m>>>)
near 1
@item
@code{elliptic_e(@/2.875-1.3125*%i, 3.0625-0.1875*%i)}
@tab
Complex
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
@item
@code{elliptic_e(@/0.75*%i, 0.5)}
@tab
Purely imaginary
m4_math(<<<\phi\colon>>>, <<<φ:>>>)
the real part is exactly 0
@end multitable
@end raggedright

@tex
\nobreak
@end tex
@anchor{tab-3-11}
@noindent
@strong{Table 3.11.} Double-float tests added for complex arguments.

@tex
\medskip
@end tex

The bigfloat tests are the same cases with bigfloat arguments, with the perturbations made smaller to suit 109 bits, such as @code{elliptic_e(bfloat(%pi/2), bfloat(1-2^-100)-2^-101*%i)}.

Without the code change, 28 of the 34 checks fail. Of the other 6, 4 are for purely imaginary
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
and 2 are parts the old code already had within the tolerance.

With this part, all 545 tests in @code{rtest_elliptic.mac} pass on SBCL, and on CLISP all but the 4 expected failures of @ref{s2-9,,section 2.9} pass. The full testsuite, with share tests, fails the same 3 tests as before on SBCL.

@node References
@chapter References
@anchor{refs}

@enumerate
@item
NIST Digital Library of Mathematical Functions, @uref{https://dlmf.nist.gov/19.25.i, §19.25(i)}, eqs. @uref{https://dlmf.nist.gov/19.25.E1, 19.25.1}, @uref{https://dlmf.nist.gov/19.25.E9, 19.25.9}, @uref{https://dlmf.nist.gov/19.25.E10, 19.25.10} and @uref{https://dlmf.nist.gov/19.25.E11, 19.25.11}; @uref{https://dlmf.nist.gov/19.16.i, §19.16(i)} for the definitions of
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_mathdot(<<<R_D>>>, <<<R_D>>>)
@item
B. C. Carlson, ``Numerical computation of real or complex elliptic integrals,'' @emph{Numerical Algorithms} 10 (1995), 13--26, @uref{https://doi.org/10.1007/BF02198293, doi:10.1007/BF02198293}; preprint @uref{https://arxiv.org/abs/math/9409227, arXiv:math/9409227}. The algorithms for
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_mathcomma(<<<R_D>>>, <<<R_D>>>)
and their homogeneity.
@item
F. Johansson et al., @uref{https://mpmath.org/, mpmath}, a Python library for arbitrary-precision arithmetic. Its @uref{https://mpmath.org/doc/current/functions/elliptic.html, elliptic functions} @code{ellipe}, @code{elliprf} and @code{elliprd} were used for the reference values.
@end enumerate

@bye
