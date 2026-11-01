\input texinfo
@c -*- mode: texinfo -*-
@c Created by Claude to describe how Maxima evaluates jacobi_sn,
@c jacobi_cn and jacobi_dn for large arguments (bug #5726).
@c
@c Generated from jacobi-sn-large-args.md.  This is the canonical
@c documentation of the implementation at the time of writing.
@c
@setfilename jacobi-sn-large-args.info
@settitle Jacobi sn, cn and dn for Large Arguments
@documentencoding UTF-8

@c Definitions so that plain TeX accepts the same formulas as MathJax.
@tex
\gdef\lvert{\vert}
\gdef\rvert{\vert}
\gdef\mathrm#1{{\rm #1}}
\gdef\text#1{\hbox{#1}}
\gdef\frac#1#2{{#1\over #2}}
\gdef\lesssim{\mathrel{\mathpalette\simrel<}}
\gdef\gtrsim{\mathrel{\mathpalette\simrel>}}
\gdef\simrel#1#2{\vcenter{\offinterlineskip\halign{$#1##$\cr#2\cr\noalign{\kern1pt}\sim\cr}}}
@end tex

@c Styles for the HTML output: table borders, and room for wide tables
@c and code on narrow screens.
@html
<style>
table.multitable { display: block; overflow-x: auto; border-collapse: collapse; }
table.multitable th, table.multitable td { border: 1px solid #999; padding: 0.25em 0.75em; }
pre.example-preformatted { overflow-x: auto; border: 1px solid #ccc; padding: 0.5em 1em; }
</style>
@end html

@ifnottex
@node Top
@top Jacobi sn, cn and dn for Large Arguments
@end ifnottex

@iftex
@titlepage
@title Jacobi sn, cn and dn for Large Arguments
@end titlepage
@end iftex

This note compares the ways we tried to fix @code{jacobi_sn},
@code{jacobi_cn} and @code{jacobi_dn} for large arguments in
@file{src/ellipt.lisp}. The problem was found while working on
@code{elliptic_eu} (bug #5715) and fixed for bug #5726. The conclusion
is to halve the argument and use the duplication formulas, only when the
argument is large, with a correction after each doubling and, when
m4_mathcomma(<<<\mathrm{Re}\ m > 1/2>>>, <<<Re m > 1/2>>>)
a change of modulus to
m4_mathdot(<<<1-m>>>, <<<1−m>>>)

@menu
* The Problem::
* The Cause::
* Argument Reduction::
* Duplication Formula::
* Cost::
* Bigfloats::
* Code Structure::
* Conclusion::
* References::
@end menu

@node The Problem
@chapter The Problem

For double floats, sn, cn and dn give garbage or signal
@code{FLOATING-POINT-OVERFLOW} when
m4_math(<<<u>>>, <<<u>>>)
is large and either
m4_math(<<<u>>>, <<<u>>>)
or
m4_math(<<<m>>>, <<<m>>>)
is complex. Examples from before any fix:

@raggedright
@multitable @columnfractions 0.45 0.55
@headitem
Input
@tab
Result
@item
@code{jacobi_sn(3000.0, 0.5+0.5*%i)}
@tab
about
m4_mathpunc(<<<;>>>, <<<7.7\times 10^{-60}>>>, <<<7.7 × 10⁻⁶⁰>>>)
correct is
m4_math(<<<-1.2185+0.3331i>>>, <<<−1.2185+0.3331i>>>)
@item
@code{jacobi_sn(5000.0, 0.5+0.5*%i)}
@tab
Lisp error
@item
@code{jacobi_sn(700.0*%i, 0.5)}
@tab
Lisp error
@item
@code{jacobi_sn(700.0*%i, 3.0)}
@tab
Lisp error
@item
@code{jacobi_sn(700.0*%i, -3.0)}
@tab
Lisp error
@end multitable
@end raggedright

It is not only complex
m4_mathdot(<<<m>>>, <<<m>>>)
Real
m4_math(<<<m>>>, <<<m>>>)
with a large imaginary
m4_math(<<<u>>>, <<<u>>>)
fails too, and so do the
m4_math(<<<m > 1>>>, <<<m > 1>>>)
and
m4_math(<<<m < 0>>>, <<<m < 0>>>)
branches, which transform to
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
and keep a complex argument.

Bigfloats did not fail: @code{jacobi_sn(3000b0, 0.5b0+0.5b0*%i)} is
correct at @code{fpprec:32}.

@node The Cause
@chapter The Cause

The descending Landen transformation divides the argument by
m4_math(<<<1+\sqrt{\mu}>>>, <<<1+√μ>>>)
at each step and ends with
m4_math(<<<\sin v>>>, <<<sin v>>>)
and
m4_mathcomma(<<<\cos v>>>, <<<cos v>>>)
where

m4_displaymath(
<<<v = \frac{u}{\prod (1+\sqrt{\mu_n})} = \frac{\pi u}{2K(m)},>>>,
<<<@example
v = u/∏(1+√μₙ) = πu/(2K(m)),
@end example>>>)

since
m4_mathdot(<<<K(m) = \frac{\pi}{2}\prod(1+\sqrt{\mu_n})>>>, <<<K(m) = (π/2)∏(1+√μₙ)>>>)
The next step squares
m4_mathcomma(<<<\sin v>>>, <<<sin v>>>)
and
m4_mathcomma(<<<\lvert\sin v\rvert \approx e^{\lvert\mathrm{Im}\ v\rvert}/2>>>, <<<|sin v| ≈ e^|Im v|/2>>>)
so with doubles
m4_math(<<<s^2>>>, <<<s²>>>)
overflows once
m4_math(<<<\lvert\mathrm{Im}\ v\rvert>>>, <<<|Im v|>>>)
exceeds about
m4_mathdot(<<<\frac{1}{2}\log(\text{most-positive-double-float}) \approx 355>>>, <<<(1/2) log(most-positive-double-float) ≈ 355>>>)
For larger arguments @code{sin} itself overflows. For complex
m4_math(<<<m>>>, <<<m>>>)
the factor
m4_math(<<<\pi/(2K(m))>>>, <<<π/(2K(m))>>>)
is complex, so even a real
m4_math(<<<u>>>, <<<u>>>)
gives
m4_math(<<<v>>>, <<<v>>>)
a large imaginary part. For
m4_math(<<<m = 0.5+0.5i>>>, <<<m = 0.5+0.5i>>>)
the factor is
m4_mathcomma(<<<0.8939-0.1699i>>>, <<<0.8939−0.1699i>>>)
and the real-axis failure starts near
m4_mathcomma(<<<u \approx 355/0.1699 \approx 2090>>>, <<<u ≈ 355/0.1699 ≈ 2090>>>)
which is what we saw.

Bigfloats have no exponent limit, so they never overflow.

@node Argument Reduction
@chapter Argument Reduction

The first fix reduces
m4_math(<<<u>>>, <<<u>>>)
by the periods before the Landen transformation.

@menu
* Reducing Always::
* Reducing Only Large Arguments::
@end menu

@node Reducing Always
@section Reducing Always

Reduce
m4_math(<<<u>>>, <<<u>>>)
to
m4_mathcomma(<<<w = u - j\cdot 2K(m) - k\cdot 2iK(1-m)>>>, <<<w = u − j·2K(m) − k·2iK(1−m)>>>)
with
m4_math(<<<j>>>, <<<j>>>)
and
m4_math(<<<k>>>, <<<k>>>)
the rounded lattice coordinates of
m4_math(<<<u>>>, <<<u>>>)
(below,
m4_mathpunc(<<<)>>>, <<<K' = K(1-m)>>>, <<<K' = K(1−m)>>>)
then fix the signs with (A&S 16.8.1,
@uref{https://dlmf.nist.gov/22.4.iii, DLMF §22.4(iii)}):

@raggedright
@multitable @columnfractions 0.2 0.4 0.4
@headitem
@tab
shift by
m4_math(<<<2K(m)>>>, <<<2K(m)>>>)
@tab
shift by
m4_math(<<<2iK(1-m)>>>, <<<2iK(1−m)>>>)
@item
sn
@tab
m4_math(<<<-\mathrm{sn}>>>, <<<−sn>>>)
@tab
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
@item
cn
@tab
m4_math(<<<-\mathrm{cn}>>>, <<<−cn>>>)
@tab
m4_math(<<<-\mathrm{cn}>>>, <<<−cn>>>)
@item
dn
@tab
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
@tab
m4_math(<<<-\mathrm{dn}>>>, <<<−dn>>>)
@end multitable
@end raggedright

We used the
m4_mathcomma(<<<2K>>>, <<<2K>>>)
m4_math(<<<2iK'>>>, <<<2iK'>>>)
lattice rather than the common periods
m4_mathcomma(<<<4K>>>, <<<4K>>>)
m4_mathdot(<<<4iK'>>>, <<<4iK'>>>)
With the larger lattice,
m4_math(<<<w>>>, <<<w>>>)
can land near
m4_mathcomma(<<<\pm 2iK'>>>, <<<±2iK'>>>)
where the Landen intermediates are huge and cancel.

This fixed every failing case, with errors at the level the
conditioning allows. Its drawback is that it also changes results for
small arguments: seven existing @code{rtest_elliptic} tolerances had to
be raised. For example, @code{jacobi_sn(20.0*%i, 0.75)} went from
m4_math(<<<3.9\times 10^{-15}>>>, <<<3.9 × 10⁻¹⁵>>>)
to
m4_mathcomma(<<<4.8\times 10^{-15}>>>, <<<4.8 × 10⁻¹⁵>>>)
and the @code{jacobi_am} bigfloat problems at @code{u = 1.5b0} went from
m4_math(<<<2.3\times 10^{-32}>>>, <<<2.3 × 10⁻³²>>>)
to
m4_mathdot(<<<3.5\times 10^{-32}>>>, <<<3.5 × 10⁻³²>>>)
Subtracting rounded multiples of
m4_math(<<<2K>>>, <<<2K>>>)
and
m4_math(<<<2iK'>>>, <<<2iK'>>>)
adds error. It also always costs
m4_mathcomma(<<<K(m)>>>, <<<K(m)>>>)
m4_math(<<<K(1-m)>>>, <<<K(1−m)>>>)
and a lattice solve.

@node Reducing Only Large Arguments
@section Reducing Only Large Arguments

A sweep of 12 values of
m4_math(<<<m>>>, <<<m>>>)
(complex, real
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
real
m4_mathcomma(<<<m < 0>>>, <<<m < 0>>>)
near 0, near 1), with
m4_math(<<<u>>>, <<<u>>>)
real, imaginary and diagonal and
m4_math(<<<\lvert u\rvert>>>, <<<|u|>>>)
from 10 to 3000, compared the unreduced code with the reduced code
against mpmath:

@itemize @bullet
@item
Below the overflow point the unreduced code was as accurate as the
reduced code, usually within an ulp or two of
m4_mathdot(<<<u>>>, <<<u>>>)
It never degrades gradually.
@item
Every failure had
m4_mathcomma(<<<\lvert\mathrm{Im}\ v\rvert > 400>>>, <<<|Im v| > 400>>>)
and every case with
m4_math(<<<\lvert\mathrm{Im}\ v\rvert < 355>>>, <<<|Im v| < 355>>>)
was correct. A few cases above 355 happened to come out right, because
once
m4_math(<<<s^2>>>, <<<s²>>>)
is infinite the formulas can still collapse to something finite.
@end itemize

So the reduction is done only when

m4_displaymath(
<<<\left\lvert\mathrm{Im}\frac{\pi u}{2K(m)}\right\rvert > \frac{1}{4}\log(\text{most-positive-double-float}) \approx 177,>>>,
<<<@example
|Im(πu/(2K(m)))| > (1/4) log(most-positive-double-float) ≈ 177,
@end example>>>)

half the overflow point, which keeps
m4_mathdot(<<<s^2 \le e^{354}>>>, <<<s² ≤ e³⁵⁴>>>)
Below that, @code{u} is used unchanged. With this, all existing
@code{rtest_elliptic} tests pass with their original tolerances.
m4_math(<<<K(1-m)>>>, <<<K(1−m)>>>)
and the lattice solve are computed only when reducing.

@node Duplication Formula
@chapter Duplication Formula

Instead of subtracting periods, halve
m4_math(<<<u>>>, <<<u>>>)
the fewest times
m4_math(<<<n>>>, <<<n>>>)
that bring
m4_math(<<<\lvert\mathrm{Im}\ v\rvert>>>, <<<|Im v|>>>)
under the same threshold, evaluate at
m4_mathcomma(<<<u/2^n>>>, <<<u/2ⁿ>>>)
and apply the duplication formulas
m4_math(<<<n>>>, <<<n>>>)
times. They follow from
@uref{https://dlmf.nist.gov/22.8.E1, DLMF 22.8.1} to
@uref{https://dlmf.nist.gov/22.8.E3, 22.8.3} with
m4_mathpunc(<<<:>>>, <<<v = u>>>, <<<v = u>>>)

m4_displaymath(
<<<\mathrm{sn}(2u|m) = \frac{2scd}{1-ms^4}, \qquad \mathrm{cn}(2u|m) = \frac{c^2-s^2d^2}{1-ms^4}, \qquad \mathrm{dn}(2u|m) = \frac{d^2-ms^2c^2}{1-ms^4},>>>,
<<<@example
sn(2u|m) = 2scd/(1 − ms⁴),
cn(2u|m) = (c² − s²d²)/(1 − ms⁴),
dn(2u|m) = (d² − ms²c²)/(1 − ms⁴),
@end example>>>)

where
m4_mathcomma(<<<s = \mathrm{sn}(u|m)>>>, <<<s = sn(u|m)>>>)
m4_math(<<<c = \mathrm{cn}(u|m)>>>, <<<c = cn(u|m)>>>)
and
m4_mathdot(<<<d = \mathrm{dn}(u|m)>>>, <<<d = dn(u|m)>>>)

A rounding error at doubling step
m4_math(<<<k>>>, <<<k>>>)
that moves the point along the curve grows by
m4_mathcomma(<<<2^{n-k}>>>, <<<2ⁿ⁻ᵏ>>>)
so the total is about
m4_mathcomma(<<<\varepsilon 2^n\lvert u_0\rvert = \varepsilon\lvert u\rvert>>>, <<<ε2ⁿ|u₀| = ε|u|>>>)
the same as the problem's own conditioning. Errors that move
m4_math(<<<(s, c, d)>>>, <<<(s, c, d)>>>)
off the curve can grow much faster; see @inlinefmtifelse{tex,
@ref{Keeping the Doubling Stable, section 4.1, Keeping the Doubling
Stable}, @ref{Keeping the Doubling Stable, section 4.1}}. On 381 random
cases above the threshold, with
m4_math(<<<m>>>, <<<m>>>)
in
m4_math(<<<[-4,4]^2>>>, <<<[−4,4]²>>>)
and
m4_math(<<<\lvert u\rvert>>>, <<<|u|>>>)
from 300 to
m4_math(<<<10^5>>>, <<<10⁵>>>)
in random directions, errors in units of
m4_math(<<<\varepsilon\lvert u\rvert>>>, <<<ε|u|>>>)
were:

@raggedright
@multitable @columnfractions 0.4 0.2 0.2 0.2
@headitem
@tab
median
@tab
90th percentile
@tab
max
@item
period reduction
@tab
2.75
@tab
8.1
@tab
183
@item
halving and doubling
@tab
2.13
@tab
6.7
@tab
60
@item
halving and doubling, with projection
@tab
2.07
@tab
6.6
@tab
60
@end multitable
@end raggedright

Each method is occasionally up to about 25 times worse than the other
on individual cases, but both stay within the conditioning. On the fixed
sweep the worst cases were 139 for doubling and 243 for reduction. For
the six double-float regression cases, doubling was more accurate on
five; the exception is
m4_mathcomma(<<<u = 700i>>>, <<<u = 700i>>>)
m4_mathcomma(<<<m = 0.5>>>, <<<m = 0.5>>>)
at about
m4_mathdot(<<<1.6\ \varepsilon\lvert u\rvert>>>, <<<1.6 ε|u|>>>)

Compared with reduction, doubling needs no
m4_mathcomma(<<<K(1-m)>>>, <<<K(1−m)>>>)
no lattice solve and no sign corrections. Halving by
m4_math(<<<2^n>>>, <<<2ⁿ>>>)
is exact, and
m4_math(<<<n>>>, <<<n>>>)
is small: about 10 for
m4_mathdot(<<<\lvert u\rvert = 10^5>>>, <<<|u| = 10⁵>>>)
It still needs
m4_math(<<<K(m)>>>, <<<K(m)>>>)
to choose
m4_mathcomma(<<<n>>>, <<<n>>>)
because the scale factor depends on
m4_mathcomma(<<<m>>>, <<<m>>>)
but only for large arguments (see @inlinefmtifelse{tex, @ref{Avoiding
the Cost of K(m), section 5.2, Avoiding the Cost of K(m)}, @ref{Avoiding
the Cost of K(m), section 5.2}}). And when @code{ELLIPTIC-SN-DESCENDING}
has to double, it must compute cn and dn as well as sn.

One risk that reduction does not have: if an intermediate
m4_math(<<<2^k u_0>>>, <<<2ᵏu₀>>>)
lands very close to a pole,
m4_math(<<<1-ms^4>>>, <<<1−ms⁴>>>)
cancels and
m4_math(<<<s^4>>>, <<<s⁴>>>)
can overflow. With doubles that needs
m4_mathcomma(<<<\lvert s\rvert \gtrsim 10^{77}>>>, <<<|s| ≳ 10⁷⁷>>>)
which none of the cases came close to. An intermediate exactly on a pole
would divide by zero.

@menu
* Keeping the Doubling Stable::
* Change of Modulus::
@end menu

@node Keeping the Doubling Stable
@section Keeping the Doubling Stable

The random sweep above rarely had
m4_math(<<<m>>>, <<<m>>>)
near 1 with large
m4_mathcomma(<<<u>>>, <<<u>>>)
and that is where plain doubling fails. For example,
@code{jacobi_sn(1e8, 0.99+0.0001*%i)} gave
m4_mathpunc(<<<;>>>, <<<-1.0158+0.0763i>>>, <<<−1.0158+0.0763i>>>)
the correct value is
m4_mathdot(<<<-0.99036+0.04309i>>>, <<<−0.99036+0.04309i>>>)

Here
m4_mathcomma(<<<n = 9>>>, <<<n = 9>>>)
and the Landen evaluation at
m4_math(<<<u/2^9>>>, <<<u/2⁹>>>)
is accurate to
m4_mathcomma(<<<1.5\times 10^{-10}>>>, <<<1.5 × 10⁻¹⁰>>>)
consistent with its conditioning. The doubling steps are what fail. From
the fourth step on, the error grows about 100 times per step, and the
identities
m4_math(<<<s^2+c^2 = 1>>>, <<<s²+c² = 1>>>)
and
m4_math(<<<ms^2+d^2 = 1>>>, <<<ms²+d² = 1>>>)
drift by the same amount:

@raggedright
@multitable @columnfractions 0.1 0.3 0.3 0.3
@headitem
step
@tab
relative error of sn
@tab
m4_math(<<<\lvert s^2+c^2-1\rvert>>>, <<<|s²+c²−1|>>>)
@tab
m4_math(<<<\lvert 1-ms^4\rvert>>>, <<<|1−ms⁴|>>>)
(exact)
@item
0
@tab
m4_math(<<<1.5\times 10^{-10}>>>, <<<1.5 × 10⁻¹⁰>>>)
@tab
m4_math(<<<4.2\times 10^{-15}>>>, <<<4.2 × 10⁻¹⁵>>>)
@tab
0.92
@item
3
@tab
m4_math(<<<2.3\times 10^{-11}>>>, <<<2.3 × 10⁻¹¹>>>)
@tab
m4_math(<<<6.5\times 10^{-12}>>>, <<<6.5 × 10⁻¹²>>>)
@tab
0.053
@item
5
@tab
m4_math(<<<1.4\times 10^{-8}>>>, <<<1.4 × 10⁻⁸>>>)
@tab
m4_math(<<<2.7\times 10^{-8}>>>, <<<2.7 × 10⁻⁸>>>)
@tab
0.040
@item
7
@tab
m4_math(<<<4.4\times 10^{-5}>>>, <<<4.4 × 10⁻⁵>>>)
@tab
m4_math(<<<8.9\times 10^{-5}>>>, <<<8.9 × 10⁻⁵>>>)
@tab
0.011
@item
9
@tab
m4_math(<<<4.2\times 10^{-2}>>>, <<<4.2 × 10⁻²>>>)
@tab
m4_math(<<<8.5\times 10^{-2}>>>, <<<8.5 × 10⁻²>>>)
@tab
0.18
@end multitable
@end raggedright

For
m4_math(<<<m>>>, <<<m>>>)
near 1, sn stays close to
m4_math(<<<\pm 1>>>, <<<±1>>>)
for most of a period, so the denominator
m4_math(<<<1-ms^4>>>, <<<1−ms⁴>>>)
is small at most steps. An error that moves
m4_math(<<<(s, c, d)>>>, <<<(s, c, d)>>>)
off the curve is then multiplied by roughly
m4_math(<<<1/\lvert 1-ms^4\rvert>>>, <<<1/|1−ms⁴|>>>)
per step, instead of 2. Fukushima (2013) reports the same instability for
duplication of cn when
m4_math(<<<m>>>, <<<m>>>)
is near 1.

The fix is to project back onto the curve after each step: divide
m4_math(<<<s>>>, <<<s>>>)
and
m4_math(<<<c>>>, <<<c>>>)
by
m4_mathcomma(<<<\sqrt{s^2+c^2}>>>, <<<√(s²+c²)>>>)
which restores
m4_mathcomma(<<<s^2+c^2 = 1>>>, <<<s²+c² = 1>>>)
and correct
m4_math(<<<d>>>, <<<d>>>)
by one Newton step for
m4_mathpunc(<<<:>>>, <<<ms^2+d^2 = 1>>>, <<<ms²+d² = 1>>>)

m4_displaymath(
<<<d \leftarrow d - \frac{ms^2+d^2-1}{2d}.>>>,
<<<@example
d ← d − (ms² + d² − 1)/(2d).
@end example>>>)

Errors along the curve are unaffected and still grow by about 2 per
step. For 28 values of
m4_math(<<<m>>>, <<<m>>>)
with
m4_math(<<<\lvert 1-m\rvert>>>, <<<|1−m|>>>)
from 0.3 to
m4_math(<<<10^{-4}>>>, <<<10⁻⁴>>>)
in four directions, maximum errors over all
m4_mathcomma(<<<m>>>, <<<m>>>)
in units of
m4_mathpunc(<<<:>>>, <<<\varepsilon\lvert u\rvert>>>, <<<ε|u|>>>)

@raggedright
@multitable @columnfractions 0.1 0.3 0.3 0.3
@headitem
m4_math(<<<u>>>, <<<u>>>)
@tab
plain doubling
@tab
period reduction
@tab
doubling with projection
@item
m4_math(<<<10^3>>>, <<<10³>>>)
@tab
4.2
@tab
5.4
@tab
4.2
@item
m4_math(<<<10^4>>>, <<<10⁴>>>)
@tab
290
@tab
4.8
@tab
4.8
@item
m4_math(<<<10^5>>>, <<<10⁵>>>)
@tab
m4_math(<<<4.9\times 10^{10}>>>, <<<4.9 × 10¹⁰>>>)
@tab
8.5
@tab
3.8
@item
m4_math(<<<10^6>>>, <<<10⁶>>>)
@tab
m4_math(<<<4.6\times 10^{9}>>>, <<<4.6 × 10⁹>>>)
@tab
3.8
@tab
3.8
@item
m4_math(<<<10^8>>>, <<<10⁸>>>)
@tab
m4_math(<<<4.6\times 10^{7}>>>, <<<4.6 × 10⁷>>>)
@tab
3.3
@tab
6.9
@end multitable
@end raggedright

Projection does not hurt on the random sweep: it is equal to or
slightly better than plain doubling (see the table above). It costs one
complex @code{sqrt} and a few divisions per doubling step, and only when
m4_math(<<<u>>>, <<<u>>>)
is halved.

@menu
* Not Near a Pole::
* Not Near a Zero of dn::
@end menu

@node Not Near a Pole
@subsection Not Near a Pole

Near a pole of sn,
m4_math(<<<s>>>, <<<s>>>)
and
m4_math(<<<c>>>, <<<c>>>)
are both large while
m4_mathcomma(<<<s^2+c^2 = 1>>>, <<<s²+c² = 1>>>)
so computing
m4_math(<<<s^2+c^2>>>, <<<s²+c²>>>)
cancels: its relative error is about
m4_mathdot(<<<\varepsilon\lvert s\rvert^2>>>, <<<ε|s|²>>>)
Plain doubling is accurate there, and an unconditional projection
destroys the result. With
m4_math(<<<u>>>, <<<u>>>)
near a pole of sn (24 cases, 4 values of
m4_mathcomma(<<<m>>>, <<<m>>>)
m4_math(<<<\lvert u\rvert>>>, <<<|u|>>>)
from 500 to 12,000), relative errors were:

@raggedright
@multitable @columnfractions 0.1 0.3 0.3 0.3
@headitem
m4_math(<<<\lvert\mathrm{sn}\rvert>>>, <<<|sn|>>>)
@tab
conditioning:
m4_math(<<<\varepsilon>>>, <<<ε>>>)
times condition number
@tab
plain doubling
@tab
projection always
@item
m4_math(<<<10^6>>>, <<<10⁶>>>)
@tab
m4_math(<<<10^{-7}>>>, <<<10⁻⁷>>>)
to
m4_math(<<<2.4\times 10^{-6}>>>, <<<2.4 × 10⁻⁶>>>)
@tab
m4_math(<<<10^{-8}>>>, <<<10⁻⁸>>>)
to
m4_math(<<<6\times 10^{-7}>>>, <<<6 × 10⁻⁷>>>)
@tab
m4_math(<<<6\times 10^{-5}>>>, <<<6 × 10⁻⁵>>>)
to
m4_math(<<<7\times 10^{-4}>>>, <<<7 × 10⁻⁴>>>)
@item
m4_math(<<<10^9>>>, <<<10⁹>>>)
@tab
m4_math(<<<10^{-4}>>>, <<<10⁻⁴>>>)
to
m4_math(<<<2.4\times 10^{-3}>>>, <<<2.4 × 10⁻³>>>)
@tab
m4_math(<<<6\times 10^{-5}>>>, <<<6 × 10⁻⁵>>>)
to
m4_math(<<<2\times 10^{-3}>>>, <<<2 × 10⁻³>>>)
@tab
about 1, or a Lisp error
@end multitable
@end raggedright

The plain-doubling column leaves out
m4_math(<<<m = 0.99+0.0001i>>>, <<<m = 0.99+0.0001i>>>)
at
m4_mathcomma(<<<\lvert u\rvert = 9450>>>, <<<|u| = 9450>>>)
where plain doubling also suffers the
m4_math(<<<m>>>, <<<m>>>)
near 1 instability, with errors of
m4_math(<<<3\times 10^{-2}>>>, <<<3 × 10⁻²>>>)
and 0.96.

So the projection is done only when
m4_mathdot(<<<\lvert s\rvert \le 2>>>, <<<|s| ≤ 2>>>)
That costs nothing: the instability needs
m4_math(<<<1-ms^4>>>, <<<1−ms⁴>>>)
to be small, and for large
m4_math(<<<\lvert s\rvert>>>, <<<|s|>>>)
it is large, so plain doubling is stable there. With the guard, the
near-pole cases were within 2.3 times
m4_math(<<<\varepsilon>>>, <<<ε>>>)
times the condition number and never worse than plain doubling. The
random-sweep results were unchanged, and the results for
m4_math(<<<m>>>, <<<m>>>)
near 1 changed only in the last digits.

Rewriting the normalization as
m4_math(<<<s/\sqrt{s^2+c^2} = 1/\sqrt{1+(c/s)^2}>>>, <<<s/√(s²+c²) = 1/√(1+(c/s)²)>>>)
avoids overflow but not the cancellation. Near a pole
m4_mathcomma(<<<c \approx \pm is>>>, <<<c ≈ ±is>>>)
so
m4_math(<<<1+(c/s)^2 = 1/s^2>>>, <<<1+(c/s)² = 1/s²>>>)
is tiny and computing it from
m4_math(<<<1 + q>>>, <<<1 + q>>>)
with
m4_math(<<<q \approx -1>>>, <<<q ≈ −1>>>)
has the same relative error,
m4_mathdot(<<<\varepsilon\lvert s\rvert^2>>>, <<<ε|s|²>>>)
With correctly rounded
m4_math(<<<s>>>, <<<s>>>)
and
m4_mathcomma(<<<c>>>, <<<c>>>)
both forms lost accuracy at the same rate, and the rewritten form was
about twice as bad. Once
m4_math(<<<\lvert s\rvert>>>, <<<|s|>>>)
is large,
m4_math(<<<s>>>, <<<s>>>)
and
m4_math(<<<c>>>, <<<c>>>)
in double precision cannot show whether the point is on the curve to
better than
m4_mathcomma(<<<\varepsilon\lvert s\rvert^2>>>, <<<ε|s|²>>>)
so no rearrangement helps. The same holds for
m4_mathcomma(<<<ms^2+d^2-1>>>, <<<ms²+d²−1>>>)
since
m4_math(<<<d \approx \pm i\sqrt{m}\ s>>>, <<<d ≈ ±i√m s>>>)
near a pole. Overflow itself is not a concern: squaring overflows only
for
m4_mathcomma(<<<\lvert s\rvert \gtrsim 10^{154}>>>, <<<|s| ≳ 10¹⁵⁴>>>)
the doubling already forms
m4_mathcomma(<<<s^4>>>, <<<s⁴>>>)
and an intermediate
m4_math(<<<u/2^k>>>, <<<u/2ᵏ>>>)
is a double, so it cannot be close enough to a pole for
m4_math(<<<\lvert s\rvert>>>, <<<|s|>>>)
to approach either limit.

For small
m4_mathcomma(<<<\lvert m\rvert>>>, <<<|m|>>>)
m4_math(<<<1-ms^4>>>, <<<1−ms⁴>>>)
is small at
m4_mathcomma(<<<\lvert s\rvert \approx \lvert m\rvert^{-1/4}>>>, <<<|s| ≈ |m|^(−1/4)>>>)
which is above 2 when
m4_mathcomma(<<<\lvert m\rvert < 1/16>>>, <<<|m| < 1/16>>>)
so the guard skips the projection there. This did not matter in tests:
for 15 values of
m4_math(<<<m>>>, <<<m>>>)
with
m4_math(<<<\lvert m\rvert>>>, <<<|m|>>>)
from 0.05 to
m4_math(<<<10^{-6}>>>, <<<10⁻⁶>>>)
and large
m4_mathcomma(<<<u>>>, <<<u>>>)
plain and guarded doubling were both within 1.6 times
m4_math(<<<\varepsilon>>>, <<<ε>>>)
times the condition number.

@node Not Near a Zero of dn
@subsection Not Near a Zero of dn

The Newton step for
m4_math(<<<d>>>, <<<d>>>)
has the same problem near a zero of dn, at
m4_math(<<<K + iK'>>>, <<<K + iK'>>>)
plus periods. There
m4_mathcomma(<<<ms^2 \approx 1>>>, <<<ms² ≈ 1>>>)
so
m4_math(<<<ms^2+d^2-1>>>, <<<ms²+d²−1>>>)
has an absolute error of about
m4_mathcomma(<<<\varepsilon>>>, <<<ε>>>)
and dividing it by
m4_math(<<<2d>>>, <<<2d>>>)
replaces
m4_math(<<<d>>>, <<<d>>>)
with noise. The next doubling multiplies that noise into
m4_mathdot(<<<s>>>, <<<s>>>)
A doubling of a point near a zero of dn lands near a zero of sn, so the
final result is somewhat ill-conditioned, but the error was far beyond
that.

This can happen through the ordinary @code{jacobi_sn} path:
m4_math(<<<u>>>, <<<u>>>)
only needs to be
m4_math(<<<2^p>>>, <<<2ᵖ>>>)
times a point near a zero of dn far enough out that halving reaches it.
For 312 such cases (6 values of
m4_mathcomma(<<<m>>>, <<<m>>>)
zeros of dn up to about
m4_math(<<<300K'>>>, <<<300K'>>>)
out, offsets
m4_math(<<<10^{-6}>>>, <<<10⁻⁶>>>)
to
m4_mathpunc(<<<)>>>, <<<10^{-12}>>>, <<<10⁻¹²>>>)
errors in units of
m4_math(<<<\varepsilon>>>, <<<ε>>>)
times
m4_math(<<<\max(\lvert u\rvert, \text{condition number})>>>, <<<max(|u|, condition number)>>>)
were:

@raggedright
@multitable @columnfractions 0.4 0.2 0.2 0.2
@headitem
correction of
m4_math(<<<d>>>, <<<d>>>)
@tab
median
@tab
90th percentile
@tab
max
@item
always (when
m4_mathpunc(<<<)>>>, <<<\lvert s\rvert \le 2>>>, <<<|s| ≤ 2>>>)
@tab
8,500
@tab
m4_math(<<<4.6\times 10^{9}>>>, <<<4.6 × 10⁹>>>)
@tab
m4_math(<<<1.8\times 10^{11}>>>, <<<1.8 × 10¹¹>>>)
@item
only when
m4_math(<<<\lvert d\rvert \ge 10^{-3}>>>, <<<|d| ≥ 10⁻³>>>)
@tab
0.68
@tab
1.35
@tab
1.76
@item
never
@tab
0.67
@tab
1.38
@tab
1.71
@item
plain doubling (no projection at all)
@tab
0.67
@tab
1.32
@tab
2.28
@end multitable
@end raggedright

Never correcting
m4_math(<<<d>>>, <<<d>>>)
is not an option: for
m4_math(<<<m>>>, <<<m>>>)
near 1 the worst error then rises from 2.9 to
m4_mathdot(<<<1.2\times 10^{8}>>>, <<<1.2 × 10⁸>>>)
So
m4_math(<<<d>>>, <<<d>>>)
is corrected only when
m4_mathdot(<<<\lvert d\rvert \ge 10^{-3}>>>, <<<|d| ≥ 10⁻³>>>)
Any threshold from
m4_math(<<<10^{-5}>>>, <<<10⁻⁵>>>)
to
m4_math(<<<10^{-3}>>>, <<<10⁻³>>>)
fixed the dn-zero cases;
m4_math(<<<10^{-6}>>>, <<<10⁻⁶>>>)
did not (worst case about 4,000). With the
m4_math(<<<10^{-3}>>>, <<<10⁻³>>>)
guard every other sweep gave results identical to correcting
m4_math(<<<d>>>, <<<d>>>)
always.

The problem was found while working on @code{elliptic_eu}, which
called @code{DESCENDING-LANDEN-DUPLICATION} with its own
m4_math(<<<n>>>, <<<n>>>)
at
m4_mathcomma(<<<u = 2(z_0+10^{-9})>>>, <<<u = 2(z₀+10⁻⁹)>>>)
m4_mathcomma(<<<z_0 = K+iK'>>>, <<<z₀ = K+iK'>>>)
m4_mathdot(<<<m = 0.5+0.5i>>>, <<<m = 0.5+0.5i>>>)
With
m4_math(<<<n = 2>>>, <<<n = 2>>>)
the result was
m4_math(<<<-7.8\times 10^{-7}-5.6\times 10^{-7}i>>>, <<<−7.8 × 10⁻⁷ − 5.6 × 10⁻⁷i>>>)
instead of
m4_mathdot(<<<-2\times 10^{-9}>>>, <<<−2 × 10⁻⁹>>>)

@node Change of Modulus
@section Change of Modulus

For
m4_math(<<<m>>>, <<<m>>>)
very close to 1, the projection is not enough. With
m4_math(<<<\lvert 1-m\rvert>>>, <<<|1−m|>>>)
from
m4_math(<<<10^{-5}>>>, <<<10⁻⁵>>>)
to
m4_math(<<<10^{-10}>>>, <<<10⁻¹⁰>>>)
(125 cases), the doubling with projection had a 90th percentile of
m4_math(<<<4\times 10^{3}>>>, <<<4 × 10³>>>)
and a worst case of
m4_mathcomma(<<<7.8\times 10^{6}>>>, <<<7.8 × 10⁶>>>)
in units of
m4_math(<<<\varepsilon>>>, <<<ε>>>)
times
m4_mathdot(<<<\max(\lvert u\rvert, \text{condition number})>>>, <<<max(|u|, condition number)>>>)

Jacobi's imaginary transformation (A&S 16.20.1 to 16.20.3,
@uref{https://dlmf.nist.gov/22.6.T1, DLMF Table 22.6.1}) changes the
parameter to
m4_mathpunc(<<<:>>>, <<<1-m>>>, <<<1−m>>>)

m4_displaymath(
<<<\mathrm{sn}(u|m) = -i\ \mathrm{sc}(iu|1-m), \qquad \mathrm{cn}(u|m) = \mathrm{nc}(iu|1-m), \qquad \mathrm{dn}(u|m) = \mathrm{dc}(iu|1-m).>>>,
<<<@example
sn(u|m) = −i sc(iu|1−m),
cn(u|m) = nc(iu|1−m),
dn(u|m) = dc(iu|1−m).
@end example>>>)

It is an identity for all complex
m4_mathdot(<<<m>>>, <<<m>>>)
Computing
m4_math(<<<1-m>>>, <<<1−m>>>)
is exact for
m4_math(<<<m>>>, <<<m>>>)
in
m4_mathdot(<<<[1/2, 2]>>>, <<<[1/2, 2]>>>)
The other changes of modulus do not help here:
m4_math(<<<1/m>>>, <<<1/m>>>)
is also near 1, and
m4_math(<<<m/(m-1)>>>, <<<m/(m−1)>>>)
is huge.

The instability is not confined to
m4_math(<<<m>>>, <<<m>>>)
near 1. Mapping plain doubling over a grid of complex
m4_math(<<<m>>>, <<<m>>>)
with
m4_math(<<<\lvert u\rvert>>>, <<<|u|>>>)
up to
m4_math(<<<10^7>>>, <<<10⁷>>>)
in four directions, it was unstable roughly where
m4_math(<<<3/4 \le \mathrm{Re}\ m \le 9/4>>>, <<<3/4 ≤ Re m ≤ 9/4>>>)
and
m4_mathcomma(<<<\lvert\mathrm{Im}\ m\rvert \le 1>>>, <<<|Im m| ≤ 1>>>)
and stable everywhere with
m4_mathdot(<<<\mathrm{Re}\ m \le 1/2>>>, <<<Re m ≤ 1/2>>>)
Since
m4_math(<<<m \mapsto 1-m>>>, <<<m ↦ 1−m>>>)
maps
m4_math(<<<\mathrm{Re}\ m > 1/2>>>, <<<Re m > 1/2>>>)
to
m4_mathcomma(<<<\mathrm{Re} < 1/2>>>, <<<Re < 1/2>>>)
the transformation is used when
m4_math(<<<u>>>, <<<u>>>)
needs halving and
m4_mathdot(<<<\mathrm{Re}\ m > 1/2>>>, <<<Re m > 1/2>>>)
A cutoff on
m4_math(<<<\lvert 1-m\rvert>>>, <<<|1−m|>>>)
does not work: plain doubling is still unstable at
m4_mathcomma(<<<\lvert 1-m\rvert = 0.5>>>, <<<|1−m| = 0.5>>>)
and if
m4_math(<<<m>>>, <<<m>>>)
is moderately close to 1 without being small,
m4_math(<<<1-m>>>, <<<1−m>>>)
can be moderately close to 1 too.

Worst errors, in units of
m4_math(<<<\varepsilon>>>, <<<ε>>>)
times
m4_mathpunc(<<<:>>>, <<<\max(\lvert u\rvert, \text{condition number})>>>, <<<max(|u|, condition number)>>>)

@raggedright
@multitable @columnfractions 0.34 0.22 0.22 0.22
@headitem
sweep
@tab
projection
@tab
transformation alone
@tab
transformation and projection
@item
m4_math(<<<\lvert 1-m\rvert>>>, <<<|1−m|>>>)
from
m4_math(<<<10^{-5}>>>, <<<10⁻⁵>>>)
to
m4_math(<<<10^{-10}>>>, <<<10⁻¹⁰>>>)
@tab
m4_math(<<<7.8\times 10^{6}>>>, <<<7.8 × 10⁶>>>)
@tab
2.25
@tab
2.25
@item
m4_math(<<<m>>>, <<<m>>>)
near 1,
m4_math(<<<\lvert 1-m\rvert \ge 10^{-4}>>>, <<<|1−m| ≥ 10⁻⁴>>>)
@tab
2.89
@tab
2.78
@tab
2.78
@item
grid of complex
m4_math(<<<m>>>, <<<m>>>)
@tab
2.78
@tab
3.19
@tab
2.78
@item
m4_math(<<<\lvert 1-m\rvert>>>, <<<|1−m|>>>)
from 0.5 to 100
@tab
6.17
@tab
19.4
@tab
4.58
@item
near poles of sn
@tab
2.32
@tab
2.15
@tab
2.15
@item
near zeros of dn
@tab
1.76
@tab
1.46
@tab
1.46
@item
random sweep (units of
m4_mathpunc(<<<)>>>, <<<\varepsilon\lvert u\rvert>>>, <<<ε|u|>>>)
@tab
59.6
@tab
59.6
@tab
59.6
@end multitable
@end raggedright

The projection columns use the
m4_math(<<<\lvert d\rvert \ge 10^{-3}>>>, <<<|d| ≥ 10⁻³>>>)
guard. The combination is best or equal everywhere. The projection is
still needed with the transformation: without it the worst case is 19.4,
at
m4_math(<<<m = 0.65-0.36i>>>, <<<m = 0.65−0.36i>>>)
and
m4_mathcomma(<<<u = 10^8>>>, <<<u = 10⁸>>>)
just past the switch, where the transformed
m4_math(<<<1-m = 0.35+0.36i>>>, <<<1−m = 0.35+0.36i>>>)
is still close enough to the unstable region to lose some accuracy.

Some individual results get slightly worse. For example,
@code{jacobi_cn(1e6*%i, 0.99+0.01*%i)} goes from
m4_math(<<<3.7\times 10^{-11}>>>, <<<3.7 × 10⁻¹¹>>>)
to
m4_mathcomma(<<<9.6\times 10^{-11}>>>, <<<9.6 × 10⁻¹¹>>>)
still under
m4_mathdot(<<<\varepsilon\lvert u\rvert = 2.2\times 10^{-10}>>>, <<<ε|u| = 2.2 × 10⁻¹⁰>>>)
Over the sweep of
m4_math(<<<m>>>, <<<m>>>)
near 1 the transformation is better on average: the median falls from
0.82 to 0.55.

Dividing by the transformed cn is safe. Its zeros are the poles of
m4_mathcomma(<<<\mathrm{sn}(u|m)>>>, <<<sn(u|m)>>>)
where the result is ill-conditioned anyway, and the near-pole sweep
above shows no loss.

@node Cost
@chapter Cost

@menu
* Timing::
* Avoiding the Cost of K(m)::
@end menu

@node Timing
@section Timing

Times are the minimum of 7 runs, in each of two separate SBCL processes.
``Original'' is @code{master} before the change; ``with bound'' is the
final version, described in the next section.

@raggedright
@multitable @columnfractions 0.24 0.19 0.19 0.19 0.19
@headitem
@tab
original
@tab
doubling,
m4_math(<<<K(m)>>>, <<<K(m)>>>)
always
@tab
doubling, with bound
@tab
sn always via duplication
@item
real
m4_mathcomma(<<<m = 0.7>>>, <<<m = 0.7>>>)
200,000 @code{sn} calls
@tab
0.33–0.36 s
@tab
0.34–0.37 s
@tab
0.36–0.42 s
@tab
0.44–0.46 s
@item
real
m4_mathcomma(<<<m = 0.999>>>, <<<m = 0.999>>>)
200,000 @code{sn} calls
@tab
0.45–0.54 s
@tab
0.49–0.54 s
@tab
0.55–0.57 s
@tab
0.64–0.67 s
@item
m4_mathcomma(<<<m = 0.5+0.5i>>>, <<<m = 0.5+0.5i>>>)
20,000 @code{sn} calls
@tab
0.12–0.13 s
@tab
0.94–0.98 s
@tab
0.13–0.18 s
@tab
0.95–1.04 s
@end multitable
@end raggedright

The real-m timings vary by 10–20% between runs. In the real-m rows
m4_math(<<<u>>>, <<<u>>>)
is real too, so neither ``K(m) always'' nor ``with bound'' computes
m4_mathpunc(<<<:>>>, <<<K(m)>>>, <<<K(m)>>>)
both return
m4_math(<<<n = 0>>>, <<<n = 0>>>)
from the first test, and their difference is noise. The bound matters
only when
m4_math(<<<u>>>, <<<u>>>)
or
m4_math(<<<m>>>, <<<m>>>)
is complex, as in the last row.

@strong{Routing sn through duplication.}
@code{ELLIPTIC-SN-CN-DN-DESCENDING} always calls the duplication
function, so @code{ELLIPTIC-SN-DESCENDING} could too, and the halving
test could move into the duplication function. But
@code{DESCENDING-LANDEN-SN-CN-DN} does three divisions per Landen step
instead of one, so every @code{sn} call would be about 25–35% slower for
real
m4_mathcomma(<<<m>>>, <<<m>>>)
even when nothing is halved. @code{ELLIPTIC-SN-DESCENDING} therefore
calls @code{DESCENDING-LANDEN-SN} when
m4_mathdot(<<<n = 0>>>, <<<n = 0>>>)

@strong{Computing K(m) on every call.} The first version of
@code{JACOBI-ARG-HALVINGS} computed
m4_math(<<<K(m)>>>, <<<K(m)>>>)
on every call with complex
m4_math(<<<u>>>, <<<u>>>)
or
m4_mathdot(<<<m>>>, <<<m>>>)
For complex
m4_mathcomma(<<<m>>>, <<<m>>>)
@code{bf-elliptic-k} costs about 40 µs, about 0.8 s per 20,000 calls on
its own, so @code{sn} became about 7 times slower than before, even for
small arguments that are never halved.

@node Avoiding the Cost of K(m)
@section Avoiding the Cost of K(m)

m4_mathcomma(<<<K(m) = \pi/(2\ \mathrm{AGM}(1, \sqrt{1-m}))>>>, <<<K(m) = π/(2 AGM(1, √(1−m)))>>>)
and each step of the AGM produces an arithmetic and a geometric mean,
neither larger in modulus than the larger of its two arguments. So

m4_displaymath(
<<<\lvert\mathrm{AGM}(1,\sqrt{1-m})\rvert \le \max\left(1, \sqrt{\lvert 1-m\rvert}\right),>>>,
<<<@example
|AGM(1, √(1−m))| ≤ max(1, √|1−m|),
@end example>>>)

and therefore

m4_displaymath(
<<<\lvert\mathrm{Im}\ v\rvert = \left\lvert\mathrm{Im}\frac{\pi u}{2K(m)}\right\rvert \le \lvert u\rvert\ \lvert\mathrm{AGM}(1,\sqrt{1-m})\rvert \le \lvert u\rvert \max\left(1, \sqrt{\lvert 1-m\rvert}\right).>>>,
<<<@example
|Im v| = |Im(πu/(2K(m)))| ≤ |u| |AGM(1, √(1−m))|
       ≤ |u| max(1, √|1−m|).
@end example>>>)

@code{JACOBI-ARG-HALVINGS} first checks this bound against the limit,
and computes
m4_math(<<<K(m)>>>, <<<K(m)>>>)
only when the bound exceeds it. The bound costs a @code{sqrt} and two
@code{abs}.

The bound only needs to be cheap and never too small. Checked against
the exact value for 4,000 random
m4_math(<<<m>>>, <<<m>>>)
with
m4_math(<<<\lvert m\rvert>>>, <<<|m|>>>)
from
m4_math(<<<10^{-3}>>>, <<<10⁻³>>>)
to
m4_math(<<<10^4>>>, <<<10⁴>>>)
in all directions, the ratio of exact value to bound was at most
0.99975, so the bound never skips a needed halving. The median ratio was
0.36 and the 10th percentile 0.08. So for some
m4_math(<<<u>>>, <<<u>>>)
near the limit
m4_math(<<<K(m)>>>, <<<K(m)>>>)
is computed unnecessarily, but no result changes: the fixed and random
sweeps give results identical to computing
m4_math(<<<K(m)>>>, <<<K(m)>>>)
every time.

With the bound, complex
m4_math(<<<m>>>, <<<m>>>)
is back to within about 10–50% of the original time.

@node Bigfloats
@chapter Bigfloats

At @code{fpprec:32}, a sweep of 9 values of
m4_math(<<<m>>>, <<<m>>>)
with
m4_math(<<<\lvert u\rvert>>>, <<<|u|>>>)
up to
m4_math(<<<10^5>>>, <<<10⁵>>>)
showed the unreduced path is better on every count:

@itemize @bullet
@item
@strong{Accuracy.} Worst error 8.7 units of
m4_math(<<<\varepsilon\lvert u\rvert>>>, <<<ε|u|>>>)
unreduced against 11.6 reduced. At
m4_mathcomma(<<<u = 10^{20}i>>>, <<<u = 10²⁰i>>>)
m4_mathcomma(<<<m = 0.5+0.5i>>>, <<<m = 0.5+0.5i>>>)
the relative error is
m4_math(<<<2.4\times 10^{-13}>>>, <<<2.4 × 10⁻¹³>>>)
unreduced and
m4_math(<<<7.7\times 10^{-13}>>>, <<<7.7 × 10⁻¹³>>>)
reduced, with
m4_mathdot(<<<\varepsilon\lvert u\rvert = 1.2\times 10^{-12}>>>, <<<ε|u| = 1.2 × 10⁻¹²>>>)
Reducing adds the rounding errors in
m4_math(<<<K(m)>>>, <<<K(m)>>>)
and
m4_mathdot(<<<K(1-m)>>>, <<<K(1−m)>>>)
@item
@strong{Time.} About 8 ms per call from
m4_math(<<<u = 10^5 i>>>, <<<u = 10⁵i>>>)
to
m4_mathdot(<<<10^{12} i>>>, <<<10¹²i>>>)
@code{DESCENDING-BASE-P} needs
m4_mathcomma(<<<\lvert m\rvert < \varepsilon e^{-2\lvert\mathrm{Im}\ u\rvert}>>>, <<<|m| < ε e^(−2|Im u|)>>>)
so large arguments take extra Landen steps, but only about
m4_math(<<<\log\lvert\mathrm{Im}\ u\rvert>>>, <<<log|Im u|>>>)
of them, and @code{exp} of a huge bigfloat costs nothing special.
@end itemize

So bigfloats are never reduced, and likewise never halved with the
duplication formula. The test is @code{CL:NUMBERP}, because
@code{NUMBERP} is shadowed in the @code{bigfloat} package and true for
bigfloats there.

@node Code Structure
@chapter Code Structure

The fix lives inside @code{ELLIPTIC-SN-DESCENDING} and
@code{ELLIPTIC-SN-CN-DN-DESCENDING}, so @code{sn}, @code{cn}, @code{dn}
and @code{elliptic-eu} call them unchanged:

@itemize @bullet
@item
The recursive Landen functions are renamed @code{DESCENDING-LANDEN-SN}
and @code{DESCENDING-LANDEN-SN-CN-DN}, changing only the @code{defun}
lines and the recursive calls. Keeping them as global functions rather
than local @code{labels} functions avoids reindenting their bodies.
@item
@code{JACOBI-ARG-HALVINGS} returns
m4_mathpunc(<<<:>>>, <<<n>>>, <<<n>>>)
0 for bigfloats, for real
m4_math(<<<u>>>, <<<u>>>)
with real
m4_mathcomma(<<<m>>>, <<<m>>>)
and when the cheap bound is below the threshold. Otherwise it computes
m4_math(<<<K(m)>>>, <<<K(m)>>>)
and the exact
m4_mathdot(<<<n>>>, <<<n>>>)
@item
@code{DESCENDING-LANDEN-DUPLICATION} evaluates at
m4_mathcomma(<<<u/2^n>>>, <<<u/2ⁿ>>>)
using @code{SCALE-FLOAT} on the real and imaginary parts, and doubles
m4_math(<<<n>>>, <<<n>>>)
times, projecting back onto the curve after each step when
m4_mathdot(<<<\lvert s\rvert \le 2>>>, <<<|s| ≤ 2>>>)
A real
m4_math(<<<u>>>, <<<u>>>)
is scaled as a real number. CL's @code{complex} returns a complex float
such as @code{#C(0.5d0 0.0d0)} even when the imaginary part is zero, so
scaling every
m4_math(<<<u>>>, <<<u>>>)
as complex would make real results complex and run the real path in
complex arithmetic.
@item
@code{DESCENDING-LANDEN-DUPLICATION} corrects
m4_math(<<<d>>>, <<<d>>>)
only when
m4_mathdot(<<<\lvert d\rvert \ge 10^{-3}>>>, <<<|d| ≥ 10⁻³>>>)
@item
@code{ELLIPTIC-SN-CN-DN-DESCENDING} uses the imaginary transformation
when
m4_math(<<<n > 0>>>, <<<n > 0>>>)
and
m4_mathdot(<<<\mathrm{Re}\ m > 1/2>>>, <<<Re m > 1/2>>>)
It computes
m4_math(<<<(s, c, d)>>>, <<<(s, c, d)>>>)
at
m4_mathcomma(<<<(iu, 1-m)>>>, <<<(iu, 1−m)>>>)
with its own halving count, and returns
m4_mathdot(<<<(-is/c, 1/c, d/c)>>>, <<<(−is/c, 1/c, d/c)>>>)
@item
@code{ELLIPTIC-SN-DESCENDING} uses @code{DESCENDING-LANDEN-SN} alone
when
m4_mathcomma(<<<n = 0>>>, <<<n = 0>>>)
for the speed reason in @inlinefmtifelse{tex, @ref{Timing, section 5.1,
Timing}, @ref{Timing, section 5.1}}. Otherwise it calls
@code{ELLIPTIC-SN-CN-DN-DESCENDING}, so that the transformation is used
for sn too. That computes
m4_math(<<<K(m)>>>, <<<K(m)>>>)
a second time, but only for large arguments.
@end itemize

@node Conclusion
@chapter Conclusion

Use halving and doubling only when
m4_math(<<<\lvert\mathrm{Im}(\pi u/(2K(m)))\rvert>>>, <<<|Im(πu/(2K(m)))|>>>)
exceeds the threshold, and never for bigfloats. After each doubling,
project
m4_math(<<<s>>>, <<<s>>>)
and
m4_math(<<<c>>>, <<<c>>>)
back onto the curve when
m4_mathcomma(<<<\lvert s\rvert \le 2>>>, <<<|s| ≤ 2>>>)
and
m4_math(<<<d>>>, <<<d>>>)
when also
m4_mathdot(<<<\lvert d\rvert \ge 10^{-3}>>>, <<<|d| ≥ 10⁻³>>>)
When
m4_mathcomma(<<<\mathrm{Re}\ m > 1/2>>>, <<<Re m > 1/2>>>)
use Jacobi's imaginary transformation to the parameter
m4_mathdot(<<<1-m>>>, <<<1−m>>>)
With these it is as accurate as period reduction, including for
m4_math(<<<m>>>, <<<m>>>)
very close to 1. It needs less code and leaves every small-argument
result unchanged. It needs only
m4_mathcomma(<<<K(m)>>>, <<<K(m)>>>)
and the cheap bound means that is computed only for large arguments, so
small arguments cost about what they did before.

All @code{rtest_elliptic} tests pass, and the full suite with share
tests has only the unrelated @code{rtestprintf} problem 38 failure,
which also fails without the change. The new tolerances were set from
SBCL and still need checking on CMUCL.

@node References
@chapter References

@itemize @bullet
@item
@uref{https://dlmf.nist.gov/22.4.iii, DLMF §22.4(iii)}, translation by
half or quarter periods.
@item
@uref{https://dlmf.nist.gov/22.6.iv, DLMF §22.6(iv)}, Jacobi's imaginary
transformation.
@item
@uref{https://dlmf.nist.gov/22.8, DLMF §22.8}, addition theorems.
@item
T.@: Fukushima, ``Precise and fast computation of Jacobian elliptic
functions by conditional duplication,'' @emph{Numer.@: Math.@:} 123
(2013), pp.@: 585–605.
@end itemize

@bye
