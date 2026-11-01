\input texinfo
@c -*- mode: texinfo -*-
@c Created by Claude to describe how Maxima computes jacobi_epsilon
@c (formerly elliptic_eu) numerically.
@c
@c Generated from elliptic-eu-complex-m.md, a Markdown note in the claude.ai
@c project used while implementing it.  That note will not be updated
@c anymore; this is the canonical description of the implementation at
@c the time of writing.
@c
@setfilename jacobi-epsilon.info
@settitle Computing Jacobi's Epsilon Function
@documentencoding UTF-8

@c Definitions so that plain TeX accepts the same formulas as MathJax.
@tex
\gdef\scriptE{{\cal E}}
\gdef\lvert{\vert}
\gdef\rvert{\vert}
\gdef\mathrm#1{{\rm #1}}
\gdef\mathcal#1{{\cal #1}}
\gdef\text#1{\hbox{#1}}
\gdef\frac#1#2{{#1\over #2}}
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
@top Computing Jacobi's Epsilon Function
@end ifnottex

@iftex
@titlepage
@title Computing Jacobi's Epsilon Function
@end titlepage
@end iftex
This note describes how to compute Jacobi's epsilon function

m4_displaymath(
<<<\mathcal{E}(u \mid m) = \int_0^u \mathrm{dn}^2(t \mid m)\thinspace dt>>>,
<<<ℰ(u|m) = ∫₀ᵘ dn²(t|m) dt>>>)

(@uref{https://dlmf.nist.gov/22.16.E17, DLMF 22.16.17}; A&S 16.26.3)
numerically, in floating point or arbitrary precision, for real and
complex
m4_math(<<<u>>>, <<<u>>>)
and
m4_mathdot(<<<m>>>, <<<m>>>)

@menu
* Notation::
* Overview::
* Real m and Real u::
* Real m and Complex u::
* Complex m - Reduction by the Periods::
* Carlson's Idea::
* From ℰ(w'|m) to ℰ(w|m)::
* Alternatives That Are Not Used::
* Round-Off::
* Validation::
* Limitations::
* References::
@end menu

@node Notation
@unnumbered Notation

@itemize @bullet
@item
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
is Maxima's @code{jacobi_epsilon(u, m)}. DLMF writes it with the modulus
m4_math(<<<k>>>, <<<k>>>)
as
m4_math(<<<\mathcal{E}(x, k)>>>, <<<ℰ(x, k)>>>)
(@uref{https://dlmf.nist.gov/22.16.E14, DLMF 22.16.14} and
@uref{https://dlmf.nist.gov/22.16.E17, DLMF 22.16.17}); Mathematica
calls it @code{JacobiEpsilon[u, m]}.
@item
m4_math(<<<E(\phi \mid m)>>>, <<<E(φ|m)>>>)
is Legendre's incomplete integral of the second kind
(@uref{https://dlmf.nist.gov/19.2.E5, DLMF 19.2.5}).
@item
m4_math(<<<K(m)>>>, <<<K(m)>>>)
and
m4_math(<<<E(m)>>>, <<<E(m)>>>)
are the complete integrals (@uref{https://dlmf.nist.gov/19.2.E8, DLMF
19.2.8}). For complex
m4_mathcomma(<<<m>>>, <<<m>>>)
m4_mathcomma(<<<K(m)>>>, <<<K(m)>>>)
m4_mathcomma(<<<E(m)>>>, <<<E(m)>>>)
m4_math(<<<K(1-m)>>>, <<<K(1−m)>>>)
and
m4_math(<<<E(1-m)>>>, <<<E(1−m)>>>)
are principal values, computed from Carlson's
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_mathdot(<<<R_D>>>, <<<R_D>>>)
@item
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_mathcomma(<<<\mathrm{cn}>>>, <<<cn>>>)
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
are taken at parameter
m4_math(<<<m>>>, <<<m>>>)
unless another parameter is shown.
@end itemize

@node Overview
@chapter Overview

The computation goes through these cases in order.

@enumerate
@item
@strong{Exact values.}
m4_mathdot(<<<\mathcal{E}(0 \mid m) = 0>>>, <<<ℰ(0|m) = 0>>>)
For
m4_mathcomma(<<<m = 0>>>, <<<m = 0>>>)
m4_mathcomma(<<<\mathrm{dn}(u \mid 0) = 1>>>, <<<dn(u|0) = 1>>>)
so
m4_mathdot(<<<\mathcal{E}(u \mid 0) = u>>>, <<<ℰ(u|0) = u>>>)
For
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
m4_math(<<<\mathrm{dn}(u \mid 1) = \mathrm{sech}\thinspace u>>>, <<<dn(u|1) = sech u>>>)
(@uref{https://dlmf.nist.gov/22.5.T4, DLMF Table 22.5.4}; A&S Table
16.6), so
m4_mathdot(<<<\mathcal{E}(u \mid 1) = \tanh u>>>, <<<ℰ(u|1) = tanh u>>>)
@item
@strong{Oddness.}
m4_math(<<<\mathrm{dn}^2>>>, <<<dn²>>>)
is even, so
m4_mathdot(<<<\mathcal{E}(-u \mid m) = -\mathcal{E}(u \mid m)>>>, <<<ℰ(−u|m) = −ℰ(u|m)>>>)
The function is evaluated only for
m4_mathcomma(<<<\mathrm{Re}\thinspace u > 0>>>, <<<Re u > 0>>>)
or for
m4_math(<<<u>>>, <<<u>>>)
on the nonnegative imaginary axis, and negated otherwise. This makes the
computed values exactly odd.
@item
If
m4_math(<<<m>>>, <<<m>>>)
and
m4_math(<<<u>>>, <<<u>>>)
are real: @inlinefmtifelse{tex, @ref{Real m and Real u, section 2, Real
@math{m} and Real @math{u}}, @ref{Real m and Real u, section 2}}.
@item
If
m4_math(<<<m>>>, <<<m>>>)
is real and
m4_math(<<<u>>>, <<<u>>>)
is complex: @inlinefmtifelse{tex, @ref{Real m and Complex u, section 3,
Real @math{m} and Complex @math{u}}, @ref{Real m and Complex u, section
3}}.
@item
If
m4_math(<<<m>>>, <<<m>>>)
is complex, for real or complex
m4_math(<<<u{:}>>>, <<<u:>>>)
@inlinefmtifelse{tex, @ref{Complex m - Reduction by the Periods,
sections 4 to 6, Complex @math{m}: Reduction by the Periods},
@ref{Complex m - Reduction by the Periods, sections 4 to 6}}.
@end enumerate

For a symbolic
m4_mathcomma(<<<m>>>, <<<m>>>)
Maxima also simplifies
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
at multiples of
m4_math(<<<K(m){:}>>>, <<<K(m):>>>)
m4_math(<<<\mathcal{E}(nK(m) \mid m) = nE(m)>>>, <<<ℰ(nK(m)|m) = nE(m)>>>)
for integer
m4_mathcomma(<<<n>>>, <<<n>>>)
the translations of @inlinefmtifelse{tex, @ref{m < 1, section 2.1,
@math{m < 1}}, @ref{m < 1, section 2.1}} by
m4_mathcomma(<<<nK(m)>>>, <<<nK(m)>>>)
and

m4_displaymath(
<<<\mathcal{E}(K(m)/2 \mid m) = \frac{E(m) + 1 - \sqrt{1-m}}{2}, \qquad
\mathcal{E}(3K(m)/2 \mid m) = \frac{3E(m) - 1 + \sqrt{1-m}}{2}.>>>,
<<<@example
ℰ(K(m)/2|m) = (E(m) + 1 − √(1−m))/2,
ℰ(3K(m)/2|m) = (3E(m) − 1 + √(1−m))/2.
@end example>>>)

The first follows from the duplication formula of @inlinefmtifelse{tex,
@ref{Duplication Instead of the Reduction, section 7.4, Duplication
Instead of the Reduction}, @ref{Duplication Instead of the Reduction,
section 7.4}} with
m4_math(<<<v = K(m)/2>>>, <<<v = K(m)/2>>>)
and
m4_mathpunc(<<<;>>>, <<<\mathrm{sn}^2(K(m)/2 \mid m) = 1/(1 + \sqrt{1-m})>>>, <<<sn²(K(m)/2|m) = 1/(1 + √(1−m))>>>)
the second from the translation by
m4_math(<<<K(m)>>>, <<<K(m)>>>)
in @inlinefmtifelse{tex, @ref{m < 1, section 2.1, @math{m < 1}}, @ref{m
< 1, section 2.1}}.

@node Real m and Real u
@ifinfo
@chapter Real m and Real u
@end ifinfo
@ifhtml
@chapter Real @math{m} and Real @math{u}
@end ifhtml
@iftex
@chapter Real @math{m} and Real @math{u}
@end iftex

@menu
* m < 1::
* m > 1::
@end menu

@node m < 1
@ifinfo
@section m < 1
@end ifinfo
@ifhtml
@section @math{m < 1}
@end ifhtml
@iftex
@section @math{m < 1}
@end iftex

@w{}
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
is quasi-periodic (@uref{https://dlmf.nist.gov/22.16.ii, DLMF
§22.16(ii)}):

m4_displaymath(
<<<\mathcal{E}(u + 2K(m) \mid m) = \mathcal{E}(u \mid m) + 2E(m),>>>,
<<<ℰ(u + 2K(m)|m) = ℰ(u|m) + 2E(m),>>>)

m4_displaymath(
<<<\mathcal{E}(u + K(m) \mid m) = \mathcal{E}(u \mid m) + E(m) - m\thinspace\mathrm{sn}(u \mid m)\thinspace\mathrm{cd}(u \mid m).>>>,
<<<ℰ(u + K(m)|m) = ℰ(u|m) + E(m) − m sn(u|m) cd(u|m).>>>)

Write
m4_math(<<<u = 2nK(m) + r>>>, <<<u = 2nK(m) + r>>>)
with
m4_math(<<<n>>>, <<<n>>>)
an integer and
m4_mathdot(<<<0 \le r < 2K(m)>>>, <<<0 ≤ r < 2K(m)>>>)
Then
m4_mathdot(<<<\mathcal{E}(u \mid m) = 2nE(m) + \mathcal{E}(r \mid m)>>>, <<<ℰ(u|m) = 2nE(m) + ℰ(r|m)>>>)
If
m4_mathcomma(<<<r < K(m)>>>, <<<r < K(m)>>>)

m4_displaymath(
<<<\mathcal{E}(r \mid m) = E(\phi \mid m), \qquad \phi = \mathrm{am}(r \mid m),>>>,
<<<ℰ(r|m) = E(φ|m), φ = am(r|m),>>>)

(@uref{https://dlmf.nist.gov/22.16.ii, DLMF §22.16(ii)}). If
m4_mathcomma(<<<r \ge K(m)>>>, <<<r ≥ K(m)>>>)
the translation by
m4_math(<<<K(m)>>>, <<<K(m)>>>)
reduces this to the same case with
m4_mathdot(<<<r - K(m)>>>, <<<r − K(m)>>>)

Since
m4_math(<<<\mathrm{sn} = \sin\phi>>>, <<<sn = sin φ>>>)
and
m4_math(<<<\mathrm{cn} = \cos\phi>>>, <<<cn = cos φ>>>)
(@uref{https://dlmf.nist.gov/22.16.E12, DLMF 22.16.12} and
@uref{https://dlmf.nist.gov/22.16.E13, DLMF 22.16.13}), and
m4_math(<<<0 \le \phi < \pi/2>>>, <<<0 ≤ φ < π/2>>>)
here,
m4_mathdot(<<<\phi = \mathrm{atan2}(\mathrm{sn}(r \mid m), \mathrm{cn}(r \mid m))>>>, <<<φ = atan2(sn(r|m), cn(r|m))>>>)
The simpler
m4_math(<<<\phi = \sin^{-1}\mathrm{sn}(r \mid m)>>>, <<<φ = sin⁻¹sn(r|m)>>>)
is ill-conditioned as
m4_mathcomma(<<<\mathrm{sn} \to 1>>>, <<<sn → 1>>>)
that is, as
m4_mathdot(<<<r \to K(m)>>>, <<<r → K(m)>>>)

This covers
m4_math(<<<m < 0>>>, <<<m < 0>>>)
as well, where
m4_math(<<<K(m)>>>, <<<K(m)>>>)
and
m4_math(<<<E(m)>>>, <<<E(m)>>>)
are real.

@node m > 1
@ifinfo
@section m > 1
@end ifinfo
@ifhtml
@section @math{m > 1}
@end ifhtml
@iftex
@section @math{m > 1}
@end iftex

The reciprocal-modulus transformation
(@uref{https://dlmf.nist.gov/22.17.E4, DLMF 22.17.4}; A&S 16.11.4) gives
m4_mathdot(<<<\mathrm{dn}(u \mid m) = \mathrm{cn}(\sqrt{m}\thinspace u \mid 1/m)>>>, <<<dn(u|m) = cn(√m u|1/m)>>>)
With
m4_mathcomma(<<<s = \sqrt{m}\thinspace t>>>, <<<s = √m t>>>)

m4_displaymath(
<<<\mathcal{E}(u \mid m) = \frac{1}{\sqrt{m}} \int_0^{\sqrt{m}\thinspace u} \mathrm{cn}^2(s \mid 1/m)\thinspace ds .>>>,
<<<ℰ(u|m) = 1/(√m) ∫₀^(√m u) cn²(s|1/m) ds.>>>)

By @uref{https://dlmf.nist.gov/22.6.E1, DLMF 22.6.1} (A&S 16.9.1),
m4_mathcomma(<<<\mathrm{cn}^2(s \mid 1/m) = (1 - m) + m\thinspace\mathrm{dn}^2(s \mid 1/m)>>>, <<<cn²(s|1/m) = (1 − m) + m dn²(s|1/m)>>>)
so

m4_displaymath(
<<<\mathcal{E}(u \mid m) = \sqrt{m}\thinspace\mathcal{E}(\sqrt{m}\thinspace u \mid 1/m) - (m - 1)\thinspace u,>>>,
<<<ℰ(u|m) = √m ℰ(√m u|1/m) − (m − 1) u,>>>)

and
m4_math(<<<0 < 1/m < 1>>>, <<<0 < 1/m < 1>>>)
is the case of @inlinefmtifelse{tex, @ref{m < 1, section 2.1, @math{m <
1}}, @ref{m < 1, section 2.1}}.

@node Real m and Complex u
@ifinfo
@chapter Real m and Complex u
@end ifinfo
@ifhtml
@chapter Real @math{m} and Complex @math{u}
@end ifhtml
@iftex
@chapter Real @math{m} and Complex @math{u}
@end iftex

Write
m4_math(<<<u = x + iv>>>, <<<u = x + iv>>>)
with
m4_math(<<<x>>>, <<<x>>>)
and
m4_math(<<<v>>>, <<<v>>>)
real, and
m4_mathdot(<<<m' = 1 - m>>>, <<<m' = 1 − m>>>)
The quasi-addition formula (@uref{https://dlmf.nist.gov/22.16.ii, DLMF
§22.16(ii)})

m4_displaymath(
<<<\mathcal{E}(a + b \mid m) = \mathcal{E}(a \mid m) + \mathcal{E}(b \mid m) - m\thinspace\mathrm{sn}(a \mid m)\thinspace\mathrm{sn}(b \mid m)\thinspace\mathrm{sn}(a + b \mid m),>>>,
<<<ℰ(a + b|m) = ℰ(a|m) + ℰ(b|m) − m sn(a|m) sn(b|m) sn(a + b|m),>>>)

Jacobi's imaginary transformation
m4_math(<<<\mathrm{sn}(iv \mid m) = i\thinspace\mathrm{sc}(v \mid m')>>>, <<<sn(iv|m) = i sc(v|m')>>>)
(@uref{https://dlmf.nist.gov/22.6.T1, DLMF Table 22.6.1}; A&S 16.20.1),
and its counterpart for
m4_mathcomma(<<<\mathcal{E}>>>, <<<ℰ>>>)

m4_displaymath(
<<<\mathcal{E}(iv \mid m) = i\bigl(v + \mathrm{sc}(v \mid m')\thinspace\mathrm{dn}(v \mid m') - \mathcal{E}(v \mid m')\bigr),>>>,
<<<ℰ(iv|m) = i(v + sc(v|m') dn(v|m') − ℰ(v|m')),>>>)

give Lawden's formula

m4_displaymath(
<<<\mathcal{E}(x + iv \mid m) = \mathcal{E}(x \mid m) - i\thinspace\mathcal{E}(v \mid m') + iv + i\thinspace\mathrm{sc}(v \mid m')\thinspace\mathrm{dn}(v \mid m') - im\thinspace\mathrm{sn}(x \mid m)\thinspace\mathrm{sc}(v \mid m')\thinspace\mathrm{sn}(x + iv \mid m).>>>,
<<<ℰ(x + iv|m) = ℰ(x|m) − i ℰ(v|m') + iv + i sc(v|m') dn(v|m') − im sn(x|m) sc(v|m') sn(x + iv|m).>>>,
<<<\eqalign{\mathcal{E}(x + iv \mid m) &= \mathcal{E}(x \mid m) - i\thinspace\mathcal{E}(v \mid m') + iv + i\thinspace\mathrm{sc}(v \mid m')\thinspace\mathrm{dn}(v \mid m') \cr &\qquad - im\thinspace\mathrm{sn}(x \mid m)\thinspace\mathrm{sc}(v \mid m')\thinspace\mathrm{sn}(x + iv \mid m).}>>>)

@w{}
m4_math(<<<\mathcal{E}(x \mid m)>>>, <<<ℰ(x|m)>>>)
and
m4_math(<<<\mathcal{E}(v \mid m')>>>, <<<ℰ(v|m')>>>)
have real arguments and real parameters, so @inlinefmtifelse{tex,
@ref{Real m and Real u, section 2, Real @math{m} and Real @math{u}},
@ref{Real m and Real u, section 2}} computes them.

The terms with
m4_math(<<<\mathrm{sc}(v \mid m')>>>, <<<sc(v|m')>>>)
are large near
m4_math(<<<\mathrm{cn}(v \mid m') = 0>>>, <<<cn(v|m') = 0>>>)
and cancel there. For
m4_mathcomma(<<<m > 0>>>, <<<m > 0>>>)
m4_math(<<<m' < 1>>>, <<<m' < 1>>>)
and
m4_math(<<<K' = K(m')>>>, <<<K' = K(m')>>>)
is real. Then
m4_math(<<<v>>>, <<<v>>>)
is first reduced to
m4_math(<<<|v| \le K'/2>>>, <<<|v| ≤ K'/2>>>)
with

m4_displaymath(
<<<\mathcal{E}(u + 2iK' \mid m) = \mathcal{E}(u \mid m) + 2i(K' - E'),>>>,
<<<ℰ(u + 2iK'|m) = ℰ(u|m) + 2i(K' − E'),>>>)

m4_displaymath(
<<<\mathcal{E}(u + s\thinspace iK' \mid m) = \mathcal{E}(u \mid m) + \frac{\mathrm{cn}(u \mid m)\thinspace\mathrm{dn}(u \mid m)}{\mathrm{sn}(u \mid m)} + s\thinspace i(K' - E'),
\qquad s = \pm 1,>>>,
<<<@example
ℰ(u + s iK'|m) = ℰ(u|m) + (cn(u|m) dn(u|m))/(sn(u|m)) + s i(K' − E'),
s = ±1,
@end example>>>)

where
m4_math(<<<E' = E(m')>>>, <<<E' = E(m')>>>)
(@uref{https://dlmf.nist.gov/22.16.ii, DLMF §22.16(ii)}). For
m4_mathcomma(<<<m \le 0>>>, <<<m ≤ 0>>>)
m4_math(<<<m' \ge 1>>>, <<<m' ≥ 1>>>)
and
m4_math(<<<\mathrm{cn}(v \mid m') = \mathrm{dn}(\sqrt{m'}\thinspace v \mid 1/m') > 0>>>, <<<cn(v|m') = dn(√m' v|1/m') > 0>>>)
for real
m4_math(<<<v>>>, <<<v>>>)
(@uref{https://dlmf.nist.gov/22.17.E3, DLMF 22.17.3}; A&S 16.11.3), so
there is no cancellation and no reduction is needed.

@node Complex m - Reduction by the Periods
@ifinfo
@chapter Complex m: Reduction by the Periods
@end ifinfo
@ifhtml
@chapter Complex @math{m}: Reduction by the Periods
@end ifhtml
@iftex
@chapter Complex @math{m}: Reduction by the Periods
@end iftex

For complex
m4_math(<<<m{:}>>>, <<<m:>>>)

@enumerate
@item
Reduce
m4_math(<<<u>>>, <<<u>>>)
by the periods
m4_math(<<<2K(m)>>>, <<<2K(m)>>>)
and
m4_math(<<<2iK(1-m)>>>, <<<2iK(1−m)>>>)
to a point
m4_math(<<<w>>>, <<<w>>>)
near the origin (below).
@item
Compute
m4_mathcomma(<<<\mathrm{sn}(w \mid m)>>>, <<<sn(w|m)>>>)
m4_math(<<<\mathrm{cn}(w \mid m)>>>, <<<cn(w|m)>>>)
and
m4_mathdot(<<<\mathrm{dn}(w \mid m)>>>, <<<dn(w|m)>>>)
@item
Evaluate Carlson's forms. They give
m4_math(<<<\mathcal{E}(w' \mid m)>>>, <<<ℰ(w'|m)>>>)
exactly, for a point
m4_math(<<<w'>>>, <<<w'>>>)
with
m4_math(<<<\mathrm{sn}(w' \mid m) = \mathrm{sn}(w \mid m)>>>, <<<sn(w'|m) = sn(w|m)>>>)
that need not be
m4_math(<<<w>>>, <<<w>>>)
(@inlinefmtifelse{tex, @ref{Carlson's Idea, section 5, Carlson's Idea},
@ref{Carlson's Idea, section 5}}).
@item
Recover
m4_math(<<<\mathcal{E}(w \mid m)>>>, <<<ℰ(w|m)>>>)
from
m4_math(<<<\mathcal{E}(w' \mid m)>>>, <<<ℰ(w'|m)>>>)
(@inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m), section 6, From
@math{@scriptE(w'|m)} to @math{@scriptE(w|m)}}, @ref{From ℰ(w'|m) to
ℰ(w|m), section 6}}), and add back the reduction.
@end enumerate

The parameter
m4_math(<<<m>>>, <<<m>>>)
is never changed; only
m4_math(<<<u>>>, <<<u>>>)
is reduced.

@w{}
m4_math(<<<\mathrm{dn}^2(u \mid m)>>>, <<<dn²(u|m)>>>)
has periods
m4_math(<<<2K(m)>>>, <<<2K(m)>>>)
and
m4_math(<<<2iK(1-m)>>>, <<<2iK(1−m)>>>)
(@uref{https://dlmf.nist.gov/22.4.T1, DLMF Table 22.4.1}; A&S Table
16.2), and
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
is quasi-periodic (@uref{https://dlmf.nist.gov/22.16.ii, DLMF
§22.16(ii)}):

m4_displaymath(
<<<\mathcal{E}(z + 2K(m) \mid m) = \mathcal{E}(z \mid m) + 2E(m),>>>,
<<<ℰ(z + 2K(m)|m) = ℰ(z|m) + 2E(m),>>>)

m4_displaymath(
<<<\mathcal{E}(z + 2iK(1-m) \mid m) = \mathcal{E}(z \mid m) + 2i\bigl(K(1-m) - E(1-m)\bigr).>>>,
<<<ℰ(z + 2iK(1−m)|m) = ℰ(z|m) + 2i(K(1−m) − E(1−m)).>>>)

These hold for
m4_mathdot(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
Both sides are analytic in
m4_math(<<<m>>>, <<<m>>>)
off the cuts of
m4_mathcomma(<<<K(m)>>>, <<<K(m)>>>)
m4_math(<<<E(m)>>>, <<<E(m)>>>)
(on
m4_mathpunc(<<<)>>>, <<<[1, \infty)>>>, <<<[1, ∞)>>>)
and of
m4_mathcomma(<<<K(1-m)>>>, <<<K(1−m)>>>)
m4_math(<<<E(1-m)>>>, <<<E(1−m)>>>)
(on
m4_mathpunc(<<<),>>>, <<<(-\infty, 0]>>>, <<<(−∞, 0]>>>)
so they hold for every non-real
m4_math(<<<m>>>, <<<m>>>)
by analytic continuation. For complex
m4_math(<<<m>>>, <<<m>>>)
the periods
m4_math(<<<\omega_1 = 2K(m)>>>, <<<ω₁ = 2K(m)>>>)
and
m4_math(<<<\omega_2 = 2iK(1-m)>>>, <<<ω₂ = 2iK(1−m)>>>)
are complex, so the lattice they span is skewed relative to the real and
imaginary axes.

Solve
m4_math(<<<z = a\thinspace\omega_1 + b\thinspace\omega_2>>>, <<<z = a ω₁ + b ω₂>>>)
for real
m4_mathcomma(<<<a>>>, <<<a>>>)
m4_mathcomma(<<<b>>>, <<<b>>>)
a
m4_math(<<<2 \times 2>>>, <<<2 × 2>>>)
real linear system with determinant

m4_displaymath(
<<<\Delta = \mathrm{Re}\thinspace\omega_1\thinspace\mathrm{Im}\thinspace\omega_2 - \mathrm{Re}\thinspace\omega_2\thinspace\mathrm{Im}\thinspace\omega_1 .>>>,
<<<Δ = Re ω₁ Im ω₂ − Re ω₂ Im ω₁.>>>)

With
m4_math(<<<j = \mathrm{round}(a)>>>, <<<j = round(a)>>>)
and
m4_math(<<<k = \mathrm{round}(b)>>>, <<<k = round(b)>>>)
for
m4_mathcomma(<<<z = u>>>, <<<z = u>>>)
let
m4_mathdot(<<<w = u - j\thinspace\omega_1 - k\thinspace\omega_2>>>, <<<w = u − j ω₁ − k ω₂>>>)
Then

m4_displaymath(
<<<\mathcal{E}(u \mid m) = \mathcal{E}(w \mid m) + 2jE(m) + 2ik\bigl(K(1-m) - E(1-m)\bigr).>>>,
<<<ℰ(u|m) = ℰ(w|m) + 2jE(m) + 2ik(K(1−m) − E(1−m)).>>>)

@menu
* Why Reduce::
@end menu

@node Why Reduce
@section Why Reduce

@inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m), Section 6, From
@math{@scriptE(w'|m)} to @math{@scriptE(w|m)}}, @ref{From ℰ(w'|m) to
ℰ(w|m), Section 6}} works for any
m4_mathcomma(<<<w>>>, <<<w>>>)
so the reduction is not needed for correctness. It keeps the arguments
of
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_math(<<<\mathrm{cn}>>>, <<<cn>>>)
and
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
small. Without it, the usual way of computing them fails for complex
m4_math(<<<m>>>, <<<m>>>)
and large arguments. The descending Landen transformation
(@uref{https://dlmf.nist.gov/22.7.i, DLMF 22.7.1–22.7.4}; A&S
16.12.1–16.12.4) scales the argument by
m4_mathcomma(<<<1/\prod_n (1 + \sqrt{\mu_n})>>>, <<<1/∏_n (1 + √μ_n)>>>)
which is complex when
m4_math(<<<m>>>, <<<m>>>)
is. For
m4_math(<<<m = 0.5 + 0.5i>>>, <<<m = 0.5 + 0.5i>>>)
the factor is
m4_mathcomma(<<<0.894 - 0.170i>>>, <<<0.894 − 0.170i>>>)
so the sine at the end of the recursion gets an argument whose imaginary
part is about
m4_math(<<<0.17\thinspace u{:}>>>, <<<0.17 u:>>>)

@raggedright
@multitable @columnfractions 0.07 0.30 0.64
@headitem
m4_math(<<<u>>>, <<<u>>>)
@tab
final argument of
m4_math(<<<\sin>>>, <<<sin>>>)
@tab
m4_math(<<<\lvert\sin\rvert \approx e^{\lvert\mathrm{Im}\rvert}>>>, <<<|sin| ≈ e^(|Im|)>>>)
@item
100
@tab
m4_math(<<<89.4 - 17.0i>>>, <<<89.4 − 17.0i>>>)
@tab
m4_math(<<<2\times10^{7}>>>, <<<2×10⁷>>>)
@item
1000
@tab
m4_math(<<<893.9 - 169.9i>>>, <<<893.9 − 169.9i>>>)
@tab
m4_math(<<<6\times10^{73}>>>, <<<6×10⁷³>>>)
@item
3000
@tab
m4_math(<<<2681.8 - 509.6i>>>, <<<2681.8 − 509.6i>>>)
@tab
m4_math(<<<2\times10^{221}>>>, <<<2×10²²¹>>>)
@item
5000
@tab
m4_math(<<<4469.6 - 849.3i>>>, <<<4469.6 − 849.3i>>>)
@tab
overflow
@end multitable
@end raggedright

At
m4_mathcomma(<<<u = 3000>>>, <<<u = 3000>>>)
m4_math(<<<\sin^2>>>, <<<sin²>>>)
in the next Landen step overflows, and
m4_math(<<<\mathrm{sn}(3000 \mid 0.5 + 0.5i)>>>, <<<sn(3000|0.5 + 0.5i)>>>)
comes out as about
m4_math(<<<7.7\times10^{-60}>>>, <<<7.7×10⁻⁶⁰>>>)
instead of
m4_mathdot(<<<-1.2185 + 0.3331i>>>, <<<−1.2185 + 0.3331i>>>)
Computing
m4_math(<<<\mathcal{E}(3000 \mid 0.5 + 0.5i)>>>, <<<ℰ(3000|0.5 + 0.5i)>>>)
with such
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_math(<<<\mathrm{cn}>>>, <<<cn>>>)
and
m4_math(<<<\mathrm{dn}{:}>>>, <<<dn:>>>)

@raggedright
@multitable @columnfractions 0.34 0.66
@headitem
@tab
m4_math(<<<\mathcal{E}(3000 \mid 0.5 + 0.5i)>>>, <<<ℰ(3000|0.5 + 0.5i)>>>)
@item
reference (50 digits)
@tab
m4_math(<<<2349.5013343334482 - 545.45726453403568i>>>, <<<2349.5013343334482 − 545.45726453403568i>>>)
@item
no reduction
@tab
m4_math(<<<2351.4185907138053 - 545.33050331162400i>>>, <<<2351.4185907138053 − 545.33050331162400i>>>)
@item
with the reduction
@tab
m4_math(<<<2349.5013343334476 - 545.4572645340356i>>>, <<<2349.5013343334476 − 545.4572645340356i>>>)
@end multitable
@end raggedright

The error without the reduction is about
m4_math(<<<8\times10^{-4}>>>, <<<8×10⁻⁴>>>)
relative; with it, about
m4_mathdot(<<<2\times10^{-16}>>>, <<<2×10⁻¹⁶>>>)
Along the real axis the unreduced computation is accurate up to about
m4_math(<<<u = 2000>>>, <<<u = 2000>>>)
and wrong from about
m4_mathdot(<<<2500>>>, <<<2500>>>)
With a large imaginary part it fails sooner;
m4_math(<<<u = 700i>>>, <<<u = 700i>>>)
overflows. Arbitrary precision does not have this problem because of its
exponent range.
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_math(<<<\mathrm{cn}>>>, <<<cn>>>)
and
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
can be computed accurately for large arguments in other ways;
@inlinefmtifelse{tex, @ref{Duplication Instead of the Reduction, section
7.4, Duplication Instead of the Reduction}, @ref{Duplication Instead of
the Reduction, section 7.4}} explains why the reduction is still
preferred.

@node Carlson's Idea
@chapter Carlson's Idea

After the reduction,
m4_math(<<<w>>>, <<<w>>>)
is near the origin, where
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_math(<<<\mathrm{cn}>>>, <<<cn>>>)
and
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
can be computed accurately. Let

m4_displaymath(
<<<s = \mathrm{sn}(w \mid m), \qquad c = \mathrm{cn}(w \mid m), \qquad d = \mathrm{dn}(w \mid m).>>>,
<<<s = sn(w|m), c = cn(w|m), d = dn(w|m).>>>)

The idea is to get
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
from these three numbers. Substituting
m4_mathcomma(<<<t = \mathrm{sn}(v \mid m)>>>, <<<t = sn(v|m)>>>)
m4_math(<<<dt = \mathrm{cn}(v \mid m)\thinspace\mathrm{dn}(v \mid m)\thinspace dv>>>, <<<dt = cn(v|m) dn(v|m) dv>>>)
in the definition, and using
m4_math(<<<\mathrm{cn}^2 = 1 - \mathrm{sn}^2>>>, <<<cn² = 1 − sn²>>>)
and
m4_math(<<<\mathrm{dn}^2 = 1 - m\thinspace\mathrm{sn}^2>>>, <<<dn² = 1 − m sn²>>>)
(@uref{https://dlmf.nist.gov/22.6.E1, DLMF 22.6.1}; A&S 16.9.1), gives

m4_displaymath(
<<<\mathcal{E}(w \mid m) = \int_0^{s} \frac{\sqrt{1-mt^2}}{\sqrt{1-t^2}}\thinspace dt>>>,
<<<ℰ(w|m) = ∫₀ˢ (√(1−mt²))/(√(1−t²)) dt>>>)

(@uref{https://dlmf.nist.gov/22.16.E14, DLMF 22.16.14}), where the
integral is taken along the image under
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
of a path from
m4_math(<<<0>>>, <<<0>>>)
to
m4_mathdot(<<<w>>>, <<<w>>>)
This is Legendre's integral
m4_math(<<<E(\phi \mid m)>>>, <<<E(φ|m)>>>)
with
m4_math(<<<\sin\phi = s>>>, <<<sin φ = s>>>)
(@uref{https://dlmf.nist.gov/19.2.E5, DLMF 19.2.5}). Carlson [1], eqs.@:
(59) and (60), gives Legendre's integrals in terms of the symmetric
integrals
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
(@uref{https://dlmf.nist.gov/19.25.E5, DLMF 19.25.5} and
@uref{https://dlmf.nist.gov/19.25.E9, DLMF 19.25.9}):

m4_displaymath(
<<<F(\phi, k) = R_F(\gamma - 1, \gamma - k^2, \gamma),>>>,
<<<F(φ, k) = R_F(γ − 1, γ − k², γ),>>>)

m4_displaymath(
<<<E(\phi, k) = R_F(\gamma - 1, \gamma - k^2, \gamma) - \frac{k^2}{3} R_D(\gamma - 1, \gamma - k^2, \gamma),
\qquad \gamma = \csc^2\phi .>>>,
<<<@example
E(φ, k) = R_F(γ − 1, γ − k², γ) − k²/3 R_D(γ − 1, γ − k², γ),
γ = csc²φ.
@end example>>>)

@w{}
m4_math(<<<R_F>>>, <<<R_F>>>)
is homogeneous of degree
m4_math(<<<-1/2>>>, <<<−1/2>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
of degree
m4_mathcomma(<<<-3/2>>>, <<<−3/2>>>)
and
m4_mathcomma(<<<(\csc^2\phi - 1, \csc^2\phi - k^2, \csc^2\phi) = \csc^2\phi\thinspace(\cos^2\phi, 1 - k^2\sin^2\phi, 1)>>>, <<<(csc²φ − 1, csc²φ − k², csc²φ) = csc²φ (cos²φ, 1 − k²sin²φ, 1)>>>)
so with
m4_math(<<<k^2 = m>>>, <<<k² = m>>>)

m4_displaymath(
<<<F(\phi \mid m) = \sin\phi\thinspace R_F(\cos^2\phi, 1 - m\sin^2\phi, 1),>>>,
<<<F(φ|m) = sin φ R_F(cos²φ, 1 − m sin²φ, 1),>>>)

m4_displaymath(
<<<E(\phi \mid m) = \sin\phi\thinspace R_F(\cos^2\phi, 1 - m\sin^2\phi, 1) - \frac{m}{3}\sin^3\phi\thinspace R_D(\cos^2\phi, 1 - m\sin^2\phi, 1).>>>,
<<<E(φ|m) = sin φ R_F(cos²φ, 1 − m sin²φ, 1) − m/3 sin³φ R_D(cos²φ, 1 − m sin²φ, 1).>>>)

Setting
m4_math(<<<\sin\phi = s>>>, <<<sin φ = s>>>)
gives
m4_math(<<<\cos^2\phi = 1 - s^2 = c^2>>>, <<<cos²φ = 1 − s² = c²>>>)
and
m4_mathcomma(<<<1 - m\sin^2\phi = 1 - ms^2 = d^2>>>, <<<1 − m sin²φ = 1 − ms² = d²>>>)
so neither
m4_math(<<<\phi>>>, <<<φ>>>)
nor
m4_math(<<<1 - s^2>>>, <<<1 − s²>>>)
has to be computed. This gives the two numbers that are actually
evaluated:

m4_displaymath(
<<<w' = s\thinspace R_F(c^2, d^2, 1),>>>,
<<<w' = s R_F(c², d², 1),>>>)

m4_displaymath(
<<<e = s\thinspace R_F(c^2, d^2, 1) - \frac{m}{3}\thinspace s^3 R_D(c^2, d^2, 1).>>>,
<<<e = s R_F(c², d², 1) − m/3 s³ R_D(c², d², 1).>>>)

Using
m4_math(<<<c^2>>>, <<<c²>>>)
and
m4_math(<<<d^2>>>, <<<d²>>>)
rather than
m4_math(<<<1 - s^2>>>, <<<1 − s²>>>)
and
m4_math(<<<1 - ms^2>>>, <<<1 − ms²>>>)
also avoids cancellation when
m4_math(<<<s>>>, <<<s>>>)
is near
m4_math(<<<\pm 1>>>, <<<±1>>>)
or
m4_math(<<<ms^2>>>, <<<ms²>>>)
is near
m4_mathdot(<<<1>>>, <<<1>>>)

What
m4_math(<<<e>>>, <<<e>>>)
and
m4_math(<<<w'>>>, <<<w'>>>)
are follows from the integrals they come from. With the principal
branches that
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
use,
m4_math(<<<w'>>>, <<<w'>>>)
and
m4_math(<<<e>>>, <<<e>>>)
are

m4_displaymath(
<<<\int_0^s \frac{dt}{\sqrt{(1-t^2)(1-mt^2)}}
\quad\text{and}\quad
\int_0^s \frac{\sqrt{1-mt^2}}{\sqrt{1-t^2}}\thinspace dt>>>,
<<<@example
∫₀ˢ dt/(√((1−t²)(1−mt²)))
and
∫₀ˢ (√(1−mt²))/(√(1−t²)) dt
@end example>>>)

along the same path from
m4_math(<<<0>>>, <<<0>>>)
to
m4_mathdot(<<<s>>>, <<<s>>>)
The substitution
m4_math(<<<t = \mathrm{sn}(v \mid m)>>>, <<<t = sn(v|m)>>>)
turns the first integral into
m4_mathcomma(<<<\int_0^{w'} dv>>>, <<<∫₀^(w') dv>>>)
so
m4_math(<<<w'>>>, <<<w'>>>)
is the point where that path ends in the plane of
m4_mathcomma(<<<v>>>, <<<v>>>)
and
m4_mathdot(<<<\mathrm{sn}(w' \mid m) = s>>>, <<<sn(w'|m) = s>>>)
It turns the second into
m4_mathcomma(<<<\int_0^{w'} \mathrm{dn}^2(v \mid m)\thinspace dv>>>, <<<∫₀^(w') dn²(v|m) dv>>>)
so

m4_displaymath(
<<<e = \mathcal{E}(w' \mid m), \qquad \mathrm{sn}(w' \mid m) = s .>>>,
<<<e = ℰ(w'|m), sn(w'|m) = s.>>>)

That is,
m4_math(<<<e>>>, <<<e>>>)
is
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
at the principal inverse
m4_math(<<<w'>>>, <<<w'>>>)
of
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
at
m4_mathdot(<<<s>>>, <<<s>>>)
It equals
m4_math(<<<\mathcal{E}(w \mid m)>>>, <<<ℰ(w|m)>>>)
only if
m4_mathcomma(<<<w' = w>>>, <<<w' = w>>>)
which need not hold. Example:
m4_mathcomma(<<<u = 3 - 2i>>>, <<<u = 3 − 2i>>>)
m4_math(<<<m = 2 + i>>>, <<<m = 2 + i>>>)
reduces to
m4_mathcomma(<<<w \approx 1.0830 + 1.2432i>>>, <<<w ≈ 1.0830 + 1.2432i>>>)
but
m4_mathcomma(<<<w' \approx 1.4830 + 0.5755i>>>, <<<w' ≈ 1.4830 + 0.5755i>>>)
and there
m4_mathdot(<<<w + w' - 2K(m) = 0>>>, <<<w + w' − 2K(m) = 0>>>)
Using
m4_math(<<<e>>>, <<<e>>>)
directly as
m4_math(<<<\mathcal{E}(w \mid m)>>>, <<<ℰ(w|m)>>>)
gives
m4_math(<<<0.4805 - 0.6271i>>>, <<<0.4805 − 0.6271i>>>)
instead of
m4_mathdot(<<<1.5016 - 1.0105i>>>, <<<1.5016 − 1.0105i>>>)
This is why
m4_math(<<<w'>>>, <<<w'>>>)
is computed: @inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m), section
6, From @math{@scriptE(w'|m)} to @math{@scriptE(w|m)}}, @ref{From
ℰ(w'|m) to ℰ(w|m), section 6}} uses it to get
m4_math(<<<\mathcal{E}(w \mid m)>>>, <<<ℰ(w|m)>>>)
from
m4_mathdot(<<<e>>>, <<<e>>>)

@node From ℰ(w'|m) to ℰ(w|m)
@ifinfo
@chapter From ℰ(w'|m) to ℰ(w|m)
@end ifinfo
@ifhtml
@chapter From @math{\mathcal{E}(w' \mid m)} to @math{\mathcal{E}(w \mid m)}
@end ifhtml
@iftex
@chapter From @math{@scriptE(w'|m)} to @math{@scriptE(w|m)}
@end iftex

@w{}
m4_math(<<<\mathrm{sn}(z \mid m)>>>, <<<sn(z|m)>>>)
is elliptic of order 2 with periods
m4_math(<<<4K(m)>>>, <<<4K(m)>>>)
and
m4_math(<<<2iK(1-m)>>>, <<<2iK(1−m)>>>)
(@uref{https://dlmf.nist.gov/22.4.T1, DLMF Table 22.4.1}). So
m4_math(<<<\mathrm{sn}(w' \mid m) = \mathrm{sn}(w \mid m)>>>, <<<sn(w'|m) = sn(w|m)>>>)
means that, for some integers
m4_math(<<<p>>>, <<<p>>>)
and
m4_mathcomma(<<<q>>>, <<<q>>>)
either

m4_displaymath(
<<<w = w' + 2pK(m) + 2iqK(1-m)>>>,
<<<w = w' + 2pK(m) + 2iqK(1−m)>>>)

or

m4_displaymath(
<<<w = 2K(m) - w' + 2pK(m) + 2iqK(1-m).>>>,
<<<w = 2K(m) − w' + 2pK(m) + 2iqK(1−m).>>>)

With the quasi-periodicity of @inlinefmtifelse{tex, @ref{Complex m -
Reduction by the Periods, section 4, Complex @math{m}: Reduction by the
Periods}, @ref{Complex m - Reduction by the Periods, section 4}} and
m4_mathcomma(<<<\mathcal{E}(2K(m) - z \mid m) = 2E(m) - \mathcal{E}(z \mid m)>>>, <<<ℰ(2K(m) − z|m) = 2E(m) − ℰ(z|m)>>>)
which follows from the oddness of
m4_mathcomma(<<<\mathcal{E}(z \mid m)>>>, <<<ℰ(z|m)>>>)
the two cases give

m4_displaymath(
<<<\mathcal{E}(w \mid m) = e + 2pE(m) + 2iq\bigl(K(1-m) - E(1-m)\bigr),>>>,
<<<ℰ(w|m) = e + 2pE(m) + 2iq(K(1−m) − E(1−m)),>>>)

m4_displaymath(
<<<\mathcal{E}(w \mid m) = 2E(m) - e + 2pE(m) + 2iq\bigl(K(1-m) - E(1-m)\bigr).>>>,
<<<ℰ(w|m) = 2E(m) − e + 2pE(m) + 2iq(K(1−m) − E(1−m)).>>>)

Compute the lattice coordinates of
m4_math(<<<w - w'>>>, <<<w − w'>>>)
and of
m4_mathcomma(<<<w + w' - 2K(m)>>>, <<<w + w' − 2K(m)>>>)
and use the case whose coordinates are integers, in practice the one
whose coordinates are closer to integers. The choice is clear-cut:
m4_math(<<<w'>>>, <<<w'>>>)
is computed to a few ulps, while the two candidates differ by
m4_mathcomma(<<<2w - 2K(m)>>>, <<<2w − 2K(m)>>>)
which is
m4_math(<<<O(1)>>>, <<<O(1)>>>)
in lattice coordinates unless
m4_math(<<<w>>>, <<<w>>>)
is congruent to
m4_mathcomma(<<<K(m)>>>, <<<K(m)>>>)
where both cases are valid and give the same value.

Both cases occur in practice. In 1600 evaluations at random
m4_math(<<<u>>>, <<<u>>>)
and
m4_mathcomma(<<<m>>>, <<<m>>>)
the second case occurred about 19% of the time, and the first with
m4_math(<<<w - w'>>>, <<<w − w'>>>)
a nonzero period about 4% of the time.

The result is

m4_displaymath(
<<<\mathcal{E}(u \mid m) = \mathcal{E}(w \mid m) + 2jE(m) + 2ik\bigl(K(1-m) - E(1-m)\bigr).>>>,
<<<ℰ(u|m) = ℰ(w|m) + 2jE(m) + 2ik(K(1−m) − E(1−m)).>>>)

Cost:
m4_mathcomma(<<<K(m)>>>, <<<K(m)>>>)
m4_mathcomma(<<<E(m)>>>, <<<E(m)>>>)
m4_mathcomma(<<<K(1-m)>>>, <<<K(1−m)>>>)
m4_mathpunc(<<<;>>>, <<<E(1-m)>>>, <<<E(1−m)>>>)
one evaluation of
m4_mathcomma(<<<s>>>, <<<s>>>)
m4_math(<<<c>>>, <<<c>>>)
and
m4_math(<<<d>>>, <<<d>>>)
together, which takes one descending Landen recursion rather than three;
one each of
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_mathdot(<<<R_D>>>, <<<R_D>>>)

@node Alternatives That Are Not Used
@chapter Alternatives That Are Not Used

@menu
* A Shift by ±iK(1−m) Away from the Poles::
* Computing ℰ from the Jacobi Amplitude::
* Lawden's Formula::
* Duplication Instead of the Reduction::
@end menu

@node A Shift by ±iK(1−m) Away from the Poles
@ifinfo
@section A Shift by ±iK(1−m) Away from the Poles
@end ifinfo
@ifhtml
@section A Shift by @math{\pm iK(1-m)} Away from the Poles
@end ifhtml
@iftex
@section A Shift by @math{@pm{}iK(1-m)} Away from the Poles
@end iftex

@w{}
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
has simple poles at
m4_mathcomma(<<<iK(1-m) + 2pK(m) + 2iqK(1-m)>>>, <<<iK(1−m) + 2pK(m) + 2iqK(1−m)>>>)
where
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
has its poles (@uref{https://dlmf.nist.gov/22.4.i, DLMF §22.4(i)}). One
could also shift
m4_math(<<<w>>>, <<<w>>>)
by
m4_mathcomma(<<<\pm iK(1-m)>>>, <<<±iK(1−m)>>>)
as @inlinefmtifelse{tex, @ref{Real m and Complex u, section 3, Real
@math{m} and Complex @math{u}}, @ref{Real m and Complex u, section 3}}
does for real
m4_mathcomma(<<<m>>>, <<<m>>>)
using

m4_displaymath(
<<<\mathcal{E}(w + s\thinspace iK(1-m) \mid m) = \mathcal{E}(w \mid m) + \frac{\mathrm{cn}(w \mid m)\thinspace\mathrm{dn}(w \mid m)}{\mathrm{sn}(w \mid m)} + s\thinspace i\bigl(K(1-m) - E(1-m)\bigr),
\qquad s = \pm 1,>>>,
<<<@example
ℰ(w + s iK(1−m)|m) = ℰ(w|m) + (cn(w|m) dn(w|m))/(sn(w|m)) + s i(K(1−m) − E(1−m)),
s = ±1,
@end example>>>,
<<<\eqalign{\mathcal{E}(w + s\thinspace iK(1-m) \mid m) &= \mathcal{E}(w \mid m) + \frac{\mathrm{cn}(w \mid m)\thinspace\mathrm{dn}(w \mid m)}{\mathrm{sn}(w \mid m)} \cr &\qquad + s\thinspace i\bigl(K(1-m) - E(1-m)\bigr), \qquad s = \pm 1,}>>>)

(@uref{https://dlmf.nist.gov/22.16.ii, DLMF §22.16(ii)}) to keep
m4_math(<<<w>>>, <<<w>>>)
away from them. It does not improve accuracy. At distance
m4_math(<<<\delta>>>, <<<δ>>>)
from the pole, both versions have relative error about
m4_mathcomma(<<<\varepsilon\thinspace\lvert u\rvert/\delta>>>, <<<ε |u|/δ>>>)
which is the conditioning of
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
near a pole:

@raggedright
@multitable @columnfractions 0.22 0.36 0.42
@headitem
m4_math(<<<\delta>>>, <<<δ>>>)
@tab
with the shift
@tab
without the shift
@item
m4_math(<<<10^{−2}>>>, <<<10⁻²>>>)
@tab
m4_math(<<<8.7\times10^{−15}>>>, <<<8.7×10⁻¹⁵>>>)
@tab
m4_math(<<<1.8\times10^{−14}>>>, <<<1.8×10⁻¹⁴>>>)
@item
m4_math(<<<10^{−4}>>>, <<<10⁻⁴>>>)
@tab
m4_math(<<<8.7\times10^{−13}>>>, <<<8.7×10⁻¹³>>>)
@tab
m4_math(<<<3.3\times10^{−13}>>>, <<<3.3×10⁻¹³>>>)
@item
m4_math(<<<10^{−6}>>>, <<<10⁻⁶>>>)
@tab
m4_math(<<<8.7\times10^{−11}>>>, <<<8.7×10⁻¹¹>>>)
@tab
m4_math(<<<2.3\times10^{−10}>>>, <<<2.3×10⁻¹⁰>>>)
@item
m4_math(<<<10^{−8}>>>, <<<10⁻⁸>>>)
@tab
m4_math(<<<8.7\times10^{−9}>>>, <<<8.7×10⁻⁹>>>)
@tab
m4_math(<<<1.9\times10^{−8}>>>, <<<1.9×10⁻⁸>>>)
@end multitable
@end raggedright

@w{}
m4_mathcomma(<<<(m = 0.5 + 0.5i>>>, <<<(m = 0.5 + 0.5i>>>)
m4_mathcomma(<<<u = iK(1-m) + \delta(1 + i)>>>, <<<u = iK(1−m) + δ(1 + i)>>>)
so
m4_math(<<<\varepsilon\thinspace\lvert u\rvert/\lvert\delta(1+i)\rvert>>>, <<<ε |u|/|δ(1+i)|>>>)
is about
m4_mathcomma(<<<1.3\times10^{-14}>>>, <<<1.3×10⁻¹⁴>>>)
m4_mathcomma(<<<10^{-12}>>>, <<<10⁻¹²>>>)
m4_mathcomma(<<<10^{-10}>>>, <<<10⁻¹⁰>>>)
m4_mathdot(<<<10^{-8}>>>, <<<10⁻⁸>>>)
Both columns follow it to within a factor of a few.)

@node Computing ℰ from the Jacobi Amplitude
@ifinfo
@section Computing ℰ from the Jacobi Amplitude
@end ifinfo
@ifhtml
@section Computing @math{\mathcal{E}} from the Jacobi Amplitude
@end ifhtml
@iftex
@section Computing @math{@scriptE} from the Jacobi Amplitude
@end iftex

For real
m4_math(<<<u>>>, <<<u>>>)
and
m4_mathcomma(<<<0 \le m < 1>>>, <<<0 ≤ m < 1>>>)
m4_math(<<<\mathcal{E}(u \mid m) = E(\mathrm{am}(u \mid m) \mid m)>>>, <<<ℰ(u|m) = E(am(u|m)|m)>>>)
(@uref{https://dlmf.nist.gov/22.16.ii, DLMF §22.16(ii)}), and
@inlinefmtifelse{tex, @ref{Real m and Real u, section 2, Real @math{m}
and Real @math{u}}, @ref{Real m and Real u, section 2}} uses this on the
reduced argument. For complex
m4_math(<<<u>>>, <<<u>>>)
or
m4_math(<<<m>>>, <<<m>>>)
it does not hold globally, even with a correct
m4_math(<<<\mathrm{am}{:}>>>, <<<am:>>>)
m4_math(<<<\mathrm{am}>>>, <<<am>>>)
follows Mathematica's branch cuts and jumps by
m4_math(<<<\pm\pi>>>, <<<±π>>>)
across them, and
m4_mathcomma(<<<E(\phi + \pi \mid m) = E(\phi \mid m) + 2E(m)>>>, <<<E(φ + π|m) = E(φ|m) + 2E(m)>>>)
so
m4_math(<<<E(\mathrm{am}(u \mid m) \mid m)>>>, <<<E(am(u|m)|m)>>>)
jumps by
m4_math(<<<\pm 2E(m)>>>, <<<±2E(m)>>>)
where
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
is continuous. On a reduced region,
m4_math(<<<E(\mathrm{am}(w \mid m) \mid m)>>>, <<<E(am(w|m)|m)>>>)
and
m4_math(<<<e>>>, <<<e>>>)
are the same principal-branch construction; they agreed to a few ulps on
a grid for 17 values of
m4_math(<<<m>>>, <<<m>>>)
and fail at the same points, so
m4_math(<<<\mathrm{am}>>>, <<<am>>>)
would not avoid @inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m),
section 6, From @math{@scriptE(w'|m)} to @math{@scriptE(w|m)}},
@ref{From ℰ(w'|m) to ℰ(w|m), section 6}}.

@node Lawden's Formula
@section Lawden's Formula

Lawden's formula of @inlinefmtifelse{tex, @ref{Real m and Complex u,
section 3, Real @math{m} and Complex @math{u}}, @ref{Real m and Complex
u, section 3}} holds for complex
m4_math(<<<m>>>, <<<m>>>)
too (checked against quadrature to 25 digits), but it reduces complex
m4_math(<<<u>>>, <<<u>>>)
to
m4_math(<<<\mathcal{E}(\mathrm{Re}\thinspace u \mid m)>>>, <<<ℰ(Re u|m)>>>)
and
m4_mathdot(<<<\mathcal{E}(\mathrm{Im}\thinspace u \mid 1-m)>>>, <<<ℰ(Im u|1−m)>>>)
For complex
m4_math(<<<m>>>, <<<m>>>)
those still need a method for real
m4_math(<<<u>>>, <<<u>>>)
and complex
m4_mathcomma(<<<m>>>, <<<m>>>)
and the method of @inlinefmtifelse{tex, @ref{Real m and Real u, section
2, Real @math{m} and Real @math{u}}, @ref{Real m and Real u, section 2}}
does not extend to that: there is no real period to reduce by.
@inlinefmtifelse{tex, @ref{Complex m - Reduction by the Periods,
Sections 4 to 6, Complex @math{m}: Reduction by the Periods},
@ref{Complex m - Reduction by the Periods, Sections 4 to 6}} handle real
and complex
m4_math(<<<u>>>, <<<u>>>)
directly.

@node Duplication Instead of the Reduction
@section Duplication Instead of the Reduction

@w{}
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_math(<<<\mathrm{cn}>>>, <<<cn>>>)
and
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
can be computed accurately for large arguments by halving
m4_mathcomma(<<<u>>>, <<<u>>>)
applying the descending Landen transformation, and doubling the result
back up with (@uref{https://dlmf.nist.gov/22.6.ii, DLMF 22.6.5–22.6.7};
A&S 16.18.1–16.18.3)

m4_displaymath(
<<<\mathrm{sn}(2v) = \frac{2scd}{1 - ms^4}, \qquad
\mathrm{cn}(2v) = \frac{c^2 - s^2d^2}{1 - ms^4}, \qquad
\mathrm{dn}(2v) = \frac{d^2 - ms^2c^2}{1 - ms^4},>>>,
<<<@example
sn(2v) = 2scd/(1 − ms⁴),
cn(2v) = (c² − s²d²)/(1 − ms⁴),
dn(2v) = (d² − ms²c²)/(1 − ms⁴),
@end example>>>)

where
m4_mathcomma(<<<s>>>, <<<s>>>)
m4_mathcomma(<<<c>>>, <<<c>>>)
m4_math(<<<d>>>, <<<d>>>)
are taken at
m4_mathdot(<<<v>>>, <<<v>>>)
After each step, restore
m4_math(<<<s^2 + c^2 = 1>>>, <<<s² + c² = 1>>>)
and
m4_math(<<<ms^2 + d^2 = 1>>>, <<<ms² + d² = 1>>>)
(@uref{https://dlmf.nist.gov/22.6.E1, DLMF 22.6.1}; A&S 16.9.1). For
m4_mathcomma(<<<\mathrm{Re}\thinspace m > 1/2>>>, <<<Re m > 1/2>>>)
use Jacobi's imaginary transformation
(@uref{https://dlmf.nist.gov/22.6.T1, DLMF Table 22.6.1}; A&S
16.20.1–16.20.3) to work with
m4_math(<<<1 - m>>>, <<<1 − m>>>)
instead.

@w{}
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
could be computed the same way, without lattice coordinates or the
correction of @inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m), section
6, From @math{@scriptE(w'|m)} to @math{@scriptE(w|m)}}, @ref{From
ℰ(w'|m) to ℰ(w|m), section 6}}. The quasi-addition formula
(@uref{https://dlmf.nist.gov/22.16.ii, DLMF §22.16(ii)})
m4_math(<<<\mathcal{E}(a + b) = \mathcal{E}(a) + \mathcal{E}(b) - m\thinspace\mathrm{sn}(a)\thinspace\mathrm{sn}(b)\thinspace\mathrm{sn}(a + b)>>>, <<<ℰ(a + b) = ℰ(a) + ℰ(b) − m sn(a) sn(b) sn(a + b)>>>)
with
m4_math(<<<a = b = v>>>, <<<a = b = v>>>)
gives

m4_displaymath(
<<<\mathcal{E}(2v \mid m) = 2\mathcal{E}(v \mid m) - m\thinspace\mathrm{sn}^2(v \mid m)\thinspace\mathrm{sn}(2v \mid m).>>>,
<<<ℰ(2v|m) = 2ℰ(v|m) − m sn²(v|m) sn(2v|m).>>>)

The method:

@enumerate
@item
Halve
m4_math(<<<u>>>, <<<u>>>)
to
m4_math(<<<v = u/2^n>>>, <<<v = u/2ⁿ>>>)
with
m4_mathdot(<<<|v| \le \min(|K(m)|, |K(1-m)|)/2>>>, <<<|v| ≤ min(|K(m)|, |K(1−m)|)/2>>>)
This is exact in binary floating point.
@item
Compute
m4_math(<<<\mathcal{E}(v \mid m)>>>, <<<ℰ(v|m)>>>)
by Carlson's forms (@inlinefmtifelse{tex, @ref{Carlson's Idea, section
5, Carlson's Idea}, @ref{Carlson's Idea, section 5}}). For
m4_math(<<<v>>>, <<<v>>>)
this small,
m4_math(<<<w' = v>>>, <<<w' = v>>>)
(exactly so in 2000 random trials), so @inlinefmtifelse{tex, @ref{From
ℰ(w'|m) to ℰ(w|m), section 6, From @math{@scriptE(w'|m)} to
@math{@scriptE(w|m)}}, @ref{From ℰ(w'|m) to ℰ(w|m), section 6}} is not
needed.
@item
Double
m4_math(<<<n>>>, <<<n>>>)
times, carrying
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
along with
m4_mathcomma(<<<s>>>, <<<s>>>)
m4_math(<<<c>>>, <<<c>>>)
and
m4_mathdot(<<<d>>>, <<<d>>>)
@end enumerate

This was tested in double precision against 40-digit references, with a
model of the doubling loop that corrects
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
only when
m4_math(<<<|d| \ge 10^{-3}>>>, <<<|d| ≥ 10⁻³>>>)
and does not use the imaginary transformation.

@strong{Accuracy.} Without the correction to
m4_math(<<<s^2 + c^2 = 1>>>, <<<s² + c² = 1>>>)
the doubling is unstable, and
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
loses accuracy with it. With the correction,
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
is about as accurate as with the reduction:

@raggedright
@multitable @columnfractions 0.19 0.24 0.15 0.20 0.23
@headitem
m4_math(<<<u>>>, <<<u>>>)
@tab
m4_math(<<<m>>>, <<<m>>>)
@tab
doublings
@tab
doubling only
@tab
with correction
@item
m4_math(<<<10^{8}>>>, <<<10⁸>>>)
@tab
m4_math(<<<0.5 + 0.5i>>>, <<<0.5 + 0.5i>>>)
@tab
27
@tab
m4_math(<<<1.1\times10^{−12}>>>, <<<1.1×10⁻¹²>>>)
@tab
m4_math(<<<3.5\times10^{−16}>>>, <<<3.5×10⁻¹⁶>>>)
@item
m4_math(<<<10^{6}>>>, <<<10⁶>>>)
@tab
m4_math(<<<0.9 + 0.001i>>>, <<<0.9 + 0.001i>>>)
@tab
21
@tab
m4_math(<<<1.3\times10^{−5}>>>, <<<1.3×10⁻⁵>>>)
@tab
m4_math(<<<5.5\times10^{−17}>>>, <<<5.5×10⁻¹⁷>>>)
@item
m4_math(<<<10^{8}>>>, <<<10⁸>>>)
@tab
m4_math(<<<0.99 + 0.0001i>>>, <<<0.99 + 0.0001i>>>)
@tab
27
@tab
m4_math(<<<1.7\times10^{−4}>>>, <<<1.7×10⁻⁴>>>)
@tab
m4_math(<<<2.4\times10^{−15}>>>, <<<2.4×10⁻¹⁵>>>)
@item
m4_math(<<<10^{6}>>>, <<<10⁶>>>)
@tab
m4_math(<<<0.999 + 0.001i>>>, <<<0.999 + 0.001i>>>)
@tab
21
@tab
m4_math(<<<5.2\times10^{−3}>>>, <<<5.2×10⁻³>>>)
@tab
m4_math(<<<6.7\times10^{−15}>>>, <<<6.7×10⁻¹⁵>>>)
@item
m4_math(<<<400 + 300i>>>, <<<400 + 300i>>>)
@tab
m4_math(<<<0.5 + 0.5i>>>, <<<0.5 + 0.5i>>>)
@tab
10
@tab
m4_math(<<<5.2\times10^{−14}>>>, <<<5.2×10⁻¹⁴>>>)
@tab
m4_math(<<<1.4\times10^{−15}>>>, <<<1.4×10⁻¹⁵>>>)
@end multitable
@end raggedright

Errors are relative. Calling
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
separately at each
m4_math(<<<u/2^k>>>, <<<u/2ᵏ>>>)
instead gives
m4_math(<<<10^{-15}>>>, <<<10⁻¹⁵>>>)
to
m4_mathcomma(<<<10^{-14}>>>, <<<10⁻¹⁴>>>)
at the cost of
m4_math(<<<n + 1>>>, <<<n + 1>>>)
calls.

Only a correctly rounded
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
would give a clear gain, since the halving is exact. With correctly
rounded
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
values the errors were about
m4_mathdot(<<<10^{-16}>>>, <<<10⁻¹⁶>>>)
But
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
computed this way is accurate to its conditioning,
m4_mathcomma(<<<\varepsilon\thinspace|u\thinspace\mathrm{sn}'(u)|>>>, <<<ε |u sn'(u)|>>>)
not correctly rounded. Both methods then give
m4_math(<<<O(\varepsilon)>>>, <<<O(ε)>>>)
relative error, because
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
grows like
m4_math(<<<u>>>, <<<u>>>)
(@inlinefmtifelse{tex, @ref{Round-Off, section 8, Round-Off},
@ref{Round-Off, section 8}}).

@strong{Poles on the path.}
m4_math(<<<\mathcal{E}(z \mid m)>>>, <<<ℰ(z|m)>>>)
has simple poles where
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
does. If some
m4_math(<<<u/2^k>>>, <<<u/2ᵏ>>>)
is within
m4_math(<<<\delta>>>, <<<δ>>>)
of a pole,
m4_math(<<<\mathcal{E}(v)>>>, <<<ℰ(v)>>>)
is about
m4_mathcomma(<<<1/\delta>>>, <<<1/δ>>>)
while
m4_math(<<<\mathcal{E}(2v)>>>, <<<ℰ(2v)>>>)
is finite. The doubling step then subtracts two terms of size
m4_math(<<<1/\delta>>>, <<<1/δ>>>)
and loses about
m4_mathcomma(<<<\varepsilon/\delta>>>, <<<ε/δ>>>)
even though
m4_math(<<<u>>>, <<<u>>>)
itself is far from any pole. Here
m4_mathcomma(<<<u = 2(p + \delta(1 + i))>>>, <<<u = 2(p + δ(1 + i))>>>)
where
m4_math(<<<p = iK(1-m)>>>, <<<p = iK(1−m)>>>)
is a pole:

@raggedright
@multitable @columnfractions 0.42 0.22 0.36
@headitem
m4_math(<<<m>>>, <<<m>>>)
@tab
m4_math(<<<\delta>>>, <<<δ>>>)
@tab
relative error
@item
m4_math(<<<0.5 + 0.5i>>>, <<<0.5 + 0.5i>>>)
@tab
m4_math(<<<10^{−3}>>>, <<<10⁻³>>>)
@tab
m4_math(<<<2.5\times10^{−13}>>>, <<<2.5×10⁻¹³>>>)
@item
m4_math(<<<0.5 + 0.5i>>>, <<<0.5 + 0.5i>>>)
@tab
m4_math(<<<10^{−5}>>>, <<<10⁻⁵>>>)
@tab
m4_math(<<<9.4\times10^{−12}>>>, <<<9.4×10⁻¹²>>>)
@item
m4_math(<<<0.5 + 0.5i>>>, <<<0.5 + 0.5i>>>)
@tab
m4_math(<<<10^{−7}>>>, <<<10⁻⁷>>>)
@tab
m4_math(<<<1.0\times10^{−9}>>>, <<<1.0×10⁻⁹>>>)
@item
0.3 (complex
m4_mathpunc(<<<)>>>, <<<u>>>, <<<u>>>)
@tab
m4_math(<<<10^{−7}>>>, <<<10⁻⁷>>>)
@tab
m4_math(<<<1.7\times10^{−9}>>>, <<<1.7×10⁻⁹>>>)
@end multitable
@end raggedright

@w{}
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_math(<<<\mathrm{cn}>>>, <<<cn>>>)
and
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
themselves are not affected. Near a pole,
m4_mathcomma(<<<s>>>, <<<s>>>)
m4_math(<<<c>>>, <<<c>>>)
and
m4_math(<<<d>>>, <<<d>>>)
are all of order
m4_mathcomma(<<<1/\delta>>>, <<<1/δ>>>)
and one term dominates in each numerator and denominator of the doubling
formulas, so nothing cancels. With
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
computed this way, the error stays within about 13 times
m4_math(<<<\varepsilon\thinspace|u\thinspace\mathrm{sn}'(u)|>>>, <<<ε |u sn'(u)|>>>)
even when an intermediate point is near a pole.

Avoiding the loss would take a quantity with no pole there, such as
m4_mathcomma(<<<\mathcal{E}(v) + \mathrm{cn}(v)\thinspace\mathrm{dn}(v)/\mathrm{sn}(v)>>>, <<<ℰ(v) + cn(v) dn(v)/sn(v)>>>)
which has no pole at
m4_mathdot(<<<iK(1-m)>>>, <<<iK(1−m)>>>)
Or it would take a fallback to the reduction.

@strong{Other costs.} For
m4_mathcomma(<<<\mathrm{Re}\thinspace m > 1/2>>>, <<<Re m > 1/2>>>)
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
would have to go through the imaginary transformation too. That
transformation adds a term
m4_math(<<<\mathrm{sn}\thinspace\mathrm{dn}/\mathrm{cn}>>>, <<<sn dn/cn>>>)
at parameter
m4_mathcomma(<<<1 - m>>>, <<<1 − m>>>)
which brings more places where large terms cancel. The base radius still
needs
m4_mathdot(<<<K(1-m)>>>, <<<K(1−m)>>>)
And
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
would depend on the internals of the doubling loop for
m4_mathdot(<<<\mathrm{sn}>>>, <<<sn>>>)

@strong{Conclusion.} The reduction is kept:

@itemize @bullet
@item
@strong{Accuracy:} duplication is at most a few times more accurate,
which does not matter for
m4_math(<<<O(\varepsilon)>>>, <<<O(ε)>>>)
relative error.
@item
@strong{Behavior:} the reduction's error follows the conditioning of
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
everywhere tested. Duplication has an extra loss near poles on the
halving path.
@item
@strong{Code:} removing that loss and handling
m4_math(<<<\mathrm{Re}\thinspace m > 1/2>>>, <<<Re m > 1/2>>>)
takes about as much code as the lattice coordinates and
@inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m), section 6, From
@math{@scriptE(w'|m)} to @math{@scriptE(w|m)}}, @ref{From ℰ(w'|m) to
ℰ(w|m), section 6}} do.
@item
@strong{Coupling:} the reduction calls
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
only at small arguments, so it does not depend on how
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
handles large arguments.
@end itemize

Duplication with exact halving would be the way to a correctly rounded
m4_mathcomma(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
if that is ever wanted. It would also need a correctly rounded
m4_mathdot(<<<\mathrm{sn}>>>, <<<sn>>>)

@node Round-Off
@chapter Round-Off

For complex
m4_mathcomma(<<<m>>>, <<<m>>>)
the reduction loses about
m4_math(<<<\lvert u\rvert\thinspace\varepsilon>>>, <<<|u| ε>>>)
in absolute terms:
m4_math(<<<K(m)>>>, <<<K(m)>>>)
and
m4_math(<<<K(1-m)>>>, <<<K(1−m)>>>)
have relative errors of a few ulps, multiplied by
m4_mathcomma(<<<j, k \approx \lvert u\rvert>>>, <<<j, k ≈ |u|>>>)
and the subtraction cancels. The lattice terms
m4_math(<<<2jE(m)>>>, <<<2jE(m)>>>)
and
m4_math(<<<2ik(K(1-m) - E(1-m))>>>, <<<2ik(K(1−m) − E(1−m))>>>)
add about as much. But
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
grows linearly in
m4_mathcomma(<<<u>>>, <<<u>>>)
so the relative error stays
m4_mathdot(<<<O(\varepsilon)>>>, <<<O(ε)>>>)
This matches the conditioning: rounding
m4_math(<<<u>>>, <<<u>>>)
to a double changes
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
by about
m4_mathcomma(<<<\varepsilon\thinspace\lvert u\thinspace\mathrm{dn}^2(u \mid m)\rvert>>>, <<<ε |u dn²(u|m)|>>>)
the same size.

Measured relative error, doubles, 128 points in four directions for
m4_mathcomma(<<<m = 0.5+0.5i>>>, <<<m = 0.5+0.5i>>>)
m4_mathcomma(<<<2+i>>>, <<<2+i>>>)
m4_mathcomma(<<<-3+0.5i>>>, <<<−3+0.5i>>>)
m4_mathcomma(<<<0.9+0.05i>>>, <<<0.9+0.05i>>>)
against 50-digit references:

@raggedright
@multitable @columnfractions 0.45 0.28 0.28
@headitem
m4_math(<<<\lvert u\rvert>>>, <<<|u|>>>)
@tab
max
@tab
median
@item
10
@tab
m4_math(<<<3.8\times10^{−15}>>>, <<<3.8×10⁻¹⁵>>>)
@tab
m4_math(<<<4.6\times10^{−16}>>>, <<<4.6×10⁻¹⁶>>>)
@item
m4_math(<<<10^{2}>>>, <<<10²>>>)
@tab
m4_math(<<<3.5\times10^{−15}>>>, <<<3.5×10⁻¹⁵>>>)
@tab
m4_math(<<<4.5\times10^{−16}>>>, <<<4.5×10⁻¹⁶>>>)
@item
m4_math(<<<10^{3}>>>, <<<10³>>>)
@tab
m4_math(<<<7.8\times10^{−15}>>>, <<<7.8×10⁻¹⁵>>>)
@tab
m4_math(<<<3.4\times10^{−16}>>>, <<<3.4×10⁻¹⁶>>>)
@item
m4_math(<<<10^{4}>>>, <<<10⁴>>>)
@tab
m4_math(<<<2.8\times10^{−15}>>>, <<<2.8×10⁻¹⁵>>>)
@tab
m4_math(<<<5.3\times10^{−16}>>>, <<<5.3×10⁻¹⁶>>>)
@item
m4_math(<<<10^{5}>>>, <<<10⁵>>>)
@tab
m4_math(<<<3.8\times10^{−15}>>>, <<<3.8×10⁻¹⁵>>>)
@tab
m4_math(<<<3.7\times10^{−16}>>>, <<<3.7×10⁻¹⁶>>>)
@item
m4_math(<<<10^{6}>>>, <<<10⁶>>>)
@tab
m4_math(<<<9.6\times10^{−15}>>>, <<<9.6×10⁻¹⁵>>>)
@tab
m4_math(<<<5.3\times10^{−16}>>>, <<<5.3×10⁻¹⁶>>>)
@item
m4_math(<<<10^{7}>>>, <<<10⁷>>>)
@tab
m4_math(<<<8.7\times10^{−15}>>>, <<<8.7×10⁻¹⁵>>>)
@tab
m4_math(<<<5.2\times10^{−16}>>>, <<<5.2×10⁻¹⁶>>>)
@item
m4_math(<<<10^{8}>>>, <<<10⁸>>>)
@tab
m4_math(<<<8.8\times10^{−15}>>>, <<<8.8×10⁻¹⁵>>>)
@tab
m4_math(<<<4.2\times10^{−16}>>>, <<<4.2×10⁻¹⁶>>>)
@end multitable
@end raggedright

If one component of the result is much smaller than
m4_mathcomma(<<<\lvert \mathcal{E}(u \mid m)\rvert>>>, <<<|ℰ(u|m)|>>>)
it still gets an absolute error of about
m4_mathcomma(<<<\lvert u\rvert\thinspace\varepsilon>>>, <<<|u| ε>>>)
so its own relative error can be large. Doing better for exact
m4_math(<<<u>>>, <<<u>>>)
would need the reduction in extra precision.

@node Validation
@chapter Validation

@itemize @bullet
@item
Quadrature references:
m4_math(<<<\int_0^u \mathrm{dn}^2(t \mid m)\thinspace dt>>>, <<<∫₀ᵘ dn²(t|m) dt>>>)
by mpmath along two different pole-free paths, accepted only when they
agree. This works because
m4_math(<<<\mathrm{dn}^2>>>, <<<dn²>>>)
has zero residues, so any two pole-free paths give the same integral.
119 points, 12 values of complex
m4_math(<<<m>>>, <<<m>>>)
(including
m4_math(<<<3 + 10^{-3}i>>>, <<<3 + 10⁻³i>>>)
and
m4_math(<<<-2 - 10^{-3}i>>>, <<<−2 − 10⁻³i>>>)
near the cuts) by 11 values of
m4_math(<<<u>>>, <<<u>>>)
up to
m4_math(<<<\lvert u\rvert \approx 14{:}>>>, <<<|u| ≈ 14:>>>)
worst relative error
m4_mathcomma(<<<1.3\times10^{-14}>>>, <<<1.3×10⁻¹⁴>>>)
most a few ulps. The larger errors come with
m4_math(<<<\lvert m\rvert>>>, <<<|m|>>>)
from 5 to 7; @inlinefmtifelse{tex, @ref{Limitations, section 10,
Limitations}, @ref{Limitations, section 10}} explains why.
@item
Large
m4_math(<<<u{:}>>>, <<<u:>>>)
the table in @inlinefmtifelse{tex, @ref{Round-Off, section 8,
Round-Off}, @ref{Round-Off, section 8}}.
@item
Large
m4_math(<<<\lvert m\rvert{:}>>>, <<<|m|:>>>)
72 points with complex
m4_mathcomma(<<<m>>>, <<<m>>>)
m4_math(<<<\lvert m\rvert>>>, <<<|m|>>>)
from 5 to
m4_math(<<<10^4>>>, <<<10⁴>>>)
in random directions, and
m4_math(<<<\lvert u\rvert>>>, <<<|u|>>>)
from 0.3 to 3, against 40-digit references: worst relative error
m4_mathcomma(<<<1.5\times10^{-13}>>>, <<<1.5×10⁻¹³>>>)
median a few ulps. 36 points with real
m4_math(<<<m = \pm 10, \pm 100, \pm 10^4>>>, <<<m = ±10, ±100, ±10⁴>>>)
and real or complex
m4_math(<<<u{:}>>>, <<<u:>>>)
worst
m4_mathcomma(<<<5.0\times10^{-12}>>>, <<<5.0×10⁻¹²>>>)
at
m4_mathpunc(<<<;>>>, <<<m = 10^4>>>, <<<m = 10⁴>>>)
see @inlinefmtifelse{tex, @ref{Limitations, section 10, Limitations},
@ref{Limitations, section 10}}.
@item
Near the cuts: 18 points with
m4_mathcomma(<<<m = 3 \pm 10^{-3}i>>>, <<<m = 3 ±10⁻³i>>>)
m4_mathcomma(<<<3 + 10^{-10}i>>>, <<<3 + 10⁻¹⁰i>>>)
m4_mathcomma(<<<-2 + 10^{-3}i>>>, <<<−2 + 10⁻³i>>>)
m4_math(<<<-2 - 10^{-10}i>>>, <<<−2 − 10⁻¹⁰i>>>)
and
m4_math(<<<50 + 10^{-6}i{:}>>>, <<<50 + 10⁻⁶i:>>>)
worst relative error
m4_mathdot(<<<3.5\times10^{-14}>>>, <<<3.5×10⁻¹⁴>>>)
m4_math(<<<K(m)>>>, <<<K(m)>>>)
and
m4_math(<<<K(1-m)>>>, <<<K(1−m)>>>)
jump across the cuts, but each side gives a valid pair of periods, and
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
is analytic in
m4_math(<<<m>>>, <<<m>>>)
there, so the method needs nothing special near them.
@item
Arbitrary precision, 32 digits: relative errors
m4_math(<<<10^{-33}>>>, <<<10⁻³³>>>)
to
m4_math(<<<10^{-31}>>>, <<<10⁻³¹>>>)
against 45-digit references.
@item
Jump test: 800 random
m4_math(<<<(u, m)>>>, <<<(u, m)>>>)
with
m4_math(<<<\lvert m\rvert>>>, <<<|m|>>>)
from 0.01 to 30 in all directions and
m4_math(<<<\lvert u\rvert>>>, <<<|u|>>>)
up to 17. At each, compare
m4_math(<<<\mathcal{E}(u+h \mid m) - \mathcal{E}(u \mid m)>>>, <<<ℰ(u+h|m) − ℰ(u|m)>>>)
with
m4_math(<<<\int_u^{u+h}\mathrm{dn}^2>>>, <<<∫_uᵘ⁺ʰ dn²>>>)
for
m4_mathdot(<<<\lvert h\rvert = 0.05>>>, <<<|h| = 0.05>>>)
A wrong branch anywhere would show as a jump of a period constant. None
found.
@end itemize

@node Limitations
@chapter Limitations

In these tests
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_math(<<<\mathrm{cn}>>>, <<<cn>>>)
and
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
were within about 50 times their conditioning,
m4_mathcomma(<<<\varepsilon\thinspace\lvert u\thinspace f'(u)/f(u)\rvert>>>, <<<ε |u f'(u)/f(u)|>>>)
for
m4_math(<<<\lvert m\rvert>>>, <<<|m|>>>)
up to
m4_mathcomma(<<<10^4>>>, <<<10⁴>>>)
so they are not what limits the accuracy. Two other effects are.

@itemize @bullet
@item
@strong{Large complex parameters.} For complex
m4_mathcomma(<<<m>>>, <<<m>>>)
the periods shrink as
m4_math(<<<\lvert m\rvert>>>, <<<|m|>>>)
grows:
m4_math(<<<\lvert K(m)\rvert>>>, <<<|K(m)|>>>)
is about 0.6 for
m4_mathdot(<<<\lvert m\rvert = 30>>>, <<<|m| = 30>>>)
So even a small
m4_math(<<<u>>>, <<<u>>>)
can have
m4_math(<<<w' \ne w>>>, <<<w' ≠ w>>>)
in @inlinefmtifelse{tex, @ref{Carlson's Idea, section 5, Carlson's
Idea}, @ref{Carlson's Idea, section 5}}, and @inlinefmtifelse{tex,
@ref{From ℰ(w'|m) to ℰ(w|m), section 6, From @math{@scriptE(w'|m)} to
@math{@scriptE(w|m)}}, @ref{From ℰ(w'|m) to ℰ(w|m), section 6}} adds
multiples of
m4_math(<<<E(m)>>>, <<<E(m)>>>)
and
m4_mathcomma(<<<K(1-m) - E(1-m)>>>, <<<K(1−m) − E(1−m)>>>)
which grow like
m4_math(<<<\sqrt{\lvert m\rvert}>>>, <<<√(|m|)>>>)
(about 5 to 6 for
m4_mathpunc(<<<).>>>, <<<\lvert m\rvert = 30>>>, <<<|m| = 30>>>)
When
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
is much smaller than these terms, their rounding errors dominate, and
the relative error is a small multiple of
m4_mathcomma(<<<\varepsilon\thinspace\lvert E(m)\rvert/\lvert\mathcal{E}(u \mid m)\rvert>>>, <<<ε |E(m)|/|ℰ(u|m)|>>>)
more than the conditioning of
m4_math(<<<\mathcal{E}>>>, <<<ℰ>>>)
alone would give. Example:
m4_mathcomma(<<<u = 0.2431 - 0.1757i>>>, <<<u = 0.2431 − 0.1757i>>>)
m4_mathcomma(<<<m = -3.065 + 29.84i>>>, <<<m = −3.065 + 29.84i>>>)
where
m4_math(<<<w - w'>>>, <<<w − w'>>>)
is a nonzero period.
m4_mathcomma(<<<\mathcal{E}(u \mid m) \approx 0.0930 - 0.1080i>>>, <<<ℰ(u|m) ≈ 0.0930 − 0.1080i>>>)
and
m4_mathdot(<<<\lvert E(m)\rvert \approx 5.6>>>, <<<|E(m)| ≈ 5.6>>>)
The relative error is
m4_mathcomma(<<<4.1\times10^{-14}>>>, <<<4.1×10⁻¹⁴>>>)
while
m4_mathcomma(<<<s>>>, <<<s>>>)
m4_mathcomma(<<<c>>>, <<<c>>>)
m4_math(<<<d>>>, <<<d>>>)
are accurate to
m4_math(<<<3\times10^{-15}>>>, <<<3×10⁻¹⁵>>>)
and
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
m4_math(<<<R_D>>>, <<<R_D>>>)
to
m4_mathdot(<<<2\times10^{-16}>>>, <<<2×10⁻¹⁶>>>)
Over the 72 points of @inlinefmtifelse{tex, @ref{Validation, section 9,
Validation}, @ref{Validation, section 9}} with large
m4_mathcomma(<<<\lvert m\rvert>>>, <<<|m|>>>)
the error was at most about 370 times
m4_math(<<<\varepsilon>>>, <<<ε>>>)
times the conditioning of
m4_mathdot(<<<\mathcal{E}>>>, <<<ℰ>>>)
@item
@strong{Real parameters above 1 with real arguments.} For real
m4_math(<<<m > 1>>>, <<<m > 1>>>)
and real
m4_mathcomma(<<<u>>>, <<<u>>>)
the formula of @inlinefmtifelse{tex, @ref{m > 1, section 2.2, @math{m >
1}}, @ref{m > 1, section 2.2}} subtracts
m4_math(<<<(m - 1)\thinspace u>>>, <<<(m − 1) u>>>)
from
m4_mathdot(<<<\sqrt{m}\thinspace\mathcal{E}(\sqrt{m}\thinspace u \mid 1/m)>>>, <<<√m ℰ(√m u|1/m)>>>)
Both are about
m4_mathcomma(<<<m u>>>, <<<m u>>>)
while
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
is of order
m4_mathcomma(<<<u>>>, <<<u>>>)
so the relative error is about
m4_mathdot(<<<\varepsilon\thinspace m\lvert u\rvert/\lvert\mathcal{E}(u \mid m)\rvert>>>, <<<ε m|u|/|ℰ(u|m)|>>>)
Example:
m4_mathcomma(<<<m = 10^4>>>, <<<m = 10⁴>>>)
m4_math(<<<u = 0.7{:}>>>, <<<u = 0.7:>>>)
m4_mathcomma(<<<\mathcal{E}(u \mid m) \approx 0.352>>>, <<<ℰ(u|m) ≈ 0.352>>>)
and the relative error is
m4_mathcomma(<<<5.0\times10^{-12}>>>, <<<5.0×10⁻¹²>>>)
about twice the estimate with
m4_mathdot(<<<\varepsilon = 2^{-53}>>>, <<<ε = 2⁻⁵³>>>)
For complex
m4_math(<<<u>>>, <<<u>>>)
the result is not small and there is no cancellation: at
m4_mathcomma(<<<m = 10^4>>>, <<<m = 10⁴>>>)
m4_math(<<<u = 2 - i>>>, <<<u = 2 − i>>>)
the error is
m4_mathdot(<<<9.5\times10^{-16}>>>, <<<9.5×10⁻¹⁶>>>)
With
m4_math(<<<s = \sqrt{m}\thinspace u>>>, <<<s = √m u>>>)
and
m4_mathcomma(<<<\mu = 1/m>>>, <<<μ = 1/m>>>)
the formula is
m4_mathcomma(<<<\sqrt{m}\thinspace\bigl(\mathcal{E}(s \mid \mu) - (1 - \mu)\thinspace s\bigr)>>>, <<<√m (ℰ(s|μ) − (1 − μ) s)>>>)
and
m4_mathdot(<<<\mathcal{E}(s \mid \mu) - (1 - \mu)\thinspace s = \mu\int_0^s \mathrm{cn}^2(t \mid \mu)\thinspace dt>>>, <<<ℰ(s|μ) − (1 − μ) s = μ∫₀ˢ cn²(t|μ) dt>>>)
Computing that integral directly would avoid the cancellation.
@end itemize

@node References
@unnumbered References

@enumerate
@item
B.@: C.@: Carlson, "Numerical computation of real or complex elliptic
integrals," @emph{Numerical Algorithms} 10 (1995), 13–26,
@uref{https://doi.org/10.1007/BF02198293, doi:10.1007/BF02198293};
preprint @uref{https://arxiv.org/abs/math/9409227, arXiv:math/9409227}.
Eqs.@: (59), (60) (numbered as in the arXiv version).
@item
NIST Digital Library of Mathematical Functions,
@uref{https://dlmf.nist.gov/19.25.i, §19.25(i)}, eqs.@: 19.25.5 and
19.25.9; and @uref{https://dlmf.nist.gov/22, chapter 22}, Jacobian
elliptic functions, in particular @uref{https://dlmf.nist.gov/22.16.ii,
§22.16(ii)}, Jacobi's epsilon function.
@item
M. Abramowitz and I. A. Stegun, @emph{Handbook of Mathematical
Functions}, chapter 16 (Jacobian elliptic functions). Equation numbers
given as "A&S" above are from it.
@end enumerate

@bye
