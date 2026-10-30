\input texinfo
@c -*- mode: texinfo -*-
@c Created by Claude to describe the complex-m case of jacobi_epsilon
@c (formerly elliptic_eu) in Maxima.
@c
@c Generated from elliptic-eu-complex-m.md, a Markdown note in the claude.ai
@c project used while implementing it.  That note will not be updated
@c anymore; this is the canonical description of the implementation at
@c the time of writing.
@c
@setfilename jacobi-epsilon.info
@settitle Computing jacobi_epsilon(u, m) for Complex m
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
@top Computing jacobi_epsilon(u, m) for Complex m
@end ifnottex

@iftex
@titlepage
@title Computing jacobi_epsilon(u, m) for Complex m
@end titlepage
@end iftex
This note explains the case of complex
m4_math(<<<m>>>, <<<m>>>)
in @code{jacobi-epsilon} in @code{src/ellipt.lisp}. It computes

m4_displaymath(
<<<\mathcal{E}(u \mid m) = \int_0^u \mathrm{dn}^2(t \mid m)\thinspace dt>>>,
<<<ℰ(u|m) = ∫₀ᵘ dn²(t|m) dt>>>)

for complex
m4_math(<<<m>>>, <<<m>>>)
and real or complex
m4_mathcomma(<<<u>>>, <<<u>>>)
in floats and bigfloats. Before this clause, @code{jacobi-epsilon}
returns
m4_math(<<<u>>>, <<<u>>>)
for
m4_math(<<<m = 0>>>, <<<m = 0>>>)
and
m4_math(<<<\tanh u>>>, <<<tanh u>>>)
for
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
and uses
m4_math(<<<\mathcal{E}(-u \mid m) = -\mathcal{E}(u \mid m)>>>, <<<ℰ(−u|m) = −ℰ(u|m)>>>)
so that the clause only sees
m4_mathcomma(<<<\mathrm{Re}\thinspace u > 0>>>, <<<Re u > 0>>>)
or
m4_math(<<<u>>>, <<<u>>>)
on the nonnegative imaginary axis. Real
m4_math(<<<m>>>, <<<m>>>)
is handled by the other two clauses of @code{jacobi-epsilon}.

@menu
* Notation::
* Outline::
* Reduction by the Periods::
* Carlson's Idea::
* From ℰ(w'|m) to ℰ(w|m)::
* Things That Were Tried and Are Not Needed::
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
is @code{jacobi_epsilon(u, m)}, Jacobi's epsilon function, formerly
@code{elliptic_eu(u, m)}. DLMF writes it with the modulus
m4_math(<<<k>>>, <<<k>>>)
as
m4_math(<<<\mathcal{E}(x, k)>>>, <<<ℰ(x, k)>>>)
(@uref{https://dlmf.nist.gov/22.16.E14, 22.16.14}); Mathematica calls it
@code{JacobiEpsilon[u, m]}.
@item
m4_math(<<<E(\phi \mid m)>>>, <<<E(φ|m)>>>)
is Legendre's incomplete integral of the second kind.
@item
m4_math(<<<E(m)>>>, <<<E(m)>>>)
is @code{elliptic_ec(m)} and
m4_math(<<<K(m)>>>, <<<K(m)>>>)
is @code{elliptic_kc(m)}.
@item
m4_mathcomma(<<<K(m)>>>, <<<K(m)>>>)
m4_mathcomma(<<<E(m)>>>, <<<E(m)>>>)
m4_math(<<<K(1-m)>>>, <<<K(1−m)>>>)
and
m4_math(<<<E(1-m)>>>, <<<E(1−m)>>>)
are @code{ell-k}, @code{ell-e}, @code{ell-kp} and @code{ell-ep} in the
code: principal values from @code{bf-elliptic-k} and
@code{bf-elliptic-ec} (Carlson's
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_mathpunc(<<<).>>>, <<<R_D>>>, <<<R_D>>>)
@item
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_mathcomma(<<<\mathrm{cn}>>>, <<<cn>>>)
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
are taken at parameter
m4_mathdot(<<<m>>>, <<<m>>>)
@end itemize

@node Outline
@chapter Outline

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
near the origin (@inlinefmtifelse{tex, @ref{Reduction by the Periods,
section 2, Reduction by the Periods}, @ref{Reduction by the Periods,
section 2}}).
@item
Compute
m4_mathcomma(<<<\mathrm{sn}(w \mid m)>>>, <<<sn(w|m)>>>)
m4_mathcomma(<<<\mathrm{cn}(w \mid m)>>>, <<<cn(w|m)>>>)
m4_math(<<<\mathrm{dn}(w \mid m)>>>, <<<dn(w|m)>>>)
with one call to @code{elliptic-sn-cn-dn-descending}.
@item
Evaluate Carlson's forms. They give
m4_math(<<<\mathcal{E}(w' \mid m)>>>, <<<ℰ(w'|m)>>>)
exactly, for a point
m4_math(<<<w'>>>, <<<w'>>>)
with
m4_math(<<<\mathrm{sn}(w' \mid m) = \mathrm{sn}(w \mid m)>>>, <<<sn(w'|m) = sn(w|m)>>>)
that need not be
m4_math(<<<w>>>, <<<w>>>)
(@inlinefmtifelse{tex, @ref{Carlson's Idea, section 3, Carlson's Idea},
@ref{Carlson's Idea, section 3}}).
@item
Recover
m4_math(<<<\mathcal{E}(w \mid m)>>>, <<<ℰ(w|m)>>>)
from
m4_math(<<<\mathcal{E}(w' \mid m)>>>, <<<ℰ(w'|m)>>>)
(@inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m), section 4, From
@math{@scriptE(w'|m)} to @math{@scriptE(w|m)}}, @ref{From ℰ(w'|m) to
ℰ(w|m), section 4}}), and add back the reduction.
@end enumerate

The parameter
m4_math(<<<m>>>, <<<m>>>)
is never changed; only
m4_math(<<<u>>>, <<<u>>>)
is reduced.

@node Reduction by the Periods
@chapter Reduction by the Periods

@w{}
m4_math(<<<\mathrm{dn}^2(u \mid m)>>>, <<<dn²(u|m)>>>)
has periods
m4_math(<<<2K(m)>>>, <<<2K(m)>>>)
and
m4_mathcomma(<<<2iK(1-m)>>>, <<<2iK(1−m)>>>)
and
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
is quasi-periodic:

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

@code{lattice-coords} solves
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
<<<\mathcal{E}(u \mid m) = \mathcal{E}(w \mid m) + 2jE(m) + 2ik\bigl(K(1-m) - E(1-m)\bigr),>>>,
<<<ℰ(u|m) = ℰ(w|m) + 2jE(m) + 2ik(K(1−m) − E(1−m)),>>>)

which is @code{lattice-e}.

@menu
* Why the Reduction Is Used::
@end menu

@node Why the Reduction Is Used
@section Why the Reduction Is Used

@inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m), Section 4, From
@math{@scriptE(w'|m)} to @math{@scriptE(w|m)}}, @ref{From ℰ(w'|m) to
ℰ(w|m), Section 4}} works for any
m4_mathcomma(<<<w>>>, <<<w>>>)
so the reduction is not needed for correctness. It was introduced
because, before bug 5726 was fixed, Maxima's
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_math(<<<\mathrm{cn}>>>, <<<cn>>>)
and
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
were not accurate for complex
m4_math(<<<m>>>, <<<m>>>)
and large arguments. The descending Landen transformation in
@code{elliptic-sn-cn-dn-descending} scales the argument by
m4_mathcomma(<<<1/\prod_n (1 + \sqrt{\mu_n})>>>, <<<1/∏_n (1 + √μ_n)>>>)
which is complex when
m4_math(<<<m>>>, <<<m>>>)
is. For
m4_math(<<<m = 0.5 + 0.5i>>>, <<<m = 0.5 + 0.5i>>>)
the factor is
m4_mathcomma(<<<0.894 - 0.170i>>>, <<<0.894 − 0.170i>>>)
so the final call to
m4_math(<<<\sin>>>, <<<sin>>>)
gets an argument whose imaginary part is about
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
in the next Landen step overflows, and @code{jacobi_sn(3000.0,
0.5+0.5*%i)} returns about
m4_math(<<<7.7\times10^{-60}>>>, <<<7.7×10⁻⁶⁰>>>)
instead of
m4_mathdot(<<<-1.2185 + 0.3331i>>>, <<<−1.2185 + 0.3331i>>>)
Without the reduction:

@raggedright
@multitable @columnfractions 0.34 0.66
@headitem
@tab
@code{jacobi_epsilon(3000.0, 0.5+0.5*%i)}
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
Along the real axis the unreduced code is accurate up to about
m4_math(<<<u = 2000>>>, <<<u = 2000>>>)
and wrong from about
m4_mathdot(<<<2500>>>, <<<2500>>>)
With a large imaginary part it fails sooner;
m4_math(<<<u = 700i>>>, <<<u = 700i>>>)
overflows. Bigfloats do not have this problem because of their exponent
range. The fix for bug 5726 has since made @code{jacobi_sn} and friends
accurate for large arguments; @inlinefmtifelse{tex, @ref{Duplication
Instead of the Reduction, section 5.4, Duplication Instead of the
Reduction}, @ref{Duplication Instead of the Reduction, section 5.4}}
explains why @code{jacobi-epsilon} keeps the reduction anyway.

@node Carlson's Idea
@chapter Carlson's Idea

With
m4_mathcomma(<<<s = \mathrm{sn}(w \mid m)>>>, <<<s = sn(w|m)>>>)
m4_mathcomma(<<<c = \mathrm{cn}(w \mid m)>>>, <<<c = cn(w|m)>>>)
m4_mathcomma(<<<d = \mathrm{dn}(w \mid m)>>>, <<<d = dn(w|m)>>>)
let

m4_displaymath(
<<<w' = s\thinspace R_F(c^2, d^2, 1),>>>,
<<<w' = s R_F(c², d², 1),>>>)

m4_displaymath(
<<<e = s\thinspace R_F(c^2, d^2, 1) - \frac{m}{3}\thinspace s^3 R_D(c^2, d^2, 1).>>>,
<<<e = s R_F(c², d², 1) − m/3 s³ R_D(c², d², 1).>>>)

Since
m4_math(<<<c^2 = 1 - s^2>>>, <<<c² = 1 − s²>>>)
and
m4_mathcomma(<<<d^2 = 1 - m s^2>>>, <<<d² = 1 − m s²>>>)
these are Carlson's forms of
m4_math(<<<F(\phi \mid m)>>>, <<<F(φ|m)>>>)
and
m4_math(<<<E(\phi \mid m)>>>, <<<E(φ|m)>>>)
with
m4_mathdot(<<<\sin\phi = s>>>, <<<sin φ = s>>>)
Carlson [1], eqs.@: (59) and (60), gives

m4_displaymath(
<<<F(\phi, k) = \sin\phi\thinspace R_F(\cos^2\phi, 1 - k^2\sin^2\phi, 1) = R_F(c - 1, c - k^2, c),>>>,
<<<F(φ, k) = sin φ R_F(cos²φ, 1 − k²sin²φ, 1) = R_F(c − 1, c − k², c),>>>)

m4_displaymath(
<<<E(\phi, k) = R_F(c - 1, c - k^2, c) - \frac{k^2}{3} R_D(c - 1, c - k^2, c),
\qquad c = \csc^2\phi .>>>,
<<<@example
E(φ, k) = R_F(c − 1, c − k², c) − k²/3 R_D(c − 1, c − k², c),
c = csc²φ.
@end example>>>)

The
m4_math(<<<\sin\phi>>>, <<<sin φ>>>)
form of
m4_math(<<<E>>>, <<<E>>>)
follows from homogeneity:
m4_math(<<<R_F>>>, <<<R_F>>>)
has degree
m4_mathcomma(<<<-1/2>>>, <<<−1/2>>>)
m4_math(<<<R_D>>>, <<<R_D>>>)
has degree
m4_mathcomma(<<<-3/2>>>, <<<−3/2>>>)
and
m4_mathdot(<<<(c - 1, c - k^2, c) = \csc^2\phi\thinspace(\cos^2\phi, 1 - k^2\sin^2\phi, 1)>>>, <<<(c − 1, c − k², c) = csc²φ (cos²φ, 1 − k²sin²φ, 1)>>>)
This is also the form in the comment above @code{bf-elliptic-e}.

So
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

along the same path, with the branches that
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_D>>>, <<<R_D>>>)
use. Substitute
m4_mathcomma(<<<t = \mathrm{sn}(v \mid m)>>>, <<<t = sn(v|m)>>>)
m4_mathdot(<<<dt = \mathrm{cn}(v \mid m)\thinspace\mathrm{dn}(v \mid m)\thinspace dv>>>, <<<dt = cn(v|m) dn(v|m) dv>>>)
The first integral becomes
m4_math(<<<\int_0^{w'} dv>>>, <<<∫₀^(w') dv>>>)
and the second
m4_mathcomma(<<<\int_0^{w'} \mathrm{dn}^2(v \mid m)\thinspace dv>>>, <<<∫₀^(w') dn²(v|m) dv>>>)
so

m4_displaymath(
<<<e = \mathcal{E}(w' \mid m), \qquad \mathrm{sn}(w' \mid m) = s .>>>,
<<<e = ℰ(w'|m), sn(w'|m) = s.>>>)

@w{}
m4_math(<<<w'>>>, <<<w'>>>)
is the principal inverse of
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
and it need not be
m4_mathdot(<<<w>>>, <<<w>>>)
Example:
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
m4_mathdot(<<<2iK(1-m)>>>, <<<2iK(1−m)>>>)
So
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

With the quasi-periodicity of @inlinefmtifelse{tex, @ref{Reduction by
the Periods, section 2, Reduction by the Periods}, @ref{Reduction by the
Periods, section 2}} and
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

The code computes the lattice coordinates of
m4_math(<<<w - w'>>>, <<<w − w'>>>)
and of
m4_math(<<<w + w' - 2K(m)>>>, <<<w + w' − 2K(m)>>>)
and uses the case whose coordinates are integers (@code{off-lattice}
picks the one closer to integers). The choice is clear-cut:
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
one call to @code{elliptic-sn-cn-dn-descending}; one each of
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_mathdot(<<<R_D>>>, <<<R_D>>>)
The single call returns the same
m4_mathcomma(<<<s>>>, <<<s>>>)
m4_math(<<<c>>>, <<<c>>>)
and
m4_math(<<<d>>>, <<<d>>>)
as @code{sn}, @code{cn} and @code{dn} (bit for bit), which for complex
m4_math(<<<m>>>, <<<m>>>)
would each run the descending recursion.

@node Things That Were Tried and Are Not Needed
@chapter Things That Were Tried and Are Not Needed

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
m4_mathdot(<<<iK(1-m) + 2pK(m) + 2iqK(1-m)>>>, <<<iK(1−m) + 2pK(m) + 2iqK(1−m)>>>)
An earlier version also shifted
m4_math(<<<w>>>, <<<w>>>)
by
m4_mathcomma(<<<\pm iK(1-m)>>>, <<<±iK(1−m)>>>)
using

m4_displaymath(
<<<\mathcal{E}(w + s\thinspace iK(1-m) \mid m) = \mathcal{E}(w \mid m) + \frac{\mathrm{cn}(w \mid m)\thinspace\mathrm{dn}(w \mid m)}{\mathrm{sn}(w \mid m)} + s\thinspace i\bigl(K(1-m) - E(1-m)\bigr),
\qquad s = \pm 1,>>>,
<<<@example
ℰ(w + s iK(1−m)|m) = ℰ(w|m) + (cn(w|m) dn(w|m))/(sn(w|m)) + s i(K(1−m) − E(1−m)),
s = ±1,
@end example>>>,
<<<\eqalign{\mathcal{E}(w + s\thinspace iK(1-m) \mid m) &= \mathcal{E}(w \mid m) + \frac{\mathrm{cn}(w \mid m)\thinspace\mathrm{dn}(w \mid m)}{\mathrm{sn}(w \mid m)} \cr &\qquad + s\thinspace i\bigl(K(1-m) - E(1-m)\bigr), \qquad s = \pm 1,}>>>)

to keep
m4_math(<<<w>>>, <<<w>>>)
away from them. It does not improve accuracy. At distance
m4_math(<<<\delta>>>, <<<δ>>>)
from the pole, both versions have relative error about
m4_mathcomma(<<<\varepsilon\thinspace\lvert u\rvert/\delta>>>, <<<ε |u|/δ>>>)
which is the conditioning of
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
near a pole:

@raggedright
@multitable @columnfractions 0.20 0.32 0.48
@headitem
m4_math(<<<\delta>>>, <<<δ>>>)
@tab
with the shift
@tab
without (current code)
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
m4_mathcomma(<<<\mathcal{E}(u \mid m) = E(\mathrm{am}(u \mid m) \mid m)>>>, <<<ℰ(u|m) = E(am(u|m)|m)>>>)
and the code for real
m4_math(<<<m>>>, <<<m>>>)
uses this on its reduced argument. For complex
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
section 4, From @math{@scriptE(w'|m)} to @math{@scriptE(w|m)}},
@ref{From ℰ(w'|m) to ℰ(w|m), section 4}}.

@node Lawden's Formula
@section Lawden's Formula

The clause for real
m4_math(<<<m>>>, <<<m>>>)
and complex
m4_math(<<<u>>>, <<<u>>>)
uses Lawden's formula, the addition theorem with
m4_math(<<<\mathrm{sn}(iv \mid m) = i\thinspace\mathrm{sc}(v \mid 1-m)>>>, <<<sn(iv|m) = i sc(v|1−m)>>>)
and Jacobi's imaginary transformation of
m4_mathdot(<<<\mathcal{E}>>>, <<<ℰ>>>)
It holds for complex
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
which the code for real
m4_math(<<<m>>>, <<<m>>>)
cannot provide: there is no real period to reduce by, and @code{(> m 1)}
and @code{(< m1 1)} have no method for complex numbers. The clause for
complex
m4_math(<<<m>>>, <<<m>>>)
handles real and complex
m4_math(<<<u>>>, <<<u>>>)
directly.

@node Duplication Instead of the Reduction
@section Duplication Instead of the Reduction

The fix for bug 5726 makes
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_math(<<<\mathrm{cn}>>>, <<<cn>>>)
and
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
accurate for large arguments. It halves
m4_mathcomma(<<<u>>>, <<<u>>>)
applies the descending Landen transformation, and doubles the result
back up with

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
After each step it restores
m4_math(<<<s^2 + c^2 = 1>>>, <<<s² + c² = 1>>>)
and
m4_mathdot(<<<ms^2 + d^2 = 1>>>, <<<ms² + d² = 1>>>)
For
m4_math(<<<\mathrm{Re}\thinspace m > 1/2>>>, <<<Re m > 1/2>>>)
it uses Jacobi's imaginary transformation to work with
m4_math(<<<1 - m>>>, <<<1 − m>>>)
instead.

@w{}
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
could be computed the same way, without lattice coordinates or the
correction of @inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m), section
4, From @math{@scriptE(w'|m)} to @math{@scriptE(w|m)}}, @ref{From
ℰ(w'|m) to ℰ(w|m), section 4}}. The addition formula
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
3, Carlson's Idea}, @ref{Carlson's Idea, section 3}}). For
m4_math(<<<v>>>, <<<v>>>)
this small,
m4_math(<<<w' = v>>>, <<<w' = v>>>)
(exactly so in 2000 random trials), so @inlinefmtifelse{tex, @ref{From
ℰ(w'|m) to ℰ(w|m), section 4, From @math{@scriptE(w'|m)} to
@math{@scriptE(w|m)}}, @ref{From ℰ(w'|m) to ℰ(w|m), section 4}} is not
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

This was tested in double precision against 40-digit references. The
tests used a Python model of the doubling loop as of commit d97d3e8430
(with
m4_math(<<<\mathrm{dn}>>>, <<<dn>>>)
corrected only when
m4_mathpunc(<<<),>>>, <<<|d| \ge 10^{-3}>>>, <<<|d| ≥ 10⁻³>>>)
before the imaginary transformation was added.

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
But the
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
of bug 5726 is accurate to its conditioning,
m4_mathcomma(<<<\varepsilon\thinspace|u\thinspace\mathrm{sn}'(u)|>>>, <<<ε |u sn'(u)|>>>)
not correctly rounded. Both methods then give
m4_math(<<<O(\varepsilon)>>>, <<<O(ε)>>>)
relative error, because
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
grows like
m4_math(<<<u>>>, <<<u>>>)
(@inlinefmtifelse{tex, @ref{Round-Off, section 6, Round-Off},
@ref{Round-Off, section 6}}).

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
formulas, so nothing cancels. With the
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
of bug 5726, the error stays within about 13 times
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
would depend on the internals of the
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
loop, through a variant of @code{descending-landen-duplication} that
also returns
m4_mathdot(<<<\mathcal{E}>>>, <<<ℰ>>>)

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
@inlinefmtifelse{tex, @ref{From ℰ(w'|m) to ℰ(w|m), section 4, From
@math{@scriptE(w'|m)} to @math{@scriptE(w|m)}}, @ref{From ℰ(w'|m) to
ℰ(w|m), section 4}} do.
@item
@strong{Coupling:} the reduction calls
m4_math(<<<\mathrm{sn}>>>, <<<sn>>>)
only at small arguments, so it does not depend on the large-argument
code of bug 5726.
@end itemize

Duplication with exact halving would be the way to a correctly rounded
m4_mathcomma(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
if that is ever wanted. It would also need a correctly rounded
m4_mathdot(<<<\mathrm{sn}>>>, <<<sn>>>)

@node Round-Off
@chapter Round-Off

The reduction loses about
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
119 points, 12 values of
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
from 5 to 7 and are probably from
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_mathcomma(<<<\mathrm{cn}>>>, <<<cn>>>)
m4_mathdot(<<<\mathrm{dn}>>>, <<<dn>>>)
@item
Large
m4_math(<<<u{:}>>>, <<<u:>>>)
the table in @inlinefmtifelse{tex, @ref{Round-Off, section 6,
Round-Off}, @ref{Round-Off, section 6}}.
@item
Bigfloats at @code{fpprec:32}: relative errors
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
@item
@code{tests/rtest_elliptic.mac} has float and bigfloat tests for complex
m4_math(<<<m{:}>>>, <<<m:>>>)
m4_math(<<<u>>>, <<<u>>>)
not reduced with
m4_mathcomma(<<<w' = w>>>, <<<w' = w>>>)
for complex and for real
m4_mathpunc(<<<;>>>, <<<u>>>, <<<u>>>)
m4_math(<<<u>>>, <<<u>>>)
reduced with
m4_mathpunc(<<<;>>>, <<<w + w' - 2K(m) = 0>>>, <<<w + w' − 2K(m) = 0>>>)
m4_math(<<<u>>>, <<<u>>>)
reduced with
m4_mathpunc(<<<;>>>, <<<w' = w>>>, <<<w' = w>>>)
and
m4_mathcomma(<<<u = 3000>>>, <<<u = 3000>>>)
which needs the reduction. Tolerances are the errors observed with SBCL.
@end itemize

@node Limitations
@chapter Limitations

@itemize @bullet
@item
For
m4_math(<<<m>>>, <<<m>>>)
just off the cuts
m4_math(<<<(m > 1>>>, <<<(m > 1>>>)
or
m4_math(<<<m < 0>>>, <<<m < 0>>>)
with a tiny imaginary part),
m4_math(<<<K(m)>>>, <<<K(m)>>>)
and
m4_math(<<<K(1-m)>>>, <<<K(1−m)>>>)
jump across the cut, but each side gives a valid pair of periods, and
m4_math(<<<\mathcal{E}(u \mid m)>>>, <<<ℰ(u|m)>>>)
is analytic in
m4_math(<<<m>>>, <<<m>>>)
there. Tested at
m4_math(<<<3 + 10^{-3}i>>>, <<<3 + 10⁻³i>>>)
and
m4_mathdot(<<<-2 - 10^{-3}i>>>, <<<−2 − 10⁻³i>>>)
@item
Accuracy for large
m4_math(<<<\lvert m\rvert>>>, <<<|m|>>>)
is limited by
m4_mathcomma(<<<\mathrm{sn}>>>, <<<sn>>>)
m4_mathcomma(<<<\mathrm{cn}>>>, <<<cn>>>)
m4_mathdot(<<<\mathrm{dn}>>>, <<<dn>>>)
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
@uref{https://dlmf.nist.gov/19.25, §19.25(i)}, eq.@: 19.25.5.
@end enumerate

@bye
