\input texinfo
@c -*- mode: texinfo -*-
@c Created by Claude to describe how Maxima computes complex values of
@c elliptic_pi for n > 1.
@c
@c Generated from elliptic-pi-complex-values.md.
@c
@setfilename elliptic-pi-complex-values.info
@settitle Complex Values of elliptic_pi for n > 1
@documentencoding UTF-8

@c Definitions so that plain TeX accepts the same formulas as MathJax.
@tex
\gdef\lvert{\vert}
\gdef\rvert{\vert}
\gdef\mathrm#1{{\rm #1}}
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
@top Complex Values of @code{elliptic_pi} for @math{n > 1}
@end ifnottex

@iftex
@titlepage
@title Complex Values of @code{elliptic_pi} for @math{n > 1}
@end titlepage
@end iftex

This note covers how Maxima should evaluate the incomplete elliptic
integral of the third kind,

m4_displaymath(
<<<\Pi(n;\varphi\thinspace|\thinspace m) = \int_0^{\varphi} \frac{d\theta}{\left(1 - n\sin^2\theta\right)\sqrt{1 - m\sin^2\theta}},>>>,
<<<Π(n; φ | m) = ∫₀^φ dθ/((1 − n sin²θ) √(1 − m sin²θ)),>>>)

for real
m4_mathcomma(<<<n > 1>>>, <<<n > 1>>>)
where the integrand has poles on the path of integration.  It
explains:

@itemize @bullet
@item
what the existing code returns;
@item
what Mathematica, Wolfram|Alpha and mpmath return;
@item
how the Sokhotski--Plemelj formula connects the two;
@item
how the result was implemented and tested.
@end itemize

@menu
* The Problem::
* What the Carlson Code Computes::
* The Sokhotski-Plemelj Formula::
* Applying It to the Integral::
* The Same Result as a Contour Indentation::
* A Check from the Change-of-Parameter Transformation::
* Regions of the Real Parameter Space::
* Implementation::
* Tests::
* Known Limitations::
* References::
@end menu

@node The Problem
@chapter The Problem

When
m4_mathcomma(<<<n > 1>>>, <<<n > 1>>>)
the factor
m4_math(<<<1 - n\sin^2\theta>>>, <<<1 − n sin²θ>>>)
vanishes at

m4_displaymath(
<<<\theta^{\ast} = \sin^{-1}\frac{1}{\sqrt{n}}>>>,
<<<θ* = sin⁻¹(1/√n)>>>)

and at
m4_math(<<<\pi - \theta^{\ast}>>>, <<<π − θ*>>>)
and their translates by
m4_mathdot(<<<k\pi>>>, <<<kπ>>>)
Once
m4_math(<<<\varphi > \theta^{\ast}>>>, <<<φ > θ*>>>)
the integral does not exist in the ordinary sense.  There are two
common conventions for assigning it a value:

@itemize @bullet
@item
@strong{The Cauchy principal value (CPV).}  This is real.
@item
@strong{The analytic continuation from one side of the branch cut}
m4_mathdot(<<<n \in (1, \infty)>>>, <<<n ∈ (1, ∞)>>>)
This is complex.
@end itemize

Mathematica, Wolfram|Alpha and mpmath use the analytic continuation.
For example,

m4_displaymath(
<<<\Pi(1.5\thinspace|\thinspace 0.5) = -0.45672031345290990\ldots \ -\  2.72069904635132678\ldots\thinspace  i .>>>,
<<<Π(1.5 | 0.5) = −0.45672031345290990… − 2.72069904635132678… i.>>>)

Before this change, Maxima returned only the real part,
m4_mathcomma(<<<-0.45672\ldots>>>, <<<−0.45672…>>>)
which is the CPV.  With the change:

@example maxima
(%i1) elliptic_pi(1.5, 1.25, 0.5);
(%o1)          0.5452480537461959 - 2.7206990463513265 %i
(%i2) elliptic_pi(1.5, 0.625, 0.5);
(%o2)                  0.8273873723894483
(%i3) fpprec : 20$
(%i4) elliptic_pi(1.5b0, 1.25b0, 0.5b0);
(%o4)     5.4524805374619554147b-1 - 2.7206990463513267759b0 %i
@end example

@node What the Carlson Code Computes
@chapter What the Carlson Code Computes

Maxima evaluates
m4_math(<<<\Pi>>>, <<<Π>>>)
through Carlson's symmetric integrals:

m4_displaymath(
<<<\eqalign{\Pi(n;\varphi\thinspace|\thinspace m) &= \sin\varphi\  R_F\negthinspace\left(c^2,\thinspace  1 - m s^2,\thinspace  1\right) \cr &\quad + \frac{n}{3}\thinspace  s^3\thinspace  R_J\negthinspace\left(c^2,\thinspace  1 - m s^2,\thinspace  1,\thinspace  1 - n s^2\right),}>>>,
<<<@example
Π(n; φ | m) = sin φ R_F(c², 1 − m s², 1)
              + (n/3) s³ R_J(c², 1 − m s², 1, 1 − n s²),
@end example>>>)

where
m4_math(<<<s = \sin\varphi>>>, <<<s = sin φ>>>)
and
m4_mathdot(<<<c = \cos\varphi>>>, <<<c = cos φ>>>)
When
m4_mathcomma(<<<n s^2 > 1>>>, <<<n s² > 1>>>)
the fourth argument
m4_math(<<<p = 1 - n s^2>>>, <<<p = 1 − n s²>>>)
of
m4_math(<<<R_J>>>, <<<R_J>>>)
is negative.

@code{bf-rj} handles real
m4_math(<<<x, y, z \ge 0>>>, <<<x, y, z ≥ 0>>>)
with
m4_math(<<<p < 0>>>, <<<p < 0>>>)
through Carlson (1995), eq.@: (33), which is the same as
@uref{https://dlmf.nist.gov/19.20.E14, DLMF 19.20.14}:

m4_displaymath(
<<<\eqalign{(y + q)\thinspace  R_J(x, y, z, -q) &= (p - y)\thinspace  R_J(x, y, z, p) - 3 R_F(x, y, z) \cr &\quad + 3\sqrt{\frac{xyz}{xz + pq}}\  R_C(xz + pq,\thinspace  pq),}>>>,
<<<@example
(y + q) R_J(x, y, z, −q) = (p − y) R_J(x, y, z, p) − 3 R_F(x, y, z)
                           + 3 √(xyz/(xz + pq)) R_C(xz + pq, pq),
@end example>>>)

with

m4_displaymath(
<<<p = y + \frac{(z - y)(y - x)}{y + q},>>>,
<<<p = y + (z − y)(y − x)/(y + q),>>>)

and
m4_math(<<<x, y, z>>>, <<<x, y, z>>>)
permuted so that
m4_mathdot(<<<(z - y)(y - x) > 0>>>, <<<(z − y)(y − x) > 0>>>)
Carlson states that this gives the Cauchy principal value, which is
real.  So the existing code returns the CPV of
m4_math(<<<\Pi>>>, <<<Π>>>)
whenever this branch is taken.

The real part was verified independently: a brute-force symmetric
cut-out around the pole converges to the same value.  For
m4_mathcomma(<<<\Pi(1.5;\thinspace 1.2\thinspace|\thinspace 0.5)>>>, <<<Π(1.5; 1.2 | 0.5)>>>)
both give
m4_mathdot(<<<0.75009395785731911\ldots>>>, <<<0.75009395785731911…>>>)

Carlson (1995) does not say how to get the imaginary part, and it does
not treat the complete case
m4_mathdot(<<<x = 0>>>, <<<x = 0>>>)
The next chapters derive the imaginary part directly from the
integral.

@node The Sokhotski-Plemelj Formula
@chapter The Sokhotski--Plemelj Formula

Let
m4_mathcomma(<<<a < 0 < b>>>, <<<a < 0 < b>>>)
and let
m4_math(<<<f>>>, <<<f>>>)
be differentiable near
m4_math(<<<0>>>, <<<0>>>)
(H@"older continuity is enough).  Then

m4_displaymath(
<<<\eqalign{\lim_{\varepsilon \to 0^{+}} \int_a^b \frac{f(x)}{x \pm i\varepsilon}\thinspace  dx &= \mathrm{PV}\negthinspace\int_a^b \frac{f(x)}{x}\thinspace  dx \cr &\quad \mp\  i\pi f(0).}>>>,
<<<@example
lim[ε → 0⁺] ∫ₐᵇ f(x)/(x ± iε) dx = PV ∫ₐᵇ f(x)/x dx ∓ iπ f(0).
@end example>>>)

The real part of the limit is the principal value.  The imaginary
part is
m4_math(<<<\pi>>>, <<<π>>>)
times the regular factor of the integrand, evaluated at the
singularity.

To see why, split the kernel into real and imaginary parts:

m4_displaymath(
<<<\frac{1}{x \pm i\varepsilon} = \frac{x}{x^2 + \varepsilon^2} \mp i\thinspace\frac{\varepsilon}{x^2 + \varepsilon^2} .>>>,
<<<1/(x ± iε) = x/(x² + ε²) ∓ i ε/(x² + ε²).>>>)

@itemize @bullet
@item
The real part equals
m4_math(<<<1/x>>>, <<<1/x>>>)
except within about
m4_math(<<<\varepsilon>>>, <<<ε>>>)
of
m4_mathcomma(<<<0>>>, <<<0>>>)
where it stays bounded and is odd.  Integrated against
m4_mathcomma(<<<f>>>, <<<f>>>)
it tends to the principal value.
@item
The imaginary part is a peak of width about
m4_math(<<<\varepsilon>>>, <<<ε>>>)
at
m4_mathcomma(<<<0>>>, <<<0>>>)
with total area
m4_mathdot(<<<\int_{-\infty}^{\infty} \varepsilon/(x^2 + \varepsilon^2)\thinspace dx = \pi>>>, <<<∫ ε/(x² + ε²) dx = π over (−∞, ∞)>>>)
As
m4_math(<<<\varepsilon \to 0>>>, <<<ε → 0>>>)
it only sees
m4_mathcomma(<<<f(0)>>>, <<<f(0)>>>)
so it contributes
m4_mathdot(<<<\mp i\pi f(0)>>>, <<<∓ iπ f(0)>>>)
@end itemize

@node Applying It to the Integral
@chapter Applying It to the Integral

Write the integrand as
m4_mathcomma(<<<h/g>>>, <<<h/g>>>)
where

m4_displaymath(
<<<\eqalign{g(\theta) &= 1 - n\sin^2\theta, \cr h(\theta) &= \frac{1}{\sqrt{1 - m\sin^2\theta}} .}>>>,
<<<@example
g(θ) = 1 − n sin²θ,
h(θ) = 1/√(1 − m sin²θ).
@end example>>>)

For
m4_mathcomma(<<<n > 1>>>, <<<n > 1>>>)
m4_math(<<<g>>>, <<<g>>>)
has simple zeros at
m4_mathcomma(<<<\theta^{\ast}>>>, <<<θ*>>>)
m4_mathcomma(<<<\pi - \theta^{\ast}>>>, <<<π − θ*>>>)
and their translates by
m4_mathdot(<<<k\pi>>>, <<<kπ>>>)
For
m4_mathcomma(<<<m < n>>>, <<<m < n>>>)
m4_math(<<<h>>>, <<<h>>>)
is smooth near each of them.

@menu
* Step 1 Move n Off the Real Axis::
* Step 2 Apply the Formula at Each Pole::
* Step 3 Evaluate at the Pole::
* Step 4 Count the Poles::
@end menu

@node Step 1 Move n Off the Real Axis
@section Step 1: Move @math{n} Off the Real Axis

Replace
m4_math(<<<n>>>, <<<n>>>)
by
m4_mathpunc(<<<:>>>, <<<n - i\varepsilon>>>, <<<n − iε>>>)

m4_displaymath(
<<<1 - (n - i\varepsilon)\sin^2\theta = g(\theta) + i\varepsilon\sin^2\theta .>>>,
<<<1 − (n − iε) sin²θ = g(θ) + iε sin²θ.>>>)

Near the pole,
m4_mathcomma(<<<1 - n\sin^2\theta \approx 0>>>, <<<1 − n sin²θ ≈ 0>>>)
so
m4_mathdot(<<<\sin^2\theta \approx 1/n > 0>>>, <<<sin²θ ≈ 1/n > 0>>>)
The denominator is therefore
m4_math(<<<g(\theta) + i\varepsilon'>>>, <<<g(θ) + iε′>>>)
with
m4_mathcomma(<<<\varepsilon' = \varepsilon\sin^2\theta \approx \varepsilon/n > 0>>>, <<<ε′ = ε sin²θ ≈ ε/n > 0>>>)
which has the
m4_math(<<<+i\varepsilon>>>, <<<+iε>>>)
form of @inlinefmtifelse{tex, @ref{The Sokhotski-Plemelj Formula,
section 3, The Sokhotski--Plemelj Formula}, @ref{The Sokhotski-Plemelj
Formula, section 3}}.  Away from the poles
m4_mathcomma(<<<g \ne 0>>>, <<<g ≠ 0>>>)
and the extra term vanishes in the limit.

@node Step 2 Apply the Formula at Each Pole
@section Step 2: Apply the Formula at Each Pole

Take a small interval
m4_math(<<<(\theta_k - \delta,\ \theta_k + \delta)>>>, <<<(θₖ − δ, θₖ + δ)>>>)
around a pole
m4_mathcomma(<<<\theta_k>>>, <<<θₖ>>>)
and substitute
m4_mathcomma(<<<u = g(\theta)>>>, <<<u = g(θ)>>>)
so that
m4_mathdot(<<<d\theta = du/g'(\theta)>>>, <<<dθ = du/g′(θ)>>>)
At
m4_mathcomma(<<<\theta^{\ast}>>>, <<<θ*>>>)
m4_mathcomma(<<<g' < 0>>>, <<<g′ < 0>>>)
so
m4_math(<<<u>>>, <<<u>>>)
decreases from
m4_math(<<<b = g(\theta^{\ast} - \delta) > 0>>>, <<<b = g(θ* − δ) > 0>>>)
to
m4_math(<<<a = g(\theta^{\ast} + \delta) < 0>>>, <<<a = g(θ* + δ) < 0>>>)
as
m4_math(<<<\theta>>>, <<<θ>>>)
increases.  Reversing the limits absorbs the sign:

m4_displaymath(
<<<\eqalign{\int_{\theta^{\ast} - \delta}^{\theta^{\ast} + \delta} \frac{h(\theta)}{g(\theta) + i\varepsilon'}\thinspace d\theta \cr &= \int_{a}^{b} \frac{h(\theta(u))}{\lvert g'(\theta(u))\rvert}\cdot\frac{du}{u + i\varepsilon'} .}>>>,
<<<@example
∫ h(θ)/(g(θ) + iε′) dθ over (θ* − δ, θ* + δ)
  = ∫ₐᵇ h(θ(u))/|g′(θ(u))| · du/(u + iε′).
@end example>>>)

At poles where
m4_mathcomma(<<<g' > 0>>>, <<<g′ > 0>>>)
such as
m4_mathcomma(<<<\pi - \theta^{\ast}>>>, <<<π − θ*>>>)
m4_math(<<<u>>>, <<<u>>>)
increases and the same expression results directly.  This is now the
integral of @inlinefmtifelse{tex, @ref{The Sokhotski-Plemelj Formula,
section 3, The Sokhotski--Plemelj Formula}, @ref{The Sokhotski-Plemelj
Formula, section 3}}, with
m4_math(<<<f(u) = h(\theta(u))/\lvert g'(\theta(u))\rvert>>>, <<<f(u) = h(θ(u))/|g′(θ(u))|>>>)
and
m4_mathdot(<<<f(0) = h(\theta_k)/\lvert g'(\theta_k)\rvert>>>, <<<f(0) = h(θₖ)/|g′(θₖ)|>>>)
The parts of the path away from the poles have no singularity and need
nothing special.  Adding everything up gives

m4_displaymath(
<<<\eqalign{\Pi(n - i0;\varphi\thinspace|\thinspace m) &= \mathrm{PV}\negthinspace\int_0^{\varphi} \frac{h(\theta)}{g(\theta)}\thinspace  d\theta \cr &\quad -\  i\pi \sum_{\theta_k \in (0,\varphi)} \frac{h(\theta_k)}{\lvert g'(\theta_k)\rvert} .}>>>,
<<<@example
Π(n − i0; φ | m) = PV ∫₀^φ h(θ)/g(θ) dθ
                   − iπ Σ h(θₖ)/|g′(θₖ)| over θₖ ∈ (0, φ).
@end example>>>)

@node Step 3 Evaluate at the Pole
@section Step 3: Evaluate at the Pole

From
m4_math(<<<\sin\theta^{\ast} = 1/\sqrt{n}>>>, <<<sin θ* = 1/√n>>>)
and
m4_mathpunc(<<<:>>>, <<<\cos\theta^{\ast} = \sqrt{(n-1)/n}>>>, <<<cos θ* = √((n − 1)/n)>>>)

m4_displaymath(
<<<\eqalign{\lvert g'(\theta^{\ast})\rvert &= 2n\sin\theta^{\ast}\cos\theta^{\ast} = 2\sqrt{n - 1}, \cr h(\theta^{\ast}) &= \frac{1}{\sqrt{1 - m/n}} = \sqrt{\frac{n}{n - m}} .}>>>,
<<<@example
|g′(θ*)| = 2n sin θ* cos θ* = 2√(n − 1),
h(θ*) = 1/√(1 − m/n) = √(n/(n − m)).
@end example>>>)

Each pole therefore contributes

m4_displaymath(
<<<-\thinspace i\pi\thinspace\frac{\sqrt{n/(n - m)}}{2\sqrt{n - 1}} = -\thinspace i\thinspace\frac{\pi}{2}\sqrt{\frac{n}{(n - 1)(n - m)}} .>>>,
<<<−iπ √(n/(n − m))/(2√(n − 1)) = −i (π/2) √(n/((n − 1)(n − m))).>>>)

@node Step 4 Count the Poles
@section Step 4: Count the Poles

At every zero of
m4_mathcomma(<<<g>>>, <<<g>>>)
the values of
m4_math(<<<\sin^2\theta>>>, <<<sin²θ>>>)
and
m4_math(<<<\lvert\sin\theta\cos\theta\rvert>>>, <<<|sin θ cos θ|>>>)
are the same, so every pole contributes the same amount.  With

m4_displaymath(
<<<\sigma = \frac{\pi}{2}\sqrt{\frac{n}{(n - 1)(n - m)}},>>>,
<<<σ = (π/2) √(n/((n − 1)(n − m))),>>>)

the imaginary part is
m4_math(<<<-\sigma>>>, <<<−σ>>>)
times the number of poles crossed between
m4_math(<<<0>>>, <<<0>>>)
and
m4_mathcomma(<<<\varphi>>>, <<<φ>>>)
counted with the sign of
m4_mathdot(<<<\varphi>>>, <<<φ>>>)
Integrating over negative
m4_math(<<<\varphi>>>, <<<φ>>>)
runs the path backwards, which is why
m4_math(<<<\Pi>>>, <<<Π>>>)
is odd in
m4_mathdot(<<<\varphi>>>, <<<φ>>>)

@strong{Check.}  For
m4_math(<<<n = 1.5>>>, <<<n = 1.5>>>)
and
m4_mathpunc(<<<:>>>, <<<m = 0.5>>>, <<<m = 0.5>>>)

m4_displaymath(
<<<\sigma = \frac{\pi}{2}\sqrt{\frac{1.5}{0.5 \cdot 1.0}} = \frac{\pi}{2}\sqrt{3} = 2.72069904635132678\ldots>>>,
<<<σ = (π/2) √(1.5/(0.5 · 1.0)) = (π/2) √3 = 2.72069904635132678…>>>)

This matches the imaginary part of
m4_math(<<<\Pi(1.5\thinspace|\thinspace 0.5)>>>, <<<Π(1.5 | 0.5)>>>)
in @inlinefmtifelse{tex, @ref{The Problem, section 1, The Problem},
@ref{The Problem, section 1}}.

@node The Same Result as a Contour Indentation
@chapter The Same Result as a Contour Indentation

With
m4_mathcomma(<<<n \to n - i\varepsilon>>>, <<<n → n − iε>>>)
the pole moves to

m4_displaymath(
<<<\theta \approx \theta^{\ast} + i\varepsilon\thinspace\frac{\sin^2\theta^{\ast}}{\lvert g'(\theta^{\ast})\rvert},>>>,
<<<θ ≈ θ* + iε sin²θ*/|g′(θ*)|,>>>)

just above the real axis.  The path along the real axis therefore
passes below the pole.  That is equivalent to indenting the path with
a small semicircle below the pole.  The semicircle goes
counterclockwise around the pole, so it contributes
m4_math(<<<+i\pi>>>, <<<+iπ>>>)
times the residue:

m4_displaymath(
<<<\eqalign{\mathop{\mathrm{Res}}_{\theta = \theta^{\ast}} \frac{h}{g} &= \frac{h(\theta^{\ast})}{g'(\theta^{\ast})} = -\frac{1}{2}\sqrt{\frac{n}{(n - 1)(n - m)}}, \cr i\pi \cdot \mathop{\mathrm{Res}} &= -\thinspace i\sigma .}>>>,
<<<@example
Res(h/g, θ = θ*) = h(θ*)/g′(θ*) = −(1/2) √(n/((n − 1)(n − m))),
iπ · Res = −iσ.
@end example>>>)

The result is the same as in @inlinefmtifelse{tex, @ref{Applying It to
the Integral, section 4, Applying It to the Integral}, @ref{Applying It
to the Integral, section 4}}.

Taking
m4_math(<<<n + i0>>>, <<<n + i0>>>)
instead puts the pole below the path and gives the complex conjugate.
The CPV is the average of the two one-sided limits.  Mathematica,
Wolfram|Alpha and mpmath all use the
m4_math(<<<n - i0>>>, <<<n − i0>>>)
side.

@node A Check from the Change-of-Parameter Transformation
@chapter A Check from the Change-of-Parameter Transformation

For
m4_math(<<<n > 1>>>, <<<n > 1>>>)
and
m4_mathcomma(<<<m < n>>>, <<<m < n>>>)
let

m4_displaymath(
<<<\eqalign{N &= \frac{m}{n}, \cr c &= \csc^2\varphi, \cr \Delta &= \sqrt{1 - m\sin^2\varphi}, \cr p_1 &= \sqrt{(n - 1)\left(1 - \frac{m}{n}\right)} .}>>>,
<<<@example
N = m/n,
c = csc²φ,
Δ = √(1 − m sin²φ),
p₁ = √((n − 1)(1 − m/n)).
@end example>>>)

The change-of-parameter transformation for
m4_math(<<<\Pi>>>, <<<Π>>>)
(@uref{https://dlmf.nist.gov/19.7.iii, DLMF 19.7(iii)};
@uref{https://personal.math.ubc.ca/~cbm/aands/page_587.htm, A&S
Chapter 17}, section 17.7) relates
m4_math(<<<\Pi(n)>>>, <<<Π(n)>>>)
to
m4_mathpunc(<<<:>>>, <<<\Pi(N)>>>, <<<Π(N)>>>)

m4_displaymath(
<<<\eqalign{\Pi(n;\varphi\thinspace|\thinspace m) &= F(\varphi\thinspace|\thinspace m) - \Pi(N;\varphi\thinspace|\thinspace m) \cr &\quad + \sqrt{c}\thinspace R_C\big((c - 1)(c - m),\ (c - n)(c - N)\big) .}>>>,
<<<@example
Π(n; φ | m) = F(φ | m) − Π(N; φ | m)
              + √c R_C((c − 1)(c − m), (c − n)(c − N)).
@end example>>>)

When
m4_mathcomma(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
m4_mathcomma(<<<0 < N < m < 1>>>, <<<0 < N < m < 1>>>)
so
m4_math(<<<\Pi(N;\varphi\thinspace|\thinspace m)>>>, <<<Π(N; φ | m)>>>)
has no pole on the path.  The singular behaviour is all in the last
term, which can be written with elementary functions.

@strong{The last term in elementary form.}  For
m4_mathcomma(<<<0 < y < x>>>, <<<0 < y < x>>>)
m4_math(<<<R_C>>>, <<<R_C>>>)
reduces to an inverse hyperbolic tangent
(@uref{https://dlmf.nist.gov/19.2.iv, DLMF 19.2(iv)}):

m4_displaymath(
<<<R_C(x, y) = \frac{1}{\sqrt{x - y}}\thinspace\tanh^{-1}\sqrt{\frac{x - y}{x}} .>>>,
<<<R_C(x, y) = tanh⁻¹(√((x − y)/x))/√(x − y).>>>)

Here
m4_math(<<<x = (c - 1)(c - m)>>>, <<<x = (c − 1)(c − m)>>>)
and
m4_mathdot(<<<y = (c - n)(c - N)>>>, <<<y = (c − n)(c − N)>>>)
In
m4_math(<<<x - y>>>, <<<x − y>>>)
the constant terms
m4_math(<<<m>>>, <<<m>>>)
and
m4_math(<<<nN>>>, <<<nN>>>)
cancel, because
m4_mathpunc(<<<:>>>, <<<nN = m>>>, <<<nN = m>>>)

m4_displaymath(
<<<\eqalign{x - y &= c\thinspace(n + N - 1 - m) \cr &= c\thinspace(n - 1)\left(1 - \frac{m}{n}\right) = c\thinspace p_1^2 .}>>>,
<<<@example
x − y = c (n + N − 1 − m)
      = c (n − 1)(1 − m/n) = c p₁².
@end example>>>)

With
m4_math(<<<c - 1 = \cot^2\varphi>>>, <<<c − 1 = cot²φ>>>)
and
m4_mathcomma(<<<c - m = \Delta^2/\sin^2\varphi>>>, <<<c − m = Δ²/sin²φ>>>)
for
m4_mathpunc(<<<:>>>, <<<0 < \varphi < \pi/2>>>, <<<0 < φ < π/2>>>)

m4_displaymath(
<<<\eqalign{\sqrt{x} &= \frac{\Delta\cos\varphi}{\sin^2\varphi}, \cr \sqrt{x - y} &= \frac{p_1}{\sin\varphi}, \cr \sqrt{\frac{x - y}{x}} &= \frac{p_1\tan\varphi}{\Delta} .}>>>,
<<<@example
√x = Δ cos φ/sin²φ,
√(x − y) = p₁/sin φ,
√((x − y)/x) = p₁ tan φ/Δ.
@end example>>>)

Since
m4_mathcomma(<<<\sqrt{c} = 1/\sin\varphi>>>, <<<√c = 1/sin φ>>>)
the factor
m4_math(<<<\sqrt{c}/\sqrt{x - y}>>>, <<<√c/√(x − y)>>>)
is
m4_mathdot(<<<1/p_1>>>, <<<1/p₁>>>)
So, using
m4_mathcomma(<<<\tanh^{-1} t = \frac{1}{2}\ln\big((1 + t)/(1 - t)\big)>>>, <<<tanh⁻¹ t = (1/2) ln((1 + t)/(1 − t))>>>)

m4_displaymath(
<<<\eqalign{\sqrt{c}\thinspace R_C\big((c - 1)(c - m),\ (c - n)(c - N)\big) &= \frac{1}{p_1}\thinspace\tanh^{-1}\frac{p_1\tan\varphi}{\Delta} \cr &= \frac{1}{2p_1}\ln\frac{\Delta + p_1\tan\varphi}{\Delta - p_1\tan\varphi} .}>>>,
<<<@example
√c R_C((c − 1)(c − m), (c − n)(c − N)) = (1/p₁) tanh⁻¹(p₁ tan φ/Δ)
    = (1/(2p₁)) ln((Δ + p₁ tan φ)/(Δ − p₁ tan φ)).
@end example>>>)

This form holds while
m4_mathcomma(<<<0 < y < x>>>, <<<0 < y < x>>>)
which is before the pole: there
m4_mathcomma(<<<c > n>>>, <<<c > n>>>)
so
m4_mathcomma(<<<y > 0>>>, <<<y > 0>>>)
and
m4_mathdot(<<<t = p_1\tan\varphi/\Delta < 1>>>, <<<t = p₁ tan φ/Δ < 1>>>)

@strong{The logarithm changes sign at the pole.}  At
m4_mathcomma(<<<\varphi = \theta^{\ast}>>>, <<<φ = θ*>>>)
m4_mathcomma(<<<\tan\theta^{\ast} = 1/\sqrt{n - 1}>>>, <<<tan θ* = 1/√(n − 1)>>>)
so

m4_displaymath(
<<<p_1\tan\theta^{\ast} = \sqrt{1 - \frac{m}{n}} = \Delta(\theta^{\ast}) .>>>,
<<<p₁ tan θ* = √(1 − m/n) = Δ(θ*).>>>)

The denominator
m4_math(<<<\Delta - p_1\tan\varphi>>>, <<<Δ − p₁ tan φ>>>)
is positive before the pole and negative past it.

@itemize @bullet
@item
@strong{Before the pole,} every term is real.
@item
@strong{Past the pole,}
m4_mathcomma(<<<c < n>>>, <<<c < n>>>)
so
m4_math(<<<y < 0>>>, <<<y < 0>>>)
and
m4_mathcomma(<<<t > 1>>>, <<<t > 1>>>)
and the argument of the logarithm is negative.  The Cauchy principal
value of
m4_math(<<<R_C>>>, <<<R_C>>>)
for a negative second argument is the real part,
m4_mathcomma(<<<\frac{1}{2p_1}\ln\lvert(\Delta + p_1\tan\varphi)/(\Delta - p_1\tan\varphi)\rvert>>>, <<<(1/(2p₁)) ln|(Δ + p₁ tan φ)/(Δ − p₁ tan φ)|>>>)
and with it the identity reproduces the real CPV of
@inlinefmtifelse{tex, @ref{What the Carlson Code Computes, section 2,
What the Carlson Code Computes}, @ref{What the Carlson Code Computes,
section 2}}.
@item
@strong{The imaginary part} comes from continuing
m4_math(<<<\tanh^{-1} t>>>, <<<tanh⁻¹ t>>>)
past
m4_mathpunc(<<<:>>>, <<<t = 1>>>, <<<t = 1>>>)
for
m4_math(<<<t > 1>>>, <<<t > 1>>>)
it is
m4_mathdot(<<<\frac{1}{2}\ln\lvert(1 + t)/(1 - t)\rvert \pm i\pi/2>>>, <<<(1/2) ln|(1 + t)/(1 − t)| ± iπ/2>>>)
That adds
m4_mathcomma(<<<\pm i\pi/(2p_1)>>>, <<<± iπ/(2p₁)>>>)
and
@end itemize

m4_displaymath(
<<<\frac{\pi}{2p_1} = \frac{\pi}{2}\sqrt{\frac{n}{(n - 1)(n - m)}} = \sigma ,>>>,
<<<π/(2p₁) = (π/2) √(n/((n − 1)(n − m))) = σ,>>>)

the same magnitude as @inlinefmtifelse{tex, @ref{Applying It to the
Integral, section 4, Applying It to the Integral}, @ref{Applying It to
the Integral, section 4}}.  Taking
m4_math(<<<-i\pi>>>, <<<−iπ>>>)
gives the
m4_math(<<<n - i0>>>, <<<n − i0>>>)
value; the principal branch
m4_math(<<<+i\pi>>>, <<<+iπ>>>)
gives the
m4_math(<<<n + i0>>>, <<<n + i0>>>)
value, the conjugate.

So a standard identity gives the same imaginary part as the
Sokhotski--Plemelj argument.  The sign still has to be chosen to
match the
m4_math(<<<n - i0>>>, <<<n − i0>>>)
convention.

@strong{The complete case.}  As
m4_mathcomma(<<<\varphi \to \pi/2>>>, <<<φ → π/2>>>)
m4_mathcomma(<<<\tan\varphi \to \infty>>>, <<<tan φ → ∞>>>)
the argument of the logarithm tends to
m4_mathcomma(<<<-1>>>, <<<−1>>>)
and its real part tends to
m4_mathdot(<<<0>>>, <<<0>>>)
With
m4_mathpunc(<<<:>>>, <<<F(\pi/2\thinspace|\thinspace m) = K(m)>>>, <<<F(π/2 | m) = K(m)>>>)

m4_displaymath(
<<<\mathrm{PV}\thinspace\Pi(n\thinspace|\thinspace m) = K(m) - \Pi\left(\frac{m}{n}\thinspace\Big|\thinspace m\right), \qquad n > 1,\ 0 < m < 1 .>>>,
<<<PV Π(n | m) = K(m) − Π(m/n | m),   n > 1, 0 < m < 1.>>>)

For
m4_math(<<<n = 1.5>>>, <<<n = 1.5>>>)
and
m4_mathpunc(<<<:>>>, <<<m = 0.5>>>, <<<m = 0.5>>>)

m4_displaymath(
<<<\eqalign{K(0.5) - \Pi\left(\frac{1}{3}\thinspace\Big|\thinspace 0.5\right) &= 1.85407467730137192 - 2.31079499075428182 \cr &= -0.45672031345290990 ,}>>>,
<<<@example
K(0.5) − Π(1/3 | 0.5) = 1.85407467730137192 − 2.31079499075428182
                      = −0.45672031345290990,
@end example>>>)

which is the real part of
m4_math(<<<\Pi(1.5\thinspace|\thinspace 0.5)>>>, <<<Π(1.5 | 0.5)>>>)
in @inlinefmtifelse{tex, @ref{The Problem, section 1, The Problem},
@ref{The Problem, section 1}}.

This transformation is not used for the computation.  Near
m4_mathcomma(<<<n = 1^{+}>>>, <<<n = 1⁺>>>)
m4_mathcomma(<<<p_1 \to 0>>>, <<<p₁ → 0>>>)
and the logarithmic term cancels against the other terms.  Near
m4_mathcomma(<<<n = m>>>, <<<n = m>>>)
m4_mathdot(<<<N \to 1>>>, <<<N → 1>>>)
The incomplete case would still need the pole counting of
@inlinefmtifelse{tex, @ref{Applying It to the Integral, section 4,
Applying It to the Integral}, @ref{Applying It to the Integral, section
4}}, because the logarithm changes sign at every pole.  For
m4_mathcomma(<<<m > n>>>, <<<m > n>>>)
m4_mathcomma(<<<N > 1>>>, <<<N > 1>>>)
so
m4_math(<<<\Pi(N)>>>, <<<Π(N)>>>)
has a pole of its own.

@node Regions of the Real Parameter Space
@chapter Regions of the Real Parameter Space

The Sokhotski--Plemelj term is needed exactly where @code{bf-rj} takes
its CPV branch, that is, where
m4_math(<<<p = 1 - n\sin^2\theta < 0>>>, <<<p = 1 − n sin²θ < 0>>>)
while
m4_mathdot(<<<y = 1 - m\sin^2\theta \ge 0>>>, <<<y = 1 − m sin²θ ≥ 0>>>)
Everywhere else, the general duplication algorithm @code{bf-rj1}
handles the continuation and already returns the
m4_math(<<<n - i0>>>, <<<n − i0>>>)
value.

Let
m4_math(<<<\theta_b = \sin^{-1}(1/\sqrt{m})>>>, <<<θ_b = sin⁻¹(1/√m)>>>)
be the branch point of
m4_mathcomma(<<<\sqrt{1 - m\sin^2\theta}>>>, <<<√(1 − m sin²θ)>>>)
defined when
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)

@raggedright
@multitable @columnfractions 0.32 0.36 0.32
@headitem
Region
@tab
Behaviour
@tab
Action
@item
m4_math(<<<n < 1>>>, <<<n < 1>>>)
@tab
No pole on the path
@tab
Nothing to add
@item
m4_math(<<<n = 1>>>, <<<n = 1>>>)
@tab
Double zero at
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
so the integral diverges for
m4_math(<<<\varphi \ge \pi/2>>>, <<<φ ≥ π/2>>>)
@tab
None; no principal value exists
@item
m4_math(<<<n > 1,\ m \le 1>>>, <<<n > 1, m ≤ 1>>>)
@tab
Poles at
m4_math(<<<\pm\theta^{\ast} + k\pi>>>, <<<±θ* + kπ>>>)
@tab
Add
m4_math(<<<-i\sigma>>>, <<<−iσ>>>)
for each pole crossed
@item
m4_mathcomma(<<<n > 1,\ 1 < m < n>>>, <<<n > 1, 1 < m < n>>>)
and
m4_math(<<<\theta^{\ast} < \lvert\varphi_r\rvert \le \theta_b>>>, <<<θ* < |φᵣ| ≤ θ_b>>>)
@tab
Pole crossed before the branch point
@tab
Add
m4_math(<<<-i\sigma\thinspace\mathrm{sgn}(\varphi_r)>>>, <<<−iσ sgn(φᵣ)>>>)
@item
m4_mathcomma(<<<n > 1,\ 1 < m < n>>>, <<<n > 1, 1 < m < n>>>)
and
m4_math(<<<\lvert\varphi_r\rvert > \theta_b>>>, <<<|φᵣ| > θ_b>>>)
@tab
Past the branch point
@tab
Nothing to add; @code{bf-rj1} already gives the
m4_math(<<<n - i0>>>, <<<n − i0>>>)
value
@item
m4_math(<<<n > 1,\ m > n>>>, <<<n > 1, m > n>>>)
@tab
Branch point comes before the pole, where
m4_math(<<<h>>>, <<<h>>>)
is imaginary
@tab
Nothing to add; @code{bf-rj1} already gives the
m4_math(<<<n - i0>>>, <<<n − i0>>>)
value
@item
m4_math(<<<m = n>>>, <<<m = n>>>)
@tab
Pole and branch point coincide, giving
m4_mathcomma(<<<g^{-3/2}>>>, <<<g^(−3/2)>>>)
which is not integrable
@tab
None; only a Hadamard finite part exists
@item
m4_math(<<<n\sin^2\varphi = 1>>>, <<<n sin²φ = 1>>>)
@tab
Endpoint on the pole, giving a logarithmic divergence
@tab
None
@end multitable
@end raggedright

Here
m4_math(<<<\varphi_r>>>, <<<φᵣ>>>)
is
m4_math(<<<\varphi>>>, <<<φ>>>)
reduced to
m4_mathcomma(<<<[-\pi/2, \pi/2]>>>, <<<[−π/2, π/2]>>>)
defined in @inlinefmtifelse{tex, @ref{Implementation, section 8,
Implementation}, @ref{Implementation, section 8}}.

The cycles of
m4_math(<<<\pi>>>, <<<π>>>)
need separate treatment.  For
m4_mathcomma(<<<m \le 1>>>, <<<m ≤ 1>>>)
the complete integral is evaluated through the CPV branch, so each
cycle needs
m4_mathdot(<<<2 \times (-i\sigma)>>>, <<<2 × (−iσ)>>>)
For
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
m4_mathcomma(<<<1 - m < 0>>>, <<<1 − m < 0>>>)
so the complete integral goes through @code{bf-rj1} and already
includes the term.

These rules were found by comparing the uncorrected code with mpmath
over a grid of
m4_mathcomma(<<<n>>>, <<<n>>>)
m4_math(<<<m>>>, <<<m>>>)
and
m4_mathdot(<<<\varphi>>>, <<<φ>>>)
The table follows the rule above.

@node Implementation
@chapter Implementation

@code{bf-elliptic-pi} reduces
m4_math(<<<\varphi>>>, <<<φ>>>)
by multiples of
m4_mathcomma(<<<\pi>>>, <<<π>>>)

m4_displaymath(
<<<\Pi(n;\thinspace\varphi_r + k\pi\thinspace|\thinspace m) = 2k\thinspace\Pi(n\thinspace|\thinspace m) + \Pi(n;\thinspace\varphi_r\thinspace|\thinspace m),>>>,
<<<Π(n; φᵣ + kπ | m) = 2k Π(n | m) + Π(n; φᵣ | m),>>>)

and sums the CPV pieces.  The new function
@code{bf-elliptic-pi-residue}, in the @code{bigfloat} package in
@file{src/ellipt.lisp}, computes the imaginary correction:

@example lisp
(defun bf-elliptic-pi-residue (n phi m)
  (if (and (realp n) (realp phi) (realp m) (> n 1))
      (let* ((bf-pi (%pi phi))
             (cycles (round phi bf-pi))
             (rem (- phi (* cycles bf-pi)))
             (s2 (expt (sin rem) 2))
             (crossings (+ (if (<= m 1)
                               (* 2 cycles)
                               0)
                           (if (and (> (* n s2) 1) (<= (* m s2) 1))
                               (signum rem)
                               0))))
        (if (zerop crossings)
            0
            (* (complex 0 -1) crossings (/ bf-pi 2)
               (sqrt (/ n (* (- n 1) (- n m)))))))
      0))
@end example

The @code{elliptic_pi} simplifier adds this correction in its
real-float and real-bigfloat branches.  The complex branches are
unchanged.

@example lisp
((float-numerical-eval-p n phi m)
 (add (elliptic-pi ($float n) ($float phi) ($float m))
      (to (bigfloat::bf-elliptic-pi-residue ($float n) ($float phi)
                                            ($float m)))))
...
((bigfloat-numerical-eval-p n phi m)
 (to (bigfloat:+ (bigfloat::bf-elliptic-pi (bigfloat:to n)
                                           (bigfloat:to phi)
                                           (bigfloat:to m))
                 (bigfloat::bf-elliptic-pi-residue (bigfloat:to n)
                                                   (bigfloat:to phi)
                                                   (bigfloat:to m)))))
@end example

@node Tests
@chapter Tests

New tests in @file{tests/rtest_elliptic.mac} check each case in both
double float and bigfloat at @code{fpprec: 32}, against 50-digit
references from mpmath's @code{ellippi}.

@itemize @bullet
@item
All inputs are dyadic rationals, so the float and bigfloat arguments
are exact.
@item
Each tolerance is the error observed in testing, rounded up to five
significant digits.
@end itemize

@raggedright
@multitable @columnfractions 0.20 0.20 0.30 0.30
@headitem
m4_math(<<<(n,\ \varphi,\ m)>>>, <<<(n, φ, m)>>>)
@tab
What it covers
@tab
Real part
@tab
Imaginary part
@item
m4_math(<<<(1.5,\ \pi/2,\ 0.5)>>>, <<<(1.5, π/2, 0.5)>>>)
@tab
Complete integral,
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
@tab
@math{-0.456720313452909897}
@tab
@math{-2.720699046351326776}
@item
@math{(1.5, 1.25, 0.5)}
@tab
Pole crossed
@tab
@math{0.545248053746195541}
@tab
@math{-2.720699046351326776}
@item
@math{(1.5, 0.625, 0.5)}
@tab
Pole not reached; imaginary part is exactly
@math{0}
@tab
@math{0.827387372389447886}
@tab
@math{0}
@item
@math{(1.5, -1.25, 0.5)}
@tab
Odd in
m4_math(<<<\varphi>>>, <<<φ>>>)
@tab
@math{-0.545248053746195541}
@tab
@math{2.720699046351326776}
@item
@math{(1.5, 2.5, 0.5)}
@tab
Reduction by
m4_math(<<<\pi>>>, <<<π>>>)
@tab
@math{-1.779327655628395173}
@tab
@math{-5.441398092702653552}
@item
@math{(1.5, 4.0, 0.5)}
@tab
Reduction by
m4_math(<<<\pi>>>, <<<π>>>)
@tab
@math{0.871635485678527766}
@tab
@math{-5.441398092702653552}
@item
@math{(2, 1, -0.75)}
@tab
@math{m < 0}
@tab
@math{0.779446017152295514}
@tab
@math{-1.339579608485734033}
@item
m4_math(<<<(1.5,\ \pi/2,\ -0.75)>>>, <<<(1.5, π/2, −0.75)>>>)
@tab
Complete integral,
@math{m < 0}
@tab
@math{0.233606481142978194}
@tab
@math{-1.813799364234217851}
@item
@math{(0.5, -1.25, 2)}
@tab
m4_mathcomma(<<<n < 1>>>, <<<n < 1>>>)
negative
m4_mathcomma(<<<\varphi>>>, <<<φ>>>)
complex result
@tab
@math{-1.532738349029281103}
@tab
@math{1.460480317267677154}
@item
@math{(4, 0.625, 2)}
@tab
m4_mathcomma(<<<1 < m < n>>>, <<<1 < m < n>>>)
pole crossed before the branch point
@tab
@math{0.555008279584908678}
@tab
@math{-1.282549830161864096}
@item
@math{(4, 1, 2)}
@tab
m4_mathcomma(<<<1 < m < n>>>, <<<1 < m < n>>>)
past the branch point
@tab
@math{-0.221709571883221198}
@tab
@math{-0.751955515548111328}
@item
@math{(1.5, 1.25, 3)}
@tab
@math{m > n}
@tab
@math{-0.780810492104561147}
@tab
@math{-1.131673674679173545}
@end multitable
@end raggedright

The last tests check the sign convention without relying on reference
values:

@itemize @bullet
@item
m4_mathcomma(<<<\Pi(n - i\varepsilon;\thinspace\varphi\thinspace|\thinspace m)>>>, <<<Π(n − iε; φ | m)>>>)
computed through the complex path, must equal the new real-@math{n}
result.
@item
m4_math(<<<\Pi(n + i\varepsilon;\thinspace\varphi\thinspace|\thinspace m)>>>, <<<Π(n + iε; φ | m)>>>)
must equal its complex conjugate.
@end itemize

They use
m4_math(<<<\varepsilon = 10^{-14}>>>, <<<ε = 10⁻¹⁴>>>)
in float and
m4_math(<<<10^{-30}>>>, <<<10⁻³⁰>>>)
in bigfloat.

With the patch, all 288 tests in the file pass.  Without the source
changes, 11 of the new tests fail.  The four that still pass cover
regions where the rule adds nothing: the no-pole case, its
imaginary-part check, past the branch point, and
m4_mathdot(<<<m > n>>>, <<<m > n>>>)

These results come from Maxima under SBCL 2.2.9.  Tolerances may need
adjusting for other Lisps.

A wider comparison of 336 cases also matched mpmath:

@itemize @bullet
@item
m4_math(<<<n \in \lbrace 1.1,\ 1.5,\ 4\rbrace>>>, <<<n ∈ @{1.1, 1.5, 4@}>>>)
@item
m4_math(<<<m \in \lbrace -2,\ 0,\ 0.5,\ 0.99,\ 1.2,\ 2,\ 3,\ 6\rbrace>>>, <<<m ∈ @{−2, 0, 0.5, 0.99, 1.2, 2, 3, 6@}>>>)
@item
14 values of
m4_mathcomma(<<<\varphi>>>, <<<φ>>>)
including negative values and several cycles of
m4_math(<<<\pi>>>, <<<π>>>)
@end itemize

The worst relative errors were:

@raggedright
@multitable @columnfractions 0.40 0.30 0.30
@headitem
@tab
m4_math(<<<m \le 1>>>, <<<m ≤ 1>>>)
@tab
@math{m > 1}
@item
Bigfloat at @code{fpprec: 30}
@tab
m4_math(<<<6 \times 10^{-27}>>>, <<<6 × 10⁻²⁷>>>)
@tab
m4_math(<<<5 \times 10^{-26}>>>, <<<5 × 10⁻²⁶>>>)
@item
Double float
@tab
m4_math(<<<8 \times 10^{-16}>>>, <<<8 × 10⁻¹⁶>>>)
@tab
m4_math(<<<4 \times 10^{-15}>>>, <<<4 × 10⁻¹⁵>>>)
@end multitable
@end raggedright

@node Known Limitations
@chapter Known Limitations

@itemize @bullet
@item
@strong{@math{m = 1}.}  @code{bf-elliptic-pi} always evaluates the
complete integral, which is infinite at
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
so even
m4_math(<<<\varphi < \pi/2>>>, <<<φ < π/2>>>)
fails with @code{0.0/0.0}.  This also happens on master.
@item
@strong{Complex arguments.}  The correction applies only to real
m4_mathcomma(<<<n>>>, <<<n>>>)
m4_math(<<<\varphi>>>, <<<φ>>>)
and
m4_mathdot(<<<m>>>, <<<m>>>)
Complex inputs go through @code{bf-rj1} unchanged.  Continuity with
the real-axis value from below is tested, but that is the only check
of the complex path here.
@end itemize

@node References
@chapter References

@itemize @bullet
@item
B.@: C.@: Carlson, ``Numerical computation of real or complex elliptic
integrals'', @emph{Numerical Algorithms} 10 (1995) 13--26,
@uref{https://arxiv.org/abs/math/9409227, arXiv:math/9409227}.
Eq.@: (33) gives the Cauchy principal value of
m4_math(<<<R_J>>>, <<<R_J>>>)
for
m4_mathdot(<<<p < 0>>>, <<<p < 0>>>)
@item
@emph{NIST Digital Library of Mathematical Functions}:
@uref{https://dlmf.nist.gov/19.7.iii, DLMF 19.7(iii)} for the change
of parameter of
m4_math(<<<\Pi>>>, <<<Π>>>)
used in @inlinefmtifelse{tex, @ref{A Check from the Change-of-Parameter
Transformation, section 6, A Check from the Change-of-Parameter
Transformation}, @ref{A Check from the Change-of-Parameter
Transformation, section 6}}, @uref{https://dlmf.nist.gov/19.20.iii,
DLMF 19.20(iii)} for
m4_math(<<<R_J>>>, <<<R_J>>>)
with
m4_math(<<<p < 0>>>, <<<p < 0>>>)
(@uref{https://dlmf.nist.gov/19.20.E14, DLMF 19.20.14}), and
@uref{https://dlmf.nist.gov/19.25, DLMF 19.25} for
m4_math(<<<\Pi>>>, <<<Π>>>)
in terms of Carlson integrals.  The identity in section 6 was checked
numerically against mpmath; its equation number in DLMF 19.7(iii) has
not been verified.
@item
M.@: Abramowitz and I.@: A.@: Stegun, @emph{Handbook of Mathematical
Functions}, section 17.7
(@uref{https://personal.math.ubc.ca/~cbm/aands/page_587.htm, A&S
Chapter 17}), for the same transformation in logarithmic form.
@item
N.@: I.@: Muskhelishvili, @emph{Singular Integral Equations}, and
F.@: D.@: Gakhov, @emph{Boundary Value Problems}.  Both are standard
treatments of the Sokhotski--Plemelj formulas.  Section numbers have
not been verified.
@item
F.@: Johansson et al., mpmath, @code{ellippi}, used for the reference
values.
@end itemize

@bye
