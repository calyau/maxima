\input texinfo
@c -*- mode: texinfo -*-
@c Created by Claude to describe how Maxima computes elliptic_pi
@c accurately where 1-n*sin(phi)^2 or 1-m*sin(phi)^2 cancels, and
@c carlson_rj for small p.
@c
@setfilename elliptic-pi-accuracy.info
@settitle Accurate Computation of elliptic_pi
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
@top Accurate Computation of @code{elliptic_pi}
@end ifnottex

@iftex
@titlepage
@title Accurate Computation of @code{elliptic_pi}
@end titlepage
@end iftex

This document records the work on bug
@url{https://sourceforge.net/p/maxima/bugs/5904/, #5904}, accuracy
problems in @code{elliptic_pi} and @code{carlson_rj} for arguments where
an intermediate quantity cancels, and the changes to
@code{src/ellipt.lisp} that fix them. It also covers bug
@url{https://sourceforge.net/p/maxima/bugs/5908/, #5908}, which affects
@code{elliptic_f} and @code{elliptic_e} as well. It explains what was
wrong, the algorithms now used, and the measurements behind them.

The changes are in @code{src/ellipt.lisp} and
@code{tests/rtest_elliptic.mac}. Measurements are from SBCL 2.2.9
against @url{https://mpmath.org/, mpmath} references at the exact double
or bigfloat inputs. The tests also pass on CMUCL, CCL and ECL. On CLISP
and on GCL 2.7, six of the new tests are expected failures
(@pxref{Tests}).

Errors are given in ulps of the exact value, with 53 bits for double
floats and 109 bits for bigfloats at @code{fpprec : 32}. The real and
imaginary parts of a complex value are measured separately, each in ulps
of that part.

Write
m4_math(<<<\Pi(n;\phi\thinspace |\thinspace m)>>>, <<<Π(n;φ | m)>>>)
for @code{elliptic_pi(n, phi, m)}, and
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
m4_math(<<<R_C>>>, <<<R_C>>>)
and
m4_math(<<<R_J>>>, <<<R_J>>>)
for @code{carlson_rf}, @code{carlson_rc} and @code{carlson_rj}.

@menu
* Summary of the Change::
* The Formula::
* The Problems::
* The Solutions::
* Implementation::
* Accuracy::
* Speed::
* Tests::
* Known Limitations::
* References::
@end menu

@node Summary of the Change
@chapter Summary of the Change

@itemize @bullet
@item
@code{bf-elliptic-pi} computes
m4_math(<<<1 - m\sin^2\phi>>>, <<<1 - m sin^2 φ>>>)
and
m4_math(<<<1 - n\sin^2\phi>>>, <<<1 - n sin^2 φ>>>)
with @code{bf-1-m*sin^2}, the function added for @code{elliptic_f} in
@url{https://sourceforge.net/p/maxima/bugs/5799/, #5799}, instead of by
a plain subtraction (@pxref{Computing y and p Accurately}).
@item
A new function, @code{bf-rj-rc-arg}, computes the second argument of
m4_math(<<<R_C>>>, <<<R_C>>>)
in the duplication step of @code{bf-rj1} without cancellation
(@pxref{The Duplication Step of R_J}). This also makes @code{carlson_rj}
accurate for small
m4_math(<<<p>>>, <<<p>>>)
and for widely separated arguments.
@item
@code{bf-elliptic-pi-residue}, which adds the imaginary part for
m4_mathcomma(<<<n > 1>>>, <<<n > 1>>>)
decides whether the pole has been crossed from the same accurate value
of
m4_math(<<<1 - n\sin^2\phi>>>, <<<1 - n sin^2 φ>>>)
(@pxref{Which Side of the Pole}).
@item
@code{bf-elliptic-pi} takes the sine and cosine of the unreduced
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
instead of reducing
m4_math(<<<\phi>>>, <<<φ>>>)
by a multiple of
m4_math(<<<\pi>>>, <<<π>>>)
first (@pxref{Reducing the Angle}).
@item
@code{bf-elliptic-pi} evaluates the complete integral only when it is
needed. For
m4_math(<<<m = 1>>>, <<<m = 1>>>)
or
m4_mathcomma(<<<n = 1>>>, <<<n = 1>>>)
where it is infinite, @code{elliptic_pi} returns the finite value for
m4_math(<<<\lvert\phi\rvert < \pi/2>>>, <<<|φ| < π/2>>>)
and signals a Maxima error otherwise (@pxref{Skipping the Complete
Integral}).
@item
For real
m4_math(<<<n > 1>>>, <<<n > 1>>>)
past the pole, and for real
m4_mathcomma(<<<n < -1>>>, <<<n < -1>>>)
@code{bf-elliptic-pi} and @code{bf-elliptic-pi-complete} use a change of
parameter, which does not have the cancellation between the terms of the
formula (@pxref{The Changes of Parameter}).
@item
The double-float functions @code{elliptic-pi} and
@code{elliptic-pi-complete} are removed. @code{bf-elliptic-pi} is used
for double floats as well (@pxref{Implementation}).
@item
For real
m4_math(<<<m > 1>>>, <<<m > 1>>>)
with
m4_math(<<<\phi>>>, <<<φ>>>)
past the branch point, the real part, which is constant there, is
computed from a complete integral, and the imaginary part from real
integrals (@pxref{Separate Real and Imaginary Parts for m > 1}).
@item
A new function, @code{bf-elliptic-period}, corrects
m4_math(<<<k = \mathrm{round}(\phi/\pi)>>>, <<<k = round(φ/π)>>>)
when
m4_math(<<<\phi>>>, <<<φ>>>)
is next to an odd multiple of
m4_mathdot(<<<\pi/2>>>, <<<π/2>>>)
@code{elliptic_f} and @code{elliptic_e} use it too
(@pxref{s4-4-2,,Section 4.4.2}).
@item
New regression tests (@pxref{Tests}).
@end itemize

Before the change, @code{elliptic_pi} lost from a third to all of its
digits in five regions:
m4_math(<<<n>>>, <<<n>>>)
near 1 with
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
m4_math(<<<m>>>, <<<m>>>)
near 1 with
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
m4_math(<<<\phi>>>, <<<φ>>>)
near the pole
m4_mathcomma(<<<n\sin^2\phi = 1>>>, <<<n sin^2 φ = 1>>>)
the imaginary part for
m4_math(<<<\phi>>>, <<<φ>>>)
near the branch point
m4_mathcomma(<<<m\sin^2\phi = 1>>>, <<<m sin^2 φ = 1>>>)
and complex
m4_math(<<<n>>>, <<<n>>>)
or
m4_math(<<<m>>>, <<<m>>>)
near 1. The worst errors there were
m4_math(<<<10^{10}>>>, <<<10^10>>>)
to
m4_math(<<<10^{13}>>>, <<<10^13>>>)
ulps. They are now 4 to 22 ulps (@pxref{Accuracy}). When
m4_math(<<<\phi>>>, <<<φ>>>)
needed reducing and the reduced angle was near
m4_math(<<<\pi/2>>>, <<<π/2>>>)
with
m4_math(<<<n>>>, <<<n>>>)
or
m4_math(<<<m>>>, <<<m>>>)
near 1, or near a pole, the errors were hundreds of ulps to
m4_math(<<<10^{7}>>>, <<<10^7>>>)
ulps, and are now 5 to 14 ulps for
m4_math(<<<\lvert\phi\rvert < 10^{8}>>>, <<<|φ| < 10^8>>>)
near
m4_math(<<<\pi/2>>>, <<<π/2>>>)
(@pxref{Reducing the Angle}). For
m4_math(<<<n > 1>>>, <<<n > 1>>>)
past the pole and for
m4_mathcomma(<<<n \ll 0>>>, <<<n << 0>>>)
where the terms of the formula cancelled, the errors were hundreds to
thousands of ulps, and are now a few ulps, except near the zero of the
real part for
m4_math(<<<n > 1>>>, <<<n > 1>>>)
(@pxref{The Changes of Parameter}). Elsewhere the results change by at
most a few ulps. For
m4_math(<<<\lvert\phi\rvert < \pi/2>>>, <<<|φ| < π/2>>>)
the time per call is 35 to 68 percent less than it was (@pxref{Speed}).

@code{carlson_rj}, and so @code{elliptic_pi}, still returns wrong values
for some complex arguments, because Carlson's algorithm for
m4_math(<<<R_J>>>, <<<R_J>>>)
is not valid for all of them (@pxref{Known Limitations}). The change
also does not address the accuracy of each part for general complex
arguments (@pxref{Known Limitations}).

@node The Formula
@chapter The Formula

Maxima computes

m4_displaymath(
<<<\Pi(n;\phi\thinspace |\thinspace m) = \int_0^{\phi}\frac{d\theta}{\left(1 - n\sin^2\theta\right)\sqrt{1 - m\sin^2\theta}}>>>,
<<<Π(n;φ | m) = integral_0^φdθ/((1 - n sin^2 θ)sqrt(1 - m sin^2 θ))>>>)

from Carlson's symmetric integrals. With

m4_displaymath(
<<<s = \sin\phi, \qquad c = \cos\phi, \qquad y = 1 - m s^2, \qquad p = 1 - n s^2 ,>>>,
<<<s = sin φ, c = cos φ, y = 1 - m s^2, p = 1 - n s^2 ,>>>)

the formula is

m4_displaymath(
<<<\Pi(n;\phi\thinspace |\thinspace m) = s\thinspace R_F(c^2, y, 1) + \frac{n}{3}\thinspace s^3 R_J(c^2, y, 1, p) .>>>,
<<<Π(n;φ | m) = s R_F(c^2, y, 1) + n/3 s^3 R_J(c^2, y, 1, p) .>>>)

This is @url{https://dlmf.nist.gov/19.25.E14, DLMF 19.25.14}, scaled by
m4_math(<<<\sin^2\phi>>>, <<<sin^2 φ>>>)
using the homogeneity of
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_mathdot(<<<R_J>>>, <<<R_J>>>)
The complete integral, with
m4_mathcomma(<<<\phi = \pi/2>>>, <<<φ = π/2>>>)
is @url{https://dlmf.nist.gov/19.25.E2, DLMF 19.25.2}:

m4_displaymath(
<<<\Pi(n\thinspace |\thinspace m) = R_F(0, 1-m, 1) + \frac{n}{3}\thinspace R_J(0, 1-m, 1, 1-n) .>>>,
<<<Π(n | m) = R_F(0, 1-m, 1) + n/3 R_J(0, 1-m, 1, 1-n) .>>>)

For other
m4_math(<<<\phi>>>, <<<φ>>>)
the code uses the periodicity of the integrand:

m4_displaymath(
<<<\Pi(n;\phi\thinspace |\thinspace m) = \Pi(n;\phi - k\pi\thinspace |\thinspace m) + 2k\thinspace \Pi(n\thinspace |\thinspace m), \qquad k = \mathrm{round}(\phi/\pi) .>>>,
<<<Π(n;φ | m) = Π(n;φ - kπ | m) + 2k Π(n | m), k = round(φ/π) .>>>)

@ref{s4-4-1,,Section 4.4.1} describes how the first term is computed
without forming
m4_mathcomma(<<<\phi - k\pi>>>, <<<φ - kπ>>>)
and @ref{s4-4-2,,Section 4.4.2} how
m4_math(<<<k>>>, <<<k>>>)
is corrected next to an odd multiple of
m4_mathdot(<<<\pi/2>>>, <<<π/2>>>)

For real
m4_math(<<<n > 1>>>, <<<n > 1>>>)
the integrand has a pole at

m4_displaymath(
<<<\theta^{\ast} = \arcsin\frac{1}{\sqrt n} ,>>>,
<<<θ^* = asin(1/sqrt(n)) ,>>>)

where
m4_mathdot(<<<p = 0>>>, <<<p = 0>>>)
Past it
m4_mathcomma(<<<p < 0>>>, <<<p < 0>>>)
and @code{bf-rj} returns the Cauchy principal value of
m4_math(<<<R_J>>>, <<<R_J>>>)
from @url{https://dlmf.nist.gov/19.20.E14, DLMF 19.20.14}.
@code{bf-elliptic-pi-residue} then adds the imaginary part,
m4_math(<<<-i\sigma>>>, <<<-iσ>>>)
for each pole crossed, where

m4_displaymath(
<<<\sigma = \frac{\pi}{2}\sqrt{\frac{n}{(n-1)(n-m)}} .>>>,
<<<σ = π/2 sqrt(n/((n-1)(n-m))) .>>>)

For real
m4_math(<<<m > 1>>>, <<<m > 1>>>)
the square root has a branch point at
m4_mathcomma(<<<\arcsin(1/\sqrt m)>>>, <<<asin(1/sqrt(m))>>>)
where
m4_mathdot(<<<y = 0>>>, <<<y = 0>>>)

@node The Problems
@chapter The Problems

@code{elliptic_pi} had seven problems. In the first three, a quantity
inside the formula is computed as a difference of nearly equal numbers.
The fourth is the reduction of the angle, which lost accuracy and could
use the wrong period count, the fifth is a failure for
m4_math(<<<m = 1>>>, <<<m = 1>>>)
or
m4_mathcomma(<<<n = 1>>>, <<<n = 1>>>)
and in the last two the terms of the formula cancel each other, before
and past the branch point of
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)
In each of them the inputs are accurate and the function is well
conditioned, so the problem is avoidable.

@menu
* Cancellation in y and p::
* Cancellation Inside R_J::
* Deciding Whether the Pole Has Been Crossed::
* The Reduction of the Angle::
* The Complete Integral for m or n Equal to 1::
* Cancellation Between the Terms of the Formula::
* Cancellation Past the Branch Point for m > 1::
* Examples::
@end menu

@node Cancellation in y and p
@section Cancellation in @math{y} and @math{p}

Both
m4_math(<<<y>>>, <<<y>>>)
and
m4_math(<<<p>>>, <<<p>>>)
were computed as written, as
m4_math(<<<1 - m s^2>>>, <<<1 - m s^2>>>)
and
m4_mathdot(<<<1 - n s^2>>>, <<<1 - n s^2>>>)
The subtraction is exact or nearly so, but
m4_math(<<<s>>>, <<<s>>>)
and the product are rounded, so the result has an absolute error of
about one rounding unit
m4_mathcomma(<<<\varepsilon>>>, <<<ε>>>)
and a relative error of about
m4_math(<<<\varepsilon/\lvert y\rvert>>>, <<<ε/|y|>>>)
or
m4_mathdot(<<<\varepsilon/\lvert p\rvert>>>, <<<ε/|p|>>>)
That matters in four places.

@itemize @bullet
@item
m4_math(<<<n>>>, <<<n>>>)
@strong{near 1 and}
m4_math(<<<\phi>>>, <<<φ>>>)
@strong{near}
m4_mathdot(<<<\pi/2>>>, <<<π/2>>>)
Here
m4_math(<<<p = c^2 + (1-n)s^2>>>, <<<p = c^2 + (1-n)s^2>>>)
is small.
m4_math(<<<\Pi>>>, <<<Π>>>)
is of order
m4_mathcomma(<<<1/\sqrt{(1-n)(1-m)}>>>, <<<1/sqrt((1-n)(1-m))>>>)
and its relative error is comparable to that of
m4_mathdot(<<<p>>>, <<<p>>>)
For example, at
m4_mathcomma(<<<n = 0.9999999999999827>>>, <<<n = 0.9999999999999827>>>)
m4_mathcomma(<<<\phi = 1.5707962718820392>>>, <<<φ = 1.5707962718820392>>>)
m4_mathcomma(<<<m = 0.37766630687876385>>>, <<<m = 0.37766630687876385>>>)
the exact
m4_math(<<<p>>>, <<<p>>>)
is
m4_mathcomma(<<<2.0335\times10^{-14}>>>, <<<2.0335×10^-14>>>)
the computed one was
m4_mathcomma(<<<2.0428\times10^{-14}>>>, <<<2.0428×10^-14>>>)
and the result was wrong in the fourth digit.
@item
m4_math(<<<m>>>, <<<m>>>)
@strong{near 1 and}
m4_math(<<<\phi>>>, <<<φ>>>)
@strong{near}
m4_mathdot(<<<\pi/2>>>, <<<π/2>>>)
Here
m4_math(<<<y = c^2 + (1-m)s^2>>>, <<<y = c^2 + (1-m)s^2>>>)
is small, and
m4_math(<<<\Pi>>>, <<<Π>>>)
grows like
m4_mathcomma(<<<\ln(1/y)>>>, <<<ln(1/y)>>>)
so its absolute error is proportional to the relative error of
m4_mathdot(<<<y>>>, <<<y>>>)
@item
m4_math(<<<\phi>>>, <<<φ>>>)
@strong{near the pole.} Here
m4_math(<<<p>>>, <<<p>>>)
changes sign, and the real part of
m4_math(<<<\Pi>>>, <<<Π>>>)
behaves like
m4_math(<<<-\ln\lvert p\rvert/(2p_1)>>>, <<<-ln |p|/(2p_1)>>>)
with
m4_mathdot(<<<p_1 = \sqrt{(n-1)(1-m/n)}>>>, <<<p_1 = sqrt((n-1)(1-m/n))>>>)
Its absolute error is about the relative error of
m4_mathcomma(<<<p>>>, <<<p>>>)
divided by
m4_mathdot(<<<2p_1>>>, <<<2p_1>>>)
@item
m4_math(<<<\phi>>>, <<<φ>>>)
@strong{near the branch point, for}
m4_mathdot(<<<m > 1>>>, <<<m > 1>>>)
Just past it, the imaginary part of
m4_math(<<<\Pi>>>, <<<Π>>>)
is proportional to
m4_mathcomma(<<<\sqrt{-y}>>>, <<<sqrt(-y)>>>)
so its relative error is half that of
m4_mathdot(<<<y>>>, <<<y>>>)
@end itemize

Complex
m4_math(<<<n>>>, <<<n>>>)
or
m4_math(<<<m>>>, <<<m>>>)
near 1, with
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
has the same problem in both parts.

@node Cancellation Inside R_J
@section Cancellation Inside @math{R_J}

@code{bf-rj1} computes
m4_math(<<<R_J(x, y, z, p)>>>, <<<R_J(x, y, z, p)>>>)
by Carlson's duplication algorithm. It starts from
m4_mathcomma(<<<x_0 = x>>>, <<<x_0 = x>>>)
m4_mathcomma(<<<y_0 = y>>>, <<<y_0 = y>>>)
m4_math(<<<z_0 = z>>>, <<<z_0 = z>>>)
and
m4_mathcomma(<<<p_0 = p>>>, <<<p_0 = p>>>)
and step
m4_math(<<<k>>>, <<<k>>>)
replaces all four variables by

m4_displaymath(
<<<x_{k+1} = \frac{x_k + \lambda_k}{4}, \qquad y_{k+1} = \frac{y_k + \lambda_k}{4}, \qquad z_{k+1} = \frac{z_k + \lambda_k}{4}, \qquad p_{k+1} = \frac{p_k + \lambda_k}{4},>>>,
<<<x_(k+1) = (x_k + λ_k)/4, y_(k+1) = (y_k + λ_k)/4, z_(k+1) = (z_k + λ_k)/4, p_(k+1) = (p_k + λ_k)/4,>>>)

where

m4_displaymath(
<<<\lambda_k = \sqrt{x_k y_k} + \sqrt{x_k z_k} + \sqrt{y_k z_k} .>>>,
<<<λ_k = sqrt(x_k y_k) + sqrt(x_k z_k) + sqrt(y_k z_k) .>>>)

So
m4_mathcomma(<<<x_k>>>, <<<x_k>>>)
m4_mathcomma(<<<y_k>>>, <<<y_k>>>)
m4_math(<<<z_k>>>, <<<z_k>>>)
and
m4_math(<<<p_k>>>, <<<p_k>>>)
are the variables after
m4_math(<<<k>>>, <<<k>>>)
steps, and they approach a common value. Step
m4_math(<<<k>>>, <<<k>>>)
also adds a term

m4_displaymath(
<<<\frac{6}{4^k d_k}\thinspace R_C(1, 1 + e_k),>>>,
<<<6/(4^k d_k) R_C(1, 1 + e_k),>>>)

where

m4_displaymath(
<<<d_k = (\sqrt{p_k}+\sqrt{x_k})(\sqrt{p_k}+\sqrt{y_k})(\sqrt{p_k}+\sqrt{z_k}), \qquad e_k = \frac{(p_k-x_k)(p_k-y_k)(p_k-z_k)}{d_k^2} .>>>,
<<<d_k = (sqrt(p_k)+sqrt(x_k))(sqrt(p_k)+sqrt(y_k))(sqrt(p_k)+sqrt(z_k)), e_k = ((p_k-x_k)(p_k-y_k)(p_k-z_k))/(d_k^2) .>>>)

When
m4_math(<<<p_k>>>, <<<p_k>>>)
is much smaller than
m4_mathcomma(<<<x_k>>>, <<<x_k>>>)
m4_math(<<<y_k>>>, <<<y_k>>>)
and
m4_mathcomma(<<<z_k>>>, <<<z_k>>>)
m4_math(<<<e_k>>>, <<<e_k>>>)
is close to
m4_math(<<<-1>>>, <<<-1>>>)
and
m4_math(<<<1 + e_k>>>, <<<1 + e_k>>>)
is about
m4_mathdot(<<<2\sqrt{p_k}\thinspace (1/\sqrt{x_k} + 1/\sqrt{y_k} + 1/\sqrt{z_k})>>>, <<<2sqrt(p_k) (1/sqrt(x_k) + 1/sqrt(y_k) + 1/sqrt(z_k))>>>)
Computed as a sum, it has a relative error of about
m4_mathdot(<<<\varepsilon/\sqrt{p_k}>>>, <<<ε/sqrt(p_k)>>>)
Since
m4_math(<<<R_C(1, w)>>>, <<<R_C(1, w)>>>)
is about
m4_math(<<<\frac12\ln(4/w)>>>, <<<1/2 ln(4/w)>>>)
for small
m4_mathcomma(<<<w>>>, <<<w>>>)
that is also the absolute error of
m4_mathdot(<<<R_C>>>, <<<R_C>>>)

So even with an accurate
m4_mathcomma(<<<p>>>, <<<p>>>)
m4_math(<<<R_J>>>, <<<R_J>>>)
was inaccurate for small
m4_mathdot(<<<p>>>, <<<p>>>)
For example, @code{carlson_rj(0.5, 1.0, 2.0, float(2^-40))} had an error
of
m4_math(<<<3.4\times10^{4}>>>, <<<3.4×10^4>>>)
ulps. In @code{elliptic_pi} this happens just before the pole, where
m4_math(<<<p>>>, <<<p>>>)
is small and positive.

The sum
m4_math(<<<1 + e_k>>>, <<<1 + e_k>>>)
cancels whenever
m4_math(<<<e_k>>>, <<<e_k>>>)
is near
m4_mathdot(<<<-1>>>, <<<-1>>>)
That also happens when the arguments are far apart and an odd number of
m4_mathcomma(<<<x_k>>>, <<<x_k>>>)
m4_math(<<<y_k>>>, <<<y_k>>>)
and
m4_math(<<<z_k>>>, <<<z_k>>>)
are larger than
m4_mathdot(<<<p_k>>>, <<<p_k>>>)
For example, with
m4_mathcomma(<<<x = 0.25>>>, <<<x = 0.25>>>)
m4_mathcomma(<<<y = 2^{40}>>>, <<<y = 2^40>>>)
m4_math(<<<z = 4>>>, <<<z = 4>>>)
and
m4_mathcomma(<<<p = 2^{20}>>>, <<<p = 2^20>>>)
the double-float @code{carlson_rj} was off by 24 ulps.

@node Deciding Whether the Pole Has Been Crossed
@section Deciding Whether the Pole Has Been Crossed

@code{bf-elliptic-pi-residue} added
m4_math(<<<-i\sigma>>>, <<<-iσ>>>)
when
m4_mathcomma(<<<n s^2 > 1>>>, <<<n s^2 > 1>>>)
computed in working precision. @code{bf-rj} takes its principal-value
branch when
m4_mathdot(<<<p < 0>>>, <<<p < 0>>>)
With
m4_math(<<<p>>>, <<<p>>>)
computed as
m4_math(<<<1 - n s^2>>>, <<<1 - n s^2>>>)
the two tests agreed with each other, but for
m4_math(<<<\phi>>>, <<<φ>>>)
within a few rounding units of the pole both could be on the wrong side.
There the imaginary part was missing or spurious, and the real part was
wrong in the first few digits. One input gave a Lisp error, because
m4_math(<<<p>>>, <<<p>>>)
was computed as exactly 0.

@node The Reduction of the Angle
@section The Reduction of the Angle

The reduction of
m4_math(<<<\phi>>>, <<<φ>>>)
to
m4_math(<<<r = \phi - k\pi>>>, <<<r = φ - kπ>>>)
had two problems: the reduced angle
m4_math(<<<r>>>, <<<r>>>)
was inaccurate, and the integer
m4_math(<<<k>>>, <<<k>>>)
could be off by one.

@anchor{s3-4-1}
@subsection The Rounding Error of the Reduced Angle

@code{bf-elliptic-pi} computed the reduced angle
m4_math(<<<r = \phi - k\pi>>>, <<<r = φ - kπ>>>)
and then took its sine and cosine. The product
m4_math(<<<k\pi>>>, <<<kπ>>>)
is rounded, so
m4_math(<<<r>>>, <<<r>>>)
has an absolute error of about
m4_math(<<<k>>>, <<<k>>>)
rounding units of
m4_mathdot(<<<\pi>>>, <<<π>>>)
The result grows like
m4_math(<<<k>>>, <<<k>>>)
too, through the term
m4_mathcomma(<<<2k\thinspace \Pi(n\thinspace |\thinspace m)>>>, <<<2k Π(n | m)>>>)
so the relative effect is about one rounding unit times the size of the
integrand at
m4_mathcomma(<<<r>>>, <<<r>>>)
whatever
m4_math(<<<k>>>, <<<k>>>)
is. It matters only where the integrand is large: where
m4_math(<<<r>>>, <<<r>>>)
is near
m4_math(<<<\pi/2>>>, <<<π/2>>>)
with
m4_math(<<<n>>>, <<<n>>>)
or
m4_math(<<<m>>>, <<<m>>>)
near 1, and where
m4_math(<<<r>>>, <<<r>>>)
is near a pole for
m4_mathdot(<<<n > 1>>>, <<<n > 1>>>)
There the errors were already present at
m4_mathdot(<<<k = 1>>>, <<<k = 1>>>)

For generic arguments the reduction is harmless at every magnitude of
m4_mathdot(<<<\phi>>>, <<<φ>>>)
Worst errors in ulps, double float / bigfloat, on 40 random points for
each range of
m4_mathcomma(<<<\lvert\phi\rvert>>>, <<<|φ|>>>)
with
m4_mathcomma(<<<0 < m < 0.9>>>, <<<0 < m < 0.9>>>)
and
m4_math(<<<n < 0.9>>>, <<<n < 0.9>>>)
for @code{elliptic_pi}:

@anchor{t3-1}
@need 1877
@multitable @columnfractions 0.28 0.18 0.18 0.18 0.18
@headitem Function @tab @math{\lvert\phi\rvert < 10} @tab @math{10^2} to @math{10^4} @tab @math{10^8} to @math{10^{12}} @tab @math{10^{12}} to @math{10^{15}}
@item @code{elliptic_f} @tab 3.4 / 3.5 @tab 2.3 / 3.9 @tab 2.4 / 4.7 @tab 2.5 / 2.6
@item @code{elliptic_e} @tab 3.7 / 4.2 @tab 2.0 / 3.4 @tab 2.8 / 4.7 @tab 2.9 / 2.9
@item @code{elliptic_pi} @tab 6.0 / 5.3 @tab 6.1 / 6.2 @tab 4.3 / 5.3 @tab 4.8 / 5.8
@end multitable

@noindent
@strong{Table 3.1.} Worst errors in ulps, double float / bigfloat, for
generic arguments and large
m4_mathcomma(<<<\lvert\phi\rvert>>>, <<<|φ|>>>)
before the change of Section 4.4.1, with SBCL.

@code{elliptic_pi} was worse than the other two where the integrand is
large. With
m4_mathcomma(<<<\phi = (k + \frac12)\pi - \delta>>>, <<<φ = (k + 1/2)π - δ>>>)
m4_mathcomma(<<<10^{-6} < \delta < 10^{-2}>>>, <<<10^-6 < δ < 10^-2>>>)
and
m4_math(<<<1 - m>>>, <<<1 - m>>>)
between
m4_math(<<<10^{-8}>>>, <<<10^-8>>>)
and
m4_mathcomma(<<<10^{-3}>>>, <<<10^-3>>>)
@code{elliptic_f} and @code{elliptic_e} had errors of at most 9.3 ulps
for
m4_mathcomma(<<<\lvert\phi\rvert < 10^{8}>>>, <<<|φ| < 10^8>>>)
and @code{elliptic_pi} had 270 to 1030 ulps. @code{elliptic_f} takes the
sine and cosine of the unreduced angle, and the integrand of
@code{elliptic_e} is small near
m4_mathdot(<<<\pi/2>>>, <<<π/2>>>)

@anchor{s3-4-2}
@subsection A Period Count That Is Off by One

The code computed
m4_math(<<<k = \mathrm{round}(\phi/\pi)>>>, <<<k = round(φ/π)>>>)
in working precision. When
m4_math(<<<\phi>>>, <<<φ>>>)
is the floating-point number nearest an odd multiple of
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
the quotient
m4_math(<<<\phi/\pi>>>, <<<φ/π>>>)
can round to exactly a half-integer, and @code{round} then returns the
even neighbour, which can be off by one. Of 600 such numbers tested,
m4_math(<<<k>>>, <<<k>>>)
was wrong for about a quarter of the double floats and about half of the
bigfloats.

The formula of @ref{The Formula} uses only
m4_math(<<<\sin\phi>>>, <<<sin φ>>>)
and
m4_mathcomma(<<<\cos^2\phi>>>, <<<cos^2 φ>>>)
so with the wrong
m4_math(<<<k>>>, <<<k>>>)
it cannot tell a reduced angle of
m4_math(<<<\pi/2 + \delta>>>, <<<π/2 + δ>>>)
from one of
m4_mathdot(<<<\pi/2 - \delta>>>, <<<π/2 - δ>>>)
The result is off by about
m4_math(<<<2\delta>>>, <<<2δ>>>)
times the integrand at
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
which is large when
m4_math(<<<n>>>, <<<n>>>)
or
m4_math(<<<m>>>, <<<m>>>)
is near 1. For example, @code{elliptic_pi(0.5, 3*float(%pi/2),
float(1-2^-30))} was off by 3236 ulps. Bigfloats have the same problem
at their own precision: @code{bfloat(%pi/2)} at @code{fpprec : 32} is
above
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
so its
m4_math(<<<k>>>, <<<k>>>)
is 1, and 0 was used.

@code{elliptic_f}, and the imaginary part of @code{elliptic_e} for
m4_math(<<<m>>>, <<<m>>>)
just above 1, were wrong in the same way. This is bug
@url{https://sourceforge.net/p/maxima/bugs/5908/, #5908}.

@node The Complete Integral for m or n Equal to 1
@section The Complete Integral for @math{m} or @math{n} Equal to 1

The complete integral
m4_math(<<<\Pi(n\thinspace |\thinspace m)>>>, <<<Π(n | m)>>>)
is infinite for
m4_math(<<<m = 1>>>, <<<m = 1>>>)
and for
m4_mathdot(<<<n = 1>>>, <<<n = 1>>>)
@code{bf-elliptic-pi} always evaluated it, even for
m4_mathcomma(<<<k = 0>>>, <<<k = 0>>>)
where it is multiplied by 0. So for
m4_math(<<<m = 1>>>, <<<m = 1>>>)
or
m4_math(<<<n = 1>>>, <<<n = 1>>>)
the double-float @code{elliptic_pi} gave a Lisp error, and the bigfloat
one did not return. For example, @code{elliptic_pi(0.5, 1.0, 1.0)} and
@code{elliptic_pi(1.0, 1.5, 0.5)} failed, although both are finite.

Where the integral does diverge, for an exact real
m4_math(<<<\phi>>>, <<<φ>>>)
with
m4_math(<<<\lvert\phi\rvert \ge \pi/2>>>, <<<|φ| >= π/2>>>)
and
m4_math(<<<m = 1>>>, <<<m = 1>>>)
or
m4_mathcomma(<<<n = 1>>>, <<<n = 1>>>)
the simplifier returned a noun form.

@node Cancellation Between the Terms of the Formula
@section Cancellation Between the Terms of the Formula

In two regions each term of the formula of @ref{The Formula} is computed
accurately, but the terms nearly cancel each other.

@itemize @bullet
@item
@strong{Past the pole for}
m4_mathdot(<<<n > 1>>>, <<<n > 1>>>)
Here
m4_mathcomma(<<<p < 0>>>, <<<p < 0>>>)
and @code{bf-rj} computes the principal value of
m4_math(<<<R_J>>>, <<<R_J>>>)
from @url{https://dlmf.nist.gov/19.20.E14, DLMF 19.20.14}, which has
three terms. One of them is
m4_mathcomma(<<<-3R_F>>>, <<<-3R_F>>>)
and it nearly cancels the term
m4_mathpunc(<<<:>>>, <<<s\thinspace R_F>>>, <<<s R_F>>>)
together they leave
m4_mathdot(<<<-m\thinspace s\thinspace R_F/(n - m)>>>, <<<-m s R_F/(n - m)>>>)
The loss grows with
m4_mathcomma(<<<n>>>, <<<n>>>)
up to
m4_math(<<<2\times10^{4}>>>, <<<2×10^4>>>)
ulps for
m4_mathdot(<<<10 < n < 10^{4}>>>, <<<10 < n < 10^4>>>)
@item
m4_mathdot(<<<n \ll 0>>>, <<<n << 0>>>)
For
m4_math(<<<n < 0>>>, <<<n < 0>>>)
there is no pole, but the two terms of the formula have opposite signs,
and for
m4_math(<<<n \ll 0>>>, <<<n << 0>>>)
the result is much smaller than either. There the loss grows roughly
like
m4_mathcomma(<<<\sqrt{\lvert n\rvert}>>>, <<<sqrt(|n|)>>>)
to thousands of ulps for
m4_math(<<<n>>>, <<<n>>>)
near
m4_mathdot(<<<-10^{6}>>>, <<<-10^6>>>)
@end itemize

The complete integral loses accuracy in the same way for
m4_math(<<<n > 1>>>, <<<n > 1>>>)
and for
m4_mathcomma(<<<n \ll 0>>>, <<<n << 0>>>)
and it enters every result with
m4_math(<<<k \ne 0>>>, <<<k ≠ 0>>>)
through the term
m4_mathdot(<<<2k\thinspace \Pi(n\thinspace |\thinspace m)>>>, <<<2k Π(n | m)>>>)

@node Cancellation Past the Branch Point for m > 1
@section Cancellation Past the Branch Point for @math{m > 1}

For real
m4_mathcomma(<<<n>>>, <<<n>>>)
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
when the reduced angle is past the branch point,
m4_math(<<<y>>>, <<<y>>>)
is negative and the Carlson functions are complex. Both the real and the
imaginary part of the result then cancel in the formula of @ref{The
Formula}, with a loss that grows with
m4_mathpunc(<<<:>>>, <<<\lvert n\rvert>>>, <<<|n|>>>)
up to
m4_math(<<<10^{5}>>>, <<<10^5>>>)
ulps in the real part for
m4_mathcomma(<<<100 < n < 10^{4}>>>, <<<100 < n < 10^4>>>)
and thousands of ulps in the imaginary part for
m4_mathdot(<<<n < -1>>>, <<<n < -1>>>)

@node Examples
@section Examples

Double-float examples of the first three problems, with exact values
from mpmath at the exact double inputs, rounded to 17 significant
digits, or 10 for the imaginary part in the fifth row:

@anchor{t3-2}
@need 4666
@multitable @columnfractions 0.3 0.28 0.27 0.15
@headitem Expression @tab Before @tab Exact value @tab Error (ulps)
@item @code{elliptic_pi(float(1-2^-40), float(%pi/2)-2^-20, 0.5)} @tab @math{1164674.2577077919} @tab @math{1164674.2576808747} @tab @math{1.2\times10^{5}}
@item @code{elliptic_pi(0.5, float(%pi/2)-2^-20, float(1-2^-40))} @tab @math{27.489278290283551} @tab @math{27.489278290246303} @tab @math{1.1\times10^{4}}
@item @code{elliptic_pi(4.0, float(%pi/6)-2^-45, 0.5)} @tab @math{9.5437197713346826} @tab @math{9.5435107790762916} @tab @math{1.2\times10^{11}}
@item @code{realpart(elliptic_pi(4.0, float(%pi/6)+2^-30, 0.5))} @tab @math{6.3354856724920410} @tab @math{6.3354856645734671} @tab @math{8.9\times10^{6}}
@item @code{imagpart(elliptic_pi(0.5, float(%pi/4)+2^-30, 2.0))} @tab @math{-5.754449718\times10^{-5}} @tab @math{-5.754449623\times10^{-5}} @tab @math{1.4\times10^{8}}
@item @code{elliptic_pi(4.0, float(%pi/6), 0.5)} @tab @math{11.424932073116509} @tab @math{11.458898371615306} @tab @math{1.9\times10^{13}}
@item @code{carlson_rj(0.5, 1.0, 2.0, float(2^-40))} @tab @math{40.526533925197306} @tab @math{40.526533925442006} @tab @math{3.4\times10^{4}}
@end multitable

@noindent
@strong{Table 3.2.} Double-float examples of the problems of Sections
3.1 to 3.3: the value before the change, the exact value from mpmath,
and the error before the change, with SBCL.

For
m4_math(<<<n = 4>>>, <<<n = 4>>>)
the pole is at
m4_mathdot(<<<\pi/6>>>, <<<π/6>>>)
The double float @code{float(%pi/6)} is just below it, and
@code{float(%pi/6)+2^-53}, the next double, is just above it. For the
latter, @code{elliptic_pi(4.0, float(%pi/6)+2^-53, 0.5)} gave the Lisp
error. Bigfloats have the same problems: @code{elliptic_pi(4b0,
bfloat(%pi/6), 0.5b0)} at @code{fpprec : 32} returned
m4_math(<<<47.656\ldots>>>, <<<47.656...>>>)
with no imaginary part, and the exact value is
m4_mathdot(<<<23.582\ldots - 0.96951\ldots i>>>, <<<23.582... - 0.96951... i>>>)

@node The Solutions
@chapter The Solutions

Each of the seven problems of @ref{The Problems} has its own remedy.
They are described here in the same order.

@menu
* Computing y and p Accurately::
* The Duplication Step of R_J::
* Which Side of the Pole::
* Reducing the Angle::
* Skipping the Complete Integral::
* The Changes of Parameter::
* Separate Real and Imaginary Parts for m > 1::
@end menu

@node Computing y and p Accurately
@section Computing @math{y} and @math{p} Accurately

@code{bf-1-m*sin^2} computes
m4_math(<<<1 - m\sin^2\phi>>>, <<<1 - m sin^2 φ>>>)
to full relative accuracy. It was added for @code{elliptic_f}, and it
has two paths.

@strong{Real}
m4_math(<<<\phi>>>, <<<φ>>>)
@strong{and}
m4_mathdot(<<<\mathrm{Re}\thinspace m \le 1>>>, <<<Re m <= 1>>>)
It uses

m4_displaymath(
<<<1 - m\sin^2\phi = \cos^2\phi + (1-m)\sin^2\phi .>>>,
<<<1 - m sin^2 φ = cos^2 φ + (1-m) sin^2 φ .>>>)

The real part is a sum of nonnegative terms, and the imaginary part is
m4_mathcomma(<<<-\mathrm{Im}\thinspace m\thinspace \sin^2\phi>>>, <<<-Im m sin^2 φ>>>)
so nothing cancels. For
m4_math(<<<m>>>, <<<m>>>)
near 1,
m4_math(<<<1 - m>>>, <<<1 - m>>>)
is computed exactly.

@strong{Other arguments.} It first computes
m4_math(<<<y = 1 - m\sin^2\phi>>>, <<<y = 1 - m sin^2 φ>>>)
in working precision. If
m4_math(<<<\lvert y\rvert > 1/16>>>, <<<|y| > 1/16>>>)
it returns that. Otherwise it treats
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
as exact and recomputes
m4_math(<<<y>>>, <<<y>>>)
in bigfloat arithmetic with
m4_math(<<<P + e + 8>>>, <<<P + e + 8>>>)
bits, where
m4_math(<<<P>>>, <<<P>>>)
is the working precision and the extra precision
m4_math(<<<e>>>, <<<e>>>)
starts at
m4_mathdot(<<<P>>>, <<<P>>>)
If the new
m4_math(<<<\lvert y\rvert>>>, <<<|y|>>>)
is at least
m4_mathcomma(<<<2^{-e}>>>, <<<2^-e>>>)
it has at least
m4_math(<<<P + 8>>>, <<<P + 8>>>)
correct bits and is rounded to working precision. If not,
m4_math(<<<e>>>, <<<e>>>)
is doubled, up to
m4_mathdot(<<<8P>>>, <<<8P>>>)

The change calls this function for both quantities:
m4_math(<<<y>>>, <<<y>>>)
with
m4_mathcomma(<<<m>>>, <<<m>>>)
and
m4_math(<<<p>>>, <<<p>>>)
with
m4_math(<<<n>>>, <<<n>>>)
in place of
m4_mathdot(<<<m>>>, <<<m>>>)
That covers the four cases of @ref{Cancellation in y and p}:

@anchor{t4-1}
@need 3017
@multitable @columnfractions 0.4 0.2 0.4
@headitem Case @tab Quantity @tab Path
@item @math{n} near 1 from below, @math{\phi} near @math{\pi/2} @tab @math{p} @tab Sum of nonnegative terms
@item @math{m} near 1 from below, @math{\phi} near @math{\pi/2} @tab @math{y} @tab Sum of nonnegative terms
@item @math{n > 1}, @math{\phi} near the pole @tab @math{p} @tab Extra precision
@item @math{m > 1}, @math{\phi} near the branch point @tab @math{y} @tab Extra precision
@item Complex @math{\phi}, or @math{\mathrm{Re}\thinspace n > 1} or @math{\mathrm{Re}\thinspace m > 1}, with small @math{\lvert p\rvert} or @math{\lvert y\rvert} @tab @math{p} or @math{y} @tab Extra precision
@end multitable

@noindent
@strong{Table 4.1.} Which path of @code{bf-1-m*sin^2} handles each case.

For real
m4_math(<<<\phi>>>, <<<φ>>>)
and complex
m4_math(<<<n>>>, <<<n>>>)
or
m4_math(<<<m>>>, <<<m>>>)
with real part at most 1, the first path applies, and both parts of
m4_math(<<<p>>>, <<<p>>>)
or
m4_math(<<<y>>>, <<<y>>>)
are accurate.

The limit of
m4_math(<<<1/16>>>, <<<1/16>>>)
is an optional argument. Above it the result is a plain subtraction,
with a relative error of up to 16 rounding units. The formula used past
the pole (@pxref{The Changes of Parameter}) is more sensitive to errors
in
m4_mathcomma(<<<p>>>, <<<p>>>)
and for
m4_math(<<<m > 1>>>, <<<m > 1>>>)
in
m4_mathcomma(<<<y>>>, <<<y>>>)
so there @code{bf-elliptic-pi} computes them again with a limit of
m4_math(<<<1/4>>>, <<<1/4>>>)
when they are between
m4_math(<<<1/16>>>, <<<1/16>>>)
and
m4_math(<<<1/4>>>, <<<1/4>>>)
in absolute value. Away from the zero of the real part, that took the
worst errors from 12.4 ulps to 6.9 in double float for
m4_mathcomma(<<<n > 1>>>, <<<n > 1>>>)
m4_math(<<<m < 1>>>, <<<m < 1>>>)
past the pole, and from 16.9 / 26.0 ulps to 5.9 / 9.1, double float /
bigfloat, for
m4_math(<<<n > m > 1>>>, <<<n > m > 1>>>)
between the pole and the branch point. The time per call there went from
52 to 54 and from 65 to 75 microseconds.

@code{bf-1-m*sin^2} is called with the unreduced
m4_mathdot(<<<\phi>>>, <<<φ>>>)
It depends on
m4_math(<<<\phi>>>, <<<φ>>>)
only through
m4_math(<<<\sin^2\phi>>>, <<<sin^2 φ>>>)
and
m4_mathcomma(<<<\cos^2\phi>>>, <<<cos^2 φ>>>)
which are the same for the reduced angle (@pxref{Reducing the Angle}).

@node The Duplication Step of R_J
@section The Duplication Step of @math{R_J}

Consider one step of @ref{Cancellation Inside R_J}. Drop the step index
m4_mathcomma(<<<k>>>, <<<k>>>)
and write
m4_mathcomma(<<<x_1>>>, <<<x_1>>>)
m4_mathcomma(<<<x_2>>>, <<<x_2>>>)
m4_math(<<<x_3>>>, <<<x_3>>>)
for the three variables
m4_mathcomma(<<<x_k>>>, <<<x_k>>>)
m4_mathcomma(<<<y_k>>>, <<<y_k>>>)
m4_math(<<<z_k>>>, <<<z_k>>>)
of that step, and
m4_math(<<<p>>>, <<<p>>>)
for
m4_mathdot(<<<p_k>>>, <<<p_k>>>)
Since
m4_mathcomma(<<<p - x_i = (\sqrt p - \sqrt{x_i})(\sqrt p + \sqrt{x_i})>>>, <<<p - x_i = (sqrt(p) - sqrt(x_i))(sqrt(p) + sqrt(x_i))>>>)

m4_displaymath(
<<<e = t_1 t_2 t_3, \qquad t_i = \frac{\sqrt p - \sqrt{x_i}}{\sqrt p + \sqrt{x_i}} .>>>,
<<<e = t_1 t_2 t_3, t_i = (sqrt(p) - sqrt(x_i))/(sqrt(p) + sqrt(x_i)) .>>>)

For real positive arguments each
m4_math(<<<t_i>>>, <<<t_i>>>)
is between
m4_math(<<<-1>>>, <<<-1>>>)
and 1. The sum
m4_math(<<<1 + e>>>, <<<1 + e>>>)
cancels when the product is near
m4_mathpunc(<<<:>>>, <<<-1>>>, <<<-1>>>)
when
m4_math(<<<p>>>, <<<p>>>)
is much smaller than all three
m4_mathcomma(<<<x_i>>>, <<<x_i>>>)
and whenever the arguments are far apart and an odd number of the
m4_math(<<<x_i>>>, <<<x_i>>>)
are larger than
m4_mathdot(<<<p>>>, <<<p>>>)

@strong{Real arguments.} Each factor is written with a small quantity of
its own, chosen by which of
m4_math(<<<\sqrt p>>>, <<<sqrt(p)>>>)
and
m4_math(<<<\sqrt{x_i}>>>, <<<sqrt(x_i)>>>)
is larger:

m4_displaymath(
<<<\lvert t_i\rvert = 1 - w_i, \qquad w_i = \frac{2\min(\sqrt p, \sqrt{x_i})}{\sqrt p + \sqrt{x_i}}, \qquad 0 \le w_i \le 1 .>>>,
<<<|t_i| = 1 - w_i, w_i = (2min(sqrt(p), sqrt(x_i)))/(sqrt(p) + sqrt(x_i)), 0 <= w_i <= 1 .>>>)

If an even number of the
m4_math(<<<t_i>>>, <<<t_i>>>)
are negative, the product is positive and

m4_displaymath(
<<<1 + e = 1 + \lvert t_1\rvert\thinspace \lvert t_2\rvert\thinspace \lvert t_3\rvert .>>>,
<<<1 + e = 1 + |t_1| |t_2| |t_3| .>>>)

If an odd number are negative,

m4_displaymath(
<<<1 + e = 1 - (1 - w_1)(1 - w_2)(1 - w_3) = w_1 + \lvert t_1\rvert\thinspace \bigl(w_2 + \lvert t_2\rvert\thinspace w_3\bigr) .>>>,
<<<1 + e = 1 - (1 - w_1)(1 - w_2)(1 - w_3) = w_1 + |t_1| (w_2 + |t_2| w_3) .>>>)

In both cases all the terms are nonnegative, so nothing cancels. The
only subtraction is in
m4_mathcomma(<<<\lvert\sqrt p - \sqrt{x_i}\rvert>>>, <<<|sqrt(p) - sqrt(x_i)|>>>)
and where it loses digits
m4_math(<<<\lvert t_i\rvert>>>, <<<|t_i|>>>)
is small and only multiplies. The square roots are those the step has
already computed.

@strong{Complex arguments.} The factors have no signs to use. With
m4_mathcomma(<<<a_i = 2\sqrt p/(\sqrt p + \sqrt{x_i})>>>, <<<a_i = 2sqrt(p)/(sqrt(p) + sqrt(x_i))>>>)
so that
m4_mathcomma(<<<t_i = a_i - 1>>>, <<<t_i = a_i - 1>>>)
expanding the product gives

m4_displaymath(
<<<1 + e = (a_1 + a_2 + a_3) - (a_1 a_2 + a_1 a_3 + a_2 a_3) + a_1 a_2 a_3 ,>>>,
<<<1 + e = (a_1 + a_2 + a_3) - (a_1 a_2 + a_1 a_3 + a_2 a_3) + a_1 a_2 a_3 ,>>>)

which does not cancel when
m4_math(<<<p>>>, <<<p>>>)
is much smaller than the
m4_mathcomma(<<<x_i>>>, <<<x_i>>>)
where the
m4_math(<<<a_i>>>, <<<a_i>>>)
are small. @code{bf-rj-rc-arg} first computes
m4_math(<<<r = 1 + e>>>, <<<r = 1 + e>>>)
directly. If
m4_math(<<<\lvert r\rvert \ge 1/2>>>, <<<|r| >= 1/2>>>)
it returns
m4_mathcomma(<<<r>>>, <<<r>>>)
whose relative error is then at most a few rounding units. Otherwise it
returns the expansion.

Each duplication step replaces
m4_math(<<<p>>>, <<<p>>>)
by
m4_mathcomma(<<<(p + \lambda)/4>>>, <<<(p + λ)/4>>>)
with
m4_mathcomma(<<<\lambda = \sqrt{x_1 x_2} + \sqrt{x_1 x_3} + \sqrt{x_2 x_3}>>>, <<<λ = sqrt(x_1 x_2) + sqrt(x_1 x_3) + sqrt(x_2 x_3)>>>)
so the variables approach each other, and the cancellation is in the
first few steps.

Past the pole,
m4_mathcomma(<<<p < 0>>>, <<<p < 0>>>)
and @code{bf-rj} calls @code{bf-rj1} with a transformed fourth argument,
which is in general not small. So for @code{elliptic_pi} near the pole
this part of the change matters just before the pole: with the change of
@ref{Computing y and p Accurately} alone, the errors that remained near
the pole, up to
m4_math(<<<5\times10^{4}>>>, <<<5×10^4>>>)
ulps, were all on that side. With both parts of the change, the results
are accurate on both sides.

@node Which Side of the Pole
@section Which Side of the Pole

@code{bf-elliptic-pi-residue} now counts the pole as crossed when the
accurate
m4_math(<<<p>>>, <<<p>>>)
is negative and the accurate
m4_math(<<<y>>>, <<<y>>>)
is nonnegative. Both come from @code{bf-1-m*sin^2}. These are the values
that @code{bf-elliptic-pi} passes to @code{bf-rj}, and @code{bf-rj}
takes its principal-value branch under the same two conditions. So the
imaginary part is added exactly when the real part is the principal
value.

For inputs taken as exact, the sign of
m4_math(<<<p>>>, <<<p>>>)
is now the correct one, even for the floating-point numbers next to the
pole. After the change:

@anchor{t4-2}
@need 2218
@multitable @columnfractions 0.3 0.26 0.27 0.17
@headitem Expression @tab Real part @tab Imaginary part @tab Error (ulps)
@item @code{elliptic_pi(4.0, float(%pi/6)-2^-53, 0.5)} @tab @math{11.126769330243924} @tab @math{0} @tab 1.25
@item @code{elliptic_pi(4.0, float(%pi/6), 0.5)} @tab @math{11.458898371615305} @tab @math{0} @tab 0.45
@item @code{elliptic_pi(4.0, float(%pi/6)+2^-53, 0.5)} @tab @math{11.480110833541522} @tab @math{-0.9695165413304057} @tab 1.14, 0.32
@end multitable

@noindent
@strong{Table 4.2.} Double-float results at three consecutive doubles
around the pole at
m4_mathcomma(<<<\pi/6>>>, <<<π/6>>>)
after all the changes, with the errors of the real and imaginary parts,
with SBCL.

@node Reducing the Angle
@section Reducing the Angle

@anchor{s4-4-1}
@subsection Using the Unreduced Angle

As in @code{bf-elliptic-f}, @code{bf-elliptic-pi} now uses

m4_displaymath(
<<<\sin(\phi - k\pi) = (-1)^k \sin\phi, \qquad \cos^2(\phi - k\pi) = \cos^2\phi, \qquad \sin^2(\phi - k\pi) = \sin^2\phi .>>>,
<<<sin(φ - kπ) = (-1)^k sin φ, cos^2(φ - kπ) = cos^2 φ, sin^2(φ - kπ) = sin^2 φ .>>>)

The formula of @ref{The Formula} needs only the sine of the reduced
angle and the squares of its sine and cosine, so it is evaluated with
m4_mathcomma(<<<(-1)^k\sin\phi>>>, <<<(-1)^k sin φ>>>)
m4_mathcomma(<<<\cos\phi>>>, <<<cos φ>>>)
and
m4_math(<<<y>>>, <<<y>>>)
and
m4_math(<<<p>>>, <<<p>>>)
from @code{bf-1-m*sin^2} at the unreduced
m4_mathdot(<<<\phi>>>, <<<φ>>>)
The sine and cosine of
m4_math(<<<\phi>>>, <<<φ>>>)
itself are accurate, so the small cosine near an odd multiple of
m4_math(<<<\pi/2>>>, <<<π/2>>>)
keeps its relative accuracy. This relies on the Lisp reducing the
argument of its @code{sin} and @code{cos} accurately. The reduced angle
is no longer formed. @code{bf-elliptic-pi-residue} computes its two
tests from the unreduced
m4_math(<<<\phi>>>, <<<φ>>>)
as well, so that it still agrees with @code{bf-elliptic-pi}
(@pxref{Which Side of the Pole}).

Worst errors in ulps of the real part, double float / bigfloat, before
and after this change, on the same 40 points for each range:

@anchor{t4-3}
@need 5014
@multitable @columnfractions 0.34 0.14 0.26 0.26
@headitem Reduced angle @tab @math{\lvert\phi\rvert} @tab Before @tab After
@item Near @math{\pi/2}, @math{m} near 1, @math{-2 < n < 0.9} @tab Below 10 @tab 276 / 475 @tab 6.1 / 13.8
@item The same @tab 10 to @math{10^2} @tab 738 / 647 @tab 5.4 / 14.5
@item The same @tab @math{10^2} to @math{10^4} @tab 590 / 739 @tab 5.4 / 8.5
@item The same @tab @math{10^4} to @math{10^8} @tab 816 / 1030 @tab 5.1 / 8.2
@item Near @math{\pi/2}, @math{n} near 1, @math{0 < m < 0.9} @tab Below 10 @tab 4870 / 8590 @tab 8.0 / 8.6
@item The same @tab 10 to @math{10^2} @tab 9940 / 3600 @tab 5.3 / 7.2
@item The same @tab @math{10^2} to @math{10^4} @tab 6520 / 6380 @tab 5.5 / 5.4
@item The same @tab @math{10^4} to @math{10^8} @tab 3280 / 3420 @tab 4.7 / 7.8
@item Within @math{10^{-3}} to @math{10^{-9}} of a pole, relatively, @math{1.1 < n < 8} @tab Below 10 @tab @math{4.2\times10^{7}} / @math{7.3\times10^{7}} @tab 7.7 / 10.5
@item The same @tab 10 to @math{10^2} @tab @math{6.8\times10^{8}} / @math{1.2\times10^{9}} @tab 46 / 116
@end multitable

@noindent
@strong{Table 4.3.} Worst errors in ulps of the real part of
@code{elliptic_pi}, double float / bigfloat, where the reduction
matters, before and after the change of Section 4.4.1, with SBCL.

Near
m4_math(<<<\pi/2>>>, <<<π/2>>>)
the perturbations are those of @ref{s3-4-1,,Section 3.4.1}, with
m4_math(<<<1 - n>>>, <<<1 - n>>>)
in the same range as
m4_mathdot(<<<1 - m>>>, <<<1 - m>>>)
For generic arguments the results are unchanged, at 3 to 9 ulps. Near a
pole with
m4_mathcomma(<<<\lvert\phi\rvert > 10>>>, <<<|φ| > 10>>>)
what remained was the cancellation against
m4_math(<<<2k\thinspace \Pi(n\thinspace |\thinspace m)>>>, <<<2k Π(n | m)>>>)
for
m4_mathcomma(<<<n > 1>>>, <<<n > 1>>>)
which the change of @ref{The Changes of Parameter} removes.

@anchor{s4-4-2}
@subsection Correcting @math{k}

The correct
m4_math(<<<k>>>, <<<k>>>)
is the one for which the reduced angle
m4_math(<<<r = \phi - k\pi>>>, <<<r = φ - kπ>>>)
is within
m4_math(<<<\pi/2>>>, <<<π/2>>>)
of 0, which means that its cosine is not negative. Since

m4_displaymath(
<<<\cos r = (-1)^k\cos\phi, \qquad \sin r = (-1)^k\sin\phi ,>>>,
<<<cos r = (-1)^k cos φ, sin r = (-1)^k sin φ ,>>>)

both signs come from the unreduced
m4_math(<<<\phi>>>, <<<φ>>>)
without cancellation. The new function @code{bf-elliptic-period}
computes
m4_math(<<<k = \mathrm{round}(\phi/\pi)>>>, <<<k = round(φ/π)>>>)
as before. If
m4_math(<<<(-1)^k\cos\phi>>>, <<<(-1)^k cos φ>>>)
is negative,
m4_math(<<<k>>>, <<<k>>>)
is off by one, and the function adds 1 to it when
m4_math(<<<(-1)^k\sin\phi>>>, <<<(-1)^k sin φ>>>)
is positive and subtracts 1 otherwise. A floating-point number is never
exactly an odd multiple of
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
so the test always decides.

@code{round} also returns the remainder, and the signs are examined only
when its absolute value is above
m4_mathcomma(<<<3/2>>>, <<<3/2>>>)
that is, when the reduced angle is near
m4_mathdot(<<<\pi/2>>>, <<<π/2>>>)
So there is no measurable cost for other
m4_mathdot(<<<\phi>>>, <<<φ>>>)

@code{bf-elliptic-pi} and @code{bf-elliptic-pi-residue} call
@code{bf-elliptic-period}, and so do the functions for @code{elliptic_f}
and @code{elliptic_e}. For complex
m4_math(<<<\phi>>>, <<<φ>>>)
it is called with the real part.

Worst errors in ulps, double float / bigfloat, at the 600 floating-point
numbers nearest
m4_mathcomma(<<<(2j + 1)\pi/2>>>, <<<(2j + 1)π/2>>>)
for
m4_math(<<<j>>>, <<<j>>>)
from
m4_math(<<<-40>>>, <<<-40>>>)
to 400 and near
m4_mathcomma(<<<10^{4}>>>, <<<10^4>>>)
m4_mathcomma(<<<10^{6}>>>, <<<10^6>>>)
m4_math(<<<10^{9}>>>, <<<10^9>>>)
and
m4_mathpunc(<<<:>>>, <<<10^{12}>>>, <<<10^12>>>)

@anchor{t4-4}
@need 2337
@multitable @columnfractions 0.5 0.27 0.23
@headitem Function @tab Before @tab After
@item @code{elliptic_f}, @math{m = 1 - 2^{-30}} @tab 3910 / 7980 @tab 2.8 / 2.0
@item @code{elliptic_e}, @math{m = 1 + 2^{-20}}, imaginary part @tab 1980 / 2050 @tab 2.8 / 1.7
@item @code{elliptic_e}, @math{m = 1/2} @tab 1.5 / 3.3 @tab 1.6 / 3.3
@item @code{elliptic_pi}, @math{n = 1/2}, @math{m = 1 - 2^{-30}} @tab 3910 / 8080 @tab 2.8 / 1.3
@item @code{elliptic_pi}, @math{n = 1 - 2^{-20}}, @math{m = 1/2} @tab 1300 / 1440 @tab 2.4 / 2.0
@end multitable

@noindent
@strong{Table 4.4.} Worst errors in ulps, double float / bigfloat, next
to odd multiples of
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
before and after the correction of
m4_mathcomma(<<<k>>>, <<<k>>>)
with SBCL.

For
m4_math(<<<m < 1>>>, <<<m < 1>>>)
away from 1 the integrand is of order 1 at
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
and the wrong
m4_math(<<<k>>>, <<<k>>>)
made no visible difference.

Since @code{bfloat(%pi/2)} is above
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
@code{elliptic_f(bfloat(%pi/2), 1b0)} and @code{elliptic_pi} with
m4_math(<<<m = 1>>>, <<<m = 1>>>)
or
m4_math(<<<n = 1>>>, <<<n = 1>>>)
at that angle now signal the error of @ref{Skipping the Complete
Integral}, where they returned a large finite value before.

@node Skipping the Complete Integral
@section Skipping the Complete Integral

@code{bf-elliptic-pi} now evaluates the complete integral only when
m4_mathdot(<<<k \ne 0>>>, <<<k ≠ 0>>>)

@itemize @bullet
@item
For
m4_mathcomma(<<<k = 0>>>, <<<k = 0>>>)
which is
m4_math(<<<\lvert\phi\rvert < \pi/2>>>, <<<|φ| < π/2>>>)
for real
m4_mathcomma(<<<\phi>>>, <<<φ>>>)
the formula of @ref{The Formula} gives the finite value:
@code{elliptic_pi(0.5, 1.0, 1.0)} is
m4_mathcomma(<<<1.4830998734200773>>>, <<<1.4830998734200773>>>)
and @code{elliptic_pi(1.0, 1.5, 0.5)} is
m4_mathdot(<<<19.14520430777344>>>, <<<19.14520430777344>>>)
@item
For
m4_math(<<<k \ne 0>>>, <<<k ≠ 0>>>)
and
m4_math(<<<m = 1>>>, <<<m = 1>>>)
or
m4_mathcomma(<<<n = 1>>>, <<<n = 1>>>)
it signals a Maxima error, which says that the integral diverges and
names the parameter that is 1.
@item
The simplifier signals the same error for an exact real
m4_math(<<<\phi>>>, <<<φ>>>)
with
m4_math(<<<\lvert\phi\rvert \ge \pi/2>>>, <<<|φ| >= π/2>>>)
and
m4_math(<<<m = 1>>>, <<<m = 1>>>)
or
m4_mathcomma(<<<n = 1>>>, <<<n = 1>>>)
when
m4_math(<<<\phi>>>, <<<φ>>>)
is a number or a rational multiple of
m4_mathcomma(<<<\pi>>>, <<<π>>>)
such as @code{elliptic_pi(1/2, 3*%pi/2, 1)}. It uses
@code{elliptic-period-count}, which the @code{elliptic_f} and
@code{elliptic_e} simplifiers use for the same purpose.
@end itemize

Skipping the complete integral for
m4_math(<<<k = 0>>>, <<<k = 0>>>)
also makes @code{elliptic_pi} faster (@pxref{Speed}).

For complex floating-point
m4_math(<<<\phi>>>, <<<φ>>>)
with
m4_math(<<<m = 1>>>, <<<m = 1>>>)
or
m4_math(<<<n = 1>>>, <<<n = 1>>>)
and
m4_math(<<<\lvert\mathrm{Re}\thinspace \phi\rvert>>>, <<<|Re φ|>>>)
past
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
the error is signalled too. On the branch that @code{elliptic_pi}
computes, the value there is not finite: as
m4_math(<<<m \to 1>>>, <<<m -> 1>>>)
or
m4_math(<<<n \to 1>>>, <<<n -> 1>>>)
the real part grows without bound and only the imaginary part converges.
For example, @code{elliptic_pi(0.5, 2.0+%i, m)} is
m4_math(<<<39.79 + 1.2447\thinspace i>>>, <<<39.79 + 1.2447 i>>>)
at
m4_mathcomma(<<<m = 1 - 10^{-8}>>>, <<<m = 1 - 10^-8>>>)
and @code{elliptic_pi(n, 2.0+%i, 0.5)} is
m4_math(<<<14048 + 1.0766\thinspace i>>>, <<<14048 + 1.0766 i>>>)
at
m4_mathdot(<<<n = 1 - 10^{-7}>>>, <<<n = 1 - 10^-7>>>)
mpmath returns an infinite real part with these imaginary parts. The
integral along a straight line from 0 to
m4_math(<<<\phi>>>, <<<φ>>>)
is finite, but it is a different branch, which is not the limit of the
values for
m4_mathdot(<<<m < 1>>>, <<<m < 1>>>)
@code{elliptic_f} signals an error in the same case.

@node The Changes of Parameter
@section The Changes of Parameter

Two changes of parameter from @url{https://dlmf.nist.gov/19.7.iii, DLMF
§19.7(iii)} avoid the cancellation of @ref{Cancellation Between the
Terms of the Formula}, one for
m4_math(<<<n > 1>>>, <<<n > 1>>>)
past the pole and one for
m4_mathdot(<<<n < -1>>>, <<<n < -1>>>)

@anchor{s4-6-1}
@subsection The Change of Parameter for @math{n > 1} Past the Pole

Let
m4_mathdot(<<<w = m/n>>>, <<<w = m/n>>>)
@url{https://dlmf.nist.gov/19.7.E8, DLMF 19.7.8} relates the integral
with parameter
m4_math(<<<n>>>, <<<n>>>)
to the one with parameter
m4_mathdot(<<<w>>>, <<<w>>>)
With
m4_mathcomma(<<<C = 1/\sin^2\phi>>>, <<<C = 1/sin^2 φ>>>)

m4_displaymath(
<<<\Pi(n;\phi\thinspace |\thinspace m) + \Pi(w;\phi\thinspace |\thinspace m) = F(\phi\thinspace |\thinspace m) + \sqrt{C}\thinspace R_C\bigl((C-1)(C-m),\thinspace (C-n)(C-w)\bigr) .>>>,
<<<Π(n;φ | m) + Π(w;φ | m) = F(φ | m) + sqrt(C) R_C((C-1)(C-m), (C-n)(C-w)) .>>>)

By the formula of @ref{The Formula} with
m4_math(<<<w>>>, <<<w>>>)
in place of
m4_mathcomma(<<<n>>>, <<<n>>>)
the difference
m4_math(<<<F(\phi\thinspace |\thinspace m) - \Pi(w;\phi\thinspace |\thinspace m)>>>, <<<F(φ | m) - Π(w;φ | m)>>>)
is a single
m4_math(<<<R_J>>>, <<<R_J>>>)
term.
m4_math(<<<R_C>>>, <<<R_C>>>)
is homogeneous of degree
m4_mathcomma(<<<-1/2>>>, <<<-1/2>>>)
so its arguments can be multiplied by
m4_mathdot(<<<\sin^4\phi>>>, <<<sin^4 φ>>>)
With
m4_mathcomma(<<<q = 1 - w s^2>>>, <<<q = 1 - w s^2>>>)
and the other quantities as in @ref{The Formula}, the result is

m4_displaymath(
<<<\Pi(n;\phi\thinspace |\thinspace m) = s\thinspace R_C(c^2 y,\; p\thinspace q) - \frac{w}{3}\thinspace s^3 R_J(c^2, y, 1, q) .>>>,
<<<Π(n;φ | m) = s R_C(c^2 y, p q) - w/3 s^3 R_J(c^2, y, 1, q) .>>>)

It has two terms where the formula of @ref{The Formula} with the
principal value of
m4_math(<<<R_J>>>, <<<R_J>>>)
has four. Past the pole
m4_mathcomma(<<<p\thinspace q < 0>>>, <<<p q < 0>>>)
and @code{bf-rc} returns the principal value of
m4_math(<<<R_C>>>, <<<R_C>>>)
for a negative second argument, which is the value needed. Since
m4_math(<<<m < n>>>, <<<m < n>>>)
there,
m4_math(<<<q>>>, <<<q>>>)
is positive and
m4_math(<<<R_J>>>, <<<R_J>>>)
needs no principal value.
m4_math(<<<q>>>, <<<q>>>)
is computed by @code{bf-1-m*sin^2} with
m4_math(<<<w>>>, <<<w>>>)
as the parameter. For
m4_math(<<<s > 0>>>, <<<s > 0>>>)
the first term is positive. The second is positive for
m4_math(<<<m < 0>>>, <<<m < 0>>>)
and negative for
m4_mathdot(<<<0 < m < n>>>, <<<0 < m < n>>>)

@anchor{s4-6-2}
@subsection The Change of Parameter for @math{n < -1}

@url{https://dlmf.nist.gov/19.7.E9, DLMF 19.7.9} relates the integral
with parameter
m4_math(<<<n>>>, <<<n>>>)
to the one with parameter
m4_mathdot(<<<(m - n)/(1 - n)>>>, <<<(m - n)/(1 - n)>>>)
Replacing that integral by the formula of @ref{The Formula} and
collecting the terms gives, with

m4_displaymath(
<<<v = \frac{1-m}{1-n}, \qquad r = c^2 + v\thinspace s^2 ,>>>,
<<<v = (1-m)/(1-n), r = c^2 + v s^2 ,>>>)

the formula

m4_displaymath(
<<<\Pi(n;\phi\thinspace |\thinspace m) = \frac{1}{1-n}\left[\thinspace s\thinspace R_F(c^2, y, 1) - \frac{n\thinspace v}{3}\thinspace s^3 R_J(c^2, y, 1, r) - n\thinspace s\thinspace \lvert c\rvert\thinspace R_C(y,\; p\thinspace r)\right] .>>>,
<<<Π(n;φ | m) = 1/(1-n) [ s R_F(c^2, y, 1) - (n v)/3 s^3 R_J(c^2, y, 1, r) - n s |c| R_C(y, p r)] .>>>)

For
m4_math(<<<n < 0>>>, <<<n < 0>>>)
and
m4_mathcomma(<<<m < 1>>>, <<<m < 1>>>)
and
m4_mathcomma(<<<s > 0>>>, <<<s > 0>>>)
all three terms are positive, whatever the sign of
m4_mathcomma(<<<m>>>, <<<m>>>)
so nothing cancels.
m4_math(<<<r>>>, <<<r>>>)
is a sum of positive terms, and
m4_math(<<<1 - m>>>, <<<1 - m>>>)
is exact when
m4_math(<<<m>>>, <<<m>>>)
is near 1. For
m4_math(<<<m > 1>>>, <<<m > 1>>>)
before the branch point the second term is negative, but the formula was
measured to be as accurate there.

@anchor{s4-6-3}
@subsection Where the Changes of Parameter Are Used

@code{bf-elliptic-pi} uses the formulas of @ref{s4-6-1,,Section 4.6.1}
and @ref{s4-6-2,,Section 4.6.2} when
m4_mathcomma(<<<n>>>, <<<n>>>)
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_math(<<<m>>>, <<<m>>>)
are real: the first when
m4_math(<<<y \ge 0>>>, <<<y >= 0>>>)
and
m4_mathcomma(<<<p < 0>>>, <<<p < 0>>>)
and the second when
m4_mathdot(<<<n < -1>>>, <<<n < -1>>>)
Everywhere else it uses the formula of @ref{The Formula}, as before.
Both formulas are identities for other
m4_math(<<<n>>>, <<<n>>>)
too, but they are not better there. For
m4_math(<<<-1 < n < 0>>>, <<<-1 < n < 0>>>)
the formula of @ref{The Formula} loses nothing. The first formula was
also tried for
m4_mathpunc(<<<:>>>, <<<n < 0>>>, <<<n < 0>>>)
it is accurate for
m4_mathcomma(<<<m \ge n/2>>>, <<<m >= n/2>>>)
but loses up to 30 ulps in double float and 57 in bigfloat for
m4_mathdot(<<<m < n/2>>>, <<<m < n/2>>>)
Where the first formula is not used for
m4_mathcomma(<<<n > 0>>>, <<<n > 0>>>)
the worst errors in ulps, double float / bigfloat, with the formula of
@ref{The Formula} and with the first formula, on 40 random points per
set with
m4_mathcomma(<<<\lvert\phi\rvert \le \pi/2>>>, <<<|φ| <= π/2>>>)
are:

@anchor{t4-5}
@need 2107
@multitable @columnfractions 0.5 0.25 0.25
@headitem Inputs @tab Section 2 @tab DLMF 19.7.8
@item @math{0 < n < 1}, @math{0 < m < n} @tab 2.6 / 2.6 @tab 4.7 / 4.1
@item @math{0 < n < m < 1} @tab 2.7 / 4.0 @tab 12.3 / 5.2
@item @math{n} within @math{10^{-2}} to @math{10^{-10}} of @math{m}, relatively @tab 2.7 / 4.1 @tab 25.5 / 84.1
@item @math{n > 1} before the pole, @math{0 < m < 1} @tab 3.1 / 3.8 @tab 3.1 / 5.6
@end multitable

@noindent
@strong{Table 4.5.} Worst errors in ulps, double float / bigfloat, of
the formula of Section 2 and of the first change of parameter, in
regions where the formula of Section 2 is kept, with SBCL.

For
m4_math(<<<0 < n < 1>>>, <<<0 < n < 1>>>)
with
m4_math(<<<m > 0>>>, <<<m > 0>>>)
the two terms of the first formula have opposite signs, where the
formula of @ref{The Formula} adds two positive terms. For
m4_math(<<<n>>>, <<<n>>>)
near
m4_math(<<<m>>>, <<<m>>>)
the quotient
m4_math(<<<w>>>, <<<w>>>)
is rounded, and near
m4_math(<<<\pi/2>>>, <<<π/2>>>)
the quantity
m4_math(<<<q>>>, <<<q>>>)
is then small and inherits a large relative error.

@anchor{s4-6-4}
@subsection The Complete Integral

At
m4_math(<<<\phi = \pi/2>>>, <<<φ = π/2>>>)
the formula of @ref{s4-6-1,,Section 4.6.1} becomes

m4_displaymath(
<<<\Pi(n\thinspace |\thinspace m) = R_C\bigl(0,\thinspace (1-n)(1-w)\bigr) - \frac{w}{3}\thinspace R_J(0, 1-m, 1, 1-w) .>>>,
<<<Π(n | m) = R_C(0, (1-n)(1-w)) - w/3 R_J(0, 1-m, 1, 1-w) .>>>)

For
m4_math(<<<n > 1>>>, <<<n > 1>>>)
the principal value of the first term is 0, so the real part of the
complete integral is the single
m4_math(<<<R_J>>>, <<<R_J>>>)
term. In the formula of @ref{s4-6-2,,Section 4.6.2} the
m4_math(<<<R_C>>>, <<<R_C>>>)
term vanishes, since
m4_mathcomma(<<<c = 0>>>, <<<c = 0>>>)
and

m4_displaymath(
<<<\Pi(n\thinspace |\thinspace m) = \frac{1}{1-n}\left[\thinspace R_F(0, 1-m, 1) - \frac{n\thinspace v}{3}\thinspace R_J(0, 1-m, 1, v)\right] ,>>>,
<<<Π(n | m) = 1/(1-n) [ R_F(0, 1-m, 1) - (n v)/3 R_J(0, 1-m, 1, v)] ,>>>)

with two positive terms. @code{bf-elliptic-pi-complete} uses these for
real
m4_math(<<<n>>>, <<<n>>>)
and
m4_mathpunc(<<<:>>>, <<<m>>>, <<<m>>>)
the first for
m4_math(<<<n > 1>>>, <<<n > 1>>>)
with
m4_mathcomma(<<<m < 1>>>, <<<m < 1>>>)
and the second for
m4_mathdot(<<<n < -1>>>, <<<n < -1>>>)
That removes the loss in
m4_math(<<<2k\thinspace \Pi(n\thinspace |\thinspace m)>>>, <<<2k Π(n | m)>>>)
for large
m4_mathdot(<<<\lvert\phi\rvert>>>, <<<|φ|>>>)

@anchor{s4-6-5}
@subsection Accuracy

Worst errors in ulps of the real part, double float / bigfloat, with the
formula of @ref{The Formula} alone and with the changes of parameter, on
the same 40 points for each set. Past the pole,
m4_math(<<<\phi>>>, <<<φ>>>)
is uniform between the pole and
m4_mathpunc(<<<:>>>, <<<\pi/2>>>, <<<π/2>>>)

@anchor{t4-6}
@need 5363
@multitable @columnfractions 0.5 0.27 0.23
@headitem Inputs @tab Before @tab After
@item Past the pole, @math{1 < n < 10}, @math{0 < m < 1} @tab 151 / 140 @tab 10.8 / 18.2
@item Past the pole, @math{1 < n < 10}, @math{-5 < m < 0} @tab 20.8 / 9.7 @tab 2.7 / 5.8
@item Past the pole, @math{10 < n < 10^{4}}, @math{0 < m < 1} @tab @math{2.3\times10^{4}} / @math{2.1\times10^{4}} @tab 18.5 / 18.6
@item @math{1.1 < n < 8}, @math{0 < m < 0.9}, @math{2 < \lvert\phi\rvert < 50} @tab 88 / 218 @tab 15.8 / 22.8
@item The same, @math{10^{2} < \lvert\phi\rvert < 10^{4}} @tab 1990 / 783 @tab 3.6 / 9.6
@item The same, @math{10^{4} < \lvert\phi\rvert < 10^{8}} @tab 344 / 289 @tab 3.8 / 5.1
@item @math{-10 < n < 0}, @math{0 < m < 1}, @math{\lvert\phi\rvert \le \pi/2} @tab 5.7 / 7.9 @tab 2.7 / 2.7
@item @math{-10^{6} < n < -10}, @math{0 < m < 1}, @math{\lvert\phi\rvert \le \pi/2} @tab 1630 / 5470 @tab 2.6 / 5.2
@item @math{-10^{6} < n < -10}, @math{-5 < m < 0}, @math{\lvert\phi\rvert \le \pi/2} @tab 1180 / 2050 @tab 3.6 / 3.2
@item @math{-10^{6} < n < -10}, @math{1 < m < 10}, @math{\phi} before the branch point @tab 295 / 264 @tab 2.8 / 3.6
@item @math{-10^{6} < n < -10}, @math{0 < m < 1}, @math{2 < \lvert\phi\rvert < 50} @tab 4090 / 8630 @tab 6.1 / 7.2
@item The same, @math{10^{2} < \lvert\phi\rvert < 10^{6}} @tab 770 / 977 @tab 5.2 / 4.9
@item @math{-100 < n < -1}, @math{-100 < m < 0}, @math{\lvert\phi\rvert \le \pi/2} @tab 19.6 / 37.9 @tab 2.6 / 2.9
@end multitable

@noindent
@strong{Table 4.6.} Worst errors in ulps of the real part of
@code{elliptic_pi}, double float / bigfloat, before and after the
changes of parameter, with SBCL.

For
m4_math(<<<-10^{6} < n < -10^{3}>>>, <<<-10^6 < n < -10^3>>>)
with
m4_math(<<<m>>>, <<<m>>>)
between
m4_math(<<<n/2>>>, <<<n/2>>>)
and
m4_mathcomma(<<<100\thinspace n>>>, <<<100 n>>>)
the worst errors after the change are 3.5 ulps in double float and 6.0
in bigfloat. The imaginary parts are unchanged. Next to the pole both
the old and the new formula are accurate, because there the result is
dominated by the logarithm of
m4_mathcomma(<<<p>>>, <<<p>>>)
and the results differ by a few rounding units.

@anchor{s4-6-6}
@subsection The Zero of the Real Part

For
m4_math(<<<n > 1>>>, <<<n > 1>>>)
and
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
the real part is
m4_math(<<<+\infty>>>, <<<+∞>>>)
at the pole and negative at
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
so it has a zero in between. Near the zero the two terms of the formula
of @ref{s4-6-1,,Section 4.6.1} cancel, as they would in any other
formula, and the error in ulps grows by the ratio of the terms to the
result. On a sample of 40 points every error was within 8 times that
ratio. Only extra precision would remove it. For
m4_math(<<<m < 0>>>, <<<m < 0>>>)
both terms are positive and there is no zero.

@anchor{s4-6-7}
@subsection Speed

Past the pole the formula of @ref{s4-6-1,,Section 4.6.1} needs one
m4_math(<<<R_C>>>, <<<R_C>>>)
and one
m4_mathcomma(<<<R_J>>>, <<<R_J>>>)
where the old path needed two
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
one
m4_math(<<<R_C>>>, <<<R_C>>>)
and one
m4_mathdot(<<<R_J>>>, <<<R_J>>>)
Time per call of double-float @code{elliptic_pi} on SBCL, in
microseconds, as the best of three runs, each the best of 7 repeats over
300 random points, with the formula of @ref{The Formula} alone and with
the changes of parameter:

@anchor{t4-7}
@need 3076
@multitable @columnfractions 0.5 0.25 0.25
@headitem Inputs @tab Before @tab After
@item @math{0 < n < 1}, @math{0 < m < 1}, @math{\lvert\phi\rvert \le \pi/2} @tab 53 @tab 54
@item @math{n > 1} past the pole @tab 79 @tab 55
@item @math{-10^{4} < n < -1}, @math{0 < m < 1}, @math{\lvert\phi\rvert \le \pi/2} @tab 79 @tab 60
@item @math{-10^{4} < n < -1}, @math{-10^{4} < m < -1}, @math{\lvert\phi\rvert \le \pi/2} @tab 74 @tab 70
@item @math{1.1 < n < 8}, @math{2 < \lvert\phi\rvert < 50} @tab 149 @tab 105
@item @math{-10^{4} < n < -1}, @math{0 < m < 1}, @math{2 < \lvert\phi\rvert < 50} @tab 171 @tab 122
@end multitable

@noindent
@strong{Table 4.7.} Time per call in microseconds of double-float
@code{elliptic_pi} on SBCL, before and after the changes of parameter.

@node Separate Real and Imaginary Parts for m > 1
@section Separate Real and Imaginary Parts for @math{m > 1}

For real
m4_mathcomma(<<<n>>>, <<<n>>>)
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
when the reduced angle is past the branch point, the real part cancels
in the changes of parameter of @ref{The Changes of Parameter} as well as
in the formula of @ref{The Formula}. But past the branch point the
integrand is purely imaginary, so each part can be computed separately
from real integrals. @code{bf-elliptic-pi} and
@code{bf-elliptic-pi-complete} do so for real
m4_mathdot(<<<n \ne m>>>, <<<n ≠ m>>>)
The expressions below are for a positive reduced angle
m4_mathcomma(<<<r>>>, <<<r>>>)
and the result is odd in
m4_mathdot(<<<r>>>, <<<r>>>)

@anchor{s4-7-1}
@subsection The Real Part

The real part does not change past the branch point, except at a pole.
By the reciprocal-modulus transformation,
@url{https://dlmf.nist.gov/19.7.E4, DLMF 19.7.4}, its value at the
branch point is a complete integral with a modulus below 1:

m4_displaymath(
<<<\mathrm{Re}\thinspace \Pi(n;\phi\thinspace |\thinspace m) = \frac{1}{\sqrt m}\thinspace \Pi\!\left(\frac{n}{m}\thinspace \Big|\thinspace \frac{1}{m}\right) .>>>,
<<<Re Π(n;φ | m) = 1/sqrt(m) Π(n/m Big| 1/m ) .>>>)

For
m4_math(<<<n > 1>>>, <<<n > 1>>>)
and
m4_math(<<<\phi>>>, <<<φ>>>)
past the pole as well, the complete form of the formula of
@ref{s4-6-1,,Section 4.6.1} gives a single term,

m4_displaymath(
<<<\mathrm{Re}\thinspace \Pi(n;\phi\thinspace |\thinspace m) = -\frac{1}{3 n \sqrt m}\thinspace R_J\!\left(0,\thinspace 1 - \frac{1}{m},\thinspace 1,\thinspace 1 - \frac{1}{n}\right) ,>>>,
<<<Re Π(n;φ | m) = -1/(3 n sqrt(m)) R_J(0, 1 - 1/m, 1, 1 - 1/n ) ,>>>)

whether the pole is before the branch point, for
m4_mathcomma(<<<n > m>>>, <<<n > m>>>)
or past it, for
m4_mathdot(<<<n < m>>>, <<<n < m>>>)
In the second case the pole lies where the integrand is imaginary, and
passing it adds a real term, which this expression includes.
@code{bf-elliptic-pi-realpart-m>1} computes the real part.

@anchor{s4-7-2}
@subsection The Imaginary Part

The imaginary part is an integral of a real function. Let
m4_math(<<<\theta_b>>>, <<<θ_b>>>)
be the branch point, where
m4_mathdot(<<<m\sin^2\theta_b = 1>>>, <<<m sin^2 θ_b = 1>>>)
For
m4_math(<<<\theta>>>, <<<θ>>>)
between
m4_math(<<<\theta_b>>>, <<<θ_b>>>)
and
m4_math(<<<r>>>, <<<r>>>)
the square root in the integrand is
m4_mathcomma(<<<i\sqrt{m\sin^2\theta - 1}>>>, <<<isqrt(m sin^2 θ - 1)>>>)
so the integrand is
m4_math(<<<-i>>>, <<<-i>>>)
times a real function, and

m4_displaymath(
<<<\mathrm{Im}\thinspace \Pi(n;\phi\thinspace |\thinspace m) = -J - \sigma\thinspace [n > m], \qquad J = \int_{\theta_b}^{r}\frac{d\theta}{(1 - n\sin^2\theta)\sqrt{m\sin^2\theta - 1}} .>>>,
<<<Im Π(n;φ | m) = -J - σ [n > m], J = integral_θ_b^rdθ/((1 - n sin^2 θ)sqrt(m sin^2 θ - 1)) .>>>)

The term
m4_mathcomma(<<<-\sigma>>>, <<<-σ>>>)
which is present only for
m4_mathcomma(<<<n > m>>>, <<<n > m>>>)
is from the pole (@pxref{The Formula}), which is then before the branch
point. For
m4_math(<<<1 < n < m>>>, <<<1 < n < m>>>)
the pole is between
m4_math(<<<\theta_b>>>, <<<θ_b>>>)
and
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
m4_math(<<<J>>>, <<<J>>>)
is a principal value, and the pole contributes to the real part instead.

@strong{The substitution.} Define
m4_math(<<<u>>>, <<<u>>>)
by

m4_displaymath(
<<<m\sin^2\theta - 1 = (m-1)\sin^2 u .>>>,
<<<m sin^2 θ - 1 = (m-1) sin^2 u .>>>)

It maps
m4_math(<<<\theta = \theta_b>>>, <<<θ = θ_b>>>)
to
m4_mathcomma(<<<u = 0>>>, <<<u = 0>>>)
m4_math(<<<\theta = \pi/2>>>, <<<θ = π/2>>>)
to
m4_mathcomma(<<<u = \pi/2>>>, <<<u = π/2>>>)
and
m4_math(<<<\theta = r>>>, <<<θ = r>>>)
to
m4_mathcomma(<<<u = \chi>>>, <<<u = chi>>>)
where

m4_displaymath(
<<<\sin^2\chi = \frac{-y}{m-1}, \qquad \cos^2\chi = \frac{m\cos^2 r}{m-1} .>>>,
<<<sin^2 chi = (-y)/(m-1), cos^2 chi = (m cos^2 r)/(m-1) .>>>)

Solving for the sine and cosine of
m4_mathcomma(<<<\theta>>>, <<<θ>>>)

m4_displaymath(
<<<\sin^2\theta = \frac{1 + (m-1)\sin^2 u}{m}, \qquad \cos^2\theta = \frac{(m-1)\cos^2 u}{m} .>>>,
<<<sin^2 θ = (1 + (m-1) sin^2 u)/m, cos^2 θ = ((m-1) cos^2 u)/m .>>>)

Differentiating the first of these, and using the second for
m4_math(<<<\cos\theta>>>, <<<cos θ>>>)
and
m4_mathcomma(<<<\sqrt{m\sin^2\theta - 1} = \sqrt{m-1}\thinspace \sin u>>>, <<<sqrt(m sin^2 θ - 1) = sqrt(m-1) sin u>>>)
gives

m4_displaymath(
<<<\frac{d\theta}{\sqrt{m\sin^2\theta - 1}} = \frac{du}{\sqrt m\thinspace \sin\theta} = \frac{du}{\sqrt{1 - (1-m)\sin^2 u}} ,>>>,
<<<dθ/sqrt(m sin^2 θ - 1) = du/(sqrt(m) sin θ) = du/(sqrt(1 - (1-m) sin^2 u)) ,>>>)

and the other factor of the integrand is

m4_displaymath(
<<<1 - n\sin^2\theta = \frac{m-n}{m}\thinspace \bigl(1 - N\sin^2 u\bigr), \qquad N = \frac{n(m-1)}{m-n} .>>>,
<<<1 - n sin^2 θ = (m-n)/m (1 - N sin^2 u), N = (n(m-1))/(m-n) .>>>)

So
m4_math(<<<J>>>, <<<J>>>)
is an integral of the same kind, from 0, with the negative parameter
m4_mathcomma(<<<1 - m>>>, <<<1 - m>>>)
which has no branch point:

m4_displaymath(
<<<J = \frac{m}{m-n}\int_0^{\chi}\frac{du}{(1 - N\sin^2 u)\sqrt{1 - (1-m)\sin^2 u}} = \frac{m}{m-n}\thinspace \Pi(N;\chi\thinspace |\thinspace 1-m) .>>>,
<<<J = m/(m-n) integral_0^chidu/((1 - N sin^2 u)sqrt(1 - (1-m) sin^2 u)) = m/(m-n) Π(N;chi | 1-m) .>>>)

In terms of the Jacobian elliptic functions with modulus
m4_mathcomma(<<<\sqrt{1 - 1/m}>>>, <<<sqrt(1 - 1/m)>>>)
the substitution is
m4_math(<<<\sin\theta = \mathrm{dn}\thinspace v>>>, <<<sin θ = dn v>>>)
with
m4_mathcomma(<<<\cos u = \mathrm{sn}\thinspace v>>>, <<<cos u = sn v>>>)
the usual one for an integral between two branch points.

@strong{For}
m4_math(<<<n < m>>>, <<<n < m>>>)
the imaginary part is
m4_mathcomma(<<<-J>>>, <<<-J>>>)
with the integral computed by @code{bf-elliptic-pi} itself. Here
m4_math(<<<N>>>, <<<N>>>)
is less than 1 for
m4_mathcomma(<<<n < 1>>>, <<<n < 1>>>)
and greater than 1 for
m4_mathcomma(<<<1 < n < m>>>, <<<1 < n < m>>>)
where @code{bf-elliptic-pi} returns the principal value.

@strong{For}
m4_mathcomma(<<<n > m>>>, <<<n > m>>>)
m4_math(<<<N>>>, <<<N>>>)
is negative, and
m4_math(<<<J>>>, <<<J>>>)
and
m4_math(<<<\sigma>>>, <<<σ>>>)
are both large when
m4_math(<<<n>>>, <<<n>>>)
is near
m4_mathcomma(<<<m>>>, <<<m>>>)
so
m4_math(<<<-J - \sigma>>>, <<<-J - σ>>>)
cancels. Apply the formula of @ref{s4-6-1,,Section 4.6.1} to
m4_mathdot(<<<\Pi(N;\chi\thinspace |\thinspace 1-m)>>>, <<<Π(N;chi | 1-m)>>>)
Its second parameter is

m4_displaymath(
<<<W = \frac{1-m}{N} = 1 - \frac{m}{n} ,>>>,
<<<W = (1-m)/N = 1 - m/n ,>>>)

and with
m4_math(<<<S = \sin\chi>>>, <<<S = sin chi>>>)
and
m4_mathcomma(<<<C = \cos\chi>>>, <<<C = cos chi>>>)
and since
m4_mathcomma(<<<1 - (1-m)S^2 = m\sin^2 r>>>, <<<1 - (1-m)S^2 = m sin^2 r>>>)

m4_displaymath(
<<<\Pi(N;\chi\thinspace |\thinspace 1-m) = S\thinspace R_C\bigl(C^2\thinspace m\sin^2 r,\; (1 - N S^2)\thinspace q\bigr) - \frac{W}{3}\thinspace S^3 R_J(C^2,\; m\sin^2 r,\; 1,\; q) ,>>>,
<<<Π(N;chi | 1-m) = S R_C(C^2 m sin^2 r, (1 - N S^2) q) - W/3 S^3 R_J(C^2, m sin^2 r, 1, q) ,>>>)

where
m4_mathdot(<<<q = 1 - W S^2>>>, <<<q = 1 - W S^2>>>)
Multiplied by
m4_mathcomma(<<<m/(m-n)>>>, <<<m/(m-n)>>>)
the coefficient of the
m4_math(<<<R_J>>>, <<<R_J>>>)
term becomes
m4_mathdot(<<<m/(3n)>>>, <<<m/(3n)>>>)
In the
m4_math(<<<R_C>>>, <<<R_C>>>)
term both arguments are positive, and because
m4_math(<<<N W = 1 - m>>>, <<<N W = 1 - m>>>)
their difference is

m4_displaymath(
<<<(1 - N S^2)\thinspace q - C^2\thinspace m\sin^2 r = S^2 (1-N)(1-W) = S^2\thinspace \frac{m^2 (n-1)}{n (n-m)} .>>>,
<<<(1 - N S^2) q - C^2 m sin^2 r = S^2 (1-N)(1-W) = S^2 (m^2 (n-1))/(n (n-m)) .>>>)

For
m4_mathcomma(<<<0 < x < z>>>, <<<0 < x < z>>>)
m4_math(<<<R_C(x, z)>>>, <<<R_C(x, z)>>>)
is
m4_math(<<<\arctan\sqrt{(z-x)/x}>>>, <<<arctansqrt((z-x)/x)>>>)
divided by
m4_mathcomma(<<<\sqrt{z-x}>>>, <<<sqrt(z-x)>>>)
and the arctangent is
m4_mathdot(<<<\pi/2 - \arctan\sqrt{x/(z-x)}>>>, <<<π/2 - arctansqrt(x/(z-x))>>>)
Here
m4_math(<<<S/\sqrt{z-x}>>>, <<<S/sqrt(z-x)>>>)
does not depend on
m4_mathcomma(<<<\chi>>>, <<<chi>>>)
and

m4_displaymath(
<<<\frac{m}{m-n}\thinspace S\thinspace R_C = -\frac{2\sigma}{\pi}\left(\frac{\pi}{2} - \arctan t\right), \qquad t = \sin r\thinspace \cos r\thinspace \sqrt{\frac{n(n-m)}{(n-1)(-y)}} .>>>,
<<<m/(m-n) S R_C = -2σ/π (π/2 - arctan t), t = sin r cos r sqrt((n(n-m))/((n-1)(-y))) .>>>)

The constant part is exactly
m4_mathcomma(<<<-\sigma>>>, <<<-σ>>>)
so it cancels the residue term in
m4_mathcomma(<<<-J - \sigma>>>, <<<-J - σ>>>)
and

m4_displaymath(
<<<\mathrm{Im}\thinspace \Pi(n;\phi\thinspace |\thinspace m) = -\frac{m}{3n}\thinspace \sin^3\chi\; R_J(\cos^2\chi,\; m\sin^2 r,\; 1,\; q) - \frac{2\sigma}{\pi}\thinspace \arctan t ,>>>,
<<<Im Π(n;φ | m) = -m/3n sin^3 chi R_J(cos^2 chi, m sin^2 r, 1, q) - 2σ/π arctan t ,>>>)

with

m4_displaymath(
<<<q = \cos^2\chi + \frac{m}{n}\sin^2\chi, \qquad \frac{2\sigma}{\pi} = \sqrt{\frac{n}{(n-1)(n-m)}} ,>>>,
<<<q = cos^2 chi + m/n sin^2 chi, 2σ/π = sqrt(n/((n-1)(n-m))) ,>>>)

where both terms are negative. At the branch point this is
m4_mathcomma(<<<-\sigma>>>, <<<-σ>>>)
and at
m4_math(<<<\pi/2>>>, <<<π/2>>>)
it is
m4_mathdot(<<<-(m/3n)\thinspace R_J(0, m, 1, m/n)>>>, <<<-(m/3n) R_J(0, m, 1, m/n)>>>)
@code{bf-elliptic-pi-imagpart-m>1} computes the imaginary part.

@anchor{s4-7-3}
@subsection Accuracy and Speed

Worst errors in ulps, double float / bigfloat, with the formula of
@ref{The Formula} and the changes of parameter alone, and with the two
parts computed separately, on the same 40 points for each set. ``Past''
means that
m4_math(<<<\phi>>>, <<<φ>>>)
is uniform between the branch point and
m4_mathdot(<<<\pi/2>>>, <<<π/2>>>)
The references are from numerical integration along a path indented
around the pole and the branch point:

@anchor{t4-8}
@need 3994
@multitable @columnfractions 0.5 0.27 0.23
@headitem Inputs @tab Before @tab After
@item @math{2 < n < 10}, @math{1 < m < n}, past @tab 25.1 / 36.5 @tab 3.9 / 5.4
@item @math{10 < n < 10^{4}}, @math{1 < m < n}, past @tab @math{2.8\times10^{4}} / @math{3.7\times10^{4}} @tab 3.7 / 6.1
@item @math{2 < n < 10}, @math{1 < m < n}, @math{2 < \lvert\phi\rvert < 50} @tab 57.0 / 71.5 @tab 5.3 / 5.0
@item The same, @math{10^{2} < \lvert\phi\rvert < 10^{4}} @tab 74.0 / 66.1 @tab 5.3 / 5.1
@item @math{1 < n < 10}, @math{n < m}, past @tab 98.1 / 127 @tab 3.5 / 6.0
@item @math{100 < n < 10^{4}}, @math{n < m}, past @tab @math{7.2\times10^{4}} / @math{1.6\times10^{5}} @tab 3.9 / 7.3
@item @math{1 < n < 10}, @math{n < m}, @math{2 < \lvert\phi\rvert < 50} @tab 357 / 432 @tab 10.0 / 47.7
@item @math{0 < n < 1}, @math{1 < m < 30}, past @tab 4.3 / 5.4 @tab 3.6 / 3.9
@item @math{-10^{4} < n < -1}, @math{1 < m < 30}, past @tab 58.8 / 184 @tab 4.9 / 5.3
@item The same, @math{2 < \lvert\phi\rvert < 50} @tab 66.6 / 70.6 @tab 3.6 / 4.4
@end multitable

@noindent
@strong{Table 4.8.} Worst errors in ulps of the real part of
@code{elliptic_pi} for
m4_math(<<<m > 1>>>, <<<m > 1>>>)
past the branch point, double float / bigfloat, with SBCL.

@anchor{t4-9}
@need 3654
@multitable @columnfractions 0.5 0.27 0.23
@headitem Inputs @tab Before @tab After
@item @math{2 < n < 10}, @math{1 < m < n}, past @tab 8.4 / 17.9 @tab 2.9 / 4.4
@item @math{10 < n < 10^{4}}, @math{1 < m < n}, past @tab 11.6 / 14.7 @tab 1.8 / 4.2
@item @math{2 < n < 10}, @math{1 < m < n}, @math{2 < \lvert\phi\rvert < 50} @tab 39.6 / 20.6 @tab 5.7 / 19.4
@item The same, @math{10^{2} < \lvert\phi\rvert < 10^{4}} @tab 12.5 / 18.4 @tab 5.2 / 5.5
@item @math{1 < n < 10}, @math{n < m}, past @tab 5.2 / 7.1 @tab 8.6 / 13.1
@item @math{100 < n < 10^{4}}, @math{n < m}, past @tab 9.9 / 12.9 @tab 4.0 / 3.4
@item @math{1 < n < 10}, @math{n < m}, @math{2 < \lvert\phi\rvert < 50} @tab 6.5 / 10.7 @tab 6.2 / 7.5
@item @math{0 < n < 1}, @math{1 < m < 30}, past @tab 3.2 / 7.9 @tab 3.2 / 3.8
@item @math{-10^{4} < n < -1}, @math{1 < m < 30}, past @tab 2070 / 5650 @tab 5.7 / 7.0
@item The same, @math{2 < \lvert\phi\rvert < 50} @tab 2370 / 4520 @tab 3.8 / 4.9
@end multitable

@noindent
@strong{Table 4.9.} Worst errors in ulps of the imaginary part of
@code{elliptic_pi} for
m4_math(<<<m > 1>>>, <<<m > 1>>>)
past the branch point, double float / bigfloat, with SBCL.

The imaginary part for
m4_mathcomma(<<<1 < n < 10>>>, <<<1 < n < 10>>>)
m4_math(<<<n < m>>>, <<<n < m>>>)
is a few ulps worse than before on this sample. There the integral for
m4_math(<<<J>>>, <<<J>>>)
has a pole and is a principal value, with the accuracy of any
m4_math(<<<n > 1>>>, <<<n > 1>>>)
past the pole. The complex evaluation was also sensitive to the last
bits of the sine and cosine: for @code{elliptic_pi(1000.0, 1.25, 2.0)}
its imaginary part was off by 38 ulps with SBCL and by 108 with ECL and
GCL, and is now the correctly rounded value with all six Lisps tested.

Past the branch point the time per call of double-float
@code{elliptic_pi} on SBCL went from about 250 microseconds to about
115, since the complex Carlson functions are no longer evaluated.

@node Implementation
@chapter Implementation

All the changes are in @code{src/ellipt.lisp}.

@itemize @bullet
@item
@code{bf-rj-rc-arg} is new. It takes
m4_mathcomma(<<<e\thinspace d^2>>>, <<<e d^2>>>)
m4_math(<<<d>>>, <<<d>>>)
and the four square roots, which @code{bf-rj1} already has, and returns
m4_math(<<<1 + e>>>, <<<1 + e>>>)
as described in @ref{The Duplication Step of R_J}.
@item
@code{bf-rj1} calls it for the second argument of @code{bf-rc}.
@item
@code{bf-elliptic-pi} calls @code{bf-1-m*sin^2} for
m4_math(<<<y>>>, <<<y>>>)
and
m4_mathcomma(<<<p>>>, <<<p>>>)
and takes the sine and cosine of the unreduced
m4_math(<<<\phi>>>, <<<φ>>>)
(@pxref{Reducing the Angle}). It evaluates
@code{bf-elliptic-pi-complete} only when
m4_math(<<<k \ne 0>>>, <<<k ≠ 0>>>)
(@pxref{Skipping the Complete Integral}).
@item
@code{bf-elliptic-pi-residue} calls @code{bf-1-m*sin^2} for its two
tests, also with the unreduced
m4_mathdot(<<<\phi>>>, <<<φ>>>)
@item
@code{bf-elliptic-pi} and @code{bf-elliptic-pi-complete} each have two
more branches for the changes of parameter (@pxref{The Changes of
Parameter}). They use @code{bf-rf}, @code{bf-rc}, @code{bf-rj} and
@code{bf-1-m*sin^2}, which are unchanged.
@item
@code{bf-elliptic-period} is new. It returns the corrected
m4_mathcomma(<<<k>>>, <<<k>>>)
and replaces @code{round} in @code{bf-elliptic-f}, @code{bf-elliptic-e},
@code{bf-elliptic-e-terms}, @code{bf-elliptic-e-m>1},
@code{bf-elliptic-pi} and @code{bf-elliptic-pi-residue}
(@pxref{s4-4-2,,Section 4.4.2}).
@item
@code{elliptic-pi-undefined} is new. It signals the error for
m4_math(<<<m = 1>>>, <<<m = 1>>>)
or
m4_mathcomma(<<<n = 1>>>, <<<n = 1>>>)
and is called by @code{bf-elliptic-pi} and by the @code{elliptic_pi}
simplifier.
@end itemize

@code{bf-1-m*sin^2} itself is unchanged. In
@code{bf-elliptic-pi-complete},
m4_math(<<<1 - m>>>, <<<1 - m>>>)
and
m4_math(<<<1 - n>>>, <<<1 - n>>>)
are computed exactly when they are small, so its existing branch is
unchanged.

@strong{Removing @code{elliptic-pi}.} The double-float functions
@code{elliptic-pi} and @code{elliptic-pi-complete} duplicated
@code{bf-elliptic-pi} and @code{bf-elliptic-pi-complete}, which, like
the other functions in the @code{bigfloat} package, work on double
floats. A second change removes them. The @code{elliptic_pi} simplifier
calls @code{bf-elliptic-pi} for real and complex double-float arguments
as well. Nothing else called them.

The double-float results before and after that second change, on 60
random points per set, with the worst errors in ulps:

@anchor{t5-1}
@need 3246
@multitable @columnfractions 0.46 0.18 0.18 0.18
@headitem Inputs @tab Identical @tab Before @tab After
@item @math{0 < n < 1}, @math{0 < m < 1}, @math{\lvert\phi\rvert \le \pi/2} @tab 60 @tab 2.3 @tab 2.3
@item @math{-10 < n < 0}, @math{0 < m < 1}, @math{\lvert\phi\rvert \le \pi/2} @tab 56 @tab 6.3 @tab 6.3
@item @math{-5 < n < 1}, @math{-100 < m < 0}, @math{\lvert\phi\rvert \le \pi/2} @tab 59 @tab 4.2 @tab 4.2
@item @math{-5 < n < 1}, @math{1 < m < 100}, @math{\lvert\phi\rvert \le \pi/2} @tab 54 @tab 20.6 @tab 20.6
@item @math{-5 < n < 1}, @math{0 < m < 1}, @math{\lvert\phi\rvert < 50} @tab 60 @tab 9.3 @tab 9.3
@item @math{n} near 1, @math{\phi} near @math{\pi/2} @tab 59 @tab 5.0 @tab 5.0
@end multitable

@noindent
@strong{Table 5.1.} Double-float results with @code{elliptic-pi} and
with @code{bf-elliptic-pi}: the number of points, of 60, with identical
results, and the worst errors in ulps, with SBCL.

The results that differ do so by about an ulp. For complex arguments, 7
of 60 results are identical, and the worst errors of a part are in the
thousands of ulps with either function (@pxref{Known Limitations}).

@node Accuracy
@chapter Accuracy

Errors below are the worst errors in ulps, as double float / bigfloat.
Each set has 50 random points. References are from mpmath at 250 bits,
at the exact inputs. ``Before'' is master without any of the changes,
and ``After'' is the branch with all of them, on the same points.

The sets where the change matters:

@anchor{t6-1}
@need 6383
@multitable @columnfractions 0.44 0.1 0.25 0.21
@headitem Inputs @tab Part @tab Before @tab After
@item @math{n = 1 - 10^{-k}}, @math{3 \le k \le 15}, @math{\phi = \pi/2 - 10^{-j}}, @math{1 \le j \le 15}, @math{0 < m < 0.9} @tab Re @tab @math{1.2\times10^{13}} / @math{8.3\times10^{12}} @tab 3.9 / 5.4
@item @math{-3 < n < 0.9}, @math{\phi = \pi/2 - 10^{-j}}, @math{m = 1 - 10^{-k}} @tab Re @tab @math{5.6\times10^{11}} / @math{3.9\times10^{12}} @tab 4.4 / 3.4
@item @math{1.05 < n < 10}, @math{\phi} within @math{10^{-2}} to @math{10^{-14}} of the pole, relatively, on both sides, @math{0 < m < 0.95} @tab Re @tab @math{1.1\times10^{12}} / @math{2.6\times10^{12}} @tab 5.4 / 4.9
@item The same @tab Im @tab 1.1 / 1.0 @tab 1.1 / 1.0
@item @math{2 < n < 10}, @math{\phi} near the pole as above, @math{1 < m < n - 0.5} @tab Re @tab @math{2.0\times10^{12}} / @math{1.5\times10^{12}} @tab 4.4 / 8.6
@item The same @tab Im @tab 1.9 / 1.6 @tab 1.9 / 1.6
@item @math{n = 1 + 10^{-k}}, @math{2 \le k \le 14}, @math{0.2 < \phi < \pi/2}, @math{0 < m < 0.9} @tab Re @tab 125 / 17.4 @tab 3.6 / 6.8
@item @math{-5 < n < 1}, @math{1 < m < 100}, @math{\phi} within @math{10^{-2}} to @math{10^{-14}} of the branch point, on both sides @tab Re @tab @math{4.6\times10^{5}} / @math{8.5\times10^{5}} @tab 3.9 / 3.6
@item The same @tab Im @tab @math{8.8\times10^{12}} / @math{2.4\times10^{13}} @tab 4.0 / 5.0
@item Complex @math{n} within @math{10^{-3}} to @math{10^{-14}} of 1, @math{\phi} near @math{\pi/2}, @math{0 < m < 0.9} @tab Re @tab @math{6.8\times10^{11}} / @math{6.4\times10^{11}} @tab 4.4 / 6.9
@item The same @tab Im @tab @math{1.7\times10^{12}} / @math{2.8\times10^{12}} @tab 4.2 / 6.4
@item Complex @math{m} within @math{10^{-3}} to @math{10^{-14}} of 1, @math{\phi} near @math{\pi/2}, @math{-2 < n < 0.9} @tab Re @tab @math{3.3\times10^{10}} / @math{1.4\times10^{10}} @tab 7.5 / 8.1
@item The same @tab Im @tab @math{1.8\times10^{12}} / @math{3.7\times10^{11}} @tab 19.8 / 21.6
@end multitable

@noindent
@strong{Table 6.1.} Worst errors in ulps, double float / bigfloat, on
the sets where the change matters, before and after, with SBCL.

The other sets, with
m4_math(<<<\lvert\phi\rvert \le \pi/2>>>, <<<|φ| <= π/2>>>)
unless stated:

@anchor{t6-2}
@need 3824
@multitable @columnfractions 0.44 0.1 0.23 0.23
@headitem Inputs @tab Part @tab Before @tab After
@item @math{0 < n < 1}, @math{0 < m < 1} @tab Re @tab 7.5 / 10.6 @tab 3.5 / 4.1
@item @math{-10 < n < 0}, @math{0 < m < 1} @tab Re @tab 5.6 / 8.0 @tab 3.2 / 3.2
@item @math{-5 < n < 1}, @math{-100 < m < 0} @tab Re @tab 4.3 / 6.1 @tab 2.6 / 6.1
@item @math{1 < n < 10}, @math{0 < m < 1} @tab Re @tab 269 / 147 @tab 13.6 / 20.9
@item The same @tab Im @tab 1.4 / 1.6 @tab 1.4 / 1.6
@item @math{-5 < n < 1}, @math{1 < m < 100} @tab Re @tab 4.3 / 5.4 @tab 2.6 / 3.7
@item The same @tab Im @tab 5.1 / 5.4 @tab 3.9 / 3.8
@item @math{-5 < n < 1}, @math{0 < m < 1}, @math{\lvert\phi\rvert < 50} @tab Re @tab 10.5 / 15.6 @tab 4.5 / 5.7
@item Complex @math{n}, @math{\phi} and @math{m}, with @math{-5 < \mathrm{Re}\thinspace n < 0.9}, @math{\lvert\mathrm{Re}\thinspace \phi\rvert < 1.5}, @math{-3 < \mathrm{Re}\thinspace m < 0.9} @tab Re @tab 6.0 / 33.1 @tab 18.0 / 29.1
@item The same @tab Im @tab 37.7 / 39.6 @tab 90.3 / 39.6
@end multitable

@noindent
@strong{Table 6.2.} Worst errors in ulps, double float / bigfloat, on
sets away from the cancellations, before and after, with SBCL.

The sum form of
m4_math(<<<y>>>, <<<y>>>)
and
m4_math(<<<p>>>, <<<p>>>)
makes
m4_mathcomma(<<<0 < n < 1>>>, <<<0 < n < 1>>>)
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
slightly more accurate. The rows with
m4_mathcomma(<<<n > 1>>>, <<<n > 1>>>)
with
m4_math(<<<-10 < n < 0>>>, <<<-10 < n < 0>>>)
and with
m4_math(<<<\lvert\phi\rvert < 50>>>, <<<|φ| < 50>>>)
improve because of the changes of parameter (@pxref{The Changes of
Parameter}). The other differences are within the scatter. The errors
for complex arguments are cancellations in a part that is much smaller
than the other (@pxref{Known Limitations}).

For @code{carlson_rj} with real arguments:

@anchor{t6-3}
@need 3416
@multitable @columnfractions 0.4 0.1 0.3 0.2
@headitem Inputs @tab Points @tab Before @tab After
@item @math{0.01 < x, y, z < 2}, @math{p = 10^{-k}}, @math{2 \le k \le 15} @tab 60 @tab @math{6.2\times10^{5}} / @math{3.7\times10^{5}} @tab 4.9 / 6.2
@item @math{x = 0}, @math{0.01 < y, z < 2}, @math{p = 10^{-k}} @tab 40 @tab 4.7 / 7.1 @tab 4.7 / 7.1
@item @math{0.01 < x, y, z < 2}, @math{0.01 < p < 5} @tab 60 @tab 5.0 / 7.2 @tab 5.0 / 7.2
@item @math{0 < x < 1}, @math{10^{3} < y < 10^{12}}, @math{z = 1}, @math{1 < p < y} @tab 60 @tab 58.5 / 12.3 @tab 6.3 / 8.9
@item @math{x}, @math{y}, @math{z} and @math{p} between @math{10^{-8}} and @math{10^{8}} @tab 60 @tab @math{7.7\times10^{4}} / @math{1.3\times10^{4}} @tab 6.0 / 6.5
@item @math{x = 0}, @math{y}, @math{z} and @math{p} between @math{10^{-6}} and @math{10^{6}} @tab 60 @tab 5.5 / 5.2 @tab 5.5 / 6.8
@end multitable

@noindent
@strong{Table 6.3.} Worst errors in ulps of @code{carlson_rj}, double
float / bigfloat, before and after, with SBCL.

The examples of @ref{Examples} after the change:

@anchor{t6-4}
@need 3476
@multitable @columnfractions 0.5 0.32 0.18
@headitem Expression @tab After @tab Error (ulps)
@item @code{elliptic_pi(float(1-2^-40), float(%pi/2)-2^-20, 0.5)} @tab @math{1164674.2576808748} @tab 0.24
@item @code{elliptic_pi(0.5, float(%pi/2)-2^-20, float(1-2^-40))} @tab @math{27.489278290246297} @tab 1.77
@item @code{elliptic_pi(4.0, float(%pi/6)-2^-45, 0.5)} @tab @math{9.5435107790762892} @tab 1.33
@item @code{realpart(elliptic_pi(4.0, float(%pi/6)+2^-30, 0.5))} @tab @math{6.3354856645734658} @tab 1.44
@item @code{imagpart(elliptic_pi(0.5, float(%pi/4)+2^-30, 2.0))} @tab @math{-5.7544496232928477\times10^{-5}} @tab 0.95
@item @code{elliptic_pi(4.0, float(%pi/6), 0.5)} @tab @math{11.458898371615305} @tab 0.45
@item @code{carlson_rj(0.5, 1.0, 2.0, float(2^-40))} @tab @math{40.526533925441989} @tab 2.41
@end multitable

@noindent
@strong{Table 6.4.} The examples of Table 3.2 after the change, with
SBCL.

@node Speed
@chapter Speed

Time per call of double-float @code{elliptic_pi} on SBCL, in
microseconds, for
m4_mathdot(<<<\lvert\phi\rvert < \pi/2>>>, <<<|φ| < π/2>>>)
Each time is the best of three runs, each the best of 7 repeats over 300
random points. ``Before'' is master without any of the changes, and
``After'' is the branch with all of them:

@anchor{t7-1}
@need 2855
@multitable @columnfractions 0.46 0.18 0.18 0.18
@headitem Inputs @tab Before @tab After @tab Change
@item @math{0 < n < 1}, @math{0 < m < 1} @tab 116 @tab 54 @tab @math{-53}%
@item @math{-10 < n < 0}, @math{0 < m < 1} @tab 139 @tab 60 @tab @math{-57}%
@item @math{1 < n < 10}, @math{0 < m < 1} @tab 148 @tab 53 @tab @math{-64}%
@item @math{n > 1}, @math{\phi} near the pole @tab 144 @tab 93 @tab @math{-35}%
@item @math{n} near 1, @math{\phi} near @math{\pi/2} @tab 116 @tab 56 @tab @math{-52}%
@item @math{m} near 1, @math{\phi} near @math{\pi/2} @tab 175 @tab 76 @tab @math{-57}%
@item @math{-5 < n < 1}, @math{1 < m < 100} @tab 347 @tab 111 @tab @math{-68}%
@item Complex @math{n}, @math{\phi} and @math{m} @tab 372 @tab 184 @tab @math{-51}%
@end multitable

@noindent
@strong{Table 7.1.} Time per call in microseconds of double-float
@code{elliptic_pi} on SBCL, before and after all the changes.

Most of the gain comes from @ref{Skipping the Complete Integral}: the
complete integral, which cost about as much as the incomplete one, is no
longer evaluated when
m4_mathdot(<<<k = 0>>>, <<<k = 0>>>)
For
m4_math(<<<k \ne 0>>>, <<<k ≠ 0>>>)
it is still needed, and for
m4_math(<<<0 < n < 1>>>, <<<0 < n < 1>>>)
the time is what it was within the variation. The rest of the gain for
m4_mathcomma(<<<n > 1>>>, <<<n > 1>>>)
m4_math(<<<n < -1>>>, <<<n < -1>>>)
and
m4_math(<<<m > 1>>>, <<<m > 1>>>)
comes from the changes of @ref{The Changes of Parameter} and
@ref{Separate Real and Imaginary Parts for m > 1}.

The other changes cost little. Replacing @code{elliptic-pi} by
@code{bf-elliptic-pi} (@pxref{Implementation}) did not change the speed
measurably, since both called the same @code{bf-rf}, @code{bf-rj} and
@code{bf-1-m*sin^2}. The extra-precision path of @code{bf-1-m*sin^2} is
the only added work. Near the pole it runs twice, once for the value and
once for the pole count, which is why the gain is smallest there.

The run-to-run variation of these timings was about 20 percent, and the
load on the machine differed between measurements, so only the
comparison within a table is meaningful.

@node Tests
@chapter Tests

New tests are added at the end of @code{tests/rtest_elliptic.mac}, in
double float and in bigfloat at @code{fpprec : 32}. They cover the
problems of @ref{The Problems}, including the floating-point numbers
next to the pole, @code{carlson_rj} with small
m4_math(<<<p>>>, <<<p>>>)
and with widely separated arguments, and the values and the errors for
m4_math(<<<m = 1>>>, <<<m = 1>>>)
and
m4_mathdot(<<<n = 1>>>, <<<n = 1>>>)
Each group fails without the corresponding change.

The inputs are dyadic rationals, or @code{float} or @code{bfloat} of
exact expressions plus a dyadic rational, so the references, from mpmath
at 600 bits or more, are for the exact inputs. For real
m4_math(<<<n > 1>>>, <<<n > 1>>>)
the reference uses the change of parameter, which has no pole on the
path, with
m4_math(<<<-i\sigma>>>, <<<-iσ>>>)
added past the pole. It agrees with mpmath's @code{ellippi} at
m4_math(<<<n - i\thinspace 2^{-700}>>>, <<<n - i 2^-700>>>)
where both were computed. The real and imaginary parts are checked
separately, by relative error.

With all the changes, the tests in @code{rtest_elliptic.mac} pass on
SBCL, CMUCL, CCL and ECL, and on GCL 2.6.14.

@strong{CLISP.} Six of the new tests are expected failures on CLISP:

@anchor{t8-1}
@need 2396
@multitable @columnfractions 0.14 0.86
@headitem Problem @tab Expression
@item 546 @tab @code{elliptic_pi(float(1-2^-40), float(%pi/2)-2^-20, 0.5)}
@item 547 @tab @code{elliptic_pi(float(1-2^-50), float(%pi/2)-2^-26, 0.25)}
@item 548 @tab @code{elliptic_pi(0.5, float(%pi/2)-2^-20, float(1-2^-40))}
@item 549 @tab @code{elliptic_pi(-2.0, float(%pi/2)-2^-26, float(1-2^-50))}
@item 555 @tab @code{elliptic_pi(float(1-2^-30)+2^-31*%i, float(%pi/2)-2^-15, 0.5)}
@item 556 @tab @code{elliptic_pi(0.5, float(%pi/2)-2^-15, float(1-2^-30)+2^-31*%i)}
@end multitable

@noindent
@strong{Table 8.1.} The tests that are expected failures on CLISP.

They have
m4_math(<<<\phi>>>, <<<φ>>>)
near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
where CLISP's double-float @code{cos} has an absolute error of about
m4_mathcomma(<<<5\times10^{-20}>>>, <<<5×10^-20>>>)
which is a relative error of up to
m4_math(<<<2\times10^{-12}>>>, <<<2×10^-12>>>)
at these angles, and with
m4_math(<<<n>>>, <<<n>>>)
or
m4_math(<<<m>>>, <<<m>>>)
near 1 the result depends directly on
m4_mathdot(<<<\cos^2\phi>>>, <<<cos^2 φ>>>)
With CLISP's values of
m4_math(<<<\sin\phi>>>, <<<sin φ>>>)
and
m4_mathcomma(<<<\cos\phi>>>, <<<cos φ>>>)
the SBCL build gives CLISP's results exactly.

@strong{GCL.} With GCL 2.7, the six double-float tests of the corrected
m4_math(<<<k>>>, <<<k>>>)
(@pxref{s4-4-2,,Section 4.4.2}), problems 653 to 658, are expected
failures. Its @code{round} with a floating-point divisor returns the
wrong remainder: @code{(round 7d0 2d0)} gives a remainder of
m4_mathcomma(<<<-0.5>>>, <<<-0.5>>>)
where it should be
m4_mathdot(<<<-1>>>, <<<-1>>>)
So @code{bf-elliptic-period} does not see that the reduced angle is near
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
and
m4_math(<<<k>>>, <<<k>>>)
is not corrected. GCL 2.6.14 returns the right remainder.

@node Known Limitations
@chapter Known Limitations

These are not addressed by this change.

@itemize @bullet
@item
@strong{The zero of the real part for}
m4_mathdot(<<<n > 1>>>, <<<n > 1>>>)
For
m4_math(<<<n > 1>>>, <<<n > 1>>>)
and
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
the real part has a zero between the pole and
m4_mathcomma(<<<\pi/2>>>, <<<π/2>>>)
and near it the error in ulps is large in any formula (@pxref{The
Changes of Parameter}). On random points the worst errors are 10 to 20
ulps.
@item
@strong{Each part for complex arguments.} For general complex arguments
the two terms of the formula of @ref{The Formula} are complex, and one
part of their sum can cancel while the other does not. That part is then
much smaller than the other and loses accuracy. For example, for
@code{elliptic_pi(-4.0-1.375*%i, 0.6875+0.4375*%i, -0.25-2.0*%i)} the
two terms are
m4_math(<<<0.84582 + 0.29413\thinspace i>>>, <<<0.84582 + 0.29413 i>>>)
and
m4_mathcomma(<<<-0.26830 - 0.29940\thinspace i>>>, <<<-0.26830 - 0.29940 i>>>)
and the exact value is
m4_mathdot(<<<0.57752580739318120 - 0.0052651806455490598\thinspace i>>>, <<<0.57752580739318120 - 0.0052651806455490598 i>>>)
The real part is off by 2.6 ulps, and the imaginary part, where the
terms cancel to a fiftieth of their size, by 40 ulps in double float and
325 in bigfloat. On random arguments the worst errors of a part are tens
of ulps (@pxref{t6-2,,Table 6.2}).
@item
@strong{@code{carlson_rj} is wrong for some complex arguments.}
Carlson's duplication algorithm for
m4_math(<<<R_J>>>, <<<R_J>>>)
is proved only under conditions on the arguments, such as
m4_mathcomma(<<<x>>>, <<<x>>>)
m4_mathcomma(<<<y>>>, <<<y>>>)
m4_math(<<<z>>>, <<<z>>>)
with nonnegative real part and
m4_mathdot(<<<\mathrm{Re}\thinspace p > 0>>>, <<<Re p > 0>>>)
Outside them it can converge to a different branch, and @code{bf-rj1}
then returns a value that differs from the integral in every digit, with
no warning. For example, at
m4_mathcomma(<<<x = 0.12578216482080001 - 0.1383689394093604\thinspace i>>>, <<<x = 0.12578216482080001 - 0.1383689394093604 i>>>)
m4_mathcomma(<<<y = 0.049731107268686634 + 0.0065747493803571264\thinspace i>>>, <<<y = 0.049731107268686634 + 0.0065747493803571264 i>>>)
m4_math(<<<z = 1>>>, <<<z = 1>>>)
and
m4_mathcomma(<<<p = -2.385580234604479 + 0.13175631851084038\thinspace i>>>, <<<p = -2.385580234604479 + 0.13175631851084038 i>>>)
Maxima returns
m4_mathcomma(<<<-2.18169 + 2.68581\thinspace i>>>, <<<-2.18169 + 2.68581 i>>>)
and direct quadrature of the integral and mpmath's @code{elliprj} both
give
m4_mathdot(<<<-1.77509 - 1.42034\thinspace i>>>, <<<-1.77509 - 1.42034 i>>>)
@code{elliptic_pi} with complex arguments can reach such values of
m4_mathpunc(<<<:>>>, <<<p>>>, <<<p>>>)
on 1500 random complex
m4_mathcomma(<<<n>>>, <<<n>>>)
m4_math(<<<\phi>>>, <<<φ>>>)
and
m4_mathcomma(<<<m>>>, <<<m>>>)
20 results were wrong. None was wrong when the condition above held, but
it held for only about half of the points, and most results outside it
are correct, including those for real
m4_math(<<<m > 1>>>, <<<m > 1>>>)
past the branch point. In every wrong case examined, the fourth variable
crossed the negative real axis between two duplication steps. mpmath and
Arb avoid the problem by integrating numerically until the arguments
satisfy the condition. Nothing is done about it here, so complex results
of @code{carlson_rj} and @code{elliptic_pi} should be checked
independently.
@end itemize

@node References
@chapter References

@enumerate
@item
NIST Digital Library of Mathematical Functions:
@url{https://dlmf.nist.gov/19.25.E14, DLMF 19.25.14} and
@url{https://dlmf.nist.gov/19.25.E2, DLMF 19.25.2} for
m4_math(<<<\Pi>>>, <<<Π>>>)
in terms of
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_mathpunc(<<<;>>>, <<<R_J>>>, <<<R_J>>>)
@url{https://dlmf.nist.gov/19.20.E14, DLMF 19.20.14} for
m4_math(<<<R_J>>>, <<<R_J>>>)
with
m4_mathpunc(<<<;>>>, <<<p < 0>>>, <<<p < 0>>>)
@url{https://dlmf.nist.gov/19.36.i, DLMF §19.36(i)} for the duplication
method; @url{https://dlmf.nist.gov/19.16.i, DLMF §19.16(i)} for the
definitions of
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
m4_math(<<<R_C>>>, <<<R_C>>>)
and
m4_mathpunc(<<<;>>>, <<<R_J>>>, <<<R_J>>>)
@url{https://dlmf.nist.gov/19.7.iii, DLMF §19.7(iii)}, with
@url{https://dlmf.nist.gov/19.7.E8, DLMF 19.7.8} and
@url{https://dlmf.nist.gov/19.7.E9, DLMF 19.7.9}, for the changes of
parameter; @url{https://dlmf.nist.gov/19.7.E4, DLMF 19.7.4} for the
reciprocal-modulus transformation.
@item
B. C. Carlson, ``Numerical computation of real or complex elliptic
integrals,'' @emph{Numerical Algorithms} 10 (1995), 13--26,
@url{https://doi.org/10.1007/BF02198293, doi:10.1007/BF02198293};
preprint @url{https://arxiv.org/abs/math/9409227, arXiv:math/9409227}.
The duplication algorithm for
m4_mathcomma(<<<R_J>>>, <<<R_J>>>)
with the
m4_math(<<<R_C(1, 1 + e)>>>, <<<R_C(1, 1 + e)>>>)
term.
@item
F. Johansson et al., @url{https://mpmath.org/, mpmath}, a Python library
for arbitrary-precision arithmetic. Its @code{ellippi}, @code{ellipf},
@code{elliprf} and @code{elliprj} were used for the reference values.
@end enumerate

@bye
