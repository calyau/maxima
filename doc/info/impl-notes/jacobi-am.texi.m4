\input texinfo
@c -*- mode: texinfo -*-
@c Generated from jacobi-am.md.  Process with
@c   m4 --prefix-builtins math.m4 jacobi-am.texi.m4 > jacobi-am.texi
@setfilename jacobi-am.info
@settitle Computing the Jacobi Amplitude am(z|m) with Sala's Series
@documentencoding UTF-8

@c Definitions so that plain TeX accepts the same formulas as MathJax.
@tex
\gdef\lvert{\vert}
\gdef\rvert{\vert}
\gdef\mathrm#1{{\rm #1}}
\gdef\text#1{\hbox{#1}}
\gdef\frac#1#2{{#1\over #2}}
\gdef\mathbb#1{{\bf #1}}
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
@top Computing the Jacobi Amplitude am(z|m) with Sala's Series
@end ifnottex

@iftex
@titlepage
@title Computing the Jacobi Amplitude am(z|m) with Sala's Series
@end titlepage
@end iftex

Reference: K.@: L.@: Sala, @inlinefmtifelse{tex,
@uref{https://kensala.com/publications/Sala_TransformationsOfTheJacobianAmplitudeFunctionAndItsCalculationViaTheArithmeticGeometricMean_SiamJMathsAnalysis_20_1989.pdf,
, "Transformations of the Jacobian Amplitude Function and Its
Calculation via the Arithmetic-Geometric Mean@comma{}"},
@uref{https://kensala.com/publications/Sala_TransformationsOfTheJacobianAmplitudeFunctionAndItsCalculationViaTheArithmeticGeometricMean_SiamJMathsAnalysis_20_1989.pdf,
"Transformations of the Jacobian Amplitude Function and Its Calculation
via the Arithmetic-Geometric Mean@comma{}"}} @emph{SIAM J.@: Math.@:
Anal.@:} 20(6), 1989, pp.@: 1514–1528.

@code{bf-jacobi-am} computes
m4_math(<<<\mathrm{am}(z|m)>>>, <<<am(z|m)>>>)
from Sala's series of Gudermannian functions (@inlinefmtifelse{tex,
@ref{The Difference gd(A + w) − gd(A − w), section 5, The Difference
gd(A + w) − gd(A − w)}, @ref{The Difference gd(A + w) − gd(A − w),
section 5}}), which converges for all complex
m4_math(<<<z>>>, <<<z>>>)
and
m4_math(<<<m>>>, <<<m>>>)
and follows Mathematica's branch cuts for am, and for small
m4_math(<<<m>>>, <<<m>>>)
from the Fourier series in the nome. @inlinefmtifelse{tex, @ref{The
Algorithm in bf-jacobi-am, Section 6, The Algorithm in bf-jacobi-am},
@ref{The Algorithm in bf-jacobi-am, Section 6}} describes the whole
algorithm. This note derives the two formulas Sala's series uses:

@enumerate
@item
m4_math(<<<\mathrm{gd}(x + iy) = \mathrm{atan2}(\sinh x, \cos y) + i\ \tanh^{-1}(\sin y\ \mathrm{sech}\  x)>>>, <<<gd(x + iy) = atan2(sinh x, cos y) + i tanh⁻¹(sin y sech x)>>>)
@item
m4_math(<<<\mathrm{gd}(A + w) - \mathrm{gd}(A - w) = 2\tan^{-1}\left(\frac{\sinh w}{\cosh A}\right)>>>, <<<gd(A + w) − gd(A − w) = 2 tan⁻¹((sinh w)/(cosh A))>>>)
@end enumerate

and states where each one, and the related identities
m4_math(<<<\mathrm{gd}(u) = 2\tan^{-1}(\tanh(u/2))>>>, <<<gd(u) = 2 tan⁻¹(tanh(u/2))>>>)
and
m4_mathcomma(<<<\mathrm{gd}(u) = \tan^{-1}(\sinh u)>>>, <<<gd(u) = tan⁻¹(sinh u)>>>)
is valid. @inlinefmtifelse{tex, @ref{The Real and Imaginary Parts of gd,
Section 4, The Real and Imaginary Parts of gd}, @ref{The Real and
Imaginary Parts of gd, Section 4}} also gives a more accurate imaginary
part as a possible improvement, and @inlinefmtifelse{tex, @ref{Accuracy
- Expected Errors and Possible Improvements, section 7, Accuracy:
Expected Errors and Possible Improvements}, @ref{Accuracy - Expected
Errors and Possible Improvements, section 7}} gives the error to expect
in tests and other possible improvements.

Throughout,
m4_mathcomma(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)
m4_mathcomma(<<<\tanh^{-1}>>>, <<<tanh⁻¹>>>)
m4_math(<<<\sinh^{-1}>>>, <<<sinh⁻¹>>>)
and
m4_math(<<<\log>>>, <<<log>>>)
denote principal values, and
m4_math(<<<\mathrm{atan2}(s, c)>>>, <<<atan2(s, c)>>>)
is the two-argument arctangent with values in
m4_mathdot(<<<(-\pi, \pi]>>>, <<<(−π, π]>>>)

@menu
* Definition and Branch Cuts::
* gd(u) = 2 tan^-1(tanh(u/2)) for All Complex u off the Cuts::
* gd(u) = tan^-1(sinh u) Only in the Strip |Im u| < π/2::
* The Real and Imaginary Parts of gd::
* The Difference gd(A + w) − gd(A − w)::
* The Algorithm in bf-jacobi-am::
* Accuracy - Expected Errors and Possible Improvements::
* Numerical Checks::
@end menu

@node Definition and Branch Cuts
@chapter Definition and Branch Cuts

Sala defines
m4_math(<<<\mathrm{gd}(z)>>>, <<<gd(z)>>>)
as the integral of
m4_math(<<<\mathrm{sech}>>>, <<<sech>>>)
from
m4_math(<<<0>>>, <<<0>>>)
to
m4_mathdot(<<<z>>>, <<<z>>>)
The integrand has poles at
m4_mathcomma(<<<z = (2t + 1)\ i\pi/2>>>, <<<z = (2t + 1) iπ/2>>>)
so gd is made single-valued by cutting the plane along the segments of
the imaginary axis

m4_displaymath(
<<<\text{from } (4t + 1)\frac{i\pi}{2} \text{ to } (4t + 3)\frac{i\pi}{2}, \qquad t \in \mathbb{Z}.>>>,
<<<from (4t + 1)iπ/2 to (4t + 3)iπ/2, t ∈ ℤ.>>>)

Off these cuts gd is analytic, with
m4_math(<<<\mathrm{gd}'(z) = \mathrm{sech}\  z>>>, <<<gd'(z) = sech z>>>)
and
m4_mathdot(<<<\mathrm{gd}(0) = 0>>>, <<<gd(0) = 0>>>)
Write
m4_math(<<<z = x + iy>>>, <<<z = x + iy>>>)
with
m4_math(<<<x, y>>>, <<<x, y>>>)
real.

@node gd(u) = 2 tan^-1(tanh(u/2)) for All Complex u off the Cuts
@ifinfo
@chapter gd(u) = 2 tan⁻¹(tanh(u/2)) for All Complex u off the Cuts
@end ifinfo
@ifhtml
@chapter @math{\mathrm{gd}(u) = 2\tan^{-1}(\tanh(u/2))} for All Complex @math{u} off the Cuts
@end ifhtml
@iftex
@chapter gd(u) = 2 tan@sup{-1}(tanh(u/2)) for All Complex @math{u} off the Cuts
@end iftex

This identity is what the difference formula in @inlinefmtifelse{tex,
@ref{The Difference gd(A + w) − gd(A − w), section 5, The Difference
gd(A + w) − gd(A − w)}, @ref{The Difference gd(A + w) − gd(A − w),
section 5}} is built on. It could also be used to compute gd directly;
@inlinefmtifelse{tex, @ref{The Real and Imaginary Parts of gd, section
4, The Real and Imaginary Parts of gd}, @ref{The Real and Imaginary
Parts of gd, section 4}} compares its accuracy with the form that is
implemented.

Let
m4_mathdot(<<<f(u) = 2\tan^{-1}(\tanh(u/2))>>>, <<<f(u) = 2 tan⁻¹(tanh(u/2))>>>)

@menu
* Derivative::
* Branch Cuts::
* Conclusion::
* On the Cuts::
@end menu

@node Derivative
@section Derivative

Where
m4_math(<<<f>>>, <<<f>>>)
is analytic,

m4_displaymath(
<<<f'(u) = \frac{2 \cdot \frac12 \mathrm{sech}^2(u/2)}{1 + \tanh^2(u/2)}
= \frac{1}{\cosh^2(u/2) + \sinh^2(u/2)}
= \frac{1}{\cosh u}
= \mathrm{sech}\  u,>>>,
<<<@example
f'(u) = (2 · 1/2 sech²(u/2))/(1 + tanh²(u/2))
= 1/(cosh²(u/2) + sinh²(u/2))
= 1/(cosh u)
= sech u,
@end example>>>)

and
m4_mathdot(<<<f(0) = 0>>>, <<<f(0) = 0>>>)

@node Branch Cuts
@section Branch Cuts

Principal
m4_math(<<<\tan^{-1}(\zeta)>>>, <<<tan⁻¹(ζ)>>>)
is analytic except for
m4_math(<<<\zeta>>>, <<<ζ>>>)
on the rays
m4_mathdot(<<<\pm i[1, \infty)>>>, <<<± i[1, ∞)>>>)
So
m4_math(<<<f>>>, <<<f>>>)
fails to be analytic only where
m4_math(<<<\tanh(u/2) = is>>>, <<<tanh(u/2) = is>>>)
with
m4_math(<<<s>>>, <<<s>>>)
real and
m4_mathcomma(<<<\lvert s\rvert \ge 1>>>, <<<| s| ≥ 1>>>)
or where
m4_math(<<<\tanh(u/2)>>>, <<<tanh(u/2)>>>)
has a pole.

@itemize @bullet
@item
m4_math(<<<\tanh(u/2)>>>, <<<tanh(u/2)>>>)
is purely imaginary only for
m4_mathcomma(<<<u = iy>>>, <<<u = iy>>>)
where
m4_mathdot(<<<\tanh(iy/2) = i\tan(y/2)>>>, <<<tanh(iy/2) = i tan(y/2)>>>)
@item
m4_math(<<<\lvert\tan(y/2)\rvert \ge 1>>>, <<<|tan(y/2)| ≥ 1>>>)
exactly when
m4_mathdot(<<<y \in [\pi/2, 3\pi/2] \bmod 2\pi>>>, <<<y ∈ [π/2, 3π/2] mod 2π>>>)
This range also contains the poles of
m4_mathcomma(<<<\tanh(u/2)>>>, <<<tanh(u/2)>>>)
at
m4_mathdot(<<<y = \pi \bmod 2\pi>>>, <<<y = π mod 2π>>>)
@end itemize

So
m4_math(<<<f>>>, <<<f>>>)
is analytic off the segments from
m4_math(<<<(4t + 1)\ i\pi/2>>>, <<<(4t + 1) iπ/2>>>)
to
m4_mathcomma(<<<(4t + 3)\ i\pi/2>>>, <<<(4t + 3) iπ/2>>>)
which are the same cuts as gd.

@node Conclusion
@section Conclusion

m4_math(<<<f>>>, <<<f>>>)
and gd are analytic on the same region, the plane minus these segments.
That region is connected. They have the same derivative and agree at
m4_mathcomma(<<<u = 0>>>, <<<u = 0>>>)
so

m4_displaymath(
<<<\mathrm{gd}(u) = 2\tan^{-1}\left(\tanh\frac{u}{2}\right) \quad \text{for all } u \text{ off the cuts.}>>>,
<<<gd(u) = 2 tan⁻¹(tanh(u/2)) for all u off the cuts.>>>)

Equivalently,
m4_mathdot(<<<\tan(\mathrm{gd}(u)/2) = \tanh(u/2)>>>, <<<tan(gd(u)/2) = tanh(u/2)>>>)

@node On the Cuts
@section On the Cuts

There
m4_math(<<<\tanh(u/2)>>>, <<<tanh(u/2)>>>)
lies on a cut of
m4_mathcomma(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)
so the value returned depends on how the complex @code{atan} in use
treats an argument on its own cut.

@node gd(u) = tan^-1(sinh u) Only in the Strip |Im u| < π/2
@ifinfo
@chapter gd(u) = tan⁻¹(sinh u) Only in the Strip |Im u| < π/2
@end ifinfo
@ifhtml
@chapter @math{\mathrm{gd}(u) = \tan^{-1}(\sinh u)} Only in the Strip @math{\lvert\mathrm{Im}\ u\rvert < \pi/2}
@end ifhtml
@iftex
@chapter gd(u) = tan@sup{-1}(sinh u) Only in the Strip |Im u| < π/2
@end iftex

For real
m4_math(<<<u>>>, <<<u>>>)
the identity
m4_math(<<<\mathrm{gd}(u) = \tan^{-1}(\sinh u)>>>, <<<gd(u) = tan⁻¹(sinh u)>>>)
holds: both sides vanish at
m4_math(<<<0>>>, <<<0>>>)
and have derivative
m4_mathdot(<<<\mathrm{sech}\  u>>>, <<<sech u>>>)
It does not hold on the whole complex plane.

With
m4_mathcomma(<<<u = x + iy>>>, <<<u = x + iy>>>)

m4_displaymath(
<<<\sinh u = \sinh x \cos y + i \cosh x \sin y.>>>,
<<<sinh u = sinh x cos y + i cosh x sin y.>>>)

Where
m4_mathcomma(<<<\cos y = 0>>>, <<<cos y = 0>>>)
this is
m4_mathcomma(<<<\pm i\cosh x>>>, <<<± i cosh x>>>)
and
m4_math(<<<\lvert\cosh x\rvert \ge 1>>>, <<<|cosh x| ≥ 1>>>)
for every
m4_mathdot(<<<x>>>, <<<x>>>)
So
m4_math(<<<\tan^{-1}(\sinh u)>>>, <<<tan⁻¹(sinh u)>>>)
has cuts along the whole horizontal lines
m4_mathcomma(<<<\mathrm{Im}\  u = \pi/2 + k\pi>>>, <<<Im u = π/2 + kπ>>>)
which gd does not have.

Inside the strip
m4_mathcomma(<<<\lvert\mathrm{Im}\  u\rvert < \pi/2>>>, <<<|Im u| < π/2>>>)
m4_math(<<<\sinh u>>>, <<<sinh u>>>)
never lands on
m4_mathpunc(<<<:>>>, <<<\pm i[1, \infty)>>>, <<<± i[1, ∞)>>>)
that needs
m4_mathcomma(<<<\cos y = 0>>>, <<<cos y = 0>>>)
or
m4_math(<<<x = 0>>>, <<<x = 0>>>)
and
m4_mathdot(<<<\lvert\sin y\rvert \ge 1>>>, <<<|sin y| ≥ 1>>>)
So
m4_math(<<<\tan^{-1}(\sinh u)>>>, <<<tan⁻¹(sinh u)>>>)
is analytic there, and by the same argument as in @inlinefmtifelse{tex,
@ref{gd(u) = 2 tan^-1(tanh(u/2)) for All Complex u off the Cuts, section
2, gd(u) = 2 tan@sup{-1}(tanh(u/2)) for All Complex u off the Cuts},
@ref{gd(u) = 2 tan^-1(tanh(u/2)) for All Complex u off the Cuts, section
2}} it equals
m4_mathdot(<<<\mathrm{gd}(u)>>>, <<<gd(u)>>>)

Outside the strip the two differ by a multiple of
m4_mathdot(<<<\pi>>>, <<<π>>>)
For example, at
m4_mathpunc(<<<:>>>, <<<u = 1 + 2i>>>, <<<u = 1 + 2i>>>)

m4_displaymath(
<<<\eqalign{\mathrm{gd}(u) &= 1.911125\ldots + 0.676553\ldots i \cr \tan^{-1}(\sinh u) &= -1.230468\ldots + 0.676553\ldots i}>>>,
<<<@example
gd(u) = 1.911125… + 0.676553… i
tan⁻¹(sinh u) = −1.230468… + 0.676553… i
@end example>>>)

@node The Real and Imaginary Parts of gd
@chapter The Real and Imaginary Parts of gd

Sala's eq.@: (2.11) gives, for
m4_mathcomma(<<<x \ne 0>>>, <<<x ≠ 0>>>)

m4_displaymath(
<<<\eqalign{\mathrm{gd}(x + iy) &= \alpha + i\beta, \cr \alpha &= \mathrm{gd}(x) + \tan^{-1}(\mathrm{csch}\  x) - \tan^{-1}(\cos y \mathrm{csch}\  x), \cr \beta &= \tanh^{-1}(\sin y \mathrm{sech}\  x).}>>>,
<<<@example
gd(x + iy) = α + iβ,
α = gd(x) + tan⁻¹(csch x) − tan⁻¹(cos y csch x),
β = tanh⁻¹(sin y sech x).
@end example>>>)

The implementation rewrites the real part as

m4_displaymath(
<<<\alpha = \mathrm{atan2}(\sinh x, \cos y)>>>,
<<<α = atan2(sinh x, cos y)>>>)

and uses Sala's
m4_math(<<<\beta>>>, <<<β>>>)
unchanged:

@example lisp
(gd (z)
  ;; Eqn 2.11, with the real part written as atan2(sinh x, cos y)
  (let ((x (realpart z))
        (y (imagpart z)))
    (complex (atan (sinh x) (cos y))
             (atanh (* (sin y) (sech x))))))
@end example

A possible improvement is to rewrite the imaginary part as

m4_displaymath(
<<<\beta = \sinh^{-1}\left(\frac{\sin y}{\sqrt{\sinh^2 x + \cos^2 y}}\right),>>>,
<<<β = sinh⁻¹((sin y)/(sqrt(sinh² x + cos² y))),>>>)

which avoids the cancellation in
m4_math(<<<\tanh^{-1}>>>, <<<tanh⁻¹>>>)
near the branch points.

@menu
* Real Part::
* Imaginary Part (Possible Improvement)::
* Overflow::
* Accuracy Comparison::
@end menu

@node Real Part
@section Real Part

Let
m4_math(<<<s = \sinh x>>>, <<<s = sinh x>>>)
and
m4_mathcomma(<<<c = \cos y>>>, <<<c = cos y>>>)
with
m4_mathdot(<<<x \ne 0>>>, <<<x ≠ 0>>>)

@enumerate
@item
For real
m4_mathcomma(<<<x>>>, <<<x>>>)
m4_math(<<<\mathrm{gd}(x) = \tan^{-1}(\sinh x) = \tan^{-1} s>>>, <<<gd(x) = tan⁻¹(sinh x) = tan⁻¹ s>>>)
(@inlinefmtifelse{tex, @ref{gd(u) = tan^-1(sinh u) Only in the Strip |Im
u| < π/2, section 3, gd(u) = tan@sup{-1}(sinh u) Only in the Strip |Im
u| < π/2}, @ref{gd(u) = tan^-1(sinh u) Only in the Strip |Im u| < π/2,
section 3}}, real case).
@item
For real
m4_mathcomma(<<<s \ne 0>>>, <<<s ≠ 0>>>)
m4_mathcomma(<<<\tan^{-1} s + \tan^{-1}(1/s) = \mathrm{sign}(s)\ \pi/2>>>, <<<tan⁻¹ s + tan⁻¹(1/s) = sign(s) π/2>>>)
and
m4_mathdot(<<<\mathrm{sign}(s) = \mathrm{sign}(x)>>>, <<<sign(s) = sign(x)>>>)
Since
m4_mathcomma(<<<\mathrm{csch}\  x = 1/s>>>, <<<csch x = 1/s>>>)
@end enumerate

m4_displaymath(
<<<\alpha = \mathrm{sign}(s)\frac{\pi}{2} - \tan^{-1}\frac{c}{s}.>>>,
<<<α = sign(s)π/2 − tan⁻¹(c/s).>>>)

@enumerate 3
@item
For
m4_mathcomma(<<<s \ne 0>>>, <<<s ≠ 0>>>)
the two-argument arctangent is
@end enumerate

m4_displaymath(
<<<\mathrm{atan2}(s, c) =
\begin{cases}
\frac{\pi}{2} - \tan^{-1}\frac{c}{s}, & s > 0 \quad (\text{values in } (0, \pi)), \\
-\frac{\pi}{2} - \tan^{-1}\frac{c}{s}, & s < 0 \quad (\text{values in } (-\pi, 0)),
\end{cases}>>>,
<<<@example
atan2(s, c) =
    π/2 − tan⁻¹(c/s), s > 0 (values in (0, π)),
    −π/2 − tan⁻¹(c/s), s < 0 (values in (−π, 0)),
@end example>>>,
<<<\mathrm{atan2}(s, c) =
\cases{\frac{\pi}{2} - \tan^{-1}\frac{c}{s}, & $s > 0 \quad (\text{values in } (0, \pi)),$ \cr -\frac{\pi}{2} - \tan^{-1}\frac{c}{s}, & $s < 0 \quad (\text{values in } (-\pi, 0)),$}>>>)

which is
m4_math(<<<\mathrm{sign}(s)\ \pi/2 - \tan^{-1}(c/s)>>>, <<<sign(s) π/2 − tan⁻¹(c/s)>>>)
in both cases.

Therefore
m4_mathdot(<<<\alpha = \mathrm{atan2}(\sinh x, \cos y)>>>, <<<α = atan2(sinh x, cos y)>>>)

@menu
* At x = 0::
@end menu

@node At x = 0
@ifinfo
@subsection At x = 0
@end ifinfo
@ifhtml
@subsection At @math{x = 0}
@end ifhtml
@iftex
@subsection At @math{x = 0}
@end iftex

Sala's formula divides by zero at
m4_math(<<<x = 0>>>, <<<x = 0>>>)
and he treats that case separately. The atan2 form needs no special
case:

@itemize @bullet
@item
m4_mathpunc(<<<:>>>, <<<\cos y > 0>>>, <<<cos y > 0>>>)
m4_mathcomma(<<<\mathrm{atan2}(0, \cos y) = 0>>>, <<<atan2(0, cos y) = 0>>>)
matching Sala's
m4_math(<<<x = 0>>>, <<<x = 0>>>)
case.
@item
m4_mathpunc(<<<:>>>, <<<\cos y < 0>>>, <<<cos y < 0>>>)
the point lies on a cut. atan2 gives
m4_mathcomma(<<<\pi>>>, <<<π>>>)
which is the limit from
m4_mathdot(<<<x > 0>>>, <<<x > 0>>>)
With IEEE signed zeros,
m4_math(<<<x = -0.0>>>, <<<x = −0.0>>>)
gives
m4_mathcomma(<<<-\pi>>>, <<<−π>>>)
the limit from
m4_mathdot(<<<x < 0>>>, <<<x < 0>>>)
So the side of the cut is chosen explicitly, unlike in the
m4_math(<<<2\tan^{-1}(\tanh(u/2))>>>, <<<2 tan⁻¹(tanh(u/2))>>>)
form.
@end itemize

The atan2 form also avoids the cancellation in Sala's form for small
m4_mathcomma(<<<x>>>, <<<x>>>)
where
m4_math(<<<\mathrm{gd}(x) + \tan^{-1}(\mathrm{csch}\  x)>>>, <<<gd(x) + tan⁻¹(csch x)>>>)
and
m4_math(<<<\tan^{-1}(\cos y \mathrm{csch}\  x)>>>, <<<tan⁻¹(cos y csch x)>>>)
are both near
m4_mathdot(<<<\pi/2>>>, <<<π/2>>>)

@node Imaginary Part (Possible Improvement)
@section Imaginary Part (Possible Improvement)

Sala's
m4_mathcomma(<<<\beta = \tanh^{-1}(\sin y \mathrm{sech}\  x)>>>, <<<β = tanh⁻¹(sin y sech x)>>>)
as implemented, loses accuracy near the branch points
m4_mathcomma(<<<x = 0>>>, <<<x = 0>>>)
m4_mathdot(<<<\sin y = \pm 1>>>, <<<sin y = ± 1>>>)
There
m4_math(<<<\sin y \mathrm{sech}\  x>>>, <<<sin y sech x>>>)
is close to
m4_mathcomma(<<<1>>>, <<<1>>>)
so
m4_math(<<<\tanh^{-1}>>>, <<<tanh⁻¹>>>)
cancels, and in floating point the product can round to
m4_math(<<<1>>>, <<<1>>>)
or above, where
m4_math(<<<\tanh^{-1}>>>, <<<tanh⁻¹>>>)
fails.

From
m4_mathcomma(<<<\tanh\beta = \sin y / \cosh x>>>, <<<tanhβ = sin y / cosh x>>>)

m4_displaymath(
<<<\sinh\beta = \frac{\tanh\beta}{\sqrt{1 - \tanh^2\beta}}
= \frac{\sin y}{\sqrt{\cosh^2 x - \sin^2 y}}
= \frac{\sin y}{\sqrt{\sinh^2 x + \cos^2 y}},>>>,
<<<@example
sinhβ = tanhβ/(sqrt(1 − tanh²β))
= (sin y)/(sqrt(cosh² x − sin² y))
= (sin y)/(sqrt(sinh² x + cos² y)),
@end example>>>)

using
m4_mathdot(<<<\cosh^2 x - \sin^2 y = \sinh^2 x + 1 - \sin^2 y = \sinh^2 x + \cos^2 y>>>, <<<cosh² x − sin² y = sinh² x + 1 − sin² y = sinh² x + cos² y>>>)
So

m4_displaymath(
<<<\beta = \sinh^{-1}\left(\frac{\sin y}{\sqrt{\sinh^2 x + \cos^2 y}}\right).>>>,
<<<β = sinh⁻¹((sin y)/(sqrt(sinh² x + cos² y))).>>>)

The sum under the square root has no cancellation. It is zero only at
m4_mathcomma(<<<x = 0>>>, <<<x = 0>>>)
m4_mathcomma(<<<\cos y = 0>>>, <<<cos y = 0>>>)
the branch points, where gd is singular.

@node Overflow
@section Overflow

In double precision
m4_math(<<<\sinh^2 x>>>, <<<sinh² x>>>)
overflows for
m4_mathdot(<<<\lvert x\rvert \gtrsim 355>>>, <<<| x| ≳ 355>>>)
Scaling by the larger of
m4_math(<<<\lvert\sinh x\rvert>>>, <<<|sinh x|>>>)
and
m4_mathcomma(<<<\lvert\cos y\rvert>>>, <<<|cos y|>>>)
as hypot does, avoids that.
m4_math(<<<\sinh x>>>, <<<sinh x>>>)
itself, and
m4_math(<<<\cosh x>>>, <<<cosh x>>>)
in the implemented
m4_mathcomma(<<<\mathrm{sech}\  x>>>, <<<sech x>>>)
overflow for
m4_mathpunc(<<<;>>>, <<<\lvert x\rvert \gtrsim 710>>>, <<<| x| ≳ 710>>>)
in that range
m4_math(<<<\alpha = \mathrm{sign}(x)\ \pi/2>>>, <<<α = sign(x) π/2>>>)
and
m4_math(<<<\beta = 0>>>, <<<β = 0>>>)
to working precision, so it would need its own branch. With bigfloats
the exponent range is effectively unlimited and none of these limits
arises. In @code{bf-jacobi-am} the reduction of @inlinefmtifelse{tex,
@ref{Reduction by 2K, section 6.1, Reduction by 2K}, @ref{Reduction by
2K, section 6.1}} keeps the arguments small enough that these limits are
not reached.

The
m4_math(<<<\sinh^{-1}>>>, <<<sinh⁻¹>>>)
form with scaling:

@example lisp
(gd (z)
  (let* ((x (realpart z))
         (y (imagpart z))
         (sx (sinh x))
         (cy (cos y))
         (big (max (abs sx) (abs cy)))
         (small (min (abs sx) (abs cy))))
    (complex (atan sx cy)
             (asinh (/ (sin y)
                       (* big (sqrt (+ 1 (expt (/ small big) 2)))))))))
@end example

@node Accuracy Comparison
@section Accuracy Comparison

Double precision, compared with a 50-digit reference evaluated at the
same double inputs, so this measures the algorithm's error and not the
conditioning of gd. 20,000 random points with
m4_math(<<<10^{-12} \le \lvert x\rvert \le 40>>>, <<<10⁻¹² ≤ | x| ≤ 40>>>)
and
m4_mathcomma(<<<\lvert y\rvert \le 10>>>, <<<| y| ≤ 10>>>)
plus points near the branch points:

@raggedright
@multitable @columnfractions 0.35 0.22 0.44
@headitem
form
@tab
worst rel. error, real
@tab
worst rel. error, imag
@item
atan2 + Sala's
m4_math(<<<\tanh^{-1}>>>, <<<tanh⁻¹>>>)
(implemented)
@tab
m4_math(<<<4 \times 10^{-16}>>>, <<<4 × 10⁻¹⁶>>>)
@tab
m4_mathpunc(<<<;>>>, <<<3 \times 10^{-10}>>>, <<<3 × 10⁻¹⁰>>>)
fails at
m4_mathcomma(<<<x = 10^{-10}>>>, <<<x = 10⁻¹⁰>>>)
m4_math(<<<y = 1.5707963>>>, <<<y = 1.5707963>>>)
@item
m4_math(<<<2\tan^{-1}(\tanh(u/2))>>>, <<<2 tan⁻¹(tanh(u/2))>>>)
@tab
m4_math(<<<1 \times 10^{-12}>>>, <<<1 × 10⁻¹²>>>)
@tab
m4_math(<<<1 \times 10^{-13}>>>, <<<1 × 10⁻¹³>>>)
@item
atan2 +
m4_math(<<<\sinh^{-1}>>>, <<<sinh⁻¹>>>)
form
@tab
m4_math(<<<4 \times 10^{-16}>>>, <<<4 × 10⁻¹⁶>>>)
@tab
m4_math(<<<4 \times 10^{-16}>>>, <<<4 × 10⁻¹⁶>>>)
@end multitable
@end raggedright

In both of the first two rows, the worst cases are near the branch
points. The
m4_math(<<<\sinh^{-1}>>>, <<<sinh⁻¹>>>)
form also needs only real functions, where
m4_math(<<<2\tan^{-1}(\tanh(u/2))>>>, <<<2 tan⁻¹(tanh(u/2))>>>)
needs complex
m4_math(<<<\tanh>>>, <<<tanh>>>)
and complex
m4_mathdot(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)

@node The Difference gd(A + w) − gd(A − w)
@chapter The Difference gd(A + w) − gd(A − w)


@menu
* The Series and the Identity::
* Where the Principal Branch Is Correct::
* Why the tan^-1 Form Is Needed::
* Where the tan^-1 Form Fails::
@end menu

@node The Series and the Identity
@section The Series and the Identity


@menu
* The Series::
* The Identity::
@end menu

@node The Series
@subsection The Series

With
m4_mathcomma(<<<K = K(m)>>>, <<<K = K(m)>>>)
m4_mathcomma(<<<K' = K(1 - m)>>>, <<<K' = K(1 − m)>>>)
and

m4_displaymath(
<<<c = \frac{\pi K}{K'}, \qquad w = \frac{\pi z}{2K'} = \frac{c\ z}{2K},>>>,
<<<c = (π K)/K', w = (π z)/2K' = (c z)/2K,>>>)

Sala's series for the amplitude is

m4_displaymath(
<<<\mathrm{am}(z|m) = \mathrm{gd}(w) + \sum_{n \ge 1} \left[\mathrm{gd}(cn + w) - \mathrm{gd}(cn - w)\right].>>>,
<<<am(z|m) = gd(w) + Σ_(n ≥ 1) [gd(cn + w) − gd(cn − w)].>>>)

For
m4_mathcomma(<<<n \ge 1>>>, <<<n ≥ 1>>>)
write
m4_mathdot(<<<A = cn>>>, <<<A = cn>>>)
For small
m4_math(<<<\lvert w\rvert>>>, <<<| w|>>>)
the two terms in each bracket nearly cancel, so we want a form that
computes the difference directly.

@node The Identity
@subsection The Identity

m4_displaymath(
<<<\mathrm{gd}(A + w) - \mathrm{gd}(A - w) = 2\tan^{-1}\left(\frac{\sinh w}{\cosh A}\right).>>>,
<<<gd(A + w) − gd(A − w) = 2 tan⁻¹((sinh w)/(cosh A)).>>>)

@menu
* Derivation::
* Check at A = 0::
@end menu

@node Derivation
@subsubsection Derivation

Let
m4_mathcomma(<<<D = \mathrm{gd}(A + w) - \mathrm{gd}(A - w)>>>, <<<D = gd(A + w) − gd(A − w)>>>)
and let
m4_mathcomma(<<<a = (A + w)/2>>>, <<<a = (A + w)/2>>>)
m4_mathcomma(<<<b = (A - w)/2>>>, <<<b = (A − w)/2>>>)
so that
m4_math(<<<a - b = w>>>, <<<a − b = w>>>)
and
m4_mathdot(<<<a + b = A>>>, <<<a + b = A>>>)

@enumerate
@item
By @inlinefmtifelse{tex, @ref{gd(u) = 2 tan^-1(tanh(u/2)) for All
Complex u off the Cuts, section 2, gd(u) = 2 tan@sup{-1}(tanh(u/2)) for
All Complex u off the Cuts}, @ref{gd(u) = 2 tan^-1(tanh(u/2)) for All
Complex u off the Cuts, section 2}},
m4_mathdot(<<<\tan(\mathrm{gd}(u)/2) = \tanh(u/2)>>>, <<<tan(gd(u)/2) = tanh(u/2)>>>)
So with
m4_math(<<<\alpha = \mathrm{gd}(A + w)/2>>>, <<<α = gd(A + w)/2>>>)
and
m4_mathcomma(<<<\beta = \mathrm{gd}(A - w)/2>>>, <<<β = gd(A − w)/2>>>)
@end enumerate

m4_displaymath(
<<<\tan\alpha = \tanh a, \qquad \tan\beta = \tanh b, \qquad \frac{D}{2} = \alpha - \beta.>>>,
<<<tanα = tanh a, tanβ = tanh b, D/2 = α − β.>>>)

@enumerate 2
@item
Tangent subtraction, an algebraic identity valid for complex values:
@end enumerate

m4_displaymath(
<<<\tan\frac{D}{2} = \frac{\tanh a - \tanh b}{1 + \tanh a \tanh b}.>>>,
<<<tan(D/2) = (tanh a − tanh b)/(1 + tanh a tanh b).>>>)

@enumerate 3
@item
Multiply the numerator and denominator by
m4_mathpunc(<<<:>>>, <<<\cosh a \cosh b>>>, <<<cosh a cosh b>>>)
@end enumerate

m4_displaymath(
<<<\eqalign{\text{numerator:} &\quad \sinh a \cosh b - \cosh a \sinh b = \sinh(a - b) = \sinh w, \cr \text{denominator:} &\quad \cosh a \cosh b + \sinh a \sinh b = \cosh(a + b) = \cosh A,}>>>,
<<<@example
numerator: sinh a cosh b − cosh a sinh b = sinh(a − b) = sinh w,
denominator: cosh a cosh b + sinh a sinh b = cosh(a + b) = cosh A,
@end example>>>)

so

m4_displaymath(
<<<\tan\frac{D}{2} = \frac{\sinh w}{\cosh A}.>>>,
<<<tan(D/2) = (sinh w)/(cosh A).>>>)

@enumerate 4
@item
Step 3 determines
m4_math(<<<D/2>>>, <<<D/2>>>)
only modulo
m4_mathdot(<<<\pi>>>, <<<π>>>)
The branch is fixed in @inlinefmtifelse{tex, @ref{Where the Principal
Branch Is Correct, section 5.2, Where the Principal Branch Is Correct},
@ref{Where the Principal Branch Is Correct, section 5.2}}.
@end enumerate

@node Check at A = 0
@ifinfo
@subsubsection Check at A = 0
@end ifinfo
@ifhtml
@subsubsection Check at @math{A = 0}
@end ifhtml
@iftex
@subsubsection Check at @math{A = 0}
@end iftex

The identity gives
m4_math(<<<\mathrm{gd}(w) - \mathrm{gd}(-w) = 2\mathrm{gd}(w)>>>, <<<gd(w) − gd(−w) = 2gd(w)>>>)
on the left and
m4_math(<<<2\tan^{-1}(\sinh w)>>>, <<<2 tan⁻¹(sinh w)>>>)
on the right. These agree only for
m4_math(<<<\lvert\mathrm{Im}\  w\rvert < \pi/2>>>, <<<|Im w| < π/2>>>)
(@inlinefmtifelse{tex, @ref{gd(u) = tan^-1(sinh u) Only in the Strip |Im
u| < π/2, section 3, gd(u) = tan@sup{-1}(sinh u) Only in the Strip |Im
u| < π/2}, @ref{gd(u) = tan^-1(sinh u) Only in the Strip |Im u| < π/2,
section 3}}). This is not a contradiction: the branch condition below
requires
m4_mathcomma(<<<\sinh\lvert w\rvert < \cosh 0 = 1>>>, <<<sinh| w| < cosh 0 = 1>>>)
i.e.@:
m4_mathdot(<<<\lvert w\rvert < \sinh^{-1} 1 \approx 0.881 < \pi/2>>>, <<<| w| < sinh⁻¹ 1 ≈ 0.881 < π/2>>>)
The series never uses
m4_mathpunc(<<<;>>>, <<<A = 0>>>, <<<A = 0>>>)
its
m4_math(<<<n = 0>>>, <<<n = 0>>>)
term is
m4_math(<<<\mathrm{gd}(w)>>>, <<<gd(w)>>>)
itself.

@node Where the Principal Branch Is Correct
@section Where the Principal Branch Is Correct

Let
m4_mathdot(<<<\zeta = \sinh w / \cosh A>>>, <<<ζ = sinh w / cosh A>>>)
The identity
m4_math(<<<D = 2\tan^{-1}\zeta>>>, <<<D = 2 tan⁻¹ζ>>>)
holds with principal
m4_math(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)
when

m4_displaymath(
<<<\sinh\lvert w\rvert < \lvert\cosh A\rvert \quad\text{and}\quad \lvert\mathrm{Re}\  w\rvert < \mathrm{Re}\  A.>>>,
<<<sinh| w| < |cosh A| and |Re w| < Re A.>>>)

For real
m4_math(<<<A>>>, <<<A>>>)
the second condition is implied by the first (shown below), but
@code{bf-jacobi-am} is also called with complex
m4_math(<<<m>>>, <<<m>>>)
and with real
m4_math(<<<m>>>, <<<m>>>)
outside
m4_mathdot(<<<[0, 1]>>>, <<<[0, 1]>>>)
Then
m4_mathcomma(<<<K' = K(1 - m)>>>, <<<K' = K(1 − m)>>>)
and so
m4_math(<<<c>>>, <<<c>>>)
and
m4_mathcomma(<<<A = cn>>>, <<<A = cn>>>)
are complex, and both tests are needed.
m4_math(<<<\mathrm{Re}\  A = n \mathrm{Re}\  c > 0>>>, <<<Re A = n Re c > 0>>>)
always (@inlinefmtifelse{tex, @ref{Why Re c > 0, section 6.2, Why Re c >
0}, @ref{Why Re c > 0, section 6.2}}).

@code{bf-jacobi-am} uses the single test

m4_displaymath(
<<<\lvert w\rvert < \mathrm{Re}\  A,>>>,
<<<| w| < Re A,>>>)

which implies both:
m4_mathcomma(<<<\lvert\mathrm{Re}\  w\rvert \le \lvert w\rvert < \mathrm{Re}\  A>>>, <<<|Re w| ≤ | w| < Re A>>>)
and
m4_mathcomma(<<<\sinh\lvert w\rvert < \sinh(\mathrm{Re}\  A) \le \lvert\cosh A\rvert>>>, <<<sinh| w| < sinh(Re A) ≤ |cosh A|>>>)
since
m4_mathdot(<<<\lvert\cosh A\rvert^2 = \sinh^2(\mathrm{Re}\  A) + \cos^2(\mathrm{Im}\  A)>>>, <<<|cosh A|² = sinh²(Re A) + cos²(Im A)>>>)
It needs no
m4_math(<<<\sinh>>>, <<<sinh>>>)
of
m4_mathcomma(<<<\lvert w\rvert>>>, <<<| w|>>>)
which can overflow. The region it gives up,
m4_mathcomma(<<<\mathrm{Re}\  A \le \lvert w\rvert < \sinh^{-1}\lvert\cosh A\rvert>>>, <<<Re A ≤ | w| < sinh⁻¹|cosh A|>>>)
has
m4_math(<<<\lvert w\rvert>>>, <<<| w|>>>)
no smaller than
m4_mathcomma(<<<\mathrm{Re}\  A>>>, <<<Re A>>>)
so there the gd difference loses at most about a digit.

@menu
* Real A and w and Condition (a)::
* Condition (b)::
* Example Where the Second Test Is Needed - m = −6::
@end menu

@node Real A and w and Condition (a)
@ifinfo
@subsection Real A and w, and Condition (a)
@end ifinfo
@ifhtml
@subsection Real @math{A} and @math{w}, and Condition (a)
@end ifhtml
@iftex
@subsection Real @math{A} and @math{w}, and Condition (a)
@end iftex


@menu
* Real A and Real w::
* Complex w::
* Condition (a)::
@end menu

@node Real A and Real w
@ifinfo
@subsubsection Real A and Real w
@end ifinfo
@ifhtml
@subsubsection Real @math{A} and Real @math{w}
@end ifhtml
@iftex
@subsubsection Real @math{A} and Real @math{w}
@end iftex

gd takes values in
m4_mathcomma(<<<(-\pi/2, \pi/2)>>>, <<<(−π/2, π/2)>>>)
so
m4_math(<<<D \in (-\pi, \pi)>>>, <<<D ∈ (−π, π)>>>)
and
m4_mathcomma(<<<D/2 \in (-\pi/2, \pi/2)>>>, <<<D/2 ∈ (−π/2, π/2)>>>)
the range of principal
m4_mathdot(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)
The identity holds for all real
m4_math(<<<A>>>, <<<A>>>)
and
m4_mathcomma(<<<w>>>, <<<w>>>)
with no condition.

@node Complex w
@ifinfo
@subsubsection Complex w
@end ifinfo
@ifhtml
@subsubsection Complex @math{w}
@end ifhtml
@iftex
@subsubsection Complex @math{w}
@end iftex

Follow the path
m4_math(<<<tw>>>, <<<tw>>>)
for
m4_mathdot(<<<0 \le t \le 1>>>, <<<0 ≤ t ≤ 1>>>)
At
m4_math(<<<t = 0>>>, <<<t = 0>>>)
both sides are
m4_mathdot(<<<0>>>, <<<0>>>)
Both sides are analytic in
m4_math(<<<t>>>, <<<t>>>)
along the path provided that

@itemize @bullet
@item
(a)
m4_math(<<<\zeta(t) = \sinh(tw)/\cosh A>>>, <<<ζ(t) = sinh(tw)/cosh A>>>)
stays off the cuts
m4_math(<<<\pm i[1, \infty)>>>, <<<± i[1, ∞)>>>)
of
m4_mathcomma(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)
and
@item
(b)
m4_math(<<<A + tw>>>, <<<A + tw>>>)
and
m4_math(<<<A - tw>>>, <<<A − tw>>>)
stay off the cuts of gd.
@end itemize

If both hold, the two sides agree along the whole path, and in
particular at
m4_mathdot(<<<t = 1>>>, <<<t = 1>>>)

@node Condition (a)
@subsubsection Condition (a)

For complex
m4_mathcomma(<<<v>>>, <<<v>>>)
m4_mathcomma(<<<\lvert\sinh v\rvert \le \sinh\lvert v\rvert>>>, <<<|sinh v| ≤ sinh| v|>>>)
since the power series of
m4_math(<<<\sinh>>>, <<<sinh>>>)
has non-negative coefficients. So along the path

m4_displaymath(
<<<\lvert\zeta(t)\rvert \le \frac{\sinh(t\lvert w\rvert)}{\lvert\cosh A\rvert} \le \frac{\sinh\lvert w\rvert}{\lvert\cosh A\rvert} < 1,>>>,
<<<|ζ(t)| ≤ (sinh(t| w|))/(|cosh A|) ≤ (sinh| w|)/(|cosh A|) < 1,>>>)

and
m4_math(<<<\zeta(t)>>>, <<<ζ(t)>>>)
stays inside the unit disk, away from the cuts of
m4_mathdot(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)

@node Condition (b)
@subsection Condition (b)


@menu
* Real A ≥ 0::
* Complex A::
@end menu

@node Real A ≥ 0
@ifinfo
@subsubsection Real A ≥ 0
@end ifinfo
@ifhtml
@subsubsection Real @math{A \ge 0}
@end ifhtml
@iftex
@subsubsection Real A ≥ 0
@end iftex

A point
m4_math(<<<A \pm tw>>>, <<<A ± tw>>>)
on a cut of gd has real part
m4_math(<<<0>>>, <<<0>>>)
and imaginary part at least
m4_math(<<<\pi/2>>>, <<<π/2>>>)
in absolute value. So it needs
m4_math(<<<\lvert\mathrm{Re}(tw)\rvert = A>>>, <<<|Re(tw)| = A>>>)
and
m4_mathcomma(<<<\lvert\mathrm{Im}(tw)\rvert \ge \pi/2>>>, <<<|Im(tw)| ≥ π/2>>>)
hence

m4_displaymath(
<<<\lvert w\rvert^2 \ge \lvert tw\rvert^2 \ge A^2 + \frac{\pi^2}{4}.>>>,
<<<| w|² ≥ | tw|² ≥ A² + π²/4.>>>)

The condition
m4_math(<<<\sinh\lvert w\rvert < \cosh A>>>, <<<sinh| w| < cosh A>>>)
gives
m4_mathdot(<<<\lvert w\rvert < \sinh^{-1}(\cosh A)>>>, <<<| w| < sinh⁻¹(cosh A)>>>)
It is enough to show

m4_displaymath(
<<<\left[\sinh^{-1}(\cosh A)\right]^2 < A^2 + \frac{\pi^2}{4} \quad \text{for all } A \ge 0.>>>,
<<<[sinh⁻¹(cosh A)]² < A² + π²/4 for all A ≥ 0.>>>)

@itemize @bullet
@item
The derivative of
m4_math(<<<\sinh^{-1}(\cosh A)>>>, <<<sinh⁻¹(cosh A)>>>)
is
m4_mathcomma(<<<\sinh A / \sqrt{\cosh^2 A + 1} < 1>>>, <<<sinh A / sqrt(cosh² A + 1) < 1>>>)
so
m4_mathcomma(<<<\sinh^{-1}(\cosh A) \le A + \sinh^{-1} 1>>>, <<<sinh⁻¹(cosh A) ≤ A + sinh⁻¹ 1>>>)
with
m4_mathdot(<<<\sinh^{-1} 1 \approx 0.8814>>>, <<<sinh⁻¹ 1 ≈ 0.8814>>>)
Then
m4_mathcomma(<<<\left[\sinh^{-1}(\cosh A)\right]^2 - A^2 \le 2 \cdot 0.8814\ A + 0.7768>>>, <<<[sinh⁻¹(cosh A)]² − A² ≤ 2 · 0.8814 A + 0.7768>>>)
which is below
m4_math(<<<\pi^2/4 \approx 2.4674>>>, <<<π²/4 ≈ 2.4674>>>)
for
m4_mathdot(<<<A < 0.95>>>, <<<A < 0.95>>>)
@item
For
m4_mathcomma(<<<A \ge 0.95>>>, <<<A ≥ 0.95>>>)
use
m4_math(<<<\sqrt{C^2 + 1} \le C + 1/(2C)>>>, <<<sqrt(C² + 1) ≤ C + 1/(2C)>>>)
with
m4_mathpunc(<<<:>>>, <<<C = \cosh A>>>, <<<C = cosh A>>>)
@end itemize

m4_displaymath(
<<<\sinh^{-1}(\cosh A) - A \le \ln\left(1 + e^{-2A} + \frac{1}{e^{2A} + 1}\right) \le 2e^{-2A} =: \delta,>>>,
<<<sinh⁻¹(cosh A) − A ≤ ln(1 + e^(−2A) + 1/(e^(2A) + 1)) ≤ 2e^(−2A) =: δ,>>>)

so
m4_mathdot(<<<\left[\sinh^{-1}(\cosh A)\right]^2 - A^2 \le \delta(2A + \delta)>>>, <<<[sinh⁻¹(cosh A)]² − A² ≤ δ(2A + δ)>>>)
At
m4_math(<<<A = 0.95>>>, <<<A = 0.95>>>)
this is about
m4_mathcomma(<<<0.66>>>, <<<0.66>>>)
and it decreases for larger
m4_mathdot(<<<A>>>, <<<A>>>)

So condition (b) holds whenever condition (a) does, for real
m4_mathdot(<<<A \ge 0>>>, <<<A ≥ 0>>>)
The largest value of
m4_math(<<<\left[\sinh^{-1}(\cosh A)\right]^2 - A^2>>>, <<<[sinh⁻¹(cosh A)]² − A²>>>)
is
m4_mathcomma(<<<\left[\sinh^{-1}(1)\right]^2 \approx 0.777>>>, <<<[sinh⁻¹(1)]² ≈ 0.777>>>)
at
m4_mathdot(<<<A = 0>>>, <<<A = 0>>>)

@node Complex A
@ifinfo
@subsubsection Complex A
@end ifinfo
@ifhtml
@subsubsection Complex @math{A}
@end ifhtml
@iftex
@subsubsection Complex @math{A}
@end iftex

The argument above assumes
m4_math(<<<A>>>, <<<A>>>)
real, and for complex
m4_math(<<<A>>>, <<<A>>>)
the first test alone is not enough. But if
m4_mathcomma(<<<\lvert\mathrm{Re}\  w\rvert < \mathrm{Re}\  A>>>, <<<|Re w| < Re A>>>)
then along the path

m4_displaymath(
<<<\mathrm{Re}(A \pm tw) \ge \mathrm{Re}\  A - t\lvert\mathrm{Re}\  w\rvert > 0,>>>,
<<<Re(A ± tw) ≥ Re A − t|Re w| > 0,>>>)

so
m4_math(<<<A \pm tw>>>, <<<A ± tw>>>)
never reaches the imaginary axis, where all the cuts of gd lie. That
gives condition (b) for any
m4_math(<<<A>>>, <<<A>>>)
with
m4_mathdot(<<<\mathrm{Re}\  A > 0>>>, <<<Re A > 0>>>)

@node Example Where the Second Test Is Needed - m = −6
@ifinfo
@subsection Example Where the Second Test Is Needed: m = −6
@end ifinfo
@ifhtml
@subsection Example Where the Second Test Is Needed: @math{m = -6}
@end ifhtml
@iftex
@subsection Example Where the Second Test Is Needed: @math{m = -6}
@end iftex

Take
m4_mathcomma(<<<m = -6>>>, <<<m = −6>>>)
a real
m4_mathdot(<<<m>>>, <<<m>>>)
Then

m4_displaymath(
<<<\eqalign{K &= 0.911490\ldots, \cr K' &= K(7) = 0.616803\ldots - 0.911490\ldots i, \cr c &= \frac{\pi K}{K'} = 1.458182\ldots + 2.154853\ldots i,}>>>,
<<<@example
K = 0.911490…,
K' = K(7) = 0.616803… − 0.911490… i,
c = (π K)/K' = 1.458182… + 2.154853… i,
@end example>>>)

and take
m4_mathcomma(<<<z = 2K' \cdot 1.47/\pi = 0.577223\ldots - 0.853001\ldots i>>>, <<<z = 2K' · 1.47/π = 0.577223… − 0.853001… i>>>)
so
m4_mathdot(<<<w = 1.47>>>, <<<w = 1.47>>>)
The first test passes, since
m4_mathdot(<<<\sinh 1.47 = 2.04 < \lvert\cosh c\rvert = 2.10>>>, <<<sinh 1.47 = 2.04 < |cosh c| = 2.10>>>)
But
m4_mathcomma(<<<\mathrm{Re}(c - w) = -0.012>>>, <<<Re(c − w) = −0.012>>>)
so
m4_math(<<<c - tw>>>, <<<c − tw>>>)
crosses the imaginary axis at
m4_mathcomma(<<<\mathrm{Im} \approx 2.15>>>, <<<Im ≈ 2.15>>>)
inside the cut of gd from
m4_math(<<<i\pi/2>>>, <<<iπ/2>>>)
to
m4_mathdot(<<<3i\pi/2>>>, <<<3iπ/2>>>)
The
m4_math(<<<n = 1>>>, <<<n = 1>>>)
term then differs by
m4_math(<<<2\pi>>>, <<<2π>>>)
between the two forms:

m4_displaymath(
<<<\eqalign{\text{all terms as gd differences:} &\quad \mathrm{am} = 5.77066731555 - 0.730409062547\ i \cr \tan^{-1} \text{ form for } n = 1\text{:} &\quad \mathrm{am} = -0.51251799163 - 0.730409062547\ i}>>>,
<<<@example
all terms as gd differences: am = 5.77066731555 − 0.730409062547 i
tan⁻¹ form for n = 1: am = −0.51251799163 − 0.730409062547 i
@end example>>>)

Both satisfy
m4_math(<<<\sin(\mathrm{am}) = \mathrm{sn}(z|m)>>>, <<<sin(am) = sn(z|m)>>>)
and
m4_math(<<<\cos(\mathrm{am}) = \mathrm{cn}(z|m)>>>, <<<cos(am) = cn(z|m)>>>)
to 30 digits, so a check against sn and cn does not detect the error.
The single test
m4_math(<<<\lvert w\rvert < \mathrm{Re}\  A>>>, <<<| w| < Re A>>>)
also rejects this point, since
m4_mathdot(<<<\lvert w\rvert = 1.47 > \mathrm{Re}\  c = 1.458>>>, <<<| w| = 1.47 > Re c = 1.458>>>)

The gd-difference value is the one consistent with Mathematica's branch
cuts. The line
m4_math(<<<\mathrm{Re}(c - w) = 0>>>, <<<Re(c − w) = 0>>>)
is
m4_math(<<<z = 2K - isK'>>>, <<<z = 2K − isK'>>>)
with
m4_math(<<<s>>>, <<<s>>>)
real, and the segment
m4_math(<<<1 \le s \le 3>>>, <<<1 ≤ s ≤ 3>>>)
is Mathematica's cut from
m4_math(<<<2K - iK'>>>, <<<2K − iK'>>>)
to
m4_mathdot(<<<2K - 3iK'>>>, <<<2K − 3iK'>>>)
This
m4_math(<<<z>>>, <<<z>>>)
is
m4_mathcomma(<<<2K - iK'(1.3718 + 0.0075i)>>>, <<<2K − iK'(1.3718 + 0.0075i)>>>)
just beside that cut. The gd-difference series jumps by
m4_math(<<<2\pi>>>, <<<2π>>>)
across the cut, while the
m4_math(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)
form continues analytically through it and returns the value from the
other side. At
m4_mathcomma(<<<w = 1.44>>>, <<<w = 1.44>>>)
on the near side, both forms give
m4_mathdot(<<<-0.465903 - 0.740155i>>>, <<<−0.465903 − 0.740155i>>>)

@node Why the tan^-1 Form Is Needed
@ifinfo
@section Why the tan⁻¹ Form Is Needed
@end ifinfo
@ifhtml
@section Why the @math{\tan^{-1}} Form Is Needed
@end ifhtml
@iftex
@section Why the tan@sup{-1} Form Is Needed
@end iftex

In double precision with
m4_math(<<<A = 3>>>, <<<A = 3>>>)
and
m4_mathpunc(<<<:>>>, <<<w = 10^{-8}>>>, <<<w = 10⁻⁸>>>)

@raggedright
@multitable @columnfractions 0.43 0.38 0.20
@headitem
quantity
@tab
value
@tab
rel. error
@item
m4_math(<<<\mathrm{gd}(3 + 10^{-8})>>>, <<<gd(3 + 10⁻⁸)>>>)
@tab
1.471304@/34211047@/2
@tab

@item
m4_math(<<<\mathrm{gd}(3 - 10^{-8})>>>, <<<gd(3 − 10⁻⁸)>>>)
@tab
1.471304@/34012391@/34
@tab

@item
difference
@tab
1.986558@/47327700@/03e−9
@tab
m4_math(<<<3.8 \times 10^{-8}>>>, <<<3.8 × 10⁻⁸>>>)
@item
m4_math(<<<2\tan^{-1}(\sinh w / \cosh A)>>>, <<<2 tan⁻¹(sinh w / cosh A)>>>)
@tab
1.986558@/54838866@/47e−9
@tab
m4_math(<<<2.6 \times 10^{-16}>>>, <<<2.6 × 10⁻¹⁶>>>)
@item
exact
@tab
1.986558@/54838866@/4…e−9
@tab

@end multitable
@end raggedright

Each gd value carries an absolute error of about
m4_mathcomma(<<<\varepsilon\ \mathrm{gd}(A)>>>, <<<ε gd(A)>>>)
while the true difference is about
m4_mathdot(<<<2\lvert w\rvert \mathrm{sech}\  A>>>, <<<2| w| sech A>>>)
The subtraction therefore loses about

m4_displaymath(
<<<\log_{10}\frac{\mathrm{gd}(A)}{2\lvert w\rvert \mathrm{sech}\  A}>>>,
<<<log_(10)gd(A)/(2| w| sech A)>>>)

digits. Every factor of 10 decrease in
m4_math(<<<\lvert w\rvert>>>, <<<| w|>>>)
costs one more digit. Once
m4_mathcomma(<<<\lvert w\rvert \lesssim \varepsilon\ \mathrm{gd}(A)\cosh(A)/2>>>, <<<| w| ≲ ε gd(A) cosh(A)/2>>>)
the difference is entirely noise. Since
m4_mathcomma(<<<w = \pi z/(2K')>>>, <<<w = π z/(2K')>>>)
this is the case of small
m4_mathdot(<<<\lvert z\rvert>>>, <<<| z|>>>)

@node Where the tan^-1 Form Fails
@ifinfo
@section Where the tan⁻¹ Form Fails
@end ifinfo
@ifhtml
@section Where the @math{\tan^{-1}} Form Fails
@end ifhtml
@iftex
@section Where the tan@sup{-1} Form Fails
@end iftex

Outside the region where the tests hold, principal
m4_math(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)
can return the wrong branch. For
m4_mathcomma(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
m4_math(<<<\zeta = \sinh w / \cosh(cn)>>>, <<<ζ = sinh w / cosh(cn)>>>)
reaches
m4_math(<<<\pm i[1, \infty)>>>, <<<± i[1, ∞)>>>)
on the lines
m4_math(<<<\mathrm{Im}\  w = \pi/2 + k\pi>>>, <<<Im w = π/2 + kπ>>>)
with
m4_mathcomma(<<<\lvert\mathrm{Re}\  w\rvert \ge cn>>>, <<<|Re w| ≥ cn>>>)
that is on

m4_displaymath(
<<<\mathrm{Im}\  z = (2k + 1)K', \qquad \lvert\mathrm{Re}\  z\rvert \ge 2nK.>>>,
<<<Im z = (2k + 1)K', |Re z| ≥ 2nK.>>>)

am has no cuts there. At
m4_mathcomma(<<<m = 0.5>>>, <<<m = 0.5>>>)
using the
m4_math(<<<\tan^{-1}>>>, <<<tan⁻¹>>>)
form for every term gave relative errors of
m4_math(<<<1.24>>>, <<<1.24>>>)
at
m4_math(<<<z = 5 + 2.5iK'>>>, <<<z = 5 + 2.5iK'>>>)
and
m4_math(<<<1.57>>>, <<<1.57>>>)
at
m4_mathdot(<<<z = 9 + 1.5iK'>>>, <<<z = 9 + 1.5iK'>>>)
In that region
m4_math(<<<\lvert w\rvert>>>, <<<| w|>>>)
is not small, so the gd difference loses little precision and is used
instead.

@node The Algorithm in bf-jacobi-am
@chapter The Algorithm in bf-jacobi-am

With
m4_mathcomma(<<<K = K(m)>>>, <<<K = K(m)>>>)
m4_mathcomma(<<<K' = K(1 - m)>>>, <<<K' = K(1 − m)>>>)
m4_math(<<<c = \pi K/K'>>>, <<<c = π K/K'>>>)
and
m4_mathpunc(<<<:>>>, <<<w = cz/(2K) = \pi z/(2K')>>>, <<<w = cz/(2K) = π z/(2K')>>>)

@enumerate
@item
m4_mathdot(<<<m = 1>>>, <<<m = 1>>>)
m4_math(<<<\mathrm{am}(z|1) = \mathrm{gd}(z)>>>, <<<am(z|1) = gd(z)>>>)
(DLMF 22.16.5).
m4_math(<<<K(1)>>>, <<<K(1)>>>)
is infinite, so the series cannot be used.
@item
m4_math(<<<1 - m>>>, <<<1 − m>>>)
@strong{rounds to}
m4_math(<<<1>>>, <<<1>>>)
(@c
m4_math(<<<\lvert m\rvert>>>, <<<| m|>>>)
below about
m4_math(<<<1.1 \times 10^{-16}>>>, <<<1.1 × 10⁻¹⁶>>>)
for doubles, or about
m4_math(<<<2^{-\text{fpprec}}>>>, <<<2^(−fpprec)>>>)
for bigfloats, with fpprec in bits).
m4_math(<<<K' = K(1)>>>, <<<K' = K(1)>>>)
cannot be computed. Use the nome series (@inlinefmtifelse{tex, @ref{The
Nome Series, 6.3, The Nome Series}, @ref{The Nome Series, 6.3}}) with
m4_mathcomma(<<<q = m/16>>>, <<<q = m/16>>>)
which is the nome to working precision.
@item
@strong{Otherwise}, reduce
m4_math(<<<z>>>, <<<z>>>)
by a multiple of
m4_math(<<<2K>>>, <<<2K>>>)
(@inlinefmtifelse{tex, @ref{Reduction by 2K, 6.1, Reduction by 2K},
@ref{Reduction by 2K, 6.1}}), then use the nome series if it converges
faster than Sala's series (@inlinefmtifelse{tex, @ref{Choosing Between
the Series, 6.4, Choosing Between the Series}, @ref{Choosing Between the
Series, 6.4}}), and Sala's series otherwise.
@end enumerate

@menu
* Reduction by 2K::
* Why Re c > 0::
* The Nome Series::
* Choosing Between the Series::
@end menu

@node Reduction by 2K
@section Reduction by 2K

Sala's series is the bilateral sum
m4_mathcomma(<<<\sum_{k \in \mathbb{Z}} \mathrm{gd}(w + kc)>>>, <<<Σ_(k ∈ ℤ) gd(w + kc)>>>)
with the
m4_math(<<<k>>>, <<<k>>>)
and
m4_math(<<<-k>>>, <<<−k>>>)
terms paired. Replacing
m4_math(<<<z>>>, <<<z>>>)
by
m4_math(<<<z + 2K>>>, <<<z + 2K>>>)
replaces
m4_math(<<<w>>>, <<<w>>>)
by
m4_mathcomma(<<<w + c>>>, <<<w + c>>>)
which reindexes the sum. The truncated sum
m4_math(<<<\sum_{k=-N}^{N}>>>, <<<Σ_(k=−N)^N>>>)
gains
m4_mathcomma(<<<\mathrm{gd}(w + (N+1)c) - \mathrm{gd}(w - Nc)>>>, <<<gd(w + (N+1)c) − gd(w − Nc)>>>)
which tends to
m4_math(<<<\mathrm{gd}(+\infty) - \mathrm{gd}(-\infty) = \pi>>>, <<<gd(+∞) − gd(−∞) = π>>>)
because
m4_mathdot(<<<\mathrm{Re}\  c > 0>>>, <<<Re c > 0>>>)
So

m4_displaymath(
<<<\mathrm{am}(z + 2K | m) = \mathrm{am}(z | m) + \pi>>>,
<<<am(z + 2K | m) = am(z | m) + π>>>)

for every
m4_math(<<<m>>>, <<<m>>>)
other than
m4_math(<<<0>>>, <<<0>>>)
and
m4_mathcomma(<<<1>>>, <<<1>>>)
including complex
m4_mathcomma(<<<m>>>, <<<m>>>)
and with the same branch cuts as the series. For the nome series the
same shift changes
m4_math(<<<t>>>, <<<t>>>)
by
m4_math(<<<2\pi>>>, <<<2π>>>)
and
m4_math(<<<t/2>>>, <<<t/2>>>)
by
m4_mathcomma(<<<\pi>>>, <<<π>>>)
so the identity holds there too.

@code{bf-jacobi-am} computes
m4_math(<<<\mathrm{am}(z - 2rK) + r\pi>>>, <<<am(z − 2rK) + rπ>>>)
with

m4_displaymath(
<<<r = \mathrm{truncate}\left(\frac{\mathrm{Re}\  w}{\mathrm{Re}\  c}\right),>>>,
<<<r = truncate((Re w)/(Re c)),>>>)

so the reduced
m4_math(<<<w>>>, <<<w>>>)
has
m4_mathdot(<<<\lvert\mathrm{Re}\  w\rvert < \mathrm{Re}\  c>>>, <<<|Re w| < Re c>>>)
Then:

@itemize @bullet
@item
m4_math(<<<\sinh>>>, <<<sinh>>>)
and
m4_math(<<<\cosh>>>, <<<cosh>>>)
never overflow in double precision. Without the reduction,
m4_math(<<<\mathrm{gd}(A + w)>>>, <<<gd(A + w)>>>)
needs
m4_math(<<<\sinh>>>, <<<sinh>>>)
at up to about
m4_mathcomma(<<<2\lvert\mathrm{Re}\  w\rvert>>>, <<<2|Re w|>>>)
which overflows for
m4_math(<<<z \gtrsim 420>>>, <<<z ≳ 420>>>)
at
m4_mathdot(<<<m = 0.5>>>, <<<m = 0.5>>>)
@item
The multiple of
m4_math(<<<\pi>>>, <<<π>>>)
is added once, instead of accumulating over
m4_math(<<<r>>>, <<<r>>>)
terms each close to
m4_mathdot(<<<\pi>>>, <<<π>>>)
@item
Fewer terms are needed. Before the series' terms start to decay,
m4_math(<<<n>>>, <<<n>>>)
must reach about
m4_mathcomma(<<<\lvert w\rvert/\lvert c\rvert>>>, <<<| w|/| c|>>>)
which is at most about
m4_math(<<<1>>>, <<<1>>>)
after the reduction. For
m4_mathcomma(<<<z = 100>>>, <<<z = 100>>>)
m4_math(<<<m = 0.5>>>, <<<m = 0.5>>>)
in double precision this is 13 terms instead of 38.
@end itemize

@menu
* Truncation Not Rounding::
* Effect on Accuracy::
@end menu

@node Truncation Not Rounding
@subsection Truncation, Not Rounding

Truncating toward zero keeps the reduced
m4_math(<<<\mathrm{Re}\  w>>>, <<<Re w>>>)
the same sign as the original, so the reduced am and
m4_math(<<<r\pi>>>, <<<rπ>>>)
have the same sign and their sum does not cancel. Rounding can flip the
sign. For example, at
m4_mathcomma(<<<z = 1.5>>>, <<<z = 1.5>>>)
m4_mathcomma(<<<m = 1.5 + i>>>, <<<m = 1.5 + i>>>)
m4_mathpunc(<<<:>>>, <<<\mathrm{Re}\  w / \mathrm{Re}\  c = 0.58>>>, <<<Re w / Re c = 0.58>>>)
rounding gives
m4_math(<<<r = 1>>>, <<<r = 1>>>)
and
m4_mathcomma(<<<\mathrm{am} = \pi + (\approx -2.21)>>>, <<<am = π + (≈ −2.21)>>>)
and the result was 10 ulps off; truncating gives
m4_mathcomma(<<<r = 0>>>, <<<r = 0>>>)
3 ulps off, the same as without reduction.

@node Effect on Accuracy
@subsection Effect on Accuracy

The reduced argument
m4_math(<<<z/(2K) - r>>>, <<<z/(2K) − r>>>)
is exact by Sterbenz's lemma, so the reduction adds no rounding error
beyond the existing error in
m4_mathdot(<<<z/(2K)>>>, <<<z/(2K)>>>)
That error is about
m4_math(<<<\lvert z/(2K)\rvert\ \varepsilon>>>, <<<| z/(2K)| ε>>>)
absolute, which makes am wrong by about
m4_mathcomma(<<<\lvert\mathrm{am}\rvert\ \varepsilon>>>, <<<|am| ε>>>)
a few ulps. @inlinefmtifelse{tex, @ref{Expected Error for Large |z|,
Section 7.1, Expected Error for Large |z|}, @ref{Expected Error for
Large |z|, Section 7.1}} has examples.

@node Why Re c > 0
@section Why Re c > 0

Let
m4_mathcomma(<<<\tau = iK'/K>>>, <<<τ = iK'/K>>>)
so that the nome is
m4_mathdot(<<<q = e^{i\pi\tau}>>>, <<<q = e^(iπτ)>>>)
Then
m4_math(<<<c = \pi K/K' = i\pi/\tau>>>, <<<c = π K/K' = iπ/τ>>>)
and

m4_displaymath(
<<<\mathrm{Re}\  c = \frac{\pi \mathrm{Im}\  \tau}{\lvert\tau\rvert^2}.>>>,
<<<Re c = (π Im τ)/|τ|².>>>)

So
m4_math(<<<\mathrm{Re}\  c > 0>>>, <<<Re c > 0>>>)
exactly when
m4_mathcomma(<<<\mathrm{Im}\  \tau > 0>>>, <<<Im τ > 0>>>)
i.e.@:
m4_mathdot(<<<\lvert q\rvert < 1>>>, <<<| q| < 1>>>)

@itemize @bullet
@item
Complex
m4_math(<<<m>>>, <<<m>>>)
off the cuts
m4_math(<<<(-\infty, 0]>>>, <<<(−∞, 0]>>>)
and
m4_mathpunc(<<<:>>>, <<<[1, \infty)>>>, <<<[1, ∞)>>>)
m4_math(<<<\mathrm{Im}\  \tau > 0>>>, <<<Im τ > 0>>>)
is the standard property of the map from
m4_math(<<<m>>>, <<<m>>>)
to
m4_mathdot(<<<\tau>>>, <<<τ>>>)
@item
Real
m4_mathpunc(<<<:>>>, <<<m < 0>>>, <<<m < 0>>>)
m4_math(<<<K>>>, <<<K>>>)
is real and positive, and
m4_math(<<<\mathrm{Im}\  \tau = \mathrm{Re}\  K' / K>>>, <<<Im τ = Re K' / K>>>)
with
m4_math(<<<\mathrm{Re}\  K(x) > 0>>>, <<<Re K(x) > 0>>>)
for real
m4_math(<<<x > 1>>>, <<<x > 1>>>)
on either side of its cut.
@item
Real
m4_mathpunc(<<<:>>>, <<<m > 1>>>, <<<m > 1>>>)
m4_math(<<<K'>>>, <<<K'>>>)
is real and positive, and
m4_math(<<<\mathrm{Im}\  \tau = K' \mathrm{Re}\  K / \lvert K\rvert^2>>>, <<<Im τ = K' Re K / | K|²>>>)
with
m4_mathdot(<<<\mathrm{Re}\  K(m) = K(1/m)/\sqrt{m} > 0>>>, <<<Re K(m) = K(1/m)/sqrt(m) > 0>>>)
@item
m4_mathpunc(<<<:>>>, <<<0 < m < 1>>>, <<<0 < m < 1>>>)
m4_math(<<<c>>>, <<<c>>>)
is real and positive.
@end itemize

So the reduction, which divides by
m4_mathcomma(<<<\mathrm{Re}\  c>>>, <<<Re c>>>)
and the convergence of Sala's series are safe for all
m4_math(<<<m>>>, <<<m>>>)
except
m4_math(<<<0>>>, <<<0>>>)
and
m4_mathdot(<<<1>>>, <<<1>>>)
m4_math(<<<\mathrm{Re}\  c \to 0>>>, <<<Re c → 0>>>)
as
m4_mathcomma(<<<m \to 0>>>, <<<m → 0>>>)
because
m4_math(<<<K'>>>, <<<K'>>>)
grows like
m4_mathpunc(<<<;>>>, <<<\frac12 \ln(16/m)>>>, <<<1/2 ln(16/m)>>>)
then Sala's series converges slowly, which is what the nome series is
for.

@node The Nome Series
@section The Nome Series

For
m4_math(<<<\lvert\mathrm{Im}\  z\rvert < K'>>>, <<<|Im z| < K'>>>)
(DLMF 22.16.9; functions.wolfram.com 09.24.06.0002.01):

m4_displaymath(
<<<\mathrm{am}(z|m) = \frac{t}{2} + 2\sum_{n \ge 1} \frac{q^n}{n(1 + q^{2n})} \sin(nt),>>>,
<<<am(z|m) = t/2 + 2Σ_(n ≥ 1) qⁿ/(n(1 + q²ⁿ)) sin(nt),>>>)

with
m4_math(<<<t = \pi z/K>>>, <<<t = π z/K>>>)
and
m4_mathcomma(<<<q = \exp(-\pi K'/K)>>>, <<<q = exp(−π K'/K)>>>)
computed from the
m4_math(<<<K>>>, <<<K>>>)
and
m4_math(<<<K'>>>, <<<K'>>>)
already needed for the reduction. This
m4_math(<<<q>>>, <<<q>>>)
uses the same
m4_math(<<<K'>>>, <<<K'>>>)
as Sala's series, so the two agree for complex
m4_mathdot(<<<m>>>, <<<m>>>)
For small
m4_mathcomma(<<<m>>>, <<<m>>>)
m4_math(<<<K' = K(1 - m)>>>, <<<K' = K(1 − m)>>>)
is computed from a rounded
m4_mathcomma(<<<1 - m>>>, <<<1 − m>>>)
but
m4_mathcomma(<<<q \approx m/16>>>, <<<q ≈ m/16>>>)
and an absolute error of about
m4_math(<<<\varepsilon>>>, <<<ε>>>)
in the effective
m4_math(<<<m>>>, <<<m>>>)
changes
m4_math(<<<q>>>, <<<q>>>)
by only about
m4_mathdot(<<<\varepsilon/16>>>, <<<ε/16>>>)

Since
m4_mathcomma(<<<\lvert\sin(nt)\rvert \le e^{n\lvert\mathrm{Im}\  t\rvert}>>>, <<<|sin(nt)| ≤ e^(n|Im t|)>>>)
the
m4_math(<<<n>>>, <<<n>>>)
-th term is bounded by
m4_math(<<<2\rho^n/n>>>, <<<2ρⁿ/n>>>)
with

m4_displaymath(
<<<\rho = \lvert q\rvert\  e^{\lvert\mathrm{Im}\  t\rvert} = \lvert q\rvert \exp\left(\pi \left\lvert\mathrm{Im}\  \frac{z}{K}\right\rvert\right).>>>,
<<<ρ = | q| e^(|Im t|) = | q| exp(π |Im (z/K)|).>>>)

The series converges for
m4_mathcomma(<<<\rho < 1>>>, <<<ρ < 1>>>)
which is the strip
m4_mathcomma(<<<\lvert\mathrm{Im}\  z\rvert < K'>>>, <<<|Im z| < K'>>>)
where am has no cuts. The loop stops when
m4_mathcomma(<<<2\rho^n/n \le \text{tol}\cdot\lvert\text{sum}\rvert>>>, <<<2ρⁿ/n ≤ tol·|sum|>>>)
not when a term is small:
m4_math(<<<\sin(nt)>>>, <<<sin(nt)>>>)
can be zero for one
m4_math(<<<n>>>, <<<n>>>)
while later terms are not. At
m4_mathcomma(<<<z = K/2>>>, <<<z = K/2>>>)
for example,
m4_math(<<<t = \pi/2>>>, <<<t = π/2>>>)
and
m4_mathcomma(<<<\sin 2t = 0>>>, <<<sin 2t = 0>>>)
so a test on the term stops at
m4_math(<<<n = 2>>>, <<<n = 2>>>)
and misses the
m4_math(<<<n = 3>>>, <<<n = 3>>>)
term.

At
m4_math(<<<z = 0>>>, <<<z = 0>>>)
the sum is
m4_math(<<<0>>>, <<<0>>>)
and the bound test never succeeds in bigfloat arithmetic, where
m4_math(<<<\rho^n>>>, <<<ρⁿ>>>)
never underflows, so
m4_math(<<<t = 0>>>, <<<t = 0>>>)
returns
m4_math(<<<0>>>, <<<0>>>)
directly. This is reachable through the reduction when
m4_math(<<<z>>>, <<<z>>>)
is an exact multiple of
m4_mathdot(<<<2K>>>, <<<2K>>>)

@node Choosing Between the Series
@section Choosing Between the Series

After the reduction, Sala's terms decay like
m4_mathcomma(<<<e^{-n\mathrm{Re}\  c}>>>, <<<e^(−nRe c)>>>)
independent of
m4_mathcomma(<<<\mathrm{Im}\  z>>>, <<<Im z>>>)
and the nome series terms like
m4_mathdot(<<<\rho^n>>>, <<<ρⁿ>>>)
@code{bf-jacobi-am} uses the nome series when
m4_mathdot(<<<\rho < e^{-\mathrm{Re}\  c}>>>, <<<ρ < e^(−Re c)>>>)
Since
m4_math(<<<\lvert q\rvert = \exp(-\pi \mathrm{Re}(K'/K))>>>, <<<| q| = exp(−π Re(K'/K))>>>)
and
m4_mathcomma(<<<\mathrm{Re}\  c = \pi \mathrm{Re}(K/K')>>>, <<<Re c = π Re(K/K')>>>)
taking logs and dividing by
m4_math(<<<\pi>>>, <<<π>>>)
gives

m4_displaymath(
<<<\left\lvert\mathrm{Im}\  \frac{z}{K}\right\rvert + \mathrm{Re}\  \frac{K}{K'} < \mathrm{Re}\  \frac{K'}{K}.>>>,
<<<|Im (z/K)| + Re (K/K') < Re (K'/K).>>>)

@itemize @bullet
@item
For real
m4_math(<<<z>>>, <<<z>>>)
and real
m4_math(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
this is
m4_mathcomma(<<<K < K'>>>, <<<K < K'>>>)
i.e.@:
m4_mathdot(<<<m < 1/2>>>, <<<m < 1/2>>>)
@item
A nonzero
m4_math(<<<\mathrm{Im}\  z>>>, <<<Im z>>>)
moves the switch toward smaller
m4_mathcomma(<<<m>>>, <<<m>>>)
since it uses up the strip where the nome series converges.
@item
m4_math(<<<\rho < 1>>>, <<<ρ < 1>>>)
is implied, so the nome series is never used where it diverges.
@item
m4_math(<<<\mathrm{Im}(z/K)>>>, <<<Im(z/K)>>>)
is unchanged by the reduction, so the test can use either
m4_mathdot(<<<z>>>, <<<z>>>)
@end itemize

Terms needed at
m4_mathcomma(<<<z = 3 + 0.5i>>>, <<<z = 3 + 0.5i>>>)
30 digits:

@raggedright
@multitable @columnfractions 0.14 0.26 0.60
@headitem
m4_math(<<<m>>>, <<<m>>>)
@tab
nome series
@tab
Sala's series (approx.)
@item
m4_math(<<<10^{-3}>>>, <<<10⁻³>>>)
@tab
8
@tab
62
@item
m4_math(<<<10^{-10}>>>, <<<10⁻¹⁰>>>)
@tab
3
@tab
167
@item
m4_math(<<<10^{-20}>>>, <<<10⁻²⁰>>>)
@tab
2
@tab
(@c
m4_math(<<<K(1 - m)>>>, <<<K(1 − m)>>>)
fails in double)
@item
m4_math(<<<10^{-300}>>>, <<<10⁻³⁰⁰>>>)
@tab
1
@tab

@end multitable
@end raggedright

@node Accuracy - Expected Errors and Possible Improvements
@chapter Accuracy: Expected Errors and Possible Improvements

@inlinefmtifelse{tex, @ref{Real m > 1 - Reciprocal-Modulus
Transformation, Sections 7.4, Real m > 1: Reciprocal-Modulus
Transformation}, @ref{Real m > 1 - Reciprocal-Modulus Transformation,
Sections 7.4}} and @inlinefmtifelse{tex, @ref{Real m < 0 -
Negative-Parameter Transformation, 7.5, Real m < 0: Negative-Parameter
Transformation}, @ref{Real m < 0 - Negative-Parameter Transformation,
7.5}} and the
m4_math(<<<\sinh^{-1}>>>, <<<sinh⁻¹>>>)
form of @inlinefmtifelse{tex, @ref{The Real and Imaginary Parts of gd,
section 4, The Real and Imaginary Parts of gd}, @ref{The Real and
Imaginary Parts of gd, section 4}} are not implemented.

@menu
* Expected Error for Large |z|::
* Small m - Before and After the Nome Series::
* Expected Error in Tests of sin(am) Against sn::
* Real m > 1 - Reciprocal-Modulus Transformation::
* Real m < 0 - Negative-Parameter Transformation::
@end menu

@node Expected Error for Large |z|
@section Expected Error for Large |z|

Without the reduction of @inlinefmtifelse{tex, @ref{Reduction by 2K,
6.1, Reduction by 2K}, @ref{Reduction by 2K, 6.1}}, for real
m4_math(<<<z>>>, <<<z>>>)
with
m4_math(<<<A = cn < w>>>, <<<A = cn < w>>>)
each term
m4_math(<<<\mathrm{gd}(A + w) - \mathrm{gd}(A - w)>>>, <<<gd(A + w) − gd(A − w)>>>)
is close to
m4_mathcomma(<<<\pi>>>, <<<π>>>)
and the sum grows to about
m4_mathdot(<<<\lvert\mathrm{am}\rvert>>>, <<<|am|>>>)
Each addition rounds at half an ulp of the running sum. At
m4_mathcomma(<<<z = 100>>>, <<<z = 100>>>)
m4_math(<<<m = 0.5>>>, <<<m = 0.5>>>)
the correctly rounded value is 84.70311272411382 (exact
84.70311272411382440…); the unreduced series gave 84.70311272411385, 2
ulps high.

With the reduction, Maxima under SBCL in double precision, against
60-digit references at the same inputs (@c
m4_mathpunc(<<<):>>>, <<<m = 0.5>>>, <<<m = 0.5>>>)

@raggedright
@multitable @columnfractions 0.13 0.58 0.29
@headitem
m4_math(<<<z>>>, <<<z>>>)
@tab
result
@tab
error
@item
100
@tab
84.70311@/27241138@/4
@tab
+0.84 ulp
@item
1000
@tab
847.1374@/03821326@/3
@tab
−0.56 ulp
@item
3000
@tab
2541.655@/52656463@/02
@tab
−0.47 ulp
@end multitable
@end raggedright

The unreduced series overflows for
m4_mathdot(<<<z \gtrsim 420>>>, <<<z ≳ 420>>>)

@node Small m - Before and After the Nome Series
@section Small m: Before and After the Nome Series

Maxima under SBCL, with the previous @code{bf-jacobi-am} (Sala's series
only) and the current one. References are 50-digit values at the same
inputs; times are per call.

Double precision:

@raggedright
@multitable @columnfractions 0.15 0.26 0.26 0.33
@headitem
call
@tab
correct value
@tab
before
@tab
after
@item
@code{jacobi_am(0.5, 1e-20)}
@tab
0.5
@tab
error: elliptic_kc(1) is undefined
@tab
0.5
@item
@code{jacobi_am(10.0, 1e-3)}
@tab
9.997613@/13982617@/5…
@tab
9.997613@/13982616@/8 (4 ulps), 84 µs
@tab
9.997613@/13982617@/5 (correctly rounded), 56 µs
@item
@code{jacobi_am(3.0, 1e-10)}
@tab
2.999999@/99992150@/731…
@tab
2.999999@/99992150@/8 (1.6 ulps), 144 µs
@tab
2.999999@/99992150@/72 (correctly rounded), 66 µs
@item
@code{jacobi_am(0.5, 1e-15)}
@tab
0.499999@/99999999@/998… (rounds to 0.5)
@tab
0.5, 132 µs
@tab
0.500000@/00000000@/01 (1 ulp), 48 µs
@end multitable
@end raggedright

Bigfloat with fpprec = 32:

@raggedright
@multitable @columnfractions 0.12 0.18 0.35 0.35
@headitem
call
@tab
correct value to 32 digits
@tab
before
@tab
after
@item
@code{jacobi_am(3b0, 1b-30)}
@tab
2.999999@/99999999@/99999999@/99999999@/2
@tab
2.999999@/99999999@/99999999@/99999999@/6 (4 in the last digit), 40.9 ms
@tab
2.999999@/99999999@/99999999@/99999999@/2, 1.1 ms
@item
@code{jacobi_am(10b0, 1b-3)}
@tab
9.997613@/13982617@/54702124@/30692094@/7
@tab
9.997613@/13982617@/54702124@/30692095@/1 (4 in the last digit), 6.4 ms
@tab
9.997613@/13982617@/54702124@/30692094@/8 (1 in the last digit), 1.2 ms
@item
@code{jacobi_am(3b0, 1b-40)}
@tab
3.0
@tab
error: elliptic_kc(1) is undefined
@tab
3.0
@end multitable
@end raggedright

The gains come from three places:

@itemize @bullet
@item
@strong{Tiny}
m4_math(<<<m>>>, <<<m>>>)
@strong{no longer fails.} When
m4_math(<<<1 - m>>>, <<<1 − m>>>)
rounds to
m4_mathcomma(<<<1>>>, <<<1>>>)
the previous code called
m4_mathdot(<<<K(1)>>>, <<<K(1)>>>)
@item
@strong{Fewer terms.} For small
m4_mathcomma(<<<m>>>, <<<m>>>)
Sala's terms decay like
m4_math(<<<e^{-n\mathrm{Re}\  c}>>>, <<<e^(−nRe c)>>>)
with
m4_math(<<<\mathrm{Re}\  c \approx 0.38>>>, <<<Re c ≈ 0.38>>>)
at
m4_mathcomma(<<<m = 10^{-10}>>>, <<<m = 10⁻¹⁰>>>)
while the nome series decays like
m4_math(<<<q^n>>>, <<<qⁿ>>>)
with
m4_mathdot(<<<q \approx m/16>>>, <<<q ≈ m/16>>>)
@item
@strong{Less accumulated rounding at larger}
m4_mathdot(<<<z>>>, <<<z>>>)
At
m4_math(<<<z = 10>>>, <<<z = 10>>>)
the previous code summed about four terms of size
m4_mathpunc(<<<;>>>, <<<\pi>>>, <<<π>>>)
the current code reduces
m4_math(<<<z>>>, <<<z>>>)
and adds
m4_math(<<<4\pi>>>, <<<4π>>>)
once.
@end itemize

It is not correctly rounded everywhere: at @code{jacobi_am(0.5, 1e-15)}
the rounding in computing
m4_math(<<<t = \pi z/K>>>, <<<t = π z/K>>>)
leaves the result 1 ulp high.

@node Expected Error in Tests of sin(am) Against sn
@section Expected Error in Tests of sin(am) Against sn

Even with am correct to a few ulps of the computation of
m4_mathcomma(<<<w>>>, <<<w>>>)
two factors enlarge
m4_mathpunc(<<<:>>>, <<<\lvert\sin(\mathrm{am}) - \mathrm{sn}\rvert>>>, <<<|sin(am) − sn|>>>)

@itemize @bullet
@item
The series computes
m4_mathcomma(<<<w = \pi z/(2K')>>>, <<<w = π z/(2K')>>>)
whose relative error is about
m4_mathdot(<<<\varepsilon>>>, <<<ε>>>)
Since
m4_mathcomma(<<<d\mathrm{am}/dz = \mathrm{dn}(z)>>>, <<<dam/dz = dn(z)>>>)
this gives an error in am of about
m4_mathdot(<<<\lvert z\rvert\ \lvert\mathrm{dn}(z)\rvert\ \varepsilon>>>, <<<| z| |dn(z)| ε>>>)
Any method that scales
m4_math(<<<z>>>, <<<z>>>)
by a period has this error.
@item
m4_math(<<<\sin(\mathrm{am})>>>, <<<sin(am)>>>)
changes with am at rate
m4_mathdot(<<<\lvert\cos\mathrm{am}\rvert = \lvert\mathrm{cn}(z)\rvert>>>, <<<|cosam| = |cn(z)|>>>)
@end itemize

So

m4_displaymath(
<<<\lvert\sin(\mathrm{am}) - \mathrm{sn}\rvert \lesssim \lvert\mathrm{cn}\rvert\ \lvert\mathrm{dn}\rvert\ \lvert z\rvert\ \varepsilon + \lvert\mathrm{am}\rvert\ \lvert\mathrm{cn}\rvert\ \varepsilon,>>>,
<<<|sin(am) − sn| ≲ |cn| |dn| | z| ε + |am| |cn| ε,>>>)

the second term from the rounding of am itself. Both
m4_math(<<<\lvert\mathrm{cn}\rvert>>>, <<<|cn|>>>)
and
m4_math(<<<\lvert\mathrm{dn}\rvert>>>, <<<|dn|>>>)
become large near the poles of sn.

Example:
m4_mathcomma(<<<z = 2ki>>>, <<<z = 2ki>>>)
m4_mathcomma(<<<m = 0.75>>>, <<<m = 0.75>>>)
at about 106 bits (fpprec 32,
m4_mathpunc(<<<).>>>, <<<\varepsilon \approx 1.2 \times 10^{-32}>>>, <<<ε ≈ 1.2 × 10⁻³²>>>)
The poles of sn on the imaginary axis are at
m4_math(<<<i(2s + 1)K' = 1.69i,\ 5.06i,\ 8.43i, \ldots>>>, <<<i(2s + 1)K' = 1.69i, 5.06i, 8.43i, …>>>)

@raggedright
@multitable @columnfractions 0.03 0.10 0.25 0.28 0.35
@headitem
m4_math(<<<k>>>, <<<k>>>)
@tab
m4_math(<<<\lvert\mathrm{sn}\rvert>>>, <<<|sn|>>>)
@tab
am
@tab
am error
@tab
m4_math(<<<\lvert\sin(\mathrm{am}) - \mathrm{sn}\rvert>>>, <<<|sin(am) − sn|>>>)
@item
1
@tab
3.57
@tab
m4_math(<<<\pi + 1.984i>>>, <<<π + 1.984i>>>)
@tab
m4_math(<<<1.3 \times 10^{-31}>>>, <<<1.3 × 10⁻³¹>>>)
@tab
m4_math(<<<4.7 \times 10^{-31}>>>, <<<4.7 × 10⁻³¹>>>)
@item
4
@tab
2.55
@tab
m4_math(<<<1.665i>>>, <<<1.665i>>>)
@tab
m4_math(<<<2.3 \times 10^{-31}>>>, <<<2.3 × 10⁻³¹>>>)
@tab
m4_math(<<<6.2 \times 10^{-31}>>>, <<<6.2 × 10⁻³¹>>>)
@item
6
@tab
5.71
@tab
m4_math(<<<-2.444i>>>, <<<−2.444i>>>)
@tab
m4_math(<<<4.1 \times 10^{-31}>>>, <<<4.1 × 10⁻³¹>>>)
@tab
m4_math(<<<2.4 \times 10^{-30}>>>, <<<2.4 × 10⁻³⁰>>>)
@item
9
@tab
1.94
@tab
m4_math(<<<\pi - 1.416i>>>, <<<π − 1.416i>>>)
@tab
m4_math(<<<9.6 \times 10^{-31}>>>, <<<9.6 × 10⁻³¹>>>)
@tab
m4_math(<<<2.1 \times 10^{-30}>>>, <<<2.1 × 10⁻³⁰>>>)
@item
5
@tab
0.11
@tab
m4_math(<<<\pi + 0.115i>>>, <<<π + 0.115i>>>)
@tab
m4_math(<<<2.7 \times 10^{-32}>>>, <<<2.7 × 10⁻³²>>>)
@tab
m4_math(<<<2.7 \times 10^{-32}>>>, <<<2.7 × 10⁻³²>>>)
@end multitable
@end raggedright

At
m4_mathcomma(<<<k = 6>>>, <<<k = 6>>>)
m4_mathdot(<<<\lvert\mathrm{cn}\rvert\ \lvert\mathrm{dn}\rvert\ \lvert z\rvert\ \varepsilon \approx 4 \times 10^{-30}>>>, <<<|cn| |dn| | z| ε ≈ 4 × 10⁻³⁰>>>)
The rtest_elliptic tolerance for this test was raised from 5.5b−32 to
3.2b−30 for this reason. A relative test, @code{closeto((sin(am) -
sn)/sn, ...)}, would need less slack because the test values reach
m4_mathdot(<<<\lvert\mathrm{sn}\rvert \approx 5.7>>>, <<<|sn| ≈ 5.7>>>)

@node Real m > 1 - Reciprocal-Modulus Transformation
@section Real m > 1: Reciprocal-Modulus Transformation

For
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
m4_math(<<<K(m)>>>, <<<K(m)>>>)
is complex, so
m4_math(<<<c>>>, <<<c>>>)
is complex and a real
m4_math(<<<z>>>, <<<z>>>)
gives a result with a spurious imaginary part of order
m4_mathdot(<<<\varepsilon>>>, <<<ε>>>)
For example,
m4_math(<<<\mathrm{am}(16 | 2.5) = 24.878832226728505 - 1.95 \times 10^{-15}\ i>>>, <<<am(16 | 2.5) = 24.878832226728505 − 1.95 × 10⁻¹⁵ i>>>)
in double precision, while the true imaginary part is about
m4_mathdot(<<<3 \times 10^{-41}>>>, <<<3 × 10⁻⁴¹>>>)

A&S 16.11 maps
m4_math(<<<m > 1>>>, <<<m > 1>>>)
to
m4_mathdot(<<<\mu = 1/m < 1>>>, <<<μ = 1/m < 1>>>)
With
m4_mathcomma(<<<v = \sqrt{m}\ u>>>, <<<v = sqrt(m) u>>>)

m4_displaymath(
<<<\mathrm{sn}(u|m) = \sqrt{\mu}\ \mathrm{sn}(v|\mu), \qquad \mathrm{cn}(u|m) = \mathrm{dn}(v|\mu),>>>,
<<<sn(u|m) = sqrt(μ) sn(v|μ), cn(u|m) = dn(v|μ),>>>)

so
m4_mathcomma(<<<e^{i\mathrm{am}(u|m)} = \mathrm{cn}(u|m) + i\mathrm{sn}(u|m) = \mathrm{dn}(v|\mu) + i\sqrt{\mu}\ \mathrm{sn}(v|\mu)>>>, <<<e^(iam(u|m)) = cn(u|m) + isn(u|m) = dn(v|μ) + isqrt(μ) sn(v|μ)>>>)
and

m4_displaymath(
<<<\mathrm{am}(u|m) = -i\log\left(\mathrm{dn}(v|\mu) + i\sqrt{\mu}\ \mathrm{sn}(v|\mu)\right) + 2\pi N,
\qquad N = \mathrm{round}\left(\frac{\mathrm{Re}\  v}{4K(\mu)}\right).>>>,
<<<@example
am(u|m) = −i log(dn(v|μ) + isqrt(μ) sn(v|μ)) + 2π N,
N = round((Re v)/4K(μ)).
@end example>>>)

@menu
* The 2π N Term::
* Check::
* Advantages::
* Implementation Notes::
@end menu

@node The 2π N Term
@ifinfo
@subsection The 2π N Term
@end ifinfo
@ifhtml
@subsection The @math{2\pi N} Term
@end ifhtml
@iftex
@subsection The 2π N Term
@end iftex

The log gives am only modulo
m4_mathcomma(<<<2\pi>>>, <<<2π>>>)
and its principal value is the branch that is continuous along the real
axis. For
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
Mathematica's cuts for am cross the real axis at
m4_mathcomma(<<<u = 2(2s + 1)K(\mu)/\sqrt{m}>>>, <<<u = 2(2s + 1)K(μ)/sqrt(m)>>>)
i.e.@: at
m4_mathcomma(<<<v = 2(2s + 1)K(\mu)>>>, <<<v = 2(2s + 1)K(μ)>>>)
and the series jumps by
m4_math(<<<2\pi>>>, <<<2π>>>)
at each crossing.
m4_math(<<<N>>>, <<<N>>>)
counts the crossings between
m4_math(<<<0>>>, <<<0>>>)
and
m4_mathdot(<<<\mathrm{Re}\  v>>>, <<<Re v>>>)

Example:
m4_mathcomma(<<<u = 16>>>, <<<u = 16>>>)
m4_mathdot(<<<m = 2.5>>>, <<<m = 2.5>>>)
The principal log gives
m4_mathcomma(<<<-0.253909001989843>>>, <<<−0.253909001989843>>>)
which is also the integral of dn along the real axis.
m4_mathcomma(<<<N = 4>>>, <<<N = 4>>>)
and
m4_mathcomma(<<<-0.253909001989843 + 8\pi = 24.8788322267285>>>, <<<−0.253909001989843 + 8π = 24.8788322267285>>>)
the series value.

@node Check
@subsection Check

400 random points with real
m4_math(<<<m \in [1.05, 20]>>>, <<<m ∈ [1.05, 20]>>>)
and
m4_mathcomma(<<<u \in [-15, 15]>>>, <<<u ∈ [−15, 15]>>>)
half with
m4_mathcomma(<<<\mathrm{Im}\  u \in [-4, 4]>>>, <<<Im u ∈ [−4, 4]>>>)
at 25 digits: the principal log alone differed from the series at 354
points, always by a multiple of
m4_mathpunc(<<<;>>>, <<<2\pi>>>, <<<2π>>>)
with the
m4_math(<<<2\pi N>>>, <<<2π N>>>)
term all 400 agreed.

@node Advantages
@subsection Advantages

m4_math(<<<K(\mu)>>>, <<<K(μ)>>>)
and
m4_math(<<<K'(\mu)>>>, <<<K'(μ)>>>)
are real, so real
m4_math(<<<u>>>, <<<u>>>)
gives an exactly real result, and the existing sn and dn code replaces
the series.

@node Implementation Notes
@subsection Implementation Notes

@itemize @bullet
@item
Real
m4_mathpunc(<<<:>>>, <<<u>>>, <<<u>>>)
m4_mathdot(<<<\mathrm{am} = \mathrm{atan2}(\sqrt{\mu}\ \mathrm{sn}(v|\mu), \mathrm{dn}(v|\mu)) + 2\pi N>>>, <<<am = atan2(sqrt(μ) sn(v|μ), dn(v|μ)) + 2π N>>>)
Since
m4_mathcomma(<<<\mathrm{dn}(v|\mu) > 0>>>, <<<dn(v|μ) > 0>>>)
the atan2 gives the continuous part directly.
@item
Complex
m4_mathpunc(<<<:>>>, <<<u>>>, <<<u>>>)
compute the real part of the log as an atan2 and the imaginary part,
m4_mathcomma(<<<-\log\lvert\mathrm{dn} + i\sqrt{\mu}\ \mathrm{sn}\rvert>>>, <<<−log|dn + isqrt(μ) sn|>>>)
with log1p, so that small am keeps its relative accuracy.
@item
Complex
m4_mathpunc(<<<:>>>, <<<m>>>, <<<m>>>)
m4_math(<<<K(\mu)>>>, <<<K(μ)>>>)
is complex, and whether
m4_math(<<<\mathrm{round}(\mathrm{Re}\  v / 4K(\mu))>>>, <<<round(Re v / 4K(μ))>>>)
is still the right crossing count has not been checked.
@end itemize

@node Real m < 0 - Negative-Parameter Transformation
@section Real m < 0: Negative-Parameter Transformation

A&S 16.10 maps
m4_math(<<<m < 0>>>, <<<m < 0>>>)
to
m4_mathdot(<<<\mu = -m/(1 - m) \in (0, 1)>>>, <<<μ = −m/(1 − m) ∈ (0, 1)>>>)
With
m4_mathcomma(<<<v = u\sqrt{1 - m}>>>, <<<v = usqrt(1 − m)>>>)

m4_displaymath(
<<<\mathrm{sn}(u|m) = \frac{\mathrm{sd}(v|\mu)}{\sqrt{1 - m}}, \qquad \mathrm{cn}(u|m) = \mathrm{cd}(v|\mu),>>>,
<<<sn(u|m) = sd(v|μ)/(sqrt(1 − m)), cn(u|m) = cd(v|μ),>>>)

so

m4_displaymath(
<<<e^{i\mathrm{am}(u|m)} = \frac{\mathrm{cn}(v|\mu) + i\mathrm{sn}(v|\mu)/\sqrt{1 - m}}{\mathrm{dn}(v|\mu)}.>>>,
<<<e^(iam(u|m)) = (cn(v|μ) + isn(v|μ)/sqrt(1 − m))/dn(v|μ).>>>)

For
m4_mathcomma(<<<m < 0>>>, <<<m < 0>>>)
m4_math(<<<K' = K(1 - m)>>>, <<<K' = K(1 − m)>>>)
is complex, which caused the branch issue in the
m4_math(<<<m = -6>>>, <<<m = −6>>>)
example of @inlinefmtifelse{tex, @ref{The Difference gd(A + w) − gd(A −
w), section 5, The Difference gd(A + w) − gd(A − w)}, @ref{The
Difference gd(A + w) − gd(A − w), section 5}}. This transformation keeps
everything real for real
m4_mathdot(<<<u>>>, <<<u>>>)
The
m4_math(<<<2\pi>>>, <<<2π>>>)
correction for complex
m4_mathcomma(<<<u>>>, <<<u>>>)
the analogue of
m4_math(<<<N>>>, <<<N>>>)
in @inlinefmtifelse{tex, @ref{Real m > 1 - Reciprocal-Modulus
Transformation, 7.4, Real m > 1: Reciprocal-Modulus Transformation},
@ref{Real m > 1 - Reciprocal-Modulus Transformation, 7.4}}, has not been
worked out or tested.

@node Numerical Checks
@chapter Numerical Checks


@menu
* Checks of the gd Identities with mpmath::
* Checks of the am Series with mpmath::
* Checks of bf-jacobi-am in Maxima::
@end menu

@node Checks of the gd Identities with mpmath
@section Checks of the gd Identities with mpmath

All at 30–40 digits with mpmath, using Sala's definitions above:

@itemize @bullet
@item
m4_math(<<<\mathrm{gd}(u)>>>, <<<gd(u)>>>)
vs
m4_mathpunc(<<<:>>>, <<<2\tan^{-1}(\tanh(u/2))>>>, <<<2 tan⁻¹(tanh(u/2))>>>)
20,000 random points with
m4_mathcomma(<<<\lvert\mathrm{Re}\  u\rvert \le 4>>>, <<<|Re u| ≤ 4>>>)
m4_mathpunc(<<<;>>>, <<<\lvert\mathrm{Im}\  u\rvert \le 12>>>, <<<|Im u| ≤ 12>>>)
largest difference
m4_mathdot(<<<3 \times 10^{-27}>>>, <<<3 × 10⁻²⁷>>>)
@item
m4_math(<<<\mathrm{gd}(u)>>>, <<<gd(u)>>>)
vs
m4_math(<<<\tan^{-1}(\sinh u)>>>, <<<tan⁻¹(sinh u)>>>)
on the same points: largest difference
m4_math(<<<\pi>>>, <<<π>>>)
(wrong branch outside
m4_mathpunc(<<<).>>>, <<<\lvert\mathrm{Im}\  u\rvert < \pi/2>>>, <<<|Im u| < π/2>>>)
@item
Sala's
m4_math(<<<\alpha>>>, <<<α>>>)
vs
m4_mathpunc(<<<:>>>, <<<\mathrm{atan2}(\sinh x, \cos y)>>>, <<<atan2(sinh x, cos y)>>>)
20,000 random points with
m4_mathcomma(<<<\lvert x\rvert \le 6>>>, <<<| x| ≤ 6>>>)
m4_mathpunc(<<<;>>>, <<<\lvert y\rvert \le 20>>>, <<<| y| ≤ 20>>>)
largest difference
m4_mathdot(<<<5 \times 10^{-41}>>>, <<<5 × 10⁻⁴¹>>>)
@item
m4_math(<<<\mathrm{gd}(A + w) - \mathrm{gd}(A - w)>>>, <<<gd(A + w) − gd(A − w)>>>)
vs
m4_mathpunc(<<<:>>>, <<<2\tan^{-1}(\sinh w / \cosh A)>>>, <<<2 tan⁻¹(sinh w / cosh A)>>>)
20,000 random points with real
m4_math(<<<A \in [0.05, 6]>>>, <<<A ∈ [0.05, 6]>>>)
and complex
m4_math(<<<w>>>, <<<w>>>)
satisfying
m4_mathpunc(<<<;>>>, <<<\sinh\lvert w\rvert < \cosh A>>>, <<<sinh| w| < cosh A>>>)
largest difference
m4_mathdot(<<<1.6 \times 10^{-30}>>>, <<<1.6 × 10⁻³⁰>>>)
@item
The same comparison with complex
m4_math(<<<A \in [0, 3] \times [-6, 6]\ i>>>, <<<A ∈ [0, 3] × [−6, 6] i>>>)
and complex
m4_math(<<<w>>>, <<<w>>>)
satisfying only
m4_mathpunc(<<<:>>>, <<<\sinh\lvert w\rvert < \lvert\cosh A\rvert>>>, <<<sinh| w| < |cosh A|>>>)
3,862 of 100,000 points differed, all by
m4_mathdot(<<<\pm 2\pi>>>, <<<± 2π>>>)
With
m4_math(<<<\lvert\mathrm{Re}\  w\rvert < \mathrm{Re}\  A>>>, <<<|Re w| < Re A>>>)
added: none.
@end itemize

@node Checks of the am Series with mpmath
@section Checks of the am Series with mpmath

All at 30–40 digits with mpmath:

@itemize @bullet
@item
m4_math(<<<\mathrm{am}(z|m)>>>, <<<am(z|m)>>>)
with both tests vs all terms as gd differences, and
m4_mathcomma(<<<\sin(\mathrm{am})>>>, <<<sin(am)>>>)
m4_math(<<<\cos(\mathrm{am})>>>, <<<cos(am)>>>)
vs sn, cn: 1,500 random points with
m4_math(<<<z \in [-3, 3] \times [-3, 3]\ i>>>, <<<z ∈ [−3, 3] × [−3, 3] i>>>)
and
m4_math(<<<m>>>, <<<m>>>)
complex in
m4_mathcomma(<<<[-10, 10] \times [-10, 10]\ i>>>, <<<[−10, 10] × [−10, 10] i>>>)
real in
m4_mathcomma(<<<[-20, 0]>>>, <<<[−20, 0]>>>)
or real in
m4_mathpunc(<<<;>>>, <<<[1, 20]>>>, <<<[1, 20]>>>)
largest relative difference
m4_mathcomma(<<<10^{-29}>>>, <<<10⁻²⁹>>>)
largest sn/cn error
m4_mathdot(<<<7 \times 10^{-28}>>>, <<<7 × 10⁻²⁸>>>)
@item
The reduction by
m4_math(<<<2K>>>, <<<2K>>>)
with the single test
m4_mathcomma(<<<\lvert w\rvert < \mathrm{Re}\  A>>>, <<<| w| < Re A>>>)
vs the unreduced series with both tests: 1,500 random points with
m4_mathcomma(<<<\lvert\mathrm{Re}\  z\rvert \le 60>>>, <<<|Re z| ≤ 60>>>)
m4_math(<<<\lvert\mathrm{Im}\  z\rvert \le 4>>>, <<<|Im z| ≤ 4>>>)
and
m4_math(<<<m>>>, <<<m>>>)
complex, real negative, real above 1, or in
m4_mathpunc(<<<;>>>, <<<(0, 1)>>>, <<<(0, 1)>>>)
largest relative difference
m4_mathdot(<<<5 \times 10^{-29}>>>, <<<5 × 10⁻²⁹>>>)
@item
The nome series vs Sala's series, with
m4_mathpunc(<<<:>>>, <<<q = \exp(-\pi K'/K)>>>, <<<q = exp(−π K'/K)>>>)
909 points with
m4_math(<<<m>>>, <<<m>>>)
real in
m4_math(<<<\pm[10^{-30}, 0.1]>>>, <<<±[10⁻³⁰, 0.1]>>>)
or complex with
m4_mathcomma(<<<\lvert m\rvert \lesssim 0.3>>>, <<<| m| ≲ 0.3>>>)
m4_mathcomma(<<<\lvert\mathrm{Re}\  z\rvert \le 30>>>, <<<|Re z| ≤ 30>>>)
m4_math(<<<\lvert\mathrm{Im}\  z\rvert \le 3>>>, <<<|Im z| ≤ 3>>>)
and
m4_mathpunc(<<<;>>>, <<<\rho < 0.5>>>, <<<ρ < 0.5>>>)
largest relative difference
m4_mathdot(<<<7 \times 10^{-28}>>>, <<<7 × 10⁻²⁸>>>)
@item
Double-precision accuracy of the three forms of gd: see
@inlinefmtifelse{tex, @ref{The Real and Imaginary Parts of gd, section
4, The Real and Imaginary Parts of gd}, @ref{The Real and Imaginary
Parts of gd, section 4}}.
@item
Errors at large
m4_mathcomma(<<<\lvert z\rvert>>>, <<<| z|>>>)
at small
m4_mathcomma(<<<m>>>, <<<m>>>)
in sin(am) vs sn tests, and the reciprocal-modulus check for
m4_mathpunc(<<<:>>>, <<<m > 1>>>, <<<m > 1>>>)
see @inlinefmtifelse{tex, @ref{Accuracy - Expected Errors and Possible
Improvements, section 7, Accuracy: Expected Errors and Possible
Improvements}, @ref{Accuracy - Expected Errors and Possible
Improvements, section 7}}.
@end itemize

@node Checks of bf-jacobi-am in Maxima
@section Checks of @code{bf-jacobi-am} in Maxima

Of @code{bf-jacobi-am} itself, in Maxima under SBCL 2.2.9:

@itemize @bullet
@item
Double precision, 300 random points, against 40-digit references.
Largest
m4_math(<<<\lvert\text{error}\rvert / (\varepsilon \max(1, \lvert\mathrm{am}\rvert))>>>, <<<|error| / (ε max(1, |am|))>>>)
by kind of
m4_mathpunc(<<<:>>>, <<<m>>>, <<<m>>>)
3.9 for
m4_mathpunc(<<<;>>>, <<<m \in (0, 1)>>>, <<<m ∈ (0, 1)>>>)
2.6 for
m4_mathpunc(<<<;>>>, <<<m < 0>>>, <<<m < 0>>>)
8.2 for
m4_math(<<<m > 1>>>, <<<m > 1>>>)
(including the spurious imaginary part of @inlinefmtifelse{tex,
@ref{Real m > 1 - Reciprocal-Modulus Transformation, section 7.4, Real m
> 1: Reciprocal-Modulus Transformation}, @ref{Real m > 1 -
Reciprocal-Modulus Transformation, section 7.4}}); 2.9 for complex
m4_mathpunc(<<<;>>>, <<<m>>>, <<<m>>>)
2.1 for
m4_mathpunc(<<<;>>>, <<<\lvert m\rvert \in [10^{-25}, 10^{-2}]>>>, <<<| m| ∈ [10⁻²⁵, 10⁻²]>>>)
3.1 for
m4_math(<<<m \in (0.05, 0.95)>>>, <<<m ∈ (0.05, 0.95)>>>)
with
m4_math(<<<\lvert z\rvert>>>, <<<| z|>>>)
up to 2000.
@item
Bigfloat with fpprec = 40, 40 random points with
m4_math(<<<\lvert\mathrm{Re}\  z\rvert \le 300>>>, <<<|Re z| ≤ 300>>>)
and
m4_math(<<<m>>>, <<<m>>>)
as above or down to
m4_mathpunc(<<<:>>>, <<<10^{-60}>>>, <<<10⁻⁶⁰>>>)
largest relative error
m4_mathdot(<<<8 \times 10^{-40}>>>, <<<8 × 10⁻⁴⁰>>>)
@item
rtest_elliptic, before and after the change: problems 229 and 278 fail
both times, since their tolerances were set under CMUCL. Problem 280
failed before and passes after. Problem 249, the
m4_mathcomma(<<<z = 2ki>>>, <<<z = 2ki>>>)
m4_math(<<<m = 1.75>>>, <<<m = 1.75>>>)
bigfloat sin(am) vs sn test, passed before and fails after: its largest
difference went from below its 6.3091b−31 tolerance to 6.33b−31.
@item
@code{run_testsuite()} after the change: the only other failure is
rtest5 problem 80, @code{describe(sin)}, because this build has no
documentation.
@end itemize

@bye
