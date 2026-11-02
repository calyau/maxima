\input texinfo
@c -*- mode: texinfo -*-
@c Created by Claude to describe the implementation of
@c inverse_jacobi_sn, inverse_jacobi_cn and inverse_jacobi_dn in Maxima.
@c
@setfilename inverse-jacobi-accuracy.info
@settitle Accurate Computation of the Inverse Jacobian Elliptic Functions
@documentencoding UTF-8

@include dlmf-ref-macros.texi

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
@top Accurate Computation of the Inverse Jacobian Elliptic Functions
@end ifnottex

@iftex
@titlepage
@title Accurate Computation of the Inverse Jacobian Elliptic Functions
@end titlepage
@end iftex

This document explains how @code{bf-inverse-jacobi-sn},
@code{bf-inverse-jacobi-cn} and @code{bf-inverse-jacobi-dn} in
@file{src/ellipt.lisp} compute @code{inverse_jacobi_sn},
@code{inverse_jacobi_cn} and @code{inverse_jacobi_dn} accurately,
and how the other nine inverse functions are computed from the same
formulas: @code{inverse_jacobi_ns}, @code{inverse_jacobi_nc} and
@code{inverse_jacobi_nd} (@pxref{Reciprocal Functions}), and
@code{inverse_jacobi_sc}, @code{inverse_jacobi_sd},
@code{inverse_jacobi_cs}, @code{inverse_jacobi_ds},
@code{inverse_jacobi_cd} and @code{inverse_jacobi_dc} (@pxref{The
Other Six Functions}).

It covers the formulas in terms of Carlson's integral
m4_math(<<<R_F>>>, <<<R_F>>>)
and how they avoid cancellation (@pxref{The Formulas and How They Are
Scaled}), branch cuts and signed zeros (@pxref{Signed Zeros},
@ref{The Imaginary Axis}), how the arguments are converted
(@pxref{Converting the Arguments}), the case
m4_math(<<<m = 1>>>, <<<m = 1>>>)
(@pxref{The Case m = 1}), and the argument scaling that prevents
overflow: the identity it relies on (@pxref{Homogeneity of R_F}), how
the scale factor is chosen (@pxref{Choosing the Scale Factor}), and why
scaling starts at @code{(> e 64)} (@pxref{Why the Threshold Is 64}).

@menu
* The Problem::
* Homogeneity of R_F::
* The Formulas and How They Are Scaled::
* The Case m = 1::
* Choosing the Scale Factor::
* Why the Threshold Is 64::
* Small m in inverse_jacobi_dn::
* Reciprocal Functions::
* The Other Six Functions::
@end menu

@node The Problem
@chapter The Problem

The three functions are computed from the form of
m4_math(<<<F>>>, <<<F>>>)
in terms of
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)

m4_displaymath(
<<<F(\phi | m) = \sin\phi \, R_F(\cos^2\phi,\ 1-m\sin^2\phi,\ 1),>>>
,
<<<@example
F(phi|m) = sin(phi)*R_F(cos(phi)^2, 1-m*sin(phi)^2, 1)
@end example>>>
)

which follows from @urldlmf{19.25.E5},
m4_math(<<<F(\phi, k) = R_F(c-1,\ c-k^2,\ c)>>>, <<<F(phi, k) = R_F(c-1, c-k^2, c)>>>)
with
m4_mathcomma(<<<c = \csc^2\phi>>>, <<<c = csc(phi)^2>>>)
and the homogeneity of
m4_math(<<<R_F>>>, <<<R_F>>>)
(@pxref{Homogeneity of R_F}).  The factor
m4_math(<<<\sin^2\phi>>>, <<<sin(phi)^2>>>)
is real and positive only for real
m4_mathcomma(<<<\phi>>>, <<<phi>>>)
so for complex arguments the formulas below were checked numerically,
by round trips through mpmath's @code{ellipfun}.  With
m4_mathcomma(<<<\phi = {\rm am}(z|m)>>>, <<<phi = am(z|m)>>>)
the Jacobian elliptic functions are
m4_mathcomma(<<<{\rm sn}\, z = \sin\phi>>>, <<<sn(z) = sin(phi)>>>)
m4_math(<<<{\rm cn}\, z = \cos\phi>>>, <<<cn(z) = cos(phi)>>>)
and
m4_math(<<<{\rm dn}^2 z = 1 - m\sin^2\phi>>>, <<<dn(z)^2 = 1 - m*sin(phi)^2>>>)
(@urldlmf{22.16.i}).  So

m4_displaymath(
<<<z = s \, R_F(c^2, d^2, 1),>>>
,
<<<@example
z = s*R_F(c^2, d^2, 1)
@end example>>>
)

with
m4_mathcomma(<<<s = {\rm sn}(z)>>>, <<<s = sn(z)>>>)
m4_math(<<<c^2 = {\rm cn}^2(z)>>>, <<<c^2 = cn(z)^2>>>)
and
m4_mathcomma(<<<d^2 = {\rm dn}^2(z)>>>, <<<d^2 = dn(z)^2>>>)
all computed from the argument
m4_mathdot(<<<u>>>, <<<u>>>)
For @code{inverse_jacobi_sn},

m4_displaymath(
<<<z = u \, R_F(1-u^2,\ 1-mu^2,\ 1).>>>
,
<<<@example
z = u*R_F(1-u^2, 1-m*u^2, 1)
@end example>>>
)

In double float,
m4_math(<<<u^2>>>, <<<u^2>>>)
overflows once
m4_mathcomma(<<<|u| \gtrsim 10^{154}>>>, <<<abs(u) > 1e154>>>)
and
m4_math(<<<mu^2>>>, <<<m*u^2>>>)
overflows once
m4_math(<<<|mu^2|>>>, <<<abs(m*u^2)>>>)
exceeds the largest double.  Before the change,
@code{inverse_jacobi_sn(1e160, 0.5)} signalled
@code{FLOATING-POINT-OVERFLOW}, and so did the corresponding calls to
@code{inverse_jacobi_cn} and @code{inverse_jacobi_dn}.

@code{bf-rf} also overflows before its arguments do.  To decide when
to stop its duplication steps, it computes

m4_displaymath(
<<<Q = {\max_j |A - x_j| \over \sqrt{\varepsilon}},
\qquad A = {x_1 + x_2 + x_3 \over 3},>>>
,
<<<@example
Q = max(abs(A - x[j]))/sqrt(eps),  A = (x[1] + x[2] + x[3])/3
@end example>>>
)

where
m4_math(<<<\varepsilon>>>, <<<eps>>>)
is the machine epsilon, so that
m4_math(<<<\sqrt{\varepsilon} \approx 1.05\times10^{-8}>>>, <<<sqrt(eps) = 1.05e-8>>>)
(@code{bferrtol}) for doubles.  So
m4_math(<<<Q>>>, <<<Q>>>)
overflows once an argument exceeds about
m4_mathdot(<<<1.8\times10^{308} \times 1.05\times10^{-8} \approx 2^{997}>>>, <<<1.8e308*1.05e-8 = 2^997>>>)
This is the first overflow in @code{bf-rf}: the sum in
m4_mathcomma(<<<A>>>, <<<A>>>)
and
m4_math(<<<x_n + \lambda_n>>>, <<<x[n] + lambda[n]>>>)
in the duplication steps, overflow only near
m4_mathdot(<<<2^{1022}>>>, <<<2^1022>>>)
One failure observed was the division
m4_mathcomma(<<<6.67\times10^{305}/1.05\times10^{-8}>>>, <<<6.67e305/1.05e-8>>>)
from @code{inverse_jacobi_nd(0.001, 1e-300)}.

@node Homogeneity of R_F
@ifinfo
@chapter Homogeneity of R_F
@end ifinfo
@ifnotinfo
@chapter Homogeneity of @math{R_F}
@end ifnotinfo

For real
m4_mathcomma(<<<\lambda > 0>>>, <<<lambda > 0>>>)

m4_displaymath(
<<<R_F(\lambda x, \lambda y, \lambda z) = \lambda^{-1/2} R_F(x, y, z).>>>
,
<<<@example
R_F(lambda*x, lambda*y, lambda*z) = lambda^(-1/2)*R_F(x, y, z)
@end example>>>
)

@menu
* References::
* Proof::
* Why the Factor Must Be Real and Positive::
@end menu

@node References
@section References

There is no numbered equation for this identity.

@itemize @bullet
@item
B. C. Carlson, ``Numerical computation of real or complex elliptic
integrals'', @emph{Numerical Algorithms} 10 (1995) 13--26,
@url{https://arxiv.org/abs/math/9409227, arXiv:math/9409227}.  The
introduction, just after eq. (7), says that
m4_math(<<<R_F>>>, <<<R_F>>>)
and
m4_math(<<<R_C>>>, <<<R_C>>>)
are homogeneous of degree
m4_mathdot(<<<-1/2>>>, <<<-1/2>>>)
It does not write out the identity or state a condition on
m4_mathdot(<<<\lambda>>>, <<<lambda>>>)
@item
@urldlmf{19.16} states homogeneity only for the general function
m4_mathcomma(<<<R_{-a}({\bf b};{\bf z})>>>, <<<R[-a](b;z)>>>)
in the text after @urldlmf{19.16.E8}.
@end itemize

@node Proof
@section Proof

The identity follows directly from the definition of
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
@urldlmf{19.16.E1}:

m4_displaymath(
<<<R_F(x,y,z) = {1\over 2}\int_0^\infty {ds \over \sqrt{s+x}\,\sqrt{s+y}\,\sqrt{s+z}}.>>>
,
<<<@example
                   inf
                  /
              1   [                   1
R_F(x,y,z) =  -   I   ------------------------------------ ds
              2   ]   sqrt(s + x) sqrt(s + y) sqrt(s + z)
                  /
                   0
@end example>>>
)

In
m4_mathcomma(<<<R_F(\lambda x, \lambda y, \lambda z)>>>, <<<R_F(lambda*x, lambda*y, lambda*z)>>>)
substitute
m4_mathcomma(<<<s = \lambda\sigma>>>, <<<s = lambda*sigma>>>)
so
m4_mathcomma(<<<ds = \lambda\, d\sigma>>>, <<<ds = lambda*dsigma>>>)
and the range of integration is unchanged because
m4_mathdot(<<<\lambda > 0>>>, <<<lambda > 0>>>)
For each factor,

m4_displaymath(
<<<\sqrt{\lambda\sigma + \lambda x} = \sqrt{\lambda}\,\sqrt{\sigma + x}.>>>
,
<<<@example
sqrt(lambda*sigma + lambda*x) = sqrt(lambda)*sqrt(sigma + x)
@end example>>>
)

This holds for principal square roots because multiplying by a
positive real number does not change the argument of a complex number.
So

m4_displaymath(
<<<R_F(\lambda x,\lambda y,\lambda z)
= {1\over 2}\int_0^\infty {\lambda\, d\sigma \over \lambda^{3/2}\sqrt{\sigma+x}\,\sqrt{\sigma+y}\,\sqrt{\sigma+z}}
= \lambda^{-1/2} R_F(x,y,z).>>>
,
<<<@example
R_F(lambda*x, lambda*y, lambda*z) = lambda^(-1/2)*R_F(x, y, z)
@end example>>>
)

With
m4_math(<<<\lambda = t^2>>>, <<<lambda = t^2>>>)
and
m4_mathcomma(<<<t > 0>>>, <<<t > 0>>>)

m4_displaymath(
<<<R_F(t^2 x, t^2 y, t^2 z) = {1\over t} R_F(x, y, z),
\qquad
\sqrt{t^2 a} = t\sqrt{a}.>>>
,
<<<@example
R_F(t^2*x, t^2*y, t^2*z) = R_F(x, y, z)/t,   sqrt(t^2*a) = t*sqrt(a)
@end example>>>
)

@node Why the Factor Must Be Real and Positive
@section Why the Factor Must Be Real and Positive

For complex
m4_mathcomma(<<<\lambda>>>, <<<lambda>>>)
multiplying by
m4_math(<<<\lambda>>>, <<<lambda>>>)
rotates the arguments of
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
and one of them can cross the branch cut
m4_mathdot(<<<(-\infty, 0]>>>, <<<(-inf, 0]>>>)
The identity then fails, not only by a sign.  For example, with
m4_math(<<<\lambda = i>>>, <<<lambda = %i>>>)
and
m4_mathcomma(<<<(x, y, z) = (1, 1, -1+i)>>>, <<<(x, y, z) = (1, 1, -1+%i)>>>)
the argument
m4_math(<<<\lambda z = -1-i>>>, <<<lambda*z = -1-%i>>>)
is on the other side of the cut from
m4_mathcomma(<<<z>>>, <<<z>>>)
and

m4_displaymath(
<<<\eqalign{
R_F(i, i, -1-i) &= 1.41204 - 0.09886\, i, \cr
i^{-1/2} R_F(1, 1, -1+i) &= 0.37510 - 1.00565\, i.
}>>>
,
<<<@example
R_F(%i, %i, -1-%i)         = 1.41204 - 0.09886*%i
%i^(-1/2)*R_F(1, 1, -1+%i) = 0.37510 - 1.00565*%i
@end example>>>
)

So the code scales only by
m4_math(<<<t^2>>>, <<<t^2>>>)
with
m4_math(<<<t>>>, <<<t>>>)
a power of 2, never by
m4_math(<<<|u|^{-2}>>>, <<<abs(u)^(-2)>>>)
times a phase or by
m4_mathdot(<<<1/u^2>>>, <<<1/u^2>>>)

@node The Formulas and How They Are Scaled
@chapter The Formulas and How They Are Scaled

@menu
* The Formulas::
* Choosing Among Equivalent Forms::
* Scaling the Formulas::
* Rounding::
* Signed Zeros::
* The Imaginary Axis::
* Converting the Arguments::
@end menu

@node The Formulas
@section The Formulas

Write
m4_mathcomma(<<<{\rm sn}^{-1}>>>, <<<sn^-1>>>)
m4_math(<<<{\rm cn}^{-1}>>>, <<<cn^-1>>>)
and
m4_math(<<<{\rm dn}^{-1}>>>, <<<dn^-1>>>)
for @code{inverse_jacobi_sn}, @code{inverse_jacobi_cn} and
@code{inverse_jacobi_dn}, and
m4_math(<<<K(m) = R_F(0, 1-m, 1)>>>, <<<K(m) = R_F(0, 1-m, 1)>>>)
for the complete integral.  All square roots are principal.

@menu
* inverse_jacobi_sn::
* inverse_jacobi_cn::
* inverse_jacobi_dn::
@end menu

@node inverse_jacobi_sn
@subsection @code{inverse_jacobi_sn}

With
m4_mathcomma(<<<{\rm sn}(z) = u>>>, <<<sn(z) = u>>>)
so that
m4_math(<<<{\rm cn}^2(z) = 1-u^2>>>, <<<cn(z)^2 = 1-u^2>>>)
and
m4_mathcomma(<<<{\rm dn}^2(z) = 1-mu^2>>>, <<<dn(z)^2 = 1-m*u^2>>>)

m4_displaymath(
<<<{\rm sn}^{-1}(u | m) = u \, R_F(1-u^2,\ 1-mu^2,\ 1).>>>
,
<<<@example
inverse_jacobi_sn(u, m) = u*R_F(1-u^2, 1-m*u^2, 1)
@end example>>>
)

This is
m4_mathdot(<<<F(\sin^{-1} u | m)>>>, <<<F(asin(u)|m)>>>)
No reflection is needed for
m4_mathcomma(<<<{\rm Re}\, u < 0>>>, <<<realpart(u) < 0>>>)
as it is for @code{inverse_jacobi_cn} and @code{inverse_jacobi_dn},
because
m4_math(<<<{\rm Re}(\sin^{-1} u)>>>, <<<realpart(asin(u))>>>)
is always in
m4_mathcomma(<<<[-\pi/2, \pi/2]>>>, <<<[-%pi/2, %pi/2]>>>)
where the form above holds.

@node inverse_jacobi_cn
@subsection @code{inverse_jacobi_cn}

This is
m4_math(<<<F(\cos^{-1} u | m)>>>, <<<F(acos(u)|m)>>>)
(@urlaands{eqn 17.4.52, 596}).  With
m4_mathcomma(<<<\varphi = \cos^{-1} u>>>, <<<phi = acos(u)>>>)
m4_math(<<<\cos\varphi = u>>>, <<<cos(phi) = u>>>)
and
m4_mathcomma(<<<\sin\varphi = \sqrt{1-u^2}>>>, <<<sin(phi) = sqrt(1-u^2)>>>)
so

m4_displaymath(
<<<{\rm cn}^{-1}(u | m) = \cases{
\sqrt{1-u^2} \, R_F(u^2,\ 1-m(1-u^2),\ 1), & ${\rm Re}\, u \ge 0$, \cr
2K(m) - {\rm cn}^{-1}(-u | m), & ${\rm Re}\, u < 0$.
}>>>
,
<<<@example
                          / sqrt(1-u^2)*R_F(u^2, 1-m*(1-u^2), 1),  realpart(u) >= 0
inverse_jacobi_cn(u, m) = <
                          \ 2*K(m) - inverse_jacobi_cn(-u, m),     realpart(u) < 0
@end example>>>
,
<<<{\rm cn}^{-1}(u | m) = \cases{
\sqrt{1-u^2} \, R_F(u^2,\ 1-m(1-u^2),\ 1), & ${\rm Re}\, u \ge 0$, \cr
2K(m) - {\rm cn}^{-1}(-u | m), & ${\rm Re}\, u < 0$.
}>>>
)

The two cases are needed because the form of
m4_math(<<<F>>>, <<<F>>>)
in terms of
m4_math(<<<R_F>>>, <<<R_F>>>)
holds only for part of the range of
m4_mathdot(<<<\cos^{-1}>>>, <<<acos>>>)
@urldlmf{19.25.E5} gives it for
m4_math(<<<0 \le {\rm Re}\, \varphi \le \pi/2>>>, <<<0 <= realpart(phi) <= %pi/2>>>)
(corrected in DLMF version 1.1.7 from
m4_mathpunc(<<<),>>>, <<<-\pi < {\rm Re}\, \varphi \le \pi>>>, <<<-%pi < realpart(phi) <= %pi>>>)
and since
m4_math(<<<F>>>, <<<F>>>)
is odd in
m4_math(<<<\varphi>>>, <<<phi>>>)
it also holds for
m4_mathdot(<<<|{\rm Re}\, \varphi| \le \pi/2>>>, <<<abs(realpart(phi)) <= %pi/2>>>)
m4_math(<<<{\rm Re}(\cos^{-1} u) \le \pi/2>>>, <<<realpart(acos(u)) <= %pi/2>>>)
exactly when
m4_mathcomma(<<<{\rm Re}\, u \ge 0>>>, <<<realpart(u) >= 0>>>)
which gives the first case.  When
m4_mathcomma(<<<{\rm Re}\, u < 0>>>, <<<realpart(u) < 0>>>)
m4_mathdot(<<<{\rm Re}\, \varphi > \pi/2>>>, <<<realpart(phi) > %pi/2>>>)
Then
m4_mathcomma(<<<\cos^{-1} u = \pi - \cos^{-1}(-u)>>>, <<<acos(u) = %pi - acos(-u)>>>)
and the quasi-periodicity
m4_math(<<<F(\varphi + \pi | m) = F(\varphi | m) + 2K(m)>>>, <<<F(phi + %pi|m) = F(phi|m) + 2*K(m)>>>)
(@urlaands{eqn 17.4.3, 592}), with
m4_math(<<<F>>>, <<<F>>>)
odd, gives

m4_displaymath(
<<<F(\cos^{-1} u | m) = 2K(m) - F(\cos^{-1}(-u) | m),>>>
,
<<<@example
F(acos(u)|m) = 2*K(m) - F(acos(-u)|m)
@end example>>>
)

where
m4_mathdot(<<<{\rm Re}\cos^{-1}(-u) < \pi/2>>>, <<<realpart(acos(-u)) < %pi/2>>>)
That is the second case.

@node inverse_jacobi_dn
@subsection @code{inverse_jacobi_dn}

The reciprocal-modulus
transformation (@urldlmf{22.17.E4}; Gradshteyn and Ryzhik 8.153.6)
gives

m4_displaymath(
<<<{\rm dn}(z | m) = {\rm cn}(\sqrt{m}\, z | 1/m),>>>
,
<<<@example
dn(z|m) = cn(sqrt(m)*z|1/m)
@end example>>>
)

so for
m4_mathcomma(<<<w = {\rm dn}(z | m)>>>, <<<w = dn(z|m)>>>)

m4_displaymath(
<<<{\rm dn}^{-1}(w | m) = {{\rm cn}^{-1}(w | 1/m) \over \sqrt{m}}.>>>
,
<<<@example
inverse_jacobi_dn(w, m) = inverse_jacobi_cn(w, 1/m)/sqrt(m)
@end example>>>
)

The two cases of the @code{inverse_jacobi_cn} formula carry over, for
the same reason.  Computing
m4_math(<<<1/m>>>, <<<1/m>>>)
first and then
m4_math(<<<1-(1/m)(1-w^2)>>>, <<<1-(1/m)*(1-w^2)>>>)
loses accuracy when
m4_math(<<<m>>>, <<<m>>>)
is near 1 and
m4_math(<<<w^2>>>, <<<w^2>>>)
is near
m4_mathcomma(<<<1-m>>>, <<<1-m>>>)
near the zero of
m4_mathcomma(<<<{\rm cn}>>>, <<<cn>>>)
because the rounding error in
m4_math(<<<1/m>>>, <<<1/m>>>)
is not small compared with the result.  So the substitution is done
algebraically,

m4_displaymath(
<<<1 - {1-w^2 \over m} = {w^2-(1-m) \over m},>>>
,
<<<@example
1 - (1-w^2)/m = (w^2-(1-m))/m
@end example>>>
)

where
m4_math(<<<1-m>>>, <<<1-m>>>)
is exact for
m4_math(<<<m>>>, <<<m>>>)
near 1.  This gives

m4_displaymath(
<<<{\rm dn}^{-1}(w | m) = \cases{
\displaystyle{\sqrt{1-w^2} \over \sqrt{m}} \, R_F\left(w^2,\ {w^2-(1-m) \over m},\ 1\right), & ${\rm Re}\, w \ge 0$, \cr
\displaystyle{2K(1/m) \over \sqrt{m}} - {\rm dn}^{-1}(-w | m), & ${\rm Re}\, w < 0$,
}>>>
,
<<<@example
                          / sqrt(1-w^2)*R_F(w^2, (w^2-(1-m))/m, 1)/sqrt(m),  realpart(w) >= 0
inverse_jacobi_dn(w, m) = <
                          \ 2*K(1/m)/sqrt(m) - inverse_jacobi_dn(-w, m),      realpart(w) < 0
@end example>>>
)

with
m4_mathcomma(<<<K(1/m) = R_F(0,\ (m-1)/m,\ 1)>>>, <<<K(1/m) = R_F(0, (m-1)/m, 1)>>>)
computed the same way rather than as
m4_mathdot(<<<R_F(0,\ 1-1/m,\ 1)>>>, <<<R_F(0, 1-1/m, 1)>>>)
Computing @code{inverse_jacobi_dn} instead as
@code{(/ (bf-inverse-jacobi-cn w (/ m)) (sqrt m))} raised the worst
double-float error on the real test points from about 1 ulp to about
m4_math(<<<2\times10^{3}>>>, <<<2e3>>>)
ulps, and on complex points from 3.3 to
m4_mathdot(<<<1.9\times10^{5}>>>, <<<1.9e5>>>)

For real
m4_math(<<<m \in (0, 1)>>>, <<<0 < m < 1>>>)
and real
m4_mathcomma(<<<w \in [\sqrt{1-m}, 1]>>>, <<<sqrt(1-m) <= w <= 1>>>)
where the result is real and lies in
m4_mathcomma(<<<[0, K(m)]>>>, <<<[0, K(m)]>>>)
multiplying the arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
by
m4_math(<<<m>>>, <<<m>>>)
(@pxref{Homogeneity of R_F}) gives a form with no division by
m4_math(<<<m{:}>>>, <<<m:>>>)

m4_displaymath(
<<<{\rm dn}^{-1}(w | m) = \sqrt{1-w^2} \, R_F(w^2-(1-m),\ mw^2,\ m).>>>
,
<<<@example
inverse_jacobi_dn(w, m) = sqrt(1-w^2)*R_F(w^2-(1-m), m*w^2, m)
@end example>>>
)

@node Choosing Among Equivalent Forms
@section Choosing Among Equivalent Forms

Some arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
have two algebraically equal forms that round differently, and the
code uses the one whose terms are smaller in magnitude
(@code{bf-inverse-jacobi-choose}):

@multitable @columnfractions 0.34 0.28 0.38
@headitem Function @tab Form A @tab Form B
@item @code{inverse_jacobi_sn} @tab @math{1-m u^2} @tab @math{(1-u^2) + (1-m) u^2}
@item @code{inverse_jacobi_cn} @tab @math{1-m(1-u^2)} @tab @math{(1-m) + m u^2}
@item @code{inverse_jacobi_dn} @tab @math{w^2-(1-m)} @tab @math{m-(1-w^2)}
@end multitable

In every case
m4_math(<<<1-u^2>>>, <<<1-u^2>>>)
is computed as
m4_mathcomma(<<<(1-u)(1+u)>>>, <<<(1-u)*(1+u)>>>)
or for complex
m4_math(<<<u>>>, <<<u>>>)
by @code{bf-diff-of-squares} (@pxref{Rounding}).

@node Scaling the Formulas
@section Scaling the Formulas

@code{bf-inverse-jacobi-scale} returns
m4_math(<<<v = ut>>>, <<<v = u*t>>>)
and
m4_mathdot(<<<t>>>, <<<t>>>)
The functions then replace
m4_math(<<<u>>>, <<<u>>>)
by
m4_math(<<<v>>>, <<<v>>>)
and every 1 by
m4_mathdot(<<<t^2>>>, <<<t^2>>>)
For @code{inverse_jacobi_sn},

m4_displaymath(
<<<\eqalign{
u \, R_F(1-u^2,\ 1-mu^2,\ 1)
&= {v\over t} \, R_F\left({t^2-v^2\over t^2},\ {t^2-mv^2\over t^2},\ {t^2\over t^2}\right) \cr
&= {v\over t}\cdot t \, R_F(t^2-v^2,\ t^2-mv^2,\ t^2) \cr
&= v \, R_F(t^2-v^2,\ t^2-mv^2,\ t^2).
}>>>
,
<<<@example
u*R_F(1-u^2, 1-m*u^2, 1)
   = (v/t)*R_F((t^2-v^2)/t^2, (t^2-m*v^2)/t^2, t^2/t^2)
   = (v/t)*t*R_F(t^2-v^2, t^2-m*v^2, t^2)
   = v*R_F(t^2-v^2, t^2-m*v^2, t^2)
@end example>>>
)

For @code{inverse_jacobi_cn}, with
m4_mathcomma(<<<\sqrt{t^2-v^2} = t\sqrt{1-u^2}>>>, <<<sqrt(t^2-v^2) = t*sqrt(1-u^2)>>>)

m4_displaymath(
<<<\sqrt{1-u^2} \, R_F(u^2,\ 1-m(1-u^2),\ 1)
= \sqrt{t^2-v^2} \, R_F(v^2,\ t^2-m(t^2-v^2),\ t^2).>>>
,
<<<@example
sqrt(1-u^2)*R_F(u^2, 1-m*(1-u^2), 1)
   = sqrt(t^2-v^2)*R_F(v^2, t^2-m*(t^2-v^2), t^2)
@end example>>>
)

For @code{inverse_jacobi_dn},

m4_displaymath(
<<<{\sqrt{1-w^2}\over\sqrt{m}} \, R_F\left(w^2,\ {w^2-(1-m)\over m},\ 1\right)
= {\sqrt{t^2-v^2}\over\sqrt{m}} \, R_F\left(v^2,\ {v^2-(1-m)t^2\over m},\ t^2\right),>>>
,
<<<@example
sqrt(1-w^2)*R_F(w^2, (w^2-(1-m))/m, 1)/sqrt(m)
   = sqrt(t^2-v^2)*R_F(v^2, (v^2-(1-m)*t^2)/m, t^2)/sqrt(m)
@end example>>>
)

and the real form becomes
m4_mathdot(<<<\sqrt{t^2-v^2} \, R_F(v^2-(1-m)t^2,\ mv^2,\ mt^2)>>>, <<<sqrt(t^2-v^2)*R_F(v^2-(1-m)*t^2, m*v^2, m*t^2)>>>)
The terms
m4_math(<<<2K(m)>>>, <<<2*K(m)>>>)
and
m4_math(<<<2K(1/m)/\sqrt{m}>>>, <<<2*K(1/m)/sqrt(m)>>>)
do not depend on
m4_math(<<<u>>>, <<<u>>>)
and are not scaled.

@node Rounding
@section Rounding

Because
m4_math(<<<t>>>, <<<t>>>)
is a power of 2,
m4_math(<<<v = ut>>>, <<<v = u*t>>>)
and
m4_math(<<<t^2>>>, <<<t^2>>>)
are exact unless they underflow.  The scaled and unscaled computations
therefore differ only in rounding, and in practice they agree almost
always (@pxref{Effect of the Threshold}).

After scaling,
m4_math(<<<1-u^2>>>, <<<1-u^2>>>)
is still computed as
m4_mathcomma(<<<(t-v)(t+v)>>>, <<<(t-v)*(t+v)>>>)
so it keeps its accuracy near
m4_math(<<<u = \pm 1>>>, <<<u = +/-1>>>)
even when a large
m4_math(<<<|m|>>>, <<<abs(m)>>>)
forces scaling there.

For complex
m4_mathcomma(<<<v = x + iy>>>, <<<v = x + %i*y>>>)
@code{bf-diff-of-squares} computes
m4_math(<<<t^2-v^2>>>, <<<t^2-v^2>>>)
from its real and imaginary parts:

m4_displaymath(
<<<t^2 - v^2 = \left((t-x)(t+x) + y^2\right) - 2xy\, i.>>>
,
<<<@example
t^2 - v^2 = ((t-x)*(t+x) + y^2) - 2*x*y*%i
@end example>>>
)

The product
m4_math(<<<(t-v)(t+v)>>>, <<<(t-v)*(t+v)>>>)
would lose the imaginary part when
m4_math(<<<|v|>>>, <<<abs(v)>>>)
is tiny compared with
m4_mathcomma(<<<t>>>, <<<t>>>)
because the real parts of
m4_math(<<<t-v>>>, <<<t-v>>>)
and
m4_math(<<<t+v>>>, <<<t+v>>>)
both round to
m4_mathdot(<<<t>>>, <<<t>>>)
That imaginary part can be the only thing that decides which side of a
branch cut of
m4_math(<<<R_F>>>, <<<R_F>>>)
the result is on.

@node Signed Zeros
@section Signed Zeros

The function
m4_math(<<<R_F>>>, <<<R_F>>>)
has a branch cut along the negative real axis, so an argument on the
cut is taken on one side or the other according to the sign of a zero
imaginary part.  In double float that sign depends on how the value
was computed.  For
m4_math(<<<u = -iy>>>, <<<u = -%i*y>>>)
with
m4_mathcomma(<<<y > 0>>>, <<<y > 0>>>)
@code{(* u u)} gives @code{#C(-y^2 -0.0)}, below the cut.  But adding
a real number to a complex number with imaginary part
m4_math(<<<-0>>>, <<<-0>>>)
gives
m4_mathcomma(<<<+0>>>, <<<+0>>>)
so
m4_mathcomma(<<<1-m(1-u^2)>>>, <<<1-m*(1-u^2)>>>)
computed from the same
m4_mathcomma(<<<u>>>, <<<u>>>)
ends up above the cut.  Two arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
that should be on the same side are then on opposite sides, and the
result is not an inverse at all.  For example,
@code{inverse_jacobi_cn(-1d10*%i, 1-2^-52)} returned
m4_mathcomma(<<<2K(m) \approx 38.73>>>, <<<2*K(m) = 38.73>>>)
where
m4_mathcomma(<<<{\rm cn} = -1>>>, <<<cn = -1>>>)
not
m4_mathdot(<<<-10^{10}i>>>, <<<-1e10*%i>>>)

So @code{bf-inverse-jacobi-rf} makes any argument with a zero
imaginary part real before calling @code{bf-rf}.  Such an argument is
then always taken on the upper side of the cut.  Bigfloats have no
signed zero and already behave this way, so this also makes float and
bigfloat agree.  Without it, the number of sweep points where float
and bigfloat gave different values rose from 8 to 195 for
@code{inverse_jacobi_nc} and from 8 to 212 for
@code{inverse_jacobi_nd}.

A simpler-looking alternative is to add zero, which also turns
m4_math(<<<-0>>>, <<<-0>>>)
into
m4_math(<<<+0>>>, <<<+0>>>)
(in IEEE arithmetic,
m4_math(<<<-0 + 0 = +0){:}>>>, <<<-0 + 0 = +0):>>>)
@code{#c(0d0 0d0)} to a complex argument, and @code{0d0} to a real
one.  (@code{#c(0 0)} does not work, since it reads as the integer 0.)
This gives the same branches everywhere tested, but it makes some
cases worse, because an argument such as @code{#C(x 0.0)} stays
complex, and @code{bf-rf} then runs in complex arithmetic.  On the
sweeps of 24,668 results, it changed 40, all in double float: 36 only
by rounding, and 4 less accurate.  The 4 are
m4_math(<<<m = 1 + 10^{-300}i>>>, <<<m = 1 + 1e-300*%i>>>)
with
m4_mathcomma(<<<u = \pm 10^{\pm 160}i>>>, <<<u = +/-1e+/-160*%i>>>)
where the scale factor
m4_math(<<<t^2>>>, <<<t^2>>>)
is subnormal and complex arithmetic loses precision on it.  For
example, for @code{inverse_jacobi_cn(1e-160*%i, 1+1e-300*%i)} the
relative error grew from
m4_math(<<<3\times10^{-11}>>>, <<<3e-11>>>)
to
m4_mathdot(<<<4\times10^{-7}>>>, <<<4e-7>>>)
Adding @code{#c(0d0 0d0)} to every argument, real ones included, is
worse still: it changed 107 results, 26 of them less accurate, such as
@code{inverse_jacobi_sn(1e160, 1e-300)}, from
m4_math(<<<1.3\times10^{-16}>>>, <<<1.3e-16>>>)
to
m4_mathcomma(<<<6\times10^{-7}>>>, <<<6e-7>>>)
and it makes real inputs go through complex arithmetic.

No such step is needed for the square roots.  The argument of
@code{sqrt} is
m4_mathcomma(<<<1-u^2>>>, <<<1-u^2>>>)
m4_mathcomma(<<<m>>>, <<<m>>>)
or a positive scale factor.  Here
m4_math(<<<1-u^2>>>, <<<1-u^2>>>)
is negative only for real
m4_math(<<<u>>>, <<<u>>>)
with
m4_mathcomma(<<<|u| > 1>>>, <<<abs(u) > 1>>>)
when it is a Lisp real, and for purely imaginary
m4_math(<<<u = iy>>>, <<<u = %i*y>>>)
it is
m4_mathdot(<<<1+y^2 > 0>>>, <<<1+y^2 > 0>>>)
m4_math(<<<m>>>, <<<m>>>)
is a Lisp real whenever its imaginary part is zero.

@node The Imaginary Axis
@section The Imaginary Axis

The formulas for
m4_math(<<<{\rm cn}^{-1}>>>, <<<cn^-1>>>)
and
m4_math(<<<{\rm dn}^{-1}>>>, <<<dn^-1>>>)
depend on
m4_math(<<<u>>>, <<<u>>>)
only through
m4_mathcomma(<<<u^2>>>, <<<u^2>>>)
and use the sign of
m4_math(<<<{\rm Re}\, u>>>, <<<realpart(u)>>>)
to choose between
m4_math(<<<u>>>, <<<u>>>)
and
m4_mathdot(<<<-u>>>, <<<-u>>>)
On the imaginary axis,
m4_mathcomma(<<<{\rm Re}\, u = 0>>>, <<<realpart(u) = 0>>>)
that does not work:
m4_math(<<<iy>>>, <<<%i*y>>>)
and
m4_math(<<<-iy>>>, <<<-%i*y>>>)
have the same
m4_mathcomma(<<<u^2>>>, <<<u^2>>>)
and neither has
m4_mathdot(<<<{\rm Re}\, u < 0>>>, <<<realpart(u) < 0>>>)
Then
m4_math(<<<u^2 = -y^2>>>, <<<u^2 = -y^2>>>)
is on the cut of
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
and the side it is taken on is what distinguishes
m4_math(<<<iy>>>, <<<%i*y>>>)
from
m4_mathdot(<<<-iy>>>, <<<-%i*y>>>)

As the limit from
m4_mathcomma(<<<{\rm Re}\, u > 0>>>, <<<realpart(u) > 0>>>)
every argument of
m4_math(<<<R_F>>>, <<<R_F>>>)
that is on the cut should be taken on the side given by the sign of
m4_mathdot(<<<{\rm Im}\, u>>>, <<<imagpart(u)>>>)
For
m4_math(<<<{\rm Im}\, u > 0>>>, <<<imagpart(u) > 0>>>)
that is the upper side, which is what @code{bf-inverse-jacobi-rf}
gives (@pxref{Signed Zeros}).  For
m4_math(<<<{\rm Im}\, u < 0>>>, <<<imagpart(u) < 0>>>)
it is the lower side, so @code{bf-inverse-jacobi-cn} and
@code{bf-inverse-jacobi-dn} instead use the reflection

m4_displaymath(
<<<f(u | m) = \overline{f(\bar u | \bar m)},>>>
,
<<<@example
f(u|m) = conjugate(f(conjugate(u)|conjugate(m)))
@end example>>>
)

evaluating at
m4_mathcomma(<<<\bar u>>>, <<<conjugate(u)>>>)
which has
m4_mathdot(<<<{\rm Im}\, \bar u > 0>>>, <<<imagpart(conjugate(u)) > 0>>>)
@code{bf-negative-imaginary-axis-p} tests for this case: it is true
when
m4_math(<<<{\rm Re}\, u = 0>>>, <<<realpart(u) = 0>>>)
(with either sign of zero) and
m4_mathdot(<<<{\rm Im}\, u < 0>>>, <<<imagpart(u) < 0>>>)
Without this, every
m4_math(<<<u = -iy>>>, <<<u = -%i*y>>>)
gave the inverse of
m4_math(<<<iy>>>, <<<%i*y>>>)
instead, a value that is not an inverse of
m4_math(<<<u>>>, <<<u>>>)
at all.

The function
m4_math(<<<{\rm sn}^{-1}>>>, <<<sn^-1>>>)
does not need this, since its formula
m4_math(<<<u \, R_F(1-u^2,\ 1-mu^2,\ 1)>>>, <<<u*R_F(1-u^2, 1-m*u^2, 1)>>>)
carries the factor
m4_math(<<<u>>>, <<<u>>>)
itself.

@node Converting the Arguments
@section Converting the Arguments

The simplifiers evaluate numerically through
@code{inverse-jacobi-numerical-eval}, which converts both arguments to
the same type before calling @code{bf-inverse-jacobi-sn},
@code{bf-inverse-jacobi-cn} or @code{bf-inverse-jacobi-dn} (or the
functions described in @ref{Reciprocal Functions} and @ref{The Other
Six Functions}): with
@code{float} when the evaluation is in double float, and with
@code{bfloat} when it is in bigfloat.  This includes complex
arguments.

Without the bigfloat conversion, a rational
m4_math(<<<u>>>, <<<u>>>)
with a bigfloat
m4_math(<<<m>>>, <<<m>>>)
reached the functions as a rational.  The double-float scaling
(@pxref{Choosing the Scale Factor}) then
applied @code{cl:abs} to the bigfloat
m4_math(<<<m>>>, <<<m>>>)
and signalled a type error.  Before the change, such arguments went
through single-float functions such as @code{acos}:
@code{inverse_jacobi_cn(1/2, 0.5b0)} returned
@code{1.142429144434984b0}, where the correct value is
@code{1.1424290580457772555013955266261b0}, and
@code{inverse_jacobi_sn(1/2+%i/3, 0.5b0)} returned only the imaginary
part.

@node The Case m = 1
@ifinfo
@chapter The Case m = 1
@end ifinfo
@ifnotinfo
@chapter The Case @math{m = 1}
@end ifnotinfo

For
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
m4_math(<<<{\rm cn}(z | 1) = {\rm dn}(z | 1) = {\rm sech}\, z>>>, <<<cn(z|1) = dn(z|1) = sech(z)>>>)
(@urlaands{eqn 16.6.2, 571}, @urlaands{eqn 16.6.3, 571},
@urldlmf{22.5.T4}), so both inverses are
m4_mathdot(<<<{\rm sech}^{-1}>>>, <<<asech>>>)
@code{bf-inverse-jacobi-cn} and @code{bf-inverse-jacobi-dn} handle
m4_math(<<<m = 1>>>, <<<m = 1>>>)
first, by calling Maxima's @code{asech} through
@code{bf-inverse-jacobi-asech}:

@example
(maxima::take '(maxima::%asech) (maxima::to u))
@end example

This comes before the reflection for the negative imaginary axis,
which conjugates its result, because @code{%asech} returns a Maxima
expression rather than a bigfloat-package number.

@code{%asech} is currently inaccurate near
m4_mathdot(<<<u = \pm 1>>>, <<<u = +/-1>>>)
For example, at
m4_math(<<<u = 1 - 10^{-12}>>>, <<<u = 1 - 1e-12>>>)
its relative error is about
m4_mathdot(<<<5.6\times10^{-5}>>>, <<<5.6e-5>>>)

@menu
* Why the General Formulas Are Not Used::
* Poles of inverse_jacobi_sn::
@end menu

@node Why the General Formulas Are Not Used
@section Why the General Formulas Are Not Used

For
m4_mathcomma(<<<{\rm Re}\, u \ge 0>>>, <<<realpart(u) >= 0>>>)
the @code{inverse_jacobi_cn} and @code{inverse_jacobi_dn} formulas
both reduce to

m4_displaymath(
<<<\sqrt{1-u^2} \, R_F(u^2, u^2, 1) = {\rm sech}^{-1}(u).>>>
,
<<<@example
sqrt(1-u^2)*R_F(u^2, u^2, 1) = asech(u)
@end example>>>
)

But they fail in three cases:

@enumerate
@item
m4_mathdot(<<<{\rm Re}\, u < 0>>>, <<<realpart(u) < 0>>>)
The reflections (@pxref{The Formulas}) use
m4_math(<<<2K(1)>>>, <<<2*K(1)>>>)
or
m4_mathcomma(<<<2K(1/m)/\sqrt{m}>>>, <<<2*K(1/m)/sqrt(m)>>>)
which are infinite at
m4_mathdot(<<<m = 1>>>, <<<m = 1>>>)
This is not only a numerical problem.  For
m4_mathcomma(<<<0 < m < 1>>>, <<<0 < m < 1>>>)
m4_mathcomma(<<<2K(1/m)/\sqrt{m} = 2K(m) \mp 2iK'(m)>>>, <<<2*K(1/m)/sqrt(m) = 2*K(m) -/+ 2*%i*K'(m)>>>)
and
m4_math(<<<K(m) \to \infty>>>, <<<K(m) -> inf>>>)
as
m4_mathcomma(<<<m \to 1>>>, <<<m -> 1>>>)
so the branch for
m4_math(<<<{\rm Re}\, u < 0>>>, <<<realpart(u) < 0>>>)
has no limit at
m4_mathdot(<<<m = 1>>>, <<<m = 1>>>)
The value there has to be chosen separately.
@item
m4_mathdot(<<<u = 0>>>, <<<u = 0>>>)
This is a pole, where
m4_math(<<<R_F(0, 0, 1)>>>, <<<R_F(0, 0, 1)>>>)
does not converge.
@item
Tiny double-float
m4_mathdot(<<<u>>>, <<<u>>>)
When
m4_math(<<<u^2>>>, <<<u^2>>>)
is subnormal or 0,
m4_math(<<<R_F(u^2, u^2, 1)>>>, <<<R_F(u^2, u^2, 1)>>>)
is inaccurate or does not converge, although
m4_math(<<<{\rm sech}^{-1}(u)>>>, <<<asech(u)>>>)
is finite.
@end enumerate

@code{%asech} handles all three.  Its principal value is the one given
by Maxima's @code{logarc(asech(u))},

m4_displaymath(
<<<{\rm sech}^{-1}(u) = \log\left({1\over u} + \sqrt{{1\over u} - 1}\,\sqrt{{1\over u} + 1}\right),>>>
,
<<<@example
asech(u) = log(1/u + sqrt(1/u - 1)*sqrt(1/u + 1))
@end example>>>
)

with real part
m4_math(<<<\ge 0>>>, <<<>= 0>>>)
and imaginary part in
m4_mathdot(<<<(-\pi, \pi]>>>, <<<(-%pi, %pi]>>>)
At
m4_math(<<<u = 0>>>, <<<u = 0>>>)
it signals a domain error, such as
@code{asech: argument 0.0 isn't in the domain of asech.}

@node Poles of inverse_jacobi_sn
@ifinfo
@section Poles of inverse_jacobi_sn
@end ifinfo
@ifnotinfo
@section Poles of @code{inverse_jacobi_sn}
@end ifnotinfo

At
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
m4_math(<<<{\rm sn}^{-1}>>>, <<<sn^-1>>>)
has poles at
m4_mathdot(<<<u = \pm 1>>>, <<<u = +/-1>>>)
There
m4_math(<<<1-u^2>>>, <<<1-u^2>>>)
and
m4_math(<<<1-mu^2>>>, <<<1-m*u^2>>>)
are both 0, and @code{bf-rf} does not converge: in float it signals
@code{FLOATING-POINT-INVALID-OPERATION}, and in bigfloat it never
returns.  @code{bf-inverse-jacobi-sn} checks for this and signals an
error like
@code{inverse_jacobi_sn: inverse_jacobi_sn(1.0, 1.0) is undefined.}

@node Choosing the Scale Factor
@chapter Choosing the Scale Factor

For a double-float
m4_mathcomma(<<<u>>>, <<<u>>>)
let
m4_math(<<<e_u>>>, <<<e_u>>>)
and
m4_math(<<<e_m>>>, <<<e_m>>>)
be the exponents from @code{decode-float} of
m4_math(<<<|u|>>>, <<<abs(u)>>>)
and
m4_mathcomma(<<<|m|>>>, <<<abs(m)>>>)
so that
m4_math(<<<|u| < 2^{e_u}>>>, <<<abs(u) < 2^e_u>>>)
and
m4_mathdot(<<<|m| < 2^{e_m}>>>, <<<abs(m) < 2^e_m>>>)
The code takes

m4_displaymath(
<<<e = e_u + \cases{
\lceil e_m/2 \rceil, & $|m| > 1$, \cr
0, & otherwise,
}>>>
,
<<<@example
e = e_u + ceiling(e_m/2)   if abs(m) > 1
e = e_u                    otherwise
@end example>>>
)

so that
m4_math(<<<|u|^2>>>, <<<abs(u)^2>>>)
and
m4_mathcomma(<<<|mu^2|>>>, <<<abs(m*u^2)>>>)
the largest terms in the unscaled arguments of
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
are below about
m4_mathdot(<<<2^{2e}>>>, <<<2^(2*e)>>>)
When
m4_math(<<<e > 64>>>, <<<e > 64>>>)
(@pxref{Why the Threshold Is 64}) it scales with
m4_mathcomma(<<<t = 2^{-s}>>>, <<<t = 2^(-s)>>>)
where

m4_displaymath(
<<<s = \max\left(\lceil e/2\rceil,\ e - 480\right).>>>
,
<<<@example
s = max(ceiling(e/2), e - 480)
@end example>>>
)

As in @ref{Scaling the Formulas},
m4_math(<<<u>>>, <<<u>>>)
is replaced by
m4_math(<<<v = ut>>>, <<<v = u*t>>>)
and every 1 by
m4_mathcomma(<<<t^2>>>, <<<t^2>>>)
so
m4_math(<<<u>>>, <<<u>>>)
is scaled by
m4_math(<<<t>>>, <<<t>>>)
and the arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
by
m4_mathdot(<<<\lambda = t^2 = 2^{-2s}>>>, <<<lambda = t^2 = 2^(-2*s)>>>)
By homogeneity (@pxref{Homogeneity of R_F}),

m4_displaymath(
<<<R_F(\lambda x,\ \lambda y,\ \lambda z) = \lambda^{-1/2} R_F(x, y, z) = {1 \over t} R_F(x, y, z)>>>
,
<<<@example
R_F(lambda*x, lambda*y, lambda*z) = lambda^(-1/2)*R_F(x, y, z) = R_F(x, y, z)/t
@end example>>>
)

for any real
m4_mathdot(<<<\lambda > 0>>>, <<<lambda > 0>>>)
With
m4_mathcomma(<<<s = \lceil e/2\rceil>>>, <<<s = ceiling(e/2)>>>)
m4_math(<<<\lambda \approx 2^{-e}{:}>>>, <<<lambda = 2^(-e):>>>)
the arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
are scaled by about
m4_mathcomma(<<<2^{-e}>>>, <<<2^(-e)>>>)
not by
m4_math(<<<2^{-2e}>>>, <<<2^(-2*e)>>>)
as with the earlier choice
m4_mathdot(<<<s = e>>>, <<<s = e>>>)
Writing
m4_math(<<<\lambda>>>, <<<lambda>>>)
as the square of a power of 2, rather than using
m4_math(<<<\lambda = 2^{-e}>>>, <<<lambda = 2^(-e)>>>)
directly, keeps
m4_math(<<<\lambda^{-1/2} = 1/t>>>, <<<lambda^(-1/2) = 1/t>>>)
exact when
m4_math(<<<e>>>, <<<e>>>)
is odd, and keeps the formulas of @ref{Scaling the Formulas} unchanged.

The scaled arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
are then made of terms between
m4_math(<<<t^2 = 2^{-2s}>>>, <<<t^2 = 2^(-2*s)>>>)
and about
m4_math(<<<2^{2e-2s}{:}>>>, <<<2^(2*e-2*s):>>>)

@itemize @bullet
@item
m4_math(<<<s = \lceil e/2\rceil>>>, <<<s = ceiling(e/2)>>>)
centers that range on 1, from about
m4_math(<<<2^{-e}>>>, <<<2^(-e)>>>)
to
m4_mathcomma(<<<2^{e}>>>, <<<2^e>>>)
so for
m4_math(<<<e>>>, <<<e>>>)
up to about 1000 neither end overflows or underflows.
@item
m4_math(<<<s \ge e - 480>>>, <<<s >= e - 480>>>)
keeps the largest term below
m4_mathcomma(<<<2^{960}>>>, <<<2^960>>>)
under the overflow of @code{bf-rf} near
m4_math(<<<2^{997}>>>, <<<2^997>>>)
(@pxref{The Problem}).  This bound takes over when
m4_mathdot(<<<e > 960>>>, <<<e > 960>>>)
@end itemize

The smallest term matters even when it is far smaller than the others.
For @code{inverse_jacobi_sn(2.0^600, 0.5)}, the first two arguments,
m4_math(<<<t^2-v^2>>>, <<<t^2-v^2>>>)
and
m4_mathcomma(<<<t^2-mv^2>>>, <<<t^2-m*v^2>>>)
are negative reals, and the third,
m4_mathcomma(<<<t^2>>>, <<<t^2>>>)
is the only source of the real part of the result,
m4_mathdot(<<<3.408\times10^{-181}>>>, <<<3.408e-181>>>)
An earlier version used
m4_mathcomma(<<<s = e>>>, <<<s = e>>>)
which multiplies the arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
by
m4_mathdot(<<<2^{-2e}>>>, <<<2^(-2*e)>>>)
That puts the largest term near 1 and the smallest at
m4_mathdot(<<<2^{-2e}>>>, <<<2^(-2*e)>>>)
Then
m4_math(<<<t^2>>>, <<<t^2>>>)
underflowed to 0 once
m4_mathcomma(<<<e > 537>>>, <<<e > 537>>>)
and the real part was returned as 0.  For the same reason,
@code{inverse_jacobi_sn(2.0^26, 2.0^1023)} lost its real part,
m4_mathcomma(<<<1.57\times10^{-162}>>>, <<<1.57e-162>>>)
and @code{inverse_jacobi_dn(2.0^538, 2.0^1023)} had a relative error of
m4_mathdot(<<<3\times10^{-11}>>>, <<<3e-11>>>)
With
m4_math(<<<s = \max(\lceil e/2\rceil,\ e-480)>>>, <<<s = max(ceiling(e/2), e-480)>>>)
all three are within 15 ulps.

@code{inverse_jacobi_dn} passes
m4_math(<<<m = 0>>>, <<<m = 0>>>)
to @code{bf-inverse-jacobi-scale}, so
m4_mathdot(<<<e = e_u>>>, <<<e = e_u>>>)
Its arguments of
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
m4_mathcomma(<<<w^2>>>, <<<w^2>>>)
m4_math(<<<(w^2-(1-m))/m>>>, <<<(w^2-(1-m))/m>>>)
and 1, do not contain
m4_mathdot(<<<mw^2>>>, <<<m*w^2>>>)
Including
m4_math(<<<\lceil e_m/2\rceil>>>, <<<ceiling(e_m/2)>>>)
made
m4_math(<<<t>>>, <<<t>>>)
much smaller than needed: for large
m4_math(<<<|w|>>>, <<<abs(w)>>>)
and
m4_mathcomma(<<<|m|>>>, <<<abs(m)>>>)
m4_math(<<<t^2>>>, <<<t^2>>>)
and
m4_math(<<<(v^2-(1-m)t^2)/m>>>, <<<(v^2-(1-m)*t^2)/m>>>)
underflowed to 0, and
m4_math(<<<R_F(v^2, 0, 0)>>>, <<<R_F(v^2, 0, 0)>>>)
does not converge.  For example,
@code{inverse_jacobi_dn(2.0^340, -2.0^700)} signalled
@code{FLOATING-POINT-INVALID-OPERATION}.

Bigfloats are never scaled, since their exponent range is effectively
unlimited.  The test is @code{cl:numberp}, because @code{numberp} in the
@code{bigfloat} package is true for bigfloats.

@menu
* When the Scaled Arguments Underflow::
* When the Smallest Argument Is Negligible::
@end menu

@node When the Scaled Arguments Underflow
@section When the Scaled Arguments Underflow

The scaled argument
m4_math(<<<v = u\, 2^{-s}>>>, <<<v = u*2^(-s)>>>)
is computed by @code{scale-float}, so it is exact.  Since
m4_mathcomma(<<<e \le e_u + 512>>>, <<<e <= e_u + 512>>>)
m4_math(<<<|v| \ge 2^{-481}>>>, <<<abs(v) >= 2^-481>>>)
whenever the code scales, so
m4_math(<<<v>>>, <<<v>>>)
and
m4_math(<<<v^2>>>, <<<v^2>>>)
never underflow.
m4_math(<<<t^2>>>, <<<t^2>>>)
does, for large
m4_math(<<<e{:}>>>, <<<e:>>>)

@multitable @columnfractions 0.3 0.3 0.4
@headitem @math{e} @tab @math{s} @tab @math{t^2}
@item up to 991 @tab up to 511 @tab normal
@item 992 to 1017 @tab 512 to 537 @tab subnormal
@item 1018 and up @tab 538 and up @tab 0
@end multitable

Reaching
m4_math(<<<e \ge 992>>>, <<<e >= 992>>>)
needs
m4_math(<<<|u| \gtrsim 2^{480}>>>, <<<abs(u) > 2^480>>>)
together with a large
m4_mathcomma(<<<|m|>>>, <<<abs(m)>>>)
or
m4_mathdot(<<<|u| \ge 2^{991}>>>, <<<abs(u) >= 2^991>>>)
The factor
m4_math(<<<t>>>, <<<t>>>)
itself underflows only when
m4_mathcomma(<<<s > 1074>>>, <<<s > 1074>>>)
which cannot happen since
m4_mathdot(<<<e \le 1536>>>, <<<e <= 1536>>>)
In CLISP, which has no denormals,
m4_math(<<<t>>>, <<<t>>>)
underflows when
m4_mathcomma(<<<s > 1022>>>, <<<s > 1022>>>)
that is,
m4_mathdot(<<<e \ge 1503>>>, <<<e >= 1503>>>)

Maxima does not trap floating-point underflow, so these give 0.  That
limit is
m4_math(<<<2^{-1074}>>>, <<<2^-1074>>>)
with denormals, as in SBCL and CMUCL, and
m4_math(<<<2^{-1022}>>>, <<<2^-1022>>>)
in CLISP:

@multitable @columnfractions 0.3 0.3 0.4
@headitem Lisp @tab @code{least-positive-double-float} @tab @code{(scale-float 1d0 (- s))} past it
@item SBCL 2.2.9 @tab 2^-1074 @tab @code{0d0}
@item ECL 21.2.1 @tab 2^-1074 @tab @code{0d0}
@item CLISP, default @tab 2^-1022 @tab signals @code{FLOATING-POINT-UNDERFLOW}
@item CLISP, as Maxima runs it @tab 2^-1022 @tab @code{0d0}
@end multitable

Maxima turns off CLISP's underflow errors in @file{src/globals.lisp}
(@code{sys::*inhibit-floating-point-underflow*}).

When
m4_math(<<<t^2>>>, <<<t^2>>>)
is subnormal or 0, a tiny real or imaginary part that comes from it
would be lost again, as with
m4_math(<<<s = e>>>, <<<s = e>>>)
above, for example for @code{inverse_jacobi_sn(1e300, 0.5)}.  No
choice of
m4_math(<<<s>>>, <<<s>>>)
avoids this: the arguments span about
m4_mathcomma(<<<2^{2e}>>>, <<<2^(2*e)>>>)
more than fits between the overflow of @code{bf-rf} and the bottom of
the double range.  @ref{When the Smallest Argument Is Negligible}
describes how the code handles it.

@node When the Smallest Argument Is Negligible
@section When the Smallest Argument Is Negligible

@code{bf-inverse-jacobi-rf-small-z} computes
m4_mathcomma(<<<f \, R_F(x, y, z)>>>, <<<f*R_F(x, y, z)>>>)
where
m4_math(<<<f>>>, <<<f>>>)
is the factor that multiplies
m4_math(<<<R_F>>>, <<<R_F>>>)
in the formula (such as
m4_math(<<<v>>>, <<<v>>>)
for @code{inverse_jacobi_sn} and
m4_math(<<<\sqrt{t^2-v^2}>>>, <<<sqrt(t^2-v^2)>>>)
for @code{inverse_jacobi_cn}), and
m4_math(<<<z>>>, <<<z>>>)
is the scaled third argument,
m4_math(<<<t^2>>>, <<<t^2>>>)
times a constant.  It is given
m4_math(<<<\sqrt{z}>>>, <<<sqrt(z)>>>)
as well as
m4_math(<<<z{:}>>>, <<<z:>>>)
m4_math(<<<t>>>, <<<t>>>)
for @code{inverse_jacobi_sn} and @code{inverse_jacobi_cn}, and
m4_math(<<<\sqrt{m}\, t>>>, <<<sqrt(m)*t>>>)
or
m4_math(<<<\sqrt{\lambda}\, t>>>, <<<sqrt(lambda)*t>>>)
for @code{inverse_jacobi_dn}.  Since
m4_math(<<<t>>>, <<<t>>>)
is a power of 2 no smaller than
m4_mathcomma(<<<2^{-1056}>>>, <<<2^-1056>>>)
m4_math(<<<\sqrt{z}>>>, <<<sqrt(z)>>>)
is representable even when
m4_math(<<<z>>>, <<<z>>>)
underflows.  When
m4_mathcomma(<<<|z| < 2^{-120}\min(|x|, |y|)>>>, <<<abs(z) < 2^-120*min(abs(x), abs(y))>>>)
it uses

m4_displaymath(
<<<R_F(x, y, z) = R_F(x, y, 0) - {\sqrt{z} \over \sqrt{x}\,\sqrt{y}} + O(z).>>>
,
<<<@example
R_F(x, y, z) = R_F(x, y, 0) - sqrt(z)/(sqrt(x)*sqrt(y)) + O(z)
@end example>>>
)

This follows from the definition of
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
@urldlmf{19.16.E1}.  Differentiating,

m4_displaymath(
<<<{\partial R_F \over \partial z}
= -{1\over 4}\int_0^\infty {ds \over \sqrt{s+x}\,\sqrt{s+y}\,(s+z)^{3/2}}.>>>
,
<<<@example
                      inf
                     /
d R_F          1     [                    ds
-----   =   -  -     I   ---------------------------------------
 d z           4     ]   sqrt(s + x) sqrt(s + y) (s + z)^(3/2)
                     /
                      0
@end example>>>
)

As
m4_mathcomma(<<<z \to 0>>>, <<<z -> 0>>>)
the integral is dominated by
m4_math(<<<s>>>, <<<s>>>)
near 0, where
m4_mathcomma(<<<\sqrt{s+x}\,\sqrt{s+y} \approx \sqrt{x}\,\sqrt{y}>>>, <<<sqrt(s+x)*sqrt(s+y) = sqrt(x)*sqrt(y)>>>)
and
m4_mathdot(<<<\int_0^\infty (s+z)^{-3/2}\, ds = 2/\sqrt{z}>>>, <<<integrate((s+z)^(-3/2), s, 0, inf) = 2/sqrt(z)>>>)
The rest of the integrand is
m4_math(<<<O(s^{-1/2})>>>, <<<O(s^(-1/2))>>>)
near 0, so its integral is bounded, and

m4_displaymath(
<<<{\partial R_F \over \partial z} = -{1 \over 2\sqrt{x}\,\sqrt{y}\,\sqrt{z}} + O(1).>>>
,
<<<@example
d R_F/d z = -1/(2*sqrt(x)*sqrt(y)*sqrt(z)) + O(1)
@end example>>>
)

Integrating from 0 to
m4_math(<<<z>>>, <<<z>>>)
gives the expansion.

@itemize @bullet
@item
@strong{Accuracy.}  The
m4_math(<<<O(z)>>>, <<<O(z)>>>)
term is about
m4_math(<<<|z|/\min(|x|, |y|) < 2^{-120}>>>, <<<abs(z)/min(abs(x), abs(y)) < 2^-120>>>)
relative to
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
far below the rounding error.  At 60 digits, the expansion differed
from
m4_math(<<<R_F>>>, <<<R_F>>>)
by about
m4_math(<<<z>>>, <<<z>>>)
relative, including for complex
m4_math(<<<x>>>, <<<x>>>)
and
m4_mathdot(<<<y>>>, <<<y>>>)
@item
@strong{Branch.}
m4_math(<<<\sqrt{x}>>>, <<<sqrt(x)>>>)
and
m4_math(<<<\sqrt{y}>>>, <<<sqrt(y)>>>)
are the principal square roots of the same arguments that
@code{bf-inverse-jacobi-rf} gives @code{bf-rf}, after making those with
a zero imaginary part real.  They are the values at
m4_math(<<<s = 0>>>, <<<s = 0>>>)
of the square roots in the integral, so the correction is on the same
branch as
m4_mathcomma(<<<R_F>>>, <<<R_F>>>)
with no choice to make.
@item
@strong{Underflow.}  The correction is computed as
m4_mathdot(<<<\left((f\sqrt{z})/\sqrt{x}\right)/\sqrt{y}>>>, <<<((f*sqrt(z))/sqrt(x))/sqrt(y)>>>)
Forming
m4_math(<<<f\sqrt{z}>>>, <<<f*sqrt(z)>>>)
first keeps it from underflowing unless the result itself does.  For
@code{inverse_jacobi_sn(1e300, 0.5)},
m4_math(<<<\sqrt{z}/(\sqrt{x}\sqrt{y})>>>, <<<sqrt(z)/(sqrt(x)*sqrt(y))>>>)
alone is about
m4_mathcomma(<<<2^{-1476}>>>, <<<2^-1476>>>)
but multiplied by
m4_math(<<<v \approx 2^{480}>>>, <<<v = 2^480>>>)
it is the real part of the result,
m4_mathdot(<<<1.414\times10^{-300}>>>, <<<1.414e-300>>>)
@item
m4_math(<<<R_F(x, y, 0)>>>, <<<R_F(x, y, 0)>>>)
needs nothing special: @code{bf-rf} handles a zero argument, as in
m4_mathdot(<<<K(m) = R_F(0, 1-m, 1)>>>, <<<K(m) = R_F(0, 1-m, 1)>>>)
@item
@strong{Bigfloats} do not underflow, so they always use
m4_math(<<<R_F>>>, <<<R_F>>>)
directly.
@end itemize

For @code{inverse_jacobi_sn(1e300, 0.5)},
m4_math(<<<x>>>, <<<x>>>)
and
m4_math(<<<y>>>, <<<y>>>)
are negative reals,
m4_math(<<<R_F(x, y, 0)>>>, <<<R_F(x, y, 0)>>>)
is purely imaginary, and the correction is the whole real part.  With
the expansion it is within 0.75 ulps; without it, it was 0.  The same
holds for @code{inverse_jacobi_cn(1e300*%i, 0.5)}.

An alternative is a formula for large
m4_math(<<<u>>>, <<<u>>>)
that makes the argument small, such as

m4_displaymath(
<<<{\rm sn}^{-1}(u | m) = \pm iK'(m) + {\rm sn}^{-1}\left({1 \over \sqrt{m}\, u} \Bigm| m\right)>>>
,
<<<@example
inverse_jacobi_sn(u, m) = +/-%i*K'(m) + inverse_jacobi_sn(1/(sqrt(m)*u), m)
@end example>>>
)

from
m4_math(<<<{\rm sn}(z + iK' | m) = 1/(\sqrt{m}\, {\rm sn}(z | m))>>>, <<<sn(z + %i*K'|m) = 1/(sqrt(m)*sn(z|m))>>>)
(@urldlmf{22.4.T3}).  A prototype fixed
@code{inverse_jacobi_sn(1e300, 0.5)}, but it chose the branch by
comparing with the result of the formula above, and that fails exactly
where the formula has lost the tiny part, since the candidates differ by
about that much.  The expansion has no such choice.

On 650 random points with
m4_math(<<<|u|>>>, <<<abs(u)>>>)
and
m4_math(<<<|m|>>>, <<<abs(m)>>>)
up to
m4_mathcomma(<<<2^{1020}>>>, <<<2^1020>>>)
the number of results with a real or imaginary part off by more than
16 ulps fell from 141 with
m4_math(<<<s = e>>>, <<<s = e>>>)
to 92 with the scale factor above, and to 78 with the expansion.  None
became worse.  Of the 78:

@itemize @bullet
@item
30 have a smaller part below the smallest double, such as about
m4_math(<<<2^{-1500}>>>, <<<2^-1500>>>)
for @code{inverse_jacobi_sn(2.0^1000, 2.0^1000)}, so 0 is the best a
double can do.
@item
33 have a smaller part below about
m4_math(<<<10^{-16}>>>, <<<1e-16>>>)
of the larger, which no evaluation of
m4_math(<<<R_F>>>, <<<R_F>>>)
resolves.  For @code{inverse_jacobi_sn(-1e300+1e290*%i, 3.0)}, the real
part is
m4_math(<<<-8.8\times10^{-75}>>>, <<<-8.8e-75>>>)
next to an imaginary part of 1.17.
@item
2 have a part that is exactly 0, where a tiny nonzero value is
returned.
@item
13 others.  Seven are moderate @code{inverse_jacobi_sn} cases with
complex
m4_mathcomma(<<<u>>>, <<<u>>>)
24 to 96 ulps off in the real part, which is extremely ill-conditioned
there: a change of 1 ulp in
m4_math(<<<u>>>, <<<u>>>)
changes it by about
m4_math(<<<10^{18}>>>, <<<1e18>>>)
ulps.  The rest are @code{inverse_jacobi_cn} and @code{inverse_jacobi_dn}
with huge arguments, 17 to 25 ulps off, and one @code{inverse_jacobi_dn}
case whose
m4_math(<<<m>>>, <<<m>>>)
has an imaginary part
m4_math(<<<10^{-303}>>>, <<<1e-303>>>)
of its real part, which was not investigated.
@end itemize

@node Why the Threshold Is 64
@ifinfo
@chapter Why the Threshold Is (> e 64)
@end ifinfo
@ifnotinfo
@chapter Why the Threshold Is @code{(> e 64)}
@end ifnotinfo

The threshold is a margin, not a derived constant.  Any value in a
wide range works.

@menu
* Lower Bound on the Threshold::
* Upper Bound on the Threshold::
* Effect of the Threshold::
@end menu

@node Lower Bound on the Threshold
@section Lower Bound: Scaling Must Only Shrink u

If scaling were applied for
m4_mathcomma(<<<e \le 0>>>, <<<e <= 0>>>)
then
m4_math(<<<s \le 0>>>, <<<s <= 0>>>)
and
m4_mathcomma(<<<t = 2^{-s} \ge 1>>>, <<<t = 2^(-s) >= 1>>>)
which enlarges the arguments instead of shrinking them.  For tiny
arguments this approaches overflow: with
m4_mathcomma(<<<|u| \approx 10^{-300}>>>, <<<abs(u) = 1e-300>>>)
m4_mathcomma(<<<e \approx -996>>>, <<<e = -996>>>)
m4_math(<<<s = -498>>>, <<<s = -498>>>)
and
m4_mathcomma(<<<t^2 = 2^{996}>>>, <<<t^2 = 2^996>>>)
at the overflow of @code{bf-rf}.  With the earlier
m4_mathcomma(<<<s = e>>>, <<<s = e>>>)
m4_mathcomma(<<<t^2 = 2^{1992}>>>, <<<t^2 = 2^1992>>>)
and a test run that always scaled failed in exactly this way.  So the
threshold must be at least 0.

@node Upper Bound on the Threshold
@section Upper Bound: The Unscaled Path Must Not Overflow

Without scaling, the largest argument of
m4_math(<<<R_F>>>, <<<R_F>>>)
is about
m4_mathdot(<<<|u|^2 \max(1, |m|) \approx 2^{2e}>>>, <<<abs(u)^2*max(1, abs(m)) = 2^(2*e)>>>)
@code{bf-rf} overflows near
m4_math(<<<2^{997}>>>, <<<2^997>>>)
(@pxref{The Problem}), so the unscaled path is safe while
m4_mathcomma(<<<2e \lesssim 996>>>, <<<2*e < 996>>>)
that is,
m4_mathdot(<<<e \lesssim 498>>>, <<<e < 498>>>)

With the threshold at 64, the unscaled path has arguments of at most
about
m4_mathcomma(<<<2^{128}>>>, <<<2^128>>>)
far from both limits.  It also bounds the arguments in the small
m4_math(<<<m>>>, <<<m>>>)
case of @code{inverse_jacobi_dn} (@pxref{Small m in inverse_jacobi_dn}).

@node Effect of the Threshold
@section Effect of the Threshold

Thresholds 0 and 64 were compared, with the earlier
m4_math(<<<s = e{:}>>>, <<<s = e:>>>)

@itemize @bullet
@item
On 1548 complex points concentrated near
m4_mathcomma(<<<u = \pm 1>>>, <<<u = +/-1>>>)
near
m4_mathcomma(<<<\sqrt{1-m}>>>, <<<sqrt(1-m)>>>)
and near
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
the results were identical.
@item
On 4346 @code{inverse_jacobi_sn}, @code{inverse_jacobi_cn} and
@code{inverse_jacobi_dn} points with
m4_math(<<<|u|>>>, <<<abs(u)>>>)
from
m4_math(<<<10^{-300}>>>, <<<1e-300>>>)
to
m4_math(<<<10^{300}>>>, <<<1e300>>>)
and
m4_math(<<<|m|>>>, <<<abs(m)>>>)
from
m4_math(<<<10^{-300}>>>, <<<1e-300>>>)
to
m4_mathcomma(<<<10^{100}>>>, <<<1e100>>>)
3 results differed, each with the same error against a 60-digit
reference.
@end itemize

So the choice has essentially no effect on accuracy.  A threshold of
64 leaves ordinary arguments on the unscaled path, so their results
are unchanged by the scaling code.  A threshold of 0 would state the
rule more simply: scale only when that shrinks
m4_mathdot(<<<u>>>, <<<u>>>)

@node Small m in inverse_jacobi_dn
@ifinfo
@chapter Small m in inverse_jacobi_dn
@end ifinfo
@ifnotinfo
@chapter Small @math{m} in @code{inverse_jacobi_dn}
@end ifnotinfo

@code{inverse_jacobi_dn} computes

m4_displaymath(
<<<z = {\sqrt{1-w^2} \over \sqrt{m}} \, R_F\left(w^2,\ {w^2-(1-m) \over m},\ 1\right),>>>
,
<<<@example
z = sqrt(1-w^2)*R_F(w^2, (w^2-(1-m))/m, 1)/sqrt(m)
@end example>>>
)

whose second argument is about
m4_mathcomma(<<<-1/m>>>, <<<-1/m>>>)
and so is the argument
m4_math(<<<(m-1)/m>>>, <<<(m-1)/m>>>)
in
m4_mathdot(<<<K(1/m)>>>, <<<K(1/m)>>>)
For tiny
m4_math(<<<m>>>, <<<m>>>)
these overflow, either in the division by
m4_math(<<<m>>>, <<<m>>>)
itself or in @code{bf-rf}.  For example,
@code{inverse_jacobi_dn(0.5, 1e-310)} overflowed computing
m4_mathdot(<<<-0.75/10^{-310}>>>, <<<-0.75/1e-310>>>)
The argument scaling described above does not help, because it shrinks
m4_math(<<<u>>>, <<<u>>>)
when
m4_math(<<<|u|>>>, <<<abs(u)>>>)
or
m4_math(<<<|mu^2|>>>, <<<abs(m*u^2)>>>)
is large, and here the large quantity comes from
m4_mathdot(<<<1/m>>>, <<<1/m>>>)
So for double-float
m4_math(<<<m>>>, <<<m>>>)
with
m4_math(<<<|m| < 2^{-500}>>>, <<<abs(m) < 2^-500>>>)
the arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
are multiplied by
m4_mathdot(<<<\lambda = 2^{-600}>>>, <<<lambda = 2^-600>>>)
By homogeneity,
m4_mathcomma(<<<R_F(\lambda a, \lambda b, \lambda c) = \lambda^{-1/2} R_F(a, b, c)>>>, <<<R_F(lambda*a, lambda*b, lambda*c) = lambda^(-1/2)*R_F(a, b, c)>>>)
so the scaled
m4_math(<<<R_F>>>, <<<R_F>>>)
is
m4_math(<<<2^{300}>>>, <<<2^300>>>)
times too large, and its result is multiplied by
m4_math(<<<\sqrt{\lambda} = 2^{-300}{:}>>>, <<<sqrt(lambda) = 2^-300:>>>)

m4_displaymath(
<<<z = {\sqrt{1-w^2}\,\sqrt{\lambda} \over \sqrt{m}} \,
R_F\left(\lambda w^2,\ {\lambda(w^2-(1-m)) \over m},\ \lambda\right).>>>
,
<<<@example
z = sqrt(1-w^2)*sqrt(lambda)*R_F(lambda*w^2, lambda*(w^2-(1-m))/m, lambda)/sqrt(m)
@end example>>>
)

The products
m4_math(<<<\lambda(w^2-(1-m))>>>, <<<lambda*(w^2-(1-m))>>>)
and
m4_math(<<<\lambda(m-1)>>>, <<<lambda*(m-1)>>>)
are formed before dividing by
m4_mathcomma(<<<m>>>, <<<m>>>)
so the quotients cannot overflow.  Because
m4_math(<<<\lambda>>>, <<<lambda>>>)
is a power of 2, the scaling is exact.

The function
m4_math(<<<{\rm dn}^{-1}(w | m)>>>, <<<inverse_jacobi_dn(w, m)>>>)
has no Taylor series in
m4_math(<<<m>>>, <<<m>>>)
about 0 that could replace this: at
m4_mathcomma(<<<m = 0>>>, <<<m = 0>>>)
m4_math(<<<{\rm dn}(z | 0) = 1>>>, <<<dn(z|0) = 1>>>)
for every
m4_mathcomma(<<<z>>>, <<<z>>>)
and for
m4_math(<<<w>>>, <<<w>>>)
outside
m4_math(<<<[\sqrt{1-m}, 1]>>>, <<<[sqrt(1-m), 1]>>>)
the result has an imaginary part that grows like
m4_mathdot(<<<{1\over 2}\log(16/m)>>>, <<<log(16/m)/2>>>)

The constants follow from the bounds above:

@itemize @bullet
@item
Without scaling
m4_math(<<<(|m| \ge 2^{-500}){:}>>>, <<<(abs(m) >= 2^-500):>>>)
m4_math(<<<w>>>, <<<w>>>)
is unscaled only while
m4_mathcomma(<<<|w| < 2^{64}>>>, <<<abs(w) < 2^64>>>)
so the second argument is at most about
m4_mathcomma(<<<2^{128+1+500} = 2^{629}>>>, <<<2^(128+1+500) = 2^629>>>)
below
m4_mathdot(<<<2^{997}>>>, <<<2^997>>>)
@item
With scaling
m4_math(<<<(|m| < 2^{-500}){:}>>>, <<<(abs(m) < 2^-500):>>>)
m4_math(<<<|m|>>>, <<<abs(m)>>>)
is at least the smallest denormal,
m4_mathcomma(<<<2^{-1074}>>>, <<<2^-1074>>>)
so the second argument is at most about
m4_mathdot(<<<2^{128+1+1074-600} = 2^{603}>>>, <<<2^(128+1+1074-600) = 2^603>>>)
@end itemize

An earlier version multiplied by
m4_mathdot(<<<\lambda = |m|>>>, <<<lambda = abs(m)>>>)
That pushed
m4_math(<<<\lambda w^2>>>, <<<lambda*w^2>>>)
into the denormal range for tiny
m4_mathcomma(<<<m>>>, <<<m>>>)
where doubles have few significant bits.  For
@code{inverse_jacobi_dn(1e-8*%i, 1e-300)}, that lost about
m4_math(<<<10^{-4}>>>, <<<1e-4>>>)
in the real part.  With
m4_math(<<<\lambda = 2^{-600}>>>, <<<lambda = 2^-600>>>)
the arguments stay normal.

@node Reciprocal Functions
@chapter The Reciprocal Functions

Write
m4_mathcomma(<<<{\rm ns}^{-1}>>>, <<<ns^-1>>>)
m4_math(<<<{\rm nc}^{-1}>>>, <<<nc^-1>>>)
and
m4_math(<<<{\rm nd}^{-1}>>>, <<<nd^-1>>>)
for @code{inverse_jacobi_ns}, @code{inverse_jacobi_nc} and
@code{inverse_jacobi_nd}.  Since
m4_mathcomma(<<<{\rm ns} = 1/{\rm sn}>>>, <<<ns = 1/sn>>>)
m4_math(<<<{\rm nc} = 1/{\rm cn}>>>, <<<nc = 1/cn>>>)
and
m4_mathcomma(<<<{\rm nd} = 1/{\rm dn}>>>, <<<nd = 1/dn>>>)

m4_displaymath(
<<<{\rm ns}^{-1}(u | m) = {\rm sn}^{-1}(1/u | m), \qquad
{\rm nc}^{-1}(u | m) = {\rm cn}^{-1}(1/u | m), \qquad
{\rm nd}^{-1}(u | m) = {\rm dn}^{-1}(1/u | m).>>>
,
<<<@example
inverse_jacobi_ns(u, m) = inverse_jacobi_sn(1/u, m)
inverse_jacobi_nc(u, m) = inverse_jacobi_cn(1/u, m)
inverse_jacobi_nd(u, m) = inverse_jacobi_dn(1/u, m)
@end example>>>
)

@code{bf-inverse-jacobi-ns}, @code{bf-inverse-jacobi-nc} and
@code{bf-inverse-jacobi-nd} use the formulas of @ref{The Formulas} with
the argument
m4_mathcomma(<<<1/u>>>, <<<1/u>>>)
but without forming
m4_mathdot(<<<1/u>>>, <<<1/u>>>)

@menu
* Why the Reciprocal Is Not Formed::
* Formulas for the Reciprocal Functions::
* Scaling for the Reciprocal Functions::
* Special Values of the Reciprocal Functions::
@end menu

@node Why the Reciprocal Is Not Formed
@section Why the Reciprocal Is Not Formed

Before the change, @code{inverse_jacobi_nc} and
@code{inverse_jacobi_nd} called @code{inverse_jacobi_cn} and
@code{inverse_jacobi_dn} with the rounded value of
m4_mathcomma(<<<1/u>>>, <<<1/u>>>)
and @code{inverse_jacobi_ns} computed
m4_mathdot(<<<F(\sin^{-1}(1/u) | m)>>>, <<<F(asin(1/u)|m)>>>)

Rounding
m4_math(<<<1/u>>>, <<<1/u>>>)
changes it by a relative amount of at most
m4_mathcomma(<<<\varepsilon/2>>>, <<<eps/2>>>)
the same as rounding
m4_math(<<<u>>>, <<<u>>>)
itself, so away from
m4_math(<<<u = \pm 1>>>, <<<u = +/-1>>>)
this is as accurate as the rest of the computation.  But every formula
of @ref{The Formulas} contains
m4_mathcomma(<<<1 - 1/u^2>>>, <<<1 - 1/u^2>>>)
and when
m4_math(<<<1/u>>>, <<<1/u>>>)
is near
m4_mathcomma(<<<\pm 1>>>, <<<+/-1>>>)
computing it from the rounded
m4_math(<<<1/u>>>, <<<1/u>>>)
cancels: a relative error
m4_math(<<<\delta>>>, <<<delta>>>)
in
m4_math(<<<1/u>>>, <<<1/u>>>)
becomes a relative error of about
m4_mathdot(<<<2\delta/|1 - 1/u^2|>>>, <<<2*delta/abs(1 - 1/u^2)>>>)
For example, @code{inverse_jacobi_nc(1 + 2.0^-30, 0.5)} returned
m4_mathcomma(<<<4.315837288520408\times10^{-5}>>>, <<<4.315837288520408e-5>>>)
about
m4_math(<<<3\times10^{6}>>>, <<<3e6>>>)
ulps from the correct value,
m4_mathdot(<<<4.3158372865106896\times10^{-5}>>>, <<<4.3158372865106896e-5>>>)

The reciprocal
m4_math(<<<1/u>>>, <<<1/u>>>)
also overflows for subnormal
m4_mathcomma(<<<u>>>, <<<u>>>)
so before the change @code{inverse_jacobi_ns(2.0^-1060, 0.5)}
signalled @code{FLOATING-POINT-OVERFLOW}.

@node Formulas for the Reciprocal Functions
@section Formulas for the Reciprocal Functions

@code{bf-inverse-jacobi-scale-recip} (@pxref{Scaling for the
Reciprocal Functions}) returns
m4_math(<<<v = t/u>>>, <<<v = t/u>>>)
and
m4_mathdot(<<<t>>>, <<<t>>>)
When it does not scale,
m4_math(<<<t = 1>>>, <<<t = 1>>>)
and
m4_math(<<<v>>>, <<<v>>>)
is
m4_math(<<<1/u>>>, <<<1/u>>>)
rounded.  The scaled formulas of @ref{Scaling the Formulas} then hold
unchanged with this
m4_mathcomma(<<<v>>>, <<<v>>>)
because they use the argument only through
m4_mathcomma(<<<v>>>, <<<v>>>)
m4_math(<<<t>>>, <<<t>>>)
and
m4_mathdot(<<<t^2 - v^2 = t^2(1 - 1/u^2)>>>, <<<t^2 - v^2 = t^2*(1 - 1/u^2)>>>)
The last is computed by @code{bf-diff-of-recip-squares}:

m4_displaymath(
<<<t^2 - v^2 = \cases{
-(1-u^2)\, v^2, & $|u| < 2$, \cr
(t-v)(t+v), & $|u| \ge 2$.
}>>>
,
<<<@example
t^2 - v^2 = -(1-u^2)*v^2     if abs(u) < 2
t^2 - v^2 = (t-v)*(t+v)      if abs(u) >= 2
@end example>>>
)

For
m4_mathcomma(<<<|u| < 2>>>, <<<abs(u) < 2>>>)
m4_math(<<<1-u^2>>>, <<<1-u^2>>>)
is computed from
m4_math(<<<u>>>, <<<u>>>)
as described in @ref{Rounding}, so it is accurate near
m4_mathcomma(<<<u = \pm 1>>>, <<<u = +/-1>>>)
and
m4_math(<<<v^2>>>, <<<v^2>>>)
carries only the rounding error of
m4_mathcomma(<<<v>>>, <<<v>>>)
a relative error.  For
m4_mathcomma(<<<|u| \ge 2>>>, <<<abs(u) >= 2>>>)
m4_mathcomma(<<<|v| \le t/2>>>, <<<abs(v) <= t/2>>>)
so the difference does not cancel.  Everywhere else, the rounding error
of
m4_math(<<<v>>>, <<<v>>>)
enters the formulas only as a small relative change, as discussed
above.  After this, @code{bf-inverse-jacobi-ns},
@code{bf-inverse-jacobi-nc} and @code{bf-inverse-jacobi-nd} share the
code of @code{bf-inverse-jacobi-sn}, @code{bf-inverse-jacobi-cn} and
@code{bf-inverse-jacobi-dn}: @code{bf-inverse-jacobi-sn-aux},
@code{bf-inverse-jacobi-cn-aux} and @code{bf-inverse-jacobi-dn-aux}.

On random points, with
m4_math(<<<m>>>, <<<m>>>)
= 0.3, 0.9,
m4_math(<<<-2>>>, <<<-2>>>)
and 3, the worst relative errors
m4_mathcomma(<<<|z - z_{\rm exact}|/|z_{\rm exact}|>>>, <<<abs(z - z_exact)/abs(z_exact)>>>)
in units of
m4_mathcomma(<<<2^{-53}>>>, <<<2^-53>>>)
were:

@multitable @columnfractions 0.24 0.12 0.16 0.2 0.28
@headitem @math{u} @tab @code{ns} @tab @code{ns} via @math{1/u} @tab @code{nc}, @code{nd} @tab @code{nc}, @code{nd} via @math{1/u}
@item @math{|u|} in [2, 1e5] @tab 4.8 @tab 4.8 @tab 8.9 @tab 8.9
@item @math{|u|} in [1e-5, 0.5] @tab 4.7 @tab 4.7 @tab 6.8 @tab 5.2
@item @math{|u - 1|} or @math{|u + 1|} < 0.5 @tab 4.4 @tab 42 @tab 6.9 @tab 1.3e3
@item @math{|u - 1|} or @math{|u + 1|} < 1e-6 @tab 5.2 @tab 1.4e4 @tab 5.3 @tab 4.7e8
@end multitable

The points in the last two rows are complex.  The first two rows
include real and complex points.

The branch is chosen from
m4_math(<<<u>>>, <<<u>>>)
as it would be from
m4_mathdot(<<<1/u>>>, <<<1/u>>>)
The real part of
m4_math(<<<1/u>>>, <<<1/u>>>)
has the sign of
m4_mathcomma(<<<{\rm Re}\, u>>>, <<<realpart(u)>>>)
so the reflection of @ref{The Formulas} is used when
m4_mathdot(<<<{\rm Re}\, u < 0>>>, <<<realpart(u) < 0>>>)
Since
m4_mathcomma(<<<1/(iy) = -i/y>>>, <<<1/(%i*y) = -%i/y>>>)
the reciprocal
m4_math(<<<1/u>>>, <<<1/u>>>)
is on the negative imaginary axis when
m4_math(<<<u>>>, <<<u>>>)
is on the positive one, so the reflection of @ref{The Imaginary Axis}
is used when
m4_math(<<<\bar u>>>, <<<conjugate(u)>>>)
is on the negative imaginary axis.  The real form of
m4_math(<<<{\rm dn}^{-1}>>>, <<<dn^-1>>>)
(@pxref{inverse_jacobi_dn}) is used for real
m4_mathcomma(<<<u \ge 1>>>, <<<u >= 1>>>)
which corresponds to
m4_mathdot(<<<1/u \in (0, 1]>>>, <<<1/u in (0, 1]>>>)

@node Scaling for the Reciprocal Functions
@section Scaling for the Reciprocal Functions

@code{bf-inverse-jacobi-scale-recip} is @code{bf-inverse-jacobi-scale}
(@pxref{Choosing the Scale Factor}) for the argument
m4_mathdot(<<<1/u>>>, <<<1/u>>>)
Since
m4_mathcomma(<<<|u| \ge 2^{e_u - 1}>>>, <<<abs(u) >= 2^(e_u - 1)>>>)
m4_mathcomma(<<<|1/u| \le 2^{1-e_u}>>>, <<<abs(1/u) <= 2^(1-e_u)>>>)
and it takes

m4_displaymath(
<<<e = (2 - e_u) + \cases{
\lceil e_m/2 \rceil, & $|m| > 1$, \cr
0, & otherwise.
}>>>
,
<<<@example
e = (2 - e_u) + ceiling(e_m/2)   if abs(m) > 1
e = 2 - e_u                      otherwise
@end example>>>
)

The threshold
m4_math(<<<e > 64>>>, <<<e > 64>>>)
and
m4_math(<<<s = \max(\lceil e/2\rceil,\ e - 480)>>>, <<<s = max(ceiling(e/2), e - 480)>>>)
are those of @ref{Choosing the Scale Factor} and @ref{Why the Threshold
Is 64}, and
m4_math(<<<v = 2^{-s}/u>>>, <<<v = 2^-s/u>>>)
is computed by one division, so
m4_math(<<<1/u>>>, <<<1/u>>>)
is never formed.  Whenever it scales,
m4_mathcomma(<<<|v| \le 2^{e-1-s} \le 2^{479}>>>, <<<abs(v) <= 2^(e-1-s) <= 2^479>>>)
even for subnormal
m4_mathdot(<<<u>>>, <<<u>>>)
As for @code{inverse_jacobi_dn}, @code{inverse_jacobi_nd} passes
m4_mathdot(<<<m = 0>>>, <<<m = 0>>>)

For example, @code{inverse_jacobi_ns(2.0^-1060, 0.5)} is now
m4_mathdot(<<<1.1447501014141682\times10^{-319} - 1.8540746773013717\, i>>>, <<<1.1447501014141682e-319 - 1.8540746773013717*%i>>>)
The tiny real part comes from the expansion described in @ref{When the
Smallest Argument Is Negligible}.

@node Special Values of the Reciprocal Functions
@section Special Values of the Reciprocal Functions

For
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
m4_mathcomma(<<<{\rm nc}(z | 1) = {\rm nd}(z | 1) = \cosh z>>>, <<<nc(z|1) = nd(z|1) = cosh(z)>>>)
so @code{bf-inverse-jacobi-nc} and @code{bf-inverse-jacobi-nd} call
Maxima's @code{acosh} through @code{bf-inverse-jacobi-acosh}, before
the reflection, as in @ref{The Case m = 1}.  For
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
m4_math(<<<{\rm ns}^{-1}>>>, <<<ns^-1>>>)
has poles at
m4_mathcomma(<<<u = \pm 1>>>, <<<u = +/-1>>>)
as
m4_math(<<<{\rm sn}^{-1}>>>, <<<sn^-1>>>)
does (@pxref{Poles of inverse_jacobi_sn}), and
@code{bf-inverse-jacobi-ns} signals the same kind of error.

For
m4_mathcomma(<<<m \ne 1>>>, <<<m # 1>>>)
m4_math(<<<u = 0>>>, <<<u = 0>>>)
is on a branch cut of all three functions.  The cuts of
m4_mathcomma(<<<{\rm sn}^{-1}>>>, <<<sn^-1>>>)
m4_math(<<<{\rm cn}^{-1}>>>, <<<cn^-1>>>)
and
m4_math(<<<{\rm dn}^{-1}>>>, <<<dn^-1>>>)
include the real
m4_math(<<<x>>>, <<<x>>>)
with
m4_mathcomma(<<<|x| > 1>>>, <<<abs(x) > 1>>>)
and
m4_math(<<<1/u>>>, <<<1/u>>>)
maps these to the real
m4_math(<<<u>>>, <<<u>>>)
in
m4_mathdot(<<<(-1, 1)>>>, <<<(-1, 1)>>>)
The limits from the two sides of
m4_math(<<<u = 0>>>, <<<u = 0>>>)
differ.  For example, @code{inverse_jacobi_ns(1e-200, 0.5)} is
m4_mathcomma(<<<1.414\times10^{-200} - 1.8541\, i>>>, <<<1.414e-200 - 1.8541*%i>>>)
and @code{inverse_jacobi_ns(-1e-200, 0.5)} is
m4_mathcomma(<<<-1.414\times10^{-200} + 1.8541\, i>>>, <<<-1.414e-200 + 1.8541*%i>>>)
so the limits of
m4_math(<<<{\rm ns}^{-1}>>>, <<<ns^-1>>>)
are
m4_mathdot(<<<\mp iK'(m)>>>, <<<-/+ %i*K'(m)>>>)
The three functions signal an error for
m4_mathcomma(<<<u = 0>>>, <<<u = 0>>>)
such as
@code{inverse_jacobi_nc: inverse_jacobi_nc(0.0, 0.5) is undefined.}
Before the change,
m4_math(<<<u = 0>>>, <<<u = 0>>>)
gave a division-by-zero error.  For
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
@code{inverse_jacobi_nc(0.0, 1.0)} and
@code{inverse_jacobi_nd(0.0, 1.0)} are
m4_mathdot(<<<\cosh^{-1} 0 = i\pi/2>>>, <<<acosh(0) = %i*%pi/2>>>)

@node The Other Six Functions
@chapter The Other Six Functions

The remaining functions are computed from the same
m4_math(<<<R_F>>>, <<<R_F>>>)
forms and share most of the code.  Write
m4_math(<<<{\rm pq}^{-1}>>>, <<<pq^-1>>>)
for @code{inverse_jacobi_pq}.

@multitable @columnfractions 0.12 0.48 0.40
@headitem Function @tab Computed as @tab Shared code
@item @code{sc} @tab @math{u R_F(1+u^2, 1+(1-m)u^2, 1)} @tab @code{bf-inverse-jacobi-sn-aux}
@item @code{sd} @tab @math{u R_F(1-(1-m)u^2, 1+m u^2, 1)} @tab @code{bf-inverse-jacobi-sd-aux}, @code{bf-inverse-jacobi-sn-aux}
@item @code{cs} @tab @code{sc} at @math{1/u} @tab @code{bf-inverse-jacobi-sn-aux}
@item @code{ds} @tab @code{sd} at @math{1/u} @tab @code{bf-inverse-jacobi-sd-aux}
@item @code{cd} @tab @code{cn} with parameter @math{m/(m-1)}, divided by @code{sqrt(1-m)} @tab @code{bf-inverse-jacobi-cd-aux}
@item @code{dc} @tab @code{cd} at @math{1/u} @tab @code{bf-inverse-jacobi-cd-aux}
@end multitable

The reciprocal functions
m4_mathcomma(<<<{\rm cs}^{-1}>>>, <<<cs^-1>>>)
m4_math(<<<{\rm ds}^{-1}>>>, <<<ds^-1>>>)
and
m4_math(<<<{\rm dc}^{-1}>>>, <<<dc^-1>>>)
never form
m4_mathdot(<<<1/u>>>, <<<1/u>>>)
They use @code{bf-inverse-jacobi-scale-recip} and
@code{bf-diff-of-recip-squares} as described in @ref{Reciprocal
Functions}.

Before the change, each of these was computed by passing an algebraic
expression in
m4_math(<<<u>>>, <<<u>>>)
to another inverse function:
m4_math(<<<{\rm sn}^{-1}(u/\sqrt{1+u^2})>>>, <<<inverse_jacobi_sn(u/sqrt(1+u^2))>>>)
for
m4_mathcomma(<<<{\rm sc}^{-1}>>>, <<<sc^-1>>>)
m4_math(<<<{\rm sn}^{-1}(u/\sqrt{1+mu^2})>>>, <<<inverse_jacobi_sn(u/sqrt(1+m*u^2))>>>)
for
m4_mathcomma(<<<{\rm sd}^{-1}>>>, <<<sd^-1>>>)
m4_math(<<<{\rm sn}^{-1}(\sqrt{1-u^2}/\sqrt{1-mu^2})>>>, <<<inverse_jacobi_sn(sqrt(1-u^2)/sqrt(1-m*u^2))>>>)
for
m4_mathcomma(<<<{\rm cd}^{-1}>>>, <<<cd^-1>>>)
and the function of
m4_math(<<<1/u>>>, <<<1/u>>>)
for the reciprocals.  These overflowed when
m4_math(<<<u^2>>>, <<<u^2>>>)
did, lost accuracy wherever the expression was rounded near a branch
point, and, because the square roots are principal, sometimes returned
the inverse of
m4_math(<<<-u>>>, <<<-u>>>)
instead of
m4_mathdot(<<<u>>>, <<<u>>>)

@menu
* inverse_jacobi_sc and inverse_jacobi_sd::
* inverse_jacobi_cs and inverse_jacobi_ds::
* inverse_jacobi_cd::
* Real m Greater Than 1::
* inverse_jacobi_dc::
* Accuracy of the Other Six Functions::
@end menu

@node inverse_jacobi_sc and inverse_jacobi_sd
@ifinfo
@section inverse_jacobi_sc and inverse_jacobi_sd
@end ifinfo
@ifnotinfo
@section @code{inverse_jacobi_sc} and @code{inverse_jacobi_sd}
@end ifnotinfo

The function
m4_math(<<<{\rm sc}^{-1}(u | m)>>>, <<<inverse_jacobi_sc(u, m)>>>)
is
m4_math(<<<F(\tan^{-1} u | m)>>>, <<<F(atan(u)|m)>>>)
(@urlaands{eqn 17.4.41, 596}).  With
m4_mathcomma(<<<{\rm sn} = u/\sqrt{1+u^2}>>>, <<<sn = u/sqrt(1+u^2)>>>)
m4_math(<<<{\rm cn}^2 = 1/(1+u^2)>>>, <<<cn^2 = 1/(1+u^2)>>>)
and
m4_mathcomma(<<<{\rm dn}^2 = (1+(1-m)u^2)/(1+u^2)>>>, <<<dn^2 = (1+(1-m)*u^2)/(1+u^2)>>>)
homogeneity (@pxref{Homogeneity of R_F}) gives

m4_displaymath(
<<<{\rm sc}^{-1}(u | m) = u \, R_F(1+u^2,\ 1+(1-m)u^2,\ 1).>>>
,
<<<@example
inverse_jacobi_sc(u, m) = u*R_F(1+u^2, 1+(1-m)*u^2, 1)
@end example>>>
)

Similarly, with
m4_mathcomma(<<<{\rm sn} = u/\sqrt{1+mu^2}>>>, <<<sn = u/sqrt(1+m*u^2)>>>)
m4_math(<<<{\rm cn}^2 = (1-(1-m)u^2)/(1+mu^2)>>>, <<<cn^2 = (1-(1-m)*u^2)/(1+m*u^2)>>>)
and
m4_mathcomma(<<<{\rm dn}^2 = 1/(1+mu^2)>>>, <<<dn^2 = 1/(1+m*u^2)>>>)

m4_displaymath(
<<<{\rm sd}^{-1}(u | m) = u \, R_F(1-(1-m)u^2,\ 1+mu^2,\ 1).>>>
,
<<<@example
inverse_jacobi_sd(u, m) = u*R_F(1-(1-m)*u^2, 1+m*u^2, 1)
@end example>>>
)

Both have the shape of the
m4_math(<<<{\rm sn}^{-1}>>>, <<<sn^-1>>>)
formula,
m4_math(<<<u \, R_F(1+au^2,\ 1+bu^2,\ 1){:}>>>, <<<u*R_F(1+a*u^2, 1+b*u^2, 1):>>>)

@multitable @columnfractions 0.25 0.25 0.25 0.25
@headitem Function @tab @math{a} @tab @math{b} @tab @math{b - a}
@item @code{sn} @tab @math{-1} @tab @math{-m} @tab @math{1-m}
@item @code{sc} @tab @math{1} @tab @math{1-m} @tab @math{-m}
@item @code{sd} @tab @math{-(1-m)} @tab @math{m} @tab @math{1}
@end multitable

@code{bf-inverse-jacobi-sn-aux} computes the second argument as either
m4_math(<<<1 + bu^2>>>, <<<1 + b*u^2>>>)
or
m4_math(<<<(1 + au^2) + (b-a)u^2>>>, <<<(1 + a*u^2) + (b-a)*u^2>>>)
(@pxref{Choosing Among Equivalent Forms}).  It takes
m4_math(<<<b - a>>>, <<<b - a>>>)
as an argument (@code{m1}) instead of computing it, so each caller can
pass the exact value.  As for
m4_mathcomma(<<<{\rm sn}^{-1}>>>, <<<sn^-1>>>)
the factor
m4_math(<<<u>>>, <<<u>>>)
carries the sign, so no reflection is needed:
m4_math(<<<{\rm Re}(\tan^{-1} u)>>>, <<<realpart(atan(u))>>>)
is always in
m4_mathdot(<<<(-\pi/2, \pi/2)>>>, <<<(-%pi/2, %pi/2)>>>)

The first argument is computed accurately:

@itemize @bullet
@item
For
m4_mathcomma(<<<{\rm sc}^{-1}>>>, <<<sc^-1>>>)
m4_math(<<<1 + u^2 = 1 - (iu)^2>>>, <<<1 + u^2 = 1 - (%i*u)^2>>>)
is computed by @code{bf-diff-of-squares}, which keeps the imaginary
part when
m4_math(<<<u>>>, <<<u>>>)
is near
m4_mathdot(<<<\pm i>>>, <<<+/- %i>>>)
@item
For
m4_mathcomma(<<<{\rm sd}^{-1}>>>, <<<sd^-1>>>)
@code{bf-inverse-jacobi-sd-aux} computes
m4_math(<<<1-(1-m)u^2>>>, <<<1-(1-m)*u^2>>>)
as either
m4_math(<<<1-(1-m)u^2>>>, <<<1-(1-m)*u^2>>>)
or
m4_mathcomma(<<<(1-u^2) + mu^2>>>, <<<(1-u^2) + m*u^2>>>)
whichever has smaller terms.  The first cancels near the branch points
m4_mathcomma(<<<u = \pm 1/\sqrt{1-m}>>>, <<<u = +/- 1/sqrt(1-m)>>>)
where the rounding of
m4_math(<<<1-m>>>, <<<1-m>>>)
matters when
m4_math(<<<m>>>, <<<m>>>)
is small.  The second is accurate there but cancels when
m4_math(<<<u>>>, <<<u>>>)
is large and
m4_math(<<<m>>>, <<<m>>>)
is near 1.
@end itemize

The imaginary transformation
m4_math(<<<{\rm sc}^{-1}(u | m) = -i \, {\rm sn}^{-1}(iu | 1-m)>>>, <<<inverse_jacobi_sc(u, m) = -%i*inverse_jacobi_sn(%i*u, 1-m)>>>)
(@urlaands{eqn 16.20.1, 574}) was not used, because it rounds
m4_mathdot(<<<1-m>>>, <<<1-m>>>)
That loses accuracy near the branch points
m4_math(<<<u = \pm i/\sqrt{1-m}>>>, <<<u = +/- %i/sqrt(1-m)>>>)
when
m4_math(<<<m>>>, <<<m>>>)
is small and
m4_math(<<<1-m>>>, <<<1-m>>>)
is not exact: about
m4_math(<<<2\times10^{9}>>>, <<<2e9>>>)
ulps at
m4_mathdot(<<<m = 10^{-10}>>>, <<<m = 1e-10>>>)
There is no such transformation from
m4_math(<<<{\rm sd}>>>, <<<sd>>>)
to
m4_mathcomma(<<<{\rm sn}>>>, <<<sn>>>)
since
m4_mathdot(<<<{\rm sd}(iz | m) = i \, {\rm sd}(z | 1-m)>>>, <<<sd(%i*z|m) = %i*sd(z|1-m)>>>)

For
m4_mathcomma(<<<m = 0>>>, <<<m = 0>>>)
m4_math(<<<{\rm sc}^{-1}>>>, <<<sc^-1>>>)
has poles at
m4_mathcomma(<<<u = \pm i>>>, <<<u = +/- %i>>>)
where
m4_math(<<<R_F(0, 0, 1)>>>, <<<R_F(0, 0, 1)>>>)
does not converge, and @code{bf-inverse-jacobi-sc} signals an error.
The function
m4_math(<<<{\rm sd}^{-1}>>>, <<<sd^-1>>>)
has no poles: two arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
are never 0 together.

Examples of the old results:

@itemize @bullet
@item
@code{inverse_jacobi_sc(2.0*%i, 0.5)} returned
m4_mathcomma(<<<1.854 - 0.826i>>>, <<<1.854 - 0.826*%i>>>)
an inverse of
m4_mathdot(<<<-2i>>>, <<<-2*%i>>>)
It is now
m4_mathdot(<<<1.8540746773013719 + 0.8260178762492453i>>>, <<<1.8540746773013719 + 0.8260178762492453*%i>>>)
@item
@code{inverse_jacobi_sc(1e10, 0.5)} returned
m4_mathcomma(<<<1.8540746773013719 = K(0.5)>>>, <<<1.8540746773013719 = K(0.5)>>>)
losing the term of order
m4_mathdot(<<<1/u>>>, <<<1/u>>>)
It is now
m4_mathdot(<<<1.8540746771599506>>>, <<<1.8540746771599506>>>)
@item
@code{inverse_jacobi_sc(1e200, 0.5)} and
@code{inverse_jacobi_sd(1e200, 0.5)} overflowed.
@item
@code{inverse_jacobi_sd(2.0, -3.0)} returned
m4_mathcomma(<<<-0.14800957393120567 - 0.842875177406298i>>>, <<<-0.14800957393120567 - 0.842875177406298*%i>>>)
where the real part has the wrong sign.  It is now
m4_mathdot(<<<0.14800957393120565 - 0.842875177406298i>>>, <<<0.14800957393120565 - 0.842875177406298*%i>>>)
@end itemize

@node inverse_jacobi_cs and inverse_jacobi_ds
@ifinfo
@section inverse_jacobi_cs and inverse_jacobi_ds
@end ifinfo
@ifnotinfo
@section @code{inverse_jacobi_cs} and @code{inverse_jacobi_ds}
@end ifnotinfo

These are
m4_math(<<<{\rm sc}^{-1}(1/u)>>>, <<<inverse_jacobi_sc(1/u)>>>)
and
m4_mathcomma(<<<{\rm sd}^{-1}(1/u)>>>, <<<inverse_jacobi_sd(1/u)>>>)
computed as described in @ref{Reciprocal Functions} with
m4_mathdot(<<<v = t/u>>>, <<<v = t/u>>>)

For
m4_mathcomma(<<<{\rm cs}^{-1}>>>, <<<cs^-1>>>)
the first argument of
m4_math(<<<R_F>>>, <<<R_F>>>)
is
m4_mathdot(<<<t^2 + v^2 = t^2 - (iv)^2>>>, <<<t^2 + v^2 = t^2 - (%i*v)^2>>>)
Since
m4_mathcomma(<<<iv = t/(-iu)>>>, <<<%i*v = t/(-%i*u)>>>)
@code{bf-diff-of-recip-squares} with
m4_math(<<<iv>>>, <<<%i*v>>>)
and
m4_math(<<<-iu>>>, <<<-%i*u>>>)
computes it as
m4_math(<<<(1+u^2)\, v^2>>>, <<<(1+u^2)*v^2>>>)
for
m4_mathcomma(<<<|u| < 2>>>, <<<abs(u) < 2>>>)
which is accurate near the poles
m4_mathdot(<<<u = \pm i>>>, <<<u = +/- %i>>>)
Computing it from the rounded
m4_mathcomma(<<<1/u>>>, <<<1/u>>>)
as before, lost up to about 600 units of
m4_math(<<<2^{-53}>>>, <<<2^-53>>>)
there; it is now within 5.

For
m4_mathcomma(<<<{\rm ds}^{-1}>>>, <<<ds^-1>>>)
@code{bf-inverse-jacobi-sd-aux} is called with
m4_math(<<<t^2 - v^2>>>, <<<t^2 - v^2>>>)
from @code{bf-diff-of-recip-squares}.  Near
m4_mathcomma(<<<u = \pm\sqrt{1-m}>>>, <<<u = +/- sqrt(1-m)>>>)
the image of the branch points of
m4_mathcomma(<<<{\rm sd}^{-1}>>>, <<<sd^-1>>>)
the error is up to about 550 units of
m4_mathcomma(<<<2^{-53}>>>, <<<2^-53>>>)
the same as from the rounded
m4_mathdot(<<<1/u>>>, <<<1/u>>>)
This is the limit that
m4_math(<<<{\rm dn}^{-1}>>>, <<<dn^-1>>>)
also has next to
m4_math(<<<\sqrt{1-m}{:}>>>, <<<sqrt(1-m):>>>)
the rounding of
m4_math(<<<u^2>>>, <<<u^2>>>)
or of
m4_math(<<<1/u>>>, <<<1/u>>>)
costs about
m4_math(<<<\varepsilon>>>, <<<eps>>>)
divided by the distance to the branch point.

At
m4_math(<<<u = 0{:}>>>, <<<u = 0:>>>)

@itemize @bullet
@item
m4_mathcomma(<<<{\rm cs}^{-1}(0 | m) = K(m)>>>, <<<inverse_jacobi_cs(0, m) = K(m)>>>)
which is what the simplifier returns for an exact 0.
@code{bf-inverse-jacobi-cs} returns @code{(bf-elliptic-k m)}.
@item
The point
m4_math(<<<u = 0>>>, <<<u = 0>>>)
is on a branch cut of
m4_mathcomma(<<<{\rm ds}^{-1}>>>, <<<ds^-1>>>)
as for
m4_math(<<<{\rm ns}^{-1}>>>, <<<ns^-1>>>)
(@pxref{Special Values of the Reciprocal Functions}), so
@code{bf-inverse-jacobi-ds} signals an error.
@item
For
m4_mathcomma(<<<m = 0>>>, <<<m = 0>>>)
m4_math(<<<{\rm cs}^{-1}>>>, <<<cs^-1>>>)
has poles at
m4_mathdot(<<<u = \pm i>>>, <<<u = +/- %i>>>)
@end itemize

@node inverse_jacobi_cd
@ifinfo
@section inverse_jacobi_cd
@end ifinfo
@ifnotinfo
@section @code{inverse_jacobi_cd}
@end ifnotinfo

With
m4_mathcomma(<<<{\rm cd}(z) = u>>>, <<<cd(z) = u>>>)
m4_mathcomma(<<<{\rm sn}^2 = (1-u^2)/(1-mu^2)>>>, <<<sn^2 = (1-u^2)/(1-m*u^2)>>>)
m4_math(<<<{\rm cn}^2 = (1-m)u^2/(1-mu^2)>>>, <<<cn^2 = (1-m)*u^2/(1-m*u^2)>>>)
and
m4_mathdot(<<<{\rm dn}^2 = (1-m)/(1-mu^2)>>>, <<<dn^2 = (1-m)/(1-m*u^2)>>>)
Multiplying the arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
by
m4_math(<<<1-mu^2>>>, <<<1-m*u^2>>>)
gives
m4_mathcomma(<<<\sqrt{1-u^2} \, R_F((1-m)u^2,\ 1-mu^2,\ 1-m)>>>, <<<sqrt(1-u^2)*R_F((1-m)*u^2, 1-m*u^2, 1-m)>>>)
but that factor is not real and positive in general.  A first version
used this form for all
m4_mathcomma(<<<m>>>, <<<m>>>)
and for
m4_math(<<<m > 1>>>, <<<m > 1>>>)
and for complex
m4_math(<<<m>>>, <<<m>>>)
it often returned an inverse of
m4_mathdot(<<<-u>>>, <<<-u>>>)

Instead, the imaginary-modulus transformation
m4_math(<<<{\rm cn}(z, ik) = {\rm cd}(z/k_1', k_1)>>>, <<<cn(z, %i*k) = cd(z/k1', k1)>>>)
(@urldlmf{22.17.E7}), with
m4_math(<<<k_1^2 = m>>>, <<<k1^2 = m>>>)
and
m4_mathcomma(<<<-k^2 = m/(m-1)>>>, <<<-k^2 = m/(m-1)>>>)
gives
m4_mathcomma(<<<{\rm cd}(z | m) = {\rm cn}(\sqrt{1-m}\, z | m/(m-1))>>>, <<<cd(z|m) = cn(sqrt(1-m)*z | m/(m-1))>>>)
so

m4_displaymath(
<<<{\rm cd}^{-1}(u | m) = {{\rm cn}^{-1}(u | m/(m-1)) \over \sqrt{1-m}}.>>>
,
<<<@example
inverse_jacobi_cd(u, m) = inverse_jacobi_cn(u, m/(m-1))/sqrt(1-m)
@end example>>>
)

With
m4_mathcomma(<<<1 - {m \over m-1}(1-u^2) = {1-mu^2 \over 1-m}>>>, <<<1 - m/(m-1)*(1-u^2) = (1-m*u^2)/(1-m)>>>)
the formula of @ref{inverse_jacobi_cn} becomes

m4_displaymath(
<<<{\rm cd}^{-1}(u | m) = \cases{
{\sqrt{1-u^2} \over \sqrt{1-m}} \, R_F\left(u^2,\ {1-mu^2 \over 1-m},\ 1\right), & ${\rm Re}\, u \ge 0$, \cr
{2K(m/(m-1)) \over \sqrt{1-m}} - {\rm cd}^{-1}(-u | m), & ${\rm Re}\, u < 0$,
}>>>
,
<<<@example
                          / sqrt(1-u^2)*R_F(u^2, (1-m*u^2)/(1-m), 1)/sqrt(1-m),  realpart(u) >= 0
inverse_jacobi_cd(u, m) = <
                          \ 2*K(m/(m-1))/sqrt(1-m) - inverse_jacobi_cd(-u, m),  realpart(u) < 0
@end example>>>
)

with
m4_mathdot(<<<K(m/(m-1)) = R_F(0,\ 1/(1-m),\ 1)>>>, <<<K(m/(m-1)) = R_F(0, 1/(1-m), 1)>>>)
The reflection is the one for
m4_mathcomma(<<<{\rm cn}^{-1}>>>, <<<cn^-1>>>)
since
m4_mathcomma(<<<{\rm cd}(2K - z) = -{\rm cd}(z)>>>, <<<cd(2*K - z) = -cd(z)>>>)
and so is the treatment of the negative imaginary axis (@pxref{The
Imaginary Axis}).  This is the same structure as
m4_mathcomma(<<<{\rm dn}^{-1}>>>, <<<dn^-1>>>)
which is
m4_math(<<<{\rm cn}^{-1}>>>, <<<cn^-1>>>)
with parameter
m4_math(<<<1/m>>>, <<<1/m>>>)
divided by
m4_math(<<<\sqrt m>>>, <<<sqrt(m)>>>)
(@pxref{inverse_jacobi_dn}), and it is computed the same way:

@itemize @bullet
@item
For real
m4_mathcomma(<<<m < 1>>>, <<<m < 1>>>)
m4_math(<<<1-m>>>, <<<1-m>>>)
is real and positive, so multiplying the arguments of
m4_math(<<<R_F>>>, <<<R_F>>>)
by it gives a form with no division,
m4_mathcomma(<<<\sqrt{1-u^2} \, R_F((1-m)u^2,\ 1-mu^2,\ 1-m)>>>, <<<sqrt(1-u^2)*R_F((1-m)*u^2, 1-m*u^2, 1-m)>>>)
and the reflection term is
m4_mathdot(<<<2K(m)>>>, <<<2*K(m)>>>)
@item
Otherwise, the arguments are divided by
m4_mathdot(<<<1-m>>>, <<<1-m>>>)
When
m4_mathcomma(<<<|1-m| < 2^{-500}>>>, <<<abs(1-m) < 2^-500>>>)
they are first multiplied by
m4_mathcomma(<<<\lambda = 2^{-600}>>>, <<<lambda = 2^-600>>>)
and the result by
m4_mathcomma(<<<\sqrt\lambda>>>, <<<sqrt(lambda)>>>)
as for small
m4_math(<<<m>>>, <<<m>>>)
in
m4_math(<<<{\rm dn}^{-1}>>>, <<<dn^-1>>>)
(@pxref{Small m in inverse_jacobi_dn}).
@item
The value
m4_math(<<<1-mu^2>>>, <<<1-m*u^2>>>)
is computed as either
m4_math(<<<1-mu^2>>>, <<<1-m*u^2>>>)
or
m4_mathcomma(<<<(1-u^2) + (1-m)u^2>>>, <<<(1-u^2) + (1-m)*u^2>>>)
as in @code{bf-inverse-jacobi-sn-aux}, and
m4_math(<<<u>>>, <<<u>>>)
is scaled with @code{bf-inverse-jacobi-scale} as for
m4_mathdot(<<<{\rm cn}^{-1}>>>, <<<cn^-1>>>)
@item
For
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
m4_math(<<<{\rm cd}(z | 1) = 1>>>, <<<cd(z|1) = 1>>>)
for every
m4_mathcomma(<<<z>>>, <<<z>>>)
so @code{bf-inverse-jacobi-cd} signals an error.
@end itemize

The result is
m4_math(<<<K(m) - {\rm sn}^{-1}(u | m)>>>, <<<K(m) - inverse_jacobi_sn(u, m)>>>)
for real
m4_mathcomma(<<<m < 1>>>, <<<m < 1>>>)
since
m4_mathcomma(<<<{\rm cd}(z) = {\rm sn}(z + K)>>>, <<<cd(z) = sn(z + K)>>>)
but computing it that way cancels near
m4_mathcomma(<<<u = 1>>>, <<<u = 1>>>)
where the result is near 0.  For
m4_math(<<<m > 1>>>, <<<m > 1>>>)
the two can differ by a period.  The form above is continuous at
m4_math(<<<u = 1>>>, <<<u = 1>>>)
and gives 0 there, while
m4_math(<<<K(m) - {\rm sn}^{-1}(u | m)>>>, <<<K(m) - inverse_jacobi_sn(u, m)>>>)
jumps there, because
m4_math(<<<u = 1>>>, <<<u = 1>>>)
is then on a branch cut of
m4_mathdot(<<<{\rm sn}^{-1}>>>, <<<sn^-1>>>)
Both give
m4_math(<<<K(m)>>>, <<<K(m)>>>)
at
m4_mathdot(<<<u = 0>>>, <<<u = 0>>>)

Examples of the old results:

@itemize @bullet
@item
@code{inverse_jacobi_cd(-0.5, 0.5)} returned
m4_mathcomma(<<<1.3184519444959686>>>, <<<1.3184519444959686>>>)
the value for 0.5, because
m4_math(<<<\sqrt{1-u^2}>>>, <<<sqrt(1-u^2)>>>)
does not depend on the sign of
m4_mathdot(<<<u>>>, <<<u>>>)
It is now
m4_mathdot(<<<2.389697410106775>>>, <<<2.389697410106775>>>)
@item
@code{inverse_jacobi_cd(1e-5, 0.5)} returned
m4_mathcomma(<<<1.8540646773009588>>>, <<<1.8540646773009588>>>)
about 1000 units of
m4_math(<<<2^{-53}>>>, <<<2^-53>>>)
off, because the argument of
m4_math(<<<{\rm sn}^{-1}>>>, <<<sn^-1>>>)
was near 1.  It is now
m4_mathdot(<<<1.8540646773013716>>>, <<<1.8540646773013716>>>)
@item
@code{inverse_jacobi_cd(1e200, 0.5)} overflowed, and
@code{inverse_jacobi_cd(0.5+0.5*%i, 3.0)} was not evaluated.
@end itemize

@node Real m Greater Than 1
@ifinfo
@section Real m > 1
@end ifinfo
@ifnotinfo
@section Real @math{m > 1}
@end ifnotinfo

For real
m4_mathcomma(<<<m > 1>>>, <<<m > 1>>>)
m4_math(<<<1-m>>>, <<<1-m>>>)
is a negative real, on the branch cut of the square root, and
m4_math(<<<m>>>, <<<m>>>)
is on a branch cut of the result.  @code{elliptic_kc} and
@code{inverse_jacobi_cn} return the limit from
m4_math(<<<{\rm Im}\, m < 0>>>, <<<imagpart(m) < 0>>>)
there.  For example,
m4_math(<<<{\rm cn}^{-1}(0.3 | 3)>>>, <<<inverse_jacobi_cn(0.3, 3)>>>)
is
m4_mathcomma(<<<1.0011 - 1.1714i>>>, <<<1.0011 - 1.1714*%i>>>)
and the limit from
m4_math(<<<{\rm Im}\, m > 0>>>, <<<imagpart(m) > 0>>>)
is its conjugate.

The general form for
m4_math(<<<{\rm cd}^{-1}>>>, <<<cd^-1>>>)
mixes the two sides: @code{sqrt} of
m4_math(<<<1-m>>>, <<<1-m>>>)
takes the upper side of its cut, which belongs to the limit from
m4_mathcomma(<<<{\rm Im}\, m < 0>>>, <<<imagpart(m) < 0>>>)
while
m4_math(<<<1/(1-m)>>>, <<<1/(1-m)>>>)
and
m4_mathcomma(<<<(1-mu^2)/(1-m)>>>, <<<(1-m*u^2)/(1-m)>>>)
when they are negative reals, are taken on the upper side of the cut of
m4_math(<<<R_F>>>, <<<R_F>>>)
(@pxref{Signed Zeros}), which belongs to the limit from
m4_mathdot(<<<{\rm Im}\, m > 0>>>, <<<imagpart(m) > 0>>>)
The result was then neither limit: @code{inverse_jacobi_cd(0.3, 3.0)}
gave
m4_mathcomma(<<<-0.680 - 1.171i>>>, <<<-0.680 - 1.171*%i>>>)
where the limits are
m4_mathdot(<<<0.680 \mp 1.171i>>>, <<<0.680 -/+ 1.171*%i>>>)

So @code{bf-inverse-jacobi-cd} computes the limit from
m4_math(<<<{\rm Im}\, m > 0>>>, <<<imagpart(m) > 0>>>)
at
m4_mathcomma(<<<\bar u>>>, <<<conjugate(u)>>>)
and conjugates it:

m4_displaymath(
<<<{\rm cd}^{-1}(u | m - i0) = \overline{{\rm cd}^{-1}(\bar u | m + i0)}.>>>
,
<<<@example
inverse_jacobi_cd(u, m - %i*0) = conjugate(inverse_jacobi_cd(conjugate(u), m + %i*0))
@end example>>>
)

@code{bf-inverse-jacobi-cd-aux} is told this with @code{upper-p}, and
takes
m4_mathcomma(<<<\sqrt{1-m} = -i\sqrt{m-1}>>>, <<<sqrt(1-m) = -%i*sqrt(m-1)>>>)
the value from that side.  When
m4_math(<<<1-u^2>>>, <<<1-u^2>>>)
is a negative real, that is, for real
m4_math(<<<u>>>, <<<u>>>)
with
m4_mathcomma(<<<|u| > 1>>>, <<<abs(u) > 1>>>)
it takes
m4_mathcomma(<<<\sqrt{1-u^2} = -i\sqrt{u^2-1}>>>, <<<sqrt(1-u^2) = -%i*sqrt(u^2-1)>>>)
so that after the conjugation
m4_math(<<<1-u^2>>>, <<<1-u^2>>>)
is on the side that @code{sqrt} gives it elsewhere.  With this, for
m4_math(<<<m>>>, <<<m>>>)
= 1.5, 3, 10,
m4_math(<<<10^{10}>>>, <<<1e10>>>)
and
m4_mathcomma(<<<1+2^{-40}>>>, <<<1+2^-40>>>)
the results agreed with those for
m4_math(<<<m - 10^{-13}(m-1)\, i>>>, <<<m - 1e-13*(m-1)*%i>>>)
at 180 random real and complex
m4_mathdot(<<<u>>>, <<<u>>>)
For real
m4_mathcomma(<<<m < 1>>>, <<<m < 1>>>)
they also agree with the limit from
m4_mathdot(<<<{\rm Im}\, m < 0>>>, <<<imagpart(m) < 0>>>)

@node inverse_jacobi_dc
@ifinfo
@section inverse_jacobi_dc
@end ifinfo
@ifnotinfo
@section @code{inverse_jacobi_dc}
@end ifnotinfo

This is
m4_mathcomma(<<<{\rm cd}^{-1}(1/u)>>>, <<<inverse_jacobi_cd(1/u)>>>)
computed as described in @ref{Reciprocal Functions}:
@code{bf-inverse-jacobi-dc-1} scales with
@code{bf-inverse-jacobi-scale-recip}, computes
m4_math(<<<t^2-v^2>>>, <<<t^2-v^2>>>)
with @code{bf-diff-of-recip-squares}, and calls
@code{bf-inverse-jacobi-cd-aux}.  The reflections are chosen as for
m4_math(<<<{\rm nc}^{-1}{:}>>>, <<<nc^-1:>>>)
by the sign of
m4_mathcomma(<<<{\rm Re}\, u>>>, <<<realpart(u)>>>)
and with the negative-imaginary-axis test applied to
m4_mathdot(<<<\bar u>>>, <<<conjugate(u)>>>)
@code{bf-inverse-jacobi-dc} handles real
m4_math(<<<m > 1>>>, <<<m > 1>>>)
as described in @ref{Real m Greater Than 1}.  It signals an error for
m4_mathcomma(<<<m = 1>>>, <<<m = 1>>>)
where
m4_mathcomma(<<<{\rm dc}(z | 1) = 1>>>, <<<dc(z|1) = 1>>>)
and for
m4_mathcomma(<<<u = 0>>>, <<<u = 0>>>)
which is on a branch cut: @code{inverse_jacobi_dc(1e-200, 0.5)} is
m4_math(<<<K + iK'>>>, <<<K + %i*K'>>>)
and @code{inverse_jacobi_dc(-1e-200, 0.5)} is
m4_mathdot(<<<K - iK'>>>, <<<K - %i*K'>>>)

Near
m4_mathcomma(<<<u = \pm 1>>>, <<<u = +/-1>>>)
computing it from the rounded
m4_math(<<<1/u>>>, <<<1/u>>>)
lost up to
m4_math(<<<4\times10^{5}>>>, <<<4e5>>>)
units of
m4_mathdot(<<<2^{-53}>>>, <<<2^-53>>>)
For example, @code{inverse_jacobi_dc(1 + 2.0^-30, 0.5)} returned
m4_mathpunc(<<<;>>>, <<<6.103515623578914\times10^{-5}>>>, <<<6.103515623578914e-5>>>)
it is now
m4_mathdot(<<<6.1035156207367436\times10^{-5}>>>, <<<6.1035156207367436e-5>>>)

@node Accuracy of the Other Six Functions
@section Accuracy of the Other Six Functions

On random points with
m4_math(<<<m>>>, <<<m>>>)
= 0.3, 0.9,
m4_mathcomma(<<<-2>>>, <<<-2>>>)
3 and
m4_math(<<<0.5+0.5i>>>, <<<0.5+0.5*%i>>>)
(and
m4_math(<<<1+2^{-30}>>>, <<<1+2^-30>>>)
for
m4_mathpunc(<<<),>>>, <<<{\rm cd}^{-1}>>>, <<<cd^-1>>>)
with real and complex
m4_math(<<<u>>>, <<<u>>>)
from
m4_math(<<<10^{-5}>>>, <<<1e-5>>>)
to
m4_math(<<<10^{5}>>>, <<<1e5>>>)
and near the poles and branch points, the worst relative errors, in
units of
m4_mathcomma(<<<2^{-53}>>>, <<<2^-53>>>)
were:

@multitable @columnfractions 0.2 0.25 0.55
@headitem Function @tab Worst error @tab Excluding
@item @code{sc} @tab 4.7 @tab
@item @code{sd} @tab 5.0 @tab @math{u} near @code{+/-1/sqrt(1-m)}, up to 280
@item @code{cs} @tab 5.7 @tab
@item @code{ds} @tab 6.3 @tab @math{u} near @code{+/-sqrt(1-m)}, up to 550
@item @code{cd} @tab 9.2 @tab
@item @code{dc} @tab 8.8 @tab
@end multitable

The excluded points are next to a branch point of the function, where
it is ill-conditioned, as described for
m4_math(<<<{\rm ds}^{-1}>>>, <<<ds^-1>>>)
in @ref{inverse_jacobi_cs and inverse_jacobi_ds}.  Every result was
checked to be an inverse, by applying the corresponding Jacobian
function.

@bye
