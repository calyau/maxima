@c -*- mode: texinfo -*-

@menu
* Introduction to Elliptic Functions and Integrals::
* Functions and Variables for Elliptic Functions::
* Functions and Variables for Elliptic Integrals::
@end menu



@node Introduction to Elliptic Functions and Integrals, Functions and Variables for Elliptic Functions, , Elliptic Functions
@comment  node-name,  next,  previous,  up
@section Introduction to Elliptic Functions and Integrals

Maxima includes support for Jacobian elliptic functions and for
complete and incomplete elliptic integrals.  This includes symbolic
manipulation of these functions and numerical evaluation as well.
Definitions of these functions and many of their properties can by
found in Abramowitz and Stegun, @urlaands{Chapter 16, 567} and
@urlaands{Chapter 17., 587}  See also @urldlmf{22.2}.  As much as possible,
we use the definitions and relationships given in Abramowitz and Stegun.

In particular, all elliptic functions and integrals use the parameter
@math{m} instead of the modulus @math{k} or the modular angle
@math{\alpha}.  The following relationships are true:

m4_displaymath(
<<<\eqalign{
m &= k^2 \cr
k &= \sin\alpha
}>>>
,
<<<@math{m = k^2}

@math{k = sin(alpha)}>>>
)

Note that Abramowitz and Stegun uses the notation 
m4_math(<<<{\rm
sn}(u|m)>>>, <<<sn(u|m)>>>) 
where we use 
m4_math(<<<{\rm
sn}(u,m)>>>, <<<sn(u,m)>>>) 
instead.  The DLMF uses modulus @math{k}
instead of the parameter @math{m}.

The elliptic functions and integrals are primarily intended to support
symbolic computation.  Therefore, most of derivatives of the functions
and integrals are known.  However, if floating-point values are given,
a floating-point result is returned.

Support for most of the other properties of elliptic functions and
integrals other than derivatives has not yet been written.

Some examples of elliptic functions:
@c ===beg===
@c jacobi_sn (u, m);
@c jacobi_sn (u, 1);
@c jacobi_sn (u, 0);
@c diff (jacobi_sn (u, m), u);
@c diff (jacobi_sn (u, m), m);
@c ===end===
@example
@group
(%i1) jacobi_sn (u, m);
(%o1)                    jacobi_sn(u, m)
@end group
@group
(%i2) jacobi_sn (u, 1);
(%o2)                        tanh(u)
@end group
@group
(%i3) jacobi_sn (u, 0);
(%o3)                        sin(u)
@end group
@group
(%i4) diff (jacobi_sn (u, m), u);
(%o4)            jacobi_cn(u, m) jacobi_dn(u, m)
@end group
@group
(%i5) diff (jacobi_sn (u, m), m);
(%o5) (jacobi_cn(u, m) jacobi_dn(u, m)
      elliptic_e(asin(jacobi_sn(u, m)), m)
 (u - ------------------------------------))/(2 m)
                     1 - m
            2
   jacobi_cn (u, m) jacobi_sn(u, m)
 + --------------------------------
              2 (1 - m)
@end group
@end example

Some examples of elliptic integrals:
@c ===beg===
@c elliptic_f (phi, m);
@c elliptic_f (phi, 0);
@c elliptic_f (phi, 1);
@c elliptic_e (phi, 1);
@c elliptic_e (phi, 0);
@c elliptic_kc (1/2);
@c makegamma (%);
@c diff (elliptic_f (phi, m), phi);
@c diff (elliptic_f (phi, m), m);
@c ===end===
@example
@group
(%i1) elliptic_f (phi, m);
(%o1)                  elliptic_f(phi, m)
@end group
@group
(%i2) elliptic_f (phi, 0);
(%o2)                          phi
@end group
@group
(%i3) elliptic_f (phi, 1);
                               phi   %pi
(%o3)                  log(tan(--- + ---))
                                2     4
@end group
@group
(%i4) elliptic_e (phi, 1);
                    phi                  phi
(%o4)       2 round(---) - sin(%pi round(---) - phi)
                    %pi                  %pi
@end group
@group
(%i5) elliptic_e (phi, 0);
(%o5)                          phi
@end group
@group
(%i6) elliptic_kc (1/2);
                                3/2
                             %pi
(%o6)                      -----------
                                  2 3
                           2 gamma (-)
                                    4
@end group
@group
(%i7) makegamma (%);
                                3/2
                             %pi
(%o7)                      -----------
                                  2 3
                           2 gamma (-)
                                    4
@end group
@group
(%i8) diff (elliptic_f (phi, m), phi);
                                1
(%o8)                 ---------------------
                                    2
                      sqrt(1 - m sin (phi))
@end group
@group
(%i9) diff (elliptic_f (phi, m), m);
       elliptic_e(phi, m) - (1 - m) elliptic_f(phi, m)
(%o9) (-----------------------------------------------
                              m
                                 cos(phi) sin(phi)
                             - ---------------------)/(2 (1 - m))
                                             2
                               sqrt(1 - m sin (phi))
@end group
@end example

Support for elliptic functions and integrals was written by Raymond
Toy.  It is placed under the terms of the General Public License (GPL)
that governs the distribution of Maxima.

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox

@node Functions and Variables for Elliptic Functions, Functions and Variables for Elliptic Integrals, Introduction to Elliptic Functions and Integrals, Elliptic Functions
@comment  node-name,  next,  previous,  up
@section Functions and Variables for Elliptic Functions
See @urlaands{Section 6.12, 569} and @urldlmf{22.2} for more
information.

@anchor{jacobi_sn}
@deffn {Function} jacobi_sn (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm sn}(u,m)>>>, <<<sn(u,m)>>>)


@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_cn}
@deffn {Function} jacobi_cn (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm cn}(u,m)>>>, <<<cn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_dn}
@deffn {Function} jacobi_dn (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm dn}(u,m)>>>, <<<dn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_ns}
@deffn {Function} jacobi_ns (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm ns}(u,m) = 1/{\rm
sn}(u,m)>>>, <<<ns(u,m) = 1/sn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_sc}
@deffn {Function} jacobi_sc (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm sc}(u,m) = {\rm
sn}(u,m)/{\rm cn}(u,m)>>>, <<<sc(u,m) = sn(u,m)/cn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_sd}
@deffn {Function} jacobi_sd (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot({\rm sd}(u,m) = {\rm
sn}(u,m)/{\rm dn}(u,m), <<<sd(u,m) = sn(u,m)/dn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_nc}
@deffn {Function} jacobi_nc (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm nc}(u,m) = 1/{\rm cn}(u,m)>>>, <<<nc(u,m) = 1/cn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_cs}
@deffn {Function} jacobi_cs (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm cs}(u,m) = {\rm
cn}(u,m)/{\rm sn}(u,m)>>>, <<<cs(u,m) = cn(u,m)/sn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_cd}
@deffn {Function} jacobi_cd (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm cd}(u,m) = {\rm cn}(u,m)/{\rm dn}(u,m)>>>, <<<cd(u,m) = cn(u,m)/dn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_nd}
@deffn {Function} jacobi_nd (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm nd}(u,m) = 1/{\rm dn}(u,m)>>>,
<<<nd(u,m) = 1/dn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_ds}
@deffn {Function} jacobi_ds (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm ds}(u,m) =
{\rm dn}(u,m)/{\rm sn}(u,m)>>>, <<<ds(u,m) = dn(u,m)/sn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_dc}
@deffn {Function} jacobi_dc (@var{u}, @var{m})
The Jacobian elliptic function 
m4_mathdot(<<<{\rm dc}(u,m) =
{\rm dn}(u,m)/{\rm cn}(u,m)>>>, <<<dc(u,m) = dn(u,m)/cn(u,m)>>>)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{jacobi_am}
@deffn {Function} jacobi_am (@var{u}, @var{m})
The Jacobi amplitude function, @code{jacobi_am}, is defined implicitly by (see
@url{http://functions.wolfram.com/09.24.02.0001.01})
m4_math(<<<z = {\rm am}(w, m)>>>,<<<z = am(w, m)>>>)
where @math{w = F(z,m)} where @math{F(z,m)} is the incomplete elliptic
integral of the first kind (@pxref{elliptic_f}).  It is defined for
all real and complex values of @math{z} and @math{m}.  In particular
for real @math{z} and @math{m} with @math{|m|<1},
m4_math(<<<{\rm am}(z,m)>>>,<<<am(z,m)>>>)
maps the entire real line to the entire real line.  For other values
of @math{z} and @math{m}, the following relationship is used:
m4_mathdot(<<<{\rm am}(z,m) = \sin^{-1}({\rm jacobi\_sn}(z, m))>>>,
<<<am(z,m) = asin(jacobi_sn(z,m))>>>)

Some examples:
@c ===beg===
@c jacobi_am(z,0);
@c jacobi_am(z,1);
@c jacobi_am(0,m);
@c jacobi_am(100, .5);
@c jacobi_am(0.5, 1.5);
@c jacobi_am(1.5b0, 1.5b0+%i);
@c ===end===
@example
@group
(%i1) jacobi_am(z,0);
(%o1)                           z
@end group
@group
(%i2) jacobi_am(z,1);
                                 z    %pi
(%o2)                   2 atan(%e ) - ---
                                       2
@end group
@group
(%i3) jacobi_am(0,m);
(%o3)                           0
@end group
@group
(%i4) jacobi_am(100, .5);
(%o4)                   84.70311272411382
@end group
@group
(%i5) jacobi_am(0.5, 1.5);
(%o5)                  0.4707197897046991
@end group
@group
(%i6) jacobi_am(1.5b0, 1.5b0+%i);
(%o6)    9.340542168700782b-1 - 3.723960452146071b-1 %i
@end group
@end example

@c ===beg===
@c plot2d([jacobi_am(x,.4),jacobi_am(x,.7),jacobi_am(x,.99),jacobi_am(x,.999999)],[x,0,10*%pi]);
@c ===end===
@example
@group
(%i1) plot2d([jacobi_am(x,.4),jacobi_am(x,.7),jacobi_am(x,.99),jacobi_am(x,.999999)],[x,0,10*%pi]);
(%o1)                         false
@end group
@end example

@ifnotinfo
Compare this plot with the plot from @urldlmf{22.16.iv}:

@image{figures/jacobi_am}
@end ifnotinfo

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_dn}
@deffn {Function} inverse_jacobi_dn (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
dn}(u,m)>>>, <<<dn(u,m)>>>) 
For 
m4_mathcomma(\sqrt{1-m}\le u \le 1,
sqrt(1-m) <= u <= 1) 
it can also be written (@urldlmf{22.15.E14}):
m4_displaymath(
{\rm inverse\_jacobi\_dn}(u, m) = \int_u^1 {dt\over \sqrt{(1-t^2)(t^2-(1-m))}},
@example
                                1
                               /
                               [               1
     inverse_jacobi_dn(u, m) = I  --------------------------- dt
                               ]             2    2
                               /  sqrt((1 - t ) (t  + m - 1))
                                u
@end example
)


@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_ns}
@deffn {Function} inverse_jacobi_ns (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
ns}(u,m)>>>, <<<ns(u,m)>>>) 
For 
m4_mathcomma(1 \le u, 1 <= u) 
it can also be written (@urldlmf{22.15.E121}):
m4_displaymath(
{\rm inverse\_jacobi\_ns}(u, m) = \int_u^{\infty} {dt\over \sqrt{(1-t^2)(t^2-m)}},
@example
                                 inf
                                /
                                [               1
      inverse_jacobi_ns(u, m) = I    ----------------------- dt
                                ]               2    2
                                /    sqrt((1 - t ) (t  - m))
                                 u
@end example
)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_sc}
@deffn {Function} inverse_jacobi_sc (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
sc}(u,m)>>>, <<<sc(u,m)>>>) 
For all @math{u} it can also be written (@urldlmf{22.15.E20}):
m4_displaymath(
{\rm inverse\_jacobi\_sc}(u, m) = \int_0^u {dt\over \sqrt{(1+t^2)(1+(1-m)t^2)}},
@example
                              u
                             /
                             [                 1
   inverse_jacobi_sc(u, m) = I  ------------------------------- dt
                             ]             2                2
                             /  sqrt((1 - t ) (1 - (1 - m) t ))
                              0
@end example
)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_sd}
@deffn {Function} inverse_jacobi_sd (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
sd}(u,m)>>>, <<<sd(u,m)>>>) 
For 
m4_mathcomma(-1/\sqrt{1-m}\le u \le
1/\sqrt{1-m}, -1/sqrt(1-m) <= 1/sqrt(1-m)) 
it can also be written (@urldlmf{22.15.E16}):
m4_displaymath(
{\rm inverse\_jacobi\_sd}(u, m) = \int_0^u {dt\over \sqrt{(1-(1-m)t^2)(1+mt^2)}},
@example
                            u
                           /
                           [                  1
 inverse_jacobi_sd(u, m) = I  --------------------------------- dt
                           ]                     2      2
                           /  sqrt((1 - (1 - m) t ) (m t  + 1))
                            0
@end example
)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_nc}
@deffn {Function} inverse_jacobi_nc (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
nc}(u,m)>>>, <<<nc(u,m)>>>) 
For 
m4_mathcomma(1\le u, 1 <= u) 
it can also be written (@urldlmf{22.15.E19}):
m4_displaymath(
{\rm inverse\_jacobi\_nc}(u, m) = \int_1^u {dt\over \sqrt{(t^2-1)(m+(1-m)t^2)}},
@example
                              1
                             /
                             [                 1
 inverse_jacobi_nc(u, m) = - I  ------------------------------- dt
                             ]         2                2
                             /  sqrt((t  - 1) ((1 - m) t  + m))
                              u
@end example
)

@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_cs}
@deffn {Function} inverse_jacobi_cs (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
cs}(u,m)>>>, <<<cs(u,m)>>>) 
For all @math{u} it can also be written (@urldlmf{22.15.E23}):
m4_displaymath(
{\rm inverse\_jacobi\_cs}(u, m) = \int_u^{\infty} {dt\over \sqrt{(1+t^2)(t^2+(1-m))}},
@example
                             inf
                            /
                            [                  1
  inverse_jacobi_cs(u, m) = I    ----------------------------- dt
                            ]          2            2
                            /    sqrt(t  + 1) sqrt(t  - m + 1)
                             u
@end example
)


@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_cd}
@deffn {Function} inverse_jacobi_cd (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
cd}(u,m)>>>, <<<cd(u,m)>>>) 
For 
m4_mathcomma(-1\le u \le 1, -1 <= u <=
1) 
it can also be written (@urldlmf{22.15.E15}):
m4_displaymath(
{\rm inverse\_jacobi\_cd}(u, m) = \int_u^1 {dt\over \sqrt{(1-t^2)(1-mt^2)}},
@example
                                1
                               /
                               [              1
     inverse_jacobi_cd(u, m) = I  ------------------------- dt
                               ]             2          2
                               /  sqrt((1 - t ) (1 - m t ))
                                u
@end example
)


@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_nd}
@deffn {Function} inverse_jacobi_nd (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
nd}(u,m)>>>, <<<nd(u,m)>>>) 
For 
m4_mathcomma(1\le u \le 1/\sqrt{1-m},
1 <= u <= 1/sqrt(1-m)) 
it can also be written (@urldlmf{22.15.E17}):
m4_displaymath(
{\rm inverse\_jacobi\_nd}(u, m) = \int_1^u {dt\over \sqrt{(t^2-1)(1-(1-m)t^2)}},
@example
                              1
                             /
                             [                 1
 inverse_jacobi_nd(u, m) = - I  ------------------------------- dt
                             ]         2                    2
                             /  sqrt((t  - 1) (1 - (1 - m) t ))
                              u
@end example
)


@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_ds}
@deffn {Function} inverse_jacobi_ds (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
ds}(u,m)>>>, <<<ds(u,m)>>>) 
For 
m4_mathcomma(\sqrt{1-m}\le u,
sqrt(1-m) <= u) 
it can also be written (@urldlmf{22.15.E22}):
m4_displaymath(
{\rm inverse\_jacobi\_ds}(u, m) = \int_u^{\infty} {dt\over \sqrt{(t^2+m)(t^2-(1-m))}},
@example
                              inf
                             /
                             [                 1
   inverse_jacobi_ds(u, m) = I    --------------------------- dt
                             ]           2            2
                             /    sqrt((t  + m - 1) (t  + m))
                              u
@end example
)


@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn

@anchor{inverse_jacobi_dc}
@deffn {Function} inverse_jacobi_dc (@var{u}, @var{m})
The inverse of the Jacobian elliptic function 
m4_mathdot(<<<{\rm
dc}(u,m)>>>, <<<dc(u,m)>>>) 
For 
m4_mathcomma(1 \le u, 1 <= u) 
it can also be written (@urldlmf{22.15.E18}):
m4_displaymath(
{\rm inverse\_jacobi\_dc}(u, m) = \int_1^u {dt\over \sqrt{(t^2-1)(t^2-m)}},
@example
                                  u
                                 /
                                 [             1
     inverse_jacobi_dc(u, m) =   I  ----------------------- dt
                                 ]         2        2
                                 /  sqrt((t  - 1) (t  - m))
                                  1
@end example
)


@opencatbox{Categories:}
@category{Elliptic functions}
@closecatbox
@end deffn


@node Functions and Variables for Elliptic Integrals,  , Functions and Variables for Elliptic Functions, Elliptic Functions
@comment  node-name,  next,  previous,  up
@section Functions and Variables for Elliptic Integrals

@anchor{elliptic_f}
@deffn {Function} elliptic_f (@var{phi}, @var{m})
The incomplete elliptic integral of the first kind, defined as

m4_displaymath(
<<<\int_0^{\phi} {\frac{d\theta}{\sqrt{1-m\sin^2\theta}}}>>>
,
<<<@math{integrate(1/sqrt(1 - m*sin(x)^2), x, 0, phi)}>>>
)

See also @ref{elliptic_e} and @ref{elliptic_kc}.

@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

@anchor{elliptic_e}
@deffn {Function} elliptic_e (@var{phi}, @var{m})
The incomplete elliptic integral of the second kind, defined as

m4_displaymath(
<<<\int_0^\phi {\sqrt{1 - m\sin^2\theta}}\, d\theta>>>
,
<<<@math{elliptic_e(phi, m) = integrate(sqrt(1 - m*sin(x)^2), x, 0, phi)}>>>
)

See also @ref{elliptic_f} and @ref{elliptic_ec}.

@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

@anchor{elliptic_eu}
@deffn {Function} elliptic_eu (@var{u}, @var{m})
The incomplete elliptic integral of the second kind, defined as

m4_displaymath(
<<<E(u, m) = \int_0^u {\rm dn}(v, m)\, dv  = \int_0^\tau \sqrt{\frac{1-m t^2}{1-t^2}}\, dt>>>
,
<<<@math{elliptic_eu(u, m) = integrate(dn(v,m)^2,v,0,u) = integrate(sqrt(1-m*t^2)/sqrt(1-t^2), t, 0, tau)}>>>
)

where 
m4_mathdot(
<<<\tau = {\rm sn}(u,m)>>>
,
<<<tau = sn(u,m)>>>
)


This is related to @code{elliptic_e} by

m4_displaymath(
<<<E(u,m) = E(\sin^{-1} {\rm sn}(u, m), m)>>>
,
<<<@math{elliptic_eu(u, m) = elliptic_e(asin(sn(u,m)),m)}>>>
)

See also @ref{elliptic_e}.
@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

@anchor{elliptic_pi}
@deffn {Function} elliptic_pi (@var{n}, @var{phi}, @var{m})
The incomplete elliptic integral of the third kind, defined as

m4_displaymath(
<<<\int_0^\phi {{d\theta}\over{(1-n\sin^2 \theta)\sqrt{1 - m\sin^2\theta}}}>>>
,
<<<@math{integrate(1/(1-n*sin(x)^2)/sqrt(1 - m*sin(x)^2), x, 0, phi)}>>>
)

@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

@anchor{elliptic_kc}
@deffn {Function} elliptic_kc (@var{m})
The complete elliptic integral of the first kind, defined as

m4_displaymath(
<<<\int_0^{\frac{\pi}{2}} {{d\theta}\over{\sqrt{1 - m\sin^2\theta}}}>>>
,
<<<@math{integrate(1/sqrt(1 - m*sin(x)^2), x, 0, pi/2)}>>>
)

For certain values of @math{m}, the value of the integral is known in
terms of @math{Gamma} functions.  Use @mref{makegamma} to evaluate them.

@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

@anchor{elliptic_ec}
@deffn {Function} elliptic_ec (@var{m})
The complete elliptic integral of the second kind, defined as

m4_displaymath(
<<<\int_0^{\frac{\pi}{2}} \sqrt{1 - m\sin^2\theta}\, d\theta>>>
,
<<<@math{integrate(sqrt(1 - m*sin(x)^2), x, 0, pi/2)}>>>
)

For certain values of @math{m}, the value of the integral is known in
terms of @math{Gamma} functions.  Use @mref{makegamma} to evaluate them.

@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

@anchor{carlson_rc}
@deffn {Function} carlson_rc (@var{x}, @var{y})
Carlson's RC integral is defined by

m4_displaymath(
<<<R_C(x, y) = \frac{1}{2} \int_0^{\infty} \frac{1}{\sqrt{t+x}(t+y)}\, dt>>>
,
<<<@math{integrate(1/2*(t+x)^(-1/2)/(t+y), t, 0, inf)}>>>
)

This integral is related to many elementary functions in the following
way:

m4_displaymath(
<<<\eqalign{
\log x &= (x-1) R_C\left(\left({\frac{1+x}{2}}\right)^2, x\right), \quad x > 0 \cr
\sin^{-1} x &= x R_C(1-x^2, 1), \quad |x| \le 1 \cr
\cos^{-1} x &= \sqrt{1-x^2} R_C(x^2,1), \quad 0 \le x \le 1  \cr
\tan^{-1} x &= x  R_C(1,1+x^2)  \cr
\sinh^{-1} x &= x  R_C(1+x^2,1)  \cr
\cosh^{-1} x &= \sqrt{x^2-1}  R_C(x^2,1), \quad x \ge 1  \cr
\tanh^{-1}(x) &= x  R_C(1,1-x^2), \quad |x| \le 1
}>>>
,
<<<@math{log(x)  = (x-1)*rc(((1+x)/2)^2, x), x > 0}

@math{asin(x) = x * rc(1-x^2, 1), |x|<= 1}

@math{acos(x) = sqrt(1-x^2)*rc(x^2,1), 0 <= x <=1}

@math{atan(x) = x * rc(1,1+x^2)}

@math{asinh(x) = x * rc(1+x^2,1)}

@math{acosh(x) = sqrt(x^2-1) * rc(x^2,1), x >= 1}

@math{atanh(x) = x * rc(1,1-x^2), |x|<=1}>>>
)

Also, we have the relationship

m4_displaymath(
<<<R_C(x,y) = R_F(x,y,y)>>>
,
@math{R_C(x,y) = R_F(x,y,y)}
)

Some special values:
m4_displaymath(
<<<\eqalign{R_C(0, 1) &= \frac{\pi}{2} \cr
R_C(0, 1/4) &= \pi \cr
R_C(2,1) &= \log(\sqrt{2} + 1) \cr
R_C(i,i+1) &= \frac{\pi}{4} + \frac{i}{2} \log(\sqrt{2}-1) \cr
R_C(0,i) &= (1-i)\frac{\pi}{2\sqrt{2}} \cr
}>>>
,
@math{R_C(0,1) = pi/2}

@math{R_C(0,1/4) = pi}

@math{R_C(2,1) = log(sqrt(2)+1)}

@math{R_C(i, i+1) = pi/4 + i/2*log(sqrt(2)+1)}

@math{R_C(0, i) = (1-i)*pi/(2*sqrt(2))}
)


@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

@anchor{carlson_rd}
@deffn {Function} carlson_rd (@var{x}, @var{y}, @var{z})
Carlson's RD integral is defined by

m4_displaymath(
<<<R_D(x,y,z) = \frac{3}{2} \int_0^{\infty} \frac{1}{\sqrt{t+x}\sqrt{t+y}\sqrt{t+z}\,(t+z)}\, dt>>>
,
<<<@math{R_D(x,y,z) = 3/2*integrate(1/(sqrt(t+x)*sqrt(t+y)*sqrt(t+z)*(t+z)), t, 0, inf)}>>>
)

We also have the special values

m4_displaymath(
<<<\eqalign{
R_D(x,x,x) &= x^{-\frac{3}{2}} \cr
R_D(0,y,y) &= \frac{3}{4} \pi y^{-\frac{3}{2}} \cr
R_D(0,2,1) &= 3 \sqrt{\pi} \frac{\Gamma(\frac{3}{4})}{\Gamma(\frac{1}{4})}
}>>>
,
<<<@math{R_D(x,x,x) = x^(-3/2)}

@math{R_D(0,y,y) = 3/4*pi*y^(-3/2)}

@math{R_D(0,2,1) = 3 sqrt(pi) gamma(3/4)/gamma(1/4)}>>>
)


It is also related to the complete elliptic E function as follows

m4_displaymath(
<<<E(m) = R_F(0, 1 - m, 1) - \frac{m}{3} R_D(0, 1 - m, 1)>>>
,
<<<@math{E(m) = R_F(0, 1 - m, 1) - (m/3)* R_D(0, 1 - m, 1)}>>>
)

@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

@anchor{carlson_rf}
@deffn {Function} carlson_rf (@var{x}, @var{y}, @var{z})
Carlson's RF integral is defined by

m4_displaymath(
<<<R_F(x,y,z) = \frac{1}{2} \int_0^{\infty} \frac{1}{\sqrt{t+x}\sqrt{t+y}\sqrt{t+z}}\, dt>>>
,
<<<@math{R_F(x,y,z) = 1/2*integrate(1/(sqrt(t+x)*sqrt(t+y)*sqrt(t+z)), t, 0, inf)}>>>
)

We also have the special values

m4_displaymath(
<<<\eqalign{
R_F(0,1,2)  &= \frac{\Gamma({\frac{1}{4}})^2}{4\sqrt{2\pi}} \cr
R_F(i,-i,0) &= \frac{\Gamma({\frac{1}{4}})^2}{4\sqrt{\pi}}
}>>>
,
<<<
@math{R_F(0,1,2) = gamma(1/4)^2/(4*sqrt(2*pi))}

@math{R_F(i,-i,0) = gamma(1/4)^2/(4*sqrt(pi))}
>>>
)

It is also related to the complete elliptic E function as follows

m4_displaymath(
<<<E(m) = R_F(0, 1 - m, 1) - \frac{m}{3} R_D(0, 1 - m, 1)>>>
,
<<<@math{E(m) = R_F(0, 1 - m, 1) - (m/3)* R_D(0, 1 - m, 1)}>>>
)

@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

@anchor{carlson_rj}
@deffn {Function} carlson_rj (@var{x}, @var{y}, @var{z}, @var{p})
Carlson's RJ integral is defined by

m4_displaymath(
<<<R_J(x,y,z) = \frac{1}{2} \int_0^{\infty} \frac{1}{\sqrt{t+x}\sqrt{t+y}\sqrt{t+z}\,(t+p)}\, dt>>>
,
<<<
@math{R_J(x,y,z) = 1/2*integrate(1/(sqrt(t+x)*sqrt(t+y)*sqrt(t+z)*(t+p)), t, 0, inf)}
>>>)

@opencatbox{Categories:}
@category{Elliptic integrals}
@closecatbox
@end deffn

