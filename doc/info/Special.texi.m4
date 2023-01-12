@c -*- mode: texinfo -*-
@menu
* Introduction to Special Functions::
* Bessel Functions::
* Airy Functions::
* Gamma and factorial Functions::
* Exponential Integrals::
* Error Function::
* Struve Functions::
* Hypergeometric Functions::
* Parabolic Cylinder Functions::
* Functions and Variables for Special Functions::  
@end menu

@c -----------------------------------------------------------------------------
@node Introduction to Special Functions, Bessel Functions, Special Functions, Special Functions
@section Introduction to Special Functions
@c -----------------------------------------------------------------------------

Special function notation follows:

@example
bessel_j (index, expr)         Bessel function, 1st kind
bessel_y (index, expr)         Bessel function, 2nd kind
bessel_i (index, expr)         Modified Bessel function, 1st kind
bessel_k (index, expr)         Modified Bessel function, 2nd kind

hankel_1 (v,z)                 Hankel function of the 1st kind
hankel_2 (v,z)                 Hankel function of the 2nd kind
struve_h (v,z)                 Struve H function
struve_l (v,z)                 Struve L function

assoc_legendre_p[v,u] (z)      Legendre function of degree v and order u 
assoc_legendre_q[v,u] (z)      Legendre function, 2nd kind

%f[p,q] ([], [], expr)         Generalized Hypergeometric function
gamma (z)                      Gamma function
gamma_incomplete_lower (a,z)   Lower incomplete gamma function
gamma_incomplete (a,z)         Tail of incomplete gamma function
hypergeometric (l1, l2, z)     Hypergeometric function
%s[u,v] (z)                    Lommel "small" s function
slommel[u,v] (z)               Lommel "big" S function
%m[u,k] (z)                    Whittaker function, 1st kind
%w[u,k] (z)                    Whittaker function, 2nd kind
erfc (z)                       Complement of the erf function

expintegral_e (v,z)            Exponential integral E
expintegral_e1 (z)             Exponential integral E1
expintegral_ei (z)             Exponential integral Ei
expintegral_li (z)             Logarithmic integral Li
expintegral_si (z)             Exponential integral Si
expintegral_ci (z)             Exponential integral Ci
expintegral_shi (z)            Exponential integral Shi
expintegral_chi (z)            Exponential integral Chi

kelliptic (z)                  Complete elliptic integral of the first 
                               kind (K)
parabolic_cylinder_d (v,z)     Parabolic cylinder D function
@end example

@opencatbox{Categories:}
@category{Bessel functions}
@category{Airy functions}
@category{Special functions}
@closecatbox

@c -----------------------------------------------------------------------------
@node Bessel Functions, Airy Functions, Introduction to Special Functions, Special Functions
@section Bessel Functions
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{bessel_j}
@deffn {Function} bessel_j (@var{v}, @var{z})

The Bessel function of the first kind of order @math{v} and argument @math{z}.
See @urlaands{eqn 9.1.10, 360} and @urldlmf{10.2.E2}.

@code{bessel_j} is defined as

m4_displaymath(
<<<J_v(z) = \sum_{k=0}^{\infty }{{{\left(-1\right)^{k}\,\left(z\over 2\right)^{v+2\,k}
 }\over{k!\,\Gamma\left(v+k+1\right)}}}>>>,
<<<@example
                inf
                ====       k  - v - 2 k  v + 2 k
                \     (- 1)  2          z
                 >    --------------------------
                /        k! gamma(v + k + 1)
                ====
                k = 0
@end example>>>
)

although the infinite series is not used for computations.

When @code{besselexpand} is @code{true}, @code{bessel_j} is expanded in terms
of elementary functions when the order @math{v} is half of an odd integer. 
See @mrefdot{besselexpand}

@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{bessel_y}
@deffn {Function} bessel_y (@var{v}, @var{z})

The Bessel function of the second kind of order @math{v} and argument @math{z}.
See @urlaands{eqn 9.1.2, 358} and @urldlmf{10.2.E3}.

@code{bessel_y} is defined as
m4_displaymath(
<<<Y_v(z) = {{\cos(\pi v)\, J_v(z) - J_{-v}(z)}\over{\sin{\pi v}}}>>>,
<<<@example
              cos(%pi v) bessel_j(v, z) - bessel_j(-v, z)
              -------------------------------------------
                             sin(%pi v)
@end example>>>
)

when @math{v} is not an integer.  When @math{v} is an integer @math{n},
the limit as @math{v} approaches @math{n} is taken.

When @code{besselexpand} is @code{true}, @code{bessel_y} is expanded in terms
of elementary functions when the order @math{v} is half of an odd integer. 
See @mrefdot{besselexpand}

@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{bessel_i}
@deffn {Function} bessel_i (@var{v}, @var{z})

The modified Bessel function of the first kind of order @math{v} and argument
@math{z}. See @urlaands{eqn 9.6.10, 375} and @urldlmf{10.25.E2}.

@code{bessel_i} is defined as
m4_displaymath(
<<<I_v(z) = \sum_{k=0}^{\infty } {{1\over{k!\,\Gamma
 \left(v+k+1\right)}} {\left(z\over 2\right)^{v+2\,k}}}>>>,
<<<@example
                    inf
                    ====   - v - 2 k  v + 2 k
                    \     2          z
                     >    -------------------
                    /     k! gamma(v + k + 1)
                    ====
                    k = 0
@end example>>>
)

although the infinite series is not used for computations.

When @code{besselexpand} is @code{true}, @code{bessel_i} is expanded in terms
of elementary functions when the order @math{v} is half of an odd integer. 
See @mrefdot{besselexpand}


@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{bessel_k}
@deffn {Function} bessel_k (@var{v}, @var{z})

The modified Bessel function of the second kind of order @math{v} and argument
@math{z}. See @urlaands{eqn 9.6.2, 375} and @urldlmf{10.27.E4}.

@code{bessel_k} is defined as
m4_displaymath(
<<<K_v(z) = {{\pi\,\csc \left(\pi\,v\right)\,\left(I_{-v}(z)-I_{v}(z)\right)}\over{2}}>>>,
<<<@example
           %pi csc(%pi v) (bessel_i(-v, z) - bessel_i(v, z))
           -------------------------------------------------
                                  2
@end example>>>
)

when @math{v} is not an integer.  If @math{v} is an integer @math{n},
then the limit as @math{v} approaches @math{n} is taken.

When @code{besselexpand} is @code{true}, @code{bessel_k} is expanded in terms
of elementary functions when the order @math{v} is half of an odd integer. 
See @mrefdot{besselexpand}


@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{hankel_1}
@deffn {Function} hankel_1 (@var{v}, @var{z})

The Hankel function of the first kind of order @math{v} and argument @math{z}.
See @urlaands{eqn 9.1.3, 358} and @urldlmf{10.4.E3}.

@code{hankel_1} is defined as

m4_displaymath(
<<<H^{(1)}_v(z) = J_v(z) + i Y_v(z)>>>,
<<<@example
   bessel_j(v,z) + %i * bessel_y(v,z)
@end example>>>
)

Maxima evaluates @code{hankel_1} numerically for a complex order @math{v} and 
complex argument @math{z} in float precision. The numerical evaluation in 
bigfloat precision is not supported.

When @code{besselexpand} is @code{true}, @code{hankel_1} is expanded in terms
of elementary functions when the order @math{v} is half of an odd integer. 
See @mrefdot{besselexpand}

Maxima knows the derivative of @code{hankel_1} wrt the argument @math{z}.

Examples:

Numerical evaluation:

@c ===beg===
@c hankel_1(1,0.5);
@c hankel_1(1,0.5+%i);
@c ===end===
@example
@group
(%i1) hankel_1(1,0.5);
(%o1)        0.24226845767487 - 1.471472392670243 %i
@end group
@group
(%i2) hankel_1(1,0.5+%i);
(%o2)       - 0.25582879948621 %i - 0.23957560188301
@end group
@end example

Expansion of @code{hankel_1} when @code{besselexpand} is @code{true}:

@c ===beg===
@c hankel_1(1/2,z),besselexpand:true;
@c ===end===
@example
@group
(%i1) hankel_1(1/2,z),besselexpand:true;
               sqrt(2) sin(z) - sqrt(2) %i cos(z)
(%o1)          ----------------------------------
                       sqrt(%pi) sqrt(z)
@end group
@end example

Derivative of @code{hankel_1} wrt the argument @math{z}. The derivative wrt the 
order @math{v} is not supported. Maxima returns a noun form:

@c ===beg===
@c diff(hankel_1(v,z),z);
@c diff(hankel_1(v,z),v);
@c ===end===
@example
@group
(%i1) diff(hankel_1(v,z),z);
             hankel_1(v - 1, z) - hankel_1(v + 1, z)
(%o1)        ---------------------------------------
                                2
@end group
@group
(%i2) diff(hankel_1(v,z),v);
                       d
(%o2)                  -- (hankel_1(v, z))
                       dv
@end group
@end example

@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{hankel_2}
@deffn {Function} hankel_2 (@var{v}, @var{z})

The Hankel function of the second kind of order @math{v} and argument @math{z}.
See @urlaands{eqn 9.1.4, 358} and @urldlmf{10.4.E3}.

@code{hankel_2} is defined as

m4_displaymath(
<<<H^{(2)}_v(z) = J_v(z) - i Y_v(z)>>>,
<<<
@example
   bessel_j(v,z) - %i * bessel_y(v,z)
@end example
>>>)

Maxima evaluates @code{hankel_2} numerically for a complex order @math{v} and 
complex argument @math{z} in float precision. The numerical evaluation in 
bigfloat precision is not supported.

When @code{besselexpand} is @code{true}, @code{hankel_2} is expanded in terms
of elementary functions when the order @math{v} is half of an odd integer. 
See @mrefdot{besselexpand}

Maxima knows the derivative of @code{hankel_2} wrt the argument @math{z}.

For examples see @code{hankel_1}.

@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{besselexpand}
@defvr {Option variable} besselexpand
Default value: @code{false}

@c REPHRASE
Controls expansion of the Bessel, Hankel and Struve functions
when the order is half of
an odd integer.  In this case, the functions can be expanded
in terms of other elementary functions.  When @code{besselexpand} is @code{true},
the Bessel function is expanded.

@example
(%i1) besselexpand: false$
(%i2) bessel_j (3/2, z);
                                    3
(%o2)                      bessel_j(-, z)
                                    2
(%i3) besselexpand: true$
(%i4) bessel_j (3/2, z);
                                        sin(z)   cos(z)
                       sqrt(2) sqrt(z) (------ - ------)
                                           2       z
                                          z
(%o4)                  ---------------------------------
                                   sqrt(%pi)
(%i5) bessel_y(3/2,z);
                                        sin(z)    cos(z)
                    sqrt(2) sqrt(z) ((- ------) - ------)
                                          z          2
                                                    z
(%o5)               -------------------------------------
                                  sqrt(%pi)
(%i6) bessel_i(3/2,z);
                                      cosh(z)   sinh(z)
                     sqrt(2) sqrt(z) (------- - -------)
                                         z         2
                                                  z
(%o6)                -----------------------------------
                                  sqrt(%pi)
(%i7) bessel_k(3/2,z);
                                      1        - z
                           sqrt(%pi) (- + 1) %e
                                      z
(%o7)                      -----------------------
                               sqrt(2) sqrt(z)

@end example

@opencatbox{Categories:}
@category{Bessel functions}
@category{Simplification flags and variables} 
@category{Special functions}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{scaled_bessel_i}
@deffn {Function} scaled_bessel_i (@var{v}, @var{z}) 

The scaled modified Bessel function of the first kind of order
@math{v} and argument @math{z}.  That is,

m4_displaymath(
<<<{\rm scaled\_bessel\_i}(v,z) = e^{-|z|} I_v(z).>>>,
<<<scaled_bessel_i(v,z) = exp(-abs(z)) * bessel_i(v,z).>>>)

This function is particularly useful
for calculating m4_math(<<<I_v(z)>>>,<<<bessel_i>>>) for large @math{z}, which is large.
However, maxima does not otherwise know much about this function.  For
symbolic work, it is probably preferable to work with the expression
@code{exp(-abs(z))*bessel_i(v, z)}.

@opencatbox{Categories:}
@category{Bessel functions}
@closecatbox
@end deffn

@anchor{scaled_bessel_i0}
@deffn {Function} scaled_bessel_i0 (@var{z}) 

Identical to @code{scaled_bessel_i(0,z)}.

@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{scaled_bessel_i1}
@deffn {Function} scaled_bessel_i1 (@var{z})

Identical to @code{scaled_bessel_i(1,z)}.
@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{%s}
@deffn {Function} %s [@var{u},@var{v}] (@var{z}) 
Lommel's little m4_math(<<<s_{\mu,\nu}(z)>>>, <<<s[u,v](z)>>>) function.  
(@urldlmf{11.9.E3})(G&R 8.570.1).

This Lommel function is the particular solution of the inhomogeneous
Bessel differential equation:

m4_displaymath(
<<<{d^2\over dz^2} + {1\over z}{dw\over dz} + \left(1-{\nu^2\over z^2}\right) w = z^{\mu-1}>>>,
<<<diff(w,z,2)+1/z*diff(w,z)+(1-v^2/z^2)*w = z^(u-1)>>>)

This can be defined by the series

m4_displaymath(
<<<s_{\mu,\nu}(z) = z^{\mu+1}\sum_{k=0}^{\infty} (-1)^k {z^{2k}\over a_{k+1}(\mu, \nu)}>>>,
<<<%s[u,v](z) = z^(u+1)*sum((-1)^k*z^(2*k)/a[k+1](u,v)>>>)

where

m4_displaymath(
<<<a_k(\mu,\nu) = \prod_{m=1}^k \left(\left(\mu + 2m-1\right)^2-\nu^2\right) = 4^k\left(\mu-\nu+1\over 2\right)_k \left(\mu+\nu+1\over 2\right)_k>>>,
<<<a[k](u,v) = product(((u+2*m-1)^2-v^2),m,1,k) = 4^k*pochhammer((u-v+1)/2,k)*pochhammer((u+v+1)/2,k)>>>)

@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@deffn {Function} slommel [@var{u},@var{v}] (@var{z}) 
Lommel's big m4_math(<<<S_{\mu,\nu}(z)>>>, <<<S[u,v](z)>>>) function.  
(@urldlmf{11.9.E5})(G&R 8.570.2).

Lommels big S function is another particular solution of the
inhomogeneous Bessel differential equation
(@pxref{%s}) defined for all values
of m4_math(\mu, u) and m4_math(\nu, v), where

m4_displaymath(
<<<\eqalign{
S_{\mu,\nu}(z) = s_{\mu,\nu}(z) + 2^{\mu-1} & \Gamma\left({\mu\over 2} + {\nu\over 2} + {1\over 2}\right) \Gamma\left({\mu\over 2} - {\nu\over 2} + {1\over 2}\right) \cr
& \times \left(\sin\left({(\mu-\nu)\pi\over 2}\right) J_{\nu}(z) - \cos\left({(\mu-\nu)\pi\over 2}\right) Y_{\nu}(z)\right)
}>>>,
<<<@math{slommel[u,v](z) = %s[u,v](z) + 2^(u-1)*gamma(u/2+v/2+1/2)*gamma(u/2-v/2+1/2)
* (sin(1/2*(u-v)*%pi)*bessel_j(v,z) - cos(1/2*(u-v)*%pi)*bessel_y(v,z))}>>>)

When m4_math(<<<\mu\pm \nu>>>, u+/-v)) is an odd
negative integer, the limit must be used.

@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn


@c -----------------------------------------------------------------------------
@node Airy Functions, Gamma and factorial Functions, Bessel Functions, Special Functions
@section Airy Functions
@c -----------------------------------------------------------------------------

The Airy functions m4_math(<<<{\rm Ai}(x)>>>,<<<Ai(x)>>>) and m4_math(<<<{\rm Bi}(x)>>>,<<<Bi(x)>>>) are defined in Abramowitz and Stegun,
@i{Handbook of Mathematical Functions}, @urlaands{Section 10.4, 446} and @urldlmf{9}.

The Airy differential equation is:

m4_displaymath(
<<<{d^2 y\over dx^2} - xy = 0>>>,
<<<@math{diff (y(x), x, 2) - x y(x) = 0}>>>)

The numerically satisfactory pair of solutions (@urldlmf{9.2#T1}) on the real line are m4_math(<<<y = {\rm Ai}(x)>>>,<<<y = Ai(x)>>>) and m4_mathdot(<<<y = {\rm Bi}(x)>>>,<<<y = Bi(x)>>>)
These two solutions are oscillatory for @math{x < 0}.  m4_math({\rm Ai}(x), Ai(x)) is
the solution subject to the condition that m4_math(y\rightarrow 0,
y->0) as m4_math(<<<x\rightarrow +\infty,>>>, <<<x ->
+inf,>>>) and m4_math({\rm Bi}(x), Bi(x)) is
the second solution with the
same amplitude as m4_math({\rm Ai}(x),Ai(x)) as m4_math(x\rightarrow-\infty,x->minf) which differs in phase
by m4_mathdot(\pi/2,%pi/2)  Also, m4_math({\rm Bi}(x),Bi(x)) is unbounded
as m4_mathdot(x\rightarrow +\infty, x->+inf)


If the argument @math{x} is a real or complex floating point 
number, the numerical value of the function is returned.

@anchor{airy_ai}
@deffn {Function} airy_ai (@var{x})
The Airy function m4_mathdot(<<<{\rm Ai}(x)>>>, <<<Ai(x)>>>)  See @urlaands{eqn 10.4.2, 446} and @urldlmf{9}.

See also @mrefcomma{airy_bi} @mrefcomma{airy_dai} and @mrefdot{airy_dbi}

@opencatbox{Categories:}
@category{Airy functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{airy_dai}
@deffn {Function} airy_dai (@var{x})
The derivative of the Airy function m4_math(<<<{\rm Ai}(x)>>>,Ai(x)):

m4_displaymath(
<<<{\rm airy\_dai}(x) = {d\over dx}{\rm Ai}(x)>>>,
<<<@math{airy_dai(x) = diff(airy_ai(x),x)>>>}) 

See @mrefdot{airy_ai}

@opencatbox{Categories:}
@category{Airy functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{airy_bi}
@deffn {Function} airy_bi (@var{x})
The Airy function m4_math(<<<{\rm Bi}(x)>>>, Bi(x)).  See @urlaands{eqn 10.4.3, 446} and  @urldlmf{9}.

See @mrefcomma{airy_ai} and @mrefdot{airy_dbi}

@opencatbox{Categories:}
@category{Airy functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{airy_dbi}
@deffn {Function} airy_dbi (@var{x})
The derivative of the Airy function m4_math(<<<{\rm Bi}(x)>>>, Bi(x)>>>):

m4_displaymath(
<<<{\rm airy\_dbi}(x) = {d\over dx}{\rm Bi}(x)>>>,
<<<@math{airy_dbi(x) = diff(airy_bi(x),x)}>>>)

See @mrefcomma{airy_ai} and @mrefdot{airy_bi}

@opencatbox{Categories:}
@category{Airy functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@node Gamma and factorial Functions, Exponential Integrals, Airy Functions, Special Functions
@section Gamma and factorial Functions
@c -----------------------------------------------------------------------------

The gamma function and the related beta, psi and incomplete gamma 
functions are defined in Abramowitz and Stegun,
@i{Handbook of Mathematical Functions}, Chapter 6.

@c FOLLOWING FUNCTIONS IN bffac.mac ARE NOT DESCRIBED IN .texi FILES: !!!
@c obfac, azetb, vonschtoonk, divrlst, obzeta, bfhzeta, bfpsi0 !!!
@c DON'T KNOW WHICH ONES ARE INTENDED FOR GENERAL USE !!!

@c FOLLOWING FUNCTIONS IN bffac.mac ARE DESCRIBED IN Number.texi: !!!
@c burn, bzeta, bfzeta !!!

@c FOLLOWING FUNCTIONS IN bffac.mac ARE DESCRIBED HERE: !!!
@c bfpsi, bffac, cbffac !!!

@c -----------------------------------------------------------------------------
@anchor{bffac}
@deffn {Function} bffac (@var{expr}, @var{n})

Bigfloat version of the factorial (shifted gamma)
function.  The second argument is how many digits to retain and return,
it's a good idea to request a couple of extra.

@example
(%i1) bffac(1/2,16);
(%o1)                        8.862269254527584b-1
(%i2) (1/2)!,numer;
(%o2)                          0.886226925452758
(%i3) bffac(1/2,32);
(%o3)                 8.862269254527580136490837416707b-1
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Numerical evaluation}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{bfpsi}
@deffn  {Function} bfpsi (@var{n}, @var{z}, @var{fpprec})
@deffnx {Function} bfpsi0 (@var{z}, @var{fpprec})

@code{bfpsi} is the polygamma function of real argument @var{z} and
integer order @var{n}.  See @ref{polygamma, psi} for further
information.  @code{bfpsi0} is the digamma function.
@code{bfpsi0(@var{z}, @var{fpprec})} is equivalent to @code{bfpsi(0,
@var{z}, @var{fpprec})}.

These functions return bigfloat values.
@var{fpprec} is the bigfloat precision of the return value.

@c psi0(1) = -%gamma IS AN INTERESTING PROPERTY BUT IN THE ABSENCE OF ANY OTHER
@c DISCUSSION OF THE PROPERTIES OF THIS FUNCTION, THIS STATEMENT SEEMS OUT OF PLACE.
@c Note @code{-bfpsi0 (1, fpprec)} provides @code{%gamma} (Euler's constant) as a bigfloat.

@example
(%i1) bfpsi0(1/3, 15);
(%o1)                        - 3.13203378002081b0
(%i2) bfpsi0(1/3, 32);
(%o2)                - 3.1320337800208063229964190742873b0
(%i3) bfpsi(0,1/3,32);
(%o3)                - 3.1320337800208063229964190742873b0
(%i4) psi[0](1/3);
                          3 log(3)       %pi
(%o4)                  (- --------) - --------- - %gamma
                             2        2 sqrt(3)
(%i5) float(%);
(%o5)                         - 3.132033780020806
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Numerical evaluation}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{cbffac}
@deffn {Function} cbffac (@var{z}, @var{fpprec})
Complex bigfloat factorial.

@code{load ("bffac")} loads this function.

@example
(%i1) cbffac(1+%i,16);
(%o1)           3.430658398165453b-1 %i + 6.529654964201666b-1
(%i2) (1+%i)!,numer;
(%o2)             0.3430658398165453 %i + 0.6529654964201667
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Complex variables}
@category{Numerical evaluation}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{gamma}
@deffn {Function} gamma (@var{z})

The basic definition of the gamma function (@urldlmf{5.2.E1} and @urlaands{eqn 6.1.1, 255}) is

m4_displaymath(
<<<\Gamma\left(z\right)=\int_{0}^{\infty }{t^{z-1}\,e^ {- t }\;dt}>>>,
<<<@example
                         inf
                        /
                        [     z - 1   - t
             gamma(z) = I    t      %e    dt
                        ]
                        /
                         0
@end example>>>)

Maxima simplifies @code{gamma} for positive integer and positive and negative 
rational numbers. For half integral values the result is a rational number
times m4_math(<<<\sqrt{\pi}>>>,<<<sqrt(%pi)>>>). The simplification for integer values is controlled by 
@code{factlim}. For integers greater than @code{factlim} the numerical result of 
the factorial function, which is used to calculate @code{gamma}, will overflow. 
The simplification for rational numbers is controlled by @code{gammalim} to 
avoid internal overflow. See @code{factlim} and @code{gammalim}.

For negative integers @code{gamma} is not defined.

Maxima can evaluate @code{gamma} numerically for real and complex values in float
and bigfloat precision.

@code{gamma} has mirror symmetry.

When @mref{gamma_expand} is @code{true}, Maxima expands @code{gamma} for 
arguments @code{z+n} and @code{z-n} where @code{n} is an integer.

Maxima knows the derivative of @code{gamma}.

Examples:

Simplification for integer, half integral, and rational numbers:

@example
(%i1) map('gamma,[1,2,3,4,5,6,7,8,9]);
(%o1)        [1, 1, 2, 6, 24, 120, 720, 5040, 40320]
(%i2) map('gamma,[1/2,3/2,5/2,7/2]);
                    sqrt(%pi)  3 sqrt(%pi)  15 sqrt(%pi)
(%o2)   [sqrt(%pi), ---------, -----------, ------------]
                        2           4            8
(%i3) map('gamma,[2/3,5/3,7/3]);
                                  2           1
                          2 gamma(-)  4 gamma(-)
                      2           3           3
(%o3)          [gamma(-), ----------, ----------]
                      3       3           9
@end example

Numerical evaluation for real and complex values:

@example
(%i4) map('gamma,[2.5,2.5b0]);
(%o4)     [1.329340388179137, 1.3293403881791370205b0]
(%i5) map('gamma,[1.0+%i,1.0b0+%i]);
(%o5) [0.498015668118356 - .1549498283018107 %i, 
          4.9801566811835604272b-1 - 1.5494982830181068513b-1 %i]
@end example

@code{gamma} has mirror symmetry:

@example
(%i6) declare(z,complex)$
(%i7) conjugate(gamma(z));
(%o7)                  gamma(conjugate(z))
@end example

Maxima expands @code{gamma(z+n)} and @code{gamma(z-n)}, when @mref{gamma_expand} 
is @code{true}:

@example
(%i8) gamma_expand:true$

(%i9) [gamma(z+1),gamma(z-1),gamma(z+2)/gamma(z+1)];
                               gamma(z)
(%o9)             [z gamma(z), --------, z + 1]
                                z - 1
@end example

The derivative of @code{gamma}:

@example
(%i10) diff(gamma(z),z);
(%o10)                  psi (z) gamma(z)
                           0
@end example

See also @mrefdot{makegamma}

The Euler-Mascheroni constant is @code{%gamma}.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@deffn {Function} log_gamma (@var{z})

The natural logarithm of the gamma function.

@example
(%i1) gamma(6);
(%o1)                                 120
(%i2) log_gamma(6);
(%o2)                              log(120)
(%i3) log_gamma(0.5);
(%o3)                         0.5723649429247004
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@deffn {Function} gamma_incomplete_lower (@var{a}, @var{z})

The lower incomplete gamma function (@urldlmf{8.2.E1} and @urlaands{eqn 6.5.2, 260}):

m4_displaymath(
<<<\gamma\left(a , z\right)=\int_{0}^{z}{t^{a-1}\,e^ {- t }\;dt}>>>,
<<<@example
                                    z
                                   /
                                   [  a - 1   - t
    gamma_incomplete_lower(a, z) = I t      %e    dt
                                   ]
                                   /
                                    0
@end example>>>
)

See also @mref{gamma_incomplete} (upper incomplete gamma function).

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{gamma_incomplete}
@deffn {Function} gamma_incomplete (@var{a}, @var{z})

The incomplete upper gamma function (@urldlmf{8.2.E2} and @urlaands{eqn 6.5.3, 260}):

m4_displaymath(
<<<\Gamma\left(a , z\right)=\int_{z}^{\infty }{t^{a-1}\,e^ {- t }\;dt}>>>,
<<<@example
                              inf
                             /
                             [     a - 1   - t
    gamma_incomplete(a, z) = I    t      %e    dt
                             ]
                             /
                              z
@end example
>>>
)

See also @mref{gamma_expand} for controlling how
@code{gamma_incomplete} is expressed in terms of elementary functions
and @code{erfc}.

Also see the related functions @mref{gamma_incomplete_regularized} and
@mref{gamma_incomplete_generalized}.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{gamma_incomplete_regularized}
@deffn {Function} gamma_incomplete_regularized (@var{a}, @var{z})

The regularized incomplete upper gamma function (@urldlmf{8.2.E4}):

m4_displaymath(
<<<Q\left(a , z\right)={{\Gamma\left(a , z\right)}\over{\Gamma\left(a\right)}}>>>,
<<<@example
gamma_incomplete_regularized(a, z) = 
                                        gamma_incomplete(a, z)
                                        ----------------------
                                               gamma(a)
@end example
>>>)

See also @mref{gamma_expand} for controlling how
@mref{gamma_incomplete} is expressed in terms of elementary functions
and @mrefdot{erfc}

Also see @mref{gamma_incomplete}.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{gamma_incomplete_generalized}
@deffn {Function} gamma_incomplete_generalized (@var{a}, @var{z1}, @var{z1})

The generalized incomplete gamma function.

m4_displaymath(
<<<\Gamma\left(a , z_{1}, z_{2}\right)=\int_{z_{1}}^{z_{2}}{t^{a-1}\,e^ {- t }\;dt}>>>,
<<<@example
gamma_incomplete_generalized(a, z1, z2) = 
                                               z2
                                              /
                                              [    a - 1   - t
                                              I   t      %e    dt
                                              ]
                                              /
                                               z1
@end example
>>>)

Also see @mref{gamma_incomplete} and @mref{gamma_incomplete_regularized}.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Special functions}
@closecatbox
@end deffn


@c -----------------------------------------------------------------------------
@anchor{gamma_expand}
@defvr {Option variable} gamma_expand
Default value: @code{false}

@code{gamma_expand} controls expansion of @mref{gamma_incomplete}.
When @code{gamma_expand} is @code{true}, @code{gamma_incomplete(v,z)}
is expanded in terms of
@code{z}, @code{exp(z)}, and @mref{gamma_incomplete} or @mref{erfc} when possible.

@example
(%i1) gamma_incomplete(2,z);
(%o1)                       gamma_incomplete(2, z)
(%i2) gamma_expand:true;
(%o2)                                true
(%i3) gamma_incomplete(2,z);
                                           - z
(%o3)                            (z + 1) %e
@group
(%i4) gamma_incomplete(3/2,z);
                              - z   sqrt(%pi) erfc(sqrt(z))
(%o4)               sqrt(z) %e    + -----------------------
                                               2
@end group
@group
(%i5) gamma_incomplete(4/3,z);
                                                    1
                                   gamma_incomplete(-, z)
                       1/3   - z                    3
(%o5)                 z    %e    + ----------------------
                                             3
@end group
@group
(%i6) gamma_incomplete(a+2,z);
             a               - z
(%o6)       z  (z + a + 1) %e    + a (a + 1) gamma_incomplete(a, z)
(%i7) gamma_incomplete(a-2, z);
        gamma_incomplete(a, z)    a - 2         z            1      - z
(%o7)   ---------------------- - z      (--------------- + -----) %e
           (1 - a) (2 - a)               (a - 2) (a - 1)   a - 2

@end group
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Simplification flags and variables}
@closecatbox

@end defvr
@c -----------------------------------------------------------------------------
@anchor{gammalim}
@defvr {Option variable} gammalim
Default value: 10000

@c REPHRASE
@code{gammalim} controls simplification of the gamma
function for integral and rational number arguments.  If the absolute
value of the argument is not greater than @code{gammalim}, then
simplification will occur.  Note that the @code{factlim} switch controls
simplification of the result of @code{gamma} of an integer argument as well.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Simplification flags and variables}
@closecatbox

@end defvr

@c NEED CROSS REFS HERE
@c NEED EXAMPLES HERE

@c -----------------------------------------------------------------------------
@anchor{makegamma}
@deffn {Function} makegamma (@var{expr})
Transforms instances of binomial, factorial, and beta
functions in @var{expr} into gamma functions.

See also @mrefdot{makefact}

@example
(%i1) makegamma(binomial(n,k));
                                 gamma(n + 1)
(%o1)                    -----------------------------
                         gamma(k + 1) gamma(n - k + 1)
(%i2) makegamma(x!);
(%o2)                            gamma(x + 1)
(%i3) makegamma(beta(a,b));
                               gamma(a) gamma(b)
(%o3)                          -----------------
                                 gamma(b + a)
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end deffn

@anchor{beta}
@c -----------------------------------------------------------------------------
@deffn {Function} beta (@var{a}, @var{b})
The beta function is defined as
m4_displaymath(
<<<{\rm B}(a, b) = {{\Gamma(a) \Gamma(b)}\over{\Gamma(a+b)}}>>>,
<<<@math{gamma(a) gamma(b)/gamma(a+b)}>>>)
(@urldlmf{5.12.E1} and @urlaands{eqn 6.2.1, 258}).

Maxima simplifies the beta function for positive integers and rational 
numbers, which sum to an integer. When @code{beta_args_sum_to_integer} is 
@code{true}, Maxima simplifies also general expressions which sum to an integer. 

For @var{a} or @var{b} equal to zero the beta function is not defined.

In general the beta function is not defined for negative integers as an 
argument. The exception is for @var{a=-n}, @var{n} a positive integer 
and @var{b} a positive integer with @code{b<=n}, it is possible to define an
analytic continuation. Maxima gives for this case a result.

When @mref{beta_expand} is @code{true}, expressions like @code{beta(a+n,b)} and 
@code{beta(a-n,b)} or @code{beta(a,b+n)} and @code{beta(a,b-n)} with @code{n} 
an integer are simplified.

Maxima can evaluate the beta function for real and complex values in float and 
bigfloat precision. For numerical evaluation Maxima uses @code{log_gamma}:

@example
           - log_gamma(b + a) + log_gamma(b) + log_gamma(a)
         %e
@end example

Maxima knows that the beta function is symmetric and has mirror symmetry.

Maxima knows the derivatives of the beta function with respect to @var{a} or 
@var{b}.

To express the beta function as a ratio of gamma functions see @code{makegamma}. 

Examples:

Simplification, when one of the arguments is an integer:

@example
(%i1) [beta(2,3),beta(2,1/3),beta(2,a)];
                               1   9      1
(%o1)                         [--, -, ---------]
                               12  4  a (a + 1)
@end example

Simplification for two rational numbers as arguments which sum to an integer:

@example
(%i2) [beta(1/2,5/2),beta(1/3,2/3),beta(1/4,3/4)];
                          3 %pi   2 %pi
(%o2)                    [-----, -------, sqrt(2) %pi]
                            8    sqrt(3)
@end example

When setting @code{beta_args_sum_to_integer} to @code{true} more general 
expression are simplified, when the sum of the arguments is an integer:

@example
(%i3) beta_args_sum_to_integer:true$
(%i4) beta(a+1,-a+2);
                                %pi (a - 1) a
(%o4)                         ------------------
                              2 sin(%pi (2 - a))
@end example

The possible results, when one of the arguments is a negative integer: 

@example
(%i5) [beta(-3,1),beta(-3,2),beta(-3,3)];
                                    1  1    1
(%o5)                            [- -, -, - -]
                                    3  6    3
@end example

@code{beta(a+n,b)} or @code{beta(a-n,b)} with @code{n} an integer simplifies when 
@mref{beta_expand} is @code{true}:

@example
(%i6) beta_expand:true$
(%i7) [beta(a+1,b),beta(a-1,b),beta(a+1,b)/beta(a,b+1)];
                    a beta(a, b)  beta(a, b) (b + a - 1)  a
(%o7)              [------------, ----------------------, -]
                       b + a              a - 1           b

@end example

Beta is not defined, when one of the arguments is zero:

@example
(%i7) beta(0,b);
beta: expected nonzero arguments; found 0, b
 -- an error.  To debug this try debugmode(true);
@end example

Numerical evaluation for real and complex arguments in float or bigfloat
precision:

@example
(%i8) beta(2.5,2.3);
(%o8) .08694748611299981

(%i9) beta(2.5,1.4+%i);
(%o9) 0.0640144950796695 - .1502078053286415 %i

(%i10) beta(2.5b0,2.3b0);
(%o10) 8.694748611299969b-2

(%i11) beta(2.5b0,1.4b0+%i);
(%o11) 6.401449507966944b-2 - 1.502078053286415b-1 %i
@end example

Beta is symmetric and has mirror symmetry:

@example
(%i14) beta(a,b)-beta(b,a);
(%o14)                                 0
(%i15) declare(a,complex,b,complex)$
(%i16) conjugate(beta(a,b));
(%o16)                 beta(conjugate(a), conjugate(b))
@end example

The derivative of the beta function wrt @code{a}:

@example
(%i17) diff(beta(a,b),a);
(%o17)               - beta(a, b) (psi (b + a) - psi (a))
                                      0             0
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@deffn {Function} beta_incomplete (@var{a}, @var{b}, @var{z})

The basic definition of the incomplete beta function
(@urldlmf{8.17.E1} and @urlaands{eqn 6.6.1, 263}) is

m4_displaymath(
<<<{\rm B}_z(a,b) = \int_0^z t^{a-1}(1-t)^{b-1}\; dt>>>,
<<<@example
@group
        z
       /
       [         b - 1  a - 1
       I  (1 - t)      t      dt
       ]
       /
        0
@end group
@end example
>>>)
@c @example
@c @group
@c         z
@c        /
@c        [         b - 1  a - 1
@c        I  (1 - t)      t      dt
@c        ]
@c        /
@c         0
@c @end group
@c @end example

This definition is possible for m4_math(<<<{\rm Re}(a) > 0>>>, <<<realpart(a)>0>>>) and m4_math(<<<{\rm Re}(b) > 0>>>, <<<realpart(b)>0>>>) and m4_math(<<<|z| < 1>>>, <<<abs(z)<1>>>).
For other values the incomplete beta function can be 
defined through a generalized hypergeometric function:

@example
   gamma(a) hypergeometric_generalized([a, 1 - b], [a + 1], z) z
@end example

(See @url{https://functions.wolfram.com/GammaBetaErf/Beta3/} for a complete definition of the incomplete beta
function.)

For negative integers @math{a = -n} and positive integers @math{b=m} with m4_math(<<<m \le n>>>, <<<m<=n>>>) the incomplete beta function is defined through

m4_displaymath(
<<<z^{n-1}\sum_{k=0}^{m-1} {{(1-m)_k z^k} \over {k! (n-k)}}>>>,
<<<@example
                            m - 1           k
                            ====  (1 - m)  z
                      n - 1 \            k
                     z       >    -----------
                            /     k! (n - k)
                            ====
                            k = 0
@end example
>>>)
@c @example
@c                             m - 1           k
@c                             ====  (1 - m)  z
@c                       n - 1 \            k
@c                      z       >    -----------
@c                             /     k! (n - k)
@c                             ====
@c                             k = 0
@c @end example

Maxima uses this definition to simplify @code{beta_incomplete} for @var{a} a 
negative integer.

For @var{a} a positive integer, @code{beta_incomplete} simplifies for any 
argument @var{b} and @var{z} and for @var{b} a positive integer for any 
argument @var{a} and @var{z}, with the exception of @var{a} a negative integer.

For @math{z=0} and m4_math(<<<{\rm Re}(a) > 0>>>, <<<realpart(a)>0>>>), @code{beta_incomplete} has the 
specific value zero. For @math{z=1} and m4_math(<<<{\rm Re}(b) > 0>>>, <<<realpart(b)>0>>>), 
@code{beta_incomplete} simplifies to the beta function @code{beta(a,b)}.

Maxima evaluates @code{beta_incomplete} numerically for real and complex values 
in float or bigfloat precision. For the numerical evaluation an expansion of the 
incomplete beta function in continued fractions is used.

When the option variable @mref{beta_expand} is @code{true}, Maxima expands
expressions like @code{beta_incomplete(a+n,b,z)} and
@code{beta_incomplete(a-n,b,z)} where n is a positive integer.

Maxima knows the derivatives of @code{beta_incomplete} with respect to the 
variables @var{a}, @var{b} and @var{z} and the integral with respect to the 
variable @var{z}.

Examples:

Simplification for @var{a} a positive integer:

@example
(%i1) beta_incomplete(2,b,z);
                                       b
                            1 - (1 - z)  (b z + 1)
(%o1)                       ----------------------
                                  b (b + 1)
@end example

Simplification for @var{b} a positive integer:

@example
(%i2) beta_incomplete(a,2,z);
                                               a
                              (a (1 - z) + 1) z
(%o2)                         ------------------
                                  a (a + 1)
@end example

Simplification for @var{a} and @var{b} a positive integer:

@example
(%i3) beta_incomplete(3,2,z);
@group
                                               3
                              (3 (1 - z) + 1) z
(%o3)                         ------------------
                                      12
@end group
@end example

@var{a} is a negative integer and @math{b<=(-a)}, Maxima simplifies:

@example
(%i4) beta_incomplete(-3,1,z);
                                       1
(%o4)                              - ----
                                        3
                                     3 z
@end example

For the specific values @math{z=0} and @math{z=1}, Maxima simplifies:

@example
(%i5) assume(a>0,b>0)$
(%i6) beta_incomplete(a,b,0);
(%o6)                                 0
(%i7) beta_incomplete(a,b,1);
(%o7)                            beta(a, b)
@end example

Numerical evaluation in float or bigfloat precision:

@example
(%i8) beta_incomplete(0.25,0.50,0.9);
(%o8)                          4.594959440269333
(%i9)  fpprec:25$
(%i10) beta_incomplete(0.25,0.50,0.9b0);
(%o10)                    4.594959440269324086971203b0
@end example

For @math{abs(z)>1} @code{beta_incomplete} returns a complex result:

@example
(%i11) beta_incomplete(0.25,0.50,1.7);
(%o11)              5.244115108584249 - 1.45518047787844 %i
@end example

Results for more general complex arguments:

@example
(%i14) beta_incomplete(0.25+%i,1.0+%i,1.7+%i);
(%o14)             2.726960675662536 - .3831175704269199 %i
(%i15) beta_incomplete(1/2,5/4*%i,2.8+%i);
(%o15)             13.04649635168716 %i - 5.802067956270001
(%i16) 
@end example

Expansion, when @mref{beta_expand} is @code{true}:

@example
(%i23) beta_incomplete(a+1,b,z),beta_expand:true;
                                                       b  a
                   a beta_incomplete(a, b, z)   (1 - z)  z
(%o23)             -------------------------- - -----------
                             b + a                 b + a

(%i24) beta_incomplete(a-1,b,z),beta_expand:true;
                                                       b  a - 1
       beta_incomplete(a, b, z) (- b - a + 1)   (1 - z)  z
(%o24) -------------------------------------- - ---------------
                       1 - a                         1 - a
@end example
 
Derivative and integral for @code{beta_incomplete}:

@example
(%i34) diff(beta_incomplete(a, b, z), z);
@group
                              b - 1  a - 1
(%o34)                 (1 - z)      z
@end group
(%i35) integrate(beta_incomplete(a, b, z), z);
              b  a
       (1 - z)  z
(%o35) ----------- + beta_incomplete(a, b, z) z
          b + a
                                       a beta_incomplete(a, b, z)
                                     - --------------------------
                                                 b + a
(%i36) factor(diff(%, z));
(%o36)              beta_incomplete(a, b, z)
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{beta_incomplete_regularized}
@deffn {Function} beta_incomplete_regularized (@var{a}, @var{b}, @var{z})

The regularized incomplete beta function (@urldlmf{8.17.E2} and
@urlaands{eqn 6.6.2, 263}), defined as 

m4_displaymath(
<<<I_z(a,b) = {{\rm B}_z(a,b)\over {\rm B}(a,b)}>>>,
<<<@example
beta_incomplete_regularized(a, b, z) = 
                                      beta_incomplete(a, b, z)
                                      ------------------------
                                             beta(a, b)
@end example
>>>)
@c @example
@c beta_incomplete_regularized(a, b, z) = 
@c                                       beta_incomplete(a, b, z)
@c                                       ------------------------
@c                                              beta(a, b)
@c @end example

As for @code{beta_incomplete} this definition is not complete. See 
@url{https://functions.wolfram.com/GammaBetaErf/BetaRegularized/} for a complete definition of
@code{beta_incomplete_regularized}.

@code{beta_incomplete_regularized} simplifies @var{a} or @var{b} a positive 
integer.

For @math{z=0} and m4_math(<<<{\rm Re}(a)>0>>>, <<<realpart(a) > 0>>>),
@code{beta_incomplete_regularized} has 
the specific value 0. For @math{z=1} and m4_math(<<<{\rm Re}(b) > 0>>>, <<<realpart(b)>0>>>), 
@code{beta_incomplete_regularized} simplifies to 1.

Maxima can evaluate @code{beta_incomplete_regularized} for real and complex 
arguments in float and bigfloat precision.

When @mref{beta_expand} is @code{true}, Maxima expands 
@code{beta_incomplete_regularized} for arguments @math{a+n} or @math{a-n}, 
where n is an integer.

Maxima knows the derivatives of @code{beta_incomplete_regularized} with respect 
to the variables @var{a}, @var{b}, and @var{z} and the integral with respect to 
the variable @var{z}.

Examples:

Simplification for @var{a} or @var{b} a positive integer:

@example
(%i1) beta_incomplete_regularized(2,b,z);
                                       b
(%o1)                       1 - (1 - z)  (b z + 1)

(%i2) beta_incomplete_regularized(a,2,z);
                                               a
(%o2)                         (a (1 - z) + 1) z

(%i3) beta_incomplete_regularized(3,2,z);
                                               3
(%o3)                         (3 (1 - z) + 1) z
@end example

For the specific values @math{z=0} and @math{z=1}, Maxima simplifies:

@example
(%i4) assume(a>0,b>0)$
(%i5) beta_incomplete_regularized(a,b,0);
(%o5)                                 0
(%i6) beta_incomplete_regularized(a,b,1);
(%o6)                                 1
@end example

Numerical evaluation for real and complex arguments in float and bigfloat 
precision:

@example
(%i7) beta_incomplete_regularized(0.12,0.43,0.9);
(%o7)                         .9114011367359802
(%i8) fpprec:32$
(%i9) beta_incomplete_regularized(0.12,0.43,0.9b0);
(%o9)               9.1140113673598075519946998779975b-1
(%i10) beta_incomplete_regularized(1+%i,3/3,1.5*%i);
(%o10)             .2865367499935403 %i - 0.122995963334684
(%i11) fpprec:20$
(%i12) beta_incomplete_regularized(1+%i,3/3,1.5b0*%i);
(%o12)      2.8653674999354036142b-1 %i - 1.2299596333468400163b-1
@end example

Expansion, when @mref{beta_expand} is @code{true}:

@example
(%i13) beta_incomplete_regularized(a+1,b,z);
                                                     b  a
                                              (1 - z)  z
(%o13) beta_incomplete_regularized(a, b, z) - ------------
                                              a beta(a, b)
(%i14) beta_incomplete_regularized(a-1,b,z);
(%o14) beta_incomplete_regularized(a, b, z)
                                                     b  a - 1
                                              (1 - z)  z
                                         - ----------------------
                                           beta(a, b) (b + a - 1)
@end example

The derivative and the integral wrt @var{z}:

@example
(%i15) diff(beta_incomplete_regularized(a,b,z),z);
                              b - 1  a - 1
                       (1 - z)      z
(%o15)                 -------------------
                           beta(a, b)
(%i16) integrate(beta_incomplete_regularized(a,b,z),z);
(%o16) beta_incomplete_regularized(a, b, z) z
                                                           b  a
                                                    (1 - z)  z
          a (beta_incomplete_regularized(a, b, z) - ------------)
                                                    a beta(a, b)
        - -------------------------------------------------------
                                   b + a
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@deffn {Function} beta_incomplete_generalized (@var{a}, @var{b}, @var{z1}, @var{z2})

The basic definition of the generalized incomplete beta function is

m4_displaymath(
<<<\int_{z_1}^{z_2} t^{a-1}(1-t)^{b-1}\; dt>>>,
<<<@example
@group
             z2
           /
           [          b - 1  a - 1
           I   (1 - t)      t      dt
           ]
           /
            z1
@end group
@end example
>>>)
@c @example
@c @group
@c              z2
@c            /
@c            [          b - 1  a - 1
@c            I   (1 - t)      t      dt
@c            ]
@c            /
@c             z1
@c @end group
@c @end example

Maxima simplifies @code{beta_incomplete_regularized} for @var{a} and @var{b} 
a positive integer.

For m4_math(<<<{\rm Re}(a) > 0>>>, <<<realpart(a)>0>>>) and m4_math(z_1 = 0, z1=0) or m4_math(z_2 = 0, z2=0), Maxima simplifies
@code{beta_incomplete_generalized} to @code{beta_incomplete}.
For m4_math({\rm Re}(b) > 0, realpart(b)>0) and m4_math(z_1 = 1, z1=1) or m4_math(z_2 = 1, z2=1), Maxima simplifies to an 
expression with @code{beta} and @code{beta_incomplete}.

Maxima evaluates @code{beta_incomplete_regularized} for real and complex values 
in float and bigfloat precision.

When @mref{beta_expand} is @code{true}, Maxima expands 
@code{beta_incomplete_generalized} for @math{a+n} and @math{a-n}, @var{n} a 
positive integer.

Maxima knows the derivative of @code{beta_incomplete_generalized} with respect 
to the variables @var{a}, @var{b}, @var{z1}, and @var{z2} and the integrals with
respect to the variables @var{z1} and @var{z2}.

Examples:

Maxima simplifies @code{beta_incomplete_generalized} for @var{a} and @var{b} a 
positive integer:

@example
(%i1) beta_incomplete_generalized(2,b,z1,z2);
                   b                      b
           (1 - z1)  (b z1 + 1) - (1 - z2)  (b z2 + 1)
(%o1)      -------------------------------------------
                            b (b + 1)
(%i2) beta_incomplete_generalized(a,2,z1,z2);
@group
                              a                      a
           (a (1 - z2) + 1) z2  - (a (1 - z1) + 1) z1
(%o2)      -------------------------------------------
                            a (a + 1)
@end group
(%i3) beta_incomplete_generalized(3,2,z1,z2);
              2      2                       2      2
      (1 - z1)  (3 z1  + 2 z1 + 1) - (1 - z2)  (3 z2  + 2 z2 + 1)
(%o3) -----------------------------------------------------------
                                  12
@end example

Simplification for specific values @math{z1=0}, @math{z2=0}, @math{z1=1}, or 
@math{z2=1}:

@example
(%i4) assume(a > 0, b > 0)$
(%i5) beta_incomplete_generalized(a,b,z1,0);
(%o5)                    - beta_incomplete(a, b, z1)

(%i6) beta_incomplete_generalized(a,b,0,z2);
(%o6)                    - beta_incomplete(a, b, z2)

(%i7) beta_incomplete_generalized(a,b,z1,1);
(%o7)              beta(a, b) - beta_incomplete(a, b, z1)

(%i8) beta_incomplete_generalized(a,b,1,z2);
(%o8)              beta_incomplete(a, b, z2) - beta(a, b)
@end example

Numerical evaluation for real arguments in float or bigfloat precision:

@example
(%i9) beta_incomplete_generalized(1/2,3/2,0.25,0.31);
(%o9)                        .09638178086368676

(%i10) fpprec:32$
(%i10) beta_incomplete_generalized(1/2,3/2,0.25,0.31b0);
(%o10)               9.6381780863686935309170054689964b-2
@end example

Numerical evaluation for complex arguments in float or bigfloat precision:

@example
(%i11) beta_incomplete_generalized(1/2+%i,3/2+%i,0.25,0.31);
(%o11)           - .09625463003205376 %i - .003323847735353769
(%i12) fpprec:20$
(%i13) beta_incomplete_generalized(1/2+%i,3/2+%i,0.25,0.31b0);
(%o13)     - 9.6254630032054178691b-2 %i - 3.3238477353543591914b-3
@end example

Expansion for @math{a+n} or @math{a-n}, @var{n} a positive integer, when 
@mref{beta_expand} is @code{true}: 

@example
(%i14) beta_expand:true$

(%i15) beta_incomplete_generalized(a+1,b,z1,z2);

               b   a           b   a
       (1 - z1)  z1  - (1 - z2)  z2
(%o15) -----------------------------
                   b + a
                      a beta_incomplete_generalized(a, b, z1, z2)
                    + -------------------------------------------
                                         b + a
(%i16) beta_incomplete_generalized(a-1,b,z1,z2);

       beta_incomplete_generalized(a, b, z1, z2) (- b - a + 1)
(%o16) -------------------------------------------------------
                                1 - a
                                    b   a - 1           b   a - 1
                            (1 - z2)  z2      - (1 - z1)  z1
                          - -------------------------------------
                                            1 - a
@end example

Derivative wrt the variable @var{z1} and integrals wrt @var{z1} and @var{z2}:

@example
(%i17) diff(beta_incomplete_generalized(a,b,z1,z2),z1);
                               b - 1   a - 1
(%o17)               - (1 - z1)      z1
(%i18) integrate(beta_incomplete_generalized(a,b,z1,z2),z1);
(%o18) beta_incomplete_generalized(a, b, z1, z2) z1
                                  + beta_incomplete(a + 1, b, z1)
(%i19) integrate(beta_incomplete_generalized(a,b,z1,z2),z2);
(%o19) beta_incomplete_generalized(a, b, z1, z2) z2
                                  - beta_incomplete(a + 1, b, z2)
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end deffn

@anchor{beta_expand}
@defvr {Option variable} beta_expand
Default value: false

When @code{beta_expand} is @code{true}, @code{beta(a,b)} and related 
functions are expanded for arguments like @math{a+n} or @math{a-n}, 
where @math{n} is an integer.

@xref{beta} for examples.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@defvr {Option variable} beta_args_sum_to_integer
Default value: false

When @code{beta_args_sum_to_integer} is @code{true}, Maxima simplifies 
@code{beta(a,b)}, when the arguments @var{a} and @var{b} sum to an integer.

@xref{beta} for examples.

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@category{Simplification flags and variables}
@closecatbox
@end defvr


@c NEED INFO HERE ABOUT THE SUBSCRIPTED FORM psi[n](x)
@c I (rtoy) don't think there is a plain psi(x) function anymore.
@c @deffn {Function} psi (@var{x})
@c @deffnx {Function} psi [@var{n}](@var{x})
@anchor{polygamma}
@deffn {Function} psi [@var{n}](@var{x})

@c The derivative of @code{log (gamma (@var{x}))} of order @code{@var{n}+1}.
@code{psi[n](x)} is the polygamma function (@urldlmf{5.2E2},
@urldlmf{5.15}, @urlaands{eqn 6.3.1, 258} and @urlaands{eqn 6.4.1, 260}) defined by
m4_displaymath(
<<<\psi^{(n)}(x) = {d^{n+1}\over{dx^{n+1}}} \log\Gamma(x)>>>,
<<<@math{diff(log(gamma(x)), x, n+1)}>>>)
Thus, @code{psi[0](@var{x})} is the first derivative,
@code{psi[1](@var{x})} is the second derivative, etc.

Maxima can compute some exact values for rational args as well for
float and bfloat args.  Several variables control what range of
rational args m4_math(<<<\psi^{(n)}(x)>>>,<<<psi[n](x)>>>)) will return an
exact value, if possible.  See @mref{maxpsiposint},
@mref{maxpsinegint}, @mref{maxpsifracnum}, and
@mrefdot{maxpsifracdenom} That is, @math{x} must lie between
@code{maxpsinegint} and @code{maxpsiposint}.  If the absolute value of
the fractional part of @math{x} is rational and has a numerator less
than @code{maxpsifracnum} and has a denominator less than
@code{maxpsifracdenom}, m4_math(<<<\psi^{(0)}(x)>>>,<<<psi[0](x)>>>) will
return an exact value.

The function @mref{bfpsi} in the @code{bffac} package can compute
numerical values.

@example
(%i1) psi[0](.25);
(%o1)                        - 4.227453533376265
(%i2) psi[0](1/4);
                                        %pi
(%o2)                    (- 3 log(2)) - --- - %gamma
                                         2
(%i3) float(%);
(%o3)                        - 4.227453533376265
(%i4) psi[2](0.75);
(%o4)                        - 5.30263321633764
(%i5) psi[2](3/4);
                                   1         3
(%o5)                         psi (-) + 4 %pi
                                 2 4
(%i6) float(%);
(%o6)                        - 5.30263321633764
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end deffn

@anchor{maxpsiposint}
@defvr {Option variable} maxpsiposint
Default value: 20

@code{maxpsiposint} is the largest positive integer value for
which m4_math(<<<\psi^{(n)}(m)>>>,<<<psi[n](x)>>>) gives an exact value for
rational @math{x}.

@example
(%i1) psi[0](20);
                             275295799
(%o1)                        --------- - %gamma
                             77597520
(%i2) psi[0](21);
(%o2)                             psi (21)
                                     0
(%i3) psi[2](20);
                      1683118856778495358491487
(%o3)              2 (------------------------- - zeta(3))
                      1401731326612193601024000
(%i4) psi[2](21);
(%o4)                            psi (21)
                                      2

@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox

@end defvr

@anchor{maxpsinegint}
@defvr {Option variable} maxpsinegint
Default value: -10

@code{maxpsinegint} is the most negative value for
which m4_math(<<<\psi^{(0)}(x)>>>,<<<psi[0](x)>>>) will try to compute an exact
value for rational @math{x}.  That is if @math{x} is less than
@code{maxpsinegint}, m4_math(<<<\psi^{(n)}(x)>>>,<<<psi[n](x)>>>) will not
return simplified answer, even if it could.

@example
(%i1) psi[0](-100/9);
                                        100
(%o1)                            psi (- ---)
                                    0    9
(%i2) psi[0](-100/11);
                         100 %pi         1     5231385863539
(%o2)            %pi cot(-------) + psi (--) + -------------
                           11          0 11    381905105400

(%i3) psi[2](-100/9);
                                        100
(%o3)                            psi (- ---)
                                    2    9
(%i4) psi[2](-100/11);
           3     100 %pi     2 100 %pi         1
(%o4) 2 %pi  cot(-------) csc (-------) + psi (--)
                   11            11          2 11
                                         74191313259470963498957651385614962459
                                       + --------------------------------------
                                          27850718060013605318710152732000000
@end example
@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox

@end defvr

@anchor{maxpsifracnum}
@defvr {Option variable} maxpsifracnum
Default value: 6

Let @math{x} be a rational number of the form @math{p/q}.
If @math{p} is greater than @code{maxpsifracnum},
then m4_math(<<<\psi^{(0)}(x)>>>,<<<@code{psi[0](x)}>>>) will not try to
return a simplified value.

@example
(%i1) psi[0](3/4);
                                        %pi
(%o1)                    (- 3 log(2)) + --- - %gamma
                                         2
(%i2) psi[2](3/4);
                                   1         3
(%o2)                         psi (-) + 4 %pi
                                 2 4
(%i3) maxpsifracnum:2;
(%o3)                                 2
(%i4) psi[0](3/4);
                                        3
(%o4)                              psi (-)
                                      0 4
(%i5) psi[2](3/4);
                                   1         3
(%o5)                         psi (-) + 4 %pi
                                 2 4
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox

@end defvr

@anchor{maxpsifracdenom}
@defvr {Option variable} maxpsifracdenom
Default value: 6

Let @math{x} be a rational number of the form @math{p/q}.
If @math{q} is greater than @code{maxpsifracdenom},
then m4_math(<<<\psi^{(0)}(x)>>>,<<<@code{psi[@var{0}](@var{x})}>>>) will
not try to return a simplified value.

@example
(%i1) psi[0](3/4);
                                        %pi
(%o1)                    (- 3 log(2)) + --- - %gamma
                                         2
(%i2) psi[2](3/4);
                                   1         3
(%o2)                         psi (-) + 4 %pi
                                 2 4
(%i3) maxpsifracdenom:2;
(%o3)                                 2
(%i4) psi[0](3/4);
                                        3
(%o4)                              psi (-)
                                      0 4
(%i5) psi[2](3/4);
                                   1         3
(%o5)                         psi (-) + 4 %pi
                                 2 4
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox

@end defvr

@c NEED CROSS REFS HERE
@c NEED EXAMPLES HERE
@anchor{makefact}
@deffn {Function} makefact (@var{expr})
Transforms instances of binomial, gamma, and beta
functions in @var{expr} into factorials.

See also @mrefdot{makegamma}

@example
(%i1) makefact(binomial(n,k));
                                      n!
(%o1)                             -----------
                                  k! (n - k)!
(%i2) makefact(gamma(x));
(%o2)                              (x - 1)!
(%i3) makefact(beta(a,b));
                               (a - 1)! (b - 1)!
(%o3)                          -----------------
                                 (b + a - 1)!
@end example

@opencatbox{Categories:}
@category{Gamma and factorial functions}
@closecatbox
@end deffn


@c AREN'T THERE OTHER FUNCTIONS THAT DO ESSENTIALLY THE SAME THING ??
@deffn {Function} numfactor (@var{expr})
Returns the numerical factor multiplying the expression
@var{expr}, which should be a single term.

@c WHY IS content MENTIONED IN THIS CONTEXT ??
@mref{content} returns the greatest common divisor (gcd) of all terms in a sum.

@example
(%i1) gamma (7/2);
                          15 sqrt(%pi)
(%o1)                     ------------
                               8
(%i2) numfactor (%);
                               15
(%o2)                          --
                               8
@end example

@opencatbox{Categories:}
@category{Expressions}
@closecatbox
@end deffn


@node Exponential Integrals, Error Function, Gamma and factorial Functions, Special Functions
@section Exponential Integrals

The Exponential Integral and related functions are defined in
Abramowitz and Stegun,
@i{Handbook of Mathematical Functions}, @urlaands{Chapter 5, 228}.

@anchor{expintegral_e1}
@deffn {Function} expintegral_e1 (@var{z})
The Exponential Integral E1(z) defined as

m4_displaymath(
<<<E_1(z) = \int_z^\infty {e^{-t} \over t} dt>>>,
<<<@math{integrate(exp(-t)/t, t, z, inf)}>>>)

with m4_math(<<<\left| \arg z \right| < \pi>>>, <<<abs(arg z) < %pi>>>). 
(@urlaands{eqn 5.1.1, 228}) and (@urldlmf{6.2E2})

This can be written in terms of other functions.  @xref{expintrep} for examples.
@opencatbox{Categories:}
@category{Exponential Integrals}
@category{Special functions}
@closecatbox
@end deffn

@deffn {Function} expintegral_ei (@var{x})
The Exponential Integral Ei(x) defined as

m4_displaymath(
<<<Ei(x)
  = - -\kern-10.5pt\int_{-x}^\infty {e^{-t} \over t} dt
  = -\kern-10.5pt\int_{-\infty}^x {e^{t} \over t} dt  >>>,
<<<@math{-integrate(%e^(-t)/t, t, -x, inf) = integrate(%e^(t)/t, t, -inf, x)} >>>)

with @math{x} real and @math{x > 0}. (@urlaands{eqn 5.1.2, 228}) and (@urldlmf{6.2E5})

This can be written in terms of other functions.  @xref{expintrep} for examples.

@opencatbox{Categories:}
@category{Exponential Integrals}
@category{Special functions}
@closecatbox
@end deffn

@deffn {Function} expintegral_li (@var{x})
The Exponential Integral li(x) defined as

m4_displaymath(
<<<li(x) = -\kern-10.5pt\int_0^x {dt \over \ln t}>>>,
<<<@math{integrate(1/ln(t), t, 0, x)}>>>)

with @math{x} real and @math{x > 1}. (@urlaands{eqn 5.1.3, 228}) and (@urldlmf{6.2E8})

This can be written in terms of other functions.  @xref{expintrep} for examples.

@opencatbox{Categories:}
@category{Exponential Integrals}
@category{Special functions}
@closecatbox
@end deffn

@anchor{expintegral_e}
@deffn {Function} expintegral_e (@var{n},@var{z})
The Exponential Integral En(z) (@urlaands{eqn 5.1.4, 228}) defined as

m4_displaymath(
<<<E_n(z) = \int_1^\infty {e^{-zt} \over t^n} dt>>>,
<<<@math{integrate(exp(-z*t)/t^n, t, 1, inf)}>>>)
with m4_math(<<<{\rm Re}(z) > 1>>>,<<<realpart(z) > 1>>>) and @math{n} a
non-negative integer.

For half-integral orders, this can be written in terms of @mref{erfc}
or @mref{erf}.  @xref{expintexpand} for examples.

@opencatbox{Categories:}
@category{Exponential Integrals}
@category{Special functions}
@closecatbox
@end deffn

@anchor{expintegral_si}
@deffn {Function} expintegral_si (@var{z})
The Exponential Integral Si(z) (@urlaands{eqn 5.2.1, 231}
and @urldlmf{6.2#E9}) defined as

m4_displaymath(
<<<{\rm Si}(z) = \int_0^z {\sin t \over t} dt>>>,
<<<@math{integrate(sin(t)/t, t, 0, z)}>>>)

This can be written in terms of other functions.  @xref{expintrep} for examples.

@opencatbox{Categories:}
@category{Exponential Integrals}
@category{Special functions}
@closecatbox
@end deffn

@anchor{expintegral_ci}
@deffn {Function} expintegral_ci (@var{z})
The Exponential Integral Ci(z) (@urlaands{eqn 5.2.2, 231}
and @urldlmf{6.2#E13}) defined as

m4_displaymath(
<<<{\rm Ci}(z) = \gamma + \log z + \int_0^z {{\cos t - 1} \over t} dt>>>,
<<<@math{%gamma + log(z) + integrate((cos(t) - 1)/t, t, 0, z)}>>>)

with m4_math(<<<|\arg z| < \pi>>>, <<<abs(arg z) < %pi>>>).

This can be written in terms of other functions.  @xref{expintrep} for examples.

@opencatbox{Categories:}
@category{Exponential Integrals}
@category{Special functions}
@closecatbox
@end deffn

@anchor{expintegral_shi}
@deffn {Function} expintegral_shi (@var{z})
The Exponential Integral Shi(z) (@urlaands{eqn 5.2.3, 231}
and @urldlmf{6.2#E15}) defined as

m4_displaymath(
<<<{\rm Shi}(z) = \int_0^z {\sinh t \over t} dt>>>,
<<<@math{integrate(sinh(t)/t, t, 0, z)}>>>)

This can be written in terms of other functions.  @xref{expintrep} for examples.

@opencatbox{Categories:}
@category{Exponential Integrals}
@category{Special functions}
@closecatbox
@end deffn

@anchor{expintegral_chi}
@deffn {Function} expintegral_chi (@var{z})
The Exponential Integral Chi(z) (@urlaands{eqn 5.2.4, 231}
and @urldlmf{6.2#E16}) defined as

m4_displaymath(
<<<{\rm Chi}(z) = \gamma + \log z + \int_0^z {{\cosh t - 1} \over t} dt>>>,
<<<@math{%gamma + log(z) + integrate((cosh(t) - 1)/t, t, 0, z)}>>>)

with m4_math(<<<|\arg z| < \pi>>>, <<<abs(arg z) < %pi>>>).

This can be written in terms of other functions.  @xref{expintrep} for examples.

@opencatbox{Categories:}
@category{Exponential Integrals}
@category{Special functions}
@closecatbox
@end deffn

@anchor{expintrep}
@defvr {Option variable} expintrep
Default value: false

Change the representation of one of the exponential integrals,
@code{expintegral_e(m, z)}, @code{expintegral_e1}, or
@code{expintegral_ei} to an equivalent form if possible.

Possible values for @code{expintrep} are @code{false},
@code{gamma_incomplete}, @code{expintegral_e1}, @code{expintegral_ei},
@code{expintegral_li}, @code{expintegral_trig}, or
@code{expintegral_hyp}.

@code{false} means that the representation is not changed.  Other
values indicate the representation is to be changed to use the
function specified where @code{expintegral_trig} means
@mref{expintegral_si}, @mref{expintegral_ci}; and @code{expintegral_hyp}
means @mref{expintegral_shi} or @mref{expintegral_chi}.

Here are some examples for @code{expintrep} set to @code{gamma_incomplete}:
@example
(%i1) expintrep:'gamma_incomplete;
(%o1)                          gamma_incomplete
(%i2) expintegral_e1(z);
(%o2)                       gamma_incomplete(0, z)
(%i3) expintegral_ei(z);
(%o3)            log(z) - log(- z) - gamma_incomplete(0, - z)
(%i4) expintegral_li(z);
(%o4)     log(log(z)) - log(- log(z)) - gamma_incomplete(0, - log(z))
(%i5) expintegral_e(n,z);
                                                   n - 1
(%o5)                  gamma_incomplete(1 - n, z) z
(%i6) expintegral_si(z);
(%o6) (%i ((- log(%i z)) + log(- %i z) - gamma_incomplete(0, %i z)
                                              + gamma_incomplete(0, - %i z)))/2
(%i7) expintegral_ci(z);
(%o7) log(z) - (log(%i z) + log(- %i z) + gamma_incomplete(0, %i z)
                                               + gamma_incomplete(0, - %i z))/2
(%i8) expintegral_shi(z);
      log(z) - log(- z) + gamma_incomplete(0, z) - gamma_incomplete(0, - z)
(%o8) ---------------------------------------------------------------------
                                        2
(%i9) expintegral_chi(z);
(%o9) 
      (- log(z)) + log(- z) + gamma_incomplete(0, z) + gamma_incomplete(0, - z)
    - -------------------------------------------------------------------------
                                          2
@end example

For @code{expintrep} set to @code{expintegral_e1}:

@example
(%i1) expintrep:'expintegral_e1;
(%o1)                           expintegral_e1
(%i2) expintegral_ei(z);
(%o2)               log(z) - log(- z) - expintegral_e1(- z)
(%i3) expintegral_li(z);
(%o3)       log(log(z)) - log(- log(z)) - expintegral_e1(- log(z))
(%i4) expintegral_e(n,z);
(%o4)                         expintegral_e(n, z)
(%i5) expintegral_si(z);
(%o5) (%i ((- log(%i z)) - expintegral_e1(%i z) + log(- %i z)
                                                   + expintegral_e1(- %i z)))/2
(%i6) expintegral_ci(z);
(%o6) log(z)
          log(- %i z) (expintegral_e1(%i z) + expintegral_e1(- %i z)) log(%i z)
        - ---------------------------------------------------------------------
                                            2
(%i7) expintegral_shi(z);
          log(z) + expintegral_e1(z) - log(- z) - expintegral_e1(- z)
(%o7)     -----------------------------------------------------------
                                       2
(%i8) expintegral_chi(z);
         (- log(z)) + expintegral_e1(z) + log(- z) + expintegral_e1(- z)
(%o8)  - ---------------------------------------------------------------
                                        2
@end example

For @code{expintrep} set to @code{expintegral_ei}:

@example
(%i1) expintrep:'expintegral_ei;
(%o1)                           expintegral_ei
(%i2) expintegral_e1(z);
                                          1
                         log(- z) - log(- -)
                                          z
(%o2)       (- log(z)) + ------------------- - expintegral_ei(- z)
                                  2
(%i3) expintegral_ei(z);
(%o3)                          expintegral_ei(z)
(%i4) expintegral_li(z);
(%o4)                       expintegral_ei(log(z))
(%i5) expintegral_e(n,z);
(%o5)                         expintegral_e(n, z)
(%i6) expintegral_si(z);
(%o6) (%i (log(%i z) + 2 (expintegral_ei(- %i z) - expintegral_ei(%i z))
                                                            %i          %i
                                        - log(- %i z) + log(--) - log(- --)))/4
                                                            z           z
(%i7) expintegral_ci(z);
(%o7) ((- log(%i z)) + 2 (expintegral_ei(%i z) + expintegral_ei(- %i z))
                                                    %i          %i
                                - log(- %i z) + log(--) + log(- --))/4 + log(z)
                                                    z           z
(%i8) expintegral_shi(z);
(%o8) ((- 2 log(z)) + 2 (expintegral_ei(z) - expintegral_ei(- z)) + log(- z)
                                                                          1
                                                                  - log(- -))/4
                                                                          z
(%i9) expintegral_chi(z);
(%o9) 
                                                                             1
   2 log(z) + 2 (expintegral_ei(z) + expintegral_ei(- z)) - log(- z) + log(- -)
                                                                             z
   ----------------------------------------------------------------------------
                                        4
@end example

For @code{expintrep} set to @code{expintegral_li}:

@example
(%i1) expintrep:'expintegral_li;
(%o1)                           expintegral_li
(%i2) expintegral_e1(z);
                                                                 1
                                                log(- z) - log(- -)
                               - z                               z
(%o2)      (- expintegral_li(%e   )) - log(z) + -------------------
                                                         2
(%i3) expintegral_ei(z);
                                               z
(%o3)                         expintegral_li(%e )
(%i4) expintegral_li(z);
(%o4)                          expintegral_li(z)
(%i5) expintegral_e(n,z);
(%o5)                         expintegral_e(n, z)
(%i6) expintegral_si(z);
                             %i z                     - %e z    %pi signum(z)
        %i (expintegral_li(%e    ) - expintegral_li(%e      ) - -------------)
                                                                      2
(%o6) - ----------------------------------------------------------------------
                                          2
(%i7) expintegral_ci(z);
                        %i z                     - %i z
       expintegral_li(%e    ) + expintegral_li(%e      )
(%o7)  ------------------------------------------------- - signum(z) + 1
                               2
(%i8) expintegral_shi(z);
                                   z                     - z
                  expintegral_li(%e ) - expintegral_li(%e   )
(%o8)             -------------------------------------------
                                       2
(%i9) expintegral_chi(z);
                                   z                     - z
                  expintegral_li(%e ) + expintegral_li(%e   )
(%o9)             -------------------------------------------
                                       2
@end example

For @code{expintrep} set to @code{expintegral_trig}:

@example
(%i1) expintrep:'expintegral_trig;
(%o1)                          expintegral_trig
(%i2) expintegral_e1(z);
(%o2) log(%i z) - %i expintegral_si(%i z) - expintegral_ci(%i z) - log(z)
(%i3) expintegral_ei(z);
(%o3) (- log(%i z)) - %i expintegral_si(%i z) + expintegral_ci(%i z) + log(z)
(%i4) expintegral_li(z);
(%o4) (- log(%i log(z))) - %i expintegral_si(%i log(z))
                                      + expintegral_ci(%i log(z)) + log(log(z))
(%i5) expintegral_e(n,z);
(%o5)                         expintegral_e(n, z)
(%i6) expintegral_si(z);
(%o6)                          expintegral_si(z)
(%i7) expintegral_ci(z);
(%o7)                          expintegral_ci(z)
(%i8) expintegral_shi(z);
(%o8)                      - %i expintegral_si(%i z)
(%i9) expintegral_chi(z);
(%o9)            (- log(%i z)) + expintegral_ci(%i z) + log(z)
@end example

For @code{expintrep} set to @code{expintegral_hyp}:

@example
(%i1) expintrep:'expintegral_hyp;
(%o1)                           expintegral_hyp
(%i2) expintegral_e1(z);
(%o2)               expintegral_shi(z) - expintegral_chi(z)
(%i3) expintegral_ei(z);
(%o3)               expintegral_shi(z) + expintegral_chi(z)
(%i4) expintegral_li(z);
(%o4)          expintegral_shi(log(z)) + expintegral_chi(log(z))
(%i5) expintegral_e(n,z);
(%o5)                         expintegral_e(n, z)
(%i6) expintegral_si(z);
(%o6)                     - %i expintegral_shi(%i z)
(%i7) expintegral_ci(z);
(%o7)           (- log(%i z)) + expintegral_chi(%i z) + log(z)
(%i8) expintegral_shi(z);
(%o8)                         expintegral_shi(z)
(%i9) expintegral_chi(z);
(%o9)                         expintegral_chi(z)
@end example

@opencatbox{Categories:}
@category{Exponential Integrals}
@closecatbox
@end defvr

@anchor{expintexpand}
@defvr {Option variable} expintexpand 
Default value: false

Expand @code{expintegral_e(n,z)} for half
integral values in terms of @code{erfc} or @code{erf} and
for positive integers in terms of @code{expintegral_ei}.

@example
(%i1) expintegral_e(1/2,z);
                                            1
(%o1)                         expintegral_e(-, z)
                                            2
(%i2) expintegral_e(1,z);
(%o2)                         expintegral_e(1, z)
(%i3) expintexpand:true;
(%o3)                                true
(%i4) expintegral_e(1/2,z);
                            sqrt(%pi) erfc(sqrt(z))
(%o4)                       -----------------------
                                    sqrt(z)
(%i5) expintegral_e(1,z);
                               1
                         log(- -) - log(- z)
                               z
(%o5)       (- log(z)) - ------------------- - expintegral_ei(- z)
                                  2
@end example

@opencatbox{Categories:}
@category{Exponential Integrals}
@closecatbox
@end defvr

@node Error Function, Struve Functions, Exponential Integrals, Special Functions
@section Error Function

The Error function and related functions are defined in
Abramowitz and Stegun,
@i{Handbook of Mathematical Functions}, @urlaands{Chapter 7, 297} and (@urldlmf{7})

@c -----------------------------------------------------------------------------
@anchor{erf}
@deffn {Function} erf (@var{z})

The Error Function erf(z):
m4_displaymath(
<<<{\rm erf}\ z = {{2\over \sqrt{\pi}}} \int_0^z e^{-t^2}\, dt>>>,
<<<@math{erf(z) = 2/sqrt(%pi)*integrate(exp(-t^2), t, 0, z)}>>>)

(@urlaands{eqn 7.1.1, 297}) and (@urldlmf{7.2.E1}).

See also flag @mrefdot{erfflag}  This can also be expressed in terms
of a hypergeometric function.  @xref{hypergeometric_representation}.
@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@anchor{erfc}
@deffn {Function} erfc (@var{z})
The Complementary Error Function erfc(z):
m4_displaymath(
<<<{\rm erfc}\ z = 1 - {\rm erf}\ z>>>,
<<<@math{erfc(z) = 1-erf(z)}>>>)

(@urlaands{eqn 7.1.2, 297}) and (@urldlmf{7.2.E2}).

This can also be expressed in terms
of a hypergeometric function.  @xref{hypergeometric_representation}.
@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@anchor{erfi}
@deffn {Function} erfi (@var{z})
The Imaginary Error Function. 
m4_displaymath(
<<<{\rm erfi}\ z = -i\, {\rm erf}(i z)>>>,
<<<@math{erfi(z) = -%i*erf(%i*z)}>>>)

@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@anchor{erf_generalized}
@deffn {Function} erf_generalized (@var{z1},@var{z2})
Generalized Error function Erf(z1,z2):
m4_displaymath(
<<<{\rm erf}(z_1, z_2) = {{2\over \sqrt{\pi}}} \int_{z_1}^{z_2} e^{-t^2}\, dt>>>,
<<<@math{erf(z) = 2/sqrt(%pi)*integrate(exp(-t^2), t, z1, z2)}>>>)

This can also be expressed in terms
of a hypergeometric function.  @xref{hypergeometric_representation}.
@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@anchor{fresnel_c}
@deffn {Function} fresnel_c (@var{z})
The Fresnel Integral

m4_displaymath(
<<<C(z) = \int_0^z \cos\left({\pi \over 2} t^2\right)\, dt>>>,
<<<@math{C(z) = integrate(cos((%pi/2)*t^2),t,0,z)}>>>)

(@urlaands{eqn 7.3.1, 300}) and (@urldlmf{7.2.E7}).

The simplification m4_math(<<<C(-x) = -C(x)>>>, <<<@code{fresnel_c(-x) = -fresnel_c(x)}>>>) is applied when
flag @mref{trigsign} is true.

The simplification m4_math(<<<C(ix) = iC(x)>>>, <<<@code{fresnel_c(%i*x) =  %i*fresnel_c(x)}>>>) is applied when
flag @mref{%iargs} is true.

See flags @mref{erf_representation} and @mref{hypergeometric_representation}.
@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@anchor{fresnel_s}
@deffn {Function} fresnel_s (@var{z})
The Fresnel Integral
m4_displaymath(
<<<S(z) = \int_0^z \sin\left({\pi \over 2} t^2\right)\, dt>>>,
<<<@math{S(z) = integrate(sin((%pi/2)*t^2),t,0,z)}>>>)

(@urlaands{eqn 7.3.2, 300}) and (@urldlmf{7.2.E8}).

The simplification m4_math(<<<S(-x) = -S(x)>>>, <<<@code{fresnel_s(-x) = -fresnel_s(x)}>>>) is applied when
flag @mref{trigsign} is true.

The simplification m4_math(<<<S(ix) = iS(x)>>>, <<<@code{fresnel_s(%i*x) =  -%i*fresnel_s(x)}>>>) is applied when
flag @mref{%iargs} is true.

See flags @mref{erf_representation} and @mref{hypergeometric_representation}.
@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@anchor{erf_representation}
@defvr {Option variable} erf_representation
Default value: false

@code{erf_representation} controls how the error functions are
represented.  It must be set to one of @code{false}, @code{erf},
@code{erfc}, or @code{erfi}.  When set to @code{false}, the error functions are not
modified.  When set to @code{erf}, all error functions (@mref{erfc},
@mref{erfi}, @mref{erf_generalized}, @mref{fresnel_s} and
@mref{fresnel_c}) are converted to @mref{erf} functions.  Similary,
@code{erfc} converts error functions to @mref{erfc}.  Finally
@code{erfi} converts the functions to @mref{erfi}.

Converting to @mref{erf}:
@example
(%i1) erf_representation:erf;
(%o1)                                true
(%i2) erfc(z);
(%o2)                               erfc(z)
(%i3) erfi(z);
(%o3)                               erfi(z)
(%i4) erf_generalized(z1,z2);
(%o4)                          erf(z2) - erf(z1)
(%i5) fresnel_c(z);
                    sqrt(%pi) (%i + 1) z           sqrt(%pi) (1 - %i) z
      (1 - %i) (erf(--------------------) + %i erf(--------------------))
                             2                              2
(%o5) -------------------------------------------------------------------
                                       4
(%i6) fresnel_s(z);
                    sqrt(%pi) (%i + 1) z           sqrt(%pi) (1 - %i) z
      (%i + 1) (erf(--------------------) - %i erf(--------------------))
                             2                              2
(%o6) -------------------------------------------------------------------
                                       4

@end example

Converting to @mref{erfc}:
@example
(%i1) erf_representation:erfc;
(%o1)                                erfc
(%i2) erf(z);
(%o2)                             1 - erfc(z)
(%i3) erfc(z);
(%o3)                               erfc(z)
(%i4) erf_generalized(z1,z2);
(%o4)                         erfc(z1) - erfc(z2)
(%i5) fresnel_s(c);
                         sqrt(%pi) (%i + 1) c
(%o5) ((%i + 1) ((- erfc(--------------------))
                                  2
                                                 sqrt(%pi) (1 - %i) c
                                  - %i (1 - erfc(--------------------)) + 1))/4
                                                          2
(%i6) fresnel_c(c);
                         sqrt(%pi) (%i + 1) c
(%o6) ((1 - %i) ((- erfc(--------------------))
                                  2
                                                 sqrt(%pi) (1 - %i) c
                                  + %i (1 - erfc(--------------------)) + 1))/4
                                                          2
@end example

Converting to @mref{erfc}:

@example
(%i1) erf_representation:erfi;
(%o1)                                erfi
(%i2) erf(z);
(%o2)                           - %i erfi(%i z)
(%i3) erfc(z);
(%o3)                          %i erfi(%i z) + 1
(%i4) erfi(z);
(%o4)                               erfi(z)
(%i5) erf_generalized(z1,z2);
(%o5)                   %i erfi(%i z1) - %i erfi(%i z2)
(%i6) fresnel_s(z);
                            sqrt(%pi) %i (%i + 1) z
(%o6) ((%i + 1) ((- %i erfi(-----------------------))
                                       2
                                                   sqrt(%pi) (1 - %i) %i z
                                            - erfi(-----------------------)))/4
                                                              2
(%i7) fresnel_c(z);
(%o7) 
                   sqrt(%pi) (1 - %i) %i z            sqrt(%pi) %i (%i + 1) z
    (1 - %i) (erfi(-----------------------) - %i erfi(-----------------------))
                              2                                  2
    ---------------------------------------------------------------------------
                                         4
@end example
@end defvr

@anchor{hypergeometric_representation}
@defvr {Option variable} hypergeometric_representation
Default value: false

Enables transformation to a Hypergeometric
representation for @mref{fresnel_s} and @mref{fresnel_c} and other
error functions.

@example
(%i1) hypergeometric_representation:true;
(%o1)                                true
(%i2) fresnel_s(z);
                                                      2  4
                                     3    3  7     %pi  z    3
                 %pi hypergeometric([-], [-, -], - -------) z
                                     4    2  4       16
(%o2)            ---------------------------------------------
                                       6
(%i3) fresnel_c(z);
                                                    2  4
                                   1    1  5     %pi  z
(%o3)              hypergeometric([-], [-, -], - -------) z
                                   4    2  4       16
(%i4) erf(z);
                                        1    3      2
                      2 hypergeometric([-], [-], - z ) z
                                        2    2
(%o4)                 ----------------------------------
                                  sqrt(%pi)
(%i5) erfi(z);
                                         1    3    2
                       2 hypergeometric([-], [-], z ) z
                                         2    2
(%o5)                  --------------------------------
                                  sqrt(%pi)
(%i6) erfc(z);
                                          1    3      2
                        2 hypergeometric([-], [-], - z ) z
                                          2    2
(%o6)               1 - ----------------------------------
                                    sqrt(%pi)
(%i7) erf_generalized(z1,z2);
                        1    3       2
      2 hypergeometric([-], [-], - z2 ) z2
                        2    2
(%o7) ------------------------------------
                   sqrt(%pi)
                                                             1    3       2
                                           2 hypergeometric([-], [-], - z1 ) z1
                                                             2    2
                                         - ------------------------------------
                                                        sqrt(%pi)
@end example

@end defvr

@node Struve Functions, Hypergeometric Functions, Error Function, Special Functions
@section Struve Functions

The Struve functions are defined in Abramowitz and Stegun,
@i{Handbook of Mathematical Functions}, @urlaands{Chapter 12, 496} and (@urldlmf{11}).
The Struve Function m4_math(<<<{\bf H}_{\nu}(z)>>>,<<<H[v](z)>>>) is a particular solution
of the differential equation:
m4_displaymath(
<<<z^2 {d^2 w \over dz^2} + z {dw \over dz} + (z^2-\nu^2)w =
{{4\left({1\over 2} z\right)^{\nu+1}} \over \sqrt{\pi} \Gamma\left(\nu + {1\over 2}\right)}>>>,
<<<@math{z^2*diff(w,z,2) + z*diff(w,z)+(z^2-v^2)*w = 4*(z/2)^(v+1)/(sqrt(%pi)*gamma(v+1/2))}>>>)

which has the general soution
m4_displaymath(
<<<w = aJ_{\nu}(z) + bY_{\nu}(z) + {\bf H}_{\nu}(z)>>>,
<<<@math{w = a*bessel_j(v,z) + b*bessel_y(v,z) + struve_h(v,z)}>>>)

@c -----------------------------------------------------------------------------
@anchor{struve_h}
@deffn {Function} struve_h (@var{v}, @var{z})
The Struve Function H of order m4_math(<<<\nu>>>, v) and argument @math{z}:

m4_displaymath(
<<<{\bf H}_{\nu}(z) = \left({z\over 2}\right)^{\nu+1}
\sum_{k=0}^{\infty} {(-1)^k\left({z\over 2}\right)^{2k} \over \Gamma\left(k + {3\over 2}\right) \Gamma\left(k + \nu + {3\over 2}\right)}>>>,
<<<@math{struve_h(v,z) = (z/2)^(v+1)*sum((-1)^k*(z/2)^(2*k)/(gamma(k+3/2)*gamma(k+v+3/2)), k, 0, inf)}>>>)

(@urlaands{eqn 12.1.3, 496}) and (@urldlmf{11.2.E1}).

When @code{besselexpand} is @code{true}, @code{struve_h} is expanded in terms
of elementary functions when the order @math{v} is half of an odd integer. 
See @mrefdot{besselexpand}

@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{struve_l}
@deffn {Function} struve_l (@var{v}, @var{z})
The Modified Struve Function L of order m4_math(<<<\nu>>>, v) and argument @math{z}:
m4_displaymath(
<<<{\bf L}_{\nu}(z) = -ie^{-{i\nu\pi\over 2}} {\bf H}_{\nu}(iz)>>>,
<<<@math{struve_l(v,z) = %i*exp(-%i*v*%pi/2)*struve_h(v,z)}>>>)

(@urlaands{eqn 12.2.1, 498}) and (@urldlmf{11.2.E2}).

When @code{besselexpand} is @code{true}, @code{struve_l} is expanded in terms
of elementary functions when the order @math{v} is half of an odd integer. 
See @mrefdot{besselexpand}

@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@node Hypergeometric Functions, Parabolic Cylinder Functions, Struve Functions, Special Functions
@section Hypergeometric Functions

The Hypergeometric Functions are defined in Abramowitz and Stegun,
@i{Handbook of Mathematical Functions}, @urlaands{Chapters 13, 504} and
@urlaands{15, 555}.

Maxima has very limited knowledge of these functions.  They
can be returned from function @code{hgfred}.

@deffn {Function} %m [@var{k},@var{u}] (@var{z}) 
Whittaker M function (@urlaands{eqn 13.1.32, 505}):

m4_displaymath(
<<<M_{\kappa,\mu}(z) = e^{-{1\over 2}z} z^{{1\over 2} + \mu} M\left({1\over 2} + \mu - \kappa, 1 + 2\mu, z\right)>>>,
<<<@math{%m[k,u](z) = exp(-z/2)*z^(1/2+u)*M(1/2+u-k,1+2*u,z)}>>>)

where @math{M(a,b,z)} is Kummer's solution of the confluent hypergeometric equation.

This can also be expressed by the series (@urldlmf{13.14.E6}):
m4_displaymath(
<<<M_{\kappa,\mu}(z) = e^{-{1\over 2} z} z^{{1\over 2} + \mu}
\sum_{s=0}^{\infty} {\left({1\over 2} + \mu - \kappa\right)_s \over (1 + 2\mu)_s s!} z^s>>>,
<<<@math{%m[k,u](z) = %e^(-z/2)*z^(1/2+u)*sum(pochhammer(1/2+u-k,s)/(pochhammer(1+2*u,s)*s!)*z^s, s, 0, inf)}>>>)

@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@deffn {Function} %w [@var{k},@var{u}] (@var{z}) 
Whittaker W function (@urlaands{eqn 13.1.33, 505}):
m4_displaymath(
<<<W_{\kappa,\mu}(z) = e^{-{1\over 2}z} z^{{1\over 2} + \mu} U\left({1\over 2} + \mu - \kappa, 1+2\mu,z\right)>>>,
<<<@math{%w[k,v](z) = exp(-z/2)*z^(1/2+u)*U(1/2+u-k, 1+2*u, z)}>>>)

where @math{U(a,b,z)} is Kummer's second solution of the confluent hypergeometric equation.

@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn


@deffn {Function} %f [@var{p},@var{q}] (@var{[a],[b],z}) 
The m4_math(<<<_{p}F_{q}(a_1,a_2,...,a_p;b_1,b_2,...,b_q;z)>>>,<<<pFq(a_1,a_2,...,a_p;b_1,b_2,...,b_q;z)>>>) hypergeometric function,
where @var{a} a list of length @var{p} and 
@var{b} a list of length @var{q}.
@opencatbox{Categories:}
@category{Bessel functions}
@category{Special functions}
@closecatbox
@end deffn

@deffn {Function} hypergeometric ([@var{a1}, ..., @var{ap}],[@var{b1}, ... ,@var{bq}], x)
The hypergeometric function. Unlike Maxima's @code{%f} hypergeometric
function, the function @code{hypergeometric} is a simplifying
function; also, @code{hypergeometric} supports complex double and
big floating point evaluation. For the Gauss hypergeometric function,
that is @math{p = 2} and @math{q = 1}, floating point evaluation
outside the unit circle is supported, but in general, it is not
supported.

When the option variable @code{expand_hypergeometric} is true (default
is false) and one of the arguments @code{a1} through @code{ap} is a
negative integer (a polynomial case), @code{hypergeometric} returns an
expanded polynomial. 

Examples:

@example
(%i1)  hypergeometric([],[],x);
(%o1) %e^x
@end example

Polynomial cases automatically expand when @code{expand_hypergeometric} is true:

@example
(%i2) hypergeometric([-3],[7],x);
(%o2) hypergeometric([-3],[7],x)

(%i3) hypergeometric([-3],[7],x), expand_hypergeometric : true;
(%o3) -x^3/504+3*x^2/56-3*x/7+1
@end example

Both double float and big float evaluation is supported:

@example
(%i4) hypergeometric([5.1],[7.1 + %i],0.42);
(%o4)       1.346250786375334 - 0.0559061414208204 %i
(%i5) hypergeometric([5,6],[8], 5.7 - %i);
(%o5)     .007375824009774946 - .001049813688578674 %i
(%i6) hypergeometric([5,6],[8], 5.7b0 - %i), fpprec : 30;
(%o6) 7.37582400977494674506442010824b-3
                          - 1.04981368857867315858055393376b-3 %i
@end example
@end deffn

@deffn {Function} hypergeometric_simp (@var{e})

@code{hypergeometric_simp} simplifies hypergeometric functions
by applying @code{hgfred}
to the arguments of any hypergeometric functions in the expression @var{e}.

Only instances of @code{hypergeometric} are affected;
any @code{%f}, @code{%w}, and @code{%m} in the expression @var{e} are not affected.
Any unsimplified hypergeometric functions are returned unchanged
(instead of changing to @code{%f} as @code{hgfred} would).

@code{load("hypergeometric");} loads this function.

See also @mrefdot{hgfred}

Examples:

@c ===beg===
@c load ("hypergeometric") $
@c foo : [hypergeometric([1,1], [2], z), hypergeometric([1/2], [1], z)];
@c hypergeometric_simp (foo);
@c bar : hypergeometric([n], [m], z + 1);
@c hypergeometric_simp (bar);
@c ===end===
@example
(%i1) load ("hypergeometric") $
(%i2) foo : [hypergeometric([1,1], [2], z), hypergeometric([1/2], [1], z)];
(%o2) [hypergeometric([1, 1], [2], z), 
                                                     1
                                     hypergeometric([-], [1], z)]
                                                     2
(%i3) hypergeometric_simp (foo);
                 log(1 - z)              z    z/2
(%o3)         [- ----------, bessel_i(0, -) %e   ]
                     z                   2
(%i4) bar : hypergeometric([n], [m], z + 1);
(%o4)            hypergeometric([n], [m], z + 1)
(%i5) hypergeometric_simp (bar);
(%o5)            hypergeometric([n], [m], z + 1)
@end example

@opencatbox{Categories:}
@category{Hypergeometric functions}
@category{Simplification functions}
@category{Special functions}
@closecatbox
@end deffn

@anchor{hgfred}
@deffn {Function} hgfred (@var{a}, @var{b}, @var{t})

Simplify the generalized hypergeometric function in terms of other,
simpler, forms.  @var{a} is a list of numerator parameters and @var{b}
is a list of the denominator parameters. 

If @code{hgfred} cannot simplify the hypergeometric function, it returns
an expression of the form @code{%f[p,q]([a], [b], x)} where @var{p} is
the number of elements in @var{a}, and @var{q} is the number of elements
in @var{b}.  This is the usual m4_math(<<<_pF_q>>>,<<<@code{pFq}>>>) generalized hypergeometric
function. 

@example
(%i1) assume(not(equal(z,0)));
(%o1)                          [notequal(z, 0)]
(%i2) hgfred([v+1/2],[2*v+1],2*%i*z);

                     v/2                               %i z
                    4    bessel_j(v, z) gamma(v + 1) %e
(%o2)               ---------------------------------------
                                       v
                                      z
(%i3) hgfred([1,1],[2],z);

                                   log(1 - z)
(%o3)                            - ----------
                                       z
(%i4) hgfred([a,a+1/2],[3/2],z^2);

                               1 - 2 a          1 - 2 a
                        (z + 1)        - (1 - z)
(%o4)                   -------------------------------
                                 2 (1 - 2 a) z

@end example

It can be beneficial to load orthopoly too as the following example
shows.  Note that @var{L} is the generalized Laguerre polynomial.

@example
(%i5) load("orthopoly")$
(%i6) hgfred([-2],[a],z);
@group

                                    (a - 1)
                                 2 L       (z)
                                    2
(%o6)                            -------------
                                   a (a + 1)
@end group
(%i7) ev(%);

                                  2
                                 z        2 z
(%o7)                         --------- - --- + 1
                              a (a + 1)    a

@end example
@end deffn

@node Parabolic Cylinder Functions, Functions and Variables for Special Functions, Hypergeometric Functions, Special Functions
@section  Parabolic Cylinder Functions

The Parabolic Cylinder Functions are defined in Abramowitz and Stegun,
@i{Handbook of Mathematical Functions}, @urlaands{Chapter 19, 686}.

Maxima has very limited knowledge of these functions.  They
can be returned from function @code{hgfred}.

@deffn {Function} parabolic_cylinder_d (@var{v}, @var{z}) 
The parabolic cylinder function @code{parabolic_cylinder_d(v,z)}. (@urlaands{eqn 19.3.1, 687}).

@c See https://mathworld.wolfram.com/ParabolicCylinderFunction.html for more info.
The solution of the Weber differential equation
m4_displaymath(
<<<y''(z) + \left(\nu + {1\over 2} - {1\over 4} z^2\right) y(z) = 0>>>,
<<<diff(y(z), z, 2) + (v+1/2-z^2/4)*y(z) = 0>>>)
has two independent solutions, one of which is m4_math(<<<D_{\nu}(z)>>>, <<<@code{parabolic_cylinder_d(v,z)}>>>), the parabolic cylinder d function.

@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn




@node Functions and Variables for Special Functions,  , Parabolic Cylinder Functions, Special Functions
@section Functions and Variables for Special Functions


@deffn {Function} lambert_w (@var{z})
The principal branch of Lambert's W function W(z) (@urldlmf{4.13}), the solution of 
m4_displaymath(
<<<z = W(z)e^{W(z)}>>>,
@math{z = W(z) * exp(W(z))})  
@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@deffn {Function} generalized_lambert_w (@var{k}, @var{z})
The @var{k}-th branch of Lambert's W function W(z) (@urldlmf{4.13}), the solution
of m4_math(<<<z=W(z)e^{W(z)}>>>,z = W(z) * exp(W(z))).

The principal branch, denoted m4_math(W_p(z),Wp(z)) in DLMF, is @code{lambert_w(z) = generalized_lambert_w(0,z)}.

The other branch with real values, denoted m4_math(W_m(z), Wm(z)) in DLMF, is @code{generalized_lambert_w(-1,z)}.
@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn


@anchor{kbateman}
@deffn {Function} kbateman [@var{v}] (@var{x})
The Bateman k function

m4_displaymath(
<<<k_v(x)
 = \frac{2}{\pi} \int_0^{\frac{\pi}{2}} \cos(x \tan\theta-v\theta)d\theta>>>,
<<<@math{kbateman[v](x) = (2/%pi) integrate(cos(x*tan(t)-v*t),t,0,%pi/2)}>>>)

It is a special case of the confluent hypergeometric function. Maxima can
calculate the Laplace transform of @code{kbateman} using @mref{laplace}
or @mrefcomma{specint} but has no other knowledge of this function.

@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn



@deffn {Function} nzeta (@var{z})
The Plasma Dispersion Function 
m4_displaymath(
<<<{\rm nzeta}(z) = i\sqrt{\pi}e^{-z^2}(1-{\rm erf}(-iz))>>>,
<<<@math{nzeta(z) = %i*sqrt(%pi)*exp(-z^2)*(1-erf(-%i*z))}>>>)

@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@deffn {Function} nzetar (@var{z})
Returns @code{realpart(nzeta(z))}.
@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

@deffn {Function} nzetai (@var{z})
Returns @code{imagpart(nzeta(z))}.
@opencatbox{Categories:}
@category{Special functions}
@closecatbox
@end deffn

