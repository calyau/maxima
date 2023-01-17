@c -*- mode: texinfo -*-
@menu
@end menu

@node Introduction to trigtools, Functions and Variables for trigtools
@section Introduction to trigtools
@center Trigtools Package
@center Aleksas Dormarkas
@center @email{aleksasd873@@gmail.com}
@center @email{aleksas.domarkas@@mif.vu.lt}
@center December 1, 2013

We use open-source computer algebra system(CAS) maxima 5.31.2.
The trigtools package contains commands that help you work with
trigonometric expessions. List of functions in trigtools package:

@itemize
@item c2sin
@item c2cos
@item c2trig
@item c2hyp
@item trigfactor
@item trigsolve
@item trigvalue
@item trigeval
@item atan contract
@end itemize

@node Functions and Variables for trigtools,  , Introduction to trigtools
@section Functions and Variables for trigtools

@menu
* c2sin and c2cos::
* c2trig::
* c2hyp::
* trigfactor::
* trigsolve::
* trigvalue::
* trigeval::
* atan_contract::
@end menu

@node c2sin and c2cos, c2trig, Functions and Variables for trigtools, Functions and Variables for trigtools
@subsection c2sin and c2cos

@anchor{c2sin}
@deffn {Function} c2sin (@var{x})
The function c2sin converts the expression m4_math(<<<a\cos x + b\sin x>>>, <<<@math{a*cos(x)+b*sin(x)}>>>) to
@m4_mathdot(<<<r\sin(x+\phi)>>>, <<<@math{r*sin(x+phi)}>>>)
@end deffn

@anchor{c2cos}
@deffn {Function} c2cos (@var{x})
The function c2cos converts the expression m4_math(<<<a\cos x + b\sin x>>>, <<<@math{ a*cos(x)+b*sin(x)}>>>) to
m4_mathdot(<<<r\cos(x-\phi)>>>, <<<@math{r*cos(x-phi)}>>>)

@end deffn

@example
(%i1) load("trigtools")$
(%i2) c2sin(3*sin(x)+4*cos(x));
                                            4
(%o2)                        5 sin(x + atan(-))
                                            3
(%i3) trigexpand(%),expand;
(%o3)                        3 sin(x) + 4 cos(x)

(%i4) c2cos(3*sin(x)-4*cos(x));
                                             3
(%o4)                       - 5 cos(x + atan(-))
                                             4
(%i5) trigexpand(%),expand;
(%o5)                        3 sin(x) - 4 cos(x)
(%i6) c2sin(sin(x)+cos(x));
                                            %pi
(%o6)                       sqrt(2) sin(x + ---)
                                             4
(%i7) trigexpand(%),expand;
(%o7)                          sin(x) + cos(x)
(%i8) c2cos(sin(x)+cos(x));
                                            %pi
(%o8)                       sqrt(2) cos(x - ---)
                                             4
(%i9) trigexpand(%),expand;
(%o9)                          sin(x) + cos(x)
@end example

Example. Solve trigonometric equation

@example
(%i10) eq:3*sin(x)+4*cos(x)=2;
(%o10)                      3 sin(x) + 4 cos(x) = 2
(%i12) eq1:c2sin(lhs(eq))=2;
                                           4
(%o35)                      5 sin(x + atan(-)) = 2
                                           3
(%i13) solvetrigwarn:false$
(%i14) solve(eq1)[1]$ x1:rhs(%);
                                    2         4
(%o15)                         asin(-) - atan(-)
                                    5         3
(%i16) float(%), numer;
(%o39)                       - 0.5157783719341241
(%i17) eq2:c2cos(lhs(eq))=2;
                                           3
(%o17)                      5 cos(x - atan(-)) = 2
(%i18) solve(eq2,x)[1]$ x2:rhs(%);
                                    3         2
(%o19)                         atan(-) + acos(-)
                                    4         5
(%i20) float(%), numer;
(%o20)                         1.802780589520693
(%i21) sol:[x1,x2];
                          2         4        3         2
(%o44)              [asin(-) - atan(-), atan(-) + acos(-)]
                          5         3        4         5
@end example

Answ.: m4_mathcomma(<<<x = x_1 + 2\pi k>>>,<<<x = x1 + 2*%pi*k>>>),@w{}
m4_mathcomma(<<<x_1 = \sin^{-1}{2\over 5} - \tan^{-1}{4\over 3}>>>, <<<x1 = asin(2/5)-atan(4/3)>>>)@w{}
or m4_mathcomma(<<<x_1 = \tan^{-1}{3\over 4} + \cos^{-1}{2\over 5}>>>,
<<<x1 = atan(3/4)+acos(2/5)>>>) for @math{k} any integer.


@node c2trig, c2hyp, c2sin and c2cos, Functions and Variables for trigtools
@subsection c2trig

@deffn {Function} c2trig
The function c2trig (convert to trigonometric) reduce expression with hyperbolic functions
sinh, cosh, tanh, coth to trigonometric expression with sin, cos, tan, cot.

Examples:

1.

@example
(%i1) load(trigtools)$
(%i2) sinh(x)=c2trig(sinh(x));
cosh(x)=c2trig(cosh(x));
tanh(x)=c2trig(tanh(x));
coth(x)=c2trig(coth(x));
(%o2)                     sinh(x) = - %i sin(%i x)
(%o3)                        cosh(x) = cos(%i x)
(%o4)                     tanh(x) = - %i tan(%i x)
(%o5)                      coth(x) = %i cot(%i x)
@end example

2. see @url{http://www.math.utexas.edu/pipermail/maxima/2013/034585.html}
@example
(%i6) cos(p+q*%i);
(%o6)                           cos(%i q + p)
(%i7) trigexpand(%);
(%o7)                cos(p) cosh(q) - %i sin(p) sinh(q)
(%i8) c2trig(%);
(%o8)                           cos(%i q + p)
@end example

3.
@example
(%i9) sin(a+b*%i);
(%o9)                           sin(%i b + a)
(%i10) trigexpand(%);
(%o10)                %i cos(a) sinh(b) + sin(a) cosh(b)
(%i11) c2trig(%);
(%o11)                           sin(%i b + a)
@end example

4.
@example
(%i12) cos(a*%i+b*%i);
(%o12)                         cos(%i b + %i a)
(%i13) trigexpand(%);
(%o13)                 sinh(a) sinh(b) + cosh(a) cosh(b)
(%i14) c2trig(%);
(%o14)                         cos(%i b + %i a)
@end example

5.
@example
(%i15) tan(a+%i*b);
(%o15)                           tan(%i b + a)
(%i16) trigexpand(%);
                              %i tanh(b) + tan(a)
(%o16)                       ---------------------
                             1 - %i tan(a) tanh(b)
(%i17) c2trig(%);
(%o217)                           tan(%i b + a)
@end example

6.

@example
(%i18) cot(x+%i*y);
(%o18)                           cot(%i y + x)
(%i19) trigexpand(%);
                           (- %i cot(x) coth(y)) - 1
(%o19)                     -------------------------
                              cot(x) - %i coth(y)
(%i20) c2trig(%);
(%o20)                           cot(%i y + x)
@end example

@end deffn

@node c2hyp, trigfactor, c2trig, Functions and Variables for trigtools
@subsection c2hyp

@node trigfactor, trigsolve, c2hyp, Functions and Variables for trigtools
@subsection trigfactor

@node trigsolve, trigvalue, trigfactor, Functions and Variables for trigtools
@subsection trigsolve

@node trigvalue, trigeval, trigsolve, Functions and Variables for trigtools
@subsection trigvalue

@node trigeval, atan_contract, trigvalue, Functions and Variables for trigtools
@subsection trigeval

@node atan_contract,  , trigeval, Functions and Variables for trigtools
@subsection atan_contract

