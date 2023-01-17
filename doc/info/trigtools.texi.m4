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

@deffn {Function} c2trig (@var{x})
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

@deffn {Function} c2hyp (@var{x})
The function c2hyp (convert to hyperbolic) convert expression with exp function
to expression with hyperbolic functions sinh, cosh.

Examples:

@example
(%i6) c2hyp(exp(x));
(%o6)                         sinh(x) + cosh(x)
(%i7) c2hyp(exp(x)+exp(x^2)+1);
                       2          2
(%o7)            sinh(x ) + cosh(x ) + sinh(x) + cosh(x) + 1
(%i8) c2hyp(exp(x)/(2*exp(y)-3*exp(z)));
                              sinh(x) + cosh(x)
(%o8)           ---------------------------------------------
                2 (sinh(y) + cosh(y)) - 3 (sinh(z) + cosh(z))
@end example

@end deffn

@node trigfactor, trigsolve, c2hyp, Functions and Variables for trigtools
@subsection trigfactor

@deffn {Function} trigfactor (@var{x})
The function trigfactor factors expresions of
form m4_mathdot(<<<\pm \sin x \pm \cos y>>>, <<<+-sin(x)+-cos(y)>>>)

Examples:

1.
@example
(%i2) trigfactor(sin(x)+cos(x));
                                            %pi
(%o2)                       sqrt(2) cos(x - ---)
                                             4
(%i3) trigrat(%);
(%o3)                          sin(x) + cos(x)
@end example

2.
@example
(%i4) trigfactor(sin(x)+cos(y));
                           y   x   %pi      y   x   %pi
(%o4)                2 cos(- - - + ---) cos(- + - - ---)
                           2   2    4       2   2    4
(%i5) trigrat(%);
(%o5)                          cos(y) + sin(x)
@end example

3.
@example
(%i6) trigfactor(sin(x)-cos(3*y));
                         3 y   x   %pi      3 y   x   %pi
(%o6)              2 sin(--- - - + ---) sin(--- + - - ---)
                          2    2    4        2    2    4
(%i7) trigrat(%);
(%o7)                         sin(x) - cos(3 y)
@end example

4.
@example
(%i8) trigfactor(-sin(5*x)-cos(3*y));
                        3 y   5 x   %pi      3 y   5 x   %pi
(%o8)           - 2 cos(--- - --- + ---) cos(--- + --- - ---)
                         2     2     4        2     2     4
(%i9) trigrat(%);
(%o9)                      (- cos(3 y)) - sin(5 x)
@end example

5.
@example 
(%i10) sin(alpha)+sin(beta)=trigfactor(sin(alpha)+sin(beta));
                                       beta   alpha      beta   alpha
(%o10)  sin(beta) + sin(alpha) = 2 cos(---- - -----) sin(---- + -----)
                                        2       2         2       2
(%i11) trigrat(%);
(%o78)          sin(beta) + sin(alpha) = sin(beta) + sin(alpha)
@end example

6.
@example
(%i12) sin(alpha)-sin(beta)=trigfactor(sin(alpha)-sin(beta));
                                        beta   alpha      beta   alpha
(%o12) sin(alpha) - sin(beta) = - 2 sin(---- - -----) cos(---- + -----)
                                         2       2         2       2
@end example

7.
@example
(%i13) cos(alpha)+cos(beta)=trigfactor(cos(alpha)+cos(beta));
                                       beta   alpha      beta   alpha
(%o80)  cos(beta) + cos(alpha) = 2 cos(---- - -----) cos(---- + -----)
                                        2       2         2       2
@end example

8.
@example
(%i14) cos(alpha)-cos(beta)=trigfactor(cos(alpha)-cos(beta));
                                       beta   alpha      beta   alpha
(%o14)  cos(alpha) - cos(beta) = 2 sin(---- - -----) sin(---- + -----)
                                        2       2         2       2
@end example

9.
@example
(%i15) trigfactor(3*sin(x)+7*cos(x));
(%o15)                        3 sin(x) + 7 cos(x)
(%i16) c2sin(%);
                                                 7
(%o16)                     sqrt(58) sin(x + atan(-))
                                                 3
(%i17) trigexpand(%),expand;
(%o17)                        3 sin(x) + 7 cos(x)
@end example

10.
@example
(%i18) trigfactor(sin(2*x));
(%o18)                             sin(2 x)
(%i19) trigexpand(%);
(%o19)                          2 cos(x) sin(x)
@end example

@end deffn

@node trigsolve, trigvalue, trigfactor, Functions and Variables for trigtools
@subsection trigsolve

@deffn {Function} trigsolve (@var{x})
The function trigsolve find solutions of trigonometric equation from
interval m4_math(<<<[a,b)>>>, <<<[a, b)>>>).

Examples:
1.
@example
(%i38) eq:eq:3*sin(x)+4*cos(x)=2;
(%o38)                      3 sin(x) + 4 cos(x) = 2

(%i39) wxplot2d([3*sin(x)+4*cos(x),2],[x,-%pi,%pi]);
(%t39)
(%o39)
(%i40) sol:trigsolve(eq,-%pi,%pi);
                  2 sqrt(21)   12              2 sqrt(21)   12
(%o40)      @{atan(---------- - --), %pi - atan(---------- + --)@}
                      5        5                   5        5
(%i41) float(%), numer;
(%o41)            @{- 0.5157783719341241, 1.802780589520693@}
@end example

Answ. : m4_math(<<<x = \tan^{-1}\left({2\sqrt{21}\over 5} - {12\over
5}\right) + 2\pi k>>>, <<<x = atan((2*sqrt(21))/5-12/5)+2*%pi*k>>>);@w{}
m4_mathcomma(<<<x = \pi - \tan^{-1}\left({2\sqrt{21}\over 5} + {12\over 5}\right) + 2\pi k>>>, <<<x=%pi-atan((2*sqrt(21))/5+12/5)+2*%pi*k>>>) @math{k} -- any integer.

2.
@example
(%i6) eq:cos(3*x)-sin(x)=sqrt(3)*(cos(x)-sin(3*x));
(%o6)         cos(3 x) - sin(x) = sqrt(3) (cos(x) - sin(3 x))
@end example

We have 6 solutions from [0, 2*pi].
@example
(%i10) trigfactor(lhs(eq))=map(trigfactor,rhs(eq));
                   %pi            %pi                      %pi            %pi
(%o15) - 2 sin(x + ---) sin(2 x - ---) = 2 sqrt(3) sin(x - ---) sin(2 x - ---)
                    4              4                        4              4
(%i11) factor(lhs(%)-rhs(%));
                 4 x + %pi                4 x - %pi       8 x - %pi
(%o11)  - 2 (sin(---------) + sqrt(3) sin(---------)) sin(---------)
                     4                        4               4
@end example

Equation is equivalent to
@example
(%i12) L:factor(rhs(%)-lhs(%));
                4 x + %pi                4 x - %pi       8 x - %pi
(%o12)   2 (sin(---------) + sqrt(3) sin(---------)) sin(---------)
                    4                        4               4
(%i13) eq1:part(L,2)=0;
                     4 x + %pi                4 x - %pi
(%o13)           sin(---------) + sqrt(3) sin(---------) = 0
                         4                        4
(%i14) eq2:part(L,3)=0;
                                 8 x - %pi
(%o14)                       sin(---------) = 0
                                     4
(%i15) S1:trigsolve(eq1,0,2*%pi);
                                 %pi  13 %pi
(%o15)                         @{---, ------@}
                                 12     12
(%i16) S2:trigsolve(eq2,0,2*%pi);
                           %pi  5 %pi  9 %pi  13 %pi
(%o16)                   @{---, -----, -----, ------@}
                            8     8      8      8
(%i17) S:listify(union(S1,S2));
                   %pi  %pi  5 %pi  13 %pi  9 %pi  13 %pi
(%o17)            [---, ---, -----, ------, -----, ------]
                   12    8     8      12      8      8
(%i18) float(%), numer;
(%o18) [0.2617993877991494, 0.3926990816987241, 1.963495408493621, 
                      3.403392041388942, 3.534291735288517, 5.105088062083414]
@end example

Answer:
m4_mathcomma(<<<x = a + 2\pi k>>>, <<<x = a+2*%pi*k>>>) where @math{a} any from @math{S}, @math{k} any integer.

3.
@example
(%i19) eq:8*cos(x)*cos(4*x)*cos(5*x)-1=0;
(%o19)               8 cos(x) cos(4 x) cos(5 x) - 1 = 0

(%i20) trigrat(%);
(%o20)          2 cos(10 x) + 2 cos(8 x) + 2 cos(2 x) + 1 = 0
@end example

Left side is periodic with period m4_math(<<<T=\pi>>>, <<<T=%pi>>>).

We have 10 solutions from [0, pi].
@example
(%i22) x4:find_root(eq, x, 1.3, 1.32);
(%o22)                        1.308996938995747
(%i23) x5:find_root(eq, x, 1.32, 1.35);
(%o23)                        1.346396851538483
@end example

Equation we multiply by m4_math(<<<2\sin x\cos 2x>>>, <<<2*sin(x)*cos(2*x)>>>):

@example
(%i25) eq*2*sin(x)*cos(2*x);
(%o25)     2 sin(x) cos(2 x) (8 cos(x) cos(4 x) cos(5 x) - 1) = 0
(%i26) eq1:trigreduce(%),expand;
(%o26)                     sin(13 x) + sin(x) = 0
(%i27) trigfactor(lhs(eq1))=0;
(%o27)                     2 cos(6 x) sin(7 x) = 0
(%i28) S1:trigsolve(cos(6*x),0,%pi);
                    %pi  %pi  5 %pi  7 %pi  3 %pi  11 %pi
(%o28)             @{---, ---, -----, -----, -----, ------@}
                    12    4    12     12      4      12
(%i29) S2:trigsolve(sin(7*x),0,%pi);
                     %pi  2 %pi  3 %pi  4 %pi  5 %pi  6 %pi
(%o29)           @{0, ---, -----, -----, -----, -----, -----@}
                      7     7      7      7      7      7
@end example

We remove solutions of m4_math(<<<\sin x = 0>>>, <<<sin(x)=0>>>) and@w{}
m4_mathdot(<<<\cos 2x = 0>>>, <<<cos(2*x) = 0>>>)

@example
(%i30) S3:trigsolve(sin(x),0,%pi);
(%o30)                               @{0@}
(%i31) S4:trigsolve(cos(2*x),0,%pi);
                                 %pi  3 %pi
(%o31)                          @{---, -----@}
                                  4     4
@end example

We find 10 solutions from m4_math(<<<[0, \pi]>>>,<<<[0, %pi]>>>):
@example
(%i32) union(S1,S2)$ setdifference(%,S3)$ setdifference(%,S4);
         %pi  %pi  2 %pi  5 %pi  3 %pi  4 %pi  7 %pi  5 %pi  6 %pi  11 %pi
(%o34) @{---, ---, -----, -----, -----, -----, -----, -----, -----, ------@}
         12    7     7     12      7      7     12      7      7      12
(%i35) S:listify(%);
        %pi  %pi  2 %pi  5 %pi  3 %pi  4 %pi  7 %pi  5 %pi  6 %pi  11 %pi
(%o35) [---, ---, -----, -----, -----, -----, -----, -----, -----, ------]
        12    7     7     12      7      7     12      7      7      12
(%i36) length(S);
(%o36)                               10
(%i37) float(S), numer;
(%o37) [0.2617993877991494, 0.4487989505128276, 0.8975979010256552, 
1.308996938995747, 1.346396851538483, 1.79519580205131, 1.832595714594046, 
2.243994752564138, 2.692793703076966, 2.879793265790644]
@end example

Answer:
m4_math(<<<x = a + 2\pi k>>>, <<<x = a+2*%pi*k>>>), where @math{a} any from @math{S}, @math{k} any integer.

@end deffn
@node trigvalue, trigeval, trigsolve, Functions and Variables for trigtools
@subsection trigvalue

@node trigeval, atan_contract, trigvalue, Functions and Variables for trigtools
@subsection trigeval

@node atan_contract,  , trigeval, Functions and Variables for trigtools
@subsection atan_contract

