@c -*- mode: texinfo -*-
@c translated Miller's postcript file to texi.m4 format by ehm

@menu
* Introduction to format::
* Functions and Variables for format::
@end menu

@node Introduction to format, Functions and Variables for format
@section Introduction to format

The @code{format} package was written by Bruce R. Miller, NIST (miller-at-cam.nist.gov).

@code{format} is a package for formatting algebraic expressions in
Maxima. It provides facilities for user-directed hierarchical
structuring of expressions, as well as for directing simplifications to
selected subexpressions. It emphasizes a semantic rather than
syntactic description of the desired form. The package also provides
utilities for obtaining efficiently the coefficients of polynomials,
trigonometric sums and power series.

In a general purpose Computer Algebra System (CAS), any particular
mathematical expression can take on a variety of forms: expanded form,
factored form or anything in between. Each form may have advantages; a
given form may be more compact than another, or allow clear expression
of certain algorithms. Or it may simply be more informative,
particularly if it has physical significance.  A CAS contains many tools
for transforming expressions. However, most are like Maxima's
@code{factor} and @code{expand}, operating only on the entire expression
or its top level. At the other extreme are operations like
@code{substpart} which extract a specific part of an expression, then
transform and replace it.  Unfortunately, the means of specifying the
piece of interest is purely syntactic, requiring the user to keep close
watch on the form of the arguments to avoid error.

The package described here gives users of Maxima more control over the
structure of expressions, and it does so using a more semantic, almost
algebraic, language describing the desired structure. It also provides a
semantic means of addressing parts of an expression for particular
simplifications. For example, to rearrange an expression into a series
in @code{eps} through order 5, whose terms will be polynomials in
@code{x} and @code{y}, whose coefficients, in turn, will be
trigonometric sums in @code{l} and @code{g} with factored coefficients
one uses the command:@*

@noindent
@code{format(foo; %series(eps; 5); %poly(x; y);%trig(l; g); %factor);}@*

@noindent
The principal tool, @code{format}, is described in section
@ref{Functions and Variables for format}. It uses procedures in
@code{coeflist} which obtain coefficients of polynomials, trigonometric
sums and power series.

@node Functions and Variables for format,  ,Introduction to format
@section Functions and Variables for format

@deffn {Function} format (@var{expr}, @var{template@sub{1}}, ...)
Each @code{template} indicates the desired form for an expression;
either the expected form or that into which it will be transformed. At
the same time, the indicated form implies a set of @emph{pieces}; the
next template in the chain applies to those pieces. For example,
@code{%poly(x)} specifies the transformation into a polynomial in
@code{x}, with the pieces being the coefficients. The passive
@code{%frac} treats the expression as a fraction; the pieces are the
numerator and denominator.  Whereas the next template formats all pieces
of the previous layer, positional @emph{subtemplates} may be used to
specify formats for each piece individually. This is most useful when
the pieces have unique roles and need to be treated differently, such as
a fraction's numerator and denominator.
@end deffn

@noindent
The full syntax of a template is@*

@code{keyword (parameter ; ...)[subtemplate ; ...]}@*

The recognized keywords are described below under @ref{Template keywords}. The parameters (if not
needed) and subtemplates (along with parentheses and brackets) are optional.

In addition to the keyword templates, arithmetic patterns are
recognized.  This is an expression involving addition, multiplication
and exponentiation containing a single instance of a keyword
template. In effect, the system `solves' the expression to be formatted
for the corresponding part, formats it accordingly and reinserts it. For
example, @code{format(X,a+%factor)} is equivalent to
@code{a+factor(X-a)}. Any other template is assumed to be a function to
be applied to the expression; the result is then formatted according to
the rest of the template chain.


Examples for general restructuring:
@c ===beg===
@c format((a+b*x)*(c-x)^2,%poly(x),factor);
@c format((1+2*eps*(q+r*cos(g))^2)^4,%series(eps,2),%trig(g),factor);
@c format((1+2*a+a^2)*b + a*(1+2*b+b^2),%sum,%product,%factor);
@c format(expand((a+x)^3-a^3),%f-a^3);
@c ===end===
@example
@group
(%i1) load("format.mac")$
(%i2) format((a+b*x)*(c-x)^2,%poly(x),factor);
                   3                2                        2
(%o2)           b x  - (2 b c - a) x  + c (b c - 2 a) x + a c
(%i3) format((1+2*eps*(q+r*cos(g))^2)^4,%series(eps,2),%trig(g),factor);
                   2      2                2
(%o3) 1 + eps (4 (r  + 2 q ) + 4 cos(2 g) r  + 16 cos(g) q r)
      2        4       2  2      4                4                  3
 + eps  (3 (3 r  + 24 q  r  + 8 q ) + 3 cos(4 g) r  + 24 cos(3 g) q r
                     2      2                 2   2      2
 + 24 cos(g) q r (3 r  + 4 q ) + 12 cos(2 g) r  (r  + 6 q )) + . . .
(%i4) format((1+2*a+a^2)*b + a*(1+2*b+b^2),%sum,%product,%factor);
                                     2          2
(%o4)                       a (b + 1)  + (a + 1)  b
@end group
@group
(%i5) format(expand((a+x)^3-a^3),%f-a^3);
                                        3    3
(%o5)                           (x + a)  - a
@end group
@end example

@subsection Template keywords
@anchor{Template keywords}

Keywords come in several @emph{classes}: Algebraic, Sums, Products,
Fractions, Complex, Bags, General, Targeting, Control, Subtemplate Aids,
and Convenience.

A few remarks about keywords: A passive keyword does not transform the
expression but treats it as a sum, fraction or whatever. The order of
the pieces corresponds to the internal ordering; subtemplate usage may
be awkward. See the documentation of @code{coerce_bag} for a description
of the coercions used. Targeting templates are basically shorthand
equivalents of structuring templates using subtemplates.

@deffn {keyword, algebraic} %poly (@var{x})
Polynomial in @var{x}. Abbrev: @code{%p}
@end deffn

@deffn {keyword, algebraic} %series (@var{e},@var{n})
Series in @var{e} through order @var{n}. Abbrev: @code{%s}
@end deffn

@deffn {keyword, algebraic} %Taylor (@var{e},@var{n})
Taylor in @var{e} through order @var{n}.
@end deffn

@deffn {keyword, algebraic} %monicpoly (@var{x1},...)
Monic polynomial in @var{xi}. Abbrev: @code{%mp}
@end deffn

@deffn {keyword, algebraic} %trig (@var{x1},...)
Trigonometric sum in @var{xi}. @code{sin} coefficients first, then
@code{cos}. Abbrev: @code{%t}
@end deffn

@deffn {keyword, algebraic} %coeff (@var{v},@var{n})
Coefficient of @var{v@sup{n}} and remainder.
@end deffn

@deffn {keyword, sums} %sum
Passive.
@end deffn

@deffn {keyword, sums} %partfrac (@var{x})
Partial fraction decomposition in @var{x}. Abbrev: @code{%pf}
@end deffn

@deffn {keyword, products} %product
Passive. Abbrev @code{%prod}
@end deffn

@deffn {keyword, products} %factor
Factored form. Abbrev @code{%f}
@end deffn

@deffn {keyword, products} %factor (minpoly)
Factored with element adjoined. Abbrev @code{%f}
@end deffn

@deffn {keyword, products} %sqfr
Square-free factored form. Abbrev @code{%sf}
@end deffn

@deffn {keyword, fractions} %frac
Passive.
@end deffn

@deffn {keyword, fractions} %ratsimp
Rationally simplified. Abbrev: @code{%r}
@end deffn

@deffn {keyword, complex} %rectform
Gaussian form (real and imaginary parts). Abbrev: @code{%g}
@end deffn

@deffn {keyword, complex} %polarform
Polar form (magnitude and phase).
@end deffn

@deffn {keyword, bags} %equation
Equation (lhs and rhs). Abbrev: @code{%eq}
@end deffn

@deffn {keyword, bags} %relation (@var{r})
Relation; @var{r} is one of @code{=,>,>=,<,<=,!=}. Abbrev: @code{%rel}
@end deffn

@deffn {keyword, bags} %list
List of elements.
@end deffn

@deffn {keyword, bags} %matrix
Matrix rows. Use @code{%list} to target elements.
@end deffn

@deffn {keyword, general} %expression
Passive. Orders the operands. Abbrev: @code{%expr}
@end deffn

@deffn {keyword, general} %performat (@var{T@sub{1}},...)
Format according to chain @var{T@sub{i}}. Orders the result, not the parts.
@end deffn

@deffn {keyword, targeting} %arg (@var{n})
Formats the @var{n}-th argument.
@end deffn

@deffn {keyword, targeting} %lhs (@var{eqn})
Formats the left hand side of an equation.
@end deffn

@deffn {keyword, targeting} %rhs (@var{eqn})
Formats the right hand side of an equation.
@end deffn

@deffn {keyword, targeting} %element (@var{i},...)
Formats an element of a matrix. Abbrev: @code{%el}
@end deffn

@deffn {keyword, targeting} %num
Formats the numerator of a fraction.
@end deffn

@deffn {keyword, targeting} %denom
Formats the denominator of a fraction.
@end deffn

@deffn {keyword, targeting} %match (@var{P})
Formats all subexpressions for which @var{P(expr)} returns @code{true}.
@end deffn

@deffn {keyword, control} %if (@var{P@sub{1}},...)[@var{T@sub{1}},...,@var{T@sub{n+1}}]
Find first @var{P@sub{i}(expr)} that evaluates to @code{true}, then format @var{expr}
using @var{T@sub{i}}, else @var{T@sub{n+1}}.
@end deffn

@deffn {keyword, subtemplate aids} %noop
Does nothing. Used to fill a subtemplate slot.
@end deffn

@deffn {keyword, subtemplate aids} [@var{T@sub{1}},@var{T@sub{2}},...]
Creates a template chain where an individual template was expected.
@end deffn

@deffn {keyword, subtemplate aids} %ditto (T)
Repeats the template so that it applies to following pieces.
@end deffn

@deffn {keyword, convenience} %subst (@var{eqns},...)
Substitutes @code{eqns} into expression and the result is formatted at the next layer.
@end deffn

@deffn {keyword, convenience} %ratsubst (@var{eqns},...)
@code{lratsubst}'s @code{eqns} into expression and the result is formatted at the next layer.
@end deffn

Example with simplification on subexpression:
@c ===beg===
@c foo:x^2*sin(y)^4-2*x^2*sin(y)^2+x^4*cos(y)^4-2*x^4*cos(y)^2+x^4+x^2+1$
@c trigsimp(foo);
@c format(foo,%p(x),trigsimp);
@c format([a=b,c=d,e=f],%equation);
@c format(%,%list);
@c m1:matrix([a^2+2*a+1=q,b^2+2*b+1=r], [c^2+2*c+1=s,d^2+2*d+1=t])$
@c format(m1,%equation,%matrix[%noop,%list[%noop,%factor]]);
@c ===end===
@example
@group
(%i1) foo:x^2*sin(y)^4-2*x^2*sin(y)^2+x^4*cos(y)^4-2*x^4*cos(y)^2+x^4+x^2+1$
(%i2) trigsimp(foo);
                     4    2     4         4    2       4
(%o2)              (x  + x ) cos (y) - 2 x  cos (y) + x  + 1
(%i3) format(foo,%p(x),trigsimp);
                                4    4       2    4
(%o3)                     x  sin (y) + x  cos (y) + 1
@end group
@end example
The following examples illustrate the usage with `bags.'
@example
@group
(%i1) format([a=b,c=d,e=f],%equation);
(%o1) [a, c, e] = [b, d, f]
(%i2) format(%,%list);
(%o2) [a = b, c = d, e = f]
(%i3) m1:matrix([a^2+2*a+1=q,b^2+2*b+1=r], [c^2+2*c+1=s,d^2+2*d+1=t])$
(%i4) format(m1,%equation,%matrix[%noop,%list[%noop,%factor]]);
                   [  2             2           ]
                   [ a  + 2 a + 1  b  + 2 b + 1 ]   [ q  r ]
(%o4)              [                            ] = [      ]
                   [  2                     2   ]   [ s  t ]
                   [ c  + 2 c + 1    (d + 1)    ]

@end group
@end example


@subsection User defined templates

New templates can be defined by giving the template keyword the property
@code{formatter}; the value should be a function (or lambda expression)
of the expression to be formatted and any parameters for the template.
For example, @code{%rectform} and @code{%if} could be defined as

@code{put(%rectform,lambda([c],block([r:rectformlist(c)],@*
        @ @ @ @ @ @ format-piece(r[1]) +%I* format-piece(r[2]))),formatter)}@*

@code{put(%if, lambda([x,test],@*
              @ @ @ @ @ @ if test(x) then format-piece(x,1)@*
              @ @ @ @ @ @ @ @ else@*
              @ @ @ @ @ @ @ @ @ @ format-piece(x,2)),formatter)}@*

@noindent
Functions used for defining templates are the following.

@deffn {function} %format_piece (@var{piece},{@var{nth}})
@code{lratsubst}'s @code{eqns} into expression and the result is formatted at the next layer.
Format a given piece of an expression, automatically accounting for
subtemplates and the remaining template chain. A specific subtemplate,
rather than the next one, can be selected by specifying @var{nth}.
@end deffn

@deffn {function} %coerce_bag (@var{op},@var{expr})
Attempts to coerce @var{expr} into an expression with @var{op} (one of
@code{=, #, <, <=, >, >=, [} or matrix) as the top-level operator. It
coerces the expression by swapping operands between layers but only if
adjacent layers are also lists, matrices or relations. This model
assumes that a list of equations, for example, can be viewed as an
equation whose sides are lists. Certain combinations, particularly those
involving inequalities may not be meaningful, however, so some caution
is advised.
@end deffn

@subsection Determining coefficients

We define the `algebras' of polynomials, trigonometric sums and power series to
be those expressions that can be cast into the following forms.

m4_displaymath(
<<<\eqalign{
{\bf P}(v_1,\cdots) &=  \{P\,|\, P=\sum_i c_i v^{p_{1,i}}_1 v^{p_{2,i}}_2 \cdots\} \cr
{\bf T}(v_1,\cdots) &=  \{T\,|\, T=\sum_i [ c_i \cos(m_{1,i} v_1+\cdots)+s_i \sin(m'_{1,i} v_1 +\cdots)] \} \cr
{\bf S}(v,O) &= \{S\,|\, S=\sum_i c_i v^{p_i};\,p_n \le O \} 
}>>>
)

The variables @var{v@sub{i}} may be any atomic expression in the sense
of @code{ratvars}. The shorthands @code{operator(op)} and
@code{match(predicate)} may be used to specify all subexpressions having
op as an operator, or that pass the predicate, respectively.

The coefficients @var{c@sub{i}} and @var{s@sub{i}} are general Maxima
expressions. In principle they would be independent of the variables
@var{v@sub{i}}, but in practice they may contain non-polynomial
dependence (or non-trigonometric, in the trigonometric case).  These
non-polynomial cases would include expressions like @code{(1 +
x)@sup{n}}, where @code{n} is symbolic. Likewise,
@code{(x@sup{a})@sup{b}} is, in general, multivalued; unless @code{a =
1} or @code{b} is a member of @code{Z}, or @code{radexpand=all}, it will
not be interpreted as @code{x@sup{ab}} is a member of
@strong{P}. Furthermore, we extend the algebras to include lists,
vectors, matrices and equations, by interpreting a list of polynomials,
say, as a polynomial with lists as coefficients.

The exponents @var{p@sub{i}} in series are restricted to numbers, but
the exponents @var{c@sub{j,i}} and multiples @var{m@sub{j,i}} for
polynomials and trigonometric sums may be general expressions
(excluding bags).

The following functions construct a list of the coefficients and `keys',
that is, the exponents or multiples. Note that these are sparse
representations; no coefficients are zero.@*

@noindent
@code{coeffs(P,v@sub{1},...) @arrow{} [[%poly,v@sub{1},...],[c@sub{1},p@sub{1,1},...],...] }@*
@code{trig_coeffs(T,v@sub{1},...) @arrow{}}@*
@ @ @ @ @ @ @ @ @ @ @ 
@code{[[%trig,v@sub{1},...],[[c@sub{1},m@sub{1,1},...],...],[[s@sub{1},m'@sub{1,1},...],...]]}@*
@code{series_coeffs(S,v,O) @arrow{} [[%series,v,O],[c@sub{1},p@sub{1}],...,[c@sub{n},p@sub{n}]}@*
@code{Taylor_coeffs(S,v,O) @arrow{} [[%Taylor,v,O],[c@sub{1},p@sub{1}],...,[c@sub{n},p@sub{n}]}@*

The latter two functions both expand an expression through order
@code{O}, but the series version only carries expands arithmetic
operations and is often considerably faster than @code{Taylor_coeffs}.

Examples:
@c ===beg===
@c cl1:coeffs((a+b*x)*(c-x)^2,x);
@c map('first,rest(coeffs((a+b*x)*(c-x)^2=q0+q1*x+q2*x^2+q3*x^3,x)));
@c trig_coeffs(2*(a+cos(x))*cos(x+3*y),x,y);
@c series_coeffs((a+b*x)*(c-x)^2,x,2);
@c coeffs((a+b*x)*sin(x),x);
@c coeffs((a+log(b)*x)*(c-log(x))^2,operator(log));
@c ===end===
@example
@group
(%i1) cl1:coeffs((a+b*x)*(c-x)^2,x);
                       2          2
(%o1) [[%poly, x], [a c , 0], [b c  - 2 a c, 1], [a - 2 b c, 2], [b, 3]]
(%i2) map('first,rest(coeffs((a+b*x)*(c-x)^2=q0+q1*x+q2*x^2+q3*x^3,x)));
                2          2
(%o2)       [a c  = q0, b c  - 2 a c = q1, a - 2 b c = q2, b = q3]
(%i3) trig_coeffs(2*(a+cos(x))*cos(x+3*y),x,y);
(%o3)      [[%trig, x, y], [], [[1, 0, 3], [2 a, 1, 3], [1, 2, 3]]]
(%i4) series_coeffs((a+b*x)*(c-x)^2,x,2);
                              2          2
(%o4)   [[%series, x, 2], [a c , 0], [b c  - 2 a c, 1], [a - 2 b c, 2]]
(%i5)  coeffs((a+b*x)*sin(x),x);
(%o5)             [[%poly, x], [a sin(x), 0], [b sin(x), 1]]
(%i6)  coeffs((a+log(b)*x)*(c-log(x))^2,operator(log));
                                    2           2
(%o6) [[%poly, log(x), log(b)], [a c , 0, 0], [c  x, 0, 1], [- 2 a c, 1, 0], 
                                         [- 2 c x, 1, 1], [a, 2, 0], [x, 2, 1]]
@end group
@end example


@subsection Related functions

@deffn {function} get_coef (@var{clist},@var{k@sub{1}},...)
Gets the coefficient from the coefficient list @var{clist} corresponding
to the keys @var{k@sub{i}}. The keys are matched to variable powers when
@var{clist} is a @code{%poly}, @code{%series} or @code{%Taylor} form. If
@var{clist} is a @code{%trig} then @var{k@sub{1}} should be @code{sin}
or @code{cos} and the remaining keys are matched to multipliers.
@end deffn

@deffn {function} uncoef (@var{clist})
Reconstructs the expression from a coefficient list @var{clist}. The
coefficient list can be any of the coefficient list forms.
@end deffn

@deffn {function} partition_poly (@var{expr},@var{test},@var{v@sub{1}},...)
Partitions @var{expr} into two polynomials; the first is made of those
monomials for which the function test returns true and the second is the
remainder. The test function is called on the powers of the @var{v@sub{i}}.
@end deffn

@deffn {function} partition_trig (@var{expr},@var{sintest},@var{costest},@var{v@sub{1}},...)
Trigonometric analog to partition poly; The functions @var{sintest} and
@var{costest} select sine and cosine terms, respectively; each are called on
the multipliers of the @var{v@sub{i}}.
@end deffn

@deffn {function} partition_series (@var{expr},@var{test},@var{v},@var{O})
@end deffn

@deffn {function} partition_Taylor (@var{expr},@var{test},@var{v},@var{O})
Analog to @code{partition_poly} for series.
@end deffn

@noindent
Example:
@c ===beg===
@c partition_poly((a+b*x)*(c-x)^2,'evenp,x);
@c ===end===
@example
@group
(%i1)  partition_poly((a+b*x)*(c-x)^2,'evenp,x);
                             2      2     3       2
(%o1)          [(a - 2 b c) x  + a c , b x  + (b c  - 2 a c) x]
@end group
@end example

@*

@noindent
@strong{Support functions}

@deffn {function} matching_parts (@var{expr},@var{predicate},@var{args}...)
Returns a list of all subexpressions of @var{expr} for which the application
@code{predicate(piece,args ... )} returns @code{true}.
@end deffn

@deffn {function} function_calls (@var{expr},@var{functions}...)
Returns a list of all calls in @var{expr} involving any of @var{functions}.
@end deffn

@deffn {function} function_arguments (@var{expr},@var{functions}...)
Returns a list of all argument lists for calls to @var{functions} in @var{expr}.
@end deffn

@noindent
Examples:
@c ===beg===
@c t2:(a+log(b)*x)*(c-log(x))^2$
@c matching_parts(t2,constantp);
@c function_calls(t2,log);
@c ===end===
@example
@group
(%i1) t2:(a+log(b)*x)*(c-log(x))^2$
(%i2) matching_parts(t2,constantp);
(%o2)                             [2, - 1]
(%i3) function_calls(t2,log);
(%o3)                         [log(x), log(b)]
@end group
@end example

@subsection Implementation

Original documentation is located in the share/contrib/format directory and contains an
appendix describing the implementation algorithm in more detail.

