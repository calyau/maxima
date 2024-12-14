@c -*- mode: texinfo -*-
@menu
* Introduction to Matrices and Linear Algebra::  
* Functions and Variables for Matrices and Linear Algebra::  
@end menu

@c -----------------------------------------------------------------------------
@node Introduction to Matrices and Linear Algebra, Functions and Variables for Matrices and Linear Algebra, Matrices and Linear Algebra, Matrices and Linear Algebra
@section Introduction to Matrices and Linear Algebra
@c -----------------------------------------------------------------------------

@menu
* Dot::                         
* Matrices::                         
* Vectors::                     
* eigen::
@end menu

@c -----------------------------------------------------------------------------
@node Dot, Matrices, Introduction to Matrices and Linear Algebra, Introduction to Matrices and Linear Algebra
@subsection Dot
@c -----------------------------------------------------------------------------

The operator @code{.} represents noncommutative multiplication and scalar
product.  When the operands are 1-column or 1-row matrices @code{a} and
@code{b}, the expression @code{a.b} is equivalent to
@code{sum (a[i]*b[i], i, 1, length(a))}.  If @code{a} and @code{b} are not
complex, this is the scalar product, also called the inner product or dot
product, of @code{a} and @code{b}.  The scalar product is defined as
@code{conjugate(a).b} when @code{a} and @code{b} are complex;
@mref{innerproduct} in the @code{eigen} package provides the complex scalar
product.

When the operands are more general matrices,
the product is the matrix product @code{a} and @code{b}.
The number of rows of @code{b} must equal the number of columns of @code{a},
and the result has number of rows equal to the number of rows of @code{a}
and number of columns equal to the number of columns of @code{b}.

To distinguish @code{.} as an arithmetic operator from the decimal point in a
floating point number, it may be necessary to leave spaces on either side.
For example, @code{5.e3} is @code{5000.0} but @code{5 . e3} is @code{5}
times @code{e3}.

There are several flags which govern the simplification of expressions
involving @code{.}, namely @mrefcomma{dot0nscsimp} @mrefcomma{dot0simp}@w{}
@mrefcomma{dot1simp} @mrefcomma{dotassoc} @mrefcomma{dotconstrules}@w{}
@mrefcomma{dotdistrib} @mrefcomma{dotexptsimp} @mrefcomma{dotident} and
@mrefdot{dotscrules}

@c -----------------------------------------------------------------------------
@node Matrices, Vectors, Dot, Introduction to Matrices and Linear Algebra
@subsection Matrices
@c -----------------------------------------------------------------------------
Matrices are handled with speed and memory-efficiency in mind. This means that
assigning a matrix to a variable will create a reference to, not a copy of the
matrix. If the matrix is modified all references to the matrix point to the
modified object (See @mref{copymatrix} for a way of avoiding this):
@c ===beg===
@c M1: matrix([0,0],[0,0]);
@c M2: M1;
@c M1[1][1]: 2;
@c M2;
@c ===end===
@example
@group
(%i1) M1: matrix([0,0],[0,0]);
                            [ 0  0 ]
(%o1)                       [      ]
                            [ 0  0 ]
@end group
@group
(%i2) M2: M1;
                            [ 0  0 ]
(%o2)                       [      ]
                            [ 0  0 ]
@end group
@group
(%i3) M1[1][1]: 2;
(%o3)                           2
@end group
@group
(%i4) M2;
                            [ 2  0 ]
(%o4)                       [      ]
                            [ 0  0 ]
@end group
@end example

Converting a matrix to nested lists and vice versa works the following way:
@c ===beg===
@c l: [[1,2],[3,4]];
@c M1: apply('matrix,l);
@c M2: transpose(M1);
@c args(M2);
@c ===end===
@example
@group
(%i1) l: [[1,2],[3,4]];
(%o1)                   [[1, 2], [3, 4]]
@end group
@group
(%i2) M1: apply('matrix,l);
                            [ 1  2 ]
(%o2)                       [      ]
                            [ 3  4 ]
@end group
@group
(%i3) M2: transpose(M1);
                            [ 1  3 ]
(%o3)                       [      ]
                            [ 2  4 ]
@end group
@group
(%i4) args(M2);
(%o4)                   [[1, 3], [2, 4]]
@end group
@end example
@c -----------------------------------------------------------------------------
@node Vectors, eigen, Matrices, Introduction to Matrices and Linear Algebra
@subsection Vectors
@c -----------------------------------------------------------------------------

@code{vect} is a package of functions for vector analysis.  @code{load ("vect")}
loads this package, and @code{demo ("vect")} displays a demonstration.
@c find maxima -name \*orth\* YIELDS NOTHING; ARE THESE FUNCTIONS IN ANOTHER FILE NOW ??
@c and SHARE;VECT ORTH contains definitions of various orthogonal curvilinear coordinate systems.

The vector analysis package can combine and simplify symbolic 
expressions including dot products and cross products, together with
the gradient, divergence, curl, and Laplacian operators.  The
distribution of these operators over sums or products is governed
by several flags, as are various other expansions, including expansion
into components in any specific orthogonal coordinate systems.
There are also functions for deriving the scalar or vector potential
of a field.

The @code{vect} package contains these functions:
@mrefcomma{vectorsimp} @mrefcomma{scalefactors} @mrefcomma{express}@w{}
@mrefcomma{potential} and @mrefdot{vectorpotential}
@c REVIEW vect.usg TO ENSURE THAT TEXINFO HAS WHATEVER IS THERE
@c PRINTFILE(VECT,USAGE,SHARE); for details.

By default the @code{vect} package does not declare the dot operator to be a
commutative operator.  To get a commutative dot operator @code{.}, the command
@code{declare(".", commutative)} must be executed.

@opencatbox{Categories:}
@category{Vectors}
@category{Share packages}
@category{Package vect}
@closecatbox

@c -----------------------------------------------------------------------------
@node eigen, , Vectors, Introduction to Matrices and Linear Algebra
@subsection eigen
@c -----------------------------------------------------------------------------

The package @code{eigen} contains several functions devoted to the
symbolic computation of eigenvalues and eigenvectors.
Maxima loads the package automatically if one of the functions
@code{eigenvalues} or @code{eigenvectors} is invoked.
The package may be loaded explicitly as @code{load ("eigen")}.

@code{demo ("eigen")} displays a demonstration of the capabilities
of this package.
@code{batch ("eigen")} executes the same demonstration,
but without the user prompt between successive computations.

The functions in the @code{eigen} package are:@*
@mrefcomma{innerproduct} @mrefcomma{unitvector} @mrefcomma{columnvector}@w{}
@mrefcomma{gramschmidt} @mrefcomma{eigenvalues}@*
@mrefcomma{eigenvectors} @mrefcomma{uniteigenvectors} and
@mrefdot{similaritytransform}

@opencatbox{Categories:}
@category{Vectors}
@category{Matrices}
@category{Share packages}
@category{Package eigen}
@closecatbox

@c end concepts Matrices and Linear Algebra

@c -----------------------------------------------------------------------------
@node Functions and Variables for Matrices and Linear Algebra,  , Introduction to Matrices and Linear Algebra, Matrices and Linear Algebra
@section Functions and Variables for Matrices and Linear Algebra
@c -----------------------------------------------------------------------------

@c -----------------------------------------------------------------------------
@anchor{addcol}
@deffn {Function} addcol (@var{M}, @var{list_1}, @dots{}, @var{list_n})

Appends the column(s) given by the one
or more lists (or matrices) onto the matrix @var{M}.

See also @mref{addrow} and @mrefdot{append}

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{addrow}
@deffn {Function} addrow (@var{M}, @var{list_1}, @dots{}, @var{list_n})

Appends the row(s) given by the one or
more lists (or matrices) onto the matrix @var{M}.

See also @mref{addcol} and @mrefdot{append}

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{adjoint}
@deffn {Function} adjoint (@var{M})

Returns the adjoint of the matrix @var{M}.
The adjoint matrix is the transpose of the matrix of cofactors of @var{M}.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{augcoefmatrix}
@deffn {Function} augcoefmatrix ([@var{eqn_1}, @dots{}, @var{eqn_m}], [@var{x_1}, @dots{}, @var{x_n}])

Returns the augmented coefficient
matrix for the variables @var{x_1}, @dots{}, @var{x_n} of the system of linear
equations @var{eqn_1}, @dots{}, @var{eqn_m}.  This is the coefficient matrix
with a column adjoined for the constant terms in each equation (i.e., those
terms not dependent upon @var{x_1}, @dots{}, @var{x_n}).

@example
(%i1) m: [2*x - (a - 1)*y = 5*b, c + b*y + a*x = 0]$
(%i2) augcoefmatrix (m, [x, y]);
                       [ 2  1 - a  - 5 b ]
(%o2)                  [                 ]
                       [ a    b      c   ]
@end example

@opencatbox{Categories:}
@category{Linear equations}
@category{Matrices}
@closecatbox
@end deffn

@c --- 04.10.2010 --------------------------------------------------------------
@anchor{cauchy_matrix}
@deffn {Function} cauchy_matrix @
@fname{cauchy_matrix} ([@var{x_1}, @var{x_2}, @dots{}, @var{x_m}], [@var{y_1}, @var{y_2}, @dots{}, @var{y_n}]) @
@fname{cauchy_matrix} ([@var{x_1}, @var{x_2}, @dots{}, @var{x_n}])

Returns a @code{n} by @var{m} Cauchy matrix with the elements @var{a[i,j]} 
= 1/(@var{x_i}+@var{y_i}).  The second argument of @code{cauchy_matrix} is 
optional.  For this case the elements of the Cauchy matrix are  
@var{a[i,j]} = 1/(@var{x_i}+@var{x_j}).

Remark: In the literature the Cauchy matrix can be found defined in two forms.
A second definition is @var{a[i,j]} = 1/(@var{x_i}-@var{y_i}).

Examples:

@c ===beg===
@c cauchy_matrix([x1, x2], [y1, y2]);
@c cauchy_matrix([x1, x2]);
@c ===end===
@example
(%i1) cauchy_matrix([x1, x2], [y1, y2]);
@group
                      [    1        1    ]
                      [ -------  ------- ]
                      [ y1 + x1  y2 + x1 ]
(%o1)                 [                  ]
                      [    1        1    ]
                      [ -------  ------- ]
                      [ y1 + x2  y2 + x2 ]
@end group

(%i2) cauchy_matrix([x1, x2]);
                      [   1         1    ]
                      [  ----    ------- ]
                      [  2 x1    x2 + x1 ]
(%o2)                 [                  ]
                      [    1       1     ]
                      [ -------   ----   ]
                      [ x2 + x1   2 x2   ]
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{charpoly}
@deffn {Function} charpoly (@var{M}, @var{x})

Returns the characteristic polynomial for the matrix @var{M}
with respect to variable @var{x}.  That is,
@code{determinant (@var{M} - diagmatrix (length (@var{M}), @var{x}))}.

@example
(%i1) a: matrix ([3, 1], [2, 4]);
                            [ 3  1 ]
(%o1)                       [      ]
                            [ 2  4 ]
(%i2) expand (charpoly (a, lambda));
                           2
(%o2)                lambda  - 7 lambda + 10
(%i3) (programmode: true, solve (%));
(%o3)               [lambda = 5, lambda = 2]
(%i4) matrix ([x1], [x2]);
                             [ x1 ]
(%o4)                        [    ]
                             [ x2 ]
(%i5) ev (a . % - lambda*%, %th(2)[1]);
                          [ x2 - 2 x1 ]
(%o5)                     [           ]
                          [ 2 x1 - x2 ]
(%i6) %[1, 1] = 0;
(%o6)                     x2 - 2 x1 = 0
(%i7) x2^2 + x1^2 = 1;
                            2     2
(%o7)                     x2  + x1  = 1
(%i8) solve ([%th(2), %], [x1, x2]);
@group
                  1               2
(%o8) [[x1 = - -------, x2 = - -------], 
               sqrt(5)         sqrt(5)

                                             1             2
                                    [x1 = -------, x2 = -------]]
                                          sqrt(5)       sqrt(5)
@end group
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{coefmatrix}
@deffn {Function} coefmatrix ([@var{eqn_1}, @dots{}, @var{eqn_m}], [@var{x_1}, @dots{}, @var{x_n}])

Returns the coefficient matrix for the
variables @var{x_1}, @dots{}, @var{x_n} of the system of linear equations
@var{eqn_1}, @dots{}, @var{eqn_m}.

@example
(%i1) coefmatrix([2*x-(a-1)*y+5*b = 0, b*y+a*x = 3], [x,y]);
                                 [ 2  1 - a ]
(%o1)                            [          ]
                                 [ a    b   ]
@end example

@opencatbox{Categories:}
@category{Linear equations}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{col}
@deffn {Function} col (@var{M}, @var{i})

Returns the @var{i}'th column of the matrix @var{M}.
The return value is a matrix.

The matrix returned by @code{col} does not share memory with the argument @var{M};
a modification to the return value does not modify @var{M}.

Examples:

@code{col} returns the @var{i}'th column of the matrix @var{M}.

@c ===beg===
@c abc: matrix ([12, 14, -4], [2, x, b], [3*y, -7, 9]);
@c col (abc, 1);
@c col (abc, 2);
@c col (abc, 3);
@c ===end===
@example
@group
(%i1) abc: matrix ([12, 14, -4], [2, x, b], [3*y, -7, 9]);
                        [ 12   14   - 4 ]
                        [               ]
(%o1)                   [  2    x    b  ]
                        [               ]
                        [ 3 y  - 7   9  ]
@end group
@group
(%i2) col (abc, 1);
                             [ 12  ]
                             [     ]
(%o2)                        [  2  ]
                             [     ]
                             [ 3 y ]
@end group
@group
(%i3) col (abc, 2);
                             [ 14  ]
                             [     ]
(%o3)                        [  x  ]
                             [     ]
                             [ - 7 ]
@end group
@group
(%i4) col (abc, 3);
                             [ - 4 ]
                             [     ]
(%o4)                        [  b  ]
                             [     ]
                             [  9  ]
@end group
@end example

The matrix returned by @code{col} does not share memory with the argument.
In this example,
assigning a new value to @code{aa2} does not modify @code{aa}.

@c ===beg===
@c aa: matrix ([1, 2, x], [7, y, 3]);
@c aa2: col (aa, 2);
@c aa2[2, 1]: 123;
@c aa2;
@c aa;
@c ===end===
@example
@group
(%i1) aa: matrix ([1, 2, x], [7, y, 3]);
                           [ 1  2  x ]
(%o1)                      [         ]
                           [ 7  y  3 ]
@end group
@group
(%i2) aa2: col (aa, 2);
                              [ 2 ]
(%o2)                         [   ]
                              [ y ]
@end group
@group
(%i3) aa2[2, 1]: 123;
(%o3)                          123
@end group
@group
(%i4) aa2;
                             [  2  ]
(%o4)                        [     ]
                             [ 123 ]
@end group
@group
(%i5) aa;
                           [ 1  2  x ]
(%o5)                      [         ]
                           [ 7  y  3 ]
@end group
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{columnvector}
@anchor{covect}
@deffn  {Function} columnvector (@var{L})
@deffnx {Function} covect (@var{L})

Returns a matrix of one column and @code{length (@var{L})} rows,
containing the elements of the list @var{L}.

@code{covect} is a synonym for @code{columnvector}.

@code{load ("eigen")} loads this function.

@c FOLLOWING COMMENT PRESERVED. WHAT DOES THIS MEAN ??
This is useful if you want to use parts of the outputs of
the functions in this package in matrix calculations.

Example:

@c HMM, SPURIOUS "redefining the Macsyma function".
@c LEAVE IT HERE SINCE THAT'S WHAT A USER ACTUALLY SEES.
@example
(%i1) load ("eigen")$
Warning - you are redefining the Macsyma function eigenvalues
Warning - you are redefining the Macsyma function eigenvectors
(%i2) columnvector ([aa, bb, cc, dd]);
                             [ aa ]
                             [    ]
                             [ bb ]
(%o2)                        [    ]
                             [ cc ]
                             [    ]
                             [ dd ]
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{copymatrix}
@deffn {Function} copymatrix (@var{M})

Returns a copy of the matrix @var{M}.  This is the only way
to make a copy aside from copying @var{M} element by element.

Note that an assignment of one matrix to another, as in @code{m2: m1}, does not
copy @code{m1}.  An assignment @code{m2 [i,j]: x} or @code{setelmx(x, i, j, m2)}
also modifies @code{m1 [i,j]}.  Creating a copy with @code{copymatrix} and then
using assignment creates a separate, modified copy.

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{determinant}
@deffn {Function} determinant (@var{M})

Computes the determinant of @var{M} by a method similar to
Gaussian elimination.

@c JUST HOW DOES ratmx AFFECT THE RESULT ??
The form of the result depends upon the setting of the switch @mrefdot{ratmx}

@c IS A SPARSE DETERMINANT SOMETHING OTHER THAN THE DETERMINANT OF A SPARSE MATRIX ??
There is a special routine for computing sparse determinants which is called
when the switches @code{ratmx} and @mref{sparse} are both @code{true}.

@c EXAMPLES NEEDED HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{detout}
@defvr {Option variable} detout
Default value: @code{false}

When @code{detout} is @code{true}, the determinant of a
matrix whose inverse is computed is factored out of the inverse.

For this switch to have an effect @mref{doallmxops} and @mref{doscmxops} should
be @code{false} (see their descriptions).  Alternatively this switch can be
given to @mref{ev} which causes the other two to be set correctly.

Example:

@example
(%i1) m: matrix ([a, b], [c, d]);
                            [ a  b ]
(%o1)                       [      ]
                            [ c  d ]
(%i2) detout: true$
(%i3) doallmxops: false$
(%i4) doscmxops: false$
(%i5) invert (m);
                          [  d   - b ]
                          [          ]
                          [ - c   a  ]
(%o5)                     ------------
                           a d - b c
@end example
@c THERE'S MORE TO THIS STORY: detout: false$ invert (m); RETURNS THE SAME THING.
@c IT APPEARS THAT doallmxops IS CRUCIAL HERE.

@opencatbox{Categories:}
@category{Matrices}
@category{Evaluation flags}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{diagmatrix}
@deffn {Function} diagmatrix (@var{n}, @var{x})

Returns a diagonal matrix of size @var{n} by @var{n} with the diagonal elements
all equal to @var{x}.  @code{diagmatrix (@var{n}, 1)} returns an identity matrix
(same as @code{ident (@var{n})}).

@var{n} must evaluate to an integer, otherwise @code{diagmatrix} complains with
an error message.

@var{x} can be any kind of expression, including another matrix.  If @var{x} is
a matrix, it is not copied; all diagonal elements refer to the same instance,
@var{x}.

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{doallmxops}
@defvr {Option variable} doallmxops
Default value: @code{true}

When @code{doallmxops} is @code{true},
@c UMM, WHAT DOES THIS MEAN EXACTLY ??
all operations relating to matrices are carried out.
When it is @code{false} then the setting of the
individual @code{dot} switches govern which operations are performed.

@c NEED EXAMPLES HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{domxexpt}
@defvr {Option variable} domxexpt
Default value: @code{true}

When @code{domxexpt} is @code{true},
a matrix exponential, @code{exp (@var{M})} where @var{M} is a matrix, is
interpreted as a matrix with element @code{[i,j]} equal to @code{exp (m[i,j])}.
Otherwise @code{exp (@var{M})} evaluates to @code{exp (@var{ev(M)})}.

@code{domxexpt} affects all expressions of the form
@code{@var{base}^@var{power}} where @var{base} is an expression assumed scalar
or constant, and @var{power} is a list or matrix.

Example:

@example
(%i1) m: matrix ([1, %i], [a+b, %pi]);
                         [   1    %i  ]
(%o1)                    [            ]
                         [ b + a  %pi ]
(%i2) domxexpt: false$
(%i3) (1 - c)^m;
                             [   1    %i  ]
                             [            ]
                             [ b + a  %pi ]
(%o3)                 (1 - c)
(%i4) domxexpt: true$
(%i5) (1 - c)^m;
                  [                      %i  ]
                  [    1 - c      (1 - c)    ]
(%o5)             [                          ]
                  [        b + a         %pi ]
                  [ (1 - c)       (1 - c)    ]
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{domxmxops}
@defvr {Option variable} domxmxops
Default value: @code{true}

When @code{domxmxops} is @code{true}, all matrix-matrix or
matrix-list operations are carried out (but not scalar-matrix
operations); if this switch is @code{false} such operations are not carried out.
@c IS THIS AN EVALUATION OR A SIMPLIFICATION FLAG ??

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{domxnctimes}
@defvr {Option variable} domxnctimes
Default value: @code{false}

When @code{domxnctimes} is @code{true}, non-commutative products of
matrices are carried out.
@c IS THIS AN EVALUATION OR A SIMPLIFICATION FLAG ??

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dontfactor}
@defvr {Option variable} dontfactor
Default value: @code{[]}

@code{dontfactor} may be set to a list of variables with respect to which
factoring is not to occur.  (The list is initially empty.) Factoring also will
not take place with respect to any variables which are less important, according
the variable ordering assumed for canonical rational expression (CRE) form, than
those on the @code{dontfactor} list.

@opencatbox{Categories:}
@category{Expressions}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{doscmxops}
@defvr {Option variable} doscmxops
Default value: @code{false}

When @code{doscmxops} is @code{true}, scalar-matrix operations are
carried out.
@c IS THIS AN EVALUATION OR A SIMPLIFICATION FLAG ??

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{doscmxplus}
@defvr {Option variable} doscmxplus
Default value: @code{false}

When @code{doscmxplus} is @code{true}, scalar-matrix operations yield
a matrix result.  This switch is not subsumed under @mrefdot{doallmxops}
@c IS THIS AN EVALUATION OR A SIMPLIFICATION FLAG ??

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dot0nscsimp}
@defvr {Option variable} dot0nscsimp
Default value: @code{true}

@c WHAT DOES THIS MEAN EXACTLY ??
When @code{dot0nscsimp} is @code{true}, a non-commutative product of zero
and a nonscalar term is simplified to a commutative product.

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dot0simp}
@defvr {Option variable} dot0simp
Default value: @code{true}

@c WHAT DOES THIS MEAN EXACTLY ??
When @code{dot0simp} is @code{true},
a non-commutative product of zero and
a scalar term is simplified to a commutative product.

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dot1simp}
@defvr {Option variable} dot1simp
Default value: @code{true}

@c WHAT DOES THIS MEAN EXACTLY ??
When @code{dot1simp} is @code{true},
a non-commutative product of one and
another term is simplified to a commutative product.

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dotassoc}
@defvr {Option variable} dotassoc
Default value: @code{true}

When @code{dotassoc} is @code{true}, an expression @code{(A.B).C} simplifies to
@code{A.(B.C)}.
@c "." MEANS NONCOMMUTATIVE MULTIPLICATION RIGHT ??

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dotconstrules}
@defvr {Option variable} dotconstrules
Default value: @code{true}

When @code{dotconstrules} is @code{true}, a non-commutative product of a
constant and another term is simplified to a commutative product.
@c TERMINOLOGY: (1) SWITCH/FLAG/SOME OTHER TERM ??
@c              (2) ASSIGN/SET/TURN ON/SOME OTHER TERM ??
Turning on this flag effectively turns on @mrefcomma{dot0simp}@w{}
@mrefcomma{dot0nscsimp} and @mref{dot1simp} as well.

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dotdistrib}
@defvr {Option variable} dotdistrib
Default value: @code{false}

When @code{dotdistrib} is @code{true}, an expression @code{A.(B + C)} simplifies
to @code{A.B + A.C}.

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dotexptsimp}
@defvr {Option variable} dotexptsimp
Default value: @code{true}

When @code{dotexptsimp} is @code{true}, an expression @code{A.A} simplifies to
@code{A^^2}.

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dotident}
@defvr {Option variable} dotident
Default value: 1

@code{dotident} is the value returned by @code{X^^0}.
@c "RETURNED" ?? IS THIS A SIMPLIFICATION OR AN EVALUATION ??

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{dotscrules}
@defvr {Option variable} dotscrules
Default value: @code{false}

When @code{dotscrules} is @code{true}, an expression @code{A.SC} or @code{SC.A}
simplifies to @code{SC*A} and @code{A.(SC*B)} simplifies to @code{SC*(A.B)}.
@c HMM, DOES "SC" MEAN "SCALAR" HERE ?? CLARIFY

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{echelon}
@deffn {Function} echelon (@var{M})

Returns the echelon form of the matrix @var{M},
as produced by Gaussian elimination.
The echelon form is computed from @var{M}
by elementary row operations such that the first
non-zero element in each row in the resulting matrix is one and the
column elements under the first one in each row are all zero.

@mref{triangularize} also carries out Gaussian elimination, but it does not
normalize the leading non-zero element in each row.

@mref{lu_factor} and @mref{cholesky} are other functions which yield
triangularized matrices.

@c ===beg===
@c M: matrix ([3, 7, aa, bb], [-1, 8, 5, 2], [9, 2, 11, 4]);
@c echelon (M);
@c ===end===
@example
@group
(%i1) M: matrix ([3, 7, aa, bb], [-1, 8, 5, 2], [9, 2, 11, 4]);
                       [  3   7  aa  bb ]
                       [                ]
(%o1)                  [ - 1  8  5   2  ]
                       [                ]
                       [  9   2  11  4  ]
@end group
@group
(%i2) echelon (M);
                  [ 1  - 8  - 5      - 2     ]
                  [                          ]
                  [         28       11      ]
                  [ 0   1   --       --      ]
(%o2)             [         37       37      ]
                  [                          ]
                  [              37 bb - 119 ]
                  [ 0   0    1   ----------- ]
                  [              37 aa - 313 ]
@end group
@end example

@opencatbox{Categories:}
@category{Linear equations}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{eigenvalues}
@anchor{eivals}
@deffn  {Function} eigenvalues (@var{M})
@deffnx {Function} eivals (@var{M})

@c eigen.mac IS AUTOLOADED IF eigenvalues OR eigenvectors IS REFERENCED; EXTEND THAT TO ALL FUNCTIONS ??
@c EACH FUNCTION INTENDED FOR EXTERNAL USE SHOULD HAVE ITS OWN DOCUMENTATION ITEM
Returns a list of two lists containing the eigenvalues of the matrix @var{M}.
The first sublist of the return value is the list of eigenvalues of the
matrix, and the second sublist is the list of the
multiplicities of the eigenvalues in the corresponding order.

@code{eivals} is a synonym for @code{eigenvalues}.

@code{eigenvalues} calls the function @mref{solve} to find the roots of the
characteristic polynomial of the matrix.  Sometimes @code{solve} may not be able
to find the roots of the polynomial; in that case some other functions in this
package (except @mrefcomma{innerproduct} @mrefcomma{unitvector}@w{}
@mref{columnvector} and @mref{gramschmidt}) will not work.
@c WHICH ONES ARE THE FUNCTIONS WHICH DON'T WORK ??
@c ACTUALLY IT'S MORE IMPORTANT TO LIST THE ONES WHICH DON'T WORK HERE
@c WHAT DOES "will not work" MEAN, ANYWAY ??
Sometimes @code{solve} may find only a subset of the roots of the polynomial.
This may happen when the factoring of the polynomial contains polynomials
of degree 5 or more. In such cases a warning message is displayed and the
only the roots found and their corresponding multiplicities are returned.

In some cases the eigenvalues found by @code{solve} may be complicated
expressions.  (This may happen when @code{solve} returns a not-so-obviously real
expression for an eigenvalue which is known to be real.)  It may be possible to
simplify the eigenvalues using some other functions.
@c WHAT ARE THOSE OTHER FUNCTIONS ??

The package @code{eigen.mac} is loaded automatically when
@code{eigenvalues} or @mref{eigenvectors} is referenced.
If @code{eigen.mac} is not already loaded,
@code{load ("eigen")} loads it.
After loading, all functions and variables in the package are available.
@c REFER TO OVERVIEW OF PACKAGE (INCLUDING LIST OF FUNCTIONS) HERE

For matrices consisting of only floating-point values, see also
@mrefdot{dgeev}

@c NEED EXAMPLES HERE
@opencatbox{Categories:}
@category{Package eigen}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{eigenvectors}
@anchor{eivects}
@deffn  {Function} eigenvectors (@var{M})
@deffnx {Function} eivects (@var{M})

Computes eigenvectors of the matrix @var{M}.
The return value is a list of two elements.
The first is a list of the eigenvalues of @var{M}
and a list of the multiplicities of the eigenvalues.
The second is a list of lists of eigenvectors.
There is one list of eigenvectors for each eigenvalue.
There may be one or more eigenvectors in each list.

@code{eivects} is a synonym for @code{eigenvectors}.

The package @code{eigen.mac} is loaded automatically when
@mref{eigenvalues} or @code{eigenvectors} is referenced.
If @code{eigen.mac} is not already loaded,
@code{load ("eigen")} loads it.
After loading, all functions and variables in the package are available.

Note that @code{eigenvectors} internally calls @code{eigenvalues} to
obtain eigenvalues. So, when @code{eigenvalues} returns a subset of
all the eigenvalues, the @code{eigenvectors} returns the corresponding
subset of the all the eigenvectors, with the same warning displayed as
@code{eigenvalues}.

The flags that affect this function are:

@code{nondiagonalizable} is set to @code{true} or @code{false} depending on
whether the matrix is nondiagonalizable or diagonalizable after
@code{eigenvectors} returns.

@code{hermitianmatrix} when @code{true}, causes the degenerate
eigenvectors of the Hermitian matrix to be orthogonalized using the
Gram-Schmidt algorithm.

@code{knowneigvals} when @code{true} causes the @code{eigen} package to assume
the eigenvalues of the matrix are known to the user and stored under the global
name @code{listeigvals}.  @code{listeigvals} should be set to a list similar
to the output @code{eigenvalues}.

The function @mref{algsys} is used here to solve for the eigenvectors.
Sometimes if the eigenvalues are messy, @code{algsys} may not be able to find a
solution.  In some cases, it may be possible to simplify the eigenvalues by
first finding them using @code{eigenvalues} command and then using other
functions to reduce them to something simpler.  Following simplification,
@code{eigenvectors} can be called again with the @code{knowneigvals} flag set
to @code{true}.

See also @mrefdot{eigenvalues}

For matrices consisting of only floating-point values, see also
@mrefdot{dgeev}

Examples:

A matrix which has just one eigenvector per eigenvalue.

@c ===beg===
@c M1: matrix ([11, -1], [1, 7]);
@c [vals, vecs] : eigenvectors (M1);
@c for i thru length (vals[1]) do disp (val[i] = vals[1][i],
@c   mult[i] = vals[2][i], vec[i] = vecs[i]);
@c ===end===
@example
@group
(%i1) M1: matrix ([11, -1], [1, 7]);
                           [ 11  - 1 ]
(%o1)                      [         ]
                           [ 1    7  ]
@end group
@group
(%i2) [vals, vecs] : eigenvectors (M1);
(%o2) [[[9 - sqrt(3), sqrt(3) + 9], [1, 1]], 
                        [[[1, sqrt(3) + 2]], [[1, 2 - sqrt(3)]]]]
@end group
@group
(%i3) for i thru length (vals[1]) do disp (val[i] = vals[1][i],
  mult[i] = vals[2][i], vec[i] = vecs[i]);
                       val  = 9 - sqrt(3)
                          1

                            mult  = 1
                                1

                    vec  = [[1, sqrt(3) + 2]]
                       1

                       val  = sqrt(3) + 9
                          2

                            mult  = 1
                                2

                    vec  = [[1, 2 - sqrt(3)]]
                       2

(%o3)                         done
@end group
@end example

A matrix which has two eigenvectors for one eigenvalue (namely 2).

@c ===beg===
@c M1 : matrix ([0, 1, 0, 0], [0, 0, 0, 0], [0, 0, 2, 0], [0, 0, 0, 2]);
@c [vals, vecs] : eigenvectors (M1);
@c for i thru length (vals[1]) do disp (val[i] = vals[1][i],
@c   mult[i] = vals[2][i], vec[i] = vecs[i]);
@c ===end===
@example
@group
(%i1) M1: matrix ([0, 1, 0, 0], [0, 0, 0, 0], [0, 0, 2, 0],
                  [0, 0, 0, 2]);
                        [ 0  1  0  0 ]
                        [            ]
                        [ 0  0  0  0 ]
(%o1)                   [            ]
                        [ 0  0  2  0 ]
                        [            ]
                        [ 0  0  0  2 ]
@end group
@group
(%i2) [vals, vecs]: eigenvectors (M1);
(%o2) [[[0, 2], [2, 2]], [[[1, 0, 0, 0]], 
                                   [[0, 0, 1, 0], [0, 0, 0, 1]]]]
@end group
@group
(%i3) for i thru length (vals[1]) do disp (val[i] = vals[1][i],
  mult[i] = vals[2][i], vec[i] = vecs[i]);
                            val  = 0
                               1

                            mult  = 2
                                1

                      vec  = [[1, 0, 0, 0]]
                         1

                            val  = 2
                               2

                            mult  = 2
                                2

               vec  = [[0, 0, 1, 0], [0, 0, 0, 1]]
                  2

(%o3)                         done
@end group
@end example

@opencatbox{Categories:}
@category{Package eigen}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{ematrix}
@deffn {Function} ematrix (@var{m}, @var{n}, @var{x}, @var{i}, @var{j})

Returns an @var{m} by @var{n} matrix, all elements of which
are zero except for the @code{[@var{i}, @var{j}]} element which is @var{x}.
@c WOW, THAT SEEMS PRETTY SPECIALIZED ...

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{entermatrix}
@deffn {Function} entermatrix (@var{m}, @var{n})

Returns an @var{m} by @var{n} matrix, reading the elements interactively.

If @var{n} is equal to @var{m}, Maxima prompts for the type of the matrix
(diagonal, symmetric, antisymmetric, or general) and for each element.
Each response is terminated by a semicolon @code{;} or dollar sign @code{$}.

If @var{n} is not equal to @var{m},
Maxima prompts for each element.

The elements may be any expressions, which are evaluated.
@code{entermatrix} evaluates its arguments.

@example
(%i1) n: 3$
(%i2) m: entermatrix (n, n)$

Is the matrix  1. Diagonal  2. Symmetric  3. Antisymmetric 
4. General
Answer 1, 2, 3 or 4 : 
1$
Row 1 Column 1: 
(a+b)^n$
Row 2 Column 2: 
(a+b)^(n+1)$
Row 3 Column 3: 
(a+b)^(n+2)$

Matrix entered.
(%i3) m;
                [        3                     ]
                [ (b + a)      0         0     ]
                [                              ]
(%o3)           [                  4           ]
                [    0      (b + a)      0     ]
                [                              ]
                [                            5 ]
                [    0         0      (b + a)  ]
@end example

@opencatbox{Categories:}
@category{Console interaction}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{genmatrix}
@deffn  {Function} genmatrix @
@fname{genmatrix} (@var{a}, @var{i_2}, @var{j_2}, @var{i_1}, @var{j_1}) @
@fname{genmatrix} (@var{a}, @var{i_2}, @var{j_2}, @var{i_1}) @
@fname{genmatrix} (@var{a}, @var{i_2}, @var{j_2})

Returns a matrix generated from @var{a}, taking element
@code{@var{a}[@var{i_1}, @var{j_1}]} as the upper-left element and
@code{@var{a}[@var{i_2}, @var{j_2}]} as the lower-right element of the matrix.
Here @var{a} is a declared array (created by @code{array} but not by
@mref{make_array}) or a @mrefcomma{hashed array} or a @mrefcomma{memoizing function} or a lambda
expression of two arguments.  (A @mref{memoizing function} is created like other functions
with @mref{:=} or @mrefcomma{define} but arguments are enclosed in square
brackets instead of parentheses.)

If @var{j_1} is omitted, it is assumed equal to @var{i_1}.
If both @var{j_1} and @var{i_1} are omitted, both are assumed equal to 1.

If a selected element @code{i,j} of the array is undefined,
the matrix will contain a symbolic element @code{@var{a}[i,j]}.

Examples:

@c ===beg===
@c h [i, j] := 1 / (i + j - 1);
@c genmatrix (h, 3, 3);
@c array (a, fixnum, 2, 2);
@c a [1, 1] : %e;
@c a [2, 2] : %pi;
@c genmatrix (a, 2, 2);
@c genmatrix (lambda ([i, j], j - i), 3, 3);
@c genmatrix (B, 2, 2);
@c ===end===
@example
@group
(%i1) h [i, j] := 1 / (i + j - 1);
                                    1
(%o1)                  h     := ---------
                        i, j    i + j - 1
@end group
@group
(%i2) genmatrix (h, 3, 3);
                           [    1  1 ]
                           [ 1  -  - ]
                           [    2  3 ]
                           [         ]
                           [ 1  1  1 ]
(%o2)                      [ -  -  - ]
                           [ 2  3  4 ]
                           [         ]
                           [ 1  1  1 ]
                           [ -  -  - ]
                           [ 3  4  5 ]
@end group
@group
(%i3) array (a, fixnum, 2, 2);
(%o3)                           a
@end group
@group
(%i4) a [1, 1] : %e;
(%o4)                          %e
@end group
@group
(%i5) a [2, 2] : %pi;
(%o5)                          %pi
@end group
@group
(%i6) genmatrix (a, 2, 2);
                           [ %e   0  ]
(%o6)                      [         ]
                           [ 0   %pi ]
@end group
@group
(%i7) genmatrix (lambda ([i, j], j - i), 3, 3);
                         [  0    1   2 ]
                         [             ]
(%o7)                    [ - 1   0   1 ]
                         [             ]
                         [ - 2  - 1  0 ]
@end group
@group
(%i8) genmatrix (B, 2, 2);
                        [ B      B     ]
                        [  1, 1   1, 2 ]
(%o8)                   [              ]
                        [ B      B     ]
                        [  2, 1   2, 2 ]
@end group
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{gramschmidt}
@deffn  {Function} gramschmidt @
@fname{gramschmidt} (@var{x}) @
@fname{gramschmidt} (@var{x}, @var{F})

Carries out the Gram-Schmidt orthogonalization algorithm on @var{x}, which is
either a matrix or a list of lists.  @var{x} is not modified by
@code{gramschmidt}.  The inner product employed by @code{gramschmidt} is
@var{F}, if present, otherwise the inner product is the function
@mrefdot{innerproduct}

If @var{x} is a matrix, the algorithm is applied to the rows of @var{x}.  If
@var{x} is a list of lists, the algorithm is applied to the sublists, which must
have equal numbers of elements.  In either case, the return value is a list of
lists, the sublists of which are orthogonal and span the same space as @var{x}.
If the dimension of the span of @var{x} is less than the number of rows or
sublists, some sublists of the return value are zero.

@mref{factor} is called at each stage of the algorithm to simplify intermediate
results.  As a consequence, the return value may contain factored integers.

@code{load("eigen")} loads this function.

Example:

Gram-Schmidt algorithm using default inner product function.

@c ===beg===
@c load ("eigen")$
@c x: matrix ([1, 2, 3], [9, 18, 30], [12, 48, 60]);
@c y: gramschmidt (x);
@c map (innerproduct, [y[1], y[2], y[3]], [y[2], y[3], y[1]]);
@c ===end===
@example
(%i1) load ("eigen")$
@group
(%i2) x: matrix ([1, 2, 3], [9, 18, 30], [12, 48, 60]);
                         [ 1   2   3  ]
                         [            ]
(%o2)                    [ 9   18  30 ]
                         [            ]
                         [ 12  48  60 ]
@end group
@group
(%i3) y: gramschmidt (x);
                       2      2            4     3
                      3      3   3 5      2  3  2  3
(%o3)  [[1, 2, 3], [- ---, - --, ---], [- ----, ----, 0]]
                      2 7    7   2 7       5     5
@end group
@group
(%i4) map (innerproduct, [y[1], y[2], y[3]], [y[2], y[3], y[1]]);
(%o4)                       [0, 0, 0]
@end group
@end example

Gram-Schmidt algorithm using a specified inner product function.

@c ===beg===
@c load ("eigen")$
@c ip (f, g) := integrate (f * g, u, a, b);
@c y: gramschmidt ([1, sin(u), cos(u)], ip), a=-%pi/2, b=%pi/2;
@c map (ip, [y[1], y[2], y[3]], [y[2], y[3], y[1]]), a=-%pi/2,
@c          b=%pi/2;
@c ===end===
@example
(%i1) load ("eigen")$
@group
(%i2) ip (f, g) := integrate (f * g, u, a, b);
(%o2)          ip(f, g) := integrate(f g, u, a, b)
@end group
@group
(%i3) y: gramschmidt ([1, sin(u), cos(u)], ip), a=-%pi/2, b=%pi/2;
                               %pi cos(u) - 2
(%o3)              [1, sin(u), --------------]
                                    %pi
@end group
@group
(%i4) map (ip, [y[1], y[2], y[3]], [y[2], y[3], y[1]]), a=-%pi/2,
         b=%pi/2;
(%o4)                       [0, 0, 0]
@end group
@end example

@opencatbox{Categories:}
@category{Package eigen}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{ident}
@deffn {Function} ident (@var{n})

Returns an @var{n} by @var{n} identity matrix.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{innerproduct}
@anchor{inprod}
@deffn  {Function} innerproduct (@var{x}, @var{y})
@deffnx {Function} inprod (@var{x}, @var{y})

Returns the inner product (also called the scalar product or dot product) of
@var{x} and @var{y}, which are lists of equal length, or both 1-column or 1-row
matrices of equal length.  The return value is @code{conjugate (x) . y},
where @code{.} is the noncommutative multiplication operator.

@code{load ("eigen")} loads this function.

@code{inprod} is a synonym for @code{innerproduct}.

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Package eigen}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{invert_by_adjoint}
@deffn {Function} invert_by_adjoint (@var{M})
Returns the inverse of the matrix @var{M}.
The inverse is computed by the adjoint method.

@code{invert_by_adjoint} honors the @mref{ratmx} and @mref{detout} flags,
the same as @mrefdot{invert}

@end deffn

@c -----------------------------------------------------------------------------
@anchor{invert}
@deffn {Function} invert (@var{M})

Returns the inverse of the matrix @var{M}.
The inverse is computed via the LU decomposition.

When @mref{ratmx} is @code{true},
elements of @var{M} are converted to canonical rational expressions (CRE),
and the elements of the return value are also CRE.

When @mref{ratmx} is @code{false},
elements of @var{M} are not converted to a common representation.
In particular, float and bigfloat elements are not converted to rationals.

When @mref{detout} is @code{true}, the determinant is factored out of the inverse.
The global flags @mref{doallmxops} and @mref{doscmxops} must be @code{false}
to prevent the determinant from being absorbed into the inverse.
@mref{xthru} can multiply the determinant into the inverse.

@mref{invert} does not apply any simplifications to the elements of the inverse
apart from the default arithmetic simplifications.
@mref{ratsimp} and @mref{expand} can apply additional simplifications.
In particular, when @var{M} has polynomial elements,
@code{expand(invert(@var{M}))} might be preferable.

@code{invert(@var{M})} is equivalent to @code{@var{M}^^-1}.

@c NEED EXAMPLES HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{list_matrix_entries}
@deffn {Function} list_matrix_entries (@var{M})

Returns a list containing the elements of the matrix @var{M}.

Example:

@c ===beg===
@c list_matrix_entries(matrix([a,b],[c,d]));
@c ===end===
@example
@group
(%i1) list_matrix_entries(matrix([a,b],[c,d]));
(%o1)                     [a, b, c, d]
@end group
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{lmxchar}
@defvr {Option variable} lmxchar
Default value: @code{[}

@code{lmxchar} is the character displayed as the left delimiter of a matrix.
See also @mrefdot{rmxchar}

@code{lmxchar} is only used when @code{display2d_unicode} is @code{false}.

Example:

@example
(%i1) display2d_unicode: false $
(%i2) lmxchar: "|"$
(%i3) matrix ([a, b, c], [d, e, f], [g, h, i]);
                           | a  b  c ]
                           |         ]
(%o3)                      | d  e  f ]
                           |         ]
                           | g  h  i ]
@end example

@opencatbox{Categories:}
@category{Display flags and variables}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{matrix}
@deffn {Function} matrix (@var{row_1}, @dots{}, @var{row_n})

Returns a rectangular matrix which has the rows @var{row_1}, @dots{},
@var{row_n}.  Each row is a list of expressions.  All rows must be the same
length.

The operations @code{+} (addition), @code{-} (subtraction), @code{*}
(multiplication), and @code{/} (division), are carried out element by element
when the operands are two matrices, a scalar and a matrix, or a matrix and a
scalar.  The operation @code{^} (exponentiation, equivalently @code{**})
is carried out element by element if the operands are a scalar and a matrix or
a matrix and a scalar, but not if the operands are two matrices.
@c WHAT DOES THIS NEXT PHRASE MEAN EXACTLY ??
All operations are normally carried out in full,
including @code{.} (noncommutative multiplication).

Matrix multiplication is represented by the noncommutative multiplication
operator @code{.}.  The corresponding noncommutative exponentiation operator
is @code{^^}.  For a matrix @code{@var{A}}, @code{@var{A}.@var{A} = @var{A}^^2}
and @code{@var{A}^^-1} is the inverse of @var{A}, if it exists.
@code{@var{A}^^-1} is equivalent to @code{invert(@var{A})}.

There are switches for controlling simplification of expressions involving dot
and matrix-list operations.  These are
@mrefcomma{doallmxops} @mrefcomma{domxexpt} @mrefcomma{domxmxops}@w{}
@mrefcomma{doscmxops} and @mrefdot{doscmxplus}
@c CHECK -- WE PROBABLY WANT EXHAUSTIVE LIST HERE

There are additional options which are related to matrices.  These are:
@mrefcomma{lmxchar} @mrefcomma{rmxchar} @mrefcomma{ratmx}@w{}
@mrefcomma{listarith} @mrefcomma{detout} @code{scalarmatrix} and
@mrefdot{sparse}
@c CHECK -- WE PROBABLY WANT EXHAUSTIVE LIST HERE

There are a number of functions which take matrices as arguments or yield
matrices as return values.
See @mrefcomma{eigenvalues} @mrefcomma{eigenvectors} @mrefcomma{determinant}@w{}
@mrefcomma{charpoly} @mrefcomma{genmatrix} @mrefcomma{addcol}@w{}
@mrefcomma{addrow} @mrefcomma{copymatrix} @mrefcomma{transpose}@w{}
@mrefcomma{echelon} and @mrefdot{rank}
@c CHECK -- WE PROBABLY WANT EXHAUSTIVE LIST HERE

Examples:

@itemize @bullet
@item
Construction of matrices from lists.
@end itemize
@example
(%i1) x: matrix ([17, 3], [-8, 11]);
                           [ 17   3  ]
(%o1)                      [         ]
                           [ - 8  11 ]
(%i2) y: matrix ([%pi, %e], [a, b]);
                           [ %pi  %e ]
(%o2)                      [         ]
                           [  a   b  ]
@end example
@itemize @bullet
@item
Addition, element by element.
@end itemize
@example
(%i3) x + y;
                      [ %pi + 17  %e + 3 ]
(%o3)                 [                  ]
                      [  a - 8    b + 11 ]
@end example
@itemize @bullet
@item
Subtraction, element by element.
@end itemize
@example
(%i4) x - y;
                      [ 17 - %pi  3 - %e ]
(%o4)                 [                  ]
                      [ - a - 8   11 - b ]
@end example
@itemize @bullet
@item
Multiplication, element by element.
@end itemize
@example
(%i5) x * y;
                        [ 17 %pi  3 %e ]
(%o5)                   [              ]
                        [ - 8 a   11 b ]
@end example
@itemize @bullet
@item
Division, element by element.
@end itemize
@example
(%i6) x / y;
                        [ 17       - 1 ]
                        [ ---  3 %e    ]
                        [ %pi          ]
(%o6)                   [              ]
                        [   8    11    ]
                        [ - -    --    ]
                        [   a    b     ]
@end example
@itemize @bullet
@item
Matrix to a scalar exponent, element by element.
@end itemize
@example
(%i7) x ^ 3;
                         [ 4913    27  ]
(%o7)                    [             ]
                         [ - 512  1331 ]
@end example
@itemize @bullet
@item
Scalar base to a matrix exponent, element by element.
@end itemize
@example
(%i8) exp(y); 
                         [   %pi    %e ]
                         [ %e     %e   ]
(%o8)                    [             ]
                         [    a     b  ]
                         [  %e    %e   ]
@end example
@itemize @bullet
@item
Matrix base to a matrix exponent.  This is not carried out element by element.
See also @mrefdot{matrixexp}
@c WHAT IS THIS ??
@end itemize
@example
(%i9) x ^ y;
                                [ %pi  %e ]
                                [         ]
                                [  a   b  ]
                     [ 17   3  ]
(%o9)                [         ]
                     [ - 8  11 ]
@end example
@itemize @bullet
@item
Noncommutative matrix multiplication.
@end itemize
@example
(%i10) x . y;
                  [ 3 a + 17 %pi  3 b + 17 %e ]
(%o10)            [                           ]
                  [ 11 a - 8 %pi  11 b - 8 %e ]
(%i11) y . x;
                [ 17 %pi - 8 %e  3 %pi + 11 %e ]
(%o11)          [                              ]
                [  17 a - 8 b     11 b + 3 a   ]
@end example
@itemize @bullet
@item
Noncommutative matrix exponentiation.
A scalar base @var{b} to a matrix power @var{M}
is carried out element by element and so @code{b^^m} is the same as @code{b^m}.
@end itemize
@example
(%i12) x ^^ 3;
                        [  3833   1719 ]
(%o12)                  [              ]
                        [ - 4584  395  ]
(%i13) %e ^^ y;
@group
                         [   %pi    %e ]
                         [ %e     %e   ]
(%o13)                   [             ]
                         [    a     b  ]
                         [  %e    %e   ]
@end group
@end example
@itemize @bullet
@item
A matrix raised to a -1 exponent with noncommutative exponentiation is the
matrix inverse, if it exists.
@end itemize
@example
(%i14) x ^^ -1;
                         [ 11      3  ]
                         [ ---  - --- ]
                         [ 211    211 ]
(%o14)                   [            ]
                         [  8    17   ]
                         [ ---   ---  ]
                         [ 211   211  ]
(%i15) x . (x ^^ -1);
                            [ 1  0 ]
(%o15)                      [      ]
                            [ 0  1 ]
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{matrixexp}
@deffn {Function} matrixexp @
@fname{matrixexp} (@var{M}) @
@fname{matrixexp} (@var{M}, @var{n}) @
@fname{matrixexp} (@var{M}, @var{V})

Calculates the matrix exponential
m4_math(
<<<e^{M\cdot V}>>>,
<<<e^(M*V)>>>,
<<<e^{MV}>>>)
@c @ifnotinfo
@c @tex
@c @math{e^{MV}}
@c @end tex
@c @ifset mathjax
@c @html
@c $$e^{M\cdot V}$$
@c @end html
@c @end ifset
@c @ifclear mathjax
@c @math{e^(M*V)}
@c @end ifclear
@c @end ifnotinfo 
@c @ifinfo
@c @math{e^(M*V)}
@c @end ifinfo
. Instead of the vector @var{V} a number @var{n} can be specified as the second
argument. If this argument is omitted @code{matrixexp} replaces it by @code{1}.

The matrix exponential of a matrix @var{M} can be expressed as a power series:
m4_displaymath(
<<<e^M=\sum_{k=0}^\infty{\left(\frac{M^k}{k!}\right)}>>>,
<<<@math{e^M=sum(M^k/k!,0,inf)}>>>
)
@c @ifnotinfo
@c @tex
@c @math{e^M=\sum_{k=0}^\infty{{M^k}\over{k!}}}
@c @end tex
@c @ifset mathjax
@c @html
@c $$e^M=\sum_{k=0}^\infty{\left(\frac{M^k}{k!}\right)}$$
@c @end html
@c @end ifset
@c @ifclear mathjax
@c @math{e^M=sum(M^k/k!,0,inf)}
@c @end ifclear
@c @end ifnotinfo 
@c @ifinfo
@c @math{e^M=sum(M^k/k!,0,inf)}
@c @end ifinfo
@c 
@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn


@c -----------------------------------------------------------------------------
@anchor{matrixmap}
@deffn {Function} matrixmap (@var{f}, @var{M})

Returns a matrix with element @code{i,j} equal to @code{@var{f}(@var{M}[i,j])}.

See also @mrefcomma{map} @mrefcomma{fullmap} @mrefcomma{fullmapl} and
@mrefdot{apply}

@c NEED EXAMPLE HERE
@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{matrixp}
@deffn {Function} matrixp (@var{expr})

Returns @code{true} if @var{expr} is a matrix, otherwise @code{false}.

@opencatbox{Categories:}
@category{Predicate functions}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{matrix_element_add}
@defvr {Option variable} matrix_element_add
Default value: @code{+}

@code{matrix_element_add} is the operation 
invoked in place of addition in a matrix multiplication.
@code{matrix_element_add} can be assigned any n-ary operator
(that is, a function which handles any number of arguments).
The assigned value may be the name of an operator enclosed in quote marks,
the name of a function,
or a lambda expression.

See also @mref{matrix_element_mult} and @mrefdot{matrix_element_transpose}

Example:

@example
(%i1) matrix_element_add: "*"$
(%i2) matrix_element_mult: "^"$
(%i3) aa: matrix ([a, b, c], [d, e, f]);
                           [ a  b  c ]
(%o3)                      [         ]
                           [ d  e  f ]
(%i4) bb: matrix ([u, v, w], [x, y, z]);
@group
                           [ u  v  w ]
(%o4)                      [         ]
                           [ x  y  z ]
@end group
(%i5) aa . transpose (bb);
                     [  u  v  w   x  y  z ]
                     [ a  b  c   a  b  c  ]
(%o5)                [                    ]
                     [  u  v  w   x  y  z ]
                     [ d  e  f   d  e  f  ]
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{matrix_element_mult}
@defvr {Option variable} matrix_element_mult
Default value: @code{*}

@code{matrix_element_mult} is the operation 
invoked in place of multiplication in a matrix multiplication.
@code{matrix_element_mult} can be assigned any binary operator.
The assigned value may be the name of an operator enclosed in quote marks,
the name of a function,
or a lambda expression.

The dot operator @code{.} is a useful choice in some contexts.

See also @mref{matrix_element_add} and @mrefdot{matrix_element_transpose}

Example:

@example
(%i1) matrix_element_add: lambda ([[x]], sqrt (apply ("+", x)))$
(%i2) matrix_element_mult: lambda ([x, y], (x - y)^2)$
(%i3) [a, b, c] . [x, y, z];
                          2          2          2
(%o3)         sqrt((c - z)  + (b - y)  + (a - x) )
(%i4) aa: matrix ([a, b, c], [d, e, f]);
                           [ a  b  c ]
(%o4)                      [         ]
                           [ d  e  f ]
(%i5) bb: matrix ([u, v, w], [x, y, z]);
                           [ u  v  w ]
(%o5)                      [         ]
                           [ x  y  z ]
(%i6) aa . transpose (bb);
               [             2          2          2  ]
               [ sqrt((c - w)  + (b - v)  + (a - u) ) ]
(%o6)  Col 1 = [                                      ]
               [             2          2          2  ]
               [ sqrt((f - w)  + (e - v)  + (d - u) ) ]

                         [             2          2          2  ]
                         [ sqrt((c - z)  + (b - y)  + (a - x) ) ]
                 Col 2 = [                                      ]
                         [             2          2          2  ]
                         [ sqrt((f - z)  + (e - y)  + (d - x) ) ]
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{matrix_element_transpose}
@defvr {Option variable} matrix_element_transpose
Default value: @code{false}

@code{matrix_element_transpose} is the operation 
applied to each element of a matrix when it is transposed.
@mref{matrix_element_mult} can be assigned any unary operator.
The assigned value may be the name of an operator enclosed in quote marks,
the name of a function, or a lambda expression.

When @code{matrix_element_transpose} equals @mrefcomma{transpose}
the @code{transpose} function is applied to every element.
When @code{matrix_element_transpose} equals @code{nonscalars},
the @code{transpose} function is applied to every nonscalar element.
If some element is an atom, the @code{nonscalars} option applies
@code{transpose} only if the atom is declared nonscalar,
while the @code{transpose} option always applies @code{transpose}.

The default value, @code{false}, means no operation is applied.

See also @mref{matrix_element_add} and @mrefdot{matrix_element_mult}

Examples:

@example
(%i1) declare (a, nonscalar)$
(%i2) transpose ([a, b]);
                        [ transpose(a) ]
(%o2)                   [              ]
                        [      b       ]
(%i3) matrix_element_transpose: nonscalars$
(%i4) transpose ([a, b]);
                        [ transpose(a) ]
(%o4)                   [              ]
                        [      b       ]
(%i5) matrix_element_transpose: transpose$
(%i6) transpose ([a, b]);
                        [ transpose(a) ]
(%o6)                   [              ]
                        [ transpose(b) ]
(%i7) matrix_element_transpose: lambda ([x], realpart(x)
      - %i*imagpart(x))$
(%i8) m: matrix ([1 + 5*%i, 3 - 2*%i], [7*%i, 11]);
                     [ 5 %i + 1  3 - 2 %i ]
(%o8)                [                    ]
                     [   7 %i       11    ]
(%i9) transpose (m);
                      [ 1 - 5 %i  - 7 %i ]
(%o9)                 [                  ]
                      [ 2 %i + 3    11   ]
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c IS THIS THE ONLY MATRIX TRACE FUNCTION ??

@c -----------------------------------------------------------------------------
@anchor{mattrace}
@deffn {Function} mattrace (@var{M})

Returns the trace (that is, the sum of the elements on the main diagonal) of
the square matrix @var{M}.

@code{mattrace} is called by @mrefcomma{ncharpoly} an alternative to Maxima's
@mrefdot{charpoly}
@c UMM, HOW IS THAT RELEVANT HERE ??

@code{load ("nchrpl")} loads this function.

@opencatbox{Categories:}
@category{Matrices}
@category{Package nchrpl}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{minor}
@deffn {Function} minor (@var{M}, @var{i}, @var{j})

Returns the @var{i}, @var{j} minor of the matrix @var{M}.  That is, @var{M}
with row @var{i} and column @var{j} removed.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{ncharpoly}
@deffn {Function} ncharpoly (@var{M}, @var{x})

Returns the characteristic polynomial of the matrix @var{M}
with respect to @var{x}.  This is an alternative to Maxima's @mrefdot{charpoly}

@code{ncharpoly} works by computing traces of powers of the given matrix,
which are known to be equal to sums of powers of the roots of the
characteristic polynomial.  From these quantities the symmetric
functions of the roots can be calculated, which are nothing more than
the coefficients of the characteristic polynomial.  @code{charpoly} works by
@c SHOULD THAT BE "m" INSTEAD OF "a" IN THE NEXT LINE ??
forming the determinant of @code{@var{x} * ident [n] - a}.  Thus
@code{ncharpoly} wins, for example, in the case of large dense matrices filled
with integers, since it avoids polynomial arithmetic altogether.

@code{load ("nchrpl")} loads this file.

@opencatbox{Categories:}
@category{Matrices}
@category{Package nchrpl}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{newdet}
@deffn {Function} newdet (@var{M})

Computes the determinant of the matrix @var{M} by the Johnson-Gentleman tree 
minor algorithm.  @code{newdet} returns the result in CRE form.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{permanent}
@deffn {Function} permanent (@var{M})

Computes the permanent of the matrix @var{M} by the Johnson-Gentleman tree
minor algorithm.  A permanent is like a determinant but with no sign changes.
@code{permanent} returns the result in CRE form.

See also @code{newdet}.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{rank}
@deffn {Function} rank (@var{M})

Computes the rank of the matrix @var{M}.  That is, the order of the
largest non-singular subdeterminant of @var{M}.

@c STATEMENT NEEDS CLARIFICATION
@var{rank} may return the
wrong answer if it cannot determine that a matrix element that is
equivalent to zero is indeed so.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{ratmx}
@defvr {Option variable} ratmx
Default value: @code{false}

When @code{ratmx} is @code{false}, determinant and matrix
addition, subtraction, and multiplication are performed in the
representation of the matrix elements and cause the result of
matrix inversion to be left in general representation.

When @code{ratmx} is @code{true},
the 4 operations mentioned above are performed in CRE form and the
result of matrix inverse is in CRE form.  Note that this may
cause the elements to be expanded (depending on the setting of @mref{ratfac})
which might not always be desired.

@opencatbox{Categories:}
@category{Matrices}
@category{Rational expressions}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{row}
@deffn {Function} row (@var{M}, @var{i})

Returns the @var{i}'th row of the matrix @var{M}.
The return value is a matrix.

The matrix returned by @code{row} shares memory with the argument @var{M};
a modification to the return value modifies @var{M}.

Examples:

@code{row} returns the @var{i}'th row of the matrix @var{M}.

@c ===beg===
@c abc: matrix ([12, 14, -4], [2, x, b], [3*y, -7, 9]);
@c row (abc, 1);
@c row (abc, 2);
@c row (abc, 3);
@c ===end===
@example
@group
(%i1) abc: matrix ([12, 14, -4], [2, x, b], [3*y, -7, 9]);
                        [ 12   14   - 4 ]
                        [               ]
(%o1)                   [  2    x    b  ]
                        [               ]
                        [ 3 y  - 7   9  ]
@end group
@group
(%i2) row (abc, 1);
(%o2)                    [ 12  14  - 4 ]
@end group
@group
(%i3) row (abc, 2);
(%o3)                      [ 2  x  b ]
@end group
@group
(%i4) row (abc, 3);
(%o4)                    [ 3 y  - 7  9 ]
@end group
@end example

The matrix returned by @code{row} shares memory with the argument.
In this example,
assigning a new value to @code{aa2} also modifies @code{aa}.

@c ===beg===
@c aa: matrix ([1, 2, x], [7, y, 3]);
@c aa2: row (aa, 2);
@c aa2[1, 3]: 123;
@c aa2;
@c aa;
@c ===end===
@example
@group
(%i1) aa: matrix ([1, 2, x], [7, y, 3]);
                           [ 1  2  x ]
(%o1)                      [         ]
                           [ 7  y  3 ]
@end group
@group
(%i2) aa2: row (aa, 2);
(%o2)                      [ 7  y  3 ]
@end group
@group
(%i3) aa2[1, 3]: 123;
(%o3)                          123
@end group
@group
(%i4) aa2;
(%o4)                     [ 7  y  123 ]
@end group
@group
(%i5) aa;
                          [ 1  2   x  ]
(%o5)                     [           ]
                          [ 7  y  123 ]
@end group
@end example

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{rmxchar}
@defvr {Option variable} rmxchar
Default value: @code{]}

@code{rmxchar} is the character drawn on the right-hand side of a matrix.

@code{rmxchar} is only used when @code{display2d_unicode} is @code{false}.

See also @mrefdot{lmxchar}

@opencatbox{Categories:}
@category{Display flags and variables}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{scalarmatrixp}
@defvr {Option variable} scalarmatrixp
Default value: @code{true}

When @code{scalarmatrixp} is @code{true}, then whenever a 1 x 1 matrix
is produced as a result of computing the dot product of matrices it
is simplified to a scalar, namely the sole element of the matrix.

When @code{scalarmatrixp} is @code{all},
then all 1 x 1 matrices are simplified to scalars.

When @code{scalarmatrixp} is @code{false}, 1 x 1 matrices are not simplified
to scalars.

@opencatbox{Categories:}
@category{Matrices}
@category{Simplification flags and variables}
@closecatbox
@end defvr

@c I WONDER WHAT THIS IS ABOUT

@c -----------------------------------------------------------------------------
@anchor{scalefactors}
@deffn {Function} scalefactors (@var{coordinatetransform})

Here the argument @var{coordinatetransform} evaluates to the form
@code{[[expression1, expression2, ...], indeterminate1, indeterminat2, ...]},
where the variables @var{indeterminate1}, @var{indeterminate2}, etc. are the
curvilinear coordinate variables and where a set of rectangular Cartesian
components is given in terms of the curvilinear coordinates by
@code{[expression1, expression2, ...]}.  @code{coordinates} is set to the vector
@code{[indeterminate1, indeterminate2,...]}, and @code{dimension} is set to the
length of this vector.  SF[1], SF[2], @dots{}, SF[DIMENSION] are set to the
coordinate scale factors, and @code{sfprod} is set to the product of these scale
factors.  Initially, @code{coordinates} is @code{[X, Y, Z]}, @code{dimension}
is 3, and SF[1]=SF[2]=SF[3]=SFPROD=1, corresponding to 3-dimensional rectangular
Cartesian coordinates.  To expand an expression into physical components in the
current coordinate system, there is a function with usage of the form
@c SOME TEXT HAS GONE MISSING HERE

@opencatbox{Categories:}
@category{Package vect}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{setelmx}
@deffn {Function} setelmx (@var{x}, @var{i}, @var{j}, @var{M})

Assigns @var{x} to the (@var{i}, @var{j})'th element of the matrix @var{M},
and returns the altered matrix.

@code{@var{M} [@var{i}, @var{j}]: @var{x}} has the same effect,
but returns @var{x} instead of @var{M}.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{similaritytransform}
@anchor{simtran}
@deffn  {Function} similaritytransform (@var{M})
@deffnx {Function} simtran (@var{M})

@code{similaritytransform} computes a similarity transform of the matrix
@code{M}.  It returns a list which is the output of the @code{uniteigenvectors}
command.  In addition if the flag @code{nondiagonalizable} is @code{false} two
global matrices @code{leftmatrix} and @code{rightmatrix} are computed.  These
matrices have the property that @code{leftmatrix . @var{M} . rightmatrix} is a
diagonal matrix with the eigenvalues of @var{M} on the diagonal.  If
@code{nondiagonalizable} is @code{true} the left and right matrices are not
computed.

If the flag @code{hermitianmatrix} is @code{true} then @code{leftmatrix} is the
complex conjugate of the transpose of @code{rightmatrix}.  Otherwise
@code{leftmatrix} is the inverse of @code{rightmatrix}.

@code{rightmatrix} is the matrix the columns of which are the unit
eigenvectors of @var{M}.  The other flags (see @code{eigenvalues} and
@code{eigenvectors}) have the same effects since
@code{similaritytransform} calls the other functions in the package in order
to be able to form @code{rightmatrix}.

@code{load ("eigen")} loads this function.

@code{simtran} is a synonym for @code{similaritytransform}.

@opencatbox{Categories:}
@category{Package eigen}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{sparse}
@defvr {Option variable} sparse
Default value: @code{false}

When @code{sparse} is @code{true}, and if @code{ratmx} is @code{true}, then
@code{determinant} will use special routines for computing sparse determinants.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{submatrix}
@deffn {Function} submatrix @
@fname{submatrix} (@var{i_1}, @dots{}, @var{i_m}, @var{M}, @var{j_1}, @dots{}, @var{j_n}) @
@fname{submatrix} (@var{i_1}, @dots{}, @var{i_m}, @var{M}) @
@fname{submatrix} (@var{M}, @var{j_1}, @dots{}, @var{j_n})

Returns a new matrix composed of the matrix @var{M} with rows @var{i_1},
@dots{}, @var{i_m} deleted, and columns @var{j_1}, @dots{}, @var{j_n} deleted.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{transpose}
@deffn {Function} transpose (@var{M})

Returns the transpose of @var{M}.

If @var{M} is a matrix, the return value is another matrix @var{N}
such that @code{N[i,j] = M[j,i]}.

If @var{M} is a list, the return value is a matrix @var{N}
of @code{length (m)} rows and 1 column, such that @code{N[i,1] = M[i]}.

Otherwise @var{M} is a symbol,
and the return value is a noun expression @code{'transpose (@var{M})}.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{triangularize}
@deffn {Function} triangularize (@var{M})

Returns the upper triangular form of the matrix @code{M},
as produced by Gaussian elimination.
The return value is the same as @code{echelon},
except that the leading nonzero coefficient in each row is not normalized to 1.

@code{lu_factor} and @code{cholesky} are other functions which yield
triangularized matrices.

@c ===beg===
@c M: matrix ([3, 7, aa, bb], [-1, 8, 5, 2], [9, 2, 11, 4]);
@c triangularize (M);
@c ===end===
@example
@group
(%i1) M: matrix ([3, 7, aa, bb], [-1, 8, 5, 2], [9, 2, 11, 4]);
                       [  3   7  aa  bb ]
                       [                ]
(%o1)                  [ - 1  8  5   2  ]
                       [                ]
                       [  9   2  11  4  ]
@end group
@group
(%i2) triangularize (M);
             [ - 1   8         5            2      ]
             [                                     ]
(%o2)        [  0   - 74     - 56         - 22     ]
             [                                     ]
             [  0    0    626 - 74 aa  238 - 74 bb ]
@end group
@end example

@opencatbox{Categories:}
@category{Linear equations}
@category{Matrices}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{uniteigenvectors}
@anchor{ueivects}
@deffn  {Function} uniteigenvectors (@var{M})
@deffnx {Function} ueivects (@var{M})

Computes unit eigenvectors of the matrix @var{M}.
The return value is a list of lists, the first sublist of which is the
output of the @code{eigenvalues} command, and the other sublists of which are
the unit eigenvectors of the matrix corresponding to those eigenvalues
respectively.

@c COPY DESCRIPTIONS OF THOSE FLAGS HERE
The flags mentioned in the description of the
@code{eigenvectors} command have the same effects in this one as well.

When @code{knowneigvects} is @code{true}, the @code{eigen} package assumes
that the eigenvectors of the matrix are known to the user and are
stored under the global name @code{listeigvects}.  @code{listeigvects} should
be set to a list similar to the output of the @code{eigenvectors} command.

@c FOLLOWING PARAGRAPH IS IN NEED OF SERIOUS CLARIFICATION
If @code{knowneigvects} is set to @code{true} and the list of eigenvectors is
given the setting of the flag @code{nondiagonalizable} may not be correct.  If
that is the case please set it to the correct value.  The author assumes that
the user knows what he is doing and will not try to diagonalize a matrix the
eigenvectors of which do not span the vector space of the appropriate dimension.

@code{load ("eigen")} loads this function.

@code{ueivects} is a synonym for @code{uniteigenvectors}.

@opencatbox{Categories:}
@category{Package eigen}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{unitvector}
@anchor{uvect}
@deffn  {Function} unitvector (@var{x})
@deffnx {Function} uvect (@var{x})

Returns @math{@var{x}/norm(@var{x})};
this is a unit vector in the same direction as @var{x}.

@code{load ("eigen")} loads this function.

@code{uvect} is a synonym for @code{unitvector}.

@opencatbox{Categories:}
@category{Package eigen}
@closecatbox
@end deffn

@c NEEDS EXAMPLES

@c -----------------------------------------------------------------------------
@anchor{vectorpotential}
@deffn {Function} vectorpotential (@var{givencurl})

Returns the vector potential of a given curl vector, in the current coordinate
system.  @code{potentialzeroloc} has a similar role as for @code{potential}, but
the order of the left-hand sides of the equations must be a cyclic permutation
of the coordinate variables.

@opencatbox{Categories:}
@category{Package vect}
@closecatbox
@end deffn

@c NEEDS A LOT OF WORK: MAKE SURE THAT ALL VECTOR SIMPLIFICATION FLAGS HAVE A
@c DESCRIPTION HERE

@c -----------------------------------------------------------------------------
@anchor{vectorsimp}
@deffn {Function} vectorsimp (@var{expr})

Applies simplifications and expansions according to the following global flags:

@flushleft
@code{expandall}, @code{expanddot}, @code{expanddotplus}, @code{expandcross}, @code{expandcrossplus},
@code{expandcrosscross}, @code{expandgrad}, @code{expandgradplus}, @code{expandgradprod},
@code{expanddiv}, @code{expanddivplus}, @code{expanddivprod}, @code{expandcurl}, @code{expandcurlplus},
@code{expandcurlcurl}, @code{expandlaplacian}, @code{expandlaplacianplus},
and @code{expandlaplacianprod}.
@end flushleft

All these flags have default value @code{false}.  The @code{plus} suffix refers
to employing additivity or distributivity.  The @code{prod} suffix refers to the
expansion for an operand that is any kind of product.

@table @code
@item expandcrosscross
Simplifies 
m4_math(<<<p \sim (q \sim r)>>>, <<<p ~ (q ~ r)>>>) 
to 
m4_mathdot(<<<(p . r)q - (p . q)r>>>, <<<(p . r)*q-(p . q)*r>>>)
@item expandcurlcurl
Simplifies 
m4_math(<<<{\rm curl}\; {\rm curl}\; p>>>, <<<curl curl p>>>) 
to 
m4_mathdot(<<<{\rm grad}\; {\rm div}\; p + {\rm div}\; {\rm grad}\; p>>>, <<<grad div p + div grad p>>>)
@item expandlaplaciantodivgrad
Simplifies 
m4_math(<<<{\rm laplacian}\; p>>>,<<<laplacian p>>>) 
to 
m4_mathdot(<<<{\rm div}\; {\rm grad}\; p>>>, <<<div grad p>>>)
@item expandcross
Enables @code{expandcrossplus} and @code{expandcrosscross}.
@item expandplus
@flushleft
Enables @code{expanddotplus}, @code{expandcrossplus}, @code{expandgradplus},
@code{expanddivplus}, @code{expandcurlplus}, and @code{expandlaplacianplus}.
@end flushleft
@item expandprod
Enables @code{expandgradprod}, @code{expanddivprod}, and @code{expandlaplacianprod}.
@end table

@c EXPLAIN THE IMPORT OF THIS STATEMENT
These flags have all been declared @code{evflag}.

@c SEEMS SOME TEXT HAS GONE MISSING HERE; COMMENT OUT FRAGMENT PENDING
@c RECOVERY AND/OR RECONSTRUCTION OF THIS PARAGRAPH
@c For orthogonal curvilinear coordinates, the global variables
@c COORDINATES[[X,Y,Z]], DIMENSION[3], SF[[1,1,1]], and SFPROD[1] are set
@c by the function invocation

@opencatbox{Categories:}
@category{Package vect}
@category{Simplification functions}
@closecatbox
@end deffn

@c -----------------------------------------------------------------------------
@anchor{vect_cross}
@defvr {Option variable} vect_cross
Default value: @code{false}

@c WHAT DOES THIS MEAN EXACTLY ??
When @code{vect_cross} is @code{true}, it allows DIFF(X~Y,T) to work where
~ is defined in SHARE;VECT (where VECT_CROSS is set to @code{true}, anyway.)

@opencatbox{Categories:}
@category{Package vect}
@category{Differential calculus}
@closecatbox
@end defvr

@c -----------------------------------------------------------------------------
@anchor{zeromatrix}
@deffn {Function} zeromatrix (@var{m}, @var{n})

Returns an @var{m} by @var{n} matrix, all elements of which are zero.

@opencatbox{Categories:}
@category{Matrices}
@closecatbox
@end deffn

