@menu
* Introduction to lapack::
* Functions and Variables for lapack::
@end menu

@node Introduction to lapack, Functions and Variables for lapack, Package lapack, Package lapack
@section Introduction to lapack

@code{lapack} is a Common Lisp translation (via the program @code{f2cl}) of the Fortran library LAPACK,
as obtained from the SLATEC project.

@opencatbox{Categories:}
@category{Numerical methods}
@category{Share packages}
@category{Package lapack}
@closecatbox


@node Functions and Variables for lapack, , Introduction to lapack, Package lapack
@section Functions and Variables for lapack

@anchor{dgeev}
@deffn {Function} dgeev @
@fname{dgeev} (@var{A}) @
@fname{dgeev} (@var{A}, @var{right_p}, @var{left_p})

Computes the eigenvalues and, optionally, the eigenvectors of a matrix @var{A}.
All elements of @var{A} must be integer or floating point numbers.
@var{A} must be square (same number of rows and columns).
@var{A} might or might not be symmetric.

To make use of this function, you must load the LaPack package via
@code{load("lapack")}.

@code{dgeev(@var{A})} computes only the eigenvalues of @var{A}.
@code{dgeev(@var{A}, @var{right_p}, @var{left_p})} computes the eigenvalues of @var{A}
and the right eigenvectors when @math{@var{right_p} = @code{true}}
and the left eigenvectors when @math{@var{left_p} = @code{true}}.

A list of three items is returned.
The first item is a list of the eigenvalues.
The second item is @code{false} or the matrix of right eigenvectors.
The third item is @code{false} or the matrix of left eigenvectors.

The right eigenvector
m4_math(<<<v_j>>>,<<<v_j>>>)
(the @math{j}-th column of the right eigenvector matrix) satisfies

m4_displaymath(
<<<\mathbf{A} v_j = \lambda_j v_j>>>,
<<<@math{A . v_j = lambda_j . v_j}>>>,
<<<{\bf A} v_j = \lambda_j v_j>>>
)

where
m4_math(<<<\lambda_j>>>,<<<@math{lambda_j}>>>)
is the corresponding eigenvalue.
The left eigenvector
m4_math(<<<u_j>>>,<<<u_j>>>)
(the @math{j}-th column of the left eigenvector matrix) satisfies

m4_displaymath(
<<<u_j^\mathbf{H} \mathbf{A} = \lambda_j u_j^\mathbf{H}>>>,
<<<@math{u_j^H . A = lambda_j . u_j^H}>>>,
<<<u_j^{\bf H} {\bf A} = \lambda_j u_j^{\bf H}>>>
)

where
m4_math(<<<u_j^\mathbf{H}>>>,<<<u_j^H>>>, <<<u_j^{\bf H}>>>)
denotes the conjugate transpose of
m4_mathdot(<<<u_j>>>,<<<u_j>>>)
For a Maxima function to compute the conjugate transpose, @pxref{ctranspose}.


The computed eigenvectors are normalized to have Euclidean norm
equal to 1, and largest component has imaginary part equal to zero.

Example:

@c ===beg===
@c load ("lapack")$
@c fpprintprec : 6;
@c M : matrix ([9.5, 1.75], [3.25, 10.45]);
@c dgeev (M);
@c [L, v, u] : dgeev (M, true, true);
@c D : apply (diag_matrix, L);
@c M . v - v . D;
@c transpose (u) . M - D . transpose (u);
@c ===end===
@example
(%i1) load ("lapack")$
@group
(%i2) fpprintprec : 6;
(%o2)                           6
@end group
@group
(%i3) M : matrix ([9.5, 1.75], [3.25, 10.45]);
                         [ 9.5   1.75  ]
(%o3)                    [             ]
                         [ 3.25  10.45 ]
@end group
@group
(%i4) dgeev (M);
(%o4)          [[7.54331, 12.4067], false, false]
@end group
@group
(%i5) [L, v, u] : dgeev (M, true, true);
                           [ - 0.666642  - 0.515792 ]
(%o5) [[7.54331, 12.4067], [                        ], 
                           [  0.745378   - 0.856714 ]
                                      [ - 0.856714  - 0.745378 ]
                                      [                        ]]
                                      [  0.515792   - 0.666642 ]
@end group
@group
(%i6) D : apply (diag_matrix, L);
                      [ 7.54331     0    ]
(%o6)                 [                  ]
                      [    0     12.4067 ]
@end group
@group
(%i7) M . v - v . D;
                [      0.0       - 8.88178e-16 ]
(%o7)           [                              ]
                [ - 8.88178e-16       0.0      ]
@end group
@group
(%i8) transpose (u) . M - D . transpose (u);
                     [ 0.0  - 4.44089e-16 ]
(%o8)                [                    ]
                     [ 0.0       0.0      ]
@end group
@end example

@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

@anchor{dgeqrf}
@deffn {Function} dgeqrf (@var{A})

Computes the QR decomposition of the matrix @var{A}.
All elements of @var{A} must be integer or floating point numbers.
@var{A} may or may not have the same number of rows and columns.

To make use of this function, you must load the LaPack package via
@code{load("lapack")}.

The real square matrix
m4_math(<<<\mathbf{A}>>>, <<<A>>>, <<<{\bf A}>>>)
can be decomposed as

m4_displaymath(
<<<\mathbf{A} = \mathbf{Q}\mathbf{R}>>>,
<<<A = QR>>>,
<<<{\bf A} = {\bf Q} {\bf R}>>>)

where
m4_math(<<<{\bf Q}>>>, <<<Q>>>)
is a square orthogonal matrix with the same number of rows as
m4_math(<<<\mathbf{A}>>>, <<<A>>>, <<<{\bf A}>>>)
and
m4_math(<<<{\bf R}>>>, <<<R>>>)
is an upper triangular matrix and is the same size as
m4_mathdot(<<<{\bf A}>>>, A)

A list of two items is returned.
The first item is the matrix
m4_mathdot(<<<{\bf Q}>>>, Q)
The second item is the matrix
m4_mathcomma(<<<{\bf R}>>>, R)
The product @math{Q . R}, where "." is the noncommutative multiplication operator,
is equal to @var{A} (ignoring floating point round-off errors).

@c ===beg===
@c load ("lapack")$
@c fpprintprec : 6;
@c M : matrix ([1, -3.2, 8], [-11, 2.7, 5.9]);
@c [q, r] : dgeqrf (M);
@c q . r - M;
@c mat_norm (%, 1);
@c ===end===
@example
(%i1) load ("lapack")$
@group
(%i2) fpprintprec : 6;
(%o2)                           6
@end group
@group
(%i3) M : matrix ([1, -3.2, 8], [-11, 2.7, 5.9]);
                      [  1    - 3.2   8  ]
(%o3)                 [                  ]
                      [ - 11   2.7   5.9 ]
@end group
@group
(%i4) [q, r] : dgeqrf (M);
       [ - 0.0905357  0.995893  ]
(%o4) [[                        ], 
       [  0.995893    0.0905357 ]
                               [ - 11.0454   2.97863   5.15148 ]
                               [                               ]]
                               [    0.0     - 2.94241  8.50131 ]
@end group
@group
(%i5) q . r - M;
         [ - 7.77156e-16   1.77636e-15   - 8.88178e-16 ]
(%o5)    [                                             ]
         [      0.0       - 1.33227e-15   8.88178e-16  ]
@end group
@group
(%i6) mat_norm (%, 1);
(%o6)                      3.10862e-15
@end group
@end example

@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

@anchor{dgesv}
@deffn {Function} dgesv (@var{A}, @var{b})

Computes the solution @var{x} of the linear equation
m4_mathcomma(<<<{\bf A} x = b>>>, <<<@var{A} @var{x} = @var{b}>>>)
where
m4_math(<<<{\bf A}>>>, <<<@var{A}>>>)
is a square matrix, and @math{b} is a matrix of the same number of rows
as
m4_math(<<<{\bf A}>>>, <<<@var{A}>>>)
and any number of columns.
The return value @math{x} is the same size as @math{b}.

To make use of this function, you must load the LaPack package via
@code{load("lapack")}.

The elements of @var{A} and @var{b} must evaluate to real floating point numbers via @code{float};
thus elements may be any numeric type, symbolic numerical constants, or expressions which evaluate to floats.
The elements of @var{x} are always floating point numbers.
All arithmetic is carried out as floating point operations.

@code{dgesv} computes the solution via the LU decomposition of @var{A}.

Examples:

@code{dgesv} computes the solution of the linear equation @math{@var{A} @var{x} = @var{b}}.

@c ===beg===
@c load("lapack")$
@c A : matrix ([1, -2.5], [0.375, 5]);
@c b : matrix ([1.75], [-0.625]);
@c x : dgesv (A, b);
@c dlange (inf_norm, b - A . x);
@c ===end===
@example
(%i1) load("lapack")$
@group
(%i2) A : matrix ([1, -2.5], [0.375, 5]);
                        [   1    - 2.5 ]
(%o2)                   [              ]
                        [ 0.375    5   ]
@end group
@group
(%i3) b : matrix ([1.75], [-0.625]);
                           [  1.75   ]
(%o3)                      [         ]
                           [ - 0.625 ]
@end group
@group
(%i4) x : dgesv (A, b);
                    [  1.2105263157894737   ]
(%o4)               [                       ]
                    [ - 0.21578947368421053 ]
@end group
@group
(%i5) dlange (inf_norm, b - A . x);
(%o5)                          0.0
@end group
@end example

@var{b} is a matrix with the same number of rows as @var{A} and any number of columns.
@var{x} is the same size as @var{b}.

@c ===beg===
@c load ("lapack")$
@c A : matrix ([1, -0.15], [1.82, 2]);
@c b : matrix ([3.7, 1, 8], [-2.3, 5, -3.9]);
@c x : dgesv (A, b);
@c dlange (inf_norm, b - A . x);
@c ===end===
@example
(%i1) load ("lapack")$
@group
(%i2) A : matrix ([1, -0.15], [1.82, 2]);
                        [  1    - 0.15 ]
(%o2)                   [              ]
                        [ 1.82    2    ]
@end group
@group
(%i3) b : matrix ([3.7, 1, 8], [-2.3, 5, -3.9]);
                       [  3.7   1    8   ]
(%o3)                  [                 ]
                       [ - 2.3  5  - 3.9 ]
@end group
@group
(%i4) x : dgesv (A, b);
(%o4) 
 [ 3.1038275406951175   1.2098548174219095  6.7817861856577215  ]
 [                                                              ]
 [ - 3.974483062032557  1.3990321161460624  - 8.121425428948527 ]
@end group
@group
(%i5) dlange (inf_norm, b - A . x);
(%o5)                1.1102230246251565e-15
@end group
@end example

The elements of @var{A} and @var{b} must evaluate to real floating point numbers.

@c ===beg===
@c load ("lapack")$
@c A : matrix ([5, -%pi], [1b0, 11/17]);
@c b : matrix ([%e], [sin(1)]);
@c x : dgesv (A, b);
@c dlange (inf_norm, b - A . x);
@c ===end===
@example
(%i1) load ("lapack")$
@group
(%i2) A : matrix ([5, -%pi], [1b0, 11/17]);
                        [   5    - %pi ]
                        [              ]
(%o2)                   [         11   ]
                        [ 1.0b0   --   ]
                        [         17   ]
@end group
@group
(%i3) b : matrix ([%e], [sin(1)]);
                           [   %e   ]
(%o3)                      [        ]
                           [ sin(1) ]
@end group
@group
(%i4) x : dgesv (A, b);
                     [ 0.6903756431559864  ]
(%o4)                [                     ]
                     [ 0.23351098255295172 ]
@end group
@group
(%i5) dlange (inf_norm, b - A . x);
(%o5)                 2.220446049250313e-16
@end group
@end example

@opencatbox{Categories:}
@category{Package lapack}
@category{Linear equations}
@closecatbox

@end deffn

@anchor{dgesvd}
@deffn {Function} dgesvd @
@fname{dgesvd} (@var{A}) @
@fname{dgesvd} (@var{A}, @var{left_p}, @var{right_p})

Computes the singular value decomposition (SVD) of a matrix @var{A},
comprising the singular values and, optionally, the left and right singular vectors.
All elements of @var{A} must be integer or floating point numbers.
@var{A} might or might not be square (same number of rows and columns).

To make use of this function, you must load the LaPack package via
@code{load("lapack")}.

Let @math{m} be the number of rows, and @math{n} the number of columns of @var{A}.
The singular value decomposition of
m4_math(<<<\mathbf{A}>>>,<<<@math{A}>>>, <<<{\bf A}>>>)
comprises three matrices,
m4_mathcomma(<<<\mathbf{U}>>>,<<<U>>>,<<<{\bf U}>>>)
m4_mathcomma(<<<\mathbf{\Sigma}>>>,<<<@math{Sigma}>>>, <<<{\bf \Sigma}>>>)
and
m4_mathcomma(<<<\mathbf{V}>>>, <<<@math{V}>>>, <<<{\bf V}>>>)
such that

@c this code breaks texi2pdf: @math{@var{A} = @var{U} . @var{Sigma} . @var{V^T}}
@c @math{@var{A} = @var{U} . @var{Sigma} . @var{V}^T}

m4_displaymath(
<<<\mathbf{A} = \mathbf{U} \mathbf{\Sigma} \mathbf{V}^T>>>,
<<<@math{{A} = {U} . {Sigma} . {V}^T}>>>,
<<<{\bf A} = {\bf U} {\bf \Sigma} {\bf V}^T>>>)


where
m4_math(<<<\mathbf{U}>>>, <<<@math{U}>>>, <<<{\bf U}>>>)
is an @math{m}-by-@math{m} unitary matrix,
m4_math(<<<\mathbf{\Sigma}>>>, <<<@math{Sigma}>>>, <<<{\bf\Sigma}>>>)
is an @math{m}-by-@math{n} diagonal matrix,
and
m4_math(<<<\mathbf{V}>>>, <<<@math{V}>>>, <<<{\bf V}>>>)
is an @math{n}-by-@math{n} unitary matrix.

Let
m4_math(<<<\mathbf{\sigma}_i>>>, <<<@math{sigma[i]}>>>, <<<{\bf \sigma}_i>>>)
be a diagonal element of
m4_mathcomma(<<<\mathbf{\Sigma}>>>, <<<@math{Sigma}>>>, <<<{\bf \Sigma}>>>)
that is,
m4_mathdot(<<<\mathbf{\Sigma}_{ii} = \sigma_i>>>,
<<<@math{@var{Sigma}[i, i]  = @var{sigma}[i]}>>>,
<<<{\bf \Sigma}_{ii}  = \sigma_i>>>)
The elements
m4_math(<<<\sigma_i>>>, <<<@math{sigma[i]}>>>)
are the so-called singular values of
m4_mathpunc(<<<;>>>, <<<\mathbf{A}>>>, <<<A>>>, <<<{\bf A}>>>)
these are real and nonnegative, and returned in descending order.
The first
m4_math(<<<\min(m, n)>>>, <<<min(m, n)>>>)
columns of
m4_math(<<<\mathbf{U}>>>, <<<@math{U}>>>, <<<{\bf U}>>>)
and
m4_math(<<<\mathbf{V}>>>, <<<@math{V}>>>, <<<{\bf V}>>>)
are the left and right singular vectors of
m4_mathdot(<<<\mathbf{A}>>>, <<<@math{A}>>>, <<<{\bf A}>>>)
Note that @code{dgesvd} returns the transpose of
m4_mathcomma(<<<\mathbf{V}>>>, <<<@math{V}>>>, <<<{\bf V}>>>)
not
m4_math(<<<\mathbf{V}>>>, <<<@math{V}>>>, <<<{\bf V}>>>)
itself.

@code{dgesvd(@var{A})} computes only the singular values of @var{A}.
@code{dgesvd(@var{A}, @var{left_p}, @var{right_p})} computes the singular values of @var{A}
and the left singular vectors when @math{@var{left_p} = @code{true}}
and the right singular vectors when @math{@var{right_p} = @code{true}}.

A list of three items is returned.
The first item is a list of the singular values.
The second item is @code{false} or the matrix of left singular vectors.
The third item is @code{false} or the matrix of right singular vectors.

Example:

@c ===beg===
@c load ("lapack")$
@c fpprintprec : 6;
@c M: matrix([1, 2, 3], [3.5, 0.5, 8], [-1, 2, -3], [4, 9, 7]);
@c dgesvd (M);
@c [sigma, U, VT] : dgesvd (M, true, true);
@c m : length (U);
@c n : length (VT);
@c Sigma:
@c   genmatrix(lambda ([i, j], if i=j then sigma[i] else 0),
@c             m, n);
@c U . Sigma . VT - M;
@c transpose (U) . U;
@c VT . transpose (VT);
@c ===end===
@example
(%i1) load ("lapack")$
@group
(%i2) fpprintprec : 6;
(%o2)                           6
@end group
@group
(%i3) M: matrix([1, 2, 3], [3.5, 0.5, 8], [-1, 2, -3], [4, 9, 7]);
                        [  1    2    3  ]
                        [               ]
                        [ 3.5  0.5   8  ]
(%o3)                   [               ]
                        [ - 1   2   - 3 ]
                        [               ]
                        [  4    9    7  ]
@end group
@group
(%i4) dgesvd (M);
(%o4)     [[14.4744, 6.38637, 0.452547], false, false]
@end group
@group
(%i5) [sigma, U, VT] : dgesvd (M, true, true);
(%o5) [[14.4744, 6.38637, 0.452547], 
[ - 0.256731  0.00816168   0.959029    - 0.119523 ]
[                                                 ]
[ - 0.526456   0.672116   - 0.206236   - 0.478091 ]
[                                                 ], 
[  0.107997   - 0.532278  - 0.0708315  - 0.83666  ]
[                                                 ]
[ - 0.803287  - 0.514659  - 0.180867    0.239046  ]
[ - 0.374486  - 0.538209  - 0.755044 ]
[                                    ]
[  0.130623   - 0.836799    0.5317   ]]
[                                    ]
[ - 0.917986   0.100488    0.383672  ]
@end group
@group
(%i6) m : length (U);
(%o6)                           4
@end group
@group
(%i7) n : length (VT);
(%o7)                           3
@end group
@group
(%i8) Sigma:
  genmatrix(lambda ([i, j], if i=j then sigma[i] else 0),
            m, n);
                 [ 14.4744     0        0     ]
                 [                            ]
                 [    0     6.38637     0     ]
(%o8)            [                            ]
                 [    0        0     0.452547 ]
                 [                            ]
                 [    0        0        0     ]
@end group
@group
(%i9) U . Sigma . VT - M;
          [  1.11022e-15        0.0       1.77636e-15 ]
          [                                           ]
          [  1.33227e-15    1.66533e-15       0.0     ]
(%o9)     [                                           ]
          [ - 4.44089e-16  - 8.88178e-16  4.44089e-16 ]
          [                                           ]
          [  8.88178e-16    1.77636e-15   8.88178e-16 ]
@end group
@group
(%i10) transpose (U) . U;
       [     1.0      5.55112e-17    2.498e-16     2.77556e-17  ]
       [                                                        ]
       [ 5.55112e-17      1.0       5.55112e-17    4.16334e-17  ]
(%o10) [                                                        ]
       [  2.498e-16   5.55112e-17       1.0       - 2.08167e-16 ]
       [                                                        ]
       [ 2.77556e-17  4.16334e-17  - 2.08167e-16       1.0      ]
@end group
@group
(%i11) VT . transpose (VT);
          [      1.0           0.0      - 5.55112e-17 ]
          [                                           ]
(%o11)    [      0.0           1.0       5.55112e-17  ]
          [                                           ]
          [ - 5.55112e-17  5.55112e-17       1.0      ]
@end group
@end example

@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

@anchor{dlange}
@anchor{zlange}
@deffn {Function} dlange (@var{norm}, @var{A})
@deffnx {Function} zlange (@var{norm}, @var{A})

Computes a norm or norm-like function of the matrix @var{A}.  If
@var{A} is a real matrix, use @code{dlange}.  For a matrix with
complex elements, use @code{zlange}.

To make use of this function, you must load the LaPack package via
@code{load("lapack")}.

@code{norm} specifies the kind of norm to be computed:
@table @code
@item max
Compute
m4_math(<<<\max(|{\bf A}_{ij}|)>>>, <<<max(abs(A(i, j)))>>>)
where @math{i} and @math{j} range over
the rows and columns, respectively, of
m4_mathdot(<<<{\bf A}>>>, <<<A>>>)
Note that this function is not a proper matrix norm.
@item one_norm
Compute the
m4_math(<<<L_1>>>, <<<L[1]>>>)
norm of
m4_mathcomma(<<<{\bf A}>>>, <<<A>>>)
that is, the maximum of the sum of the absolute value of elements in each column.
@item inf_norm
Compute the
m4_math(<<<L_\infty>>>, <<<L[inf]>>>)
norm of
m4_mathcomma(<<<{\bf A}>>>, <<<A>>>)
that is, the maximum of the sum of the absolute value of elements in each row.
@item frobenius
Compute the Frobenius norm of
m4_mathcomma(<<<{\bf A}>>>, <<<A>>>)
that is, the square root of the sum of squares of the matrix elements.
@end table

@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

@anchor{dgemm}
@deffn {Function} dgemm @
@fname{dgemm} (@var{A}, @var{B}) @
@fname{dgemm} (@var{A}, @var{B}, @var{options})
Compute the product of two matrices and optionally add the product to
a third matrix.

In the simplest form, @code{dgemm(@var{A}, @var{B})} computes the
product of the two real matrices, @var{A} and @var{B}.

To make use of this function, you must load the LaPack package via
@code{load("lapack")}.

In the second form, @code{dgemm} computes
m4_math(<<<\alpha {\bf A} {\bf B} + \beta {\bf C}>>>,
<<<@var{alpha} * @var{A} * @var{B} + @var{beta} * @var{C}>>>)
where
m4_mathcomma(<<<{\bf A}>>>, <<<@var{A}>>>)
m4_mathcomma(<<<{\bf B}>>>, <<<@var{B}>>>)
and
m4_math(<<<{\bf C}>>>, <<<@var{C}>>>)
are real matrices of the appropriate sizes and
m4_math(<<<\alpha>>>, <<<alpha>>>)
and
m4_math(<<<\beta>>>, <<<beta>>>)
are real numbers.  Optionally,
m4_math(<<<{\bf A}>>>, <<<@var{A}>>>)
and/or
m4_math(<<<{\bf B}>>>, <<<@var{B}>>>)
can
be transposed before computing the product.  The extra parameters are
specified by optional keyword arguments: The keyword arguments are
optional and may be specified in any order.  They all take the form
@code{key=val}.  The keyword arguments are:

@table @code
@item C
The matrix
m4_math(<<<{\bf C}>>>, <<<@var{C}>>>)
that should be added.  The default is @code{false},
which means no matrix is added.
@item alpha
The product of
m4_math(<<<{\bf A}>>>, <<<@var{A}>>>)
and
m4_math(<<<{\bf B}>>>, <<<@var{B}>>>)
is multiplied by this value.  The
default is 1.
@item beta
If a matrix
m4_math(<<<{\bf C}>>>, <<<@var{C}>>>)
is given, this value multiplies
m4_math(<<<{\bf C}>>>, <<<@var{C}>>>)
before it
is added.  The default value is 0, which implies that
m4_math(<<<{\bf C}>>>, <<<@var{C}>>>)
is not
added, even if
m4_math(<<<{\bf C}>>>, <<<@var{C}>>>)
is given.  Hence, be sure to specify a non-zero
value for
m4_mathdot(<<<\beta>>>, <<<@math{beta}>>>)
@item transpose_a
If @code{true}, the transpose of
m4_math(<<<{\bf A}>>>, <<<@var{A}>>>)
is used instead of
m4_math(<<<{\bf A}>>>, <<<@var{A}>>>)
for the product.  The default is @code{false}.
@item transpose_b
If @code{true}, the transpose of
m4_math(<<<{\bf B}>>>, <<<@var{B}>>>)
is used instead of
m4_math(<<<{\bf B}>>>, <<<@var{B}>>>)
for the product.  The default is @code{false}.
@end table

@c ===beg===
@c load ("lapack")$
@c A : matrix([1,2,3],[4,5,6],[7,8,9]);
@c B : matrix([-1,-2,-3],[-4,-5,-6],[-7,-8,-9]);
@c C : matrix([3,2,1],[6,5,4],[9,8,7]);
@c dgemm(A,B);
@c A . B;
@c dgemm(A,B,transpose_a=true);
@c transpose(A) . B;
@c dgemm(A,B,c=C,beta=1);
@c A . B + C;
@c dgemm(A,B,c=C,beta=1, alpha=-1);
@c -A . B + C;
@c ===end===
@example
(%i1) load ("lapack")$
@group
(%i2) A : matrix([1,2,3],[4,5,6],[7,8,9]);
                           [ 1  2  3 ]
                           [         ]
(%o2)                      [ 4  5  6 ]
                           [         ]
                           [ 7  8  9 ]
@end group
@group
(%i3) B : matrix([-1,-2,-3],[-4,-5,-6],[-7,-8,-9]);
                        [ - 1  - 2  - 3 ]
                        [               ]
(%o3)                   [ - 4  - 5  - 6 ]
                        [               ]
                        [ - 7  - 8  - 9 ]
@end group
@group
(%i4) C : matrix([3,2,1],[6,5,4],[9,8,7]);
                           [ 3  2  1 ]
                           [         ]
(%o4)                      [ 6  5  4 ]
                           [         ]
                           [ 9  8  7 ]
@end group
@group
(%i5) dgemm(A,B);
                  [ - 30.0   - 36.0   - 42.0  ]
                  [                           ]
(%o5)             [ - 66.0   - 81.0   - 96.0  ]
                  [                           ]
                  [ - 102.0  - 126.0  - 150.0 ]
@end group
@group
(%i6) A . B;
                     [ - 30   - 36   - 42  ]
                     [                     ]
(%o6)                [ - 66   - 81   - 96  ]
                     [                     ]
                     [ - 102  - 126  - 150 ]
@end group
@group
(%i7) dgemm(A,B,transpose_a=true);
                  [ - 66.0  - 78.0   - 90.0  ]
                  [                          ]
(%o7)             [ - 78.0  - 93.0   - 108.0 ]
                  [                          ]
                  [ - 90.0  - 108.0  - 126.0 ]
@end group
@group
(%i8) transpose(A) . B;
                     [ - 66  - 78   - 90  ]
                     [                    ]
(%o8)                [ - 78  - 93   - 108 ]
                     [                    ]
                     [ - 90  - 108  - 126 ]
@end group
@group
(%i9) dgemm(A,B,c=C,beta=1);
                  [ - 27.0  - 34.0   - 41.0  ]
                  [                          ]
(%o9)             [ - 60.0  - 76.0   - 92.0  ]
                  [                          ]
                  [ - 93.0  - 118.0  - 143.0 ]
@end group
@group
(%i10) A . B + C;
                     [ - 27  - 34   - 41  ]
                     [                    ]
(%o10)               [ - 60  - 76   - 92  ]
                     [                    ]
                     [ - 93  - 118  - 143 ]
@end group
@group
(%i11) dgemm(A,B,c=C,beta=1, alpha=-1);
                     [ 33.0   38.0   43.0  ]
                     [                     ]
(%o11)               [ 72.0   86.0   100.0 ]
                     [                     ]
                     [ 111.0  134.0  157.0 ]
@end group
@group
(%i12) -A . B + C;
                        [ 33   38   43  ]
                        [               ]
(%o12)                  [ 72   86   100 ]
                        [               ]
                        [ 111  134  157 ]
@end group
@end example
@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

@anchor{zgeev}
@deffn {Function} zgeev @
@fname{zgeev} (@var{A}) @
@fname{zgeev} (@var{A}, @var{right_p}, @var{left_p})

Like @mrefcomma{dgeev} but the matrix
m4_math(<<<{\bf A}>>>, <<<@var{A}>>>)
is complex.

To make use of this function, you must load the LaPack package via
@code{load("lapack")}.

@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

@anchor{zheev}
@deffn {Function} zheev @
@fname{zheev} (@var{A}) @
@fname{zheev} (@var{A}, @var{eigvec_p})

Like @mrefcomma{dgeev} but the matrix
m4_math(<<<{\bf A}>>>, <<<@var{A}>>>)
is assumed to be a square
complex Hermitian matrix. If @var{eigvec_p} is @code{true}, then the
eigenvectors of the matrix are also computed.

To make use of this function, you must load the LaPack package via
@code{load("lapack")}.

No check is made that the matrix
m4_math(<<<{\bf A}>>>, <<<@var{A}>>>)
is, in fact, Hermitian.

A list of two items is returned, as in @code{dgeev}: a list of
eigenvalues, and @code{false} or the matrix of the eigenvectors.

@c ===beg===
@c load("lapack")$
@c M: matrix(
@c      [9.14 +%i*0.00 ,   -4.37 -%i*9.22 ,  -1.98 -%i*1.72 ,  -8.96 -%i*9.50],
@c      [-4.37 +%i*9.22 ,  -3.35 +%i*0.00 ,   2.25 -%i*9.51 ,   2.57 +%i*2.40],
@c      [-1.98 +%i*1.72 ,   2.25 +%i*9.51 ,  -4.82 +%i*0.00 ,  -3.24 +%i*2.04],
@c      [-8.96 +%i*9.50 ,   2.57 -%i*2.40 ,  -3.24 -%i*2.04 ,   8.44 +%i*0.00]);
@c zheev(M);
@c E: zheev(M,true)$
@c E[1];
@c E[2];
@c ===end===
@example
(%i1) load("lapack")$
@group
(%i2) M: matrix(
     [9.14 +%i*0.00 ,   -4.37 -%i*9.22 ,  -1.98 -%i*1.72 ,  -8.96 -%i*9.50],
     [-4.37 +%i*9.22 ,  -3.35 +%i*0.00 ,   2.25 -%i*9.51 ,   2.57 +%i*2.40],
     [-1.98 +%i*1.72 ,   2.25 +%i*9.51 ,  -4.82 +%i*0.00 ,  -3.24 +%i*2.04],
     [-8.96 +%i*9.50 ,   2.57 -%i*2.40 ,  -3.24 -%i*2.04 ,   8.44 +%i*0.00]);
               [      9.14      ]         [ - 9.22 %i - 4.37 ]
               [                ]         [                  ]
               [ 9.22 %i - 4.37 ]         [      - 3.35      ]
(%o2)  Col 1 = [                ] Col 2 = [                  ]
               [ 1.72 %i - 1.98 ]         [  9.51 %i + 2.25  ]
               [                ]         [                  ]
               [ 9.5 %i - 8.96  ]         [  2.57 - 2.4 %i   ]
                 [ - 1.72 %i - 1.98 ]         [ - 9.5 %i - 8.96 ]
                 [                  ]         [                 ]
                 [  2.25 - 9.51 %i  ]         [  2.4 %i + 2.57  ]
         Col 3 = [                  ] Col 4 = [                 ]
                 [      - 4.82      ]         [ 2.04 %i - 3.24  ]
                 [                  ]         [                 ]
                 [ - 2.04 %i - 3.24 ]         [      8.44       ]
@end group
@group
(%i3) zheev(M);
(%o3) [[- 16.004746472094734, - 6.764970154793324, 
                   6.6657114535070985, 25.51400517338097], false]
@end group
(%i4) E: zheev(M,true)$
@group
(%i5) E[1];
(%o5) [- 16.004746472094737, - 6.764970154793325, 
                           6.665711453507101, 25.514005173380962]
@end group
@group
(%i6) E[2];
               [  0.26746505331727455 %i + 0.21754535866650165  ]
               [                                                ]
               [  0.002696730886619885 %i + 0.6968836773391712  ]
(%o6)  Col 1 = [                                                ]
               [ - 0.6082406376714117 %i - 0.012106142926979313 ]
               [                                                ]
               [              0.15930818580950368               ]
         [  0.26449374706674444 %i + 0.4773693349937472   ]
         [                                                ]
         [ - 0.28523890360316206 %i - 0.14143627420116733 ]
 Col 2 = [                                                ]
         [  0.2654607680986639 %i + 0.44678181171841735   ]
         [                                                ]
         [               0.5750762708542709               ]
         [  0.28106497673059216 %i - 0.13352639282451817  ]
         [                                                ]
         [  0.28663101328695556 %i - 0.4536971347853274   ]
 Col 3 = [                                                ]
         [ - 0.29336843237542953 %i - 0.49549724255410565 ]
         [                                                ]
         [               0.5325337537576771               ]
         [ - 0.5737316575503476 %i - 0.39661467994277055 ]
         [                                               ]
         [ 0.018265026190214573 %i + 0.35305577043870173 ]
 Col 4 = [                                               ]
         [ 0.16737009000854253 %i + 0.01476684746229564  ]
         [                                               ]
         [              0.6002632636961784               ]
@end group
@end example

@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

