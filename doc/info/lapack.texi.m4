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

The right eigenvector @math{v(j)} (the @math{j}-th column of the right eigenvector matrix) satisfies

@math{A . v(j) = lambda(j) . v(j)}

where @math{lambda(j)} is the corresponding eigenvalue.
The left eigenvector @math{u(j)} (the @math{j}-th column of the left eigenvector matrix) satisfies

@math{u(j)**H . A = lambda(j) . u(j)**H}

where @math{u(j)**H} denotes the conjugate transpose of @math{u(j)}.
The Maxima function @code{ctranspose} computes the conjugate transpose.

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
(%i2) fpprintprec : 6;
(%o2)                           6
(%i3) M : matrix ([9.5, 1.75], [3.25, 10.45]);
                         [ 9.5   1.75  ]
(%o3)                    [             ]
                         [ 3.25  10.45 ]
(%i4) dgeev (M);
(%o4)          [[7.54331, 12.4067], false, false]
(%i5) [L, v, u] : dgeev (M, true, true);
                           [ - .666642  - .515792 ]
(%o5) [[7.54331, 12.4067], [                      ], 
                           [  .745378   - .856714 ]
                                        [ - .856714  - .745378 ]
                                        [                      ]]
                                        [  .515792   - .666642 ]
(%i6) D : apply (diag_matrix, L);
                      [ 7.54331     0    ]
(%o6)                 [                  ]
                      [    0     12.4067 ]
(%i7) M . v - v . D;
                [      0.0       - 8.88178E-16 ]
(%o7)           [                              ]
                [ - 8.88178E-16       0.0      ]
(%i8) transpose (u) . M - D . transpose (u);
                     [ 0.0  - 4.44089E-16 ]
(%o8)                [                    ]
                     [ 0.0       0.0      ]
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

A list of two items is returned.
The first item is the matrix @var{Q}, which is a square, orthonormal matrix
which has the same number of rows as @var{A}.
The second item is the matrix @var{R}, which is the same size as @var{A},
and which has all elements equal to zero below the diagonal.
The product @code{@var{Q} . @var{R}}, where "." is the noncommutative multiplication operator,
is equal to @var{A} (ignoring floating point round-off errors).

@c ===beg===
@c load ("lapack")$
@c fpprintprec : 6;
@c M : matrix ([1, -3.2, 8], [-11, 2.7, 5.9]);
@c [q, r] : dgeqrf (M);
@c q . r - M;
@c mat_norm (%);
@c ===end===
@example
(%i1) load ("lapack") $
(%i2) fpprintprec : 6 $
(%i3) M : matrix ([1, -3.2, 8], [-11, 2.7, 5.9]) $
(%i4) [q, r] : dgeqrf (M);
       [ - .0905357  .995893  ]
(%o4) [[                      ], 
       [  .995893    .0905357 ]
                               [ - 11.0454   2.97863   5.15148 ]
                               [                               ]]
                               [     0      - 2.94241  8.50131 ]
(%i5) q . r - M;
         [ - 7.77156E-16   1.77636E-15   - 8.88178E-16 ]
(%o5)    [                                             ]
         [      0.0       - 1.33227E-15   8.88178E-16  ]
(%i6) mat_norm (%, 1);
(%o6)                      3.10862E-15
@end example

@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

@anchor{dgesv}
@deffn {Function} dgesv (@var{A}, @var{b})

Computes the solution @var{x} of the linear equation @math{@var{A} @var{x} = @var{b}},
where @var{A} is a square matrix, and @var{b} is a matrix of the same number of rows
as @var{A} and any number of columns.
The return value @var{x} is the same size as @var{b}.

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
@c A : matrix ([1, -2.5], [0.375, 5]);
@c b : matrix ([1.75], [-0.625]);
@c x : dgesv (A, b);
@c dlange (inf_norm, b - A . x);
@c ===end===
@example
(%i1) A : matrix ([1, -2.5], [0.375, 5]);
                               [   1    - 2.5 ]
(%o1)                          [              ]
                               [ 0.375    5   ]
(%i2) b : matrix ([1.75], [-0.625]);
                                  [  1.75   ]
(%o2)                             [         ]
                                  [ - 0.625 ]
(%i3) x : dgesv (A, b);
                            [  1.210526315789474  ]
(%o3)                       [                     ]
                            [ - 0.215789473684211 ]
(%i4) dlange (inf_norm, b - A.x);
(%o4)                                 0.0
@end example

@var{b} is a matrix with the same number of rows as @var{A} and any number of columns.
@var{x} is the same size as @var{b}.

@c ===beg===
@c A : matrix ([1, -0.15], [1.82, 2]);
@c b : matrix ([3.7, 1, 8], [-2.3, 5, -3.9]);
@c x : dgesv (A, b);
@c dlange (inf_norm, b - A . x);
@c ===end===
@example
(%i1) A : matrix ([1, -0.15], [1.82, 2]);
                               [  1    - 0.15 ]
(%o1)                          [              ]
                               [ 1.82    2    ]
(%i2) b : matrix ([3.7, 1, 8], [-2.3, 5, -3.9]);
                              [  3.7   1    8   ]
(%o2)                         [                 ]
                              [ - 2.3  5  - 3.9 ]
(%i3) x : dgesv (A, b);
      [  3.103827540695117  1.20985481742191    6.781786185657722 ]
(%o3) [                                                           ]
      [ -3.974483062032557  1.399032116146062  -8.121425428948527 ]
(%i4) dlange (inf_norm, b - A . x);
(%o4)                       1.1102230246251565E-15
@end example

The elements of @var{A} and @var{b} must evaluate to real floating point numbers.

@c ===beg===
@c A : matrix ([5, -%pi], [1b0, 11/17]);
@c b : matrix ([%e], [sin(1)]);
@c x : dgesv (A, b);
@c dlange (inf_norm, b - A . x);
@c ===end===
@example
(%i1) A : matrix ([5, -%pi], [1b0, 11/17]);
                               [   5    - %pi ]
                               [              ]
(%o1)                          [         11   ]
                               [ 1.0b0   --   ]
                               [         17   ]
(%i2) b : matrix ([%e], [sin(1)]);
                                  [   %e   ]
(%o2)                             [        ]
                                  [ sin(1) ]
(%i3) x : dgesv (A, b);
                             [ 0.690375643155986 ]
(%o3)                        [                   ]
                             [ 0.233510982552952 ]
(%i4) dlange (inf_norm, b - A . x);
(%o4)                        2.220446049250313E-16
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
The singular value decomposition of @var{A} comprises three matrices,
@var{U}, @var{Sigma}, and @var{V^T},
such that

@c this code breaks texi2pdf: @math{@var{A} = @var{U} . @var{Sigma} . @var{V^T}}
@math{@var{A} = @var{U} . @var{Sigma} . @var{V}^T}

where @var{U} is an @math{m}-by-@math{m} unitary matrix,
@var{Sigma} is an @math{m}-by-@math{n} diagonal matrix,
and @var{V^T} is an @math{n}-by-@math{n} unitary matrix.

Let @math{sigma[i]} be a diagonal element of @math{Sigma},
that is, @math{@var{Sigma}[i, i] = @var{sigma}[i]}.
The elements @math{sigma[i]} are the so-called singular values of @var{A};
these are real and nonnegative, and returned in descending order.
The first @math{min(m, n)} columns of @var{U} and @var{V} are
the left and right singular vectors of @var{A}.
Note that @code{dgesvd} returns the transpose of @var{V}, not @var{V} itself.

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
(%i2) fpprintprec : 6;
(%o2)                           6
(%i3) M: matrix([1, 2, 3], [3.5, 0.5, 8], [-1, 2, -3], [4, 9, 7]);
                        [  1    2    3  ]
                        [               ]
                        [ 3.5  0.5   8  ]
(%o3)                   [               ]
                        [ - 1   2   - 3 ]
                        [               ]
                        [  4    9    7  ]
(%i4) dgesvd (M);
(%o4)      [[14.4744, 6.38637, .452547], false, false]
(%i5) [sigma, U, VT] : dgesvd (M, true, true);
(%o5) [[14.4744, 6.38637, .452547], 
[ - .256731  .00816168   .959029    - .119523 ]
[                                             ]
[ - .526456   .672116   - .206236   - .478091 ]
[                                             ], 
[  .107997   - .532278  - .0708315  - 0.83666 ]
[                                             ]
[ - .803287  - .514659  - .180867    .239046  ]
[ - .374486  - .538209  - .755044 ]
[                                 ]
[  .130623   - .836799   0.5317   ]]
[                                 ]
[ - .917986   .100488    .383672  ]
(%i6) m : length (U);
(%o6)                           4
(%i7) n : length (VT);
(%o7)                           3
(%i8) Sigma:
        genmatrix(lambda ([i, j], if i=j then sigma[i] else 0),
                  m, n);
                  [ 14.4744     0        0    ]
                  [                           ]
                  [    0     6.38637     0    ]
(%o8)             [                           ]
                  [    0        0     .452547 ]
                  [                           ]
                  [    0        0        0    ]
(%i9) U . Sigma . VT - M;
          [  1.11022E-15        0.0       1.77636E-15 ]
          [                                           ]
          [  1.33227E-15    1.66533E-15       0.0     ]
(%o9)     [                                           ]
          [ - 4.44089E-16  - 8.88178E-16  4.44089E-16 ]
          [                                           ]
          [  8.88178E-16    1.77636E-15   8.88178E-16 ]
(%i10) transpose (U) . U;
       [     1.0      5.55112E-17    2.498E-16     2.77556E-17  ]
       [                                                        ]
       [ 5.55112E-17      1.0       5.55112E-17    4.16334E-17  ]
(%o10) [                                                        ]
       [  2.498E-16   5.55112E-17       1.0       - 2.08167E-16 ]
       [                                                        ]
       [ 2.77556E-17  4.16334E-17  - 2.08167E-16       1.0      ]
(%i11) VT . transpose (VT);
          [      1.0           0.0      - 5.55112E-17 ]
          [                                           ]
(%o11)    [      0.0           1.0       5.55112E-17  ]
          [                                           ]
          [ - 5.55112E-17  5.55112E-17       1.0      ]
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
Compute @math{max(abs(A(i, j)))} where @math{i} and @math{j} range over
the rows and columns, respectively, of @var{A}.
Note that this function is not a proper matrix norm.
@item one_norm
Compute the @math{L[1]} norm of @var{A},
that is, the maximum of the sum of the absolute value of elements in each column.
@item inf_norm
Compute the @math{L[inf]} norm of @var{A},
that is, the maximum of the sum of the absolute value of elements in each row.
@item frobenius
Compute the Frobenius norm of @var{A},
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

In the second form, @code{dgemm} computes the @math{@var{alpha} *
@var{A} * @var{B} + @var{beta} * @var{C}} where @var{A}, @var{B},
@var{C} are real matrices of the appropriate sizes and @var{alpha} and
@var{beta} are real numbers.  Optionally, @var{A} and/or @var{B} can
be transposed before computing the product.  The extra parameters are
specified by optional keyword arguments: The keyword arguments are
optional and may be specified in any order.  They all take the form
@code{key=val}.  The keyword arguments are:

@table @code
@item C
The matrix @var{C} that should be added.  The default is @code{false},
which means no matrix is added.
@item alpha
The product of @var{A} and @var{B} is multiplied by this value.  The
default is 1.
@item beta
If a matrix @var{C} is given, this value multiplies @var{C} before it
is added.  The default value is 0, which implies that @var{C} is not
added, even if @var{C} is given.  Hence, be sure to specify a non-zero
value for @var{beta}.
@item transpose_a
If @code{true}, the transpose of @var{A} is used instead of @var{A}
for the product.  The default is @code{false}.
@item transpose_b
If @code{true}, the transpose of @var{B} is used instead of @var{B}
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
@c -A . B + C
@c ===end===
@example
(%i1) load ("lapack")$
(%i2) A : matrix([1,2,3],[4,5,6],[7,8,9]);
                                  [ 1  2  3 ]
                                  [         ]
(%o2)                             [ 4  5  6 ]
                                  [         ]
                                  [ 7  8  9 ]
(%i3) B : matrix([-1,-2,-3],[-4,-5,-6],[-7,-8,-9]);
                               [ - 1  - 2  - 3 ]
                               [               ]
(%o3)                          [ - 4  - 5  - 6 ]
                               [               ]
                               [ - 7  - 8  - 9 ]
(%i4) C : matrix([3,2,1],[6,5,4],[9,8,7]);
                                  [ 3  2  1 ]
                                  [         ]
(%o4)                             [ 6  5  4 ]
                                  [         ]
                                  [ 9  8  7 ]
(%i5) dgemm(A,B);
                         [ - 30.0   - 36.0   - 42.0  ]
                         [                           ]
(%o5)                    [ - 66.0   - 81.0   - 96.0  ]
                         [                           ]
                         [ - 102.0  - 126.0  - 150.0 ]
(%i6) A . B;
                            [ - 30   - 36   - 42  ]
                            [                     ]
(%o6)                       [ - 66   - 81   - 96  ]
                            [                     ]
                            [ - 102  - 126  - 150 ]
(%i7) dgemm(A,B,transpose_a=true);
                         [ - 66.0  - 78.0   - 90.0  ]
                         [                          ]
(%o7)                    [ - 78.0  - 93.0   - 108.0 ]
                         [                          ]
                         [ - 90.0  - 108.0  - 126.0 ]
(%i8) transpose(A) . B;
                           [ - 66  - 78   - 90  ]
                           [                    ]
(%o8)                      [ - 78  - 93   - 108 ]
                           [                    ]
                           [ - 90  - 108  - 126 ]
(%i9) dgemm(A,B,c=C,beta=1);
                         [ - 27.0  - 34.0   - 41.0  ]
                         [                          ]
(%o9)                    [ - 60.0  - 76.0   - 92.0  ]
                         [                          ]
                         [ - 93.0  - 118.0  - 143.0 ]
(%i10) A . B + C;
                            [ - 27  - 34   - 41  ]
                            [                    ]
(%o10)                      [ - 60  - 76   - 92  ]
                            [                    ]
                            [ - 93  - 118  - 143 ]
(%i11) dgemm(A,B,c=C,beta=1, alpha=-1);
                            [ 33.0   38.0   43.0  ]
                            [                     ]
(%o11)                      [ 72.0   86.0   100.0 ]
                            [                     ]
                            [ 111.0  134.0  157.0 ]
(%i12) -A . B + C;
                               [ 33   38   43  ]
                               [               ]
(%o12)                         [ 72   86   100 ]
                               [               ]
                               [ 111  134  157 ]

@end example
@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

@anchor{zgeev}
@deffn {Function} zgeev @
@fname{zgeev} (@var{A}) @
@fname{zgeev} (@var{A}, @var{right_p}, @var{left_p})

Like @code{dgeev}, but the matrix @var{A} is complex.

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

Like @code{dgeev}, but the matrix @var{A} is assumed to be a square
complex Hermitian matrix. If @var{eigvec_p} is @code{true}, then the
eigenvectors of the matrix are also computed.

To make use of this function, you must load the LaPack package via
@code{load("lapack")}.

No check is made that the matrix @var{A} is, in fact, Hermitian.

A list of two items is returned, as in @code{dgeev}: a list of
eigenvalues, and @code{false} or the matrix of the eigenvectors.

@c ===beg===
@c load("lapack")
@c A: matrix(
@c      [9.14 +%i*0.00 ,   -4.37 -%i*9.22 ,  -1.98 -%i*1.72 ,  -8.96 -%i*9.50],
@c      [-4.37 +%i*9.22 ,  -3.35 +%i*0.00 ,   2.25 -%i*9.51 ,   2.57 +%i*2.40],
@c      [-1.98 +%i*1.72 ,   2.25 +%i*9.51 ,  -4.82 +%i*0.00 ,  -3.24 +%i*2.04],
@c      [-8.96 +%i*9.50 ,   2.57 -%i*2.40 ,  -3.24 -%i*2.04 ,   8.44 +%i*0.00]);
@c zheev(M);
@c E: zheev(M,true)$
@c E[1];
@c E[2];
An example of computing the eigenvalues and then eigenvalues and
eigenvectors of an Hermitian matrix.
@example
(%i1) load("lapack")$
(%i2) A: matrix(
     [9.14 +%i*0.00 ,   -4.37 -%i*9.22 ,  -1.98 -%i*1.72 ,  -8.96 -%i*9.50],
     [-4.37 +%i*9.22 ,  -3.35 +%i*0.00 ,   2.25 -%i*9.51 ,   2.57 +%i*2.40],
     [-1.98 +%i*1.72 ,   2.25 +%i*9.51 ,  -4.82 +%i*0.00 ,  -3.24 +%i*2.04],
     [-8.96 +%i*9.50 ,   2.57 -%i*2.40 ,  -3.24 -%i*2.04 ,   8.44 +%i*0.00]);
(%o2) 
  [      9.14       (- 9.22 %i) - 4.37  (- 1.72 %i) - 1.98  (- 9.5 %i) - 8.96 ]
  [                                                                           ]
  [ 9.22 %i - 4.37        - 3.35          2.25 - 9.51 %i      2.4 %i + 2.57   ]
  [                                                                           ]
  [ 1.72 %i - 1.98    9.51 %i + 2.25          - 4.82         2.04 %i - 3.24   ]
  [                                                                           ]
  [ 9.5 %i - 8.96     2.57 - 2.4 %i     (- 2.04 %i) - 3.24        8.44        ]
(%i3) zheev(A);
(%o3) [[- 16.00474647209473, - 6.764970154793324, 6.665711453507098, 
                                                     25.51400517338097], false]
(%i4) E:zheev(A,true)$
(%i5) E[1];
(%o5) [- 16.00474647209474, - 6.764970154793325, 6.665711453507101, 
                                                             25.51400517338096]
(%i6) E[2];
               [   0.2674650533172745 %i + 0.2175453586665017    ]
               [                                                 ]
               [  0.002696730886619885 %i + 0.6968836773391712   ]
(%o6)  Col 1 = [                                                 ]
               [ (- 0.6082406376714117 %i) - 0.01210614292697931 ]
               [                                                 ]
               [               0.1593081858095037                ]
         [   0.2644937470667444 %i + 0.4773693349937472   ]
         [                                                ]
         [ (- 0.2852389036031621 %i) - 0.1414362742011673 ]
 Col 2 = [                                                ]
         [   0.2654607680986639 %i + 0.4467818117184174   ]
         [                                                ]
         [               0.5750762708542709               ]
         [   0.2810649767305922 %i - 0.1335263928245182   ]
         [                                                ]
         [   0.2866310132869556 %i - 0.4536971347853274   ]
 Col 3 = [                                                ]
         [ (- 0.2933684323754295 %i) - 0.4954972425541057 ]
         [                                                ]
         [               0.5325337537576771               ]
         [ (- 0.5737316575503476 %i) - 0.3966146799427706 ]
         [                                                ]
         [  0.01826502619021457 %i + 0.3530557704387017   ]
 Col 4 = [                                                ]
         [  0.1673700900085425 %i + 0.01476684746229564   ]
         [                                                ]
         [               0.6002632636961784               ]
@end example

@opencatbox{Categories:}
@category{Package lapack}
@closecatbox

@end deffn

