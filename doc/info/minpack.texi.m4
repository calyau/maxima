@menu
* Introduction to minpack::
* Functions and Variables for minpack::
@end menu

@node Introduction to minpack, Functions and Variables for minpack, Package minpack, Package minpack
@section Introduction to minpack

@code{Minpack} is a Common Lisp translation (via @code{f2cl}) of the
Fortran library @url{https://www.netlib.org/minpack/, MINPACK as obtained from Netlib}.

@opencatbox{Categories:}
@category{Numerical methods} 
@category{Optimization}
@category{Share packages}
@category{Package minpack}
@closecatbox

@node Functions and Variables for minpack,  , Introduction to minpack, Package minpack
@section Functions and Variables for minpack

@anchor{minpack_lsquares}
@deffn {Function} minpack_lsquares (@var{flist}, @var{varlist}, @var{guess}, ['tolerance = @var{tolerance}, 'jacobian = @var{jacobian}])

Compute the point that minimizes the sum of the squares of the
functions in the list @var{flist}.  The variables are in the list
@var{varlist}.  An initial guess of the optimum point must be provided
in @var{guess}.

Let @var{flist} be a list of @math{m} functions,
m4_mathdot(<<<f_i(x_1, x_2, ..., x_n)>>>, <<<f_i(x_1, x_2, ..., x_n)>>>)
Then this function can be used to find the values of
m4_math(<<<x_1, x_2, ..., x_n>>>, <<<x_1, x_2, ..., x_n>>>)
that solve the least squares problem

m4_displaymath(
<<<\sum_i^m f_i(x_1, x_2,...,x_n)^2>>>,
<<<
               m
              ____
              ╲      2
               ⟩    f (x_1, x_2,..., x_n)
              ╱      i
              ‾‾‾‾
              i = 1
>>>)


The optional keyword arguments, @var{tolerance} and @var{jacobian}
provide some control over the algorithm.

@table @code
@item tolerance
the estimated relative error desired in the sum of squares.  The
default value is approximately
m4_mathdot(<<<1.0537\times 10^{-8}>>>, <<<1.0537e-8>>>)
@item jacobian
specifies the Jacobian.  If @var{jacobian}
is not given or is @code{true} (the default), the Jacobian is computed
from @var{flist}.  If @var{jacobian} is @code{false}, a numerical
approximation is used.  @xref{jacobian, Jacobian}.
@end table

@code{minpack_lsquares} returns a list of three items as follows:
@enumerate
@item
The estimated solution
@item
The sum of squares
@item
The success of the algorithm. The possible values are
@enumerate 0
@item 
improper input parameters.
@item 
algorithm estimates that the relative error in the sum of squares is
at most @code{tolerance}. 
@item 
algorithm estimates that the relative error between x and the solution
is at most @code{tolerance}. 
@item 
conditions for info = 1 and info = 2 both hold.
@item 
fvec is orthogonal to the columns of the jacobian to machine
precision. 
@item 
number of calls to fcn with iflag = 1 has reached 100*(n+1).
@item 
tol is too small. no further reduction in the sum of squares is
possible. 
@item 
tol is too small. no further improvement in the approximate solution x
is possible. 
@end enumerate
@end enumerate

Here is an example using Powell's singular function.
@c ===beg===
@c load("minpack")$
@c powell(x1,x2,x3,x4) := [x1+10*x2, sqrt(5)*(x3-x4), (x2-2*x3)^2, sqrt(10)*(x1-x4)^2]$
@c minpack_lsquares(powell(x1,x2,x3,x4), [x1,x2,x3,x4], [3,-1,0,1]);
@c ===end===
@example
(%i1) load("minpack")$
(%i2) powell(x1,x2,x3,x4) := [x1+10*x2, sqrt(5)*(x3-x4), (x2-2*x3)^2, sqrt(10)*(x1-x4)^2]$
@group
(%i3) minpack_lsquares(powell(x1,x2,x3,x4), [x1,x2,x3,x4], [3,-1,0,1]);
(%o3) [[1.6521175961683935e-17, - 1.6521175961683934e-18, 
2.6433881538694683e-18, 2.6433881538694683e-18], 
6.109327859207777e-34, 4]
@end group
@end example

Same problem but use numerical approximation to Jacobian.
@c ===beg===
@c load("minpack")$
@c powell(x1,x2,x3,x4) := [x1+10*x2, sqrt(5)*(x3-x4), (x2-2*x3)^2, sqrt(10)*(x1-x4)^2]$
@c minpack_lsquares(powell(x1,x2,x3,x4), [x1,x2,x3,x4], [3,-1,0,1], jacobian = false);
@c ===end===
@example
(%i1) load("minpack")$
(%i2) powell(x1,x2,x3,x4) := [x1+10*x2, sqrt(5)*(x3-x4), (x2-2*x3)^2, sqrt(10)*(x1-x4)^2]$
@group
(%i3) minpack_lsquares(powell(x1,x2,x3,x4), [x1,x2,x3,x4], [3,-1,0,1], jacobian = false);
(%o3) [[5.060282149485331e-11, - 5.060282149491206e-12, 
2.1794478435472183e-11, 2.1794478435472183e-11], 
3.534491794847031e-21, 5]
@end group
@end example

@opencatbox{Categories:}
@category{Package minpack}
@closecatbox

@end deffn

@anchor{minpack_solve}
@deffn {Function} minpack_solve (@var{flist}, @var{varlist}, @var{guess}, ['tolerance = @var{tolerance}, 'jacobian = @var{jacobian}])

Solve a system of @code{n} equations in @code{n} unknowns.
The @code{n} equations are given in the list @var{flist}, and the
unknowns are in @var{varlist}.  An initial guess of the solution must
be provided in @var{guess}.

Let @var{flist} be a list of @math{m} functions,
m4_mathdot(<<<f_i(x_1, x_2, ..., x_n)>>>, <<<f_i(x_1, x_2, ..., x_n)>>>)
Then this functions solves the system of @math{m} nonlinear equations
in @math{n} variables:

m4_displaymath(
<<<f_i(x_1, x_2, ..., x_n) = 0>>>,
<<<f_i(x_1, x_2, ..., x_n) = 0>>>)

The optional keyword arguments, @var{tolerance} and @var{jacobian}
provide some control over the algorithm.

@table @code
@item tolerance
the estimated relative error desired in the sum of squares.  The
default value is approximately
m4_mathdot(<<<1.0537\times 10^{-8}>>>, <<<1.0537e-8>>>)
@item jacobian
specifies the Jacobian.  If @var{jacobian}
is not given or is @code{true} (the default), the Jacobian is computed
from @var{flist}.  If @var{jacobian} is @code{false}, a numerical
approximation is used.  @xref{jacobian, Jacobian}.
@end table

@code{minpack_solve} returns a list of three items as follows:
@enumerate
@item
The estimated solution
@item
The sum of squares
@item
The success of the algorithm.  The possible values are
@enumerate 0
@item
improper input parameters.
@item
algorithm estimates that the relative error in the solution is
at most @code{tolerance}. 
@item
number of calls to fcn with iflag = 1 has reached 100*(n+1).
@item
tol is too small. no further reduction in the sum of squares is
possible. 
@item
Iteration is not making good progress.
@end enumerate
@end enumerate

@c ===beg===
@c load("minpack")$
@c powell(x1,x2,x3,x4) := [x1+10*x2, sqrt(5)*(x3-x4), (x2-2*x3)^2, sqrt(10)*(x1-x4)^2]$
@c minpack_lsquares(powell(x1,x2,x3,x4), [x1,x2,x3,x4], [3,-1,0,1]);
@c ===end===
@example
(%i1) load("minpack")$
(%i2) powell(x1,x2,x3,x4) := [x1+10*x2, sqrt(5)*(x3-x4), (x2-2*x3)^2, sqrt(10)*(x1-x4)^2]$
@group
(%i3) minpack_lsquares(powell(x1,x2,x3,x4), [x1,x2,x3,x4], [3,-1,0,1]);
(%o3) [[1.6521175961683935e-17, - 1.6521175961683934e-18, 
2.6433881538694683e-18, 2.6433881538694683e-18], 
6.109327859207777e-34, 4]
@end group
@end example
In this particular case, we can solve this analytically:
@c ===beg===
@c powell(x1,x2,x3,x4) := [x1+10*x2, sqrt(5)*(x3-x4), (x2-2*x3)^2, sqrt(10)*(x1-x4)^2]$
@c solve(powell(x1,x2,x3,x4),[x1,x2,x3,x4]);
@c ===end===
@example
(%i1) powell(x1,x2,x3,x4) := [x1+10*x2, sqrt(5)*(x3-x4), (x2-2*x3)^2, sqrt(10)*(x1-x4)^2]$
@group
(%i2) solve(powell(x1,x2,x3,x4),[x1,x2,x3,x4]);
(%o2)          [[x1 = 0, x2 = 0, x3 = 0, x4 = 0]]
@end group
@end example
and we see that the numerical solution is quite close the analytical one.

@opencatbox{Categories:}
@category{Package minpack}
@closecatbox

@end deffn

@c Local Variables: 
@c mode: texinfo
@c TeX-master: "include-maxima"
@c End: 
