# Gamma function simplification

Maxima code for simplifying expressions that involve gamma and factorial functions. 

## Installation

To use the `gamma_simp` package, copy the file `gamma_simp.mac` to a folder that Maxima can find. To load the package, enter `load(gamma_simp)` at the Maxima command line.

To view the paths that Maxima searches to find a *.mac file, enter 
`file_search_maxima;` at a Maxima command line. 

## Usage

There are two user level functions in the package. They are `gamma_simp` and `factorial_simp.` Both of these functions take a single Maxima expression as input and both return a simplification of the input. 

The function `gamma_simp` matches subexpressions of the input to various gamma function identities and replaces the match with a simplification. The simplification process generally only converts gamma functions to gamma functions (not, for example,
into beta functions), but depending on the value of the option variable `pochhammer_max_index,` the output can involve a pochhammer symbol.

The function `factorial_simp` works similarly. 

To simplify all gamma-like functions, including binomial coefficients, pochhammer symbols, beta functions, and factorials, apply the function `makegamma` to the input of `gamma_simp.` 

_Option variables:_ The option variables `radsubstflag,` `pochhammer_max_index,` and `ratfac` sometimes alter the results of `gamma_simp` and `factorial_simp.` 

_Related functions:_ `factcomb,` `minfactorial,` `makegamma,` and `makefactorial.`

## Examples

The last example shows the use of `makegamma` to pre-process the input.
~~~
(%i1)	load(gamma_simp)$

(%i2)	gamma_simp(gamma(z)*gamma(1-z));
(%o2)	%pi/sin(%pi*z)

(%i3)	gamma_simp(gamma(1/4)*gamma(3/4));
(%o3)	sqrt(2)*%pi

(%i4)	gamma_simp(gamma(z)*gamma(z + 1/2));
(%o4)	sqrt(%pi)*2^(1-2*z)*gamma(2*z)

(%i5)	gamma_simp(x*gamma(x)+gamma(x+3)/(x+2));
(%o5)	x*gamma(x+1)+2*gamma(x+1)

(%i6)	gamma_simp(makegamma((k - n) *binomial(n,k)  + n * binomial(n-1,k)));
(%o6)	0
~~~

## Details

The functions `gamma_simp` and `factorial_simp` make a good effort to simplify every vanishing expression to zero. But if an expression doesn't simplify to zero, it does _not_ mean that the expression is non vanishing.

Additionally, `gamma_simp` does not always simplify semantically identical expressions to syntactically identical expressions. That is, `gamma_simp` does not produce a canonical form. An example:

~~~
(%i1)	load(gamma_simp)$

(%i2)	declare(n,integer)$

(%i3)	xx : makegamma(pochhammer(a-n,n));
(%o3)	gamma(a)/gamma(a-n)

(%i4)	yy : makegamma(pochhammer(1-a,n)*(-1)^n);
(%o4)	((-1)^n*gamma(n-a+1))/gamma(1-a)

(%i5)	gamma_simp(xx) = gamma_simp(yy);
(%o5)	gamma(a)/gamma(a-n)=((-1)^n*gamma(n-a+1))/gamma(1-a)

(%i6)	trigexpand(gamma_simp(xx-yy));
(%o6)	0
~~~

Although the expressions xx and yy are semantically the same (see %o6), `gamma_simp` does not simplify them to identical expressions.


## Identities

The function `gamma_simp` matches subexpressions of the input to the left hand side of each of the following identities and replaces them by the right side:

$$ 
\dfrac{\Gamma\left(z+1\right)}{z} = \Gamma\left(z\right), \quad z \in \mathbf{C_\neq 0} 
$$ 

$$ 
\Gamma\left(z\right)\Gamma\left(1-z\right)= \dfrac{\uppi}{\sin\left(\uppi z\right)},  \quad z \in \mathbf{C} \setminus \mathbf{Z}, 
$$

$$
\prod_{k=0}^{n-1}\Gamma\left (z+\frac{k}{n}\right) = (2\uppi)^{(n-1)/2}n^{1/2-nz}  \Gamma \left(n z \right)
$$

$$
\Gamma\left(\dfrac{1}{2}+\mathrm{i}y\right)\Gamma\left(\dfrac{1}{2}-\mathrm{i}%
y\right)=\dfrac{\uppi}{\cosh\left(\uppi y\right)},
$$

$$
\Gamma\left(\mathrm{i} y\right) \Gamma\left(-\mathrm{i} y\right)  = 
\frac{\uppi}{y\sinh\left(\uppi y\right)}, \quad y \in \mathbf{R}_{\neq 0}
$$

See: http://dlmf.nist.gov/5.5.E1, http://dlmf.nist.gov/5.5.E3, http://dlmf.nist.gov/5.4.E4, and http://dlmf.nist.gov/5.4.E3 

## Implementation

The package is written in the Maxima language.

The function `gamma_simp` matches subexpressions of the input to various gamma function identities. The function `gamma_simp` uses `radsubst` to match the subexpressions, it does _not_ explicitly use Maxima's pattern matcher.

The function `factorial_simp` converts all factorials to gamma form. It then dispatches `gamma_simp` and converts back to factorial form. Any gamma functions in
the input are protected from participating in the gamma function simplification process.

The only user level functions in the package are `gamma_simp` and `factorial_simp.`
The remaining functions in the package are _not_ intended to be user level functions. 


## Testing

To run the test suite for the package `gamma_simp,` enter `batch(rtest_gamma_simp, 'test)` at the Maxima prompt. To do this, you will need to copy the file `rtest_gamma_simp` to a folder that Maxima can find. 


## Thanks

Part of the test file (`rtest_gamma_simp`) is adapted from the [SymPy package](https://www.sympy.org/en/index.html) for simplification of gamma functions. I thank the SymPy developers for making this resource available.

Additionally, I thank readers of the Maxima list, including Oscar Benjamin, Stavros Macrakis, Ray Rogers, and Raymond Toy, for suggestions and encouragement. Of course, all bugs are mine.

_Reference:_ https://github.com/sympy/sympy/blob/master/sympy/simplify/tests/test_gammasimp.py
