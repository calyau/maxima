@c -*- mode: texinfo -*-
@menu
* Introduction to distrib::
* Functions and Variables for continuous distributions::
* Functions and Variables for discrete distributions::
@end menu

@c Define an m4 macro for the NormalRV function that is used below.
@c Can't use @macro because m4 is processed too late for this to work.
m4_define(<<<m4_Normal_RV>>>,
m4_math(<<<{\it Normal}($1, $2)>>>, <<<Normal($1, $2)>>>))

@c Define the function student t to denote Student's t random variable
m4_define(<<<m4_Student_T_RV>>>,
@math{t($1)})

@c Define noncentral Student's t RV
m4_define(<<<m4_Noncentral_T_RV>>>,
m4_math(<<<{\it nc\_t}($1, $2)>>>,<<<nc_t($1, $2)>>>))

@c Define Chi^2(n), the chi-squared random variate function.
m4_define(<<<m4_Chi2_RV>>>,
m4_math(<<<\chi^2($1)>>>, <<<Chi^2($2)>>>))

@c Define the Gamma RV
m4_define(<<<m4_Gamma_RV>>>,
m4_math(<<<\Gamma\left($1,$2\right)>>>, <<<Gamma($1,$2)>>>))

@c Define the noncentral chi-squared RV
m4_define(<<<m4_noncentral_chi2_RV>>>,
m4_math(<<<{\it nc\_Chi}^2($1,$2)>>>, <<<nc_Chi^2($1,$2)>>>))

@c Define the exponential RV
m4_define(<<<m4_Exponential_RV>>>,
m4_math(<<<{\it Exponential}($1)>>>, <<<Exponential($1)>>>))

@c Define the Weibull RV
m4_define(<<<m4_Weibull_RV>>>,
m4_math(<<<{\it Weibull}($1,$2)>>>, <<<Weibull($1,$2)>>>))

@c Define lognormal RV
m4_define(<<<m4_Lognormal_RV>>>,
m4_math(<<<{\it Lognormal}($1,$2)>>>,<<<Lognormal($1,$2)>>>))

@c Define beta RV
m4_define(<<<m4_Beta_RV>>>,
m4_math(<<<{\it Beta}($1,$2)>>>,<<<Beta($1,$2)>>>))

@c Define continuous uniform RV
m4_define(<<<m4_Continuous_Uniform_RV>>>,
m4_math(<<<{\it
ContinuousUniform}($1,$2)>>>,<<<ContinuousUniform($1,$2)>>>))

@c Define logistic RV
m4_define(<<<m4_Logistic_RV>>>,
m4_math(<<<{\it Logistic}($1,$2)>>>,<<<<Logistice($1,$2)>>>))

@c Define pareto RV
m4_define(<<<m4_Pareto_RV>>>,
m4_math(<<<{\it Pareto}($1,$2)>>>,<<<Pareto($1,$2)>>>))

@c Define Rayleigh RV
m4_define(<<<m4_Rayleigh_RV>>>,
m4_math(<<<{\it Rayleigh}($1)>>>,<<<Rayleigh($1)>>>))

@c Define Laplace RV
m4_define(<<<m4_Laplace_RV>>>,
m4_math(<<<{\it Laplace}($1,$2)>>>,<<<Laplace($1,$2)>>>))

@c Define Cauchy RV
m4_define(<<<m4_Cauchy_RV>>>,
m4_math(<<<{\it Cauchy}($1,$2)>>>,<<<Cauchy($1,$2)>>>))

@c Define Gumbel RV
m4_define(<<<m4_Gumbel_RV>>>,
m4_math(<<<{\it Gumbel}($1,$2)>>>,<<<Gumbel($1,$2)>>>))

@c Define Binomial RV
m4_define(<<<m4_Binomial_RV>>>,
m4_math(<<<{\it Binomial}($1,$2)>>>,<<<Binomial($1,$2)>>>))

@c Define Poisson RV
m4_define(<<<m4_Poisson_RV>>>,
m4_math(<<<{\it Poisson}($1)>>>,<<<Poisson($1)>>>))

@c Define Bernoulli RV
m4_define(<<<m4_Bernoulli_RV>>>,
m4_math(<<<{\it Bernoulli}($1)>>>,<<<Bernoulli($1)>>>))

@c Define Geometric RV
m4_define(<<<m4_Geometric_RV>>>,
m4_math(<<<{\it Geometric}($1)>>>,<<<Geometric($1)>>>))

@c Define Discrete Uniform RV
m4_define(<<<m4_DiscreteUniform_RV>>>,
m4_math(<<<{\it DiscreteUniform}($1)>>>,<<<DiscreteUniform($1)>>>))

@c Define Hypergeometric RV
m4_define(<<<m4_Hypergeometric_RV>>>,
m4_math(<<<{\it Hypergeometric}($1,$2,$3)>>>,<<<Hypergeometric($1,$2,$3)>>>))

@c Define Negative Binomial RV
m4_define(<<<m4_NegativeBinomial_RV>>>,
m4_math(<<<{\it NegativeBinomial}($1,$2)>>>,<<<NegativeBinomial($1,$2)>>>))


@node Introduction to distrib, Functions and Variables for continuous distributions, , Package distrib
@section Introduction to distrib


Package @code{distrib} contains a set of functions for making probability computations on both discrete and continuous univariate models. 

What follows is a short reminder of basic probabilistic related definitions.

Let @math{f(x)} be the @var{density function} of an absolute continuous random variable @math{X}. The @var{cumulative distribution function} is defined as
m4_displaymath(
<<<F\left(x\right)=\int_{ -\infty }^{x}{f\left(u\right)\;du}>>>,
<<<
@example
                       x
                      /
                      [
               F(x) = I     f(u) du
                      ]
                      /
                       minf
@end example
>>>)
which equals the probability 
m4_mathdot(<<<{\rm Pr}(X \le x)>>>, <<<Pr(X <= x)>>>)

The @var{mean} value is a localization parameter and is defined as
m4_displaymath(
<<<E\left[X\right]=\int_{ -\infty }^{\infty }{x\,f\left(x\right)\;dx}>>>,
<<<
@example
                     inf
                    /
                    [
           E[X]  =  I   x f(x) dx
                    ]
                    /
                     minf
@end example
>>>)

The @var{variance} is a measure of variation,
m4_displaymath(
<<<V\left[X\right]=\int_{ -\infty }^{\infty }{f\left(x\right)\,\left(x
 -E\left[X\right]\right)^2\;dx}>>>,
<<<
@example
                 inf
                /
                [                    2
         V[X] = I     f(x) (x - E[X])  dx
                ]
                /
                 minf
@end example
>>>)
which is a positive real number. The square root of the variance is
the @var{standard deviation}, 
m4_mathcomma(<<<D[x]=\sqrt{V[X]}>>>, <<<D[X]=sqrt(V[X])>>>) 
and it is another measure of variation.

The @var{skewness coefficient} is a measure of non-symmetry,
m4_displaymath(
<<<SK\left[X\right]={{\int_{ -\infty }^{\infty }{f\left(x\right)\,
 \left(x-E\left[X\right]\right)^3\;dx}}\over{D\left[X\right]^3}}>>>,
<<<
@example
                 inf
                /
            1   [                    3
  SK[X] = ----- I     f(x) (x - E[X])  dx
              3 ]
          D[X]  /
                 minf
@end example
>>>)

And the @var{kurtosis coefficient} measures the peakedness of the distribution,
m4_displaymath(
<<<KU\left[X\right]={{\int_{ -\infty }^{\infty }{f\left(x\right)\,
 \left(x-E\left[X\right]\right)^4\;dx}}\over{D\left[X\right]^4}}-3>>>,
<<<
@example
                 inf
                /
            1   [                    4
  KU[X] = ----- I     f(x) (x - E[X])  dx - 3
              4 ]
          D[X]  /
                 minf
@end example
>>>)
If @math{X} is gaussian, @math{KU[X]=0}. In fact, both skewness and kurtosis are shape parameters used to measure the non--gaussianity of a distribution.

If the random variable @math{X} is discrete, the density, or @var{probability}, function @math{f(x)} takes positive values within certain countable set of numbers @math{x_i}, and zero elsewhere. In this case, the cumulative distribution function is
m4_displaymath(
<<< F\left(x\right)=\sum_{x_{i}\leq x}{f\left(x_{i}\right)} >>>,
<<<
@example
                       ====
                       \
                F(x) =  >    f(x )
                       /        i
                       ====
                      x <= x
                       i
@end example
>>>)

The mean, variance, standard deviation, skewness coefficient and kurtosis coefficient take the form
m4_displaymath(
<<<\eqalign{
E\left[X\right]&=\sum_{x_{i}}{x_{i}f\left(x_{i}\right)}, \cr
V\left[X\right]&=\sum_{x_{i}}{f\left(x_{i}\right)\left(x_{i}-E\left[X\right]\right)^2},\cr
D\left[X\right]&=\sqrt{V\left[X\right]},\cr
SK\left[X\right]&={{\sum_{x_{i}}{f\left(x\right)\,
 \left(x-E\left[X\right]\right)^3\;dx}}\over{D\left[X\right]^3}}, \cr
KU\left[X\right]&={{\sum_{x_{i}}{f\left(x\right)\,
 \left(x-E\left[X\right]\right)^4\;dx}}\over{D\left[X\right]^4}}-3, 
}>>>,
<<<
@example
          ====
          \
   E[X] =  >  x  f(x ) ,
          /    i    i
          ====
           x 
            i

           ====
           \                     2
   V[X] =   >    f(x ) (x - E[X])  ,
           /        i    i
           ====
            x
             i

          D[X] = sqrt(V[X]),

                     ====
              1      \                     3
  SK[X] =  -------    >    f(x ) (x - E[X])  
           D[X]^3    /        i    i
                     ====
                      x
                       i

                     ====
              1      \                     4
  KU[X] =  -------    >    f(x ) (x - E[X])   - 3 ,
           D[X]^4    /        i    i
                     ====
                      x
                       i
@end example
>>>)
respectively.

There is a naming convention in package @code{distrib}. Every function name has two parts, the first one makes reference to the function or parameter we want to calculate,
@example
Functions:
   Density function            (pdf_*)
   Distribution function       (cdf_*)
   Quantile                    (quantile_*)
   Mean                        (mean_*)
   Variance                    (var_*)
   Standard deviation          (std_*)
   Skewness coefficient        (skewness_*)
   Kurtosis coefficient        (kurtosis_*)
   Random variate              (random_*)
@end example

The second part is an explicit reference to the probabilistic model,
@example
Continuous distributions:
   Normal              (*normal)
   Student             (*student_t)
   Chi^2               (*chi2)
   Noncentral Chi^2    (*noncentral_chi2)
   F                   (*f)
   Exponential         (*exp)
   Lognormal           (*lognormal)
   Gamma               (*gamma)
   Beta                (*beta)
   Continuous uniform  (*continuous_uniform)
   Logistic            (*logistic)
   Pareto              (*pareto)
   Weibull             (*weibull)
   Rayleigh            (*rayleigh)
   Laplace             (*laplace)
   Cauchy              (*cauchy)
   Gumbel              (*gumbel)

Discrete distributions:
   Binomial             (*binomial)
   Poisson              (*poisson)
   Bernoulli            (*bernoulli)
   Geometric            (*geometric)
   Discrete uniform     (*discrete_uniform)
   hypergeometric       (*hypergeometric)
   Negative binomial    (*negative_binomial)
   Finite discrete      (*general_finite_discrete)
@end example

For example, @code{pdf_student_t(x,n)} is the density function of the Student distribution with @var{n} degrees of freedom, @code{std_pareto(a,b)} is the standard deviation of the Pareto distribution with parameters @var{a} and @var{b} and @code{kurtosis_poisson(m)} is the kurtosis coefficient of the Poisson distribution with mean @var{m}.


In order to make use of package @code{distrib} you need first to load it by typing
@example
(%i1) load("distrib")$
@end example

For comments, bugs or suggestions, please contact the author at @var{'riotorto AT yahoo DOT com'}.

@opencatbox{Categories:}
@category{Statistical functions}
@category{Share packages}
@category{Package distrib}
@closecatbox




@node Functions and Variables for continuous distributions, Functions and Variables for discrete distributions, Introduction to distrib, Package distrib
@section Functions and Variables for continuous distributions
Maxima knows the following kinds of continuous distributions.

@menu
* Normal Random Variable::
* Student's t Random Variable::
* Noncentral Student's t Random Variable::
* Chi-squared Random Variable::
* Noncentral Chi-squared Random Variable::
* F Random Variable::
* Exponential Random Variable::
* Lognormal Random Variable::
* Gamma Random Variable::
* Beta Random Variable::
* Continuous Uniform Random Variable::
* Logistic Random Variable::
* Pareto Random Variable::
* Weibull Random Variable::
* Rayleigh Random Variable::
* Laplace Random Variable::
* Cauchy Random Variable::
* Gumbel Random Variable::
@end menu

@node Normal Random Variable, Student's t Random Variable, Functions and Variables for continuous distributions, Functions and Variables for continuous distributions
@subsection Normal Random Variable

Normal random variables (also called Gaussian) is denoted
by 
m4_Normal_RV(m, s) 
where
@math{m} is the mean and @math{s > 0} is the standard deviation.

@anchor{pdf_normal}
@deffn {Function} pdf_normal (@var{x},@var{m},@var{s})
Returns the value at @var{x} of the density function of a 
m4_Normal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; m, s) = {1\over s\sqrt{2\pi}} e^{\displaystyle -{(x-m)^2\over 2s^2}}>>>,
<<<
@example
                                 2
                          (x - m)
                        - --------
                               2
                            2 s
                      %e
      f(x, m, s) = -------------------
                   sqrt(2) sqrt(%pi) s
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_normal}
@deffn {Function} cdf_normal (@var{x},@var{m},@var{s})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Normal_RV(m,s) 
random variable, with @math{s>0}. This function is defined in terms of Maxima's built-in error function @code{erf}.

The cdf can be written analytically:
m4_displaymath(
<<<F(x; m, s) = {1\over 2} + {1\over 2} {\rm erf}\left(x-m\over s\sqrt{2}\right)>>>,
<<<
@example
                        x - m
                  erf(---------)
                      sqrt(2) s    1
     F(x, m, s) = -------------- + -
                        2          2
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c cdf_normal(x,m,s);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_normal(x,m,s);
                             x - m
                       erf(---------)
                           sqrt(2) s    1
(%o2)                  -------------- + -
                             2          2
@end group
@end example

See also @mrefdot{erf}

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_normal}
@deffn {Function} quantile_normal (@var{q},@var{m},@var{s})
Returns the @var{q}-quantile of a 
m4_Normal_RV(m,s) 
random variable, with @math{s>0}; in other words, this is the inverse of @mrefdot{cdf_normal} Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@c ===beg===
@c load ("distrib")$
@c quantile_normal(95/100,0,1);
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) quantile_normal(95/100,0,1);
                                         9
(%o2)                sqrt(2) inverse_erf(--)
                                         10
@end group
@group
(%i3) float(%);
(%o3)                   1.644853626951472
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_normal}
@deffn {Function} mean_normal (@var{m},@var{s})
Returns the mean of a 
m4_Normal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = m>>>,
<<<
@example
                             E[X] = m
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_normal}
@deffn {Function} var_normal (@var{m},@var{s})
Returns the variance of a 
m4_Normal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = s^2>>>,
<<<
@example
                                     2
                             V[X] = s
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn

@anchor{std_normal}
@deffn {Function} std_normal (@var{m},@var{s})
Returns the standard deviation of a 
m4_Normal_RV(m,s) 
random variable, with @math{s>0}, namely @var{s}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = s>>>,
<<<
@example
                             D[X] = s
@end example

>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_normal}
@deffn {Function} skewness_normal (@var{m},@var{s})
Returns the skewness coefficient of a 
m4_Normal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = 0>>>,
<<<
@example
                             SK[X] = 0
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_normal}
@deffn {Function} kurtosis_normal (@var{m},@var{s})
Returns the kurtosis coefficient of a 
m4_Normal_RV(m,s) 
random variable, with @math{s>0}, which is always equal to 0. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = 0>>>,
<<<
@example
                             SK[X] = 0
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_normal}
@deffn {Function} random_normal (@var{m},@var{s}) @
@fname{random_normal} (@var{m},@var{s},@var{n})

Returns a 
m4_Normal_RV(m,s) 
random variate, with @math{s>0}. Calling @code{random_normal} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

This is an implementation of the Box-Mueller algorithm, as described in Knuth, D.E. (1981) @var{Seminumerical Algorithms. The Art of Computer Programming.} Addison-Wesley.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Student's t Random Variable, Noncentral Student's t Random Variable, Normal Random Variable, Functions and Variables for continuous distributions
@subsection Student's t Random Variable


Student's t random variable is denoted by 
m4_Student_T_RV(n) 
where
@math{n} is the degrees of freedom with @math{n > 0}.  If @math{Z} is
a 
m4_Normal_RV(0,1) 
variable and @math{V} is an
independent 
m4_math(\chi^2, chi^2) 
random variable with @math{n} degress of
freedom, then

m4_displaymath(
<<<Z \over \sqrt{V/n}>>>,
<<<@math{Z/sqrt(V/n)}>>>)

has a Student's @math{t}-distribution with @math{n} degrees of freedom.

@anchor{pdf_student_t}
@deffn {Function} pdf_student_t (@var{x},@var{n})
Returns the value at @var{x} of the density function of a Student
random variable 
m4_Student_T_RV(n)
, with @math{n>0} degrees of freedom. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; n) = \left[\sqrt{n} B\left({1\over 2}, {n\over 2}\right)\right]^{-1}
\left(1+{x^2\over n}\right)^{\displaystyle -{n+1\over 2}}>>>,
<<<
@example
                                     (- n) - 1
                               2     ---------
                      n + 1   x          2
                gamma(-----) (-- + 1)
                        2     n
      f(x, n) = ------------------------------
                                  n
                  sqrt(%pi) gamma(-) sqrt(n)
                                  2
@end example
@math{(sqrt(n)*beta(1/2,n/2))^(-1) (1+x^2/n)^(-(n+1)/2)}>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_student_t}
@deffn {Function} cdf_student_t (@var{x},@var{n})
Returns the value at @var{x} of the cumulative distribution function of a Student
random variable 
m4_Student_T_RV(n)
, with @math{n>0} degrees of freedom.

The cdf is
m4_displaymath(
<<<F(x; n) =
\cases{
1-\displaystyle{1\over 2} I_t\left({n\over 2}, {1\over 2}\right) & $x \ge 0$ \cr
\cr
\displaystyle{1\over 2} I_t\left({n\over 2}, {1\over 2}\right) & $x < 0$
}>>>,
<<<
@example
         [ 1-1/2*I_t(n/2, 1/2)  x >= 0
F(x,n) = [
         [ 1/2*I_t(n/2, 1/2)    x < 0
@end example
>>>)

where 
m4_math(<<<t = n/(n+x^2)>>>, <<<t = n/(n+x^2)>>>) 
and 
m4_math(<<<I_t(a,b)>>>, <<<I_t(a,b)>>>) 
is the
@ref{beta_incomplete_regularized} function.

@c ===beg===
@c load ("distrib")$
@c cdf_student_t(1/2, 7/3);
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_student_t(1/2, 7/3);
                                            7  1  28
                beta_incomplete_regularized(-, -, --)
                                            6  2  31
(%o2)       1 - -------------------------------------
                                  2
@end group
@group
(%i3) float(%);
(%o3)                  0.6698450596140415
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_student_t}
@deffn {Function} quantile_student_t (@var{q},@var{n})
Returns the @var{q}-quantile of a Student random variable 
m4_Student_T_RV(n)
, with @math{n>0}; in other words, this is the inverse of @code{cdf_student_t}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_student_t}
@deffn {Function} mean_student_t (@var{n})
Returns the mean of a Student random variable 
m4_Student_T_RV(n)
, with @math{n>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = 0>>>,
<<<
@example
                             E[X] = 0
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_student_t}
@deffn {Function} var_student_t (@var{n})
Returns the variance of a Student random variable 
m4_Student_T_RV(n)
, with @math{n>2}.

The variance is
m4_displaymath(
<<<V[X] = {n\over n-2}>>>,
<<<
@example
                                    n
                           V[X] = -----
                                  n - 2
@end example

>>>)
@c ===beg===
@c load ("distrib")$
@c var_student_t(n);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) var_student_t(n);
                                n
(%o2)                         -----
                              n - 2
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_student_t}
@deffn {Function} std_student_t (@var{n})
Returns the standard deviation of a Student random variable 
m4_Student_T_RV(n)
, with @math{n>2}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = \sqrt{\displaystyle{n\over n-2}}>>>,
<<<
@example
                                     n
                       D[X] = sqrt(-----)
                                   n - 2
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_student_t}
@deffn {Function} skewness_student_t (@var{n})
Returns the skewness coefficient of a Student random variable 
m4_Student_T_RV(n)
, with @math{n>3}, which is always equal to 0. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = 0>>>,
<<<
@example
                             SK[X] = 0
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_student_t}
@deffn {Function} kurtosis_student_t (@var{n})
Returns the kurtosis coefficient of a Student random variable 
m4_Student_T_RV(n)
, with @math{n>4}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {6\over n-4}>>>,
<<<
@example
                                    6
                          KU[X] = -----
                                  n - 4
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_student_t}
@deffn {Function} random_student_t (@var{n}) @
@fname{random_student_t} (@var{n},@var{m})

Returns a Student random variate 
m4_Student_T_RV(n)
, with @math{n>0}. Calling @code{random_student_t} with a second argument @var{m}, a random sample of size @var{m} will be simulated.

The implemented algorithm is based on the fact that if @math{Z} is a
normal random variable 
m4_Normal_RV(0,1) 
and @math{S^2} is
a 
m4_math(\chi^2, chi squared) 
random variable with @math{n} degrees of
freedom, 
m4_Chi2_RV(n)
, then

m4_displaymath(
<<<X={{Z}\over{\sqrt{{S^2}\over{n}}}}>>>,
<<<
@example
                           Z
                 X = -------------
                     /   2  \ 1/2
                     |  S   |
                     | ---  |
                     \  n   /
@end example
>>>)
is a Student random variable with @math{n} degrees of freedom, 
m4_Student_T_RV(n)
.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Noncentral Student's t Random Variable, Chi-squared Random Variable, Student's t Random Variable, Functions and Variables for continuous distributions
@subsection Noncentral Student's t Random Variable

Let @math{ncp} be the non-centrality parameter, @math{n} be the
degrees of freedom for the non-central Student's @math{t} random
variable.

Then let @math{X} be a 
m4_Normal_RV(n,ncp) 
and @math{S^2} be an
independent 
m4_math(\chi^2, chi squared) 
random variable with @math{n} degrees of freedom, the
random variable
m4_displaymath(
<<<U = {X \over \sqrt{S^2\over n}}>>>,
<<<@math{U = X/sqrt(S^2/n)}>>>)

has a non-central Student's @math{t} distribution with non-centrality
parameter @math{ncp}.

@anchor{pdf_noncentral_student_t}
@deffn {Function} pdf_noncentral_student_t (@var{x},@var{n},@var{ncp})
Returns the value at @var{x} of the density function of a noncentral
Student random variable 
m4_Noncentral_T_RV(n,ncp)
, with @math{n>0} degrees of freedom and noncentrality parameter @math{ncp}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; n, \mu) = \left[\sqrt{n} B\left({1\over 2}, {n\over
2}\right)\right]^{-1}\left(1+{x^2\over n}\right)^{-{(n+1)/2}}
e^{-\mu^2/ 2}
\bigg[A_n(x; \mu) + B_n(x; \mu)\bigg]>>>,
<<<
@example
                           2
                         mu          (- n) - 1
                       - ---         ---------
                          2    2         2
                     %e      (x  + 1)          (B(x, n, mu) + An(x, n, mu))
f(x;n,mu) =          -------------------------------------------------
                                         1  n
                                    beta(-, -) sqrt(n)
                                         2  2

@end example
>>>)
where
m4_displaymath(
<<<\eqalign{
A_n(x;\mu) &= {}_1F_1\left({n+1\over 2}; {1\over 2}; {\mu^2 x^2\over
2\left(x^2+n\right)}\right) \cr
B_n(x;\mu) &= {\sqrt{2}\mu x \over \sqrt{x^2+n}} {\Gamma\left({n\over
2} + 1\right)\over \Gamma\left({n+1\over 2}\right)}\;
{}_1F_1\left({n\over 2} + 1; {3\over 2}; {\mu^2 x^2\over
2\left(x^2+n\right)}\right)
}>>>,
<<<
@example
                                        2  2
                       n + 1    1     mu  x
 A(x, n, mu) = %f    ([-----], [-], ----------)
                 1, 1    2      2       2
                                    2 (x  + n)

                                                   2  2
                                  n        3     mu  x           n
               sqrt(2) mu %f    ([- + 1], [-], ----------) gamma(- + 1) x
                            1, 1  2        2       2             2
                                               2 (x  + n)
 B(x, n, mu) = ----------------------------------------------------------
                                     n + 1        2
                               gamma(-----) sqrt(x  + n)
                                       2
@end example
>>>)
and 
m4_math(\mu, mu) 
is the non-centrality parameter @math{ncp}.

Sometimes an extra work is necessary to get the final result.

@c ===beg===
@c load ("distrib")$
@c expand(pdf_noncentral_student_t(3,5,0.1));
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) expand(pdf_noncentral_student_t(3,5,0.1));
rat: replaced 0.01889822365046136 by 15934951/843198350 = 0.01889822365046136

rat: replaced -8.734356480209641 by -294697965/33740089 = -8.734356480209641

rat: replaced 4.136255165816327 by 51033443/12338079 = 4.136255165816332

rat: replaced 1.08061432164203 by 56754827/52520891 = 1.08061432164203

rat: replaced 0.0565127306411839 by 5608717/99246965 = 0.05651273064118384

rat: replaced -300.8069396896258 by -79782423/265228 = -300.8069396896256

rat: replaced 160.6269176184973 by 178374907/1110492 = 160.626917618497
                           7/2                         7/2
      0.04296414417400905 5      1.323650307289301e-6 5
(%o2) ------------------------ + -------------------------
         3/2   5/2                       sqrt(%pi)
        2    14    sqrt(%pi)
                                                              7/2
                                         1.94793720435093e-4 5
                                       + ------------------------
                                                   %pi
@end group
@group
(%i3) float(%);
(%o3)                  0.02080593159405671
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_noncentral_student_t}
@deffn {Function} cdf_noncentral_student_t (@var{x},@var{n},@var{ncp})
Returns the value at @var{x} of the cumulative distribution function of a
noncentral Student random variable 
m4_Noncentral_T_RV(n,ncp)
, with @math{n>0} degrees of freedom and noncentrality parameter @math{ncp}. This function has no closed form and it is numerically computed.

@c ===beg===
@c load ("distrib")$
@c cdf_noncentral_student_t(-2,5,-5);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_noncentral_student_t(-2,5,-5);
(%o2)                   0.995203009331975
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_noncentral_student_t}
@deffn {Function} quantile_noncentral_student_t (@var{q},@var{n},@var{ncp})
Returns the @var{q}-quantile of a noncentral Student random variable 
m4_Noncentral_T_RV(n,ncp)
, with @math{n>0} degrees of freedom and noncentrality parameter @math{ncp}; in other words, this is the inverse of @code{cdf_noncentral_student_t}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_noncentral_student_t}
@deffn {Function} mean_noncentral_student_t (@var{n},@var{ncp})
Returns the mean of a noncentral Student random variable 
m4_Noncentral_T_RV(n,ncp)
, with @math{n>1} degrees of freedom and noncentrality parameter @math{ncp}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = {\mu \sqrt{n}\; \Gamma\left(\displaystyle{n-1\over 2}\right) \over
\sqrt{2}\;\Gamma\left(\displaystyle{n\over 2}\right)}>>>,
<<<
@example
                   n - 1
          mu gamma(-----) sqrt(n)
                     2
E[X] =    -----------------------
                           n
             sqrt(2) gamma(-)
                           2
@end example
>>>)

where 
m4_math(\mu, mu) 
is the noncentrality parameter @math{ncp}.

@c ===beg===
@c load ("distrib")$
@c mean_noncentral_student_t(df,k);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) mean_noncentral_student_t(df,k);
                          df - 1
                    gamma(------) sqrt(df) k
                            2
(%o2)               ------------------------
                                     df
                       sqrt(2) gamma(--)
                                     2
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_noncentral_student_t}
@deffn {Function} var_noncentral_student_t (@var{n},@var{ncp})
Returns the variance of a noncentral Student random variable 
m4_Noncentral_T_RV(n,ncp)
, with @math{n>2} degrees of freedom and noncentrality parameter @math{ncp}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {n(\mu^2+1)\over n-2} - {n\mu^2\; \Gamma\left(\displaystyle{n-1\over 2}\right)^2
\over 2\Gamma\left(\displaystyle{n\over 2}\right)^2}>>>,
<<<
@example
                             2 n - 1       2
               2        gamma (-----) n ncp
         n (ncp  + 1)            2
V[X] =   ------------ - --------------------
            n - 2                  2 n
                            2 gamma (-)
                                     2
@end example
>>>)

where 
m4_math(\mu, mu) 
is the noncentrality parameter @math{ncp}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_noncentral_student_t}
@deffn {Function} std_noncentral_student_t (@var{n},@var{ncp})
Returns the standard deviation of a noncentral Student random variable 
m4_Noncentral_T_RV(n,ncp)
, with @math{n>2} degrees of freedom and noncentrality parameter @math{ncp}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = \sqrt{{n(\mu^2+1)\over n-2} - {n\mu^2\; \Gamma\left(\displaystyle{n-1\over 2}\right)^2
\over 2\Gamma\left(\displaystyle{n\over 2}\right)^2}}>>>,
<<<
@example
                             2 n - 1       2
                    2        gamma (-----) n ncp
              n (ncp  + 1)            2
D[X] =   sqrt(------------ - --------------------)
                 n - 2                  2 n
                                 2 gamma (-)
                                          2
@end example
>>>,
<<<>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_noncentral_student_t}
@deffn {Function} skewness_noncentral_student_t (@var{n},@var{ncp})
Returns the skewness coefficient of a noncentral Student random
variable 
m4_Noncentral_T_RV(n,ncp)
, with @math{n>3} degrees of freedom and noncentrality parameter @math{ncp}. To make use of this function, write first @code{load("distrib")}.

@c The TeX form is obtained from
@c tex(skewness_noncentral_student_t(n,mu)).  Likewise the info form
@c is just cut-n-pasted from maxima's terminal output.
If @math{U} is a non-central Student's @math{t} random variable with
@math{n} degrees of freedom and a noncentrality parameter 
m4_mathcomma(\mu, mu) 
the skewness is
m4_displaymath(
<<<\eqalign{
SK[U] &= 
{\mu\sqrt{n}\,\Gamma\left({{n-1}\over{2}}\right)
\over{\sqrt{2}\Gamma\left({{n
 }\over{2}}\right)\sigma^{3}}}\left({{n
 \left(2n+\mu^2-3\right)}\over{\left(n-3\right)\left(n-2\right)}}
 -2\sigma^2\right) \cr
 \sigma^2 &= {{n\left(\mu^2+1\right)}\over{n-2}}-{{n \mu^2\,
 \Gamma\left({{n-1}\over{2}}\right)^2}\over{2\Gamma\left({{n
 }\over{2}}\right)^2}}
}
>>>,
<<<
@example
SK[U] = 
                                     2
          n - 1           n (2 n + mu  - 3)
(mu gamma(-----) sqrt(n) (-----------------
            2              (n - 3) (n - 2)
                      2      2 n - 1
         2          mu  gamma (-----) n
      (mu  + 1) n                2
 - 2 (----------- - -------------------)))
         n - 2                 2 n
                        2 gamma (-)
                                 2
                                    2      2 n - 1
                       2          mu  gamma (-----) n
                n   (mu  + 1) n                2      3/2
/(sqrt(2) gamma(-) (----------- - -------------------)   )
                2      n - 2                 2 n
                                      2 gamma (-)
                                               2
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_noncentral_student_t}
@deffn {Function} kurtosis_noncentral_student_t (@var{n},@var{ncp})
Returns the kurtosis coefficient of a noncentral Student random
variable 
m4_Noncentral_T_RV(n,ncp)
, with @math{n>4} degrees of freedom and noncentrality parameter @math{ncp}. To make use of this function, write first @code{load("distrib")}.

If @math{U} is a non-central Student's @math{t} random variable with
@math{n} degrees of freedom and a noncentrality parameter 
m4_mathcomma(\mu, mu) 
the kurtosis is

@c The formula we see can be basically derived by computing
@c (kurtosis_noncentral_student_t(n,mu)+3)*var_noncentral_student_t(n,mu)^2,
@c which comes from the definition of kurtosis.  The rest is then
@c obtained by replacing a constant by F.
m4_displaymath(
<<<\eqalign{
KU[U] &=
{\mu_4\over \sigma^4} - 3\cr
  \mu_4 &= {{\left(\mu^4+6\mu^2+3\right)n^2}\over{(n-4)(n-2)}}
 -\left({{n\left(3(3n-5)+\mu^2(n+1)\right)
 }\over{(n-3)(n-2)}}-3\sigma^2\right) F \cr
 \sigma^2 &= {{n\left(\mu^2+1\right)}\over{n-2}}-{{n \mu^2
 \Gamma\left({{n-1}\over{2}}\right)^2}\over{2\Gamma\left({{n
 }\over{2}}\right)^2}} \cr
 F &= {n\mu^2\Gamma\left({n-1\over 2}\right)^2 \over
 2\sigma^4\Gamma\left({n\over 2}\right)^2}
}>>>,
<<<
@example
KU[U] = 
    4       2       2
 (mu  + 6 mu  + 3) n       2      2 n - 1
(-------------------- - (mu  gamma (-----) n
   (n - 4) (n - 2)                    2
                                                     2      2 n - 1
                     2                  2          mu  gamma (-----) n
  n (3 (3 n - 5) + mu  (n + 1))      (mu  + 1) n                2
 (----------------------------- - 3 (----------- - -------------------)))
         (n - 3) (n - 2)                n - 2                 2 n
                                                       2 gamma (-)
                                                                2
                                 2      2 n - 1
                    2          mu  gamma (-----) n
         2 n     (mu  + 1) n                2      2
/(2 gamma (-)))/(----------- - -------------------)  - 3
           2        n - 2                 2 n
                                   2 gamma (-)
                                            2
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_noncentral_student_t}
@deffn {Function} random_noncentral_student_t (@var{n},@var{ncp}) @
@fname{random_noncentral_student_t} (@var{n},@var{ncp},@var{m})

Returns a noncentral Student random variate 
m4_Noncentral_T_RV(n,ncp)
, with @math{n>0}. Calling @code{random_noncentral_student_t} with a third argument @var{m}, a random sample of size @var{m} will be simulated.

The implemented algorithm is based on the fact that if @var{X} is a
normal random variable 
m4_Normal_RV(ncp,1) 
and @math{S^2} is
a 
m4_math(\chi^2, chi square) 
random variable with @var{n} degrees of freedom, 
m4_Chi2_RV(n)
, then
m4_displaymath(
<<<U={{X}\over{\sqrt{{S^2}\over{n}}}}>>>,
<<<
@example
                           X
                 U = -------------
                     /   2  \ 1/2
                     |  S   |
                     | ---  |
                     \  n   /
@end example
>>>)
is a noncentral Student random variable with @math{n} degrees of
freedom and noncentrality parameter @math{ncp}, 
m4_Noncentral_T_RV(n,ncp)
.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Chi-squared Random Variable, Noncentral Chi-squared Random Variable, Noncentral Student's t Random Variable, Functions and Variables for continuous distributions
@subsection Chi-squared Random Variable

Let 
m4_math(<<<X_1, X_2, \ldots, X_n>>>, <<<X_1, X_2, ...,
X_k>>>) 
be independent and identically distributed 
m4_Normal_RV(0,1) 
variables.  Then
m4_displaymath(
<<<X^2 = \sum_{i=1}^n X_i^2>>>,
<<<@math{X^2 = sum(X_i^2, i, 1, n)}>>>)

is said to follow a chi-square distribution with @math{n} degrees of
freedom.

@anchor{pdf_chi2}
@deffn {Function} pdf_chi2 (@var{x},@var{n})
Returns the value at @var{x} of the density function of a Chi-square
random variable 
m4_Chi2_RV(n)
, with @math{n>0}.
The 
m4_Chi2_RV(n) 
random variable is equivalent to the 
m4_Gamma_RV(n/2,2)
.

The pdf is

m4_displaymath(
<<<f(x; n) =
\cases{
 \displaystyle{x^{n/2-1} e^{-x/2} \over 2^{n/2}
 \Gamma\left(\displaystyle{n\over 2}\right)} & for $x
 > 0$ \cr
\cr
 0 & otherwise
}>>>,
<<<
@example
             [  n/2 - 1   - x/2
             [ x        %e      unit_step(x)
             [ -----------------------------     for x >= 0
             [               n   n/2
   f(x, n) = [         gamma(-) 2
             [               2
             [
             [ 0                                 otherwise
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c pdf_chi2(x,n);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) pdf_chi2(x,n);
                   n/2 - 1   - x/2
                  x        %e      unit_step(x)
(%o2)             -----------------------------
                                n   n/2
                          gamma(-) 2
                                2
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_chi2}
@deffn {Function} cdf_chi2 (@var{x},@var{n})
Returns the value at @math{x} of the cumulative distribution function of a
Chi-square random variable 
m4_Chi2_RV(n)
, with @math{n>0}.

The cdf is
m4_displaymath(
<<<F(x; n) =
\cases{
1 - Q\left(\displaystyle{n\over 2}, {x\over 2}\right) & $x > 0$ \cr
0 & otherwise
}>>>,
<<<
@example
          [        n  x
          [ (1 - Q(-, -))     for x >= 0
          [        2  2
F(x, n) = [
          [ 0                 otherwise
@end example
>>>)
where @math{Q(a,z)} is the @ref{gamma_incomplete_regularized} function.

@c ===beg===
@c load ("distrib")$
@c cdf_chi2(3,4);
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_chi2(3,4);
                                                 3
(%o2)        1 - gamma_incomplete_regularized(2, -)
                                                 2
@end group
@group
(%i3) float(%);
(%o3)                  0.4421745996289252
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_chi2}
@deffn {Function} quantile_chi2 (@var{q},@var{n})
Returns the @var{q}-quantile of a Chi-square random variable 
m4_Chi2_RV(n)
, with @math{n>0}; in other words, this is the inverse of @code{cdf_chi2}. Argument @var{q} must be an element of @math{[0,1]}.

This function has no closed form and it is numerically computed.

@c ===beg===
@c load ("distrib")$
@c quantile_chi2(0.99,9);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) quantile_chi2(0.99,9);
(%o2)                   21.66599433346194
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_chi2}
@deffn {Function} mean_chi2 (@var{n})
Returns the mean of a Chi-square random variable 
m4_Chi2_RV(n)
, with @math{n>0}.

The 
m4_Chi2_RV(n) 
random variable is equivalent to the 
m4_Gamma_RV(n/2,2)
.

The mean is
m4_displaymath(
<<<E[X] = n>>>,
<<<
@example
                            E[X] = n
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c mean_chi2(n);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) mean_chi2(n);
(%o2)                           n
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_chi2}
@deffn {Function} var_chi2 (@var{n})
Returns the variance of a Chi-square random variable 
m4_Chi2_RV(n)
, with @math{n>0}.

The 
m4_Chi2_RV(n) 
random variable is equivalent to the 
m4_Gamma_RV(n/2,2)
.

The variance is
m4_displaymath(
<<<V[X] = 2n>>>,
<<<
@example
                           V[X] = 2 n
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c var_chi2(n);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) var_chi2(n);
(%o2)                          2 n
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_chi2}
@deffn {Function} std_chi2 (@var{n})
Returns the standard deviation of a Chi-square random variable 
m4_Chi2_RV(n)
, with @math{n>0}.

The 
m4_Chi2_RV(n) 
random variable is equivalent to the 
m4_Gamma_RV(n/2,2)
.

The standard deviation is
m4_displaymath(
<<<D[X] = \sqrt{2n}>>>,
<<<
@example
                     D[X] = sqrt(2) sqrt(n)
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c std_chi2(n);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) std_chi2(n);
(%o2)                    sqrt(2) sqrt(n)
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_chi2}
@deffn {Function} skewness_chi2 (@var{n})
Returns the skewness coefficient of a Chi-square random variable 
m4_Chi2_RV(n)
, with @math{n>0}.

The 
m4_Chi2_RV(n) 
random variable is equivalent to the 
m4_Gamma_RV(n/2,2)
.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = \sqrt{8\over n}>>>,
<<<
@example
                                   3/2
                                  2
                         SK[X] = -------
                                 sqrt(n)
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c skewness_chi2(n);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) skewness_chi2(n);
                               3/2
                              2
(%o2)                        -------
                             sqrt(n)
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_chi2}
@deffn {Function} kurtosis_chi2 (@var{n})
Returns the kurtosis coefficient of a Chi-square random variable 
m4_Chi2_RV(n)
, with @math{n>0}.

The 
m4_Chi2_RV(n) 
random variable is equivalent to the 
m4_Gamma_RV(n/2,2)
.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {12\over n}>>>,
<<<
@example
                                   12
                           KU[X] = --
                                   n
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c kurtosis_chi2(n);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) kurtosis_chi2(n);
                               12
(%o2)                          --
                               n
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_chi2}
@deffn {Function} random_chi2 (@var{n}) @
@fname{random_chi2} (@var{n},@var{m})

Returns a Chi-square random variate 
m4_Chi2_RV(n)
, with @math{n>0}. Calling @code{random_chi2} with a second argument @var{m}, a random sample of size @var{m} will be simulated.

The simulation is based on the Ahrens-Cheng algorithm. See @code{random_gamma} for details.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Noncentral Chi-squared Random Variable, F Random Variable, Chi-squared Random Variable, Functions and Variables for continuous distributions
@subsection Noncentral Chi-squared Random Variable

Let 
m4_math(<<<X_1, X_2, ..., X_n>>>, <<<X[1], X[2], ..., X[n]>>>) 
be @math{n} 
independent normally distributed random variables with
means 
m4_math(\mu_k, mu[k]) 
and unit variances.  Then the random variable

m4_displaymath(
<<<\sum_{k=1}^n X_k^2>>>,
<<<@math{sum(X[k]^2, k, 1, n)}>>>)

has a noncentral 
m4_math(\chi^2, chi-squared)
distribution.  The
number of degrees of freedom is @math{n}, and the noncentrality
parameter is defined by

m4_displaymath(
<<<\sum_{k=1}^n \mu_k^2>>>,
<<<@math{sum(mu[k]^2, k, 1, n)}>>>)

@anchor{pdf_noncentral_chi2}
@deffn {Function} pdf_noncentral_chi2 (@var{x},@var{n},@var{ncp})
Returns the value at @math{x} of the density function of a 
noncentral 
m4_math(\chi^2, Chi-square) 
random
variable 
m4_noncentral_chi2(n,ncp)
, with @math{n>0} and noncentrality
parameter 
m4_mathdot(<<<ncp \ge 0>>>, <<<ncp >= 0>>>) 
To 
make use of this function, write first @code{load("distrib")}.

For @math{x < 0}, the pdf is 0, and for 
m4_math(x \ge 0, x >= 0) 
the pdf is
m4_displaymath(
<<<f(x; n, \lambda) =
{1\over 2}e^{-(x+\lambda)/2} \left(x\over
\lambda\right)^{n/4-1/2}I_{{n\over 2} - 1}\left(\sqrt{n \lambda}\right)
>>>,
<<<
@example
 f(x, n, ncp) = 
                                                       (- x) - ncp
                                                       -----------
                  n                     x  n/4 - 1/2        2
         bessel_i(- - 1, sqrt(ncp x)) (---)          %e            unit_step(x)
                  2                    ncp
         ----------------------------------------------------------------------
                                           2
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_noncentral_chi2}
@deffn {Function} cdf_noncentral_chi2 (@var{x},@var{n},@var{ncp})
Returns the value at @var{x} of the cumulative distribution function of a
noncentral Chi-square random variable 
m4_noncentral_chi2(n,ncp)
, with
@math{n>0} and noncentrality parameter 
m4_mathdot(<<<ncp \ge 0>>>, <<<ncp >= 0>>>) 
To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_noncentral_chi2}
@deffn {Function} quantile_noncentral_chi2 (@var{q},@var{n},@var{ncp})
Returns the @var{q}-quantile of a noncentral Chi-square random
variable 
m4_noncentral_chi2(n,ncp)
, with @math{n>0} and noncentrality
parameter 
m4_math(<<<ncp \ge 0>>>, <<<ncp >= 0>>>)
; in other words, this is the inverse of @code{cdf_noncentral_chi2}. Argument @var{q} must be an element of @math{[0,1]}.

This function has no closed form and it is numerically computed.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_noncentral_chi2}
@deffn {Function} mean_noncentral_chi2 (@var{n},@var{ncp})
Returns the mean of a noncentral Chi-square random variable
m4_noncentral_chi2(n,ncp)
, with @math{n>0} and noncentrality parameter 
m4_mathdot(<<<ncp \ge 0>>>, <<<ncp >= 0>>>)

The mean is
m4_displaymath(
<<<E[X] = n + \mu>>>,
<<<
@example
                         E[X] = ncp + n
@end example
>>>)
@ifnotinfo
where 
m4_math(\mu) 
is the noncentrality parameter @var{ncp}.
@end ifnotinfo


@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_noncentral_chi2}
@deffn {Function} var_noncentral_chi2 (@var{n},@var{ncp})
Returns the variance of a noncentral Chi-square random variable
m4_noncentral_chi2(n,ncp)
, with @math{n>0} and noncentrality parameter 
m4_mathdot(<<<ncp \ge 0>>>, <<<ncp >= 0>>>)

The variance is
m4_displaymath(
<<<V[X] = 2(n+2\mu)>>>,
<<<
@example
                      V[X] = 2 (2 ncp + n)
@end example
>>>)
@ifnotinfo
where 
m4_math(\mu) 
is the noncentrality parameter @var{ncp}.
@end ifnotinfo

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_noncentral_chi2}
@deffn {Function} std_noncentral_chi2 (@var{n},@var{ncp})
Returns the standard deviation of a noncentral Chi-square random
variable 
m4_noncentral_chi2(n,ncp)
, with @math{n>0} and noncentrality
parameter 
m4_mathdot(<<<ncp \ge 0>>>, <<<ncp >= 0>>>)

The standard deviation is
m4_displaymath(
<<<D[X] = \sqrt{2(n+2\mu)}>>>,
<<<
@example
                 D[X] = sqrt(2) sqrt(2 ncp + n)
@end example
>>>)
@ifnotinfo
where 
m4_math(\mu) 
is the noncentrality parameter @var{ncp}.
@end ifnotinfo
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_noncentral_chi2}
@deffn {Function} skewness_noncentral_chi2 (@var{n},@var{ncp})
Returns the skewness coefficient of a noncentral Chi-square random
variable
m4_noncentral_chi2(n,ncp)
, with @math{n>0} and noncentrality
parameter 
m4_mathdot(<<<ncp \ge 0>>>, <<<ncp >= 0>>>)

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {2^{3/2}(n+3\mu) \over (n+2\mu)^{3/2}}>>>,
<<<
@example
          3/2
         2    (n + 3 mu)
 SK[X] = ---------------
                    3/2
          (n + 2 mu)

@end example
>>>)
@ifnotinfo
where 
m4_math(\mu) 
is the noncentrality parameter @var{ncp}.
@end ifnotinfo


@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_noncentral_chi2}
@deffn {Function} kurtosis_noncentral_chi2 (@var{n},@var{ncp})
Returns the kurtosis coefficient of a noncentral Chi-square random
variable
m4_noncentral_chi2(n,ncp)
, with @math{n>0} and noncentrality
parameter 
m4_mathdot(<<<ncp \ge 0>>>, <<<ncp >= 0>>>)

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {12(n+4\mu)\over (2+2\mu)^2}>>>,
<<<
@example
           12 (n + 4 mu)
   KU[X] = -------------
                      2
            (n + 2 mu)
@end example
>>>)
@ifnotinfo
where 
m4_math(\mu) 
is the noncentrality parameter @var{ncp}.
@end ifnotinfo

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_noncentral_chi2}
@deffn {Function} random_noncentral_chi2 (@var{n},@var{ncp}) @
@fname{random_noncentral_chi2} (@var{n},@var{ncp},@var{m})

Returns a noncentral Chi-square random variate
m4_noncentral_chi2(n,ncp)
, with @math{n>0} and noncentrality parameter 
m4_mathdot(<<<ncp \ge 0>>>, <<<ncp >= 0>>>) 
Calling @code{random_noncentral_chi2} with a third argument @var{m}, a random sample of size @var{m} will be simulated.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn



@node F Random Variable, Exponential Random Variable, Noncentral Chi-squared Random Variable, Functions and Variables for continuous distributions
@subsection F Random Variable

Let @math{S_1} and @math{S_2} be independent random variables with
a 
m4_math(\chi^2, chi-squared) 
distribution with degrees of freedom
@math{n} and @math{m}, respectively.  Then
m4_displaymath(
<<<F = {S_1/n \over S_2/m}>>>,
<<<@math{F = (S_1/n)/(S_2/m)}>>>) has an @math{F} distribution with @math{n} and @math{m} degrees of
freedom.

@anchor{pdf_f}
@deffn {Function} pdf_f (@var{x},@var{m},@var{n})
Returns the value at @var{x} of the density function of a F random variable @math{F(m,n)}, with @math{m,n>0}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; m, n) =
\cases{
B\left(\displaystyle{m\over 2}, \displaystyle{n\over 2}\right)^{-1}
\left(\displaystyle{m\over n}\right)^{m/ 2}
x^{m/2-1}
\left(1 + \displaystyle{m\over n}x\right)^{-\left(n+m\right)/2} & $x >
0$ \cr
\cr
0 & otherwise
}>>>,
<<<
@example
 f(x, m, n) = 
                                                         (- n) - m
                                                         ---------
                    n n/2       n + m   n/2 - 1  n x         2
                   (-)    gamma(-----) x        (--- + 1)          unit_step(x)
                    m             2               m
                   ------------------------------------------------------------
                                              m        n
                                        gamma(-) gamma(-)
                                              2        2

@end example>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_f}
@deffn {Function} cdf_f (@var{x},@var{m},@var{n})
Returns the value at @var{x} of the cumulative distribution function of a F random variable @math{F(m,n)}, with @math{m,n>0}.

The cdf is
m4_displaymath(
<<<F(x; m, n) =
\cases{
1 - I_z\left(\displaystyle{m\over 2}, {n\over 2}\right) & $x > 0$ \cr
0 & otherwise
}>>>,
<<<
@example
                                               n  m     n
 F(x, m, n) = (1 - beta_incomplete_regularized(-, -, -------))
                                               2  2  m x + n
                                                             unit_step(x)
@end example
>>>)

@ifnotinfo
where
m4_displaymath(
<<<z = {n\over mx+n}>>>,
<<<>>>)

and 
m4_math(I_z(a,b)) 
is the @ref{beta_incomplete_regularized}
function.
@end ifnotinfo
@c ===beg===
@c load ("distrib")$
@c cdf_f(2,3,9/4);
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_f(2,3,9/4);
                                            9  3  3
(%o2)       1 - beta_incomplete_regularized(-, -, --)
                                            8  2  11
@end group
@group
(%i3) float(%);
(%o3)                  0.6675672817900802
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_f}
@deffn {Function} quantile_f (@var{q},@var{m},@var{n})
Returns the @var{q}-quantile of a F random variable @math{F(m,n)}, with @math{m,n>0}; in other words, this is the inverse of @code{cdf_f}. Argument @var{q} must be an element of @math{[0,1]}.

@c ===beg===
@c load ("distrib")$
@c quantile_f(2/5,sqrt(3),5);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) quantile_f(2/5,sqrt(3),5);
(%o2)                  0.5189478385736904
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_f}
@deffn {Function} mean_f (@var{m},@var{n})
Returns the mean of a F random variable @math{F(m,n)}, with @math{m>0, n>2}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = {n\over n-2}>>>,
<<<
@example
                                   n
                          E[X] = -----
                                 n - 2
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_f}
@deffn {Function} var_f (@var{m},@var{n})
Returns the variance of a F random variable @math{F(m,n)}, with @math{m>0, n>4}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {2n^2(n+m-2) \over m(n-4)(n-2)^2}>>>,
<<<
@example
               2
            2 n  (n + m - 2)
    V[X] = ------------------
                            2
           m (n - 4) (n - 2)
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_f}
@deffn {Function} std_f (@var{m},@var{n})
Returns the standard deviation of a F random variable @math{F(m,n)}, with @math{m>0, n>4}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = {\sqrt{2}\, n \over n-2} \sqrt{n+m-2\over m(n-4)}>>>,
<<<
@example
                        n         n + m - 2
  D[X] = sqrt(2) sqrt(-----) sqrt(---------)
                      n - 2       m (n - 4)
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_f}
@deffn {Function} skewness_f (@var{m},@var{n})
Returns the skewness coefficient of a F random variable @math{F(m,n)}, with @math{m>0, n>6}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {(n+2m-2)\sqrt{8(n-4)} \over (n-6)\sqrt{m(n+m-2)}}>>>,
<<<
@example
          3/2
         2    sqrt(n - 4) (n + 2 m - 2)
 SK[X] = ------------------------------
          (n - 6) sqrt(m (n + m - 2))

@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_f}
@deffn {Function} kurtosis_f (@var{m},@var{n})
Returns the kurtosis coefficient of a F random variable @math{F(m,n)}, with @math{m>0, n>8}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = 12{m(n+m-2)(5n-22) + (n-4)(n-2)^2 \over m(n-8)(n-6)(n+m-2)}>>>,
<<<
@example
                                                        2
          12 (m (n + m - 2) (5 n - 22) + (n - 4) (n - 2) )
  KU[X] = ------------------------------------------------
                   m (n - 8) (n - 6) (n + m - 2)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_f}
@deffn {Function} random_f (@var{m},@var{n}) @
@fname{random_f} (@var{m},@var{n},@var{k})

Returns a F random variate @math{F(m,n)}, with @math{m,n>0}. Calling @code{random_f} with a third argument @var{k}, a random sample of size @var{k} will be simulated.

The simulation algorithm is based on the fact that if @var{X} is a
@math{Chi^2(m)} random variable and @math{Y} is a 
m4_Chi2_RV(n) 
random variable, then
m4_displaymath(
<<<F={{n X}\over{m Y}}>>>,
<<<
@example
                        n X
                    F = ---
                        m Y
@end example
>>>)
is a F random variable with @var{m} and @var{n} degrees of freedom, @math{F(m,n)}.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Exponential Random Variable, Lognormal Random Variable, F Random Variable, Functions and Variables for continuous distributions
@subsection Exponential Random Variable

The @emph{exponential distribution} is the probablity distribution of
the time between events in a process where the events occur
continuously and independently at a constant average rate.

@anchor{pdf_exp}
@deffn {Function} pdf_exp (@var{x},@var{m})
Returns the value at @var{x} of the density function of an 
m4_Exponential_RV(m) 
random variable, with @math{m>0}.

The 
m4_Exponential_RV(m) 
random variable is equivalent to the 
m4_Weibull_RV(1,1/m)
.

The pdf is
m4_displaymath(
<<<f(x; m) =
\cases{
me^{-mx} & for $x \ge 0$ \cr
0 & otherwise
}>>>,
<<<
@example
          [ m*exp(-m*x) for x >= 0
f(x, m) = [
          [ 0           otherwise
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c pdf_exp(x,m);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) pdf_exp(x,m);
                         - m x
(%o2)                m %e      unit_step(x)
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_exp}
@deffn {Function} cdf_exp (@var{x},@var{m})
Returns the value at @var{x} of the cumulative distribution function of an 
m4_Exponential_RV(m) 
random variable, with @math{m>0}.

The 
m4_Exponential_RV(m) 
random variable is equivalent to the 
m4_Weibull_RV(1,1/m)
.

The cdf is
m4_displaymath(
<<<F(x; m) =
\cases{
1 - e^{-mx} & $x \ge 0$ \cr
0 & otherwise
}>>>,
<<<
@example
         [ 1 - exp(-m*x)  for x >= 0
F(x,n) = [
         [ 0              otherwise
@end example
>>>)
@c ===beg===
@c load ("distrib")$
@c cdf_exp(x,m);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_exp(x,m);
                          - m x
(%o2)              (1 - %e     ) unit_step(x)
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_exp}
@deffn {Function} quantile_exp (@var{q},@var{m})
Returns the @var{q}-quantile of an 
m4_Exponential_RV(m) 
random variable, with @math{m>0}; in other words, this is the inverse of @code{cdf_exp}. Argument @var{q} must be an element of @math{[0,1]}.

The 
m4_Exponential_RV(m) 
random variable is equivalent to the 
m4_Weibull_RV(1,1/m)
.

@c ===beg===
@c load ("distrib")$
@c quantile_exp(0.56,5);
@c quantile_exp(0.56,m);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) quantile_exp(0.56,5);
(%o2)                  0.1641961104139661
@end group
@group
(%i3) quantile_exp(0.56,m);
                       0.8209805520698303
(%o3)                  ------------------
                               m
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_exp}
@deffn {Function} mean_exp (@var{m})
Returns the mean of an 
m4_Exponential_RV(m) 
random variable, with @math{m>0}.

The 
m4_Exponential_RV(m) 
random variable is equivalent to the 
m4_Weibull_RV(1,1/m)
.

The mean is
m4_displaymath(
<<<E[X] = {1\over m}>>>,
<<<
@example
                                   1
                            E[X] = -
                                   m
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c mean_exp(m);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) mean_exp(m);
                                1
(%o2)                           -
                                m
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_exp}
@deffn {Function} var_exp (@var{m})
Returns the variance of an 
m4_Exponential_RV(m) 
random variable, with @math{m>0}.

The 
m4_Exponential_RV(m) 
random variable is equivalent to the 
m4_Weibull_RV(1,1/m)
.

The variance is
m4_displaymath(
<<<V[X] = {1\over m^2}>>>,
<<<
@example
                                   1
                              VX = --
                                    2
                                   m
@end example

>>>)
@c ===beg===
@c load ("distrib")$
@c var_exp(m);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) var_exp(m);
                               1
(%o2)                          --
                                2
                               m
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_exp}
@deffn {Function} std_exp (@var{m})
Returns the standard deviation of an 
m4_Exponential_RV(m) 
random variable, with @math{m>0}.

The 
m4_Exponential_RV(m) 
random variable is equivalent to the 
m4_Weibull_RV(1,1/m)
.

The standard deviation is
m4_displaymath(
<<<D[X] = {1\over m}>>>,
<<<
@example
                                   1
                            D[X] = -
                                   m
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c std_exp(m);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) std_exp(m);
                                1
(%o2)                           -
                                m
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_exp}
@deffn {Function} skewness_exp (@var{m})
Returns the skewness coefficient of an 
m4_Exponential_RV(m) 
random variable, with @math{m>0}.

The 
m4_Exponential_RV(m) 
random variable is equivalent to the 
m4_Weibull_RV(1,1/m)
.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = 2>>>,
<<<
@example
                            SK[X] = 2
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c skewness_exp(m);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) skewness_exp(m);
(%o2)                           2
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_exp}
@deffn {Function} kurtosis_exp (@var{m})
Returns the kurtosis coefficient of an 
m4_Exponential_RV(m) 
random variable, with @math{m>0}.

The 
m4_Exponential_RV(m)
random variable is equivalent to the 
m4_Weibull_RV(1,1/m)
.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = 6>>>,
<<<
@example
                            KU[X] = 6
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c kurtosis_exp(m);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) kurtosis_exp(m);
(%o2)                           6
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_exp}
@deffn {Function} random_exp (@var{m}) @
@fname{random_exp} (@var{m},@var{k})

Returns an 
m4_Exponential_RV(m) 
random variate, with @math{m>0}. Calling @code{random_exp} with a second argument @var{k}, a random sample of size @var{k} will be simulated.

The simulation algorithm is based on the general inverse method.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Lognormal Random Variable, Gamma Random Variable, Exponential Random Variable, Functions and Variables for continuous distributions
@subsection Lognormal Random Variable

The @emph{lognormal} distribution is distribution for a random
variable whose logarithm is normally distributed.

@anchor{pdf_lognormal}
@deffn {Function} pdf_lognormal (@var{x},@var{m},@var{s})
Returns the value at @var{x} of the density function of a 
m4_Lognormal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; m, s) =
\cases{
\displaystyle{1\over x s \sqrt{2\pi}}
\exp\left(-\displaystyle{\left(\log x - m\right)^2\over 2s^2}\right) & for $x \ge
0$ \cr
\cr
0 & for $x < 0$
}>>>,
<<<
@example
                   [                   2
                   [       (log(x) - m)
                   [     - -------------
                   [              2
                   [           2 s
                   [   %e
      f(x, m, s) = [ ---------------------      for x >= 0
                   [ sqrt(2) sqrt(%pi) s x
                   [
                   [ 0                          for x < 0
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_lognormal}
@deffn {Function} cdf_lognormal (@var{x},@var{m},@var{s})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Lognormal_RV(m,s) 
random variable, with @math{s>0}. This function is defined in terms of Maxima's built-in error function @code{erf}.

The cdf is
m4_displaymath(
<<<F(x; m, s) =
\cases{
\displaystyle{1\over 2}\left[1+{\rm erf}\left({\log x - m\over s\sqrt{2}}\right)\right] &
for $x > 0$ \cr
\cr
0 & for $x \le 0$
}>>>,
<<<
@example
                                         log(x) - m
                                     erf(----------)
                                         sqrt(2) s     1
          F(x, m, s) = unit_step(x) (--------------- + -)
                                            2          2
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c cdf_lognormal(x,m,s);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_lognormal(x,m,s);
                                 log(x) - m
                             erf(----------)
                                 sqrt(2) s     1
(%o2)          unit_step(x) (--------------- + -)
                                    2          2
@end group
@end example

See also @mrefdot{erf}

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_lognormal}
@deffn {Function} quantile_lognormal (@var{q},@var{m},@var{s})
Returns the @var{q}-quantile of a 
m4_Lognormal_RV(m,s) 
random variable, with @math{s>0}; in other words, this is the inverse of @code{cdf_lognormal}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@c ===beg===
@c load ("distrib")$
@c quantile_lognormal(95/100,0,1);
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) quantile_lognormal(95/100,0,1);
                     sqrt(2) inverse_erf(9/10)
(%o2)              %e
@end group
@group
(%i3) float(%);
(%o3)                   5.180251602233015
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_lognormal}
@deffn {Function} mean_lognormal (@var{m},@var{s})
Returns the mean of a 
m4_Lognormal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = \exp\left(m+{s^2\over 2}\right)>>>,
<<<
@example
                                   2
                                  s
                                  -- + m
                                  2
                         E[X] = %e
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_lognormal}
@deffn {Function} var_lognormal (@var{m},@var{s})
Returns the variance of a 
m4_Lognormal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = \left(\exp\left(s^2\right) - 1\right) \exp\left(2m+s^2\right)>>>,
<<<
@example
                             2         2
                            s         s  + 2 m
                  V[X] = (%e   - 1) %e

@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn

@anchor{std_lognormal}
@deffn {Function} std_lognormal (@var{m},@var{s})
Returns the standard deviation of a 
m4_Lognormal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = \sqrt{\left(\exp\left(s^2\right) - 1\right)}
\exp\left(m+{s^2\over 2}\right)>>>,
<<<
@example
                                         2
                                        s  + 2 m
                               2        --------
                              s            2
                D[X] = sqrt(%e   - 1) %e

@end example

>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_lognormal}
@deffn {Function} skewness_lognormal (@var{m},@var{s})
Returns the skewness coefficient of a 
m4_Lognormal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = \left(\exp\left(s^2\right)+2\right)\sqrt{\exp\left(s^2\right)-1}>>>,
<<<
@example
                                2          2
                               s          s
                SK[X] = sqrt(%e   - 1) (%e   + 2)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_lognormal}
@deffn {Function} kurtosis_lognormal (@var{m},@var{s})
Returns the kurtosis coefficient of a 
m4_Lognormal_RV(m,s) 
random variable, with @math{s>0}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = \exp\left(4s^2\right)+2\exp\left(3s^2\right)+3\exp\left(2s^2\right)-3>>>,
<<<
@example
                         2          2          2
                      4 s        3 s        2 s
            KU[X] = %e     + 2 %e     + 3 %e     - 3

@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_lognormal}
@deffn {Function} random_lognormal (@var{m},@var{s}) @
@fname{random_lognormal} (@var{m},@var{s},@var{n})

Returns a 
m4_Lognormal_RV(m,s) 
random variate, with @math{s>0}. Calling @code{random_lognormal} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

Log-normal variates are simulated by means of random normal variates. See @code{random_normal} for details.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Gamma Random Variable, Beta Random Variable, Lognormal Random Variable, Functions and Variables for continuous distributions
@subsection Gamma Random Variable

The @emph{gamma distribution} is a two-parameter family of probability
distributions.  Maxima uses the parameterization using the shape and
scale for the first and second parameters of the distribution.

@anchor{pdf_gamma}
@deffn {Function} pdf_gamma (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the density function of a 
m4_Gamma_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The shape parameter is @math{a}, and the scale parameter is @math{b}.

The pdf is
m4_displaymath(
<<<f(x; a, b) = {x^{a-1}e^{-x/b}\over b^a \Gamma(a)}>>>,
<<<
@example
                    a - 1   - x/b
                   x      %e      unit_step(x)
      f(x, a, b) = ---------------------------
                                     a
                           gamma(a) b
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_gamma}
@deffn {Function} cdf_gamma (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Gamma_RV(a,b) 
random variable, with @math{a,b>0}. 

The cdf is
m4_displaymath(
<<<F(x; a, b) =
\cases{
1-Q(a,{x\over b}) & for $x \ge 0$ \cr
\cr
0 & for $x < 0$
}>>>,
<<<
@example
             [ 1 - Q(a,x/b) for x>= 0
F(x, a, b) = [
             [ 0            for x < 0
@end example
>>>)
where @math{Q(a,z)} is the @ref{gamma_incomplete_regularized} function.
@c ===beg===
@c load ("distrib")$
@c cdf_gamma(3,5,21);
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_gamma(3,5,21);
                                                 1
(%o2)        1 - gamma_incomplete_regularized(5, -)
                                                 7
@end group
@group
(%i3) float(%);
(%o3)                 4.402663157376807e-7
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_gamma}
@deffn {Function} quantile_gamma (@var{q},@var{a},@var{b})
Returns the @var{q}-quantile of a 
m4_Gamma_RV(a,b) 
random variable, with @math{a,b>0}; in other words, this is the inverse of @code{cdf_gamma}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_gamma}
@deffn {Function} mean_gamma (@var{a},@var{b})
Returns the mean of a 
m4_Gamma_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = ab>>>,
<<<
@example
                           E[X] = a b
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_gamma}
@deffn {Function} var_gamma (@var{a},@var{b})
Returns the variance of a 
m4_Gamma_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = ab^2>>>,
<<<
@example
                                     2
                           V[X] = a b
@end example

>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn

@anchor{std_gamma}
@deffn {Function} std_gamma (@var{a},@var{b})
Returns the standard deviation of a 
m4_Gamma_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = b\sqrt{a}>>>,
<<<
@example
                       D[X] = sqrt(a) b
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_gamma}
@deffn {Function} skewness_gamma (@var{a},@var{b})
Returns the skewness coefficient of a 
m4_Gamma_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {2\over \sqrt{a}}>>>,
<<<
@example
                                     2
                          SK[X] = -------
                                  sqrt(a)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_gamma}
@deffn {Function} kurtosis_gamma (@var{a},@var{b})
Returns the kurtosis coefficient of a 
m4_Gamma_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {6\over a}>>>,
<<<
@example
                                     6
                             KU[X] = -
                                     a

@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_gamma}
@deffn {Function} random_gamma (@var{a},@var{b}) @
@fname{random_gamma} (@var{a},@var{b},@var{n})

Returns a 
m4_Gamma_RV(a,b) 
random variate, with @math{a,b>0}. Calling @code{random_gamma} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is a combination of two procedures, depending on the value of parameter @var{a}:

For 
m4_mathcomma(<<<a \ge 1>>>, <<<a >= 1>>>)
Cheng, R.C.H. and Feast, G.M. (1979). @var{Some simple gamma variate generators}. Appl. Stat., 28, 3, 290-295.

For 
m4_mathcomma(<<<0 \lt a \lt 1>>>, <<<0 < a < 1>>>, <<<0 < a < 1>>>)
Ahrens, J.H. and Dieter, U. (1974). @var{Computer methods for sampling from gamma, , poisson and binomial distributions}. Computing, 12, 223-246.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Beta Random Variable, Continuous Uniform Random Variable, Gamma Random Variable, Functions and Variables for continuous distributions
@subsection Beta Random Variable

The @emph{beta} distribution is a family of distributions defined over
@math{[0,1]} parameterized by two positive shape parameters @math{a},
and @math{b}.

@anchor{pdf_beta}
@deffn {Function} pdf_beta (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the density function of a 
m4_Beta_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; a, b) =
\cases{
\displaystyle{x^{a-1}(1-x)^{b-1} \over B(a,b)} & for $0 \le x \le 1$
\cr
\cr
0 & otherwise
}>>>,
<<<
@example
              [        b - 1  a - 1
              [ (1 - x)      x
 f(x, a, b) = [ -------------------   for 0 <= x <= 1
              [     beta(a, b)
              [
              [ 0                     otherwise
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn



@anchor{cdf_beta}
@deffn {Function} cdf_beta (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Beta_RV(a,b) 
random variable, with @math{a,b>0}.

The cdf is
m4_displaymath(
<<<F(x; a, b) =
\cases{
0 & $x < 0$ \cr
I_x(a,b) & $0 \le x \le 1$ \cr
1 & $x > 1$
}>>>,
<<<
@example
             [ 0                                     for x < 0
             [
F(x, a, b) = [ beta_incomplete_regularized(a, b, x)  for 0 <= x <= 1
             [
             [ 1                                     for x > 1
@end example
>>>)
@c ===beg===
@c load ("distrib")$
@c cdf_beta(1/3,15,2);
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_beta(1/3,15,2);
                               11
(%o2)                       --------
                            14348907
@end group
@group
(%i3) float(%);
(%o3)                 7.666089131388195e-7
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_beta}
@deffn {Function} quantile_beta (@var{q},@var{a},@var{b})
Returns the @var{q}-quantile of a 
m4_Beta_RV(a,b) 
random variable, with @math{a,b>0}; in other words, this is the inverse of @code{cdf_beta}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_beta}
@deffn {Function} mean_beta (@var{a},@var{b})
Returns the mean of a 
m4_Beta_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = {a\over a+b}>>>,
<<<
@example
                                    a
                           E[X] = -----
                                  b + a
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_beta}
@deffn {Function} var_beta (@var{a},@var{b})
Returns the variance of a 
m4_Beta_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {ab \over (a+b)^2(a+b+1)}>>>,
<<<
@example
                   a b
    V[X] = --------------------
                  2
           (b + a)  (b + a + 1)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn

@anchor{std_beta}
@deffn {Function} std_beta (@var{a},@var{b})
Returns the standard deviation of a 
m4_Beta_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = {1\over a+b}\sqrt{ab\over a+b+1}>>>,
<<<
@example
                   a b
           sqrt(---------)
                b + a + 1
    D[X] = ---------------
             abs(b + a)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_beta}
@deffn {Function} skewness_beta (@var{a},@var{b})
Returns the skewness coefficient of a 
m4_Beta_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {2(b-a)\sqrt{a+b+1} \over (a+b+2)\sqrt{ab}}>>>,
<<<
@example
           2 (b - a) sqrt(b + a + 1)
   SK[X] = -------------------------
             sqrt(a b) (b + a + 2)
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_beta}
@deffn {Function} kurtosis_beta (@var{a},@var{b})
Returns the kurtosis coefficient of a 
m4_Beta_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {3(a+b+1)\left(2(a+b)^2+ab(a+b-6)\right) \over
ab(a+b+2)(a+b+3)} - 3>>>,
<<<
@example
                                  2
          3 (b + a + 1) (2 (b + a)  + a b (b + a - 6))
  KU[X] = -------------------------------------------- - 3
                  a b (b + a + 2) (b + a + 3)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_beta}
@deffn {Function} random_beta (@var{a},@var{b}) @
@fname{random_beta} (@var{a},@var{b},@var{n})

Returns a 
m4_Beta_RV(a,b) 
random variate, with @math{a,b>0}. Calling @code{random_beta} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is defined in Cheng, R.C.H.  (1978). @var{Generating Beta Variates with Nonintegral Shape Parameters}. Communications of the ACM, 21:317-322

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Continuous Uniform Random Variable, Logistic Random Variable, Beta Random Variable, Functions and Variables for continuous distributions
@subsection Continuous Uniform Random Variable

The @emph{continuous uniform} distribution is constant over the
interval @math{[a,b]} and is zero elsewhere.

@anchor{pdf_continuous_uniform}
@deffn {Function} pdf_continuous_uniform (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the density function of a
m4_Continuous_Uniform_RV(a,b) 
random variable, with 
m4_mathdot(<<<a \lt b>>>, <<<a < b>>>, <<<a < b>>>) 
To make use of this function, write first @code{load("distrib")}.

The pdf
m4_displaymath(
<<<f(x; a, b) =
\cases{
\displaystyle{1\over b-a} & for $0 \le x \le 1$ \cr
\cr
0 & otherwise
}>>>,
<<<
@example
             [ 1/(b-a) for 0 <= x <= 1
f(x, a, b) = [
             [ 0       otherwise
@end example
>>>)
and is 0 otherwise.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_continuous_uniform}
@deffn {Function} cdf_continuous_uniform (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a
m4_Continuous_Uniform_RV(a,b) 
random variable, with 
m4_mathdot(<<<a \lt b>>>, <<<a < b>>>, <<<a < b>>>) 
To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; a, b) =
\cases{
0 & for $x < a$ \cr
\cr
\displaystyle{x-a\over b-a} & for $a \le x \le b$ \cr
\cr
1 & for $x > b$
}>>>,
<<<
@example
           [ 0              for x < a
F(x,a,b) = [ (x-a)/(b-a)    for a <= x <= b
           [ 1              for x > b
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_continuous_uniform}
@deffn {Function} quantile_continuous_uniform (@var{q},@var{a},@var{b})
Returns the @var{q}-quantile of a 
m4_Continuous_Uniform_RV(a,b) 
random
variable, with 
m4_math(<<<a \lt b>>>, <<<a < b>>>, <<<a < b>>>)
; in other words, this is the inverse of @code{cdf_continuous_uniform}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_continuous_uniform}
@deffn {Function} mean_continuous_uniform (@var{a},@var{b})
Returns the mean of a 
m4_Continuous_Uniform_RV(a,b) 
random variable,
with 
m4_mathdot(<<<a \lt b>>>, <<<a < b>>>, <<<a < b>>>) 
To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = {a+b\over 2}>>>,
<<<
@example
                                  b + a
                           E[X] = -----
                                    2

@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_continuous_uniform}
@deffn {Function} var_continuous_uniform (@var{a},@var{b})
Returns the variance of a 
m4_Continuous_Uniform_RV(a,b) 
random
variable, with 
m4_mathdot(<<<a \lt b>>>, <<<a < b>>>, <<<a < b>>>) 
To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {(b-a)^2\over 12}>>>,
<<<
@example
                                        2
                                 (b - a)
                          V[X] = --------
                                    12
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn

@anchor{std_continuous_uniform}
@deffn {Function} std_continuous_uniform (@var{a},@var{b})
Returns the standard deviation of a 
m4_Continuous_Uniform_RV(a,b)
random variable, with 
m4_mathdot(<<<a \lt b>>>, <<<a < b>>>, <<<a < b>>>) 
To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = {b-a \over 2\sqrt{3}}>>>,
<<<
@example
                                abs(b - a)
                         D[X] = ----------
                                2 sqrt(3)

@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_continuous_uniform}
@deffn {Function} skewness_continuous_uniform (@var{a},@var{b})
Returns the skewness coefficient of a 
m4_Continuous_Uniform_RV(a,b)
random variable, with 
m4_mathdot(<<<a \lt b>>>, <<<a < b>>>, <<<a < b>>>) 
To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = 0>>>,
<<<
@example
                            SK[X] = 0
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_continuous_uniform}
@deffn {Function} kurtosis_continuous_uniform (@var{a},@var{b})
Returns the kurtosis coefficient of a 
m4_Continuous_Uniform_RV(a,b)
random variable, with 
m4_mathdot(<<<a \lt b>>>, <<<a < b>>>, <<<a < b>>>) 
To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = -{6\over5}>>>,
<<<
                                     6
                           KU[X] = - -
                                     5
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_continuous_uniform}
@deffn {Function} random_continuous_uniform (@var{a},@var{b}) @
@fname{random_continuous_uniform} (@var{a},@var{b},@var{n})

Returns a 
m4_Continuous_Uniform_RV(a,b) 
random variate, with 
m4_mathdot(<<<a \lt b>>>, <<<a < b>>>, <<<a < b>>>) 
Calling @code{random_continuous_uniform} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

This is a direct application of the @code{random} built-in Maxima function.

See also @mrefdot{random} To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Logistic Random Variable, Pareto Random Variable, Continuous Uniform Random Variable, Functions and Variables for continuous distributions
@subsection Logistic Random Variable

The @emph{logistic} distribution is a continuous distribution where
its cumulative distribution function is the logistic function.

@anchor{pdf_logistic}
@deffn {Function} pdf_logistic (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the density function of a 
m4_Logistic_RV(a,b) 
random variable , with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

@math{a} is the location parameter and @math{b} is the scale
parameter.

The pdf is
m4_displaymath(
<<<f(x; a, b) = {e^{-(x-a)/b} \over b\left(1 + e^{-(x-a)/b}\right)^2}>>>,
<<<
@example
                                      a - x
                                      -----
                                        b
                                    %e
                   f(x, a, b) = ----------------
                                     a - x
                                     -----
                                       b       2
                                b (%e      + 1)
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_logistic}
@deffn {Function} cdf_logistic (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Logistic_RV(a,b) 
random variable , with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; a, b) = {1\over 1+e^{-(x-a)/b}}>>>,
<<<
@example
                                       1
                     F(x, a, b) = -----------
                                    a - x
                                    -----
                                      b
                                  %e      + 1
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_logistic}
@deffn {Function} quantile_logistic (@var{q},@var{a},@var{b})
Returns the @var{q}-quantile of a 
m4_Logistic_RV(a,b) 
random variable , with @math{b>0}; in other words, this is the inverse of @code{cdf_logistic}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_logistic}
@deffn {Function} mean_logistic (@var{a},@var{b})
Returns the mean of a 
m4_Logistic_RV(a,b) 
random variable , with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = a>>>,
<<<
@example
                            E[X] = a
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_logistic}
@deffn {Function} var_logistic (@var{a},@var{b})
Returns the variance of a 
m4_Logistic_RV(a,b) 
random variable , with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {\pi^2 b^2 \over 3}>>>,
<<<
@example
                                   2  2
                                %pi  b
                         V[X] = -------
                                   3

@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_logistic}
@deffn {Function} std_logistic (@var{a},@var{b})
Returns the standard deviation of a 
m4_Logistic_RV(a,b) 
random variable , with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = {\pi b\over \sqrt{3}}>>>,
<<<
@example
                               %pi abs(b)
                        D[X] = ----------
                                sqrt(3)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_logistic}
@deffn {Function} skewness_logistic (@var{a},@var{b})
Returns the skewness coefficient of a 
m4_Logistic_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = 0>>>,
<<<
@example
                            SK[X] = 0
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_logistic}
@deffn {Function} kurtosis_logistic (@var{a},@var{b})
Returns the kurtosis coefficient of a 
m4_Logistic_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {6\over 5}>>>,
<<<
@example
                                    6
                            KU[X] = -
                                    5
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_logistic}
@deffn {Function} random_logistic (@var{a},@var{b}) @
@fname{random_logistic} (@var{a},@var{b},@var{n})

Returns a 
m4_Logistic_RV(a,b) 
random variate, with @math{b>0}. Calling @code{random_logistic} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is based on the general inverse method.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Pareto Random Variable, Weibull Random Variable, Logistic Random Variable, Functions and Variables for continuous distributions
@subsection Pareto Random Variable

@anchor{pdf_pareto}
@deffn {Function} pdf_pareto (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the density function of a 
m4_Pareto_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; a, b) = 
\cases{
\displaystyle{a b^a \over x^{a+1}} & for $x \ge b$ \cr
\cr
0 & for $x < b$
}>>>,
<<<
@example
             [ a*b^a/x^(a+1)   for x >= b
f(x, a, b) = [
             [ 0               for x < b
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_pareto}
@deffn {Function} cdf_pareto (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Pareto_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; a, b) =
\cases{
1-\left(\displaystyle{b\over x}\right)^a & for $x \ge b$\cr
0 & for $x < b$
}>>>,
<<<
@example
             [ 1 - (b/x)^a     for x >= 0
F(x, a, b) = [
             [ 0               for x < 0
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_pareto}
@deffn {Function} quantile_pareto (@var{q},@var{a},@var{b})
Returns the @var{q}-quantile of a 
m4_Pareto_RV(a,b) 
random variable, with @math{a,b>0}; in other words, this is the inverse of @code{cdf_pareto}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_pareto}
@deffn {Function} mean_pareto (@var{a},@var{b})
Returns the mean of a 
m4_Pareto_RV(a,b) 
random variable, with @math{a>1,b>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = {ab\over a-1}>>>,
<<<@math{E[X] = a*b/(a-1)}>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_pareto}
@deffn {Function} var_pareto (@var{a},@var{b})
Returns the variance of a 
m4_Pareto_RV(a,b) 
random variable, with @math{a>2,b>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {ab^2\over (a-2)(a-1)^2}>>>,
<<<
@example
                              2
                           a b
              V[X] = ----------------
                                    2
                     (a - 2) (a - 1)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn

@anchor{std_pareto}
@deffn {Function} std_pareto (@var{a},@var{b})
Returns the standard deviation of a 
m4_Pareto_RV(a,b) 
random variable, with @math{a>2,b>0}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = {b\over a-1} \sqrt{a\over a-2}>>>,
<<<
@example
                              a
                       sqrt(-----) b
                            a - 2
                D[X] = -------------
                           a - 1
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn



@anchor{skewness_pareto}
@deffn {Function} skewness_pareto (@var{a},@var{b})
Returns the skewness coefficient of a 
m4_Pareto_RV(a,b) 
random variable, with @math{a>3,b>0}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {2(a+1)\over a-3} \sqrt{a-2\over a}>>>,
<<<
@example
                     2 sqrt(a - 2) (a + 1)
             SK[X] = ---------------------
                        (a - 3) sqrt(a)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_pareto}
@deffn {Function} kurtosis_pareto (@var{a},@var{b})
Returns the kurtosis coefficient of a 
m4_Pareto_RV(a,b) 
random variable, with @math{a>4,b>0}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {6\left(a^3+a^2-6*a-2\right) \over a(a-3)(a-4)} - 3>>>,
<<<
@example
                        3    2
                    6 (a  + a  - 6 a - 2)
            KU[X] = --------------------- - 3
                          2
                      a (a  - 7 a + 12)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_pareto}
@deffn {Function} random_pareto (@var{a},@var{b}) @
@fname{random_pareto} (@var{a},@var{b},@var{n})

Returns a 
m4_Pareto_RV(a,b) 
random variate, with @math{a>0,b>0}. Calling @code{random_pareto} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is based on the general inverse method.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Weibull Random Variable, Rayleigh Random Variable, Pareto Random Variable, Functions and Variables for continuous distributions
@subsection Weibull Random Variable

@anchor{pdf_weibull}
@deffn {Function} pdf_weibull (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the density function of a 
m4_Weibull_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; a, b) =
\cases{
\displaystyle{1\over b} \left({x\over b}\right)^{a-1} e^{-(x/b)^a} &
for $x \ge 0$ \cr
\cr
0 & for $x < 0$
}>>>,
<<<
@example
             [                       a
             [      x a - 1   - (x/b)
             [   a (-)      %e         unit_step(x)
             [      b
             [   ----------------------------------   for x >= 0
             [                   b
f(x, a, b) = [
             [ 0                                      for x < 0
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_weibull}
@deffn {Function} cdf_weibull (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Weibull_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; a, b) =
\cases{
1 - e^{-(x/b)^a} & for $x \ge 0$ \cr
0 & for $x < 0$
}>>>,
<<<
@example
             [ 1 - exp(-(x/b)^a)      for x >= 0
F(x, a, b) = [
             [ 0                      for x < 0
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_weibull}
@deffn {Function} quantile_weibull (@var{q},@var{a},@var{b})
Returns the @var{q}-quantile of a 
m4_Weibull_RV(a,b) 
random variable, with @math{a,b>0}; in other words, this is the inverse of @code{cdf_weibull}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_weibull}
@deffn {Function} mean_weibull (@var{a},@var{b})
Returns the mean of a 
m4_Weibull_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = b\Gamma\left(1+{1\over a}\right)>>>,
<<<
@example
                                   1
                      E[X] = gamma(- + 1) b
                                   a
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_weibull}
@deffn {Function} var_weibull (@var{a},@var{b})
Returns the variance of a 
m4_Weibull_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = b^2\left[\Gamma\left(1+{2\over a}\right) -
\Gamma\left(1+{1\over a}\right)^2\right]>>>,
<<<
@example
                         2             2 1        2
           V[X] = (gamma(- + 1) - gamma (- + 1)) b
                         a               a
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn

@anchor{std_weibull}
@deffn {Function} std_weibull (@var{a},@var{b})
Returns the standard deviation of a 
m4_Weibull_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<D[X] = b\sqrt{\Gamma\left(1+{2\over a}\right) -
\Gamma\left(1+{1\over a}\right)^2}>>>,
<<<
@example
                             2             2 1
           D[X] = sqrt(gamma(- + 1) - gamma (- + 1)) b
                             a               a
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn



@anchor{skewness_weibull}
@deffn {Function} skewness_weibull (@var{a},@var{b})
Returns the skewness coefficient of a 
m4_Weibull_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {\displaystyle\Gamma\left(1+{3\over a}\right)
-3\Gamma\left(1+{1\over a}\right)\Gamma\left(1+{2\over
a}\right)+2\Gamma\left(1+{1\over a}\right)^3
 \over
 \displaystyle\left[\Gamma\left(1+{2\over a}\right)-\Gamma\left(1+{1\over
 a}\right)^2\right]^{3/2}
} 
>>>,
<<<
@example
              3                1            2               3 1
        gamma(- + 1) - 3 gamma(- + 1) gamma(- + 1) + 2 gamma (- + 1)
              a                a            a                 a
SK[X] = ------------------------------------------------------------
                            2             2 1      3/2
                     (gamma(- + 1) - gamma (- + 1))
                            a               a
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_weibull}
@deffn {Function} kurtosis_weibull (@var{a},@var{b})
Returns the kurtosis coefficient of a 
m4_Weibull_RV(a,b) 
random variable, with @math{a,b>0}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {
\Gamma_4
 - 4\Gamma_1 \Gamma_3
 + 6\Gamma_1^2 \Gamma_2
 - 3 \Gamma_1^4
\over
 \left[\Gamma_2 - \Gamma_1^2\right]^2
} - 3>>>,
<<<
@example
                4                1            3
 KU[X] = (gamma(- + 1) - 4 gamma(- + 1) gamma(- + 1)
                a                a            a
          2 1            2               4 1
 + 6 gamma (- + 1) gamma(- + 1) - 3 gamma (- + 1))
            a            a                 a
        2             2 1      2
/(gamma(- + 1) - gamma (- + 1))  - 3
        a               a
@end example
>>>)
@ifnotinfo
where 
m4_mathdot(\Gamma_k = \Gamma\left(1+k/a\right))
@end ifnotinfo

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_weibull}
@deffn {Function} random_weibull (@var{a},@var{b}) @
@fname{random_weibull} (@var{a},@var{b},@var{n})

Returns a 
m4_Weibull_RV(a,b) 
random variate, with @math{a,b>0}. Calling @code{random_weibull} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is based on the general inverse method.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn



@node Rayleigh Random Variable, Laplace Random Variable, Weibull Random Variable, Functions and Variables for continuous distributions
@subsection Rayleigh Random Variable

The @emph{Rayleigh} distribution coincides with the 
m4_math(\chi^2,
chi-squared) 
distribution with two degrees of freedom.

@anchor{pdf_rayleigh}
@deffn {Function} pdf_rayleigh (@var{x},@var{b})
Returns the value at @var{x} of the density function of a 
m4_Rayleigh_RV(b) 
random variable, with @math{b>0}.

The 
m4_Rayleigh_RV(b) 
random variable is equivalent to the 
m4_Weibull_RV(2,1/b)
.

The pdf is
m4_displaymath(
<<<f(x; b) =
\cases{
2b^2 x e^{-b^2 x^2} & for $x \ge 0$ \cr
0 & for $x < 0$
}>>>,
<<<
@example
          [ 2*b^2*x*exp(-b^2*x^2)    for x>= 0
f(x, b) = [
          [ 0                        otherwise
@end example
>>>)
@c ===beg===
@c load ("distrib")$
@c pdf_rayleigh(x,b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) pdf_rayleigh(x,b);
                              2  2
                     2     - b  x
(%o2)             2 b  x %e        unit_step(x)
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_rayleigh}
@deffn {Function} cdf_rayleigh (@var{x},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Rayleigh_RV(b) 
random variable, with @math{b>0}.

The 
m4_Rayleigh_RV(b) 
random variable is equivalent to the 
m4_Weibull_RV(2,1/b)
.

The cdf is
m4_displaymath(
<<<F(x; b) =
\cases{
1 - e^{-b^2 x^2} & for $x \ge 0$\cr
0 & for $x < 0$
}>>>,
<<<
@example
          [ 1 - exp(-b^2*x^2)    for x >= 0
F(x, b) = [
          [ 0                    for x < 0
@end example
>>>)
@c ===beg===
@c load ("distrib")$
@c cdf_rayleigh(x,b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_rayleigh(x,b);
                            2  2
                         - b  x
(%o2)             (1 - %e       ) unit_step(x)
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_rayleigh}
@deffn {Function} quantile_rayleigh (@var{q},@var{b})
Returns the @var{q}-quantile of a 
m4_Rayleigh_RV(b) 
random variable, with @math{b>0}; in other words, this is the inverse of @code{cdf_rayleigh}. Argument @var{q} must be an element of @math{[0,1]}.

The 
m4_Rayleigh_RV(b) 
random variable is equivalent to the 
m4_Weibull_RV(2,1/b)
.

@c ===beg===
@c load ("distrib")$
@c quantile_rayleigh(0.99,b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) quantile_rayleigh(0.99,b);
                        2.145966026289347
(%o2)                   -----------------
                                b
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_rayleigh}
@deffn {Function} mean_rayleigh (@var{b})
Returns the mean of a 
m4_Rayleigh_RV(b) 
random variable, with @math{b>0}.

The 
m4_Rayleigh_RV(b) 
random variable is equivalent to the 
m4_Weibull_RV(2,1/b)
.

The mean is
m4_displaymath(
<<<E[X] = {\sqrt{\pi}\over 2b}>>>,
<<<
@example
                               sqrt(%pi)
                        E[X] = ---------
                                  2 b
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c mean_rayleigh(b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) mean_rayleigh(b);
                            sqrt(%pi)
(%o2)                       ---------
                               2 b
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_rayleigh}
@deffn {Function} var_rayleigh (@var{b})
Returns the variance of a 
m4_Rayleigh_RV(b) 
random variable, with @math{b>0}.

The 
m4_Rayleigh_RV(b) 
random variable is equivalent to the 
m4_Weibull_RV(2,1/b)
.

The variance is
m4_displaymath(
<<<V[X] = {1\over b^2}\left(1-{\pi \over 4}\right)>>>,
<<<
@example
                                    %pi
                                1 - ---
                                     4
                         V[X] = -------
                                   2
                                  b
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c var_rayleigh(b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) var_rayleigh(b);
                                 %pi
                             1 - ---
                                  4
(%o2)                        -------
                                2
                               b
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_rayleigh}
@deffn {Function} std_rayleigh (@var{b})
Returns the standard deviation of a 
m4_Rayleigh_RV(b) 
random variable, with @math{b>0}.

The 
m4_Rayleigh_RV(b) 
random variable is equivalent to the 
m4_Weibull_RV(2,1/b)
.

The standard deviation is
m4_displaymath(
<<<D[X] = {1\over b}\sqrt{\displaystyle 1 - {\pi\over 4}}>>>,
<<<
@example
                                     %pi
                            sqrt(1 - ---)
                                      4
                     D[X] = -------------
                                  b
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c std_rayleigh(b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) std_rayleigh(b);
                                   %pi
                          sqrt(1 - ---)
                                    4
(%o2)                     -------------
                                b
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_rayleigh}
@deffn {Function} skewness_rayleigh (@var{b})
Returns the skewness coefficient of a
m4_Rayleigh_RV(b) 
random variable, with @math{b>0}.

The 
m4_Rayleigh_RV(b) 
random variable is equivalent to the 
m4_Weibull_RV(2,1/b)
.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {2\sqrt{\pi}(\pi - 3)\over (4-\pi)^{3/2}}>>>,
<<<
@example
                            3/2
                         %pi      3 sqrt(%pi)
                         ------ - -----------
                           4           4
                 SK[X] = --------------------
                                  %pi 3/2
                             (1 - ---)
                                   4
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c skewness_rayleigh(b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) skewness_rayleigh(b);
                         3/2
                      %pi      3 sqrt(%pi)
                      ------ - -----------
                        4           4
(%o2)                 --------------------
                               %pi 3/2
                          (1 - ---)
                                4
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_rayleigh}
@deffn {Function} kurtosis_rayleigh (@var{b})
Returns the kurtosis coefficient of a
m4_Rayleigh_RV(b) 
random variable, with @math{b>0}.

The 
m4_Rayleigh_RV(b) 
random variable is equivalent to the 
m4_Weibull_RV(2,1/b)
.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {32-3\pi\over (4-\pi)^2} - 3>>>,
<<<
@example
                                     2
                                3 %pi
                            2 - ------
                                  16
                    KU[X] = ---------- - 3
                                 %pi 2
                            (1 - ---)
                                  4
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c kurtosis_rayleigh(b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) kurtosis_rayleigh(b);
                                  2
                             3 %pi
                         2 - ------
                               16
(%o2)                    ---------- - 3
                              %pi 2
                         (1 - ---)
                               4
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_rayleigh}
@deffn {Function} random_rayleigh (@var{b}) @
@fname{random_rayleigh} (@var{b},@var{n})

Returns a 
m4_Rayleigh_RV(b) 
random variate, with @math{b>0}. Calling @code{random_rayleigh} with a second argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is based on the general inverse method.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn



@node Laplace Random Variable, Cauchy Random Variable, Rayleigh Random Variable, Functions and Variables for continuous distributions
@subsection Laplace Random Variable

The @emph{Laplace} distribution is a continuous probability
distribution that is sometimes called the double exponential
distribution because it can be thought of as two exponential
distributions spliced back to back.

@anchor{pdf_laplace}
@deffn {Function} pdf_laplace (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the density function of a 
m4_Laplace_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

Here, @math{a} is the location parameter (or mean), and @math{b} is
the scale parameter, related to the variance.

The pdf is
m4_displaymath(
<<<f(x; a, b) = {1\over 2b}\exp\left(-{|x-a|\over b}\right)>>>,
<<<
@example
                           abs(x - a)
                         - ----------
                               b
                       %e
          f(x, a, b) = --------------
                            2 b
@end example
@math{1/(2*b)*exp(-abs(x-a)/b)}>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_laplace}
@deffn {Function} cdf_laplace (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Laplace_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; a, b) =
\cases{
\displaystyle{1\over 2} \exp\left({x-a\over b}\right) & for $x < a$\cr
\cr
1-\displaystyle{1\over 2} \exp\left({x-a\over b}\right) & for $x \ge a$
}>>>,
<<<
@example
             [ 1/2*exp((x-a)/b)     for x < a
F(x, a, b) = [
             [ 1-1/2*exp((x-a)/b)   for x >= a
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_laplace}
@deffn {Function} quantile_laplace (@var{q},@var{a},@var{b})
Returns the @var{q}-quantile of a 
m4_Laplace_RV(a,b) 
random variable, with @math{b>0}; in other words, this is the inverse of @code{cdf_laplace}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_laplace}
@deffn {Function} mean_laplace (@var{a},@var{b})
Returns the mean of a 
m4_Laplace_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = a>>>,
<<<
@example
                            E[X] = a
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_laplace}
@deffn {Function} var_laplace (@var{a},@var{b})
Returns the variance of a 
m4_Laplace_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = 2b^2>>>,
<<<
@example
                                     2
                           V[X] = 2 b
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_laplace}
@deffn {Function} std_laplace (@var{a},@var{b})
Returns the standard deviation of a 
m4_Laplace_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = \sqrt{2} b>>>,
<<<
@example
                        D[X] = sqrt(2) b
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_laplace}
@deffn {Function} skewness_laplace (@var{a},@var{b})
Returns the skewness coefficient of a 
m4_Laplace_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = 0>>>,
<<<
@example
                            SK[X] = 0
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_laplace}
@deffn {Function} kurtosis_laplace (@var{a},@var{b})
Returns the kurtosis coefficient of a 
m4_Laplace_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = 3>>>,
<<<
@example
                            KU[X] = 3
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_laplace}
@deffn {Function} random_laplace (@var{a},@var{b}) @
@fname{random_laplace} (@var{a},@var{b},@var{n})

Returns a 
m4_Laplace_RV(a,b) 
random variate, with @math{b>0}. Calling @code{random_laplace} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is based on the general inverse method.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn



@node Cauchy Random Variable, Gumbel Random Variable, Laplace Random Variable, Functions and Variables for continuous distributions
@subsection Cauchy Random Variable

The @emph{Cauchy} distribution (also known as the Lorentz
distribution) is the distribution of of the ratio of two independent
normally distributed random variables with mean zero.

Note that the mean, variance, standard deviation, skewness
coefficient, and kurtosis coefficient are all undefined for the Cauchy
distribution.  The integrals do not converge in this case.

@anchor{pdf_cauchy}
@deffn {Function} pdf_cauchy (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the density function of a 
m4_Cauchy_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; a, b) = {b\over \pi\left((x-a)^2+b^2\right)}>>>,
<<<
@example
                            b
      f(x, a, b) = -------------------
                               2    2
                   %pi ((x - a)  + b )
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_cauchy}
@deffn {Function} cdf_cauchy (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Cauchy_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; a, b) = {1\over 2} + {1\over \pi} \tan^{-1} {x-a\over b}>>>,
<<<
@example
                                     x - a
                                atan(-----)
                                       b      1
                   F(x, a, b) = ----------- + -
                                    %pi       2
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_cauchy}
@deffn {Function} quantile_cauchy (@var{q},@var{a},@var{b})
Returns the @var{q}-quantile of a 
m4_Cauchy_RV(a,b) 
random variable, with @math{b>0}; in other words, this is the inverse of @code{cdf_cauchy}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_cauchy}
@deffn {Function} random_cauchy (@var{a},@var{b}) @
@fname{random_cauchy} (@var{a},@var{b},@var{n})

Returns a 
m4_Cauchy_RV(a,b) 
random variate, with @math{b>0}. Calling @code{random_cauchy} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is based on the general inverse method.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn



@node Gumbel Random Variable,  , Cauchy Random Variable, Functions and Variables for continuous distributions
@subsection Gumbel Random Variable

@anchor{pdf_gumbel}
@deffn {Function} pdf_gumbel (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the density function of a 
m4_Gumbel_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; a, b) = {1\over b} \exp\left[{a-x\over b} - \exp\left({a-x\over b}\right)\right]>>>,
<<<
@example
                             a - x
                             -----
                   a - x       b
                   ----- - %e
                     b
                 %e
    f(x, a, b) = -----------------
                         b
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_gumbel}
@deffn {Function} cdf_gumbel (@var{x},@var{a},@var{b})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Gumbel_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; a, b) = \exp\left[-\exp\left({a-x\over b}\right)\right]>>>,
<<<
@example
                       a - x
                       -----
                         b
                   - %e
    F(x, a, b) = %e
@end example
>>>)
@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_gumbel}
@deffn {Function} quantile_gumbel (@var{q},@var{a},@var{b})
Returns the @var{q}-quantile of a 
m4_Gumbel_RV(a,b) 
random variable, with @math{b>0}; in other words, this is the inverse of @code{cdf_gumbel}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_gumbel}
@deffn {Function} mean_gumbel (@var{a},@var{b})
Returns the mean of a 
m4_Gumbel_RV(a,b) 
random variable, with @math{b>0}.

The mean is
m4_displaymath(
<<<E[X] = a+b\gamma>>>,
<<<
@example
                       E[X] = %gamma b + a
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c mean_gumbel(a,b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) mean_gumbel(a,b);
(%o2)                     %gamma b + a
@end group
@end example
where symbol @code{%gamma} stands for the Euler-Mascheroni constant. See also @mrefdot{%gamma}

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_gumbel}
@deffn {Function} var_gumbel (@var{a},@var{b})
Returns the variance of a 
m4_Gumbel_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {\pi^2\over 6} b^2>>>,
<<<
@example
                                   2  2
                                %pi  b
                         V[X] = -------
                                   6
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_gumbel}
@deffn {Function} std_gumbel (@var{a},@var{b})
Returns the standard deviation of a 
m4_Gumbel_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = {\pi \over \sqrt{6}} b>>>,
<<<
@example
                                 %pi b
                         D[X] = -------
                                sqrt(6)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_gumbel}
@deffn {Function} skewness_gumbel (@var{a},@var{b})
Returns the skewness coefficient of a 
m4_Gumbel_RV(a,b) 
random variable, with @math{b>0}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {12\sqrt{6}\over \pi^3} \zeta(3)>>>,
<<<
@example
                                3/2
                             2 6    zeta(3)
                     SK[X] = --------------
                                     3
                                  %pi
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c skewness_gumbel(a,b);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) skewness_gumbel(a,b);
                            3/2
                         2 6    zeta(3)
(%o2)                    --------------
                                 3
                              %pi
@end group
@end example
where @code{zeta} stands for the Riemann's zeta function.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_gumbel}
@deffn {Function} kurtosis_gumbel (@var{a},@var{b})
Returns the kurtosis coefficient of a 
m4_Gumbel_RV(a,b) 
random variable, with @math{b>0}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {12\over 5}>>>,
<<<
@example
                                   12
                           KU[X] = --
                                   5
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_gumbel}
@deffn {Function} random_gumbel (@var{a},@var{b}) @
@fname{random_gumbel} (@var{a},@var{b},@var{n})

Returns a 
m4_Gumbel_RV(a,b) 
random variate, with @math{b>0}. Calling @code{random_gumbel} with a third argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is based on the general inverse method.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn


@node Functions and Variables for discrete distributions,  , Functions and Variables for continuous distributions, Package distrib
@section Functions and Variables for discrete distributions

Maxima knows the following kinds of discrete distributions


@menu
* General Finite Discrete Random Variable::
* Binomial Random Variable::
* Poisson Random Variable::
* Bernoulli Random Variable::
* Geometric Random Variable::
* Discrete Uniform Random Variable::
* Hypergeometric Random Variable::
* Negative Binomial Random Variable::
@end menu

@node General Finite Discrete Random Variable, Binomial Random Variable, Functions and Variables for discrete distributions, Functions and Variables for discrete distributions
@subsection General Finite Discrete Random Variable

@anchor{pdf_general_finite_discrete}
@deffn {Function} pdf_general_finite_discrete (@var{x},@var{v})
Returns the value at @var{x} of the probability function of a general finite discrete random variable, with vector probabilities @math{v}, such that @code{Pr(X=i) = v_i}. Vector @math{v} can be a list of nonnegative expressions, whose components will be normalized to get a vector of probabilities. To make use of this function, write first @code{load("distrib")}.

@c ===beg===
@c load ("distrib")$
@c pdf_general_finite_discrete(2, [1/7, 4/7, 2/7]);
@c pdf_general_finite_discrete(2, [1, 4, 2]);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) pdf_general_finite_discrete(2, [1/7, 4/7, 2/7]);
                                4
(%o2)                           -
                                7
@end group
@group
(%i3) pdf_general_finite_discrete(2, [1, 4, 2]);
                                4
(%o3)                           -
                                7
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_general_finite_discrete}
@deffn {Function} cdf_general_finite_discrete (@var{x},@var{v})
Returns the value at @var{x} of the cumulative distribution function of a general finite discrete random variable, with vector probabilities @math{v}.

See @code{pdf_general_finite_discrete} for more details.

@c ===beg===
@c load ("distrib")$
@c cdf_general_finite_discrete(2, [1/7, 4/7, 2/7]);
@c cdf_general_finite_discrete(2, [1, 4, 2]);
@c cdf_general_finite_discrete(2+1/2, [1, 4, 2]);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_general_finite_discrete(2, [1/7, 4/7, 2/7]);
                                5
(%o2)                           -
                                7
@end group
@group
(%i3) cdf_general_finite_discrete(2, [1, 4, 2]);
                                5
(%o3)                           -
                                7
@end group
@group
(%i4) cdf_general_finite_discrete(2+1/2, [1, 4, 2]);
                                5
(%o4)                           -
                                7
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_general_finite_discrete}
@deffn {Function} quantile_general_finite_discrete (@var{q},@var{v})
Returns the @var{q}-quantile of a general finite discrete random variable, with vector probabilities @math{v}.

See @code{pdf_general_finite_discrete} for more details.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_general_finite_discrete}
@deffn {Function} mean_general_finite_discrete (@var{v})
Returns the mean of a general finite discrete random variable, with vector probabilities @math{v}.

See @code{pdf_general_finite_discrete} for more details.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_general_finite_discrete}
@deffn {Function} var_general_finite_discrete (@var{v})
Returns the variance of a general finite discrete random variable, with vector probabilities @math{v}.

See @code{pdf_general_finite_discrete} for more details.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_general_finite_discrete}
@deffn {Function} std_general_finite_discrete (@var{v})
Returns the standard deviation of a general finite discrete random variable, with vector probabilities @math{v}.

See @code{pdf_general_finite_discrete} for more details.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_general_finite_discrete}
@deffn {Function} skewness_general_finite_discrete (@var{v})
Returns the skewness coefficient of a general finite discrete random variable, with vector probabilities @math{v}.

See @code{pdf_general_finite_discrete} for more details.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_general_finite_discrete}
@deffn {Function} kurtosis_general_finite_discrete (@var{v})
Returns the kurtosis coefficient of a general finite discrete random variable, with vector probabilities @math{v}.

See @code{pdf_general_finite_discrete} for more details.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_general_finite_discrete}
@deffn {Function} random_general_finite_discrete (@var{v}) @
@fname{random_general_finite_discrete} (@var{v},@var{m})

Returns a general finite discrete random variate, with vector probabilities @math{v}. Calling @code{random_general_finite_discrete} with a second argument @var{m}, a random sample of size @var{m} will be simulated.

See @code{pdf_general_finite_discrete} for more details.

@c ===beg===
@c load ("distrib")$
@c random_general_finite_discrete([1,3,1,5]);
@c random_general_finite_discrete([1,3,1,5], 10);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) random_general_finite_discrete([1,3,1,5]);
(%o2)                           4
@end group
@group
(%i3) random_general_finite_discrete([1,3,1,5], 10);
(%o3)            [3, 4, 3, 4, 4, 4, 4, 2, 4, 4]
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Binomial Random Variable, Poisson Random Variable, General Finite Discrete Random Variable, Functions and Variables for discrete distributions
@subsection Binomial Random Variable

The @emph{binomial distribution} with parameters @math{n} and @math{p}
is a discrete probability distribution.  It consists of @math{n}
independent experiments where each experiment consists of a
Boolean-valued outcome where a success occurs with a probablity
@math{p}.

For example, a biased coin that comes up heads with probablity
@math{p} is tossed @math{n} times.  Then the probability of exactly
@math{k} heads in @math{n} tosses is given by the binomial
distribution.

@anchor{pdf_binomial}
@deffn {Function} pdf_binomial (@var{x},@var{n},@var{p})
Returns the value at @var{x} of the probability function of a 
m4_Binomial_RV(n,p) 
random variable, with @math{0 \leq p \leq 1} and @math{n} a positive integer. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; n, p) = {n\choose x} (1-p)^{n-x}p^x>>>,
<<<
@example
                                               n - x  x
            f(x, n, p) = binomial(n, x) (1 - p)      p
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_binomial}
@deffn {Function} cdf_binomial (@var{x},@var{n},@var{p})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Binomial_RV(n,p) 
random variable, with @math{0 \leq p \leq 1} and @math{n} a positive integer.

The cdf is
m4_displaymath(
<<<F(x; n, p) = I_{1-p}(n-\lfloor x \rfloor, \lfloor x \rfloor + 1)>>>,
<<<
@example
F(x, n, p) = beta_incomplete_regularized(n - floor(x), floor(x) + 1, 
                                                                  1 - p)
@end example
>>>)
@ifnotinfo
where 
m4_math(I_z(a,b)) 
is the @ref{beta_incomplete_regularized}
function.
@end ifnotinfo


@c ===beg===
@c load ("distrib")$
@c cdf_binomial(5,7,1/6);
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_binomial(5,7,1/6);
                              7775
(%o2)                         ----
                              7776
@end group
@group
(%i3) float(%);
(%o3)                  0.9998713991769548
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_binomial}
@deffn {Function} quantile_binomial (@var{q},@var{n},@var{p})
Returns the @var{q}-quantile of a 
m4_Binomial_RV(n,p) 
random variable, with @math{0 \leq p \leq 1} and @math{n} a positive integer; in other words, this is the inverse of @code{cdf_binomial}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_binomial}
@deffn {Function} mean_binomial (@var{n},@var{p})
Returns the mean of a 
m4_Binomial_RV(n,p) 
random variable, with @math{0 \leq p \leq 1} and @math{n} a positive integer. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = np>>>,
<<<
@example
                            E[X] = n p
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_binomial}
@deffn {Function} var_binomial (@var{n},@var{p})
Returns the variance of a 
m4_Binomial_RV(n,p) 
random variable, with @math{0 \leq p \leq 1} and @math{n} a positive integer. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = np(1-p)>>>,
<<<
@example
                       V[X] = n (1 - p) p
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_binomial}
@deffn {Function} std_binomial (@var{n},@var{p})
Returns the standard deviation of a 
m4_Binomial_RV(n,p) 
random variable, with @math{0 \leq p \leq 1} and @math{n} a positive integer. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = \sqrt{np(1-p)}>>>,
<<<
@example
                    D[X] = sqrt(n (1 - p) p)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_binomial}
@deffn {Function} skewness_binomial (@var{n},@var{p})
Returns the skewness coefficient of a 
m4_Binomial_RV(n,p) 
random variable, with @math{0 \leq p \leq 1} and @math{n} a positive integer. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {1-2p\over \sqrt{np(1-p)}}>>>,
<<<
@example
                                 1 - 2 p
                    SK[X] = -----------------
                            sqrt(n (1 - p) p)
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_binomial}
@deffn {Function} kurtosis_binomial (@var{n},@var{p})
Returns the kurtosis coefficient of a 
m4_Binomial_RV(n,p) 
random variable, with @math{0 \leq p \leq 1} and @math{n} a positive integer. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {1-6p(1-p)\over np(1-p)}>>>,
<<<
@example
                             1 - 6 (1 - p) p
                     KU[X] = ---------------
                               n (1 - p) p
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_binomial}
@deffn {Function} random_binomial (@var{n},@var{p}) @
@fname{random_binomial} (@var{n},@var{p},@var{m})

Returns a 
m4_Binomial_RV(n,p) 
random variate, with @math{0 \leq p \leq 1} and @math{n} a positive integer. Calling @code{random_binomial} with a third argument @var{m}, a random sample of size @var{m} will be simulated.

The implemented algorithm is based on the one described in Kachitvichyanukul, V. and Schmeiser, B.W. (1988) @var{Binomial Random Variate Generation}. Communications of the ACM, 31, Feb., 216.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Poisson Random Variable, Bernoulli Random Variable, Binomial Random Variable, Functions and Variables for discrete distributions
@subsection Poisson Random Variable

The @emph{Poisson distribution} is a discrete probability
distribution. It is the probability that a given number of events
occur in a fixed interval when the events occur independently of the
time of the last event, and the events occur with a known constant
rate.

@anchor{pdf_poisson}
@deffn {Function} pdf_poisson (@var{x},@var{m})
Returns the value at @var{x} of the probability function of a 
m4_Poisson_RV(m) 
random variable, with @math{m>0}. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; m) = {m^x e^{-m}\over x!}>>>,
<<<
@example
                                   x   - m
                                  m  %e
                        f(x, m) = --------
                                     x!
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_poisson}
@deffn {Function} cdf_poisson (@var{x},@var{m})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Poisson_RV(m) 
random variable, with @math{m>0}.

The cdf is
m4_displaymath(
<<<F(x; m) = Q(\lfloor x \rfloor + 1, m)>>>,
<<<
@example
      F(x, m) = gamma_incomplete_regularized(floor(x) + 1, m)
@end example
>>>)
@ifnotinfo
where @math{Q(x,m)} is the @ref{gamma_incomplete_regularized}
function.
@end ifnotinfo

@c ===beg===
@c load ("distrib")$
@c cdf_poisson(3,5);
@c float(%);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_poisson(3,5);
(%o2)          gamma_incomplete_regularized(4, 5)
@end group
@group
(%i3) float(%);
(%o3)                  0.2650259152973619
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_poisson}
@deffn {Function} quantile_poisson (@var{q},@var{m})
Returns the @var{q}-quantile of a 
m4_Poisson_RV(m) 
random variable, with @math{m>0}; in other words, this is the inverse of @code{cdf_poisson}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_poisson}
@deffn {Function} mean_poisson (@var{m})
Returns the mean of a 
m4_Poisson_RV(m) 
random variable, with  @math{m>0}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = m>>>,
<<<
@example
                            E[X] = m
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_poisson}
@deffn {Function} var_poisson (@var{m})
Returns the variance of a 
m4_Poisson_RV(m) 
random variable, with  @math{m>0}. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = m>>>,
<<<
@example
                            V[X] = m
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_poisson}
@deffn {Function} std_poisson (@var{m})
Returns the standard deviation of a 
m4_Poisson_RV(m) 
random variable, with @math{m>0}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<V[X] = \sqrt{m}>>>,
<<<
@example
                         D[X] = sqrt(m)
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_poisson}
@deffn {Function} skewness_poisson (@var{m})
Returns the skewness coefficient of a 
m4_Poisson_RV(m) 
random variable, with @math{m>0}. To make use of this function, write first @code{load("distrib")}.

The skewness is
m4_displaymath(
<<<SK[X] = {1\over \sqrt{m}}>>>,
<<<
@example
                                    1
                         SK[X] = -------
                                 sqrt(m)
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_poisson}
@deffn {Function} kurtosis_poisson (@var{m})
Returns the kurtosis coefficient of a Poisson random variable  @math{Poi(m)}, with @math{m>0}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {1\over m}>>>,
<<<
@example
                                    1
                            KU[X] = -
                                    m
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_poisson}
@deffn {Function} random_poisson (@var{m}) @
@fname{random_poisson} (@var{m},@var{n})

Returns a 
m4_Poisson_RV(m) 
random variate, with @math{m>0}. Calling @code{random_poisson} with a second argument @var{n}, a random sample of size @var{n} will be simulated.

The implemented algorithm is the one described in Ahrens, J.H. and Dieter, U. (1982) @var{Computer Generation of Poisson Deviates From Modified Normal Distributions}. ACM Trans. Math. Software, 8, 2, June,163-179.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Bernoulli Random Variable, Geometric Random Variable, Poisson Random Variable, Functions and Variables for discrete distributions
@subsection Bernoulli Random Variable

The @emph{Bernoulli distribution} is a discrete probability
distribution which takes on two values, 0 and 1.  The value 1 occurs
with probability @math{p}, and 0 occurs with probabilty @math{1-p}.

It is equivalent to the 
m4_Binomial_RV(1,p) 
distribution (@pxref{Binomial Random Variable})

@anchor{pdf_bernoulli}
@deffn {Function} pdf_bernoulli (@var{x},@var{p})
Returns the value at @var{x} of the probability function of a 
m4_Bernoulli_RV(p) 
random variable, with @math{0 \leq p \leq 1}.

The 
m4_Bernoulli_RV(p) 
random variable is equivalent to the 
m4_Binomial_RV(1,p)
.

The mean is
m4_displaymath(
<<<f(x; p) = p^x (1-p)^{1-x}>>>,
<<<
@example
                                       1 - x  x
                      f(x, p) = (1 - p)      p
@end example

>>>)

@c ===beg===
@c load ("distrib")$
@c pdf_bernoulli(1,p);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) pdf_bernoulli(1,p);
(%o2)                           p
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_bernoulli}
@deffn {Function} cdf_bernoulli (@var{x},@var{p})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Bernoulli_RV(p) 
random variable, with @math{0 \leq p \leq 1}. To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; p) = I_{1-p}(1-\lfloor x \rfloor, \lfloor x \rfloor + 1)>>>,
<<<
@example
 F(x, n, p) = beta_incomplete_regularized(n - floor(x), floor(x) + 1, 
                                                                    1 - p)
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_bernoulli}
@deffn {Function} quantile_bernoulli (@var{q},@var{p})
Returns the @var{q}-quantile of a 
m4_Bernoulli_RV(p) 
random variable, with @math{0 \leq p \leq 1}; in other words, this is the inverse of @code{cdf_bernoulli}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_bernoulli}
@deffn {Function} mean_bernoulli (@var{p})
Returns the mean of a 
m4_Bernoulli_RV(p) 
random variable, with @math{0 \leq p \leq 1}.

The 
m4_Bernoulli_RV(p) 
random variable is equivalent to the 
m4_Binomial_RV(1,p)
.

The mean is
m4_displaymath(
<<<E[X] = p>>>,
<<<
@example
                            E[X] = p
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c mean_bernoulli(p);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) mean_bernoulli(p);
(%o2)                           p
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_bernoulli}
@deffn {Function} var_bernoulli (@var{p})
Returns the variance of a 
m4_Bernoulli_RV(p) 
random variable, with @math{0 \leq p \leq 1}.

The 
m4_Bernoulli_RV(p) 
random variable is equivalent to the 
m4_Binomial_RV(1,p)
.

The variance is
m4_displaymath(
<<<V[X] = p(1-p)>>>,
<<<
@example
                        V[X] = (1 - p) p
@end example
>>>)

@c ===beg===
@c load ("distrib")$
@c var_bernoulli(p);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) var_bernoulli(p);
(%o2)                       (1 - p) p
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_bernoulli}
@deffn {Function} std_bernoulli (@var{p})
Returns the standard deviation of a 
m4_Bernoulli_RV(p) 
random variable, with @math{0 \leq p \leq 1}.

The 
m4_Bernoulli_RV(p) 
random variable is equivalent to the
m4_Binomial_RV(1,p)
.

The standard deviation is
m4_displaymath(
<<<D[X] = \sqrt{p(1-p)}>>>,
<<<
@example
                     D[X] = sqrt((1 - p) p)
@end example

>>>)

@c ===beg===
@c load ("distrib")$
@c std_bernoulli(p);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) std_bernoulli(p);
(%o2)                    sqrt((1 - p) p)
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_bernoulli}
@deffn {Function} skewness_bernoulli (@var{p})
Returns the skewness coefficient of a 
m4_Bernoulli_RV(p) 
random variable, with @math{0 \leq p \leq 1}.

The 
m4_Bernoulli_RV(p)
random variable is equivalent to the 
m4_Binomial_RV(1,p)
.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {1-2p \over \sqrt{p(1-p)}}>>>,
<<<
@example
                                 1 - 2 p
                     SK[X] = ---------------
                             sqrt((1 - p) p)
@end example

>>>)

@c ===beg===
@c load ("distrib")$
@c skewness_bernoulli(p);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) skewness_bernoulli(p);
                             1 - 2 p
(%o2)                    ---------------
                         sqrt((1 - p) p)
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_bernoulli}
@deffn {Function} kurtosis_bernoulli (@var{p})
Returns the kurtosis coefficient of a 
m4_Bernoulli_RV(p) 
random variable, with @math{0 \leq p \leq 1}.

The 
m4_Bernoulli_RV(p) 
random variable is equivalent to the 
m4_Binomial_RV(1,p)
.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {1-6p(1-p) \over p(1-p)}>>>,
<<<
@example
                             1 - 6 (1 - p) p
                     KU[X] = ---------------
                                (1 - p) p
@end example

>>>)

@c ===beg===
@c load ("distrib")$
@c kurtosis_bernoulli(p);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) kurtosis_bernoulli(p);
                         1 - 6 (1 - p) p
(%o2)                    ---------------
                            (1 - p) p
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_bernoulli}
@deffn {Function} random_bernoulli (@var{p}) @
@fname{random_bernoulli} (@var{p},@var{n})

Returns a 
m4_Bernoulli_RV(p) 
random variate, with @math{0 \leq p \leq 1}. Calling @code{random_bernoulli} with a second argument @var{n}, a random sample of size @var{n} will be simulated.

This is a direct application of the @code{random} built-in Maxima function.

See also @mrefdot{random} To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Geometric Random Variable, Discrete Uniform Random Variable, Bernoulli Random Variable, Functions and Variables for discrete distributions
@subsection Geometric Random Variable

The @emph{Geometric distibution} is a discrete probability
distribution.  It is the distribution of the number 
Bernoulli trials that fail before the first success.

Consider flipping a biased coin where heads occurs with probablity
@math{p}.   Then the probability of @math{k-1} tails in a row followed
by heads is given by the 
m4_Geometric_RV(p) 
distribution.

@anchor{pdf_geometric}
@deffn {Function} pdf_geometric (@var{x},@var{p})
Returns the value at @var{x} of the probability function of a 
m4_Geometric_RV(p) 
random variable, with
@math{0 < p \leq 1}

The pdf is
m4_displaymath(
<<<f(x; p) = p(1-p)^x>>>,
<<<
@example
                                        x
                       f(x, p) = (1 - p)  p
@end example

>>>)

This is interpreted as the probability of @math{x} failures before the first success.

@code{load("distrib")} loads this function.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_geometric}
@deffn {Function} cdf_geometric (@var{x},@var{p})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Geometric_RV(p) 
random variable, with
@math{0 < p \leq  1}

The cdf is
m4_displaymath(
<<<1-(1-p)^{1 + \lfloor x \rfloor}>>>,
<<<
@example
                                      floor(x) + 1
                 F(x, p) = 1 - (1 - p)
@end example

>>>)

@code{load("distrib")} loads this function.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_geometric}
@deffn {Function} quantile_geometric (@var{q},@var{p})
Returns the @var{q}-quantile of a 
m4_Geometric_RV(p) 
random variable,
with 
m4_math(<<<0 \lt p \le 1>>>, <<<0 < p <= 1>>>, <<<0 < p <= 1>>>)
;
in other words, this is the inverse of @code{cdf_geometric}.
Argument @var{q} must be an element of @math{[0,1]}.

The probability from which the quantile is derived is defined as @math{p (1 - p)^x}.
This is interpreted as the probability of @math{x} failures before the first success.

@code{load("distrib")} loads this function.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_geometric}
@deffn {Function} mean_geometric (@var{p})
Returns the mean of a 
m4_Geometric_RV(p) 
random variable, with
@math{0 < p \leq 1}.

The mean is
m4_displaymath(
<<<E[X] = {1\over p} - 1>>>,
<<<
@example
                                 1
                          E[X] = - - 1
                                 p
@end example

>>>)

The probability from which the mean is derived is defined as @math{p (1 - p)^x}.
This is interpreted as the probability of @math{x} failures before the first success.

@code{load("distrib")} loads this function.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_geometric}
@deffn {Function} var_geometric (@var{p})
Returns the variance of a 
m4_Geometric_RV(p) 
random variable, with
@math{0 < p \leq 1}.

The variance is
m4_displaymath(
<<<V[X] = {1-p\over p^2}>>>,
<<<
@example
                                 1 - p
                          V[X] = -----
                                   2
                                  p
@end example

>>>)

@code{load("distrib")} loads this function.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_geometric}
@deffn {Function} std_geometric (@var{p})
Returns the standard deviation of a 
m4_Geometric_RV(p) 
random variable, with
@math{0 < p \leq 1}.

m4_displaymath(
<<<D[X] = {\sqrt{1-p} \over p}>>>,
<<<
@example
                              sqrt(1 - p)
                       D[X] = -----------
                                   p
@end example

>>>)

@code{load("distrib")} loads this function.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_geometric}
@deffn {Function} skewness_geometric (@var{p})
Returns the skewness coefficient of a 
m4_Geometric_RV(p) 
random variable, with
@math{0 < p \leq 1}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {2-p \over \sqrt{1-p}}>>>,
<<<
@example
                                  2 - p
                       SK[X] = -----------
                               sqrt(1 - p)
@end example

>>>)

@code{load("distrib")} loads this function.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_geometric}
@deffn {Function} kurtosis_geometric (@var{p})
Returns the kurtosis coefficient of a geometric random variable  
m4_Geometric_RV(p)
, with
@math{0 < p \leq 1}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {p^2-6p+6 \over 1-p}>>>,
<<<
@example
                               2
                              p  - 6 p + 6
                      KU[X] = ------------
                                 1 - p
@end example

>>>)

@code{load("distrib")} loads this function.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_geometric}
@deffn {Function} random_geometric (@var{p}) @
@fname{random_geometric} (@var{p},@var{n})

@code{random_geometric(@var{p})} returns one random sample from a 
m4_Geometric_RV(p) 
distribution,
with 
m4_mathdot(<<<0 \lt p \le 1>>>, <<<0 < p <= 1>>>, <<<0 < p \le 1>>>)

@code{random_geometric(@var{p}, @var{n})} returns a list of @var{n} random samples.

The algorithm is based on simulation of Bernoulli trials.

The probability from which the random sample is derived is defined as @math{p (1 - p)^x}.
This is interpreted as the probability of @math{x} failures before the first success.

@code{load("distrib")} loads this function.


@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Discrete Uniform Random Variable, Hypergeometric Random Variable, Geometric Random Variable, Functions and Variables for discrete distributions
@subsection Discrete Uniform Random Variable

The @emph{Discrete uniform distribution} is a discrete probablity
distribution where a finite number of values are equally likely to
occur.  The values are @math{1,2,3,...,n}.

For example throwing a fair die of 6 sides numbered 1 through 6
follows a 
m4_DiscreteUniform_RV(1/6) 
distribution.

@anchor{pdf_discrete_uniform}
@deffn {Function} pdf_discrete_uniform (@var{x},@var{n})
Returns the value at @var{x} of the probability function of a 
m4_DiscreteUniform_RV(n) 
random variable, with @math{n} a strictly positive integer. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x,n) = {1\over n}>>>,
<<<
@example
                                       1
                             f(x, n) = -
                                       n
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_discrete_uniform}
@deffn {Function} cdf_discrete_uniform (@var{x},@var{n})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_DiscreteUniform_RV(n) 
random variable, with @math{n} a strictly positive integer. To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; n) = {\lfloor x \rfloor \over n}>>>,
<<<
@example
                                  floor(x)
                        F(x, n) = --------
                                     n
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_discrete_uniform}
@deffn {Function} quantile_discrete_uniform (@var{q},@var{n})
Returns the @var{q}-quantile of a 
m4_DiscreteUniform_RV(n) 
random variable, with @math{n} a strictly positive integer; in other words, this is the inverse of @code{cdf_discrete_uniform}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_discrete_uniform}
@deffn {Function} mean_discrete_uniform (@var{n})
Returns the mean of a 
m4_DiscreteUniform_RV(n) 
random variable, with @math{n} a strictly positive integer. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = {n+1\over 2}>>>,
<<<
@example
                                 n + 1
                          E[X] = -----
                                   2
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_discrete_uniform}
@deffn {Function} var_discrete_uniform (@var{n})
Returns the variance of a 
m4_DiscreteUniform_RV(n) 
random variable, with @math{n} a strictly positive integer. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {n^2-1 \over 12}>>>,
<<<
@example
                                  2
                                 n  - 1
                          V[X] = ------
                                   12
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_discrete_uniform}
@deffn {Function} std_discrete_uniform (@var{n})
Returns the standard deviation of a 
m4_DiscreteUniform_RV(n) 
random variable, with @math{n} a strictly positive integer. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = {\sqrt{n^2-1} \over 2\sqrt{3}}>>>,
<<<
@example
                                    2
                              sqrt(n  - 1)
                       D[X] = ------------
                               2 sqrt(3)
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_discrete_uniform}
@deffn {Function} skewness_discrete_uniform (@var{n})
Returns the skewness coefficient of a 
m4_DiscreteUniform_RV(n) 
random variable, with @math{n} a strictly positive integer. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = 0>>>,
<<<
@example
                            SK[X] = 0
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_discrete_uniform}
@deffn {Function} kurtosis_discrete_uniform (@var{n})
Returns the kurtosis coefficient of a 
m4_DiscreteUniform_RV(n) 
random variable, with @math{n} a strictly positive integer. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = - {6(n^2+1)\over 5 (n^2-1)}>>>,
<<<
@example
                                    2
                                6 (n  + 1)
                   KU[X] = - -----------------
                             5 (n - 1) (n + 1)

@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_discrete_uniform}
@deffn {Function} random_discrete_uniform (@var{n}) @
@fname{random_discrete_uniform} (@var{n},@var{m})

Returns a 
m4_DiscreteUniform_RV(n) 
random variate, with @math{n} a strictly positive integer. Calling @code{random_discrete_uniform} with a second argument @var{m}, a random sample of size @var{m} will be simulated.

This is a direct application of the @code{random} built-in Maxima function.

See also @mrefdot{random} To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Hypergeometric Random Variable, Negative Binomial Random Variable, Discrete Uniform Random Variable, Functions and Variables for discrete distributions
@subsection Hypergeometric Random Variable

The @emph{hypergeometric distribution} is a discrete probability
distribution.

Let @math{n_1} be the number of objects of a class
@math{A} and @math{n_2} be the number of objects of class @math{B}.
We take out @math{n} objects, @emph{without} replacment.  Then the
hypergeometric distribution is the probability that exactly @math{k}
objects are from class @math{A}.  Of course @math{n \leq n_1 + n_2}.

@anchor{pdf_hypergeometric}
@deffn {Function} pdf_hypergeometric (@var{x},@var{n_1},@var{n_2},@var{n})
Returns the value at @var{x} of the probability function of a 
m4_Hypergeometric_RV(n1,n2,n)
random variable, with @math{n_1}, @math{n_2} and @math{n} non negative
integers and @math{n\leq n_1+n_2}.
Being @math{n_1} the number of objects of class A, @math{n_2} the number of objects of class B, and
@math{n} the size of the sample without replacement, this function returns the probability of
event "exactly @var{x} objects are of class A". 

To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; n_1, n_2, n) = {\displaystyle{n_1\choose x} {n_2 \choose n-x}
\over \displaystyle{n_2+n_1 \choose n}}>>>,
<<<
@example
                           binomial(n_1, x) binomial(n_2, n - x)
       f(x, n_1, n_2, n) = -----------------------------------
                                  binomial(n_2 + n_1, n)
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_hypergeometric}
@deffn {Function} cdf_hypergeometric (@var{x},@var{n_1},@var{n_2},@var{n})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_Hypergeometric_RV(n1,n2,n) 
random variable, with @math{n_1}, @math{n_2} and @math{n} non negative
integers and @math{n\leq n_1+n_2}. 
See @code{pdf_hypergeometric} for a more complete description.

To make use of this function, write first @code{load("distrib")}.

The cdf is
m4_displaymath(
<<<F(x; n_1, n_2, n) = {n_2+n_1\choose n}^{-1}
\sum_{k=0}^{\lfloor x \rfloor} {n_1 \choose k} {n_2 \choose n - k}>>>,
<<<
@example
                      floor(x)
                      ====
                      \
                       >       binomial(n_1, k) binomial(n_2, n - k)
                      /
                      ====
                      k = 0
  F(x, n_1, n_2, n) = --------------------------------------------
                                  binomial(n_2 + n_1, n)
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_hypergeometric}
@deffn {Function} quantile_hypergeometric (@var{q},@var{n1},@var{n2},@var{n})
Returns the @var{q}-quantile of a 
m4_Hypergeometric_RV(n1,n2,n) 
random
variable, with @var{n1}, @var{n2} and @var{n} non negative integers
and @math{n\leq n1+n2}; in other words, this is the inverse of @code{cdf_hypergeometric}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_hypergeometric}
@deffn {Function} mean_hypergeometric (@var{n_1},@var{n_2},@var{n})
Returns the mean of a discrete uniform random variable 
m4_Hypergeometric_RV(n_1,n_2,n)
, with @math{n_1}, @math{n_2} and @math{n} non negative integers and @math{n\leq n_1+n_2}. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = {n n_1\over n_2+n_1}>>>,
<<<
@example
                                 n n_1
                         E[X] = -------
                                n_2 + n_1

@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_hypergeometric}
@deffn {Function} var_hypergeometric (@var{n1},@var{n2},@var{n})
Returns the variance of a hypergeometric  random variable 
m4_Hypergeometric_RV(n_1,n_2,n)
,
with @math{n_1}, @math{n_2} and @math{n} non negative integers and 
m4_mathdot(<<<n \le n_1 + n_2>>>, <<<n <= n_1 + n_2>>>)
To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {n n_1 n_2 (n_1 + n_2 - n)
 \over
 (n_1 + n_2 - 1) (n_1 + n_2)^2}>>>,
<<<
@example
                        n n_1 n_2 (n_2 + n_1 - n)
                V[X] = ----------------------------
                                                  2
                       (n_2 + n_1 - 1) (n_2 + n_1)
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_hypergeometric}
@deffn {Function} std_hypergeometric (@var{n_1},@var{n_2},@var{n})
Returns the standard deviation of a 
m4_Hypergeometric_RV(n_1,n_2,n)
random variable, with @math{n_1}, @math{n_2} and @math{n} non negative integers and @math{n\leq n_1+n_2}. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = {1\over n_1+n_2}\sqrt{n n_1 n_2 (n_1 + n_2 - n) \over n_1+n_2-1}>>>,
<<<
@example
                          n n_1 n_2 (n_2 + n_1 - n)
                     sqrt(-------------------------)
                                n_2 + n_1 - 1
              D[X] = -------------------------------
                                n_2 + n_1
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_hypergeometric}
@deffn {Function} skewness_hypergeometric (@var{n_1},@var{n_2},@var{n})
Returns the skewness coefficient of a 
m4_Hypergeometric_RV(n1,n2,n) 
random variable, with @math{n_1}, @math{n_2} and @math{n} non negative integers and @math{n\leq n1+n2}. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {(n_2-n_2)(n_1+n_2-2n)\over n_1+n_2-2}
\sqrt{n_1+n_2-1 \over n n_1 n_2 (n_1+n_2-n)}>>>,
<<<
@example
                                                 n_2 + n_1 - 1
        (n_2 - n_1) (n_2 + n_1 - 2 n) sqrt(-------------------------)
                                           n n_1 n_2 (n_2 + n_1 - n)
SK[X] = -------------------------------------------------------------
                                n_2 + n_1 - 2

@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_hypergeometric}
@deffn {Function} kurtosis_hypergeometric (@var{n_1},@var{n_2},@var{n})
Returns the kurtosis coefficient of a 
m4_Hypergeometric_RV(n_1,n_2,n) 
random variable, with @math{n_1}, @math{n_2} and @math{n} non negative integers and @math{n\leq n1+n2}. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<
\eqalign{
KU[X] = &
 \left[{C(1)C(0)^2
   \over
  n n_1 n_2 C(3)C(2)C(n)}\right. \cr
  & \times 
  \left.\left(
    {3n_1n_2\left((n-2)C(0)^2+6nC(n)-n^2C(0)\right)
    \over
    C(0)^2
    }
    -6nC(n) + C(0)C(-1)
  \right)\right] \cr
  &-3
}>>>,
<<<
@example
                                    2
KU[X] = ((n_2 + n_1 - 1) (n_2 + n_1)
                                2                          2
  3 n_1 n_2 ((n - 2) (n_2 + n_1)  + 6 n (n_2 + n_1 - n) - n  (n_2 + n_1))
 (-----------------------------------------------------------------------
                                          2
                               (n_2 + n_1)
 - 6 n (n_2 + n_1 - n) + (n_2 + n_1) (n_2 + n_1 + 1)))
/(n n_1 n_2 (n_2 + n_1 - 3) (n_2 + n_1 - 2) (n_2 + n_1 - n)) - 3
@end example
>>>)
@ifnotinfo
where 
m4_mathdot(C(k) = n_1+n_2-k)
@end ifnotinfo

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_hypergeometric}
@deffn {Function} random_hypergeometric (@var{n1},@var{n2},@var{n}) @
@fname{random_hypergeometric} (@var{n1},@var{n2},@var{n},@var{m})

Returns a 
m4_Hypergeometric_RV(n1,n2,n) 
random variate,
with @var{n1}, @var{n2} and @var{n} non negative integers and 
m4_mathdot(<<<n \le n_1 + n_2>>>, <<<n <= n_1 + n_2>>>)
Calling @code{random_hypergeometric} with a fourth argument @var{m}, a random sample of size @var{m} will be simulated.

Algorithm described in Kachitvichyanukul, V., Schmeiser, B.W. (1985) @var{Computer generation of hypergeometric random variates.} Journal of Statistical Computation and Simulation 22, 127-145.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn

@node Negative Binomial Random Variable,  , Hypergeometric Random Variable, Functions and Variables for discrete distributions
@subsection Negative Binomial Random Variable

The @emph{negative binomial distribution} is a discrete probability
distribution.  Suppose we have a sequence of Bernoulli trials where
each trial has two outcomes called ``success'' and ``failure'' where
``success'' occurs with probablity @math{p} and ``failure'' with
probability @math{1-p}.  We observe the sequence until a predefined
number @math{r} of sucesses have occurred.  Then the number of
failures seen will have a 
m4_NegativeBinomial_RV(r, p) 
distribution.

@anchor{pdf_negative_binomial}
@deffn {Function} pdf_negative_binomial (@var{x},@var{n},@var{p})
Returns the value at @var{x} of the probability function of a 
m4_NegativeBinomial_RV(n,p) 
random variable, with @math{0 < p \leq 1} and @math{n} a positive number. To make use of this function, write first @code{load("distrib")}.

The pdf is
m4_displaymath(
<<<f(x; n, p) = {x+n-1 \choose n-1} (1-p)^xp^n>>>,
<<<
@example
                                   x  n
                            (1 - p)  p  gamma(x + n)
               f(x, n, p) = ------------------------
                                  gamma(n) x!
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{cdf_negative_binomial}
@deffn {Function} cdf_negative_binomial (@var{x},@var{n},@var{p})
Returns the value at @var{x} of the cumulative distribution function of a 
m4_NegativeBinomial_RV(n,p) 
random variable, with @math{0 < p \leq 1} and @math{n} a positive number.

The cdf is
m4_displaymath(
<<<F(x; n, p) = I_p(n,\lfloor x \rfloor + 1)>>>,
<<<
@example
   F(x, n, p) = beta_incomplete_regularized(n, floor(x) + 1, p)
@end example
>>>)
@ifnotinfo
where 
m4_math(I_p(a,b)) 
is the @ref{beta_incomplete_regularized} function.
@end ifnotinfo

@c ===beg===
@c load ("distrib")$
@c cdf_negative_binomial(3,4,1/8);
@c ===end===
@example
(%i1) load ("distrib")$
@group
(%i2) cdf_negative_binomial(3,4,1/8);
                              3271
(%o2)                        ------
                             524288
@end group
@end example

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{quantile_negative_binomial}
@deffn {Function} quantile_negative_binomial (@var{q},@var{n},@var{p})
Returns the @var{q}-quantile of a 
m4_NegativeBinomial_RV(n,p) 
random variable, with @math{0 < p \leq 1} and @math{n} a positive number; in other words, this is the inverse of @code{cdf_negative_binomial}. Argument @var{q} must be an element of @math{[0,1]}. To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{mean_negative_binomial}
@deffn {Function} mean_negative_binomial (@var{n},@var{p})
Returns the mean of a 
m4_NegativeBinomial_RV(n,p) 
random variable, with @math{0 < p \leq 1} and @math{n} a positive number. To make use of this function, write first @code{load("distrib")}.

The mean is
m4_displaymath(
<<<E[X] = {n(1-p)\over p}>>>,
<<<
@example
                               n (1 - p)
                        E[X] = ---------
                                   p
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{var_negative_binomial}
@deffn {Function} var_negative_binomial (@var{n},@var{p})
Returns the variance of a 
m4_NegativeBinomial_RV(n,p) 
random variable, with @math{0 < p \leq 1} and @math{n} a positive number. To make use of this function, write first @code{load("distrib")}.

The variance is
m4_displaymath(
<<<V[X] = {n(1-p)\over p^2}>>>,
<<<
@example
                               n (1 - p)
                        V[X] = ---------
                                   2
                                  p
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{std_negative_binomial}
@deffn {Function} std_negative_binomial (@var{n},@var{p})
Returns the standard deviation of a 
m4_NegativeBinomial_RV(n,p) 
random variable, with @math{0 < p \leq 1} and @math{n} a positive number. To make use of this function, write first @code{load("distrib")}.

The standard deviation is
m4_displaymath(
<<<D[X] = {\sqrt{n(1-p)}\over p}>>>,
<<<
@example
                            sqrt(n (1 - p))
                     D[X] = ---------------
                                   p
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{skewness_negative_binomial}
@deffn {Function} skewness_negative_binomial (@var{n},@var{p})
Returns the skewness coefficient of a 
m4_NegativeBinomial_RV(n,p) 
random variable, with @math{0 < p \leq 1} and @math{n} a positive number. To make use of this function, write first @code{load("distrib")}.

The skewness coefficient is
m4_displaymath(
<<<SK[X] = {2-p \over \sqrt{n(1-p)}}>>>,
<<<
@example
                                  2 - p
                     SK[X] = ---------------
                             sqrt(n (1 - p))
@end example
>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{kurtosis_negative_binomial}
@deffn {Function} kurtosis_negative_binomial (@var{n},@var{p})
Returns the kurtosis coefficient of a 
m4_NegativeBinomial_RV(n,p) 
random variable, with @math{0 < p \leq 1} and @math{n} a positive number. To make use of this function, write first @code{load("distrib")}.

The kurtosis coefficient is
m4_displaymath(
<<<KU[X] = {p^2-6p+6 \over n(1-p)}>>>,
<<<
@example
                               2
                              p  - 6 p + 6
                      KU[X] = ------------
                               n (1 - p)
@end example

>>>)

@opencatbox{Categories:}
@category{Package distrib}
@closecatbox

@end deffn


@anchor{random_negative_binomial}
@deffn {Function} random_negative_binomial (@var{n},@var{p}) @
@fname{random_negative_binomial} (@var{n},@var{p},@var{m})

Returns a 
m4_NegativeBinomial_RV(n,p) 
random variate, with @math{0 < p \leq 1} and @math{n} a positive number. Calling @code{random_negative_binomial} with a third argument @var{m}, a random sample of size @var{m} will be simulated.

Algorithm described in Devroye, L. (1986) @var{Non-Uniform Random Variate Generation}. Springer Verlag, p. 480.

To make use of this function, write first @code{load("distrib")}.

@opencatbox{Categories:}
@category{Package distrib}
@category{Random numbers}
@closecatbox

@end deffn
@c Undefine all the m4 macros we defined in this file.
m4_undefine(<<<m4_Normal_RV>>>)
