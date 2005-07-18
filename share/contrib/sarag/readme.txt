---------------------------------------------------------------
---------------------------------------------------------------
                         S A R A G

       SOME ALGORITHMS IN REAL ALGEBRAIC GEOMETRY

                    ALPHA Version 0.0.2

---------------------------------------------------------------
developed by Fabrizio Caruso

under the scientific guidance of Marie-Françoise Roy

at the University of Rennes 1, France

with support from the RAAG network

---------------------------------------------------------------

Please report bugs to: fabrizio.caruso@math.univ-rennes1.fr

---------------------------------------------------------------
Also part of the free interactive book

"Algorithms in Real Algebraic Geometry"

by Saugata Basu, Richard Pollack, Marie-Françoise Roy

Springer-Verlag, Berlin, 2003

which together with SARAG is available for download at 

http://name.math.univ-rennes1.fr/marie-francoise.roy/bpr-posted1.html 

---------------------------------------------------------------
---------------------------------------------------------------
THE SARAG LIBRARY
 
---------------------------------------------------------------

This is a free open source Maxima library for 
real algebraic geometry.

As of this version, SARAG provides functions for linear algebra,
theory of subresultants, Cauchy Index and its application
to real root counting, isolation of real roots,
sign determination and Thom encodings.


---------------------------------------------------------------
---------------------------------------------------------------
REQUIREMENTS

---------------------------------------------------------------

The library has been developed and tested 
with Maxima version 5.9.1.
The latest version of Maxima is available on line at
http://maxima.sourceforge.net/

---------------------------------------------------------------
---------------------------------------------------------------
LOADING THE FILES

loadSARAG.mc
---------------------------------------------------------------

The library is contained in the following files:

settings.mc (general settings)
aliases.mc (name conventions)
lowLevel.mc (low level routines)
linearAlgebra.mc (linear algebra and matrix manipulation)
rootCounting.mc (real root counting)
rootIsolation.mc (root isolation by De Casteljau method)
signDetermination.mc (sign determination)

In order to load the library either load each single file
with the "LOAD" Maxima command
or edit a file that takes care of loading all the files. 

Together with the files you find the file
"loadSARAG.mc" that will load the files if they are
in the current directory.

The file "examples.mc" contains some examples
from the book.

---------------------------------------------------------------
---------------------------------------------------------------
THE MANUAL

readme.txt
---------------------------------------------------------------

This manual describes the high level functions ("main functions")
of the SARAG library and the most important auxiliary functions.

For more details and for the theory behind the algorithms
we refer to "Algorithms in Real Algebraic Geometry"
by S. Basu, R. Pollack, M.-F. Roy
which together with SARAG is available for download at 
http://name.math.univ-rennes1.fr/marie-francoise.roy/bpr-posted1.html 


ATTENTION: 

(*) Items marked with this symbol are not fully tested
or are missing some minor features.

(**) These items are not available, yet, but will
be included soon.

---------------------------------------------------------------
---------------------------------------------------------------
SETTINGS

settings.mc
---------------------------------------------------------------

The library contains some settings in the file "settings.mc".

The settings and their default values are:

- VERBOSE [0 (non-verbose)] (**)
Verbosity level of the commands.

- LINEAR_SOLVER [linsolve (built-in Maxima linear solver)]
Solver of linear systems used in some routines.

- MOD_TEST_PRIME [2]
Prime used in some modular tests.

- ASSUME_EXPANDED [FALSE]
Assunption on whether the polynomial input of the main functions
is in expanded form

---------------------------------------------------------------
---------------------------------------------------------------
NAMING CONVENTIONS

aliases.mc
---------------------------------------------------------------

The names of main functions are formed by adding
prefixes to specify the method/algorithm to be used and 
suffixes to specify an alternative version of the
function or output.
Auxiliary functions may not follow such conventions.

Therefore the general name convention for the names 
of the high level functions is the following:

METHOD | WHAT IS TO BE COMPUTED | MODIFIER

When a prefix is added capitalization 
of the function is used to distinguish the
parts of the new name.

When no prefix or no suffix is used the
default version of function with the
default method/algorithm will be called
as set in the file: "aliases.mc"

Examples:

tarskiQuery [computes the Tarski query over all R with
the default algorithm]

sRemTarskiQueryBetween [computes the Tarski query
over an interval using the signed remainder sequence]

det [computes the determinant of a matrix with the
default algorithm]

bareissDet [computes the determinant of a matrix
by using Bareiss method]

sSubRes [computes the signed subresultant sequence]

sSubResExt [computes the extended signed subresultant sequence]


---------------------------------------------------------------
---------------------------------------------------------------
COMMANDS
---------------------------------------------------------------

Here we describe the most important functions:
all the "main functions" and some 
important "auxiliary functions".

REMARK: 
Our naming conventions and the
global variables VERBOSE and ASSUME_EXPANDED
only apply to "main functions".

When multiple methods are available for a 
main function  we list all of them.
They are accessible by adding the name 
of the method as a prefix and by capitalizing
the first letter of the name of the function.
For more details on names of functions read
the section on naming conventions.


ATTENTION:

-------------------------------------------------
Polynomial Input in Expanded Form

If ASSUME_EXPANDED is set to TRUE then
whenever the input of a main function contains polynomials
we are always assuming that they are in EXPANDED FORM.

Independently on the value of ASSUME_EXPANDED 
we will always assume that the polynomial
input of auxiliary functions is in EXPANDED FORM.

Therefore, in these cases,
when defining a polynomial use for instance:

p : expand(...);

-------------------------------
Form of the Output

The output of all functions of the library is a Maxima expression. 
Maxima uses brakets "[", "]" to to describe couples and lists
(e.g. an open inteval ]a,b[ is described by a couple
containing the ends, which in Maxima is "[a,b]")


---------------------------------------------------------------
LINEAR ALGEBRA

linearAlgebra.mc
---------------------------------------------------------------

This file contains functions related to Gaussian elimination
and matrix manipulations.

---------------------------------------------------------------
Auxiliary functions


- matrixProd(a,b)
INPUT : a and b are lists of lists representing the rows 
of two matrices such that a has the same number of columns
as b as rows
OUTPUT : the row-column product matrix of a, b

---------------------------------------------------------------
Main functions

- det(m)
INPUT : m is a list of lists representing the rows of a matrix 
METHOD : 
-- gauss [Gaussian elimination with no pivot optimization], 
-- bareiss [Bareiss method]
OUTPUT : the determinant


- elim(m) 
INPUT : m is a list of lists representing the rows of a matrix
METHOD : 
-- gauss [Gaussian elimination with no pivot optimization], 
-- bareiss [Bareiss method] (**)
OUTPUT : [t,c,z]
where 
1) t is an upper triangular equivalent form of m
with possibly some zero rows computed by Gaussian
elimination with divisions and columns exchanges
2) c is the list of couples describing the columns
exchanges
3) z is the list of zero rows


-----------------------------------------------------------------
CHARACTERISTIC POLYNOMIAL

linearAlgebra.mc
-----------------------------------------------------------------

-----------------------------------------------------------------
Auxiliary functions

- getCoeffFromNewton(ns) 
INPUT : a list containing the the Newton sums of
a given polynomial
OUTPUT : an array containing the coefficients of the
corresponding polynomial

-----------------------------------------------------------------
Main functions

- charPol(A,var)
INPUT : A is a list of lists representing the rows of a matrix,
an indeterminate var
METHOD : 
-- gauss [Gaussian elimination with no pivot optimization], (**) 
-- bareiss [Bareiss method], (**)
-- babyGiant [baby step, giant step trace-based method]
OUTPUT : the characteristic polynomial of A in the indeterminate var


-----------------------------------------------------------------
SIGNED REMAINDER SEQUENCE

rootCounting.mc
-----------------------------------------------------------------

-----------------------------------------------------------------
Main functions

- sRem(a,b,x)
INPUT : a, b polynomials in the x indeterminate
OUTPUT : list containing signed remainder sequence

- sRemExt(a,b,x)
INPUT : a, b polynomials in the x indeterminate
OUTPUT : [r,u,v] forming the extended signed remainder sequence 
where r is the signed remainder sequence and u and v are
the corresponding sequences of cofactors

-----------------------------------------------------------------
SIGNED SUBRESULTANTS

rootCounting.mc
-----------------------------------------------------------------

-----------------------------------------------------------------
Main functions

- sSubRes(a,b,var)
INPUT : a,b polynomials in the x indeterminate with deg(a)>deg(b)
OUTPUT : [sSubRes,s] where 
1) sSubRes is a list containing the signed subresultant sequence
2) s is the list of the corresponding leading coefficients 


- sSubResExt(a,b,var) 
INPUT : a,b polynomials in the x indeterminate with deg(a)>deg(b)
OUTPUT : [sSubRes,s,u,v] where
1) sSubRes is a list containing the signed subresultant sequence
2) s is the list of the leading coefficients
3) u, v are the corresponding cofactors

--------------------------------------
GCD and GCD-free part by subresultants

- gcdFreePart(P,Q,var) (*)
INPUT : polynomials P,Q in var
OUTPUT : a couple [g,f]
where g is gcd(P,Q) and
f is the gcd-free part of P with respect to Q,
both the gcd and the gcd-free are considered up
to a constant multiple

-----------------------------------------------------------------
ROOTS COUNTING

rootCouting.mc
rootIsolation.mc (for De Casteljau-based's method) 
-----------------------------------------------------------------

Notation: 
In the documentation related to "cauchyIndex.mc",
we always assume that deg p > deg q.

-----------------------------------------------------------------
Auxiliary functions

- sturmSequence(p,x)
INPUT : polynomial p in the x indeterminate
OUTPUT : the Sturm sequence of p

-----------------------------------------------------------------
Main functions


----------------------------------------------
Cauchy Index on the real line

- cauchyIndex(q,p,x)
INPUT : q,p polynomials in the x indeterminate,
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
OUTPUT : Cauchy index of q/p in the whole real line

- tarskiQuery(q,p,x)
INPUT : q,p polynomials in the x indeterminate,
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
OUTPUT : Tarski query of q,p in the whole real line

- numberOfRoots(p,x)
INPUT : p polynomials in the x indeterminate
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
-- deCasteljau [De Casteljau method for isolation]
-- monomial [monomial method for isolation] (**)
OUTPUT : number of real roots of p

--------------------------------------------------
Cauchy Index on an open interval

Remark: Here we assume that a and b are not roots


- cauchyIndexBetween(num,den,x,a,b)
INPUT : num,den polynomials in the x indeterminate,
a,b are either real or -INFINITY or +INFINITY
METHOD : 
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
OUTPUT : Cauchy index of num/den in the interval ]a,b[

- tarskiQueryBetween(q,p,x,a,b)
INPUT : q,p polynomials in the x indeterminate,
a,b are either real or -INFINITY or +INFINITY
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
OUTPUT : Tarski query of  q,p in ]a,b[

- numberOfRootsBetween(p,x,a,b)
INPUT : p polynomials in the x indeterminate,
a,b are either real or -INFINITY or +INFINITY
METHOD :
-- sRem [signed remainder sequence]
-- sSubRes [signed subresultants]
-- deCasteljau [De Casteljau method for isolation] (**)
-- monomial [monomial method for isolation] (**)
OUTPUT : number of real roots of p in the interval ]a,b[

-----------------
Hankel Signature

- hankelSignature(seq) (*)
INPUT : sequence seq of odd length of elements of an integral domain
OUTPUT : signature of the Hankel quadratic form for seq

-----------------
Complex Roots with positive/negative real part

- posNegDiff(p,x)
INPUT : polynomial p in x
OUTPUT : difference between the number of roots of p with
positive real part and those with negative real part

-----------------
Bezoutian related

- bez(p,q,var,x,y)
INPUT : p,q polynomials in var, variables x,y
OUTPUT : Bezoutian of p,q with respect to x,y

-----------------------------------------------------------------
ISOLATION OF ROOTS

rootIsolation.mc
-----------------------------------------------------------------

The routines contained in this files deal with
the problem of isolation of real roots by
using the conversion to the Bernstein basis
and De Casteljau's method.

-----------------------------------------------------------------
Auxiliary functions

--------------------------
Cauchy Bounds

- cauchyRootUpperBound(pol,x)
INPUT : polynomial pol in x
OUTPUT : upper bound for the absolute values of all its real roots

- cauchyRootLowerBound(pol,x)
INPUT : polynomial pol in x
OUTPUT : lower bound for the absolute values of 
all its non-zero real roots

- primeCauchyRootUpperBound(pol,x)
INPUT : polynomial pol in x
OUTPUT : (alternative) upper bound for the 
absolute values of all its real roots

- primeCauchyRootLowerBound(pol,x)
INPUT : polynomial pol in x
OUTPUT : (alternative) lower bound for the 
absolute values of all its non-zero real roots

---------------------------
Bernstein Basis

- bernsteinCoeffList(p,x,c,d) 
INPUT : polynomial p in the x indeterminate, parameters c,d
OUTPUT : list containing the coefficients of p in the
Bernstein basis for c,d

- bernsteinSplit(coeffList, c, d, e)
INPUT : list coeffList containing the coefficients of a polynomial
in the Bernstein basis for c,d,
parameters c,d,e 
OUTPUT : [bern_ce, bern_ed] 
where
1) bern_cd is a list containing the coefficients
in the Bernstein basis for c,e and
2) bern_de is a list containing the coefficients
in the Bernstein basis for e,d

- specialBernsteinSplit(coeffList,c,d)
INPUT : list coeffList containing the coefficients of a polynomial P
in the Bernstein basis for c,d,
parameters c,d
OUTPUT : [bern_first,bern_second]
where
bern_first is a list containing the coefficients
in the Bernstein basis for c,(c+d)/2 of 2^deg(P) P
bern_second is a list containing the coefficients
in the Bernstein basis for (c+d)/2,d of 2^deg(P) P

-----------------------------------------------------------------
Main functions

- isolateRoots(pol,x)
INPUT : polynomial pol in x
METHOD:
-- deCasteljau [De Casteljau method for root isolation]
-- monomial [root isolation in the monomial basis] (**)
OUTPUT : list of elements  
of the form either
a) [pt]
describing the real root pt 
or 
b) [a,b]
describing the open interval "]a,b["   

- rootsSign(isInt,p,q,x)
INPUT : polynomials p,q in x,
isolating list isInt for the real roots of p in the
same form as in the output of "isolateRealRoots"
METHOD:
-- deCasteljau [De Casteljau method for root isolation]
-- monomial [root isolation in the monomial basis] (**)
OUTPUT : [[nCPtList,nCIntList],[cPtList,cIntList]]
where 
1) nCPtList, cPtList are lists of couples
describing a real root and the sign of q
at this real root
2) nCIntList,cIntList are lists of couples
describing an open interval containing exactly one 
the real-root of p, and describing the sign of q
at this root

- compareRoots(p,q,x)
INPUT : polynomials p,q in x
METHOD:
-- deCasteljau [De Casteljau method for root isolation]
-- monomial [root isolation in the monomial basis] (**)
OUTPUT : [com,signNComP,signNComQ]
where
1) com is an isolating list for the common real roots
of p and q
2) signNComP is an isolating list with signs of q for the
real roots of p that are not roots of q
3) signNComQ is an isolating list with signs of p for the
real roots of q that are not roots of p
Remark: all these one intervals and points isolate
the union of the zeros of p and q, i.e. also have
empty intersection among themselves.

-----------------------------------------------------------------
SIGN DETERMINATION

signDetermination.mc
-----------------------------------------------------------------


-----------------------------------------------------------------
Auxiliary functions

- matrixOfSigns(adaExpList,signCondList)
INPUT : list adaExpList of t-uples of exponents,
list of of t-uples signs (-1,0,1), where adaExpList is adapted to sign
determination for a set of t elements (polynomials) on signCondList
OUTPUT : the corresponding matrix of signs

- tarskiQueryVector(adaExpList,polList,var,SQ,ptSet)
INPUT : list adaExpoList of t-uples of exponents,
list of polynomials in var, 
description of a finite set of points ptSet,
algorithm SQ to compute
the Tarski query with respect to ptSet
OUTPUT : vector containing the Tarski Queries 
with respect to ptSet of 
the polynomials in polList with exponents given
by adaExpList

-----------------------------------------------------------------
Main functions

- signDetermination(polList,ptSet,sqAlg,var)
INPUT : list polList of polynomials in var, 
a description of a finite set of points,
an algorithm sqAlg to compute the Tarski query of polynomials
in polList
with respect to ptSet
METHOD :
-- naive [brute force method on a huge matrix of signs]
-- smart [a method that uses much smaller matrices of signs]
OUTPUT : a list representing 
a subset of  the all elements of {0,-1,1}^polList
describing the possible signs of the polynomials 
in polList at ptSet

-----------------------------------------------------------------
THOM ENCODINGS

signDetermination.mc
-----------------------------------------------------------------

Note: Here we refer to "extended Thom-encoding" for P with 
respect to Q as to the sign determination for Q, all the
derivatives of Q, all the derivatives of P at the roots of P

-----------------------------------------------------------------
Auxiliary functions

- thomLess(lhs,rhs) (*)
INPUT : Thom encodings lhs, rhs
OUTPUT : TRUE if lhs<rhs, FALSE otherwise

- thomSort(thomList) (*)
INPUT : list of Thom-encodings
OUTPUT : order list of Thom-encodings

- extThomEncoding(P,Q,var) (*)
INPUT : polynomials P,Q in var
OUTPUT : a list of "extended Thom-encodings" for P w.r.t. Q,

- extThomMerge(lhs,lhsDeg,rhs,rhsDeg) (*) 
INPUT : lhs, rhs are "extended Thom-encodings" for two polynomials
one with respect to the other,
lhsDeg, rhsDeg are the degrees of the polynomials
OUTPUT : a list containing
elements of the form [owner,thomInf] with
the following possibilities
a) [0,thomP,thomQ]
for a common root of P
with Thom encoding thomP 
and of Q with Thom encoding thomQ
b) [1,thomP] 
for a root of P not of Q with Thom encoding thomP
c) [2,thomQ] 
for a root of Q not of P
with Thom encoding thomQ
-----------------------------------------------------------------
Main functions

- thomEncoding(P,var) (*)
INPUT : polynomial P in var
OUTPUT : a list containing the Thom encoding of the real roots of P

- thomCompare(P,Q,var) (*)
INPUT : polynomials P,Q in var
OUTPUT : a list containing
elements of the form [owner,thomInf] with
the following possibilities
a) [0,thomP,thomQ]
for a common root of P
with Thom encoding thomP 
and of Q with Thom encoding thomQ
b) [1,thomP] 
for a root of P not of Q with Thom encoding thomP
c) [2,thomQ] 
for a root of Q not of P
with Thom encoding thomQ




