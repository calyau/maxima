--------------------------------------------------------------------------
ZEILBERGER (Version 3.0)
by Fabrizio Caruso
--------------------------------------------------------------------------
Implementation Zeilberger's algorithm for
definite hypergeometric summation and of 
Gosper's algorithm for indefinite hypergeometric
summation in the Maxima computer algebra system.


The package also uses Axel Riese's optimization (Filtering).

The package has been tested with 5.9.1 of Maxima.
--------------------------------------------------------------------------

This package was developed by Fabrizio Caruso

[Version 1.0]
at RISC-Linz, 
J.Kepler Universitaet (Austria) 

[Version 2.0]
at Dipartimento di Matematica "L. Tonelli", 
Università di Pisa (Italy) [Version 2.0]

[Version 3.0]
at IRMAR, 
Université de Rennes 1.

--------------------------------------------------------------------------
DESCRIPTION OF THE PROBLEMS
--------------------------------------------------------------------------

THE INDEFINITE PROBLEM

The package provides an implementation of Gosper's algorithm
for indefinite hypergeometric summation:

Given a hypergeometric term F_k in k we want to find its hypergeometric
anti-difference, i.e. a hypergeometric term f_k such that:

F_k = f_(k+1) - f_k.


--------------------------------------------------------------------------

THE DEFINITE PROBLEM

The package provides an implementation of Zeilberger's algorithm
for definite hypergeometric summation:

Given a proper hypergeometric term (in n and k) F_(n,k) and a
positive integer d we want to find a d-th order linear
recurrence with polynomial coefficients (in n) for F_(n,k)
and a rational function R in n and k such that:

a_0 F_(n,k) + ... + a_d F_(n+d),k = Delta_K(R(n,k) F_(n,k)),

where

Delta_k is the k-forward difference operator, i.e.,
Delta_k(t_k) := t_(k+1) - t_k.

--------------------------------------------------------------------------
LOADING
--------------------------------------------------------------------------

In order to load the package 
either load all the ".mc" files manually or 
first edit the line of "LOADZeilberger.mc" defining 
the directory ("dir") containing the
package files, then load "LOADZeilberger.mc" 
which will take care of loading and evalutating 
all the necessary files.

--------------------------------------------------------------------------
INSTRUCTIONS
--------------------------------------------------------------------------

- AntiDifference(F_k,k)

AntiDifference either finds the hypergeometric anti-difference
of F_k if it exists, or if it does not exist, it outputs:

NO_HYP_ANTIDIFFERENCE
--------------------------------------------------------------------------

- Gosper(F_k,k)

Gosper either finds the rational certificate R(k) for F_k, i.e.
a rational function such that:

F_k = R(k+1) F_(k+1) - R(k) F_k
 
if it exists, or if it does not exist, it outputs:

NO_HYP_SOL
---------------------------------------------------------------------------

- GosperSum(F_k,k,a,b) 

GosperSum either sums F_k over the interval [a,b] if
F_k has a hypergeometric anti-difference, or 
if it does not have it, it outpus:

NONGOSPER_SUMMABLE
---------------------------------------------------------------------------

- parGosper(F_n,k , k , n , d)

parGosper tries to find a d-th order recurrence.
If it finds one it outputs it in the form (*).
If it finds none it outputs the empty solution [].
---------------------------------------------------------------------------

- Zeilberger(F_n,k , k , n)

Zeilberger starts by invoking "Gosper" and if it fails
tries "parGosper" with order 1 and tries up to 
the order given by the variable MAX_ORD.
If Zeilberger finds a solution before reaching MAX_ORD
it stops and yields the solution in the form (*) otherwise
it tries a higher oder.
If no solution is found it outputs [].
Remark: "Gosper" is skipped if the setting
GOSPER_IN_ZEILBERGER is set to FALSE.
--------------------------------------------------------------------------

(*)
FORM OF THE OUTPUT OF "parGosper" AND "ZEILBERGER"

The algorithms yields a sequence:
[s_1,s_2, ..., s_m] of solutions.

Each solution has the following form:

[R(n,k), [a_0, a_1, ..., a_d]] 

--------------------------------------------------------------------------
SETTINGS
--------------------------------------------------------------------------

The package provides many settings set by the
following variables whose default values are defined
in the file "settings.mc".

--------------------------------------------------------------------------
General settings

MAX_ORD : maximum order used by Zeilberger [5]

SIMPLIFIED_OUTPUT : further simplification of the solution [FALSE]

LINEAR_SOLVER : which solver is used to solve the system
in Zeilberger's algorithm [linsolve]

WARNINGS : warnings during execution [TRUE]

GOSPER_IN_ZEILBERGER : "Zeilberger" tries "Gosper" [TRUE]

TRIVIAL_SOLUTIONS :  Solutions by Zeilberger's algorithm
involving zero certificate or all identically zero coefficients
are also output [TRUE]

--------------------------------------------------------------------------
Settings related to the modular test

MOD_TEST : modular test for discarting systems with no solutions 
in "parGosper" and "Zeilberger" [FALSE]

MODULAR_LINEAR_SOLVER : linear solver used by the modular test [linsolve]

EV_POINT : evaluation point at which the variable "n" is evaluated
when performing the modular test [BIG_PRIMES[10]]

MOD_BIG_PRIME : modulo used by the modular test [BIG_PRIMES[1]]

MOD_THRESHOLD : threshold for the order for which a modular test
can be used [4]

--------------------------------------------------------------------------
VERBOSITY LEVELS
--------------------------------------------------------------------------

There are also verbose versions of the commands
which are called by adding one of the following prefixes:

"Summary" : just a summary at the end is shown
"Verbose" : some information in the intermidiate steps
"VeryVerbose" : more information
"Extra" : even more information including information on
the linear system in Zeilberger's algorithm


For example: "GosperVerbose", "parGosperVeryVerbose",
"ZeilbergerExtra", "AntiDifferenceSummary".

