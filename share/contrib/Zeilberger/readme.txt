--------------------------------------------------------------------------
Fabrizio Caruso's Implementation of Zeilberger's algorithm for
definite hypergeometric summation in the Maxima computer algebra system.


The package implements Axel Riese's optimization (Filtering).
--------------------------------------------------------------------------

This package has been developed at RISC-Linz, 
J.Kepler Universitaet (Austria).

--------------------------------------------------------------------------

Copyright (C) Fabrizio Caruso and the RISC Combinatorics Group
Johannes Kepler Universitaet, Linz, Austria                   

This program is free software; you can redistribute it and/or modify        
it under the terms of the GNU General Public License as published by        
the Free Software Foundation; either version 2 of the License, or           
(at your option) any later version.                                         
                                                                           
This program is distributed in the hope that it will be useful,             
but WITHOUT ANY WARRANTY; without even the implied warranty of              
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               
GNU General Public License for more details.                               

--------------------------------------------------------------------------


THE PROBLEM

The package provides an implementation of Zeilberger's algorithm
for definite hypergeometric summation.

Given a proper hypergeometric term (in n and k) F_n,k and a
positive integer d we want to find a d-th order linear
recurrence with polynomial coefficients (in n) for F_n,k
and a rational function R in n and k such that:

a_0 F_n,k + ... + a_d F_(n+d),k = Delta_K(R(n,k) F(n,k)),

where

Delta_k is the k-forward difference operator, i.e.,
Delta_k(t_k) := t_(k+1) - t_k.

--------------------------------------------------------------------------

LOADING

To load the package load the batch file 

"LoADZeilberger.mac",

which will take care of loading and evalutating 
all the necessary files.

--------------------------------------------------------------------------

INSTRUCTIONS

The package provides two commands:

parGosper(F_n,k , k , n , d),

Zeilberger(F_n,k , k , n)


parGosper tries to find a d-th order recurrence.
If it finds none it outputs the empty solution [].


Zeilberger starts with order 1 and
tries up to the order given by the variable MAX_ORD.
If Zeilberger finds a solution before reaching MAX_ORD
it stops and yields the solution otherwise
it tries a higher order.

--------------------------------------------------------------------------

FORM OF THE OUTPUT

The algorithms yields a sequence:
[s_1,s_2, ..., s_m] of solutions.

Each solution has the following form:

[R(n,k), [a_0, a_1, ..., a_d]] 


--------------------------------------------------------------------------

VERBOSITY LEVELS

There are also verbose and very verbose versions of the commands:

parGosperVerbose

parGosperVeryVerbose

ZeilbergerVerbose

ZeilbergerVeryVerbose

