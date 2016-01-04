@menu
* Introduction to celine::
@end menu

@node Introduction to celine
@section Introduction to celine

Maxima implementation of Sister Celine's method. Barton Willis wrote this code. It is released under the @uref{https://creativecommons.org/about/cc0,Creative Commons CC0 license}.

Celine's method is described in Sections 4.1--4.4 of the book "A=B", by Marko Petkovsek, Herbert S. Wilf, and Doron Zeilberger.
This book is available at @uref{http://www.math.rutgers.edu/~zeilberg/AeqB.pdf}

Let f = F(n,k). The function celine returns a set of recursion relations for F of the form

    p_0(n) * fff(n,k) + p_1(n) * fff(n+1,k) + ... +  p_p(n) * fff(n+p,k+q),

where p_0 through p_p are polynomials. If Maxima is unable to determine that sum(sum(a(i,j) * F(n+i,k+j),i,0,p),j,0,q) / F(n,k) 
is a rational function of n and k, celine returns the empty set. When f involves parameters (variables other than n or k), celine
might make assumptions about these parameters. Using 'put' with a key of 'proviso,' Maxima saves these assumptions on the input 
label.

To use this function, first load the package integer_sequence, opsubst, and to_poly_solve.

Examples:

@c ===beg===
@c load("integer_sequence")$
@c load("opsubst")$
@c load("to_poly_solve")$
@c load("celine")$
@c celine(n!,n,k,1,0);
@c ===end===
@example
(%i1) load("integer_sequence")$
(%i2) load("opsubst")$
(%i3) load("to_poly_solve")$
(%i4) load("celine")$
@group
(%i5) celine(n!,n,k,1,0);
(%o5)       @{fff(n + 1, k) - n fff(n, k) - fff(n, k)@}
@end group
@end example

Verification that this result is correct:
@c ===beg===
@c load("integer_sequence")$
@c load("opsubst")$
@c load("to_poly_solve")$
@c load("celine")$
@c g1:{fff(n+1,k)-n*fff(n,k)-fff(n,k)};
@c ratsimp(minfactorial(first(g1))),fff(n,k) := n!;
@c ===end===
@example
(%i1) load("integer_sequence")$
(%i2) load("opsubst")$
(%i3) load("to_poly_solve")$
(%i4) load("celine")$
@group
(%i5) g1:@{fff(n+1,k)-n*fff(n,k)-fff(n,k)@};
(%o5)       @{fff(n + 1, k) - n fff(n, k) - fff(n, k)@}
@end group
@group
(%i6) ratsimp(minfactorial(first(g1))),fff(n,k) := n!;
(%o6)                           0
@end group
@end example

An example with parameters including the test that the result of the example
is correct:
@c ===beg===
@c load("integer_sequence")$
@c load("opsubst")$
@c load("to_poly_solve")$
@c load("celine")$
@c e : pochhammer(a,k) * pochhammer(-k,n) / (pochhammer(b,k));
@c recur : celine(e,n,k,2,1);
@c /* Test this result for correctness */
@c first(%), fff(n,k) := ''(e)$
@c makefact(makegamma(%))$
@c minfactorial(factor(minfactorial(factor(%))));
@c ===end===
@example
(%i1) load("integer_sequence")$
(%i2) load("opsubst")$
(%i3) load("to_poly_solve")$
(%i4) load("celine")$
@group
(%i5) e : pochhammer(a,k) * pochhammer(-k,n) / (pochhammer(b,k));
                           (a)  (- k)
                              k      n
(%o5)                      -----------
                              (b)
                                 k
@end group
@group
(%i6) recur : celine(e,n,k,2,1);
(%o6) @{fff(n + 2, k + 1) - fff(n + 2, k) - b fff(n + 1, k + 1)
 + n ((- fff(n + 1, k + 1)) + 2 fff(n + 1, k) - a fff(n, k)
 - fff(n, k)) + a (fff(n + 1, k) - fff(n, k)) + 2 fff(n + 1, k)
    2
 - n  fff(n, k)@}
@end group
(%i7) /* Test this result for correctness */
(%i8) first(%), fff(n,k) := ''(e)$
@group
(%i9) makefact(makegamma(%))$
(%o9)                           0
@end group
(%i10) minfactorial(factor(minfactorial(factor(%))));
@end example

The proviso data suggests that setting a = b may result in a lower order recursion
which is shown by the following example:
@c ===beg===
@c load("integer_sequence")$
@c load("opsubst")$
@c load("to_poly_solve")$
@c load("celine")$
@c e : pochhammer(a,k) * pochhammer(-k,n) / (pochhammer(b,k));
@c recur : celine(e,n,k,2,1);
@c get('%,'proviso);
@c celine(subst(b=a,e),n,k,1,1);
@c ===end===
@example
(%i1) load("integer_sequence")$
(%i2) load("opsubst")$
(%i3) load("to_poly_solve")$
(%i4) load("celine")$
@group
(%i5) e : pochhammer(a,k) * pochhammer(-k,n) / (pochhammer(b,k));
                           (a)  (- k)
                              k      n
(%o5)                      -----------
                              (b)
                                 k
@end group
@group
(%i6) recur : celine(e,n,k,2,1);
(%o6) @{fff(n + 2, k + 1) - fff(n + 2, k) - b fff(n + 1, k + 1)
 + n ((- fff(n + 1, k + 1)) + 2 fff(n + 1, k) - a fff(n, k)
 - fff(n, k)) + a (fff(n + 1, k) - fff(n, k)) + 2 fff(n + 1, k)
    2
 - n  fff(n, k)@}
@end group
@group
(%i7) get('%,'proviso);
(%o7)                         false
@end group
@group
(%i8) celine(subst(b=a,e),n,k,1,1);
(%o8) @{fff(n + 1, k + 1) - fff(n + 1, k) + n fff(n, k)
                                                     + fff(n, k)@}
@end group
@end example
