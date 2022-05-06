# pslq
A [Maxima](http://maxima.sf.net/) package for the PSLQ algorithm.

Forked from: https://github.com/andrejv/identify
Author: Andrej Vodopivec.

    (%i1) load("pslq.mac")$
    (%i2) root: float(sin(%pi/12))$
    (%i3) guess_exact_value(root);
    (%o3) sqrt(2-sqrt(3))/2
    (%i4) makelist(root^i, i, 0, 4)$
    (%i5) pslq_integer_relation(%);
    (%o5) [-1,0,16,0,-16]
    (%i6) makelist(x^i, i, 0, 4).%;
    (%o6) -16*x^4+16*x^2-1
    (%i7) solve(%);
    (%o7) [x=-sqrt(sqrt(3)+2)/2,x=sqrt(sqrt(3)+2)/2,x=-sqrt(2-sqrt(3))/2,x=sqrt(2-sqrt(3))/2]
