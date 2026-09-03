# The sign of x has two normal forms, x/abs(x) and abs(x)/x, and their difference does not simplify

**Version:** Maxima git master (5.50post), SBCL, `domain : real`. A missed simplification rather than a wrong result.

The simplifier knows that `abs(x)^2` is `x^2`: it reduces `(x/abs(x))^2` and `(abs(x)/x)^2` to 1, `abs(x)^3` to `x^2*abs(x)`, `x^2/abs(x)^3` to `1/abs(x)`, `abs(x)/x^2` to `1/abs(x)` and `x/abs(x)^2` to `1/x`, so at most one power of `abs(x)` survives in a product and the even part is moved to `x`. But it has no single form for the sign of `x`: `x*abs(x)^(-1)` stays `x/abs(x)` and `abs(x)*x^(-1)` stays `abs(x)/x`, and a sum of the two is not recognized as 0 by the simplifier or by `expand`, only by `ratsimp` and `radcan` which happen to produce `abs(x)^2 - x^2` on the way.

```
(%i1) display2d : false$
(%i2) [x/abs(x), abs(x)/x, (x/abs(x))*(abs(x)/x), (x/abs(x))^2, (abs(x)/x)^2];
(%o2) [x/abs(x),abs(x)/x,1,1,1]
(%i3) [abs(x)^3, x^2/abs(x)^3, abs(x)/x^2, x/abs(x)^2, (x/abs(x))^3];
(%o3) [x^2*abs(x),1/abs(x),1/abs(x),1/x,x/abs(x)]
(%i4) x/abs(x) - abs(x)/x;
(%o4) x/abs(x)-abs(x)/x
(%i5) expand((1 + x/abs(x))*(1 - abs(x)/x));
(%o5) x/abs(x)-abs(x)/x
(%i6) expand((1 + x/abs(x))*(1 - x/abs(x)));
(%o6) 0
(%i7) [ratsimp(x/abs(x) - abs(x)/x), radcan(x/abs(x) - abs(x)/x), is(equal(x/abs(x), abs(x)/x))];
(%o7) [0,0,true]
(%i8) [abs(x^3)^(1/3), abs(-x^3)^(-1/3), abs(x)^(2/3)];
(%o8) [x^(2/3)*abs(x)^(1/3),1/(x^(2/3)*abs(x)^(1/3)),x^(2/3)]
```

(%o5) and (%o6) are the same product written with the two forms of the sign; only the second collapses. (%o8) shows the same gap for a root: `abs(x^3)^(1/3)` is `abs(x)`, but `abs(x^3)` is first normalized to `x^2*abs(x)` and the root then distributed over it, while `abs(x)^(2/3)` alone does become `x^(2/3)`.

**Why it matters.** With `domain : real` the principal branch of a power of a negative quantity, as needed next to `gamma_incomplete`, is naturally written as a phase `alpha + beta*x/abs(x)` with constant `alpha` and `beta`, and such phases from different sources, an antiderivative and its derivative, or two terms of `gamma_expand`, have to cancel under `expand`. They do so only if every source writes the sign the same way; the work on the `domain : real` branch had to arrange that by hand (multiplying the sign into the phase by the parity of the power of `x` that comes with it, and avoiding `abs(z)^s` for the modulus).

**Proposal.** Pick one canonical form for the sign and have `simptimes` produce it. Since the simplifier already keeps at most one power of `abs(x)` and moves the even part to `x`, the natural rule is the one it applies for positive exponents extended to negative ones: `abs(x)^(-1)*x` and `abs(x)*x^(-1)` both to `x/abs(x)`, and in general `x^p*abs(x)^q` with `p + q` even to `x^(p+q)` and with `p + q` odd to `x^(p+q-1)*abs(x)` or `x^(p+q+1)/abs(x)`, whichever keeps the exponent of `abs(x)` in `{-1, 1}`. Then (%o4) and (%o5) are 0 by simplification. For a fractional power, `(x^(2*m)*abs(x))^(1/(2*m+1))` could be recognized as `abs(x)`, but that is a smaller matter.
