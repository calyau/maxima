# Nice to have: one predicate for "a power with an even numerator and an odd denominator", so that abs(x)^(2/m) and x^n*abs(x)^(1/3) with a declared parity simplify like the numeric cases, and the places that test it share the code

With `domain : real` a power `x^(p/q)` with an even `p` and an odd `q` is the real root of `x^(1/q)` squared, so it is `abs(x)^(p/q)`, and the simplifier uses that: `abs(x)^(2/3)` is `x^(2/3)`, and `x^(2/3)*abs(x)^(1/3)` is `abs(x)`. It uses it only when `p/q` is a number. When the parity is known but symbolic, `n` declared `even` or `m` declared `odd`, some of it happens and some does not:

```
(%i1) display2d : false$
(%i2) declare(n, even, m, odd)$
(%i3) [abs(x)^n, abs(x)^(2*m), abs(x)^(2/3), abs(x)^(2/m), abs(x)^(n/3), abs(x)^(2*n/m)];
(%o3) [x^n,x^(2*m),x^(2/3),abs(x)^(2/m),abs(x)^(n/3),abs(x)^((2*n)/m)]
(%i4) [x^(2/3)*abs(x)^(1/3), x^n*abs(x)^(1/3), x^(2/m)*abs(x)^(1/m), x^(n/3)*abs(x)^(1/3), x^4/abs(x), x^n/abs(x)];
(%o4) [abs(x),x^n*abs(x)^(1/3),x^(2/m)*abs(x)^(1/m),x^(n/3)*abs(x)^(1/3),x^2*abs(x),x^n/abs(x)]
(%i5) map(csign, [x^(2/m), x^(n/3), x^(2*n/m), x^(1/n)]);
(%o5) [pz,pz,pz,complex]
```

`csign` already knows that `x^(2/m)` and `x^(n/3)` are nonnegative, but the `abs` rules do not: `abs(x)^(2/m)` stays, `x^(2/m)*abs(x)^(1/m)` could be `abs(x)^(3/m)` and `x^n*abs(x)^(1/3)` could be `abs(x)^(n+1/3)`. Nothing here gives a wrong result; `x^(1/n)` is an even root, `csign` says `complex`, and nothing touches it. It is a missed simplification, and the reason is that the parity of an exponent is tested in four places with four different pieces of code:

- `simpexpt` in `src/simp.lisp`, the clause that turns `abs(x)^pot` into `x^pot`, tests `(mevenp pot)`, which through `evod` in `src/compar.lisp` handles a number, a declared symbol, and products, sums and non-negative integer powers of those, plus `(evnump pot)` for a rational with an even numerator, with `domain : real`. `evod` does not look into a reciprocal or a rational coefficient, so `2/m` and `n/3` are not recognized.
- The `timesin` clauses that combine `x^a*abs(x)^b` into `abs(x)^(a+b)` test `(and (ratnump a) (evenp (cadr a)))` for `a` and `(or (integerp b) (ratnump b))` for `b`: numbers only.
- `sign-mexpt` in `src/compar.lisp` judges `b^(p/q)` by its numerator when `(eq (evod q) '$odd)`, so it does see `2/m` and `n/3`.
- `odd-root-p` in `src/rpart.lisp`, used by `rectform`, `carg`, the integrator and the derivative of `gamma_incomplete`, tests `(maxima-integerp ($num pow))` and `(eq (evod ($denom pow)) '$odd)`, the same idea again.

The four older `abs` clauses of `timesin`, the ones that cancel even powers between `abs(x)` and `x`, are a fifth place, with `integerp` only: `x^4/abs(x)` is `x^2*abs(x)`, `x^n/abs(x)` stays.

A single predicate, "an exponent whose numerator is even and whose denominator is odd, by `evod` on `$num` and `$denom`", would answer for `2/3`, `n`, `2*m`, `2/m`, `n/3` and `2*n/m` alike, and could replace the parity test in all of these places: in `simpexpt` next to `mevenp` under `domain : real`, in the `timesin` combination for `a`, with the integer case still excluded there, and as the numerator half of `odd-root-p` and of the `sign-mexpt` rule. That is fewer lines than there are today, one definition to get right instead of four, and the symbolic cases come for free. Only `domain : real` may use the fraction half of it; with `domain : complex` `x^(2/m)` is on the principal branch, where the identity fails for a negative `x`, and the same restriction that `simpexpt` already has for `2/3` applies.
