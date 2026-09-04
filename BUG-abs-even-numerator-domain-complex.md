# abs(x)^(2/3) simplifies to x^(2/3) with domain : complex, where x^(2/3) is on the principal branch

**Status: fixed in `master` as bug #5225** (*Don't simplify abs(x)^(2/3) to x^(2/3) under domain : complex*).

With `domain : real` the simplifier takes the real root, `(-8)^(2/3)` is 4, so `abs(x)^(2/3)` and `x^(2/3)` are the same function of a real `x` and the simplifier rightly rewrites the one as the other. With `domain : complex` the power is on the principal branch, `(-8)^(2/3)` is `4*(-1)^(2/3)`, and the two differ for every negative `x`. The rewrite happens anyway:

```
(%i1) display2d : false$
(%i2) domain : complex$
(%i3) abs(x)^(2/3);
(%o3) x^(2/3)
(%i4) subst(x = -8, abs(x)^(2/3));
(%o4) 4*(-1)^(2/3)
(%i5) float(rectform(%));
(%o5) 3.4641016151377544*%i-2.0
(%i6) abs(-8)^(2/3);
(%o6) 4
```

So under `domain : complex` an expression that is 4 at `x = -8` is turned into one that is `-2 + 3.46*%i` there. The same happens for every exponent with an even numerator and an odd denominator, `abs(x)^(4/5)` becomes `x^(4/5)`, and for any base that `csign` reports as real, `abs(x-1)^(2/3)`, `abs(sin(x))^(2/3)`.

The clause is in `simpexpt` in `src/simp.lisp`, the one that rewrites `abs(x)^pot` as `x^pot` for an exponent that satisfies `(or (evnump pot) (mevenp pot))`, with `domain : real` unless `csign` reports the base complex and with `domain : complex` when it reports it real. `evnump` accepts an even integer and a rational with an even numerator. For an even integer, or a symbol declared `even`, the identity `abs(x)^n = x^n` holds for a real `x` in both domains. For the fraction it holds only for the real root, so the `domain : complex` arm should not accept it:

```lisp
(or (mevenp pot)
    (and (eq $domain '$real) (evnump pot)))
```

With that, `abs(x)^(2/3)` stays as it is under `domain : complex`, `abs(x)^2` is still `x^2` in both domains, and nothing changes with `domain : real`. The full test suite passes, core and share. A randomized comparison of products of powers of a base and of its `abs` against direct numerical evaluation, 300 products at real points under `domain : complex`, had 22 wrong results before and none after.
