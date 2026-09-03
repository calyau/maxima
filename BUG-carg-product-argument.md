# carg, polarform and rectform of a product do not reduce the argument to (-%pi, %pi]

**Version:** Maxima git master (5.50post), SBCL. The same with `domain : real` and `domain : complex`.

`absarg` in `src/rpart.lisp` computes the argument of a product as the sum of the arguments of its factors and never reduces the sum into `(-%pi, %pi]`. A power does get reduced, by a `ceiling` term, a product does not. So `carg(-x)` is `atan2(0,x)+%pi`, which is `2*%pi` for a negative `x`, and every function built on `absarg` that takes a root of the argument returns a wrong value for such `x`: `rectform(sqrt(-x))` is `-2^(3/2)` at `x = -8`, where `sqrt(8)` is `2^(3/2)`.

```
(%i1) display2d : false$
(%i2) carg(-x);
(%o2) atan2(0,x)+%pi
(%i3) subst(x = -8, %);
(%o3) 2*%pi
(%i4) carg(-x*y);
(%o4) atan2(0,y)+atan2(0,x)+%pi
(%i5) subst([x = -1, y = -1], %);
(%o5) 3*%pi
(%i6) rectform(sqrt(-x));
(%o6) %i*sin((atan2(0,x)+%pi)/2)*sqrt(abs(x))+cos((atan2(0,x)+%pi)/2)*sqrt(abs(x))
(%i7) subst(x = -8, %);
(%o7) -2^(3/2)
(%i8) sqrt(-x), x = -8;
(%o8) 2^(3/2)
(%i9) domain : complex$
(%i10) rectform((-x)^(1/3));
(%o10) %i*sin((atan2(0,x)+%pi)/3)*abs(x)^(1/3)+cos((atan2(0,x)+%pi)/3)*abs(x)^(1/3)
(%i11) subst(x = -8, %);
(%o11) sqrt(3)*%i-1
(%i12) float(rectform((-x)^(1/3))), x = -8;
(%o12) 2.0
(%i13) carg(x^3);
(%o13) 3*atan2(0,x)-2*%pi*ceiling((3*atan2(0,x)-%pi)/(2*%pi))
```

The last output shows the reduction that a power gets and a product lacks.

**Expected:** `carg(-x)` equal to `atan2(0, -x)`, that is 0 for `x < 0` and `%pi` for `x > 0` (for instance `%pi - atan2(0, x)`), `carg(-x*y)` equal to `%pi` at `x = y = -1`, `rectform(sqrt(-x))` equal to `2^(3/2)` at `x = -8`, and `rectform((-x)^(1/3))` equal to 2 there with `domain : complex`.

**Where:** `absarg` in `src/rpart.lisp`, the `mtimes` clause, which returns `(2pistrip (addn argl t))`; `2pistrip` strips only exact multiples of `2*%pi` and leaves a sum such as `atan2(0,x)+%pi` alone.
