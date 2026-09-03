# rectform(atan2(y, 0)) is wrong for negative y and carries a spurious imaginary part

**Version:** Maxima git master (5.50post), SBCL, `domain : real` (the default).

`rectform` of `atan2(y, 0)` for a symbolic real `y` returns an expression that is off by `2*%pi` for `y < 0`, and that has a nonzero-looking imaginary part `-%i*log(abs(y)/sqrt(y^2))`, which is zero but is not simplified: `sqrt(y^2)` was produced with `$domain` bound to `complex` inside `risplit` and is not resimplified to `abs(y)` once the binding is gone. The real part is not reduced into `(-%pi, %pi]` either. Any expression that has such an `atan2` in an exponent inherits the error, as the last input shows.

```
(%i1) display2d : false$
(%i2) rectform(atan2(y, 0));
(%o2) (2*%pi*ceiling((2*atan2(0,y)-%pi)/(2*%pi))+%pi)/2-%i*log(abs(y)/sqrt(y^2))
(%i3) subst(y = 2, %);
(%o3) %pi/2
(%i4) subst(y = -2, rectform(atan2(y, 0)));
(%o4) (3*%pi)/2
(%i5) atan2(-2, 0);
(%o5) -%pi/2
(%i6) imagpart(atan2(y, 0));
(%o6) -log(abs(y)/sqrt(y^2))
(%i7) rectform(%e^(%i*atan2(y, 0)/3));
(%o7) %e^(log(abs(y)/sqrt(y^2))/3)*%i*sin((2*%pi*ceiling((2*atan2(0,y)-%pi)/(2*%pi))+%pi)/6)+%e^(log(abs(y)/sqrt(y^2))/3)*cos((2*%pi*ceiling((2*atan2(0,y)-%pi)/(2*%pi))+%pi)/6)
```

**Expected:** `atan2(y, 0)` is real for every real `y`, so `imagpart(atan2(y, 0))` should be 0 and `rectform(atan2(y, 0))` an expression equal to `%pi/2` for `y > 0` and `-%pi/2` for `y < 0`, for instance `atan2(y, 0)` itself, or `%pi/2 - atan2(0, y)`, which `rectform(atan2(y, 1))` and `rectform(atan2(1, 0))` get right.

**Where:** `risplit` in `src/rpart.lisp`, the clause for `%atan2`, which goes through the logarithmic form of the arc tangent; the reduction by the `ceiling` term is applied to `2*atan2(0, y)` and does not bring the result back into the principal range.
