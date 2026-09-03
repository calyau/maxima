# gamma_incomplete_lower(a, z) with a rational order a and a float z is not evaluated

**Version:** Maxima git master (5.50post), SBCL.

For an order that is neither an integer nor a half integer, `gamma_incomplete_lower` with a float or bigfloat argument returns `gamma(a) - gamma_incomplete(a, z)` with `gamma(a)` left exact, instead of a number. The upper function evaluates, and so does `gamma_incomplete_lower` for a half-integer or an integer order.

```
(%i1) display2d : false$
(%i2) gamma_incomplete_lower(1/3, 8.0);
(%o2) gamma(1/3)-7.799182611869946e-5
(%i3) gamma_incomplete_lower(1/3, -8.0);
(%o3) gamma(1/3)+719.2849028282662*%i+412.60039373722566
(%i4) gamma_incomplete_lower(1/3, 8.0*%i);
(%o4) gamma(1/3)+0.10901538887260959*%i+0.221572520567445
(%i5) gamma_incomplete_lower(1/3, 8.0b0*%i);
(%o5) gamma(1/3)+1.090153888726096b-1*%i+2.215725205674452b-1
(%i6) gamma_incomplete_lower(0.5, 8.0*%i);
(%o6) 0.22561853882998203*%i+2.040645378736624
(%i7) gamma_incomplete(1/3, 8.0*%i);
(%o7) -(0.10901538887260959*%i)-0.221572520567445
(%i8) float(gamma_incomplete_lower(1/3, 8*%i));
(%o8) 0.10901538887260959*%i+2.900511055275193
```

**Expected:** a float in each of (%o2) to (%o5), as in (%o8): `2.6788605428816297` for `gamma_incomplete_lower(1/3, 8.0)`.

**Where:** the simplifier of `gamma_incomplete_lower` in `src/gamma.lisp`, whose numerical clause subtracts the numerically evaluated `gamma_incomplete(a, z)` from `gamma(a)` built from the exact `a`, so that `gamma(a)` is not evaluated when `a` is exact and only `z` is a float.

A related omission: `gamma_incomplete_lower` has no `conjugate-function` property, so `rectform(gamma_incomplete_lower(1/3, %i*x^3))` returns `realpart` and `imagpart` noun forms, where `rectform(gamma_incomplete(1/3, %i*x^3))` uses the mirror symmetry of the function.
