# csign returns pnz for gamma(z), tan(z), erfc(z), f(z) and others when z is declared complex, and the abs rules of the simplifier act on it

`csign` answers `complex` for `sin(z)`, `exp(z)`, `log(z)`, `z^2`, `z-1` and most functions of a `z` declared `complex`, but `pnz` for a good number of others, among them `gamma(z)`, `gamma(z+1)`, `tan(z)`, `erfc(z)`, `zeta(z)`, `bessel_j(0, z)`, `z!`, `psi[0](z)`, `expintegral_e1(z)`, `log_gamma(z)`, `beta(z, w)`, `gamma_incomplete(2, z)`, `lambert_w(z)`, `polylog(2, z)`, `airy_ai(z)`, `elliptic_kc(z)`, `struve_h(1, z)`, `hankel_1(1, z)`, `floor(z)`, `round(z)` and any undefined function `f(z)`. `pnz` means real of unknown sign, and several simplifier rules for `abs` take it at its word:

```
(%i1) display2d : false$
(%i2) declare(z, complex)$
(%i3) map(csign, [sin(z), exp(z), log(z), z^2, gamma(z), tan(z), erfc(z), zeta(z), bessel_j(0,z), f(z), z[1]]);
(%o3) [complex,complex,complex,complex,pnz,pnz,pnz,pnz,pnz,pnz,complex]
(%i4) [abs(gamma(z))^3, abs(gamma(z))^(2/3), gamma(z)^2/abs(gamma(z))^3, abs(f(z))^3];
(%o4) [gamma(z)^2*abs(gamma(z)),gamma(z)^(2/3),1/abs(gamma(z)),f(z)^2*abs(f(z))]
```

All four results are wrong for a complex `gamma(z)` or `f(z)`. At `z = -1.3+0.7*%i`, `abs(gamma(z))^3` is about 0.311 and `gamma(z)^2*abs(gamma(z))` is about `-0.158+0.268*%i`:

```
(%i5) z0 : -1.3+0.7*%i$
(%i6) float(rectform([abs(gamma(z0))^3, gamma(z0)^2*abs(gamma(z0))]));
(%o6) [0.3110869165059718,0.2677271840061178*%i-0.1584210357413051]
```

The rules are in `timesin` and `simpexpt` in `src/simp.lisp`, all guarded by `(not (member ($csign ...) '($complex $imaginary)))`, which is the right guard as long as `csign` is right; the same weakness reaches every later rule with that guard. The gap is in `src/compar.lisp`: the sign of a function application whose operator has no sign rule is computed as if its value were real, whatever the arguments are. A conservative fix is for `csign` to answer `complex` for an application with no rule of its own whenever an argument is not real by `csign`, leaving the functions that have a rule, `sin`, `exp`, `log`, `abs`, the powers and so on, as they are. Functions known to be real for a real argument, `gamma` and `erfc` among them, would then need that rule to say so for a real argument only.

Nothing changes for an undeclared symbol, which is real for `csign`: `abs(gamma(x))^3` is `gamma(x)^2*abs(gamma(x))` for a real `x`, which is right.
