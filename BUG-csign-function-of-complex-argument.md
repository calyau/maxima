# csign(gamma(z)) is pnz for a z declared complex, and the abs rules of the simplifier act on it

`csign` answers `complex` for `sin(z)`, `exp(z)`, `log(z)` or `z^2` when `z` is declared `complex`, but `pnz`, real of unknown sign, for `gamma(z)`, `tan(z)`, `erfc(z)`, `zeta(z)`, `bessel_j(0, z)`, `psi[0](z)`, `expintegral_e1(z)`, `lambert_w(z)`, `floor(z)`, an undefined `f(z)` and others. The `abs` rules of `simpexpt` and `timesin` in `src/simp.lisp` are guarded by `csign` and trust it:

```
(%i1) display2d : false$
(%i2) declare(z, complex)$
(%i3) map(csign, [sin(z), gamma(z), tan(z), erfc(z), f(z)]);
(%o3) [complex,pnz,pnz,pnz,pnz]
(%i4) [abs(gamma(z))^3, abs(gamma(z))^(2/3), gamma(z)^2/abs(gamma(z))^3, abs(f(z))^3];
(%o4) [gamma(z)^2*abs(gamma(z)),gamma(z)^(2/3),1/abs(gamma(z)),f(z)^2*abs(f(z))]
```

All four are wrong for a complex value: at `z = -1.3+0.7*%i`, `abs(gamma(z))^3` is 0.311 and `gamma(z)^2*abs(gamma(z))` is `-0.158+0.268*%i`.

The gap is in `src/compar.lisp`: an application whose operator has no sign rule of its own is treated as real, whatever its arguments. Answering `complex` there whenever an argument is not real by `csign` would close it; the functions that are real for a real argument, `gamma` and `erfc` among them, would keep `pnz` for a real one. Nothing changes for an undeclared symbol.
