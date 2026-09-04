# Let a derivative defined by a DEFGRAD lambda return a final expression

**Status: fixed at the base of branch `claude/maxima-rectform-root-branch-pp1qu1`**, commit *SDIFFGRAD: let a derivative given as a function return a final result*, by the first option below: a lambda that returns `t` as a second value has its result taken as it is, and a lambda returning one value is substituted into as before. And it was a bug after all, not only a trap: the one lambda in the tree, the derivative of `gamma_incomplete` with respect to its order, evaluated its template with `meval`, so a value of the Maxima variable `a` or `z` went into the derivative of any `gamma_incomplete`:

```
(%i1) a : 5$
(%i2) assume(b > 0)$
(%i3) diff(gamma_incomplete(b, y), b);
(%o3) (gamma_incomplete(5,y)-24)*log(y)+576*hypergeometric_regularized([5,5],[6,6],-y)*y^5+24*(25/12-%gamma)
```

That lambda now builds its result from its arguments and returns it with the second value, and so do the `z` derivatives of the branch, which call `principal-power` directly, without the template described below. The rest of this note is as it was written.

**Version:** Maxima git master (5.50post), SBCL. Not a bug in current behaviour, a trap in the Lisp interface for defining derivatives, and a request to give it a second calling convention.

`DEFGRAD` (src/mopers.lisp) lets a derivative be given as a lambda, which `SDIFFGRAD` (src/comm.lisp) applies to the actual arguments of the function. The result is then treated like the other kind of derivative, a template in the placeholder symbols of the argument list: `SDIFFGRAD` runs it through `$psubstitute`, replacing each placeholder by the corresponding actual argument. So a lambda has to return a template as well, as the one derivative in the tree that is a lambda does (the derivative of `gamma_incomplete` with respect to its order, in src/gamma.lisp, returns `(meval #$$ ... a ... z $)`). A lambda that builds its result from the actual arguments gets it corrupted whenever an actual argument contains a symbol that happens to be named like a placeholder. The derivative below, for a dummy function `foo(a, z)` with `d/dz foo(a, z) = 2*z`, is right for the variable `y` and wrong for the variable `z`:

```
(%i1) display2d : false$
(%i2) :lisp (defgrad $foo ($a $z) nil #'(lambda ($a $z) (declare (ignore $a)) (mul 2 $z)))
(%i3) diff(foo(1, y^2), y);
(%o3) 4*y^3
(%i4) diff(foo(1, z^2), z);
(%o4) 4*z^5
(%i5) :lisp (defgrad $bar ($a $z) nil #'(lambda ($a $z) (declare (ignore $a $z)) (meval #$$ 2*z $)))
(%i6) diff(bar(1, z^2), z);
(%o6) 4*z^3
(%i7) diff(bar(1, y^2), y);
(%o7) 4*y^3
```

In (%o4) the lambda returned `2*z^2`, built from the actual second argument, and the substitution of `z^2` for the symbol `z` turned it into `2*z^4` before the chain rule multiplied by `2*z`. The template version `bar` is immune, since its `z` is the placeholder.

**Why a final expression is sometimes needed.** A template is enough when the derivative is a fixed formula in the arguments. It is not enough when the form of the derivative depends on the structure of an argument: the principal-branch derivative of `gamma_incomplete(a, z)` with `domain : real`, for a `z` such as `-x^3` or `%i*x^3`, has to write `z^(a-1)` as `abs(k)^s*abs(x)^(n*s)` times a phase in `x/abs(x)`, with `k`, `x` and `n` read off the actual `z`. As a template, `abs(z)^s` simplifies to `(x^2*abs(x))^s` and then to `x^(2*s)*abs(x)^s`, which does not cancel against anything, so the work on the `domain : real` branch had to encode the modulus as `(z^2*conjugate(k)/k)^(s/2)` in the placeholder, guard `k` with `freeof` against the placeholder symbols, and refuse a symbolic order altogether, where a final expression built from the actual arguments would have been one call. `SDIFFGRAD` already has a precedent for a derivative that is not re-substituted: the special case for `%hypergeometric`, which calls `DIFF-HYPERGEOMETRIC` with the actual arguments and uses its result as it is.

**Proposal.** One of:

- Let a lambda signal that its result is final, for instance by returning a second value `t`, or by a keyword in `DEFGRAD`; `SDIFFGRAD` then skips the substitution for it. Existing lambdas, returning one value, keep the template behaviour.
- A property, say `sdiff-function`, holding a function of the expression and the variable, looked up by `SDIFFGRAD` before the `grad` property, generalizing the `%hypergeometric` and `pdiff` special cases already there.

Whatever the choice, the `DEFGRAD` docstring should say that a lambda's result is substituted into, since nothing in it does now, and the derivative of `gamma_incomplete` with respect to its order could then be written more directly as well.
