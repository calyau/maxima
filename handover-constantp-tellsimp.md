# Handover: constantp() and functions with tellsimp rules

## 1. Summary

**Keep built-in functions built in when they get tellsimp rules.**
MOPP, which tells built-in operators from user functions, took every function with `tellsimp` or `tellsimpafter` rules for a user function, so `constantp(log(2))` became `false` once `log` had a rule, and stayed `false` after `remrule` or `kill(all)`. MOPP now looks at the simplifier the function had before its rules.

## 2. Reproducer/Demo

**Old behavior** (pristine `HEAD` build):

```
(%i1) constantp(log(2));
(%o1)                                true
(%i2) tellsimp(log(x*y), log(x) + log(y))$

(%i3) constantp(log(2));
(%o3)                                false
(%i4) remrule(log, all)$

(%i5) constantp(log(2));
(%o5)                                false
```

**New behavior** (patched build):

```
(%i1) constantp(log(2));
(%o1)                                true
(%i2) tellsimp(log(x*y), log(x) + log(y))$

(%i3) constantp(log(2));
(%o3)                                true
(%i4) remrule(log, all)$

(%i5) constantp(log(2));
(%o5)                                true
```

The same mix-up let `:=` redefine a built-in function once it had a rule.

**Old behavior** (pristine `HEAD` build):

```
(%i1) log(x) := 1;
define: function name cannot be a built-in operator or special symbol; found: log
 -- an error. To debug this try: debugmode(true);

(%i2) tellsimp(log(x*y), log(x) + log(y))$

(%i3) log(x) := 1;
(%o3)                             log(x) := 1
```

**New behavior** (patched build):

```
(%i1) log(x) := 1;
define: function name cannot be a built-in operator or special symbol; found: log
 -- an error. To debug this try: debugmode(true);

(%i2) tellsimp(log(x*y), log(x) + log(y))$

(%i3) log(x) := 1;
define: function name cannot be a built-in operator or special symbol; found: log
 -- an error. To debug this try: debugmode(true);
```

**Internals.** `tellsimp` and `tellsimpafter` install the rule function as the OPERATORS property, for user functions as well, and keep the earlier OPERATORS values in OLDRULES, with the original simplifier (NIL for a user function) as the last element. So MOPP rejected every function with rules (RULECHK), which is wrong for a built-in such as `%log`. REMRULE leaves `(SIMP-%LOG)` in OLDRULES, so the wrong answer even outlived the rules. Now MOPP accepts a function with rules if the last element of OLDRULES is non-NIL. `$CONSTANTP` asks MOPP for every operator. The other callers (`:=`, `::=`, `defrule`, `atvalue`, `subst` on subscripted functions, the redefinition warning) now treat a built-in with rules like one without. Over the full test suite, MOPP's answer changes in 3179 calls, all for built-ins that earlier test files gave rules (`abs`, `derivative`, `.`, `imagpart`, `round`, `signum`, `sec`). No test result changes.

## 3. Bug report

**Title:**

```
constantp(log(2)) returns false once log has tellsimp rules
```

**Body:**

````markdown
Once a `tellsimp` or `tellsimpafter` rule exists for a built-in function such as `log`, `constantp` no longer recognizes a call of that function with constant arguments as constant:

```
(%i1) constantp(log(2));
(%o1)                                true
(%i2) tellsimp(log(x*y), log(x) + log(y))$

(%i3) constantp(log(2));
(%o3)                                false
(%i4) remrule(log, all)$

(%i5) constantp(log(2));
(%o5)                                false
```

`log(2)` is constant whether or not there are rules for `log`, so `(%o3)` and `(%o5)` should be `true`. Neither `remrule` nor `kill(all)` helps, so a single rule for `log` anywhere in a session, for example in an earlier test file, affects `constantp` for the rest of the session.

A related symptom: `log(x) := 1` is refused while `log` has no rules, but accepted once it has one:

```
(%i1) log(x) := 1;
define: function name cannot be a built-in operator or special symbol; found: log
 -- an error. To debug this try: debugmode(true);

(%i2) tellsimp(log(x*y), log(x) + log(y))$

(%i3) log(x) := 1;
(%o3)                             log(x) := 1
```
````

## 4. Code patching instructions

### `src/mlisp.lisp`: MOPP

Lets a function with rules count as an operator if it had a simplifier before them. Indentation uses tabs, as in the file.

**Replace** this block:

```lisp
(defun mopp (fun)
  (and (not (eq fun 'mqapply))
       (or (mopp1 fun)
	   (and (get fun 'operators) (not (rulechk fun))
		(not (member fun rulefcnl :test #'eq)) (not (get fun 'opers))))))
```

**with:**

```lisp
(defun mopp (fun)
  (and (not (eq fun 'mqapply))
       (or (mopp1 fun)
	   ;; TELLSIMP rules give FUN an OPERATORS property too, so with rules
	   ;; check the simplifier FUN had before them, last in its OLDRULES.
	   (and (get fun 'operators)
		(or (not (rulechk fun)) (car (last (mget fun 'oldrules))))
		(not (member fun rulefcnl :test #'eq)) (not (get fun 'opers))))))
```

## 5. Proposed test cases

**New**, appended at the end of `tests/rtest_rules.mac`:

```
/* Bug #NNNN: "constantp(log(2)) returns false once log has tellsimp rules" */

block([r],
  tellsimp(log(x*y), log(x) + log(y)),
  r : constantp(log(2)),
  remrule(log, all),
  [r, constantp(log(2))]);
[true, true]$
```

Ran in a scratch copy of `tests/rtest_rules.mac`: the new problem (242) gives `[false, false]` on a pristine build and passes on the patched one, and the whole file passes (242/242). `run_testsuite(share_tests = true)` on the patched build: 1 of 21,676 tests failed, `rtestprintf.mac` problem 38, which fails the same way on the pristine build (SBCL 2.2.9 float printing). `tests/depcheck.sh` is clean.

## 6. Proposed Git commit message

```
Fix constantp() of functions with tellsimp rules

Once a built-in function such as log() had a tellsimp() or
tellsimpafter() rule, constantp(log(2)) returned false. Neither
remrule() nor kill(all) helped, so a single rule spoiled constantp()
for the rest of the session.

Maxima took every function with rules for a user function, because a
rule gives it a simplifier, as built-in functions have. Now it looks at
the simplifier the function had before its rules. This also makes :=
refuse to redefine a built-in function with rules, as it does for one
without.

This fixes bug #NNNN.

AI-Assisted-By: Claude Opus 5.5
```
