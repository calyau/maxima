Maxima 5.39 change log
======================

New items in core:
------------------

New functions firstn and lastn

Changes in core:
----------------

Special variable clean-up
Crosscompiling support
Gnuplot 5.0 or newer versions
Documentation updates
function save: portable way to save "fast" arrays (hash tables)
functions timedate, parse_timedate: handle timezone offsets
Floating point overflow handling
Complex numericals
Testsuite additions and improvements
GIT fixes
ODE improvements
Load .wxm files from wxMaxima
Temporary file handling improvements

New in share:
-------------

package decfp: decimal bigfloat package (thanks to Richard Fateman)

Changes in share:
-----------------

In some share test scripts, put reset() at the top
package solver: rename solver.mac to Solver.mac
package stringproc: new functions slessp, slesspignore, sgreaterp, sgreaterpignore (string comparison)
package charsets: several updates and improvements; still work in progress
package boolsimp: code clean-up; no functional changes
package descriptive, functions continuous_freq, discrete_freq: faster counting algorithm; accept arrays as well as lists

Bug fixes:
----------

[54822ea] Fixed bug #3258: diff with negative order in integrate
[0e4ed41] Fix issue #3255 by changing how resimplification works
[55ba236] In risplit, in preparation of fixing issue #3255 (logarc)
[6e7f9dd] Partial fix #3216: In package lapack, load without compile
[ad86a80] Fixed bug #3232: In ezunits, don't compile predicate functions
[0ec66c6] Fixed GCL bug #49689: Mark a known failure in rtest11
[98b0349] Fixed bug #3213: Round when formatting bigfloats
[3745efa] Another test for bug #3244.
[67fd622] Fixed bug #3246: look-up entries for inv. trig antiderivatives
[120d8be] Fixed bug #3241: In MQAPPLY1, call AMPERCHK
[e916dd0] Fixed bug #3235: In RGRP, rephrase test for ECL
[7060221] Add tests from bug #3221
[2204fca] Fixed bug #3221: elliptic_pi(n,z,m) wrong for z > %pi/2
[cb3dc1c] Fixed bug #453: algsys fails in simple case
[7efa7eb] Add tests from SF bug 2059.
[4851e2c] Add tests from SF [bugs:2038] and [bugs:#2736]
640ca75 SF [bug:#1266][bug:#3208] Don't use $radcan when simplifying in algsys.
2986fc0 Fix SF [bugs:#3208]. Use sqrtdenest in algsys to simplify partial solutions.
5fd2f55 [bugs:#3208] algsys.lisp:presultant: resimplify result
4734ec8 share-subdirs.lisp is auto-generated *and* in version control - which is afaik unavoidable for the lisp-only build system. It also currently is out-of-sync with the repo => Updated the repo version.
f91d677 Increase output when running share/contrib/diffequations/tests
a5873ba SF bugs 3210 and 3212.  Fix algsys regressions.
9a4694f Bug #2796 can't load ode2 with n declared constant
3c6c473 Bug report ID: 1621 Wrong solution to ode2
cf9cbb2 Fixed bug #2667: Distinguish trig function exponents in TeX
bf4cf94 In rtest_log, additional tests for SF bug #3105.
9ef3fee Fixed bug #3148: Most of the change is in indentation
8f523fc Fixed bug #3194: No simplification of tan(x+n*%pi) and cot(x+n*%pi)
fdadebc Fixed bug #3186 and other get_plot_option issues.
3fd2b4c Fixed bug #3022: Update the RPM spec file
aa0a365 Fixed bug #3180: declare(foo, antisymmetric)
9ad3f2f Add test for SF bug #3090
a57ff0c Add test for SF bug #3170
4568937 Fixed bug #3144: Avoid unbounded recursion in PTIMES%E
0bcccbe Fixed bug #3109: is(sin(x) <= 1) returns "unknown"
037ea4a Fixed bugs #3114, #3115: introduced by [647516] Fix integrate(x=0,x)

Unnumbered bugs:
----------------

[c8007b9] Partial fix to read_xpm.
[35790e2] Correct documentation for rest
[eb56614] Fix test for orderlessp transitive
[85aff4c] Fix in gcl-builds smismatch
[d37c3e4] Fix in timedate, round TZ
[74800c2] Remove redundant definition of copy from share/affine
[8ec3a05] Another fix for summation of rational functions
[394ca66] In package facexp, remove "&&" because
[b7ee537] Ensure that *OPR-TABLE* associates "{" with $SET
[9f43849] Ran update_examples on Simplification.texi
[dd6fbb1] Fix uses of complex-float-numerical-eval-p
[07363c2] Fix bug in running test files
[6d1b6fe] Partially fixed assumptions regarding products and powers
[639dd40] Partially fixed assumptions regarding products and powers
[691cbc4] Fix taylor testsuite failure bug introduced by [22f4fd]
