Maxima 5.40 change log
======================

New items in core:
------------------
[0eda309] New functions encode_time and decode_time, with test cases.

New items in share:
-------------------
[534a7d1] add symplectic_ode package
[46a7a0f] Add Kaplan-Meier's survival and empirical distribution estimators

Changes in core:
----------------
[cb259bb] change O(n^2) method for flattening list to O(n) method.
[21cf8e1] split totient into two parts
[6ef2b26] optimized gcfactor and tried to make it easier to read.
[1594167] optimize gc squaring and inline gctime1
[d7ff7c3] inline gctimes.
[4c8b6c6] sped up psumsq.
[f306940] speeded up imodp.
[efadb61] new implementation of ; is about a factor of 2 faster on sbcl
[9c79308] move jacbi and related functions from rat3c to numth
[75d8ceb] sped up divsum by splitting it in two parts where the lower
          Numerous cross-compiling changes.
          Numerous documentation changes and improvements.
          Build/installation improvements.
          Many updates to examples.
          Many changes and improvements to test system.

Changes in share:
--------------
[9c7c70c] Commit new version of COMA. Sent by Wilhelm Haager
[0ef9ab0] Check for a constant "variable" of integration in romberg and bromberg
[834ffb3] Import ODEPACK fortran code.
[06bc1bd] Julia() and mandelbrot() are modified to create maxoutXXX.gnuplot
[dca80e3] plotdf: An option to change the arrow density. Patch from themusicgod1.

Bug fixes:
----------
[6ff1570] fix bug 3291: ecl on Linux needs utf-8
[db7788f] fix bug3300 Incorrect sha256sum output
[b896d1f] In rtest_translator, skip some tests which cause a crash (bug #3291).
[f1bc324] Implement numerical evaluation for gamma_greek. Fixes SF bug #3277.
[eec3d7f] In tests for kill(props), work around bug #3289.
[6267b88] In lsquares_mse, ensure temporary variable is assigned the correct value. #3282.
[b58a547] In lsquares_mse, ensure temporary variable is assigned the correct value. #3282.
[a7239b1] In RISCHLOGEPROG, detect zero divisor. This fixes SF bug #3274.
[eaacba6] Unlock :common-lisp package when compiling with SBCL. Fixes SF bug #3271.
[71ee1c0] Fix bug #2740: demo(romberg) fails
[5fb8f81] Add test for bug #2295: Failure to evaluate definite integral
[35c625e] Add tests for bug #2314: Two different results for an integral
[56fb70e] In tests for simplify_sum, add a few more tests from bug report #3236.

Unnumbered bugs:
----------------
[06c0fc7] operatingsystem: Make chdir work with recent gcl versions.
[7277f54] gcfactor(4/(%i+1)) gave an internal error in $bothcoef.
[c477689] Give informative error message when some index is not an integer: "rat(-1) isn't an integer?" from mailing list 2017-03-04.
[96c57d5] work around bug in gcl decode-universal-time.
[b4819b4] work around bug in gcl decode-universal-time.
[a36bf53] When evaluating float(diff(...)), apply float only to the first argument. Fixes bug reported to mailing list 2017-02-27.
[bffe54f] In GETL-LM-FCN-PROP, return NIL for argument which is not a symbol. Fixes bug reported to mailing list 2017-03-04.
[ef1fa9e] Remove built-in symbols from the list for user-defined properties. Fixes bug reported to mailing list 2017-02-24.
[70e05dd] Handle Lisp arrays in MQAPPLY expressions more carefully. Fixes bug reported to mailing list 2017-02-02.
[761cc1c] Handle Lisp arrays in MQAPPLY expressions more carefully. Fixes bug reported to mailing list 2017-02-02.
[b6dd08c] A solve() enhancement by Richard Fateman that should fix a zillion of bug tickets.
[b6b5c5c] Translate define and ":=" correctly by nuking the translator property. Fixes bug reported to mailing list circa 2017-01-24.
[5cfc6fd] Fix bernoulli and binomial pdfs when p=0 or p=1
[d6739e9] Fix bernoulli and binomial pdfs when p=0 or p=1
