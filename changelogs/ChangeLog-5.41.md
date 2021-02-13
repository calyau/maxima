Maxima 5.41 change log
======================

  Cross-compiling changes and improvements.
  Documentation changes and improvements.
  Build/installation improvements.
  Updates to examples.
  Changes and improvements to test system.
  Changes and updates to nightly builds and tests.
  Added the rules to build a snappy package.
  Windows installer improvements and fixes.
  Improvements to Windows maxima.bat.
  Debian build improvements.
  Compatibility improvements with diverse LISP compilers.
  Plotting improvements; improved GNUPLOT error handling.
  Code optimizations.
  Removed some old/unused code; cosmetic changes.
  New test cases.

Bug fixes:
----------

#3329: Follow-on work to bug fix: avoid long argument lists in ssort and sreverse.
#3329: Avoid large number of arguments in simplode.
#3315: Fixed Maxima/Windows, when a username contains a space
#2356: diff(...,%pi) doesn't give an error
#3152, #2901, #2550, #2549, #2480, #2063: only simplify abs(x)^2 and (x^a)^b if x not complex

Unnumbered bugs:
----------------

271a4b9 Revise GET-DIRS as suggested by Camm Maguire 2017-09-18
cae7add gensym could return an integer instead of a symbol
6555d51 Fixes a bug in plotdf
fce837e Fixed trigsimp bug reported to mailing list on 2017-06-03.
4ecf453 Fixed compile/sort bug reported to mailing list on 2017-09-05.
e646bd5 Fixed run_testsuite() bug reported to mailing list on 2017-07-12.
2ef63bb Handle special case MU = 0 in RNDPOISSON-AHRENS. Fixes bug: random_poisson(0) => error
aa9eef0 In DECLARE1, handle alphabetic declaration separately from other cases. 
aa9eef0 Fixed alphabetic declaration bug reported to mailing list circa 2017-07-23.

