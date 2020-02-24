Maxima next change log
======================

New items in core:
------------------
 * Additional float constants: LEAST-NEGATIVE-NORMALIZED-FLONUM,
   largest_negative_float, least_negative_float, 
   least_positive_normalized_float, and least_negative_normalized_float.
 * Support for pre/post sub/superscripts (get_index_properties,
   declare_index_properties and remove_index_properties).
    (2) Make declare_index_properties
 * Mark non-trivial results from gcfactor with a GCFACTORED flag

New items in share:
-------------------
 * pytranslate, which converts maxima to python
 * rtest_inteqn
 * descriptive: find_runs and find_runs_inverse
 
Changes in core:
----------------
 * Code clean ups and removal of functions that only had a historical
   meaning
 * In TEST-BATCH, ensure that list of unexpectedly passed tests is 
   nonempty before reporting it.
 * :lisp-quiet can now be told not to use subsequent :lisp commands
 * defmfun now adds an ARG-LIST property that allows frontends to
   validate arguments
 * An improved version of float_approx_equal
 * Better handling of non-numerical input to find_root
 * Better handling of defint errors triggered by laplace()
 * Avoid variable capture in the WITH-NEW-CONTEXT macro
 * Evaluate the body of a WITH-NEW-CONTEXT form as an implicit PROGN
 * Numbers are no loger compared using EQ instead of EQL
 * Characters are no loger compared using EQ instead of EQL
 * Fixed bogus translations of apply2 and applyb2
 * Fixed bogus translations of atan and atan2 with float arguments
 * Fixed the inconsistent translation of signum with a float argument
 * Updated gentran to v.5
 * Improved the pattern matcher for "+" and "*" expressions, see 
   maxima-discuss, circa 2019-12-27: "Function to recognise series"
 * allow zn_log to work on subgroups
 * gamma_incomplete_lower is now returned in the noun form in many places
 
Changes in share:
--------------
 * Interpol used funmake iinnstead of apply
 * An updated version of COMA
 * A testbench for draw()
 * plot2d with a single function now uses xlabel
 * Many minor corrections
 * In package amatrix, protect against empty matrices when converting 
   amatrix to matrix and vice versa

Bug fixes:
----------
 * #3412: Corrected the translation of conditionals
 * #3587, +3489: Better handling of limits of logarithms
 * #3521: In simplify_sum, avoid double-counting -min_ni term in 
   to_hypergeometric
 * #3265: gcfactor(0) -> division by zero
 * #2839: gcfactor(9) => 9
 * #3287: Cross product of scalar zero versus vector yields zero result
 * #3583: Stack overflow for equality testing with assumptions
 * #3608: logic_simplify handles inequalities incorrectly 
 * #3607: printprops displays matchdeclare properties incorrectly when value is a lambda

Unnumbered bugs:
----------------
 * plot now works correctly with function names containing _, ^,
   {, } and @
 * Maxima now no longer becoms unusable with errset=t
 * FREE-LISP-VARS property for LET was broken
 * translator: improved error message when a throw is not in a catch
 * PATCH-UP-MEVAL-IN-FSET had a bogus BARFO call
 * *standard-input* was broking during gentranin runs
 * Problem loading abs_integrate in imaxima mode (emacs)
 * compgrind:true caused compfile to not write to the output file
 * compgrind:true didn't ensure pretty-printing of compfile output
 * Taylor expansion for gamma_incomplete_lower(1/2,z) now works
 * compgrind:true didn't ensure pretty-printing of compfile output
 * gamma_incomplete_lower now respects gamma_expand

Additional improvements:
------------------------
 * The documentation now is build use a stock makeinfo
 * Many other improvements to the way documentation is generated
 * The RPM files now end up in the build directory
 * A way to run many draw() and plot() examples for manual tests
 * More out-of-source-build improvements
 * "make dvi" was subject to bit-rot and should now work again
 * Resolved a few cases in which common variable names in 
   arguments caused unexpected behavior
 * Better ABCL integration
 * Added a help message (Option: /?) to the windows installer
 * Fix errors when installing on MacOS via Homebrew
 * run_testsuite(share_tests=only) now doesn't fail due to 
   unicode-sniffer.lisp not having been loaded
 * Draw now translates the coordinates of labels, too
 * Use DLMF 8.8.9 to expand gamma_incomplete(a+n,z)
 * Expand gamma_incomplete for rational order
 * Many code cleanups
 * Many improvements to the documentation
