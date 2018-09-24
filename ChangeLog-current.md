Maxima 5.42 change log
======================

New items in core:
------------------

New items in share:
-------------------
 * A test bench for draw

Changes in core:
----------------
 * "make check" now runs both the normal and the share test bench
 * "make check" now also runs the tests from rtest_ask.
 * test bench failures that resulted in "error-catch" now show a
   more descriptive error message
 * a warning() command that works similar to error()
 * Windows installer: support components (one can deselect optional components
   like VTK, wxMaxima or Gnuplot during the installation to save space)
 * MAPATOM returns true for mapatom(-4)
 
Changes in share:
--------------
 * Draw/MS Windows: Gnuplot now supports multiplot
 * Draw/MS Windows: Gnuplot now supports multiple draw windows
 * wrstcse: A simple package for tolerance calculations.
 * The test bench file's names now all begin with "rtest".
 * Marked many known bad tests as "bad" so changes that break something
   are easier to find.
 * engineering-format now allows to specify which number range doesn't
   need exponents.

Bug fixes:
----------
 * #3459 Wrong limit calculation
 * #3458 addcol
 * #3457 true[2] gives wrong error
 * #3426 Precision problem for small positive values in bfpsi0
 * #3410 extremal_subst gives bogus results with undefined predicate

Unnumbered bugs:
----------------
 * Windows installer: Fix the plotting problem with Clisp/Windows and long user names.
 * Windows installer: Fix reading maximarc on Windows, when the username contains a space.
