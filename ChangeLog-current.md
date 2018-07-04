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

Unnumbered bugs:
----------------
