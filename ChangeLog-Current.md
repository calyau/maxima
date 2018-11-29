Maxima next change log
======================

New items in core:
------------------
 * nonnegative_lp now replaces nonegative_lp which now is merely an alias.
 * The environment variable MAXIMA_DOC_PREFIX that allows to override the
   location the documentation is searched for.

New items in share:
-------------------
 * Many improvements to the simplex algorithm including the ability to handle
   symbolic inputs
 * The ode package now has a testbench.

Changes in core:
----------------
 * Many strings in error messages weren't properly formatted
 * The error message on calling a command with the wrong number of arguments
   now in most cases tells which command the error was raised by.
   error message that didn't tell which command the error was caused by.
 * to_cl, a function that outputs the lisp code a maxima function would be 
   compiled to now is autoloaded.
 * A few functions from the linear algebra package weren't autoloaded.
 * Removed the unused variable "variables_set" that wasn't assigned a meaningful
   value to by the build system anyway.
 * out-of-tree-builds now should work if maxima isn't compiled using ECL 
   (see src/maxima.system for details about the reason)
 * Part of the build process for the documentation is now off-loaded to 
   doc/info/build_html.sh 
 * The build system now uses /usr/bin/perl instead of /usr/local/bin/perl
 * Tweaked the memory management for GCL towards big maxima projects by default
 * Compatibility for newer autotools.
 * configure now has an --enable-abcl switch.
 * Many improvements for gentran, a translator from maxima to efficient
   c, ratfor or fortran code.
 * Maxima.bat now autodetects 64-bit systems and does the necessary
   modifications for making lapack work in SBCL it this test is positive.
 * perl is now uniformly called as /usr/bin/env perl
 * The Windows installer can now include ABCL
 * Corrected many markers that tell if an example can be updated using the 
   doc/info/update_example scripts.

Changes in share:
--------------
 *  The the arguments to Krylov matrix are now checked for being of the 
    correct type.
 *  ODEPAK now works in conjunction with ECL.
 *  LAPACK now works in conjunction with ECL, too.
 *  Sarag: Fix the missing definition of SQUARE_FREE_ALGORITHM used in the 
    certificate code.. It adds explicit details while proving the sign of 
	a polynomial in a given interval
 *  Sarag: Make certificate proof more explicit (verify certificate)
 *  Added the existing lapack checks to the share testsuite for all lisps
    except ECL (in ECL a try to use lapack results in an error message)
	and SBCL (receives an out-of-memory, depending on the system).
 *  A primitive testsuite for ODEPACK that tests if it works at all.

Bug fixes:
----------
 * #3470: 
     -Replace the option variable nonegative_lp with nonnegative_lp; the
      former is retained as an alias.
    -Correct spelling of non-negative in documentation.
    -Document the undocumented optional input <all> to min/maximize_lp.
 * #3463: mention epsilon_lp more explicitly in minimize_lp
 
Unnumbered bugs:
----------------
 * ezUnits: microS is microSiemens, not microSievert.
 * radcan(sqrt(-1/(sqrt(-3)+1))*sqrt(2)) runs out-of-memory
 
Additional improvements:
------------------------
 * Updated the external utilities for the Windows installer
 * The wxMaxima version the Windows installer comes with now is
   Version 18.11.3
 * Nightly Test: A summary of the share tests
 * Crosscompiling: Add 'maxima_longnames.c' to automake
 * Documentation Updates

