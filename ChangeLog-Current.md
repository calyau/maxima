Maxima next change log
======================

New items in core:
------------------
 * nonnegative_lp now replaces nonegative_lp which now is merely an alias.
 * The environment variable MAXIMA_DOC_PREFIX that allows to override the
   location the documentation is searched for.

New items in share:
-------------------
 * Many improvements to the simplex algorithm incluing the ability to handle
   symbolic inputs

Changes in core:
----------------
 * to_cl, a function that outputs the lisp code a maxima function would be 
   compiled to now is autoloadedx.
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
Changes in share:
--------------
 * 
 
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

Additional improvements:
------------------------
 * Updated the external utilities for the Windows Installer
 * The wxMaxima version the Windows Installer comes with now is 
   Version 18.10.2
 * Nightly Test: A summary of the share tests
 * Crosscompiling: Add 'maxima_longnames.c' to automake
