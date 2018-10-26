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
 * Made out-of-tree-builds work except for ECL (see src/maxima.system for 
   details about the ECL bug)
 * The build system now uses /usr/bin/perl instead of /usr/local/bin/perl
 * Tweaked the memory management for GCL towards big maxima projects by default
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
