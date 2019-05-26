Maxima next change log
======================

New items in core:
------------------
 * nonnegative_lp now replaces nonegative_lp which now is merely an alias.
 * The environment variable MAXIMA_DOC_PREFIX that allows to override the
   location the documentation is searched for.
 * The environment variable GCL_DISABLE_MULTIPROCESS_MEMORY_POOL disables
   GCL's ability to share the system memory between gcl-compiled maxima
   processes.
 * The lisp function defun-maclisp, a function that allows to define 
   non-user functions using the maclisp nargs syntax.
 * Documentation: ./update_examples now interprets lines beginning with 
   the string "input:" as text that should be sent as input to the 
   preceding command.
 * garbage_collect() which manually triggers the lisp's garbage collector 
   or returns false.
 * newdet: An determinant algorithm that is optimized on sparse matrices
 * maxima_frontend and maxima_frontend_version: Two variables that can be
   set in order to inform build_info() which frontend is in use.
 * cartesian_product_list makes cartesian products between lists.

New items in share:
-------------------
 * Many improvements to the simplex algorithm including the ability to handle
   symbolic inputs
 * The ode package now has a testbench.
 * killing.dem, a demo for Killing vector fields.
 * The logic share package now provides logic_simplify that uses the
   K-Map reduction method by Quine-Mccluskey, see merge-request #10.

Changes in core:
----------------
 * Many strings in error messages now are properly formatted
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
 * Resolved the "simplifya: I don't know how to simplify this operator: ~M"
   error triggered by some array actions.
 * The Windows installer can now include ABCL.
 * Corrected many markers that tell if an example can be updated using the 
   doc/info/update_example scripts.
 * user-visible functions are now defined using defmfun, only internally-used
   ones by defun.
 * The testsuite summary now not only which tells unexpectedly failed, but also
   which ones unexpectedly passed.
 * The maxima-to-lisp translator now handles the case properly that a list
   is assigned to a global variable.
 * ./configure: Tried to make the summary nicer.
 * maxima.asd should mow match maxima.system again.
 * The lists of test suite files and share test suite files now are maxima lists
   and therefore can be manipulated without having to resolve to lisp commands.
 * The lists of core and share test suite files (testsuite_files and 
   share_testsuite_files) are now available directly after starting maxima and 
   not only after using or manually loading the test suite.
 * "make check" manipulates the list of tests now in a more canonical way in order
   to automatically run the interactive tests, as well.
 * "make check" now compiles maxima before running the tests, if maxima still 
   needs compiling.
 * ./configure --enable-quiet-build now muffles more warnings.
 * src/shares_subdirs.lisp is now no more generated directly but using an 
   immediate file which hinders "make distclean" from automatically deleting it.
 * documented the clisp bug that if a frontend acknowledges a network packet 
   while maxima is still preparing the next one the result might be garbled
   data.
 * Small improvements for ./configure's --enable-quiet-build switch
 * The one-file-per-chapter version of the html manual now starts in 
   maxima-1.html, not in maxima.html
 * The documentation now is build using the standard makeinfo tools
 * doc/info/update_examples.sh can now be used for more examples of the 
   documentation including interactive ones.
 * Many documentation updates including many hyperrefs and more and better 
   examples.
 * add series expansion for expintegral_si and gamma_incomplete. This
   unfortunately breaks the following integrals found in the test suite:
     integrate(log(cot(x) - 1), x, 0, %pi/4);           /* rtest16  524 */
     integrate(log(cos(x)), x, 0, %pi/2);               /* rtest16  525 */
     integrate((log((2-x)/2)+log(2))/(1-x), x, 0, 1);   /* rtestint 232 */
 * float_approx_equal now does what the comments in the code say.
 * an ecl-based maxima no more enters an endless loop if the front-end dies.
 * maxima-sbcl now supports non-ascii user names and install dirs on MacOs 
   and MS Windows
 * translate() now temporarily sets prederror to true which produces an
   error message if the translator fails to translate a nested if() 
   correctly, see Bug #3412.
 * A workaround for a bug in GCL that makes $load_pathname work again
 * "make clean" now removes the temporary files for lisps that aren't
   currently configured.
 * "make check" now runs the share and the core testsuite in the same
   maxima session.
 * Fix the error message for an invalid upper limit of integration
   If the upper limit of integration was invalid, the error message
   incorrectly printed the lower limit instead.
 * The feature "nonscalar" was named "nonscalarp" in some places.
   Now it is named "nonscalar" consistently. 
 * Arraymake should now output an error if the result isn't a valid array
 * interval(a,b) created an interval, but maxima lacked functions to do
   anything with these intervals => this undocumented function was 
   commented out.
 * MRELATIONP now knows how to deal with the "not equal" operator.
 * shipped around a clisp bug that caused broken output with fast front-ends
 * '[x][1]; now is output as a list, too.
 * The last output label of a batch file is no more clobbered with the file
   name.
 * emaxima now correctly expands tabs in emacs' comint buffer.
 * "make install" now installs emaxima and imaxima in a place emacs will 
   find by default. configure's --emacs-prefix= option allows to choose
   a different directory.
 * Code clean up.
 
Changes in share:
--------------
 * The the arguments to Krylov matrix are now checked for being of the 
   correct type.
 * ODEPAK now works in conjunction with ECL.
 * LAPACK now works in conjunction with ECL, too.
 * Sarag: Fix the missing definition of SQUARE_FREE_ALGORITHM used in the 
   certificate code.. It adds explicit details while proving the sign of 
   a polynomial in a given interval
 * Sarag: Make certificate proof more explicit (verify certificate)
 * Added the existing lapack checks to the share testsuite for all lisps
   except ECL (in ECL a try to use lapack results in an error message)
   and SBCL (receives an out-of-memory, depending on the system).
 * A primitive testsuite for ODEPACK that tests if it works at all.
 * ezunits: A conversion rule for nondimensional/(sum of dimensional)
 * ezunits: 0 now can have a dimension.
 * ezunits test script: Avoid kill(m) which kills the unit meter.
 * The tests for the "sym" package are now part of the share testsuite.
 * The tests for the "ode" package are now part of the share testsuite, too
   but have been disabled as they failed.
 * Sarag no more overwrites the function resultant() which means the 
   share test suite no more kills this function in a kill(all);
 * Draw now accepts numbers as color specification besides the html-style
   and the gnuplot-style color identifiers it already understood.
 * Draw now accepts plot titles even if they aren't strings.
 * wrstcse now gets the ranges and resolution of random numbers for monte
   carlo analysis right.

Bug fixes:
----------
 * #3470: 
    -Replace the option variable nonegative_lp with nonnegative_lp; the
      former is retained as an alias.
    -Correct spelling of non-negative in documentation.
    -Document the undocumented optional input <all> to min/maximize_lp.
 * #3463: mention epsilon_lp more explicitly in minimize_lp
 * #3497: Resolved an error in demos concerning the Ricci tensor
 * #3497: ctensor documentation on the Ricci tensor is inconsistent
 * #3532: Wrong substitution on integrating cos(x)*abs(cos(x))
 
Unnumbered bugs:
----------------
 * ezUnits: microS is microSiemens, not microSievert.
 * radcan(sqrt(-1/(sqrt(-3)+1))*sqrt(2)) runs out-of-memory
 
Additional improvements:
------------------------
 * Updated the external utilities for the Windows installer
 * Updated the wxMaxima version the Windows installer comes with
 * Nightly Test: A summary of the share tests
 * Crosscompiling: Add 'maxima_longnames.c' to automake
 * Documentation Updates

