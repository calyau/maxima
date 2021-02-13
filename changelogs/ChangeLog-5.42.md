Maxima 5.42 change log
======================

New items in core:
------------------
 * New function define_opproperty to define an operator simplification
 * The environment MAXIMA_INITIAL_FOLDER if set tells Maxima which
   folder to start in.
 * The variable $factor_max_degree that prevents certain polynomials
   from being factored if their degree exceeds that value instead of
   letting factorization cause an out-of-memory.
 * The variable factor_max_degree_print_warning that tells if to
   output an warning in this case.
 * A test bench for Maxima's interactive functions.

New items in share:
-------------------
 * A test bench for draw
 * timeout.lisp
 * new version of package share/contrib/Eulix
 * Trotter-Johnson algorithms
 * combinatorics.lisp 
 * A new README file for the share directory.

Changes in core:
----------------
 * "make check" now runs both the normal and the share test bench
 * "make check" now also runs the tests from rtest_ask.
 * test bench failures that resulted in "error-catch" now show a
   more descriptive error message
 * a warning() command that works similar to error()
 * Windows installer: support components (one can deselect optional
   components like VTK, wxMaxima or Gnuplot during the installation to
   save space) 
 * MAPATOM returns true for mapatom(-4)
 * A file with test cases for draw
 * A file with test cases for drawdf
 * A file with test cases for plot
 * errcatch() now produced meaningful error messages.
 * Add support for multiple return values to the ERRSET macro
 * scene() is dropped from plot() as it depends on Tcl/VTK which is no
   more actively supported and currently is broken.
 * gcl: concurrent Maxima processes now can negotiate which can use
   how much memory.
 * Emit warning from parser when a list is constructed and then
   discarded in ([a,b,c], ...).
 * Emacs > 26.1 compatibility.
 * Maxima now deletes its temp files on exit.
 * Automatic file encoding detection if the file begins in a BOM.
 * Improved the behavior of the exterior product operator when acting
   on ordinary products. 
 * Updated the Spanish translation
 * The functions that work with permutations, given as lists of
   consecutive integers from 1 to n, will now use the prefix "perm",
   to distinguish them from those that permute any arbitrary list or
   set, which use the prefix "permutation". The name of
   permutation_index was changed to perm_length and apply_permutation
   became permute. 
 * On Windows Maxima now autodetects the directory it finds its files
   in. 
 * Ask the user to use ; at the demo prompt, to make it work also in
   Xmaxima. 
 * Check for correct syntax of parametric plots.
 * Removed the old non-adaptive parametric plotter
 * Typecheck backtrace's argument
 * Better desktop integration for Linux systems
 
Changes in share:
--------------
 * Draw/MS Windows: Gnuplot now supports multiplot
 * Draw/MS Windows: Gnuplot now supports multiple draw windows
 * wrstcse: A simple package for tolerance calculations.
 * The test bench file's names now all begin with "rtest".
 * Marked many known bad tests as "bad" so changes that break
   something are easier to find.
 * engineering-format now allows to specify which number range doesn't
   need exponents.
 * Drawdf now no more translates its input which made it slightly
   faster, but sometimes caused internal errors. Setting
   drawdf_translate:true reverts it to the old behavior
 * The share testsuite now tests the draw package, too.
 * Draw: Allow vector coordinates to be transformed
 * Draw: Resolved a warning for multiplot svg output
 * Draw: Contour can now be set to "none" as documented in the
   manual. 
 * Draw: Support for vtk7. A "draw_renderer:vtk7" enables it.
 * Draw: Resizing the window now can change the aspect ratio of the
   plot. 
 * Draw: Allow to specify numeric line types
 * Draw: A finer default grid
 * Enable remove(x, rule) to succeed when x is a string.
 * vector.dem and qual.dem now can be loaded.
 * Xmaxima now accepts more of the standard options. 
 * Vtk (except of the plot routines) now is migrated to vtk-python
   which is actively supported in favour of the no-more working
   tcl-vtk
 
Bug fixes:
----------
 * 3459: Wrong limit calculation
 * 3458: addcol mishandles empty matrix as first argument
 * 3457: true[2] gives wrong error
 * 3426: Precision problem for small positive values in bfpsi0
 * 3410: extremal_subst gives bogus results with undefined predicate
 * 2822: After load("diff_form") `functions' fails
 * 3363: Documentation of modulus incorrectly says it applies to all
         rational numbers 
 * 3439: Simplode output on empty list
 * 3023: divide(1,0) gave a Lisp error instead of a Maxima one
 * 1581: sublis & sublis_apply_lambda:true causing Lisp errors
 * 2803: trace/timer do not work for functions with quoted arguments
 * 3247: Maxima's trace and timer handle rules incorrectly
 * 3368: integrate('limit(...),...) internal error
 * 2880: integral of secant shouldn't need principal value
 * 2116: lambda form for taylor_simplifier
 * 3431: error system variable holds unsimplified list, causing errors
         to be repeated when trying to access it
 * 3423: tellsimpafter: circular rule attempted when loading package
         vect twice 
 * 3337: Wrong scalefactor for cartesian2d
 * 2644: integrate(1/(1+s^7),s,0,%pi) includes a false term
 * 3413: false in definite integral of rational
 * 2845: Avoid initialization-time compile in commac.lisp
 * 3416: limit gives limit(x^r,x,inf)=>0 when "Is r pnz" is answered
         zero 
 * 2928, 2994 and 3419: Detect an endless loop in BPROG (simp.lisp)
         and throw rat-error. rat(1/(x^(2/3)+1)), algebraic and
         similar expressions no longer loop forever. 
 * 3009, 3146 and 3147: Certain polynomials when factored caused an
         out-of-memory 
 * 3422: li[2] and li[3] numerical evaluation fails for complex not in
         rectangular form 
 * 3402: Unbinding defined variable generates error in compiled
         functions 
 * 3406: pdf_geometric appears to be incorrect and/or poorly
         documented
 * 3403: Function/lambda parameters declared constant cause error
 * 2012: Lisp stack overflow with dpart
 * 3390: ?great mishandles box
 * 3387: kill(all) looses mtime's "nary" property
 * 1820: missing eigenvectors
 * 3373: Running the test suite twice causes errors
 * 3379: recur.mac correct bug in varc2
 * 3375: algsys failure on homogeneous linear equations
 * 3293: derivatives don't format correctly using alt-display TeX
 * 3238: plot fails with small x-range due to rounding
 * 2835: Parametric plotting failed if the variable was not t
 * 3356: sign(nz * nz) = nz
 * 3349: Using VTK on windows gives 'vtkpythonC:' error 
 * 3344: Conflict between sym package and grobner package 
 
Unnumbered bugs:
----------------
 * Windows installer: include 2 missing files in the with 'make dist'
   generated tar file. Allows building the crosscompiled installer
   from a released tar and solves a problem with SBCL and Lapack.
   (was missing in the 5.42.0 source release, discovered during
   building the Windows installer and already included in the
   5.42.0 installer)
 * Windows installer: Fixed the link to SBCL in the information about
   bundled software. (was missing in the 5.42.0 source release,
   discovered during building the Windows installer and already
   included in the 5.42.0 installer)
 * Windows installer: Fix the plotting problem with Clisp/Windows and
   long user names. 
 * Windows installer: Fix reading maximarc on Windows, when the
   username contains a space. 
 * Garbage Collector Defaults for gcl that allow Maxima to be able to
   run the testbench without getting an out-of-memory
 * Many error messages now are easier to understand
 * Some share tests contained timeouts that were set to values that
   were easily to trigger even on current computer systems
 * share/gf and share/contrib/fresnel can now be loaded without
   encountering an error.
 * simplode on a singleton list could return a non-string
 * If keepfloat was true partfrac often resulted in errors. Now
   keepfloat is false during partfrac().
 * 'make pdf' now works for de and pt.
 * A Lisp error appeared when display2d:true and there are nested
   DISPLA calls
 * tex(): The output for matrices is now upwards compatible to
   amsmath.sty
 * Fixed some shell quoting issues
 * Many translator improvements
 * Fix charfun when prederror is true
 * Quieted many irrelevant warnings on loading packages, for example
   caused by autoload.
 * Fixed a bug in perms, when called with three arguments
 * beta_incomplete now works correctly with sums.
 * Windows: Fixed reading maximarc on Windows, when the username
   contains a space.
 * Lapack now can be compiled out-of-the-box on SBCL on Windows (64Bit).
 * running rtest_levin after rtest15 causes some failures
   because MLIST is not recognized as an operator.
 * Process command line option --run-string correctly
 * changes to stream handling for SBCL, which improve
   writefile, appendfile, asksign, describe, and entermatrix.
 * trigsimp fails with pderivop
 * In $ARRAY, handle list argument + use_fast_arrays correctly.
 * Resolved many potential variable name clashes with user-specified
   variables
 * Xmaxima: Resolved an error message that caused a crash
 * Xmaxima: Removed Win98 workarounds that made problems with modern
   Windows systems 
 * Xmaxima now supports file and directory names with spaces in them.

Additional improvements:
------------------------
 * Additional bug fixes
 * Performance improvements
 * Much work on the manual including many hyperlinks, clarifications 
   and additional information on how to speed up user functions.
 * A script that tests loading packages twice or package combinations
 * A few files that didn't need an "executable" flag were marked
   as executable.
 * Updated all references to wxMaxima to wxMaxima's new URL.
 * The wxMaxima version the Windows Installer comes with now is 
   Version 18.10.2
 
