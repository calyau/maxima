Maxima 5.42 change log
======================

New items in core:
------------------
 * New function define_opproperty to define an operator simplification
 * The environment MAXIMA_INITIAL_FOLDER if set tells maxima which folder
   to start in.
 * The variable $factor_max_degree (+ $factor_max_degree_print_warning) 
   that prevents certain polynomials from being factored if their degree 
   exceeds that value instead of letting factorization cause an out-of-memory. 
New items in share:
-------------------
 * A test bench for draw
 * timeout.lisp
 
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
 * A file with test cases for draw
 * A file with test cases for drawdf
 * A file with test cases for plot
 * errcatch() now produced meaningful error messages.
 * Add support for multiple return values to the ERRSET macro
 * scene() is dropped from plot() as it depends on tcl/vtk which is no more 
   actively supported and currently is broken.
 * gcl: concurrent maxima processes now can negotiate which can use how much
   memory.
 * Emit warning from parser when a list is constructed and then discarded in
   ([a,b,c], ...).
 * emacs > 26.1 compatibility.
 * Maxima now deletes its temp files on exit.
 * Automatic file encoding detection if the file begins in a BOM.
 
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
 * Drawdf now no more translates its input which made it slightly faster, but
   sometimes caused internal errors. Setting drawdf_translate:true reverts it
   to the old behavior
 * The share testsuite now tests the draw package, too.
 * Draw: Allow vector coordinates to be transformed
 * Draw: Resolved a warning for multiplot svg output
 * Draw: Contour can now be set to "none" as documented in the manual.
 * Draw: Support for vtk7. A "draw_renderer:vtk7" enables it.
 * Draw: Resizing the window now can change the aspect ratio of the plot.
 * Draw: Allow to specify numeric line types
 * Draw: A finer default grid
 * Enable remove(x, rule) to succeed when x is a string.
 * vector.dem now can be loaded.
 
  
Bug fixes:
----------
 * #3459 Wrong limit calculation
 * #3458 addcol
 * #3457 true[2] gives wrong error
 * #3426 Precision problem for small positive values in bfpsi0
 * #3410 extremal_subst gives bogus results with undefined predicate
 * #2822: After load("diff_form") `functions' fails
 * #3363: Documentation of modulus incorrectly says it applies to all rational numbers
 * #3439: Simplode output on empty list
 * #3023: divide(1,0) gave a Lisp error instead of a maxima one
 * #1581: sublis & sublis_apply_lambda:true causing lisp errors
 * #2803: trace/timer do not work for functions with quoted arguments
 * #3247: Maxima's trace and timer handle rules incorrectly
 * #3368: integrate('limit(...),...) internal error
 * #2880: integral of secant shouldn't need principal value
 * #2116: lambda form for taylor_simplifier
 * #3431: error system variable holds unsimplified list, causing errors 
          to be repeated when trying to access it
 * #3423: tellsimpafter: circular rule attempted when loading package vect twice
 * #3337: Wrong scalefactor for cartesian2d
 * #2644: integrate(1/(1+s^7),s,0,%pi) includes a false term
 * #3413: false in definite integral of rational
 * #2845: Avoid initialization-time compile in commac.lisp
 * #3416: limit gives limit(x^r,x,inf)=>0 when "Is r pnz" is answered zero
 * #2928, #2994 and #3419: Detect an endless loop in BPROG (simp.lisp) and throw 
          rat-error. rat(1/(x^(2/3)+1)), algebraic and similar expressions no 
		  longer loop forever. 
 * #3009, #3146 and #3147: Certain polynomials when factored caused an out-of-memory
 * #3422: li[2] and li[3] numerical evaluation fails for complex not in rectangular form
 * #3402: Unbinding defined variable generates error in compiled functions
 * #3406: pdf_geometric appears to be incorrect and/or poorly documented 
 
Unnumbered bugs:
----------------
 * Windows installer: Fix the plotting problem with Clisp/Windows and long user names.
 * Windows installer: Fix reading maximarc on Windows, when the username contains a space.
 * Garbage Collector Defaults for gcl that allow maxima to be able to run the testbench
   without getting an out-of-memory
 * Many error messages now are easier to understand
 * Some share tests contained timeouts that were set to values that were easily to trigger
   even on current computer systems
 * share/gf and share/contrib/fresnel can now be loaded without encountering an error.
 * simplode on a singleton list could return a non-string
 * If keepfloat was true partfrac often resulted in errors. Now keepfloat is false during
   partfrac().
 * 'make pdf' now works for de and pt.
 * A lisp error appeared when display2d:true and there are nested DISPLA calls
 * tex(): The output for matrices is now upwards compatible to amsmath.sty
 * A script that tests loading packages twice or package combinations
 * Fixed some shell quoting issues
 * Many translator improvements
 * Fix charfun when prederror is true
