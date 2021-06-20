Maxima 5.45 change log
======================

New items in core:
------------------
 * A ~m (Maxima pretty printer) format directive for printf
 * New methods contour and implicit function for plot2d
 * New file geomview_def.lisp. Code to parse plots for Geomview
   moved from plot.lisp to that file
 * New code to commute conjugate and derivative
 * New tests for rtestconjugate.mac

New items in share:
-------------------
 * hompack package with interface to polsys
 * FFTPACK5: Fast Fourier Transform package
 * pwilt: Piece-Wise Inverse Laplace Transform improves ilt

Changes in core:
----------------
 * Fix and extend the definition of mapatom, see maxima-discuss thread
   "Non-mapatomic atoms".
 * Improved num_partitions from O(n^2) to O(n^1.5)
 * Give signum, unit_step, log_gamma and mod an integral property
 * Express anti derivative of mod in terms of mod, not floor
 * Add grad property to binomial
 * Give %gamma a sign-function property
 * No float-to-bigfloat conversion warning if no precision is lost
 * Change ?gensym to gensym in linearalgebra.mac
 * Make polynomialp a built-in function
 * atan2 supports arbitrary precision input
 * Reset changed some bigfloat variables to decimal fractions
 * Fix incorrect values of zeta(%i) and zeta(%i-2)
 * Resolved an error when using bigfloat exponents
 * Correct the hypergeometric representation of erf, erfc and erfi
 * Add hypergeometric representation of erf_generalized
 * Plot object for plotting programs with methods for Gnuplot and Xmaxima
 * More options added to the plotting programs
 * Option adapt_depth can be 0, to turn off adaptive plotting
 * Extend specint(exp(-s*t)*t^z,t) and laplace(t^z,t,s) to complex z
 * In TeX output, put csc, sec, and cot onto list of trig-like functions
   for TEX-MEXPT
 * Improved LispWorks, CLISP and SBCL compatibility
 * Some improvements for GCL
 * Many fixes for the translator
 * Many display-related fixes
 * Many code cleanups
 * Many error messages now are easier-to-understand

Changes in share:
-----------------
 * Remove bogus gradef for signum in abs_integrate.mac
 * share/contrib/wrstcse: A better way to generate random numbers between -1
   and 1
 * fft did result in the conjugate of the actual result
 * draw: don't cleanup files on close that were assigned a name by the user
 * Non-ascii filename support for plotting, operatingsystem and numericalio
   on sbcl+MSW
 * fmin_cobyla now sets ierr = -1 if constraints might not be satisfied
 * linearalgebra: fix for some singular matrices
 * dynamics: julia and mandelbrot updated for the new version of plot.lisp
 * symplifying: disallow illegal functions in simpfunmake

Changes in Xmaxima:
-------------------
 * Tcl 8.6 is now required (themed widgets and rotated strings).
 * Plots with a better look and resizeable. Size change and replot
   buttons have been removed.
 * History saved in a local file to be reused in later sessions
 * Configuration file saved in home directory in Windows
 * List of browsers to search updated
 * Code clean-up

Changes in the Windows installer:
---------------------------------
 * Admin permissions are now required (as for most Windows
   programs).  Installation without admin permissions worked, but
   left some spurious registry entries after uninstallation. This
   should be fixed now.
 * Windows Installer: Updated ABCL, TCL, wxWidgets and wxMaxima.
 * The start menu entry is now named "Maxima computer algebra system"
 * Build 64bit by default. Add instructions for compiler configuration
 * Mingw crosscompiler configured to use Posix threads
 * Require CMake 3.10

Bug fixes for numbered bugs:
----------------------------
 * #2976: "local" doesn't work in translated code
 * #3154: lratsubst NOT as described in Help file
 * #3376: lratsusbst causes bind stack overflow on large lists
 * #3417: sign(1/zero) => 0 (where equal(zero,0))
 * #3434: kill(ratvars) should give an error
 * #3543: bug with polynomialp
 * #3549: removing mmref.tex and maxima_pdf.texi from info directory
 * #3562: integrate(1/(1+tan(x)), x, 0, %pi/2) gives complex result, should be
   %pi/4
 * #3576: odelin wrong answer on simplest ode
 * #3642: Lisp error when translating assume
 * #3643: "DEFMFUN creates $NAME and $NAME-IMPL, contradicting documentation"
 * #3648: plot output depends on *read-default-float-format*
 * #3650: We trusted gnuplot's datafile separator to be unmodified
 * #3658: error with floats appearing instead of integers in lratsubst
 * #3680: limit(x/sin(1/x),x,0) wrong
 * #3681: limit(n^n/(n^n+(n-1)^n),n,inf) wrong
 * #3682: limit(n^(n-1)/(n^n+(n-1)^n),n,inf) gives Lisp error
 * #3671, 3674, 3675: imaxima-related bugs
 * #3685: descriptive/continuous_freq causes Lisp error given rational numbers
 * #3700: stringproc/Mysterious error in ssubst
 * #3704: Translator gives internal error
 * #3706: lratsusbst causes bind stack overflow on large lists
 * #3710: plot3d fails with [grid,300,300]
 * #3714: Update maxima to use vtk-8.2.0
 * #3715: draw3d with variables x[1], x[2] not working
 * #3720: conjugate of und, ind, zerob, and zeroa
 * #3722: missing simp flags in subexpressions conjugate nounform
 * #3723: conjugate of summations (check if summation range is real)
 * #3728: missing simp flag from set_partitions
 * #3732: plot_format gnuplot_pipes creates file in addition to a pipe
 * #3733: $gamma vs %gamma confusion
 * #3734: $hypergeometric_u vs %hypergeometric_u confusion
 * #3736: Quoting either min or max inhibits simplification
 * #3738: xlabel, ylabel in set_plot_option ignored
 * #3745: Quoting either elliptic_f inhibits simplification
 * #3746: derivative of inverse_jacobi_sn is noun/verb confused
 * #3749: Calling demo() causes crash in Maxima 5.41.0 and 5.44.0 (GCL)
 * #3751: implicit_plot option handling for style and legend is out of date
 * #3752: missing simp flag from permutations
 * #3753: powerset has missing simp flag
 * #3760: apropos("") errors
 * #3768: imaxima causes an error
 * #3774: Horizontal parts of plots at ymin/ymax are invisible
 * #3777: rat fails on denormalized floats
 * #30520: (from Sage bug tracker) Error in the sign of a product
 * #31557: (from Sage bug tracker) Fix product(-x, x, 1, SR.var('n')) to
   return (-1)^n*factorial(n)
 * #1501: (from Wxmaxima bug tacker) Wrong description of plotting options

Unnumbered bugs fixed:
---------------------
 * The noun and verb forms of kron_delta were backwards
 * Bug fix: gcfactor(x*%i) => lisp error
 * Several fixes to imaxima
 * translator: several bugs fixed and obsolete Maclisp code removed
 * Taylor series of signum
 * Maxima asking for the sign of 'und'
 * Series for erf/fresnel_c/fresnel_s using wrong var
 * rat, algebraic, BPROG, was: converting to float
 * Remove calls to $ratsimp and $rectform in res1m, since it broke some
   definite integrals
 * Prevent the symbol 'xz from leaking out of the "ptan" function
 * Option z for plot3d now works in Xmaxima format

Documentation:
--------------
 * Plotting section updated with the new methods of plot2d (implicit
   functions and contours), new options and removing obsolete programs
   contour_plot and implicit_plot
 * Documentation for the VTK interface of the draw package
 * Documentation for hstep and pwilt (Piece-Wise Inverse Laplace Transform)
 * lratsusbst and fullratsubst documentation updated
 * ieqn documentation improved
 * refer load_pathname in documentation of load
 * Some characters in the PDF documentation (e.g. "_", "|") were displayed wrong
 * Spelling errors corrected
 * Texinfo version updated and a switch added to turn off page numbers in
   references
 * Improved appearance for the PDF version of the manual
 * Changelogs for older releases moved to a subdirectory

