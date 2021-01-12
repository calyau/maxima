New items in core:
------------------
 * A ~m (Maxima pretty printer) format directive for printf.

New items in share:
-------------------
 *  A hompack package with interface to polsys
 *  FFTPACK5

Changes in core:
----------------
 * Fix and extend the definition of mapatom, see maxima-discuss thread "Non-mapatomic atoms".
 * Many fixes for the translator
 * Many display-related fixes
 * Compatibility to SBCL-1.4.11 and up
 * Many error messages now are easier-to-understand
 * Many code cleanups
 * Improved num_partitions from O(n^2) to O(n^1.5)
 * give signum, unit_step, log_gamma and mod an integral property
 * Express anti derivative of mod in terms of mod, not floor
 * add grad property to binomial
 * the noun and verb forms of kron_delta were backwards
 * give %gamma a sign-function property
 * No float-to-bigfloat conversion warning if no precision is lost
 * Change ?gensym to gensym in linearalgebra.mac
 * Make polynomialp a built-in function
 * atan2 supports arbitrary precision input
 * Reset changed some bigfloat variables to decimal fractions
 * Better LispWorks compatibility
 * Better clisp compatibility
 * Fix incorrect values of zeta(%i) and zeta(%i-2)
 * Resolved an error when using bigfloat exponents
 * Bug fix: gcfactor(x*%i) => lisp error

Changes in share:
-----------------
 * remove bogus gradef for signum in abs_integrate.mac
 * share/contrib/wrstcse: A better way to generate random numbers between -1 and 1
 * fft did result in the conjugate of the actual result
 * draw: don't cleanup files on close that were assigned a name by the user
 * Correct the hypergeometric representation of erf, erfc and erfi
 * Add hypergeometric representation of erf_generalized
 * Non-ascii filename support for plotting, operatingsystem and numericalio on sbcl+MSW
 * fmin_coblya now sets ierr = -1 if constraints might not be satisfied

Changes in Xmaxima:
-------------------
 * Tcl 8.5 compatibility

Changes in the Windows installer:
---------------------------------
 * Admin permissions are now required (as for most Windows programs).
   Installation without admin permissions worked, but left some
   spurious registry entries after uninstallation. This should be fixed now.
 * Windows Installer: Updated ABCL, TCL, wxWidgets and wxMaxima.
 * The start menu entry is now named "Maxima computer algebra system"

Bug fixes for numbered bugs:
----------------------------
 * #3417: sign(1/zero) => 0 (where equal(zero,0))
 * #3650: We trusted gnuplot's datafile separator to be unmodified
 * #3643: "DEFMFUN creates $NAME and $NAME-IMPL, contradicting documentation"
 * #3671, 3674, 3675: imaxima-related bugs
 * #3685: descriptive/"continuous_freq causes Lisp error given rational numbers"
 * #3658: "error with floats appearing instead of integers in lratsubst"
 * #3700: stringproc/"Mysterious error in ssubst"
