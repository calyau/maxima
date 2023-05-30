Maxima 5.47 change log
======================

New items in core:
------------------
 * function powerseries: power series for function lambert_w
   (thanks to Dimiter Prodanov)
 * --init option strips any directories and the last extension from the
   given value.  This value is used as the basename instead of
   "maxima-init" for the name of the init file.  This is an incompatible
   change.
 * --init-mac and --init-lisp are now (deprecated) aliases for --preload.
   This is an incompatible change.
 * The variable $maxima_frontend_bugreportinfo allows the frontend to
   tell bug_report() how to report frontend bugs.

New items in share:
-------------------
 * package pslq: PSLQ algorithm for finding integer relations
   among inexact numbers (thanks to Andrej Vodopivec)
 * package nelder_mead: Nelder-Mead algorithm for minimization without
   derivatives (thanks to Mario S. Mommer and Andrej Vodopivec)
 * package decfp: exact decimal floats (thanks to Richard J. Fateman)

Changes in core:
----------------
 * Maxima/ABCL: Use the option --noinform for ABCL.
   This suppresses the startup messages from ABCL, so that e.g. the Maxima
   option --quiet *is* quiet.
 * Fix command line options for Maxima with ABCL.
 * Functions that return lambda expressions with free variables are
   now translated correctly.  Previously many of these lambda expressions
   would cause internal errors when called.
 * Array functions and subscripted functions can now be translated.
   Previously these would fail to translate with no explanation.
 * configure: If the user doesn't specify a specific lisp maxima is compiled
   with all suitable lisp compilers.
 * --no-init now actually inhibits load if the user init files.
 * Init files that are loaded are now printed on startup
 * Add functions and variables for inspecting floating point properties
   * Variables:  `most_positive_float`, `most_negative_float`, `least_positive_float`, `least_positive_normalized_float`, `least_negative_float`
   * Functions: `float_eps`, `bitfloat_eps`, `float_precision`, `unit_in_last_place`, `is_power_of_two`, `scale_float`, `decode_float`, `integer_decode_float`
 * Simplify `elliptic_kc((sqrt(3)+2)/4)`
 * Fix bug in converting `inverse_jacobi_ds` to `elliptic_f`.
 * bfallroots with integer coefficients was not returning roots with the specified precision.
 * Add hypergeometric representations for `elliptic_ec`, `elliptic_kc`, `expintegral_chi`, `expintegral_ci`, `expintegral_shi`, `expintegral_si`, `expintegral_li`, `expintegral_ei`, `expintegral_e`
 * `hgfred` can simplify some 2F2 functions to `expintegral_ci`.
 * `hgfred` can simplify some 1F2 functions to `expintegral_si`.
 * Reject invalid values for `debugmode`.
 * `disp_time` variable removed because it was undocumented and didn't actually do anything.
 * Floats are displayed readably by default (fpprintprec = 0)
 * Function `texput`: punt to previously defined TeX output function
   when `false` is returned
 * Strings are comparable under "<", "<=", ">=", ">"
 * Function `plot2d`: new option `gnuplot_svg_background`

Changes in share:
-----------------
 * package contrib_ode: Fix name of Clairaut differential equation
   in code and documentation.  Was misspelled as Clairault.
 * package tensor: commit code from Toth & Turyshev paper for trace-free
   decomposition, for time and memory assessments
 * package namespaces: expunge this package, moved to Github
 * package ezunits: derive units for diff, integrate, and 'at'
 * function mnewton: optional argument for the Jacobian
 * function mnewton: revise mnewton for greater simplicity and speed
 * Document `harmonic_number` and `harmonic_to_psi` functions from `simplify_sum`.
 * package draw: allow points(...) to contain nonnumeric (including non-finite floats) data
 * package draw: honor fill_density in errors, polygon, ellipse, bars, explicit, and region
 * package descriptive: implementation of skyline (outline) histogram
 * package descriptive: optional weight argument recognized by mean, var, std, and other functions
 * package distrib: maximum likelihood estimators (`mle_something`) for several distributions
 * package graphs: new functions `get_unique_vertex_by_label` and `get_all_vertices_by_label`
 

Changes in Xmaxima:
-------------------

Changes in the Windows installer:
---------------------------------
 * Update Gnuplot, SBCL, wxWidgets and TCL/TK.
 * Compile a recent texinfo for building the installer. Maxima now needs
   texinfo 6.8, but 6.7 is included in the usual build machines for
   nightly builds, etc.

Bug fixes for numbered bugs:
----------------------------
 * \#545: multivar taylor gives 1^2
 * \#608: taylor(x^a,[x],0,1) unsimplified
 * \#1743: limit of trig expression
 * \#3026: missing info files not well-handled
 * \#3071: limit of expressions with signum not very powerful
 * \#3136: gruntz(atan2(x^2-2,x^3-2*x),x,sqrt(2),minus) => atan2(0,0) undefined 
 * \#3279: limit incorrect with domain:complex
 * \#3280: gruntz incorrect limit
 * \#3415: limit doesn't check for zero coefficients in limit((a*x+1)/(a*x+2),x,inf)
 * \#3592: Wrong limit
 * \#3631: gen_laguerre returns 0 to a negative exponent
 * \#3834: abconvtest undocumented and possibly unused
 * \#3848: ratsubst error ZEROP: ((MMINUS) 1) is not a number
 * \#3926: Various limits give UND where they should give IND
 * \#3953 Pressing q necessary to continue when plot2d output to svg
 * \#3956 expand(1/((sqrt(2)-1)*(sqrt(2)+1))) => 1/1 (unsimplified)
 * \#3963 trace doesn't detect calls to functions defined by DEFMFUN in Lisp code
 * \#3958 plot2d with multiple discrete plots fails
 * \#3959 plot2d + Gnuplot 4 with `plot title noenhanced`
 * \#3965 maxima --list-avail fails with GCL, but works with ECL and SBCL
 * \#3966 li[s](1) should be zeta(s) (with conditions on s)
 * \#3967 elliptic_e(5*%pi/4,1) inconsistent with numerical evaluation
 * \#3968 zeta(-r) should use analytic continuation?
 * \#3970 draw does not do adequate argument checking
 * \#3972: gcl only: Autocompletion inverts command case
 * \#3982 tex complains about Unicode character in symbol
 * \#3984 limit for und + something yields something instead of und
 * \#3985: integrate(sin(x*cos(x)), x), risch; causes unlimited recursion
 * \#3992 Add word to Integration.texi
 * \#3996: parse_string fails to parse string which contains semicolon
 * \#3998: Lisp error when parser encounters eof in a comment
 * \#4008: translator and prederror
 * \#4018: defint(foo,,0,inf) lisp error when denom(foo) contains %i
 * \#4029 limit(cos(1/x)^2 + sin(1/x)^2 + cos(x),x,0) --> ind
 * \#4035 Invisible characters should work better
 * \#4036: prederror affects bigfloat calculations
 * \#4043: bug in to_poly
 * \#4045 Different results for integration in Maxima 5.45.1 and 5.46.0
 * \#4046 plot2d legend option shows the wrong label in the list 
 * \#4048 An incorrect limit
 * \#4050: Maxima can't handle limits of Fibonacci expressions
 * \#4056 Cannot create very large data array
 * \#4060 Wrong limit of -3*li[3](-%e^x)+3*x*li[2](-%e^x)+x^3 at x -> inf
 * \#4061 2.0^1024 gives "false" on second try
 * \#4062 limit(li[3](x), x, inf) gives li[3](inf)
 * \#4064 Simple limit triggers Lisp error "1 is not of type LIST"
 * \#4077 Replace opcons with ftake*
 * \#4084 Error in [lin]solve with orderless called before
 * \#4088: maxima variable prefer_d undocumented
 * \#4089 Specvar ans in schatc
 * \#4090 output formatting: -x-1 has redundant parentheses: (-x)-1
 * \#4092 Repeated factorial (n!)! displayed the same as double factorial n!!
 * \#4094 Build warning in share: grep: warning: stray \ before #
 * \#4029 limit(cos(1/x)^2 + sin(1/x)^2 + cos(x),x,0) --> ind
 * \#4097 Bad string representation of integers in the first argument of `save`
 * \#4100 On macOS, building Maxima has some issues
 * \#4107 least_positive_float doesn't print/read correctly in float/bfloat
 * \#4109 Limits of polylogarithms
 * \#4112 Double superscript in tex(conjugate(z))^2
 * \#4118 facsum + operator(".") + dotdistrib:true + GCL = FAIL
 * \#4119 ECL fails decode_float(-0.25) test
 * \#4120 is("foo"<3) gives internal error

Unnumbered bugs fixed:
---------------------
 * commit 7a7114c: avoid call to undefined function expintegral_ei
 * commit fe1d8cf: avoid clobbering global state when autoloading
 * commit 70a5f78: expunge calls to 'sign' in share package distrib
 * commit 7bc968a: in printf, be more consistent about non-numeric argument for ~e, ~f, and ~g

Documentation:
--------------
 * Help can use html docs instead of displaying help to the terminal.
   This is enabled by setting output_format_for_help to 'html; it defaults
   to 'text.  Use 'frontend to display help using Xmaxima, wxMaxima or
   other frontends.
 * Add documentation for trigtools to the manual.
 * Some reordering of the trig functions in the manual.
 * Some renaming of sections in the manual to be more explicit.
 * Add more cross references.
 * Add more examples for `erf_representation`, `hypergeometric_representation`, `expintrep`, `expintexpand`, `besselexpand`
 * Add examples for the ggf package.

Build system:
-------------

