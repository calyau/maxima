Maxima 5.46 change log
===========================

New items in core:
------------------
 * Added eval_string_lisp to read in and evaluate lisp forms from a string
 * Maxima compiled with ABCL can use network connections (option "-s"),
   which is used by frontends like Xmaxima and wxMaxima.
 * Added gnuplot_send to open a Gnuplot pipe and send a command to it.
 * new code to commute conjugate of derivative (Commit [565fcf]).
 * new plot option window. It can be used to send the plot to a window
   different from the default 0 window.

New items in share:
-------------------
 * new package quantum_computing: a simulator of quantum computing
   circuits.
 * new package test_batch_encodings: tests for batch and batchload with different character encodings
 * new file conics_04.mac: intersections of conic sections
 * package distrib: implementation of inverse gamma distribution
 * package stringproc: extend the range of recognized encodings somewhat
 * package numericalio: extend function md5sum to accept binary input stream
 * package numericalio: recognize 1-element strings as equivalent to symbols for separators
 * package gentran: update to new version contributed by Michael Stern

Changes in core:
----------------
 * plot2d, plot3d, mandelbrot and julia now use gnuplot_pipes by default
   in all operating systems and in all cases (plot shown on the screen or
   saved to a graphics file). No temporary files are used.
 * option gnuplot_strings made more consistent in different terminals.
 * option gnuplot_script_file now accepts complete path file names.

Changes in share:
-----------------
 * draw: Restored the default gnuplot_pipes format (Commit [a54f2f])
 * draw: Restores the window, font and size options for default
   terminal (Commit [096f0b])

Changes in Xmaxima:
-------------------
 * fix to allow several plotdf windows to work independently by refreshing
   the expressions of the derivatives every time a new trajectory is plotted.

Changes in the Windows installer:
---------------------------------
 * Update SBCL, wxMaxima, TCL/TK.
   A newer Gnuplot version is available, but causes problems when using
   pipes on Windows, therefore that was not updated.
 * Use the HTML manual (not the PDF version) in the start menu.
 * Strip included C programs (saves some space).
 * The command line option "--userdir" did not work on Windows, fixed that.

Bug fixes for numbered bugs:
----------------------------
 * \#380: algsys a*b=c*d grossly incomplete
 * \#484: limit(x=0,x,0) wrong
 * \#926: sign errors in cartan package
 * \#1097: pattern variable appears in letsimp result
 * \#1848: taytorat leaks internal gensyms from multivar expansions
 * \#2247: not plotting from Vista, probably due to national characters
 * \#2388: wrong limit
 * \#2446: horner of multivariate taylor gives junk
 * \#2709: `letsimp' is wrong if `ratexpand' is called
 * \#2837: ev causes bogus WNA checks for sum, product, define and ":"
 * \#2876: Error simplifying infix operators declared l/rassociative
 * \#2921: errcatch fails to catch various errors
 * \#2953: limit loops endlessly
 * \#3068: taylor of CRE fails
 * \#3159: plot3d in Windows not recognizing plot option color
 * \#3453: Mesh Lines Bugged | Cannot Change Their Color from Default
 * \#3462: simplify_sum() variable name clash
 * \#3483: limit apparently causes infinite loop
 * \#3520: "Directory does not exist" errors on Windows 10
 * \#3542: Unable to display second plot until first one is closed
 * \#3605: Variable confusion in function handling Taylor series
 * \#3654: uniteigenvectors fails if uv[1] is used in the user main program
 * \#3656: Update builtins-list.txt
 * \#3718: incorrect trigonometric definite integral
 * \#3736: Quoting either min or max inhibits simplification
 * \#3764: limit of min works with assume but not with asksign
 * \#3765: min(und,...) gives error with trylevel > 1
 * \#3769: max or min called on CRE expressions
 * \#3789: package ezunits: ev(dimensions(u), nouns) stack overflow
 * \#3793: plot2d fails on small x-range
 * \#3796: plot3d doesn't support rotation in 5.45
 * \#3797: plot2d(0, ...) gives "can't plot with empty y range" -- regression
 * \#3801: error sourcing .xmaximarc on Windows
 * \#3805: plot2d should give a clean user error for undefined functions
 * \#3807: plot2d heuristic to detect unbound variables excludes valid cases
 * \#3810: integrate error "not of type FIXNUM" for integrand with floats in it
 * \#3819: Implicit plot2d calculating 1/0 gives Lisp error
 * \#3820: testsuite with display_all = true
 * \#3825: apply('forget, facts()) gives Lisp error
 * \#3826: limit returns temp variable expression
 * \#3838: limit(atan(sin(x)),x,inf,plus) --> atan(ind)
 * \#3844: Wrong limit involving gamma function
 * \#3881: plot2d not ignoring errors within functions
 * \#3883: plot creates invalid gnuplot command
 * \#3893: display2d and long numbers
 * \#3907: gnuplot_postamble not actually the last Gnuplot output before plot
 * \#3910: correct 'an Unicode' in doc/info/stringproc.texi
 * \#3921: Expanded subtracted from unexpanded with e^ix does not integrate to zero
 * \#3925: Maxima help button opens "file not found"
 * \#3934: expand(1/(1+%i)^4) => (-4)^(-1) (unsimplified)
 * \#3935: Noncommutative multiplication with string argument triggers "declare: argument must be a symbol"
 * \#3936: plot2d sends invalid file to gnuplot
 * \#3945: 'props' isn't empty at startup
 * \#3950: letsimp confuses symbols and nullary applications
 * \#3951: screen terminal no longer works as described in docs
 * \#3952: plot2d clipping warnings not appearing
 * \#3953: Pressing q necessary to continue when plot2d output to svg
 * \#3956: expand(1/((sqrt(2)-1)*(sqrt(2)+1))) => 1/1 (unsimplified)
 * \#3958: plot2d with multiple discrete plots fails
 * \#3959: plot2d + Gnuplot 4 with `plot title noenhanced`

Unnumbered bugs fixed:
---------------------
 * (x^^-1) . x simplified to 1 instead of dotident (commit [c8d115d])
 * Adjust derivatives of beta_incomplete and friends (commit [ad682b0])
 * mailing list 2022-01-04: Bad vect package? (commit [c6110df])
 * mailing list 2021-06-27: ev(xxx,pred) vs is(xxx) (commit [37206b8])
 * Stackoverflow: [(wx)Maxima: texput for powers of expressions](https://stackoverflow.com/questions/66056058/wxmaxima-texput-for-powers-of-expressions) (commit [4e65bc3])
 * Stackoverflow: [How to plot a bode_gain inside the wxmaxima GUI?](https://stackoverflow.com/questions/70102803/how-to-plot-a-bode-gain-inside-the-wxmaxima-gui) (commit [6bee3ca])

Documentation:
--------------
 * Examples in the introduction to strings (Data Types and Structures chapter)
   fixed.
 * New build scripts in Lisp, Python is no longer needed to build the documentation
 * Update japanese documentation
 * New configuration options --enable-lang-ru and --enable-lang-ja for building
   japanese and russian documentation
 * New configuration option --enable-build-docs (default = yes) to make it
   possible to omit building documentation
 * Documentation is only produced in UTF-8 encoding and no other encoding
 * Expunge build and runtime machinery for doc/info/<lang>.utf8
 * Remove configuration options --enable-lang-<lang>-utf8
 * Fix building PDF documentation for non-english languages. Use xeTeX
   make pdf PDFTEX=xetex
   to build Japanese and Russian PDFs.

Build system:
-------------
 * Many improvements wrt out-of-tree-builds and translations of the manual
