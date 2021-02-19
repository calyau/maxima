Maxima 5.38 change log
======================

New items in core:
------------------
 * commit [ae11414]: Images in the HTML manual now are automatically shrunk to fit within the browser window, if neeeded.
 * commit [d817ac5]: A singlepage HTML manual (doc/info/maxima_singlepage.html)

 * commit [2b0f8c4]: Docu/Creating a release: Added a request to inform the wxMaxima team that aims to make a simultaneous release.
 * commits [b2302ed]-[fcbb0e4]: Commit charsets package as posted by Michel Talon
 * commit [1818e0b]: Add configure option --enable-mathjax to enable usage of MathJax in the html docs
 * commit [7ca8a27]: Add simple documentation for bf_fft and bf_inverse_fft.
 * commit [d1def61]: Added a small GUI for Windows users for selecting their default Lisp interpreter.
 * commit [688f662]: Allow setting the Lisp type (MAXIMA_LISP=...) using maximarc on Windows.
 * commit [bdaf3c7]: Try to exit gracefully if we are unable to connect to a server

New items in share:
-------------------
 * commits [68d866e]-[4d1f046]: draw: grid now not only accepts boolean values, but also a list of 2 numbers.

Changes in core:
----------------
 * commits [3631127]-[82aea3a]: Documentation: Add example images for the draw options.

 * commit [baa97e3]: Crosscompiling: update sbcl to the current version.
 * commit [eedadc7]: Adjust the tolerances of two li[2] numerical tests
 * commit [79d9dcd]: Prevent the removal of the integervalued feature during a reset()
 * commit [05b922f]: Work around bug in ecl that causes all tests to fail.
 * commit [f798207]: Removed the --dynamic-space-size option for sbcl.
 * commit [eba5341]: Fix bug in utf-8-fix-start-end and improve pseudo-unicode-support for GCL
 * commit [d95efeb]: WIP: Bigfloat FFT routines.

Changes in share:
--------------

 * commit [4ae042fc]: Split up implementation of FFT in a separate package
 * commit [473ea6c]: Add the draw package to autoload updating the documentation
 * commit [993aea7]: First attempt to rework numericalio.lisp for greater speed and memory efficiency.

Bug fixes:
----------

 * commit [22f4fd0]: Apply patch in bug report 3120.
 * commit [c57712d]: Fix #3105: li[s](1.0) doesn't simplify.
 * commit [3b94068]: Fix #3112: inaccurate zeta(n) for negative even n.
 * commit [f7b7b06]: Fix #3098: li[3] nuemrical evaluation.
 * commit [689fdbc]: Fix SF bug report #3104: "limit(log(1 - exp(x)), x, 0, plus), numer => stack overflow"
 * commit [689fdbc]: Fix SF bug report #3103: "limit of li[..](...) with numer:true infinite recursion"
 * commit [30d5d01]: Fix SF bug #3102: "find_root(x,x,-1e300,1e300) => overflow"
 * commit [0443536]: Fix SF bug report #3099: "Float read is single-precision, though float() is double-precision"
 * commit [33666cc]: Fix bug #481: ('m)[1] (meval)
 * commit [7398246]: Fixed bug #3075: '#3075 answer "3*false" from "integrate(3*asinh(x),x,-inf,inf)"'
 * commit [2c7bf01]: Fixed bug #3081 'abs(...) -> "bfloat: attempted conversion of floating-point infinity."'
 * commit [26375cd]: Fix #3069 limit(a/abs(x),x,0) crashes with stack overflow
 * commit [df38b65]: Fix bug:#3065.
 * commit [7409f45]: Fix bug #3058: Lingering assumptions after an interrupted simpsum
 * commit [0848773]: Add test for SF bug #2302: "'at' applied to definite integral"
 * commit [9c89d36]: A quick solution for Bug 3052
 * commit [0b6a9a1]: Fix SF bug #3049: "set should act like list"
 * commit [4b6c2df]: Fix SF bug #3045: "Save command now fails to generate lisp file"
 * commit [3c8d45b]: Fix bug #3044: opsubst crash
 * commit [38c5ac7]: Fix SourceForge bug #2159 - integration_with_logabs.
 * commit [20d4d93]: Fix SF bug #3003: make pdf returns an error.
 * commit [49a2df1]: Fix SourceForge bug #3017 - "integrate_use_rootsof" leads to wrong result.
 * commit [7c12736]: Add tests to verify that SF bug #365 is fixed. Add tests for related bug #3072
    
Unnumbered bugs:
----------------

 * commit [f4af2a1]: Use per-maxima-instance filenames for draw and plot. Without this simultaneous plotting from concurrent maxima instances might result in name collisions.
 * commit [5cc41da]: Allow sbcl to use enough memory to compile Lapack (linux+mac only).
 * commit [4be3d80]: Remove the two black lines at the border of the bounding box that were introduced with gnuplot 5.0
 * commit [592b38b]: Fix integrate((4*x^3-x^2+4*x)^(-2/3)*(12*x^2-2*x+4),x);
 * commits [f7b7b06]...[eedadc7]: li[2] and li[3] fixes.
 * commit [366b08f]: Try to fix the trace output when the lisp_print trace option is set
 * commit [b637368]: In MEVAL1, call GETCHARN only if argument is a symbol. Fixes bug reported to mailing list 2016-01-06
