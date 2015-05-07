Maxima 5.36 change log
======================

New items in core:
------------------

* function parse_timedate: parse time/date string 


New items in share:
-------------------

* function lll: Lenstra-Lenstra-Lovasz algorithm


Changes in core:
----------------

* at: remove variables in simplification 

Other changes:
--------------

* package contrib_ode: update test suite 
* package ezunits: additional femto, pico, nano, and micro fractions
* package ezunits: apply rules more aggressively
* package maxima-odesolve: documentation
* package gentran: update
* package draw: geometric transformations for 2D implicit functions
* package coma: new version
* package maximaMathML: hook mechanism
* package noninteractive: new script expand_branches.mac


Bug fixes:
----------

* [#2924]: Maxima fails to run on lisp interpreters
* [#2922]: input '20log(x)' causes strange error
* [#2919]: Definite integration broken: integrate(1/(x^2), x, -inf, inf) gives zero
* [#2913]: trigrat crashes with variable name "e"
* [#2907]: ratsubst(z, sin(x + 1), 0) crashes when radsubstflag = true
* [#2898]: limit of continuous --> und
* [#2892]: julia() and mandelbrot() creates maxout.gnuplot in different place than plot2d()/plot3d()
* [#2883]: load (lsquares); Error
* [#2881]: quad_qags recently got problems evaluating erf
* [#2878]: Installing from maxima-5.35.1.tar.gz requires Makeinfo
* [#2873]: implicit_plot does not accept symbolic ranges
* [#2865]: Use exp as a list
* [#2862]: Incorrect result for integrate(u/(u+1)^2,u,0,inf);
* [#2854]: Integral hangs with domain:complex 
* [#2853]: abs_integrate hangs after encountering log(0)

Unnumbered bugs:
----------------

* commit [b0a9ab]: Fix jacobi_sc(elliptic_kc(m)/2,m)
* commit [ff92d9]: Fix make_elliptic_f for inverse_jacobi_ns and inverse_jacobi_nc
* commit [b7dc04]: Added device info to paths in cl-info::\*info-tables\*
* commit [95c186]: Fixes a bug in the style option parser
* commit [bf9a67]: Correct the derivative of bessel_y wrt order: diff(bessel_y(v,z),v)
* commit [0d8d8a]: Handle the (finite) geometric sums with quotient 1.
* commit [b3cffd]: Fix bug controlling non numeric symbols in explicit
* commit [114860]: Add device to maxima-load-pathname-directory.
* commit [72d617]: Fix maxima -s PORT command.
* mailing list 2015-03-16: [Requesting advice on simplification rules for user-defined operators][1]
* mailing list 2015-02-09: [save(..) can't save syntax extensions in a readable form][2]
* mailing list 2015-02-16: [expand phenomenon][3]
* mailing list 2015-02-08: [Newbie lisp question][4]
* mailing list 2015-01-22: [Matrix inversion with detout = true?][5]
* mailing list 2015-01-09: [Problem loading package COMA][6]
* mailing list 2015-01-05: [order of operands is changed due to source information][7]

[1]: https://sourceforge.net/p/maxima/mailman/message/33599981/
[2]: https://sourceforge.net/p/maxima/mailman/message/33377390/
[3]: https://sourceforge.net/p/maxima/mailman/message/33442588/
[4]: https://sourceforge.net/p/maxima/mailman/message/33375738/
[5]: https://sourceforge.net/p/maxima/mailman/message/33255733/
[6]: https://sourceforge.net/p/maxima/mailman/message/33219967/
[7]: https://sourceforge.net/p/maxima/mailman/message/33204686/
