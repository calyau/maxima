Maxima 5.43 change log
======================

New items in core:
------------------

 * function cartesian_product_list: Cartesian product of lists.

 * function garbage_collect: trigger Lisp garbage collector.

 * variables maxima_frontend, maxima_frontend_version:
   inform `build_info` which user interface is in use.


New items in share:
-------------------

 * killing.dem: a demo for Killing vector fields.

 * package logic: new function logic_simplify implements K-Map reduction
   (Quine-McCluskey algorithm)


Changes in core:
----------------

 * function to_cl: now autoloaded.

 * package linearalgebra: autoload all functions.

 * function run_testsuite: summary now not only tells which unexpectedly
   failed, but also which ones unexpectedly passed.

 * series expansion for expintegral_si and gamma_incomplete; breaks
   some existing tests, see commit 47a6afd.

 * function translate: generate code which translates conditional
   expressions with prederror = true
 
Changes in share:
--------------

 * package gentran: new version (thanks to Michael Stern)

 * package simplex: improvements, including symbolic inputs.

 * package ode: test cases.

 * package odepack: works with ECL.

 * package lapack: works with ECL.

 * package sarag: fix missing definition of SQUARE_FREE_ALGORITHM.

 * package sarag: make certificate proof more explicit (verify certificate)

 * share test suite: include tests for packages lapack, sym, and ode.

 * share test suite: a few simple tests for package odepack.

 * ezunits: conversion rule for nondimensional/(sum of dimensional)

 * ezunits: 0 now can have a dimension.

 * package draw: accepts color index number 0, 1, 2, ... 17.

 * package draw: accepts plot titles which are not strings.

 * package wrstcse: improved range and resolution of Monte Carlo analysis.


User interfaces:
----------------

 * SF Patch #88: Emaxima now correctly expands tabs in the comint buffer of emacs.


Bug fixes:
----------

 * #3549: "patch to mmref, and removing mmref.tex and maxima_pdf.texi from info directory"
 * #3532: "Integrator doesn't use a new variable internally, causing facts on the original variable to be used for the substitution variable"
 * #3529: "crash with gcl due to wrong maxima startup script"
 * #3524: "Discrepancy in intromax"
 * #3514: "ECL: Marking rtest16/test step 50 as \"known bad\" doesn't work"
 * #3497: "ctensor documentation on the Ricci tensor is inconsistent"
 * #3496: "Hardcoded interpreter path in doc/info/build_index.pl causes build fail on FreeBSD"
 * #3494: "vector.dem is broken"
 * #3484: "taylor of expintegral_si is wrong"
 * #3470: "Error in maximize_lp "
 * #3463: "Documentation: mention parameter epsilon_lp more explicitly in documentation for minimize_lp"
 * #3459: "Wrong limit calculation"
 * #3412: "Bug when translating functions that contain an \"if\" (in my case an implicit if)"

Unnumbered bugs:
----------------

 * mailing list 2019-05-11: "Problems with gramschmidt"
 * mailing list 2019-05-06: "problem with elem() after kill(all)"
 * mailing list 2019-04-02: "maxima lists"
 * mailing list 2019-03-08: "GCL build broken".
 * mailing list 2019-03-08: "batch overwrites last output line"
 * mailing list 2019-03-06: "the share testsuite + MAXIMA:$RESULTANT is undefined errors"
 * mailing list 2018-12-03: "error in compiling function with global variable"
 * mailing list 2018-11-01: "radcan(sqrt(-1/(sqrt(-3)+1))*sqrt(2))"
 * commit c164f5f: SIMPEXPT was marking unsimplified expressions with SIMP
 * commit 61c71db: Fix siemens/sievert typo (ezunits)

 
Build system:
-------------

 * Crosscompiling: Add 'maxima_longnames.c' to automake

 * configure: enable build with ABCL via --enable-abcl.

 * Windows installer: can now include ABCL.

 * Windows installer: updated external utilities.

 * Windows installer: updated wxMaxima version.

 * "make check" manipulates the list of tests now in a more canonical way in order
   to automatically run the interactive tests, as well.

 * ./configure --enable-quiet-build now muffles more warnings.

 * build documentation using the standard makeinfo tools.

 * maxima-sbcl now supports non-ASCII user names and install dirs on MacOs 
   and MS Windows

 * "make install" now installs emaxima and imaxima in a place emacs will 
   find by default. configure's --emacs-prefix= option allows to choose
   a different directory.


Documentation:
--------------

 * ./update_examples now interprets lines beginning with 
   the string "input:" as text that should be sent as input.


Runtime:
--------

 * environment variable MAXIMA_DOC_PREFIX overrides the
   location the documentation is searched for.

 * environment variable GCL_DISABLE_MULTIPROCESS_MEMORY_POOL
   disables memory-sharing between GCL-compiled Maxima processes.

 * Maxima.bat now autodetects 64-bit systems and does the necessary
   modifications for making lapack work in SBCL if this test is positive.

