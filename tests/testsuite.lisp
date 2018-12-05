;;; The Maxima test suite is defined to be the sum of all the tests in
;;; the files listed in the Maxima option variable $testsuite_files.
;;; Each entry can either be a plain file name (minus extension), e.g.,
;;; "testfile.mac", or a Maxima list containing a file name followed by
;;; another Maxima list of the numbers of the test problems that are
;;; expected to fail, e.g.  ((mlist simp) "testfile.mac" ((mlist) 7 9 13)).

(setf $testsuite_files
      `((mlist simp)
	((mlist simp) "rtest_rules"
	 #+allegro ((mlist simp) 11 13))
        "rtestnset" 
        ((mlist simp) "rtest1" ((mlist simp) 183 185 186))
        ((mlist simp) "rtest1a" ((mlist simp) 33))
        ((mlist simp) "rtest2" ((mlist simp) 86 95))
	"rtest4"
        ;; Mark tests that require the documentation as known failures
        ;; if this was a lisp-only build
        ((mlist simp) "rtest5"
                 ,@(and (boundp '*autoconf-lisp-only-build*)
                        (symbol-value '*autoconf-lisp-only-build*)
                        (list* '(mlist simp) (list 78 80))))
	 ;; 46 = ECL bug #437: mishandling float format
        ((mlist simp) "rtest6"
	 #+ecl ((mlist simp) 44 46))
        "rtest6a" "rtest6b" "rtest7"
        "rtest9" 
	((mlist simp) "rtest9a"
	 #+allegro ((mlist simp) 24 27 30 31 35 36 47 48 51 52 55 56 59 60 63 64 67 68 71 72))
        ((mlist simp) "rtest10" ((mlist simp) 24 25))
        ((mlist simp) "rtest11"
	 #+allegro ((mlist simp) 136 137 158 174)
	 #+ccl64 ((mlist simp) 158 174)
	 #+gcl ((mlist simp) 158 174 175))
        "rtest13" "rtest13s"
	((mlist simp) "rtest14"
	 #+ecl ((mlist simp) 201 233 234 249 250 251 252 267 297 298 307 310 312 315 319))
        "rtest15"
	;; ccl versions 1.11 and earlier fail test 50.  Mark it as a
	;; known failure.  Presumably 1.12 will have this fixed.
	;; Test 561 tickles bug in ECL if signed zero enabled; see ECL
	;; bug #329. Fixed post-16.1.3.
        ((mlist simp) "rtest16"
	 #+ccl ((mlist simp) 50)
	 #+ecl ((mlist 50 561))
	 #+allegro ((mlist simp) 50 241))
        "rtestode" "rtestode_zp"
        "rtest3"
	((mlist simp) "rtest8"
	 #+ecl ((mlist simp) 104))
        ((mlist simp) "rtest12"
	 ((mlist simp) 76 78))
        "rexamples"
        ((mlist simp) "rtesthyp"
	 ((mlist simp) 105 112 113 123 124 128))
        ((mlist simp) "rtest_hypgeo"
	 ((mlist simp) 143))
        "rtestmt19937"
        "rtest_allnummod"
        "rtestconjugate"
        ((mlist simp) "rtestsum"
	 ((mlist simp) 3 4 18 75))
	;; Tested with acl 10.1
	((mlist simp) "rtest_trig"
	 #+allegro ((mlist simp) 58))
        "rtest_zeta"
        "rtest_diff_invtrig"
        "rtest_scalarp"
        "rtest_everysome"
        "rtestint"
        "rtest_numth"
        "rtestifactor"
        ((mlist simp) "rtest_equal"
	 ((mlist simp) 157 160))
        "rtest_abs"
        ((mlist simp) "rtest_taylor"
	 ((mlist simp) 88 91 94 99 123 124))
        ((mlist simp) "rtest_dot")
        "rtest_mset"
        "rtest_boolean"
        "rtest_round"
        ((mlist simp) "rtest_map"
	 ((mlist simp) 2 3 4))
        ((mlist simp) "rtest_sign"
	 ((mlist simp) 21 25 30 40 65 72 79))
        "rtest_algebraic"
	;; Using the gcl version that is shipped with Ubuntu 16.04 (Long term Support)
	;; test 307 of rtest_gamma results in an error-catch on i386, but not in
	;; i64.
	;; Unfortunately many gcl versions have the same version number so we cannot
	;; test for the buggy version in order to get full support for the lts version.
	;;
	;; On ECL 15.3.7 (but not on ECL versions from 2014 or 2016) rtest_gamma
	;; most of the times crashes on ia32 and sometimes crashes on x64.
	((mlist simp) "rtest_gamma"
	 #+gcl ((mlist simp) 307)
	 #+allegro   ((mlist simp) 48 198 663 745))
        "rtest_expintegral"
        "rtest_signum"
        "rtest_lambert_w"
        ((mlist simp) "rtest_elliptic"
	 #-allegro ((mlist simp) 129 143)
	 #+allegro ((mlist simp) 92 129 143))
        "rtest_integrate"
        "rtest_integrate_special"
        ((mlist simp) "rtest_sqrt"
	 ((mlist simp) 89))
        ((mlist simp) "rtest_carg"
	 ((mlist simp) 40 41))
        ((mlist simp) "rtest_log")
        ((mlist simp) "rtest_power"
	 ((mlist simp) 19 20 26 58 65))
        "rtestdefstruct"
	;; Tested with acl 10.1
	((mlist simp) "rtest_limit"
	 #+allegro ((mlist simp) 184 185))
        "rtest_powerseries"
        ((mlist simp) "rtest_laplace"
	 ((mlist simp) 29 49 50 51 54 59 60 61 62 78 80))
        "rtest_plotoptions"
	"rtest_algsys"
        "rtest_trace"
	))

