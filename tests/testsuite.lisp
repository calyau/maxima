;;; The Maxima test suite is defined to be the sum of all the tests
;;; in the files listed in the Maxima option variable $testsuite_files.
;;; Each entry can either be a plain file name (minus extension), e.g.,
;;; "testfile.mac", or a Maxima list containing a file name followed by
;;; the numbers of the test problems that are expected to fail, e.g.
;;; ((mlist simp) "testfile.mac" 7 9 13).

(setf $testsuite_files
      `((mlist simp)
#-acl        "rtest_rules"
#+acl   ((mlist) "rtest_rules" 11 13)
        "rtestnset" 
        ((mlist) "rtest1" 183 185 186)
        ((mlist) "rtest1a" 33)
        ((mlist) "rtest2" 86 95)
	"rtest4"
        ;; Mark tests that require the documentation as known failures
        ;; if this was a lisp-only build
        ((mlist) "rtest5"
                 ,@(and (boundp '*autoconf-lisp-only-build*)
                        (symbol-value '*autoconf-lisp-only-build*)
                        (list 78 80)))
        ((mlist) "rtest6" #+ecl 44 #+ecl 46) ;; 46 = ECL bug #437: mishandling float format
        "rtest6a" "rtest6b" "rtest7"
        "rtest9" 
#-acl   "rtest9a"
#+acl   ((mlist simp) "rtest9a" 24 27 30 31 35 36 47 48 51 52 55 56 59 60 63 64 67 68 71 72)
        ((mlist simp) "rtest10" 24 25)
        ((mlist) "rtest11" #+acl 136 #+acl 137 #+(or gcl cmucl ccl64 acl) 158 #+(or gcl cmucl ccl64 acl) 174 #+gcl 175)
        "rtest13" "rtest13s"
#-ecl   "rtest14"
#+ecl   ((mlist simp) "rtest14" 201 233 234 249 250 251 252 267 297 298 307 310 312 315 319)
        "rtest15"
	;; ccl versions 1.11 and earlier fail test 50.  Mark it as a
	;; known failure.  Presumably 1.12 will have this fixed.
    ;; Test 561 tickles bug in ECL if signed zero enabled; see ECL bug #329. Fixed post-16.1.3.
        ((mlist simp) "rtest16" #+ccl 50 #+ecl 50 #+acl 50 #+acl 241 #+ecl 561)
        "rtestode" "rtestode_zp"
        "rtest3"
#-ecl	"rtest8"
#+ecl   ((mlist simp) "rtest8" 104)
        ((mlist simp) "rtest12" 76 78)
        "rexamples"
        ((mlist simp) "rtesthyp" 105 112 113 123 124 128)
        ((mlist simp) "rtest_hypgeo" 143)
        "rtestmt19937"
        "rtest_allnummod"
        "rtestconjugate"
        ((mlist simp) "rtestsum" 3 4 18 75)
        "rtest_trig"
        "rtest_zeta"
        "rtest_diff_invtrig"
        "rtest_scalarp"
        "rtest_everysome"
        "rtestint"
        "rtest_numth"
        "rtestifactor"
        ((mlist simp) "rtest_equal" 157 160)
        "rtest_abs"
        ((mlist simp) "rtest_taylor" 88 91 94 99 123 124)
        ((mlist simp) "rtest_dot")
        "rtest_mset"
        "rtest_boolean"
        "rtest_round"
        ((mlist simp) "rtest_map" 2 3 4)
        ((mlist simp) "rtest_sign" 21 25 30 40 65 72 79)
        "rtest_algebraic"
	;; Using the gcl version that is shipped with Ubuntu 16.04 (Long term Support)
	;; test 307 of rtest_gamma results in an error-catch on i386, but not in
	;; i64.
	;; Unfortunately many gcl versions have the same version number so we cannot
	;; test for the buggy version in order to get full support for the lts version.
	#-(or gcl acl)   "rtest_gamma"	
	#+gcl   ((mlist simp) "rtest_gamma" 307)
	#+acl   ((mlist simp) "rtest_gamma" 48 198 663 745)
        "rtest_expintegral"
        "rtest_signum"
        "rtest_lambert_w"
        ((mlist) "rtest_elliptic" #+acl 92 129 143)
        "rtest_integrate"
        "rtest_integrate_special"
        ((mlist simp) "rtest_sqrt" 89)
        ((mlist simp) "rtest_carg" 40 41)
        ((mlist simp) "rtest_log")
        ((mlist simp) "rtest_power" 19 20 26 58 65)
        "rtestdefstruct"
#-acl   "rtest_limit"
#+acl   ((mlist simp) "rtest_limit" 184)
        "rtest_powerseries"
        ((mlist) "rtest_laplace" 29 49 50 51 54 59 60 61 62 78 80)
        "rtest_plotoptions"
	"rtest_algsys"
        "rtest_trace"
	))

