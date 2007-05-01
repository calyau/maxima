;;; The Maxima test suite is defined to be the sum of all the tests
;;; in the files listed in *testsuite-files*.
;;; Each entry can either be a plain file name (minus extension), e.g.,
;;; "testfile.mac", or a list containing a file name followed by the numbers
;;; of the test problems that are expected to fail, e.g.
;;; ("testfile.mac" 7 9 13).

(setf $testsuite_files
      '((mlist simp)
	"rtestnset" "rtest1" "rtest1a" "rtest2" "rtest4"  "rtest5"
	"rtest6" "rtest6a" "rtest6b" "rtest7" "rtest9"
	"rtest9a"
    ((mlist) "rtest10" 24 25)
    "rtest11" "rtest13" "rtest13s"
	((mlist simp) "rtest14" 57 63)
	((mlist simp) "rtest15")
	((mlist simp) "rtest16" 5)
	"rtestode" "rtestode_zp"
	"rtest3" "rtest8"
    ((mlist) "rtest12" 76 78)
    "rexamples"
	"rtesthyp"
	"rtestmt19937"
    ((mlist) "rtest_allnummod" 135 136)
    "rtestconjugate"
    ((mlist) "rtestsum" 3 4 18)
    "rtest_trig"
    "rtest_zeta"
    "rtest_diff_invtrig"
    "rtest_scalarp"
    "rtest_everysome"
    "rtestint"
    "rtestifactor"
    "rtest_equal"
    ((mlist) "rtest_abs" 42 43)
    ((mlist) "rtest_taylor" 66 67 72 88 89 90 91 94 96 99 104 110 112 113 118 119 120 121 122 123 124)
    ((mlist) "rtest_dot")
    "rtest_mset"
    "rtest_boolean"
    "rtest_round"
    ))
