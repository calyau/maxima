;;; The Maxima test suite is defined to be the sum of all the tests
;;; in the files listed in *testsuite-files*.
;;; Each entry can either be a plain file name (minus extension), e.g.,
;;; "testfile.mac", or a list containing a file name followed by the numbers
;;; of the test problems that are expected to fail, e.g.
;;; ("testfile.mac" 7 9 13).

(setf *testsuite-files*
      '("rtest1.mac" "rtest1a.mac" "rtest2.mac" "rtest4.mac"  "rtest5.mac"
	"rtest6.mac" "rtest6a.mac" "rtest6b.mac" "rtest7.mac" "rtest9.mac"
	"rtest9a.mac" "rtest10.mac" "rtest11.mac" "rtest13.mac" "rtest13s.mac"
	("rtest14.mac" 54 57 61 64 66)
	("rtest15.mac" 4)
	("rtest16.mac" 4)
	"rtestode.mac" "rtestode_zp.mac"
	"rtest3.mac" "rtest8.mac" "rtest12.mac" "rexamples.mac"
	"rtesthyp.mac" "rtestnset.mac"
	))
